module Lang.Analysis.InferRefinedType
  ( inferRefinedType,
  )
where

import Control.Monad
import Control.Monad.Error.Class
import qualified Data.HashMap.Strict as Map
import Index.AST
import Lang.Type.AST
import Lang.Expr.AST
import Lang.Analysis.Derivation
import Data.Maybe
import Index.Unify
import Lang.Library.Constant
import Control.Monad.State

-- | @inferRefinedType e@ infers the refined type and width upper-bound of expression @e@.
-- If successful, it returns a pair @(t, i)@, where @t@ is the inferred type of @e@ 
-- and @i@ is the upper bound on the width of the circuit built by @e@.
-- Otherwise, it throws a 'TypeError'.
inferRefinedType :: Expr -> TypeDerivation (Type, Maybe Index)
-- UNIT
inferRefinedType EUnit = withScope EUnit $ do
  i <- ifGlobalResources Identity
  return (TUnit, i)
-- VARIABLE
inferRefinedType e@(EVar id) = withScope e $ do
  typ <- typingContextLookup id
  i <- ifGlobalResources $ typeSize typ
  return (typ, join i) -- i might be Nothing bcause a) typ is an arrow type annotated with Nothing or b) we are not synthesizing indices. TODO refactor this
-- TUPLE
inferRefinedType e@(ETuple es) = withScope e $ do
  when (length es < 2) $ error "Internal error: tuple with less than 2 elements escaped parser."
  (infrs, wcs) <- mapAndUnzipM (withEnvSize . inferRefinedType) es
  let (typs, is) = unzip infrs
  k <- ifGlobalResources $ foldAnno Sequence Identity [Parallel <$> i <*> (Parallel <$> subsequentwc step wcs <*> previouswc step typs) | (i, step) <- zip is [0 ..]]
  return (TTensor typs, join k)
    where
      subsequentwc :: Int -> [Maybe Index] -> Maybe Index
      subsequentwc step wcs = foldAnno Parallel Identity $ drop (step+1) wcs
      previouswc :: Int -> [Type] -> Maybe Index
      previouswc step typs = foldAnno Parallel Identity $ take step $ map typeSize typs
      foldAnno :: (Index -> Index -> Index)  -> Index -> [Maybe Index] -> Maybe Index
      foldAnno _ acc [] = Just acc
      foldAnno _ _ (Nothing:_) = Nothing
      foldAnno f acc (Just i:is) = foldAnno f (f acc i) is
-- NIL
inferRefinedType e@(ENil anno) = withScope e $ case anno of
  Just (TVar _) -> throwLocalError $ CannotSynthesizeType e
  Just typ -> do
    k <- ifGlobalResources Identity
    let id = fresh "i" [typ]
    return (TList id (Number 0) typ, k)
  Nothing -> error "Internal error: nil without type annotation"
-- ABSTRACTION
inferRefinedType e@(EAbs p annotyp e1) = withScope e $ do
  checkWellFormedness annotyp
  (ids, annotyps) <- makePatternBindings p annotyp SizedLists
  ((typ, i), wc) <- withEnvSize $ withBoundVariables ids annotyps $ inferRefinedType e1
  k <- ifGlobalResources wc
  return (TArrow annotyp typ i (join k), join k)
-- LIFT
inferRefinedType e@(ELift e1) = withScope e $ do
  (typ, i) <- withNonLinearContext $ inferRefinedType e1
  k <- ifGlobalResources Identity
  return (TBang i typ, k)
-- CONS
inferRefinedType e@(ECons e1 e2) = withScope e $ do
  (TList id j typ1, i1) <- inferRefinedType e1
  ((typ1', i2), wc) <- withEnvSize $ inferRefinedType e2
  unlessSubtype typ1' (isub (isubSingleton id j) typ1) $ do
    expected <- runSimplifyType typ1
    actual <- runSimplifyType typ1'
    throwLocalError $ UnexpectedType e2 expected actual
  -- i1 || wires in e2 >> i2 || #(typ1) >> sum[id < j+1] #(typ1)):
  k <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> wc) <*> (Sequence <$> (Parallel <$> i2 <*> typeSize (TList id j typ1)) <*> (BoundedParallel id (Plus j (Number 1)) <$> typeSize typ1))
  return (TList id (Plus j (Number 1)) typ1, join k)
-- FOLD
inferRefinedType e@(EFold e1 e2 e3) = withScope e $ do
  -- Step Function --
  (steptyp@(TBang o0 (TIForall step (TArrow (TTensor [acctyp, elemtyp]) incacctyp stepres o1) o2 o3)), i1) <- inferRefinedType e1
  -- the following 4 checks force all perceivable circuit-building to happen in the function and not before
  -- this is not really necessary, but it massively simplifies inference and does not affect expressivity.
  unlessIdentity o0 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o0)
  unlessIdentity o1 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o1)
  unlessIdentity o2 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o2)
  unlessIdentity o3 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o3)
  -- acctyp' <: acctyp{id+1/id}
  unlessSubtype incacctyp (isub (isubSingleton step (IVar step `Plus` Number 1)) acctyp) $ throwLocalError $ UnfoldableStepfunction e1 steptyp
  -- List Argument --
  ((argtyp@(TList id' arglen elemtyp'), i3), wc2) <- withEnvSize $ inferRefinedType e3
  let invariant = [Leq (IVar step `Plus` Number 1) arglen] -- step index is bounded by the length of the list
  -- elemtyp'{arglen-(id+1)/id'} <: elemtyp
  let flippedelemtyp' = isub (isubSingleton id' (arglen `Minus` (IVar step `Plus` Number 1))) elemtyp'
  unlessSubtypeAssuming invariant flippedelemtyp' elemtyp $ throwLocalError $ UnfoldableArg e3 argtyp
  -- Accumulator --
  ((acctyp'', i2), wc1) <- withEnvSize $ inferRefinedType e2
  -- acctyp'' <: acctyp{0/id}
  let baseacctyp = isub (isubSingleton step (Number 0)) acctyp
  unlessSubtypeAssuming invariant acctyp'' baseacctyp $ throwLocalError $ UnexpectedType e2 baseacctyp acctyp'' 
  -- width upper bound of ONLY fold execution: max(#(acctyp{0/id},>>[id<arglen] stepwidth || ||[id'<arglen-(id+1)] #(elemtyp)))
  k1 <- ifGlobalResources $ Max <$> typeSize baseacctyp <*> (BoundedSequence step arglen <$> (Parallel <$> stepres {-todo has to be changed afterwards to include bang and such-} <*> (BoundedParallel id' (Minus arglen (Plus (IVar step) (Number 1))) <$> typeSize elemtyp))) --todo check
  -- the total upper bound takes into consideration the evaluation of e1, e2, e3 and the fold execution
  -- (i1 || wires in e2 and e3) >> (i2 || wires in e3) >> (i3 || wires in the result of e2) >> k1:
  k2 <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> (Parallel <$> wc1 <*> wc2)) <*> (Sequence <$> (Parallel <$> i2 <*> wc2) <*> (Sequence <$> (Parallel <$> i3 <*> typeSize (isub (isubSingleton step (Number 0)) acctyp)) <*> join k1))
  return (isub (isubSingleton step arglen) acctyp, join k2) --todo check
-- APPLICATION (FUNCTIONS)
inferRefinedType e@(EApp e1 e2) = withScope e $ do
  (TArrow annotyp typ3 j1 j2, i1) <- inferRefinedType e1
  ((typ2, i2), wc) <- withEnvSize $ inferRefinedType e2
  unlessSubtype typ2 annotyp $ do
    actual <- runSimplifyType typ2
    throwLocalError $ UnexpectedType e2 annotyp actual
  -- (i1 || wires in e2) >> (i2 || j2) >> j1:
  k <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> wc) <*> (Sequence <$> (Parallel <$> i2 <*> j2) <*> j1)
  return (typ3, join k)
-- APPLY (CIRCUITS)
inferRefinedType e@(EApply e1 e2) = withScope e $ do
  (TCirc j intyp outtyp, i1) <- inferRefinedType e1
  ((typ2, i2), wc) <- withEnvSize $ inferRefinedType e2
  unlessSubtype typ2 intyp $ do
    actual <- runSimplifyType typ2
    throwLocalError $ UnexpectedType e2 intyp actual
  k <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> wc) <*> (Sequence <$> i2 <*> j)
  return (outtyp, join k)
-- BOX --todo local metrics
inferRefinedType e@(EBox anno e1) = withScope e $ case anno of
  Just annotyp -> do
    (typ1@(TBang o0 (TArrow typ2 typ3 j1 _)), i) <- inferRefinedType e1
    unlessIdentity o0 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o0) --todo this is a simplification
    unlessSubtype annotyp typ2 $ throwLocalError $ UnboxableType e1 typ1
    unless (isBundleType typ3) $ throwLocalError $ UnboxableType e1 typ1
    return (TCirc j1 annotyp typ3, i)
  Nothing -> error "Internal error: box without type annotation"
-- LET-IN
inferRefinedType e@(ELet p e1 e2) = withScope e $ do
  (typ1, i1) <- inferRefinedType e1
  (ids, typs) <- makePatternBindings p typ1 SizedLists
  ((typ2, i2), wc) <- withEnvSize $ withBoundVariables ids typs $ inferRefinedType e2
  -- (i1 || wires in e2) >> i2):
  k <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> wc) <*> i2
  return (typ2, join k)
-- ANNOTATION
inferRefinedType e@(EAnno e1 annotyp) = withScope e $ do
  checkWellFormedness annotyp
  (typ, i) <- inferRefinedType e1
  unlessSubtype typ annotyp $ do
    actual <- runSimplifyType typ
    throwLocalError $ UnexpectedType e1 annotyp actual
  return (annotyp, i)
-- FORCE
inferRefinedType e@(EForce e1) = withScope e $ do
  (TBang j typ', i) <- inferRefinedType e1
  -- j >> i:
  k <- ifGlobalResources $ Sequence <$> i <*> j
  return (typ', join k)
-- INDEX ABSTRACTION
inferRefinedType e@(EIAbs id e1) = withScope e $ do
  ((typ, i), wc) <- withEnvSize $ withBoundIndexVariables [id] $ inferRefinedType e1
  k <- ifGlobalResources wc
  return (TIForall id typ i (join k), join k)
-- INDEX APPLICATION
inferRefinedType e@(EIApp e1 g) = withScope e $ do
  (TIForall id typ2 j1 _, i) <- inferRefinedType e1
  checkWellFormedness g
  -- i >> isub g id j1:
  k <- ifGlobalResources $ Sequence <$> i <*> isub (isubSingleton id g) j1
  return (isub (isubSingleton id g) typ2, join k)
-- CONSTANTS
inferRefinedType e@(EConst c) = withScope e $ do
  k <- ifGlobalResources Identity
  grs <- gets grs
  lrs <- gets lrs
  let typ = typeOf c
  let typ' = if isNothing grs then stripGlobalAnnotations typ else typ
  let typ'' = if isNothing lrs then stripLocalAnnotations typ' else typ'
  return (typ'', k)
-- ASSUMPTION (unsafe)
inferRefinedType e@(EAssume e1 annotyp) = withScope e $ do
  checkWellFormedness annotyp
  (_, i) <- inferRefinedType e1
  return (annotyp, i)


--- EXPORTED WRAPPER FUNCTIONS ---------------------------------------------------------------

-- -- | @runRefinedTypeInferenceWith env e@ runs the whole type inference pipeline on expression @e@,
-- -- using the typing environment @env@.
-- runRefinedTypeInferenceWith :: TypingEnvironment -> Expr -> IO (Either TypeError (Type, Maybe Index))
-- runRefinedTypeInferenceWith env e = runExceptT $ do
--   e' <- runBaseTypeInference env e
--   ((typ, i), remaining) <- runTypeDerivation (inferRefinedType e') env
--   when (envIsLinear remaining) $ do
--     let remainingNames = [id | (id, bs) <- Map.toList (typingContext remaining), any mustBeUsed bs]
--     throwError $ UnusedLinearVariable (head remainingNames) [e]
--   return (typ, i)

-- -- | @runRefinedTypeInference e sh@ runs the whole type inference pipeline on expression @e@,
-- -- using the handle @sh@ to communicate with the SMT solver.
-- runRefinedTypeInference :: Expr -> SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> IO (Either TypeError (Type, Maybe Index))
-- runRefinedTypeInference e sh grs lrs = runRefinedTypeInferenceWith (emptyEnv sh grs lrs) e