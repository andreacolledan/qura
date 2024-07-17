module Lang.Analysis.Analyze
  ( runAnalysis,
    runAnalysisWith,
  )
where

import Control.Monad
import Control.Monad.Error.Class
import qualified Data.HashMap.Strict as Map
import Index.AST
import Lang.Type.AST
import Lang.Expr.AST
import Lang.Analysis.Derivation
import Lang.Analysis.Annotate
import Control.Monad.Except
import Solving.CVC5 (SolverHandle)
import Index.Semantics.Resource (GlobalResourceSemantics, LocalResourceSemantics)
import Data.Maybe
import Lang.Library.Constant

--- TYPE SYNTHESIS MODULE ---------------------------------------------------------------
---
--- This module contains the main logic to synthesize the full type of a PQR expression
--- after it has been preprocessed. Type synthesis assumes that the structure of a type
--- is already correct and focuses instead on the indices that annotate types.
--- At this stage, there should be no type variables left.
-----------------------------------------------------------------------------------------

-- | @analyze e@ infers the indexed type and width upper-bound of expression @e@.
-- If successful, it returns a pair @(t, i)@, where @t@ is the inferred type of @e@ 
-- and @i@ is the upper bound on the width of the circuit built by @e@.
-- Otherwise, it throws a 'TypeError'.
analyze :: Expr -> TypeDerivation (Type, Maybe Index)
-- UNIT
analyze EUnit = withScope EUnit $ do
  i <- ifGlobalResources Identity
  return (TUnit, i)
-- VARIABLE
analyze e@(EVar id) = withScope e $ do
  typ <- typingContextLookup id
  i <- ifGlobalResources $ wireCount typ
  return (typ, join i) -- i might be Nothing bcause a) typ is an arrow type annotated with Nothing or b) we are not synthesizing indices. TODO refactor this
-- TUPLE
analyze e@(ETuple es) = withScope e $ do
  when (length es < 2) $ error "Internal error: tuple with less than 2 elements escaped parser."
  (infrs, wcs) <- mapAndUnzipM (withWireCount . analyze) es
  let (typs, is) = unzip infrs
  k <- ifGlobalResources $ foldAnno Sequence Identity [Parallel <$> i <*> (Parallel <$> subsequentwc step wcs <*> previouswc step typs) | (i, step) <- zip is [0 ..]] --todo check
  return (TTensor typs, join k)
    where
      subsequentwc :: Int -> [Maybe Index] -> Maybe Index
      subsequentwc step wcs = foldAnno Parallel Identity $ drop (step+1) wcs
      previouswc :: Int -> [Type] -> Maybe Index
      previouswc step typs = foldAnno Parallel Identity $ take step $ map wireCount typs
      foldAnno :: (Index -> Index -> Index)  -> Index -> [Maybe Index] -> Maybe Index
      foldAnno _ acc [] = Just acc
      foldAnno _ _ (Nothing:_) = Nothing
      foldAnno f acc (Just i:is) = foldAnno f (f acc i) is
-- NIL
analyze e@(ENil anno) = withScope e $ case anno of
  Just (TVar _) -> throwLocalError $ CannotSynthesizeType e
  Just typ -> do
    checkWellFormedness typ
    k <- ifGlobalResources Identity
    return (TList "i" (Number 0) typ, k)
  Nothing -> error "Internal error: nil without type annotation"
-- ABSTRACTION
analyze e@(EAbs p annotyp e1) = withScope e $ do
  checkWellFormedness annotyp
  (ids, annotyps) <- makePatternBindings p annotyp SizedLists
  ((typ, i), wc) <- withWireCount $ withBoundVariables ids annotyps $ analyze e1
  k <- ifGlobalResources wc
  return (TArrow annotyp typ i (join k), join k)
-- LIFT
analyze e@(ELift e1) = withScope e $ do
  (typ, i) <- withNonLinearContext $ analyze e1
  k <- ifGlobalResources Identity
  return (TBang i typ, k)
-- CONS
analyze e@(ECons e1 e2) = withScope e $ do --TODO check
  (TList id j typ1, i1) <- analyze e1
  ((typ1', i2), wc) <- withWireCount $ analyze e2
  unlessSubtype typ1' (isub j id typ1) $ do
    expected <- runSimplifyType typ1
    actual <- runSimplifyType typ1'
    throwLocalError $ UnexpectedType e2 expected actual
  -- i1 || wires in e2 >> i2 || #(typ1) >> sum[id < j+1] #(typ1)):
  k <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> wc) <*> (Sequence <$> (Parallel <$> i2 <*> wireCount (TList id j typ1)) <*> (BoundedParallel id (Plus j (Number 1)) <$> wireCount typ1))
  return (TList id (Plus j (Number 1)) typ1, join k)
-- FOLD
analyze e@(EFold e1 e2 e3) = withScope e $ do
  -- naming conventions are not satisfied here because the rule is HARD to parse
  (steptyp@(TBang o0 (TIForall id (TArrow (TTensor [acctyp, elemtyp]) acctyp' stepwidth o1) o2 o3)), i1) <- analyze e1
  -- the following 4 checks force all perceivable circuit-building to happen in the function and not before
  -- this is not really necessary, but it is a simplification for now
  unlessIdentity o0 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o0)
  unlessIdentity o1 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o1)
  unlessIdentity o2 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o2)
  unlessIdentity o3 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o3)
  -- acctyp' <: acctyp{id+1/id}
  unlessSubtype acctyp' (isub (Plus (IndexVariable id) (Number 1)) id acctyp) $ throwLocalError $ UnfoldableStepfunction e1 steptyp
  ((acctyp'', i2), wc1) <- withWireCount $ analyze e2
  -- acctyp'' <: acctyp{0/id}
  let baseacctyp = isub (Number 0) id acctyp
  unlessSubtype acctyp'' baseacctyp $ throwLocalError $ UnfoldableAccumulator e2 acctyp''
  ((argtyp@(TList id' arglen elemtyp'), i3), wc2) <- withWireCount $ analyze e3
  -- elemtyp'{arglen-(id+1)/id'} <: elemtyp
  let flippedelemtyp' = isub (arglen `Minus` (IndexVariable id `Plus` Number 1)) id' elemtyp'
  unlessSubtype flippedelemtyp' elemtyp $ throwLocalError $ UnfoldableArg e3 argtyp
  -- width upper bound of ONLY fold execution: max(#(acctyp{0/id},>>[id<arglen] stepwidth || ||[id'<arglen-(id+1)] #(elemtyp)))
  k1 <- ifGlobalResources $ Max <$> wireCount baseacctyp <*> (BoundedSequence id arglen <$> (Parallel <$> stepwidth {-todo has to be changed afterwards to include bang and such-} <*> (BoundedParallel id' (Minus arglen (Plus (IndexVariable id) (Number 1))) <$> wireCount elemtyp))) --todo check
  -- the total upper bound takes into consideration the evaluation of e1, e2, e3 and the fold execution
  -- (i1 || wires in e2 and e3) >> (i2 || wires in e3) >> (i3 || wires in the result of e2) >> k1:
  k2 <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> (Parallel <$> wc1 <*> wc2)) <*> (Sequence <$> (Parallel <$> i2 <*> wc2) <*> (Sequence <$> (Parallel <$> i3 <*> wireCount (isub (Number 0) id acctyp)) <*> join k1))
  return (isub arglen id acctyp, join k2) --todo check
-- APPLICATION (FUNCTIONS)
analyze e@(EApp e1 e2) = withScope e $ do
  (TArrow annotyp typ3 j1 j2, i1) <- analyze e1
  ((typ2, i2), wc) <- withWireCount $ analyze e2
  unlessSubtype typ2 annotyp $ do
    actual <- runSimplifyType typ2
    throwLocalError $ UnexpectedType e2 annotyp actual
  -- (i1 || wires in e2) >> (i2 || j2) >> j1:
  k <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> wc) <*> (Sequence <$> (Parallel <$> i2 <*> j2) <*> j1)
  return (typ3, join k)
-- APPLY (CIRCUITS) --todo local metrics
analyze e@(EApply e1 e2) = withScope e $ do
  (TCirc j intyp outtyp, i1) <- analyze e1
  ((typ2, i2), wc) <- withWireCount $ analyze e2
  unlessSubtype typ2 intyp $ do
    expected <- runSimplifyType intyp
    actual <- runSimplifyType typ2
    throwLocalError $ UnexpectedType e2 expected actual
  -- (i1 || wires in e2) >> i2 >> j:
  k <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> wc) <*> (Sequence <$> i2 <*> j)
  return (outtyp, join k)
-- BOX --todo local metrics
analyze e@(EBox anno e1) = withScope e $ case anno of
  Just annotyp -> do
    (typ1@(TBang o0 (TArrow typ2 typ3 j1 _)), i) <- analyze e1
    unlessIdentity o0 $ throwLocalError $ UnexpectedIndex (Number 0) (fromJust o0) --todo this is a simplification
    unlessSubtype annotyp typ2 $ throwLocalError $ UnboxableType e1 typ1
    unless (isBundleType typ3) $ throwLocalError $ UnboxableType e1 typ1
    return (TCirc j1 annotyp typ3, i)
  Nothing -> error "Internal error: box without type annotation"
-- LET-IN
analyze e@(ELet p e1 e2) = withScope e $ do
  (typ1, i1) <- analyze e1
  (ids, typs) <- makePatternBindings p typ1 SizedLists
  ((typ2, i2), wc) <- withWireCount $ withBoundVariables ids typs $ analyze e2
  -- (i1 || wires in e2) >> i2):
  k <- ifGlobalResources $ Sequence <$> (Parallel <$> i1 <*> wc) <*> i2
  return (typ2, join k)
-- ANNOTATION
analyze e@(EAnno e1 annotyp) = withScope e $ do
  checkWellFormedness annotyp
  (typ, i) <- analyze e1
  unlessSubtype typ annotyp $ do
    actual <- runSimplifyType typ
    throwLocalError $ UnexpectedType e1 annotyp actual
  return (annotyp, i)
-- FORCE
analyze e@(EForce e1) = withScope e $ do
  (TBang j typ', i) <- analyze e1
  -- j >> i:
  k <- ifGlobalResources $ Sequence <$> i <*> j
  return (typ', join k)
-- INDEX ABSTRACTION
analyze e@(EIAbs id e1) = withScope e $ do
  ((typ, i), wc) <- withWireCount $ withBoundIndexVariable id $ analyze e1
  k <- ifGlobalResources wc
  return (TIForall id typ i (join k), join k)
-- INDEX APPLICATION
analyze e@(EIApp e1 g) = withScope e $ do
  (TIForall id typ2 j1 _, i) <- analyze e1
  checkWellFormedness g
  -- i >> isub g id j1:
  k <- ifGlobalResources $ Sequence <$> i <*> isub g id j1
  return (isub g id typ2, join k)
-- CONSTANTS
analyze e@(EConst c) = withScope e $ do
  k <- ifGlobalResources Identity
  return (typeOf c, k)
-- ASSUMPTION (unsafe)
analyze e@(EAssume e1 annotyp) = withScope e $ do
  checkWellFormedness annotyp
  (_, i) <- analyze e1
  return (annotyp, i)

--- EXPORTED WRAPPER FUNCTIONS ---------------------------------------------------------------

-- | @runAnalysisWith env e@ runs the whole type inference pipeline on expression @e@,
-- using the typing environment @env@.
runAnalysisWith :: TypingEnvironment -> Expr -> IO (Either TypeError (Type, Maybe Index))
runAnalysisWith env e = runExceptT $ do
  e' <- runAnnotation env e
  ((typ, i), remaining) <- runTypeDerivation (analyze e') env
  when (envIsLinear remaining) $ do
    let remainingNames = [id | (id, bs) <- Map.toList (typingContext remaining), any mustBeUsed bs]
    throwError $ UnusedLinearVariable (head remainingNames) [e]
  return (typ, i)

-- | @runAnalysis e sh@ runs the whole type inference pipeline on expression @e@,
-- using the handle @sh@ to communicate with the SMT solver.
runAnalysis :: Expr -> SolverHandle -> Maybe GlobalResourceSemantics -> Maybe LocalResourceSemantics -> IO (Either TypeError (Type, Maybe Index))
runAnalysis e sh grs lrs = runAnalysisWith (emptyEnv sh grs lrs) e