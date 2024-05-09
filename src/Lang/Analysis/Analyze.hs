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
import Lang.Library.Constant
import Lang.Analysis.Derivation
import Lang.Analysis.Annotate
import Control.Monad.Except
import Solving.CVC5 (SolverHandle)

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
analyze :: Expr -> TypeDerivation (Type, Index)
-- UNIT
analyze EUnit = withScope EUnit $ return (TUnit, Number 0)
-- VARIABLE
analyze e@(EVar id) = withScope e $ do
  typ <- typingContextLookup id
  return (typ, wireCount typ)
-- TUPLE
analyze e@(ETuple es) = withScope e $ do
  when (length es < 2) $ error "Internal error: tuple with less than 2 elements escaped parser."
  ress <- mapM (withWireCount . analyze) es
  let (infrs, wcs) = unzip ress
  let (typs, is) = unzip infrs
  let k = foldl1 Max [i `Plus` subsequentwc step wcs `Plus` previouswc step typs | (i, step) <- zip is [0 ..]]
  return (TTensor typs, k)
    where
      subsequentwc step wcs = foldl Plus (Number 0) $ drop (step+1) wcs
      previouswc step typs = foldl Plus (Number 0) $ take step $ map wireCount typs
-- NIL
analyze e@(ENil anno) = withScope e $ case anno of
  Just (TVar _) -> throwLocalError $ CannotSynthesizeType e
  Just typ -> do
    checkWellFormedness typ
    return (TList (Number 0) typ, Number 0)
  Nothing -> error "Internal error: nil without type annotation"
-- ABSTRACTION
analyze e@(EAbs p annotyp e1) = withScope e $ do
  checkWellFormedness annotyp
  (ids, annotyps) <- makePatternBindings p annotyp SizedLists
  ((typ, i), wc) <- withWireCount $ withBoundVariables ids annotyps $ analyze e1
  return (TArrow annotyp typ i wc, wc)
-- LIFT
analyze e@(ELift e1) = withScope e $ do
  (typ, i) <- withNonLinearContext $ analyze e1
  unlessEq i (Number 0) $ throwLocalError $ UnexpectedWidthAnnotation e1 (Number 0) i
  return (TBang typ, Number 0)
-- CONS
analyze e@(ECons e1 e2) = withScope e $ do
  (typ1, i1) <- analyze e1
  ((TList j typ1', i2), wc) <- withWireCount $ analyze e2
  unlessSubtype (TList j typ1') (TList j typ1) $ do
    expected <- runSimplifyType (TList j typ1)
    actual <- runSimplifyType (TList j typ1')
    throwLocalError $ UnexpectedType e2 expected actual
  -- max (i1 + wires in e2, i2 + #(typ1), (j+1) * #(typ1)):
  let k = Max (Plus i1 wc) (Max (Plus i2 (wireCount typ1)) (Mult (Plus j (Number 1)) (wireCount typ1)))
  return (TList (Plus j (Number 1)) typ1, k)
-- FOLD
analyze e@(EFold e1 e2 e3) = withScope e $ do
  -- naming conventions are not satisfied here because the rule is HARD to parse
  (steptyp@(TBang (TIForall id (TArrow (TTensor [acctyp, elemtyp]) acctyp' stepwidth o1) o2 o3)), i1) <- analyze e1
  unlessZero o1 $ throwLocalError $ UnexpectedIndex (Number 0) o1
  unlessZero o2 $ throwLocalError $ UnexpectedIndex (Number 0) o2
  unlessZero o3 $ throwLocalError $ UnexpectedIndex (Number 0) o3
  unlessSubtype acctyp' (isub (Plus (IndexVariable id) (Number 1)) id acctyp) $ throwLocalError $ UnfoldableStepfunction e1 steptyp
  ((acctyp'', i2), wc1) <- withWireCount $ analyze e2
  unlessSubtype acctyp'' (isub (Number 0) id acctyp) $ throwLocalError $ UnfoldableAccumulator e2 acctyp''
  ((argtyp@(TList arglen elemtyp'), i3), wc2) <- withWireCount $ analyze e3
  unlessSubtype elemtyp' elemtyp $ throwLocalError $ UnfoldableArg e3 argtyp
  -- width upper bound of ONLY fold execution: max(#(acctyp{0/i},maximum[i<arglen] stepwidth + (arglen-(i+1))*#(elemtyp)))
  let k1 = Max (wireCount (isub (Number 0) id acctyp)) (Maximum id arglen (Plus stepwidth (Mult (Minus arglen (Plus (IndexVariable id) (Number 1))) (wireCount elemtyp))))
  -- the total upper bound takes into consideration the evaluation of e1, e2, e3 and the fold execution
  -- max(i1 + wires in e2 and e3, i2 + wires in e3, i3 + wires in the result of e2, k1):
  let k2 = Max (Plus i1 (Plus wc1 wc2)) (Max (Plus i2 wc2) (Max (Plus i3 (wireCount (isub (Number 0) id acctyp))) k1))
  return (isub arglen id acctyp, k2)
-- APPLICATION (FUNCTIONS)
analyze e@(EApp e1 e2) = withScope e $ do
  (TArrow annotyp typ3 j1 j2, i1) <- analyze e1
  ((typ2, i2), wc) <- withWireCount $ analyze e2
  unlessSubtype typ2 annotyp $ do
    actual <- runSimplifyType typ2
    throwLocalError $ UnexpectedType e2 annotyp actual
  -- max(i1 + wires in e2, i2 + j2, j1):
  let k = Max (Plus i1 wc) (Max (Plus i2 j2) j1)
  return (typ3, k)
-- APPLY (CIRCUITS)
analyze e@(EApply e1 e2) = withScope e $ do
  (TCirc j intyp outtyp, i1) <- analyze e1
  ((typ2, i2), wc) <- withWireCount $ analyze e2
  unlessSubtype typ2 intyp $ do
    expected <- runSimplifyType intyp
    actual <- runSimplifyType typ2
    throwLocalError $ UnexpectedType e2 expected actual
  -- max(i1 + wires in e2, i2, j):
  let k = Max (Plus i1 wc) (Max i2 j)
  return (outtyp, k)
-- BOX
analyze e@(EBox anno e1) = withScope e $ case anno of
  Just annotyp -> do
    (typ1@(TBang (TArrow typ2 typ3 j1 _)), i) <- analyze e1
    unlessSubtype annotyp typ2 $ throwLocalError $ UnboxableType e1 typ1
    unless (isBundleType typ3) $ throwLocalError $ UnboxableType e1 typ1
    return (TCirc j1 annotyp typ3, i)
  Nothing -> error "Internal error: box without type annotation"
-- LET-IN
analyze e@(ELet p e1 e2) = withScope e $ do
  (typ1, i1) <- analyze e1
  (ids, typs) <- makePatternBindings p typ1 SizedLists
  ((typ2, i2), wc) <- withWireCount $ withBoundVariables ids typs $ analyze e2
  -- max(i1 + wires in e2, i2):
  let k = Max (Plus i1 wc) i2
  return (typ2, k)
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
  (TBang typ', i) <- analyze e1
  return (typ', i)
-- INDEX ABSTRACTION
analyze e@(EIAbs id e1) = withScope e $ do
  ((typ, i), wc) <- withWireCount $ withBoundIndexVariable id $ analyze e1
  return (TIForall id typ i wc, wc)
-- INDEX APPLICATION
analyze e@(EIApp e1 g) = withScope e $ do
  (TIForall id typ2 j1 _, i) <- analyze e1
  checkWellFormedness g
  return (isub g id typ2, Max i (isub g id j1))
-- CONSTANTS
analyze e@(EConst c) = withScope e $ return (typeOf c, Number 0)
-- ASSUMPTION (unsafe)
analyze e@(EAssume e1 annotyp) = withScope e $ do
  checkWellFormedness annotyp
  (_, i) <- analyze e1
  return (annotyp, i)

--- EXPORTED WRAPPER FUNCTIONS ---------------------------------------------------------------

-- | @runAnalysisWith env e@ runs the whole type inference pipeline on expression @e@,
-- using the typing environment @env@.
runAnalysisWith :: TypingEnvironment -> Expr -> IO (Either TypeError (Type, Index))
runAnalysisWith env e = runExceptT $ do
  e' <- runAnnotation env e
  ((typ, i), remaining) <- runTypeDerivation (analyze e') env
  when (envIsLinear remaining) $ do
    let remainingNames = [id | (id, bs) <- Map.toList (typingContext remaining), any mustBeUsed bs]
    throwError $ UnusedLinearVariable (head remainingNames) [e]
  return (typ, i)

-- | @runAnalysis e sh@ runs the whole type inference pipeline on expression @e@,
-- using the handle @sh@ to communicate with the SMT solver.
runAnalysis :: Expr -> SolverHandle -> IO (Either TypeError (Type, Index))
runAnalysis e sh = runAnalysisWith (emptyEnv sh) e