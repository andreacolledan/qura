module Lang.Analysis.Pre
  ( annotateNil,
  )
where

import Index.AST
import Lang.Type.AST
import Lang.Type.Unify
import Lang.Expr.AST
import Lang.Expr.Constant
import Lang.Analysis.Derivation
import Data.Foldable
import Control.Monad.Extra

--- PREPROCESSING MODULE ------------------------------------------------------------------------------------
---
--- This module defines the preprocessing stage of type inference.
--- Actual inference (i.e. unification) is only carried out at this stage.
--- Right now, this stage is responsible for checking obvious (i.e. not having to do with indices)
--- type errors and annotating empty lists with the correct parameter type.
-------------------------------------------------------------------------------------------------------------

-- At this stage, placeholder for all indices, which are irrelevant
irr :: Index
irr = IndexVariable "_"

-- | @ inferBaseType e @ infers the type of expression @e@, without indices, annotating intermediate expressions as necessary.
-- The result is a triple @(e', t, s)@, where @e'@ is the annotated expression, @t@ is its type, and @s@ is the compound
-- type substitution that was applied to the expression.
-- TODO: many unnecessary substitutions are happening, their application could be better tailored to the specific typing case
inferBaseType :: Expr -> TypeDerivation (Expr, Type, TypeSubstitution)
-- UNIT
inferBaseType EUnit = withScope EUnit $ return (EUnit, TUnit, mempty)
-- VARIABLE
inferBaseType e@(EVar id) = withScope e $ do
  typ <- typingContextLookup id
  return (EVar id, typ, mempty)
-- TUPLE
inferBaseType e@(ETuple es) = withScope e $ do
  when (length es < 2) $ error "Internal error: tuple with less than 2 elements escaped parser."
  (es', typs, subs) <- unzip3 <$> mapM inferBaseType es
  let sub = fold subs
  return (tsub sub $ ETuple es', tsub sub $ TTensor typs, sub)
-- NIL
inferBaseType e@(ENil _) = withScope e $ do
  typ <- TVar <$> makeFreshVariable "a"
  return (ENil (Just typ), TList irr typ, mempty)
-- ABSTRACTION
inferBaseType e@(EAbs p annotyp e1) = withScope e $ do
  (ids, annotyps) <- makePatternBindings Nothing p annotyp
  (e1', typ1, sub1) <- withBoundVariables ids annotyps $ inferBaseType e1
  return (EAbs p (tsub sub1 annotyp) e1', tsub sub1 (TArrow annotyp typ1 irr irr), sub1)
-- LIFT
inferBaseType e@(ELift e1) = withScope e $ do
  (e1', typ1, sub1) <- withNonLinearContext $ inferBaseType e1
  return (ELift e1', TBang typ1, sub1)
-- CONS
inferBaseType e@(ECons e1 e2) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  (e2', typ2, sub2) <- inferBaseType e2
  sub3 <- unify e2 typ2 (TList irr (tsub sub2 typ1))
  let sub = sub3 <> sub2 <> sub1
  return (tsub sub (ECons e1' e2'), TList irr (tsub sub typ1), sub)
-- LET-IN
inferBaseType e@(ELet p e1 e2) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  (ids, typs) <- makePatternBindings Nothing p typ1
  (e2', typ2, sub2) <- withBoundVariables ids typs $ inferBaseType e2
  let sub = sub2 <> sub1
  return (tsub sub (ELet p e1' e2'), tsub sub typ2, sub)
-- ANNOTATION
inferBaseType e@(EAnno e1 annotyp) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  sub2 <- unify e1 typ1 annotyp
  let sub = sub2 <> sub1
  return (tsub sub (EAnno e1' annotyp), tsub sub annotyp, sub)
-- FORCE
inferBaseType e@(EForce e1) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  typ1' <- TVar <$> makeFreshVariable "a"
  sub2 <- unify e1 typ1 (TBang typ1')
  let sub = sub2 <> sub1
  return (tsub sub (EForce e1'), tsub sub typ1', sub)
-- APPLICATION (FUNCTIONS)
inferBaseType e@(EApp e1 e2) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  (e2', typ2, sub2) <- inferBaseType e2
  typ1c <- TVar <$> makeFreshVariable "a"
  sub3 <- unify e1 (tsub sub2 typ1) (TArrow typ2 typ1c irr irr)
  let sub = sub3 <> sub2 <> sub1
  return (tsub sub (EApp e1' e2'), tsub sub typ1c, sub)
-- APPLY (CIRCUITS)
inferBaseType e@(EApply e1 e2) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  (e2', typ2, sub2) <- inferBaseType e2
  btc <- TVar <$> makeFreshVariable "a"
  let sub = sub2 <> sub1
  unless (isBundleType typ2) $ throwLocalError $ ExpectedBundleType e2 typ2
  sub3 <- unify e1 (tsub sub typ1) (TCirc irr typ2 btc)
  let sub = sub3 <> sub2 <> sub1
  return (tsub sub (EApply e1' e2'), tsub sub btc, sub)
-- BOX
inferBaseType e@(EBox _ e1) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  typ1' <- TVar <$> makeFreshVariable "a"
  typ1'' <- TVar <$> makeFreshVariable "a"
  sub2 <- unify e1 typ1 (TBang (TArrow typ1' typ1'' irr irr))
  let sub = sub2 <> sub1
  unless (isBundleType (tsub sub typ1') && isBundleType (tsub sub typ1'')) $ throwLocalError $ UnboxableType e1 (tsub sub typ1)
  return (tsub sub (EBox (Just (tsub sub typ1')) e1'), tsub sub (TCirc irr (tsub sub typ1') (tsub sub typ1'')), sub)
-- FOLD
inferBaseType e@(EFold e1 e2 e3) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  (e2', typ2, sub2) <- inferBaseType e2
  (e3', typ3, sub3) <- inferBaseType e3
  elemtyp <- TVar <$> makeFreshVariable "a"
  acctyp <- TVar <$> makeFreshVariable "a"
  let sub = sub3 <> sub2 <> sub1
  stepsub <- unify e1 (tsub sub typ1) (TBang (TIForall "_" (TArrow (TTensor [acctyp, elemtyp]) acctyp irr irr) irr irr))
  let sub = stepsub <> sub3 <> sub2 <> sub1
  accsub <- unify e2 (tsub sub typ2) (tsub sub acctyp)
  let sub = accsub <> stepsub <> sub3 <> sub2 <> sub1
  argsub <- unify e3 (tsub sub typ3) (tsub sub $ TList irr elemtyp)
  let sub = argsub <> accsub <> stepsub <> sub3 <> sub2 <> sub1
  return (tsub sub (EFold e1' e2' e3'), tsub sub acctyp, sub)
-- INDEX ABSTRACTION
inferBaseType e@(EIAbs id e1) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  return (EIAbs id e1', TIForall id typ1 irr irr, sub1)
-- INDEX APPLICATION
inferBaseType e@(EIApp e1 g) = withScope e $ do
  (e1', typ1, sub1) <- inferBaseType e1
  typ1' <- TVar <$> makeFreshVariable "a"
  sub2 <- unify e1 typ1 (TIForall "i" typ1' irr irr)
  let sub = sub2 <> sub1
  return (tsub sub (EIApp e1' g), tsub sub typ1', sub)
-- CONSTANTS
inferBaseType e@(EConst c) = withScope e $ return (EConst c, typeOf c, mempty)
-- ASSUME
inferBaseType e@(EAssume e1 annotyp) = withScope e $ do
  (e1', _, sub1) <- inferBaseType e1
  return (EAssume e1' annotyp, annotyp, sub1)

--- TOP-LEVEL EXPORTED FUNCTIONS -------------------------------------------------------

-- | @ annotateNil env e @ annotates all the empty lists in expression @e@ with the correct parameter type under environment @env@.
-- If successful, returns the annotated expression. Otherwise, returns the error.
annotateNil :: TypingEnvironment -> Expr -> DerivationResult Expr
annotateNil env e = extractFirst <$> evalTypeDerivation (inferBaseType e) env
  where extractFirst (a, _, _) = a
