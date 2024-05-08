module Lang.Analysis.InferSpec (spec) where

import Data.Either
import Index.AST
import Index.Semantics
import Lang.Type.AST
import Lang.Type.Semantics
import Lang.Expr.AST
import Lang.Analysis.Infer
import Test.Hspec
import Lang.Expr.Constant
import Lang.Analysis.Derivation
    ( emptyEnv, makeEnv, makeEnvForall, TypeError, TypingEnvironment )
import Solving.CVC5 (withSolver, SolverHandle)
import Lang.Expr.Pattern

-- | Helper function used specifically in a test environment. @runInferenceForTesting env expr qfh@ runs inference
-- on @expr@ under environment @env@ using @qfh@ to access the solver. It returns 'Either' a 'TypeError' ('Left') or
-- a pair of the inferred type and index ('Right'). The type and index are simplified aggressively for readability.
runInferenceForTesting :: TypingEnvironment -> Expr -> SolverHandle -> IO (Either TypeError (Type, Index))
runInferenceForTesting env expr qfh = do
  outcome <- runTypeInferenceWith env expr qfh
  case outcome of
    Left err -> return $ Left err
    Right (t, i) -> do
      t' <- simplifyType qfh t
      i' <- simplifyIndexStrong qfh i
      return $ Right (t', i')

-- | Like 'shouldSatisfy', but for 'IO' actions.
shouldSatisfyIO :: (HasCallStack, Show a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyIO` p = action >>= (`shouldSatisfy` p)

spec :: Spec
spec = around (withSolver Nothing) $ do
    describe "type inference on values" $ do
    -- Tests for the type inference of terms that do not produce any side-effect
      context "when typing the unit value" $ do
        it "produces the unit type if the context is empty" $ \qfh -> do
          -- ∅;∅;∅ ⊢ () ==> () ; 0
          runInferenceForTesting emptyEnv EUnit qfh `shouldReturn` Right (TUnit, Number 0)
        it "produces the unit type if the context is non-linear" $ \qfh -> do
          -- ∅;x:(),y:Circ[1](Qubit,Qubit);∅ ⊢ () =/=> () ; 0
          let gamma = [("x", TUnit), ("y", TCirc (Number 1) (TWire Qubit) (TWire Qubit))]
          runInferenceForTesting (makeEnv gamma) EUnit qfh `shouldReturn` Right (TUnit, Number 0)
        it "fails when there are linear resources in the environment" $ \qfh -> do
          -- ∅;x:Qubit;∅ ⊢ () =/=>
          let gamma = [("x", TWire Qubit)]
          runInferenceForTesting (makeEnv gamma) EUnit qfh `shouldSatisfyIO` isLeft
      context "when typing variables" $ do
        it "produces the type of the variable if the rest of the context is empty" $ \qfh -> do
          -- ∅;x:Qubit;∅ ⊢ x ==> Qubit ; 1
          let gamma = [("x", TWire Qubit)]
          runInferenceForTesting (makeEnv gamma) (EVar "x") qfh `shouldReturn` Right (TWire Qubit, Number 1)
        it "produces the type of the variable if the rest of the context is nonlinear" $ \qfh -> do
          -- ∅;x:(),y:Qubit;∅ ⊢ y ==> Qubit ; 1
          let gamma = [("x", TUnit), ("y", TWire Qubit)]
          runInferenceForTesting (makeEnv gamma) (EVar "y") qfh `shouldReturn` Right (TWire Qubit, Number 1)
        it "fails when there are other linear resources in the environment" $ \qfh -> do
          -- ∅;x:Qubit,y:Qubit;∅ ⊢ y =/=>
          let gamma = [("x", TWire Qubit), ("y", TWire Qubit)]
          runInferenceForTesting (makeEnv gamma) (EVar "y") qfh `shouldSatisfyIO` isLeft
      context "when typing pairs" $ it "produces the correct tensor type and the sum of the wirecounts of the elements" $ \qfh -> do
        -- ∅;∅;∅ ⊢ ((),()) ==> ((),()) ; 0
        let expr = ETuple [EUnit, EUnit]
        let expected = (TTensor [TUnit, TUnit], Number 0)
        runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        -- ∅;x:Qubit,y:Bit;∅ ⊢ (x,y) ==> (Qubit,Bit) ; 2
        let gamma = [("x", TWire Qubit), ("y", TWire Bit)]
        let expr = ETuple [EVar "x", EVar "y"]
        let expected = (TTensor [TWire Qubit, TWire Bit], Number 2)
        runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        -- ∅;x:Qubit,y:Bit,z:Qubit;∅ ⊢ (x,(y,z)) ==> (Qubit,(Bit,Qubit)) ; 3
        let gamma = [("x", TWire Qubit), ("y", TWire Bit), ("z", TWire Qubit)]
        let expr = ETuple [EVar "x", ETuple [EVar "y", EVar "z"]]
        let expected = (TTensor [TWire Qubit, TTensor [TWire Bit, TWire Qubit]], Number 3)
        runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
      context "when typing the empty list" $ do
        it "produces an empty list type and wirecount zero if the context is empty" $ \qfh -> do
          -- ∅;∅;∅ ⊢ [] :: List[0] () ==> List[0] () ; 0
          runInferenceForTesting emptyEnv (EAnno (ENil Nothing) (TList (Number 0) TUnit)) qfh `shouldReturn` Right (TList (Number 0) TUnit, Number 0)
        it "produces an empty list type and wirecount zero if the context is non-linear" $ \qfh -> do
          -- ∅;x:();∅ ⊢ [] :: List[0] () ==> List[0] () ; 0
          let gamma = [("x", TUnit)]
          runInferenceForTesting (makeEnv gamma) (EAnno (ENil Nothing) (TList (Number 0) TUnit)) qfh `shouldReturn` Right (TList (Number 0) TUnit, Number 0)
        it "fails when there are linear resources in the environment" $ \qfh -> do
          -- ∅;x:Qubit;∅ ⊢ [] :: List[0] () =/=> 
          let gamma = [("x", TWire Qubit)]
          runInferenceForTesting (makeEnv gamma) (EAnno (ENil Nothing) (TList (Number 0) TUnit)) qfh `shouldSatisfyIO` isLeft
        it "fails when the parameter type is unconstrained" $ \qfh -> do
          -- ∅;∅;∅ ⊢ [] =/=>
          runInferenceForTesting emptyEnv (ENil Nothing) qfh `shouldSatisfyIO` isLeft
      context "when typing cons" $ do
        it "produces the correct list type and the wirecount of the elements times the length of the list" $ \qfh -> do
          -- ∅;x:Qubit;∅ ⊢ x:[] ==> List[1] Qubit ; 1
          let gamma = [("x", TWire Qubit)]
          let expr = ECons (EVar "x") (ENil Nothing)
          let expected = (TList (Number 1) (TWire Qubit), Number 1)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
          -- ∅;x:Qubit,y:Qubit;∅ ⊢ x:y:[] ==> List[2] (Qubit,Bit) ; 2
          let gamma = [("x", TWire Qubit), ("y", TWire Qubit)]
          let expr = ECons (EVar "x") (ECons (EVar "y") (ENil Nothing))
          let expected = (TList (Number 2) (TWire Qubit), Number 2)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
          -- ∅;x:(Bit,Bit),xs:List[100] (Bit,Bit) ⊢ x:xs ==> List[101] (Bit,Bit) ; 202
          let gamma = [("x", TTensor [TWire Bit, TWire Bit]), ("xs", TList (Number 100) (TTensor [TWire Bit, TWire Bit]))]
          let expr = ECons (EVar "x") (EVar "xs")
          let expected = (TList (Number 101) (TTensor [TWire Bit, TWire Bit]), Number 202)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "fails when the head is of a different type than the rest of the list" $ \qfh -> do
          -- ∅;x:Qubit,y:Bit;∅ ⊢ x:y:[] =/=> 
          let gamma = [("x", TWire Qubit), ("y", TWire Bit)]
          let expr = ECons (EVar "x") (ECons (EVar "y") (ENil Nothing))
          runInferenceForTesting (makeEnv gamma) expr qfh `shouldSatisfyIO` isLeft
      context "when typing abstractions" $ do
        it "produces the correct arrow type and a wirecount equal to the second annotation of the arrow" $ \qfh -> do
          -- ∅;∅;∅ ⊢ \x :: Qubit . x ==> Qubit ->[1,0] Qubit ; 0
          let expr = EAbs (PVar "x") (TWire Qubit) (EVar "x")
          let expected = (TArrow (TWire Qubit) (TWire Qubit) (Number 1) (Number 0), Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
          -- ∅;x:Qubit;∅ ⊢ \y :: Bit . (x,y) ==> Bit ->[2,1] (Qubit,Bit) ; 1
          let gamma = [("x", TWire Qubit)]
          let expr = EAbs (PVar "y") (TWire Bit) (ETuple [EVar "x", EVar "y"])
          let expected = (TArrow (TWire Bit) (TTensor [TWire Qubit, TWire Bit]) (Number 2) (Number 1), Number 1)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
          -- i;x:Qubit;∅ ⊢ \y :: List[i] Qubit . x:y => List[i] Qubit ->[i+1,1] List[i+1] Qubit ; 1
          let gamma = [("x", TWire Qubit)]
          let expr = EAbs (PVar "y") (TList (IndexVariable "i") (TWire Qubit)) (ECons (EVar "x") (EVar "y"))
          let expected = (TArrow (TList (IndexVariable "i") (TWire Qubit)) (TList (Plus (IndexVariable "i") (Number 1)) (TWire Qubit)) (Plus (Number 1) (IndexVariable "i")) (Number 1), Number 1)
          let theta = ["i"]
          runInferenceForTesting  (makeEnvForall theta gamma) expr qfh `shouldReturn` Right expected
        it "succeeds if the argument is of parameter type and is used more than once" $ \qfh -> do
          -- ∅;∅;∅ ⊢ \x :: () . (x,x) ==> () ->[0,0] ((),()) ; 0
          let expr = EAbs (PVar "x") TUnit (ETuple [EVar "x", EVar "x"])
          let expected = (TArrow TUnit (TTensor [TUnit, TUnit]) (Number 0) (Number 0), Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "succeeds if the argument is of parameter type and is not used" $ \qfh -> do
          -- ∅;∅;∅ ⊢ \x :: () . () ==> () ->[0,0] () ; 0
          let expr = EAbs (PVar "x") TUnit EUnit
          let expected = (TArrow TUnit TUnit (Number 0) (Number 0), Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "fails when the body uses the argument more than once" $ \qfh -> do
          -- ∅;∅;∅ ⊢ \x :: Qubit . (x,x) =/=>
          let expr = EAbs (PVar "x") (TWire Qubit) (ETuple [EVar "x", EVar "x"])
          runInferenceForTesting emptyEnv expr qfh `shouldSatisfyIO` isLeft
        it "fails when the body does not use the argument" $ \qfh -> do
          -- ∅;∅;∅ ⊢ \x :: Qubit . () =/=>
          let expr = EAbs (PVar "x") (TWire Qubit) EUnit
          runInferenceForTesting emptyEnv expr qfh `shouldSatisfyIO` isLeft
        it "succeeds in the case of shadowing, provided every linear variable is used once" $ \qfh -> do
          -- ∅;x:Qubit;∅ ⊢ (\x :: Qubit . x) x ==> Qubit ; 1
          let gamma = [("x", TWire Qubit)]
          let expr = EApp (EAbs (PVar "x") (TWire Qubit) (EVar "x")) (EVar "x")
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right (TWire Qubit, Number 1)
        it "fails if the formal parameter type mentions an undeclared index variable" $ \qfh -> do
          -- ∅;∅;∅ ⊢ \x :: List[i] Qubit . x =/=>
          let expr = EAbs (PVar "x") (TList (IndexVariable "i") (TWire Qubit)) (EVar "x")
          runInferenceForTesting emptyEnv expr qfh `shouldSatisfyIO` isLeft
      context "when typing lifted expressions" $ do
        it "produces the correct bang type and a wirecount of zero" $ \qfh -> do
          -- ∅;∅;∅ ⊢ lift () ==> !() ; 0
          let expr = ELift EUnit
          let expected = (TBang TUnit, Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
          -- ∅;∅;∅ ⊢ !(\x :: Qubit . x) ==> !(Qubit ->[1,0] Qubit) ; 0
        it "succeeds if the lifted expression consumes linear resources from within its scope" $ \qfh -> do
          -- ∅;∅;∅ ⊢ lift (\x::Qubit . x) ==> !(Qubit ->[1,0] Qubit) ; 0
          let expr = ELift (EAbs (PVar "x") (TWire Qubit) (EVar "x"))
          let expected = (TBang (TArrow (TWire Qubit) (TWire Qubit) (Number 1) (Number 0)), Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "fails if the lifted expression consumes linear resources from outside its scope" $ \qfh -> do
          -- ∅;x:Qubit;∅ ⊢ lift x =/=> 
          let gamma = [("x", TWire Qubit)]
          let expr = ELift (EVar "x")
          runInferenceForTesting (makeEnv gamma) expr qfh `shouldSatisfyIO` isLeft
        it "fails if the lifted expression builds a circuit" $ \qfh -> do
          -- ∅;∅;∅ ⊢ lift apply(QInit0,()) =/=>
          let expr = ELift (EApply (EVar "QInit0") EUnit)
          runInferenceForTesting emptyEnv expr qfh `shouldSatisfyIO` isLeft
      context "when typing index abstraction" $ do
        it "produces the correct forall type and the wirecount of the environment" $ \qfh -> do
          -- ∅;∅;∅ ⊢ @i . () ==> i ->[0,0] () ; 0
          let expr = EIAbs "i" EUnit
          let expected = (TIForall "i" TUnit (Number 0) (Number 0), Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
          -- ∅;x:Qubit;∅ ⊢ @i . \xs :: List[i] Qubit . x:xs ==> i ->[1,1] List[i] Qubit ->[i+1,1] List[i+1] Qubit ; 1
          let gamma = [("x", TWire Qubit)]
          let expr = EIAbs "i" (EAbs (PVar "xs") (TList (IndexVariable "i") (TWire Qubit)) (ECons (EVar "x") (EVar "xs")))
          let expected = (TIForall "i" (TArrow (TList (IndexVariable "i") (TWire Qubit)) (TList (Plus (IndexVariable "i") (Number 1)) (TWire Qubit)) (Plus (Number 1) (IndexVariable "i")) (Number 1)) (Number 1) (Number 1), Number 1)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "fails if the index variable already exists" $ \qfh -> do
          -- i;∅;∅ ⊢ @i . () =/=>
          let expr = EIAbs "i" EUnit
          runInferenceForTesting (makeEnvForall ["i"] []) expr qfh `shouldSatisfyIO` isLeft
          -- ∅;∅;∅ ⊢ @i . (@i . ()) @i =/=>
          let expr = EIAbs "i" (EIApp (EIAbs "i" EUnit) (IndexVariable "i"))
          runInferenceForTesting emptyEnv expr qfh `shouldSatisfyIO` isLeft
    describe "type inference on effectful expressions" $ do
    -- Tests for the type inference of terms that produce side-effects
    -- Either by nature or because some sub-terms are effectful
      context "when typing pairs" $ do
        it "produces the correct tensor type and upper bound when the first element computes" $ \qfh -> do
          -- ∅;f:Qubit ->[2,0] Bit,x:Qubit,y:Bit;∅ ⊢ (f x, y) ==> (Bit,Bit) ; 3
          -- while f x builds something of width 2, y of width 1 flows alongside: width is 3
          let gamma = [
                ("f", TArrow (TWire Qubit) (TWire Bit) (Number 2) (Number 0)),
                ("x", TWire Qubit), ("y", TWire Bit)]
          let expr = ETuple [EApp (EVar "f") (EVar "x"), EVar "y"]
          let expected = (TTensor [TWire Bit, TWire Bit], Number 3)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the correct tensor type and upper bound when the second element computes" $ \qfh -> do
          -- ∅;x:Qubit,y:Qubit,g:Qubit ->[2,0] Bit;∅ ⊢ (x, g y) ==> (Qubit,Bit) ; 3
          -- while g y builds something of width 2, x of width 1 flows alongside: width is 3
          let gamma = [
                ("x", TWire Qubit), ("y", TWire Qubit),
                ("g", TArrow (TWire Qubit) (TWire Bit) (Number 2) (Number 0))]
          let expr = ETuple [EVar "x", EApp (EVar "g") (EVar "y")]
          let expected = (TTensor [TWire Qubit, TWire Bit], Number 3)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
      context "when typing cons" $ do
        it "produces the correct list type and upper bound when the head computes" $ \qfh -> do
          -- ∅;f:Qubit ->[2,0] Bit,x:Qubit,y:List[3] Bit;∅ ⊢ f x:y ==> List[4] Bit ; 5
          -- while f x builds something of width 2, y of width 3 flows alongside: width is 5
          let gamma = [
                ("f", TArrow (TWire Qubit) (TWire Bit) (Number 2) (Number 0)),
                ("x", TWire Qubit), ("y", TList (Number 3) (TWire Bit))]
          let expr = ECons (EApp (EVar "f") (EVar "x")) (EVar "y")
          let expected = (TList (Number 4) (TWire Bit), Number 5)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the correct list type and upper bound when the tail computes" $ \qfh -> do
          -- ∅;x:Qubit,y:Qubit,g:Qubit ->[7,0] List[2] Qubit ⊢ x:g y ==> List[3] Qubit ; 8
          -- while g y builds something of width 7, x of width 1 flows alongside: width is 8
          let gamma = [
                ("x", TWire Qubit), ("y", TWire Qubit),
                ("g", TArrow (TWire Qubit) (TList (Number 2) (TWire Qubit)) (Number 7) (Number 0))]
          let expr = ECons (EVar "x") (EApp (EVar "g") (EVar "y"))
          let expected = (TList (Number 3) (TWire Qubit), Number 8)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
      context "when typing application" $ do
        it "produces the correct type and upper bound when both function and argument are values" $ \qfh -> do
          -- ∅;l:Qubit;∅ ⊢ (\x::Qubit.x) l ==> Qubit ; 1
          let gamma = [("l", TWire Qubit)]
          let expr = EApp (EAbs (PVar "x") (TWire Qubit) (EVar "x")) (EVar "l")
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right (TWire Qubit, Number 1)
        it "produces the correct type and wirecount when the applied term computes" $ \qfh -> do
          -- ∅;f:Qubit ->[2,0] Qubit ->[3,1] Bit, x:Qubit, y:Qubit;∅ ⊢ (f x) y ==> Bit ; 3
          -- while f x builds something of width 2, y of width 1 flows alongside: width is 3
          let gamma = [
                ("f", TArrow (TWire Qubit) (TArrow (TWire Qubit) (TWire Bit) (Number 3) (Number 1)) (Number 2) (Number 0)),
                ("x", TWire Qubit), ("y", TWire Qubit)]
          let expr = EApp (EApp (EVar "f") (EVar "x")) (EVar "y")
          let expected = (TWire Bit, Number 3)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the correct type and wirecount when the argument computes" $ \qfh -> do
          -- ∅;f:Qubit ->[2,0] Bit, g : Qubit ->[3,1] Qubit, x:Qubit;∅ ⊢ f (g x) => Bit ; 3
          -- while g x builds something of width 3, f of width 0 flows alongside: width is 3
          let gamma = [
                ("f", TArrow (TWire Qubit) (TWire Bit) (Number 2) (Number 0)),
                ("g", TArrow (TWire Qubit) (TWire Qubit) (Number 3) (Number 1)),
                ("x", TWire Qubit)]
          let expr = EApp (EVar "f") (EApp (EVar "g") (EVar "x"))
          let expected = (TWire Bit, Number 3)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the correct type and wirecount when both function and argument compute" $ \qfh -> do
          -- ∅;f:!(Qubit ->[2,0] Bit), g : Qubit ->[3,1] Qubit, x:Qubit;∅ ⊢ (force f) (g x) => Bit ; 3
          -- while g x builds something of width 3, f of width 0 flows alongside: width is 3
          let gamma = [
                ("f", TBang (TArrow (TWire Qubit) (TWire Bit) (Number 2) (Number 0))),
                ("g", TArrow (TWire Qubit) (TWire Qubit) (Number 3) (Number 1)),
                ("x", TWire Qubit)]
          let expr = EApp (EForce (EVar "f")) (EApp (EVar "g") (EVar "x"))
          let expected = (TWire Bit, Number 3)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "succeeds if the argument is of a subtype of the formal parameter" $ \qfh -> do
          -- ∅;c:Circ[1](Qubit,Qubit);∅ ⊢ (\x::Circ[2](Qubit,Qubit).x) c ==> Circ[2](Qubit,Qubit) ; 0
          let gamma = [("c", TCirc (Number 1) (TWire Qubit) (TWire Qubit))]
          let expr = EApp (EAbs (PVar "x") (TCirc (Number 2) (TWire Qubit) (TWire Qubit)) (EVar "x")) (EVar "c")
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right (TCirc (Number 2) (TWire Qubit) (TWire Qubit), Number 0)
        it "fails if the function is not a function type" $ \qfh -> do
          -- ∅;l:Qubit;∅ ⊢ l (\x::Qubit.x) =/=>
          let gamma = [("l", TWire Qubit)]
          let expr = EApp (EVar "l") (EAbs (PVar "x") (TWire Qubit) (EVar "x"))
          runInferenceForTesting (makeEnv gamma) expr qfh `shouldSatisfyIO` isLeft
        it "fails if the argument is not of the expected type" $ \qfh -> do
          -- ∅;∅;l:Qubit ⊢ (\x::Bit.x) l =/=>
          let gamma = [("l", TWire Qubit)]
          let expr = EApp (EAbs (PVar "x") (TWire Bit) (EVar "x")) (EVar "l")
          runInferenceForTesting (makeEnv gamma) expr qfh `shouldSatisfyIO` isLeft
      context "when typing apply" $ do
        it "produces the correct type and wirecount when both function and argument are values" $ \qfh -> do
          -- ∅;∅;∅ ⊢ apply(QInit0,()) ==> Qubit ; 1
          let expr = EApply (EConst QInit0) EUnit
          let expected = (TWire Qubit, Number 1)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "produces the correct type and wirecount when the applied circuit term computes" $ \qfh -> do
          -- ∅;f:!(Qubit -o[2,0] Qubit);q:Qubit;∅ ⊢ apply(box f, q) ==> Qubit ; 2
          let gamma = [
                ("f", TBang (TArrow (TWire Qubit) (TWire Qubit) (Number 2) (Number 0))),
                ("q", TWire Qubit)]
          let expr = EApply (EBox (Just $ TWire Qubit) (EVar "f")) (EVar "q")
          let expected = (TWire Qubit, Number 2)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the correct type and wirecount when the circuit application argument computes" $ \qfh -> do
          -- ∅;rev:!(i ->[0,0] List[i] Qubit -o[i,0] List[i] Qubit),c:Circ[16](List[8] Qubit, List[8] Qubit), qs:List[8] Qubit;∅ ⊢ apply(c, (force rev) @8 qs) ==> List[8] Qubit ; 16
          let gamma = [
                ("rev", TBang (TIForall "i" (TArrow (TList (IndexVariable "i") (TWire Qubit)) (TList (IndexVariable "i") (TWire Qubit)) (IndexVariable "i") (Number 0)) (Number 0) (Number 0))),
                ("c", TCirc (Number 16) (TList (Number 8) (TWire Qubit)) (TList (Number 8) (TWire Qubit))),
                ("qs", TList (Number 8) (TWire Qubit))]
          let expr = EApply (EVar "c") (EApp (EIApp (EForce (EVar "rev")) (Number 8)) (EVar "qs"))
          let expected = (TList (Number 8) (TWire Qubit), Number 16)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the correct type and wirecount when both circuit term and argument compute" $ \qfh -> do
          -- ∅;f:!(Qubit -o[2,0] Qubit),q:Qubit,g:Qubit ->[2,0] Qubit;∅ ⊢ apply(box f, g q) ==> Qubit ; 2
          let gamma = [
                ("f", TBang (TArrow (TWire Qubit) (TWire Qubit) (Number 2) (Number 0))),
                ("q", TWire Qubit),
                ("g", TArrow (TWire Qubit) (TWire Qubit) (Number 2) (Number 0))]
          let expr = EApply (EBox (Just $ TWire Qubit) (EVar "f")) (EApp (EVar "g") (EVar "q"))
          let expected = (TWire Qubit, Number 2)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "fails if the applied term is not a circuit term" $ \qfh -> do
          -- ∅;f:Qubit ->[2,0] Qubit,q:Qubit;∅ ⊢ apply(f,q) =/=>
          let gamma = [
                ("f", TArrow (TWire Qubit) (TWire Qubit) (Number 2) (Number 0)),
                ("q", TWire Qubit)]
          let expr = EApply (EVar "f") (EVar "q")
          runInferenceForTesting (makeEnv gamma) expr qfh `shouldSatisfyIO` isLeft
        it "fails if the argument is not of the expected bundle type" $ \qfh -> do
          -- ∅;b:Bit;∅ ⊢ apply(QInit0,b) =/=>
          let gamma = [("b", TWire Bit)]
          let expr = EApply (EConst QInit0) (EVar "b")
          runInferenceForTesting (makeEnv gamma) expr qfh `shouldSatisfyIO` isLeft
      context "when typing box" $ do
        it "produces the correct type and wirecount when the argument is a value" $ \qfh -> do
          -- ∅;f:!(Qubit -o[2,0] Qubit);∅ ⊢ box f ==> Circ[2](Qubit,Qubit) ; 0
          let gamma = [("f", TBang (TArrow (TWire Qubit) (TWire Qubit) (Number 2) (Number 0)))]
          let expr = EBox (Just $ TWire Qubit) (EVar "f")
          let expected = (TCirc (Number 2) (TWire Qubit) (TWire Qubit), Number 0)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the correct type and wirecount when the argument computes" $ \qfh -> do
          -- ∅;f:() -o[3,0] !(Qubit -o[2,0] Bit);∅ ⊢ box (f ()) ==> Circ[2](Qubit,Bit) ; 3
          let gamma = [("f", TArrow TUnit (TBang (TArrow (TWire Qubit) (TWire Bit) (Number 2) (Number 0))) (Number 3) (Number 0))]
          let expr = EBox (Just $ TWire Qubit) (EApp (EVar "f") EUnit)
          let expected = (TCirc (Number 2) (TWire Qubit) (TWire Bit), Number 3)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "fails if the argument is not a duplicable function" $ \qfh -> do
          -- ∅;f:Qubit ->[2,0] Qubit;∅ ⊢ box f =/=>
          let gamma = [("f", TArrow (TWire Qubit) (TWire Qubit) (Number 2) (Number 0))]
          let expr = EBox (Just $ TWire Qubit) (EVar "f")
          runInferenceForTesting (makeEnv gamma) expr qfh `shouldSatisfyIO` isLeft
        it "fails if the argument is not a circuit building function" $ \qfh -> do
          -- ∅;f:!(Qubit -o[2,0] Circ[2](Qubit,Qubit));∅ ⊢ box f =/=>
          let gamma = [("f", TBang (TArrow (TWire Qubit) (TCirc (Number 2) (TWire Qubit) (TWire Qubit)) (Number 2) (Number 0)))]
          let expr = EBox (Just $ TWire Qubit) (EVar "f")
          runInferenceForTesting (makeEnv gamma) expr qfh `shouldSatisfyIO` isLeft
      context "when typing index application" $ it "produces the instantiated type of body expression and its wirecount" $ \qfh -> do
        -- ∅;∅;∅ ⊢ (@i . \x :: List[i] Qubit . x) @100 ==> List[100] Qubit -o[100,0] List[100] Qubit ; 0
        let expr = EIApp (EIAbs "i" (EAbs (PVar "x") (TList (IndexVariable "i") (TWire Qubit)) (EVar "x"))) (Number 100)
        let expected = (TArrow (TList (Number 100) (TWire Qubit)) (TList (Number 100) (TWire Qubit)) (Number 100) (Number 0), Number 0)
        runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
      context "when typing let-in" $ do
        it "produces the right type and wirecount when both the bound expression and the body are values" $ \qfh -> do
          -- ∅;∅;∅ ⊢ let x = () in (x,x) ==> ((),()) ; 0
          let expr = ELet (PVar "x") EUnit (ETuple [EVar "x", EVar "x"])
          let expected = (TTensor [TUnit, TUnit], Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when the bound expression computes" $ \qfh -> do
          -- ∅;∅;∅ ⊢ let x = apply(QInit0,()) in (x,()) ==> (Qubit,()) ; 1
          let expr = ELet (PVar "x") (EApply (EConst QInit0) EUnit) (ETuple [EVar "x", EUnit])
          let expected = (TTensor [TWire Qubit, TUnit], Number 1)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when the body computes" $ \qfh -> do
          -- ∅;∅;∅ ⊢ let x = () in apply(QInit0,x) ==> Qubit ; 1
          let expr = ELet (PVar "x") EUnit (EApply (EConst QInit0) (EVar "x"))
          let expected = (TWire Qubit, Number 1)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when both the bound expression and the body compute" $ \qfh -> do
          -- ∅;∅;∅ ⊢ let x = apply(QInit0,()) in apply(Hadamard,x) ==> Qubit ; 1
          let expr = ELet (PVar "x") (EApply (EConst QInit0) EUnit) (EApply (EConst Hadamard) (EVar "x"))
          let expected = (TWire Qubit, Number 1)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
      context "when typing the destructuring let-in" $ do
        it "produces the right type and wirecount when both the bound expression and the body are values" $ \qfh -> do
          -- ∅;∅;∅ ⊢ let (x,y) = ((),()) in (y,x) ==> ((),()) ; 0
          let expr = ELet (PTuple [PVar "x", PVar "y"]) (ETuple [EUnit, EUnit]) (ETuple [EVar "y", EVar "x"])
          let expected = (TTensor [TUnit, TUnit], Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when the bound expression computes" $ \qfh -> do
          -- ∅;initTwo:() -o[2,0] (Qubit,Qubit);∅ ⊢ let (x,y) = initTwo () in (y,x) ==> (Qubit,Qubit) ; 2
          let gamma = [("initTwo", TArrow TUnit (TTensor [TWire Qubit, TWire Qubit]) (Number 2) (Number 0))]
          let expr = ELet (PTuple [PVar "x", PVar "y"]) (EApp (EVar "initTwo") EUnit) (ETuple [EVar "y", EVar "x"])
          let expected = (TTensor [TWire Qubit, TWire Qubit], Number 2)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when the body computes" $ \qfh -> do
          -- ∅;initTwo:() -o[2,0] (Qubit,Qubit);∅ ⊢ let (x,y) = ((),()) in initTwo y ==> (Qubit,Qubit) ; 2
          let gamma = [("initTwo", TArrow TUnit (TTensor [TWire Qubit, TWire Qubit]) (Number 2) (Number 0))]
          let expr = ELet (PTuple [PVar "x", PVar "y"]) (ETuple [EUnit, EUnit]) (EApp (EVar "initTwo") (EVar "y"))
          let expected = (TTensor [TWire Qubit, TWire Qubit], Number 2)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when both the bound expression and the body compute" $ \qfh -> do
          -- ∅;initTwo:() -o[2,0] (Qubit,Qubit);∅ ⊢ let (x,y) = initTwo () in apply(CNot,(y,x)) ==> (Qubit,Qubit) ; 2
          let gamma = [("initTwo", TArrow TUnit (TTensor [TWire Qubit, TWire Qubit]) (Number 2) (Number 0))]
          let expr = ELet (PTuple [PVar "x", PVar "y"]) (EApp (EVar "initTwo") EUnit) (EApply (EConst CNot) (ETuple [EVar "y", EVar "x"]))
          let expected = (TTensor [TWire Qubit, TWire Qubit], Number 2)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "fails when the bound expression is not of tensor type" $ \qfh -> do
          -- ∅;∅;∅ ⊢ let (x,y) = apply(QInit0,()) in (y,x) =/=>
          let expr = ELet (PTuple [PVar "x", PVar "y"]) (EApply (EConst QInit0) EUnit) (ETuple [EVar "y", EVar "x"])
          runInferenceForTesting emptyEnv expr qfh `shouldSatisfyIO` isLeft
      context "when typing force" $ do
        it "produces the right type and wirecount when the argument is a value" $ \qfh -> do
          -- ∅;∅;∅ ⊢ force (lift \x::Qubit . x) ==> Qubit -o[1,0] Qubit ; 0
          let expr = EForce (ELift (EAbs (PVar "x") (TWire Qubit) (EVar "x")))
          let expected = (TArrow (TWire Qubit) (TWire Qubit) (Number 1) (Number 0), Number 0)
          runInferenceForTesting  emptyEnv expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when the argument computes" $ \qfh -> do
          -- ∅;f:i ->[1,0] !(Qubit -o[i,0] Qubit);∅ ⊢ force (f @5) ==> Qubit -o[5,0] Qubit ; 1
          let gamma = [("f", TIForall "i" (TBang (TArrow (TWire Qubit) (TWire Qubit) (IndexVariable "i") (Number 0))) (Number 1) (Number 0))]
          let expr = EForce (EIApp (EVar "f") (Number 5))
          let expected = (TArrow (TWire Qubit) (TWire Qubit) (Number 5) (Number 0), Number 1)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "fails if the argument is not of bang type" $ \qfh -> do
          -- ∅;∅;∅ ⊢ force QInit0 =/=>
          let expr = EForce (EConst QInit0)
          runInferenceForTesting emptyEnv expr qfh `shouldSatisfyIO` isLeft
      context "when typing fold" $ do
        it "produces the right type and wirecount when all the arguments are values" $ \qfh -> do
          -- ∅;step:!(i ->[0,0] (List[i] Qubit, Qubit) -o[1,0] List[i+1] Qubit); l:Qubit,k:Qubit ⊢ fold (step, [], [l,k]) ==> List[2] Qubit ; 2
          let gamma = [
                ("step", TBang (TIForall "i" (TArrow (TTensor [TList (IndexVariable "i") (TWire Qubit), TWire Qubit]) (TList (Plus (IndexVariable "i") (Number 1)) (TWire Qubit)) (Number 1) (Number 0)) (Number 0) (Number 0))),("l", TWire Qubit), ("k", TWire Qubit)]
          let expr = EFold (EVar "step") (ENil Nothing) (ECons (EVar "l") (ECons (EVar "k") (ENil Nothing)))
          let expected = (TList (Number 2) (TWire Qubit), Number 2)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when the step argument computes" $ \qfh -> do
          -- ∅;pstep:j ->[2*j,0] !(i ->[0,0] (List[i] Qubit, Qubit) -o[1,0] List[i+1] Qubit), l:Qubit,k:Qubit;∅ ⊢ fold (pstep @2, [], [l,k]) ==> List[2] Qubit ; 6
          -- while pstep @ 2 is building a circuit of width 4, l and k pass besides it: total width is 6
          let gamma = [
                ("pstep", TIForall "j" (TBang (TIForall "i" (TArrow (TTensor [TList (IndexVariable "i") (TWire Qubit), TWire Qubit]) (TList (Plus (IndexVariable "i") (Number 1)) (TWire Qubit)) (Number 1) (Number 0)) (Number 0) (Number 0))) (Mult (Number 2) (IndexVariable "j")) (Number 0)),("l", TWire Qubit), ("k", TWire Qubit)]
          let expr = EFold (EIApp (EVar "pstep") (Number 2)) (ENil Nothing) (ECons (EVar "l") (ECons (EVar "k") (ENil Nothing)))
          let expected = (TList (Number 2) (TWire Qubit), Number 6)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
        it "produces the right type and wirecount when the starting accumulator argument computes" $ \qfh -> do
          -- ∅;step:!(i ->[0,0] (List[1+2*i] Qubit, Qubit) -o[1+2*(i+1),0] List[1+2*(i+1)] Qubit), inc: j ->[0,0] List[j] Qubit -o[j+1,0] List[j+1] Qubit;∅ ⊢ fold (step, (inc @0) [], [l,k]) ==> List[5] Qubit ; 5
          let gamma = [
                ("step", TBang (TIForall "i" (TArrow (TTensor [TList (Plus (Number 1) (Mult (Number 2) (IndexVariable "i"))) (TWire Qubit), TWire Qubit]) (TList (Plus (Number 1) (Mult (Number 2) (Plus (IndexVariable "i") (Number 1)))) (TWire Qubit)) (Plus (Number 1) (Mult (Number 2) (Plus (IndexVariable "i") (Number 1)))) (Number 0)) (Number 0) (Number 0))),
                ("inc", TIForall "j" (TArrow (TList (IndexVariable "j") (TWire Qubit)) (TList (Plus (IndexVariable "j") (Number 1)) (TWire Qubit)) (Plus (IndexVariable "j") (Number 1)) (Number 0)) (Number 0) (Number 0)),("l", TWire Qubit), ("k", TWire Qubit)]
          let expr = EFold (EVar "step") (EApp (EIApp (EVar "inc") (Number 0)) (ENil Nothing)) (ECons (EVar "l") (ECons (EVar "k") (ENil Nothing)))
          let expected = (TList (Number 5) (TWire Qubit), Number 5)
          runInferenceForTesting  (makeEnv gamma) expr qfh `shouldReturn` Right expected
          pending -- TODO
        it "produces the right type and wirecount when the list argument computes" $ const pending -- TODO
        it "produces the right type and wirecount when all the arguments compute" $ const pending -- TODO
        -- We do not test other combinations of computing arguments so far
        it "fails if the step function is not duplicable" $ const pending -- TODO
        it "fails if the step argument is not universally quantified" $ const pending -- TODO
        it "fails if the step argument is not of function type" $ const pending -- TODO
        it "fails if the step function is non-increasing" $ const pending -- TODO
        it "fails if the starting accumulator is not of the expected type" $ const pending -- TODO
        it "fails if the argument is not of list type" $ const pending -- TODO
    describe "domination tests" $ do
    -- Testing by cases on which evaluation step consumes more resources than the others
    -- E.g. for application M N, three tests in which: M dominates the cost, N dominates, the result of the application dominates
      context "when typing pairs" $ do
        it "produces the correct type and wirecount when the first element dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the second element dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the result dominates" $ const pending -- TODO
      context "when typing cons" $ do
        it "produces the correct type and wirecount when the head dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the tail dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the result dominates" $ const pending -- TODO
      context "when typing application" $ do
        it "produces the correct type and wirecount when the function dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the argument dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the result dominates" $ const pending -- TODO
      context "when typing apply" $ do
        it "produces the correct type and wirecount when the circuit term dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the argument dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the result dominates" $ const pending -- TODO
      context "when typing fold" $ do
        it "produces the correct type and wirecount when the step function dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the starting accumulator dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the argument list dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the result dominates" $ const pending -- TODO
      context "when typing let-in" $ do
        it "produces the correct type and wirecount when the bound expression dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the body dominates" $ const pending -- TODO
      context "when typing the destructuring let-in" $ do
        it "produces the correct type and wirecount when the bound expression dominates" $ const pending -- TODO
        it "produces the correct type and wirecount when the body dominates" $ const pending -- TODO
    describe "Checking and subsumption" $ do
    -- Testing the subtyping relation and the annotation construct
      it "should work" $ const pending -- TODO





