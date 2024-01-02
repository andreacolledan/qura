module Circuit.SyntaxSpec where
import Test.Hspec
import Circuit.Syntax
import qualified Data.Map as Map
import WireBundle.Syntax

widthSpec :: Spec
widthSpec = do
    describe "circuit width" $ do
        it "matches the size of the label context for identity circuits" $ do
            -- width(ID_∅) = 0
            width (Id Map.empty) `shouldBe` 0
            -- width (ID_{q:Qubit}) = 1
            width (Id $ Map.fromList [("q",Qubit)]) `shouldBe` 1
            -- width(ID_{q:Qubit,r:Qubit,s:Qubit}) = 3
            width (Id $ Map.fromList [("q",Qubit),("r",Qubit),("s",Qubit)]) `shouldBe` 3
        it "does not increase when an operation other than an init is applied" $ do
            -- width (ID_{q:Qubit}; H(q) -> q) = 1
            width (Seq (Id $ Map.fromList[("q",Qubit)]) Discard (Label "q") UnitValue) `shouldBe` 1
            -- width (ID_{q:Qubit}; H(q) -> q; X(q) -> q) = 1
            width (Seq (Seq (Id $ Map.fromList [("q",Qubit)]) Hadamard (Label "q") (Label "q")) PauliX (Label "q") (Label "q")) `shouldBe` 1
        it "should increase when an init is applied and no discarded qubits exist" $ do
            -- width (ID_∅; Init(*) -> q) = 1
            width (Seq (Id Map.empty) Init UnitValue (Label "q")) `shouldBe` 1
            -- width (ID_{q:Qubit}; H(q) -> q; Init(*) -> r) = 2
            width (Seq (Seq (Id $ Map.fromList [("q",Qubit)]) Hadamard (Label "q") (Label "q")) Init UnitValue (Label "r")) `shouldBe` 2
            -- width (ID_∅; Init(*) -> q; Init(*) -> r) = 2
            width (Seq (Seq (Id Map.empty) Init UnitValue (Label "q")) Init UnitValue (Label "r")) `shouldBe` 2