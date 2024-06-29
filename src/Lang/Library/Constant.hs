module Lang.Library.Constant (
  Constant(..),
  typeOf
) where
import PrettyPrinter
import Lang.Type.AST
import Index.AST
import Index.Semantics.Resource (GlobalResourceSemantics (opGroundTruth))
import Circuit

--- CONSTANTS -----------------------------------------------------------------------------------------------
---
--- This module defines the constants of the PQR language.
-------------------------------------------------------------------------------------------------------------

-- Enum of constants
data Constant
  -- Qubit metaoperations
  = Boxed QuantumOperation
  -- Functions
  | MakeCRGate
  | MakeNToffoli
  | MakeNCZ
  | MakeUnitList
  deriving (Eq, Show)

instance Pretty Constant where
  pretty (Boxed QInit0) = "QInit0"
  pretty (Boxed QInit1) = "QInit1"
  pretty (Boxed QDiscard) = "QDiscard"
  pretty (Boxed CInit0) = "CInit0"
  pretty (Boxed CInit1) = "CInit1"
  pretty (Boxed CDiscard) = "CDiscard"
  pretty (Boxed Meas) = "Meas"
  pretty (Boxed Hadamard) = "Hadamard"
  pretty (Boxed PauliX) = "PauliX"
  pretty (Boxed PauliY) = "PauliY"
  pretty (Boxed PauliZ) = "PauliZ"
  pretty (Boxed CNot) = "CNot"
  pretty (Boxed CZ) = "CZ"
  pretty (Boxed CCNot) = "CCNot"
  pretty (Boxed CCZ) = "CCZ"
  pretty (Boxed Toffoli) = "Toffoli"
  pretty MakeCRGate = "MakeCRGate"
  pretty MakeNToffoli = "MakeNToffoli"
  pretty MakeNCZ = "MakeNCZ"
  pretty MakeUnitList = "MakeUnitList"

-- | @typeOf c@ returns the type of constant @c@
typeOf :: Maybe GlobalResourceSemantics -> Constant -> Type
typeOf grs (Boxed QInit0) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just QInit0)) TUnit (TWire Qubit)
typeOf grs (Boxed QInit1) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just QInit1)) TUnit (TWire Qubit)
typeOf grs (Boxed QDiscard) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just QDiscard)) (TWire Qubit) TUnit
typeOf grs (Boxed Meas) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just Meas)) (TWire Qubit) (TWire Bit)
typeOf grs (Boxed CInit0) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just CInit0)) TUnit (TWire Bit)
typeOf grs (Boxed CInit1) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just CInit1)) TUnit (TWire Bit)
typeOf grs (Boxed CDiscard) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just CDiscard)) (TWire Bit) TUnit
typeOf grs (Boxed Hadamard) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just Hadamard)) (TWire Qubit) (TWire Qubit)
typeOf grs (Boxed PauliX) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just PauliX)) (TWire Qubit) (TWire Qubit)
typeOf grs (Boxed PauliY) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just PauliY)) (TWire Qubit) (TWire Qubit)
typeOf grs (Boxed PauliZ) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just PauliZ)) (TWire Qubit) (TWire Qubit)
typeOf grs (Boxed CNot) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just CNot)) (TTensor [TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit])
typeOf grs (Boxed CZ) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just CZ)) (TTensor [TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit])
typeOf grs (Boxed CCNot) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just CCNot)) (TTensor [TWire Bit, TWire Qubit]) (TTensor [TWire Bit, TWire Qubit])
typeOf grs (Boxed CCZ) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just CCZ)) (TTensor [TWire Bit, TWire Qubit]) (TTensor [TWire Bit, TWire Qubit])
typeOf grs (Boxed Toffoli) = TCirc (Number <$> (opGroundTruth <$> grs <*> Just Toffoli)) (TTensor [TWire Qubit, TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit, TWire Qubit])
typeOf grs MakeCRGate = TIForall "i" (TCirc (Number <$> (opGroundTruth <$> grs <*> Just CZ)) (TTensor [TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit])) (Just $ Number 0) (Just $ Number 0)
typeOf grs MakeNToffoli = TIForall "i" (TCirc (Parallel (IndexVariable "i") . Number <$> (opGroundTruth <$> grs <*> Just PauliX)) (TTensor [TList "_" (IndexVariable "i") (TWire Qubit), TWire Qubit]) (TTensor [TList "_" (IndexVariable "i") (TWire Qubit), TWire Qubit])) (Just $ Number 0) (Just $ Number 0)
typeOf grs MakeNCZ = TIForall "i" (TCirc (Parallel (IndexVariable "i") . Number <$> (opGroundTruth <$> grs <*> Just PauliZ)) (TTensor [TList "_" (IndexVariable "i") (TWire Qubit), TWire Qubit]) (TTensor [TList "_" (IndexVariable "i") (TWire Qubit), TWire Qubit])) (Just $ Number 0) (Just $ Number 0)
typeOf _ MakeUnitList = TIForall "i" (TList "_" (IndexVariable "i") TUnit) (Just $ Number 0) (Just $ Number 0)
