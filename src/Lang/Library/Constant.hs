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
  pretty (Boxed Toffoli) = "Toffoli"
  pretty MakeCRGate = "MakeCRGate"
  pretty MakeNToffoli = "MakeNToffoli"
  pretty MakeNCZ = "MakeNCZ"
  pretty MakeUnitList = "MakeUnitList"

-- | @typeOf c@ returns the type of constant @c@
typeOf :: GlobalResourceSemantics -> Constant -> Type
typeOf grs (Boxed QInit0) = TCirc (Just $ Number $ opGroundTruth grs QInit0) TUnit (TWire Qubit)
typeOf grs (Boxed QInit1) = TCirc (Just $ Number $ opGroundTruth grs QInit1) TUnit (TWire Qubit)
typeOf grs (Boxed QDiscard) = TCirc (Just $ Number $ opGroundTruth grs QDiscard) (TWire Qubit) TUnit
typeOf grs (Boxed Meas) = TCirc (Just $ Number $ opGroundTruth grs Meas) (TWire Qubit) (TWire Bit)
typeOf grs (Boxed CInit0) = TCirc (Just $ Number $ opGroundTruth grs CInit0) TUnit (TWire Bit)
typeOf grs (Boxed CInit1) = TCirc (Just $ Number $ opGroundTruth grs CInit1) TUnit (TWire Bit)
typeOf grs (Boxed CDiscard) = TCirc (Just $ Number $ opGroundTruth grs CDiscard) (TWire Bit) TUnit
typeOf grs (Boxed Hadamard) = TCirc (Just $ Number $ opGroundTruth grs Hadamard) (TWire Qubit) (TWire Qubit)
typeOf grs (Boxed PauliX) = TCirc (Just $ Number $ opGroundTruth grs PauliX) (TWire Qubit) (TWire Qubit)
typeOf grs (Boxed PauliY) = TCirc (Just $ Number $ opGroundTruth grs PauliY) (TWire Qubit) (TWire Qubit)
typeOf grs (Boxed PauliZ) = TCirc (Just $ Number $ opGroundTruth grs PauliZ) (TWire Qubit) (TWire Qubit)
typeOf grs (Boxed CNot) = TCirc (Just $ Number $ opGroundTruth grs CNot) (TTensor [TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit])
typeOf grs (Boxed CZ) = TCirc (Just $ Number $ opGroundTruth grs CZ) (TTensor [TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit])
typeOf grs (Boxed Toffoli) = TCirc (Just $ Number $ opGroundTruth grs Toffoli) (TTensor [TWire Qubit, TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit, TWire Qubit])
typeOf grs MakeCRGate = TIForall "i" (TCirc (Just $ Number $ opGroundTruth grs CZ) (TTensor [TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit])) (Just $ Number 0) (Just $ Number 0)
typeOf grs MakeNToffoli = TIForall "i" (TCirc (Parallel (IndexVariable "i") <$> Just (Number $ opGroundTruth grs PauliX)) (TTensor [TList "_" (IndexVariable "i") (TWire Qubit), TWire Qubit]) (TTensor [TList "_" (IndexVariable "i") (TWire Qubit), TWire Qubit])) (Just $ Number 0) (Just $ Number 0)
typeOf grs MakeNCZ = TIForall "i" (TCirc (Parallel (IndexVariable "i") <$> Just (Number $ opGroundTruth grs PauliZ)) (TTensor [TList "_" (IndexVariable "i") (TWire Qubit), TWire Qubit]) (TTensor [TList "_" (IndexVariable "i") (TWire Qubit), TWire Qubit])) (Just $ Number 0) (Just $ Number 0)
typeOf _ MakeUnitList = TIForall "i" (TList "_" (IndexVariable "i") TUnit) (Just $ Number 0) (Just $ Number 0)
