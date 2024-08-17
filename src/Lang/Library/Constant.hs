module Lang.Library.Constant (
  Constant(..),
  typeOf
) where
import PrettyPrinter
import Lang.Type.AST
import Index.AST
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
  | MakeUnitList
  deriving (Eq, Show)

instance Pretty Constant where
  pretty (Boxed (QInit b)) = "QInit" ++ if b then "1" else "0"  
  pretty (Boxed QDiscard) = "QDiscard"
  pretty (Boxed (CInit b)) = "CInit" ++ if b then "1" else "0"
  pretty (Boxed CDiscard) = "CDiscard"
  pretty (Boxed Meas) = "Meas"
  pretty (Boxed Hadamard) = "Hadamard"
  pretty (Boxed PauliX) = "PauliX"
  pretty (Boxed PauliY) = "PauliY"
  pretty (Boxed PauliZ) = "PauliZ"
  pretty (Boxed T) = "T"
  pretty (Boxed (R n)) = "R" ++ show n
  pretty (Boxed CNot) = "CNot"
  pretty (Boxed CZ) = "CZ"
  pretty (Boxed (CR n)) = "CR" ++ show n
  pretty (Boxed CCNot) = "CCNot"
  pretty (Boxed CCZ) = "CCZ"
  pretty (Boxed Toffoli) = "Toffoli"
  pretty MakeCRGate = "MakeCRGate"
  pretty MakeUnitList = "MakeUnitList"

-- | @typeOf c@ returns the type of constant @c@
typeOf :: Constant -> Type
typeOf (Boxed (QInit b)) = TCirc (Just $ Operation (QInit b)) TUnit (TWire Qubit (Just $ Output (QInit b) 0 []))
typeOf (Boxed QDiscard) = TIForall "i0" (TCirc (Just $ Operation QDiscard) (TWire Qubit (Just $ IndexVariable "i0")) TUnit) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed Meas) = TIForall "i0" (TCirc (Just $ Operation Meas) (TWire Qubit (Just (IndexVariable "i0"))) (TWire Bit (Just (Output Meas 0 [IndexVariable "i0"])))) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed (CInit b)) = TCirc (Just $ Operation (CInit b)) TUnit (TWire Bit (Just $ Output (CInit b) 0 []))
typeOf (Boxed CDiscard) = TIForall "i0" (TCirc (Just $ Operation CDiscard) (TWire Bit (Just (IndexVariable "i0"))) TUnit) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed Hadamard) = TIForall "i0" (TCirc (Just $ Operation Hadamard) (TWire Qubit (Just $ IndexVariable "i0")) (TWire Qubit (Just $ Output Hadamard 0 [IndexVariable "i0"]))) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed PauliX) = TIForall "i0" (TCirc (Just $ Operation PauliX) (TWire Qubit (Just $ IndexVariable "i0")) (TWire Qubit (Just $ Output PauliX 0 [IndexVariable "i0"]))) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed PauliY) = TIForall "i0" (TCirc (Just $ Operation PauliY) (TWire Qubit (Just $ IndexVariable "i0")) (TWire Qubit (Just $ Output PauliY 0 [IndexVariable "i0"]))) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed PauliZ) = TIForall "i0" (TCirc (Just $ Operation PauliZ) (TWire Qubit (Just $ IndexVariable "i0")) (TWire Qubit (Just $ Output PauliZ 0 [IndexVariable "i0"]))) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed T) = TIForall "i0" (TCirc (Just $ Operation T) (TWire Qubit (Just $ IndexVariable "i0")) (TWire Qubit (Just $ Output T 0 [IndexVariable "i0"]))) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed (R n)) = TIForall "i0" (TCirc (Just $ Operation (R n)) (TWire Qubit (Just $ IndexVariable "i0")) (TWire Qubit (Just $ Output (R n) 0 [IndexVariable "i0"]))) (Just $ Number 0) (Just $ Number 0)

typeOf (Boxed CNot) = TIForall "i0" ( TIForall "i1" (TCirc (Just $ Operation CNot) (TTensor [TWire Qubit (Just $ IndexVariable "i0"), TWire Qubit (Just $ IndexVariable "i1")]) (TTensor [TWire Qubit (Just $ Output CNot 0 [IndexVariable "i0", IndexVariable "i1"]), TWire Qubit (Just $ Output CNot 1 [IndexVariable "i0", IndexVariable "i1"])])) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed CZ) = TIForall "i0" ( TIForall "i1" (TCirc (Just $ Operation CZ) (TTensor [TWire Qubit (Just $ IndexVariable "i0"), TWire Qubit (Just $ IndexVariable "i1")]) (TTensor [TWire Qubit (Just $ Output CZ 0 [IndexVariable "i0", IndexVariable "i1"]), TWire Qubit (Just $ Output CZ 1 [IndexVariable "i0", IndexVariable "i1"])])) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed (CR n)) = TIForall "i0" ( TIForall "i1" (TCirc (Just $ Operation (CR n)) (TTensor [TWire Qubit (Just $ IndexVariable "i0"), TWire Qubit (Just $ IndexVariable "i1")]) (TTensor [TWire Qubit (Just $ Output (CR n) 0 [IndexVariable "i0", IndexVariable "i1"]), TWire Qubit (Just $ Output (CR n) 1 [IndexVariable "i0", IndexVariable "i1"])])) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed CCNot) = TIForall "i0" ( TIForall "i1" (TCirc (Just $ Operation CCNot) (TTensor [TWire Bit (Just $ IndexVariable "i0"), TWire Qubit (Just $ IndexVariable "i1")]) (TTensor [TWire Bit (Just $ Output CCNot 0 [IndexVariable "i0", IndexVariable "i1"]), TWire Qubit (Just $ Output CCNot 1 [IndexVariable "i0", IndexVariable "i1"])])) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)
typeOf (Boxed CCZ) = TIForall "i0" ( TIForall "i1" (TCirc (Just $ Operation CCZ) (TTensor [TWire Bit (Just $ IndexVariable "i0"), TWire Qubit (Just $ IndexVariable "i1")]) (TTensor [TWire Bit (Just $ Output CCZ 0 [IndexVariable "i0", IndexVariable "i1"]), TWire Qubit (Just $ Output CCZ 1 [IndexVariable "i0", IndexVariable "i1"])])) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)

typeOf (Boxed Toffoli) = TIForall "i0" ( TIForall "i1" ( TIForall "i2" (TCirc (Just $ Operation Toffoli) (TTensor [TWire Qubit (Just $ IndexVariable "i0"), TWire Qubit (Just $ IndexVariable "i1"), TWire Qubit (Just $ IndexVariable "i2")]) (TTensor [TWire Qubit (Just $ Output Toffoli 0 [IndexVariable "i0", IndexVariable "i1", IndexVariable "i2"]), TWire Qubit (Just $ Output Toffoli 1 [IndexVariable "i0", IndexVariable "i1", IndexVariable "i2"]), TWire Qubit (Just $ Output Toffoli 2 [IndexVariable "i0", IndexVariable "i1", IndexVariable "i2"])])) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)

-- single-qubit single-controlled R gate family
typeOf MakeCRGate = TIForall "i" (TIForall "i0" (TIForall "i1" (TCirc (Just $ Operation (CR 0)) (TTensor [TWire Qubit (Just $ IndexVariable "i0"), TWire Qubit (Just $ IndexVariable "i1")]) (TTensor [TWire Qubit (Just $ Output (CR 0) 0 [IndexVariable "i0", IndexVariable "i1"]), TWire Qubit (Just $ Output (CR 0) 1 [IndexVariable "i0", IndexVariable "i1"])])) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)) (Just $ Number 0) (Just $ Number 0)
-- list of unit of length n
typeOf MakeUnitList = TIForall "n" (TList "i" (IndexVariable "n") TUnit) (Just $ Number 0) (Just $ Number 0)
