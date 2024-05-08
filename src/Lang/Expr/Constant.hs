module Lang.Expr.Constant (
  Constant(..),
  typeOf
) where
import PrettyPrinter
import Lang.Type.AST
import Index.AST

--- CONSTANTS -----------------------------------------------------------------------------------------------
---
--- This module defines the constants of the PQR language.
-------------------------------------------------------------------------------------------------------------

-- Enum of constants
data Constant
  -- Qubit metaoperations
  = QInit0
  | QInit1
  | QDiscard
  | Meas
  -- Bit metaoperations
  | CInit0
  | CInit1
  | CDiscard
  -- Single qubit gates
  | Hadamard
  | PauliX
  | PauliY
  | PauliZ
  -- Two qubit gates
  | CNot
  -- Three qubit gates
  | Toffoli
  -- Functions
  | MakeCRGate
  | MakeNToffoli
  | MakeNCZ
  | MakeUnitList
  deriving (Eq, Show)

instance Pretty Constant where
  pretty QInit0 = "QInit0"
  pretty QInit1 = "QInit1"
  pretty QDiscard = "QDiscard"
  pretty CInit0 = "CInit0"
  pretty CInit1 = "CInit1"
  pretty CDiscard = "CDiscard"
  pretty Meas = "Meas"
  pretty Hadamard = "Hadamard"
  pretty PauliX = "PauliX"
  pretty PauliY = "PauliY"
  pretty PauliZ = "PauliZ"
  pretty CNot = "CNot"
  pretty Toffoli = "Toffoli"
  pretty MakeCRGate = "MakeCRGate"
  pretty MakeNToffoli = "MakeNToffoli"
  pretty MakeNCZ = "MakeNCZ"
  pretty MakeUnitList = "MakeUnitList"

-- | @typeOf c@ returns the type of constant @c@
typeOf :: Constant -> Type
typeOf QInit0 = TCirc (Number 1) TUnit (TWire Qubit)
typeOf QInit1 = TCirc (Number 1) TUnit (TWire Qubit)
typeOf QDiscard = TCirc (Number 1) (TWire Qubit) TUnit
typeOf Meas = TCirc (Number 1) (TWire Qubit) (TWire Bit)
typeOf CInit0 = TCirc (Number 1) TUnit (TWire Bit)
typeOf CInit1 = TCirc (Number 1) TUnit (TWire Bit)
typeOf CDiscard = TCirc (Number 1) (TWire Bit) TUnit
typeOf Hadamard = TCirc (Number 1) (TWire Qubit) (TWire Qubit)
typeOf PauliX = TCirc (Number 1) (TWire Qubit) (TWire Qubit)
typeOf PauliY = TCirc (Number 1) (TWire Qubit) (TWire Qubit)
typeOf PauliZ = TCirc (Number 1) (TWire Qubit) (TWire Qubit)
typeOf CNot = TCirc (Number 2) (TTensor [TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit])
typeOf Toffoli = TCirc (Number 3) (TTensor [TWire Qubit, TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit, TWire Qubit])
typeOf MakeCRGate = TIForall "i" (TCirc (Number 2) (TTensor [TWire Qubit, TWire Qubit]) (TTensor [TWire Qubit, TWire Qubit])) (Number 0) (Number 0)
typeOf MakeNToffoli = TIForall "i" (TCirc (Plus (IndexVariable "i") (Number 1)) (TTensor [TList (IndexVariable "i") (TWire Qubit), TWire Qubit]) (TTensor [TList (IndexVariable "i") (TWire Qubit), TWire Qubit])) (Number 0) (Number 0)
typeOf MakeNCZ = TIForall "i" (TCirc (Plus (IndexVariable "i") (Number 1)) (TTensor [TList (IndexVariable "i") (TWire Qubit), TWire Qubit]) (TTensor [TList (IndexVariable "i") (TWire Qubit), TWire Qubit])) (Number 0) (Number 0)
typeOf MakeUnitList = TIForall "i" (TList (IndexVariable "i") TUnit) (Number 0) (Number 0)
