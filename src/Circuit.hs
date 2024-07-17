module Circuit (WireType(..), QuantumOperation(..)) where
import PrettyPrinter

data WireType = Bit | Qubit deriving (Show, Eq)
instance Pretty WireType where
  pretty Bit = "Bit"
  pretty Qubit = "Qubit"

data QuantumOperation =
  -- Qubit metaoperations
  QInit Bool
  | QDiscard
  | Meas
  -- Bit metaoperations
  | CInit Bool
  | CDiscard
  -- Single qubit gates
  | Hadamard
  | PauliX
  | PauliY
  | PauliZ
  | T
  | R Int
  -- Two qubit gates
  | CNot
  | CZ
  | CR Int
  -- Classically controlled gates
  | CCNot
  | CCZ
  -- Three qubit gates
  | Toffoli
  deriving (Show, Eq)