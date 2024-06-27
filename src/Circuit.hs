module Circuit (WireType(..), QuantumOperation(..)) where
import PrettyPrinter

data WireType = Bit | Qubit deriving (Show, Eq)
instance Pretty WireType where
  pretty Bit = "Bit"
  pretty Qubit = "Qubit"

data QuantumOperation =
  -- Qubit metaoperations
  QInit0
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
  | CZ
  -- Three qubit gates
  | Toffoli
  deriving (Show, Eq)