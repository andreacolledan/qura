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
  | Rinv Int
  -- Two qubit gates
  | CNot
  | CZ
  | CR Int
  | CRinv Int
  -- Classically controlled gates
  | CCNot
  | CCZ
  -- Three qubit gates
  | Toffoli
  deriving (Show, Eq)


instance Pretty QuantumOperation where
  pretty (QInit b) = "QInit" ++ if b then "1" else "0"  
  pretty QDiscard = "QDiscard"
  pretty (CInit b) = "CInit" ++ if b then "1" else "0"
  pretty CDiscard = "CDiscard"
  pretty Meas = "Meas"
  pretty Hadamard = "Hadamard"
  pretty PauliX = "PauliX"
  pretty PauliY = "PauliY"
  pretty PauliZ = "PauliZ"
  pretty T = "T"
  pretty (R n) = "R" ++ show n
  pretty (Rinv n) = "R^-1" ++ show n
  pretty CNot = "CNot"
  pretty CZ = "CZ"
  pretty (CR n) = "CR" ++ show n
  pretty (CRinv n) = "CR^-1" ++ show n
  pretty CCNot = "CCNot"
  pretty CCZ = "CCZ"
  pretty Toffoli = "Toffoli"