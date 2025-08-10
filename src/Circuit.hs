module Circuit where
-- module Circuit (WireType(..), QuantumOperation(..), Circuit(..)) where

import PrettyPrinter

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (intercalate)

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


-- Circuit Datatype

type Label = String

type WireBundle = [Label] -- wire bundles

type LabelContext = Map Label WireType -- Q

data Circuit = -- Define circuit buffers. This corresponds to CRL expressions in the original paper
    Id LabelContext
  | CCons Circuit QuantumOperation WireBundle WireBundle
  deriving Show

makeIdCircuit :: [(Label, WireType)] -> Circuit
makeIdCircuit pairs = Id (Map.fromList pairs)

seqOp :: Circuit -> QuantumOperation -> WireBundle -> WireBundle -> Circuit
seqOp c op inLabels outLabels = CCons c op inLabels outLabels

prettyWireBundle :: WireBundle -> String
prettyWireBundle ls = case ls of
  []  -> "∗"
  [l] -> l
  _   -> "⟨" ++ intercalate "," ls ++ "⟩"

prettyLabelContext :: LabelContext -> String
prettyLabelContext ctx =
  let pairs = Map.toList ctx
      prettyPair (l, t) = l ++ ":" ++ pretty t
  in intercalate ", " (map prettyPair pairs)

instance Pretty Circuit where
  pretty = unlines . linesOf
    where
      linesOf :: Circuit -> [String]
      linesOf (Id ctx) = ["id: " ++ prettyLabelContext ctx]
      linesOf (CCons c op ins outs) =
        let prev = linesOf c
            this = pretty (op) ++ " (" ++ prettyWireBundle ins ++ ") -> " ++ prettyWireBundle outs
        in prev ++ [this]