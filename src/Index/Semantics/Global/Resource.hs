module Index.Semantics.Global.Resource where
import Circuit
import Index.AST
import PrettyPrinter

data GlobalResourceSemantics = GlobalResourceSemantics
  { name :: String,
    desugarIdentity :: Index,
    desugarWire :: WireType -> Index,
    desugarOperation :: QuantumOperation -> Index,
    desugarSequence :: Index -> Index -> Index,
    desugarParallel :: Index -> Index -> Index,
    desugarBoundedSequence :: IVarId -> Index -> Index -> Index,
    desugarBoundedParallel :: IVarId -> Index -> Index -> Index,
    opGroundTruth :: QuantumOperation -> Int
  }

instance Pretty GlobalResourceSemantics where
  pretty = name