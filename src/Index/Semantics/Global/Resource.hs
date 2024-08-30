module Index.Semantics.Global.Resource where
import Circuit
import Index.AST
import PrettyPrinter

data GlobalMetricModule = GlobalMetricModule
  { name :: String,
    desugarIdentity :: Index,
    desugarWire :: WireType -> Index,
    desugarOperation :: QuantumOperation -> Index,
    desugarSequence :: Index -> Index -> Index,
    desugarParallel :: Index -> Index -> Index,
    desugarBoundedSequence :: IVarId -> Index -> Index -> Index,
    desugarBoundedParallel :: IVarId -> Index -> Index -> Index
  }

instance Pretty GlobalMetricModule where
  pretty = name