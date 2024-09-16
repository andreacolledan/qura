module Index.Semantics.Global.Resource where
import Circuit
import Index.AST
import PrettyPrinter

-- | A global metric module defines how to compute a global metric of a circuit.
data GlobalMetricModule = GlobalMetricModule
  { -- | The name of the metric
    name :: String,
    -- | The neutral element of the metric
    desugarIdentity :: Index,
    -- | The metric of a wire, given its type
    desugarWire :: WireType -> Index,
    -- | The metric of the composition in sequence of two circuits
    desugarSequence :: Index -> Index -> Index,
    -- | The metric of the composition in parallel of two circuits
    desugarParallel :: Index -> Index -> Index,
    -- | The metric of the composition in sequence of a bounded series of circuits
    desugarBoundedSequence :: IVarId -> Index -> Index -> Index,
    -- | The metric of the composition in parallel of a bounded series of circuits
    desugarBoundedParallel :: IVarId -> Index -> Index -> Index,
    -- | The metric of a quantum operation
    desugarOperation :: QuantumOperation -> Index
  }

instance Pretty GlobalMetricModule where
  pretty = name