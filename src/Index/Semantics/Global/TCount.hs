module Index.Semantics.Global.TCount (tCountMetric) where
import Index.Semantics.Global.Resource
import Index.AST
import Circuit

-- | The global metric module for T-count.
-- T-count is defined informally as the total number of T gates in a circuit.
tCountMetric :: GlobalMetricModule
tCountMetric =
  GlobalMetricModule
    { name = "T-count",
      desugarIdentity = Number 0,           -- no T gates is 0
      desugarWire = const (Number 0),       -- wires naturally count as 0 T gates
      desugarSequence = Plus,               -- T-count of sequence comp. is sum of T-counts
      desugarParallel = Plus,               -- T-count of parallel comp. is sum of T-counts
      desugarBoundedSequence = BoundedSum,
      desugarBoundedParallel = BoundedSum,
      desugarOperation = Number . opTCount
  }

-- | @opTCount op@ returns the number of T gates in operation @op@.
opTCount :: QuantumOperation -> Int
opTCount T = 1
opTCount _ = 0
