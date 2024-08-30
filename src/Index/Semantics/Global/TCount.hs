module Index.Semantics.Global.TCount (tCountMetric) where
import Index.Semantics.Global.Resource
import Index.AST
import Circuit

tCountMetric :: GlobalMetricModule
tCountMetric =
  GlobalMetricModule
    { name = "T-count",
      desugarIdentity = Number 0,
      desugarWire = const (Number 0),
      desugarOperation = Number . opTCount,
      desugarSequence = Plus,
      desugarParallel = Plus,
      desugarBoundedSequence = BoundedSum,
      desugarBoundedParallel = BoundedSum
  }

opTCount :: QuantumOperation -> Int
opTCount T = 1
opTCount _ = 0
