module Index.Semantics.Global.Width (
  widthMetric
) where


import Index.Semantics.Global.Resource
import Index.AST
import Circuit

widthMetric :: GlobalMetricModule
widthMetric =
  GlobalMetricModule
  { name = "width",
    desugarIdentity = Number 0,
    desugarWire = const (Number 1),
    desugarOperation = Number . opWidths,
    desugarSequence = Max,
    desugarParallel = Plus,
    desugarBoundedSequence = BoundedMax,
    desugarBoundedParallel = BoundedSum
  }

opWidths :: QuantumOperation -> Int
opWidths (QInit _) = 1
opWidths QDiscard = 1
opWidths Meas = 1
opWidths (CInit _) = 1
opWidths CDiscard = 1
opWidths Hadamard = 1
opWidths PauliX = 1
opWidths PauliY = 1
opWidths PauliZ = 1
opWidths T = 1
opWidths (R _) = 1
opWidths CNot = 2
opWidths CZ = 2
opWidths (CR _) = 2
opWidths CCNot = 2
opWidths CCZ = 2
opWidths Toffoli = 3