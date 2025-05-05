module Metric.Global.Width (widthMetric) where

import Circuit
import Metric.Global
import PQ.Index

-- | The global metric module for width.
-- Width is defined informally as the maximum number of wires that are active at the same time.
widthMetric :: GlobalMetricModule
widthMetric =
  GlobalMetricModule
  { name = "width",
    desugarIdentity = Number 0,           -- no width is 0
    desugarWire = const (Number 1),       -- each wire is wide 1
    desugarSequence = Max,                -- width of sequence comp. is max of widths
    desugarParallel = Plus,               -- width of parallel comp. is sum of widths
    desugarBoundedSequence = BoundedMax,
    desugarBoundedParallel = BoundedSum,
    desugarOperation = Number . opWidths
  }

-- | @opWidths op@ returns the width of operation @op@.
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
opWidths (Rinv _) = 1
opWidths CNot = 2
opWidths CZ = 2
opWidths (CR _) = 2
opWidths (CRinv _) = 2
opWidths CCNot = 2
opWidths CCZ = 2
opWidths Toffoli = 3