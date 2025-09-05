module Metric.Global.QasmGateCount (qasmGateCountMetric) where

import Circuit
import Metric.Global
import PQ.Index

-- | The global metric module for qasm gate count.
-- Qasm Gate count is defined informally as the total number of gates in a circuit.
qasmGateCountMetric :: GlobalMetricModule
qasmGateCountMetric =
  GlobalMetricModule
    { name = "gate count",
      desugarIdentity = Number 0,             -- no gates is 0
      desugarWire = const (Number 0),         -- wires naturally count as 0 gates
      desugarSequence = Plus,                 -- gate count of sequence comp. is sum of gate counts   
      desugarParallel = Plus,                 -- gate count of parallel comp. is sum of gate counts
      desugarBoundedSequence = BoundedSum,
      desugarBoundedParallel = BoundedSum,
      desugarOperation = Number . opGateCount
  }

-- | @opGateCount op@ returns the number of gates in operation @op@.
opGateCount :: QuantumOperation -> Int
opGateCount Meas = 1
opGateCount Hadamard = 1
opGateCount PauliX = 1
opGateCount PauliY = 1
opGateCount PauliZ = 1
opGateCount T = 1
opGateCount (R _) = 1
opGateCount (Rinv _) = 1
opGateCount CNot = 1
opGateCount CZ = 1
opGateCount (CR _) = 1
opGateCount (CRinv _) = 1
opGateCount CCNot = 1
opGateCount CCZ = 1
opGateCount Toffoli = 1
--note: metaoperations do not count as gates
opGateCount QDiscard = 0
opGateCount (CInit _) = 0
opGateCount CDiscard = 0
--to initialize a qubit to 1 we need an x gate
opGateCount (QInit False) = 0
opGateCount (QInit True) = 1