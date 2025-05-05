{-# LANGUAGE LambdaCase #-}

module Metric.Global.Qubits (qubitsMetric) where

import Circuit
import Metric.Global
import PQ.Index

-- | The global metric module for qubits.
-- Qubits is defined informally as the maximum number of qubits that are active at the same time.
qubitsMetric :: GlobalMetricModule
qubitsMetric =
  GlobalMetricModule
  { name = "qubits",
    desugarIdentity = Number 0,                             -- no qubits is 0
    desugarWire = \case Qubit -> Number 1; Bit -> Number 0, -- qubits count as 1, bits count as 0
    desugarSequence = Max,                                  -- qubits of sequence comp. is max of qubits
    desugarParallel = Plus,                                 -- qubits of parallel comp. is sum of qubits
    desugarBoundedSequence = BoundedMax,
    desugarBoundedParallel = BoundedSum,
    desugarOperation = Number . opQubits
  }

-- | @opQubits op@ returns the number of qubits involved operation @op@.
opQubits :: QuantumOperation -> Int
opQubits (QInit _) = 1
opQubits QDiscard = 1
opQubits Meas = 1
opQubits Hadamard = 1
opQubits PauliX = 1
opQubits PauliY = 1
opQubits PauliZ = 1
opQubits T = 1
opQubits (R _) = 1
opQubits (Rinv _) = 1
opQubits CNot = 2
opQubits CZ = 2
opQubits (CR _) = 2
opQubits (CRinv _) = 2
opQubits CCNot = 1
opQubits CCZ = 1
opQubits Toffoli = 3
opQubits (CInit _) = 0
opQubits CDiscard = 0
