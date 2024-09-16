{-# LANGUAGE LambdaCase #-}
module Index.Semantics.Global.Bits (bitsMetric) where

import Index.Semantics.Global.Resource
import Index.AST
import Circuit

-- | The global metric module for bits.
-- Bits is defined informally as the maximum number of bits that are active at the same time.
bitsMetric :: GlobalMetricModule
bitsMetric =
  GlobalMetricModule
  { name = "bits",
    desugarIdentity = Number 0,                             -- no bits is 0
    desugarWire = \case Qubit -> Number 0; Bit -> Number 1, -- qubits count as 0, bits count as 1
    desugarSequence = Max,                                  -- bits of sequence comp. is max of bits
    desugarParallel = Plus,                                 -- bits of parallel comp. is sum of bits
    desugarBoundedSequence = BoundedMax,
    desugarBoundedParallel = BoundedSum,
    desugarOperation = Number . opBits
  }

-- | @opBits op@ returns the number of bits involved operation @op@.
opBits :: QuantumOperation -> Int
opBits (CInit _) = 1
opBits CDiscard = 1
opBits Meas = 1
opBits CCNot = 1
opBits CCZ = 1
opBits (QInit _) = 0
opBits QDiscard = 0
opBits Hadamard = 0
opBits PauliX = 0
opBits PauliY = 0
opBits PauliZ = 0
opBits T = 0
opBits (R _) = 0
opBits CNot = 0
opBits CZ = 0
opBits (CR _) = 0
opBits Toffoli = 0