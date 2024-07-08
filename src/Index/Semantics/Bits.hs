{-# LANGUAGE LambdaCase #-}
module Index.Semantics.Bits (bitsResourceSemantics) where

import Index.Semantics.Resource
import Index.AST
import Circuit

bitsResourceSemantics :: GlobalResourceSemantics
bitsResourceSemantics =
  GlobalResourceSemantics
  {
    desugarIdentity = Number 0,
    desugarWire = \case Qubit -> Number 0; Bit -> Number 1,
    desugarSequence = Max,
    desugarParallel = Plus,
    desugarBoundedSequence = BoundedMax,
    desugarBoundedParallel = BoundedSum,
    opGroundTruth = opBits
  }

opBits :: QuantumOperation -> Int
opBits CInit0 = 1
opBits CInit1 = 1
opBits CDiscard = 1
opBits Meas = 1
opBits CCNot = 1
opBits CCZ = 1
opBits QInit0 = 0
opBits QInit1 = 0
opBits QDiscard = 0
opBits Hadamard = 0
opBits PauliX = 0
opBits PauliY = 0
opBits PauliZ = 0
opBits T = 0
opBits CNot = 0
opBits CZ = 0
opBits Toffoli = 0