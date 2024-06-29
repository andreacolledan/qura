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
opBits _ = 0