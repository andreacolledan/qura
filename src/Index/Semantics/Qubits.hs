{-# LANGUAGE LambdaCase #-}
module Index.Semantics.Qubits (qubitsResourceSemantics) where

import Index.Semantics.Resource
import Index.AST
import Circuit

qubitsResourceSemantics :: GlobalResourceSemantics
qubitsResourceSemantics =
  GlobalResourceSemantics
  {
    desugarIdentity = Number 0,
    desugarWire = \case Qubit -> Number 1; Bit -> Number 0,
    desugarOperation = Number . opQubits,
    desugarSequence = Max,
    desugarParallel = Plus,
    desugarBoundedSequence = BoundedMax,
    desugarBoundedParallel = BoundedSum,
    opGroundTruth = opQubits
  }

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
opQubits CNot = 2
opQubits CZ = 2
opQubits (CR _) = 2
opQubits CCNot = 1
opQubits CCZ = 1
opQubits Toffoli = 3
opQubits (CInit _) = 0
opQubits CDiscard = 0
