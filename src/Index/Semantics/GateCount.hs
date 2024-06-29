module Index.Semantics.GateCount where
import Index.Semantics.Resource
import Index.AST
import Circuit

gateCountResourceSemantics :: GlobalResourceSemantics

gateCountResourceSemantics =
  GlobalResourceSemantics
    { desugarIdentity = Number 0,
      desugarWire = const (Number 0),
      desugarSequence = Plus,
      desugarParallel = Plus,
      desugarBoundedSequence = BoundedSum,
      desugarBoundedParallel = BoundedSum,
      opGroundTruth = opGateCount
  }

opGateCount :: QuantumOperation -> Int
opGateCount Hadamard = 1
opGateCount PauliX = 1
opGateCount PauliY = 1
opGateCount PauliZ = 1
opGateCount CNot = 1
opGateCount CZ = 1
opGateCount CCNot = 1
opGateCount CCZ = 1
opGateCount Toffoli = 1
opGateCount _ = 0
