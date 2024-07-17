module Index.Semantics.GateCount (gateCountResourceSemantics) where
import Index.Semantics.Resource
import Index.AST
import Circuit

gateCountResourceSemantics :: GlobalResourceSemantics

gateCountResourceSemantics =
  GlobalResourceSemantics
    { desugarIdentity = Number 0,
      desugarWire = const (Number 0),
      desugarOperation = Number . opGateCount,
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
opGateCount T = 1
opGateCount (R _) = 1
opGateCount CNot = 1
opGateCount CZ = 1
opGateCount (CR _) = 1
opGateCount CCNot = 1
opGateCount CCZ = 1
opGateCount Toffoli = 1
--note: metaoperations do not count as gates
opGateCount (QInit _) = 0
opGateCount QDiscard = 0
opGateCount Meas = 0
opGateCount (CInit _) = 0
opGateCount CDiscard = 0
