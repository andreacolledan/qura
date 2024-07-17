module Index.Semantics.TCount (tCountResourceSemantics) where
import Index.Semantics.Resource
import Index.AST
import Circuit

tCountResourceSemantics :: GlobalResourceSemantics
tCountResourceSemantics =
  GlobalResourceSemantics
    { desugarIdentity = Number 0,
      desugarWire = const (Number 0),
      desugarOperation = Number . opTCount,
      desugarSequence = Plus,
      desugarParallel = Plus,
      desugarBoundedSequence = BoundedSum,
      desugarBoundedParallel = BoundedSum,
      opGroundTruth = opTCount
  }

opTCount :: QuantumOperation -> Int
opTCount T = 1
opTCount _ = 0
