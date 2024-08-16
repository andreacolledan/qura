module Index.Semantics.Resource where
import Circuit
import Index.AST

data GlobalResourceSemantics = GlobalResourceSemantics
  { desugarIdentity :: Index,
    desugarWire :: WireType -> Index,
    desugarOperation :: QuantumOperation -> Index,
    desugarSequence :: Index -> Index -> Index,
    desugarParallel :: Index -> Index -> Index,
    desugarBoundedSequence :: IndexVariableId -> Index -> Index -> Index,
    desugarBoundedParallel :: IndexVariableId -> Index -> Index -> Index,
    opGroundTruth :: QuantumOperation -> Int
  }

newtype LocalResourceSemantics = LocalResourceSemantics
  {
    desugarOutput :: QuantumOperation -> Int -> [Index] -> Index
  }