module Index.Semantics.Width (widthResourceSemantics) where


import Index.Semantics.Resource
import Index.AST
import Control.Monad.State
import Solving.CVC5
import Circuit

widthResourceSemantics :: GlobalResourceSemantics
widthResourceSemantics =
  GlobalResourceSemantics
  {
    desugarIdentity = Number 0,
    desugarWire = const (Number 1),
    desugarSequence = Max,
    desugarParallel = Plus,
    desugarBoundedSequence = BoundedMax,
    desugarBoundedParallel = BoundedSum,
    --interpretIdentity = 0,
    --interpretWire = const 1,
    --interpretSequence = max,
    --interpretParallel = (+),
    --smtEmbedIdentity = "0",
    --smtEmbedWire = const "1",
    --smtEmbedSequence = \i j -> "(max " ++ i ++ " " ++ j ++ ")",
    --smtEmbedParallel = \i j -> "(+ " ++ i ++ " " ++ j ++ ")",
    --smtEmbedBoundedSequence = smtBoundedMax,
    --smtEmbedBoundedParallel = smtBoundedSum, -- overapproximates
    opGroundTruth = opWidths
  }

smtBoundedMax :: IndexVariableId -> Index -> Index -> (Index -> State Int (String, String)) -> State Int (String, String)
smtBoundedMax id i j embed = do
  (pre, _, maxName) <- smtBoundedMaxGeneric id i j embed
  return (pre, maxName)

smtBoundedSum :: IndexVariableId -> Index -> Index -> (Index -> State Int (String, String)) -> State Int (String, String)
smtBoundedSum id i j embed = do
  (pre, upper, maxName) <- smtBoundedMaxGeneric id i j embed
  return (pre, "(* " ++ upper ++ " " ++ maxName ++ ")")

opWidths :: QuantumOperation -> Int
opWidths QInit0 = 1
opWidths QInit1 = 1
opWidths QDiscard = 1
opWidths Meas = 1
opWidths CInit0 = 1
opWidths CInit1 = 1
opWidths CDiscard = 1
opWidths Hadamard = 1
opWidths PauliX = 1
opWidths PauliY = 1
opWidths PauliZ = 1
opWidths CNot = 2
opWidths CZ = 2
opWidths Toffoli = 3