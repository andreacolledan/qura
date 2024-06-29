module Index.Semantics.GateCount where
import Index.Semantics.Resource
import Index.AST
import Circuit
import Control.Monad.State
import PrettyPrinter

gateCountResourceSemantics :: GlobalResourceSemantics

gateCountResourceSemantics =
  GlobalResourceSemantics
    { desugarIdentity = Number 0,
      desugarWire = const (Number 0),
      desugarSequence = Plus,
      desugarParallel = Plus,
      desugarBoundedSequence = BoundedSum,
      desugarBoundedParallel = BoundedSum,
      --interpretIdentity = 0,
      --interpretWire = const 0,
      --interpretSequence = (+),
      --interpretParallel = (+),
      --smtEmbedIdentity = "0",
      --smtEmbedWire = const "0",
      --smtEmbedSequence = \i j -> "(+ " ++ i ++ " " ++ j ++ ")",
      --smtEmbedParallel = \i j -> "(+ " ++ i ++ " " ++ j ++ ")",
      --smtEmbedBoundedSequence = desugarSum, -- overapproximates
      --smtEmbedBoundedParallel = desugarSum, -- overapproximates
      opGroundTruth = opGateCount
  }

desugarSum :: IndexVariableId -> Index -> Index -> (Index -> State Int (String, String)) -> State Int (String, String)
desugarSum id i j embed = do
  count <- get
  put $ count + 1
  let maxName = "_max" ++ show count
  let argMaxName = "_argmax" ++ show count
  (di, i') <- embed i
  s <- get
  (dj, j') <- embed (isub (IndexVariable argMaxName) id j)
  put s
  (_, j'') <- embed (isub (IndexVariable "_w") id j)
  -- the following declarations must occur before the constraints of the sub-terms
  let d0 =
        "; the following variables stand for the max value and argmax of " ++ pretty (BoundedMax id i j) ++ "\n"
          ++ "(declare-const " ++ maxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ maxName ++ "))\n"
          ++ "(declare-const " ++ argMaxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ argMaxName ++ "))\n"
  -- the following declrations must occur after the constraints of the sub-terms
  let d =
        "; the following block ensures that " ++ maxName ++ " = " ++ pretty (BoundedMax id i j) ++ "\n"
          ++ "(assert (=> (<= " ++ i' ++ " 0) (= " ++ maxName ++ " 0)))\n"
          ++ "(assert (=> (> " ++ i' ++ " 0) (= " ++ maxName ++ " " ++ j' ++ ")))\n"
          ++ "(assert (< " ++ argMaxName ++ " " ++ i' ++ "))\n"
          ++ "(assert (forall ((_w Int)) (=> "
            ++ "(and (<= 0 _w) (< _w " ++ i' ++ "))"
            ++ "(<= " ++ j'' ++ " " ++ j' ++ "))))\n"
  return (d0 ++ di ++ dj ++ d, "(* " ++ i' ++ " " ++ maxName ++ ")")

opGateCount :: QuantumOperation -> Int
opGateCount Hadamard = 1
opGateCount PauliX = 1
opGateCount PauliY = 1
opGateCount PauliZ = 1
opGateCount CNot = 1
opGateCount CZ = 1
opGateCount Toffoli = 1
opGateCount _ = 0
