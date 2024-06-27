module Index.Semantics.Width (widthResourceSemantics) where


import Index.Semantics.Resource
import Index.AST
import Control.Monad.State
import PrettyPrinter
import Circuit

widthResourceSemantics :: GlobalResourceSemantics
widthResourceSemantics =
  GlobalResourceSemantics
  {
    desugarIdentity = Number 0,
    desugarWire = const (Number 1),
    desugarSequence = Max,
    desugarParallel = Plus,
    desugarBoundedSequence = Maximum,
    desugarBoundedParallel = \id i j -> Mult i (Maximum id i j),
    desugarIndependentBoundedSequence = \_ j -> j,
    desugarIndependentBoundedParallel = Mult,
    interpretIdentity = 0,
    interpretWire = const 1,
    interpretSequence = max,
    interpretParallel = (+),
    smtEmbedIdentity = "0",
    smtEmbedWire = const "1",
    smtEmbedSequence = \i j -> "(max " ++ i ++ " " ++ j ++ ")",
    smtEmbedParallel = \i j -> "(+ " ++ i ++ " " ++ j ++ ")",
    smtEmbedBoundedSequence = desugarMaximum,
    smtEmbedBoundedParallel = desugarSum,
    opGroundTruth = opWidths
  }

desugarMaximum :: IndexVariableId -> Index -> Index -> (Index -> State Int (String, String)) -> State Int (String, String)
desugarMaximum id i j embed = do
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
        "; the following variables stand for the max value and argmax of " ++ pretty (Maximum id i j) ++ "\n"
          ++ "(declare-const " ++ maxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ maxName ++ "))\n"
          ++ "(declare-const " ++ argMaxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ argMaxName ++ "))\n"
  -- the following declrations must occur after the constraints of the sub-terms
  let d =
        "; the following block ensures that " ++ maxName ++ " = " ++ pretty (Maximum id i j) ++ "\n"
          ++ "(assert (=> (<= " ++ i' ++ " 0) (= " ++ maxName ++ " 0)))\n"
          ++ "(assert (=> (> " ++ i' ++ " 0) (= " ++ maxName ++ " " ++ j' ++ ")))\n"
          ++ "(assert (< " ++ argMaxName ++ " " ++ i' ++ "))\n"
          ++ "(assert (forall ((_w Int)) (=> "
            ++ "(and (<= 0 _w) (< _w " ++ i' ++ "))"
            ++ "(<= " ++ j'' ++ " " ++ j' ++ "))))\n"
  return (d0 ++ di ++ dj ++ d, maxName)

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
        "; the following variables stand for the max value and argmax of " ++ pretty (Maximum id i j) ++ "\n"
          ++ "(declare-const " ++ maxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ maxName ++ "))\n"
          ++ "(declare-const " ++ argMaxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ argMaxName ++ "))\n"
  -- the following declrations must occur after the constraints of the sub-terms
  let d =
        "; the following block ensures that " ++ maxName ++ " = " ++ pretty (Maximum id i j) ++ "\n"
          ++ "(assert (=> (<= " ++ i' ++ " 0) (= " ++ maxName ++ " 0)))\n"
          ++ "(assert (=> (> " ++ i' ++ " 0) (= " ++ maxName ++ " " ++ j' ++ ")))\n"
          ++ "(assert (< " ++ argMaxName ++ " " ++ i' ++ "))\n"
          ++ "(assert (forall ((_w Int)) (=> "
            ++ "(and (<= 0 _w) (< _w " ++ i' ++ "))"
            ++ "(<= " ++ j'' ++ " " ++ j' ++ "))))\n"
  return (d0 ++ di ++ dj ++ d, "(* " ++ i' ++ " " ++ maxName ++ ")")

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