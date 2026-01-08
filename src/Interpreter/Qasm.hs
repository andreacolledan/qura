module Interpreter.Qasm where

import Interpreter.Metric (ProgramMetrics (..))
import Interpreter.Simple (simplifyCircuit)
import Circuit
  ( Circuit (..),
    CircuitInstruction,
    LabelCounts,
    circTolist,
    getContext,
    initCounter,
    listToCirc,
    maxCount
  )
import Circuit.Type (QuantumOperation (..), WireType (..))
import Circuit.Bundle (Label, LabelContext, WireBundle (..), namesInBundle)
import PrettyPrinter (Pretty (..))
import Interface (CLArguments (..))

import qualified Data.Map.Strict as Map (findWithDefault, insert, toList)
import qualified Data.Set as Set (Set, empty, insert, member, null, toList)

type QasmInstruction = String -- maybe create a class program of saveable strings

data QasmProgram = QasmProg {
  filename :: String,
  metrics :: ProgramMetrics,
  instructions :: [QasmInstruction]
} deriving Show

instance Pretty QasmProgram where
  pretty QasmProg {filename = fp, metrics = m, instructions = i} =
    "/*\n========================================================================\n" ++ 
    "Program generated from the ProtoQuipper file \"" ++ fp ++ "\"\nQasm-specific " ++
    pretty m ++ 
    "========================================================================\n*/\n\n" ++ 
    unlines i

-- converts a circuit to a qasm program.
-- README at the moment, the circuit is converted to a seq multiple times, maybe always work with the sequences instead
circuitToQasm :: Circuit -> CLArguments -> QasmProgram
circuitToQasm circ CommandLineArguments {filepath=fp, qubitRecycling = r} =
  let 
    simplified = simplifyCircuit r circ
    simplified' = adjustForQasm simplified
    qasmProg = -- FIXME this get printed between the metrics comment and the qasm program...
      -- trace("> Qasm Simplified Circuit:\n"++pretty simplified'++"\n\n> Actual Program:")$
        getQasm simplified' 
    qasmMetrics = computeQasmMetrics simplified'
  in QasmProg fp qasmMetrics qasmProg

--- ADJUSTING FOR QASM ---

-- | converts classical operations to quantum ops, modifies bits used for controls to qubits, 
-- initializes bits resulting from a measure 
adjustForQasm :: Circuit -> Circuit
adjustForQasm circ = 
  let
    circSeq' = go (circTolist circ) Set.empty []
    circ' = listToCirc circSeq' -- this creates the new ctx
  in circ'
  where 
    go :: [CircuitInstruction] -> Set.Set Label -> [CircuitInstruction] -> [CircuitInstruction]
    go [] _ ops = ops
    go (step:steps) existing ops = 
      case step of
        (QInit b, (ins, WLab name)) ->
          if name `Set.member` existing
            then go steps existing ops
            else go steps (Set.insert name existing) (ops ++ [step])
        (QDiscard, (ins, outs)) ->
          go steps existing $ ops ++ [step]
        (Meas, (WLab q, WLab b)) -> 
          if b `Set.member` existing
            then go steps existing $ ops ++ [step]
            else go ([(CInit False, (WUnit, WLab b)), step] ++ steps) existing $ ops -- add a CInit and adjust from there
        (CInit b, (ins, WLab name)) ->
          let 
            op = --trace("checkig if "++show name++" classically controls")$ 
              if bitClassicallyControls name steps
                then QInit b
                else CInit b
          in go steps (Set.insert name existing) $ ops ++ [(op, (ins, WLab name))]
        -- classically-controlled operations
        (CCNot, (ins, outs)) ->
          go steps existing $ ops ++ [(CNot, (ins, outs))]
        (CCZ, (ins, outs)) ->
          go steps existing $ ops ++ [(CZ, (ins, outs))]
        (op, (ins, outs)) ->
          go steps existing $ ops ++ [step]

bitClassicallyControls :: Label -> [CircuitInstruction] -> Bool
bitClassicallyControls _ [] = False
bitClassicallyControls name (step:steps) = 
  case step of
    (CCNot, (WTuple [WLab ctrl, WLab trgt], _)) ->
      if ctrl == name then True else bitClassicallyControls name steps
    (CCZ, (WTuple [WLab ctrl, WLab trgt], _)) ->
      if ctrl == name then True else bitClassicallyControls name steps
    _ -> bitClassicallyControls name steps

--- INSTRUCTION GENERATION ---

getHeader :: String -> [String]
getHeader v = case v of
  "qasm3.0" -> ["OPENQASM 3.0;","include \"stdgates.inc\";"]
  -- _ -> error "[getHeader] Unsupported version: " ++ show v

-- Generates the instructions representing the program from a circuit
-- README maybe add the version as a command line arg (and maybe add errors along the way)
getQasm :: Circuit -> [QasmInstruction]
getQasm circ = 
  let
    header = getHeader "qasm3.0" -- version, imports
    circSeq = circTolist circ
    instructions = opsToQasm circSeq
  in header ++ instructions

--- PROGRAM CONVERSION -- 

thetaStr :: Int -> String
thetaStr n = "pi/" ++ show (2^(n-1))

thetaInvStr :: Int -> String
thetaInvStr n = "-" ++ thetaStr n

-- convert a list of quantum operations and labels to a list of the corresponding qasm instructions
opsToQasm :: [CircuitInstruction] -> [QasmInstruction]
opsToQasm = concatMap opToQasm

-- Convert a quantum operation to a list of the corresponding qasm instructions
opToQasm :: CircuitInstruction -> [QasmInstruction]
-- Qubit metaoperations
opToQasm (QInit b, (_, WLab name)) = 
  ["qubit " ++ name ++ ";"] ++ if b then ["x " ++ name ++ ";"] else []
opToQasm (QDiscard, (WLab name, _))  = 
  ["reset " ++ name ++ ";"]
opToQasm (Meas, (WLab q, WLab b)) = 
  [b ++ " = measure " ++ q ++ ";"]
-- Bit metaoperations
opToQasm (CInit b, (_, WLab name))  =
  ["bit[1] " ++ name ++ " = \"" ++ (if b then "1" else "0") ++ "\";"]
opToQasm (CDiscard, (WLab name, _)) = [] -- no instruction exists to discard a bit, nor the need to do it
-- Single qubit gates
opToQasm (Hadamard, (WLab name, _)) = 
  ["h " ++ name ++ ";"]
opToQasm (PauliX, (WLab name, _)) = 
  ["x " ++ name ++ ";"]
opToQasm (PauliY, (WLab name, _)) = 
  ["y " ++ name ++ ";"]
opToQasm (PauliZ, (WLab name, _)) = 
  ["z " ++ name ++ ";"]
opToQasm (T, (WLab name, _)) = 
  ["t " ++ name ++ ";"]
opToQasm (R n, (WLab name, _)) = 
  ["rz(" ++ thetaStr n ++ ") " ++ name ++ ";"]
opToQasm (Rinv n, (WLab name, _)) = 
  ["rz(" ++ thetaInvStr n ++ ") " ++ name ++ ";"]
-- Two qubit gates
opToQasm (CNot, (WTuple [WLab ctrl, WLab trgt], _)) = 
  ["cx " ++ ctrl ++ "," ++ trgt ++ ";"]
opToQasm (CZ, (WTuple [WLab ctrl, WLab trgt], _)) = 
  ["cz " ++ ctrl ++ "," ++ trgt ++ ";"]
opToQasm (CR n, (WTuple [WLab ctrl, WLab trgt], _)) = 
  ["crz(" ++ thetaStr n ++ ") " ++ ctrl ++ "," ++ trgt ++ ";"]
opToQasm (CRinv n, (WTuple [WLab ctrl, WLab trgt], _)) = 
  ["crz(" ++ thetaInvStr n ++ ") " ++ ctrl ++ "," ++ trgt ++ ";"]
-- Classically controlled gates
-- TODO maybe remove these cases, as the quantum operation are converted before in `adjustForQasm `
opToQasm (CCNot, (WTuple [WLab ctrl, WLab trgt], _)) = 
  error "[opToQasm] CCNot should have been converted to CNot"
opToQasm (CCZ, (WTuple [WLab ctrl, WLab trgt], _)) = 
  error "[opToQasm] CCZ should have been converted to CZ"
-- Three qubit gates
opToQasm (Toffoli, (WTuple [WLab ctrl1, WLab ctrl2, WLab trgt], _)) = 
  ["ccx " ++ ctrl1 ++ "," ++ ctrl2 ++ "," ++ trgt ++ ";"]
-- undefined
opToQasm (unk, (ins, _)) = ["// placeholder for: "++show unk++" (params: "++pretty ins++")"]


--- METRICS CALCULATION ---

-- README
-- > width: qubits in the label context. This should be true because it is updated with 
--          the used wires of the simplified circuit.
--
-- > depth: we take the maximum number of operaitons applied to a single wire. 
--          Recall that we switch classically controlled gates for quantum controlled gates 
--          using a measured qubit. Depth is bigger because we are not using the bit wire...
--
-- > gatecount: straightforward, we simply count the gates.

computeQasmMetrics :: Circuit -> ProgramMetrics
computeQasmMetrics circ = 
  let 
    -- w = length $ wireNames (getContext circ) Qubit -- FIXME
    w = length (getContext circ) -- number of variables
    d = getQasmDepth circ
    gc = getQasmGateCount circ
  in ProgMetrics w d gc

wireNames :: LabelContext -> WireType -> [Label]
wireNames ctx typ = [label | (label, wireType) <- Map.toList ctx, wireType == typ]

-- DEPTH
-- When computing the depth, we don't simply add one to the counts of each label of the gate, but we have
-- to take the maximum depth of the labels in the gates, add one and then update all the labels with this new depth.
increaseCounter :: LabelCounts -> Set.Set Label -> LabelCounts
increaseCounter lc labels = increaseCounterAmount 1 lc labels
increaseCounterAmount :: Int -> LabelCounts -> Set.Set Label -> LabelCounts
increaseCounterAmount amount lc labels
  | Set.null labels = lc
  | otherwise = foldr (\label acc -> Map.insert label newVal acc) lc labels
  where
    currentMax = maximum $ 0 : [ Map.findWithDefault 0 label lc | label <- Set.toList labels ]
    newVal = currentMax + amount

getQasmDepth :: Circuit -> Int
getQasmDepth circ = 
  let
    lc = go circ $ initCounter $ getContext circ
    d = maxCount lc
  in d
  where
    go :: Circuit -> LabelCounts -> LabelCounts
    go (Id _) lc = lc
    go (CCons circ op ins outs) lc = case op of
      -- QInit:
      --    in Qasm, a qubit is init to 0 with depth 0. To have it set to 1 we use an X gate,
      --    hence depth is 1. Gatecount behaves the same
      QInit b -> 
        if b 
          then
            let
              lc' = increaseCounter lc $ namesInBundle outs
            in go circ lc'
          else go circ lc
      QDiscard ->
        let
          lc' = increaseCounter lc $ namesInBundle ins
        in go circ lc'
      Meas ->
        let
          lc' = increaseCounter lc $ namesInBundle ins
        in go circ lc'
      CInit _ -> go circ lc
      CDiscard -> go circ lc
      MCNot m ->
        let
          d = (2*(m-1)+1) -- 2(m − 1) TOFFOLI gates and one CNOT gate in sequence
          lc' = increaseCounterAmount d lc $ namesInBundle ins
        in go circ lc'
      _ -> 
        let
          lc' = increaseCounter lc $ namesInBundle ins
        in go circ lc'

-- GATECOUNT

type GateCount = Int
emptyGateCount :: GateCount
emptyGateCount = 0
increaseGateCount :: GateCount -> Int -> GateCount
increaseGateCount gc n = gc + n
increaseGateCount1 :: GateCount -> GateCount
increaseGateCount1 gc = increaseGateCount gc 1

getQasmGateCount :: Circuit -> Int
getQasmGateCount circ = 
  let
    gc = go circ emptyGateCount
  in gc
  where
    go :: Circuit -> GateCount -> GateCount
    go (Id _) gc = gc
    go (CCons circ op ins outs) gc = case op of
      -- QInit:
      --    in Qasm, a qubit is init to 0 with depth 0. To have it set to 1 we use an X gate,
      --    hence depth is 1. Gatecount behaves the same
      QInit b -> 
        if b 
          then
            let
              gc' = increaseGateCount1 gc
            in go circ gc'
          else go circ gc
      QDiscard -> -- discarding wouldn't account for gatecounts, but in qasm resetting does
        let
          gc' = increaseGateCount1 gc
        in go circ gc'
      Meas ->
        let
          gc' = increaseGateCount1 gc
        in go circ gc'
      CInit _ -> go circ gc
      CDiscard -> go circ gc
      MCNot m ->
        let
          gc' = increaseGateCount gc (2*(m-1)+1) -- 2(m − 1) TOFFOLI gates and one CNOT gate
        in go circ gc'
      _ ->
        let
          gc' = increaseGateCount1 gc
        in go circ gc'