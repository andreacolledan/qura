module Interpreter.Qasm where

import Interpreter.RuntimeError
import Interpreter.Metric
import Interpreter.Simple
import Circuit
import Circuit.Type
import Circuit.Bundle
import PrettyPrinter
import Interface

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)

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
circuitToQasm :: Circuit -> CLArguments -> QasmProgram
circuitToQasm circ CommandLineArguments {filepath=fp, qubitRecycling = r} =
  let 
    simplified = simplifyCircuit r circ
    qasmProg = -- FIXME this get printed between the metrics comment and the qasm program...
      --trace("> Qasm Simplified Circuit:\n"++pretty simplified++"\n\n> Actual Program:")$
        getQasm simplified
    qasmMetrics = computeQasmMetrics simplified
  in QasmProg fp qasmMetrics qasmProg

--- INSTRUCTION GENERATION ---

getHeader :: String -> [String]
getHeader v = case v of
  "qasm3.0" -> ["OPENQASM 3.0;","include \"stdgates.inc\";"]
  -- _ -> error "[getHeader] Unsupported version: " ++ show v

-- Generates the instructions representing the program from a circuit
-- FIXME for now we simply convert the Circuit 1 to 1.
-- Later, we might want to add a toggle to prefer width/depth on qubit inits
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

-- convert a list of quantum operations and labels to a list of qasm instructions
opsToQasm :: [CircuitInstruction] -> [QasmInstruction]
opsToQasm = fst . foldl step ([], Set.empty)
  where
    step (acc, labels) op =
      let (instr, labels') = opToQasm op labels
      in (acc ++ instr, labels')

-- Convert a quantum operation to a list of qasm instructions. A list is used to keep trace of
-- initialized qubits.
opToQasm :: CircuitInstruction -> Set.Set Label -> ([QasmInstruction], Set.Set Label)
-- Qubit metaoperations
opToQasm (QInit b, (_, WLab name)) existing =
  let 
    decl = if name `Set.member` existing
             then []
             else ["qubit " ++ name ++ ";"]
  in (decl ++ if b then ["x " ++ name ++ ";"] else [], Set.insert name existing)
opToQasm (QDiscard, (WLab name, _)) existing = 
  (["reset " ++ name ++ ";"], existing)
opToQasm (Meas, (WLab q, WLab b)) existing = 
  let 
    decl = if b `Set.member` existing
      then []
      else ["bit[1] " ++ b ++ ";"]
  in (decl ++ [b ++ " = measure " ++ q ++ ";"], Set.insert b existing)
-- Bit metaoperations
opToQasm (CInit b, (_, WLab name)) existing =
  let 
    decl = if name `Set.member` existing
      then ""
      else "bit[1] "
  in ( [decl ++ name ++ " = \"" ++ (if b then "1" else "0") ++ "\";"], Set.insert name existing)
opToQasm (CDiscard, (WLab name, _)) existing = 
  ([], existing) -- no instruction exists to discard a bit, nor the need to do it
-- Single qubit gates
opToQasm (Hadamard, (WLab name, _)) existing = 
  (["h " ++ name ++ ";"], existing)
opToQasm (PauliX, (WLab name, _)) existing = 
  (["x " ++ name ++ ";"], existing)
opToQasm (PauliY, (WLab name, _)) existing = 
  (["y " ++ name ++ ";"], existing)
opToQasm (PauliZ, (WLab name, _)) existing = 
  (["z " ++ name ++ ";"], existing)
opToQasm (T, (WLab name, _)) existing = 
  (["t " ++ name ++ ";"], existing)
opToQasm (R n, (WLab name, _)) existing = 
  (["rz(" ++ thetaStr n ++ ") " ++ name ++ ";"], existing)
opToQasm (Rinv n, (WLab name, _)) existing = 
  (["rz(" ++ thetaInvStr n ++ ") " ++ name ++ ";"], existing)
-- Two qubit gates
opToQasm (CNot, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["cx " ++ ctrl ++ "," ++ trgt ++ ";"], existing)
opToQasm (CZ, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["cz " ++ ctrl ++ "," ++ trgt ++ ";"], existing)
opToQasm (CR n, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["crz(" ++ thetaStr n ++ ") " ++ ctrl ++ "," ++ trgt ++ ";"], existing)
opToQasm (CRinv n, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["crz(" ++ thetaInvStr n ++ ") " ++ ctrl ++ "," ++ trgt ++ ";"], existing)
-- Classically controlled gates
-- README Actually, we treat those as quantum-controlled gates 
-- TODO since we simplify to not have classically-controlled, this should not exist anymore
opToQasm (CCNot, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["cx " ++ ctrl ++ "," ++ trgt ++ ";"], existing) -- README we are using quantum gates!
opToQasm (CCZ, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["cz " ++ ctrl ++ "," ++ trgt ++ ";"], existing) -- README we are using quantum gates!
-- Three qubit gates
opToQasm (Toffoli, (WTuple [WLab ctrl1, WLab ctrl2, WLab trgt], _)) existing = 
  (["ccx " ++ ctrl1 ++ "," ++ ctrl2 ++ "," ++ trgt ++ ";"], existing)
-- undefined
opToQasm (unk, (ins, _)) e = (["// placeholder for: "++show unk++" ("++pretty ins++")"], e)


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
      --    hence depth is 1. Gatecount behaves the same. -- CHECKME is this fine?
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
      --    hence depth is 1. Gatecount behaves the same. -- CHECKME is this fine?
      QInit b -> 
        if b 
          then
            let
              gc' = increaseGateCount1 gc -- CHECKME: or maybe not??? qura doesnt not account for this
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