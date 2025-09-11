module Interpreter.Qasm where

import Interpreter.RuntimeError
import Interpreter.Metric
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
    -- qasmProg = getQasm simplified
    qasmProg = -- FIXME this get printed between the metrics comment and the qasm program...
      -- trace("> Qasm Simplified Circuit:\n"++pretty simplified++"\n\n> Actual Program:")$
        getQasm simplified
    qasmMetrics = computeQasmMetrics simplified
  in QasmProg fp qasmMetrics qasmProg

-- | convert a circuit to have the same input and output names and update label context.
-- So, going from:
-- > CNot ((q2, q1)) -> (q3, q4);
-- to:
-- > CNot ((q2, q1)) -> (q2, q1);
simplifyCircuit :: Bool -> Circuit -> Circuit
simplifyCircuit r circ = 
  let 
  -- listify the operations
    circSeq = circTolist circ
  -- update names such that ins=outs and propagate the renamings
    circ' = getSimple r circSeq
  -- extract the actual labels
    labels = namesInCircuit' circ'
  -- update tthe label context
    newCtx = filterContext (getContext circ) labels -- using labels extract from the old context the relevant names
    circ'' = updateCircContext circ' newCtx
  in circ''

-- | TODO TODO TODO TODO ehm this quite unoptimezed as it doesnt recycle on the least deep qubit, but on the first alphabetically :) we need to bring a labelcounts during the simplification
getSimple :: Bool -> [CircuitInstruction] -> Circuit
getSimple _ [] = mkIdCircuit []
getSimple recycle ops = go ops recycle Set.empty (mkIdCircuit [])
  where
    go :: [CircuitInstruction] -> Bool -> Set.Set Label -> Circuit -> Circuit
    go [] _ _ circ = circ
    go (step:steps) recycle discarded circ = 
      case step of
        (Meas, (q, c)) ->
          let
            renaming = getWBRenaming (q, c)
            bundleRenaming = renameBundle renaming
            steps' =  map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs))) steps
            q' = bundleRenaming q
          in go steps' recycle discarded $ CCons circ Meas q' c
        (QDiscard, (WLab disc, _)) -> 
          go steps recycle (Set.insert disc discarded) $ CCons circ QDiscard (WLab disc) WUnit
        (QInit v, (_, WLab name))
          | recycle -> 
            let 
              (name', discarded') = pickOrDefault name discarded
              renaming = getWBRenaming (WLab name', WLab name)
              bundleRenaming = renameBundle renaming
              steps' =  map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs))) steps
            -- if the renaming is empty, it means that we are initalizing a new qubit,
            -- if a renaming occured, it means that we are reusing a discarded qubit
            in go steps' recycle discarded' $ CCons circ (QInit v) WUnit (WLab name')
          | otherwise -> 
            let
              outs = WLab name
              renaming = getWBRenaming (WUnit, outs)
              bundleRenaming = renameBundle renaming
              steps' =  map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs))) steps
              outs' = bundleRenaming outs
            in go steps' recycle discarded $ CCons circ (QInit v) WUnit outs'
        (qop, (ins, outs)) ->
          let
            renaming = getWBRenaming (ins, outs)
            bundleRenaming = renameBundle renaming
            steps' =  map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs))) steps
            ins' = bundleRenaming ins
            outs' = bundleRenaming outs
          in go steps' recycle discarded $ CCons circ qop ins' outs'

-- | Picks one element from the set if available, --FIXME doesnt acocun for depth, use picklessdepth from circuit.hs
-- otherwise returns the default value.
-- Also returns the updated set without the picked element.
pickOrDefault :: (Ord a) => a -> Set.Set a -> (a, Set.Set a)
pickOrDefault def s =
    case Set.minView s of
        Just (x, s') -> (x, s')    -- take smallest element and remaining set
        Nothing -> (def, s)   -- set is empty, use default

-- awful name
-- create a renaming from the second wire bundle to the first wire bundle
getWBRenaming :: (WireBundle, WireBundle) -> Renaming
getWBRenaming (WUnit, _) = Map.empty
getWBRenaming (_, WUnit) = Map.empty
getWBRenaming (WLab ins, WLab outs)
  | ins == outs = Map.empty
  | otherwise = Map.fromList [(outs, ins)]
getWBRenaming (WTuple ins, WTuple outs) =
    Map.unions $ zipWith getWBRenamingPair ins outs
  where
    getWBRenamingPair :: WireBundle -> WireBundle -> Renaming
    getWBRenamingPair i o = getWBRenaming (i, o)
getWBRenaming (WNil _, WNil _) = Map.empty
getWBRenaming (WCons xs x, WCons ys y) =
    Map.unions [getWBRenaming (xs, ys), getWBRenaming (x, y)]
getWBRenaming _ = error "[getWBRenaming] Unexpected error."

-- filter the context to only keep pairs existing in the given set
filterContext :: LabelContext -> Set.Set String -> LabelContext
filterContext ctx labels = Map.filterWithKey (\k _ -> k `Set.member` labels) ctx

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
      else ["bit " ++ b ++ ";"]
  in (decl ++ [b ++ " = measure " ++ q ++ ";"], Set.insert b existing)
-- Bit metaoperations
opToQasm (CInit b, (_, WLab name)) existing =
  let 
    decl = if name `Set.member` existing
      then []
      else ["bit "]
  in (decl ++ [name ++ " = " ++ (if b then "1" else "0") ++ ";"], Set.insert name existing)
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
  (["cx " ++ ctrl ++ ", " ++ trgt ++ ";"], existing)
opToQasm (CZ, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["cz " ++ ctrl ++ ", " ++ trgt ++ ";"], existing)
opToQasm (CR n, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["crz(" ++ thetaStr n ++ ") " ++ ctrl ++ ", " ++ trgt ++ ";"], existing)
opToQasm (CRinv n, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["crz(" ++ thetaInvStr n ++ ") " ++ ctrl ++ ", " ++ trgt ++ ";"], existing)
-- Classically controlled gates
-- README Actually, we treat those as quantum-controlled gates
opToQasm (CCNot, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["cx " ++ ctrl ++ ", " ++ trgt ++ ";"], existing) -- README we are using quantum gates!
opToQasm (CCZ, (WTuple [WLab ctrl, WLab trgt], _)) existing = 
  (["cz " ++ ctrl ++ ", " ++ trgt ++ ";"], existing) -- README we are using quantum gates!
-- Three qubit gates
opToQasm (Toffoli, (WTuple [WLab ctrl1, WLab ctrl2, WLab trgt], _)) existing = 
  (["ccx " ++ ctrl1 ++ ", " ++ ctrl2 ++ ", " ++ trgt ++ ";"], existing)
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