module Interpreter.Qasm where

import Interpreter.RuntimeError
import Circuit
import PrettyPrinter(pretty)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)

type QasmProgram = String -- maybe create a class program of saveable strings

-- converts a circuit to a qasm program.
circuitToQasm :: Circuit -> Either RuntimeError QasmProgram
circuitToQasm circ =
  let 
    simplified = simplifyCircuit circ
    -- simplified = trace("\nInput Circuit:\n"++pretty circ)$simplifyCircuit circ
    -- qasmProg = getQasm simplified
    qasmProg = trace("\nSimplified Circuit:\n"++pretty simplified++"\n\nActual Program:\n")$getQasm simplified
  in Right qasmProg

-- | convert a circuit to have the same input and output names and update label context.
-- So, going from:
-- > CNot ((q2, q1)) -> (q3, q4);
-- to:
-- > CNot ((q2, q1)) -> (q2, q1);
simplifyCircuit :: Circuit -> Circuit
simplifyCircuit circ = 
  -- listify the operatinos
  let 
    circSeq = circTolist circ
  -- update names such that ins=outs and propagate the renamings
    circ' = getSimple circSeq
  -- extract the actual labels
    labels = namesInCircuit' circ'
  -- update tthe label context
    newCtx = filterContext (getContext circ) labels -- using labels extract from the old context the relevant names
    circ'' = updateCircContext circ' newCtx
  in circ''

-- changes the names of the wirebundles and reconstructs the circuit
getSimple:: [(QuantumOperation, (WireBundle, WireBundle))] -> Circuit
getSimple [] = mkIdCircuit []
getSimple ops = go ops (mkIdCircuit [])
  where 
    go :: [(QuantumOperation, (WireBundle, WireBundle))] -> Circuit -> Circuit
    go [] circ = circ
    go (step:steps) circ = 
      case step of
        (Meas, (q, c)) -> go steps $ CCons circ Meas q c -- ins =/= outs
        (op, (ins, outs)) ->
          let
            renaming = getWBRenaming (ins, outs)
            bundleRenaming = renameBundle renaming
            steps' =  map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs))) steps
            ins' = bundleRenaming ins
            outs' = bundleRenaming outs
          in go steps' $ CCons circ op ins' outs'

-- create a list of (op,(ins,outs)) from a circuit. Ignores the label context
circTolist :: Circuit -> [(QuantumOperation, (WireBundle, WireBundle))]
circTolist (Id _) = []
circTolist (CCons circ op ins outs) = circTolist circ ++ [(op, (ins, outs))]


-- awful name
getWBRenaming :: (WireBundle, WireBundle) -> Renaming
getWBRenaming (WUnit, _) = Map.empty
getWBRenaming (_, WUnit) = Map.empty
getWBRenaming (WLab ins, WLab outs) = Map.fromList [(outs, ins)]
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

--- STRING GENERATION ---

getHeader :: String -> String
getHeader v = case v of
  "qasm3.0" -> "OPENQASM 3.0;\ninclude \"stdgates.inc\";"
  _ -> error "[getHeader] Unsupported version " ++ show v

thetaStr :: Int -> String
thetaStr n = "pi/" ++ show (2^(n-1))

thetaInvStr :: Int -> String
thetaInvStr n = "-" ++ thetaStr n

-- README should we have a constructor for qasm terms and return that, then stringify later?
opToQasm :: (QuantumOperation, (WireBundle, WireBundle)) -> Maybe String
-- Qubit metaoperations
opToQasm (QInit b, (_, WLab name)) =
  let 
    decl = "qubit " ++ name ++ ";"
  in if b
    then Just $ decl ++ "\nx " ++ name ++ ";"
    else Just $ decl
opToQasm (QDiscard, (WLab name, _)) = Just $ "reset " ++ name ++ ";"
opToQasm (Meas, (WLab q, WLab b)) = Just $ b++ " = measure " ++ q ++ ";"
-- Bit metaoperations
opToQasm (CInit b, (_, WLab name)) =
  Just $ "bit " ++ name ++ " = " ++ (if b then "1" else "0") ++ ";" -- in qiskit we cant assign values to bits
opToQasm (CDiscard, (WLab name, _)) = Nothing -- no intruction to do so, nor a reason
-- Single qubit gates
opToQasm (Hadamard, (WLab name, _)) = Just $ "h " ++ name ++ ";"
opToQasm (PauliX, (WLab name, _)) = Just $ "x " ++ name ++ ";"
opToQasm (PauliY, (WLab name, _)) = Just $ "y " ++ name ++ ";"
opToQasm (PauliZ, (WLab name, _)) = Just $ "z " ++ name ++ ";"
opToQasm (T, (WLab name, _)) = Just $ "t " ++ name ++ ";"
opToQasm (R n, (WLab name, _)) = Just $ "rz(" ++ thetaStr n ++ ") " ++ name ++ ";"
opToQasm (Rinv n, (WLab name, _)) = Just $ "rz(" ++ thetaInvStr n ++ ") " ++ name ++ ";"
-- Two qubit gates
opToQasm (CNot, (WTuple [WLab ctrl, WLab trgt], _)) = Just $ "cx " ++ ctrl ++ ", " ++ trgt ++ ";"
opToQasm (CZ, (WTuple [WLab ctrl, WLab trgt], _)) = Just $ "cz " ++ ctrl ++ ", " ++ trgt ++ ";"
opToQasm (CR n, (WTuple [WLab ctrl, WLab trgt], _)) = Just $ "crz(" ++ thetaStr n ++ ") " ++ ctrl ++ ", " ++ trgt ++ ";"
opToQasm (CRinv n, (WTuple [WLab ctrl, WLab trgt], _)) = Just $ "crz(" ++ thetaInvStr n ++ ") " ++ ctrl ++ ", " ++ trgt ++ ";"
-- Classically controlled gates
opToQasm (CCNot, (WTuple [WLab ctrl, WLab trgt], _)) = Just $ "if(" ++ ctrl ++ ") x " ++ trgt ++ ";"
opToQasm (CCZ, (WTuple [WLab ctrl, WLab trgt], _)) = Just $ "if(" ++ ctrl ++ ") z " ++ trgt ++ ";"
-- Three qubit gates
opToQasm (CNot, (WTuple [WLab ctrl1, WLab ctrl2, WLab trgt], _)) = Just $ "ccx " ++ ctrl1 ++ ", " ++ ctrl2 ++ ", " ++ trgt ++ ";"
opToQasm _ = Just $ "placeolder"

bitsNames :: LabelContext -> [Label]
bitsNames ctx = [label | (label, wireType) <- Map.toList ctx, wireType == Bit]

initBit :: Label -> String
initBit name = "bit " ++ name ++ ";"

-- given all the bists of the label context, remove the bits that are explicitely initialized
filterBitLbels :: [Label] -> [(QuantumOperation, (WireBundle, WireBundle))] -> [Label]
filterBitLbels labels ops =
    filter (`notElem` bitsToRemove) labels
  where
    -- Extract labels from WLab in operations where the op is CInit
    bitsToRemove :: [Label]
    bitsToRemove = [label | (CInit _, (WLab label, _)) <- ops]

-- Generates the string representing the program from a circuit
-- FIXME for now we simply convert the Circuit 1 to 1.
-- Later, we might want to add a toggle to prefer width/depth on qubit inits
-- README maybe add the version as a command line arg (and maybe add errors along the way)
getQasm :: Circuit -> QasmProgram
getQasm circ = 
  let
    header = getHeader "qasm3.0" -- version, imports
    circSeq = circTolist circ
    -- Bits that are not explicitely initialized need to be init.
    -- If we want to save the result of a Meas, the bit should already exist,
    -- but we can't init all of the bits otherwise we can't assign them to a specific value
    -- i dont know if all of this is qiskit only :)
    ctx = getContext circ
    bits = bitsNames ctx
    bitsInits = map initBit $ filterBitLbels bits circSeq
    -- stringify operations
    instructions = mapMaybe opToQasm circSeq
  in unlines $ [header] ++ bitsInits ++ instructions