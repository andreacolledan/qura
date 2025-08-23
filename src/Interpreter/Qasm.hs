module Interpreter.Qasm where

import Interpreter.RuntimeError
import Circuit
import PrettyPrinter(pretty)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

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
      let
        (op, (ins, outs)) = step
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
  "qasm3.0" -> "OPENQASM 3.0;"
  _ -> error "[getHeader] Unsupported version " ++ show v

-- README should we have a constructor for qasm terms and return that, then stringify later?
opToQasm :: (QuantumOperation, (WireBundle, WireBundle)) -> String
opToQasm _ = "QASM operation!"

-- Generates the string representing the program from a circuit
-- FIXME for now we simply convert the Circuit 1 to 1.
-- Later, we might want to add a toggle to prefer width/depth on qubit inits
-- README maybe add the version as a command line arg (and maybe add errors along the way)
getQasm :: Circuit -> QasmProgram
getQasm circ = 
  let
    header = getHeader "qasm3.0"
    ctx = getContext circ
    instructions = map opToQasm $ circTolist circ
  in unlines $ [header] ++ instructions