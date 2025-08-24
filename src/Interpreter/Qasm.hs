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
circuitToQasm :: Bool -> Circuit -> Either RuntimeError QasmProgram
circuitToQasm pw circ =
  let 
    simplified = simplifyCircuit pw circ
    qasmProg = trace("> Preferring width: "++show pw++"\nSimplified Circuit:\n"++pretty simplified++"\n\nActual Program:\n")$getQasm simplified
  in Right qasmProg

-- | convert a circuit to have the same input and output names and update label context.
-- So, going from:
-- > CNot ((q2, q1)) -> (q3, q4);
-- to:
-- > CNot ((q2, q1)) -> (q2, q1);
simplifyCircuit :: Bool -> Circuit -> Circuit
simplifyCircuit pw circ = 
  let 
  -- listify the operations
    circSeq = circTolist circ
  -- update names such that ins=outs and propagate the renamings
    circ' = getSimple pw circSeq
  -- extract the actual labels
    labels = namesInCircuit' circ'
  -- update tthe label context
    newCtx = filterContext (getContext circ) labels -- using labels extract from the old context the relevant names
    circ'' = updateCircContext circ' newCtx
  in circ''

-- changes the names of the wirebundles and reconstructs the circuit.
getSimple :: Bool -> [(QuantumOperation, (WireBundle, WireBundle))] -> Circuit
getSimple True seq = getSimplePW seq
getSimple False seq = getSimpleNoPW seq

-- initializes new qubits trying to reset unused qubits.
getSimplePW:: [(QuantumOperation, (WireBundle, WireBundle))] -> Circuit
getSimplePW [] = mkIdCircuit []
getSimplePW ops = go ops Set.empty (mkIdCircuit [])
  where 
    go :: [(QuantumOperation, (WireBundle, WireBundle))] -> Set.Set Label -> Circuit -> Circuit
    go [] _ circ = circ
    go (step:steps) discarded circ = 
      case step of
        (Meas, (q, c)) -> -- we apply the renaming BUT we keep outputs
          let
            renaming = getWBRenaming (q, c)
            bundleRenaming = renameBundle renaming
            steps' =  map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs))) steps
            q' = bundleRenaming q
          in go steps' discarded $ CCons circ Meas q' c
        (QDiscard, (WLab disc, _)) -> 
          go steps (Set.insert disc discarded) $ CCons circ QDiscard (WLab disc) (WLab disc)
        (QInit v, (_, WLab name)) ->
          let 
            (name', discarded') = pickOrDefault name discarded
            renaming = getWBRenaming (WLab name', WLab name)
            bundleRenaming = renameBundle renaming
            steps' =  map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs))) steps
          -- if the renaming is empty, it means that we are initalizing a new qubit,
          -- if a renaming occured, it means that we are reusing a discarded qubit
          in go steps' discarded' $ CCons circ (QInit v) (if Map.null renaming then WUnit else WLab name') (WLab name')
        (op, (ins, outs)) ->
          let
            renaming = getWBRenaming (ins, outs)
            bundleRenaming = renameBundle renaming
            steps' =  map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs))) steps
            ins' = bundleRenaming ins
            outs' = bundleRenaming outs
          in go steps' discarded $ CCons circ op ins' outs'

-- Initializes new qubit at depth 0.
getSimpleNoPW:: [(QuantumOperation, (WireBundle, WireBundle))] -> Circuit
getSimpleNoPW [] = mkIdCircuit []
getSimpleNoPW ops = go ops (mkIdCircuit [])
  where 
    go :: [(QuantumOperation, (WireBundle, WireBundle))] -> Circuit -> Circuit
    go [] circ = circ
    go (step:steps) circ = 
      case step of
        (Meas, (q, c)) -> -- we apply the renaming BUT we keep outputs
          let
            renaming = getWBRenaming (q, c)
            bundleRenaming = renameBundle renaming
            steps' =  map (\(op, (q, c)) -> (op, (bundleRenaming q, bundleRenaming c))) steps
            q' = bundleRenaming q
          in go steps' $ CCons circ Meas q' c
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

-- | Picks one element from the set if available,
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
-- to avoid empty lines we return Nothing
opToQasm :: (QuantumOperation, (WireBundle, WireBundle)) -> Maybe String
-- Qubit metaoperations
opToQasm (QInit b, (init, WLab name)) = -- check the input to know if the qubit needs initialization
  case init of
    WUnit -> 
      let 
        decl = "qubit " ++ name ++ ";"
      in if b
        then Just $ decl ++ "\nx " ++ name ++ ";"
        else Just $ decl
    WLab _ -> 
      if b
        then Just $ "x " ++ name ++ ";" -- already init, so we set to 1
        else Nothing -- already set to 0, no need to init
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
-- README Actually, we treat those as quantum-controlled gates
opToQasm (CCNot, (WTuple [WLab ctrl, WLab trgt], _)) = Just $ "cx " ++ ctrl ++ ", " ++ trgt ++ ";" -- README we are using quantum gates!
opToQasm (CCZ, (WTuple [WLab ctrl, WLab trgt], _)) = Just $ "cz " ++ ctrl ++ ", " ++ trgt ++ ";" -- README we are using quantum gates!
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