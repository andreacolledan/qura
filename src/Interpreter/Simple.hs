module Interpreter.Simple where

import Interpreter.RuntimeError
import Interpreter.Metric
import Circuit
import Circuit.Type
import Circuit.Bundle
import PrettyPrinter

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

  
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

-- | TODO TODO TODO TODO this quite unoptimized as it doesnt recycle on the least deep qubit, but on the first alphabetically :) we need to bring a labelcounts during the simplification
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
