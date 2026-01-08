module Interpreter.Simple where

import Circuit
  ( Circuit (..),
    CircuitInstruction,
    LabelCounts,
    circTolist,
    getContext,
    initCounter,
    mkIdCircuit,
    namesInCircuit',
    pickLessDeep,
    updateCircContext,
    updateDepthAmount
  )
import Circuit.Type (QuantumOperation (..))
import Circuit.Bundle (Label, LabelContext, Renaming, WireBundle (..), renameBundle)

import qualified Data.Map.Strict as Map (empty, filterWithKey, fromList, unions)
import qualified Data.Set as Set (Set, empty, insert, member)

  
-- | convert a circuit to have the same input and output names and update label context.
-- So, going from:
-- > CNot ((q2, q1)) -> (q3, q4);
-- to:
-- > CNot ((q2, q1)) -> (q2, q1);
simplifyCircuit :: Bool -> Circuit -> Circuit -- README maybe this can be used for other languages aswell and could be moved to runIntepreter
simplifyCircuit r circ = 
  let 
  -- listify the operations
    circSeq = circTolist circ
    ctx = getContext circ
  -- update names such that ins=outs and propagate the renamings
    circ' = getSimple r ctx circSeq
  -- extract the actual labels
    labels = namesInCircuit' circ'
  -- update tthe label context
    newCtx = filterContext ctx labels -- using labels extract from the old context the relevant names
    circ'' = updateCircContext circ' newCtx
  in circ''

-- | this has been optimized as it now recycles on the least deep qubit
getSimple :: Bool -> LabelContext -> [CircuitInstruction] -> Circuit
getSimple _ _ [] = mkIdCircuit []
getSimple recycle ctx ops = go ops recycle Set.empty (initCounter ctx) (mkIdCircuit [])
  where
    go :: [CircuitInstruction] -> Bool -> Set.Set Label -> LabelCounts -> Circuit -> Circuit
    go [] _ _ _ circ = circ
    go (step:steps) recycle discarded lc circ = 
      case step of
        (Meas, (q, c)) ->
          let
            renaming = makeWireBundleRenaming (q, c)
            bundleRenaming = renameBundle renaming
            steps' = renameSteps bundleRenaming steps
            q' = bundleRenaming q
            c' = bundleRenaming c
            lc' = updateDepthAmount 1 lc q' c'
          in go steps' recycle discarded lc' $ CCons circ Meas q' c'
        (QDiscard, (WLab disc, _)) -> 
          go steps recycle (Set.insert disc discarded) lc $ CCons circ QDiscard (WLab disc) WUnit
        (QInit v, (_, WLab name))
          | recycle ->
            let 
              (name', discarded') = pickLessDeep name lc discarded
              renaming =
                -- trace(show lc ++", discarded: "++show discarded ++"\nname' = "++name'++", discarded' = "++show discarded') $ 
                  makeWireBundleRenaming (WLab name', WLab name)
              bundleRenaming =
                -- trace ("[getSimple/QInit recycle] renaming = " ++ show renaming) $
                  renameBundle renaming
              steps' = renameSteps bundleRenaming steps
              lc' = updateDepthAmount 0 lc (WLab name) (WLab name')
            -- if the renaming is empty, it means that we are initalizing a new qubit,
            -- if a renaming occured, it means that we are reusing a discarded qubit
            in go steps' recycle discarded' lc' $ CCons circ (QInit v) WUnit (WLab name')
          | otherwise -> 
            let
              outs = WLab name
              renaming = makeWireBundleRenaming (WUnit, outs)
              bundleRenaming = renameBundle renaming
              steps' = renameSteps bundleRenaming steps
              outs' = bundleRenaming outs
            in go steps' recycle discarded lc $ CCons circ (QInit v) WUnit outs'
        (qop, (ins, outs)) ->
          let
            renaming = makeWireBundleRenaming (ins, outs)
            bundleRenaming = renameBundle renaming
            steps' = renameSteps bundleRenaming steps
            ins' = bundleRenaming ins
            outs' = bundleRenaming outs
            lc' = updateDepthAmount 1 lc ins' outs'
          in go steps' recycle discarded lc' $ CCons circ qop ins' outs'

    renameSteps :: (WireBundle -> WireBundle) -> [CircuitInstruction] -> [CircuitInstruction]
    renameSteps bundleRenaming =
      map (\(op, (ins, outs)) -> (op, (bundleRenaming ins, bundleRenaming outs)))

-- create a renaming from the second wire bundle to the first wire bundle
makeWireBundleRenaming :: (WireBundle, WireBundle) -> Renaming
makeWireBundleRenaming (WUnit, _) = Map.empty
makeWireBundleRenaming (_, WUnit) = Map.empty
makeWireBundleRenaming (WLab ins, WLab outs)
  | ins == outs = Map.empty
  | otherwise = Map.fromList [(outs, ins)]
makeWireBundleRenaming (WTuple ins, WTuple outs) =
    Map.unions $ zipWith makeWireBundleRenamingPair ins outs
  where
    makeWireBundleRenamingPair :: WireBundle -> WireBundle -> Renaming
    makeWireBundleRenamingPair i o = makeWireBundleRenaming (i, o)
makeWireBundleRenaming (WNil _, WNil _) = Map.empty
makeWireBundleRenaming (WCons xs x, WCons ys y) =
    Map.unions [makeWireBundleRenaming (xs, ys), makeWireBundleRenaming (x, y)]
makeWireBundleRenaming _ = error "[makeWireBundleRenaming] Unexpected error."

-- filter the context to only keep pairs existing in the given set
filterContext :: LabelContext -> Set.Set String -> LabelContext
filterContext ctx labels = Map.filterWithKey (\k _ -> k `Set.member` labels) ctx
