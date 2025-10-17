{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Circuit where

import PrettyPrinter
import Circuit.Type
import Circuit.Bundle
import Analyzer.Unify
import Interpreter.Metric

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (intercalate)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Circuit Datatype

data Circuit = -- This corresponds to CRL expressions in the original paper
    Id LabelContext
  | CCons Circuit QuantumOperation WireBundle WireBundle
  deriving (Eq, Show)

mkIdCircuit :: [(Label, WireType)] -> Circuit
mkIdCircuit pairs = Id (Map.fromList pairs)

-- recurse to the id to extract the context
getContext :: Circuit -> LabelContext
getContext (Id q) = q
getContext (CCons circ _ _ _ ) = getContext circ

-- updates the context. it does not check if the labels are correct.
updateCircContext :: Circuit -> LabelContext -> Circuit
updateCircContext (Id _) q = Id q
updateCircContext (CCons circ op ins outs) q = CCons (updateCircContext circ q) op ins outs

instance Pretty WireBundle where
  pretty bundle = case bundle of
    WUnit -> "*"
    WLab l -> l
    WTuple t -> "(" ++ intercalate ", " (map pretty t) ++ ")"
    WNil _ -> "()" -- ?
    WCons e1 e2 -> "(" ++ pretty e1 ++ ":" ++ pretty e2 ++ ")"

instance Pretty LabelContext where
  pretty ctx
    | Map.null ctx = "[empty]"
    | otherwise    =
        let pairs = Map.toList ctx
            prettyPair (l, t) = l ++ ":" ++ pretty t
        in intercalate ", " (map prettyPair pairs)

instance Pretty Circuit where
  pretty circ =
    let circLines = linesOf circ
        circId    = head circLines
        opsLines  = drop 1 circLines
        circOps   = if null opsLines
                    then "[no operations]"
                    else concatLines opsLines
    in circId ++ "\n> Operations:\n" ++ circOps
    where
      linesOf :: Circuit -> [String]
      linesOf (Id ctx) = ["> Label Context: " ++ pretty ctx]
      linesOf (CCons c op ins outs) =
        linesOf c ++ [pretty op ++ " (" ++ pretty ins ++ ") -> " ++ pretty outs]

      concatLines :: [String] -> String
      concatLines []     = "[no operations]" -- safety fallback
      concatLines [x]    = x ++ "."
      concatLines (x:xs) = x ++ ";\n" ++ concatLines xs


-- Circuit operations and helpers

namesInCircuit :: Circuit -> Set.Set String
namesInCircuit (Id ctx) = Set.fromList (Map.keys ctx)
namesInCircuit (CCons circ _ ins outs) =
    Set.unions
      [ namesInCircuit circ
      , namesInBundle ins
      , namesInBundle outs
      ]

-- ignores the label context and only registers wire bundles
namesInCircuit' :: Circuit -> Set.Set String
namesInCircuit' (Id ctx) = Set.empty
namesInCircuit' (CCons circ _ ins outs) =
    Set.unions
      [ namesInCircuit' circ
      , namesInBundle ins
      , namesInBundle outs
      ]

namesInBox :: (WireBundle, Circuit, WireBundle) -> Set.Set String -- string bcs there are labels and variableIds
namesInBox (ins, circ, outs) = 
  Set.unions [namesInBundle ins, namesInCircuit circ, namesInBundle outs]

circConcat :: Circuit -> Circuit -> Circuit
circConcat c (Id _) = c
circConcat c (CCons d g l k) = CCons (circConcat c d) g l k

--- rename 
renameCircuit :: Renaming -> Circuit -> Circuit
renameCircuit rn (Id ctx) =
  Id (renameLabelContext rn ctx)
renameCircuit rn (CCons c op ins outs) =
  CCons (renameCircuit rn c) op (renameBundle rn ins) (renameBundle rn outs)

updateBoxNames :: Renaming -> (WireBundle, Circuit, WireBundle) -> (WireBundle, Circuit, WireBundle)
updateBoxNames rn (ins, circ, outs) = (renameBundle rn ins, renameCircuit rn circ, renameBundle rn outs)

--- metrics
type CircuitInstruction = (QuantumOperation, (WireBundle, WireBundle))
-- type CircuitSequence = [CircuitInstruction]

-- create a list of (op,(ins,outs)) from a circuit. Ignores the label context
circTolist :: Circuit -> [CircuitInstruction]
circTolist (Id _) = []
circTolist (CCons circ op ins outs) = circTolist circ ++ [(op, (ins, outs))]

-- convert a list of circ. instr. to a circuit with a new label context
listToCirc :: [CircuitInstruction] -> Circuit
listToCirc instrs = foldl (\circ (op, (ins, outs)) -> CCons circ op ins outs) (Id $ extractInits instrs) instrs
  
extractInits :: [CircuitInstruction] -> LabelContext
extractInits instrs = go instrs emptyContext
  where
    go [] ctx = ctx
    go (step:steps) ctx =
      case step of
        (QInit _, (_, WLab q)) -> go steps $ updateContext ctx q Qubit
        (CInit _, (_, WLab b)) -> go steps $ updateContext ctx b Bit
        _ -> go steps ctx

-- use the recycling flag to compute the 3 standard metrics
getCircuitMetrics :: Bool -> Circuit -> ProgramMetrics
getCircuitMetrics recycle circ = 
  let 
    circSeq = circTolist circ
    w = getWidth recycle circSeq
    d = getDepth recycle circ -- needs the ctx
    gc = getGateCount circSeq
  in ProgMetrics w d gc

getWidth :: Bool -> [CircuitInstruction] -> Int
getWidth _ [] = 0
getWidth recycle instrs = go recycle instrs 0 0 
  where
    go :: Bool -- recycling
       -> [CircuitInstruction] -- instructions
       -> Int -- currently discarded qubits
       -> Int -- currently discarded bits
       -> Int -- number of output wires
    go _ [] _ _ = 0
    go r ((op,_):steps) q b = case op of
      -- inits
      QInit _ -> if r && q > 0 
        then go r steps (q-1) b -- we recycle and have currently discarded qubits
        else 1 + go r steps q b -- we use a new wire if no discarded or no recycling
      CInit _ -> if r && b > 0 
        then go r steps q (b-1) -- we recycle and have currently discarded bits
        else 1 + go r steps q b -- we use a new wire if no discarded or no recycling
      -- discards
      QDiscard -> go r steps (q+1) b
      CDiscard -> go r steps q (b+1)
      -- measure
      Meas -> go r steps q b
      -- other operations
      _ -> go r steps q b 

--- DEPTH 
type LabelCounts = Map.Map Label Int
initCounter :: LabelContext -> LabelCounts
initCounter ctx = Map.fromList [(label, 0) | label <- Map.keys ctx]
-- When computing the depth, we don't simply add one to the counts of each label of the gate, but we have
-- to take the maximum depth of the labels in the gates, add one and then update all the *output* labels with this new depth.
updateDepthAmount :: Int -> LabelCounts -> WireBundle -> WireBundle -> LabelCounts
updateDepthAmount amount lc ins outs = -- update outs with the max depth+amount of ins
  let 
    insnames = namesInBundle ins
    outsnames = namesInBundle outs
    inDepths = [ Map.findWithDefault 0 l lc | l <- Set.toList insnames ]
    d = amount + maximum (0 : inDepths)
  in foldr (\out acc -> Map.insert out d acc) lc outsnames
maxCount :: LabelCounts -> Int
maxCount lc | Map.null lc = 0 | otherwise = maximum (Map.elems lc)
-- | Picks the least deep element from the set if available, otherwise returns the default value.
-- Also returns the updated set without the picked element.
pickLessDeep :: Label -> LabelCounts -> Set.Set Label -> (Label, Set.Set Label)
pickLessDeep l lc discarded =
    let elems = Set.toList discarded
        chosen = if null elems
          then l
          else minimumBy (comparing (\lbl -> lc Map.! lbl)) elems
        discarded' = Set.delete chosen discarded
    in (chosen, discarded')

-- |
getDepth :: Bool -> Circuit -> Int
getDepth recycle circ =
  let
    instrs = circTolist circ
    lc = go recycle instrs Set.empty Set.empty $ initCounter $ getContext circ
  in maxCount lc
    where 
      go :: Bool
         -> [CircuitInstruction]
         -> Set.Set Label -- currently discared qubits
         -> Set.Set Label -- currently discared bits
         -> LabelCounts -- current depths
         -> LabelCounts
      go _ [] _ _ lc = lc
      go recycle ((op,(ins,outs)):steps) qubits bits lc = case op of
        -- operations that initializes new wires or recycles them
        QInit _ -> -- if recycling puts the new qubit at the depth of the less deep discarded qubit
          if recycle 
            then let
              WLab name = outs
              (name', qubits') = pickLessDeep name lc qubits
              -- put the new label at depth of the least deep dicscarded label
              lc' = updateDepthAmount 0 lc (WLab name) (WLab name') 
            in go recycle steps qubits' bits lc'
            else go recycle steps qubits bits lc
        CInit _ -> -- if recycling puts the new qubit at the depth of the less deep discarded qubit
          if recycle 
            then let
              WLab name = outs
              (name', bits') = pickLessDeep name lc bits
              -- put the new label at depth of the least deep dicscarded label
              lc' = updateDepthAmount 0 lc (WLab name) (WLab name') 
            in go recycle steps qubits bits' lc'
            else go recycle steps qubits bits lc
        -- operations that discards wires
        QDiscard -> 
          let
            WLab name = ins
            qubits' = Set.insert name qubits
          in go recycle steps qubits' bits lc
        CDiscard -> 
          let
            WLab name = ins
            bits' = Set.insert name bits
          in go recycle steps qubits bits' lc
        -- measurement and remaining operations (gates)
        _ -> go recycle steps qubits bits $ updateDepthAmount 1 lc ins outs

--- GATECOUNT
getGateCount :: [CircuitInstruction] -> Int
getGateCount [] = 0
getGateCount ((op,_):steps) = case op of
  -- operations with no cost
  QInit _ -> getGateCount steps
  QDiscard -> getGateCount steps
  CInit _ -> getGateCount steps
  CDiscard -> getGateCount steps
  -- other gates have cost 1
  _ -> 1 + getGateCount steps