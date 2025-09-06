module Circuit.Bundle where

import Circuit.Type
import PQ.Index

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Bundles Datatype

type Label = String

data BundleType =
    BUnit
  | BWire WireType
  | BTensor [BundleType]
  | BList IVarId Index BundleType
  deriving (Eq, Show)

data WireBundle =
    WUnit 
  | WLab Label
  | WTuple [WireBundle]
  | WNil (Maybe BundleType) 
  | WCons WireBundle WireBundle 
  deriving (Eq, Show)

namesInBundle :: WireBundle -> Set.Set String
namesInBundle WUnit = Set.empty
namesInBundle (WLab label) = Set.singleton label
namesInBundle (WTuple ws) = Set.unions (map namesInBundle ws)
namesInBundle (WNil _) = Set.empty
namesInBundle (WCons w ws) = namesInBundle w `Set.union` namesInBundle ws


-- typeOfBundle :: WireBundle -> BundleType
-- typeOfBundle WUnit = BUnit
-- typeOfBundle _ = undefined

outTypeQuantOP :: QuantumOperation -> BundleType
outTypeQuantOP (QInit _) = BWire Qubit
outTypeQuantOP (QDiscard) = BUnit
outTypeQuantOP (Meas) = BWire Bit
outTypeQuantOP (CInit _) = BWire Bit
outTypeQuantOP (CDiscard) = BUnit
outTypeQuantOP (Hadamard) = BWire Qubit
outTypeQuantOP (PauliX) = BWire Qubit
outTypeQuantOP (PauliY) = BWire Qubit
outTypeQuantOP (PauliZ) = BWire Qubit
outTypeQuantOP (T) = BWire Qubit
outTypeQuantOP (R _) = BWire Qubit
outTypeQuantOP (Rinv _) = BWire Qubit
outTypeQuantOP (CNot) = BTensor [BWire Qubit, BWire Qubit]
outTypeQuantOP (CZ) = BTensor [BWire Qubit, BWire Qubit]
outTypeQuantOP (CR _) = BTensor [BWire Qubit, BWire Qubit]
outTypeQuantOP (CRinv _) = BTensor [BWire Qubit, BWire Qubit]
outTypeQuantOP (CCNot) = BTensor [BWire Bit, BWire Qubit]
outTypeQuantOP (CCZ) = BTensor [BWire Bit, BWire Qubit]
outTypeQuantOP (Toffoli) = BTensor [BWire Qubit, BWire Qubit, BWire Qubit]

-- Label Context 

type LabelContext = Map Label WireType -- Q

emptyContext :: LabelContext
emptyContext = Map.empty

insert :: LabelContext -> (Label, WireType) -> LabelContext
insert q (l, t) = Map.insert l t q

mergeContexts :: LabelContext -> LabelContext -> LabelContext
mergeContexts = Map.union

freshlabels :: BundleType -> LabelContext -> (LabelContext, WireBundle)
freshlabels t q = case t of
  BUnit -> (q, WUnit)

  BWire wt -> 
    let
      base = basename wt
      names = [base : show n | n <- [(0::Int)..]]
      name = head $ filter (`Map.notMember` q) names
      q' = Map.insert name wt q
    in (q', WLab name)

  BTensor ts ->
    let
      go :: LabelContext -> [BundleType] -> (LabelContext, [WireBundle])
      go ctx [] = (ctx, [])
      go ctx (b:bs) = 
        let 
          (ctx', wb) = freshlabels b ctx
          (ctx'', wbs) = go ctx' bs
        in (ctx'', wb : wbs)
      (q', wbs) = go q ts
    in (q', WTuple wbs)
    
  BList ivar i typ -> undefined


  err -> error $ "[freshLabels] requested: "++ show err

freshBoxLabels :: BundleType -> (LabelContext, WireBundle)
freshBoxLabels t = freshlabels t emptyContext
