{-# LANGUAGE InstanceSigs #-}

module Circuit.Bundle where

import Circuit.Type (QuantumOperation (..), WireType (..), basename)
import PQ.Index (IVarId, Index (..))
import Analyzer.Unify
  ( HasIndex (..),
    IndexSubstitution,
    fresh,
    isubCodomain,
    isubDomain,
    isubSingleton
  )
import PQ.Type (Type (..))
import Eval.Index (evalIndexNoHandle)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
  ( elems,
    empty,
    findWithDefault,
    fromList,
    insert,
    keysSet,
    notMember,
    toList,
    union,
    withoutKeys
  )
import qualified Data.HashSet as HSet (HashSet)
import Data.Map.Strict (Map)
import Debug.Trace (trace)

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

---------------------------

maybeTypeToBundleType :: Maybe Type -> Maybe BundleType
maybeTypeToBundleType Nothing  = Nothing
maybeTypeToBundleType (Just typ) = Just $ typeToBundleType typ

typeToBundleType :: Type -> BundleType
typeToBundleType TUnit = BUnit
typeToBundleType (TWire wt i) = BWire wt
typeToBundleType (TTensor typs) = BTensor $ map typeToBundleType typs
typeToBundleType (TCirc i typ1 typ2) = typeToBundleType typ1
typeToBundleType (TArrow typ1 typ2 i j) = trace("[TArrow] "++show typ1++", "++show typ2++", "++show i++", "++show j)$undefined
typeToBundleType (TBang i typ) = typeToBundleType typ
typeToBundleType (TList ivar i typ) = BList ivar i $ typeToBundleType typ
typeToBundleType (TVar tvar) = trace("")$undefined
typeToBundleType (TIForall ivar typ i j) = trace("")$undefined

maybeBundleTypeToType :: Maybe BundleType -> Maybe Type
maybeBundleTypeToType Nothing = Nothing
maybeBundleTypeToType (Just btyp) = Just $ bundleTypeToType btyp

bundleTypeToType :: BundleType -> Type
bundleTypeToType BUnit = TUnit
bundleTypeToType (BWire wt) = TWire wt Nothing
bundleTypeToType (BTensor btyps) = TTensor $ map bundleTypeToType btyps
bundleTypeToType (BList ivar i btyp) = TList ivar i $ bundleTypeToType btyp

namesInBundle :: WireBundle -> Set.Set String
namesInBundle WUnit = Set.empty
namesInBundle (WLab label) = Set.singleton label
namesInBundle (WTuple ws) = Set.unions (map namesInBundle ws)
namesInBundle (WNil _) = Set.empty
namesInBundle (WCons w ws) = namesInBundle w `Set.union` namesInBundle ws

orderedNamesInBundle :: WireBundle -> [String]
orderedNamesInBundle WUnit = []
orderedNamesInBundle (WLab label) = [label]
orderedNamesInBundle (WTuple ws) = concatMap orderedNamesInBundle ws
orderedNamesInBundle (WNil _) = []
orderedNamesInBundle (WCons w ws) = orderedNamesInBundle w ++ orderedNamesInBundle ws

suffixWBNames :: String -> WireBundle -> WireBundle
suffixWBNames _ WUnit = WUnit
suffixWBNames s (WLab label) = WLab $ label ++ s
suffixWBNames s (WTuple ws) = WTuple $ map (suffixWBNames s) ws
suffixWBNames _ (WNil btyp) = WNil btyp
suffixWBNames s (WCons w ws) = WCons (suffixWBNames s w) (suffixWBNames s ws)

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
outTypeQuantOP (MCNot n) = -- placeholder
  BTensor [BList "i" (Number n) (BWire Qubit), BWire Qubit]

instance HasIndex BundleType where
  iv :: BundleType -> HSet.HashSet IVarId
  iv _ = undefined
  ifv :: BundleType -> HSet.HashSet IVarId
  ifv _ = undefined
  isub :: IndexSubstitution -> BundleType -> BundleType
  isub _ BUnit = BUnit
  isub _ (BWire wt) = (BWire wt)
  isub sub (BTensor btyps) = BTensor (map (isub sub) btyps)
  isub sub (BList id j typ) =
    let id' = fresh (fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub)) [typ]
        renaming = isubSingleton id (IVar id')  
        in BList id' (isub sub . isub renaming $ j) (isub sub . isub renaming $ typ)

-- Label Context 

type LabelContext = Map Label WireType -- Q

emptyContext :: LabelContext
emptyContext = Map.empty

mkContext :: [(Label, WireType)] -> LabelContext
mkContext = Map.fromList 

mergeContexts :: LabelContext -> LabelContext -> LabelContext
mergeContexts = Map.union

updateContext :: LabelContext -> Label -> WireType -> LabelContext
updateContext ctx label wtype = Map.insert label wtype ctx

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
    
  BList i length btyp -> case evalIndexNoHandle length of
    Number n
      | n==0 -> (emptyContext, WNil (Just btyp))
      | otherwise -> 
        let
          (q', wb1) = freshlabels (BList i (Number $ n-1) btyp) q
          (q'', wb2) = freshlabels (isub (isubSingleton i (Number $ n-1)) btyp) $ mergeContexts q q'
        in-- trace("[freshlabels]\nt: "++show t++"\nq: "++show q++"\nq': "++show q'++"\nq'': "++show q'')$
          (q'', WCons wb1 wb2)
        
        --   WCons (freshlabels (BList i (Number $ n-1) btyp)) (freshlabels $ isub (isubSingleton i (Number $ n-1)) btyp)
-- freshlabels (BList i length bt) = WCons (freshlabels (BList i length-1 bt)) (freshlabels (bt{length-1/i})) 
    err -> error $ "[freshLabels] BList index did not evaluate to a number, got: " ++ show err

  err -> error $ "[freshLabels] requested: "++ show err

freshBoxLabels :: BundleType -> (LabelContext, WireBundle)
freshBoxLabels t = freshlabels t emptyContext

mkConsTyped :: Maybe BundleType -> [WireBundle] -> WireBundle
mkConsTyped Nothing = foldl WCons (WNil Nothing)
mkConsTyped (Just btyp) = foldl WCons (WNil $ Just btyp)

-- renaming

type Renaming = Map String String

-- same version but uses the label context to extract the type of the label 
-- and uses it as a base name for the label instead of appending '
-- FIXME this is so wrong whenever the same name appears
createRenamingWithLC :: LabelContext -> LabelContext -> (WireBundle, WireBundle) -> Renaming
createRenamingWithLC old avoid (circWB, boxIn) =
    let
        -- Step 1: map boxIn -> circWB labels
        circLabels = orderedNamesInBundle circWB
        boxLabels  = orderedNamesInBundle boxIn
        boxMapping = Map.fromList (zip boxLabels circLabels)

        -- Step 2: remove boxIn labels from old
        old' = Map.withoutKeys old $ Set.fromList boxLabels

        -- Step 3: build set of names to avoid initially
        initialAvoid = Set.unions
            [ Map.keysSet avoid
            , Set.fromList circLabels
            , Set.fromList (Map.elems boxMapping)
            ]

        -- Step 4: fold over old' to generate fresh names incrementally
        (freshMapping, _) = foldl
            (\(m, used) (name, typ) ->
                let newName = freshName typ used
                in (Map.insert name newName m, Set.insert newName used)
            )
            (Map.empty, initialAvoid)
            (Map.toList old')
    in
        boxMapping `Map.union` freshMapping
  where
    freshName :: WireType -> Set.Set String -> String
    freshName wt usedSet =
        let base = basename wt
            names = [base : show n | n <- [0..]]
        in head $ filter (`Set.notMember` usedSet) names
        
renameBundle :: Renaming -> WireBundle -> WireBundle
renameBundle _ WUnit = WUnit
-- the default is not needed in the use case, but the general function might need it
renameBundle rn (WLab label) = WLab (Map.findWithDefault label label rn) 
renameBundle rn (WTuple ws) = WTuple (map (renameBundle rn) ws)
renameBundle _ (WNil t) = WNil t
renameBundle rn (WCons w ws) = WCons (renameBundle rn w) (renameBundle rn ws)

renameLabelContext :: Renaming -> LabelContext -> LabelContext
renameLabelContext rn ctx =
  Map.fromList
    [ (Map.findWithDefault label label rn, wt)
    | (label, wt) <- Map.toList ctx
    ]
