{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Circuit where
-- module Circuit (WireType(..), QuantumOperation(..), Circuit(..)) where

import PrettyPrinter

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (intercalate)
import qualified Data.Set as Set
import Debug.Trace (trace)


data WireType = Bit | Qubit deriving (Show, Eq)
instance Pretty WireType where
  pretty Bit = "Bit"
  pretty Qubit = "Qubit"
basename :: WireType -> Char
basename Bit = 'b'
basename Qubit = 'q'

data QuantumOperation =
  -- Qubit metaoperations
  QInit Bool
  | QDiscard
  | Meas
  -- Bit metaoperations
  | CInit Bool
  | CDiscard
  -- Single qubit gates
  | Hadamard
  | PauliX
  | PauliY
  | PauliZ
  | T
  | R Int
  | Rinv Int
  -- Two qubit gates
  | CNot
  | CZ
  | CR Int
  | CRinv Int
  -- Classically controlled gates
  | CCNot
  | CCZ
  -- Three qubit gates
  | Toffoli
  deriving (Show, Eq)


instance Pretty QuantumOperation where
  pretty (QInit b) = "QInit" ++ if b then "1" else "0"  
  pretty QDiscard = "QDiscard"
  pretty (CInit b) = "CInit" ++ if b then "1" else "0"
  pretty CDiscard = "CDiscard"
  pretty Meas = "Meas"
  pretty Hadamard = "Hadamard"
  pretty PauliX = "PauliX"
  pretty PauliY = "PauliY"
  pretty PauliZ = "PauliZ"
  pretty T = "T"
  pretty (R n) = "R" ++ show n
  pretty (Rinv n) = "R^-1" ++ show n
  pretty CNot = "CNot"
  pretty CZ = "CZ"
  pretty (CR n) = "CR" ++ show n
  pretty (CRinv n) = "CR^-1" ++ show n
  pretty CCNot = "CCNot"
  pretty CCZ = "CCZ"
  pretty Toffoli = "Toffoli"


-- Bundles Datatype

type Label = String

data BundleType =
    BUnit
  | BWire WireType
  | BTensor [BundleType]
  -- | BList IVarId Index BundleType
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


typeOfBundle :: WireBundle -> BundleType
typeOfBundle WUnit = BUnit
typeOfBundle _ = undefined

typeOfQuantOP :: QuantumOperation -> BundleType
typeOfQuantOP (QInit _) = BWire Qubit
typeOfQuantOP (QDiscard) = BUnit
typeOfQuantOP (Meas) = BWire Bit
typeOfQuantOP (CInit _) = BWire Bit
typeOfQuantOP (CDiscard) = BUnit
typeOfQuantOP (Hadamard) = BWire Qubit
typeOfQuantOP (PauliX) = BWire Qubit
typeOfQuantOP (PauliY) = BWire Qubit
typeOfQuantOP (PauliZ) = BWire Qubit
typeOfQuantOP (T) = BWire Qubit
typeOfQuantOP (R _) = BWire Qubit
typeOfQuantOP (Rinv _) = BWire Qubit
typeOfQuantOP (CNot) = BTensor [BWire Qubit, BWire Qubit]
typeOfQuantOP (CZ) = BTensor [BWire Qubit, BWire Qubit]
typeOfQuantOP (CR _) = BTensor [BWire Qubit, BWire Qubit]
typeOfQuantOP (CRinv _) = BTensor [BWire Qubit, BWire Qubit]
typeOfQuantOP (CCNot) = BTensor [BWire Bit, BWire Qubit]
typeOfQuantOP (CCZ) = BTensor [BWire Bit, BWire Qubit]
typeOfQuantOP (Toffoli) = BTensor [BWire Qubit, BWire Qubit, BWire Qubit]

-- Label Context 

type LabelContext = Map Label WireType -- Q

emptyContext :: LabelContext
emptyContext = Map.empty

insert :: LabelContext -> (Label, WireType) -> LabelContext
insert q (l, t) = Map.insert l t q

freshlabels :: BundleType -> LabelContext -> (LabelContext, WireBundle)
freshlabels t q = case t of
  BUnit -> (q, WUnit)
  
  BWire wt -> 
    let
      base = basename wt
      names = [base : show n | n <- [(1::Int)..]]
      -- look in the map and pick the first name{x} available
      name = head $ filter (`Map.notMember` q) names
      q' = insert q (name, wt)
    in (q', WLab name)

  _ -> undefined

-- Circuit Datatype

data Circuit = -- This corresponds to CRL expressions in the original paper
    Id LabelContext
  | CCons Circuit QuantumOperation WireBundle WireBundle
  deriving (Eq, Show)

mkIdCircuit :: [(Label, WireType)] -> Circuit
mkIdCircuit pairs = Id (Map.fromList pairs)

getContext :: Circuit -> LabelContext
getContext (Id q) = q
getContext (CCons circ _ _ _ ) = getContext circ

updateContext :: Circuit -> LabelContext -> Circuit
updateContext (Id _) q = Id q
updateContext (CCons circ op ins outs) q = CCons (updateContext circ q) op ins outs

instance Pretty WireBundle where
  pretty bundle = case bundle of
    WUnit -> "*"
    WLab l -> l
    WTuple t -> "(" ++ intercalate ", " (map pretty t) ++ ")"
    WNil _ -> "()" -- ?
    WCons e1 e2 -> "(" ++ pretty e1 ++ ":" ++ pretty e2 ++ ")"

instance Pretty LabelContext where
  pretty ctx =
    let pairs = Map.toList ctx
        prettyPair (l, t) = l ++ ":" ++ pretty t
    in intercalate ", " (map prettyPair pairs)

instance Pretty Circuit where
  pretty circ = 
    let 
      circLines = linesOf circ
      circId = head circLines
      circOps = concatLines (drop 1 circLines)
    in circId ++ "\n> Operations:\n" ++ circOps
    where
      linesOf :: Circuit -> [String]
      linesOf (Id ctx) = ["> Label Context: " ++ pretty ctx]
      linesOf (CCons c op ins outs) =
        linesOf c ++ [pretty op ++ " (" ++ pretty ins ++ ") -> " ++ pretty outs]

      concatLines :: [String] -> String
      concatLines []     = ""
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

namesInBox :: (WireBundle, Circuit, WireBundle) -> Set.Set String -- string bcs there are labels and variableIds
namesInBox (ins, circ, outs) = 
  Set.unions [namesInBundle ins, namesInCircuit circ, namesInBundle outs]

circConcat :: Circuit -> Circuit -> Circuit
circConcat c (Id _) = c
circConcat c (CCons d g l k) = CCons (circConcat c d) g l k

-- renaming

type Renaming = Map String String

createRenaming :: Set.Set String -> Set.Set String -> Renaming
createRenaming old avoid =
    Map.fromList [ (name, freshName name avoid) | name <- Set.toList old ]
  where
    freshName n avoidSet
      | n `Set.notMember` avoidSet = n
      | otherwise = freshName (n ++ "'") avoidSet

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

renameCircuit :: Renaming -> Circuit -> Circuit
renameCircuit rn (Id ctx) =
  Id (renameLabelContext rn ctx)
renameCircuit rn (CCons c op ins outs) =
  CCons (renameCircuit rn c) op (renameBundle rn ins) (renameBundle rn outs)

updateBoxNames :: Renaming -> (WireBundle, Circuit, WireBundle) -> (WireBundle, Circuit, WireBundle)
updateBoxNames rn (ins, circ, outs) = (renameBundle rn ins, renameCircuit rn circ, renameBundle rn outs)
