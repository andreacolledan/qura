{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Circuit where
-- module Circuit (WireType(..), QuantumOperation(..), Circuit(..)) where

import PrettyPrinter

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (intercalate)

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


-- Circuit Datatype

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

type LabelContext = Map Label WireType -- Q

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

typeOfBundle :: WireBundle -> BundleType
typeOfBundle WUnit = BUnit
typeOfBundle _ = undefined

typeOfQuantOP :: QuantumOperation -> BundleType
typeOfQuantOP (QInit _) = BWire Qubit
typeOfQuantOP _ = undefined


data Circuit = -- Define circuit buffers. This corresponds to CRL expressions in the original paper
    Id LabelContext
  | CCons Circuit QuantumOperation WireBundle WireBundle
  deriving (Eq, Show) -- do I also want the context in CCons (for easier access)

makeIdCircuit :: [(Label, WireType)] -> Circuit
makeIdCircuit pairs = Id (Map.fromList pairs)

getContext :: Circuit -> LabelContext
getContext (Id q) = q
getContext (CCons circ _ _ _ ) = getContext circ

updateContext :: Circuit -> LabelContext -> Circuit
updateContext (Id _) q = Id q
updateContext (CCons circ op ins outs) q = CCons (updateContext circ q) op ins outs

instance Pretty WireBundle where
  pretty bundle = show bundle

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
