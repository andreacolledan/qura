{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Circuit where

import PrettyPrinter
import Circuit.Type
import Circuit.Bundle
import Analyzer.Unify

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (intercalate)
import qualified Data.Set as Set
import Debug.Trace (trace)


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

-- renaming

type Renaming = Map String String

-- | Create a renaming for labels:
--   1. Map labels in boxIn to labels in circWB.
--   2. Remove these labels from old.
--   3. Rename remaining labels in old so they avoid conflicts.
createRenaming :: Set.Set String -> Set.Set String -> (WireBundle, WireBundle) -> Renaming
createRenaming old avoid (circWB, boxIn) =
    let -- create mapping from boxIn -> circWB labels
        circLabels = Set.toList (namesInBundle circWB)
        boxLabels  = Set.toList (namesInBundle boxIn)
        boxMapping = Map.fromList (zip boxLabels circLabels)
        -- remove boxIn labels from old
        old' = old `Set.difference` Set.fromList boxLabels
        -- fresh renaming for remaining labels
        freshMapping = Map.fromList
            [ (name, freshName name allAvoid)
            | name <- Set.toList old'
            ]
          where
            -- avoid conflicts with circWB labels, boxIn targets, AND initial avoid set
            allAvoid = avoid `Set.union` Set.fromList circLabels
    in boxMapping `Map.union` freshMapping
  where
    freshName :: String -> Set.Set String -> String
    freshName n avoidSet
      | n `Set.notMember` avoidSet = n
      | otherwise = freshName (n ++ "'") avoidSet
-- same version but uses the label context to extract the type of the label 
-- and uses it as a base name for the label instead of appending '
createRenamingLC :: LabelContext -> LabelContext -> (WireBundle, WireBundle) -> Renaming
createRenamingLC old avoid (circWB, boxIn) =
    let
        -- Step 1: map boxIn -> circWB labels
        circLabels = Set.toList (namesInBundle circWB)
        boxLabels  = Set.toList (namesInBundle boxIn)
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

renameCircuit :: Renaming -> Circuit -> Circuit
renameCircuit rn (Id ctx) =
  Id (renameLabelContext rn ctx)
renameCircuit rn (CCons c op ins outs) =
  CCons (renameCircuit rn c) op (renameBundle rn ins) (renameBundle rn outs)

updateBoxNames :: Renaming -> (WireBundle, Circuit, WireBundle) -> (WireBundle, Circuit, WireBundle)
updateBoxNames rn (ins, circ, outs) = (renameBundle rn ins, renameCircuit rn circ, renameBundle rn outs)

-- instance HasIndex BundleType where
--   iv :: BundleType -> HSet.HashSet IVarId
--   iv _ = undefined
--   ifv :: BundleType -> HSet.HashSet IVarId
--   ifv _ = undefined
--   isub :: IndexSubstitution -> BundleType -> BundleType
--   isub _ = undefined