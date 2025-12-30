{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Interpreter (
    runInterpreter,
    Configuration(..),
    InterpreterResult(..),
    QasmProgram(..)
) where

-- I havent understand yet how to properly import the modules,
-- for now, I am simply importing what I need directly
import Interpreter.RuntimeError (RuntimeError (..))
import Interpreter.Configuration (Configuration (..), startConfigEvaluation)
import Interpreter.Qasm (QasmProgram (..), circuitToQasm)
import Interpreter.Metric (ProgramMetrics)
import PQ.Module (Module (..), TopLevelDefinition (..), prettyTopLevelDefinition)
import PQ.Expr
  ( Expr (..),
    Pattern (..),
    VariableId,
    createRenaming,
    renameExpr,
    renamePattern
  )
import PQ.Type (Type (..))
import Circuit (Circuit, getCircuitMetrics, mkIdCircuit)
import PrettyPrinter (Pretty (..))
import Prelude hiding (id)
import Interface (CLArguments (..))

import Debug.Trace (trace)
import qualified Data.Map as Map (Map, elems, fromList, keys, lookup, null, toList)
import qualified Data.Set as Set (Set, fromList)
import Data.List (intercalate)

data InterpreterResult = InterpResult {
  cfg :: Configuration,
  circMetrics :: ProgramMetrics,
  qasm :: QasmProgram
  -- maybe other languages
} deriving Show

-- | @runInterpreter mod libs@ interprets module @mod@, with libraries @libs@.
-- Returns either a runtime error, or a configuration of a circuit object and a value.
runInterpreter :: Module -> [Module] -> CLArguments -> Either RuntimeError InterpreterResult
runInterpreter mod libs CommandLineArguments {filepath = fp, qubitRecycling = r} = do
  let mod' = mod {name = fp}
  (term, circ) <- mergeModLibs mod' libs
  config <- startConfigEvaluation (Config circ term)
  let metrics = getCircuitMetrics r $ circuit config 
  let qasmProg = circuitToQasm (circuit config) (CommandLineArguments {filepath = fp, qubitRecycling = r}) -- once we have the string we could save it to file
  -- saveProgram qasmProg -- maybe
  Right $ InterpResult config metrics qasmProg

-- this is a double map for future reasons, maybe two libs use the same names
-- for the modules, and we can distinct them with module.function (?).
-- For now I search the term in all the modules, if it appears in more than
-- one, I throw an error
type ModulesMap = Map.Map String (Map.Map VariableId TopLevelDefinition)

instance Pretty ModulesMap where
  pretty modulesMap =
    unlines $ map prettyModule $ Map.toList modulesMap
    where
      prettyModule (moduleName, defs) =
        "-- Module: " ++ moduleName ++ "\n"
        ++ unlines (map (\(_,a)-> prettyTopLevelDefinition a) (Map.toList defs))

createMapFromModules :: [Module] -> ModulesMap
createMapFromModules modules =
  Map.fromList [(name m, buildDefMap (tldefs m)) | m <- modules]
  where
    buildDefMap :: [TopLevelDefinition] -> Map.Map VariableId TopLevelDefinition
    buildDefMap defs = Map.fromList [(id', d) | d@(TopLevelDefinition id' _ _ _) <- defs]

-- extract the top level definition from a module and return it and the remainaing definitions
extractDefFromModule :: VariableId -> [TopLevelDefinition] -> Either RuntimeError (TopLevelDefinition, [TopLevelDefinition])
extractDefFromModule vid defs =
  case break ((== vid) . id) defs of
      (before, def:after) ->
          Right (def, before ++ after)
      _ ->
          Left $ RuntimeError $
              "Definition not found: " ++ show vid

mergeModLibs :: Module -> [Module] -> Either RuntimeError (Expr, Circuit)
mergeModLibs (Module programName e i defs) libs = do
  -- for now I assume no dependencies inside the libraries, (a function of the lib uses another one from the same lib)
  -- and of course no cross dependencies between the libraries.
  (main, otherDefs) <- extractDefFromModule "main" defs -- TODO extract the main
  
  -- TODO check that the main has no args
  
  let definitionsMap = createMapFromModules ((Module programName e i otherDefs) : libs)
  let (TopLevelDefinition mainId mainArgs mainSign sartDef) = main
  -- substitute in the maining tldef using the maps
  completeProgramExpr <- 
    trace ( ""
      -- ++"---- main:\n"++(show (TopLevelDefinition mainId mainArgs mainSign sartDef))
      -- ++"-- main:\n"++(prettyTopLevelDefinition (TopLevelDefinition mainId mainArgs signature sartDef))
      -- ++"\n---- def map:\n"++(show definitionsMap)
      -- ++"\n"++(pretty definitionsMap)
    ) $ 
      applyModulesMap definitionsMap programName mainId sartDef
  
  -- also wrap the main
  let initialCircuit = mkIdCircuit [] -- starting label context is always empty
  -- let initialCircuit = idCircuitFromArgs (mainArgs, mainSign) -- main is always identity
  Right (completeProgramExpr, initialCircuit)
  

topLevelDefNames :: ModulesMap -> Set.Set VariableId
topLevelDefNames =
  Set.fromList . concatMap Map.keys . Map.elems

--
applyModulesMap :: ModulesMap -- maps
                -> String -- current module
                -> VariableId -- current definition
                -> Expr -- definition body
                -> Either RuntimeError Expr
applyModulesMap maps currMod currDef expr
  | Map.null maps = Right expr -- not really needed but would save some time
  | otherwise = case expr of
    EUnit -> Right EUnit

    EVar x -> do
      -- search the definition
      searchResult <- searchDefinition maps currMod currDef x
      case searchResult of 
        Just (foundDefMod, foundTldef) -> do
          -- check if something needs to be subbed inside it. We remove the current module to avoid loops
          -- let maps' = Map.delete currMod maps
          let (TopLevelDefinition foundName foundArgs foundSign foundExpr) = foundTldef
          newExpr <- applyModulesMap maps foundDefMod foundName foundExpr
          -- wrap the definition with abstractions for its vars
          let newExpr' = wrapExpr newExpr foundArgs foundSign

          -- finally, return the lifted function
          Right $ ELift newExpr'
        
        Nothing -> Right expr

    ELab _ -> Right expr

    ETuple es -> do
      es' <- mapM (applyModulesMap maps currMod currDef) es
      Right $ ETuple es'

    EAbs ptrn typ e -> do -- TODO handle when the name of the pattern already exist in the modules (check the let i guess)
      e' <- applyModulesMap maps currMod currDef e
      Right $ EAbs ptrn typ e'

    ECirc _ _ _ -> Right expr

    ELift e -> do
      e' <- applyModulesMap maps currMod currDef e
      Right $ ELift e'

    ENil typ -> Right $ ENil typ

    ECons e1 e2 -> do
      e1' <- applyModulesMap maps currMod currDef e1
      e2' <- applyModulesMap maps currMod currDef e2
      Right $ ECons e1' e2'

    EFold e1 e2 e3 -> do
      e1' <- applyModulesMap maps currMod currDef e1
      e2' <- applyModulesMap maps currMod currDef e2
      e3' <- applyModulesMap maps currMod currDef e3
      Right $ EFold e1' e2' e3'

    EApp e1 e2 -> do
      e1' <- applyModulesMap maps currMod currDef e1
      e2' <- applyModulesMap maps currMod currDef e2
      Right $ EApp e1' e2'

    EApply e1 e2 -> do
      e1' <- applyModulesMap maps currMod currDef e1
      e2' <- applyModulesMap maps currMod currDef e2
      Right $ EApply e1' e2'

    EBox typ e -> do
      e' <- applyModulesMap maps currMod currDef e
      Right $ EBox typ e'

    EForce e -> do
      e' <- applyModulesMap maps currMod currDef e
      Right $ EForce e'

    ELet ptrn e1 e2 -> do -- handle when the name of the pattern already exist in the modules
      -- create a renaming for the pattern such to have a different than the tldefs
      let avoid = topLevelDefNames maps
      let renaming = createRenaming avoid ptrn
      -- rename the pattern
      let ptrn' = renamePattern renaming ptrn
      -- safely apply modules in the first term
      e1' <- applyModulesMap maps currMod currDef e1
      -- rename in the second term
      let e2' = renameExpr renaming e2
      -- apply the definitions in the second term after renaming
      e2'' <- applyModulesMap maps currMod currDef e2'
      Right $ ELet ptrn' e1' e2''

    EAnno e typ -> do
      e' <- applyModulesMap maps currMod currDef e
      Right $ EAnno e' typ

    EIAbs ivar e -> do
      e' <- applyModulesMap maps currMod currDef e
      Right $ EIAbs ivar e'

    EIApp e i -> do
      e' <- applyModulesMap maps currMod currDef e
      Right $ EIApp e' i

    EConst c -> Right $ EConst c

    EAssume e typ -> do
      e' <- applyModulesMap maps currMod currDef e
      Right $ EAssume e' typ

-- Look up a variable’s definition, preferring the source module but avoiding self-recursion
searchDefinition :: ModulesMap
                 -> String -- source module
                 -> String -- source definition
                 -> VariableId -- target definition
                 -> Either RuntimeError (Maybe (String, TopLevelDefinition))
searchDefinition maps sourceMod sourceDef trgtDef =
  --trace ("[SearchDef.] Searching " ++ trgtDef ++ " definition, requested in " ++ sourceMod ++ "." ++ sourceDef) $
  if sourceDef /= trgtDef
    then
      case Map.lookup sourceMod maps >>= Map.lookup trgtDef of
        Just tldef -> --trace(" > Using "++trgtDef++" from "++sourceMod)$
          Right $ Just (sourceMod, tldef)  -- found in source module
        Nothing -> searchInOtherModules  -- not found in source module, continue
    else
      searchInOtherModules  -- same name as sourceDef, skip source module

  where
    searchInOtherModules :: Either RuntimeError (Maybe (String, TopLevelDefinition))
    searchInOtherModules =
      case [ (modName, tldef)
           | (modName, defsMap) <- Map.toList maps
           , modName /= sourceMod
           , Just tldef <- [Map.lookup trgtDef defsMap]
           ] of
        [] -> --trace(" > "++trgtDef++" not found")$
          Right Nothing  -- not found anywhere
        [(modName, tldef)] -> --trace(" > Using "++id tldef++" from "++modName)$
          Right $ Just (modName, tldef)  -- found in exactly one library
        defs -> Left $ RuntimeError err  -- multiple definitions
          where
            err = "Multiple definitions found for " ++ trgtDef
                  ++ ".\nIt is defined in the following modules:\n"
                  ++ intercalate ",\n" (map fst defs)


-- wraps an expression with abstraction on his args in order to be able to lift it
wrapExpr :: Expr -> [Pattern] -> Maybe Type -> Expr
wrapExpr e [] _ = e
wrapExpr e _ Nothing = e
wrapExpr e (p:ps) (Just typ) = case typ of
  TUnit -> undefined
  TWire _ _ -> EAbs p typ e
  TTensor _ -> EAbs p typ e
  TCirc _ typ1 _ -> wrapExpr e (p:ps) (Just typ1) -- TODO check
  TArrow typ1 typ2 _ _ -> EAbs p typ1 $ wrapExpr e ps (Just typ2)
  -- TArrow typ1 _ _ _ -> wrapExpr e (p:ps) (Just typ1) -- only expand on the ifrst argument of TArrow
  TBang _ typ -> wrapExpr e (p:ps) (Just typ) -- remove the TBang
  TList _ _ _ -> EAbs p typ e
  TVar _ -> undefined
  TIForall ivarid typ' _ _ -> EIAbs ivarid (wrapExpr e ps (Just typ'))
