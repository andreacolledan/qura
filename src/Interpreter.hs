{-# LANGUAGE FlexibleInstances #-}

module Interpreter
  ( runInterpreter,
    Configuration (..),
    InterpreterResult (..),
    QasmProgram (..),
  )
where

import Circuit (Circuit, getCircuitMetrics, mkIdCircuit)
import Data.List (intercalate)
import qualified Data.Map as Map (Map, elems, fromList, keys, lookup, null, toList, withoutKeys)
import qualified Data.Set as Set (Set, fromList)
import Interface (CLArguments (..))
import Interpreter.Configuration (Configuration (..), evalConfiguration)
import Interpreter.Metric (ProgramMetrics)
import Interpreter.Qasm (QasmProgram (..), circuitToQasm)
import Interpreter.RuntimeError (RuntimeError (..))
import PQ.Expr
  ( Expr (..),
    Pattern (..),
    VariableId,
    varsInPattern,
  )
import PQ.Module (Module (..), TopLevelDefinition (..), prettyTopLevelDefinition)
import PQ.Type (Type (..))
import PrettyPrinter (Pretty (..))
import Prelude hiding (id)

data InterpreterResult = InterpResult {
  cfg :: Configuration,
  circMetrics :: ProgramMetrics,
  qasm :: QasmProgram
} deriving Show

-- | @runInterpreter mod libs@ interprets module @mod@, with libraries @libs@.
-- Returns either a runtime error, or a configuration of a circuit object and a value.
runInterpreter :: Module -> [Module] -> CLArguments -> Either RuntimeError InterpreterResult
runInterpreter mod libs cmdArgs@CommandLineArguments{filepath = fp, qubitRecycling = r} = do
  let mod' = mod {name = fp}
  (term, circ) <- mergeModLibs mod' libs
  config <- evalConfiguration (Config circ term)
  let metrics = getCircuitMetrics r $ circuit config
  let qasmProg = circuitToQasm (circuit config) cmdArgs
  Right $ InterpResult config metrics qasmProg

-- The semantics of a module is a map from variable names to definitions
type Namespace = Map.Map VariableId TopLevelDefinition

-- The semantics of all the modules that make up a program is a namespace environment,
-- i.e. a mapping from module names to namespaces
type NamespaceEnvironment = Map.Map String Namespace

instance Pretty NamespaceEnvironment where
  pretty env =
    unlines $ map prettyNamespace $ Map.toList env
    where
      prettyNamespace (moduleName, defs) =
        "-- Namespace: " ++ moduleName ++ "\n"
        ++ unlines (map (\(_,a)-> prettyTopLevelDefinition a) (Map.toList defs))

createMapFromModules :: [Module] -> NamespaceEnvironment
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
  (main, otherDefs) <- extractDefFromModule "main" defs

  -- TODO: check that the main has no arguments

  let definitionsMap = createMapFromModules (Module programName e i otherDefs : libs)
  let (TopLevelDefinition mainId mainArgs mainSign sartDef) = main
  -- substitute in the main tldef using the maps
  completeProgramExpr <- applyModulesMap definitionsMap programName mainId sartDef

  let initialCircuit = mkIdCircuit [] -- starting label context is always empty
  Right (completeProgramExpr, initialCircuit)


topLevelDefNames :: NamespaceEnvironment -> Set.Set VariableId
topLevelDefNames = Set.fromList . concatMap Map.keys . Map.elems

--
applyModulesMap :: NamespaceEnvironment -- maps
                -> String     -- current module
                -> VariableId -- current definition
                -> Expr       -- definition body
                -> Either RuntimeError Expr
applyModulesMap maps currMod currDef expr
  | Map.null maps = Right expr -- not really needed but could save some time
  | otherwise = case expr of
    EUnit -> Right EUnit

    EVar x -> do
      -- search the definition
      searchResult <- searchDefinition maps currMod currDef x
      case searchResult of
        Just (foundDefMod, foundTldef) -> do
          -- check if something needs to be subbed inside it. We remove the current module to avoid loops
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

    EAbs ptrn typ e -> do
      let maps' = maps `Map.withoutKeys` varsInPattern ptrn
      e' <- applyModulesMap maps' currMod currDef e
      Right $ EAbs ptrn typ e'

    ECirc {} -> Right expr

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

    ELet ptrn e1 e2 -> do
      let maps' = maps `Map.withoutKeys` varsInPattern ptrn
      e1' <- applyModulesMap maps currMod currDef e1
      e2' <- applyModulesMap maps' currMod currDef e2
      Right $ ELet ptrn e1' e2'

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
searchDefinition :: NamespaceEnvironment
                 -> String -- source module
                 -> String -- source definition
                 -> VariableId -- target definition
                 -> Either RuntimeError (Maybe (String, TopLevelDefinition))
searchDefinition maps sourceMod sourceDef trgtDef =
  if sourceDef /= trgtDef
    then case Map.lookup sourceMod maps >>= Map.lookup trgtDef of
      Just tldef -> Right $ Just (sourceMod, tldef) -- found in source module
      Nothing -> searchInOtherModules -- not found in source module, continue
    else
      searchInOtherModules -- same name as sourceDef, skip source module
  where
    searchInOtherModules :: Either RuntimeError (Maybe (String, TopLevelDefinition))
    searchInOtherModules =
      case [ (modName, tldef)
             | (modName, defsMap) <- Map.toList maps,
               modName /= sourceMod,
               Just tldef <- [Map.lookup trgtDef defsMap]
           ] of
        [] ->
          -- trace(" > "++trgtDef++" not found")$
          Right Nothing -- not found anywhere
        [(modName, tldef)] ->
          -- trace(" > Using "++id tldef++" from "++modName)$
          Right $ Just (modName, tldef) -- found in exactly one library
        defs -> Left $ RuntimeError err -- multiple definitions
          where
            err =
              "Multiple definitions found for "
                ++ trgtDef
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
  TCirc _ typ1 _ -> wrapExpr e (p:ps) (Just typ1) -- TODO: maybe it's simply `e`
  TArrow typ1 typ2 _ _ -> EAbs p typ1 $ wrapExpr e ps (Just typ2)
  TBang _ typ -> wrapExpr e (p:ps) (Just typ)
  TList _ _ _ -> EAbs p typ e
  TVar _ -> undefined
  TIForall ivarid typ' _ _ -> EIAbs ivarid (wrapExpr e ps (Just typ'))
