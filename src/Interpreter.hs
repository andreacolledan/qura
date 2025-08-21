{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Interpreter (
    runInterpreter,
    Configuration(..),
) where

-- I havent understand yet how to properly import the modules,
-- for now, I am simply importing what I need directly
import Interpreter.RuntimeError
import Interpreter.Configuration
import PQ (Module)
import PQ.Module
import PQ.Expr
import PQ.Type
import Circuit
import PrettyPrinter (Pretty (..))

import Debug.Trace (trace)
import qualified Data.Map as M
import Data.List (intercalate)

-- | @runInterpreter mod libs@ interprets module @mod@, with libraries @libs@.
-- Returns either a runtime error, or a configuration of a circuit object and a value.
runInterpreter :: Module -> [Module] -> Either RuntimeError Configuration
runInterpreter mod libs = do
  (term, circuit) <- mergeModLibs mod libs
  startConfigEvaluation (Config circuit term)

-- this is a double map for future reasons, maybe two libs uses a same names
-- for the modules, and we can distinct them with module.function (?).
-- For now I search the term in all the modules, if it appears in more than
-- one, I throw an error
type ModulesMap = M.Map String (M.Map VariableId TopLevelDefinition)

instance Pretty ModulesMap where
  pretty modulesMap =
    unlines $ map prettyModule $ M.toList modulesMap
    where
      prettyModule (moduleName, defs) =
        "-- Module: " ++ moduleName ++ "\n"
        ++ unlines (map (\(_,a)-> prettyTopLevelDefinition a) (M.toList defs))

createMapFromModules :: [Module] -> ModulesMap
createMapFromModules modules =
  M.fromList [(name m, buildDefMap (tldefs m)) | m <- modules]
  where
    buildDefMap :: [TopLevelDefinition] -> M.Map VariableId TopLevelDefinition
    buildDefMap defs = M.fromList [(id', d) | d@(TopLevelDefinition id' _ _ _) <- defs]

splitTLDEFs :: [TopLevelDefinition] -> ([TopLevelDefinition], Maybe TopLevelDefinition)
splitTLDEFs [] = ([], Nothing)
splitTLDEFs [x] = ([], Just x)
splitTLDEFs xs = (init xs, Just (last xs))

-- idCircuitFromArgs :: ([Pattern], Maybe Type) -> Circuit
-- idCircuitFromArgs _ = mkIdCircuit []
-- -- idCircuitFromArgs :: TopLevelDefinition -> LabelContext
-- -- idCircuitFromArgs (TopLevelDefinition _ a s _) = 
-- --   let ctx = pairArgPattern a s
-- --   in mkIdCircuit ctx
-- --     where
-- --       pairArgPattern :: [Pattern] -> Maybe Type -> [(Label, WireType)]
-- --       pairArgPattern [] _ = []
-- --       pairArgPattern _ Nothing = []
-- --       pairArgPattern (p:ps) (Just typ) = case typ of
-- --         TUnit -> []
-- --         TWire typ _ -> 
-- --           let PVar name = p
-- --           in [(name ,typ)]
-- --         -- TTensor _ -> EAbs p typ 
-- --         -- TCirc _ _ _ -> undefined
-- --         -- TArrow typ1 _ _ _ -> pairArgPattern  (p:ps) (Just typ1) -- only expand on the ifrst argument of TArrow
-- --         -- TBang _ typ -> pairArgPattern  (p:ps) (Just typ) -- remove the TBang
-- --         -- TList _ _ _ -> EAbs p typ 
-- --         -- TVar _ -> undefined
-- --         -- TIForall ivarid typ' _ _ -> EIAbs ivarid (pairArgPattern  ps (Just typ'))

mergeModLibs :: Module -> [Module] -> Either RuntimeError (Expr, Circuit)
mergeModLibs (Module programName e i defs) libs = do
  -- for now I assume no dependencies inside the libraries,
  -- and of course no cross dependencies between the libraries.

  -- take the last element of the tldefs and use it as 'start'
  -- convert the remaining to a map aswell as the libs.
  -- From this 'start', substitute with the maps, being careful with the
  -- var names
  let (remaining, start) = splitTLDEFs defs -- or maybe we set that only the main is exectued
  let definitionsMap = createMapFromModules ((Module programName e i remaining) : libs)
  
  case start of
    Just (TopLevelDefinition startId startArgs startSign sartDef) -> do
      -- trace ( ""
        -- ++"---- start:\n"++(show (TopLevelDefinition startId startArgs startSign sartDef))
        -- ++"-- start:\n"++(prettyTopLevelDefinition (TopLevelDefinition startId startArgs signature sartDef))
        -- ++"\n---- def map:\n"++(show definitionsMap)
        -- ++"\n"++(pretty definitionsMap)
        -- ) $ 
      if startId /= "main"
        then Left $ RuntimeError "No main function found in the file"
        else do

          -- substitute in the starting tldef using the maps
            fullExpr <- applyModulesMap definitionsMap (programName, sartDef)

            -- also wrap the start
            let wrappedStart = fullExpr--wrapExpr fullExpr startArgs startSign
            let initialCircuit = mkIdCircuit [] -- start is always identity
            -- let initialCircuit = idCircuitFromArgs (startArgs, startSign) -- start is always identity
            Right (wrappedStart, initialCircuit)

    Nothing -> Left $ RuntimeError "No definitions in the input module."
  
-- given the main expression, sub in variables taken from the modules, using their name
applyModulesMap :: ModulesMap -> (String, Expr) -> Either RuntimeError Expr
applyModulesMap maps (progName, startDef) 
  | M.null maps = Right startDef -- not really needed but would save some time
  | otherwise = case startDef of
    EUnit -> Right EUnit

    EVar x -> case searchDefinition maps progName x of
      Left err -> Left err

      Right (Just (sourceModule, (TopLevelDefinition _ tldefArgs tldefSign tldefExpr))) -> -- definition found
        do 
          -- Since that we are assuming no dependencies in the lib, for now we only recurse
          -- if the definition was found in the user program (so the module progName)
          tldefExpr' <- if sourceModule == progName
            then applyModulesMap maps (progName, tldefExpr)
            else pure tldefExpr

          -- wrap the definition with abstractions for its vars
          let tldefExpr'' = wrapExpr tldefExpr' tldefArgs tldefSign
          -- trace (""
          --   ++"\nWrapping:\n> "++pretty tldefExpr'
          --   ++"\nwith args:\n> "++show tldefArgs
          --   ++"\nand with signature:\n> "++pretty tldefSign
          --   ++"\nWrapping output:\n> "++(pretty tldefExpr''))$ 

          -- finally, return the lifted function
          Right $ ELift tldefExpr''

      Right Nothing -> Right startDef -- No definition found, return the term itself

    ELab _ -> Right startDef

    ETuple es -> do
      es' <- mapM (\e -> applyModulesMap maps (progName, e)) es
      Right $ ETuple es'

    EAbs ptrn typ e -> do
      e' <- applyModulesMap maps (progName, e)
      Right $ EAbs ptrn typ e'

    ECirc _ _ _ -> Right startDef

    ELift e -> do
      e' <- applyModulesMap maps (progName, e)
      Right $ ELift e'

    ENil typ -> Right $ ENil typ

    ECons e1 e2 -> do
      e1' <- applyModulesMap maps (progName, e1)
      e2' <- applyModulesMap maps (progName, e2)
      Right $ ECons e1' e2'

    EFold e1 e2 e3 -> do
      e1' <- applyModulesMap maps (progName, e1)
      e2' <- applyModulesMap maps (progName, e2)
      e3' <- applyModulesMap maps (progName, e3)
      Right $ EFold e1' e2' e3'

    EApp e1 e2 -> do
      e1' <- applyModulesMap maps (progName, e1)
      e2' <- applyModulesMap maps (progName, e2)
      Right $ EApp e1' e2'

    EApply e1 e2 -> do
      e1' <- applyModulesMap maps (progName, e1)
      e2' <- applyModulesMap maps (progName, e2)
      Right $ EApply e1' e2'

    EBox _ _ -> Left $ RuntimeError "EBox not supported yet"

    EForce e -> do
      e' <- applyModulesMap maps (progName, e)
      Right $ EForce e'

    ELet ptrn e1 e2 -> do
      e1' <- applyModulesMap maps (progName, e1)
      e2' <- applyModulesMap maps (progName, e2)
      Right $ ELet ptrn e1' e2'

    EAnno e typ -> do
      e' <- applyModulesMap maps (progName, e)
      Right $ EAnno e' typ

    EIAbs ivar e -> do
      e' <- applyModulesMap maps (progName, e)
      Right $ EIAbs ivar e'

    EIApp e i -> do
      e' <- applyModulesMap maps (progName, e)
      Right $ EIApp e' i

    EConst c -> Right $ EConst c

    EAssume e typ -> do
      e' <- applyModulesMap maps (progName, e)
      Right $ EAssume e' typ

-- Raises a run time error in case a definition appears in more than one module
-- and we are unsure about which one to use.
-- For now, I am not checking if the id is in the form module.name because
-- I dont even know if a function can be called like that in .pq
searchDefinition :: ModulesMap -> String -> VariableId -> Either RuntimeError (Maybe (String, TopLevelDefinition))
-- modName is used to default to the user defined definition.
-- defName is the name of the definition in which we are trying to substitute in.
-- x is the name of the definition that we are looking for
searchDefinition maps userModName x =
  -- trace ("\nSearching a definition for EVar "++x++", found inside the definition '"++defName++"' in the module '"++userModName++"'.")$
  -- a list of (mod_name, Expr) representing the found definitions
  let foundDefinitions = [(moduleName, tldef) | (moduleName, defsMap) <- M.toList maps, Just tldef <- [M.lookup x defsMap]]
  in case foundDefinitions of
    [] -> 
      -- trace (
      --   "> No definition found for "++x
      -- )$
      Right Nothing
    [(moduleName, tldef)] -> 
      -- trace (
      --   "> FOUND: Using "++x++" from "++moduleName
      -- )$
      Right (Just (moduleName, tldef))
    defs -> -- if more than one definition is found and the name of the variable to look for
            -- is the same as the name if the definition itself, than we have to look for the 
            -- 'true' definition on another module, otherwise we substitute infinitely.
       
      -- remove found definitions if it comes from its module (it's itself)
      case filter (\(moduleName, _) -> moduleName /= userModName) defs of
          [] -> Left $ RuntimeError "A definition is defined using itself recursively."
          -- if we only have one definition left use it,
          [(moduleName, tldef)] -> 
            -- trace (
            --   "> FOUND: Using "++x++" from "++moduleName
            -- )$
            Right (Just (moduleName, tldef))
          -- otherwise try to default to the user-defined one
          filteredDefs -> case lookup userModName filteredDefs of
            Just tldef -> 
              -- trace (
              --   "> FOUND: Using "++x++" from "++userModName
              -- )$
              Right (Just (userModName, tldef))
            -- more than one definition in the libraries. Unsure on which one to use
            Nothing -> Left $ RuntimeError err
              where
                err = "Multiple definitions found for " ++ x 
                    ++ ".\nIt is defined in the following modules:\n"
                    ++ (intercalate ",\n" (map fst defs))


-- wraps an expression with abstraction on his args in order to be able to lift it
wrapExpr :: Expr -> [Pattern] -> Maybe Type -> Expr
wrapExpr e [] _ = e
wrapExpr e _ Nothing = e
wrapExpr e (p:ps) (Just typ) = case typ of
  TUnit -> undefined
  TWire _ _ -> EAbs p typ e
  TTensor _ -> EAbs p typ e
  TCirc _ _ _ -> undefined
  TArrow typ1 _ _ _ -> wrapExpr e (p:ps) (Just typ1) -- only expand on the ifrst argument of TArrow
  TBang _ typ -> wrapExpr e (p:ps) (Just typ) -- remove the TBang
  TList _ _ _ -> EAbs p typ e
  TVar _ -> undefined
  TIForall ivarid typ' _ _ -> EIAbs ivarid (wrapExpr e ps (Just typ'))
