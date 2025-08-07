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


-- ghci commands
-- import System.Environment (withArgs)
-- withArgs ["examples/dumbNot.pq"] main

-- | @runInterpreter mod libs@ interprets module @mod@, with libraries @libs@.
-- Returns either a runtime error, or a configuration of a circuit object and a value.
runInterpreter :: Module -> [Module] -> Either RuntimeError Configuration
runInterpreter mod libs = do
  term <- mergeModLibs mod libs
  circuit <- Right CTodo -- TODO
  evalConfiguration (Config circuit term)

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

mergeModLibs :: Module -> [Module] -> Either RuntimeError Expr
mergeModLibs (Module n e i defs) libs = do
  -- for now I assume no dependencies inside the libraries,
  -- and of course no cross dependencies between the libraries.

  -- take the last element of the tldefs and use it as 'start'
  -- convert the remaining to a map aswell as the libs.
  -- From this 'start', substitute with the maps, being careful with the
  -- var names
  let (remaining, start) = splitTLDEFs defs
  let definitionsMap = createMapFromModules ((Module n e i remaining) : libs)

  case start of
    Just (TopLevelDefinition startId startArgs startSign sartDef) -> 
      -- let s="dumbNot" in
      trace ( ""
        ++"---- start:\n"++(show (TopLevelDefinition startId startArgs startSign sartDef))
        ++"\n---- def map:\n"++(show definitionsMap)
      --   -- ++"-- start:\n"++(prettyTopLevelDefinition (TopLevelDefinition startId startArgs signature sartDef))
      --   -- ++"\n"++(pretty definitionsMap)
      --   -- ++"\n-- example "++s++" search:\n"++show (searchDefinition definitionsMap s)
        ) $ 
    -- substitute in the starting tldef using the maps
      case applyModulesMap definitionsMap sartDef of
        Right expr -> Right $ wrapExpr expr startArgs startSign
        Left err -> Left err

    Nothing -> Left (RuntimeError "No definitions in the input module")
  
applyModulesMap :: ModulesMap -> Expr -> Either RuntimeError Expr
applyModulesMap maps t 
  | M.null maps = Right t -- not really needed but would save some time
  | otherwise = case t of
    EUnit -> Right EUnit

    EVar x -> case searchDefinition maps x of
      Left err -> Left err

      Right (Just (TopLevelDefinition tldefId tldefArgs tldefSign tldefExpr)) -> -- definition found
        do 
          -- also apply the modules to the found def
          tldefExpr' <- applyModulesMap maps tldefExpr
          -- wrap the def with abstractions for its vars
          let tldefExpr'' = wrapExpr tldefExpr' tldefArgs tldefSign
          -- finally, return the lifted function
          Right (ELift tldefExpr'')

      Right Nothing -> Right t -- No definition found, return the term itself

    ETuple exprs -> do
      exprs' <- mapM (\e -> applyModulesMap maps e) exprs
      Right (ETuple exprs')

    EAbs ptrn typ expr -> do
      expr' <- applyModulesMap maps expr
      Right (EAbs ptrn typ expr')

    ELift expr -> do
      expr' <- applyModulesMap maps expr
      Right (ELift expr')

    ENil typ -> Right (ENil typ)

    ECons expr1 expr2 -> do
      expr1' <- applyModulesMap maps expr1
      expr2' <- applyModulesMap maps expr2
      Right (ECons expr1' expr2')

    EFold _ _ _ -> Left (RuntimeError "EFold not supported yet")
    -- EFold expr1 expr2 t3 -> EFold (applyModulesMap maps expr1) (applyModulesMap maps expr2) (applyModulesMap maps t3)

    EApp expr1 expr2 -> do
      expr1' <- applyModulesMap maps expr1
      expr2' <- applyModulesMap maps expr2
      Right (EApp expr1' expr2')

    EApply expr1 expr2 -> do
      expr1' <- applyModulesMap maps expr1
      expr2' <- applyModulesMap maps expr2
      Right (EApply expr1' expr2')

    EBox _ _ -> Left (RuntimeError "EBox not supported yet")

    EForce expr -> do
      expr' <- applyModulesMap maps expr
      Right (EForce expr')

    ELet ptrn expr1 expr2 -> do
      expr1' <- applyModulesMap maps expr1
      expr2' <- applyModulesMap maps expr2
      Right (ELet ptrn expr1' expr2')

    EAnno expr typ -> do
      expr' <- applyModulesMap maps expr
      Right (EAnno expr' typ)

    EIAbs ivar expr -> do
      expr' <- applyModulesMap maps expr
      Right (EIAbs ivar expr')

    EIApp expr i -> do
      expr' <- applyModulesMap maps expr
      Right (EIApp expr' i)

    EConst c -> Right (EConst c)

    EAssume expr typ -> do
      expr' <- applyModulesMap maps expr
      Right (EAssume expr' typ)

-- Raises a run time error in case a definition appears in more than one module
-- and we are unsure about which one to use.
-- For now, I am not checking if the id is in the form module.name because
-- I dont even know if a function can be called like that in .pq
searchDefinition :: ModulesMap -> VariableId -> Either RuntimeError (Maybe TopLevelDefinition)
searchDefinition maps varId =
  -- a list of (mod_name, Expr) represen ting when defs are found
  let foundDefinitions = [(moduleName, tldef) | (moduleName, defsMap) <- M.toList maps, Just tldef <- [M.lookup varId defsMap]]
  in case foundDefinitions of
    [] -> Right Nothing
    [(_, def)] -> Right (Just def)
    defs -> Left $ RuntimeError err
      where
        err = "Multiple definitions found for " ++ varId 
            ++ ".\nIt is defined in the following modules:\n"
            ++ (intercalate ",\n" (map fst defs))

wrapExpr :: Expr -> [Pattern] -> Maybe Type -> Expr
-- I dont know if args can be empty and the type not be Nothing, and viceversa
wrapExpr e [] _= e
wrapExpr e _ Nothing = e
-- Analyze the type along with the patterns -> How do I extract the types corresponding to the patterns?
wrapExpr e (p:ps) _ = EAbs undefined TUnit (wrapExpr e ps undefined) -- TODO