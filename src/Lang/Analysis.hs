module Lang.Analysis (
  runAnalysis,
  --re-exports
  TypeError,
  Type
) where
import Lang.Analysis.Derivation
import Lang.Module.AST
import Lang.Expr.AST
import Lang.Type.AST
import Index.AST
import Lang.Analysis.InferBaseType (inferBaseType)
import Lang.Analysis.InferRefinedType (inferRefinedType)
import Solving.CVC5
import Index.Semantics.Global.Resource
import Index.Semantics.Local.Resource
import Control.Monad.Except

analyzeExpression :: Expr -> TypeDerivation (Type, Maybe Index)
analyzeExpression e = do
  (e', _, _) <- withEnvironmentRollback $ inferBaseType e
  inferRefinedType e'

analyzeTopLevelDefinition :: TopLevelDefinition -> TypeDerivation (VariableId, Type)
analyzeTopLevelDefinition (id, msig, e) = do
  (typ, i) <- withNonLinearContext $ analyzeExpression e
  ftyp <- runSimplifyType $ TBang i typ
  case msig of
    Just sigtyp -> do
      unlessSubtype ftyp sigtyp $ throwLocalError $ UnexpectedType e sigtyp ftyp
      return (id, sigtyp)
    Nothing -> return (id, ftyp)

analyzeModule :: Module -> TypeDerivation [(VariableId, Type)]
analyzeModule mod = go (tldefs mod)
  where
    go [] = return []
    go (tldef : rtldefs) = do
      (fid, fsig) <- analyzeTopLevelDefinition tldef
      rbindings <- withBoundVariables [fid] [fsig] (go rtldefs)
      return $ (fid, fsig) : rbindings

withLibraries :: [Module] -> TypeDerivation a -> TypeDerivation a
withLibraries [] der = der
withLibraries (lib : rlibs) der = do
  libraryBindings <- analyzeModule lib
  uncurry withBoundVariables (unzip libraryBindings) $ withLibraries rlibs der

runAnalysis :: Module
  -> [Module]
  -> SolverHandle
  -> Maybe GlobalMetricModule
  -> Maybe LocalMetricModule
  -> IO (Either TypeError [(VariableId, Type)])
runAnalysis mod libs sh mgmm mlmm = runExceptT $ evalTypeDerivation (withLibraries libs $ analyzeModule mod) (emptyEnv sh mgmm mlmm)