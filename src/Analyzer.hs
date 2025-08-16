module Analyzer (
  runAnalysis,
  --re-exports
  TypeError,
  Type
) where
import Analyzer.Derivation
import PQ.Module
import PQ.Expr
import PQ.Type
import PQ.Index
import Analyzer.InferBaseType (inferBaseType)
import Analyzer.InferRefinedType (inferRefinedType)
import Solver
import Metric
import Control.Monad.Except
import Control.Monad (unless, join)

-- | Analyze an expression, inferring its type and possibly its effect.
-- Runs both Milner-style inference and refinement synthesis, in sequence.
analyzeExpression :: Expr -> TypeDerivation (Type, Maybe Index)
analyzeExpression e = do
  (e', _, _) <- withEnvironmentRollback $ inferBaseType e
  inferRefinedType e'

-- | Analyze a top-level function definition. Returns a type binding for the function.
-- Note that top-level definitions are always lifted and effect-less.
analyzeTopLevelDefinition :: TopLevelDefinition -> TypeDerivation (VariableId, Type)
-- Top-level definition with arguments and type signature
analyzeTopLevelDefinition (TopLevelDefinition id args (Just sig) e) = do
  case sig of
    TBang _ typ -> do
      (inferTyp, inferEff) <- withNonLinearContext $ inferTLDefType args e typ
      let atyp = TBang inferEff inferTyp
      unlessSubtype atyp sig $ throwLocalError . UnexpectedType (EVar id) sig =<< runSimplifyType atyp
      finalSignature <- runSimplifyType sig -- TODO: necessary so that implicit closure annotations of Identity are desugared. Inefficient, redo.
      return (id, finalSignature)
    typ -> throwLocalError $ UnbangedSignature id typ
  where
    -- | @inferTLDefType args e typ eff@ checks that the top-level definition of the function @id@
    -- with parameters @args@ and body @e@ has type @typ@ and effect @eff@.
    inferTLDefType :: [Pattern] -> Expr -> Type -> TypeDerivation (Type, Maybe Index)
    -- no arguments, just infer the type of e
    inferTLDefType [] e _ = withScope e $ analyzeExpression e
    -- at least one argument, remainder has arrow type, treat this like an abstraction
    inferTLDefType (arg:rargs) e (TArrow domType codType _ _) = do
      checkWellFormedness domType
      (varNames, varTypes) <- makePatternBindings arg domType SizedLists
      ((typ, eff), asize) <- withEnvSize $ withBoundVariables varNames varTypes $ inferTLDefType rargs e codType
      effAnno <- ifGlobalResources Identity
      sizeAnno <- ifGlobalResources asize
      return (TArrow domType typ eff (join sizeAnno), effAnno)
    -- at least one argument, remainder has forall type, treat this like an index absraction
    inferTLDefType (arg:rargs) e (TIForall tArg codType _ _) = do
      unless (arg == PVar tArg) $ throwLocalError $ UnexpectedIndexVariableArgument id tArg arg
      ((typ, eff), asize) <- withEnvSize $ withBoundIndexVariables [tArg] $ inferTLDefType rargs e codType
      effAnno <- ifGlobalResources Identity
      sizeAnno <- ifGlobalResources asize
      return (TIForall tArg typ eff (join sizeAnno), effAnno)
    -- at least one argument, but other type form. This is an error.
    inferTLDefType (arg:_) _ _ = throwLocalError $ ExtraArgument id arg
-- Top-level definition with no arguments and no type signature: just infer the type
analyzeTopLevelDefinition (TopLevelDefinition id [] Nothing e) = do
  (typ, eff) <- withNonLinearContext $ analyzeExpression e
  ftyp <- runSimplifyType $ TBang eff typ
  return (id, ftyp)
-- Top-level definition with arguments, but no type signature: cannot infer type, throw error.
analyzeTopLevelDefinition (TopLevelDefinition id (arg:_) Nothing _)
  = throwLocalError $ MissingSignature id arg


-- | Analyze all the top-level definitions in a module. Return a list of type bindings.
analyzeModule :: Module -> TypeDerivation [(VariableId, Type)]
analyzeModule mod = go (tldefs mod)
  where
    go [] = return []
    go (tldef : rtldefs) = do
      (fid, fsig) <- analyzeTopLevelDefinition tldef
      rbindings <- withBoundVariables [fid] [fsig] (go rtldefs)
      return $ (fid, fsig) : rbindings

-- | @withLibraries libs der@ runs type derivation @der@
-- where all the bindings coming from the the modules in @libs@ are available.
withLibraries :: [Module] -> TypeDerivation a -> TypeDerivation a
withLibraries [] der = der
withLibraries (lib : rlibs) der = do
  libraryBindings <- analyzeModule lib
  uncurry withBoundVariables (unzip libraryBindings) $ withLibraries rlibs der

-- | @runAnalysis mod libs sh mgmm mlmm@ analyzes module @mod@, with libraries @libs@,
-- solver @sh@, and optional global and local metric modules @mgmm@ and @mlmm@.
-- Returns either a type error, or a list with the type bindings from @mod@.
runAnalysis :: Module
  -> [Module]
  -> SolverHandle
  -> Maybe GlobalMetricModule
  -> Maybe LocalMetricModule
  -> IO (Either TypeError [(VariableId, Type)])
runAnalysis mod libs sh mgmm mlmm = runExceptT $ evalTypeDerivation (withLibraries libs $ analyzeModule mod) (emptyEnv sh mgmm mlmm)