module Analyzer
  ( runAnalysis,
    -- re-exports
    TypeError,
    Type,
  )
where

import Analyzer.Derivation
  ( SizeDiscipline (SizedLists),
    TypeDerivation,
    TypeError
      ( ExtraArgument,
        MissingSignature,
        UnbangedSignature,
        UnexpectedIndexVariableArgument,
        UnexpectedType
      ),
    checkWellFormedness,
    emptyEnv,
    evalTypeDerivation,
    ifGlobalResources,
    makePatternBindings,
    runSimplifyType,
    throwLocalError,
    unlessSubtype,
    withBoundIndexVariables,
    withBoundVariables,
    withEnvSize,
    withEnvironmentRollback,
    withNonLinearContext,
    withScope,
  )
import Analyzer.InferBaseType (inferBaseType)
import Analyzer.InferRefinedType (inferRefinedType)
import Control.Monad (join, unless)
import Control.Monad.Except (runExceptT)
import Metric.Global (GlobalMetricModule)
import Metric.Local (LocalMetricModule)
import PQ.Expr (Expr (EVar), Pattern (PVar), VariableId)
import PQ.Index (Index (Identity))
import PQ.Module
  ( Module (Module),
    TopLevelDefinition (TopLevelDefinition),
  )
import PQ.Type (Type (TArrow, TBang, TIForall))
import Solver.SMT (SolverHandle)

-- | Analyze an expression, annotating it with type information,
-- inferring its overall type and possibly its effect.
-- Runs both Milner-style inference and refinement synthesis, in sequence.
analyzeExpression :: Expr -> TypeDerivation (Expr, Type, Maybe Index)
analyzeExpression e = do
  (e', _, _) <- withEnvironmentRollback $ inferBaseType e
  (typ, i) <- inferRefinedType e'
  return (e', typ, i)

-- | Analyze a top-level function definition. Returns a binding, in the
-- form of the name of the function, its type-annotated body, and its type.
-- Note that top-level definitions are always pure and effect-less.
analyzeTopLevelDefinition :: TopLevelDefinition -> TypeDerivation TopLevelDefinition
-- Top-level definition with arguments and type signature
analyzeTopLevelDefinition (TopLevelDefinition id args (Just sig) e) = do
  case sig of
    TBang _ typ -> do
      (e', inferTyp, inferEff) <- withNonLinearContext $ inferTLDefType args e typ
      let atyp = TBang inferEff inferTyp
      unlessSubtype atyp sig $ throwLocalError . UnexpectedType (EVar id) sig =<< runSimplifyType atyp
      return $ TopLevelDefinition id args (Just sig) e'
    typ -> throwLocalError $ UnbangedSignature id typ
  where
    -- | @inferTLDefType args e typ eff@ checks that the top-level definition of the function @id@
    -- with parameters @args@ and body @e@ has type @typ@ and effect @eff@.
    inferTLDefType :: [Pattern] -> Expr -> Type -> TypeDerivation (Expr, Type, Maybe Index)
    -- no arguments, just infer the type of e
    inferTLDefType [] e _ = withScope e $ analyzeExpression e
    -- at least one argument, remainder has arrow type, treat this like an abstraction
    inferTLDefType (arg:rargs) e (TArrow domType codType _ _) = do
      checkWellFormedness domType
      (varNames, varTypes) <- makePatternBindings arg domType SizedLists
      ((e', typ, eff), asize) <- withEnvSize $ withBoundVariables varNames varTypes $ inferTLDefType rargs e codType
      effAnno <- ifGlobalResources Identity
      sizeAnno <- ifGlobalResources asize
      return (e', TArrow domType typ eff (join sizeAnno), effAnno)
    -- at least one argument, remainder has forall type, treat this like an index absraction
    inferTLDefType (arg:rargs) e (TIForall tArg codType _ _) = do
      unless (arg == PVar tArg) $ throwLocalError $ UnexpectedIndexVariableArgument id tArg arg
      ((e', typ, eff), asize) <- withEnvSize $ withBoundIndexVariables [tArg] $ inferTLDefType rargs e codType
      effAnno <- ifGlobalResources Identity
      sizeAnno <- ifGlobalResources asize
      return (e', TIForall tArg typ eff (join sizeAnno), effAnno)
    -- at least one argument, but other type form. This is an error.
    inferTLDefType (arg:_) _ _ = throwLocalError $ ExtraArgument id arg
-- Top-level definition with no arguments and no type signature: just infer the type
analyzeTopLevelDefinition (TopLevelDefinition id [] Nothing e) = do
  (e', typ, eff) <- withNonLinearContext $ analyzeExpression e
  ftyp <- runSimplifyType $ TBang eff typ
  return $ TopLevelDefinition id [] (Just ftyp) e'
-- Top-level definition with arguments, but no type signature: cannot infer type, throw error.
analyzeTopLevelDefinition (TopLevelDefinition id (arg:_) Nothing _)
  = throwLocalError $ MissingSignature id arg


-- | Analyze all the top-level definitions in a module. Return a list of bindings.
analyzeModule :: Module -> TypeDerivation Module
analyzeModule (Module name exports imports tldefs) = Module name exports imports <$> go tldefs
  where
    go [] = return []
    go (tldef : rtldefs) = do
      TopLevelDefinition fid fargs (Just fsig) fbody <- analyzeTopLevelDefinition tldef
      rbindings <- withBoundVariables [fid] [fsig] (go rtldefs)
      return $ TopLevelDefinition fid fargs (Just fsig) fbody : rbindings

-- | @withLibraries libs der@ runs type derivation @der@
-- where all the bindings coming from the the modules in @libs@ are available.
withLibraries :: [Module] -> TypeDerivation a -> TypeDerivation a
withLibraries [] der = der
withLibraries (lib : rlibs) der = do
  libraryBindings <- analyzeModule lib
  uncurry withBoundVariables (toTypeBindings libraryBindings) $ withLibraries rlibs der
  where
    toTypeBindings :: Module -> ([VariableId], [Type])
    toTypeBindings (Module _ _ _ tldefs) = foldr collect ([], []) tldefs
      where
        collect :: TopLevelDefinition -> ([VariableId], [Type]) -> ([VariableId], [Type])
        collect (TopLevelDefinition id _ (Just typ) _) (ids, types) = (id : ids, typ : types)
        collect (TopLevelDefinition _ _ Nothing _) _ = error "Internal error: library top-level definition without type"

-- | @runAnalysis mod libs sh mgmm mlmm@ analyzes module @mod@, with libraries @libs@,
-- solver @sh@, and optional global and local metric modules @mgmm@ and @mlmm@.
-- Returns either a type error, or a list with the bindings from @mod@.
runAnalysis :: Module
  -> [Module]
  -> SolverHandle
  -> Maybe GlobalMetricModule
  -> Maybe LocalMetricModule
  -> IO (Either TypeError Module)
runAnalysis mod libs sh mgmm mlmm = runExceptT $ evalTypeDerivation (withLibraries libs $ analyzeModule mod) (emptyEnv sh mgmm mlmm)