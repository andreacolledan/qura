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
import Control.Monad (unless)
import Data.Maybe (fromJust)

-- | Analyze an expression, inferring its type and possibly its effect.
-- Runs both Milner-style inference and refinement synthesis, in sequence.
analyzeExpression :: Expr -> TypeDerivation (Type, Maybe Index)
analyzeExpression e = do
  (e', _, _) <- withEnvironmentRollback $ inferBaseType e
  inferRefinedType e'

-- | Analyze a top-level function definition. Returns a type binding for the function.
-- Note that top-level definitions are always pure and effect-less.
analyzeTopLevelDefinition :: TopLevelDefinition -> TypeDerivation (VariableId, Type)
-- Top-level definition with arguments and type signature
analyzeTopLevelDefinition (TopLevelDefinition id args (Just sig) e) = do
  case sig of
    TBang eff typ -> withNonLinearContext (checkTldef args e typ eff ) >> return (id, sig)
    typ -> throwLocalError $ UnbangedSignature id typ
  where
    -- | @checkTldef args e typ eff@ checks that the top-level definition of the function @id@
    -- with parameters @args@ and body @e@ has type @typ@ and effect @eff@.
    checkTldef :: [VariableId] -> Expr -> Type -> Maybe Index -> TypeDerivation ()
    checkTldef [] e typ eff = withScope e $ do
      (atyp, aeff) <- analyzeExpression e
      unlessSubtype atyp typ $ throwLocalError $ UnexpectedType e typ atyp
      unlessGRLeq aeff eff $ throwLocalError $ UnexpectedEffect e (fromJust eff) (fromJust aeff) -- If either is Nothing, then unlessGRLeq handles the situation
    checkTldef (arg:rargs) e typ eff = do
      (fSize, asize) <- withEnvSize $ case typ of
        TArrow domType codType fEff fSize -> do
          withBoundVariables [arg] [domType] $ checkTldef rargs e codType fEff
          return fSize
        TIForall tArg codType fEff fSize -> do
          unless (arg == tArg) $ throwLocalError $ BoundIndexVariableMismatch id tArg arg
          withBoundIndexVariable arg $ checkTldef rargs e codType fEff
          return fSize
        _ -> throwLocalError $ ExtraArgument id arg
      unlessGRLeq (Just Identity) eff $ error "Internal error"
      unlessGREq asize fSize $ throwLocalError $ UnexpectedIndex (fromJust fSize) (fromJust asize)
-- Top-level definition with no arguments and no type signature: just infer the type
analyzeTopLevelDefinition (TopLevelDefinition id [] Nothing e) = do
  (typ, i) <- withNonLinearContext $ analyzeExpression e
  ftyp <- runSimplifyType $ TBang i typ
  return (id, ftyp)
-- Top-level definition with arguments, but no type signature: cannot infer type, throw error.
analyzeTopLevelDefinition (TopLevelDefinition _ (arg:_) Nothing _)
  = fail $ "Cannot determine type for function parameter " ++ arg


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