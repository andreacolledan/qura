{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lang.Analysis.Derivation
  ( TypeDerivation,
    TypeError (..),
    DerivationResult,
    TypingEnvironment (..),
    emptyEnv,
    makeEnv,
    mustBeUsed,
    makeEnvForall,
    envIsLinear,
    runTypeDerivation,
    evalTypeDerivation,
    execTypeDerivation,
    throwLocalError,
    typingContextLookup,
    substituteInEnvironment,
    checkWellFormedness,
    makeFreshVariable,
    unify,
    withBoundVariables,
    withWireCount,
    withNonLinearContext,
    withBoundIndexVariable,
    withScope,
    unlessSubtype,
    unlessEq,
    unlessLeq,
    unlessZero,
    makePatternBindings
  )
where

import Control.Monad ( unless, when, zipWithM, zipWithM_ )
import Control.Monad.Error.Class
import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Index.AST
import Lang.Type.AST
import Lang.Type.Unify
import Lang.Expr.AST
import PrettyPrinter
import Control.Monad.Except
import Control.Monad.Identity
import Lang.Type.Semantics (checkSubtype)
import Index.Semantics
import Solving.CVC5 (SolverHandle)
import Lang.Expr.Pattern

--- TYPE DERIVATIONS MODULE --------------------------------------------------------------
---
--- This module contains base definitions to work with type derivations in
--- a linear setting. It defines the type of type derivation computations,
--- their environment, the basic operations used to manipulate it and
--- some useful combinators to build more complex derivations.
------------------------------------------------------------------------------------------

--- BINDINGS ------------------------------------------------------------------

-- | The datatype of bindings (carries the type of a variable and whether it has been used yet)
data BindingInfo = BindingInfo {getType :: Type, isUsed :: Bool} deriving (Eq, Show)

-- 
instance Wide BindingInfo where
  wireCount binding = if isUsed binding then Number 0 else wireCount (getType binding)

instance Pretty BindingInfo where
  pretty = pretty . getType

-- | @canBeUsed b@ returns 'True' if the binding @b@ is of a parameter type
-- or if it is a linear type and the corresponding variable has not been used yet.
canBeUsed :: BindingInfo -> Bool
canBeUsed (BindingInfo typ used) = not used || not (isLinear typ)

-- | @mustBeUsed b@ returns 'True' if the binding @b@ is of a linear type
-- and the corresponding variable has not been used yet.
mustBeUsed :: BindingInfo -> Bool
mustBeUsed (BindingInfo typ used) = not used && isLinear typ

--- TYPING CONTEXTS -----------------------------------------------------------

-- | The datatype of typing contexts (Corresponds to Γ or Φ in the paper)
type TypingContext = Map.HashMap VariableId [BindingInfo]

-- | The empty typing context
emptyctx :: Map.HashMap a b
emptyctx = Map.empty

--- TYPING ENVIRONMENTS --------------------------------------------------------

-- | The datatype of typing environments.
-- Represents the state of any linear type derivation
data TypingEnvironment = TypingEnvironment
  { typingContext :: TypingContext, -- attributes types to variable names (linear/nonlinear)
    indexContext :: IndexContext, -- keeps track of the existing index variables in the environment
    scopes :: [Expr], -- a list of the expressions enclosing the current one
    liftedExpression :: Bool, -- whether the current expression is in a nonlinear context
    freshCounter :: Int -- a counter for generating fresh index variables
  }

instance Wide TypingEnvironment where
  wireCount TypingEnvironment {typingContext = gamma} = wireCount gamma

-- | @makeEnvForall theta gamma q@ initializes a typing environment from the dictionary-like definitions of @gamma@ and @q@.
-- The index variables in @theta@ are considered to be in scope.
makeEnvForall :: [IndexVariableId] -> [(VariableId, Type)] -> TypingEnvironment
makeEnvForall theta gamma =
  let gamma' = Map.fromList [(id, [BindingInfo typ False]) | (id, typ) <- gamma]
   in TypingEnvironment gamma' (Set.fromList theta) [] True 0

-- | @makeEnv gamma q@ initializes a typing environment from the dictionary-like definitions of @gamma@ and @q@.
-- No index variables are considered to be in scope.
makeEnv :: [(VariableId, Type)] -> TypingEnvironment
makeEnv = makeEnvForall []

-- | The empty typing environment. No variables are in scope.
emptyEnv :: TypingEnvironment
emptyEnv = makeEnv [] 

-- | @envIsLinear env@ returns 'True' if the environment @env@ contains any linear variables or labels.
envIsLinear :: TypingEnvironment -> Bool
envIsLinear TypingEnvironment {typingContext = gamma} =
  let remainingVars = [id | (id, bs) <- Map.toList gamma, any mustBeUsed bs] -- remaining linear variables
   in not (null remainingVars)

--- TYPING ERRORS ---------------------------------------------------------------

-- The datatype of errors that can occur during a derivation
data TypeError
  = UnboundVariable VariableId [Expr]
  | UnboundIndexVariable IndexVariableId [Expr]
  | UnexpectedType Expr Type Type [Expr]
  | UnexpectedIndex Index Index [Expr]
  | UnexpectedWidthAnnotation Expr Index Index [Expr]
  | ExpectedBundleType Expr Type [Expr]
  | CannotSynthesizeType Expr [Expr]
  | -- Pattern errors
    PatternMismatch Pattern Type [Expr]
  | ConsEmptyList Pattern Type [Expr]
  | -- Linearity errors
    UnusedLinearVariable VariableId [Expr]
  | OverusedLinearVariable VariableId [Expr]
  | LiftedLinearVariable VariableId [Expr]
  | -- Box errors
    UnboxableType Expr Type [Expr]
  | -- Fold errors
    UnfoldableStepfunction Expr Type [Expr]
  | UnfoldableAccumulator Expr Type [Expr]
  | UnfoldableArg Expr Type [Expr]
  | -- Other
    ShadowedIndexVariable IndexVariableId [Expr]
  | UnexpectedEmptyList Expr Type [Expr]
  deriving (Eq)

instance Show TypeError where
  show (UnboundVariable id surr) = "* Unbound variable '" ++ id ++ "'" ++ printSurroundings surr
  show (UnusedLinearVariable id surr) = "* Unused linear variable '" ++ id ++ "'" ++ printSurroundings surr
  show (LiftedLinearVariable id surr) = "* Linear variable '" ++ id ++ "' cannot be consumed in a lifted expression" ++ printSurroundings surr
  show (UnexpectedType exp typ1 typ2 surr) =
    "* Expected expression '" ++ trnc 80 (pretty exp) ++ "'\n   to have type\n    '" ++ pretty typ1 ++ "',\n   got\n    '" ++ pretty typ2 ++ "'\n   instead" ++ printSurroundings surr
  show (UnexpectedWidthAnnotation m i j surr) =
    "* Expected expression '" ++ pretty m ++ "' to have width annotation '" ++ pretty i ++ "', got '" ++ pretty j ++ "' instead" ++ printSurroundings surr
  show (UnexpectedIndex i1 i2 surr) = "* Expected index '" ++ pretty i1 ++ "', got '" ++ pretty i2 ++ "' instead" ++ printSurroundings surr
  show (UnboxableType v typ surr) = "* Cannot box value '" ++ pretty v ++ "' of type '" ++ pretty typ ++ "'" ++ printSurroundings surr
  show (UnfoldableStepfunction v typ surr) = "* Expression '" ++ pretty v ++ "' of type '" ++ pretty typ ++ "' is not a valid step function" ++ printSurroundings surr
  show (UnfoldableAccumulator v typ surr) = "* Expression '" ++ pretty v ++ "' of type '" ++ pretty typ ++ "' is not a valid accumulator" ++ printSurroundings surr
  show (UnfoldableArg v typ surr) = "* Expression '" ++ pretty v ++ "' of type '" ++ pretty typ ++ "' is not a valid fold argument" ++ printSurroundings surr
  show (UnboundIndexVariable id surr) = "* Unbound index variable '" ++ id ++ "'" ++ printSurroundings surr
  show (ShadowedIndexVariable id surr) = "* Shadowed index variable '" ++ id ++ "'" ++ printSurroundings surr
  show (OverusedLinearVariable id surr) = "* Linear variable '" ++ id ++ "' is used more than once" ++ printSurroundings surr
  show (UnexpectedEmptyList e typ surr) = "* Cannot conclude that expression '" ++ pretty e ++ "' of type '" ++ pretty typ ++ "' is a non-empty list" ++ printSurroundings surr
  show (ExpectedBundleType e typ surr) = "* Expected expression '" ++ pretty e ++ "' to have bundle type, got '" ++ pretty typ ++ "' instead" ++ printSurroundings surr
  show (CannotSynthesizeType e surr) = "* Cannot synthesize type for expression '" ++ pretty e ++ "'. Consider annotating it with a type" ++ printSurroundings surr

-- | @printSurroundings es@ returns a string describing the expressions in @es@, if any
printSurroundings :: [Expr] -> String
printSurroundings [] = ""
printSurroundings (e : es) = "\n* While typing " ++ pretty e ++ go es 3
  where
    go :: [Expr] -> Int -> String
    go [] _ = ""
    go _ 0 = "\n..."
    go (e : es) n = "\n  In " ++ trnc 80 (pretty e) ++ go es (n - 1)

-- | @printConstructor t@ returns a string describing the top-level constructor of type @t@
printConstructor :: Type -> String
printConstructor TUnit = "unit type"
printConstructor (TWire {}) = "wire type"
printConstructor (TTensor {}) = "tensor type"
printConstructor (TCirc {}) = "circuit type"
printConstructor (TArrow {}) = "arrow type"
printConstructor (TBang {}) = "bang type"
printConstructor (TList {}) = "list type"
printConstructor (TVar {}) = "type variable"
printConstructor (TIForall {}) = "forall type"

-- | @trnc n s@ returns the first @n@ characters of @s@, followed by "..." if @s@ is longer than @n@
trnc :: Int -> String -> String
trnc n s = if length s > n then take n s ++ "..." else s

--- TYPE DERIVATIONS ---------------------------------------------------------------

type DerivationResult = ExceptT TypeError IO

-- The datatype of type derivations
-- Stateful computations with a typing environment, which may throw a type error
type TypeDerivation = StateT TypingEnvironment DerivationResult

runTypeDerivation :: TypeDerivation a -> TypingEnvironment -> DerivationResult (a, TypingEnvironment)
runTypeDerivation = runStateT

evalTypeDerivation :: TypeDerivation a -> TypingEnvironment -> DerivationResult a
evalTypeDerivation = evalStateT

execTypeDerivation :: TypeDerivation a -> TypingEnvironment -> DerivationResult TypingEnvironment
execTypeDerivation = execStateT

-- Basic derivation operators:

throwLocalError :: ([Expr] -> TypeError) -> TypeDerivation a
throwLocalError err = do
  exprs <- gets scopes
  throwError $ err exprs

-- typingContextLookup x looks up variable x in the typing context
-- It removes it if its type is linear
-- throws UnboundVariable if the variable is absent
typingContextLookup :: VariableId -> TypeDerivation Type
typingContextLookup id = do
  env@TypingEnvironment {typingContext = gamma} <- get
  bindings <- maybe (throwLocalError $ UnboundVariable id) return (Map.lookup id gamma)
  case bindings of
    (b : bs) ->
      if canBeUsed b
        then do
          put env {typingContext = Map.insert id (BindingInfo (getType b) True : bs) gamma}
          return $ getType b
        else throwLocalError $ OverusedLinearVariable id
    [] -> error "Internal error: empty binding list"

-- | @substituteInEnvironment sub@ applies the substitution @sub@ to the typing environment
substituteInEnvironment :: TypeSubstitution -> TypeDerivation ()
substituteInEnvironment sub = do
  env@TypingEnvironment {typingContext = gamma} <- get
  let gamma' = Map.map (map (\(BindingInfo t u) -> BindingInfo (tsub sub t) u)) gamma
  put env {typingContext = gamma'}

-- | @checkWellFormedness x@ checks that all the index variables in @x@ are in scope.
-- It throws 'UnboundIndexVariable' if any of them is not.
checkWellFormedness :: (HasIndex a) => a -> TypeDerivation ()
checkWellFormedness x = do
  theta <- gets indexContext
  case ifv x `Set.difference` theta of
    fv  | Set.null fv -> return () -- all the free variables in the type are also in the context, good
        | otherwise ->  throwLocalError $ UnboundIndexVariable (head . Set.toList $ fv) -- some free variables are not in scope, bad

-- | @makeFreshVariable prefix@ returns a fresh variable name with the given prefix.
-- TODO: using 'scopes', this function could also return a variable that is fresh in the current scope.
makeFreshVariable :: String -> TypeDerivation VariableId
makeFreshVariable prefix = do
  env@TypingEnvironment {freshCounter = c} <- get
  put env {freshCounter = c + 1}
  return $ prefix ++ show c

-- | @unify e t1 t2@ attempts to find the most general type substitution @sub@ such that @sub t1 == t2@.
-- If such a substitution does not exist, it throws 'UnexpectedType'. If it exists, the resulting substitution
-- is applied to the current typing environment and returned.
-- Expression @e@ is only used for error reporting.
unify :: Expr -> Type -> Type -> TypeDerivation TypeSubstitution
unify e t1 t2 = case mgtu t1 t2 of
  Just sub -> do
    substituteInEnvironment sub
    return sub
  Nothing -> throwLocalError $ UnexpectedType e t2 t1

makePatternBindings :: Maybe SolverHandle -> Pattern -> Type -> TypeDerivation ([VariableId], [Type])
makePatternBindings mqfh pat typ = unzip <$> go mqfh pat typ
  where
    go :: Maybe SolverHandle -> Pattern -> Type -> TypeDerivation [(VariableId, Type)]
    go _ (PVar id) typ = return [(id, typ)]
    go mqfh (PTuple ps) (TTensor ts) = concat <$> zipWithM (go mqfh) ps ts
    go (Just qfh) p@(PCons p1 p2) typ@(TList i typ1) = do
      -- used during inference with indices, check that list is not empty
      unlessLeq qfh (Number 1) i $ throwLocalError $ ConsEmptyList p typ
      bindings1 <- go mqfh p1 typ1
      bindings2 <- go mqfh p2 (TList (Minus i (Number 1)) typ1)
      return $ bindings1 ++ bindings2
    go Nothing (PCons p1 p2) (TList i typ1) = do
      -- used during base inference without indices, ignore list length
      bindings1 <- go mqfh p1 typ1
      bindings2 <- go mqfh p2 (TList i typ1)
      return $ bindings1 ++ bindings2

    go _ p t = throwLocalError $ PatternMismatch p t



--- DERIVATION COMBINATORS ------------------------------------------------------

withBoundVariables :: [VariableId] -> [Type] -> TypeDerivation a -> TypeDerivation a
withBoundVariables ids typs der = do
  zipWithM_ bindVariable ids typs
  outcome <- der
  mapM_ unbindVariable (reverse ids) -- this throws an error if x is linear and der does not consume it
  return outcome
  where
    bindVariable :: VariableId -> Type -> TypeDerivation ()
    bindVariable id typ = do
      env@TypingEnvironment {typingContext = gamma} <- get
      bs <- maybe (return []) return (Map.lookup id gamma)
      let gamma' = Map.insert id (BindingInfo typ False : bs) gamma
      put env {typingContext = gamma'}
    unbindVariable :: VariableId -> TypeDerivation ()
    unbindVariable id = do
      env@TypingEnvironment {typingContext = gamma} <- get
      case Map.lookup id gamma of
        Nothing -> error "Internal error: tried to unbind non-existing variable"
        Just [] -> error "Internal error: tried to unbind variable with empty binding list"
        Just (b : bs) -> do
          when (mustBeUsed b) (throwLocalError $ UnusedLinearVariable id)
          put env {typingContext = if null bs then Map.delete id gamma else Map.insert id bs gamma}

-- | @withWireCount der@ is derivation @der@ in which the result of the computation is paired with an index describing
-- how many wires have been consumed during @der@.
withWireCount :: TypeDerivation a -> TypeDerivation (a, Index)
withWireCount der = do
  TypingEnvironment {typingContext = gamma} <- get
  outcome <- der
  TypingEnvironment {typingContext = gamma'} <- get
  -- count how many linear resources have disappeared from the contexts
  let gammaDiff = diffcount gamma gamma'
  let resourceCount = gammaDiff
  return (outcome, resourceCount)
  where
    diffcount :: TypingContext -> TypingContext -> Index
    diffcount gamma1 gamma2 =
      wireCount $
        Map.elems $
          Map.differenceWith
            ( \bs1 bs2 -> case (bs1, bs2) of
                -- it was an available linear resource in gamma1 and it is a used linear resource in gamma2:
                (b1 : _, b2 : _) -> if canBeUsed b1 && not (canBeUsed b2) then Just [b1] else Nothing
                (_, _) -> error "Internal error: empty binding list"
            )
            gamma1
            gamma2

-- | @withNonLinearContext der@ is derivation @der@ in which a 'LiftedLinearVariable' error is thrown if any linear resource from the
-- existing typing context is consumed. This is useful to ensure that a computation is not consuming linear resources.
withNonLinearContext :: TypeDerivation a -> TypeDerivation a
withNonLinearContext der = do
  TypingEnvironment {typingContext = gamma} <- get
  outcome <- der
  TypingEnvironment {typingContext = gamma'} <- get
  let gammaconsumed = linearconsumed gamma gamma'
  unless (Map.null gammaconsumed) $ do
    let remainingNames = Map.keys gammaconsumed
    throwLocalError $ LiftedLinearVariable (head remainingNames)
  return outcome
  where
    linearconsumed :: TypingContext -> TypingContext -> TypingContext
    linearconsumed =
      Map.differenceWith
        ( \bs1 bs2 -> case (bs1, bs2) of
            -- it was an available linear resource in gamma1 and it is a used linear resource in gamma2:
            (b1 : _, b2 : _) -> if mustBeUsed b1 && not (canBeUsed b2) then Just [b1] else Nothing
            (_, _) -> error "Internal error: empty binding list"
        )

-- | @withBoundIndexVariable id der@ is derivation @der@ in which index variable @id@ is in scope.
withBoundIndexVariable :: IndexVariableId -> TypeDerivation a -> TypeDerivation a
withBoundIndexVariable id der = do
  env@TypingEnvironment {indexContext = theta} <- get
  when (Set.member id theta) $ throwLocalError $ ShadowedIndexVariable id
  put env {indexContext = Set.insert id theta}
  outcome <- der
  env@TypingEnvironment {indexContext = theta} <- get
  put env {indexContext = Set.delete id theta}
  return outcome

-- | @withScope e der@ is derivation @der@ in which expression the enclosing expression @e@ is visible.
-- This is only used to provide good error messages in case of failure and it has no effect on the contexts.
withScope :: Expr -> TypeDerivation a -> TypeDerivation a
withScope e der = do
  env@TypingEnvironment {scopes = es} <- get
  put env {scopes = e : es}
  outcome <- der
  env@TypingEnvironment {scopes = es} <- get
  put env {scopes = tail es}
  return outcome

unlessSubtype :: SolverHandle -> Type -> Type -> TypeDerivation () -> TypeDerivation ()
unlessSubtype qfh t1 t2 der = do
  c <- liftIO $ checkSubtype qfh t1 t2
  unless c der

unlessLeq :: SolverHandle -> Index -> Index -> TypeDerivation () -> TypeDerivation ()
unlessLeq qfh i j der = do
  c <- liftIO $ checkLeq qfh i j
  unless c der

unlessEq :: SolverHandle -> Index -> Index -> TypeDerivation () -> TypeDerivation ()
unlessEq qfh i j der = do
  c <- liftIO $ checkEq qfh i j
  unless c der

unlessZero :: SolverHandle -> Index -> TypeDerivation () -> TypeDerivation ()
unlessZero qfh i = unlessEq qfh i (Number 0)


--- OTHER STUFF ----------------------------------------------------------------

-- Necessary to avoid redundant case analysis in subsequent passes
instance MonadFail (Either TypeError) where
  fail _ = error "Internal error: unexpected type form in subsequent typing pass"


instance MonadFail Identity where
  fail _ = error "Internal error: unexpected type form in subsequent typing pass"