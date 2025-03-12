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
    unifyTypes,
    withBoundVariables,
    withEnvSize,
    withNonLinearContext,
    withBoundIndexVariable,
    withScope,
    unlessSubtype,
    unlessSubtypeAssuming,
    unlessEq,
    unlessLeq,
    unlessIdentity,
    makePatternBindings,
    runSimplifyType,
    ifGlobalResources,
    SizeDiscipline (..),
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
import Lang.Analysis.Environment
import Lang.Analysis.TypeError
import Control.Monad.Except
import Control.Monad.Identity
import Lang.Type.Semantics (checkSubtype, simplifyType)
import Index.Semantics
import Solving.CVC5 (SolverHandle)
import Lang.Expr.Pattern
import Index.Unify

--- TYPE DERIVATIONS MODULE --------------------------------------------------------------
---
--- This module contains base definitions to work with type derivations in
--- a linear setting. It defines the type of type derivation computations,
--- the basic type derivations that interact with the environment and
--- some useful combinators to build more complex derivations.
------------------------------------------------------------------------------------------


--- BASIC DERIVATIONS ---------------------------------------------------------------

type DerivationResult = ExceptT TypeError IO

-- The datatype of type derivations
-- Stateful computations with a typing environment that may throw a type error
type TypeDerivation = StateT TypingEnvironment DerivationResult

runTypeDerivation :: TypeDerivation a -> TypingEnvironment -> DerivationResult (a, TypingEnvironment)
runTypeDerivation = runStateT

evalTypeDerivation :: TypeDerivation a -> TypingEnvironment -> DerivationResult a
evalTypeDerivation = evalStateT

execTypeDerivation :: TypeDerivation a -> TypingEnvironment -> DerivationResult TypingEnvironment
execTypeDerivation = execStateT

--- BASIC DERIVATION OPERATIONS ------------------------------------------------------

-- | @throwLocalError err@ throws a type error passing the current scope to the error constructor
throwLocalError :: ([Expr] -> TypeError) -> TypeDerivation a
throwLocalError err = do
  exprs <- gets scopes
  throwError $ err exprs

-- | @typingContextLookup id@ looks up the type of variable @id@ in the typing context.
-- If the variable is linear, it is marked as used.
-- If the variable is not found, it throws 'UnboundVariable'.
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
makeFreshVariable :: String -> TypeDerivation VariableId
makeFreshVariable prefix = do
  env@TypingEnvironment {freshCounter = c} <- get
  put env {freshCounter = c + 1}
  return $ prefix ++ show c

-- | @unifyType e t1 t2@ attempts to find the most general type substitution @sub@ such that @sub t1 == t2@.
-- If such a substitution does not exist, it throws 'UnexpectedType'. If it exists, the resulting substitution
-- is applied to the current typing environment and returned.
-- Expression @e@ is only used for error reporting.
unifyTypes :: Expr -> Type -> Type -> TypeDerivation TypeSubstitution
unifyTypes e t1 t2 = case mgtu t1 t2 of
  Just sub -> do
    substituteInEnvironment sub
    return sub
  Nothing -> throwLocalError $ UnexpectedType e t2 t1

-- | Flag to determine whether lists must be checked for non-emptiness
data SizeDiscipline = SizedLists | UnsizedLists deriving (Eq, Show)
-- | @makePatternBindings pat typ sl@ returns a list of pairs of variable names and types
-- obtained by matching pattern @pat@ with type @typ@. The size discipline @sl@ determines
-- whether lists must be checked for non-emptiness.
makePatternBindings :: Pattern -> Type -> SizeDiscipline -> TypeDerivation ([VariableId], [Type])
makePatternBindings pat typ sl = do
  sh <- gets solverHandle
  unzip <$> go sh sl pat typ
  where
    go :: SolverHandle -> SizeDiscipline -> Pattern -> Type -> TypeDerivation [(VariableId, Type)]
    go _ _ PHole _ = return []
    go _ _ (PVar id) typ = return [(id, typ)]
    go sh sl (PTuple ps) (TTensor ts) = concat <$> zipWithM (go sh sl) ps ts
    go sh sl p@(PCons p1 p2) typ@(TList id i typ1) = do
      -- used during inference with indices, check that list is not empty
      when (sl == SizedLists) $ unlessLeq (Number 1) i $ throwLocalError $ ConsEmptyList p typ
      bindings1 <- go sh sl p1 (TList id (Minus i (Number 1)) typ1)
      bindings2 <- go sh sl p2 typ1
      return $ bindings1 ++ bindings2
    go _ _ p t = throwLocalError $ PatternMismatch p t

-- | @runSimplifyType t@ simplifies type @t@ using the environment's SMT solver,
-- global resource semantics, and local resource semantics.
runSimplifyType :: Type -> TypeDerivation Type
runSimplifyType t = do
  sh <- gets solverHandle
  grs <- gets grs
  lrs <- gets lrs
  liftIO $ simplifyType sh grs lrs t


--- DERIVATION COMBINATORS ------------------------------------------------------

-- | @withBoundVariables ids typs der@ is derivation @der@ in which
-- the variables in @ids@ are bound to the corresponding types in @typs@.
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

-- | @withEnvSize der@ is derivation @der@ in which the result of the computation is paired with an index describing
-- the size of the portion of the environment that has been consumed during @der@.
withEnvSize :: TypeDerivation a -> TypeDerivation (a, Maybe Index)
withEnvSize der = do
  TypingEnvironment {typingContext = gamma} <- get
  outcome <- der
  TypingEnvironment {typingContext = gamma'} <- get
  -- check the size of the linear resources that have disappeared from the environment
  let consumedSize = diffSize gamma gamma'
  return (outcome, consumedSize)
  where
    diffSize :: TypingContext -> TypingContext -> Maybe Index
    diffSize  gamma1 gamma2 =
      typeSize  $
        Map.elems $
          Map.differenceWith
            ( \bs1 bs2 -> case (bs1, bs2) of
                -- it was an available linear resource in gamma1 and it is a used linear resource in gamma2:
                (b1 : _, b2 : _) -> if canBeUsed b1 && not (canBeUsed b2) then Just [b1] else Nothing
                (_, _) -> error "Internal error: empty binding list"
            )
            gamma1
            gamma2

-- | @withNonLinearContext der@ is derivation @der@ in which a 'LiftedLinearVariable' error
-- is thrown if any linear resource from the existing typing context is consumed.
-- This is useful to ensure that a computation is not consuming linear resources.
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
withBoundIndexVariable :: IVarId -> TypeDerivation a -> TypeDerivation a
withBoundIndexVariable id der = do
  env@TypingEnvironment {indexContext = theta} <- get
  when (Set.member id theta) $ throwLocalError $ ShadowedIndexVariable id
  put env {indexContext = Set.insert id theta}
  outcome <- der
  env@TypingEnvironment {indexContext = theta} <- get
  put env {indexContext = Set.delete id theta}
  return outcome

-- | @withScope e der@ is derivation @der@ in which expression the enclosing expression @e@ is visible.
-- This is only used to provide good error messages in case of failure and it has no effect on the environment.
withScope :: Expr -> TypeDerivation a -> TypeDerivation a
withScope e der = do
  env@TypingEnvironment {scopes = es} <- get
  put env {scopes = e : es}
  outcome <- der
  env@TypingEnvironment {scopes = es} <- get
  put env {scopes = tail es}
  return outcome

-- | @unlessSubtypeAssuming cs t1 t2 der@ is a derivation that behaves like @der@ if @t1@ is not a subtype of @t2@,
-- assuming that the constraints @cs@ hold. If @t1@ is a subtype of @t2@, it does nothing.
unlessSubtypeAssuming :: [Constraint] -> Type -> Type -> TypeDerivation () -> TypeDerivation ()
unlessSubtypeAssuming cs t1 t2 der = do
  sh <- gets solverHandle
  grs <- gets grs
  lrs <- gets lrs
  c <- liftIO $ checkSubtype cs sh grs lrs t1 t2
  unless c der

-- | @unlessSubtype t1 t2 der@ is shorthand for @unlessSubtypeAssuming [] t1 t2 der@.
unlessSubtype :: Type -> Type -> TypeDerivation () -> TypeDerivation ()
unlessSubtype = unlessSubtypeAssuming []

-- | @ifGlobalResources x@ is a derivation that returns @Just x@ if global resources are enabled, and @Nothing@ otherwise.
ifGlobalResources :: a -> TypeDerivation (Maybe a)
ifGlobalResources x = do
  grs <- gets grs
  case grs of
    Nothing -> return Nothing
    Just _ -> return $ Just x


--- Arithmetic index checking ---

-- | @unlessLeq i j der@ is a derivation that behaves like @der@ if @i@ is not less than or equal to @j@.
-- If @i@ is less than or equal to @j@, it does nothing.
unlessLeq :: Index -> Index -> TypeDerivation () -> TypeDerivation ()
unlessLeq i j der = do
  sh <- gets solverHandle
  c <- liftIO $ checkLeq [] sh i j
  unless c der

-- | @unlessEq i j der@ is a derivation that behaves like @der@ if @i@ is not equal to @j@.
-- If @i@ is equal to @j@, it does nothing.
unlessEq :: Index -> Index -> TypeDerivation () -> TypeDerivation ()
unlessEq i j der = do
  sh <- gets solverHandle
  c <- liftIO $ checkEq [] sh i j
  unless c der


--- Global resource index checking ---

-- | @unlessGRLeq@ is like @unlessLeq@ but specialized to global metric annotations.
unlessGRLeq :: Maybe Index -> Maybe Index -> TypeDerivation () -> TypeDerivation ()
unlessGRLeq i j der = do
  qfh <- gets solverHandle
  grs <- gets grs
  c <- liftIO $ checkGRLeq [] qfh grs i j
  unless c der

-- | @unlessGREq@ is like @unlessEq@ but specialized to global metric annotations.
unlessGREq :: Maybe Index -> Maybe Index -> TypeDerivation () -> TypeDerivation ()
unlessGREq i j der = do
  qfh <- gets solverHandle
  grs <- gets grs
  c <- liftIO $ checkGREq [] qfh grs i j
  unless c der

--- Local resource index checking ---

-- | @unlessLRLeq@ is like @unlessLeq@ but specialized to local metric annotations.
unlessLRLeq :: Maybe Index -> Maybe Index -> TypeDerivation () -> TypeDerivation ()
unlessLRLeq i j der = do
  qfh <- gets solverHandle
  lrs <- gets lrs
  c <- liftIO $ checkLRLeq [] qfh lrs i j
  unless c der

-- | @unlessLREq@ is like @unlessEq@ but specialized to local metric annotations.
unlessLREq :: Maybe Index -> Maybe Index -> TypeDerivation () -> TypeDerivation ()
unlessLREq i j der = do
  qfh <- gets solverHandle
  lrs <- gets lrs
  c <- liftIO $ checkLREq [] qfh lrs i j
  unless c der

-- | @unlessIdentity i der@ is a derivation that behaves like @der@ if @i@ is not equal to the identity index.
-- If @i@ is equal to the identity index, it does nothing.
unlessIdentity :: Maybe Index -> TypeDerivation () -> TypeDerivation ()
unlessIdentity i = unlessGREq i (Just Index.AST.Identity)



--- OTHER STUFF ----------------------------------------------------------------

-- Necessary to avoid redundant case analysis in subsequent passes
instance MonadFail (Either TypeError) where
  fail _ = error "Internal error: unexpected type form in subsequent typing pass"


instance MonadFail Identity where
  fail _ = error "Internal error: unexpected type form in subsequent typing pass"