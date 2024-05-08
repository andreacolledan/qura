module Lang.Analysis.Environment where
import Lang.Type.AST
import PrettyPrinter
import qualified Data.HashMap.Lazy as Map
import Lang.Expr.AST
import Index.AST
import qualified Data.HashSet as Set
import Solving.CVC5


--- TYPE ENVIRONMENT MODULE ------------------------------------------------------------
---
--- This module defines the typing environment used by the analysis procedures.Constant
--- It defines bindings, typing contexts, and their encasing environments.
----------------------------------------------------------------------------------------

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
  { typingContext :: TypingContext,     -- attributes types to variable names (linear/nonlinear)
    indexContext :: IndexContext,       -- keeps track of the existing index variables in the environment
    scopes :: [Expr],                   -- a list of the expressions enclosing the current one
    liftedExpression :: Bool,           -- whether the current expression is in a nonlinear context
    freshCounter :: Int,                -- a counter for generating fresh index variables
    solverHandle :: SolverHandle        -- the handle to the SMT solver
  }

instance Wide TypingEnvironment where
  wireCount TypingEnvironment {typingContext = gamma} = wireCount gamma

-- | @makeEnvForall theta gamma q@ initializes a typing environment from the dictionary-like definitions of @gamma@ and @q@.
-- The index variables in @theta@ are considered to be in scope.
makeEnvForall :: [IndexVariableId] -> [(VariableId, Type)] -> SolverHandle -> TypingEnvironment
makeEnvForall theta gamma sh =
  let gamma' = Map.fromList [(id, [BindingInfo typ False]) | (id, typ) <- gamma]
   in TypingEnvironment gamma' (Set.fromList theta) [] True 0 sh

-- | @makeEnv gamma q@ initializes a typing environment from the dictionary-like definitions of @gamma@ and @q@.
-- No index variables are considered to be in scope.
makeEnv :: [(VariableId, Type)] -> SolverHandle -> TypingEnvironment
makeEnv = makeEnvForall []

-- | The empty typing environment. No variables are in scope.
emptyEnv ::  SolverHandle -> TypingEnvironment
emptyEnv = makeEnv [] 

-- | @envIsLinear env@ returns 'True' if the environment @env@ contains any linear variables or labels.
envIsLinear :: TypingEnvironment -> Bool
envIsLinear TypingEnvironment {typingContext = gamma} =
  let remainingVars = [id | (id, bs) <- Map.toList gamma, any mustBeUsed bs] -- remaining linear variables
   in not (null remainingVars)