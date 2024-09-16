module Lang.Analysis.Environment where
import Lang.Type.AST
import PrettyPrinter
import qualified Data.HashMap.Lazy as Map
import Lang.Expr.AST
import Index.AST
import qualified Data.HashSet as Set
import Solving.CVC5
import Index.Semantics.Global.Resource
import Index.Semantics.Local.Resource


--- TYPE ENVIRONMENT MODULE ------------------------------------------------------------
---
--- This module defines the typing environment used by the analysis procedures.
--- It defines bindings, typing contexts, and their encasing environments.
----------------------------------------------------------------------------------------

--- BINDINGS ------------------------------------------------------------------

-- | The datatype of bindings (carries the type of a variable and whether it has been used yet)
data BindingInfo = BindingInfo {getType :: Type, isUsed :: Bool} deriving (Eq, Show)

instance HasSize BindingInfo where
  typeSize binding = if isUsed binding then Just Identity else typeSize (getType binding)

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

-- | The datatype of typing contexts
type TypingContext = Map.HashMap VariableId [BindingInfo]

-- | The empty typing context
emptyctx :: Map.HashMap a b
emptyctx = Map.empty


--- TYPING ENVIRONMENTS --------------------------------------------------------

-- | The datatype of typing environments.
-- Represents the state of any linear type derivation
data TypingEnvironment = TypingEnvironment
  { typingContext :: TypingContext,       -- attributes types to variable names (linear/nonlinear)
    indexContext :: IndexContext,         -- keeps track of the existing index variables in the environment
    scopes :: [Expr],                     -- a list of the expressions enclosing the current one
    liftedExpression :: Bool,             -- whether the current expression is in a nonlinear context
    freshCounter :: Int,                  -- a counter for generating fresh index variables
    solverHandle :: SolverHandle,         -- the handle to the SMT solver
    grs :: Maybe GlobalMetricModule,      -- the global resource semantics (if any)
    lrs :: Maybe LocalMetricModule        -- the local resource semantics (if any)
  }

instance HasSize TypingEnvironment where
  typeSize TypingEnvironment {typingContext = gamma} = typeSize gamma

-- | @makeEnvForall ictx tctx qfh mgrs mlrs@ initializes a typing environment with index context @ictx@,
-- typing context @tctx@, solver handle @qfh@, and global/local resource semantics @mgrs@/@mlrs@.
makeEnvForall :: [IVarId] -> [(VariableId, Type)] -> SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> TypingEnvironment
makeEnvForall theta gamma qfh =
  let gamma' = Map.fromList [(id, [BindingInfo typ False]) | (id, typ) <- gamma]
   in TypingEnvironment gamma' (Set.fromList theta) [] True 0 qfh

-- | @makeEnv tctx qfh mgrs mlrs@ initializes a typing environment with empty index context typing context @tctx@,
-- solver handle @qfh@, and global/local resource semantics @mgrs@/@mlrs@.
makeEnv :: [(VariableId, Type)] -> SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> TypingEnvironment
makeEnv = makeEnvForall []

-- | @emptyEnv qfh mgrs mlrs@ initializes an empty typing environment with solver handle @qfh@,
-- and global/local resource semantics @mgrs@/@mlrs@.
emptyEnv ::  SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> TypingEnvironment
emptyEnv = makeEnv []

-- | @envIsLinear env@ returns 'True' if the environment @env@ contains any linear variables.
envIsLinear :: TypingEnvironment -> Bool
envIsLinear TypingEnvironment {typingContext = gamma} =
  let remainingVars = [id | (id, bs) <- Map.toList gamma, any mustBeUsed bs] -- remaining linear variables
   in not (null remainingVars)