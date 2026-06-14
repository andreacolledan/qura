module Interpreter.RuntimeError
  ( RuntimeError (..),
  )
where

-- for now I always return RuntimeError with a simple desc, laer we will create the different errors
data RuntimeError = 
  RuntimeError String
  deriving Show

-- module Interpreter.RuntimeError where

-- import PQ.Expr
-- import PQ.Index (Index)
-- import PQ.Type (Type)
-- import PrettyPrinter (Pretty (pretty))

-- import Data.List (intercalate)
-- import Control.Monad.Except (ExceptT(..), throwError, runExceptT, catchError)
-- import Control.Monad.State  (State, evalState, get, modify)

-- -- The datatype of errors that can occur during runtime evaluation
-- data RuntimeError
--   = -- Variable and binding errors
--     UnboundVariable VariableId [Expr]
--   | UnassignedVariable VariableId [Expr]
--   | RecursiveDefinition VariableId [Expr]
--   | -- Type reduction errors  
--     ExpectedAbstraction Expr Expr [Expr]
--   | ExpectedLift Expr Expr [Expr]
--   | ExpectedCircuitOrConstant Expr Expr [Expr]
--   | ExpectedIndexAbstraction Expr Expr [Expr]
--   | ExpectedConstant Expr Expr [Expr]
--   | -- Index evaluation errors
--     IndexNotNumber Index [Expr]
--   | -- Conversion errors
--     CannotConvertToWireBundle Expr [Expr]
--   | -- Module and definition errors
--     NoMainFunction String [Expr]
--   | NoDefinitionsInModule String [Expr]
--   | MultipleDefinitions VariableId [String] [Expr]
--   | -- Unsupported features
--     UnsupportedConstruct String Expr [Expr]
--   | -- General evaluation error with context
--     EvaluationError String [Expr]
--   deriving (Eq)

-- instance Show RuntimeError where
--   show (UnboundVariable id stack) = 
--     "* Unbound variable '" ++ id ++ "'" ++ printEvaluationStack stack
--   show (UnassignedVariable id stack) = 
--     "* Variable '" ++ id ++ "' has not been assigned to any value" ++ printEvaluationStack stack
--   show (RecursiveDefinition id stack) = 
--     "* Definition '" ++ id ++ "' is defined using itself recursively" ++ printEvaluationStack stack
--   show (ExpectedAbstraction expected actual stack) = 
--     "* Expected abstraction but got '" ++ trnc 80 (pretty actual) ++ "'" ++ printEvaluationStack stack
--   show (ExpectedLift expected actual stack) = 
--     "* Expected lift but got '" ++ trnc 80 (pretty actual) ++ "'" ++ printEvaluationStack stack
--   show (ExpectedCircuitOrConstant expected actual stack) = 
--     "* Expected circuit or constant but got '" ++ trnc 80 (pretty actual) ++ "'" ++ printEvaluationStack stack
--   show (ExpectedIndexAbstraction expected actual stack) = 
--     "* Expected index abstraction but got '" ++ trnc 80 (pretty actual) ++ "'" ++ printEvaluationStack stack
--   show (ExpectedConstant expected actual stack) = 
--     "* Expected constant but got '" ++ trnc 80 (pretty actual) ++ "'" ++ printEvaluationStack stack
--   show (IndexNotNumber idx stack) = 
--     "* Index '" ++ pretty idx ++ "' did not reduce to a number" ++ printEvaluationStack stack
--   show (CannotConvertToWireBundle expr stack) = 
--     "* Cannot convert expression '" ++ trnc 80 (pretty expr) ++ "' to wire bundle" ++ printEvaluationStack stack
--   show (NoMainFunction moduleName stack) = 
--     "* No main function found in module '" ++ moduleName ++ "'" ++ printEvaluationStack stack
--   show (NoDefinitionsInModule moduleName stack) = 
--     "* No definitions found in module '" ++ moduleName ++ "'" ++ printEvaluationStack stack
--   show (MultipleDefinitions varId modules stack) = 
--     "* Multiple definitions found for '" ++ varId ++ "' in modules: " ++ 
--     intercalate ", " modules ++ printEvaluationStack stack
--   show (UnsupportedConstruct feature expr stack) = 
--     "* Unsupported construct '" ++ feature ++ "' in expression '" ++ 
--     trnc 80 (pretty expr) ++ "'" ++ printEvaluationStack stack
--   show (EvaluationError msg stack) = 
--     "* Evaluation error: " ++ msg ++ printEvaluationStack stack

-- -- Helper functions similar to TypeError
-- printEvaluationStack :: [Expr] -> String
-- printEvaluationStack [] = ""
-- printEvaluationStack (e : es) = "\n* While evaluating " ++ pretty e ++ go es 3
--   where
--     go :: [Expr] -> Int -> String
--     go [] _ = ""
--     go _ 0 = "\n..."
--     go (e : es) n = "\n  In " ++ trnc 80 (pretty e) ++ go es (n - 1)

-- trnc :: Int -> String -> String
-- trnc n s = if length s > n then take n s ++ "..." else s

-- -- evaluation env

-- type IntrepretingResult a = ExceptT RuntimeError (State [Expr]) a

-- -- Run IntrepretingResult, producing Either RuntimeError a (stack starts empty)
-- runEval :: IntrepretingResult a -> Either RuntimeError a
-- runEval m = evalState (runExceptT m) []

-- -- Stack helpers
-- push :: Expr -> IntrepretingResult ()
-- push e = modify (e :)

-- pop :: IntrepretingResult ()
-- pop = modify (\s -> case s of [] -> []; (_:xs) -> xs)

-- getStack :: IntrepretingResult [Expr]
-- getStack = get

-- -- Throw an error that captures the current stack
-- throwWithStack :: ([Expr] -> RuntimeError) -> IntrepretingResult a
-- throwWithStack mk = getStack >>= (throwError . mk)

-- -- Convenience for generic messages
-- throwMsg :: String -> IntrepretingResult a
-- throwMsg msg = throwWithStack (EvaluationError msg)

-- -- Automatically push the current expression, run the action, and pop,
-- -- even if an error occurs inside.
-- withFrame :: Expr -> IntrepretingResult a -> IntrepretingResult a
-- withFrame e action = do
--   push e
--   action `catchError` \er -> pop >> throwError er
--   <* pop