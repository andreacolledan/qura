module Solver
  ( checkEq,
    checkLeq,
    checkGRLeq,
    checkLRLeq,
    checkGREq,
    checkLREq,
    -- re-exports
    SolverHandle,
    Constraint(..),
    withSolver
  )
where

import PQ.Index
import Solver.Constraint
import Solver.SMT
import qualified Data.HashSet as Set
import Metric
import Analyzer.Unify
import Eval.Index
import Panic


-- Fundamental checking functions

-- | @checkLeq cs qfh i j@ checks if index expression @i@ is lesser-or-equal than index expression @j@
-- for all assignments of their free index variables, under the assumption of constraints @cs@.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkLeq :: [Constraint] -> SolverHandle -> Index -> Index -> IO Bool
checkLeq cs qfh i j = do
  i' <- evalIndex qfh i
  j' <- evalIndex qfh j
  case (i', j') of
    (i', j') | i' == j' -> return True -- identical indices are lesser-or-equal
    (Number n, Number m) -> return $ n <= m -- number indices are lesser-or-equal iff their values are lesser-or-equal
    (i', j') -> querySMT qfh cs $ Leq i' j' -- in all other cases, query the solver

-- | @checkEq cs qfh i j@ checks if index expressions @i@ and @j@ are equal
-- for all assignments of their free index variables, under the assumption of constraints @cs@.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkEq :: [Constraint] -> SolverHandle -> Index -> Index -> IO Bool
checkEq cs qfh i j = do
  i' <- evalIndex qfh i
  j' <- evalIndex qfh j
  case (i', j') of
    (i', j') | i' == j' -> return True -- identical indices are equal
    (i', j') | Set.null (ifv i') && Set.null (ifv j') -> return False -- if both indices are closed and not equal, they are not equal
    (i', j') -> querySMT qfh cs $ Eq i' j' -- in all other cases, query the solver


-- Resource-specialized checking functions

-- | @checkGRLeq cs qfh grs i j@ is like 'checkLeq', but defaults to true if global resource module @grs@ is not given.
checkGRLeq :: [Constraint] -> SolverHandle -> Maybe GlobalMetricModule -> Maybe Index -> Maybe Index -> IO Bool
checkGRLeq _ _ Nothing _ _ = return True
checkGRLeq cs qfh (Just grs) (Just i) (Just j) = checkLeq cs qfh (desugarIndex (Just grs) Nothing i) (desugarIndex (Just grs) Nothing j)
checkGRLeq _ _ (Just _) _ _ = missingGlobalResourceAnnotationPanic "checkGRLeq"

-- | @checkGREq cs qfh grs i j@ is like 'checkEq', but defaults to true if global resource module @grs@ is not given.
checkGREq :: [Constraint] -> SolverHandle -> Maybe GlobalMetricModule -> Maybe Index -> Maybe Index -> IO Bool
checkGREq _ _ Nothing _ _ = return True
checkGREq cs qfh (Just grs) (Just i) (Just j) = checkEq cs qfh (desugarIndex (Just grs) Nothing i) (desugarIndex (Just grs) Nothing j)
checkGREq _ _ (Just _) _ _ = missingGlobalResourceAnnotationPanic "checkGREq"

-- | @checkLRLeq cs qfh lrs i j@ is like 'checkLeq', but defaults to true if local resource module @lrs@ is not given.
checkLRLeq :: [Constraint] -> SolverHandle -> Maybe LocalMetricModule -> Maybe Index -> Maybe Index -> IO Bool
checkLRLeq _ _ Nothing _ _ = return True
checkLRLeq cs qfh (Just lrs) (Just i) (Just j) = checkLeq cs qfh (desugarIndex Nothing (Just lrs) i) (desugarIndex Nothing (Just lrs) j)
checkLRLeq _ _ (Just _) _ _ = missingLocalResourceAnnotationPanic "checkLRLeq"

-- | @checkLREq cs qfh lrs i j@ is like 'checkEq', but defaults to true if local resource module @lrs@ is not given.
checkLREq :: [Constraint] -> SolverHandle -> Maybe LocalMetricModule -> Maybe Index -> Maybe Index -> IO Bool
checkLREq _ _ Nothing _ _ = return True
checkLREq cs qfh (Just lrs) (Just i) (Just j) = checkEq cs qfh (desugarIndex Nothing (Just lrs) i) (desugarIndex Nothing (Just lrs) j)
checkLREq _ _ (Just _) _ _ = missingLocalResourceAnnotationPanic "checkLREq"

