module Solver.SMT (
  querySMT,
  SolverHandle,
  withSolver
) where

import qualified Solver.SMT.CVC5 as CVC5
import Solver.Constraint

type SolverHandle = CVC5.SolverHandle

querySMT :: SolverHandle -> [Constraint] -> Constraint -> IO Bool
querySMT = CVC5.querySMT

withSolver :: Maybe FilePath -> (SolverHandle -> IO r) -> IO r
withSolver = CVC5.withSolver