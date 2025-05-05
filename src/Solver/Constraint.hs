module Solver.Constraint (Constraint(..)) where

import PQ.Index
import PrettyPrinter
  
  
-- | The datatype of index constraints
data Constraint = Eq Index Index | Leq Index Index
  deriving (Show, Eq)

instance Pretty Constraint where
  pretty (Eq i j) = pretty i ++ " = " ++ " " ++ pretty j
  pretty (Leq i j) = pretty i ++ " <= " ++ pretty j

