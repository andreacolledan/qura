module Index.Semantics
  ( simplifyIndex,
    checkEq,
    checkLeq,
  )
where

import Index.AST
import Solving.CVC5
import System.IO.Extra (Handle)
import qualified Data.Set as Set

-- Index semantics in terms of a reduction function
-- simplifyIndex qfh i reduces i as much as possible and returns the result
-- If i is closed, then the result is in the form (Number n) for some n
-- If i is not closed, then some tactics are employed to simplify the expression,
-- but the result may still contain free variables
simplifyIndex :: Handle -> Index -> Index
simplifyIndex _ (Number n) = Number n
simplifyIndex _ (IndexVariable id) = IndexVariable id
simplifyIndex qfh (Plus i j) = case (simplifyIndex qfh i, simplifyIndex qfh j) of
  (Number n, Number m) -> Number (n + m)
  (i', Number 0) -> i' -- zero is right identity
  (Number 0, j') -> j' -- zero is left identity
  (i', j') -> Plus i' j' -- do not reduce further
simplifyIndex qfh (Max i j) = case (simplifyIndex qfh i, simplifyIndex qfh j) of
  (Number n, Number m) -> Number (max n m)
  (i', Number 0) -> i' -- zero is right identity
  (Number 0, j') -> j' -- zero is left identity
  (i', j') | checkEq qfh i' j' -> i' -- idempotent
  (i', j') -> Max i' j' -- do not reduce further
simplifyIndex qfh (Mult i j) = case (simplifyIndex qfh i, simplifyIndex qfh j) of
  (Number n, Number m) -> Number (n * m)
  (_, Number 0) -> Number 0 -- zero is right absorbing
  (Number 0, _) -> Number 0 -- zero is left absorbing
  (i', Number 1) -> i' -- one is right identity
  (Number 1, j') -> j' -- one is left identity
  (i', j') -> Mult i' j' -- do not reduce further
simplifyIndex qfh (Minus i j) = case (simplifyIndex qfh i, simplifyIndex qfh j) of
  (Number n, Number m) -> Number (max 0 (n - m))
  (i', Number 0) -> i' -- zero is right identity
  (Number 0, _) -> Number 0 -- zero is left absorbing
  (i', j') | checkEq qfh i' j' -> Number 0 -- i - i = 0 for all i
  (i', j') -> Minus i' j' -- do not reduce further
simplifyIndex qfh (Maximum id i j) = case simplifyIndex qfh i of
  -- if upper bound is 0, the range is empty and the maximum defaults to 0
  Number 0 -> Number 0
  -- if the upper bound is known, unroll the maximum into a sequence of binary maxima
  Number n ->
    let unrolling = foldr1 Max [isub (Number step) id (simplifyIndex qfh j) | step <- [0 .. n - 1]]
     in simplifyIndex qfh unrolling
  -- if the upper bound is not known, do not reduce further
  i' -> Maximum id i' (simplifyIndex qfh j)
  

-- Θ ⊨ i = j (figs. 10,15)
-- in this implementation, Θ implicitly contains all the free variables in i and j
checkEq :: Handle -> Index -> Index -> Bool
checkEq qfh i j = case (simplifyIndex qfh i, simplifyIndex qfh j) of
  (i', j') | i' == j' -> True -- identical indices are equal
  (i', j') | Set.null (ifv i') && Set.null (ifv j') -> False -- if both indices are closed and not equal, they are not equal
  (i', j') -> querySMTWithContext qfh $ Constraint Eq i' j' -- in all other cases, query the solver

-- Θ ⊨ i ≤ j (figs. 12,15)
-- in this implementation, Θ implicitly contains all the free variables in i and j
checkLeq :: Handle -> Index -> Index -> Bool
checkLeq qfh i j = case (simplifyIndex qfh i, simplifyIndex qfh j) of
  (i', j') | i' == j' -> True -- identical indices are lesser-or-equal
  (Number n, Number m) -> n <= m -- number indices are lesser-or-equal iff their values are lesser-or-equal
  (i', j') -> querySMTWithContext qfh $ Constraint Leq i' j' -- in all other cases, query the solver
