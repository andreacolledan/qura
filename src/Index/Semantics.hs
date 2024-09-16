module Index.Semantics
  ( evalIndex,
    checkEq,
    checkLeq,
    simplifyIndex,
    maybeSimplifyIndex,
    checkGRLeq,
    checkLRLeq,
    checkGREq,
    checkLREq
  )
where

import Index.AST
import Solving.CVC5
import qualified Data.HashSet as Set
import Index.Semantics.Global.Resource
import Index.Semantics.Local.Resource
import PrettyPrinter
import Index.Unify

-- | @toNumber i@ returns the natural number represented by index expression @i@ if it is a number.
toNumber :: Index -> Maybe Int
toNumber (Number n) = Just n
toNumber _ = Nothing

-- | @desugarIndex mgrs mlrs i@ desugars the abstract resource operations in index expression @i@
-- according to global resource module mgrs@ and local resource module @mlrs@.
desugarIndex :: Maybe GlobalMetricModule -> Maybe LocalMetricModule -> Index -> Index
desugarIndex _ _ (Number n) = Number n
desugarIndex _ _ (IVar id) = IVar id
desugarIndex mgrs mlrs (Plus i j) = Plus (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs mlrs (Max i j) = Max (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs mlrs (Mult i j) = Mult (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs mlrs (Minus i j) = Minus (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs mlrs (BoundedMax id i j) = BoundedMax id (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs mlrs (BoundedSum id i j) = BoundedSum id (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex (Just grs) _ Identity = desugarIdentity grs
desugarIndex (Just grs) _ (Wire wt) = desugarWire grs wt
desugarIndex (Just grs) _ (Operation op) = desugarOperation grs op
desugarIndex mgrs@(Just grs) mlrs (Sequence i j) = desugarSequence grs (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs@(Just grs) mlrs (Parallel i j) = desugarParallel grs (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs@(Just grs) mlrs (BoundedSequence id i j) = desugarBoundedSequence grs id (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs@(Just grs) mlrs (BoundedParallel id i j) = desugarBoundedParallel grs id (desugarIndex mgrs mlrs i) (desugarIndex mgrs mlrs j)
desugarIndex mgrs mlrs@(Just lrs) (Output op n is) = desugarOutput lrs op n (desugarIndex mgrs mlrs <$> is)
desugarIndex _ _ i = error $ "Internal error: resource operator was not desugared (desugarIndex): " ++ pretty i

-- | @simplifyIndexStrong grs lrs qfh i@ returns index expression @i@ in a normal form.
-- Note that this might not be a natural number (e.g. if @i@ contains free variables).
-- 'SolverHandle' @qfh@ is used to interact with the SMT solver.
evalIndex :: SolverHandle -> Index -> IO Index
evalIndex _ (Number n) = return $ Number n
evalIndex _ (IVar id) = return $ IVar id
evalIndex qfh (Plus i j) = do
  i' <- evalIndex qfh i
  j' <- evalIndex qfh j
  return $ case (i',j') of
    (Number n, Number m) -> Number (n + m)
    (i', Number 0) -> i'    -- zero is right identity
    (Number 0, j') -> j'    -- zero is left identity
    (i', j') -> Plus i' j'  -- do not reduce further
evalIndex qfh (Max i j) = do
  i' <- evalIndex qfh i
  j' <- evalIndex qfh j
  case (i',j') of
    (Number n, Number m) -> return $ Number (max n m)
    (i', Number 0) -> return i' -- zero is right identity
    (Number 0, j') -> return j' -- zero is left identity
    (i', j') -> do
      -- try to evaluate using solver
      -- note: for some reason, inverting these two checks yields much worse results
      -- do NOT change the order of the checks
      ci <- querySMT qfh [] $ Leq j' i'
      if ci then return i'
        else do
          cj <- querySMT qfh [] $ Leq i' j'
          if cj then return j'
          else return $ Max i' j'   -- do not reduce further
evalIndex qfh (Mult i j) = do
  i' <- evalIndex qfh i
  j' <- evalIndex qfh j
  return $ case (i', j') of
    (Number n, Number m) -> Number (n * m)
    (_, Number 0) -> Number 0 -- zero is right absorbing
    (Number 0, _) -> Number 0 -- zero is left absorbing
    (i', Number 1) -> i'      -- one is right identity
    (Number 1, j') -> j'      -- one is left identity
    (i', j') -> Mult i' j'    -- do not reduce further
evalIndex qfh (Minus i j) = do
  i' <- evalIndex qfh i
  j' <- evalIndex qfh j
  case (i',j') of
    (Number n, Number m) -> return $ Number (max 0 (n - m))
    (i', Number 0) -> return i' -- zero is right identity
    (Number 0, _) -> return $ Number 0 -- zero is left absorbing
    (i',j') -> do
      c <- querySMT qfh [] $ Eq i' j'
      return $ if c
        then Number 0      -- equal terms cancel each other out
        else Minus i' j'   -- do not reduce further
evalIndex qfh (BoundedMax id i j) = do
  i' <- evalIndex qfh i
  case i' of
    -- if upper bound is 0, the range is empty and the maximum defaults to 0
    Number 0 -> return $ Number 0
    -- if the upper bound is known, unroll the maximum into a sequence of binary maxima
    Number n -> do
      elems <- sequence [evalIndex qfh (isub (isubSingleton id (Number step)) j) | step <- [0 .. n - 1]]
      let unrolling = foldr1 Max elems
      evalIndex qfh unrolling
    i' -> do
      j' <- evalIndex qfh j
      if id `Set.member` ifv j'
        then return $ BoundedMax id i' j' -- do not reduce further
        else evalIndex qfh j' --use shortcut
evalIndex qfh (BoundedSum id i j) = do
  i' <- evalIndex qfh i
  case i' of
    -- if upper bound is 0, the range is empty and the sum defaults to 0
    Number 0 -> return $ Number 0
    -- if the upper bound is known, unroll the bounded sum into a sequence of binary sums
    Number n -> do
      elems <- sequence [evalIndex qfh (isub (isubSingleton id (Number step)) j) | step <- [0 .. n - 1]]
      let unrolling = foldr1 Plus elems
      evalIndex qfh unrolling
    i' -> do
      j' <- evalIndex qfh j
      if id `Set.member` ifv j'
        then return $ BoundedSum id i' j' -- do not reduce further
        else evalIndex qfh $ Mult i' j' --use shortcut
evalIndex _ i = error $ "Internal error: resource operator was not desugared (evalIndex): " ++ pretty i 

-- | @simplifyIndex qfh grs lrs i@ desugars index expression @i@ and then simplifies it to a normal form.
-- 'SolverHandle' @qfh@ is used to interact with the SMT solver.
simplifyIndex :: SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> Index -> IO Index
simplifyIndex qfh grs lrs i = evalIndex qfh (desugarIndex grs lrs i)

-- | @maybeSimplifyIndex qfh grs lrs i@ is like 'simplifyIndex', but defaults to `Nothing`  if index expression @i@ is `Nothing`.
maybeSimplifyIndex :: SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> Maybe Index -> IO (Maybe Index)
maybeSimplifyIndex _ _ _ Nothing = return Nothing
maybeSimplifyIndex qfh grs lrs (Just i) = Just <$> simplifyIndex qfh grs lrs i


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
checkGRLeq _ _ (Just _) _ _ = error "Internal error: missing index in global resource annotation (checkGRLeq)."

-- | @checkGREq cs qfh grs i j@ is like 'checkEq', but defaults to true if global resource module @grs@ is not given.
checkGREq :: [Constraint] -> SolverHandle -> Maybe GlobalMetricModule -> Maybe Index -> Maybe Index -> IO Bool
checkGREq _ _ Nothing _ _ = return True
checkGREq cs qfh (Just grs) (Just i) (Just j) = checkEq cs qfh (desugarIndex (Just grs) Nothing i) (desugarIndex (Just grs) Nothing j)
checkGREq _ _ (Just _) _ _ = error "Internal error: missing index in global resource annotation (checkGREq)."

-- | @checkLRLeq cs qfh lrs i j@ is like 'checkLeq', but defaults to true if local resource module @lrs@ is not given.
checkLRLeq :: [Constraint] -> SolverHandle -> Maybe LocalMetricModule -> Maybe Index -> Maybe Index -> IO Bool
checkLRLeq _ _ Nothing _ _ = return True
checkLRLeq cs qfh (Just lrs) (Just i) (Just j) = checkLeq cs qfh (desugarIndex Nothing (Just lrs) i) (desugarIndex Nothing (Just lrs) j)
checkLRLeq _ _ (Just _) _ _ = error "Internal error: missing index in global resource annotation (checkLRLeq)."

-- | @checkLREq cs qfh lrs i j@ is like 'checkEq', but defaults to true if local resource module @lrs@ is not given.
checkLREq :: [Constraint] -> SolverHandle -> Maybe LocalMetricModule -> Maybe Index -> Maybe Index -> IO Bool
checkLREq _ _ Nothing _ _ = return True
checkLREq cs qfh (Just lrs) (Just i) (Just j) = checkEq cs qfh (desugarIndex Nothing (Just lrs) i) (desugarIndex Nothing (Just lrs) j)
checkLREq _ _ (Just _) _ _ = error "Internal error: missing index in global resource annotation (checkLREq)."

