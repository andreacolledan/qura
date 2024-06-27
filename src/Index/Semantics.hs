module Index.Semantics
  ( evalIndex,
    checkEq,
    checkLeq,
    simplifyIndex,
    maybeEvalIndex,
    maybeSimplifyIndex,
    maybeCheckEq,
    maybeCheckLeq,
  )
where

import Index.AST
import Solving.CVC5
import qualified Data.HashSet as Set
import Index.Semantics.Resource

toNumber :: Index -> Maybe Int
toNumber (Number n) = Just n
toNumber _ = Nothing

simplifyIndex :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Index -> IO Index
simplifyIndex qfh grs lrs i = evalIndex qfh grs lrs (desugarIndex grs i)

desugarIndex :: GlobalResourceSemantics -> Index -> Index
desugarIndex grs Identity = desugarIdentity grs
desugarIndex grs (Wire wt) = desugarWire grs wt
desugarIndex grs (Sequence i j) = desugarSequence grs (desugarIndex grs i) (desugarIndex grs j)
desugarIndex grs (Parallel i j) = desugarParallel grs (desugarIndex grs i) (desugarIndex grs j)
desugarIndex grs (BoundedSequence id i j) = desugarBoundedSequence grs id (desugarIndex grs i) (desugarIndex grs j)
desugarIndex grs (BoundedParallel id i j) = desugarBoundedParallel grs id (desugarIndex grs i) (desugarIndex grs j)
desugarIndex _ i = i

-- | @simplifyIndexStrong grs lrs qfh i@ returns index expression @i@ in a normal form.
-- Note that this might not be a natural number (e.g. if @i@ contains free variables).
-- 'SolverHandle' @qfh@ is used to interact with the SMT solver.
evalIndex :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Index -> IO Index
evalIndex _ _ _ (Number n) = return $ Number n
evalIndex _ _ _ (IndexVariable id) = return $ IndexVariable id
evalIndex qfh grs lrs (Plus i j) = do
  i' <- evalIndex qfh grs lrs i
  j' <- evalIndex qfh grs lrs j
  return $ case (i',j') of
    (Number n, Number m) -> Number (n + m)
    (i', Number 0) -> i'    -- zero is right identity
    (Number 0, j') -> j'    -- zero is left identity
    (i', j') -> Plus i' j'  -- do not reduce further
evalIndex qfh grs lrs (Max i j) = do
  i' <- evalIndex qfh grs lrs i
  j' <- evalIndex qfh grs lrs j
  case (i',j') of
    (Number n, Number m) -> return $ Number (max n m)
    (i', Number 0) -> return i' -- zero is right identity
    (Number 0, j') -> return j' -- zero is left identity
    (i', j') -> do
      c <- checkEq qfh grs lrs i' j'
      return $ if c
        then i'          -- idemptotent
        else Max i' j'   -- do not reduce further
evalIndex qfh grs lrs (Mult i j) = do
  i' <- evalIndex qfh grs lrs i
  j' <- evalIndex qfh grs lrs j
  return $ case (i', j') of
    (Number n, Number m) -> Number (n * m)
    (_, Number 0) -> Number 0 -- zero is right absorbing
    (Number 0, _) -> Number 0 -- zero is left absorbing
    (i', Number 1) -> i'      -- one is right identity
    (Number 1, j') -> j'      -- one is left identity
    (i', j') -> Mult i' j'    -- do not reduce further
evalIndex qfh grs lrs (Minus i j) = do
  i' <- evalIndex qfh grs lrs i
  j' <- evalIndex qfh grs lrs j
  case (i',j') of
    (Number n, Number m) -> return $ Number (max 0 (n - m))
    (i', Number 0) -> return i' -- zero is right identity
    (Number 0, _) -> return $ Number 0 -- zero is left absorbing
    (i',j') -> do
      c <- checkEq qfh grs lrs i' j'
      return $ if c
        then Number 0      -- equal terms cancel each other out
        else Minus i' j'   -- do not reduce further
evalIndex qfh grs lrs (Maximum id i j) = do
  i' <- evalIndex qfh grs lrs i
  case i' of
    -- if upper bound is 0, the range is empty and the maximum defaults to 0
    Number 0 -> return $ Number 0
    -- if the upper bound is known, unroll the maximum into a sequence of binary maxima
    Number n -> do
      elems <- sequence [evalIndex qfh grs lrs (isub (Number step) id j) | step <- [0 .. n - 1]]
      let unrolling = foldr1 Max elems
      evalIndex qfh grs lrs unrolling
    i' -> do
      j' <- evalIndex qfh grs lrs j
      if id `Set.member` ifv j'
        then return $ Maximum id i' j' -- do not reduce further
        else evalIndex qfh grs lrs j' --use shortcut
evalIndex qfh grs lrs (OpOutput op n is) = do
  is <- mapM (evalIndex qfh grs lrs) is
  case mapM toNumber is of
    Just ns -> return $ Number $ outputInterpretation lrs op n ns
    Nothing -> return $ OpOutput op n is
evalIndex _ grs _ Identity = do
  return $ Number $ interpretIdentity grs
evalIndex _ grs _ (Wire wt) = do
  return $ Number $ interpretWire grs wt
evalIndex qfh grs lrs (Sequence i j) = do
  i <- evalIndex qfh grs lrs i
  j <- evalIndex qfh grs lrs j
  case (i,j) of
    (Number n, Number m) -> do
      return $ Number $ interpretSequence grs n m
    (Number e, j') | e == interpretIdentity grs -> return j'  -- Identity is left identity for sequence
    (i', Number e) | e == interpretIdentity grs -> return i' -- Identity is right identity for sequence
    (i',j') -> return $ Sequence i' j'
evalIndex qfh grs lrs (Parallel i j) = do
  i <- evalIndex qfh grs lrs i
  j <- evalIndex qfh grs lrs j
  case (i,j) of
    (Number n, Number m) -> do
      return $ Number $ interpretParallel grs n m
    (Number e, j') | e == interpretIdentity grs -> return j'  -- Identity is left identity for parallel
    (i', Number e) | e == interpretIdentity grs -> return i' -- Identity is right identity for parallel
    (i',j') -> return $ Parallel i' j'
evalIndex qfh grs lrs (BoundedSequence id i j) = do
  i <- evalIndex qfh grs lrs i
  case i of
    -- if upper bound is 0, the range is empty and the bounded sequence defaults to 0
    Number 0 -> return $ Number $ interpretIdentity grs
    -- if the upper bound is known, unroll the operator into a sequence of binary sequences
    Number n -> do
      unrolling <- foldr1 Sequence <$> sequence [evalIndex qfh grs lrs (isub (Number step) id j) | step <- [0 .. n - 1]]
      evalIndex qfh grs lrs unrolling
      -- if the upper bound is not known, do not reduce further
    i' -> do
      j' <- evalIndex qfh grs lrs j
      if id `Set.member` ifv j'
        then return $ BoundedSequence id i' j' -- do not reduce further
        else evalIndex qfh grs lrs $ desugarIndependentBoundedSequence grs i' j' --use shortcut
evalIndex qfh grs lrs (BoundedParallel id i j) = do
  i <- evalIndex qfh grs lrs i
  case i of
    -- if upper bound is 0, the range is empty and the bounded parallel defaults to 0
    Number 0 -> return $ Number $ interpretIdentity grs
    -- if the upper bound is known, unroll the operator into a sequence of binary parallels
    Number n -> do
      unrolling <- foldr1 Parallel <$> sequence [evalIndex qfh grs lrs (isub (Number step) id j) | step <- [0 .. n - 1]]
      evalIndex qfh grs lrs unrolling
      -- if the upper bound is not known, do not reduce further
    i' -> do
      j' <- evalIndex qfh grs lrs j
      if id `Set.member` ifv j'
        then return $ BoundedParallel id i' j' -- do not reduce further
        else evalIndex qfh grs lrs $ desugarIndependentBoundedParallel grs i' j' --use shortcut


-- Θ ⊨ i = j (figs. 10,15)
-- | @checkEq qfh i j@ checks if index expressions @i@ and @j@ are equal
-- for all assignments of their free index variables.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkEq :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Index -> Index -> IO Bool
checkEq qfh grs lrs i j = do
  i' <- simplifyIndex qfh grs lrs i
  j' <- simplifyIndex qfh grs lrs j
  case (i', j') of
    (i', j') | i' == j' -> return True -- identical indices are equal
    (i', j') | Set.null (ifv i') && Set.null (ifv j') -> return False -- if both indices are closed and not equal, they are not equal
    (i', j') -> querySMTWithContext qfh grs lrs $ Constraint Eq i' j' -- in all other cases, query the solver

-- Θ ⊨ i ≤ j (figs. 12,15)
-- | @checkLeq qfh i j@ checks if index expression @i@ is lesser-or-equal than index expression @j@
-- for all assignments of their free index variables.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkLeq :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Index -> Index -> IO Bool
checkLeq qfh grs lrs i j = do
  i' <- simplifyIndex qfh grs lrs i
  j' <- simplifyIndex qfh grs lrs j
  case (i', j') of
    (i', j') | i' == j' -> return True -- identical indices are lesser-or-equal
    (Number n, Number m) -> return $ n <= m -- number indices are lesser-or-equal iff their values are lesser-or-equal
    (i', j') -> querySMTWithContext qfh grs lrs $ Constraint Leq i' j' -- in all other cases, query the solver

-- MAYBE variants

maybeSimplifyIndex :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Maybe Index -> IO (Maybe Index)
maybeSimplifyIndex _ _ _ Nothing = return Nothing
maybeSimplifyIndex qfh grs lrs (Just i) = Just <$> simplifyIndex qfh grs lrs i

maybeEvalIndex :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Maybe Index -> IO (Maybe Index)
maybeEvalIndex _ _ _ Nothing = return Nothing
maybeEvalIndex qfh grs lrs (Just i) = Just <$> evalIndex qfh grs lrs i

maybeCheckEq :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Maybe Index -> Maybe Index -> IO Bool
maybeCheckEq qfh grs lrs (Just i) (Just j) = checkEq qfh grs lrs i j
maybeCheckEq _ _ _ _ _ = return True -- if either index is missing, the equality holds vacuously

maybeCheckLeq :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Maybe Index -> Maybe Index -> IO Bool
maybeCheckLeq qfh grs lrs (Just i) (Just j) = checkLeq qfh grs lrs i j
maybeCheckLeq _ _ _ _ _ = return True -- if either index is missing, the inequality holds vacuously