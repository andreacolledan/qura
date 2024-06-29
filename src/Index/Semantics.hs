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
import Index.Semantics.Resource
import PrettyPrinter

toNumber :: Index -> Maybe Int
toNumber (Number n) = Just n
toNumber _ = Nothing

simplifyIndex :: SolverHandle -> Maybe GlobalResourceSemantics -> Maybe LocalResourceSemantics -> Index -> IO Index
simplifyIndex qfh grs lrs i = evalIndex qfh (desugarIndex grs lrs i)

desugarIndex :: Maybe GlobalResourceSemantics -> Maybe LocalResourceSemantics -> Index -> Index
desugarIndex _ _ (Number n) = Number n
desugarIndex _ _ (IndexVariable id) = IndexVariable id
desugarIndex mgrs lrs (Plus i j) = Plus (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex mgrs lrs (Max i j) = Max (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex mgrs lrs (Mult i j) = Mult (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex mgrs lrs (Minus i j) = Minus (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex mgrs lrs (BoundedMax id i j) = BoundedMax id (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex mgrs lrs (BoundedSum id i j) = BoundedSum id (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex (Just grs) _ Identity = desugarIdentity grs
desugarIndex (Just grs) _ (Wire wt) = desugarWire grs wt
desugarIndex mgrs@(Just grs) lrs (Sequence i j) = desugarSequence grs (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex mgrs@(Just grs) lrs (Parallel i j) = desugarParallel grs (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex mgrs@(Just grs) lrs (BoundedSequence id i j) = desugarBoundedSequence grs id (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex mgrs@(Just grs) lrs (BoundedParallel id i j) = desugarBoundedParallel grs id (desugarIndex mgrs lrs i) (desugarIndex mgrs lrs j)
desugarIndex grs mlrs@(Just lrs) (OpOutput op n is) = desugarOutput lrs op n (desugarIndex grs mlrs <$> is)
desugarIndex _ _ i = error $ "Internal error: resource operator was not desugared (desugarIndex): " ++ pretty i

-- | @simplifyIndexStrong grs lrs qfh i@ returns index expression @i@ in a normal form.
-- Note that this might not be a natural number (e.g. if @i@ contains free variables).
-- 'SolverHandle' @qfh@ is used to interact with the SMT solver.
evalIndex :: SolverHandle -> Index -> IO Index
evalIndex _ (Number n) = return $ Number n
evalIndex _ (IndexVariable id) = return $ IndexVariable id
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
      c <- checkEq qfh i' j'
      return $ if c
        then i'          -- idemptotent
        else Max i' j'   -- do not reduce further
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
      c <- checkEq qfh i' j'
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
      elems <- sequence [evalIndex qfh (isub (Number step) id j) | step <- [0 .. n - 1]]
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
      elems <- sequence [evalIndex qfh (isub (Number step) id j) | step <- [0 .. n - 1]]
      let unrolling = foldr1 Plus elems
      evalIndex qfh unrolling
    i' -> do
      j' <- evalIndex qfh j
      if id `Set.member` ifv j'
        then return $ BoundedSum id i' j' -- do not reduce further
        else evalIndex qfh $ Mult i' j' --use shortcut
--evalIndex qfh grs lrs (OpOutput op n is) = do
--  let lrs' = fromMaybe (error "Internal: uninterpretable index escaped parser") lrs
--  is <- mapM (evalIndex qfh grs lrs) is
--  case mapM toNumber is of
--    Just ns -> return $ Number $ outputInterpretation lrs' op n ns
--    Nothing -> return $ OpOutput op n is
--evalIndex _ grs _ Identity = do
--  let grs' = fromMaybe (error "Internal: uninterpretable index escaped parser") grs
--  return $ Number $ interpretIdentity grs'
--evalIndex _ grs _ (Wire wt) = do
--  let grs' = fromMaybe (error "Internal: uninterpretable index escaped parser") grs
--  return $ Number $ interpretWire grs' wt
--evalIndex qfh grs lrs (Sequence i j) = do
--  let grs' = fromMaybe (error "Internal: uninterpretable index escaped parser") grs
--  i <- evalIndex qfh grs lrs i
--  j <- evalIndex qfh grs lrs j
--  case (i,j) of
--    (Number n, Number m) -> do
--      return $ Number $ interpretSequence grs' n m
--    (Number e, j') | e == interpretIdentity grs' -> return j'  -- Identity is left identity for sequence
--    (i', Number e) | e == interpretIdentity grs' -> return i' -- Identity is right identity for sequence
--    (i',j') -> return $ Sequence i' j'
--evalIndex qfh grs lrs (Parallel i j) = do
--  let grs' = fromMaybe (error "Internal: uninterpretable index escaped parser") grs
--  i <- evalIndex qfh grs lrs i
--  j <- evalIndex qfh grs lrs j
--  case (i,j) of
--    (Number n, Number m) -> do
--      return $ Number $ interpretParallel grs' n m
--    (Number e, j') | e == interpretIdentity grs' -> return j'  -- Identity is left identity for parallel
--    (i', Number e) | e == interpretIdentity grs' -> return i' -- Identity is right identity for parallel
--    (i',j') -> return $ Parallel i' j'
--evalIndex qfh grs lrs (BoundedSequence id i j) = do
--  let grs' = fromMaybe (error "Internal: uninterpretable index escaped parser") grs
--  i <- evalIndex qfh grs lrs i
--  case i of
--    -- if upper bound is 0, the range is empty and the bounded sequence defaults to 0
--    Number 0 -> return $ Number $ interpretIdentity grs'
--    -- if the upper bound is known, unroll the operator into a sequence of binary sequences
--    Number n -> do
--      unrolling <- foldr1 Sequence <$> sequence [evalIndex qfh grs lrs (isub (Number step) id j) | step <- [0 .. n - 1]]
--      evalIndex qfh grs lrs unrolling
--      -- if the upper bound is not known, do not reduce further
--    i' -> do
--      j' <- evalIndex qfh grs lrs j
--      return $ BoundedSequence id i' j'
--evalIndex qfh grs lrs (BoundedParallel id i j) = do
--  let grs' = fromMaybe (error "Internal: uninterpretable index escaped parser") grs
--  i <- evalIndex qfh grs lrs i
--  case i of
--    -- if upper bound is 0, the range is empty and the bounded parallel defaults to 0
--    Number 0 -> return $ Number $ interpretIdentity grs'
--    -- if the upper bound is known, unroll the operator into a sequence of binary parallels
--    Number n -> do
--      unrolling <- foldr1 Parallel <$> sequence [evalIndex qfh grs lrs (isub (Number step) id j) | step <- [0 .. n - 1]]
--      evalIndex qfh grs lrs unrolling
--      -- if the upper bound is not known, do not reduce further
--    i' -> do
--      j' <- evalIndex qfh grs lrs j
--      return $ BoundedParallel id i' j'
evalIndex _ i = error $ "Internal error: resource operator was not desugared (evalIndex): " ++ pretty i 

-- Fundamental checking functions

-- Θ ⊨ i ≤ j (figs. 12,15)
-- | @checkLeq qfh i j@ checks if index expression @i@ is lesser-or-equal than index expression @j@
-- for all assignments of their free index variables.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkLeq :: SolverHandle -> Index -> Index -> IO Bool
checkLeq qfh i j = do
  i' <- evalIndex qfh i
  j' <- evalIndex qfh j
  case (i', j') of
    (i', j') | i' == j' -> return True -- identical indices are lesser-or-equal
    (Number n, Number m) -> return $ n <= m -- number indices are lesser-or-equal iff their values are lesser-or-equal
    (i', j') -> querySMTWithContext qfh $ Constraint Leq i' j' -- in all other cases, query the solver

-- Θ ⊨ i = j (figs. 10,15)
-- | @checkEq qfh i j@ checks if index expressions @i@ and @j@ are equal
-- for all assignments of their free index variables.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkEq :: SolverHandle -> Index -> Index -> IO Bool
checkEq qfh i j = do
  i' <- evalIndex qfh i
  j' <- evalIndex qfh j
  case (i', j') of
    (i', j') | i' == j' -> return True -- identical indices are equal
    (i', j') | Set.null (ifv i') && Set.null (ifv j') -> return False -- if both indices are closed and not equal, they are not equal
    (i', j') -> querySMTWithContext qfh $ Constraint Eq i' j' -- in all other cases, query the solver

checkGRLeq :: SolverHandle -> Maybe GlobalResourceSemantics -> Maybe Index -> Maybe Index -> IO Bool
checkGRLeq _ Nothing _ _ = return True
checkGRLeq qfh (Just grs) (Just i) (Just j) = checkLeq qfh (desugarIndex (Just grs) Nothing i) (desugarIndex (Just grs) Nothing j)
checkGRLeq _ (Just _) _ _ = error "Internal error: missing index in global resource annotation (checkGRLeq)."

checkGREq :: SolverHandle -> Maybe GlobalResourceSemantics -> Maybe Index -> Maybe Index -> IO Bool
checkGREq _ Nothing _ _ = return True
checkGREq qfh (Just grs) (Just i) (Just j) = checkEq qfh (desugarIndex (Just grs) Nothing i) (desugarIndex (Just grs) Nothing j)
checkGREq _ (Just _) _ _ = error "Internal error: missing index in global resource annotation (checkGREq)."

checkLRLeq :: SolverHandle -> Maybe LocalResourceSemantics -> Maybe Index -> Maybe Index -> IO Bool
checkLRLeq _ Nothing _ _ = return True
checkLRLeq qfh (Just lrs) (Just i) (Just j) = checkLeq qfh (desugarIndex Nothing (Just lrs) i) (desugarIndex Nothing (Just lrs) j)
checkLRLeq _ (Just _) _ _ = error "Internal error: missing index in global resource annotation (checkLRLeq)."

checkLREq :: SolverHandle -> Maybe LocalResourceSemantics -> Maybe Index -> Maybe Index -> IO Bool
checkLREq _ Nothing _ _ = return True
checkLREq qfh (Just lrs) (Just i) (Just j) = checkEq qfh (desugarIndex Nothing (Just lrs) i) (desugarIndex Nothing (Just lrs) j)
checkLREq _ (Just _) _ _ = error "Internal error: missing index in global resource annotation (checkLREq)."

-- MAYBE variants

maybeSimplifyIndex :: SolverHandle -> Maybe GlobalResourceSemantics -> Maybe LocalResourceSemantics -> Maybe Index -> IO (Maybe Index)
maybeSimplifyIndex _ _ _ Nothing = return Nothing
maybeSimplifyIndex qfh grs lrs (Just i) = Just <$> simplifyIndex qfh grs lrs i