module Lang.Type.Semantics where

import Index.AST
import Index.Semantics
import Lang.Type.AST
import Solving.CVC5 (SolverHandle)
import Control.Monad (zipWithM)


-- | @simplifyType t@ returns type @t@ in which all index annotations have been simplified
-- to a normal form according to 'simplifyIndex'.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
simplifyType :: SolverHandle -> Type -> IO Type
simplifyType qfh (TTensor ts) = TTensor <$> mapM (simplifyType qfh) ts
simplifyType qfh (TArrow t1 t2 i j) = TArrow <$> simplifyType qfh t1 <*> simplifyType qfh t2 <*> simplifyIndexStrong qfh i <*> simplifyIndexStrong qfh j
simplifyType qfh (TBang t) = TBang <$> simplifyType qfh t
simplifyType qfh (TList i t) = TList <$> simplifyIndexStrong qfh i <*> simplifyType qfh t
simplifyType qfh (TCirc i inBtype outBtype) = TCirc <$> simplifyIndexStrong qfh i <*> pure inBtype <*> pure outBtype
simplifyType qfh (TIForall id t i j) = TIForall id <$> simplifyType qfh t <*> simplifyIndexStrong qfh i <*> simplifyIndexStrong qfh j
simplifyType _ t = return t

-- Θ ⊢ t1 <: t2 (Figure 15)
-- | @checkSubtype qfh t1 t2@ checks if type @t1@ is a subtype of type @t2@.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkSubtype :: SolverHandle -> Type -> Type -> IO Bool
checkSubtype _ TUnit TUnit = return True
checkSubtype _ (TWire wtype1) (TWire wtype2) = return $ wtype1 == wtype2
checkSubtype qfh (TBang t) (TBang t') = checkSubtype qfh t t'
checkSubtype qfh (TTensor ts) (TTensor ts')
  | length ts == length ts' = do
    cs <- zipWithM (checkSubtype qfh) ts ts'
    return $ and cs
  | otherwise = return False
checkSubtype qfh (TArrow t1 t2 i j) (TArrow t1' t2' i' j') = do
  c1 <- checkSubtype qfh t1' t1
  c2 <- checkSubtype qfh t2 t2'
  c3 <- checkLeq qfh i i'
  c4 <- checkEq qfh j j'
  return $ c1 && c2 && c3 && c4
checkSubtype qfh (TCirc i t1 t2) (TCirc i' t1' t2') = do
  c1 <- checkLeq qfh i i'
  c2 <- checkSubtype qfh t1' t1
  c3 <- checkSubtype qfh t2 t2'
  return $ c1 && c2 && c3
checkSubtype qfh (TList i t) (TList i' t') = do
  c1 <- checkEq qfh i i'
  c2 <- checkSubtype qfh t t'
  return $ c1 && c2
checkSubtype qfh (TIForall id t i j) (TIForall id' t' i' j') =
  let fid = fresh id [i, j, IndexVariable id', i', j']
      fid' = fresh fid [t, t'] -- must do this in two steps since t and t' cannot be put in the same list above
   in do
    c1 <- checkSubtype qfh (isub (IndexVariable fid') id t) (isub (IndexVariable fid') id' t')
    c2 <- checkLeq qfh (isub (IndexVariable fid') id i) (isub (IndexVariable fid') id' i')
    c3 <- checkEq qfh (isub (IndexVariable fid') id j) (isub (IndexVariable fid') id' j')
    return $ c1 && c2 && c3
checkSubtype _ _ _ = return False
