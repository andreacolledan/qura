module Lang.Type.Semantics where

import Index.AST
import Index.Semantics
import Lang.Type.AST
import Solving.CVC5 (SolverHandle)
import Control.Monad (zipWithM)
import Index.Semantics.Resource (GlobalResourceSemantics, LocalResourceSemantics)

-- | @simplifyType t@ returns type @t@ in which all index annotations have been simplified
-- to a normal form according to 'simplifyIndexStrong'.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
simplifyType :: SolverHandle -> Maybe GlobalResourceSemantics -> Maybe LocalResourceSemantics -> Type -> IO Type
simplifyType qfh grs lrs (TTensor ts) = TTensor <$> mapM (simplifyType qfh grs lrs) ts
simplifyType qfh grs lrs (TArrow t1 t2 i j) = TArrow <$> simplifyType qfh grs lrs t1 <*> simplifyType qfh grs lrs t2 <*> maybeSimplifyIndex qfh grs lrs i <*> maybeSimplifyIndex qfh grs lrs j
simplifyType qfh grs lrs (TBang i t) = TBang <$> maybeSimplifyIndex qfh grs lrs i <*> simplifyType qfh grs lrs t
simplifyType qfh grs lrs (TList id i t) = TList id <$> simplifyIndex qfh grs lrs i <*> simplifyType qfh grs lrs t
simplifyType qfh grs lrs (TCirc i inBtype outBtype) = TCirc <$> maybeSimplifyIndex qfh grs lrs i <*> pure inBtype <*> pure outBtype
simplifyType qfh grs lrs (TIForall id t i j) = TIForall id <$> simplifyType qfh grs lrs t <*> maybeSimplifyIndex qfh grs lrs i <*> maybeSimplifyIndex qfh grs lrs j
simplifyType _ _ _ t = return t

-- Θ ⊢ t1 <: t2 (Figure 15)
-- | @checkSubtype qfh grs lrs t1 t2@ checks if type @t1@ is a subtype of type @t2@.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkSubtype :: SolverHandle -> Maybe GlobalResourceSemantics -> Maybe LocalResourceSemantics -> Type -> Type -> IO Bool
checkSubtype _ _ _ TUnit TUnit = return True
checkSubtype _ _ _ (TWire wtype1) (TWire wtype2) = return $ wtype1 == wtype2
checkSubtype qfh grs lrs (TBang i1 t1) (TBang i2 t2) = do
  c1 <- checkGRLeq qfh grs i1 i2
  c2 <- checkSubtype qfh grs lrs t1 t2
  return $ c1 && c2
checkSubtype qfh grs lrs (TTensor ts) (TTensor ts')
  | length ts == length ts' = do
    cs <- zipWithM (checkSubtype qfh grs lrs) ts ts'
    return $ and cs
  | otherwise = return False
checkSubtype qfh grs lrs (TArrow t1 t2 i j) (TArrow t1' t2' i' j') = do
  c1 <- checkSubtype qfh grs lrs t1' t1
  c2 <- checkSubtype qfh grs lrs t2 t2'
  c3 <- checkGRLeq qfh grs i i' 
  c4 <- checkGREq qfh grs j j' 
  return $ c1 && c2 && c3 && c4
checkSubtype qfh grs lrs (TCirc i t1 t2) (TCirc i' t1' t2') = do
  c1 <- checkGRLeq qfh grs i i' 
  c2 <- checkSubtype qfh grs lrs t1' t1
  c3 <- checkSubtype qfh grs lrs t2 t2'
  return $ c1 && c2 && c3
checkSubtype qfh grs lrs (TList id i t) (TList id' i' t') =
  let fid = fresh (fresh (fresh id [i, i']) [IndexVariable id']) [t, t']
    in do
      c1 <- checkEq qfh i i'
      c2 <- checkSubtype qfh grs lrs (isub (IndexVariable fid) id t) (isub (IndexVariable fid) id' t')
      return $ c1 && c2
checkSubtype qfh grs lrs (TIForall id t i j) (TIForall id' t' i' j') =
  let fid = fresh (fresh (fresh id [i, j, i', j']) [IndexVariable id']) [t, t']
    in do
      c1 <- checkSubtype qfh grs lrs (isub (IndexVariable fid) id t) (isub (IndexVariable fid) id' t')
      c2 <- checkGRLeq qfh grs (isub (IndexVariable fid) id i) (isub (IndexVariable fid) id' i') 
      c3 <- checkGREq qfh grs (isub (IndexVariable fid) id j) (isub (IndexVariable fid) id' j') 
      return $ c1 && c2 && c3
checkSubtype _ _ _ _ _ = return False
