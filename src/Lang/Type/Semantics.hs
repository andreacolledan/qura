module Lang.Type.Semantics where

import Index.AST
import Index.Semantics
import Lang.Type.AST
import Solving.CVC5 (SolverHandle)
import Control.Monad (zipWithM)
import Index.Semantics.Global.Resource
import Index.Semantics.Local.Resource
import Index.Unify

-- | @simplifyType t@ returns type @t@ in which all index annotations have been simplified
-- to a normal form according to 'simplifyIndexStrong'.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
simplifyType :: SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> Type -> IO Type
simplifyType qfh grs lrs (TWire wtype i) = TWire wtype <$> maybeSimplifyIndex qfh grs lrs i
simplifyType qfh grs lrs (TTensor ts) = TTensor <$> mapM (simplifyType qfh grs lrs) ts
simplifyType qfh grs lrs (TArrow t1 t2 i j) = TArrow <$> simplifyType qfh grs lrs t1 <*> simplifyType qfh grs lrs t2 <*> maybeSimplifyIndex qfh grs lrs i <*> maybeSimplifyIndex qfh grs lrs j
simplifyType qfh grs lrs (TBang i t) = TBang <$> maybeSimplifyIndex qfh grs lrs i <*> simplifyType qfh grs lrs t
simplifyType qfh grs lrs (TList id i t) = TList id <$> simplifyIndex qfh grs lrs i <*> simplifyType qfh grs lrs t
simplifyType qfh grs lrs (TCirc i inBtype outBtype) = TCirc <$> maybeSimplifyIndex qfh grs lrs i <*> simplifyType qfh grs lrs inBtype <*> simplifyType qfh grs lrs outBtype
simplifyType qfh grs lrs (TIForall id t i j) = TIForall id <$> simplifyType qfh grs lrs t <*> maybeSimplifyIndex qfh grs lrs i <*> maybeSimplifyIndex qfh grs lrs j
simplifyType _ _ _ t = return t

-- Θ ⊢ t1 <: t2 (Figure 15)
-- | @checkSubtypeAssuming cs qfh grs lrs t1 t2@ checks if type @t1@ is a subtype of type @t2@.
-- SolverHandle @qfh@ is used to interact with the SMT solver.
checkSubtype :: [Constraint] -> SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> Type -> Type -> IO Bool
checkSubtype _ _ _ _ TUnit TUnit = return True
checkSubtype cs qfh _ lrs (TWire wtype1 i) (TWire wtype2 j) = do
  c <- checkLRLeq cs qfh lrs i j
  return $ wtype1 == wtype2 && c
checkSubtype cs qfh grs lrs (TBang i1 t1) (TBang i2 t2) = do
  c1 <- checkGRLeq cs qfh grs i1 i2
  c2 <- checkSubtype cs qfh grs lrs t1 t2
  return $ c1 && c2
checkSubtype cs qfh grs lrs (TTensor ts) (TTensor ts')
  | length ts == length ts' = do
    cs <- zipWithM (checkSubtype cs qfh grs lrs) ts ts'
    return $ and cs
  | otherwise = return False
checkSubtype cs qfh grs lrs (TArrow t1 t2 i j) (TArrow t1' t2' i' j') = do
  c1 <- checkSubtype cs qfh grs lrs t1' t1
  c2 <- checkSubtype cs qfh grs lrs t2 t2'
  c3 <- checkGRLeq cs qfh grs i i' 
  c4 <- checkGREq cs qfh grs j j' 
  return $ c1 && c2 && c3 && c4
checkSubtype cs qfh grs lrs (TCirc i t1 t2) (TCirc i' t1' t2') = do
  c1 <- checkGRLeq cs qfh grs i i' 
  c2 <- checkSubtype cs qfh grs lrs t1' t1
  c3 <- checkSubtype cs qfh grs lrs t2 t2'
  return $ c1 && c2 && c3
checkSubtype cs qfh grs lrs (TList id i t) (TList id' i' t') = do
  cempty1 <- checkEq cs qfh i (Number 0)
  cempty2 <- checkEq cs qfh i' (Number 0)
  if cempty1 && cempty2 then return True else do -- empty list types are equal regardless of parameter
    let fid = fresh (fresh (fresh id [i, i']) [IVar id']) [t, t']
    c1 <- checkEq cs qfh i i'
    c2 <- checkSubtype cs qfh grs lrs (isub (isubSingleton id (IVar fid)) t) (isub (isubSingleton id' (IVar fid)) t')
    return $ c1 && c2
checkSubtype cs qfh grs lrs (TIForall id t i j) (TIForall id' t' i' j') =
  let fid = fresh (fresh (fresh id [i, j, i', j']) [IVar id']) [t, t']
    in do
      c1 <- checkSubtype cs qfh grs lrs (isub (isubSingleton id (IVar fid)) t) (isub (isubSingleton id' (IVar fid)) t')
      c2 <- checkGRLeq cs qfh grs (isub (isubSingleton id (IVar fid)) i) (isub (isubSingleton id' (IVar fid)) i') 
      c3 <- checkGREq cs qfh grs (isub (isubSingleton id (IVar fid)) j) (isub (isubSingleton id' (IVar fid)) j') 
      return $ c1 && c2 && c3
checkSubtype cs _ _ _ _ _ = return False