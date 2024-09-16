module Lang.Type.Semantics where

import Index.AST
import Index.Semantics
import Lang.Type.AST
import Solving.CVC5 (SolverHandle)
import Control.Monad (zipWithM)
import Index.Semantics.Global.Resource
import Index.Semantics.Local.Resource
import Index.Unify

-- | @simplifyType qfh mgrs mlrs t@ returns type @t@ in which all index annotations have been simplified
-- to a normal form according to 'simplifyIndex'.
-- @qfh@, @mgrs@, and @mlrs@ are required by 'simplifyIndex'.
simplifyType :: SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> Type -> IO Type
simplifyType qfh mgrs mlrs (TWire wtype i) = TWire wtype <$> maybeSimplifyIndex qfh mgrs mlrs i
simplifyType qfh mgrs mlrs (TTensor ts) = TTensor <$> mapM (simplifyType qfh mgrs mlrs) ts
simplifyType qfh mgrs mlrs (TArrow t1 t2 i j) = TArrow <$> simplifyType qfh mgrs mlrs t1 <*> simplifyType qfh mgrs mlrs t2 <*> maybeSimplifyIndex qfh mgrs mlrs i <*> maybeSimplifyIndex qfh mgrs mlrs j
simplifyType qfh mgrs mlrs (TBang i t) = TBang <$> maybeSimplifyIndex qfh mgrs mlrs i <*> simplifyType qfh mgrs mlrs t
simplifyType qfh mgrs mlrs (TList id i t) = TList id <$> simplifyIndex qfh mgrs mlrs i <*> simplifyType qfh mgrs mlrs t
simplifyType qfh mgrs mlrs (TCirc i inBtype outBtype) = TCirc <$> maybeSimplifyIndex qfh mgrs mlrs i <*> simplifyType qfh mgrs mlrs inBtype <*> simplifyType qfh mgrs mlrs outBtype
simplifyType qfh mgrs mlrs (TIForall id t i j) = TIForall id <$> simplifyType qfh mgrs mlrs t <*> maybeSimplifyIndex qfh mgrs mlrs i <*> maybeSimplifyIndex qfh mgrs mlrs j
simplifyType _ _ _ t = return t

-- | @checkSubtype cs qfh mgrs mlrs t1 t2@ checks if type @t1@ is a subtype of type @t2@.,
-- under the assumption that the constraints @cs@ hold.
-- @qfh@, @mgrs@, and @mlrs@ are required by index checking functions.
checkSubtype :: [Constraint] -> SolverHandle -> Maybe GlobalMetricModule -> Maybe LocalMetricModule -> Type -> Type -> IO Bool
checkSubtype _ _ _ _ TUnit TUnit = return True
checkSubtype cs qfh _ mlrs (TWire wtype1 i) (TWire wtype2 j) = do
  c <- checkLRLeq cs qfh mlrs i j
  return $ wtype1 == wtype2 && c
checkSubtype cs qfh mgrs mlrs (TBang i1 t1) (TBang i2 t2) = do
  c1 <- checkGRLeq cs qfh mgrs i1 i2
  c2 <- checkSubtype cs qfh mgrs mlrs t1 t2
  return $ c1 && c2
checkSubtype cs qfh mgrs mlrs (TTensor ts) (TTensor ts')
  | length ts == length ts' = do
    cs <- zipWithM (checkSubtype cs qfh mgrs mlrs) ts ts'
    return $ and cs
  | otherwise = return False
checkSubtype cs qfh mgrs mlrs (TArrow t1 t2 i j) (TArrow t1' t2' i' j') = do
  c1 <- checkSubtype cs qfh mgrs mlrs t1' t1
  c2 <- checkSubtype cs qfh mgrs mlrs t2 t2'
  c3 <- checkGRLeq cs qfh mgrs i i' 
  c4 <- checkGREq cs qfh mgrs j j' 
  return $ c1 && c2 && c3 && c4
checkSubtype cs qfh mgrs mlrs (TCirc i t1 t2) (TCirc i' t1' t2') = do
  c1 <- checkGRLeq cs qfh mgrs i i' 
  c2 <- checkSubtype cs qfh mgrs mlrs t1' t1
  c3 <- checkSubtype cs qfh mgrs mlrs t2 t2'
  return $ c1 && c2 && c3
checkSubtype cs qfh mgrs mlrs (TList id i t) (TList id' i' t') = do
  cempty1 <- checkEq cs qfh i (Number 0)
  cempty2 <- checkEq cs qfh i' (Number 0)
  if cempty1 && cempty2 then return True else do -- empty list types are equal regardless of parameter
    let fid = fresh (fresh (fresh id [i, i']) [IVar id']) [t, t']
    c1 <- checkEq cs qfh i i'
    c2 <- checkSubtype cs qfh mgrs mlrs (isub (isubSingleton id (IVar fid)) t) (isub (isubSingleton id' (IVar fid)) t')
    return $ c1 && c2
checkSubtype cs qfh mgrs mlrs (TIForall id t i j) (TIForall id' t' i' j') =
  let fid = fresh (fresh (fresh id [i, j, i', j']) [IVar id']) [t, t']
    in do
      c1 <- checkSubtype cs qfh mgrs mlrs (isub (isubSingleton id (IVar fid)) t) (isub (isubSingleton id' (IVar fid)) t')
      c2 <- checkGRLeq cs qfh mgrs (isub (isubSingleton id (IVar fid)) i) (isub (isubSingleton id' (IVar fid)) i') 
      c3 <- checkGREq cs qfh mgrs (isub (isubSingleton id (IVar fid)) j) (isub (isubSingleton id' (IVar fid)) j') 
      return $ c1 && c2 && c3
checkSubtype _ _ _ _ _ _ = return False