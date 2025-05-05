module Analyzer.Subtype
  ( checkSubtype,
  )
where

import Analyzer.Unify (HasIndex (isub), fresh, isubSingleton)
import Control.Monad (zipWithM)
import Metric (GlobalMetricModule, LocalMetricModule)
import PQ.Index (Index (IVar, Number))
import PQ.Type (Type (..))
import Solver

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