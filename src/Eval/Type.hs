module Eval.Type
  ( simplifyType,
  )
where

import Eval.Index
import Metric
import PQ.Type
import Solver.SMT

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