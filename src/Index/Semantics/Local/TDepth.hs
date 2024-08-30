module Index.Semantics.Local.TDepth where

import Index.Semantics.Local.Resource
import Index.AST
import Circuit

tDepthMetric :: LocalMetricModule
tDepthMetric =
  LocalMetricModule
    { name = "T-depth",
      desugarOutput = \op _ is -> case op of
        T -> foldr (Max . (Number 1 `Plus`)) (Number 0) is
        _ -> foldr Max (Number 0) is
    }