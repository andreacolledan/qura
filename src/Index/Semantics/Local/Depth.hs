module Index.Semantics.Local.Depth where

import Index.Semantics.Local.Resource
import Index.AST

depthMetric :: LocalMetricModule
depthMetric =
  LocalMetricModule
    { name = "depth",
      desugarOutput = \_ _ is -> foldr (Max . (Number 1 `Plus`)) (Number 0) is
    }