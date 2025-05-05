module Metric.Local.Depth (depthMetric) where

import Metric.Local
import PQ.Index

-- | The local metric module for depth.
-- Depth of a wire is defined informally as the maximum number of operations
-- in any path from an input or initialization to the wire
depthMetric :: LocalMetricModule
depthMetric =
  LocalMetricModule
    { name = "depth",
      -- | depth of any output wire is the max of depths of the inputs plus one
      desugarOutput = \_ _ is -> foldr (Max . (Number 1 `Plus`)) (Number 0) is
    }