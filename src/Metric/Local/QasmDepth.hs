module Metric.Local.QasmDepth (qasmDepthMetric) where

import Circuit.Type
import Metric.Local
import PQ.Index

-- | The local metric module for depth.
-- Qasm depth of a wire is defined informally as the maximum number of operations
-- in any path from an input or initialization to the wire
qasmDepthMetric :: LocalMetricModule
qasmDepthMetric =
  LocalMetricModule
    { name = "qasmdepth",
      -- | depth of any output wire is the max of depths of the inputs plus one
      desugarOutput = \op _ is -> case op of
        QInit True -> Number 1
        _ -> foldr (Max . (Number 1 `Plus`)) (Number 0) is
    }

-- FIXME how do i add the fact that the qinit1 has depth 1 and mcnot hass depth 2(m − 1) + 1?