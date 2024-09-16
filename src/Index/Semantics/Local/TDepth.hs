module Index.Semantics.Local.TDepth where

import Index.Semantics.Local.Resource
import Index.AST
import Circuit

-- | The local metric module for T-depth.
-- T-depth of a wire is defined informally as the maximum number of T gates
-- in any path from an input or initialization to the wire
tDepthMetric :: LocalMetricModule
tDepthMetric =
  LocalMetricModule
    { name = "T-depth",
      -- | depth of any output wire of the T gate is the max of depths of the inputs plus one,
      -- depth of any output wire of the other gates is the max of depths of the inputs
      desugarOutput = \op _ is -> case op of
        T -> foldr (Max . (Number 1 `Plus`)) (Number 0) is
        _ -> foldr Max (Number 0) is
    }