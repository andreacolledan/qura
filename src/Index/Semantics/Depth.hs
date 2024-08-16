module Index.Semantics.Depth where

import Index.Semantics.Resource
import Index.AST

depthResourceSemantics :: LocalResourceSemantics
depthResourceSemantics =
  LocalResourceSemantics
    {
      desugarOutput = \_ _ is -> foldr (Max . (Number 1 `Plus`)) (Number 0) is
    }