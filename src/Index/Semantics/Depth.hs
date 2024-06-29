module Index.Semantics.Depth where

import Index.Semantics.Resource
import Index.AST

depthResourceSemantics :: LocalResourceSemantics
depthResourceSemantics =
  LocalResourceSemantics
    {
      desugarOutput = \_ _ is -> Number 1 `Plus` foldr1 Max is,
      embedOutput = undefined
    }