module Index.Semantics.Depth where

import Index.Semantics.Resource

depthResourceSemantics :: LocalResourceSemantics
depthResourceSemantics =
  LocalResourceSemantics
    {
      outputInterpretation = \_ _ is -> 1 + maximum is,
      embedOutput = undefined
    }