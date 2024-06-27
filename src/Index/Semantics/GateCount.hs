module Index.Semantics.GateCount where
import Index.Semantics.Resource

gateCountResourceSemantics :: GlobalResourceSemantics

gateCountResourceSemantics =
  GlobalResourceSemantics
    {
    interpretIdentity = 0,
    interpretWire = const 0,
    interpretSequence = (+),
    interpretParallel = (+),
    smtEmbedIdentity = "0",
    smtEmbedWire = const "0",
    smtEmbedSequence = \i j -> "(+ " ++ i ++ " " ++ j ++ ")",
    smtEmbedParallel = \i j -> "(+ " ++ i ++ " " ++ j ++ ")",
    smtEmbedBoundedSequence = undefined,
    smtEmbedBoundedParallel = undefined
  }