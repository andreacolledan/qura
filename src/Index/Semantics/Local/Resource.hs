module Index.Semantics.Local.Resource where
import Circuit
import Index.AST ( Index )
import PrettyPrinter

data LocalResourceSemantics = LocalResourceSemantics
  {
    name :: String,
    desugarOutput :: QuantumOperation -> Int -> [Index] -> Index
  }

instance Pretty LocalResourceSemantics where
  pretty = name