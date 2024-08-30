module Index.Semantics.Local.Resource where
import Circuit
import Index.AST ( Index )
import PrettyPrinter

data LocalMetricModule = LocalMetricModule
  {
    name :: String,
    desugarOutput :: QuantumOperation -> Int -> [Index] -> Index
  }

instance Pretty LocalMetricModule where
  pretty = name