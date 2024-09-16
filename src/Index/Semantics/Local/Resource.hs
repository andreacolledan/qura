module Index.Semantics.Local.Resource where
import Circuit
import Index.AST ( Index )
import PrettyPrinter

-- | A local metric module defines how to compute a local metric of a circuit.
data LocalMetricModule = LocalMetricModule
  { -- | The name of the metric
    name :: String,
    -- | The local metric associated with each output of an operation, given the operation's inputs
    -- @desugarOutput op i is@ returns the local metric associated with the @i@-th output of operation @op@, given the input local metrics @is@.
    desugarOutput :: QuantumOperation -> Int -> [Index] -> Index
  }

instance Pretty LocalMetricModule where
  pretty = name