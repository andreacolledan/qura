module Lang.Expr.Pattern where
import PrettyPrinter
import Data.List (intercalate)

type VariableId = String

-- | The datatype of binding patterns
data Pattern
  = PHole                 -- Ignore pattern   : _ 
  | PVar VariableId       -- Variable pattern : x, y, z, ...
  | PTuple [Pattern]      -- Tuple pattern    : (p1, p2, ...)
  | PCons Pattern Pattern -- Cons pattern     : p1 : p2
  deriving (Eq, Show)

instance Pretty Pattern where
  pretty PHole = "_"
  pretty (PVar id) = id
  pretty (PTuple ps) = "(" ++ intercalate ", " (map pretty ps) ++ ")"
  pretty (PCons p1 p2) = "(" ++ pretty p1 ++ ":" ++ pretty p2 ++ ")"



    