module Lang.Expr.Pattern where
import PrettyPrinter
import Data.List (intercalate)

type VariableId = String

data Pattern
  = PVar VariableId
  | PTuple [Pattern]
  | PCons Pattern Pattern
  deriving (Eq, Show)

instance Pretty Pattern where
  pretty (PVar id) = id
  pretty (PTuple ps) = "(" ++ intercalate ", " (map pretty ps) ++ ")"
  pretty (PCons p1 p2) = "(" ++ pretty p1 ++ ":" ++ pretty p2 ++ ")"



    