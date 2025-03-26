module Lang.Analysis.TypeError where
import Lang.Expr.AST
import Lang.Type.AST
import Index.AST
import Lang.Expr.Pattern
import PrettyPrinter

--- TYPING ERROR MODULE ------------------------------------------------------------------
---
--- This module defines typing errors that can occur during the type derivation process.
------------------------------------------------------------------------------------------

-- The datatype of errors that can occur during a derivation
data TypeError
  = UnboundVariable VariableId [Expr]
  | UnboundIndexVariable IVarId [Expr]
  | UnexpectedType Expr Type Type [Expr]
  | UnexpectedIndex Index Index [Expr]
  | UnexpectedEffect Expr Index Index [Expr]
  | ExpectedBundleType Expr Type [Expr]
  | CannotSynthesizeType Expr [Expr]
  | -- Pattern errors
    PatternMismatch Pattern Type [Expr]
  | ConsEmptyList Pattern Type [Expr]
  | -- Linearity errors
    UnusedLinearVariable VariableId [Expr]
  | OverusedLinearVariable VariableId [Expr]
  | LiftedLinearVariable VariableId [Expr]
  | -- Box errors
    UnboxableType Expr Type [Expr]
  | -- Fold errors
    UnfoldableStepfunction Expr Type [Expr]
  | UnfoldableAccumulator Expr Type [Expr]
  | UnfoldableArg Expr Type [Expr]
  | -- Top-level definitions
    UnexpectedIndexVariableArgument VariableId IVarId Pattern [Expr]
  | ExtraArgument VariableId Pattern [Expr]
  | UnbangedSignature VariableId Type [Expr]
  | MissingSignature VariableId Pattern [Expr]
  | -- Other
    ShadowedIndexVariable IVarId [Expr]
  | UnexpectedEmptyList Expr Type [Expr]
  deriving (Eq)

instance Show TypeError where
  show (UnboundVariable id surr) = "* Unbound variable '" ++ id ++ "'" ++ printSurroundings surr
  show (UnusedLinearVariable id surr) = "* Unused linear variable '" ++ id ++ "'" ++ printSurroundings surr
  show (LiftedLinearVariable id surr) = "* Linear variable '" ++ id ++ "' cannot be consumed in a lifted expression" ++ printSurroundings surr
  show (UnexpectedType exp typ1 typ2 surr) =
    "* Expected expression '" ++ trnc 80 (pretty exp) ++ "'\n   to have type\n    '" ++ pretty typ1 ++ "',\n   got\n    '" ++ pretty typ2 ++ "'\n   instead" ++ printSurroundings surr
  show (UnexpectedIndex i1 i2 surr) = "* Expected index '" ++ pretty i1 ++ "', got '" ++ pretty i2 ++ "' instead" ++ printSurroundings surr
  show (UnboxableType v typ surr) = "* Cannot box value '" ++ pretty v ++ "' of type '" ++ pretty typ ++ "'" ++ printSurroundings surr
  show (UnexpectedEffect exp i1 i2 surr) =
    "* Expected expression '" ++ trnc 80 (pretty exp) ++ "'\n   to have effect\n    '" ++ pretty i1 ++ "',\n   got\n    '" ++ pretty i2 ++ "'\n   instead" ++ printSurroundings surr
  show (UnfoldableStepfunction v typ surr) = "* Expression '" ++ pretty v ++ "' of type '" ++ pretty typ ++ "' is not a valid step function" ++ printSurroundings surr
  show (UnfoldableAccumulator v typ surr) = "* Expression '" ++ pretty v ++ "' of type '" ++ pretty typ ++ "' is not a valid accumulator" ++ printSurroundings surr
  show (UnfoldableArg v typ surr) = "* Expression '" ++ pretty v ++ "' of type '" ++ pretty typ ++ "' is not a valid fold argument" ++ printSurroundings surr
  show (UnboundIndexVariable id surr) = "* Unbound index variable '" ++ id ++ "'" ++ printSurroundings surr
  show (ShadowedIndexVariable id surr) = "* Shadowed index variable '" ++ id ++ "'" ++ printSurroundings surr
  show (OverusedLinearVariable id surr) = "* Linear variable '" ++ id ++ "' is used more than once" ++ printSurroundings surr
  show (UnexpectedEmptyList e typ surr) = "* Cannot conclude that expression '" ++ pretty e ++ "' of type '" ++ pretty typ ++ "' is a non-empty list" ++ printSurroundings surr
  show (ExpectedBundleType e typ surr) = "* Expected expression '" ++ pretty e ++ "' to have bundle type, got '" ++ pretty typ ++ "' instead" ++ printSurroundings surr
  show (CannotSynthesizeType e surr) = "* Cannot synthesize type for expression '" ++ pretty e ++ "'. Consider annotating it with a type" ++ printSurroundings surr
  show (PatternMismatch p typ surr) = "* Pattern '" ++ pretty p ++ "' does not match type '" ++ pretty typ ++ "'" ++ printSurroundings surr
  show (ConsEmptyList p typ surr) = "* Pattern '" ++ pretty p ++ "' does not match type '" ++ pretty typ ++ "' because the latter allows the empty list" ++ printSurroundings surr
  show (UnexpectedIndexVariableArgument fname id pat _) = "* Mismatching index parameters in top-level definition '" ++ fname ++ "': signature binds '" ++ id ++ "', while definition binds '" ++ pretty pat ++ "'."
  show (ExtraArgument fname pat _) = "* Extra argument in top-level definition '" ++ fname ++ "': '" ++ pretty pat ++ "'."
  show (MissingSignature fname pat _) = "* Cannot determine type of argument '" ++ pretty pat ++ "' in top-level definition '" ++ fname ++ "'. Consider providing a type signature for '" ++ fname ++ "'."
  show (UnbangedSignature fname typ _) = "* Expected top-level definition '" ++ fname ++ "' to have a banged type signature, got '" ++ pretty typ ++ "' instead."

-- | @printSurroundings es@ returns a string describing the expressions in @es@, if any
printSurroundings :: [Expr] -> String
printSurroundings [] = ""
printSurroundings (e : es) = "\n* While typing " ++ pretty e ++ go es 3
  where
    go :: [Expr] -> Int -> String
    go [] _ = ""
    go _ 0 = "\n..."
    go (e : es) n = "\n  In " ++ trnc 80 (pretty e) ++ go es (n - 1)

-- | @trnc n s@ returns the first @n@ characters of @s@, followed by "..." if @s@ is longer than @n@
trnc :: Int -> String -> String
trnc n s = if length s > n then take n s ++ "..." else s