module Lang.Module.AST where
import Lang.Expr.AST (VariableId, Expr)
import Lang.Type.AST
import PrettyPrinter (Pretty(..))
import Data.Maybe

type Import = String -- Placeholder, currently unused

data TopLevelDefinition = TopLevelDefinition{
  id :: VariableId,
  args :: [VariableId],
  signature :: Maybe Type,
  definition :: Expr
}

prettyTopLevelDefinition :: TopLevelDefinition -> String
prettyTopLevelDefinition (TopLevelDefinition id args mtyp e) = 
  (if isJust mtyp then id ++ " :: " ++ pretty mtyp ++ "\n" else "") ++
  id ++ unwords args ++ " = " ++ pretty e ++ "\n" 

data Module = Module {
  name :: String,
  exports :: [VariableId],
  imports :: [Import],
  tldefs :: [TopLevelDefinition]
}

instance Pretty Module where
  pretty (Module name exports imports tldefs) = unwords $ map prettyTopLevelDefinition tldefs