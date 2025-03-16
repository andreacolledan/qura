module Lang.Module.AST where
import Lang.Expr.AST (VariableId, Expr)
import Lang.Type.AST
import PrettyPrinter (Pretty(..))
import Control.Monad (join)
import Data.Maybe

type Import = String -- Placeholder, currently unused

type TopLevelDefinition = (VariableId, Maybe Type, Expr)

prettyTopLevelDefinition :: TopLevelDefinition -> String
prettyTopLevelDefinition (id, mtyp, e) = 
  (if isJust mtyp then id ++ " :: " ++ pretty mtyp ++ "\n" else "") ++
  id ++ " = " ++ pretty e ++ "\n" 

data Module = Module {
  name :: String,
  exports :: [VariableId],
  imports :: [Import],
  tldefs :: [TopLevelDefinition]
}

instance Pretty Module where
  pretty (Module name exports imports tldefs) = join $ map prettyTopLevelDefinition tldefs