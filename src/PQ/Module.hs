module PQ.Module where

import Data.Maybe
import PQ.Expr
import PQ.Type
import PrettyPrinter (Pretty (..))

type Import = String -- Placeholder, currently unused

data TopLevelDefinition = TopLevelDefinition{
  id :: VariableId,
  args :: [Pattern],
  signature :: Maybe Type,
  definition :: Expr
} deriving Show

prettyTopLevelDefinition :: TopLevelDefinition -> String
prettyTopLevelDefinition (TopLevelDefinition id args mtyp e) = 
  (if isJust mtyp then id ++ " :: " ++ pretty mtyp ++ "\n" else "") ++
  id ++ unwords (map pretty args) ++ " = " ++ pretty e ++ "\n" 

data Module = Module {
  name :: String,
  exports :: [VariableId],
  imports :: [Import],
  tldefs :: [TopLevelDefinition]
} deriving Show

instance Pretty Module where
  pretty (Module name exports imports tldefs) = unwords $ map prettyTopLevelDefinition tldefs