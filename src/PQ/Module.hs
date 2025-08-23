module PQ.Module where

import Data.Maybe (isJust)
import Metric.Global (GlobalMetricModule)
import Metric.Local (LocalMetricModule)
import PQ.Expr (Expr, Pattern, VariableId)
import PQ.Type (Type)
import PrettyPrinter (Pretty (..))

data Pragma
  = VerifyGlobal GlobalMetricModule
  | VerifyLocal LocalMetricModule

instance Show Pragma where
  show (VerifyGlobal grs) = "{-# VERIFY_GLOBAL " ++ pretty grs ++ " #-}"
  show (VerifyLocal lrs) = "{-# VERIFY_LOCAL " ++ pretty lrs ++ " #-}"

type Import = String -- Placeholder, currently unused

data TopLevelDefinition = TopLevelDefinition
  { id :: VariableId,
    args :: [Pattern],
    signature :: Maybe Type,
    definition :: Expr
  }
  deriving (Show)

prettyTopLevelDefinition :: TopLevelDefinition -> String
prettyTopLevelDefinition (TopLevelDefinition id args mtyp e) =
  (if isJust mtyp then id ++ " :: " ++ pretty mtyp ++ "\n" else "")
    ++ id
    ++ unwords (map pretty args)
    ++ " = "
    ++ pretty e
    ++ "\n"

data Module = Module
  { pragmas :: [Pragma],
    name :: String,
    exports :: [VariableId],
    imports :: [Import],
    tldefs :: [TopLevelDefinition]
  }
  deriving (Show)

instance Pretty Module where
  pretty (Module pragmas name exports imports tldefs) = unwords $ map prettyTopLevelDefinition tldefs
