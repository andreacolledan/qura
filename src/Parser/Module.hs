module Parser.Module (parseModule) where

import Control.Monad (when)
import Data.Functor (($>))
import Metric.Global (GlobalMetricModule)
import Metric.Global.Bits (bitsMetric)
import Metric.Global.GateCount (gateCountMetric)
import Metric.Global.Qubits (qubitsMetric)
import Metric.Global.TCount (tCountMetric)
import Metric.Global.Width (widthMetric)
import Metric.Local (LocalMetricModule)
import Metric.Local.Depth (depthMetric)
import Metric.Local.TDepth (tDepthMetric)
import PQ.Expr (Expr, Pattern, VariableId)
import PQ.Module
  ( Module (..),
    Pragma (VerifyGlobal, VerifyLocal),
    TopLevelDefinition (..),
  )
import PQ.Type (Type)
import Parser.Core
  ( Parser,
    doubleColon,
    equalSign,
    identifier,
    indented,
    many,
    nonIndented,
    pragmabraces,
    symbol,
    (<?>),
  )
import Parser.Expr (parseExpr, parsePattern)
import Parser.Type (typeExpression)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    optional,
    (<|>),
  )

-- | Parse a PQ module, i.e. a sequence of top-level declarations.
parseModule :: Parser Module
parseModule = do
  pragmas <- many pragma
  -- TODO module header
  -- TODO imports
  tldefs <- many topLevelDefinition
  eof
  return
    Module
      { pragmas = pragmas,
        name = "",
        exports = [],
        imports = [],
        tldefs = tldefs
      }

pragma :: Parser Pragma
pragma = pragmabraces $ verifyGlobalPragma <|> verifyLocalPragma
  where
    verifyGlobalPragma :: Parser Pragma
    verifyGlobalPragma =
      symbol "VERIFY_GLOBAL" >> VerifyGlobal <$> globalMetricName

    globalMetricName :: Parser GlobalMetricModule
    globalMetricName =
      (symbol "width" $> widthMetric)
        <|> (symbol "gatecount" $> gateCountMetric)
        <|> (symbol "tcount" $> tCountMetric)
        <|> (symbol "bits" $> bitsMetric)
        <|> (symbol "qubits" $> qubitsMetric)

    verifyLocalPragma :: Parser Pragma
    verifyLocalPragma =
      symbol "VERIFY_LOCAL" >> VerifyLocal <$> localMetricName

    localMetricName :: Parser LocalMetricModule
    localMetricName =
      (symbol "depth" $> depthMetric)
        <|> (symbol "tdepth" $> tDepthMetric)

-- | Parse a top-level declaration, i.e. an optional type signature followed by a definition
topLevelDefinition :: Parser TopLevelDefinition
topLevelDefinition =
  do
    sig <- optional functionSignature
    (name, args, definition) <- functionDefinition
    case sig of
      Nothing -> return $ TopLevelDefinition name args Nothing definition
      Just (name', signature) -> do
        when (name /= name') $
          fail $
            "The type signature for `"
              ++ name'
              ++ "' should be followed by an accompanying binding, found `"
              ++ name
              ++ "'"
        return $ TopLevelDefinition name args (Just signature) definition
    <?> "top-level definition"

-- | Parse "@f :: typ@" as a top-level type signature.
functionSignature :: Parser (VariableId, Type)
functionSignature =
  do
    functionName <- try $ nonIndented $ identifier <* doubleColon
    functionType <- indented typeExpression
    return (functionName, functionType)
    <?> "function signature"

-- | Parse "@f = expr@" as a top-level definition.
functionDefinition :: Parser (VariableId, [Pattern], Expr)
functionDefinition =
  do
    functionName <- nonIndented identifier
    args <- many parsePattern
    equalSign
    functionDef <- indented parseExpr
    return (functionName, args, functionDef)
    <?> "function definition"
