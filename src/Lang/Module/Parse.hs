module Lang.Module.Parse (
  parseModule
) where
import Parser
import Lang.Expr.AST
import Lang.Type.AST
import Control.Monad
import Lang.Type.Parse
import Lang.Expr.Parse
import Lang.Module.AST (TopLevelDefinition, Module(..))
import Text.Megaparsec

parseModule :: Parser Module
parseModule = do
  -- TODO module header
  -- TODO imports
  tldefs <- many topLevelDefinition
  return Module {
    name = "",
    exports = [],
    imports = [],
    tldefs = tldefs
  }

topLevelDefinition :: Parser TopLevelDefinition
topLevelDefinition =
  do
    sig <- optional $ try $ nonIndented functionSignature
    (name, definition) <- nonIndented functionDefinition
    case sig of
      Nothing -> return (name, Nothing, definition)
      Just (name', signature) -> do
        when (name /= name') $
          fail $ "The type signature for `" ++ name' ++ "' should be followed by an accompanying binding, found `" ++ name ++ "'"
        return (name, Just signature, definition)
  <?> "top-level definition"

functionSignature :: Parser (String, Type)
functionSignature =
  do
    functionName <- identifier
    doubleColon
    functionType <- indented parseType
    return (functionName, functionType)
  <?> "function signature"

functionDefinition :: Parser (String, Expr)
functionDefinition =
  do
    functionName <- identifier
    symbol "="
    functionDef <- indented parseExpr
    return (functionName, functionDef)
  <?> "function definition"