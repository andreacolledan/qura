module Lang.Module.Parse (
  parseModule
) where
import Parser
import Lang.Expr.AST
import Lang.Type.AST
import Control.Monad
import Lang.Type.Parse
import Lang.Expr.Parse
import Lang.Module.AST (TopLevelDefinition(..), Module(..))
import Text.Megaparsec
import Lang.Expr.Pattern (Pattern)

-- | Parse a PQ module, i.e. a sequence of top-level declarations.
parseModule :: Parser Module
parseModule = do
  -- TODO module header
  -- TODO imports
  tldefs <- many topLevelDefinition
  eof
  return Module {
    name = "",
    exports = [],
    imports = [],
    tldefs = tldefs
  }

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
          fail $ "The type signature for `" ++ name' ++ "' should be followed by an accompanying binding, found `" ++ name ++ "'"
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