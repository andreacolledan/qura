module Parser (
  Parser,
  ParserConfig(..),
  whitespace,
  symbol,
  number,
  keyword,
  identifier,
  parens,
  brackets,
  braces,
  comma,
  colon,
  doubleColon,
  hole,
  Parser.runParser
)
where

import Control.Monad.Reader (ReaderT, runReaderT)
import Text.Megaparsec
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Char (space1, string, alphaNumChar, letterChar)
import Control.Monad

--- Custom Parser type

data ParserConfig = ParserConfig {
  parsegra :: Bool,
  parselra :: Bool
}

type Parser = ReaderT ParserConfig (Parsec Void String)

--- Fundamental lexing functions

-- | Space consumer for lexing
sc :: Parser ()
sc = Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")

-- | Wrapper, consumes trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

-- | Shorthand for parsing verbatim strings as lexemes
symbol :: String -> Parser String
symbol = Lex.symbol sc

whitespace :: Parser ()
whitespace = sc


--- Numbers

-- | Parse a decimal number
number :: Parser Integer
number = lexeme Lex.decimal <?> "natural number"


--- Symbols

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

comma :: Parser String
comma = symbol "," <?> "comma"

colon :: Parser String
colon = try $ symbol ":" <* notFollowedBy (symbol ":")

doubleColon :: Parser String
doubleColon = symbol "::"

hole :: Parser String
hole = try $ symbol "_" <* notFollowedBy alphaNumChar

--- Keywords and identifiers

reservedKeywords :: [String]
reservedKeywords = 
  -- Language expressions
  [ "let"
  , "in"
  , "Circ"
  , "apply"
  , "fold"
  , "let"
  , "in"
  , "forall" -- also a keyword for types
  , "def"
  -- Types
  , "Bit"
  , "Qubit"
  , "List"
  , "Circ"
  -- Indices
  , "max"
  , "sum"
  ]

keyword :: String -> Parser String
keyword kw = lexeme $ try $ string kw <* notFollowedBy alphaNumChar

idStart :: Parser Char
idStart = letterChar

idChar :: Parser Char
idChar = alphaNumChar

identifier :: Parser String
identifier = lexeme $ try $ do
  id <- (:) <$> idStart <*> many idChar
  when (id `elem` reservedKeywords) $ fail ("expected identifier, found reserved keyword " ++ id)
  return id

--- Runner

runParser :: ParserConfig -> Parser a -> String -> String -> Either (ParseErrorBundle String Void) a
runParser pconfig p = parse (runReaderT p pconfig)

