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
  Parser.runParser,
  isParsingGRA,
  isParsingLRA,
  indented,
  nonIndented
)
where

import Text.Megaparsec
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Char (space1, string, alphaNumChar, letterChar)
import Control.Monad
import Control.Monad.State.Strict

--- Custom Parser type

data ParserConfig = ParserConfig {
  parsegra :: Bool,
  parselra :: Bool
}

data ParserState = ParserState {
  baseIndent :: Pos,
  config :: ParserConfig
}

type Parser = StateT ParserState (Parsec Void String)

indented :: Parser a -> Parser a
indented p = do
  ParserState{baseIndent = bi, config = cfg} <- get
  put ParserState{baseIndent = mkPos . (+1) . unPos $ bi , config = cfg}
  res <- p
  put ParserState{baseIndent = bi , config = cfg}
  return res

isParsingGRA :: Parser Bool
isParsingGRA = gets (parsegra . config)

isParsingLRA :: Parser Bool
isParsingLRA = gets (parselra . config)

--- Fundamental lexing functions

-- | Space consumer for lexing, consumes all whitespace, including newlines
sc :: Parser ()
sc = Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")

-- | Parse 'p' consuming all trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = do
  baseIndent <- gets baseIndent
  currentIndent <- Lex.indentLevel
  unless (currentIndent >= baseIndent) $ fail $ "Indentation error: expected input at level " ++ show (unPos baseIndent) ++ " or greater, got level " ++ show (unPos currentIndent)
  Lex.lexeme sc p

-- | Parse a verbatim string as a lexeme
symbol :: String -> Parser ()
symbol = void <$> lexeme . string

-- | Consume all whitespace, including newlines
whitespace :: Parser ()
whitespace = sc

nonIndented :: Parser a -> Parser a
nonIndented = Lex.nonIndented sc

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

comma :: Parser ()
comma = symbol "," <?> "comma"

colon :: Parser ()
colon = try $ symbol ":" <* notFollowedBy (symbol ":")

doubleColon :: Parser ()
doubleColon = symbol "::"

hole :: Parser ()
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
  --, "def"
  -- Types
  , "Bit"
  , "Qubit"
  , "List"
  , "Circ"
  -- Indices
  , "max"
  , "sum"
  ]

keyword :: String -> Parser ()
keyword kw = void <$> lexeme $ try $ string kw <* notFollowedBy alphaNumChar

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
runParser pconfig p = parse (evalStateT p ParserState{baseIndent = pos1, config = pconfig})

