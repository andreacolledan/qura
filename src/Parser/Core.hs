module Parser.Core
  ( Parser,
    ParserState (..),
    symbol,
    whitespace,
    number,
    keyword,
    identifier,
    parens,
    brackets,
    braces,
    pragmabraces,
    comma,
    dot,
    emptyParens,
    emptyBrackets,
    arrow,
    lessSign,
    plus,
    star,
    hyphen,
    bang,
    colon,
    doubleColon,
    bangDoubleColon,
    underscore,
    equalSign,
    backslash,
    dollarSign,
    at,
    indented,
    nonIndented,
    whenGlobalAnalysis,
    whenLocalAnalysis,
    many,
    (<?>),
    withDefault,
    ParserError,
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as Lex

--- Custom Parser type

-- | The state of the PQ parser
data ParserState
  = ParserState
  { -- | The current indentation level.
    baseIndent :: Pos,
    -- | Whether global metric annotations should be parsed.
    parseGMA :: Bool,
    -- | Whether local metric annotations should be parsed.
    parseLMA :: Bool
  }

-- | The type of the stateful PQ parser (see @Parsec@)
type Parser = StateT ParserState (Parsec Void String)

type ParserError = ParseErrorBundle String Void

--- Fundamental lexing functions ---

-- | Space consumer for lexing, consumes all whitespace, including newlines
sc :: Parser ()
sc =
  Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")

-- | @lexeme p@ parses @p@, checking the indentation level and consuming all trailing whitespace.
-- Fails if the lexeme is below the current indentation level.
lexeme :: Parser a -> Parser a
lexeme p = do
  baseIndent <- gets baseIndent
  currentIndent <- Lex.indentLevel
  unless (currentIndent >= baseIndent) $
    fail $
      "Indentation error: expected input at level "
        ++ show (unPos baseIndent)
        ++ " or greater, got level "
        ++ show (unPos currentIndent)
  Lex.lexeme sc p

-- | Parse a verbatim string as a lexeme
symbol :: String -> Parser ()
symbol = void <$> lexeme . string

-- | Consume all whitespace, including newlines
whitespace :: Parser ()
whitespace = sc

--- Recurring symbols

-- | Parse ".".
dot :: Parser ()
dot = symbol "." <?> "dot"

-- | Parse ",".
comma :: Parser ()
comma = symbol "," <?> "comma"

-- | Parse ":".
colon :: Parser ()
colon = try $ symbol ":" <* notFollowedBy (symbol ":")

-- | Parse "::".
doubleColon :: Parser ()
doubleColon = symbol "::"

-- | Parse "_".
underscore :: Parser ()
underscore = try $ symbol "_" <* notFollowedBy alphaNumChar

-- | Parse "()".
emptyParens :: Parser ()
emptyParens = symbol "()"

-- | Parse "[]".
emptyBrackets :: Parser ()
emptyBrackets = symbol "[]"

-- | Parse "-o".
arrow :: Parser ()
arrow = symbol "-o"

-- | Parse "-".
hyphen :: Parser ()
hyphen = try $ symbol "-" <* notFollowedBy (symbol "o")

-- | Parse "+".
plus :: Parser ()
plus = symbol "+"

-- | Parse "*".
star :: Parser ()
star = symbol "*"

-- | Parse "<".
lessSign :: Parser ()
lessSign = symbol "<"

-- | Parse "!".
bang :: Parser ()
bang = try $ symbol "!" <* notFollowedBy (symbol ":")

-- | Parse "=".
equalSign :: Parser ()
equalSign = symbol "="

-- | Parse "\".
backslash :: Parser ()
backslash = symbol "\\"

-- | Parse "@".
at :: Parser ()
at = symbol "@"

-- | Parse "$".
dollarSign :: Parser ()
dollarSign = symbol "$"

-- | Parse "!::".
bangDoubleColon :: Parser ()
bangDoubleColon = symbol "!::"

--- Keywords and identifiers ---

-- | The list of reserved PQ keywords: @let@, @in@, @fold@, @apply@, @forall@, @Circ@, @Bit@, @Qubit@, @List@, @max@, and @sum@.
reservedKeywords :: [String]
reservedKeywords =
  -- Language expressions
  [ "let",
    "in",
    "apply",
    "fold",
    "forall", -- also a keyword for types
    -- Types
    "Circ",
    "Bit",
    "Qubit",
    "List",
    -- Indices
    "max",
    "sum"
  ]

-- | Parse a reserved keyword. Throws an exception if the specified keyword is not a valid keyword in 'reservedKeywords'.
keyword :: String -> Parser ()
keyword kw = do
  unless (kw `elem` reservedKeywords) $
    error $
      "Internal error: tried to parse `"
        ++ kw
        ++ "' as a keyword, but it is not."
  _ <- lexeme $ try $ string kw <* notFollowedBy alphaNumChar
  return ()

-- | Parse an identifier. Fails without consuming anything if the identifier is a reserved keyword.
identifier :: Parser String
identifier = lexeme $
  try $
    do
      id <- (:) <$> idStart <*> many idChar
      when (id `elem` reservedKeywords) $
        fail ("expected identifier, found reserved keyword " ++ id)
      return id
  where
    -- \| Parse a character allowed at the start of an identifier (a letter).
    idStart :: Parser Char
    idStart = letterChar

    -- \| Parse a character allowed in an identifier (an alphanumerical character).
    idChar :: Parser Char
    idChar = alphaNumChar

-- | Parse a natural number
number :: Parser Integer
number = lexeme Lex.decimal <?> "natural number"

--- Basic combinators ---

-- | @indented p@ parses @p@ if it is more indented than the current indentation level. Fails otherwise.
indented :: Parser a -> Parser a
indented p = do
  state@(ParserState {baseIndent = bi}) <- get
  put state {baseIndent = mkPos . (+ 1) . unPos $ bi}
  res <- p
  put state
  return res

-- | @nonIndented p@ parses @p@ if it is not preceded by any indentation. Fails otherwise.
nonIndented :: Parser a -> Parser a
nonIndented = Lex.nonIndented sc

-- | @parens p@ parses @p@ enclosed in "(" and ")".
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | @brackets p@ parses @p@ enclosed in "[" and "]".
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | @braces p@ parses @p@ enclosed in "{" and "}".
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | @pragmaBraces p@ parses @p@ enclosed in "{-#" and "#-}".
pragmabraces :: Parser a -> Parser a
pragmabraces = between (symbol "{-#") (symbol "#-}")

-- | @whenGlobalAnalysis p@ parses @p@ if the parser is configured to take into account global metric annotations.
-- Otherwise, @p@ is consumed, if present, and the parser returns 'Nothing'.
whenGlobalAnalysis :: Parser a -> Parser (Maybe a)
whenGlobalAnalysis p = do
  shouldParse <- gets parseGMA
  if shouldParse
    then Just <$> p
    else optional p >> return Nothing

-- | @whenLocalAnalysis p@ parses @p@ if the parser is configured to take into account local metric annotations.
-- Otherwise, @p@ is consumed, if present, and the parser returns 'Nothing'.
whenLocalAnalysis :: Parser a -> Parser (Maybe a)
whenLocalAnalysis p = do
  shouldParse <- gets parseLMA
  if shouldParse
    then Just <$> p
    else optional p >> return Nothing

withDefault :: a -> Parser (Maybe a) -> Parser a
withDefault def p = fromMaybe def <$> p