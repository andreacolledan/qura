module Parser
  ( Parser.runParser,
    --re-exports
    ParserError,
    parseModule,
    errorBundlePretty
  )
where

import Control.Monad.State.Strict (evalStateT)
import Parser.Core
  ( Parser,
    ParserError,
    ParserState (ParserState, baseIndent, parseGMA, parseLMA),
  )
import Parser.Module (parseModule)
import Text.Megaparsec (errorBundlePretty, parse, pos1)


-- | @runParser p parseGMA parseLMA filename content@ runs parser @p@ on @content@.
-- @parseGMA@ and @parseLMA@ control whether annotations for global and local metrics are parsed, respectively.
-- @filename@ is only used for error reporting, the function does not read from any file.
-- Returns 'Either' a 'ParseErrorBundle' from "Megaparsec" or the result of @p@.
runParser :: Parser a -> Bool -> Bool -> String -> String -> Either ParserError a
runParser p parseGMA parseLMA = parse (evalStateT p ParserState {baseIndent = pos1, parseGMA = parseGMA, parseLMA = parseLMA})
