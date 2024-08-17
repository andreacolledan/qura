{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Index.Parse
  ( parseIndex,
    delimitedIndex,
    ParserConfig (..),
    Parser,
  )
where

import Index.AST
import Text.Parsec (alphaNum, lower, oneOf, try, (<|>), Parsec)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Prim ((<?>))
import Text.Parsec.Token
import Text.Parsec.Combinator

data ParserConfig = ParserConfig {
  parsegra :: Bool,
  parselra :: Bool
}

type Parser = Parsec String ParserConfig

--- INDEX PARSING MODULE ------------------------------------------------------------
---
--- This module contains the logic to parse index expressions.
-------------------------------------------------------------------------------------

TokenParser
  { parens = m_parens,
    identifier = m_identifier,
    reservedOp = m_reservedOp,
    reserved = m_reserved,
    whiteSpace = m_whiteSpace,
    natural = m_natural,
    symbol = m_symbol
  } =
    makeTokenParser
      emptyDef
        { identStart = lower,
          identLetter = alphaNum,
          opStart = oneOf "+-*<",
          opLetter = oneOf "+-*<",
          reservedOpNames = ["+", "-", "*", "<"],
          reservedNames = ["max","sum"]
        }

-- Parses "n" as (Number n)
parseNat :: Parser Index
parseNat =
  do
    n <- m_natural
    return $ Number $ fromInteger n
    <?> "natural number"

-- Parses an identifier "id "as (IndexVariable id)
parseIndexVariable :: Parser Index
parseIndexVariable =
  do
    v <- m_identifier
    return $ IVar v
    <?> "index variable"

-- Parses "max(i, j)" as (Max i j)
parseMax :: Parser Index
parseMax =
  do
    try $ do
      m_reserved "max"
      m_symbol "("
    i1 <- parseIndex
    m_symbol ","
    i2 <- parseIndex
    m_symbol ")"
    return $ Max i1 i2
    <?> "max expression"

multOp :: Parser (Index -> Index -> Index)
multOp =
  do
    m_reservedOp "*"
    return Mult
    <?> "multiplication"

plusOp :: Parser (Index -> Index -> Index)
plusOp =
  do
    m_reservedOp "+"
    return Plus
    <?> "plus"

minusOp :: Parser (Index -> Index -> Index)
minusOp =
  do
    m_reservedOp "-"
    return Minus
    <?> "minus"

manyMaximumOp :: Parser (Index -> Index)
manyMaximumOp = foldr1 (.) <$> many1 maximumOp
  where
    maximumOp :: Parser (Index -> Index)
    maximumOp =
      do
        try $ do
          m_reserved "max"
          m_symbol "["
        ivar <- m_identifier
        m_reservedOp "<"
        i <- parseIndex
        m_symbol "]"
        return $ BoundedMax ivar i

manySumOp :: Parser (Index -> Index)
manySumOp = foldr1 (.) <$> many1 sumOp
  where
    sumOp :: Parser (Index -> Index)
    sumOp =
      do
        try $ do
          m_reserved "sum"
          m_symbol "["
        ivar <- m_identifier
        m_reservedOp "<"
        i <- parseIndex
        m_symbol "]"
        return $ BoundedSum ivar i
  
delimitedIndex :: Parser Index
delimitedIndex =
  m_parens parseIndex
    <|> parseNat
    <|> parseIndexVariable
    <|> parseMax
    <?> "delimited index"

-- Parses an index expression
parseIndex :: Parser Index
parseIndex =
  let -- Usual arithmetic operator associativity and precedence
      indexOperators =
        [ [Infix multOp AssocLeft],
          [Infix plusOp AssocLeft],
          [Infix minusOp AssocLeft],
          [Prefix manyMaximumOp, Prefix manySumOp]
        ]
   in buildExpressionParser indexOperators delimitedIndex <?> "index expression"
