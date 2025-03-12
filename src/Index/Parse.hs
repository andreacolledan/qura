{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Index.Parse
  ( parseIndex,
    delimitedIndex
  )
where

import Index.AST
import Text.Megaparsec
import Parser
import Control.Monad.Combinators.Expr

--- INDEX PARSING MODULE ------------------------------------------------------------
---
--- This module contains the logic to parse index expressions.
-------------------------------------------------------------------------------------

-- Parses "n" as (Number n)
parseNat :: Parser Index
parseNat =
  do
    n <- number
    return $ Number $ fromInteger n
    <?> "natural number"

-- Parses an identifier "id "as (IndexVariable id)
parseIndexVariable :: Parser Index
parseIndexVariable =
  do
    v <- identifier
    return $ IVar v
    <?> "index variable"

-- Parses "max(i, j)" as (Max i j)
parseMax :: Parser Index
parseMax =
  do
    try $ do
      keyword "max"
      symbol "("
    i1 <- parseIndex
    symbol ","
    i2 <- parseIndex
    symbol ")"
    return $ Max i1 i2
    <?> "max expression"

multOp :: Parser (Index -> Index -> Index)
multOp =
  do
    symbol "*"
    return Mult
    <?> "multiplication"

plusOp :: Parser (Index -> Index -> Index)
plusOp =
  do
    symbol "+"
    return Plus
    <?> "plus"

minusOp :: Parser (Index -> Index -> Index)
minusOp =
  do
    symbol "-"
    return Minus
    <?> "minus"

manyMaximumOp :: Parser (Index -> Index)
manyMaximumOp = foldr1 (.) <$> some maximumOp
  where
    maximumOp :: Parser (Index -> Index)
    maximumOp =
      do
        try $ do
          keyword "max"
          symbol "["
        ivar <- identifier
        symbol "<"
        i <- parseIndex
        symbol "]"
        return $ BoundedMax ivar i

manySumOp :: Parser (Index -> Index)
manySumOp = foldr1 (.) <$> some sumOp
  where
    sumOp :: Parser (Index -> Index)
    sumOp =
      do
        try $ do
          keyword "sum"
          symbol "["
        ivar <- identifier
        symbol "<"
        i <- parseIndex
        symbol "]"
        return $ BoundedSum ivar i
  
delimitedIndex :: Parser Index
delimitedIndex =
  parens parseIndex
    <|> parseNat
    <|> parseIndexVariable
    <|> parseMax
    <?> "delimited index"

-- Parses an index expression
parseIndex :: Parser Index
parseIndex =
  let -- Usual arithmetic operator associativity and precedence
      indexOperators =
        [ [InfixL multOp],
          [InfixL plusOp],
          [InfixL minusOp],
          [Prefix manyMaximumOp, Prefix manySumOp]
        ]
   in makeExprParser delimitedIndex indexOperators <?> "index expression"
