{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.Index
  ( indexExpression,
  )
where

import Control.Monad.Combinators.Expr
import PQ.Index
import Parser.Core
import Text.Megaparsec

--- Delimited Indices ---

-- | Parse a natural number /n/ as @Number n@.
naturalNumber :: Parser Index
naturalNumber = Number . fromInteger <$> number <?> "natural number"

-- | Parse an identifier /id/ " as @IndexVariable id@.
indexVariable :: Parser Index
indexVariable = IVar <$> identifier <?> "index variable"

-- | Parse "max(i, j)"" as @Max i j@.
binaryMax :: Parser Index
binaryMax = do
    (i1,i2) <- try $ do
      keyword "max"
      parens $ (,) <$> indexExpression <* comma <*> indexExpression
    return $ Max i1 i2
    <?> "max expression"


--- Index Operators ---

-- | Parse "*" as the "Mult _ _" index operator.
multOperator :: Parser (Index -> Index -> Index)
multOperator =
  do
    star
    return Mult
    <?> "multiplication"

-- | Parse "+" as the "Plus _ _" index operator.
plusOperator :: Parser (Index -> Index -> Index)
plusOperator =
  do
    plus
    return Plus
    <?> "plus"

-- | Parse "-" as the "Minus _ _" index operator.
minusOperator :: Parser (Index -> Index -> Index)
minusOperator =
  do
    hyphen
    return Minus
    <?> "minus"


--- Low-precedence Prefix Operators ---
-- These operators have the least precedence, i.e. they extend until the end of the expression.
-- The 'makeExprParser' function does not deal well with them, so it is better to define them like this.

-- | Parse "max[id < i] j" as @BoundedMax id i j@
boundedMaximum :: Parser Index
boundedMaximum = do
  (iterator, bound) <- try $ do
    keyword "max"
    brackets $ (,) <$> identifier <* lessSign <*> indexExpression
  i <- indexExpression
  return $ BoundedMax iterator bound i

-- | Parse "sum[id < i] j" as @BoundedSum id i j@
boundedSum :: Parser Index
boundedSum = do
  keyword "sum" 
  (iterator, bound) <- brackets $ (,) <$> identifier <* lessSign <*> indexExpression
  i <- indexExpression
  return $ BoundedSum iterator bound i


--- Index Expression Parsing

-- | Parse an index with certain boundaries.
baseIndex :: Parser Index
baseIndex =
  parens indexExpression
    <|> naturalNumber
    <|> indexVariable
    <|> binaryMax
    <|> boundedMaximum
    <|> boundedSum
    <?> "base index"

-- | Table of index operators, ranked from highest to lowest precedence.
indexOperatorTable :: [[Operator Parser Index]]
indexOperatorTable =
  [ [InfixL multOperator],
    [InfixL minusOperator],
    [InfixL plusOperator]
  ]

-- | Parse an index expression.
indexExpression :: Parser Index
indexExpression = makeExprParser baseIndex indexOperatorTable <?> "index expression"