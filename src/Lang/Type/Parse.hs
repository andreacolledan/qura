{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lang.Type.Parse
  ( parseType,
  )
where

import Index.Parse
import Lang.Type.AST
import Text.Megaparsec
import Parser
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.Combinators.Expr

--- TYPE PARSER MODULE -------------------------------------------
---
--- This module defines the parser for PQR types.
--- The parser is built using the Parsec library.
------------------------------------------------------------------

-- Resource annotation parser combinators

parseIfGlobalAnalysis :: Parser a -> Parser (Maybe a)
parseIfGlobalAnalysis p = do
  ParserConfig{parsegra = shouldParse} <- ask
  if shouldParse
    then Just <$> p
    else optional p >> return Nothing

parseIfLocalAnalysis :: Parser a -> Parser (Maybe a)
parseIfLocalAnalysis p = do
  ParserConfig{parselra = shouldParse} <- ask
  if shouldParse
    then Just <$> p
    else optional p >> return Nothing

-- Parses "t1 -o[i,j] t2" as (Arrow t1 t2 i j)
arrowOperator :: Parser (Type -> Type -> Type)
arrowOperator = do
  symbol "-o"
  resAnno <- parseIfGlobalAnalysis $ brackets $ do
    i <- parseIndex
    _ <- comma
    j <- parseIndex
    return (i, j)
  return $ case resAnno of
    Just (i, j) -> \t1 t2 -> TArrow t1 t2 (Just i) (Just j)
    Nothing -> \t1 t2 -> TArrow t1 t2 Nothing Nothing

-- Parses "forall[i,j] id . t" as (IForall id t i j)
forallOperator :: Parser (Type -> Type)
forallOperator = do
  keyword "forall"
  resAnno <- parseIfGlobalAnalysis $ brackets $ do
    i <- parseIndex
    _ <- comma
    j <- parseIndex
    return (i, j)
  id <- identifier
  _ <- symbol "."
  return $ case resAnno of
    Just (i,j) -> \t -> TIForall id t (Just i) (Just j)
    Nothing -> \t -> TIForall id t Nothing Nothing

-- Parses "Circ[i](btype1, btype2)" as (Circ i btype1 btype2)
circ :: Parser Type
circ = do
  symbol "Circ"
  i <- parseIfGlobalAnalysis $ brackets parseIndex
  (btype1, btype2) <- parens $ do
    btype1 <- parseType
    _ <- comma
    btype2 <- parseType
    return (btype1, btype2)
  return $ TCirc i btype1 btype2

-- Parses "Bit" as (TWire Bit)
bit :: Parser Type
bit = do
  keyword "Bit"
  i <- parseIfLocalAnalysis $ braces parseIndex
  return (TWire Bit i)

-- Parses "Qubit" as (TWire Qubit)
qubit :: Parser Type
qubit = do
  keyword "Qubit"
  i <- parseIfLocalAnalysis $ braces parseIndex
  return (TWire Qubit i) 

-- Parses "()" as TUnit
unitType :: Parser Type
unitType = try (keyword "()") >> return TUnit

-- Parses "(t1, t2, ..., tn)" as (TPair (TPair ... (TPair t1 t2) ... tn))
-- Sugar: n-tensors are desugared left-associatively
tensor :: Parser Type
tensor = do
  try $ do
    elems <- parens $ sepBy1 parseType comma
    unless (length elems >= 2) $ fail "Tensors must have at least two elements"
    return $ TTensor elems

-- Parses "List[i]" as a prefix operator t |-> TList i t
listOperator :: Parser (Type -> Type)
listOperator = do
  symbol "List"
  (id,i) <- brackets $ do
    id <- identifier
    symbol "<"
    i <- parseIndex
    return (id,i)
  return $ TList id i

-- Parses "!" as a prefix operator TBang
bangOperator :: Parser (Type -> Type)
bangOperator = do
  symbol "!"
  i <- parseIfGlobalAnalysis $ brackets parseIndex
  return $ TBang i

delimitedType :: Parser Type
delimitedType =
  unitType
  <|> bit
  <|> qubit
  <|> tensor
  <|> parens parseType
  <|> circ
  <?> "type"

-- Parses a type
parseType :: Parser Type
parseType =
  let table =
        [ [Prefix listOperator, Prefix bangOperator],
          [InfixR arrowOperator], -- arrows have lower precedence than bangs and list constructors
          [Prefix forallOperator]
        ]
   in makeExprParser delimitedType table <?> "type"