{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lang.Type.Parse
  ( parseType,
  )
where

import Index.Parse
import Lang.Type.AST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token
import Control.Monad (unless)

--- TYPE PARSER MODULE -------------------------------------------
---
--- This module defines the parser for PQR types.
--- The parser is built using the Parsec library.
------------------------------------------------------------------

typeLang :: LanguageDef st
typeLang =
  emptyDef
    { reservedOpNames = ["->", "-o", "!"],
      reservedNames = ["Bit", "Qubit", "List", "Circ"]
    }

typeTokenParser :: TokenParser st
typeTokenParser@TokenParser
  { parens = m_parens,
    identifier = m_identifier,
    reserved = m_reserved,
    reservedOp = m_reservedOp,
    brackets = m_brackets,
    comma = m_comma,
    commaSep1 = m_commaSep1,
    symbol = m_symbol
  } = makeTokenParser typeLang

-- Parses "t1 -o[i,j] t2" as (Arrow t1 t2 i j)
arrowOperator :: Parser (Type -> Type -> Type)
arrowOperator = do
  m_reservedOp "-o"
  (i, j) <- m_brackets $ do
    i <- parseIndex
    _ <- m_comma
    j <- parseIndex
    return (i, j)
  return $ \t1 t2 -> TArrow t1 t2 (Just i) (Just j)

-- Parses "id ->[i,j] t" as (IForall id t i j)
forallOperator :: Parser (Type -> Type)
forallOperator = do
  id <- m_identifier
  m_reservedOp "->"
  (i, j) <- m_brackets $ do
    i <- parseIndex
    _ <- m_comma
    j <- parseIndex
    return (i, j)
  return $ \t -> TIForall id t (Just i) (Just j)

-- Parses "Circ[i](btype1, btype2)" as (Circ i btype1 btype2)
circ :: Parser Type
circ = do
  m_reservedOp "Circ"
  i <- m_brackets parseIndex
  (btype1, btype2) <- m_parens $ do
    btype1 <- parseType
    _ <- m_comma
    btype2 <- parseType
    return (btype1, btype2)
  return $ TCirc (Just i) btype1 btype2

-- Parses "Bit" as (TWire Bit)
bit :: Parser Type
bit = m_reserved "Bit" >> return (TWire Bit)

-- Parses "Qubit" as (TWire Qubit)
qubit :: Parser Type
qubit = m_reserved "Qubit" >> return (TWire Qubit)

-- Parses "()" as TUnit
unitType :: Parser Type
unitType = try (m_reserved "()") >> return TUnit

-- Parses "(t1, t2, ..., tn)" as (TPair (TPair ... (TPair t1 t2) ... tn))
-- Sugar: n-tensors are desugared left-associatively
tensor :: Parser Type
tensor = do
  try $ do
    elems <- m_parens $ m_commaSep1 parseType
    unless (length elems >= 2) $ fail "Tensors must have at least two elements"
    return $ TTensor elems

-- Parses "List[i]" as a prefix operator t |-> TList i t
listOperator :: Parser (Type -> Type)
listOperator = do
  m_reservedOp "List"
  (id,i) <- m_brackets $ do
    id <- m_identifier
    m_reservedOp "<"
    i <- parseIndex
    return (id,i)
  return $ TList id i

-- Parses "!" as a prefix operator TBang
bangOperator :: Parser (Type -> Type)
bangOperator = do
  m_reservedOp "!"
  i <- m_brackets parseIndex
  return $ TBang (Just i)

delimitedType :: Parser Type
delimitedType =
  unitType
  <|> bit
  <|> qubit
  <|> tensor
  <|> m_parens parseType
  <|> circ
  <?> "type"

-- Parses a type
parseType :: Parser Type
parseType =
  let table =
        [ [Prefix listOperator, Prefix bangOperator],
          [Infix arrowOperator AssocRight], -- arrows have lower precedence than bangs and list constructors
          [Prefix forallOperator]
        ]
   in buildExpressionParser table delimitedType <?> "type"