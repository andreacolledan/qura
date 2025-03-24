module Lang.Type.Parse
  ( typeExpression,
  )
where

import Control.Monad.Combinators.Expr
import Index.Parse
import Lang.Type.AST
import Parser
import Text.Megaparsec

--- Delimited Types ---

-- | Parse "@Circ[i](btype1, btype2)@" as @Circ i btype1 btype2@.
circ :: Parser Type
circ = do
  keyword "Circ"
  i <- whenGlobalAnalysis $ brackets indexExpression
  (btype1, btype2) <- parens $ do
    btype1 <- typeExpression
    _ <- comma
    btype2 <- typeExpression
    return (btype1, btype2)
  return $ TCirc i btype1 btype2

-- | Parse "@Bit@" as @TWire Bit@.
bit :: Parser Type
bit = do
  keyword "Bit"
  i <- whenLocalAnalysis $ braces indexExpression
  return (TWire Bit i)

-- | Parse "@Qubit@" as @TWire Qubit@.
qubit :: Parser Type
qubit = do
  keyword "Qubit"
  i <- whenLocalAnalysis $ braces indexExpression
  return (TWire Qubit i)

-- | Parse "@()@" as @TUnit@.
unitType :: Parser Type
unitType = try emptyParens >> return TUnit

-- | Parse "@(t1, t2, ..., tn)@" as @TPair (TPair ... (TPair t1 t2) ... tn)@.
-- Sugar: n-tensors are desugared left-associatively.
tensor :: Parser Type
tensor = do
  try $ do
    elems <- parens $ sepBy1 typeExpression comma
    return $ if length elems >= 2
      then TTensor elems
      else head elems -- if just (t1) default to returning t1 by itself

--- Type Operators ---

-- | Parse @-o[i,j]@ as the @TArrow _ _ i j@ type constructor.
arrowOperator :: Parser (Type -> Type -> Type)
arrowOperator = do
  arrow
  resAnno <- whenGlobalAnalysis $ brackets $ do
    i <- indexExpression
    _ <- comma
    j <- indexExpression
    return (i, j)
  return $ case resAnno of
    Just (i, j) -> \t1 t2 -> TArrow t1 t2 (Just i) (Just j)
    Nothing -> \t1 t2 -> TArrow t1 t2 Nothing Nothing

-- | Parse "@List[id < i]@" as the prefix operator @TList id i _@.
listOperator :: Parser (Type -> Type)
listOperator = do
  keyword "List"
  (id, i) <- brackets $ do
    id <- identifier <|> (underscore >> return "_") -- allow to parse "_" as a list type index
    lessSign
    i <- indexExpression
    return (id, i)
  return $ TList id i

-- | Parse "@![i]@" as the prefix operator @TBang i@.
bangOperator :: Parser (Type -> Type)
bangOperator = do
  bang
  i <- whenGlobalAnalysis $ brackets indexExpression
  return $ TBang i

--- Low-precedence Prefix Operators ---
-- These operators have the least precedence, i.e. they extend until the end of the expression.
-- The 'makeExprParser' function does not deal well with them, so it is better to define them like this.

-- | Parse "@forall[i,j] id. typ@" as @TIForall id typ i j@.
forallType :: Parser Type
forallType = do
  keyword "forall"
  resAnno <- whenGlobalAnalysis $ brackets $ do
    i <- indexExpression
    comma
    j <- indexExpression
    return (i, j)
  id <- identifier
  dot
  typ <- typeExpression
  return $ case resAnno of
    Just (i, j) -> TIForall id typ (Just i) (Just j)
    Nothing -> TIForall id typ Nothing Nothing


--- Type Expression Parsing ---

-- | Parse a type with certain boundaries.
baseType :: Parser Type
baseType =
  unitType
    <|> bit
    <|> qubit
    <|> tensor
    <|> circ
    <|> parens typeExpression
    <|> forallType
    <?> "base type"

-- | Table of type operators, ranked from highest to lowest precedence.
typeOperatorTable :: [[Operator Parser Type]]
typeOperatorTable =
  [ [Prefix bangOperator],
    [Prefix listOperator],
    [InfixR arrowOperator]
  ]

-- | Parse a type expression.
typeExpression :: Parser Type
typeExpression = makeExprParser baseType typeOperatorTable <?> "type expression"