{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lang.Expr.Parse
  ( parseExpr
  )
where

import Data.Char
import Index.Parse
import Lang.Type.Parse
import Lang.Expr.AST
import Lang.Library.Constant
import Text.Megaparsec
import Parser
import Control.Monad
import Lang.Expr.Pattern
import Lang.Type.AST
import Circuit
import Control.Monad.Combinators.Expr

--- UNIFIED LANGUAGE PARSER ---------------------------------------------------------------------------------
---
--- This module defines the parser for the unified, practical syntax of PQR.
--- This is the main parser used in the application. It is implemented using the Parsec library.
-----------------------------------------------------------------------------------------------------------

--- ELEMENTARY EXPRESSION PARSERS -----------------------------------------------------------------------------------

-- parse "()" as EUnit
unitValue :: Parser Expr
unitValue =
  do
    symbol "()"
    return EUnit
    <?> "unit value"

-- parse "[]" as ENil
nil :: Parser Expr
nil =
  do
    symbol "[]"
    return $ ENil Nothing
    <?> "empty list"

-- parse a lowercase identifier as a variable
variable :: Parser Expr
variable = try $ do
  name@(first : _) <- identifier
  if not (isUpper first) -- accepts "_", unlike isLower
    then return $ EVar name
    else
      fail "Variable names must start with a lowercase letter"
        <?> "variable"

-- parse an uppercase identifier as a constant
constant :: Parser Expr
constant = try $ do
  name <- identifier
  case name of
    "QInit0" -> return $ EConst (Boxed (QInit False))
    "QInit1" -> return $ EConst (Boxed (QInit True))
    "QDiscard" -> return $ EConst (Boxed QDiscard)
    "Meas" -> return $ EConst (Boxed Meas)
    "CInit0" -> return $ EConst (Boxed (CInit False))
    "CInit1" -> return $ EConst (Boxed (CInit True))
    "CDiscard" -> return $ EConst (Boxed CDiscard)
    "Hadamard" -> return $ EConst (Boxed Hadamard)
    "PauliX" -> return $ EConst (Boxed PauliX)
    "PauliY" -> return $ EConst (Boxed PauliY)
    "PauliZ" -> return $ EConst (Boxed PauliZ)
    "T" -> return $ EConst (Boxed T)
    "CNot" -> return $ EConst (Boxed CNot)
    "CZ" -> return $ EConst (Boxed CZ)
    "CCNot" -> return $ EConst (Boxed CCNot)
    "CCZ" -> return $ EConst (Boxed CCZ)
    "Toffoli" -> return $ EConst (Boxed Toffoli)
    _ -> fail $ "Unrecognized constant \"" ++ name ++ "\""
    <?> "constant"

-- parse "\p :: t . e" as the (EAbs p t e)
lambda :: Parser Expr
lambda =
  do
    p <- try $ do
      symbol "\\"
      parsePattern
    doubleColon
    typ <- parseType
    symbol "."
    e <- parseExpr
    return $ EAbs p typ e
    <?> "abstraction"


-- parse "(e1, e2, ..., en)" as (ETuple [e1, e2, ..., en])
tuple :: Parser Expr
tuple =
  do
    elems <- try $ do
      elems <- parens $ sepBy1 parseExpr comma
      when (length elems < 2) $ fail "Tuples must have at least two elements"
      return elems
    return $ ETuple elems
    <?> "tuple"

-- parse "let p = e1 in e2" as (ELet p e1 e2)
letIn :: Parser Expr
letIn = do
    keyword "let"
    p <- parsePattern
    symbol "="
    e1 <- parseExpr
    keyword "in"
    e2 <- parseExpr
    return $ ELet p e1 e2
    <?> "let-in"

-- parse "[e1, e2, ..., en]" as (ECons e1 (ECons ... (ECons en ENil) ...))
-- Sugar: list literals are desugared right-associatively into nested Cons with Nil at the end
list :: Parser Expr
list =
  do
    elems <- brackets $ sepBy1 parseExpr comma
    return $ foldl ECons (ENil Nothing) elems
    <?> "list literal"

-- parse "fold(e1, e2, e3)" as (EFold e1 e2 e3)
fold :: Parser Expr
fold =
  do
    keyword "fold"
    (e1, e2, e3) <- parens $ do
      e1 <- parseExpr
      comma
      e2 <- parseExpr
      comma
      e3 <- parseExpr
      return (e1, e2, e3)
    return $ EFold e1 e2 e3
    <?> "fold"

-- parse "apply(e1, e2)" as (EApply e1 e2)
apply :: Parser Expr
apply =
  do
    keyword "apply"
    (e1, e2) <- parens $ do
      e1 <- parseExpr
      comma
      e2 <- parseExpr
      return (e1, e2)
    return $ EApply e1 e2
    <?> "apply"

-- parse "forall i . e" as (EIAbs i e)
iabs :: Parser Expr
iabs =
  do
    i <- try $ keyword "forall" *> identifier <* symbol "."
    e <- parseExpr
    return $ EIAbs i e
    <?> "index abstraction"

--- EXPRESSION OPERATOR PARSERS -----------------------------------------------------------------------------------

-- intercept "lift", "force" and "box" as special cases of EApp
makeApp :: Expr -> Expr -> Expr
makeApp (EVar "lift") = ELift
makeApp (EVar "force") = EForce
makeApp (EVar "box") = EBox Nothing
makeApp e = EApp e

-- parse spaces as the infix operator EApp
appOp :: Parser (Expr -> Expr -> Expr)
appOp = return makeApp <?> "application" -- TODO make sure that we do not need to parse anything before returning the operator

-- parse "$" as the infix operator EApp (lowest precedence)
dollarOp :: Parser (Expr -> Expr -> Expr)
dollarOp = symbol "$" >> return makeApp <?> "application"

-- parse ":: t" as the postfix operator (\e -> EAnno e t), possibly applied multiple times
manyAnnOp :: Parser (Expr -> Expr)
manyAnnOp = foldr1 (flip (.)) <$> some annOp
  where
    annOp :: Parser (Expr -> Expr)
    annOp = do
      doubleColon
      t <- parseType
      return $ flip EAnno t
      <?> "type annotation"

-- parse "!:: t" as the postfix operator (\e -> EAssume e t), possibly applied multiple times
manyAssumeOp :: Parser (Expr -> Expr)
manyAssumeOp = foldr1 (flip (.)) <$> some assumeOp
  where
    assumeOp :: Parser (Expr -> Expr)
    assumeOp =
      do
        symbol "!::"
        t <- parseType
        return $ flip EAssume t
        <?> "type assumption"

-- parse "@ i" as the postfix operator (\e -> EIApp e i), possibly applied multiple times
manyIappOp :: Parser (Expr -> Expr)
manyIappOp = foldr1 (flip (.)) <$> some iappOp
  where
    iappOp :: Parser (Expr -> Expr)
    iappOp =
      do
        i <- try $ do
          symbol "@"
          parseIndex
        return $ flip EIApp i
        <?> "index application"

-- parse ":" as the infix operator Cons
consOp :: Parser (Expr -> Expr -> Expr)
consOp = colon >> return ECons <?> "cons operator"


--- PATTERN AND PATTERN OPERATOR PARSERS -----------------------------------------------------------------------------------

-- parse a lowercase identifier as a variable pattern
pvariable :: Parser Pattern
pvariable = try $ do
  name@(first : _) <- identifier
  if not (isUpper first) -- accepts "_", unlike isLower
    then return $ PVar name
    else
      fail "Variable names must start with a lowercase letter"
        <?> "variable pattern"

-- parse (p1, p2, ..., pn) as a pattern (PTuple [p1, p2, ..., pn])
ptuple :: Parser Pattern
ptuple =
  do
    elems <- try $ do
      elems <- parens $ sepBy1 parsePattern comma
      when (length elems < 2) $ fail "Tuple patterns must have at least two elements"
      return elems
    return $ PTuple elems
    <?> "tuple pattern"

phole :: Parser Pattern
phole = hole >> return PHole

-- parse ":" as the infix operator PCons
pconsOp :: Parser (Pattern -> Pattern -> Pattern)
pconsOp = colon >> return PCons <?> "cons operator"

--- TOP-LEVEL PARSERS -----------------------------------------------------------------------------------

-- parse a destructuring pattern
parsePattern :: Parser Pattern
parsePattern = let
  operatorTable =
    [ [InfixL pconsOp]
    ]
  simplePattern =
    pvariable
    <|> ptuple
    <|> parens parsePattern
    <|> phole
  in makeExprParser simplePattern operatorTable <?> "pattern"
      
-- parse a PQR expression whose syntactic bounds are unambiguous
delimitedExpr :: Parser Expr
delimitedExpr =
  unitValue
    <|> nil
    <|> tuple
    <|> list
    <|> parens parseExpr
    <|> apply
    <|> fold
    <|> constant
    <|> variable
    <?> "delimited expression"

-- parse a PQR expression
parseExpr :: Parser Expr
parseExpr =
  let operatorTable =
        [ [InfixL appOp],
          [InfixL consOp],
          [Postfix manyIappOp],
          [Postfix manyAnnOp],
          [Postfix manyAssumeOp],
          [InfixR dollarOp]
        ]
      simpleExpr =
        delimitedExpr
          <|> lambda
          <|> iabs
          <|> letIn
   in makeExprParser simpleExpr operatorTable <?> "expression"

-- parse a PQR program: an expression between optional leading whitespace and EOF
parseProgram :: Parser Expr
parseProgram =
  do
    whitespace
    e <- parseExpr
    eof
    return e
    <?> "program"