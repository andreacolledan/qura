module Lang.Expr.Parse
  ( parseExpr,
  parsePattern
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
import Circuit
import Control.Monad.Combinators.Expr

--- Delimited Expressions ---

-- parse "@()@" as @EUnit@
unitValue :: Parser Expr
unitValue = emptyParens >> return EUnit <?> "unit value"

-- parse "[]" as ENil
nil :: Parser Expr
nil = emptyBrackets >> return (ENil Nothing) <?> "empty list"

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
    "MakeRGate" -> return $ EConst MakeRGate
    "MakeCRGate" -> return $ EConst MakeCRGate
    "MakeMCNot" -> return $ EConst MakeMCNot
    "MakeUnitList" -> return $ EConst MakeUnitList
    _ -> fail $ "Unrecognized constant \"" ++ name ++ "\""
    <?> "constant"

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


--- Expression Operators ---

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
dollarOp = dollarSign >> return makeApp <?> "application"

-- parse ":: t" as the postfix operator (\e -> EAnno e t), possibly applied multiple times
manyAnnOp :: Parser (Expr -> Expr)
manyAnnOp = foldr1 (flip (.)) <$> some annOp
  where
    annOp :: Parser (Expr -> Expr)
    annOp = do
      doubleColon
      flip EAnno <$> typeExpression
      <?> "type annotation"

-- parse "!:: t" as the postfix operator (\e -> EAssume e t), possibly applied multiple times
manyAssumeOp :: Parser (Expr -> Expr)
manyAssumeOp = foldr1 (flip (.)) <$> some assumeOp
  where
    assumeOp :: Parser (Expr -> Expr)
    assumeOp =
      do
        bangDoubleColon
        flip EAssume <$> typeExpression
        <?> "type assumption"

-- parse "@ i" as the postfix operator (\e -> EIApp e i), possibly applied multiple times
manyIappOp :: Parser (Expr -> Expr)
manyIappOp = foldr1 (flip (.)) <$> some iappOp
  where
    iappOp :: Parser (Expr -> Expr)
    iappOp =
      do
        i <- try $ do
          at
          indexExpression
        return $ flip EIApp i
        <?> "index application"

-- parse ":" as the infix operator Cons
consOp :: Parser (Expr -> Expr -> Expr)
consOp = colon >> return ECons <?> "cons operator"


--- Low-precedence Prefix Operators ---
-- These operators have the least precedence, i.e. they extend until the end of the expression.
-- The 'makeExprParser' function does not deal well with them, so it is better to define them like this.

-- parse "\p :: t . e" as the (EAbs p t e)
abstraction :: Parser Expr
abstraction =
  do
    p <- try $ do
      backslash
      parsePattern
    doubleColon
    typ <- typeExpression
    dot
    EAbs p typ <$> parseExpr
    <?> "abstraction"

-- parse "let p = e1 in e2" as (ELet p e1 e2)
letIn :: Parser Expr
letIn = do
    keyword "let"
    p <- parsePattern
    equalSign
    e1 <- parseExpr
    keyword "in"
    ELet p e1 <$> parseExpr
    <?> "let-in"

-- parse "forall i . e" as (EIAbs i e)
iabs :: Parser Expr
iabs =
  do
    i <- try $ keyword "forall" *> identifier <* dot
    EIAbs i <$> parseExpr
    <?> "index abstraction"



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
phole = underscore >> return PHole

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
          <|> abstraction
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