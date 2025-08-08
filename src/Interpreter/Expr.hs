module Interpreter.Expr where

import PQ.Expr
import PQ.Type
import Interpreter.RuntimeError

-- semantic rules 
eval :: Expr -> Either RuntimeError Expr
-- UNIT VALUE
eval (EUnit) = undefined

-- VARIABLE
eval (EVar x) = Right (EVar x)

-- PAIR
eval (ETuple expr) = do
  expr' <- mapM eval expr -- ??
  Right (ETuple expr')

-- ABSTRACTION
eval (EAbs p typ expr) = Right $ EAbs p typ expr -- can't be reduced by themselves

-- LIFT
-- eval (ELift expr) = Right (ELift expr)

-- (ECons expr1 (ECons ... (ECons en ENil)
-- ENIL
eval (ENil typ) = Right (ENil typ) -- ??

-- CONS 
eval (ECons expr1 expr2) = do
  expr1' <- eval expr1
  expr2' <- eval expr2 -- ??
  Right (ECons expr1' expr2')

-- FOLD 
-- -- kinda incomplete
-- eval (EFold _ w (ENil _)) = Right w
-- eval (EFold f v (ECons w' w)) = do -- where is the index?
--   case eval f of
--     ELift m -> do
--       y <- eval m -- how do I account for index=0?
--       z <- eval (EApp v w)
--       Right (eval (EFold (ELift m) z w')) -- how do I update the index?
--     _ -> Left "Type error: no ELift found inside the fold function in EFold"

-- APPLICATION
eval (EApp (EAbs x _ expr1) expr2) = do -- FIXME maybe the first term needs to be first reduced into an abs instead of searchiing it immediately
  expr2' <- eval expr2
  eval $ sub x expr2' expr1
eval _ = Left $ RuntimeError "M is not a function in: EApp M N."

-- APPLY
-- eval (EApply )

-- BOX 
-- eval (EBox T t) = do
--   case eval t of
--     ELift n -> do
--       (Q, l) <- freshlabels T
--       case eval EApp (id_Q, (n l)) of
--         (D, l') -> Right ??(l, D, l)
--         _ -> Left "Type error: (idQ,N ℓ) did not reduce to a label ℓ'"   
--     _ -> Left "Type error: the body M of EBox did not redice to ELift."

-- FORCE
eval (EForce expr) = do
  expr' <- eval expr
  case expr' of
    ELift m' -> eval m'
    _ -> Left $ RuntimeError "No ELift found inside EForce."

-- LET
eval (ELet p expr1 expr2) = do -- do i need lifting and wrapping?? -> no bcs they have no params??
  expr1' <- eval expr1
  eval (sub p expr1' expr2)

-- ANNOTATION

-- INDEX ABSTRACTION

-- INDEX APPLICATION

-- CONSTANT

-- TYPE ASSUMPTION

-- unmatched cases
eval _ = Left $ RuntimeError "Unspecified error during evaluation. Default case reached."

sub :: Pattern -> Expr -> Expr -> Expr
sub trgt expr body = case body of
  EUnit -> EUnit
  EVar x -> case trgt of
    PVar y -> if x==y then expr else body
  ETuple _ -> undefined
  -- EAbs
  -- ELift
  -- ENil
  -- ECons
  -- EFold _ _ _ -> undefined
  -- EApp
  -- EApply
  -- EBox _ _ -> undefined
  -- EForce
  -- ELet 
  -- EAnno
  -- EIAbs
  -- EIApp
  -- EConst
  -- EAssume
  _ -> undefined

-- wraps an expression with abstraction on his args in roder to be able to lift it
wrapExpr :: Expr -> [Pattern] -> Maybe Type -> Expr
wrapExpr e [] _ = e
wrapExpr e _ Nothing = e
wrapExpr expr (p:ps) (Just typ) = case typ of
  TUnit -> undefined
  TWire _ _ -> EAbs p typ expr
  TTensor _ -> EAbs p typ expr
  TCirc _ _ _ -> undefined
  TArrow typ1 _ _ _ -> wrapExpr expr (p:ps) (Just typ1) -- only expand on the ifrst argument of TArrow
  TBang _ typ -> wrapExpr expr (p:ps) (Just typ) -- remove the TBang
  TList _ _ _ -> EAbs p typ expr
  TVar _ -> undefined
  TIForall ivarid typ' _ _ -> EIAbs ivarid (wrapExpr expr ps (Just typ'))
  _ -> undefined
