module Interpreter.Expr where

import PQ.Expr
import PQ.Type
import Interpreter.RuntimeError

import Debug.Trace (trace)
import qualified Data.Set as Set
import Data.List (foldl')

-- semantic rules 
eval :: Expr -> Either RuntimeError Expr
-- UNIT VALUE
eval (EUnit) = undefined

-- VARIABLE
eval (EVar x) = Right (EVar x)

-- PAIR
eval (ETuple tpl) = do
  tpl' <- mapM eval tpl -- ??
  Right (ETuple tpl')

-- ABSTRACTION
eval (EAbs p typ e) = Right $ EAbs p typ e -- can't be reduced by themselves

-- LIFT
eval (ELift e) = Right (ELift e)

-- (ECons e1 (ECons ... (ECons en ENil)
-- ENIL
eval (ENil typ) = Right (ENil typ) -- ??

-- CONS 
eval (ECons e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2 -- ??
  Right (ECons e1' e2')

-- FOLD 
eval (EFold _ _ _ ) = undefined
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
eval (EApp abs arg) = do -- FIXME maybe the first term needs to be first reduced into an abs instead of searching it immediately
  abs' <- eval abs
  case abs' of
    EAbs p _ body -> do
      arg' <- eval arg
      eval $ sub p arg' body
    _ -> Left $ RuntimeError "The first argument of EApp did not reduce to an abstraction."

-- APPLY
eval (EApply e1 e2) = undefined

-- BOX 
eval (EBox _ _) = undefined
-- eval (EBox T t) = do
--   case eval t of
--     ELift n -> do
--       (Q, l) <- freshlabels T
--       case eval EApp (id_Q, (n l)) of
--         (D, l') -> Right ??(l, D, l)
--         _ -> Left "Type error: (idQ,N ℓ) did not reduce to a label ℓ'"   
--     _ -> Left "Type error: the body M of EBox did not redice to ELift."

-- FORCE
eval (EForce e) = do
  e' <- eval e
  case e' of
    ELift m' -> eval m'
    _ -> Left $ RuntimeError "No ELift found inside EForce."

-- LET
eval (ELet p e1 e2) = case p of
  PHole -> undefined
  PVar pvar -> do
    e1' <- eval e1
    eval (sub p e1' e2)
  PTuple ptpl -> -- I give for granted that e1 is a ETuple of the same length
    let ETuple etpl = e1 in
    eval $ foldl' (\acc (ptrn,expr) -> sub ptrn expr acc) e2 (zip ptpl etpl)
  PCons _ _ -> undefined

-- ANNOTATION

-- INDEX ABSTRACTION

-- INDEX APPLICATION

-- CONSTANT


-- TYPE ASSUMPTION

-- unmatched cases
eval _ = Left $ RuntimeError "Unspecified error during evaluation. Default case reached."

varsInPattern :: Pattern -> Set.Set VariableId
varsInPattern PHole = Set.empty
varsInPattern (PVar v) = Set.singleton v
varsInPattern (PTuple ps) = Set.unions (map varsInPattern ps)
varsInPattern (PCons p1 p2) = Set.union (varsInPattern p1) (varsInPattern p2)

freeVars :: Expr -> Set.Set VariableId
freeVars EUnit = Set.empty
freeVars (EVar x) = Set.singleton x
freeVars (ETuple es) = Set.unions (map freeVars es)
freeVars (EAbs p _ body) = freeVars body `Set.difference` varsInPattern p
freeVars (ELift e) = freeVars e
freeVars (ENil _) = Set.empty
freeVars (ECons e1 e2) = Set.union (freeVars e1) (freeVars e2)
freeVars (EFold e1 e2 e3) = Set.unions (map freeVars [e1,e2,e3])
freeVars (EApp e1 e2) = Set.union (freeVars e1) (freeVars e2)
freeVars (EApply e1 e2) = Set.union (freeVars e1) (freeVars e2)
freeVars (EBox _ e) = freeVars e
freeVars (EForce e) = freeVars e
freeVars (ELet p e1 e2) =
  Set.union (freeVars e1) (freeVars e2 `Set.difference` varsInPattern p)
freeVars (EAnno e _) = freeVars e
freeVars (EIAbs _ e) = freeVars e
freeVars (EIApp e _) = freeVars e
freeVars (EConst _) = Set.empty
freeVars (EAssume e _) = freeVars e

-- we rename by adding '
freshVar :: Set.Set VariableId -> VariableId -> VariableId
freshVar used x = head $ dropWhile (`Set.member` used) candidates
  where
    candidates = [x ++ replicate n '\'' | n <- [1..]]

rename :: VariableId -> VariableId -> Expr -> Expr
rename _ _ _ = undefined

sub :: Pattern -> Expr -> Expr -> Expr
sub trgt new body = case body of
  EUnit -> EUnit

  EVar vid -> case trgt of
    PVar y -> if vid==y then new else body
    _ -> body -- can't match an EVar with anything else than a PVar

  ETuple _ -> undefined

  EAbs p typ e
    | p == trgt -> body -- the occurences are bounded by the p of the EAbs
    | otherwise -> case p of
        PHole -> undefined
        PVar pvar -> 
          if pvar `Set.member` freeVars new
            then 
              let pvar' = freshVar (Set.union (freeVars new) (freeVars e)) pvar
                  e' = rename pvar pvar' e
              in EAbs (PVar pvar') typ (sub trgt new e')
            else
              EAbs p typ (sub trgt new e) -- no risk of capturing
        
        PTuple _ -> undefined
        
        PCons _ _ -> undefined

  ELift e -> ELift $ sub trgt new e

  ENil _ -> undefined

  ECons _ _ -> undefined

  EFold _ _ _ -> undefined

  EApp e1 e2 -> EApp (sub trgt new e1) (sub trgt new e2)

  EApply _ _ -> undefined

  EBox _ _ -> undefined

  EForce e -> EForce $ sub trgt new e

  ELet p e1 e2 -> 
    let e1' = sub trgt new e1 -- we can always sub in the first expression
    in if p == trgt
      then ELet p e1' e2 -- the occurences are bounded by the p of the EAbs
      else case p of
        PHole -> undefined
        PVar pvar ->
          if pvar `Set.member` freeVars new
            then 
              let pvar' = freshVar (freeVars new `Set.union` freeVars e2 `Set.union` varsInPattern trgt) pvar
                  e2' = sub p (EVar pvar') e2
              in ELet (PVar pvar') e1' (sub trgt new e2')
            else
              ELet p e1' (sub trgt new e2)

        PTuple tpl -> 
          let 
            ETuple e1tpl = e1
            e1tpl' = map (sub trgt new) e1tpl
            e1' = ETuple e1tpl'
          in if trgt `elem` tpl
            then ELet p e1' e2
            else ELet p e1' (sub trgt new e2)

        PCons _ _ -> undefined

  EAnno _ _ -> undefined

  EIAbs _ _ -> undefined

  EIApp _ _ -> undefined

  EConst _ -> undefined

  EAssume _ _ -> undefined


-- wraps an expression with abstraction on his args in order to be able to lift it
wrapExpr :: Expr -> [Pattern] -> Maybe Type -> Expr
wrapExpr e [] _ = e
wrapExpr e _ Nothing = e
wrapExpr e (p:ps) (Just typ) = case typ of
  TUnit -> undefined
  TWire _ _ -> EAbs p typ e
  TTensor _ -> EAbs p typ e
  TCirc _ _ _ -> undefined
  TArrow typ1 _ _ _ -> wrapExpr e (p:ps) (Just typ1) -- only expand on the ifrst argument of TArrow
  TBang _ typ -> wrapExpr e (p:ps) (Just typ) -- remove the TBang
  TList _ _ _ -> EAbs p typ e
  TVar _ -> undefined
  TIForall ivarid typ' _ _ -> EIAbs ivarid (wrapExpr e ps (Just typ'))
