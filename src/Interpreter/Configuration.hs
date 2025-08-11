module Interpreter.Configuration where

import Interpreter.RuntimeError
import Interpreter.Expr(exprFreeVars)
import PQ.Expr
import Circuit
import PrettyPrinter (Pretty (..))

import Debug.Trace (trace)
import qualified Data.Set as Set

-- TODO a configuration is a pair of a Circuit (to be defined) and a term
-- Corresponds to (C,M) in the original paper
data Configuration = Config {
    circuit :: Circuit, -- TBD
    term :: Expr
} deriving Show

startConfigEvaluation :: Configuration -> Either RuntimeError Configuration
startConfigEvaluation (Config circ expr) = 
  trace (""
      -- ++ "\n\n-- Pretty Circuit:\n"++ pretty circ
      ++ "\n\n-- Pretty Expr:\n"++pretty expr
      ++ "\n\n-- Circuit:\n"++ pretty circ
      -- ++ "\n\n-- Full Expr:\n"++show expr
    ) $ 
    evalConfiguration (Config circ expr)

evalConfiguration :: Configuration -> Either RuntimeError Configuration
evalConfiguration (Config circ expr) = 
  case expr of
    EUnit -> undefined

    EVar x -> Right $ Config circ expr

    ETuple tpl -> undefined -- fold on the config?

    EAbs p typ e -> 
      -- this term does not reduce by itself, but if the pattern is a tuple
      -- we unfold the abstraction into multiple abstractions
      -- (maybe)
      Right $ Config circ expr

    ELift e -> Right $ Config circ expr

    ENil e -> undefined

    ECons e1 e2 -> do -- idk?
      (Config circ' e1') <- evalConfiguration (Config circ e1)
      (Config circ'' e2') <- evalConfiguration (Config circ' e2)
      Right $ Config circ'' (ECons e1' e2')

    EFold _ _ _ -> undefined

    EApp abs arg -> do
      (Config circ' abs') <- evalConfiguration (Config circ abs)
      case abs' of 
        EAbs p _ body -> do
          (Config circ'' arg') <- evalConfiguration (Config circ' arg)
          evalConfiguration $ subInConfiguration p arg' (Config circ'' body)

        _ -> Left $ RuntimeError "The first argument of EApp did not reduce to an abstraction."

    EApply e1 e2 -> undefined
    -- EApply e1 e2 -> do
    --   (Config circ' )
    --   (Config circ'' e2') <- evalConfiguration (Config circ' e2)

    EBox typ e -> undefined

    EForce e -> do
      (Config circ' e') <- evalConfiguration (Config circ e)
      case e' of
        ELift m' -> evalConfiguration (Config circ' m')
        _ -> Left $ RuntimeError "No ELift found inside EForce."

    ELet p e1 e2 -> case p of
      PHole -> undefined
      PVar _ -> do
        (Config circ' e1') <- evalConfiguration (Config circ e1)
        evalConfiguration $ subInConfiguration p e1' (Config circ' e2)
      
      PTuple _ -> -- e1 is an ETuple
        evalConfiguration $ subInConfiguration p e1 (Config circ e2)

      PCons e1 e2 -> undefined

    EAnno _ _ -> undefined
    EIAbs _ _ -> undefined
    EIApp _ _ -> undefined
    EConst _ -> undefined
    EAssume _ _ -> undefined

renameWithContext :: VariableId -> VariableId -> Configuration -> Configuration
renameWithContext _ _ _ = undefined

-- do we need the context?
freshVar :: Set.Set VariableId -> VariableId -> VariableId
freshVar _ _ = undefined 

-- I do believe that with every rename we have to update the context.
-- (otherwise how do I know to which wire the operation is referring to?)
subInConfiguration :: Pattern -> Expr -> Configuration -> Configuration
subInConfiguration trgt new config = 
  let (Config circ body) = config in
  case trgt of
    PHole -> undefined -- typechecked?

    PVar pvar -> 
      case body of
        EUnit -> config

        EVar vid -> case trgt of
          PVar y -> if vid==y
            then Config circ new -- do I add new args to Q?
            else config
          _ -> undefined -- trying to match a PVar with something else than an EVar

        ETuple (et:ets) ->
          let 
            PTuple (pt:pts) = trgt
            (Config circ' et') = subInConfiguration pt et config
            (Config circ'' (ETuple ets')) = subInConfiguration 
                                    (PTuple pts)
                                    (ETuple ets)
                                    (Config circ' body) -- <--- whenever I do something like this, do I need to ensure that the labels are 'compatible'?
          in Config circ'' (ETuple (et':ets'))
        ETuple [] -> Config circ (ETuple [])

        EAbs p typ e
          | p == trgt -> config
          | pvar `Set.member` exprFreeVars new -- do I need the circuit to check exprFreeVars?
              -> do
                let pvar' = freshVar (Set.union (exprFreeVars new) (exprFreeVars e)) pvar
                let (Config circ' e') = renameWithContext pvar pvar' (Config circ e)
                let (Config circ'' e'') = subInConfiguration trgt new (Config circ' e')
                Config circ'' (EAbs (PVar pvar') typ e'')
          | otherwise -> do
            let (Config circ' e') = subInConfiguration trgt new (Config circ e)
            Config circ' (EAbs p typ e')

        ELift e -> 
          let (Config circ' e') = subInConfiguration trgt new (Config circ e)
          in Config circ' (ELift e')

        ENil e -> undefined

        ECons e1 e2 -> undefined

        EFold e1 e2 e3 -> undefined

        EApp e1 e2 -> 
          let
            (Config circ' e1') = subInConfiguration trgt new (Config circ e1)
            (Config circ'' e2') = subInConfiguration trgt new (Config circ' e2)
          in Config circ'' (EApp e1' e2')

        EApply e1 e2 -> undefined

        EBox e1 e2 -> undefined

        EForce e -> 
          let (Config circ' e') = subInConfiguration trgt new (Config circ e)
          in Config circ' (EForce e')

        ELet p e1 e2 -> case p of 
          PHole -> undefined

          PVar pLet ->
            let (Config circ' e1') = subInConfiguration trgt new (Config circ e1)
            in if pLet `Set.member` exprFreeVars new
            then 
              let pLet' = freshVar (exprFreeVars new `Set.union` exprFreeVars e2 `Set.union` Set.singleton pvar) pLet
                  (Config circ'' e2') = renameWithContext pLet pLet' (Config circ' e2)
                  (Config circ''' e2'') = subInConfiguration trgt new (Config circ'' e2')
              in Config circ''' (ELet (PVar pLet') e1' e2'')
            else 
              let (Config circ'' e2') = subInConfiguration trgt new (Config circ' e2)
              in Config circ' (ELet p e1' e2')
            
          PTuple tpl ->
            let 
              ETuple e1tpl = e1
              unfoldedTupleExpr = unfoldLetTuple tpl e1tpl e2
            in subInConfiguration trgt new (Config circ unfoldedTupleExpr)
              where
                unfoldLetTuple :: [Pattern] -> [Expr] -> Expr -> Expr
                unfoldLetTuple [] [] body = body
                unfoldLetTuple (p:ps) (e:es) body = ELet p e (unfoldLetTuple ps es body)
                -- typechecked?
                unfoldLetTuple _ _ _ = error "unfoldLetTuple: pattern and expression lists must have same length"                
          
          PCons _ _ -> undefined

        EAnno e typ-> undefined

        EIAbs vid e-> undefined

        EIApp e i -> undefined

        EConst c -> undefined

        EAssume e typ -> undefined


    PTuple (et:ets) -> -- subbing a tuple is the same as having a chain of single substitutions
      undefined

    PCons _ _ -> undefined