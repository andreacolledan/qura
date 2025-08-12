module Interpreter.Configuration where

import Interpreter.RuntimeError
import PQ.Expr
import Circuit
import PrettyPrinter (Pretty (..))
import PQ.Constant

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
      ++ "-- Circuit Expr:\n"++pretty expr
    ) $ 
    evalConfiguration (Config circ expr)

exprToWirebundle :: Expr -> WireBundle
exprToWirebundle EUnit = WUnit
exprToWirebundle (ELab l) = WLab l
exprToWirebundle (ETuple ls) = WTuple $ map exprToWirebundle ls
exprToWirebundle _ = error "unexpected error"

wirebundleToExpr :: WireBundle -> Expr
wirebundleToExpr (WUnit) = EUnit
wirebundleToExpr (WLab l) = ELab l
wirebundleToExpr (WTuple ls) = ETuple $ map wirebundleToExpr ls 


append :: Circuit -> WireBundle -> WireBundle -> Circuit -> WireBundle -> Configuration
append c k l d l' = 
  let
  -- 1) collect all the names appearing in l d l'
    oldNames = namesInBox (l, d, l')
    avoidNames = namesInBox (WUnit, c, k)

  -- 2) create a renaming from l to t so that label in t don't appear in c
    renaming = createRenaming oldNames avoidNames

  -- 3) use the renaming to obtain l d l'-> t d' t'
    (t, d', t') = updateBoxNames renaming (l, d, l')

  -- 4) concat c::d' and obtain c'
    c' = circConcat c d' -- is the last instruction g(t*)->t' already in d'?
    
  in
  -- 5) return (c', t')
  Config c' (wirebundleToExpr t')


appendEConst :: Circuit -> WireBundle -> QuantumOperation -> Configuration
appendEConst circ k op = 
  let
    t = typeOfQuantOP op
    q = getContext circ
    (q', l) = freshlabels t q
    circ' = CCons circ op k l
    circ'' = updateContext circ' q'
  in Config circ'' (wirebundleToExpr l)

evalConfiguration :: Configuration -> Either RuntimeError Configuration
evalConfiguration (Config circ expr) = 
  case expr of
    EUnit -> Right $ Config circ expr

    EVar x -> Right $ Config circ expr -- value

    ETuple tpl -> Right $ Config circ expr -- value

    EAbs p typ e -> -- value
      Right $ Config circ expr

    ELift e -> Right $ Config circ expr

    ENil e -> Right $ Config circ expr -- value

    ECons e1 e2 -> Right $ Config circ expr -- value

    EFold _ _ _ -> undefined

    EApp abs arg -> do
      (Config circ' abs') <- evalConfiguration (Config circ abs)
      case abs' of 
        EAbs p _ body -> do
          (Config circ'' arg') <- evalConfiguration (Config circ' arg)
          evalConfiguration $ subInConfiguration p arg' (Config circ'' body)

        _ -> Left $ RuntimeError "The first argument of EApp did not reduce to an abstraction."

    EApply e1 e2 -> do
        Config circ' e1' <- evalConfiguration (Config circ e1)
        Config circ'' e2' <- evalConfiguration (Config circ' e2)
        let k = exprToWirebundle e2'
        case e1' of
          ECirc l d l' -> Right $ append circ'' k l d l'
          EConst (Boxed op) -> Right $ appendEConst circ'' k op
          _ -> Left $ RuntimeError "First argument of EApply did not reduce to ECirc or EConst."

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
    EConst c -> Right $ Config circ expr
    EAssume _ _ -> undefined

varsInPattern :: Pattern -> Set.Set VariableId
varsInPattern PHole = Set.empty
varsInPattern (PVar v) = Set.singleton v
varsInPattern (PTuple ps) = Set.unions (map varsInPattern ps)
varsInPattern (PCons p1 p2) = Set.union (varsInPattern p1) (varsInPattern p2)

exprFreeVars :: Expr -> Set.Set VariableId
exprFreeVars EUnit = Set.empty
exprFreeVars (EVar x) = Set.singleton x
exprFreeVars (ELab _) = Set.empty -- ??
exprFreeVars (ETuple es) = Set.unions (map exprFreeVars es)
exprFreeVars (EAbs p _ body) = exprFreeVars body `Set.difference` varsInPattern p
exprFreeVars (ECirc _ _ _) = Set.empty -- ??
exprFreeVars (ELift e) = exprFreeVars e
exprFreeVars (ENil _) = Set.empty
exprFreeVars (ECons e1 e2) = Set.union (exprFreeVars e1) (exprFreeVars e2)
exprFreeVars (EFold e1 e2 e3) = Set.unions (map exprFreeVars [e1,e2,e3])
exprFreeVars (EApp e1 e2) = Set.union (exprFreeVars e1) (exprFreeVars e2)
exprFreeVars (EApply e1 e2) = Set.union (exprFreeVars e1) (exprFreeVars e2)
exprFreeVars (EBox _ e) = exprFreeVars e
exprFreeVars (EForce e) = exprFreeVars e
exprFreeVars (ELet p e1 e2) =
  Set.union (exprFreeVars e1) (exprFreeVars e2 `Set.difference` varsInPattern p)
exprFreeVars (EAnno e _) = exprFreeVars e
exprFreeVars (EIAbs _ e) = exprFreeVars e
exprFreeVars (EIApp e _) = exprFreeVars e
exprFreeVars (EConst _) = Set.empty
exprFreeVars (EAssume e _) = exprFreeVars e

renameWithContext :: VariableId -> VariableId -> Configuration -> Configuration
renameWithContext _ _ _ = undefined

-- do we need the context?
freshVar :: Set.Set VariableId -> VariableId -> VariableId
freshVar _ _ = undefined 

-- I do believe that with every rename we have to update the context.
-- (otherwise how do I know to which wire the operation is referring to?)
subInConfiguration :: Pattern -> Expr -> Configuration -> Configuration
subInConfiguration trgt new (Config circ body) = 
  let config = (Config circ body) in
  case trgt of
    PHole -> config

    PVar pvar -> 
      case body of
        EUnit -> config

        EVar vid -> if vid==pvar
          then Config circ new -- do I add new args to Q?
          else config
        
        ELab _ -> config

        ETuple [] -> Config circ (ETuple [])
        ETuple (et:ets) ->
          let 
            -- sub in the first element
            (Config circ' et') = subInConfiguration trgt new (Config circ et)
            -- sub in the remaining elements of the tuple
            (Config circ'' (ETuple ets')) = 
              subInConfiguration trgt new (Config circ' (ETuple ets))
          in Config circ'' (ETuple (et':ets'))

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

        ECirc _ _ _ -> config

        ELift e -> 
          let (Config circ' e') = subInConfiguration trgt new (Config circ e)
          in Config circ' (ELift e')

        ENil e -> config

        ECons e1 e2 -> 
          let
            (Config circ' e1') = subInConfiguration trgt new (Config circ e1)
            (Config circ'' e2') = subInConfiguration trgt new (Config circ' e2)
          in Config circ'' (ECons e1' e2')

        EFold e1 e2 e3 -> undefined

        EApp e1 e2 -> 
          let
            (Config circ' e1') = subInConfiguration trgt new (Config circ e1)
            (Config circ'' e2') = subInConfiguration trgt new (Config circ' e2)
          in Config circ'' (EApp e1' e2')

        EApply e1 e2 -> 
          let
            (Config circ' e1') = subInConfiguration trgt new (Config circ e1)
            (Config circ'' e2') = subInConfiguration trgt new (Config circ' e2)
          in Config circ'' (EApply e1' e2')

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
            
          PTuple tpl -> -- we simply convert the let tuple expression to a chain of lets
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
          
          PCons _ _ -> --undefined
            let 
              unfoldedConsExpr = trace(show trgt)$unfoldLetCons p e1 e2
            in subInConfiguration trgt new (Config circ unfoldedConsExpr)
              where -- this feels a bit scuffed idk
                unfoldLetCons :: Pattern -> Expr -> Expr -> Expr
                unfoldLetCons _ (ENil typ) expr = error "Trying to assign to a PCons a smaller ECons"
                -- the last pattern gets subbed with the remaining list 
                -- (if they have the same length its gonna be the last element and ENil)
                unfoldLetCons (PCons PHole p) e expr = e
                
                unfoldLetCons (PCons ps p) (ECons es e) expr =
                  ELet p e (unfoldLetCons ps es expr)
                
                unfoldLetCons p e expr = expr -- cant unfold yet
                

        EAnno e typ-> undefined

        EIAbs vid e-> undefined

        EIApp e i -> undefined

        EConst c -> config

        EAssume e typ -> undefined


    PTuple (et:ets) -> -- subbing a tuple is the same as having a chain of single substitutions
      undefined

    PCons _ _ -> undefined