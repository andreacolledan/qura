module Interpreter.Configuration where

import Interpreter.RuntimeError
import PQ.Expr
import PQ.Type
import PQ.Index
import Analyzer.Unify
import Circuit
import PrettyPrinter (Pretty (..))
import PQ.Constant
import Eval.Index

import Debug.Trace (trace)

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

instance Pretty Configuration where
  pretty (Config circ expr) = pretty circ ++"\n> Expression:\n"++ pretty expr

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
    t = outTypeQOP op
    q = getContext circ
    (q', l) = freshlabels t q
    circ' = CCons circ op k l
    circ'' = updateCircContext circ' q'
    lExpr = wirebundleToExpr l
  in Config circ'' lExpr

evalConfiguration :: Configuration -> Either RuntimeError Configuration
evalConfiguration (Config circ expr) = 
  let config = Config circ expr in
  -- trace("\nEvaluating:\n"++show expr)$case expr of
  -- trace("\nEvaluating:\n"++pretty config)$case expr of
  case expr of
    EUnit -> Right config

    -- EVar x -> Left $ RuntimeError $ "The variable "++show x++" has not been assigned to any value." --"image2.png said that (C,x) evaluates to Error :)"
    EVar _ -> Right config -- value

    ELab _ -> Right config -- value

    ETuple tpl -> Right config -- value

    EAbs _ _ _ -> -- value
      Right config

    ECirc _ _ _ -> Right config

    ELift _ -> Right config

    ENil _ -> Right config -- value

    ECons _ _ -> Right config -- value

    EFold _ _ _ -> undefined

    EApp abs arg -> do
      (Config circ' abs') <- evalConfiguration (Config circ abs)
      case abs' of 
        EAbs p _ body -> do
          (Config circ'' arg') <- evalConfiguration (Config circ' arg)
          evalConfiguration $ Config circ'' $ psub p arg' body

        _ -> Left $ RuntimeError "The first argument of EApp did not reduce to an abstraction."

    EApply e1 e2 -> do
        Config circ' e1' <- evalConfiguration (Config circ e1)
        Config circ'' e2' <- evalConfiguration (Config circ' e2)
        k <- exprToWirebundle e2'
        case e1' of
          ECirc l d l' -> 
            let appended = append circ'' k l d l'
            in Right appended
          EConst (Boxed op) -> 
            let appended = appendEConst circ'' k op
            in Right appended
          _ -> Left $ RuntimeError "First argument of EApply did not reduce to ECirc or EConst."

    EBox typ e -> do
      (Config circ' e') <- evalConfiguration (Config circ e)
      case e' of
        ELift n -> do
          let t = typeToBundleType typ
          let (q,l) = freshlabels t emptyContext
          let lExpr = wirebundleToExpr l
          (Config d lExpr') <- evalConfiguration $ Config (Id q) (EApp n lExpr)
          l' <- exprToWirebundle lExpr'
          Right $ Config circ' (ECirc l d l')
        _ -> Left $ RuntimeError "EBox did not reduce to an ELift"

    EForce e -> do
      (Config circ' e') <- evalConfiguration (Config circ e)
      case e' of
        ELift m' -> evalConfiguration (Config circ' m')
        _ -> Left $ RuntimeError "No ELift found inside EForce."

    ELet p e1 e2 -> do
      (Config circ' e1') <- evalConfiguration (Config circ e1)
      evalConfiguration $ Config circ' $ psub p e1' e2

    EAnno _ _ -> undefined

    EIAbs _ _ -> Right $ config

    -- EIApp m i -> undefined
    EIApp m i -> do
      (Config circ' m') <- evalConfiguration (Config circ m)
      case m' of
        EIAbs ivar n -> do
          -- eval index i and obtain w
          -- let w = trace("\n\nPIPPO is evaluating i:\n>> "++show i++"\n")$i
          let w = Number 0

          -- sub ivar with w in n and obtain (Config circ' n')
          let n' = isub (isubSingleton ivar w) n

          evalConfiguration (Config circ' n')
        
        EConst c -> do
          e <- handleEConst c i
          Right $ Config circ' e

        -- _ -> Right $ Config circ' m'
        -- _ -> trace(pretty config)$Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs."
        err -> trace("Error in M@I\nArgs:\n> M:\n "++show m++"\n> I:\n"++show i++"\nThe first arg reduced to:\n"++show m'++"\nin the circuit\n"++pretty circ)$Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs."

    EConst c -> Right $ Config circ expr

    EAssume _ _ -> undefined


handleEConst :: Constant -> Index -> Either RuntimeError Expr
handleEConst c i = Right $ EConst c