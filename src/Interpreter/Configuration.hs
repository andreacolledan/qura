module Interpreter.Configuration where

import Interpreter.RuntimeError
import Interpreter.Expr
import PQ.Expr
import Circuit
import PrettyPrinter (Pretty (..))

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
      ++ "\n\n-- Pretty Circuit:\n"++ pretty circ
      ++ "\n\n-- Pretty Expr:\n"++pretty expr
      ++ "\n\n-- Circuit:\n"++ pretty circ
      ++ "\n\n-- Full Expr:\n"++show expr
    ) $ 
    evalConfiguration (Config circ expr)

evalConfiguration :: Configuration -> Either RuntimeError Configuration
evalConfiguration (Config circ expr) = 
-- TODO: move here the derivation rules from Expr and bring the circuit during evaluation
  -- expr' <- eval expr
  -- Right $ Config circ expr'
  case expr of
    EUnit -> undefined

    EVar x -> Right $ Config circ expr

    ETuple tpl -> undefined -- fold on the config?

    EAbs p typ e -> Right $ Config circ expr

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
      PVar pvar -> do
        (Config circ' e1') <- evalConfiguration (Config circ e1)
        evalConfiguration $ subInConfiguration p e1' (Config circ' e2)
      PTuple tpl -> undefined
      PCons e1 e2 -> undefined


subInConfiguration :: Pattern -> Expr -> Configuration -> Configuration
subInConfiguration trgt new config = undefined 