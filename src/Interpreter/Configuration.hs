module Interpreter.Configuration
  ( startConfigEvaluation,
    Configuration (..),
  )
where

import Analyzer.Unify (HasIndex (isub), isubSingleton)
import Circuit (Circuit (..), circConcat, getContext, updateBoxNames, updateCircContext)
import Circuit.Bundle
  ( WireBundle (..),
    createRenamingWithLC,
    freshBoxLabels,
    freshlabels,
    maybeTypeToBundleType,
    mergeContexts,
    outTypeQuantOP,
  )
import Circuit.Type (QuantumOperation (..))
import Eval.Index (evalIndexNoHandle)
import Interpreter.RuntimeError (RuntimeError (..))
import PQ.Constant (Constant (..))
import PQ.Expr (Expr (..), psub, wirebundleToExpr)
import PQ.Index (Index (..))
import PQ.Type (Type (..))
import PrettyPrinter (Pretty (..))
import Debug.Trace
import Control.Monad.Error.Class (throwError)

-- A configuration is a pair of a Circuit and a term
-- Corresponds to (C,M) in the original paper
data Configuration = Config {
    circuit :: Circuit,
    term :: Expr
} deriving Show

startConfigEvaluation :: Configuration -> Either RuntimeError Configuration
startConfigEvaluation = evalConfiguration

instance Pretty Configuration where
  pretty (Config circ expr) = "Circuit:\n" ++ pretty circ ++"\n> Expression:\n"++ pretty expr

append :: Circuit -> WireBundle -> WireBundle -> Circuit -> WireBundle -> Configuration
append c k l d l' =
  let
  -- 1) collect all the names appearing in l d l'
    circCtx = getContext c
    boxCtx = getContext d
  -- 2) create a renaming from l to t such that:
    -- the inputs of the box become the labels applied to the box (and rename the whole box accordingly)
    -- the other labels in the box do not match any label in the circuit
    renaming = --trace("\n[append] circ: "++pretty c++"\nboxed: "++pretty d++"\ncircCtx: "++show circCtx++"\nk: "++pretty k++"\nl:"++pretty l)$
      createRenamingWithLC boxCtx circCtx (k,l)
  -- 3) use the renaming to obtain l d l'-> t d' t'
    (t, d', t') = --trace("renaming: "++show renaming)$
      updateBoxNames renaming (l, d, l')
  -- 4) concat c::d' and obtain c'
    c' = circConcat c d'
  -- 5) update label context
    boxCtx' = getContext d'
    newCtx = mergeContexts circCtx boxCtx'
    c'' = updateCircContext c' newCtx
  in
  -- 6) return (c'', t')
  Config c'' (wirebundleToExpr t')

appendQuantOP :: Circuit -> WireBundle -> QuantumOperation -> Configuration
appendQuantOP circ k op =
  let
    t = outTypeQuantOP op
    q = getContext circ
    (q', l) = freshlabels t q
    circ' = CCons circ op k l
    circ'' = updateCircContext circ' q'
    lExpr = wirebundleToExpr l
  in Config circ'' lExpr

evalConfiguration :: Configuration -> Either RuntimeError Configuration
evalConfiguration config@(Config circ expr) =
  case expr of
    EUnit -> Right config

    EVar x -> Left $ RuntimeError $ "The variable " ++ show x++" has not been assigned to any value."

    ELab _ -> Right config

    ETuple [] -> Right config
    ETuple (w:ws) -> do
      Config circ' w' <- evalConfiguration $ Config circ w
      wsconfig <- evalConfiguration $ Config circ' (ETuple ws)
      case wsconfig of
        Config circ'' (ETuple ws') -> Right $ Config circ'' (ETuple (w':ws'))
        err -> error $ "[ETuple] ETuple did not reduce to ETuple. Got: " ++ pretty err

    EAbs {} -> Right config

    ECirc {} -> Right config

    ELift _ -> Right config

    ENil _ -> Right config

    ECons e1 e2 -> do
      Config circ' e1' <- evalConfiguration (Config circ e1)
      Config circ'' e2' <- evalConfiguration (Config circ' e2)
      Right $ Config circ'' $ ECons e1' e2'

    EFold dupFunTerm startAccTerm listTerm -> do
      Config circ' dupFun <- evalConfiguration $ Config circ dupFunTerm -- reduce step function
      case dupFun of
        ELift fun -> do
          Config circ'' startAcc <- evalConfiguration $ Config circ' startAccTerm -- reduce starting accumulator
          Config circ''' list <- evalConfiguration $ Config circ'' listTerm
          evalFold 0 circ''' fun startAcc list
          where
            evalFold :: Int -> Circuit -> Expr -> Expr -> Expr -> Either RuntimeError Configuration

            -- FOLD-END rule in the original paper
            evalFold _ circ  _ startAcc (ENil _) = return $ Config circ startAcc
            
            -- FOLD-STEP rule in the original paper
            evalFold i circ fun acc (ECons prefix last) = do
              (Config circ' stepFun) <- evalConfiguration $ Config circ (EIApp fun (Number i) ) -- instantiate step function with current iteration number
              trace ("  [evalFold] Step " ++ show i ++ ": function after applying index " ++ show i ++ ":\n  " ++ pretty stepFun) $ pure ()

              (Config circ'' newAcc) <- evalConfiguration $ Config circ' (EApp stepFun (ETuple [acc, last])) -- apply it the accumulator and current element
              trace ("  [evalFold] Step " ++ show i ++ ": after applying fold function, new acc = " ++ pretty newAcc) $ pure ()

              trace ("  [evalFold] Step " ++ show i ++ ": remaining input = " ++ pretty prefix) $ pure () -- fold over remaining prefix
              finalConfig <- evalFold (i + 1) circ'' fun newAcc prefix

              -- Evaluate final config to normalize circuit state (todo: do without this step)
              evalConfiguration finalConfig

            evalFold _ _ _ _ notAList = throwError $ RuntimeError $ "Third argument of fold reduced to:\n" ++ pretty notAList ++ "\ninstead of a list."

        _ -> throwError $ RuntimeError $ "First argument of fold reduced to:\n" ++ pretty dupFun ++ "\ninstead of a lifted term."

    EApp abs arg -> do
      (Config circ' abs') <- evalConfiguration (Config circ abs)
      case abs' of
        EAbs p _ body -> do
          (Config circ'' arg') <- evalConfiguration (Config circ' arg)
          body' <- psub p arg' body
          evalConfiguration $ Config circ'' body'

        err -> Left $ RuntimeError $ "The first argument of EApp did not reduce to an abstraction. Got: "  ++pretty err

    EApply e1 e2 -> do
        Config circ' e1' <- evalConfiguration (Config circ e1)
        Config circ'' e2' <- evalConfiguration (Config circ' e2)
        k <- exprToWirebundle e2' -- raises an error if the boxed cirucit did not produce a wirebundle
        case e1' of
          ECirc l d l' ->
            Right $ append circ'' k l d l'

          EConst (Boxed op) ->
            let config' = appendQuantOP circ'' k op
            in Right config'

          err -> Left $ RuntimeError $ "First argument of EApply did not reduce to ECirc or EConst. Argument is "++show k++".\nGot "++pretty err

    EBox typ e -> do
      (Config circ' e') <- evalConfiguration $ Config circ e
      case e' of
        ELift n ->
          case maybeTypeToBundleType typ of
            Just t -> do
              let (q,l) = freshBoxLabels t
              let lExpr = wirebundleToExpr l
              (Config d lExpr') <- evalConfiguration $ Config (Id q) (EApp n lExpr)
              l' <- exprToWirebundle lExpr' -- raises an error if the boxed cirucit did not produce a wirebundle
              Right $ Config circ' $ ECirc l d l'

            Nothing -> error "[eval EBox] Type of box is Nothing."

        _ -> Left $ RuntimeError "EBox did not reduce to an ELift"

    EForce e -> do
      (Config circ' e') <- evalConfiguration $ Config circ e
      case e' of
        ELift m' -> evalConfiguration $ Config circ' m'
        _ -> Left $ RuntimeError "No ELift found inside EForce."

    ELet p e1 e2 -> do
      (Config circ' e1') <- evalConfiguration $ Config circ e1
      expr' <- psub p e1' e2
      let circ'' = Config circ' expr'
      evalConfiguration circ''

    EAnno e _ -> evalConfiguration $ Config circ e

    EIAbs _ _ -> Right config

    EIApp m i -> do
      (Config circ' m') <- evalConfiguration (Config circ m)
      -- eval index i and obtain w
      case evalIndexNoHandle i of
        Number w -> case m' of
          EIAbs ivar n -> do
              -- sub ivar with w in n and obtain (Config circ' n')
              let n' = isub (isubSingleton ivar (Number w)) n

              Config circ'' n'' <- evalConfiguration (Config circ' n')
              Right $ Config circ'' n''

          EConst c -> do
            let e = handleEConst c w
            Right $ Config circ' e

          -- _ -> trace(pretty config)$Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs."
          _ -> Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs or EConst."

        _ -> Left $ RuntimeError "The index of the EIApp did not reduce to a number."

    EConst _ -> Right config

    EAssume e _ -> evalConfiguration $ Config circ e

handleEConst :: Constant -> Int -> Expr
handleEConst (Boxed op) _ = EConst $ Boxed op
handleEConst c i = case c of
  MakeRGate -> EConst $ Boxed $ R i
  MakeRinvGate -> EConst $ Boxed $ Rinv i
  MakeCRGate -> EConst $ Boxed $ CR i
  MakeCRinvGate -> EConst $ Boxed $ CRinv i
  MakeUnitList -> l
    where
      niltyp = Just TUnit
      l = foldr (\_ acc -> ECons acc EUnit) (ENil niltyp) [1..i]
  _ -> error $ "[handleEConst] Constant " ++ show c ++ " is not supported."

exprToWirebundle :: Expr -> Either RuntimeError WireBundle
exprToWirebundle EUnit = Right WUnit
exprToWirebundle (ELab l) = Right $ WLab l
exprToWirebundle (ETuple ls) = do
  ws <- mapM exprToWirebundle ls
  return (WTuple ws)
exprToWirebundle (ECons h t) = do
  h' <- exprToWirebundle h
  t' <- exprToWirebundle t
  Right $ WCons h' t'
exprToWirebundle (ENil typ) = Right $ WNil $ maybeTypeToBundleType typ
-- likely caused by EApply on a non assigned label (for example if there is no main)
exprToWirebundle e = Left $ RuntimeError ("Cannot convert the Expr:\n> "++show e++"\n to a WireBundle")
