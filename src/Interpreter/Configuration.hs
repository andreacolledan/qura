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

-- a configuration is a pair of a Circuit and a term
-- Corresponds to (C,M) in the original paper
data Configuration = Config {
    circuit :: Circuit,
    term :: Expr
} deriving Show

startConfigEvaluation :: Configuration -> Either RuntimeError Configuration
startConfigEvaluation (Config circ expr) = 
  -- trace (""
  --     ++ "-- Circuit Expr:\n"++pretty expr
  --   ) $ 
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

appendConst :: Circuit -> WireBundle -> Constant -> Configuration
appendConst circ k (Boxed op) = appendQuantOP circ k op
appendConst circ k _ = undefined

evalConfiguration :: Configuration -> Either RuntimeError Configuration
evalConfiguration (Config circ expr) = 
  let config = Config circ expr in
  -- trace("\nEvaluating:\n"++pretty expr)$
  trace("\nEvaluating:\n"++pretty config)$
  -- trace("\nEvaluating:\n"++show expr)$
  case expr of
    EUnit -> Right config

    EVar x -> Left $ RuntimeError $ "The variable "++show x++" has not been assigned to any value." --"image2.png said that (C,x) evaluates to Error :)"
    -- EVar _ -> Right config -- value

    ELab _ -> Right config -- value

    ETuple tpl -> Right config -- value

    EAbs _ _ _ -> Right config -- value

    ECirc _ _ _ -> Right config

    ELift _ -> Right config

    ENil _ -> Right config -- value

    ECons _ _ -> Right config -- value

    EFold _ w (ENil _) -> Right $ Config circ w
    EFold fun v w -> do
    -- EFold fun v w -> trace("\nEvaluating:\n"++pretty config)$do
    -- EFold fun v w -> trace("\n[EFold] Evaluating the EFold:\n > "++""++"\n > acc: "++pretty v++"\n > input: "++pretty w)$do
      (Config circ' fun') <- evalConfiguration $ Config circ fun
      case fun' of
        ELift m -> do
        -- ELift m -> trace("\n[EFold] Evaluating the EFold in:\n"++pretty circ'++"\n > acc: "++pretty v++"\n > input: "++pretty w)$trace("[EFold] fold function:\n > "++pretty m)$do
          foldResult <- evalFold 0 circ' $ EFold m v w
          -- foldResult <- trace("[EFold] fold function:\n > "++pretty m)$evalFold 0 circ' $ EFold m v w
            -- trace("[EFold] Result:\n"++pretty foldResult)$
          Right foldResult
          where 
            evalFold :: Int -> Circuit -> Expr -> Either RuntimeError Configuration
            -- FOLD-END rule
            evalFold _ circ (EFold _ w (ENil _)) = Right $ Config circ w
            -- FOLD-STEP
            evalFold i circ (EFold m v (ECons w' w)) = do
              -- Apply index i to the lifted function
              let mi = EIApp m (Number i)
              -- trace ("  [evalFold] Step " ++ show i ++ ": applying index " ++ show i ++ " to fold function") $ pure ()

              -- Evaluate the indexed function
              (Config d y) <- evalConfiguration $ Config circ mi
              -- trace ("  [evalFold] Step " ++ show i ++ ": function after applying index:\n  " ++ pretty y) $ pure ()

              -- Apply the function to the accumulator and current element
              (Config e z) <- evalConfiguration $ Config d (EApp y (ETuple [v, w]))
              -- trace ("  [evalFold] Step " ++ show i ++ ": after applying fold function, new acc = " ++ pretty z) $ pure ()

              -- Continue folding over the rest
              -- trace ("  [evalFold] Step " ++ show i ++ ": remaining input = " ++ pretty w') $ pure ()
              step <- evalFold (i + 1) e $ EFold m z w'

              -- Evaluate result at this step to normalize circuit state
              evalConfiguration step

            -- UNDEFINED fallback
            evalFold i circ (EFold a b c) =
              error ("\nUNDEFINED:\nindex: " ++ show i ++
                    "\nEFold a b c\n a: " ++ pretty a ++
                    "\n b: " ++ show b ++
                    "\n c: " ++ show c) 

          --   evalFold :: Int -> Circuit -> Expr -> Either RuntimeError Configuration
          --   -- FOLD-END rule
          --   evalFold _ circ (EFold _ w (ENil _)) = Right $ Config circ w
          --   -- FOLD-STEP
          --   evalFold i circ (EFold m v (ECons w' w)) = do
          --     let mi = EIApp m (Number i)
          --     -- do I have to eval the index?
          --     (Config d y) <- evalConfiguration $ Config circ mi
          --     (Config e z) <- evalConfiguration $ Config d (EApp y (ETuple [v,w]))
          --     step <- evalFold (i+1) e $ EFold m z w'
          --     evalConfiguration step

          --   evalFold i circ (EFold a b c) = error ("\nUNDEFINED:\nindex: "++show i++"\nEFold a b c\na: "++pretty a++"\nb: "++show b++"\nc: "++show c)

        _ -> Left $ RuntimeError $ "First argument of EFold did not reduce to a Lift, it reduced to:\n"++pretty fun'

    EApp abs arg -> do
      (Config circ' abs') <- evalConfiguration (Config circ abs)
      case abs' of 
        EAbs p _ body -> do
          (Config circ'' arg') <- evalConfiguration (Config circ' arg)
          evalConfiguration $ Config circ'' $ psub p arg' body

        err -> Left $ RuntimeError $ "The first argument of EApp did not reduce to an abstraction."

    EApply e1 e2 -> do
        Config circ' e1' <- evalConfiguration (Config circ e1)
        Config circ'' e2' <- evalConfiguration (Config circ' e2)
        k <- exprToWirebundle e2'
        case e1' of
          ECirc l d l' -> 
            Right $ append circ'' k l d l'

          EConst c -> 
            let config' = appendConst circ'' k c
            in Right config'
            
          err -> Left $ RuntimeError $ "First argument of EApply did not reduce to ECirc or EConst.\nGot "++pretty err

    EBox typ e -> do -- TODO: untested
      (Config circ' e') <- evalConfiguration (Config circ e)
      case e' of
        ELift n -> 
          case typeToBundleType typ of
            Just t -> do
              let (q,l) = freshlabels t emptyContext
              let lExpr = wirebundleToExpr l
              (Config d lExpr') <- evalConfiguration $ Config (Id q) (EApp n lExpr)
              l' <- exprToWirebundle lExpr'
              Right $ Config circ' (ECirc l d l')
            
            Nothing -> undefined
            
        _ -> Left $ RuntimeError "EBox did not reduce to an ELift"

    EForce e -> do
      (Config circ' e') <- evalConfiguration (Config circ e)
      case e' of
        ELift m' -> evalConfiguration (Config circ' m')
        _ -> Left $ RuntimeError "No ELift found inside EForce."

    ELet p e1 e2 -> do
      (Config circ' e1') <- evalConfiguration (Config circ e1)
      let expr' = psub p e1' e2
      let circ'' = Config circ' expr' 
      evalConfiguration circ''

    EAnno e typ -> do -- TODO check, I dont have a rule for this
      Config circ' e' <- evalConfiguration $ Config circ e
      Right $ Config circ' $ EAnno e' typ

    EIAbs _ _ -> Right $ config

    EIApp m i -> do
      (Config circ' m') <- evalConfiguration (Config circ m)
      case m' of
        EIAbs ivar n -> do
          -- eval index i and obtain w
          case evalIndex' i of
            Number w -> do
            -- Number w -> trace("[evalConfiguration/EIapp] subbing variable "++show ivar++" with value "++show i++" (="++show w++")")$do
              -- sub ivar with w in n and obtain (Config circ' n')
              let n' = isub (isubSingleton ivar (Number w)) n

              evalConfiguration (Config circ' n')
              -- trace("\n> subbed "++show ivar++" with "++show w++" in\n"++pretty n++"\n> and got:\n"++pretty n')$evalConfiguration (Config circ' n')

            _ -> Left $ RuntimeError "The index of the EIApp did not reduce to a number."
        
        EConst c -> do
          e <- handleEConst c i
          Right $ Config circ' e

        -- _ -> Right $ Config circ' m'
        -- _ -> trace(pretty config)$Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs."
        _ -> trace("Error in M@I\nArgs:\n> M:\n "++pretty m++"\n> I:\n"++show i++"\nThe first arg reduced to:\n"++pretty m'++"\nin the circuit\n"++pretty circ)$Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs."

    EConst c -> Right $ Config circ $ EConst c

    EAssume e typ -> do -- TODO check, I dont have a rule for this
      Config circ' e' <- evalConfiguration $ Config circ e
      Right $ Config circ' $ EAssume e' typ

handleEConst :: Constant -> Index -> Either RuntimeError Expr
handleEConst (Boxed op) _ = Right $ EConst $ Boxed op
handleEConst c (Number i) = case c of
  MakeRGate -> Right $ EConst $ Boxed $ R i
  MakeRinvGate -> Right $ EConst $ Boxed $ Rinv i
  MakeCRGate -> Right $ EConst $ Boxed $ CR i
  MakeCRinvGate -> Right $ EConst $ Boxed $ CRinv i
  MakeMCNot -> undefined
  MakeUnitList -> Right l
    where
      niltyp = (Just TUnit) -- or maybe Nothing?
      l = foldr (\_ acc -> ECons acc EUnit) (ENil niltyp) [1..i]
handleEConst _ _ = Left $ RuntimeError "Index is not a Number."

exprToWirebundle :: Expr -> Either RuntimeError WireBundle
exprToWirebundle EUnit = Right $ WUnit
exprToWirebundle (ELab l) = Right $ WLab l
exprToWirebundle (ETuple ls) = do
  ws <- mapM exprToWirebundle ls
  return (WTuple ws)
exprToWirebundle (ECons h t) = do
  h' <- exprToWirebundle h
  t' <- exprToWirebundle t
  Right $ WCons h' t'
exprToWirebundle (ENil typ) = Right $ WNil $ typeToBundleType typ
-- likely caused by EApply on a non assigned label (for example if there is no main)
exprToWirebundle e = Left $ RuntimeError ("Cannot convert the Expr:\n> "++show e++"\n to a WireBundle")
