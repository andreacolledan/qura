module Interpreter.Configuration where

import Interpreter.RuntimeError
import PQ.Expr
import PQ.Type
import PQ.Index
import Analyzer.Unify
import Circuit
import Circuit.Type
import Circuit.Bundle
import PrettyPrinter (Pretty (..))
import PQ.Constant
import Eval.Index

import Debug.Trace (trace)
import qualified Data.Set as Set

-- a configuration is a pair of a Circuit and a term
-- Corresponds to (C,M) in the original paper
data Configuration = Config {
    circuit :: Circuit,
    term :: Expr
} deriving Show

startConfigEvaluation :: Configuration -> Either RuntimeError Configuration
startConfigEvaluation (Config circ expr) = --trace ("> Circuit Expression:\n"++pretty expr) $ 
  evalConfiguration (Config circ expr)

instance Pretty Configuration where
  pretty (Config circ expr) = pretty circ ++"\n> Expression:\n"++ pretty expr

append :: Circuit -> WireBundle -> WireBundle -> Circuit -> WireBundle -> Configuration
append c k l d l' = 
  let
  -- 1) collect all the names appearing in l d l'
    circCtx = getContext c
    boxCtx = getContext d
    -- circNames = namesInBox (WUnit, c, k)
    -- boxNames = namesInBox (l, d, l')
  -- 2) create a renaming from l to t such that:
    -- the inputs of the box become the labels applied to the box (and rename the whole box accordingly)
    -- the other labels in the box do not match any label in the circuit
    renaming = --trace("\n[append] box: "++show boxCtx++"\ncirc: "++show circCtx++"\nk: "++pretty k++"\nl:"++pretty l)$
      createRenamingWithLC boxCtx circCtx (k,l)
    -- renaming = trace("box: "++show boxNames++"\ncirc: "++show circNames++"\nk: "++pretty k++"\nl:"++pretty l)$
    --   createRenaming boxNames circNames (k,l)
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
  -- 6) return (c', t')
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
evalConfiguration (Config circ expr) = 
  let config = Config circ expr in
  -- trace("\nEvaluating:\n"++pretty expr)$
  -- trace("\nEvaluating:\n"++pretty config)$
  -- trace("\nEvaluating:\n"++show expr)$
  case expr of
    EUnit -> Right config

    EVar x -> Left $ RuntimeError $ "The variable "++show x++" has not been assigned to any value." --"image2.png said that (C,x) evaluates to Error :)"
    -- EVar _ -> Right config -- value

    ELab _ -> Right config

    ETuple [] -> Right config
    ETuple (w:ws) -> do
      Config circ' w' <- evalConfiguration $ Config circ w
      wsconfig <- evalConfiguration $ Config circ' (ETuple ws)
      case wsconfig of 
        Config circ'' (ETuple ws') ->
          Right $ Config circ'' (ETuple $ (w':ws'))
        err -> error $ "[ETuple] ETuple did not reduce to ETuple. Got: " ++ pretty err

    EAbs _ _ _ -> Right config

    ECirc _ _ _ -> Right config

    ELift _ -> Right config

    ENil _ -> Right config

    ECons e1 e2 -> do
      Config circ' e1' <- evalConfiguration (Config circ e1)
      Config circ'' e2' <- evalConfiguration (Config circ' e2)
      Right $ Config circ'' $ ECons e1' e2'

    EFold _ w (ENil _) -> Right $ Config circ w
    EFold fun v w -> do
    -- EFold fun v w -> trace("\nEvaluating:\n"++pretty config)$do
    -- EFold fun v w -> trace("\n[EFold] Evaluating the EFold:\n > "++""++"\n > acc: "++pretty v++"\n > input: "++pretty w)$do
      (Config circ' fun') <- evalConfiguration $ Config circ fun
      case fun' of
        ELift m -> do
          -- try to reduce the input to a ECons first
          Config circ'' w' <- evalConfiguration $ Config circ' w
          -- then evaluate the fold
          foldResult <- evalFold 0 circ'' $ EFold m v w'
          -- foldResult <- trace("\n[EFold] Evaluating the EFold in the circuit:\n"++pretty circ''++"\n[EFold] Fold values:\n > acc: "++pretty v++"\n > input: "++pretty w'++"\n > fold function: "++pretty m)$evalFold 0 circ'' $ EFold m v w'
          -- trace("[EFold] Result:\n"++pretty foldResult)$Right foldResult
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
              -- trace ("  [evalFold] Step " ++ show i ++ ": after applying fold function, new acc = " ++ pretty z') $ pure ()

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

        _ -> Left $ RuntimeError $ "First argument of EFold did not reduce to a Lift, it reduced to:\n"++pretty fun'

    EApp abs arg -> do
      (Config circ' abs') <- evalConfiguration (Config circ abs)
      case abs' of 
        EAbs p _ body -> do
          (Config circ'' arg') <- evalConfiguration (Config circ' arg)
          let body' = psub p arg' body
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
      let expr' = psub p e1' e2
      let circ'' = Config circ' expr' 
      evalConfiguration circ''

    EAnno e _ -> evalConfiguration $ Config circ e

    EIAbs _ _ -> Right $ config

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

          ECirc _ _ _ -> Right $ Config circ' m' -- FIXME is this correct? By doing so I am ignoring the index of the args
        
          -- _ -> Right $ Config circ' m'
          -- _ -> trace(pretty config)$Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs."
          _ -> trace("Error in M@I\nArgs:\n> M:\n "++pretty m++"\n> I:\n"++show i++"\nThe first arg reduced to:\n"++pretty m'++"\nin the circuit\n"++pretty circ)$Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs or EConst."
          
        _ -> Left $ RuntimeError "The index of the EIApp did not reduce to a number."


    EConst c -> Right config

    EAssume e _ -> evalConfiguration $ Config circ e

createBoxedMCNot :: Int -> Expr -- README tbf this is not completely correct beacuse the type of CNot an Toffoli is not really the same as MCNot...
createBoxedMCNot 0 = ECirc WUnit (mkIdCircuit []) WUnit
createBoxedMCNot 1 = -- CNot
  let -- we use boxq because the renaming in append WAS really bad, dont sure if fixed
    ins = WTuple [ WCons (WNil (Just (BWire Qubit))) (WLab "boxq0"), WLab "boxq1" ]
    outs = WTuple [ WCons (WNil (Just (BWire Qubit))) (WLab "boxq2"), WLab "boxq3" ]
    circ = CCons (mkIdCircuit [("boxq"++show i, Qubit) | i <- [0..3]]) CNot ins outs
  in ECirc ins circ outs
createBoxedMCNot 2 = -- Toffoli
  let
    ins = WTuple [ WCons (WCons (WNil (Just (BWire Qubit))) (WLab "boxq0")) (WLab "boxq1"), WLab "boxq2" ]
    outs = WTuple [ WCons (WCons (WNil (Just (BWire Qubit))) (WLab "boxq3")) (WLab "boxq4"), WLab "boxq5" ]
    circ = CCons (mkIdCircuit [("boxq"++show i, Qubit) | i <- [0..5]]) Toffoli ins outs
  in ECirc ins circ outs
createBoxedMCNot m = 
  let
    -- initial wires
    ctrls = [ WLab $ "boxq" ++ show i | i <- [0..(m-1)] ] -- m controls
    trgt = WLab $ "boxtrgt"
    ins = WTuple [ mkConsTyped (Just $ BWire Qubit) ctrls, trgt]
    ancillas = [WLab $ "boxaux" ++ show i | i <- [(m+1)..(2*m)] ] -- m-1 ancillas

    -- init ancillas
    initAncillas = foldl (\acc a -> CCons acc (QInit False) WUnit a ) (mkIdCircuit []) ancillas

    wb = WTuple [head ctrls, head $ drop 1 ctrls, head ancillas]
    step1 = CCons initAncillas Toffoli wb $ suffixWBNames "'" wb
    -- forward = 

    -- names = Set.toList $ namesInCircuit circ
    -- outs = -- collect boxqi and boxtrgt with the most '
    -- circ' = updateCircContext circ $ mkContext [(n, Qubit) | n <- names]
  in trace(pretty step1)$undefined
  -- in ECirc ins circ outs

handleEConst :: Constant -> Int -> Expr
handleEConst (Boxed op) _ = EConst $ Boxed op
handleEConst c i = case c of
  MakeRGate -> EConst $ Boxed $ R i
  MakeRinvGate -> EConst $ Boxed $ Rinv i
  MakeCRGate -> EConst $ Boxed $ CR i
  MakeCRinvGate -> EConst $ Boxed $ CRinv i
  -- MakeMCNot -> createBoxedMCNot i
  MakeMCNot -> -- placeholder
    EConst $ Boxed $ MCNot i
  MakeUnitList -> l
    where
      niltyp = (Just TUnit) -- FIXME   or maybe Nothing?
      l = foldr (\_ acc -> ECons acc EUnit) (ENil niltyp) [1..i]

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
exprToWirebundle (ENil typ) = Right $ WNil $ maybeTypeToBundleType typ
-- likely caused by EApply on a non assigned label (for example if there is no main)
exprToWirebundle e = Left $ RuntimeError ("Cannot convert the Expr:\n> "++show e++"\n to a WireBundle")
