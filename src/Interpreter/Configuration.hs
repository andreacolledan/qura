module Interpreter.Configuration
  ( startConfigEvaluation,
    Configuration (..),
  )
where

import Analyzer.Unify (HasIndex (isub), isubSingleton)
import Circuit (Circuit (..), circConcat, getContext, updateBoxNames, updateCircContext)
import Circuit.Bundle
  ( WireBundle (..),
    makeRenamingforAppend,
    freshBoxLabels,
    freshlabels,
    maybeTypeToBundleType,
    mergeContexts,
    outputType,
  )
import Circuit.Type (QuantumOperation (..))
import Eval.Index (evalIndexNoHandle)
import Interpreter.RuntimeError (RuntimeError (..))
import PQ.Constant (Constant (..))
import PQ.Expr (Expr (..), psub, wirebundleToExpr)
import PQ.Index (Index (..))
import PQ.Type (Type (..))
import PrettyPrinter (Pretty (..))
import Control.Monad.Error.Class (throwError)
import Panic (panic)
import qualified Data.Map.Strict as Map

-- A configuration is a pair of a Circuit and a term
-- Corresponds to (C,M) in the original paper
data Configuration = Config {
    circuit :: Circuit,
    term :: Expr
} deriving Show

startConfigEvaluation :: Configuration -> Either RuntimeError Configuration
startConfigEvaluation = evalConfiguration

instance Pretty Configuration where
  pretty (Config circ expr) = "Circuit:\n" ++ pretty circ ++ "\nExpression:\n" ++ pretty expr

-- | evalConfiguration config evaluates config according to the big-step semantics of PQ.
-- If successful, it returns a configuration of a circuit and a value,
-- otherwise it returns a RuntimeError object.
evalConfiguration :: Configuration -> Either RuntimeError Configuration
evalConfiguration config@(Config circ expr) =

  case expr of

    EVar x -> panic $ "Unbound variable: " ++ x

    ETuple (firstTerm : restOfTuple) -> do
      Config circ' firstVal <- evalConfiguration $ Config circ firstTerm
      Config circ'' rest <- evalConfiguration $ Config circ' (ETuple restOfTuple)
      case rest of
        ETuple restOfVals -> return $ Config circ'' (ETuple (firstVal : restOfVals))
        _ -> panic $ "Rest of tuple did not reduce to a tuple. Got\n\t" ++ pretty rest

    ECons prefixTerm tailTerm -> do
      Config circ' prefix <- evalConfiguration (Config circ prefixTerm)
      Config circ'' tail <- evalConfiguration (Config circ' tailTerm)
      return $ Config circ'' $ ECons prefix tail

    EFold dupFunTerm startAccTerm listTerm -> do
      Config circ' dupFun <- evalConfiguration $ Config circ dupFunTerm -- reduce step function
      case dupFun of
        ELift fun -> do
          Config circ'' startAcc <- evalConfiguration $ Config circ' startAccTerm -- reduce starting accumulator
          Config circ''' list <- evalConfiguration $ Config circ'' listTerm
          evalFold 0 circ''' fun startAcc list
          where
            -- | evalFold n circ fun acc list is a helper function that implements the semantics of folding
            -- function fun over list with starting accumulator acc and starting step number n
            -- it returns a configuration with the final circuit and final accumulator
            evalFold :: Int -> Circuit -> Expr -> Expr -> Expr -> Either RuntimeError Configuration

            -- FOLD-END rule in the original paper
            evalFold _ circ  _ acc (ENil _) = return $ Config circ acc
            
            -- FOLD-STEP rule in the original paper
            evalFold i circ fun acc (ECons prefix last) = do
              (Config circ' stepFun) <- evalConfiguration $ Config circ (EIApp fun (Number i) ) -- instantiate step function with current iteration number
              -- trace ("  [evalFold] Step " ++ show i ++ ": function after applying index " ++ show i ++ ":\n  " ++ pretty stepFun) $ pure ()

              (Config circ'' newAcc) <- evalConfiguration $ Config circ' (EApp stepFun (ETuple [acc, last])) -- apply it the accumulator and current element
              -- trace ("  [evalFold] Step " ++ show i ++ ": after applying fold function, new acc = " ++ pretty newAcc) $ pure ()

              -- trace ("  [evalFold] Step " ++ show i ++ ": remaining input = " ++ pretty prefix) $ pure () -- fold over remaining prefix
              finalConfig <- evalFold (i + 1) circ'' fun newAcc prefix

              -- Evaluate final config to normalize circuit state (todo: do without this step)
              evalConfiguration finalConfig

            evalFold _ _ _ _ notAList = panic $ "Third argument of fold did not reduce to a list. Got\n\t" ++ pretty notAList

        _ -> panic $ "First argument of fold did not reduce to a lifted term. Got\n\t" ++ pretty dupFun

    EApp funTerm argTerm -> do
      (Config circ' fun) <- evalConfiguration (Config circ funTerm)
      case fun of
        EAbs pattern _ funBody -> do
          (Config circ'' arg) <- evalConfiguration (Config circ' argTerm)
          instancedBody <- psub pattern arg funBody
          evalConfiguration $ Config circ'' instancedBody
        _ -> panic $ "First argument of an application did not reduce to an abstraction. Got\n\t"  ++ pretty fun

    EApply boxedCircTerm bundleTerm -> do
        Config circ' boxedCirc <- evalConfiguration (Config circ boxedCircTerm)
        Config circ'' bundleVal <- evalConfiguration (Config circ' bundleTerm)
        wireBundle <- exprToWirebundle bundleVal -- raises an error if the boxed cirucit did not produce a wirebundle
        case boxedCirc of

          ECirc l d l' ->
            return $ append circ'' wireBundle l d l'

          EConst (Boxed op) ->
            let config' = appendQuantOP circ'' wireBundle op
            in return config'

          _ -> panic $ "First argument of apply did not reduce to a boxed circuit or a circuit constant.\nArgument is\n\t" ++ show wireBundle ++ "\nGot\n\t" ++ pretty boxedCirc

    EBox inputType dupFunTerm -> do
      (Config circ' dupFun) <- evalConfiguration $ Config circ dupFunTerm
      case dupFun of

        ELift fun ->
          case maybeTypeToBundleType inputType of
            Just inputBundleType -> do
              let (inputLContext, inputLabels) = freshBoxLabels inputBundleType
              (Config builtCirc outputLabelExpr) <- evalConfiguration $ Config (Id inputLContext) (EApp fun (wirebundleToExpr inputLabels))
              outputLabels <- exprToWirebundle outputLabelExpr -- raises an error if the boxed cirucit did not produce a wirebundle
              return $ Config circ' $ ECirc inputLabels builtCirc outputLabels

            Nothing -> panic "Box is not annotated with any input wire type"

        _ -> panic $ "First argument of box did not reduce to a lifted term. Got\n\t" ++ show dupFun

    EForce dupTerm -> do
      (Config circ' dupVal) <- evalConfiguration $ Config circ dupTerm
      case dupVal of
        ELift term -> evalConfiguration $ Config circ' term
        _ -> panic $ "First argument of force did not reduce to a lifted term. Got\n\t" ++ show dupVal

    ELet pattern boundTerm bodyTerm -> do
      (Config circ' boundVal) <- evalConfiguration $ Config circ boundTerm
      instancedBodyTerm <- psub pattern boundVal bodyTerm
      evalConfiguration $ Config circ' instancedBodyTerm

    EIApp funTerm indexTerm -> do
      (Config circ' fun) <- evalConfiguration (Config circ funTerm)
      -- eval index indexTerm and obtain number n
      let indexNormalForm = evalIndexNoHandle indexTerm
      case indexNormalForm of
        Number n -> case fun of

          EIAbs ivar funBody -> do
              -- sub ivar with w in n and obtain (Config circ' n')
              let instancedFunBody = isub (isubSingleton ivar (Number n)) funBody
              evalConfiguration (Config circ' instancedFunBody)

          EConst const -> return $ Config circ' $ evalConstantFunctionApplication const n

          -- _ -> trace(pretty config)$Left $ RuntimeError "The first argument of EIApp did not reduce to an EIAbs."
          _ -> panic $ "Something that is not an index function is being applied to an index: " ++ show fun

        _ -> panic $ "Argument of an index function did not reduce to a number. Got\n\t" ++ show indexNormalForm

    -- Type annotations are ignored at runtime

    EAssume e _ -> evalConfiguration $ Config circ e

    EAnno term _ -> evalConfiguration $ Config circ term

    -- Configs with values do not evaluate (i.e. they evaluate to themselves)

    EIAbs _ _ -> return config

    EConst _ -> return config

    EAbs {} -> return config

    ECirc {} -> return config

    ELift _ -> return config

    ENil _ -> return config

    ELab _ -> return config

    EUnit -> return config

    ETuple [] -> return config

-- | append circ targetLabels inputLabels appCirc outputLabels appends circuit appCirc, with input labels
-- inputLabels and output labels outputLabels, to circuit circ, on the wires identified by the labels
-- in targetLabels.
-- It handles the renaming of the labels in appCirc, so as to avoid label capture.
append :: Circuit -> WireBundle -> WireBundle -> Circuit -> WireBundle -> Configuration
append underlyingCirc targetLabels inputLabels appCirc outputLabels =
  let
  -- 1) collect all the names appearing in appCirc
    underlyingCircLabels = Map.keys $ getContext underlyingCirc
    appCircLabels = Map.keys $ getContext appCirc
  -- 2) create a renaming (inputLabels, appCirc, outputLabels) -> (targetLabels, appCirc', outputLabels')
  -- such that the labels in appCirc' and outputLabels' do not occur in underlyingCirc
    renaming = makeRenamingforAppend appCircLabels underlyingCircLabels targetLabels inputLabels
    (_, appCirc', outputLabels') = updateBoxNames renaming (inputLabels, appCirc, outputLabels)
  -- 3) concatenate the two circuits
    finalCirc = circConcat underlyingCirc appCirc'
  -- 4) compute the label context of the resulting circuit
    underlyingCircCtx = getContext underlyingCirc
    appCircCtx' = getContext appCirc'
    newCtx = mergeContexts underlyingCircCtx appCircCtx'
    finalCirc' = updateCircContext finalCirc newCtx
  in
  -- 5) return resulting circuit and the renamed outputs of appCirc'
  Config finalCirc' (wirebundleToExpr outputLabels')

-- | appendQuantOp circ targetLabels op appends quantum operation op to circuit circ on the wires
-- identified by the labels in targetLabels
appendQuantOP :: Circuit -> WireBundle -> QuantumOperation -> Configuration
appendQuantOP circ targetLabels op =
  let
    t = outputType op
    q = getContext circ
    (q', l) = freshlabels t q
    circ' = CCons circ op targetLabels l
    circ'' = updateCircContext circ' q'
    lExpr = wirebundleToExpr l
  in Config circ'' lExpr

-- | evalConstantFunctionApplication fun n evaluates a primitive
-- index function const applied to an input value of n.
-- It returns the output value of the index function.
evalConstantFunctionApplication :: Constant -> Int -> Expr
evalConstantFunctionApplication (Boxed op) _ = EConst $ Boxed op
evalConstantFunctionApplication c n = case c of
  MakeRGate -> EConst $ Boxed $ R n
  MakeRinvGate -> EConst $ Boxed $ Rinv n
  MakeCRGate -> EConst $ Boxed $ CR n
  MakeCRinvGate -> EConst $ Boxed $ CRinv n
  MakeUnitList -> foldr (\_ acc -> ECons acc EUnit) (ENil (Just TUnit)) [1..n]
  _ -> panic $ "Constant not recognized: " ++ show c

-- | exprToWirebundle expr casts expr to a WireBundle, if possible
-- (wire bundles are effectively a subset of Expr).
-- Otherwise, it raises a RuntimeError.
exprToWirebundle :: Expr -> Either RuntimeError WireBundle
exprToWirebundle EUnit = return WUnit
exprToWirebundle (ELab l) = return $ WLab l
exprToWirebundle (ETuple ls) = do
  ws <- mapM exprToWirebundle ls
  return (WTuple ws)
exprToWirebundle (ECons h t) = do
  h' <- exprToWirebundle h
  t' <- exprToWirebundle t
  return $ WCons h' t'
exprToWirebundle (ENil typ) = return $ WNil $ maybeTypeToBundleType typ
-- likely caused by EApply on a non assigned label (for example if there is no main)
exprToWirebundle e = throwError $ RuntimeError ("Cannot convert the Expr:\n> " ++ show e ++ "\n to a WireBundle")
