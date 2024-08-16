module Solving.CVC5
  ( querySMT,
    withSolver,
    SolverHandle,
    smtBoundedMaxGeneric
  )
where

import Control.Monad
import Control.Monad.State (MonadState (..), State, evalState)
import qualified Data.HashSet as Set
import Index.AST
import PrettyPrinter
import System.Process as Proc
import System.IO
import Control.Concurrent
import Control.Exception
import Index.Unify
import Data.List

--- SMT SOLVER (CVC5) MODULE ------------------------------------------------------------
---
--- This module contains the logic to communicate with the CVC5 SMT solver.
--- SMT queries from the same program are all written to the same file, which is then
--- passed to the solver. The solver's response is then read from the same file.
-----------------------------------------------------------------------------------------


embedConstraint :: Constraint -> State Int (String, String)
embedConstraint (Eq i j) = do
  (di, ei) <- embedIndex i
  (dj, ej) <- embedIndex j
  return (di ++ dj, "(= " ++ ei ++ " " ++ ej ++ ")")
embedConstraint (Leq i j) = do
  (di, ei) <- embedIndex i
  (dj, ej) <- embedIndex j
  return (di ++ dj, "(<= " ++ ei ++ " " ++ ej ++ ")")

-- | @embedIndex grs lrs grs lrs i@ desugars all bounded operations in index @i@ into fresh, appropriately constrained variables.
-- It returns a pair @(d, i')@ where @d@ is a string containing the applicable SMTLIB declarations and constraints
-- that must precede the main query in the smtlib file and @i'@ is an appropriate encoding of i as an SMTLIB term, where
-- every occurrence of a bounded operation is replaced by the corresponding newly introduced variable.
embedIndex :: Index -> State Int (String, String)
embedIndex (IndexVariable id) = return ("", id)
embedIndex (Number n) = return ("", show n)
embedIndex (Plus i j) = do
  (di, i') <- embedIndex i
  (dj, j') <- embedIndex j
  return (di ++ dj, "(+ " ++ i' ++ " " ++ j' ++ ")")
embedIndex (Max i j) = do
  (di, i') <- embedIndex i
  (dj, j') <- embedIndex j
  return (di ++ dj, "(max " ++ i' ++ " " ++ j' ++ ")")
embedIndex (Mult i j) = do
  (di, i') <- embedIndex i
  (dj, j') <- embedIndex j
  return (di ++ dj, "(* " ++ i' ++ " " ++ j' ++ ")")
embedIndex (Minus i j) = do
  (di, i') <- embedIndex i
  (dj, j') <- embedIndex j
  return (di ++ dj, "(natminus " ++ i' ++ " " ++ j' ++ ")")
embedIndex (BoundedMax id i j) = do
  (d, _, maxName) <- smtBoundedMaxGeneric id i j embedIndex
  return (d, maxName)
embedIndex (BoundedSum id i j) = do
  (d, i', maxName) <- smtBoundedMaxGeneric id i j embedIndex
  return (d, "(* " ++ i' ++ " " ++ maxName ++ ")")
embedIndex i = error $ "Internal error: resource operator was not desugared (embedIndex):" ++ pretty i

smtBoundedMaxGeneric :: IndexVariableId -> Index -> Index -> (Index -> State Int (String, String)) -> State Int (String, String, String)
smtBoundedMaxGeneric id i j embed = do
  count <- get
  put $ count + 1
  let maxName = "_max" ++ show count
  let argMaxName = "_argmax" ++ show count
  (di, i') <- embed i
  s <- get
  (dj, j') <- embed (isub (isubSingleton id (IndexVariable argMaxName)) j)
  put s
  (_, j'') <- embed (isub (isubSingleton id (IndexVariable "_w")) j)
  -- the following declarations must occur before the constraints of the sub-terms
  let d0 =
        "; the following variables stand for the max value and argmax of " ++ pretty (BoundedMax id i j) ++ "\n"
          ++ "(declare-const " ++ maxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ maxName ++ "))\n"
          ++ "(declare-const " ++ argMaxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ argMaxName ++ "))\n"
  -- the following declrations must occur after the constraints of the sub-terms
  let d =
        "; the following block ensures that " ++ maxName ++ " = " ++ pretty (BoundedMax id i j) ++ "\n"
          ++ "(assert (=> (<= " ++ i' ++ " 0) (= " ++ maxName ++ " 0)))\n"
          ++ "(assert (=> (> " ++ i' ++ " 0) (= " ++ maxName ++ " " ++ j' ++ ")))\n"
          ++ "(assert (< " ++ argMaxName ++ " " ++ i' ++ "))\n"
          ++ "(assert (forall ((_w Int)) (=> "
            ++ "(and (<= 0 _w) (< _w " ++ i' ++ "))"
            ++ "(<= " ++ j'' ++ " " ++ j' ++ "))))\n"
  return (d0 ++ di ++ dj ++ d, i', maxName)


containsBoundedSum :: Index -> Bool
containsBoundedSum (BoundedSum {}) = True
containsBoundedSum (IndexVariable _) = False
containsBoundedSum (Number _) = False
containsBoundedSum (Plus i j) = containsBoundedSum i || containsBoundedSum j
containsBoundedSum (Max i j) = containsBoundedSum i || containsBoundedSum j
containsBoundedSum (Mult i j) = containsBoundedSum i || containsBoundedSum j
containsBoundedSum (Minus i j) = containsBoundedSum i || containsBoundedSum j
containsBoundedSum (BoundedMax _ i j) = containsBoundedSum i || containsBoundedSum j
containsBoundedSum i = error $ "Internal error: resource operator was not desugared (containsBoundedSum):" ++ pretty i


-- | @querySMTWithContext qfh c@ queries the CVC5 solver to check if the constraint @c@ holds for every possible assignment of its free variables.
-- It returns @True@ if the constraint holds, @False@ otherwise or if an error occurs in the interaction with the solver.
-- The handle @qfh@ is used to communicate with the SMT solver.
querySMT :: SolverHandle -> [Constraint] -> Constraint -> IO Bool
-- Since bounded sums are overapproximated in SMT, the presence of
-- a bounded sum in an equality, or on the right of a LEQ, makes
-- the constraint undecidable by the SMT. i.e.
-- a <= a' => b <= b' => a' = b' =/=> a = b
-- and b <= b' => a <= b' =/=> a <= b,
-- but a <= a' => a' <= b => a <= b.
querySMT _ _ (Eq i j) | containsBoundedSum i || containsBoundedSum j = return False
querySMT _ _ (Leq _ j) | containsBoundedSum j = return False
-- if bounded sums are not a problem, proceed:
querySMT (sin, sout) hypotheses thesis = do
  hPutStrLn sin $ "\n; PROVE " ++ pretty thesis
  unless (null hypotheses) $ hPutStrLn sin $ "; ASSUMING" ++ intercalate ", " (pretty <$> hypotheses)
  hPutStrLn sin "(push 1)"
  forM_ (ifv thesis `Set.union` ifv hypotheses) $ \id -> do
    -- for each free index variable in c, initialize an unknown natural variable:
    hPutStrLn sin $ "(declare-const " ++ id ++ " Int)"
    hPutStrLn sin $ "(assert (<= 0 " ++ id ++ "))"
  forM_ hypotheses $ \h -> do
    -- for each hypothesis, embed it as an SMTLIB term and assert it:
    let (d, eh) = evalState (embedConstraint h) 0
    hPutStr sin d
    hPutStrLn sin $ "(assert " ++ eh ++ ")"
  -- embed the thesis as an SMTLIB term:
  let (d, ethesis) = evalState (embedConstraint thesis) 0
  hPutStr sin d -- dump the constraints that desugar bounded maxima
  -- try to find a counterexample to c:
  hPutStrLn sin "; assert the negation of the constraint to check if it is valid"
  hPutStrLn sin $ "(assert (not " ++ ethesis ++ "))"
  hPutStrLn sin "(check-sat)"
  hFlush sin
  resp <- hGetLine sout
  -- append the response to the query file as a comment:
  result <- case resp of -- response is sat/unsat for each query so far
    "unsat" -> do
      -- cannot find a counterexample ==> the constraint is valid
      hPutStrLn sin "; founds unsat (valid)"
      return True
    "sat" -> do
      -- found a counterexample ==> the constraint is invalid
      hPutStrLn sin "; found sat (invalid)"
      return False
    [] -> do
      -- empty response is considered an error
      hPutStrLn sin "; got empty response"
      error $ "CVC5 empty response while solving " ++ pretty thesis
    other -> do
      -- any other response is considered an error
      hPutStrLn sin $ "; got response: " ++ other
      error $ "CVC5 unknown response: " ++ other ++ " while solving " ++ pretty thesis
  hPutStrLn sin "(pop 1)"
  return result

-- | @SolverHandle@ is a pair of handles to communicate with the SMT solver.
-- The first handle is used to send queries to the solver, the second handle is used to read the solver's responses.
type SolverHandle = (Handle, Handle)

-- | @withSolver filepath deb action@ initializes a new SMT solver process and runs the action @action@ with the solver handle.
-- The file @filepath@ is used to store the queries and responses of the solver. If @deb@ is @Just debfile@, the queries are also written to the file @debfile@.
withSolver :: Maybe FilePath -> (SolverHandle -> IO r) -> IO r
withSolver deb action = do
  let teePrefix = maybe "" (\debfile -> " tee " ++ debfile ++ " | ") deb
  p@(Just sIn, Just sOut, Just sErr, _) <- createProcess $ (shell $ teePrefix ++ "cvc5 -q --incremental --interactive"){
    std_in = CreatePipe,
    std_out = CreatePipe,
    std_err = CreatePipe
  }
  _ <- forkIO (hGetContents sErr >>= \s -> void (evaluate (length s))) -- drain stderr
  hPutStrLn sIn "(set-logic HO_ALL)" -- TODO this might be made less powerful, check
  hPutStrLn sIn "(define-fun max ((x Int) (y Int)) Int (ite (< x y) y x)) ; max(x,y)" -- define the max function
  hPutStrLn sIn "(define-fun natminus ((x Int) (y Int)) Int (ite (< x y) 0 (- x y))) ; minus(x,y)" -- define the minus function
  outcome <- action (sIn, sOut)
  cleanupProcess p
  return outcome

