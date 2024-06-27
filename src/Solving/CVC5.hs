module Solving.CVC5
  ( querySMTWithContext,
    withSolver,
    SolverHandle,
  )
where

import Control.Monad
import Control.Monad.State (MonadState (..), State, evalState, runState)
import qualified Data.HashSet as Set
import Index.AST
import PrettyPrinter
import System.Process as Proc
import System.IO
import Control.Concurrent
import Control.Exception
import Index.Semantics.Resource

--- SMT SOLVER (CVC5) MODULE ------------------------------------------------------------
---
--- This module contains the logic to communicate with the CVC5 SMT solver.
--- SMT queries from the same program are all written to the same file, which is then
--- passed to the solver. The solver's response is then read from the same file.
-----------------------------------------------------------------------------------------

-- | @embedConstraint rel@ returns a string representing the relation @rel@ in SMTLIB format.
--
-- >>> embedConstraint Eq
-- "="
--
-- >>> embedConstraint Leq
-- "<="
embedConstraint :: IRel -> String
embedConstraint Eq = "="
embedConstraint Leq = "<="

-- | @embedIndex grs lrs grs lrs i@ desugars all bounded operations in index @i@ into fresh, appropriately constrained variables.
-- It returns a pair @(d, i')@ where @d@ is a string containing the applicable SMTLIB declarations and constraints
-- that must precede the main query in the smtlib file and @i'@ is an appropriate encoding of i as an SMTLIB term, where
-- every occurrence of a bounded operation is replaced by the corresponding newly introduced variable.
embedIndex :: GlobalResourceSemantics -> LocalResourceSemantics -> Index -> State Int (String, String)
embedIndex _ _ (IndexVariable id) = return ("", id)
embedIndex _ _ (Number n) = return ("", show n)
embedIndex grs lrs (Plus i j) = do
  (di, i') <- embedIndex grs lrs i
  (dj, j') <- embedIndex grs lrs j
  return (di ++ dj, "(+ " ++ i' ++ " " ++ j' ++ ")")
embedIndex grs lrs (Max i j) = do
  (di, i') <- embedIndex grs lrs i
  (dj, j') <- embedIndex grs lrs j
  return (di ++ dj, "(max " ++ i' ++ " " ++ j' ++ ")")
embedIndex grs lrs (Mult i j) = do
  (di, i') <- embedIndex grs lrs i
  (dj, j') <- embedIndex grs lrs j
  return (di ++ dj, "(* " ++ i' ++ " " ++ j' ++ ")")
embedIndex grs lrs (Minus i j) = do
  (di, i') <- embedIndex grs lrs i
  (dj, j') <- embedIndex grs lrs j
  return (di ++ dj, "(natminus " ++ i' ++ " " ++ j' ++ ")")
embedIndex grs lrs (Maximum id i j) = do
  count <- get
  put $ count + 1
  let maxName = "_max" ++ show count
  let argMaxName = "_argmax" ++ show count
  (di, i') <- embedIndex grs lrs i
  s <- get
  (dj, j') <- embedIndex grs lrs (isub (IndexVariable argMaxName) id j)
  put s
  (_, j'') <- embedIndex grs lrs (isub (IndexVariable "_w") id j)
  -- the following declarations must occur before the constraints of the sub-terms
  let d0 =
        "; the following variables stand for the max value and argmax of " ++ pretty (Maximum id i j) ++ "\n"
          ++ "(declare-const " ++ maxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ maxName ++ "))\n"
          ++ "(declare-const " ++ argMaxName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ argMaxName ++ "))\n"
  -- the following declrations must occur after the constraints of the sub-terms
  let d =
        "; the following block ensures that " ++ maxName ++ " = " ++ pretty (Maximum id i j) ++ "\n"
          ++ "(assert (=> (<= " ++ i' ++ " 0) (= " ++ maxName ++ " 0)))\n"
          ++ "(assert (=> (> " ++ i' ++ " 0) (= " ++ maxName ++ " " ++ j' ++ ")))\n"
          ++ "(assert (< " ++ argMaxName ++ " " ++ i' ++ "))\n"
          ++ "(assert (forall ((_w Int)) (=> "
            ++ "(and (<= 0 _w) (< _w " ++ i' ++ "))"
            ++ "(<= " ++ j'' ++ " " ++ j' ++ "))))\n"
  return (d0 ++ di ++ dj ++ d, maxName)
embedIndex grs lrs (OpOutput op n is) = do
  (ds, is) <- mapAndUnzipM (embedIndex grs lrs) is
  return (concat ds, embedOutput lrs op n is)
embedIndex grs _ Identity = return ("", show $ interpretIdentity grs)
embedIndex grs _ (Wire wt) = return ("", show $ interpretWire grs wt)
embedIndex grs lrs (Sequence i j) = do
  (di, i') <- embedIndex grs lrs i
  (dj, j') <- embedIndex grs lrs j
  return (di ++ dj, smtEmbedSequence grs i' j')
embedIndex grs lrs (Parallel i j) = do
  (di, i') <- embedIndex grs lrs i
  (dj, j') <- embedIndex grs lrs j
  return (di ++ dj, smtEmbedParallel grs i' j')
embedIndex grs lrs (BoundedSequence id i j) = smtEmbedBoundedSequence grs id i j (embedIndex grs lrs)
embedIndex grs lrs (BoundedParallel id i j) = smtEmbedBoundedParallel grs id i j (embedIndex grs lrs)


-- | @querySMTWithContext qfh c@ queries the CVC5 solver to check if the constraint @c@ holds for every possible assignment of its free variables.
-- It returns @True@ if the constraint holds, @False@ otherwise or if an error occurs in the interaction with the solver.
-- The handle @qfh@ is used to communicate with the SMT solver.
querySMTWithContext :: SolverHandle -> GlobalResourceSemantics -> LocalResourceSemantics -> Constraint -> IO Bool
querySMTWithContext (sin, sout) grs lrs c@(Constraint rel i j) = do
  hPutStrLn sin $ "\n; PROVE " ++ pretty c
  hPutStrLn sin "(push 1)"
  forM_ (ifv i `Set.union` ifv j) $ \id -> do
    -- for each free index variable in c, initialize an unknown natural variable:
    hPutStrLn sin $ "(declare-const " ++ id ++ " Int)"
    hPutStrLn sin $ "(assert (<= 0 " ++ id ++ "))"
  let ((ci, i'), n) = runState (embedIndex grs lrs i) 0
  let (cj, j') = evalState (embedIndex grs lrs j) n
  hPutStr sin (ci ++ cj) -- dump the constraints that desugar bounded maxima
  -- try to find a counterexample to c:
  hPutStrLn sin "; assert the negation of the constraint to check if it is valid"
  hPutStrLn sin $ "(assert (not (" ++ embedConstraint rel ++ " " ++ i' ++ " " ++ j' ++ ")))"
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
      error $ "CVC5 empty response while solving " ++ pretty c
    other -> do
      -- any other response is considered an error
      hPutStrLn sin $ "; got response: " ++ other
      error $ "CVC5 unknown response: " ++ other ++ " while solving " ++ pretty c
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

