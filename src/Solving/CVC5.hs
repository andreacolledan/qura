module Solving.CVC5
  ( querySMT,
    withSolver,
    SolverHandle,
    smtBoundedMaxGeneric
  )
where

import Control.Monad
import Control.Monad.State (MonadState (..), StateT, lift, evalStateT)
import qualified Data.HashSet as Set
import Index.AST
import PrettyPrinter
import System.Process as Proc
import System.IO
import Control.Concurrent
import Control.Exception
import Index.Unify
import Data.List (intercalate)
import Control.Monad.Extra (concatMapM)
import Data.Foldable
import Util (undesugaredPanic)

--- SMT SOLVER (CVC5) MODULE ------------------------------------------------------------
---
--- This module contains the logic to communicate with the CVC5 SMT solver.
-----------------------------------------------------------------------------------------

-- | @ApproximationStrategy@ is an enumeration of the possible approximation strategies when encoding an index as an SMTLIB term.
data ApproximationStrategy = Exact | Overapproximate | Underapproximate

-- | @invert as@ inverts the approximation strategy @as@.
invert :: ApproximationStrategy -> ApproximationStrategy
invert Exact = Exact
invert Overapproximate = Underapproximate
invert Underapproximate = Overapproximate

-- | @embedConstraint c@ turns a `Constraint` @c@ into a pair of strings @(d, ec)@
-- where @d@ is a string containing the SMTLIB declarations and constraints that must precede
-- the main query and @ec@ is an appropriate encoding of @c@ as an SMTLIB term.
embedConstraint :: Constraint -> StateT Int Maybe (String, String)
embedConstraint (Eq i j) = do
  (di, ei) <- embedIndex i
  (dj, ej) <- embedIndex j
  return (di ++ dj, "(= " ++ ei ++ " " ++ ej ++ ")")
embedConstraint (Leq i j) = do
  i' <- lift $ approximate Overapproximate i
  j' <- lift $ approximate Underapproximate j
  (di, ei) <- embedIndex i'
  (dj, ej) <- embedIndex j'
  return (di ++ dj, "(<= " ++ ei ++ " " ++ ej ++ ")")

approximate :: ApproximationStrategy -> Index -> Maybe Index
approximate _ i@(IVar _) = return i
approximate _ i@(Number _) = return i
approximate as (Plus i j) = do
  i' <- approximate as i
  j' <- approximate as j
  return $ Plus i' j'
approximate as (Max i j) = do
  i' <- approximate as i
  j' <- approximate as j
  return $ Max i' j'
approximate as (Mult i j) = do
  i' <- approximate as i
  j' <- approximate as j
  return $ Mult i' j'
approximate as (Minus i j) = do
  i' <- approximate as i
  j' <- approximate (invert as) j
  return $ Minus i' j'
approximate as (BoundedMax id i j) = do
  i' <- approximate as i
  j' <- approximate as j
  return $ BoundedMax id i' j'
approximate as (BoundedMin id i j) = do
  i' <- approximate as i
  j' <- approximate (invert as) j
  return $ BoundedMin id i' j'
approximate Overapproximate (BoundedSum id i j) = approximate Overapproximate (Mult i (BoundedMax id i j)) -- sum[id < i] j <= i * max[id < i] j
approximate Underapproximate (BoundedSum id i j) = approximate Underapproximate (Mult i (BoundedMin id i j)) -- sum[id < i] j >= i * min[id < i] j
approximate Exact (BoundedSum {}) = Nothing -- cannot encode a bounded sum exactly in SMT
approximate _ i = undesugaredPanic "approximate" $ show i

-- | @embedIndex i@ turns index @i@ into a pair of strings @(d, ei)@
-- where @ei@ is an appropriate encoding of @i@ as an SMTLIB term and @d@ is a string containing
-- the SMTLIB declarations and constraints that must precede the encoding of @i@.
-- Note: bounded sums and maxima are handled by `smtBoundedMaxGeneric`: they
-- are desugared into fresh variables subject to appropriate constraints.
embedIndex :: Index -> StateT Int Maybe (String, String)
embedIndex (IVar id) = return ("", id)
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
  count <- get
  put $ count + 1
  let maxName = "_max" ++ show count
  let argMaxName = "_argmax" ++ show count
  (di, i') <- embedIndex i
  s <- get
  (dj, maximumJ) <- embedIndex (isub (isubSingleton id (IVar argMaxName)) j)
  put s
  (_, anyOtherJ) <- embedIndex (isub (isubSingleton id (IVar "_w")) j)
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
          ++ "(assert (=> (> " ++ i' ++ " 0) (= " ++ maxName ++ " " ++ maximumJ ++ ")))\n"
          ++ "(assert (< " ++ argMaxName ++ " " ++ i' ++ "))\n"
          ++ "(assert (forall ((_w Int)) (=> "
            ++ "(and (<= 0 _w) (< _w " ++ i' ++ "))"
            ++ "(<= " ++ anyOtherJ ++ " " ++ maximumJ ++ "))))\n"
  return (d0 ++ di ++ dj ++ d, maxName)
embedIndex (BoundedMin id i j) = do
  count <- get
  put $ count + 1
  let minName = "_min" ++ show count
  let argMinName = "_argmin" ++ show count
  (di, i') <- embedIndex i
  s <- get
  (dj, minimumJ) <- embedIndex (isub (isubSingleton id (IVar argMinName)) j)
  put s
  (_, anyOtherJ) <- embedIndex (isub (isubSingleton id (IVar "_w")) j)
  -- the following declarations must occur before the constraints of the sub-terms
  let d0 =
        "; the following variables stand for the max value and argmax of " ++ pretty (BoundedMin id i j) ++ "\n"
          ++ "(declare-const " ++ minName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ minName ++ "))\n"
          ++ "(declare-const " ++ argMinName ++ " Int)\n"
          ++ "(assert (<= 0 " ++ argMinName ++ "))\n"
  -- the following declrations must occur after the constraints of the sub-terms
  let d =
        "; the following block ensures that " ++ minName ++ " = " ++ pretty (BoundedMin id i j) ++ "\n"
          ++ "(assert (=> (<= " ++ i' ++ " 0) (= " ++ minName ++ " 0)))\n"
          ++ "(assert (=> (> " ++ i' ++ " 0) (= " ++ minName ++ " " ++ minimumJ ++ ")))\n"
          ++ "(assert (< " ++ argMinName ++ " " ++ i' ++ "))\n"
          ++ "(assert (forall ((_w Int)) (=> "
            ++ "(and (<= 0 _w) (< _w " ++ i' ++ "))"
            ++ "(>= " ++ anyOtherJ ++ " " ++ minimumJ ++ "))))\n"
  return (d0 ++ di ++ dj ++ d, minName)
embedIndex (BoundedSum {}) = lift Nothing
embedIndex i = undesugaredPanic "embedIndex" $ show i

-- | @smtBoundedMaxGeneric id i j embed@ turns a bounded maximum @max[id < i] j@ into a triple
-- of strings @(d, i', maxName)@ where @maxName@ is a fresh variable representing the maximum of @j@ for @id@
-- going from 0 to @i@, @i'@ is the encoding of @i@ as an SMTLIB term, and @d@ is a string containing
-- the SMTLIB declarations and constraints that ensure @maxName = max[id < i] j@.
smtBoundedMaxGeneric :: IVarId -> Index -> Index
  -> (Index -> StateT Int Maybe (String, String)) -> StateT Int Maybe (String, String, String)
smtBoundedMaxGeneric id i j embed = do
  count <- get
  put $ count + 1
  let maxName = "_max" ++ show count
  let argMaxName = "_argmax" ++ show count
  (di, i') <- embed i
  s <- get
  (dj, j') <- embed (isub (isubSingleton id (IVar argMaxName)) j)
  put s
  (_, j'') <- embed (isub (isubSingleton id (IVar "_w")) j)
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

makeQuery :: [Constraint] -> Constraint -> Maybe String
makeQuery assumptions query = do
  variableDeclarations <- concatMapM declareVariable $ toList (ifv query `Set.union` ifv assumptions)
  assumptions <- concatMapM makeAssumption assumptions
  goal <- makeGoal query
  return $ variableDeclarations ++ assumptions ++ goal ++ "(check-sat)\n"
  where
    declareVariable :: IVarId -> Maybe String
    declareVariable id = return $ "(declare-const " ++ id ++ " Int)" ++ "\n" ++ "(assert (<= 0 " ++ id ++ "))" ++ "\n"
    makeAssumption :: Constraint -> Maybe String
    makeAssumption ass = do
      (d, eass) <- evalStateT (embedConstraint ass) 0
      return $ d ++ "(assert " ++ eass ++ ")"
    makeGoal :: Constraint -> Maybe String
    makeGoal goal = do
      (d, egoal) <- evalStateT (embedConstraint goal) 0
      return $ d ++ "(assert (not " ++ egoal ++ "))"


-- | @querySMTWithContext qfh cs c@ queries the CVC5 solver to check if the constraint @c@ holds for every possible assignment of its free variables, assuming that the constraints @cs@ hold.
-- It returns @True@ if the constraint holds, @False@ otherwise or if an error occurs in the interaction with the solver.
-- The handle @qfh@ is used to communicate with the SMT solver.
querySMT :: SolverHandle -> [Constraint] -> Constraint -> IO Bool
querySMT (sin, sout) assumptions query = do
  hPutStrLn sin $ "\n; PROVE " ++ pretty query
  unless (null assumptions) $ hPutStrLn sin $ "; ASSUMING " ++ intercalate ", " (pretty <$> assumptions)
  hPutStrLn sin "(push 1)"
  case makeQuery assumptions query of
    Nothing -> 
      -- could not soundly embed the query, SMT cannot tell us anything
      return False
    Just q -> do
      hPutStr sin q
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
          error $ "CVC5 empty response while solving " ++ pretty query
        other -> do
          -- any other response is considered an error
          hPutStrLn sin $ "; got response: " ++ other
          error $ "CVC5 unknown response: " ++ other ++ " while solving " ++ pretty query
      hPutStrLn sin "(pop 1)"
      return result

-- | @SolverHandle@ is a pair of handles to communicate with the SMT solver.
-- The first handle is used to send queries to the solver, the second handle is used to read the solver's responses.
type SolverHandle = (Handle, Handle)

-- | @withSolver mdebug action@ initializes a new SMT solver process
-- and runs the action @action@ with the solver handle.
-- If @mdebug@ is @Just debugfile@, the queries are also written to the file @debugfile@ for debugging.
withSolver :: Maybe FilePath -> (SolverHandle -> IO r) -> IO r
withSolver mdebug action = do
  let teePrefix = maybe "" (\debugfile -> " tee " ++ debugfile ++ " | ") mdebug
  p@(Just sIn, Just sOut, Just sErr, _) <- createProcess $ (shell $ teePrefix ++ "cvc5 -q --incremental --interactive"){
    std_in = CreatePipe,
    std_out = CreatePipe,
    std_err = CreatePipe
  }
  --_ <- forkIO (hGetContents sErr >>= \s -> void (evaluate (length s))) -- drain stderr -- TODO figure out if needed
  hPutStrLn sIn "(set-logic HO_ALL)" -- TODO this might be made less powerful, check
  hPutStrLn sIn "(define-fun max ((x Int) (y Int)) Int (ite (< x y) y x)) ; max(x,y)" -- define the max function
  hPutStrLn sIn "(define-fun natminus ((x Int) (y Int)) Int (ite (< x y) 0 (- x y))) ; minus(x,y)" -- define the minus function
  outcome <- action (sIn, sOut)
  cleanupProcess p
  return outcome

