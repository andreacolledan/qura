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

-- MAYBE inside the circuit there are the labels that are also the pvars 
-- appearing in the term and that can be found in the args of the tldef...
-- or maybe it is not important and we can simply use the PVar found during evaluatino idk
-- tbf idk if the circuit need to already account for all the possible qubits used,
-- Or i could maybe use the result of the analysis.
-- Or i can simply add new wires when encountering the operations.
-- For now I left untouched the circuit and only eval the Expr. :)
evalConfiguration :: Configuration -> Either RuntimeError Configuration
evalConfiguration (Config circ expr) = 
  trace (""
      -- ++ "-- Circuit:\n"++ show circ
      ++ "\n-- Full Expr:\n"++pretty expr
      ++ "\n-- Full Expr:\n"++show expr
    ) $ 
    do
      reduced <- eval expr
      Right $ Config circ reduced