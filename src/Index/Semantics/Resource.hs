module Index.Semantics.Resource where
import Circuit
import Index.AST
import Control.Monad.State

data GlobalResourceFlag = Width | GateCount | QubitWidth | BitWidth | TCount
data LocalResourceFlag = Depth | TDepth

data GlobalResourceSemantics = GlobalResourceSemantics
  { -- If possible, translate resource index constructors to arithmetic constructors
    desugarIdentity :: Index,
    desugarWire :: WireType -> Index,
    desugarSequence :: Index -> Index -> Index,
    desugarParallel :: Index -> Index -> Index,
    desugarBoundedSequence :: IndexVariableId -> Index -> Index -> Index,
    desugarBoundedParallel :: IndexVariableId -> Index -> Index -> Index,
    desugarIndependentBoundedSequence :: Index -> Index -> Index,
    desugarIndependentBoundedParallel :: Index -> Index -> Index,
    -- Alternatively, interpret the resource index constructors
    interpretIdentity :: Int,
    interpretWire :: WireType -> Int,
    interpretSequence :: Int -> Int -> Int, -- also accounts for bounded sequences
    interpretParallel :: Int -> Int -> Int, -- also accounts for bounded parallels
    -- Embed resource index constructors in SMT-LIB
    smtEmbedIdentity :: String,
    smtEmbedWire :: WireType -> String,
    smtEmbedSequence :: String -> String -> String,
    smtEmbedParallel :: String -> String -> String,
    smtEmbedBoundedSequence :: IndexVariableId -> Index -> Index           -- Bounded sequence subterms
                              -> (Index -> State Int (String, String))  -- Recursive desugaring call
                              -> State Int (String, String),            -- Pair of constraints and embedded term
    smtEmbedBoundedParallel :: IndexVariableId -> Index -> Index           -- Bounded sequence subterms
                              -> (Index -> State Int (String, String))  -- Recursive desugaring call
                              -> State Int (String, String),            -- Pair of constraints and embedded term
    -- Provide ground truth for the resource consumption of basic quantum operations
    opGroundTruth :: QuantumOperation -> Int
  }

data LocalResourceSemantics = LocalResourceSemantics
  { outputInterpretation :: QuantumOperation -> Int -> [Int] -> Int,
    embedOutput :: QuantumOperation -> Int -> [String] -> String
  }