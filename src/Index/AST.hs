{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Index.AST
  ( Index (..),
    IndexVariableId,
    IndexContext,
    Constraint (..),
    emptyictx,
  )
where

import qualified Data.HashSet as Set
import PrettyPrinter
import Circuit
import Data.List (intercalate)

type IndexVariableId = String

-- (fig. 8)
-- | The datatype of index expressions
data Index
  = IndexVariable IndexVariableId         -- Index variable         : i, j, k,...
  | Number Int                            -- Natural number         : 0,1,2,...
  | Plus Index Index                      -- Sum of indices         : i + j
  | Max Index Index                       -- Max of indices         : max(i, j)
  | Mult Index Index                      -- Product of indices     : i * j
  | Minus Index Index                     -- Natural subtraction    : i - j
  | BoundedMax IndexVariableId Index Index-- Bounded maximum        :max[id < i] j
  | BoundedSum IndexVariableId Index Index-- Bounded sum            :sum[id < i] j
  -- Resource operations 
  | Output QuantumOperation Int [Index]         -- Local resource annotation of op output       -- Output[g,n](i1,...,in)
  | Operation QuantumOperation                  -- Global resource consumption of an operation  -- Op[g]
  | Identity                                    -- No global resource consumption               -- None
  | Wire WireType                               -- Global resource consumption of a wire        -- Wire[w]
  | Sequence Index Index                        -- Composition in sequence of global resources  -- i >> j
  | Parallel Index Index                        -- Composition in parallel of global resources  -- i || j
  | BoundedSequence IndexVariableId Index Index -- Bounded composition in sequence              -- >>[id < i] j
  | BoundedParallel IndexVariableId Index Index -- Bounded composition in parallel              -- ||[id < i] j
  deriving (Show, Eq)

instance Pretty Index where
  pretty (IndexVariable id) = id
  pretty (Number n) = show n
  pretty (Plus i j) = "(" ++ pretty i ++ " + " ++ pretty j ++ ")"
  pretty (Max i j) = "max(" ++ pretty i ++ ", " ++ pretty j ++ ")"
  pretty (Mult i j) = "(" ++ pretty i ++ " * " ++ pretty j ++ ")"
  pretty (Minus i j) = "(" ++ pretty i ++ " - " ++ pretty j ++ ")"
  pretty (BoundedMax id i j) = "max[" ++ id ++ " < " ++ pretty i ++ "] " ++ pretty j
  pretty (BoundedSum id i j) = "sum[" ++ id ++ " < " ++ pretty i ++ "] " ++ pretty j
  pretty (Output op n is) = "Output[" ++ show op ++ "," ++ show n ++ "](" ++ intercalate ", " (pretty <$> is) ++ ")"
  pretty Identity = "Identity"
  pretty (Wire wt) = "Wire[" ++ show wt ++ "]"
  pretty (Operation op) = "Operation[" ++ show op ++ "]"
  pretty (Sequence i j) = "(" ++ pretty i ++ " >> " ++ pretty j ++ ")"
  pretty (Parallel i j) = "(" ++ pretty i ++ " || " ++ pretty j ++ ")"
  pretty (BoundedSequence id i j) = "S[" ++ id ++ " < " ++ pretty i ++ "] " ++ pretty j
  pretty (BoundedParallel id i j) = "P[" ++ id ++ " < " ++ pretty i ++ "] " ++ pretty j

-- Corresponds to Θ in the paper
type IndexContext = Set.HashSet IndexVariableId

-- | The empty index context
emptyictx :: IndexContext
emptyictx = Set.empty

-- | The datatype of index constraints
data Constraint = Eq Index Index | Leq Index Index
  deriving (Show, Eq)

instance Pretty Constraint where
  pretty (Eq i j) = pretty i ++ " = " ++ " " ++ pretty j
  pretty (Leq i j) = pretty i ++ " <= " ++ pretty j


