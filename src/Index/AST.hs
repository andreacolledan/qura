{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Index.AST
  ( Index (..),
    IVarId,
    IndexContext,
    Constraint (..),
    emptyictx,
  )
where

import qualified Data.HashSet as Set
import PrettyPrinter
import Circuit
import Data.List (intercalate)

type IVarId = String

-- | The datatype of index expressions
data Index
  = IVar IVarId                           -- Index variable         : i, j, k,...
  | Number Int                            -- Natural number         : 0,1,2,...
  | Plus Index Index                      -- Sum of indices         : i + j
  | Max Index Index                       -- Max of indices         : max(i, j)
  | Mult Index Index                      -- Product of indices     : i * j
  | Minus Index Index                     -- Natural subtraction    : i - j
  | BoundedMax IVarId Index Index         -- Bounded maximum        : max[id < i] j
  | BoundedMin IVarId Index Index         -- Bounded minimum        : min[id < i] j
  | BoundedSum IVarId Index Index         -- Bounded sum            : sum[id < i] j
  -- Abstract resource operators 
  | Output QuantumOperation Int [Index]   -- Local resource annotation of op output       : output[g,n](i1,...,in)
  | Operation QuantumOperation            -- Global resource consumption of an operation  : operation[g]
  | Identity                              -- No global resource consumption               : identity
  | Wire WireType                         -- Global resource consumption of a wire        : wire[w]
  | Sequence Index Index                  -- Composition in sequence of global resources  : i >> j
  | Parallel Index Index                  -- Composition in parallel of global resources  : i || j
  | BoundedSequence IVarId Index Index    -- Bounded composition in sequence              : seq[id < i] j
  | BoundedParallel IVarId Index Index    -- Bounded composition in parallel              : par[id < i] j
  deriving (Show, Eq)

instance Pretty Index where
  prettyPrec _ (IVar id) = id
  prettyPrec _ (Number n) = show n
  prettyPrec prec (Plus i j) = withinPar (prec > 6) $ prettyPrec 6 i ++ " + " ++ prettyPrec 6 j
  prettyPrec _ (Max i j) = "max(" ++ pretty i ++ ", " ++ pretty j ++ ")"
  prettyPrec prec (Mult i j) = withinPar (prec > 7) $ prettyPrec 7 i ++ " * " ++ prettyPrec 7 j
  prettyPrec prec (Minus i j) = withinPar (prec > 5) $ prettyPrec 5 i ++ " - " ++ prettyPrec 6 j
  prettyPrec _ (BoundedMax id i j) = "max[" ++ id ++ " < " ++ pretty i ++ "]" ++ "(" ++ pretty j ++ ")"
  prettyPrec _ (BoundedMin id i j) = "min[" ++ id ++ " < " ++ pretty i ++ "]" ++ "(" ++ pretty j ++ ")"
  prettyPrec _ (BoundedSum id i j) = "sum[" ++ id ++ " < " ++ pretty i ++ "]" ++ "(" ++ pretty j ++ ")"
  prettyPrec _ (Output op n is) = "output[" ++ show op ++ "," ++ show n ++ "](" ++ intercalate ", " (pretty <$> is) ++ ")"
  prettyPrec _ Identity = "identity"
  prettyPrec _ (Wire wt) = "wire[" ++ show wt ++ "]"
  prettyPrec _ (Operation op) = "operation[" ++ show op ++ "]"
  prettyPrec prec (Sequence i j) = withinPar (prec > 3) $ prettyPrec 5 i ++ " >> " ++ prettyPrec 3 j
  prettyPrec prec (Parallel i j) = withinPar (prec > 4) $ prettyPrec 5 i ++ " || " ++ prettyPrec 4 j
  prettyPrec _ (BoundedSequence id i j) = "seq[" ++ id ++ " < " ++ pretty i ++ "]" ++ "(" ++ pretty j ++ ")"
  prettyPrec _ (BoundedParallel id i j) = "par[" ++ id ++ " < " ++ pretty i ++ "]" ++ "(" ++ pretty j ++ ")"

-- | The datatype of index contexts
type IndexContext = Set.HashSet IVarId

-- | The empty index context
emptyictx :: IndexContext
emptyictx = Set.empty

-- | The datatype of index constraints
data Constraint = Eq Index Index | Leq Index Index
  deriving (Show, Eq)

instance Pretty Constraint where
  pretty (Eq i j) = pretty i ++ " = " ++ " " ++ pretty j
  pretty (Leq i j) = pretty i ++ " <= " ++ pretty j


