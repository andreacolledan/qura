{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Index.AST
  ( Index (..),
    IndexVariableId,
    IndexContext,
    HasIndex (..),
    Constraint (..),
    IRel (..),
    emptyictx,
    fresh,
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
  | OpOutput QuantumOperation Int [Index]       -- Local resource annotation of op output       -- Output[g,n](i1,...,in)
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
  pretty (OpOutput op n is) = "Output[" ++ show op ++ "," ++ show n ++ "](" ++ intercalate ", " (pretty <$> is) ++ ")"
  pretty Identity = "Identity"
  pretty (Wire wt) = "Wire[" ++ show wt ++ "]"
  pretty (Sequence i j) = "(" ++ pretty i ++ " >> " ++ pretty j ++ ")"
  pretty (Parallel i j) = "(" ++ pretty i ++ " || " ++ pretty j ++ ")"
  pretty (BoundedSequence id i j) = "S[" ++ id ++ " < " ++ pretty i ++ "] " ++ pretty j
  pretty (BoundedParallel id i j) = "P[" ++ id ++ " < " ++ pretty i ++ "] " ++ pretty j

-- Corresponds to Θ in the paper
type IndexContext = Set.HashSet IndexVariableId

-- | The empty index context
emptyictx :: IndexContext
emptyictx = Set.empty

-- | The class of types that contain index variables
class HasIndex a where
  -- | @iv x@ returns the set of index variables (bound or free) that occur in @x@
  iv :: a -> Set.HashSet IndexVariableId
  -- | @ifv x@ returns the set of free index variables that occur in @x@
  ifv :: a -> Set.HashSet IndexVariableId
  -- | @isub i id x@ substitutes the index variable @id@ by the index @i@ in @x@
  isub :: Index -> IndexVariableId -> a -> a

instance HasIndex Index where
  iv :: Index -> Set.HashSet IndexVariableId
  iv (IndexVariable id) = Set.singleton id
  iv (Number _) = Set.empty
  iv (Plus i j) = iv i `Set.union` iv j
  iv (Max i j) = iv i `Set.union` iv j
  iv (Mult i j) = iv i `Set.union` iv j
  iv (Minus i j) = iv i `Set.union` iv j
  iv (BoundedMax id i j) = Set.insert id (iv i `Set.union` iv j)
  iv (BoundedSum id i j) = Set.insert id (iv i `Set.union` iv j)
  iv (OpOutput _ _ is) = Set.unions $ iv <$> is
  iv Identity = Set.empty
  iv (Wire _) = Set.empty
  iv (Sequence i j) = iv i `Set.union` iv j
  iv (Parallel i j) = iv i `Set.union` iv j
  iv (BoundedSequence id i j) = Set.insert id (iv i `Set.union` iv j)
  iv (BoundedParallel id i j) = Set.insert id (iv i `Set.union` iv j)
  ifv :: Index -> Set.HashSet IndexVariableId
  ifv (IndexVariable id) = Set.singleton id
  ifv (Number _) = Set.empty
  ifv (Plus i j) = ifv i `Set.union` ifv j
  ifv (Max i j) = ifv i `Set.union` ifv j
  ifv (Mult i j) = ifv i `Set.union` ifv j
  ifv (Minus i j) = ifv i `Set.union` ifv j
  ifv (BoundedMax id i j) = Set.delete id (ifv i `Set.union` ifv j)
  ifv (BoundedSum id i j) = Set.delete id (ifv i `Set.union` ifv j)
  ifv (OpOutput _ _ is) = Set.unions $ ifv <$> is
  ifv Identity = Set.empty
  ifv (Wire _) = Set.empty
  ifv (Sequence i j) = ifv i `Set.union` ifv j
  ifv (Parallel i j) = ifv i `Set.union` ifv j
  ifv (BoundedSequence id i j) = Set.delete id (ifv i `Set.union` ifv j)
  ifv (BoundedParallel id i j) = Set.delete id (ifv i `Set.union` ifv j)
  isub :: Index -> IndexVariableId -> Index -> Index
  isub _ _ (Number n) = Number n
  isub i id j@(IndexVariable id') = if id == id' then i else j
  isub i id (Plus j k) = Plus (isub i id j) (isub i id k)
  isub i id (Max j k) = Max (isub i id j) (isub i id k)
  isub i id (Mult j k) = Mult (isub i id j) (isub i id k)
  isub i id (Minus j k) = Minus (isub i id j) (isub i id k)
  isub i id (BoundedMax id' j k) =
    let id'' = fresh id' [IndexVariable id, i, k] -- find an id'', preferably id', that is not id and does not capture anything in i or k
     in BoundedMax id'' (isub i id j) (isub i id . isub (IndexVariable id'') id' $ k)
  isub i id (BoundedSum id' j k) =
    let id'' = fresh id' [IndexVariable id, i, k] -- find an id'', preferably id', that is not id and does not capture anything in i or k
     in BoundedSum id'' (isub i id j) (isub i id . isub (IndexVariable id'') id' $ k)
  isub i id (OpOutput op n is) = OpOutput op n (isub i id <$> is)
  isub _ _ Identity = Identity
  isub _ _ (Wire wt) = Wire wt
  isub i id (Sequence j k) = Sequence (isub i id j) (isub i id k)
  isub i id (Parallel j k) = Parallel (isub i id j) (isub i id k)
  isub i id (BoundedSequence id' j k) =
    let id'' = fresh id' [IndexVariable id, i, k] -- find an id'', preferably id', that is not id and does not capture anything in i or k
     in BoundedSequence id'' (isub i id j) (isub i id . isub (IndexVariable id'') id' $ k)
  isub i id (BoundedParallel id' j k) =
    let id'' = fresh id' [IndexVariable id, i, k] -- find an id'', preferably id', that is not id and does not capture anything in i or k
     in BoundedParallel id'' (isub i id j) (isub i id . isub (IndexVariable id'') id' $ k)


-- | @fresh id xs@ returns a fresh index variable name that does not occur in @xs@, @id@ if possible.
fresh :: (HasIndex a) => IndexVariableId -> [a] -> IndexVariableId
fresh id xs =
  let toavoid = Set.unions $ iv <$> xs
   in head $ filter (not . (`Set.member` toavoid)) $ id : [id ++ show n | n <- [0 ..]]

-- Natural lifting of well-formedness to traversable data structures
instance (Traversable t, HasIndex a) => HasIndex (t a) where
  iv :: t a -> Set.HashSet IndexVariableId
  iv x = let ivets = iv <$> x in foldr Set.union Set.empty ivets
  ifv :: t a -> Set.HashSet IndexVariableId
  ifv x = let ifvets = ifv <$> x in foldr Set.union Set.empty ifvets
  isub :: Index -> IndexVariableId -> t a -> t a
  isub i id x = isub i id <$> x

-- | The datatype of index relations
data IRel = Eq | Leq
  deriving (Show, Eq)

instance Pretty IRel where
  pretty Eq = "="
  pretty Leq = "≤"

-- | The datatype of index constraints
data Constraint = Constraint IRel Index Index
  deriving (Show, Eq)

instance Pretty Constraint where
  pretty (Constraint rel i j) = pretty i ++ " " ++ pretty rel ++ " " ++ pretty j
