{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Index.Unify (
  IndexSubstitution,
  isubDomain,
  isubCodomain,
  isubSingleton,
  HasIndex(..),
  fresh,
  mgiu
) where

import Index.AST
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import Control.Monad
import Data.Foldable

--- INDEX SUBSTITUTION ---

newtype IndexSubstitution = IndexSubstitution (Map.HashMap IVarId Index)

isubDomain :: IndexSubstitution -> [IVarId]
isubDomain (IndexSubstitution map) = Map.keys map

isubCodomain :: IndexSubstitution -> [Index]
isubCodomain (IndexSubstitution map) = Map.elems map

isubSingleton :: IVarId -> Index -> IndexSubstitution
isubSingleton id i = IndexSubstitution $ Map.singleton id i

-- | @compose sub1 sub2@ composes the type substitutions @sub1@ and @sub2@
-- according to @sub2 ∘ sub1 = sub1 ∪ (sub1 sub2)@.
compose :: IndexSubstitution -> IndexSubstitution -> IndexSubstitution
compose sub1@(IndexSubstitution map1) (IndexSubstitution map2) = IndexSubstitution $ Map.union (Map.map (isub sub1) map2) map1

-- It is useful for readability to make substitution a Monoid with identity and composition
instance Semigroup IndexSubstitution where
  (<>) :: IndexSubstitution -> IndexSubstitution -> IndexSubstitution
  (<>) = compose
instance Monoid IndexSubstitution where
  mempty = IndexSubstitution Map.empty
  mappend = (<>)

-- | The class of types that contain index variables
class HasIndex a where
  -- | @iv x@ returns the set of index variables (bound or free) that occur in @x@
  iv :: a -> Set.HashSet IVarId
  -- | @ifv x@ returns the set of free index variables that occur in @x@
  ifv :: a -> Set.HashSet IVarId
  -- | @isub sub x@ substitutes the index variable @id@ by the index @i@ in @x@
  isub :: IndexSubstitution -> a -> a

instance HasIndex Index where
  iv :: Index -> Set.HashSet IVarId
  iv (IVar id) = Set.singleton id
  iv (Number _) = Set.empty
  iv (Plus i j) = iv i `Set.union` iv j
  iv (Max i j) = iv i `Set.union` iv j
  iv (Mult i j) = iv i `Set.union` iv j
  iv (Minus i j) = iv i `Set.union` iv j
  iv (BoundedMax id i j) = Set.insert id (iv i `Set.union` iv j)
  iv (BoundedSum id i j) = Set.insert id (iv i `Set.union` iv j)
  iv (Output _ _ is) = Set.unions $ iv <$> is
  iv Identity = Set.empty
  iv (Wire _) = Set.empty
  iv (Operation _) = Set.empty
  iv (Sequence i j) = iv i `Set.union` iv j
  iv (Parallel i j) = iv i `Set.union` iv j
  iv (BoundedSequence id i j) = Set.insert id (iv i `Set.union` iv j)
  iv (BoundedParallel id i j) = Set.insert id (iv i `Set.union` iv j)
  ifv :: Index -> Set.HashSet IVarId
  ifv (IVar id) = Set.singleton id
  ifv (Number _) = Set.empty
  ifv (Plus i j) = ifv i `Set.union` ifv j
  ifv (Max i j) = ifv i `Set.union` ifv j
  ifv (Mult i j) = ifv i `Set.union` ifv j
  ifv (Minus i j) = ifv i `Set.union` ifv j
  ifv (BoundedMax id i j) = Set.delete id (ifv i `Set.union` ifv j)
  ifv (BoundedSum id i j) = Set.delete id (ifv i `Set.union` ifv j)
  ifv (Output _ _ is) = Set.unions $ ifv <$> is
  ifv Identity = Set.empty
  ifv (Wire _) = Set.empty
  ifv (Operation _) = Set.empty
  ifv (Sequence i j) = ifv i `Set.union` ifv j
  ifv (Parallel i j) = ifv i `Set.union` ifv j
  ifv (BoundedSequence id i j) = Set.delete id (ifv i `Set.union` ifv j)
  ifv (BoundedParallel id i j) = Set.delete id (ifv i `Set.union` ifv j)
  isub :: IndexSubstitution -> Index -> Index
  isub _ (Number n) = Number n
  isub (IndexSubstitution map) j@(IVar id) = Map.findWithDefault j id map
  isub sub (Plus j k) = Plus (isub sub j) (isub sub k)
  isub sub (Max j k) = Max (isub sub j) (isub sub k)
  isub sub (Mult j k) = Mult (isub sub j) (isub sub k)
  isub sub (Minus j k) = Minus (isub sub j) (isub sub k)
  isub sub (BoundedMax id j k) =
    let id' = fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub ++ [k])
        renaming = isubSingleton id (IVar id')
     in BoundedMax id' (isub sub j) (isub sub . isub renaming $ k)
  isub sub (BoundedSum id j k) =
    let id' = fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub ++ [k])
        renaming = isubSingleton id (IVar id')
     in BoundedSum id' (isub sub j) (isub sub . isub renaming $ k)
  isub sub (Output op n is) = Output op n (isub sub <$> is)
  isub _ Identity = Identity
  isub _ (Wire wt) = Wire wt
  isub _ (Operation op) = Operation op
  isub sub (Sequence j k) = Sequence (isub sub j) (isub sub k)
  isub sub (Parallel j k) = Parallel (isub sub j) (isub sub k)
  isub sub (BoundedSequence id j k) =
    let id' = fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub ++ [k])
        renaming = isubSingleton id (IVar id')
     in BoundedSequence id' (isub sub j) (isub sub . isub renaming $ k)
  isub sub (BoundedParallel id j k) =
    let id' = fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub ++ [k])
        renaming = isubSingleton id (IVar id')
     in BoundedParallel id' (isub sub j) (isub sub . isub renaming $ k)

instance HasIndex Constraint where
  iv :: Constraint -> Set.HashSet IVarId
  iv (Eq i j) = iv i `Set.union` iv j
  iv (Leq i j) = iv i `Set.union` iv j
  ifv :: Constraint -> Set.HashSet IVarId
  ifv (Eq i j) = ifv i `Set.union` ifv j
  ifv (Leq i j) = ifv i `Set.union` ifv j
  isub :: IndexSubstitution -> Constraint -> Constraint
  isub sub (Eq i j) = Eq (isub sub i) (isub sub j)
  isub sub (Leq i j) = Leq (isub sub i) (isub sub j)


-- | @fresh id xs@ returns a fresh index variable name that does not occur in @xs@, @id@ if possible.
fresh :: (HasIndex a) => IVarId -> [a] -> IVarId
fresh id xs =
  let toavoid = Set.unions $ iv <$> xs
   in head $ filter (not . (`Set.member` toavoid)) $ id : [id ++ show n | n <- [0 ..]]

-- Natural lifting of well-formedness to traversable data structures
instance (Traversable t, HasIndex a) => HasIndex (t a) where
  iv :: t a -> Set.HashSet IVarId
  iv x = let ivets = iv <$> x in foldr Set.union Set.empty ivets
  ifv :: t a -> Set.HashSet IVarId
  ifv x = let ifvets = ifv <$> x in foldr Set.union Set.empty ifvets
  isub :: IndexSubstitution -> t a -> t a
  isub sub x = isub sub <$> x

--- INDEX UNIFICATION ---

-- | @assignIndexVariable id i@ attempts to return the singleton substitution @[id ↦ i]@.
-- It returns the empty substitution if @id == i@.
-- It returns 'Nothing' if @id@ occurs in @i@.
assignIndexVariable :: IVarId -> Index -> Maybe IndexSubstitution
assignIndexVariable id (IVar id') | id == id' = return mempty
assignIndexVariable id i | id `Set.member` ifv i = Nothing
assignIndexVariable id i = return $ IndexSubstitution $ Map.singleton id i

-- | @mgiu i1 i2@ attempts to return the most general index substitution sub such that @sub i1 == i2@.
-- If such a substitution does not exist, it returns 'Nothing'
mgiu :: Index -> Index -> Maybe IndexSubstitution
mgiu (IVar id) i = assignIndexVariable id i
mgiu i (IVar id) = assignIndexVariable id i --TODO check if correct
mgiu (Number n) (Number m) | n == m = return mempty
mgiu (Plus i j) (Plus i' j') = do
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub sub1 j) (isub sub1 j')
  return $ compose sub2 sub1
mgiu (Max i j) (Max i' j') = do
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub sub1 j) (isub sub1 j')
  return $ compose sub2 sub1
mgiu (Mult i j) (Mult i' j') = do
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub sub1 j) (isub sub1 j')
  return $ compose sub2 sub1
mgiu (Minus i j) (Minus i' j') = do
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub sub1 j) (isub sub1 j')
  return $ compose sub2 sub1
mgiu (BoundedMax id i j) (BoundedMax id' i' j') = do
  let fid = fresh (fresh id [i, i']) [j, j']
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub (isubSingleton id (IVar fid)) j) (isub (isubSingleton id' (IVar fid)) j')
  return $ compose sub2 sub1
mgiu (BoundedSum id i j) (BoundedSum id' i' j') = do
  let fid = fresh (fresh id [i, i']) [j, j']
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub (isubSingleton id (IVar fid)) j) (isub (isubSingleton id' (IVar fid)) j')
  return $ compose sub2 sub1
mgiu (Output op n is) (Output op' n' is') | op == op' && n == n' = do
  subs <- zipWithM mgiu is is'
  return $ fold subs
mgiu Identity Identity = return mempty
mgiu (Wire wt) (Wire wt') | wt == wt' = return mempty
mgiu (Operation op) (Operation op') | op == op' = return mempty
mgiu (Sequence i j) (Sequence i' j') = do
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub sub1 j) (isub sub1 j')
  return $ compose sub2 sub1
mgiu (Parallel i j) (Parallel i' j') = do
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub sub1 j) (isub sub1 j')
  return $ compose sub2 sub1
mgiu (BoundedSequence id i j) (BoundedSequence id' i' j') = do
  let fid = fresh (fresh id [i, i']) [j, j']
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub (isubSingleton id (IVar fid)) j) (isub (isubSingleton id' (IVar fid)) j')
  return $ compose sub2 sub1
mgiu (BoundedParallel id i j) (BoundedParallel id' i' j') = do
  let fid = fresh (fresh id [i, i']) [j, j']
  sub1 <- mgiu i i'
  sub2 <- mgiu (isub (isubSingleton id (IVar fid)) j) (isub (isubSingleton id' (IVar fid)) j')
  return $ compose sub2 sub1
mgiu _ _ = Nothing
