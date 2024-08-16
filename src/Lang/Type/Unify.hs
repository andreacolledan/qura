{-# LANGUAGE InstanceSigs #-}
module Lang.Type.Unify (
  TypeSubstitution(..),
  HasType(..),
  mgtu
) where

import Lang.Type.AST
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Control.Monad (zipWithM)
import Data.Foldable
import Control.Monad.Extra (when)
import Index.AST
import Index.Unify

--- TYPE UNIFICATION MODULE ---------------------------------------------------------------------------------
---
--- This module defines the unification algorithm for PQR types. Indices are ignored at this stage.
-------------------------------------------------------------------------------------------------------------

--- TYPE SUBSTITUTION ----------------------------------------------------------------------------------------

newtype TypeSubstitution = TypeSubstitution (Map.HashMap TVarId Type)

-- | @compose sub1 sub2@ composes the type substitutions @sub1@ and @sub2@
-- according to @sub2 ∘ sub1 = sub1 ∪ (sub1 sub2)@.
compose :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
compose sub1@(TypeSubstitution map1) (TypeSubstitution map2) = TypeSubstitution $ Map.union (Map.map (tsub sub1) map2) map1

-- It is useful for readability to make substitution a Monoid with identity and composition
instance Semigroup TypeSubstitution where
  (<>) :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
  (<>) = compose
instance Monoid TypeSubstitution where
  mempty = TypeSubstitution Map.empty
  mappend = (<>)

-- | Typeclass of datatypes with type variables
class HasType a where
  -- | @tfv t@ returns the set of free type variables in @t@
  tfv :: a -> Set.HashSet TVarId
  -- | @tsub sub t@ applies the type substitution @sub@ to @t@
  tsub :: TypeSubstitution -> a -> a

instance HasType Type where
  tfv TUnit = Set.empty
  tfv (TWire _ _) = Set.empty
  tfv (TTensor ts) = foldr (Set.union . tfv) Set.empty ts
  tfv (TCirc {}) = Set.empty
  tfv (TArrow t1 t2 _ _) = tfv t1 `Set.union` tfv t2
  tfv (TBang _ t) = tfv t
  tfv (TList _ _ t) = tfv t
  tfv (TVar id) = Set.singleton id
  tfv (TIForall _ t _ _) = tfv t
  tsub _ TUnit = TUnit
  tsub _ typ@(TWire _ _) = typ
  tsub sub (TTensor ts) = TTensor (map (tsub sub) ts)
  tsub _ typ@(TCirc {}) = typ
  tsub sub (TArrow typ1 typ2 i j) = TArrow (tsub sub typ1) (tsub sub typ2) i j
  tsub sub (TBang i typ) = TBang i (tsub sub typ)
  tsub sub (TList id i typ) = TList id i (tsub sub typ)
  tsub (TypeSubstitution map) typ@(TVar id) = Map.findWithDefault typ id map
  tsub sub (TIForall id typ i j) = TIForall id (tsub sub typ) i j


--- UNIFICATION ---------------------------------------------------------------------------------

-- | @assignTVar id t@ attempts to return the singleton substitution @[id ↦ t]@.
-- It returns the empty substitution if @id == t@.
-- It returns 'Nothing' if @id@ occurs in @t@.
assignTVar :: TVarId -> Type -> Maybe TypeSubstitution
assignTVar id (TVar id') | id == id' = return mempty
assignTVar id t | id `Set.member` tfv t = Nothing
assignTVar id t = return $ TypeSubstitution $ Map.singleton id t

-- | @mgtu t1 t2@ attempts to find the most general type substitution @sub@ such that @sub t1 == t2@.
-- If such a substitution does not exist, it returns 'Nothing'.
mgtu :: Type -> Type -> Maybe TypeSubstitution
mgtu (TVar id) t = assignTVar id t
mgtu t (TVar id) = assignTVar id t
mgtu TUnit TUnit = return mempty
mgtu (TWire wt1 _) (TWire wt2 _) | wt1 == wt2 = return mempty
mgtu (TTensor ts) (TTensor ts')
  | length ts == length ts' = do
    when (length ts < 2) $ error "Internal error: Tensors must have at least two elements"
    subs <- zipWithM mgtu ts ts'
    return $ fold subs
  | otherwise = Nothing
mgtu (TCirc _ t1 t2) (TCirc _ t1' t2') = do
  sub1 <- mgtu t1 t1'
  sub2 <- mgtu t2 t2'
  return $ compose sub2 sub1
mgtu (TArrow typ1 typ2 _ _) (TArrow typ1' typ2' _ _) = do
  sub1 <- mgtu typ1 typ1'
  sub2 <- mgtu (tsub sub1 typ2) (tsub sub1 typ2')
  return $ compose sub2 sub1
mgtu (TBang _ typ) (TBang _ typ') = mgtu typ typ'
mgtu (TList _ _ typ) (TList _ _ typ') = mgtu typ typ'
mgtu (TIForall _ typ _ _) (TIForall _ typ' _ _) = mgtu typ typ'
mgtu _ _ = Nothing