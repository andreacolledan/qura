{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lang.Type.AST
  ( Type (..),
    TVarId,
    isLinear,
    isBundleType,
    WireType(..),
    Wide(..)
  )
where

import qualified Data.HashSet as Set
import Index.AST
import PrettyPrinter
import Data.List (intercalate)
import Circuit

--- TYPE SYNTAX MODULE ---------------------------------------------------------------------------------------
---
--- This module defines the abstract syntax of PQR types.
--- The syntax is almost identical to the one in the paper, with the exception of the introduction of
--- type variables (used internally) and dependent function types (which internalize the index
--- meta-theoretical results in the paper).
-------------------------------------------------------------------------------------------------------------

type TVarId = String

-- Fig. 8
-- | The datatype of PQR types
data Type
  = TUnit                                       -- Unit type        : ()
  | TWire WireType                              -- Wire type        : Bit | Qubit
  | TTensor [Type]                              -- Tensor type      : (t1, t2, ...)
  | TCirc (Maybe Index) Type Type               -- Circuit type     : Circ[i](t1, t2)
  | TArrow Type Type (Maybe Index) (Maybe Index)-- Function type    : t1 -o[i,j] t2
  | TBang (Maybe Index) Type                    -- Bang type        : ![i]t
  | TList IndexVariableId Index Type            -- List type        : TList[id < i] t
  | TVar TVarId                                 -- Type variable    : 
  | TIForall IndexVariableId Type (Maybe Index) (Maybe Index) -- Dep. fun. type   : i ->[i,j] t
  deriving (Show, Eq)

prettyAnno :: Maybe Index -> String
prettyAnno Nothing = ""
prettyAnno (Just i) = "[" ++ pretty i ++ "]"

prettyAnnos :: Maybe Index -> Maybe Index -> String
prettyAnnos Nothing Nothing = ""
prettyAnnos (Just i) (Just j) = "[" ++ pretty i ++ ", " ++ pretty j ++ "]"
prettyAnnos _ _ = error "Internal: inconsistent annotations (prettyAnnos)"

instance Pretty Type where
  pretty TUnit = "()"
  pretty (TWire wt) = pretty wt
  pretty (TTensor ts) = "(" ++ intercalate ", " (map pretty ts) ++ ")"
  pretty (TCirc i inBtype outBtype) = "Circ" ++ prettyAnno i ++ "(" ++ pretty inBtype ++ ", " ++ pretty outBtype ++ ")"
  pretty (TArrow typ1 typ2 i j) = "(" ++ pretty typ1 ++ " -o" ++ prettyAnnos i j ++ " " ++ pretty typ2 ++ ")"
  pretty (TBang i typ) = "!" ++ prettyAnno i  ++ pretty typ
  pretty (TList id i typ) = "List[" ++ id ++ "<" ++ pretty i ++ "] " ++ pretty typ
  pretty (TVar id) = id
  pretty (TIForall id typ i j) = "(" ++ id ++ " ->" ++ prettyAnnos i j ++ " " ++ pretty typ ++ ")"

-- Def. 2 (Wire Count)
-- PQR types are amenable to wire counting
instance Wide Type where
  wireCount TUnit = return Identity
  wireCount (TWire wt) = return $ Wire wt
  wireCount (TTensor ts) = do
    wirecounts <- mapM wireCount ts
    return $ foldl Parallel Identity wirecounts
  wireCount (TCirc {}) = return Identity
  wireCount (TArrow _ _ _ i) = i
  wireCount (TBang _ _) = return Identity
  wireCount (TList id i t) = do
    wirecount <- wireCount t
    return $ BoundedParallel id i wirecount
  wireCount (TIForall _ _ _ i) = i
  wireCount (TVar _) = error "Cannot count wires of a type variable"

-- PQR types are amenable to the notion of well-formedness with respect to an index context
instance HasIndex Type where
  iv :: Type -> Set.HashSet IndexVariableId
  iv TUnit = Set.empty
  iv (TWire _) = Set.empty
  iv (TTensor ts) = foldr (Set.union . iv) Set.empty ts
  iv (TCirc i _ _) = iv i
  iv (TArrow typ1 typ2 i j) = iv typ1 `Set.union` iv typ2 `Set.union` iv i `Set.union` iv j
  iv (TBang i typ) = iv i `Set.union` iv typ
  iv (TList id i typ) = Set.insert id $ iv i `Set.union` iv typ
  iv (TVar _) = Set.empty
  iv (TIForall id typ i j) = Set.insert id (iv typ `Set.union` iv i `Set.union` iv j)
  ifv :: Type -> Set.HashSet IndexVariableId
  ifv TUnit = Set.empty
  ifv (TWire _) = Set.empty
  ifv (TTensor ts) = foldr (Set.union . ifv) Set.empty ts
  ifv (TCirc i _ _) = ifv i
  ifv (TArrow typ1 typ2 i j) = ifv typ1 `Set.union` ifv typ2 `Set.union` ifv i `Set.union` ifv j
  ifv (TBang i typ) = ifv i `Set.union` ifv typ
  ifv (TList id i typ) = Set.delete id $ ifv i `Set.union` ifv typ
  ifv (TVar _) = Set.empty
  ifv (TIForall id typ i j) = Set.delete id (ifv typ `Set.union` ifv i `Set.union` ifv j)
  isub :: Index -> IndexVariableId -> Type -> Type
  isub _ _ TUnit = TUnit
  isub _ _ (TWire wtype) = TWire wtype
  isub i id (TTensor ts) = TTensor (map (isub i id) ts)
  isub i id (TCirc j inBtype outBtype) = TCirc (isub i id j) (isub i id inBtype) (isub i id outBtype) -- Bundle types have no free variables
  isub i id (TArrow typ1 typ2 j k) = TArrow (isub i id typ1) (isub i id typ2) (isub i id j) (isub i id k)
  isub i id (TBang j typ) = TBang (isub i id j) (isub i id typ)
  isub i id (TList id' j typ) = --TODO check
    let id'' = fresh (fresh id' [IndexVariable id, i, j]) [typ]
        in TList id'' (isub i id . isub (IndexVariable id'') id' $ j) (isub i id . isub (IndexVariable id'') id' $ typ)
  isub _ _ (TVar a) = TVar a
  isub i id (TIForall id' typ j k) =
    let id'' = fresh (fresh (fresh id' [IndexVariable id, i]) [j, k]) [typ]
     in TIForall id'' (isub i id . isub (IndexVariable id'') id' $ typ) (isub i id . isub (IndexVariable id'') id' $ j) (isub i id . isub (IndexVariable id'') id' $ k)

-- | @isLinear t@ returns 'True' @t@ is a linear type, and 'False' otherwise
isLinear :: Type -> Bool
isLinear TUnit = False
isLinear (TWire _) = True
isLinear (TTensor ts) = any isLinear ts
isLinear (TCirc {}) = False
isLinear (TArrow {}) = True
isLinear (TBang _ _) = False
isLinear (TList _ (Number 0) _) = False  -- Empty lists can be discarded
isLinear (TList _ _ typ) = isLinear typ
isLinear (TVar _) = False -- Variables are only used in the pre-processing stage, so we are permissive here
isLinear (TIForall _ typ _ _) = isLinear typ

isBundleType :: Type -> Bool
isBundleType TUnit = True
isBundleType (TWire _) = True
isBundleType (TTensor ts) = all isBundleType ts
isBundleType (TList _ _ typ) = isBundleType typ
-- isBundleType (TVar _) = True -- TODO check
isBundleType _ = False

--- WIRE COUNTING ---------------------------------------------------------------------------------

-- The class of datatypes that can contain wires and are thus amenable to wire counting
-- Def. 2 (Wire Count)
class Wide a where
  wireCount :: a -> Maybe Index -- #(•) in the paper

instance Wide WireType where
  wireCount wt = Just (Wire wt)

-- Any traversable structure of elements with wire counts can be wire counted
-- Its wire count is the sum of the wire counts of its elements
instance (Traversable t, Wide a) => Wide (t a) where
  wireCount x = do
    wirecounts <- mapM wireCount x
    return $ foldr Parallel Identity wirecounts

instance Wide a => Wide (Maybe a) where
  wireCount Nothing = Nothing
  wireCount (Just x) = wireCount x