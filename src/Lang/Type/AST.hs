{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lang.Type.AST
  ( Type (..),
    TVarId,
    isLinear,
    isBundleType,
    WireType(..),
    HasSize(..),
    stripGlobalAnnotations,
    stripLocalAnnotations
  )
where

import qualified Data.HashSet as Set
import Index.AST
import PrettyPrinter
import Data.List (intercalate)
import Circuit
import Index.Unify

--- TYPE SYNTAX MODULE ---------------------------------------------------------------------------------------
---
--- This module defines the abstract syntax of PQR types.
--- The syntax is almost identical to the one in the paper, with the exception of the introduction of
--- type variables (used internally) and dependent function types (which internalize the index
--- meta-theoretical results in the paper).
-------------------------------------------------------------------------------------------------------------

type TVarId = String

-- | The datatype of PQR types
data Type
  = TUnit                                             -- Unit type        : ()
  | TWire WireType (Maybe Index)                      -- Wire type        : Bit{i} | Qubit{i}
  | TTensor [Type]                                    -- Tensor type      : (t1, t2, ...)
  | TCirc (Maybe Index) Type Type                     -- Circuit type     : Circ[i](t1, t2)
  | TArrow Type Type (Maybe Index) (Maybe Index)      -- Function type    : t1 -o[i, j] t2
  | TBang (Maybe Index) Type                          -- Bang type        : ![i]t
  | TList IVarId Index Type                           -- List type        : TList[id < i] t
  | TVar TVarId                                       -- Type variable    : 
  | TIForall IVarId Type (Maybe Index) (Maybe Index)  -- Dep. fun. type   : forall[i, j] id. t
  deriving (Show, Eq)

-- | @prettyGlobalAnnotation i@ returns a pretty-printed version of annotation @i@
-- within square brackets, or the empty string if @i@ is 'Nothing'
prettyGlobalAnnotation :: Maybe Index -> String
prettyGlobalAnnotation Nothing = ""
prettyGlobalAnnotation (Just i) = "[" ++ pretty i ++ "]"

-- | @prettyGlobalAnnotations i j@ returns a pretty-printed version of annotations @i@ and @j@
-- within square brackets, or the empty string if both are 'Nothing'
prettyGlobalAnnotations :: Maybe Index -> Maybe Index -> String
prettyGlobalAnnotations Nothing Nothing = ""
prettyGlobalAnnotations (Just i) (Just j) = "[" ++ pretty i ++ ", " ++ pretty j ++ "]"
prettyGlobalAnnotations _ _ = error "Internal: inconsistent annotations (prettyAnnos)"

-- | @prettyLocalAnnotation i@ returns a pretty-printed version of annotation @i@
-- within curly brackets, or the empty string if @i@ is 'Nothing'
prettyLocalAnnotation :: Maybe Index -> String
prettyLocalAnnotation Nothing = ""
prettyLocalAnnotation (Just i) = "{" ++ pretty i ++ "}"

instance Pretty Type where
  prettyPrec _ TUnit = "()"
  prettyPrec _ (TWire wt i) = pretty wt ++ prettyLocalAnnotation i
  prettyPrec _ (TTensor ts) = "(" ++ intercalate ", " (map pretty ts) ++ ")"
  prettyPrec _ (TCirc i inBtype outBtype) = "Circ" ++ prettyGlobalAnnotation i ++ "(" ++ pretty inBtype ++ ", " ++ pretty outBtype ++ ")"
  prettyPrec prec (TArrow typ1 typ2 i j) = withinPar (prec > 5) $ prettyPrec 5 typ1 ++ " -o" ++ prettyGlobalAnnotations i j ++ " " ++ prettyPrec 5 typ2
  prettyPrec prec (TBang i typ) = withinPar (prec > 6) $ "!" ++ prettyGlobalAnnotation i  ++ prettyPrec 6 typ
  prettyPrec prec (TList id i typ) = withinPar (prec > 6) "List[" ++ id ++ "<" ++ pretty i ++ "] " ++ prettyPrec 6 typ
  prettyPrec _ (TVar id) = id
  prettyPrec prec (TIForall id typ i j) = withinPar (prec > 4) $ "forall" ++ prettyGlobalAnnotations i j ++ " " ++ id ++ ". " ++ prettyPrec 4 typ

instance HasSize Type where
  typeSize TUnit = return Identity
  typeSize (TWire wt _) = return $ Wire wt
  typeSize (TTensor ts) = do
    wirecounts <- mapM typeSize ts
    return $ foldl Parallel Identity wirecounts
  typeSize (TCirc {}) = return Identity
  typeSize (TArrow _ _ _ i) = i
  typeSize (TBang _ _) = return Identity
  typeSize (TList id i t) = do
    wirecount <- typeSize t
    return $ BoundedParallel id i wirecount
  typeSize (TIForall _ _ _ i) = i
  typeSize (TVar _) = error "Cannot count wires of a type variable"

instance HasIndex Type where
  iv :: Type -> Set.HashSet IVarId
  iv TUnit = Set.empty
  iv (TWire _ i) = iv i
  iv (TTensor ts) = foldr (Set.union . iv) Set.empty ts
  iv (TCirc i _ _) = iv i
  iv (TArrow typ1 typ2 i j) = iv typ1 `Set.union` iv typ2 `Set.union` iv i `Set.union` iv j
  iv (TBang i typ) = iv i `Set.union` iv typ
  iv (TList id i typ) = Set.insert id $ iv i `Set.union` iv typ
  iv (TVar _) = Set.empty
  iv (TIForall id typ i j) = Set.insert id (iv typ `Set.union` iv i `Set.union` iv j)
  ifv :: Type -> Set.HashSet IVarId
  ifv TUnit = Set.empty
  ifv (TWire _ i) = ifv i
  ifv (TTensor ts) = foldr (Set.union . ifv) Set.empty ts
  ifv (TCirc i _ _) = ifv i
  ifv (TArrow typ1 typ2 i j) = ifv typ1 `Set.union` ifv typ2 `Set.union` ifv i `Set.union` ifv j
  ifv (TBang i typ) = ifv i `Set.union` ifv typ
  ifv (TList id i typ) = Set.delete id $ ifv i `Set.union` ifv typ
  ifv (TVar _) = Set.empty
  ifv (TIForall id typ i j) = Set.delete id (ifv typ `Set.union` ifv i `Set.union` ifv j)
  isub :: IndexSubstitution -> Type -> Type
  isub _ TUnit = TUnit
  isub sub (TWire wtype j) = TWire wtype (isub sub j)
  isub sub (TTensor ts) = TTensor (map (isub sub) ts)
  isub sub (TCirc j inBtype outBtype) = TCirc (isub sub j) (isub sub inBtype) (isub sub outBtype) -- Bundle types have no free variables
  isub sub (TArrow typ1 typ2 j k) = TArrow (isub sub typ1) (isub sub typ2) (isub sub j) (isub sub k)
  isub sub (TBang j typ) = TBang (isub sub j) (isub sub typ)
  isub sub (TList id j typ) = --TODO check
    let id' = fresh (fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub)) [typ]
        renaming = isubSingleton id (IVar id')  
        in TList id' (isub sub . isub renaming $ j) (isub sub . isub renaming $ typ)
  isub _ (TVar a) = TVar a
  isub sub (TIForall id typ j k) =
    let id' = fresh (fresh (fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub)) [j,k]) [typ]
        renaming = isubSingleton id (IVar id')  
     in TIForall id' (isub sub . isub renaming $ typ) (isub sub . isub renaming $ j) (isub sub . isub renaming $ k)

-- | @isLinear t@ checks whether type @t@ is a linear type
isLinear :: Type -> Bool
isLinear TUnit = False
isLinear (TWire _ _) = True
isLinear (TTensor ts) = any isLinear ts
isLinear (TCirc {}) = False
isLinear (TArrow {}) = True
isLinear (TBang _ _) = False
isLinear (TList _ (Number 0) _) = False  -- Empty lists can be discarded
isLinear (TList _ _ typ) = isLinear typ
isLinear (TVar _) = False -- Variables are only used in the pre-processing stage, so we are permissive here
isLinear (TIForall _ typ _ _) = isLinear typ

-- | @isBundleType t@ checks whether type @t@ is a bundle type,
-- that is, a wire type or a tuple/list of bundle types
isBundleType :: Type -> Bool
isBundleType TUnit = True
isBundleType (TWire _ _) = True
isBundleType (TTensor ts) = all isBundleType ts
isBundleType (TList _ _ typ) = isBundleType typ
isBundleType _ = False

-- | @stripGlobalAnnotations t@ removes all global metric annotations from type @t@
stripGlobalAnnotations :: Type -> Type
stripGlobalAnnotations TUnit = TUnit
stripGlobalAnnotations (TVar id) = TVar id
stripGlobalAnnotations (TArrow t1 t2 _ _) = TArrow (stripGlobalAnnotations t1) (stripGlobalAnnotations t2) Nothing Nothing
stripGlobalAnnotations (TIForall id t _ _) = TIForall id (stripGlobalAnnotations t) Nothing Nothing
stripGlobalAnnotations (TList id i t) = TList id i (stripGlobalAnnotations t)
stripGlobalAnnotations (TBang _ t) = TBang Nothing (stripGlobalAnnotations t)
stripGlobalAnnotations (TCirc _ t1 t2) = TCirc Nothing (stripGlobalAnnotations t1) (stripGlobalAnnotations t2)
stripGlobalAnnotations (TTensor ts) = TTensor (map stripGlobalAnnotations ts)
stripGlobalAnnotations (TWire wt i) = TWire wt i

-- | @stripLocalAnnotations t@ removes all local metric annotations from type @t@
stripLocalAnnotations :: Type -> Type
stripLocalAnnotations TUnit = TUnit
stripLocalAnnotations (TVar id) = TVar id
stripLocalAnnotations (TArrow t1 t2 i j) = TArrow (stripLocalAnnotations t1) (stripLocalAnnotations t2) i j
stripLocalAnnotations (TIForall id t i j) = TIForall id (stripLocalAnnotations t) i j
stripLocalAnnotations (TList id i t) = TList id i (stripLocalAnnotations t)
stripLocalAnnotations (TBang i t) = TBang i (stripLocalAnnotations t)
stripLocalAnnotations (TCirc i t1 t2) = TCirc i (stripLocalAnnotations t1) (stripLocalAnnotations t2)
stripLocalAnnotations (TTensor ts) = TTensor (map stripLocalAnnotations ts)
stripLocalAnnotations (TWire wt _) = TWire wt Nothing


--- TYPE SIZE ---------------------------------------------------------------------------------

-- The class of datatypes that are amenable to a notion of "circuit size"
class HasSize a where
  typeSize :: a -> Maybe Index -- #(•) in the paper

instance HasSize WireType where
  typeSize wt = Just (Wire wt)

-- Any traversable structure of sized elements is itself sized
instance (Traversable t, HasSize a) => HasSize (t a) where
  typeSize x = do
    wirecounts <- mapM typeSize x
    return $ foldr Parallel Identity wirecounts -- disjoint pieces of data move "in parallel"

instance HasSize a => HasSize (Maybe a) where
  typeSize Nothing = Nothing
  typeSize (Just x) = typeSize x