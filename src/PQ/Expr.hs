{-# LANGUAGE InstanceSigs #-}

module PQ.Expr
  ( VariableId,
    Pattern (..),
    Expr (..),
  )
where

import Analyzer.Unify (HasType (..), TypeSubstitution)
import qualified Data.HashSet as Set
import Data.List (intercalate)
import PQ.Constant
import PQ.Index
import PQ.Type
import PrettyPrinter (Pretty (..))


type VariableId = String

-- | The datatype of binding patterns
data Pattern
  = PHole                 -- Ignore pattern   : _ 
  | PVar VariableId       -- Variable pattern : x, y, z, ...
  | PTuple [Pattern]      -- Tuple pattern    : (p1, p2, ...)
  | PCons Pattern Pattern -- Cons pattern     : p1 : p2
  deriving (Eq, Show)

instance Pretty Pattern where
  pretty PHole = "_"
  pretty (PVar id) = id
  pretty (PTuple ps) = "(" ++ intercalate ", " (map pretty ps) ++ ")"
  pretty (PCons p1 p2) = "(" ++ pretty p1 ++ ":" ++ pretty p2 ++ ")"


-- | The datatype of PQR expressions
data Expr =
  EUnit                                       -- Unit value               : ()
  | EVar VariableId                           -- Variable                 : x, y, z, ...          
  | ETuple [Expr]                             -- Pair                     : (e1, e2)
  | EAbs Pattern Type Expr                    -- Abstraction              : \p :: t . e
  | ELift Expr                                -- Lift                     : lift e
  | ENil (Maybe Type)                         -- Nil                      : []
  | ECons Expr Expr                           -- Cons                     : e : es
  | EFold Expr Expr Expr                      -- Fold                     : fold (e1, e2, e3)
  | EApp Expr Expr                            -- Application              : e1 e2
  | EApply Expr Expr                          -- Apply                    : apply(e1, e2)
  | EBox (Maybe Type) Expr                    -- Box                      : box :: bt e
  | EForce Expr                               -- Force                    : force e
  | ELet Pattern Expr Expr                    -- Let                      : let p = e1 in e2
  | EAnno Expr Type                           -- Type annotation          : e :: t
  | EIAbs IVarId Expr                         -- Index Abstraction        : forall id . e
  | EIApp Expr Index                          -- Index Application        : e @ i
  | EConst Constant                           -- Constant                 : QInit0, Hadamard, ...
  | EAssume Expr Type                         -- Type assumption          : e !:: t
  deriving (Eq, Show)

instance Pretty Expr where
  pretty EUnit = "()"
  pretty (EVar id) = id
  pretty (ETuple es) = "(" ++ intercalate ", " (map pretty es) ++ ")"
  pretty (EAbs p t e) = "(\\" ++ pretty p ++ " :: " ++ pretty t ++ " . " ++ pretty e ++ ")" 
  pretty (EApp e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty e2 ++ ")"
  pretty (ELift e) = "(lift " ++ pretty e ++ ")"
  pretty (EForce e) = "(force " ++ pretty e ++ ")"
  pretty (ENil _) = "[]"
  pretty (ECons e1 e2) = "(" ++ pretty e1 ++ ":" ++ pretty e2 ++ ")"
  pretty (EFold e1 e2 e3) = "fold (" ++ pretty e1 ++ ", " ++ pretty e2 ++ ", " ++ pretty e3 ++ ")"
  pretty (EAnno e t) = "(" ++ pretty e ++ " :: " ++ pretty t ++ ")"
  pretty (EApply e1 e2) = "apply(" ++ pretty e1 ++ ", " ++ pretty e2 ++ ")"
  pretty (EBox _ e) = "(box" ++ " " ++ pretty e ++ ")"
  pretty (ELet p e1 e2) = "(let " ++ pretty p ++ " = " ++ pretty e1 ++ " in " ++ pretty e2 ++ ")"
  pretty (EIAbs id e) = "(forall " ++ id ++ " . " ++ pretty e ++ ")"
  pretty (EIApp e i) = "(" ++ pretty e ++ " @ " ++ pretty i ++ ")"
  pretty (EConst c) = pretty c
  pretty (EAssume e t) = "(" ++ pretty e ++ " !:: " ++ pretty t ++ ")"

instance HasType Expr where
  tfv :: Expr -> Set.HashSet TVarId
  tfv EUnit = Set.empty
  tfv (EVar _) = Set.empty
  tfv (ETuple es) = foldr (Set.union . tfv) Set.empty es
  tfv (EAbs _ t e) = tfv t `Set.union` tfv e
  tfv (EApp e1 e2) = tfv e1 `Set.union` tfv e2
  tfv (ELift e) = tfv e
  tfv (EForce e) = tfv e
  tfv (ENil anno) = maybe Set.empty tfv anno
  tfv (ECons e1 e2) = tfv e1 `Set.union` tfv e2
  tfv (EFold e1 e2 e3) = tfv e1 `Set.union` tfv e2 `Set.union` tfv e3
  tfv (EAnno e t) = tfv e `Set.union` tfv t
  tfv (EApply e1 e2) = tfv e1 `Set.union` tfv e2
  tfv (EBox _ e) = tfv e
  tfv (ELet _ e1 e2) = tfv e1 `Set.union` tfv e2
  tfv (EIAbs _ e) = tfv e
  tfv (EIApp e _) = tfv e
  tfv (EConst _) = Set.empty
  tfv (EAssume e t) = tfv e `Set.union` tfv t
  tsub :: TypeSubstitution -> Expr -> Expr
  tsub _ EUnit = EUnit
  tsub _ (EVar id) = EVar id
  tsub sub (ETuple es) = ETuple (map (tsub sub) es)
  tsub sub (EAbs id t e) = EAbs id (tsub sub t) (tsub sub e)
  tsub sub (EApp e1 e2) = EApp (tsub sub e1) (tsub sub e2)
  tsub sub (ELift e) = ELift (tsub sub e)
  tsub sub (EForce e) = EForce (tsub sub e)
  tsub sub (ENil mt) = ENil (tsub sub <$> mt)
  tsub sub (ECons e1 e2) = ECons (tsub sub e1) (tsub sub e2)
  tsub sub (EFold e1 e2 e3) = EFold (tsub sub e1) (tsub sub e2) (tsub sub e3)
  tsub sub (EAnno e t) = EAnno (tsub sub e) (tsub sub t)
  tsub sub (EApply e1 e2) = EApply (tsub sub e1) (tsub sub e2)
  tsub sub (EBox mt e) = EBox (tsub sub <$> mt) (tsub sub e)
  tsub sub (ELet id e1 e2) = ELet id (tsub sub e1) (tsub sub e2)
  tsub sub (EIAbs id e) = EIAbs id (tsub sub e)
  tsub sub (EIApp e i) = EIApp (tsub sub e) i
  tsub _ e@(EConst _) = e
  tsub sub (EAssume e t) = EAssume (tsub sub e) (tsub sub t)
  


  