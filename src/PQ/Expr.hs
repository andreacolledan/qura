{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PQ.Expr
  ( VariableId,
    Pattern (..),
    Expr (..),
    isBundle,
    exprToWirebundle,
    wirebundleToExpr,
    renameInPattern,
    psub
  )
where

import Analyzer.Unify 
  -- (
  --   HasType (..),
  --   TypeSubstitution,
  --   HasIndex(..),
  --   IndexSubstitution,
  --   isubSingleton,
  --   isubDomain,
  --   isubCodomain,
  --   fresh
  -- )
import qualified Data.HashSet as HSet
import Data.List (intercalate)
import PQ.Constant
import PQ.Index
import PQ.Type
import PrettyPrinter (Pretty (..))
import Circuit
import Interpreter.RuntimeError

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)


type VariableId = String

-- | The datatype of binding patterns
data Pattern
  = PHole                 -- Ignore pattern   : _ 
  | PVar VariableId       -- Variable pattern : x, y, z, ...
  | PTuple [Pattern]      -- Tuple pattern    : (p1, p2, ...)
  | PCons Pattern Pattern -- Cons pattern     : p1 : p2
  deriving (Eq, Show)

varsInPattern :: Pattern -> Set.Set VariableId
varsInPattern PHole = Set.empty
varsInPattern (PVar v) = Set.singleton v
varsInPattern (PTuple ps) = Set.unions (map varsInPattern ps)
varsInPattern (PCons p1 p2) = Set.union (varsInPattern p1) (varsInPattern p2)

instance Pretty Pattern where
  pretty PHole = "_"
  pretty (PVar id) = id
  pretty (PTuple ps) = "(" ++ intercalate ", " (map pretty ps) ++ ")"
  pretty (PCons p1 p2) = "(" ++ pretty p1 ++ ":" ++ pretty p2 ++ ")"

renameInPattern :: VariableId -> VariableId -> Pattern -> Pattern
renameInPattern old new pat = case pat of
  PHole -> PHole
  PVar v
    | v == old -> PVar new
    | otherwise -> PVar v
  PTuple ps -> PTuple (map (renameInPattern old new) ps)
  PCons p1 p2 -> PCons (renameInPattern old new p1) (renameInPattern old new p2)


-- | The datatype of PQR expressions
data Expr =
  EUnit                                       -- Unit value               : ()
  | EVar VariableId                           -- Variable                 : x, y, z, ...          
  | ELab Label                                -- Label                    : ℓ, k
  | ETuple [Expr]                             -- Pair                     : (e1, e2)
  | EAbs Pattern Type Expr                    -- Abstraction              : \p :: t . e
  | ECirc WireBundle Circuit WireBundle       -- Box circuit              : (ℓ,D,ℓ')
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

exprFreeVars :: Expr -> Set.Set VariableId
exprFreeVars EUnit = Set.empty
exprFreeVars (EVar x) = Set.singleton x
exprFreeVars (ELab _) = Set.empty -- ??
exprFreeVars (ETuple es) = Set.unions (map exprFreeVars es)
exprFreeVars (EAbs p _ body) = exprFreeVars body `Set.difference` varsInPattern p
exprFreeVars (ECirc _ _ _) = Set.empty -- ??
exprFreeVars (ELift e) = exprFreeVars e
exprFreeVars (ENil _) = Set.empty
exprFreeVars (ECons e1 e2) = Set.union (exprFreeVars e1) (exprFreeVars e2)
exprFreeVars (EFold e1 e2 e3) = Set.unions (map exprFreeVars [e1,e2,e3])
exprFreeVars (EApp e1 e2) = Set.union (exprFreeVars e1) (exprFreeVars e2)
exprFreeVars (EApply e1 e2) = Set.union (exprFreeVars e1) (exprFreeVars e2)
exprFreeVars (EBox _ e) = exprFreeVars e
exprFreeVars (EForce e) = exprFreeVars e
exprFreeVars (ELet p e1 e2) =
  Set.union (exprFreeVars e1) (exprFreeVars e2 `Set.difference` varsInPattern p)
exprFreeVars (EAnno e _) = exprFreeVars e
exprFreeVars (EIAbs _ e) = exprFreeVars e
exprFreeVars (EIApp e _) = exprFreeVars e
exprFreeVars (EConst _) = Set.empty
exprFreeVars (EAssume e _) = exprFreeVars e

instance Pretty Expr where
  pretty EUnit = "()"
  pretty (EVar id) = id
  pretty (ELab l) = l
  pretty (ETuple es) = "(" ++ intercalate ", " (map pretty es) ++ ")"
  pretty (EAbs p t e) = "(\\" ++ pretty p ++ " :: " ++ pretty t ++ " . " ++ pretty e ++ ")" 
  pretty (ECirc ins circ outs) = "(" ++ pretty ins ++ ", " ++ "[BOXED CIRC]" ++ ", "++ pretty outs ++")" -- FIXME if we pretty circ we get a loooot of lines no?
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
  tfv :: Expr -> HSet.HashSet TVarId
  tfv EUnit = HSet.empty
  tfv (EVar _) = HSet.empty
  tfv (ETuple es) = foldr (HSet.union . tfv) HSet.empty es
  tfv (EAbs _ t e) = tfv t `HSet.union` tfv e
  tfv (EApp e1 e2) = tfv e1 `HSet.union` tfv e2
  tfv (ELift e) = tfv e
  tfv (EForce e) = tfv e
  tfv (ENil anno) = maybe HSet.empty tfv anno
  tfv (ECons e1 e2) = tfv e1 `HSet.union` tfv e2
  tfv (EFold e1 e2 e3) = tfv e1 `HSet.union` tfv e2 `HSet.union` tfv e3
  tfv (EAnno e t) = tfv e `HSet.union` tfv t
  tfv (EApply e1 e2) = tfv e1 `HSet.union` tfv e2
  tfv (EBox _ e) = tfv e
  tfv (ELet _ e1 e2) = tfv e1 `HSet.union` tfv e2
  tfv (EIAbs _ e) = tfv e
  tfv (EIApp e _) = tfv e
  tfv (EConst _) = HSet.empty
  tfv (EAssume e t) = tfv e `HSet.union` tfv t
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

-- newtype IndexSubstitution = IndexSubstitution (Map.HashMap IVarId Index)

-- i dont remember if i can put this on Analyzer/Unify.hs due to conflicts
instance HasIndex Expr where
  iv :: Expr -> HSet.HashSet IVarId
  iv _ = undefined
  ifv :: Expr -> HSet.HashSet IVarId
  ifv _ = undefined
  isub :: IndexSubstitution -> Expr -> Expr
  -- look for Type and Index in the Expr and sub inside them
  isub _ EUnit = EUnit 
  isub _ (EVar id) = EVar id
  isub _ (ELab l) = ELab l
  isub sub (ETuple es) = ETuple (map (isub sub) es)
  isub sub (EAbs p t e) = EAbs p (isub sub t) (isub sub e)
  isub _ (ECirc ins circ outs) = ECirc ins circ outs
  isub sub (EApp e1 e2) = EApp (isub sub e1) (isub sub e2)
  isub sub (ELift e) = ELift $ isub sub e
  isub sub (EForce e) = EForce $ isub sub e
  isub sub (ENil typ) = case typ of
    Nothing -> ENil Nothing
    Just t -> ENil $ Just $ isub sub t 
  isub sub (ECons e1 e2) = ECons (isub sub e1) (isub sub e2)
  isub sub (EFold e1 e2 e3) = EFold (isub sub e1) (isub sub e2) (isub sub e3)
  isub sub (EAnno e t) = EAnno (isub sub e) (isub sub t)
  isub sub (EApply e1 e2) = EApply (isub sub e1) (isub sub e2)
  isub sub (EBox typ e) = case typ of
    Nothing -> EBox Nothing $ isub sub e
    Just t -> EBox (Just $ isub sub t) $ isub sub e
  isub sub (ELet p e1 e2) = ELet p (isub sub e1) (isub sub e2)
  isub sub (EIAbs id e) = -- bounds the index variable
  -- TODO: check
    let id' = fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub)
        renaming = isubSingleton id (IVar id')
    in EIAbs id' (isub sub e)
  isub sub (EIApp e i) = EIApp (isub sub e) (isub sub i)
  isub _ (EConst c) = EConst c
  isub sub (EAssume e t) = EAssume (isub sub e) (isub sub t)

------------------------------------------------

psub :: Pattern -> Expr -> Expr -> Expr
psub x v m = case x of
  PHole -> m

  PVar pvar -> 
    case m of
      EUnit -> EUnit

      EVar y -> if pvar == y then v else m
      
      ELab l -> ELab l

      ETuple [] -> ETuple []
      ETuple (et:ets) ->
        let 
          et' = psub x v et
          (ETuple ets') = psub x v (ETuple ets)
        in ETuple (et':ets')

      EAbs p typ e ->
        let boundVars = varsInPattern p
            conflictVars = Set.intersection boundVars (exprFreeVars v)
        in if Set.null conflictVars
          then EAbs p typ (psub x v e)  -- no conflict, safe to substitute
          else
            -- there are conflicts, rename each conflicting variable
            let
              -- generate fresh names for each conflicting variable
              freshMap = Map.fromSet (\y -> freshVariableId (exprFreeVars e `Set.union` exprFreeVars v) y) conflictVars
              -- rename them in the body
              e' = foldr (\(old,new) acc -> rename old new acc) e (Map.toList freshMap)
              -- rename them in the pattern
              p' = foldr (\(old,new) acc -> renameInPattern old new acc) p (Map.toList freshMap)
            in EAbs p' typ (psub x v e')


      ECirc l c k -> ECirc l c k 

      ELift m' -> ELift $ psub x v m'

      ENil typ -> ENil typ

      ECons w1 w2 -> ECons (psub x v w1) (psub x v w2)

      EFold w1 w2 w3 -> EFold (psub x v w1) (psub x v w2) (psub x v w3)

      EApp w1 w2 -> EApp (psub x v w1) (psub x v w2)

      EApply w1 w2 -> EApply (psub x v w1) (psub x v w2)

      EBox t w -> EBox t $ psub x v w

      EForce w -> EForce $ psub x v w

      ELet p e1 e2 ->
        let e1'  = psub x v e1
            fvV  = exprFreeVars v
            bnds = varsInPattern p
        in case p of
          PHole ->
            ELet PHole e1' (psub x v e2)

          PVar y ->
            if y == pvar then
              -- binder shadows, skip substitution in e2
              ELet (PVar y) e1' e2
            else if y `Set.member` fvV then
              let y'  = freshVariableId (fvV `Set.union` exprFreeVars e2) y
                  e2' = rename y y' e2
              in ELet (PVar y') e1' (psub x v e2')
            else
              ELet (PVar y) e1' (psub x v e2)

          PTuple ps ->
            case e1' of
              ETuple es | length ps == length es ->
                let body = psub x v e2
                in foldr (\(pi,ei) acc -> ELet pi ei acc) body (zip ps es)
              _ ->
                if pvar `Set.member` bnds
                  then ELet p e1' e2
                  else ELet p e1' (psub x v e2)

          PCons ph pt ->
            case e1' of
              ECons eh et ->
                let body = psub x v e2
                in ELet ph eh (ELet pt et body)
              _ ->
                if pvar `Set.member` bnds
                  then ELet p e1' e2
                  else ELet p e1' (psub x v e2)

      EAnno w typ -> EAnno (psub x v w) typ

      EIAbs id e -> EIAbs id $ psub x v e

      EIApp e i -> EIApp (psub x v e) i

      EConst c -> EConst c

      EAssume w typ -> EAssume (psub x v w) typ

  PTuple [] -> m
  PTuple (pt:pts) -> 
    case v of
      ETuple etpl -> case etpl of
        [] -> ETuple []
        (et:ets) -> 
          let m' = psub pt et m
          in psub (PTuple pts) (ETuple ets) m'
      _ -> error "psub: cannot substitute tuple with non-tuple"

  PCons _ _ -> undefined
------------------------------------------------
isBundle :: Expr -> Bool
isBundle EUnit = True
isBundle (ELab _) = True
isBundle (ETuple ls) = all isBundle ls
isBundle (ECons _ _) = undefined -- prob needed
isBundle _ = False

exprToWirebundle :: Expr -> Either RuntimeError WireBundle
exprToWirebundle EUnit = Right $ WUnit
exprToWirebundle (ELab l) = Right $ WLab l
exprToWirebundle (ETuple ls) = do
  ws <- mapM exprToWirebundle ls
  return (WTuple ws)
exprToWirebundle (ECons h t) = undefined
exprToWirebundle e = Left $ RuntimeError ("Cannot convert the Expr:\n> "++show e++"\n to a WireBundle")

wirebundleToExpr :: WireBundle -> Expr
wirebundleToExpr (WUnit) = EUnit
wirebundleToExpr (WLab l) = ELab l
wirebundleToExpr (WTuple ls) = ETuple $ map wirebundleToExpr ls 

-- do we also update the context? isnt this a renaming inside the expr
-- before quantum operations are applied? -> i dont think ctx is needed
rename :: VariableId -> VariableId -> Expr -> Expr
-- rename old' v' config = trace("\nSub "++show old'++" with "++show v'++" in the config:\n"++pretty config)$undefined
-- README: I am not extracting circ' every time since it shouldnt change... hopefully
rename old v expr = 
  -- trace("\nRenaming "++show old++" to "++show v++" in "++pretty expr)$case expr of
  case expr of
    EUnit -> expr 
    
    EVar x
      | x==old -> EVar v
      | otherwise -> expr
    
    ELab _ -> expr
    
    ETuple es -> 
      let es' = map (rename old v) es
      in ETuple es'
    
    EAbs p typ body ->
      case p of 
        PHole -> EAbs p typ $ rename old v body
        PVar pvar
          | pvar==old -> EAbs (PVar v) typ (rename old v body)
          | otherwise -> EAbs (PVar pvar) typ (rename old v body)
        PTuple _ -> 
          let 
            p' = renameInPattern old v p
            body' = rename old v body
          in EAbs p' typ body'
        PCons _ _ -> 
          let 
            p' = renameInPattern old v p
            body' = rename old v body
          in EAbs p' typ body'
    
    ECirc _ _ _ -> expr
    
    ELift e -> ELift $ rename old v e
    
    ENil _ -> expr
    
    ECons e1 e2 -> ECons (rename old v e1) (rename old v e2)
    
    EFold e1 e2 e3-> EFold (rename old v e1) (rename old v e2) (rename old v e3)
    
    EApp e1 e2 -> EApp (rename old v e1) (rename old v e2)
    
    EApply e1 e2 -> EApply (rename old v e1) (rename old v e2)
    
    EBox typ e -> EBox typ (rename old v e)
    
    EForce e -> EForce $ rename old v e
    
    ELet p e1 e2-> ELet (renameInPattern old v p) (rename old v e1) (rename old v e2)
    
    EAnno e typ -> EAnno (rename old v e) typ
    
    EIAbs i e -> EIAbs i (rename old v e)
    
    EIApp e i -> EIApp (rename old v e) i
    
    EConst _ -> expr
    
    EAssume e typ -> EAssume (rename old v e) typ

-- do we need the context?
freshVariableId :: Set.Set VariableId -> VariableId -> VariableId
freshVariableId used x = head $ dropWhile (`Set.member` used) candidates
  where
    candidates = [x ++ replicate n '\'' | n <- [0..]]

