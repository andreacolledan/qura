{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PQ.Expr
  ( VariableId,
    Pattern (..),
    Expr (..),
    isBundle,
    wirebundleToExpr,
    renamePattern,
    renameExpr,
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
import Circuit.Bundle

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

renamePattern :: Map.Map VariableId VariableId -> Pattern -> Pattern
renamePattern m pat = case pat of
  PHole -> PHole
  PVar x -> case Map.lookup x m of
              Just v  -> PVar v
              Nothing -> PVar x
  PTuple ps -> PTuple $ map (renamePattern m) ps
  PCons ph pt -> PCons (renamePattern m ph) (renamePattern m pt)


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

varsInExpr :: Expr -> Set.Set VariableId
varsInExpr EUnit = Set.empty
varsInExpr (EVar x) = Set.singleton x
varsInExpr (ELab _) = Set.empty -- ??
varsInExpr (ETuple es) = Set.unions (map varsInExpr es)
varsInExpr (EAbs p _ body) = varsInExpr body `Set.difference` varsInPattern p
varsInExpr (ECirc _ _ _) = Set.empty -- ??
varsInExpr (ELift e) = varsInExpr e
varsInExpr (ENil _) = Set.empty
varsInExpr (ECons e1 e2) = Set.union (varsInExpr e1) (varsInExpr e2)
varsInExpr (EFold e1 e2 e3) = Set.unions (map varsInExpr [e1,e2,e3])
varsInExpr (EApp e1 e2) = Set.union (varsInExpr e1) (varsInExpr e2)
varsInExpr (EApply e1 e2) = Set.union (varsInExpr e1) (varsInExpr e2)
varsInExpr (EBox _ e) = varsInExpr e
varsInExpr (EForce e) = varsInExpr e
varsInExpr (ELet p e1 e2) =
  Set.union (varsInExpr e1) (varsInExpr e2 `Set.difference` varsInPattern p)
varsInExpr (EAnno e _) = varsInExpr e
varsInExpr (EIAbs _ e) = varsInExpr e
varsInExpr (EIApp e _) = varsInExpr e
varsInExpr (EConst _) = Set.empty
varsInExpr (EAssume e _) = varsInExpr e

instance Pretty Expr where
  pretty EUnit = "()"
  pretty (EVar id) = id
  pretty (ELab l) = l
  pretty (ETuple es) = "(" ++ intercalate ", " (map pretty es) ++ ")"
  pretty (EAbs p t e) = "(\\" ++ pretty p ++ " :: " ++ pretty t ++ " . " ++ pretty e ++ ")" 
  pretty (ECirc ins circ outs) = "(boxed (" ++ pretty ins ++ ", " ++ "[BOXED CIRC]" ++ ", "++ pretty outs ++"))" -- FIXME if we pretty circ we get a loooot of lines no?
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
  -- TODO double check iv and ifv
instance HasIndex Expr where
  -- | @iv x@ returns the set of index variables (bound or free) that occur in @x@
  iv :: Expr -> HSet.HashSet IVarId
  -- iv _ = undefined
  iv EUnit            = HSet.empty
  iv (EVar _)         = HSet.empty
  iv (ELab _)         = HSet.empty
  iv (ETuple es)      = HSet.unions (map iv es)
  iv (EAbs _ t e)     = iv t `HSet.union` iv e
  iv (ECirc _ _ _)    = HSet.empty
  iv (EApp e1 e2)     = iv e1 `HSet.union` iv e2
  iv (ELift e)        = iv e
  iv (EForce e)       = iv e
  iv (ENil Nothing)   = HSet.empty
  iv (ENil (Just t))  = iv t
  iv (ECons e1 e2)    = iv e1 `HSet.union` iv e2
  iv (EFold e1 e2 e3) = HSet.unions [iv e1, iv e2, iv e3]
  iv (EAnno e t)      = iv e `HSet.union` iv t
  iv (EApply e1 e2)   = iv e1 `HSet.union` iv e2
  iv (EBox Nothing e) = iv e
  iv (EBox (Just t) e)= iv t `HSet.union` iv e
  iv (ELet _ e1 e2)   = iv e1 `HSet.union` iv e2
  iv (EIAbs i e)      = HSet.insert i (iv e)  -- bound variable also counted
  iv (EIApp e i)      = iv e `HSet.union` iv i
  iv (EConst _)       = HSet.empty
  iv (EAssume e t)    = iv e `HSet.union` iv t
  iv _                = HSet.empty
  -- | @ifv x@ returns the set of free index variables that occur in @x@
  ifv :: Expr -> HSet.HashSet IVarId
  -- ifv _ = undefined
  ifv = go HSet.empty
    where
      go bound (EIAbs i e)      = go (HSet.insert i bound) e
      go bound (EIApp e i)      = go bound e `HSet.union` ifv i
      go bound (ETuple es)      = HSet.unions (map (go bound) es)
      go bound (EAbs _ t e)     = ifv t `HSet.union` go bound e
      go bound (EApp e1 e2)     = go bound e1 `HSet.union` go bound e2
      go bound (ELift e)        = go bound e
      go bound (EForce e)       = go bound e
      go bound (ENil Nothing)   = HSet.empty
      go bound (ENil (Just t))  = ifv t
      go bound (ECons e1 e2)    = go bound e1 `HSet.union` go bound e2
      go bound (EFold e1 e2 e3) = HSet.unions [go bound e1, go bound e2, go bound e3]
      go bound (EAnno e t)      = go bound e `HSet.union` ifv t
      go bound (EApply e1 e2)   = go bound e1 `HSet.union` go bound e2
      go bound (EBox Nothing e) = go bound e
      go bound (EBox (Just t) e)= ifv t `HSet.union` go bound e
      go bound (ELet _ e1 e2)   = go bound e1 `HSet.union` go bound e2
      go bound (EAssume e t)    = go bound e `HSet.union` ifv t
      go bound (EVar _)         = HSet.empty
      go bound (ELab _)         = HSet.empty
      go bound (ECirc _ _ _)    = HSet.empty
      go bound (EConst _)       = HSet.empty
      go bound _                = HSet.empty
-- | @isub sub x@ substitutes the index variable @id@ by the index @i@ in @x@
  isub :: IndexSubstitution -> Expr -> Expr
  isub sub (EIAbs id e) = -- bounds the index variable
    let id' = fresh id ((IVar <$> isubDomain sub) ++ isubCodomain sub) -- ++ (IVar <$> (HSet.toList $ ifv e))) -- TODO to we need to add vars in e?
        renaming = isubSingleton id (IVar id')
    in EIAbs id' (isub sub . isub renaming $ e)
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
  isub sub (EIApp e i) = EIApp (isub sub e) (isub sub i)
  isub _ (EConst c) = EConst c
  isub sub (EAssume e t) = EAssume (isub sub e) (isub sub t)

--------------------------------------------------------------------------------

freshVariableId :: Set.Set VariableId -> VariableId -> VariableId
freshVariableId used x = head $ dropWhile (`Set.member` used) candidates
  where
    candidates = [x ++ replicate n '\'' | n <- [0..]]

getSetRenaming :: Set.Set VariableId -> Set.Set VariableId -> Map.Map VariableId VariableId
getSetRenaming toRename toAvoid =
  fst $ foldl go (Map.empty, toAvoid) (Set.toList toRename)
  where
    go (m, used) x =
      let x' = freshVariableId used x
      in if x' == x
           then (m, used)               -- no renaming needed, keep map as-is
           else (Map.insert x x' m, Set.insert x' used)

-- TODO maybe change return type to either runtimerror expr
-- (if the errors raised can even happen after type checking) (idk)
psub :: Pattern -> Expr -> Expr -> Expr
-- psub x v m = trace("\n====\nsubbing: "++show x ++"\nwith: "++pretty v++"\nin:\n>>> "++pretty m)$case x of
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
        -- trace ("\n[psub:EAbs] trying to substitute " ++ show x ++ " with " ++ pretty v
        --       ++ "\n in expr: " ++ pretty (EAbs p typ e)) $

        let pVars = varsInPattern p in
        -- trace ("[psub:EAbs] pattern vars: " ++ show pVars) $

        if pvar `Set.member` pVars
          -- then trace ("[psub:EAbs] shadowing detected: " ++ show pvar
          --             ++ " is bound in " ++ show pVars
          --             ++ " → skipping substitution in body") $
          then  EAbs p typ e
          else
            let vVars = varsInExpr v in
            -- trace ("[psub:EAbs] free vars in v: " ++ show vVars) $

            let renaming = getSetRenaming pVars vVars in
            -- trace ("[psub:EAbs] renaming computed: " ++ show renaming) $

            let torename = (EAbs p typ e) in
            -- trace ("[psub:EAbs] applying renaming map to: "++pretty torename) $
            
            let (EAbs p' typ e') = renameExpr renaming torename in
            -- trace ("[psub:EAbs] after renaming: " ++ pretty (EAbs p' typ e')) $

            let recCall = psub x v e' in
            -- trace ("[psub:EAbs] recursive call result: " ++ pretty recCall) $
            EAbs p' typ recCall

      -- EAbs p typ e ->
      --   let pVars = varsInPattern p in
      --   if pvar `Set.member` pVars
      --     then EAbs p typ e -- can't sub x in: \x . (...)
      --     else -- ensure that variables in v are different from those in p
      --       let
      --         vVars = varsInExpr v
      --         renaming = getSetRenaming pVars vVars -- new names map for p` (possibly empty)
      --         (EAbs p' typ e') = renameExpr renaming $ EAbs p typ e
      --       in EAbs p' typ $ psub x v e'

      ECirc l c k -> ECirc l c k 

      ELift m' -> ELift $ psub x v m'

      ENil typ -> ENil typ

      ECons w1 w2 -> ECons (psub x v w1) (psub x v w2)

      EFold w1 w2 w3 -> EFold (psub x v w1) (psub x v w2) (psub x v w3)

      EApp w1 w2 -> EApp (psub x v w1) (psub x v w2)

      EApply w1 w2 -> EApply (psub x v w1) (psub x v w2)

      EBox t w -> EBox t $ psub x v w

      EForce w -> EForce $ psub x v w

      ELet p e1 e2 -> -- really similar to EAbs
        let 
          e1' = psub x v e1 
          pVars = varsInPattern p
        in if pvar `Set.member` pVars
          then ELet p e1' e2
          else
            let
              vVars = varsInExpr v
              renaming = getSetRenaming pVars vVars
              e2' = renameExpr renaming e2
              p' = renamePattern renaming p
              e2'' = psub x v e2'
            in ELet p' e1' e2''

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
      _ -> error $ "psub: cannot substitute tuple with non-tuple\n Tried subbing:\n> "++show x++"\n with:\n> "++pretty v

  PCons phead ptail ->
    case v of
      ECons vhead vtail ->
        let m' = psub phead vhead m
        in psub ptail vtail m'
      _ -> error "psub: cannot substitute cons pattern with non-cons value"


------------------------------------------------
isBundle :: Expr -> Bool
isBundle EUnit = True
isBundle (ELab _) = True
isBundle (ETuple ls) = all isBundle ls
isBundle (ECons h t) = isBundle h && isBundle t
isBundle (ENil _) = True -- idk... can it appear by itself? 
isBundle _ = False

wirebundleToExpr :: WireBundle -> Expr
wirebundleToExpr (WUnit) = EUnit
wirebundleToExpr (WLab l) = ELab l
wirebundleToExpr (WTuple ls) = ETuple $ map wirebundleToExpr ls 
wirebundleToExpr (WNil btyp) = ENil $ maybeBundleTypeToType btyp
wirebundleToExpr (WCons h t) = ECons (wirebundleToExpr h) (wirebundleToExpr t)

renameExpr :: Map.Map VariableId VariableId -> Expr -> Expr
renameExpr m expr =
  if Map.null m 
    then expr
    else
    -- trace("\nRenaming "++show m++" in "++pretty expr)$case expr of
    case expr of
      EUnit -> EUnit

      EVar x -> case Map.lookup x m of
                  Just v  -> EVar v
                  Nothing -> EVar x

      ELab l -> ELab l

      ETuple es -> ETuple $ map (renameExpr m) es

      EAbs p typ body ->
        let p' = renamePattern m p
            body' = renameExpr m body
        in EAbs p' typ body'

      ECirc l c k -> ECirc l c k

      ELift e -> ELift $ renameExpr m e

      ENil typ -> ENil typ

      ECons e1 e2 -> ECons (renameExpr m e1) (renameExpr m e2)

      EFold e1 e2 e3 -> EFold (renameExpr m e1) (renameExpr m e2) (renameExpr m e3)

      EApp e1 e2 -> EApp (renameExpr m e1) (renameExpr m e2)

      EApply e1 e2 -> EApply (renameExpr m e1) (renameExpr m e2)

      EBox typ e -> EBox typ (renameExpr m e)

      EForce e -> EForce (renameExpr m e)

      ELet p e1 e2 -> ELet (renamePattern m p) (renameExpr m e1) (renameExpr m e2)

      EAnno e typ -> EAnno (renameExpr m e) typ

      EIAbs i e -> EIAbs i (renameExpr m e)

      EIApp e i -> EIApp (renameExpr m e) i

      EConst c -> EConst c

      EAssume e typ -> EAssume (renameExpr m e) typ
