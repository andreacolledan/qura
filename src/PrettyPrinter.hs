{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter
  ( Pretty (..),
    withinPar
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.HashSet as Set

-- | The @Pretty@ typeclass is used to define a pretty-printing function 'pretty' for a type.
class Pretty a where
  -- | The 'pretty' function is used to pretty-print a value of type @a@.
  pretty :: a -> String
  pretty = prettyPrec 0
  prettyPrec :: Int -> a -> String
  prettyPrec _ = pretty

withinPar :: Bool -> String -> String
withinPar True s = "(" ++ s ++ ")"
withinPar False s = s

-- If v is pretty, then so is a HashMap with String keys and v values (used for typing contexts)
instance (Pretty v) => Pretty (HashMap.HashMap String v) where
  pretty m = "{" ++ List.intercalate ", " (List.map (\(k, v) -> k ++ " : " ++ pretty v) (HashMap.toList m)) ++ "}"

-- If v is pretty, then so is a Set of v (used for index contexts)
instance (Pretty a) => Pretty (Set.HashSet a) where
  pretty s = "{" ++ List.intercalate ", " (List.map pretty (Set.toList s)) ++ "}"

-- If a and b are both pretty, then so is the sum of a and b
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a) = pretty a
  pretty (Right b) = pretty b

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "_"
  pretty (Just a) = pretty a
