module Interpreter.Metric where

import PrettyPrinter

-- | This class is used to represent the static metrics of a curcuit object.
data ProgramMetrics = ProgMetrics {
  width :: Int,
  depth :: Int,
  gatecount :: Int
} deriving Show

instance Pretty ProgramMetrics where
  pretty ProgMetrics {width = w, depth = d, gatecount = gc} =
    "Metric Values:\n" ++
    " - Width: " ++ show w ++ "\n" ++
    " - Depth: " ++ show d ++ "\n" ++
    " - Gatecount: " ++ show gc ++ "\n" 