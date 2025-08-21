module Interpreter.RuntimeError where

-- for now I always return RuntimeError with a simple desc, laer we will create the different errors
data RuntimeError = 
  RuntimeError String --TODO: definition of runtime error data type, similar to Analyzer.TypeError
  deriving Show