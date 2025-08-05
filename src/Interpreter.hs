module Interpreter where

import Interpreter.RuntimeError
import Interpreter.Configuration

-- | @runInterpreter mod libs@ interprets module @mod@, with libraries @libs@.
-- Returns either a runtime error, or a configuration of a circuit object and a value.
runInterpreter :: Module -> [Module] -> Either RuntimeError Configuration
runInterpreter mod libs = undefined --TODO