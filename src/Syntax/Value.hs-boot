module Syntax.Value (Value) where

-- Avoid cyclic dependency issue with `Interpreter.Env`.

data Value

instance Show Value

instance Eq Value
