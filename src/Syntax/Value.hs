{-# OPTIONS_GHC -Wno-partial-fields #-}

module Syntax.Value (Value (..)) where

import Data.Text (Text)
import Interpreter.Env (Env)
import Syntax.Expr (Expr)

data Value
  = Num {num :: Int}
  | Boolean {bool :: Bool}
  | Closure {argNames :: [Text], body :: Expr, envClos :: Env}
  | Pair {first :: Value, second :: Value}
  | String {value :: Text}
  deriving (Show, Eq)
