{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Syntax.Value (Value (..)) where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Interpreter.Env (Env)
import Syntax.OptExpr (OptExpr)
import TextShow

data Value
  = Num {num :: Int}
  | Boolean {bool :: Bool}
  | Closure {argKeys :: [Int], body :: OptExpr, envClos :: Env}
  | Pair {first :: Value, second :: Value}
  | String {value :: Text}
  deriving (Show, Eq)

instance TextShow Value where
  showb val = case val of
    Num num -> showb num
    Boolean bool -> showb bool
    Closure {} -> "<#closure>"
    Pair first second -> showb [i|(#{showb first}, #{showb second})|]
    String val -> fromText val
