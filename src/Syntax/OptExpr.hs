{-# OPTIONS_GHC -Wno-partial-fields #-}

module Syntax.OptExpr (OptExpr (..), IntParameter (..)) where

import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Syntax.Operations (BinaryOp)

-- Didn't really help all that much...
data OptExpr
  = Int {valueInt :: Int32}
  | Str {valueStr :: Text}
  | Call {callee :: OptExpr, arguments :: [OptExpr]}
  | Binary {lhs :: OptExpr, op :: BinaryOp, rhs :: OptExpr}
  | Function {parameters :: [IntParameter], value :: OptExpr}
  | Let {name :: IntParameter, value :: OptExpr, next :: OptExpr}
  | If {condition :: OptExpr, thenBody :: OptExpr, otherwiseBody :: OptExpr}
  | Print {value :: OptExpr}
  | First {value :: OptExpr}
  | Second {value :: OptExpr}
  | Bool {valueBool :: Bool}
  | Tuple {first :: OptExpr, second :: OptExpr}
  | Var {keyVar :: Int}
  deriving (Show, Eq, Generic)

newtype IntParameter = IntParameter {key :: Int}
  deriving (Show, Eq, Generic)
