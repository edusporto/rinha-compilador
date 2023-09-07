module Syntax.Program (Program (..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Syntax.Expr (Expr)

data Program = Program {name :: String, expression :: Expr}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
