module Syntax.Program (Program (..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Syntax.Term (Term)

data Program = Program {name :: String, expression :: Term}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
