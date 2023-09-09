module Syntax.Operations (BinaryOp (..)) where

import Data.Aeson
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data BinaryOp
  = -- | Addition
    Add
  | -- | Subtractions
    Sub
  | -- | Multiplication
    Mul
  | -- | Division
    Div
  | -- | Division remainder
    Rem
  | -- | Equality
    Eq
  | -- | Inequality
    Neq
  | -- | Less than
    Lt
  | -- | Greater than
    Gt
  | -- | Less than or equal
    Lte
  | -- | Greater than or equal
    Gte
  | -- | Logical and
    And
  | -- | Logical or
    Or
  deriving (Show, Eq, Generic, Read)

instance FromJSON BinaryOp where
  parseJSON = withObject "BinaryOp" $ \obj -> do
    kind <- obj .: "op"
    case readMaybe kind of
      Nothing -> fail "invalid BinaryOp"
      Just op -> return op

instance ToJSON BinaryOp

-- >>> eitherDecode "{\"op\": \"Gt\"}" :: Either String BinaryOp
-- Right Gt
