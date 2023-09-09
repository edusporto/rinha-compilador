{-# OPTIONS_GHC -Wno-partial-fields #-}

module Syntax.Expr (Expr (..), Parameter (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.List (isInfixOf)
import Data.Text (Text)
import GHC.Generics (Generic)
import Syntax.Operations (BinaryOp)

data Expr
  = Int {valueInt :: Int}
  | Str {valueStr :: Text}
  | Call {callee :: Expr, arguments :: [Expr]}
  | Binary {lhs :: Expr, op :: BinaryOp, rhs :: Expr}
  | Function {parameters :: [Parameter], value :: Expr}
  | Let {name :: Parameter, value :: Expr, next :: Expr}
  | If {condition :: Expr, thenBody :: Expr, elseBody :: Expr}
  | Print {value :: Expr}
  | First {value :: Expr}
  | Second {value :: Expr}
  | Bool {valueBool :: Bool}
  | Tuple {first :: Expr, second :: Expr}
  | Var {textVar :: Text}
  deriving (Show, Eq, Generic)

newtype Parameter = Parameter {text :: Text}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- >>> encode $ Parameter "Hi!"
-- "{\"text\":\"Hi!\"}"

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \label ->
           if
               | "value" `isInfixOf` label -> "value"
               | "then" `isInfixOf` label -> "then"
               | "else" `isInfixOf` label -> "else"
               | "text" `isInfixOf` label -> "text"
               | otherwise -> label,
         sumEncoding =
           TaggedObject
             { tagFieldName = "kind",
               contentsFieldName = "contents"
             }
       }
     ''Expr
 )

-- >>> decode "true" :: Maybe Bool
-- Just True

-- >>> encode $ Int 5
-- "{\"kind\":\"Int\",\"value\":5}"
-- >>> decode "{\"kind\":\"Int\",\"value\":5}" :: Maybe Expr
-- Just (Int {valueInt = 5})
-- >>> decode "{\"kind\":\"Int\",\"value\":5, \"bla\": 0}" :: Maybe Expr
-- Just (Int {valueInt = 5})
