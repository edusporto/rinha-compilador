{-# OPTIONS_GHC -Wno-partial-fields #-}

module Syntax.Expr (Expr (..), Parameter (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Text (Text)
import GHC.Generics (Generic)
import Syntax.Operations (BinaryOp)

data Expr
  = Int {valueInt :: Int32}
  | Str {valueStr :: Text}
  | Call {callee :: Expr, arguments :: [Expr]}
  | Binary {lhs :: Expr, op :: BinaryOp, rhs :: Expr}
  | Function {parameters :: [Parameter], value :: Expr}
  | Let {name :: Parameter, value :: Expr, next :: Expr}
  | If {condition :: Expr, thenBody :: Expr, otherwiseBody :: Expr}
  | Print {value :: Expr}
  | First {value :: Expr}
  | Second {value :: Expr}
  | Bool {valueBool :: Bool}
  | Tuple {first :: Expr, second :: Expr}
  | Var {textVar :: Text}
  deriving (Show, Eq, Generic)

newtype Parameter = Parameter {text :: Text}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \label ->
           if
               | "value" `isInfixOf` label -> "value"
               | "then" `isInfixOf` label -> "then"
               | "otherwise" `isInfixOf` label -> "otherwise"
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
