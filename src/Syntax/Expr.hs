{-# OPTIONS_GHC -Wno-partial-fields #-}

module Syntax.Expr (Expr (..)) where

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
  | Function {parameters :: [Expr], value :: Expr}
  | Let {name :: Expr, value :: Expr, next :: Expr}
  | If {condition :: Expr, thenBody :: Expr, elseBody :: Expr}
  | Print {value :: Expr}
  | First {value :: Expr}
  | Second {value :: Expr}
  | Bool {valueBool :: Bool}
  | Tuple {first :: Expr, second :: Expr}
  | Var {text :: Text}
  deriving (Show, Eq, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \cons ->
           if
               | "value" `isInfixOf` cons -> "value"
               | "then" `isInfixOf` cons -> "then"
               | "else" `isInfixOf` cons -> "else"
               | otherwise -> cons,
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
