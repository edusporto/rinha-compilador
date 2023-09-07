{-# OPTIONS_GHC -Wno-partial-fields #-}

module Syntax.Term (Term (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.List (isInfixOf)
import Data.Text (Text)
import GHC.Generics (Generic)
import Syntax.Operations (BinaryOp)

data Term
  = Int {valueInt :: Int}
  | Str {valueStr :: Text}
  | Call {callee :: Term, arguments :: [Term]}
  | Binary {lhs :: Term, op :: BinaryOp, rhs :: Term}
  | Function {parameters :: [Term], value :: Term}
  | Let {name :: Term, value :: Term, next :: Term}
  | If {condition :: Term, thenBody :: Term, elseBody :: Term}
  | Print {value :: Term}
  | First {value :: Term}
  | Second {value :: Term}
  | Bool {value :: Term}
  | Tuple {first :: Term, second :: Term}
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
     ''Term
 )

-- >>> encode $ Int 5
-- "{\"kind\":\"Int\",\"value\":5}"
-- >>> decode "{\"kind\":\"Int\",\"value\":5}" :: Maybe Term
-- Just (Int {valueInt = 5})
-- >>> decode "{\"kind\":\"Int\",\"value\":5, \"bla\": 0}" :: Maybe Term
-- Just (Int {valueInt = 5})
