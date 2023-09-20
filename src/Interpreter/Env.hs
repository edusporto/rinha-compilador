{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter.Env
  ( Env (..),
    Binding,
    extend,
    Interpreter.Env.lookup,
    empty,
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import {-# SOURCE #-} Syntax.Value (Value)

newtype Env = Env {getMap :: M.Map Text Value}
  deriving (Show, Eq)

type Binding = (Text, Value)

extend :: Binding -> Env -> Env
extend (name, value) (Env map) = Env (M.insert name value map)

lookup :: Text -> Env -> Maybe Value
lookup name (Env map) = M.lookup name map

empty :: Env
empty = Env M.empty
