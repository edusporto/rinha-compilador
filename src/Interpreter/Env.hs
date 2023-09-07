{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter.Env
  ( Env (..),
    Binding (..),
    extend,
    Interpreter.Env.lookup,
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import Syntax.Value (Value)

newtype Env = Env {getMap :: M.Map Text Value}

data Binding = Binding {name :: Text, value :: Value}

extend :: Binding -> Env -> Env
extend (Binding name value) (Env map) = Env (M.insert name value map)

lookup :: Text -> Env -> Maybe Value
lookup name (Env map) = M.lookup name map
