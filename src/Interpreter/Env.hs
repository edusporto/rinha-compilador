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
extend (key, value) (Env map) = Env (M.insert key value map)
{-# INLINE extend #-}

lookup :: Text -> Env -> Maybe Value
lookup key (Env map) = M.lookup key map
{-# INLINE lookup #-}

empty :: Env
empty = Env M.empty
{-# INLINE empty #-}
