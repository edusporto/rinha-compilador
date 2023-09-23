{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter.Env
  ( Env (..),
    Binding,
    extend,
    Interpreter.Env.lookup,
    empty,
  )
where

import qualified Data.IntMap as M
import {-# SOURCE #-} Syntax.Value (Value)

newtype Env = Env {getMap :: M.IntMap Value}
  deriving (Show, Eq)

type Binding = (Int, Value)

extend :: Binding -> Env -> Env
extend (key, value) (Env map) = Env (M.insert key value map)

lookup :: Int -> Env -> Maybe Value
lookup key (Env map) = M.lookup key map

empty :: Env
empty = Env M.empty
