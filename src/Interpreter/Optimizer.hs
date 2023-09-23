{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Interpreter.Optimizer (optimize) where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Syntax.Expr as E
import qualified Syntax.OptExpr as O

optimize :: E.Expr -> O.OptExpr
optimize = optimize' M.empty 0

optimize' :: M.Map Text Int -> Int -> E.Expr -> O.OptExpr
optimize' m nextKey expr = case expr of
  --------------------------
  -- Optimizations happening
  E.Let (E.Parameter name) value next ->
    O.Let (O.IntParameter (choose name)) (saveKey name value) (saveKey name next)
  E.Function parameters value ->
    let names = map E.text parameters
        (m', nextKey') = saveManyKeys names
        keys = map (m' M.!) names
     in O.Function (map O.IntParameter keys) (optimize' m' nextKey' value)
  E.Var textVar -> O.Var (m M.! textVar)
  --------------------------
  -- No optimizations
  E.Int val -> O.Int val
  E.Str val -> O.Str val
  -- TODO: mapping continue without sharing `m` changes might be a problem
  E.Call callee arguments -> O.Call (continue callee) (map continue arguments)
  E.Binary lhs op rhs -> O.Binary (continue lhs) op (continue rhs)
  E.If cond thenBody otherwiseBody -> O.If (continue cond) (continue thenBody) (continue otherwiseBody)
  E.Print val -> O.Print (continue val)
  E.First val -> O.First (continue val)
  E.Second val -> O.Second (continue val)
  E.Bool valueBool -> O.Bool valueBool
  -- TODO: continue without sharing `m` might be a problem
  E.Tuple first second -> O.Tuple (continue first) (continue second)
  --------------------------
  where
    continue = optimize' m nextKey
    choose name = case M.lookup name m of
      Just int -> int
      _ -> nextKey
    saveKey name = case M.lookup name m of
      Just _ -> continue
      _ -> optimize' (M.insert name nextKey m) (nextKey + 1)
    saveManyKeys :: [Text] -> (M.Map Text Int, Int)
    saveManyKeys =
      foldr
        ( \name (lastMap, key) -> case M.lookup name lastMap of
            Just _ -> (lastMap, key)
            Nothing -> (M.insert name key lastMap, key + 1)
        )
        (m, nextKey)
