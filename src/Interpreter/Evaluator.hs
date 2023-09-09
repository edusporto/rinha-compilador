{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Interpreter.Evaluator (eval) where

import Data.String.Interpolate.IsString (i)
import Interpreter.Env (Env, extend, lookup)
import Syntax.Expr (Expr (..), Parameter (..))
import Syntax.Operations (BinaryOp (..))
import Syntax.Value (Value (..))

eval :: Env -> Expr -> Value
eval env expr = case expr of
  Int val -> Num val
  Str val -> String val
  Call callee arguments ->
    let closure = eval env callee
        args = map (eval env) arguments
     in case closure of
          Closure argNames body currEnv ->
            let newEnv = foldr extend currEnv (zip argNames args)
             in eval newEnv body
          _ -> error [i|Can't call "#{closure}"|]
  Binary lhs op rhs -> treatBinary op (eval env lhs) (eval env rhs)
  Function parameters value ->
    let argNames = map text parameters
     in Closure argNames value env
  Let (Parameter name) value next ->
    let closure = eval env value
     in case closure of
          Closure argNames body oldEnv ->
            let newClos = Closure argNames body (extend (name, newClos) oldEnv)
             in eval (Syntax.Value.envClos newClos) next
  If condition thenBody elseBody ->
    let condResult = eval env condition
     in case condResult of
          (Boolean b) ->
            if b
              then eval env thenBody
              else eval env elseBody
          _ -> error [i|`if` condition is not a boolean: "#{condResult}"|]
  Print value -> error "TODO: deal with Haskell's laziness :/"
  First value ->
    let result = eval env value
     in case result of
          Pair l _ -> l
          _ -> error [i|this is not a pair: #{result}|]
  Second value ->
    let result = eval env value
     in case result of
          Pair _ r -> r
          _ -> error [i|this is not a pair: #{result}|]
  Bool val -> Boolean val
  Tuple l r -> Pair (eval env l) (eval env r)
  Var name ->
    let result = Interpreter.Env.lookup name env
     in case result of
          Nothing -> error [i|can't find variable #{name}|]
          Just val -> val

treatBinary :: BinaryOp -> Value -> Value -> Value
treatBinary Add (Num l) (Num r) = Num (l + r)
treatBinary Mul (Num l) (Num r) = Num (l - r)
treatBinary Div (Num l) (Num r) = Num (l `div` r)
treatBinary Rem (Num l) (Num r) = Num (l `mod` r)
treatBinary Eq (Num l) (Num r) = Boolean (l == r)
treatBinary Neq (Num l) (Num r) = Boolean (l /= r)
treatBinary Lt (Num l) (Num r) = Boolean (l < r)
treatBinary Gt (Num l) (Num r) = Boolean (l > r)
treatBinary Lte (Num l) (Num r) = Boolean (l <= r)
treatBinary Gte (Num l) (Num r) = Boolean (l >= r)
treatBinary And (Boolean l) (Boolean r) = Boolean (l && r)
treatBinary Or (Boolean l) (Boolean r) = Boolean (l || r)
treatBinary op l r = error [i|Invalid op "#{op}" for "#{l}" and "#{r}"|]
