{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Interpreter.Evaluator (eval) where

import Control.Monad.Writer (Writer, tell)
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import Interpreter.Env (Env, extend, lookup)
import Syntax.Expr (Expr (..), Parameter (..))
import Syntax.Operations (BinaryOp (..))
import Syntax.Value (Value (..))
import TextShow

eval :: Env -> Expr -> Writer T.Text Value
eval env expr = case expr of
  Int val -> return $ Num val
  Str val -> return $ String val
  Call callee arguments -> do
    closure <- eval env callee
    args <- mapM (eval env) arguments
    case closure of
      Closure argNames body currEnv ->
        let newEnv = foldr extend currEnv (zip argNames args)
         in eval newEnv body
      _ -> error [i|Can't call "#{closure}"|]
  Binary lhs op rhs -> do
    left <- eval env lhs
    right <- eval env rhs
    return $ treatBinary op left right
  Function parameters value ->
    let argNames = map text parameters
     in return $ Closure argNames value env
  Let (Parameter name) value next -> do
    closure <- eval env value
    case closure of
      Closure argNames body oldEnv ->
        let newClos = Closure argNames body (extend (name, newClos) oldEnv)
         in eval (Syntax.Value.envClos newClos) next
  If condition thenBody elseBody -> do
    condResult <- eval env condition
    case condResult of
      (Boolean b) ->
        if b
          then eval env thenBody
          else eval env elseBody
      _ -> error [i|`if` condition is not a boolean: "#{condResult}"|]
  Print expr -> do
    value <- eval env expr
    tell (showt value)
    return value
  First value -> do
    result <- eval env value
    case result of
      Pair l _ -> return l
      _ -> error [i|this is not a pair: #{result}|]
  Second value -> do
    result <- eval env value
    case result of
      Pair _ r -> return r
      _ -> error [i|this is not a pair: #{result}|]
  Bool val -> return $ Boolean val
  Tuple l r -> do
    left <- eval env l
    right <- eval env r
    return $ Pair left right
  Var name ->
    let result = Interpreter.Env.lookup name env
     in case result of
          Nothing -> error [i|can't find variable #{name}|]
          Just val -> return val

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
