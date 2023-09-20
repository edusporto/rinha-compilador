module Interpreter
  ( someFunc,
    interpret,
  )
where

import Control.Monad.Writer (runWriter)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Interpreter.Env as Env
import Interpreter.Evaluator
import Syntax.Expr
import Syntax.Value

someFunc :: IO ()
someFunc = putStrLn "someFunc"

interpret :: BL.ByteString -> (Value, T.Text)
interpret jsonCode =
  let decoded = eitherDecode jsonCode :: Either String Expr
   in case decoded of
        Left err -> error [i|Couldn't parse code: #{err}|]
        Right ast -> runWriter $ eval Env.empty ast
