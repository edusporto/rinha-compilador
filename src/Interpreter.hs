module Interpreter (interpret) where

import Control.Monad.Writer (runWriter)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Interpreter.Env as Env
import Interpreter.Evaluator
-- import Interpreter.Optimizer (optimize)
import Syntax.Program
import Syntax.Value

interpret :: BL.ByteString -> Either String (Value, [T.Text])
interpret jsonCode =
  fmap
    ( \(Program _ ast) ->
        -- let optAst = optimize ast in
        runWriter (eval Env.empty ast)
    )
    (eitherDecode jsonCode)
