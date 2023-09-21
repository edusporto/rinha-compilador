module Interpreter (interpret) where

import Control.Monad.Writer (runWriter)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Interpreter.Env as Env
import Interpreter.Evaluator
import Syntax.Program
import Syntax.Value

interpret :: BL.ByteString -> Either String (Value, [T.Text])
interpret jsonCode =
  fmap (\(Program _ ast) -> runWriter (eval Env.empty ast)) (eitherDecode jsonCode)
