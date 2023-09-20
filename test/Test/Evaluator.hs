module Test.Evaluator where

import Data.Aeson
import Data.String.Interpolate.IsString (i)
import Syntax.Expr
import Syntax.Operations
import Syntax.Program
import Test.FromFile (testWithFileContent)
import Test.Tasty
import Test.Tasty.HUnit

evalTests :: [TestTree]
evalTests =
  []

-- TODO
programEvaluationTest :: TestName -> FilePath -> Value -> TestTree
programEvaluationTest = undefined
