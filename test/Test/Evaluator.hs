module Test.Evaluator (evalTests) where

import Control.Monad.Writer (runWriter)
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import Interpreter (interpret)
import qualified Interpreter.Env as Env
import Interpreter.Evaluator
import Syntax.Expr
import Syntax.Operations
import Syntax.Value
import Test.FromFile (testWithFileContent)
import Test.Tasty
import Test.Tasty.HUnit

evalTests :: [TestTree]
evalTests =
  [ testCase "Let with number" $
      run (Let (Parameter "x") (Int 5) (Var "x")) @?= (Num 5, []),
    testCase "Multiple prints" $
      run
        (Let (Parameter "_") (Print (Str "hi")) (Print (Str "there")))
        @?= (String "there", ["hi", "there"]),
    testCase "String equality" $
      run (Binary (Str "hi") Eq (Str "hi")) @?= (Boolean True, []),
    testCase "Binary inequality" $
      run (Binary (Bool True) Neq (Bool False)) @?= (Boolean True, []),
    programEvaluationTest
      "Fibonacci"
      "rinha-de-compiler/files/fib.json"
      (Num 55, ["55"]),
    programEvaluationTest
      "Combination"
      "rinha-de-compiler/files/combination.json"
      (Num 45, ["45"]),
    programEvaluationTest
      "Sum"
      "rinha-de-compiler/files/sum.json"
      (Num 15, ["15"])
  ]

run :: Expr -> (Value, [T.Text])
run expr = runWriter (eval Env.empty expr)

programEvaluationTest :: TestName -> FilePath -> (Value, [T.Text]) -> TestTree
programEvaluationTest programName path expected =
  testWithFileContent [i|#{programName} program evaluation|] path $
    \code -> case interpret code of
      Left err -> error ("Couldn't execute: " <> err)
      Right result -> assertEqual "Correct evaluation" result expected
