module Test.Syntax (syntaxTests) where

import Data.Aeson
import Data.String.Interpolate.IsString (i)
import Syntax.Expr
import Syntax.Operations
import Syntax.Program
import Test.FromFile (testWithFileContent)
import Test.Tasty
import Test.Tasty.HUnit

syntaxTests :: [TestTree]
syntaxTests =
  [ testCase "Parameter encoding" $
      encode (Parameter "Hi") @?= [i|{"text":"Hi"}|],
    testCase "Int encoding" $
      encode (Int 5) @?= [i|{"kind":"Int","value":5}|],
    testCase "Int decoding" $
      decode [i|{"kind":"Int","value":4,"bla":0}|] @?= Just (Int {valueInt = 4}),
    testCase "Operator decoding" $
      eitherDecode [i|"Gt"|] @?= Right Gt,
    programParsingTest "Fibonacci" "rinha-de-compiler/files/fib.json",
    programParsingTest "Combination" "rinha-de-compiler/files/combination.json",
    programParsingTest "Sum" "rinha-de-compiler/files/sum.json"
  ]

programParsingTest :: TestName -> FilePath -> TestTree
programParsingTest programName path =
  testWithFileContent [i|#{programName} program parsing|] path $
    \code ->
      case (eitherDecode code :: Either String Program) of
        Left err -> assertFailure $ "Could not decode program: " <> err
        Right _ -> mempty
