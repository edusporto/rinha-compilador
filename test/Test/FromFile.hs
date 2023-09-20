module Test.FromFile (testWithFileContent) where

import qualified Data.ByteString.Lazy as BL
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

type HandleWithContent = (Handle, BL.ByteString)

acquireFile :: FilePath -> IOMode -> IO HandleWithContent
acquireFile path mode =
  let handleIO = openFile path mode
   in do
        handle <- handleIO
        fileText <- BL.hGetContents handle
        pure (handle, fileText)

releaseFile :: HandleWithContent -> IO ()
releaseFile (handle, _) = hClose handle

testWithFileContent :: TestName -> FilePath -> (BL.ByteString -> Assertion) -> TestTree
testWithFileContent testName path test =
  withResource
    (acquireFile path ReadMode)
    releaseFile
    (\getResource -> testCase testName $ getResource >>= \(_, content) -> test content)
