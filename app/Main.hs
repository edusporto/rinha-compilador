module Main (main) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Interpreter

main :: IO ()
main = do
  readFromEnv <- try $ BL.readFile "/var/rinha/source.rinha.json"
  case readFromEnv of
    Right contents -> tryToEval contents
    Left (_ :: SomeException) -> BL.getContents >>= tryToEval

tryToEval :: BL.ByteString -> IO ()
tryToEval jsonCode = case interpret jsonCode of
  Left err -> putStrLn ("Error: " <> err)
  Right (_, printed) -> TIO.putStr (T.unlines printed)
