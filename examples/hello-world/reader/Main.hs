module Main (main) where

import Data.Text.IO qualified as Text
import HelloArguments (readConnectionArguments)
import HelloCommon (failure)
import HelloReader (runHelloReader)
import System.Environment (getArgs)

main :: IO ()
main = do
  (descriptor, remaining) <- getArgs >>= readConnectionArguments
  if null remaining then runHelloReader descriptor >>= Text.putStrLn else failure "unexpected reader arguments"
