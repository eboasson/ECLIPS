module Main (main) where

import Data.Text.IO qualified as Text
import HelloArguments (readConnectionArguments)
import HelloCommon (failure)
import HelloPublisher (runHelloPublisher)
import System.Environment (getArgs)

main :: IO ()
main = do
  (descriptor, remaining) <- getArgs >>= readConnectionArguments
  if null remaining then runHelloPublisher descriptor >>= Text.putStrLn else failure "unexpected publisher arguments"
