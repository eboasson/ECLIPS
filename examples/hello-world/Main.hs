module Main (main) where

import Data.Text.IO qualified as Text
import FounderDeployment (withFounderDeployment)
import HelloApplication (runFounderApplication)
import System.Environment (getArgs)

main :: IO ()
main = do
  arguments <- getArgs
  greeting <- case arguments of
    ["--bootstrap"] -> withFounderDeployment runFounderApplication
    _ -> ioError (userError "usage: eclips-hello-world --bootstrap (or use eclips-hello-launcher with a deployment descriptor)")
  Text.putStrLn greeting
