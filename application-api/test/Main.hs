module Main (main) where

import ConnectionProperties qualified
import RuntimeProperties qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "eclips-application-api" [RuntimeProperties.tests, ConnectionProperties.tests])
