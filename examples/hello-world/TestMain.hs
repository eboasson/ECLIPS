{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Text (Text)
import FounderDeployment (withFounderDeployment)
import FounderProperties (propFounderConfiguration)
import HelloApplication (runFounderApplication)
import PreparedChildProperties (runPreparedChildProcess, runPreparedChildTour)
import System.Environment (getArgs)
import System.Timeout (timeout)
import Test.QuickCheck (isSuccess, quickCheckResult)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["--prepared-child", path] -> runPreparedChildProcess path
    [] -> runIntegration
    _ -> ioError (userError "unexpected integration test arguments")

runIntegration :: IO ()
runIntegration = do
  propertyResult <- quickCheckResult propFounderConfiguration
  unless (isSuccess propertyResult) (ioError (userError "founder configuration property failed"))
  checkGreeting "fresh singleton founder" (withFounderDeployment runFounderApplication)
  putStrLn "running prepared-child OS tour"
  runPreparedChildTour

checkGreeting :: String -> IO Text -> IO ()
checkGreeting label run = do
  putStrLn ("running " <> label)
  observed <- timeout 60_000_000 run
  case observed of
    Just "Hello, world!" -> pure ()
    Just other -> ioError (userError ("unexpected hello-world value: " <> show other))
    Nothing -> ioError (userError (label <> " hello-world integration exceeded its outer deadlock watchdog"))
