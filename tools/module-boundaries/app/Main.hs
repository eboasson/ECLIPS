module Main (main) where

import Eclips.ModuleBoundaries (checkProject, renderViolations)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  arguments <- getArgs
  projectRoot <- case arguments of
    [root] -> pure root
    _ -> die "usage: eclips-module-boundaries PROJECT_ROOT"
  violations <- checkProject projectRoot
  case violations of
    [] -> pure ()
    _ -> die (renderViolations violations)
