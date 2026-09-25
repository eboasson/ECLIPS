module Main (main) where

import Eclips.TypeBoundaries (mainWithArguments)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mainWithArguments
