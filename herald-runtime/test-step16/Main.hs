module Main (main) where

import Step16MarkerRepairProperties qualified
import Step16TourProperties qualified
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Runners (NumThreads (NumThreads))

main :: IO ()
main =
  defaultMain
    ( localOption
        (NumThreads 1)
        (testGroup "Step16 real transport" [Step16MarkerRepairProperties.tests, Step16TourProperties.tests])
    )
