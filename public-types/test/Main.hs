module Main
  ( main,
  )
where

import ReceiptRetirementProperties qualified
import SortIdProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TimingProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-public-types"
        [SortIdProperties.tests, TimingProperties.tests, ReceiptRetirementProperties.tests]
    )
