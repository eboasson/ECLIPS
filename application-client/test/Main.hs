module Main
  ( main,
  )
where

import ClientProperties qualified
import LifecycleProperties qualified
import ReceiptRetirementProperties qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "eclips-application-client" [ClientProperties.tests, LifecycleProperties.tests, ReceiptRetirementProperties.tests])
