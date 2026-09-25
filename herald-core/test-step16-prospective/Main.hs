module Main
  ( main,
  )
where

import Step16DisappearanceProperties qualified
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain Step16DisappearanceProperties.tests
