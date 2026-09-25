module Main
  ( main,
  )
where

import FrameProperties qualified
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain FrameProperties.tests
