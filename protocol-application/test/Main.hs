module Main
  ( main,
  )
where

import CodecProperties qualified
import FrameProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TypesProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-protocol-application"
        [ TypesProperties.tests,
          CodecProperties.tests,
          FrameProperties.tests
        ]
    )
