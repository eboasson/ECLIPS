module Main
  ( main,
  )
where

import CodecProperties qualified
import FrameProperties qualified
import NativeTcpProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TypesProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-protocol-raft"
        [ TypesProperties.tests,
          CodecProperties.tests,
          FrameProperties.tests,
          NativeTcpProperties.tests
        ]
    )
