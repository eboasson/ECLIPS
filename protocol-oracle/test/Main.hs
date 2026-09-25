module Main
  ( main,
  )
where

import AdmissionProperties qualified
import CodecProperties qualified
import DisappearanceProperties qualified
import FrameProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TypesProperties qualified
import VoterProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-protocol-oracle"
        [ VoterProperties.tests,
          AdmissionProperties.tests,
          DisappearanceProperties.tests,
          TypesProperties.tests,
          CodecProperties.tests,
          FrameProperties.tests
        ]
    )
