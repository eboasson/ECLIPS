module Main
  ( main,
  )
where

import AdministrationRuntimeProperties qualified
import ArbiterProperties qualified
import ConformanceProperties qualified
import ConnectionProperties qualified
import CoordinationProperties qualified
import PeerDialProperties qualified
import RecordingProperties qualified
import RuntimeClosureProperties qualified
import RuntimeHookProperties qualified
import RuntimeProperties qualified
import Step9ComposedProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TraceProperties qualified
import TransferProperties qualified
import VerificationClosureProperties qualified

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-herald-runtime"
        [ AdministrationRuntimeProperties.tests,
          ArbiterProperties.tests,
          ConnectionProperties.tests,
          ConformanceProperties.tests,
          CoordinationProperties.tests,
          PeerDialProperties.tests,
          RecordingProperties.tests,
          RuntimeClosureProperties.tests,
          RuntimeHookProperties.tests,
          RuntimeProperties.tests,
          Step9ComposedProperties.tests,
          TraceProperties.tests,
          TransferProperties.tests,
          VerificationClosureProperties.tests
        ]
    )
