module Main (main) where

import Control.Exception (evaluate)
import OracleHealthTcpProperties qualified
import OracleReplicaWaitProperties qualified
import OracleSubmissionTrackerProperties qualified
import Step12ControlProperties qualified
import Step12Fixtures (step12OracleTcpConfiguration)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  -- The parallel test runner must not be the first evaluator of the shared,
  -- mutually referenced raw and checked genesis fixtures.
  _ <- evaluate step12OracleTcpConfiguration
  defaultMain
    ( testGroup
        "Step 13 Increment 1"
        [ OracleSubmissionTrackerProperties.tests,
          OracleHealthTcpProperties.tests,
          OracleReplicaWaitProperties.tests,
          Step12ControlProperties.tests
        ]
    )
