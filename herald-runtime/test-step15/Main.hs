module Main (main) where

import Control.Exception (evaluate)
import RepeatedRetirementProperties qualified
import Step15ApplicationLossProperties qualified
import Step15Configuration (step15OracleTcpConfiguration)
import Step15CrashProperties qualified
import Step15FoundationProperties qualified
import Step15TerminalRepairProperties qualified
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Runners (NumThreads (NumThreads))

main :: IO ()
main = do
  _ <- evaluate step15OracleTcpConfiguration
  defaultMain
    ( localOption
        (NumThreads 1)
        ( testGroup
            "Step 15 real transport"
            [ RepeatedRetirementProperties.tests,
              Step15ApplicationLossProperties.tests,
              Step15FoundationProperties.tests,
              Step15CrashProperties.tests,
              Step15TerminalRepairProperties.tests
            ]
        )
    )
