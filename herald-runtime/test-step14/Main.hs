module Main (main) where

import ApplicationTerminalTcpProperties qualified
import Control.Exception (evaluate)
import Step12Fixtures (step12RoutedOracleTcpConfiguration)
import Step14AdministrationProperties qualified
import Step14AlignmentLossProperties qualified
import Step14ApplicationReplyLossProperties qualified
import Step14DeferredEndProperties qualified
import Step14LabelProperties qualified
import Step14LifecycleProperties qualified
import Step14PreparationReadinessProperties qualified
import Step14PreparedReleaseProperties qualified
import Step14TargetEndProperties qualified
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Runners (NumThreads (NumThreads))

main :: IO ()
main = do
  -- Keep the mutually referenced raw and checked deployment fixtures out of
  -- Tasty's parallel first-evaluation race.
  _ <- evaluate step12RoutedOracleTcpConfiguration
  defaultMain
    -- Match the certified Cabal test parallelism while retaining a local
    -- option so direct executable runs exercise the same four-way schedule.
    ( localOption
        (NumThreads 4)
        ( testGroup
            "Step 14 real transport"
            [ ApplicationTerminalTcpProperties.tests,
              Step14LabelProperties.tests,
              Step14LifecycleProperties.tests,
              Step14AdministrationProperties.tests,
              Step14AlignmentLossProperties.tests,
              Step14TargetEndProperties.tests,
              Step14DeferredEndProperties.tests,
              Step14PreparedReleaseProperties.tests,
              Step14PreparationReadinessProperties.tests,
              Step14ApplicationReplyLossProperties.tests
            ]
        )
    )
