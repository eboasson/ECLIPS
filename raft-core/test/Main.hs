module Main
  ( main,
  )
where

import CheckpointProperties qualified
import ConfigurationProperties qualified
import GenesisProperties qualified
import LogWorkProperties qualified
import RaftModelProperties qualified
import ReconfigurationModelProperties qualified
import SmallClusterProperties qualified
import System.Environment (getArgs)
import Test.Tasty (defaultMain, testGroup)
import Text.Read (readMaybe)
import TransitionProperties qualified

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--raft-allocation-probe", count] | Just applications <- readMaybe count -> LogWorkProperties.runAllocationProbe applications
    _ -> runTests

runTests :: IO ()
runTests =
  defaultMain
    ( testGroup
        "eclips-raft-core"
        [ GenesisProperties.tests,
          ConfigurationProperties.tests,
          CheckpointProperties.tests,
          LogWorkProperties.tests,
          RaftModelProperties.tests,
          ReconfigurationModelProperties.tests,
          SmallClusterProperties.tests,
          TransitionProperties.tests
        ]
    )
