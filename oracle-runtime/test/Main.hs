module Main (main) where

import AdapterRetentionProperties qualified
import CheckpointCacheProperties qualified
import ClusterProperties qualified
import ConformanceClientProperties qualified
import ConnectedWaitProperties qualified
import Control.Exception (evaluate)
import CoordinationProperties qualified
import FailureRuntimeProperties qualified
import HealthRuntimeProperties qualified
import LeaseQueueProperties qualified
import RaftAdmissionProperties qualified
import RaftRetryProperties qualified
import RecordingProperties qualified
import RetainedRequestProperties qualified
import RuntimeProperties qualified
import SocketProgressProperties qualified
import SocketRetirementProperties qualified
import Test.Tasty (defaultMain, testGroup)
import TestFixtures (fixtureTcpConfiguration)
import VoterRuntimeProperties qualified
import WatchServeProperties qualified
import WatchWorkerProperties qualified
import WorkCountsProperties qualified

main :: IO ()
main = do
  -- The parallel test runner must not be the first evaluator of the shared,
  -- mutually referenced raw and checked genesis fixtures.
  _ <- evaluate fixtureTcpConfiguration
  defaultMain
    ( testGroup
        "oracle-runtime"
        [ AdapterRetentionProperties.tests,
          CheckpointCacheProperties.tests,
          RuntimeProperties.tests,
          HealthRuntimeProperties.tests,
          RecordingProperties.tests,
          CoordinationProperties.tests,
          ConformanceClientProperties.tests,
          ConnectedWaitProperties.tests,
          LeaseQueueProperties.tests,
          RetainedRequestProperties.tests,
          SocketProgressProperties.tests,
          SocketRetirementProperties.tests,
          WatchServeProperties.tests,
          WatchWorkerProperties.tests,
          WorkCountsProperties.tests,
          ClusterProperties.tests,
          RaftAdmissionProperties.tests,
          RaftRetryProperties.tests,
          VoterRuntimeProperties.tests,
          FailureRuntimeProperties.tests
        ]
    )
