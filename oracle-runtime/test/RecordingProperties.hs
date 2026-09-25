module RecordingProperties (tests) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Eclips.Domain.Identity
  ( controlIndex,
    controlIndexWord64,
  )
import Eclips.Oracle.Runtime
  ( OracleRuntimeConfiguration,
    OracleRuntimeEvent (..),
    OracleRuntimeRecordingMode (..),
    configureOracleRuntimeDiagnostics,
    configureOracleRuntimeRecording,
    oracleRuntimeRecording,
    startOracleRuntime,
    stopOracleRuntime,
  )
import Eclips.Protocol.Oracle.Types
  ( controlIndexDto,
    controlIndexDtoWord64,
  )
import Eclips.Protocol.Raft.Types
  ( raftLogIndexDtoFromCore,
    raftLogIndexDtoToCore,
  )
import Eclips.Raft.Identity
  ( raftDurationMicrosWord64,
    raftLogIndex,
  )
import Eclips.Raft.Input
  ( RaftInputView (SelectElectionTimeoutView),
    raftInputView,
  )
import Test.Tasty
  ( TestTree,
    localOption,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    QuickCheckTests (..),
    counterexample,
    ioProperty,
    testProperty,
    (===),
  )
import TestFixtures
  ( fixtureFirstRuntimeConfiguration,
    fixtureRuntimeConfigurationForSample,
  )

tests :: TestTree
tests =
  testGroup
    "recording"
    [ testCase "default recording streams diagnostics without retaining live or final history" caseDiagnosticsOnly,
      testCase "history capture can be disabled without suppressing diagnostics" caseDisableCapture,
      testCase "post-stop snapshots retain typed timeout input and terminal stop" caseFinalRecording,
      localOption
        (QuickCheckTests 20)
        (testProperty "generated entropy samples select the exact inclusive timeout" propTimeoutSelection),
      testProperty "the ERFT log-index DTO preserves runtime positions" propRaftLogIndexDto,
      testProperty "the EORC cursor DTO preserves runtime prefixes" propControlIndexDto
    ]

caseDiagnosticsOnly :: IO ()
caseDiagnosticsOnly = checkDiagnosticsOnly fixtureFirstRuntimeConfiguration

caseDisableCapture :: IO ()
caseDisableCapture = checkDiagnosticsOnly (configureOracleRuntimeRecording OracleDiagnosticsOnly (configureOracleRuntimeRecording OracleCaptureHistory fixtureFirstRuntimeConfiguration))

checkDiagnosticsOnly :: OracleRuntimeConfiguration -> IO ()
checkDiagnosticsOnly configuration = do
  delivered <- newIORef []
  started <- startOracleRuntime (configureOracleRuntimeDiagnostics (\event -> modifyIORef' delivered (event :)) configuration)
  runtime <- either (assertFailure . show) pure started
  live <- oracleRuntimeRecording runtime
  stopOracleRuntime runtime
  final <- oracleRuntimeRecording runtime
  events <- readIORef delivered
  assertEqual "live snapshot does not retain diagnostics" [] live
  assertEqual "post-stop snapshot does not retain diagnostics" [] final
  assertBool "diagnostic callback still sees native activity" (any isRaftInput events)
  assertBool "diagnostic callback still sees terminal stop" (RuntimeReplicaStopped `elem` events)
  where
    isRaftInput RuntimeRaftInputInstalled {} = True
    isRaftInput _ = False

caseFinalRecording :: IO ()
caseFinalRecording = do
  started <- startOracleRuntime (configureOracleRuntimeRecording OracleCaptureHistory fixtureFirstRuntimeConfiguration)
  runtime <- either (assertFailure . show) pure started
  stopOracleRuntime runtime
  events <- oracleRuntimeRecording runtime
  assertBool
    "selected election duration was retained as a typed Raft input"
    (any isRaftInput events)
  assertBool "terminal stop is retained" (RuntimeReplicaStopped `elem` events)
  where
    isRaftInput RuntimeRaftInputInstalled {} = True
    isRaftInput _ = False

propTimeoutSelection :: Word -> Property
propTimeoutSelection generated = ioProperty $ do
  let sample = fromIntegral generated
      expected = 60000 + sample `mod` 60001
  started <- startOracleRuntime (configureOracleRuntimeRecording OracleCaptureHistory (fixtureRuntimeConfigurationForSample sample))
  case started of
    Left failure ->
      pure (counterexample ("runtime failed to start: " <> show failure) False)
    Right runtime -> do
      stopOracleRuntime runtime
      events <- oracleRuntimeRecording runtime
      let selected =
            [ raftDurationMicrosWord64 duration
            | RuntimeRaftInputInstalled input <- events,
              SelectElectionTimeoutView _ duration <- [raftInputView input]
            ]
      pure
        $ counterexample
          ("selected durations were " <> show selected)
          ( case selected of
              first : _ -> first == expected
              [] -> False
          )

propRaftLogIndexDto :: Word -> Property
propRaftLogIndexDto generated =
  let index = raftLogIndex (fromIntegral generated)
   in raftLogIndexDtoToCore (raftLogIndexDtoFromCore index) === index

propControlIndexDto :: Word -> Property
propControlIndexDto generated =
  let index = controlIndex (fromIntegral generated)
   in controlIndexDtoWord64 (controlIndexDto (fromIntegral generated))
        === controlIndexWord64 index
