module RuntimeProperties (tests) where

import Control.Concurrent
  ( forkIO,
  )
import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVarIO,
    newTVarIO,
    putTMVar,
    readTVar,
    takeTMVar,
    writeTVar,
  )
import Eclips.Domain.Identity (controlIndex)
import Eclips.Oracle.Runtime
  ( OracleRuntimeExit (OracleRuntimeFailed),
    OracleRuntimeFailure (RuntimeEssentialChildFailed),
    OracleRuntimeRecordingMode (OracleCaptureHistory),
    awaitOracleRuntimeExit,
    configureOracleRuntimeDiagnostics,
    configureOracleRuntimeRecording,
    oracleRuntimeConfiguration,
    oracleRuntimeRecording,
    oracleRuntimeVoterChange,
    runtimeElectionTimeoutSource,
    startOracleRuntime,
    stopOracleRuntime,
  )
import Eclips.Oracle.Voter (mkVoterChangeId)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import TestFixtures
  ( fixtureFirstRaftGenesis,
    fixtureFirstRuntimeConfiguration,
    fixtureHeraldEpoch,
    fixtureInvalidOracleGenesis,
    fixtureInvalidRaftGenesis,
    fixtureOracleGenesis,
  )

tests :: TestTree
tests =
  testGroup
    "runtime"
    [ testCase "both raw genesis values are rejected before runtime activity" caseGenesisValidation,
      testCase "startup child failure is typed and joins siblings" caseStartupFailure,
      testCase "an essential child failure automatically stops the published replica" caseAutomaticFailureStop,
      testCase "concurrent scoped stop is idempotent and post-stop observation terminates" caseConcurrentStop
    ]

caseGenesisValidation :: IO ()
caseGenesisValidation = do
  let unusedEntropy = runtimeElectionTimeoutSource (ioError (userError "entropy must remain unused"))
      invalidRaft =
        oracleRuntimeConfiguration
          fixtureInvalidRaftGenesis
          fixtureOracleGenesis
          fixtureHeraldEpoch
          unusedEntropy
      invalidOracle =
        oracleRuntimeConfiguration
          fixtureFirstRaftGenesis
          fixtureInvalidOracleGenesis
          fixtureHeraldEpoch
          unusedEntropy
  case invalidRaft of
    Left _ -> pure ()
    Right _ -> assertFailure "invalid raw Raft genesis produced a runtime configuration"
  case invalidOracle of
    Left _ -> pure ()
    Right _ -> assertFailure "invalid raw Oracle genesis produced a runtime configuration"

caseStartupFailure :: IO ()
caseStartupFailure = do
  let faulty =
        configureOracleRuntimeDiagnostics
          (const (ioError (userError "injected diagnostic failure")))
          fixtureFirstRuntimeConfiguration
  started <- startOracleRuntime faulty
  case started of
    Left RuntimeEssentialChildFailed {} -> pure ()
    Left other -> assertFailure ("unexpected startup fault: " <> show other)
    Right runtime -> stopOracleRuntime runtime >> assertFailure "faulty recording owner was published"

caseAutomaticFailureStop :: IO ()
caseAutomaticFailureStop = do
  failNow <- newTVarIO False
  let sink _ = do
        failing <- atomically (readTVar failNow)
        if failing
          then ioError (userError "injected post-start diagnostic failure")
          else pure ()
      configured = configureOracleRuntimeDiagnostics sink fixtureFirstRuntimeConfiguration
  started <- startOracleRuntime configured
  runtime <- either (assertFailure . show) pure started
  atomically (writeTVar failNow True)
  observed <- timeout 2000000 (awaitOracleRuntimeExit runtime)
  case observed of
    Just (OracleRuntimeFailed RuntimeEssentialChildFailed {}) -> pure ()
    Just other -> assertFailure ("unexpected terminal result: " <> show other)
    Nothing -> assertFailure "essential child failure did not stop the replica"
  stopOracleRuntime runtime

caseConcurrentStop :: IO ()
caseConcurrentStop = do
  started <- startOracleRuntime (configureOracleRuntimeRecording OracleCaptureHistory fixtureFirstRuntimeConfiguration)
  runtime <- either (assertFailure . show) pure started
  first <- newEmptyTMVarIO
  second <- newEmptyTMVarIO
  _ <- forkIO (stopOracleRuntime runtime >> atomically (putTMVar first ()))
  _ <- forkIO (stopOracleRuntime runtime >> atomically (putTMVar second ()))
  atomically (takeTMVar first)
  atomically (takeTMVar second)
  events <- oracleRuntimeRecording runtime
  assertBool "post-stop recording remains available" (not (null events))
  identifier <- either (assertFailure . show) pure (mkVoterChangeId (controlIndex 1))
  queried <- timeout 2000000 (oracleRuntimeVoterChange runtime identifier)
  assertEqual "post-stop voter query terminates without a live owner" (Just Nothing) queried
