{-# LANGUAGE OverloadedStrings #-}

module OracleReplicaWaitProperties (tests) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (finally)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Eclips.Domain.Identity (controlIndex)
import Eclips.Herald.OracleClient qualified as Client
import Eclips.Herald.Runtime
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission (..), submitOracleClientIngress)
import Eclips.Herald.Runtime.Internal.Owner (startHeraldRuntime)
import Eclips.Herald.Runtime.Internal.Types qualified as Internal
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (HeraldRuntimeScopeClosed))
import Eclips.Oracle.Canonical (canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Command qualified as Command
import Eclips.Oracle.Effect qualified as Effect
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.State qualified as Oracle
import Eclips.Oracle.Transition qualified as Oracle
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Identity (RaftNodeId, mkRaftNodeId, raftNodeIdBytes)
import RuntimeFixtures (fixtureApplicationRecoveryConfiguration, fixturePeerRecoveryConfiguration)
import Step12Fixtures
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "owner-published Oracle replica registration"
    [ testCase "preexisting genesis registration needs no queued status query" preexisting,
      testCase "shutdown releases an unsatisfied registration wait" shutdown,
      testCase "a committed registration wakes its selected waiter" committed
    ]

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

initialOracle :: Oracle.OracleState
initialOracle = checked (Oracle.initialOracle step12CheckedOracleGenesis)

contacts :: Client.OracleContactSet
contacts = checked (Client.oracleContactSet (NE.singleton contact))
  where
    contact = checked (Client.oracleContact (checked (Client.oracleNodeClaim (raftNodeIdBytes step12InitialLeaderNode))) "127.0.0.1" 14001)

configuration :: HeraldRuntimeConfiguration
configuration =
  configureHeraldRuntimeOracleVoters (Voter.oracleVoterConfiguration initialOracle) (Voter.oracleReplicaRegistrations initialOracle)
    $ heraldRuntimeConfiguration
      step12H1Genesis
      step12H1Bootstraps
      contacts
      step12H1GeneratorSeedSource
      systemRuntimeMonotonicClock
      (runtimePeerWorkDelayMicroseconds 0)
      (heraldRuntimeHandlers (const (pure ())))
      fixtureApplicationRecoveryConfiguration
      fixturePeerRecoveryConfiguration

newNode :: RaftNodeId
newNode = checked (mkRaftNodeId (BS.replicate 32 0xf1))

within :: String -> IO value -> IO value
within name action = timeout 2000000 action >>= maybe (assertFailure (name <> " timed out")) pure

preexisting :: Assertion
preexisting = do
  let expected = case Voter.oracleReplicaRegistrations initialOracle of value : _ -> value; [] -> error "no bootstrap registration"
  outcome <- withHeraldRuntime configuration $ \runtime -> do
    actual <- within "preexisting registration" (awaitHeraldOracleReplicaRegistration runtime (Voter.replicaRegistrationNode expected))
    actual @?= Right expected
    Voter.replicaRegistrationControlIndex expected @?= controlIndex 0
  outcome @?= Right ((), HeraldRuntimeScopeClosed)

shutdown :: Assertion
shutdown = do
  runtime <- within "runtime startup" (startHeraldRuntime configuration) >>= either (assertFailure . show) pure
  result <- newEmptyMVar
  waiter <- forkFinally (awaitHeraldOracleReplicaRegistration runtime newNode) (putMVar result)
  ( do
      Internal.runtimeCloseScope runtime
      observed <- within "registration waiter shutdown" (takeMVar result)
      case observed of
        Right value -> value @?= Left (HeraldOracleQueryNotSubmitted RuntimeStopped)
        Left exception -> assertFailure (show exception)
    )
    `finally` (killThread waiter >> Internal.runtimeCloseScope runtime)

committed :: Assertion
committed = do
  actions <- newTQueueIO
  let configured = configureHeraldRuntimeOracleActionSink (atomically . writeTQueue actions) configuration
      endpoint = checked (Voter.oracleReplicaEndpoint "127.0.0.1" 14002)
      registrationContact = Voter.oracleReplicaContact endpoint endpoint endpoint
      envelope = Command.oracleEnvelope (oracleClientRequestId step12H1HeraldEpoch 1) Nothing step12H1HeraldEpoch (Command.registerOracleReplicaCommand newNode step12H4HeraldEpoch registrationContact)
      (registered, _, batch) = checked (Oracle.stepOracle envelope initialOracle)
      entry = case [value | Effect.EmitAppliedOracleEntry value <- Effect.oracleEffects batch] of [value] -> canonicalizeAppliedOracleEntry value; _ -> error "registration emitted no exact entry"
      expected = case filter ((== newNode) . Voter.replicaRegistrationNode) (Voter.oracleReplicaRegistrations registered) of [value] -> value; _ -> error "registration was not accepted"
      awaitAction :: (Client.OracleClientAction -> Maybe value) -> IO value
      awaitAction select = do
        action <- atomically (readTQueue actions)
        maybe (awaitAction select) pure (select action)
  outcome <- withHeraldRuntime configured $ \runtime -> do
    attempt <- within "initial Hello action" (awaitAction (\case Client.ConnectAndHelloOracle value _ -> Just value; _ -> Nothing))
    let node = Client.oracleContactNode (Client.oracleConnectAttemptContact attempt)
    submittedHello <- submitOracleClientIngress runtime (Client.OracleHelloReceived attempt (Client.oracleHelloAcceptance node (Client.oracleObservedTerm 1) (controlIndex 0) (Just node) True))
    submittedHello @?= Queued
    binding <- within "established Watch action" (awaitAction (\case Client.WatchOracle value _ -> Just value; _ -> Nothing))
    result <- newEmptyMVar
    waiter <- forkFinally (awaitHeraldOracleReplicaRegistration runtime newNode) (putMVar result)
    ( do
        submitted <- submitOracleClientIngress runtime (Client.OracleEntriesReceived binding (NE.singleton entry))
        submitted @?= Queued
        observed <- within "committed registration notification" (takeMVar result)
        case observed of
          Right value -> value @?= Right expected
          Left exception -> assertFailure (show exception)
      )
      `finally` killThread waiter
  outcome @?= Right ((), HeraldRuntimeScopeClosed)
