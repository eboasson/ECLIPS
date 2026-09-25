{-# LANGUAGE OverloadedStrings #-}

module RaftAdmissionProperties (tests) where

import Control.Concurrent.MVar (modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString qualified as Bytes
import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.Startup (HeraldMember (heraldMemberEpoch))
import Eclips.Oracle.Command qualified as Command
import Eclips.Oracle.Genesis (checkedOracleRaftConfigurationDigest)
import Eclips.Oracle.Genesis qualified as Genesis
import Eclips.Oracle.Identity (oracleClientRequestId, raftConfigurationDigestBytes)
import Eclips.Oracle.Runtime (OracleRuntimeEvent (RuntimeRaftInputInstalled), OracleRuntimeExit (OracleRuntimeFailed), OracleRuntimeFailure (RuntimeEssentialChildFailed), configureOracleRuntimeReplicaResolver)
import Eclips.Oracle.Runtime.ConformanceClient
import Eclips.Oracle.Runtime.Internal.ReplicaRegistration
import Eclips.Oracle.Runtime.Internal.TCP.Socket
import Eclips.Oracle.Runtime.TCP
import Eclips.Oracle.Transition qualified as Oracle
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Raft.Frame
import Eclips.Protocol.Raft.Types
import Eclips.Raft.Identity (RaftNodeId, mkRaftNodeId)
import Eclips.Raft.Input (RaftInputView (ObserveRequestView), raftInputView)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestFixtures (awaitReadyLeader, fixtureInitialLeaderNode, fixtureSingletonCheckedGenesis, fixtureSingletonTcpConfiguration)
import TestFixtures qualified as F

tests :: TestTree
tests =
  testGroup
    "registered ERFT lane admission"
    [ testCase "a registered non-genesis peer reaches the native owner without receiving a vote" caseRegisteredPeer,
      testCase "replica contacts cannot replace local bindings or use an unresolved port" caseRegistryShape,
      testCase "late bootstrap cannot retain a conflicting earlier-index registration" caseEarlierRegistrationConflict,
      testCase "genesis placeholders defer only their matching contact enrichment" caseGenesisEnrichment,
      testCase "a late contradictory discovery response fails the supervised role" caseLateDiscoveryFailure
    ]

-- Every supplied record comes from an admitted pure Oracle history; no checked
-- registration constructor is exposed or forged by these admission properties.
registrationFacts :: (Voter.OracleReplicaRegistration, Voter.OracleReplicaRegistration, Voter.OracleReplicaRegistration, Voter.OracleReplicaRegistration)
registrationFacts =
  let must :: (Show problem) => Either problem value -> value
      must = either (error . show) id
      initial = must (Oracle.initialOracle (must (Genesis.checkOracleGenesis F.fixtureVoterChangeOracleGenesis)))
      endpoint = must (Voter.oracleReplicaEndpoint "127.0.0.1" 14001)
      contact = Voter.oracleReplicaContact endpoint endpoint endpoint
      home = F.fixtureHeraldEpoch
      envelope sequenceNumber command = Command.oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home command
      step command state = case must (Oracle.stepOracle command state) of (successor, _, _) -> successor
      registration node state = case filter ((== node) . Voter.replicaRegistrationNode) (Voter.oracleReplicaRegistrations state) of
        [value] -> value
        _ -> error "expected admitted registration fixture"
      enriched = step (envelope 1 (Command.registerOracleReplicaCommand fixtureInitialLeaderNode home contact)) initial
      targetHost = heraldMemberEpoch (F.fixtureMembers !! 1)
      register sequenceNumber = envelope sequenceNumber (Command.registerOracleReplicaCommand extraNode targetHost contact)
      early = step (register 1) initial
      late = step (register 2) enriched
   in (registration fixtureInitialLeaderNode initial, registration fixtureInitialLeaderNode enriched, registration extraNode early, registration extraNode late)

caseEarlierRegistrationConflict :: Assertion
caseEarlierRegistrationConflict = do
  let (_, _, early, late) = registrationFacts
  Voter.replicaRegistrationControlIndex early @?= controlIndex 1
  Voter.replicaRegistrationControlIndex late @?= controlIndex 2
  compareReplicaRegistration late (Just early) @?= ReplicaRegistrationContradiction
  compareReplicaRegistration early (Just late) @?= ReplicaRegistrationContradiction
  compareReplicaRegistration late (Just late) @?= ReplicaRegistrationMatched
  compareReplicaRegistration late Nothing @?= ReplicaRegistrationPending

caseGenesisEnrichment :: Assertion
caseGenesisEnrichment = do
  let (genesis, enriched, unrelated, _) = registrationFacts
  compareReplicaRegistration enriched (Just genesis) @?= ReplicaRegistrationPending
  compareReplicaRegistration enriched (Just enriched) @?= ReplicaRegistrationMatched
  compareReplicaRegistration unrelated (Just genesis) @?= ReplicaRegistrationContradiction

caseLateDiscoveryFailure :: Assertion
caseLateDiscoveryFailure = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  first <- newMVar True
  let (_, _, early, late) = registrationFacts
      resolve node
        | node /= extraNode = pure Nothing
        | otherwise = do
            firstRequest <- modifyMVar first (\previous -> pure (False, previous))
            if firstRequest then putMVar entered () >> takeMVar release >> pure (Just late) else pure (Just early)
  configuration <- checked (oracleTcpClusterConfiguration [configureOracleRuntimeReplicaResolver resolve F.fixtureVoterChangeRuntimeConfiguration])
  completed <- timeout 5_000_000 $ withOracleTcpCluster configuration $ \cluster -> do
    _ <- awaitReadyLeader cluster
    endpoint <- case oracleTcpRaftEndpoints cluster of [(_, value)] -> pure value; _ -> assertFailure "expected one founder"
    genesis <- checked (Genesis.checkOracleGenesis F.fixtureVoterChangeOracleGenesis)
    local <- checked (raftNodeIdDtoFromCore fixtureInitialLeaderNode)
    remote <- checked (raftNodeIdDtoFromCore extraNode)
    digest <- checked (raftGenesisDigestDto (raftConfigurationDigestBytes (checkedOracleRaftConfigurationDigest genesis)))
    hello <- checked (raftHelloClaim digest remote local)
    context <- checked (raftConnectionContext digest remote local)
    let connect = connectTcpEndpoint (ResolvedTcpEndpoint (oracleTcpEndpointHost endpoint) (oracleTcpEndpointPort endpoint))
    bracket connect closeSocketQuietly $ \delayed -> do
      sendSocketBytes delayed (encodeRaftFrame (RaftHello hello))
      takeMVar entered
      client <- newConformanceClient genesis F.fixtureHeraldId F.fixtureHeraldEpoch cluster >>= checked
      contact <- maybe (assertFailure "early registration has no contact") pure (Voter.replicaRegistrationContact early)
      request <- prepareConformanceRequest client Nothing (Command.registerOracleReplicaCommand extraNode (Voter.replicaRegistrationHost early) contact)
      _ <- submitConformanceRequest client request
      -- A second valid ERFT request traverses the owner and dispatcher after
      -- the registration's applied receipt. Its response proves the queued
      -- committed registry update has run before releasing the stale lookup.
      bracket connect closeSocketQuietly $ \established -> do
        sendSocketBytes established (encodeRaftFrame (RaftHello hello))
        helloBytes <- receiveRawFrame established
        decoder <- case feedRaftFrame (initialRaftFrameDecoder context) helloBytes of
          RaftFrameFeedResult [RaftHello _] (NeedRaftFrameBytes value) -> pure value
          other -> assertFailure ("second registration lane failed: " <> show other)
        vote <- checked (requestVoteDto (raftTermDto 1) remote (raftLogIndexDto 0) (raftTermDto 0))
        sendSocketBytes established (encodeRaftFrame (RaftRpc vote))
        response <- receiveRawFrame established
        case feedRaftFrame decoder response of
          RaftFrameFeedResult [RaftRpc _] (NeedRaftFrameBytes _) -> pure ()
          other -> assertFailure ("native dispatcher barrier failed: " <> show other)
        putMVar release ()
        exited <- awaitOracleTcpNodeExit cluster fixtureInitialLeaderNode
        case exited of
          Just (OracleRuntimeFailed (RuntimeEssentialChildFailed "raft-registration" _)) -> pure ()
          other -> assertFailure ("contradiction failed only its connection: " <> show other)
  case completed of
    Just (Right ()) -> pure ()
    other -> assertFailure ("late discovery invariant schedule failed: " <> show other)

caseRegistryShape :: Assertion
caseRegistryShape = do
  contact <- checked (oracleTcpListenEndpoint "127.0.0.1" 12345)
  unresolved <- checked (oracleTcpListenEndpoint "127.0.0.1" 0)
  assertRejected OracleTcpConfigurationReplicaContactConflict (configureOracleTcpReplicaContacts [(fixtureInitialLeaderNode, contact)] fixtureSingletonTcpConfiguration)
  assertRejected OracleTcpConfigurationReplicaContactConflict (configureOracleTcpReplicaContacts [(extraNode, contact), (extraNode, contact)] fixtureSingletonTcpConfiguration)
  assertRejected OracleTcpConfigurationReplicaContactPortZero (configureOracleTcpReplicaContacts [(extraNode, unresolved)] fixtureSingletonTcpConfiguration)
  where
    assertRejected expected = \case
      Left actual -> assertEqual "checked transport registry" expected actual
      Right _ -> assertFailure "invalid replica registry admitted"

caseRegisteredPeer :: Assertion
caseRegisteredPeer = do
  -- The extra node is lower than the local node, so it owns the ordinary
  -- deterministic initiation side. This locator is a contact hint only.
  contact <- checked (oracleTcpListenEndpoint "127.0.0.1" 12345)
  configuration <- checked (configureOracleTcpReplicaContacts [(extraNode, contact)] F.fixtureRecordedSingletonTcpConfiguration)
  result <- timeout 5_000_000 $ withOracleTcpCluster configuration $ \cluster -> do
    _ <- awaitReadyLeader cluster
    endpoint <- case oracleTcpRaftEndpoints cluster of
      [(_, value)] -> pure value
      _ -> assertFailure "singleton listener fixture changed"
    local <- checked (raftNodeIdDtoFromCore fixtureInitialLeaderNode)
    remote <- checked (raftNodeIdDtoFromCore extraNode)
    digest <- checked (raftGenesisDigestDto (raftConfigurationDigestBytes (checkedOracleRaftConfigurationDigest fixtureSingletonCheckedGenesis)))
    hello <- checked (raftHelloClaim digest remote local)
    context <- checked (raftConnectionContext digest remote local)
    wrongDigest <- checked (raftGenesisDigestDto (Bytes.replicate 32 255))
    unknown <- checked (raftNodeIdDto (Bytes.replicate 32 1))
    wrongRun <- checked (raftHelloClaim wrongDigest remote local)
    unknownPeer <- checked (raftHelloClaim digest unknown local)
    forM_ [wrongRun, unknownPeer] $ \rejected ->
      bracket (connectTcpEndpoint (ResolvedTcpEndpoint (oracleTcpEndpointHost endpoint) (oracleTcpEndpointPort endpoint))) closeSocketQuietly $ \connection -> do
        sendSocketBytes connection (encodeRaftFrame (RaftHello rejected))
        received <- receiveSocketBytes connection 1
        assertEqual "wrong run or unregistered source never establishes a lane" Bytes.empty received
    -- An already-elected singleton has a newer log; transport identity alone
    -- cannot make this stale candidate eligible for a native vote.
    request <- checked (requestVoteDto (raftTermDto 1) remote (raftLogIndexDto 0) (raftTermDto 0))
    bracket (connectTcpEndpoint (ResolvedTcpEndpoint (oracleTcpEndpointHost endpoint) (oracleTcpEndpointPort endpoint))) closeSocketQuietly $ \connection -> do
      sendSocketBytes connection (encodeRaftFrame (RaftHello hello))
      helloBytes <- receiveRawFrame connection
      decoder <- case feedRaftFrame (initialRaftFrameDecoder context) helloBytes of
        RaftFrameFeedResult [RaftHello admitted] (NeedRaftFrameBytes successor) -> do
          assertEqual "reply retains immutable genesis" digest (raftHelloGenesisDigest admitted)
          pure successor
        other -> assertFailure ("registered non-genesis Hello rejected: " <> show other)
      sendSocketBytes connection (encodeRaftFrame (RaftRpc request))
      responseBytes <- receiveRawFrame connection
      case feedRaftFrame decoder responseBytes of
        RaftFrameFeedResult [RaftRpc response] (NeedRaftFrameBytes _) ->
          assertEqual "native log freshness denies the vote" (Just (local, False)) ((\(_, source, granted) -> (source, granted)) <$> raftRequestVoteResponseFields response)
        other -> assertFailure ("registered request failed before native response: " <> show other)
      recording <- concatMap snd <$> oracleTcpNodeRecordings cluster
      assertBool "exact registered source reaches the private Raft owner" (any observedExtra recording)
  case result of
    Just (Right ()) -> pure ()
    other -> assertFailure ("registered ERFT schedule failed: " <> show other)
  where
    observedExtra (RuntimeRaftInputInstalled input) = case raftInputView input of
      ObserveRequestView source _ -> source == extraNode
      _ -> False
    observedExtra _ = False

extraNode :: RaftNodeId
extraNode = either (error . show) id (mkRaftNodeId (Bytes.replicate 32 0))
checked :: (Show problem) => Either problem value -> IO value
checked = either (assertFailure . show) pure
