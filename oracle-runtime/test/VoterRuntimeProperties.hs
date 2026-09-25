{-# LANGUAGE OverloadedStrings #-}

-- | Public-boundary TCP schedules: learners begin with empty logs, registration
-- precedes startup, and every command crosses EORC. No kernel state is copied.
module VoterRuntimeProperties (tests, within, poll, withCluster, send) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, check, newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (bracket, finally, onException)
import Control.Monad (foldM, forM_, void)
import Data.ByteString.Char8 qualified as ByteString
import Data.List (findIndex, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Oracle.Command (OracleCommand)
import Eclips.Oracle.Genesis (checkOracleGenesis, raftVoterBinding, raftVoterBindingNode)
import Eclips.Oracle.Identity (oracleClientRequestSequence)
import Eclips.Oracle.Projection
import Eclips.Oracle.Receipt
import Eclips.Oracle.Runtime
import Eclips.Oracle.Runtime.ConformanceClient
import Eclips.Oracle.Runtime.Internal.Submission (mayCancelVoterPreparation)
import Eclips.Oracle.Runtime.TCP
import Eclips.Oracle.Voter
import Eclips.Protocol.Oracle.Types (oracleHealthReplyConfiguration, oracleHealthReplyGeneration)
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Checkpoint (raftCheckpointIndex)
import Eclips.Raft.Configuration (RaftConfigurationRefView (..), RaftVotingConfigurationView (..), raftConfigurationRefView, raftVotingConfigurationView)
import Eclips.Raft.Identity (RaftNodeId)
import Eclips.Raft.Input (RaftInputView (..), RaftRpcView (InstallSnapshotView), raftInputView, raftRpcView)
import Eclips.Raft.State (RaftRole (RaftFollower))
import HealthRuntimeProperties (queryHealth)
import Network.Socket qualified as Socket
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestFixtures

tests :: TestTree
tests =
  testGroup "retained voter TCP administration"
    $ [ testCase "p08-voter-growth-demotion: singleton commissions learners and demotes its current leader without retiring its Herald" caseGrowthAndDemotion,
        testCase "p08-voter-cancellation: cancellation stays reachable while a missing learner gates ordinary proposals" caseCancellation,
        testCase "p08-voter-quorum-unavailable: an appended joint change stays pending without required voting majorities" caseQuorumUnavailable,
        testCase "a late learner installs a compacted Oracle checkpoint without diagnostic digests" (caseOracleCheckpointTransfer OracleStateDigestDisabled),
        testCase "a late learner installs and continues a digest-checked Oracle checkpoint" (caseOracleCheckpointTransfer OracleStateDigestEnabled),
        testCase "a checkpoint cannot switch the configured diagnostic digest mode" caseCheckpointDigestModeMismatch
      ]
      <> [testCase ("p08-voter-leader-loss-" <> phaseName phase) (caseLeaderLoss phase) | phase <- [minBound .. maxBound]]

within :: String -> IO a -> IO a
within label action = timeout 10000000 action >>= maybe (assertFailure ("timed out: " <> label)) pure

poll :: IO (Maybe a) -> IO a
poll observe = observe >>= maybe (threadDelay 1000 >> poll observe) pure

withCluster :: OracleTcpClusterConfiguration -> (OracleTcpCluster -> IO a) -> IO a
withCluster configuration use = withOracleTcpCluster configuration use >>= either (assertFailure . show) pure

clientFor :: OracleTcpCluster -> IO ConformanceClient
clientFor cluster =
  newConformanceClient
    (checked "voter-change genesis" (checkOracleGenesis fixtureVoterChangeOracleGenesis))
    fixtureHeraldId
    fixtureHeraldEpoch
    cluster
    >>= either (assertFailure . show) pure

send :: ConformanceClient -> OracleCommand -> IO (ConformanceRequest, OracleReceipt)
send client command = do
  request <- prepareConformanceRequest client Nothing command
  receipt <- within ("committed receipt for " <> show command) (submitConformanceRequest client request)
  pure (request, receipt)

registrationFrom :: OracleReceipt -> IO OracleReplicaRegistration
registrationFrom receipt = case oracleReceiptVoterResult receipt of
  Just (ReplicaRegistered registration) -> pure registration
  Just (ReplicaAlreadyVoter registration) -> pure registration
  Just (ReplicaAlreadyLearner registration) -> pure registration
  other -> assertFailure ("registration rejected: " <> show other <> "; " <> show receipt)

changeFrom :: OracleReceipt -> IO VoterChange
changeFrom receipt = case oracleReceiptVoterResult receipt of
  Just (VoterChangeAccepted change) -> pure change
  other -> assertFailure ("change rejected: " <> show other <> "; " <> show receipt)

statusAt :: OracleTcpCluster -> RaftNodeId -> IO OracleRuntimeStatus
statusAt cluster node = oracleTcpNodeStatuses cluster >>= maybe (assertFailure "replica missing") pure . lookup node

endpoint :: OracleTcpEndpoint -> OracleReplicaEndpoint
endpoint value =
  checked
    "replica endpoint"
    ( oracleReplicaEndpoint
        (ByteString.pack (oracleTcpEndpointHost value))
        (oracleTcpEndpointPort value)
    )

registerFounder :: ConformanceClient -> OracleTcpCluster -> IO OracleReplicaRegistration
registerFounder client cluster = do
  oracle <- maybe (assertFailure "founder EORC endpoint missing") pure (lookup fixtureInitialLeaderNode (oracleTcpOracleContacts cluster))
  raft <- maybe (assertFailure "founder ERFT endpoint missing") pure (lookup fixtureInitialLeaderNode (oracleTcpRaftEndpoints cluster))
  (_, receipt) <-
    send
      client
      ( registerOracleReplicaCommand
          fixtureInitialLeaderNode
          fixtureHeraldEpoch
          (oracleReplicaContact (endpoint oracle) (endpoint raft) (endpoint oracle))
      )
  registrationFrom receipt

-- The OS chooses a free loopback port. These finite, serial fixtures immediately
-- use the reservation as the advertised endpoint before learner startup.
freeEndpoint :: IO OracleReplicaEndpoint
freeEndpoint = bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
  Socket.bind socket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  address <- Socket.getSocketName socket
  case address of
    Socket.SockAddrInet port _ -> pure (checked "free endpoint" (oracleReplicaEndpoint "127.0.0.1" (fromIntegral port)))
    _ -> assertFailure "unexpected loopback address family"

registerLearner :: ConformanceClient -> Int -> IO OracleReplicaRegistration
registerLearner client ordinal = do
  oracle <- freeEndpoint
  raft <- freeEndpoint
  let node = fixtureRaftNodes !! ordinal
      host = heraldMemberEpoch (fixtureMembers !! ordinal)
  (_, receipt) <- send client (registerOracleReplicaCommand node host (oracleReplicaContact oracle raft oracle))
  registrationFrom receipt

withLearner :: OracleReplicaRegistration -> OracleReplicaRegistration -> (OracleTcpCluster -> IO a) -> IO a
withLearner = withLearnerRecording OracleDiagnosticsOnly

withLearnerRecording :: OracleRuntimeRecordingMode -> OracleReplicaRegistration -> OracleReplicaRegistration -> (OracleTcpCluster -> IO a) -> IO a
withLearnerRecording = withLearnerDigestMode OracleStateDigestDisabled

withLearnerDigestMode :: OracleStateDigestMode -> OracleRuntimeRecordingMode -> OracleReplicaRegistration -> OracleReplicaRegistration -> (OracleTcpCluster -> IO a) -> IO a
withLearnerDigestMode digestMode recording founder learner use = do
  contact <- maybe (assertFailure "learner lacks contact") pure (replicaRegistrationContact learner)
  founderContact <- maybe (assertFailure "founder lacks contact") pure (replicaRegistrationContact founder)
  let node = replicaRegistrationNode learner
      listen address = checked "listener" (oracleTcpListenEndpoint (ByteString.unpack (replicaEndpointHost address)) (replicaEndpointPort address))
      runtime =
        checked
          "learner runtime"
          ( oracleRuntimeLearnerConfiguration
              (fixtureVoterChangeRaftGenesis node)
              fixtureVoterChangeOracleGenesis
              learner
              (runtimeElectionTimeoutSource (pure (if node == fixtureReplacementNode then 30000 else 60000)))
          )
      configuration = checked "learner cluster" (oracleTcpClusterConfiguration [configureOracleRuntimeStateDigest digestMode (configureOracleRuntimeRecording recording runtime)])
      listeners =
        checked
          "learner listeners"
          ( configureOracleTcpListeners
              [(node, listen (replicaContactOracle contact), listen (replicaContactRaft contact))]
              configuration
          )
      contacts =
        checked
          "founder contact"
          ( configureOracleTcpReplicaContacts
              [(replicaRegistrationNode founder, listen (replicaContactRaft founderContact))]
              listeners
          )
  withCluster contacts use

awaitStable :: [OracleTcpCluster] -> [RaftNodeId] -> IO ()
awaitStable clusters expected = within "all replica adapters install stable configuration" $ poll $ do
  statuses <- concat <$> traverse oracleTcpNodeStatuses clusters
  let correct (_, status) = case voterConfigurationView (oracleRuntimeVoterConfiguration status) of
        StableVoterConfigurationView bindings ->
          map raftVoterBindingNode (oracleVoterBindingList bindings) == expected
            && oracleRuntimePendingVoterChange status == Nothing
        _ -> False
  pure (if all correct statuses then Just () else Nothing)

-- The receiver is absent while the source compacts. Its actual Oracle owner
-- must recover one unconsumed result, sparse retirement and old watch entries
-- from InstallSnapshot; none can be reconstructed by replaying discarded logs.
caseOracleCheckpointTransfer :: OracleStateDigestMode -> Assertion
caseOracleCheckpointTransfer digestMode = do
  let configured = checked "recorded checkpoint founder" (oracleTcpClusterConfiguration [configureOracleRuntimeStateDigest digestMode (configureOracleRuntimeRecording OracleCaptureHistory fixtureVoterChangeRuntimeConfiguration)])
  withCluster configured $ \founderCluster -> do
    _ <- awaitReadyLeader founderCluster
    client <- clientFor founderCluster
    founder <- registerFounder client founderCluster
    learner <- registerLearner client 1
    retainedRequest <- prepareConformanceRequest client Nothing validOracleCommand
    sent <- within "the retained request loses its physical reply" (submitConformanceRequestWithoutReply client fixtureInitialLeaderNode retainedRequest)
    assertBool "the request crossed the actual EORC lane" sent
    retainedReceipt <- within "the lost-reply result is committed" $ poll $ do
      observed <- queryConformanceRequest client retainedRequest
      pure $ case observed of
        ConformanceRequestFound receipt -> Just receipt
        _ -> Nothing
    (retiredRequest, _) <- send client validOracleCommand
    lastRequest <- foldM (\_ _ -> fst <$> send client validOracleCommand) retiredRequest ([1 .. 8] :: [Int])
    let high = oracleClientRequestSequence (conformanceRequestId lastRequest)
        outstanding = oracleClientRequestSequence (conformanceRequestId retainedRequest)
        progress = checked "one unconsumed receipt" (Lifetime.receiptRetirement (Just high) (Set.singleton outstanding))
    confirmed <- within "the source retires later receipts around the outstanding result" (retireConformanceReceiptProgress client progress)
    confirmed @?= progress
    sourceHistory <- NonEmpty.toList <$> within "the original watch prefix" (watchConformanceEntries client (controlIndex 0))
    source <- statusAt founderCluster fixtureInitialLeaderNode
    within "the source installs its native local checkpoint before learner startup" $ poll $ do
      events <- concatMap snd <$> oracleTcpNodeRecordings founderCluster
      pure $ if any (capturedThrough (oracleRuntimeAppliedRaftIndex source)) events then Just () else Nothing
    assertBool "watch entries carry exactly the configured witness presence" (all ((== (digestMode == OracleStateDigestEnabled)) . isJust . appliedEntryPostStateDigest) sourceHistory)
    withLearnerDigestMode digestMode OracleCaptureHistory founder learner $ \learnerCluster -> do
      allClient <- addConformanceReplicaCluster client learnerCluster >>= either (assertFailure . show) pure
      (snapshotIndex, installedEvents) <- within "the late learner installs and acknowledges a real snapshot" $ poll $ do
        events <- concatMap snd <$> oracleTcpNodeRecordings learnerCluster
        let installed =
              [ index
              | RuntimeRaftInputInstalled input <- events,
                ObserveRequestView sourceNode rpc <- [raftInputView input],
                sourceNode == fixtureInitialLeaderNode,
                InstallSnapshotView _ _ snapshot <- [raftRpcView rpc],
                let index = raftCheckpointIndex snapshot,
                index >= oracleRuntimeAppliedRaftIndex source,
                any (acknowledged index) events
              ]
        pure $ case installed of
          index : _ -> Just (index, events)
          [] -> Nothing
      let installPosition = findIndex (receivedSnapshot snapshotIndex) installedEvents
          ackPosition = findIndex (acknowledged snapshotIndex) installedEvents
      assertBool "native snapshot admission precedes its application installation acknowledgement" (case (installPosition, ackPosition) of (Just before, Just after) -> before < after; _ -> False)
      let beforeAcknowledgement = takeWhile (not . acknowledged snapshotIndex) installedEvents
      assertEqual "snapshot installation does not replay old Oracle commands" [] [request | RuntimeOracleInputInstalled request _ <- beforeAcknowledgement]
      assertEqual "snapshot installation does not replay the historical watch entries" [] [index | RuntimeWatchEntryRetained index <- beforeAcknowledgement]
      caughtUp <- statusAt learnerCluster (replicaRegistrationNode learner)
      oracleRuntimeAppliedControlIndex caughtUp @?= oracleRuntimeAppliedControlIndex source
      assertBool "the installed prefix reaches the application owner" (oracleRuntimeAppliedRaftIndex caughtUp >= snapshotIndex)
      oracleRuntimeServiceReady caughtUp @?= False
      -- Commission the installed state, then demote and stop its source. Every
      -- later public query must be answered by the independently installed owner.
      initial <- oracleRuntimeVoterConfiguration <$> statusAt founderCluster fixtureInitialLeaderNode
      let complete = checked "founder and installed learner" (oracleVoterBindings (sortOn raftVoterBindingNode [raftVoterBinding (replicaRegistrationNode r) (replicaRegistrationHost r) | r <- [founder, learner]]))
      (_, commissioned) <- send allClient (beginVoterChangeCommand (voterConfigurationId initial) ExplicitCommission complete)
      _ <- changeFrom commissioned
      awaitStable [founderCluster, learnerCluster] (map raftVoterBindingNode (oracleVoterBindingList complete))
      grown <- oracleRuntimeVoterConfiguration <$> statusAt founderCluster fixtureInitialLeaderNode
      let survivor = checked "installed learner survives" (oracleVoterBindings [raftVoterBinding (replicaRegistrationNode learner) (replicaRegistrationHost learner)])
      (_, demoted) <- send allClient (beginVoterChangeCommand (voterConfigurationId grown) ExplicitDemotion survivor)
      _ <- changeFrom demoted
      awaitStable [founderCluster, learnerCluster] [replicaRegistrationNode learner]
      elected <- within "the installed learner becomes the sole ready leader" (awaitReadyLeader learnerCluster)
      elected @?= replicaRegistrationNode learner
      stopOracleTcpNode founderCluster fixtureInitialLeaderNode >>= (@?= True)
      assertEqual "the new owner retained the outstanding lost-reply receipt" (ConformanceRequestFound retainedReceipt) =<< within "query installed receipt" (queryConformanceRequest allClient retainedRequest)
      assertEqual "the new owner retained sparse receipt retirement" (ConformanceRequestRetired (OracleRequestPrefixRetired high)) =<< within "query installed retired classification" (queryConformanceRequest allClient retiredRequest)
      recovered <- within "exact retry is immutable after installation and election" (submitConformanceRequest allClient retainedRequest)
      recovered @?= retainedReceipt
      (_, suffixReceipt) <- send allClient alternateOracleCommand
      assertBool "a new application command follows installed state and configuration suffix" (oracleReceiptControlIndex suffixReceipt > oracleReceiptControlIndex demoted)
      successorHistory <- NonEmpty.toList <$> within "watch after source loss" (watchConformanceEntries allClient (controlIndex 0))
      assertBool "the installed owner retains the configured mode for new entries" (all ((== (digestMode == OracleStateDigestEnabled)) . isJust . appliedEntryPostStateDigest) successorHistory)
      take (length sourceHistory) successorHistory @?= sourceHistory
      assertEqual "only ordinary suffix publication adds the new application result" 1 (length [() | entry <- successorHistory, Just command <- [appliedEntryCommand entry], appliedEntryRequestId command == oracleReceiptRequestId suffixReceipt])
  where
    capturedThrough index (RuntimeRaftInputInstalled input) = case raftInputView input of
      CheckpointApplicationView captured _ -> captured >= index
      _ -> False
    capturedThrough _ _ = False
    receivedSnapshot index (RuntimeRaftInputInstalled input) = case raftInputView input of
      ObserveRequestView _ rpc -> case raftRpcView rpc of
        InstallSnapshotView _ _ snapshot -> raftCheckpointIndex snapshot == index
        _ -> False
      _ -> False
    receivedSnapshot _ _ = False
    acknowledged index (RuntimeRaftInputInstalled input) = case raftInputView input of
      AcknowledgeCheckpointView installed -> installed == index
      _ -> False
    acknowledged _ _ = False

-- The native snapshot itself is valid, but its Oracle payload must not change
-- the immutable local diagnostic mode chosen before this owner started.
caseCheckpointDigestModeMismatch :: Assertion
caseCheckpointDigestModeMismatch = do
  let configured = checked "digest-enabled founder" (oracleTcpClusterConfiguration [configureOracleRuntimeStateDigest OracleStateDigestEnabled (configureOracleRuntimeRecording OracleCaptureHistory fixtureVoterChangeRuntimeConfiguration)])
  withCluster configured $ \founderCluster -> do
    _ <- awaitReadyLeader founderCluster
    client <- clientFor founderCluster
    founder <- registerFounder client founderCluster
    learner <- registerLearner client 1
    _ <- send client validOracleCommand
    source <- statusAt founderCluster fixtureInitialLeaderNode
    within "source checkpoint precedes mismatched learner" $ poll $ do
      events <- concatMap snd <$> oracleTcpNodeRecordings founderCluster
      pure $ if any (capturedThrough (oracleRuntimeAppliedRaftIndex source)) events then Just () else Nothing
    withLearner founder learner $ \learnerCluster -> do
      observed <- within "mismatched checkpoint stops the owner" (awaitOracleTcpNodeExit learnerCluster (replicaRegistrationNode learner))
      observed @?= Just (OracleRuntimeFailed (RuntimeEssentialChildFailed "oracle-checkpoint" "checkpoint diagnostic digest mode differs from runtime configuration"))
  where
    capturedThrough index (RuntimeRaftInputInstalled input) = case raftInputView input of
      CheckpointApplicationView captured _ -> captured >= index
      _ -> False
    capturedThrough _ _ = False

caseGrowthAndDemotion :: Assertion
caseGrowthAndDemotion = withCluster fixtureVoterChangeTcpConfiguration $ \founderCluster -> do
  _ <- awaitReadyLeader founderCluster
  client <- clientFor founderCluster
  founder <- registerFounder client founderCluster
  b <- registerLearner client 1
  c <- registerLearner client 2
  withLearner founder b $ \bCluster -> withLearner founder c $ \cCluster -> do
    clientB <- addConformanceReplicaCluster client bCluster >>= either (assertFailure . show) pure
    allClient <- addConformanceReplicaCluster clientB cCluster >>= either (assertFailure . show) pure
    initial <- oracleRuntimeVoterConfiguration <$> statusAt founderCluster fixtureInitialLeaderNode
    let complete =
          checked
            "complete binding"
            ( oracleVoterBindings
                ( sortOn
                    raftVoterBindingNode
                    [raftVoterBinding (replicaRegistrationNode r) (replicaRegistrationHost r) | r <- [founder, b, c]]
                )
            )
    (request, receipt) <- send allClient (beginVoterChangeCommand (voterConfigurationId initial) ExplicitCommission complete)
    _ <- changeFrom receipt
    awaitStable [founderCluster, bCluster, cCluster] fixtureRaftNodes
    replayed <- within "immutable begin receipt after completion" (submitConformanceRequest allClient request)
    replayed @?= receipt
    grown <- oracleRuntimeVoterConfiguration <$> statusAt founderCluster fixtureInitialLeaderNode
    let retained = checked "retained bindings" (oracleVoterBindings (filter ((/= fixtureInitialLeaderNode) . raftVoterBindingNode) (oracleVoterBindingList complete)))
    (_, removalReceipt) <- send allClient (beginVoterChangeCommand (voterConfigurationId grown) ExplicitDemotion retained)
    _ <- changeFrom removalReceipt
    awaitStable [founderCluster, bCluster, cCluster] (drop 1 fixtureRaftNodes)
    removed <- statusAt founderCluster fixtureInitialLeaderNode
    oracleRuntimeRole removed @?= RaftFollower
    demotedContact <- maybe (assertFailure "demoted role health endpoint missing") pure (lookup fixtureInitialLeaderNode (oracleTcpOracleContacts founderCluster))
    demotedHealth <- within "demotion preserves active Herald health admission" (queryHealth (checked "demoted genesis" (checkOracleGenesis fixtureVoterChangeOracleGenesis)) demotedContact 48)
    snd (oracleHealthReplyConfiguration demotedHealth) @?= voterConfigurationNative (oracleRuntimeVoterConfiguration removed)
    -- The same cohost remains an ordinary active Herald; an application command
    -- with its residence can still commit through a surviving voter.
    (_, applicationReceipt) <-
      send allClient validOracleCommand `onException` do
        observations <- concat <$> traverse oracleTcpNodeStatuses [founderCluster, bCluster, cCluster]
        print [(node, oracleRuntimeRole status, oracleRuntimeTerm status, oracleRuntimeLeaderHint status, oracleRuntimeServiceReady status, oracleRuntimeAppliedRaftIndex status, oracleRuntimeAppliedControlIndex status) | (node, status) <- observations]
    assertBool
      "application after voter demotion has a later control index"
      (oracleReceiptControlIndex applicationReceipt > oracleReceiptControlIndex removalReceipt)
    history <- NonEmpty.toList <$> within "configuration watch history" (watchConformanceEntries allClient (controlIndex 0))
    let configurations = [configuration | entry <- history, Just configuration <- [appliedEntryConfiguration entry]]
    length configurations @?= 4
    forM_ [entry | entry <- history, Just _ <- [appliedEntryConfiguration entry]] $ \entry ->
      appliedEntryCommand entry @?= Nothing

caseCancellation :: Assertion
caseCancellation = withCluster fixtureVoterChangeTcpConfiguration $ \cluster -> do
  _ <- awaitReadyLeader cluster
  client <- clientFor cluster
  founder <- registerFounder client cluster
  learner <- registerLearner client 1
  initial <- oracleRuntimeVoterConfiguration <$> statusAt cluster fixtureInitialLeaderNode
  let desired =
        checked
          "two bindings"
          ( oracleVoterBindings
              ( sortOn
                  raftVoterBindingNode
                  [raftVoterBinding (replicaRegistrationNode r) (replicaRegistrationHost r) | r <- [founder, learner]]
              )
          )
  (request, receipt) <- send client (beginVoterChangeCommand (voterConfigurationId initial) ExplicitCommission desired)
  change <- changeFrom receipt
  mayCancelVoterPreparation (voterChangeId change) (Just change) @?= True
  mayCancelVoterPreparation (checked "different change" (mkVoterChangeId (controlIndex 1))) (Just change) @?= False
  within "native learner gate" $ poll $ do
    status <- statusAt cluster fixtureInitialLeaderNode
    pure (if oracleRuntimeConfigurationFrontier status /= Nothing then Just () else Nothing)
  beforeRetry <- oracleRuntimeConfigurationFrontier <$> statusAt cluster fixtureInitialLeaderNode
  retried <- within "retained preparation receipt while learner unavailable" (submitConformanceRequest client request)
  retried @?= receipt
  afterRetry <- oracleRuntimeConfigurationFrontier <$> statusAt cluster fixtureInitialLeaderNode
  afterRetry @?= beforeRetry
  (_, cancelled) <- send client (cancelVoterChangeCommand (voterChangeId change))
  case oracleReceiptVoterResult cancelled of
    Just (VoterChangeCancelledResult terminal) -> voterChangePhase terminal @?= VoterChangeCancelled
    _ -> assertFailure ("cancellation rejected: " <> show cancelled)
  withLearner founder learner $ \learnerCluster -> do
    awaitStable [cluster, learnerCluster] [fixtureInitialLeaderNode]
    void (send client validOracleCommand)
    history <- NonEmpty.toList <$> within "cancelled history" (watchConformanceEntries client (controlIndex 0))
    [configuration | entry <- history, Just configuration <- [appliedEntryConfiguration entry]] @?= []

-- Each interrupted leader owns a real supervised Oracle/Raft runtime. Its
-- ordinary cohost remains in the checked Herald membership throughout.
data FailurePhase = Preparing | JointAppended | JointCommitted | FinalAppended | FinalCommitted
  deriving stock (Eq, Enum, Bounded, Show)

phaseName :: FailurePhase -> String
phaseName phase = case phase of
  Preparing -> "preparing"
  JointAppended -> "joint-appended"
  JointCommitted -> "joint-committed"
  FinalAppended -> "final-appended"
  FinalCommitted -> "final-committed"

matchesPhase :: FailurePhase -> OracleVoterCheckpoint -> Bool
matchesPhase phase checkpoint = case (phase, checkpoint) of
  (Preparing, OracleVoterPreparationCaptured _) -> True
  (JointAppended, OracleVoterConfigurationAppended configuration _) -> joint configuration
  (JointCommitted, OracleVoterConfigurationCommitted configuration _) -> joint configuration
  (FinalAppended, OracleVoterConfigurationAppended configuration _) -> not (joint configuration)
  (FinalCommitted, OracleVoterConfigurationCommitted configuration _) -> not (joint configuration)
  _ -> False
  where
    joint configuration = case raftVotingConfigurationView configuration of
      JointRaftConfigurationView {} -> True
      StableRaftConfigurationView {} -> False

caseLeaderLoss :: FailurePhase -> Assertion
caseLeaderLoss phase = do
  armed <- newTVarIO False
  fired <- newTVarIO False
  let checkpoint event = do
        trigger <- atomically $ do
          active <- readTVar armed
          if active && matchesPhase phase event
            then do
              writeTVar armed False
              writeTVar fired True
              pure True
            else pure False
        if trigger then ioError (userError ("injected Oracle role loss at " <> phaseName phase)) else pure ()
      configured =
        checked
          "checkpointed founder"
          ( oracleTcpClusterConfiguration
              [configureOracleRuntimeVoterCheckpoint checkpoint fixtureVoterChangeRuntimeConfiguration]
          )
  withCluster configured $ \founderCluster -> do
    _ <- awaitReadyLeader founderCluster
    client <- clientFor founderCluster
    founder <- registerFounder client founderCluster
    b <- registerLearner client 1
    c <- registerLearner client 2
    withLearner founder b $ \bCluster -> withLearner founder c $ \cCluster -> do
      clientB <- addConformanceReplicaCluster client bCluster >>= either (assertFailure . show) pure
      allClient <- addConformanceReplicaCluster clientB cCluster >>= either (assertFailure . show) pure
      initial <- oracleRuntimeVoterConfiguration <$> statusAt founderCluster fixtureInitialLeaderNode
      let complete =
            checked
              "complete binding"
              ( oracleVoterBindings
                  ( sortOn
                      raftVoterBindingNode
                      [raftVoterBinding (replicaRegistrationNode r) (replicaRegistrationHost r) | r <- [founder, b, c]]
                  )
              )
      void (send allClient (beginVoterChangeCommand (voterConfigurationId initial) ExplicitCommission complete))
      awaitStable [founderCluster, bCluster, cCluster] fixtureRaftNodes
      grown <- oracleRuntimeVoterConfiguration <$> statusAt founderCluster fixtureInitialLeaderNode
      let survivors =
            checked
              "survivor bindings"
              ( oracleVoterBindings
                  (filter ((/= fixtureInitialLeaderNode) . raftVoterBindingNode) (oracleVoterBindingList complete))
              )
      atomically (writeTVar armed True)
      (request, receipt) <- send allClient (beginVoterChangeCommand (voterConfigurationId grown) ExplicitDemotion survivors)
      accepted <- changeFrom receipt
      within "the selected owner checkpoint" (atomically (readTVar fired >>= check))
      awaitStable [bCluster, cCluster] (drop 1 fixtureRaftNodes)
      replayed <- within "original intent receipt after leader loss" (submitConformanceRequest allClient request)
      replayed @?= receipt
      history <- NonEmpty.toList <$> within "surviving configuration history" (watchConformanceEntries allClient (controlIndex 0))
      let observed = [configuration | entry <- history, Just configuration <- [appliedEntryConfiguration entry]]
          terminal =
            [ change
            | entry <- history,
              event <- appliedEntryProjectionEvents entry,
              OracleVoterChangeChangedView change <- [oracleProjectionEventView event],
              voterChangeId change == voterChangeId accepted,
              voterChangePhase change == VoterChangeCompleted
            ]
      length observed @?= 4
      length terminal @?= 1
      void (send allClient validOracleCommand)

-- Pause after native joint append but before any outbound effects can expose it.
-- Removing both remote voters then prevents either required majority. Recorded
-- subsequent heartbeats prove the live owner resumed; no elapsed delay is used
-- as evidence that the joint entry was proposed or that the peers stopped.
caseQuorumUnavailable :: Assertion
caseQuorumUnavailable = do
  armed <- newTVarIO False
  appended <- newTVarIO Nothing
  released <- newTVarIO False
  let checkpoint event = case event of
        OracleVoterConfigurationAppended configuration index -> do
          pause <- atomically $ do
            active <- readTVar armed
            if active && matchesPhase JointAppended event
              then writeTVar armed False >> writeTVar appended (Just (configuration, index)) >> pure True
              else pure False
          if pause then atomically (readTVar released >>= check) else pure ()
        _ -> pure ()
      configured =
        checked
          "quorum-loss checkpoint"
          ( oracleTcpClusterConfiguration
              [configureOracleRuntimeRecording OracleCaptureHistory (configureOracleRuntimeVoterCheckpoint checkpoint fixtureVoterChangeRuntimeConfiguration)]
          )
  withCluster configured $ \founderCluster -> do
    _ <- awaitReadyLeader founderCluster
    client <- clientFor founderCluster
    founder <- registerFounder client founderCluster
    b <- registerLearner client 1
    c <- registerLearner client 2
    withLearner founder b $ \bCluster -> withLearner founder c $ \cCluster -> do
      clientB <- addConformanceReplicaCluster client bCluster >>= either (assertFailure . show) pure
      allClient <- addConformanceReplicaCluster clientB cCluster >>= either (assertFailure . show) pure
      initial <- oracleRuntimeVoterConfiguration <$> statusAt founderCluster fixtureInitialLeaderNode
      let complete =
            checked
              "all bindings"
              ( oracleVoterBindings
                  ( sortOn
                      raftVoterBindingNode
                      [raftVoterBinding (replicaRegistrationNode r) (replicaRegistrationHost r) | r <- [founder, b, c]]
                  )
              )
      void (send allClient (beginVoterChangeCommand (voterConfigurationId initial) ExplicitCommission complete))
      awaitStable [founderCluster, bCluster, cCluster] fixtureRaftNodes
      grown <- oracleRuntimeVoterConfiguration <$> statusAt founderCluster fixtureInitialLeaderNode
      let survivors =
            checked
              "remaining bindings"
              ( oracleVoterBindings
                  (filter ((/= fixtureInitialLeaderNode) . raftVoterBindingNode) (oracleVoterBindingList complete))
              )
      atomically (writeTVar armed True)
      (_, receipt) <- send allClient (beginVoterChangeCommand (voterConfigurationId grown) ExplicitDemotion survivors)
      accepted <- changeFrom receipt
      ( do
          (joint, appendedIndex) <- within "joint appended before transmission" $ atomically $ do
            observed <- readTVar appended
            maybe retry pure observed
          within "remote voters stop" $ do
            stopOracleTcpNode bCluster (replicaRegistrationNode b) >>= (@?= True)
            stopOracleTcpNode cCluster (replicaRegistrationNode c) >>= (@?= True)
          atomically (writeTVar released True)
          within "native owner resumes through later heartbeat attempts" $ poll $ do
            recordings <- concatMap snd <$> oracleTcpNodeRecordings founderCluster
            let isProposal (RuntimeRaftInputInstalled input) = case raftInputView input of
                  ProposeConfigurationView _ _ configuration _ -> configuration == joint
                  _ -> False
                isProposal _ = False
                isHeartbeat (RuntimeRaftInputInstalled input) = case raftInputView input of
                  FireHeartbeatTimerView _ -> True
                  _ -> False
                isHeartbeat _ = False
                afterJoint = dropWhile (not . isProposal) recordings
            pure (if length (filter isHeartbeat afterJoint) >= 2 then Just () else Nothing)
          pending <- statusAt founderCluster fixtureInitialLeaderNode
          oracleRuntimeVoterConfiguration pending @?= grown
          oracleRuntimePendingVoterChange pending @?= Just accepted
          oracleRuntimeAppliedControlIndex pending @?= oracleReceiptControlIndex receipt
          assertBool
            "uncommitted joint entry never advances the applied native prefix"
            (oracleRuntimeAppliedRaftIndex pending < appendedIndex)
          oracleRuntimeServiceReady pending @?= False
          voterChangePhase accepted @?= VoterChangePreparing
          endpointValue <-
            maybe
              (assertFailure "founder EORC health endpoint missing")
              pure
              (lookup fixtureInitialLeaderNode (oracleTcpOracleContacts founderCluster))
          let genesis = checked "health genesis" (checkOracleGenesis fixtureVoterChangeOracleGenesis)
          health <- within "fresh native health under unavailable joint quorum" (queryHealth genesis endpointValue 47)
          let (effectiveReference, effectiveConfiguration) = oracleHealthReplyConfiguration health
          effectiveConfiguration @?= joint
          case raftConfigurationRefView effectiveReference of
            RaftConfigurationEntryRefView index _ -> index @?= appendedIndex
            GenesisRaftConfigurationRefView -> assertFailure "health returned old committed genesis"
          assertBool "native health generation advanced beyond initial observations" (oracleHealthReplyGeneration health > 0)
          -- A second independently correlated query of the same native view
          -- does not create a new configuration generation or control entry.
          again <- within "same native view in a later round" (queryHealth genesis endpointValue 48)
          oracleHealthReplyConfiguration again @?= oracleHealthReplyConfiguration health
          oracleHealthReplyGeneration again @?= oracleHealthReplyGeneration health
        )
        `finally` atomically (writeTVar released True)
