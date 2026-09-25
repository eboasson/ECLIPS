module ClusterProperties (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.List (findIndex)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.ProcessStart (processStartProcessEpochId, processStartProcessId, processStartResidence)
import Eclips.Oracle.Identity (oracleClientRequestSequence)
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (ProcessStartedView),
    ProcessOriginView (DynamicStartView),
    appliedEntryControlIndex,
    appliedEntryProjectionEvents,
    appliedEntryReceiptRetirement,
    oracleProjectionEventView,
    processRecordOrigin,
    processRecordProcessEpoch,
    processRecordProcessId,
    processRecordResidence,
  )
import Eclips.Oracle.Receipt (OracleRequestRetirement (..), oracleReceiptControlIndex)
import Eclips.Oracle.Runtime
  ( OracleRuntimeEvent (..),
    OracleRuntimeStatus,
    oracleRuntimeAppliedControlIndex,
  )
import Eclips.Oracle.Runtime.ConformanceClient
  ( ConformanceConflictResult (ConformanceConflictConnectionClosed),
    ConformanceRequestResult (ConformanceRequestAbsent, ConformanceRequestFound, ConformanceRequestRetired),
    ConformanceSubmissionDisposition (ConformanceSubmissionRetired),
    conformanceRequestId,
    newConformanceClient,
    prepareConflictingConformanceRequest,
    prepareConformanceRequest,
    queryConformanceRequest,
    retireConformanceReceiptProgress,
    retireConformanceReceipts,
    submitConflictingConformanceRequest,
    submitConformanceRequest,
    submitConformanceRequestDispositionVia,
    submitConformanceRequestWithoutReply,
    watchConformanceEntries,
    withConformanceReceiptRetirement,
    withConformanceReceiptRetirementProgress,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    oracleTcpNodeRecordings,
    stopOracleTcpNode,
    withOracleTcpCluster,
  )
import Eclips.Oracle.Runtime.TCP qualified as TCP
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Effect (RaftProposalStatus (RaftProposalCommitted))
import Eclips.Raft.Identity
  ( RaftLogIndex,
    RaftNodeId,
    raftLogIndex,
  )
import Eclips.Raft.Input
  ( RaftEntry (LeaderNoOp),
    RaftInputView (..),
    RaftRpcView
      ( AppendEntriesView,
        RequestVoteResponseView,
        RequestVoteView
      ),
    raftInputView,
    raftLogEntryIndex,
    raftLogEntryPayload,
    raftRpcView,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import TestFixtures
  ( alternateOracleCommand,
    alternateStart,
    awaitNodesAppliedPrefix,
    awaitReadyLeader,
    fixtureCheckedGenesis,
    fixtureFollowerNode,
    fixtureHeraldEpoch,
    fixtureHeraldId,
    fixtureInitialLeaderNode,
    fixtureRaftNodes,
    fixtureRecordedSingletonTcpConfiguration,
    fixtureRecordedTcpConfiguration,
    fixtureReplacementNode,
    fixtureSingletonCheckedGenesis,
    fixtureSingletonTcpConfiguration,
    validOracleCommand,
  )

tests :: TestTree
tests =
  testGroup
    "cluster"
    [ testCase "explicit listeners resolve their host and preserve ordinary Oracle traffic" caseExplicitListeners,
      testCase "listener configuration admits one exact assignment per node" caseListenerAssignments,
      testCase "singleton commits ordinary Start, retry, conflict, and watch without peer RPC" caseSingletonVertical,
      testCase
        "three voters project Start epochs across redirect, retry, conflict, and failover"
        caseThreeVoterVertical,
      testCase "receipt retirement bounds the published retry lifetime across repeated rejected requests" caseReceiptRetirement,
      testCase "sparse retirement keeps an unresolved request through later receipts and an unchanged-high-water release" caseSparseReceiptRetirement,
      testCase "committed receipt retirement survives follower application and leader change" caseReceiptRetirementLeaderChange,
      testCase "fresh, duplicate, and conflicting requests carry receipt retirement through the production adapter" casePiggybackReceiptRetirement
    ]

caseSparseReceiptRetirement :: IO ()
caseSparseReceiptRetirement = do
  composed <- withOracleTcpCluster fixtureSingletonTcpConfiguration $ \cluster -> do
    _ <- awaitReadyLeader cluster
    client <- newConformanceClient fixtureSingletonCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster >>= either (assertFailure . show) pure
    unresolved <- prepareConformanceRequest client Nothing validOracleCommand
    let progress n = either (error . show) id (Lifetime.receiptRetirement (Just n) (Set.singleton 1))
    forM_ [2 .. 24] $ \n -> do
      request <- prepareConformanceRequest client (Just (controlIndex 1000000)) validOracleCommand
      _ <- submitConformanceRequest client (withConformanceReceiptRetirementProgress (progress (n - 1)) request)
      confirmed <- retireConformanceReceiptProgress client (progress n)
      assertEqual "the unresolved exception survives commitment" (progress n) confirmed
      assertEqual "later published result is reclaimed" (ConformanceRequestRetired (OracleRequestPrefixRetired n)) =<< queryConformanceRequest client request
      observed <- queryConformanceRequest client unresolved
      assertBool "an unsubmitted exceptional request stays available" (case observed of ConformanceRequestAbsent _ -> True; _ -> False)
    -- A carrier below H is valid while it remains an exception. Once consumed,
    -- removing that exception at the same H must trigger a fresh commitment.
    receipt <- submitConformanceRequest client (withConformanceReceiptRetirementProgress (progress 24) unresolved)
    assertEqual "the exceptional request executes exactly once" (ConformanceRequestFound receipt) =<< queryConformanceRequest client unresolved
    let closed = Lifetime.receiptRetirementPrefix (Just 24)
    assertEqual "same-H exception removal is acknowledged" closed =<< retireConformanceReceiptProgress client closed
    assertEqual "a delayed exception snapshot cannot reopen it" closed =<< retireConformanceReceiptProgress client (progress 24)
    assertEqual "the once-exceptional published result is now retired" (ConformanceRequestRetired (OracleRequestPrefixRetired 24)) =<< queryConformanceRequest client unresolved
  either (assertFailure . show) pure composed

caseReceiptRetirement :: IO ()
caseReceiptRetirement = do
  composed <- withOracleTcpCluster fixtureSingletonTcpConfiguration $ \cluster -> do
    leader <- awaitReadyLeader cluster
    client <- newConformanceClient fixtureSingletonCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster >>= either (assertFailure . show) pure
    forM_ [1 .. 24] $ \sequenceNumber -> do
      request <- prepareConformanceRequest client Nothing validOracleCommand
      assertEqual "maintenance does not consume an ordinary request sequence" sequenceNumber (oracleClientRequestSequence (conformanceRequestId request))
      receipt <- submitConformanceRequest client request
      duplicate <- submitConformanceRequest client request
      assertEqual "exact result remains available until explicit release" receipt duplicate
      confirmed <- retireConformanceReceipts client sequenceNumber
      assertEqual "the production maintenance reply confirms the prefix" sequenceNumber confirmed
      queried <- queryConformanceRequest client request
      assertEqual "published result has the committed retired classification" (ConformanceRequestRetired (OracleRequestPrefixRetired sequenceNumber)) queried
      stale <- submitConformanceRequestDispositionVia client leader request
      assertEqual "a delayed exact submission cannot execute again" (Just (ConformanceSubmissionRetired (OracleRequestPrefixRetired sequenceNumber))) stale
      conflicting <- maybe (assertFailure "alternate Start did not conflict") pure (prepareConflictingConformanceRequest request alternateOracleCommand)
      conflicted <- submitConformanceRequestDispositionVia client leader conflicting
      assertEqual "conflict guarantees end at explicit retirement" (Just (ConformanceSubmissionRetired (OracleRequestPrefixRetired sequenceNumber))) conflicted
      repeated <- retireConformanceReceipts client sequenceNumber
      assertEqual "acknowledgement replay is idempotent" sequenceNumber repeated
      statuses <- TCP.oracleTcpNodeStatuses cluster
      let indexes = [oracleRuntimeAppliedControlIndex status | (_, status) <- statuses]
      assertEqual "only a fresh request and advancing retirement consume control entries" [controlIndex (2 * sequenceNumber)] indexes
  either (assertFailure . show) pure composed

caseReceiptRetirementLeaderChange :: IO ()
caseReceiptRetirementLeaderChange = do
  composed <- withOracleTcpCluster fixtureRecordedTcpConfiguration $ \cluster -> do
    firstLeader <- awaitReadyLeader cluster
    client <- newConformanceClient fixtureCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster >>= either (assertFailure . show) pure
    request <- prepareConformanceRequest client Nothing validOracleCommand
    handed <- submitConformanceRequestWithoutReply client fixtureFollowerNode request
    assertBool "the lost-reply request was dispatched" handed
    _ <- awaitNodesAppliedPrefix cluster fixtureRaftNodes 1
    recovered <- queryConformanceRequest client request
    case recovered of
      ConformanceRequestFound _ -> pure ()
      other -> assertFailure ("unacknowledged receipt was lost: " <> show other)
    frontier <- retireConformanceReceipts client 1
    assertEqual "retirement confirms only after commitment" 1 frontier
    _ <- awaitNodesAppliedPrefix cluster fixtureRaftNodes 2
    stopped <- stopOracleTcpNode cluster firstLeader
    assertBool "the original leader stops" stopped
    nextLeader <- awaitReadyLeader cluster
    assertBool "a survivor becomes service ready" (nextLeader /= firstLeader)
    queried <- queryConformanceRequest client request
    assertEqual "the new leader retained the lifetime boundary" (ConformanceRequestRetired (OracleRequestPrefixRetired 1)) queried
    obsolete <- submitConformanceRequestDispositionVia client nextLeader request
    assertEqual "a replicated retired request stays inert after leader change" (Just (ConformanceSubmissionRetired (OracleRequestPrefixRetired 1))) obsolete
    replayed <- retireConformanceReceipts client 1
    assertEqual "duplicate maintenance survives leader change" 1 replayed
    fresh <- prepareConformanceRequest client Nothing alternateOracleCommand
    receipt <- submitConformanceRequest client fresh
    assertEqual "retirement and obsolete retries do not consume another control index" (controlIndex 3) (oracleReceiptControlIndex receipt)
  either (assertFailure . show) pure composed

casePiggybackReceiptRetirement :: IO ()
casePiggybackReceiptRetirement = do
  composed <- withOracleTcpCluster fixtureSingletonTcpConfiguration $ \cluster -> do
    _ <- awaitReadyLeader cluster
    client <- newConformanceClient fixtureSingletonCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster >>= either (assertFailure . show) pure
    first <- prepareConformanceRequest client Nothing validOracleCommand
    _ <- submitConformanceRequest client first
    second <- prepareConformanceRequest client Nothing validOracleCommand
    secondReceipt <- submitConformanceRequest client second
    duplicate <- submitConformanceRequest client (withConformanceReceiptRetirement 1 second)
    assertEqual "a changed release promise preserves the original exact receipt" secondReceipt duplicate
    assertEqual "the duplicate retires the earlier published receipt" (ConformanceRequestRetired (OracleRequestPrefixRetired 1)) =<< queryConformanceRequest client first
    assertEqual "the duplicate keeps its own published receipt" (ConformanceRequestFound secondReceipt) =<< queryConformanceRequest client second
    third <- prepareConformanceRequest client Nothing validOracleCommand
    thirdReceipt <- submitConformanceRequest client third
    assertEqual "duplicate plus advancing prefix uses one maintenance entry" (controlIndex 4) (oracleReceiptControlIndex thirdReceipt)
    conflict <- maybe (assertFailure "alternate Start did not conflict") pure (prepareConflictingConformanceRequest third alternateOracleCommand)
    conflicted <- submitConflictingConformanceRequest client (withConformanceReceiptRetirement 2 conflict)
    assertEqual "semantic conflict keeps its normal protocol disposition" ConformanceConflictConnectionClosed conflicted
    assertEqual "the conflicting request still releases the prior prefix" (ConformanceRequestRetired (OracleRequestPrefixRetired 2)) =<< queryConformanceRequest client second
    assertEqual "the conflicting request preserves its original receipt" (ConformanceRequestFound thirdReceipt) =<< queryConformanceRequest client third
    fourth <- prepareConformanceRequest client Nothing validOracleCommand
    fourthReceipt <- submitConformanceRequest client (withConformanceReceiptRetirement 3 fourth)
    assertEqual "fresh command and advancing prefix use a single control entry" (controlIndex 6) (oracleReceiptControlIndex fourthReceipt)
    assertEqual "fresh piggyback retires the old published receipt" (ConformanceRequestRetired (OracleRequestPrefixRetired 3)) =<< queryConformanceRequest client third
    original <- submitConformanceRequest client fourth
    assertEqual "an undecorated delayed retry has the same semantic identity" fourthReceipt original
    suffix <- watchConformanceEntries client (controlIndex 5)
    assertEqual "the ordinary watch entry confirms its piggyback frontier" [Just (fixtureHeraldEpoch, 3)] (map appliedEntryReceiptRetirement (NonEmpty.toList suffix))
  either (assertFailure . show) pure composed

caseExplicitListeners :: IO ()
caseExplicitListeners = do
  endpoint <- either (assertFailure . show) pure (TCP.oracleTcpListenEndpoint "localhost" 0)
  configured <- either (assertFailure . show) pure (TCP.configureOracleTcpListeners [(fixtureInitialLeaderNode, endpoint, endpoint)] fixtureSingletonTcpConfiguration)
  composed <- withOracleTcpCluster configured $ \cluster -> do
    _ <- awaitReadyLeader cluster
    let contacts = TCP.oracleTcpOracleContacts cluster
    assertBool "every listener reports a resolved nonzero port" (all ((/= 0) . TCP.oracleTcpEndpointPort . snd) (contacts <> TCP.oracleTcpRaftEndpoints cluster))
    client <- newConformanceClient fixtureSingletonCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster >>= either (assertFailure . show) pure
    request <- prepareConformanceRequest client (Just (controlIndex 0)) validOracleCommand
    receipt <- submitConformanceRequest client request
    assertEqual "DNS-selected address carries the checked Oracle command" (controlIndex 1) (oracleReceiptControlIndex receipt)
    stopped <- stopOracleTcpNode cluster fixtureInitialLeaderNode
    assertBool "the deployment supervisor can stop its co-host" stopped
    exited <- TCP.awaitOracleTcpNodeExit cluster fixtureInitialLeaderNode
    assertBool "co-host exit remains observable after its workers join" (case exited of Just _ -> True; Nothing -> False)
  either (assertFailure . show) pure composed

caseListenerAssignments :: IO ()
caseListenerAssignments = do
  endpoint <- either (assertFailure . show) pure (TCP.oracleTcpListenEndpoint "127.0.0.1" 41001)
  let checkedFailure expected result = case result of
        Left actual -> assertEqual "listener assignment rejection" expected actual
        Right _ -> assertFailure "invalid listener assignment was admitted"
  checkedFailure TCP.OracleTcpConfigurationListenerSetMismatch (TCP.configureOracleTcpListeners [] fixtureSingletonTcpConfiguration)
  checkedFailure TCP.OracleTcpConfigurationListenerSetMismatch (TCP.configureOracleTcpListeners [(fixtureInitialLeaderNode, endpoint, endpoint), (fixtureInitialLeaderNode, endpoint, endpoint)] fixtureSingletonTcpConfiguration)
  checkedFailure TCP.OracleTcpConfigurationListenerConflict (TCP.configureOracleTcpListeners [(fixtureInitialLeaderNode, endpoint, endpoint)] fixtureSingletonTcpConfiguration)

caseSingletonVertical :: IO ()
caseSingletonVertical = do
  composed <-
    withOracleTcpCluster fixtureRecordedSingletonTcpConfiguration $ \cluster -> do
      leader <- awaitReadyLeader cluster
      assertEqual "the sole voter elects itself" fixtureInitialLeaderNode leader
      client <-
        newConformanceClient fixtureSingletonCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster
          >>= either (assertFailure . show) pure
      request <- prepareConformanceRequest client (Just (controlIndex 0)) validOracleCommand
      receipt <- submitConformanceRequest client request
      assertEqual "first Start follows the leader no-op" (controlIndex 1) (oracleReceiptControlIndex receipt)
      duplicate <- submitConformanceRequest client request
      assertEqual "exact retry returns the retained receipt" receipt duplicate
      retained <- queryConformanceRequest client request
      assertEqual "retained result is queryable" (ConformanceRequestFound receipt) retained
      conflict <-
        maybe
          (assertFailure "alternate Start did not conflict")
          pure
          (prepareConflictingConformanceRequest request alternateOracleCommand)
      conflicted <- submitConflictingConformanceRequest client conflict
      assertEqual "conflicting reuse closes its lane" ConformanceConflictConnectionClosed conflicted
      fresh <- prepareConformanceRequest client (Just (controlIndex 1)) alternateOracleCommand
      freshReceipt <- submitConformanceRequest client fresh
      assertEqual "fresh command advances control once" (controlIndex 2) (oracleReceiptControlIndex freshReceipt)
      watched <- watchConformanceEntries client (controlIndex 0)
      assertEqual "watch omits no-op, retry, and conflict" [controlIndex 1, controlIndex 2] (fmap appliedEntryControlIndex (NonEmpty.toList watched))
      resumed <- watchConformanceEntries client (controlIndex 1)
      assertEqual "exclusive watch cursor resumes at the next entry" [controlIndex 2] (fmap appliedEntryControlIndex (NonEmpty.toList resumed))
      recordings <- awaitRecordings cluster $ \observed ->
        let events = recordingFor leader observed
         in hasEntryAcknowledgement (raftLogIndex 5) events
              && RuntimeWatchEntryPublished (controlIndex 2) `elem` events
              && RuntimeRaftCheckpointOfferObserved (raftLogIndex 5) (Just (raftLogIndex 5)) `elem` events
      let events = recordingFor leader recordings
          acknowledged =
            [ index
            | RuntimeRaftInputInstalled input <- events,
              AcknowledgeCommittedEntriesView index <- [raftInputView input]
            ]
      assertEqual
        "the adapter acknowledges every native position once, including the no-op"
        (fmap raftLogIndex [1, 2, 3, 4, 5])
        acknowledged
      mapM_
        ( \index -> do
            assertBool ("ordinary application committed at " <> show index) (hasCommittedProposalAt index events)
            assertBool ("adapter acknowledged " <> show index) (hasEntryAcknowledgement index events)
        )
        (fmap raftLogIndex [2, 3, 4, 5])
      let offers = [index | RuntimeRaftInputInstalled input <- events, CheckpointApplicationView index _ <- [raftInputView input]]
          observedOffers = [(offered, retainedIndex) | RuntimeRaftCheckpointOfferObserved offered retainedIndex <- events]
      assertEqual "the dispatcher observes the actual checkpoint after every offer" (fmap (\index -> (index, Just index)) offers) observedOffers
      assertBool "checkpoint adoption is observed after its input was installed" (all (\(offered, _) -> eventPosition (isCheckpointOffer offered) events < eventPosition (== RuntimeRaftCheckpointOfferObserved offered (Just offered)) events) observedOffers)
      assertBool "all progress used local inputs only" (not (any isIncomingPeerRpc events))
      mapM_
        ( \index -> do
            assertEqual "entry retained exactly once" 1 (countEvent (== RuntimeWatchEntryRetained index) events)
            assertEqual "entry published exactly once" 1 (countEvent (== RuntimeWatchEntryPublished index) events)
        )
        (fmap controlIndex [1, 2])
      assertLedgerAcknowledgementOrder leader events
      assertOwnerHandoffOrder leader events
  either (assertFailure . show) pure composed
  where
    isCheckpointOffer expected = \case
      RuntimeRaftInputInstalled input -> case raftInputView input of
        CheckpointApplicationView index _ -> index == expected
        _ -> False
      _ -> False
    isIncomingPeerRpc = \case
      RuntimeRaftInputInstalled input -> case raftInputView input of
        ObserveRequestView {} -> True
        ObserveResponseView {} -> True
        _ -> False
      _ -> False

caseThreeVoterVertical :: IO ()
caseThreeVoterVertical = do
  composed <-
    withOracleTcpCluster fixtureRecordedTcpConfiguration $ \cluster -> do
      firstLeader <- awaitReadyLeader cluster
      assertEqual "the deterministic first leader is R1" fixtureInitialLeaderNode firstLeader
      client <-
        newConformanceClient
          fixtureCheckedGenesis
          fixtureHeraldId
          fixtureHeraldEpoch
          cluster
          >>= either (assertFailure . show) pure
      request <- prepareConformanceRequest client (Just (controlIndex 0)) validOracleCommand
      handed <- submitConformanceRequestWithoutReply client fixtureFollowerNode request
      assertBool "the exact request was handed off after a follower redirect" handed

      let survivors = [fixtureReplacementNode, fixtureFollowerNode]
      beforeStop <- awaitNodesAppliedPrefix cluster survivors 1
      assertNodesAtControl
        "R2 and R3 applied Start one before R1 stops"
        survivors
        1
        beforeStop
      beforeStopRecordings <-
        awaitRecordings cluster $ \recordings ->
          hasCommittedProposalAt (raftLogIndex 2) (recordingFor firstLeader recordings)
            && all
              (\node -> hasEntryAcknowledgement (raftLogIndex 2) (recordingFor node recordings))
              (firstLeader : survivors)
            && hasLeaderNoOpAt (raftLogIndex 1) (recordingFor fixtureFollowerNode recordings)
      assertInitialSchedule firstLeader survivors beforeStopRecordings

      stopped <- stopOracleTcpNode cluster firstLeader
      assertBool "the current leader stopped" stopped
      replacement <- awaitReadyLeader cluster
      assertEqual "the deterministic replacement leader is R2" fixtureReplacementNode replacement

      recovered <- queryConformanceRequest client request
      receipt <- case recovered of
        ConformanceRequestFound found -> pure found
        other -> assertFailure ("expected retained receipt after failover, got " <> show other)
      assertEqual
        "recovered receipt names control one"
        (controlIndex 1)
        (oracleReceiptControlIndex receipt)

      duplicate <- submitConformanceRequest client request
      assertEqual "exact duplicate returns byte-identical semantic receipt" receipt duplicate
      conflict <-
        maybe
          (assertFailure "alternate Start command was not conflicting")
          pure
          (prepareConflictingConformanceRequest request alternateOracleCommand)
      conflictResult <- submitConflictingConformanceRequest client conflict
      assertEqual
        "committed conflicting reuse closes its EORC lane"
        ConformanceConflictConnectionClosed
        conflictResult

      statuses <- awaitNodesAppliedPrefix cluster survivors 1
      assertNodesAtControl
        "duplicate and conflict create no second control entry"
        survivors
        1
        statuses
      recordings <-
        awaitRecordings cluster $ \observed ->
          hasCommittedProposalAt (raftLogIndex 4) (recordingFor replacement observed)
            && hasCommittedProposalAt (raftLogIndex 5) (recordingFor replacement observed)
            && all
              ( \node ->
                  let events = recordingFor node observed
                   in hasEntryAcknowledgement (raftLogIndex 4) events
                        && hasEntryAcknowledgement (raftLogIndex 5) events
              )
              survivors
            && hasLeaderNoOpAt (raftLogIndex 3) (recordingFor fixtureFollowerNode observed)
      assertExactRuntimeSchedule firstLeader replacement survivors recordings

      fresh <- prepareConformanceRequest client (Just (controlIndex 1)) alternateOracleCommand
      freshReceipt <- submitConformanceRequest client fresh
      assertEqual
        "surviving majority advances to control two"
        (controlIndex 2)
        (oracleReceiptControlIndex freshReceipt)
      watched <- watchConformanceEntries client (controlIndex 1)
      let startedEntry = NonEmpty.head watched
      assertEqual
        "TCP watch resumes from the exact exclusive cursor"
        (controlIndex 2)
        (appliedEntryControlIndex startedEntry)
      case fmap oracleProjectionEventView (appliedEntryProjectionEvents startedEntry) of
        [ProcessStartedView record] -> do
          assertEqual
            "the runtime projects the dynamic process identity"
            (processStartProcessId alternateStart)
            (processRecordProcessId record)
          assertEqual
            "the runtime projects the dynamic process epoch"
            (processStartProcessEpochId alternateStart)
            (processRecordProcessEpoch record)
          assertEqual
            "the runtime projects the dynamic residence"
            (processStartResidence alternateStart)
            (processRecordResidence record)
          assertEqual
            "the projected process fact carries control two and its stable lifecycle request"
            ( DynamicStartView
                (controlIndex 2)
                (conformanceRequestId fresh)
            )
            (processRecordOrigin record)
        events -> assertFailure ("the accepted alternate Start carried unexpected projection events: " <> show events)
  case composed of
    Left failure -> assertFailure (show failure)
    Right () -> pure ()

assertNodesAtControl ::
  String ->
  [RaftNodeId] ->
  Word64 ->
  [(RaftNodeId, OracleRuntimeStatus)] ->
  IO ()
assertNodesAtControl context nodes expected statuses =
  let byNode = Map.fromList statuses
   in mapM_
        ( \node ->
            case Map.lookup node byNode of
              Nothing -> assertFailure (context <> ": missing node " <> show node)
              Just status ->
                assertEqual
                  (context <> " at " <> show node)
                  (controlIndex expected)
                  (oracleRuntimeAppliedControlIndex status)
        )
        nodes

awaitRecordings ::
  OracleTcpCluster ->
  (Map RaftNodeId [OracleRuntimeEvent] -> Bool) ->
  IO (Map RaftNodeId [OracleRuntimeEvent])
awaitRecordings cluster ready = do
  observed <- timeout 5000000 loop
  maybe (assertFailure "timed out waiting for exact runtime recording evidence") pure observed
  where
    loop = do
      recordings <- Map.fromList <$> oracleTcpNodeRecordings cluster
      if ready recordings
        then pure recordings
        else threadDelay 10000 >> loop

recordingFor :: RaftNodeId -> Map RaftNodeId [OracleRuntimeEvent] -> [OracleRuntimeEvent]
recordingFor node recordings = Map.findWithDefault [] node recordings

assertInitialSchedule ::
  RaftNodeId ->
  [RaftNodeId] ->
  Map RaftNodeId [OracleRuntimeEvent] ->
  IO ()
assertInitialSchedule leader survivors recordings = do
  assertBool
    "R1 installed a granted vote response over the duplex ERFT route"
    (hasGrantedVoteResponse (recordingFor leader recordings))
  mapM_
    ( \node ->
        assertBool
          ("the initial vote request reached " <> show node)
          (hasVoteRequestFrom leader (recordingFor node recordings))
    )
    survivors
  assertBool
    "R1 committed the first semantic proposal exactly at Raft log 2"
    (hasCommittedProposalAt (raftLogIndex 2) (recordingFor leader recordings))
  assertBool
    "R3 observed R1's current-term no-op at Raft log 1"
    (hasLeaderNoOpAt (raftLogIndex 1) (recordingFor fixtureFollowerNode recordings))
  mapM_
    ( \node -> do
        let events = recordingFor node recordings
        assertBool
          ("application log 2 was acknowledged at " <> show node)
          (hasEntryAcknowledgement (raftLogIndex 2) events)
        assertEqual
          ("control one was retained once before failover at " <> show node)
          1
          (countEvent (== RuntimeWatchEntryRetained (controlIndex 1)) events)
        assertEqual
          ("control one was published once before failover at " <> show node)
          1
          (countEvent (== RuntimeWatchEntryPublished (controlIndex 1)) events)
        assertLedgerAcknowledgementOrder node events
    )
    (leader : survivors)

assertExactRuntimeSchedule ::
  RaftNodeId ->
  RaftNodeId ->
  [RaftNodeId] ->
  Map RaftNodeId [OracleRuntimeEvent] ->
  IO ()
assertExactRuntimeSchedule firstLeader replacement survivors recordings = do
  assertBool
    "the first Start occupied Raft log 2"
    (hasCommittedProposalAt (raftLogIndex 2) (recordingFor firstLeader recordings))
  assertBool
    "R3 observed the replacement leader's current-term no-op at Raft log 3"
    (hasLeaderNoOpAt (raftLogIndex 3) (recordingFor fixtureFollowerNode recordings))
  let replacementEvents = recordingFor replacement recordings
  assertBool
    "the exact duplicate occupied and committed Raft log 4"
    (hasCommittedProposalAt (raftLogIndex 4) replacementEvents)
  assertBool
    "the conflicting reuse occupied and committed Raft log 5"
    (hasCommittedProposalAt (raftLogIndex 5) replacementEvents)
  mapM_
    ( \node -> do
        let events = recordingFor node recordings
        mapM_
          ( \index ->
              assertBool
                ("adapter acknowledged " <> show index <> " at " <> show node)
                (hasEntryAcknowledgement index events)
          )
          [raftLogIndex 2, raftLogIndex 4, raftLogIndex 5]
        assertEqual
          ("only one control entry was retained at " <> show node)
          1
          (countEvent (== RuntimeWatchEntryRetained (controlIndex 1)) events)
        assertEqual
          ("only one control entry was published at " <> show node)
          1
          (countEvent (== RuntimeWatchEntryPublished (controlIndex 1)) events)
        assertLedgerAcknowledgementOrder node events
        assertOwnerHandoffOrder node events
    )
    survivors

assertLedgerAcknowledgementOrder :: RaftNodeId -> [OracleRuntimeEvent] -> IO ()
assertLedgerAcknowledgementOrder node events =
  case ( eventPosition (== RuntimeWatchEntryRetained (controlIndex 1)) events,
         eventPosition (isEntryAcknowledgement (raftLogIndex 2)) events,
         eventPosition (== RuntimeWatchEntryPublished (controlIndex 1)) events
       ) of
    (Just retained, Just acknowledged, Just published) ->
      assertBool
        ("ledger retention, Raft acknowledgement, and exposure order at " <> show node)
        (retained < acknowledged && acknowledged < published)
    _ -> assertFailure ("missing ledger ordering evidence at " <> show node)

assertOwnerHandoffOrder :: RaftNodeId -> [OracleRuntimeEvent] -> IO ()
assertOwnerHandoffOrder node events = do
  assertBool
    ("every Raft input is installed before its whole batch at " <> show node)
    (raftHandoffsOrdered events)
  assertBool
    ("every Oracle input is installed before its whole batch at " <> show node)
    (oracleHandoffsOrdered events)

hasCommittedProposalAt :: RaftLogIndex -> [OracleRuntimeEvent] -> Bool
hasCommittedProposalAt expected =
  any $ \case
    RuntimeProposalObserved _ (RaftProposalCommitted actual) -> actual == expected
    _ -> False

hasEntryAcknowledgement :: RaftLogIndex -> [OracleRuntimeEvent] -> Bool
hasEntryAcknowledgement expected = any (isEntryAcknowledgement expected)

isEntryAcknowledgement :: RaftLogIndex -> OracleRuntimeEvent -> Bool
isEntryAcknowledgement expected = \case
  RuntimeRaftInputInstalled input ->
    case raftInputView input of
      AcknowledgeCommittedEntriesView actual -> actual == expected
      _ -> False
  _ -> False

hasLeaderNoOpAt :: RaftLogIndex -> [OracleRuntimeEvent] -> Bool
hasLeaderNoOpAt expected =
  any $ \case
    RuntimeRaftInputInstalled input ->
      case raftInputView input of
        ObserveRequestView _ rpc ->
          case raftRpcView rpc of
            AppendEntriesView _ _ _ _ entries _ ->
              any
                ( \entry ->
                    raftLogEntryIndex entry == expected
                      && raftLogEntryPayload entry == LeaderNoOp
                )
                entries
            _ -> False
        _ -> False
    _ -> False

hasVoteRequestFrom :: RaftNodeId -> [OracleRuntimeEvent] -> Bool
hasVoteRequestFrom expected =
  any $ \case
    RuntimeRaftInputInstalled input ->
      case raftInputView input of
        ObserveRequestView source rpc
          | source == expected -> case raftRpcView rpc of
              RequestVoteView _ candidate _ _ -> candidate == expected
              _ -> False
        _ -> False
    _ -> False

hasGrantedVoteResponse :: [OracleRuntimeEvent] -> Bool
hasGrantedVoteResponse =
  any $ \case
    RuntimeRaftInputInstalled input ->
      case raftInputView input of
        ObserveResponseView source _ rpc -> case raftRpcView rpc of
          RequestVoteResponseView _ voter granted ->
            granted && voter == source
          _ -> False
        _ -> False
    _ -> False

countEvent :: (OracleRuntimeEvent -> Bool) -> [OracleRuntimeEvent] -> Int
countEvent matches = length . filter matches

eventPosition :: (OracleRuntimeEvent -> Bool) -> [OracleRuntimeEvent] -> Maybe Int
eventPosition = findIndex

raftHandoffsOrdered :: [OracleRuntimeEvent] -> Bool
raftHandoffsOrdered = go . foldr retain []
  where
    retain event accumulated = case event of
      RuntimeRaftInputInstalled {} -> event : accumulated
      RuntimeRaftBatchHanded {} -> event : accumulated
      _ -> accumulated

    go [] = True
    go (RuntimeRaftBatchHanded {} : remaining) = go remaining
    go (RuntimeRaftInputInstalled {} : RuntimeRaftBatchHanded {} : remaining) = go remaining
    go _ = False

oracleHandoffsOrdered :: [OracleRuntimeEvent] -> Bool
oracleHandoffsOrdered = go . foldr retain []
  where
    retain event accumulated = case event of
      RuntimeOracleInputInstalled {} -> event : accumulated
      RuntimeOracleBatchHanded {} -> event : accumulated
      _ -> accumulated

    go [] = True
    go (RuntimeOracleInputInstalled {} : RuntimeOracleBatchHanded {} : remaining) = go remaining
    go _ = False
