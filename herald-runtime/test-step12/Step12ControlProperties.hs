{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Step12ControlProperties (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Concurrent.STM
  ( TVar,
    atomically,
    check,
    isEmptyTQueue,
    modifyTVar',
    newTVarIO,
    readTVar,
    retry,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (finally)
import Control.Monad (forM_, replicateM_, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (findIndex)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Identity (ControlIndex, controlIndex)
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.ProcessStart (processStartResidence)
import Eclips.Domain.Sort.Profile (PredefinedSortRole (NeutralVertexRole))
import Eclips.Herald.Administration
  ( AdminCorrelationId,
    adminCorrelationId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (RunOracleClientAction),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (OracleInput),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction (..),
    OracleClientDiagnostic (StaleOracleClientIngress),
    OracleClientIngress
      ( OracleBindingLost,
        OracleConnectFailed,
        OracleEntriesReceived,
        OracleHelloReceived,
        OracleRetryElapsed
      ),
    OracleConnectAttempt,
    OracleContactSet,
    OracleRetry,
    OracleRetryPurpose (OracleConnectionRetry),
    oracleCandidateLane,
    oracleConnectAttemptContact,
    oracleConnectAttemptFromExclusive,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient qualified as Client
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    awaitHeraldRuntimeExit,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    configureHeraldRuntimeOracleActionSink,
    configureHeraldRuntimeOracleHealthSink,
    configureHeraldRuntimeOracleVoters,
    heraldRuntimeConfiguration,
    requestHeraldRuntimeDrain,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (LaneOffered),
    heraldRuntimeHandlers,
    runtimeAdministrationConnectionHandlers,
    runtimeConfiguredAdministrationConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeAdministrationRegistration (..),
    RuntimeRegistration (..),
    RuntimeSubmission (Queued),
    registerAdministrationConnection,
    registerConfiguredAdministrationConnection,
    submitConfiguredAdministrationDto,
    submitDisappearanceProbeAbort,
    submitOracleClientIngress,
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (batchEnvelopeEffects),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    startHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent),
    TraceLedger,
    snapshotTraceLedger,
  )
import Eclips.Herald.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Herald.Runtime.TCP
  ( HeraldTcpFailure (HeraldTcpScopeFailure),
    PeerDialRetryDelay,
    peerDialRetryDelayMicroseconds,
  )
import Eclips.Herald.Runtime.TCP.Internal.Oracle (runOracleManager)
import Eclips.Herald.Runtime.TCP.Internal.OracleHealth (runOracleHealthManager)
import Eclips.Herald.Runtime.TCP.Internal.Scope
  ( newTcpContext,
    spawnTcpChild,
    stopTcpContext,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( OracleManagerProbeEvent (..),
    TcpContext (..),
    TcpTrackedThread (..),
  )
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (HeraldRuntimeDrained))
import Eclips.Oracle.Canonical
  ( canonicalAppliedOracleEntryValue,
    canonicalOracleEnvelopeValue,
  )
import Eclips.Oracle.Command qualified as OracleCommand
import Eclips.Oracle.Disappearance qualified as OracleDisappearance
import Eclips.Oracle.Identity (OracleClientRequestId, oracleClientRequestId)
import Eclips.Oracle.Progress qualified as Progress
import Eclips.Oracle.Projection
  ( AppliedOracleEntry,
    OracleProjectionEventView (ProcessStartedView),
    ProcessOriginView (DynamicStartView),
    appliedEntryCommand,
    appliedEntryControlIndex,
    appliedEntryPostStateDigest,
    appliedEntryProjectionEvents,
    appliedEntryReceipt,
    oracleProjectionEventView,
    processRecordOrigin,
  )
import Eclips.Oracle.Receipt
  ( OracleReceipt,
    OracleRequestRetirement (..),
    oracleReceiptControlIndex,
    oracleReceiptDisappearanceResult,
    oracleReceiptRequestId,
  )
import Eclips.Oracle.Runtime
  ( OracleRuntimeEvent (..),
    OracleRuntimeStatus,
    oracleRuntimeAppliedControlIndex,
  )
import Eclips.Oracle.Runtime.ConformanceClient
  ( ConformanceClient,
    ConformanceConflictResult (ConformanceConflictConnectionClosed),
    ConformanceRequestResult (ConformanceRequestFound, ConformanceRequestRetired),
    ConformanceSubmissionDisposition (ConformanceSubmissionRetired),
    conformanceRequestFromCanonical,
    newConformanceClient,
    prepareConflictingConformanceRequest,
    prepareConformanceRequest,
    queryConformanceRequest,
    submitConflictingConformanceRequest,
    submitConformanceRequest,
    submitConformanceRequestDispositionVia,
    submitConformanceRequestWithoutReply,
    watchConformanceEntries,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    oracleTcpNodeRecordings,
    oracleTcpNodeStatuses,
    stopOracleTcpNode,
    withOracleTcpCluster,
  )
import Eclips.Oracle.State qualified as OracleState
import Eclips.Oracle.Transition qualified as OracleTransition
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Admin.Types qualified as Admin
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Effect (RaftProposalStatus (RaftProposalCommitted))
import Eclips.Raft.Identity
  ( RaftLogIndex,
    RaftNodeId,
    raftLogIndex,
    raftNodeIdBytes,
  )
import Eclips.Raft.Input
  ( RaftEntry (LeaderNoOp),
    RaftInputView (..),
    RaftRpcView (AppendEntriesView),
    raftInputView,
    raftLogEntryIndex,
    raftLogEntryPayload,
    raftRpcView,
  )
import RuntimeFixtures
  ( fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeedSource,
  )
import Step12Fixtures
  ( awaitStep12AppliedPrefix,
    awaitStep12ReadyLeader,
    step12AlternateOracleCommand,
    step12CheckedOracleGenesis,
    step12FirstContactFollowerNode,
    step12H1Bootstraps,
    step12H1GeneratorSeedSource,
    step12H1Genesis,
    step12H2Bootstraps,
    step12H2GeneratorSeedSource,
    step12H2Genesis,
    step12H3Bootstraps,
    step12H3GeneratorSeedSource,
    step12H3Genesis,
    step12H4Bootstraps,
    step12H4GeneratorSeedSource,
    step12H4Genesis,
    step12InitialLeaderNode,
    step12OracleContacts,
    step12OracleTcpConfiguration,
    step12RecordedOracleTcpConfiguration,
    step12ReplacementLeaderNode,
    step12StartedProcess,
    step12SubmissionFollowerNode,
    step12SubmittingHeraldEpoch,
    step12SubmittingHeraldId,
    step12SystemId,
    step12ValidOracleCommand,
    withReservedUnavailableOracleContacts,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Step 13 Increment 1 Herald process watch"
    [ testCase
        "non-voter H4 follows a fresh follower redirect, coalesces physical loss, and reconnects from the exact cursor"
        caseH4RedirectWatchReconnect,
      testCase
        "live disappearance intents bound physical Oracle contact work and one retained-request repair"
        caseDisappearanceOracleAttemptBound,
      testCase
        "checked unavailable contacts leave an active retry that drain joins"
        caseUnavailableContactsDrainRetry,
      testCase
        "scope stop wakes an Oracle candidate parked before binding"
        caseCandidateScopeStopWakes,
      testCase
        "a missing physical Oracle lane reports one binding loss under repeated actions"
        caseMissingPhysicalOracleLane,
      testCase
        "the Herald releases a settled administration receipt through the production Oracle maintenance lane"
        caseHeraldReceiptRetirement,
      testGroup
        "Step13Increment1Acceptance"
        [ testCase
            "four Heralds converge on one Start process entry across Oracle leader loss"
            caseFourHeraldProcessProjection
        ]
    ]

caseHeraldReceiptRetirement :: Assertion
caseHeraldReceiptRetirement = do
  composed <- withOracleTcpCluster step12RecordedOracleTcpConfiguration $ \cluster -> do
    leader <- awaitStep12ReadyLeader cluster
    withOracleHarness (h2RuntimeConfiguration (step12OracleContacts cluster)) (checkedRetryDelay 10_000) $ \harness -> do
      awaitHarnessWatchAt (controlIndex 0) harness
      registration <-
        registerConfiguredAdministrationConnection
          (harnessRuntime harness)
          (runtimeConfiguredAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
      administration <- case registration of
        ConnectionRegistered reference -> pure reference
        RegistrationStopped -> assertFailure "configured administration registration stopped"
      deployment <- either (assertFailure . show) pure (Admin.adminDeploymentIdClaim (Identity.systemIdBytes step12SystemId))
      unknownProcess <- either (assertFailure . show) pure (Admin.adminProcessEpochIdClaim (ByteString.replicate 32 0xf1))
      assertEqual "the configured administrator is authorized" Queued
        =<< submitConfiguredAdministrationDto (harnessRuntime harness) administration (Admin.AdminHello deployment Admin.ProcessAdministrator)
      assertEqual "End enters the actual Herald owner" Queued
        =<< submitConfiguredAdministrationDto
          (harnessRuntime harness)
          administration
          (Admin.EndProcessEpoch (Admin.adminCorrelationIdClaim 19_001) unknownProcess Admin.AdminExplicitAdministrativeEnd)
      actions <- awaitActions "the settled End creates receipt maintenance" harness (any (\case SubmitOracleProgress {} -> True; _ -> False))
      canonical <- case [Client.oracleRequestDispatchEnvelope dispatch | SubmitOracleRequest _ dispatch <- actions] of
        first : _ -> pure first
        [] -> assertFailure "receipt retirement had no owner-authored request"
      let requestId = OracleCommand.oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical)
      awaitHarnessKernelInput
        "the production maintenance confirmation returns to the Herald owner"
        harness
        (\case Client.OracleProgressConfirmed _ observed through -> observed == requestId && Progress.oracleProgressReceipts through == Lifetime.receiptRetirementPrefix (Just 1); _ -> False)
      -- Result queries may be answered by a follower. Waiting for any replica
      -- permits the fresh client to observe a receipt before that follower has
      -- applied its retirement, despite the leader's confirmation above.
      let awaitRetirementEverywhere = do
            statuses <- oracleTcpNodeStatuses cluster
            if not (null statuses) && all ((== controlIndex 2) . oracleRuntimeAppliedControlIndex . snd) statuses
              then pure ()
              else threadDelay 10_000 >> awaitRetirementEverywhere
      observedRetirement <- timeout 5_000_000 awaitRetirementEverywhere
      assertEqual "every query-serving replica applies receipt retirement" (Just ()) observedRetirement
      client <- newConformanceClient step12CheckedOracleGenesis step12SubmittingHeraldId step12SubmittingHeraldEpoch cluster >>= either (assertFailure . show) pure
      let request = conformanceRequestFromCanonical canonical
      result <- queryConformanceRequest client request
      assertEqual "the actual Herald acknowledgement retires the Oracle query result" (ConformanceRequestRetired (OracleRequestPrefixRetired 1)) result
      late <- submitConformanceRequestDispositionVia client leader request
      assertEqual "late exact dispatch stays retired" (Just (ConformanceSubmissionRetired (OracleRequestPrefixRetired 1))) late
      statuses <- oracleTcpNodeStatuses cluster
      assertBool "late dispatch does not recreate a semantic control entry" (all ((== controlIndex 2) . oracleRuntimeAppliedControlIndex . snd) statuses)
      assertHarnessStillLive "the settled receipt remains locally usable" harness
  either (assertFailure . show) pure composed

caseFourHeraldProcessProjection :: Assertion
caseFourHeraldProcessProjection = do
  composed <-
    withOracleTcpCluster step12RecordedOracleTcpConfiguration $ \cluster -> do
      let contacts = step12OracleContacts cluster
          retryDelay = checkedRetryDelay 10_000
      withOracleHarness (h1RuntimeConfiguration contacts) retryDelay $ \h1 ->
        withOracleHarness (h2RuntimeConfiguration contacts) retryDelay $ \h2 ->
          withOracleHarness (h3RuntimeConfiguration contacts) retryDelay $ \h3 ->
            withHeldOracleWatchHarness (h4RuntimeConfiguration contacts) retryDelay $ \h4 -> do
              schedule <- runFourHeraldOracleSchedule cluster h1 h2 h3 h4
              assertStartProcessProjection schedule h1 h2 h3 h4
              drainHarness (adminCorrelationId 12_704) h4
              drainHarness (adminCorrelationId 12_703) h3
              drainHarness (adminCorrelationId 12_702) h2
              drainHarness (adminCorrelationId 12_701) h1
  either (assertFailure . show) pure composed

data FourHeraldOracleSchedule
  = FourHeraldOracleSchedule ConformanceClient OracleReceipt OracleBinding

runFourHeraldOracleSchedule ::
  OracleTcpCluster ->
  OracleHarness ->
  OracleHarness ->
  OracleHarness ->
  OracleHarness ->
  IO FourHeraldOracleSchedule
runFourHeraldOracleSchedule cluster h1 h2 h3 h4 = do
  leader <- awaitStep12ReadyLeader cluster
  assertEqual "the deterministic first leader is R1" step12InitialLeaderNode leader
  let allNodes = [step12InitialLeaderNode, step12ReplacementLeaderNode, step12SubmissionFollowerNode]
      survivors = [step12ReplacementLeaderNode, step12SubmissionFollowerNode]
  initialStatuses <- oracleTcpNodeStatuses cluster
  assertNodesAtControl "every Oracle begins at genesis control" allNodes 0 initialStatuses
  mapM_ (awaitHarnessWatchAt (controlIndex 0)) [h1, h2, h3]
  initialH4Binding <- awaitHeldControlZeroWatch h4

  client <-
    newConformanceClient
      step12CheckedOracleGenesis
      step12SubmittingHeraldId
      step12SubmittingHeraldEpoch
      cluster
      >>= either (assertFailure . show) pure
  request <- prepareConformanceRequest client (Just (controlIndex 0)) step12ValidOracleCommand
  handed <- submitConformanceRequestWithoutReply client step12SubmissionFollowerNode request
  assertBool "the exact Start command was handed off through follower R3" handed
  beforeStop <- awaitNodesAtControl cluster allNodes 1
  assertNodesAtControl "all three Oracle replicas applied control one before R1 stopped" allNodes 1 beforeStop
  mapM_ (awaitHarnessWatchAt (controlIndex 1)) [h1, h2, h3]
  assertHarnessHasNoAppliedOracleEntries h4
  beforeStopRecordings <-
    awaitOracleRecordings cluster $ \recordings ->
      hasCommittedProposalAt (raftLogIndex 2) (recordingFor leader recordings)
        && all
          (\node -> hasEntryAcknowledgement (raftLogIndex 2) (recordingFor node recordings))
          allNodes
        && hasLeaderNoOpAt (raftLogIndex 1) (recordingFor step12SubmissionFollowerNode recordings)
  assertInitialOracleSchedule leader allNodes beforeStopRecordings

  h1ConnectCount <- length . connectActions <$> atomically (readTVar (harnessActions h1))
  stopped <- stopOracleTcpNode cluster leader
  assertBool "only the R1 voter/Oracle scope stopped" stopped
  assertHarnessStillLive "R1's independently scoped co-host H1" h1

  replacement <- awaitStep12ReadyLeader cluster
  assertEqual "the deterministic replacement leader is R2" step12ReplacementLeaderNode replacement
  _ <-
    awaitActions
      "H1 reconnect from its retained control-one cursor"
      h1
      ( hasNewConnectAt
          (controlIndex 1)
          h1ConnectCount
      )
  replacementH4Binding <- awaitReplacementHeldControlZeroWatch initialH4Binding h4

  recovered <- queryConformanceRequest client request
  receipt <- case recovered of
    ConformanceRequestFound found -> pure found
    other -> assertFailure ("expected retained receipt after failover, got " <> show other)
  assertEqual "the recovered receipt names control one" (controlIndex 1) (oracleReceiptControlIndex receipt)

  duplicate <- submitConformanceRequest client request
  assertEqual "the exact duplicate returns the byte-identical receipt" receipt duplicate
  conflict <-
    maybe
      (assertFailure "the alternate Start did not conflict with the retained request")
      pure
      (prepareConflictingConformanceRequest request step12AlternateOracleCommand)
  conflictResult <- submitConflictingConformanceRequest client conflict
  assertEqual
    "conflicting request-ID reuse closes its EORC lane"
    ConformanceConflictConnectionClosed
    conflictResult

  survivingStatuses <- awaitNodesAtControl cluster survivors 1
  assertNodesAtControl
    "duplicate and conflict produce no second control entry"
    survivors
    1
    survivingStatuses
  recordings <-
    awaitOracleRecordings cluster $ \observed ->
      hasCommittedProposalAt (raftLogIndex 4) (recordingFor replacement observed)
        && hasCommittedProposalAt (raftLogIndex 5) (recordingFor replacement observed)
        && all
          ( \node ->
              let events = recordingFor node observed
               in hasEntryAcknowledgement (raftLogIndex 4) events
                    && hasEntryAcknowledgement (raftLogIndex 5) events
          )
          survivors
        && hasLeaderNoOpAt (raftLogIndex 3) (recordingFor step12SubmissionFollowerNode observed)
  assertExactOracleSchedule leader replacement survivors recordings
  pure (FourHeraldOracleSchedule client receipt replacementH4Binding)

assertStartProcessProjection ::
  FourHeraldOracleSchedule ->
  OracleHarness ->
  OracleHarness ->
  OracleHarness ->
  OracleHarness ->
  Assertion
assertStartProcessProjection (FourHeraldOracleSchedule client receipt replacementH4Binding) h1 h2 h3 h4 = do
  assertHarnessHasNoAppliedOracleEntries h4
  releasedBinding <- releaseHeldControlZeroWatch h4
  assertEqual
    "the released H4 watch belongs to the replacement R2 lane"
    replacementH4Binding
    releasedBinding
  h4Entries <- awaitHarnessAppliedOracleEntries 1 h4
  case h4Entries of
    [h4Entry] -> do
      h4Command <- maybe (assertFailure "recovered Start must produce a command entry") pure (appliedEntryCommand h4Entry)
      watched <- watchConformanceEntries client (controlIndex 0)
      assertEqual
        "the authoritative watch prefix contains exactly control one"
        [h4Entry]
        (NonEmpty.toList watched)
      assertEqual
        "H4 applied the byte-identical recovered Start receipt"
        receipt
        (appliedEntryReceipt h4Command)
      assertEqual
        "H4's Start entry is control one"
        (controlIndex 1)
        (appliedEntryControlIndex h4Entry)
      case oracleProjectionEventView <$> appliedEntryProjectionEvents h4Entry of
        [ProcessStartedView started] -> case processRecordOrigin started of
          DynamicStartView startIndex requestId -> do
            assertEqual
              "the projected process fact carries its Start control index"
              (controlIndex 1)
              startIndex
            assertEqual
              "H4 projected the exact lifecycle request identity"
              (oracleReceiptRequestId receipt)
              requestId
          origin ->
            assertFailure
              ("the accepted Start entry carried an unexpected origin: " <> show origin)
        events ->
          assertFailure
            ("the accepted Start entry carried unexpected projection events: " <> show events)
      otherEntries <- traverse (awaitHarnessAppliedOracleEntries 1) [h1, h2, h3]
      precedingEntries <- traverse requireOnlyEntry otherEntries
      let allEntries = precedingEntries <> [h4Entry]
      assertBool
        "all four Herald projections retain the same Start post-state digest"
        (all ((== appliedEntryPostStateDigest h4Entry) . appliedEntryPostStateDigest) allEntries)
    other -> assertFailure ("H4 applied an unexpected Oracle entry sequence: " <> show other)
  where
    requireOnlyEntry [entry] = pure entry
    requireOnlyEntry entries = assertFailure ("expected one Herald projection entry, got " <> show entries)

releaseHeldControlZeroWatch :: OracleHarness -> IO OracleBinding
releaseHeldControlZeroWatch harness = do
  released <-
    atomically $ do
      held <- readTVar (harnessHeldOracleWatch harness)
      case held of
        Just action@(WatchOracle binding cursor)
          | cursor == controlIndex 0 -> do
              writeTVar (harnessHeldOracleWatch harness) Nothing
              writeTQueue (harnessTcpContext harness).tcpOracleActions action
              pure (Right binding)
        other -> pure (Left other)
  case released of
    Right binding -> pure binding
    Left other -> assertFailure ("H4 had no releasable control-zero watch: " <> show other)

awaitHarnessAppliedOracleEntries :: Int -> OracleHarness -> IO [AppliedOracleEntry]
awaitHarnessAppliedOracleEntries expected harness = do
  observed <- timeout 5_000_000 loop
  maybe (assertFailure "timed out waiting for the exact Herald Oracle projection prefix") pure observed
  where
    loop = do
      entries <- harnessAppliedOracleEntries harness
      if length entries >= expected
        then pure entries
        else threadDelay 10_000 >> loop

harnessAppliedOracleEntries :: OracleHarness -> IO [AppliedOracleEntry]
harnessAppliedOracleEntries harness = do
  events <- snapshotHarnessEvents harness
  pure
    [ canonicalAppliedOracleEntryValue entry
    | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
      OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
      entry <- NonEmpty.toList entries
    ]

assertNodesAtControl ::
  String ->
  [RaftNodeId] ->
  Word64 ->
  [(RaftNodeId, OracleRuntimeStatus)] ->
  Assertion
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

awaitNodesAtControl ::
  OracleTcpCluster ->
  [RaftNodeId] ->
  Word64 ->
  IO [(RaftNodeId, OracleRuntimeStatus)]
awaitNodesAtControl cluster nodes expected = do
  observed <- timeout 5_000_000 loop
  maybe (assertFailure "timed out waiting for the exact Oracle control prefix") pure observed
  where
    loop = do
      statuses <- oracleTcpNodeStatuses cluster
      let byNode = Map.fromList statuses
          reached node =
            maybe
              False
              ((== controlIndex expected) . oracleRuntimeAppliedControlIndex)
              (Map.lookup node byNode)
      if all reached nodes
        then pure statuses
        else threadDelay 10_000 >> loop

awaitOracleRecordings ::
  OracleTcpCluster ->
  (Map RaftNodeId [OracleRuntimeEvent] -> Bool) ->
  IO (Map RaftNodeId [OracleRuntimeEvent])
awaitOracleRecordings cluster ready = do
  observed <- timeout 5_000_000 loop
  maybe (assertFailure "timed out waiting for exact Oracle runtime recording evidence") pure observed
  where
    loop = do
      recordings <- Map.fromList <$> oracleTcpNodeRecordings cluster
      if ready recordings
        then pure recordings
        else threadDelay 10_000 >> loop

recordingFor :: RaftNodeId -> Map RaftNodeId [OracleRuntimeEvent] -> [OracleRuntimeEvent]
recordingFor node recordings = Map.findWithDefault [] node recordings

assertInitialOracleSchedule ::
  RaftNodeId ->
  [RaftNodeId] ->
  Map RaftNodeId [OracleRuntimeEvent] ->
  Assertion
assertInitialOracleSchedule leader nodes recordings = do
  assertBool
    "R1 committed the first Start at Raft log 2"
    (hasCommittedProposalAt (raftLogIndex 2) (recordingFor leader recordings))
  assertBool
    "R3 observed R1's current-term no-op at Raft log 1"
    (hasLeaderNoOpAt (raftLogIndex 1) (recordingFor step12SubmissionFollowerNode recordings))
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
    nodes

assertExactOracleSchedule ::
  RaftNodeId ->
  RaftNodeId ->
  [RaftNodeId] ->
  Map RaftNodeId [OracleRuntimeEvent] ->
  Assertion
assertExactOracleSchedule firstLeader replacement survivors recordings = do
  assertBool
    "the first Start occupied Raft log 2"
    (hasCommittedProposalAt (raftLogIndex 2) (recordingFor firstLeader recordings))
  assertBool
    "R3 observed R2's current-term no-op at Raft log 3"
    (hasLeaderNoOpAt (raftLogIndex 3) (recordingFor step12SubmissionFollowerNode recordings))
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
  case survivors of
    first : second : _ ->
      assertEqual
        "the surviving Oracle owners installed the same semantic outcomes"
        (oracleInstalledEvents (recordingFor first recordings))
        (oracleInstalledEvents (recordingFor second recordings))
    _ -> assertFailure "the exact surviving Oracle pair was absent"

assertLedgerAcknowledgementOrder :: RaftNodeId -> [OracleRuntimeEvent] -> Assertion
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

assertOwnerHandoffOrder :: RaftNodeId -> [OracleRuntimeEvent] -> Assertion
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

countEvent :: (OracleRuntimeEvent -> Bool) -> [OracleRuntimeEvent] -> Int
countEvent matches = length . filter matches

eventPosition :: (OracleRuntimeEvent -> Bool) -> [OracleRuntimeEvent] -> Maybe Int
eventPosition = findIndex

oracleInstalledEvents :: [OracleRuntimeEvent] -> [OracleRuntimeEvent]
oracleInstalledEvents = filter $ \case
  RuntimeOracleInputInstalled {} -> True
  _ -> False

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

caseH4RedirectWatchReconnect :: Assertion
caseH4RedirectWatchReconnect = do
  composed <-
    withOracleTcpCluster step12OracleTcpConfiguration $ \cluster -> do
      leader <- awaitStep12ReadyLeader cluster
      assertEqual "the deterministic first leader is R1" step12InitialLeaderNode leader
      let contacts = step12OracleContacts cluster
          configuration = h4RuntimeConfiguration contacts
      withOracleHarness configuration (checkedRetryDelay 10_000) $ \harness -> do
        actionsAfterRedirect <-
          awaitActions
            "H4 fresh follower redirect and leader watch"
            harness
            (hasInitialRedirectPath step12FirstContactFollowerNode leader)
        assertRedirectOrder step12FirstContactFollowerNode leader actionsAfterRedirect

        client <-
          newConformanceClient
            step12CheckedOracleGenesis
            step12SubmittingHeraldId
            step12SubmittingHeraldEpoch
            cluster
            >>= either (assertFailure . show) pure
        request <- prepareConformanceRequest client (Just (controlIndex 0)) step12ValidOracleCommand
        _receipt <- submitConformanceRequest client request
        awaitStep12AppliedPrefix cluster 1

        actionsAtControlOne <-
          awaitActions
            "H4 control-one rewatch"
            harness
            ((>= 1) . length . watchesAt (controlIndex 1))
        let firstControlOneWatches = watchesAt (controlIndex 1) actionsAtControlOne
        assertEqual "the first applied entry advances H4's watch cursor once" 1 (length firstControlOneWatches)

        (lostBinding, lostAttempt) <-
          case firstControlOneWatches of
            [binding] ->
              case [attempt | BindOracleConnection attempt current <- actionsAtControlOne, current == binding] of
                [attempt] -> pure (binding, attempt)
                attempts ->
                  assertFailure
                    ( "expected one physical attempt for the current Oracle binding, got "
                        <> show attempts
                    )
            bindings ->
              assertFailure
                ( "expected one current Oracle binding before physical loss, got "
                    <> show bindings
                )

        closeClaimedSockets harness
        awaitOracleBindingLoss harness lostBinding
        assertFinalizedOracleLaneCoalescesActions harness lostAttempt lostBinding
        actionsAfterReconnect <-
          awaitActions
            "H4 exact-cursor reconnect"
            harness
            (hasExactReconnect (controlIndex 1))
        assertExactReconnect actionsAfterReconnect

        events <- snapshotHarnessEvents harness
        assertEqual
          "physical finalization and queued stale actions report the lost binding once"
          [lostBinding]
          [ observed
          | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
            OracleInput (OracleBindingLost observed) <- [inputBody input],
            observed == lostBinding
          ]
        let applied =
              [ entries
              | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
                OracleInput (OracleEntriesReceived _ entries) <- [inputBody input]
              ]
        assertEqual "H4 selects one committed Oracle batch" 1 (length applied)
        case applied of
          [entries] ->
            assertEqual
              "H4 selected control one"
              (controlIndex 1)
              (appliedEntryControlIndex (canonicalAppliedOracleEntryValue (NonEmpty.head entries)))
          _ -> assertFailure "the exact one-batch assertion was contradicted"

        drainHarness (adminCorrelationId 12_601) harness
        -- Joining the TCP scope is the exact completion witness for the
        -- redirected candidate reader and its finalizer. Only then is absence
        -- of a late stale-loss diagnostic a closed-world assertion.
        stopTcpContext (harnessTcpContext harness)
        assertTrackedChildrenJoined harness
        assertRetiredCandidateHasNoLateLoss 1 harness
  either (assertFailure . show) pure composed

-- The measured finite schedule opens O=2 probes and authorizes one Abort per
-- probe. V=3 is the checked Oracle voter-contact set (not report membership).
-- Count actual connect, Submit, and accepted Redirect-frame interpretations separately
-- from owner actions and retained request identities. The contact allowance is
-- one V-sized redirect/contact sweep per opened probe. Thus coefficients are
-- (o0,oO,oV,oX)=(0,1,1,4), with X in {0,1} and X<=O. Closing the first physical
-- Submit lane before its write forces the same retained intention onto a fresh
-- binding; the four-attempt repair allowance is one send and one contact sweep.
caseDisappearanceOracleAttemptBound :: Assertion
caseDisappearanceOracleAttemptBound = do
  baseline <- runDisappearanceAttemptSchedule False
  repaired <- runDisappearanceAttemptSchedule True
  assertEqual "connection loss preserves the two logical Abort intentions" baseline.logicalIntents repaired.logicalIntents
  assertBool
    "one deliberate loss adds at most the explicit four-attempt repair term"
    (repaired.physicalAttempts <= baseline.physicalAttempts + 4)
  assertEqual "the gated first write is the sole extra physical submission" (baseline.submits + 1) repaired.submits

-- These counters are measured only after the runtime drains and the TCP scope
-- joins, so absence of another physical attempt is a closed trace assertion.
data DisappearanceAttemptCounts = DisappearanceAttemptCounts
  { logicalIntents :: Int,
    physicalAttempts :: Int,
    submits :: Int
  }

runDisappearanceAttemptSchedule :: Bool -> IO DisappearanceAttemptCounts
runDisappearanceAttemptSchedule injectLoss = do
  composed <- withOracleTcpCluster step12OracleTcpConfiguration $ \cluster -> do
    leader <- awaitStep12ReadyLeader cluster
    probes <- newTVarIO []
    held <- newEmptyMVar
    release <- newEmptyMVar
    firstSubmit <- newTVarIO True
    let contacts = step12OracleContacts cluster
        voters = NonEmpty.length (Client.oracleContacts contacts)
        configureContext context = atomically $ writeTVar context.tcpOracleManagerProbe $ Just $ \event -> do
          shouldHold <- atomically $ do
            modifyTVar' probes (<> [event])
            case event of
              OraclePhysicalSubmitAttempt {} | injectLoss -> do
                first <- readTVar firstSubmit
                writeTVar firstSubmit False
                pure first
              _ -> pure False
          when shouldHold $ case event of
            OraclePhysicalSubmitAttempt binding _ -> do
              void (tryPutMVar held binding)
              readMVar release
            _ -> pure ()
    assertEqual "the bound uses all checked Oracle voter contacts" 3 voters
    withOracleHarnessContext
      configureContext
      id
      False
      False
      (h4RuntimeConfiguration contacts)
      (checkedRetryDelay 10_000)
      $ \harness -> do
        _ <-
          awaitActions
            "initial follower redirect and leader watch"
            harness
            (hasInitialRedirectPath step12FirstContactFollowerNode leader)
        client <-
          newConformanceClient
            step12CheckedOracleGenesis
            step12SubmittingHeraldId
            step12SubmittingHeraldEpoch
            cluster
            >>= either (assertFailure . show) pure
        initialOracle <- either (assertFailure . show) pure (OracleTransition.initialOracle step12CheckedOracleGenesis)
        forM_ [1 :: Word64, 2] $ \ordinal -> do
          object <- either (assertFailure . show) pure (Identity.mkGlobalObjectId (ByteString.replicate 32 (200 + fromIntegral ordinal)))
          sequenceNumber <- either (assertFailure . show) pure (Identity.mkStructuralSequence ordinal)
          subject <-
            either
              (assertFailure . show)
              pure
              ( Disappearance.controlledPredefinedDisappearanceSubject
                  NeutralVertexRole
                  object
                  (Identity.structuralOccurrenceId step12SubmittingHeraldEpoch sequenceNumber)
                  Nothing
              )
          let membership = OracleState.oracleCurrentMembership initialOracle
              command =
                OracleCommand.openDisappearanceProbeCommand
                  subject
                  (Disappearance.disappearanceSubjectMembershipCoordinate subject membership)
              beforeOpen = 2 * (ordinal - 1)
              openIndex = controlIndex (beforeOpen + 1)
              abortIndex = controlIndex (beforeOpen + 2)
          request <- prepareConformanceRequest client (Just (controlIndex beforeOpen)) command
          receipt <- submitConformanceRequest client request
          probe <- case oracleReceiptDisappearanceResult receipt of
            Just (OracleDisappearance.DisappearanceOpenAccepted result) ->
              case Disappearance.disappearanceOpenResultView result of
                Disappearance.OpenedDisappearanceProbe identifier -> pure identifier
                other -> assertFailure ("expected fresh live Open: " <> show other)
            other -> assertFailure ("expected live disappearance result: " <> show other)
          _ <- awaitActions "live disappearance Open applied by H4" harness (not . null . watchesAt openIndex)
          submission <- submitDisappearanceProbeAbort (harnessRuntime harness) probe
          assertEqual "authorized Abort enters the actual runtime owner" Queued submission
          when (injectLoss && ordinal == 1) $ do
            pending <- timeout 5_000_000 (readMVar held)
            binding <- maybe (assertFailure "timed out at physical Abort send") pure pending
            closeClaimedSockets harness
            awaitOracleBindingLoss harness binding
            void (tryPutMVar release ())
          _ <- awaitActions "retained Abort terminal applied by H4" harness (not . null . watchesAt abortIndex)
          pure ()
        drainHarness (adminCorrelationId (if injectLoss then 16_902 else 16_901)) harness
        stopTcpContext (harnessTcpContext harness)
        assertTrackedChildrenJoined harness
        actions <- atomically (readTVar (harnessActions harness))
        physical <- atomically (readTVar probes)
        events <- snapshotHarnessEvents harness
        let requests =
              [ OracleCommand.oracleEnvelopeRequestId
                  (canonicalOracleEnvelopeValue (Client.oracleRequestDispatchEnvelope dispatch))
              | SubmitOracleRequest _ dispatch <- actions
              ]
            physicalRequests = [request | OraclePhysicalSubmitAttempt _ request <- physical]
            connects = length [() | OraclePhysicalConnectAttempt _ <- physical]
            redirects = length [() | OraclePhysicalRedirect _ <- physical]
            sendCount = length physicalRequests
            total = connects + redirects + sendCount
            losses =
              [ binding
              | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
                OracleInput (OracleBindingLost binding) <- [inputBody input]
              ]
            opened = 2
            repairCount = if injectLoss then 1 else 0
            bound = 0 + opened + opened * voters + 4 * repairCount
        assertEqual "one retained request identity per live Abort" opened (Set.size (Set.fromList requests))
        assertEqual
          "every physical submit belongs to an owner-authored retained intention"
          (Set.fromList requests)
          (Set.fromList physicalRequests)
        assertRedirectOrder step12FirstContactFollowerNode leader actions
        assertEqual
          "the Hello-hint redirect crosses actual follower and leader contacts"
          [raftNodeIdBytes step12FirstContactFollowerNode, raftNodeIdBytes leader]
          (take 2 [Client.oracleNodeClaimBytes (oracleContactNode (oracleConnectAttemptContact attempt)) | OraclePhysicalConnectAttempt attempt <- physical])
        -- A fresh Hello leader hint supplies this redirect; it need not emit an
        -- OracleRedirect frame. Its two physical connects remain in the V term.
        assertEqual "only the deliberately closed established lane is lost" repairCount (length losses)
        assertBool "repair X never exceeds opened probes O" (repairCount <= opened)
        assertBool ("physical attempts satisfy 0 + O + O*V + 4*X: " <> show (connects, sendCount, redirects, bound)) (total <= bound)
        pure (DisappearanceAttemptCounts (Set.size (Set.fromList requests)) total sendCount)
  either (assertFailure . show) pure composed

caseUnavailableContactsDrainRetry :: Assertion
caseUnavailableContactsDrainRetry =
  withReservedUnavailableOracleContacts $ \contacts releaseFirst -> do
    let configuration = fixtureRuntimeConfiguration contacts
    withOracleHarness configuration (checkedRetryDelay 5_000_000) $ \harness -> do
      _ <-
        awaitActions
          "the first reserved unavailable contact attempt"
          harness
          (any isConnectAction)
      -- A bound, non-listening socket leaves connect pending on macOS. Release
      -- only the already-selected first reservation to produce the ordinary
      -- refusal; the other two ports remain reserved for the whole run.
      releaseFirst
      _ <-
        awaitActions
          "unavailable-contact retry"
          harness
          (any isScheduledRetry)
      awaitTrackedChildren harness 3
      drainHarness (adminCorrelationId 12_602) harness
      stopTcpContext (harnessTcpContext harness)
      assertTrackedChildrenJoined harness

caseCandidateScopeStopWakes :: Assertion
caseCandidateScopeStopWakes = do
  composed <-
    withOracleTcpCluster step12OracleTcpConfiguration $ \cluster -> do
      _ <- awaitStep12ReadyLeader cluster
      bindBatchEntered <- newEmptyMVar
      retainBindBatch <- newEmptyMVar
      let holdBindBatch hooks =
            hooks
              { hookBeforeDispatchBatch = \batch ->
                  when (hasOracleBind batch) $ do
                    first <- tryPutMVar bindBatchEntered ()
                    when first (takeMVar retainBindBatch)
              }
          configuration = h4RuntimeConfiguration (step12OracleContacts cluster)
      withOracleHarnessWithHooks
        holdBindBatch
        configuration
        (checkedRetryDelay 10_000)
        $ \harness -> do
          parked <- timeout 2_000_000 (readMVar bindBatchEntered)
          assertBool
            "the Oracle candidate reached its pre-binding disposition wait"
            (parked == Just ())
          stopped <- timeout 2_000_000 (stopTcpContext (harnessTcpContext harness))
          assertBool
            "scope retirement wakes and joins the pre-binding Oracle candidate"
            (stopped == Just ())
          assertTrackedChildrenJoined harness
  either (assertFailure . show) pure composed

hasOracleBind :: BatchEnvelope -> Bool
hasOracleBind =
  any
    (\case RunOracleClientAction BindOracleConnection {} -> True; _ -> False)
    . effectBatchMembers
    . batchEnvelopeEffects

caseMissingPhysicalOracleLane :: Assertion
caseMissingPhysicalOracleLane =
  withReservedUnavailableOracleContacts $ \contacts _ ->
    withMissingOracleLaneHarness
      (fixtureRuntimeConfiguration contacts)
      (checkedRetryDelay 5_000_000)
      $ \harness -> do
        initialActions <-
          awaitActions
            "the retained logical Oracle connect attempt"
            harness
            (any isConnectAction)
        attempt <- case [value | ConnectAndHelloOracle value _ <- initialActions] of
          [value] -> pure value
          values -> assertFailure ("expected one retained Oracle connect attempt, got " <> show values)
        let node = oracleContactNode (oracleConnectAttemptContact attempt)
            acceptance =
              oracleHelloAcceptance
                node
                (oracleObservedTerm 1)
                (controlIndex 0)
                (Just node)
                True
        assertEqual
          "the ready Hello enters the pure Oracle client"
          Queued
          =<< submitOracleClientIngress
            (harnessRuntime harness)
            (OracleHelloReceived attempt acceptance)

        boundActions <-
          awaitActions
            "the logical Oracle binding without a physical lane"
            harness
            (any isBindAction)
        binding <- case [value | BindOracleConnection _ value <- boundActions] of
          [value] -> pure value
          values -> assertFailure ("expected one logical Oracle binding, got " <> show values)
        let repeated =
              [ WatchOracle binding (controlIndex 0),
                ReleaseDeferredOracleSubmission binding missingLaneRequest
              ]
            managerQueue = (harnessTcpContext harness).tcpOracleActions
        atomically $ do
          replicateM_ 4 (mapM_ (writeTQueue managerQueue) repeated)
          -- The diagnostic is a no-op at the physical manager boundary. Once
          -- it has been taken from this FIFO, every preceding missing-lane
          -- action has completed its physical interpretation.
          writeTQueue managerQueue (ReportOracleClientDiagnostic StaleOracleClientIngress)
        drained <-
          timeout
            2_000_000
            ( atomically $ do
                empty <- isEmptyTQueue managerQueue
                check empty
            )
        assertBool "the Oracle manager consumed the missing-lane action suffix" (drained == Just ())

        -- The manager submits every binding-loss ingress before returning to
        -- its FIFO. Expiring the still-current retry after the physical FIFO
        -- drain is therefore a same-source kernel FIFO tail marker behind all
        -- losses which the suffix could have generated.
        retryToken <- awaitActiveOracleRetry "the missing-lane reconnect retry" harness
        assertEqual
          "the post-suffix Oracle retry marker enters the runtime"
          Queued
          =<< submitOracleClientIngress
            (harnessRuntime harness)
            (OracleRetryElapsed retryToken)
        awaitHarnessKernelInput
          "the post-suffix Oracle retry kernel FIFO marker"
          harness
          (== OracleRetryElapsed retryToken)

        actionsAfterLoss <- atomically (readTVar (harnessActions harness))
        events <- snapshotHarnessEvents harness
        let reportedBindings =
              [ observed
              | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
                OracleInput (OracleBindingLost observed) <- [inputBody input]
              ]
            scheduledRetries =
              [ scheduled
              | ScheduleOracleRetry OracleConnectionRetry scheduled <- actionsAfterLoss
              ]
        assertEqual
          "repeated missing-lane actions report the current logical binding once"
          [binding]
          reportedBindings
        assertEqual
          "the one binding-loss input starts one reconnect retry"
          1
          (length scheduledRetries)
        drainHarness (adminCorrelationId 12_603) harness

missingLaneRequest :: OracleClientRequestId
missingLaneRequest = oracleClientRequestId home 12_603
  where
    home = processStartResidence step12StartedProcess

data OracleHarness
  = OracleHarness
      HeraldRuntime
      (RuntimeInternal.ConnectionRef RuntimeInternal.AdministrationPlane)
      TcpContext
      (TVar [OracleClientAction])
      (TVar (Maybe OracleClientAction))
      (MVar TraceLedger)

harnessRuntime :: OracleHarness -> HeraldRuntime
harnessRuntime (OracleHarness runtime _ _ _ _ _) = runtime

harnessAdministration :: OracleHarness -> RuntimeInternal.ConnectionRef RuntimeInternal.AdministrationPlane
harnessAdministration (OracleHarness _ administration _ _ _ _) = administration

harnessTcpContext :: OracleHarness -> TcpContext
harnessTcpContext (OracleHarness _ _ context _ _ _) = context

harnessActions :: OracleHarness -> TVar [OracleClientAction]
harnessActions (OracleHarness _ _ _ actions _ _) = actions

harnessHeldOracleWatch :: OracleHarness -> TVar (Maybe OracleClientAction)
harnessHeldOracleWatch (OracleHarness _ _ _ _ held _) = held

harnessTrace :: OracleHarness -> MVar TraceLedger
harnessTrace (OracleHarness _ _ _ _ _ trace) = trace

withOracleHarness ::
  HeraldRuntimeConfiguration ->
  PeerDialRetryDelay ->
  (OracleHarness -> IO result) ->
  IO result
withOracleHarness = withOracleHarnessGating id False False

withOracleHarnessWithHooks ::
  (RuntimeHooks -> RuntimeHooks) ->
  HeraldRuntimeConfiguration ->
  PeerDialRetryDelay ->
  (OracleHarness -> IO result) ->
  IO result
withOracleHarnessWithHooks configureHooks =
  withOracleHarnessGating configureHooks False False

withHeldOracleWatchHarness ::
  HeraldRuntimeConfiguration ->
  PeerDialRetryDelay ->
  (OracleHarness -> IO result) ->
  IO result
withHeldOracleWatchHarness = withOracleHarnessGating id False True

-- | Keep the owner-authored connect fact but deliberately omit its physical
-- interpretation. The manager therefore receives Bind, Watch, and request
-- actions in their ordinary order with no candidate socket behind them.
withMissingOracleLaneHarness ::
  HeraldRuntimeConfiguration ->
  PeerDialRetryDelay ->
  (OracleHarness -> IO result) ->
  IO result
withMissingOracleLaneHarness = withOracleHarnessGating id True False

withOracleHarnessGating ::
  (RuntimeHooks -> RuntimeHooks) ->
  Bool ->
  Bool ->
  HeraldRuntimeConfiguration ->
  PeerDialRetryDelay ->
  (OracleHarness -> IO result) ->
  IO result
withOracleHarnessGating = withOracleHarnessContext (const (pure ()))

withOracleHarnessContext ::
  (TcpContext -> IO ()) ->
  (RuntimeHooks -> RuntimeHooks) ->
  Bool ->
  Bool ->
  HeraldRuntimeConfiguration ->
  PeerDialRetryDelay ->
  (OracleHarness -> IO result) ->
  IO result
withOracleHarnessContext configureContext configureHooks suppressConnect holdControlZeroWatch baseConfiguration retryDelay use = do
  context <- newTcpContext
  configureContext context
  actions <- newTVarIO []
  heldWatch <- newTVarIO Nothing
  trace <- newEmptyMVar
  let actionSink action =
        atomically $ do
          modifyTVar' actions (<> [action])
          if suppressConnect && isConnectAction action
            then pure ()
            else
              if holdControlZeroWatch && isControlZeroWatch action
                then writeTVar heldWatch (Just action)
                else writeTQueue context.tcpOracleActions action
      configuration =
        configureHeraldRuntimeOracleActionSink actionSink
          $ configureHeraldRuntimeOracleHealthSink
            (\roundNumber cadence claims contacts -> atomically (writeTQueue context.tcpOracleHealthRounds (roundNumber, cadence, claims, contacts)))
            baseConfiguration
      hooks =
        configureHooks
          defaultRuntimeHooks
            { hookRuntimeInitialized = \_ _ _ _ _ _ ledger ->
                void (tryPutMVar trace ledger)
            }
  started <- startHeraldRuntimeWithHooks hooks configuration
  case started of
    Left failure -> do
      stopTcpContext context
      assertFailure ("failed to start observed Herald runtime: " <> show failure)
    Right runtime ->
      ( do
          manager <-
            spawnTcpChild
              context
              (Just HeraldTcpScopeFailure)
              (runOracleManager (spawnTcpChild context Nothing) context runtime retryDelay)
          assertBool "the real outbound EORC manager entered the TCP scope" (maybe False (const True) manager)
          healthManager <-
            spawnTcpChild
              context
              (Just HeraldTcpScopeFailure)
              (runOracleHealthManager context runtime)
          assertBool "the independent native Oracle health manager entered the TCP scope" (maybe False (const True) healthManager)
          administration <- registerAdministration runtime
          use (OracleHarness runtime administration context actions heldWatch trace)
      )
        `finally` do
          stopTcpContext context
          RuntimeInternal.runtimeCloseScope runtime

isControlZeroWatch :: OracleClientAction -> Bool
isControlZeroWatch (WatchOracle _ cursor) = cursor == controlIndex 0
isControlZeroWatch _ = False

awaitHeldControlZeroWatch :: OracleHarness -> IO OracleBinding
awaitHeldControlZeroWatch harness = do
  observed <-
    timeout
      5_000_000
      ( atomically $ do
          held <- readTVar (harnessHeldOracleWatch harness)
          case held of
            Just action -> pure action
            Nothing -> retry
      )
  case observed of
    Just action@(WatchOracle binding cursor) -> do
      assertEqual "H4's retained watch starts from control zero" (controlIndex 0) cursor
      assertBool "the retained H4 action is the watch gate" (isControlZeroWatch action)
      pure binding
    Just other -> assertFailure ("H4 retained an unexpected Oracle action: " <> show other)
    Nothing -> assertFailure "timed out waiting for H4's retained control-zero watch"

awaitReplacementHeldControlZeroWatch :: OracleBinding -> OracleHarness -> IO OracleBinding
awaitReplacementHeldControlZeroWatch predecessor harness = do
  observed <-
    timeout
      5_000_000
      ( atomically $ do
          held <- readTVar (harnessHeldOracleWatch harness)
          case held of
            Just (WatchOracle binding cursor)
              | cursor == controlIndex 0,
                binding /= predecessor ->
                  pure binding
            _ -> retry
      )
  case observed of
    Just binding -> pure binding
    Nothing -> do
      held <- atomically (readTVar (harnessHeldOracleWatch harness))
      actions <- atomically (readTVar (harnessActions harness))
      let bindings =
            [ binding
            | BindOracleConnection _ binding <- actions
            ]
          watches =
            [ (binding, cursor)
            | WatchOracle binding cursor <- actions
            ]
          recent = drop (max 0 (length actions - 20)) actions
      assertFailure
        ( "timed out waiting for H4's distinct R2-bound control-zero watch; held="
            <> show held
            <> "; bindings="
            <> show bindings
            <> "; watches="
            <> show watches
            <> "; action-count="
            <> show (length actions)
            <> "; recent-actions="
            <> show recent
        )

awaitHarnessWatchAt :: ControlIndex -> OracleHarness -> Assertion
awaitHarnessWatchAt cursor harness = do
  _ <-
    awaitActions
      ("Herald watch at " <> show cursor)
      harness
      (not . null . watchesAt cursor)
  pure ()

assertHarnessHasNoAppliedOracleEntries :: OracleHarness -> Assertion
assertHarnessHasNoAppliedOracleEntries harness = do
  events <- snapshotHarnessEvents harness
  let applied =
        [ ()
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
          OracleInput (OracleEntriesReceived _ _) <- [inputBody input]
        ]
  assertEqual "H4 remains at control zero while its watch is held" [] applied

assertHarnessStillLive :: String -> OracleHarness -> Assertion
assertHarnessStillLive context harness = do
  observed <- RuntimeInternal.runtimeTryExit (harnessRuntime harness)
  assertEqual (context <> " remains live") Nothing observed

registerAdministration ::
  HeraldRuntime ->
  IO (RuntimeInternal.ConnectionRef RuntimeInternal.AdministrationPlane)
registerAdministration runtime = do
  registered <-
    registerAdministrationConnection
      runtime
      (runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
  case registered of
    AdministrationConnectionRegistered connection -> pure connection
    other -> assertFailure ("failed to register Step-13 Increment-1 administration lane: " <> show other)

drainHarness :: AdminCorrelationId -> OracleHarness -> IO ()
drainHarness correlation harness = do
  submitted <-
    requestHeraldRuntimeDrain
      (harnessRuntime harness)
      (harnessAdministration harness)
      correlation
  assertEqual "the orderly drain entered the runtime" Queued submitted
  observed <- timeout 3_000_000 (awaitHeraldRuntimeExit (harnessRuntime harness))
  assertEqual
    "the Herald reaches its retained drained exit"
    (Just (Right HeraldRuntimeDrained))
    observed

awaitActions ::
  String ->
  OracleHarness ->
  ([OracleClientAction] -> Bool) ->
  IO [OracleClientAction]
awaitActions context harness ready = do
  observed <-
    timeout
      5_000_000
      ( atomically $ do
          actions <- readTVar (harnessActions harness)
          check (ready actions)
          pure actions
      )
  case observed of
    Just actions -> pure actions
    Nothing -> do
      actions <- atomically (readTVar (harnessActions harness))
      assertFailure
        ( "timed out waiting for "
            <> context
            <> "; observed actions: "
            <> show actions
        )

hasInitialRedirectPath :: RaftNodeId -> RaftNodeId -> [OracleClientAction] -> Bool
hasInitialRedirectPath follower leader actions =
  case connectActions actions of
    (firstAttempt, firstNode, firstCursor) : (secondAttempt, secondNode, secondCursor) : _ ->
      firstNode == raftNodeIdBytes follower
        && secondNode == raftNodeIdBytes leader
        && firstCursor == controlIndex 0
        && secondCursor == controlIndex 0
        && hasImmediateRedirectPath firstAttempt secondAttempt actions
        && not (null (watchesAt (controlIndex 0) actions))
    _ -> False

assertRedirectOrder :: RaftNodeId -> RaftNodeId -> [OracleClientAction] -> Assertion
assertRedirectOrder follower leader actions =
  case connectActions actions of
    (firstAttempt, firstNode, firstCursor) : (secondAttempt, secondNode, secondCursor) : _ -> do
      assertEqual "H4's checked first contact is the follower" (raftNodeIdBytes follower) firstNode
      assertEqual "H4 follows the admitted leader hint" (raftNodeIdBytes leader) secondNode
      assertEqual "the follower Hello starts from genesis" (controlIndex 0) firstCursor
      assertEqual "redirect preserves the exact genesis cursor" (controlIndex 0) secondCursor
      assertBool
        "first-term evidence rejects the follower lane and immediately contacts the distinct fixed leader without a retry timer"
        (hasImmediateRedirectPath firstAttempt secondAttempt actions)
      assertBool "H4 establishes a watch from control zero" (not (null (watchesAt (controlIndex 0) actions)))
    _ -> assertFailure "H4 did not expose two ordered connect attempts"

hasImmediateRedirectPath :: OracleConnectAttempt -> OracleConnectAttempt -> [OracleClientAction] -> Bool
hasImmediateRedirectPath rejectedAttempt successorAttempt actions =
  case ( findIndex
           (== RejectOracleConnection (oracleCandidateLane rejectedAttempt))
           actions,
         findIndex (isConnectAttempt successorAttempt) actions
       ) of
    (Just rejectedIndex, Just connectedIndex) ->
      rejectedIndex < connectedIndex
        && not
          ( any
              isScheduledRetry
              (take (connectedIndex - rejectedIndex - 1) (drop (rejectedIndex + 1) actions))
          )
    _ -> False

assertRetiredCandidateHasNoLateLoss :: Int -> OracleHarness -> Assertion
assertRetiredCandidateHasNoLateLoss expectedDiagnostics harness = do
  actions <- atomically (readTVar (harnessActions harness))
  assertEqual
    "retiring the rejected follower socket adds no diagnostic beyond the explicit FIFO marker"
    expectedDiagnostics
    ( length
        [ ()
        | ReportOracleClientDiagnostic StaleOracleClientIngress <- actions
        ]
    )

connectActions :: [OracleClientAction] -> [(OracleConnectAttempt, ByteString, ControlIndex)]
connectActions actions =
  [ (attempt, Client.oracleNodeClaimBytes (oracleContactNode (oracleConnectAttemptContact attempt)), oracleConnectAttemptFromExclusive attempt)
  | ConnectAndHelloOracle attempt _ <- actions
  ]

hasNewConnectAt :: ControlIndex -> Int -> [OracleClientAction] -> Bool
hasNewConnectAt cursor previousCount actions =
  any
    (\(_, _, fromExclusive) -> fromExclusive == cursor)
    (drop previousCount (connectActions actions))

isConnectAttempt :: OracleConnectAttempt -> OracleClientAction -> Bool
isConnectAttempt expected (ConnectAndHelloOracle attempt _) = attempt == expected
isConnectAttempt _ _ = False

watchesAt :: ControlIndex -> [OracleClientAction] -> [OracleBinding]
watchesAt expected actions =
  [binding | WatchOracle binding cursor <- actions, cursor == expected]

hasExactReconnect :: ControlIndex -> [OracleClientAction] -> Bool
hasExactReconnect cursor actions =
  any
    (\(_, _, fromExclusive) -> fromExclusive == cursor)
    (drop 2 (connectActions actions))
    && distinctBindings (watchesAt cursor actions)

assertExactReconnect :: [OracleClientAction] -> Assertion
assertExactReconnect actions = do
  assertBool
    "the replacement Hello carries the exact applied cursor"
    (any (\(_, _, cursor) -> cursor == controlIndex 1) (drop 2 (connectActions actions)))
  case watchesAt (controlIndex 1) actions of
    first : second : _ ->
      assertBool "reconnect establishes a new binding at the retained cursor" (first /= second)
    _ -> assertFailure "H4 did not watch control one across two distinct bindings"

distinctBindings :: [OracleBinding] -> Bool
distinctBindings (first : second : _) = first /= second
distinctBindings _ = False

closeClaimedSockets :: OracleHarness -> IO ()
closeClaimedSockets harness = do
  closes <- atomically (readTVar (harnessTcpContext harness).tcpSocketCloses)
  sequence_ (map snd closes)

awaitOracleBindingLoss :: OracleHarness -> OracleBinding -> IO ()
awaitOracleBindingLoss harness expected =
  awaitHarnessKernelInput
    "the established Oracle socket's binding loss"
    harness
    (\case OracleBindingLost binding -> binding == expected; _ -> False)

-- Close an actually established EORC socket, then replay the owner actions
-- which can already be queued behind its finalizer. The stale Bind is
-- particularly important: it must not renew the missing-report lease when its
-- candidate lane has disappeared. Queue drain is a deterministic manager FIFO
-- barrier; the stale ConnectFailed marker is then a kernel FIFO barrier behind
-- every loss the manager could have submitted.
assertFinalizedOracleLaneCoalescesActions ::
  OracleHarness ->
  OracleConnectAttempt ->
  OracleBinding ->
  Assertion
assertFinalizedOracleLaneCoalescesActions harness attempt binding = do
  let repeated =
        [ WatchOracle binding (controlIndex 1),
          ReleaseDeferredOracleSubmission binding missingLaneRequest
        ]
      managerQueue = (harnessTcpContext harness).tcpOracleActions
  atomically $ do
    writeTQueue managerQueue (BindOracleConnection attempt binding)
    replicateM_ 4 (mapM_ (writeTQueue managerQueue) repeated)
    writeTQueue managerQueue (ReportOracleClientDiagnostic StaleOracleClientIngress)
  drained <-
    timeout
      2_000_000
      ( atomically $ do
          empty <- isEmptyTQueue managerQueue
          check empty
      )
  assertBool "the real Oracle manager consumed the post-finalization action suffix" (drained == Just ())

  assertEqual
    "the stale finalization marker enters behind every manager-authored loss"
    Queued
    =<< submitOracleClientIngress
      (harnessRuntime harness)
      (OracleConnectFailed attempt)
  awaitHarnessKernelInput
    "the post-finalization kernel FIFO marker"
    harness
    (== OracleConnectFailed attempt)

awaitActiveOracleRetry :: String -> OracleHarness -> IO OracleRetry
awaitActiveOracleRetry context harness = do
  actions <- awaitActions context harness (not . null . activeOracleRetries)
  case activeOracleRetries actions of
    [retryToken] -> pure retryToken
    retryTokens -> assertFailure (context <> " expected one active retry, got " <> show retryTokens)

activeOracleRetries :: [OracleClientAction] -> [OracleRetry]
activeOracleRetries actions =
  [ retryToken
  | ScheduleOracleRetry _ retryToken <- actions,
    retryToken `notElem` [cancelled | CancelOracleRetry cancelled <- actions]
  ]

awaitHarnessKernelInput ::
  String ->
  OracleHarness ->
  (Client.OracleClientIngress -> Bool) ->
  IO ()
awaitHarnessKernelInput context harness matches = do
  ledger <- readMVar (harnessTrace harness)
  observed <-
    timeout
      2_000_000
      ( atomically $ do
          events <- snapshotTraceLedger ledger
          check
            ( any
                ( \case
                    KernelEvent _ (KernelTraceStepped _ _ input (Right _)) ->
                      case inputBody input of
                        OracleInput ingress -> matches ingress
                        _ -> False
                    _ -> False
                )
                events
            )
      )
  assertBool context (observed == Just ())

snapshotHarnessEvents :: OracleHarness -> IO [RuntimeTraceEvent]
snapshotHarnessEvents harness = do
  ledger <- readMVar (harnessTrace harness)
  atomically (snapshotTraceLedger ledger)

awaitTrackedChildren :: OracleHarness -> Int -> IO ()
awaitTrackedChildren harness expected = do
  observed <-
    timeout
      2_000_000
      ( atomically $ do
          children <- readTVar (harnessTcpContext harness).tcpThreads
          check (length children >= expected)
      )
  assertBool "the unavailable lane retains its Oracle and health managers plus its retry child" (maybe False (const True) observed)

assertTrackedChildrenJoined :: OracleHarness -> Assertion
assertTrackedChildrenJoined harness = do
  children <- atomically (readTVar (harnessTcpContext harness).tcpThreads)
  completions <- traverse trackedCompletion children
  assertBool "every TCP child has a join witness after drain" (and completions)
  where
    trackedCompletion (TcpTrackedThread _ done) = maybe False (const True) <$> tryReadMVar done

isScheduledRetry :: OracleClientAction -> Bool
isScheduledRetry ScheduleOracleRetry {} = True
isScheduledRetry _ = False

isConnectAction :: OracleClientAction -> Bool
isConnectAction ConnectAndHelloOracle {} = True
isConnectAction _ = False

isBindAction :: OracleClientAction -> Bool
isBindAction BindOracleConnection {} = True
isBindAction _ = False

step12RuntimeConfiguration ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  OracleContactSet ->
  HeraldRuntimeConfiguration
step12RuntimeConfiguration genesis bootstraps seedSource contacts =
  configureHeraldRuntimeOracleVoters
    (Voter.oracleVoterConfiguration initial)
    (Voter.oracleReplicaRegistrations initial)
    $ heraldRuntimeConfiguration
      genesis
      bootstraps
      contacts
      seedSource
      systemRuntimeMonotonicClock
      (runtimePeerWorkDelayMicroseconds 0)
      (heraldRuntimeHandlers (const (pure ())))
      (either (error . show) id (checkApplicationRecoveryConfiguration 30_000_000))
      (either (error . show) id (checkPeerRecoveryConfiguration 30_000_000))
  where
    initial = either (error . show) id (OracleTransition.initialOracle step12CheckedOracleGenesis)

h1RuntimeConfiguration :: OracleContactSet -> HeraldRuntimeConfiguration
h1RuntimeConfiguration =
  step12RuntimeConfiguration
    step12H1Genesis
    step12H1Bootstraps
    step12H1GeneratorSeedSource

h2RuntimeConfiguration :: OracleContactSet -> HeraldRuntimeConfiguration
h2RuntimeConfiguration =
  step12RuntimeConfiguration
    step12H2Genesis
    step12H2Bootstraps
    step12H2GeneratorSeedSource

h3RuntimeConfiguration :: OracleContactSet -> HeraldRuntimeConfiguration
h3RuntimeConfiguration =
  step12RuntimeConfiguration
    step12H3Genesis
    step12H3Bootstraps
    step12H3GeneratorSeedSource

h4RuntimeConfiguration :: OracleContactSet -> HeraldRuntimeConfiguration
h4RuntimeConfiguration =
  step12RuntimeConfiguration
    step12H4Genesis
    step12H4Bootstraps
    step12H4GeneratorSeedSource

fixtureRuntimeConfiguration :: OracleContactSet -> HeraldRuntimeConfiguration
fixtureRuntimeConfiguration contacts =
  heraldRuntimeConfiguration
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    contacts
    fixtureGeneratorSeedSource
    systemRuntimeMonotonicClock
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    (either (error . show) id (checkApplicationRecoveryConfiguration 30_000_000))
    (either (error . show) id (checkPeerRecoveryConfiguration 30_000_000))

checkedRetryDelay :: Word64 -> PeerDialRetryDelay
checkedRetryDelay = either (error . show) id . peerDialRetryDelayMicroseconds
