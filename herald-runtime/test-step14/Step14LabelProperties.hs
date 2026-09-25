{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Step14LabelProperties (tests) where

import Control.Concurrent
  ( forkFinally,
    threadDelay,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    readMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, finally)
import Control.Monad (foldM, forM_, unless, void, when, zipWithM)
import Data.List (nub, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    startupAccessProcess,
  )
import Eclips.Application.Types.Identity (asPrivateObjectId)
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToVoid),
    LabelResult (LabelApplied),
  )
import Eclips.Application.Types.Operation
  ( ApplicationOperation (LabelApplication, WriteApplication),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (LabelCompleted, WriteCompleted),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (ProcessLabel, VoidLabel),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (WriteAccepted),
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    LabelDecisionId,
    controlIndex,
  )
import Eclips.Domain.Label qualified as DomainLabel
import Eclips.Domain.Topology (deriveMemberSetDigest)
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RequestId,
    RetainedRequestReplyBody
      ( Completed
      ),
  )
import Eclips.Herald.Discovery
  ( BindingAdmission,
    PeerBinding,
    PeerDialClass,
    PeerHelloDisposition (PeerHelloAccepted),
    peerBindingRemoteHeraldEpoch,
    peerDialIntentClass,
    peerDialIntentGenerationWord64,
    peerDialIntentHeraldEpoch,
    peerDialIntentHeraldId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect
      ( RunOracleClientAction,
        SendApplicationReply,
        SetPeerCandidateDisposition
      ),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
    HeraldInputBody (ApplicationRequestInput, OracleInput, PeerInput, RuntimeObserved),
    PeerControl
      ( PeerKnownHeralds,
        PeerLabelInstalled,
        PeerPlacementAcknowledged,
        PeerPlacementUpdate,
        PeerStreamCompleted,
        PeerStreamFrontierAdvanced,
        PeerStreamReceived,
        PeerStreamResumeAccepted,
        PeerStreamResumeOffered,
        PeerStructuralAppliedReported,
        PeerTopologyCutAccepted,
        PeerTopologyCutAnnounced,
        PeerTopologyCutEstablished,
        PeerTopologyCutEstablishedAcknowledged
      ),
    PeerIngress
      ( PeerDispatchSelected,
        PeerHelloReceived
      ),
    RuntimeObservation (PeerBindingLost, PeerDispatchObserved),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (..),
    OracleClientIngress (..),
    OracleRetryPurpose,
    oracleBindingContact,
    oracleBindingGeneration,
    oracleBindingGenerationWord64,
    oracleConnectAttemptContact,
    oracleConnectAttemptFromExclusive,
    oracleConnectAttemptOrdinal,
    oracleContactNode,
    oracleHelloAcceptedLeaderHint,
    oracleHelloAcceptedLocalApplied,
    oracleHelloAcceptedNode,
    oracleHelloAcceptedServiceReady,
    oracleHelloAcceptedTerm,
    oracleRedirectLeaderHint,
    oracleRedirectTerm,
    oracleRequestDispatchEnvelope,
  )
import Eclips.Herald.Peer.RPC qualified as PeerRpc
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome
      ( PeerDispatchDeferred,
        PeerDispatchWritten
      ),
    peerDispatchAttemptItem,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( ConnectionPromotion (PeerConnectionPromoted),
    KernelTraceStep (KernelTraceStepped),
    RuntimeEventOrdinal,
    RuntimeTraceEvent (KernelEvent, ShellEvent),
    ShellTraceEvent
      ( ShellConnectionClosed,
        ShellConnectionPromoted,
        ShellConnectionRegistered,
        ShellConnectionWriterFailed,
        ShellEffectRouted,
        ShellPeerDialDelivered,
        ShellPeerOutcomeSuppressed,
        ShellPeerOutcomeWon
      ),
    TraceCursor,
    snapshotTraceLedger,
    snapshotTraceLedgerSince,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( closePeerTransportGate,
    openPeerTransportGate,
    peerTransportGateIsOpen,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( OracleManagerProbeEvent (..),
    OracleSubmissionProgressEvent (..),
    PeerDialProbeEvent (..),
    PeerDialProbePhase,
  )
import Eclips.Oracle.Canonical
  ( canonicalAppliedOracleEntryValue,
    canonicalOracleEnvelopeValue,
  )
import Eclips.Oracle.Command (oracleCommandProgress, oracleEnvelopeCommand)
import Eclips.Oracle.Identity (oracleClientRequestHome, oracleClientRequestSequence)
import Eclips.Oracle.Label
  ( LiveTerminalOutcomeView (LiveReleasedOutcomeView),
    liveDecisionId,
    liveDecisionMemberSetDigest,
    liveTerminalOutcomeView,
    oracleCommandTag,
  )
import Eclips.Oracle.Progress (oracleProgressCovers, oracleProgressLabelsThrough, oracleProgressReceipts)
import Eclips.Oracle.Projection
  ( AppliedOracleEntry,
    OracleProjectionEventView (..),
    appliedEntryCommand,
    appliedEntryControlIndex,
    appliedEntryOracleProgress,
    appliedEntryProjectionEvents,
    appliedEntryReceiptRetirement,
    appliedEntryRequestId,
    oracleProjectionEventView,
  )
import Eclips.Oracle.Runtime
  ( OracleRuntimeEvent (RuntimeRaftInputInstalled),
    OracleRuntimeRecordingMode (OracleCaptureHistory),
    configureOracleRuntimeRecording,
    oracleRuntimeAppliedControlIndex,
    oracleRuntimeCompletedWorkflowCount,
    oracleRuntimeLabelRetiredThrough,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    oracleTcpNodeRecordings,
    oracleTcpNodeStatuses,
  )
import Eclips.Protocol.Application.Types (applicationClientNonce)
import Eclips.Protocol.Peer.Types qualified as PeerProtocol
import Eclips.Public.Types.ReceiptRetirement
  ( receiptIsRetired,
    receiptRetirementExceptions,
    receiptRetirementHighWater,
  )
import Eclips.Raft.Input
  ( RaftInputView (..),
    RaftRpcView (..),
    raftInputView,
    raftRpcView,
  )
import PeerEvidence (data SemanticPeerAlignmentControl, data SemanticPeerControlReceived, data SemanticPeerPublicationReceived, data SemanticSendPeerControl, data SemanticSendPeerItem)
import Step12Fixtures
  ( step12H1HeraldEpoch,
    step12H2HeraldEpoch,
    step12H3HeraldEpoch,
    step12H4HeraldEpoch,
  )
import Step14ApplicationWorkflow
  ( Step14ApplicationTopology (..),
    awaitStep14Message,
    establishStep14ApplicationTopology,
    expectStep14ApplicationLabel,
    expectStep14WriteAccepted,
    publishStep14Message,
    readStep14Messages,
    readStep14SequencingCarrier,
    step14MessageValue,
  )
import Step14Fixtures
  ( Step14Deployment,
    Step14Herald,
    Step14PeerTraceCut (..),
    awaitStep14H4PeerIsolation,
    awaitStep14OraclePrefix,
    awaitStep14PeerMesh,
    semanticStep14InputBody,
    snapshotStep14HeraldOracleManagerEvents,
    snapshotStep14HeraldOracleManagerEventsSince,
    snapshotStep14HeraldPeerDialsSince,
    snapshotStep14HeraldTrace,
    snapshotStep14HeraldTraceSince,
    snapshotStep14PeerTraceCut,
    step14ApplicationLivenessConfiguration,
    step14H1,
    step14H2,
    step14H3,
    step14H4,
    step14H4PeerTransportGate,
    step14HeraldTraceCursor,
    step14HeraldTraceLedger,
    step14OracleCluster,
    step14PApplicationAttachment,
    step14PApplicationEndpoint,
    step14QApplicationAttachment,
    step14QApplicationEndpoint,
    withStep14DeploymentWithOracleConfigurations,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Step-14 label release certification"
    [ testCase
        "settled labels release while H4 is isolated and complete after its installation report reconnects"
        caseSettledLabelRecoversPeerIsolation
    ]

caseSettledLabelRecoversPeerIsolation :: IO ()
caseSettledLabelRecoversPeerIsolation = do
  completed <-
    timeout 90_000_000
      $ withStep14DeploymentWithOracleConfigurations (map (configureOracleRuntimeRecording OracleCaptureHistory))
      $ \deployment -> do
        let h4Gate = step14H4PeerTransportGate deployment
        initiallyOpen <- peerTransportGateIsOpen h4Gate
        assertBool "H4 starts connected to the peer plane" initiallyOpen
        awaitStep14PeerMesh deployment
        withStep14Applications
          (openPeerTransportGate h4Gate)
          deployment
          $ \pApplication pStartup qApplication qStartup -> do
            topologyObserved <-
              timeout 30_000_000
                $ establishStep14ApplicationTopology
                  pApplication
                  pStartup
                  qApplication
                  qStartup
            topology <- case topologyObserved of
              Nothing ->
                assertFailure "the common P/Q topology did not establish"
              Just established -> pure established

            preFenceCall <- publishStep14Message pApplication topology preFenceMessage
            expectStep14WriteAccepted "pre-fence publication" preFenceCall
            awaitMessageWithin "pre-fence publication at Q" qApplication topology preFenceMessage
            -- Cover the exact assigned source frontiers before taking the cut.
            -- Release needs no new source evidence; completion still collects installations.
            awaitSourceCompletionBeforeIsolation (step14H1 deployment)

            isolationCut <- snapshotStep14PeerTraceCut deployment
            closePeerTransportGate h4Gate
            awaitStep14H4PeerIsolation deployment
            isolationEvidence <- awaitH4SemanticIsolation deployment isolationCut
            gateAfterCut <- peerTransportGateIsOpen h4Gate
            assertBool "H4 is partitioned before label decision" (not gateAfterCut)

            beforeLabel <- step14HeraldTraceCursor (step14H1 deployment)
            let expectedLabel = (ProcessLabel (startupAccessProcess pStartup), 0)
            labelCall <-
              EAPP.label
                pApplication
                (asPrivateObjectId topology.pSequencingObject)
                expectedLabel
                LabelToVoid
            labelCompletion <- observeApplicationCall labelCall
            (labelRequest, labelOrdinal) <-
              awaitExactApplicationInput
                "the retained label request"
                deployment
                beforeLabel
                ( LabelApplication
                    (asPrivateObjectId topology.pSequencingObject)
                    expectedLabel
                    LabelToVoid
                )
            openEvidence <- awaitLabelOpenEvidence deployment beforeLabel
            assertBool "the retained label call precedes its Oracle decision" (labelOrdinal < openEvidence.openOrdinal)
            awaitAllHeraldsMatchingLabelOpen
              deployment
              isolationEvidence.isolationCursors
              openEvidence
            awaitStep14OraclePrefix 1 deployment
            assertAllOracleVotersReachedOpen deployment
            gateAfterOpen <- peerTransportGateIsOpen h4Gate
            assertBool
              "H4 remains peer-cut after independently applying the same decision"
              (not gateAfterOpen)

            beforePostFence <- step14HeraldTraceCursor (step14H1 deployment)
            postFenceCall <- publishStep14Message pApplication topology postFenceMessage
            postFenceCompletion <- observeApplicationCall postFenceCall
            (postFenceRequest, sentinelOrdinal) <-
              awaitExactApplicationInput
                "the exact post-fence write request"
                deployment
                beforePostFence
                ( WriteApplication
                    topology.pNablaWriter
                    (PublishValue (step14MessageValue postFenceMessage))
                )
            assertBool
              "the exact post-fence input follows the captured four-Herald decision"
              (openEvidence.openOrdinal < sentinelOrdinal)
            awaitLabelReleaseEvidence deployment openEvidence.decision
            pending <- tryReadMVar labelCompletion
            assertBool "Release cannot complete the application while H4's installation report is isolated" (case pending of Nothing -> True; Just _ -> False)

            beforeReconnect <- finalizeH4SemanticIsolation deployment isolationEvidence
            preReopenH1 <- case beforeReconnect of
              h1Trace : _ -> pure h1Trace
              [] -> assertFailure "the final pre-reopen trace omitted H1"
            assertEqual
              "no canonical completion can bypass the isolated participant"
              []
              [decision | OracleProjectionTrace _ _ (LabelWorkflowCompletedView decision _) <- oracleProjectionTrace preReopenH1]
            gateBeforeReopen <- peerTransportGateIsOpen h4Gate
            assertBool "H4 remains peer-cut through Release" (not gateBeforeReopen)
            beforeReconnectCuts <- snapshotStep14PeerTraceCut deployment
            let beforeReconnectPeerDialCursors =
                  fmap step14PeerTraceCutPeerDialCursor beforeReconnectCuts
                beforeReconnectOracleManagerCursors =
                  fmap step14PeerTraceCutOracleManagerCursor beforeReconnectCuts
            openPeerTransportGate h4Gate
            reopened <- peerTransportGateIsOpen h4Gate
            assertBool "H4 peer admission reopens" reopened
            awaitStep14PeerMesh deployment
            labelResult <- awaitObservedCall "label result after H4 reconnects" labelCompletion
            assertEqual
              "retained direct installation evidence completes after reconnect"
              (EAPP.ApplicationCallSucceeded (LabelCompleted LabelApplied))
              labelResult
            postFenceResult <- awaitObservedCall "post-fence publication result" postFenceCompletion
            assertEqual
              "the later selected publication applies after release"
              (EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted))
              postFenceResult
            awaitMessageWithin "post-fence publication at Q after reconnect" qApplication topology postFenceMessage
            beforeRejoin <- readStep14Messages qApplication topology
            assertBool "Q retains the preceding settled value" (step14MessageValue preFenceMessage `elem` beforeRejoin)
            assertBool "Q observes the later selected value after release" (step14MessageValue postFenceMessage `elem` beforeRejoin)
            carrier <- readStep14SequencingCarrier pApplication topology
            expectStep14ApplicationLabel
              "P observes the sequencing target's released void label"
              (VoidLabel, 1)
              carrier

            afterInstallation <- snapshotStep14HeraldTrace (step14H1 deployment)
            assertCapturedLabelCompletion openEvidence.decision afterInstallation
            assertRetainedCompletionAfterRelease openEvidence.decision labelRequest (LabelCompleted LabelApplied) afterInstallation
            assertRetainedCompletionAfterRelease openEvidence.decision postFenceRequest (WriteCompleted WriteAccepted) afterInstallation
            -- No further application calls drive retirement. The ordinary
            -- binding-scoped idle timers must deliver every Herald's promise.
            awaitIdleLabelRetirement deployment openEvidence.decision afterInstallation

            pure
              ( deploymentHeralds deployment,
                step14OracleCluster deployment,
                beforeReconnect,
                beforeReconnectPeerDialCursors,
                beforeReconnectOracleManagerCursors
              )
  case completed of
    Nothing ->
      assertFailure "the four-Herald label schedule exceeded its failure bound"
    Just
      ( heralds,
        oracleCluster,
        beforeReconnect,
        beforeReconnectPeerDialCursors,
        beforeReconnectOracleManagerCursors
        ) -> do
        -- The scoped fixture has semantically drained and joined every Herald
        -- before returning.  This final snapshot therefore includes delayed
        -- reconnect work rather than accepting a merely momentary quiet trace.
        afterReconnect <- traverse snapshotStep14HeraldTrace heralds
        peerDialDeltas <-
          zipWithM
            ( \cursor herald ->
                snd <$> snapshotStep14HeraldPeerDialsSince cursor herald
            )
            beforeReconnectPeerDialCursors
            heralds
        oracleManagerDeltas <-
          zipWithM
            ( \cursor herald ->
                snd
                  <$> snapshotStep14HeraldOracleManagerEventsSince
                    cursor
                    herald
            )
            beforeReconnectOracleManagerCursors
            heralds
        void
          ( assertReconnectSemanticWork
              beforeReconnect
              afterReconnect
              peerDialDeltas
              oracleManagerDeltas
              (reconnectOracleEvidence oracleCluster (zipWith (drop . length) beforeReconnect afterReconnect))
          )

withStep14Applications ::
  IO () ->
  Step14Deployment ->
  ( EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    IO result
  ) ->
  IO result
withStep14Applications releasePeerGate deployment use = do
  pResult <-
    EAPP.withApplication
      ( EAPP.applicationConfiguration
          (step14PApplicationEndpoint deployment)
          step14PApplicationAttachment
          (applicationClientNonce 14_001)
          step14ApplicationLivenessConfiguration
      )
      ( \pApplication pStartup -> do
          qResult <-
            EAPP.withApplication
              ( EAPP.applicationConfiguration
                  (step14QApplicationEndpoint deployment)
                  step14QApplicationAttachment
                  (applicationClientNonce 14_002)
                  step14ApplicationLivenessConfiguration
              )
              ( \qApplication qStartup ->
                  use pApplication pStartup qApplication qStartup
                    `finally` releasePeerGate
              )
          requireApplicationResult "Q" qResult
      )
  requireApplicationResult "P" pResult

requireApplicationResult ::
  String ->
  Either EAPP.ApplicationRuntimeFailure result ->
  IO result
requireApplicationResult name = \case
  Left failure ->
    assertFailure (name <> " application runtime failed: " <> show failure)
  Right result -> pure result

observeApplicationCall ::
  EAPP.ApplicationCall ->
  IO (MVar (Either SomeException EAPP.ApplicationCallCompletion))
observeApplicationCall call = do
  observed <- newEmptyMVar
  _ <- forkFinally (EAPP.awaitApplicationCall call) (void . tryPutMVar observed)
  pure observed

data LabelOpenEvidence = LabelOpenEvidence
  { decision :: LabelDecisionId,
    openOrdinal :: RuntimeEventOrdinal
  }

data OracleProjectionTrace
  = OracleProjectionTrace
      RuntimeEventOrdinal
      ControlIndex
      OracleProjectionEventView
  deriving stock (Show)

awaitLabelOpenEvidence :: Step14Deployment -> TraceCursor -> IO LabelOpenEvidence
awaitLabelOpenEvidence deployment initialCursor = do
  observed <- timeout 10_000_000 (loop initialCursor)
  case observed of
    Nothing -> do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      oracleManagerEvents <-
        snapshotStep14HeraldOracleManagerEvents (step14H1 deployment)
      oracleStatuses <- oracleTcpNodeStatuses (step14OracleCluster deployment)
      oracleRecordings <- oracleTcpNodeRecordings (step14OracleCluster deployment)
      assertFailure
        ( "H1 did not retain the four-Herald label decision while H4 was cut; "
            <> "Oracle projection trace: "
            <> show (oracleProjectionTrace events)
            <> "; Oracle activity: "
            <> show (oracleActivityTrace events)
            <> "; Oracle statuses: "
            <> show oracleStatuses
            <> "; Oracle manager pacing/progress: "
            <> show (oracleManagerPhysicalSummary oracleManagerEvents)
            <> "; Raft input summaries: "
            <> show (fmap (fmap raftInputSummary) oracleRecordings)
        )
    Just evidence -> pure evidence
  where
    loop cursor = do
      (nextCursor, events) <-
        snapshotStep14HeraldTraceSince cursor (step14H1 deployment)
      case labelOpenEvidence events of
        Left problem -> assertFailure problem
        Right Nothing -> threadDelay 10_000 >> loop nextCursor
        Right (Just evidence) -> pure evidence

raftInputSummary :: [OracleRuntimeEvent] -> (Map.Map String Int, [String])
raftInputSummary events =
  ( Map.fromListWith (+) [(kind, 1) | kind <- kinds],
    reverse (take 12 (reverse kinds))
  )
  where
    kinds =
      [ raftInputKind (raftInputView input)
      | RuntimeRaftInputInstalled input <- events
      ]

-- Read only retained recordings after the fixture has stopped, and only when
-- a work bound fails. Ordinals below count Raft inputs, not elapsed time.
reconnectOracleEvidence :: OracleTcpCluster -> [[RuntimeTraceEvent]] -> IO String
reconnectOracleEvidence cluster heraldDeltas = do
  recordings <- oracleTcpNodeRecordings cluster
  pure
    ( "; retained Raft input counts/tail="
        <> show (fmap (fmap raftInputSummary) recordings)
        <> "; Raft RPC term/source groups (first input, last input, count), election timer inputs="
        <> show (fmap (fmap raftTermInputSummary) recordings)
        <> "; per-Herald post-reconnect Oracle term/prefix observations="
        <> show
          ( zip
              [1 :: Int ..]
              (fmap (oracleLifecycleSummary . filter termOrPrefixInput) heraldDeltas)
          )
    )
  where
    termOrPrefixInput = \case
      KernelEvent _ (KernelTraceStepped _ _ input (Right _)) -> case inputBody input of
        OracleInput OracleHelloReceived {} -> True
        OracleInput OracleRedirectReceived {} -> True
        OracleInput OracleEntriesReceived {} -> True
        _ -> False
      _ -> False

raftTermInputSummary :: [OracleRuntimeEvent] -> (Map.Map String (Int, Int, Int), [(Int, String)])
raftTermInputSummary events =
  ( Map.fromListWith
      combine
      [ (rpcTermSource (raftRpcView rpc), (ordinal, ordinal, 1))
      | (ordinal, input) <- inputs,
        rpc <- case input of
          ObserveRequestView _ request -> [request]
          ObserveResponseView _ _ response -> [response]
          _ -> []
      ],
    [ (ordinal, show generation)
    | (ordinal, FireElectionTimerView generation) <- inputs
    ]
  )
  where
    inputs = zip [1 ..] [raftInputView input | RuntimeRaftInputInstalled input <- events]
    combine (first, lastInput, count) (otherFirst, otherLast, otherCount) =
      (min first otherFirst, max lastInput otherLast, count + otherCount)
    rpcTermSource = \case
      RequestVoteView term node _ _ -> "RequestVote " <> show (term, node)
      RequestVoteResponseView term node granted -> "VoteResponse " <> show (term, node, granted)
      AppendEntriesView term node _ _ _ _ -> "AppendEntries " <> show (term, node)
      AppendEntriesResponseView term node _ -> "AppendResponse " <> show (term, node)
      InstallSnapshotView term node _ -> "InstallSnapshot " <> show (term, node)
      InstallSnapshotResponseView term node _ installed -> "SnapshotResponse " <> show (term, node, installed)

raftInputKind :: RaftInputView bytes -> String
raftInputKind = \case
  ObserveRequestView _ rpc -> case raftRpcView rpc of
    RequestVoteView {} -> "request-vote-received"
    AppendEntriesView {} -> "append-entries-received"
    InstallSnapshotView {} -> "install-snapshot-received"
    RequestVoteResponseView {} -> "invalid-request-vote-response"
    AppendEntriesResponseView {} -> "invalid-append-response"
    InstallSnapshotResponseView {} -> "invalid-snapshot-response"
  ObserveResponseView _ _ rpc -> case raftRpcView rpc of
    RequestVoteResponseView _ _ True -> "granted-vote-response"
    RequestVoteResponseView _ _ False -> "denied-vote-response"
    AppendEntriesResponseView {} -> "append-entries-response"
    InstallSnapshotResponseView {} -> "install-snapshot-response"
    RequestVoteView {} -> "invalid-vote-request-response"
    AppendEntriesView {} -> "invalid-append-request-response"
    InstallSnapshotView {} -> "invalid-snapshot-request-response"
  SelectElectionTimeoutView {} -> "election-timeout-selected"
  FireElectionTimerView {} -> "election-timer-fired"
  FireHeartbeatTimerView {} -> "heartbeat-timer-fired"
  FireRecentLeaderTimerView {} -> "recent-leader-timer-fired"
  ProposeApplicationView {} -> "application-proposed"
  AcknowledgeCommittedEntriesView {} -> "entry-acknowledged"
  CheckpointApplicationView {} -> "application-checkpointed"
  AcknowledgeCheckpointView {} -> "checkpoint-acknowledged"
  UpdateReplicationTargetsView {} -> "replication-targets-updated"
  BeginConfigurationChangeView {} -> "configuration-preparation-begun"
  CancelConfigurationChangeView {} -> "configuration-preparation-cancelled"
  ObserveLearnerReadinessView {} -> "learner-readiness-observed"
  ProposeConfigurationView {} -> "configuration-proposed"

awaitAllHeraldsMatchingLabelOpen ::
  Step14Deployment ->
  [TraceCursor] ->
  LabelOpenEvidence ->
  IO ()
awaitAllHeraldsMatchingLabelOpen deployment initialCursors expected = do
  unless (length initialCursors == length heralds)
    $ assertFailure
      "the label-Open cursor set no longer matches the four-Herald deployment"
  observed <- timeout 10_000_000 (loop initialCursors (replicate 4 False))
  case observed of
    Nothing -> do
      traces <- snapshotDeploymentTraces deployment
      oracleManagerEvents <-
        traverse
          snapshotStep14HeraldOracleManagerEvents
          (deploymentHeralds deployment)
      oracleStatuses <- oracleTcpNodeStatuses (step14OracleCluster deployment)
      assertFailure
        ( "all four Heralds did not apply the same label decision while H4 was peer-cut; "
            <> "expected decision="
            <> show expected.decision
            <> "; label projections="
            <> show (fmap labelProjectionReporterSummary traces)
            <> "; Oracle lifecycles="
            <> show (fmap oracleLifecycleSummary traces)
            <> "; Oracle activity="
            <> show (fmap oracleActivityTrace traces)
            <> "; Oracle manager pacing/progress="
            <> show (fmap oracleManagerPhysicalSummary oracleManagerEvents)
            <> "; Oracle statuses="
            <> show oracleStatuses
            <> "; post-Open peer receipts="
            <> show (fmap postOpenPeerReceiptSummary traces)
        )
    Just () -> pure ()
  where
    heralds = deploymentHeralds deployment
    loop cursors retained = do
      deltas <- snapshotDeploymentTraceDeltas cursors deployment
      observed <- zipWithM observe retained (fmap snd deltas)
      if and observed
        then pure ()
        else threadDelay 10_000 >> loop (fmap fst deltas) observed

    observe True _ = pure True
    observe False events = case labelOpenEvidence events of
      Left problem -> assertFailure problem
      Right Nothing -> pure False
      Right (Just actual) -> do
        assertEqual
          "every Herald applies the exact same label decision observed by H1"
          expected.decision
          actual.decision
        pure True

assertAllOracleVotersReachedOpen :: Step14Deployment -> IO ()
assertAllOracleVotersReachedOpen deployment = do
  statuses <- oracleTcpNodeStatuses (step14OracleCluster deployment)
  assertEqual
    "the label schedule retains all three Oracle voter runtimes"
    3
    (length statuses)
  assertBool
    ( "all three Oracle voters reached the decision's prefix independently of "
        <> "H4's peer evidence; statuses="
        <> show statuses
    )
    ( all
        ((>= controlIndex 1) . oracleRuntimeAppliedControlIndex . snd)
        statuses
    )

labelOpenEvidence ::
  [RuntimeTraceEvent] ->
  Either String (Maybe LabelOpenEvidence)
labelOpenEvidence events =
  case [ (ordinal, index, opened)
       | OracleProjectionTrace ordinal index (LabelDecidedView opened _ _) <- trace
       ] of
    [] -> Right Nothing
    [(openOrdinal, openIndex, opened)] -> do
      unlessEither
        (openIndex == controlIndex 1)
        ("the label decision did not occupy prefix 1: " <> show openIndex)
      unlessEither
        (liveDecisionMemberSetDigest opened == deriveMemberSetDigest (NonEmpty.fromList (Set.toAscList expectedHeralds)))
        ( "the label decision did not capture exactly H1-H4: "
            <> show (liveDecisionMemberSetDigest opened)
        )
      Right
        ( Just
            LabelOpenEvidence
              { decision = liveDecisionId opened,
                openOrdinal
              }
        )
    opens -> Left ("expected one label decision trace event, got " <> show opens)
  where
    trace = oracleProjectionTrace events

expectedHeralds :: Set.Set HeraldEpoch
expectedHeralds =
  Set.fromList
    [ step12H1HeraldEpoch,
      step12H2HeraldEpoch,
      step12H3HeraldEpoch,
      step12H4HeraldEpoch
    ]

unlessEither :: Bool -> String -> Either String ()
unlessEither condition problem =
  if condition then Right () else Left problem

-- Installation reports arrive directly at the home collector between the
-- atomic decision and its single canonical completion.
awaitLabelReleaseEvidence :: Step14Deployment -> LabelDecisionId -> IO ()
awaitLabelReleaseEvidence deployment expected = do
  released <- timeout 15_000_000 loop
  assertBool "the Oracle releases independently of isolated peer installation reports" (released == Just ())
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      if any isExpectedDecision (oracleProjectionTrace events)
        then pure ()
        else threadDelay 10_000 >> loop
    isExpectedDecision = \case
      OracleProjectionTrace _ _ (LabelDecidedView decision terminal _) ->
        liveDecisionId decision == expected && case liveTerminalOutcomeView terminal of
          LiveReleasedOutcomeView {} -> True
          _ -> False
      _ -> False

assertCapturedLabelCompletion :: LabelDecisionId -> [RuntimeTraceEvent] -> IO ()
assertCapturedLabelCompletion expected events = do
  let trace = oracleProjectionTrace events
      installed =
        [ (ordinal, DomainLabel.labelInstallationReporter report, DomainLabel.labelInstallationControlIndex report)
        | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events,
          PeerInput (SemanticPeerControlReceived _ (PeerLabelInstalled report)) <- [inputBody input],
          DomainLabel.labelInstallationDecisionId report == expected
        ]
      completionOrdinals = [ordinal | OracleProjectionTrace ordinal _ (LabelWorkflowCompletedView decision _) <- trace, decision == expected]
      decisions = nub [index | OracleProjectionTrace _ index (LabelDecidedView decision terminal _) <- trace, liveDecisionId decision == expected, LiveReleasedOutcomeView {} <- [liveTerminalOutcomeView terminal]]
      completions = nub [index | OracleProjectionTrace _ index (LabelWorkflowCompletedView decision _) <- trace, decision == expected]
  releaseIndex <- exactlyOne "Applied decision" decisions
  completionIndex <- exactlyOne "workflow completion" completions
  completionOrdinal <- exactlyOne "completion application" completionOrdinals
  assertEqual
    "direct installation reports cover every remote captured Herald"
    (Set.delete step12H1HeraldEpoch expectedHeralds)
    (Set.fromList [reporter | (_, reporter, _) <- installed])
  assertBool "direct installation reports bind the exact Release" (all (\(_, _, at) -> at == releaseIndex) installed)
  assertBool
    "each participant reports installation before canonical completion"
    ( all
        (\reporter -> any (\(ordinal, source, _) -> source == reporter && ordinal < completionOrdinal) installed)
        (Set.toList (Set.delete step12H1HeraldEpoch expectedHeralds))
    )
  assertBool "the immutable label decision precedes completion" (releaseIndex < completionIndex)
  where
    exactlyOne _ [index] = pure index
    exactlyOne phase indices = assertFailure (phase <> " was not unique: " <> show indices)

awaitIdleLabelRetirement :: Step14Deployment -> LabelDecisionId -> [RuntimeTraceEvent] -> IO ()
awaitIdleLabelRetirement deployment expected events = do
  decisionIndex <- case nub [index | OracleProjectionTrace _ index (LabelDecidedView decision _ _) <- oracleProjectionTrace events, liveDecisionId decision == expected] of
    [index] -> pure index
    indices -> assertFailure ("the retired label must have one original decision index: " <> show indices)
  observed <- timeout 15_000_000 (awaitReclaimed decisionIndex)
  case observed of
    Nothing -> do
      statuses <- oracleTcpNodeStatuses (step14OracleCluster deployment)
      traces <- traverse snapshotStep14HeraldTrace (deploymentHeralds deployment)
      assertFailure
        ( "idle Oracle progress did not reclaim the final completed label; statuses="
            <> show statuses
            <> "; covering idle submissions="
            <> show (fmap (idleSubmissions decisionIndex) traces)
        )
    Just () -> do
      traces <- traverse snapshotStep14HeraldTrace (deploymentHeralds deployment)
      forM_ (zip [1 :: Int ..] traces) $ \(herald, trace) ->
        assertBool
          ("H" <> show herald <> " advertised the final label through its real idle timer")
          (not (null (idleSubmissions decisionIndex trace)))
  where
    awaitReclaimed decisionIndex = do
      statuses <- fmap (fmap snd) (oracleTcpNodeStatuses (step14OracleCluster deployment))
      if not (null statuses) && all (\status -> oracleRuntimeLabelRetiredThrough status >= decisionIndex && oracleRuntimeCompletedWorkflowCount status == 0) statuses
        then pure ()
        else threadDelay 10_000 >> awaitReclaimed decisionIndex
    idleSubmissions decisionIndex trace =
      [ ordinal
      | KernelEvent ordinal (KernelTraceStepped _ _ input (Right effects)) <- trace,
        OracleInput (OracleProgressFlush _) <- [inputBody input],
        RunOracleClientAction (SubmitOracleProgress _ canonical) <- effectBatchMembers effects,
        Just progress <- [oracleCommandProgress (oracleEnvelopeCommand (canonicalOracleEnvelopeValue canonical))],
        oracleProgressLabelsThrough progress >= decisionIndex
      ]

assertRetainedCompletionAfterRelease :: LabelDecisionId -> RequestId -> RegularCallResult -> [RuntimeTraceEvent] -> IO ()
assertRetainedCompletionAfterRelease decision request expected events = do
  let releases = [ordinal | OracleProjectionTrace ordinal _ (LabelDecidedView released terminal _) <- oracleProjectionTrace events, liveDecisionId released == decision, LiveReleasedOutcomeView {} <- [liveTerminalOutcomeView terminal]]
      replies =
        [ ordinal
        | KernelEvent ordinal (KernelTraceStepped _ _ _ (Right effects)) <- events,
          SendApplicationReply _ (RetainedRequestReply _ body) <- effectBatchMembers effects,
          body == Completed request expected
        ]
  assertBool ("the exact retained application request completes as " <> show expected) (not (null replies))
  case releases of
    [] -> assertFailure "the completed application request has no installed Release"
    _ -> assertBool "selected application completion never precedes Release" (all (>= minimum releases) replies)

-- Use ordinary public wire projections to inspect the source's advertised
-- assignment maxima and observed contiguous completion, without reading a
-- runtime owner's kernel state or relying on a quiet-time heuristic.
awaitSourceCompletionBeforeIsolation :: Step14Herald -> IO ()
awaitSourceCompletionBeforeIsolation source = do
  initialCursor <- step14HeraldTraceCursor source
  before <- snapshotStep14HeraldTrace source
  let frontiers =
        Map.fromListWith
          max
          [ (peerBindingRemoteHeraldEpoch binding, PeerProtocol.positiveStreamSequenceDtoWord64 nextSequence - 1)
          | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- before,
            SemanticSendPeerControl binding control@PeerStreamFrontierAdvanced {} <- effectBatchMembers effects,
            Right (PeerProtocol.PeerControlEnvelope _ (PeerProtocol.StreamFrontierAdvancedDto _ nextSequence)) <- [PeerRpc.projectPeerControl control]
          ]
  assertBool "the topology and preceding write assigned real source publications" (any (> 0) (Map.elems frontiers))
  settled <- timeout 10_000_000 (loop frontiers initialCursor (completionProgress before))
  case settled of
    Just () -> pure ()
    Nothing -> do
      latest <- snapshotStep14HeraldTrace source
      assertFailure ("the source's pre-isolation completion frontiers did not settle: required=" <> show frontiers <> "; observed=" <> show (completionProgress latest))
  where
    loop frontiers cursor completed
      | and [covered peer through completed | (peer, through) <- Map.toAscList frontiers] = pure ()
      | otherwise = do
          threadDelay 10_000
          (nextCursor, events) <- snapshotStep14HeraldTraceSince cursor source
          loop frontiers nextCursor (Map.unionWith (<>) completed (completionProgress events))
    covered _ 0 _ = True
    covered peer through completed = case Map.lookup peer completed of
      Nothing -> False
      Just progress ->
        receiptRetirementHighWater progress >= Just through
          && maybe True (> through) (Set.lookupMin (receiptRetirementExceptions progress))
    completionProgress events =
      Map.fromListWith
        (<>)
        [ (peerBindingRemoteHeraldEpoch binding, progress)
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
          PeerInput (SemanticPeerControlReceived binding control) <- [inputBody input],
          Right (PeerProtocol.PeerControlEnvelope _ dto) <- [PeerRpc.projectPeerControl control],
          progress <- case dto of
            PeerProtocol.StreamCompletedDto _ completion -> [completion]
            PeerProtocol.StreamResumeOfferedDto offer -> [PeerProtocol.resumeOfferCompletion offer]
            PeerProtocol.StreamResumeAcceptedDto response -> [PeerProtocol.resumeResponseCompletion response]
            _ -> []
        ]

oracleProjectionTrace :: [RuntimeTraceEvent] -> [OracleProjectionTrace]
oracleProjectionTrace events =
  [ OracleProjectionTrace ordinal (appliedEntryControlIndex entry) projection
  | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events,
    OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
    canonical <- NonEmpty.toList entries,
    let entry = canonicalAppliedOracleEntryValue canonical,
    event <- appliedEntryProjectionEvents entry,
    let projection = oracleProjectionEventView event
  ]

labelProjectionReporterSummary :: [RuntimeTraceEvent] -> [(ControlIndex, LabelDecisionId, String)]
labelProjectionReporterSummary events =
  [ (index, liveDecisionId decision, show (liveTerminalOutcomeView terminal))
  | OracleProjectionTrace _ index (LabelDecidedView decision terminal _) <- oracleProjectionTrace events
  ]

oracleLifecycleSummary ::
  [RuntimeTraceEvent] ->
  (Int, [(RuntimeEventOrdinal, String)])
oracleLifecycleSummary events =
  (length activity, reverse (take 48 (reverse activity)))
  where
    activity =
      [ (ordinal, description)
      | event <- events,
        description <- case event of
          KernelEvent _ (KernelTraceStepped _ _ input (Right _)) ->
            lifecycleIngress (inputBody input)
          ShellEvent _ (ShellEffectRouted _ _ effect) ->
            lifecycleEffect effect
          _ -> [],
        let ordinal = eventOrdinal event
      ]

    eventOrdinal = \case
      KernelEvent ordinal _ -> ordinal
      ShellEvent ordinal _ -> ordinal

    lifecycleIngress = \case
      OracleInput ingress -> case ingress of
        OracleConnectFailed attempt ->
          ["ConnectFailed " <> attemptSummary attempt]
        OracleHelloReceived attempt acceptance ->
          [ "Hello "
              <> attemptSummary attempt
              <> " acceptedNode="
              <> show (oracleHelloAcceptedNode acceptance)
              <> " term="
              <> show (oracleHelloAcceptedTerm acceptance)
              <> " localApplied="
              <> show (oracleHelloAcceptedLocalApplied acceptance)
              <> " leaderHint="
              <> show (oracleHelloAcceptedLeaderHint acceptance)
              <> " ready="
              <> show (oracleHelloAcceptedServiceReady acceptance)
          ]
        OracleRedirectReceived lane redirect ->
          [ "Redirect lane="
              <> show lane
              <> " leaderHint="
              <> show (oracleRedirectLeaderHint redirect)
              <> " term="
              <> show (oracleRedirectTerm redirect)
          ]
        OracleEntriesReceived binding entries ->
          [ "Entries "
              <> bindingSummary binding
              <> " indices="
              <> show
                ( fmap
                    ( appliedEntryControlIndex
                        . canonicalAppliedOracleEntryValue
                    )
                    (NonEmpty.toList entries)
                )
          ]
        OracleBindingLost binding ->
          ["BindingLost " <> bindingSummary binding]
        _ -> []
      _ -> []

    lifecycleEffect = \case
      RunOracleClientAction action -> case action of
        ConnectAndHelloOracle attempt _ ->
          ["Connect " <> attemptSummary attempt]
        BindOracleConnection attempt binding ->
          [ "Bind "
              <> attemptSummary attempt
              <> " "
              <> bindingSummary binding
          ]
        WatchOracle binding cursor ->
          ["Watch " <> bindingSummary binding <> " cursor=" <> show cursor]
        _ -> []
      _ -> []

    attemptSummary attempt =
      "attempt="
        <> show (oracleConnectAttemptOrdinal attempt)
        <> " node="
        <> show (oracleContactNode (oracleConnectAttemptContact attempt))
        <> " cursor="
        <> show (oracleConnectAttemptFromExclusive attempt)

    bindingSummary binding =
      "generation="
        <> show
          ( oracleBindingGenerationWord64
              (oracleBindingGeneration binding)
          )
        <> " node="
        <> show (oracleContactNode (oracleBindingContact binding))

postOpenPeerReceiptSummary ::
  [RuntimeTraceEvent] ->
  (Map.Map HeraldEpoch Int, Map.Map HeraldEpoch Int)
postOpenPeerReceiptSummary events = case openOrdinals of
  [] -> (Map.empty, Map.empty)
  firstOpen : _ ->
    ( counts
        [ peerBindingRemoteHeraldEpoch binding
        | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events,
          ordinal >= firstOpen,
          PeerInput (SemanticPeerPublicationReceived binding _) <- [inputBody input]
        ],
      counts
        [ peerBindingRemoteHeraldEpoch binding
        | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events,
          ordinal >= firstOpen,
          PeerInput (SemanticPeerControlReceived binding PeerStreamCompleted {}) <-
            [inputBody input]
        ]
    )
  where
    openOrdinals =
      [ ordinal
      | OracleProjectionTrace ordinal _ LabelDecidedView {} <-
          oracleProjectionTrace events
      ]
    counts =
      Map.fromListWith (+)
        . fmap (\remote -> (remote, 1 :: Int))

oracleActivityTrace ::
  [RuntimeTraceEvent] ->
  (Int, [(String, Int)], [(RuntimeEventOrdinal, String)])
oracleActivityTrace events =
  let activity =
        [ (ordinal, description)
        | event <- events,
          (ordinal, description) <- case event of
            KernelEvent eventOrdinal (KernelTraceStepped _ _ input (Right effects)) ->
              [ (eventOrdinal, oracleIngressName ingress)
              | OracleInput ingress <- [inputBody input]
              ]
                <> [ (eventOrdinal, "Emitted:" <> oracleActionName action)
                   | RunOracleClientAction action <- effectBatchMembers effects
                   ]
            ShellEvent eventOrdinal (ShellEffectRouted _ _ (RunOracleClientAction action)) ->
              [(eventOrdinal, "Routed:" <> oracleActionName action)]
            _ -> []
        ]
      counts =
        [ (description, length (filter ((== description) . snd) activity))
        | description <- nub (fmap snd activity)
        ]
   in (length activity, counts, reverse (take 40 (reverse activity)))
  where
    oracleIngressName = \case
      OracleContactsDiscovered {} -> "OracleContactsDiscovered"
      OracleLocalReplicaConfigured {} -> "OracleLocalReplicaConfigured"
      OracleConnectFailed {} -> "OracleConnectFailed"
      OracleHelloReceived {} -> "OracleHelloReceived"
      OracleRedirectReceived {} -> "OracleRedirectReceived"
      OracleBindingLost {} -> "OracleBindingLost"
      OracleRetryElapsed {} -> "OracleRetryElapsed"
      OracleSubmissionNotReadyReceived {} ->
        "OracleSubmissionNotReadyReceived"
      OracleSubmissionDeferredReceived {} ->
        "OracleSubmissionDeferredReceived"
      OracleProgressConfirmed {} -> "OracleProgressConfirmed"
      OracleProgressFlush {} -> "OracleProgressFlush"
      OracleProgressNotReadyReceived {} -> "OracleProgressNotReadyReceived"
      OracleRequestRetiredReceived {} -> "OracleRequestRetiredReceived"
      OracleEntriesReceived {} -> "OracleEntriesReceived"

    oracleActionName = \case
      ConnectAndHelloOracle attempt _ ->
        "ConnectAndHelloOracle " <> show attempt
      BindOracleConnection attempt binding ->
        "BindOracleConnection " <> show attempt <> " " <> show binding
      RejectOracleConnection {} -> "RejectOracleConnection"
      WatchOracle binding cursor ->
        "WatchOracle " <> show binding <> " " <> show cursor
      SubmitOracleRequest {} -> "SubmitOracleRequest"
      SubmitOracleProgress {} -> "SubmitOracleProgress"
      ScheduleOracleProgress {} -> "ScheduleOracleProgress"
      ReleaseOracleProgress {} -> "ReleaseOracleProgress"
      AssociateOracleSubmissionRetry {} ->
        "AssociateOracleSubmissionRetry"
      ReleaseDeferredOracleSubmission {} ->
        "ReleaseDeferredOracleSubmission"
      ScheduleOracleRetry {} -> "ScheduleOracleRetry"
      CancelOracleRetry {} -> "CancelOracleRetry"
      ReportOracleClientDiagnostic {} ->
        "ReportOracleClientDiagnostic"

awaitObservedCall ::
  String ->
  MVar (Either SomeException EAPP.ApplicationCallCompletion) ->
  IO EAPP.ApplicationCallCompletion
awaitObservedCall context observed = do
  settled <- timeout 20_000_000 (readMVar observed)
  case settled of
    Nothing -> assertFailure (context <> " did not settle")
    Just (Left failure) ->
      assertFailure (context <> " raised " <> show failure)
    Just (Right completion) -> pure completion

snapshotDeploymentTraces :: Step14Deployment -> IO [[RuntimeTraceEvent]]
snapshotDeploymentTraces deployment =
  traverse
    snapshotStep14HeraldTrace
    (deploymentHeralds deployment)

snapshotDeploymentPeerDialDeltas ::
  [Int] ->
  Step14Deployment ->
  IO [[PeerDialProbeEvent]]
snapshotDeploymentPeerDialDeltas cursors deployment = do
  let heralds = deploymentHeralds deployment
  unless (length cursors == length heralds)
    $ assertFailure
      "the peer-dial cursor set no longer matches the four-Herald deployment"
  zipWithM
    ( \cursor herald ->
        snd <$> snapshotStep14HeraldPeerDialsSince cursor herald
    )
    cursors
    heralds

snapshotDeploymentOracleManagerDeltas ::
  [Int] ->
  Step14Deployment ->
  IO [[OracleManagerProbeEvent]]
snapshotDeploymentOracleManagerDeltas cursors deployment = do
  let heralds = deploymentHeralds deployment
  unless (length cursors == length heralds)
    $ assertFailure
      "the Oracle-manager cursor set no longer matches the four-Herald deployment"
  zipWithM
    ( \cursor herald ->
        snd <$> snapshotStep14HeraldOracleManagerEventsSince cursor herald
    )
    cursors
    heralds

deploymentHeralds :: Step14Deployment -> [Step14Herald]
deploymentHeralds deployment =
  fmap ($ deployment) [step14H1, step14H2, step14H3, step14H4]

data H4SemanticIsolationEvidence = H4SemanticIsolationEvidence
  { isolationCursors :: [TraceCursor],
    isolationPeerDialCursors :: [Int],
    isolationOracleManagerCursors :: [Int],
    isolationLosses :: [Map.Map PeerBinding Int],
    expectedIsolationLosses :: [Map.Map PeerBinding Int]
  }

awaitH4SemanticIsolation ::
  Step14Deployment ->
  [Step14PeerTraceCut] ->
  IO H4SemanticIsolationEvidence
awaitH4SemanticIsolation deployment cuts = do
  unless (length cuts == 4)
    $ assertFailure
      ("the pre-cut snapshot did not cover all four Heralds: " <> show (length cuts))
  unless (fmap Map.size expectedLossBindings == [1, 1, 1, 3])
    $ assertFailure
      ( "the pre-cut transport mesh did not expose the exact H4 leases: "
          <> show beforeLeases
      )
  observed <- timeout 10_000_000 (loop before (replicate 4 Map.empty))
  case observed of
    Just evidence -> pure evidence
    Nothing -> do
      deltas <- snapshotDeploymentTraceDeltas before deployment
      peerDialDeltas <-
        snapshotDeploymentPeerDialDeltas beforePeerDialCursors deployment
      oracleManagerDeltas <-
        snapshotDeploymentOracleManagerDeltas
          beforeOracleManagerCursors
          deployment
      fullTraces <- snapshotDeploymentTraces deployment
      assertFailure
        ( "the four Herald owners did not observe each exact pre-cut H4 binding loss once; expected="
            <> show expectedLossBindings
            <> "; observed="
            <> show (fmap (bindingLossCounts . snd) deltas)
            <> "; pre-cut transport leases="
            <> show beforeLeases
            <> "; trace summaries="
            <> show (fmap (reconnectTraceSummary . snd) deltas)
            <> "; connection lifecycles="
            <> show (fmap (connectionLifecycle . snd) deltas)
            <> "; full connection lifecycles="
            <> show (fmap connectionLifecycle fullTraces)
            <> "; physical peer-dial phases="
            <> show (fmap peerDialPhysicalSummary peerDialDeltas)
            <> "; Oracle manager pacing/progress="
            <> show (fmap oracleManagerPhysicalSummary oracleManagerDeltas)
        )
  where
    loop cursors observedLosses = do
      deltas <- snapshotDeploymentTraceDeltas cursors deployment
      let nextCursors = fmap fst deltas
          losses =
            zipWith
              (Map.unionWith (+))
              observedLosses
              (fmap (bindingLossCounts . snd) deltas)
      unless (and (zipWith lossCountsAllowed expectedLossBindings losses))
        $ do
          peerDialDeltas <-
            snapshotDeploymentPeerDialDeltas beforePeerDialCursors deployment
          oracleManagerDeltas <-
            snapshotDeploymentOracleManagerDeltas
              beforeOracleManagerCursors
              deployment
          assertFailure
            ( "the H4 cut produced a foreign or duplicate binding loss; expected="
                <> show expectedLossBindings
                <> "; observed="
                <> show losses
                <> "; physical peer-dial phases="
                <> show (fmap peerDialPhysicalSummary peerDialDeltas)
                <> "; Oracle manager pacing/progress="
                <> show (fmap oracleManagerPhysicalSummary oracleManagerDeltas)
            )
      if losses == expectedLossBindings
        then
          pure
            H4SemanticIsolationEvidence
              { isolationCursors = nextCursors,
                isolationPeerDialCursors = beforePeerDialCursors,
                isolationOracleManagerCursors = beforeOracleManagerCursors,
                isolationLosses = losses,
                expectedIsolationLosses = expectedLossBindings
              }
        else threadDelay 10_000 >> loop nextCursors losses

    before = fmap step14PeerTraceCutCursor cuts
    beforePeerDialCursors = fmap step14PeerTraceCutPeerDialCursor cuts
    beforeOracleManagerCursors =
      fmap step14PeerTraceCutOracleManagerCursor cuts
    beforeLeases = fmap step14PeerTraceCutLeases cuts
    expectedLossBindings = expectedH4LossBindings beforeLeases

finalizeH4SemanticIsolation ::
  Step14Deployment ->
  H4SemanticIsolationEvidence ->
  IO [[RuntimeTraceEvent]]
finalizeH4SemanticIsolation deployment evidence = do
  checkpoints <-
    snapshotDeploymentTraceCheckpoints
      evidence.isolationCursors
      deployment
  let finalLosses =
        zipWith
          (Map.unionWith (+))
          evidence.isolationLosses
          (fmap (bindingLossCounts . checkpointDelta) checkpoints)
  unless
    ( and
        ( zipWith
            lossCountsAllowed
            evidence.expectedIsolationLosses
            finalLosses
        )
    )
    $ do
      peerDialDeltas <-
        snapshotDeploymentPeerDialDeltas
          evidence.isolationPeerDialCursors
          deployment
      oracleManagerDeltas <-
        snapshotDeploymentOracleManagerDeltas
          evidence.isolationOracleManagerCursors
          deployment
      assertFailure
        ( "the H4 cut produced a foreign or duplicate late binding loss before reopen; expected="
            <> show evidence.expectedIsolationLosses
            <> "; observed="
            <> show finalLosses
            <> "; physical peer-dial phases="
            <> show (fmap peerDialPhysicalSummary peerDialDeltas)
            <> "; Oracle manager pacing/progress="
            <> show (fmap oracleManagerPhysicalSummary oracleManagerDeltas)
        )
  assertEqual
    "the final pre-reopen trace retains each exact pre-cut H4 binding loss once"
    evidence.expectedIsolationLosses
    finalLosses
  pure (fmap checkpointFull checkpoints)

data DeploymentTraceCheckpoint = DeploymentTraceCheckpoint
  { checkpointDelta :: [RuntimeTraceEvent],
    checkpointFull :: [RuntimeTraceEvent]
  }

snapshotDeploymentTraceCheckpoints ::
  [TraceCursor] ->
  Step14Deployment ->
  IO [DeploymentTraceCheckpoint]
snapshotDeploymentTraceCheckpoints cursors deployment = do
  let heralds = deploymentHeralds deployment
  unless (length cursors == length heralds)
    $ assertFailure
      "the isolation cursor set no longer matches the four-Herald deployment"
  atomically
    $ zipWithM
      ( \cursor herald -> do
          (_, delta) <-
            snapshotTraceLedgerSince cursor (step14HeraldTraceLedger herald)
          full <- snapshotTraceLedger (step14HeraldTraceLedger herald)
          pure (DeploymentTraceCheckpoint delta full)
      )
      cursors
      heralds

snapshotDeploymentTraceDeltas ::
  [TraceCursor] ->
  Step14Deployment ->
  IO [(TraceCursor, [RuntimeTraceEvent])]
snapshotDeploymentTraceDeltas cursors deployment = do
  let heralds = deploymentHeralds deployment
  unless (length cursors == length heralds)
    $ assertFailure
      "the trace cursor set no longer matches the four-Herald deployment"
  atomically
    $ zipWithM
      ( \cursor herald ->
          snapshotTraceLedgerSince cursor (step14HeraldTraceLedger herald)
      )
      cursors
      heralds

expectedH4LossBindings ::
  [[(PeerBinding, Word64)]] ->
  [Map.Map PeerBinding Int]
expectedH4LossBindings =
  zipWith
    ( \expectedRemotes leases ->
        Map.fromList
          [ (binding, 1 :: Int)
          | (binding, _) <- leases,
            peerBindingRemoteHeraldEpoch binding `Set.member` expectedRemotes
          ]
    )
    [ Set.singleton step12H4HeraldEpoch,
      Set.singleton step12H4HeraldEpoch,
      Set.singleton step12H4HeraldEpoch,
      Set.fromList
        [ step12H1HeraldEpoch,
          step12H2HeraldEpoch,
          step12H3HeraldEpoch
        ]
    ]

bindingLossCounts :: [RuntimeTraceEvent] -> Map.Map PeerBinding Int
bindingLossCounts events =
  Map.fromListWith
    (+)
    [ (binding, 1 :: Int)
    | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
      RuntimeObserved (PeerBindingLost binding) <- [inputBody input]
    ]

lossCountsAllowed ::
  Map.Map PeerBinding Int ->
  Map.Map PeerBinding Int ->
  Bool
lossCountsAllowed expected =
  all
    (\(binding, count) -> Map.lookup binding expected == Just count)
    . Map.toList

connectionLifecycle ::
  [RuntimeTraceEvent] ->
  [(RuntimeEventOrdinal, ShellTraceEvent)]
connectionLifecycle events =
  [ (ordinal, event)
  | ShellEvent ordinal event <- events,
    case event of
      ShellConnectionRegistered {} -> True
      ShellConnectionPromoted {} -> True
      ShellConnectionClosed {} -> True
      ShellConnectionWriterFailed {} -> True
      _ -> False
  ]

data PeerDialPhysicalAttribution = PeerDialPhysicalAttribution
  { physicalPeerDialClass :: PeerDialClass,
    physicalPeerDialTarget :: (HeraldId, HeraldEpoch),
    physicalPeerDialGeneration :: Word64,
    physicalPeerDialPhase :: PeerDialProbePhase
  }
  deriving stock (Eq, Show)

data PeerDialPhysicalSummary = PeerDialPhysicalSummary
  { physicalPeerDialEvents :: Int,
    physicalPeerDialAttribution :: [(PeerDialPhysicalAttribution, Int)]
  }
  deriving stock (Show)

peerDialPhysicalSummary :: [PeerDialProbeEvent] -> PeerDialPhysicalSummary
peerDialPhysicalSummary events =
  PeerDialPhysicalSummary
    { physicalPeerDialEvents = length events,
      physicalPeerDialAttribution =
        [ (attribution, length (filter (== attribution) attributions))
        | attribution <- nub attributions
        ]
    }
  where
    attributions =
      [ PeerDialPhysicalAttribution
          { physicalPeerDialClass = peerDialIntentClass intent,
            physicalPeerDialTarget =
              ( peerDialIntentHeraldId intent,
                peerDialIntentHeraldEpoch intent
              ),
            physicalPeerDialGeneration =
              peerDialIntentGenerationWord64 intent,
            physicalPeerDialPhase = phase
          }
      | PeerDialProbeEvent intent phase <- events
      ]

data OracleSubmissionProgressKind
  = CommittedPrefixProgressKind
  | TerminalRequestProgressKind
  | ServiceReadyLeaderProgressKind
  deriving stock (Eq, Ord, Show)

data OracleManagerPhysicalSummary
  = OracleManagerPhysicalSummary
      [((OracleRetryPurpose, Word64, Int), Int)]
      [((OracleSubmissionProgressKind, Bool), Int)]

instance Show OracleManagerPhysicalSummary where
  show (OracleManagerPhysicalSummary retryPacing progress) =
    "OracleManagerPhysicalSummary {retryPacingByPurposeDelayCoalesced = "
      <> show retryPacing
      <> ", progressByKindAndUseful = "
      <> show progress
      <> "}"

oracleManagerPhysicalSummary ::
  [OracleManagerProbeEvent] ->
  OracleManagerPhysicalSummary
oracleManagerPhysicalSummary events =
  OracleManagerPhysicalSummary
    (counts retryPacing)
    (counts progress)
  where
    retryPacing =
      [ (purpose, delayMicroseconds, coalescedCount)
      | OracleRetryPaced
          purpose
          _retry
          delayMicroseconds
          coalescedCount <-
          events
      ]
    progress =
      [ (oracleSubmissionProgressKind event, useful)
      | OracleProgressObserved event useful <- events
      ]
    counts :: (Ord value) => [value] -> [(value, Int)]
    counts =
      Map.toList
        . Map.fromListWith (+)
        . fmap (\value -> (value, 1 :: Int))

oracleSubmissionProgressKind ::
  OracleSubmissionProgressEvent ->
  OracleSubmissionProgressKind
oracleSubmissionProgressKind = \case
  OracleCommittedPrefixProgress {} -> CommittedPrefixProgressKind
  OracleTerminalRequestProgress {} -> TerminalRequestProgressKind
  OracleServiceReadyLeaderProgress {} -> ServiceReadyLeaderProgressKind

data ReconnectTraceSummary = ReconnectTraceSummary
  { kernelSteps :: Int,
    oracleInputs :: Int,
    oracleReceiptMaintenanceInputs :: Int,
    oracleMaintenanceOnlyEntryBatches :: Int,
    oracleMaintenanceEntryDeliveries :: Int,
    oracleInputKinds :: [(String, Int)],
    oracleSubmissions :: Int,
    oracleSubmissionTags :: [(Word64, Int)],
    oracleSubmissionDrivers :: [(String, Int)],
    oracleRetriesScheduled :: Int,
    oracleRetriesCancelled :: Int,
    oracleRetrySchedulePurposes :: [(OracleRetryPurpose, Int)],
    oracleRetryScheduleDrivers :: [(String, Int)],
    oracleEntryBatches :: Int,
    oracleEntries :: Int,
    connectionPromotions :: Int,
    acceptedBindingAdmissions :: [(BindingAdmission, Int)],
    connectionClosures :: Int,
    connectionWriterFailures :: Int,
    peerHellos :: Int,
    bindingLosses :: Int,
    peerDialIntentsDelivered :: Int,
    peerDialIntentClasses :: [(PeerDialClass, Int)],
    peerDialIntentGenerations :: [((HeraldEpoch, Word64), Int)],
    dispatchSelections :: Int,
    dispatchSelectionRemotes :: [(HeraldEpoch, Int)],
    peerItemsSent :: Int,
    peerItemAttemptBindings :: [(PeerBinding, Int)],
    uniquePeerLogicalItems :: Int,
    peerWrittenOutcomes :: Int,
    uniqueWrittenPeerLogicalItems :: Int,
    dispatchDeferrals :: Int,
    physicalDispatchDeferrals :: Int,
    publicationsReceived :: Int,
    peerControls :: Int,
    frontierAdvances :: Int,
    resumeOffers :: Int,
    resumeAcceptances :: Int,
    reconnectCataloguePermissions :: Int,
    reconnectCatalogueReplayBatches :: Int,
    reconnectCatalogueReplayControls :: Int,
    reconnectCatalogueReplayControlsByBinding :: [(PeerBinding, Int)],
    receivedAcknowledgements :: Int,
    completedAcknowledgements :: Int,
    structuralReports :: Int,
    placementControls :: Int,
    topologyControls :: Int,
    alignmentControls :: Int,
    knownHeraldControls :: Int,
    invariantFaults :: Int
  }
  deriving stock (Show)

reconnectTraceSummary :: [RuntimeTraceEvent] -> ReconnectTraceSummary
reconnectTraceSummary events =
  ReconnectTraceSummary
    { kernelSteps =
        length [() | KernelEvent _ (KernelTraceStepped _ _ _ _) <- events],
      oracleInputs = countInput (\case OracleInput _ -> True; _ -> False),
      oracleReceiptMaintenanceInputs = countInput (\case OracleInput ingress -> isReceiptMaintenanceIngress ingress; _ -> False),
      oracleMaintenanceOnlyEntryBatches =
        countInput (\case OracleInput (OracleEntriesReceived _ entries) -> all (isReceiptMaintenanceEntry . canonicalAppliedOracleEntryValue) entries; _ -> False),
      oracleMaintenanceEntryDeliveries = length (filter isReceiptMaintenanceEntry (observedOracleEntries events)),
      oracleInputKinds = counts oracleIngresses,
      oracleSubmissions =
        length
          [ ()
          | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
            RunOracleClientAction (SubmitOracleRequest _ _) <- effectBatchMembers effects
          ],
      oracleSubmissionTags = counts submissionTags,
      oracleSubmissionDrivers = counts submissionDrivers,
      oracleRetriesScheduled = length retrySchedules,
      oracleRetriesCancelled = length cancelledRetries,
      oracleRetrySchedulePurposes = counts (fmap firstOfThree retrySchedules),
      oracleRetryScheduleDrivers = counts (fmap thirdOfThree retrySchedules),
      oracleEntryBatches = countInput (\case OracleInput (OracleEntriesReceived _ _) -> True; _ -> False),
      oracleEntries =
        sum
          [ NonEmpty.length entries
          | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
            OracleInput (OracleEntriesReceived _ entries) <- [inputBody input]
          ],
      connectionPromotions =
        length [() | ShellEvent _ (ShellConnectionPromoted _ _ _) <- events],
      acceptedBindingAdmissions = counts bindingAdmissions,
      connectionClosures =
        length [() | ShellEvent _ (ShellConnectionClosed _ _) <- events],
      connectionWriterFailures =
        length [() | ShellEvent _ (ShellConnectionWriterFailed _) <- events],
      peerHellos = countInput (\case PeerInput PeerHelloReceived {} -> True; _ -> False),
      bindingLosses = countInput (\case RuntimeObserved (PeerBindingLost _) -> True; _ -> False),
      peerDialIntentsDelivered = length deliveredDialIntents,
      peerDialIntentClasses = counts (fmap peerDialIntentClass deliveredDialIntents),
      peerDialIntentGenerations =
        counts
          [ ( peerDialIntentHeraldEpoch intent,
              peerDialIntentGenerationWord64 intent
            )
          | intent <- deliveredDialIntents
          ],
      dispatchSelections = countInput (\case PeerInput (PeerDispatchSelected _ _) -> True; _ -> False),
      dispatchSelectionRemotes =
        [ (remote, length (filter (== remote) selectedRemotes))
        | remote <- nub selectedRemotes
        ],
      peerItemsSent =
        length peerItemAttempts,
      peerItemAttemptBindings = counts (fmap fst peerItemAttempts),
      uniquePeerLogicalItems =
        length (nub (fmap (peerDispatchAttemptItem . snd) peerItemAttempts)),
      peerWrittenOutcomes = length writtenPeerAttempts,
      uniqueWrittenPeerLogicalItems =
        length (nub (fmap peerDispatchAttemptItem writtenPeerAttempts)),
      dispatchDeferrals =
        countInput
          ( \case
              RuntimeObserved (PeerDispatchObserved _ PeerDispatchDeferred) -> True
              _ -> False
          ),
      physicalDispatchDeferrals =
        length
          [ ()
          | ShellEvent _ (ShellPeerOutcomeWon _ _ PeerDispatchDeferred) <- events
          ],
      publicationsReceived = countInput (\case PeerInput (SemanticPeerPublicationReceived _ _) -> True; _ -> False),
      peerControls = countInput (\case PeerInput (SemanticPeerControlReceived _ _) -> True; _ -> False),
      frontierAdvances = countControl (\case PeerStreamFrontierAdvanced _ _ -> True; _ -> False),
      resumeOffers = countControl (\case PeerStreamResumeOffered _ -> True; _ -> False),
      resumeAcceptances = countControl (\case PeerStreamResumeAccepted _ -> True; _ -> False),
      reconnectCataloguePermissions = length bindingAdmissions,
      reconnectCatalogueReplayBatches =
        length (filter (not . null . snd) reconnectCatalogueBatches),
      reconnectCatalogueReplayControls =
        sum (fmap (length . snd) reconnectCatalogueBatches),
      reconnectCatalogueReplayControlsByBinding =
        [ (binding, sum [length controls | (current, controls) <- reconnectCatalogueBatches, current == binding])
        | binding <- nub (fmap fst reconnectCatalogueBatches)
        ],
      receivedAcknowledgements = countControl (\case PeerStreamReceived _ _ -> True; _ -> False),
      completedAcknowledgements = countControl (\case PeerStreamCompleted _ _ -> True; _ -> False),
      structuralReports = countControl (\case PeerStructuralAppliedReported _ -> True; _ -> False),
      placementControls =
        countControl
          ( \case
              PeerPlacementUpdate _ -> True
              PeerPlacementAcknowledged _ -> True
              _ -> False
          ),
      topologyControls =
        countControl
          ( \case
              PeerTopologyCutAnnounced _ -> True
              PeerTopologyCutAccepted _ -> True
              PeerTopologyCutEstablished _ -> True
              PeerTopologyCutEstablishedAcknowledged _ -> True
              _ -> False
          ),
      alignmentControls = countControl (\case SemanticPeerAlignmentControl _ -> True; _ -> False),
      knownHeraldControls = countControl (\case PeerKnownHeralds _ -> True; _ -> False),
      invariantFaults =
        length
          [ ()
          | KernelEvent _ (KernelTraceStepped _ _ _ (Left _)) <- events
          ]
    }
  where
    submissionTags =
      [ fromIntegral
          ( oracleCommandTag
              ( oracleEnvelopeCommand
                  ( canonicalOracleEnvelopeValue
                      (oracleRequestDispatchEnvelope dispatch)
                  )
              )
          )
      | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
        RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers effects
      ]
    oracleIngresses =
      [ oracleIngressConstructor ingress
      | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
        OracleInput ingress <- [inputBody input]
      ]
    submissionDrivers =
      [ inputSubmissionDriver (inputBody input)
      | KernelEvent _ (KernelTraceStepped _ _ input (Right effects)) <- events,
        RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers effects
      ]
    retrySchedules =
      [ (purpose, retry, inputSubmissionDriver (inputBody input))
      | KernelEvent _ (KernelTraceStepped _ _ input (Right effects)) <- events,
        RunOracleClientAction (ScheduleOracleRetry purpose retry) <- effectBatchMembers effects
      ]
    cancelledRetries =
      [ retry
      | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
        RunOracleClientAction (CancelOracleRetry retry) <- effectBatchMembers effects
      ]
    bindingAdmissions =
      [ admission
      | ShellEvent
          _
          ( ShellEffectRouted
              _
              _
              (SetPeerCandidateDisposition _ (PeerHelloAccepted admission _))
            ) <-
          events
      ]
    deliveredDialIntents =
      [ intent
      | ShellEvent _ (ShellPeerDialDelivered intent) <- events
      ]
    selectedRemotes =
      [ peerBindingRemoteHeraldEpoch binding
      | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
        PeerInput (PeerDispatchSelected _ binding) <- [inputBody input]
      ]
    peerItemAttempts =
      [ (binding, attempt)
      | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
        SemanticSendPeerItem binding attempt <- effectBatchMembers effects
      ]
    writtenPeerAttempts =
      [ attempt
      | ShellEvent _ shell <- events,
        attempt <- case shell of
          ShellPeerOutcomeWon _ current PeerDispatchWritten -> [current]
          ShellPeerOutcomeSuppressed _ current PeerDispatchWritten -> [current]
          _ -> []
      ]
    reconnectCatalogueBatches =
      [ (binding, repairControls)
      | KernelEvent _ (KernelTraceStepped _ _ input (Right effects)) <- events,
        PeerInput (SemanticPeerControlReceived binding (PeerStreamResumeOffered _)) <-
          [inputBody input],
        let sentControls =
              [ control
              | SemanticSendPeerControl target control <- effectBatchMembers effects,
                target == binding
              ],
        any isResumeAcceptance sentControls,
        any isReconnectCatalogueMarker sentControls,
        let repairControls = filter isReconnectRepairControl sentControls
      ]
    isResumeAcceptance = \case
      PeerStreamResumeAccepted _ -> True
      _ -> False
    isReconnectCatalogueMarker = \case
      PeerStructuralAppliedReported _ -> True
      _ -> False
    firstOfThree (first, _, _) = first
    thirdOfThree (_, _, third) = third
    countInput predicate =
      length
        [ ()
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
          predicate (inputBody input)
        ]
    countControl predicate =
      countInput
        ( \case
            PeerInput (SemanticPeerControlReceived _ control) -> predicate control
            _ -> False
        )
    counts values =
      [ (value, length (filter (== value) values))
      | value <- nub values
      ]

inputSubmissionDriver :: HeraldInputBody -> String
inputSubmissionDriver = \case
  ApplicationRequestInput {} -> "ApplicationRequest"
  OracleInput ingress -> "Oracle:" <> oracleIngressConstructor ingress
  PeerInput ingress -> case ingress of
    SemanticPeerControlReceived _ control ->
      "PeerControl:" <> takeWhile (/= ' ') (show control)
    PeerDispatchSelected {} -> "PeerDispatchSelected"
    PeerHelloReceived {} -> "PeerHelloReceived"
    SemanticPeerPublicationReceived {} -> "PeerPublicationReceived"
    _ -> "PeerInput"
  RuntimeObserved observation ->
    "RuntimeObserved:" <> takeWhile (/= ' ') (show observation)
  _ -> "Other"

oracleIngressConstructor :: OracleClientIngress -> String
oracleIngressConstructor = \case
  OracleContactsDiscovered {} -> "ContactsDiscovered"
  OracleLocalReplicaConfigured {} -> "ContactsDiscovered"
  OracleConnectFailed {} -> "ConnectFailed"
  OracleHelloReceived {} -> "HelloReceived"
  OracleRedirectReceived {} -> "RedirectReceived"
  OracleBindingLost {} -> "BindingLost"
  OracleRetryElapsed {} -> "RetryElapsed"
  OracleSubmissionNotReadyReceived {} -> "SubmissionNotReadyReceived"
  OracleSubmissionDeferredReceived {} -> "SubmissionDeferredReceived"
  OracleProgressConfirmed {} -> "ProgressConfirmed"
  OracleProgressFlush {} -> "ProgressFlush"
  OracleProgressNotReadyReceived {} -> "ProgressNotReadyReceived"
  OracleRequestRetiredReceived {} -> "RequestRetiredReceived"
  OracleEntriesReceived {} -> "EntriesReceived"

isReceiptMaintenanceIngress :: OracleClientIngress -> Bool
isReceiptMaintenanceIngress = \case
  OracleProgressConfirmed {} -> True
  OracleProgressFlush {} -> True
  OracleProgressNotReadyReceived {} -> True
  OracleRequestRetiredReceived {} -> True
  _ -> False

isReceiptMaintenanceEntry :: AppliedOracleEntry -> Bool
isReceiptMaintenanceEntry entry =
  appliedEntryCommand entry == Nothing && appliedEntryReceiptRetirement entry /= Nothing

observedOracleEntries :: [RuntimeTraceEvent] -> [AppliedOracleEntry]
observedOracleEntries events =
  [ canonicalAppliedOracleEntryValue canonical
  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
    OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
    canonical <- NonEmpty.toList entries
  ]

-- In this fixed-membership scenario each standalone announcement must release
-- an actual ordinary request or label decision for its home. Process piggyback
-- metadata too, so an already-released fact cannot justify another message.
-- Label-only progress is independent of how many requests that home issued.
assertReceiptMaintenanceAccounting :: [[RuntimeTraceEvent]] -> IO ()
assertReceiptMaintenanceAccounting snapshots = do
  let entries = Map.elems (Map.fromList [(appliedEntryControlIndex entry, entry) | entry <- concatMap observedOracleEntries snapshots])
      ordinary = Set.fromList [appliedEntryRequestId command | entry <- entries, Just command <- [appliedEntryCommand entry]]
      decisions = Set.fromList [appliedEntryControlIndex entry | entry <- entries, event <- appliedEntryProjectionEvents entry, LabelDecidedView {} <- [oracleProjectionEventView event]]
      maintenance = filter isReceiptMaintenanceEntry entries
      account promises entry = case appliedEntryOracleProgress entry of
        Nothing -> pure promises
        Just (home, offered) -> do
          let previous = Map.findWithDefault mempty home promises
              combined = previous <> offered
              newlyReleasedRequest request =
                oracleClientRequestHome request == home
                  && receiptIsRetired (oracleClientRequestSequence request) (oracleProgressReceipts combined)
                  && not (receiptIsRetired (oracleClientRequestSequence request) (oracleProgressReceipts previous))
              newlyReleasedDecision index = index > oracleProgressLabelsThrough previous && index <= oracleProgressLabelsThrough combined
          when (isReceiptMaintenanceEntry entry) $ do
            assertBool "standalone progress advances the full promise strictly" (offered /= previous && oracleProgressCovers offered previous)
            assertBool
              ("standalone progress releases a concrete request or label decision: " <> show (appliedEntryControlIndex entry, home, previous, offered))
              (any newlyReleasedRequest ordinary || any newlyReleasedDecision decisions)
          pure (Map.insert home combined promises)
  assertBool
    "standalone maintenance cannot outnumber the request and per-home label obligations it releases"
    (length maintenance <= Set.size ordinary + length snapshots * Set.size decisions)
  void (foldM account Map.empty entries)

-- | Check the causal shape of reconnect repair and keep its completed,
-- post-drain work below bounds calibrated from repeated four-Herald runs.
-- The limits retain scheduling headroom while remaining amplification guards
-- rather than latency targets; their exact values are part of the unchanged
-- Step-14 acceptance contract.
assertReconnectSemanticWork ::
  [[RuntimeTraceEvent]] ->
  [[RuntimeTraceEvent]] ->
  [[PeerDialProbeEvent]] ->
  [[OracleManagerProbeEvent]] ->
  IO String ->
  IO ReconnectTraceSummary
assertReconnectSemanticWork
  before
  after
  peerDialDeltas
  oracleManagerDeltas
  failureEvidence = do
    assertEqual
      "reconnect trace snapshots cover the same Herald owners"
      (length before)
      (length after)
    assertEqual
      "reconnect physical peer-dial snapshots cover the same Herald owners"
      (length before)
      (length peerDialDeltas)
    assertEqual
      "reconnect Oracle-manager snapshots cover the same Herald owners"
      (length before)
      (length oracleManagerDeltas)
    let deltas = zipWith (drop . length) before after
        aggregate = reconnectTraceSummary (concat deltas)
        physicalAggregate = peerDialPhysicalSummary (concat peerDialDeltas)
        oracleManagerAggregate =
          oracleManagerPhysicalSummary (concat oracleManagerDeltas)
        maintenanceDeliveries = sum [Set.size (Set.fromList [appliedEntryControlIndex entry | entry <- observedOracleEntries delta, isReceiptMaintenanceEntry entry]) | delta <- deltas]
        semanticOracleInputs = aggregate.oracleInputs - aggregate.oracleReceiptMaintenanceInputs - aggregate.oracleMaintenanceOnlyEntryBatches
    assertReceiptMaintenanceAccounting after
    forM_ (zip3 [1 :: Int ..] before deltas) $ \(herald, earlier, delta) -> do
      assertResumeOffersAreCausal herald earlier delta
      assertRepairTailOrdering herald earlier delta
    assertBool
      "the H4 reconnect exercised at least one completed resume exchange"
      (aggregate.resumeAcceptances > 0)
    assertEqual
      "post-reconnect work contains no Herald invariant fault"
      0
      aggregate.invariantFaults
    assertBool
      "post-reconnect logical Deferred outcomes stay bounded by the finite peer handoff"
      (aggregate.dispatchDeferrals <= 4)
    assertBool
      "post-reconnect physical Deferred outcomes stay bounded by the finite peer handoff"
      (aggregate.physicalDispatchDeferrals <= 4)
    let assertBound description upperBound observed =
          assertReconnectBound description upperBound observed aggregate physicalAggregate oracleManagerAggregate failureEvidence
    assertBound "kernel steps" 2_000 aggregate.kernelSteps
    assertBound "semantic Oracle inputs" 128 semanticOracleInputs
    assertBound "Oracle submissions" 320 aggregate.oracleSubmissions
    assertBound "peer controls" 1_500 aggregate.peerControls
    -- Structural reports certify an exact applied control prefix, so a new
    -- maintenance coordinate legitimately adds at most one report to each of
    -- the other three Heralds. Preserve the old semantic-work baseline.
    assertBound "structural reports including maintenance control progress" (220 + 3 * maintenanceDeliveries) aggregate.structuralReports
    assertBound "alignment controls" 1_000 aggregate.alignmentControls
    assertBound "resume offers" 64 aggregate.resumeOffers
    assertBound "resume acceptances" 64 aggregate.resumeAcceptances
    assertBound "dispatch selections" 128 aggregate.dispatchSelections
    pure aggregate

assertReconnectBound ::
  String ->
  Int ->
  Int ->
  ReconnectTraceSummary ->
  PeerDialPhysicalSummary ->
  OracleManagerPhysicalSummary ->
  IO String ->
  IO ()
assertReconnectBound
  description
  upperBound
  observed
  aggregate
  physicalAggregate
  oracleManagerAggregate
  failureEvidence =
    unless (observed <= upperBound) $ do
      evidence <- failureEvidence
      assertFailure
        ( "post-reconnect "
            <> description
            <> " exceeded "
            <> show upperBound
            <> "; observed aggregate="
            <> show aggregate
            <> "; physical peer-dial aggregate="
            <> show physicalAggregate
            <> "; Oracle manager pacing/progress aggregate="
            <> show oracleManagerAggregate
            <> evidence
        )

-- A full resume offer belongs either to physical connection establishment or
-- to the cumulative acknowledgement plan for a concrete retained gap.
-- Ordinary stream growth uses FrontierAdvanced and therefore cannot replay the
-- stream merely because new semantic work was enqueued. An identical offer on
-- one binding carries no new frontier/gap evidence and is therefore a feedback
-- loop even when it happens to share a batch with an acknowledgement.
assertResumeOffersAreCausal ::
  Int ->
  [RuntimeTraceEvent] ->
  [RuntimeTraceEvent] ->
  IO ()
assertResumeOffersAreCausal herald before events = do
  case [ (ordinal, inputBody input)
       | (ordinal, binding, _, input, effects) <- emissions,
         not (isPeerHello input),
         not (hasStreamAcknowledgement binding effects)
       ] of
    [] -> pure ()
    unexpected ->
      assertFailure
        ( "H"
            <> show herald
            <> " emitted ResumeOffered outside peer Hello or a stream "
            <> "acknowledgement repair plan; ordinary logical enqueue must use "
            <> "FrontierAdvanced: "
            <> show unexpected
        )
  case repeatedOffers priorOffers emissions of
    [] -> pure ()
    repeated ->
      assertFailure
        ( "H"
            <> show herald
            <> " repeated an identical ResumeOffered on one physical binding "
            <> "without new frontier/gap evidence: "
            <> show repeated
        )
  where
    priorOffers =
      [ (binding, offer)
      | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- before,
        SemanticSendPeerControl binding (PeerStreamResumeOffered offer) <-
          effectBatchMembers effects
      ]
    emissions =
      [ (ordinal, binding, offer, input, effects)
      | KernelEvent ordinal (KernelTraceStepped _ _ input (Right effects)) <- events,
        SemanticSendPeerControl binding (PeerStreamResumeOffered offer) <-
          effectBatchMembers effects
      ]

    repeatedOffers _ [] = []
    repeatedOffers seen ((ordinal, binding, offer, input, effects) : remaining)
      | (binding, offer) `elem` seen =
          ( ordinal,
            inputSummary input,
            effectSummary effects,
            binding,
            offer
          )
            : repeatedOffers seen remaining
      | otherwise =
          repeatedOffers ((binding, offer) : seen) remaining

    hasStreamAcknowledgement binding effects =
      any
        ( \case
            SemanticSendPeerControl acknowledgementBinding (PeerStreamReceived _ _) ->
              acknowledgementBinding == binding
            SemanticSendPeerControl acknowledgementBinding (PeerStreamCompleted _ _) ->
              acknowledgementBinding == binding
            _ -> False
        )
        (effectBatchMembers effects)
    isPeerHello input = case inputBody input of
      PeerInput PeerHelloReceived {} -> True
      _ -> False

    inputSummary input = case inputBody input of
      OracleInput ingress -> "OracleInput/" <> constructorName ingress
      PeerInput ingress -> "PeerInput/" <> constructorName ingress
      RuntimeObserved observation ->
        "RuntimeObserved/" <> constructorName observation
      body -> constructorName body

    effectSummary = fmap effectName . effectBatchMembers

    effectName = \case
      SemanticSendPeerControl _ control ->
        "SendPeerControl/" <> constructorName control
      effect -> constructorName effect

    constructorName :: (Show value) => value -> String
    constructorName = takeWhile (\character -> character /= ' ' && character /= '{') . show

-- A reconnect catalogue is attributed at its sender, where one atomic owner
-- step exposes both the ResumeOffered input and its ordered effects. The
-- explicit structural report is the catalogue marker used by
-- deduplicateStructuralReports; immutable alignment evidence without that
-- marker is ordinary newly-enabled work, even when another source sent the
-- same value earlier. Counting marked batches against exact admissions and
-- physical promotions makes this proof independent of a trace cut through the
-- receiving source FIFO.
assertRepairTailOrdering ::
  Int ->
  [RuntimeTraceEvent] ->
  [RuntimeTraceEvent] ->
  IO ()
assertRepairTailOrdering herald before events = do
  catalogueBatches <- concat <$> traverse inspectResumeStep resumeSteps
  forM_ (Map.toList (catalogueBatchesByBinding catalogueBatches))
    $ \(key@(source, binding), batches) -> do
      let admissionOrdinals =
            Map.findWithDefault [] key admissionsByBinding
          promotionOrdinals =
            Map.findWithDefault [] key promotionsByBinding
      forM_ (zip [1 :: Int ..] (sortOn fst batches))
        $ \(consumed, (ordinal, controls)) -> do
          let earlierAdmissions = filter (< ordinal) admissionOrdinals
              earlierPromotions = filter (< ordinal) promotionOrdinals
              allowance =
                min (length earlierAdmissions) (length earlierPromotions)
          assertBool
            ( "H"
                <> show herald
                <> " source "
                <> show source
                <> " binding "
                <> show binding
                <> " emitted catalogue batch at "
                <> show ordinal
                <> " beyond its cumulative admission/promotion allowance; "
                <> "consumed="
                <> show consumed
                <> "; allowance="
                <> show allowance
                <> "; earlier PeerHelloAccepted admissions="
                <> show earlierAdmissions
                <> "; earlier ShellConnectionPromoted events="
                <> show earlierPromotions
                <> "; indexed same-binding controls="
                <> show controls
            )
            (consumed <= allowance)
  where
    whole = before <> events
    resumeSteps =
      [ (ordinal, source, binding, indexedControls binding effects)
      | KernelEvent ordinal (KernelTraceStepped _ source input (Right effects)) <- whole,
        PeerInput (SemanticPeerControlReceived binding (PeerStreamResumeOffered _)) <-
          [inputBody input]
      ]
    inspectResumeStep (ordinal, source, binding, controls) = do
      let acceptanceIndices =
            [ index
            | (index, PeerStreamResumeAccepted _) <- controls
            ]
          markerIndices =
            [ index
            | (index, PeerStructuralAppliedReported _) <- controls
            ]
      case acceptanceIndices of
        [acceptanceIndex] ->
          case markerIndices of
            [] -> pure []
            _ -> do
              let repairControls =
                    [ (index, control)
                    | (index, control) <- controls,
                      isReconnectRepairControl control
                    ]
                  prematureRepairs =
                    filter ((<= acceptanceIndex) . fst) repairControls
              unless (null prematureRepairs)
                $ assertFailure
                  ( "H"
                      <> show herald
                      <> " source "
                      <> show source
                      <> " binding "
                      <> show binding
                      <> " emitted catalogue controls before ResumeAccepted at "
                      <> show ordinal
                      <> "; acceptance index="
                      <> show acceptanceIndex
                      <> "; structural marker indices="
                      <> show markerIndices
                      <> "; premature indexed repair controls="
                      <> show prematureRepairs
                      <> "; indexed same-binding controls="
                      <> show controls
                  )
              pure [(source, binding, ordinal, controls)]
        _ ->
          assertFailure
            ( "H"
                <> show herald
                <> " source "
                <> show source
                <> " binding "
                <> show binding
                <> " handled ResumeOffered at "
                <> show ordinal
                <> " without exactly one same-binding ResumeAccepted; "
                <> "acceptance indices="
                <> show acceptanceIndices
                <> "; indexed same-binding controls="
                <> show controls
            )
    indexedControls binding effects =
      [ (index, control)
      | (index, SemanticSendPeerControl target control) <-
          zip [0 :: Int ..] (effectBatchMembers effects),
        target == binding
      ]
    admissionsByBinding =
      Map.fromListWith
        (flip (<>))
        [ ((source, binding), [ordinal])
        | KernelEvent ordinal (KernelTraceStepped _ source _ (Right effects)) <- whole,
          SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <-
            effectBatchMembers effects
        ]
    promotionsByBinding =
      Map.fromListWith
        (flip (<>))
        [ ((source, binding), [ordinal])
        | ShellEvent
            ordinal
            ( ShellConnectionPromoted
                _
                source
                (PeerConnectionPromoted binding)
              ) <-
            whole
        ]
    catalogueBatchesByBinding =
      Map.fromListWith
        (flip (<>))
        . fmap
          ( \(source, binding, ordinal, controls) ->
              ((source, binding), [(ordinal, controls)])
          )

isReconnectRepairControl :: PeerControl -> Bool
isReconnectRepairControl = \case
  PeerStructuralAppliedReported _ -> True
  PeerTopologyCutAnnounced _ -> True
  PeerTopologyCutEstablished _ -> True
  SemanticPeerAlignmentControl _ -> True
  _ -> False

awaitExactApplicationInput ::
  String ->
  Step14Deployment ->
  TraceCursor ->
  ApplicationOperation ->
  IO (RequestId, RuntimeEventOrdinal)
awaitExactApplicationInput context deployment initialCursor expected = do
  observed <- timeout 10_000_000 (loop initialCursor)
  case observed of
    Nothing -> assertFailure (context <> " did not enter H1")
    Just request -> pure request
  where
    loop cursor = do
      (nextCursor, events) <-
        snapshotStep14HeraldTraceSince cursor (step14H1 deployment)
      case [ (request, ordinal)
           | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events,
             ApplicationRequestInput
               (CallApplicationRequest _ _ request operation) <-
               [semanticStep14InputBody input],
             operation == expected
           ] of
        [] -> threadDelay 10_000 >> loop nextCursor
        [observedInput] -> pure observedInput
        requests ->
          assertFailure
            (context <> " entered H1 more than once: " <> show requests)

awaitMessageWithin ::
  String ->
  EAPP.Application ->
  Step14ApplicationTopology ->
  Text ->
  IO ()
awaitMessageWithin context application topology message = do
  observed <- timeout 30_000_000 (awaitStep14Message application topology message)
  unless (observed == Just ())
    $ assertFailure (context <> " did not become visible")

preFenceMessage, postFenceMessage :: Text
preFenceMessage = "01 pre-fence"
postFenceMessage = "02 post-fence"
