{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Canonical End may follow an Applied decision before its installation
-- collection completes. H4 retains its already-installed report at the runtime
-- dispatch boundary; the immutable Applied result survives that later End.
module Step14DeferredEndProperties (tests) where

import Control.Concurrent
  ( MVar,
    readMVar,
    threadDelay,
  )
import Control.Concurrent.MVar
  ( newEmptyMVar,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( TMVar,
    atomically,
  )
import Control.Exception (finally)
import Control.Monad (unless, void, when)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    startupAccessProcess,
  )
import Eclips.Application.Types.Identity (asPrivateObjectId)
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToProcess),
    LabelResult (LabelApplied),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabel,
    ApplicationLabelOwner (ProcessLabel, ZombieLabel),
    ApplicationValue (LabelValue, RecordValue),
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    LabelDecisionId,
    ProcessEpochId,
    processEpochIdBytes,
    systemIdBytes,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ExplicitAdministrativeEnd),
  )
import Eclips.Herald.Administration qualified as Administration
import Eclips.Herald.EffectBatch
  ( HeraldEffect
      ( SendAdministrationReply
      ),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (OracleInput),
    PeerControl (PeerLabelInstalled),
    inputBody,
  )
import Eclips.Herald.OracleClient (OracleClientIngress (OracleEntriesReceived))
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (batchEnvelopeEffects),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent),
    TraceLedger,
    snapshotTraceLedger,
  )
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue)
import Eclips.Oracle.Label (LiveTerminalOutcomeView (LiveReleasedOutcomeView), liveDecisionId, liveTerminalOutcomeView)
import Eclips.Oracle.Projection
  ( OracleProjectionEventView
      ( LabelDecidedView,
        LabelWorkflowCompletedView,
        ProcessEpochEndedEventView
      ),
    appliedEntryControlIndex,
    appliedEntryProjectionEvents,
    oracleProjectionEventView,
  )
import Eclips.Protocol.Admin.Types
  ( AdminClientDto (EndProcessEpoch),
    AdminCompletedResultDto (ProcessEpochEndedDto),
    AdminCorrelationIdClaim,
    AdminDeploymentIdClaim,
    AdminProcessEndReason (AdminExplicitAdministrativeEnd),
    AdminProcessEpochIdClaim,
    AdminResultStatusDto (AdminAccepted, AdminCompleted),
    AdminServerDto (AdminResult),
    adminCorrelationIdClaim,
    adminDeploymentIdClaim,
    adminProcessEpochIdClaim,
  )
import Eclips.Protocol.Application.Types (applicationClientNonce)
import PeerEvidence (data SemanticSendPeerControl)
import Step12Fixtures
  ( step12H2ProcessEpochId,
    step12SystemId,
  )
import Step14AdministrationClient
  ( Step14AdministrationClient,
    Step14AdministrationReceive (..),
    receiveStep14AdministrationForWithin,
    sendStep14AdministrationRequest,
    withStep14AdministrationClient,
  )
import Step14ApplicationWorkflow
  ( Step14ApplicationTopology (..),
    establishStep14ApplicationTopology,
    expectStep14ApplicationLabel,
    expectStep14LabelResult,
    readStep14SequencingCarrier,
  )
import Step14Fixtures
  ( Step14Deployment,
    Step14Herald,
    Step14HeraldName (Step14H4),
    awaitStep14CellWithin,
    awaitStep14OraclePrefix,
    awaitStep14PeerMesh,
    newStep14Cell,
    publishStep14Cell,
    signalStep14Latch,
    snapshotStep14HeraldTrace,
    step14AdministrationEndpoint,
    step14ApplicationLivenessConfiguration,
    step14H2,
    step14PApplicationAttachment,
    step14PApplicationEndpoint,
    step14QApplicationAttachment,
    step14QApplicationEndpoint,
    withStep14DeploymentWithHooks,
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
    "Step-14 End during installation collection"
    [ testCase
        "End supersedes the effective label while the successful installation report is delayed"
        caseSelectedDestinationEndIsDeferred
    ]

caseSelectedDestinationEndIsDeferred :: Assertion
caseSelectedDestinationEndIsDeferred = do
  h4Ledger <- newEmptyMVar
  pauseClaimed <- newEmptyMVar
  pausedDecision <- newStep14Cell
  releaseH4 <- newEmptyMVar
  let hooksFor = \case
        Step14H4 ->
          h4InstallationReportPause
            h4Ledger
            pauseClaimed
            pausedDecision
            releaseH4
        _ -> defaultRuntimeHooks
      scenario deployment =
        runDeferredEndScenario pausedDecision releaseH4 deployment
          `finally` signalStep14Latch releaseH4
  completed <-
    timeout
      scenarioTimeoutMicroseconds
      (withStep14DeploymentWithHooks hooksFor scenario)
  unless (completed == Just ())
    $ assertFailure "the End-during-collection real-TCP schedule exceeded its failure bound"

-- | Pause the first report batch after H4 installs its immutable decision.
h4InstallationReportPause ::
  MVar TraceLedger ->
  MVar () ->
  TMVar LabelDecisionId ->
  MVar () ->
  RuntimeHooks
h4InstallationReportPause ledgerCell claimed pausedDecision release =
  defaultRuntimeHooks
    { hookRuntimeInitialized = \_ _ _ _ _ _ ledger ->
        void (tryPutMVar ledgerCell ledger),
      hookBeforeDispatchBatch = \batch ->
        when (containsInstallationReport batch) $ do
          ledger <- readMVar ledgerCell
          events <- atomically (snapshotTraceLedger ledger)
          case resolvedWillApplyDecisions events of
            [] -> pure ()
            decisions -> do
              first <- tryPutMVar claimed ()
              when first $ do
                published <- publishStep14Cell pausedDecision (last decisions)
                unless published
                  $ assertFailure "H4 published its paused label decision more than once"
                readMVar release
    }

containsInstallationReport :: BatchEnvelope -> Bool
containsInstallationReport =
  any isSubmission . effectBatchMembers . batchEnvelopeEffects
  where
    isSubmission = \case
      SemanticSendPeerControl _ PeerLabelInstalled {} -> True
      _ -> False

runDeferredEndScenario ::
  TMVar LabelDecisionId ->
  MVar () ->
  Step14Deployment ->
  IO ()
runDeferredEndScenario pausedDecision releaseH4 deployment = do
  awaitStep14PeerMesh deployment
  withStep14PTopology
    (signalStep14Latch releaseH4)
    deployment
    $ \pApplication pStartup topology -> do
      labelCall <-
        EAPP.label
          pApplication
          (asPrivateObjectId topology.pSequencingObject)
          ((ProcessLabel (startupAccessProcess pStartup), 0))
          (LabelToProcess topology.localizedQProcess)

      observedDecision <-
        awaitStep14CellWithin pauseTimeoutMicroseconds pausedDecision
      decision <- case observedDecision of
        Nothing ->
          assertFailure
            "H4 did not retain its installation report after the Applied decision"
        Just value -> pure value

      let h2 = step14H2 deployment
          endpoint = step14AdministrationEndpoint h2
      withStep14AdministrationClient endpoint deploymentClaim $ \client -> do
        sendStep14AdministrationRequest client endRequest
        expectAdministrationResult
          "End(Q) is accepted while H4 retains its installation report"
          AdminAccepted
          client

        let terminal = AdminCompleted (ProcessEpochEndedDto qProcessClaim)
        expectAdministrationResult "End(Q) completes while H4 retains its report" terminal client
        beforeRelease <- snapshotStep14HeraldTrace h2
        assertEqual "the original End(Q) correlation emits one terminal reply" 1 (endTerminalReplyCount beforeRelease)
        assertBool
          "End(Q) has its canonical projection before collection completes"
          (not (null (processEndProjectionIndices step12H2ProcessEpochId beforeRelease)))
        assertEqual
          "the label still has no canonical completion"
          []
          [observed | (_, LabelWorkflowCompletedView observed _) <- projectionViews beforeRelease, observed == decision]

        signalStep14Latch releaseH4
        expectStep14LabelResult
          "the P-to-Q sequencing label applies after H4 releases its installation report"
          LabelApplied
          labelCall
        awaitStep14OraclePrefix 3 deployment
        awaitCompletedDecision h2 decision

        sendStep14AdministrationRequest client endRequest
        expectAdministrationResult
          "an exact End(Q) retry replays the retained terminal result"
          terminal
          client
        afterRetry <- snapshotStep14HeraldTrace h2
        assertEqual
          "the exact retry emits one additional retained terminal reply"
          2
          (endTerminalReplyCount afterRetry)

        assertWorkflowBeforeEnd decision afterRetry
        awaitZombieCarrier pApplication topology

withStep14PTopology ::
  IO () ->
  Step14Deployment ->
  ( EAPP.Application ->
    ApplicationStartupAccess ->
    Step14ApplicationTopology ->
    IO result
  ) ->
  IO result
withStep14PTopology releaseH4 deployment use = do
  pResult <-
    EAPP.withApplication
      ( EAPP.applicationConfiguration
          (step14PApplicationEndpoint deployment)
          step14PApplicationAttachment
          (applicationClientNonce 14_801)
          step14ApplicationLivenessConfiguration
      )
      ( \pApplication pStartup -> do
          qResult <-
            EAPP.withApplication
              ( EAPP.applicationConfiguration
                  (step14QApplicationEndpoint deployment)
                  step14QApplicationAttachment
                  (applicationClientNonce 14_802)
                  step14ApplicationLivenessConfiguration
              )
              ( \qApplication qStartup -> do
                  topology <- establishTopology pApplication pStartup qApplication qStartup
                  -- Keep Q's application lifetime through the intentionally
                  -- deferred administrative End, rather than closing it here.
                  use pApplication pStartup topology
                    `finally` releaseH4
              )
          requireApplicationResult "Q" qResult
      )
  requireApplicationResult "P" pResult
  where
    establishTopology ::
      EAPP.Application ->
      ApplicationStartupAccess ->
      EAPP.Application ->
      ApplicationStartupAccess ->
      IO Step14ApplicationTopology
    establishTopology pApplication pStartup qApplication qStartup =
      establishStep14ApplicationTopology
        pApplication
        pStartup
        qApplication
        qStartup

requireApplicationResult ::
  String ->
  Either EAPP.ApplicationRuntimeFailure result ->
  IO result
requireApplicationResult name = \case
  Left failure ->
    assertFailure (name <> " application runtime failed: " <> show failure)
  Right result -> pure result

awaitCompletedDecision ::
  Step14Herald ->
  LabelDecisionId ->
  IO ()
awaitCompletedDecision herald expected = do
  observed <- timeout replyTimeoutMicroseconds loop
  unless (observed == Just ())
    $ assertFailure
      "H2 did not project canonical completion for the installed decision"
  where
    loop = do
      events <- snapshotStep14HeraldTrace herald
      if expected `elem` [observed | (_, LabelWorkflowCompletedView observed _) <- projectionViews events]
        then pure ()
        else threadDelay 10_000 >> loop

expectAdministrationResult ::
  String ->
  AdminResultStatusDto ->
  Step14AdministrationClient ->
  Assertion
expectAdministrationResult context expected client = do
  observed <-
    receiveStep14AdministrationForWithin
      replyTimeoutMicroseconds
      endCorrelationClaim
      client
  case observed of
    Step14AdministrationReceived actual ->
      assertEqual
        context
        (AdminResult endCorrelationClaim expected)
        actual
    Step14AdministrationReceiveFailed failure ->
      assertFailure (context <> ": receive failed: " <> show failure)
    Step14AdministrationReceiveTimedOut ->
      assertFailure (context <> ": timed out")

assertWorkflowBeforeEnd ::
  LabelDecisionId ->
  [RuntimeTraceEvent] ->
  Assertion
assertWorkflowBeforeEnd decision events = do
  workflowIndex <-
    requireOnly
      "the exact label workflow-completion projection"
      [ index
      | (index, LabelWorkflowCompletedView observed _) <- projectionViews events,
        observed == decision
      ]
  endIndex <-
    requireOnly
      "the exact End(Q) projection"
      (processEndProjectionIndices step12H2ProcessEpochId events)
  decisionIndex <-
    requireOnly
      "the exact Applied label decision"
      [ index
      | (index, LabelDecidedView observed terminal _) <- projectionViews events,
        liveDecisionId observed == decision,
        LiveReleasedOutcomeView {} <- [liveTerminalOutcomeView terminal]
      ]
  assertBool
    "decision < End(Q) < canonical installation completion"
    (decisionIndex < endIndex && endIndex < workflowIndex)

awaitZombieCarrier ::
  EAPP.Application ->
  Step14ApplicationTopology ->
  IO ()
awaitZombieCarrier application topology = do
  observed <- timeout replyTimeoutMicroseconds loop
  carrier <- case observed of
    Nothing ->
      assertFailure
        "P did not observe its ended Q destination as a zombie label"
    Just value -> pure value
  expectStep14ApplicationLabel
    "P reads ZombieLabel(Q) after the ordered End projects"
    ((ZombieLabel topology.localizedQProcess, 1))
    carrier
  where
    expected = (ZombieLabel topology.localizedQProcess, 1)
    loop = do
      carrier <- readStep14SequencingCarrier application topology
      if carrierLabel carrier == Just expected
        then pure carrier
        else threadDelay 10_000 >> loop

carrierLabel :: ApplicationValue -> Maybe ApplicationLabel
carrierLabel = \case
  RecordValue fields -> case Map.lookup "label" fields of
    Just (LabelValue value) -> Just value
    _ -> Nothing
  _ -> Nothing

resolvedWillApplyDecisions :: [RuntimeTraceEvent] -> [LabelDecisionId]
resolvedWillApplyDecisions events =
  [ liveDecisionId decision
  | (_, LabelDecidedView decision terminal _) <- projectionViews events,
    LiveReleasedOutcomeView {} <- [liveTerminalOutcomeView terminal]
  ]

processEndProjectionIndices ::
  ProcessEpochId ->
  [RuntimeTraceEvent] ->
  [ControlIndex]
processEndProjectionIndices process events =
  [ index
  | (index, ProcessEpochEndedEventView observed _ ExplicitAdministrativeEnd) <-
      projectionViews events,
    observed == process
  ]

projectionViews ::
  [RuntimeTraceEvent] ->
  [(ControlIndex, OracleProjectionEventView)]
projectionViews events =
  [ (appliedEntryControlIndex applied, oracleProjectionEventView event)
  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
    OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
    canonical <- NonEmpty.toList entries,
    let applied = canonicalAppliedOracleEntryValue canonical,
    event <- appliedEntryProjectionEvents applied
  ]

endTerminalReplyCount :: [RuntimeTraceEvent] -> Int
endTerminalReplyCount events =
  length
    [ ()
    | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
      SendAdministrationReply
        binding
        ( Administration.RetainedAdminResult
            correlation
            (Administration.AdminEndRequestCompleted process)
          ) <-
        effectBatchMembers effects,
      correlation == endCorrelation binding,
      process == step12H2ProcessEpochId
    ]

requireOnly :: (Show value) => String -> [value] -> IO value
requireOnly context = \case
  [value] -> pure value
  observed ->
    assertFailure (context <> ": observed " <> show observed)

deploymentClaim :: AdminDeploymentIdClaim
deploymentClaim =
  checked
    "Step-14 deferred-End deployment claim"
    (adminDeploymentIdClaim (systemIdBytes step12SystemId))

qProcessClaim :: AdminProcessEpochIdClaim
qProcessClaim =
  checked
    "Step-14 Q process-epoch claim"
    (adminProcessEpochIdClaim (processEpochIdBytes step12H2ProcessEpochId))

endCorrelation :: Administration.AdministrationBinding -> Administration.AdminCorrelationId
endCorrelation binding = Administration.adminCorrelationIdForBinding binding 14_801

endCorrelationClaim :: AdminCorrelationIdClaim
endCorrelationClaim = adminCorrelationIdClaim 14_801

endRequest :: AdminClientDto
endRequest =
  EndProcessEpoch
    endCorrelationClaim
    qProcessClaim
    AdminExplicitAdministrativeEnd

scenarioTimeoutMicroseconds, pauseTimeoutMicroseconds, replyTimeoutMicroseconds :: Int
scenarioTimeoutMicroseconds = 60_000_000
pauseTimeoutMicroseconds = 15_000_000
replyTimeoutMicroseconds = 10_000_000

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
