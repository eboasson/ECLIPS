{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Real-TCP proof that losing the physical delivery of a successful Label
-- reply ends the application connection lifetime without repeating the
-- semantic operation. The Herald retains the committed release independently
-- of the now unavailable application result.
module Step14ApplicationReplyLossProperties (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    readMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Exception (throwIO)
import Control.Monad (unless, void, when)
import Data.List.NonEmpty qualified as NonEmpty
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
  ( ApplicationOperation (LabelApplication),
  )
import Eclips.Application.Types.Result
  ( OperationPendingReason (LabelSettlementPending),
    RegularCallResult (LabelCompleted),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabel,
    ApplicationLabelOwner (ProcessLabel),
  )
import Eclips.Domain.Label (ReleasedLabelStateView (ReleasedLabelView), releasedLabelStateView)
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ApplicationPermanentlyLost))
import Eclips.Domain.Value qualified as DomainValue
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RequestId,
    RetainedRequestReplyBody (Completed, OperationAccepted),
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionUnavailableReason (ApplicationSessionNoLongerLive),
    sessionBindingSessionId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (DisposeApplicationSession, SendApplicationReply),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( ApplicationReceiptRetirementIngress (RetireApplicationReceipts),
    ApplicationRequestIngress (CallApplicationRequest),
    ApplicationRetirementWork (RetiringApplicationRequest),
    ApplicationSessionIngress (ResumeApplicationSession),
    HeraldInputBody (ApplicationReceiptRetirementInput, ApplicationRequestInput, ApplicationSessionInput, OracleInput, RuntimeObserved),
    RuntimeObservation (ApplicationBindingLost),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientIngress (OracleEntriesReceived),
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (batchEnvelopeEffects),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( ConnectionPromotion (ApplicationConnectionPromoted),
    KernelTraceStep (KernelTraceStepped),
    PhysicalConnectionRef (PhysicalApplicationRef),
    PhysicalLaneClass (ApplicationLane),
    RuntimeEventOrdinal,
    RuntimeTraceEvent (..),
    ShellTraceEvent
      ( ShellConnectionClosed,
        ShellConnectionPromoted,
        ShellConnectionWriterFailed,
        ShellEffectRouted,
        ShellLaneOffered
      ),
  )
import Eclips.Oracle.Canonical
  ( canonicalAppliedOracleEntryValue,
  )
import Eclips.Oracle.Label
  ( LiveTerminalOutcomeView (LiveReleasedOutcomeView),
    liveDecisionId,
    liveRecordStoredState,
    liveTerminalOutcomeView,
  )
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (..),
    appliedEntryProjectionEvents,
    oracleProjectionEventView,
  )
import Eclips.Protocol.Application.Types (applicationClientNonce)
import Step12Fixtures (step12H1ProcessEpochId)
import Step14ApplicationWorkflow
  ( Step14ApplicationTopology (..),
    establishStep14ApplicationTopology,
    expectStep14ApplicationLabel,
    readStep14SequencingCarrier,
  )
import Step14Fixtures
  ( Step14Deployment,
    Step14HeraldName (Step14H1),
    awaitStep14PeerMesh,
    snapshotStep14HeraldTrace,
    step14ApplicationLivenessConfiguration,
    step14H1,
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
    "Step-14 application reply loss"
    [ testCase
        "a lost successful Label reply ends its session while the release remains unique"
        caseSuccessfulLabelReplyLost
    ]

caseSuccessfulLabelReplyLost :: Assertion
caseSuccessfulLabelReplyLost = do
  acceptanceObserved <- newEmptyMVar
  acceptanceDequeued <- newEmptyMVar
  terminalArmed <- newEmptyMVar
  failedPhysical <- newEmptyMVar
  let hooksFor = \case
        Step14H1 ->
          defaultRuntimeHooks
            { hookBeforeDispatchBatch =
                observeLabelReplyBatches
                  acceptanceObserved
                  acceptanceDequeued
                  terminalArmed,
              hookAfterWriterCommandDequeued =
                observeAcceptanceAndFailTerminal
                  acceptanceObserved
                  acceptanceDequeued
                  terminalArmed
                  failedPhysical
            }
        _ -> defaultRuntimeHooks
  completed <-
    timeout 75_000_000
      $ withStep14DeploymentWithHooks hooksFor
      $ \deployment -> do
        awaitStep14PeerMesh deployment
        withStep14Applications deployment
          $ \pApplication pStartup qApplication qStartup -> do
            topology <-
              establishStep14ApplicationTopology
                pApplication
                pStartup
                qApplication
                qStartup
            original <- readStep14SequencingCarrier pApplication topology
            expectStep14ApplicationLabel
              "the sequencing target begins under P"
              ((ProcessLabel (startupAccessProcess pStartup), 0))
              original

            labelCall <-
              EAPP.label
                pApplication
                (asPrivateObjectId topology.pSequencingObject)
                ((ProcessLabel (startupAccessProcess pStartup), 0))
                LabelToVoid
            failed <- timeout 20_000_000 (readMVar failedPhysical)
            failedLane <- case failed of
              Nothing ->
                assertFailure
                  "the successful Label terminal reply never reached the injected writer loss"
              Just physical -> pure physical

            completion <- timeout 30_000_000 (EAPP.awaitApplicationCall labelCall)
            assertEqual
              "the original Label call becomes unknown when its established connection is lost"
              (Just (EAPP.ApplicationCallUnknown EAPP.SessionNoLongerLive))
              completion
            assertEqual
              "the application observes its terminal lifetime boundary"
              EAPP.SessionNoLongerLive
              =<< EAPP.awaitApplicationUnavailable pApplication
            later <-
              EAPP.label
                pApplication
                (asPrivateObjectId topology.pSequencingObject)
                ((ProcessLabel (startupAccessProcess pStartup), 0))
                LabelToVoid
            assertEqual
              "a closed application cannot issue another semantic label"
              EAPP.ApplicationCallClosed
              =<< EAPP.awaitApplicationCall later
            events <- awaitApplicationLossTrace deployment
            terminalRequest <- readMVar terminalArmed
            assertApplicationLossTrace
              ((ProcessLabel (startupAccessProcess pStartup), 0))
              topology
              terminalRequest
              failedLane
              events
  unless (completed == Just ())
    $ assertFailure "the application-reply-loss schedule exceeded its failure bound"

observeLabelReplyBatches ::
  MVar RequestId ->
  MVar () ->
  MVar RequestId ->
  BatchEnvelope ->
  IO ()
observeLabelReplyBatches acceptanceObserved acceptanceDequeued armed batch = do
  case labelAcceptanceRequests batch of
    [] -> pure ()
    [request] -> void (tryPutMVar acceptanceObserved request)
    requests ->
      assertFailure
        ("one batch emitted duplicate Label acceptances: " <> show requests)
  case successfulLabelTerminalRequests batch of
    [] -> pure ()
    [request] -> do
      accepted <- tryReadMVar acceptanceObserved
      assertEqual
        "the terminal reply names the accepted Label request"
        (Just request)
        accepted
      dequeued <- timeout 5_000_000 (readMVar acceptanceDequeued)
      unless (dequeued == Just ())
        $ assertFailure "the Label acceptance was not dequeued before its terminal"
      void (tryPutMVar armed request)
    requests ->
      assertFailure
        ("one batch emitted duplicate successful Label terminals: " <> show requests)

labelAcceptanceRequests :: BatchEnvelope -> [RequestId]
labelAcceptanceRequests batch =
  [ request
  | SendApplicationReply
      _
      ( RetainedRequestReply
          _
          (OperationAccepted request LabelSettlementPending)
        ) <-
      effectBatchMembers (batchEnvelopeEffects batch)
  ]

successfulLabelTerminalRequests :: BatchEnvelope -> [RequestId]
successfulLabelTerminalRequests batch =
  [ request
  | SendApplicationReply
      _
      ( RetainedRequestReply
          _
          (Completed request (LabelCompleted LabelApplied))
        ) <-
      effectBatchMembers (batchEnvelopeEffects batch)
  ]

observeAcceptanceAndFailTerminal ::
  MVar RequestId ->
  MVar () ->
  MVar RequestId ->
  MVar PhysicalConnectionRef ->
  PhysicalConnectionRef ->
  IO ()
observeAcceptanceAndFailTerminal acceptanceObserved acceptanceDequeued armed failed physical@PhysicalApplicationRef {} = do
  terminal <- tryReadMVar armed
  case terminal of
    Nothing -> do
      accepted <- tryReadMVar acceptanceObserved
      case accepted of
        Nothing -> pure ()
        Just _ -> void (tryPutMVar acceptanceDequeued ())
    Just _ -> do
      first <- tryPutMVar failed physical
      when first
        $ throwIO (userError "intentional loss of a successful Label reply")
observeAcceptanceAndFailTerminal _ _ _ _ _ = pure ()

awaitApplicationLossTrace ::
  Step14Deployment ->
  IO [RuntimeTraceEvent]
awaitApplicationLossTrace deployment = do
  observed <- timeout 10_000_000 loop
  case observed of
    Nothing ->
      assertFailure "H1 did not expose session disposal, completed Label and automatic process End"
    Just events -> pure events
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      if not (null (applicationWriterFailures events))
        && not (null (applicationTerminations events))
        && not (null [() | LabelWorkflowCompletedView {} <- oracleProjectionViews events])
        && not (null [() | ProcessEpochEndedEventView process _ ApplicationPermanentlyLost <- oracleProjectionViews events, process == step12H1ProcessEpochId])
        then pure events
        else threadDelay 10_000 >> loop

assertApplicationLossTrace ::
  ApplicationLabel ->
  Step14ApplicationTopology ->
  RequestId ->
  PhysicalConnectionRef ->
  [RuntimeTraceEvent] ->
  Assertion
assertApplicationLossTrace expectedLabel topology terminalRequest failedLane events = do
  (initialOrdinal, initialPhysical, initialBinding) <-
    case applicationPromotions events of
      [promotion] -> pure promotion
      promotions -> assertFailure ("expected exactly the original P application promotion, got " <> show promotions)
  assertEqual
    "the client never requests Resume after losing its session"
    []
    [ ordinal
    | KernelEvent ordinal (KernelTraceStepped _ _ input (Right _)) <- events,
      ApplicationSessionInput ResumeApplicationSession {} <- [inputBody input]
    ]

  terminalRouteOrdinal <- case terminalRouteEvents terminalRequest events of
    [ordinal] -> pure ordinal
    routes ->
      assertFailure
        ("expected one routed terminal Label reply, got " <> show routes)
  failureOrdinal <- case applicationWriterFailures events of
    [(ordinal, physical)] -> do
      assertEqual
        "the injected writer failure belongs to P's original physical lane"
        initialPhysical
        physical
      assertEqual
        "the hook and runtime identify the same failed physical lane"
        failedLane
        physical
      pure ordinal
    failures ->
      assertFailure
        ("expected one application writer failure, got " <> show failures)
  closeOrdinal <- case applicationCloseEvents events of
    [(ordinal, physical)] -> do
      assertEqual
        "the writer failure closes P's original physical lane"
        initialPhysical
        physical
      pure ordinal
    closes ->
      assertFailure
        ("expected one application connection close, got " <> show closes)
  terminationOrdinal <- case applicationTerminations events of
    [(ordinal, session)] -> do
      assertEqual "the loss removes the admitted session" (sessionBindingSessionId initialBinding) session
      pure ordinal
    terminations -> assertFailure ("expected one immediate session termination, got " <> show terminations)
  assertBool
    "terminal routing, writer loss, physical close and owner disposal are ordered"
    ( initialOrdinal < terminalRouteOrdinal
        && terminalRouteOrdinal < failureOrdinal
        && failureOrdinal < closeOrdinal
        && closeOrdinal < terminationOrdinal
    )
  assertEqual
    "the failed terminal command was never offered to the TCP lane"
    []
    [ ordinal
    | (ordinal, physical) <- applicationLaneOffers events,
      physical == failedLane,
      terminalRouteOrdinal < ordinal,
      ordinal < failureOrdinal
    ]

  let expectedOperation =
        LabelApplication
          (asPrivateObjectId topology.pSequencingObject)
          expectedLabel
          LabelToVoid
      labelRequests =
        [ request
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
          (request, operation) <- labelRequestInput (inputBody input),
          operation == expectedOperation
        ]
      terminalReplies =
        [ request
        | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
          SendApplicationReply
            _
            ( RetainedRequestReply
                _
                (Completed request (LabelCompleted LabelApplied))
              ) <-
            effectBatchMembers effects
        ]
  assertEqual
    "connection loss does not submit the semantic Label again"
    [terminalRequest]
    labelRequests
  assertEqual
    "the Herald kernel emits one terminal Label reply"
    [terminalRequest]
    terminalReplies

  let views = oracleProjectionViews events
      opened =
        [ liveDecisionId decision
        | LabelDecidedView decision _ _ <- views
        ]
  decision <- case opened of
    [single] -> pure single
    decisions ->
      assertFailure
        ("expected one live Label decision, got " <> show decisions)
  assertEqual
    "the successful Label has one Applied decision"
    [decision]
    [ liveDecisionId observed
    | LabelDecidedView observed terminal _ <- views,
      LiveReleasedOutcomeView {} <- [liveTerminalOutcomeView terminal]
    ]
  assertEqual
    "the committed decision carries exactly Void generation one"
    [ReleasedLabelView (DomainValue.VoidLabel, 1)]
    [releasedLabelStateView (liveRecordStoredState overlay) | LabelDecidedView observed terminal _ <- views, liveDecisionId observed == decision, LiveReleasedOutcomeView _ _ _ overlay <- [liveTerminalOutcomeView terminal]]
  assertEqual
    "application loss commits one automatic process End"
    [step12H1ProcessEpochId]
    [process | ProcessEpochEndedEventView process _ ApplicationPermanentlyLost <- views, process == step12H1ProcessEpochId]
  assertEqual
    "the successful Label workflow completes once"
    [decision]
    [ observed
    | LabelWorkflowCompletedView observed _ <- views
    ]

applicationPromotions ::
  [RuntimeTraceEvent] ->
  [(RuntimeEventOrdinal, PhysicalConnectionRef, ApplicationSessionBinding)]
applicationPromotions events =
  [ (ordinal, physical, binding)
  | ShellEvent
      ordinal
      ( ShellConnectionPromoted
          physical@PhysicalApplicationRef {}
          _
          (ApplicationConnectionPromoted binding)
        ) <-
      events
  ]

applicationWriterFailures ::
  [RuntimeTraceEvent] ->
  [(RuntimeEventOrdinal, PhysicalConnectionRef)]
applicationWriterFailures events =
  [ (ordinal, physical)
  | ShellEvent
      ordinal
      (ShellConnectionWriterFailed physical@PhysicalApplicationRef {}) <-
      events
  ]

applicationCloseEvents ::
  [RuntimeTraceEvent] ->
  [(RuntimeEventOrdinal, PhysicalConnectionRef)]
applicationCloseEvents events =
  [ (ordinal, physical)
  | ShellEvent
      ordinal
      (ShellConnectionClosed physical@PhysicalApplicationRef {} _) <-
      events
  ]

applicationLaneOffers ::
  [RuntimeTraceEvent] ->
  [(RuntimeEventOrdinal, PhysicalConnectionRef)]
applicationLaneOffers events =
  [ (ordinal, physical)
  | ShellEvent
      ordinal
      (ShellLaneOffered physical@PhysicalApplicationRef {} ApplicationLane _) <-
      events
  ]

terminalRouteEvents ::
  RequestId ->
  [RuntimeTraceEvent] ->
  [RuntimeEventOrdinal]
terminalRouteEvents expected events =
  [ ordinal
  | ShellEvent
      ordinal
      ( ShellEffectRouted
          _
          _
          ( SendApplicationReply
              _
              ( RetainedRequestReply
                  _
                  (Completed request (LabelCompleted LabelApplied))
                )
            )
        ) <-
      events,
    request == expected
  ]

applicationTerminations :: [RuntimeTraceEvent] -> [(RuntimeEventOrdinal, ApplicationSessionId)]
applicationTerminations events =
  [ (ordinal, session)
  | KernelEvent ordinal (KernelTraceStepped _ _ input (Right effects)) <- events,
    RuntimeObserved (ApplicationBindingLost binding) <- [inputBody input],
    DisposeApplicationSession session ApplicationSessionNoLongerLive <- effectBatchMembers effects,
    session == sessionBindingSessionId binding
  ]

labelRequestInput :: HeraldInputBody -> [(RequestId, ApplicationOperation)]
labelRequestInput = \case
  ApplicationRequestInput (CallApplicationRequest _ _ request operation) -> [(request, operation)]
  ApplicationReceiptRetirementInput (RetireApplicationReceipts _ _ _ (Just (RetiringApplicationRequest request operation))) -> [(request, operation)]
  _ -> []

oracleProjectionViews :: [RuntimeTraceEvent] -> [OracleProjectionEventView]
oracleProjectionViews events =
  [ oracleProjectionEventView projection
  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
    OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
    canonical <- NonEmpty.toList entries,
    projection <-
      appliedEntryProjectionEvents
        (canonicalAppliedOracleEntryValue canonical)
  ]

withStep14Applications ::
  Step14Deployment ->
  ( EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    IO result
  ) ->
  IO result
withStep14Applications deployment use = do
  pResult <-
    EAPP.withApplication
      ( EAPP.applicationConfiguration
          (step14PApplicationEndpoint deployment)
          step14PApplicationAttachment
          (applicationClientNonce 14_401)
          step14ApplicationLivenessConfiguration
      )
      ( \pApplication pStartup -> do
          qResult <-
            EAPP.withApplication
              ( EAPP.applicationConfiguration
                  (step14QApplicationEndpoint deployment)
                  step14QApplicationAttachment
                  (applicationClientNonce 14_402)
                  step14ApplicationLivenessConfiguration
              )
              (use pApplication pStartup)
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
