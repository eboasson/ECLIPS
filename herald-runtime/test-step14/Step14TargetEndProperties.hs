{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An End ordered before an atomic label decision yields immutable NotApplied.
-- The home submission is held at its transport boundary; ordinary application
-- work and canonical End still progress, including while H4 is peer-isolated.
module Step14TargetEndProperties (tests) where

import Control.Concurrent
  ( forkFinally,
    threadDelay,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    readMVar,
    tryPutMVar,
  )
import Control.Exception
  ( SomeException,
    finally,
  )
import Control.Monad (unless, void, when)
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    startupAccessProcess,
  )
import Eclips.Application.Types.Identity
  ( PrivateUniqueId,
    asPrivateObjectId,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToProcess),
    LabelResult (LabelNotApplied),
  )
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Application.Types.Operation
  ( ApplicationOperation (LabelApplication, NewIdApplication),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (LabelCompleted, NewIdCompleted),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (ProcessLabel),
  )
import Eclips.Domain.Identity
  ( processEpochIdBytes,
    systemIdBytes,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ExplicitAdministrativeEnd),
  )
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RequestId,
    RetainedRequestReplyBody (Cancelled, Completed, Rejected),
    retainedReplyBodyRequestId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (SendApplicationReply),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
    HeraldInputBody (ApplicationRequestInput, OracleInput),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (SubmitOracleRequest),
    OracleClientIngress (OracleEntriesReceived),
    oracleRequestDispatchEnvelope,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent),
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( closePeerTransportGate,
    openPeerTransportGate,
    peerTransportGateIsOpen,
    setHeraldTcpOracleActionProbe,
  )
import Eclips.Oracle.Canonical
  ( canonicalAppliedOracleEntryValue,
    canonicalOracleEnvelopeValue,
  )
import Eclips.Oracle.Command (oracleEnvelopeCommand)
import Eclips.Oracle.Label
  ( LiveNotAppliedReason (LiveTargetProcessEnded),
    LiveTerminalOutcomeView (LiveNotAppliedOutcomeView, LiveReleasedOutcomeView),
    liveDecisionId,
    liveTerminalOutcomeView,
    oracleCommandTag,
  )
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (..),
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
    readStep14SequencingCarrier,
  )
import Step14Fixtures
  ( Step14Deployment,
    awaitStep14H4PeerIsolation,
    awaitStep14LatchWithin,
    awaitStep14OraclePrefix,
    awaitStep14PeerMesh,
    newStep14Latch,
    semanticStep14InputBody,
    signalStep14Latch,
    snapshotStep14HeraldTrace,
    step14AdministrationEndpoint,
    step14ApplicationLivenessConfiguration,
    step14H1,
    step14H2,
    step14H4PeerTransportGate,
    step14HeraldTcp,
    step14PApplicationAttachment,
    step14PApplicationEndpoint,
    step14QApplicationAttachment,
    step14QApplicationEndpoint,
    withStep14Deployment,
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
    "Step-14 target-End label settlement"
    [ testCase
        "End before decision yields NotApplied without installation collection"
        caseTargetEndsBeforeDecision
    ]

caseTargetEndsBeforeDecision :: Assertion
caseTargetEndsBeforeDecision = do
  readyClaimed <- newEmptyMVar
  readyHeld <- newStep14Latch
  releaseH4Ready <- newStep14Latch
  let probe action =
        when (isFenceReadySubmission action) $ do
          first <- tryPutMVar readyClaimed ()
          when first $ do
            signalStep14Latch readyHeld
            readMVar releaseH4Ready
      scenario deployment = do
        let h4Gate = step14H4PeerTransportGate deployment
            h4Tcp = step14HeraldTcp (step14H1 deployment)
            releaseProbe = do
              signalStep14Latch releaseH4Ready
              setHeraldTcpOracleActionProbe h4Tcp Nothing
              openPeerTransportGate h4Gate
        setHeraldTcpOracleActionProbe h4Tcp (Just probe)
        runTargetEndSchedule readyHeld releaseH4Ready deployment
          `finally` releaseProbe
  completed <-
    timeout 90_000_000
      $ withStep14Deployment scenario
  unless (completed == Just ())
    $ assertFailure "the target-End label schedule exceeded its failure bound"

runTargetEndSchedule :: MVar () -> MVar () -> Step14Deployment -> IO ()
runTargetEndSchedule readyHeld releaseH4Ready deployment = do
  let h4Gate = step14H4PeerTransportGate deployment
      releaseGate =
        openPeerTransportGate h4Gate
          `finally` signalStep14Latch releaseH4Ready
  initiallyOpen <- peerTransportGateIsOpen h4Gate
  assertBool "H4 starts connected to the peer plane" initiallyOpen
  awaitStep14PeerMesh deployment
  withPApplicationAndTopology releaseGate deployment $ \pApplication pStartup topology -> do
    let pProcess = startupAccessProcess pStartup
    originalCarrier <- readStep14SequencingCarrier pApplication topology
    expectStep14ApplicationLabel
      "the sequencing target initially belongs to P"
      ((ProcessLabel pProcess, 0))
      originalCarrier

    labelCall <-
      EAPP.label
        pApplication
        (asPrivateObjectId topology.pSequencingObject)
        ((ProcessLabel pProcess, 0))
        (LabelToProcess topology.localizedQProcess)
    labelCompletion <- observeApplicationCall labelCall
    readyWasHeld <- awaitStep14LatchWithin 15_000_000 readyHeld
    assertBool "the home retained its settled decision submission" readyWasHeld
    labelRequest <-
      awaitExactApplicationInput
        "the target-End label request"
        deployment
        ( LabelApplication
            (asPrivateObjectId topology.pSequencingObject)
            ((ProcessLabel pProcess, 0))
            (LabelToProcess topology.localizedQProcess)
        )
    closePeerTransportGate h4Gate
    awaitStep14H4PeerIsolation deployment
    isolated <- peerTransportGateIsOpen h4Gate
    assertBool
      "H4 is peer-isolated after the home settles the selected prefix"
      (not isolated)
    readyCheckpoint <- snapshotStep14HeraldTrace (step14H1 deployment)
    assertNoRetainedTerminalEffect
      "the paused decision has no terminal result"
      labelRequest
      readyCheckpoint

    laterCall <- EAPP.newid pApplication BareNewId
    laterCompletion <- observeApplicationCall laterCall
    laterRequest <- awaitBareNewIdInput deployment
    laterResult <- awaitObservedCall "independent NewId while label submission is paused" laterCompletion
    generated <- case laterResult of
      EAPP.ApplicationCallSucceeded (NewIdCompleted identifier) -> pure identifier
      other -> assertFailure ("independent NewId returned an unexpected result: " <> show other)

    endTargetProcess deployment
    awaitStep14OraclePrefix 1 deployment
    endedCheckpoint <- snapshotStep14HeraldTrace (step14H1 deployment)
    assertNoRetainedTerminalEffect
      "target End preserves the original paused label call"
      labelRequest
      endedCheckpoint
    signalStep14Latch releaseH4Ready
    awaitStep14OraclePrefix 2 deployment
    completedEvents <- awaitCompletedTargetEndTrace deployment
    assertTargetEndOracleEvidence completedEvents

    labelResult <- awaitObservedCall "target-End label result" labelCompletion
    assertEqual
      "the atomic decision reports that the requested target ended"
      (EAPP.ApplicationCallSucceeded (LabelCompleted LabelNotApplied))
      labelResult
    isolatedAfterFailure <- peerTransportGateIsOpen h4Gate
    assertBool "NotApplied completes without collecting an installation report from H4" (not isolatedAfterFailure)
    openPeerTransportGate h4Gate
    retainedCarrier <- readStep14SequencingCarrier pApplication topology
    expectStep14ApplicationLabel
      "NotApplied retains the sequencing target's original P label"
      ((ProcessLabel pProcess, 0))
      retainedCarrier

    events <- snapshotStep14HeraldTrace (step14H1 deployment)
    assertBareNewIdCompletedOnce laterRequest generated events

isFenceReadySubmission :: OracleClientAction -> Bool
isFenceReadySubmission = \case
  SubmitOracleRequest _ dispatch ->
    oracleCommandTag
      ( oracleEnvelopeCommand
          ( canonicalOracleEnvelopeValue
              (oracleRequestDispatchEnvelope dispatch)
          )
      )
      == 2
  _ -> False

withPApplicationAndTopology ::
  IO () ->
  Step14Deployment ->
  ( EAPP.Application ->
    ApplicationStartupAccess ->
    Step14ApplicationTopology ->
    IO result
  ) ->
  IO result
withPApplicationAndTopology releaseGate deployment use = do
  pResult <-
    EAPP.withApplication
      ( EAPP.applicationConfiguration
          (step14PApplicationEndpoint deployment)
          step14PApplicationAttachment
          (applicationClientNonce 14_301)
          step14ApplicationLivenessConfiguration
      )
      ( \pApplication pStartup -> do
          qResult <-
            EAPP.withApplication
              ( EAPP.applicationConfiguration
                  (step14QApplicationEndpoint deployment)
                  step14QApplicationAttachment
                  (applicationClientNonce 14_302)
                  step14ApplicationLivenessConfiguration
              )
              ( \qApplication qStartup -> do
                  topology <-
                    establishStep14ApplicationTopology
                      pApplication
                      pStartup
                      qApplication
                      qStartup
                  -- Q stays alive until the scenario's explicit End(Q).
                  -- Leaving this scope would end its TCP-owned lifetime.
                  use pApplication pStartup topology
                    `finally` releaseGate
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

assertNoRetainedTerminalEffect ::
  String ->
  RequestId ->
  [RuntimeTraceEvent] ->
  IO ()
assertNoRetainedTerminalEffect context request events =
  case [ body
       | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
         SendApplicationReply _ (RetainedRequestReply _ body) <-
           effectBatchMembers effects,
         retainedReplyBodyRequestId body == request,
         isTerminal body
       ] of
    [] -> pure ()
    observed ->
      assertFailure (context <> ": observed terminal effects " <> show observed)
  where
    isTerminal = \case
      Completed _ _ -> True
      Rejected _ _ -> True
      Cancelled _ _ -> True
      _ -> False

awaitObservedCall ::
  String ->
  MVar (Either SomeException EAPP.ApplicationCallCompletion) ->
  IO EAPP.ApplicationCallCompletion
awaitObservedCall context observed = do
  settled <- timeout 30_000_000 (readMVar observed)
  case settled of
    Nothing -> assertFailure (context <> " did not settle")
    Just (Left failure) ->
      assertFailure (context <> " raised " <> show failure)
    Just (Right completion) -> pure completion

awaitExactApplicationInput ::
  String ->
  Step14Deployment ->
  ApplicationOperation ->
  IO RequestId
awaitExactApplicationInput context deployment expected = do
  observed <- timeout 10_000_000 loop
  case observed of
    Nothing -> assertFailure (context <> " did not enter H1")
    Just request -> pure request
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      case [ request
           | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
             ApplicationRequestInput
               (CallApplicationRequest _ _ request operation) <-
               [semanticStep14InputBody input],
             operation == expected
           ] of
        [] -> threadDelay 10_000 >> loop
        [request] -> pure request
        requests ->
          assertFailure
            (context <> " entered H1 more than once: " <> show requests)

awaitBareNewIdInput :: Step14Deployment -> IO RequestId
awaitBareNewIdInput deployment = do
  observed <- timeout 10_000_000 loop
  case observed of
    Nothing -> assertFailure "H1 did not retain the later BareNewId input"
    Just request -> pure request
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      case bareNewIdRequests events of
        [] -> threadDelay 10_000 >> loop
        [request] -> pure request
        requests ->
          assertFailure
            ("H1 retained duplicate BareNewId inputs: " <> show requests)

bareNewIdRequests :: [RuntimeTraceEvent] -> [RequestId]
bareNewIdRequests events =
  [ request
  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
    ApplicationRequestInput
      ( CallApplicationRequest
          _
          _
          request
          (NewIdApplication BareNewId)
        ) <-
      [semanticStep14InputBody input]
  ]

assertBareNewIdCompletedOnce ::
  RequestId ->
  PrivateUniqueId ->
  [RuntimeTraceEvent] ->
  Assertion
assertBareNewIdCompletedOnce request generated events = do
  assertEqual
    "the later BareNewId entered the Herald exactly once"
    [request]
    (bareNewIdRequests events)
  assertEqual
    "the queued BareNewId emitted exactly one terminal completion"
    [Completed request (NewIdCompleted generated)]
    [ body
    | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
      SendApplicationReply _ (RetainedRequestReply _ body) <-
        effectBatchMembers effects,
      retainedReplyBodyRequestId body == request,
      case body of
        Completed _ _ -> True
        _ -> False
    ]

endTargetProcess :: Step14Deployment -> IO ()
endTargetProcess deployment =
  withStep14AdministrationClient
    (step14AdministrationEndpoint (step14H2 deployment))
    deploymentClaim
    $ \client -> do
      sendStep14AdministrationRequest client targetEndRequest
      expectExactAdminStatus
        "target End is retained"
        AdminAccepted
        client
      terminal <- awaitTargetEndTerminal client
      assertEqual
        "target End reaches its terminal EADM result before H4 reconnects"
        (AdminCompleted (ProcessEpochEndedDto targetProcessClaim))
        terminal

awaitTargetEndTerminal ::
  Step14AdministrationClient ->
  IO AdminResultStatusDto
awaitTargetEndTerminal client = do
  observed <-
    receiveStep14AdministrationForWithin
      adminReplyTimeoutMicroseconds
      targetEndCorrelation
      client
  case observed of
    Step14AdministrationReceived
      (AdminResult correlation status)
        | correlation == targetEndCorrelation -> case status of
            AdminAccepted -> awaitTargetEndTerminal client
            _ -> pure status
    Step14AdministrationReceived unexpected ->
      assertFailure ("target End returned an unexpected DTO: " <> show unexpected)
    Step14AdministrationReceiveFailed failure ->
      assertFailure ("target End receive failed: " <> show failure)
    Step14AdministrationReceiveTimedOut ->
      assertFailure "target End did not reach a terminal result"

expectExactAdminStatus ::
  String ->
  AdminResultStatusDto ->
  Step14AdministrationClient ->
  Assertion
expectExactAdminStatus context expected client = do
  observed <-
    receiveStep14AdministrationForWithin
      adminReplyTimeoutMicroseconds
      targetEndCorrelation
      client
  case observed of
    Step14AdministrationReceived (AdminResult correlation actual)
      | correlation == targetEndCorrelation ->
          assertEqual context expected actual
    Step14AdministrationReceived unexpected ->
      assertFailure (context <> ": unexpected DTO " <> show unexpected)
    Step14AdministrationReceiveFailed failure ->
      assertFailure (context <> ": receive failed: " <> show failure)
    Step14AdministrationReceiveTimedOut ->
      assertFailure (context <> ": timed out")

awaitCompletedTargetEndTrace ::
  Step14Deployment ->
  IO [RuntimeTraceEvent]
awaitCompletedTargetEndTrace deployment = do
  observed <- timeout 10_000_000 loop
  case observed of
    Nothing -> do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      assertFailure
        ( "H1 did not consume the completed target-End Oracle workflow; "
            <> "kernel faults: "
            <> show (kernelFaults events)
            <> "; projection events: "
            <> show (oracleProjectionViews events)
        )
    Just events -> pure events
  where
    loop = do
      events <- snapshotStep14HeraldTrace (step14H1 deployment)
      let targetFailures =
            [ ()
            | LabelDecidedView _ terminal _ <- oracleProjectionViews events,
              LiveNotAppliedOutcomeView _ _ _ (LiveTargetProcessEnded process) _ _ <- [liveTerminalOutcomeView terminal],
              process == step12H2ProcessEpochId
            ]
      if null targetFailures then threadDelay 10_000 >> loop else pure events

kernelFaults :: [RuntimeTraceEvent] -> [String]
kernelFaults events =
  [ show fault
  | KernelEvent _ (KernelTraceStepped _ _ _ (Left fault)) <- events
  ]

assertTargetEndOracleEvidence :: [RuntimeTraceEvent] -> Assertion
assertTargetEndOracleEvidence events = do
  let views = oracleProjectionViews events
      targetOutcomes =
        [ (liveDecisionId decided, decisionIndex)
        | LabelDecidedView decided terminal _ <- views,
          LiveNotAppliedOutcomeView _ decisionIndex _ (LiveTargetProcessEnded process) _ _ <- [liveTerminalOutcomeView terminal],
          process == step12H2ProcessEpochId
        ]
  (decision, resolveIndex) <- case targetOutcomes of
    [outcome] -> pure outcome
    other ->
      assertFailure
        ("expected one TargetProcessEnded projection event, got " <> show other)
  let endIndices =
        [ endIndex
        | ProcessEpochEndedEventView
            process
            endIndex
            ExplicitAdministrativeEnd <-
            views,
          process == step12H2ProcessEpochId
        ]
  assertBool
    "the exact target End precedes the NotApplied decision"
    (any (< resolveIndex) endIndices)
  assertEqual
    "NotApplied needs no canonical completion command"
    []
    [observed | LabelWorkflowCompletedView observed _ <- views, observed == decision]
  assertEqual
    "TargetProcessEnded emits no Applied decision"
    []
    [ liveDecisionId observed
    | LabelDecidedView observed terminal _ <- views,
      liveDecisionId observed == decision,
      LiveReleasedOutcomeView {} <- [liveTerminalOutcomeView terminal]
    ]

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

deploymentClaim :: AdminDeploymentIdClaim
deploymentClaim =
  checked
    "Step-14 target-End deployment claim"
    (adminDeploymentIdClaim (systemIdBytes step12SystemId))

targetProcessClaim :: AdminProcessEpochIdClaim
targetProcessClaim =
  checked
    "Step-14 target process-epoch claim"
    (adminProcessEpochIdClaim (processEpochIdBytes step12H2ProcessEpochId))

targetEndCorrelation :: AdminCorrelationIdClaim
targetEndCorrelation = adminCorrelationIdClaim 14_305

targetEndRequest :: AdminClientDto
targetEndRequest =
  EndProcessEpoch
    targetEndCorrelation
    targetProcessClaim
    AdminExplicitAdministrativeEnd

adminReplyTimeoutMicroseconds :: Int
adminReplyTimeoutMicroseconds = 20_000_000

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
