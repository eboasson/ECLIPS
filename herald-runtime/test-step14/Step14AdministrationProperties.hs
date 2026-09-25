{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Step14AdministrationProperties
  ( tests,
  )
where

import Control.Concurrent
  ( MVar,
    newEmptyMVar,
    readMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Exception (finally, throwIO)
import Control.Monad (void, when)
import Eclips.Domain.Identity
  ( systemIdBytes,
  )
import Eclips.Herald.Administration qualified as Administration
import Eclips.Herald.Administration.RPC (AdministrationOutbound (SendEstablishedAdministration), projectAdministrationEffect)
import Eclips.Herald.EffectBatch
  ( HeraldEffect (SendAdministrationReply),
    effectBatchMembers,
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (batchEnvelopeEffects),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( ConnectionPromotion (ConfiguredAdministrationConnectionPromoted),
    PhysicalConnectionRef (PhysicalConfiguredAdministrationRef),
    RuntimeEventOrdinal,
    RuntimeTraceEvent (ShellEvent),
    ShellTraceEvent
      ( ShellConnectionClosed,
        ShellConnectionPromoted,
        ShellConnectionWriterFailed,
        ShellEffectRouted
      ),
  )
import Eclips.Protocol.Admin.Types
  ( AdminClientDto (..),
    AdminCompletedResultDto (..),
    AdminCorrelationIdClaim,
    AdminDeploymentIdClaim,
    AdminProcessEndReason (AdminExplicitAdministrativeEnd),
    AdminProcessEpochIdClaim,
    AdminResultStatusDto (..),
    AdminServerDto (..),
    adminCorrelationIdClaim,
    adminCorrelationIdClaimWord64,
    adminDeploymentIdClaim,
  )
import Eclips.Public.Types.ReceiptRetirement (receiptRetirementPrefix)
import Step12Fixtures
  ( step12SystemId,
  )
import Step14AdministrationClient
  ( Step14AdministrationClient,
    Step14AdministrationReceive (..),
    Step14AdministrationReceiveFailure (Step14AdministrationLaneClosed),
    closeStep14AdministrationClient,
    connectStep14AdministrationClient,
    receiveStep14AdministrationForWithin,
    receiveStep14AdministrationWithin,
    sendStep14AdministrationRequest,
  )
import Step14Fixtures
  ( Step14Deployment,
    Step14HeraldName (..),
    awaitStep14PeerMesh,
    snapshotStep14HeraldTrace,
    step14AdministrationEndpoint,
    step14H2,
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
    "Step-14 process administration over real TCP"
    [ testCase
        "Dynamic Start survives lost delivery; replacement operators have fresh receipt lifetimes"
        caseStartEndBeforeAttachmentReconnect
    ]

caseStartEndBeforeAttachmentReconnect :: Assertion
caseStartEndBeforeAttachmentReconnect = do
  acceptedObserved <- newEmptyMVar
  terminalArmed <- newEmptyMVar
  terminalAttemptFailed <- newEmptyMVar
  let hooksFor = \case
        Step14H2 -> h2LostTerminalReply acceptedObserved terminalArmed terminalAttemptFailed
        _ -> defaultRuntimeHooks
  observed <-
    timeout
      scenarioTimeoutMicroseconds
      (withStep14DeploymentWithHooks hooksFor (runScenario acceptedObserved terminalAttemptFailed))
  case observed of
    Nothing -> assertFailure "Step-14 dynamic Start/End administration scenario timed out"
    Just () -> pure ()

h2LostTerminalReply ::
  MVar () ->
  MVar Administration.AdminCorrelationId ->
  MVar (Administration.AdminCorrelationId, PhysicalConnectionRef) ->
  RuntimeHooks
h2LostTerminalReply acceptedObserved armed failed =
  defaultRuntimeHooks
    { hookBeforeDispatchBatch = \batch ->
        case expectedTerminalCorrelations batch of
          [] -> pure ()
          correlation : _ -> do
            readMVar acceptedObserved
            void (tryPutMVar armed correlation),
      hookAfterWriterCommandDequeued = \case
        physical@PhysicalConfiguredAdministrationRef {} -> do
          expected <- tryReadMVar armed
          case expected of
            Nothing -> pure ()
            Just correlation -> do
              first <- tryPutMVar failed (correlation, physical)
              when first
                $ throwIO
                  (userError "intentional loss of a process-administration terminal reply")
        _ -> pure ()
    }

expectedTerminalCorrelations :: BatchEnvelope -> [Administration.AdminCorrelationId]
expectedTerminalCorrelations batch =
  [ correlation
  | SendAdministrationReply
      _
      (Administration.RetainedAdminResult correlation status) <-
      effectBatchMembers (batchEnvelopeEffects batch),
    isExpectedTerminal correlation status
  ]

isExpectedTerminal :: Administration.AdminCorrelationId -> Administration.AdminResultStatus -> Bool
isExpectedTerminal correlation status = case status of
  Administration.AdminStartRequestCompleted {} -> Administration.adminCorrelationIdWord64 correlation == 14_101
  Administration.AdminEndRequestCompleted {} -> Administration.adminCorrelationIdWord64 correlation == 14_102
  _ -> False

runScenario :: MVar () -> MVar (Administration.AdminCorrelationId, PhysicalConnectionRef) -> Step14Deployment -> IO ()
runScenario acceptedObserved terminalAttemptFailed deployment = do
  awaitStep14PeerMesh deployment
  let endpoint = step14AdministrationEndpoint (step14H2 deployment)
  first <- connectStep14AdministrationClient endpoint deploymentClaim
  (lostCorrelation, failedPhysical) <-
    ( do
        sendStep14AdministrationRequest first startRequest
        expectExactResult "Start is retained before its Oracle workflow" startCorrelation AdminAccepted first
        void (tryPutMVar acceptedObserved ())
        failed <- readMVar terminalAttemptFailed
        expectOldLaneTerminalLoss first
        pure failed
    )
      `finally` closeStep14AdministrationClient first

  -- The test obtains the generated process from the already-produced semantic
  -- effect, not through recovery of the closed administrator's receipt.
  trace <- snapshotStep14HeraldTrace (step14H2 deployment)
  process <-
    requireOnly
      "the lost Start still produced one generated live process"
      [ generated
      | ShellEvent _ (ShellEffectRouted _ _ effect@(SendAdministrationReply _ (Administration.RetainedAdminResult correlation _))) <- trace,
        correlation == lostCorrelation,
        Right (Just (SendEstablishedAdministration _ (AdminResult _ (AdminCompleted (StartProcessReadyDto generated _))))) <- [projectAdministrationEffect effect]
      ]
  second <- connectStep14AdministrationClient endpoint deploymentClaim
  ( do
      sendStep14AdministrationRequest second (GetAdminResult startCorrelation)
      expectExactServer "old Start receipt is absent in the fresh operator lifetime" startCorrelation (AdminAbsent startCorrelation) second
      assertLostTerminalLifetimeTrace deployment lostCorrelation failedPhysical
      sendStep14AdministrationRequest second (endRequest process)
      expectExactResult "End owns a new independent receipt" endCorrelation AdminAccepted second
      ended <- awaitTerminal endCorrelation second
      assertEqual "the old generated child survives delivery loss and can be ended" (expectedEndTerminal process) ended
      sendStep14AdministrationRequest second (endRequest process)
      expectExactResult "same live operator retries the exact terminal End" endCorrelation ended second
      sendStep14AdministrationRequest second (StartProcessEpoch endCorrelation)
      expectExactServer "typed reuse within one lifetime conflicts" endCorrelation (AdminConflict endCorrelation) second
      let released = receiptRetirementPrefix (Just (adminCorrelationIdClaimWord64 endCorrelation))
      sendStep14AdministrationRequest second (AdminWithRetirement released (GetAdminResult endCorrelation))
      expectExactServer "consumed End receipt has a typed retired result in its live binding" endCorrelation (AdminResultRetired endCorrelation released) second
      sendStep14AdministrationRequest second (RetireAdminReceipts released)
      acknowledgment <- receiveStep14AdministrationWithin replyTimeoutMicroseconds second
      assertEqual "idle retirement repeats the same released set" (Step14AdministrationReceived (AdminReceiptsRetired released)) acknowledgment
    )
    `finally` closeStep14AdministrationClient second
  third <- connectStep14AdministrationClient endpoint deploymentClaim
  ( do
      sendStep14AdministrationRequest third (GetAdminResult startCorrelation)
      expectExactServer "third operator cannot recover first operator's Start" startCorrelation (AdminAbsent startCorrelation) third
      sendStep14AdministrationRequest third (GetAdminResult endCorrelation)
      expectExactServer "third operator cannot recover second operator's End" endCorrelation (AdminAbsent endCorrelation) third
    )
    `finally` closeStep14AdministrationClient third

expectOldLaneTerminalLoss :: Step14AdministrationClient -> Assertion
expectOldLaneTerminalLoss client = do
  observed <-
    receiveStep14AdministrationWithin replyTimeoutMicroseconds client
  case observed of
    Step14AdministrationReceiveFailed Step14AdministrationLaneClosed -> pure ()
    Step14AdministrationReceived escaped ->
      assertFailure
        ("the terminal reply escaped on the intentionally failed old lane: " <> show escaped)
    Step14AdministrationReceiveFailed failure ->
      assertFailure
        ("the intentionally failed old lane ended unexpectedly: " <> show failure)
    Step14AdministrationReceiveTimedOut ->
      assertFailure "the intentionally failed old administration lane did not close"

assertLostTerminalLifetimeTrace ::
  Step14Deployment ->
  Administration.AdminCorrelationId ->
  PhysicalConnectionRef ->
  Assertion
assertLostTerminalLifetimeTrace deployment correlation failedPhysical = do
  events <- snapshotStep14HeraldTrace (step14H2 deployment)
  (initialPromotion, initialPhysical, initialBinding, replacementPromotion, replacementPhysical, replacementBinding) <-
    case administrationPromotions events of
      [ (firstOrdinal, firstPhysical, firstBinding),
        (secondOrdinal, secondPhysical, secondBinding)
        ] ->
          pure
            ( firstOrdinal,
              firstPhysical,
              firstBinding,
              secondOrdinal,
              secondPhysical,
              secondBinding
            )
      promotions ->
        assertFailure
          ("expected the old and replacement administration promotions, got " <> show (length promotions))
  assertEqual
    "the injected terminal writer failure belongs to the old administration lane"
    initialPhysical
    failedPhysical
  assertBool
    "the fresh delivery lifetime uses a distinct physical administration lane"
    (initialPhysical /= replacementPhysical)

  lostRoute <-
    requireOnly
      "the exact terminal reply routed once to the old binding"
      (terminalRouteOrdinals initialBinding correlation events)
  failure <-
    requireOnly
      "one exact old-lane administration writer failure"
      [ ordinal
      | ShellEvent ordinal (ShellConnectionWriterFailed physical) <- events,
        physical == initialPhysical
      ]
  close <-
    requireOnly
      "one exact old-lane administration close"
      [ ordinal
      | ShellEvent ordinal (ShellConnectionClosed physical _) <- events,
        physical == initialPhysical
      ]
  assertEqual
    "the closed lifetime terminal is never rerouted to a replacement"
    []
    (terminalRouteOrdinals replacementBinding correlation events)
  assertBool
    "terminal production, old-lane loss, close, and replacement remain ordered"
    (initialPromotion < lostRoute && lostRoute < failure && failure < close && close < replacementPromotion)

administrationPromotions ::
  [RuntimeTraceEvent] ->
  [(RuntimeEventOrdinal, PhysicalConnectionRef, Administration.AdministrationBinding)]
administrationPromotions events =
  [ (ordinal, physical, binding)
  | ShellEvent
      ordinal
      ( ShellConnectionPromoted
          physical@PhysicalConfiguredAdministrationRef {}
          _
          (ConfiguredAdministrationConnectionPromoted binding)
        ) <-
      events
  ]

terminalRouteOrdinals ::
  Administration.AdministrationBinding ->
  Administration.AdminCorrelationId ->
  [RuntimeTraceEvent] ->
  [RuntimeEventOrdinal]
terminalRouteOrdinals expectedBinding expectedCorrelation events =
  [ ordinal
  | ShellEvent
      ordinal
      ( ShellEffectRouted
          _
          _
          ( SendAdministrationReply
              binding
              (Administration.RetainedAdminResult correlation status)
            )
        ) <-
      events,
    binding == expectedBinding,
    correlation == expectedCorrelation,
    isExpectedTerminal correlation status
  ]

requireOnly :: (Show value) => String -> [value] -> IO value
requireOnly context = \case
  [value] -> pure value
  observed -> assertFailure (context <> ": observed " <> show observed)

awaitTerminal :: AdminCorrelationIdClaim -> Step14AdministrationClient -> IO AdminResultStatusDto
awaitTerminal correlation client = do
  observed <- receiveStep14AdministrationForWithin replyTimeoutMicroseconds correlation client
  case observed of
    Step14AdministrationReceived (AdminResult actual AdminAccepted) | actual == correlation -> awaitTerminal correlation client
    Step14AdministrationReceived (AdminResult actual terminal) | actual == correlation -> pure terminal
    other -> assertFailure ("expected retained terminal result: " <> show other)

expectExactResult ::
  String ->
  AdminCorrelationIdClaim ->
  AdminResultStatusDto ->
  Step14AdministrationClient ->
  Assertion
expectExactResult context correlation status =
  expectExactServer context correlation (AdminResult correlation status)

expectExactServer ::
  String ->
  AdminCorrelationIdClaim ->
  AdminServerDto ->
  Step14AdministrationClient ->
  Assertion
expectExactServer context correlation expected client = do
  observed <-
    receiveStep14AdministrationForWithin
      replyTimeoutMicroseconds
      correlation
      client
  case observed of
    Step14AdministrationReceived actual ->
      assertEqual context expected actual
    Step14AdministrationReceiveFailed failure ->
      assertFailure (context <> ": receive failed: " <> show failure)
    Step14AdministrationReceiveTimedOut ->
      assertFailure (context <> ": timed out")

deploymentClaim :: AdminDeploymentIdClaim
deploymentClaim =
  checked
    "Step-14 administration deployment claim"
    (adminDeploymentIdClaim (systemIdBytes step12SystemId))

startCorrelation, endCorrelation :: AdminCorrelationIdClaim
startCorrelation = adminCorrelationIdClaim 14_101
endCorrelation = adminCorrelationIdClaim 14_102

startRequest :: AdminClientDto
startRequest = StartProcessEpoch startCorrelation
endRequest :: AdminProcessEpochIdClaim -> AdminClientDto
endRequest process = EndProcessEpoch endCorrelation process AdminExplicitAdministrativeEnd
expectedEndTerminal :: AdminProcessEpochIdClaim -> AdminResultStatusDto
expectedEndTerminal process = AdminCompleted (ProcessEpochEndedDto process)

scenarioTimeoutMicroseconds, replyTimeoutMicroseconds :: Int
scenarioTimeoutMicroseconds = 30_000_000
replyTimeoutMicroseconds = 10_000_000

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
