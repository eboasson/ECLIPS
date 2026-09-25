{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Step9ComposedProperties
  ( tests,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkFinally,
    killThread,
    yield,
  )
import Control.Concurrent.Chan
  ( Chan,
    newChan,
    readChan,
    writeChan,
  )
import Control.Concurrent.MVar
  ( MVar,
    modifyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryReadMVar,
    tryTakeMVar,
  )
import Control.Exception
  ( SomeException,
    bracket,
    bracket_,
    displayException,
    finally,
    try,
  )
import Control.Monad
  ( forM_,
    replicateM_,
    unless,
    void,
    when,
  )
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Client qualified as Client
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Operation
  ( ApplicationOperation (ReadApplication, WriteApplication),
  )
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Result (RegularCallResult (..))
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value
  ( ApplicationValue (SortDefinitionValue),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten),
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    SortId,
    bootstrapManifestIdBytes,
  )
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Application.RPC (ApplicationOutbound (..))
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerCandidate,
    PeerHelloDisposition (..),
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Input
  ( ApplicationLifecycleIngress (CallApplicationLifecycle),
    ApplicationReceiptRetirementIngress (RetireApplicationReceipts),
    ApplicationRequestIngress (CallApplicationRequest),
    ApplicationRetirementWork (..),
    HeraldInput,
    HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (PeerDispatchSelected),
    RuntimeObservation (DrainBarrierObserved, PeerDispatchObserved),
    inputBody,
  )
import Eclips.Herald.Peer.RPC
  ( CandidatePeerContext (..),
    CandidatePeerInbound (..),
    EstablishedPeerInbound (..),
    PeerRpcAdmissionError,
    admitCandidatePeerEnvelope,
    admitEstablishedPeerEnvelope,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome (..),
    PeerDispatchTicket,
    PeerLogicalAttempt,
    PeerLogicalItem,
    peerDispatchAttemptItem,
    peerDispatchTicketDestinationHeraldEpoch,
  )
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    awaitHeraldRuntimeExit,
    heraldRuntimeConfiguration,
    requestHeraldRuntimeDrain,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
  )
import Eclips.Herald.Runtime.Connection
  ( AdministrationPlane,
    ApplicationPlane,
    ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeApplicationConnectionHandlers,
    RuntimeLaneOffer (..),
    RuntimePeerConnectionHandlers,
    heraldRuntimeHandlers,
    runtimeAdministrationConnectionHandlers,
    runtimeApplicationConnectionHandlers,
    runtimePeerConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeAdministrationRegistration (..),
    RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    openPeerCandidate,
    registerAdministrationConnection,
    registerApplicationConnection,
    registerPeerConnection,
    submitApplicationDto,
    submitPeerControlWithProgress,
    submitPeerHello,
    submitPeerPublication,
    submitPeerPublicationWithProgress,
  )
import Eclips.Herald.Runtime.Internal.Arbiter
  ( SourceFamily (PeerSources),
    SourceId (SourceId),
  )
import Eclips.Herald.Runtime.Internal.Conformance
  ( RecordedHeraldRuntime,
    RecordingSchedule (..),
    advanceRecordedBatch,
    advanceRecordedBatchesUntilPeerOutcome,
    awaitRecordedBatch,
    awaitRecordedDelay,
    awaitRecordedDrainCut,
    awaitRecordedPeerBindingLoss,
    awaitRecordedPeerControlAfter,
    awaitRecordedPeerPublicationAfter,
    injectRecordedEffectBatch,
    pauseRecordedBatchPump,
    recordedHeraldRuntime,
    releaseRecordedBatch,
    releaseRecordedDelay,
    resumeRecordedBatchPump,
    snapshotRecordedHeraldRuntime,
    snapshotRecordedRuntimeEvents,
    startRecordedHeraldRuntime,
  )
import Eclips.Herald.Runtime.Internal.Coordination (BatchOrdinal)
import Eclips.Herald.Runtime.Internal.Recording
  ( RuntimeTrace,
    replayRuntimeTrace,
    runtimeTraceEvents,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (..),
    RuntimeEventOrdinal,
    RuntimeTraceEvent (..),
    ShellTraceEvent (..),
  )
import Eclips.Herald.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (..))
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Protocol.Application.Types qualified as Protocol
import Eclips.Protocol.Peer.Types (PeerEnvelope)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import PairRuntimeFixtures
  ( pairBidirectionalLocalBootstraps,
    pairBidirectionalRemoteBootstraps,
    pairLocalBootstrapId,
    pairLocalGeneratorSeedSource,
    pairLocalGenesis,
    pairRemoteBootstrapId,
    pairRemoteGeneratorSeedSource,
    pairRemoteGenesis,
  )
import PeerEvidence (data SemanticPeerControlReceived, data SemanticPeerPublicationReceived, data SemanticSendPeerControl, data SemanticSendPeerItem)
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
import System.Timeout (timeout)
import Test.Tasty
  ( TestTree,
    localOption,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    QuickCheckTests (..),
    counterexample,
    ioProperty,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "two-Herald public runtime gate"
    [ testCase
        "application loss is terminal while surviving sessions retain peer retransmission work"
        caseComposedRuntimePair,
      testCase
        "destination retirement purges delayed work and leaves another destination live"
        caseDestinationRetirement,
      localOption
        (QuickCheckTests 100)
        (testProperty "100 generated safe runtime schedules preserve the composed result" propGeneratedSchedules)
    ]

caseDestinationRetirement :: Assertion
caseDestinationRetirement = do
  attemptGate <- newAttemptGate
  localClientTrace <- newIORef []
  remoteClientTrace <- newIORef []
  withRecordedRuntimePair DeterministicallyGatedRecording $ \localRecorded _ remoteRecorded _ -> do
    let localRuntime = recordedHeraldRuntime localRecorded
        remoteRuntime = recordedHeraldRuntime remoteRecorded
    pair <- newRuntimePair localRecorded remoteRecorded attemptGate
    _ <- connectPeerGeneration pair
    localClient <- openPureClient localClientTrace localRuntime pairLocalBootstrapId 711
    remoteClient <- openPureClient remoteClientTrace remoteRuntime pairRemoteBootstrapId 712
    (_, Just retiredTicket) <-
      beginPostCutSuppressedPublication
        remoteClientTrace
        remoteRuntime
        remoteClient
        remoteRecorded
        0
    let retiredDestination = peerDispatchTicketDestinationHeraldEpoch retiredTicket
    injectRecordedEffectBatch
      remoteRecorded
      (orderedEffectBatch [CancelPeerDestination retiredDestination])
    awaitTicketSuppressions "destination cancellation" remoteRecorded retiredTicket 1
    releaseRecordedDelay remoteRecorded
    injectRecordedEffectBatch
      remoteRecorded
      (orderedEffectBatch [SchedulePeerDispatch retiredTicket])
    awaitTicketSuppressions "late destination schedule" remoteRecorded retiredTicket 2
    retiredEvents <- snapshotRecordedRuntimeEvents remoteRecorded
    assertBool
      "the cancelled delayed wake never releases or selects its ticket"
      ( not
          ( any
              ( \case
                  ShellEvent _ (ShellDelayedWorkReleased ticket) -> ticket == retiredTicket
                  KernelEvent _ (KernelTraceStepped _ _ input _) -> case inputBody input of
                    PeerInput (PeerDispatchSelected ticket _) -> ticket == retiredTicket
                    _ -> False
                  _ -> False
              )
              retiredEvents
          )
      )
    (_, Just survivorTicket) <-
      beginPostCutSuppressedPublication
        localClientTrace
        localRuntime
        localClient
        localRecorded
        pairDelayMicros
    assertBool
      "the independent destination differs from the retired destination"
      (peerDispatchTicketDestinationHeraldEpoch survivorTicket /= retiredDestination)
    releaseRecordedDelay localRecorded
    awaitTicketRelease "independent destination" localRecorded survivorTicket

caseComposedRuntimePair :: Assertion
caseComposedRuntimePair = do
  production <- runPairScenario False ProductionShapedRecording fixedSchedule
  recorded <- runPairScenario False DeterministicallyGatedRecording fixedSchedule
  drainCut <- runPairScenario True DeterministicallyGatedRecording fixedSchedule
  assertSummary ProductionShapedRecording (pairRunSummary production)
  assertSummary DeterministicallyGatedRecording (pairRunSummary recorded)
  assertSummary DeterministicallyGatedRecording (pairRunSummary drainCut)
  assertPairReplay production
  assertPairReplay recorded
  assertPairReplay drainCut
  assertLiteralDrainCut drainCut
  assertEqual
    "production-shaped and gated pair runs reach the same public semantic summary"
    (semanticPairSummary (pairRunSummary production))
    (semanticPairSummary (pairRunSummary recorded))
  assertEqual
    "the two pure client owners select the same inputs and complete effect batches"
    (pairClientTrace production)
    (pairClientTrace recorded)

propGeneratedSchedules :: Word8 -> Property
propGeneratedSchedules seed = ioProperty $ do
  let schedule = scheduleFromSeed seed
  observed <- try (runPairScenario True DeterministicallyGatedRecording schedule) :: IO (Either SomeException PairRun)
  pure $ case observed of
    Left problem ->
      counterexample
        ("schedule " <> show schedule <> " failed: " <> displayException problem)
        False
    Right run ->
      counterexample
        (generatedFailureEvidence schedule run)
        (generatedRunComplete run)

generatedRunComplete :: PairRun -> Bool
generatedRunComplete run =
  summaryIsComplete (pairRunSummary run)
    && length (summaryAttempts (pairRunSummary run)) == 3
    && pairReplaySucceeded run
    && pairTraceComplete run

generatedFailureEvidence :: PairSchedule -> PairRun -> String
generatedFailureEvidence schedule run =
  "schedule "
    <> show schedule
    <> " checks="
    <> show
      ( summaryIsComplete summary,
        pairReplaySucceeded run,
        delayTraceComplete (pairLocalTrace run) (summaryAttempts summary),
        firstAttemptTraceComplete run,
        replacementAttemptTraceComplete run,
        literalDrainEvidence run,
        postCutTicketTraceComplete (pairRemoteTrace run),
        queuedDrainCallWasNotSelected (pairRemoteTrace run)
      )
    <> " produced "
    <> show summary
  where
    summary = pairRunSummary run

literalDrainEvidence :: PairRun -> (Maybe [PeerDispatchOutcome], Maybe [PeerDispatchOutcome], Maybe [PeerDispatchOutcome], Maybe Bool, [PeerDispatchTicket])
literalDrainEvidence run = case pairLiteralDrainAttempt run of
  Nothing -> (Nothing, Nothing, Nothing, Nothing, [])
  Just attempt ->
    ( Just (wonOutcomes attempt events),
      Just (suppressedOutcomes attempt events),
      Just (kernelOutcomes attempt events),
      Just (orderedDrainEvents attempt events),
      drainCutDelayedSuppressions (runtimeTraceEvents (pairRemoteTrace run))
    )
  where
    events = runtimeTraceEvents (pairLocalTrace run)

data StaleReleasePosition
  = ReleaseAfterReplacementRegistration
  | ReleaseAfterReplacementHandshake
  | ReleaseAfterRetryDelay
  deriving stock (Eq, Show, Enum, Bounded)

data IndependentReleasePosition
  = ReleaseBeforePeerReconnect
  | ReleaseBeforeRemoteRead
  | ReleaseAfterRemoteRead
  | KeepBlockedUntilDrain
  deriving stock (Eq, Show, Enum, Bounded)

data PairSchedule = PairSchedule
  { scheduleStaleRelease :: StaleReleasePosition,
    scheduleIndependentRelease :: IndependentReleasePosition,
    scheduleCloseRemoteFirst :: Bool,
    scheduleYields :: Int
  }
  deriving stock (Eq, Show)

fixedSchedule :: PairSchedule
fixedSchedule =
  PairSchedule
    { scheduleStaleRelease = ReleaseAfterRetryDelay,
      scheduleIndependentRelease = KeepBlockedUntilDrain,
      scheduleCloseRemoteFirst = True,
      scheduleYields = 2
    }

scheduleFromSeed :: Word8 -> PairSchedule
scheduleFromSeed seed =
  PairSchedule
    { scheduleStaleRelease =
        toEnum (fromIntegral seed `mod` staleReleaseCount),
      scheduleIndependentRelease =
        toEnum (fromIntegral (seed `div` 3) `mod` independentReleaseCount),
      scheduleCloseRemoteFirst = odd (seed `div` 12),
      scheduleYields = fromIntegral (seed `div` 24) `mod` 3
    }

staleReleaseCount :: Int
staleReleaseCount = length ([minBound .. maxBound] :: [StaleReleasePosition])

independentReleaseCount :: Int
independentReleaseCount = length ([minBound .. maxBound] :: [IndependentReleasePosition])

data PairSummary = PairSummary
  { summaryLostWrite :: Client.ApplicationUnavailableReason,
    summaryCommittedSort :: SortId,
    summaryRemoteValues :: [ApplicationValue],
    summaryExpectedValue :: ApplicationValue,
    summaryAttempts :: [PeerLogicalAttempt],
    summaryDelayCalls :: [Word64],
    summaryStaleCallbackReturned :: Bool,
    summaryLiteralDrainCut :: Bool,
    summaryForwardingFailures :: [(String, RuntimeSubmission)],
    summaryLocalExit :: Either RuntimeInternal.HeraldRuntimeFailure HeraldRuntimeExit,
    summaryRemoteExit :: Either RuntimeInternal.HeraldRuntimeFailure HeraldRuntimeExit
  }
  deriving stock (Eq, Show)

semanticPairSummary :: PairSummary -> PairSummary
semanticPairSummary summary =
  summary
    { summaryAttempts = [],
      summaryDelayCalls = []
    }

data ClientTraceStep
  = ClientTraceStep
      Client.ApplicationClientInput
      [Client.ApplicationClientEffect]
  deriving stock (Eq, Show)

data PairClientTrace
  = PairClientTrace
      [ClientTraceStep]
      [ClientTraceStep]
  deriving stock (Eq, Show)

data PairRun = PairRun
  { pairRunSummary :: PairSummary,
    pairLiteralDrainAttempt :: Maybe PeerLogicalAttempt,
    pairLocalTrace :: RuntimeTrace,
    pairRemoteTrace :: RuntimeTrace,
    pairClientTrace :: PairClientTrace,
    pairReplayFailures :: [String]
  }
  deriving stock (Show)

pairReplaySucceeded :: PairRun -> Bool
pairReplaySucceeded = null . pairReplayFailures

assertPairReplay :: PairRun -> Assertion
assertPairReplay run = do
  assertEqual "both recorded kernel traces replay to their exact final state" [] (pairReplayFailures run)
  assertBool
    "the live delay trace correlates every eligible ticket with one release, suppression, and selection fate"
    (delayTraceComplete (pairLocalTrace run) (summaryAttempts (pairRunSummary run)))
  assertBool
    "the first attempt, close-winner, late old callback, and stale retry ticket are exactly correlated"
    (firstAttemptTraceComplete run)
  assertBool
    "the replacement attempt wins and reaches the kernel exactly once as Written"
    (replacementAttemptTraceComplete run)

assertLiteralDrainCut :: PairRun -> Assertion
assertLiteralDrainCut run = case pairLiteralDrainAttempt run of
  Nothing -> assertFailure "the literal drain scenario did not retain its handed peer attempt"
  Just drainAttempt -> do
    let localEvents = runtimeTraceEvents (pairLocalTrace run)
        suppressedWritten =
          filter (== PeerDispatchWritten) (suppressedOutcomes drainAttempt localEvents)
    assertEqual
      "drain wins the handed peer outcome exactly once as Deferred"
      [PeerDispatchDeferred]
      (wonOutcomes drainAttempt localEvents)
    assertEqual
      "the late physical Written result is suppressed exactly once"
      [PeerDispatchWritten]
      suppressedWritten
    assertEqual
      "the kernel observes only the drain-winning Deferred outcome"
      [PeerDispatchDeferred]
      (kernelOutcomes drainAttempt localEvents)
    assertBool
      "the handed outcome settles before the drain barrier and FinishDrain route"
      (orderedDrainEvents drainAttempt localEvents)
    assertBool
      ( "the exact opposite-direction publication ticket is suppressed once by the cut and is never released or selected; evidence="
          <> show (postCutTicketEvidence (pairRemoteTrace run))
      )
      (postCutTicketTraceComplete (pairRemoteTrace run))
    assertBool
      "the queued application Call behind the held disposition was never selected"
      (queuedDrainCallWasNotSelected (pairRemoteTrace run))

pairTraceComplete :: PairRun -> Bool
pairTraceComplete run =
  delayTraceComplete (pairLocalTrace run) (summaryAttempts summary)
    && firstAttemptTraceComplete run
    && replacementAttemptTraceComplete run
    && (not (summaryLiteralDrainCut summary) || literalDrainTraceComplete run)
    && (not (summaryLiteralDrainCut summary) || queuedDrainCallWasNotSelected (pairRemoteTrace run))
  where
    summary = pairRunSummary run

delayTraceComplete :: RuntimeTrace -> [PeerLogicalAttempt] -> Bool
delayTraceComplete trace attempts =
  not (null eligible)
    && allUnique eligible
    && allUnique released
    && allUnique suppressed
    && allUnique selected
    && all (`elem` eligible) (released <> selected)
    && all hasOneTerminalFate eligible
    && all (\ticket -> ticket `elem` released && ticket `notElem` suppressed) selected
    && all (`notElem` selected) suppressed
    && length selected >= length attempts
  where
    events = runtimeTraceEvents trace
    eligible = [ticket | ShellEvent _ (ShellDelayedWorkEligible ticket) <- events]
    released = [ticket | ShellEvent _ (ShellDelayedWorkReleased ticket) <- events]
    suppressed = [ticket | ShellEvent _ (ShellDelayedWorkSuppressed ticket) <- events]
    selected =
      [ ticket
      | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
        PeerInput (PeerDispatchSelected ticket _) <- [inputBody input]
      ]
    hasOneTerminalFate ticket =
      ticket `elem` suppressed
        || (ticket `elem` released && ticket `elem` selected)

firstAttemptTraceComplete :: PairRun -> Bool
firstAttemptTraceComplete run = case summaryAttempts (pairRunSummary run) of
  firstAttempt : _ ->
    wonOutcomes firstAttempt events == [PeerDispatchDeferred]
      && suppressedOutcomes firstAttempt events == [PeerDispatchDeferred]
      && kernelOutcomes firstAttempt events == [PeerDispatchDeferred]
      && case ( attemptSelectionTickets firstAttempt events,
                scheduledTicketsForOutcome firstAttempt PeerDispatchDeferred events
              ) of
        ([firstTicket], [staleRetryTicket]) ->
          firstTicket /= staleRetryTicket
            && ticketTraceEvidence firstTicket events == (1, 1, 0, 1, [firstAttempt])
            && ticketTraceEvidence staleRetryTicket events == (1, 1, 0, 1, [])
        _ -> False
  [] -> False
  where
    events = runtimeTraceEvents (pairLocalTrace run)

replacementAttemptTraceComplete :: PairRun -> Bool
replacementAttemptTraceComplete run = case summaryAttempts (pairRunSummary run) of
  _ : replacement : _ ->
    wonOutcomes replacement events == [PeerDispatchWritten]
      && kernelOutcomes replacement events == [PeerDispatchWritten]
  _ -> False
  where
    events = runtimeTraceEvents (pairLocalTrace run)

attemptSelectionTickets :: PeerLogicalAttempt -> [RuntimeTraceEvent] -> [PeerDispatchTicket]
attemptSelectionTickets expected events =
  [ ticket
  | KernelEvent _ (KernelTraceStepped _ _ input (Right batch)) <- events,
    PeerInput (PeerDispatchSelected ticket _) <- [inputBody input],
    SemanticSendPeerItem _ attempt <- effectBatchMembers batch,
    attempt == expected
  ]

scheduledTicketsForOutcome :: PeerLogicalAttempt -> PeerDispatchOutcome -> [RuntimeTraceEvent] -> [PeerDispatchTicket]
scheduledTicketsForOutcome expectedAttempt expectedOutcome events =
  [ ticket
  | KernelEvent _ (KernelTraceStepped _ _ input (Right batch)) <- events,
    RuntimeObserved (PeerDispatchObserved attempt outcome) <- [inputBody input],
    attempt == expectedAttempt,
    outcome == expectedOutcome,
    SchedulePeerDispatch ticket <- effectBatchMembers batch
  ]

ticketTraceEvidence :: PeerDispatchTicket -> [RuntimeTraceEvent] -> (Int, Int, Int, Int, [PeerLogicalAttempt])
ticketTraceEvidence expected events =
  ( countShell isEligible,
    countShell isReleased,
    countShell isSuppressed,
    length selectedBatches,
    [ attempt
    | batch <- selectedBatches,
      SemanticSendPeerItem _ attempt <- effectBatchMembers batch
    ]
  )
  where
    countShell predicate = length [() | ShellEvent _ event <- events, predicate event]
    isEligible (ShellDelayedWorkEligible ticket) = ticket == expected
    isEligible _ = False
    isReleased (ShellDelayedWorkReleased ticket) = ticket == expected
    isReleased _ = False
    isSuppressed (ShellDelayedWorkSuppressed ticket) = ticket == expected
    isSuppressed _ = False
    selectedBatches =
      [ batch
      | KernelEvent _ (KernelTraceStepped _ _ input (Right batch)) <- events,
        PeerInput (PeerDispatchSelected ticket _) <- [inputBody input],
        ticket == expected
      ]

literalDrainTraceComplete :: PairRun -> Bool
literalDrainTraceComplete run = case pairLiteralDrainAttempt run of
  Just drainAttempt ->
    let won = wonOutcomes drainAttempt events
        suppressedWritten = filter (== PeerDispatchWritten) (suppressedOutcomes drainAttempt events)
        observed = kernelOutcomes drainAttempt events
        ordered = orderedDrainEvents drainAttempt events
     in won == [PeerDispatchDeferred]
          && suppressedWritten == [PeerDispatchWritten]
          && observed == [PeerDispatchDeferred]
          && ordered
          && postCutTicketTraceComplete (pairRemoteTrace run)
  Nothing -> False
  where
    events = runtimeTraceEvents (pairLocalTrace run)

drainCutDelayedSuppressions :: [RuntimeTraceEvent] -> [PeerDispatchTicket]
drainCutDelayedSuppressions events =
  [ ticket
  | ShellEvent _ (ShellDelayedWorkSuppressed ticket) <- eventsThroughCut
  ]
  where
    eventsThroughCut = case break isDrainCut events of
      (_, []) -> []
      (beforeCut, cut : _) -> beforeCut <> [cut]
    isDrainCut = \case
      ShellEvent _ ShellDrainCut {} -> True
      _ -> False

postCutTicketTraceComplete :: RuntimeTrace -> Bool
postCutTicketTraceComplete trace =
  case ( scheduledTicketsForDefinition postCutSuppressionDefinition events,
         drainCutDelayedSuppressions events
       ) of
    ([scheduled], [suppressed]) ->
      scheduled == suppressed
        && ticketTraceEvidence scheduled events == (1, 0, 1, 0, [])
    _ -> False
  where
    events = runtimeTraceEvents trace

postCutTicketEvidence :: RuntimeTrace -> ([PeerDispatchTicket], [PeerDispatchTicket], [(PeerDispatchTicket, (Int, Int, Int, Int, [PeerLogicalAttempt]))])
postCutTicketEvidence trace =
  (scheduled, suppressed, [(ticket, ticketTraceEvidence ticket events) | ticket <- scheduled <> suppressed])
  where
    events = runtimeTraceEvents trace
    scheduled = scheduledTicketsForDefinition postCutSuppressionDefinition events
    suppressed = drainCutDelayedSuppressions events

scheduledTicketsForDefinition :: ApplicationSortDefinition -> [RuntimeTraceEvent] -> [PeerDispatchTicket]
scheduledTicketsForDefinition expected events =
  [ ticket
  | KernelEvent _ (KernelTraceStepped _ _ input (Right batch)) <- events,
    ApplicationRequestInput (CallApplicationRequest _ _ _ (WriteApplication _ (PublishValue (SortDefinitionValue definition)))) <- [semanticStep9InputBody input],
    definition == expected,
    SchedulePeerDispatch ticket <- effectBatchMembers batch
  ]

wonOutcomes :: PeerLogicalAttempt -> [RuntimeTraceEvent] -> [PeerDispatchOutcome]
wonOutcomes attempt events =
  [ outcome
  | ShellEvent _ (ShellPeerOutcomeWon _ observed outcome) <- events,
    observed == attempt
  ]

suppressedOutcomes :: PeerLogicalAttempt -> [RuntimeTraceEvent] -> [PeerDispatchOutcome]
suppressedOutcomes attempt events =
  [ outcome
  | ShellEvent _ (ShellPeerOutcomeSuppressed _ observed outcome) <- events,
    observed == attempt
  ]

kernelOutcomes :: PeerLogicalAttempt -> [RuntimeTraceEvent] -> [PeerDispatchOutcome]
kernelOutcomes attempt events =
  [ outcome
  | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
    RuntimeObserved (PeerDispatchObserved observed outcome) <- [inputBody input],
    observed == attempt
  ]

orderedDrainEvents :: PeerLogicalAttempt -> [RuntimeTraceEvent] -> Bool
orderedDrainEvents attempt events =
  case traverse (findMatchingIndex events) predicates of
    Just [won, suppressed, observed, queued, barrier, finished] ->
      won < observed
        && observed < queued
        && queued < barrier
        && barrier < finished
        && won < suppressed
    _ -> False
  where
    predicates =
      [ isWonDeferredEvent,
        isSuppressedWrittenEvent,
        isObservedDeferredEvent,
        isBarrierQueuedEvent,
        isBarrierObservedEvent,
        isFinishRoutedEvent
      ]
    isWonDeferredEvent = \case
      ShellEvent _ (ShellPeerOutcomeWon _ observed PeerDispatchDeferred) -> observed == expected
      _ -> False
      where
        expected = attempt
    isObservedDeferredEvent = \case
      KernelEvent _ (KernelTraceStepped _ _ input _) ->
        inputBody input == RuntimeObserved (PeerDispatchObserved attempt PeerDispatchDeferred)
      _ -> False
    isSuppressedWrittenEvent = \case
      ShellEvent _ (ShellPeerOutcomeSuppressed _ observed PeerDispatchWritten) -> observed == attempt
      _ -> False
    isBarrierQueuedEvent = \case
      ShellEvent _ ShellDrainBarrierQueued {} -> True
      _ -> False
    isBarrierObservedEvent = \case
      KernelEvent _ (KernelTraceStepped _ _ input _) -> case inputBody input of
        RuntimeObserved DrainBarrierObserved {} -> True
        _ -> False
      _ -> False
    isFinishRoutedEvent = \case
      ShellEvent _ (ShellEffectRouted _ _ FinishDrain {}) -> True
      _ -> False

findMatchingIndex :: [value] -> (value -> Bool) -> Maybe Int
findMatchingIndex values predicate = findIndex predicate values

-- Trace assertions identify semantic work regardless of piggybacked receipt
-- metadata. Standalone retirement remains distinct in the recorded history.
semanticStep9InputBody :: HeraldInput -> HeraldInputBody
semanticStep9InputBody input = case inputBody input of
  ApplicationReceiptRetirementInput (RetireApplicationReceipts binding session _ (Just work)) -> case work of
    RetiringApplicationRequest request operation ->
      ApplicationRequestInput (CallApplicationRequest binding session request operation)
    RetiringApplicationLifecycle request locator command ->
      ApplicationLifecycleInput (CallApplicationLifecycle binding session request locator command)
  body -> body

queuedDrainCallWasNotSelected :: RuntimeTrace -> Bool
queuedDrainCallWasNotSelected trace = not (any isQueuedDrainCall (runtimeTraceEvents trace))
  where
    isQueuedDrainCall = \case
      KernelEvent _ (KernelTraceStepped _ _ input _) -> case semanticStep9InputBody input of
        ApplicationRequestInput (CallApplicationRequest _ _ _ (ReadApplication query)) ->
          query == queuedDrainQuery
        _ -> False
      _ -> False

allUnique :: (Eq value) => [value] -> Bool
allUnique values = length values == length (unique values)

unique :: (Eq value) => [value] -> [value]
unique = foldr (\value retained -> if value `elem` retained then retained else value : retained) []

replayFailure ::
  String ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeTrace ->
  [String]
replayFailure label genesis bootstraps trace =
  case replayRuntimeTrace genesis bootstraps trace of
    Left mismatch -> [label <> " replay mismatch: " <> show mismatch]
    Right _ -> []

summaryIsComplete :: PairSummary -> Bool
summaryIsComplete summary =
  summaryLostWrite summary == Client.SessionNoLongerLive
    && summaryExpectedValue summary == SortDefinitionValue (DeclaredSortDefinition declaredDescriptor (Just (summaryCommittedSort summary)))
    && summaryExpectedValue summary `elem` summaryRemoteValues summary
    && length (summaryRemoteValues summary) == 7
    && physicalAttemptShape
      (summaryLiteralDrainCut summary)
      (summaryAttempts summary)
    && length (summaryDelayCalls summary)
      >= ( if summaryLiteralDrainCut summary
             then 4
             else 3
         )
    && all (== pairDelayMicros) (summaryDelayCalls summary)
    && summaryStaleCallbackReturned summary
    && null (summaryForwardingFailures summary)
    && summaryLocalExit summary == Right HeraldRuntimeDrained
    && summaryRemoteExit summary == Right HeraldRuntimeDrained

assertSummary :: RecordingSchedule -> PairSummary -> Assertion
assertSummary recordingSchedule summary = do
  assertBool
    "the lost application stays terminal while its admitted value propagates to H2"
    (summaryIsComplete summary)
  case recordingSchedule of
    ProductionShapedRecording ->
      assertBool
        "production scheduling reaches the first and at least one same-item replacement attempt"
        ( length (summaryAttempts summary)
            >= (if summaryLiteralDrainCut summary then 3 else 2)
        )
    DeterministicallyGatedRecording ->
      assertEqual
        "deterministic gating reaches every expected physical attempt"
        (if summaryLiteralDrainCut summary then 3 else 2)
        (length (summaryAttempts summary))
  assertBool
    "the original and replacement schedules cross the injected delay boundary"
    ( length (summaryDelayCalls summary) >= 2
        && all (== pairDelayMicros) (summaryDelayCalls summary)
    )
  assertEqual "all typed forwarding completed before drain" [] (summaryForwardingFailures summary)

-- The production host delay is not a protocol barrier.  A replacement binding
-- may release its first retained attempt before the opposite resume offer is
-- stepped; an empty received prefix then validly selects another generation of
-- that same immutable item.  Gated schedules retain their exact cardinality.
physicalAttemptShape :: Bool -> [PeerLogicalAttempt] -> Bool
physicalAttemptShape False attempts@(first : _ : _) =
  allUniqueAttempts attempts
    && all
      ((== peerDispatchAttemptItem first) . peerDispatchAttemptItem)
      attempts
physicalAttemptShape True attempts@[first, replacement, drainAttempt] =
  allUniqueAttempts attempts
    && peerDispatchAttemptItem first == peerDispatchAttemptItem replacement
    && peerDispatchAttemptItem drainAttempt /= peerDispatchAttemptItem first
physicalAttemptShape _ _ = False

allUniqueAttempts :: [PeerLogicalAttempt] -> Bool
allUniqueAttempts attempts = length (uniqueAttempts attempts) == length attempts

uniqueAttempts :: [PeerLogicalAttempt] -> [PeerLogicalAttempt]
uniqueAttempts = foldr (\attempt retained -> if attempt `elem` retained then retained else attempt : retained) []

runPairScenario :: Bool -> RecordingSchedule -> PairSchedule -> IO PairRun
runPairScenario literalCut recordingSchedule schedule = do
  attemptGate <- newAttemptGate
  observedDelays <- newIORef []
  localClientTrace <- newIORef []
  remoteClientTrace <- newIORef []
  withRecordedRuntimePair recordingSchedule $ \localRecorded localPump remoteRecorded remotePump -> do
    let localRuntime = recordedHeraldRuntime localRecorded
        remoteRuntime = recordedHeraldRuntime remoteRecorded
    pair <- newRuntimePair localRecorded remoteRecorded attemptGate
    initialGeneration <- connectPeerGeneration pair
    localAdmin <- registerAdministration localRuntime
    remoteAdmin <- registerAdministration remoteRuntime
    localClient <- openPureClient localClientTrace localRuntime pairLocalBootstrapId 701
    -- This independent session exists before the fault and keeps the process
    -- live. The lost session itself is never resumed or replaced.
    survivingLocalClient <- openPureClient localClientTrace localRuntime pairLocalBootstrapId 705
    assertBool "the surviving session has its own identity" (clientSession survivingLocalClient /= clientSession localClient)
    remoteClient <- openPureClient remoteClientTrace remoteRuntime pairRemoteBootstrapId 702
    blocked <- startBlockedClient remoteRuntime pairRemoteBootstrapId 703

    (committedSort, lostWrite) <-
      publishWithLostReply observedDelays localRecorded attemptGate localClientTrace localRuntime localClient
    firstAttemptResult <- timeout 3000000 (takeMVar (attemptFirstEntered attemptGate))
    firstAttempt <- case firstAttemptResult of
      Just attempt -> pure attempt
      Nothing -> do
        localControls <- readIORef (endpointControls (pairLocal pair))
        remoteControls <- readIORef (endpointControls (pairRemote pair))
        assertFailure
          ( "blocked first peer attempt timed out; localControls="
              <> show localControls
              <> ", remoteControls="
              <> show remoteControls
          )

    releaseIndependentAt schedule ReleaseBeforePeerReconnect blocked
    closePeerGeneration schedule pair initialGeneration
    replacementPending <- beginPeerGeneration pair
    -- Registering the replacement makes its physical identities available,
    -- while completing its handshake may already wake the dormant retry.  The
    -- causal cut and phase-one receipt hold therefore belong between those two
    -- operations.  Only cumulative stream acknowledgements consult this hold,
    -- so unrelated handshake controls continue through the replacement writer.
    capturedReceipt <- newEmptyMVar
    publicationWatermark <- latestRuntimeEventOrdinal =<< currentTraceEvents remoteRecorded
    armReceiptHold
      (endpointHoldAcknowledgements (pairRemote pair))
      ( ReceiptHoldExpectation
          (pendingRemoteReference replacementPending)
          capturedReceipt
      )
    releaseStaleAt schedule ReleaseAfterReplacementRegistration attemptGate
    replacementGeneration <- completePeerGeneration pair replacementPending
    assertBool
      "reconnection advances both logical peer bindings"
      ( generationLocalBinding initialGeneration /= generationLocalBinding replacementGeneration
          && generationRemoteBinding initialGeneration /= generationRemoteBinding replacementGeneration
      )
    releaseStaleAt schedule ReleaseAfterReplacementHandshake attemptGate

    observeRecordedDelay observedDelays "the close-side Deferred schedules exactly one replacement delay" localRecorded
    releaseStaleAt schedule ReleaseAfterRetryDelay attemptGate
    _ <- awaitMVar "late first outcome callback" (attemptFirstReturned attemptGate)
    _ <- awaitMVar "old local peer close" (generationLocalClosed initialGeneration)
    _ <- awaitMVar "old remote peer close" (generationRemoteClosed initialGeneration)
    assertStalePeerGeneration pair initialGeneration

    let receiptContext =
          show recordingSchedule
            <> if literalCut then " literal-drain" else " ordinary"
    (replacementAttempt, forwarded) <-
      bracket_
        ( awaitRecordedEvidence
            (receiptContext <> " remote pump ownership")
            (pauseBatchPump remotePump)
        )
        (resumeBatchPump remotePump)
        ( do
            replacement@(replacementAttempt', _) <-
              releaseDelayedUntilReplacement observedDelays localRecorded attemptGate
            awaitRecordedEvidence
              "remote kernel publication delivery"
              ( awaitRecordedPeerPublicationAfter
                  publicationWatermark
                  remoteRecorded
                  (peerDispatchAttemptItem replacementAttempt')
              )
            events <- currentTraceEvents remoteRecorded
            (receiptBatch, receiptHandoff, receiptBinding, receiptEffect, receiptControl) <-
              publicationReceiptRouteFence
                publicationWatermark
                (peerDispatchAttemptItem replacementAttempt')
                events
            assertEqual
              "the publication receipt targets the replacement remote binding"
              (generationRemoteBinding replacementGeneration)
              receiptBinding
            driveRecordedEffectRoute
              "remote post-receipt acknowledgement"
              remoteRecorded
              receiptHandoff
              receiptBatch
              receiptEffect
            capturedBindingAndControl <-
              awaitMVar "captured replacement receipt acknowledgement" capturedReceipt
            assertEqual
              "the phase-one physical hold captured the exact derived receipt"
              (receiptBinding, receiptControl)
              capturedBindingAndControl
            enteredControl <-
              awaitAcknowledgementHandlerOwnership
                (pairRemote pair)
                (generationRemoteClosed replacementGeneration)
                remotePump
                remoteRecorded
                receiptBinding
            assertEqual
              "the routed receipt acknowledgement enters its exact peer writer"
              receiptControl
              enteredControl
            heldControl <-
              awaitMVar
                "held post-receipt acknowledgement"
                (endpointHeldAcknowledgement (pairRemote pair))
            assertEqual
              "the exact writer-owned receipt acknowledgement reaches the held gate"
              enteredControl
              heldControl
            pure replacement
        )
    assertEqual "the retransmitted opaque item enters H2" Queued forwarded
    assertBool
      "the replacement attempt retains the exact immutable peer item"
      ( firstAttempt /= replacementAttempt
          && peerDispatchAttemptItem firstAttempt == peerDispatchAttemptItem replacementAttempt
      )
    (remoteClientAfterFirst, firstRemoteValues) <- readRemoteDefinition remoteClientTrace 2 remoteRuntime remoteClient

    duplicateForwarded <-
      submitPeerPublication
        remoteRuntime
        (generationRemoteReference replacementGeneration)
        (generationRemoteBinding replacementGeneration)
        (peerDispatchAttemptItem replacementAttempt)
    assertEqual "the exact duplicate publication enters H2" Queued duplicateForwarded
    duplicateValues <- readRemoteDefinition remoteClientTrace 3 remoteRuntime remoteClientAfterFirst
    let remoteClientAfterDuplicate = fst duplicateValues
        visibleAfterDuplicate = snd duplicateValues
    assertEqual "duplicate delivery replays the exact visible cut without reapplying" firstRemoteValues visibleAfterDuplicate

    assertPreDrainAttemptEvidence
      recordingSchedule
      firstAttempt
      replacementAttempt
      attemptGate
    closePeerGeneration schedule pair replacementGeneration
    -- The local writer is not held, so its close callback is the causal fence
    -- that the old local generation has actually terminated.  Do not let the
    -- replacement handshake race an asynchronously queued close and then infer
    -- termination from a short elapsed-time bound.
    awaitWriterTermination
      "closed acknowledgement-loss local peer"
      attemptGate
      (generationLocalClosed replacementGeneration)
    acknowledgementPending <- beginPeerGeneration pair
    resumeTicketsBefore <-
      scheduledTicketsForResumeAcceptance <$> currentTraceEvents localRecorded
    acknowledgementGeneration <- completePeerGeneration pair acknowledgementPending
    resumeTicketsAfter <-
      scheduledTicketsForResumeAcceptance <$> currentTraceEvents localRecorded
    releaseOptionalScheduledDelays
      observedDelays
      localRecorded
      "resume-selected duplicate"
      (filter (`notElem` resumeTicketsBefore) resumeTicketsAfter)
    -- The remote writer was intentionally held inside the exact receipt
    -- acknowledgement.  Releasing that handler lets the close already queued
    -- behind it run; its callback is the corresponding remote termination
    -- fence.  A generous timeout below is only an outer deadlock watchdog.
    putOnce (endpointReleaseAcknowledgements (pairRemote pair)) ()
    awaitWriterTermination
      "closed acknowledgement-loss remote peer"
      attemptGate
      (generationRemoteClosed replacementGeneration)
    assertStalePeerGeneration pair replacementGeneration

    releaseIndependentAt schedule ReleaseBeforeRemoteRead blocked
    (remoteClientAfterRead, remoteValues) <- readRemoteDefinition remoteClientTrace 4 remoteRuntime remoteClientAfterDuplicate
    written <-
      case [ identifier
           | SortDefinitionValue
               (DeclaredSortDefinition descriptor (Just identifier)) <-
               remoteValues,
             descriptor == declaredDescriptor
           ] of
        [identifier] -> pure identifier
        identifiers ->
          assertFailure
            ( "expected one remotely visible declared SortId, got "
                <> show (length identifiers)
            )
    let expectedValue =
          SortDefinitionValue
            (DeclaredSortDefinition declaredDescriptor (Just written))
    assertBool
      "H2's local read observes the definition propagated from H1"
      (expectedValue `elem` remoteValues)
    assertEqual "the remote visible cut contains six primordial definitions and the declaration" 7 (length remoteValues)
    releaseIndependentAt schedule ReleaseAfterRemoteRead blocked

    (remoteClientForDrain, _) <-
      if literalCut
        then
          beginPostCutSuppressedPublication
            remoteClientTrace
            remoteRuntime
            remoteClientAfterRead
            remoteRecorded
            0
        else pure (remoteClientAfterRead, Nothing)

    (queuedApplication, drainAttempt) <-
      if literalCut
        then do
          pauseBatchPump remotePump
          queued <- startDispositionPausedCall remoteRuntime remoteClientForDrain
          awaitRecordedEvidence "paused remote disposition batch" (awaitRecordedBatch remoteRecorded)
          attempt <- beginDrainAttempt observedDelays localClientTrace localRuntime survivingLocalClient localRecorded attemptGate
          pauseBatchPump localPump
          pure (Just queued, Just attempt)
        else pure (Nothing, Nothing)

    failuresBeforeDrain <- readIORef (pairForwardingFailures pair)
    assertEqual "all typed relays are admitted before the drain cut" [] failuresBeforeDrain
    assertEqual "the local drain request is queued" Queued
      =<< requestHeraldRuntimeDrain localRuntime localAdmin (adminCorrelationId 301)
    assertEqual "the remote drain request is queued" Queued
      =<< requestHeraldRuntimeDrain remoteRuntime remoteAdmin (adminCorrelationId 302)
    when literalCut $ do
      awaitRecordedEvidence "local drain cut" (awaitRecordedDrainCut localRecorded)
      awaitRecordedEvidence "remote drain cut" (awaitRecordedDrainCut remoteRecorded)
      awaitRecordedEvidence "route remote held Open batch" (releaseRecordedBatch remoteRecorded)
      awaitRecordedEvidence "remote pre-cut batch" (awaitRecordedBatch remoteRecorded)
      awaitRecordedEvidence "route remote post-cut batch" (releaseRecordedBatch remoteRecorded)
      resumeBatchPump remotePump
      forM_ drainAttempt $ \attempt ->
        awaitRecordedEvidence
          "drain-winning deferred peer outcome"
          (advanceRecordedBatchesUntilPeerOutcome localRecorded attempt PeerDispatchDeferred)
      putOnce (attemptDrainRelease attemptGate) ()
      _ <- awaitMVar "late drain-attempt writer result" (attemptDrainReturned attemptGate)
      forM_ drainAttempt $ \attempt ->
        awaitRecordedEvidence
          "suppressed late drain-attempt writer result"
          (awaitRecordedPeerSuppression localRecorded attempt PeerDispatchWritten)
      resumeBatchPump localPump
      pure ()
    localExit <- awaitRuntimeExit "local composed drain" localRuntime
    remoteExit <- awaitRuntimeExit "remote composed drain" remoteRuntime

    forM_ drainAttempt $ \attempt ->
      assertBool "the handed drain attempt is distinct from the completed publication" (peerDispatchAttemptItem attempt /= peerDispatchAttemptItem replacementAttempt)
    _ <- awaitMVar "drain local peer close" (generationLocalClosed acknowledgementGeneration)
    _ <- awaitMVar "drain remote peer close" (generationRemoteClosed acknowledgementGeneration)
    _ <- awaitMVar "surviving sibling application close" (applicationLaneClosed (clientLane survivingLocalClient))
    _ <- awaitMVar "remote reader application close" (applicationLaneClosed (clientLane remoteClient))
    _ <- awaitMVar "unrelated blocked writer close" (blockedClientClosed blocked)
    forM_ queuedApplication $ \lane ->
      void (awaitMVar "queued-unapplied application source close" (applicationLaneClosed lane))

    attempts <- fmap snd <$> readIORef (attemptObserved attemptGate)
    staleReturned <- readIORef (attemptLateReturnObserved attemptGate)
    delayCalls <- readIORef observedDelays
    localTrace <- snapshotRecordedHeraldRuntime localRecorded
    remoteTrace <- snapshotRecordedHeraldRuntime remoteRecorded
    localClientEvents <- readIORef localClientTrace
    remoteClientEvents <- readIORef remoteClientTrace
    let summary =
          PairSummary
            { summaryLostWrite = lostWrite,
              summaryCommittedSort = committedSort,
              summaryRemoteValues = remoteValues,
              summaryExpectedValue = expectedValue,
              summaryAttempts = attempts,
              summaryDelayCalls = delayCalls,
              summaryStaleCallbackReturned = staleReturned,
              summaryLiteralDrainCut = literalCut,
              summaryForwardingFailures = failuresBeforeDrain,
              summaryLocalExit = localExit,
              summaryRemoteExit = remoteExit
            }
        replayFailures =
          replayFailure "local" pairLocalGenesis pairBidirectionalLocalBootstraps localTrace
            <> replayFailure "remote" pairRemoteGenesis pairBidirectionalRemoteBootstraps remoteTrace
    pure
      PairRun
        { pairRunSummary = summary,
          pairLiteralDrainAttempt = drainAttempt,
          pairLocalTrace = localTrace,
          pairRemoteTrace = remoteTrace,
          pairClientTrace = PairClientTrace localClientEvents remoteClientEvents,
          pairReplayFailures = replayFailures
        }

performYields :: PairSchedule -> IO ()
performYields schedule = replicateM_ (scheduleYields schedule) yield

releaseDelayedUntilReplacement ::
  IORef [Word64] ->
  RecordedHeraldRuntime ->
  AttemptGate ->
  IO (PeerLogicalAttempt, RuntimeSubmission)
releaseDelayedUntilReplacement observedDelays recorded gate = do
  -- Release the close-side ticket already observed by the caller. Its stale
  -- selection deterministically schedules the one replacement ticket.
  releaseRecordedDelay recorded
  (_, actual) <-
    awaitRecordedEvidence
      "replacement publication delay"
      (awaitRecordedDelay recorded)
  append observedDelays actual
  assertEqual "the replacement publication retains the configured delay" pairDelayMicros actual
  releaseRecordedDelay recorded
  awaitMVar "replacement publication relay" (attemptReplacementForwarded gate)

observeRecordedDelay :: IORef [Word64] -> String -> RecordedHeraldRuntime -> IO ()
observeRecordedDelay observedDelays context recorded = do
  (_, actual) <- awaitRecordedEvidence context (awaitRecordedDelay recorded)
  append observedDelays actual
  assertEqual context pairDelayMicros actual

releaseOptionalScheduledDelays ::
  IORef [Word64] ->
  RecordedHeraldRuntime ->
  String ->
  [PeerDispatchTicket] ->
  IO ()
releaseOptionalScheduledDelays observedDelays recorded context expected =
  forM_ expected $ \ticket -> do
    (actualTicket, actualDelay) <-
      awaitRecordedEvidence
        (context <> " exact ticket delay")
        (awaitRecordedDelay recorded)
    assertEqual (context <> " delay belongs to its exact scheduled ticket") ticket actualTicket
    assertEqual (context <> " retains the configured delay") pairDelayMicros actualDelay
    append observedDelays actualDelay
    releaseRecordedDelay recorded

scheduledTicketsForResumeAcceptance :: [RuntimeTraceEvent] -> [PeerDispatchTicket]
scheduledTicketsForResumeAcceptance events =
  [ ticket
  | KernelEvent _ (KernelTraceStepped _ _ input (Right batch)) <- events,
    PeerInput (SemanticPeerControlReceived _ PeerStreamResumeAccepted {}) <- [inputBody input],
    SchedulePeerDispatch ticket <- effectBatchMembers batch
  ]

currentTraceEvents :: RecordedHeraldRuntime -> IO [RuntimeTraceEvent]
currentTraceEvents = snapshotRecordedRuntimeEvents

latestRuntimeEventOrdinal :: [RuntimeTraceEvent] -> IO RuntimeEventOrdinal
latestRuntimeEventOrdinal events = case reverse events of
  event : _ -> pure (runtimeEventOrdinal event)
  [] -> assertFailure "the running remote runtime had no trace watermark"

runtimeEventOrdinal :: RuntimeTraceEvent -> RuntimeEventOrdinal
runtimeEventOrdinal = \case
  KernelEvent ordinal _ -> ordinal
  ShellEvent ordinal _ -> ordinal

publicationReceiptRouteFence ::
  RuntimeEventOrdinal ->
  PeerLogicalItem ->
  [RuntimeTraceEvent] ->
  IO (BatchOrdinal, RuntimeEventOrdinal, PeerBinding, HeraldEffect, PeerControl)
publicationReceiptRouteFence watermark expectedItem = go
  where
    go [] = assertFailure "the exact remote publication step was absent from the recorder ledger"
    go (event : retained) = case event of
      KernelEvent ordinal (KernelTraceStepped _ _ input (Right batch))
        | ordinal > watermark,
          PeerInput (SemanticPeerPublicationReceived _ item) <- inputBody input,
          item == expectedItem ->
            case [ (binding, effect, control)
                 | effect@(SemanticSendPeerControl binding control) <- effectBatchMembers batch,
                   isCumulativeStreamAcknowledgement control
                 ] of
              [(binding, effect, control)] -> do
                (handoffOrdinal, actualBatch) <- case retained of
                  ShellEvent handoffEventOrdinal (ShellBatchHanded handedBatch) : _ ->
                    pure (handoffEventOrdinal, handedBatch)
                  nextEvent : _ ->
                    assertFailure
                      ( "the exact remote publication step was not immediately followed by its handed batch: "
                          <> show nextEvent
                      )
                  [] ->
                    assertFailure
                      "the exact remote publication step had no following handed batch"
                pure (actualBatch, handoffOrdinal, binding, effect, control)
              effects ->
                assertFailure
                  ( "the exact remote publication step produced "
                      <> show (length effects)
                      <> " receipt acknowledgements"
                  )
      _ -> go retained

isCumulativeStreamAcknowledgement :: PeerControl -> Bool
isCumulativeStreamAcknowledgement = \case
  PeerStreamReceived {} -> True
  PeerStreamCompleted {} -> True
  _ -> False

driveRecordedEffectRoute ::
  String ->
  RecordedHeraldRuntime ->
  RuntimeEventOrdinal ->
  BatchOrdinal ->
  HeraldEffect ->
  IO ()
driveRecordedEffectRoute context recorded handoffOrdinal expectedBatch expectedEffect = go
  where
    go = do
      events <- currentTraceEvents recorded
      if exactEffectWasRoutedAfter handoffOrdinal expectedBatch expectedEffect events
        then pure ()
        else do
          awaitRecordedEvidence (context <> " batch") (awaitRecordedBatch recorded)
          awaitRecordedEvidence (context <> " route") (releaseRecordedBatch recorded)
          go

exactEffectWasRoutedAfter :: RuntimeEventOrdinal -> BatchOrdinal -> HeraldEffect -> [RuntimeTraceEvent] -> Bool
exactEffectWasRoutedAfter handoffOrdinal expectedBatch expectedEffect = any $ \case
  ShellEvent ordinal (ShellEffectRouted batch _ effect) ->
    ordinal > handoffOrdinal
      && batch == expectedBatch
      && effect == expectedEffect
  _ -> False

releaseStaleAt :: PairSchedule -> StaleReleasePosition -> AttemptGate -> IO ()
releaseStaleAt schedule position gate =
  when (scheduleStaleRelease schedule == position) $ do
    performYields schedule
    putOnce (attemptReleaseFirst gate) ()

releaseIndependentAt :: PairSchedule -> IndependentReleasePosition -> BlockedClient -> IO ()
releaseIndependentAt schedule position blocked =
  when (scheduleIndependentRelease schedule == position) $ do
    performYields schedule
    putOnce (blockedClientRelease blocked) ()

pairDelayMicros :: Word64
pairDelayMicros = 700

data AttemptGate = AttemptGate
  { attemptObserved :: IORef [(Int, PeerLogicalAttempt)],
    attemptFirstEntered :: MVar PeerLogicalAttempt,
    attemptReleaseFirst :: MVar (),
    attemptFirstReturned :: MVar (),
    attemptLateReturnObserved :: IORef Bool,
    attemptReplacementForwarded :: MVar (PeerLogicalAttempt, RuntimeSubmission),
    attemptDrainArmed :: MVar (),
    attemptDrainEntered :: MVar PeerLogicalAttempt,
    attemptDrainRelease :: MVar (),
    attemptDrainReturned :: MVar ()
  }

newAttemptGate :: IO AttemptGate
newAttemptGate =
  AttemptGate
    <$> newIORef []
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newIORef False
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar

data PeerEndpoint = PeerEndpoint
  { endpointName :: String,
    endpointRuntime :: HeraldRuntime,
    endpointRecorded :: RecordedHeraldRuntime,
    endpointReference :: MVar (ConnectionRef PeerPlane),
    endpointBinding :: MVar PeerBinding,
    endpointControls :: IORef [PeerControl],
    endpointHoldAcknowledgements :: MVar (Maybe ReceiptHoldExpectation),
    endpointAcknowledgementHandlerEntered :: MVar PeerControl,
    endpointHeldAcknowledgement :: MVar PeerControl,
    endpointReleaseAcknowledgements :: MVar (),
    endpointAttemptGate :: Maybe AttemptGate
  }

newPeerEndpoint :: String -> RecordedHeraldRuntime -> Maybe AttemptGate -> IO PeerEndpoint
newPeerEndpoint name recorded attemptGate =
  PeerEndpoint name (recordedHeraldRuntime recorded) recorded
    <$> newEmptyMVar
    <*> newEmptyMVar
    <*> newIORef []
    <*> newMVar Nothing
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> pure attemptGate

data ReceiptHoldExpectation
  = ReceiptHoldExpectation
      (ConnectionRef PeerPlane)
      (MVar (PeerBinding, PeerControl))

armReceiptHold :: MVar (Maybe ReceiptHoldExpectation) -> ReceiptHoldExpectation -> IO ()
armReceiptHold gate expectation = do
  armed <-
    modifyMVar gate $ \case
      Nothing -> pure (Just expectation, True)
      retained -> pure (retained, False)
  unless armed (assertFailure "receipt acknowledgement hold was already armed")

claimReceiptHold ::
  MVar (Maybe ReceiptHoldExpectation) ->
  ConnectionRef PeerPlane ->
  PeerBinding ->
  PeerControl ->
  IO Bool
claimReceiptHold gate reference binding control =
  modifyMVar gate $ \expectation -> case expectation of
    Just (ReceiptHoldExpectation expectedReference capturedReceipt)
      | reference == expectedReference -> do
          captured <- tryPutMVar capturedReceipt (binding, control)
          unless captured (assertFailure "replacement receipt capture was already occupied")
          pure (Nothing, True)
    _ -> pure (expectation, False)

data AcknowledgementHandlerOutcome
  = AcknowledgementHandlerEntered PeerControl
  | AcknowledgementBindingLost
  | AcknowledgementConnectionClosed
  | AcknowledgementPumpStopped (Either SomeException ())
  | AcknowledgementRuntimeStopped (Either RuntimeInternal.HeraldRuntimeFailure HeraldRuntimeExit)
  | AcknowledgementWatcherFailed String SomeException
  | AcknowledgementOwnershipTimedOut

-- The exact receipt effect has already been recorded as routed before this is
-- called.  Race its physical writer entry against every terminal owner that
-- can make that entry impossible, with one bounded invariant-failure fallback.
awaitAcknowledgementHandlerOwnership ::
  PeerEndpoint ->
  MVar () ->
  BatchPump ->
  RecordedHeraldRuntime ->
  PeerBinding ->
  IO PeerControl
awaitAcknowledgementHandlerOwnership
  endpoint
  connectionClosed
  pump
  recorded
  binding = do
    outcome <- newEmptyMVar
    observed <-
      bracket
        ( mapM
            (spawnAcknowledgementWatcher outcome)
            [ ( "peer writer entry",
                AcknowledgementHandlerEntered
                  <$> readMVar (endpointAcknowledgementHandlerEntered endpoint)
              ),
              ( "peer binding loss",
                AcknowledgementBindingLost
                  <$ awaitRecordedPeerBindingLoss recorded binding
              ),
              ( "physical peer close",
                AcknowledgementConnectionClosed <$ readMVar connectionClosed
              ),
              ( "remote batch pump",
                AcknowledgementPumpStopped <$> awaitBatchPumpTermination pump
              ),
              ( "remote runtime",
                AcknowledgementRuntimeStopped
                  <$> awaitHeraldRuntimeExit (endpointRuntime endpoint)
              )
            ]
        )
        (mapM_ killThread)
        ( const $ do
            waited <- timeout 3000000 (takeMVar outcome)
            pure (maybe AcknowledgementOwnershipTimedOut id waited)
        )
    case observed of
      AcknowledgementHandlerEntered control -> pure control
      AcknowledgementBindingLost ->
        assertFailure "the exact receipt acknowledgement lost its peer binding before writer entry"
      AcknowledgementConnectionClosed ->
        assertFailure "the replacement peer connection closed before receipt writer entry"
      AcknowledgementPumpStopped stopped ->
        assertFailure ("the remote batch pump stopped before receipt writer entry: " <> show stopped)
      AcknowledgementRuntimeStopped stopped ->
        assertFailure ("the remote runtime stopped before receipt writer entry: " <> show stopped)
      AcknowledgementWatcherFailed label problem ->
        assertFailure
          ( label
              <> " ownership watcher failed before receipt writer entry: "
              <> displayException problem
          )
      AcknowledgementOwnershipTimedOut ->
        assertFailure "receipt acknowledgement writer or terminal owner timed out"

spawnAcknowledgementWatcher ::
  MVar AcknowledgementHandlerOutcome ->
  (String, IO AcknowledgementHandlerOutcome) ->
  IO ThreadId
spawnAcknowledgementWatcher outcome (label, action) =
  forkFinally action $ \case
    Left problem -> putOnce outcome (AcknowledgementWatcherFailed label problem)
    Right observed -> putOnce outcome observed

mvarOccupied :: MVar value -> IO Bool
mvarOccupied cell = do
  observed <- tryReadMVar cell
  pure $ case observed of
    Nothing -> False
    Just _ -> True

awaitBatchPumpTermination :: BatchPump -> IO (Either SomeException ())
awaitBatchPumpTermination (BatchPump _ _ done) = readMVar done

data RuntimePair = RuntimePair
  { pairLocal :: PeerEndpoint,
    pairRemote :: PeerEndpoint,
    pairForwardingFailures :: IORef [(String, RuntimeSubmission)]
  }

newRuntimePair :: RecordedHeraldRuntime -> RecordedHeraldRuntime -> AttemptGate -> IO RuntimePair
newRuntimePair localRecorded remoteRecorded attemptGate = do
  failures <- newIORef []
  local <- newPeerEndpoint "local" localRecorded (Just attemptGate)
  remote <- newPeerEndpoint "remote" remoteRecorded Nothing
  pure (RuntimePair local remote failures)

data PendingPeerGeneration = PendingPeerGeneration
  { pendingLocalReference :: ConnectionRef PeerPlane,
    pendingRemoteReference :: ConnectionRef PeerPlane,
    pendingLocalLease :: PeerRelayLease,
    pendingRemoteLease :: PeerRelayLease,
    pendingLocalClosed :: MVar (),
    pendingRemoteClosed :: MVar (),
    pendingLocalResume :: MVar PeerResumeForwardWitness,
    pendingRemoteResume :: MVar PeerResumeForwardWitness
  }

data PeerResumeForwardWitness
  = PeerResumeForwardWitness
      (ConnectionRef PeerPlane)
      PeerBinding
      PeerControl

data PeerGeneration = PeerGeneration
  { generationLocalReference :: ConnectionRef PeerPlane,
    generationRemoteReference :: ConnectionRef PeerPlane,
    generationLocalLease :: PeerRelayLease,
    generationRemoteLease :: PeerRelayLease,
    generationLocalBinding :: PeerBinding,
    generationRemoteBinding :: PeerBinding,
    generationLocalClosed :: MVar (),
    generationRemoteClosed :: MVar ()
  }

-- A physical writer keeps the opposite lease that was current when this
-- generation was wired.  Late output therefore reaches the old physical lane
-- (and becomes stale after close) instead of being retargeted onto a replacement.
data PeerRelayLease = PeerRelayLease
  { relayLeaseReference :: MVar (ConnectionRef PeerPlane),
    relayLeaseBinding :: MVar PeerBinding,
    relayLeaseRetired :: MVar ()
  }

newPeerRelayLease :: IO PeerRelayLease
newPeerRelayLease = PeerRelayLease <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar

connectPeerGeneration :: RuntimePair -> IO PeerGeneration
connectPeerGeneration pair = beginPeerGeneration pair >>= completePeerGeneration pair

beginPeerGeneration :: RuntimePair -> IO PendingPeerGeneration
beginPeerGeneration pair = do
  clearMVar (endpointBinding (pairLocal pair))
  clearMVar (endpointBinding (pairRemote pair))
  localClosed <- newEmptyMVar
  remoteClosed <- newEmptyMVar
  localResume <- newEmptyMVar
  remoteResume <- newEmptyMVar
  localLease <- newPeerRelayLease
  remoteLease <- newPeerRelayLease
  localReference <-
    registerPeer
      (endpointRuntime (pairLocal pair))
      ( relayPeerHandlers
          pair
          (pairLocal pair)
          localLease
          (pairRemote pair)
          remoteLease
          localClosed
          localResume
      )
  remoteReference <-
    registerPeer
      (endpointRuntime (pairRemote pair))
      ( relayPeerHandlers
          pair
          (pairRemote pair)
          remoteLease
          (pairLocal pair)
          localLease
          remoteClosed
          remoteResume
      )
  putMVar (relayLeaseReference localLease) localReference
  putMVar (relayLeaseReference remoteLease) remoteReference
  replaceMVar (endpointReference (pairLocal pair)) localReference
  replaceMVar (endpointReference (pairRemote pair)) remoteReference
  pure
    PendingPeerGeneration
      { pendingLocalReference = localReference,
        pendingRemoteReference = remoteReference,
        pendingLocalLease = localLease,
        pendingRemoteLease = remoteLease,
        pendingLocalClosed = localClosed,
        pendingRemoteClosed = remoteClosed,
        pendingLocalResume = localResume,
        pendingRemoteResume = remoteResume
      }

completePeerGeneration :: RuntimePair -> PendingPeerGeneration -> IO PeerGeneration
completePeerGeneration pair pending = do
  localControlWatermark <- latestRuntimeEventOrdinal =<< currentTraceEvents (endpointRecorded (pairLocal pair))
  remoteControlWatermark <- latestRuntimeEventOrdinal =<< currentTraceEvents (endpointRecorded (pairRemote pair))
  assertEqual "the local peer candidate enters its source FIFO" Queued
    =<< openPeerCandidate
      (endpointRuntime (pairLocal pair))
      (pendingLocalReference pending)
      mempty
      Nothing
  localBinding <- awaitReadMVar "local accepted peer binding" (endpointBinding (pairLocal pair))
  remoteBinding <- awaitReadMVar "remote accepted peer binding" (endpointBinding (pairRemote pair))
  localResume <- awaitMVar "local stream-resume response" (pendingLocalResume pending)
  remoteResume <- awaitMVar "remote stream-resume response" (pendingRemoteResume pending)
  -- ResumeAccepted is the terminal handshake control and the writer/source
  -- FIFOs place every earlier control before it.  Fence each direction by the
  -- exact replacement source and exact forwarded terminal input.
  awaitPeerResumeTerminal
    (pairRemote pair)
    remoteControlWatermark
    (pendingRemoteReference pending)
    remoteBinding
    localResume
  awaitPeerResumeTerminal
    (pairLocal pair)
    localControlWatermark
    (pendingLocalReference pending)
    localBinding
    remoteResume
  pure
    PeerGeneration
      { generationLocalReference = pendingLocalReference pending,
        generationRemoteReference = pendingRemoteReference pending,
        generationLocalLease = pendingLocalLease pending,
        generationRemoteLease = pendingRemoteLease pending,
        generationLocalBinding = localBinding,
        generationRemoteBinding = remoteBinding,
        generationLocalClosed = pendingLocalClosed pending,
        generationRemoteClosed = pendingRemoteClosed pending
      }
closePeerGeneration :: PairSchedule -> RuntimePair -> PeerGeneration -> IO ()
closePeerGeneration schedule pair generation = do
  putOnce (relayLeaseRetired (generationLocalLease generation)) ()
  putOnce (relayLeaseRetired (generationRemoteLease generation)) ()
  let closeLocal =
        assertEqual "the blocked local peer lease closes exactly once" ConnectionClosed
          =<< closeRuntimeConnection
            (endpointRuntime (pairLocal pair))
            (generationLocalReference generation)
      closeRemote =
        assertEqual "the remote peer lease closes exactly once" ConnectionClosed
          =<< closeRuntimeConnection
            (endpointRuntime (pairRemote pair))
            (generationRemoteReference generation)
  if scheduleCloseRemoteFirst schedule
    then closeRemote >> performYields schedule >> closeLocal
    else closeLocal >> performYields schedule >> closeRemote
  localRetired <- mvarOccupied (relayLeaseRetired (generationLocalLease generation))
  remoteRetired <- mvarOccupied (relayLeaseRetired (generationRemoteLease generation))
  assertBool "closing retires both immutable generation relay leases" (localRetired && remoteRetired)

assertStalePeerGeneration :: RuntimePair -> PeerGeneration -> IO ()
assertStalePeerGeneration pair generation =
  assertEqual "the removed local lease is stale" StalePhysicalGeneration
    =<< closeRuntimeConnection
      (endpointRuntime (pairLocal pair))
      (generationLocalReference generation)

relayPeerHandlers ::
  RuntimePair ->
  PeerEndpoint ->
  PeerRelayLease ->
  PeerEndpoint ->
  PeerRelayLease ->
  MVar () ->
  MVar PeerResumeForwardWitness ->
  RuntimePeerConnectionHandlers
relayPeerHandlers pair own ownLease target targetLease closed resumeObserved =
  runtimePeerConnectionHandlers
    ( \candidate envelope ->
        case admitForwardedCandidate candidate envelope of
          Left _ -> pure LaneLost
          Right (CandidatePeerHello admittedCandidate hello generation members) -> do
            reference <- readMVar (relayLeaseReference targetLease)
            submission <-
              submitPeerHello
                (endpointRuntime target)
                reference
                admittedCandidate
                mempty
                hello
                generation
                members
                Nothing
            recordRelayForwarding pair targetLease (endpointName own <> " Hello") submission
            pure LaneOffered
    )
    ( \_ disposition -> do
        case disposition of
          PeerHelloAccepted _ binding -> do
            replaceMVar (relayLeaseBinding ownLease) binding
            ownReference <- readMVar (relayLeaseReference ownLease)
            currentReference <- readMVar (endpointReference own)
            when (ownReference == currentReference) (replaceMVar (endpointBinding own) binding)
          PeerHelloRejected _ ->
            append
              (pairForwardingFailures pair)
              (endpointName own <> " Hello rejected", RuntimeStopped)
        pure LaneOffered
    )
    ( \binding envelope -> do
        ownReference <- readMVar (relayLeaseReference ownLease)
        let holdOrForwardAcknowledgement progress acknowledgement = do
              holdAcknowledgement <-
                claimReceiptHold
                  (endpointHoldAcknowledgements own)
                  ownReference
                  binding
                  acknowledgement
              if holdAcknowledgement
                then do
                  putOnce (endpointAcknowledgementHandlerEntered own) acknowledgement
                  putOnce (endpointHeldAcknowledgement own) acknowledgement
                  takeMVar (endpointReleaseAcknowledgements own)
                  pure LaneLost
                else void (forwardControl progress acknowledgement) >> pure LaneOffered
        case admitEstablishedPeerEnvelope envelope of
          Left _ -> pure LaneLost
          Right (EstablishedPeerPublication _ _) -> pure LaneLost
          Right (EstablishedPeerControl progress control) -> do
            result <- case control of
              PeerStreamResumeAccepted {} -> do
                (submission, witness) <- forwardControl progress control
                when (submission == Queued) (putOnce resumeObserved witness)
                pure LaneOffered
              PeerStreamReceived {} ->
                holdOrForwardAcknowledgement progress control
              PeerStreamCompleted {} ->
                holdOrForwardAcknowledgement progress control
              _ -> void (forwardControl progress control) >> pure LaneOffered
            append (endpointControls own) control
            pure result
    )
    (\_ _ -> pure LaneOffered)
    ( \_ attempt envelope -> case admitEstablishedPeerEnvelope envelope of
        Right (EstablishedPeerPublication progress item) ->
          handlePeerAttempt pair own target targetLease progress attempt item
        _ -> pure PeerDispatchFailed
    )
    (putOnce closed ())
  where
    forwardControl progress control = do
      reference <- readMVar (relayLeaseReference targetLease)
      binding <- readMVar (relayLeaseBinding targetLease)
      submission <- submitPeerControlWithProgress (endpointRuntime target) reference binding progress control
      recordRelayForwarding pair targetLease (endpointName own <> " control") submission
      pure (submission, PeerResumeForwardWitness reference binding control)

admitForwardedCandidate ::
  PeerCandidate ->
  PeerEnvelope ->
  Either PeerRpcAdmissionError CandidatePeerInbound
admitForwardedCandidate candidate envelope =
  case admitCandidatePeerEnvelope RemoteInitiatedCandidate envelope of
    right@(Right (CandidatePeerHello derived _ _ _))
      | derived == candidate -> right
    _ -> admitCandidatePeerEnvelope (LocallyInitiatedCandidate candidate) envelope

awaitPeerResumeTerminal ::
  PeerEndpoint ->
  RuntimeEventOrdinal ->
  ConnectionRef PeerPlane ->
  PeerBinding ->
  PeerResumeForwardWitness ->
  IO ()
awaitPeerResumeTerminal
  target
  watermark
  expectedReference
  expectedBinding
  (PeerResumeForwardWitness actualReference actualBinding control) = do
    assertEqual
      "the terminal resume control targets the exact replacement reference"
      expectedReference
      actualReference
    assertEqual
      "the terminal resume control targets the exact replacement binding"
      expectedBinding
      actualBinding
    case control of
      PeerStreamResumeAccepted {} -> pure ()
      other -> assertFailure ("expected terminal PeerStreamResumeAccepted, got " <> show other)
    awaitRecordedEvidence
      ( endpointName target
          <> " runtime stepping the exact terminal resume control from source "
          <> show (RuntimeInternal.connectionRefOrdinal expectedReference)
      )
      ( awaitRecordedPeerControlAfter
          watermark
          (SourceId PeerSources (RuntimeInternal.connectionRefOrdinal expectedReference))
          expectedBinding
          control
          (endpointRecorded target)
      )

handlePeerAttempt :: RuntimePair -> PeerEndpoint -> PeerEndpoint -> PeerRelayLease -> ReceiptRetirement -> PeerLogicalAttempt -> PeerLogicalItem -> IO PeerDispatchOutcome
handlePeerAttempt pair own target targetLease progress attempt item = do
  assertEqual "wire projection retains the exact opaque publication item" (peerDispatchAttemptItem attempt) item
  case endpointAttemptGate own of
    Nothing -> relayWritten
    Just gate -> do
      ordinal <-
        atomicModifyIORef'
          (attemptObserved gate)
          ( \observed ->
              let next = length observed + 1
               in (observed <> [(next, attempt)], next)
          )
      case ordinal of
        1 -> do
          putOnce (attemptFirstEntered gate) attempt
          takeMVar (attemptReleaseFirst gate)
          atomicModifyIORef' (attemptLateReturnObserved gate) (const (True, ()))
          putOnce (attemptFirstReturned gate) ()
          pure PeerDispatchDeferred
        _ -> do
          drainArmed <- tryTakeMVar (attemptDrainArmed gate)
          case drainArmed of
            Just () -> do
              putOnce (attemptDrainEntered gate) attempt
              takeMVar (attemptDrainRelease gate)
              putOnce (attemptDrainReturned gate) ()
              pure PeerDispatchWritten
            Nothing -> do
              submission <- relayPublicationWithProgress target targetLease progress item
              recordRelayForwarding pair targetLease (endpointName own <> " publication") submission
              putOnce (attemptReplacementForwarded gate) (attempt, submission)
              pure (submissionOutcome submission)
  where
    relayWritten = do
      submission <- relayPublicationWithProgress target targetLease progress item
      recordRelayForwarding pair targetLease (endpointName own <> " publication") submission
      pure (submissionOutcome submission)

relayPublicationWithProgress :: PeerEndpoint -> PeerRelayLease -> ReceiptRetirement -> PeerLogicalItem -> IO RuntimeSubmission
relayPublicationWithProgress target targetLease progress item = do
  reference <- readMVar (relayLeaseReference targetLease)
  binding <- readMVar (relayLeaseBinding targetLease)
  submitPeerPublicationWithProgress
    (endpointRuntime target)
    reference
    binding
    progress
    item

submissionOutcome :: RuntimeSubmission -> PeerDispatchOutcome
submissionOutcome Queued = PeerDispatchWritten
submissionOutcome _ = PeerDispatchFailed

recordRelayForwarding :: RuntimePair -> PeerRelayLease -> String -> RuntimeSubmission -> IO ()
recordRelayForwarding pair targetLease context submission = do
  retired <- mvarOccupied (relayLeaseRetired targetLease)
  let staleRetiredGeneration = retired && submission == StalePhysicalGeneration
  -- A generation is retired before either physical side closes, so a callback
  -- crossing the two close operations can be classified without consulting a
  -- mutable endpoint reference.  Staleness of a live relay remains a failure.
  unless (submission == Queued || staleRetiredGeneration)
    $ append (pairForwardingFailures pair) (context, submission)

data ApplicationLane = ApplicationLane
  { applicationLaneReference :: ConnectionRef ApplicationPlane,
    applicationLaneOutput :: Chan ApplicationOutbound,
    applicationLaneDropped :: Chan ApplicationOutbound,
    applicationLaneArmLoss :: MVar (),
    applicationLaneReleaseLoss :: MVar (),
    applicationLaneClosed :: MVar ()
  }

startDispositionPausedCall :: HeraldRuntime -> ActiveClient -> IO ApplicationLane
startDispositionPausedCall runtime active = do
  lane <- newApplicationLane runtime
  attachment <-
    checkedIO
      "queued-unapplied attachment"
      (Protocol.applicationAttachmentClaim (bootstrapManifestIdBytes pairRemoteBootstrapId))
  let open = Protocol.OpenSession attachment (Protocol.applicationClientNonce 704)
  assertEqual "the extra application Open is queued before drain" Queued
    =<< submitApplicationDto runtime (applicationLaneReference lane) open
  assertEqual "the same source retains an unapplied Call behind its disposition" Queued
    =<< submitApplicationDto
      runtime
      (applicationLaneReference lane)
      ( Protocol.Call
          (clientSession active)
          (Protocol.applicationRequestIdClaim 9001)
          (ReadApplication queuedDrainQuery)
      )
  pure lane

queuedDrainQuery :: ApplicationQuery
queuedDrainQuery =
  ApplicationQuery
    { applicationQueryDeltas = Set.empty,
      applicationQueryPredicate = QueryAlways
    }

newApplicationLane :: HeraldRuntime -> IO ApplicationLane
newApplicationLane runtime = do
  output <- newChan
  dropped <- newChan
  armLoss <- newEmptyMVar
  releaseLoss <- newEmptyMVar
  closed <- newEmptyMVar
  reference <-
    registerApplication
      runtime
      ( runtimeApplicationConnectionHandlers
          ( \outbound -> case outbound of
              SendEstablishedApplication {} -> do
                lose <- tryTakeMVar armLoss
                case lose of
                  Just () -> do
                    writeChan dropped outbound
                    takeMVar releaseLoss
                    pure LaneLost
                  Nothing -> writeChan output outbound >> pure LaneOffered
              _ -> writeChan output outbound >> pure LaneOffered
          )
          (putOnce closed ())
      )
  pure (ApplicationLane reference output dropped armLoss releaseLoss closed)

data ActiveClient = ActiveClient
  { clientOwner :: Client.ApplicationClientState,
    clientLane :: ApplicationLane,
    clientAccess :: Access.ApplicationStartupAccess,
    clientSession :: Protocol.ApplicationSessionClaim
  }

openPureClient :: IORef [ClientTraceStep] -> HeraldRuntime -> BootstrapManifestId -> Word64 -> IO ActiveClient
openPureClient clientTrace runtime bootstrap nonce = do
  attachment <-
    checkedIO
      "application attachment claim"
      (Protocol.applicationAttachmentClaim (bootstrapManifestIdBytes bootstrap))
  let initial =
        Client.initialApplicationClient
          fixtureApplicationClientRecoveryConfiguration
          attachment
          (Protocol.applicationClientNonce nonce)
  (started, startEffects) <- advanceClient clientTrace "start application client" Client.Start initial
  openingAttempt <- requestedTransportAttempt "Start" startEffects
  lane <- newApplicationLane runtime
  (awaitingOpen, transportEffects) <-
    advanceClient
      clientTrace
      "make application transport available"
      (Client.TransportAvailable openingAttempt (clientInstant 100))
      started
  openDto <- soleSentDto "OpenSession" transportEffects
  assertEqual "OpenSession enters the runtime lane" Queued
    =<< submitApplicationDto runtime (applicationLaneReference lane) openDto
  opened <- awaitChan "SessionOpened" (applicationLaneOutput lane)
  serverDto <- case opened of
    AcceptApplicationCandidate _ _ dto@Protocol.SessionOpened {} -> pure dto
    other -> assertFailure ("expected SessionOpened acceptance, got " <> show other)
  (active, openingEffects) <-
    advanceClient
      clientTrace
      "deliver SessionOpened"
      (Client.ServerDtoReceived openingAttempt (clientInstant 101) serverDto)
      awaitingOpen
  access <- case openingEffects of
    [Client.SessionEstablishedOnTransport actualAttempt, Client.SessionReady startup]
      | actualAttempt == openingAttempt -> pure startup
    other -> assertFailure ("expected SessionReady, got " <> show other)
  session <- case serverDto of
    Protocol.SessionOpened _ current _ _ -> pure current
    _ -> assertFailure "the accepted Open omitted its session claim"
  pure (ActiveClient active lane access session)

publishWithLostReply ::
  IORef [Word64] ->
  RecordedHeraldRuntime ->
  AttemptGate ->
  IORef [ClientTraceStep] ->
  HeraldRuntime ->
  ActiveClient ->
  IO (SortId, Client.ApplicationUnavailableReason)
publishWithLostReply observedDelays recorded attemptGate clientTrace runtime active = do
  sortAccess <- sortDefinitionAccess (clientAccess active)
  let invocation = Client.applicationInvocationId 1
      definition = DeclaredSortDefinition declaredDescriptor Nothing
  (pending, publishEffects) <-
    advanceClient
      clientTrace
      "submit sort definition"
      ( Client.submitWrite
          invocation
          (Access.predefinedWriter sortAccess)
          (PublishValue (SortDefinitionValue definition))
      )
      (clientOwner active)
  publishDto <- soleSentDto "publish sort definition" publishEffects
  putMVar (applicationLaneArmLoss (clientLane active)) ()
  assertEqual "the publish DTO enters H1" Queued
    =<< submitApplicationDto
      runtime
      (applicationLaneReference (clientLane active))
      publishDto
  dropped <- awaitChan "lost publish reply" (applicationLaneDropped (clientLane active))
  droppedDto <- case dropped of
    SendEstablishedApplication _ dto -> pure dto
    other -> assertFailure ("expected dropped established reply, got " <> show other)
  observeRecordedDelay observedDelays "the first peer ticket reaches the injected delay" recorded
  releaseRecordedDelay recorded
  _ <- awaitReadMVar "the first physical peer attempt follows the released delay" (attemptFirstEntered attemptGate)
  putOnce (applicationLaneReleaseLoss (clientLane active)) ()
  _ <- awaitMVar "lost application transport close" (applicationLaneClosed (clientLane active))
  assertEqual "LaneLost already invalidated the old application lease" StalePhysicalGeneration
    =<< closeRuntimeConnection runtime (applicationLaneReference (clientLane active))

  attachedAttempt <- currentTransportAttempt "lost publish reply" pending
  (terminated, lostEffects) <-
    advanceClient
      clientTrace
      "observe publish transport loss"
      (Client.TransportLost attachedAttempt (clientInstant 102))
      pending
  assertEqual
    "the lost application receives terminal uncertainty without a redial"
    [ Client.CallUnknown invocation Client.SessionNoLongerLive,
      Client.HeraldPermanentlyUnavailableObserved Client.SessionNoLongerLive,
      Client.CloseTransport
    ]
    lostEffects
  assertEqual "lost session owns no replacement transport" Nothing (Client.applicationClientCurrentTransportAttempt terminated)
  (afterLateReply, lateEffects) <-
    advanceClient
      clientTrace
      "late reply cannot revive the lost session"
      (Client.ServerDtoReceived attachedAttempt (clientInstant 103) droppedDto)
      terminated
  assertBool "late old-session reply is state-exact" (terminated == afterLateReply)
  assertEqual "late old-session reply cannot complete the application call" [] lateEffects
  committedSort <- assertSuccessfulServerWrite "captured but undelivered publish reply" droppedDto
  pure (committedSort, Client.SessionNoLongerLive)

beginDrainAttempt ::
  IORef [Word64] ->
  IORef [ClientTraceStep] ->
  HeraldRuntime ->
  ActiveClient ->
  RecordedHeraldRuntime ->
  AttemptGate ->
  IO PeerLogicalAttempt
beginDrainAttempt observedDelays clientTrace runtime active recorded gate = do
  sortAccess <- sortDefinitionAccess (clientAccess active)
  let invocation = Client.applicationInvocationId 3
      definition = DeclaredSortDefinition drainDescriptor Nothing
  (_, effects) <-
    advanceClient
      clientTrace
      "submit drain-race sort definition"
      (Client.submitWrite invocation (Access.predefinedWriter sortAccess) (PublishValue (SortDefinitionValue definition)))
      (clientOwner active)
  dto <- soleSentDto "drain-race publication" effects
  assertEqual "the drain-race publication enters H1" Queued
    =<< submitApplicationDto runtime (applicationLaneReference (clientLane active)) dto
  observeRecordedDelay observedDelays "the drain-race ticket reaches the exact delay boundary" recorded
  armed <- tryPutMVar (attemptDrainArmed gate) ()
  unless armed (assertFailure "the exact drain-attempt hold was already armed")
  releaseRecordedDelay recorded
  awaitMVar "handed peer outcome before drain" (attemptDrainEntered gate)

beginPostCutSuppressedPublication ::
  IORef [ClientTraceStep] ->
  HeraldRuntime ->
  ActiveClient ->
  RecordedHeraldRuntime ->
  Word64 ->
  IO (ActiveClient, Maybe PeerDispatchTicket)
beginPostCutSuppressedPublication clientTrace runtime active recorded expectedDelay = do
  sortAccess <- sortDefinitionAccess (clientAccess active)
  let invocation = Client.applicationInvocationId 5
  (pending, effects) <-
    advanceClient
      clientTrace
      "submit opposite-direction pre-cut publication"
      ( Client.submitWrite
          invocation
          (Access.predefinedWriter sortAccess)
          (PublishValue (SortDefinitionValue postCutSuppressionDefinition))
      )
      (clientOwner active)
  dto <- soleSentDto "opposite-direction pre-cut publication" effects
  assertEqual "the opposite-direction publication enters H2" Queued
    =<< submitApplicationDto runtime (applicationLaneReference (clientLane active)) dto
  ticket <- awaitScheduledDefinitionTicket recorded postCutSuppressionDefinition
  actualDelay <-
    awaitRecordedEvidence
      "opposite-direction pre-cut ticket delay"
      (awaitMatchingRecordedDelay recorded ticket)
  assertEqual "the held opposite-direction ticket retains its configured host delay" expectedDelay actualDelay
  (afterAcknowledgements, serverDto) <- awaitEstablishedReply clientTrace "opposite-direction publication reply" (clientLane active) pending
  (successor, resultEffects) <-
    advanceClient
      clientTrace
      "deliver opposite-direction publication reply"
      (currentServerDto (clientInstant 200) serverDto afterAcknowledgements)
      afterAcknowledgements
  case withoutMaintenance resultEffects of
    [Client.CallFinished actual (Client.ApplicationCallSucceeded (WriteCompleted (SortDefinitionWritten _)))]
      | actual == invocation -> pure (active {clientOwner = successor}, Just ticket)
    other -> assertFailure ("expected completed opposite-direction publication, got " <> show other)

awaitScheduledDefinitionTicket :: RecordedHeraldRuntime -> ApplicationSortDefinition -> IO PeerDispatchTicket
awaitScheduledDefinitionTicket recorded definition =
  awaitRecordedEvidence "scheduled definition ticket" go
  where
    go = do
      tickets <- scheduledTicketsForDefinition definition <$> snapshotRecordedRuntimeEvents recorded
      case reverse tickets of
        ticket : _ -> pure ticket
        [] -> yield >> go

awaitMatchingRecordedDelay :: RecordedHeraldRuntime -> PeerDispatchTicket -> IO Word64
awaitMatchingRecordedDelay recorded expected = do
  (ticket, delay) <- awaitRecordedDelay recorded
  if ticket == expected
    then pure delay
    else releaseRecordedDelay recorded >> awaitMatchingRecordedDelay recorded expected

readRemoteDefinition :: IORef [ClientTraceStep] -> Word64 -> HeraldRuntime -> ActiveClient -> IO (ActiveClient, [ApplicationValue])
readRemoteDefinition clientTrace invocationNumber runtime active = do
  access <- sortDefinitionAccess (clientAccess active)
  let invocation = Client.applicationInvocationId invocationNumber
      query =
        ApplicationQuery
          { applicationQueryDeltas = Set.singleton (Access.predefinedReader access),
            applicationQueryPredicate = QueryAlways
          }
  (pending, readEffects) <-
    advanceClient clientTrace "submit H2 read" (Client.submitRead invocation query) (clientOwner active)
  readDto <- soleSentDto "H2 read" readEffects
  assertEqual "the read DTO enters H2" Queued
    =<< submitApplicationDto runtime (applicationLaneReference (clientLane active)) readDto
  (afterAcknowledgements, serverDto) <- awaitEstablishedReply clientTrace "H2 retained read reply" (clientLane active) pending
  (successor, resultEffects) <-
    advanceClient
      clientTrace
      "deliver H2 read reply"
      (currentServerDto (clientInstant 200) serverDto afterAcknowledgements)
      afterAcknowledgements
  values <- case withoutMaintenance resultEffects of
    [ Client.CallFinished
        actual
        (Client.ApplicationCallSucceeded (ReadCompleted observed))
      ]
        | actual == invocation -> pure observed
    other -> assertFailure ("expected completed H2 read, got " <> show other)
  pure (active {clientOwner = successor}, values)

-- Acknowledgements are emitted before the attached semantic request's reply.
-- Apply them to the same pure owner without mistaking them for completion.
awaitEstablishedReply :: IORef [ClientTraceStep] -> String -> ApplicationLane -> Client.ApplicationClientState -> IO (Client.ApplicationClientState, Protocol.ApplicationServerDto)
awaitEstablishedReply trace context lane owner = do
  outbound <- awaitChan context (applicationLaneOutput lane)
  case outbound of
    SendEstablishedApplication _ dto@Protocol.ReceiptsRetired {} -> do
      (acknowledged, effects) <- advanceClient trace (context <> " retirement acknowledgement") (currentServerDto (clientInstant 200) dto owner) owner
      assertEqual "receipt acknowledgement does not complete or dispatch a call" [] effects
      awaitEstablishedReply trace context lane acknowledged
    SendEstablishedApplication _ dto -> pure (owner, dto)
    other -> assertFailure (context <> ": expected established reply, got " <> show other)

-- The deterministic client fixture records its timer intention; flowing calls
-- carry the progress and owner shutdown ends the remaining receipt lifetime.
-- Dedicated client/API tests interpret the idle timer itself.
withoutMaintenance :: [Client.ApplicationClientEffect] -> [Client.ApplicationClientEffect]
withoutMaintenance = filter $ \case
  Client.ScheduleReceiptRetirement {} -> False
  _ -> True

assertSuccessfulServerWrite :: String -> Protocol.ApplicationServerDto -> IO SortId
assertSuccessfulServerWrite _ (Protocol.RequestRetained _ _ _ (Protocol.Completed (WriteCompleted (SortDefinitionWritten sortId)))) = pure sortId
assertSuccessfulServerWrite context other =
  assertFailure (context <> ": expected retained sort-definition write, got " <> show other)

sortDefinitionAccess :: Access.ApplicationStartupAccess -> IO Access.PredefinedAccess
sortDefinitionAccess startup =
  case (Map.lookup (Access.environmentWriterKey Access.SortDefinitionRole) (Access.accessEntries (Access.startupAccessPrimordial startup)), Map.lookup (Access.environmentReaderKey Access.SortDefinitionRole) (Access.accessEntries (Access.startupAccessPrimordial startup))) of
    (Just (Access.Writer writer), Just (Access.Reader reader)) -> pure (Access.predefinedAccess Access.SortDefinitionRole writer reader)
    _ -> assertFailure "SessionOpened omitted SortDefinition startup access"

data BlockedClient = BlockedClient
  { blockedClientRelease :: MVar (),
    blockedClientClosed :: MVar ()
  }

startBlockedClient :: HeraldRuntime -> BootstrapManifestId -> Word64 -> IO BlockedClient
startBlockedClient runtime bootstrap nonce = do
  blockedTrace <- newIORef []
  attachment <-
    checkedIO
      "blocked-client attachment"
      (Protocol.applicationAttachmentClaim (bootstrapManifestIdBytes bootstrap))
  let initial =
        Client.initialApplicationClient
          fixtureApplicationClientRecoveryConfiguration
          attachment
          (Protocol.applicationClientNonce nonce)
  (started, startEffects) <- advanceClient blockedTrace "start blocked client" Client.Start initial
  openingAttempt <- requestedTransportAttempt "blocked client Start" startEffects
  entered <- newEmptyMVar
  release <- newEmptyMVar
  closed <- newEmptyMVar
  reference <-
    registerApplication
      runtime
      ( runtimeApplicationConnectionHandlers
          (\_ -> putOnce entered () >> takeMVar release >> pure LaneOffered)
          (putOnce closed ())
      )
  (_, transportEffects) <-
    advanceClient
      blockedTrace
      "make blocked transport available"
      (Client.TransportAvailable openingAttempt (clientInstant 100))
      started
  openDto <- soleSentDto "blocked OpenSession" transportEffects
  assertEqual "the unrelated OpenSession enters H2" Queued
    =<< submitApplicationDto runtime reference openDto
  _ <- awaitMVar "unrelated application writer blocks" entered
  pure (BlockedClient release closed)

advanceClient ::
  IORef [ClientTraceStep] ->
  String ->
  Client.ApplicationClientInput ->
  Client.ApplicationClientState ->
  IO (Client.ApplicationClientState, [Client.ApplicationClientEffect])
advanceClient clientTrace context input predecessor =
  case Client.stepApplicationClient input predecessor of
    Left fault -> assertFailure (context <> ": client invariant fault: " <> show fault)
    Right (Client.ApplicationClientCommandRejected rejection) ->
      assertFailure (context <> ": client command rejected: " <> show rejection)
    Right (Client.ApplicationClientAdvanced successor batch) -> do
      let effects = Client.applicationClientEffects batch
      append clientTrace (ClientTraceStep input effects)
      pure (successor, effects)

soleSentDto :: String -> [Client.ApplicationClientEffect] -> IO Protocol.ApplicationClientDto
soleSentDto context effects =
  -- advanceClient retains the complete batch in the causal trace. This manual
  -- fixture records the idle timer while its ordinary DTO carries progress.
  case withoutMaintenance effects of
    [Client.SendApplicationDto dto] -> pure dto
    _ -> assertFailure (context <> ": expected one DTO plus optional receipt scheduling, got " <> show effects)

fixtureApplicationClientRecoveryConfiguration :: Client.ApplicationClientRecoveryConfiguration
fixtureApplicationClientRecoveryConfiguration =
  case Client.applicationClientRecoveryConfiguration 10 1_000_000 of
    Right configuration -> configuration
    Left problem -> error ("invalid fixture application recovery configuration: " <> show problem)

clientInstant :: Word64 -> Client.ApplicationClientMonotonicInstant
clientInstant = Client.applicationClientMonotonicInstant

currentTransportAttempt ::
  String ->
  Client.ApplicationClientState ->
  IO Client.ApplicationTransportAttemptGeneration
currentTransportAttempt context state =
  case Client.applicationClientCurrentTransportAttempt state of
    Just attempt -> pure attempt
    Nothing -> assertFailure (context <> ": client has no current transport attempt")

currentServerDto ::
  Client.ApplicationClientMonotonicInstant ->
  Protocol.ApplicationServerDto ->
  Client.ApplicationClientState ->
  Client.ApplicationClientInput
currentServerDto observedAt dto state =
  case Client.applicationClientCurrentTransportAttempt state of
    Just attempt -> Client.ServerDtoReceived attempt observedAt dto
    Nothing -> error "active fixture client has no current transport attempt"

requestedTransportAttempt ::
  String ->
  [Client.ApplicationClientEffect] ->
  IO Client.ApplicationTransportAttemptGeneration
requestedTransportAttempt context effects =
  case [attempt | Client.RequestTransport attempt <- effects] of
    [attempt] -> pure attempt
    _ -> assertFailure (context <> ": expected one transport request, got " <> show effects)

declaredDescriptor :: ApplicationSortDescriptor
declaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections = [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

drainDescriptor :: ApplicationSortDescriptor
drainDescriptor =
  declaredDescriptor
    { minimumRetentionMicros = 1
    }

postCutSuppressionDefinition :: ApplicationSortDefinition
postCutSuppressionDefinition =
  DeclaredSortDefinition
    ( declaredDescriptor
        { minimumRetentionMicros = 2
        }
    )
    Nothing

registerApplication ::
  HeraldRuntime ->
  RuntimeApplicationConnectionHandlers ->
  IO (ConnectionRef ApplicationPlane)
registerApplication runtime handlers = do
  registration <- registerApplicationConnection runtime handlers
  case registration of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "running runtime rejected application registration"

registerPeer ::
  HeraldRuntime ->
  RuntimePeerConnectionHandlers ->
  IO (ConnectionRef PeerPlane)
registerPeer runtime handlers = do
  registration <- registerPeerConnection runtime handlers
  case registration of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "running runtime rejected peer registration"

registerAdministration :: HeraldRuntime -> IO (ConnectionRef AdministrationPlane)
registerAdministration runtime = do
  registration <-
    registerAdministrationConnection
      runtime
      (runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
  case registration of
    AdministrationConnectionRegistered reference -> pure reference
    AdministrationRegistrationUnavailable -> assertFailure "first administration registration was unavailable"
    AdministrationRegistrationStopped -> assertFailure "running runtime rejected administration registration"

withRecordedRuntimePair ::
  RecordingSchedule ->
  (RecordedHeraldRuntime -> BatchPump -> RecordedHeraldRuntime -> BatchPump -> IO value) ->
  IO value
withRecordedRuntimePair schedule use =
  withRecordedRuntime
    schedule
    (runtimeConfiguration pairLocalGenesis pairBidirectionalLocalBootstraps pairLocalGeneratorSeedSource pairDelayMicros)
    ( \localRuntime ->
        withRecordedRuntime
          schedule
          (runtimeConfiguration pairRemoteGenesis pairBidirectionalRemoteBootstraps pairRemoteGeneratorSeedSource 0)
          ( \remoteRuntime -> do
              localPump <- newBatchPump localRuntime
              remotePump <- newBatchPump remoteRuntime
              use localRuntime localPump remoteRuntime remotePump
                `finally` (closeBatchPump localPump >> closeBatchPump remotePump)
          )
    )

withRecordedRuntime ::
  RecordingSchedule ->
  HeraldRuntimeConfiguration ->
  (RecordedHeraldRuntime -> IO value) ->
  IO value
withRecordedRuntime schedule configuration use = do
  started <- startRecordedHeraldRuntime schedule configuration
  case started of
    Left failure -> assertFailure ("runtime initialization failed: " <> show failure)
    Right recorded -> use recorded `finally` closeRecordedRuntime recorded

data BatchPump = BatchPump RecordedHeraldRuntime ThreadId (MVar (Either SomeException ()))

newBatchPump :: RecordedHeraldRuntime -> IO BatchPump
newBatchPump recorded = do
  done <- newEmptyMVar
  thread <- forkFinally (foreverIO (advanceRecordedBatch recorded)) (putMVar done)
  pure (BatchPump recorded thread done)

pauseBatchPump :: BatchPump -> IO ()
pauseBatchPump (BatchPump recorded _ _) = pauseRecordedBatchPump recorded

resumeBatchPump :: BatchPump -> IO ()
resumeBatchPump (BatchPump recorded _ _) = resumeRecordedBatchPump recorded

closeBatchPump :: BatchPump -> IO ()
closeBatchPump (BatchPump _ thread done) = do
  -- Stop the observer before joining it.  Waiting for its active flag first
  -- can deadlock after a scenario failure has already stopped the dispatcher
  -- whose batch-completion hook the observer owns.
  killThread thread
  void (takeMVar done)

closeRecordedRuntime :: RecordedHeraldRuntime -> IO ()
closeRecordedRuntime recorded = RuntimeInternal.runtimeCloseScope (recordedHeraldRuntime recorded)

foreverIO :: IO value -> IO ()
foreverIO action = action >> foreverIO action

runtimeConfiguration ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  Word64 ->
  HeraldRuntimeConfiguration
runtimeConfiguration genesis bootstraps seedSource delay =
  heraldRuntimeConfiguration
    genesis
    bootstraps
    fixtureOracleContacts
    seedSource
    (runtimeMonotonicClock (pure (monotonicInstant 100)))
    (runtimePeerWorkDelayMicroseconds delay)
    (heraldRuntimeHandlers (const (pure ())))
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration

append :: IORef [value] -> value -> IO ()
append values value = atomicModifyIORef' values (\retained -> (retained <> [value], ()))

putOnce :: MVar value -> value -> IO ()
putOnce cell value = void (tryPutMVar cell value)

replaceMVar :: MVar value -> value -> IO ()
replaceMVar cell value = do
  clearMVar cell
  putMVar cell value

clearMVar :: MVar value -> IO ()
clearMVar cell = void (tryTakeMVar cell)

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

awaitMVar :: String -> MVar value -> IO value
awaitMVar context cell = do
  result <- timeout 3000000 (takeMVar cell)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

assertPreDrainAttemptEvidence ::
  RecordingSchedule ->
  PeerLogicalAttempt ->
  PeerLogicalAttempt ->
  AttemptGate ->
  IO ()
assertPreDrainAttemptEvidence recordingSchedule first replacement gate = do
  attempts <- readIORef (attemptObserved gate)
  assertBool
    "pre-drain attempts retain the first/replacement prefix and same immutable item"
    (valid attempts)
  where
    valid observed =
      case (recordingSchedule, observed) of
        (DeterministicallyGatedRecording, [(1, firstObserved), (2, replacementObserved)]) ->
          firstObserved == first && replacementObserved == replacement
        (ProductionShapedRecording, (1, firstObserved) : (2, replacementObserved) : remaining) ->
          firstObserved == first
            && replacementObserved == replacement
            && allUniqueAttempts (fmap snd observed)
            && all
              (sameItem first . snd)
              remaining
        _ -> False

    sameItem expected observed =
      peerDispatchAttemptItem observed == peerDispatchAttemptItem expected

awaitWriterTermination :: String -> AttemptGate -> MVar () -> IO ()
awaitWriterTermination context gate cell = do
  result <- timeout 30000000 (readMVar cell)
  case result of
    Just () -> pure ()
    Nothing -> do
      attempts <- readIORef (attemptObserved gate)
      drainArmed <- mvarOccupied (attemptDrainArmed gate)
      drainEntered <- mvarOccupied (attemptDrainEntered gate)
      assertFailure
        ( context
            <> " did not terminate before the outer watchdog; attempts="
            <> show attempts
            <> ", drainArmed="
            <> show drainArmed
            <> ", drainEntered="
            <> show drainEntered
        )

awaitRecordedEvidence :: String -> IO value -> IO value
awaitRecordedEvidence context action = do
  result <- timeout 3000000 action
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " recorder evidence timed out")

awaitRecordedPeerSuppression :: RecordedHeraldRuntime -> PeerLogicalAttempt -> PeerDispatchOutcome -> IO ()
awaitRecordedPeerSuppression recorded expectedAttempt expectedOutcome = go
  where
    go = do
      events <- snapshotRecordedRuntimeEvents recorded
      if any isExpectedSuppression events
        then pure ()
        else yield >> go
    isExpectedSuppression = \case
      ShellEvent _ (ShellPeerOutcomeSuppressed _ attempt outcome) ->
        attempt == expectedAttempt && outcome == expectedOutcome
      _ -> False

awaitTicketSuppressions :: String -> RecordedHeraldRuntime -> PeerDispatchTicket -> Int -> IO ()
awaitTicketSuppressions context recorded expected expectedCount =
  awaitRecordedEvidence context go
  where
    go = do
      events <- snapshotRecordedRuntimeEvents recorded
      let count =
            length
              [ ()
              | ShellEvent _ (ShellDelayedWorkSuppressed ticket) <- events,
                ticket == expected
              ]
      if count >= expectedCount then pure () else yield >> go

awaitTicketRelease :: String -> RecordedHeraldRuntime -> PeerDispatchTicket -> IO ()
awaitTicketRelease context recorded expected =
  awaitRecordedEvidence context go
  where
    go = do
      events <- snapshotRecordedRuntimeEvents recorded
      if any released events then pure () else yield >> go
    released (ShellEvent _ (ShellDelayedWorkReleased ticket)) = ticket == expected
    released _ = False

awaitReadMVar :: String -> MVar value -> IO value
awaitReadMVar context cell = do
  result <- timeout 3000000 (readMVar cell)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitChan :: String -> Chan value -> IO value
awaitChan context channel = do
  result <- timeout 3000000 (readChan channel)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitRuntimeExit ::
  String ->
  HeraldRuntime ->
  IO (Either RuntimeInternal.HeraldRuntimeFailure HeraldRuntimeExit)
awaitRuntimeExit context runtime = do
  result <- timeout 3000000 (awaitHeraldRuntimeExit runtime)
  case result of
    Just observed -> pure observed
    Nothing -> assertFailure (context <> " timed out")
