{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module PeerDeliveryCoordinatorProperties (tests) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment qualified as Alignment
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership (heraldMembershipGenerationActiveMemberSetDigest, heraldMembershipGenerationId)
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
    emptyEffectBatch,
    singletonEffectBatch,
  )
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( HeraldInputBody (RuntimeObserved),
    PeerControl (..),
    RuntimeObservation (TimerObserved),
    heraldInput,
  )
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDelivery qualified as Peers
import Eclips.Herald.PeerDelivery.State qualified as Delivery
import Eclips.Herald.PeerPayload (peerLogicalDisappearanceProbeMarkerItem)
import Eclips.Herald.PeerStream qualified as Stream
import Eclips.Herald.PeerStream.State qualified as StreamState
import Eclips.Herald.Startup.State
  ( HeraldPhase (HeraldServing),
    HeraldState,
    heraldPhase,
    replaceStartupDiscoveryState,
    replaceStartupPeerStreamState,
    startupDiscoveryState,
    startupIsolationState,
    startupOracleProjectionState,
    startupPeerDeliveryState,
    startupPeerStreamState,
  )
import Eclips.Herald.Time (MonotonicInstant, monotonicInstant)
import Eclips.Herald.Timer.Internal
  ( TimerAttempt,
    TimerOutcome (TimerFired),
    TimerSpec,
    timerSpecAbsoluteDeadline,
  )
import Eclips.Herald.Transition (stepHerald)
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.PeerDelivery qualified as Coordinator
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureCheckedInitialBootstraps,
    fixtureGeneratorSeed,
    fixtureHeraldMembershipGeneration,
    fixtureIdentifierBytes,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteMember,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "peer delivery coordinator"
    [ testCase "received evidence arms one timer and control piggyback cancels it" casePiggyback,
      testCase "idle receipt flush sends once and does not create an acknowledgement loop" caseIdleFlush,
      testCase "an early timer reoffers its deadline and stale callbacks are inert" caseEarlyTimer,
      testCase "self-fencing cancels a pending idle receipt flush" caseIsolationCancelsFlush,
      testCase "retiring a peer reclaims queued delivery and its idle receipt timer" caseRetirementReclaimsDelivery,
      testCase "reconnect replays outstanding evidence but never recreates confirmed deliveries" caseReconnect,
      testCase "evidence created while disconnected is retained and replayed on reconnect" caseDisconnectedEvidence,
      testCase "invalid peer receipt leaves the offered payload outstanding" caseInvalidProgress,
      testCase "live deferred and failed publications reoffer receipt progress once" caseDeferredPublicationReceipt
    ]

casePiggyback :: Assertion
casePiggyback = do
  (initial, binding) <- boundFixture
  let received = Coordinator.recordEvidence binding Alignment.firstAlignmentDeliverySequence initial
  (armed, effects) <- checked (Coordinator.normalizeEffects received emptyEffectBatch)
  (attempt, specification) <- oneTimer effects
  (again, repeated) <- checked (Coordinator.normalizeEffects armed emptyEffectBatch)
  assertBool "already armed state is unchanged" (armed == again)
  assertEqual "rechecking dirty progress does not arm again" [] (effectBatchMembers repeated)
  (sent, emitted) <- checked (Coordinator.normalizeEffects again (singletonEffectBatch (SendPeerControl binding (PeerKnownHeralds []))))
  assertEqual
    "next ordinary control carries the receipt and cancels idle work"
    [CancelTimer attempt, SendPeerControlWithProgress binding progressOne (PeerKnownHeralds [])]
    (effectBatchMembers emitted)
  assertEqual "receipt progress remains available for later piggyback" progressOne (Delivery.receiptProgress (peer binding sent).delivery)
  assertBool "the sent header clears dirty progress" (not (peer binding sent).dirty)
  assertEqual "no flush remains armed" Nothing (peer binding sent).timer
  (stale, staleEffects) <- timerObserved attempt (timerSpecAbsoluteDeadline specification) sent
  assertBool "callback from the cancelled timer changes nothing" (stale == sent)
  assertEqual "cancelled timer does not emit progress" [] (effectBatchMembers staleEffects)

caseIdleFlush :: Assertion
caseIdleFlush = do
  (initial, binding) <- boundFixture
  let received = Coordinator.recordEvidence binding Alignment.firstAlignmentDeliverySequence initial
  (armed, effects) <- checked (Coordinator.normalizeEffects received emptyEffectBatch)
  (attempt, specification) <- oneTimer effects
  (expired, flush) <- timerObserved attempt (timerSpecAbsoluteDeadline specification) armed
  (settled, emitted) <- checked (Coordinator.normalizeEffects expired flush)
  assertEqual
    "quiet lane emits one receipt-only control"
    [SendPeerControlWithProgress binding progressOne PeerAlignmentDeliveryProgress]
    (effectBatchMembers emitted)
  (unchanged, later) <- checked (Coordinator.normalizeEffects settled emptyEffectBatch)
  assertBool "normalization is quiescent after the flush" (settled == unchanged)
  assertEqual "the receipt is not itself sequenced evidence" [] (effectBatchMembers later)
  (stale, staleEffects) <- timerObserved attempt (timerSpecAbsoluteDeadline specification) settled
  assertBool "duplicate expiration does not dirty the receipt" (settled == stale)
  assertEqual "duplicate expiration emits nothing" [] (effectBatchMembers staleEffects)
  (repair, repairEffects) <- checked (Coordinator.normalizeEffects (Coordinator.recordEvidence binding Alignment.firstAlignmentDeliverySequence settled) emptyEffectBatch)
  (repairAttempt, _) <- oneTimer repairEffects
  assertBool "duplicate evidence repairs a lost receipt with a new timer attempt" (repairAttempt /= attempt)
  (oldCallback, oldEffects) <- timerObserved attempt (timerSpecAbsoluteDeadline specification) repair
  assertBool "old callback cannot consume the newer receipt timer" (oldCallback == repair)
  assertEqual "old callback does not send a second flush" [] (effectBatchMembers oldEffects)
  (sender, _) <- checked (Coordinator.normalizeEffects initial (singletonEffectBatch (SendPeerControl binding (PeerAlignmentControl acceptance))))
  acknowledged <- checked (Coordinator.mergeProgress binding progressOne sender)
  (quiet, reply) <- checked (Coordinator.normalizeEffects acknowledged emptyEffectBatch)
  assertEqual "receipt retires the sender's delivery" [] (Delivery.outstanding (peer binding quiet).delivery)
  assertEqual "receiving a receipt does not reply or arm a receipt timer" [] (effectBatchMembers reply)

caseEarlyTimer :: Assertion
caseEarlyTimer = do
  (initial, binding) <- boundFixture
  (armed, effects) <- checked (Coordinator.normalizeEffects (Coordinator.recordEvidence binding Alignment.firstAlignmentDeliverySequence initial) emptyEffectBatch)
  (attempt, specification) <- oneTimer effects
  (early, rearmed) <- timerObserved attempt (monotonicInstant 0) armed
  assertBool "early callback preserves retained deadline" (early == armed)
  assertEqual "early callback repeats only the same timer" [ArmTimer attempt specification] (effectBatchMembers rearmed)

caseIsolationCancelsFlush :: Assertion
caseIsolationCancelsFlush = do
  (initial, binding) <- boundFixture
  (armed, effects) <- checked (Coordinator.normalizeEffects (Coordinator.recordEvidence binding Alignment.firstAlignmentDeliverySequence initial) emptyEffectBatch)
  (flushAttempt, _) <- oneTimer effects
  let witness = Isolation.stateWitness (startupIsolationState armed)
  graceAttempt <- maybe (assertFailure "fixture has no isolation grace timer") pure (Isolation.isolationWitnessCurrentTimer witness)
  deadline <- maybe (assertFailure "fixture has no isolation grace deadline") pure (Isolation.isolationWitnessCurrentDeadline witness)
  (fenced, fenceEffects) <- checked (stepHerald (heraldInput deadline (RuntimeObserved (TimerObserved graceAttempt (TimerFired deadline)))) armed)
  assertEqual "no live application lane requires an isolation drain" Isolation.IsolationTerminalView (Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState fenced)))
  assertBool "the real fence cancels the outstanding receipt timer" (CancelTimer flushAttempt `elem` effectBatchMembers fenceEffects)
  assertEqual "the delivery owner no longer retains a timer" Nothing (peer binding fenced).timer
  (quiet, later) <- checked (Coordinator.normalizeEffects fenced emptyEffectBatch)
  assertBool "fenced delivery state stays quiescent" (quiet == fenced)
  assertEqual "retained dirty evidence cannot restart a flush after fencing" [] (effectBatchMembers later)
  (stale, staleEffects) <- timerObserved flushAttempt deadline quiet
  assertBool "a late cancelled flush cannot change the fenced state" (stale == quiet)
  assertEqual "a late cancelled flush sends no peer control" [] (effectBatchMembers staleEffects)

caseReconnect :: Assertion
caseReconnect = do
  (initial, oldBinding) <- boundFixture
  (offered, _) <- checked (Coordinator.normalizeEffects initial (singletonEffectBatch (SendPeerControl oldBinding (PeerAlignmentControl acceptance))))
  let seeded = Coordinator.markSeeded oldBinding offered
      expected = [PeerAlignmentEvidenceDelivered Alignment.firstAlignmentDeliverySequence acceptance]
  disconnected <- disconnect oldBinding seeded
  (reconnected, newBinding) <- connect 102 disconnected
  assertBool "the replacement has a new binding identity" (oldBinding /= newBinding)
  assertEqual "reconnect keeps the original delivery position" expected (Coordinator.reconnectEvidence newBinding [acceptance] reconnected)
  staleReceipt <- checked (Coordinator.mergeProgress oldBinding progressOne reconnected)
  assertBool "receipt from the superseded binding cannot reclaim delivery" (staleReceipt == reconnected)
  acknowledged <- checked (Coordinator.mergeProgress newBinding progressOne reconnected)
  disconnectedAgain <- disconnect newBinding acknowledged
  (confirmed, finalBinding) <- connect 103 disconnectedAgain
  assertEqual "retained semantic catalogue does not recreate a confirmed delivery" [] (Coordinator.reconnectEvidence finalBinding [acceptance] confirmed)
  assertEqual "confirmed payload and key index are both reclaimed" (0, 0, 0) (Delivery.retainedCounts (peer finalBinding confirmed).delivery)

caseRetirementReclaimsDelivery :: Assertion
caseRetirementReclaimsDelivery = do
  (initial, binding) <- boundFixture
  let remote = Discovery.peerBindingRemoteHeraldEpoch binding
  (offered, _) <- checked (Coordinator.normalizeEffects initial (singletonEffectBatch (QueuePeerAlignmentEvidence remote acceptance)))
  (armed, effects) <- checked (Coordinator.normalizeEffects (Coordinator.recordEvidence binding Alignment.firstAlignmentDeliverySequence offered) emptyEffectBatch)
  (attempt, specification) <- oneTimer effects
  assertEqual "retirement starts with one outstanding payload" (1, 1, 0) (Delivery.retainedCounts (peer binding armed).delivery)
  (retired, emitted) <- checked (Coordinator.normalizeEffects armed (singletonEffectBatch (CancelPeerDestination remote)))
  assertEqual "retirement cancels the exact timer before dropping the destination" [CancelTimer attempt, CancelPeerDestination remote] (effectBatchMembers emitted)
  assertEqual "no payload, receipt, seeding flag or timer row survives retirement" [] (Peers.peers (startupPeerDeliveryState retired))
  (stale, staleEffects) <- timerObserved attempt (timerSpecAbsoluteDeadline specification) retired
  assertBool "late timer cannot recreate the retired peer row" (stale == retired)
  assertEqual "late timer sends no retired-epoch receipt" [] (effectBatchMembers staleEffects)

caseDisconnectedEvidence :: Assertion
caseDisconnectedEvidence = do
  (initial, oldBinding) <- boundFixture
  disconnected <- disconnect oldBinding (Coordinator.markSeeded oldBinding initial)
  let remote = Discovery.peerBindingRemoteHeraldEpoch oldBinding
  (queued, effects) <- checked (Coordinator.normalizeEffects disconnected (singletonEffectBatch (QueuePeerAlignmentEvidence remote acceptance)))
  assertEqual "no physical output or receipt timer exists while disconnected" [] (effectBatchMembers effects)
  assertEqual "disconnected generation retains one delivery and one key" (1, 1, 0) (Delivery.retainedCounts (peer oldBinding queued).delivery)
  (connected, binding) <- connect 104 queued
  let replay = Coordinator.reconnectEvidence binding [] connected
  assertEqual "reconnect discovers the retained delivery even without old catalogue roots" [PeerAlignmentEvidenceDelivered Alignment.firstAlignmentDeliverySequence acceptance] replay
  case replay of
    [control] -> do
      (sent, emitted) <- checked (Coordinator.normalizeEffects connected (singletonEffectBatch (SendPeerControl binding control)))
      assertEqual "replay emits the same sequence without an empty progress wrapper" [SendPeerControl binding control] (effectBatchMembers emitted)
      assertEqual "replay does not allocate a second logical delivery" (1, 1, 0) (Delivery.retainedCounts (peer binding sent).delivery)
    _ -> assertFailure "unexpected reconnect replay"

caseInvalidProgress :: Assertion
caseInvalidProgress = do
  (initial, binding) <- boundFixture
  empty <- checked (Coordinator.mergeProgress binding Receipt.emptyReceiptRetirement initial)
  assertBool "empty progress does not create a peer receipt row" (empty == initial)
  (offered, _) <- checked (Coordinator.normalizeEffects initial (singletonEffectBatch (SendPeerControl binding (PeerAlignmentControl acceptance))))
  case Coordinator.mergeProgress binding (Receipt.receiptRetirementPrefix (Just 2)) offered of
    Left Delivery.DeliveryAcknowledgementBeyondAllocation -> pure ()
    other -> assertFailure ("unexpected invalid receipt result: " <> either show (const "success") other)
  assertEqual
    "failed admission cannot release the offered evidence"
    [(Alignment.firstAlignmentDeliverySequence, acceptance)]
    (Delivery.outstanding (peer binding offered).delivery)

caseDeferredPublicationReceipt :: Assertion
caseDeferredPublicationReceipt = do
  (initial, binding) <- boundFixture
  let local = Genesis.checkedLocalHeraldEpoch fixtureCheckedGenesis
      remote = Discovery.peerBindingRemoteHeraldEpoch binding
      probe = mustAdmit (Disappearance.deriveDisappearanceProbeId (Identity.controlIndex 75))
      coordinate =
        Disappearance.admitDisappearanceSubjectMembershipCoordinate
          (mustAdmit (Disappearance.mkDisappearanceSubjectDigest (fixtureIdentifierBytes 76)))
          (heraldMembershipGenerationId fixtureHeraldMembershipGeneration)
          (heraldMembershipGenerationActiveMemberSetDigest fixtureHeraldMembershipGeneration)
      marker = peerLogicalDisappearanceProbeMarkerItem probe coordinate
  direction <- checked (Stream.mkStreamDirection local remote)
  generation <- checked (Stream.mkPeerDispatchBindingGeneration (Discovery.peerBindingGenerationWord64 (Discovery.peerBindingGeneration binding)))
  bound <- checked (StreamState.prepareDispatchBinding direction generation (startupPeerStreamState initial))
  enqueued <- checked (StreamState.prepareSequenceBoundEnqueue (Map.singleton remote (marker :| [marker])) (fst (StreamState.commitDispatchBinding bound)))
  ticket <- case StreamState.preparedEnqueueDispatchTickets enqueued of
    [value] -> pure value
    values -> assertFailure ("expected one dispatch ticket: " <> show values)
  selection <- checked (StreamState.prepareDispatchSelection ticket generation (fst (StreamState.commitEnqueue enqueued)))
  attempt <- maybe (assertFailure "fixture dispatch did not select an attempt") pure (StreamState.preparedDispatchSelectionAttempt selection)
  let attempting = replaceStartupPeerStreamState (fst (StreamState.commitDispatchSelection selection)) initial
      received = Coordinator.recordEvidence binding Alignment.firstAlignmentDeliverySequence attempting
  (handed, handedEffects) <- checked (Coordinator.normalizeEffects received (singletonEffectBatch (SendPeerItem binding attempt)))
  assertEqual "the publication carries the only pending receipt" [SendPeerItemWithProgress binding progressOne attempt] (effectBatchMembers handedEffects)
  assertBool "the handed header clears dirty progress" (not (peer binding handed).dirty)
  forM_ [Stream.PeerDispatchDeferred, Stream.PeerDispatchFailed] $ \outcome -> do
    (settled, replacement) <- checked (PeerControl.applyPeerDispatchObservation True attempt outcome handed)
    (repaired, effects) <- checked (Coordinator.normalizeEffects settled replacement)
    retryTicket <- case [value | SchedulePeerDispatch value <- effectBatchMembers effects] of
      [value] -> pure value
      values -> assertFailure ("expected one retry ticket: " <> show values)
    flushAttempt <- case [value | ArmTimer value _ <- effectBatchMembers effects] of
      [value] -> pure value
      values -> assertFailure ("expected one receipt flush timer: " <> show values)
    assertBool "unsent publication restores dirty receipt progress" (peer binding repaired).dirty
    (flushed, flushEffects) <- checked (Coordinator.normalizeEffects repaired (singletonEffectBatch (SendPeerControl binding PeerAlignmentDeliveryProgress)))
    assertEqual "retry receipt retains cumulative progress" [CancelTimer flushAttempt, SendPeerControlWithProgress binding progressOne PeerAlignmentDeliveryProgress] (effectBatchMembers flushEffects)
    (duplicate, duplicateEffects) <- checked (PeerControl.applyPeerDispatchObservation True attempt outcome flushed >>= uncurry Coordinator.normalizeEffects)
    assertBool "repeated physical outcome does not restart a completed flush" (duplicate == flushed)
    assertEqual "repeated outcome emits no timer or dispatch" [] (effectBatchMembers duplicateEffects)
    nextSelection <- checked (StreamState.prepareDispatchSelection retryTicket generation (startupPeerStreamState flushed))
    let nextAttempting = replaceStartupPeerStreamState (fst (StreamState.commitDispatchSelection nextSelection)) flushed
    (stale, staleEffects) <- checked (PeerControl.applyPeerDispatchObservation True attempt outcome nextAttempting >>= uncurry Coordinator.normalizeEffects)
    assertBool "superseded outcome cannot dirty the current receipt" (stale == nextAttempting)
    assertEqual "superseded outcome emits no flush" [] (effectBatchMembers staleEffects)
  (written, writtenEffects) <- checked (PeerControl.applyPeerDispatchObservation True attempt Stream.PeerDispatchWritten handed >>= uncurry Coordinator.normalizeEffects)
  assertBool "a successful write does not reoffer receipt progress" (not (peer binding written).dirty)
  assertEqual "written still schedules the second item, without a receipt timer" 1 (length (effectBatchMembers writtenEffects))
  assertBool "the remaining effect is the next dispatch" (all (\case SchedulePeerDispatch _ -> True; _ -> False) (effectBatchMembers writtenEffects))
  (emptyProgress, emptyEffects) <- checked (PeerControl.applyPeerDispatchObservation True attempt Stream.PeerDispatchDeferred attempting >>= uncurry Coordinator.normalizeEffects)
  assertEqual "deferral without received evidence does not create a receipt row" [] (Peers.peers (startupPeerDeliveryState emptyProgress))
  assertBool "deferral without receipts only schedules the retry" (all (\case SchedulePeerDispatch _ -> True; _ -> False) (effectBatchMembers emptyEffects))

boundFixture :: IO (HeraldState, Discovery.PeerBinding)
boundFixture = do
  (initial, _) <-
    checked
      ( initialHerald
          (monotonicInstant 0)
          fixtureCheckedGenesis
          fixtureCheckedInitialBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  assertEqual "fixture starts in the serving phase" HeraldServing (heraldPhase initial)
  assertBool "fixture local Herald belongs to current membership" (OracleProjection.oracleViewLocalHeraldIsCurrent (OracleProjection.oracleView (startupOracleProjectionState initial)))
  connect 101 initial

connect :: Word64 -> HeraldState -> IO (HeraldState, Discovery.PeerBinding)
connect nonceNumber initial = do
  let nonce = Discovery.connectionNonce nonceNumber
      member = fixtureRemoteMember
      candidate = Discovery.peerCandidate (Genesis.heraldMemberId member) (Genesis.heraldMemberEpoch member) nonce
      hello = Discovery.peerHello (Genesis.checkedSystemId fixtureCheckedGenesis) (Genesis.heraldMemberId member) (Genesis.heraldMemberEpoch member) nonce Set.empty (Identity.controlIndex 0) (Genesis.checkedCatalogueDigest fixtureCheckedGenesis) (Genesis.checkedInitialProjectionDigest fixtureCheckedInitialBootstraps) Nothing
  prepared <- checked (DiscoveryState.preparePeerHello candidate hello (startupDiscoveryState initial))
  case DiscoveryState.commitPeerHello prepared of
    (discovery, Discovery.PeerHelloAccepted _ binding) -> pure (replaceStartupDiscoveryState discovery initial, binding)
    (_, disposition) -> assertFailure ("fixture Hello was not accepted: " <> show disposition) >> error "unreachable"

disconnect :: Discovery.PeerBinding -> HeraldState -> IO HeraldState
disconnect binding initial = do
  prepared <- checked (DiscoveryState.prepareBindingLoss binding (startupDiscoveryState initial))
  let (discovery, wasCurrent) = DiscoveryState.commitBindingLoss prepared
  assertBool "fixture disconnect removes its current binding" wasCurrent
  pure (replaceStartupDiscoveryState discovery initial)

peer :: Discovery.PeerBinding -> HeraldState -> Peers.PeerState
peer binding = Peers.peerState (Discovery.peerBindingRemoteHeraldEpoch binding) . startupPeerDeliveryState

oneTimer :: EffectBatch -> IO (TimerAttempt, TimerSpec)
oneTimer effects = case effectBatchMembers effects of
  [ArmTimer attempt specification] -> pure (attempt, specification)
  other -> assertFailure ("expected exactly one flush timer: " <> show other) >> error "unreachable"

timerObserved :: TimerAttempt -> MonotonicInstant -> HeraldState -> IO (HeraldState, EffectBatch)
timerObserved attempt firedAt state = case Coordinator.observeTimer attempt (TimerFired firedAt) state of
  Just result -> pure result
  Nothing -> assertFailure "receipt timer was not recognized" >> error "unreachable"

progressOne :: Receipt.ReceiptRetirement
progressOne = Receipt.receiptRetirementPrefix (Just 1)

acceptance :: Protocol.AlignmentControl
acceptance =
  Protocol.AlignmentCutAcceptanceAdvertised
    ( Protocol.alignmentCutAccepted
        (mustAdmit (Alignment.mkContextClassGenerationId (fixtureIdentifierBytes 70)))
        (Genesis.checkedLocalHeraldEpoch fixtureCheckedGenesis)
        (mustAdmit (Identity.mkTopologyCutId (fixtureIdentifierBytes 71)))
        (mustAdmit (Alignment.physicalPlacementRevisionVector fixtureHeraldMembershipGeneration [(remote, Alignment.firstPlacementRevision) | remote <- Genesis.checkedActiveHeraldEpochs fixtureCheckedGenesis]))
        Alignment.EmptyHeraldPublicationPrefix
    )

checked :: (Show problem) => Either problem value -> IO value
checked = either (\problem -> assertFailure (show problem) >> error "unreachable") pure

mustAdmit :: (Show problem) => Either problem value -> value
mustAdmit = either (error . show) id
