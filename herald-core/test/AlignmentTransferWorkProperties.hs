{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module AlignmentTransferWorkProperties (tests) where

import AlignmentCoordinatorProperties qualified as CoordinatorFixture
import AlignmentRecoveryProperties qualified as RecoveryFixture
import Control.Monad (foldM, forM_)
import Data.List (find, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Domain.Alignment
import Eclips.Domain.Context (deriveContextGraph)
import Eclips.Domain.Graph (VertexId (DeltaVertex))
import Eclips.Domain.Sort.Profile qualified as SortProfile
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Domain.Topology (deriveTopologyCutId)
import Eclips.Herald.Alignment.CutQueries qualified as CutQueries
import Eclips.Herald.Alignment.Generation
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.EffectBatch
import Eclips.Herald.Input (PeerControl (PeerAlignmentControl))
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Startup.State
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
import Eclips.Herald.UseCase.AlignmentTransfer qualified as Coordinator
import GenesisFixtures (fixtureLocalMember, fixtureRemoteMember)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "selective alignment transfer work"
    [ QC.testProperty "destination progress journal matches exact transcript tuple changes" propDestinationJournal,
      testCase "loss journal consumption preserves prepared cut-query entries" caseLossHarvestPreservesCutQueries,
      testCase "blocked attempts stay dormant on unrelated inputs" caseBlockedAttempts,
      testCase "remote member evidence wakes only certificate work and agrees with full scan" caseCertificateWake,
      testCase "bootstrap fulfillment waits for Live before readiness; every ready precedes every certificate" caseReadyCertificateOrder
    ]

propDestinationJournal :: [Word8] -> QC.Property
propDestinationJournal choices = QC.ioProperty $ do
  let subscribe = RecoveryFixture.freshSubscribe
      identifier = Protocol.alignmentSubscribeSubscriptionId subscribe
      admitted = RecoveryFixture.commitDestinationBootstrap subscribe Transfer.emptyState
      (created, consumed) = Transfer.takeDestinationProgressChanges admitted
      preparedSource = checked (Transfer.prepareSourceSubscription subscribe initialStoreRevision [] Transfer.SourceLiveImmediate consumed)
      controls = Transfer.preparedSourceSubscriptionControls preparedSource
      source = fst (Transfer.commitSourceSubscription preparedSource)
      pick choice = listToMaybe (drop (fromIntegral choice `mod` length controls) controls)
      schedule = [control | choice <- take 40 choices, Just control <- [pick choice]] <> controls
  assertEqual "new destination registers its exact subscription" (Set.singleton identifier) created
  assertBool "the snapshot fixture is nonempty" (not (null controls))
  afterFrames <- foldM (observe identifier) source schedule
  let future = Protocol.AlignmentLiveAdvertised (Protocol.alignmentLive identifier (nextStoreRevision initialStoreRevision))
  futureHeld <- observe identifier afterFrames future
  duplicate <- observe identifier futureHeld future
  cancelled <- cancel identifier Protocol.AlignmentSourceIncarnationLost duplicate
  stronger <- cancel identifier Protocol.AlignmentRelationRemoved cancelled
  _ <- cancel identifier Protocol.AlignmentDestinationIncarnationLost stronger
  pure True
  where
    observe identifier previous control = do
      next <- applyDestination control previous
      let changed = facts identifier next /= facts identifier previous
          (reported, consumed) = Transfer.takeDestinationProgressChanges next
      assertEqual "only a changed applied/Live/ack/cancel tuple notifies" (if changed then Set.singleton identifier else Set.empty) reported
      assertEqual "draining has ordinary equality and consumes once" Set.empty (fst (Transfer.takeDestinationProgressChanges consumed))
      assertEqual "destination journal never invalidates transcript ownership" (Right ()) (Transfer.validateAlignmentTransferState consumed)
      pure consumed
    cancel identifier reason previous = do
      let cancellation = Protocol.alignmentCancel identifier reason
          next = fst (Transfer.commitAlignmentCancellation (checked (Transfer.prepareAlignmentCancellation cancellation previous)))
          changed = facts identifier next /= facts identifier previous
          (reported, consumed) = Transfer.takeDestinationProgressChanges next
      assertEqual "cancellation changes use the same exact tuple rule" (if changed then Set.singleton identifier else Set.empty) reported
      let replay = fst (Transfer.commitAlignmentCancellation (checked (Transfer.prepareAlignmentCancellation cancellation consumed)))
      assertEqual "exact cancellation retries are silent" Set.empty (fst (Transfer.takeDestinationProgressChanges replay))
      assertEqual "stronger cancellation preserves the independent index invariant" (Right ()) (Transfer.validateAlignmentTransferState replay)
      pure replay
    facts identifier state = do
      destination <- Transfer.lookupDestinationSubscription identifier state
      pure (Transfer.destinationSubscriptionAppliedThrough destination, Transfer.destinationSubscriptionLiveThrough destination, Transfer.destinationSubscriptionAcknowledgedThrough destination, Transfer.destinationSubscriptionCancellation destination)

caseLossHarvestPreservesCutQueries :: Assertion
caseLossHarvestPreservesCutQueries = do
  let original = CoordinatorFixture.retainedCutQueryFixture
      prepared = CoordinatorFixture.cachePreparedFor original
      expected = (CutQueries.cutQueryPositions prepared, CutQueries.cutQueryRetainedKeys prepared)
      warm = replaceStartupAlignmentCutQueryCache prepared original
      (_, _, vectors) = CutQueries.cutQueryRetainedKeys prepared
  assertBool "fixture has actual pending Placement loss notification" (fst (Placement.takeAlignmentLossChange (startupPlacementState original)))
  assertBool "fixture has retained placement suffix entries" (not (Set.null vectors))
  (successor, effects) <- checkedIO (Coordinator.reconcileAlignmentLosses warm)
  let retained = startupAlignmentCutQueryCache successor
  assertEqual "journal-only physical owner replacement preserves every cut-query entry" expected (CutQueries.cutQueryPositions retained, CutQueries.cutQueryRetainedKeys retained)
  assertEqual "no active alignment plan is cancelled" [] (effectBatchMembers effects)

caseBlockedAttempts :: Assertion
caseBlockedAttempts = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, _) <- CoordinatorFixture.readyAlignmentFixture local remote
  (initial, _) <- CoordinatorFixture.connectedWithAlignment alignment
  assertBool "fixture has genuine pending bootstrap imports" (not (null (Alignment.bootstrapImportEntries alignment)))
  (blocked, effects, work) <- differential "blocked bootstrap" Coordinator.advanceBootstrapAttempts Coordinator.advanceBootstrapAttemptsExhaustive initial
  assertEqual "missing local acceptance has no effects" [] (effectBatchMembers effects)
  assertEqual "testing a blocked attempt is not semantic progress" Coordinator.AlignmentTransferWorkUnchanged work
  assertEqual "initial bootstrap work is consumed" Set.empty (Alignment.pendingTransferWork Alignment.BootstrapAttemptStage (startupAlignmentState blocked))
  (ordinary, _, _) <- differential "ordinary attempt stage" Coordinator.advanceOrdinaryAttempts Coordinator.advanceOrdinaryAttemptsExhaustive blocked
  forM_ [1 :: Int .. 4] $ \_ -> do
    (quiet, quietEffects, quietWork) <- differential "parked bootstrap" Coordinator.advanceBootstrapAttempts Coordinator.advanceBootstrapAttemptsExhaustive ordinary
    assertBool "parked stage preserves the complete owner" (quiet == ordinary)
    assertEqual "parked stage emits nothing" [] (effectBatchMembers quietEffects)
    assertEqual "parked stage reports unchanged" Coordinator.AlignmentTransferWorkUnchanged quietWork

caseCertificateWake :: Assertion
caseCertificateWake = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, generation) <- CoordinatorFixture.readyAlignmentFixture local remote
  (initial, _) <- CoordinatorFixture.connectedWithAlignment alignment
  let generationId = alignmentGenerationId generation
      members = NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation))
      localMember = required (find ((== local) . alignmentMemberHerald) members)
      remoteMember = required (find ((== remote) . alignmentMemberHerald) members)
      key = (generationId, alignmentMemberDelta localMember)
      remoteReady = Protocol.classMemberReady (Protocol.nextAlignmentEvidenceSequence Protocol.firstAlignmentEvidenceSequence) generationId (alignmentMemberStoreIncarnation remoteMember) (deriveMemberReadyEvidenceDigest "stage4-remote-ready") initialStoreRevision
      install state =
        replaceStartupAlignmentState
          (fst (Alignment.commitClassMemberReady (checked (Alignment.prepareClassMemberReady remoteReady (startupAlignmentState state)))))
          state
  (blocked, _, _) <- differential "certificate needs remote readiness" Coordinator.advanceReadinessAndCertificates Coordinator.advanceReadinessAndCertificatesExhaustive initial
  assertEqual "certificate remains registered but parked" (Set.singleton key) (Alignment.transferWorkIds (Alignment.MemberCertificateStage local) (startupAlignmentState blocked))
  assertEqual "initial certificate check consumed its dirt" Set.empty (Alignment.pendingTransferWork (Alignment.MemberCertificateStage local) (startupAlignmentState blocked))
  let woken = install blocked
  assertEqual "remote readiness wakes exact local certificate" (Set.singleton key) (Alignment.pendingTransferWork (Alignment.MemberCertificateStage local) (startupAlignmentState woken))
  assertEqual "already ready local member is not reevaluated" Set.empty (Alignment.pendingTransferWork (Alignment.MemberReadinessStage local) (startupAlignmentState woken))
  (certified, effects, work) <- differential "remote readiness completes certificate" Coordinator.advanceReadinessAndCertificates Coordinator.advanceReadinessAndCertificatesExhaustive woken
  assertEqual "one local certificate is advertised" 1 (length [() | SendPeerControl _ (PeerAlignmentControl (Protocol.AlignmentHistoricalCertificateAdvertised _)) <- effectBatchMembers effects])
  assertEqual "certificate insertion reports progress" Coordinator.AlignmentTransferWorkChanged work
  assertEqual "completed local member leaves the combined entry inventory" Set.empty (Alignment.pendingTransferMemberIds local (startupAlignmentState certified))
  assertEqual "duplicate evidence does not recreate completed certificate work" Set.empty (Alignment.pendingTransferWork (Alignment.MemberCertificateStage local) (startupAlignmentState (install certified)))
  (quiet, quietEffects, _) <- differential "completed certificate replay" Coordinator.advanceReadinessAndCertificates Coordinator.advanceReadinessAndCertificatesExhaustive (install certified)
  assertEqual "completed evidence remains quiet" [] (effectBatchMembers quietEffects)
  assertEqual "certificate work preserves Alignment indexes" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState quiet))

caseReadyCertificateOrder :: Assertion
caseReadyCertificateOrder = do
  (initial, generations) <- twoLocalMembersFixture
  let local = case fixtureLocalMember of HeraldMember _ epoch -> epoch
      owner = startupAlignmentState initial
      attempts = Alignment.bootstrapImportAttemptEntries owner
      stageKeys = Set.fromList (map fst attempts)
      complete includeLive state (_, attempt) = do
        let subscribe = Alignment.bootstrapImportAttemptSubscribe attempt
            transfer = Alignment.alignmentTransferState (startupAlignmentState state)
            prepared = checked (Transfer.prepareSourceSubscription subscribe initialStoreRevision [] Transfer.SourceLiveImmediate transfer)
            controls = [control | control <- Transfer.preparedSourceSubscriptionControls prepared, includeLive || case control of Protocol.AlignmentLiveAdvertised _ -> False; _ -> True]
            admitted = fst (Transfer.commitSourceSubscription prepared)
        completed <- foldM (flip applyDestination) admitted controls
        pure (replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState completed (startupAlignmentState state)) state)
  assertEqual "fixture has two separately ordered bootstrap imports" 2 (length attempts)
  (withoutLive, _, _) <- differential "empty bootstrap transcripts" Coordinator.advanceBootstrapFulfillments Coordinator.advanceBootstrapFulfillmentsExhaustive initial
  snapshots <- foldM (complete False) withoutLive attempts
  (held, effects, work) <- differential "applied snapshots without Live cannot fulfill" Coordinator.advanceBootstrapFulfillments Coordinator.advanceBootstrapFulfillmentsExhaustive snapshots
  assertEqual "snapshot completion alone preserves every import" stageKeys (Set.fromList (map fst (Alignment.bootstrapImportEntries (startupAlignmentState held))))
  assertEqual "held imports emit no cancellation" [] (effectBatchMembers effects)
  assertEqual "held imports are not progress" Coordinator.AlignmentTransferWorkUnchanged work
  live <- foldM (complete True) held attempts
  (fulfilled, _, _) <- differential "equal applied/Live/ack prefixes fulfill" Coordinator.advanceBootstrapFulfillments Coordinator.advanceBootstrapFulfillmentsExhaustive live
  assertEqual "fulfilled imports leave active scheduling" [] (Alignment.bootstrapImportEntries (startupAlignmentState fulfilled))
  (ready, readyEffects, _) <- differential "all readiness before certificates" Coordinator.advanceReadinessAndCertificates Coordinator.advanceReadinessAndCertificatesExhaustive fulfilled
  let expectedMembers = sortOn (\(generation, member) -> (alignmentGenerationId generation, alignmentMemberDelta member)) [(generation, member) | generation <- generations, member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)), alignmentMemberHerald member == local]
      expectedIds = [alignmentGenerationId generation | (generation, _) <- expectedMembers]
      actual =
        [ case control of
            Protocol.AlignmentMemberReadyAdvertised evidence -> Left (Protocol.classMemberReadyGeneration evidence)
            Protocol.AlignmentHistoricalCertificateAdvertised certificate -> Right (Protocol.historicalCertificateClassGeneration certificate)
            _ -> error "unexpected readiness-stage control"
        | effect <- effectBatchMembers readyEffects,
          control <- case effect of
            QueuePeerAlignmentEvidence _ evidence -> [evidence]
            SendPeerControl _ (PeerAlignmentControl evidence) -> [evidence]
            _ -> []
        ]
  assertEqual "the frozen phase boundary preserves evidence ordering" (map Left expectedIds <> map Right expectedIds) actual
  assertEqual "every local pair has completed" Set.empty (Alignment.pendingTransferMemberIds local (startupAlignmentState ready))
  assertEqual "completed Alignment ownership is valid" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState ready))
  assertEqual "physical empty Store histories are valid" (Right ()) (Store.validateStoreHistoryState (startupStoreState ready))

-- Two local singleton classes with actual empty retained Store slots. Their
-- initial bootstrap attempts are admitted through the ordinary owner APIs;
-- tests deliver the same checked empty snapshot frames as a real source emits.
twoLocalMembersFixture :: IO (HeraldState, [AlignmentGeneration])
twoLocalMembersFixture = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (template, templateGeneration) <- CoordinatorFixture.readyAlignmentFixture local remote
  let cut = alignmentGenerationCut templateGeneration
      members = NonEmpty.toList (alignmentCutExactMembers cut)
      typedSort = sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut)
      inputMembers = [generationMemberInput (alignmentMemberDelta member) (alignmentMemberStoreIncarnation member) local initialStoreRevision | member <- members]
      context = checked (deriveContextGraph (map (DeltaVertex . alignmentMemberDelta) members) [] (map alignmentMemberDelta members))
      envelope = Alignment.bootstrapImportSubscribeEnvelope (snd (required (listToMaybe (Alignment.bootstrapImportEntries template))))
      cause = Protocol.alignmentObligationCause envelope
      debt = normalizeStructuralDebts [structuralConsequenceDebt (structuralDebtKey cause TopologyAlignmentDebt typedSort Nothing) (structuralDebtEvidence Set.empty Set.empty Set.empty)]
      withDebt = fst (Alignment.commitStructuralDebtRetention (checked (Alignment.prepareStructuralDebtRetention cause debt Alignment.emptyState)))
  plan <- case prepareAlignmentGenerationPlan typedSort (alignmentCutTopologyCut cut) (alignmentCutPhysicalPlacementRevisionVector cut) context inputMembers [] [] Set.empty of
    Right (AlignmentGenerationReady ready) -> pure ready
    value -> assertFailure ("two-local-member generation plan: " <> show value)
  let generations = alignmentGenerationPlanGenerations plan
      promoted = fst (Alignment.commitAlignmentPromotion (checked (Alignment.prepareAlignmentPromotion local cause typedSort (deriveTopologyCutId (alignmentCutTopologyCut cut)) (alignmentCutPhysicalPlacementRevisionVector cut) plan withDebt)))
      accept state generation =
        let acceptance = Protocol.alignmentCutAccepted (alignmentGenerationId generation) local (deriveTopologyCutId (alignmentCutTopologyCut (alignmentGenerationCut generation))) (alignmentCutPhysicalPlacementRevisionVector (alignmentGenerationCut generation)) EmptyHeraldPublicationPrefix
         in fst (Alignment.commitAlignmentCutAcceptance (checked (Alignment.prepareAlignmentCutAcceptance acceptance state)))
      accepted = foldl accept promoted generations
      attempted = foldl (\state key -> fst (Alignment.commitBootstrapImportAttempt (checked (Alignment.prepareBootstrapImportAttempt key state)))) accepted (map fst (Alignment.bootstrapImportEntries accepted))
  (connected, _) <- CoordinatorFixture.connectedWithAlignment attempted
  let stores =
        Store.commitStoreBootstrap
          ( checked
              ( Store.prepareStoreBootstrap
                  [Store.localStoreSpec (Store.HeraldSystemView local SortProfile.SortDefinitionRole) (alignmentMemberDelta member) (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut) (alignmentMemberStoreIncarnation member) | member <- members]
                  (startupStoreState connected)
              )
          )
  pure (replaceStartupStoreState stores connected, generations)

differential ::
  String ->
  (HeraldState -> Either Coordinator.AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, Coordinator.AlignmentTransferWorkDisposition)) ->
  (HeraldState -> Either Coordinator.AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, Coordinator.AlignmentTransferWorkDisposition)) ->
  HeraldState ->
  IO (HeraldState, EffectBatch, Coordinator.AlignmentTransferWorkDisposition)
differential label selective exhaustive predecessor = do
  result@(successor, effects, work) <- either (assertFailure . ((label <> ": ") <>) . show) pure (selective predecessor)
  (reference, referenceEffects, referenceWork) <- either (assertFailure . ((label <> " exhaustive: ") <>) . show) pure (exhaustive predecessor)
  assertBool (label <> ": full owner metadata matches independent enumeration") (successor == reference)
  assertEqual (label <> ": ordered effects match") (effectBatchMembers referenceEffects) (effectBatchMembers effects)
  assertEqual (label <> ": semantic work disposition matches") referenceWork work
  pure result

applyDestination :: Protocol.AlignmentControl -> Transfer.State -> IO Transfer.State
applyDestination control state = do
  prepared <- checkedIO $ case control of
    Protocol.AlignmentSnapshotStarted start -> Transfer.prepareDestinationSnapshotStart start state
    Protocol.AlignmentSnapshotChunkTransferred chunk -> Transfer.prepareDestinationSnapshotChunk chunk state
    Protocol.AlignmentSnapshotEnded end -> Transfer.prepareDestinationSnapshotEnd end state
    Protocol.AlignmentChangeTransferred change -> Transfer.prepareDestinationChange change state
    Protocol.AlignmentLiveAdvertised live -> Transfer.prepareDestinationLive live state
    _ -> error "non-destination fixture control"
  pure (let (successor, _) = Transfer.commitDestinationWork prepared in successor)

checked :: (Show error) => Either error value -> value
checked = either (error . show) id
checkedIO :: (Show error) => Either error value -> IO value
checkedIO = either (assertFailure . show) pure
required :: Maybe value -> value
required = maybe (error "missing selective-transfer fixture value") id
