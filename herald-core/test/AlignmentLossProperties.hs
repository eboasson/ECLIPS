{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module AlignmentLossProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Context (ContextGraph, deriveContextGraph)
import Eclips.Domain.Graph
  ( EdgePayload,
    EdgeStrength (Preserve),
    VertexId (DeltaVertex),
    edgePayload,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Route (ReplicaStrength (Normal))
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    labelReleaseCause,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationPlan,
    AlignmentGenerationPreparation (AlignmentGenerationReady),
    GenerationMemberInput,
    alignmentGenerationCut,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    generationMemberInput,
    prepareAlignmentGenerationPlan,
  )
import Eclips.Herald.Alignment.Loss
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    StructuralDebtKind (TopologyAlignmentDebt),
    normalizeStructuralDebts,
    sortOccurrence,
    structuralConsequenceDebt,
    structuralDebtEvidence,
    structuralDebtKey,
  )
import GenesisFixtures (fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    shuffle,
    testProperty,
    vectorOf,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "alignment loss"
    [ testCase "destination loss tombstones ordinary and bootstrap work without waiting" destinationLossTombstonesCompletePlan,
      testCase "remote destination loss terminalizes a surviving source transcript" remoteDestinationLossTerminalizesSource,
      testCase "inbound cancellation and remote destination loss converge in either order" remoteDestinationCancellationRaceConverges,
      testCase "remote destination loss preserves a closure barrier shared by another source" remoteDestinationLossPreservesSharedBarrier,
      testCase "import-only fresh-base destination loss tombstones the coordinate" importOnlyDestinationLoss,
      testCase "fresh-base attempted loss joins source retirement to plan tombstone" freshBaseAttemptedLoss,
      testCase "destination loss after fulfillment still tombstones the current generation" destinationLossAfterFulfillment,
      testCase "certified remote current member loss tombstones the replicated plan without subscriptions" remoteCurrentMemberLossAfterReadiness,
      testCase "superseded remote member loss preserves historical plans and reselects a certified predecessor" supersededRemoteMemberLossPreservesHistory,
      testCase "source loss retires and freshly reselects ordinary and bootstrap attempts" sourceLossReselectsBothKinds,
      testCase "membership retirement defers source reselection until successor-base advancement" membershipRetirementDefersSourceReselection,
      testCase "source exhaustion preserves unsourced ordinary and bootstrap work" sourceLossWithoutReplacement,
      testCase "one loss atomically tombstones a destination plan and retires a surviving predecessor source" bothRolesCommitAtomically,
      testCase "loss before selection excludes the dead source from later work" lossBeforeSelectionIsMonotone,
      testCase "remote placement loss recovers exact retained consequence causes" remotePlacementLossFindsExactCauses,
      testCase "remote placement loss reopens a complete passive plan without importing owner work" passivePlacementLossFindsExactCauses,
      testCase "loss-aware selectors reject an already-selected dead source" incumbentLossExclusionIsChecked,
      testCase "certificate insertion permutation does not change canonical selection" selectionOrderIsCanonical,
      testCase "a coordinator rejects a foreign destination owner" foreignDestinationOwnerIsRejected,
      testCase "a coordinator rejects foreign destination ownership retained only in history" historicalForeignDestinationOwnerIsRejected,
      testProperty "generated mixed loss schedules match the independent work model" propGeneratedMixedLossSchedules,
      testProperty "generated both-role schedules are atomic and replay exact" propGeneratedBothRoleSchedules,
      testProperty "generated distinct destination losses converge in either order" propGeneratedDistinctDestinationLossesConverge
    ]

destinationLossTombstonesCompletePlan :: Assertion
destinationLossTombstonesCompletePlan = do
  let fixture = combinedFixture False
      predecessor = coordinatorFor fixture.attemptedOwner
      oldOrdinary = Alignment.alignmentAttemptEntries fixture.attemptedOwner
      oldBootstrap = Alignment.bootstrapImportAttemptEntries fixture.attemptedOwner
      prepared = checked "destination loss" (prepareAlignmentLoss causeA localDestinationLoss predecessor)
      plan = preparedAlignmentLossPlan prepared
      successor = commitAlignmentLoss prepared
      owner = alignmentLossCoordinatorOwner successor
      retry = checked "destination loss replay" (prepareAlignmentLoss causeA localDestinationLoss successor)
  assertBool "the fixture has ordinary attempts" (not (null oldOrdinary))
  assertBool "the fixture has bootstrap attempts" (not (null oldBootstrap))
  assertBool "at least one exact plan cause is retained" (not (Set.null (alignmentLossPlanInvalidationCauses plan)))
  assertEqual "ordinary work is gone immediately" [] (Alignment.activeAlignmentObligationEntries owner)
  assertEqual "bootstrap work is gone immediately" [] (Alignment.bootstrapImportEntries owner)
  assertEqual "ordinary attempts are gone immediately" [] (Alignment.alignmentAttemptEntries owner)
  assertEqual "bootstrap attempts are gone immediately" [] (Alignment.bootstrapImportAttemptEntries owner)
  case alignmentLossPlanInvalidationEvents plan of
    [(_, event)] -> do
      assertBool
        "the tombstone retains every ordinary owner"
        (not (null (planInvalidationEventObligations event)))
      assertBool
        "the tombstone retains every bootstrap owner"
        (not (null (planInvalidationEventBootstrapImports event)))
    receipts -> error ("expected one plan receipt, got " <> show (length receipts))
  assertEqual
    "each subscription has one final dominant cancellation"
    (length (alignmentLossPlanCancellations plan))
    ( Set.size
        ( Set.fromList
            (Protocol.alignmentCancelSubscriptionId <$> alignmentLossPlanCancellations plan)
        )
    )
  assertBool "the discarded coordinate is marked for debt redrive" (not (null (alignmentLossCoordinatorRedriveRequired successor)))
  assertEqual "exact replay has no second transition" AlignmentLossUnchanged (preparedAlignmentLossDisposition retry)
  assertEqual "exact replay returns the original full result" plan (preparedAlignmentLossPlan retry)
  assertEqual "exact replay retains the current successor" successor (commitAlignmentLoss retry)
  assertEqual "composite invariant" (Right ()) (validateAlignmentLossCoordinatorState successor)

remoteDestinationLossTerminalizesSource :: Assertion
remoteDestinationLossTerminalizesSource = do
  let (subscribe, predecessorOwner) = remoteDestinationSourceOwner
      identifier = Protocol.alignmentSubscribeSubscriptionId subscribe
      lost = qualifiedStoreCoordinate remoteHerald remoteDelta remoteStore
      unrelated =
        qualifiedStoreCoordinate remoteHerald remoteDelta alternateStore
      predecessor = coordinatorFor predecessorOwner
      prepared =
        checked
          "remote destination loss"
          (prepareAlignmentLoss causeA lost predecessor)
      plan = preparedAlignmentLossPlan prepared
      successor = commitAlignmentLoss prepared
      successorOwner = alignmentLossCoordinatorOwner successor
      successorTransfer = Alignment.alignmentTransferState successorOwner
      cancellation =
        Protocol.alignmentCancel
          identifier
          Protocol.AlignmentDestinationIncarnationLost
      replay =
        checked
          "remote destination loss replay"
          (prepareAlignmentLoss causeA lost successor)
      preparedChanges =
        checked
          "terminal source cannot advance"
          ( Transfer.prepareSourceChanges
              identifier
              []
              (Protocol.alignmentLive identifier DomainAlignment.initialStoreRevision)
              successorTransfer
          )
      (afterChanges, changeDisposition) =
        Transfer.commitSourceChanges preparedChanges
  assertEqual
    "the source transcript exposes its exact structural cause"
    (Set.singleton causeA)
    (alignmentLossRelevantCauses lost predecessorOwner)
  assertEqual
    "an unrelated destination incarnation exposes no cause"
    Set.empty
    (alignmentLossRelevantCauses unrelated predecessorOwner)
  assertBool
    "the fixture starts with one incomplete closure wait"
    (not (null (Transfer.predecessorInputClosureEntries (Alignment.alignmentTransferState predecessorOwner))))
  assertBool
    "the real-shape local barrier owner is distinct from the outgoing source owner"
    ( all
        ( (/= Protocol.alignmentSubscribeObligationId subscribe)
            . Protocol.alignmentSubscribeObligationId
            . fst
            . snd
        )
        (Transfer.predecessorInputClosureEntries (Alignment.alignmentTransferState predecessorOwner))
    )
  assertEqual
    "the exact remote destination cancels the surviving source transcript"
    (Just cancellation)
    ( Transfer.sourceSubscriptionCancellation
        =<< Transfer.lookupSourceSubscription identifier successorTransfer
    )
  assertEqual
    "destination loss removes its incomplete closure wait immediately"
    []
    (Transfer.predecessorInputClosureEntries successorTransfer)
  assertEqual
    "the loss plan retains exactly the terminal destination cancellation"
    [cancellation]
    (alignmentLossPlanCancellations plan)
  assertEqual
    "terminal source retry cannot replay snapshot or live work"
    [Protocol.AlignmentCancelled cancellation]
    (Transfer.sourceRetryControls identifier successorTransfer)
  assertEqual
    "terminal source advancement is inert"
    Transfer.AlignmentTransferUnchanged
    changeDisposition
  assertEqual
    "terminal advancement preserves the transfer"
    successorTransfer
    afterChanges
  assertEqual
    "terminal advancement returns only the retained cancellation"
    [Protocol.AlignmentCancelled cancellation]
    (Transfer.preparedSourceChangeControls preparedChanges)
  assertEqual
    "exact loss replay is unchanged"
    AlignmentLossUnchanged
    (preparedAlignmentLossDisposition replay)
  assertEqual
    "exact loss replay preserves the successor"
    successor
    (commitAlignmentLoss replay)
  assertEqual
    "remote destination successor invariant"
    (Right ())
    (validateAlignmentLossCoordinatorState successor)

remoteDestinationLossPreservesSharedBarrier :: Assertion
remoteDestinationLossPreservesSharedBarrier = do
  let (lostSubscribe, survivingSubscribe, predecessorOwner) =
        sharedRemoteDestinationSourceOwner
      lostIdentifier = Protocol.alignmentSubscribeSubscriptionId lostSubscribe
      survivingIdentifier = Protocol.alignmentSubscribeSubscriptionId survivingSubscribe
      predecessorTransfer = Alignment.alignmentTransferState predecessorOwner
      prepared =
        checked
          "remote destination loss with shared barrier"
          ( prepareAlignmentLoss
              causeA
              (qualifiedStoreCoordinate remoteHerald remoteDelta remoteStore)
              (coordinatorFor predecessorOwner)
          )
      successor = commitAlignmentLoss prepared
      successorTransfer =
        Alignment.alignmentTransferState
          (alignmentLossCoordinatorOwner successor)
      survivingCancellation =
        Protocol.alignmentCancel
          survivingIdentifier
          Protocol.AlignmentRelationRemoved
      preparedSurvivingCancellation =
        checked
          "terminalize the source sharing the barrier"
          ( Transfer.prepareAlignmentCancellation
              survivingCancellation
              successorTransfer
          )
      (terminalTransfer, terminalDisposition) =
        Transfer.commitAlignmentCancellation preparedSurvivingCancellation
      replayedSurvivingCancellation =
        checked
          "replay shared source cancellation"
          ( Transfer.prepareAlignmentCancellation
              survivingCancellation
              terminalTransfer
          )
      (replayedTransfer, replayDisposition) =
        Transfer.commitAlignmentCancellation replayedSurvivingCancellation
  assertEqual
    "the fixture starts with one shared executable barrier"
    1
    (length (Transfer.predecessorInputClosureEntries predecessorTransfer))
  assertEqual
    "terminalizing one source retains the barrier required by its live peer"
    (Transfer.predecessorInputClosureEntries predecessorTransfer)
    (Transfer.predecessorInputClosureEntries successorTransfer)
  assertEqual
    "the lost-destination source is terminal"
    ( Just
        ( Protocol.alignmentCancel
            lostIdentifier
            Protocol.AlignmentDestinationIncarnationLost
        )
    )
    ( Transfer.sourceSubscriptionCancellation
        =<< Transfer.lookupSourceSubscription lostIdentifier successorTransfer
    )
  assertEqual
    "the source sharing the barrier remains live"
    (Just Nothing)
    ( Transfer.sourceSubscriptionCancellation
        <$> Transfer.lookupSourceSubscription survivingIdentifier successorTransfer
    )
  assertEqual
    "shared-barrier successor invariant"
    (Right ())
    (validateAlignmentLossCoordinatorState successor)
  assertEqual
    "terminalizing the last source removes the now-orphan barrier"
    []
    (Transfer.predecessorInputClosureEntries terminalTransfer)
  assertEqual
    "last-source cancellation records work"
    Transfer.AlignmentTransferRetained
    terminalDisposition
  assertEqual
    "last-source cancellation replay is inert"
    Transfer.AlignmentTransferUnchanged
    replayDisposition
  assertEqual
    "last-source cancellation replay preserves the terminal transfer"
    terminalTransfer
    replayedTransfer
  assertEqual
    "terminal shared-barrier transfer invariant"
    (Right ())
    (Transfer.validateAlignmentTransferState terminalTransfer)

remoteDestinationCancellationRaceConverges :: Assertion
remoteDestinationCancellationRaceConverges = do
  let (subscribe, predecessorOwner) = remoteDestinationSourceOwner
      identifier = Protocol.alignmentSubscribeSubscriptionId subscribe
      cancellation =
        Protocol.alignmentCancel
          identifier
          Protocol.AlignmentDestinationIncarnationLost
      lost = qualifiedStoreCoordinate remoteHerald remoteDelta remoteStore
      lossFirst =
        commitAlignmentLoss
          ( checked
              "destination loss before inbound cancellation"
              (prepareAlignmentLoss causeA lost (coordinatorFor predecessorOwner))
          )
      preparedInbound =
        checked
          "retain inbound destination cancellation first"
          ( Transfer.prepareAlignmentCancellation
              cancellation
              (Alignment.alignmentTransferState predecessorOwner)
          )
      (cancelledTransfer, _) =
        Transfer.commitAlignmentCancellation preparedInbound
      cancelledOwner =
        Alignment.replaceAlignmentTransferState cancelledTransfer predecessorOwner
      cancellationFirst =
        commitAlignmentLoss
          ( checked
              "destination loss after inbound cancellation"
              (prepareAlignmentLoss causeA lost (coordinatorFor cancelledOwner))
          )
  assertEqual
    "cancellation-first cleanup reaches the same retained owner as loss-first cleanup"
    (alignmentLossCoordinatorOwner lossFirst)
    (alignmentLossCoordinatorOwner cancellationFirst)
  assertEqual
    "the cancellation-first schedule removes the dead closure wait"
    []
    ( Transfer.predecessorInputClosureEntries
        ( Alignment.alignmentTransferState
            (alignmentLossCoordinatorOwner cancellationFirst)
        )
    )
  assertEqual
    "cancellation-first successor invariant"
    (Right ())
    (validateAlignmentLossCoordinatorState cancellationFirst)

importOnlyDestinationLoss :: Assertion
importOnlyDestinationLoss = do
  let owner = singletonImportOnlyOwner
      predecessor = coordinatorFor owner
      prepared = checked "import-only destination loss" (prepareAlignmentLoss causeA localDestinationLoss predecessor)
      successorOwner = alignmentLossCoordinatorOwner (commitAlignmentLoss prepared)
  assertEqual "fixture has no relation obligation" [] (Alignment.activeAlignmentObligationEntries owner)
  assertBool "fixture has live fresh-base bootstrap work" (not (null (Alignment.bootstrapImportEntries owner)))
  assertEqual "import-only destination is tombstoned" [] (Alignment.bootstrapImportEntries successorOwner)
  case alignmentLossPlanInvalidationEvents (preparedAlignmentLossPlan prepared) of
    [(_, event)] -> do
      assertEqual "no ordinary owner is fabricated" [] (planInvalidationEventObligations event)
      assertBool "the exact import is retained in the tombstone" (not (null (planInvalidationEventBootstrapImports event)))
    receipts -> error ("expected one import-only receipt, got " <> show (length receipts))

freshBaseAttemptedLoss :: Assertion
freshBaseAttemptedLoss = do
  let (generation, key, attempt, owner) = singletonAttemptedOwner
      predecessor = coordinatorFor owner
      prepared = checked "attempted fresh-base loss" (prepareAlignmentLoss causeA localDestinationLoss predecessor)
      plan = preparedAlignmentLossPlan prepared
      successorOwner = alignmentLossCoordinatorOwner (commitAlignmentLoss prepared)
      expectedFresh = case Alignment.bootstrapImportKeySource key of
        Alignment.BootstrapFromFreshBase fresh -> fresh
        source -> error ("expected fresh-base key, got " <> show source)
      expectedCause =
        Alignment.AlignmentPlanFreshBaseStoreLost
          (alignmentGenerationId generation)
          expectedFresh
          (Alignment.alignmentSourceCoordinate localHerald localStore)
  assertBool "fresh-base source contributes its exact plan cause" (Set.member expectedCause (alignmentLossPlanInvalidationCauses plan))
  case alignmentLossPlanBootstrapRetirements plan of
    [(retired, receipt, Nothing)] -> do
      assertEqual "the exact pre-loss attempt is terminally retained" attempt retired
      assertEqual "the live receipt retains the exact attempt" attempt (Alignment.bootstrapImportAttemptInvalidationAttempt receipt)
      assertBool
        "source loss is joined to the plan-dominant receipt"
        ( Set.member
            Protocol.AlignmentSourceIncarnationLost
            (Alignment.bootstrapImportAttemptInvalidationReasons receipt)
        )
    retirements -> error ("expected one fresh-base retirement, got " <> show (length retirements))
  assertEqual "the fresh-base import is removed with its plan" [] (Alignment.bootstrapImportEntries successorOwner)
  assertEqual "fresh-base attempted successor invariant" (Right ()) (Alignment.validateAlignmentState successorOwner)

destinationLossAfterFulfillment :: Assertion
destinationLossAfterFulfillment = do
  let owner = fulfilledSingletonOwner
      predecessor = coordinatorFor owner
      prepared = checked "fulfilled destination loss" (prepareAlignmentLoss causeA localDestinationLoss predecessor)
      successor = commitAlignmentLoss prepared
  assertEqual "fulfilled fixture has no active bootstrap owner" [] (Alignment.bootstrapImportEntries owner)
  assertEqual "fulfilled fixture has no active attempt" [] (Alignment.bootstrapImportAttemptEntries owner)
  assertBool
    "current generation membership rediscovers the lost destination"
    (not (Set.null (alignmentLossPlanInvalidationCauses (preparedAlignmentLossPlan prepared))))
  assertBool "fulfilled coordinate is marked for debt redrive" (not (null (alignmentLossCoordinatorRedriveRequired successor)))
  assertEqual "fulfilled destination successor invariant" (Right ()) (validateAlignmentLossCoordinatorState successor)

remoteCurrentMemberLossAfterReadiness :: Assertion
remoteCurrentMemberLossAfterReadiness = do
  let plan = readyPlan foreignSingletonContext [remoteMember] [] Set.empty
      generation = only "remote current generation" (alignmentGenerationPlanGenerations plan)
      generationId = alignmentGenerationId generation
      promoted = commitPromotion occurrenceA plan (retainDebt occurrenceA Alignment.emptyState)
      owner = certifyGeneration generation promoted
      initial = coordinatorFor owner
      prepared = checked "lose certified remote member" (prepareAlignmentLoss causeA remoteSourceLoss initial)
      successor = commitAlignmentLoss prepared
      replay = checked "replay remote current-member loss" (prepareAlignmentLoss causeA remoteSourceLoss successor)
      expected = Alignment.AlignmentPlanDestinationStoreLost generationId (Protocol.destinationStore remoteDelta remoteStore)
      forward = applyLosses [(causeA, remoteSourceLoss), (causeAt 2, remoteSourceLoss)] initial
      backward = applyLosses [(causeAt 2, remoteSourceLoss), (causeA, remoteSourceLoss)] initial
  assertEqual "the replica owns no bootstrap imports for the completed remote member" [] (Alignment.bootstrapImportEntries owner)
  assertEqual "the replica owns no outstanding bootstrap attempts" [] (Alignment.bootstrapImportAttemptEntries owner)
  assertEqual "no source subscription is needed to discover replicated membership loss" [] (Transfer.sourceSubscriptionEntries (Alignment.alignmentTransferState owner))
  assertEqual "no destination subscription is needed to discover replicated membership loss" [] (Transfer.destinationSubscriptionEntries (Alignment.alignmentTransferState owner))
  assertBool "the remote member has an admitted historical certificate" (not (null (Alignment.alignmentHistoricalCertificateEntries owner)))
  assertEqual "the exact remote member invalidates the whole coordinate" (Set.singleton expected) (alignmentLossPlanInvalidationCauses (preparedAlignmentLossPlan prepared))
  assertEqual "the invalidated coordinate becomes live debt again" [(typedSort, topologyId, placement, Set.singleton causeA)] (redriveProjection successor)
  assertEqual "invalidation preserves exact historical certificates" (Alignment.alignmentHistoricalCertificateEntries owner) (Alignment.alignmentHistoricalCertificateEntries (alignmentLossCoordinatorOwner successor))
  assertEqual "no nonexistent subscription cancellation is fabricated" [] (alignmentLossPlanCancellations (preparedAlignmentLossPlan prepared))
  assertEqual "the same loss is exactly idempotent" AlignmentLossUnchanged (preparedAlignmentLossDisposition replay)
  assertEqual "exact replay preserves every owner and receipt" successor (commitAlignmentLoss replay)
  assertEqual "distinct retained loss causes converge in either order" forward backward
  assertEqual "remote current-member loss preserves the owner invariant" (Right ()) (validateAlignmentLossCoordinatorState forward)

supersededRemoteMemberLossPreservesHistory :: Assertion
supersededRemoteMemberLossPreservesHistory = do
  let oldPlan = readyPlan mergedContext recoveryMembers [] Set.empty
      oldGeneration = only "historical merged generation" (alignmentGenerationPlanGenerations oldPlan)
      oldId = alignmentGenerationId oldGeneration
      initial = commitPromotion occurrenceA oldPlan (retainDebt occurrenceB (retainDebt occurrenceA Alignment.emptyState))
      accepted = foldl' (\state herald -> retainCutAcceptanceFor herald oldGeneration state) initial [localHerald, remoteHerald]
      fulfilled = foldl' fulfillBootstrapImport accepted (fst <$> Alignment.bootstrapImportEntries accepted)
      certified = certifyGeneration oldGeneration fulfilled
      currentPlan = readyPlanAt nextPlacement singletonContext [localMember] [oldGeneration] (Set.singleton oldId)
      currentGeneration = only "surviving local generation" (alignmentGenerationPlanGenerations currentPlan)
      currentId = alignmentGenerationId currentGeneration
      current = commitPromotionAt nextPlacement occurrenceB currentPlan certified
      currentAccepted = foldl' (\state herald -> retainCutAcceptanceFor herald currentGeneration state) current [localHerald, remoteHerald]
      key = only "current predecessor import" (fst <$> predecessorBootstrapEntries currentAccepted)
      attempted = createBootstrapAttempt currentAccepted key
      oldAttempt = only "selected historical remote source" (snd <$> Alignment.bootstrapImportAttemptEntries attempted)
      prepared = checked "lose superseded remote source" (prepareAlignmentLoss causeA remoteSourceLoss (coordinatorFor attempted))
      successor = commitAlignmentLoss prepared
      owner = alignmentLossCoordinatorOwner successor
      replacement = only "replacement predecessor source" (snd <$> Alignment.bootstrapImportAttemptEntries owner)
  assertEqual "the lost remote Store belongs only to a superseded generation" [currentId] (alignmentGenerationId <$> Alignment.latestAlignmentGenerationsForSort typedSort attempted)
  assertEqual "the first source really is the remote historical representative" remoteHerald (Alignment.bootstrapImportAttemptSourceHerald oldAttempt)
  assertEqual "historical-only source loss cannot invalidate the current plan" Set.empty (alignmentLossPlanInvalidationCauses (preparedAlignmentLossPlan prepared))
  assertEqual "no historical plan is retroactively tombstoned" (Alignment.alignmentPlanInvalidationEntries attempted) (Alignment.alignmentPlanInvalidationEntries owner)
  assertEqual "the predecessor generation and its certificates remain retained" (Alignment.alignmentHistoricalCertificateEntries attempted) (Alignment.alignmentHistoricalCertificateEntries owner)
  assertEqual "source repair preserves the stable predecessor import" key (Alignment.bootstrapImportKeyValue (Alignment.bootstrapImportAttemptImport replacement))
  assertEqual "source repair uses a surviving certified representative" localHerald (Alignment.bootstrapImportAttemptSourceHerald replacement)
  assertBool "the replacement owns a fresh subscription" (Protocol.alignmentSubscribeSubscriptionId (Alignment.bootstrapImportAttemptSubscribe oldAttempt) /= Protocol.alignmentSubscribeSubscriptionId (Alignment.bootstrapImportAttemptSubscribe replacement))
  assertEqual "source repair does not request a new plan" [] (alignmentLossCoordinatorRedriveRequired successor)
  assertEqual "historical source repair preserves the owner invariant" (Right ()) (validateAlignmentLossCoordinatorState successor)

-- Retain the checked wire evidence a replica receives after all exact members
-- have completed their imports. No private owner records are constructed.
certifyGeneration :: AlignmentGeneration -> Alignment.State -> Alignment.State
certifyGeneration generation owner = foldl' (flip retainHistoricalCertificate) withReady certificates
  where
    generationId = alignmentGenerationId generation
    members = NonEmpty.toList (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation))
    ready =
      [ Protocol.classMemberReady
          (evidenceSequenceAt index)
          generationId
          (DomainAlignment.alignmentMemberStoreIncarnation member)
          (DomainAlignment.deriveMemberReadyEvidenceDigest (ByteString.singleton (fromIntegral index)))
          DomainAlignment.initialStoreRevision
      | (index, member) <- zip [0 ..] members
      ]
    withReady = foldl' (flip retainMemberReadiness) owner ready
    certificates =
      [ Protocol.historicalCertificate
          generationId
          (Protocol.classMemberReadyStoreIncarnation memberReady)
          (Protocol.classMemberReadySetDigest ready)
          (DomainAlignment.deriveBootstrapEvidenceDigest (DomainAlignment.memberReadyEvidenceDigestBytes (Protocol.classMemberReadyPredecessorAndBasePrefixDigest memberReady)))
          DomainAlignment.initialStoreRevision
      | memberReady <- ready
      ]

sourceLossReselectsBothKinds :: Assertion
sourceLossReselectsBothKinds = do
  let fixture = combinedFixture False
      predecessor = coordinatorFor fixture.attemptedOwner
      oldOrdinary = fmap snd (Alignment.alignmentAttemptEntries fixture.attemptedOwner)
      oldBootstrap = fmap snd (Alignment.bootstrapImportAttemptEntries fixture.attemptedOwner)
      prepared = checked "selected source loss" (prepareAlignmentLoss causeA remoteSourceLoss predecessor)
      plan = preparedAlignmentLossPlan prepared
      successor = commitAlignmentLoss prepared
      owner = alignmentLossCoordinatorOwner successor
      ordinaryRetirements = alignmentLossPlanOrdinaryRetirements plan
      bootstrapRetirements = alignmentLossPlanBootstrapRetirements plan
      currentStores =
        [ (DomainAlignment.alignmentMemberHerald member, DomainAlignment.alignmentMemberStoreIncarnation member)
        | generation <- Alignment.latestAlignmentGenerationsForSort typedSort fixture.attemptedOwner,
          member <- NonEmpty.toList (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation))
        ]
  assertEqual "the current remote member is a different physical incarnation" [(remoteHerald, replacementRemoteStore)] currentStores
  assertBool "the superseded source still owns an ordinary attempt" (not (null oldOrdinary))
  assertBool "the superseded source still owns a predecessor bootstrap attempt" (not (null oldBootstrap))
  assertEqual "predecessor source loss does not discard a destination plan" Set.empty (alignmentLossPlanInvalidationCauses plan)
  assertEqual "every ordinary selected attempt is retired" (length oldOrdinary) (length ordinaryRetirements)
  assertEqual "every bootstrap selected attempt is retired" (length oldBootstrap) (length bootstrapRetirements)
  assertEqual
    "ordinary retirement preserves the full exact attempt identity"
    oldOrdinary
    [retired | (retired, _, _) <- ordinaryRetirements]
  assertEqual
    "ordinary low-level receipts preserve the exact attempt"
    oldOrdinary
    [Alignment.alignmentAttemptInvalidationAttempt receipt | (_, receipt, _) <- ordinaryRetirements]
  assertEqual
    "bootstrap retirement preserves the full exact attempt identity"
    oldBootstrap
    [retired | (retired, _, _) <- bootstrapRetirements]
  assertEqual
    "bootstrap low-level receipts preserve the exact attempt"
    oldBootstrap
    [Alignment.bootstrapImportAttemptInvalidationAttempt receipt | (_, receipt, _) <- bootstrapRetirements]
  assertBool
    "ordinary replacements are fresh and use the surviving exact source"
    ( all
        ( \(retired, _, replacement) -> case replacement of
            Nothing -> False
            Just selected ->
              Protocol.alignmentAttemptSourceHerald selected == localHerald
                && Protocol.alignmentAttemptSourceStoreIncarnation selected == alternateStore
                && Protocol.alignmentAttemptSubscriptionId selected
                  /= Protocol.alignmentAttemptSubscriptionId retired
        )
        ordinaryRetirements
    )
  assertBool
    "bootstrap replacements are fresh and use the surviving exact source"
    ( all
        ( \(retired, _, replacement) -> case replacement of
            Nothing -> False
            Just selected ->
              Alignment.bootstrapImportAttemptSourceHerald selected == localHerald
                && Protocol.alignmentSubscribeSourceStoreIncarnation
                  (Alignment.bootstrapImportAttemptSubscribe selected)
                  == localStore
                && Protocol.alignmentSubscribeSubscriptionId
                  (Alignment.bootstrapImportAttemptSubscribe selected)
                  /= Protocol.alignmentSubscribeSubscriptionId
                    (Alignment.bootstrapImportAttemptSubscribe retired)
        )
        bootstrapRetirements
    )
  assertEqual "ordinary obligations survive" (length fixture.ordinaryIds) (length (Alignment.activeAlignmentObligationEntries owner))
  assertEqual "bootstrap imports survive" (length fixture.bootstrapKeys) (length (predecessorBootstrapEntries owner))
  assertEqual "live owner invariant" (Right ()) (Alignment.validateAlignmentState owner)
  assertEqual "composite invariant" (Right ()) (validateAlignmentLossCoordinatorState successor)

membershipRetirementDefersSourceReselection :: Assertion
membershipRetirementDefersSourceReselection = do
  let fixture = combinedFixture False
      predecessor = coordinatorFor fixture.attemptedOwner
      oldOrdinary = fmap snd (Alignment.alignmentAttemptEntries fixture.attemptedOwner)
      oldBootstrap = fmap snd (Alignment.bootstrapImportAttemptEntries fixture.attemptedOwner)
      oldFulfilled = Alignment.bootstrapImportFulfillmentEntries fixture.attemptedOwner
      prepared =
        checked
          "membership-retirement selected source loss"
          ( prepareAlignmentLossAfterMembershipRetirement
              causeA
              remoteSourceLoss
              predecessor
          )
      plan = preparedAlignmentLossPlan prepared
      successor = commitAlignmentLoss prepared
      owner = alignmentLossCoordinatorOwner successor
      replay =
        checked
          "membership-retirement selected source loss replay"
          ( prepareAlignmentLossAfterMembershipRetirement
              causeA
              remoteSourceLoss
              successor
          )
  assertBool
    "the membership-retirement fixture contains a fulfilled import"
    (not (null oldFulfilled))
  assertEqual
    "membership retirement keeps every stable ordinary obligation"
    (length fixture.ordinaryIds)
    (length (Alignment.activeAlignmentObligationEntries owner))
  assertEqual
    "membership retirement keeps every stable bootstrap import"
    (length fixture.bootstrapKeys)
    (length (predecessorBootstrapEntries owner))
  assertEqual
    "membership retirement preserves fulfilled imports byte-for-byte"
    oldFulfilled
    (Alignment.bootstrapImportFulfillmentEntries owner)
  assertEqual
    "the exact ordinary attempts are retired"
    oldOrdinary
    [retired | (retired, _, _) <- alignmentLossPlanOrdinaryRetirements plan]
  assertEqual
    "the exact bootstrap attempts are retired"
    oldBootstrap
    [retired | (retired, _, _) <- alignmentLossPlanBootstrapRetirements plan]
  assertBool
    "membership-time ordinary retirements carry no predecessor-generation replacement"
    (all (\(_, _, replacement) -> replacement == Nothing) (alignmentLossPlanOrdinaryRetirements plan))
  assertBool
    "membership-time bootstrap retirements carry no predecessor-generation replacement"
    (all (\(_, _, replacement) -> replacement == Nothing) (alignmentLossPlanBootstrapRetirements plan))
  assertEqual
    "no ordinary replacement is installed before successor-base advancement"
    []
    (Alignment.alignmentAttemptEntries owner)
  assertEqual
    "no bootstrap replacement is installed before successor-base advancement"
    []
    (Alignment.bootstrapImportAttemptEntries owner)
  assertBool
    "the unavailable source evidence survives for later successor-base selection"
    ( Set.member
        (Alignment.unavailableAlignmentSource remoteHerald remoteDelta remoteStore)
        (Alignment.alignmentUnavailableSources owner)
    )
  assertEqual
    "exact deferred loss replay is unchanged"
    AlignmentLossUnchanged
    (preparedAlignmentLossDisposition replay)
  assertEqual
    "exact deferred loss replay preserves the successor"
    successor
    (commitAlignmentLoss replay)
  assertEqual
    "deferred successor invariant"
    (Right ())
    (validateAlignmentLossCoordinatorState successor)

sourceLossWithoutReplacement :: Assertion
sourceLossWithoutReplacement = do
  let fixture = combinedFixtureWithCertificates False True
      predecessor = coordinatorFor fixture.attemptedOwner
      prepared = checked "exhausted source loss" (prepareAlignmentLoss causeA remoteSourceLoss predecessor)
      plan = preparedAlignmentLossPlan prepared
      successor = commitAlignmentLoss prepared
      owner = alignmentLossCoordinatorOwner successor
  assertEqual "source exhaustion does not tombstone a plan" Set.empty (alignmentLossPlanInvalidationCauses plan)
  assertBool
    "ordinary retirement records honest absence of replacement"
    (all (\(_, _, replacement) -> replacement == Nothing) (alignmentLossPlanOrdinaryRetirements plan))
  assertBool
    "bootstrap retirement records honest absence of replacement"
    (all (\(_, _, replacement) -> replacement == Nothing) (alignmentLossPlanBootstrapRetirements plan))
  assertEqual "ordinary obligations remain retained" (length fixture.ordinaryIds) (length (Alignment.activeAlignmentObligationEntries owner))
  assertEqual "predecessor bootstrap imports remain retained" (length fixture.bootstrapKeys) (length (predecessorBootstrapEntries owner))
  assertEqual "ordinary attempts are explicitly absent" [] (Alignment.alignmentAttemptEntries owner)
  assertEqual "predecessor bootstrap attempts are explicitly absent" [] (Alignment.bootstrapImportAttemptEntries owner)
  assertEqual "no destination debt redrive is fabricated" [] (alignmentLossCoordinatorRedriveRequired successor)
  assertEqual "exhausted successor invariant" (Right ()) (validateAlignmentLossCoordinatorState successor)

bothRolesCommitAtomically :: Assertion
bothRolesCommitAtomically = do
  let (oldGeneration, successorKey, oldAttempt, owner) = bothRolesOwner
      lost = qualifiedStoreCoordinate localHerald localDelta localStore
      predecessor = coordinatorFor owner
      prepared = checked "both-role loss" (prepareAlignmentLoss causeA lost predecessor)
      plan = preparedAlignmentLossPlan prepared
      successor = commitAlignmentLoss prepared
      successorOwner = alignmentLossCoordinatorOwner successor
      oldGenerationId = alignmentGenerationId oldGeneration
  assertBool
    "the old incarnation's destination plan is tombstoned"
    ( any
        ( \cause -> case cause of
            Alignment.AlignmentPlanDestinationStoreLost generation destination ->
              generation == oldGenerationId
                && Protocol.destinationStoreIncarnation destination == localStore
            _ -> False
        )
        (Set.toAscList (alignmentLossPlanInvalidationCauses plan))
    )
  case alignmentLossPlanBootstrapRetirements plan of
    [(retired, receipt, Nothing)] -> do
      assertEqual "the surviving coordinate's exact source attempt is retired" oldAttempt retired
      assertEqual "its low-level receipt retains the exact attempt" oldAttempt (Alignment.bootstrapImportAttemptInvalidationAttempt receipt)
    retirements -> error ("expected one surviving source retirement, got " <> show (length retirements))
  assertBool
    "the successor predecessor import survives unsourced"
    (any ((== successorKey) . fst) (Alignment.bootstrapImportEntries successorOwner))
  assertEqual "no replacement is fabricated" [] (Alignment.bootstrapImportAttemptEntries successorOwner)
  assertBool "the destination coordinate alone is marked for redrive" (not (null (alignmentLossCoordinatorRedriveRequired successor)))
  assertEqual "both-role atomic successor invariant" (Right ()) (validateAlignmentLossCoordinatorState successor)

lossBeforeSelectionIsMonotone :: Assertion
lossBeforeSelectionIsMonotone = do
  let fixture = combinedFixture False
      predecessor = coordinatorFor fixture.baseOwner
      prepared = checked "unselected source loss" (prepareAlignmentLoss causeA remoteSourceLoss predecessor)
      afterLoss = commitAlignmentLoss prepared
      afterLossOwner = alignmentLossCoordinatorOwner afterLoss
  assertEqual "no nonexistent ordinary attempt is retired" [] (alignmentLossPlanOrdinaryRetirements (preparedAlignmentLossPlan prepared))
  assertEqual "no nonexistent bootstrap attempt is retired" [] (alignmentLossPlanBootstrapRetirements (preparedAlignmentLossPlan prepared))
  assertBool
    "the unavailable exact source is durable without an attempt"
    ( Set.member
        (Alignment.unavailableAlignmentSource remoteHerald remoteDelta remoteStore)
        (Alignment.alignmentUnavailableSources afterLossOwner)
    )
  (withOrdinary, ordinaryAttempts) <-
    foldlM
      ( \(state, retained) identifier -> do
          let (successor, attempt) =
                checked
                  "loss-aware ordinary selection"
                  (prepareLossAwareAlignmentAttempt identifier state)
          pure (successor, retained <> [attempt])
      )
      (afterLoss, [])
      fixture.ordinaryIds
  (_, bootstrapAttempts) <-
    foldlM
      ( \(state, retained) key -> do
          let (successor, attempt) =
                checked
                  "loss-aware bootstrap selection"
                  (prepareLossAwareBootstrapImportAttempt key state)
          pure (successor, retained <> [attempt])
      )
      (withOrdinary, [])
      fixture.bootstrapKeys
  assertBool
    "later ordinary work cannot select the source lost before any attempt"
    ( all
        ( \attempt ->
            Protocol.alignmentAttemptSourceHerald attempt == localHerald
              && Protocol.alignmentAttemptSourceStoreIncarnation attempt == alternateStore
        )
        ordinaryAttempts
    )
  assertBool
    "later bootstrap work cannot select the source lost before any attempt"
    ( all
        ( \attempt ->
            Alignment.bootstrapImportAttemptSourceHerald attempt == localHerald
              && Protocol.alignmentSubscribeSourceStoreIncarnation
                (Alignment.bootstrapImportAttemptSubscribe attempt)
                == localStore
        )
        bootstrapAttempts
    )

remotePlacementLossFindsExactCauses :: Assertion
remotePlacementLossFindsExactCauses = do
  let owner = (combinedFixture False).attemptedOwner
      expected =
        Set.fromList
          [ structuralOccurrenceCause occurrenceA,
            structuralOccurrenceCause occurrenceB
          ]
  assertEqual
    "a selected remote source is linked to the exact retained promotions"
    expected
    (alignmentLossRelevantCauses remoteSourceLoss owner)
  assertEqual
    "a local destination is linked to the same complete plan causes"
    expected
    (alignmentLossRelevantCauses localDestinationLoss owner)

passivePlacementLossFindsExactCauses :: Assertion
passivePlacementLossFindsExactCauses = do
  let cause = structuralOccurrenceCause occurrenceA
      plan = readyPlan chainContext recoveryMembers [] Set.empty
      identifiers = Set.toAscList (Set.fromList (alignmentGenerationId <$> alignmentGenerationPlanGenerations plan))
      key = Alignment.alignmentPromotionKey cause typedSort topologyId placement
      retained = retainDebt occurrenceA Alignment.emptyState
      imported = checked "import checked completion" (Alignment.retainHistoricalAlignmentCoverage survivingHerald key plan retained)
      owner = checked "adopt canonical old frontier" (Alignment.adoptHistoricalAlignmentFrontier survivingHerald [(typedSort, identifiers)] imported)
      initial = checked "passive loss coordinator" (alignmentLossCoordinatorState survivingHerald owner)
      prepared = checked "lose passive remote member" (prepareAlignmentLoss cause remoteSourceLoss initial)
      successor = commitAlignmentLoss prepared
      successorOwner = alignmentLossCoordinatorOwner successor
      replay = checked "replay passive member loss" (prepareAlignmentLoss cause remoteSourceLoss successor)
      noOwnerWork state =
        null (Alignment.alignmentPromotionEntries state)
          && null (Alignment.alignmentObligationEntries state)
          && null (Alignment.bootstrapImportEntries state)
          && null (Alignment.alignmentAttemptEntries state)
          && null (Alignment.bootstrapImportAttemptEntries state)
          && null (Alignment.alignmentCutAcceptanceEntries state)
          && null (Alignment.alignmentLocalMemberReadinessEntries state)
          && Alignment.alignmentTransferState state == Alignment.alignmentTransferState Alignment.emptyState
  assertBool "the passive plan has multiple sibling generations" (length identifiers > 1)
  assertEqual "checked passive completion suppresses the receiver's own debt" [] (Alignment.liveAlignmentStructuralDebtEntries owner)
  assertEqual "the exact lost member recovers its passive completed cause" (Set.singleton cause) (alignmentLossRelevantCauses remoteSourceLoss owner)
  assertEqual "another incarnation cannot recover that cause" Set.empty (alignmentLossRelevantCauses (qualifiedStoreCoordinate remoteHerald remoteDelta replacementRemoteStore) owner)
  assertEqual "another Herald cannot recover that cause" Set.empty (alignmentLossRelevantCauses (qualifiedStoreCoordinate survivingHerald remoteDelta remoteStore) owner)
  assertEqual "one lost member tombstones the complete sibling coordinate" 1 (length (Alignment.alignmentPlanInvalidationEntries successorOwner))
  assertEqual "every sibling disappears from the live frontier" [(typedSort, [])] (Alignment.historicalAlignmentFrontier successorOwner)
  assertEqual "the invalidated plan no longer covers its cause" [] (Alignment.alignmentCompletedCauseCoverage successorOwner)
  assertEqual "the receiver's exact debt becomes live again" (Alignment.liveAlignmentStructuralDebtEntries retained) (Alignment.liveAlignmentStructuralDebtEntries successorOwner)
  assertEqual "the whole coordinate is ready for recovery" [(typedSort, topologyId, placement, Set.singleton cause)] (redriveProjection successor)
  assertEqual "immutable sibling evidence remains retained" (Alignment.alignmentGenerationEntries owner) (Alignment.alignmentGenerationEntries successorOwner)
  assertBool "neither import nor loss manufactures old owner work or votes" (all noOwnerWork [owner, successorOwner])
  assertEqual "no nonexistent subscription needs cancellation" [] (alignmentLossPlanCancellations (preparedAlignmentLossPlan prepared))
  assertEqual "exact loss replay is idempotent" AlignmentLossUnchanged (preparedAlignmentLossDisposition replay)
  assertEqual "exact replay preserves the complete successor" successor (commitAlignmentLoss replay)
  assertEqual "passive loss preserves the composite invariant" (Right ()) (validateAlignmentLossCoordinatorState successor)

incumbentLossExclusionIsChecked :: Assertion
incumbentLossExclusionIsChecked = do
  let fixture = combinedFixture False
      failed = Set.singleton (Alignment.alignmentSourceCoordinate remoteHerald remoteStore)
      (ordinaryId, _) = first "ordinary incumbent" (Alignment.alignmentAttemptEntries fixture.attemptedOwner)
      (bootstrapKey, _) = first "bootstrap incumbent" (Alignment.bootstrapImportAttemptEntries fixture.attemptedOwner)
  assertEqual
    "ordinary incumbent cannot bypass an additional durable loss fact"
    ( Left
        ( Alignment.AlignmentAttemptIncumbentSourceExcluded
            ordinaryId
            (Alignment.alignmentSourceCoordinate remoteHerald remoteStore)
        )
    )
    (fmap (const ()) (Alignment.prepareAlignmentAttemptExcluding failed ordinaryId fixture.attemptedOwner))
  assertEqual
    "bootstrap incumbent cannot bypass an additional durable loss fact"
    ( Left
        ( Alignment.BootstrapImportIncumbentSourceExcluded
            bootstrapKey
            (Alignment.alignmentSourceCoordinate remoteHerald remoteStore)
        )
    )
    (fmap (const ()) (Alignment.prepareBootstrapImportAttemptExcluding failed bootstrapKey fixture.attemptedOwner))

selectionOrderIsCanonical :: Assertion
selectionOrderIsCanonical = do
  let normalOrder = combinedFixture False
      reversed = combinedFixture True
      ordinarySources fixture =
        [ ( Protocol.alignmentAttemptSourceHerald attempt,
            Protocol.alignmentAttemptSourceStoreIncarnation attempt
          )
        | (_, attempt) <- Alignment.alignmentAttemptEntries fixture.attemptedOwner
        ]
      bootstrapSources fixture =
        [ ( Alignment.bootstrapImportAttemptSourceHerald attempt,
            Protocol.alignmentSubscribeSourceStoreIncarnation
              (Alignment.bootstrapImportAttemptSubscribe attempt)
          )
        | (_, attempt) <- Alignment.bootstrapImportAttemptEntries fixture.attemptedOwner
        ]
  assertEqual "ordinary selection ignores certificate insertion order" (ordinarySources normalOrder) (ordinarySources reversed)
  assertEqual "bootstrap selection ignores certificate insertion order" (bootstrapSources normalOrder) (bootstrapSources reversed)
  assertBool "the live canonical key chooses the lower Herald/source incarnation" (all (== (remoteHerald, remoteStore)) (ordinarySources normalOrder <> bootstrapSources normalOrder))

foreignDestinationOwnerIsRejected :: Assertion
foreignDestinationOwnerIsRejected = do
  let plan = readyPlan foreignSingletonContext [remoteMember] [] Set.empty
      withDebt = retainDebt occurrenceA Alignment.emptyState
      foreignOwner =
        fst
          ( Alignment.commitAlignmentPromotion
              ( checked
                  "promote foreign destination owner"
                  ( Alignment.prepareAlignmentPromotion
                      remoteHerald
                      (structuralOccurrenceCause occurrenceA)
                      typedSort
                      topologyId
                      placement
                      plan
                      withDebt
                  )
              )
          )
  assertBool
    "the fixture contains a foreign destination import"
    (not (null (Alignment.bootstrapImportEntries foreignOwner)))
  assertEqual
    "the coordinator owns destinations only for its declared Herald"
    (Left (AlignmentLossOwnerHeraldMismatch localHerald remoteHerald))
    (alignmentLossCoordinatorState localHerald foreignOwner)

historicalForeignDestinationOwnerIsRejected :: Assertion
historicalForeignDestinationOwnerIsRejected = do
  let fixture = combinedFixture False
      prepared =
        checked
          "terminalize the local destination plan"
          ( prepareAlignmentLoss
              (structuralOccurrenceCause occurrenceA)
              localDestinationLoss
              (coordinatorFor fixture.attemptedOwner)
          )
      successor = commitAlignmentLoss prepared
      owner = alignmentLossCoordinatorOwner successor
  assertEqual "the destination has no active ordinary obligations" [] (Alignment.activeAlignmentObligationEntries owner)
  assertEqual "the destination has no active bootstrap imports" [] (Alignment.bootstrapImportEntries owner)
  assertEqual "the terminal owner remains fully valid" (Right ()) (validateAlignmentLossCoordinatorState successor)
  assertEqual
    "retained destination history still binds the local Herald"
    (Left (AlignmentLossOwnerHeraldMismatch remoteHerald localHerald))
    (alignmentLossCoordinatorState remoteHerald owner)

data GeneratedLossTarget
  = GeneratedRemoteSource
  | GeneratedLocalDestination
  | GeneratedAlternateDestination
  deriving (Eq, Show)

data GeneratedLossCommand = GeneratedLossCommand
  { generatedLossTarget :: GeneratedLossTarget,
    generatedLossCauseIndex :: Int
  }
  deriving (Eq, Show)

type GeneratedLossKey =
  (StructuralConsequenceCause, QualifiedStoreCoordinate)

data CombinedLossReference = CombinedLossReference
  { combinedSeenKeys :: Set.Set GeneratedLossKey,
    combinedLostStores :: Set.Set QualifiedStoreCoordinate,
    combinedDestinationCauses :: Set.Set StructuralConsequenceCause,
    combinedPlanTombstoned :: Bool,
    combinedRemoteSourceLost :: Bool
  }
  deriving (Eq, Show)

data CombinedLossTrace = CombinedLossTrace
  { combinedTraceState :: AlignmentLossCoordinatorState,
    combinedTraceReference :: CombinedLossReference,
    combinedTracePlans :: Map.Map GeneratedLossKey AlignmentLossPlan,
    combinedTraceChecks :: [Property]
  }

initialCombinedLossReference :: CombinedLossReference
initialCombinedLossReference =
  CombinedLossReference
    { combinedSeenKeys = Set.empty,
      combinedLostStores = Set.empty,
      combinedDestinationCauses = Set.empty,
      combinedPlanTombstoned = False,
      combinedRemoteSourceLost = False
    }

generatedMixedLossSchedule :: Gen [GeneratedLossCommand]
generatedMixedLossSchedule = do
  sourceIndex <- chooseInt (1, 8)
  localIndex <- chooseInt (1, 8)
  alternateIndex <- chooseInt (1, 8)
  extraCount <- chooseInt (0, 4)
  extras <- vectorOf extraCount generatedLossCommand
  let required =
        [ GeneratedLossCommand GeneratedRemoteSource sourceIndex,
          GeneratedLossCommand GeneratedLocalDestination localIndex,
          GeneratedLossCommand GeneratedAlternateDestination alternateIndex
        ]
  replay <- elements required
  shuffle (replay : required <> extras)

generatedLossCommand :: Gen GeneratedLossCommand
generatedLossCommand =
  GeneratedLossCommand
    <$> elements
      [ GeneratedRemoteSource,
        GeneratedLocalDestination,
        GeneratedAlternateDestination
      ]
    <*> chooseInt (1, 8)

generatedLossCommandCause :: GeneratedLossCommand -> StructuralConsequenceCause
generatedLossCommandCause command = causeAt command.generatedLossCauseIndex

generatedLossCommandStore :: GeneratedLossCommand -> QualifiedStoreCoordinate
generatedLossCommandStore command = case command.generatedLossTarget of
  GeneratedRemoteSource -> remoteSourceLoss
  GeneratedLocalDestination -> localDestinationLoss
  GeneratedAlternateDestination -> alternateDestinationLoss

generatedLossCommandKey :: GeneratedLossCommand -> GeneratedLossKey
generatedLossCommandKey command =
  (generatedLossCommandCause command, generatedLossCommandStore command)

generatedLossCommandIsDestination :: GeneratedLossCommand -> Bool
generatedLossCommandIsDestination command = case command.generatedLossTarget of
  GeneratedRemoteSource -> False
  GeneratedLocalDestination -> True
  GeneratedAlternateDestination -> True

advanceCombinedLossReference ::
  GeneratedLossCommand -> CombinedLossReference -> CombinedLossReference
advanceCombinedLossReference command predecessor
  | Set.member key predecessor.combinedSeenKeys = predecessor
  | otherwise =
      predecessor
        { combinedSeenKeys = Set.insert key predecessor.combinedSeenKeys,
          combinedLostStores =
            Set.insert (generatedLossCommandStore command) predecessor.combinedLostStores,
          combinedDestinationCauses =
            if generatedLossCommandIsDestination command
              then
                Set.insert
                  (generatedLossCommandCause command)
                  predecessor.combinedDestinationCauses
              else predecessor.combinedDestinationCauses,
          combinedPlanTombstoned =
            predecessor.combinedPlanTombstoned
              || generatedLossCommandIsDestination command,
          combinedRemoteSourceLost =
            predecessor.combinedRemoteSourceLost
              || command.generatedLossTarget == GeneratedRemoteSource
        }
  where
    key = generatedLossCommandKey command

propGeneratedMixedLossSchedules :: Property
propGeneratedMixedLossSchedules =
  forAll generatedMixedLossSchedule $ \commands ->
    let fixture = combinedFixture False
        initial =
          CombinedLossTrace
            { combinedTraceState = coordinatorFor fixture.attemptedOwner,
              combinedTraceReference = initialCombinedLossReference,
              combinedTracePlans = Map.empty,
              combinedTraceChecks = []
            }
     in case foldlM (applyGeneratedMixedLoss fixture) initial commands of
          Left problem -> counterexample problem False
          Right trace ->
            counterexample
              ("generated commands: " <> show commands)
              (conjoin trace.combinedTraceChecks)

applyGeneratedMixedLoss ::
  CombinedFixture ->
  CombinedLossTrace ->
  GeneratedLossCommand ->
  Either String CombinedLossTrace
applyGeneratedMixedLoss fixture trace command = do
  prepared <-
    either
      (Left . ("generated mixed loss failed: " <>) . show)
      Right
      ( prepareAlignmentLoss
          (generatedLossCommandCause command)
          (generatedLossCommandStore command)
          trace.combinedTraceState
      )
  let key = generatedLossCommandKey command
      exactReplay = Map.lookup key trace.combinedTracePlans
      successor = commitAlignmentLoss prepared
      reference = advanceCombinedLossReference command trace.combinedTraceReference
      plan = preparedAlignmentLossPlan prepared
      transitionChecks = case exactReplay of
        Just retainedPlan ->
          [ preparedAlignmentLossDisposition prepared === AlignmentLossUnchanged,
            plan === retainedPlan,
            successor === trace.combinedTraceState
          ]
        Nothing ->
          [ preparedAlignmentLossDisposition prepared === AlignmentLossApplied,
            length (alignmentLossPlanInvalidationEvents plan)
              === if generatedLossCommandIsDestination command then 1 else 0,
            length (alignmentLossPlanOrdinaryRetirements plan)
              === expectedOrdinaryRetirements,
            length (alignmentLossPlanBootstrapRetirements plan)
              === expectedBootstrapRetirements
          ]
      expectedSourceRetirement =
        command.generatedLossTarget == GeneratedRemoteSource
          && not trace.combinedTraceReference.combinedRemoteSourceLost
          && not trace.combinedTraceReference.combinedPlanTombstoned
      expectedOrdinaryRetirements =
        if expectedSourceRetirement then length fixture.ordinaryIds else 0
      expectedBootstrapRetirements =
        if expectedSourceRetirement then length fixture.bootstrapKeys else 0
      retainedPlans = case exactReplay of
        Just _ -> trace.combinedTracePlans
        Nothing -> Map.insert key plan trace.combinedTracePlans
  replay <-
    either
      (Left . ("generated immediate loss replay failed: " <>) . show)
      Right
      ( prepareAlignmentLoss
          (generatedLossCommandCause command)
          (generatedLossCommandStore command)
          successor
      )
  let replayed = commitAlignmentLoss replay
      replayChecks =
        [ preparedAlignmentLossDisposition replay === AlignmentLossUnchanged,
          preparedAlignmentLossPlan replay === plan,
          replayed === successor,
          validateAlignmentLossCoordinatorState replayed === Right (),
          counterexample
            "re-admission lost the retained destination ownership boundary"
            ( alignmentLossCoordinatorState remoteHerald (alignmentLossCoordinatorOwner replayed)
                === Left (AlignmentLossOwnerHeraldMismatch remoteHerald localHerald)
            )
        ]
  Right
    CombinedLossTrace
      { combinedTraceState = successor,
        combinedTraceReference = reference,
        combinedTracePlans = retainedPlans,
        combinedTraceChecks =
          trace.combinedTraceChecks
            <> transitionChecks
            <> replayChecks
            <> combinedLossReferenceChecks fixture reference successor
      }

combinedLossReferenceChecks ::
  CombinedFixture ->
  CombinedLossReference ->
  AlignmentLossCoordinatorState ->
  [Property]
combinedLossReferenceChecks fixture reference state =
  [ alignmentLossCoordinatorLostStores state === reference.combinedLostStores,
    Set.fromList (fst <$> alignmentLossReceiptEntries state)
      === reference.combinedSeenKeys,
    redriveProjection state === expectedRedriveProjection,
    length (Alignment.activeAlignmentObligationEntries owner)
      === expectedOrdinaryCount,
    length (predecessorBootstrapEntries owner)
      === expectedBootstrapCount,
    length (Alignment.alignmentAttemptEntries owner)
      === expectedOrdinaryCount,
    length (Alignment.bootstrapImportAttemptEntries owner)
      === expectedBootstrapCount,
    counterexample
      "ordinary source selection disagrees with the independent loss model"
      (all ((== expectedOrdinarySource) . ordinarySource) ordinaryAttempts),
    counterexample
      "bootstrap source selection disagrees with the independent loss model"
      (all ((== expectedBootstrapSource) . bootstrapSource) bootstrapAttempts),
    Alignment.validateAlignmentState owner === Right (),
    validateAlignmentLossCoordinatorState state === Right ()
  ]
  where
    owner = alignmentLossCoordinatorOwner state
    ordinaryAttempts = snd <$> Alignment.alignmentAttemptEntries owner
    bootstrapAttempts = snd <$> Alignment.bootstrapImportAttemptEntries owner
    expectedOrdinaryCount =
      if reference.combinedPlanTombstoned
        then 0
        else length fixture.ordinaryIds
    expectedBootstrapCount =
      if reference.combinedPlanTombstoned
        then 0
        else length fixture.bootstrapKeys
    expectedOrdinarySource
      | reference.combinedRemoteSourceLost = (localHerald, alternateStore)
      | otherwise = (remoteHerald, remoteStore)
    expectedBootstrapSource
      | reference.combinedRemoteSourceLost = (localHerald, localStore)
      | otherwise = (remoteHerald, remoteStore)
    expectedRedriveProjection
      | Set.null reference.combinedDestinationCauses = []
      | otherwise =
          [ ( typedSort,
              topologyId,
              placement,
              reference.combinedDestinationCauses
            )
          ]

ordinarySource ::
  Protocol.AlignmentAttempt -> (Identity.HeraldEpoch, Identity.StoreIncarnationId)
ordinarySource attempt =
  ( Protocol.alignmentAttemptSourceHerald attempt,
    Protocol.alignmentAttemptSourceStoreIncarnation attempt
  )

bootstrapSource ::
  Alignment.BootstrapImportAttempt ->
  (Identity.HeraldEpoch, Identity.StoreIncarnationId)
bootstrapSource attempt =
  ( Alignment.bootstrapImportAttemptSourceHerald attempt,
    Protocol.alignmentSubscribeSourceStoreIncarnation
      (Alignment.bootstrapImportAttemptSubscribe attempt)
  )

redriveProjection ::
  AlignmentLossCoordinatorState ->
  [ ( SortOccurrence,
      Identity.TopologyCutId,
      DomainAlignment.PhysicalPlacementRevisionVector,
      Set.Set StructuralConsequenceCause
    )
  ]
redriveProjection state =
  [ ( Alignment.alignmentPlanCoordinateSort coordinate,
      Alignment.alignmentPlanCoordinateTopologyCut coordinate,
      Alignment.alignmentPlanCoordinatePlacementVector coordinate,
      causes
    )
  | (coordinate, causes) <- alignmentLossCoordinatorRedriveRequired state
  ]

data BothRoleLossTrace = BothRoleLossTrace
  { bothRoleTraceState :: AlignmentLossCoordinatorState,
    bothRoleSeenCauses :: Set.Set StructuralConsequenceCause,
    bothRoleTracePlans :: Map.Map StructuralConsequenceCause AlignmentLossPlan,
    bothRoleTraceChecks :: [Property]
  }

generatedBothRoleSchedule :: Gen [Int]
generatedBothRoleSchedule = do
  firstIndex <- chooseInt (1, 8)
  secondIndex <- chooseInt (1, 8)
  extraCount <- chooseInt (0, 4)
  extras <- vectorOf extraCount (chooseInt (1, 8))
  replay <- elements [firstIndex, secondIndex]
  shuffle (replay : firstIndex : secondIndex : extras)

propGeneratedBothRoleSchedules :: Property
propGeneratedBothRoleSchedules =
  forAll generatedBothRoleSchedule $ \indices ->
    let (oldGeneration, successorKey, oldAttempt, owner) = bothRolesOwner
        initial =
          BothRoleLossTrace
            { bothRoleTraceState = coordinatorFor owner,
              bothRoleSeenCauses = Set.empty,
              bothRoleTracePlans = Map.empty,
              bothRoleTraceChecks = []
            }
     in case foldlM
          ( applyGeneratedBothRoleLoss
              (alignmentGenerationId oldGeneration)
              successorKey
              oldAttempt
          )
          initial
          indices of
          Left problem -> counterexample problem False
          Right trace ->
            counterexample
              ("generated both-role cause indices: " <> show indices)
              (conjoin trace.bothRoleTraceChecks)

applyGeneratedBothRoleLoss ::
  DomainAlignment.ContextClassGenerationId ->
  Alignment.BootstrapImportKey ->
  Alignment.BootstrapImportAttempt ->
  BothRoleLossTrace ->
  Int ->
  Either String BothRoleLossTrace
applyGeneratedBothRoleLoss oldGeneration successorKey oldAttempt trace causeIndex = do
  let consequence = causeAt causeIndex
      lost = qualifiedStoreCoordinate localHerald localDelta localStore
  prepared <-
    either
      (Left . ("generated both-role loss failed: " <>) . show)
      Right
      (prepareAlignmentLoss consequence lost trace.bothRoleTraceState)
  let replay = Map.lookup consequence trace.bothRoleTracePlans
      firstPhysicalLoss = Set.null trace.bothRoleSeenCauses
      successor = commitAlignmentLoss prepared
      plan = preparedAlignmentLossPlan prepared
      retainedCauses = Set.insert consequence trace.bothRoleSeenCauses
      transitionChecks = case replay of
        Just retainedPlan ->
          [ preparedAlignmentLossDisposition prepared === AlignmentLossUnchanged,
            plan === retainedPlan,
            successor === trace.bothRoleTraceState
          ]
        Nothing ->
          [ preparedAlignmentLossDisposition prepared === AlignmentLossApplied,
            length (alignmentLossPlanInvalidationEvents plan) === 1,
            length (alignmentLossPlanBootstrapRetirements plan)
              === if firstPhysicalLoss then 1 else 0,
            counterexample
              "the generated both-role event lost its old destination generation"
              ( planInvalidatesGeneration oldGeneration plan
                  && not (Set.null (alignmentLossPlanInvalidationCauses plan))
              )
          ]
      retainedPlans = case replay of
        Just _ -> trace.bothRoleTracePlans
        Nothing -> Map.insert consequence plan trace.bothRoleTracePlans
      owner = alignmentLossCoordinatorOwner successor
      retainedSuccessorImports =
        [ bootstrapImport
        | (key, bootstrapImport) <- Alignment.bootstrapImportEntries owner,
          key == successorKey
        ]
      retirementReceipts =
        [ receipt
        | (_, receipt) <- Alignment.bootstrapImportAttemptInvalidationEntries owner,
          Alignment.bootstrapImportAttemptInvalidationAttempt receipt == oldAttempt
        ]
      stateChecks =
        [ alignmentLossCoordinatorLostStores successor === Set.singleton lost,
          Set.fromList (fst <$> alignmentLossReceiptEntries successor)
            === Set.fromList [(retained, lost) | retained <- Set.toAscList retainedCauses],
          redriveProjection successor
            === [(typedSort, topologyId, placement, retainedCauses)],
          length retainedSuccessorImports === 1,
          Alignment.bootstrapImportAttemptEntries owner === [],
          length retirementReceipts === 1,
          counterexample
            "the surviving predecessor attempt lacks the exact source-loss reason"
            ( all
                ( Set.member Protocol.AlignmentSourceIncarnationLost
                    . Alignment.bootstrapImportAttemptInvalidationReasons
                )
                retirementReceipts
            ),
          Alignment.validateAlignmentState owner === Right (),
          validateAlignmentLossCoordinatorState successor === Right ()
        ]
  Right
    BothRoleLossTrace
      { bothRoleTraceState = successor,
        bothRoleSeenCauses = retainedCauses,
        bothRoleTracePlans = retainedPlans,
        bothRoleTraceChecks = trace.bothRoleTraceChecks <> transitionChecks <> stateChecks
      }

planInvalidatesGeneration ::
  DomainAlignment.ContextClassGenerationId -> AlignmentLossPlan -> Bool
planInvalidatesGeneration generation plan =
  any
    ( \cause -> case cause of
        Alignment.AlignmentPlanDestinationStoreLost observed _ ->
          observed == generation
        Alignment.AlignmentPlanFreshBaseStoreLost observed _ _ ->
          observed == generation
        Alignment.AlignmentPlanSuperseded _ _ -> False
    )
    (Set.toAscList (alignmentLossPlanInvalidationCauses plan))

generatedDistinctCauseIndices :: Gen (Int, Int)
generatedDistinctCauseIndices = do
  firstIndex <- chooseInt (1, 8)
  offset <- chooseInt (1, 7)
  let secondIndex = ((firstIndex + offset - 1) `mod` 8) + 1
  pure (firstIndex, secondIndex)

propGeneratedDistinctDestinationLossesConverge :: Property
propGeneratedDistinctDestinationLossesConverge =
  forAll generatedDistinctCauseIndices $ \(firstIndex, secondIndex) ->
    let initial = coordinatorFor (combinedFixture False).attemptedOwner
        firstLoss = (causeAt firstIndex, localDestinationLoss)
        secondLoss = (causeAt secondIndex, alternateDestinationLoss)
        forward = applyLosses [firstLoss, secondLoss] initial
        reverseOrder = applyLosses [secondLoss, firstLoss] initial
        expectedCauses = Set.fromList [causeAt firstIndex, causeAt secondIndex]
     in conjoin
          [ forward === reverseOrder,
            redriveProjection forward
              === [(typedSort, topologyId, placement, expectedCauses)],
            validateAlignmentLossCoordinatorState forward === Right ()
          ]

applyLosses ::
  [(StructuralConsequenceCause, QualifiedStoreCoordinate)] ->
  AlignmentLossCoordinatorState ->
  AlignmentLossCoordinatorState
applyLosses losses initial =
  foldl'
    ( \state (cause, store) ->
        commitAlignmentLoss
          (checked "ordered alignment loss" (prepareAlignmentLoss cause store state))
    )
    initial
    losses

data CombinedFixture = CombinedFixture
  { baseOwner :: Alignment.State,
    attemptedOwner :: Alignment.State,
    ordinaryIds :: [Protocol.AlignmentObligationId],
    bootstrapKeys :: [Alignment.BootstrapImportKey]
  }

combinedFixture :: Bool -> CombinedFixture
combinedFixture reverseCertificates =
  combinedFixtureWithCertificates reverseCertificates False

combinedFixtureWithCertificates :: Bool -> Bool -> CombinedFixture
combinedFixtureWithCertificates reverseCertificates onlyRemoteCertificate =
  CombinedFixture
    { baseOwner = withSupersedingRemotePlan,
      attemptedOwner = withBootstrapAttempts,
      ordinaryIds,
      bootstrapKeys
    }
  where
    priorPlan = readyPlan mergedContext recoveryMembers [] Set.empty
    priorGenerations = alignmentGenerationPlanGenerations priorPlan
    predecessorIds = Set.fromList (alignmentGenerationId <$> priorGenerations)
    successorPlan = readyPlan chainContext recoveryMembers priorGenerations predecessorIds
    successorGenerations = alignmentGenerationPlanGenerations successorPlan
    withDebts = retainDebt occurrenceB (retainDebt occurrenceA Alignment.emptyState)
    withPrior = commitPromotion occurrenceA priorPlan withDebts
    promoted = commitPromotion occurrenceB successorPlan withPrior
    accepted =
      foldl'
        (\state (herald, generation) -> retainCutAcceptanceFor herald generation state)
        promoted
        [ (herald, generation)
        | generation <- priorGenerations <> successorGenerations,
          herald <- [remoteHerald, localHerald]
        ]
    predecessor = only "merged predecessor" priorGenerations
    ordinaryIds = fst <$> Alignment.activeAlignmentObligationEntries promoted
    ordinarySourceGenerationIds =
      Set.toAscList
        ( Set.fromList
            [ Protocol.alignmentObligationSourceGeneration obligation
            | (_, obligation) <- Alignment.activeAlignmentObligationEntries promoted
            ]
        )
    ordinarySourceGenerations =
      [ checked
          "ordinary source generation"
          ( maybe
              (Left generationId)
              Right
              (Alignment.lookupAlignmentGeneration generationId promoted)
          )
      | generationId <- ordinarySourceGenerationIds
      ]
    certifiedGenerations =
      Map.elems
        ( Map.fromList
            [ (alignmentGenerationId generation, generation)
            | generation <- predecessor : ordinarySourceGenerations
            ]
        )
    generationMembers generation =
      DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation)
    indexedMembers =
      zip
        [0 ..]
        [ (generation, member)
        | generation <- certifiedGenerations,
          member <- NonEmpty.toList (generationMembers generation)
        ]
    readiness =
      [ ( generation,
          Protocol.classMemberReady
            (evidenceSequenceAt index)
            (alignmentGenerationId generation)
            (DomainAlignment.alignmentMemberStoreIncarnation member)
            (DomainAlignment.deriveMemberReadyEvidenceDigest (ByteString.singleton (fromIntegral index)))
            DomainAlignment.initialStoreRevision
        )
      | (index, (generation, member)) <- indexedMembers
      ]
    withReadiness =
      foldl'
        (\state (_, ready) -> retainMemberReadiness ready state)
        accepted
        readiness
    certificate generation ready =
      Protocol.historicalCertificate
        (alignmentGenerationId generation)
        (Protocol.classMemberReadyStoreIncarnation ready)
        ( Protocol.classMemberReadySetDigest
            [ retainedReady
            | (retainedGeneration, retainedReady) <- readiness,
              alignmentGenerationId retainedGeneration
                == alignmentGenerationId generation
            ]
        )
        ( DomainAlignment.deriveBootstrapEvidenceDigest
            ( DomainAlignment.memberReadyEvidenceDigestBytes
                (Protocol.classMemberReadyPredecessorAndBasePrefixDigest ready)
            )
        )
        DomainAlignment.initialStoreRevision
    certificates =
      [ retained
      | (generation, ready) <- readiness,
        let retained = certificate generation ready,
        not onlyRemoteCertificate
          || Protocol.historicalCertificateSourceStoreIncarnation retained
            == remoteStore
      ]
    withCertificates =
      foldl'
        (flip retainHistoricalCertificate)
        withReadiness
        (if reverseCertificates then reverse certificates else certificates)
    remoteFreshBaseKeys =
      [ key
      | (key, _) <- Alignment.bootstrapImportEntries withCertificates,
        Alignment.BootstrapFromFreshBase fresh <-
          [Alignment.bootstrapImportKeySource key],
        DomainAlignment.freshMemberBaseStoreIncarnation fresh == remoteStore
      ]
    withFulfilledRemoteFreshBase =
      foldl' fulfillBootstrapImport withCertificates remoteFreshBaseKeys
    -- The old source class is historical before its representative is lost.
    -- This foreign-only successor creates no new local work: the previous
    -- ordinary stream and predecessor imports remain owned until completion.
    -- Keeping remoteStore in the latest plan would instead make its loss a
    -- destination loss on every replica, regardless of these source attempts.
    supersedingPlan =
      readyPlanAt
        remoteReplacementPlacement
        foreignSingletonContext
        [replacementRemoteMember]
        successorGenerations
        (Set.fromList (alignmentGenerationId <$> successorGenerations))
    withSupersedingRemotePlan =
      commitPromotionAt
        remoteReplacementPlacement
        occurrenceC
        supersedingPlan
        (retainDebt occurrenceC withFulfilledRemoteFreshBase)
    withOrdinaryAttempts =
      foldl' createOrdinaryAttempt withSupersedingRemotePlan ordinaryIds
    bootstrapKeys = fst <$> predecessorBootstrapEntries withOrdinaryAttempts
    withBootstrapAttempts = foldl' createBootstrapAttempt withOrdinaryAttempts bootstrapKeys

singletonImportOnlyOwner :: Alignment.State
singletonImportOnlyOwner = commitPromotion occurrenceA plan (retainDebt occurrenceA Alignment.emptyState)
  where
    plan = readyPlan singletonContext [localMember] [] Set.empty

singletonAttemptedOwner ::
  ( AlignmentGeneration,
    Alignment.BootstrapImportKey,
    Alignment.BootstrapImportAttempt,
    Alignment.State
  )
singletonAttemptedOwner =
  (generation, key, attempt, attempted)
  where
    plan = readyPlan singletonContext [localMember] [] Set.empty
    generation = only "singleton generation" (alignmentGenerationPlanGenerations plan)
    promoted = commitPromotion occurrenceA plan (retainDebt occurrenceA Alignment.emptyState)
    accepted = retainCutAcceptanceFor localHerald generation promoted
    key = only "singleton fresh-base key" (fst <$> Alignment.bootstrapImportEntries accepted)
    attempted = createBootstrapAttempt accepted key
    attempt = only "singleton fresh-base attempt" (snd <$> Alignment.bootstrapImportAttemptEntries attempted)

fulfilledSingletonOwner :: Alignment.State
fulfilledSingletonOwner = fulfilled
  where
    (generation, _, attempt, attempted) = singletonAttemptedOwner
    subscribe = Alignment.bootstrapImportAttemptSubscribe attempt
    subscription = Protocol.alignmentSubscribeSubscriptionId subscribe
    sourceStore = Protocol.alignmentSubscribeSourceStoreIncarnation subscribe
    digest =
      Protocol.alignmentSemanticSnapshotDigest
        (alignmentGenerationId generation)
        sourceStore
        DomainAlignment.initialStoreRevision
        []
    controls =
      [ Protocol.AlignmentSnapshotStarted
          ( Protocol.alignmentSnapshotStart
              subscription
              DomainAlignment.initialStoreRevision
              digest
              1
          ),
        Protocol.AlignmentSnapshotChunkTransferred
          (Protocol.alignmentSnapshotChunk subscription 0 []),
        Protocol.AlignmentSnapshotEnded
          ( Protocol.alignmentSnapshotEnd
              subscription
              DomainAlignment.initialStoreRevision
              digest
          ),
        Protocol.AlignmentLiveAdvertised
          (Protocol.alignmentLive subscription DomainAlignment.initialStoreRevision)
      ]
    completedTransfer = foldl' applyDestinationControl (Alignment.alignmentTransferState attempted) controls
    completed = Alignment.replaceAlignmentTransferState completedTransfer attempted
    prepared =
      checked
        "fulfill singleton bootstrap"
        ( Alignment.prepareBootstrapImportFulfillment
            attempt
            DomainAlignment.initialStoreRevision
            completed
        )
    (fulfilled, _) = Alignment.commitBootstrapImportFulfillment prepared

bothRolesOwner ::
  ( AlignmentGeneration,
    Alignment.BootstrapImportKey,
    Alignment.BootstrapImportAttempt,
    Alignment.State
  )
bothRolesOwner =
  (oldGeneration, successorKey, successorAttempt, attempted)
  where
    oldPlan = readyPlan singletonContext [localMember] [] Set.empty
    oldGeneration = only "both-role old generation" (alignmentGenerationPlanGenerations oldPlan)
    successorPlan =
      readyPlanAt
        nextPlacement
        singletonContext
        [replacementLocalMember]
        [oldGeneration]
        (Set.singleton (alignmentGenerationId oldGeneration))
    successorGeneration =
      only "both-role successor generation" (alignmentGenerationPlanGenerations successorPlan)
    withDebts = retainDebt occurrenceB (retainDebt occurrenceA Alignment.emptyState)
    oldPromoted = commitPromotionAt placement occurrenceA oldPlan withDebts
    oldAccepted = retainCutAcceptanceFor localHerald oldGeneration oldPromoted
    ready =
      Protocol.classMemberReady
        Protocol.firstAlignmentEvidenceSequence
        (alignmentGenerationId oldGeneration)
        localStore
        localReadyForBothRoles
        DomainAlignment.initialStoreRevision
    withReady = retainMemberReadiness ready oldAccepted
    certificate =
      Protocol.historicalCertificate
        (alignmentGenerationId oldGeneration)
        localStore
        (Protocol.classMemberReadySetDigest [ready])
        ( DomainAlignment.deriveBootstrapEvidenceDigest
            (DomainAlignment.memberReadyEvidenceDigestBytes localReadyForBothRoles)
        )
        DomainAlignment.initialStoreRevision
    withCertificate = retainHistoricalCertificate certificate withReady
    promoted = commitPromotionAt nextPlacement occurrenceB successorPlan withCertificate
    accepted = retainCutAcceptanceFor localHerald successorGeneration promoted
    successorKey =
      only
        "both-role predecessor key"
        [ key
        | (key, _) <- Alignment.bootstrapImportEntries accepted,
          Alignment.BootstrapFromPredecessor predecessor <-
            [Alignment.bootstrapImportKeySource key],
          predecessor == alignmentGenerationId oldGeneration
        ]
    attempted = createBootstrapAttempt accepted successorKey
    successorAttempt =
      only
        "both-role predecessor attempt"
        [ attempt
        | (key, attempt) <- Alignment.bootstrapImportAttemptEntries attempted,
          key == successorKey
        ]

applyDestinationControl :: Transfer.State -> Protocol.AlignmentControl -> Transfer.State
applyDestinationControl predecessor control = case control of
  Protocol.AlignmentSnapshotStarted start -> commit (Transfer.prepareDestinationSnapshotStart start predecessor)
  Protocol.AlignmentSnapshotChunkTransferred chunk -> commit (Transfer.prepareDestinationSnapshotChunk chunk predecessor)
  Protocol.AlignmentSnapshotEnded end -> commit (Transfer.prepareDestinationSnapshotEnd end predecessor)
  Protocol.AlignmentChangeTransferred change -> commit (Transfer.prepareDestinationChange change predecessor)
  Protocol.AlignmentLiveAdvertised live -> commit (Transfer.prepareDestinationLive live predecessor)
  _ -> predecessor
  where
    commit prepared =
      fst
        ( Transfer.commitDestinationWork
            (checked "apply destination bootstrap control" prepared)
        )

predecessorBootstrapEntries ::
  Alignment.State -> [(Alignment.BootstrapImportKey, Alignment.BootstrapImport)]
predecessorBootstrapEntries owner =
  [ (key, bootstrapImport)
  | (key, bootstrapImport) <- Alignment.bootstrapImportEntries owner,
    Alignment.BootstrapFromPredecessor _ <- [Alignment.bootstrapImportKeySource key]
  ]

remoteDestinationSourceOwner ::
  (Protocol.AlignmentSubscribe, Alignment.State)
remoteDestinationSourceOwner =
  (subscribe, Alignment.replaceAlignmentTransferState withClosure Alignment.emptyState)
  where
    predecessorGeneration =
      checked
        "remote-destination predecessor generation"
        (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 0xd1))
    destinationGeneration =
      checked
        "remote-destination successor generation"
        (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 0xd2))
    obligationIdentifier =
      Protocol.alignmentObligationId
        remoteHerald
        Protocol.firstAlignmentObligationSequence
    subscriptionIdentifier =
      Protocol.alignmentSubscriptionId
        remoteHerald
        Protocol.firstAlignmentSubscriptionSequence
    obligation =
      checked
        "remote-destination source obligation"
        ( Protocol.alignmentObligation
            obligationIdentifier
            causeA
            sortId
            sortOccurrenceId
            destinationGeneration
            (Protocol.destinationStore remoteDelta remoteStore :| [])
            predecessorGeneration
            Normal
        )
    subscribe =
      checked
        "remote-destination source subscribe"
        ( Protocol.alignmentSubscribe
            obligation
            subscriptionIdentifier
            localStore
            ( Protocol.BootstrapProof
                destinationGeneration
                (DomainAlignment.deriveBootstrapEvidenceDigest "remote-destination-source")
            )
        )
    preparedSource =
      checked
        "retain remote-destination source transcript"
        ( Transfer.prepareSourceSubscription
            subscribe
            DomainAlignment.initialStoreRevision
            []
            Transfer.SourceLiveWithheld
            Transfer.emptyState
        )
    (withSource, _) = Transfer.commitSourceSubscription preparedSource
    barrierObligationIdentifier =
      Protocol.alignmentObligationId
        localHerald
        Protocol.firstAlignmentObligationSequence
    barrierSubscriptionIdentifier =
      Protocol.alignmentSubscriptionId
        localHerald
        Protocol.firstAlignmentSubscriptionSequence
    barrierObligation =
      checked
        "remote-destination local barrier obligation"
        ( Protocol.alignmentObligation
            barrierObligationIdentifier
            causeA
            sortId
            sortOccurrenceId
            predecessorGeneration
            (Protocol.destinationStore localDelta localStore :| [])
            predecessorGeneration
            Normal
        )
    barrierCertificate =
      Protocol.historicalCertificate
        predecessorGeneration
        alternateStore
        (DomainAlignment.deriveMemberReadyEvidenceDigest "remote-destination-local-barrier")
        (DomainAlignment.deriveBootstrapEvidenceDigest "remote-destination-local-barrier-prefix")
        DomainAlignment.initialStoreRevision
    barrierAttempt =
      checked
        "remote-destination local barrier attempt"
        ( Protocol.alignmentAttempt
            barrierObligation
            barrierSubscriptionIdentifier
            remoteHerald
            barrierCertificate
        )
    preparedDestination =
      checked
        "retain distinct local ordinary barrier transcript"
        (Transfer.prepareDestinationAttempt barrierAttempt withSource)
    (withDestination, _) =
      Transfer.commitDestinationAttempt preparedDestination
    preparedClosure =
      checked
        "retain incomplete remote-destination closure wait"
        ( Transfer.preparePredecessorInputClosures
            destinationGeneration
            (Set.singleton barrierSubscriptionIdentifier)
            withDestination
        )
    (withClosure, _) = Transfer.commitPredecessorInputClosures preparedClosure

sharedRemoteDestinationSourceOwner ::
  (Protocol.AlignmentSubscribe, Protocol.AlignmentSubscribe, Alignment.State)
sharedRemoteDestinationSourceOwner =
  (lostSubscribe, survivingSubscribe, successorOwner)
  where
    (lostSubscribe, predecessorOwner) = remoteDestinationSourceOwner
    predecessorGeneration =
      checked
        "shared-barrier predecessor generation"
        (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 0xd1))
    destinationGeneration =
      checked
        "shared-barrier successor generation"
        (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 0xd2))
    survivingObligation =
      checked
        "shared-barrier surviving source obligation"
        ( Protocol.alignmentObligation
            ( Protocol.alignmentObligationId
                survivingHerald
                Protocol.firstAlignmentObligationSequence
            )
            causeA
            sortId
            sortOccurrenceId
            destinationGeneration
            (Protocol.destinationStore alternateDelta alternateStore :| [])
            predecessorGeneration
            Normal
        )
    survivingSubscribe =
      checked
        "shared-barrier surviving source subscribe"
        ( Protocol.alignmentSubscribe
            survivingObligation
            ( Protocol.alignmentSubscriptionId
                survivingHerald
                Protocol.firstAlignmentSubscriptionSequence
            )
            localStore
            ( Protocol.BootstrapProof
                destinationGeneration
                (DomainAlignment.deriveBootstrapEvidenceDigest "shared-barrier-surviving-source")
            )
        )
    preparedSource =
      checked
        "retain shared-barrier surviving source"
        ( Transfer.prepareSourceSubscription
            survivingSubscribe
            DomainAlignment.initialStoreRevision
            []
            Transfer.SourceLiveWithheld
            (Alignment.alignmentTransferState predecessorOwner)
        )
    (successorTransfer, _) = Transfer.commitSourceSubscription preparedSource
    successorOwner =
      Alignment.replaceAlignmentTransferState successorTransfer predecessorOwner

coordinatorFor :: Alignment.State -> AlignmentLossCoordinatorState
coordinatorFor = checked "alignment-loss coordinator" . alignmentLossCoordinatorState localHerald

createOrdinaryAttempt :: Alignment.State -> Protocol.AlignmentObligationId -> Alignment.State
createOrdinaryAttempt owner identifier =
  Alignment.replaceAlignmentTransferState successorTransfer withAttempt
  where
    prepared =
      checked
        "create ordinary attempt"
        (Alignment.prepareAlignmentAttempt identifier owner)
    attempt = Alignment.preparedAlignmentAttempt prepared
    (withAttempt, _) = Alignment.commitAlignmentAttempt prepared
    (successorTransfer, _) =
      Transfer.commitDestinationAttempt
        ( checked
            "retain ordinary destination subscription"
            ( Transfer.prepareDestinationAttempt
                attempt
                (Alignment.alignmentTransferState withAttempt)
            )
        )

createBootstrapAttempt :: Alignment.State -> Alignment.BootstrapImportKey -> Alignment.State
createBootstrapAttempt owner key =
  fst
    ( Alignment.commitBootstrapImportAttempt
        (checked "create predecessor bootstrap attempt" (Alignment.prepareBootstrapImportAttempt key owner))
    )

fulfillBootstrapImport :: Alignment.State -> Alignment.BootstrapImportKey -> Alignment.State
fulfillBootstrapImport owner key = fulfilled
  where
    attempted = createBootstrapAttempt owner key
    attempt =
      only
        "bootstrap attempt to fulfill"
        [ retained
        | (retainedKey, retained) <- Alignment.bootstrapImportAttemptEntries attempted,
          retainedKey == key
        ]
    subscribe = Alignment.bootstrapImportAttemptSubscribe attempt
    subscription = Protocol.alignmentSubscribeSubscriptionId subscribe
    sourceStore = Protocol.alignmentSubscribeSourceStoreIncarnation subscribe
    digest =
      Protocol.alignmentSemanticSnapshotDigest
        (Alignment.bootstrapImportKeyGeneration key)
        sourceStore
        DomainAlignment.initialStoreRevision
        []
    controls =
      [ Protocol.AlignmentSnapshotStarted
          ( Protocol.alignmentSnapshotStart
              subscription
              DomainAlignment.initialStoreRevision
              digest
              1
          ),
        Protocol.AlignmentSnapshotChunkTransferred
          (Protocol.alignmentSnapshotChunk subscription 0 []),
        Protocol.AlignmentSnapshotEnded
          ( Protocol.alignmentSnapshotEnd
              subscription
              DomainAlignment.initialStoreRevision
              digest
          ),
        Protocol.AlignmentLiveAdvertised
          (Protocol.alignmentLive subscription DomainAlignment.initialStoreRevision)
      ]
    completedTransfer =
      foldl'
        applyDestinationControl
        (Alignment.alignmentTransferState attempted)
        controls
    completed = Alignment.replaceAlignmentTransferState completedTransfer attempted
    prepared =
      checked
        "fulfill bootstrap import"
        ( Alignment.prepareBootstrapImportFulfillment
            attempt
            DomainAlignment.initialStoreRevision
            completed
        )
    (fulfilled, _) = Alignment.commitBootstrapImportFulfillment prepared

commitPromotion ::
  Identity.StructuralOccurrenceId ->
  AlignmentGenerationPlan ->
  Alignment.State ->
  Alignment.State
commitPromotion occurrence plan owner =
  commitPromotionAt placement occurrence plan owner

commitPromotionAt ::
  DomainAlignment.PhysicalPlacementRevisionVector ->
  Identity.StructuralOccurrenceId ->
  AlignmentGenerationPlan ->
  Alignment.State ->
  Alignment.State
commitPromotionAt selectedPlacement occurrence plan owner =
  fst
    ( Alignment.commitAlignmentPromotion
        ( checked
            "promote generation plan"
            ( Alignment.prepareAlignmentPromotion
                localHerald
                (structuralOccurrenceCause occurrence)
                typedSort
                topologyId
                selectedPlacement
                plan
                owner
            )
        )
    )

retainDebt :: Identity.StructuralOccurrenceId -> Alignment.State -> Alignment.State
retainDebt occurrence owner =
  fst
    ( Alignment.commitStructuralDebtRetention
        ( checked
            "retain alignment debt"
            ( Alignment.prepareStructuralDebtRetention
                (structuralOccurrenceCause occurrence)
                ( normalizeStructuralDebts
                    [ structuralConsequenceDebt
                        ( structuralDebtKey
                            (structuralOccurrenceCause occurrence)
                            TopologyAlignmentDebt
                            typedSort
                            Nothing
                        )
                        (structuralDebtEvidence Set.empty Set.empty Set.empty)
                    ]
                )
                owner
            )
        )
    )

retainCutAcceptanceFor ::
  Identity.HeraldEpoch -> AlignmentGeneration -> Alignment.State -> Alignment.State
retainCutAcceptanceFor herald generation owner =
  fst
    ( Alignment.commitAlignmentCutAcceptance
        ( checked
            "retain cut acceptance"
            ( Alignment.prepareAlignmentCutAcceptance
                ( Protocol.alignmentCutAccepted
                    (alignmentGenerationId generation)
                    herald
                    (Topology.deriveTopologyCutId (DomainAlignment.alignmentCutTopologyCut cut))
                    (DomainAlignment.alignmentCutPhysicalPlacementRevisionVector cut)
                    DomainAlignment.EmptyHeraldPublicationPrefix
                )
                owner
            )
        )
    )
  where
    cut = alignmentGenerationCut generation

retainMemberReadiness :: Protocol.ClassMemberReady -> Alignment.State -> Alignment.State
retainMemberReadiness ready owner =
  fst
    ( Alignment.commitClassMemberReady
        (checked "retain member readiness" (Alignment.prepareClassMemberReady ready owner))
    )

retainHistoricalCertificate ::
  Protocol.HistoricalCertificate -> Alignment.State -> Alignment.State
retainHistoricalCertificate certificate owner =
  fst
    ( Alignment.commitHistoricalCertificate
        (checked "retain historical certificate" (Alignment.prepareHistoricalCertificate certificate owner))
    )

readyPlan ::
  ContextGraph ->
  [GenerationMemberInput] ->
  [AlignmentGeneration] ->
  Set.Set DomainAlignment.ContextClassGenerationId ->
  AlignmentGenerationPlan
readyPlan graphContext members prior certificates =
  readyPlanAt placement graphContext members prior certificates

readyPlanAt ::
  DomainAlignment.PhysicalPlacementRevisionVector ->
  ContextGraph ->
  [GenerationMemberInput] ->
  [AlignmentGeneration] ->
  Set.Set DomainAlignment.ContextClassGenerationId ->
  AlignmentGenerationPlan
readyPlanAt selectedPlacement graphContext members prior certificates =
  case prepareAlignmentGenerationPlan
    typedSort
    topology
    selectedPlacement
    graphContext
    members
    prior
    []
    certificates of
    Right (AlignmentGenerationReady plan) -> plan
    other -> error ("expected a ready generation plan, got " <> show other)

recoveryMembers :: [GenerationMemberInput]
recoveryMembers = [localMember, remoteMember, alternateMember]

localMember, replacementLocalMember, remoteMember, replacementRemoteMember, alternateMember :: GenerationMemberInput
localMember =
  generationMemberInput localDelta localStore localHerald DomainAlignment.initialStoreRevision
replacementLocalMember =
  generationMemberInput
    localDelta
    replacementLocalStore
    localHerald
    DomainAlignment.initialStoreRevision
remoteMember =
  generationMemberInput remoteDelta remoteStore remoteHerald DomainAlignment.initialStoreRevision
replacementRemoteMember =
  generationMemberInput remoteDelta replacementRemoteStore remoteHerald DomainAlignment.initialStoreRevision
alternateMember =
  generationMemberInput
    alternateDelta
    alternateStore
    localHerald
    DomainAlignment.initialStoreRevision

mergedContext, chainContext, singletonContext, foreignSingletonContext :: ContextGraph
mergedContext =
  context
    "merged context"
    [DeltaVertex localDelta, DeltaVertex remoteDelta, DeltaVertex alternateDelta]
    [ edgePayload (DeltaVertex localDelta) (DeltaVertex remoteDelta) Preserve,
      edgePayload (DeltaVertex remoteDelta) (DeltaVertex localDelta) Preserve,
      edgePayload (DeltaVertex remoteDelta) (DeltaVertex alternateDelta) Preserve,
      edgePayload (DeltaVertex alternateDelta) (DeltaVertex remoteDelta) Preserve
    ]
    [localDelta, remoteDelta, alternateDelta]
chainContext =
  context
    "remote-source to local-destination context"
    [DeltaVertex localDelta, DeltaVertex remoteDelta, DeltaVertex alternateDelta]
    [ edgePayload (DeltaVertex remoteDelta) (DeltaVertex alternateDelta) Preserve,
      edgePayload (DeltaVertex alternateDelta) (DeltaVertex remoteDelta) Preserve,
      edgePayload (DeltaVertex remoteDelta) (DeltaVertex localDelta) Preserve
    ]
    [localDelta, remoteDelta, alternateDelta]
singletonContext =
  context "singleton context" [DeltaVertex localDelta] [] [localDelta]
foreignSingletonContext =
  context "foreign singleton context" [DeltaVertex remoteDelta] [] [remoteDelta]

context :: String -> [VertexId] -> [EdgePayload] -> [Identity.DeltaId] -> ContextGraph
context label vertices edges active =
  checked label (deriveContextGraph vertices edges active)

topology :: Topology.TopologyCut
topology =
  checked
    "alignment-loss topology cut"
    ( Topology.topologyCut
        (Topology.sameGenerationPredecessor topologyPredecessor)
        ( Topology.topologyFrontier
            (Structural.emptyStructuralVersionVector membership)
            (Identity.controlIndex 0)
        )
        (Topology.deriveTopologyOccurrenceDigest "step-14-alignment-loss-topology")
    )

topologyId :: Identity.TopologyCutId
topologyId = Topology.deriveTopologyCutId topology

placement :: DomainAlignment.PhysicalPlacementRevisionVector
placement =
  checked
    "alignment-loss placement"
    ( DomainAlignment.physicalPlacementRevisionVector
        membership
        [ (remoteHerald, DomainAlignment.firstPlacementRevision),
          (localHerald, DomainAlignment.firstPlacementRevision)
        ]
    )

nextPlacement :: DomainAlignment.PhysicalPlacementRevisionVector
nextPlacement =
  checked
    "next alignment-loss placement"
    ( DomainAlignment.physicalPlacementRevisionVector
        membership
        [ (remoteHerald, DomainAlignment.firstPlacementRevision),
          ( localHerald,
            DomainAlignment.nextPlacementRevision
              DomainAlignment.firstPlacementRevision
          )
        ]
    )

remoteReplacementPlacement :: DomainAlignment.PhysicalPlacementRevisionVector
remoteReplacementPlacement =
  checked
    "remote replacement alignment-loss placement"
    ( DomainAlignment.physicalPlacementRevisionVector
        membership
        [ ( remoteHerald,
            DomainAlignment.nextPlacementRevision
              DomainAlignment.firstPlacementRevision
          ),
          (localHerald, DomainAlignment.firstPlacementRevision)
        ]
    )

membership :: Membership.HeraldMembershipGeneration
membership =
  checked
    "alignment-loss membership generation"
    ( Membership.genesisHeraldMembershipGeneration
        membershipSystem
        (remoteHerald :| [localHerald])
    )

membershipSystem :: Identity.SystemId
membershipSystem =
  checked
    "alignment-loss membership system"
    (Identity.mkSystemId (fixtureIdentifierBytes 0xf0))

typedSort :: SortOccurrence
typedSort = sortOccurrence sortId sortOccurrenceId

localReadyForBothRoles :: DomainAlignment.MemberReadyEvidenceDigest
localReadyForBothRoles =
  DomainAlignment.deriveMemberReadyEvidenceDigest "both-role-local-ready"

localDestinationLoss, alternateDestinationLoss, remoteSourceLoss :: QualifiedStoreCoordinate
localDestinationLoss = qualifiedStoreCoordinate localHerald localDelta localStore
alternateDestinationLoss =
  qualifiedStoreCoordinate localHerald alternateDelta alternateStore
remoteSourceLoss = qualifiedStoreCoordinate remoteHerald remoteDelta remoteStore

causeA :: StructuralConsequenceCause
causeA = causeAt 1

causeAt :: Int -> StructuralConsequenceCause
causeAt index =
  checked
    "label release loss cause"
    (labelReleaseCause decision (Identity.controlIndex (fromIntegral index)))

occurrenceA, occurrenceB, occurrenceC :: Identity.StructuralOccurrenceId
occurrenceA =
  Identity.structuralOccurrenceId
    localHerald
    (checked "occurrence A" (Identity.mkStructuralSequence 1))
occurrenceB =
  Identity.structuralOccurrenceId
    localHerald
    (checked "occurrence B" (Identity.mkStructuralSequence 2))
occurrenceC =
  Identity.structuralOccurrenceId
    localHerald
    (checked "occurrence C" (Identity.mkStructuralSequence 3))

remoteHerald, localHerald, survivingHerald :: Identity.HeraldEpoch
remoteHerald = identity "remote Herald" Identity.mkHeraldEpoch 0x10
localHerald = identity "local Herald" Identity.mkHeraldEpoch 0x20
survivingHerald = identity "surviving Herald" Identity.mkHeraldEpoch 0x30

localDelta, remoteDelta, alternateDelta :: Identity.DeltaId
localDelta = identity "local delta" Identity.mkDeltaId 0x30
remoteDelta = identity "remote delta" Identity.mkDeltaId 0x31
alternateDelta = identity "alternate delta" Identity.mkDeltaId 0x32

localStore, replacementLocalStore, remoteStore, replacementRemoteStore, alternateStore :: Identity.StoreIncarnationId
localStore = identity "local store" Identity.mkStoreIncarnationId 0x50
replacementLocalStore =
  identity "replacement local store" Identity.mkStoreIncarnationId 0x51
remoteStore = identity "remote store" Identity.mkStoreIncarnationId 0x40
replacementRemoteStore = identity "replacement remote store" Identity.mkStoreIncarnationId 0x41
alternateStore = identity "alternate store" Identity.mkStoreIncarnationId 0x60

sortId :: Identity.SortId
sortId = identity "sort" Identity.mkSortId 0x60

sortOccurrenceId :: Identity.SortDefinitionOccurrenceId
sortOccurrenceId = identity "sort occurrence" Identity.mkSortDefinitionOccurrenceId 0x61

decision :: Identity.LabelDecisionId
decision = identity "loss decision" Identity.mkLabelDecisionId 0x70

topologyPredecessor :: Identity.TopologyCutId
topologyPredecessor = identity "topology predecessor" Identity.mkTopologyCutId 0x80

identity ::
  (Show problem) =>
  String ->
  (ByteString.ByteString -> Either problem value) ->
  Word ->
  value
identity label constructor marker =
  checked label (constructor (fixtureIdentifierBytes (fromIntegral marker)))

only :: String -> [value] -> value
only _ [value] = value
only label values = error (label <> ": expected one, got " <> show (length values))

first :: String -> [value] -> value
first _ (value : _) = value
first label [] = error (label <> ": expected a value")

evidenceSequenceAt :: Int -> Protocol.AlignmentEvidenceSequence
evidenceSequenceAt index =
  iterate
    Protocol.nextAlignmentEvidenceSequence
    Protocol.firstAlignmentEvidenceSequence
    !! index

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id

foldlM ::
  (Monad monad) =>
  (accumulator -> value -> monad accumulator) ->
  accumulator ->
  [value] ->
  monad accumulator
foldlM _ initial [] = pure initial
foldlM step initial (value : remaining) =
  step initial value >>= \successor -> foldlM step successor remaining
