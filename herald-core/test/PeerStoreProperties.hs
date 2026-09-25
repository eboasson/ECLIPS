{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PeerStoreProperties
  ( tests,
  )
where

import Control.Monad (foldM, forM_)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word8)
import Eclips.Domain.Alignment
  ( StoreRevision,
    initialStoreRevision,
    nextStoreRevision,
  )
import Eclips.Domain.Disappearance (deriveDisappearanceProbeId)
import Eclips.Domain.Identity
  ( DeltaId,
    PublicationId,
    StoreIncarnationId,
    controlIndex,
    genesisAuthorityEpoch,
    mkDeltaId,
    mkGlobalUniqueId,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkStoreIncarnationId,
    mkTopologyCutId,
    nablaSequence,
    nablaSequenceWord64,
    publicationAuthorityEpoch,
    publicationId,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationValue,
    mkCheckedPublication,
  )
import Eclips.Domain.Route (ReplicaStrength (Normal, Weak), freezeRoute, routeDestination)
import Eclips.Domain.Sort.Canonical
  ( canonicalizeCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (OrdinaryApplicationMutation),
    DescriptorAdmission (ApplicationDescriptor),
    DescriptorSpec (..),
    PredicateExpression (AlwaysPredicate, NeverPredicate),
    RankDirection (Ascending),
    RankTerm (RankApplicationValue),
    SortKind (ControlledSort),
    checkDescriptor,
  )
import Eclips.Domain.Startup
  ( PredefinedSortRole (SortDefinitionRole),
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaPublication,
    primordialReplicaRole,
    primordialReplicaSortId,
  )
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Domain.StructuralConsequence (predefinedDisappearanceCause)
import Eclips.Domain.Value
  ( directProjection,
    globalUniqueIdSchema,
    globalUniqueIdValue,
    int64Schema,
    int64Value,
    mkFieldName,
    recordSchema,
    recordValue,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
  )
import Eclips.Herald.Store.State qualified as Store
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureIdentifierBytes,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Positive (..),
    Property,
    ioProperty,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "incoming peer Store application"
    [ testCase "a mixed current/stale batch applies only exact current stores" caseMixedBatch,
      testCase "retained local delivery skips removed or replaced Stores without retargeting" caseRetainedLocalRoute,
      testCase "an equal duplicate is an exact idempotent receipt" caseDuplicate,
      testCase "receipt strength joins through a revision-addressable observation" caseReceiptStrengthJoin,
      testCase "distinct routed provenance is revision-addressable even at equal strength" caseDistinctRoutedProvenance,
      testCase "retained revisions and source evidence are exact and contiguous" caseRetainedHistory,
      testCase "snapshot provenance separates representative and strength witnesses" caseSnapshotEvidence,
      testCase "alignment replays representative at witnessed strength without retargeting" caseAlignmentReplay,
      testCase "visible-only local take leaves the retained revision unchanged" caseLocalTakeRevision,
      testCase "passivation retains queryable history and rejects closed-incarnation reuse" caseInactiveHistory,
      testCase "controlled purge suppresses every retained incarnation and stale replay" caseControlledObjectPurge,
      testCase "a zero-match controlled purge is inherited by the first Store incarnation" caseControlledObjectPurgeBeforeStore,
      testCase "terminal purge preserves raw history across an unrelated retained update" caseControlledPurgeHistoryAfterUnrelatedUpdate,
      testProperty "any positive duplicate suffix leaves the retained prefix unchanged" propDuplicateSuffix,
      testProperty "every finite apply/take trace advances iff retained state or evidence changes" propRevisionIffRetainedChange
    ]

caseMixedBatch :: Assertion
caseMixedBatch = do
  fixture <- storeFixture
  let predecessorReceipts =
        maybe
          []
          Store.storeSlotApplicationReceipts
          (Store.lookupStoreSlot (delta fixture) (state fixture))
  prepared <-
    checked
      "mixed peer application"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          ( Store.peerStoreDestination
              (delta fixture)
              (incarnation fixture)
              Normal
              :| [ Store.peerStoreDestination
                     (absent fixture)
                     (absentIncarnation fixture)
                     Weak
                 ]
          )
          (publication fixture)
          (state fixture)
      )
  let (successor, outcomes) = Store.commitPeerStoreApplication prepared
  assertEqual
    "outcomes preserve the exact destination order"
    [Store.PeerStoreApplied, Store.PeerStoreTerminallyIgnored]
    (fmap Store.peerStoreOutcomeDisposition (toList outcomes))
  slot <-
    maybe
      (assertFailure "current store disappeared")
      pure
      (Store.lookupStoreSlot (delta fixture) successor)
  assertEqual
    "the current store retains the new immutable receipt"
    (Just Normal)
    (lookup (checkedPublicationId (publication fixture)) (Store.storeSlotApplicationReceipts slot))
  assertEqual
    "the current store adds exactly one receipt"
    (length predecessorReceipts + 1)
    (length (Store.storeSlotApplicationReceipts slot))
  assertEqual
    "the predecessor remains untouched"
    predecessorReceipts
    ( maybe
        []
        Store.storeSlotApplicationReceipts
        (Store.lookupStoreSlot (delta fixture) (state fixture))
    )

-- These destinations were both current when the source accepted its route.
-- Delayed structural completion can run after one reader has passivated and
-- even reactivated; only the surviving exact incarnation may receive the data.
caseRetainedLocalRoute :: Assertion
caseRetainedLocalRoute = forM_ [False, True] $ \replace -> forM_ [Weak, Normal] $ \strength -> do
  fixture <- storeFixture
  let original = slotFor fixture (state fixture)
      local = checkedLocalHeraldEpoch fixtureCheckedGenesis
      inactive = Store.passivateStoreForInvariantTest (delta fixture) (state fixture)
      replacementIncarnation = absentIncarnation fixture
  route <-
    checked
      "accepted local route"
      ( freezeRoute
          [ routeDestination (delta fixture) (incarnation fixture) local Normal,
            routeDestination (alignmentDelta fixture) (alignmentIncarnation fixture) local strength
          ]
      )
  predecessor <-
    if replace
      then
        Store.commitStoreBootstrap
          <$> checked
            "install a replacement Store"
            ( Store.prepareStoreBootstrap
                [ Store.localStoreSpec
                    (Store.storeSlotProvenance original)
                    (delta fixture)
                    (Store.storeSlotSortId original)
                    (Store.storeSlotOccurrenceId original)
                    replacementIncarnation
                ]
                inactive
            )
      else pure inactive
  survivingBefore <-
    maybe
      (assertFailure "surviving reader missing before delivery")
      pure
      (Store.lookupStoreSlot (alignmentDelta fixture) predecessor)
  case Store.prepareStoreApplication (envelope fixture) route (publication fixture) predecessor of
    Left (Store.StoreTransitionDeltaMissing actual)
      | not replace ->
          assertEqual "immediate application still rejects a missing destination" (delta fixture) actual
    Left (Store.StoreTransitionIncarnationMismatch actual expected found) | replace -> do
      assertEqual "immediate application rejects the stale Delta" (delta fixture) actual
      assertEqual "the frozen incarnation remains the expected destination" (incarnation fixture) expected
      assertEqual "the replacement cannot satisfy immediate application" replacementIncarnation found
    Left problem -> assertFailure ("unexpected immediate-route refusal: " <> show problem)
    Right _ -> assertFailure "immediate application accepted a stale route"
  prepared <-
    checked
      "settle retained local route"
      (Store.prepareRetainedStoreApplication (envelope fixture) route (publication fixture) predecessor)
  let successor = Store.commitStoreApplication prepared
  survivingAfter <-
    maybe
      (assertFailure "surviving reader missing after delivery")
      pure
      (Store.lookupStoreSlot (alignmentDelta fixture) successor)
  assertEqual
    "delayed data never reaches a replacement incarnation"
    (Store.lookupStoreSlot (delta fixture) predecessor)
    (Store.lookupStoreSlot (delta fixture) successor)
  assertEqual
    "the closed original incarnation retains its exact history"
    (Store.lookupRetainedStoreSlot (incarnation fixture) predecessor)
    (Store.lookupRetainedStoreSlot (incarnation fixture) successor)
  assertEqual
    "the surviving destination records its frozen route strength"
    (Just strength)
    (lookup (checkedPublicationId (publication fixture)) (Store.storeSlotApplicationReceipts survivingAfter))
  assertEqual
    "the surviving destination records exactly one delivery transition"
    (nextStoreRevision (Store.storeSlotRevision survivingBefore))
    (Store.storeSlotRevision survivingAfter)
  retry <-
    checked
      "replay retained local route"
      (Store.prepareRetainedStoreApplication (envelope fixture) route (publication fixture) successor)
  assertBool
    "replaying the frozen route is exactly idempotent"
    (Store.commitStoreApplication retry == successor)
  assertEqual
    "retained local completion preserves Store history invariants"
    (Right ())
    (Store.validateStoreHistoryState successor)

caseDuplicate :: Assertion
caseDuplicate = do
  fixture <- storeFixture
  first <-
    checked
      "first peer application"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          (Store.peerStoreDestination (delta fixture) (incarnation fixture) Normal :| [])
          (publication fixture)
          (state fixture)
      )
  let (afterFirst, _) = Store.commitPeerStoreApplication first
  duplicate <-
    checked
      "duplicate peer application"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          (Store.peerStoreDestination (delta fixture) (incarnation fixture) Normal :| [])
          (publication fixture)
          afterFirst
      )
  let (afterDuplicate, outcomes) = Store.commitPeerStoreApplication duplicate
  assertEqual
    "the duplicate preserves the exact store witness"
    (Store.storeSlots afterFirst)
    (Store.storeSlots afterDuplicate)
  assertEqual
    "the duplicate is observed as already applied"
    [Store.PeerStoreAlreadyApplied]
    (fmap Store.peerStoreOutcomeDisposition (toList outcomes))

caseReceiptStrengthJoin :: Assertion
caseReceiptStrengthJoin = do
  fixture <- storeFixture
  first <-
    checked
      "first weak peer application"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          (Store.peerStoreDestination (delta fixture) (incarnation fixture) Weak :| [])
          (publication fixture)
          (state fixture)
      )
  let (afterFirst, _) = Store.commitPeerStoreApplication first
      revisionAfterFirst = revisionFor fixture afterFirst
  strengthened <-
    checked
      "normal receipt strengthening"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          (Store.peerStoreDestination (delta fixture) (incarnation fixture) Normal :| [])
          (publication fixture)
          afterFirst
      )
  let (afterStrengthening, outcomes) = Store.commitPeerStoreApplication strengthened
  assertEqual
    "strengthening is a legitimate application outcome"
    [Store.PeerStoreApplied]
    (fmap Store.peerStoreOutcomeDisposition (toList outcomes))
  assertEqual
    "the receipt joins to normal"
    (Just Normal)
    ( lookup
        (checkedPublicationId (publication fixture))
        (Store.storeSlotApplicationReceipts (slotFor fixture afterStrengthening))
    )
  assertEqual
    "the stronger observation advances the retained alignment evidence prefix"
    (nextStoreRevision revisionAfterFirst)
    (revisionFor fixture afterStrengthening)

caseDistinctRoutedProvenance :: Assertion
caseDistinctRoutedProvenance = do
  fixture <- storeFixture
  first <-
    checked
      "first normal peer application"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          (Store.peerStoreDestination (delta fixture) (incarnation fixture) Normal :| [])
          (publication fixture)
          (state fixture)
      )
  let (afterFirst, _) = Store.commitPeerStoreApplication first
      revisionAfterFirst = revisionFor fixture afterFirst
      original = envelope fixture
      laterEnvelope =
        Store.storeObservationEnvelope
          (Store.storeObservationEnvelopeSourceProcess original)
          (Store.storeObservationEnvelopeSortOccurrence original)
          (Store.storeObservationEnvelopeSourceTopology original)
          (controlIndex 8)
          (Store.storeObservationEnvelopeStructuralStamp original)
  second <-
    checked
      "same publication with later routed provenance"
      ( Store.preparePeerStoreApplication
          laterEnvelope
          (Store.peerStoreDestination (delta fixture) (incarnation fixture) Normal :| [])
          (publication fixture)
          afterFirst
      )
  let (afterSecond, outcomes) = Store.commitPeerStoreApplication second
      revisionAfterSecond = revisionFor fixture afterSecond
  assertEqual
    "new causal provenance is applied rather than classified as an exact replay"
    [Store.PeerStoreApplied]
    (fmap Store.peerStoreOutcomeDisposition (toList outcomes))
  assertEqual
    "the distinct routed observation advances one exact revision"
    (nextStoreRevision revisionAfterFirst)
    revisionAfterSecond
  changes <-
    checked
      "changes after the first routed observation"
      ( Store.retainedStoreChangesAfter
          (incarnation fixture)
          revisionAfterFirst
          afterSecond
      )
  case changes of
    [change] -> do
      assertEqual
        "the raw winner remains unchanged"
        Nothing
        (Store.retainedStoreChangeTransition change)
      assertEqual
        "the exact later envelope remains revision-addressable"
        (Store.RoutedStoreObservation laterEnvelope)
        ( Store.retainedStoreObservationOrigin
            (Store.retainedStoreChangeObservation change)
        )
    other -> assertFailure ("expected one provenance-only change, got " <> show other)
  assertEqual
    "provenance-only history remains valid"
    (Right ())
    (Store.validateStoreHistoryState afterSecond)

caseRetainedHistory :: Assertion
caseRetainedHistory = do
  fixture <- storeFixture
  assertEqual
    "a new incarnation begins at revision zero"
    initialStoreRevision
    (revisionFor fixture (state fixture))
  _ <- checked "initial Store history invariant" (Store.validateStoreHistoryState (state fixture))
  initialSnapshot <-
    checked
      "revision-zero snapshot"
      ( Store.retainedStoreSnapshotAt
          (incarnation fixture)
          initialStoreRevision
          (state fixture)
      )
  assertEqual
    "snapshot names the exact incarnation"
    (incarnation fixture)
    (Store.retainedStoreSnapshotIncarnation initialSnapshot)
  assertBool
    "the trusted primordial base is present at the empty retained-change prefix"
    (not (null (Store.retainedStoreSnapshotFacts initialSnapshot)))
  afterFirst <- applyPeer fixture Weak (state fixture)
  let firstRevision = nextStoreRevision initialStoreRevision
  assertEqual
    "one actual retained transition advances exactly once"
    firstRevision
    (revisionFor fixture afterFirst)
  _ <- checked "successor Store history invariant" (Store.validateStoreHistoryState afterFirst)
  changes <-
    checked
      "changes after revision zero"
      ( Store.retainedStoreChangesAfter
          (incarnation fixture)
          initialStoreRevision
          afterFirst
      )
  change <- case changes of
    [onlyChange] -> pure onlyChange
    _ -> assertFailure ("expected one retained change, got " <> show changes)
  assertEqual
    "the change key is the reached revision"
    firstRevision
    (Store.retainedStoreChangeRevision change)
  let observation = Store.retainedStoreChangeObservation change
  assertEqual
    "the exact checked publication is retained"
    (publication fixture)
    (Store.retainedStoreObservationPublication observation)
  assertEqual
    "incoming routed strength is retained independently"
    Weak
    (Store.retainedStoreObservationIncomingStrength observation)
  assertEqual
    "the exact definition occurrence is retained"
    (Store.storeObservationEnvelopeSortOccurrence (envelope fixture))
    (Store.retainedStoreObservationSortOccurrence observation)
  assertEqual
    "the complete destination-free source envelope is retained"
    (Store.RoutedStoreObservation (envelope fixture))
    (Store.retainedStoreObservationOrigin observation)

caseSnapshotEvidence :: Assertion
caseSnapshotEvidence = do
  fixture <- storeFixture
  successor <- applyPeer fixture Weak (state fixture)
  snapshot <-
    checked
      "revision-one snapshot"
      ( Store.retainedStoreSnapshotAt
          (incarnation fixture)
          (revisionFor fixture successor)
          successor
      )
  fact <-
    maybe
      (assertFailure "fresh representative missing from retained snapshot")
      pure
      ( find
          ( (== checkedPublicationId (publication fixture))
              . retainedStoreObservationId
              . Store.retainedStoreSnapshotFactRepresentative
          )
          (Store.retainedStoreSnapshotFacts snapshot)
      )
  let representative = Store.retainedStoreSnapshotFactRepresentative fact
      strengthWitness = Store.retainedStoreSnapshotFactStrengthWitness fact
  assertEqual
    "representative records the weak observation that advanced provenance"
    Weak
    (Store.retainedStoreObservationIncomingStrength representative)
  assertEqual
    "joined normal strength names its distinct exact primordial witness"
    Normal
    (Store.retainedStoreObservationIncomingStrength strengthWitness)
  assertBool
    "the snapshot does not fabricate representative-at-joined-strength metadata"
    ( retainedStoreObservationId representative
        /= retainedStoreObservationId strengthWitness
    )
  assertEqual
    "effective logical-state strength remains separate"
    Normal
    (Store.retainedStoreSnapshotFactStrength fact)

caseAlignmentReplay :: Assertion
caseAlignmentReplay = do
  fixture <- storeFixture
  sourceActive <- applyPeer fixture Weak (state fixture)
  let sourceRevision = revisionFor fixture sourceActive
      sourceInactive =
        Store.passivateStoreForInvariantTest (delta fixture) sourceActive
  snapshot <-
    checked
      "inactive source snapshot"
      ( Store.retainedStoreSnapshotAt
          (incarnation fixture)
          sourceRevision
          sourceInactive
      )
  fact <-
    maybe
      (assertFailure "alignment representative missing from retained snapshot")
      pure
      ( find
          ( (== checkedPublicationId (publication fixture))
              . retainedStoreObservationId
              . Store.retainedStoreSnapshotFactRepresentative
          )
          (Store.retainedStoreSnapshotFacts snapshot)
      )
  let representative = Store.retainedStoreSnapshotFactRepresentative fact
      strengthWitness = Store.retainedStoreSnapshotFactStrengthWitness fact
      joinedStrength = Store.retainedStoreSnapshotFactStrength fact
  assertBool
    "alignment source carries separate representative and strength witnesses"
    ( retainedStoreObservationId representative
        /= retainedStoreObservationId strengthWitness
    )
  replayObservation <-
    checked
      "attenuated alignment observation"
      ( Store.retainedStoreObservation
          (Store.retainedStoreObservationPublication representative)
          joinedStrength
          (Store.retainedStoreObservationSortOccurrence representative)
          (Store.retainedStoreObservationOrigin representative)
      )
  case Store.prepareAlignmentStoreApplication
    ( Store.peerStoreDestination
        (alignmentDelta fixture)
        (alignmentIncarnation fixture)
        Weak
        :| []
    )
    replayObservation
    sourceInactive of
    Left (Store.StoreTransitionAlignmentStrengthMismatch actualDelta Normal Weak) ->
      assertEqual
        "strength mismatch names the exact destination"
        (alignmentDelta fixture)
        actualDelta
    Left problem ->
      assertFailure ("unexpected alignment strength result: " <> show problem)
    Right _ ->
      assertFailure "alignment admission accepted strength after construction"
  prepared <-
    checked
      "alignment replay"
      ( Store.prepareAlignmentStoreApplication
          ( Store.peerStoreDestination
              (alignmentDelta fixture)
              (alignmentIncarnation fixture)
              joinedStrength
              :| []
          )
          replayObservation
          sourceInactive
      )
  let (afterReplay, outcomes) = Store.commitPeerStoreApplication prepared
  assertEqual
    "the current destination admits the witnessed representative"
    [Store.PeerStoreApplied]
    (fmap Store.peerStoreOutcomeDisposition (toList outcomes))
  destinationChanges <-
    checked
      "alignment destination changes"
      ( Store.retainedStoreChangesAfter
          (alignmentIncarnation fixture)
          initialStoreRevision
          afterReplay
      )
  case destinationChanges of
    [change] ->
      assertEqual
        "the destination history preserves the exact replay observation"
        replayObservation
        (Store.retainedStoreChangeObservation change)
    _ ->
      assertFailure
        ("expected one alignment destination change, got " <> show destinationChanges)
  duplicate <-
    checked
      "duplicate alignment replay"
      ( Store.prepareAlignmentStoreApplication
          ( Store.peerStoreDestination
              (alignmentDelta fixture)
              (alignmentIncarnation fixture)
              joinedStrength
              :| []
          )
          replayObservation
          afterReplay
      )
  let (afterDuplicate, duplicateOutcomes) =
        Store.commitPeerStoreApplication duplicate
  assertEqual
    "the exact alignment replay is idempotent"
    [Store.PeerStoreAlreadyApplied]
    (fmap Store.peerStoreOutcomeDisposition (toList duplicateOutcomes))
  assertEqual
    "the duplicate preserves the retained destination prefix"
    ( Store.lookupRetainedStoreSlot
        (alignmentIncarnation fixture)
        afterReplay
    )
    ( Store.lookupRetainedStoreSlot
        (alignmentIncarnation fixture)
        afterDuplicate
    )
  let destinationInactive =
        Store.passivateStoreForInvariantTest
          (alignmentDelta fixture)
          afterDuplicate
      retainedBeforeIgnored =
        Store.lookupRetainedStoreSlot
          (alignmentIncarnation fixture)
          destinationInactive
  ignored <-
    checked
      "inactive alignment destination"
      ( Store.prepareAlignmentStoreApplication
          ( Store.peerStoreDestination
              (alignmentDelta fixture)
              (alignmentIncarnation fixture)
              joinedStrength
              :| []
          )
          replayObservation
          destinationInactive
      )
  let (afterIgnored, ignoredOutcomes) = Store.commitPeerStoreApplication ignored
  assertEqual
    "inactive alignment destinations remain terminal rather than retargeted"
    [Store.PeerStoreTerminallyIgnored]
    (fmap Store.peerStoreOutcomeDisposition (toList ignoredOutcomes))
  assertEqual
    "inactive replay preserves retained history"
    retainedBeforeIgnored
    ( Store.lookupRetainedStoreSlot
        (alignmentIncarnation fixture)
        afterIgnored
    )
  assertEqual
    "alignment replay preserves the whole Store invariant"
    (Right ())
    (Store.validateStoreHistoryState afterIgnored)

caseLocalTakeRevision :: Assertion
caseLocalTakeRevision = do
  fixture <- storeFixture
  afterPublication <- applyPeer fixture Weak (state fixture)
  let beforeRevision = revisionFor fixture afterPublication
  prepared <-
    checked
      "local take"
      (Store.prepareExactLocalTake (visibleTakeKeys fixture afterPublication) afterPublication)
  let (afterTake, matches) = Store.commitExactLocalTake prepared
  assertBool "the query exercised visible state" (not (null matches))
  assertEqual
    "visible-only mutation leaves the retained revision fixed"
    beforeRevision
    (revisionFor fixture afterTake)
  assertEqual
    "visible-only mutation leaves the retained change prefix fixed"
    ( Store.retainedStoreChangesAfter
        (incarnation fixture)
        initialStoreRevision
        afterPublication
    )
    ( Store.retainedStoreChangesAfter
        (incarnation fixture)
        initialStoreRevision
        afterTake
    )

caseInactiveHistory :: Assertion
caseInactiveHistory = do
  fixture <- storeFixture
  active <- applyPeer fixture Weak (state fixture)
  before <-
    checked
      "active snapshot"
      ( Store.retainedStoreSnapshotAt
          (incarnation fixture)
          (revisionFor fixture active)
          active
      )
  let inactive = Store.passivateStoreForInvariantTest (delta fixture) active
  assertEqual
    "passivation removes only the active Delta index"
    Nothing
    (Store.lookupStoreSlot (delta fixture) inactive)
  retained <-
    maybe
      (assertFailure "passivation discarded retained Store history")
      pure
      (Store.lookupRetainedStoreSlot (incarnation fixture) inactive)
  assertEqual
    "the inactive Store retains its exact revision"
    (Store.retainedStoreSnapshotRevision before)
    (Store.storeSlotRevision retained)
  after <-
    checked
      "inactive snapshot"
      ( Store.retainedStoreSnapshotAt
          (incarnation fixture)
          (Store.retainedStoreSnapshotRevision before)
          inactive
      )
  assertEqual "inactive history is byte-for-byte semantic state" before after
  _ <- checked "inactive Store history invariant" (Store.validateStoreHistoryState inactive)
  case Store.reactivateRetainedStoreForInvariantTest
    (delta fixture)
    (incarnation fixture)
    inactive of
    Left (Store.StructuralStoreClosedIncarnationReuse actualDelta actualIncarnation) -> do
      assertEqual "closed Delta" (delta fixture) actualDelta
      assertEqual "closed incarnation" (incarnation fixture) actualIncarnation
    Left problem -> assertFailure ("unexpected closed-incarnation result: " <> show problem)
    Right _ -> assertFailure "a closed Store incarnation was reactivated"

caseControlledObjectPurge :: Assertion
caseControlledObjectPurge = do
  fixture <- storeFixture
  predecessor <- applyPeer fixture Weak (state fixture)
  let currentSlot = slotFor fixture predecessor
      targetSort = Store.storeSlotSortId currentSlot
      targetOccurrence = Store.storeSlotOccurrenceId currentSlot
      objectKey = checkedPublicationKey (publication fixture)
      matchingSlots =
        filter
          ((== targetSort) . Store.storeSlotSortId)
          (Store.retainedStoreSlots predecessor)
      matchingIncarnations = fmap Store.storeSlotIncarnation matchingSlots
      expectedPurgedFacts =
        sum
          [ snd
              ( DomainStore.purgeObjectKey
                  objectKey
                  (Store.storeSlotContents slot)
              )
          | slot <- matchingSlots
          ]
  probe <- checked "disappearance probe" (deriveDisappearanceProbeId (controlIndex 91))
  cause <-
    checked
      "disappearance consequence"
      (predefinedDisappearanceCause probe (controlIndex 92))
  prepared <-
    checked
      "controlled object purge"
      ( Store.prepareControlledObjectPurge
          cause
          targetSort
          objectKey
          predecessor
      )
  let (successor, summary) = Store.commitControlledObjectPurge prepared
  assertEqual
    "summary names the target sort"
    targetSort
    (Store.controlledObjectPurgeSummarySortId summary)
  assertEqual
    "summary names the target key"
    objectKey
    (Store.controlledObjectPurgeSummaryObjectKey summary)
  assertEqual
    "every retained incarnation of the sort is matched"
    matchingIncarnations
    (Store.controlledObjectPurgeSummaryMatchedIncarnations summary)
  assertEqual
    "the first purge changes every matching incarnation"
    matchingIncarnations
    (Store.controlledObjectPurgeSummaryAffectedIncarnations summary)
  assertEqual
    "the summary counts all removed Store projection facts"
    expectedPurgedFacts
    (Store.controlledObjectPurgeSummaryPurgedFactCount summary)
  forM_ matchingIncarnations $ \incarnation -> do
    slot <-
      maybe
        (assertFailure "purge lost a retained Store incarnation")
        pure
        (Store.lookupRetainedStoreSlot incarnation successor)
    assertEqual
      "the exact terminal cause is retained"
      (Just cause)
      (Store.storeSlotTerminalObjectPurgeCause objectKey slot)
    assertEqual
      "the visible winner is purged"
      Nothing
      (DomainStore.lookupVisible objectKey (Store.storeSlotContents slot))
    assertEqual
      "the retained winner is purged"
      Nothing
      (DomainStore.lookupRetained objectKey (Store.storeSlotContents slot))
  historical <-
    checked
      "immutable pre-purge Store snapshot"
      ( Store.retainedStoreSnapshotAt
          (incarnation fixture)
          (revisionFor fixture predecessor)
          successor
      )
  assertBool
    "the immutable snapshot transcript remains queryable"
    ( any
        ( (== objectKey)
            . checkedPublicationKey
            . Store.retainedStoreObservationPublication
            . Store.retainedStoreSnapshotFactRepresentative
        )
        (Store.retainedStoreSnapshotFacts historical)
    )
  duplicate <-
    checked
      "duplicate controlled object purge"
      ( Store.prepareControlledObjectPurge
          cause
          targetSort
          objectKey
          successor
      )
  let (afterDuplicate, duplicateSummary) =
        Store.commitControlledObjectPurge duplicate
  assertBool "duplicate purge is state-idempotent" (successor == afterDuplicate)
  assertEqual
    "duplicate purge changes no incarnation"
    []
    (Store.controlledObjectPurgeSummaryAffectedIncarnations duplicateSummary)
  assertEqual
    "duplicate purge removes no fact"
    0
    (Store.controlledObjectPurgeSummaryPurgedFactCount duplicateSummary)
  divergentProbe <-
    checked
      "divergent disappearance probe"
      (deriveDisappearanceProbeId (controlIndex 93))
  divergentCause <-
    checked
      "divergent disappearance consequence"
      (predefinedDisappearanceCause divergentProbe (controlIndex 94))
  case Store.prepareControlledObjectPurge
    divergentCause
    targetSort
    objectKey
    afterDuplicate of
    Left
      ( Store.ControlledObjectPurgeLedgerCauseConflict
          conflictingSort
          conflictingKey
          retainedCause
          incomingCause
        ) -> do
        assertEqual
          "the authoritative terminal ledger reports the conflicting sort"
          targetSort
          conflictingSort
        assertEqual "the conflicting key is exact" objectKey conflictingKey
        assertEqual "the incumbent terminal cause is retained" cause retainedCause
        assertEqual "the divergent terminal cause is rejected" divergentCause incomingCause
    Left problem -> assertFailure ("unexpected divergent purge conflict: " <> show problem)
    Right _ -> assertFailure "a divergent terminal Store-purge cause was accepted"
  later <- laterPublication fixture
  ignored <-
    checked
      "post-terminal peer Store application"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          ( Store.peerStoreDestination
              (delta fixture)
              (incarnation fixture)
              Normal
              :| []
          )
          later
          afterDuplicate
      )
  let (afterIgnored, outcomes) = Store.commitPeerStoreApplication ignored
  assertEqual
    "post-terminal traffic is ignored"
    [Store.PeerStoreTerminallyIgnored]
    (fmap Store.peerStoreOutcomeDisposition (toList outcomes))
  assertBool "ignored traffic changes no Store fact" (afterDuplicate == afterIgnored)
  assertBool
    "ignored traffic retains no receipt"
    ( checkedPublicationId later
        `notElem` Store.storeSlotAppliedPublicationIds (slotFor fixture afterIgnored)
    )
  terminalObservation <- checked "unreceived terminal observation" (Store.retainedStoreObservation later Normal targetOccurrence (Store.RoutedStoreObservation (envelope fixture)))
  unrelated <- unrelatedPublication fixture
  unrelatedObservation <- checked "unrelated unsuppressed observation" (Store.retainedStoreObservation unrelated Normal targetOccurrence (Store.RoutedStoreObservation (envelope fixture)))
  wrongOccurrence <- checked "different terminal query occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xef))
  let originalEnvelope = envelope fixture
      differentEnvelope = Store.storeObservationEnvelope (Store.storeObservationEnvelopeSourceProcess originalEnvelope) wrongOccurrence (Store.storeObservationEnvelopeSourceTopology originalEnvelope) (Store.storeObservationEnvelopeControlPrerequisite originalEnvelope) Nothing
  wrongOccurrenceObservation <- checked "wrong occurrence terminal observation" (Store.retainedStoreObservation later Normal wrongOccurrence (Store.RoutedStoreObservation differentEnvelope))
  let suppressed destination storeIncarnation observation store = Store.currentStoreObservationSuppressed destination storeIncarnation observation store
  assertBool "current terminal query needs no newly retained Store receipt" (suppressed (delta fixture) (incarnation fixture) terminalObservation afterIgnored)
  assertBool "no suppression exists before the actual purge" (not (suppressed (delta fixture) (incarnation fixture) terminalObservation predecessor))
  assertBool "a missing destination cannot witness terminal readiness" (not (suppressed (absent fixture) (absentIncarnation fixture) terminalObservation afterIgnored))
  assertBool "a stale incarnation cannot witness terminal readiness" (not (suppressed (delta fixture) (absentIncarnation fixture) terminalObservation afterIgnored))
  assertBool "an unrelated object is not terminally suppressed" (not (suppressed (delta fixture) (incarnation fixture) unrelatedObservation afterIgnored))
  assertBool "a different occurrence is not suppressed by this current slot" (not (suppressed (delta fixture) (incarnation fixture) wrongOccurrenceObservation afterIgnored))
  inheritedPrepared <-
    checked
      "post-terminal Store bootstrap"
      ( Store.prepareStoreBootstrap
          [ Store.localStoreSpec
              ( Store.HeraldSystemView
                  (checkedLocalHeraldEpoch fixtureCheckedGenesis)
                  SortDefinitionRole
              )
              (absent fixture)
              targetSort
              targetOccurrence
              (absentIncarnation fixture)
          ]
          afterIgnored
      )
  let inherited = Store.commitStoreBootstrap inheritedPrepared
  inheritedSlot <-
    maybe
      (assertFailure "post-terminal Store was not installed")
      pure
      (Store.lookupStoreSlot (absent fixture) inherited)
  assertEqual
    "a later incarnation inherits terminal suppression"
    (Just cause)
    (Store.storeSlotTerminalObjectPurgeCause objectKey inheritedSlot)
  assertEqual
    "a later incarnation cannot restore the retained winner from its base transcript"
    Nothing
    (DomainStore.lookupRetained objectKey (Store.storeSlotContents inheritedSlot))
  assertBool "a newly current inherited incarnation supplies the same exact terminal witness" (suppressed (absent fixture) (absentIncarnation fixture) terminalObservation inherited)
  assertEqual
    "purge overlays preserve the Store history invariant"
    (Right ())
    (Store.validateStoreHistoryState inherited)

caseControlledObjectPurgeBeforeStore :: Assertion
caseControlledObjectPurgeBeforeStore = do
  objectField <- checked "zero-match object field" (mkFieldName "object")
  payloadField <- checked "zero-match payload field" (mkFieldName "payload")
  schema <-
    checked
      "zero-match controlled schema"
      ( recordSchema
          [ (objectField, globalUniqueIdSchema),
            (payloadField, int64Schema)
          ]
      )
  descriptor <-
    canonicalizeCheckedDescriptor
      <$> checked
        "zero-match controlled descriptor"
        ( checkDescriptor
            ApplicationDescriptor
            DescriptorSpec
              { descriptorSpecKind = ControlledSort,
                descriptorSpecSchema = schema,
                descriptorSpecKeyProjections = [directProjection objectField],
                descriptorSpecValidity = AlwaysPredicate,
                descriptorSpecObsolescence = NeverPredicate,
                descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
                descriptorSpecMinimumRetentionMicros = 0,
                descriptorSpecImmutable = False,
                descriptorSpecLabelField = Nothing,
                descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
                descriptorSpecStructuralCarrierRole = Nothing
              }
        )
  object <- checked "zero-match controlled object" (mkGlobalUniqueId (fixtureIdentifierBytes 0xe1))
  value <-
    checked
      "zero-match controlled value"
      ( recordValue
          [ (objectField, globalUniqueIdValue object),
            (payloadField, int64Value 17)
          ]
      )
  writer <- checked "zero-match writer" (mkNablaId (fixtureIdentifierBytes 0xe2))
  let identifier =
        publicationId
          writer
          genesisAuthorityEpoch
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
          (nablaSequence 1)
  publication <-
    checked
      "zero-match controlled publication"
      (mkCheckedPublication descriptor identifier value)
  delta <- checked "zero-match Delta" (mkDeltaId (fixtureIdentifierBytes 0xe3))
  incarnation <-
    checked
      "zero-match Store incarnation"
      (mkStoreIncarnationId (fixtureIdentifierBytes 0xe4))
  occurrence <-
    checked
      "zero-match sort occurrence"
      (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xe5))
  sourceProcess <-
    checked
      "zero-match source process"
      (mkProcessEpochId (fixtureIdentifierBytes 0xe6))
  sourceTopology <-
    checked
      "zero-match source topology"
      (mkTopologyCutId (fixtureIdentifierBytes 0xe7))
  probe <-
    checked
      "zero-match disappearance probe"
      (deriveDisappearanceProbeId (controlIndex 97))
  cause <-
    checked
      "zero-match disappearance consequence"
      (predefinedDisappearanceCause probe (controlIndex 98))
  initial <- checked "zero-match initial Store state" (Store.initialState fixtureCheckedGenesis)
  let targetSort = descriptorSortId descriptor
      targetKey = checkedPublicationKey publication
      matchingBefore =
        filter
          ((== targetSort) . Store.storeSlotSortId)
          (Store.retainedStoreSlots initial)
  assertEqual "the application-defined sort initially has no Store" [] matchingBefore
  preparedPurge <-
    checked
      "zero-match controlled purge"
      (Store.prepareControlledObjectPurge cause targetSort targetKey initial)
  let (purged, purgeSummary) = Store.commitControlledObjectPurge preparedPurge
  assertEqual
    "the purge matches no existing incarnation"
    []
    (Store.controlledObjectPurgeSummaryMatchedIncarnations purgeSummary)
  assertEqual
    "the Store owner retains suppression without an incarnation"
    [((targetSort, targetKey), cause)]
    (Store.storeTerminalObjectPurgeEntries purged)
  preparedBootstrap <-
    checked
      "first post-purge Store bootstrap"
      ( Store.prepareStoreBootstrap
          [ Store.localStoreSpec
              (Store.ApplicationReader sourceProcess SortDefinitionRole)
              delta
              targetSort
              occurrence
              incarnation
          ]
          purged
      )
  let installed = Store.commitStoreBootstrap preparedBootstrap
  slot <-
    maybe
      (assertFailure "the first post-purge Store was not installed")
      pure
      (Store.lookupStoreSlot delta installed)
  assertEqual
    "the first Store incarnation inherits the terminal cause"
    (Just cause)
    (Store.storeSlotTerminalObjectPurgeCause targetKey slot)
  preparedStale <-
    checked
      "stale observation into first post-purge Store"
      ( Store.preparePeerStoreApplication
          ( Store.storeObservationEnvelope
              sourceProcess
              occurrence
              sourceTopology
              (controlIndex 7)
              Nothing
          )
          (Store.peerStoreDestination delta incarnation Normal :| [])
          publication
          installed
      )
  let (afterStale, outcomes) = Store.commitPeerStoreApplication preparedStale
  assertEqual
    "the inherited terminal suppression rejects stale traffic"
    [Store.PeerStoreTerminallyIgnored]
    (fmap Store.peerStoreOutcomeDisposition (toList outcomes))
  assertBool "terminally ignored traffic is state-idempotent" (afterStale == installed)
  assertEqual
    "the zero-match purge and inherited projection preserve Store history"
    (Right ())
    (Store.validateStoreHistoryState afterStale)

caseControlledPurgeHistoryAfterUnrelatedUpdate :: Assertion
caseControlledPurgeHistoryAfterUnrelatedUpdate = do
  fixture <- storeFixture
  withTarget <- applyPeer fixture Weak (state fixture)
  let targetSlot = slotFor fixture withTarget
      targetSort = Store.storeSlotSortId targetSlot
      targetKey = checkedPublicationKey (publication fixture)
  probe <- checked "history-regression disappearance probe" (deriveDisappearanceProbeId (controlIndex 95))
  cause <-
    checked
      "history-regression disappearance consequence"
      (predefinedDisappearanceCause probe (controlIndex 96))
  preparedPurge <-
    checked
      "history-regression controlled object purge"
      ( Store.prepareControlledObjectPurge
          cause
          targetSort
          targetKey
          withTarget
      )
  let (purged, _) = Store.commitControlledObjectPurge preparedPurge
      revisionBeforeUnrelated = revisionFor fixture purged
  unrelated <- unrelatedPublication fixture
  let unrelatedKey = checkedPublicationKey unrelated
  assertBool "the follow-up publication has a distinct object key" (targetKey /= unrelatedKey)
  preparedUnrelated <-
    checked
      "unrelated retained Store update"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          ( Store.peerStoreDestination
              (delta fixture)
              (incarnation fixture)
              Weak
              :| []
          )
          unrelated
          purged
      )
  let (successor, outcomes) = Store.commitPeerStoreApplication preparedUnrelated
      successorSlot = slotFor fixture successor
      successorRevision = revisionFor fixture successor
  assertEqual
    "the unrelated publication performs one retained transition"
    (nextStoreRevision revisionBeforeUnrelated)
    successorRevision
  assertEqual
    "the unrelated publication is applied"
    [Store.PeerStoreApplied]
    (fmap Store.peerStoreOutcomeDisposition (toList outcomes))
  assertEqual
    "the terminally purged key stays absent from live contents"
    Nothing
    (DomainStore.lookupRetained targetKey (Store.storeSlotContents successorSlot))
  assertBool
    "the unrelated key remains present in live contents"
    ( DomainStore.lookupRetained unrelatedKey (Store.storeSlotContents successorSlot)
        /= Nothing
    )
  latestSnapshot <-
    checked
      "latest raw Store snapshot"
      ( Store.retainedStoreSnapshotAt
          (incarnation fixture)
          successorRevision
          successor
      )
  let historicalRepresentatives =
        fmap
          ( Store.retainedStoreObservationPublication
              . Store.retainedStoreSnapshotFactRepresentative
          )
          (Store.retainedStoreSnapshotFacts latestSnapshot)
      historicalKeys = fmap checkedPublicationKey historicalRepresentatives
  assertBool "raw history still contains the terminally purged key" (targetKey `elem` historicalKeys)
  assertBool "raw history contains the unrelated key" (unrelatedKey `elem` historicalKeys)
  assertBool
    "raw history retains the exact pre-purge representative"
    (checkedPublicationId (publication fixture) `elem` fmap checkedPublicationId historicalRepresentatives)
  assertBool
    "raw history records the exact unrelated representative"
    (checkedPublicationId unrelated `elem` fmap checkedPublicationId historicalRepresentatives)
  assertEqual
    "raw replay plus the terminal purge overlay validates"
    (Right ())
    (Store.validateStoreHistoryState successor)

propDuplicateSuffix :: Positive Int -> Property
propDuplicateSuffix (Positive rawCount) = ioProperty $ do
  fixture <- storeFixture
  afterFirst <- applyPeer fixture Weak (state fixture)
  afterDuplicates <-
    foldM
      (\current _ -> applyPeer fixture Weak current)
      afterFirst
      [1 .. min 32 rawCount]
  pure
    ( revisionFor fixture afterDuplicates == revisionFor fixture afterFirst
        && Store.retainedStoreChangesAfter
          (incarnation fixture)
          initialStoreRevision
          afterDuplicates
          == Store.retainedStoreChangesAfter
            (incarnation fixture)
            initialStoreRevision
            afterFirst
        && Store.validateStoreHistoryState afterDuplicates == Right ()
    )

propRevisionIffRetainedChange :: [Word8] -> Property
propRevisionIffRetainedChange rawActions = ioProperty $ do
  fixture <- storeFixture
  (_, valid) <-
    foldM
      (traceStep fixture)
      (state fixture, True)
      (take 48 rawActions)
  pure valid

traceStep ::
  StoreFixture ->
  (Store.State, Bool) ->
  Word8 ->
  IO (Store.State, Bool)
traceStep fixture (predecessor, valid) action
  | action `mod` 3 == 2 = do
      let beforeRevision = revisionFor fixture predecessor
      prepared <-
        checked
          "trace local take"
          (Store.prepareExactLocalTake (visibleTakeKeys fixture predecessor) predecessor)
      let (successor, _) = Store.commitExactLocalTake prepared
      pure
        ( successor,
          valid
            && revisionFor fixture successor == beforeRevision
            && Store.validateStoreHistoryState successor == Right ()
        )
  | otherwise = do
      let incomingStrength = if even action then Weak else Normal
          beforeSlot = slotFor fixture predecessor
          beforeRevision = Store.storeSlotRevision beforeSlot
      (_, expectedResult) <-
        checked
          "domain retained-change projection"
          ( DomainStore.applyPublicationDetailed
              incomingStrength
              (publication fixture)
              (Store.storeSlotContents beforeSlot)
          )
      successor <- applyPeer fixture incomingStrength predecessor
      incomingObservation <-
        checked
          "trace retained observation"
          ( Store.retainedStoreObservation
              (publication fixture)
              incomingStrength
              (Store.storeObservationEnvelopeSortOccurrence (envelope fixture))
              (Store.RoutedStoreObservation (envelope fixture))
          )
      let afterRevision = revisionFor fixture successor
          observationAdded =
            incomingObservation
              `notElem` Store.storeSlotApplicationObservations beforeSlot
          expectedRevision = case expectedResult of
            DomainStore.NoRetainedChange
              | observationAdded -> nextStoreRevision beforeRevision
              | otherwise -> beforeRevision
            DomainStore.RetainedChanged _ -> nextStoreRevision beforeRevision
          revisionChanged = afterRevision /= beforeRevision
          retainedChanged = case expectedResult of
            DomainStore.NoRetainedChange -> False
            DomainStore.RetainedChanged _ -> True
      pure
        ( successor,
          valid
            && afterRevision == expectedRevision
            && revisionChanged == (retainedChanged || observationAdded)
            && Store.validateStoreHistoryState successor == Right ()
        )

visibleTakeKeys :: StoreFixture -> Store.State -> [Store.ExactLocalTakeKey]
visibleTakeKeys fixture current =
  [ Store.exactLocalTakeKey
      (delta fixture)
      (incarnation fixture)
      (checkedPublicationKey publication)
      publication
  | (_, stored) <- DomainStore.visibleInstances (Store.storeSlotContents (slotFor fixture current)),
    let publication = DomainStore.storedPublication stored
  ]

applyPeer :: StoreFixture -> ReplicaStrength -> Store.State -> IO Store.State
applyPeer fixture strength predecessor = do
  prepared <-
    checked
      "peer store application"
      ( Store.preparePeerStoreApplication
          (envelope fixture)
          ( Store.peerStoreDestination
              (delta fixture)
              (incarnation fixture)
              strength
              :| []
          )
          (publication fixture)
          predecessor
      )
  pure (fst (Store.commitPeerStoreApplication prepared))

slotFor :: StoreFixture -> Store.State -> Store.StoreSlot
slotFor fixture current =
  case Store.lookupStoreSlot (delta fixture) current of
    Just slot -> slot
    Nothing -> error "Store fixture lost its active slot"

revisionFor :: StoreFixture -> Store.State -> StoreRevision
revisionFor fixture = Store.storeSlotRevision . slotFor fixture

retainedStoreObservationId :: Store.RetainedStoreObservation -> PublicationId
retainedStoreObservationId =
  checkedPublicationId . Store.retainedStoreObservationPublication

data StoreFixture = StoreFixture
  { state :: Store.State,
    delta :: DeltaId,
    incarnation :: StoreIncarnationId,
    alignmentDelta :: DeltaId,
    alignmentIncarnation :: StoreIncarnationId,
    absent :: DeltaId,
    absentIncarnation :: StoreIncarnationId,
    publication :: CheckedPublication,
    envelope :: Store.StoreObservationEnvelope
  }

storeFixture :: IO StoreFixture
storeFixture = do
  replica <-
    maybe
      (assertFailure "missing primordial sort-definition replica")
      pure
      ( find
          ((== SortDefinitionRole) . primordialReplicaRole)
          (checkedPrimordialReplicas fixtureCheckedGenesis)
      )
  delta <- checked "DeltaId" (mkDeltaId (fixtureIdentifierBytes 201))
  incarnation <-
    checked "StoreIncarnationId" (mkStoreIncarnationId (fixtureIdentifierBytes 202))
  alignmentDelta <-
    checked "alignment DeltaId" (mkDeltaId (fixtureIdentifierBytes 207))
  alignmentIncarnation <-
    checked
      "alignment StoreIncarnationId"
      (mkStoreIncarnationId (fixtureIdentifierBytes 208))
  absent <- checked "absent DeltaId" (mkDeltaId (fixtureIdentifierBytes 203))
  absentIncarnation <-
    checked
      "absent StoreIncarnationId"
      (mkStoreIncarnationId (fixtureIdentifierBytes 204))
  initial <- checked "initial Store" (Store.initialState fixtureCheckedGenesis)
  bootstrap <-
    checked
      "Store bootstrap"
      ( Store.prepareStoreBootstrap
          [ Store.localStoreSpec
              ( Store.HeraldSystemView
                  (checkedLocalHeraldEpoch fixtureCheckedGenesis)
                  SortDefinitionRole
              )
              delta
              (primordialReplicaSortId replica)
              (primordialReplicaOccurrenceId replica)
              incarnation,
            Store.localStoreSpec
              ( Store.HeraldSystemView
                  (checkedLocalHeraldEpoch fixtureCheckedGenesis)
                  SortDefinitionRole
              )
              alignmentDelta
              (primordialReplicaSortId replica)
              (primordialReplicaOccurrenceId replica)
              alignmentIncarnation
          ]
          initial
      )
  let seed = primordialReplicaPublication replica
      seedIdentifier = checkedPublicationId seed
      freshIdentifier =
        publicationId
          (publicationNabla seedIdentifier)
          (publicationAuthorityEpoch seedIdentifier)
          (publicationSourceHeraldEpoch seedIdentifier)
          ( nablaSequence
              (nablaSequenceWord64 (publicationNablaSequence seedIdentifier) + 1000)
          )
  publication <-
    checked
      "fresh checked publication"
      ( mkCheckedPublication
          (primordialReplicaDescriptor replica)
          freshIdentifier
          (checkedPublicationValue seed)
      )
  sourceProcess <-
    checked "source ProcessEpochId" (mkProcessEpochId (fixtureIdentifierBytes 205))
  sourceTopology <-
    checked "source TopologyCutId" (mkTopologyCutId (fixtureIdentifierBytes 206))
  pure
    StoreFixture
      { state = Store.commitStoreBootstrap bootstrap,
        delta = delta,
        incarnation = incarnation,
        alignmentDelta = alignmentDelta,
        alignmentIncarnation = alignmentIncarnation,
        absent = absent,
        absentIncarnation = absentIncarnation,
        publication = publication,
        envelope =
          Store.storeObservationEnvelope
            sourceProcess
            (primordialReplicaOccurrenceId replica)
            sourceTopology
            (controlIndex 7)
            Nothing
      }

laterPublication :: StoreFixture -> IO CheckedPublication
laterPublication fixture = do
  replica <-
    maybe
      (assertFailure "missing primordial sort-definition replica")
      pure
      ( find
          ((== SortDefinitionRole) . primordialReplicaRole)
          (checkedPrimordialReplicas fixtureCheckedGenesis)
      )
  let prior = checkedPublicationId (publication fixture)
      identifier =
        publicationId
          (publicationNabla prior)
          (publicationAuthorityEpoch prior)
          (publicationSourceHeraldEpoch prior)
          (nablaSequence (nablaSequenceWord64 (publicationNablaSequence prior) + 1))
  checked
    "later checked publication"
    ( mkCheckedPublication
        (primordialReplicaDescriptor replica)
        identifier
        (checkedPublicationValue (publication fixture))
    )

unrelatedPublication :: StoreFixture -> IO CheckedPublication
unrelatedPublication fixture = do
  descriptorReplica <-
    maybe
      (assertFailure "missing primordial sort-definition replica")
      pure
      ( find
          ((== SortDefinitionRole) . primordialReplicaRole)
          (checkedPrimordialReplicas fixtureCheckedGenesis)
      )
  unrelatedReplica <-
    maybe
      (assertFailure "missing unrelated primordial definition replica")
      pure
      ( find
          ((/= SortDefinitionRole) . primordialReplicaRole)
          (checkedPrimordialReplicas fixtureCheckedGenesis)
      )
  let prior = checkedPublicationId (publication fixture)
      identifier =
        publicationId
          (publicationNabla prior)
          (publicationAuthorityEpoch prior)
          (publicationSourceHeraldEpoch prior)
          (nablaSequence (nablaSequenceWord64 (publicationNablaSequence prior) + 2))
  checked
    "unrelated checked publication"
    ( mkCheckedPublication
        (primordialReplicaDescriptor descriptorReplica)
        identifier
        (checkedPublicationValue (primordialReplicaPublication unrelatedReplica))
    )

toList :: NonEmpty value -> [value]
toList (first :| rest) = first : rest

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure
