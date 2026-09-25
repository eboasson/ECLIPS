{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module RegularDefinitionStoreRetirementProperties
  ( tests,
  )
where

import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (ApplicationSortDescriptor),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (BoolSchema, RecordSchema),
  )
import Eclips.Application.Types.SortDescriptor qualified as SortSyntax
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    HeraldPublicationPrefix (HeraldPublicationPrefixThrough),
    StoreRevision,
    initialStoreRevision,
    mkHeraldPublicationPosition,
    nextStoreRevision,
  )
import Eclips.Domain.Disappearance
  ( DisappearanceSubject,
    deriveRegularSortOccurrenceClaim,
    regularSortDefinitionDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    TopologyCutId,
    controlIndex,
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
    sortIdBytes,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationKey,
    mkCheckedPublication,
  )
import Eclips.Domain.Route (ReplicaStrength (Normal))
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Profile (sortDefinitionValue)
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Domain.Startup
  ( PredefinedSortRole (DeltaRole, NablaRole, SortDefinitionRole),
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
  )
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Domain.Value
  ( LabelOwner (VoidLabel),
    boolValue,
    bytesValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortDescriptor,
  )
import Eclips.Herald.Genesis.Internal
  ( PrimordialDefinitionReplica,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
    checkedSystemId,
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaPublicationId,
    primordialReplicaRole,
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
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "regular definition Store retirement"
    [ testCase
        "retirement purges only the effective projection"
        caseEffectiveProjectionOnly,
      testCase
        "a stale lower-control reoffer is terminally ignored"
        caseStaleReoffer,
      testCase
        "an equal definition after Resolve is admitted and visible"
        casePostResolveRedefinition,
      testCase
        "a post-Resolve definition can outrank a suppressed cross-writer predecessor by epoch"
        caseLowerPublicationIdRedefinition,
      testCase
        "alignment snapshots suppress stale Nabla and Delta sort references by retirement epoch"
        caseStructuralReferenceSuppression,
      testCase
        "retirement terminally suppresses ordinary values of the retired occurrence"
        caseOrdinaryValueSuppression,
      testCase
        "retirement counts a replaced same-key Store winner as removed work"
        caseReplacedWinnerWorkCount,
      testCase
        "a receipt-only fallback remains revision-addressable for alignment after retirement"
        caseReceiptOnlyFallbackAlignment,
      testCase
        "an unrelated later write cannot resurrect the retired definition"
        caseUnrelatedWriteDoesNotResurrect,
      testCase
        "retirement preserves an unrelated local take in the same Store"
        caseUnrelatedLocalTakeDoesNotResurrect,
      testCase
        "exact retirement replay after redefinition is a whole-state no-op"
        caseReplayAfterRedefinition,
      testProperty
        "classification separates covered, unordered, and post-Resolve writes"
        propWriteClassification,
      testProperty
        "current suppression queries agree with terminal application at every Resolve boundary"
        propCurrentObservationSuppression,
      testProperty
        "retired ordinary occurrences stay terminal at every control prerequisite"
        propOrdinaryValueSuppression,
      testCase
        "a second retirement cycle suppresses only through its later Resolve"
        caseSecondRetirementCycle
    ]

caseEffectiveProjectionOnly :: Assertion
caseEffectiveProjectionOnly = do
  let before = fixturePopulatedState fixture
      beforeSlot = requiredSlot before
      beforeRevision = Store.storeSlotRevision beforeSlot
      beforeReceipts = Store.storeSlotApplicationReceipts beforeSlot
      beforeChanges = historyAfterGenesis before
      beforeSnapshot = snapshotAt beforeRevision before
      (after, summary) = retireFirst before
      afterSlot = requiredSlot after
      afterSnapshot = snapshotAt beforeRevision after
  assertEqual
    "the fixture starts with the definition visible"
    (Just (fixtureOldPublication fixture))
    (visiblePublicationFor (fixtureOldPublication fixture) beforeSlot)
  assertEqual
    "the old definition is absent from the effective visible projection"
    Nothing
    (visiblePublicationFor (fixtureOldPublication fixture) afterSlot)
  assertEqual
    "the old definition is absent from the effective retained projection"
    Nothing
    (retainedPublicationFor (fixtureOldPublication fixture) afterSlot)
  assertEqual
    "retirement does not advance the raw retained revision"
    beforeRevision
    (Store.storeSlotRevision afterSlot)
  assertEqual
    "retirement preserves every immutable receipt"
    beforeReceipts
    (Store.storeSlotApplicationReceipts afterSlot)
  assertEqual
    "retirement preserves the raw retained-change transcript"
    beforeChanges
    (historyAfterGenesis after)
  assertEqual
    "retirement preserves the historical raw snapshot"
    beforeSnapshot
    afterSnapshot
  assertBool
    "the retained snapshot still names the old definition publication"
    (snapshotContains (fixtureOldPublication fixture) afterSnapshot)
  assertEqual
    "the fresh retirement is classified as applied"
    Store.RegularDefinitionRetirementApplied
    (Store.regularDefinitionRetirementSummaryDisposition summary)
  assertBool
    "the sort-definition Store incarnation was affected"
    ( fixtureStoreIncarnation fixture
        `elem` Store.regularDefinitionRetirementSummaryAffectedIncarnations summary
    )
  assertBool
    "at least one effective Store fact was purged"
    (Store.regularDefinitionRetirementSummaryPurgedFactCount summary > 0)
  assertEqual
    "the projected successor remains a valid replay of its raw history"
    (Right ())
    (Store.validateStoreHistoryState after)

caseStaleReoffer :: Assertion
caseStaleReoffer = do
  let (retired, _) = retireFirst (fixturePopulatedState fixture)
      stale = definitionPublicationAt 101 regularDescriptor
      (afterStale, outcomes) = applyDefinition (controlIndex 6) stale retired
  assertEqual
    "the lower-prerequisite write reaches a terminal ignored outcome"
    [Store.PeerStoreTerminallyIgnored]
    outcomes
  assertBool
    "terminal ignore preserves the complete Store owner state"
    (afterStale == retired)
  assertEqual
    "the ignored publication acquires no receipt"
    Nothing
    ( lookup
        (checkedPublicationId stale)
        (Store.storeSlotApplicationReceipts (requiredSlot afterStale))
    )

casePostResolveRedefinition :: Assertion
casePostResolveRedefinition = do
  let (retired, _) = retireFirst (fixturePopulatedState fixture)
      redefinition = definitionPublicationAt 102 regularDescriptor
      (redefined, outcomes) =
        applyDefinition firstResolveIndex redefinition retired
      slot = requiredSlot redefined
  assertEqual
    "the Resolve-qualified definition is applied"
    [Store.PeerStoreApplied]
    outcomes
  assertEqual
    "the redefinition is the effective visible representative"
    (Just redefinition)
    (visiblePublicationFor redefinition slot)
  assertEqual
    "the redefinition is also retained"
    (Just redefinition)
    (retainedPublicationFor redefinition slot)
  assertEqual
    "the new publication has an immutable receipt"
    (Just Normal)
    ( lookup
        (checkedPublicationId redefinition)
        (Store.storeSlotApplicationReceipts slot)
    )
  assertEqual
    "post-Resolve redefinition preserves the Store invariant"
    (Right ())
    (Store.validateStoreHistoryState redefined)

caseLowerPublicationIdRedefinition :: Assertion
caseLowerPublicationIdRedefinition = do
  let old = fixtureOldPublication fixture
      (retired, _) = retireFirst (fixturePopulatedState fixture)
      retiredSlot = requiredSlot retired
      redefinition = lowerWriterDefinitionPublication regularDescriptor
      (redefined, outcomes) =
        applyDefinition firstResolveIndex redefinition retired
      slot = requiredSlot redefined
      revision = Store.storeSlotRevision slot
      rawSnapshot = snapshotAt revision redefined
      alignmentSnapshot =
        checkedPure
          "current alignment-facing Store snapshot"
          ( Store.retainedStoreAlignmentSnapshotAt
              (fixtureStoreIncarnation fixture)
              revision
              redefined
          )
      initialAlignmentSnapshot =
        checkedPure
          "initial alignment-facing Store snapshot"
          ( Store.retainedStoreAlignmentSnapshotAt
              (fixtureStoreIncarnation fixture)
              initialStoreRevision
              redefined
          )
      changes = historyAfterGenesis redefined
      redefinitionChange =
        required
          "lower-ID redefinition alignment change"
          ( find
              ( (== redefinition)
                  . Store.retainedStoreObservationPublication
                  . Store.retainedStoreChangeObservation
              )
              changes
          )
  assertBool
    "the cross-writer redefinition deliberately sorts below the raw predecessor"
    (checkedPublicationId redefinition < checkedPublicationId old)
  assertEqual
    "the Resolve-qualified lower-ID definition is applied"
    [Store.PeerStoreApplied]
    outcomes
  assertEqual
    "an effective-only successor advances the alignment revision"
    (nextStoreRevision (Store.storeSlotRevision retiredSlot))
    revision
  assertEqual
    "the revisioned change records that the raw transcript did not advance"
    Nothing
    (Store.retainedStoreChangeTransition redefinitionChange)
  assertBool
    "the immutable raw snapshot retains the higher-ID predecessor"
    ( snapshotContains old rawSnapshot
        && not (snapshotContains redefinition rawSnapshot)
    )
  assertBool
    "a transfer based before the successor can send it as a change"
    ( not (snapshotContains redefinition initialAlignmentSnapshot)
        && any
          ( (== redefinition)
              . Store.retainedStoreObservationPublication
              . Store.retainedStoreChangeObservation
          )
          changes
    )
  assertBool
    "a transfer based at the successor revision snapshots the new epoch"
    ( snapshotContains redefinition alignmentSnapshot
        && not (snapshotContains old alignmentSnapshot)
    )
  assertEqual
    "the new occurrence nevertheless selects the lower-ID definition effectively"
    (Just redefinition)
    (visiblePublicationFor redefinition slot)
  assertBool
    "accepted-observation evidence retains the raw loser"
    ( any
        ( (== redefinition)
            . Store.retainedStoreObservationPublication
        )
        (Store.storeSlotApplicationObservations slot)
    )
  assertEqual
    "the cross-writer effective projection remains replay-valid"
    (Right ())
    (Store.validateStoreHistoryState redefined)

caseStructuralReferenceSuppression :: Assertion
caseStructuralReferenceSuppression =
  mapM_ exercise [NablaRole, DeltaRole]
  where
    exercise role = do
      let carrier = carrierFor role
          localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
          system = checkedSystemId fixtureCheckedGenesis
          delta = deriveSystemViewDeltaId system localHerald role
          incarnation =
            deriveSystemViewStoreIncarnationId system localHerald role
          occurrence = primordialReplicaOccurrenceId carrier
          stalePublication = structuralReferencePublicationAt role 200
          laterStalePublication = structuralReferencePublicationAt role 201
          (populated, initialOutcomes) =
            applyDefinitionTo
              delta
              incarnation
              occurrence
              (fixtureSourceProcess fixture)
              (fixtureSourceTopology fixture)
              (controlIndex 3)
              stalePublication
              (fixtureInitialState fixture)
          populatedSlot = requiredSlotAt delta populated
          populatedRevision = Store.storeSlotRevision populatedSlot
          (retired, retirementSummary) = retireFirst populated
          retiredSlot = requiredSlotAt delta retired
          rawRetiredSnapshot = snapshotAtIncarnation incarnation populatedRevision retired
          alignedRetiredSnapshot =
            alignmentSnapshotAt incarnation populatedRevision retired
          (afterLaterStale, laterStaleOutcomes) =
            applyDefinitionTo
              delta
              incarnation
              occurrence
              (fixtureSourceProcess fixture)
              (fixtureSourceTopology fixture)
              (controlIndex 6)
              laterStalePublication
              retired
          (reprojected, successorOutcomes) =
            applyDefinitionTo
              delta
              incarnation
              occurrence
              (fixtureSourceProcess fixture)
              (fixtureSourceTopology fixture)
              firstResolveIndex
              stalePublication
              afterLaterStale
          reprojectedSlot = requiredSlotAt delta reprojected
          reprojectedRevision = Store.storeSlotRevision reprojectedSlot
          alignedSuccessorSnapshot =
            alignmentSnapshotAt incarnation reprojectedRevision reprojected
      assertEqual
        (show role <> " reference is initially applied")
        [Store.PeerStoreApplied]
        initialOutcomes
      assertEqual
        (show role <> " reference is initially effective")
        (Just stalePublication)
        (visiblePublicationFor stalePublication populatedSlot)
      assertEqual
        (show role <> " stale reference is removed from the effective Store")
        Nothing
        (visiblePublicationFor stalePublication retiredSlot)
      assertBool
        (show role <> " Store participates in retirement reprojection")
        ( incarnation
            `elem` Store.regularDefinitionRetirementSummaryAffectedIncarnations
              retirementSummary
        )
      assertBool
        (show role <> " raw historical snapshot remains auditable")
        (snapshotContains stalePublication rawRetiredSnapshot)
      assertBool
        (show role <> " alignment snapshot omits the stale reference")
        (not (snapshotContains stalePublication alignedRetiredSnapshot))
      assertEqual
        (show role <> " later lower-prerequisite reference is terminal")
        [Store.PeerStoreTerminallyIgnored]
        laterStaleOutcomes
      assertBool
        (show role <> " terminal reference changes no Store state")
        (afterLaterStale == retired)
      assertEqual
        (show role <> " same publication is re-admitted at Resolve")
        [Store.PeerStoreApplied]
        successorOutcomes
      assertEqual
        (show role <> " Resolve-qualified replay is effective again")
        (Just stalePublication)
        (visiblePublicationFor stalePublication reprojectedSlot)
      assertBool
        (show role <> " successor alignment snapshot contains the reference")
        (snapshotContains stalePublication alignedSuccessorSnapshot)
      assertEqual
        (show role <> " structural reference history remains replay-valid")
        (Right ())
        (Store.validateStoreHistoryState reprojected)

caseOrdinaryValueSuppression :: Assertion
caseOrdinaryValueSuppression = do
  let delta =
        checkedPure
          "ordinary retired-value Delta"
          (mkDeltaId (fixtureIdentifierBytes 0xf0))
      incarnation =
        checkedPure
          "ordinary retired-value Store incarnation"
          (mkStoreIncarnationId (fixtureIdentifierBytes 0xf1))
      occurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixtureCheckedGenesis)
          (descriptorSortId regularDescriptor)
          Genesis
      bootstrapped =
        Store.commitStoreBootstrap
          ( checkedPure
              "bootstrap regular destination Store"
              ( Store.prepareStoreBootstrap
                  [ Store.localStoreSpec
                      (Store.ApplicationReader (fixtureSourceProcess fixture) SortDefinitionRole)
                      delta
                      (descriptorSortId regularDescriptor)
                      occurrence
                      incarnation
                  ]
                  (fixturePopulatedState fixture)
              )
          )
      original = ordinaryPublicationAt 260 True
      staleBeforeResolve = ordinaryPublicationAt 261 False
      staleAtResolve = ordinaryPublicationAt 262 True
      (populated, initialOutcomes) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          (controlIndex 3)
          original
          bootstrapped
      populatedSlot = requiredSlotAt delta populated
      populatedRevision = Store.storeSlotRevision populatedSlot
      rawSnapshot = snapshotAtIncarnation incarnation populatedRevision populated
      (retired, retirementSummary) = retireFirst populated
      retiredSlot = requiredSlotAt delta retired
      alignmentSnapshot = alignmentSnapshotAt incarnation populatedRevision retired
      (afterStaleBeforeResolve, beforeResolveOutcomes) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          (controlIndex 6)
          staleBeforeResolve
          retired
      (afterStaleAtResolve, atResolveOutcomes) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          firstResolveIndex
          staleAtResolve
          afterStaleBeforeResolve
  assertEqual
    "the ordinary predecessor value is initially applied"
    [Store.PeerStoreApplied]
    initialOutcomes
  assertEqual
    "the ordinary predecessor value is initially visible"
    (Just original)
    (visiblePublicationFor original populatedSlot)
  assertEqual
    "regular retirement removes the ordinary value from the effective Store"
    Nothing
    (visiblePublicationFor original retiredSlot)
  assertBool
    "regular retirement accounts for the ordinary-value Store"
    ( incarnation
        `elem` Store.regularDefinitionRetirementSummaryAffectedIncarnations
          retirementSummary
    )
  assertBool
    "the raw historical snapshot remains auditable"
    (snapshotContains original rawSnapshot)
  assertBool
    "the alignment-facing snapshot omits the retired ordinary value"
    (not (snapshotContains original alignmentSnapshot))
  assertEqual
    "a delayed pre-Resolve ordinary value is terminally ignored"
    [Store.PeerStoreTerminallyIgnored]
    beforeResolveOutcomes
  assertEqual
    "an old-occurrence value stays terminal even with the Resolve prerequisite"
    [Store.PeerStoreTerminallyIgnored]
    atResolveOutcomes
  assertBool
    "terminal ordinary reoffers leave the Store owner unchanged"
    (afterStaleAtResolve == retired)
  assertEqual
    "ordinary-value suppression remains replay-valid"
    (Right ())
    (Store.validateStoreHistoryState afterStaleAtResolve)

caseReplacedWinnerWorkCount :: Assertion
caseReplacedWinnerWorkCount = do
  let role = NablaRole
      carrier = carrierFor role
      localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      system = checkedSystemId fixtureCheckedGenesis
      delta = deriveSystemViewDeltaId system localHerald role
      incarnation =
        deriveSystemViewStoreIncarnationId system localHerald role
      occurrence = primordialReplicaOccurrenceId carrier
      sharedObjectTag = 233
      fallback =
        structuralReferencePublicationFor
          role
          210
          sharedObjectTag
          False
          (descriptorSortId alternateDescriptor)
      retiredWinner =
        structuralReferencePublicationFor
          role
          211
          sharedObjectTag
          True
          (descriptorSortId regularDescriptor)
      (withFallback, _) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          (controlIndex 3)
          fallback
          (fixtureInitialState fixture)
      (withWinner, _) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          (controlIndex 3)
          retiredWinner
          withFallback
      beforeSlot = requiredSlotAt delta withWinner
      (retired, summary) = retireFirst withWinner
      afterSlot = requiredSlotAt delta retired
  assertEqual
    "the target reference deliberately wins the shared object key before retirement"
    (Just retiredWinner)
    (visiblePublicationFor retiredWinner beforeSlot)
  assertEqual
    "suppressing the target reference reveals the unrelated retained fallback"
    (Just fallback)
    (visiblePublicationFor fallback afterSlot)
  assertEqual
    "the replaced visible, retained, and logical facts each count as removed"
    3
    (Store.regularDefinitionRetirementSummaryPurgedFactCount summary)
  assertEqual
    "same-key fallback reprojection remains replay-valid"
    (Right ())
    (Store.validateStoreHistoryState retired)

caseReceiptOnlyFallbackAlignment :: Assertion
caseReceiptOnlyFallbackAlignment = do
  let role = NablaRole
      carrier = carrierFor role
      localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      system = checkedSystemId fixtureCheckedGenesis
      delta = deriveSystemViewDeltaId system localHerald role
      incarnation =
        deriveSystemViewStoreIncarnationId system localHerald role
      occurrence = primordialReplicaOccurrenceId carrier
      sharedObjectTag = 234
      fallback =
        structuralReferencePublicationFor
          role
          212
          sharedObjectTag
          False
          (descriptorSortId regularDescriptor)
      retiredWinner =
        structuralReferencePublicationFor
          role
          213
          sharedObjectTag
          True
          (descriptorSortId regularDescriptor)
      (withWinner, _) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          (controlIndex 3)
          retiredWinner
          (fixtureInitialState fixture)
      winnerRevision = Store.storeSlotRevision (requiredSlotAt delta withWinner)
      (withLowFallback, lowFallbackOutcomes) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          (controlIndex 3)
          fallback
          withWinner
      lowFallbackRevision =
        Store.storeSlotRevision (requiredSlotAt delta withLowFallback)
      (withFallback, fallbackOutcomes) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          firstResolveIndex
          fallback
          withLowFallback
      fallbackSlot = requiredSlotAt delta withFallback
      fallbackRevision = Store.storeSlotRevision fallbackSlot
      fallbackChange =
        required
          "receipt-only fallback revision"
          ( find
              ( \change ->
                  Store.retainedStoreChangeRevision change == fallbackRevision
                    && Store.retainedStoreObservationPublication
                      (Store.retainedStoreChangeObservation change)
                      == fallback
              )
              ( checkedPure
                  "receipt-only fallback changes"
                  ( Store.retainedStoreChangesAfter
                      incarnation
                      winnerRevision
                      withFallback
                  )
              )
          )
      (retired, _) = retireFirst withFallback
      retiredSlot = requiredSlotAt delta retired
      aligned = alignmentSnapshotAt incarnation fallbackRevision retired
  assertBool
    "the unrelated fallback deliberately sorts below the retired winner"
    (checkedPublicationId fallback < checkedPublicationId retiredWinner)
  assertEqual
    "the first low-control fallback receipt is accepted even though it loses the current projection"
    [Store.PeerStoreApplied]
    lowFallbackOutcomes
  assertEqual
    "the Resolve-qualified provenance observation is independently accepted"
    [Store.PeerStoreApplied]
    fallbackOutcomes
  assertEqual
    "the losing logical fact first advances the raw revision"
    (nextStoreRevision winnerRevision)
    lowFallbackRevision
  assertEqual
    "the receipt-only provenance observation advances the alignment revision again"
    (nextStoreRevision lowFallbackRevision)
    fallbackRevision
  assertEqual
    "the receipt-only observation leaves the raw transcript unchanged"
    Nothing
    (Store.retainedStoreChangeTransition fallbackChange)
  assertEqual
    "the higher-ranked structural reference remains current before retirement"
    (Just retiredWinner)
    (visiblePublicationFor retiredWinner fallbackSlot)
  assertEqual
    "retiring the winner reveals the receipt-only unrelated fallback"
    (Just fallback)
    (visiblePublicationFor fallback retiredSlot)
  assertBool
    "the exact current alignment snapshot carries the revealed fallback"
    ( snapshotContains fallback aligned
        && not (snapshotContains retiredWinner aligned)
    )
  assertEqual
    "receipt-only history remains replay-valid after retirement"
    (Right ())
    (Store.validateStoreHistoryState retired)

caseUnrelatedWriteDoesNotResurrect :: Assertion
caseUnrelatedWriteDoesNotResurrect = do
  let (retired, _) = retireFirst (fixturePopulatedState fixture)
      unrelated = definitionPublicationAt 103 alternateDescriptor
      (afterUnrelated, outcomes) =
        applyDefinition (controlIndex 8) unrelated retired
      slot = requiredSlot afterUnrelated
  assertEqual
    "the unrelated descriptor publication is applied"
    [Store.PeerStoreApplied]
    outcomes
  assertEqual
    "the unrelated descriptor becomes visible"
    (Just unrelated)
    (visiblePublicationFor unrelated slot)
  assertEqual
    "reprojection after unrelated work does not resurrect the old definition"
    Nothing
    (visiblePublicationFor (fixtureOldPublication fixture) slot)
  assertEqual
    "the raw history still contains both definition publications"
    [ fixtureOldPublication fixture,
      unrelated
    ]
    (fmap Store.retainedStoreObservationPublication (historyObservations afterUnrelated))
  assertEqual
    "the unrelated successor remains internally replayable"
    (Right ())
    (Store.validateStoreHistoryState afterUnrelated)

caseUnrelatedLocalTakeDoesNotResurrect :: Assertion
caseUnrelatedLocalTakeDoesNotResurrect = do
  let unrelated = definitionPublicationAt 108 alternateDescriptor
      (withBoth, outcomes) =
        applyDefinition (controlIndex 4) unrelated (fixturePopulatedState fixture)
      takeKey =
        Store.exactLocalTakeKey
          (fixtureDelta fixture)
          (fixtureStoreIncarnation fixture)
          (checkedPublicationKey unrelated)
          unrelated
      preparedTake =
        checkedPure
          "prepare unrelated exact local take"
          (Store.prepareExactLocalTake [takeKey] withBoth)
      (afterTake, taken) = Store.commitExactLocalTake preparedTake
      (retired, _) = retireFirst afterTake
      retiredSlot = requiredSlot retired
  assertEqual
    "the unrelated definition is established before the take"
    [Store.PeerStoreApplied]
    outcomes
  assertEqual
    "the exact unrelated publication is taken"
    [unrelated]
    taken
  assertEqual
    "the local take hides but retains the unrelated value"
    (Nothing, Just unrelated)
    ( visiblePublicationFor unrelated (requiredSlot afterTake),
      retainedPublicationFor unrelated (requiredSlot afterTake)
    )
  assertEqual
    "retirement does not resurrect the unrelated taken value"
    (Nothing, Just unrelated)
    ( visiblePublicationFor unrelated retiredSlot,
      retainedPublicationFor unrelated retiredSlot
    )
  assertEqual
    "the target definition is still removed by retirement"
    Nothing
    (visiblePublicationFor (fixtureOldPublication fixture) retiredSlot)
  assertEqual
    "the locally hidden successor remains replay-valid"
    (Right ())
    (Store.validateStoreHistoryState retired)

caseReplayAfterRedefinition :: Assertion
caseReplayAfterRedefinition = do
  let (retired, _) = retireFirst (fixturePopulatedState fixture)
      redefinition = definitionPublicationAt 104 regularDescriptor
      (redefined, outcomes) =
        applyDefinition firstResolveIndex redefinition retired
      replayPrepared =
        checkedPure
          "exact retirement replay"
          ( Store.prepareRegularDefinitionRetirement
              firstSubject
              firstLocalCut
              firstResolveIndex
              redefined
          )
      replaySummary =
        Store.preparedRegularDefinitionRetirementSummary replayPrepared
      (replayed, committedSummary) =
        Store.commitRegularDefinitionRetirement replayPrepared
  assertEqual
    "the fixture really performed a post-Resolve redefinition"
    [Store.PeerStoreApplied]
    outcomes
  assertEqual
    "preparation exposes exact replay before commit"
    Store.RegularDefinitionRetirementExactReplay
    (Store.regularDefinitionRetirementSummaryDisposition replaySummary)
  assertEqual
    "commit preserves the exact replay disposition"
    Store.RegularDefinitionRetirementExactReplay
    (Store.regularDefinitionRetirementSummaryDisposition committedSummary)
  assertBool
    "an exact replay after redefinition preserves the complete owner"
    (replayed == redefined)
  assertEqual
    "the replay cannot erase the post-Resolve definition"
    (Just redefinition)
    (visiblePublicationFor redefinition (requiredSlot replayed))
  assertEqual
    "the replay reports no physical work"
    ([], [], 0)
    ( Store.regularDefinitionRetirementSummaryMatchedIncarnations replaySummary,
      Store.regularDefinitionRetirementSummaryAffectedIncarnations replaySummary,
      Store.regularDefinitionRetirementSummaryPurgedFactCount replaySummary
    )

propWriteClassification :: Word8 -> Word8 -> Property
propWriteClassification rawCut rawLater =
  let cutValue = 1 + fromIntegral rawCut
      laterValue = cutValue + 1 + fromIntegral rawLater
      localCut = publicationPrefix cutValue
      coveredPosition = publicationPosition cutValue
      laterPosition = publicationPosition laterValue
      resolveIndex = controlIndex (laterValue + 1)
      prepared =
        Store.prepareRegularDefinitionRetirement
          firstSubject
          localCut
          resolveIndex
          (fixtureInitialState fixture)
   in case prepared of
        Left problem ->
          counterexample ("retirement preparation rejected: " <> show problem) False
        Right accepted ->
          let (retired, _) = Store.commitRegularDefinitionRetirement accepted
              suppression =
                required
                  "classifier retirement suppression"
                  (Store.lookupRegularRetirementSuppression firstSubject retired)
           in conjoin
                [ Store.classifyRegularDefinitionWrite
                    firstSubject
                    (Just coveredPosition)
                    (controlIndex 0)
                    (fixtureInitialState fixture)
                    === Store.RegularDefinitionWriteUnsuppressed,
                  Store.classifyRegularDefinitionWrite
                    firstSubject
                    (Just coveredPosition)
                    (controlIndex 0)
                    retired
                    === Store.RegularDefinitionWriteCoveredByCut suppression,
                  Store.classifyRegularDefinitionWrite
                    firstSubject
                    (Just laterPosition)
                    (controlIndex 0)
                    retired
                    === Store.RegularDefinitionWriteMissingResolvePrerequisite suppression,
                  Store.classifyRegularDefinitionWrite
                    firstSubject
                    Nothing
                    (controlIndex 0)
                    retired
                    === Store.RegularDefinitionWriteMissingResolvePrerequisite suppression,
                  Store.classifyRegularDefinitionWrite
                    firstSubject
                    (Just coveredPosition)
                    resolveIndex
                    retired
                    === Store.RegularDefinitionWriteAdmissibleAfterResolve suppression
                ]

-- Readiness must use the owner's exact current suppression classification.
-- In particular it cannot turn an unrelated or post-Resolve observation into
-- terminal work simply because some older definition has been retired.
propCurrentObservationSuppression :: Word8 -> Word8 -> Property
propCurrentObservationSuppression rawResolve rawPrerequisite =
  let resolveIndex = controlIndex (1 + fromIntegral rawResolve)
      prerequisite = controlIndex (fromIntegral rawPrerequisite)
      initial = fixtureInitialState fixture
      (retired, _) = Store.commitRegularDefinitionRetirement (checkedPure "query retirement" (Store.prepareRegularDefinitionRetirement firstSubject firstLocalCut resolveIndex initial))
      publication = definitionPublicationAt 401 regularDescriptor
      occurrence = fixtureCarrierOccurrence fixture
      observationAt definition occurrenceId control = checkedPure "query observation" (Store.retainedStoreObservation definition Normal occurrenceId (Store.RoutedStoreObservation (Store.storeObservationEnvelope (fixtureSourceProcess fixture) occurrenceId (fixtureSourceTopology fixture) control Nothing)))
      observation = observationAt publication occurrence prerequisite
      queried candidate = Store.currentStoreObservationSuppressed (fixtureDelta fixture) (fixtureStoreIncarnation fixture) candidate retired
      expected = prerequisite < resolveIndex
      (applied, outcomes) = applyDefinition prerequisite publication retired
      absent = checkedPure "absent query Delta" (mkDeltaId (fixtureIdentifierBytes 0xec))
      stale = checkedPure "stale query incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 0xed))
      wrongOccurrence = checkedPure "different query occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xee))
   in conjoin
        [ queried observation === expected,
          outcomes === [if expected then Store.PeerStoreTerminallyIgnored else Store.PeerStoreApplied],
          (if expected then applied == retired else applied /= retired) === True,
          Store.currentStoreObservationSuppressed (fixtureDelta fixture) (fixtureStoreIncarnation fixture) observation initial === False,
          Store.currentStoreObservationSuppressed absent (fixtureStoreIncarnation fixture) observation retired === False,
          Store.currentStoreObservationSuppressed (fixtureDelta fixture) stale observation retired === False,
          queried (observationAt publication wrongOccurrence prerequisite) === False,
          queried (observationAt (ordinaryPublicationAt 403 True) occurrence prerequisite) === False,
          queried (observationAt publication occurrence resolveIndex) === False,
          queried (observationAt (definitionPublicationAt 402 alternateDescriptor) occurrence prerequisite) === False,
          (lookup (checkedPublicationId publication) (Store.storeSlotApplicationReceipts (requiredSlot retired)) == Nothing) === True
        ]

propOrdinaryValueSuppression :: Word8 -> Property
propOrdinaryValueSuppression rawPrerequisite =
  let delta =
        checkedPure
          "property ordinary retired-value Delta"
          (mkDeltaId (fixtureIdentifierBytes 0xf2))
      incarnation =
        checkedPure
          "property ordinary retired-value Store incarnation"
          (mkStoreIncarnationId (fixtureIdentifierBytes 0xf3))
      occurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixtureCheckedGenesis)
          (descriptorSortId regularDescriptor)
          Genesis
      bootstrapped =
        Store.commitStoreBootstrap
          ( checkedPure
              "bootstrap property ordinary destination Store"
              ( Store.prepareStoreBootstrap
                  [ Store.localStoreSpec
                      (Store.ApplicationReader (fixtureSourceProcess fixture) SortDefinitionRole)
                      delta
                      (descriptorSortId regularDescriptor)
                      occurrence
                      incarnation
                  ]
                  (fixturePopulatedState fixture)
              )
          )
      (retired, _) = retireFirst bootstrapped
      prerequisite = controlIndex (fromIntegral rawPrerequisite)
      publication =
        ordinaryPublicationAt
          (300 + fromIntegral rawPrerequisite)
          (even rawPrerequisite)
      (successor, outcomes) =
        applyDefinitionTo
          delta
          incarnation
          occurrence
          (fixtureSourceProcess fixture)
          (fixtureSourceTopology fixture)
          prerequisite
          publication
          retired
   in counterexample
        ("control prerequisite: " <> show prerequisite)
        ( conjoin
            [ outcomes === [Store.PeerStoreTerminallyIgnored],
              counterexample
                "terminal ordinary observation changed Store state"
                (successor == retired)
            ]
        )

caseSecondRetirementCycle :: Assertion
caseSecondRetirementCycle = do
  let (afterFirstRetirement, _) = retireFirst (fixturePopulatedState fixture)
      firstRedefinition = definitionPublicationAt 105 regularDescriptor
      (afterFirstRedefinition, firstOutcomes) =
        applyDefinition firstResolveIndex firstRedefinition afterFirstRetirement
      secondResolveIndex = controlIndex 13
      secondSubject = subjectAfter firstResolveIndex
      secondPrepared =
        checkedPure
          "second regular Store retirement"
          ( Store.prepareRegularDefinitionRetirement
              secondSubject
              (publicationPrefix 9)
              secondResolveIndex
              afterFirstRedefinition
          )
      (afterSecondRetirement, secondSummary) =
        Store.commitRegularDefinitionRetirement secondPrepared
      staleSecond = definitionPublicationAt 106 regularDescriptor
      (afterStaleSecond, staleOutcomes) =
        applyDefinition (controlIndex 12) staleSecond afterSecondRetirement
      secondRedefinition = definitionPublicationAt 107 regularDescriptor
      (afterSecondRedefinition, secondOutcomes) =
        applyDefinition secondResolveIndex secondRedefinition afterStaleSecond
  assertEqual
    "the first redefinition is established before its own retirement"
    [Store.PeerStoreApplied]
    firstOutcomes
  assertEqual
    "the second retirement removes the first successor definition"
    Nothing
    ( visiblePublicationFor
        firstRedefinition
        (requiredSlot afterSecondRetirement)
    )
  assertEqual
    "the second suppression is freshly installed"
    Store.RegularDefinitionRetirementApplied
    (Store.regularDefinitionRetirementSummaryDisposition secondSummary)
  assertEqual
    "a write between the two Resolve indices is stale for the second cycle"
    [Store.PeerStoreTerminallyIgnored]
    staleOutcomes
  assertBool
    "the stale second-cycle write changes no Store state"
    (afterStaleSecond == afterSecondRetirement)
  assertEqual
    "the second Resolve admits the next equal definition"
    [Store.PeerStoreApplied]
    secondOutcomes
  assertEqual
    "the latest equal definition is visible"
    (Just secondRedefinition)
    ( visiblePublicationFor
        secondRedefinition
        (requiredSlot afterSecondRedefinition)
    )
  assertEqual
    "both occurrence-qualified retirement suppressions remain retained"
    2
    (length (Store.regularRetirementSuppressions afterSecondRedefinition))
  assertEqual
    "the two-cycle state remains replay-valid"
    (Right ())
    (Store.validateStoreHistoryState afterSecondRedefinition)

data Fixture = Fixture
  { fixtureInitialState :: Store.State,
    fixturePopulatedState :: Store.State,
    fixtureDelta :: DeltaId,
    fixtureStoreIncarnation :: StoreIncarnationId,
    fixtureCarrierOccurrence :: SortDefinitionOccurrenceId,
    fixtureSourceProcess :: ProcessEpochId,
    fixtureSourceTopology :: TopologyCutId,
    fixtureOldPublication :: CheckedPublication
  }

fixture :: Fixture
fixture =
  Fixture
    { fixtureInitialState = initial,
      fixturePopulatedState = populated,
      fixtureDelta = delta,
      fixtureStoreIncarnation = incarnation,
      fixtureCarrierOccurrence = primordialReplicaOccurrenceId sortDefinitionCarrier,
      fixtureSourceProcess = sourceProcess,
      fixtureSourceTopology = sourceTopology,
      fixtureOldPublication = oldPublication
    }
  where
    initial =
      checkedPure
        "initial Store"
        (Store.initialState fixtureCheckedGenesis)
    localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
    system = checkedSystemId fixtureCheckedGenesis
    delta = deriveSystemViewDeltaId system localHerald SortDefinitionRole
    incarnation =
      deriveSystemViewStoreIncarnationId system localHerald SortDefinitionRole
    sourceProcess =
      checkedPure
        "definition source process"
        (mkProcessEpochId (fixtureIdentifierBytes 220))
    sourceTopology =
      checkedPure
        "definition source topology"
        (mkTopologyCutId (fixtureIdentifierBytes 221))
    oldPublication = definitionPublicationAt 100 regularDescriptor
    populated =
      fst
        ( applyDefinitionTo
            delta
            incarnation
            (primordialReplicaOccurrenceId sortDefinitionCarrier)
            sourceProcess
            sourceTopology
            (controlIndex 3)
            oldPublication
            initial
        )

firstSubject :: DisappearanceSubject
firstSubject =
  regularSortDefinitionDisappearanceSubject
    ( deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureCheckedGenesis)
        regularDescriptor
        Genesis
    )

subjectAfter :: ControlIndex -> DisappearanceSubject
subjectAfter priorResolve =
  regularSortDefinitionDisappearanceSubject
    ( deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureCheckedGenesis)
        regularDescriptor
        ( checkedPure
            "resolved retirement occurrence base"
            (resolvedRetirementOccurrenceBase priorResolve)
        )
    )

firstResolveIndex :: ControlIndex
firstResolveIndex = controlIndex 7

firstLocalCut :: HeraldPublicationPrefix
firstLocalCut = publicationPrefix 5

retireFirst :: Store.State -> (Store.State, Store.RegularDefinitionRetirementSummary)
retireFirst state =
  Store.commitRegularDefinitionRetirement
    ( checkedPure
        "first regular Store retirement"
        ( Store.prepareRegularDefinitionRetirement
            firstSubject
            firstLocalCut
            firstResolveIndex
            state
        )
    )

applyDefinition ::
  ControlIndex ->
  CheckedPublication ->
  Store.State ->
  (Store.State, [Store.PeerStoreDisposition])
applyDefinition =
  applyDefinitionTo
    (fixtureDelta fixture)
    (fixtureStoreIncarnation fixture)
    (fixtureCarrierOccurrence fixture)
    (fixtureSourceProcess fixture)
    (fixtureSourceTopology fixture)

applyDefinitionTo ::
  DeltaId ->
  StoreIncarnationId ->
  SortDefinitionOccurrenceId ->
  ProcessEpochId ->
  TopologyCutId ->
  ControlIndex ->
  CheckedPublication ->
  Store.State ->
  (Store.State, [Store.PeerStoreDisposition])
applyDefinitionTo delta incarnation occurrence process topology prerequisite publication state =
  let prepared =
        checkedPure
          "definition Store application"
          ( Store.preparePeerStoreApplication
              ( Store.storeObservationEnvelope
                  process
                  occurrence
                  topology
                  prerequisite
                  Nothing
              )
              (Store.peerStoreDestination delta incarnation Normal :| [])
              publication
              state
          )
      (successor, outcomes) = Store.commitPeerStoreApplication prepared
   in (successor, fmap Store.peerStoreOutcomeDisposition (toList outcomes))

requiredSlot :: Store.State -> Store.StoreSlot
requiredSlot state =
  requiredSlotAt (fixtureDelta fixture) state

requiredSlotAt :: DeltaId -> Store.State -> Store.StoreSlot
requiredSlotAt delta state =
  required
    "sort-definition system-view Store"
    (Store.lookupStoreSlot delta state)

visiblePublicationFor ::
  CheckedPublication -> Store.StoreSlot -> Maybe CheckedPublication
visiblePublicationFor publication slot =
  DomainStore.storedPublication
    <$> DomainStore.lookupVisible
      (checkedPublicationKey publication)
      (Store.storeSlotContents slot)

retainedPublicationFor ::
  CheckedPublication -> Store.StoreSlot -> Maybe CheckedPublication
retainedPublicationFor publication slot =
  DomainStore.storedPublication
    <$> DomainStore.lookupRetained
      (checkedPublicationKey publication)
      (Store.storeSlotContents slot)

historyAfterGenesis :: Store.State -> [Store.RetainedStoreChange]
historyAfterGenesis state =
  checkedPure
    "retained Store changes"
    ( Store.retainedStoreChangesAfter
        (fixtureStoreIncarnation fixture)
        initialStoreRevision
        state
    )

historyObservations :: Store.State -> [Store.RetainedStoreObservation]
historyObservations = fmap Store.retainedStoreChangeObservation . historyAfterGenesis

snapshotAt :: StoreRevision -> Store.State -> Store.RetainedStoreSnapshot
snapshotAt revision state =
  snapshotAtIncarnation (fixtureStoreIncarnation fixture) revision state

snapshotAtIncarnation ::
  StoreIncarnationId ->
  StoreRevision ->
  Store.State ->
  Store.RetainedStoreSnapshot
snapshotAtIncarnation incarnation revision state =
  checkedPure
    "retained Store snapshot"
    ( Store.retainedStoreSnapshotAt
        incarnation
        revision
        state
    )

alignmentSnapshotAt ::
  StoreIncarnationId ->
  StoreRevision ->
  Store.State ->
  Store.RetainedStoreSnapshot
alignmentSnapshotAt incarnation revision state =
  checkedPure
    "alignment-facing Store snapshot"
    (Store.retainedStoreAlignmentSnapshotAt incarnation revision state)

snapshotContains :: CheckedPublication -> Store.RetainedStoreSnapshot -> Bool
snapshotContains publication =
  any
    ( (== checkedPublicationId publication)
        . checkedPublicationId
        . Store.retainedStoreObservationPublication
        . Store.retainedStoreSnapshotFactRepresentative
    )
    . Store.retainedStoreSnapshotFacts

publicationPrefix :: Word64 -> HeraldPublicationPrefix
publicationPrefix = HeraldPublicationPrefixThrough . publicationPosition

publicationPosition :: Word64 -> HeraldPublicationPosition
publicationPosition =
  checkedPure "Herald publication position" . mkHeraldPublicationPosition

regularDescriptor, alternateDescriptor :: CanonicalDescriptor
regularDescriptor = admittedDescriptor "key"
alternateDescriptor = admittedDescriptor "alternate_key"

admittedDescriptor :: Text -> CanonicalDescriptor
admittedDescriptor field =
  admittedApplicationSortDescriptor
    ( checkedPure
        "regular application descriptor"
        ( admitApplicationSortDefinition
            ( DeclaredSortDefinition
                ApplicationSortDescriptor
                  { SortSyntax.sortKind = RegularSort,
                    SortSyntax.valueSchema =
                      RecordSchema (Map.singleton field BoolSchema),
                    SortSyntax.keyProjections =
                      [ApplicationProjection (field :| [])],
                    SortSyntax.validityPredicate = AlwaysPredicate,
                    SortSyntax.obsolescencePredicate = NeverPredicate,
                    SortSyntax.rankTerms =
                      RankApplicationValue Ascending :| [],
                    SortSyntax.minimumRetentionMicros = 0,
                    SortSyntax.isImmutable = False,
                    SortSyntax.labelField = Nothing
                  }
                Nothing
            )
        )
    )

definitionPublicationAt :: Word64 -> CanonicalDescriptor -> CheckedPublication
definitionPublicationAt offset descriptor =
  checkedPure
    "checked regular sort-definition publication"
    ( mkCheckedPublication
        (primordialReplicaDescriptor sortDefinitionCarrier)
        (freshPublicationId offset)
        (sortDefinitionValue descriptor)
    )

ordinaryPublicationAt :: Word64 -> Bool -> CheckedPublication
ordinaryPublicationAt offset key =
  checkedPure
    "checked ordinary regular publication"
    ( mkCheckedPublication
        regularDescriptor
        (freshPublicationId offset)
        ( checkedPure
            "ordinary regular publication value"
            (recordValue [(checkedPure "ordinary key field" (mkFieldName "key"), boolValue key)])
        )
    )

structuralReferencePublicationAt ::
  PredefinedSortRole -> Word64 -> CheckedPublication
structuralReferencePublicationAt role offset =
  structuralReferencePublicationFor
    role
    offset
    (case role of NablaRole -> 230; DeltaRole -> 231; _ -> 232)
    False
    (descriptorSortId regularDescriptor)

structuralReferencePublicationFor ::
  PredefinedSortRole ->
  Word64 ->
  Word8 ->
  Bool ->
  SortId ->
  CheckedPublication
structuralReferencePublicationFor role offset objectTag selfSequenced referencedSort =
  checkedPure
    (show role <> " checked structural reference publication")
    ( mkCheckedPublication
        (primordialReplicaDescriptor carrier)
        (freshPublicationId offset)
        value
    )
  where
    carrier = carrierFor role
    object =
      checkedPure
        (show role <> " structural reference object")
        (mkGlobalUniqueId (fixtureIdentifierBytes objectTag))
    objectField = checkedPure "structural object field" (mkFieldName "object_id")
    labelField = checkedPure "structural label field" (mkFieldName "label")
    sortField = checkedPure "structural sort field" (mkFieldName "sort_id")
    sequencingField =
      checkedPure
        "structural sequencing field"
        (mkFieldName "sequencing_object")
    commonFields =
      [ (objectField, globalUniqueIdValue object),
        (labelField, labelValue (VoidLabel, 0)),
        (sortField, bytesValue (sortIdBytes referencedSort))
      ]
    fields = case role of
      NablaRole ->
        ( sequencingField,
          optionalGlobalUniqueIdValue
            (if selfSequenced then Just object else Nothing)
        )
          : commonFields
      _ -> commonFields
    value =
      checkedPure
        (show role <> " structural reference value")
        (recordValue fields)

lowerWriterDefinitionPublication :: CanonicalDescriptor -> CheckedPublication
lowerWriterDefinitionPublication descriptor =
  checkedPure
    "checked lower-writer regular sort-definition publication"
    ( mkCheckedPublication
        (primordialReplicaDescriptor sortDefinitionCarrier)
        ( publicationId
            (checkedPure "lower definition NablaId" (mkNablaId (fixtureIdentifierBytes 0)))
            (publicationAuthorityEpoch seed)
            (publicationSourceHeraldEpoch seed)
            (nablaSequence (nablaSequenceWord64 (publicationNablaSequence seed) + 1000))
        )
        (sortDefinitionValue descriptor)
    )
  where
    seed = primordialReplicaPublicationId sortDefinitionCarrier

freshPublicationId :: Word64 -> PublicationId
freshPublicationId offset =
  publicationId
    (publicationNabla seed)
    (publicationAuthorityEpoch seed)
    (publicationSourceHeraldEpoch seed)
    (nablaSequence (nablaSequenceWord64 (publicationNablaSequence seed) + offset))
  where
    seed = primordialReplicaPublicationId sortDefinitionCarrier

sortDefinitionCarrier :: PrimordialDefinitionReplica
sortDefinitionCarrier =
  carrierFor SortDefinitionRole

carrierFor :: PredefinedSortRole -> PrimordialDefinitionReplica
carrierFor role =
  required
    ("primordial carrier " <> show role)
    ( find
        ((== role) . primordialReplicaRole)
        (checkedPrimordialReplicas fixtureCheckedGenesis)
    )

required :: String -> Maybe value -> value
required description =
  maybe (error (description <> ": missing fixture value")) id

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure description =
  either (error . ((description <> ": ") <>) . show) id
