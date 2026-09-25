{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StructuralReconciliationProperties
  ( tests,
  )
where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.List (find, permutations)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import Eclips.Domain.Graph
  ( EdgeStrength (..),
    VertexId (..),
    edgeDestination,
    edgeSource,
    edgeStrengthSymbol,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    StructuralSequence,
    controlIndex,
    genesisAuthorityEpoch,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    globalUniqueIdFromGlobalObjectId,
    mkDeltaId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkStoreIncarnationId,
    mkStructuralSequence,
    mkTopologyCutId,
    nablaSequence,
    publicationId,
    sortIdBytes,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
  )
import Eclips.Domain.Label
  ( ReleasedLabelState,
    releasedDeleted,
    releasedLabel,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    PublicationWinnerKey,
    checkedPublicationId,
    checkedPublicationWinnerKey,
    mkCheckedPublication,
  )
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (..))
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    allPredefinedSortRoles,
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profileSortFor,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRoot,
    AppliedRootRole (..),
    appliedProcessEnvironmentObjects,
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootObjectId,
    appliedRootRole,
    checkedInitialTopologyEdges,
    checkedInitialTopologyVertices,
    predefinedOccurrenceFor,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    projectStructuralVersionVector,
    structuralPrefixThrough,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    labelReleaseCause,
    processEndCause,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value
  ( FieldName,
    Label,
    LabelOwner (..),
    Value,
    boolValue,
    bytesValue,
    enumValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.Genesis.Internal
  ( checkedActiveHeraldEpochs,
    checkedInitialBootstraps,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedPredefinedOccurrenceSet,
    checkedSystemId,
  )
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.PeerPublication qualified as Peer
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    StructuralDebtKind (..),
    StructuralIdentity (..),
    normalizeStructuralDebts,
    qualifiedPlacementFact,
    qualifiedPlacementSort,
    qualifiedStoreIncarnation,
    qualifiedStoreSort,
    sortOccurrence,
    sortOccurrenceDefinition,
    structuralConsequenceDebt,
    structuralConsequenceDebtEvidence,
    structuralConsequenceDebtKey,
    structuralDebtEvidence,
    structuralDebtKey,
    structuralDebtKeyKind,
    structuralDebtKeySort,
    structuralDebtKnownPlacements,
    structuralDebtPredecessorResources,
    structuralDebtSetEntries,
    structuralDebtSetNull,
  )
import Eclips.Herald.Structural.Reconciliation
  ( AppliedControlledProjection,
    BaselineStructuralWinner,
    ControllerProjection (..),
    DynamicLocalStoreSpec,
    FreshStoreIncarnationEvidence,
    FreshStoreIncarnationSource (..),
    PreparedStructuralReconciliation,
    ReconciliationViews,
    RootActivityProjection (..),
    StructuralApplication,
    StructuralApplicationClassification (..),
    StructuralAppliedInvariantProblem (..),
    StructuralAppliedState,
    StructuralControlBasePreparation (..),
    StructuralControlOverlayView (..),
    StructuralControlTarget (..),
    StructuralDependency (..),
    StructuralPreparation (..),
    StructuralProjectionProvenance (..),
    StructuralProjectionSnapshot,
    StructuralReconciliationProblem (..),
    StructuralRetirementSuppression (..),
    StructuralVertexProjection (..),
    appliedControlledProjectionObject,
    appliedControlledProjectionPublication,
    appliedDynamicDeltaControlPrerequisite,
    appliedDynamicDeltaController,
    appliedDynamicDeltaDelta,
    appliedDynamicDeltaObject,
    appliedDynamicDeltaOccurrence,
    appliedDynamicDeltaSort,
    baselineStructuralWinner,
    captureStructuralCarrierBase,
    captureStructuralControlBase,
    controlledPatchObservation,
    controlledPatchWinnerChange,
    dropStructuralControlOverlayForInvariantTest,
    dynamicLocalStoreSpec,
    dynamicStoreIncarnation,
    dynamicStoreIncarnationSource,
    dynamicStoreProcess,
    dynamicStoreSort,
    emptyStructuralAppliedState,
    establishStructuralAppliedMembership,
    exactChangeAfter,
    exactChangeBefore,
    freshStoreIncarnationEvidence,
    lookupPreparedNablaProjection,
    ownedEdgeProjectionPayload,
    ownedEdgeProjectionPublication,
    placementPatchChanges,
    prepareCarriedStructuralReconciliation,
    prepareNablaProjectionsAt,
    prepareRetirementSuppressedStructuralReconciliation,
    prepareStructuralCarrierBaseImport,
    prepareStructuralControlBaseImport,
    prepareStructuralFrontierSummary,
    prepareStructuralLabelControlRefresh,
    prepareStructuralProcessEndRefreshes,
    prepareStructuralProjectionAtAdmittedVector,
    prepareStructuralProjectionAtVector,
    prepareStructuralProjectionRefresh,
    prepareStructuralReconciliation,
    prepareStructuralReconciliationAtAdmittedPredecessor,
    prepareUnchangedStructuralLabelControlRefresh,
    preparedStructuralCarrierBaseGraphPatch,
    preparedStructuralCarrierBasePredecessor,
    preparedStructuralCarrierBaseSuccessor,
    preparedStructuralChangesState,
    preparedStructuralClassification,
    preparedStructuralControlledPatch,
    preparedStructuralDebts,
    preparedStructuralGraphPatch,
    preparedStructuralPlacementPatch,
    preparedStructuralPredecessor,
    preparedStructuralSatisfiedDependencies,
    preparedStructuralStorePatch,
    preparedStructuralSuccessor,
    reconciliationViews,
    reconciliationViewsWithEndedProcesses,
    relocateStructuralControlHistoryForInvariantTest,
    restrictStructuralControlBaseToObjects,
    storePatchChanges,
    storePatchRetainedResourceChanges,
    structuralApplication,
    structuralApplicationControlPrerequisite,
    structuralApplicationOccurrence,
    structuralApplicationPublication,
    structuralAppliedBaselineStoreMatches,
    structuralAppliedControlHistory,
    structuralAppliedControlOverlays,
    structuralAppliedDynamicDeltaCoordinates,
    structuralAppliedEdgeProjectionAt,
    structuralAppliedEdgeProjections,
    structuralAppliedHistoryOccurrences,
    structuralAppliedLocalStores,
    structuralAppliedOccurrenceStoreResources,
    structuralAppliedRetainedStoreResources,
    structuralAppliedRetirementSuppressionAt,
    structuralAppliedStateWithBaseline,
    structuralAppliedStateWithGenesis,
    structuralAppliedStateWithStores,
    structuralAppliedUsedStoreIncarnations,
    structuralAppliedVertexProjectionAt,
    structuralAppliedVertexProjections,
    structuralCarrierBaseControlBase,
    structuralCarrierBasePublications,
    structuralControlBaseControlIndices,
    structuralControlBaseGreatestControlIndex,
    structuralControlBaseHistory,
    structuralControlBaseObjects,
    structuralControlBaseOverlays,
    structuralControlBaseStepCause,
    structuralControlBaseStepPreparation,
    structuralControlCheckpointAt,
    structuralFrontierControlPrerequisite,
    structuralGraphEdgeChanges,
    structuralGraphPatchNull,
    structuralGraphVertexChanges,
    structuralNablaProjectionAt,
    structuralProjectionDigestEntryControlCause,
    structuralProjectionSnapshotCanonicalBytes,
    structuralProjectionSnapshotDeletedControls,
    structuralProjectionSnapshotDigestEntries,
    structuralProjectionSnapshotEdges,
    structuralProjectionSnapshotVertices,
    structuralRetirementSuppressionControlPrerequisite,
    structuralVertexProjectionOccurrence,
    structuralVertexProjectionPublication,
    structuralVertexProjectionVertex,
    validateStructuralAppliedState,
    validateStructuralFrontier,
    validateStructuralFrontierExhaustive,
    withReconciliationDiagnosticChecks,
  )
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureCheckedInitialBootstraps,
    fixtureGenesisRetirementLineage,
    fixtureIdentifierBytes,
    fixtureRetirementResolution,
    fixtureStep14CheckedGenesis,
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
    counterexample,
    testProperty,
    (===),
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "structural reconciliation preparation"
    [ testCase "an edge uses only endpoints covered by its predecessor vector" caseCoveredEdge,
      testCase
        "an Edge inherits retirement suppression only from a covered endpoint occurrence"
        caseCoveredEndpointRetirementSuppression,
      testCase
        "a concurrent same-object suppression cannot raise an Edge's inherited floor"
        caseNoncausalSameObjectRetirementSuppression,
      testCase
        "an Edge inherits the maximum floor across two covered suppressed endpoints"
        caseMaximumEndpointRetirementSuppression,
      testCase
        "retirement-suppression replay preserves its original occurrence-local floor"
        caseRetirementSuppressionReplay,
      testCase "baseline winners resolve together and participate in updates" caseBaselineUpdate,
      testCase "baseline publication and object-role identity is preserved" caseBaselineIdentityGuards,
      testCase "an unrepresented checked-genesis vertex cannot become an implicit winner" caseUnrepresentedBaseline,
      testCase "an ahead winner does not change an older causal projection" caseConcurrentProjection,
      testCase "a causally dominated occurrence skips mutable dependencies" caseDominatedFastPath,
      testCase "a nonwinner still validates closed nominal fields" caseMalformedNonwinner,
      testCase "nabla derives controller and local Normal activity" caseNablaReadiness,
      testCase "process-epoch carrier is excluded" caseProcessCarrierRejected,
      testCase "delta activation retains exact source-qualified resource" caseDeltaActivation,
      testCase "a hidden causal delta retains metadata without active placement" caseHiddenDeltaResource,
      testCase "hidden same-sort occurrence refresh preserves current leaves and exact debt" caseHiddenOccurrenceRefresh,
      testCase "same-occurrence refresh passivates and reactivates freshly" caseProjectionRefresh,
      testCase "same-occurrence refresh preserves admitted root meanings and controller residence" caseProjectionRefreshRootMeaning,
      testCase "same-occurrence refresh preserves admitted endpoint roles" caseProjectionRefreshEdgeMeaning,
      testCase "seeded stores reject remote and duplicate incarnations" caseStoreSeedChecks,
      testCase "an active baseline delta requires its exact local Store" caseBaselineActiveStore,
      testCase "same-definition forwarding preserves the carried sort in cutover debt" caseCarriedSortForward,
      testCase "debt evidence is exact-sort qualified" casePerSortDebtEvidence,
      testCase "causal shape checks source, membership, and covered dots" caseCausalShapeChecks,
      testCase "historical projection rejects a non-causally-closed vector" caseProjectionClosure,
      testCase "disabled diagnostics preserve structural input admission" caseDiagnosticProjectionAdmission,
      testProperty "admitted projections and local preparations agree in both diagnostic modes" propAdmittedProjectionDiagnostics,
      testProperty "source summaries preserve exhaustive frontier admission and prefix controls across import" propStructuralFrontierSummaries,
      testCase "established membership projection preserves surviving predecessor dependencies" caseDescendantProjectionClosure,
      testCase "canonical topology projection excludes Herald-local root activity" caseCanonicalActivityIndependent,
      testCase "paired carrier/control import preserves handover before End without synthesizing End" caseCarrierBaseEndOrder,
      testCase "paired carrier import preserves original resolved sort occurrence" caseCarrierBaseOriginalSort,
      testCase "equal absent topology ignores redundant deletion provenance" caseKnownVersusSuppressedOriginal,
      testProperty "post-deletion topology equality is independent of deletion and retirement cuts" propKnownVersusSuppressedOriginal,
      testCase "paired carrier import preserves suppressed and dominated missing meanings" caseCarrierBaseSuppressedAndDominated,
      testCase "paired carrier import verifies fresh genesis and discards donor Stores" caseCarrierBaseGenesis,
      testCase "control base import retains exact history with receiver-only Store resources" caseControlBaseResources,
      testCase "control base import holds for missing carriers and retains suppressed history" caseControlBaseDependencies,
      testProperty "control base import matches ordered labels at every historical cut" propControlBaseHistory,
      testCase "label controls retain the first genuine controller cause until a later handoff" caseLabelControlHandoff,
      testCase "label deletion is terminal topology material" caseLabelControlDelete,
      testCase "checked genesis roots fold ordered handoff and delete controls into exact prefix projections" caseGenesisControlPrefixes,
      testCase "process End refreshes every and only dynamically controlled occurrence" caseProcessEndControls,
      testCase "every genesis environment object accepts ordinary handoff and deletion" caseGenesisEnvironmentControls,
      testCase "process End passivates checked genesis endpoints without fabricating occurrences" caseGenesisProcessEndControls,
      testCase "replay is a complete no-op" caseReplay,
      testCase "delayed roots retain historical End before a newer handoff or deletion" caseDelayedEndControlHistory,
      testCase "End leaves delayed Void, Zombie and another process roots unchanged" caseDelayedEndUnrelated,
      testCase "delayed delta uses End of its released controller rather than its intrinsic owner" caseDelayedReleasedControllerEnd,
      testProperty "delayed End roots preserve intrinsic history and allocate no dead Store" propDelayedProcessEnd,
      testProperty "batched Nabla projections preserve exact historical coordinates under reordered repeated questions" propPreparedNablaProjections,
      testProperty "same-controller label controls never churn the structural owner" propSameControllerControlNoChurn,
      testCase "unchanged label projections bypass complete views for local, passive and remote roots" caseUnchangedLabelProjectionMatrix,
      testCase "genesis authority-only labels match full reconciliation without constructing complete views" caseUnchangedGenesisLabels,
      testCase "changed activity, controller and Store incarnation cannot take the unchanged label path" caseChangedLabelProjectionBoundaries,
      testProperty "indexed label refresh preserves repeated controls and replacement provenance" propIndexedLabelControlHistory,
      testProperty "debt normalization is order-independent and occurrence-separating" propDebtNormalization,
      testProperty "concurrent publications of one controlled object converge in both orders" propConcurrentSameObject,
      testProperty "independent concurrent occurrences converge under every order" propIndependentPermutation
    ]

caseCoveredEdge :: Assertion
caseCoveredEdge = do
  let source = object 10
      destination = object 11
      edgeObject = object 12
      sourceApplication = app remoteA 1 emptyVector NeutralVertexRole 1 (neutralValue source)
      edge predecessor =
        app localHerald 1 predecessor EdgeRole 2 (edgeValue edgeObject source destination Preserve)
      currentViews = views [NeutralVertex destination] [] []
  sourcePrepared <- prepareReady currentViews sourceApplication Nothing emptyState
  assertEqual
    "neutral establishes its endpoint and exact committed occurrence"
    ( Set.fromList
        [ StructuralEndpointDependency source,
          StructuralPredecessorDependency (structuralOccurrence sourceApplication)
        ]
    )
    (preparedStructuralSatisfiedDependencies sourcePrepared)
  let afterSource = preparedStructuralSuccessor sourcePrepared
  assertHeld
    "uncovered concurrent endpoint"
    (Set.singleton (StructuralEndpointDependency source))
    (prepareStructuralReconciliation currentViews (edge emptyVector) Nothing afterSource)
  prepared <-
    prepareReady
      currentViews
      (edge (vectorThrough [(remoteA, 1)]))
      Nothing
      afterSource
  assertEqual
    "one exact owned edge change"
    [edgeObject]
    (Map.keys (structuralGraphEdgeChanges (preparedStructuralGraphPatch prepared)))
  assertEqual
    "an edge establishes its exact committed occurrence"
    ( Set.singleton
        ( StructuralPredecessorDependency
            (structuralOccurrence (edge (vectorThrough [(remoteA, 1)])))
        )
    )
    (preparedStructuralSatisfiedDependencies prepared)
  assertBool "cutover debt is explicit" (hasDebt CutoverDebt prepared)

caseCoveredEndpointRetirementSuppression :: Assertion
caseCoveredEndpointRetirementSuppression = do
  let endpoint = object 13
      destination = object 14
      edgeObject = object 15
      endpointApplication =
        app
          remoteA
          1
          emptyVector
          NablaRole
          3
          (nablaRootValue endpoint (VoidLabel, 0) dataSort)
      suppression =
        DirectRegularSortRetirementSuppression dataSort (controlIndex 4)
      edgeApplication predecessor =
        app
          localHerald
          1
          predecessor
          EdgeRole
          4
          (edgeValue edgeObject endpoint destination Preserve)
      inherited =
        InheritedEndpointRetirementSuppression
          (structuralOccurrence endpointApplication)
          (controlIndex 4)
  state <-
    applyRetirementSuppressedReady
      suppression
      (views [] [] [])
      endpointApplication
      emptyState
  assertEqual
    "an uncovered retained endpoint does not suppress the Edge"
    (Right Nothing)
    ( structuralRetirementSuppressionControlPrerequisite
        emptyVector
        (structuralPublication (edgeApplication emptyVector))
        state
    )
  assertEqual
    "covering the exact suppressed endpoint inherits its floor"
    (Right (Just inherited))
    ( structuralRetirementSuppressionControlPrerequisite
        (vectorThrough [(remoteA, 1)])
        (structuralPublication (edgeApplication (vectorThrough [(remoteA, 1)])))
        state
    )

caseNoncausalSameObjectRetirementSuppression :: Assertion
caseNoncausalSameObjectRetirementSuppression = do
  let endpoint = object 16
      destination = object 17
      edgeObject = object 18
      causal =
        app
          remoteA
          1
          emptyVector
          NablaRole
          5
          (nablaRootValue endpoint (VoidLabel, 0) dataSort)
      noncausal =
        app
          remoteB
          1
          emptyVector
          NablaRole
          6
          (nablaRootValue endpoint (VoidLabel, 0) dataSort)
      causalSuppression =
        DirectRegularSortRetirementSuppression dataSort (controlIndex 5)
      noncausalSuppression =
        DirectRegularSortRetirementSuppression dataSort (controlIndex 50)
      predecessor = vectorThrough [(remoteA, 1)]
      edgeApplication =
        app
          localHerald
          1
          predecessor
          EdgeRole
          7
          (edgeValue edgeObject endpoint destination Preserve)
  afterCausal <-
    applyRetirementSuppressedReady
      causalSuppression
      (views [] [] [])
      causal
      emptyState
  afterConcurrent <-
    applyRetirementSuppressedReady
      noncausalSuppression
      (views [] [] [])
      noncausal
      afterCausal
  assertEqual
    "the uncovered concurrent occurrence cannot contribute its higher floor"
    ( Right
        ( Just
            ( InheritedEndpointRetirementSuppression
                (structuralOccurrence causal)
                (controlIndex 5)
            )
        )
    )
    ( structuralRetirementSuppressionControlPrerequisite
        predecessor
        (structuralPublication edgeApplication)
        afterConcurrent
    )

caseMaximumEndpointRetirementSuppression :: Assertion
caseMaximumEndpointRetirementSuppression = do
  let source = object 19
      destination = object 20
      edgeObject = object 21
      sourceApplication =
        app
          remoteA
          1
          emptyVector
          NablaRole
          8
          (nablaRootValue source (VoidLabel, 0) dataSort)
      destinationApplication =
        app
          remoteB
          1
          emptyVector
          NablaRole
          9
          (nablaRootValue destination (VoidLabel, 0) dataSort)
      sourceSuppression =
        DirectRegularSortRetirementSuppression dataSort (controlIndex 11)
      destinationSuppression =
        DirectRegularSortRetirementSuppression dataSort (controlIndex 6)
      predecessor = vectorThrough [(remoteA, 1), (remoteB, 1)]
      edgeApplication =
        app
          localHerald
          1
          predecessor
          EdgeRole
          10
          (edgeValue edgeObject source destination Preserve)
  afterSource <-
    applyRetirementSuppressedReady
      sourceSuppression
      (views [] [] [])
      sourceApplication
      emptyState
  afterDestination <-
    applyRetirementSuppressedReady
      destinationSuppression
      (views [] [] [])
      destinationApplication
      afterSource
  assertEqual
    "the maximum floor wins rather than the later endpoint candidate"
    ( Right
        ( Just
            ( InheritedEndpointRetirementSuppression
                (structuralOccurrence sourceApplication)
                (controlIndex 11)
            )
        )
    )
    ( structuralRetirementSuppressionControlPrerequisite
        predecessor
        (structuralPublication edgeApplication)
        afterDestination
    )

caseRetirementSuppressionReplay :: Assertion
caseRetirementSuppressionReplay = do
  let endpoint = object 23
      endpointApplication =
        app
          remoteA
          1
          emptyVector
          NablaRole
          11
          (nablaRootValue endpoint (VoidLabel, 0) dataSort)
      original =
        DirectRegularSortRetirementSuppression dataSort (controlIndex 7)
      later =
        DirectRegularSortRetirementSuppression dataSort (controlIndex 13)
      currentViews = views [] [] []
  state <-
    applyRetirementSuppressedReady
      original
      currentViews
      endpointApplication
      emptyState
  replayed <-
    prepareRetirementSuppressedReady
      later
      currentViews
      endpointApplication
      state
  assertEqual
    "replay is a whole-state no-op"
    state
    (preparedStructuralSuccessor replayed)
  assertEqual
    "the occurrence keeps the retirement floor under which it was first admitted"
    (Just original)
    ( structuralAppliedRetirementSuppressionAt
        (structuralOccurrence endpointApplication)
        (preparedStructuralSuccessor replayed)
    )

caseBaselineUpdate :: Assertion
caseBaselineUpdate = do
  let source = object 20
      destination = object 21
      edgeObject = object 22
      edgeValue' = edgeValue edgeObject source destination Preserve
      baselineEdge = publication remoteA EdgeRole 50 edgeValue'
      supplied =
        Map.fromList
          [ (source, baseline NeutralVertexRole remoteA 1 (neutralValue source)),
            (destination, baseline NeutralVertexRole remoteA 2 (neutralValue destination)),
            (edgeObject, baselineStructuralWinner (carrierOccurrence EdgeRole) baselineEdge)
          ]
      currentViews = views [] [] []
      low = app localHerald 1 emptyVector EdgeRole 10 edgeValue'
      high = app localHerald 1 emptyVector EdgeRole 80 edgeValue'
  initial <- baselineState currentViews supplied Map.empty
  assertEqual
    "baseline edge resolves through controlled baseline vertices"
    (Just (Just (checkedPublicationId baselineEdge)))
    (ownedEdgeProjectionPublication <$> Map.lookup edgeObject (structuralAppliedEdgeProjections initial))
  assertBool "fixture low loses" (winnerKey low < checkedPublicationWinnerKey baselineEdge)
  lowPrepared <- prepareReady currentViews low Nothing initial
  assertEqual "lower candidate" StructuralApplicationAppliedNonWinner (preparedStructuralClassification lowPrepared)
  assertBool "lower graph no-op" (structuralGraphPatchNull (preparedStructuralGraphPatch lowPrepared))
  assertBool "lower causal no-op" (structuralDebtSetNull (preparedStructuralDebts lowPrepared))

  assertBool "fixture high wins" (winnerKey high > checkedPublicationWinnerKey baselineEdge)
  highPrepared <- prepareReady currentViews high Nothing initial
  before <- winnerBefore highPrepared
  assertEqual "exact baseline predecessor" (Just (checkedPublicationId baselineEdge)) (appliedControlledProjectionPublication before)
  assertEqual
    "stronger update owns one edge change"
    [edgeObject]
    (Map.keys (structuralGraphEdgeChanges (preparedStructuralGraphPatch highPrepared)))

caseBaselineIdentityGuards :: Assertion
caseBaselineIdentityGuards = do
  let subject = object 30
      endpoint = object 31
      baselinePublication = publication remoteA NeutralVertexRole 7 (neutralValue subject)
      supplied =
        Map.singleton
          subject
          (baselineStructuralWinner (carrierOccurrence NeutralVertexRole) baselinePublication)
      currentViews = views [NeutralVertex endpoint] [] []
  initial <- baselineState currentViews supplied Map.empty
  let reused =
        structuralApplication
          (occurrence remoteA 1)
          emptyVector
          (carrierOccurrence NeutralVertexRole)
          baselinePublication
          (controlIndex 0)
  case prepareStructuralReconciliation currentViews reused Nothing initial of
    Left (StructuralPublicationBaselineConflict actual) ->
      assertEqual "baseline publication" (checkedPublicationId baselinePublication) actual
    other -> assertFailure ("unexpected baseline reuse: " <> show other)
  let wrongRole =
        app localHerald 1 emptyVector EdgeRole 8 (edgeValue subject endpoint endpoint Preserve)
  case prepareStructuralReconciliation currentViews wrongRole Nothing initial of
    Left (StructuralObjectRoleConflict actual NeutralVertexCarrier EdgeCarrier) ->
      assertEqual "role object" subject actual
    other -> assertFailure ("unexpected baseline role check: " <> show other)

caseUnrepresentedBaseline :: Assertion
caseUnrepresentedBaseline = do
  let nabla = checked "baseline nabla" (mkNablaId (fixtureIdentifierBytes 32))
      subject = globalObjectIdFromNablaId nabla
      currentViews = views [NablaVertex nabla] [] []
      candidate =
        app
          localHerald
          1
          emptyVector
          NablaRole
          8
          (nablaRootValue subject (VoidLabel, 0) dataSort)
  case prepareStructuralReconciliation currentViews candidate Nothing emptyState of
    Left (StructuralUnrepresentedBaselineObject actual) ->
      assertEqual "checked baseline object" subject actual
    other ->
      assertFailure
        ("unrepresented baseline was treated as an absent winner: " <> show other)

caseConcurrentProjection :: Assertion
caseConcurrentProjection = do
  let source = object 40
      destination = object 41
      edgeObject = object 42
      value = edgeValue edgeObject source destination Preserve
      low = app remoteA 1 emptyVector EdgeRole 10 value
      high = app remoteB 1 emptyVector EdgeRole 20 value
      currentViews = views [NeutralVertex source, NeutralVertex destination] [] []
  assertBool "fixture full winner" (winnerKey high > winnerKey low)
  afterHigh <- applyReady currentViews high Nothing emptyState
  lowPrepared <- prepareReady currentViews low Nothing afterHigh
  let successor = preparedStructuralSuccessor lowPrepared
  assertEqual "full-current nonwinner" StructuralApplicationAppliedNonWinner (preparedStructuralClassification lowPrepared)
  assertBool "current graph unchanged" (structuralGraphPatchNull (preparedStructuralGraphPatch lowPrepared))
  assertBool "older cut still has topology debt" (hasDebt TopologyAlignmentDebt lowPrepared)
  assertEqual
    "current remains B"
    (Just (checkedPublicationId (structuralPublication high)))
    (ownedEdgeProjectionPublication (structuralAppliedEdgeProjections successor Map.! edgeObject))
  snapshot <- projectionReady currentViews (vectorThrough [(remoteA, 1)]) successor
  assertEqual
    "A-only frontier reconstructs A"
    (Just (checkedPublicationId (structuralPublication low)))
    (ownedEdgeProjectionPublication (structuralProjectionSnapshotEdges snapshot Map.! edgeObject))

caseDominatedFastPath :: Assertion
caseDominatedFastPath = do
  let source = object 45
      destination = object 46
      edgeObject = object 47
      value = edgeValue edgeObject source destination Preserve
      high = app remoteA 1 emptyVector EdgeRole 20 value
      low = app remoteB 1 (vectorThrough [(remoteA, 1)]) EdgeRole 10 value
      readyViews = views [NeutralVertex source, NeutralVertex destination] [] []
      unavailableViews = views [] [] []
  afterHigh <- applyReady readyViews high Nothing emptyState
  prepared <- prepareReady unavailableViews low Nothing afterHigh
  assertEqual
    "dominated intrinsic occurrence is retained"
    StructuralApplicationAppliedNonWinner
    (preparedStructuralClassification prepared)
  assertBool "no graph consequence" (structuralGraphPatchNull (preparedStructuralGraphPatch prepared))
  assertBool "no causal debt" (structuralDebtSetNull (preparedStructuralDebts prepared))
  case prepareStructuralProjectionRefresh
    readyViews
    (structuralOccurrence low)
    Nothing
    (preparedStructuralSuccessor prepared) of
    Left (StructuralRefreshOccurrenceNotProjectable actual) ->
      assertEqual "non-projectable occurrence" (structuralOccurrence low) actual
    other -> assertFailure ("unexpected dominated refresh result: " <> show other)

caseMalformedNonwinner :: Assertion
caseMalformedNonwinner = do
  let delta = deltaId 50
      subject = globalObjectIdFromDeltaId delta
      high = app remoteB 1 emptyVector DeltaRole 20 (rootValue subject (VoidLabel, 0) dataSort)
      malformed =
        app
          remoteA
          1
          emptyVector
          DeltaRole
          10
          (rootValueBytes subject (VoidLabel, 0) (ByteString.replicate 31 0))
      currentViews = views [] [] []
  afterHigh <- applyReady currentViews high Nothing emptyState
  assertBool "malformed fixture loses" (winnerKey malformed < winnerKey high)
  case prepareStructuralReconciliation currentViews malformed Nothing afterHigh of
    Left (StructuralValueShapeContradiction DeltaCarrier) -> pure ()
    other -> assertFailure ("unexpected malformed nonwinner: " <> show other)

caseNablaReadiness :: Assertion
caseNablaReadiness = do
  let nabla = checked "nabla" (mkNablaId (fixtureIdentifierBytes 60))
      subject = globalObjectIdFromNablaId nabla
      process = processId 61
      root = app localHerald 1 emptyVector NablaRole 1 (nablaRootValue subject ((ProcessLabel process, 0)) dataSort)
  assertHeld
    "controller"
    (Set.singleton (StructuralProcessDependency process))
    (prepareStructuralReconciliation (views [] [] []) root Nothing emptyState)
  prepared <-
    prepareReady
      (views [] [(process, localHerald)] [(process, subject)])
      root
      Nothing
      emptyState
  let projected = exactChangeAfter (structuralGraphVertexChanges (preparedStructuralGraphPatch prepared) Map.! subject)
  case projected of
    Just vertex@(NablaVertexProjection _ actualNabla actualSort controller activity) -> do
      assertEqual "occurrence" (Just (structuralOccurrence root)) (structuralVertexProjectionOccurrence vertex)
      assertEqual "nabla" nabla actualNabla
      assertEqual "sort occurrence" dataSortOccurrence actualSort
      assertEqual "controller" (LiveProcessController process localHerald) controller
      assertEqual "activity" (ActiveRootHere process) activity
    other -> assertFailure ("unexpected nabla projection: " <> show other)
  assertEqual "no placement" Map.empty (placementPatchChanges (preparedStructuralPlacementPatch prepared))
  assertEqual
    "nabla establishes its endpoint and exact committed occurrence, not its pre-existing effective sort"
    ( Set.fromList
        [ StructuralEndpointDependency subject,
          StructuralPredecessorDependency (structuralOccurrence root)
        ]
    )
    (preparedStructuralSatisfiedDependencies prepared)

caseProcessCarrierRejected :: Assertion
caseProcessCarrierRejected = do
  let candidate = app localHerald 1 emptyVector ProcessEpochRole 1 (processValue (object 62))
  case prepareStructuralReconciliation (views [] [] []) candidate Nothing emptyState of
    Left StructuralProcessCarrierUnsupported -> pure ()
    other -> assertFailure ("unexpected process carrier: " <> show other)

caseDeltaActivation :: Assertion
caseDeltaActivation = do
  let delta = deltaId 70
      subject = globalObjectIdFromDeltaId delta
      process = processId 71
      incarnation = storeId 72
      root = app localHerald 1 emptyVector DeltaRole 1 (rootValue subject ((ProcessLabel process, 0)) dataSort)
      activeViews = views [] [(process, localHerald)] [(process, subject)]
  assertHeld
    "fresh resource"
    (Set.singleton (FreshStoreIncarnationDependency delta))
    (prepareStructuralReconciliation activeViews root Nothing emptyState)
  prepared <-
    prepareReady
      activeViews
      root
      (Just (freshStoreIncarnationEvidence ConfiguredManifestStoreIncarnation delta incarnation))
      emptyState
  let placement = placementPatchChanges (preparedStructuralPlacementPatch prepared)
      stored = storePatchChanges (preparedStructuralStorePatch prepared)
      destination = exactChangeAfter (placement Map.! delta)
      successor = preparedStructuralSuccessor prepared
  assertEqual "leaf mirrors" placement stored
  case destination of
    Just store -> do
      assertEqual "incarnation" incarnation (dynamicStoreIncarnation store)
      assertEqual "source" ConfiguredManifestStoreIncarnation (dynamicStoreIncarnationSource store)
    Nothing -> assertFailure "missing active delta destination"
  assertEqual
    "source ledger"
    (Just ConfiguredManifestStoreIncarnation)
    (Map.lookup incarnation (structuralAppliedUsedStoreIncarnations successor))
  assertBool "Store activation debt" (hasDebt StoreActivationDebt prepared)
  assertBool "Placement activation debt" (hasDebt PlacementActivationDebt prepared)
  assertEqual
    "delta establishes its endpoint and exact committed occurrence, not its pre-existing effective sort"
    ( Set.fromList
        [ StructuralEndpointDependency subject,
          StructuralPredecessorDependency (structuralOccurrence root)
        ]
    )
    (preparedStructuralSatisfiedDependencies prepared)
  let exactStore =
        qualifiedStoreIncarnation
          localHerald
          delta
          dataSortOccurrence
          incarnation
      exactPlacement =
        qualifiedPlacementFact
          localHerald
          delta
          dataSortOccurrence
          incarnation
      destinationDebts =
        [ debt
        | debt <- structuralDebtSetEntries (preparedStructuralDebts prepared),
          structuralDebtKeyKind (structuralConsequenceDebtKey debt)
            == DestinationAlignmentDebt
        ]
  case destinationDebts of
    [debt] -> do
      assertEqual
        "destination debt key is exact and occurrence-qualified"
        ( structuralDebtKey
            (structuralOccurrenceCause (structuralOccurrence root))
            DestinationAlignmentDebt
            dataSortOccurrence
            (Just exactStore)
        )
        (structuralConsequenceDebtKey debt)
      assertEqual
        "destination debt names the exact known placement"
        (Set.singleton exactPlacement)
        ( structuralDebtKnownPlacements
            (structuralConsequenceDebtEvidence debt)
        )
      assertEqual
        "first activation has no predecessor resource"
        Set.empty
        ( structuralDebtPredecessorResources
            (structuralConsequenceDebtEvidence debt)
        )
    debts ->
      assertFailure
        ("expected one exact destination-alignment debt, got: " <> show debts)

caseHiddenDeltaResource :: Assertion
caseHiddenDeltaResource = do
  let delta = deltaId 80
      subject = globalObjectIdFromDeltaId delta
      process = processId 81
      lowIncarnation = storeId 82
      highIncarnation = storeId 83
      value = rootValue subject ((ProcessLabel process, 0)) dataSort
      low = app remoteA 1 emptyVector DeltaRole 10 value
      high = app remoteB 1 emptyVector DeltaRole 20 value
      activeViews = views [] [(process, localHerald)] [(process, subject)]
  afterHigh <-
    applyReady
      activeViews
      high
      (Just (freshStoreIncarnationEvidence GeneratedStoreIncarnation delta highIncarnation))
      emptyState
  prepared <-
    prepareReady
      activeViews
      low
      (Just (freshStoreIncarnationEvidence ConfiguredManifestStoreIncarnation delta lowIncarnation))
      afterHigh
  let successor = preparedStructuralSuccessor prepared
  assertEqual "full-current nonwinner" StructuralApplicationAppliedNonWinner (preparedStructuralClassification prepared)
  assertEqual "no active leaf" Map.empty (placementPatchChanges (preparedStructuralPlacementPatch prepared))
  assertEqual "B remains active" [highIncarnation] (dynamicStoreIncarnation <$> structuralAppliedLocalStores successor)
  assertBool
    "both resources retained"
    (all (`Map.member` structuralAppliedRetainedStoreResources successor) [lowIncarnation, highIncarnation])
  assertEqual
    "A resource association"
    (Just lowIncarnation)
    ( dynamicStoreIncarnation
        <$> Map.lookup (structuralOccurrence low) (structuralAppliedOccurrenceStoreResources successor)
    )
  assertEqual
    "both occurrence-exact dynamic coordinates survive the ahead winner"
    [ ( structuralOccurrence low,
        subject,
        delta,
        dataSortOccurrence,
        LiveProcessController process localHerald,
        controlIndex 0
      ),
      ( structuralOccurrence high,
        subject,
        delta,
        dataSortOccurrence,
        LiveProcessController process localHerald,
        controlIndex 0
      )
    ]
    [ ( appliedDynamicDeltaOccurrence coordinate,
        appliedDynamicDeltaObject coordinate,
        appliedDynamicDeltaDelta coordinate,
        appliedDynamicDeltaSort coordinate,
        appliedDynamicDeltaController coordinate,
        appliedDynamicDeltaControlPrerequisite coordinate
      )
    | coordinate <- structuralAppliedDynamicDeltaCoordinates successor
    ]
  assertBool "hidden Store debt" (hasDebt StoreActivationDebt prepared)
  assertBool "no hidden Placement debt" (not (hasDebt PlacementActivationDebt prepared))
  assertEqual
    "active Store leaf is unchanged"
    Map.empty
    (storePatchChanges (preparedStructuralStorePatch prepared))
  assertEqual
    "retained resource is an explicit Store patch"
    [lowIncarnation]
    (Map.keys (storePatchRetainedResourceChanges (preparedStructuralStorePatch prepared)))

caseHiddenOccurrenceRefresh :: Assertion
caseHiddenOccurrenceRefresh = do
  let delta = deltaId 84
      subject = globalObjectIdFromDeltaId delta
      process = processId 85
      oldAIncarnation = storeId 86
      bIncarnation = storeId 87
      newAIncarnation = storeId 88
      a =
        app
          remoteA
          1
          emptyVector
          DeltaRole
          10
          (rootValue subject ((ProcessLabel process, 0)) dataSort)
      b =
        app
          remoteB
          1
          emptyVector
          DeltaRole
          20
          (rootValue subject ((ProcessLabel process, 0)) dataSort)
      carriedSort = dataSortOccurrence
      exactB = qualifiedStoreIncarnation localHerald delta carriedSort bIncarnation
      activeViews = views [] [(process, localHerald)] [(process, subject)]
      passiveViews = views [] [(process, localHerald)] []
  assertBool "B is the definite full-current winner" (winnerKey b > winnerKey a)
  afterB <-
    applyReady
      activeViews
      b
      (Just (freshStoreIncarnationEvidence GeneratedStoreIncarnation delta bIncarnation))
      emptyState
  afterA <-
    applyReady
      activeViews
      a
      (Just (freshStoreIncarnationEvidence GeneratedStoreIncarnation delta oldAIncarnation))
      afterB
  assertEqual
    "both projectable occurrences retain their own exact resource"
    ( Map.fromList
        [ (structuralOccurrence a, oldAIncarnation),
          (structuralOccurrence b, bIncarnation)
        ]
    )
    ( dynamicStoreIncarnation
        <$> structuralAppliedOccurrenceStoreResources afterA
    )
  case structuralAppliedLocalStores afterA of
    [store] -> do
      assertEqual "B owns the active incarnation" bIncarnation (dynamicStoreIncarnation store)
      assertEqual "B owns the active exact sort" carriedSort (dynamicStoreSort store)
    stores -> assertFailure ("unexpected initial active Stores: " <> show stores)
  initialA <- projectionReady activeViews (vectorThrough [(remoteA, 1)]) afterA
  assertDeltaProjection
    "A-only active frontier"
    delta
    carriedSort
    (ActiveRootHere process)
    initialA

  stableA <- refreshReady activeViews (structuralOccurrence a) Nothing afterA
  assertBool
    "hidden active refresh has no current graph patch"
    (structuralGraphPatchNull (preparedStructuralGraphPatch stableA))
  assertEqual
    "hidden active refresh reuses A without Placement mutation"
    Map.empty
    (placementPatchChanges (preparedStructuralPlacementPatch stableA))
  assertEqual
    "hidden active refresh reuses A without active Store mutation"
    Map.empty
    (storePatchChanges (preparedStructuralStorePatch stableA))
  let stableState = preparedStructuralSuccessor stableA
  assertEqual
    "hidden active refresh preserves A's association"
    (Just oldAIncarnation)
    ( dynamicStoreIncarnation
        <$> Map.lookup
          (structuralOccurrence a)
          (structuralAppliedOccurrenceStoreResources stableState)
    )

  passiveB <-
    refreshReady passiveViews (structuralOccurrence b) Nothing stableState
  let afterPassiveB = preparedStructuralSuccessor passiveB
  assertEqual
    "full-current B passivation closes the active Store"
    []
    (structuralAppliedLocalStores afterPassiveB)
  assertEqual
    "only hidden A remains associated after B passivates"
    (Map.singleton (structuralOccurrence a) oldAIncarnation)
    ( dynamicStoreIncarnation
        <$> structuralAppliedOccurrenceStoreResources afterPassiveB
    )

  passiveA <-
    refreshReady passiveViews (structuralOccurrence a) Nothing afterPassiveB
  let afterPassiveA = preparedStructuralSuccessor passiveA
      exactOldA =
        qualifiedStoreIncarnation
          localHerald
          delta
          carriedSort
          oldAIncarnation
      aHandoffs =
        [ debt
        | debt <- structuralDebtSetEntries (preparedStructuralDebts passiveA),
          structuralDebtKeyKind (structuralConsequenceDebtKey debt)
            == StoreReplacementDebt
        ]
  assertBool
    "hidden A passivation has no current graph patch"
    (structuralGraphPatchNull (preparedStructuralGraphPatch passiveA))
  assertEqual
    "hidden A passivation has no Placement mutation"
    Map.empty
    (placementPatchChanges (preparedStructuralPlacementPatch passiveA))
  assertEqual
    "hidden A passivation has no active Store mutation"
    Map.empty
    (storePatchChanges (preparedStructuralStorePatch passiveA))
  assertEqual
    "both passive occurrences have released their associations"
    Map.empty
    (structuralAppliedOccurrenceStoreResources afterPassiveA)
  assertEqual
    "active leaves remain empty after hidden A refresh"
    []
    (structuralAppliedLocalStores afterPassiveA)
  case aHandoffs of
    [debt] -> do
      assertEqual
        "hidden A handoff is occurrence/sort/incarnation exact"
        ( structuralDebtKey
            (structuralOccurrenceCause (structuralOccurrence a))
            StoreReplacementDebt
            carriedSort
            (Just exactOldA)
        )
        (structuralConsequenceDebtKey debt)
      assertEqual
        "hidden A handoff has no active placement for its carried sort"
        Set.empty
        ( structuralDebtKnownPlacements
            (structuralConsequenceDebtEvidence debt)
        )
      assertEqual
        "hidden A handoff preserves both exact predecessors for its carried sort"
        (Set.fromList [exactOldA, exactB])
        ( structuralDebtPredecessorResources
            (structuralConsequenceDebtEvidence debt)
        )
    debts ->
      assertFailure
        ("expected one hidden A Store-replacement debt, got: " <> show debts)
  passiveASnapshot <-
    projectionReady passiveViews (vectorThrough [(remoteA, 1)]) afterPassiveA
  assertDeltaProjection
    "A-only passive frontier"
    delta
    carriedSort
    PassiveRoot
    passiveASnapshot

  assertHeld
    "hidden A reactivation requires a fresh incarnation"
    (Set.singleton (FreshStoreIncarnationDependency delta))
    ( prepareStructuralProjectionRefresh
        activeViews
        (structuralOccurrence a)
        Nothing
        afterPassiveA
    )
  case prepareStructuralProjectionRefresh
    activeViews
    (structuralOccurrence a)
    ( Just
        ( freshStoreIncarnationEvidence
            GeneratedStoreIncarnation
            delta
            oldAIncarnation
        )
    )
    afterPassiveA of
    Left (StructuralFreshIncarnationReused actualDelta actualIncarnation) -> do
      assertEqual "hidden reactivation delta" delta actualDelta
      assertEqual "hidden retired incarnation" oldAIncarnation actualIncarnation
    other -> assertFailure ("unexpected hidden old-incarnation result: " <> show other)
  activeA <-
    refreshReady
      activeViews
      (structuralOccurrence a)
      ( Just
          ( freshStoreIncarnationEvidence
              GeneratedStoreIncarnation
              delta
              newAIncarnation
          )
      )
      afterPassiveA
  let afterActiveA = preparedStructuralSuccessor activeA
      exactNewA =
        qualifiedStoreIncarnation
          localHerald
          delta
          carriedSort
          newAIncarnation
      aDestinations =
        [ debt
        | debt <- structuralDebtSetEntries (preparedStructuralDebts activeA),
          structuralDebtKeyKind (structuralConsequenceDebtKey debt)
            == DestinationAlignmentDebt
        ]
  assertBool
    "hidden A activation has no current graph patch"
    (structuralGraphPatchNull (preparedStructuralGraphPatch activeA))
  assertEqual
    "hidden A activation leaves Placement empty"
    Map.empty
    (placementPatchChanges (preparedStructuralPlacementPatch activeA))
  assertEqual
    "hidden A activation leaves active Store mutations empty"
    Map.empty
    (storePatchChanges (preparedStructuralStorePatch activeA))
  assertEqual
    "hidden A activation leaves full-current active Stores empty"
    []
    (structuralAppliedLocalStores afterActiveA)
  assertEqual
    "only A selects its new hidden resource"
    (Map.singleton (structuralOccurrence a) newAIncarnation)
    ( dynamicStoreIncarnation
        <$> structuralAppliedOccurrenceStoreResources afterActiveA
    )
  assertBool "hidden A owns Store-activation debt" (hasDebt StoreActivationDebt activeA)
  assertBool "hidden A owns no Placement-activation debt" (not (hasDebt PlacementActivationDebt activeA))
  case aDestinations of
    [debt] -> do
      assertEqual
        "hidden A destination is occurrence/sort/new-incarnation exact"
        ( structuralDebtKey
            (structuralOccurrenceCause (structuralOccurrence a))
            DestinationAlignmentDebt
            carriedSort
            (Just exactNewA)
        )
        (structuralConsequenceDebtKey debt)
      assertEqual
        "hidden A destination advertises no full-current placement"
        Set.empty
        ( structuralDebtKnownPlacements
            (structuralConsequenceDebtEvidence debt)
        )
      assertEqual
        "hidden A destination retains both retired resources as predecessor evidence"
        (Set.fromList [exactOldA, exactB])
        ( structuralDebtPredecessorResources
            (structuralConsequenceDebtEvidence debt)
        )
    debts ->
      assertFailure
        ("expected one hidden A destination debt, got: " <> show debts)
  activeASnapshot <-
    projectionReady activeViews (vectorThrough [(remoteA, 1)]) afterActiveA
  assertDeltaProjection
    "reactivated A-only frontier"
    delta
    carriedSort
    (ActiveRootHere process)
    activeASnapshot
  let currentProjection = structuralAppliedVertexProjections afterActiveA Map.! subject
  case currentProjection of
    DeltaVertexProjection _ actualDelta actualSort _ PassiveRoot -> do
      assertEqual "full-current B delta" delta actualDelta
      assertEqual "full-current B retains the delta's carried sort" carriedSort actualSort
    projection ->
      assertFailure
        ("hidden A refresh changed full-current B projection: " <> show projection)

assertDeltaProjection ::
  String ->
  DeltaId ->
  SortOccurrence ->
  RootActivityProjection ->
  StructuralProjectionSnapshot ->
  Assertion
assertDeltaProjection context expectedDelta expectedSort expectedActivity snapshot =
  case Map.elems (structuralProjectionSnapshotVertices snapshot) of
    [DeltaVertexProjection _ actualDelta actualSort _ actualActivity] -> do
      assertEqual (context <> " delta") expectedDelta actualDelta
      assertEqual (context <> " sort") expectedSort actualSort
      assertEqual (context <> " activity") expectedActivity actualActivity
    projections ->
      assertFailure (context <> ": unexpected projection set: " <> show projections)

caseProjectionRefresh :: Assertion
caseProjectionRefresh = do
  let delta = deltaId 90
      subject = globalObjectIdFromDeltaId delta
      process = processId 91
      firstIncarnation = storeId 92
      secondIncarnation = storeId 93
      root = app localHerald 1 emptyVector DeltaRole 1 (rootValue subject ((ProcessLabel process, 0)) dataSort)
      activeViews = views [] [(process, localHerald)] [(process, subject)]
      passiveViews = reconciliationViews localHerald (Map.delete dataSort effectiveSorts) Set.empty Map.empty Set.empty
      replacementSort = sortOccurrence dataSort (sortOccurrenceDefinition otherSortOccurrence)
      reactivatedViews =
        reconciliationViews
          localHerald
          (Map.insert dataSort replacementSort effectiveSorts)
          Set.empty
          Map.empty
          (Set.singleton (process, subject))
  active <-
    applyReady
      activeViews
      root
      (Just (freshStoreIncarnationEvidence GeneratedStoreIncarnation delta firstIncarnation))
      emptyState
  stablePrepared <- refreshReady activeViews (structuralOccurrence root) Nothing active
  assertBool "unchanged occurrence refresh is explicitly a no-op" (not (preparedStructuralChangesState stablePrepared))
  passivePrepared <- refreshReady passiveViews (structuralOccurrence root) Nothing active
  assertBool "passivating a projection changes its reconciliation facts" (preparedStructuralChangesState passivePrepared)
  let passive = preparedStructuralSuccessor passivePrepared
      close = placementPatchChanges (preparedStructuralPlacementPatch passivePrepared) Map.! delta
  assertEqual "closed old resource" (Just firstIncarnation) (dynamicStoreIncarnation <$> exactChangeBefore close)
  assertEqual "no successor" Nothing (exactChangeAfter close)
  assertEqual "no Controlled observation" Nothing (controlledPatchObservation (preparedStructuralControlledPatch passivePrepared))
  assertEqual "history unchanged" (structuralAppliedHistoryOccurrences active) (structuralAppliedHistoryOccurrences passive)
  assertBool "old resource retained" (Map.member firstIncarnation (structuralAppliedRetainedStoreResources passive))
  assertEqual
    "passivated occurrence no longer selects the retired resource"
    Nothing
    (Map.lookup (structuralOccurrence root) (structuralAppliedOccurrenceStoreResources passive))
  let exactOldStore =
        qualifiedStoreIncarnation
          localHerald
          delta
          dataSortOccurrence
          firstIncarnation
      handoffDebts =
        [ debt
        | debt <- structuralDebtSetEntries (preparedStructuralDebts passivePrepared),
          structuralDebtKeyKind (structuralConsequenceDebtKey debt)
            == StoreReplacementDebt
        ]
  case handoffDebts of
    [debt] -> do
      assertEqual
        "replacement debt key names the exact occurrence and retired resource"
        ( structuralDebtKey
            (structuralOccurrenceCause (structuralOccurrence root))
            StoreReplacementDebt
            dataSortOccurrence
            (Just exactOldStore)
        )
        (structuralConsequenceDebtKey debt)
      assertEqual
        "passivation leaves no known active placement"
        Set.empty
        ( structuralDebtKnownPlacements
            (structuralConsequenceDebtEvidence debt)
        )
      assertEqual
        "replacement debt preserves the exact predecessor resource"
        (Set.singleton exactOldStore)
        ( structuralDebtPredecessorResources
            (structuralConsequenceDebtEvidence debt)
        )
    debts ->
      assertFailure
        ("expected one exact Store-replacement debt, got: " <> show debts)
  assertHeld
    "reactivation fresh resource"
    (Set.singleton (FreshStoreIncarnationDependency delta))
    (prepareStructuralProjectionRefresh reactivatedViews (structuralOccurrence root) Nothing passive)
  case prepareStructuralProjectionRefresh
    reactivatedViews
    (structuralOccurrence root)
    (Just (freshStoreIncarnationEvidence GeneratedStoreIncarnation delta firstIncarnation))
    passive of
    Left (StructuralFreshIncarnationReused actualDelta actualIncarnation) -> do
      assertEqual "delta" delta actualDelta
      assertEqual "old incarnation" firstIncarnation actualIncarnation
    other -> assertFailure ("unexpected old resource refresh: " <> show other)
  activePrepared <-
    refreshReady
      reactivatedViews
      (structuralOccurrence root)
      (Just (freshStoreIncarnationEvidence GeneratedStoreIncarnation delta secondIncarnation))
      passive
  let reactivated = preparedStructuralSuccessor activePrepared
  assertEqual "new active resource" [secondIncarnation] (dynamicStoreIncarnation <$> structuralAppliedLocalStores reactivated)
  assertEqual
    "used resource ledger"
    (Set.fromList [firstIncarnation, secondIncarnation])
    (Map.keysSet (structuralAppliedUsedStoreIncarnations reactivated))
  assertEqual "reactivated Store retains the admitted sort" [dataSortOccurrence] (dynamicStoreSort <$> structuralAppliedLocalStores reactivated)
  expected <- projectionReady activeViews (vectorThrough [(localHerald, 1)]) active
  actual <- projectionReady reactivatedViews (vectorThrough [(localHerald, 1)]) reactivated
  assertEqual
    "possession refresh preserves canonical meaning through missing and replacement definitions"
    (structuralProjectionSnapshotCanonicalBytes expected)
    (structuralProjectionSnapshotCanonicalBytes actual)

caseProjectionRefreshRootMeaning :: Assertion
caseProjectionRefreshRootMeaning = do
  let subject = object 94
      process = processId 95
      originalViews = views [] [(process, remoteA)] []
      replacementSort = sortOccurrence dataSort (sortOccurrenceDefinition otherSortOccurrence)
      changedViews =
        reconciliationViews
          localHerald
          (Map.insert dataSort replacementSort effectiveSorts)
          Set.empty
          (Map.singleton process localHerald)
          (Set.singleton (process, subject))
      missingViews = reconciliationViews localHerald (Map.delete dataSort effectiveSorts) Set.empty Map.empty Set.empty
  forM_ [NablaRole, DeltaRole] $ \role ->
    forM_ [VoidLabel, ZombieLabel process, ProcessLabel process] $ \owner -> do
      let value = case role of
            NablaRole -> nablaRootValue subject (owner, 0) dataSort
            _ -> rootValue subject (owner, 0) dataSort
          candidate = app remoteA 1 emptyVector role 1 value
      original <- applyReady originalViews candidate Nothing emptyState
      expected <- projectionReady originalViews (vectorThrough [(remoteA, 1)]) original
      forM_ [changedViews, missingViews] $ \currentViews -> do
        refreshed <- refreshReady currentViews (structuralOccurrence candidate) Nothing original
        let successor = preparedStructuralSuccessor refreshed
        assertEqual "sort/controller dependency changes do not reinterpret retained meaning" original successor
        assertBool "retained meaning refresh is a no-op" (not (preparedStructuralChangesState refreshed))
        assertBool "no graph change" (structuralGraphPatchNull (preparedStructuralGraphPatch refreshed))
        assertBool "no new alignment debt" (structuralDebtSetNull (preparedStructuralDebts refreshed))
        assertEqual "portable original meaning stays exact" (captureStructuralCarrierBase original) (captureStructuralCarrierBase successor)
        actual <- projectionReady currentViews (vectorThrough [(remoteA, 1)]) successor
        assertEqual "historical projection is not rebound" (structuralProjectionSnapshotCanonicalBytes expected) (structuralProjectionSnapshotCanonicalBytes actual)

caseProjectionRefreshEdgeMeaning :: Assertion
caseProjectionRefreshEdgeMeaning = do
  let source = object 96
      destination = object 97
      subject = object 98
      originalViews = views [NeutralVertex source, NeutralVertex destination] [] []
      candidate = app remoteA 1 emptyVector EdgeRole 1 (edgeValue subject source destination Preserve)
      changedViews = views [NablaVertex (checked "rebound endpoint" (mkNablaId (fixtureIdentifierBytes 96))), NeutralVertex destination] [] []
  original <- applyReady originalViews candidate Nothing emptyState
  forM_ [changedViews, views [] [] []] $ \currentViews -> do
    refreshed <- refreshReady currentViews (structuralOccurrence candidate) Nothing original
    assertEqual "endpoint views cannot reinterpret the admitted edge" original (preparedStructuralSuccessor refreshed)
    assertBool "edge refresh is a no-op" (not (preparedStructuralChangesState refreshed))
    assertBool "no edge patch" (structuralGraphPatchNull (preparedStructuralGraphPatch refreshed))
    assertBool "no topology debt" (structuralDebtSetNull (preparedStructuralDebts refreshed))

caseStoreSeedChecks :: Assertion
caseStoreSeedChecks = do
  let firstDelta = deltaId 100
      secondDelta = deltaId 101
      process = processId 102
      incarnation = storeId 103
      store herald delta =
        dynamicLocalStoreSpec
          process
          (globalObjectIdFromDeltaId delta)
          herald
          delta
          dataSortOccurrence
          incarnation
          ConfiguredManifestStoreIncarnation
  case structuralAppliedStateWithStores (views [] [] []) emptyVector (Map.singleton firstDelta (store remoteA firstDelta)) of
    Left (StructuralLocalStoreHeraldMismatch expected actual) -> do
      assertEqual "local" localHerald expected
      assertEqual "remote" remoteA actual
    other -> assertFailure ("unexpected remote store seed: " <> show other)
  case structuralAppliedStateWithStores
    (views [] [] [])
    emptyVector
    (Map.fromList [(firstDelta, store localHerald firstDelta), (secondDelta, store localHerald secondDelta)]) of
    Left (StructuralLocalStoreIncarnationConflict actual first second) -> do
      assertEqual "incarnation" incarnation actual
      assertEqual "deltas" (Set.fromList [firstDelta, secondDelta]) (Set.fromList [first, second])
    other -> assertFailure ("unexpected duplicate store seed: " <> show other)

caseBaselineActiveStore :: Assertion
caseBaselineActiveStore = do
  let delta = deltaId 105
      subject = globalObjectIdFromDeltaId delta
      process = processId 106
      incarnation = storeId 107
      currentViews = views [] [(process, localHerald)] [(process, subject)]
      baselinePublication =
        publication remoteA DeltaRole 1 (rootValue subject ((ProcessLabel process, 0)) dataSort)
      supplied =
        Map.singleton
          subject
          (baselineStructuralWinner (carrierOccurrence DeltaRole) baselinePublication)
      exactStore =
        dynamicLocalStoreSpec
          process
          subject
          localHerald
          delta
          dataSortOccurrence
          incarnation
          ConfiguredManifestStoreIncarnation
  case structuralAppliedStateWithBaseline currentViews emptyVector supplied Map.empty of
    Right (Left dependencies) ->
      assertEqual
        "missing exact Store is held"
        (Set.singleton (FreshStoreIncarnationDependency delta))
        dependencies
    other -> assertFailure ("unexpected missing baseline Store result: " <> show other)
  let wrongStore =
        dynamicLocalStoreSpec
          process
          subject
          localHerald
          delta
          otherSortOccurrence
          incarnation
          ConfiguredManifestStoreIncarnation
  case structuralAppliedStateWithBaseline currentViews emptyVector supplied (Map.singleton delta wrongStore) of
    Left (StructuralBaselineStoreMismatch actual) ->
      assertEqual "mismatched baseline delta" delta actual
    other -> assertFailure ("unexpected mismatched baseline Store result: " <> show other)
  exactState <- baselineState currentViews supplied (Map.singleton delta exactStore)
  assertBool
    "the exact local baseline Store remains bound to its publication provenance"
    ( structuralAppliedBaselineStoreMatches
        (checkedPublicationId baselinePublication)
        exactStore
        exactState
    )
  let passive =
        Map.singleton
          subject
          (baseline DeltaRole remoteA 2 (rootValue subject (VoidLabel, 0) dataSort))
  case structuralAppliedStateWithBaseline currentViews emptyVector passive (Map.singleton delta exactStore) of
    Left (StructuralBaselineStoreWithoutActiveProjection actual) ->
      assertEqual "unmatched passive Store" delta actual
    other -> assertFailure ("unexpected passive baseline Store result: " <> show other)

caseCarriedSortForward :: Assertion
caseCarriedSortForward = do
  let delta = deltaId 110
      subject = globalObjectIdFromDeltaId delta
      value = rootValue subject (VoidLabel, 0) dataSort
      first = app localHerald 1 emptyVector DeltaRole 1 value
      second =
        app
          localHerald
          2
          (vectorThrough [(localHerald, 1)])
          DeltaRole
          2
          value
      currentViews = views [] [] []
  assertBool "forwarded publication wins" (winnerKey second > winnerKey first)
  afterFirst <- applyReady currentViews first Nothing emptyState
  prepared <- prepareReady currentViews second Nothing afterFirst
  let cutoverSorts =
        Set.fromList
          [ structuralDebtKeySort key
          | debt <- structuralDebtSetEntries (preparedStructuralDebts prepared),
            let key = structuralConsequenceDebtKey debt,
            structuralDebtKeyKind key == CutoverDebt
          ]
  assertEqual "forwarding retains one carried sort" (Set.singleton dataSortOccurrence) cutoverSorts

casePerSortDebtEvidence :: Assertion
casePerSortDebtEvidence = do
  let firstDelta = deltaId 120
      secondDelta = deltaId 121
      process = processId 122
      store delta typed incarnation =
        dynamicLocalStoreSpec process (globalObjectIdFromDeltaId delta) localHerald delta typed incarnation ConfiguredManifestStoreIncarnation
      stores =
        Map.fromList
          [ (firstDelta, store firstDelta dataSortOccurrence (storeId 123)),
            (secondDelta, store secondDelta otherSortOccurrence (storeId 124))
          ]
      currentViews = views [] [] []
      candidate = app remoteA 1 emptyVector NeutralVertexRole 1 (neutralValue (object 125))
  seeded <- stateWithStores currentViews stores
  prepared <- prepareReady currentViews candidate Nothing seeded
  mapM_ assertQualified (structuralDebtSetEntries (preparedStructuralDebts prepared))
  where
    assertQualified debt = do
      let typedSort = structuralDebtKeySort (structuralConsequenceDebtKey debt)
          evidence = structuralConsequenceDebtEvidence debt
      assertBool
        "placements exact-sort"
        (all ((== typedSort) . qualifiedPlacementSort) (structuralDebtKnownPlacements evidence))
      assertBool
        "resources exact-sort"
        (all ((== typedSort) . qualifiedStoreSort) (structuralDebtPredecessorResources evidence))

caseCausalShapeChecks :: Assertion
caseCausalShapeChecks = do
  let value = neutralValue (object 130)
      missingDot = app localHerald 2 (vectorThrough [(localHerald, 1)]) NeutralVertexRole 1 value
  case prepareStructuralReconciliation (views [] [] []) missingDot Nothing emptyState of
    Left (StructuralPredecessorDotMissing actual) ->
      assertEqual "missing dot" (occurrence localHerald 1) actual
    other -> assertFailure ("unexpected missing dot: " <> show other)
  let sourceMismatch =
        structuralApplication
          (occurrence remoteA 1)
          emptyVector
          (carrierOccurrence NeutralVertexRole)
          (publication localHerald NeutralVertexRole 2 value)
          (controlIndex 0)
  case prepareStructuralReconciliation (views [] [] []) sourceMismatch Nothing emptyState of
    Left (StructuralOccurrencePublicationSourceMismatch actualOccurrence actualPublication) -> do
      assertEqual "occurrence source" remoteA actualOccurrence
      assertEqual "publication source" localHerald actualPublication
    other -> assertFailure ("unexpected source mismatch: " <> show other)
  let shortMembership = localHerald :| [remoteA]
      shortGeneration =
        checked
          "short membership"
          ( genesisHeraldMembershipGeneration
              (checkedSystemId fixtureCheckedGenesis)
              shortMembership
          )
      shortVector = emptyStructuralVersionVector shortGeneration
      wrongMembership = app localHerald 1 shortVector NeutralVertexRole 3 value
  case prepareStructuralReconciliation (views [] [] []) wrongMembership Nothing emptyState of
    Left StructuralPredecessorMembershipMismatch -> pure ()
    other -> assertFailure ("unexpected membership mismatch: " <> show other)

caseProjectionClosure :: Assertion
caseProjectionClosure = do
  let first = app remoteA 1 emptyVector NeutralVertexRole 1 (neutralValue (object 135))
      second =
        app
          remoteB
          1
          (vectorThrough [(remoteA, 1)])
          NeutralVertexRole
          1
          (neutralValue (object 136))
      currentViews = views [] [] []
  afterFirst <- applyReady currentViews first Nothing emptyState
  afterSecond <- applyReady currentViews second Nothing afterFirst
  case prepareStructuralProjectionAtVector
    currentViews
    (vectorThrough [(remoteB, 1)])
    (controlIndex 0)
    afterSecond of
    Left (StructuralProjectionPredecessorNotCovered actual) ->
      assertEqual
        "included occurrence with omitted predecessor"
        (structuralOccurrence second)
        actual
    other -> assertFailure ("unexpected nonclosed projection result: " <> show other)
  let malformedStamp =
        app
          localHerald
          1
          (vectorThrough [(remoteB, 1)])
          NeutralVertexRole
          1
          (neutralValue (object 137))
  case prepareStructuralReconciliation currentViews malformedStamp Nothing afterSecond of
    Left (StructuralProjectionPredecessorNotCovered actual) ->
      assertEqual
        "stamp includes the same nonclosed occurrence"
        (structuralOccurrence second)
        actual
    other -> assertFailure ("unexpected nonclosed stamp result: " <> show other)

-- Diagnostic policy only affects the seam whose caller already owns the
-- frontier proof. Unadmitted input must retain its ordinary dependency/error
-- behavior even when the runtime chooses to omit repeated owner audits.
caseDiagnosticProjectionAdmission :: Assertion
caseDiagnosticProjectionAdmission = do
  let enabled = views [] [] []
      disabled = withReconciliationDiagnosticChecks DiagnosticChecksDisabled enabled
      missing = app localHerald 2 (vectorThrough [(localHerald, 1)]) NeutralVertexRole 1 (neutralValue (object 138))
  case prepareStructuralReconciliation disabled missing Nothing emptyState of
    Left (StructuralPredecessorDotMissing actual) ->
      assertEqual "incoming missing predecessor still requires its causal dot" (occurrence localHerald 1) actual
    other -> assertFailure ("diagnostic policy changed incoming admission: " <> show other)
  assertEqual
    "generic projection still rejects a missing dot with diagnostics disabled"
    (Left (StructuralPredecessorDotMissing (occurrence localHerald 1)))
    (prepareStructuralProjectionAtVector disabled (vectorThrough [(localHerald, 1)]) (controlIndex 0) emptyState)
  let first = app remoteA 1 emptyVector NeutralVertexRole 1 (neutralValue (object 139))
      second = app remoteB 1 (vectorThrough [(remoteA, 1)]) NeutralVertexRole 1 (neutralValue (object 140))
      nonclosed = vectorThrough [(remoteB, 1)]
  afterFirst <- applyReady enabled first Nothing emptyState
  afterSecond <- applyReady enabled second Nothing afterFirst
  assertEqual
    "generic projection still rejects an available but nonclosed frontier"
    (Left (StructuralProjectionPredecessorNotCovered (structuralOccurrence second)))
    (prepareStructuralProjectionAtVector disabled nonclosed (controlIndex 0) afterSecond)
  assertEqual
    "enabled admitted-frontier diagnostics still check the caller proof"
    (Left (StructuralProjectionPredecessorNotCovered (structuralOccurrence second)))
    (prepareStructuralProjectionAtAdmittedVector enabled nonclosed (controlIndex 0) afterSecond)
  let otherGeneration = checked "different diagnostic membership" (genesisHeraldMembershipGeneration (checkedSystemId fixtureCheckedGenesis) (localHerald :| [remoteA]))
  assertEqual
    "membership shape remains mandatory on the admitted seam"
    (Left StructuralProjectionMembershipMismatch)
    (prepareStructuralProjectionAtAdmittedVector disabled (emptyStructuralVersionVector otherGeneration) (controlIndex 0) afterSecond)
  let nonclosedApplication = app localHerald 1 nonclosed NeutralVertexRole 1 (neutralValue (object 141))
      wrongSequence = app localHerald 2 emptyVector NeutralVertexRole 1 (neutralValue (object 142))
  assertEqual
    "incoming reconciliation still rejects an available but nonclosed predecessor"
    (Left (StructuralProjectionPredecessorNotCovered (structuralOccurrence second)))
    (prepareStructuralReconciliation disabled nonclosedApplication Nothing afterSecond)
  assertEqual
    "local preparation still checks its source sequence with diagnostics disabled"
    (Left (StructuralPredecessorSourceMismatch localHerald))
    (prepareStructuralReconciliationAtAdmittedPredecessor disabled wrongSequence Nothing emptyState)
  assertEqual
    "enabled local diagnostics still check the predecessor proof"
    (Left (StructuralPredecessorDotMissing (occurrence localHerald 1)))
    (prepareStructuralReconciliationAtAdmittedPredecessor enabled missing Nothing emptyState)
  let delta = deltaId 143
      subject = globalObjectIdFromDeltaId delta
      process = processId 144
      activeViews = views [] [(process, localHerald)] [(process, subject)]
      root = app localHerald 1 emptyVector DeltaRole 1 (rootValue subject (ProcessLabel process, 0) dataSort)
      fresh = freshStoreIncarnationEvidence GeneratedStoreIncarnation delta (storeId 145)
  forM_ [DiagnosticChecksEnabled, DiagnosticChecksDisabled] $ \checks -> do
    let configured = withReconciliationDiagnosticChecks checks activeViews
    assertHeld
      "local preparation retains the genuine fresh Store dependency"
      (Set.singleton (FreshStoreIncarnationDependency delta))
      (prepareStructuralReconciliationAtAdmittedPredecessor configured root Nothing emptyState)
    assertEqual
      "local fresh Store retry matches fully checked preparation"
      (prepareStructuralReconciliation activeViews root (Just fresh) emptyState)
      (prepareStructuralReconciliationAtAdmittedPredecessor configured root (Just fresh) emptyState)

propAdmittedProjectionDiagnostics :: Property
propAdmittedProjectionDiagnostics =
  QC.forAll (QC.listOf (QC.elements [remoteA, remoteB])) $ \sources ->
    case foldM advance (emptyState, Map.empty, [emptyVector]) (zip [1 :: Word64 ..] (take 8 sources)) of
      Left problem -> counterexample problem False
      Right (state, counts, frontiers) ->
        let local = app localHerald 1 (vectorThrough (Map.toAscList counts)) NeutralVertexRole 1 (neutralValue (object 190))
         in QC.conjoin
              [ QC.conjoin
                  [ counterexample
                      ("local preparation " <> show checks)
                      ( prepareStructuralReconciliationAtAdmittedPredecessor
                          (withReconciliationDiagnosticChecks checks currentViews)
                          local
                          Nothing
                          state
                          === prepareStructuralReconciliation currentViews local Nothing state
                      )
                  | checks <- [DiagnosticChecksEnabled, DiagnosticChecksDisabled]
                  ],
                QC.conjoin
                  [ counterexample
                      (show (checks, frontier))
                      ( prepareStructuralProjectionAtAdmittedVector
                          (withReconciliationDiagnosticChecks checks currentViews)
                          frontier
                          (controlIndex 0)
                          state
                          === prepareStructuralProjectionAtVector currentViews frontier (controlIndex 0) state
                      )
                  | checks <- [DiagnosticChecksEnabled, DiagnosticChecksDisabled],
                    frontier <- frontiers
                  ]
              ]
  where
    currentViews = views [] [] []
    advance (state, counts, frontiers) (ordinal, source) =
      let next = Map.findWithDefault 0 source counts + 1
          candidate = app source next (vectorThrough (Map.toAscList counts)) NeutralVertexRole ordinal (neutralValue (object (180 + fromIntegral (ordinal `mod` 3))))
          successorCounts = Map.insert source next counts
       in case prepareStructuralReconciliation currentViews candidate Nothing state of
            Right (StructuralReady prepared) ->
              Right
                ( preparedStructuralSuccessor prepared,
                  successorCounts,
                  frontiers <> [vectorThrough (Map.toAscList successorCounts)]
                )
            other -> Left ("generated causal history did not apply: " <> show other)

-- Generate genuinely admitted concurrent histories, then query both closed
-- and nonclosed cuts and missing suffixes. A source either sees everything or
-- only its own previous causal frontier; both choices preserve source order.
propStructuralFrontierSummaries :: Property
propStructuralFrontierSummaries =
  QC.forAll ((,) <$> QC.listOf stepInput <*> QC.vectorOf 16 queryInput) $ \(steps, queries) ->
    case foldM advance (emptyState, Map.empty, Map.empty, []) (zip [1 :: Word64 ..] (take 12 steps)) of
      Left problem -> counterexample problem False
      Right (state, counts, _, applications) ->
        case prepareStructuralCarrierBaseImport currentViews (captureStructuralCarrierBase state) emptyState of
          Left problem -> counterexample ("carrier import failed: " <> show problem) False
          Right imported ->
            let importedState = preparedStructuralCarrierBaseSuccessor imported
                queryCounts =
                  Map.empty
                    : counts
                    : [ Map.fromList [(source, requested `mod` (Map.findWithDefault 0 source counts + 2)) | (source, requested) <- zip sources values]
                      | values <- queries
                      ]
             in QC.conjoin
                  [ counterexample "derived summaries survive portable owner reconstruction" (importedState === state),
                    QC.conjoin
                      [ counterexample
                          (show (requested, structuralAppliedHistoryOccurrences owner))
                          ( QC.conjoin
                              [ validateStructuralAppliedState owner === Right (),
                                validateStructuralFrontier frontier owner === validateStructuralFrontierExhaustive frontier owner,
                                case prepareStructuralFrontierSummary frontier owner of
                                  Left _ -> QC.property True
                                  Right summary -> structuralFrontierControlPrerequisite summary === exhaustiveControl requested applications
                              ]
                          )
                      | owner <- [state, importedState],
                        requested <- queryCounts,
                        let frontier = vectorThrough (Map.toAscList (Map.filter (> 0) requested))
                      ]
                  ]
  where
    sources = [localHerald, remoteA, remoteB]
    currentViews = views [] [] []
    stepInput = (,,) <$> QC.elements sources <*> QC.arbitrary <*> QC.choose (0, 7 :: Word64)
    queryInput = QC.vectorOf 3 (QC.choose (0, 20 :: Word64))
    advance (state, counts, ownFrontiers, applications) (ordinal, (source, observesAll, prerequisite)) =
      let next = Map.findWithDefault 0 source counts + 1
          causalCounts = if observesAll then counts else Map.findWithDefault Map.empty source ownFrontiers
          candidate =
            structuralApplication
              (occurrence source next)
              (vectorThrough (Map.toAscList causalCounts))
              (carrierOccurrence NeutralVertexRole)
              (publication source NeutralVertexRole ordinal (neutralValue (object (180 + fromIntegral (ordinal `mod` 3)))))
              (controlIndex prerequisite)
       in case prepareStructuralReconciliation currentViews candidate Nothing state of
            Right (StructuralReady prepared) ->
              Right
                ( preparedStructuralSuccessor prepared,
                  Map.insert source next counts,
                  Map.insert source (Map.insert source next causalCounts) ownFrontiers,
                  candidate : applications
                )
            other -> Left ("generated concurrent history did not apply: " <> show other)
    exhaustiveControl requested applications =
      maximum
        ( controlIndex 0
            : [ structuralApplicationControlPrerequisite candidate
              | candidate <- applications,
                let identifier = structuralOccurrence candidate,
                let requestedPrefix = Map.findWithDefault 0 (structuralOccurrenceSourceHeraldEpoch identifier) requested,
                requestedPrefix > 0,
                structuralOccurrenceSourceSequence identifier <= positiveSequence requestedPrefix
              ]
        )

caseDescendantProjectionClosure :: Assertion
caseDescendantProjectionClosure = do
  let first = app remoteA 1 emptyVector NeutralVertexRole 1 (neutralValue (object 135))
      second = app remoteB 1 (vectorThrough [(remoteA, 1)]) NeutralVertexRole 1 (neutralValue (object 136))
      currentViews = views [] [] []
      target = checked "retire an unrelated origin" (retireHeraldMembershipGeneration (controlIndex 2) (fixtureRetirementResolution (controlIndex 2)) localHerald fixedMembershipGeneration)
      lineage = fixtureGenesisRetirementLineage fixedMembershipGeneration target
      project vector = checked "project exact descendant coordinates" (projectStructuralVersionVector lineage vector)
      baseAnchor = checked "empty predecessor base" (TerminalSource.terminalSourceGenesisPredecessorBase fixedMembershipGeneration (checked "base anchor cut" (mkTopologyCutId (fixtureIdentifierBytes 231))))
      inventories = [checked "empty retired source inventory" (TerminalSource.terminalSourceInventory lineage localHerald reporter [] [] []) | reporter <- [remoteA, remoteB]]
      topologyDigest = checked "base topology digest" (Topology.mkTopologyOccurrenceDigest (fixtureIdentifierBytes 232))
      union = checked "empty source terminal union" (TerminalSource.deriveTerminalSourceUnion lineage lineage baseAnchor inventories TerminalSource.emptyTerminalSourcePayloadArchive (\_ _ -> topologyDigest))
      ready = checked "terminal union readiness" (TerminalSource.terminalSourceUnionReady emptyVector (controlIndex 2) union)
      acceptances = [checked "base survivor acceptance" (TerminalSource.terminalSourceUnionAcceptance target reporter ready) | reporter <- [remoteA, remoteB]]
      established = checked "established union" (TerminalSource.terminalSourceUnionEstablished target union acceptances)
      predecessor = checked "base topology predecessor" (TerminalSource.terminalSourceUnionTopologyPredecessor lineage union)
      cut = checked "base topology cut" (Topology.topologyCut predecessor (Topology.topologyFrontier (TerminalSource.terminalSourceUnionSuccessorInitialVector union) (TerminalSource.terminalSourceUnionTerminalControlPrefix union)) topologyDigest)
      base = checked "checked installed membership base" (TerminalSource.establishedSuccessorStructuralBase lineage established cut)
  afterFirst <- applyReady currentViews first Nothing emptyState
  afterSecond <- applyReady currentViews second Nothing afterFirst
  successor <- either (assertFailure . show) pure (establishStructuralAppliedMembership base (project (vectorThrough [(remoteA, 1), (remoteB, 1)])) afterSecond)
  assertEqual "membership installation preserves immutable history" (structuralAppliedHistoryOccurrences afterSecond) (structuralAppliedHistoryOccurrences successor)
  importedBase <- either (assertFailure . show) pure (prepareStructuralCarrierBaseImport currentViews (captureStructuralCarrierBase successor) emptyState)
  assertEqual "carrier base preserves established membership and original predecessor proofs" successor (preparedStructuralCarrierBaseSuccessor importedBase)
  case prepareStructuralProjectionAtVector currentViews (project (vectorThrough [(remoteB, 1)])) (controlIndex 2) successor of
    Left (StructuralProjectionPredecessorNotCovered actual) -> assertEqual "retained evidence does not include a surviving predecessor beyond the requested frontier" (structuralOccurrence second) actual
    other -> assertFailure ("unexpected nonclosed descendant projection: " <> show other)
  _ <- projectionAtControl currentViews (project (vectorThrough [(remoteA, 1), (remoteB, 1)])) (controlIndex 2) successor
  -- Carried originals are admitted at the checked current causal frontier,
  -- but retain their original predecessor. That predecessor need not dominate
  -- the prior same-source original, so using only the last endpoint is unsound.
  let carriedOriginal = app remoteB 2 (vectorThrough [(remoteB, 1)]) NeutralVertexRole 2 (neutralValue (object 137))
      current = project (vectorThrough [(remoteA, 1), (remoteB, 1)])
      stamp =
        checked
          "checked nonmonotone original"
          ( Peer.mkStructuralOccurrenceStamp
              (structuralOccurrence carriedOriginal)
              (vectorThrough [(remoteB, 1)])
              (checkedPublicationId (structuralPublication carriedOriginal))
              (checked "original digest" (Peer.mkStructuralPublicationDigest (fixtureIdentifierBytes 233)))
              NeutralVertexCarrier
          )
      carried = checked "checked survivor transfer" (TerminalSource.carryStampedSurvivorOccurrence stamp current base)
  carriedState <- case prepareCarriedStructuralReconciliation currentViews carried carriedOriginal Nothing successor of
    Right (StructuralReady prepared) -> pure (preparedStructuralSuccessor prepared)
    other -> assertFailure ("carried original did not apply: " <> show other)
  let complete = project (vectorThrough [(remoteA, 1), (remoteB, 2)])
      nonclosed = project (vectorThrough [(remoteB, 2)])
      expectedFailure = Left (StructuralProjectionPredecessorNotCovered (structuralOccurrence second))
  assertEqual "an earlier nonclosed requirement remains visible behind a weaker carried endpoint" expectedFailure (validateStructuralFrontier nonclosed carriedState)
  assertEqual "the same carried cut retains its complete causal dependencies" (Right ()) (validateStructuralFrontier complete carriedState)
  importedCarried <- either (assertFailure . show) pure (prepareStructuralCarrierBaseImport currentViews (captureStructuralCarrierBase carriedState) emptyState)
  assertEqual "portable reconstruction preserves nonmonotone original anchors" carriedState (preparedStructuralCarrierBaseSuccessor importedCarried)
  let nextGeneration = app remoteB 3 complete NeutralVertexRole 3 (neutralValue (object 138))
  afterGeneration <- applyReady currentViews nextGeneration Nothing carriedState
  forM_ [carriedState, preparedStructuralCarrierBaseSuccessor importedCarried, afterGeneration] $ \owner -> do
    assertEqual "mixed-generation summaries remain valid" (Right ()) (validateStructuralAppliedState owner)
    forM_
      [ vectorThrough [(remoteB, 2)],
        vectorThrough [(remoteA, 1), (remoteB, 2)],
        nonclosed,
        complete,
        project (vectorThrough [(remoteB, 3)]),
        project (vectorThrough [(remoteA, 1), (remoteB, 3)]),
        project (vectorThrough [(remoteA, 2), (remoteB, 4)])
      ]
      $ \frontier ->
        assertEqual
          "source summary matches exact exhaustive membership/base and missing-dot behavior"
          (validateStructuralFrontierExhaustive frontier owner)
          (validateStructuralFrontier frontier owner)

caseCanonicalActivityIndependent :: Assertion
caseCanonicalActivityIndependent = do
  let nabla = checked "activity nabla" (mkNablaId (fixtureIdentifierBytes 138))
      subject = globalObjectIdFromNablaId nabla
      process = processId 139
      candidate =
        app
          remoteA
          1
          emptyVector
          NablaRole
          1
          (nablaRootValue subject ((ProcessLabel process, 0)) dataSort)
      processes = Map.singleton process localHerald
      homeViews =
        reconciliationViews
          localHerald
          effectiveSorts
          Set.empty
          processes
          (Set.singleton (process, subject))
      peerViews =
        reconciliationViews
          remoteB
          effectiveSorts
          Set.empty
          processes
          Set.empty
      frontier = vectorThrough [(remoteA, 1)]
  homeState <- applyReady homeViews candidate Nothing emptyState
  peerState <- applyReady peerViews candidate Nothing emptyState
  homeSnapshot <- projectionReady homeViews frontier homeState
  peerSnapshot <- projectionReady peerViews frontier peerState
  case ( Map.elems (structuralProjectionSnapshotVertices homeSnapshot),
         Map.elems (structuralProjectionSnapshotVertices peerSnapshot)
       ) of
    ( [NablaVertexProjection _ _ _ _ (ActiveRootHere homeProcess)],
      [NablaVertexProjection _ _ _ _ (RemoteController peerProcess residence)]
      ) -> do
        assertEqual "same controller process" homeProcess peerProcess
        assertEqual "canonical residence" localHerald residence
    projections ->
      assertFailure ("fixture did not produce different local activity: " <> show projections)
  assertEqual
    "common topology transcript ignores local activity"
    (structuralProjectionSnapshotCanonicalBytes homeSnapshot)
    (structuralProjectionSnapshotCanonicalBytes peerSnapshot)
  let passivate = labelControlCause 140 1
      reactivate = labelControlCause 141 2
      controls currentViews initial = do
        passive <- preparedStructuralSuccessor <$> labelControlReady currentViews passivate subject (releasedLabel (VoidLabel, 1)) initial
        preparedStructuralSuccessor <$> labelControlReady currentViews reactivate subject (releasedLabel (ProcessLabel process, 2)) passive
  homeControlled <- either assertFailure pure (controls homeViews homeState)
  peerControlled <- either assertFailure pure (controls peerViews peerState)
  assertBool "the controlled roots retain different receiver activity" (structuralAppliedVertexProjections homeControlled /= structuralAppliedVertexProjections peerControlled)
  assertEqual "portable control bases exclude donor activity from current and historical overlays" (captureStructuralControlBase homeControlled) (captureStructuralControlBase peerControlled)
  assertPortableControlBase homeControlled
  assertPortableControlBase peerControlled
  (importedPeer, historyOnly) <-
    applyControlBaseReady peerState (prepareStructuralControlBaseImport peerViews (captureStructuralControlBase homeControlled) Map.empty peerState)
  assertEqual "receiver activity and exact past controls match independent replay" peerControlled importedPeer
  assertEqual "roundtrip to original controller needs only historical metadata" 1 (length historyOnly)
  forM_ historyOnly assertHistoryOnlyControlImport

caseCarrierBaseEndOrder :: Assertion
caseCarrierBaseEndOrder = do
  (homeViews, subject, processA, processB, homeInitial) <- either assertFailure pure labelControlFixture
  let secondSubject = object 151
      second = app remoteA 1 emptyVector NablaRole 21 (nablaRootValue secondSubject (ProcessLabel processA, 0) dataSort)
      original = app remoteB 1 emptyVector NablaRole 20 (nablaRootValue subject (ProcessLabel processA, 0) dataSort)
      cause = checked "paired End" (processEndCause processA (controlIndex 2))
      beforeViews = reconciliationViews (heraldId 152) effectiveSorts Set.empty (Map.fromList [(processA, localHerald), (processB, remoteA)]) Set.empty
      afterViews = reconciliationViewsWithEndedProcesses (Map.singleton processA (localHerald, controlIndex 2)) (reconciliationViews (heraldId 152) effectiveSorts Set.empty (Map.singleton processB remoteA) Set.empty)
      applyControls currentViews initial = do
        handoff <- either assertFailure pure (labelControlReady currentViews (labelControlCause 153 1) subject (releasedLabel (ProcessLabel processB, 1)) initial)
        ended <- either (assertFailure . show) pure (prepareStructuralProcessEndRefreshes currentViews cause processA (preparedStructuralSuccessor handoff))
        assertEqual "End applies only to the still-owned root" 1 (length ended)
        pure (preparedStructuralSuccessor (snd (last ended)))
  homeBoth <- applyReady homeViews second Nothing homeInitial
  donor <- applyControls homeViews homeBoth
  referenceFirst <- applyReady beforeViews original Nothing emptyState
  referenceBoth <- applyReady beforeViews second Nothing referenceFirst
  reference <- applyControls beforeViews referenceBoth
  let base = captureStructuralCarrierBase donor
  prepared <- either (assertFailure . show) pure (prepareStructuralCarrierBaseImport afterViews base emptyState)
  let imported = preparedStructuralCarrierBaseSuccessor prepared
  assertEqual "exact carrier/control pairing" (captureStructuralControlBase donor) (structuralCarrierBaseControlBase base)
  assertEqual "receiver-specific state matches correctly ordered event replay" reference imported
  assertEqual "capture excludes donor activity" base (captureStructuralCarrierBase reference)
  assertEqual "root handed away before End retains live Q" (Just (LiveProcessController processB remoteA, RemoteController processB remoteA)) (rootControllerActivity =<< Map.lookup subject (structuralAppliedVertexProjections imported))
  assertEqual "the other root retains its exact End cause" (Just (ZombieProcessController processA, PassiveRoot)) (rootControllerActivity =<< Map.lookup secondSubject (structuralAppliedVertexProjections imported))
  assertEqual "the aggregate Graph patch starts from the receiver" emptyState (preparedStructuralCarrierBasePredecessor prepared)
  assertEqual "aggregate creates both receiver vertices" (structuralAppliedVertexProjections imported) (Map.mapMaybe exactChangeAfter (structuralGraphVertexChanges (preparedStructuralCarrierBaseGraphPatch prepared)))
  assertEqual "aggregate predecessor has neither vertex" (Map.fromSet (const Nothing) (Set.fromList [subject, secondSubject])) (Map.map exactChangeBefore (structuralGraphVertexChanges (preparedStructuralCarrierBaseGraphPatch prepared)))
  forM_ [0 .. 2] $ \prefix -> do
    expected <- projectionAtControl beforeViews (vectorThrough [(remoteA, 1), (remoteB, 1)]) (controlIndex prefix) reference
    actual <- projectionAtControl afterViews (vectorThrough [(remoteA, 1), (remoteB, 1)]) (controlIndex prefix) imported
    assertEqual "every historical cut preserves ordering" (structuralProjectionSnapshotCanonicalBytes expected) (structuralProjectionSnapshotCanonicalBytes actual)
  assertEqual "copied indexes retain their invariants" (Right ()) (validateStructuralAppliedState imported)

caseCarrierBaseOriginalSort :: Assertion
caseCarrierBaseOriginalSort = do
  let subject = object 154
      candidate = app remoteA 1 emptyVector DeltaRole 1 (rootValue subject (VoidLabel, 0) dataSort)
      originalViews = views [] [] []
      replacementSort = sortOccurrence dataSort (sortOccurrenceDefinition otherSortOccurrence)
      currentViews = reconciliationViews (heraldId 152) (Map.insert dataSort replacementSort effectiveSorts) Set.empty Map.empty Set.empty
      noSortViews = reconciliationViews (heraldId 152) Map.empty Set.empty Map.empty Set.empty
  donor <- applyReady originalViews candidate Nothing emptyState
  reinterpreted <- applyReady currentViews candidate Nothing emptyState
  assertBool "replaying against current definitions changes the typed meaning" (structuralAppliedVertexProjections donor /= structuralAppliedVertexProjections reinterpreted)
  let base = captureStructuralCarrierBase donor
  forM_ [currentViews, noSortViews] $ \receiverViews -> do
    prepared <- either (assertFailure . show) pure (prepareStructuralCarrierBaseImport receiverViews base emptyState)
    let imported = preparedStructuralCarrierBaseSuccessor prepared
    assertEqual "original sort occurrence survives retirement or reintroduction" donor imported
    assertEqual "publications retain original carrier definition, not carried sort" [(carrierOccurrence DeltaRole, structuralPublication candidate)] (structuralCarrierBasePublications base)
    expected <- projectionAtControl originalViews (vectorThrough [(remoteA, 1)]) (controlIndex 0) donor
    actual <- projectionAtControl receiverViews (vectorThrough [(remoteA, 1)]) (controlIndex 0) imported
    assertEqual "historical digest ignores the receiver's effective registry" (structuralProjectionSnapshotCanonicalBytes expected) (structuralProjectionSnapshotCanonicalBytes actual)

-- This characterizes the checked Reconciliation owners, not a claimed reachable
-- whole-Herald execution. The leaf accepts a supplied retirement proof; the
-- Registry/Oracle owners authenticate that proof in production. No state fields
-- are constructed or mutated directly here.
caseKnownVersusSuppressedOriginal :: Assertion
caseKnownVersusSuppressedOriginal = checkKnownVersusSuppressedOriginal 2 4

propKnownVersusSuppressedOriginal :: Positive Word8 -> Positive Word8 -> Property
propKnownVersusSuppressedOriginal (Positive deletionOffset) (Positive retirementLag) =
  QC.ioProperty $ do
    let deletionIndex = 1 + fromIntegral deletionOffset
        resolveIndex = deletionIndex + 1 + fromIntegral retirementLag
    checkKnownVersusSuppressedOriginal deletionIndex resolveIndex
    pure True

checkKnownVersusSuppressedOriginal :: Word64 -> Word64 -> Assertion
checkKnownVersusSuppressedOriginal deletionIndex resolveIndex = do
  let subject = object 231
      currentViews = views [] [] []
      receiverViews = reconciliationViews remoteB effectiveSorts Set.empty Map.empty Set.empty
      original = app remoteA 1 emptyVector NablaRole 1 (nablaRootValue subject (VoidLabel, 0) dataSort)
      frontier = vectorThrough [(remoteA, 1)]
      deleteCause = labelControlCause 232 deletionIndex
      suppression = DirectRegularSortRetirementSuppression dataSort (controlIndex resolveIndex)
  admitted <- applyReady currentViews original Nothing emptyState
  let originalBase = captureStructuralCarrierBase admitted
  deletion <- either assertFailure pure (labelControlReady currentViews deleteCause subject (releasedDeleted 1) admitted)
  let known = preparedStructuralSuccessor deletion
      controls = captureStructuralControlBase known
  late <- applyRetirementSuppressedReady suppression receiverViews original emptyState
  (suppressed, attached) <-
    applyControlBaseReady
      late
      (prepareStructuralControlBaseImport receiverViews controls Map.empty late)
  forM_ [admitted, known, late, suppressed] $ \owner ->
    assertEqual "each checked leaf owner passes its independent audit" (Right ()) (validateStructuralAppliedState owner)
  assertEqual "both owners use exactly the same original structural occurrence" (structuralAppliedHistoryOccurrences known) (structuralAppliedHistoryOccurrences suppressed)
  assertEqual "both owners retain the same original carrier publication" (structuralCarrierBasePublications originalBase) (structuralCarrierBasePublications (captureStructuralCarrierBase suppressed))
  assertEqual "both owners retain the same ordered deletion control" controls (captureStructuralControlBase suppressed)
  assertEqual "receiver records the later first-admission suppression" (Just suppression) (structuralAppliedRetirementSuppressionAt (structuralOccurrence original) suppressed)
  assertEqual "originally admitted meaning acquired no retirement-suppression disposition" Nothing (structuralAppliedRetirementSuppressionAt (structuralOccurrence original) known)
  assertEqual "suppressed control import only attaches history" 1 (length attached)
  forM_ attached assertHistoryOnlyControlImport
  assertEqual "both current vertex projections are absent" (structuralAppliedVertexProjections known) (structuralAppliedVertexProjections suppressed)
  assertEqual "neither current owner exposes the deleted Nabla" Map.empty (structuralAppliedVertexProjections known)
  forM_ [(currentViews, known), (receiverViews, suppressed)] $ \(ownerViews, owner) -> do
    replayed <- prepareReady ownerViews original Nothing owner
    assertEqual "delayed original replay preserves terminal state" owner (preparedStructuralSuccessor replayed)
    assertBool "delayed original replay has no Graph consequence" (structuralGraphPatchNull (preparedStructuralGraphPatch replayed))
    assertEqual "delayed original replay cannot restore the Nabla" Map.empty (structuralAppliedVertexProjections (preparedStructuralSuccessor replayed))
  forM_ [0, deletionIndex - 1, deletionIndex, resolveIndex - 1, resolveIndex, resolveIndex + 1] $ \index -> do
    knownCut <- projectionAtControl currentViews frontier (controlIndex index) known
    suppressedCut <- projectionAtControl receiverViews frontier (controlIndex index) suppressed
    let context = "control " <> show index
    assertEqual (context <> ": suppressed occurrence has no historical vertex") Map.empty (structuralProjectionSnapshotVertices suppressedCut)
    assertEqual (context <> ": suppressed occurrence has no deletion provenance") Map.empty (structuralProjectionSnapshotDeletedControls suppressedCut)
    if index < deletionIndex
      then do
        assertBool (context <> ": original admitted Nabla exists before deletion") (Map.member subject (structuralProjectionSnapshotVertices knownCut))
        assertEqual (context <> ": no premature deletion provenance") Map.empty (structuralProjectionSnapshotDeletedControls knownCut)
        assertBool
          (context <> ": canonical topology distinguishes a genuinely present object")
          (structuralProjectionSnapshotCanonicalBytes knownCut /= structuralProjectionSnapshotCanonicalBytes suppressedCut)
      else do
        assertEqual (context <> ": both effective projections are absent") (structuralProjectionSnapshotVertices knownCut) (structuralProjectionSnapshotVertices suppressedCut)
        assertEqual
          (context <> ": equal absent topology has the same canonical bytes")
          (structuralProjectionSnapshotCanonicalBytes knownCut)
          (structuralProjectionSnapshotCanonicalBytes suppressedCut)
        assertEqual
          (context <> ": admitted meaning retains exact deletion provenance even after Resolve")
          (Map.singleton subject (OccurrenceStructuralProvenance (structuralOccurrence original) (checkedPublicationId (structuralPublication original)), deleteCause))
          (structuralProjectionSnapshotDeletedControls knownCut)

caseCarrierBaseSuppressedAndDominated :: Assertion
caseCarrierBaseSuppressedAndDominated = do
  let subject = object 155
      hidden = object 156
      high = app remoteA 1 emptyVector NablaRole 20 (nablaRootValue subject (VoidLabel, 0) dataSort)
      low = app remoteB 1 (vectorThrough [(remoteA, 1)]) NablaRole 10 (nablaRootValue subject (VoidLabel, 0) dataSort)
      suppressed = app localHerald 1 emptyVector DeltaRole 30 (rootValue hidden (VoidLabel, 0) dataSort)
      suppression = DirectRegularSortRetirementSuppression dataSort (controlIndex 4)
      currentViews = views [] [] []
      observerViews = reconciliationViews (heraldId 152) Map.empty Set.empty Map.empty Set.empty
  afterHigh <- applyReady currentViews high Nothing emptyState
  afterLow <- applyReady currentViews low Nothing afterHigh
  donor <- applyRetirementSuppressedReady suppression currentViews suppressed afterLow
  prepared <- either (assertFailure . show) pure (prepareStructuralCarrierBaseImport observerViews (captureStructuralCarrierBase donor) emptyState)
  let imported = preparedStructuralCarrierBaseSuccessor prepared
  assertEqual "suppressed and dominated exact history is preserved" donor imported
  assertEqual "suppression floor is copied unchanged" (Just suppression) (structuralAppliedRetirementSuppressionAt (structuralOccurrence suppressed) imported)
  assertEqual "suppressed carrier creates no graph vertex" Nothing (Map.lookup hidden (structuralAppliedVertexProjections imported))
  case prepareStructuralProjectionRefresh currentViews (structuralOccurrence low) Nothing imported of
    Left (StructuralRefreshOccurrenceNotProjectable occurrence') -> assertEqual "dominated occurrence still has no invented resolved meaning" (structuralOccurrence low) occurrence'
    other -> assertFailure ("unexpected dominated import: " <> show other)
  assertEqual "all canonical publications remain available to Controlled reconstruction" 3 (length (structuralCarrierBasePublications (captureStructuralCarrierBase imported)))
  assertEqual "copied suppression/index shape remains valid" (Right ()) (validateStructuralAppliedState imported)

caseCarrierBaseGenesis :: Assertion
caseCarrierBaseGenesis = do
  (_, membership, donor, _, _, _) <- genesisReconciliationFixture
  let bootstraps = checkedInitialBootstraps fixtureCheckedInitialBootstraps
      processes = Map.fromList [(appliedProcessEpochId bootstrap, appliedProcessResidence bootstrap) | bootstrap <- bootstraps]
      observerViews = reconciliationViews (heraldId 152) effectiveSorts (Set.fromList (checkedInitialTopologyVertices (checkedInitialTopologyProjection fixtureCheckedInitialBootstraps))) processes Set.empty
      base = captureStructuralCarrierBase donor
  receiver <- case structuralAppliedStateWithGenesis (checkedSystemId fixtureStep14CheckedGenesis) observerViews membership bootstraps of
    Right (Right ready) -> pure ready
    other -> assertFailure ("observer genesis not ready: " <> show other)
  assertBool "source fixture owns local Stores" (not (Map.null (structuralAppliedRetainedStoreResources donor)))
  prepared <- either (assertFailure . show) pure (prepareStructuralCarrierBaseImport observerViews base receiver)
  let imported = preparedStructuralCarrierBaseSuccessor prepared
  assertEqual "matching genesis import derives exactly the fresh observer projection" receiver imported
  assertBool "unchanged receiver genesis needs no Graph patch" (structuralGraphPatchNull (preparedStructuralCarrierBaseGraphPatch prepared))
  assertEqual "receiver retains no donor Store reservation" Map.empty (structuralAppliedRetainedStoreResources imported)
  assertEqual "capture omits donor local resource and activity" base (captureStructuralCarrierBase imported)
  case prepareStructuralCarrierBaseImport observerViews base emptyState of
    Left StructuralCarrierBaseGenesisMismatch -> pure ()
    other -> assertFailure ("missing receiver genesis was accepted: " <> show other)
  case prepareStructuralCarrierBaseImport observerViews base donor of
    Left StructuralCarrierBaseReceiverNotFresh -> pure ()
    other -> assertFailure ("resource-owning receiver was accepted: " <> show other)
  let dynamic = app localHerald 1 membership NeutralVertexRole 45 (neutralValue (object 157))
  used <- applyReady observerViews dynamic Nothing receiver
  case prepareStructuralCarrierBaseImport observerViews base used of
    Left StructuralCarrierBaseReceiverNotFresh -> pure ()
    other -> assertFailure ("receiver with already admitted history was accepted: " <> show other)

-- The receiver realizes only the final controller. Historical local activations
-- are retained as control facts without manufacturing their former resources.
caseControlBaseResources :: Assertion
caseControlBaseResources = do
  let delta = deltaId 142
      subject = globalObjectIdFromDeltaId delta
      process = processId 143
      homeViews = views [] [(process, localHerald)] [(process, subject)]
      peerViews = reconciliationViews remoteB effectiveSorts Set.empty (Map.singleton process localHerald) Set.empty
      carrier = app remoteA 1 emptyVector DeltaRole 1 (rootValue subject (VoidLabel, 0) dataSort)
      evidence ordinal = freshStoreIncarnationEvidence GeneratedStoreIncarnation delta (storeId ordinal)
      step index label supplied predecessor =
        either
          assertFailure
          pure
          (reconciliationReady "delta label" (prepareStructuralLabelControlRefresh homeViews (labelControlCause 144 index) subject label supplied predecessor))
  initial <- applyReady homeViews carrier Nothing emptyState
  first <- step 1 (releasedLabel (ProcessLabel process, 1)) (Just (evidence 145)) initial
  passive <- step 2 (releasedLabel (VoidLabel, 2)) Nothing (preparedStructuralSuccessor first)
  final <- step 3 (releasedLabel (ProcessLabel process, 3)) (Just (evidence 146)) (preparedStructuralSuccessor passive)
  let donor = preparedStructuralSuccessor final
      base = captureStructuralControlBase donor
  assertEqual "fixture retains both original local resources" 2 (Map.size (structuralAppliedRetainedStoreResources donor))
  case prepareStructuralControlBaseImport homeViews base Map.empty initial of
    Right (StructuralControlBaseHeld dependencies) ->
      assertEqual "only final local activation requires an incarnation" (Set.singleton (FreshStoreIncarnationDependency delta)) dependencies
    other -> assertFailure ("unexpected local activation preparation: " <> show other)
  let preparation = prepareStructuralControlBaseImport homeViews base (Map.singleton subject (evidence 147)) initial
  case preparation of
    Right (StructuralControlBaseReady steps) ->
      assertEqual "only the present consequence supplies a Store/debt cause" [Just (labelControlCause 144 3), Nothing] (map structuralControlBaseStepCause steps)
    other -> assertFailure ("unexpected receiver preparation: " <> show other)
  (imported, prepared) <- applyControlBaseReady initial preparation
  assertEqual "the receiver keeps only its newly supplied incarnation" (Set.singleton (storeId 147)) (Map.keysSet (structuralAppliedRetainedStoreResources imported))
  assertEqual "current receiver activity matches replay" (structuralAppliedVertexProjections donor) (structuralAppliedVertexProjections imported)
  assertEqual "all past causes retain their original coordinates" (structuralAppliedControlHistory donor) (structuralAppliedControlHistory imported)
  assertEqual "present consequence and historical attachment are separate" 2 (length prepared)
  assertHistoryOnlyControlImport (last prepared)
  forM_ [0 .. 3] $ \index -> do
    donorProjection <- projectionAtControl homeViews (vectorThrough [(remoteA, 1)]) (controlIndex index) donor
    importedProjection <- projectionAtControl homeViews (vectorThrough [(remoteA, 1)]) (controlIndex index) imported
    assertEqual "every historical digest agrees" (structuralProjectionSnapshotCanonicalBytes donorProjection) (structuralProjectionSnapshotCanonicalBytes importedProjection)
  (_, exactReplay) <- applyControlBaseReady imported (prepareStructuralControlBaseImport homeViews base Map.empty imported)
  assertEqual "exact repeated base import performs no work" [] exactReplay
  peerInitial <- applyReady peerViews carrier Nothing emptyState
  (peer, _) <- applyControlBaseReady peerInitial (prepareStructuralControlBaseImport peerViews base Map.empty peerInitial)
  assertEqual "remote import never borrows donor resources" Map.empty (structuralAppliedRetainedStoreResources peer)
  assertEqual "remote activity is derived from receiver residence" (Just (LiveProcessController process localHerald, RemoteController process localHerald)) (rootControllerActivity =<< Map.lookup subject (structuralAppliedVertexProjections peer))
  deletion <- step 4 (releasedDeleted 4) Nothing donor
  let deleted = preparedStructuralSuccessor deletion
  (importedDeletion, deletionPreparations) <- applyControlBaseReady initial (prepareStructuralControlBaseImport homeViews (captureStructuralControlBase deleted) Map.empty initial)
  assertEqual "terminal current state does not replay past activations" Map.empty (structuralAppliedRetainedStoreResources importedDeletion)
  assertEqual "terminal history is preserved exactly" (structuralAppliedControlHistory deleted) (structuralAppliedControlHistory importedDeletion)
  assertEqual "terminal import has no current local destination" [] (structuralAppliedLocalStores importedDeletion)
  assertHistoryOnlyControlImport (last deletionPreparations)

caseControlBaseDependencies :: Assertion
caseControlBaseDependencies = do
  (currentViews, subject, _, processB, initial) <- either assertFailure pure labelControlFixture
  handoff <- either assertFailure pure (labelControlReady currentViews (labelControlCause 148 1) subject (releasedLabel (ProcessLabel processB, 1)) initial)
  let donor = preparedStructuralSuccessor handoff
      base = captureStructuralControlBase donor
      subjectDependency = Set.singleton (StructuralControlCarrierDependency subject)
  case prepareStructuralControlBaseImport currentViews base Map.empty emptyState of
    Right (StructuralControlBaseHeld dependencies) -> assertEqual "exact missing carrier" subjectDependency dependencies
    other -> assertFailure ("unexpected missing carrier result: " <> show other)
  (_, emptyPreparations) <- applyControlBaseReady emptyState (prepareStructuralControlBaseImport currentViews (restrictStructuralControlBaseToObjects Set.empty base) Map.empty emptyState)
  assertEqual "restricted staging cannot create an unknown carrier" [] emptyPreparations
  (imported, _) <- applyControlBaseReady initial (prepareStructuralControlBaseImport currentViews (restrictStructuralControlBaseToObjects (Set.singleton subject) base) Map.empty initial)
  assertEqual "known-carrier restriction retains the complete applicable base" donor imported
  conflicting <- either assertFailure pure (labelControlReady currentViews (labelControlCause 149 1) subject (releasedLabel (VoidLabel, 1)) initial)
  case prepareStructuralControlBaseImport currentViews base Map.empty (preparedStructuralSuccessor conflicting) of
    Left (StructuralControlBaseHistoryConflict index object') -> assertEqual "incompatible existing history cannot be silently replaced" (controlIndex 1, subject) (index, object')
    other -> assertFailure ("unexpected conflicting history result: " <> show other)
  let carrier = app remoteB 1 emptyVector NablaRole 20 (nablaRootValue subject (ProcessLabel (processId 177), 0) dataSort)
      suppression = DirectRegularSortRetirementSuppression dataSort (controlIndex 2)
  suppressed <- applyRetirementSuppressedReady suppression currentViews carrier emptyState
  (importedSuppressed, suppressedPreparations) <- applyControlBaseReady suppressed (prepareStructuralControlBaseImport currentViews base Map.empty suppressed)
  assertEqual "a retained suppressed carrier satisfies the exact history dependency" (structuralAppliedControlHistory donor) (structuralAppliedControlHistory importedSuppressed)
  assertEqual "historical controls cannot resurrect retired carrier projections" Map.empty (structuralAppliedVertexProjections importedSuppressed)
  forM_ suppressedPreparations assertHistoryOnlyControlImport

propControlBaseHistory :: [Bool] -> Bool -> Property
propControlBaseHistory choices local = QC.ioProperty $ do
  (homeViews, subject, processA, processB, homeInitial) <- either assertFailure pure labelControlFixture
  let currentViews = if local then homeViews else reconciliationViews remoteB effectiveSorts Set.empty (Map.fromList [(processA, localHerald), (processB, remoteA)]) Set.empty
      carrier = app remoteB 1 emptyVector NablaRole 20 (nablaRootValue subject (ProcessLabel processA, 0) dataSort)
      controls = zip [1 ..] (False : True : take 12 choices)
      replay view initial =
        foldM
          (\predecessor (index, chooseA) -> preparedStructuralSuccessor <$> either assertFailure pure (labelControlReady view (labelControlCause 150 index) subject (releasedLabel (ProcessLabel (if chooseA then processA else processB), index)) predecessor))
          initial
          controls
  donor <- replay homeViews homeInitial
  receiverInitial <- applyReady currentViews carrier Nothing emptyState
  reference <- replay currentViews receiverInitial
  (imported, _) <- applyControlBaseReady receiverInitial (prepareStructuralControlBaseImport currentViews (captureStructuralControlBase donor) Map.empty receiverInitial)
  assertEqual "current controller and immutable history match full event replay" reference imported
  if local
    then pure ()
    else do
      paired <- either (assertFailure . show) pure (prepareStructuralCarrierBaseImport currentViews (captureStructuralCarrierBase donor) emptyState)
      assertEqual "paired carrier/control import matches complete event replay" reference (preparedStructuralCarrierBaseSuccessor paired)
      assertEqual "paired capture is independent of receiver activity" (captureStructuralCarrierBase donor) (captureStructuralCarrierBase (preparedStructuralCarrierBaseSuccessor paired))
  forM_ [0 .. fromIntegral (length controls)] $ \index -> do
    expected <- projectionAtControl currentViews (vectorThrough [(remoteB, 1)]) (controlIndex index) reference
    actual <- projectionAtControl currentViews (vectorThrough [(remoteB, 1)]) (controlIndex index) imported
    assertEqual "historical cut matches every replay prefix" (structuralProjectionSnapshotCanonicalBytes expected) (structuralProjectionSnapshotCanonicalBytes actual)
  pure True

applyControlBaseReady ::
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralControlBasePreparation ->
  IO (StructuralAppliedState, [PreparedStructuralReconciliation])
applyControlBaseReady initial result = do
  steps <- case result of
    Right (StructuralControlBaseReady ready) -> pure ready
    other -> assertFailure ("control base import not ready: " <> show other)
  forM_ steps $ \step -> case structuralControlBaseStepCause step of
    Nothing -> assertHistoryOnlyControlImport (structuralControlBaseStepPreparation step)
    Just _ -> pure ()
  let prepared = map structuralControlBaseStepPreparation steps
  final <-
    foldM
      ( \predecessor ready -> do
          assertEqual "atomic preparation uses the exact preceding owner" predecessor (preparedStructuralPredecessor ready)
          _ <- assertPreparedStateChange ready
          let successor = preparedStructuralSuccessor ready
          assertEqual "prepared owner retains a coherent indexed control history" (Right ()) (validateStructuralAppliedState successor)
          pure successor
      )
      initial
      prepared
  pure (final, prepared)

assertHistoryOnlyControlImport :: PreparedStructuralReconciliation -> Assertion
assertHistoryOnlyControlImport prepared = do
  assertBool "history attachment has no Graph consequence" (structuralGraphPatchNull (preparedStructuralGraphPatch prepared))
  assertEqual "history attachment has no Placement consequence" Map.empty (placementPatchChanges (preparedStructuralPlacementPatch prepared))
  assertEqual "history attachment has no Store consequence" Map.empty (storePatchChanges (preparedStructuralStorePatch prepared))
  assertEqual "history attachment retains no past Store" Map.empty (storePatchRetainedResourceChanges (preparedStructuralStorePatch prepared))
  assertBool "history attachment creates no debts" (structuralDebtSetNull (preparedStructuralDebts prepared))

caseLabelControlHandoff :: Assertion
caseLabelControlHandoff = do
  (currentViews, subject, processA, processB, initial) <-
    either assertFailure pure labelControlFixture
  let firstCause = labelControlCause 180 1
      repeatedCause = labelControlCause 181 2
      handbackCause = labelControlCause 182 3
  first <-
    either
      assertFailure
      pure
      ( labelControlReady
          currentViews
          firstCause
          subject
          (releasedLabel ((ProcessLabel processB, 1)))
          initial
      )
  let afterFirst = preparedStructuralSuccessor first
  _ <- assertPreparedStateChange first
  assertBool "actual control handoff changes reconciliation" (preparedStructuralChangesState first)
  assertEqual
    "the first genuine handoff installs its exact control cause"
    [ ( subject,
        StructuralControllerOverlayView
          firstCause
          (LiveProcessController processB remoteA)
      )
    ]
    (structuralAppliedControlOverlays afterFirst)

  repeated <-
    either
      assertFailure
      pure
      ( labelControlReady
          currentViews
          repeatedCause
          subject
          (releasedLabel ((ProcessLabel processB, 2)))
          afterFirst
      )
  let afterRepeated = preparedStructuralSuccessor repeated
  _ <- assertPreparedStateChange repeated
  assertBool "same-controller release reports no semantic mutation" (not (preparedStructuralChangesState repeated))
  assertBool
    "a same-controller release has no graph consequence"
    (structuralGraphPatchNull (preparedStructuralGraphPatch repeated))
  assertEqual
    "a same-controller release retains the first genuine cause"
    afterFirst
    afterRepeated

  handback <-
    either
      assertFailure
      pure
      ( labelControlReady
          currentViews
          handbackCause
          subject
          (releasedLabel ((ProcessLabel processA, 3)))
          afterRepeated
      )
  assertEqual
    "a later genuine handback replaces the retained control cause"
    [ ( subject,
        StructuralControllerOverlayView
          handbackCause
          (LiveProcessController processA localHerald)
      )
    ]
    (structuralAppliedControlOverlays (preparedStructuralSuccessor handback))

caseLabelControlDelete :: Assertion
caseLabelControlDelete = do
  (currentViews, subject, processA, _, initial) <-
    either assertFailure pure labelControlFixture
  let deleteCause = labelControlCause 183 4
      restoreCause = labelControlCause 184 5
  deletion <-
    either
      assertFailure
      pure
      (labelControlReady currentViews deleteCause subject (releasedDeleted 1) initial)
  let deleted = preparedStructuralSuccessor deletion
  assertEqual
    "delete retains one exact terminal overlay"
    [(subject, StructuralDeletedOverlayView deleteCause)]
    (structuralAppliedControlOverlays deleted)
  assertEqual
    "delete removes the dynamic vertex projection"
    Nothing
    (Map.lookup subject (structuralAppliedVertexProjections deleted))
  assertPortableControlBase deleted
  (imported, _) <- applyControlBaseReady initial (prepareStructuralControlBaseImport currentViews (captureStructuralControlBase deleted) Map.empty initial)
  assertEqual "terminal base matches ordinary deletion replay" deleted imported
  (_, replay) <- applyControlBaseReady imported (prepareStructuralControlBaseImport currentViews (captureStructuralControlBase deleted) Map.empty imported)
  assertEqual "terminal import replay creates no transaction" [] replay
  case prepareStructuralLabelControlRefresh
    currentViews
    restoreCause
    subject
    (releasedLabel ((ProcessLabel processA, 1)))
    Nothing
    deleted of
    Left (StructuralControlRestoresDeletedObject actual) ->
      assertEqual "terminal object" subject actual
    Left problem ->
      assertFailure ("unexpected restore problem: " <> show problem)
    Right outcome ->
      assertFailure ("deleted topology was restored: " <> show outcome)

caseProcessEndControls :: Assertion
caseProcessEndControls = do
  let processA = processId 185
      processB = processId 186
      firstNabla = checked "first End nabla" (mkNablaId (fixtureIdentifierBytes 187))
      secondNabla = checked "second End nabla" (mkNablaId (fixtureIdentifierBytes 188))
      otherNabla = checked "unrelated End nabla" (mkNablaId (fixtureIdentifierBytes 189))
      firstObject = globalObjectIdFromNablaId firstNabla
      secondObject = globalObjectIdFromNablaId secondNabla
      otherObject = globalObjectIdFromNablaId otherNabla
      currentViews =
        views
          []
          [(processA, remoteA), (processB, remoteB)]
          []
      first =
        app
          localHerald
          1
          emptyVector
          NablaRole
          21
          (nablaRootValue firstObject ((ProcessLabel processA, 0)) dataSort)
      second =
        app
          remoteA
          1
          emptyVector
          NablaRole
          22
          (nablaRootValue secondObject ((ProcessLabel processA, 0)) dataSort)
      unrelated =
        app
          remoteB
          1
          emptyVector
          NablaRole
          23
          (nablaRootValue otherObject ((ProcessLabel processB, 0)) dataSort)
  afterFirst <- applyReady currentViews first Nothing emptyState
  afterSecond <- applyReady currentViews second Nothing afterFirst
  beforeEnd <- applyReady currentViews unrelated Nothing afterSecond
  let cause = checked "process End cause" (processEndCause processA (controlIndex 7))
  prepared <-
    either
      (assertFailure . ("process End topology problem: " <>) . show)
      pure
      (prepareStructuralProcessEndRefreshes currentViews cause processA beforeEnd)
  assertEqual
    "every and only controlled dynamic occurrence is refreshed"
    ( Set.fromList
        [ OccurrenceStructuralControlTarget (structuralApplicationOccurrence first),
          OccurrenceStructuralControlTarget (structuralApplicationOccurrence second)
        ]
    )
    (Set.fromList (fmap fst prepared))
  afterEnd <- case reverse prepared of
    (_, finalRefresh) : _ -> pure (preparedStructuralSuccessor finalRefresh)
    [] -> assertFailure "process End prepared no affected occurrence"
  assertEqual
    "every affected root retains the exact End cause and zombie controller"
    ( Map.fromList
        [ (firstObject, StructuralControllerOverlayView cause (ZombieProcessController processA)),
          (secondObject, StructuralControllerOverlayView cause (ZombieProcessController processA))
        ]
    )
    (Map.fromList (structuralAppliedControlOverlays afterEnd))
  assertEqual
    "the first ended root is a zombie"
    (Just (ZombieProcessController processA))
    (projectedNablaController firstObject afterEnd)
  assertEqual
    "the second ended root is a zombie"
    (Just (ZombieProcessController processA))
    (projectedNablaController secondObject afterEnd)
  assertEqual
    "an unrelated controller remains live and has no overlay"
    (Just (LiveProcessController processB remoteB))
    (projectedNablaController otherObject afterEnd)
  (imported, _) <- applyControlBaseReady beforeEnd (prepareStructuralControlBaseImport currentViews (captureStructuralControlBase afterEnd) Map.empty beforeEnd)
  assertEqual "imported End consequences match ordinary event replay" afterEnd imported

-- Exercise local and remote ownership, both root kinds, and independently
-- chosen ordered End positions. Possession is deliberately still present:
-- canonical End alone must prevent creating a local Store.
propDelayedProcessEnd :: Bool -> Bool -> Positive Int -> Property
propDelayedProcessEnd isNabla isLocal (Positive supplied) = QC.ioProperty $ do
  let process = processId 185
      subject = object 187
      residence = if isLocal then localHerald else remoteA
      endIndex = controlIndex (1 + fromIntegral (supplied `mod` 20))
      beforeIndex = controlIndex (fromIntegral (supplied `mod` 20))
      currentViews =
        reconciliationViewsWithEndedProcesses
          (Map.singleton process (residence, endIndex))
          (views [] [] [(process, subject)])
      role = if isNabla then NablaRole else DeltaRole
      value = if isNabla then nablaRootValue else rootValue
      candidate = app remoteA 1 emptyVector role 21 (value subject (ProcessLabel process, 3) dataSort)
      frontier = vectorThrough [(remoteA, 1)]
      cause = checked "delayed End cause" (processEndCause process endIndex)
  prepared <- prepareReady currentViews candidate Nothing emptyState
  let successor = preparedStructuralSuccessor prepared
  assertEqual "retained history is well formed" (Right ()) (validateStructuralAppliedState successor)
  assertEqual
    "End has its actual ordered cause"
    [(subject, StructuralControllerOverlayView cause (ZombieProcessController process))]
    (structuralAppliedControlOverlays successor)
  assertEqual
    "current root is a passive zombie"
    (Just (ZombieProcessController process, PassiveRoot))
    (rootControllerActivity =<< Map.lookup subject (structuralAppliedVertexProjections successor))
  assertEqual "no active Store" [] (structuralAppliedLocalStores successor)
  assertEqual "no historical Store created after death" Map.empty (structuralAppliedRetainedStoreResources successor)
  before <- projectionAtControl currentViews frontier beforeIndex successor
  after <- projectionAtControl currentViews frontier endIndex successor
  assertEqual
    "historical controller remains original process"
    (Just (LiveProcessController process residence))
    (fst <$> (rootControllerActivity =<< Map.lookup subject (structuralProjectionSnapshotVertices before)))
  assertEqual
    "End affects the root only from its exact position"
    (Just (ZombieProcessController process, PassiveRoot))
    (rootControllerActivity =<< Map.lookup subject (structuralProjectionSnapshotVertices after))
  replay <- prepareReady currentViews candidate Nothing successor
  assertEqual "replay is unchanged" successor (preparedStructuralSuccessor replay)
  refreshed <- refreshReady currentViews (structuralOccurrence candidate) Nothing successor
  assertEqual "refresh cannot reactivate ended process" successor (preparedStructuralSuccessor refreshed)
  pure True

caseDelayedEndUnrelated :: Assertion
caseDelayedEndUnrelated = do
  let process = processId 185
      other = processId 186
      subject = object 187
      currentViews =
        reconciliationViewsWithEndedProcesses
          (Map.singleton process (localHerald, controlIndex 7))
          (views [] [(other, remoteB)] [])
  forM_ [False, True] $ \isNabla ->
    forM_
      [ (VoidLabel, VoidController),
        (ZombieLabel process, ZombieProcessController process),
        (ProcessLabel other, LiveProcessController other remoteB)
      ]
      $ \(owner, expected) -> do
        let role = if isNabla then NablaRole else DeltaRole
            value = if isNabla then nablaRootValue else rootValue
            candidate = app remoteA 1 emptyVector role 21 (value subject (owner, 0) dataSort)
        successor <- applyReady currentViews candidate Nothing emptyState
        assertEqual
          "unrelated original label unchanged"
          (Just expected)
          (fst <$> (rootControllerActivity =<< Map.lookup subject (structuralAppliedVertexProjections successor)))
        assertEqual "unrelated End adds no object overlay" [] (structuralAppliedControlOverlays successor)
  let withoutSort =
        reconciliationViewsWithEndedProcesses
          (Map.singleton process (localHerald, controlIndex 7))
          (reconciliationViews localHerald (Map.delete dataSort effectiveSorts) Set.empty Map.empty Set.empty)
      missingSort = app remoteA 1 emptyVector NablaRole 21 (nablaRootValue subject (ProcessLabel process, 0) dataSort)
  assertHeld
    "End still waits for the carried sort"
    (Set.singleton (EffectiveSortDependency dataSort))
    (prepareStructuralReconciliation withoutSort missingSort Nothing emptyState)
  let unknown = app remoteA 1 emptyVector NablaRole 21 (nablaRootValue subject (ProcessLabel (processId 189), 0) dataSort)
  assertHeld
    "an unknown process still holds"
    (Set.singleton (StructuralProcessDependency (processId 189)))
    (prepareStructuralReconciliation currentViews unknown Nothing emptyState)

caseDelayedEndControlHistory :: Assertion
caseDelayedEndControlHistory = do
  let process = processId 185
      other = processId 186
      subject = object 187
      baseViews = views [] [(process, remoteA), (other, remoteB)] []
      currentViews =
        reconciliationViewsWithEndedProcesses
          (Map.singleton process (remoteA, controlIndex 7))
          baseViews
      root dot predecessor =
        app
          remoteA
          dot
          predecessor
          NablaRole
          dot
          (nablaRootValue subject (ProcessLabel process, 0) dataSort)
      first = root 1 emptyVector
  initial <- applyReady baseViews first Nothing emptyState
  handed <-
    either
      (assertFailure . show)
      pure
      (labelControlReady baseViews (labelControlCause 210 8) subject (releasedLabel (ProcessLabel other, 1)) initial)
  let afterHandoff = preparedStructuralSuccessor handed
      second = root 2 (vectorThrough [(remoteA, 1)])
  late <- applyReady currentViews second Nothing afterHandoff
  assertEqual
    "newer Q handoff remains current"
    (Just (LiveProcessController other remoteB))
    (projectedNablaController subject late)
  before <- projectionAtControl currentViews (vectorThrough [(remoteA, 2)]) (controlIndex 6) late
  ended <- projectionAtControl currentViews (vectorThrough [(remoteA, 2)]) (controlIndex 7) late
  assertEqual
    "prefix before End preserves P"
    (Just (LiveProcessController process remoteA))
    (fst <$> (rootControllerActivity =<< Map.lookup subject (structuralProjectionSnapshotVertices before)))
  assertEqual
    "late End history exists before newer Q handoff"
    (Just (ZombieProcessController process))
    (fst <$> (rootControllerActivity =<< Map.lookup subject (structuralProjectionSnapshotVertices ended)))
  deleted <-
    either
      (assertFailure . show)
      pure
      (labelControlReady currentViews (labelControlCause 211 9) subject (releasedDeleted 2) late)
  lateDeleted <- applyReady currentViews (root 3 (vectorThrough [(remoteA, 2)])) Nothing (preparedStructuralSuccessor deleted)
  assertEqual
    "terminal deletion remains current"
    Nothing
    (Map.lookup subject (structuralAppliedVertexProjections lateDeleted))
  assertEqual "late history preserves owner invariants" (Right ()) (validateStructuralAppliedState lateDeleted)

caseDelayedReleasedControllerEnd :: Assertion
caseDelayedReleasedControllerEnd = do
  let process = processId 185
      other = processId 186
      delta = deltaId 187
      subject = globalObjectIdFromDeltaId delta
      baseViews = views [] [(process, localHerald), (other, remoteB)] [(process, subject)]
      currentViews =
        reconciliationViewsWithEndedProcesses
          (Map.singleton other (remoteB, controlIndex 7))
          baseViews
      root dot predecessor =
        app
          remoteA
          dot
          predecessor
          DeltaRole
          dot
          (rootValue subject (ProcessLabel process, 0) dataSort)
      fresh = freshStoreIncarnationEvidence ConfiguredManifestStoreIncarnation delta (storeId 188)
  initial <- applyReady baseViews (root 1 emptyVector) (Just fresh) emptyState
  handed <-
    either
      assertFailure
      pure
      (labelControlReady baseViews (labelControlCause 210 6) subject (releasedLabel (ProcessLabel other, 1)) initial)
  prepared <-
    prepareReady
      currentViews
      (root 2 (vectorThrough [(remoteA, 1)]))
      Nothing
      (preparedStructuralSuccessor handed)
  let successor = preparedStructuralSuccessor prepared
  assertEqual
    "current released controller becomes a passive zombie"
    (Just (ZombieProcessController other, PassiveRoot))
    (rootControllerActivity =<< Map.lookup subject (structuralAppliedVertexProjections successor))
  assertEqual
    "intrinsic live owner gets no new Store"
    []
    (structuralAppliedLocalStores successor)
  assertEqual
    "no additional retained resource allocated"
    (structuralAppliedRetainedStoreResources initial)
    (structuralAppliedRetainedStoreResources successor)

rootControllerActivity :: StructuralVertexProjection -> Maybe (ControllerProjection, RootActivityProjection)
rootControllerActivity = \case
  NablaVertexProjection _ _ _ controller activity -> Just (controller, activity)
  DeltaVertexProjection _ _ _ controller activity -> Just (controller, activity)
  _ -> Nothing

caseGenesisControlPrefixes :: Assertion
caseGenesisControlPrefixes = do
  (currentViews, membership, initial, _, writerRoot, remoteBootstrap) <-
    genesisReconciliationFixture
  let subject = appliedRootObjectId writerRoot
      remoteProcess = appliedProcessEpochId remoteBootstrap
      handoffCause = labelControlCause 210 1
      sameControllerCause = labelControlCause 211 2
      deleteCause = labelControlCause 212 3
  initialSnapshot <- projectionAtControl currentViews membership (controlIndex 0) initial
  initialVertex <-
    maybe
      (assertFailure "checked genesis writer is absent from its seeded projection")
      (pure . structuralVertexProjectionVertex)
      (Map.lookup subject (structuralProjectionSnapshotVertices initialSnapshot))
  assertBool
    "the selected genesis writer participates in checked initial topology"
    ( any
        (\edge -> edgeSource edge == initialVertex || edgeDestination edge == initialVertex)
        (checkedInitialTopologyEdges (checkedInitialTopologyProjection fixtureCheckedInitialBootstraps))
    )

  handoff <-
    either
      assertFailure
      pure
      ( labelControlReady
          currentViews
          handoffCause
          subject
          (releasedLabel ((ProcessLabel remoteProcess, 1)))
          initial
      )
  let afterHandoff = preparedStructuralSuccessor handoff
  atHandoff <- projectionAtControl currentViews membership (controlIndex 1) afterHandoff
  assertEqual
    "the present genesis entry binds the exact handoff cause"
    (Just handoffCause)
    ( structuralProjectionDigestEntryControlCause
        =<< Map.lookup subject (structuralProjectionSnapshotDigestEntries atHandoff)
    )
  olderAfterHandoff <-
    projectionAtControl currentViews membership (controlIndex 0) afterHandoff
  assertEqual
    "an ahead handoff state reconstructs the exact genesis prefix"
    (structuralProjectionSnapshotCanonicalBytes initialSnapshot)
    (structuralProjectionSnapshotCanonicalBytes olderAfterHandoff)

  repeated <-
    either
      assertFailure
      pure
      ( labelControlReady
          currentViews
          sameControllerCause
          subject
          (releasedLabel ((ProcessLabel remoteProcess, 2)))
          afterHandoff
      )
  let afterRepeated = preparedStructuralSuccessor repeated
  assertEqual "same-controller genesis control is a state no-op" afterHandoff afterRepeated
  assertEqual
    "same-controller genesis control does not churn retained control history"
    (structuralControlCheckpointAt (controlIndex 1) afterHandoff)
    (structuralControlCheckpointAt (controlIndex 2) afterRepeated)

  deletion <-
    either
      assertFailure
      pure
      (labelControlReady currentViews deleteCause subject (releasedDeleted 3) afterRepeated)
  let afterDelete = preparedStructuralSuccessor deletion
  assertEqual
    "the complete retained control history is owner-valid"
    (Right ())
    (validateStructuralAppliedState afterDelete)
  case validateStructuralAppliedState
    ( relocateStructuralControlHistoryForInvariantTest
        (controlIndex 3)
        (controlIndex 4)
        subject
        afterDelete
    ) of
    Left StructuralAppliedControlHistoryCauseIndexMismatch {} -> pure ()
    other -> assertFailure ("relocated control cause escaped validation: " <> show other)
  assertEqual
    "current overlays must be the exact fold of retained history"
    (Left StructuralAppliedCurrentControlOverlayMismatch)
    ( validateStructuralAppliedState
        (dropStructuralControlOverlayForInvariantTest subject afterDelete)
    )
  atDelete <- projectionAtControl currentViews membership (controlIndex 3) afterDelete
  assertEqual
    "deleted genesis presence retains diagnostic control provenance"
    (Just (GenesisStructuralProvenance subject, deleteCause))
    (Map.lookup subject (structuralProjectionSnapshotDeletedControls atDelete))
  assertEqual
    "deleted genesis writer is absent from the induced projection"
    Nothing
    (Map.lookup subject (structuralProjectionSnapshotVertices atDelete))
  assertBool
    "deleted genesis writer removes every incident projected edge"
    ( all
        ( \owned ->
            let edge = ownedEdgeProjectionPayload owned
             in edgeSource edge /= initialVertex && edgeDestination edge /= initialVertex
        )
        (Map.elems (structuralProjectionSnapshotEdges atDelete))
    )
  assertBool
    "removing the writer and its incident edges changes the normalized topology transcript"
    ( structuralProjectionSnapshotCanonicalBytes atDelete
        /= structuralProjectionSnapshotCanonicalBytes atHandoff
    )
  olderFromAhead <-
    projectionAtControl currentViews membership (controlIndex 0) afterDelete
  assertEqual
    "a state ahead of delete still reconstructs the older genesis route topology"
    (structuralProjectionSnapshotCanonicalBytes initialSnapshot)
    (structuralProjectionSnapshotCanonicalBytes olderFromAhead)

caseGenesisEnvironmentControls :: Assertion
caseGenesisEnvironmentControls = do
  (currentViews, membership, initial, localBootstrap, _, remoteBootstrap) <- genesisReconciliationFixture
  let remoteProcess = appliedProcessEpochId remoteBootstrap
      handoffCause = labelControlCause 213 1
      deleteCause = labelControlCause 214 2
      endpoints = Set.fromList (map appliedRootObjectId (appliedProcessRoots localBootstrap))
  initialSnapshot <- projectionAtControl currentViews membership (controlIndex 0) initial
  forM_ (appliedProcessEnvironmentObjects localBootstrap) $ \subject -> do
    handoff <- either assertFailure pure (labelControlReady currentViews handoffCause subject (releasedLabel (ProcessLabel remoteProcess, 1)) initial)
    let afterHandoff = preparedStructuralSuccessor handoff
    assertEqual
      "handoff changes only endpoint activity in the structural projection"
      (if subject `Set.member` endpoints then Just (StructuralControllerOverlayView handoffCause (LiveProcessController remoteProcess (appliedProcessResidence remoteBootstrap))) else Nothing)
      (lookup subject (structuralAppliedControlOverlays afterHandoff))
    deletion <- either assertFailure pure (labelControlReady currentViews deleteCause subject (releasedDeleted 2) afterHandoff)
    let afterDelete = preparedStructuralSuccessor deletion
    assertEqual "ordinary deletion retains valid structural history" (Right ()) (validateStructuralAppliedState afterDelete)
    atDelete <- projectionAtControl currentViews membership (controlIndex 2) afterDelete
    assertEqual
      "each manifest object's deletion has its exact ordered cause"
      (Just (GenesisStructuralProvenance subject, deleteCause))
      (Map.lookup subject (structuralProjectionSnapshotDeletedControls atDelete))
    assertBool
      "deletion removes the object's vertex or edge projection"
      (Map.notMember subject (structuralProjectionSnapshotVertices atDelete) && Map.notMember subject (structuralProjectionSnapshotEdges atDelete))
    earlier <- projectionAtControl currentViews membership (controlIndex 0) afterDelete
    assertEqual
      "deletion preserves the exact genesis prefix"
      (structuralProjectionSnapshotCanonicalBytes initialSnapshot)
      (structuralProjectionSnapshotCanonicalBytes earlier)

caseGenesisProcessEndControls :: Assertion
caseGenesisProcessEndControls = do
  (currentViews, _, initial, localBootstrap, _, _) <- genesisReconciliationFixture
  let process = appliedProcessEpochId localBootstrap
      cause = checked "genesis End cause" (processEndCause process (controlIndex 7))
      expectedTargets =
        Set.fromList
          [ GenesisStructuralControlTarget (appliedRootObjectId root)
          | root <- appliedProcessRoots localBootstrap
          ]
  prepared <-
    either
      (assertFailure . ("genesis process End topology problem: " <>) . show)
      pure
      (prepareStructuralProcessEndRefreshes currentViews cause process initial)
  assertEqual
    "End targets each checked endpoint without a synthetic occurrence"
    expectedTargets
    (Set.fromList (fmap fst prepared))
  afterEnd <- case reverse prepared of
    (_, finalRefresh) : _ -> pure (preparedStructuralSuccessor finalRefresh)
    [] -> assertFailure "genesis End prepared no affected root"
  assertBool
    "End passivates every local checked-genesis reader Store"
    ( null
        [ store
        | store <- structuralAppliedLocalStores afterEnd,
          dynamicStoreProcess store == process
        ]
    )
  assertEqual
    "every ended genesis endpoint retains the exact ordered zombie cause"
    ( Map.fromList
        [ ( objectId,
            StructuralControllerOverlayView cause (ZombieProcessController process)
          )
        | objectId <- Set.toAscList (Set.map targetObject expectedTargets)
        ]
    )
    ( Map.fromList
        [ (objectId, overlay)
        | (objectId, overlay) <- structuralAppliedControlOverlays afterEnd,
          Set.member objectId (Set.map targetObject expectedTargets)
        ]
    )
  where
    targetObject = \case
      GenesisStructuralControlTarget objectId -> objectId
      PublishedBaselineStructuralControlTarget objectId _ -> objectId
      OccurrenceStructuralControlTarget _ -> error "genesis fixture produced an occurrence target"

genesisReconciliationFixture ::
  IO
    ( ReconciliationViews,
      StructuralVersionVector,
      StructuralAppliedState,
      AppliedProcessBootstrap,
      AppliedRoot,
      AppliedProcessBootstrap
    )
genesisReconciliationFixture = do
  members <-
    maybe
      (assertFailure "checked genesis has no active Herald membership")
      pure
      (NonEmpty.nonEmpty (checkedActiveHeraldEpochs fixtureStep14CheckedGenesis))
  generation <-
    either
      (assertFailure . ("checked genesis membership: " <>) . show)
      pure
      ( genesisHeraldMembershipGeneration
          (checkedSystemId fixtureStep14CheckedGenesis)
          members
      )
  let membership = emptyStructuralVersionVector generation
      bootstraps = checkedInitialBootstraps fixtureCheckedInitialBootstraps
      topology = checkedInitialTopologyProjection fixtureCheckedInitialBootstraps
      processes =
        Map.fromList
          [ (appliedProcessEpochId bootstrap, appliedProcessResidence bootstrap)
          | bootstrap <- bootstraps
          ]
      possessions =
        Set.fromList
          [ (appliedProcessEpochId bootstrap, subject)
          | bootstrap <- bootstraps,
            appliedProcessResidence bootstrap == localHerald,
            subject <- appliedProcessEnvironmentObjects bootstrap
          ]
      currentViews =
        reconciliationViews
          localHerald
          effectiveSorts
          (Set.fromList (checkedInitialTopologyVertices topology))
          processes
          possessions
      localWriters =
        [ (bootstrap, root)
        | bootstrap <- bootstraps,
          appliedProcessResidence bootstrap == localHerald,
          root <- appliedProcessRoots bootstrap,
          WriterRoot {} <- [appliedRootRole root]
        ]
      remoteBootstrap =
        find ((/= localHerald) . appliedProcessResidence) bootstraps
  (localBootstrap, writerRoot) <- case localWriters of
    selected : _ -> pure selected
    [] -> assertFailure "checked genesis fixture has no local writer root"
  remote <-
    maybe
      (assertFailure "checked genesis fixture has no remote configured process")
      pure
      remoteBootstrap
  seeded <-
    either
      (assertFailure . ("checked genesis reconciliation: " <>) . show)
      pure
      (structuralAppliedStateWithGenesis (checkedSystemId fixtureStep14CheckedGenesis) currentViews membership bootstraps)
  initial <- case seeded of
    Left dependencies ->
      assertFailure ("checked genesis reconciliation held: " <> show dependencies)
    Right ready -> pure ready
  pure (currentViews, membership, initial, localBootstrap, writerRoot, remote)

projectionAtControl ::
  ReconciliationViews ->
  StructuralVersionVector ->
  ControlIndex ->
  StructuralAppliedState ->
  IO StructuralProjectionSnapshot
projectionAtControl currentViews frontier prefix state =
  case prepareStructuralProjectionAtVector currentViews frontier prefix state of
    Right (Right snapshot) -> pure snapshot
    Right (Left dependencies) ->
      assertFailure ("unexpected controlled projection hold: " <> show dependencies)
    Left problem ->
      assertFailure ("unexpected controlled projection problem: " <> show problem)

propPreparedNablaProjections :: Property
propPreparedNablaProjections =
  QC.forAll (QC.listOf1 (QC.elements [0 :: Int, 1, 2])) $ \choices ->
    QC.forAll (QC.shuffle [0 :: Int, 1, 2, 0, 1, 0]) $ \questions ->
      case history (take 8 choices) of
        Left problem -> counterexample problem False
        Right states ->
          QC.conjoin
            [ let prepared = prepareNablaProjectionsAt frontier (controlIndex prefix) state
               in QC.conjoin
                    [ counterexample
                        (show (stage, prefix, question))
                        ( structuralNablaProjectionAt writer frontier (controlIndex prefix) state
                            === (prepared >>= lookupPreparedNablaProjection writer)
                        )
                    | question <- questions,
                      Just writer <- [Map.lookup question writers]
                    ]
            | (stage, state) <- zip [0 :: Word64 ..] states,
              prefix <- [0 .. stage],
              frontier <- [emptyVector, vectorThrough [(remoteB, 1)]]
            ]
  where
    writers =
      Map.fromList
        [ (0, checked "queried writer" (mkNablaId (fixtureIdentifierBytes 179))),
          (1, checked "absent writer" (mkNablaId (fixtureIdentifierBytes 180))),
          (2, checked "another absent writer" (mkNablaId (fixtureIdentifierBytes 181)))
        ]
    history choices = do
      (currentViews, subject, processA, processB, initial) <- labelControlFixture
      snd
        <$> foldM
          ( \(predecessor, states) (ordinal, choice) -> do
              let target = case choice of
                    0 -> ProcessLabel processA
                    1 -> ProcessLabel processB
                    _ -> VoidLabel
              prepared <-
                labelControlReady
                  currentViews
                  (labelControlCause (fromIntegral (190 + ordinal)) ordinal)
                  subject
                  (releasedLabel (target, ordinal))
                  predecessor
              let successor = preparedStructuralSuccessor prepared
              Right (successor, states <> [successor])
          )
          (initial, [initial])
          (zip [1 ..] choices)

-- A successful target-local preparation must not force the global sort or
-- graph inputs. The old full preparation remains an independent result oracle.
labelTargetViews :: [(ProcessEpochId, HeraldEpoch)] -> [(ProcessEpochId, GlobalObjectId)] -> ReconciliationViews
labelTargetViews residences possessions =
  reconciliationViews
    localHerald
    (error "unchanged label evaluated the complete sort registry")
    (error "unchanged label evaluated the complete graph view")
    (Map.fromList residences)
    (Set.fromList possessions)

assertLabelProjectionReuse ::
  String ->
  Bool ->
  ReconciliationViews ->
  ReconciliationViews ->
  StructuralConsequenceCause ->
  GlobalObjectId ->
  ReleasedLabelState ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  IO PreparedStructuralReconciliation
assertLabelProjectionReuse context expectReuse fullViews targetViews cause subject released fresh predecessor = do
  reference <- either assertFailure pure (reconciliationReady context (prepareStructuralLabelControlRefresh fullViews cause subject released fresh predecessor))
  optimized <- either (assertFailure . show) pure (prepareUnchangedStructuralLabelControlRefresh targetViews cause subject released fresh predecessor)
  case optimized of
    Just prepared -> do
      assertBool (context <> ": changed projection was incorrectly reused") expectReuse
      assertEqual (context <> ": complete preparation equals full reference") reference prepared
      assertBool (context <> ": reuse has no reconciliation mutation") (not (preparedStructuralChangesState prepared))
      assertEqual (context <> ": reuse preserves hidden resources and history") predecessor (preparedStructuralSuccessor prepared)
    Nothing -> assertBool (context <> ": eligible projection was not reused") (not expectReuse)
  assertEqual (context <> ": current winner index is coherent") (Right ()) (validateStructuralAppliedState (preparedStructuralSuccessor reference))
  assertIndexedProjections (preparedStructuralSuccessor reference)
  pure reference

assertIndexedProjections :: StructuralAppliedState -> Assertion
assertIndexedProjections state = do
  let vertices = structuralAppliedVertexProjections state
      edges = structuralAppliedEdgeProjections state
      subjects = Map.keysSet vertices `Set.union` Map.keysSet edges `Set.union` Set.singleton (object 249)
  forM_ (Set.toList subjects) $ \subject -> do
    assertEqual "indexed vertex equals the complete projection" (Map.lookup subject vertices) (structuralAppliedVertexProjectionAt subject state)
    assertEqual "indexed edge equals the complete projection" (Map.lookup subject edges) (structuralAppliedEdgeProjectionAt subject state)

caseUnchangedLabelProjectionMatrix :: Assertion
caseUnchangedLabelProjectionMatrix = do
  let process = processId 160
      residences = [(process, localHerald)]
      rootSubject = object 161
      source = object 162
      destination = object 163
      cause = labelControlCause 164 1
  forM_ [NablaRole, DeltaRole] $ \role ->
    forM_ [(localHerald, True), (localHerald, False), (remoteA, False)] $ \(residence, normal) -> do
      let processes = [(process, residence)]
          possessions = [(process, rootSubject) | normal]
          currentViews = views [] processes possessions
          candidate = app remoteB 1 emptyVector role 1 ((if role == NablaRole then nablaRootValue else rootValue) rootSubject (ProcessLabel process, 0) dataSort)
          fresh = [freshStoreIncarnationEvidence GeneratedStoreIncarnation (deltaId 161) (storeId 165) | role == DeltaRole && normal]
          evidence = case fresh of value : _ -> Just value; [] -> Nothing
      initial <- applyReady currentViews candidate evidence emptyState
      _ <- assertLabelProjectionReuse (show (role, residence, normal)) True currentViews (labelTargetViews processes possessions) cause rootSubject (releasedLabel (ProcessLabel process, 1)) Nothing initial
      pure ()
  forM_ [(NeutralVertexRole, neutralValue rootSubject), (EdgeRole, edgeValue rootSubject source destination Preserve)] $ \(role, value) -> do
    let currentViews = views [NeutralVertex source, NeutralVertex destination] residences []
    initial <- applyReady currentViews (app remoteB 1 emptyVector role 1 value) Nothing emptyState
    forM_ [VoidLabel, ProcessLabel process, ZombieLabel process] $ \owner -> do
      _ <- assertLabelProjectionReuse (show (role, owner)) True currentViews (labelTargetViews residences []) cause rootSubject (releasedLabel (owner, 1)) Nothing initial
      pure ()

caseUnchangedGenesisLabels :: Assertion
caseUnchangedGenesisLabels = do
  (currentViews, _, initial, localBootstrap, _, _) <- genesisReconciliationFixture
  let process = appliedProcessEpochId localBootstrap
      subjects = appliedProcessEnvironmentObjects localBootstrap
      targetViews = labelTargetViews [(process, localHerald)] [(process, subject) | subject <- subjects]
  forM_ subjects $ \subject -> do
    _ <- assertLabelProjectionReuse "genesis same owner" True currentViews targetViews (labelControlCause 166 1) subject (releasedLabel (ProcessLabel process, 1)) Nothing initial
    pure ()

caseChangedLabelProjectionBoundaries :: Assertion
caseChangedLabelProjectionBoundaries = do
  let process = processId 167
      other = processId 168
      subject = object 169
      delta = deltaId 169
      processes = [(process, localHerald), (other, localHerald)]
      possessed = [(process, subject), (other, subject)]
      activeViews = views [] processes possessed
      passiveViews = views [] processes []
      activeTarget = labelTargetViews processes possessed
      passiveTarget = labelTargetViews processes []
      initialFresh = freshStoreIncarnationEvidence GeneratedStoreIncarnation delta (storeId 170)
      replacementFresh = freshStoreIncarnationEvidence GeneratedStoreIncarnation delta (storeId 171)
      root = app remoteB 1 emptyVector DeltaRole 1 (rootValue subject (ProcessLabel process, 0) dataSort)
      cause = labelControlCause 172 1
  initial <- applyReady activeViews root (Just initialFresh) emptyState
  passivated <- assertLabelProjectionReuse "same owner losing Normal possession" False passiveViews passiveTarget cause subject (releasedLabel (ProcessLabel process, 1)) Nothing initial
  let passive = preparedStructuralSuccessor passivated
  _ <- assertLabelProjectionReuse "same owner gaining Normal possession" False activeViews activeTarget (labelControlCause 173 2) subject (releasedLabel (ProcessLabel process, 2)) (Just replacementFresh) passive
  handoff <- assertLabelProjectionReuse "same-Herald controller handoff" False activeViews activeTarget cause subject (releasedLabel (ProcessLabel other, 1)) (Just replacementFresh) initial
  assertEqual "same-Herald handoff creates a new Store incarnation" [storeId 171] (dynamicStoreIncarnation <$> structuralAppliedLocalStores (preparedStructuralSuccessor handoff))
  assertBool "same-Herald handoff retains its old Store" (Map.member (storeId 170) (structuralAppliedRetainedStoreResources (preparedStructuralSuccessor handoff)))
  _ <- assertLabelProjectionReuse "deletion" False activeViews activeTarget cause subject (releasedDeleted 1) Nothing initial
  let remoteProcesses = [(process, remoteA), (other, remoteB)]
      remoteViews = views [] remoteProcesses []
      remoteTarget = labelTargetViews remoteProcesses []
      remoteRoot = app remoteB 1 emptyVector NablaRole 1 (nablaRootValue subject (ProcessLabel process, 0) dataSort)
  remote <- applyReady remoteViews remoteRoot Nothing emptyState
  _ <- assertLabelProjectionReuse "remote controller changes despite passive local placement" False remoteViews remoteTarget cause subject (releasedLabel (ProcessLabel other, 1)) Nothing remote
  forM_
    [ ("missing activation resource", activeViews, activeTarget, releasedLabel (ProcessLabel process, 2), FreshStoreIncarnationDependency delta, passive),
      ("missing controller residence", views [] [] [], labelTargetViews [] [], releasedLabel (ProcessLabel process, 1), StructuralProcessDependency process, remote)
    ]
    $ \(context, fullViews, targetViews, released, dependency, predecessor) -> do
      optimized <- either (assertFailure . ((context <> ": ") <>) . show) pure (prepareUnchangedStructuralLabelControlRefresh targetViews cause subject released Nothing predecessor)
      assertEqual (context <> " retains the full-path validation or dependency") Nothing optimized
      assertHeld context (Set.singleton dependency) (prepareStructuralLabelControlRefresh fullViews cause subject released Nothing predecessor)
  let unexpected = prepareUnchangedStructuralLabelControlRefresh activeTarget cause subject (releasedLabel (ProcessLabel process, 1)) (Just replacementFresh) initial
  assertEqual "unchanged resource rejects unexpected fresh identity" (Left (StructuralFreshIncarnationUnexpected delta)) unexpected
  assertEqual "full reference rejects the same unexpected identity" (Left (StructuralFreshIncarnationUnexpected delta)) (prepareStructuralLabelControlRefresh activeViews cause subject (releasedLabel (ProcessLabel process, 1)) (Just replacementFresh) initial)

propIndexedLabelControlHistory :: [Word8] -> Property
propIndexedLabelControlHistory choices = QC.ioProperty $ do
  let process = processId 174
      other = processId 175
      subject = object 176
      processes = [(process, remoteA), (other, remoteB)]
      currentViews = views [] processes []
      targetViews = labelTargetViews processes []
      initialRoot = app remoteB 1 emptyVector NablaRole 1 (nablaRootValue subject (ProcessLabel process, 0) dataSort)
  initial <- applyReady currentViews initialRoot Nothing emptyState
  (final, _, _) <-
    foldM
      ( \(predecessor, owner, sequenceNumber) (index, choice) -> do
          let nextOwner = if even choice then process else other
              nextSequence = sequenceNumber + 1
              candidate = app remoteB nextSequence (vectorThrough [(remoteB, sequenceNumber)]) NablaRole nextSequence (nablaRootValue subject (ProcessLabel nextOwner, index) dataSort)
          if choice `mod` 3 == 0
            then do
              replaced <- applyReady currentViews candidate Nothing predecessor
              assertEqual "replacement refresh keeps indexed winners coherent" (Right ()) (validateStructuralAppliedState replaced)
              assertIndexedProjections replaced
              -- A retained canonical overlay can dominate the replacement carrier.
              let effectiveOwner = case Map.lookup subject (structuralAppliedVertexProjections replaced) >>= rootControllerActivity of
                    Just (LiveProcessController actual _, _) -> actual
                    _ -> error "replacement Nabla lost its live controller"
              _ <- assertLabelProjectionReuse "same owner after replacement provenance" True currentViews targetViews (labelControlCause 177 (index + 1)) subject (releasedLabel (ProcessLabel effectiveOwner, index + 1)) Nothing replaced
              pure (replaced, effectiveOwner, nextSequence)
            else do
              refreshed <- assertLabelProjectionReuse "reordered owner history" (nextOwner == owner) currentViews targetViews (labelControlCause 177 index) subject (releasedLabel (ProcessLabel nextOwner, index)) Nothing predecessor
              pure (preparedStructuralSuccessor refreshed, nextOwner, sequenceNumber)
      )
      (initial, process, 1)
      (zip [1 ..] ([0, 1, 2, 3, 4, 5] <> take 12 choices))
  assertPortableControlBase initial
  assertPortableControlBase final
  pure True

assertPortableControlBase :: StructuralAppliedState -> Assertion
assertPortableControlBase state = do
  let base = captureStructuralControlBase state
      overlays = structuralAppliedControlOverlays state
      history = structuralAppliedControlHistory state
      indices = Set.fromList [index | (index, _, _) <- history]
      objects = Set.fromList (map fst overlays <> [subject | (_, subject, _) <- history])
  assertEqual "portable capture retains exact current control views" overlays (structuralControlBaseOverlays base)
  assertEqual "portable capture retains superseded causes at their exact coordinates" history (structuralControlBaseHistory base)
  assertEqual "carrier dependencies name exactly the retained control objects" objects (structuralControlBaseObjects base)
  assertEqual "control dependencies include every and only retained cause position" indices (structuralControlBaseControlIndices base)
  assertEqual "empty and nonempty greatest control coordinates agree" (Set.lookupMax indices) (structuralControlBaseGreatestControlIndex base)

propSameControllerControlNoChurn :: Positive Int -> Property
propSameControllerControlNoChurn (Positive supplied) =
  case labelControlFixture >>= repeatedSameController count of
    Left problem -> counterexample problem (False === True)
    Right states -> case states of
      [] -> counterexample "no repeated label controls" (False === True)
      first : remaining ->
        counterexample
          "same-controller label controls changed the structural owner"
          (all (== first) remaining === True)
  where
    count = 1 + supplied `mod` 4

    repeatedSameController
      repetitions
      (currentViews, subject, _, processB, initial) =
        snd
          <$> foldM
            ( \(predecessor, states) ordinal -> do
                prepared <-
                  labelControlReady
                    currentViews
                    ( labelControlCause
                        (fromIntegral (190 + ordinal))
                        (fromIntegral ordinal)
                    )
                    subject
                    (releasedLabel ((ProcessLabel processB, 1)))
                    predecessor
                let successor = preparedStructuralSuccessor prepared
                Right (successor, states <> [successor])
            )
            (initial, [])
            [1 .. repetitions]

labelControlFixture ::
  Either
    String
    (ReconciliationViews, GlobalObjectId, ProcessEpochId, ProcessEpochId, StructuralAppliedState)
labelControlFixture = do
  let rootNabla = checked "label-control nabla" (mkNablaId (fixtureIdentifierBytes 179))
      subject = globalObjectIdFromNablaId rootNabla
      processA = processId 177
      processB = processId 178
      currentViews =
        views
          []
          [(processA, localHerald), (processB, remoteA)]
          [(processA, subject)]
      candidate =
        app
          remoteB
          1
          emptyVector
          NablaRole
          20
          (nablaRootValue subject ((ProcessLabel processA, 0)) dataSort)
  initial <-
    preparedStructuralSuccessor
      <$> reconciliationReady
        "dynamic label-control root"
        (prepareStructuralReconciliation currentViews candidate Nothing emptyState)
  Right (currentViews, subject, processA, processB, initial)

labelControlReady ::
  ReconciliationViews ->
  StructuralConsequenceCause ->
  GlobalObjectId ->
  ReleasedLabelState ->
  StructuralAppliedState ->
  Either String PreparedStructuralReconciliation
labelControlReady currentViews cause subject released predecessor =
  reconciliationReady
    "label-control refresh"
    ( prepareStructuralLabelControlRefresh
        currentViews
        cause
        subject
        released
        Nothing
        predecessor
    )

reconciliationReady ::
  String ->
  Either StructuralReconciliationProblem StructuralPreparation ->
  Either String PreparedStructuralReconciliation
reconciliationReady context result = case result of
  Left problem -> Left (context <> ": " <> show problem)
  Right (StructuralHeld dependencies) ->
    Left (context <> " held: " <> show dependencies)
  Right (StructuralReady prepared) -> Right prepared

labelControlCause :: Word8 -> Word64 -> StructuralConsequenceCause
labelControlCause decisionSeed index =
  checked
    "label control cause"
    (labelReleaseCause (decisionId decisionSeed) (controlIndex index))

decisionId :: Word8 -> LabelDecisionId
decisionId seed = checked "label decision" (mkLabelDecisionId (fixtureIdentifierBytes seed))

projectedNablaController ::
  GlobalObjectId -> StructuralAppliedState -> Maybe ControllerProjection
projectedNablaController subject state =
  case Map.lookup subject (structuralAppliedVertexProjections state) of
    Just (NablaVertexProjection _ _ _ controller _) -> Just controller
    _ -> Nothing

caseReplay :: Assertion
caseReplay = do
  let candidate = app localHerald 1 emptyVector NeutralVertexRole 1 (neutralValue (object 140))
      currentViews = views [] [] []
  state <- applyReady currentViews candidate Nothing emptyState
  prepared <- prepareReady currentViews candidate Nothing state
  assertEqual "classification" StructuralApplicationReplay (preparedStructuralClassification prepared)
  assertEqual "state" state (preparedStructuralSuccessor prepared)
  assertEqual "observation" Nothing (controlledPatchObservation (preparedStructuralControlledPatch prepared))
  assertBool "graph" (structuralGraphPatchNull (preparedStructuralGraphPatch prepared))
  assertEqual "placement" Map.empty (placementPatchChanges (preparedStructuralPlacementPatch prepared))
  assertEqual "store" Map.empty (storePatchChanges (preparedStructuralStorePatch prepared))
  assertBool "debt" (structuralDebtSetNull (preparedStructuralDebts prepared))

propDebtNormalization :: Property
propDebtNormalization =
  counterexample "normalization changed with order or occurrence"
    $ all ((== expected) . normalizeStructuralDebts) (permutations supplied)
      === True
  where
    typedSort = dataSortOccurrence
    identity = StructuralVertexIdentity (NeutralVertex (object 150))
    store = qualifiedStoreIncarnation localHerald (deltaId 151) typedSort (storeId 152)
    placement = qualifiedPlacementFact localHerald (deltaId 151) typedSort (storeId 152)
    firstEvidence = structuralDebtEvidence (Set.singleton identity) Set.empty Set.empty
    secondEvidence = structuralDebtEvidence Set.empty (Set.singleton placement) (Set.singleton store)
    firstKey =
      structuralDebtKey
        (structuralOccurrenceCause (occurrence localHerald 1))
        DestinationAlignmentDebt
        typedSort
        (Just store)
    secondKey =
      structuralDebtKey
        (structuralOccurrenceCause (occurrence remoteA 1))
        DestinationAlignmentDebt
        typedSort
        (Just store)
    supplied =
      [ structuralConsequenceDebt firstKey firstEvidence,
        structuralConsequenceDebt firstKey secondEvidence,
        structuralConsequenceDebt secondKey firstEvidence
      ]
    expected = normalizeStructuralDebts supplied

propConcurrentSameObject :: Property
propConcurrentSameObject
  | winnerKey high <= winnerKey low =
      counterexample "same-object fixture lacks a definite winner" False
  | otherwise =
      case (outcome [low, high], outcome [high, low]) of
        (Left problem, _) -> counterexample problem False
        (_, Left problem) -> counterexample problem False
        (Right lowThenHigh, Right highThenLow) ->
          counterexample
            "same-object concurrent publications diverged by arrival order"
            (lowThenHigh === highThenLow)
  where
    subject = object 155
    value = neutralValue subject
    low = app remoteA 1 emptyVector NeutralVertexRole 10 value
    high = app remoteB 1 emptyVector NeutralVertexRole 20 value
    currentViews = views [] [] []

    outcome order = do
      (state, controlled, debts) <-
        foldM step (emptyState, Map.empty, []) order
      lowSnapshot <- projectAt (vectorThrough [(remoteA, 1)]) state
      highSnapshot <- projectAt (vectorThrough [(remoteB, 1)]) state
      finalWinner <-
        maybe
          (Left "same-object outcome omitted the final Controlled winner")
          Right
          (Map.lookup subject controlled)
      lowProjection <-
        maybe
          (Left "low frontier omitted its exact graph projection")
          Right
          (Map.lookup subject (structuralProjectionSnapshotVertices lowSnapshot))
      highProjection <-
        maybe
          (Left "high frontier omitted its exact graph projection")
          Right
          (Map.lookup subject (structuralProjectionSnapshotVertices highSnapshot))
      finalGraphProjection <-
        maybe
          (Left "same-object outcome omitted the final graph projection")
          Right
          (Map.lookup subject (structuralAppliedVertexProjections state))
      let retainedHistory = structuralAppliedHistoryOccurrences state
      if appliedControlledProjectionPublication finalWinner
        /= Just (checkedPublicationId (structuralPublication high))
        then Left "final Controlled winner is not the greater publication"
        else Right ()
      if structuralVertexProjectionPublication finalGraphProjection
        /= Just (checkedPublicationId (structuralPublication high))
        then Left "final graph projection is not the greater publication"
        else Right ()
      if structuralVertexProjectionPublication lowProjection
        /= Just (checkedPublicationId (structuralPublication low))
        then Left "low-only frontier did not reconstruct the low publication"
        else Right ()
      if structuralVertexProjectionPublication highProjection
        /= Just (checkedPublicationId (structuralPublication high))
        then Left "high-only frontier did not reconstruct the high publication"
        else Right ()
      if length retainedHistory /= 2
        || Set.fromList retainedHistory
          /= Set.fromList [structuralOccurrence low, structuralOccurrence high]
        then Left "retained occurrence history is not complete"
        else Right ()
      Right
        ( finalWinner,
          structuralAppliedVertexProjections state,
          retainedHistory,
          structuralProjectionSnapshotVertices lowSnapshot,
          structuralProjectionSnapshotVertices highSnapshot,
          normalizeStructuralDebts debts
        )

    step (state, controlled, debts) candidate =
      case prepareStructuralReconciliation currentViews candidate Nothing state of
        Left problem -> Left ("same-object reconciliation problem: " <> show problem)
        Right (StructuralHeld dependencies) ->
          Left ("same-object occurrence unexpectedly held: " <> show dependencies)
        Right (StructuralReady prepared) ->
          Right
            ( preparedStructuralSuccessor prepared,
              applyControlledPatch controlled prepared,
              structuralDebtSetEntries (preparedStructuralDebts prepared) <> debts
            )

    applyControlledPatch controlled prepared =
      case controlledPatchWinnerChange (preparedStructuralControlledPatch prepared) of
        Nothing -> controlled
        Just change -> case exactChangeAfter change of
          Just projected ->
            Map.insert
              (appliedControlledProjectionObject projected)
              projected
              controlled
          Nothing -> case exactChangeBefore change of
            Just projected ->
              Map.delete
                (appliedControlledProjectionObject projected)
                controlled
            Nothing -> controlled

    projectAt frontier state =
      case prepareStructuralProjectionAtVector currentViews frontier (controlIndex 0) state of
        Left problem -> Left ("historical projection problem: " <> show problem)
        Right (Left dependencies) ->
          Left ("historical projection unexpectedly held: " <> show dependencies)
        Right (Right snapshot) -> Right snapshot

propIndependentPermutation :: Property
propIndependentPermutation =
  case traverse outcome (permutations concurrent) of
    Left problem -> counterexample problem False
    Right [] -> counterexample "no permutations" False
    Right (expected : observed) ->
      counterexample "concurrent projection/resource/debt changed with order"
        $ all (== expected) observed
          === True
  where
    neutral = app localHerald 1 emptyVector NeutralVertexRole 1 (neutralValue (object 160))
    nabla = checked "nabla" (mkNablaId (fixtureIdentifierBytes 161))
    nablaRoot = app remoteA 1 emptyVector NablaRole 1 (nablaRootValue (globalObjectIdFromNablaId nabla) (VoidLabel, 0) dataSort)
    delta = deltaId 162
    deltaRoot = app remoteB 1 emptyVector DeltaRole 1 (rootValue (globalObjectIdFromDeltaId delta) (VoidLabel, 0) dataSort)
    concurrent = [neutral, nablaRoot, deltaRoot]
    currentViews = views [] [] []

    outcome order = do
      (state, debts) <- foldM step (emptyState, []) order
      Right
        ( structuralAppliedVertexProjections state,
          structuralAppliedEdgeProjections state,
          structuralAppliedLocalStores state,
          structuralAppliedRetainedStoreResources state,
          structuralAppliedOccurrenceStoreResources state,
          normalizeStructuralDebts debts
        )

    step (state, debts) candidate =
      case prepareStructuralReconciliation currentViews candidate Nothing state of
        Left problem -> Left ("problem: " <> show problem)
        Right (StructuralHeld dependencies) -> Left ("held: " <> show dependencies)
        Right (StructuralReady prepared) ->
          Right
            ( preparedStructuralSuccessor prepared,
              structuralDebtSetEntries (preparedStructuralDebts prepared) <> debts
            )

views ::
  [VertexId] ->
  [(ProcessEpochId, HeraldEpoch)] ->
  [(ProcessEpochId, GlobalObjectId)] ->
  ReconciliationViews
views vertices processes possessions =
  reconciliationViews
    localHerald
    effectiveSorts
    (Set.fromList vertices)
    (Map.fromList processes)
    (Set.fromList possessions)

effectiveSorts :: Map SortId SortOccurrence
effectiveSorts =
  Map.fromList
    [ (profileSortFor role, carrierOccurrence role)
    | role <- allPredefinedSortRoles
    ]

app :: HeraldEpoch -> Word64 -> StructuralVersionVector -> PredefinedSortRole -> Word64 -> Value -> StructuralApplication
app source dot predecessor role publicationSequence value =
  structuralApplication
    (occurrence source dot)
    predecessor
    (carrierOccurrence role)
    (publication source role publicationSequence value)
    (controlIndex 0)

publication :: HeraldEpoch -> PredefinedSortRole -> Word64 -> Value -> CheckedPublication
publication source role sequenceNumber value =
  checked
    "publication"
    ( mkCheckedPublication
        (predefinedCatalogueDescriptor (profileEntryFor role))
        ( publicationId
            (checked "publication nabla" (mkNablaId (fixtureIdentifierBytes 200)))
            genesisAuthorityEpoch
            source
            (nablaSequence sequenceNumber)
        )
        value
    )

baseline :: PredefinedSortRole -> HeraldEpoch -> Word64 -> Value -> BaselineStructuralWinner
baseline role source sequenceNumber value =
  baselineStructuralWinner (carrierOccurrence role) (publication source role sequenceNumber value)

occurrence :: HeraldEpoch -> Word64 -> StructuralOccurrenceId
occurrence source sequenceNumber = structuralOccurrenceId source (positiveSequence sequenceNumber)

positiveSequence :: Word64 -> StructuralSequence
positiveSequence = checked "structural sequence" . mkStructuralSequence

carrierOccurrence :: PredefinedSortRole -> SortOccurrence
carrierOccurrence role =
  sortOccurrence
    (profileSortFor role)
    (predefinedOccurrenceFor role (checkedPredefinedOccurrenceSet fixtureCheckedGenesis))

dataSort :: SortId
dataSort = profileSortFor SortDefinitionRole

dataSortOccurrence, otherSortOccurrence :: SortOccurrence
dataSortOccurrence = carrierOccurrence SortDefinitionRole
otherSortOccurrence = carrierOccurrence EdgeRole

fixedMembership :: NonEmpty HeraldEpoch
fixedMembership = localHerald :| [remoteA, remoteB]

fixedMembershipGeneration :: HeraldMembershipGeneration
fixedMembershipGeneration =
  checked
    "fixed membership"
    ( genesisHeraldMembershipGeneration
        (checkedSystemId fixtureCheckedGenesis)
        fixedMembership
    )

emptyVector :: StructuralVersionVector
emptyVector = emptyStructuralVersionVector fixedMembershipGeneration

vectorThrough :: [(HeraldEpoch, Word64)] -> StructuralVersionVector
vectorThrough supplied =
  checked
    "vector"
    ( mkStructuralVersionVector
        fixedMembershipGeneration
        [ (herald, maybe emptyStructuralPrefix (structuralPrefixThrough . positiveSequence) (lookup herald supplied))
        | herald <- [localHerald, remoteA, remoteB]
        ]
    )

emptyState :: StructuralAppliedState
emptyState = emptyStructuralAppliedState emptyVector

neutralValue :: GlobalObjectId -> Value
neutralValue subject =
  checked
    "neutral value"
    (recordValue [(objectIdField, objectValue subject), (labelField, labelValue (VoidLabel, 0))])

edgeValue :: GlobalObjectId -> GlobalObjectId -> GlobalObjectId -> EdgeStrength -> Value
edgeValue subject source destination strength =
  checked
    "edge value"
    ( recordValue
        [ (objectIdField, objectValue subject),
          (labelField, labelValue (VoidLabel, 0)),
          (sourceField, objectValue source),
          (destinationField, objectValue destination),
          (strengthField, enumValue (edgeStrengthSymbol strength))
        ]
    )

rootValue :: GlobalObjectId -> Label -> SortId -> Value
rootValue subject label sortId = rootValueBytes subject label (sortIdBytes sortId)

nablaRootValue :: GlobalObjectId -> Label -> SortId -> Value
nablaRootValue subject label sortId =
  checked
    "nabla root value"
    ( recordValue
        [ (objectIdField, objectValue subject),
          (labelField, labelValue label),
          (sortIdField, bytesValue (sortIdBytes sortId)),
          (sequencingObjectField, optionalGlobalUniqueIdValue Nothing)
        ]
    )

rootValueBytes :: GlobalObjectId -> Label -> ByteString.ByteString -> Value
rootValueBytes subject label sortBytes =
  checked
    "root value"
    ( recordValue
        [ (objectIdField, objectValue subject),
          (labelField, labelValue label),
          (sortIdField, bytesValue sortBytes)
        ]
    )

processValue :: GlobalObjectId -> Value
processValue subject =
  checked
    "process value"
    ( recordValue
        [ (objectIdField, objectValue subject),
          (labelField, labelValue (VoidLabel, 0)),
          (processIdField, bytesValue (fixtureIdentifierBytes 201)),
          (residenceField, bytesValue (fixtureIdentifierBytes 202)),
          (liveField, boolValue True)
        ]
    )

objectValue :: GlobalObjectId -> Value
objectValue = globalUniqueIdValue . globalUniqueIdFromGlobalObjectId

assertPreparedStateChange :: PreparedStructuralReconciliation -> IO PreparedStructuralReconciliation
assertPreparedStateChange prepared = do
  assertEqual
    "prepared state-change disposition matches independent semantic equality"
    (preparedStructuralPredecessor prepared /= preparedStructuralSuccessor prepared)
    (preparedStructuralChangesState prepared)
  pure prepared

prepareReady :: ReconciliationViews -> StructuralApplication -> Maybe FreshStoreIncarnationEvidence -> StructuralAppliedState -> IO PreparedStructuralReconciliation
prepareReady currentViews candidate fresh state =
  case prepareStructuralReconciliation currentViews candidate fresh state of
    Right (StructuralReady prepared) -> assertPreparedStateChange prepared
    Right (StructuralHeld dependencies) -> assertFailure ("unexpected hold: " <> show dependencies)
    Left problem -> assertFailure ("unexpected problem: " <> show problem)

prepareRetirementSuppressedReady ::
  StructuralRetirementSuppression ->
  ReconciliationViews ->
  StructuralApplication ->
  StructuralAppliedState ->
  IO PreparedStructuralReconciliation
prepareRetirementSuppressedReady suppression currentViews candidate state =
  case prepareRetirementSuppressedStructuralReconciliation suppression currentViews candidate state of
    Right (StructuralReady prepared) -> assertPreparedStateChange prepared
    Right (StructuralHeld dependencies) ->
      assertFailure ("unexpected retirement-suppression hold: " <> show dependencies)
    Left problem ->
      assertFailure ("unexpected retirement-suppression problem: " <> show problem)

applyRetirementSuppressedReady ::
  StructuralRetirementSuppression ->
  ReconciliationViews ->
  StructuralApplication ->
  StructuralAppliedState ->
  IO StructuralAppliedState
applyRetirementSuppressedReady suppression currentViews candidate state =
  preparedStructuralSuccessor
    <$> prepareRetirementSuppressedReady suppression currentViews candidate state

refreshReady :: ReconciliationViews -> StructuralOccurrenceId -> Maybe FreshStoreIncarnationEvidence -> StructuralAppliedState -> IO PreparedStructuralReconciliation
refreshReady currentViews target fresh state =
  case prepareStructuralProjectionRefresh currentViews target fresh state of
    Right (StructuralReady prepared) -> assertPreparedStateChange prepared
    Right (StructuralHeld dependencies) -> assertFailure ("unexpected refresh hold: " <> show dependencies)
    Left problem -> assertFailure ("unexpected refresh problem: " <> show problem)

applyReady :: ReconciliationViews -> StructuralApplication -> Maybe FreshStoreIncarnationEvidence -> StructuralAppliedState -> IO StructuralAppliedState
applyReady currentViews candidate fresh state =
  preparedStructuralSuccessor <$> prepareReady currentViews candidate fresh state

projectionReady :: ReconciliationViews -> StructuralVersionVector -> StructuralAppliedState -> IO StructuralProjectionSnapshot
projectionReady currentViews frontier state =
  case prepareStructuralProjectionAtVector currentViews frontier (controlIndex 0) state of
    Right (Right snapshot) -> pure snapshot
    Right (Left dependencies) -> assertFailure ("unexpected projection hold: " <> show dependencies)
    Left problem -> assertFailure ("unexpected projection problem: " <> show problem)

baselineState :: ReconciliationViews -> Map GlobalObjectId BaselineStructuralWinner -> Map DeltaId DynamicLocalStoreSpec -> IO StructuralAppliedState
baselineState currentViews supplied stores =
  case structuralAppliedStateWithBaseline currentViews emptyVector supplied stores of
    Right (Right state) -> pure state
    Right (Left dependencies) -> assertFailure ("unexpected baseline hold: " <> show dependencies)
    Left problem -> assertFailure ("unexpected baseline problem: " <> show problem)

stateWithStores :: ReconciliationViews -> Map DeltaId DynamicLocalStoreSpec -> IO StructuralAppliedState
stateWithStores currentViews stores =
  case structuralAppliedStateWithStores currentViews emptyVector stores of
    Right state -> pure state
    Left problem -> assertFailure ("unexpected seed problem: " <> show problem)

winnerBefore :: PreparedStructuralReconciliation -> IO AppliedControlledProjection
winnerBefore prepared =
  case controlledPatchWinnerChange (preparedStructuralControlledPatch prepared) >>= exactChangeBefore of
    Just projected -> pure projected
    Nothing -> assertFailure "missing old controlled winner"

assertHeld :: String -> Set.Set StructuralDependency -> Either StructuralReconciliationProblem StructuralPreparation -> Assertion
assertHeld context expected = \case
  Right (StructuralHeld actual) -> assertEqual context expected actual
  Right (StructuralReady prepared) -> assertFailure (context <> ": ready: " <> show prepared)
  Left problem -> assertFailure (context <> ": problem: " <> show problem)

hasDebt :: StructuralDebtKind -> PreparedStructuralReconciliation -> Bool
hasDebt kind =
  any ((== kind) . structuralDebtKeyKind . structuralConsequenceDebtKey)
    . structuralDebtSetEntries
    . preparedStructuralDebts

winnerKey :: StructuralApplication -> PublicationWinnerKey
winnerKey = checkedPublicationWinnerKey . structuralPublication

structuralPublication :: StructuralApplication -> CheckedPublication
structuralPublication = structuralApplicationPublication

structuralOccurrence :: StructuralApplication -> StructuralOccurrenceId
structuralOccurrence = structuralApplicationOccurrence

localHerald :: HeraldEpoch
localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis

remoteA, remoteB :: HeraldEpoch
remoteA = heraldId 241
remoteB = heraldId 242

object :: Word8 -> GlobalObjectId
object seed = checked "object" (mkGlobalObjectId (fixtureIdentifierBytes seed))

processId :: Word8 -> ProcessEpochId
processId seed = checked "process" (mkProcessEpochId (fixtureIdentifierBytes seed))

deltaId :: Word8 -> DeltaId
deltaId seed = checked "delta" (mkDeltaId (fixtureIdentifierBytes seed))

storeId :: Word8 -> StoreIncarnationId
storeId seed = checked "store" (mkStoreIncarnationId (fixtureIdentifierBytes seed))

heraldId :: Word8 -> HeraldEpoch
heraldId seed = checked "Herald" (mkHeraldEpoch (fixtureIdentifierBytes seed))

objectIdField, labelField, sortIdField, sequencingObjectField :: FieldName
objectIdField = field "object_id"
labelField = field "label"
sortIdField = field "sort_id"
sequencingObjectField = field "sequencing_object"

sourceField, destinationField, strengthField :: FieldName
sourceField = field "source_vertex"
destinationField = field "destination_vertex"
strengthField = field "strength"

processIdField, residenceField, liveField :: FieldName
processIdField = field "process_id"
residenceField = field "residence"
liveField = field "live"

field :: String -> FieldName
field name = checked "field" (mkFieldName (Text.pack name))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
