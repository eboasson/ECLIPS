{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module LabelPatchProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Data.List (permutations)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve),
    VertexId (DeltaVertex, NablaVertex),
    edgeStrengthSymbol,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    AuthorityEpochView (LabelAuthorityEpochView),
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    NablaId,
    NablaSequencing (UnsequencedNabla),
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    authorityEpochView,
    controlIndex,
    genesisAuthorityEpoch,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    globalUniqueIdBytes,
    globalUniqueIdFromGlobalObjectId,
    labelAuthorityEpoch,
    mkDeltaId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkStoreIncarnationId,
    mkStructuralSequence,
    nablaSequence,
    publicationId,
    structuralOccurrenceId,
  )
import Eclips.Domain.Label
  ( LabelRecord,
    LabelRevision,
    PreparedAuthorityDispositionView (..),
    PreparedLabelFacts,
    PriorAuthorityJustification,
    PriorAuthorityJustificationView (ExistingReleasedAuthorityView),
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    checkedGenesisAuthority,
    derivePreparedLabelDigest,
    existingReleasedAuthority,
    labelRecordRetainedAuthority,
    labelRevisionControlIndex,
    mkLabelRevision,
    mkPreparedLabelFacts,
    noTargetAuthority,
    preparedAuthorityDisposition,
    preparedAuthorityDispositionView,
    preparedLabelDigestBytes,
    preparedLabelFactsExpectedPriorAuthority,
    preparedLabelFactsPriorAuthorityJustification,
    preparedLabelFactsResolveIndex,
    priorAuthorityJustificationView,
    releasedDeleted,
    releasedLabel,
    releasedLabelStateGeneration,
    releasedLabelStateView,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication (CheckedPublication, mkCheckedPublication)
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (..))
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profileSortFor,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    InitialProjectionDigest,
    appliedProcessEpochId,
    appliedProcessResidence,
    profileCatalogueDigest,
  )
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.Topology (deriveMemberSetDigest)
import Eclips.Domain.Value
  ( Label,
    LabelOwner (..),
    Value,
    enumValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    recordValue,
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal
  ( checkedActiveHeraldEpochs,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.LabelPatch hiding (commitPatchRelease)
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import GenesisFixtures
  ( fixtureCheckedInitialBootstraps,
    fixtureGeneratorSeed,
    fixtureGeneratorSeedH2,
    fixtureOracleProjectionState,
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
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "live label patches"
    [ testCase "checked specification derives an absent ordinary target" checkedAbsentOrdinary,
      testCase "checked specification preserves genesis structural provenance" checkedGenesisProvenance,
      testCase "checked specification rejects catalogue and Controlled role disagreement" checkedStructuralRoleMismatch,
      testCase "root affected-sort cuts contain only their carried sort" checkedRootSortCuts,
      testCase "remote and passive deltas need no local placement" checkedRemoteAndPassiveDelta,
      testCase "local delta activation requires Normal possession" checkedLocalDeltaPossession,
      testCase "controller handoff replaces an exact old Store incarnation" checkedHandoffStoreReplacement,
      testCase "every role derives a narrow live/delete recipe at installation" roleRecipes,
      testCase "edge delete covers its complete effective-sort cut" multiSortEdgeDelete,
      testCase "delta cut couples every sort to its Store disposition" multiSortDeltaScope,
      testCase "delta placement outside the effective registry is rejected" deltaSortMustBeEffective,
      testCase "delta-to-void allocates no Store incarnation" deltaToVoid,
      testCase "remote delta controller allocates no local Store incarnation" deltaRemoteController,
      testCase "Store identities allocate only with atomic installation" atomicPreallocation,
      testCase "exact preparation and release replay allocate nothing twice" exactReplay,
      testCase "label work counters survive preparation, installation and exact replay" workSurvivesInstallation,
      testCase "pending facts become compact installation evidence without retaining a recipe" compactInstallationEvidence,
      testCase "installation uses the current materialized role and preserves preparation provenance" releaseAfterMaterialization,
      testCase "unrelated identity allocation before installation is preserved" releaseGeneratorInterleaving,
      testCase "exact replay rejects a foreign generator lineage" foreignGeneratorReplay,
      testCase "specification facts and digest come from typed evidence" typedFactsCoherence,
      testCase "patch evidence preserves chained label-release authority" chainedAuthorityCoherence,
      testCase "local installation uses exactly its canonical decision index" installationUsesDecisionIndex,
      testCase "unrelated interleaving survives release byte-for-byte" unrelatedInterleaving,
      testCase "target holds release only with their own patch" exactTargetHolds,
      testCase "terminal deletion cannot be resurrected" terminalDelete,
      testCase "successor preparation checks prior state and revision" checkedPrior,
      testCase "checked label base carries portable predecessors without donor local ownership" checkedBasePortable,
      testCase "checked label base rejects pending capture and preserves terminal deletion" checkedBaseBoundaries,
      testCase "installed and imported predecessors accept takeover after the former owner ends" checkedBaseSuccessorAfterEnd,
      QC.testProperty "checked label base and local suffix agree with uninterrupted latest predecessors" propCheckedBaseSuccession,
      testCase "independent preparation order has one canonical retained result" independentOrder
    ]

checkedAbsentOrdinary :: Assertion
checkedAbsentOrdinary = do
  let object = fixtureObject 0xa0
  specification <-
    checkedBoundarySpecification
      0xa0
      object
      liveState
      liveState
      fixtureEmptyStructuralProgress
      Controlled.emptyState
      fixtureEmptyPlacement
      fixtureEmptyStore
  assertEqual
    "absence from both structural and Controlled catalogues is ordinary absence"
    AbsentOrdinaryTargetView
    (targetRoleView (patchSpecificationRole specification))

checkedGenesisProvenance :: Assertion
checkedGenesisProvenance = do
  let neutralObject = fixtureObject 0xa1
      nabla = fixtureNabla 0xa2
      delta = fixtureDelta 0xa3
      edgeObject = fixtureObject 0xa4
      cases =
        [ ( 0xa1,
            "neutral",
            neutralObject,
            fixtureStructuralProgress
              [fixtureStructuralBaseline NeutralVertexRole neutralObject],
            fixtureObservedStructuralState NeutralVertexRole neutralObject,
            NeutralTargetView GenesisStructuralTargetView
          ),
          ( 0xa2,
            "nabla",
            globalObjectIdFromNablaId nabla,
            fixtureEmptyStructuralProgress,
            fixtureNablaControlledState nabla (fixtureProcess 1) (fixtureHerald 1),
            NablaTargetView nabla GenesisStructuralTargetView
          ),
          ( 0xa3,
            "delta",
            globalObjectIdFromDeltaId delta,
            fixtureEmptyStructuralProgress,
            fixtureDeltaControlledState delta (fixtureProcess 1) (fixtureHerald 1),
            DeltaTargetView delta GenesisStructuralTargetView
          ),
          ( 0xa4,
            "edge",
            edgeObject,
            fixtureStructuralProgress
              [fixtureStructuralBaseline EdgeRole edgeObject],
            fixtureObservedStructuralState EdgeRole edgeObject,
            EdgeTargetView GenesisStructuralTargetView
          )
        ]
  mapM_
    ( \(ordinal, label, object, progress, controlled, expected) -> do
        specification <-
          checkedBoundarySpecification
            ordinal
            object
            liveState
            liveState
            progress
            controlled
            fixtureEmptyPlacement
            fixtureEmptyStore
        assertEqual
          (label <> " retains the closed genesis provenance arm")
          expected
          (targetRoleView (patchSpecificationRole specification))
    )
    cases

checkedStructuralRoleMismatch :: Assertion
checkedStructuralRoleMismatch = do
  let object = fixtureObject 0xa5
      facts = fixturePreparedFacts 0xa5 object liveState Nothing liveState
      digest = derivePreparedLabelDigest facts
  assertEqual
    "the structural catalogue and Controlled cannot classify one object differently"
    ( Left
        ( PatchContextStructuralCatalogueMismatch
            object
            NeutralVertexCarrier
            (Just EdgeCarrier)
        )
    )
    ( checkedPatchSpecification
        (fixtureHerald 1)
        facts
        digest
        ( fixtureStructuralProgress
            [fixtureStructuralBaseline NeutralVertexRole object]
        )
        (fixtureObservedStructuralState EdgeRole object)
        fixtureSortRegistry
        fixtureEmptyPlacement
        fixtureEmptyStore
        fixtureOracleProjection
    )

checkedRootSortCuts :: Assertion
checkedRootSortCuts = do
  let nablaCarriedSort = fixtureCarrierSort NablaRole
      deltaCarriedSort = fixtureCarrierSort DeltaRole
      nabla = fixtureNabla 0xa6
      nablaObject = globalObjectIdFromNablaId nabla
      delta = fixtureDelta 0xa7
      deltaObject = globalObjectIdFromDeltaId delta
  nablaSpecification <-
    checkedBoundarySpecification
      0xa6
      nablaObject
      liveState
      liveState
      fixtureEmptyStructuralProgress
      (fixtureNablaControlledState nabla (fixtureProcess 1) (fixtureHerald 1))
      fixtureEmptyPlacement
      fixtureEmptyStore
  deltaSpecification <-
    checkedBoundarySpecification
      0xa7
      deltaObject
      liveState
      liveState
      fixtureEmptyStructuralProgress
      (fixtureDeltaControlledState delta (fixtureProcess 1) (fixtureHerald 1))
      fixtureEmptyPlacement
      fixtureEmptyStore
  assertBool
    "the registry fixture has unrelated effective sorts"
    ( fixtureEffectiveSorts /= Set.singleton nablaCarriedSort
        && fixtureEffectiveSorts /= Set.singleton deltaCarriedSort
    )
  assertEqual
    "nabla scope is its carried sort"
    (Set.singleton nablaCarriedSort)
    (patchSpecificationAffectedSorts nablaSpecification)
  assertEqual
    "delta scope is its carried sort"
    (Set.singleton deltaCarriedSort)
    (patchSpecificationAffectedSorts deltaSpecification)

checkedRemoteAndPassiveDelta :: Assertion
checkedRemoteAndPassiveDelta = do
  let delta = fixtureDelta 0xa8
      object = globalObjectIdFromDeltaId delta
      remoteState = releasedLabel ((ProcessLabel (fixtureProcess 2), 0))
      controlled =
        fixtureDeltaControlledState delta (fixtureProcess 2) (fixtureHerald 2)
      cases =
        [ (0xa8, "remote", remoteState),
          (0xa9, "passive", releasedLabel (VoidLabel, 0))
        ]
  mapM_
    ( \(ordinal, label, proposed) -> do
        specification <-
          checkedBoundarySpecification
            ordinal
            object
            remoteState
            proposed
            fixtureEmptyStructuralProgress
            controlled
            fixtureEmptyPlacement
            fixtureEmptyStore
        assertEqual
          (label <> " delta reserves no local Store without a placement")
          Set.empty
          (patchSpecificationStoreActivations specification)
    )
    cases

checkedLocalDeltaPossession :: Assertion
checkedLocalDeltaPossession = do
  let delta = fixtureDelta 0xaa
      object = globalObjectIdFromDeltaId delta
      carriedSort = fixturePlacementSort
      remotePossessor =
        fixtureDeltaControlledState delta (fixtureProcess 2) (fixtureHerald 2)
      knownWithoutNormal =
        fixtureInstallControlledProcess
          0xf9
          (fixtureProcess 1)
          (fixtureHerald 1)
          remotePossessor
      localPossessor =
        fixtureDeltaControlledState delta (fixtureProcess 1) (fixtureHerald 1)
  withoutNormal <-
    checkedBoundarySpecification
      0xaa
      object
      liveState
      liveState
      fixtureEmptyStructuralProgress
      knownWithoutNormal
      fixtureEmptyPlacement
      fixtureEmptyStore
  withNormal <-
    checkedBoundarySpecification
      0xab
      object
      liveState
      liveState
      fixtureEmptyStructuralProgress
      localPossessor
      fixtureEmptyPlacement
      fixtureEmptyStore
  assertBool
    "the negative cut knows the proposed local controller"
    (Controlled.controlledProcessFact (fixtureProcess 1) knownWithoutNormal /= Nothing)
  assertBool
    "that known local controller lacks Normal possession of the delta"
    (not (Controlled.controlledHasNormalPossession (fixtureProcess 1) object knownWithoutNormal))
  assertEqual
    "a merely local and live label is insufficient"
    Set.empty
    (patchSpecificationStoreActivations withoutNormal)
  assertEqual
    "Normal possession admits the exact carried-sort activation"
    (Set.singleton (storeActivation delta carriedSort))
    (patchSpecificationStoreActivations withNormal)

checkedHandoffStoreReplacement :: Assertion
checkedHandoffStoreReplacement = do
  generator <- fixtureGenerator
  oldGenerated <-
    checked
      "generate predecessor Store incarnation"
      (IdGenerator.prepareGeneratedId generator)
  let delta = fixtureDelta 0xac
      object = globalObjectIdFromDeltaId delta
      carriedSort = fixturePlacementSort
      activation = storeActivation delta carriedSort
      oldIncarnation =
        checkedPure
          "reinterpret predecessor Store incarnation"
          ( mkStoreIncarnationId
              (globalUniqueIdBytes (IdGenerator.preparedGlobalUniqueId oldGenerated))
          )
      successorGenerator = IdGenerator.commitGeneratedId oldGenerated
      controlled =
        fixtureDeltaControlledState delta (fixtureProcess 1) (fixtureHerald 1)
      stores = fixtureStoreWithSlot delta carriedSort oldIncarnation
      placements = fixturePlacementStateAtWithIncarnation delta carriedSort oldIncarnation
      remoteState = releasedLabel ((ProcessLabel (fixtureProcess 2), 0))
  assertEqual
    "the predecessor cut really contains the matching active Store slot"
    (Just oldIncarnation)
    (Store.storeSlotIncarnation <$> Store.lookupStoreSlot delta stores)
  retained <-
    checkedBoundarySpecification
      0xac
      object
      liveState
      liveState
      fixtureEmptyStructuralProgress
      controlled
      placements
      stores
  handoff <-
    checkedBoundarySpecification
      0xad
      object
      remoteState
      liveState
      fixtureEmptyStructuralProgress
      controlled
      placements
      stores
  assertEqual
    "an unchanged controller retains the exact matching Store"
    Set.empty
    (patchSpecificationStoreActivations retained)
  assertEqual
    "a controller handoff invalidates reuse of that Store incarnation"
    (Set.singleton activation)
    (patchSpecificationStoreActivations handoff)
  prepared <-
    checked
      "prepare handoff replacement"
      (prepareLabelPatch handoff successorGenerator initialState)
  released <- releasePrepared handoff prepared
  case Map.lookup activation (labelPatchRecipePreallocatedStores (requireReleasedRecipe released)) of
    Nothing -> assertFailure "handoff did not allocate its admitted replacement at installation"
    Just replacement ->
      assertBool
        "the replacement identity is fresh rather than the matching old slot"
        (replacement /= oldIncarnation)

checkedBoundarySpecification ::
  Word8 ->
  GlobalObjectId ->
  ReleasedLabelState ->
  ReleasedLabelState ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  Placement.State ->
  Store.State ->
  IO PatchSpecification
checkedBoundarySpecification ordinal object prior proposed progress controlled placements stores =
  let facts = fixturePreparedFacts ordinal object prior Nothing proposed
      digest = derivePreparedLabelDigest facts
   in checked
        "checked patch specification"
        ( checkedPatchSpecification
            (fixtureHerald 1)
            facts
            digest
            progress
            controlled
            fixtureSortRegistry
            placements
            stores
            fixtureOracleProjection
        )

fixtureEmptyStructuralProgress :: GraphProgress.StructuralProgressState
fixtureEmptyStructuralProgress = fixtureStructuralProgress []

fixtureStructuralProgress ::
  [(GlobalObjectId, Reconciliation.BaselineStructuralWinner)] ->
  GraphProgress.StructuralProgressState
fixtureStructuralProgress entries =
  checkedPure
    "live fixture structural progress"
    ( GraphProgress.initialStructuralProgressState
        (checkedSystemId fixtureStep14CheckedGenesis)
        (checkedLocalHeraldEpoch fixtureStep14CheckedGenesis)
        membership
        (checkedInitialProjectionDigest fixtureCheckedInitialBootstraps)
        applied
    )
  where
    membership =
      checkedPure
        "live fixture membership generation"
        ( Membership.genesisHeraldMembershipGeneration
            (checkedSystemId fixtureStep14CheckedGenesis)
            fixtureActiveHeralds
        )
    membershipVector =
      emptyStructuralVersionVector membership
    applied =
      case Reconciliation.structuralAppliedStateWithBaseline
        fixtureReconciliationViews
        membershipVector
        (Map.fromList entries)
        Map.empty of
        Right (Right ready) -> ready
        Right (Left held) -> error ("fixture structural baseline held: " <> show held)
        Left problem -> error ("fixture structural baseline rejected: " <> show problem)

fixtureReconciliationViews :: Reconciliation.ReconciliationViews
fixtureReconciliationViews =
  Reconciliation.reconciliationViews
    (fixtureHerald 1)
    ( Map.fromList
        [ (sortOccurrenceSortId occurrence, occurrence)
        | occurrence <- Set.toAscList fixtureEffectiveSorts
        ]
    )
    (Set.fromList [NablaVertex fixtureEdgeNabla, DeltaVertex fixtureEdgeDelta])
    Map.empty
    Set.empty

fixtureStructuralBaseline ::
  PredefinedSortRole ->
  GlobalObjectId ->
  (GlobalObjectId, Reconciliation.BaselineStructuralWinner)
fixtureStructuralBaseline role object =
  ( object,
    Reconciliation.baselineStructuralWinner
      (fixtureCarrierSort role)
      (fixtureStructuralPublication role object (VoidLabel, 0))
  )

fixtureObservedStructuralState ::
  PredefinedSortRole -> GlobalObjectId -> Controlled.State
fixtureObservedStructuralState role object =
  fixtureObservedStructuralStateAt role object (VoidLabel, 0)

fixtureObservedStructuralStateAt ::
  PredefinedSortRole -> GlobalObjectId -> Label -> Controlled.State
fixtureObservedStructuralStateAt role object label =
  fst
    ( Controlled.commitControlledPeerObservation
        ( checkedPure
            "retain structural Controlled observation"
            (Controlled.prepareControlledPeerObservation observation Controlled.emptyState)
        )
    )
  where
    descriptor = predefinedCatalogueDescriptor (profileEntryFor role)
    occurrence = sortOccurrenceDefinition (fixtureCarrierSort role)
    publication = fixtureStructuralPublication role object label
    observation =
      checkedPure
        "checked structural Controlled observation"
        (Controlled.checkControlledObservation descriptor occurrence publication)

fixtureStructuralPublication ::
  PredefinedSortRole -> GlobalObjectId -> Label -> CheckedPublication
fixtureStructuralPublication role object label =
  checkedPure
    "checked structural Controlled publication"
    (mkCheckedPublication descriptor identifier (fixtureStructuralValue role object label))
  where
    descriptor = predefinedCatalogueDescriptor (profileEntryFor role)
    identifier =
      publicationId
        (fixtureNabla 0xf7)
        genesisAuthorityEpoch
        (fixtureHerald 1)
        (nablaSequence 1)

fixtureStructuralValue :: PredefinedSortRole -> GlobalObjectId -> Label -> Value
fixtureStructuralValue role object label =
  case role of
    NeutralVertexRole ->
      checkedPure
        "neutral carrier value"
        ( recordValue
            [ ( checkedPure "object field" (mkFieldName "object_id"),
                globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)
              ),
              (checkedPure "label field" (mkFieldName "label"), labelValue label)
            ]
        )
    EdgeRole ->
      checkedPure
        "edge carrier value"
        ( recordValue
            [ ( checkedPure "object field" (mkFieldName "object_id"),
                globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)
              ),
              (checkedPure "label field" (mkFieldName "label"), labelValue label),
              ( checkedPure "source field" (mkFieldName "source_vertex"),
                globalUniqueIdValue
                  (globalUniqueIdFromGlobalObjectId (globalObjectIdFromNablaId fixtureEdgeNabla))
              ),
              ( checkedPure "destination field" (mkFieldName "destination_vertex"),
                globalUniqueIdValue
                  (globalUniqueIdFromGlobalObjectId (globalObjectIdFromDeltaId fixtureEdgeDelta))
              ),
              ( checkedPure "strength field" (mkFieldName "strength"),
                enumValue (edgeStrengthSymbol Preserve)
              )
            ]
        )
    _ -> error "fixture structural publication supports only neutral and edge roles"

fixtureNablaControlledState ::
  NablaId -> ProcessEpochId -> HeraldEpoch -> Controlled.State
fixtureNablaControlledState nabla process residence =
  fixtureRootControlledState
    process
    residence
    [ Controlled.writerRootFact
        process
        NablaRole
        (sortOccurrenceSortId carrier)
        (sortOccurrenceDefinition carrier)
        genesisAuthorityEpoch
        (controlIndex 0)
        nabla
        UnsequencedNabla
    ]
  where
    carrier = fixtureCarrierSort NablaRole

fixtureDeltaControlledState ::
  DeltaId -> ProcessEpochId -> HeraldEpoch -> Controlled.State
fixtureDeltaControlledState delta process residence =
  fixtureRootControlledState
    process
    residence
    [ Controlled.readerRootFact
        process
        DeltaRole
        (sortOccurrenceSortId carrier)
        (sortOccurrenceDefinition carrier)
        genesisAuthorityEpoch
        (controlIndex 0)
        delta
    ]
  where
    carrier = fixtureCarrierSort DeltaRole

fixtureRootControlledState ::
  ProcessEpochId -> HeraldEpoch -> [Controlled.RootFact] -> Controlled.State
fixtureRootControlledState process residence roots =
  Controlled.commitControlledBootstrap
    ( checkedPure
        "fixture Controlled roots"
        ( Controlled.prepareControlledBootstrap
            ( Controlled.processFact
                (checkedPure "fixture ProcessId" (mkProcessId (bytes 0xf8)))
                process
                residence
                genesisAuthorityEpoch
            )
            roots
            Controlled.emptyState
        )
    )

fixtureInstallControlledProcess ::
  Word8 -> ProcessEpochId -> HeraldEpoch -> Controlled.State -> Controlled.State
fixtureInstallControlledProcess processByte process residence state =
  Controlled.commitControlledBootstrap
    ( checkedPure
        "install known fixture process"
        ( Controlled.prepareControlledBootstrap
            ( Controlled.processFact
                (checkedPure "known fixture ProcessId" (mkProcessId (bytes processByte)))
                process
                residence
                genesisAuthorityEpoch
            )
            []
            state
        )
    )

fixtureCarrierSort :: PredefinedSortRole -> SortOccurrence
fixtureCarrierSort role =
  case [ entry
       | entry <- SortRegistry.registryEntries fixtureSortRegistry,
         SortRegistry.registryEntrySortId entry == profileSortFor role
       ] of
    [entry] ->
      sortOccurrence
        (SortRegistry.registryEntrySortId entry)
        (SortRegistry.registryEntryOccurrenceId entry)
    _ -> error "fixture registry lost a unique predefined role"

fixtureEdgeNabla :: NablaId
fixtureEdgeNabla = fixtureNabla 0xf5

fixtureEdgeDelta :: DeltaId
fixtureEdgeDelta = fixtureDelta 0xf6

fixtureEmptyPlacement :: Placement.State
fixtureEmptyPlacement = Placement.initialState (fixtureHerald 1)

fixturePlacementIncarnation :: StoreIncarnationId
fixturePlacementIncarnation = fixtureStoreIncarnation 0xe2

fixtureStoreWithSlot ::
  DeltaId -> SortOccurrence -> StoreIncarnationId -> Store.State
fixtureStoreWithSlot delta sort incarnation =
  Store.commitStoreBootstrap
    ( checkedPure
        "matching old Store slot"
        ( Store.prepareStoreBootstrap
            [ Store.localStoreSpec
                (Store.ApplicationReader (fixtureProcess 1) DeltaRole)
                delta
                (sortOccurrenceSortId sort)
                (sortOccurrenceDefinition sort)
                incarnation
            ]
            fixtureEmptyStore
        )
    )

roleRecipes :: Assertion
roleRecipes = do
  generator <- fixtureGenerator
  let occurrence = fixtureOccurrence 1
      nabla = fixtureNabla 20
      delta = fixtureDelta 30
      allEffectiveSorts = fixtureEffectiveSortCut
      nablaSorts = fixtureAffectedSortCut
      deltaSorts = fixtureDeltaSortCut delta
      roles =
        [ (fixtureObject 10, absentOrdinaryPatchContext, InstallFutureReleasedOverlay, InstallFutureTerminalSuppression),
          (fixtureObject 11, ordinaryControlledPatchContext, OverlayOrdinaryControlledObject, DeleteOrdinaryControlledObject),
          (fixtureObject 12, neutralPatchContext occurrence allEffectiveSorts, OverlayNeutralVertex, DeleteNeutralVertex),
          (globalObjectIdFromNablaId nabla, nablaPatchContext nabla occurrence nablaSorts, RelabelNablaProjection, DeleteNablaProjection),
          (globalObjectIdFromDeltaId delta, deltaPatchContext delta occurrence deltaSorts fixtureLocalProcessCut, RelabelDeltaProjection, DeleteDeltaProjection),
          (fixtureObject 13, edgePatchContext occurrence allEffectiveSorts, OverlayEdge, DeleteEdge)
        ]
  mapM_
    ( \(ordinal, (object, context, liveForm, deleteForm)) -> do
        let liveSpecification = baseSpecification (fromIntegral (40 + ordinal)) object context liveState
            deleteSpecification = baseSpecification (fromIntegral (80 + ordinal)) object context (releasedDeleted 0)
            liveDecision = fixtureDecision (fromIntegral (40 + ordinal))
            deleteDecision = fixtureDecision (fromIntegral (80 + ordinal))
        live <- checked "prepare live role" (prepareLabelPatch liveSpecification generator initialState)
        deleted <- checked "prepare delete role" (prepareLabelPatch deleteSpecification generator initialState)
        liveRelease <- releasePrepared liveSpecification live
        deleteRelease <- releasePrepared deleteSpecification deleted
        let liveRecipe = requireReleasedRecipe liveRelease
            deleteRecipe = requireReleasedRecipe deleteRelease
        assertEqual "live recipe" liveForm (labelPatchRecipeForm liveRecipe)
        assertEqual "delete recipe" deleteForm (labelPatchRecipeForm deleteRecipe)
        assertEqual
          "live release builds the current installation recipe"
          liveForm
          (labelPatchRecipeForm (requireReleasedRecipe liveRelease))
        assertEqual
          "delete release builds the current installation recipe"
          deleteForm
          (labelPatchRecipeForm (requireReleasedRecipe deleteRelease))
        assertEqual
          "live installation retains its original local role"
          (labelPatchRecipeRole liveRecipe)
          (installedLabelRole (preparedReleasedPatchInstallation liveRelease))
        assertEqual
          "delete installation retains its original local role"
          (labelPatchRecipeRole deleteRecipe)
          (installedLabelRole (preparedReleasedPatchInstallation deleteRelease))
        assertEqual
          "live installation discards the preparation recipe"
          Nothing
          (lookupRetainedLabelPatch liveDecision (commitPatchRelease liveRelease) >>= retainedPatchViewPreparation)
        assertEqual
          "delete installation discards the preparation recipe"
          Nothing
          (lookupRetainedLabelPatch deleteDecision (commitPatchRelease deleteRelease) >>= retainedPatchViewPreparation)
        assertEqual
          "live role reaches its retained release terminal"
          (Just (PatchReleasedView (decisionIndex liveSpecification)))
          ( retainedPatchViewPhase
              <$> lookupRetainedLabelPatch liveDecision (commitPatchRelease liveRelease)
          )
        assertEqual
          "delete role reaches its retained release terminal"
          (Just (PatchReleasedView (decisionIndex deleteSpecification)))
          ( retainedPatchViewPhase
              <$> lookupRetainedLabelPatch deleteDecision (commitPatchRelease deleteRelease)
          )
    )
    (zip [0 :: Int ..] roles)

multiSortEdgeDelete :: Assertion
multiSortEdgeDelete = do
  generator <- fixtureGenerator
  let object = fixtureObject 14
      occurrence = fixtureOccurrence 14
      sorts = fixtureEffectiveSorts
      context =
        edgePatchContext
          occurrence
          fixtureEffectiveSortCut
      deletion = baseSpecification 14 object context (releasedDeleted 0)
      overlay = baseSpecification 15 (fixtureObject 15) context liveState
  deleted <- checked "prepare edge deletion" (prepareLabelPatch deletion generator initialState)
  overlaid <- checked "prepare edge overlay" (prepareLabelPatch overlay generator initialState)
  releasedDeletion <- releasePrepared deletion deleted
  releasedOverlay <- releasePrepared overlay overlaid
  let deletionRecipe = requireReleasedRecipe releasedDeletion
      overlayRecipe = requireReleasedRecipe releasedOverlay
      (heldDeletion, _) = commitLabelPatch deleted
  assertEqual "every contextual sort is affected" sorts (labelPatchRecipeAffectedSorts deletionRecipe)
  assertEqual "a label-only edge overlay has no graph scope" Set.empty (labelPatchRecipeAffectedSorts overlayRecipe)
  mapM_
    ( \sort ->
        assertBool
          "each affected sort is held"
          (HeldEffectiveSortWork sort `Set.member` labelPatchRecipeHeldWork deletionRecipe)
    )
    (Set.toAscList sorts)
  assertBool
    "edge structural subject is held by construction"
    ( HeldStructuralWork object (dynamicStructuralTarget occurrence)
        `Set.member` labelPatchRecipeHeldWork deletionRecipe
    )
  assertBool
    "edge deletion holds the future-sort wildcard"
    (HeldAllEffectiveSortWork `Set.member` labelPatchRecipeHeldWork deletionRecipe)
  assertBool
    "a sort induced after preparation is covered by the wildcard"
    (targetWorkIsHeld (HeldEffectiveSortWork (fixtureSort 0xf0)) heldDeletion)
  assertBool
    "a concrete dependent structural occurrence is covered by its subject wildcard"
    ( targetWorkIsHeld
        ( HeldStructuralDependentWork
            (dynamicStructuralTarget occurrence)
            (dynamicStructuralTarget (fixtureOccurrence 0xf1))
        )
        heldDeletion
    )
  assertBool
    "edge overlay does not freeze future sorts"
    (HeldAllEffectiveSortWork `Set.notMember` labelPatchRecipeHeldWork overlayRecipe)

multiSortDeltaScope :: Assertion
multiSortDeltaScope = do
  generator <- fixtureGenerator
  let delta = fixtureDelta 32
      object = globalObjectIdFromDeltaId delta
      occurrence = fixtureOccurrence 32
      sorts = fixtureEffectiveSorts
      activatedSorts = Set.singleton fixturePlacementSort
      context =
        deltaPatchContext
          delta
          occurrence
          (fixtureDeltaSortCut delta)
          fixtureLocalProcessCut
      live = baseSpecification 16 object context liveState
      deletion = baseSpecification 17 object context (releasedDeleted 0)
  preparedLive <- checked "prepare live multi-sort delta" (prepareLabelPatch live generator initialState)
  preparedDelete <- checked "prepare deleted multi-sort delta" (prepareLabelPatch deletion generator initialState)
  releasedLive <- releasePrepared live preparedLive
  releasedDelete <- releasePrepared deletion preparedDelete
  let liveRecipe = requireReleasedRecipe releasedLive
      deleteRecipe = requireReleasedRecipe releasedDelete
      (heldLive, _) = commitLabelPatch preparedLive
  assertEqual "the disposition map defines the complete affected cut" sorts (labelPatchRecipeAffectedSorts liveRecipe)
  assertEqual
    "only activation-required sorts reserve identities"
    activatedSorts
    (Set.map storeActivationSort (Map.keysSet (labelPatchRecipePreallocatedStores liveRecipe)))
  assertEqual "delete retains the same complete delta scope" sorts (labelPatchRecipeAffectedSorts deleteRecipe)
  assertEqual "delete never reserves replacement Stores" Map.empty (labelPatchRecipePreallocatedStores deleteRecipe)
  mapM_
    ( \sort ->
        assertBool
          "each delta sort is held"
          (HeldEffectiveSortWork sort `Set.member` labelPatchRecipeHeldWork liveRecipe)
    )
    (Set.toAscList sorts)
  assertBool
    "a concrete future Store incarnation is covered by the delta wildcard"
    ( targetWorkIsHeld
        (HeldStoreWork delta (fixtureStoreIncarnation 0xf2))
        heldLive
    )
  assertBool
    "a concrete future alignment incarnation is covered by the delta wildcard"
    ( targetWorkIsHeld
        (HeldAlignmentWork delta (fixtureStoreIncarnation 0xf3))
        heldLive
    )

deltaSortMustBeEffective :: Assertion
deltaSortMustBeEffective = do
  let delta = fixtureDelta 35
      outside = fixtureSort 0xf4
  assertEqual
    "a complete cut cannot silently drop the placement sort"
    (Left (PatchContextPlacementSortNotEffective delta outside))
    ( checkedDeltaSortCut
        delta
        fixtureSortRegistry
        (fixturePlacementStateAt delta outside)
        fixtureEmptyStore
    )

deltaToVoid :: Assertion
deltaToVoid = do
  generator <- fixtureGenerator
  let delta = fixtureDelta 34
      context =
        deltaPatchContext
          delta
          (fixtureOccurrence 34)
          (fixtureDeltaSortCut delta)
          fixtureLocalProcessCut
      specification =
        baseSpecification
          18
          (globalObjectIdFromDeltaId delta)
          context
          (releasedLabel (VoidLabel, 0))
  prepared <-
    checked
      "prepare delta-to-void"
      (prepareLabelPatch specification generator initialState)
  released <- releasePrepared specification prepared
  assertEqual
    "void passivation allocates no replacement Store"
    Map.empty
    (labelPatchRecipePreallocatedStores (requireReleasedRecipe released))

deltaRemoteController :: Assertion
deltaRemoteController = do
  generator <- fixtureGenerator
  let delta = fixtureDelta 36
      context =
        deltaPatchContext
          delta
          (fixtureOccurrence 36)
          (fixtureDeltaSortCut delta)
          fixtureLocalProcessCut
      specification =
        baseSpecification
          19
          (globalObjectIdFromDeltaId delta)
          context
          (releasedLabel ((ProcessLabel (fixtureProcess 2), 0)))
  prepared <-
    checked
      "prepare remote delta handoff"
      (prepareLabelPatch specification generator initialState)
  released <- releasePrepared specification prepared
  assertEqual
    "a remotely resident controller cannot allocate a local Store"
    Map.empty
    (labelPatchRecipePreallocatedStores (requireReleasedRecipe released))

atomicPreallocation :: Assertion
atomicPreallocation = do
  generator <- fixtureGenerator
  let delta = fixtureDelta 31
      object = globalObjectIdFromDeltaId delta
      context = deltaPatchContext delta (fixtureOccurrence 2) (fixtureDeltaSortCut delta) fixtureLocalProcessCut
      specification = baseSpecification 1 object context liveState
  prepared <- checked "prepare delta holds" (prepareLabelPatch specification generator initialState)
  let (pending, unchangedGenerator) = commitLabelPatch prepared
  assertEqual "fresh disposition" PatchPreparedFresh (preparedLabelPatchDisposition prepared)
  assertBool "local preparation retains only pending evidence" ((lookupRetainedLabelPatch (fixtureDecision 1) pending >>= retainedPatchViewPreparation) /= Nothing)
  assertEqual "local preparation allocates no Store identity" (generatorCounterWitness generator) (generatorCounterWitness unchangedGenerator)
  released <- checked "allocate Store at installation" (preparePatchRelease specification (decisionIndex specification) unchangedGenerator pending)
  let (installed, generated) = LabelPatch.commitPatchRelease released
      stores = labelPatchRecipePreallocatedStores (requireReleasedRecipe released)
  assertEqual "installation allocates exactly one identity per activation" 1 (Map.size stores)
  assertEqual "generator advances atomically with installation" 1 (generatorCounterWitness generated)
  assertEqual "predecessor remained unchanged" 0 (generatorCounterWitness generator)
  assertEqual
    "installed exact replay cannot rewind Store allocation"
    (Left (PatchGeneratorPrecedesRetainedAllocation 1 0))
    (preparedLabelPatchDisposition <$> prepareLabelPatch specification generator installed)

workSurvivesInstallation :: Assertion
workSurvivesInstallation = do
  generator <- fixtureGenerator
  let specification = baseSpecification 1 (fixtureObject 31) ordinaryContext liveState
      observed = LabelStructuralWork 1 2 3 4 5 6 7 8 9 10 11
      initial = recordLabelStructuralWork observed initialState
      expected = labelStructuralWorkCounts initial
  prepared <- checked "prepare with retained work counts" (prepareLabelPatch specification generator initial)
  let (pending, pendingGenerator) = commitLabelPatch prepared
  assertEqual "preparation retains all cumulative work" expected (labelStructuralWorkCounts pending)
  released <- checked "install with retained work counts" (preparePatchRelease specification (decisionIndex specification) pendingGenerator pending)
  let (installed, installedGenerator) = LabelPatch.commitPatchRelease released
  assertEqual "installation retains all cumulative work" expected (labelStructuralWorkCounts installed)
  replayed <- checked "replay with retained work counts" (preparePatchRelease specification (decisionIndex specification) installedGenerator installed)
  assertEqual "exact replay does not reset or recount work" expected (labelStructuralWorkCounts (commitPatchRelease replayed))

compactInstallationEvidence :: Assertion
compactInstallationEvidence = do
  generator <- fixtureGenerator
  let delta = fixtureDelta 30
      object = globalObjectIdFromDeltaId delta
      context = deltaPatchContext delta (fixtureOccurrence 30) (fixtureDeltaSortCut delta) fixtureLocalProcessCut
      specification = baseSpecification 30 object context liveState
  prepared <- checked "prepare active delta holds" (prepareLabelPatch specification generator initialState)
  let (active, unchangedGenerator) = commitLabelPatch prepared
  assertEqual "pending preparation reserves no Store" (generatorCounterWitness generator) (generatorCounterWitness unchangedGenerator)
  released <- checked "release active delta" (preparePatchRelease specification (decisionIndex specification) unchangedGenerator active)
  let (completed, generated) = LabelPatch.commitPatchRelease released
      installed = preparedReleasedPatchInstallation released
      recipe = requireReleasedRecipe released
  retained <- maybe (assertFailure "installation was not retained") pure (lookupRetainedLabelPatch (fixtureDecision 30) completed)
  assertEqual "historical state contains no pending holds" Nothing (retainedPatchViewPreparation retained)
  assertEqual "historical state contains exact installed evidence" (Just installed) (retainedPatchViewInstallation retained)
  assertEqual "installed facts retain canonical outcome and predecessor" (patchSpecificationPreparedFacts specification) (installedLabelPreparedFacts installed)
  assertEqual "installed digest remains exact" (patchSpecificationPreparedDigest specification) (installedLabelPreparedDigest installed)
  assertEqual "installed role is the role used by the effect applicator" (labelPatchRecipeRole recipe) (installedLabelRole installed)
  assertEqual "installed revision remains exact" (decisionIndex specification) (installedLabelReleaseIndex installed)
  assertEqual "completed preparation retains no target holds" Set.empty (heldTargetWork completed)
  assertBool "transient release recipe authenticates against installed proof" (installedLabelMatchesRecipe installed recipe)
  replay <- checked "replay preparation after installation" (prepareLabelPatch specification generated completed)
  assertEqual "installed replay cannot reconstruct pending work" Nothing (preparedLabelPatchPreparation replay)
  assertBool "installed replay retains all state and allocates nothing" (commitLabelPatch replay == (completed, generated))
  assertEqual
    "installed replay still rejects a generator before the retained allocation"
    (Left (PatchGeneratorPrecedesRetainedAllocation (generatorCounterWitness generated) (generatorCounterWitness generator)))
    (preparedLabelPatchDisposition <$> prepareLabelPatch specification generator completed)
  releaseReplay <- checked "replay installed release" (preparePatchRelease specification (decisionIndex specification) generated completed)
  assertEqual "release replay returns no recipe to apply" Nothing (preparedReleasedPatchRecipe releaseReplay)
  assertEqual "release replay retains historical proof" installed (preparedReleasedPatchInstallation releaseReplay)
  assertBool "release replay preserves generator and owners" (LabelPatch.commitPatchRelease releaseReplay == (completed, generated))

-- The decision and its prepared report are unchanged when local description
-- knowledge grows. The transient installation plan must use the supplied current
-- owner cut, while the retained preparation provenance still authenticates replay.
releaseAfterMaterialization :: Assertion
releaseAfterMaterialization = do
  generator <- fixtureGenerator
  let delta = fixtureDelta 37
      object = globalObjectIdFromDeltaId delta
      absent = baseSpecification 37 object absentOrdinaryPatchContext liveState
      materialized =
        patchSpecification
          (patchSpecificationPreparedFacts absent)
          (patchSpecificationPreparedDigest absent)
          (deltaPatchContext delta (fixtureOccurrence 37) (fixtureDeltaSortCut delta) fixtureLocalProcessCut)
  prepared <- checked "local preparation before local description" (prepareLabelPatch absent generator initialState)
  let (pending, unadvanced) = commitLabelPatch prepared
  pendingView <- maybe (assertFailure "missing pending decision") pure (preparedLabelPatchPreparation prepared)
  assertEqual "local preparation provenance records absence" AbsentOrdinaryTargetView (targetRoleView (pendingLabelPreparationRole pendingView))
  released <- checked "installation after local description" (preparePatchRelease materialized (decisionIndex materialized) unadvanced pending)
  let (installedState, generated) = LabelPatch.commitPatchRelease released
      installed = preparedReleasedPatchInstallation released
      recipe = requireReleasedRecipe released
  assertEqual "installation uses the current delta role" (targetRoleView (patchSpecificationRole materialized)) (targetRoleView (installedLabelRole installed))
  assertEqual "installation separately retains local preparation provenance" AbsentOrdinaryTargetView (targetRoleView (installedLabelPreparationRole installed))
  assertEqual "current delta consequences allocate its required Store" 1 (Map.size (labelPatchRecipePreallocatedStores recipe))
  assertBool "the current direct-effects plan authenticates against installation" (installedLabelMatchesRecipe installed recipe)
  replay <- checked "replay original absent local preparation" (prepareLabelPatch absent generated installedState)
  assertEqual "local preparation replay does not resurrect pending work" Nothing (preparedLabelPatchPreparation replay)
  assertBool "local preparation replay preserves all installed owners" (commitLabelPatch replay == (installedState, generated))
  repeated <- checked "installation replay after another context change" (preparePatchRelease absent (decisionIndex absent) generated installedState)
  assertEqual "installation replay cannot rewrite historical installation role" installed (preparedReleasedPatchInstallation repeated)
  assertEqual "installation replay creates no second direct-effects plan" Nothing (preparedReleasedPatchRecipe repeated)
  assertBool "installation replay allocates nothing" (LabelPatch.commitPatchRelease repeated == (installedState, generated))

releaseGeneratorInterleaving :: Assertion
releaseGeneratorInterleaving = do
  generator <- fixtureGenerator
  let delta = fixtureDelta 38
      specification =
        baseSpecification
          38
          (globalObjectIdFromDeltaId delta)
          (deltaPatchContext delta (fixtureOccurrence 38) (fixtureDeltaSortCut delta) fixtureLocalProcessCut)
          liveState
  prepared <- checked "retain local preparation before unrelated allocation" (prepareLabelPatch specification generator initialState)
  let (pending, unchanged) = commitLabelPatch prepared
  mapM_ (checkInterleaving pending unchanged specification) [0 .. 31]
  where
    checkInterleaving pending generator specification count = do
      let currentGenerator = advanceGenerator count generator
      released <- checked "install using current generator" (preparePatchRelease specification (decisionIndex specification) currentGenerator pending)
      let (installed, generated) = LabelPatch.commitPatchRelease released
      assertEqual "installation preserves unrelated IDs and adds exactly one Store" (generatorCounterWitness currentGenerator + 1) (generatorCounterWitness generated)
      repeated <- checked "repeat after unrelated allocations" (preparePatchRelease specification (decisionIndex specification) generated installed)
      assertBool "retry preserves the exact current generator" (LabelPatch.commitPatchRelease repeated == (installed, generated))

exactReplay :: Assertion
exactReplay = do
  generator <- fixtureGenerator
  let object = fixtureObject 9
      specification = baseSpecification 2 object ordinaryContext liveState
  first <- checked "prepare first" (prepareLabelPatch specification generator initialState)
  let (preparedState, generatedState) = commitLabelPatch first
  replay <- checked "prepare exact replay" (prepareLabelPatch specification generatedState preparedState)
  assertEqual "replay classification" PatchPreparationExactReplay (preparedLabelPatchDisposition replay)
  assertBool "generator unchanged" (snd (commitLabelPatch replay) == generatedState)
  assertEqual "canonical facts and holds unchanged" (preparedLabelPatchPreparation first) (preparedLabelPatchPreparation replay)
  released <-
    checked
      "release"
      ( preparePatchRelease
          specification
          (decisionIndex specification)
          generatedState
          preparedState
      )
  let releasedState = commitPatchRelease released
  repeated <-
    checked
      "repeat release"
      ( preparePatchRelease
          specification
          (decisionIndex specification)
          generatedState
          releasedState
      )
  assertEqual "release replay state" releasedState (commitPatchRelease repeated)

foreignGeneratorReplay :: Assertion
foreignGeneratorReplay = do
  generator <- fixtureGenerator
  foreignGenerator <-
    checked
      "initialize foreign generator"
      (IdGenerator.initialState fixtureGeneratorSeedH2)
  let specification =
        baseSpecification
          21
          (fixtureObject 21)
          ordinaryContext
          liveState
  first <-
    checked
      "prepare retained lineage"
      (prepareLabelPatch specification generator initialState)
  let (retained, _) = commitLabelPatch first
      foreignAhead = advanceGenerator 8 foreignGenerator
  assertBool
    "the unrelated lineage really has a higher counter"
    ( generatorCounterWitness foreignAhead
        > generatorCounterWitness generator
    )
  assertEqual
    "counter position alone cannot authenticate exact replay"
    (Left PatchGeneratorLineageMismatch)
    (preparedLabelPatchDisposition <$> prepareLabelPatch specification foreignAhead retained)

typedFactsCoherence :: Assertion
typedFactsCoherence = do
  let object = fixtureObject 22
      prior = liveState
      proposed = releasedLabel ((ProcessLabel (fixtureProcess 2), 0))
      facts = fixturePreparedAuthorityFacts 22 object prior Nothing proposed
      digest = derivePreparedLabelDigest facts
      specification = patchSpecification facts digest ordinaryContext
      factsWithoutAuthority = fixturePreparedFacts 22 object prior Nothing proposed
      sameDecisionWithoutAuthority =
        patchSpecification
          factsWithoutAuthority
          (derivePreparedLabelDigest factsWithoutAuthority)
          ordinaryContext
  assertEqual
    "the specification retains the exact admitted facts"
    facts
    (patchSpecificationPreparedFacts specification)
  assertEqual
    "the live specification carries the common prepared-facts digest"
    (preparedLabelDigestBytes digest)
    (preparedLabelDigestBytes (patchSpecificationPreparedDigest specification))
  assertEqual
    "a changed controller derives release-time authority from checked prepared evidence"
    DeriveAuthorityAtReleaseView
    (preparedAuthorityDispositionView (patchSpecificationAuthorityDisposition specification))
  assertBool
    "omitting checked authority evidence changes the retained specification and digest"
    ( specification /= sameDecisionWithoutAuthority
        && patchSpecificationPreparedDigest specification
          /= patchSpecificationPreparedDigest sameDecisionWithoutAuthority
    )

chainedAuthorityCoherence :: Assertion
chainedAuthorityCoherence = do
  let nabla = fixtureNabla 24
      object = globalObjectIdFromNablaId nabla
      processBState = releasedLabel ((ProcessLabel (fixtureProcess 2), 1))
      processBAgain = releasedLabel (ProcessLabel (fixtureProcess 2), 2)
      initialOwner = fixtureNablaControlledState nabla (fixtureProcess 1) (fixtureHerald 1)
      changedFacts =
        fixturePreparedAuthorityFacts
          25
          object
          liveState
          Nothing
          processBState
      changedRevision = fixtureRevision 26
      afterChanged =
        installControlledRelease
          changedFacts
          changedRevision
          initialOwner
      changedAuthority = labelAuthorityEpoch (controlIndex 26)
      sameFacts =
        fixturePreparedFactsWithAuthority
          27
          object
          processBState
          (Just changedRevision)
          changedAuthority
          (existingReleasedAuthority changedRevision)
          processBState
      sameRevision = fixtureRevision 28
      afterSame =
        installControlledRelease
          sameFacts
          sameRevision
          afterChanged
      sameSpecification =
        patchSpecification
          sameFacts
          (derivePreparedLabelDigest sameFacts)
          ordinaryContext
      passivatedFacts =
        fixturePreparedFactsWithAuthority
          29
          object
          processBAgain
          (Just sameRevision)
          changedAuthority
          (existingReleasedAuthority sameRevision)
          (releasedLabel (VoidLabel, 0))
      passivatedRevision = fixtureRevision 30
      afterPassivated =
        installControlledRelease
          passivatedFacts
          passivatedRevision
          afterSame
      reactivatedFacts =
        fixturePreparedFactsWithAuthority
          31
          object
          (releasedLabel (VoidLabel, 3))
          (Just passivatedRevision)
          changedAuthority
          (existingReleasedAuthority passivatedRevision)
          processBState
      afterReactivated =
        installControlledRelease
          reactivatedFacts
          (fixtureRevision 32)
          afterPassivated
      alternateVoidFacts =
        fixturePreparedAuthorityFacts
          25
          object
          liveState
          Nothing
          (releasedLabel (VoidLabel, 0))
      afterAlternateVoid =
        installControlledRelease
          alternateVoidFacts
          changedRevision
          initialOwner
      alternateBfacts =
        fixturePreparedFactsWithAuthority
          27
          object
          (releasedLabel (VoidLabel, 1))
          (Just changedRevision)
          genesisAuthorityEpoch
          (existingReleasedAuthority changedRevision)
          processBState
      afterAlternateB =
        installControlledRelease
          alternateBfacts
          sameRevision
          afterAlternateVoid
      alternateAuthority = labelAuthorityEpoch (controlIndex 28)
      alternatePassivatedFacts =
        fixturePreparedFactsWithAuthority
          29
          object
          processBAgain
          (Just sameRevision)
          alternateAuthority
          (existingReleasedAuthority sameRevision)
          (releasedLabel (VoidLabel, 0))
      passivatedRecord = requiredReleasedLabelRecord object afterPassivated
      reactivatedRecord = requiredReleasedLabelRecord object afterReactivated
  assertEqual
    "same-label facts bind the exact label-release tenure"
    (Just (LabelAuthorityEpochView (controlIndex 26)))
    (authorityEpochView <$> preparedLabelFactsExpectedPriorAuthority sameFacts)
  assertEqual
    "same-label disposition retains that tenure"
    (RetainPriorAuthorityView changedAuthority)
    (preparedAuthorityDispositionView (patchSpecificationAuthorityDisposition sameSpecification))
  assertEqual
    "PatchSpecification carries the exact live common digest"
    (derivePreparedLabelDigest sameFacts)
    (patchSpecificationPreparedDigest sameSpecification)
  assertEqual
    "passivation retains the established label tenure in prepared evidence"
    (RetainPriorAuthorityView changedAuthority)
    (preparedAuthorityDispositionView (patchSpecificationAuthorityDisposition (patchSpecification passivatedFacts (derivePreparedLabelDigest passivatedFacts) ordinaryContext)))
  assertEqual
    "authority origin remains r26 after the intervening r28 same-label record"
    (Just (LabelAuthorityEpochView (controlIndex 26)))
    (authorityEpochView <$> labelRecordRetainedAuthority passivatedRecord)
  assertEqual
    "existing-release justification names the latest prior record r28"
    (Just (ExistingReleasedAuthorityView sameRevision))
    ( priorAuthorityJustificationView
        <$> preparedLabelFactsPriorAuthorityJustification passivatedFacts
    )
  assertBool
    "forked histories with the same B/r28 prior but different authority origins digest differently"
    ( derivePreparedLabelDigest passivatedFacts
        /= derivePreparedLabelDigest alternatePassivatedFacts
    )
  assertEqual
    "reactivation derives and installs a new label tenure"
    (Just (LabelAuthorityEpochView (controlIndex 32)))
    (authorityEpochView <$> labelRecordRetainedAuthority reactivatedRecord)
  assertEqual
    "the alternate history really established its different r28 tenure"
    (Just alternateAuthority)
    ( labelRecordRetainedAuthority
        (requiredReleasedLabelRecord object afterAlternateB)
    )

installationUsesDecisionIndex :: Assertion
installationUsesDecisionIndex = do
  generator <- fixtureGenerator
  let specification = baseSpecification 23 (fixtureObject 23) ordinaryContext liveState
      index = decisionIndex specification
      wrong = controlIndex 25
  prepared <- checked "prepare one local installation" (prepareLabelPatch specification generator initialState)
  let (retained, _) = commitLabelPatch prepared
  installed <- checked "install at canonical decision" (preparePatchRelease specification index generator retained)
  assertEqual "installation retains the decision coordinate" index (installedLabelReleaseIndex (preparedReleasedPatchInstallation installed))
  assertEqual
    "a different control coordinate is rejected"
    (Left (PatchDecisionIndexMismatch index wrong))
    (requireReleasedRecipe <$> preparePatchRelease specification wrong generator retained)

unrelatedInterleaving :: Assertion
unrelatedInterleaving = do
  generator <- fixtureGenerator
  let specificationA = baseSpecification 3 (fixtureObject 3) ordinaryContext liveState
      specificationB = baseSpecification 4 (fixtureObject 4) ordinaryContext liveState
  (stateA, generatorA) <- prepareAndCommit generator initialState specificationA
  (interleaved, _) <- prepareAndCommit generatorA stateA specificationB
  let unrelatedBefore = lookupRetainedLabelPatch (fixtureDecision 4) interleaved
  release <-
    checked
      "release A"
      ( preparePatchRelease
          specificationA
          (decisionIndex specificationA)
          generatorA
          interleaved
      )
  let successor = commitPatchRelease release
  assertEqual "unrelated retained patch" unrelatedBefore (lookupRetainedLabelPatch (fixtureDecision 4) successor)
  assertEqual "unrelated hold" True (targetWorkIsHeld (HeldObjectControlWork (fixtureObject 4)) successor)

exactTargetHolds :: Assertion
exactTargetHolds = do
  generator <- fixtureGenerator
  let objectA = fixtureObject 5
      objectB = fixtureObject 6
      specA = baseSpecification 5 objectA ordinaryContext liveState
      specB = baseSpecification 6 objectB ordinaryContext liveState
  (stateA, generatorA) <- prepareAndCommit generator initialState specA
  (stateB, _) <- prepareAndCommit generatorA stateA specB
  assertBool
    "object work cannot be understated by the checked context"
    (targetWorkIsHeld (HeldObjectControlWork objectB) stateB)
  assertBool
    "all target application work is held by category"
    (targetWorkIsHeld (HeldAllApplicationWorkForObject objectA) stateB)
  assertBool
    "all target peer work is held by category"
    (targetWorkIsHeld (HeldAllPeerWorkForObject objectA) stateB)
  assertBool
    "a concrete application item matches the object-scoped wildcard"
    ( targetWorkIsHeld
        (HeldApplicationWork objectA (fixtureProcess 1) 99)
        stateB
    )
  assertBool
    "a concrete peer publication matches the object-scoped wildcard"
    ( targetWorkIsHeld
        (HeldPeerPublication objectA (fixtureHerald 1) (fixturePublication 99))
        stateB
    )
  assertBool
    "process-lifecycle work cannot be understated by the checked context"
    (targetWorkIsHeld (HeldProcessLifecycleWork (fixtureProcess 1)) stateB)
  release <-
    checked
      "release A"
      ( preparePatchRelease
          specA
          (decisionIndex specA)
          generatorA
          stateB
      )
  let successor = commitPatchRelease release
  assertBool
    "A categorical hold removed"
    (not (targetWorkIsHeld (HeldAllApplicationWorkForObject objectA) successor))
  assertBool
    "A concrete application item is released with its wildcard"
    ( not
        ( targetWorkIsHeld
            (HeldApplicationWork objectA (fixtureProcess 1) 99)
            successor
        )
    )
  assertBool "B hold retained" (targetWorkIsHeld (HeldObjectControlWork objectB) successor)

terminalDelete :: Assertion
terminalDelete = do
  generator <- fixtureGenerator
  let object = fixtureObject 7
      deletion = baseSpecification 7 object ordinaryContext (releasedDeleted 0)
  (preparedState, generated) <- prepareAndCommit generator initialState deletion
  released <-
    checked
      "release delete"
      ( preparePatchRelease
          deletion
          (decisionIndex deletion)
          generated
          preparedState
      )
  let deletedState = commitPatchRelease released
  repeated <- checked "equal delayed delete replay" (prepareLabelPatch deletion generated deletedState)
  assertEqual "old decision still replays" PatchPreparationExactReplay (preparedLabelPatchDisposition repeated)
  let newSuccessor =
        baseSpecification
          8
          object
          ordinaryContext
          liveState
  assertEqual
    "a genuinely new decision cannot succeed the terminal deletion"
    (Left (PatchObjectTerminallyDeleted object))
    (preparedLabelPatchDisposition <$> prepareLabelPatch newSuccessor generated deletedState)

checkedBasePortable :: Assertion
checkedBasePortable = do
  generator <- fixtureGenerator
  receiverGenerator <- checked "receiver generator" (IdGenerator.initialState fixtureGeneratorSeedH2)
  let delta = fixtureDelta 81
      object = globalObjectIdFromDeltaId delta
      ordinary = baseSpecification 81 object ordinaryContext liveState
      structural =
        patchSpecification
          (patchSpecificationPreparedFacts ordinary)
          (patchSpecificationPreparedDigest ordinary)
          (deltaPatchContext delta (fixtureOccurrence 81) (fixtureDeltaSortCut delta) fixtureLocalProcessCut)
      counted = recordLabelStructuralWork (noLabelStructuralWork {labelWorkFullPreparations = 7}) initialState
  donorPrepared <- checked "prepare donor structural installation" (prepareLabelPatch structural (advanceGenerator 9 generator) counted)
  donorRelease <- releasePrepared structural donorPrepared
  alternatePrepared <- checked "prepare alternative local role" (prepareLabelPatch ordinary receiverGenerator initialState)
  alternateRelease <- releasePrepared ordinary alternatePrepared
  base <- checked "capture donor label base" (captureCheckedLabelPatchBase (commitPatchRelease donorRelease))
  alternateBase <- checked "capture same semantic facts from another host role" (captureCheckedLabelPatchBase (commitPatchRelease alternateRelease))
  assertEqual "portable evidence excludes role, generator prefix/floor and counters" base alternateBase
  assertEqual "a release cannot be imported below its canonical cut" (Left (PatchBaseReleaseBeyondCut (decisionIndex ordinary) (controlIndex 0))) (installCheckedLabelPatchBase (controlIndex 0) base initialState)
  imported <- checked "install into fresh receiver" (installCheckedLabelPatchBase (decisionIndex ordinary) base initialState)
  assertEqual "import does not fabricate receiver-local installation proof" Map.empty (retainedLabelPatches imported)
  assertEqual "import carries no held work" Set.empty (heldTargetWork imported)
  assertEqual "import carries no donor work counters" (labelStructuralWorkCounts initialState) (labelStructuralWorkCounts imported)
  proof <- maybe (assertFailure "missing imported decision proof") pure (lookupImportedLabelPatch (patchSpecificationDecision ordinary) imported)
  assertEqual "imported object" object (importedLabelPatchObject proof)
  assertEqual "imported canonical facts" (patchSpecificationPreparedFacts ordinary) (importedLabelPatchPreparedFacts proof)
  assertEqual "imported canonical digest" (patchSpecificationPreparedDigest ordinary) (importedLabelPatchPreparedDigest proof)
  assertEqual "imported exact revision" (decisionIndex ordinary) (importedLabelPatchReleaseIndex proof)
  let prior = patchSpecificationProposedState ordinary
      priorRevision = Just (fixtureRevision 82)
      wrongStateFacts = fixturePreparedFacts 83 object liveState priorRevision liveState
      wrongRevisionFacts = fixturePreparedFacts 83 object prior (Just (fixtureRevision 81)) liveState
      specification facts = patchSpecification facts (derivePreparedLabelDigest facts) ordinaryContext
  assertEqual "baseline preserves full prior generation checks" (Left (PatchPriorStateMismatch prior liveState)) (preparedLabelPatchDisposition <$> prepareLabelPatch (specification wrongStateFacts) receiverGenerator imported)
  assertEqual "baseline preserves exact prior revision checks" (Left (PatchPriorRevisionMismatch priorRevision (Just (fixtureRevision 81)))) (preparedLabelPatchDisposition <$> prepareLabelPatch (specification wrongRevisionFacts) receiverGenerator imported)

checkedBaseBoundaries :: Assertion
checkedBaseBoundaries = do
  generator <- fixtureGenerator
  let object = fixtureObject 85
      deletion = baseSpecification 85 object ordinaryContext (releasedDeleted 0)
  pending <- checked "prepare terminal patch" (prepareLabelPatch deletion generator initialState)
  let (pendingState, _) = commitLabelPatch pending
  assertEqual "capture cannot transfer a pending installation" (Left (PatchObjectBusy object (patchSpecificationDecision deletion))) (captureCheckedLabelPatchBase pendingState)
  released <- releasePrepared deletion pending
  base <- checked "capture terminal predecessor" (captureCheckedLabelPatchBase (commitPatchRelease released))
  assertEqual "installation cannot overwrite receiver-local pending state" (Left PatchBaseTargetNotPristine) (installCheckedLabelPatchBase (decisionIndex deletion) base pendingState)
  imported <- checked "install terminal predecessor" (installCheckedLabelPatchBase (decisionIndex deletion) base initialState)
  assertEqual "installation cannot replace an imported baseline" (Left PatchBaseTargetNotPristine) (installCheckedLabelPatchBase (decisionIndex deletion) base imported)
  assertEqual "terminal import remains terminal" (Left (PatchObjectTerminallyDeleted object)) (preparedLabelPatchDisposition <$> prepareLabelPatch (baseSpecification 87 object ordinaryContext liveState) generator imported)
  recaptured <- checked "recapture imported terminal predecessor" (captureCheckedLabelPatchBase imported)
  assertEqual "recapture preserves current terminal evidence" base recaptured

propCheckedBaseSuccession :: QC.Positive Int -> QC.Positive Int -> QC.Property
propCheckedBaseSuccession (QC.Positive before) (QC.Positive after) = QC.ioProperty $ do
  donorGenerator <- fixtureGenerator
  receiverGenerator <- checked "independent receiver generator" (IdGenerator.initialState fixtureGeneratorSeedH2)
  let prefixCount = 1 + before `mod` 9
      suffixCount = 1 + after `mod` 9
      object = fixtureObject 89
      install (state, generator, previous) ordinal = do
        let (prior, revision) = case previous of
              Nothing -> (liveState, Nothing)
              Just previousSpecification -> (patchSpecificationProposedState previousSpecification, Just (checkedPure "prior revision" (mkLabelRevision (decisionIndex previousSpecification))))
            facts = fixturePreparedFacts (fromIntegral ordinal) object prior revision liveState
            specification = patchSpecification facts (derivePreparedLabelDigest facts) ordinaryContext
        prepared <- checked "prepare next suffix patch" (prepareLabelPatch specification generator state)
        released <- releasePrepared specification prepared
        let (successor, nextGenerator) = LabelPatch.commitPatchRelease released
        pure (successor, nextGenerator, Just specification)
  (donor, continuedGenerator, latest) <- foldM install (initialState, donorGenerator, Nothing) [1 .. prefixCount]
  base <- checked "capture generated prefix" (captureCheckedLabelPatchBase donor)
  imported <- checked "install generated prefix" (installCheckedLabelPatchBase (controlIndex (fromIntegral prefixCount + 1)) base initialState)
  let suffix = [prefixCount + 1 .. prefixCount + suffixCount]
  (uninterrupted, _, _) <- foldM install (donor, continuedGenerator, latest) suffix
  (adopted, _, _) <- foldM install (imported, receiverGenerator, latest) suffix
  expected <- checked "capture uninterrupted latest state" (captureCheckedLabelPatchBase uninterrupted)
  actual <- checked "capture adopted plus suffix latest state" (captureCheckedLabelPatchBase adopted)
  assertEqual "latest predecessor agrees with uninterrupted execution" expected actual
  assertEqual "base capture retains only one current predecessor per object" 1 (length (checkedLabelPatchBaseEntries actual))
  assertEqual "receiver retains only its own suffix installations" suffixCount (Map.size (retainedLabelPatches adopted))
  assertEqual "imported predecessor proof survives subsequent local installations" (importedLabelPatches imported) (importedLabelPatches adopted)
  assertEqual "adopted cut is independent of later local releases" (importedLabelBaseControlIndex imported) (importedLabelBaseControlIndex adopted)
  pure True

checkedBaseSuccessorAfterEnd :: Assertion
checkedBaseSuccessorAfterEnd = do
  generator <- fixtureGenerator
  let nabla = fixtureNabla 90
      object = globalObjectIdFromNablaId nabla
      process = fixtureProcess 1
      initialOwner = fixtureNablaControlledState nabla process (fixtureHerald 1)
      firstFacts = fixturePreparedAuthorityFacts 90 object liveState Nothing liveState
      first = patchSpecification firstFacts (derivePreparedLabelDigest firstFacts) ordinaryContext
      revision = fixtureRevision 91
      withLabel = installControlledRelease firstFacts revision initialOwner
      storedPrior = patchSpecificationProposedState first
      ended = Controlled.commitControlledProcessRetirement (Controlled.prepareControlledProcessRetirement process withLabel)
      effectivePrior = releasedLabel (ZombieLabel process, 1)
      target = releasedLabel (ProcessLabel (fixtureProcess 2), 0)
      successorFacts = fixturePreparedAuthorityFacts 92 object effectivePrior (Just revision) target
      successor = patchSpecification successorFacts (derivePreparedLabelDigest successorFacts) ordinaryContext
  assertEqual "End makes the unchanged generation's effective owner a Zombie" (Just effectivePrior) (Controlled.controlledEffectiveLabelState object ended)
  assertEqual
    "Controlled rejects the claimed Zombie prior when no End has occurred"
    (Left (Controlled.ControlledLabelReleasePriorStateMismatch effectivePrior storedPrior))
    (Controlled.preparedControlledLabelReleaseDisposition <$> Controlled.prepareControlledLabelRelease fixtureInitialProjection successorFacts (decisionIndex successor) withLabel)
  assertEqual
    "Controlled accepts the canonical next release against the ended owner"
    (Right Controlled.ControlledLabelReleaseInstalled)
    (Controlled.preparedControlledLabelReleaseDisposition <$> Controlled.prepareControlledLabelRelease fixtureInitialProjection successorFacts (decisionIndex successor) ended)
  prepared <- checked "prepare prior Process-labelled patch" (prepareLabelPatch first generator initialState)
  released <- releasePrepared first prepared
  let (installed, nextGenerator) = LabelPatch.commitPatchRelease released
  base <- checked "capture predecessor before takeover" (captureCheckedLabelPatchBase installed)
  imported <- checked "import predecessor before takeover" (installCheckedLabelPatchBase (decisionIndex first) base initialState)
  let prepareBoth specification =
        ( preparedLabelPatchDisposition <$> prepareLabelPatch specification nextGenerator installed,
          preparedLabelPatchDisposition <$> prepareLabelPatch specification nextGenerator imported
        )
      wrongProcess = releasedLabel (ZombieLabel (fixtureProcess 3), 1)
      wrongGeneration = releasedLabel (ZombieLabel process, 0)
      rejectedPriors =
        [ ("another ended process cannot replace the predecessor", wrongProcess, Just revision, PatchPriorStateMismatch storedPrior wrongProcess),
          ("End cannot change the predecessor generation", wrongGeneration, Just revision, PatchPriorStateMismatch storedPrior wrongGeneration),
          ("End cannot change the predecessor revision", effectivePrior, Just (fixtureRevision 90), PatchPriorRevisionMismatch (Just revision) (Just (fixtureRevision 90)))
        ]
  assertEqual
    "ordinary and imported exact predecessors admit the effective Zombie prior"
    (Right PatchPreparedFresh, Right PatchPreparedFresh)
    (prepareBoth successor)
  mapM_
    ( \(description, prior, priorRevision, problem) ->
        let facts = fixturePreparedAuthorityFacts 92 object prior priorRevision target
            specification = patchSpecification facts (derivePreparedLabelDigest facts) ordinaryContext
         in assertEqual description (Left problem, Left problem) (prepareBoth specification)
    )
    rejectedPriors

checkedPrior :: Assertion
checkedPrior = do
  generator <- fixtureGenerator
  let nabla = fixtureNabla 8
      object = globalObjectIdFromNablaId nabla
      initialOwner = fixtureNablaControlledState nabla (fixtureProcess 1) (fixtureHerald 1)
      firstFacts = fixturePreparedAuthorityFacts 22 object liveState Nothing liveState
      first =
        patchSpecification
          firstFacts
          (derivePreparedLabelDigest firstFacts)
          ordinaryContext
  (preparedState, generated) <- prepareAndCommit generator initialState first
  released <-
    checked
      "release first"
      ( preparePatchRelease
          first
          (decisionIndex first)
          generated
          preparedState
      )
  let current = commitPatchRelease released
      controlledOwner =
        installControlledRelease
          firstFacts
          (fixtureRevision 23)
          initialOwner
      wrongFacts =
        fixturePreparedFactsWithAuthority
          24
          object
          (releasedLabel (VoidLabel, 0))
          (Just (fixtureRevision 23))
          genesisAuthorityEpoch
          (existingReleasedAuthority (fixtureRevision 23))
          liveState
  assertEqual
    "live Controlled preparation rejects a wrong prior state"
    (Left (Controlled.ControlledLabelReleasePriorStateMismatch (releasedLabel (VoidLabel, 0)) (fixtureSuccessorState liveState liveState)))
    ( Controlled.preparedControlledLabelReleaseDisposition
        <$> Controlled.prepareControlledLabelRelease
          fixtureInitialProjection
          wrongFacts
          (controlIndex 25)
          controlledOwner
    )
  let wrongRevisionFacts =
        fixturePreparedFactsWithAuthority
          24
          object
          (fixtureSuccessorState liveState liveState)
          (Just (fixtureRevision 22))
          genesisAuthorityEpoch
          (existingReleasedAuthority (fixtureRevision 22))
          liveState
  assertEqual
    "live Controlled preparation rejects the wrong revision"
    ( Left
        ( Controlled.ControlledLabelReleasePriorRevisionMismatch
            (Just (fixtureRevision 22))
            (Just (fixtureRevision 23))
        )
    )
    ( Controlled.preparedControlledLabelReleaseDisposition
        <$> Controlled.prepareControlledLabelRelease
          fixtureInitialProjection
          wrongRevisionFacts
          (controlIndex 25)
          controlledOwner
    )
  let prior = fixtureSuccessorState liveState liveState
      validFacts =
        checkedPure
          "distinct successor at non-advancing decision index"
          ( mkPreparedLabelFacts
              (fixtureDecision 24)
              (controlIndex 23)
              object
              (fixtureSuccessorState prior liveState)
              prior
              (Just (fixtureRevision 23))
              profileCatalogueDigest
              (Just genesisAuthorityEpoch)
              (Just (existingReleasedAuthority (fixtureRevision 23)))
              (preparedAuthorityDisposition (Just genesisAuthorityEpoch) (fixtureReleasedLabel prior) liveState)
              (deriveMemberSetDigest fixtureActiveHeralds)
          )
      validSuccessor =
        patchSpecification
          validFacts
          (derivePreparedLabelDigest validFacts)
          ordinaryContext
  next <- checked "prepare checked successor" (prepareLabelPatch validSuccessor generated current)
  let (nextState, _) = commitLabelPatch next
  assertEqual
    "release revision must advance the checked prior"
    (Left (PatchReleaseRevisionDidNotAdvance (fixtureRevision 23) (fixtureRevision 23)))
    ( requireReleasedRecipe
        <$> preparePatchRelease
          validSuccessor
          (decisionIndex validSuccessor)
          generated
          nextState
    )

independentOrder :: Assertion
independentOrder = do
  generator <- fixtureGenerator
  let specifications =
        [ baseSpecification 31 (fixtureObject 31) ordinaryContext liveState,
          baseSpecification 32 (fixtureObject 32) absentOrdinaryPatchContext liveState,
          baseSpecification
            33
            (fixtureObject 33)
            (edgePatchContext (fixtureOccurrence 33) fixtureEffectiveSortCut)
            liveState
        ]
  outcomes <- mapM (applyAll generator) (permutations specifications)
  case outcomes of
    [] -> assertFailure "permutations unexpectedly empty"
    first : rest -> mapM_ (assertEqual "canonical retained patches" first) rest

applyAll ::
  IdGenerator.State ->
  [PatchSpecification] ->
  IO (Map.Map LabelDecisionId RetainedPatchView)
applyAll generator specifications = do
  (state, _) <- foldl step (pure (initialState, generator)) specifications
  pure (retainedLabelPatches state)
  where
    step accumulated specification = do
      (state, currentGenerator) <- accumulated
      prepareAndCommit currentGenerator state specification

prepareAndCommit ::
  IdGenerator.State ->
  State ->
  PatchSpecification ->
  IO (State, IdGenerator.State)
prepareAndCommit generator state specification =
  commitLabelPatch
    <$> checked "prepare patch" (prepareLabelPatch specification generator state)

baseSpecification ::
  Word8 ->
  GlobalObjectId ->
  PatchContext ->
  ReleasedLabelState ->
  PatchSpecification
baseSpecification ordinal object context proposed =
  let facts = fixturePreparedFacts ordinal object liveState Nothing proposed
   in patchSpecification facts (derivePreparedLabelDigest facts) context

ordinaryContext :: PatchContext
ordinaryContext = ordinaryControlledPatchContext

fixturePreparedFacts ::
  Word8 ->
  GlobalObjectId ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  ReleasedLabelState ->
  PreparedLabelFacts
fixturePreparedFacts ordinal object prior priorRevision proposed =
  checkedPure
    "prepared label facts"
    ( mkPreparedLabelFacts
        (fixtureDecision ordinal)
        (controlIndex (fromIntegral ordinal + 1))
        object
        (fixtureSuccessorState prior proposed)
        prior
        priorRevision
        profileCatalogueDigest
        Nothing
        Nothing
        noTargetAuthority
        (deriveMemberSetDigest fixtureActiveHeralds)
    )

fixturePreparedAuthorityFacts ::
  Word8 ->
  GlobalObjectId ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  ReleasedLabelState ->
  PreparedLabelFacts
fixturePreparedAuthorityFacts ordinal object prior priorRevision proposed =
  fixturePreparedFactsWithAuthority
    ordinal
    object
    prior
    priorRevision
    genesisAuthorityEpoch
    ( maybe
        (checkedGenesisAuthority fixtureInitialProjection)
        existingReleasedAuthority
        priorRevision
    )
    proposed

fixturePreparedFactsWithAuthority ::
  Word8 ->
  GlobalObjectId ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  AuthorityEpoch ->
  PriorAuthorityJustification ->
  ReleasedLabelState ->
  PreparedLabelFacts
fixturePreparedFactsWithAuthority ordinal object prior priorRevision authority justification proposed =
  checkedPure
    "prepared label facts with authority"
    ( mkPreparedLabelFacts
        (fixtureDecision ordinal)
        (controlIndex (fromIntegral ordinal + 1))
        object
        (fixtureSuccessorState prior proposed)
        prior
        priorRevision
        profileCatalogueDigest
        (Just authority)
        (Just justification)
        ( preparedAuthorityDisposition
            (Just authority)
            (fixtureReleasedLabel prior)
            proposed
        )
        (deriveMemberSetDigest fixtureActiveHeralds)
    )

installControlledRelease ::
  PreparedLabelFacts ->
  LabelRevision ->
  Controlled.State ->
  Controlled.State
installControlledRelease facts revision owner =
  Controlled.commitControlledLabelRelease
    ( checkedPure
        "install live Controlled label release"
        ( Controlled.prepareControlledLabelRelease
            fixtureInitialProjection
            facts
            (labelRevisionControlIndex revision)
            owner
        )
    )

requiredReleasedLabelRecord ::
  GlobalObjectId -> Controlled.State -> LabelRecord
requiredReleasedLabelRecord object owner =
  case Controlled.controlledReleasedLabelRecord object owner of
    Just record -> record
    Nothing -> error "live Controlled fixture lost its released label record"

fixtureReleasedLabel :: ReleasedLabelState -> Label
fixtureReleasedLabel state = case releasedLabelStateView state of
  ReleasedLabelView label -> label
  (ReleasedDeletedView _) -> error "fixture prior cannot be deleted"

liveState :: ReleasedLabelState
liveState = releasedLabel ((ProcessLabel (fixtureProcess 1), 0))

fixtureGenerator :: IO IdGenerator.State
fixtureGenerator = checked "initialize generator" (IdGenerator.initialState fixtureGeneratorSeed)

advanceGenerator :: Int -> IdGenerator.State -> IdGenerator.State
advanceGenerator remaining state
  | remaining <= 0 = state
  | otherwise =
      advanceGenerator
        (remaining - 1)
        ( IdGenerator.commitGeneratedId
            ( checkedPure
                "advance foreign generator"
                (IdGenerator.prepareGeneratedId state)
            )
        )

generatorCounterWitness :: IdGenerator.State -> Word64
generatorCounterWitness =
  IdGenerator.witnessedNextGeneratorCounter
    . IdGenerator.idGeneratorStateWitness

fixtureDecision :: Word8 -> LabelDecisionId
fixtureDecision byte = checkedPure "decision" (mkLabelDecisionId (bytes byte))

fixtureObject :: Word8 -> GlobalObjectId
fixtureObject byte = checkedPure "object" (mkGlobalObjectId (bytes byte))

fixtureProcess :: Word8 -> ProcessEpochId
fixtureProcess byte
  | byte == 1 = genesisLocalProcess
  | byte == 2 = genesisRemoteProcess
  | otherwise = checkedPure "process" (mkProcessEpochId (bytes byte))

fixtureHerald :: Word8 -> HeraldEpoch
fixtureHerald byte
  | byte == 1 = localHerald
  | byte == 2 = remoteHerald
  | otherwise = checkedPure "herald" (mkHeraldEpoch (bytes byte))

fixtureNabla :: Word8 -> NablaId
fixtureNabla byte = checkedPure "nabla" (mkNablaId (bytes byte))

fixtureDelta :: Word8 -> DeltaId
fixtureDelta byte = checkedPure "delta" (mkDeltaId (bytes byte))

fixtureStoreIncarnation :: Word8 -> StoreIncarnationId
fixtureStoreIncarnation byte =
  checkedPure "Store incarnation" (mkStoreIncarnationId (bytes byte))

fixturePublication :: Word64 -> PublicationId
fixturePublication sequenceNumber =
  publicationId
    (fixtureNabla 0xee)
    genesisAuthorityEpoch
    (fixtureHerald 1)
    (nablaSequence sequenceNumber)

fixtureSortId :: Word8 -> SortId
fixtureSortId byte = checkedPure "sort" (mkSortId (bytes byte))

fixtureSort :: Word8 -> SortOccurrence
fixtureSort byte =
  sortOccurrence
    (fixtureSortId byte)
    (checkedPure "sort occurrence" (mkSortDefinitionOccurrenceId (bytes (byte + 64))) :: SortDefinitionOccurrenceId)

fixtureAffectedSortCut :: CheckedAffectedSortCut
fixtureAffectedSortCut = checkedAffectedSortCut fixtureSortRegistry

fixtureEffectiveSortCut :: CheckedEffectiveSortCut
fixtureEffectiveSortCut = effectiveSortPatchCut fixtureSortRegistry

fixtureEffectiveSorts :: Set.Set SortOccurrence
fixtureEffectiveSorts =
  Set.fromList
    [ sortOccurrence
        (SortRegistry.registryEntrySortId entry)
        (SortRegistry.registryEntryOccurrenceId entry)
    | entry <- SortRegistry.registryEntries fixtureSortRegistry
    ]

fixtureSortRegistry :: SortRegistry.State
fixtureSortRegistry = SortRegistry.initialState fixtureStep14CheckedGenesis

fixtureLocalProcessCut :: CheckedLocalProcessCut
fixtureLocalProcessCut =
  checkedLocalProcessCut (fixtureHerald 1) fixtureOracleProjection

fixtureOracleProjection :: OracleProjection.State
fixtureOracleProjection = fixtureOracleProjectionState

fixtureDeltaSortCut :: DeltaId -> CheckedDeltaSortCut
fixtureDeltaSortCut delta =
  checkedPure
    "delta-sort cut"
    ( checkedDeltaSortCut
        delta
        fixtureSortRegistry
        (fixturePlacementState delta)
        fixtureEmptyStore
    )

fixturePlacementState :: DeltaId -> Placement.State
fixturePlacementState delta =
  fixturePlacementStateAt delta fixturePlacementSort

fixturePlacementStateAt :: DeltaId -> SortOccurrence -> Placement.State
fixturePlacementStateAt delta placementSort =
  fixturePlacementStateAtWithIncarnation
    delta
    placementSort
    fixturePlacementIncarnation

fixturePlacementStateAtWithIncarnation ::
  DeltaId -> SortOccurrence -> StoreIncarnationId -> Placement.State
fixturePlacementStateAtWithIncarnation delta placementSort incarnation =
  Placement.commitPlacementBootstrap
    ( checkedPure
        "delta placement"
        ( Placement.preparePlacementBootstrap
            [ Placement.localPlacement
                delta
                (sortOccurrenceSortId placementSort)
                (sortOccurrenceDefinition placementSort)
                (fixtureObject 0xe1)
                (fixtureProcess 1)
                (fixtureHerald 1)
                incarnation
                (controlIndex 1)
            ]
            (Placement.initialState (fixtureHerald 1))
        )
    )

fixtureEmptyStore :: Store.State
fixtureEmptyStore = checkedPure "empty Store" (Store.initialState fixtureStep14CheckedGenesis)

fixturePlacementSort :: SortOccurrence
fixturePlacementSort = fixtureCarrierSort DeltaRole

fixtureOccurrence :: Word8 -> StructuralOccurrenceId
fixtureOccurrence byte =
  structuralOccurrenceId
    (checkedPure "herald" (mkHeraldEpoch (bytes (byte + 96))) :: HeraldEpoch)
    (checkedPure "sequence" (mkStructuralSequence (fromIntegral byte + 1)))

fixtureRevision :: Word8 -> LabelRevision
fixtureRevision byte = checkedPure "revision" (mkLabelRevision (controlIndex (fromIntegral byte)))

fixtureInitialProjection :: InitialProjectionDigest
fixtureInitialProjection = checkedInitialProjectionDigest fixtureCheckedInitialBootstraps

fixtureActiveHeralds :: NonEmpty HeraldEpoch
fixtureActiveHeralds =
  case checkedActiveHeraldEpochs fixtureStep14CheckedGenesis of
    first : remaining -> first :| remaining
    [] -> error "checked fixture genesis has no active Heralds"

localHerald, remoteHerald :: HeraldEpoch
localHerald = checkedLocalHeraldEpoch fixtureStep14CheckedGenesis
remoteHerald = appliedProcessResidence fixtureRemoteBootstrap

genesisLocalProcess, genesisRemoteProcess :: ProcessEpochId
genesisLocalProcess = appliedProcessEpochId fixtureLocalBootstrap
genesisRemoteProcess = appliedProcessEpochId fixtureRemoteBootstrap

fixtureLocalBootstrap, fixtureRemoteBootstrap :: AppliedProcessBootstrap
fixtureLocalBootstrap =
  requiredBootstrap
    "local applied process"
    ((== localHerald) . appliedProcessResidence)
fixtureRemoteBootstrap =
  requiredBootstrap
    "remote applied process"
    ((/= localHerald) . appliedProcessResidence)

requiredBootstrap ::
  String -> (AppliedProcessBootstrap -> Bool) -> AppliedProcessBootstrap
requiredBootstrap label predicate =
  case filter predicate (checkedInitialBootstraps fixtureCheckedInitialBootstraps) of
    [bootstrap] -> bootstrap
    matches -> error (label <> ": expected one fixture bootstrap, found " <> show (length matches))

bytes :: Word8 -> ByteString.ByteString
bytes byte = ByteString.replicate 32 byte

checkedPure :: (Show error) => String -> Either error value -> value
checkedPure label = either (error . ((label <> ": ") <>) . show) id

checked :: (Show error) => String -> Either error value -> IO value
checked label = either (\problem -> assertFailure (label <> ": " <> show problem)) pure

-- A fixture destination is given the one successor of its captured prior.
fixtureSuccessorState :: ReleasedLabelState -> ReleasedLabelState -> ReleasedLabelState
fixtureSuccessorState prior destination =
  let next = releasedLabelStateGeneration prior + 1
   in case releasedLabelStateView destination of
        ReleasedLabelView (owner, _) -> releasedLabel (owner, next)
        ReleasedDeletedView _ -> releasedDeleted next

-- Fresh preparations and the first installation carry transient installation work;
-- completed replays deliberately expose only historical evidence.
commitPatchRelease :: PreparedPatchRelease -> State
commitPatchRelease = fst . LabelPatch.commitPatchRelease

releasePrepared :: PatchSpecification -> PreparedLabelPatch -> IO PreparedPatchRelease
releasePrepared specification prepared =
  let (state, generator) = commitLabelPatch prepared
   in checked "derive current installation" (preparePatchRelease specification (decisionIndex specification) generator state)

requireReleasedRecipe :: PreparedPatchRelease -> LabelPatchRecipe
requireReleasedRecipe = maybe (error "expected first-release recipe") id . preparedReleasedPatchRecipe

decisionIndex :: PatchSpecification -> ControlIndex
decisionIndex = preparedLabelFactsResolveIndex . patchSpecificationPreparedFacts
