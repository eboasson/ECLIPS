{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module AlignmentCoordinatorProperties
  ( tests,
    readyAlignmentFixture,
    connectedWithAlignment,
    generationPlan,
    promotedFixture,
    generationFastPathFixture,
    retainedCutQueryFixture,
    cachePreparedFor,
  )
where

import GenesisFixtures (fixtureHeraldAfterProbePrefix, fixtureRetirementResolution)

import ApplicationLabelProperties (settleStructuralPublication)
import Control.Monad (foldM, forM_, guard)
import ControlledRemovalProperties (settledDeltaControlledRemovalFixture)
import Data.List (elemIndex, find, nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( ContextClassGenerationId,
    HeraldPublicationPrefix (..),
    PhysicalPlacementRevisionVector,
    alignmentCutExactMembers,
    alignmentCutFreshMemberBaseEvidence,
    alignmentCutPhysicalPlacementRevisionVector,
    alignmentCutPredecessorGenerationIds,
    alignmentCutTopologyCut,
    alignmentMemberDelta,
    alignmentMemberHerald,
    alignmentMemberStoreIncarnation,
    deriveBootstrapEvidenceDigest,
    deriveMemberReadyEvidenceDigest,
    firstHeraldPublicationPosition,
    firstPlacementRevision,
    freshMemberBaseDelta,
    initialStoreRevision,
    memberReadyEvidenceDigestBytes,
    mkContextClassGenerationId,
    nextPlacementRevision,
    physicalPlacementRevisionEntries,
    physicalPlacementRevisionVector,
  )
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Context (ContextGraph, deriveContextGraph)
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve),
    VertexId (DeltaVertex),
    edgePayload,
  )
import Eclips.Domain.Identity
  ( DeltaId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    TopologyCutId,
    controlIndex,
    mkDeltaId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkStoreIncarnationId,
    mkStructuralSequence,
    mkTopologyCutId,
    structuralOccurrenceId,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication qualified as DomainPublication
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Sort.Profile qualified as SortProfile
import Eclips.Domain.Startup qualified as Startup
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    labelReleaseCause,
    processEndCause,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Topology
  ( TopologyCut,
    deriveTopologyCutId,
    deriveTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyCutFrontier,
    topologyFrontier,
    topologyFrontierAppliedControlPrefix,
  )
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.Alignment.CutQueries qualified as CutQueries
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationPlan,
    AlignmentGenerationPreparation (AlignmentGenerationHeld, AlignmentGenerationReady),
    GenerationMemberInput,
    alignmentGenerationAnnouncer,
    alignmentGenerationCut,
    alignmentGenerationCutAnnounce,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    generationMemberInput,
    prepareAlignmentGenerationPlan,
  )
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol
  ( AlignmentControl (..),
    AlignmentCutAccepted,
    AlignmentRouteCutoverMarker,
    ClassMemberReady,
    HistoricalCertificate,
    alignmentCutAccepted,
    alignmentCutAcceptedHerald,
    alignmentCutAcceptedPublicationPrefix,
    alignmentEvidenceSequenceWord64,
    alignmentObligationCause,
    alignmentRouteCutoverMarker,
    classMemberReady,
    classMemberReadyEvidenceSequence,
    classMemberReadyPredecessorAndBasePrefixDigest,
    classMemberReadySetDigest,
    destinationStore,
    firstAlignmentEvidenceSequence,
    historicalCertificate,
    nextAlignmentEvidenceSequence,
  )
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    peerCandidate,
    peerHello,
  )
import Eclips.Herald.Discovery.Internal qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (QueuePeerAlignmentEvidence, RejectPeerConnection, SendPeerControl, SetPeerCandidateDisposition),
    PeerProtocolDisposition (ClosePeerControlProtocol),
    effectBatchMembers,
    singletonEffectBatch,
  )
import Eclips.Herald.Genesis
  ( DeploymentManifest (..),
    HeraldMember (..),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( topologyCutAcceptance,
    topologyCutAnnounceId,
  )
import Eclips.Herald.Graph.Protocol qualified as GraphProtocol
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as Terminal
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( HeraldInput,
    HeraldInputBody (PeerInput),
    PeerControl (PeerAlignmentControl, PeerAlignmentEvidenceDelivered),
    PeerIngress (PeerControlReceived, PeerControlReceivedWithProgress),
    heraldInput,
  )
import Eclips.Herald.Join.Replay qualified as JoinReplay
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDelivery qualified as DeliveryOwner
import Eclips.Herald.PeerDelivery.State qualified as Delivery
import Eclips.Herald.PeerPublication qualified as PeerPublication
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement
  ( PlacementUpdate (FullPlacementSnapshot),
    deltaRouteOccurrenceId,
    deltaRouteSortId,
  )
import Eclips.Herald.Placement qualified as PlacementProtocol
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    freezeStartupSemanticControl,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupAlignmentState,
    replaceStartupDiscoveryState,
    replaceStartupGraphState,
    replaceStartupLastObservedTime,
    replaceStartupOracleProjectionState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupAlignmentCutQueryCache,
    startupAlignmentState,
    startupControlOracleProjectionState,
    startupControlledState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupInitialBootstraps,
    startupLastObservedTime,
    startupOracleProjectionState,
    startupPeerDeliveryState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    StructuralConsequenceDebt,
    StructuralDebtKind (TopologyAlignmentDebt),
    StructuralIdentity (StructuralVertexIdentity),
    normalizeStructuralDebts,
    sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
    structuralConsequenceDebt,
    structuralConsequenceDebtKey,
    structuralDebtEvidence,
    structuralDebtKey,
    structuralDebtKeyCause,
    structuralDebtKeySort,
    structuralDebtSetEntries,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Herald.UseCase.Alignment
  ( AlignmentCausePromotionDisposition (..),
    AlignmentGenerationControlDisposition (..),
    AlignmentGenerationWorkDisposition (..),
    AlignmentRouteCutoverProblem (..),
    activePlacementRoutesForSort,
    advanceAlignmentGenerations,
    advanceAlignmentGenerationsWithDisposition,
    advanceAlignmentGenerationsWithPreparationForTest,
    alignmentCausePromotionAuthorized,
    alignmentCausePromotionDisposition,
    alignmentCausesInProjectionOrder,
    alignmentFirstMatchingRecoveryCut,
    alignmentFirstViableRecoveryTopologyCut,
    alignmentGenerationControlDelta,
    alignmentPlanAnchorGeneration,
    alignmentReconnectRetryControls,
    alignmentRecoveryCandidateCuts,
    alignmentReplayTargetIsNext,
    alignmentRetryControls,
    anchorAlignmentPromotionAuthorized,
    anchorAlignmentPromotionMayDerive,
    applyAlignmentGenerationControlWithDisposition,
    deriveAlignmentGenerationPreparationAt,
    deriveAlignmentInputsAt,
    liveLatestAlignmentGenerationsForSort,
    prepareAlignmentRecoveryCutView,
    replayAlignmentPlanAt,
    retainAlignmentRouteCutoverMarker,
    retainedAnchorGenerationIds,
    spontaneousAlignmentPromotionAuthorized,
    spontaneousAlignmentPromotionMayDerive,
  )
import Eclips.Herald.UseCase.AlignmentTransfer
  ( AlignmentTransferWorkDisposition (..),
    advanceAlignmentTransferPass,
    advanceAlignmentTransferPassExhaustive,
    advanceAlignmentTransfers,
    advancePredecessorInputClosures,
    advancePredecessorInputClosuresExhaustive,
    advanceRouteCutovers,
    advanceSourcePrefixes,
    advanceSourcePrefixesExhaustive,
    alignmentSnapshotWithdrawnSources,
    applyAlignmentTransferControl,
    completeInputClosureWitnesses,
    routeCutoverSetComplete,
    sameAlignmentTransferWorkState,
    validateAlignmentTransferCoordinatorRelationships,
  )
import Eclips.Herald.UseCase.OracleAdvance (structuralReconciliationViews)
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.PeerDelivery qualified as DeliveryCoordinator
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Public.Types.ReceiptRetirement (emptyReceiptRetirement, receiptRetirementPrefix)
import GenesisFixtures
  ( currentPeerHelloReceived,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedInitialBootstraps,
    fixtureDeploymentAt,
    fixtureGeneratorSeed,
    fixtureHeraldRetirementReason,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
    fixtureStep14CheckedGenesis,
  )
import Step16RegularRetirementAcceptanceProperties (liveRegularAlignmentGateFixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    ioProperty,
    shuffle,
    testProperty,
    vectorOf,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "alignment generation coordinator"
    [ testProperty
        "scoped production fanout matches arbitrary-branch delta through ingress and pending release"
        propScopedGenerationFanout,
      testCase
        "first live placement observes exact withdrawals only after frontier adoption"
        caseSnapshotWithdrawnSources,
      testProperty
        "live debt index matches retained-history coverage through overlapping promotions and recovery"
        propLiveDebtIndex,
      testCase
        "passive history and frontier changes wake preparation only on new facts"
        casePreparationHistoricalReadinessChanges,
      testCase
        "semantic evidence catalogues are independent of delivery receipts"
        caseLevelTriggeredEvidence,
      testProperty
        "control delta and all four semantic catalogues match the payload reference on admitted histories"
        propControlDeltaReference,
      testCase
        "control delta preserves changed same-key payloads between independent valid branches"
        caseControlDeltaIndependentBranches,
      testCase
        "candidate slice expands late anchor dependencies without changing the old generation"
        caseControlDeltaLateAnchor,
      testCase
        "top-level fast path preserves pending work on duplicate cut acceptance"
        caseStepDuplicateCutAcceptanceFastPath,
      testCase
        "synthetic fast path reuses immutable evidence on duplicate cut announcement"
        caseStepDuplicateCutAnnounceFastPath,
      testCase
        "top-level fast path preserves pending work on duplicate member readiness"
        caseStepDuplicateMemberReadyFastPath,
      testCase
        "top-level fast path preserves pending work on duplicate historical certificate"
        caseStepDuplicateCertificateFastPath,
      testCase
        "deferred evidence rejection retains the newly admitted readiness and its receipt"
        caseDeferredRejectionKeepsFreshReceipt,
      testCase
        "valid piggyback progress survives rejection of the current semantic payload"
        caseRejectedEvidenceKeepsValidProgress,
      testCase
        "confirmed dependency-held evidence survives reconnect and later admission"
        caseConfirmedPendingEvidenceSurvivesReconnect,
      testCase
        "generation work drains admitted pending evidence and then quiesces"
        caseGenerationWorkDrainsThenQuiesces,
      testProperty
        "selective source prefixes match exhaustive ordered advancement across Store commits"
        propSelectiveSourcePrefixReference,
      testCase
        "weakened alignment preserves Weak genesis descriptions and read possession"
        caseWeakGenesisAlignment,
      testCase
        "local source delivery wakes an existing later source in the same pass"
        (caseSourcePrefixLoopback True),
      testCase
        "local source delivery retains an earlier source for the next pass"
        (caseSourcePrefixLoopback False),
      testCase
        "an unchanged whole-alignment pass terminates without effects"
        caseAlignmentTransferPassQuiesces,
      testProperty
        "explicit alignment work converges to the structural reference fixed point"
        propAlignmentTransferDispositionMatchesReference,
      testProperty
        "generated admitted evidence converges through changed and unchanged dispositions"
        propGeneratedAdmittedEvidenceConverges,
      testCase
        "duplicate generation evidence enables no reciprocal semantic work"
        caseGenerationEvidenceQueueQuiesces,
      testCase
        "route cutover authenticates source, predecessor, generation, and predecessor member"
        caseRouteCutoverAdmission,
      testProperty
        "admitted route closure queries match retained-history scans through replay and loss"
        propRouteClosureQueriesReference,
      testCase
        "predecessor source admission retains immutable closure facts through replay and cancellation"
        casePredecessorSourceAdmissionFacts,
      testProperty
        "blocked predecessor input closures stay dormant until scoped route changes"
        propSelectiveInputClosureReference,
      testCase
        "semantic membership retirement wakes blocked input closure while a frozen projection stays dormant"
        caseInputClosureMembershipWake,
      testCase
        "synchronous input closure Live advances later sources and defers earlier wakes"
        caseInputClosureLoopbackOrder,
      testCase
        "expected input owners survive closure and disappear only on plan invalidation"
        caseInputClosureExpectedOwnerIndex,
      testProperty
        "input closure witness lookups match reconstructed maps through prefix progress and retirement"
        propInputClosureWitnessQueriesReference,
      testCase
        "membership retirement projects route cutover work onto survivors without deleting history"
        caseRetiredRouteCutoverProjection,
      testCase
        "completed cutover candidate retirement reports exact owner progress"
        caseRouteCutoverRetirementWork,
      testCase
        "local member readiness allocates one immutable evidence sequence"
        caseLocalReadyAllocation,
      testCase
        "ahead generation evidence is retained and removed exactly"
        casePendingGenerationEvidence,
      testCase
        "fixed least Herald announces the anchor even when another Herald hosts it"
        caseFixedAnnouncerOwnsAnchor,
      testCase
        "retained generation announcement anchors remain distinct across attempts at the same cut"
        caseAttemptAnchors,
      testCase
        "empty plans use exact fixed-authority announcement and acceptance without generation work"
        caseEmptyPlanProtocolConvergence,
      testProperty
        "promotion preflight matches post-plan authorization under member presentation"
        propPromotionPreflightMatchesPostPlanAuthorization,
      testCase
        "nonleast whole-Herald spontaneous work holds without state or effects"
        caseNonleastWholeHeraldPromotionPreflight,
      testCase
        "a delayed same-cause anchor recovers after an already-observed incarnation loss"
        casePendingAnchorSameCauseRecovery,
      testCase
        "obsolete usable predecessors wait for withdrawal then reset past skipped successors"
        caseObsoletePlanResetRecovery,
      testCase
        "one fixed anchor vector converges three Heralds and cannot be overwritten"
        caseDivergentVectorAnchorConvergence,
      testProperty
        "post-seal readiness reaches a passive newcomer without old cut votes"
        propPassivePostSealEvidence,
      testCase
        "covered removal-only debt does not block a later anchor replay"
        caseCoveredRemovalOnlyDoesNotBlockReplay,
      testCase
        "unpromoted earlier debt still blocks a later anchor replay"
        caseUnpromotedEarlierDebtBlocksReplay,
      testCase
        "a live older coordinate does not block a later same-sort promotion"
        caseSequentialSameSortPromotion,
      testCase
        "invalidated Delta plans recover at the first route-matching deletion cut"
        caseInvalidatedDeltaPlansRecoverAtDeletionCut,
      testCase
        "recovery selection ignores a later installed suffix and pins anchor replay"
        caseRecoverySelectionIgnoresLaterInstalledSuffix,
      testProperty
        "batched recovery suffixes preserve ordered scans through holes, failures and changing bounds"
        propBatchedRecoverySelectionReference,
      testCase
        "batched recovery does not evaluate unreachable projections"
        caseBatchedRecoverySelectionDemand,
      testProperty
        "retained cut queries equal fresh preparation through repeated scope churn"
        propRetainedCutQueryScope,
      testCase
        "retained cut queries reuse answers without touching fresh source inputs"
        caseRetainedCutQueryReuse,
      testProperty
        "idle generation passes bypass fresh cut inputs and retire orphan vector queries"
        propIdleGenerationSkipsCutPreparation,
      testCase
        "unchanged non-announcement evidence preserves cut queries without fresh placement reads"
        caseUnchangedEvidenceSkipsCutPreparation,
      testCase
        "cut-query owner invalidation preserves semantic state equality"
        caseRetainedCutQueryInvalidation,
      testProperty
        "held marker-release coordinator passes retain cut queries"
        propMarkerReleaseRetainsCutQueries,
      testCase
        "warm and cold cut caches produce identical pending-anchor transitions"
        caseRetainedCutQueryTransitions,
      testCase
        "virgin debt selects the earliest route-matching cut and preserves pending prerequisites"
        caseVirginSelectionUsesEarliestMatchingCut,
      testCase
        "coalesced virgin causes preserve certified predecessor imports without duplicate work"
        caseVirginPromotionPreservesPredecessorImports,
      testCase
        "mixed control causes promote by ControlIndex rather than constructor order"
        caseMixedControlCauseOrder
    ]

caseCoveredRemovalOnlyDoesNotBlockReplay :: Assertion
caseCoveredRemovalOnlyDoesNotBlockReplay =
  assertBool
    "the first outstanding group is the announced later topology cut"
    ( alignmentReplayTargetIsNext
        successorTopologyId
        [ (topologyId, [AlignmentCauseCovered]),
          (successorTopologyId, [AlignmentCausePromotionDue])
        ]
    )

caseUnpromotedEarlierDebtBlocksReplay :: Assertion
caseUnpromotedEarlierDebtBlocksReplay =
  assertBool
    "a genuinely outstanding earlier group remains the sequencing barrier"
    ( not
        ( alignmentReplayTargetIsNext
            successorTopologyId
            [ (topologyId, [AlignmentCausePromotionDue]),
              (successorTopologyId, [AlignmentCausePromotionDue])
            ]
        )
    )

caseStepDuplicateCutAcceptanceFastPath :: Assertion
caseStepDuplicateCutAcceptanceFastPath = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, _, accepted, _, certificate) <-
    generationFastPathFixture local remote
  let withAcceptance =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain duplicate remote cut acceptance"
          (Alignment.prepareAlignmentCutAcceptance accepted alignment)
      predecessorAlignment =
        retainPendingControl
          remote
          (AlignmentHistoricalCertificateAdvertised certificate)
          withAcceptance
  (predecessor, binding) <- connectedWithAlignment predecessorAlignment
  assertTopLevelAlignmentFastPath
    "duplicate cut acceptance"
    binding
    (AlignmentCutAcceptanceAdvertised accepted)
    predecessor
    predecessorAlignment
    []

caseStepDuplicateCutAnnounceFastPath :: Assertion
caseStepDuplicateCutAnnounceFastPath = do
  let HeraldMember _ announcer = fixtureLocalMember
      HeraldMember _ receiver = fixtureRemoteMember
      retainedPrefix =
        HeraldPublicationPrefixThrough firstHeraldPublicationPosition
  (alignment, generation, _, _, certificate) <-
    generationFastPathFixtureAtLocalPrefix retainedPrefix receiver announcer
  let generationId = alignmentGenerationId generation
      cut = alignmentGenerationCut generation
      localAcceptance =
        alignmentCutAccepted
          generationId
          receiver
          (deriveTopologyCutId (alignmentCutTopologyCut cut))
          (alignmentCutPhysicalPlacementRevisionVector cut)
          retainedPrefix
      predecessorAlignment =
        retainPendingControl
          announcer
          (AlignmentHistoricalCertificateAdvertised certificate)
          alignment
      control = AlignmentCutAnnounced (alignmentGenerationCutAnnounce generation)
  (predecessor, binding) <-
    connectedWithAlignmentAt fixtureRemoteMember fixtureLocalMember predecessorAlignment
  -- This owner-focused fixture deliberately gives the retained immutable
  -- acceptance a different prefix from the otherwise-initial Publication
  -- owner. It isolates exact-evidence reuse; the real H4 acceptance test covers
  -- the reachable direction in which publication advances after acceptance.
  assertEqual
    "the synthetic fixture retained its distinct immutable prefix"
    retainedPrefix
    (alignmentCutAcceptedPublicationPrefix localAcceptance)
  case applyAlignmentGenerationControlWithDisposition binding control predecessor of
    Left problem ->
      assertFailure ("classify duplicate cut announcement: " <> show problem)
    Right Nothing ->
      assertFailure "classify duplicate cut announcement: control was not handled"
    Right (Just (classified, _, disposition)) -> do
      assertBool
        "the detailed duplicate cut announcement is state-idempotent"
        (classified == predecessor)
      assertEqual
        "the detailed duplicate cut announcement is explicitly unchanged"
        AlignmentGenerationEvidenceUnchanged
        disposition
  assertTopLevelAlignmentFastPath
    "synthetic immutable-evidence duplicate cut announcement"
    binding
    control
    predecessor
    predecessorAlignment
    []

caseStepDuplicateMemberReadyFastPath :: Assertion
caseStepDuplicateMemberReadyFastPath = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, _, _, ready, certificate) <-
    generationFastPathFixture local remote
  let predecessorAlignment =
        retainPendingControl
          remote
          (AlignmentHistoricalCertificateAdvertised certificate)
          alignment
  (predecessor, binding) <- connectedWithAlignment predecessorAlignment
  assertTopLevelAlignmentFastPath
    "duplicate member readiness"
    binding
    (AlignmentMemberReadyAdvertised ready)
    predecessor
    predecessorAlignment
    []

caseStepDuplicateCertificateFastPath :: Assertion
caseStepDuplicateCertificateFastPath = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, _, accepted, _, certificate) <-
    generationFastPathFixture local remote
  let withCertificate =
        commit
          Alignment.commitHistoricalCertificate
          "retain duplicate historical certificate"
          (Alignment.prepareHistoricalCertificate certificate alignment)
      predecessorAlignment =
        retainPendingControl
          remote
          (AlignmentCutAcceptanceAdvertised accepted)
          withCertificate
  (predecessor, binding) <- connectedWithAlignment predecessorAlignment
  assertTopLevelAlignmentFastPath
    "duplicate historical certificate"
    binding
    (AlignmentHistoricalCertificateAdvertised certificate)
    predecessor
    predecessorAlignment
    []

caseDeferredRejectionKeepsFreshReceipt :: Assertion
caseDeferredRejectionKeepsFreshReceipt = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, generation) <- readyAlignmentFixture local remote
  (connected, binding) <- connectedWithAlignment alignment
  let generationId = alignmentGenerationId generation
      ready =
        classMemberReady
          firstAlignmentEvidenceSequence
          generationId
          storeB
          (deriveMemberReadyEvidenceDigest "deferred-rejection-ready")
          initialStoreRevision
      invalidCertificate =
        historicalCertificate
          generationId
          storeB
          (deriveMemberReadyEvidenceDigest "not-the-complete-ready-set")
          (deriveBootstrapEvidenceDigest "deferred-invalid-prefix")
          initialStoreRevision
  (held, _, _, _) <-
    either
      (assertFailure . show)
      pure
      ( PeerControl.applyPeerControlWithInstalledCuts
          binding
          (PeerAlignmentControl (AlignmentHistoricalCertificateAdvertised invalidCertificate))
          connected
      )
  assertEqual "the incomplete certificate is dependency-held" 1 (length (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState held)))
  (successor, effects, _, disposition) <-
    either
      (assertFailure . show)
      pure
      ( PeerControl.applyPeerControlWithInstalledCuts
          binding
          (PeerAlignmentEvidenceDelivered DomainAlignment.firstAlignmentDeliverySequence (AlignmentMemberReadyAdvertised ready))
          held
      )
  assertEqual "the current readiness was admitted" PeerControl.PeerControlServingWorkRequired disposition
  assertEqual "new readiness remains retained" (Just ready) (Alignment.lookupAlignmentMemberReadiness generationId storeB (startupAlignmentState successor))
  assertEqual "the older invalid certificate is removed" [] (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState successor))
  assertBool "the deferred contradiction closes its source binding" (RejectPeerConnection binding ClosePeerControlProtocol `elem` effectBatchMembers effects)
  assertEqual
    "a deferred close does not suppress receipt of new valid evidence"
    (receiptRetirementPrefix (Just 1))
    (Delivery.receiptProgress (DeliveryOwner.peerState remote (startupPeerDeliveryState successor)).delivery)
  assertEqual "the semantic owner remains valid" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState successor))

caseConfirmedPendingEvidenceSurvivesReconnect :: Assertion
caseConfirmedPendingEvidenceSurvivesReconnect = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember remoteId remote = fixtureRemoteMember
  (withGeneration, generation) <- readyAlignmentFixture local remote
  (connected, binding) <- connectedWithAlignment Alignment.emptyState
  let generationId = alignmentGenerationId generation
      ready = classMemberReady firstAlignmentEvidenceSequence generationId storeB (deriveMemberReadyEvidenceDigest "confirmed-pending-readiness") initialStoreRevision
      control = AlignmentMemberReadyAdvertised ready
      deliveryControl = PeerAlignmentEvidenceDelivered DomainAlignment.firstAlignmentDeliverySequence control
      pendingControls state = map Alignment.pendingGenerationEvidenceControl (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState state))
      progress state = Delivery.receiptProgress (DeliveryOwner.peerState remote (startupPeerDeliveryState state)).delivery
  (held, _, _, _) <- either (assertFailure . show) pure (PeerControl.applyPeerControlWithInstalledCuts binding deliveryControl connected)
  assertEqual "the semantic owner retains the payload before its generation arrives" [control] (pendingControls held)
  assertEqual "retained dependencies suffice to confirm transport delivery" (receiptRetirementPrefix (Just 1)) (progress held)
  (sender, _) <- either (assertFailure . show) pure (Delivery.offer (1 :: Int) control Delivery.emptyState)
  confirmed <- either (assertFailure . show) pure (Delivery.acknowledge (progress held) sender)
  assertEqual "the sender can reclaim its payload while semantic admission is pending" [] (Delivery.outstanding confirmed)
  (disconnected, _, _) <- either (assertFailure . show) pure (PeerControl.applyPeerBindingLoss binding held)
  let genesis = startupGenesis disconnected
      nonce = connectionNonce 1_338
      candidate = peerCandidate remoteId remote nonce
      hello = peerHello (checkedSystemId genesis) remoteId remote nonce Set.empty (controlIndex 0) (checkedCatalogueDigest genesis) (checkedInitialProjectionDigest (startupInitialBootstraps disconnected)) Nothing
  (reconnected, helloEffects) <- checkedStep "reconnect while retained evidence awaits its generation" (heraldInput (monotonicInstant 2) (PeerInput (currentPeerHelloReceived disconnected candidate Set.empty hello))) disconnected
  newBinding <- case [accepted | SetPeerCandidateDisposition _ (PeerHelloAccepted _ accepted) <- effectBatchMembers helloEffects] of
    [accepted] -> pure accepted
    observed -> assertFailure ("expected replacement binding: " <> show observed)
  assertBool "the physical association is replaced" (newBinding /= binding)
  assertEqual "dependency-held payload survives physical reconnect" [control] (pendingControls reconnected)
  assertEqual "receive progress also survives physical reconnect" (progress held) (progress reconnected)
  (replayed, replayEffects, _, disposition) <- either (assertFailure . show) pure (PeerControl.applyPeerControlWithInstalledCuts newBinding deliveryControl reconnected)
  assertEqual "replayed confirmed evidence is a semantic no-op" PeerControl.PeerControlGenerationEvidenceUnchanged disposition
  assertEqual "duplicate delivery does not produce a semantic reply" [] (effectBatchMembers replayEffects)
  assertEqual "duplicate delivery preserves the dependency-held payload" [control] (pendingControls replayed)
  -- Install the checked generation fixture with the exact retained input. This
  -- isolates dependency release from topology-plan discovery, while preserving
  -- the peer receipt and the pending semantic owner fact across that boundary.
  let dependencies = replaceStartupAlignmentState (retainPendingControl remote control withGeneration) replayed
  (admitted, _) <- either (assertFailure . show) pure (advanceAlignmentGenerations dependencies)
  assertEqual "dependency arrival consumes the pending semantic entry" [] (pendingControls admitted)
  assertEqual "the already-confirmed payload becomes retained readiness" (Just ready) (Alignment.lookupAlignmentMemberReadiness generationId storeB (startupAlignmentState admitted))
  assertEqual "dependency release needs no sender replay or new receipt" (progress held) (progress admitted)
  assertEqual "semantic admission preserves the full alignment invariant" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState admitted))

caseRejectedEvidenceKeepsValidProgress :: Assertion
caseRejectedEvidenceKeepsValidProgress = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, generation, _, _, _) <- generationFastPathFixture local remote
  (connected, binding) <- connectedWithAlignment alignment
  let accepted = checkedMaybe "retained local acceptance" (Alignment.lookupAlignmentCutAcceptance (alignmentGenerationId generation) local alignment)
      control = AlignmentCutAcceptanceAdvertised accepted
  (offered, _) <-
    either
      (assertFailure . show)
      pure
      (DeliveryCoordinator.normalizeEffects connected (singletonEffectBatch (QueuePeerAlignmentEvidence remote control)))
  assertEqual "the outbound evidence is initially unconfirmed" 1 (length (Delivery.outstanding (DeliveryOwner.peerState remote (startupPeerDeliveryState offered)).delivery))
  (successor, effects) <-
    checkedStep
      "independent receipt with rejected current evidence"
      ( heraldInput
          (monotonicInstant 2)
          ( PeerInput
              ( PeerControlReceivedWithProgress
                  binding
                  (receiptRetirementPrefix (Just 1))
                  (PeerAlignmentEvidenceDelivered DomainAlignment.firstAlignmentDeliverySequence control)
              )
          )
      )
      offered
  let delivery = (DeliveryOwner.peerState remote (startupPeerDeliveryState successor)).delivery
  assertEqual "valid progress retires the outbound evidence" [] (Delivery.outstanding delivery)
  assertEqual "a peer cannot deliver another Herald's acceptance" [RejectPeerConnection binding ClosePeerControlProtocol] (effectBatchMembers effects)
  assertEqual "rejected incoming evidence is never confirmed" emptyReceiptRetirement (Delivery.receiptProgress delivery)
  assertEqual "semantic rejection preserves the admitted alignment owner" alignment (startupAlignmentState successor)

assertTopLevelAlignmentFastPath ::
  String ->
  PeerBinding ->
  AlignmentControl ->
  HeraldState ->
  Alignment.State ->
  [HeraldEffect] ->
  Assertion
assertTopLevelAlignmentFastPath context binding control predecessor expectedAlignment expectedEffects = do
  (successor, effects) <-
    checkedStep
      context
      ( heraldInput
          (monotonicInstant 2)
          ( PeerInput
              (PeerControlReceived binding (PeerAlignmentControl control))
          )
      )
      predecessor
  assertBool
    (context <> ": only observed time and the exact alignment owner change")
    ( ( replaceStartupLastObservedTime (monotonicInstant 2)
          . replaceStartupAlignmentState expectedAlignment
          $ predecessor
      )
        == successor
    )
  assertEqual
    (context <> ": unrelated admissible pending evidence remains undriven")
    (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState predecessor))
    (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState successor))
  assertEqual
    (context <> ": exact direct effects")
    expectedEffects
    (effectBatchMembers effects)

generationFastPathFixture ::
  HeraldEpoch ->
  HeraldEpoch ->
  IO
    ( Alignment.State,
      AlignmentGeneration,
      AlignmentCutAccepted,
      ClassMemberReady,
      HistoricalCertificate
    )
generationFastPathFixture =
  generationFastPathFixtureAtLocalPrefix EmptyHeraldPublicationPrefix

generationFastPathFixtureAtLocalPrefix ::
  HeraldPublicationPrefix ->
  HeraldEpoch ->
  HeraldEpoch ->
  IO
    ( Alignment.State,
      AlignmentGeneration,
      AlignmentCutAccepted,
      ClassMemberReady,
      HistoricalCertificate
    )
generationFastPathFixtureAtLocalPrefix localPrefix local remote = do
  (withLocalReady, generation) <- readyAlignmentFixture local remote
  let generationId = alignmentGenerationId generation
      cut = alignmentGenerationCut generation
      acceptance herald prefix =
        alignmentCutAccepted
          generationId
          herald
          (deriveTopologyCutId (alignmentCutTopologyCut cut))
          (alignmentCutPhysicalPlacementRevisionVector cut)
          prefix
      localAcceptance = acceptance local localPrefix
      remoteAcceptance = acceptance remote EmptyHeraldPublicationPrefix
      withLocalAcceptance =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain local fast-path cut acceptance"
          ( Alignment.prepareAlignmentCutAcceptance
              localAcceptance
              withLocalReady
          )
      remoteReady =
        classMemberReady
          (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence)
          generationId
          storeB
          (deriveMemberReadyEvidenceDigest "fast-path-remote-ready")
          initialStoreRevision
      withReadiness =
        commit
          Alignment.commitClassMemberReady
          "retain remote fast-path readiness"
          (Alignment.prepareClassMemberReady remoteReady withLocalAcceptance)
      localReady =
        checkedMaybe
          "lookup local fast-path readiness"
          (Alignment.lookupAlignmentMemberReadiness generationId storeA withReadiness)
      certificate =
        historicalCertificate
          generationId
          storeB
          (classMemberReadySetDigest [localReady, remoteReady])
          ( deriveBootstrapEvidenceDigest
              ( memberReadyEvidenceDigestBytes
                  (classMemberReadyPredecessorAndBasePrefixDigest remoteReady)
              )
          )
          initialStoreRevision
  pure
    ( withReadiness,
      generation,
      remoteAcceptance,
      remoteReady,
      certificate
    )

retainPendingControl ::
  HeraldEpoch ->
  AlignmentControl ->
  Alignment.State ->
  Alignment.State
retainPendingControl remote control state =
  commit
    Alignment.commitPendingGenerationEvidence
    "retain unrelated pending fast-path evidence"
    (Alignment.preparePendingGenerationEvidence remote control state)

connectedWithAlignment :: Alignment.State -> IO (HeraldState, PeerBinding)
connectedWithAlignment =
  connectedWithAlignmentAt fixtureLocalMember fixtureRemoteMember

connectedWithAlignmentAt ::
  HeraldMember ->
  HeraldMember ->
  Alignment.State ->
  IO (HeraldState, PeerBinding)
connectedWithAlignmentAt localMember remoteMember alignment = do
  let HeraldMember remoteId remote = remoteMember
      nonce = connectionNonce 1_337
      candidate = peerCandidate remoteId remote nonce
      hello =
        peerHello
          (checkedSystemId genesis)
          remoteId
          remote
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest genesis)
          (checkedInitialProjectionDigest initialBootstraps)
          Nothing
      initial =
        fst
          ( checked
              "initialize top-level alignment fast-path receiver"
              ( initialHerald
                  (monotonicInstant 0)
                  genesis
                  initialBootstraps
                  fixtureOracleContacts
                  fixtureGeneratorSeed
                  fixtureApplicationRecoveryConfiguration
                  fixturePeerRecoveryConfiguration
              )
          )
      genesis =
        checked
          "check top-level alignment fast-path receiver genesis"
          (checkHeraldGenesis (fixtureDeploymentAt localMember))
      initialBootstraps =
        checked
          "check top-level alignment fast-path receiver bootstraps"
          ( checkInitialBootstraps
              genesis
              ( PrimordialProcessManifest
                  (take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId])
              )
          )
  (connected, effects) <-
    checkedStep
      "connect remote for top-level alignment fast path"
      ( heraldInput
          (monotonicInstant 1)
          (PeerInput (currentPeerHelloReceived initial candidate Set.empty hello))
      )
      initial
  binding <- case [ accepted
                  | SetPeerCandidateDisposition _ (PeerHelloAccepted _ accepted) <-
                      effectBatchMembers effects
                  ] of
    [accepted] -> pure accepted
    observed ->
      assertFailure
        ("expected one fast-path peer binding, got " <> show observed)
  pure (replaceStartupAlignmentState alignment connected, binding)

checkedStep :: String -> HeraldInput -> HeraldState -> IO (HeraldState, EffectBatch)
checkedStep context input predecessor =
  case stepHerald input predecessor of
    Left fault -> assertFailure (context <> ": " <> show fault)
    Right result -> pure result

checkedMaybe :: String -> Maybe value -> value
checkedMaybe context = maybe (error context) id

caseGenerationWorkDrainsThenQuiesces :: Assertion
caseGenerationWorkDrainsThenQuiesces = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (withLocalReady, generation) <- readyAlignmentFixture local remote
  let generationId = alignmentGenerationId generation
      remoteReady =
        classMemberReady
          (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence)
          generationId
          storeB
          (deriveMemberReadyEvidenceDigest "pending-remote-ready")
          initialStoreRevision
      pendingControl = AlignmentMemberReadyAdvertised remoteReady
      preparedPending =
        checked
          "retain pending remote readiness"
          ( Alignment.preparePendingGenerationEvidence
              remote
              pendingControl
              withLocalReady
          )
      (pendingAlignment, pendingDisposition) =
        Alignment.commitPendingGenerationEvidence preparedPending
      predecessor = initializedWithAlignment pendingAlignment
  assertEqual
    "the fixture adds exact pending work"
    Alignment.PendingGenerationEvidenceRetained
    pendingDisposition

  (successor, _) <- advance "drain pending generation evidence" predecessor
  assertBool
    "draining admitted evidence changes the generation state"
    (startupAlignmentState successor /= pendingAlignment)
  assertEqual
    "the admitted pending entry is removed"
    []
    (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState successor))
  assertEqual
    "the remote readiness is retained before the fixed point quiesces"
    (Just remoteReady)
    ( Alignment.lookupAlignmentMemberReadiness
        generationId
        storeB
        (startupAlignmentState successor)
    )

  (quiescent, quiescentEffects) <-
    advance "re-run quiescent generation work" successor
  assertBool
    "a quiescent generation pass is state-idempotent"
    (quiescent == successor)
  assertEqual
    "a quiescent generation pass emits no effects"
    []
    (effectBatchMembers quiescentEffects)
  assertControlCatalogueDelta local remote pendingAlignment (startupAlignmentState successor)
  assertControlCatalogueDelta local remote (startupAlignmentState successor) (startupAlignmentState quiescent)
  where
    advance context state =
      case advanceAlignmentGenerations state of
        Left problem -> assertFailure (context <> ": " <> show problem)
        Right result -> pure result

-- This stage fixture uses a real predefined regular Store and its checked
-- primordial snapshot. The transfer owner is admitted directly so this test
-- isolates continuing-prefix scheduling from generation-planning stages.
propSelectiveSourcePrefixReference :: Property
propSelectiveSourcePrefixReference =
  forAll (chooseInt (1, 5)) $ \rounds -> ioProperty $ do
    (connected, _) <- connectedWithAlignment Alignment.emptyState
    let HeraldMember _ local = fixtureLocalMember
        HeraldMember _ remote = fixtureRemoteMember
        slot = checkedMaybe "source-prefix regular system Store" (find (\candidate -> Store.storeSlotProvenance candidate == Store.HeraldSystemView local SortProfile.SortDefinitionRole) (Store.storeSlots (startupStoreState connected)))
        incarnation = Store.storeSlotIncarnation slot
        generation = checked "source-prefix fixture generation" (mkContextClassGenerationId (fixtureIdentifierBytes 0xd8))
        snapshot = checked "source-prefix actual snapshot" (Store.retainedStoreSnapshotAt incarnation (Store.storeSlotRevision slot) (startupStoreState connected))
        facts = fmap snapshotFact (Store.retainedStoreSnapshotFacts snapshot)
        identifiers = take 2 (fmap (AlignmentProtocol.alignmentSubscriptionId remote) (iterate AlignmentProtocol.nextAlignmentSubscriptionSequence AlignmentProtocol.firstAlignmentSubscriptionSequence))
        retainSource transfer identifier =
          let obligation =
                checked
                  "source-prefix checked obligation"
                  (AlignmentProtocol.alignmentObligation (AlignmentProtocol.alignmentObligationId remote AlignmentProtocol.firstAlignmentObligationSequence) (cause successorOccurrence) (Store.storeSlotSortId slot) (Store.storeSlotOccurrenceId slot) generation (destinationStore deltaA storeA :| []) generation Route.Normal)
              subscribe = checked "source-prefix checked subscribe" (AlignmentProtocol.alignmentSubscribe obligation identifier incarnation (AlignmentProtocol.BootstrapProof generation (deriveBootstrapEvidenceDigest "source-prefix fixture")))
           in fst (Transfer.commitSourceSubscription (checked "source-prefix source admission" (Transfer.prepareSourceSubscription subscribe (Store.storeSlotRevision slot) facts Transfer.SourceLiveImmediate transfer)))
        owner = startupAlignmentState connected
        sources = foldl retainSource (Alignment.alignmentTransferState owner) identifiers
        initial = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState sources owner) connected
        raw = Store.retainedStoreObservationPublication (Store.retainedStoreSnapshotFactRepresentative (checkedMaybe "source-prefix nonempty primordial snapshot" (listToMaybe (Store.retainedStoreSnapshotFacts snapshot))))
        entry = checkedMaybe "source-prefix descriptor" (SortRegistry.lookupEffectiveSort (Store.storeSlotSortId slot) (startupSortRegistryState connected))
        observe ordinal state =
          let oldId = DomainPublication.checkedPublicationId raw
              identifier = Identity.publicationId (Identity.publicationNabla oldId) (Identity.publicationAuthorityEpoch oldId) local (Identity.nablaSequence (10000 + fromIntegral ordinal))
              publication = checked "source-prefix fresh raw publication" (DomainPublication.mkCheckedPublication (SortRegistry.registryEntryDescriptor entry) identifier (DomainPublication.checkedPublicationValue raw))
              observation = checked "source-prefix checked observation" (Store.retainedStoreObservation publication Route.Normal (Store.storeSlotOccurrenceId slot) Store.PrimordialStoreObservation)
              prepared = checked "source-prefix Store commit" (Store.prepareAlignmentStoreApplication (Store.peerStoreDestination (Store.storeSlotDelta slot) incarnation Route.Normal :| []) observation (startupStoreState state))
           in replaceStartupStoreState (fst (Store.commitPeerStoreApplication prepared)) state
    assertBool "fixture snapshot contains real primordial evidence" (not (null facts))
    (settled, _, _) <- assertSourcePrefixReference "initial admission" initial
    assertBool "notification consumption remains visible in ordinary equality" (settled /= initial)
    assertBool "caught-up admission changes no semantic work state" (sameAlignmentTransferWorkState settled initial)
    final <-
      foldM
        ( \state ordinal -> do
            let updated = observe ordinal state
            assertEqual "changed raw Store names its exact incarnation" (Set.singleton incarnation) (Store.changedSourceStoreIncarnations (startupStoreState updated))
            (advanced, effects, disposition) <- assertSourcePrefixReference "new raw Store revision" updated
            assertEqual "raw changes advance semantic work" AlignmentTransferWorkChanged disposition
            assertEqual
              "each subscriber emits its change before Live in source order"
              (concatMap (\identifier -> [Left identifier, Right identifier]) identifiers)
              [ case control of
                  AlignmentChangeTransferred change -> Left (AlignmentProtocol.alignmentChangeSubscriptionId change)
                  AlignmentLiveAdvertised live -> Right (AlignmentProtocol.alignmentLiveSubscriptionId live)
                  _ -> error "source-prefix fixture emitted an unrelated control"
              | SendPeerControl _ (PeerAlignmentControl control) <- effectBatchMembers effects
              ]
            let transfer = Alignment.alignmentTransferState (startupAlignmentState advanced)
                currentSlot = checkedMaybe "source-prefix retained current Store" (Store.lookupRetainedStoreSlot incarnation (startupStoreState advanced))
            assertEqual "completed source prefixes do not self-redirty" Set.empty (Transfer.pendingSourcePrefixIds transfer)
            forM_ identifiers $ \identifier ->
              assertEqual "the complete current prefix has been sent" (Just (Store.storeSlotRevision currentSlot)) (Transfer.sourceSubscriptionSentThrough <$> Transfer.lookupSourceSubscription identifier transfer)
            (quiescent, quietEffects, quietWork) <- assertSourcePrefixReference "clean follow-up" advanced
            assertBool "clean follow-up preserves full owner metadata" (quiescent == advanced)
            assertEqual "clean follow-up emits no effects" [] (effectBatchMembers quietEffects)
            assertEqual "clean follow-up reports no semantic work" AlignmentTransferWorkUnchanged quietWork
            (replayed, duplicateEffects, duplicateWork) <- assertSourcePrefixReference "duplicate raw publication" (observe ordinal quiescent)
            assertEqual "duplicate raw publication emits no source effects" [] (effectBatchMembers duplicateEffects)
            assertEqual "duplicate raw publication does not advance sources" AlignmentTransferWorkUnchanged duplicateWork
            pure replayed
        )
        settled
        [1 .. rounds]
    assertEqual "source scheduling preserves checked transfer state" (Right ()) (Transfer.validateAlignmentTransferState (Alignment.alignmentTransferState (startupAlignmentState final)))
    pure True
  where
    snapshotFact fact =
      checked
        "source-prefix exact snapshot fact"
        (AlignmentProtocol.retainedStateEvidence (evidence (Store.retainedStoreSnapshotFactRepresentative fact)) (evidence (Store.retainedStoreSnapshotFactStrengthWitness fact)) (Store.retainedStoreSnapshotFactStrength fact))
    evidence observation =
      let publication = Store.retainedStoreObservationPublication observation
       in AlignmentProtocol.primordialRetainedPublicationEvidence (DomainPublication.checkedPublicationId publication) (DomainPublication.checkedPublicationSort publication) (Store.retainedStoreObservationSortOccurrence observation) (DomainPublication.checkedPublicationCanonicalValue publication) (Store.retainedStoreObservationIncomingStrength observation)

-- Exercise the exact canonical-genesis origin branch through the real transfer
-- coordinator. Both a normal source crossing a Weaken path and an already weak
-- source must remain Weak when its description is read by another process.
caseWeakGenesisAlignment :: Assertion
caseWeakGenesisAlignment = forM_ [Route.Normal, Route.Weak] $ \sourceStrength -> do
  (connected, binding) <- connectedWithAlignment Alignment.emptyState
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
      bootstraps = OracleProjection.oracleViewAppliedBootstraps (OracleProjection.oracleView (startupOracleProjectionState connected))
      bootstrapAt residence = checkedMaybe "genesis alignment bootstrap" (find ((== residence) . Startup.appliedProcessResidence) bootstraps)
      receiver = bootstrapAt local
      source = bootstrapAt remote
      process = Startup.appliedProcessEpochId receiver
      object = Startup.appliedProcessEnvironmentHub source
      reader bootstrap =
        checkedMaybe "genesis neutral-vertex reader"
          $ listToMaybe
            [ (root, delta, incarnation)
            | root <- Startup.appliedProcessRoots bootstrap,
              Startup.appliedRootCatalogueRole root == SortProfile.NeutralVertexRole,
              Startup.ReaderRoot delta <- [Startup.appliedRootRole root],
              Just (_, incarnation) <- [Startup.appliedRootPlacement root]
            ]
      (targetRoot, targetDelta, targetStore) = reader receiver
      (_, _, sourceStore) = reader source
      sort = Startup.appliedRootSortId targetRoot
      carrierOccurrence = Startup.appliedRootOccurrenceId targetRoot
      publication =
        checkedMaybe "canonical remote hub publication"
          $ listToMaybe
            [ value
            | (_, _, value) <- Startup.appliedProcessEnvironmentPublications (checkedSystemId (startupGenesis connected)) bootstraps source,
              DomainPublication.checkedPublicationSort value == sort
            ]
      evidence = AlignmentProtocol.primordialRetainedPublicationEvidence (DomainPublication.checkedPublicationId publication) sort carrierOccurrence (DomainPublication.checkedPublicationCanonicalValue publication) sourceStrength
      fact = checked "canonical genesis snapshot fact" (AlignmentProtocol.retainedStateEvidence evidence evidence sourceStrength)
      sourceGeneration = checked "genesis source generation" (mkContextClassGenerationId (fixtureIdentifierBytes 0xd8))
      destinationGeneration = checked "genesis destination generation" (mkContextClassGenerationId (fixtureIdentifierBytes 0xd9))
      obligation = checked "weakened genesis obligation" (AlignmentProtocol.alignmentObligation (AlignmentProtocol.alignmentObligationId local AlignmentProtocol.firstAlignmentObligationSequence) (cause successorOccurrence) sort carrierOccurrence destinationGeneration (destinationStore targetDelta targetStore :| []) sourceGeneration Route.Weak)
      identifier = AlignmentProtocol.alignmentSubscriptionId local AlignmentProtocol.firstAlignmentSubscriptionSequence
      certificate = historicalCertificate sourceGeneration sourceStore (deriveMemberReadyEvidenceDigest "weak-genesis-source") (deriveBootstrapEvidenceDigest "weak-genesis-source") initialStoreRevision
      attempt = checked "remote genesis destination attempt" (AlignmentProtocol.alignmentAttempt obligation identifier remote certificate)
      preparedDestination = checked "genesis destination admission" (Transfer.prepareDestinationAttempt attempt Transfer.emptyState)
      destination = fst (Transfer.commitDestinationAttempt preparedDestination)
      preparedSource = checked "canonical genesis source snapshot" (Transfer.prepareSourceSubscription (Transfer.preparedDestinationAttemptSubscribe preparedDestination) initialStoreRevision [fact] Transfer.SourceLiveImmediate Transfer.emptyState)
      installed = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState destination (startupAlignmentState connected)) connected
      applyControl state control = fst <$> either (assertFailure . show) pure (applyAlignmentTransferControl binding control state)
      slot state = checkedMaybe "genesis destination Store" (Store.lookupStoreSlot targetDelta (startupStoreState state))
      identifierOf = DomainPublication.checkedPublicationId publication
  assertEqual "the receiver starts without this remote description" Nothing (lookup identifierOf (Store.storeSlotApplicationReceipts (slot installed)))
  assertEqual "the receiver starts without possession of the remote hub" Nothing (Controlled.controlledEffectivePossessionStrength process object (startupControlledState installed))
  received <- foldM applyControl installed (Transfer.preparedSourceSubscriptionControls preparedSource)
  assertEqual "the exact genesis receipt remains Weak" (Just Route.Weak) (lookup identifierOf (Store.storeSlotApplicationReceipts (slot received)))
  assertEqual "the retained logical state remains Weak" (Just Route.Weak) (DomainStore.logicalStateStrength (DomainPublication.checkedPublicationLogicalState publication) (Store.storeSlotContents (slot received)))
  assertEqual "alignment alone grants no application possession" Nothing (Controlled.controlledEffectivePossessionStrength process object (startupControlledState received))
  let descriptor = SortRegistry.registryEntryDescriptor (checkedMaybe "genesis carrier descriptor" (SortRegistry.lookupEffectiveSort sort (startupSortRegistryState received)))
      visible = checkedMaybe "aligned remote hub is visible" (find ((== identifierOf) . DomainPublication.checkedPublicationId . DomainStore.storedPublication . snd) (DomainStore.visibleInstances (Store.storeSlotContents (slot received))))
      observed = checked "aligned genesis controlled observation" (Controlled.checkControlledObservation descriptor carrierOccurrence (DomainStore.storedPublication (snd visible)))
      readOwner = Controlled.commitControlledStoreObservation (checked "read weakened genesis description" (Controlled.prepareControlledStoreRead process targetDelta (DomainStore.storedStrength (snd visible)) observed (startupControlledState received)))
  assertEqual "reading the description grants only Weak possession" (Just Route.Weak) (Controlled.controlledEffectivePossessionStrength process object readOwner)
  assertBool "the read cannot confer normal label authority" (not (Controlled.controlledHasNormalPossession process object readOwner))
  assertEqual "weakened genesis Store history remains valid" (Right ()) (Store.validateStoreHistoryState (startupStoreState received))
  assertEqual "weakened genesis transfer history remains valid" (Right ()) (Transfer.validateAlignmentTransferState (Alignment.alignmentTransferState (startupAlignmentState received)))

-- Checked local source/destination transcripts carry actual application-authored
-- routed evidence through runLoopback. Three separately retained Store slots
-- make the middle slot both a destination and another source's dependency.
caseSourcePrefixLoopback :: Bool -> Assertion
caseSourcePrefixLoopback forwardOrder = do
  let (seed, _, _, _, suppliedControls) = liveRegularAlignmentGateFixture False
      HeraldMember localId local = fixtureLocalMember
      retained =
        checkedMaybe
          "loopback routed change"
          (listToMaybe [AlignmentProtocol.alignmentChangeRetainedTransition change | AlignmentChangeTransferred change <- suppliedControls])
      prototype =
        checkedMaybe
          "loopback admitted destination prototype"
          (listToMaybe (Transfer.destinationSubscriptionEntries (Alignment.alignmentTransferState (startupAlignmentState seed))))
      envelope = AlignmentProtocol.alignmentSubscribeObligation (Transfer.destinationSubscriptionSubscribe (snd prototype))
      loopbackSortOccurrence = AlignmentProtocol.alignmentObligationSortDefinitionOccurrenceId envelope
      sort = AlignmentProtocol.alignmentObligationSortId envelope
      coordinates =
        [ (checked "loopback Store delta" (mkDeltaId (fixtureIdentifierBytes tag)), checked "loopback Store incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes tag)))
        | tag <- [0xd1, 0xd2, 0xd3]
        ]
      firstCoordinate = checkedMaybe "first loopback Store" (listToMaybe coordinates)
      middleCoordinate = checkedMaybe "middle loopback Store" (listToMaybe (drop 1 coordinates))
      lastCoordinate = checkedMaybe "last loopback Store" (listToMaybe (drop 2 coordinates))
      store =
        Store.commitStoreBootstrap
          (checked "install actual loopback Stores" (Store.prepareStoreBootstrap [Store.localStoreSpec (Store.HeraldSystemView local SortProfile.SortDefinitionRole) delta sort loopbackSortOccurrence incarnation | (delta, incarnation) <- coordinates] (startupStoreState seed)))
      withStores = replaceStartupStoreState store seed
      firstSequence = AlignmentProtocol.firstAlignmentSubscriptionSequence
      secondSequence = AlignmentProtocol.nextAlignmentSubscriptionSequence firstSequence
      upstream = AlignmentProtocol.alignmentSubscriptionId local (if forwardOrder then firstSequence else secondSequence)
      downstream = AlignmentProtocol.alignmentSubscriptionId local (if forwardOrder then secondSequence else firstSequence)
      localBinding = Discovery.PeerBinding localId local Discovery.firstPeerBindingGeneration (Discovery.PeerCandidate localId local (Discovery.ConnectionNonce 2001))
      sourceSnapshot incarnation =
        let snapshot = checked "loopback actual initial snapshot" (Store.retainedStoreSnapshotAt incarnation initialStoreRevision store)
         in map snapshotFact (Store.retainedStoreSnapshotFacts snapshot)
      admit (priorTransfer, priorControls) (identifier, (_, sourceStore), (targetDelta, targetStore)) =
        let obligation =
              checked
                "loopback exact obligation"
                (AlignmentProtocol.alignmentObligation (AlignmentProtocol.alignmentObligationId local (if identifier == upstream then AlignmentProtocol.firstAlignmentObligationSequence else AlignmentProtocol.nextAlignmentObligationSequence AlignmentProtocol.firstAlignmentObligationSequence)) (alignmentObligationCause envelope) sort loopbackSortOccurrence (AlignmentProtocol.alignmentObligationDestinationGeneration envelope) (destinationStore targetDelta targetStore :| []) (AlignmentProtocol.alignmentObligationSourceGeneration envelope) Route.Normal)
            certificate = historicalCertificate (AlignmentProtocol.alignmentObligationSourceGeneration envelope) sourceStore (deriveMemberReadyEvidenceDigest "local-loopback-source") (deriveBootstrapEvidenceDigest "local-loopback-source") initialStoreRevision
            attempt = checked "loopback local destination attempt" (AlignmentProtocol.alignmentAttempt obligation identifier local certificate)
            preparedDestination = checked "loopback destination admission" (Transfer.prepareDestinationAttempt attempt priorTransfer)
            withDestination = fst (Transfer.commitDestinationAttempt preparedDestination)
            preparedSource = checked "loopback source admission" (Transfer.prepareSourceSubscription (Transfer.preparedDestinationAttemptSubscribe preparedDestination) initialStoreRevision (sourceSnapshot sourceStore) Transfer.SourceLiveImmediate withDestination)
         in (fst (Transfer.commitSourceSubscription preparedSource), priorControls <> Transfer.preparedSourceSubscriptionControls preparedSource)
      (owner, initialControls) = foldl admit (Transfer.emptyState, []) [(upstream, firstCoordinate, middleCoordinate), (downstream, middleCoordinate, lastCoordinate)]
      installed = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState owner (startupAlignmentState withStores)) withStores
      applyInitial current control = do
        (next, effects) <- either (assertFailure . show) pure (applyAlignmentTransferControl localBinding control current)
        foldM applyInitial next [reply | SendPeerControl _ (PeerAlignmentControl reply) <- effectBatchMembers effects]
  primed <- foldM applyInitial installed initialControls
  (clean, _, _) <- assertSourcePrefixReference "local source clean baselines" primed
  let entry = checkedMaybe "loopback effective descriptor" (SortRegistry.lookupEffectiveSort sort (startupSortRegistryState clean))
      value = checked "loopback canonical actual publication" (Value.decodeCanonicalValue (Value.canonicalValueByteString (AlignmentProtocol.retainedPublicationEvidenceCanonicalValue retained)))
      publication = checked "loopback actual checked publication" (DomainPublication.mkCheckedPublication (SortRegistry.registryEntryDescriptor entry) (AlignmentProtocol.retainedPublicationEvidencePublicationId retained) value)
      origin = case AlignmentProtocol.retainedPublicationEvidenceOrigin retained of
        AlignmentProtocol.RoutedRetainedObservation source sourceTopology prerequisite stamp -> Store.RoutedStoreObservation (Store.storeObservationEnvelope source loopbackSortOccurrence sourceTopology prerequisite stamp)
        _ -> error "loopback fixture requires admitted routed evidence"
      observation = checked "loopback actual Store observation" (Store.retainedStoreObservation publication Route.Normal loopbackSortOccurrence origin)
      (firstDelta, firstStore) = firstCoordinate
      changedStore = fst (Store.commitPeerStoreApplication (checked "append actual routed source evidence" (Store.prepareAlignmentStoreApplication (Store.peerStoreDestination firstDelta firstStore Route.Normal :| []) observation (startupStoreState clean))))
      changed = replaceStartupStoreState changedStore clean
      transferOf = Alignment.alignmentTransferState . startupAlignmentState
      revisionOf coordinate state = Store.storeSlotRevision (checkedMaybe "loopback retained Store" (Store.lookupRetainedStoreSlot (snd coordinate) (startupStoreState state)))
      sourceThrough identifier state = Transfer.sourceSubscriptionSentThrough (checkedMaybe "loopback retained source" (Transfer.lookupSourceSubscription identifier (transferOf state)))
  assertEqual "only the upstream Store changed before the pass" (Set.singleton firstStore) (Store.changedSourceStoreIncarnations changedStore)
  assertEqual "downstream source starts clean" Set.empty (Transfer.pendingSourcePrefixIds (transferOf changed))
  (firstPass, firstEffects, firstWork) <- assertSourcePrefixReference "local delivery first pass" changed
  assertEqual "the first source really delivered through local Store application" (DomainAlignment.nextStoreRevision initialStoreRevision) (revisionOf middleCoordinate firstPass)
  assertEqual "local delivery emits no fabricated peer traffic" [] (effectBatchMembers firstEffects)
  assertEqual "local delivery reports semantic progress" AlignmentTransferWorkChanged firstWork
  if forwardOrder
    then do
      assertEqual "later existing source advances during the same pass" (revisionOf middleCoordinate firstPass) (sourceThrough downstream firstPass)
      assertEqual "the same pass reaches the final Store" (revisionOf middleCoordinate firstPass) (revisionOf lastCoordinate firstPass)
      assertEqual "all source wakeups were consumed" Set.empty (Transfer.pendingSourcePrefixIds (transferOf firstPass))
    else do
      assertEqual "earlier source has not crossed its frozen cursor" initialStoreRevision (sourceThrough downstream firstPass)
      assertEqual "earlier source remains queued for the next pass" (Set.singleton downstream) (Transfer.pendingSourcePrefixIds (transferOf firstPass))
      assertEqual "the final Store waits for that next pass" initialStoreRevision (revisionOf lastCoordinate firstPass)
  (secondPass, secondEffects, secondWork) <- assertSourcePrefixReference "local delivery second pass" firstPass
  assertEqual "the next pass reaches the complete middle prefix" (revisionOf middleCoordinate secondPass) (sourceThrough downstream secondPass)
  assertEqual "the next pass reaches the final Store" (revisionOf middleCoordinate secondPass) (revisionOf lastCoordinate secondPass)
  assertEqual "the next pass consumes all source work" Set.empty (Transfer.pendingSourcePrefixIds (transferOf secondPass))
  assertEqual "the next pass preserves local delivery" [] (effectBatchMembers secondEffects)
  assertEqual "only a deferred earlier source needs another semantic pass" (if forwardOrder then AlignmentTransferWorkUnchanged else AlignmentTransferWorkChanged) secondWork
  assertEqual "real loopback owners remain valid" (Right ()) (Transfer.validateAlignmentTransferState (transferOf secondPass))
  assertEqual "real loopback Store history remains valid" (Right ()) (Store.validateStoreHistoryState (startupStoreState secondPass))
  where
    snapshotFact fact =
      checked
        "loopback primordial snapshot fact"
        (AlignmentProtocol.retainedStateEvidence (evidence (Store.retainedStoreSnapshotFactRepresentative fact)) (evidence (Store.retainedStoreSnapshotFactStrengthWitness fact)) (Store.retainedStoreSnapshotFactStrength fact))
    evidence observation =
      let publication = Store.retainedStoreObservationPublication observation
       in AlignmentProtocol.primordialRetainedPublicationEvidence (DomainPublication.checkedPublicationId publication) (DomainPublication.checkedPublicationSort publication) (Store.retainedStoreObservationSortOccurrence observation) (DomainPublication.checkedPublicationCanonicalValue publication) (Store.retainedStoreObservationIncomingStrength observation)

assertSourcePrefixReference :: String -> HeraldState -> IO (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
assertSourcePrefixReference context predecessor = do
  sparse@(successor, effects, disposition) <- either (assertFailure . ((context <> ": ") <>) . show) pure (advanceSourcePrefixes predecessor)
  (reference, referenceEffects, referenceDisposition) <- either (assertFailure . ((context <> " full scan: ") <>) . show) pure (advanceSourcePrefixesExhaustive predecessor)
  assertBool (context <> " preserves full-scan owner state including work metadata") (successor == reference)
  assertEqual (context <> " preserves exact ordered effects") (effectBatchMembers referenceEffects) (effectBatchMembers effects)
  assertEqual (context <> " preserves semantic work disposition") referenceDisposition disposition
  pure sparse

caseAlignmentTransferPassQuiesces :: Assertion
caseAlignmentTransferPassQuiesces = do
  -- Exercise real Startup initialization; installing a raw leaf owner here
  -- would deliberately replace its initial context, local identity and queues.
  let predecessor = initializedCoordinatorHerald
  (successor, effects, disposition) <-
    checkedAlignmentPass "empty alignment pass" predecessor
  assertEqual
    "the pass reports exact semantic quiescence"
    AlignmentTransferWorkUnchanged
    disposition
  assertBool "initial construction leaves no unobserved notifications" (successor == predecessor)
  assertBool "the quiescent pass retains semantic state" (sameAlignmentTransferWorkState successor predecessor)
  assertEqual
    "the quiescent pass emits no effects"
    []
    (effectBatchMembers effects)
  (fixed, fixedEffects) <-
    checkedAlignmentFixedPoint "empty alignment fixed point" predecessor
  assertBool "the fixed point stops at the unchanged pass" (successor == fixed)
  assertEqual
    "the fixed point preserves the unchanged pass effects"
    (effectBatchMembers effects)
    (effectBatchMembers fixedEffects)

propAlignmentTransferDispositionMatchesReference :: Property
propAlignmentTransferDispositionMatchesReference =
  forAll (chooseInt (1, 4)) $ \replays ->
    ioProperty $ do
      let HeraldMember _ local = fixtureLocalMember
          HeraldMember _ remote = fixtureRemoteMember
      (withLocalReady, generation) <- readyAlignmentFixture local remote
      let generationId = alignmentGenerationId generation
          remoteReady =
            classMemberReady
              (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence)
              generationId
              storeB
              (deriveMemberReadyEvidenceDigest "pending-fixed-point-ready")
              initialStoreRevision
          pendingControl = AlignmentMemberReadyAdvertised remoteReady
          retainPending 0 state = Right state
          retainPending remaining state = do
            prepared <-
              mapGeneratedProblem
                ( Alignment.preparePendingGenerationEvidence
                    remote
                    pendingControl
                    state
                )
            let (successor, _) = Alignment.commitPendingGenerationEvidence prepared
            retainPending (remaining - 1) successor
          result = do
            pendingAlignment <- retainPending replays withLocalReady
            let predecessor = initializedWithAlignment pendingAlignment
            (explicitState, explicitEffects) <-
              mapTransferProblem (advanceAlignmentTransfers predecessor)
            (tracedState, tracedEffects, dispositions, passLaws) <-
              traceAlignmentFixedPoint 16 predecessor
            (referenceState, referenceEffects) <-
              referenceAlignmentFixedPoint 16 predecessor
            Right
              ( explicitState,
                explicitEffects,
                tracedState,
                tracedEffects,
                referenceState,
                referenceEffects,
                dispositions,
                passLaws
              )
      pure
        ( counterexample ("pending evidence replays: " <> show replays)
            $ case result of
              Left problem -> counterexample problem (False === True)
              Right
                ( explicitState,
                  explicitEffects,
                  tracedState,
                  tracedEffects,
                  referenceState,
                  referenceEffects,
                  dispositions,
                  passLaws
                  ) ->
                  conjoin
                    [ counterexample "the admitted pass reports work"
                        $ take 1 dispositions === [AlignmentTransferWorkChanged],
                      counterexample "a changed pass is followed through quiescence"
                        $ last dispositions === AlignmentTransferWorkUnchanged,
                      counterexample "the explicit trace executed the terminal pass"
                        $ length dispositions >= 2,
                      counterexample
                        "every pass reports unchanged exactly when it preserves state"
                        $ and passLaws === True,
                      counterexample "the public fixed point matches its explicit pass trace"
                        $ (explicitState == tracedState) === True,
                      counterexample "the public fixed point preserves traced effects"
                        $ effectBatchMembers explicitEffects
                          === effectBatchMembers tracedEffects,
                      counterexample "the disposition fixed point matches structural reference state"
                        $ sameAlignmentTransferWorkState explicitState referenceState === True,
                      counterexample "the disposition fixed point matches structural reference effects"
                        $ effectBatchMembers explicitEffects
                          === effectBatchMembers referenceEffects
                    ]
        )

checkedAlignmentPass ::
  String ->
  HeraldState ->
  IO (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
checkedAlignmentPass context state =
  case advanceAlignmentTransferPass state of
    Left problem -> assertFailure (context <> ": " <> show problem)
    Right result -> pure result

checkedRouteCutoverPass ::
  String ->
  HeraldState ->
  IO (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
checkedRouteCutoverPass context state =
  case advanceRouteCutovers state of
    Left problem -> assertFailure (context <> ": " <> show problem)
    Right result -> pure result

checkedAlignmentFixedPoint ::
  String ->
  HeraldState ->
  IO (HeraldState, EffectBatch)
checkedAlignmentFixedPoint context state =
  case advanceAlignmentTransfers state of
    Left problem -> assertFailure (context <> ": " <> show problem)
    Right result -> pure result

traceAlignmentFixedPoint ::
  Int ->
  HeraldState ->
  Either
    String
    ( HeraldState,
      EffectBatch,
      [AlignmentTransferWorkDisposition],
      [Bool]
    )
traceAlignmentFixedPoint limit initial = go limit initial mempty [] []
  where
    go remaining predecessor effects dispositions passLaws
      | remaining <= 0 = Left "explicit alignment disposition fixed point did not terminate"
      | otherwise = do
          (successor, passEffects, disposition) <-
            mapTransferProblem (advanceAlignmentTransferPass predecessor)
          (reference, referenceEffects, referenceDisposition) <-
            mapTransferProblem (advanceAlignmentTransferPassExhaustive predecessor)
          let accumulated = effects <> passEffects
              observed = dispositions <> [disposition]
              observedLaws =
                passLaws
                  <> [ (disposition == AlignmentTransferWorkUnchanged)
                         == (sameAlignmentTransferWorkState successor predecessor),
                       sameAlignmentTransferWorkState successor reference,
                       effectBatchMembers passEffects == effectBatchMembers referenceEffects,
                       disposition == referenceDisposition,
                       Alignment.validateAlignmentState (startupAlignmentState successor) == Right (),
                       Alignment.validateAlignmentState (startupAlignmentState reference) == Right ()
                     ]
          case disposition of
            AlignmentTransferWorkUnchanged ->
              Right (successor, accumulated, observed, observedLaws)
            AlignmentTransferWorkChanged ->
              go
                (remaining - 1)
                successor
                accumulated
                observed
                observedLaws

-- The deliberately structural termination check is the former implementation
-- retained only as a test oracle for representative changed schedules.
referenceAlignmentFixedPoint ::
  Int ->
  HeraldState ->
  Either String (HeraldState, EffectBatch)
referenceAlignmentFixedPoint limit initial = go limit initial mempty
  where
    go remaining predecessor effects
      | remaining <= 0 = Left "structural alignment reference fixed point did not terminate"
      | otherwise = do
          (successor, passEffects, _) <-
            mapTransferProblem (advanceAlignmentTransferPassExhaustive predecessor)
          let accumulated = effects <> passEffects
          if sameAlignmentTransferWorkState successor predecessor
            then Right (successor, accumulated)
            else go (remaining - 1) successor accumulated

mapTransferProblem ::
  (Show problem) =>
  Either problem value ->
  Either String value
mapTransferProblem = either (Left . show) Right

data GeneratedAdmittedEvidence
  = GeneratedCutAcceptance
  | GeneratedMemberReadiness
  deriving stock (Eq, Ord, Show)

data GeneratedEvidenceDisposition
  = GeneratedCutAcceptanceRetained
  | GeneratedCutAcceptanceUnchanged
  | GeneratedMemberReadinessRetained
  | GeneratedMemberReadinessUnchanged
  deriving stock (Eq, Ord, Show)

generatedAdmittedEvidenceSchedule :: Gen [GeneratedAdmittedEvidence]
generatedAdmittedEvidenceSchedule = do
  cutReplays <- chooseInt (1, 4)
  readinessReplays <- chooseInt (1, 4)
  shuffle
    ( replicate (cutReplays + 1) GeneratedCutAcceptance
        <> replicate (readinessReplays + 1) GeneratedMemberReadiness
    )

propGeneratedAdmittedEvidenceConverges :: Property
propGeneratedAdmittedEvidenceConverges =
  forAll generatedAdmittedEvidenceSchedule $ \schedule ->
    ioProperty $ do
      let HeraldMember _ local = fixtureLocalMember
          HeraldMember _ remote = fixtureRemoteMember
      (initialAlignment, generation) <- readyAlignmentFixture local remote
      let generationId = alignmentGenerationId generation
          cut = alignmentGenerationCut generation
          accepted =
            alignmentCutAccepted
              generationId
              remote
              (deriveTopologyCutId (alignmentCutTopologyCut cut))
              (alignmentCutPhysicalPlacementRevisionVector cut)
              EmptyHeraldPublicationPrefix
          ready =
            classMemberReady
              (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence)
              generationId
              storeB
              (deriveMemberReadyEvidenceDigest "generated-remote-ready")
              initialStoreRevision
          expectedDispositions = expectedEvidenceDispositions schedule
          result = do
            (directAlignment, directDispositions) <-
              admitGeneratedEvidence accepted ready schedule initialAlignment
            (pendingAlignment, pendingDispositions) <-
              retainGeneratedPendingEvidence
                remote
                accepted
                ready
                schedule
                initialAlignment
            (converged, _) <-
              mapGeneratedProblem
                (advanceAlignmentGenerations (initializedWithAlignment pendingAlignment))
            (quiescent, quiescentEffects) <-
              mapGeneratedProblem (advanceAlignmentGenerations converged)
            Right
              ( directAlignment,
                directDispositions,
                pendingDispositions,
                converged,
                quiescent,
                quiescentEffects
              )
      pure
        ( counterexample ("generated evidence schedule: " <> show schedule)
            $ case result of
              Left problem -> counterexample problem (False === True)
              Right
                ( directAlignment,
                  directDispositions,
                  pendingDispositions,
                  converged,
                  quiescent,
                  quiescentEffects
                  ) ->
                  conjoin
                    [ counterexample "direct admission dispositions"
                        $ directDispositions === expectedDispositions,
                      counterexample "pending admission dispositions"
                        $ pendingDispositions === expectedDispositions,
                      counterexample "every changed and unchanged evidence arm was exercised"
                        $ Set.fromList directDispositions
                          === Set.fromList
                            [ GeneratedCutAcceptanceRetained,
                              GeneratedCutAcceptanceUnchanged,
                              GeneratedMemberReadinessRetained,
                              GeneratedMemberReadinessUnchanged
                            ],
                      counterexample "the changed pass converged to direct admission"
                        $ sameAlignmentTransferWorkState converged (replaceStartupAlignmentState directAlignment converged) === True,
                      counterexample "the fixed point drained all pending evidence"
                        $ Alignment.pendingGenerationEvidenceEntries
                          (startupAlignmentState converged)
                          === [],
                      counterexample "the converged alignment state remains valid"
                        $ Alignment.validateAlignmentState (startupAlignmentState converged)
                          === Right (),
                      counterexample "the direct alignment indexes remain independently valid"
                        $ Alignment.validateAlignmentState directAlignment
                          === Right (),
                      counterexample "the terminal unchanged pass is state-idempotent"
                        $ (quiescent == converged) === True,
                      counterexample "the terminal unchanged pass is effect-free"
                        $ effectBatchMembers quiescentEffects === []
                    ]
        )

admitGeneratedEvidence ::
  AlignmentCutAccepted ->
  ClassMemberReady ->
  [GeneratedAdmittedEvidence] ->
  Alignment.State ->
  Either String (Alignment.State, [GeneratedEvidenceDisposition])
admitGeneratedEvidence accepted ready schedule initial = go [] initial schedule
  where
    go dispositions state [] = Right (state, reverse dispositions)
    go dispositions state (evidence : remaining) = do
      (successor, disposition) <- case evidence of
        GeneratedCutAcceptance -> do
          prepared <-
            mapGeneratedProblem
              (Alignment.prepareAlignmentCutAcceptance accepted state)
          let (retained, observed) =
                Alignment.commitAlignmentCutAcceptance prepared
          Right
            ( retained,
              case observed of
                Alignment.AlignmentCutEvidenceRetained ->
                  GeneratedCutAcceptanceRetained
                Alignment.AlignmentCutEvidenceUnchanged ->
                  GeneratedCutAcceptanceUnchanged
            )
        GeneratedMemberReadiness -> do
          prepared <-
            mapGeneratedProblem
              (Alignment.prepareClassMemberReady ready state)
          let (retained, observed) = Alignment.commitClassMemberReady prepared
          Right
            ( retained,
              case observed of
                Alignment.AlignmentReadinessRetained ->
                  GeneratedMemberReadinessRetained
                Alignment.AlignmentReadinessUnchanged ->
                  GeneratedMemberReadinessUnchanged
            )
      go (disposition : dispositions) successor remaining

retainGeneratedPendingEvidence ::
  HeraldEpoch ->
  AlignmentCutAccepted ->
  ClassMemberReady ->
  [GeneratedAdmittedEvidence] ->
  Alignment.State ->
  Either String (Alignment.State, [GeneratedEvidenceDisposition])
retainGeneratedPendingEvidence remote accepted ready schedule initial =
  go [] initial schedule
  where
    go dispositions state [] = Right (state, reverse dispositions)
    go dispositions state (evidence : remaining) = do
      let control = case evidence of
            GeneratedCutAcceptance -> AlignmentCutAcceptanceAdvertised accepted
            GeneratedMemberReadiness -> AlignmentMemberReadyAdvertised ready
      prepared <-
        mapGeneratedProblem
          (Alignment.preparePendingGenerationEvidence remote control state)
      let (successor, observed) =
            Alignment.commitPendingGenerationEvidence prepared
      disposition <- case (evidence, observed) of
        (GeneratedCutAcceptance, Alignment.PendingGenerationEvidenceRetained) ->
          Right GeneratedCutAcceptanceRetained
        (GeneratedCutAcceptance, Alignment.PendingGenerationEvidenceUnchanged) ->
          Right GeneratedCutAcceptanceUnchanged
        (GeneratedMemberReadiness, Alignment.PendingGenerationEvidenceRetained) ->
          Right GeneratedMemberReadinessRetained
        (GeneratedMemberReadiness, Alignment.PendingGenerationEvidenceUnchanged) ->
          Right GeneratedMemberReadinessUnchanged
        (_, Alignment.PendingGenerationEvidenceRemoved) ->
          Left "pending evidence insertion unexpectedly reported removal"
      go (disposition : dispositions) successor remaining

expectedEvidenceDispositions ::
  [GeneratedAdmittedEvidence] -> [GeneratedEvidenceDisposition]
expectedEvidenceDispositions = go False False
  where
    go _ _ [] = []
    go cutSeen readinessSeen (evidence : remaining) =
      case evidence of
        GeneratedCutAcceptance ->
          ( if cutSeen
              then GeneratedCutAcceptanceUnchanged
              else GeneratedCutAcceptanceRetained
          )
            : go True readinessSeen remaining
        GeneratedMemberReadiness ->
          ( if readinessSeen
              then GeneratedMemberReadinessUnchanged
              else GeneratedMemberReadinessRetained
          )
            : go cutSeen True remaining

mapGeneratedProblem :: (Show problem) => Either problem value -> Either String value
mapGeneratedProblem = either (Left . show) Right

caseLevelTriggeredEvidence :: Assertion
caseLevelTriggeredEvidence = do
  (promoted, generation) <- promotedFixture
  let generationId = alignmentGenerationId generation
      localAcceptance =
        alignmentCutAccepted
          generationId
          heraldA
          topologyId
          placement
          EmptyHeraldPublicationPrefix
      withLocalAcceptance =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain local cut acceptance"
          (Alignment.prepareAlignmentCutAcceptance localAcceptance promoted)
      initialRetry = alignmentRetryControls heraldA heraldB withLocalAcceptance
  assertBool
    "announcer reoffers the retained checked cut without reconstructing it"
    ( AlignmentCutAnnounced (alignmentGenerationCutAnnounce generation)
        `elem` initialRetry
    )
  assertBool
    "acceptor reoffers its own immutable evidence"
    (any (isAcceptanceFrom heraldA) initialRetry)

  let remoteAcceptance =
        alignmentCutAccepted
          generationId
          heraldB
          topologyId
          placement
          EmptyHeraldPublicationPrefix
      withRemoteAcceptance =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain remote cut acceptance"
          (Alignment.prepareAlignmentCutAcceptance remoteAcceptance withLocalAcceptance)
      afterRemoteAcceptance =
        alignmentRetryControls heraldA heraldB withRemoteAcceptance
  assertBool
    "the announcer stops cut reoffer after the remote's own acceptance"
    (not (any isAnnounce afterRemoteAcceptance))
  assertEqual
    "retaining remote acceptance does not advertise another semantic fact"
    []
    (alignmentGenerationControlDelta heraldA heraldB withLocalAcceptance withRemoteAcceptance)

  let readyA =
        classMemberReady
          firstAlignmentEvidenceSequence
          generationId
          storeA
          (deriveMemberReadyEvidenceDigest "ready-a")
          initialStoreRevision
      readyB =
        classMemberReady
          (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence)
          generationId
          storeB
          (deriveMemberReadyEvidenceDigest "ready-b")
          initialStoreRevision
      withReadyA =
        commit
          Alignment.commitClassMemberReady
          "retain local member readiness"
          (Alignment.prepareClassMemberReady readyA withRemoteAcceptance)
      withReady =
        commit
          Alignment.commitClassMemberReady
          "retain remote member readiness"
          (Alignment.prepareClassMemberReady readyB withReadyA)
      readinessRetry = alignmentRetryControls heraldA heraldB withReady
      reconnectRetry = alignmentReconnectRetryControls heraldA heraldB withReady
  assertBool
    "local member readiness remains offered"
    (any isMemberReady readinessRetry)
  assertEqual
    "semantic reconnect catalogue contains the same immutable roots"
    readinessRetry
    reconnectRetry
  assertEqual
    "unchanged readiness does not repeat semantic advertisements"
    []
    (alignmentGenerationControlDelta heraldA heraldB withReady withReady)

  let certificate =
        historicalCertificate
          generationId
          storeA
          (classMemberReadySetDigest [readyB, readyA])
          ( deriveBootstrapEvidenceDigest
              ( memberReadyEvidenceDigestBytes
                  (classMemberReadyPredecessorAndBasePrefixDigest readyA)
              )
          )
          initialStoreRevision
      changedPrefixCertificate =
        historicalCertificate
          generationId
          storeA
          (classMemberReadySetDigest [readyB, readyA])
          (deriveBootstrapEvidenceDigest "changed-complete-prefix")
          initialStoreRevision
  assertEqual
    "the completed prefix digest is exactly the source-ready transcript"
    ( Left
        ( Alignment.AlignmentCertificatePrefixDigestMismatch
            ( deriveBootstrapEvidenceDigest
                ( memberReadyEvidenceDigestBytes
                    (classMemberReadyPredecessorAndBasePrefixDigest readyA)
                )
            )
            (deriveBootstrapEvidenceDigest "changed-complete-prefix")
        )
    )
    ( fmap
        (const ())
        (Alignment.prepareHistoricalCertificate changedPrefixCertificate withReady)
    )
  let
    certified =
      commit
        Alignment.commitHistoricalCertificate
        "retain local historical certificate"
        (Alignment.prepareHistoricalCertificate certificate withReady)
    certificateRetry = alignmentRetryControls heraldA heraldB certified
  assertBool
    "the local source certificate is reoffered after reconnect"
    (any isHistoricalCertificate certificateRetry)

caseGenerationEvidenceQueueQuiesces :: Assertion
caseGenerationEvidenceQueueQuiesces = do
  (promoted, generation) <- promotedFixture
  let generationId = alignmentGenerationId generation
      acceptanceA =
        alignmentCutAccepted
          generationId
          heraldA
          topologyId
          placement
          EmptyHeraldPublicationPrefix
      acceptanceB =
        alignmentCutAccepted
          generationId
          heraldB
          topologyId
          placement
          EmptyHeraldPublicationPrefix
      withAcceptanceA =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain queue acceptance A"
          (Alignment.prepareAlignmentCutAcceptance acceptanceA promoted)
      withAcceptances =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain queue acceptance B"
          (Alignment.prepareAlignmentCutAcceptance acceptanceB withAcceptanceA)
      readyA =
        classMemberReady
          firstAlignmentEvidenceSequence
          generationId
          storeA
          (deriveMemberReadyEvidenceDigest "queue-ready-a")
          initialStoreRevision
      readyB =
        classMemberReady
          (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence)
          generationId
          storeB
          (deriveMemberReadyEvidenceDigest "queue-ready-b")
          initialStoreRevision
      withReadyA =
        commit
          Alignment.commitClassMemberReady
          "retain queue readiness A"
          (Alignment.prepareClassMemberReady readyA withAcceptances)
      withReadiness =
        commit
          Alignment.commitClassMemberReady
          "retain queue readiness B"
          (Alignment.prepareClassMemberReady readyB withReadyA)
      certificateB =
        historicalCertificate
          generationId
          storeB
          (classMemberReadySetDigest [readyA, readyB])
          ( deriveBootstrapEvidenceDigest
              ( memberReadyEvidenceDigestBytes
                  (classMemberReadyPredecessorAndBasePrefixDigest readyB)
              )
          )
          initialStoreRevision
      retained =
        commit
          Alignment.commitHistoricalCertificate
          "retain queue certificate B"
          (Alignment.prepareHistoricalCertificate certificateB withReadiness)
      replayedAcceptance =
        commit Alignment.commitAlignmentCutAcceptance "replay remote acceptance" (Alignment.prepareAlignmentCutAcceptance acceptanceB retained)
      replayedReadiness =
        commit Alignment.commitClassMemberReady "replay remote readiness" (Alignment.prepareClassMemberReady readyB replayedAcceptance)
      replayed =
        commit Alignment.commitHistoricalCertificate "replay remote certificate" (Alignment.prepareHistoricalCertificate certificateB replayedReadiness)
  assertEqual
    "remote acceptance enables no reciprocal semantic fact"
    []
    (alignmentGenerationControlDelta heraldA heraldB withAcceptanceA withAcceptances)
  assertEqual "all repeated immutable evidence is state-idempotent" retained replayed
  assertEqual
    "an unchanged evidence state enables no controls"
    []
    ( alignmentGenerationControlDelta
        (error "unchanged alignment delta evaluated local Herald")
        (error "unchanged alignment delta evaluated remote Herald")
        retained
        replayed
    )

-- The old observed-set formulation remains independent of the production
-- expected-key query. Every marker here enters through composed admission;
-- arbitrary leaf transfers need not satisfy its fixed-source subset law.
propRouteClosureQueriesReference :: Property
propRouteClosureQueriesReference =
  forAll (chooseInt (1, 4)) $ \repetitions ->
    forAll (shuffle [0 :: Int, 1, 2, 3, 4]) $ \order -> ioProperty $ do
      -- Certify the remote-only predecessor before promoting the successor.
      -- Herald A owns that predecessor; B is the newly added member/source.
      (initial, generation, _) <- routeProjectionAlignmentFixtureFrom True heraldB heraldA
      let generationId = alignmentGenerationId generation
          cut = alignmentGenerationCut generation
          acceptance herald =
            alignmentCutAccepted
              generationId
              herald
              (deriveTopologyCutId (alignmentCutTopologyCut cut))
              (alignmentCutPhysicalPlacementRevisionVector cut)
              EmptyHeraldPublicationPrefix
          marker = checked "reference incoming cutover" (alignmentRouteCutoverMarker (Plan.alignmentGenerationBirthPlanId generation) generationId heraldB heraldA)
          advance event state = case event of
            0 -> pure (commit Alignment.commitAlignmentCutAcceptance "route reference local acceptance" (Alignment.prepareAlignmentCutAcceptance (acceptance heraldA) state))
            1 -> pure (commit Alignment.commitAlignmentCutAcceptance "route reference remote acceptance" (Alignment.prepareAlignmentCutAcceptance (acceptance heraldB) state))
            2 -> case retainAlignmentRouteCutoverMarker heraldB heraldA marker state of
              Left (AlignmentRouteCutoverSourceAcceptanceMissing _ _) -> do
                assertEqual
                  "an unadmitted marker has no installed key"
                  Nothing
                  (Transfer.lookupRouteCutover generationId heraldB heraldA (Alignment.alignmentTransferState state))
                pure state
              Left problem -> assertFailure ("route reference admission: " <> show problem)
              Right successor -> pure successor
            3
              | Alignment.lookupAlignmentCutAcceptance generationId heraldA state == Nothing -> pure state
              | otherwise ->
                  pure
                    ( Alignment.replaceAlignmentTransferState
                        ( commit
                            Transfer.commitLocalRoutePrefixSettlement
                            "route reference settlement"
                            (Transfer.prepareLocalRoutePrefixSettlement generationId heraldA (Alignment.alignmentTransferState state))
                        )
                        state
                    )
            _ ->
              pure
                ( commit
                    Alignment.commitAlignmentPlanInvalidation
                    "route reference exact Store loss"
                    (Alignment.prepareAlignmentPlanInvalidation (Alignment.AlignmentPlanDestinationStoreLost generationId (destinationStore deltaA storeA)) state)
                )
          schedule = [2] <> concat (replicate repetitions order) <> [0, 1, 2, 3, 4, 2]
          activeSets =
            [ Set.fromList [herald | (herald, include) <- zip [heraldA, heraldB, heraldC] flags, include]
            | flags <- sequence (replicate 3 [False, True])
            ]
      histories <- foldM (\states event -> do successor <- advance event (last states); pure (states <> [successor])) [initial] schedule
      forM_ histories $ \state -> do
        assertEqual "route reference history is admitted and valid" (Right ()) (Alignment.validateAlignmentState state)
        assertMarkerAdmissionFacts state
        forM_ (Alignment.alignmentGenerationEntries state) $ \(identifier, retainedGeneration) ->
          forM_ activeSets $ \active ->
            forM_ [heraldA, heraldB, heraldC] $ \predecessor ->
              assertEqual
                "expected-key route query equals the frozen observed-set reference"
                (referenceRouteCutoverSetComplete active identifier predecessor (alignmentGenerationCut retainedGeneration) (Alignment.alignmentTransferState state))
                (routeCutoverSetComplete active identifier predecessor (alignmentGenerationCut retainedGeneration) (Alignment.alignmentTransferState state))
      let finalTransfer = Alignment.alignmentTransferState (last histories)
      assertEqual "late delivery and replay retain exactly one historical marker" [marker] (map snd (Transfer.routeCutoverEntries finalTransfer))
      assertBool
        "missing active source initially blocks completeness"
        (not (routeCutoverSetComplete (Set.fromList [heraldA, heraldB]) generationId heraldA cut (Alignment.alignmentTransferState initial)))
      assertBool
        "the admitted marker completes its active route set"
        (routeCutoverSetComplete (Set.fromList [heraldA, heraldB]) generationId heraldA cut finalTransfer)
      pure True

referenceRouteCutoverSetComplete ::
  Set.Set HeraldEpoch ->
  ContextClassGenerationId ->
  HeraldEpoch ->
  DomainAlignment.AlignmentCut ->
  Transfer.State ->
  Bool
referenceRouteCutoverSetComplete active generation predecessor cut transfer
  | Set.notMember predecessor active = True
  | otherwise = observed == expected
  where
    fixed = Set.fromList (map fst (NonEmpty.toList (physicalPlacementRevisionEntries (alignmentCutPhysicalPlacementRevisionVector cut))))
    expected = Set.delete predecessor (Set.intersection active fixed)
    observed =
      Set.fromList
        [ source
        | ((retainedGeneration, source, retainedPredecessor), _) <- Transfer.routeCutoverEntries transfer,
          retainedGeneration == generation,
          retainedPredecessor == predecessor,
          Set.member source active,
          Set.member retainedPredecessor active
        ]

assertMarkerAdmissionFacts :: Alignment.State -> Assertion
assertMarkerAdmissionFacts alignment =
  forM_ (Transfer.routeCutoverEntries (Alignment.alignmentTransferState alignment)) $ \((generationId, source, predecessor), _) -> do
    let generation = checkedMaybe "marker generation retained" (Alignment.lookupAlignmentGeneration generationId alignment)
        fixed = map fst (NonEmpty.toList (physicalPlacementRevisionEntries (alignmentCutPhysicalPlacementRevisionVector (alignmentGenerationCut generation))))
    assertBool "installed marker retains immutable source acceptance" (Alignment.lookupAlignmentCutAcceptance generationId source alignment /= Nothing)
    assertBool "installed marker source is in the frozen placement" (source `elem` fixed)
    assertBool "installed markers exclude local self markers" (source /= predecessor)

assertCurrentClosureRouteQueries :: HeraldState -> Assertion
assertCurrentClosureRouteQueries state = do
  assertMarkerAdmissionFacts alignment
  forM_ (Alignment.alignmentGenerationEntries alignment) $ \(identifier, generation) ->
    forM_ (Set.toAscList (Set.insert local active)) $ \predecessor ->
      assertEqual
        "membership-projected closure query equals the retained-history reference"
        (referenceRouteCutoverSetComplete active identifier predecessor (alignmentGenerationCut generation) transfer)
        (routeCutoverSetComplete active identifier predecessor (alignmentGenerationCut generation) transfer)
  where
    alignment = startupAlignmentState state
    transfer = Alignment.alignmentTransferState alignment
    local = checkedLocalHeraldEpoch (startupGenesis state)
    active =
      Set.fromList
        ( NonEmpty.toList
            ( Membership.heraldMembershipGenerationActiveHeraldEpochs
                (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state)))
            )
        )

-- Exercise the real source-admission boundary with a certified predecessor,
-- exact local Store, and authenticated remote binding. This fixture supplies
-- the coordinator dependencies directly; it is not a whole-Herald graph fixture.
predecessorSourceAdmissionFixture :: IO (HeraldState, PeerBinding, AlignmentProtocol.AlignmentSubscribe, AlignmentGeneration)
predecessorSourceAdmissionFixture = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (promoted, generation, _) <- routeProjectionAlignmentFixtureFrom True remote local
  predecessorId <- case alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation) of
    [identifier] -> pure identifier
    identifiers -> assertFailure ("source admission fixture requires one predecessor, got " <> show identifiers)
  let predecessor = checkedMaybe "source admission predecessor" (Alignment.lookupAlignmentGeneration predecessorId promoted)
      predecessorCutValue = alignmentGenerationCut predecessor
      accepted =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain exact source predecessor acceptance"
          ( Alignment.prepareAlignmentCutAcceptance
              ( alignmentCutAccepted
                  predecessorId
                  local
                  (deriveTopologyCutId (alignmentCutTopologyCut predecessorCutValue))
                  (alignmentCutPhysicalPlacementRevisionVector predecessorCutValue)
                  EmptyHeraldPublicationPrefix
              )
              promoted
          )
  importKey <- case [key | (key, _) <- Alignment.bootstrapImportEntries accepted, Alignment.bootstrapImportKeySource key == Alignment.BootstrapFromPredecessor predecessorId] of
    key : _ -> pure key
    [] -> assertFailure "source admission fixture requires a real predecessor bootstrap import"
  -- Obtain the generation/certificate proof from normal destination preparation;
  -- the source independently admits the remote-owned singleton envelope below.
  let proofAttempt = Alignment.preparedBootstrapImportAttempt (checked "prepare genuine predecessor bootstrap proof" (Alignment.prepareBootstrapImportAttempt importKey accepted))
      proof = AlignmentProtocol.alignmentSubscribeSourceProof (Alignment.bootstrapImportAttemptSubscribe proofAttempt)
      subscription = AlignmentProtocol.alignmentSubscriptionId remote AlignmentProtocol.firstAlignmentSubscriptionSequence
      obligation =
        checked
          "source admission remote bootstrap obligation"
          ( AlignmentProtocol.alignmentObligation
              (AlignmentProtocol.alignmentObligationId remote AlignmentProtocol.firstAlignmentObligationSequence)
              (cause successorOccurrence)
              (sortOccurrenceSortId typedSort)
              (sortOccurrenceDefinition typedSort)
              (alignmentGenerationId generation)
              (destinationStore deltaA storeA :| [])
              predecessorId
              Route.Normal
          )
      subscribe = checked "source admission exact request" (AlignmentProtocol.alignmentSubscribe obligation subscription storeB proof)
  (connected, binding) <- connectedWithAlignment accepted
  let store =
        Store.commitStoreBootstrap
          ( checked
              "source admission exact retained Store"
              ( Store.prepareStoreBootstrap
                  [Store.localStoreSpec (Store.HeraldSystemView local SortProfile.SortDefinitionRole) deltaB (sortOccurrenceSortId typedSort) (sortOccurrenceDefinition typedSort) storeB]
                  (startupStoreState connected)
              )
          )
      initial = replaceStartupStoreState store connected
  pure (initial, binding, subscribe, generation)

casePredecessorSourceAdmissionFacts :: Assertion
casePredecessorSourceAdmissionFacts = do
  (initial, binding, subscribe, _) <- predecessorSourceAdmissionFixture
  let subscription = AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe
      apply control state = case applyAlignmentTransferControl binding control state of
        Left problem -> assertFailure ("predecessor source admission: " <> show problem)
        Right result -> pure result
      request = AlignmentSubscribeRequested subscribe
      sourceOf state = checkedMaybe "retained real predecessor source" (Transfer.lookupSourceSubscription subscription (Alignment.alignmentTransferState (startupAlignmentState state)))
  (admitted, admittedEffects) <- apply request initial
  assertEqual "real source admission retains the exact submitted request" [subscribe] (predecessorClosureSourceSubscriptions admitted)
  assertBool "predecessor source withholds Live pending input closure" (not (Transfer.sourceSubscriptionLiveReleased (sourceOf admitted)))
  (replayed, replayedEffects) <- apply request admitted
  assertBool "exact source request replay preserves state" (replayed == admitted)
  assertEqual "exact source request replay repeats the same snapshot controls" admittedEffects replayedEffects
  let cancellation = AlignmentProtocol.alignmentCancel subscription AlignmentProtocol.AlignmentRelationRemoved
  (cancelled, _) <- apply (AlignmentCancelled cancellation) replayed
  assertEqual "cancellation is retained on the actual source" (Just cancellation) (Transfer.sourceSubscriptionCancellation (sourceOf cancelled))
  (terminalReplay, _) <- apply request cancelled
  assertBool "terminal source replay preserves state" (terminalReplay == cancelled)
  forM_ [admitted, replayed, cancelled, terminalReplay] $ \state -> do
    assertEqual "source history preserves admitted alignment invariants" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState state))
    assertEqual "source history remains non-vacuous" [subscribe] (predecessorClosureSourceSubscriptions state)
    assertClosureSourceAdmissionFacts state

propSelectiveInputClosureReference :: Property
propSelectiveInputClosureReference =
  forAll (chooseInt (1, 5)) $ \quietReplays -> ioProperty $ do
    (initial, binding, subscribe, generation) <- predecessorSourceAdmissionFixture
    let identifier = AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe
        secondIdentifier = AlignmentProtocol.alignmentSubscriptionId (AlignmentProtocol.alignmentSubscriptionIdDestinationHerald identifier) (AlignmentProtocol.nextAlignmentSubscriptionSequence AlignmentProtocol.firstAlignmentSubscriptionSequence)
        secondSubscribe = checked "second source for the same closure" (AlignmentProtocol.alignmentSubscribe (AlignmentProtocol.alignmentSubscribeObligation subscribe) secondIdentifier (AlignmentProtocol.alignmentSubscribeSourceStoreIncarnation subscribe) (AlignmentProtocol.alignmentSubscribeSourceProof subscribe))
        sources = Set.fromList [identifier, secondIdentifier]
        generationId = alignmentGenerationId generation
        local = checkedLocalHeraldEpoch (startupGenesis initial)
        HeraldMember _ remote = fixtureRemoteMember
        apply control state = fst <$> either (assertFailure . show) pure (applyAlignmentTransferControl binding control state)
        replaceTransfer update state =
          let alignment = startupAlignmentState state
           in replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState (update (Alignment.alignmentTransferState alignment)) alignment) state
        settle = replaceTransfer (commit Transfer.commitLocalRoutePrefixSettlement "settle source closure prefix" . Transfer.prepareLocalRoutePrefixSettlement generationId local)
        marker = checked "closure enabling route marker" (alignmentRouteCutoverMarker (Plan.alignmentGenerationBirthPlanId generation) generationId remote local)
        mark state = replaceStartupAlignmentState (checked "admit closure enabling route marker" (retainAlignmentRouteCutoverMarker remote local marker (startupAlignmentState state))) state
        pending = Transfer.pendingInputClosureSourceIds . Alignment.alignmentTransferState . startupAlignmentState
        cancelled = AlignmentProtocol.alignmentCancel secondIdentifier AlignmentProtocol.AlignmentRelationRemoved
    admitted <- apply (AlignmentSubscribeRequested subscribe) initial >>= apply (AlignmentSubscribeRequested secondSubscribe)
    assertEqual "only the two actual source admissions begin dirty" sources (pending admitted)
    (blocked, blockedEffects, blockedWork) <- assertInputClosureReference "initial blocked closures" admitted
    assertEqual "route prerequisites block both sources without effects" [] (effectBatchMembers blockedEffects)
    assertEqual "testing a blocked predicate is not semantic work" AlignmentTransferWorkUnchanged blockedWork
    assertEqual "initial blocked evaluations consume all work" Set.empty (pending blocked)
    forM_ [1 .. quietReplays] $ \tick -> do
      let unchangedFacts = replaceStartupLastObservedTime (monotonicInstant (fromIntegral tick)) blocked
      (quiet, effects, work) <- assertInputClosureReference "unrelated observation" unchangedFacts
      assertBool "unchanged dependencies select no source and preserve the complete owner" (quiet == unchangedFacts)
      assertEqual "dormant source work stays empty" Set.empty (pending quiet)
      assertEqual "dormant closure emits no effects" [] (effectBatchMembers effects)
      assertEqual "dormant closure reports no work" AlignmentTransferWorkUnchanged work
    let settled = settle blocked
    assertEqual "one child settlement wakes exactly its two sources" sources (pending settled)
    (waitingForMarker, _, _) <- assertInputClosureReference "settlement alone is insufficient" settled
    assertEqual "the second blocker is parked after one evaluation" Set.empty (pending waitingForMarker)
    afterCancellation <- apply (AlignmentCancelled cancelled) waitingForMarker
    assertEqual "cancellation removes the source from closure inventory" (Set.singleton identifier) (Transfer.inputClosureSourceIds (Alignment.alignmentTransferState (startupAlignmentState afterCancellation)))
    (cancelledQuiet, _, _) <- assertInputClosureReference "cancelled source cannot be selected" afterCancellation
    let ready = mark cancelledQuiet
    assertEqual "the route marker wakes only the remaining eligible source" (Set.singleton identifier) (pending ready)
    (released, releaseEffects, releaseWork) <- assertInputClosureReference "last route prerequisite releases Live" ready
    assertEqual "source release reports actual work" AlignmentTransferWorkChanged releaseWork
    assertBool "source release emits its retained Live control" (not (null (effectBatchMembers releaseEffects)))
    assertBool "the surviving source was released" (Transfer.sourceSubscriptionLiveReleased (checkedMaybe "released source" (Transfer.lookupSourceSubscription identifier (Alignment.alignmentTransferState (startupAlignmentState released)))))
    assertEqual "released sources leave closure work" Set.empty (pending released)
    (releasedAgain, repeatedEffects, repeatedWork) <- assertInputClosureReference "completed closure replay" (mark released)
    assertBool "duplicate route evidence preserves the completed owner" (releasedAgain == released)
    assertEqual "completed closure has no repeated effects" [] (effectBatchMembers repeatedEffects)
    assertEqual "completed closure has no repeated work" AlignmentTransferWorkUnchanged repeatedWork
    let invalidatedAlignment = commit Alignment.commitAlignmentPlanInvalidation "invalidate a parked source child plan" (Alignment.prepareAlignmentPlanInvalidation (Alignment.AlignmentPlanDestinationStoreLost generationId (destinationStore deltaA storeA)) (startupAlignmentState waitingForMarker))
        invalidated = replaceStartupAlignmentState invalidatedAlignment waitingForMarker
    (invalidatedQuiet, invalidatedEffects, invalidatedWork) <- assertInputClosureReference "invalidated child consumes its notification" invalidated
    assertEqual "invalidated plan cannot release source Live" [] (effectBatchMembers invalidatedEffects)
    assertEqual "already invalidated facts add no closure work" AlignmentTransferWorkUnchanged invalidatedWork
    assertEqual "invalidated source predicates park again" Set.empty (pending invalidatedQuiet)
    pure True

caseInputClosureMembershipWake :: Assertion
caseInputClosureMembershipWake = do
  (initial, binding, subscribe, generation) <- predecessorSourceAdmissionFixture
  admitted <- fst <$> either (assertFailure . show) pure (applyAlignmentTransferControl binding (AlignmentSubscribeRequested subscribe) initial)
  let identifier = AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe
      generationId = alignmentGenerationId generation
      local = checkedLocalHeraldEpoch (startupGenesis admitted)
      HeraldMember _ remote = fixtureRemoteMember
      alignment = startupAlignmentState admitted
      transfer = commit Transfer.commitLocalRoutePrefixSettlement "settle before membership change" (Transfer.prepareLocalRoutePrefixSettlement generationId local (Alignment.alignmentTransferState alignment))
      settled = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState transfer alignment) admitted
  (parked, _, _) <- assertInputClosureReference "missing active remote route marker" settled
  assertEqual "membership fixture starts with no pending closure evaluation" Set.empty (Transfer.pendingInputClosureSourceIds (Alignment.alignmentTransferState (startupAlignmentState parked)))
  -- Apply the real checked membership transition to its admitted Oracle prefix;
  -- the source fixture supplies only the alignment dependencies, not a full graph.
  let retiredProjection = startupOracleProjectionState (retireOracleMember remote (initializedWithAlignment Alignment.emptyState))
      retired = replaceStartupOracleProjectionState retiredProjection parked
      frozen = replaceStartupOracleProjectionState retiredProjection (freezeStartupSemanticControl parked)
      membershipOf = OracleProjection.oracleViewCurrentHeraldMembershipId . OracleProjection.oracleView
  assertBool "the control projection really advanced behind the frozen semantic cut" (membershipOf (startupControlOracleProjectionState frozen) /= membershipOf (startupOracleProjectionState frozen))
  (stillParked, heldEffects, heldWork) <- assertInputClosureReference "control-only membership behind a frozen cut" frozen
  assertBool "frozen semantic membership selects no closure work" (stillParked == frozen)
  assertEqual "control-only membership emits no closure effects" [] (effectBatchMembers heldEffects)
  assertEqual "control-only membership adds no semantic work" AlignmentTransferWorkUnchanged heldWork
  (released, effects, work) <- assertInputClosureReference "semantic membership retirement removes the blocker" retired
  assertEqual "actual membership change enables closure" AlignmentTransferWorkChanged work
  assertBool "membership change emits source Live" (not (null (effectBatchMembers effects)))
  assertBool "the source closes without an inactive member's marker" (Transfer.sourceSubscriptionLiveReleased (checkedMaybe "membership-released source" (Transfer.lookupSourceSubscription identifier (Alignment.alignmentTransferState (startupAlignmentState released)))))
  (quiet, _, quietWork) <- assertInputClosureReference "same membership generation is not another wake" released
  assertBool "membership synchronization stops after the scalar change" (quiet == released)
  assertEqual "membership replay is quiescent" AlignmentTransferWorkUnchanged quietWork

-- The old A -> B input is local at both ends. After A and B join a new child,
-- two remote child subscriptions export B's predecessor Store. Preparing the
-- first barrier really replays Subscribe through the local source and Live back
-- through its destination. That wake crosses both sides of the stage cursor.
caseInputClosureLoopbackOrder :: Assertion
caseInputClosureLoopbackOrder = do
  let HeraldMember localId local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
      localBinding = Discovery.PeerBinding localId local Discovery.firstPeerBindingGeneration (Discovery.PeerCandidate localId local (Discovery.ConnectionNonce 2301))
      members = checked "closure loopback membership" (Membership.genesisHeraldMembershipGeneration (checkedSystemId fixtureStep14CheckedGenesis) (local :| [remote]))
      placements = checked "closure loopback placement" (physicalPlacementRevisionVector members [(local, firstPlacementRevision), (remote, firstPlacementRevision)])
      firstTopology = checked "closure loopback predecessor topology" (topologyCut (sameGenerationPredecessor predecessorCut) (topologyFrontier (emptyStructuralVersionVector members) (controlIndex 0)) (deriveTopologyOccurrenceDigest "closure-loopback-predecessor"))
      secondTopology = checked "closure loopback child topology" (topologyCut (sameGenerationPredecessor (deriveTopologyCutId firstTopology)) (topologyCutFrontier firstTopology) (deriveTopologyOccurrenceDigest "closure-loopback-child"))
      initialMembers = [generationMemberInput deltaA storeA local initialStoreRevision, generationMemberInput deltaB storeB local initialStoreRevision]
      childMembers = initialMembers <> [generationMemberInput deltaC storeC remote initialStoreRevision]
      childGraph = checked "closure loopback merged graph" (deriveContextGraph [DeltaVertex deltaA, DeltaVertex deltaB, DeltaVertex deltaC] [edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve, edgePayload (DeltaVertex deltaB) (DeltaVertex deltaC) Preserve, edgePayload (DeltaVertex deltaC) (DeltaVertex deltaA) Preserve] [deltaA, deltaB, deltaC])
      planAt cut graph memberInputs predecessors = case prepareAlignmentGenerationPlan typedSort cut placements graph memberInputs predecessors [] (Set.fromList (map alignmentGenerationId predecessors)) of
        Right (AlignmentGenerationReady plan) -> pure plan
        result -> assertFailure ("closure loopback plan: " <> show result)
      promote cut consequence plan state = commit Alignment.commitAlignmentPromotion "promote closure loopback plan" (Alignment.prepareAlignmentPromotion local consequence typedSort (deriveTopologyCutId cut) placements plan (retainCauseDebt consequence state))
      accept herald generation state =
        let cut = alignmentGenerationCut generation
         in commit Alignment.commitAlignmentCutAcceptance "accept closure loopback generation" (Alignment.prepareAlignmentCutAcceptance (alignmentCutAccepted (alignmentGenerationId generation) herald (deriveTopologyCutId (alignmentCutTopologyCut cut)) (alignmentCutPhysicalPlacementRevisionVector cut) EmptyHeraldPublicationPrefix) state)
      certify state generation =
        let identifier = alignmentGenerationId generation
            member = NonEmpty.head (alignmentCutExactMembers (alignmentGenerationCut generation))
            incarnation = alignmentMemberStoreIncarnation member
            ready = classMemberReady firstAlignmentEvidenceSequence identifier incarnation (deriveMemberReadyEvidenceDigest "closure-loopback-predecessor") initialStoreRevision
            withReady = commit Alignment.commitClassMemberReady "retain closure loopback readiness" (Alignment.prepareClassMemberReady ready state)
            certificate = historicalCertificate identifier incarnation (classMemberReadySetDigest [ready]) (deriveBootstrapEvidenceDigest (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest ready))) initialStoreRevision
         in commit Alignment.commitHistoricalCertificate "certify closure loopback predecessor" (Alignment.prepareHistoricalCertificate certificate withReady)
      applyLocal state control = do
        (next, effects) <- either (assertFailure . show) pure (applyAlignmentTransferControl localBinding control state)
        foldM applyLocal next [reply | SendPeerControl _ (PeerAlignmentControl reply) <- effectBatchMembers effects]
      transferOf = Alignment.alignmentTransferState . startupAlignmentState
      replaceTransfer update state =
        let alignment = startupAlignmentState state
         in replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState (update (Alignment.alignmentTransferState alignment)) alignment) state
  firstPlan <- planAt firstTopology oneWayContextGraph initialMembers []
  let predecessors = alignmentGenerationPlanGenerations firstPlan
      promoted = promote firstTopology (cause occurrence) firstPlan Alignment.emptyState
      accepted = foldl (flip (accept local)) promoted predecessors
      certified = foldl certify accepted predecessors
  owner <- case Alignment.activeAlignmentObligationEntries certified of
    [(identifier, _)] -> pure identifier
    entries -> assertFailure ("closure loopback requires one ordinary input owner: " <> show entries)
  let preparedAttempt = checked "prepare closure loopback ordinary attempt" (Alignment.prepareAlignmentAttempt owner certified)
      ordinaryAttempt = Alignment.preparedAlignmentAttempt preparedAttempt
      withAttempt = fst (Alignment.commitAlignmentAttempt preparedAttempt)
      preparedDestination = checked "admit closure loopback ordinary destination" (Transfer.prepareDestinationAttempt ordinaryAttempt (Alignment.alignmentTransferState withAttempt))
      ordinarySubscribe = Transfer.preparedDestinationAttemptSubscribe preparedDestination
      ordinaryIdentifier = AlignmentProtocol.alignmentSubscribeSubscriptionId ordinarySubscribe
      withDestination = Alignment.replaceAlignmentTransferState (fst (Transfer.commitDestinationAttempt preparedDestination)) withAttempt
  (connected, binding) <- connectedWithAlignment withDestination
  let stores = Store.commitStoreBootstrap (checked "retain closure loopback Stores" (Store.prepareStoreBootstrap [Store.localStoreSpec (Store.HeraldSystemView local SortProfile.SortDefinitionRole) delta (sortOccurrenceSortId typedSort) (sortOccurrenceDefinition typedSort) incarnation | (delta, incarnation) <- [(deltaA, storeA), (deltaB, storeB)]] (startupStoreState connected)))
  primed <- applyLocal (replaceStartupStoreState stores connected) (AlignmentSubscribeRequested ordinarySubscribe)
  childPlan <- planAt secondTopology childGraph childMembers predecessors
  child <- case alignmentGenerationPlanGenerations childPlan of
    [generation] -> pure generation
    generations -> assertFailure ("closure loopback requires one merged child: " <> show generations)
  let childId = alignmentGenerationId child
      childAlignment = accept remote child (accept local child (promote secondTopology (cause successorOccurrence) childPlan (startupAlignmentState primed)))
      predecessorId = AlignmentProtocol.alignmentObligationDestinationGeneration (AlignmentProtocol.alignmentSubscribeObligation ordinarySubscribe)
      importKey = checkedMaybe "closure loopback B predecessor import" (listToMaybe [key | (key, _) <- Alignment.bootstrapImportEntries childAlignment, Alignment.bootstrapImportKeySource key == Alignment.BootstrapFromPredecessor predecessorId])
      importAttempt = Alignment.preparedBootstrapImportAttempt (checked "prepare closure loopback child proof" (Alignment.prepareBootstrapImportAttempt importKey childAlignment))
      proof = AlignmentProtocol.alignmentSubscribeSourceProof (Alignment.bootstrapImportAttemptSubscribe importAttempt)
      obligation = checked "closure loopback remote child obligation" (AlignmentProtocol.alignmentObligation (AlignmentProtocol.alignmentObligationId remote AlignmentProtocol.firstAlignmentObligationSequence) (cause successorOccurrence) (sortOccurrenceSortId typedSort) (sortOccurrenceDefinition typedSort) childId (destinationStore deltaC storeC :| []) predecessorId Route.Normal)
      identifiers = [AlignmentProtocol.alignmentSubscriptionId remote subscriptionSequence | subscriptionSequence <- [AlignmentProtocol.firstAlignmentSubscriptionSequence, AlignmentProtocol.nextAlignmentSubscriptionSequence AlignmentProtocol.firstAlignmentSubscriptionSequence]]
      firstIdentifier = checkedMaybe "closure loopback first source" (listToMaybe identifiers)
      secondIdentifier = checkedMaybe "closure loopback second source" (listToMaybe (drop 1 identifiers))
      subscribes = [checked "closure loopback child subscribe" (AlignmentProtocol.alignmentSubscribe obligation identifier storeB proof) | identifier <- identifiers]
      childState = replaceStartupAlignmentState childAlignment primed
      applyRemote state subscribe = fst <$> either (assertFailure . show) pure (applyAlignmentTransferControl binding (AlignmentSubscribeRequested subscribe) state)
  admitted <- foldM applyRemote childState subscribes
  (parked, _, _) <- assertInputClosureReference "local barrier sources await their route" admitted
  let settled = replaceTransfer (commit Transfer.commitLocalRoutePrefixSettlement "settle closure loopback prefix" . Transfer.prepareLocalRoutePrefixSettlement childId local) parked
      marker = checked "closure loopback route marker" (alignmentRouteCutoverMarker (Plan.alignmentPlanIdFromClaimedCoordinates typedSort (deriveTopologyCutId secondTopology) placements) childId remote local)
      ready = replaceStartupAlignmentState (checked "retain closure loopback marker" (retainAlignmentRouteCutoverMarker remote local marker (startupAlignmentState settled))) settled
      released identifier state = Transfer.sourceSubscriptionLiveReleased (checkedMaybe "closure loopback retained source" (Transfer.lookupSourceSubscription identifier (transferOf state)))
      liveIdentifiers effects = [AlignmentProtocol.alignmentLiveSubscriptionId live | SendPeerControl _ (PeerAlignmentControl (AlignmentLiveAdvertised live)) <- effectBatchMembers effects]
  assertEqual "the local ordinary input is available before barrier replay" (Set.singleton owner) (Alignment.requiredPredecessorInputOwnersAt (predecessorId, storeB) (startupAlignmentState ready))
  assertEqual "no barrier was witnessed ahead of the actual Subscribe replay" Nothing (Transfer.lookupPredecessorInputClosure childId owner (transferOf ready))
  (firstPass, firstEffects, firstWork) <- assertInputClosureReference "synchronous closure Live first pass" ready
  assertBool "local Subscribe really produced the retained Live witness" (case Transfer.lookupPredecessorInputClosure childId owner (transferOf firstPass) of Just (subscribe, Just _) -> AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe == ordinaryIdentifier; _ -> False)
  assertBool "the later source consumes that witness in the same pass" (released secondIdentifier firstPass)
  assertBool "the already consumed source keeps its re-entrant wake" (not (released firstIdentifier firstPass))
  assertEqual "the earlier source is the only deferred closure" (Set.singleton firstIdentifier) (Transfer.pendingInputClosureSourceIds (transferOf firstPass))
  assertEqual "only the later source advertises Live on the first pass" [secondIdentifier] (liveIdentifiers firstEffects)
  assertEqual "local barrier replay is real semantic work" AlignmentTransferWorkChanged firstWork
  (secondPass, secondEffects, secondWork) <- assertInputClosureReference "synchronous closure Live second pass" firstPass
  assertBool "the next pass completes the deferred source" (released firstIdentifier secondPass)
  assertEqual "only the earlier source advertises Live on the second pass" [firstIdentifier] (liveIdentifiers secondEffects)
  assertEqual "the deferred closure contributes one more semantic pass" AlignmentTransferWorkChanged secondWork
  assertEqual "both completed sources leave the work inventory" Set.empty (Transfer.pendingInputClosureSourceIds (transferOf secondPass))
  (quiet, quietEffects, quietWork) <- assertInputClosureReference "synchronous closure Live quiescence" secondPass
  assertBool "all synchronous closure work quiesces" (quiet == secondPass)
  assertEqual "quiescence sends no duplicate controls" [] (effectBatchMembers quietEffects)
  assertEqual "quiescence reports no semantic work" AlignmentTransferWorkUnchanged quietWork

assertInputClosureReference :: String -> HeraldState -> IO (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
assertInputClosureReference context predecessor = do
  sparse@(successor, effects, disposition) <- either (assertFailure . ((context <> ": ") <>) . show) pure (advancePredecessorInputClosures predecessor)
  (reference, referenceEffects, referenceDisposition) <- either (assertFailure . ((context <> " exhaustive: ") <>) . show) pure (advancePredecessorInputClosuresExhaustive predecessor)
  assertBool (context <> " preserves the independently scanned semantic state") (sameAlignmentTransferWorkState successor reference)
  assertEqual (context <> " preserves exact ordered effects") (effectBatchMembers referenceEffects) (effectBatchMembers effects)
  assertEqual (context <> " preserves actual work disposition") referenceDisposition disposition
  forM_ [successor, reference] $ \state -> do
    assertEqual (context <> " validates expected owner and Transfer indexes independently") (Right ()) (Alignment.validateAlignmentState (startupAlignmentState state))
    assertEqual (context <> " validates exact source admission facts") (Right ()) (Transfer.validateAlignmentTransferState (Alignment.alignmentTransferState (startupAlignmentState state)))
  pure sparse

caseInputClosureExpectedOwnerIndex :: Assertion
caseInputClosureExpectedOwnerIndex = do
  plan <- case prepareAlignmentGenerationPlan typedSort topology placement oneWayContextGraph generationMembers [] [] Set.empty of
    Right (AlignmentGenerationReady ready) -> pure ready
    result -> assertFailure ("expected-owner fixture plan: " <> show result)
  let promote consequence state = commit Alignment.commitAlignmentPromotion "expected-owner promotion" (Alignment.prepareAlignmentPromotion heraldB consequence typedSort topologyId placement plan (retainCauseDebt consequence state))
      initial = promote (cause occurrence) Alignment.emptyState
      clean = Alignment.clearInputClosureChanges initial
      promoted = promote (cause successorOccurrence) clean
      closing = Alignment.alignmentClosingObligationIds promoted
      project state = Map.fromListWith Set.union [(target, Set.singleton identifier) | (identifier, obligation) <- Alignment.activeAlignmentObligationEntries state <> Alignment.alignmentClosedObligationEntries state, target <- targets obligation]
      targets obligation = [(AlignmentProtocol.alignmentObligationDestinationGeneration obligation, AlignmentProtocol.destinationStoreIncarnation destination) | destination <- NonEmpty.toList (AlignmentProtocol.alignmentObligationDestinationStores obligation)]
      checkIndex state = do
        assertEqual "owner index remains independently valid" (Right ()) (Alignment.validateAlignmentState state)
        forM_ (Map.toAscList (project promoted <> project state)) $ \(target, _) ->
          assertEqual "target owner query equals active union closed" (Map.findWithDefault Set.empty target (project state)) (Alignment.requiredPredecessorInputOwnersAt target state)
  assertBool "fixture creates a real ordinary owner" (not (null (Alignment.activeAlignmentObligationEntries initial)))
  assertBool "the second cause closes a prior ordinary owner" (not (null closing))
  let (promotedChanges, consumed) = Alignment.takeInputClosureChanges promoted
      expectedTargets = Set.fromList (concatMap (targets . snd) (Alignment.activeAlignmentObligationEntries promoted))
  assertEqual "new and closing logical owners notify exact targets" expectedTargets promotedChanges.changedTargets
  closed <- foldM (\state identifier -> pure (commit Alignment.commitAlignmentObligationClosureCompletion "complete expected-owner closure" (Alignment.prepareAlignmentObligationClosureCompletion identifier state))) consumed closing
  assertEqual "active to closed keeps the complete target inventory" (project promoted) (project closed)
  let (closedChanges, closedClean) = Alignment.takeInputClosureChanges closed
  assertBool "closure completion notifies affected target facts" (not (Set.null closedChanges.changedTargets))
  obligation <- case Alignment.activeAlignmentObligationEntries closedClean of
    (_, value) : _ -> pure value
    [] -> assertFailure "expected-owner fixture requires remaining active work"
  let generation = AlignmentProtocol.alignmentObligationDestinationGeneration obligation
      destination = NonEmpty.head (AlignmentProtocol.alignmentObligationDestinationStores obligation)
      invalidated = commit Alignment.commitAlignmentPlanInvalidation "invalidate current expected owners" (Alignment.prepareAlignmentPlanInvalidation (Alignment.AlignmentPlanDestinationStoreLost generation destination) closedClean)
      (invalidatedChanges, _) = Alignment.takeInputClosureChanges invalidated
  assertBool "plan invalidation removes active logical work" (null (Alignment.activeAlignmentObligationEntries invalidated))
  assertBool "closed owners remain authoritative after plan invalidation" (not (null (Alignment.alignmentClosedObligationEntries invalidated)))
  assertBool "plan validity notifies its immutable generations" (Set.member generation invalidatedChanges.invalidatedGenerations)
  forM_ [Alignment.emptyState, initial, clean, promoted, consumed, closed, closedClean, invalidated] checkIndex

assertClosureSourceAdmissionFacts :: HeraldState -> Assertion
assertClosureSourceAdmissionFacts state = do
  assertMarkerAdmissionFacts alignment
  forM_ (predecessorClosureSourceSubscriptions state) $ \subscribe -> do
    let obligation = AlignmentProtocol.alignmentSubscribeObligation subscribe
        generationId = AlignmentProtocol.alignmentObligationDestinationGeneration obligation
        predecessorId = AlignmentProtocol.alignmentObligationSourceGeneration obligation
        sourceStore = AlignmentProtocol.alignmentSubscribeSourceStoreIncarnation subscribe
        predecessor = checkedMaybe "closure source predecessor remains retained" (Alignment.lookupAlignmentGeneration predecessorId alignment)
    assertBool
      "admitted predecessor bootstrap retains local successor acceptance"
      (Alignment.lookupAlignmentCutAcceptance generationId local alignment /= Nothing)
    assertEqual
      "admitted predecessor source Store keeps its immutable local owner"
      (Just local)
      (alignmentMemberHerald <$> find ((== sourceStore) . alignmentMemberStoreIncarnation) (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut predecessor))))
  where
    alignment = startupAlignmentState state
    local = checkedLocalHeraldEpoch (startupGenesis state)

predecessorClosureSourceSubscriptions :: HeraldState -> [AlignmentProtocol.AlignmentSubscribe]
predecessorClosureSourceSubscriptions state =
  [ subscribe
  | (_, source) <- Transfer.sourceSubscriptionEntries (Alignment.alignmentTransferState (startupAlignmentState state)),
    let subscribe = Transfer.sourceSubscriptionSubscribe source,
    AlignmentProtocol.BootstrapProof generation _ <- [AlignmentProtocol.alignmentSubscribeSourceProof subscribe],
    AlignmentProtocol.alignmentObligationSourceGeneration (AlignmentProtocol.alignmentSubscribeObligation subscribe) /= generation
  ]

-- Observe a checked transfer history at every prefix, including a barrier
-- inserted before or after Live, exact retransmission, and receipt-preserving
-- cancellation. The reference intentionally reconstructs both complete maps.
propInputClosureWitnessQueriesReference :: Property
propInputClosureWitnessQueriesReference =
  forAll (chooseInt (0, 4)) $ \barrierPosition ->
    forAll (chooseInt (1, 4)) $ \replays -> ioProperty $ do
      (alignment, generation) <- successorGenerationFixture
      predecessorId <- case alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation) of
        [identifier] -> pure identifier
        identifiers -> assertFailure ("witness query fixture requires one predecessor, got " <> show identifiers)
      let generationId = alignmentGenerationId generation
          subscription = AlignmentProtocol.alignmentSubscriptionId heraldA AlignmentProtocol.firstAlignmentSubscriptionSequence
          owner = AlignmentProtocol.alignmentObligationId heraldA AlignmentProtocol.firstAlignmentObligationSequence
          missingOwner = AlignmentProtocol.alignmentObligationId heraldA (AlignmentProtocol.nextAlignmentObligationSequence AlignmentProtocol.firstAlignmentObligationSequence)
          obligation =
            checked
              "witness query ordinary obligation"
              ( AlignmentProtocol.alignmentObligation
                  owner
                  (cause occurrence)
                  (sortOccurrenceSortId typedSort)
                  (sortOccurrenceDefinition typedSort)
                  predecessorId
                  (destinationStore deltaA storeA :| [])
                  predecessorId
                  Route.Normal
              )
          certificate = historicalCertificate predecessorId storeB (deriveMemberReadyEvidenceDigest "witness reference certificate") (deriveBootstrapEvidenceDigest "witness reference bootstrap") initialStoreRevision
          attempt = checked "witness query destination attempt" (AlignmentProtocol.alignmentAttempt obligation subscription heraldB certificate)
          initial = commit Transfer.commitDestinationAttempt "witness query destination admission" (Transfer.prepareDestinationAttempt attempt Transfer.emptyState)
          digest = AlignmentProtocol.alignmentSemanticSnapshotDigest predecessorId storeB initialStoreRevision []
          retain prepared state = fst (Transfer.commitDestinationWork (checked "witness query destination progress" (prepared state)))
          frames =
            [ retain (Transfer.prepareDestinationSnapshotStart (AlignmentProtocol.alignmentSnapshotStart subscription initialStoreRevision digest 1)),
              retain (Transfer.prepareDestinationSnapshotChunk (AlignmentProtocol.alignmentSnapshotChunk subscription 0 [])),
              retain (Transfer.prepareDestinationSnapshotEnd (AlignmentProtocol.alignmentSnapshotEnd subscription initialStoreRevision digest)),
              retain (Transfer.prepareDestinationLive (AlignmentProtocol.alignmentLive subscription initialStoreRevision))
            ]
          barrier state =
            commit
              Transfer.commitPredecessorInputClosures
              "witness query barrier admission"
              (Transfer.preparePredecessorInputClosures generationId (Set.singleton subscription) state)
          localPrefix state =
            commit
              Transfer.commitLocalRoutePrefixSettlement
              "witness query local settlement"
              (Transfer.prepareLocalRoutePrefixSettlement generationId heraldA state)
          actions =
            take barrierPosition frames
              <> [barrier]
              <> drop barrierPosition frames
              <> concat (replicate replays [last frames, barrier, localPrefix])
          histories = scanl (flip ($)) initial actions
          complete = last histories
          prefixes = checkedMaybe "witness reference completed prefixes" (referenceInputClosureWitnesses generationId (Set.singleton owner) complete)
          receipt =
            commit
              Transfer.commitPredecessorInputClosureReceipt
              "witness query receipt"
              (Transfer.preparePredecessorInputClosureReceipt generationId heraldA storeA initialStoreRevision prefixes complete)
          cancel state =
            commit
              Transfer.commitAlignmentCancellation
              "witness query terminal cancellation"
              (Transfer.prepareAlignmentCancellation (AlignmentProtocol.alignmentCancel subscription AlignmentProtocol.AlignmentRelationRemoved) state)
          terminalHistories = take (replays + 1) (iterate cancel receipt)
      assertEqual "fixture retains both real generation identities" 2 (length (Alignment.alignmentGenerationEntries alignment))
      forM_ (Transfer.emptyState : histories <> terminalHistories) $ \state -> do
        assertEqual "witness query history preserves transfer invariants" (Right ()) (Transfer.validateAlignmentTransferState state)
        forM_ [predecessorId, generationId] $ \identifier -> do
          forM_ [Set.empty, Set.singleton owner, Set.singleton missingOwner, Set.fromList [owner, missingOwner]] $ \owners ->
            assertEqual
              "direct witness lookups equal the frozen reconstructed-map reference"
              (referenceInputClosureWitnesses identifier owners state)
              (completeInputClosureWitnesses identifier owners state)
          forM_ [owner, missingOwner] $ \queriedOwner ->
            assertEqual
              "barrier keyed lookup equals retained-entry lookup"
              (lookup (identifier, queriedOwner) (Transfer.predecessorInputClosureEntries state))
              (Transfer.lookupPredecessorInputClosure identifier queriedOwner state)
          forM_ [heraldA, heraldB] $ \predecessor ->
            assertEqual
              "local-prefix membership equals retained-entry membership"
              ((identifier, predecessor) `elem` Transfer.localRoutePrefixSettlementEntries state)
              (Transfer.localRoutePrefixSettled identifier predecessor state)
      assertEqual "missing barrier is not a complete witness" Nothing (completeInputClosureWitnesses generationId (Set.singleton owner) initial)
      assertEqual "completed terminal owner keeps its exact witness" (Just prefixes) (completeInputClosureWitnesses generationId (Set.singleton owner) (last terminalHistories))
      pure True

referenceInputClosureWitnesses ::
  ContextClassGenerationId ->
  Set.Set AlignmentProtocol.AlignmentObligationId ->
  Transfer.State ->
  Maybe [Transfer.MemberReadinessPrefix]
referenceInputClosureWitnesses generation expectedOwners transfer =
  traverse completed (Set.toAscList expectedOwners)
  where
    barriers = Map.fromList (Transfer.predecessorInputClosureEntries transfer)
    destinations = Map.fromList (Transfer.destinationSubscriptionEntries transfer)
    completed owner = do
      (subscribe, witnessed) <- Map.lookup (generation, owner) barriers
      through <- witnessed
      destination <- Map.lookup (AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe) destinations
      applied <- Transfer.destinationSubscriptionAppliedThrough destination
      live <- Transfer.destinationSubscriptionLiveThrough destination
      acknowledged <- Transfer.destinationSubscriptionAcknowledgedThrough destination
      guard (applied >= through && live >= through && acknowledged >= through)
      pure (Transfer.memberReadinessPrefix (AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe) applied through acknowledged)

caseRouteCutoverAdmission :: Assertion
caseRouteCutoverAdmission = do
  (retained, generation) <- successorGenerationFixture
  let generationId = alignmentGenerationId generation
      sourceAcceptance =
        alignmentCutAccepted
          generationId
          heraldB
          successorTopologyId
          placement
          EmptyHeraldPublicationPrefix
      accepted =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain route-cutover source acceptance"
          (Alignment.prepareAlignmentCutAcceptance sourceAcceptance retained)
      marker =
        checked
          "route cutover marker"
          (alignmentRouteCutoverMarker (Plan.alignmentGenerationBirthPlanId generation) generationId heraldB heraldA)
  assertEqual
    "the marker waits for the source's exact cut acceptance"
    (Left (AlignmentRouteCutoverSourceAcceptanceMissing generationId heraldB))
    (retainAlignmentRouteCutoverMarker heraldB heraldA marker retained)
  admitted <- case retainAlignmentRouteCutoverMarker heraldB heraldA marker accepted of
    Left problem -> assertFailure ("valid route cutover was rejected: " <> show problem)
    Right successor -> pure successor
  assertEqual
    "exact replay is idempotent"
    (Right admitted)
    (retainAlignmentRouteCutoverMarker heraldB heraldA marker admitted)
  assertEqual
    "the authenticated remote must be the marker source"
    (Left (AlignmentRouteCutoverSourceMismatch heraldA heraldB))
    (retainAlignmentRouteCutoverMarker heraldA heraldA marker accepted)
  assertEqual
    "the local Herald must be the marker predecessor"
    (Left (AlignmentRouteCutoverPredecessorMismatch heraldB heraldA))
    (retainAlignmentRouteCutoverMarker heraldB heraldB marker accepted)

  let missingGeneration =
        checked
          "missing generation identifier"
          (mkContextClassGenerationId (fixtureIdentifierBytes 0xba))
      missingGenerationMarker =
        checked
          "missing generation marker"
          (alignmentRouteCutoverMarker (Plan.alignmentGenerationBirthPlanId generation) missingGeneration heraldB heraldA)
  assertEqual
    "the new generation must already be retained"
    (Left (AlignmentRouteCutoverGenerationMissing missingGeneration))
    ( retainAlignmentRouteCutoverMarker
        heraldB
        heraldA
        missingGenerationMarker
        accepted
    )

  let missingMemberMarker =
        checked
          "missing predecessor member marker"
          (alignmentRouteCutoverMarker (Plan.alignmentGenerationBirthPlanId generation) generationId heraldB heraldC)
  assertEqual
    "the local predecessor must host a store in a frozen predecessor generation"
    ( Left
        ( AlignmentRouteCutoverPredecessorStoreMissing
            generationId
            heraldC
        )
    )
    ( retainAlignmentRouteCutoverMarker
        heraldB
        heraldC
        missingMemberMarker
        accepted
    )

caseRouteCutoverRetirementWork :: Assertion
caseRouteCutoverRetirementWork = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, _, _) <- routeProjectionAlignmentFixtureFrom True local remote
  let initial = initializedWithAlignment alignment
  (settled, _, _) <- checkedRouteCutoverPass "establish every cutover before candidate retirement" initial
  let settledTransfer = Alignment.alignmentTransferState (startupAlignmentState settled)
      -- Install the admitted transfer successor into the pre-retirement owner.
      -- Its completed markers/assignments remain coherent with the settled peer
      -- stream; only the conservative pending-candidate index needs cleanup.
      retirementOnly =
        replaceStartupAlignmentState
          (Alignment.replaceAlignmentTransferState settledTransfer alignment)
          settled
  assertBool
    "the staged owner still contains completed cutover candidates"
    (not (null (Alignment.pendingAlignmentRouteCutoverEntries local (startupAlignmentState retirementOnly))))
  assertEqual
    "the staged owner preserves all cutover relationships"
    (Right ())
    (validateAlignmentTransferCoordinatorRelationships retirementOnly)
  assertEqual
    "the staged owner preserves the complete checked alignment invariant"
    (Right ())
    (Alignment.validateAlignmentState (startupAlignmentState retirementOnly))
  (retired, effects, disposition) <- checkedRouteCutoverPass "retire completed candidates only" retirementOnly
  assertEqual "candidate retirement contributes exact changed work" AlignmentTransferWorkChanged disposition
  assertBool "retirement changes the owner" (retired /= retirementOnly)
  assertBool "retirement returns the same completely settled owner" (retired == settled)
  assertEqual "candidate retirement emits no protocol effect" [] (effectBatchMembers effects)
  assertBool
    "candidate retirement leaves peer-stream assignments intact"
    (startupPeerStreamState retired == startupPeerStreamState retirementOnly)
  (replayed, replayEffects, replayDisposition) <- checkedRouteCutoverPass "replay candidate retirement" retired
  assertEqual "retirement replay reports no work" AlignmentTransferWorkUnchanged replayDisposition
  assertBool "retirement replay preserves the exact owner" (replayed == retired)
  assertEqual "retirement replay emits no effects" [] (effectBatchMembers replayEffects)

caseRetiredRouteCutoverProjection :: Assertion
caseRetiredRouteCutoverProjection = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ retired = fixtureRemoteMember
  (alignment, generation, incomingMarker) <-
    routeProjectionAlignmentFixture local retired
  let generationId = alignmentGenerationId generation
      outgoingKey = (generationId, local, retired)
      incomingKey = (generationId, retired, local)
      active = initializedWithAlignment alignment
  assertEqual
    "the active fixture has coherent route-cutover relationships"
    (Right ())
    (validateAlignmentTransferCoordinatorRelationships active)
  (activeAdvanced, _, activeDisposition) <-
    checkedRouteCutoverPass "active route-cutover eligibility" active
  let activeAdvancedAlignment = startupAlignmentState activeAdvanced
      activeAdvancedRoutes =
        Transfer.routeCutoverEntries
          (Alignment.alignmentTransferState activeAdvancedAlignment)
  assertEqual
    "active late eligibility reports exact retained work"
    AlignmentTransferWorkChanged
    activeDisposition
  assertBool
    ( "the active predecessor is initially eligible for one outgoing cutover: routes="
        <> show (fmap fst activeAdvancedRoutes)
        <> ", invalidations="
        <> show (fmap fst (Alignment.alignmentPlanInvalidationEntries activeAdvancedAlignment))
    )
    ( outgoingKey
        `elem` fmap
          fst
          activeAdvancedRoutes
    )

  (activeReplayed, activeReplayEffects, activeReplayDisposition) <-
    checkedRouteCutoverPass "retained active route-cutover replay" activeAdvanced
  assertEqual
    "retained active cutovers produce no further owner work"
    AlignmentTransferWorkUnchanged
    activeReplayDisposition
  assertBool "retained active cutovers preserve the owner" (activeReplayed == activeAdvanced)
  assertEqual "retained active cutovers emit no effects" [] (effectBatchMembers activeReplayEffects)
  assertEqual
    "full route-cutover audit still includes retained active evidence"
    (Right ())
    (validateAlignmentTransferCoordinatorRelationships activeReplayed)

  let preparedPeerRetirement =
        checked
          "retire the route-cutover destination stream"
          (PeerStream.preparePeerRetirement retired (startupPeerStreamState activeAdvanced))
  assertBool
    "peer retirement records the exact active cutover assignment"
    (not (null (PeerStream.preparedPeerRetirementSettledAssignments preparedPeerRetirement)))
  let (retiredStream, _) = PeerStream.commitPeerRetirement preparedPeerRetirement
      withHistoricalAssignment =
        replaceStartupPeerStreamState retiredStream
          . retireOracleMember retired
          $ activeAdvanced
  (retainedHistory, retainedHistoryEffects, retainedHistoryDisposition) <-
    checkedRouteCutoverPass
      "retained route-cutover assignment after retirement"
      withHistoricalAssignment
  assertBool
    "historical assignments and retirement receipts remain byte-for-byte retained"
    (retainedHistory == withHistoricalAssignment)
  assertEqual
    "retained historical assignments are no longer current work"
    AlignmentTransferWorkUnchanged
    retainedHistoryDisposition
  assertEqual
    "retained historical assignments emit no effects"
    []
    (effectBatchMembers retainedHistoryEffects)

  let afterRetirement = retireOracleMember retired active
      predecessorStream = startupPeerStreamState afterRetirement
  assertEqual
    "retirement makes the historical inactive marker irrelevant to current relationships"
    (Right ())
    (validateAlignmentTransferCoordinatorRelationships afterRetirement)
  (projected, projectedEffects, projectedDisposition) <-
    checkedRouteCutoverPass "retired route-cutover projection" afterRetirement
  let projectedTransfer =
        Alignment.alignmentTransferState (startupAlignmentState projected)
      projectedKeys = fmap fst (Transfer.routeCutoverEntries projectedTransfer)
  assertBool
    "the retired source's historical marker remains retained"
    (incomingKey `elem` projectedKeys)
  assertBool
    "late eligibility cannot create a fresh cutover to the retired predecessor"
    (outgoingKey `notElem` projectedKeys)
  assertBool
    "the projected cutover pass cannot enqueue work to the retired peer"
    (startupPeerStreamState projected == predecessorStream)
  assertEqual
    "retirement leaves no current cutover work"
    AlignmentTransferWorkUnchanged
    projectedDisposition
  assertBool
    "retirement projection is state-identical"
    (projected == afterRetirement)
  assertEqual
    "retirement projection is effect-free"
    []
    (effectBatchMembers projectedEffects)

  (replayed, replayEffects, replayDisposition) <-
    checkedRouteCutoverPass "retired route-cutover replay" projected
  assertBool
    "route-cutover replay still retains the historical marker"
    ( incomingMarker
        `elem` fmap
          snd
          ( Transfer.routeCutoverEntries
              (Alignment.alignmentTransferState (startupAlignmentState replayed))
          )
    )
  assertBool
    "route-cutover replay never synthesizes the retired destination assignment"
    ( outgoingKey
        `notElem` fmap
          fst
          ( Transfer.routeCutoverEntries
              (Alignment.alignmentTransferState (startupAlignmentState replayed))
          )
    )
  assertBool
    "route-cutover replay leaves the peer stream untouched"
    (startupPeerStreamState replayed == predecessorStream)
  assertEqual
    "route-cutover replay is unchanged"
    AlignmentTransferWorkUnchanged
    replayDisposition
  assertBool "route-cutover replay is state-identical" (replayed == projected)
  assertEqual
    "route-cutover replay is effect-free"
    []
    (effectBatchMembers replayEffects)
  forM_
    [active, activeAdvanced, activeReplayed, withHistoricalAssignment, retainedHistory, afterRetirement, projected, replayed]
    assertCurrentClosureRouteQueries

caseLocalReadyAllocation :: Assertion
caseLocalReadyAllocation = do
  (promoted, generation) <- promotedFixture
  let generationId = alignmentGenerationId generation
      digest = deriveMemberReadyEvidenceDigest "local-ready-prefix"
      firstPrepared =
        checked
          "prepare first local member readiness"
          ( Alignment.prepareLocalClassMemberReady
              generationId
              storeA
              digest
              initialStoreRevision
              promoted
          )
      firstReady = Alignment.preparedLocalClassMemberReady firstPrepared
      (retained, firstDisposition) =
        Alignment.commitLocalClassMemberReady firstPrepared
      retryPrepared =
        checked
          "prepare duplicate local member readiness"
          ( Alignment.prepareLocalClassMemberReady
              generationId
              storeA
              digest
              initialStoreRevision
              retained
          )
      (replayed, retryDisposition) =
        Alignment.commitLocalClassMemberReady retryPrepared
  assertEqual
    "the first owner-local sequence is positive one"
    1
    ( alignmentEvidenceSequenceWord64
        (classMemberReadyEvidenceSequence firstReady)
    )
  assertEqual
    "first allocation is retained"
    Alignment.AlignmentReadinessRetained
    firstDisposition
  assertEqual
    "exact retry returns the same immutable evidence"
    firstReady
    (Alignment.preparedLocalClassMemberReady retryPrepared)
  assertEqual
    "exact retry consumes no sequence"
    Alignment.AlignmentReadinessUnchanged
    retryDisposition
  assertEqual "exact retry is state-idempotent" retained replayed
  case Alignment.prepareLocalClassMemberReady
    generationId
    storeA
    (deriveMemberReadyEvidenceDigest "changed-prefix")
    initialStoreRevision
    retained of
    Left problem ->
      assertEqual
        "changing the frozen prefix conflicts"
        (Alignment.AlignmentReadinessConflict generationId storeA)
        problem
    Right _ -> assertFailure "changed local readiness was accepted"
  assertEqual
    "owner-local evidence sequence invariant"
    (Right ())
    (Alignment.validateAlignmentState retained)

casePendingGenerationEvidence :: Assertion
casePendingGenerationEvidence = do
  let generationId =
        checked
          "ahead generation identifier"
          (mkContextClassGenerationId (fixtureIdentifierBytes 0xbc))
      accepted =
        alignmentCutAccepted
          generationId
          heraldB
          topologyId
          placement
          EmptyHeraldPublicationPrefix
      control = AlignmentCutAcceptanceAdvertised accepted
      firstPrepared =
        checked
          "retain ahead acceptance"
          ( Alignment.preparePendingGenerationEvidence
              heraldB
              control
              Alignment.emptyState
          )
      (retained, firstDisposition) =
        Alignment.commitPendingGenerationEvidence firstPrepared
      replayPrepared =
        checked
          "replay ahead acceptance"
          ( Alignment.preparePendingGenerationEvidence
              heraldB
              control
              retained
          )
      (_, replayDisposition) =
        Alignment.commitPendingGenerationEvidence replayPrepared
  assertEqual
    "first ahead evidence is retained"
    Alignment.PendingGenerationEvidenceRetained
    firstDisposition
  assertEqual
    "equal ahead evidence is idempotent"
    Alignment.PendingGenerationEvidenceUnchanged
    replayDisposition
  assertEqual
    "one exact authenticated message is retained"
    [(heraldB, control)]
    [ ( Alignment.pendingGenerationEvidenceRemoteHerald evidence,
        Alignment.pendingGenerationEvidenceControl evidence
      )
    | evidence <- Alignment.pendingGenerationEvidenceEntries retained
    ]
  let conflicting =
        AlignmentCutAcceptanceAdvertised
          ( alignmentCutAccepted
              generationId
              heraldB
              predecessorCut
              placement
              EmptyHeraldPublicationPrefix
          )
  case Alignment.preparePendingGenerationEvidence
    heraldB
    conflicting
    retained of
    Left problem ->
      assertEqual
        "unequal evidence at the immutable coordinate conflicts"
        Alignment.PendingGenerationEvidenceConflict
        problem
    Right _ -> assertFailure "conflicting pending evidence was accepted"
  let removedPrepared =
        checked
          "remove admitted pending evidence"
          ( Alignment.preparePendingGenerationEvidenceRemoval
              heraldB
              control
              retained
          )
      (removed, removalDisposition) =
        Alignment.commitPendingGenerationEvidence removedPrepared
  assertEqual
    "exact removal is classified"
    Alignment.PendingGenerationEvidenceRemoved
    removalDisposition
  assertEqual
    "exact removal leaves no pending evidence"
    []
    (Alignment.pendingGenerationEvidenceEntries removed)
  let survivorControl =
        AlignmentCutAcceptanceAdvertised
          ( alignmentCutAccepted
              generationId
              heraldC
              topologyId
              placement
              EmptyHeraldPublicationPrefix
          )
      survivorPrepared =
        checked
          "retain survivor-authored ahead acceptance"
          ( Alignment.preparePendingGenerationEvidence
              heraldC
              survivorControl
              retained
          )
      (mixed, _) = Alignment.commitPendingGenerationEvidence survivorPrepared
      preparedRetirement =
        Alignment.preparePendingGenerationEvidenceRetirement heraldB mixed
      expectedRemoved = [(heraldB, control)]
      evidenceCoordinates state =
        [ ( Alignment.pendingGenerationEvidenceRemoteHerald evidence,
            Alignment.pendingGenerationEvidenceControl evidence
          )
        | evidence <- Alignment.pendingGenerationEvidenceEntries state
        ]
      removedCoordinates evidence =
        [ ( Alignment.pendingGenerationEvidenceRemoteHerald entry,
            Alignment.pendingGenerationEvidenceControl entry
          )
        | entry <- evidence
        ]
      (afterRetirement, committedRemoved) =
        Alignment.commitPendingGenerationEvidenceRetirement preparedRetirement
  assertEqual
    "retirement preparation names exactly the retired peer's unactioned evidence"
    expectedRemoved
    ( removedCoordinates
        (Alignment.preparedPendingGenerationEvidenceRetirementRemoved preparedRetirement)
    )
  assertEqual
    "retirement commits the same exact removal receipt"
    expectedRemoved
    (removedCoordinates committedRemoved)
  assertEqual
    "survivor-authored pending evidence remains retryable"
    [(heraldC, survivorControl)]
    (evidenceCoordinates afterRetirement)
  let replayRetirement =
        Alignment.preparePendingGenerationEvidenceRetirement heraldB afterRetirement
      (replayedRetirement, replayRemoved) =
        Alignment.commitPendingGenerationEvidenceRetirement replayRetirement
  assertBool
    "pending-evidence retirement replay is state-identical"
    (replayedRetirement == afterRetirement)
  assertEqual
    "pending-evidence retirement replay removes nothing"
    []
    replayRemoved

propPromotionPreflightMatchesPostPlanAuthorization :: Property
propPromotionPreflightMatchesPostPlanAuthorization =
  forAll (shuffle [heraldA, heraldB, heraldC]) $ \presentation ->
    case presentation of
      [] -> counterexample "shuffle lost every fixed member" (False === True)
      first : remaining ->
        let fixedMembers = first :| remaining
            activePlan =
              checkedReadyPlan
                "active preflight plan"
                twoClassContextGraph
                twoClassMembers
            removalOnlyPlan =
              checkedReadyPlan
                "removal-only preflight plan"
                emptyContextGraph
                []
            activeRoutesPresent = not (null twoClassMembers)
            removalOnlyRoutesPresent = False
            anchor =
              checkedMaybe
                "active preflight plan has no anchor"
                (alignmentPlanAnchorGeneration activePlan)
            anchorId = alignmentGenerationId anchor
            wrongAnchorId =
              checked
                "wrong preflight anchor"
                (mkContextClassGenerationId (fixtureIdentifierBytes 0xca))
            postSpontaneous local _ =
              local == minimum (NonEmpty.toList fixedMembers)
            postAnchor source announced plan =
              source == minimum (NonEmpty.toList fixedMembers)
                && maybe
                  False
                  ((== announced) . alignmentGenerationId)
                  (alignmentPlanAnchorGeneration plan)
            preflightThenAnchor source announced plan =
              anchorAlignmentPromotionMayDerive source fixedMembers
                && maybe
                  False
                  ((== announced) . alignmentGenerationId)
                  (alignmentPlanAnchorGeneration plan)
            spontaneousChecks local =
              [ counterexample "active-route spontaneous preflight"
                  $ spontaneousAlignmentPromotionMayDerive
                    local
                    fixedMembers
                    activeRoutesPresent
                    === postSpontaneous local activePlan,
                counterexample "active-plan spontaneous final policy"
                  $ spontaneousAlignmentPromotionAuthorized
                    local
                    fixedMembers
                    activePlan
                    === postSpontaneous local activePlan,
                counterexample "removal-only spontaneous preflight"
                  $ spontaneousAlignmentPromotionMayDerive
                    local
                    fixedMembers
                    removalOnlyRoutesPresent
                    === postSpontaneous local removalOnlyPlan,
                counterexample "removal-only spontaneous final policy"
                  $ spontaneousAlignmentPromotionAuthorized
                    local
                    fixedMembers
                    removalOnlyPlan
                    === postSpontaneous local removalOnlyPlan
              ]
            anchorChecks source announced plan =
              [ counterexample "anchor preflight followed by exact plan check"
                  $ preflightThenAnchor source announced plan
                    === postAnchor source announced plan,
                counterexample "anchor final policy"
                  $ anchorAlignmentPromotionAuthorized
                    source
                    fixedMembers
                    announced
                    plan
                    === postAnchor source announced plan
              ]
            routeShapeChecks =
              [ counterexample "active routes derive generations"
                  $ not (null (alignmentGenerationPlanGenerations activePlan))
                    === activeRoutesPresent,
                counterexample "active routes derive an anchor"
                  $ maybe False (const True) (alignmentPlanAnchorGeneration activePlan)
                    === activeRoutesPresent,
                counterexample "absent routes derive no generations"
                  $ not (null (alignmentGenerationPlanGenerations removalOnlyPlan))
                    === removalOnlyRoutesPresent,
                counterexample "absent routes derive no anchor"
                  $ maybe False (const True) (alignmentPlanAnchorGeneration removalOnlyPlan)
                    === removalOnlyRoutesPresent
              ]
         in counterexample ("fixed-member presentation: " <> show presentation)
              $ conjoin
                ( routeShapeChecks
                    <> concatMap spontaneousChecks presentation
                    <> concat
                      [ anchorChecks source announced plan
                      | source <- presentation,
                        announced <- [anchorId, wrongAnchorId],
                        plan <- [activePlan, removalOnlyPlan]
                      ]
                )
  where
    checkedReadyPlan context graph members =
      case prepareAlignmentGenerationPlan
        typedSort
        multiTopology
        multiPlacement
        graph
        members
        []
        []
        Set.empty of
        Right (AlignmentGenerationReady plan) -> plan
        result -> error (context <> ": " <> show result)

    emptyContextGraph =
      checked
        "derive empty preflight context graph"
        (deriveContextGraph [] [] [])

caseNonleastWholeHeraldPromotionPreflight :: Assertion
caseNonleastWholeHeraldPromotionPreflight = do
  let leastRaw = initializedAt fixtureLocalMember
      nonleastRaw = initializedAt fixtureRemoteMember
      leastInitial = advanceControl (installRemotePlacement nonleastRaw leastRaw)
      nonleastInitial = advanceControl (installRemotePlacement leastRaw nonleastRaw)
      remoteReport =
        GraphProgress.structuralLocalReport
          (startupStructuralProgressState nonleastInitial)
      leastReported =
        replaceStartupStructuralProgressState
          ( fst
              ( GraphProgress.commitStructuralReport
                  ( checked
                      "retain whole-Herald preflight remote report"
                      ( GraphProgress.prepareStructuralReport
                          remoteReport
                          (startupStructuralProgressState leastInitial)
                      )
                  )
              )
          )
          leastInitial
      preparedProposal =
        case checked
          "prepare whole-Herald preflight cut"
          ( GraphProgress.prepareTopologyCutProposal
              (reconciliationViews leastReported)
              checkpoint
              (startupStructuralProgressState leastReported)
          ) of
          GraphProgress.TopologyCutProposalReady prepared -> prepared
          GraphProgress.TopologyCutProposalHeld dependencies ->
            error
              ( "whole-Herald preflight cut dependencies were held: "
                  <> show dependencies
              )
          GraphProgress.TopologyCutProposalUnavailable ->
            error "whole-Herald preflight cut was unavailable"
      announce =
        GraphProgress.preparedTopologyCutProposalAnnounce preparedProposal
      leastAnnounced =
        replaceStartupStructuralProgressState
          (GraphProgress.commitTopologyCutProposal preparedProposal)
          leastReported
      remoteAcceptance =
        topologyCutAcceptance
          (topologyCutAnnounceId announce)
          remoteReport
      leastAcceptedProgress =
        GraphProgress.commitTopologyCutAcceptance
          ( checked
              "accept whole-Herald preflight cut"
              ( GraphProgress.prepareTopologyCutAcceptance
                  remoteAcceptance
                  (startupStructuralProgressState leastAnnounced)
              )
          )
      preparedEstablishment =
        checked
          "establish whole-Herald preflight cut"
          (GraphProgress.prepareTopologyCutEstablishment leastAcceptedProgress)
      established =
        GraphProgress.preparedTopologyCutEstablished preparedEstablishment
      leastInstalled =
        replaceStartupStructuralProgressState
          (GraphProgress.commitTopologyCutEstablishment preparedEstablishment)
          leastAnnounced
      nonleastInstalled =
        replaceStartupStructuralProgressState
          ( GraphProgress.commitTopologyCutEstablished
              ( checked
                  "install whole-Herald preflight cut on nonleast"
                  ( GraphProgress.prepareTopologyCutEstablished
                      (reconciliationViews nonleastInitial)
                      checkpoint
                      established
                      (startupStructuralProgressState nonleastInitial)
                  )
              )
          )
          nonleastInitial
      (affectedSort, least, leastSuccessor, leastDisposition) =
        selectPromotableSort leastInstalled
      nonleast = withDueDebt affectedSort nonleastInstalled
  assertEqual
    "the proposal waits for the remote cut acceptance"
    ( GraphProgress.structuralLastInstalledCutId
        (startupStructuralProgressState leastReported)
    )
    ( GraphProgress.structuralLastInstalledCutId
        (startupStructuralProgressState leastAnnounced)
    )
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (connected, binding) <- connectedWithAlignment (startupAlignmentState least)
  let connectedLeast = replaceStartupDiscoveryState (startupDiscoveryState connected) least
  (scopedSuccessor, scopedEffects, _) <- advance "scoped real promotion" connectedLeast
  assertEqual
    "real fixed-point promotion retains the same owner under a current binding"
    (startupAlignmentState leastSuccessor)
    (startupAlignmentState scopedSuccessor)
  assertScopedGenerationEffects
    "real fixed-point promotion"
    local
    remote
    binding
    connectedLeast
    scopedSuccessor
    scopedEffects
    []
  assertBool
    "real promotion comparison exercises nonempty fanout"
    (not (null (effectBatchMembers scopedEffects)))
  anchorControl <- case [ control
                        | SendPeerControl _ (PeerAlignmentControl control@(AlignmentPlanAnnounced scopedAnnounce)) <- effectBatchMembers scopedEffects,
                          isJust (Alignment.lookupAlignmentPlan (AlignmentProtocol.alignmentPlanAnnounceId scopedAnnounce) (startupAlignmentState scopedSuccessor))
                        ] of
    control : _ -> pure control
    [] -> assertFailure "real promotion did not emit an anchor"
  (nonleastConnected, reverseBinding) <-
    connectedWithAlignmentAt
      fixtureRemoteMember
      fixtureLocalMember
      (startupAlignmentState nonleast)
  let receiving = replaceStartupDiscoveryState (startupDiscoveryState nonleastConnected) nonleast
  (received, receivedEffects, _) <- case applyAlignmentGenerationControlWithDisposition reverseBinding anchorControl receiving of
    Left problem -> assertFailure ("scoped real anchor ingress: " <> show problem)
    Right Nothing -> assertFailure "real anchor ingress was not handled"
    Right (Just result) -> pure result
  assertScopedGenerationEffects
    "real anchor ingress"
    remote
    local
    reverseBinding
    receiving
    received
    receivedEffects
    []
  assertBool
    "real anchor ingress performs checked promotion"
    (not (null (Alignment.alignmentPromotionEntries (startupAlignmentState received))))
  (nonleastSuccessor, nonleastEffects, nonleastDisposition) <-
    advance "nonleast whole-Herald generation work" nonleast
  assertEqual
    "the fixed least fixture proves that active promotion work exists"
    AlignmentGenerationWorkChanged
    leastDisposition
  assertBool
    "the fixed least retains the selected generation plan"
    (leastSuccessor /= least)
  assertEqual
    "the nonleast preflight reports no semantic work"
    AlignmentGenerationWorkUnchanged
    nonleastDisposition
  let beforeAlignment = startupAlignmentState nonleast
      expectedAlignment = Alignment.consumeGenerationPlanWork (Set.singleton affectedSort) beforeAlignment
      expected = replaceStartupAlignmentState expectedAlignment (withConsumedGenerationInputChanges nonleast)
  assertEqual "the nonleast fixture has one newly dirty sort" (Set.singleton affectedSort) (Alignment.pendingGenerationPlanWork beforeAlignment)
  assertBool
    "the first blocked evaluation consumes only its exact plan work and physical input notifications"
    (nonleastSuccessor == expected)
  assertEqual "the blocked sort remains registered for later dependencies" (Set.singleton affectedSort) (Alignment.generationPlanWorkIds (startupAlignmentState nonleastSuccessor))
  assertEqual "the blocked generation work index remains valid" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState nonleastSuccessor))
  assertEqual
    "the nonleast preflight emits no effects"
    []
    (effectBatchMembers nonleastEffects)
  (nonleastRepeated, repeatedEffects, repeatedDisposition) <- advance "repeat parked nonleast generation work" nonleastSuccessor
  assertBool "a parked nonleast replay preserves the exact whole Herald" (nonleastRepeated == nonleastSuccessor)
  assertEqual "a parked nonleast replay emits no effects" [] (effectBatchMembers repeatedEffects)
  assertEqual "a parked nonleast replay has no semantic work" AlignmentGenerationWorkUnchanged repeatedDisposition
  where
    advance context state =
      case advanceAlignmentGenerationsWithDisposition state of
        Left problem -> assertFailure (context <> ": " <> show problem)
        Right result -> pure result

    checkpoint =
      GraphProgress.topologyControlCheckpointWithConsequence
        (controlIndex 1)
        True
        "whole-Herald-promotion-preflight"

    advanceControl state =
      replaceStartupStructuralProgressState successor state
      where
        successor =
          fst
            ( GraphProgress.commitStructuralControlProgress
                ( checked
                    "advance whole-Herald preflight control"
                    ( GraphProgress.prepareStructuralControlProgress
                        (controlIndex 1)
                        (startupStructuralProgressState state)
                    )
                )
            )

    installRemotePlacement source target =
      replaceStartupPlacementState remotePlacement target
      where
        (_, localResult) =
          Placement.commitLocalSnapshot
            ( checked
                "prepare retained whole-Herald preflight placement"
                ( Placement.prepareRetainedLocalSnapshot
                    (startupPlacementState source)
                )
            )
        (remotePlacement, _) =
          Placement.commitRemotePlacement
            ( checked
                "install remote whole-Herald preflight placement"
                ( Placement.prepareRemotePlacement
                    (FullPlacementSnapshot (Placement.localSnapshotMessage localResult))
                    (startupPlacementState target)
                )
            )

    initializedAt local =
      initialized
      where
        initialized =
          fst
            ( checked
                "initialize whole-Herald preflight fixture"
                ( initialHerald
                    (monotonicInstant 0)
                    genesis
                    initialBootstraps
                    fixtureOracleContacts
                    fixtureGeneratorSeed
                    fixtureApplicationRecoveryConfiguration
                    fixturePeerRecoveryConfiguration
                )
            )
        genesis =
          checked
            "check whole-Herald preflight genesis"
            (checkHeraldGenesis (fixtureDeploymentAt local))
        initialBootstraps =
          checked
            "check whole-Herald preflight bootstraps"
            ( checkInitialBootstraps
                genesis
                ( PrimordialProcessManifest
                    (take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId])
                )
            )

    selectPromotableSort state =
      case [ ( affectedSort,
               predecessor,
               successor,
               disposition
             )
           | affectedSort <- activeSorts state,
             let predecessor = withDueDebt affectedSort state,
             Right (successor, _, disposition) <-
               [advanceAlignmentGenerationsWithDisposition predecessor],
             disposition == AlignmentGenerationWorkChanged
           ] of
        selected : _ -> selected
        [] -> error "whole-Herald preflight fixture has no promotable active sort"

    activeSorts state =
      Set.toAscList
        ( Set.fromList
            [ sortOccurrence
                (deltaRouteSortId route)
                (deltaRouteOccurrenceId route)
            | (_, ownerRoutes) <- routesAtCurrentVector state,
              route <- ownerRoutes
            ]
        )

    routesAtCurrentVector state =
      checked
        "select whole-Herald preflight placement routes"
        ( Placement.placementRoutesAtVector
            ( checked
                "select whole-Herald preflight placement vector"
                ( Placement.currentPhysicalPlacementRevisionVector
                    ( OracleProjection.oracleViewCurrentHeraldMembership
                        (OracleProjection.oracleView (startupOracleProjectionState state))
                    )
                    (startupPlacementState state)
                )
            )
            (startupPlacementState state)
        )

    withDueDebt affectedSort state =
      replaceStartupAlignmentState withDebt state
      where
        consequence =
          checked
            "whole-Herald preflight consequence"
            (processEndCause mixedProcess (controlIndex 1))
        debts =
          normalizeStructuralDebts
            [ structuralConsequenceDebt
                (structuralDebtKey consequence TopologyAlignmentDebt affectedSort Nothing)
                (structuralDebtEvidence Set.empty Set.empty Set.empty)
            ]
        withDebt =
          commit
            Alignment.commitStructuralDebtRetention
            "retain whole-Herald preflight debt"
            ( Alignment.prepareStructuralDebtRetention
                consequence
                debts
                (startupAlignmentState state)
            )

    reconciliationViews state =
      Reconciliation.reconciliationViews
        (GraphProgress.structuralProgressLocalHerald (startupStructuralProgressState state))
        ( Map.fromList
            [ ( SortRegistry.registryEntrySortId entry,
                sortOccurrence
                  (SortRegistry.registryEntrySortId entry)
                  (SortRegistry.registryEntryOccurrenceId entry)
              )
            | entry <- SortRegistry.registryEntries (startupSortRegistryState state)
            ]
        )
        (Set.fromList (Graph.graphNonStructuralBaselineVertices (startupGraphState state)))
        ( Map.fromList
            [ (process, residence)
            | process <-
                OracleProjection.projectedProcessEpochs
                  (startupOracleProjectionState state),
              OracleProjection.oracleViewProcessIsLive
                process
                (OracleProjection.oracleView (startupOracleProjectionState state)),
              Just residence <-
                [ OracleProjection.oracleViewProcessResidence
                    process
                    (OracleProjection.oracleView (startupOracleProjectionState state))
                ]
            ]
        )
        ( Set.fromList
            ( Controlled.controlledNormalPossessionEntries
                (startupControlledState state)
            )
        )

-- Reuse the real application-defined regular reader. Unlike the predefined
-- sorts, it has no mandatory private system-view reader at the other Heralds.
-- Re-admit its definition and stamped occurrence into a freshly initialized
-- receiver, then install the same checked topology certificate.
pendingAnchorCustomSortFixture :: IO (SortOccurrence, HeraldState, HeraldState, HeraldState)
pendingAnchorCustomSortFixture = do
  (authored, _) <- settledDeltaControlledRemovalFixture
  let HeraldMember _ source = fixtureLocalMember
      HeraldMember _ receiver = fixtureRemoteMember
      sourceGenesis = startupGenesis authored
      members = checkedActiveHeralds sourceGenesis
      original = fixtureDeploymentAt fixtureRemoteMember
      deployment = original {deploymentActiveHeralds = members, deploymentOracleGenesis = (deploymentOracleGenesis original) {oracleGenesisActiveHeralds = members}}
      genesis = checked "custom-sort receiver genesis" (checkHeraldGenesis deployment)
      bootstraps = checked "custom-sort receiver bootstraps" (checkInitialBootstraps genesis (PrimordialProcessManifest (take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId])))
      initial = fst (checked "initialize custom-sort receiver" (initialHerald (monotonicInstant 0) genesis bootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration))
      withOtherRemote state member
        | heraldMemberEpoch member `elem` [source, receiver] = state
        | otherwise =
            let snapshot = checked "retain unrelated remote coordinate" (PlacementProtocol.placementSnapshot (heraldMemberEpoch member) PlacementProtocol.firstPlacementSequence [])
             in replaceStartupPlacementState (fst (Placement.commitRemotePlacement (checked "admit unrelated remote coordinate" (Placement.prepareRemotePlacement (FullPlacementSnapshot snapshot) (startupPlacementState state))))) state
      rawReceiver = foldl withOtherRemote initial members
      customOccurrences =
        [ sortOccurrence (SortRegistry.registryEntrySortId entry) (SortRegistry.registryEntryOccurrenceId entry)
        | entry <- SortRegistry.registryEntries (startupSortRegistryState authored),
          SortRegistry.registryEntryPredefinedRole entry == Nothing,
          not (null (liveLatestAlignmentGenerationsForSort (sortOccurrence (SortRegistry.registryEntrySortId entry) (SortRegistry.registryEntryOccurrenceId entry)) (startupAlignmentState authored)))
        ]
      definitionRecords = [record | (_, record) <- Publication.outgoingPublicationEntries (startupPublicationState authored), DomainPublication.checkedPublicationSort (Publication.outgoingPublicationChecked record) == SortProfile.profileSortFor SortProfile.SortDefinitionRole]
  assertEqual "custom-sort source has no unstated canonical control dependency" (controlIndex 0) (OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState authored)))
  affectedSort <- case customOccurrences of
    [value] -> pure value
    values -> assertFailure ("expected one real application-defined alignment sort: " <> show values)
  assertBool "the application really published its custom definition" (not (null definitionRecords))
  defined <- foldM installDefinition rawReceiver definitionRecords
  occurrences <- mapM (captureOccurrence authored) (Publication.stampedStructuralStageEntries (startupPublicationState authored))
  assertBool "the application really stamped its dynamic reader" (not (null occurrences))
  replayed <- foldM replayOccurrence defined occurrences
  installed <- foldM installCut replayed (GraphProgress.structuralInstalledEstablishedChain (startupStructuralProgressState authored))
  assertEqual "receiver reaches the exact real application topology" (GraphProgress.structuralLastInstalledCutId (startupStructuralProgressState authored)) (GraphProgress.structuralLastInstalledCutId (startupStructuralProgressState installed))
  assertEqual "replay does not import source alignment owners" [] (Alignment.alignmentGenerationEntries (startupAlignmentState installed))
  assertEqual "receiver's structural owner validates after semantic replay" (Right ()) (GraphProgress.validateStructuralProgressState (startupStructuralProgressState installed))
  pure (affectedSort, authored, installed, rawReceiver)
  where
    installDefinition state record = do
      let publication = Publication.outgoingPublicationChecked record
          definitionOccurrence = Publication.outgoingPublicationOccurrenceId record
          strength = Publication.outgoingPublicationSourceStrength record
          source = Publication.outgoingPublicationSourceProcess record
          definitionTopology = Publication.outgoingPublicationSourceTopologyPrerequisite record
          prerequisite = Publication.outgoingPublicationControlPrerequisite record
          context = PeerInput.peerInputContext (startupGenesis state) (startupOracleProjectionState state) (startupDiscoveryState state) (startupPlacementState state)
          carrier = checkedMaybe "definition carrier registry entry" (SortRegistry.lookupPredefinedRole SortProfile.SortDefinitionRole (startupSortRegistryState state))
      either (assertFailure . show) pure (PeerInput.validateRoutedPublicationAuthorityHistory context (startupStructuralProgressState state) (startupControlledState state) (PeerInput.routedPublicationAuthorityClaim (SortRegistry.registryEntryDescriptor carrier) definitionOccurrence publication source strength definitionTopology prerequisite Nothing))
      let slot = checkedMaybe "receiver definition system view" (find (\candidate -> Store.storeSlotProvenance candidate == Store.HeraldSystemView (checkedLocalHeraldEpoch (startupGenesis state)) SortProfile.SortDefinitionRole) (Store.storeSlots (startupStoreState state)))
          observation = checked "retain exact source definition observation" (Store.retainedStoreObservation publication strength definitionOccurrence (Store.RoutedStoreObservation (Store.storeObservationEnvelope source definitionOccurrence definitionTopology prerequisite Nothing)))
          preparedStore = checked "apply definition to receiver-owned system view" (Store.prepareAlignmentStoreApplication (Store.peerStoreDestination (Store.storeSlotDelta slot) (Store.storeSlotIncarnation slot) strength :| []) observation (startupStoreState state))
          store = fst (Store.commitPeerStoreApplication preparedStore)
          descriptor = checked "decode actual application definition" (SortProfile.decodeSortDefinitionValue (DomainPublication.checkedPublicationValue publication))
          induction = checked "plan receiver sort induction" (SortRegistry.planSortInduction (checkedSystemId (startupGenesis state)) descriptor (startupSortRegistryState state))
          registry = SortRegistry.commitSortInduction (checked "induct actual application definition" (SortRegistry.prepareSortInduction descriptor (SortRegistry.sortInductionPlanOccurrenceId induction) publication (startupSortRegistryState state)))
      pure (replaceStartupSortRegistryState registry (replaceStartupStoreState store state))
    captureOccurrence state (_, stage) = do
      let entry = checkedMaybe "stamped carrier sort" (SortRegistry.lookupEffectiveSort (DomainPublication.checkedPublicationSort (Publication.stampedStructuralChecked stage)) (startupSortRegistryState state))
          bytes = checked "capture actual stamped occurrence bytes" (PeerPublication.structuralPublicationCanonicalBytesForSemantics (SortRegistry.registryEntryDescriptor entry) (Publication.stampedStructuralSortOccurrenceId stage) (Publication.stampedStructuralChecked stage) (Publication.stampedStructuralSourceProcess stage) (Publication.stampedStructuralSourceStrength stage) (Publication.stampedStructuralSourceTopologyPrerequisite stage) (Publication.stampedStructuralControlPrerequisite stage))
      pure (checked "capture actual stamped occurrence" (Terminal.terminalStructuralOccurrence (Publication.stampedStructuralStamp stage) bytes))
    replayOccurrence state retainedOccurrence = case JoinReplay.replayJoinStructuralOccurrence retainedOccurrence state of
      Left problem -> assertFailure ("checked custom reader replay: " <> show problem)
      Right Nothing -> assertFailure "the real custom reader still lacks a historical prerequisite"
      Right (Just successor) -> pure successor
    installCut state established
      | GraphProtocol.topologyCutEstablishedId established `elem` GraphProgress.structuralInstalledCutIds progress = pure state
      | otherwise = do
          let prefix = topologyFrontierAppliedControlPrefix (topologyCutFrontier (GraphProtocol.topologyCutEstablishedCut established))
              (changed, bytes) = Reconciliation.structuralControlCheckpointAt prefix (GraphProgress.structuralProgressReconciliation progress)
              checkpoint = GraphProgress.topologyControlCheckpointWithConsequence prefix changed bytes
          prepared <- either (assertFailure . show) pure (GraphProgress.prepareTopologyCutEstablished (structuralReconciliationViews state) checkpoint established progress)
          pure (replaceStartupStructuralProgressState (GraphProgress.commitTopologyCutEstablished prepared) state)
      where
        progress = startupStructuralProgressState state

connectPendingAnchorSource :: HeraldState -> IO (HeraldState, PeerBinding)
connectPendingAnchorSource = connectPendingAnchorPeer fixtureLocalMember

connectPendingAnchorPeer :: HeraldMember -> HeraldState -> IO (HeraldState, PeerBinding)
connectPendingAnchorPeer (HeraldMember sourceId source) state = do
  let nonce = connectionNonce 1_339
      candidate = peerCandidate sourceId source nonce
      genesis = startupGenesis state
      hello = peerHello (checkedSystemId genesis) sourceId source nonce Set.empty (controlIndex 0) (checkedCatalogueDigest genesis) (checkedInitialProjectionDigest (startupInitialBootstraps state)) Nothing
  (connected, effects) <- checkedStep "connect actual pending-anchor source" (heraldInput (startupLastObservedTime state) (PeerInput (currentPeerHelloReceived state candidate Set.empty hello))) state
  case [binding | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- effectBatchMembers effects] of
    [binding] -> pure (connected, binding)
    values -> assertFailure ("expected one actual pending-anchor binding: " <> show values)

-- This application-defined sort has a remote-only class. Its old and replacement
-- anchors cover the same structural cause. A receiver may learn the exact loss
-- before delayed placement history lets it admit the old anchor; ordinary loss
-- reconciliation must still converge to the canonical replacement.
casePendingAnchorSameCauseRecovery :: Assertion
casePendingAnchorSameCauseRecovery = do
  (affectedSort, authored, receiving, rawReceiver) <- pendingAnchorCustomSortFixture
  checkPending affectedSort authored receiving rawReceiver
  where
    HeraldMember _ source = fixtureLocalMember
    HeraldMember _ receiver = fixtureRemoteMember
    checkPending affectedSort authored receiving rawReceiver = do
      oldGeneration <- case liveLatestAlignmentGenerationsForSort affectedSort (startupAlignmentState authored) of
        [generation] -> pure generation
        values -> assertFailure ("expected one remote-only real class: " <> show values)
      let oldCut = alignmentGenerationCut oldGeneration
          oldId = alignmentGenerationId oldGeneration
          oldTopology = deriveTopologyCutId (alignmentCutTopologyCut oldCut)
          oldVector = alignmentCutPhysicalPlacementRevisionVector oldCut
          oldMembers = NonEmpty.toList (alignmentCutExactMembers oldCut)
          oldPlan = checkedMaybe "current old routing plan" (Alignment.currentAlignmentPlanForSort affectedSort (startupAlignmentState authored))
          oldAnchor = AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement oldPlan)
          (_, oldAdvertisement) = Placement.commitLocalSnapshot (checked "read old anchor placement" (Placement.prepareRetainedLocalSnapshot (startupPlacementState authored)))
          oldSnapshot = Placement.localSnapshotMessage oldAdvertisement
          replacedMember = NonEmpty.head (alignmentCutExactMembers oldCut)
          replacedDelta = alignmentMemberDelta replacedMember
          lostIncarnation = alignmentMemberStoreIncarnation replacedMember
          replacementIncarnation = checked "replacement remote incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 0xe9))
          replaceRoute route
            | PlacementProtocol.deltaRouteDelta route /= replacedDelta = route
            | otherwise = case PlacementProtocol.viewDeltaRoute route of
                PlacementProtocol.ApplicationDeltaRouteView delta routeSort definition controller process _ prerequisite -> PlacementProtocol.applicationDeltaRoute delta routeSort definition controller process replacementIncarnation prerequisite
                PlacementProtocol.PrivateSystemViewDeltaRouteView {} -> error "remote-only class unexpectedly selects a private system view"
          newSnapshot = checked "replacement full placement" (PlacementProtocol.placementSnapshot source (PlacementProtocol.nextPlacementSequence (PlacementProtocol.placementSnapshotSequence oldSnapshot)) (map replaceRoute (PlacementProtocol.placementSnapshotRoutes oldSnapshot)))
          install snapshot state =
            replaceStartupPlacementState
              (fst (Placement.commitRemotePlacement (checked "admit delayed full placement" (Placement.prepareRemotePlacement (FullPlacementSnapshot snapshot) (startupPlacementState state)))))
              state
          -- This independently initialized receiver has installed the shared
          -- topology certificate, but its ordinary placement lane is delayed.
          withoutPlacement = replaceStartupPlacementState (startupPlacementState rawReceiver) receiving
          withCurrentPlacement = install newSnapshot withoutPlacement
          currentVector = checked "replacement physical vector" (Placement.currentPhysicalPlacementRevisionVector (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState withCurrentPlacement))) (startupPlacementState withCurrentPlacement))
      assertBool "the receiver has no old class member or local readiness prerequisite" (all ((/= receiver) . alignmentMemberHerald) oldMembers)
      assertBool "replacement is an exact new incarnation" (replacementIncarnation /= lostIncarnation)
      currentRoutes <- either (assertFailure . show) pure (activePlacementRoutesForSort withCurrentPlacement affectedSort currentVector)
      newPlan <- case deriveAlignmentGenerationPreparationAt withCurrentPlacement affectedSort oldTopology currentVector currentRoutes Map.empty [] [] Set.empty of
        Right (AlignmentGenerationReady plan) -> pure plan
        result -> assertFailure ("canonical replacement plan did not derive at the real cut: " <> show result)
      newGeneration <- case alignmentGenerationPlanGenerations newPlan of
        [generation] -> pure generation
        values -> assertFailure ("replacement plan is not singleton: " <> show values)
      let newId = alignmentGenerationId newGeneration
          newCut = alignmentGenerationCut newGeneration
          newIdentifier = Plan.alignmentGenerationBirthPlanId newGeneration
          newBinding = checked "replacement complete binding" (AlignmentProtocol.alignmentPlanBindingClaim (fmap alignmentMemberDelta (alignmentCutExactMembers newCut)) Plan.AlignmentCreated newId)
          newAnchor = AlignmentPlanAnnounced (checked "replacement complete plan" (AlignmentProtocol.alignmentPlanAnnounce newIdentifier (alignmentCutTopologyCut newCut) (Just (Plan.alignmentPlanId oldPlan)) Plan.AlignmentPredecessorInvalidated [newBinding] [] [alignmentGenerationCutAnnounce newGeneration]))
      assertEqual "canonical replacement excludes withdrawn ancestry" [] (alignmentCutPredecessorGenerationIds (alignmentGenerationCut newGeneration))
      (waiting, binding) <- connectPendingAnchorSource withoutPlacement
      let apply context control state = case applyAlignmentGenerationControlWithDisposition binding control state of
            Left problem -> assertFailure (context <> ": " <> show problem)
            Right Nothing -> assertFailure (context <> ": generation control was not handled")
            Right (Just (successor, effects, _)) -> pure (successor, effects)
      (oldHeld, _) <- apply "hold old anchor for missing exact placement" oldAnchor waiting
      (bothHeld, _) <- apply "hold successor for missing exact placement" newAnchor oldHeld
      assertEqual "neither missing coordinate can fabricate an admitted generation" [] (Alignment.alignmentGenerationEntries (startupAlignmentState bothHeld))
      (sourceSettled, _) <- case advanceAlignmentTransfers authored of
        Left problem -> assertFailure ("source cannot complete its actual local bootstrap transfers: " <> show problem)
        Right result -> pure result
      let certifiedSource = startupAlignmentState sourceSettled
          readiness = [ready | member <- oldMembers, Just ready <- [Alignment.lookupAlignmentMemberReadiness oldId (alignmentMemberStoreIncarnation member) certifiedSource]]
          certificates = [certificate | member <- oldMembers, Just certificate <- [Alignment.lookupAlignmentHistoricalCertificate oldId (alignmentMemberStoreIncarnation member) certifiedSource]]
      assertEqual "the source completes every actual local member before receiver admission" (length oldMembers) (length readiness)
      assertEqual "the source produces every actual historical certificate before receiver admission" (length oldMembers) (length certificates)
      assertEqual "the receiver has not supplied a cut vote for the source's readiness" Nothing (Alignment.lookupAlignmentCutAcceptance oldId receiver certifiedSource)
      assertEqual "remote readiness and certificates follow actual transfer fulfillment" (Right ()) (Alignment.validateAlignmentState certifiedSource)
      held <- foldM (\state control -> fst <$> apply "retain remote evidence ahead of its anchor" control state) bothHeld (map AlignmentMemberReadyAdvertised readiness <> map AlignmentHistoricalCertificateAdvertised certificates)
      let current = install newSnapshot held
          withHistory = replaceStartupPlacementState (checked "retain delayed exact old coordinate" (Placement.retainHistoricalPlacementSnapshot oldSnapshot (startupPlacementState current))) current
          unavailable = Alignment.unavailableAlignmentSource source replacedDelta lostIncarnation
          (knownLoss, _) = Alignment.commitUnavailableAlignmentSourceRetention (Alignment.prepareUnavailableAlignmentSourceRetention unavailable (startupAlignmentState withHistory))
          released = replaceStartupAlignmentState knownLoss withHistory
      assertBool "the exact withdrawal is already observed before old-anchor retry" (Set.member unavailable (Alignment.alignmentUnavailableSources knownLoss))
      assertBool "both old and successor anchors remain pending before release" (all (\control -> control `elem` map Alignment.pendingGenerationEvidenceControl (Alignment.pendingGenerationEvidenceEntries knownLoss)) [oldAnchor, newAnchor])
      (admitted, admissionEffects) <- apply "retry the now-resolvable old anchor" oldAnchor released
      (settled, transferEffects) <- case advanceAlignmentTransfers admitted of
        Left problem -> assertFailure ("ordinary loss and transfer convergence: " <> show problem)
        Right result -> pure result
      let final = startupAlignmentState settled
          rejected = [effect | effect@RejectPeerConnection {} <- effectBatchMembers (admissionEffects <> transferEffects)]
      assertEqual "known withdrawal cannot turn the canonical successor into a peer protocol rejection" [] rejected
      assertBool "ordinary loss reconciliation tombstones the old exact coordinate" (Alignment.alignmentPlanIsInvalidated affectedSort oldTopology oldVector final)
      assertEqual "the ordinary transfer pass admits the exact canonical replacement" (Just newGeneration) (Alignment.lookupAlignmentGeneration newId final)
      assertBool "the receiver accepts the replacement cut" (Alignment.lookupAlignmentCutAcceptance newId receiver final /= Nothing)
      assertEqual "withdrawn generation is no longer live ancestry" [newId] (map alignmentGenerationId (liveLatestAlignmentGenerationsForSort affectedSort final))
      assertEqual "complete alignment owner validates after delayed-anchor recovery" (Right ()) (Alignment.validateAlignmentState final)

-- The receiver really observed a Store withdrawal; it is not merely missing a
-- predecessor message. A descendant cannot recover that ancestry by waiting for
-- more certificates, so the fixed announcer selects a fresh attempt once its
-- own placement contains the same withdrawal.
caseObsoletePlanResetRecovery :: Assertion
caseObsoletePlanResetRecovery = do
  (affectedSort, authored, receiving, _) <- pendingAnchorCustomSortFixture
  let HeraldMember _ source = fixtureLocalMember
      HeraldMember _ receiver = fixtureRemoteMember
      oldPlan = currentPlan affectedSort authored
      oldIdentifier = Plan.alignmentPlanId oldPlan
      oldVector = Plan.alignmentPlanIdPlacement oldIdentifier
      oldTopology = Plan.alignmentPlanIdTopology oldIdentifier
      oldAnchor = AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement oldPlan)
      oldGeneration = checkedMaybe "old custom generation" (listToMaybe (routingPlanGenerations oldPlan))
      lostMember = NonEmpty.head (alignmentCutExactMembers (alignmentGenerationCut oldGeneration))
      lostDelta = alignmentMemberDelta lostMember
      lostIncarnation = alignmentMemberStoreIncarnation lostMember
      replacementIncarnation = checked "obsolete-plan replacement incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 0xe9))
      (_, advertisement) = Placement.commitLocalSnapshot (checked "old source placement" (Placement.prepareRetainedLocalSnapshot (startupPlacementState authored)))
      oldSnapshot = Placement.localSnapshotMessage advertisement
      replaceRoute route
        | PlacementProtocol.deltaRouteDelta route /= lostDelta = route
        | otherwise = case PlacementProtocol.viewDeltaRoute route of
            PlacementProtocol.ApplicationDeltaRouteView delta typed definition controller process _ prerequisite -> PlacementProtocol.applicationDeltaRoute delta typed definition controller process replacementIncarnation prerequisite
            PlacementProtocol.PrivateSystemViewDeltaRouteView {} -> error "expected an application Store in the custom sort"
      replacementRoutes = map replaceRoute (PlacementProtocol.placementSnapshotRoutes oldSnapshot)
      (newSourcePlacement, newAdvertisement) = Placement.commitLocalSnapshot (checked "checked source withdrawal projection" (Placement.prepareLocalSnapshotForRoutes replacementRoutes (startupPlacementState authored)))
      newSnapshot = Placement.localSnapshotMessage newAdvertisement
      install snapshot state = replaceStartupPlacementState (fst (Placement.commitRemotePlacement (checked "observe source placement" (Placement.prepareRemotePlacement (FullPlacementSnapshot snapshot) (startupPlacementState state))))) state
      currentMembership = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState authored))
      laterVector vector = checked "later unrelated placement coordinate" (physicalPlacementRevisionVector currentMembership [(home, if home == receiver then nextPlacementRevision revision else revision) | (home, revision) <- NonEmpty.toList (physicalPlacementRevisionEntries vector)])
      causeForPlan = checkedMaybe "real custom-sort structural cause" (listToMaybe [Alignment.alignmentPromotionKeyCause key | (key, _) <- Alignment.alignmentPromotionEntries (startupAlignmentState authored), Alignment.alignmentPromotionKeySort key == affectedSort])
      retain plan state = replaceStartupAlignmentState (commit Alignment.commitAlignmentPromotion "retain actual planner successor" (Alignment.prepareAlignmentPlanPromotion source causeForPlan plan (startupAlignmentState state))) state
  assertEqual "the withdrawn member is really hosted by the source" source (alignmentMemberHerald lostMember)
  routes <- either (assertFailure . show) pure (activePlacementRoutesForSort authored affectedSort oldVector)
  (installedTopology, context, members) <- either (assertFailure . show) pure (deriveAlignmentInputsAt authored affectedSort oldTopology oldVector routes Map.empty)
  let makeSuccessor prior vector = case Plan.prepareAlignmentPlan affectedSort installedTopology vector context members (Just (prior, Plan.AlignmentPredecessorUsable)) Set.empty of
        Right (Plan.AlignmentPlanReady plan) -> plan
        result -> error ("carried successor did not derive: " <> show result)
      successor = makeSuccessor oldPlan (laterVector oldVector)
      descendant = makeSuccessor successor (laterVector (laterVector oldVector))
      successorIdentifier = Plan.alignmentPlanId successor
      successorAnchor = AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement successor)
      descendantAnchor = AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement descendant)
      authoredDescendant = retain descendant (retain successor authored)
  assertBool "Q depends on the usable old plan" (Plan.alignmentPlanPredecessor successor == Just oldIdentifier && Plan.alignmentPlanPredecessorStatus successor == Plan.AlignmentPredecessorUsable)
  assertBool "Q carries the old generations" (all ((== Plan.AlignmentCarried) . Plan.alignmentPlanBindingDisposition . snd) (Plan.alignmentPlanBindings successor))
  (connectedReceiver, fromSource) <- connectPendingAnchorSource (install oldSnapshot receiving)
  (admittedReceiver, _) <- apply "admit the original plan" fromSource oldAnchor connectedReceiver
  assertEqual "receiver has admitted P" (Just oldIdentifier) (Plan.alignmentPlanId <$> Alignment.currentAlignmentPlanForSort affectedSort (startupAlignmentState admittedReceiver))
  (lostReceiver, _) <- either (assertFailure . show) pure (advanceAlignmentTransfers (install newSnapshot admittedReceiver))
  assertBool "ordinary transfer reconciliation invalidates P after the exact withdrawal" (Alignment.alignmentPlanInvalidated oldIdentifier (startupAlignmentState lostReceiver))
  -- Membership retirement may discard the latest remote placement before the
  -- receiver emits its first loss report. The frozen birth coordinate remains
  -- enough to name the exact unavailable Store.
  let retiredProjectionReceiver = retireOracleMember source admittedReceiver
      unreportedWithdrawal = install newSnapshot retiredProjectionReceiver
      unreportedLoss = commit Alignment.commitAlignmentPlanInvalidation "retain loss before its first report" (Alignment.prepareAlignmentPlanInvalidation (Alignment.AlignmentPlanDestinationStoreLost (alignmentGenerationId oldGeneration) (destinationStore lostDelta lostIncarnation)) (startupAlignmentState unreportedWithdrawal))
      retiredPlacement = fst (Placement.commitRemotePlacementRetirement (checked "retire unavailable source placement" (Placement.prepareRemotePlacementRetirement source (startupPlacementState unreportedWithdrawal))))
      retiredReceiver = replaceStartupAlignmentState unreportedLoss (replaceStartupPlacementState retiredPlacement unreportedWithdrawal)
      frozenSourceRevision = checkedMaybe "source birth placement revision" (lookup source (NonEmpty.toList (physicalPlacementRevisionEntries oldVector)))
  assertEqual "the first loss report has not been emitted before retirement" [] (Alignment.alignmentPlanObsoleteForSort affectedSort (startupAlignmentState retiredReceiver))
  assertEqual "retirement removes the current remote sequence" Nothing (Placement.remotePlacementSequence source retiredPlacement)
  (retiredReported, _) <- either (assertFailure . show) pure (advanceAlignmentGenerations retiredReceiver)
  retiredReport <- case [value | value <- Alignment.alignmentPlanObsoleteEntries (startupAlignmentState retiredReported), AlignmentProtocol.alignmentPlanObsoleteId value == oldIdentifier, AlignmentProtocol.alignmentPlanObsoleteReporter value == receiver] of
    [value] -> pure value
    values -> assertFailure ("expected first loss report after home retirement: " <> show values)
  assertEqual "first report after retirement keeps the frozen exact Store witness" [AlignmentProtocol.alignmentLostStore source lostDelta lostIncarnation frozenSourceRevision] (NonEmpty.toList (AlignmentProtocol.alignmentPlanObsoleteLostStores retiredReport))
  (reportedReceiver, reportEffects) <- apply "reject Q's unusable ancestry" fromSource successorAnchor lostReceiver
  report <- case [value | QueuePeerAlignmentEvidence destination (AlignmentPlanObsoleteAdvertised value) <- effectBatchMembers reportEffects, destination == source, AlignmentProtocol.alignmentPlanObsoleteId value == successorIdentifier] of
    [value] -> pure value
    values -> assertFailure ("expected one exact obsolete-plan report, got " <> show values)
  assertEqual "the report names the receiver" receiver (AlignmentProtocol.alignmentPlanObsoleteReporter report)
  assertEqual "the report preserves the exact Store and observed withdrawal revision" [AlignmentProtocol.alignmentLostStore source lostDelta lostIncarnation (PlacementProtocol.placementSnapshotSequence newSnapshot)] (NonEmpty.toList (AlignmentProtocol.alignmentPlanObsoleteLostStores report))
  (repeatedReceiver, repeatedEffects) <- apply "duplicate obsolete successor" fromSource successorAnchor reportedReceiver
  assertBool "duplicate obsolete announcement is inert" (reportedReceiver == repeatedReceiver)
  assertEqual "duplicate obsolete announcement does not resend a report" [] (effectBatchMembers repeatedEffects)
  (connectedSource, fromReceiver) <- connectPendingAnchorPeer fixtureRemoteMember authoredDescendant
  (reportedSource, _) <- apply "receive the exact withdrawal witness" fromReceiver (AlignmentPlanObsoleteAdvertised report) connectedSource
  (staleSource, _) <- either (assertFailure . show) pure (advanceAlignmentGenerations reportedSource)
  assertEqual "old placement cannot authorize reset" (Plan.alignmentPlanId descendant) (Plan.alignmentPlanId (currentPlan affectedSort staleSource))
  let currentSource = replaceStartupPlacementState newSourcePlacement staleSource
      sourceStores = startupStoreState currentSource
      receiverStores = startupStoreState reportedReceiver
  (resetSource, _) <- either (assertFailure . show) pure (advanceAlignmentGenerations currentSource)
  let reset = currentPlan affectedSort resetSource
      resetIdentifier = Plan.alignmentPlanId reset
      resetControl = AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement reset)
  assertEqual "matching withdrawal selects the next attempt" (DomainAlignment.nextAlignmentPlanAttempt (Plan.alignmentPlanIdAttempt oldIdentifier)) (Plan.alignmentPlanIdAttempt resetIdentifier)
  assertEqual "reset has no predecessor dependency" Nothing (Plan.alignmentPlanPredecessor reset)
  assertEqual "reset is explicit" Plan.AlignmentPredecessorReset (Plan.alignmentPlanPredecessorStatus reset)
  assertBool "reset creates every class afresh" (all ((== Plan.AlignmentCreated) . Plan.alignmentPlanBindingDisposition . snd) (Plan.alignmentPlanBindings reset))
  assertBool "reset has no old generation ancestry" (all (null . alignmentCutPredecessorGenerationIds . alignmentGenerationCut) (routingPlanGenerations reset))
  assertBool "planning leaves the source Store intact" (sourceStores == startupStoreState resetSource)
  (resetReceiver, _) <- apply "fresh reset skips Q and Q2" fromSource resetControl reportedReceiver
  assertEqual "receiver adopts the fresh attempt without either skipped predecessor" (Just resetIdentifier) (Plan.alignmentPlanId <$> Alignment.currentAlignmentPlanForSort affectedSort (startupAlignmentState resetReceiver))
  assertBool "reset leaves the receiver Store intact" (receiverStores == startupStoreState resetReceiver)
  (afterStale, staleEffects) <- foldM (\(state, effects) control -> do (successorState, nextEffects) <- apply "old announcement after reset" fromSource control state; pure (successorState, effects <> nextEffects)) (resetReceiver, mempty) [oldAnchor, successorAnchor, descendantAnchor]
  assertBool "delayed old announcements cannot revive superseded work" (afterStale == resetReceiver)
  assertEqual "delayed old announcements emit no controls" [] (effectBatchMembers staleEffects)
  (afterOldReport, oldReportEffects) <- apply "old report after reset" fromReceiver (AlignmentPlanObsoleteAdvertised report) resetSource
  assertBool "duplicate report cannot create another attempt" (afterOldReport == resetSource)
  assertEqual "duplicate report emits no controls" [] (effectBatchMembers oldReportEffects)
  forM_ [resetSource, resetReceiver] $ \state -> do
    assertEqual "reset owner invariants" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState state))
    assertEqual "exact replay reconstructs the reset predecessor policy" (Right reset) (replayAlignmentPlanAt state resetIdentifier)
  let secondIncarnation = checked "second replacement incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 0xea))
      replaceAgain route
        | PlacementProtocol.deltaRouteDelta route /= lostDelta = route
        | otherwise = case PlacementProtocol.viewDeltaRoute route of
            PlacementProtocol.ApplicationDeltaRouteView delta typed definition controller process _ prerequisite -> PlacementProtocol.applicationDeltaRoute delta typed definition controller process secondIncarnation prerequisite
            PlacementProtocol.PrivateSystemViewDeltaRouteView {} -> error "expected the replacement application Store"
      secondRoutes = map replaceAgain replacementRoutes
      (secondPlacement, secondAdvertisement) = Placement.commitLocalSnapshot (checked "second checked source withdrawal" (Placement.prepareLocalSnapshotForRoutes secondRoutes (startupPlacementState resetSource)))
      secondSnapshot = Placement.localSnapshotMessage secondAdvertisement
  (secondLostReceiver, secondLossEffects) <- either (assertFailure . show) pure (advanceAlignmentTransfers (install secondSnapshot resetReceiver))
  assertBool "a second withdrawal invalidates the fresh attempt" (Alignment.alignmentPlanInvalidated resetIdentifier (startupAlignmentState secondLostReceiver))
  (secondReportedReceiver, secondReportEffects) <- either (assertFailure . show) pure (advanceAlignmentGenerations secondLostReceiver)
  secondReport <- case [value | QueuePeerAlignmentEvidence destination (AlignmentPlanObsoleteAdvertised value) <- effectBatchMembers (secondLossEffects <> secondReportEffects), destination == source, AlignmentProtocol.alignmentPlanObsoleteId value == resetIdentifier] of
    [value] -> pure value
    values -> assertFailure ("expected one second-loss report: " <> show values)
  assertEqual "second report names the replacement, not the original Store" [AlignmentProtocol.alignmentLostStore source lostDelta replacementIncarnation (PlacementProtocol.placementSnapshotSequence secondSnapshot)] (NonEmpty.toList (AlignmentProtocol.alignmentPlanObsoleteLostStores secondReport))
  (secondReportedSource, _) <- apply "receive replacement Store loss" fromReceiver (AlignmentPlanObsoleteAdvertised secondReport) resetSource
  assertEqual "second loss also waits for its placement witness" resetIdentifier (Plan.alignmentPlanId (currentPlan affectedSort secondReportedSource))
  (secondResetSource, _) <- either (assertFailure . show) pure (advanceAlignmentGenerations (replaceStartupPlacementState secondPlacement secondReportedSource))
  let secondReset = currentPlan affectedSort secondResetSource
      secondResetIdentifier = Plan.alignmentPlanId secondReset
  assertEqual "second loss increments the attempt again" (DomainAlignment.nextAlignmentPlanAttempt (Plan.alignmentPlanIdAttempt resetIdentifier)) (Plan.alignmentPlanIdAttempt secondResetIdentifier)
  (secondResetReceiver, _) <- apply "adopt second fresh attempt" fromSource (AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement secondReset)) secondReportedReceiver
  assertEqual "both receivers converge after repeated loss" (Just secondResetIdentifier) (Plan.alignmentPlanId <$> Alignment.currentAlignmentPlanForSort affectedSort (startupAlignmentState secondResetReceiver))
  assertBool "repeated reset preserves source Store" (startupStoreState secondResetSource == sourceStores)
  assertBool "repeated reset preserves receiver Store" (startupStoreState secondResetReceiver == receiverStores)
  forM_ [secondResetSource, secondResetReceiver] $ \state -> do
    assertEqual "repeated reset owner invariants" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState state))
    assertEqual "repeated reset replays exactly" (Right secondReset) (replayAlignmentPlanAt state secondResetIdentifier)
  where
    currentPlan selectedSort state = checkedMaybe "current custom-sort routing plan" (Alignment.currentAlignmentPlanForSort selectedSort (startupAlignmentState state))
    apply label binding control state = case applyAlignmentGenerationControlWithDisposition binding control state of
      Left problem -> assertFailure (label <> ": " <> show problem)
      Right Nothing -> assertFailure (label <> ": control was not handled")
      Right (Just (successor, effects, _)) -> pure (successor, effects)

caseFixedAnnouncerOwnsAnchor :: Assertion
caseFixedAnnouncerOwnsAnchor = do
  plan <- explicitTwoClassPlan multiPlacement
  let generations = routingPlanGenerations plan
      firstClass = checkedMaybe "nonempty class table" (listToMaybe generations)
      withDebt = retainCauseDebt (cause occurrence) Alignment.emptyState
      promoted = commit Alignment.commitAlignmentPromotion "promote explicit two-class plan" (Alignment.prepareAlignmentPlanPromotion heraldA (cause occurrence) plan withDebt)
      announced = [AlignmentProtocol.alignmentPlanAnnounceId announce | AlignmentPlanAnnounced announce <- alignmentRetryControls heraldA heraldB promoted]
  forM_ [(heraldA, heraldB), (heraldB, heraldC), (heraldC, heraldA)] $ \(local, remote) -> assertControlCatalogueDelta local remote withDebt promoted
  assertEqual "fixture has two context classes" 2 (length generations)
  assertEqual "first class birth announcer remains its least physical host" heraldB (alignmentGenerationAnnouncer firstClass)
  assertEqual "whole plan authority is the least fixed Herald" (Just heraldA) (Plan.alignmentPlanAnnouncer plan)
  assertEqual "one complete explicit plan is announced" [Plan.alignmentPlanId plan] announced

caseAttemptAnchors :: Assertion
caseAttemptAnchors = do
  first <- explicitTwoClassPlan multiPlacement
  replacement <- case Plan.prepareAlignmentPlanAtAttempt (DomainAlignment.nextAlignmentPlanAttempt DomainAlignment.initialAlignmentPlanAttempt) typedSort multiTopology multiPlacement twoClassContextGraph twoClassMembers (Just (first, Plan.AlignmentPredecessorInvalidated)) Set.empty of
    Right (Plan.AlignmentPlanReady plan) -> pure plan
    result -> assertFailure ("distinct-attempt plan unavailable: " <> show result)
  let firstAnchor = checkedMaybe "first attempt anchor" (listToMaybe (routingPlanGenerations first))
      nextAnchor = checkedMaybe "replacement attempt anchor" (listToMaybe (routingPlanGenerations replacement))
      withDebt = retainCauseDebt (cause occurrence) Alignment.emptyState
      promoted = commit Alignment.commitAlignmentPromotion "promote first anchor attempt" (Alignment.prepareAlignmentPlanPromotion heraldA (cause occurrence) first withDebt)
      lost = Alignment.AlignmentPlanDestinationStoreLost (alignmentGenerationId firstAnchor) (destinationStore deltaA storeA)
      invalidated = commit Alignment.commitAlignmentPlanInvalidation "invalidate first anchor attempt" (Alignment.prepareAlignmentPlanInvalidation lost promoted)
      replaced = commit Alignment.commitAlignmentPromotion "promote replacement anchor attempt" (Alignment.prepareAlignmentPlanPromotion heraldA (cause occurrence) replacement invalidated)
      expected = Set.fromList [alignmentGenerationId firstAnchor, alignmentGenerationId nextAnchor]
  assertEqual "both attempts have a class host different from their plan announcer" [heraldB, heraldB] (map alignmentGenerationAnnouncer [firstAnchor, nextAnchor])
  assertEqual "identical topology and placement keep both attempts' fixed-announcer anchors" expected (retainedAnchorGenerationIds replaced)
  assertEqual "the fresh attempt does not revive the prior generation IDs" Set.empty (Set.intersection (Set.fromList (map alignmentGenerationId (routingPlanGenerations first))) (Set.fromList (map alignmentGenerationId (routingPlanGenerations replacement))))
  assertEqual "retained old and new attempt evidence preserves owner invariants" (Right ()) (Alignment.validateAlignmentState replaced)

caseEmptyPlanProtocolConvergence :: Assertion
caseEmptyPlanProtocolConvergence = do
  let emptyGraph = checked "empty routing context" (deriveContextGraph [] [] [])
      prepare vector = case Plan.prepareAlignmentPlan typedSort multiTopology vector emptyGraph [] Nothing Set.empty of
        Right (Plan.AlignmentPlanReady plan) -> pure plan
        result -> assertFailure ("empty explicit plan unavailable: " <> show result)
  selectedPlan <- prepare multiPlacement
  alternatePlan <- prepare alternateMultiPlacement
  let identifier = Plan.alignmentPlanId selectedPlan
      announcement = AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement selectedPlan)
      withDebt = retainCauseDebt (cause occurrence) Alignment.emptyState
      promote local state = commit Alignment.commitAlignmentPromotion "promote selected empty plan" (Alignment.prepareAlignmentPlanPromotion local (cause occurrence) selectedPlan state)
      authored = promote heraldA withDebt
      observed = promote heraldB withDebt
      acceptance = checked "accept exact empty plan" (AlignmentProtocol.alignmentPlanAccepted identifier heraldB EmptyHeraldPublicationPrefix)
      accepted = fst (checked "retain empty plan acceptance" (Alignment.retainAlignmentPlanAcceptance acceptance observed))
      acknowledged = fst (checked "retain remote empty plan acceptance" (Alignment.retainAlignmentPlanAcceptance acceptance authored))
  assertBool "different local empty placement choices would have different predecessor identities" (identifier /= Plan.alignmentPlanId alternatePlan)
  assertEqual "empty plans retain the fixed least-member authority" (Just heraldA) (Plan.alignmentPlanAnnouncer selectedPlan)
  assertBool "the fixed least member may derive the empty frontier" (spontaneousAlignmentPromotionMayDerive heraldA (heraldA :| [heraldB, heraldC]) False)
  forM_ [heraldB, heraldC] $ \other ->
    assertBool "other members cannot install an independently selected empty frontier" (not (spontaneousAlignmentPromotionMayDerive other (heraldA :| [heraldB, heraldC]) False))
  assertEqual "an empty plan creates no generation identity" [] (Alignment.alignmentGenerationEntries authored)
  assertEqual "an empty plan creates no ordinary owners" [] (Alignment.activeAlignmentObligationEntries authored)
  assertEqual "an empty plan creates no bootstrap imports" [] (Alignment.bootstrapImportEntries authored)
  assertEqual "reconnect advertises the complete empty plan" [announcement] (alignmentRetryControls heraldA heraldB authored)
  assertEqual "ordinary delta advertises the empty plan without a generation candidate" [announcement] (alignmentGenerationControlDelta heraldA heraldB withDebt authored)
  assertEqual "only fixed authority advertises empty plans" [] (alignmentRetryControls heraldB heraldA observed)
  assertEqual "empty acceptance is keyed by the exact plan" [AlignmentPlanAcceptanceAdvertised acceptance] (alignmentGenerationControlDelta heraldB heraldA observed accepted)
  assertEqual "empty acceptance suppresses that exact announcement" [] (alignmentRetryControls heraldA heraldB acknowledged)
  assertEqual "empty acceptance manufactures no birth-cut votes" [] (Alignment.alignmentCutAcceptanceEntries accepted)
  assertEqual "empty acceptance manufactures no member readiness" [] (Alignment.alignmentMemberReadinessEntries accepted)
  assertEqual "empty current frontier is retained explicitly" (Just identifier) (Plan.alignmentPlanId <$> Alignment.currentAlignmentPlanForSort typedSort accepted)
  assertEqual "selected empty frontier replay is inert" authored (promote heraldA authored)
  forM_ [authored, observed, accepted, acknowledged] $ \state ->
    assertEqual "empty protocol states preserve all owner invariants" (Right ()) (Alignment.validateAlignmentState state)
  -- Exercise the actual ingress emission helper: a retained empty table has no
  -- generation key through which its newly admitted local acceptance can fan out.
  (receiving, binding) <- connectedWithAlignmentAt fixtureRemoteMember fixtureLocalMember Alignment.emptyState
  let local = checkedLocalHeraldEpoch (startupGenesis receiving)
      HeraldMember _ remote = fixtureLocalMember
      currentMembership = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState receiving))
      progress = startupStructuralProgressState receiving
      currentCut = checked "empty ingress checked topology" (topologyCut (sameGenerationPredecessor (GraphProgress.structuralLastInstalledCutId progress)) (topologyFrontier (emptyStructuralVersionVector currentMembership) (controlIndex 0)) (deriveTopologyOccurrenceDigest "empty-ingress-retained-plan"))
      currentPlacement = checked "empty ingress placement" (physicalPlacementRevisionVector currentMembership [(herald, firstPlacementRevision) | herald <- NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs currentMembership)])
      receivingPlan = case Plan.prepareAlignmentPlan typedSort currentCut currentPlacement emptyGraph [] Nothing Set.empty of
        Right (Plan.AlignmentPlanReady plan) -> plan
        result -> error ("empty ingress plan unavailable: " <> show result)
      receivingIdentifier = Plan.alignmentPlanId receivingPlan
      receivingOwner = commit Alignment.commitAlignmentPromotion "retain empty ingress plan" (Alignment.prepareAlignmentPlanPromotion local (cause occurrence) receivingPlan (retainCauseDebt (cause occurrence) Alignment.emptyState))
      beforeIngress = replaceStartupAlignmentState receivingOwner receiving
      announcementControl = AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement receivingPlan)
  (afterIngress, ingressEffects, _) <- case applyAlignmentGenerationControlWithDisposition binding announcementControl beforeIngress of
    Right (Just result) -> pure result
    Left problem -> assertFailure ("empty ingress was not admitted: " <> show problem)
    Right Nothing -> assertFailure "empty ingress was not handled"
  admittedAcceptance <- maybe (assertFailure "empty ingress did not retain its local plan acceptance") pure (Alignment.lookupAlignmentPlanAcceptance receivingIdentifier local (startupAlignmentState afterIngress))
  assertEqual
    "production empty-plan ingress emits its exact plan acceptance"
    [QueuePeerAlignmentEvidence remote (AlignmentPlanAcceptanceAdvertised admittedAcceptance)]
    (effectBatchMembers ingressEffects)
  assertEqual "production empty-plan ingress creates no generation candidates" [] (Alignment.alignmentGenerationEntries (startupAlignmentState afterIngress))
  (replayedIngress, repeatedEffects, _) <- case applyAlignmentGenerationControlWithDisposition binding announcementControl afterIngress of
    Right (Just result) -> pure result
    Left problem -> assertFailure ("empty ingress replay was not admitted: " <> show problem)
    Right Nothing -> assertFailure "empty ingress replay was not handled"
  assertBool "production empty-plan replay is state-identical" (replayedIngress == afterIngress)
  assertEqual "production empty-plan replay does not resend acceptance" [] (effectBatchMembers repeatedEffects)

caseDivergentVectorAnchorConvergence :: Assertion
caseDivergentVectorAnchorConvergence = do
  selectedPlan <- explicitTwoClassPlan multiPlacement
  alternatePlan <- explicitTwoClassPlan alternateMultiPlacement
  assertEqual "candidate vectors retain complete class tables" (2, 2) (length (routingPlanGenerations selectedPlan), length (routingPlanGenerations alternatePlan))
  assertBool "different placement vectors give distinct plan identities" (Plan.alignmentPlanId selectedPlan /= Plan.alignmentPlanId alternatePlan)
  assertBool "different vectors bind distinct created generations" (generationIds selectedPlan /= generationIds alternatePlan)
  assertEqual "fixed least Herald alone announces the chosen coordinate" (Just heraldA) (Plan.alignmentPlanAnnouncer selectedPlan)
  let promoteAt local plan = commit Alignment.commitAlignmentPromotion "promote selected explicit plan" (Alignment.prepareAlignmentPlanPromotion local (cause occurrence) plan (retainCauseDebt (cause occurrence) Alignment.emptyState))
      retainedA = promoteAt heraldA selectedPlan
      retainedB = promoteAt heraldB selectedPlan
      retainedC = promoteAt heraldC selectedPlan
      retainedIds = fmap (alignmentGenerationId . snd) . Alignment.alignmentGenerationEntries
  assertEqual "selected coordinate converges on all owners" (retainedIds retainedA, retainedIds retainedA) (retainedIds retainedB, retainedIds retainedC)
  assertEqual "complete relation table also converges" (Alignment.alignmentGenerationRelationEntries retainedA, Alignment.alignmentGenerationRelationEntries retainedA) (Alignment.alignmentGenerationRelationEntries retainedB, Alignment.alignmentGenerationRelationEntries retainedC)
  selectedAnnounce <- case [announce | AlignmentPlanAnnounced announce <- alignmentRetryControls heraldA heraldB retainedA, AlignmentProtocol.alignmentPlanAnnounceId announce == Plan.alignmentPlanId selectedPlan] of
    [announce] -> pure announce
    values -> assertFailure ("expected one selected plan announcement, got " <> show (length values))
  assertEqual "announcement carries exact selected placement" multiPlacement (Plan.alignmentPlanIdPlacement (AlignmentProtocol.alignmentPlanAnnounceId selectedAnnounce))
  assertEqual "announcement contains every binding" 2 (length (AlignmentProtocol.alignmentPlanAnnounceBindings selectedAnnounce))
  assertBool "active coordinate rejects a late alternate vector" (not (alignmentCausePromotionAuthorized (cause occurrence) typedSort (deriveTopologyCutId multiTopology) alternateMultiPlacement retainedB))
  let exactReplay = checked "exact selected plan replay" (Alignment.prepareAlignmentPlanPromotion heraldB (cause occurrence) selectedPlan retainedB)
      (replayed, disposition) = Alignment.commitAlignmentPromotion exactReplay
  assertEqual "exact plan replay is classified unchanged" Alignment.AlignmentPromotionUnchanged disposition
  assertEqual "exact plan replay is state-idempotent" retainedB replayed
  where
    generationIds = map alignmentGenerationId . routingPlanGenerations

explicitTwoClassPlan :: PhysicalPlacementRevisionVector -> IO Plan.AlignmentPlan
explicitTwoClassPlan vector = case Plan.prepareAlignmentPlan typedSort multiTopology vector twoClassContextGraph twoClassMembers Nothing Set.empty of
  Right (Plan.AlignmentPlanReady plan) -> pure plan
  result -> assertFailure ("explicit class plan unavailable: " <> show result)

routingPlanGenerations :: Plan.AlignmentPlan -> [AlignmentGeneration]
routingPlanGenerations = map (Plan.alignmentPlanBindingGeneration . snd) . Plan.alignmentPlanBindings

caseSequentialSameSortPromotion :: Assertion
caseSequentialSameSortPromotion = do
  firstPlan <- generationPlan
  firstGeneration <- case alignmentGenerationPlanGenerations firstPlan of
    [generation] -> pure generation
    generations ->
      assertFailure
        ("expected one first generation, got " <> show (length generations))
  let withDebts =
        retainDebt
          successorOccurrence
          (retainDebt occurrence Alignment.emptyState)
      afterFirst =
        commit
          Alignment.commitAlignmentPromotion
          "promote first same-sort occurrence"
          ( Alignment.prepareAlignmentPromotion
              heraldA
              (cause occurrence)
              typedSort
              topologyId
              placement
              firstPlan
              withDebts
          )
  assertEqual
    "the live first coordinate remains covered when placement advances"
    AlignmentCauseCovered
    ( alignmentCausePromotionDisposition
        (cause occurrence)
        typedSort
        topologyId
        advancedPlacement
        afterFirst
    )
  assertEqual
    "the second occurrence is due while the old vector is still selected"
    AlignmentCausePromotionDue
    ( alignmentCausePromotionDisposition
        (cause successorOccurrence)
        typedSort
        successorTopologyId
        placement
        afterFirst
    )
  assertEqual
    "the second occurrence remains due at its qualified placement vector"
    AlignmentCausePromotionDue
    ( alignmentCausePromotionDisposition
        (cause successorOccurrence)
        typedSort
        successorTopologyId
        advancedPlacement
        afterFirst
    )

  secondPlan <- case prepareAlignmentGenerationPlan
    typedSort
    successorTopology
    advancedPlacement
    contextGraphFixture
    generationMembers
    []
    []
    Set.empty of
    Right (AlignmentGenerationReady plan) -> pure plan
    result -> assertFailure ("second generation plan was not ready: " <> show result)
  let afterSecond =
        commit
          Alignment.commitAlignmentPromotion
          "promote second same-sort occurrence"
          ( Alignment.prepareAlignmentPromotion
              heraldA
              (cause successorOccurrence)
              typedSort
              successorTopologyId
              advancedPlacement
              secondPlan
              afterFirst
          )
      expectedPromotionKeys =
        Set.fromList
          [ Alignment.alignmentPromotionKey
              (cause occurrence)
              typedSort
              topologyId
              placement,
            Alignment.alignmentPromotionKey
              (cause successorOccurrence)
              typedSort
              successorTopologyId
              advancedPlacement
          ]
      retainedPromotionKeys =
        Set.fromList (fmap fst (Alignment.alignmentPromotionEntries afterSecond))
  assertEqual
    "the first coordinate stays frozen while the second uses the new vector"
    expectedPromotionKeys
    retainedPromotionKeys
  assertEqual
    "both same-sort promotions remain internally valid"
    (Right ())
    (Alignment.validateAlignmentState afterSecond)

  let lossCause =
        Alignment.AlignmentPlanDestinationStoreLost
          (alignmentGenerationId firstGeneration)
          (destinationStore deltaA storeA)
      invalidatedFirst =
        commit
          Alignment.commitAlignmentPlanInvalidation
          "invalidate first destination coordinate"
          (Alignment.prepareAlignmentPlanInvalidation lossCause afterFirst)
      livePrior = liveLatestAlignmentGenerationsForSort typedSort invalidatedFirst
      replacementMembers =
        [ generationMemberInput deltaA storeC heraldA initialStoreRevision,
          generationMemberInput deltaB storeB heraldB initialStoreRevision
        ]
  assertControlCatalogueDelta heraldA heraldB afterFirst invalidatedFirst
  assertControlCatalogueDelta heraldB heraldA afterFirst invalidatedFirst
  assertEqual
    "an exact tombstoned coordinate blocks until placement advances"
    AlignmentCausePromotionBlocked
    ( alignmentCausePromotionDisposition
        (cause occurrence)
        typedSort
        topologyId
        placement
        invalidatedFirst
    )

  assertEqual
    "destination loss makes the first occurrence due at a fresh vector"
    AlignmentCausePromotionDue
    ( alignmentCausePromotionDisposition
        (cause occurrence)
        typedSort
        topologyId
        advancedPlacement
        invalidatedFirst
    )
  assertEqual
    "all-invalidated recovery may advance to a successor topology"
    AlignmentCausePromotionDue
    ( alignmentCausePromotionDisposition
        (cause occurrence)
        typedSort
        successorTopologyId
        advancedPlacement
        invalidatedFirst
    )
  assertEqual
    "the coordinate tombstone also blocks an unpromoted occurrence"
    AlignmentCausePromotionBlocked
    ( alignmentCausePromotionDisposition
        (cause successorOccurrence)
        typedSort
        topologyId
        placement
        invalidatedFirst
    )
  assertEqual
    "an unpromoted occurrence becomes due at the fresh vector"
    AlignmentCausePromotionDue
    ( alignmentCausePromotionDisposition
        (cause successorOccurrence)
        typedSort
        topologyId
        advancedPlacement
        invalidatedFirst
    )
  assertEqual
    "an invalidated latest generation is not a replacement prerequisite"
    []
    livePrior
  case prepareAlignmentGenerationPlan
    typedSort
    topology
    advancedPlacement
    contextGraphFixture
    replacementMembers
    [firstGeneration]
    []
    Set.empty of
    Right (AlignmentGenerationHeld missing) ->
      assertEqual
        "the historical predecessor really would wait for its impossible certificate"
        (Set.singleton (alignmentGenerationId firstGeneration))
        missing
    result ->
      assertFailure
        ("invalidated predecessor unexpectedly did not hold replacement: " <> show result)
  case prepareAlignmentGenerationPlan
    typedSort
    topology
    advancedPlacement
    contextGraphFixture
    replacementMembers
    livePrior
    []
    Set.empty of
    Right (AlignmentGenerationReady replacementPlan) ->
      case alignmentGenerationPlanGenerations replacementPlan of
        [replacement] -> do
          let replacementCut = alignmentGenerationCut replacement
          assertEqual
            "replacement planning retains no invalidated predecessor"
            []
            (alignmentCutPredecessorGenerationIds replacementCut)
          assertEqual
            "replacement planning treats the complete member set as fresh"
            (Set.fromList [deltaA, deltaB])
            ( Set.fromList
                (freshMemberBaseDelta <$> alignmentCutFreshMemberBaseEvidence replacementCut)
            )
        generations ->
          assertFailure
            ("replacement plan unexpectedly split its context class: " <> show generations)
    result ->
      assertFailure
        ("fresh Store-incarnation replacement did not recompute its plan: " <> show result)
  where
    retainDebt retainedOccurrence =
      commit
        Alignment.commitStructuralDebtRetention
        "retain same-sort structural debt"
        . Alignment.prepareStructuralDebtRetention
          (cause retainedOccurrence)
          ( normalizeStructuralDebts
              [ structuralConsequenceDebt
                  ( structuralDebtKey
                      (cause retainedOccurrence)
                      TopologyAlignmentDebt
                      typedSort
                      Nothing
                  )
                  (structuralDebtEvidence Set.empty Set.empty Set.empty)
              ]
          )

caseInvalidatedDeltaPlansRecoverAtDeletionCut :: Assertion
caseInvalidatedDeltaPlansRecoverAtDeletionCut = do
  (beforeRemoval, removed) <- settledDeltaControlledRemovalFixture
  let beforeAlignment = startupAlignmentState beforeRemoval
      removedAlignment = startupAlignmentState removed
      retainedBeforeKeys = fmap fst (Alignment.alignmentPromotionEntries beforeAlignment)
      invalidatedKeys =
        filter (promotionCoordinateInvalidated removedAlignment) retainedBeforeKeys
      invalidatedCauseSorts =
        Set.fromList
          [ (Alignment.alignmentPromotionKeyCause key, Alignment.alignmentPromotionKeySort key)
          | key <- invalidatedKeys
          ]
      installedBeforeDeletion =
        GraphProgress.structuralLastInstalledCutId
          (startupStructuralProgressState removed)
  assertBool
    "real Delta removal invalidates at least one previously promoted plan"
    (not (null invalidatedKeys))
  assertBool
    "controlled removal retains every invalidated promotion receipt"
    ( Set.fromList retainedBeforeKeys
        `Set.isSubsetOf` Set.fromList
          (fmap fst (Alignment.alignmentPromotionEntries removedAlignment))
    )

  settled <- settleStructuralPublication removed
  let settledAlignment = startupAlignmentState settled
      deletionCut =
        GraphProgress.structuralLastInstalledCutId
          (startupStructuralProgressState settled)
      settledKeys = fmap fst (Alignment.alignmentPromotionEntries settledAlignment)
      liveReplacementKeys =
        [ key
        | key <- settledKeys,
          Set.member
            (Alignment.alignmentPromotionKeyCause key, Alignment.alignmentPromotionKeySort key)
            invalidatedCauseSorts,
          not (promotionCoordinateInvalidated settledAlignment key)
        ]
      liveDebtCauseSorts =
        Set.fromList
          [ (structuralDebtKeyCause key, structuralDebtKeySort key)
          | debt <- Alignment.liveAlignmentStructuralDebtEntries settledAlignment,
            let key = structuralConsequenceDebtKey debt
          ]
  assertBool
    "the removal installs a distinct successor topology cut"
    (deletionCut /= installedBeforeDeletion)
  assertBool
    "every invalidated cause/sort obtains a live replacement receipt"
    ( invalidatedCauseSorts
        `Set.isSubsetOf` Set.fromList
          [ (Alignment.alignmentPromotionKeyCause key, Alignment.alignmentPromotionKeySort key)
          | key <- liveReplacementKeys
          ]
    )
  assertBool
    "route-mismatched predecessor plans recover at the deletion cut"
    ( all
        ((== deletionCut) . Alignment.alignmentPromotionKeyTopologyCut)
        liveReplacementKeys
    )
  assertEqual
    "the recovered cause/sorts retain no executable structural debt"
    Set.empty
    (Set.intersection invalidatedCauseSorts liveDebtCauseSorts)
  assertBool
    "recovery preserves every pre-removal promotion receipt"
    (Set.fromList retainedBeforeKeys `Set.isSubsetOf` Set.fromList settledKeys)

  (replayed, replayEffects, replayDisposition) <-
    case advanceAlignmentGenerationsWithDisposition settled of
      Left problem -> assertFailure ("repeat recovered alignment pass: " <> show problem)
      Right result -> pure result
  assertEqual
    "the recovered alignment pass is level-idempotent"
    AlignmentGenerationWorkUnchanged
    replayDisposition
  assertBool "the recovered alignment replay preserves whole state" (replayed == settled)
  assertEqual "the recovered alignment replay emits no effects" [] (effectBatchMembers replayEffects)
  where
    promotionCoordinateInvalidated alignment key =
      Alignment.alignmentPlanIsInvalidated
        (Alignment.alignmentPromotionKeySort key)
        (Alignment.alignmentPromotionKeyTopologyCut key)
        (Alignment.alignmentPromotionKeyPlacementVector key)
        alignment

-- Several query sets use one prepared view in forward, reverse and repeated
-- order. Different lower bounds model newly executable debts and changing
-- incumbent coordinates during a fixed point; the reference rebuilds the old
-- candidate list and scans each question independently.
propBatchedRecoverySelectionReference :: Property
propBatchedRecoverySelectionReference =
  forAll (chooseInt (1, 14)) $ \count ->
    forAll (vectorOf count (chooseInt (0, 2))) $ \outcomes ->
      forAll (shuffle [0 .. count - 1]) $ \order ->
        let installed =
              [ checked "batched installed cut" (mkTopologyCutId (fixtureIdentifierBytes (fromIntegral (0xb0 + index))))
              | index <- [0 .. count - 1]
              ]
            missing = checked "missing batched incumbent" (mkTopologyCutId (fixtureIdentifierBytes 0xaf))
            byCut = Map.fromList (zip installed outcomes)
            check cut = case Map.findWithDefault 0 cut byCut of
              0 -> Right False
              1 -> Right True
              _ -> Left cut
            prepared = prepareAlignmentRecoveryCutView installed check
            coordinate position = case drop position installed of
              first : _ -> first
              [] -> missing
            coveringPositions = Nothing : fmap Just order
            incumbentSets =
              [ Set.empty,
                Set.singleton missing,
                Set.fromList [missing, coordinate 0],
                Set.fromList installed
              ]
                <> [Set.singleton (coordinate position) | position <- order]
                <> [Set.fromList [coordinate a, coordinate b] | (a, b) <- zip order (reverse order)]
            queries =
              [ (live, covering, incumbents)
              | live <- [False, True],
                covering <- coveringPositions,
                incumbents <- incumbentSets
              ]
            selected (live, covering, incumbents) =
              alignmentFirstMatchingRecoveryCut id live (coordinate <$> covering) incumbents prepared
            reference (live, covering, incumbents)
              | live = Right (coordinate <$> covering)
              | otherwise = do
                  candidates <- alignmentRecoveryCandidateCuts installed incumbents
                  let covered cut = case covering of
                        Nothing -> False
                        Just lowerBound -> maybe False (>= lowerBound) (elemIndex cut installed)
                  (answer, _) <-
                    alignmentFirstViableRecoveryTopologyCut
                      (\() cut -> (\matches -> (matches, ())) <$> check cut)
                      ()
                      (filter covered candidates)
                  Right answer
         in conjoin
              [ counterexample (show query) (selected query === reference query)
              | query <- queries <> reverse queries <> queries
              ]

caseBatchedRecoverySelectionDemand :: Assertion
caseBatchedRecoverySelectionDemand = do
  let before = topologyId
      matching = successorTopologyId
      after = checked "undemanded later cut" (mkTopologyCutId (fixtureIdentifierBytes 0xae))
      missing = checked "uninstalled incumbent" (mkTopologyCutId (fixtureIdentifierBytes 0xad))
      installed = [before, matching, after]
      unreachable = error "an unreachable projection was evaluated"
      prepared = prepareAlignmentRecoveryCutView installed (\cut -> if cut == matching then Right True else unreachable)
      query = alignmentFirstMatchingRecoveryCut id False (Just matching)
  assertEqual "the suffix skips an earlier unqueried failure" (Right (Just matching)) (query Set.empty prepared)
  assertEqual "repeated answers share the same earliest match" (Right (Just matching)) (query (Set.singleton matching) prepared)
  assertEqual
    "no coverage still validates missing incumbents first"
    (Left missing)
    (alignmentFirstMatchingRecoveryCut id False Nothing (Set.singleton missing) prepared)
  assertEqual
    "no coverage does not inspect any candidate"
    (Right Nothing)
    (alignmentFirstMatchingRecoveryCut id False Nothing Set.empty prepared)
  assertEqual
    "a live incumbent bypasses even the candidate view"
    (Right (Just before))
    (alignmentFirstMatchingRecoveryCut id True (Just before) (Set.singleton missing) unreachable)
  let membershipMismatch =
        prepareAlignmentRecoveryCutView
          installed
          (\cut -> if cut == before then Right False else if cut == matching then Right True else unreachable)
  assertEqual
    "a mismatch advances to the first match without demanding later candidates"
    (Right (Just matching))
    (alignmentFirstMatchingRecoveryCut id False (Just before) Set.empty membershipMismatch)

-- Missing and available vectors, empty scopes, and repeated removal/reinsertion
-- exercise cold cache entries as well as shared answers. The reference rebuilds
-- from the current owner facts for every question rather than retaining a view.
propRetainedCutQueryScope :: Property
propRetainedCutQueryScope =
  forAll (vectorOf 12 ((,,) <$> chooseInt (0, 3) <*> chooseInt (0, 3) <*> chooseInt (0, 3))) $ \scopes ->
    conjoin (walk CutQueries.emptyCutQueryCache (scopes <> reverse scopes <> scopes))
  where
    state = retainedCutQueryFixture
    progress = startupStructuralProgressState state
    installed = GraphProgress.structuralInstalledCutIds progress
    available = checked "complete cache fixture vector" (Placement.currentPhysicalPlacementRevisionVector (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state))) (startupPlacementState state))
    causes = [cause occurrence, cause successorOccurrence]
    sorts = [typedSort, sortOccurrence (SortRegistry.registryEntrySortId entry) (SortRegistry.registryEntryOccurrenceId entry)]
      where
        entry = checkedMaybe "cache fixture predefined sort" (find (const True) (SortRegistry.registryEntries (startupSortRegistryState state)))
    vectors = [available, advancedPlacement]
    select mask choices = Set.fromList [value | (bit, value) <- zip [1, 2] choices, mask `div` bit `mod` 2 == 1]
    prepare (wantedCauses, wantedSorts, wantedVectors) =
      CutQueries.prepareCutQueryCache (structuralReconciliationViews state) progress (startupPlacementState state) wantedCauses wantedSorts wantedVectors
    observe cache =
      ( CutQueries.cutQueryPositions cache,
        fmap (`CutQueries.cutQueryCauseCoverage` cache) causes,
        [ fmap (fmap (fmap observeSort . Map.toAscList)) (CutQueries.cutQueriesAtPlacement vector cache)
        | vector <- vectors
        ]
      )
    observeSort (affectedSort, query) =
      (affectedSort, [CutQueries.alignmentFirstMatchingRecoveryCut CutQueries.cutQueryMissingTopology live covering Set.empty query | live <- [False, True], covering <- Nothing : fmap Just installed])
    walk _ [] = []
    walk predecessor ((causeMask, sortMask, vectorMask) : rest) =
      let scope@(wantedCauses, wantedSorts, wantedVectors) = (select causeMask causes, select sortMask sorts, select vectorMask vectors)
          extended = prepare scope predecessor
          retained = CutQueries.trimCutQueryCache wantedCauses wantedSorts wantedVectors extended
          fresh = prepare scope CutQueries.emptyCutQueryCache
       in counterexample
            ("retained scope " <> show (causeMask, sortMask, vectorMask))
            (conjoin [observe retained === observe fresh, CutQueries.cutQueryRetainedKeys retained === scope])
            : walk retained rest

-- Keep an available bootstrap placement and establish an actual non-genesis
-- cut through the checked report/proposal/acceptance ceremony. Genesis itself
-- is a distinguished predecessor, not an entry in structuralInstalledCutIds.
retainedCutQueryFixture :: HeraldState
retainedCutQueryFixture =
  replaceStartupStructuralProgressState
    (GraphProgress.commitTopologyCutEstablishment establishment)
    withPlacements
  where
    initial = initializedCoordinatorHerald
    local = checkedLocalHeraldEpoch (startupGenesis initial)
    members = fmap heraldMemberEpoch (checkedActiveHeralds (startupGenesis initial))
    remotes = filter (/= local) members
    withPlacements = foldl retainRemote initial remotes
    retainRemote state owner =
      let snapshot = checked "cache fixture remote snapshot" (PlacementProtocol.placementSnapshot owner PlacementProtocol.firstPlacementSequence [])
          prepared = checked "cache fixture remote snapshot admission" (Placement.prepareRemotePlacement (FullPlacementSnapshot snapshot) (startupPlacementState state))
       in replaceStartupPlacementState (fst (Placement.commitRemotePlacement prepared)) state
    control = controlIndex 1
    advanced =
      fst
        ( GraphProgress.commitStructuralControlProgress
            (checked "cache fixture control prefix" (GraphProgress.prepareStructuralControlProgress control (startupStructuralProgressState withPlacements)))
        )
    reports =
      [ GraphProtocol.structuralAppliedReport
          owner
          (GraphProtocol.structuralAppliedReportVersionVector (GraphProgress.structuralLocalReport advanced))
          control
      | owner <- remotes
      ]
    reported = foldl retainReport advanced reports
    retainReport progress report =
      fst
        ( GraphProgress.commitStructuralReport
            (checked "cache fixture remote structural report" (GraphProgress.prepareStructuralReport report progress))
        )
    checkpoint = GraphProgress.topologyControlCheckpointWithConsequence control True "retained-cut-query-fixture"
    proposal = case checked
      "cache fixture topology proposal"
      (GraphProgress.prepareTopologyCutProposal (structuralReconciliationViews withPlacements) checkpoint reported) of
      GraphProgress.TopologyCutProposalReady prepared -> prepared
      GraphProgress.TopologyCutProposalHeld dependencies -> error ("cache fixture cut unexpectedly held: " <> show dependencies)
      GraphProgress.TopologyCutProposalUnavailable -> error "cache fixture cut unexpectedly unavailable"
    announced = GraphProgress.commitTopologyCutProposal proposal
    cut = topologyCutAnnounceId (GraphProgress.preparedTopologyCutProposalAnnounce proposal)
    accepted = foldl retainAcceptance announced reports
    retainAcceptance progress report =
      GraphProgress.commitTopologyCutAcceptance
        (checked "cache fixture remote cut acceptance" (GraphProgress.prepareTopologyCutAcceptance (topologyCutAcceptance cut report) progress))
    establishment = checked "cache fixture established cut" (GraphProgress.prepareTopologyCutEstablishment accepted)

cachePreparedFor :: HeraldState -> CutQueries.CutQueryCache
cachePreparedFor state =
  CutQueries.prepareCutQueryCache
    (structuralReconciliationViews state)
    (startupStructuralProgressState state)
    (startupPlacementState state)
    (Set.fromList [cause occurrence, cause successorOccurrence])
    (Set.singleton typedSort)
    (Set.fromList [placement, advancedPlacement])
    CutQueries.emptyCutQueryCache

caseRetainedCutQueryReuse :: Assertion
caseRetainedCutQueryReuse = do
  let state = retainedCutQueryFixture
      progress = startupStructuralProgressState state
      vector = checked "reuse fixture vector" (Placement.currentPhysicalPlacementRevisionVector (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state))) (startupPlacementState state))
      causes = Set.singleton (cause occurrence)
      sorts = Set.singleton typedSort
      vectors = Set.singleton vector
      prepared = CutQueries.prepareCutQueryCache (structuralReconciliationViews state) progress (startupPlacementState state) causes sorts vectors CutQueries.emptyCutQueryCache
      observe cache =
        ( CutQueries.cutQueryPositions cache,
          CutQueries.cutQueryCauseCoverage (cause occurrence) cache,
          fmap (fmap (fmap (CutQueries.alignmentFirstMatchingRecoveryCut CutQueries.cutQueryMissingTopology False (Just (GraphProgress.structuralLastInstalledCutId progress)) Set.empty))) (CutQueries.cutQueriesAtPlacement vector cache)
        )
      expected = observe prepared
      reused =
        CutQueries.prepareCutQueryCache
          (error "reused cut query reconstructed reconciliation views")
          (error "reused cut query rescanned structural history")
          (error "reused cut query reread placement history")
          causes
          sorts
          vectors
          prepared
  assertBool
    "the fixture reaches a route-matching historical projection"
    ( case CutQueries.cutQueriesAtPlacement vector prepared of
        Just (Right queries) ->
          all
            (== Right (Just (GraphProgress.structuralLastInstalledCutId progress)))
            [CutQueries.alignmentFirstMatchingRecoveryCut CutQueries.cutQueryMissingTopology False (Just (GraphProgress.structuralLastInstalledCutId progress)) Set.empty query | query <- Map.elems queries]
        _ -> False
    )
  assertEqual "the existing core, coverage and suffix answers are reused" expected (observe reused)

-- A generation poll must still read the physical owners' notification fields.
-- Poison only its cut-preparation callback, while testing both newly harvested
-- notifications and an already parked fixture with identical semantic owners.
propIdleGenerationSkipsCutPreparation :: Property
propIdleGenerationSkipsCutPreparation =
  forAll (chooseInt (1, 20)) $ \count -> ioProperty $ do
    let original = withConsumedGenerationInputChanges retainedCutQueryFixture
        cache = cachePreparedFor original
        warm = replaceStartupAlignmentCutQueryCache cache original
        withChanges = replaceStartupAlignmentCutQueryCache cache retainedCutQueryFixture
        positions = CutQueries.cutQueryPositions cache
        (causes, _, _) = CutQueries.cutQueryRetainedKeys cache
        guarded = advanceAlignmentGenerationsWithPreparationForTest (\_ -> error "idle generation pass prepared fresh cut queries")
        advance run state = do
          (successor, effects, disposition) <- either (assertFailure . show) pure (run state)
          assertEqual "idle pass reports no generation work" AlignmentGenerationWorkUnchanged disposition
          assertEqual "idle pass emits no effects" [] (effectBatchMembers effects)
          assertEqual "idle pass preserves the exact Alignment owner" (startupAlignmentState original) (startupAlignmentState successor)
          assertEqual "idle Alignment scheduling remains valid" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState successor))
          let retained = startupAlignmentCutQueryCache successor
          assertEqual "idle pass preserves installed structural metadata" positions (CutQueries.cutQueryPositions retained)
          assertEqual "idle pass retires vector queries without reconstructing current placement" (causes, Set.empty, Set.empty) (CutQueries.cutQueryRetainedKeys retained)
          pure successor
    unpoisoned <- advance advanceAlignmentGenerationsWithDisposition warm
    assertBool "the ordinary idle pass preserves the exact whole Herald" (unpoisoned == original)
    consumed <- advance guarded withChanges
    assertBool "new physical notifications are consumed without cut preparation" (consumed == original)
    _ <- foldM (\state _ -> advance guarded state) warm [1 .. count]
    pure True

-- Expected notification consumption only. Preserve every other metadata field,
-- work registration, dependency relation and semantic owner fact.
withConsumedGenerationInputChanges :: HeraldState -> HeraldState
withConsumedGenerationInputChanges state =
  replaceStartupStructuralProgressState (GraphProgress.clearAlignmentPlanChange (startupStructuralProgressState state))
    . replaceStartupGraphState (Graph.clearAlignmentPlanChange (startupGraphState state))
    . replaceStartupSortRegistryState (SortRegistry.clearAlignmentPlanChange (startupSortRegistryState state))
    . replaceStartupPlacementState (Placement.clearAlignmentPlanChange (startupPlacementState state))
    $ state

-- Exact accepted/readiness/certificate replay and already-queued blocked
-- evidence all leave the query inventory untouched. The guarded source read
-- would fail if eager preparation were reintroduced before disposition is known.
caseUnchangedEvidenceSkipsCutPreparation :: Assertion
caseUnchangedEvidenceSkipsCutPreparation = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (alignment, _, accepted, ready, certificate) <- generationFastPathFixture local remote
  let withAcceptance =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain bypass-test acceptance"
          (Alignment.prepareAlignmentCutAcceptance accepted alignment)
      complete =
        commit
          Alignment.commitHistoricalCertificate
          "retain bypass-test certificate"
          (Alignment.prepareHistoricalCertificate certificate withAcceptance)
      acceptedControl = AlignmentCutAcceptanceAdvertised accepted
      blocked = retainPendingControl remote acceptedControl Alignment.emptyState
      scenarios =
        [ ("accepted cut", acceptedControl, complete),
          ("member readiness", AlignmentMemberReadyAdvertised ready, complete),
          ("historical certificate", AlignmentHistoricalCertificateAdvertised certificate, complete),
          ("already queued blocked acceptance", acceptedControl, blocked)
        ]
  mapM_ checkScenario scenarios
  where
    checkScenario (label, control, alignment) = do
      (connected, binding) <- connectedWithAlignment alignment
      let cache = cachePreparedFor connected
          warm = replaceStartupAlignmentCutQueryCache cache connected
          poisoned =
            replaceStartupAlignmentCutQueryCache
              cache
              (replaceStartupPlacementState (error (label <> " replay prepared fresh placement queries")) warm)
          apply state = case applyAlignmentGenerationControlWithDisposition binding control state of
            Left problem -> assertFailure (label <> ": " <> show problem)
            Right Nothing -> assertFailure (label <> ": no generation-control handoff")
            Right (Just result) -> pure result
      (expected, expectedEffects, expectedDisposition) <- apply warm
      (observed, effects, disposition) <- apply poisoned
      assertBool (label <> " is semantically idempotent") (expected == warm)
      assertEqual (label <> " preserves Alignment owner") (startupAlignmentState expected) (startupAlignmentState observed)
      assertEqual (label <> " preserves exact response effects") (effectBatchMembers expectedEffects) (effectBatchMembers effects)
      assertEqual (label <> " preserves evidence disposition") expectedDisposition disposition
      assertEqual
        (label <> " preserves the retained query inventory")
        (CutQueries.cutQueryRetainedKeys cache)
        (CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache observed))

propMarkerReleaseRetainsCutQueries :: Property
propMarkerReleaseRetainsCutQueries =
  forAll (chooseInt (1, 24)) $ \repetitions -> ioProperty $ do
    let original = retainedCutQueryFixture
        cache = cachePreparedFor original
        warm = replaceStartupAlignmentCutQueryCache cache original
        positions = CutQueries.cutQueryPositions cache
        scope = CutQueries.cutQueryRetainedKeys cache
        release state _ = do
          let (successor, effects, disposition) = checked "release held route markers" (StructuralCoordinator.releasePendingAlignmentRouteCutoversWithDisposition state)
          assertEqual "no pending route marker remains held" StructuralCoordinator.PendingAlignmentRouteCutoversHeld disposition
          assertEqual "no pending route marker emits no effect" [] (effectBatchMembers effects)
          assertBool "held release preserves complete semantic Herald state" (successor == state)
          let retained = startupAlignmentCutQueryCache successor
          assertEqual "held release preserves installed-cut metadata" positions (CutQueries.cutQueryPositions retained)
          assertEqual "held release preserves all cached query coordinates" scope (CutQueries.cutQueryRetainedKeys retained)
          pure successor
    _ <- foldM release warm [1 .. repetitions]
    pure True

caseRetainedCutQueryInvalidation :: Assertion
caseRetainedCutQueryInvalidation = do
  let original = retainedCutQueryFixture
      cache = cachePreparedFor original
      warm = replaceStartupAlignmentCutQueryCache cache original
      positions = CutQueries.cutQueryPositions cache
      scope = CutQueries.cutQueryRetainedKeys cache
      (causes, _, _) = scope
      invalidators =
        [ ("structural progress", replaceStartupStructuralProgressState (startupStructuralProgressState warm)),
          ("effective sorts", replaceStartupSortRegistryState (startupSortRegistryState warm)),
          ("graph baseline", replaceStartupGraphState (startupGraphState warm))
        ]
  assertBool "warming derived queries preserves semantic Herald equality" (warm == original)
  assertBool "fixture really prepared installed-cut metadata" (not (Map.null positions))
  mapM_
    ( \(label, replace) -> do
        let changed = replace warm
            retained = startupAlignmentCutQueryCache changed
        assertBool (label <> " replacement preserves unchanged owner semantics") (changed == original)
        assertEqual (label <> " clears structural answers") Map.empty (CutQueries.cutQueryPositions retained)
        assertEqual (label <> " clears every query coordinate") (Set.empty, Set.empty, Set.empty) (CutQueries.cutQueryRetainedKeys retained)
    )
    invalidators
  let afterPlacement = replaceStartupPlacementState (startupPlacementState warm) warm
      placementCache = startupAlignmentCutQueryCache afterPlacement
  assertBool "placement replacement still preserves semantic equality" (afterPlacement == original)
  assertEqual "placement replacement preserves structural work" positions (CutQueries.cutQueryPositions placementCache)
  assertEqual "placement replacement drops vector/sort answers only" (causes, Set.empty, Set.empty) (CutQueries.cutQueryRetainedKeys placementCache)
  mapM_
    ( \changed -> do
        let retained = startupAlignmentCutQueryCache changed
        assertEqual "unrelated owner traffic preserves structural work" positions (CutQueries.cutQueryPositions retained)
        assertEqual "unrelated owner traffic preserves query scope" scope (CutQueries.cutQueryRetainedKeys retained)
    )
    [ replaceStartupAlignmentState (startupAlignmentState warm) warm,
      replaceStartupOracleProjectionState (startupOracleProjectionState warm) warm,
      replaceStartupStoreState (startupStoreState warm) warm
    ]

-- Exercise the real regular-sort receiver before and after installing a delayed
-- placement snapshot. This changes a cached placement failure into an available
-- exact vector, while the structural history remains unchanged.
caseRetainedCutQueryTransitions :: Assertion
caseRetainedCutQueryTransitions = do
  (affectedSort, authored, receiving, rawReceiver) <- pendingAnchorCustomSortFixture
  _ <- case liveLatestAlignmentGenerationsForSort affectedSort (startupAlignmentState authored) of
    [value] -> pure value
    values -> assertFailure ("expected one real cached-anchor generation: " <> show values)
  let currentPlan = checkedMaybe "current cached routing plan" (Alignment.currentAlignmentPlanForSort affectedSort (startupAlignmentState authored))
      announce = AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement currentPlan)
      withoutPlacement = replaceStartupPlacementState (startupPlacementState rawReceiver) receiving
      vector = Plan.alignmentPlanIdPlacement (Plan.alignmentPlanId currentPlan)
  (connected, binding) <- connectPendingAnchorSource withoutPlacement
  let scopeCauses = Set.fromList [structuralDebtKeyCause (structuralConsequenceDebtKey debt) | debt <- structuralDebtSetEntries (Alignment.alignmentStructuralDebts (startupAlignmentState connected))]
      warmCache =
        CutQueries.prepareCutQueryCache
          (structuralReconciliationViews connected)
          (startupStructuralProgressState connected)
          (startupPlacementState connected)
          scopeCauses
          (Set.singleton affectedSort)
          (Set.singleton vector)
          CutQueries.emptyCutQueryCache
      warm = replaceStartupAlignmentCutQueryCache warmCache connected
      cold state = replaceStartupAlignmentCutQueryCache CutQueries.emptyCutQueryCache state
      admit state = applyAlignmentGenerationControlWithDisposition binding announce state
      assertAdvance state =
        assertBool
          "warm/cold generation advancement is identical"
          (advanceAlignmentGenerationsWithDisposition state == advanceAlignmentGenerationsWithDisposition (cold state))
  assertBool
    "missing vector really yields a cached placement failure"
    (case CutQueries.cutQueriesAtPlacement vector warmCache of Just (Left (CutQueries.CutQueryPlacementProblem _)) -> True; _ -> False)
  assertBool "warm/cold pending-anchor admission is identical" (admit warm == admit (cold warm))
  pending <- case admit warm of
    Right (Just (state, _, _)) -> pure state
    result -> assertFailure ("expected pending-anchor admission, got " <> either show (const "no handoff") result)
  assertBool
    "the unavailable input vector stays retained while its announcement is queued"
    (Set.member vector (let (_, _, vectors) = CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache pending) in vectors))
  assertAdvance pending
  let (_, advertisement) = Placement.commitLocalSnapshot (checked "cached anchor source snapshot" (Placement.prepareRetainedLocalSnapshot (startupPlacementState authored)))
      snapshot = Placement.localSnapshotMessage advertisement
      installedPlacement = fst (Placement.commitRemotePlacement (checked "install delayed cached anchor placement" (Placement.prepareRemotePlacement (FullPlacementSnapshot snapshot) (startupPlacementState pending))))
      withPlacement = replaceStartupPlacementState installedPlacement pending
  assertBool
    "the delayed snapshot invalidates the former placement failure"
    (case CutQueries.cutQueriesAtPlacement vector (startupAlignmentCutQueryCache withPlacement) of Nothing -> True; _ -> False)
  assertAdvance withPlacement
  assertBool "warm/cold released-anchor admission is identical" (admit withPlacement == admit (cold withPlacement))

caseVirginSelectionUsesEarliestMatchingCut :: Assertion
caseVirginSelectionUsesEarliestMatchingCut = do
  let oldCut = topologyId
      matchingCut = successorTopologyId
      laterCut = checked "virgin later cut" (mkTopologyCutId (fixtureIdentifierBytes 0xd4))
      installed = [oldCut, matchingCut, laterCut]
      candidates = checked "virgin candidates have no incumbent lower bound" (alignmentRecoveryCandidateCuts installed Set.empty)
      routeMatches = Map.fromList [(oldCut, False), (matchingCut, True), (laterCut, True)]
      select matches = alignmentFirstViableRecoveryTopologyCut (check matches) []
      check :: Map.Map TopologyCutId Bool -> [TopologyCutId] -> TopologyCutId -> Either String (Bool, [TopologyCutId])
      check matches visited candidate = Right (Map.findWithDefault False candidate matches, visited <> [candidate])
      selected = checked "select virgin cut" (select routeMatches candidates)
  assertEqual "virgin selection considers the entire installed chain" installed candidates
  assertEqual
    "the old route mismatch cannot pin virgin debt forever"
    (Just matchingCut, [oldCut, matchingCut])
    selected
  assertEqual
    "an installed suffix cannot change the first matching identity"
    selected
    (checked "select without later suffix" (select routeMatches (init candidates)))
  assertEqual
    "replaying the same announced placement vector selects the identical cut"
    selected
    (checked "select anchor vector" (select routeMatches candidates))
  assertEqual
    "an unavailable matching topology stays pending after inspecting every cut"
    (Nothing, installed)
    (checked "unavailable virgin placement" (select Map.empty candidates))
  assertEqual
    "an earlier projection failure cannot be hidden by a later matching cut"
    (Left "projection unavailable" :: Either String (Maybe TopologyCutId, [TopologyCutId]))
    ( alignmentFirstViableRecoveryTopologyCut
        (\visited candidate -> if candidate == oldCut then Left "projection unavailable" else check routeMatches visited candidate)
        []
        candidates
    )

caseVirginPromotionPreservesPredecessorImports :: Assertion
caseVirginPromotionPreservesPredecessorImports = do
  (ready, predecessor, _, _, certificate) <- generationFastPathFixture heraldA heraldB
  let certified =
        commit
          Alignment.commitHistoricalCertificate
          "admit predecessor certificate"
          (Alignment.prepareHistoricalCertificate certificate ready)
      predecessorId = alignmentGenerationId predecessor
      selectedTopology =
        checked
          "coalesced successor topology"
          ( topologyCut
              (sameGenerationPredecessor (deriveTopologyCutId (alignmentCutTopologyCut (alignmentGenerationCut predecessor))))
              (topologyFrontier (emptyStructuralVersionVector membershipGeneration) (controlIndex 3))
              (deriveTopologyOccurrenceDigest "coalesced-virgin-successor")
          )
      selectedCut = deriveTopologyCutId selectedTopology
      successorCause = cause successorOccurrence
      thirdCause = cause (structuralOccurrenceId heraldA (checked "third structural sequence" (mkStructuralSequence 3)))
      causes = [successorCause, thirdCause]
      retainDebt retainedCause =
        commit Alignment.commitStructuralDebtRetention "retain coalesced virgin cause"
          . Alignment.prepareStructuralDebtRetention
            retainedCause
            ( normalizeStructuralDebts
                [ structuralConsequenceDebt
                    (structuralDebtKey retainedCause TopologyAlignmentDebt typedSort Nothing)
                    (structuralDebtEvidence Set.empty Set.empty Set.empty)
                ]
            )
      withDebts = foldr retainDebt certified causes
      preparePlan certificates =
        prepareAlignmentGenerationPlan typedSort selectedTopology advancedPlacement contextGraphFixture generationMembers [predecessor] [] certificates
  case preparePlan Set.empty of
    Right (AlignmentGenerationHeld missing) ->
      assertEqual "cut selection cannot bypass an incomplete live predecessor" (Set.singleton predecessorId) missing
    result -> assertFailure ("expected predecessor certificate prerequisite: " <> show result)
  plan <- case preparePlan (Set.singleton predecessorId) of
    Right (AlignmentGenerationReady value) -> pure value
    result -> assertFailure ("certified coalesced successor was not ready: " <> show result)
  generation <- case alignmentGenerationPlanGenerations plan of
    [value] -> pure value
    values -> assertFailure ("expected one successor class: " <> show values)
  let generationId = alignmentGenerationId generation
      promote retainedCause =
        commit Alignment.commitAlignmentPromotion "promote coalesced virgin cause"
          . Alignment.prepareAlignmentPromotion heraldA retainedCause typedSort selectedCut advancedPlacement plan
      afterFirst = promote successorCause withDebts
      afterBoth = promote thirdCause afterFirst
      imports alignment =
        [ key
        | (key, _) <- Alignment.bootstrapImportEntries alignment,
          Alignment.bootstrapImportKeyGeneration key == generationId
        ]
      receipts = Alignment.alignmentPromotionEntries afterBoth
  assertEqual "the selected predecessor remains part of successor ancestry" [predecessorId] (alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation))
  assertEqual "represented members are not reset to empty fresh bases" [] (alignmentCutFreshMemberBaseEvidence (alignmentGenerationCut generation))
  assertEqual "the local member imports its certified predecessor exactly once" 1 (length (imports afterBoth))
  assertEqual "both causes reuse the same immutable bootstrap import" (imports afterFirst) (imports afterBoth)
  assertBool
    "every successor import names the retained predecessor"
    (all ((== Alignment.BootstrapFromPredecessor predecessorId) . Alignment.bootstrapImportKeySource) (imports afterBoth))
  assertEqual "the exact admitted predecessor certificate survives promotion" (Alignment.alignmentHistoricalCertificateEntries certified) (Alignment.alignmentHistoricalCertificateEntries afterBoth)
  assertBool "all earlier receipts remain retained" (all (`elem` receipts) (Alignment.alignmentPromotionEntries certified))
  assertEqual
    "each coalesced cause retains its own exact promotion receipt"
    (Set.fromList causes)
    ( Set.fromList
        [ Alignment.alignmentPromotionKeyCause key
        | (key, _) <- receipts,
          Alignment.alignmentPromotionKeyTopologyCut key == selectedCut
        ]
    )
  assertEqual "coalesced promotion remains internally valid" (Right ()) (Alignment.validateAlignmentState afterBoth)
  assertEqual "exact replay allocates no new import or receipt" afterBoth (promote thirdCause afterBoth)

caseRecoverySelectionIgnoresLaterInstalledSuffix :: Assertion
caseRecoverySelectionIgnoresLaterInstalledSuffix = do
  let originalCut = checked "recovery original cut" (mkTopologyCutId (fixtureIdentifierBytes 0xd1))
      deletionCut = checked "recovery deletion cut" (mkTopologyCutId (fixtureIdentifierBytes 0xd2))
      laterCut = checked "recovery later cut" (mkTopologyCutId (fixtureIdentifierBytes 0xd3))
      installed = [originalCut, deletionCut, laterCut]
      firstIncumbent = Set.singleton originalCut
      routeMatches =
        Map.fromList
          [ (originalCut, False),
            (deletionCut, True),
            (laterCut, True)
          ]
  candidates <- recoveryCandidates installed firstIncumbent
  assertEqual
    "recovery starts at the latest incumbent coordinate inclusively"
    installed
    candidates
  (currentSelection, currentVisited) <- select routeMatches candidates
  (anchorSelection, anchorVisited) <- select routeMatches candidates
  (selectionWithoutSuffix, _) <- select routeMatches (init candidates)
  assertEqual
    "current-vector recovery selects the first route-matching cut"
    (Just deletionCut)
    currentSelection
  assertEqual
    "the supplied anchor vector makes the identical selection"
    currentSelection
    anchorSelection
  assertEqual
    "a later installed cut is not inspected after the canonical match"
    [originalCut, deletionCut]
    currentVisited
  assertEqual
    "anchor replay stops at the same canonical match"
    currentVisited
    anchorVisited
  assertEqual
    "installing an unrelated suffix cannot change recovery identity"
    currentSelection
    selectionWithoutSuffix
  assertBool
    "the canonical deletion-cut anchor is the next replay target"
    ( alignmentReplayTargetIsNext
        deletionCut
        [ (deletionCut, [AlignmentCausePromotionDue]),
          (laterCut, [AlignmentCausePromotionDue])
        ]
    )
  assertBool
    "a later anchor cannot overtake the canonical deletion-cut target"
    ( not
        ( alignmentReplayTargetIsNext
            laterCut
            [ (deletionCut, [AlignmentCausePromotionDue]),
              (laterCut, [AlignmentCausePromotionDue])
            ]
        )
    )

  repeatedCandidates <-
    recoveryCandidates installed (Set.fromList [originalCut, deletionCut])
  (repeatedSelection, repeatedVisited) <-
    select
      ( Map.fromList
          [ (originalCut, True),
            (deletionCut, False),
            (laterCut, True)
          ]
      )
      repeatedCandidates
  assertEqual
    "a repeated recovery cannot scan behind its latest incumbent"
    [deletionCut, laterCut]
    repeatedCandidates
  assertEqual
    "a repeated recovery advances without topology-time regression"
    (Just laterCut)
    repeatedSelection
  assertEqual
    "the obsolete earlier match is never reconsidered"
    [deletionCut, laterCut]
    repeatedVisited

  case alignmentFirstViableRecoveryTopologyCut
    (faultingCheck originalCut)
    []
    candidates of
    Left problem -> assertEqual "an earlier projection fault is not skipped" "held" problem
    Right result ->
      assertFailure
        ("projection fault incorrectly selected a later recovery cut: " <> show result)
  where
    recoveryCandidates installed incumbents =
      case alignmentRecoveryCandidateCuts installed incumbents of
        Left missing -> assertFailure ("recovery incumbent cut is absent: " <> show missing)
        Right candidates -> pure candidates

    select matches candidates =
      case alignmentFirstViableRecoveryTopologyCut check [] candidates of
        Left problem -> assertFailure problem
        Right result -> pure result
      where
        check ::
          [TopologyCutId] ->
          TopologyCutId ->
          Either String (Bool, [TopologyCutId])
        check visited cut =
          Right (Map.findWithDefault False cut matches, visited <> [cut])

    faultingCheck ::
      TopologyCutId ->
      [TopologyCutId] ->
      TopologyCutId ->
      Either String (Bool, [TopologyCutId])
    faultingCheck blocked visited cut
      | cut == blocked = Left "held"
      | otherwise = Right (True, visited <> [cut])

caseMixedControlCauseOrder :: Assertion
caseMixedControlCauseOrder = do
  plan <- case prepareAlignmentGenerationPlan
    typedSort
    topology
    placement
    oneWayContextGraph
    generationMembers
    []
    []
    Set.empty of
    Right (AlignmentGenerationReady ready) -> pure ready
    result -> assertFailure ("mixed-cause generation plan was not ready: " <> show result)
  let olderEnd =
        checked
          "older mixed End cause"
          (processEndCause mixedProcess (controlIndex 3))
      newerLabel =
        checked
          "newer mixed label cause"
          (labelReleaseCause mixedDecision (controlIndex 7))
      coveringFrontier =
        topologyFrontier
          (emptyStructuralVersionVector membershipGeneration)
          (controlIndex 7)
      ordered =
        checked
          "mixed-cause projection order"
          ( alignmentCausesInProjectionOrder
              coveringFrontier
              (Set.fromList [newerLabel, olderEnd])
          )
      withDebts = foldl (flip retainCauseDebt) Alignment.emptyState ordered
      promoted = foldl (promote plan) withDebts ordered
      closing = Set.fromList (Alignment.alignmentClosingObligationIds promoted)
      liveCauses =
        [ alignmentObligationCause obligation
        | (identifier, obligation) <- Alignment.activeAlignmentObligationEntries promoted,
          Set.notMember identifier closing
        ]
  assertEqual
    "control causes are globally ordered by their exact index"
    [olderEnd, newerLabel]
    ordered
  assertBool "the directional plan creates live destination work" (not (null liveCauses))
  assertBool
    "the final non-closing work reflects the newer control fact"
    (all (== newerLabel) liveCauses)
  assertEqual "both ordered control facts receive coverage" (Set.fromList [olderEnd, newerLabel]) (Set.fromList (map (Alignment.alignmentPromotionKeyCause . fst) (Alignment.alignmentPromotionEntries promoted)))
  where
    promote plan state consequence =
      fst
        ( Alignment.commitAlignmentPromotion
            ( checked
                "promote mixed control cause"
                ( Alignment.prepareAlignmentPromotion
                    heraldB
                    consequence
                    typedSort
                    topologyId
                    placement
                    plan
                    state
                )
            )
        )

retainCauseDebt :: StructuralConsequenceCause -> Alignment.State -> Alignment.State
retainCauseDebt consequence state =
  fst
    ( Alignment.commitStructuralDebtRetention
        ( checked
            "retain cause-qualified mixed debt"
            ( Alignment.prepareStructuralDebtRetention
                consequence
                ( normalizeStructuralDebts
                    [ structuralConsequenceDebt
                        (structuralDebtKey consequence TopologyAlignmentDebt typedSort Nothing)
                        (structuralDebtEvidence Set.empty Set.empty Set.empty)
                    ]
                )
                state
            )
        )
    )

oneWayContextGraph :: ContextGraph
oneWayContextGraph =
  checked
    "derive one-way context graph"
    ( deriveContextGraph
        [DeltaVertex deltaA, DeltaVertex deltaB]
        [edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve]
        [deltaA, deltaB]
    )

initializedWithAlignment :: Alignment.State -> HeraldState
initializedWithAlignment alignment =
  replaceStartupAlignmentState
    ( Alignment.replaceAlignmentTransferState
        (Transfer.seedInputClosureMembership (OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView (startupOracleProjectionState initial))) (Alignment.alignmentTransferState alignment))
        alignment
    )
    initial
  where
    initial = initializedCoordinatorHerald

initializedCoordinatorHerald :: HeraldState
initializedCoordinatorHerald =
  fst
    ( checked
        "initialize alignment coordinator Herald"
        ( initialHerald
            (monotonicInstant 0)
            fixtureStep14CheckedGenesis
            fixtureCheckedInitialBootstraps
            fixtureOracleContacts
            fixtureGeneratorSeed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    )

readyAlignmentFixture ::
  HeraldEpoch ->
  HeraldEpoch ->
  IO (Alignment.State, AlignmentGeneration)
readyAlignmentFixture local remote = do
  let members = local :| [remote]
      fixtureMembership =
        checked
          "readiness-ack membership generation"
          ( Membership.genesisHeraldMembershipGeneration
              (checkedSystemId fixtureStep14CheckedGenesis)
              members
          )
      fixturePlacement =
        checked
          "readiness-ack placement vector"
          ( physicalPlacementRevisionVector
              fixtureMembership
              [ (remote, firstPlacementRevision),
                (local, firstPlacementRevision)
              ]
          )
      fixtureTopology =
        checked
          "readiness-ack topology cut"
          ( topologyCut
              (sameGenerationPredecessor predecessorCut)
              ( topologyFrontier
                  (emptyStructuralVersionVector fixtureMembership)
                  (controlIndex 0)
              )
              (deriveTopologyOccurrenceDigest "readiness-ack-topology")
          )
      memberInputs =
        [ generationMemberInput deltaA storeA local initialStoreRevision,
          generationMemberInput deltaB storeB remote initialStoreRevision
        ]
  plan <-
    case prepareAlignmentGenerationPlan
      typedSort
      fixtureTopology
      fixturePlacement
      contextGraphFixture
      memberInputs
      []
      []
      Set.empty of
      Right (AlignmentGenerationReady prepared) -> pure prepared
      result -> assertFailure ("readiness-ack generation plan was not ready: " <> show result)
  generation <- case alignmentGenerationPlanGenerations plan of
    [onlyGeneration] -> pure onlyGeneration
    generations ->
      assertFailure
        ("expected one readiness-ack generation, got " <> show (length generations))
  let debts =
        normalizeStructuralDebts
          [ structuralConsequenceDebt
              (structuralDebtKey (cause occurrence) TopologyAlignmentDebt typedSort Nothing)
              (structuralDebtEvidence Set.empty Set.empty Set.empty)
          ]
      withDebt =
        commit
          Alignment.commitStructuralDebtRetention
          "retain readiness-ack structural debt"
          (Alignment.prepareStructuralDebtRetention (cause occurrence) debts Alignment.emptyState)
      promoted =
        commit
          Alignment.commitAlignmentPromotion
          "promote readiness-ack structural debt"
          ( Alignment.prepareAlignmentPromotion
              (alignmentGenerationAnnouncer generation)
              (cause occurrence)
              typedSort
              (deriveTopologyCutId fixtureTopology)
              fixturePlacement
              plan
              withDebt
          )
      preparedReady =
        checked
          "prepare local readiness evidence"
          ( Alignment.prepareLocalClassMemberReady
              (alignmentGenerationId generation)
              storeA
              (deriveMemberReadyEvidenceDigest "ready-for-ack")
              initialStoreRevision
              promoted
          )
      withReady = fst (Alignment.commitLocalClassMemberReady preparedReady)
  pure (withReady, generation)

promotedFixture :: IO (Alignment.State, AlignmentGeneration)
promotedFixture = do
  plan <- generationPlan
  generation <- case alignmentGenerationPlanGenerations plan of
    [onlyGeneration] -> pure onlyGeneration
    generations ->
      assertFailure
        ("expected one context generation, got " <> show (length generations))
  let debts =
        normalizeStructuralDebts
          [ structuralConsequenceDebt
              (structuralDebtKey (cause occurrence) TopologyAlignmentDebt typedSort Nothing)
              (structuralDebtEvidence Set.empty Set.empty Set.empty)
          ]
      withDebt =
        fst
          ( Alignment.commitStructuralDebtRetention
              ( checked
                  "retain structural debt"
                  (Alignment.prepareStructuralDebtRetention (cause occurrence) debts Alignment.emptyState)
              )
          )
      promoted =
        fst
          ( Alignment.commitAlignmentPromotion
              ( checked
                  "promote structural debt"
                  ( Alignment.prepareAlignmentPromotion
                      heraldA
                      (cause occurrence)
                      typedSort
                      topologyId
                      placement
                      plan
                      withDebt
                  )
              )
          )
  pure (promoted, generation)

successorGenerationFixture :: IO (Alignment.State, AlignmentGeneration)
successorGenerationFixture = do
  (predecessor, predecessorGeneration) <- promotedFixture
  plan <- case prepareAlignmentGenerationPlan
    typedSort
    successorTopology
    placement
    contextGraphFixture
    generationMembers
    [predecessorGeneration]
    []
    (Set.singleton (alignmentGenerationId predecessorGeneration)) of
    Right (AlignmentGenerationReady ready) -> pure ready
    result -> assertFailure ("successor generation plan was not ready: " <> show result)
  generation <- case alignmentGenerationPlanGenerations plan of
    [onlyGeneration] -> pure onlyGeneration
    generations ->
      assertFailure
        ("expected one successor context generation, got " <> show (length generations))
  let debts =
        normalizeStructuralDebts
          [ structuralConsequenceDebt
              ( structuralDebtKey
                  (cause successorOccurrence)
                  TopologyAlignmentDebt
                  typedSort
                  Nothing
              )
              (structuralDebtEvidence Set.empty Set.empty Set.empty)
          ]
      withDebt =
        commit
          Alignment.commitStructuralDebtRetention
          "retain successor structural debt"
          ( Alignment.prepareStructuralDebtRetention
              (cause successorOccurrence)
              debts
              predecessor
          )
      promoted =
        commit
          Alignment.commitAlignmentPromotion
          "promote successor structural debt"
          ( Alignment.prepareAlignmentPromotion
              heraldA
              (cause successorOccurrence)
              typedSort
              successorTopologyId
              placement
              plan
              withDebt
          )
  pure (promoted, generation)

routeProjectionAlignmentFixture ::
  HeraldEpoch ->
  HeraldEpoch ->
  IO (Alignment.State, AlignmentGeneration, AlignmentRouteCutoverMarker)
routeProjectionAlignmentFixture = routeProjectionAlignmentFixtureFrom False

-- The stronger retirement-work fixture certifies a remote-only predecessor
-- before adding the local member. Its remote readiness needs no local Store
-- fulfillment; the local successor cutover is still produced by the coordinator.
routeProjectionAlignmentFixtureFrom ::
  Bool ->
  HeraldEpoch ->
  HeraldEpoch ->
  IO (Alignment.State, AlignmentGeneration, AlignmentRouteCutoverMarker)
routeProjectionAlignmentFixtureFrom certifiedRemotePredecessor local retired = do
  let members = local :| [retired]
      membershipValue =
        checked
          "route-projection membership generation"
          ( Membership.genesisHeraldMembershipGeneration
              (checkedSystemId fixtureStep14CheckedGenesis)
              members
          )
      placementValue =
        checked
          "route-projection placement vector"
          ( physicalPlacementRevisionVector
              membershipValue
              [ (retired, firstPlacementRevision),
                (local, firstPlacementRevision)
              ]
          )
      firstTopology =
        checked
          "route-projection predecessor topology"
          ( topologyCut
              (sameGenerationPredecessor predecessorCut)
              ( topologyFrontier
                  (emptyStructuralVersionVector membershipValue)
                  (controlIndex 0)
              )
              (deriveTopologyOccurrenceDigest "route-projection-predecessor")
          )
      secondTopology =
        checked
          "route-projection successor topology"
          ( topologyCut
              (sameGenerationPredecessor (deriveTopologyCutId firstTopology))
              ( topologyFrontier
                  (emptyStructuralVersionVector membershipValue)
                  (controlIndex 0)
              )
              (deriveTopologyOccurrenceDigest "route-projection-successor")
          )
      memberInputs =
        [ generationMemberInput deltaA storeA local initialStoreRevision,
          generationMemberInput deltaB storeB retired initialStoreRevision
        ]
  firstPlan <- case prepareAlignmentGenerationPlan
    typedSort
    firstTopology
    placementValue
    ( if certifiedRemotePredecessor
        then checked "remote predecessor context graph" (deriveContextGraph [DeltaVertex deltaB] [] [deltaB])
        else contextGraphFixture
    )
    ( if certifiedRemotePredecessor
        then [generationMemberInput deltaB storeB retired initialStoreRevision]
        else memberInputs
    )
    []
    []
    Set.empty of
    Right (AlignmentGenerationReady plan) -> pure plan
    result ->
      assertFailure
        ("route-projection predecessor plan was not ready: " <> show result)
  predecessorGeneration <- case alignmentGenerationPlanGenerations firstPlan of
    [generation] -> pure generation
    generations ->
      assertFailure
        ( "expected one route-projection predecessor generation, got "
            <> show (length generations)
        )
  let withPredecessorDebt =
        retainCauseDebt (cause occurrence) Alignment.emptyState
      promotedPredecessor =
        commit
          Alignment.commitAlignmentPromotion
          "promote route-projection predecessor"
          ( Alignment.prepareAlignmentPromotion
              (alignmentGenerationAnnouncer predecessorGeneration)
              (cause occurrence)
              typedSort
              (deriveTopologyCutId firstTopology)
              placementValue
              firstPlan
              withPredecessorDebt
          )
      withPredecessor
        | certifiedRemotePredecessor =
            let identifier = alignmentGenerationId predecessorGeneration
                ready =
                  classMemberReady
                    firstAlignmentEvidenceSequence
                    identifier
                    storeB
                    (deriveMemberReadyEvidenceDigest "remote-predecessor-ready")
                    initialStoreRevision
                withReadiness =
                  commit
                    Alignment.commitClassMemberReady
                    "retain remote predecessor readiness"
                    (Alignment.prepareClassMemberReady ready promotedPredecessor)
                certificate =
                  historicalCertificate
                    identifier
                    storeB
                    (classMemberReadySetDigest [ready])
                    (deriveBootstrapEvidenceDigest (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest ready)))
                    initialStoreRevision
             in commit
                  Alignment.commitHistoricalCertificate
                  "certify remote predecessor before successor promotion"
                  (Alignment.prepareHistoricalCertificate certificate withReadiness)
        | otherwise = promotedPredecessor
  successorPlan <- case prepareAlignmentGenerationPlan
    typedSort
    secondTopology
    placementValue
    contextGraphFixture
    memberInputs
    [predecessorGeneration]
    []
    (Set.singleton (alignmentGenerationId predecessorGeneration)) of
    Right (AlignmentGenerationReady plan) -> pure plan
    result ->
      assertFailure
        ("route-projection successor plan was not ready: " <> show result)
  generation <- case alignmentGenerationPlanGenerations successorPlan of
    [single] -> pure single
    generations ->
      assertFailure
        ( "expected one route-projection successor generation, got "
            <> show (length generations)
        )
  let withSuccessorDebt =
        retainCauseDebt (cause successorOccurrence) withPredecessor
      promoted =
        commit
          Alignment.commitAlignmentPromotion
          "promote route-projection successor"
          ( Alignment.prepareAlignmentPromotion
              (alignmentGenerationAnnouncer generation)
              (cause successorOccurrence)
              typedSort
              (deriveTopologyCutId secondTopology)
              placementValue
              successorPlan
              withSuccessorDebt
          )
      generationId = alignmentGenerationId generation
      cut = alignmentGenerationCut generation
      acceptance herald =
        alignmentCutAccepted
          generationId
          herald
          (deriveTopologyCutId (alignmentCutTopologyCut cut))
          (alignmentCutPhysicalPlacementRevisionVector cut)
          EmptyHeraldPublicationPrefix
      withLocalAcceptance =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain route-projection local acceptance"
          (Alignment.prepareAlignmentCutAcceptance (acceptance local) promoted)
      withAcceptances =
        commit
          Alignment.commitAlignmentCutAcceptance
          "retain route-projection remote acceptance"
          (Alignment.prepareAlignmentCutAcceptance (acceptance retired) withLocalAcceptance)
      transfer = Alignment.alignmentTransferState withAcceptances
      withLocalPrefix =
        commit
          Transfer.commitLocalRoutePrefixSettlement
          "retain route-projection local prefix"
          ( Transfer.prepareLocalRoutePrefixSettlement
              generationId
              local
              transfer
          )
      incomingMarker =
        checked
          "route-projection historical incoming marker"
          (alignmentRouteCutoverMarker (Plan.alignmentGenerationBirthPlanId generation) generationId retired local)
      withHistoricalMarker
        | certifiedRemotePredecessor = transfer
        | otherwise =
            commit
              Transfer.commitRouteCutoverMarker
              "retain route-projection historical incoming marker"
              (Transfer.prepareRouteCutoverMarker incomingMarker withLocalPrefix)
  pure
    ( Alignment.replaceAlignmentTransferState
        withHistoricalMarker
        withAcceptances,
      generation,
      incomingMarker
    )

retireOracleMember :: HeraldEpoch -> HeraldState -> HeraldState
retireOracleMember retired supplied =
  replaceStartupOracleProjectionState
    (OracleProjection.commitMembershipAdvance prepared)
    state
  where
    state = fixtureHeraldAfterProbePrefix supplied
    index = controlIndex 2
    projection = startupOracleProjectionState state
    view = OracleProjection.oracleView projection
    current = OracleProjection.oracleViewCurrentHeraldMembership view
    successor =
      checked
        "route-projection Oracle membership successor"
        (Membership.retireHeraldMembershipGeneration index (fixtureRetirementResolution index) retired current)
    residentEnds =
      [ OracleProjection.membershipAdvanceProcessEnd
          process
          index
          fixtureHeraldRetirementReason
      | process <- OracleProjection.projectedProcessEpochs projection,
        OracleProjection.oracleViewProcessIsLive process view,
        OracleProjection.oracleViewProcessResidence process view == Just retired
      ]
    prepared =
      checked
        "route-projection Oracle membership advance"
        ( OracleProjection.prepareMembershipAdvance
            index
            successor
            residentEnds
            projection
        )

generationPlan :: IO AlignmentGenerationPlan
generationPlan =
  case prepareAlignmentGenerationPlan
    typedSort
    topology
    placement
    contextGraphFixture
    generationMembers
    []
    []
    Set.empty of
    Right (AlignmentGenerationReady plan) -> pure plan
    result -> assertFailure ("generation plan was not ready: " <> show result)

contextGraphFixture :: ContextGraph
contextGraphFixture =
  checked
    "derive context graph"
    ( deriveContextGraph
        [DeltaVertex deltaA, DeltaVertex deltaB]
        [ edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve,
          edgePayload (DeltaVertex deltaB) (DeltaVertex deltaA) Preserve
        ]
        [deltaA, deltaB]
    )

generationMembers :: [GenerationMemberInput]
generationMembers =
  [ generationMemberInput deltaA storeA heraldA initialStoreRevision,
    generationMemberInput deltaB storeB heraldB initialStoreRevision
  ]

twoClassContextGraph :: ContextGraph
twoClassContextGraph =
  checked
    "derive two-class context graph"
    ( deriveContextGraph
        [ DeltaVertex deltaA,
          DeltaVertex deltaB,
          DeltaVertex deltaC,
          DeltaVertex deltaD
        ]
        [ edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve,
          edgePayload (DeltaVertex deltaB) (DeltaVertex deltaA) Preserve,
          edgePayload (DeltaVertex deltaC) (DeltaVertex deltaD) Preserve,
          edgePayload (DeltaVertex deltaD) (DeltaVertex deltaC) Preserve
        ]
        [deltaA, deltaB, deltaC, deltaD]
    )

twoClassMembers :: [GenerationMemberInput]
twoClassMembers =
  [ generationMemberInput deltaA storeA heraldB initialStoreRevision,
    generationMemberInput deltaB storeB heraldB initialStoreRevision,
    generationMemberInput deltaC storeC heraldC initialStoreRevision,
    generationMemberInput deltaD storeD heraldC initialStoreRevision
  ]

isAnnounce :: AlignmentControl -> Bool
isAnnounce = \case
  AlignmentPlanAnnounced _ -> True
  AlignmentCutAnnounced _ -> True
  _ -> False

isAcceptanceFrom :: HeraldEpoch -> AlignmentControl -> Bool
isAcceptanceFrom expected = \case
  AlignmentPlanAcceptanceAdvertised accepted -> AlignmentProtocol.alignmentPlanAcceptedHerald accepted == expected
  AlignmentCutAcceptanceAdvertised accepted ->
    alignmentCutAcceptedHerald accepted == expected
  _ -> False

isMemberReady :: AlignmentControl -> Bool
isMemberReady = \case
  AlignmentMemberReadyAdvertised _ -> True
  _ -> False

isHistoricalCertificate :: AlignmentControl -> Bool
isHistoricalCertificate = \case
  AlignmentHistoricalCertificateAdvertised _ -> True
  _ -> False

commit ::
  (Show problem) =>
  (prepared -> (state, disposition)) ->
  String ->
  Either problem prepared ->
  state
commit commitPrepared context =
  fst . commitPrepared . checked context

typedSort :: SortOccurrence
typedSort = sortOccurrence sortId sortOccurrenceId

placement :: PhysicalPlacementRevisionVector
placement =
  checked
    "placement vector"
    ( physicalPlacementRevisionVector
        membershipGeneration
        [(heraldB, firstPlacementRevision), (heraldA, firstPlacementRevision)]
    )

advancedPlacement :: PhysicalPlacementRevisionVector
advancedPlacement =
  checked
    "advanced placement vector"
    ( physicalPlacementRevisionVector
        membershipGeneration
        [ (heraldB, nextPlacementRevision firstPlacementRevision),
          (heraldA, firstPlacementRevision)
        ]
    )

multiPlacement :: PhysicalPlacementRevisionVector
multiPlacement =
  checked
    "three-Herald placement vector"
    ( physicalPlacementRevisionVector
        multiMembershipGeneration
        [ (heraldC, firstPlacementRevision),
          (heraldA, firstPlacementRevision),
          (heraldB, firstPlacementRevision)
        ]
    )

alternateMultiPlacement :: PhysicalPlacementRevisionVector
alternateMultiPlacement =
  checked
    "divergent three-Herald placement vector"
    ( physicalPlacementRevisionVector
        multiMembershipGeneration
        [ (heraldC, firstPlacementRevision),
          (heraldA, firstPlacementRevision),
          (heraldB, nextPlacementRevision firstPlacementRevision)
        ]
    )

topology :: TopologyCut
topology =
  checked
    "alignment-coordinator topology cut"
    ( topologyCut
        (sameGenerationPredecessor predecessorCut)
        ( topologyFrontier
            (emptyStructuralVersionVector membershipGeneration)
            (controlIndex 0)
        )
        (deriveTopologyOccurrenceDigest "alignment-coordinator-topology")
    )

topologyId :: TopologyCutId
topologyId = deriveTopologyCutId topology

multiTopology :: TopologyCut
multiTopology =
  checked
    "alignment-coordinator three-Herald topology cut"
    ( topologyCut
        (sameGenerationPredecessor predecessorCut)
        ( topologyFrontier
            (emptyStructuralVersionVector multiMembershipGeneration)
            (controlIndex 0)
        )
        (deriveTopologyOccurrenceDigest "alignment-coordinator-three-herald-topology")
    )

successorTopology :: TopologyCut
successorTopology =
  checked
    "alignment-coordinator successor topology cut"
    ( topologyCut
        (sameGenerationPredecessor topologyId)
        ( topologyFrontier
            (emptyStructuralVersionVector membershipGeneration)
            (controlIndex 0)
        )
        (deriveTopologyOccurrenceDigest "alignment-coordinator-successor-topology")
    )

successorTopologyId :: TopologyCutId
successorTopologyId = deriveTopologyCutId successorTopology

membership :: NonEmpty HeraldEpoch
membership = heraldA :| [heraldB]

multiMembership :: NonEmpty HeraldEpoch
multiMembership = heraldA :| [heraldB, heraldC]

membershipGeneration :: Membership.HeraldMembershipGeneration
membershipGeneration =
  checked
    "alignment-coordinator membership generation"
    ( Membership.genesisHeraldMembershipGeneration
        (checkedSystemId fixtureStep14CheckedGenesis)
        membership
    )

multiMembershipGeneration :: Membership.HeraldMembershipGeneration
multiMembershipGeneration =
  checked
    "alignment-coordinator three-Herald membership generation"
    ( Membership.genesisHeraldMembershipGeneration
        (checkedSystemId fixtureStep14CheckedGenesis)
        multiMembership
    )

occurrence :: StructuralOccurrenceId
occurrence =
  structuralOccurrenceId
    heraldA
    (checked "structural sequence" (mkStructuralSequence 1))

cause :: StructuralOccurrenceId -> StructuralConsequenceCause
cause = structuralOccurrenceCause

successorOccurrence :: StructuralOccurrenceId
successorOccurrence =
  structuralOccurrenceId
    heraldA
    (checked "successor structural sequence" (mkStructuralSequence 2))

mixedProcess :: ProcessEpochId
mixedProcess =
  checked
    "mixed-cause process"
    (mkProcessEpochId (fixtureIdentifierBytes 0xc1))

mixedDecision :: LabelDecisionId
mixedDecision =
  checked
    "mixed-cause label decision"
    (mkLabelDecisionId (fixtureIdentifierBytes 0xc2))

sortId :: SortId
sortId = checked "sort" (mkSortId (fixtureIdentifierBytes 0xb1))

sortOccurrenceId :: SortDefinitionOccurrenceId
sortOccurrenceId =
  checked
    "sort occurrence"
    (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xb2))

deltaA, deltaB, deltaC, deltaD :: DeltaId
deltaA = checked "delta A" (mkDeltaId (fixtureIdentifierBytes 0xb3))
deltaB = checked "delta B" (mkDeltaId (fixtureIdentifierBytes 0xb4))
deltaC = checked "delta C" (mkDeltaId (fixtureIdentifierBytes 0xbd))
deltaD = checked "delta D" (mkDeltaId (fixtureIdentifierBytes 0xbe))

storeA, storeB, storeC, storeD :: StoreIncarnationId
storeA = checked "store A" (mkStoreIncarnationId (fixtureIdentifierBytes 0xb5))
storeB = checked "store B" (mkStoreIncarnationId (fixtureIdentifierBytes 0xb6))
storeC = checked "store C" (mkStoreIncarnationId (fixtureIdentifierBytes 0xbf))
storeD = checked "store D" (mkStoreIncarnationId (fixtureIdentifierBytes 0xc0))

heraldA, heraldB, heraldC :: HeraldEpoch
heraldA = checked "herald A" (mkHeraldEpoch (fixtureIdentifierBytes 0xb7))
heraldB = checked "herald B" (mkHeraldEpoch (fixtureIdentifierBytes 0xb8))
heraldC = checked "herald C" (mkHeraldEpoch (fixtureIdentifierBytes 0xbb))

predecessorCut :: TopologyCutId
predecessorCut =
  checked
    "predecessor topology cut"
    (mkTopologyCutId (fixtureIdentifierBytes 0xb9))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

-- Frozen original payload catalogue and selectors. These use neither the new
-- control keys nor the changed catalogue projection and delta implementation.
referenceRetryControls ::
  HeraldEpoch ->
  HeraldEpoch ->
  Alignment.State ->
  [AlignmentControl]
referenceRetryControls local remote state =
  planAnnounces
    <> planAcceptances
    <> announces
    <> acceptances
    <> readiness
    <> certificates
  where
    explicitPlans = Map.fromList (Alignment.alignmentPlanEntries state)
    remoteInPlan identifier = remote `elem` fmap fst (physicalPlacementRevisionEntries (Plan.alignmentPlanIdPlacement identifier))
    planAccepted = Set.fromList (map fst (Alignment.alignmentPlanAcceptanceEntries state))
    planAnnounces = [AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement plan) | (identifier, plan) <- Map.toAscList explicitPlans, Plan.alignmentPlanAnnouncer plan == Just local, remoteInPlan identifier, Set.notMember (identifier, remote) planAccepted]
    planAcceptances = [AlignmentPlanAcceptanceAdvertised accepted | ((identifier, accepting), accepted) <- Alignment.alignmentPlanAcceptanceEntries state, accepting == local, remoteInPlan identifier]
    explicitBirth generation = Map.member (Plan.alignmentGenerationBirthPlanId generation) explicitPlans
    generations = Alignment.alignmentGenerationEntries state
    referenceAnchorGenerationIds = referenceRetainedAnchorGenerationIds state
    generationFor identifier = Alignment.lookupAlignmentGeneration identifier state
    remoteIsFixedMember generation =
      any
        ((== remote) . fst)
        ( NonEmpty.toList
            ( physicalPlacementRevisionEntries
                ( alignmentCutPhysicalPlacementRevisionVector
                    (alignmentGenerationCut generation)
                )
            )
        )
    remoteNeedsHistoricalEvidence generation =
      remoteIsFixedMember generation
        || Alignment.historicalAlignmentRecipient remote (alignmentGenerationId generation) state
    acceptedKeys =
      Set.fromList
        [ key
        | (key, _) <- Alignment.alignmentCutAcceptanceEntries state
        ]

    announces =
      [ AlignmentCutAnnounced (alignmentGenerationCutAnnounce generation)
      | (generationId, generation) <- generations,
        not (explicitBirth generation),
        referenceExpectedCutAnnouncer referenceAnchorGenerationIds generation == local,
        remoteIsFixedMember generation,
        Set.notMember (generationId, remote) acceptedKeys
      ]

    acceptances =
      [ AlignmentCutAcceptanceAdvertised accepted
      | ((generationId, accepting), accepted) <-
          Alignment.alignmentCutAcceptanceEntries state,
        accepting == local,
        Just generation <- [generationFor generationId],
        not (explicitBirth generation),
        remoteIsFixedMember generation
      ]

    readiness =
      [ AlignmentMemberReadyAdvertised ready
      | ((generationId, store), ready) <-
          Alignment.alignmentMemberReadinessEntries state,
        Just generation <- [generationFor generationId],
        referenceMemberHeraldForStore store generation == Just local,
        remoteNeedsHistoricalEvidence generation
      ]

    certificates =
      [ AlignmentHistoricalCertificateAdvertised certificate
      | ((generationId, store), certificate) <-
          Alignment.alignmentHistoricalCertificateEntries state,
        Just generation <- [generationFor generationId],
        referenceMemberHeraldForStore store generation == Just local,
        remoteNeedsHistoricalEvidence generation
      ]

-- A join snapshot can contain a plan whose certificates are produced later.
-- The new current peer must receive that original source-owned evidence while
-- remaining outside the old plan's acceptance and local readiness obligations.
propPassivePostSealEvidence :: Property
propPassivePostSealEvidence =
  forAll (shuffle [heraldA, heraldB]) $ \order -> ioProperty $ do
    plan <- generationPlan
    (promoted, generation) <- promotedFixture
    let identifier = alignmentGenerationId generation
        readyA = classMemberReady firstAlignmentEvidenceSequence identifier storeA (deriveMemberReadyEvidenceDigest "post-seal-A") initialStoreRevision
        readyB = classMemberReady firstAlignmentEvidenceSequence identifier storeB (deriveMemberReadyEvidenceDigest "post-seal-B") initialStoreRevision
        retainReady ready state = commit Alignment.commitClassMemberReady "retain post-seal readiness" (Alignment.prepareClassMemberReady ready state)
        withReadiness = retainReady readyB (retainReady readyA promoted)
        certificate = historicalCertificate identifier storeA (classMemberReadySetDigest [readyA, readyB]) (deriveBootstrapEvidenceDigest (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest readyA))) initialStoreRevision
        beforeTransfer = commit Alignment.commitHistoricalCertificate "retain post-seal certificate" (Alignment.prepareHistoricalCertificate certificate withReadiness)
        source = Alignment.retainHistoricalAlignmentRecipient heraldC 1 (Set.singleton identifier) beforeTransfer
        newcomer = checked "retain passive join plan" (Alignment.retainHistoricalAlignmentPlan heraldC plan Alignment.emptyState)
        advertisements sender = alignmentRetryControls sender heraldC source
        deliveries = [(sender, control) | sender <- order, control <- advertisements sender]
        receivedReadiness = foldl (\state (_, control) -> case control of AlignmentMemberReadyAdvertised ready -> retainReady ready state; _ -> state) newcomer deliveries
        received = foldl (\state (_, control) -> case control of AlignmentHistoricalCertificateAdvertised value -> commit Alignment.commitHistoricalCertificate "receive post-seal certificate" (Alignment.prepareHistoricalCertificate value state); _ -> state) receivedReadiness deliveries
    assertEqual "untransferred old evidence is not advertised to the newcomer" [] (concatMap (\sender -> alignmentRetryControls sender heraldC beforeTransfer) order)
    assertEqual "both old member stores advertise to the new peer" 2 (length [() | (_, AlignmentMemberReadyAdvertised _) <- deliveries])
    assertEqual "source certificate reaches the new peer" [(identifier, storeA)] (map fst (Alignment.alignmentHistoricalCertificateEntries received))
    assertEqual "passive receiver never accepts an old cut" [] (Alignment.alignmentCutAcceptanceEntries received)
    assertEqual "passive receiver never creates local readiness" [] (Alignment.alignmentLocalMemberReadinessEntries received)
    assertEqual "passive receiver never creates local obligations" [] (Alignment.alignmentObligationEntries received)
    assertEqual "passive receiver retains no live frontier before canonical adoption" [] (Alignment.latestAlignmentGenerationsForSort typedSort received)
    assertBool "old cut announcements and votes are not sent to the newcomer" (all (\(_, control) -> case control of AlignmentMemberReadyAdvertised {} -> True; AlignmentHistoricalCertificateAdvertised {} -> True; _ -> False) deliveries)
    let replayedReadiness = foldl (\state (_, control) -> case control of AlignmentMemberReadyAdvertised ready -> retainReady ready state; _ -> state) received deliveries
        replayed = foldl (\state (_, control) -> case control of AlignmentHistoricalCertificateAdvertised value -> commit Alignment.commitHistoricalCertificate "replay post-seal certificate" (Alignment.prepareHistoricalCertificate value state); _ -> state) replayedReadiness deliveries
    assertEqual "passive immutable evidence replay retains no new semantic work" received replayed
    assertEqual "receiver validates with original certificates and no old authority" (Right ()) (Alignment.validateAlignmentState received)
    pure True

referenceExpectedCutAnnouncer ::
  Set.Set ContextClassGenerationId -> AlignmentGeneration -> HeraldEpoch
referenceExpectedCutAnnouncer referenceAnchorGenerationIds generation
  | Set.member (alignmentGenerationId generation) referenceAnchorGenerationIds =
      minimum
        ( fmap
            fst
            ( NonEmpty.toList
                ( physicalPlacementRevisionEntries
                    ( alignmentCutPhysicalPlacementRevisionVector
                        (alignmentGenerationCut generation)
                    )
                )
            )
        )
  | otherwise = alignmentGenerationAnnouncer generation

-- | Every retained generation was inserted by a checked promotion receipt.
-- The receipt already groups the complete plan at one exact sort, topology,
-- and placement coordinate.  Recover its anchor directly instead of
-- rediscovering sibling groups by repeatedly hashing immutable topology cuts.
referenceRetainedAnchorGenerationIds :: Alignment.State -> Set.Set ContextClassGenerationId
referenceRetainedAnchorGenerationIds state =
  Set.fromList
    [ alignmentGenerationId anchor
    | identifiers <- Map.elems generationIdsByCoordinate,
      let retainedGenerations =
            [ generation
            | identifier <- Set.toAscList identifiers,
              Just generation <- [Alignment.lookupAlignmentGeneration identifier state]
            ],
      Just anchor <- [referenceAnchorGeneration retainedGenerations]
    ]
  where
    generationIdsByCoordinate =
      Map.fromListWith
        Set.union
        [ ( ( Alignment.alignmentPromotionKeySort key,
              Alignment.alignmentPromotionKeyTopologyCut key,
              Alignment.alignmentPromotionKeyPlacementVector key
            ),
            Set.fromList
              (Alignment.alignmentPromotionReceiptGenerationIds receipt)
          )
        | (key, receipt) <- Alignment.alignmentPromotionEntries state
        ]

referenceMemberHeraldForStore ::
  StoreIncarnationId ->
  AlignmentGeneration ->
  Maybe HeraldEpoch
referenceMemberHeraldForStore store generation =
  alignmentMemberHerald
    <$> find
      ((== store) . alignmentMemberStoreIncarnation)
      (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))

referenceAnchorGeneration :: [AlignmentGeneration] -> Maybe AlignmentGeneration
referenceAnchorGeneration generations =
  case sortOn
    ( minimum
        . fmap alignmentMemberDelta
        . NonEmpty.toList
        . alignmentCutExactMembers
        . alignmentGenerationCut
    )
    generations of
    [] -> Nothing
    anchor : _ -> Just anchor

assertControlCatalogueDelta :: HeraldEpoch -> HeraldEpoch -> Alignment.State -> Alignment.State -> Assertion
assertControlCatalogueDelta local remote predecessor successor = do
  let before = referenceRetryControls local remote predecessor
      after = referenceRetryControls local remote successor
  assertEqual "original predecessor catalogue including order" before (alignmentRetryControls local remote predecessor)
  assertEqual "original successor catalogue including order" after (alignmentRetryControls local remote successor)
  assertEqual
    "original stable full-payload list difference"
    (filter (`notElem` before) after)
    (alignmentGenerationControlDelta local remote predecessor successor)
  assertEqual
    "semantic reconnect catalogue contains every relevant immutable root"
    after
    (alignmentReconnectRetryControls local remote successor)

data ControlReferenceEvent
  = ReferenceAcceptA
  | ReferenceAcceptB
  | ReferenceReadyA
  | ReferenceReadyB
  | ReferenceCertificateA
  | ReferenceCertificateB
  deriving stock (Eq, Ord, Show)

controlReferenceSchedule :: Gen [ControlReferenceEvent]
controlReferenceSchedule = do
  evidence <- repeated [ReferenceAcceptA, ReferenceAcceptB, ReferenceReadyA, ReferenceReadyB]
  certificates <- repeated [ReferenceCertificateA, ReferenceCertificateB]
  pure (evidence <> certificates)
  where
    repeated events = do
      count <- chooseInt (2, 4)
      shuffle (concatMap (replicate count) events)

propControlDeltaReference :: Property
propControlDeltaReference =
  forAll controlReferenceSchedule $ \schedule -> ioProperty $ do
    (promoted, generation) <- promotedFixture
    let generationId = alignmentGenerationId generation
        acceptance herald = alignmentCutAccepted generationId herald topologyId placement EmptyHeraldPublicationPrefix
        readyA = classMemberReady firstAlignmentEvidenceSequence generationId storeA (deriveMemberReadyEvidenceDigest "reference-ready-a") initialStoreRevision
        readyB = classMemberReady (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence) generationId storeB (deriveMemberReadyEvidenceDigest "reference-ready-b") initialStoreRevision
        certificate store ready = historicalCertificate generationId store (classMemberReadySetDigest [readyA, readyB]) (deriveBootstrapEvidenceDigest (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest ready))) initialStoreRevision
        next state = \case
          ReferenceAcceptA -> commit Alignment.commitAlignmentCutAcceptance "reference accept A" (Alignment.prepareAlignmentCutAcceptance (acceptance heraldA) state)
          ReferenceAcceptB -> commit Alignment.commitAlignmentCutAcceptance "reference accept B" (Alignment.prepareAlignmentCutAcceptance (acceptance heraldB) state)
          ReferenceReadyA -> commit Alignment.commitClassMemberReady "reference ready A" (Alignment.prepareClassMemberReady readyA state)
          ReferenceReadyB -> commit Alignment.commitClassMemberReady "reference ready B" (Alignment.prepareClassMemberReady readyB state)
          ReferenceCertificateA -> commit Alignment.commitHistoricalCertificate "reference certificate A" (Alignment.prepareHistoricalCertificate (certificate storeA readyA) state)
          ReferenceCertificateB -> commit Alignment.commitHistoricalCertificate "reference certificate B" (Alignment.prepareHistoricalCertificate (certificate storeB readyB) state)
        states = scanl next promoted schedule
        transitions = zip states (drop 1 states)
        peers = [(heraldA, heraldB), (heraldB, heraldA)]
        family :: AlignmentControl -> String
        family = \case
          AlignmentPlanAnnounced _ -> "announce"
          AlignmentPlanAcceptanceAdvertised _ -> "acceptance"
          AlignmentCutAnnounced _ -> "announce"
          AlignmentCutAcceptanceAdvertised _ -> "acceptance"
          AlignmentMemberReadyAdvertised _ -> "readiness"
          AlignmentHistoricalCertificateAdvertised _ -> "certificate"
          _ -> error "frozen catalogue emitted another control family"
        allControls = [control | state <- states, (local, remote) <- peers, control <- referenceRetryControls local remote state]
    forM_ states $ \state -> do
      assertEqual "reference history contains only admitted valid states" (Right ()) (Alignment.validateAlignmentState state)
      forM_ peers $ \(local, remote) -> do
        assertControlCatalogueDelta local remote Alignment.emptyState state
        assertControlCatalogueDelta local remote promoted state
        assertControlCatalogueDelta local remote state state
    -- Reversing admitted snapshots exercises reenabled roots when acceptance
    -- sets differ across valid branches; it is not a rollback API.
    forM_ (transitions <> map (\(before, after) -> (after, before)) transitions <> zip states (reverse states)) $ \(before, after) ->
      forM_ peers $ \(local, remote) -> assertControlCatalogueDelta local remote before after
    assertBool "generated schedule contains exact unchanged replay transitions" (any (uncurry (==)) transitions)
    assertEqual
      "generated histories exercise every one of the four semantic catalogue families"
      (Set.fromList ["announce", "acceptance", "readiness", "certificate"])
      (Set.fromList (map family allControls))
    let final = last states
    assertEqual "semantic catalogue retains local acceptance independent of delivery" True (any (isAcceptanceFrom heraldA) (referenceRetryControls heraldA heraldB final))
    assertEqual "semantic catalogue retains local readiness independent of delivery" True (any isMemberReady (referenceRetryControls heraldA heraldB final))
    assertEqual
      "the unchanged-state fast path still does not evaluate its Herald arguments"
      []
      (alignmentGenerationControlDelta (error "forced local Herald") (error "forced remote Herald") final final)
    (withSuccessor, _) <- successorGenerationFixture
    assertControlCatalogueDelta heraldA heraldB promoted withSuccessor
    assertControlCatalogueDelta heraldB heraldA promoted withSuccessor
    pure True

caseControlDeltaIndependentBranches :: Assertion
caseControlDeltaIndependentBranches = do
  (promoted, generation) <- promotedFixture
  let generationId = alignmentGenerationId generation
      accepted prefix = alignmentCutAccepted generationId heraldA topologyId placement prefix
      emptyAcceptance = accepted EmptyHeraldPublicationPrefix
      laterAcceptance = accepted (HeraldPublicationPrefixThrough firstHeraldPublicationPosition)
      branch payload = commit Alignment.commitAlignmentCutAcceptance "independent checked acceptance branch" (Alignment.prepareAlignmentCutAcceptance payload promoted)
      left = branch emptyAcceptance
      right = branch laterAcceptance
      readiness digest = classMemberReady firstAlignmentEvidenceSequence generationId storeA (deriveMemberReadyEvidenceDigest digest) initialStoreRevision
      firstReady = readiness "reference-branch-ready-first"
      secondReady = readiness "reference-branch-ready-second"
      readyBranch payload = commit Alignment.commitClassMemberReady "independent checked readiness branch" (Alignment.prepareClassMemberReady payload promoted)
      readyLeft = readyBranch firstReady
      readyRight = readyBranch secondReady
  forM_ [left, right, readyLeft, readyRight] $ \state ->
    assertEqual "independent branch is admitted and valid" (Right ()) (Alignment.validateAlignmentState state)
  forM_ [(left, right), (right, left), (readyLeft, readyRight), (readyRight, readyLeft)] $ \(before, after) ->
    assertControlCatalogueDelta heraldA heraldB before after
  assertEqual
    "same generation/acceptor with changed prefix remains newly different"
    [AlignmentCutAcceptanceAdvertised laterAcceptance]
    (alignmentGenerationControlDelta heraldA heraldB left right)
  assertEqual
    "same generation/store with changed readiness remains newly different"
    [AlignmentMemberReadyAdvertised secondReady]
    (alignmentGenerationControlDelta heraldA heraldB readyLeft readyRight)

-- Both owner snapshots are built through checked plans and promotions. This
-- deliberately exercises the owner's receipt dependency independently of the
-- whole-Herald policy that ordinarily admits complete anchored plans together.
caseControlDeltaLateAnchor :: Assertion
caseControlDeltaLateAnchor = do
  let singletonPlan delta store herald =
        case prepareAlignmentGenerationPlan
          typedSort
          topology
          placement
          (checked "singleton anchor context" (deriveContextGraph [DeltaVertex delta] [] [delta]))
          [generationMemberInput delta store herald initialStoreRevision]
          []
          []
          Set.empty of
          Right (AlignmentGenerationReady plan) -> pure plan
          result -> assertFailure ("singleton anchor plan unavailable: " <> show result)
      promote retainedCause plan predecessor =
        let debts =
              normalizeStructuralDebts
                [ structuralConsequenceDebt
                    (structuralDebtKey retainedCause TopologyAlignmentDebt typedSort Nothing)
                    (structuralDebtEvidence Set.empty Set.empty Set.empty)
                ]
            withDebt =
              commit
                Alignment.commitStructuralDebtRetention
                "anchor dependency debt"
                (Alignment.prepareStructuralDebtRetention retainedCause debts predecessor)
         in commit
              Alignment.commitAlignmentPromotion
              "anchor dependency promotion"
              (Alignment.prepareAlignmentPromotion heraldA retainedCause typedSort topologyId placement plan withDebt)
  oldPlan <- singletonPlan deltaB storeB heraldB
  newPlan <- singletonPlan deltaA storeA heraldA
  oldGeneration <- case alignmentGenerationPlanGenerations oldPlan of
    [generation] -> pure generation
    _ -> assertFailure "old anchor plan is not singleton"
  let before = promote (cause occurrence) oldPlan Alignment.emptyState
      after = promote (cause successorOccurrence) newPlan before
      oldId = alignmentGenerationId oldGeneration
      expected = AlignmentCutAnnounced (alignmentGenerationCutAnnounce oldGeneration)
  forM_ [before, after] $ \state ->
    assertEqual "anchor dependency snapshot is an admitted owner" (Right ()) (Alignment.validateAlignmentState state)
  assertEqual "existing generation payload is unchanged" (Alignment.lookupAlignmentGeneration oldId before) (Alignment.lookupAlignmentGeneration oldId after)
  assertBool "old anchor is included through its changed sibling plan coordinate" (Set.member oldId (Alignment.alignmentChangedControlGenerations before after))
  let committedIds = Set.fromList (map alignmentGenerationId (alignmentGenerationPlanGenerations newPlan))
  assertBool
    "scoped promotion keys include the unchanged old anchor dependency"
    (Set.member oldId (Alignment.alignmentControlPlanGenerations committedIds before after))
  assertBool
    "scoped sibling expansion is symmetric across the exact snapshots"
    (Alignment.alignmentControlPlanGenerations committedIds before after == Alignment.alignmentControlPlanGenerations committedIds after before)
  assertBool "old anchor was previously announced only by the fixed least Herald" (expected `notElem` referenceRetryControls heraldB heraldA before)
  assertBool "late lower anchor enables the old generation's member announcer" (expected `elem` referenceRetryControls heraldB heraldA after)
  forM_ [(heraldA, heraldB), (heraldB, heraldA)] $ \(local, remote) -> do
    assertControlCatalogueDelta local remote before after
    assertControlCatalogueDelta local remote after before

-- Frozen pre-index definition; the test does not consult the new index or its
-- internal reconstruction helper to derive expected coverage.
referenceLiveDebtEntries :: Alignment.State -> [StructuralConsequenceDebt]
referenceLiveDebtEntries state =
  filter
    ((`Set.notMember` covered) . structuralConsequenceDebtKey)
    (structuralDebtSetEntries (Alignment.alignmentStructuralDebts state))
  where
    covered =
      Set.unions
        [ Alignment.alignmentPromotionReceiptDebtKeys receipt
        | (key, receipt) <- Alignment.alignmentPromotionEntries state,
          not
            ( Alignment.alignmentPlanIsInvalidated
                (Alignment.alignmentPromotionKeySort key)
                (Alignment.alignmentPromotionKeyTopologyCut key)
                (Alignment.alignmentPromotionKeyPlacementVector key)
                state
            )
        ]

propLiveDebtIndex :: Property
propLiveDebtIndex = forAll schedule $ \(repetitions, invalidationOrder) -> ioProperty $ do
  firstPlan <- generationPlan
  -- The planner's available-certificate ID set describes the prerequisite;
  -- the owner must also retain that exact predecessor's checked certificate
  -- before the successor generation is promoted below.
  secondPlan <- case prepareAlignmentGenerationPlan
    typedSort
    successorTopology
    placement
    contextGraphFixture
    generationMembers
    (alignmentGenerationPlanGenerations firstPlan)
    []
    (Set.fromList (map alignmentGenerationId (alignmentGenerationPlanGenerations firstPlan))) of
    Right (AlignmentGenerationReady plan) -> pure plan
    result -> assertFailure ("live debt successor plan unavailable: " <> show result)
  let retainedCause = cause occurrence
      otherCause = cause successorOccurrence
      debt value identities =
        normalizeStructuralDebts
          [ structuralConsequenceDebt
              (structuralDebtKey value TopologyAlignmentDebt typedSort Nothing)
              (structuralDebtEvidence (Set.fromList (map (StructuralVertexIdentity . DeltaVertex) identities)) Set.empty Set.empty)
          ]
      retain value identities state =
        commit
          Alignment.commitStructuralDebtRetention
          "index debt retention"
          (Alignment.prepareStructuralDebtRetention value (debt value identities) state)
      promote selectedTopology selectedPlacement plan state =
        commit
          Alignment.commitAlignmentPromotion
          "index promotion"
          (Alignment.prepareAlignmentPromotion heraldA retainedCause typedSort (deriveTopologyCutId selectedTopology) selectedPlacement plan state)
      generationOf plan = case alignmentGenerationPlanGenerations plan of
        [generation] -> generation
        _ -> error "live debt fixture requires one class"
      invalidation generation = Alignment.AlignmentPlanDestinationStoreLost (alignmentGenerationId generation) (destinationStore deltaA storeA)
      invalidate index state =
        commit
          Alignment.commitAlignmentPlanInvalidation
          "index invalidation"
          (Alignment.prepareAlignmentPlanInvalidation (invalidation ([generationOf firstPlan, generationOf secondPlan] !! index)) state)
      check state = do
        assertEqual "live debt query equals the frozen complete-history definition" (referenceLiveDebtEntries state) (Alignment.liveAlignmentStructuralDebtEntries state)
        assertEqual "complete owner including derived debt index remains valid" (Right ()) (Alignment.validateAlignmentState state)
        let retainedKeys = Alignment.alignmentCauseCoverageKeys state
            selectedTopologies = Set.toAscList (Set.fromList (topologyId : successorTopologyId : missingTopology : map Alignment.alignmentPromotionKeyTopologyCut retainedKeys))
            selectedPlacements = Set.toAscList (Set.fromList (placement : map Alignment.alignmentPromotionKeyPlacementVector retainedKeys))
            missingTopology = checked "absent invalidation query topology" (mkTopologyCutId (fixtureIdentifierBytes 0xfe))
        forM_ [(selectedTopology, selectedPlacement) | selectedTopology <- selectedTopologies, selectedPlacement <- selectedPlacements] $ \(selectedTopology, selectedPlacement) -> do
          let reference =
                any
                  ( \(_, receipt) ->
                      let coordinate = Alignment.alignmentPlanInvalidationReceiptCoordinate receipt
                       in Alignment.alignmentPlanCoordinateSort coordinate == typedSort
                            && Alignment.alignmentPlanCoordinateTopologyCut coordinate == selectedTopology
                            && Alignment.alignmentPlanCoordinatePlacementVector coordinate == selectedPlacement
                  )
                  (Alignment.alignmentPlanInvalidationEntries state)
          assertEqual "exact invalidation membership equals the old receipt scan across live, lost and replacement coordinates" reference (Alignment.alignmentPlanIsInvalidated typedSort selectedTopology selectedPlacement state)
      initial = retain otherCause [deltaB] (retain retainedCause [deltaA] Alignment.emptyState)
      retainedStates = take repetitions (iterate (retain retainedCause [deltaA]) initial)
      firstPromoted = promote topology placement firstPlan (last retainedStates)
      firstGenerationId = alignmentGenerationId (generationOf firstPlan)
      firstLocalReadiness =
        checked
          "index predecessor local readiness"
          ( Alignment.prepareLocalClassMemberReady
              firstGenerationId
              storeA
              (deriveMemberReadyEvidenceDigest "index-predecessor-ready-a")
              initialStoreRevision
              firstPromoted
          )
      firstLocalReady = Alignment.preparedLocalClassMemberReady firstLocalReadiness
      firstWithLocalReady = fst (Alignment.commitLocalClassMemberReady firstLocalReadiness)
      firstRemoteReady =
        classMemberReady
          firstAlignmentEvidenceSequence
          firstGenerationId
          storeB
          (deriveMemberReadyEvidenceDigest "index-predecessor-ready-b")
          initialStoreRevision
      firstWithReadiness =
        commit
          Alignment.commitClassMemberReady
          "index predecessor remote readiness"
          (Alignment.prepareClassMemberReady firstRemoteReady firstWithLocalReady)
      firstCertificate =
        historicalCertificate
          firstGenerationId
          storeA
          (classMemberReadySetDigest [firstLocalReady, firstRemoteReady])
          ( deriveBootstrapEvidenceDigest
              (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest firstLocalReady))
          )
          initialStoreRevision
      firstCertified =
        commit
          Alignment.commitHistoricalCertificate
          "index predecessor certificate"
          (Alignment.prepareHistoricalCertificate firstCertificate firstWithReadiness)
      refreshed = retain retainedCause [deltaC] firstCertified
      twiceCovered = promote successorTopology placement secondPlan refreshed
      losses = concatMap (replicate repetitions) invalidationOrder
      lossStates = scanl (flip invalidate) twiceCovered losses
      invalidated = last lossStates
      retainedKey = structuralDebtKey retainedCause TopologyAlignmentDebt typedSort Nothing
      isDue state = retainedKey `elem` map structuralConsequenceDebtKey (Alignment.liveAlignmentStructuralDebtEntries state)
  forM_
    (Alignment.emptyState : retainedStates <> [firstPromoted, firstWithLocalReady, firstWithReadiness, firstCertified, refreshed] <> lossStates)
    check
  assertBool "unpromoted retained debt is active" (isDue initial)
  assertBool "promotion and later merged evidence do not reactivate covered debt" (not (isDue refreshed))
  assertBool "invalidating only one of two covering plans leaves debt retired" (not (isDue (lossStates !! 1)))
  assertBool "invalidating the final covering plan reactivates its original debt" (isDue invalidated)
  forM_ [heraldA, heraldB] $ \local -> do
    assertEqual "reactivated debt does not reactivate invalidated member production" [] (map (alignmentGenerationId . fst) (Alignment.pendingAlignmentMemberProgress local invalidated))
    assertEqual "reactivated debt does not reactivate invalidated cutover production" [] (map fst (Alignment.pendingAlignmentRouteCutoverEntries local invalidated))
  assertEqual
    "invalidation preserves all original and merged evidence"
    (Alignment.alignmentStructuralDebts twiceCovered)
    (Alignment.alignmentStructuralDebts invalidated)
  let invalidatedReplay = promote topology placement firstPlan invalidated
  check invalidatedReplay
  assertEqual "replaying an invalidated receipt does not retire recovering debt" invalidated invalidatedReplay
  assertBool "recovering debt remains live after old receipt replay" (isDue invalidatedReplay)
  let recoveryPlacement =
        checked
          "live debt recovery placement"
          ( physicalPlacementRevisionVector
              membershipGeneration
              [(heraldA, nextPlacementRevision firstPlacementRevision), (heraldB, firstPlacementRevision)]
          )
      replacementMembers = [generationMemberInput deltaA storeC heraldA initialStoreRevision, generationMemberInput deltaB storeB heraldB initialStoreRevision]
  replacement <- readyPlanAt topology recoveryPlacement replacementMembers
  let recovered = promote topology recoveryPlacement replacement invalidated
      recoveredReplay = promote topology recoveryPlacement replacement recovered
      recoveredRefreshed = retain retainedCause [deltaB] recoveredReplay
  forM_ [recovered, recoveredReplay, recoveredRefreshed] check
  assertBool "recovery retires the original debt again" (not (isDue recoveredRefreshed))
  assertEqual
    "replacement promotion creates fresh member work while retiring the recovering debt"
    (map alignmentGenerationId (alignmentGenerationPlanGenerations replacement))
    (map (alignmentGenerationId . fst) (Alignment.pendingAlignmentMemberProgress heraldA recovered))
  assertEqual "exact promotion replay preserves the complete index" recovered recoveredReplay
  let assertReadinessChange context changed transition predecessor = do
        let successor = transition (Alignment.clearPreparationReadinessChanges predecessor)
            expected = if changed then Set.singleton typedSort else Set.empty
            (observed, consumed) = Alignment.takePreparationReadinessChanges successor
        assertEqual (context <> ": exact preparation readiness notification") expected observed
        assertEqual (context <> ": drain changes only the journal") (Alignment.clearPreparationReadinessChanges successor) consumed
        assertEqual (context <> ": second drain is empty") Set.empty (fst (Alignment.takePreparationReadinessChanges consumed))
        assertEqual (context <> ": readiness drain preserves predecessor-closure notifications") (fst (Alignment.takeInputClosureChanges successor)) (fst (Alignment.takeInputClosureChanges consumed))
        assertEqual (context <> ": predecessor-closure drain preserves readiness notifications") observed (fst (Alignment.takePreparationReadinessChanges (Alignment.clearInputClosureChanges successor)))
      retainRemoteReady state = commit Alignment.commitClassMemberReady "journal remote readiness" (Alignment.prepareClassMemberReady firstRemoteReady state)
      retainLocalReady state =
        fst
          ( Alignment.commitLocalClassMemberReady
              ( checked
                  "journal local readiness"
                  ( Alignment.prepareLocalClassMemberReady
                      firstGenerationId
                      storeA
                      (deriveMemberReadyEvidenceDigest "index-predecessor-ready-a")
                      initialStoreRevision
                      state
                  )
              )
          )
      retainCertificate state = commit Alignment.commitHistoricalCertificate "journal certificate" (Alignment.prepareHistoricalCertificate firstCertificate state)
  assertEqual "several live debts and promotions coalesce by sort" (Set.singleton typedSort) (fst (Alignment.takePreparationReadinessChanges recovered))
  assertReadinessChange "first live debt" True (retain retainedCause [deltaA]) Alignment.emptyState
  forM_ retainedStates $ \state -> do
    assertReadinessChange "repeated debt" False (retain retainedCause [deltaA]) state
    assertReadinessChange "extra evidence on an already-live debt" False (retain retainedCause [deltaC]) state
  assertReadinessChange "first promotion" True (promote topology placement firstPlan) initial
  assertReadinessChange "exact promotion replay" False (promote topology placement firstPlan) firstPromoted
  assertReadinessChange "remote readiness is delivery evidence" False retainRemoteReady firstPromoted
  assertReadinessChange "first owner-local readiness" True retainLocalReady firstPromoted
  assertReadinessChange "owner-local readiness replay" False retainLocalReady firstWithLocalReady
  assertReadinessChange "certificate delivery does not change local readiness" False retainCertificate firstWithReadiness
  assertReadinessChange "merged evidence on a covered debt" False (retain retainedCause [deltaC]) firstCertified
  assertReadinessChange "successor promotion" True (promote successorTopology placement secondPlan) refreshed
  forM_ [0, 1] $ \index -> do
    assertReadinessChange "first coordinate invalidation" True (invalidate index) twiceCovered
    assertReadinessChange "coordinate invalidation replay" False (invalidate index) (invalidate index twiceCovered)
  assertReadinessChange "invalidated promotion replay" False (promote topology placement firstPlan) invalidated
  assertReadinessChange "recovery promotion" True (promote topology recoveryPlacement replacement) invalidated
  assertReadinessChange "recovery replay" False (promote topology recoveryPlacement replacement) recovered
  pure True
  where
    schedule = (,) <$> chooseInt (1, 4) <*> shuffle [0, 1]
    readyPlanAt selectedTopology selectedPlacement members =
      case prepareAlignmentGenerationPlan typedSort selectedTopology selectedPlacement contextGraphFixture members [] [] Set.empty of
        Right (AlignmentGenerationReady plan) -> pure plan
        result -> assertFailure ("live debt fixture plan unavailable: " <> show result)

casePreparationHistoricalReadinessChanges :: Assertion
casePreparationHistoricalReadinessChanges = do
  plan <- generationPlan
  let key = Alignment.alignmentPromotionKey (cause occurrence) typedSort topologyId placement
      identifiers = Set.toAscList (Set.fromList (map alignmentGenerationId (alignmentGenerationPlanGenerations plan)))
      importPlan state = checked "preparation passive plan" (Alignment.retainHistoricalAlignmentPlan heraldC plan state)
      cover state = checked "preparation historical coverage" (Alignment.retainHistoricalAlignmentCoverage heraldC key plan state)
      adopt entries state = checked "preparation historical frontier" (Alignment.adoptHistoricalAlignmentFrontier heraldC entries state)
      observe context expected state = do
        let (changes, consumed) = Alignment.takePreparationReadinessChanges state
        assertEqual context expected changes
        assertEqual (context <> ": draining twice") Set.empty (fst (Alignment.takePreparationReadinessChanges consumed))
        pure consumed
      affected = Set.singleton typedSort
  imported <- observe "retaining passive generation payload does not select readiness" Set.empty (importPlan Alignment.emptyState)
  covered <- observe "new coverage wakes its sort" affected (cover imported)
  _ <- observe "exact coverage replay is silent" Set.empty (cover covered)
  _ <- observe "debt first retained after completed historical coverage stays retired" Set.empty (retainCauseDebt (cause occurrence) covered)
  let withDebt = Alignment.clearPreparationReadinessChanges (retainCauseDebt (cause occurrence) imported)
  _ <- observe "coverage removes an already retained live debt" affected (cover withDebt)
  adopted <- observe "first nonempty frontier wakes its sort" affected (adopt [(typedSort, identifiers)] imported)
  _ <- observe "exact frontier replay is silent" Set.empty (adopt [(typedSort, identifiers)] adopted)
  emptyFrontier <- observe "explicit empty frontier is still a new semantic fact" affected (adopt [(typedSort, [])] Alignment.emptyState)
  _ <- observe "empty frontier replay is silent" Set.empty (adopt [(typedSort, [])] emptyFrontier)
  pure ()

-- Compare the actual public coordinator's effects, not a second implementation
-- of its change-key accumulation. Pending replay uses the same private evidence
-- admission leaves as current ingress and forces their composed scope to cross
-- the fixed point before this comparison occurs.
propScopedGenerationFanout :: Property
propScopedGenerationFanout =
  forAll ((,) <$> chooseInt (1, 3) <*> shuffle [0 :: Int, 1, 2]) $ \(repetitions, order) -> ioProperty $ do
    let HeraldMember _ local = fixtureLocalMember
        HeraldMember _ remote = fixtureRemoteMember
    (withReady, generation) <- readyAlignmentFixture local remote
    let generationId = alignmentGenerationId generation
        cut = alignmentGenerationCut generation
        accepted herald =
          alignmentCutAccepted
            generationId
            herald
            (deriveTopologyCutId (alignmentCutTopologyCut cut))
            (alignmentCutPhysicalPlacementRevisionVector cut)
            EmptyHeraldPublicationPrefix
        initial =
          commit
            Alignment.commitAlignmentCutAcceptance
            "scoped local acceptance"
            (Alignment.prepareAlignmentCutAcceptance (accepted local) withReady)
        remoteReady =
          classMemberReady
            (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence)
            generationId
            storeB
            (deriveMemberReadyEvidenceDigest "scoped-remote-ready")
            initialStoreRevision
        localReady =
          checkedMaybe
            "scoped local readiness"
            (Alignment.lookupAlignmentMemberReadiness generationId storeA initial)
        certificate =
          historicalCertificate
            generationId
            storeB
            (classMemberReadySetDigest [localReady, remoteReady])
            ( deriveBootstrapEvidenceDigest
                (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest remoteReady))
            )
            initialStoreRevision
        controls =
          Map.fromList
            [ (0, AlignmentCutAcceptanceAdvertised (accepted remote)),
              (1, AlignmentMemberReadyAdvertised remoteReady),
              (2, AlignmentHistoricalCertificateAdvertised certificate)
            ]
        schedule = concat (replicate repetitions [controls Map.! key | key <- order])
    (connected, binding) <- connectedWithAlignment initial
    ingressSuccessor <-
      foldM
        ( \predecessor control -> do
            handled <- case applyAlignmentGenerationControlWithDisposition binding control predecessor of
              Left problem -> assertFailure ("scoped ingress: " <> show problem)
              Right Nothing -> assertFailure "scoped ingress was not handled"
              Right (Just result) -> pure result
            let (successor, effects, _) = handled
            assertScopedGenerationEffects "current ingress" local remote binding predecessor successor effects []
            pure successor
        )
        connected
        (schedule <> schedule)
    assertEqual
      "scoped ingress releases all pending dependencies"
      []
      (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState ingressSuccessor))
    assertEqual
      "scoped ingress retains the dependency-blocked certificate"
      (Just certificate)
      (Alignment.lookupAlignmentHistoricalCertificate generationId storeB (startupAlignmentState ingressSuccessor))
    let pending = foldl (flip (retainPendingControl remote)) initial schedule
        pendingPredecessor = replaceStartupAlignmentState pending connected
    (pendingSuccessor, pendingEffects) <- case advanceAlignmentGenerations pendingPredecessor of
      Left problem -> assertFailure ("scoped pending drain: " <> show problem)
      Right result -> pure result
    assertScopedGenerationEffects
      "pending dependency release"
      local
      remote
      binding
      pendingPredecessor
      pendingSuccessor
      pendingEffects
      []
    assertEqual
      "retaining remote evidence requires no reciprocal semantic advertisement"
      []
      (effectBatchMembers pendingEffects)
    assertEqual
      "scoped fixed point removes all admitted pending entries"
      []
      (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState pendingSuccessor))
    assertEqual
      "scoped fixed point retains the released certificate"
      (Just certificate)
      (Alignment.lookupAlignmentHistoricalCertificate generationId storeB (startupAlignmentState pendingSuccessor))
    (replayed, replayEffects) <- case advanceAlignmentGenerations pendingSuccessor of
      Left problem -> assertFailure ("scoped pending replay: " <> show problem)
      Right result -> pure result
    assertScopedGenerationEffects "quiescent scoped replay" local remote binding pendingSuccessor replayed replayEffects []
    pure True

assertScopedGenerationEffects ::
  String ->
  HeraldEpoch ->
  HeraldEpoch ->
  PeerBinding ->
  HeraldState ->
  HeraldState ->
  EffectBatch ->
  [AlignmentControl] ->
  Assertion
assertScopedGenerationEffects context local remote binding before after effects responses = do
  let predecessor = startupAlignmentState before
      successor = startupAlignmentState after
      expected = nub (responses <> alignmentGenerationControlDelta local remote predecessor successor)
  assertControlCatalogueDelta local remote predecessor successor
  assertEqual
    (context <> ": actual scoped fanout equals the generic catalogue delta")
    [ case control of
        AlignmentPlanAcceptanceAdvertised {} -> QueuePeerAlignmentEvidence remote control
        AlignmentCutAcceptanceAdvertised {} -> QueuePeerAlignmentEvidence remote control
        AlignmentMemberReadyAdvertised {} -> QueuePeerAlignmentEvidence remote control
        _ -> SendPeerControl binding (PeerAlignmentControl control)
    | control <- expected
    ]
    (effectBatchMembers effects)

caseSnapshotWithdrawnSources :: Assertion
caseSnapshotWithdrawnSources = do
  plan <- readyAt topology generationMembers
  replacement <- readyAt successorTopology [generationMemberInput deltaA storeA heraldA initialStoreRevision, generationMemberInput deltaB storeC heraldB initialStoreRevision]
  let imported = checked "import pre-join placement plan" (Alignment.retainHistoricalAlignmentPlan heraldC plan Alignment.emptyState)
      identifiers selected = Set.toAscList (Set.fromList (map alignmentGenerationId (alignmentGenerationPlanGenerations selected)))
      adopted = checked "adopt frozen placement frontier" (Alignment.adoptHistoricalAlignmentFrontier heraldC [(typedSort, identifiers plan)] imported)
      originalRevision = nextPlacementRevision firstPlacementRevision
      laterRevision = nextPlacementRevision originalRevision
      expected = Set.singleton (Alignment.unavailableAlignmentSource heraldB deltaB storeB)
      observed local snapshots = alignmentSnapshotWithdrawnSources local (`Map.lookup` snapshots)
      at revision routes = Map.singleton heraldB (revision, routes)
      exact = route deltaB storeB
      replaced = route deltaB storeC
      otherDelta = route deltaC storeB
      missing = at originalRevision []
  assertEqual "an absent live snapshot cannot prove withdrawal" Set.empty (observed heraldC Map.empty adopted)
  assertEqual "an older snapshot cannot disprove the frozen coordinate" Set.empty (observed heraldC (at firstPlacementRevision []) adopted)
  assertEqual "the exact frozen route is still live" Set.empty (observed heraldC (at originalRevision [exact]) adopted)
  assertEqual "the first current snapshot already proves an exact missing incarnation" expected (observed heraldC missing adopted)
  assertEqual "a later full snapshot may replace the same Delta's incarnation" expected (observed heraldC (at laterRevision [replaced]) adopted)
  assertEqual "a different Delta cannot impersonate the original member even with equal incarnation bytes" expected (observed heraldC (at originalRevision [otherDelta]) adopted)
  forM_ [[exact, otherDelta], [otherDelta, exact]] $ \routes ->
    assertEqual "route presentation order cannot create a withdrawal" Set.empty (observed heraldC (at laterRevision routes) adopted)
  assertEqual
    "remote snapshot observations never infer loss of a local member"
    Set.empty
    (observed heraldA (Map.insert heraldA (laterRevision, []) (at originalRevision [exact])) adopted)
  assertEqual "live snapshot arrival before passive adoption creates no executable old plan loss" Set.empty (observed heraldC missing imported)
  assertEqual "adoption later rechecks that same retained first snapshot" expected (observed heraldC missing adopted)
  let bothPlans = checked "retain a later historical replacement plan" (Alignment.retainHistoricalAlignmentPlan heraldC replacement imported)
      current = checked "adopt only the later complete frontier" (Alignment.adoptHistoricalAlignmentFrontier heraldC [(typedSort, identifiers replacement)] bothPlans)
  assertBool "the superseded original generation remains in historical storage" (all (\identifier -> Alignment.lookupAlignmentGeneration identifier current /= Nothing) (identifiers plan))
  assertEqual "a superseded non-latest incarnation is not newly declared unavailable" Set.empty (observed heraldC (at laterRevision [replaced]) current)
  assertEqual "exact live-snapshot observations preserve checked state" (Right ()) (Alignment.validateAlignmentState current)
  where
    readyAt selectedTopology members =
      case prepareAlignmentGenerationPlan typedSort selectedTopology advancedPlacement contextGraphFixture members [] [] Set.empty of
        Right (AlignmentGenerationReady plan) -> pure plan
        other -> assertFailure ("snapshot withdrawal plan: " <> show other)
    route delta incarnation =
      PlacementProtocol.applicationDeltaRoute
        delta
        sortId
        sortOccurrenceId
        (checked "snapshot route controller" (mkGlobalObjectId (fixtureIdentifierBytes 0xe1)))
        (checked "snapshot route process" (mkProcessEpochId (fixtureIdentifierBytes 0xe2)))
        incarnation
        (controlIndex 0)
