{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module PeerInputProperties
  ( tests,
    assertMarkerWorkReference,
    fixtureFuturePublicationItem,
    fixturePublicationItemForTopologyCut,
    fixtureProtocolViolationItems,
    fixtureSuccessorGenerationOrdinaryItem,
    fixtureSuccessorGenerationStructuralItem,
  )
where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (ApplicationSortDescriptor),
    ApplicationSortKind (ControlledSort, RegularSort),
    ApplicationValueSchema (BoolSchema, LabelSchema, RecordSchema, UniqueIdSchema),
  )
import Eclips.Application.Types.SortDescriptor qualified as SortSyntax
import Eclips.Domain.Alignment
  ( ContextClassGenerationId,
    HeraldPublicationPrefix (EmptyHeraldPublicationPrefix),
    deriveBootstrapEvidenceDigest,
    deriveMemberReadyEvidenceDigest,
    firstPlacementRevision,
    initialStoreRevision,
    memberReadyEvidenceDigestBytes,
    physicalPlacementRevisionVector,
  )
import Eclips.Domain.Context (deriveContextGraph)
import Eclips.Domain.Disappearance
  ( deriveRegularSortOccurrenceClaim,
    regularSortDefinitionDisappearanceSubject,
  )
import Eclips.Domain.Disappearance qualified as DomainDisappearance
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve),
    VertexId (DeltaVertex, NeutralVertex),
    edgePayload,
    edgeStrengthSymbol,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    BootstrapManifestId,
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    NablaId,
    NablaSequencing (..),
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    TopologyCutId,
    controlIndex,
    controlIndexWord64,
    firstStructuralSequence,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    globalUniqueIdBytes,
    globalUniqueIdFromGlobalObjectId,
    labelAuthorityEpoch,
    mkDeltaId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkHeraldId,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkStoreIncarnationId,
    mkStructuralSequence,
    mkTopologyCutId,
    nablaIdFromGlobalObjectId,
    nablaSequence,
    nextStructuralSequence,
    publicationId,
    publicationSourceHeraldEpoch,
    sortIdBytes,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Label
  ( ReleasedLabelStateView (..),
    mkPreparedLabelFacts,
    preparedAuthorityDisposition,
    releasedDeleted,
    releasedLabelStateView,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    genesisHeraldMembershipGeneration,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    mkCheckedPublication,
  )
import Eclips.Domain.Route (ReplicaStrength (Normal, Weak))
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, EdgeCarrier, NablaCarrier, NeutralVertexCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (DeltaRole, EdgeRole, NablaRole, NeutralVertexRole, ProcessEpochRole),
    predefinedCatalogueDescriptor,
    predefinedCatalogueSortId,
    profileCatalogueDigest,
    profileEntryFor,
    profilePredefinedCatalogue,
    sortDefinitionValue,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Domain.Startup
  ( AppliedRootRole (ReaderRoot, WriterRoot),
    ConfiguredProcessBootstrap,
    HeraldMember (..),
    PredefinedSortRole (SortDefinitionRole),
    configuredProcessBootstrapProcessEpochId,
    configuredProcessBootstrapProcessId,
    configuredProcessBootstrapResidence,
    configuredProcessBootstrapRoots,
    configuredRootBootstrapCatalogueRole,
    configuredRootBootstrapReaderDelta,
    configuredRootBootstrapWriterNabla,
    configuredRootBootstrapWriterSequencing,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    predefinedOccurrenceFor,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    structuralPrefixThrough,
    structuralVersionVectorComponent,
    structuralVersionVectorEntries,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    labelReleaseCause,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Topology
  ( deriveTopologyCutId,
    deriveTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyFrontier,
  )
import Eclips.Domain.Value
  ( LabelOwner (ProcessLabel, VoidLabel),
    Value,
    boolValue,
    bytesValue,
    canonicalValueByteString,
    canonicalValueBytes,
    decodeCanonicalValue,
    enumValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
    textValue,
  )
import Eclips.Herald.Alignment.CutQueries qualified as CutQueries
import Eclips.Herald.Alignment.Generation
  ( AlignmentGenerationPreparation (AlignmentGenerationReady),
    alignmentGenerationCut,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    generationMemberInput,
    prepareAlignmentGenerationPlan,
  )
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol
  ( alignmentCutAccepted,
    alignmentRouteCutoverMarker,
    classMemberReady,
    classMemberReadyPredecessorAndBasePrefixDigest,
    classMemberReadySetDigest,
    firstAlignmentEvidenceSequence,
    historicalCertificate,
    nextAlignmentEvidenceSequence,
  )
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as AlignmentTransfer
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortDescriptor,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Bootstrap qualified as Bootstrap
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Evidence qualified as DisappearanceEvidence
import Eclips.Herald.Disappearance.Protocol qualified as DisappearanceProtocol
import Eclips.Herald.Disappearance.State qualified as DisappearanceOwner
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Discovery.Internal (discoveryStatic)
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    PeerProtocolDisposition (ClosePeerPublicationProtocol),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    DeploymentManifest (..),
    OracleGenesisManifest (..),
    checkHeraldGenesis,
  )
import Eclips.Herald.Genesis.Internal
  ( AppliedProcessBootstrap,
    AppliedRoot,
    CheckedInitialBootstraps,
    PrimordialProcessManifest (..),
    appliedProcessAuthority,
    appliedProcessEpochId,
    appliedProcessId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootControlPrerequisite,
    appliedRootObjectId,
    appliedRootOccurrenceId,
    appliedRootRole,
    appliedRootSortId,
    checkInitialBootstraps,
    checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedLocalHeraldId,
    checkedPredefinedOccurrenceSet,
    checkedPrimordialReplicas,
    checkedSystemId,
    genesisAuthorityEpoch,
    lookupConfiguredProcessBootstrap,
    lookupSystemBootstrapWriter,
    primordialReplicaDescriptor,
    primordialReplicaRole,
    systemBootstrapWriterAuthority,
    systemBootstrapWriterNablaId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol qualified as GraphProtocol
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource
  ( deriveTerminalStructuralPayloadDigest,
    terminalStructuralOccurrence,
  )
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input qualified as HeraldInput
import Eclips.Herald.LabelBarrier.State qualified as LabelBarrier
import Eclips.Herald.OracleClient
  ( OracleClientAction (BindOracleConnection, ConnectAndHelloOracle),
    OracleClientIngress (OracleEntriesReceived, OracleHelloReceived),
    oracleConnectAttemptContact,
    oracleConnectAttemptFromExclusive,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDispatch.Internal (peerLogicalItem, peerPublicationItem)
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (..),
    peerLogicalPayloadDigest,
    peerLogicalPublicationItem,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PeerPublicationProblem (StructuralStampPublicationMismatch),
    PublicationBatch,
    PublicationDestination,
    mkOrdinaryPeerPublication,
    mkPublicationBatch,
    mkStructuralOccurrenceStamp,
    mkStructuralPeerPublication,
    peerPublicationBatch,
    peerPublicationDigest,
    peerPublicationStructuralCanonicalBytes,
    peerPublicationStructuralStamp,
    presentedOrdinaryPeerPublication,
    presentedStructuralPeerPublication,
    publicationBatchCanonicalValue,
    publicationBatchControlPrerequisite,
    publicationBatchDestinations,
    publicationBatchId,
    publicationBatchOccurrenceId,
    publicationBatchSortId,
    publicationBatchSourceProcess,
    publicationBatchSourceStrength,
    publicationBatchSourceTopologyPrerequisite,
    publicationDestination,
    publicationDestinationDelta,
    publicationDestinationStoreIncarnation,
    structuralOccurrenceStampCarrierRole,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
    structuralOccurrenceStampPublicationDigest,
    structuralPublicationCanonicalBytesForSemantics,
    structuralPublicationDigestFor,
  )
import Eclips.Herald.PeerStream
  ( PeerItemDigest,
    ReceiveDisposition (..),
    ReceiveProgress (ReceivedPending),
    SequencedItem,
    StreamDirection,
    StreamSequence,
    emptyStreamPrefix,
    firstStreamSequence,
    mkPeerDispatchBindingGeneration,
    mkPeerItemDigest,
    mkStreamDirection,
    nextStreamSequence,
    peerItemDigestBytes,
    sequencedItem,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamDirectionSource,
    streamPrefixSequence,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementMessage
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupDiscoveryState,
    replaceStartupGraphState,
    replaceStartupIdGeneratorState,
    replaceStartupLabelBarrierState,
    replaceStartupOracleProjectionState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    replaceStartupWaitState,
    startupAlignmentCutQueryCache,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDisappearanceState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupLabelBarrierState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
    startupWaitState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( StructuralDebtKind (TopologyAlignmentDebt),
    normalizeStructuralDebts,
    sortOccurrence,
    sortOccurrenceDefinition,
    structuralConsequenceDebt,
    structuralDebtEvidence,
    structuralDebtKey,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransferCoordinator
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.PeerPlacement qualified as PeerPlacement
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Herald.Wait.State qualified as Wait
import Eclips.Oracle.Canonical
  ( canonicalizeAppliedOracleEntry,
  )
import Eclips.Oracle.Command
  ( OracleCommand,
    OracleEnvelope,
    oracleEnvelope,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkOracleGenesis,
    deriveRaftConfigurationDigest,
    oracleGenesis,
    raftVoterBinding,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label qualified as Oracle
import Eclips.Oracle.Projection (AppliedOracleEntry)
import Eclips.Oracle.State (OracleState)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Genesis
  ( RaftNativeConfiguration,
    checkRaftGenesis,
    checkedRaftNativeConfiguration,
    raftGenesis,
  )
import Eclips.Raft.Identity
  ( RaftDurationMicros,
    RaftNodeId,
    mkRaftDurationMicros,
    mkRaftNodeId,
  )
import GenesisFixtures
  ( currentPeerHelloReceived,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureDeploymentAt,
    fixtureDeploymentManifest,
    fixtureGeneratorSeed,
    fixtureHeraldRetirementReason,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
    fixtureRetirementResolution,
    fixtureStep14CheckedGenesis,
  )
import MembershipSuccessorProgressProperties (baseForLineageWithPayloads)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "incoming peer publication coordinator"
    [ QC.testProperty "skipped full placement snapshots retain exact Store losses through partial withdrawal and replay" (QC.withNumTests 20 propPlacementWithdrawalLosses),
      testCase "a contiguous exact-current batch advances received and completed" caseContiguous,
      testCase "a later payload prerequisite is independent of source creation and remains held" caseLaterPayloadPrerequisite,
      testCase
        "retired peer definitions and structural references respect the Resolve-qualified successor"
        caseRetiredPeerDefinition,
      testCase
        "retired label-controlled ordinary peer values leave semantic owners unchanged"
        caseRetiredOrdinaryPeerValue,
      testCase "a retained publication from a retired source completes locally without acknowledging it" caseRetiredSourceDependencyRelease,
      testCase "a retained G1 structural wait follows G2 while preserving its immutable admission and payload" caseRepeatedSuccessorWait,
      testCase "the matching control entry releases Store, Graph, and completion exactly once" caseControlledRelease,
      testCase "a reached rejected entry still releases target-local controlled data" caseControlledRejection,
      testCase "control-first and data-first delivery converge and replay idempotently" caseControlledDeliveryConvergence,
      testCase "sort and control dependencies release to the same result in either order" caseDualDependencyReleaseOrder,
      testCase
        "a definition-triggered deferred rejection closes only the original peer binding"
        caseDefinitionTriggeredDeferredRejection,
      testCase "an unknown source topology is retained and an early release is an exact no-op" caseSourceTopologyHeld,
      testCase
        "a final pre-authentication hold can become a causal structural hold"
        casePreAuthenticationToStructuralHold,
      testCase "structural reception requires its system view and admits frozen application readers" caseStructuralDestinationAdmission,
      testCase
        "a label-deleted Delta current is transcript-only without a Store-local tombstone"
        caseTerminalStructuralStoreSuppression,
      testCase
        "dynamic Start grants no hidden genesis writer authority for Nabla or Delta roots"
        caseHiddenBootstrapWriterRejected,
      testCase
        "a label-deleted Edge current retains history without recreating topology"
        caseTerminalStructuralEdgeSuppression,
      testCase "terminal-source repair re-admits exact semantics and replays idempotently" caseTerminalStructuralMaterialization,
      testCase "retired terminal payload replay preserves its receipt through two installed bases" caseTerminalReplayAfterRepeatedRetirement,
      testCase "an active Delta allocates one receiver incarnation and retry does not allocate again" caseActiveDeltaReceiverAllocation,
      QC.testProperty "structural ingress preserves the complete owner invariant across causal delivery orders and retries" (QC.withNumTests 20 propStructuralIngressInvariant),
      testCase "a cross-source structural successor retains an absent causal predecessor and wakes on arrival" caseAbsentStructuralPredecessor,
      testCase "a causal successor waits on the predecessor occurrence rather than its non-structural hold" caseNonStructuralHeldPredecessor,
      testCase "a reverse-ordered cross-direction successor wakes after its structurally held predecessor" caseReverseDirectionStructuralPredecessor,
      testCase "a same-direction causal successor wakes after its structurally held predecessor" caseHeldStructuralPredecessor,
      QC.testProperty "one blocked publication plus completed semantic duplicates retains bounded combined receipt ownership" (QC.withNumTests 20 propSparseReceiptOwnership),
      testCase "exact stream retry and fresh semantic duplicate never reapply Store" caseDeduplication,
      testCase "a gap remains hidden and its closing suffix applies atomically" caseGapSuffix,
      testCase "gap repair offers change only with new frontier or gap evidence" caseGapRepairOfferDeduplication,
      testCase "mixed current and absent destinations retain exact terminal outcomes" caseMixedDestinations,
      testCase "payload digest disagreement is rejected before installation" caseDigestMismatch,
      testCase "malformed future input cannot reserve its sequence" caseMalformedFutureCorrection,
      testCase "peer protocol claims have complete boundary classification" caseProtocolViolationMatrix,
      testCase "an owner-derived stronger receipt makes weaker delivery idempotent" caseOwnerReceiptJoin,
      testCase "a route-cutover item waits for generation and source acceptance before one completion" caseRouteCutoverGenerationHeld,
      testCase
        "a delayed invalid route-cutover is terminally rejected at its source binding"
        caseDelayedRouteCutoverRejection,
      testCase "a retired source terminally settles a route-cutover whose evidence cannot arrive" caseRetiredSourceRouteCutoverSettlement,
      testCase "a route-cutover marker cannot overtake an earlier pending publication" caseRouteCutoverCannotOvertakePublication,
      testCase "a pending marker does not head-of-line block a later publication" casePendingMarkerAllowsLaterPublication,
      testCase "publication completion exposes an earlier pre-Open disappearance marker" casePublicationCompletionExposesDisappearanceMarker,
      testCase "route-cutover completion exposes an earlier pre-Open disappearance marker" caseRouteCutoverCompletionExposesDisappearanceMarker,
      testCase "a stale binding is an exact no-op with no acknowledgement" caseStaleBinding
    ]

-- A private-owner counterpart to the opaque Step-7 wire schedule: loss facts
-- must survive snapshot revision gaps, and a stale advertisement cannot restore
-- a withdrawn Store as an alignment source.
propPlacementWithdrawalLosses :: Word64 -> QC.Property
propPlacementWithdrawalLosses generated = QC.ioProperty $ do
  let genesis = fixtureCheckedGenesis
      manifest = PrimordialProcessManifest [firstLocalBootstrapId, fixtureRemoteBootstrapId]
      firstOrdinal = 2 + generated `mod` 10000
      nonce = Discovery.connectionNonce firstOrdinal
  remoteGenesis <- checked "withdrawal remote genesis" (checkHeraldGenesis (fixtureDeploymentAt fixtureRemoteMember))
  bootstraps <- checked "withdrawal bootstrap projection" (checkInitialBootstraps genesis manifest)
  remoteBootstraps <- checked "withdrawal remote bootstrap projection" (checkInitialBootstraps remoteGenesis manifest)
  let initialize owner initialBootstraps =
        fst
          <$> checked
            "initialize withdrawal Herald"
            (initialHerald (monotonicInstant 0) owner initialBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)
      step observed body predecessor =
        checked
          "composed withdrawal peer transition"
          (verifiedStepHerald (HeraldInput.heraldInput (monotonicInstant observed) body) predecessor)
  initial <- initialize genesis bootstraps
  remote <- initialize remoteGenesis remoteBootstraps
  advertised <- checked "retained remote placement snapshot" (Placement.prepareRetainedLocalSnapshot (startupPlacementState remote))
  let snapshot = Placement.localSnapshotMessage (Placement.preparedLocalSnapshotResult advertised)
      owner = PlacementMessage.placementSnapshotOwner snapshot
      routes = PlacementMessage.placementSnapshotRoutes snapshot
      candidate = Discovery.peerCandidate (checkedLocalHeraldId remoteGenesis) owner nonce
      hello = Discovery.peerHello (checkedSystemId genesis) (checkedLocalHeraldId remoteGenesis) owner nonce Set.empty (controlIndex 0) (checkedCatalogueDigest genesis) (checkedInitialProjectionDigest bootstraps) Nothing
  (connected, helloEffects) <- step 1 (HeraldInput.PeerInput (currentPeerHelloReceived initial candidate Set.empty hello)) initial
  binding <- case [admitted | SetPeerCandidateDisposition observed (Discovery.PeerHelloAccepted _ admitted) <- effectBatchMembers helloEffects, observed == candidate] of
    [admitted] -> pure admitted
    other -> assertFailure ("withdrawal fixture has no unique live peer binding: " <> show other)
  retainedRoute <- case routes of
    first : _ : _ -> pure first
    _ -> assertFailure "withdrawal fixture must advertise multiple independent Store coordinates"
  first <- checked "late first placement sequence" (PlacementMessage.mkPlacementSequence firstOrdinal)
  partial <- checked "skipped partial withdrawal sequence" (PlacementMessage.mkPlacementSequence (firstOrdinal + 2))
  final <- checked "skipped complete withdrawal sequence" (PlacementMessage.mkPlacementSequence (firstOrdinal + 5))
  initialSnapshot <- checked "late complete placement snapshot" (PlacementMessage.placementSnapshot owner first routes)
  partialSnapshot <- checked "partial placement withdrawal" (PlacementMessage.placementSnapshot owner partial [retainedRoute])
  finalSnapshot <- checked "complete placement withdrawal" (PlacementMessage.placementSnapshot owner final [])
  let deliver observed value = step observed (HeraldInput.PeerInput (HeraldInput.PeerControlReceived binding (HeraldInput.PeerPlacementUpdate (PlacementMessage.FullPlacementSnapshot value))))
      unavailable = Alignment.alignmentUnavailableSources . startupAlignmentState
      coordinate route = Alignment.unavailableAlignmentSource owner (PlacementMessage.deltaRouteDelta route) (PlacementMessage.deltaRouteStoreIncarnation route)
      lost = Set.fromList (map coordinate (filter (/= retainedRoute) routes))
      allLost = Set.fromList (map coordinate routes)
      remoteRoutes = Placement.remotePlacementRoutes owner . startupPlacementState
  (placed, _) <- deliver 2 initialSnapshot connected
  assertEqual "late first snapshot installs its actual complete routes" routes (remoteRoutes placed)
  assertEqual "late first snapshot invents no unobserved Store loss" (unavailable connected) (unavailable placed)
  (withdrawn, _) <- deliver 3 partialSnapshot placed
  assertEqual "partial withdrawal retains exactly the still-advertised route" [retainedRoute] (remoteRoutes withdrawn)
  assertEqual "each omitted Store gains its exact durable source exclusion" (unavailable placed <> lost) (unavailable withdrawn)
  assertBool "the retained Store is not incorrectly excluded" (Set.notMember (coordinate retainedRoute) (unavailable withdrawn))
  (duplicated, _) <- deliver 4 partialSnapshot withdrawn
  (stale, _) <- deliver 5 initialSnapshot duplicated
  forM_ [duplicated, stale] $ \observed -> do
    assertEqual "duplicate and stale snapshots preserve the current partial projection" [retainedRoute] (remoteRoutes observed)
    assertEqual "duplicate and stale snapshots preserve every exact loss fact" (unavailable withdrawn) (unavailable observed)
    assertBool "duplicate and stale snapshots add no historical placement revision" (startupPlacementState withdrawn == startupPlacementState observed)
  (empty, _) <- deliver 6 finalSnapshot stale
  assertEqual "the later full withdrawal removes the final remote route" [] (remoteRoutes empty)
  assertEqual "the final withdrawal retains losses for every original Store" (unavailable placed <> allLost) (unavailable empty)
  (replayed, _) <- deliver 7 partialSnapshot empty
  assertEqual "a stale partial snapshot cannot resurrect the final withdrawn Store" [] (remoteRoutes replayed)
  assertEqual "all withdrawn source exclusions survive stale partial replay" (unavailable empty) (unavailable replayed)
  pure True

caseContiguous :: Assertion
caseContiguous = do
  fixture <- peerFixture
  (successor, result) <-
    checked
      "contiguous peer input"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          fixture.item
          fixture.state
      )
  assertEqual
    "stream disposition"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition result)
  acknowledgement <- requireAcknowledgement result
  assertEqual
    "received through one"
    (Just firstStreamSequence)
    (streamPrefixSequence (PeerInput.peerAcknowledgementReceivedPrefix acknowledgement))
  assertEqual
    "completed through one"
    (Just firstStreamSequence)
    (streamPrefixSequence (PeerInput.peerAcknowledgementCompletedPrefix acknowledgement))
  assertEqual
    "one semantic publication was processed"
    [checkedPublicationId fixture.publication]
    (PeerInput.peerInputPublicationIds result)
  record <- requireIncoming fixture successor
  assertEqual "publication is terminally applied" Publication.Applied (Publication.incomingPublicationDisposition record)
  assertEqual
    "first semantic admission captures the current Oracle membership"
    ( OracleProjection.oracleViewCurrentHeraldMembershipId
        (OracleProjection.oracleView fixture.oracle)
    )
    (Publication.incomingPublicationMembershipGenerationId record)
  slot <- requireDestinationSlot fixture successor
  assertEqual
    "the exact current incarnation retained its receipt"
    (Just Normal)
    (lookup (checkedPublicationId fixture.publication) (Store.storeSlotApplicationReceipts slot))
  retained <-
    checked
      "incoming stream records"
      ( PeerStream.incomingRetainedItems
          fixture.direction
          (PeerInput.peerInputPeerStreamState successor)
      )
  assertEqual "the stream item is complete" [] (fmap snd retained)

-- Scheduling metadata is observable to the tests but is not a stream effect.
-- Legacy before/after idempotence assertions discard declarations only here;
-- sparse/fullscan equivalence below retains and checks both watcher directions.
sameMarkerSemantics :: PeerInput.PeerInputState -> PeerInput.PeerInputState -> Bool
sameMarkerSemantics left right = PeerInput.samePeerInputMarkerWorkState (withoutWatches left) (withoutWatches right)
  where
    withoutWatches state = replacePeerInputPeerStreamAndControlled cleared (PeerInput.peerInputControlledState state) state
      where
        cleared = foldl' clearFamily (PeerInput.peerInputPeerStreamState state) [PeerStream.RouteCutoverMarkers, PeerStream.DisappearanceProbeMarkers]
        clearFamily stream family = Set.foldl' (\current direction -> PeerStream.retainIncomingMarkerDependencies family direction Set.empty current) stream (PeerStream.incomingMarkerDirections family stream)

assertMarkerSchedulingValid :: PeerInput.PeerInputState -> Assertion
assertMarkerSchedulingValid state = do
  assertEqual
    "incoming marker registrations and inverse watches remain valid"
    (Right ())
    (PeerStream.validatePeerStreamState (PeerInput.peerInputPeerStreamState state))
  assertBool
    "generation work registrations and inverse watches remain valid"
    (Alignment.generationSchedulingValid (PeerInput.peerInputAlignmentState state))
  assertBool
    "transfer work registrations and inverse watches remain valid"
    (Alignment.transferSchedulingValid (PeerInput.peerInputAlignmentState state))

type MarkerRelease = PeerInput.PeerInputContext -> PeerInput.PeerInputState -> Either PeerInput.PeerInputProblem (PeerInput.PeerInputState, PeerInput.PeerDependencyReleaseResult)

markerReleaseReference :: MarkerRelease -> MarkerRelease -> MarkerRelease
markerReleaseReference selective exhaustive context state =
  case (selective context state, exhaustive context state) of
    (Left actual, Left expected) | actual == expected -> Left actual
    (Right actual@(after, result), Right (reference, referenceResult))
      | result == referenceResult,
        PeerInput.samePeerInputMarkerWorkState after reference,
        PeerStream.validatePeerStreamState (PeerInput.peerInputPeerStreamState after) == Right (),
        PeerStream.validatePeerStreamState (PeerInput.peerInputPeerStreamState reference) == Right () ->
          Right actual
    _ -> error "ordered marker selective/fullscan state, effect, error, or index mismatch"

releasePendingAlignmentRouteCutoversChecked :: MarkerRelease
releasePendingAlignmentRouteCutoversChecked = markerReleaseReference PeerInput.releasePendingAlignmentRouteCutovers PeerInput.releasePendingAlignmentRouteCutoversExhaustive

releasePendingDisappearanceProbeMarkersChecked :: MarkerRelease
releasePendingDisappearanceProbeMarkersChecked = markerReleaseReference PeerInput.releasePendingDisappearanceProbeMarkers PeerInput.releasePendingDisappearanceProbeMarkersExhaustive

assertMarkerWorkReference :: HeraldState -> Assertion
assertMarkerWorkReference state = do
  let (context, owners) = peerInputSliceForHerald state
  forM_ [releasePendingAlignmentRouteCutoversChecked, releasePendingDisappearanceProbeMarkersChecked] $ \release ->
    case release context owners of
      Left _ -> pure () -- exact errors have already been compared
      Right (after, _) -> assertEqual "marker reference owner invariant" (Right ()) (PeerStream.validatePeerStreamState (PeerInput.peerInputPeerStreamState after))

caseRouteCutoverGenerationHeld :: Assertion
caseRouteCutoverGenerationHeld = do
  fixture <- peerFixture
  (promoted, accepted, generation, _, _) <- routeCutoverAlignmentFixture fixture
  retainedGeneration <- case Alignment.lookupAlignmentGeneration generation promoted of
    Just value -> pure value
    Nothing -> assertFailure "route-cutover fixture generation is missing"
  marker <-
    checked
      "future route-cutover marker"
      ( alignmentRouteCutoverMarker
          (fixtureGenerationPlan promoted generation)
          generation
          fixture.remoteEpoch
          fixture.localEpoch
      )
  let payload = PeerLogicalRouteCutover marker
      transferOf = Alignment.alignmentTransferState . PeerInput.peerInputAlignmentState
      installedMarker state = AlignmentTransfer.lookupRouteCutover generation fixture.remoteEpoch fixture.localEpoch (transferOf state)
      projectedRouteComplete state =
        AlignmentTransferCoordinator.routeCutoverSetComplete
          (Set.fromList [fixture.localEpoch, fixture.remoteEpoch])
          generation
          fixture.localEpoch
          (alignmentGenerationCut retainedGeneration)
          (transferOf state)
      item =
        sequencedItem
          fixture.direction
          firstStreamSequence
          (peerLogicalPayloadDigest payload)
          payload
      third = nextStreamSequence (nextStreamSequence firstStreamSequence)
      future =
        sequencedItem
          fixture.direction
          third
          (peerLogicalPayloadDigest payload)
          payload
  (held, result) <-
    checked
      "retain generation-held route cutover"
      ( PeerInput.applyPeerLogicalItem
          fixture.context
          fixture.binding
          item
          fixture.state
      )
  acknowledgement <- requireAcknowledgement result
  assertEqual
    "the marker advances only the received prefix"
    (Just firstStreamSequence, Nothing)
    ( streamPrefixSequence
        (PeerInput.peerAcknowledgementReceivedPrefix acknowledgement),
      streamPrefixSequence
        (PeerInput.peerAcknowledgementCompletedPrefix acknowledgement)
    )
  retained <-
    checked
      "generation-held logical stream"
      ( PeerStream.incomingRetainedItems
          fixture.direction
          (PeerInput.peerInputPeerStreamState held)
      )
  assertEqual
    "the exact marker remains received-pending"
    [(item, PeerStream.IncomingReceivedPending)]
    retained
  (heldWithGap, gapResult) <-
    checked
      "retain a later item behind the held marker"
      ( PeerInput.applyPeerLogicalItem
          fixture.context
          fixture.binding
          future
          held
      )
  gapAcknowledgement <- requireAcknowledgement gapResult
  assertBool
    "the newly exposed gap produces one repair offer"
    (maybe False (const True) (PeerInput.peerAcknowledgementRepairOffer gapAcknowledgement))
  (replayed, releaseResult) <-
    checked
      "early route-cutover dependency release"
      ( releasePendingAlignmentRouteCutoversChecked
          fixture.context
          heldWithGap
      )
  assertBool "early release is state-idempotent" (sameMarkerSemantics replayed heldWithGap)
  assertEqual
    "the absent generation keeps the marker held"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition releaseResult)
  let parkedStream = PeerInput.peerInputPeerStreamState replayed
      routePending = PeerStream.pendingIncomingMarkerDirections PeerStream.RouteCutoverMarkers
  assertEqual "blocked marker parks its direction" Set.empty (routePending parkedStream)
  forM_ [1 :: Int .. 8] $ \_ -> do
    (unchanged, noProgress) <- checked "parked marker no-op" (releasePendingAlignmentRouteCutoversChecked fixture.context replayed)
    assertBool "unrelated redrive does not evaluate or rewrite the parked head" (unchanged == replayed)
    assertEqual "parked marker has no scheduled direction" Set.empty (routePending (PeerInput.peerInputPeerStreamState unchanged))
    assertEqual "parked marker emits no acknowledgements" [] (PeerInput.peerDependencyAcknowledgements noProgress)
  let generationInstalled = replacePeerInputAlignment promoted replayed
  (awaitingAcceptance, acceptanceRelease) <-
    checked
      "route-cutover release before source acceptance"
      ( releasePendingAlignmentRouteCutoversChecked
          fixture.context
          generationInstalled
      )
  assertBool
    "installing the generation alone does not complete the marker"
    (sameMarkerSemantics awaitingAcceptance generationInstalled)
  assertEqual
    "the exact source acceptance remains a dependency"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition acceptanceRelease)
  forM_ [held, heldWithGap, replayed, generationInstalled, awaitingAcceptance] $ \pending -> do
    assertEqual "received-pending markers do not enter the installed marker index" Nothing (installedMarker pending)
    assertBool "received-pending markers cannot satisfy the indexed closure query" (not (projectedRouteComplete pending))
  let sourceAccepted = replacePeerInputAlignment accepted awaitingAcceptance
  (completed, completedRelease) <-
    checked
      "route-cutover release after source acceptance"
      ( releasePendingAlignmentRouteCutoversChecked
          fixture.context
          sourceAccepted
      )
  assertEqual
    "the admitted source acceptance releases the marker exactly once"
    PeerInput.PeerDependencyReleased
    (PeerInput.peerDependencyReleaseDisposition completedRelease)
  assertEqual "composed admission installs exactly the accepted marker" (Just marker) (installedMarker completed)
  assertBool "only the installed marker completes the projected route set" (projectedRouteComplete completed)
  assertBool
    "the indexed marker's source acceptance remains retained"
    (Alignment.lookupAlignmentCutAcceptance generation fixture.remoteEpoch (PeerInput.peerInputAlignmentState completed) /= Nothing)
  completionAcknowledgement <- case PeerInput.peerDependencyAcknowledgements completedRelease of
    [completion] -> pure completion
    acknowledgements ->
      assertFailure
        ( "expected one route-cutover completion acknowledgement, got "
            <> show (length acknowledgements)
        )
  assertEqual
    "completion-only progress inside the same gap does not repeat a full resume"
    Nothing
    (PeerInput.peerAcknowledgementRepairOffer completionAcknowledgement)
  completedRetained <-
    checked
      "completed route-cutover stream"
      ( PeerStream.incomingRetainedItems
          fixture.direction
          (PeerInput.peerInputPeerStreamState completed)
      )
  assertEqual
    "the marker is complete only after both unordered dependencies"
    [(future, PeerStream.GapRetained)]
    completedRetained
  (replayedCompletion, replayedResult) <-
    checked
      "route-cutover completion replay"
      ( releasePendingAlignmentRouteCutoversChecked
          fixture.context
          completed
      )
  assertBool "completion replay is state-idempotent" (sameMarkerSemantics replayedCompletion completed)
  assertEqual "completion replay preserves the installed closure proof" (installedMarker completed) (installedMarker replayedCompletion)
  assertEqual
    "completion replay emits no second completion"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition replayedResult)
  assertEqual
    "completion replay emits no acknowledgement"
    []
    (PeerInput.peerDependencyAcknowledgements replayedResult)

caseDelayedRouteCutoverRejection :: Assertion
caseDelayedRouteCutoverRejection = do
  fixture <- peerFixture
  (_, accepted, generation, predecessorGeneration, _) <- routeCutoverAlignmentFixture fixture
  marker <-
    checked
      "delayed invalid route-cutover marker"
      ( alignmentRouteCutoverMarker
          (fixtureGenerationPlan accepted predecessorGeneration)
          predecessorGeneration
          fixture.remoteEpoch
          fixture.localEpoch
      )
  let payload = PeerLogicalRouteCutover marker
      item =
        sequencedItem
          fixture.direction
          firstStreamSequence
          (peerLogicalPayloadDigest payload)
          payload
  (held, _) <-
    checked
      "retain route cutover before its generation is available"
      ( PeerInput.applyPeerLogicalItem
          fixture.context
          fixture.binding
          item
          fixture.state
      )
  assertIncomingProgress
    "the not-yet-decidable route cutover is received-pending"
    fixture.direction
    [PeerStream.IncomingReceivedPending]
    held
  let exposed = replacePeerInputAlignment accepted held
  (settled, result) <-
    checked
      "reject route cutover after its missing evidence becomes available"
      (releasePendingAlignmentRouteCutoversChecked fixture.context exposed)
  assertEqual
    "the invalid route cutover is transport-terminal"
    PeerInput.PeerDependencyReleased
    (PeerInput.peerDependencyReleaseDisposition result)
  rejection <- case PeerInput.peerDependencyDeferredRejections result of
    [value] -> pure value
    actual ->
      assertFailure
        ("expected one delayed route-cutover rejection, got " <> show actual)
  assertEqual
    "the route-cutover rejection is attributed to its source direction"
    fixture.direction
    (PeerInput.deferredPeerRejectionDirection rejection)
  assertEqual
    "a control-only route-cutover rejection has no publication owner"
    Nothing
    (PeerInput.deferredPeerRejectionPublicationId rejection)
  assertEqual
    "the rejected route-cutover direction receives no acknowledgement"
    []
    (PeerInput.peerDependencyAcknowledgements result)
  let (routeChanges, harvestedAlignment) = Alignment.takeRouteMarkerChanges (PeerInput.peerInputAlignmentState exposed)
  assertEqual
    "both retained generations notify the route release"
    (Set.fromList [generation, predecessorGeneration])
    routeChanges
  assertEqual
    "rejecting the route cutover only consumes its alignment notifications"
    harvestedAlignment
    (PeerInput.peerInputAlignmentState settled)
  forM_ [exposed, settled] assertMarkerSchedulingValid
  assertIncomingProgress
    "the rejected route cutover no longer blocks completion"
    fixture.direction
    []
    settled
  coldComposed <- fullHeraldStateForPeerControlItem fixture fixture.oracle exposed
  let composed = withMarkerReleaseCutQueries coldComposed
  (coordinated, coordinatedEffects, _) <-
    checked
      "coordinate delayed route-cutover rejection"
      (StructuralCoordinator.releasePendingAlignmentRouteCutoversWithDisposition composed)
  assertEqual
    "releasing a rejected route marker preserves cached cut queries"
    (CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache composed))
    (CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache coordinated))
  assertEqual
    "the coordinated route-cutover rejection closes the exact source binding"
    [(fixture.binding, ClosePeerPublicationProtocol)]
    [ (binding, disposition)
    | RejectPeerConnection binding disposition <- effectBatchMembers coordinatedEffects
    ]
  assertBool
    "the coordinated route-cutover rejection sends no later control to that binding"
    ( all
        ( \case
            SendPeerControl binding _ -> binding /= fixture.binding
            _ -> True
        )
        (effectBatchMembers coordinatedEffects)
    )

caseRetiredSourceRouteCutoverSettlement :: Assertion
caseRetiredSourceRouteCutoverSettlement = do
  fixture <- peerFixture
  (promoted, _, generation, predecessorGeneration, _) <- routeCutoverAlignmentFixture fixture
  marker <-
    checked
      "retired-source route-cutover marker"
      ( alignmentRouteCutoverMarker
          (fixtureGenerationPlan promoted generation)
          generation
          fixture.remoteEpoch
          fixture.localEpoch
      )
  let payload = PeerLogicalRouteCutover marker
      item =
        sequencedItem
          fixture.direction
          firstStreamSequence
          (peerLogicalPayloadDigest payload)
          payload
  (held, _) <-
    checked
      "retain route cutover before source retirement"
      ( PeerInput.applyPeerLogicalItem
          fixture.context
          fixture.binding
          item
          fixture.state
      )
  preparedRetirement <-
    checked
      "retire route-cutover source"
      ( PeerStream.preparePeerRetirement
          fixture.remoteEpoch
          (PeerInput.peerInputPeerStreamState held)
      )
  let (retiredPeerStream, _) = PeerStream.commitPeerRetirement preparedRetirement
      generationMissing =
        replacePeerInputPeerStreamAndControlled
          retiredPeerStream
          (PeerInput.peerInputControlledState held)
          held
      acceptanceMissing = replacePeerInputAlignment promoted generationMissing
      settleAndAssert dependency expectedChanges retired = do
        (settled, result) <-
          checked
            ("settle retired-source route cutover with " <> dependency)
            (releasePendingAlignmentRouteCutoversChecked fixture.context retired)
        assertEqual
          (dependency <> " is terminally settled")
          PeerInput.PeerDependencyReleased
          (PeerInput.peerDependencyReleaseDisposition result)
        assertEqual
          "terminal local settlement acknowledges no retired peer"
          []
          (PeerInput.peerDependencyAcknowledgements result)
        let (routeChanges, harvestedAlignment) = Alignment.takeRouteMarkerChanges (PeerInput.peerInputAlignmentState retired)
        assertEqual "only installed generations provide retirement release notifications" expectedChanges routeChanges
        assertEqual
          "terminal settlement only consumes route notifications without retaining a marker"
          harvestedAlignment
          (PeerInput.peerInputAlignmentState settled)
        forM_ [retired, settled] assertMarkerSchedulingValid
        forM_ [PeerStream.RouteCutoverMarkers, PeerStream.DisappearanceProbeMarkers] $ \family ->
          assertBool
            "the exhausted retired direction leaves every marker work index"
            (Set.notMember fixture.direction (PeerStream.incomingMarkerDirections family (PeerInput.peerInputPeerStreamState settled)))
        retained <-
          checked
            "terminally settled retired-source stream"
            ( PeerStream.incomingRetainedItems
                fixture.direction
                (PeerInput.peerInputPeerStreamState settled)
            )
        assertEqual
          "the retained assignment no longer blocks the completed prefix"
          []
          retained
        (replayed, replayResult) <-
          checked
            "replay retired-source route-cutover settlement"
            (releasePendingAlignmentRouteCutoversChecked fixture.context settled)
        assertBool "terminal settlement replay is state-identical" (sameMarkerSemantics replayed settled)
        assertEqual
          "terminal settlement replay emits no second completion"
          PeerInput.PeerDependencyStillHeld
          (PeerInput.peerDependencyReleaseDisposition replayResult)
        assertEqual
          "terminal settlement replay emits no acknowledgement"
          []
          (PeerInput.peerDependencyAcknowledgements replayResult)
  settleAndAssert "missing generation" Set.empty generationMissing
  settleAndAssert "missing source acceptance" (Set.fromList [generation, predecessorGeneration]) acceptanceMissing

caseRouteCutoverCannotOvertakePublication :: Assertion
caseRouteCutoverCannotOvertakePublication = do
  fixture <- peerFixture
  (_, accepted, generation, predecessorGeneration, _) <- routeCutoverAlignmentFixture fixture
  pendingBatch <-
    checked
      "pending predecessor publication"
      ( mkPublicationBatch
          (checkedPublicationId fixture.publication)
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (checkedPublicationCanonicalValue fixture.publication)
          Normal
          fixture.topologyCut
          (controlIndex 1)
          (fixture.destination :| [])
      )
  let publicationItem =
        ordinarySequencedItem
          fixture.direction
          firstStreamSequence
          pendingBatch
      withGeneration = replacePeerInputAlignment accepted fixture.state
  (publicationHeld, publicationResult) <-
    checked
      "retain earlier pending publication"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          publicationItem
          withGeneration
      )
  assertEqual
    "the earlier publication is retained pending"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition publicationResult)
  marker <-
    checked
      "ordered marker behind pending publication"
      ( alignmentRouteCutoverMarker
          (fixtureGenerationPlan accepted generation)
          generation
          fixture.remoteEpoch
          fixture.localEpoch
      )
  let markerPayload = PeerLogicalRouteCutover marker
      markerItem =
        sequencedItem
          fixture.direction
          (nextStreamSequence firstStreamSequence)
          (peerLogicalPayloadDigest markerPayload)
          markerPayload
  (bothHeld, _) <-
    checked
      "retain marker behind pending publication"
      ( PeerInput.applyPeerLogicalItem
          fixture.context
          fixture.binding
          markerItem
          publicationHeld
      )
  (notOvertaken, releaseResult) <-
    checked
      "attempt marker release behind pending publication"
      (releasePendingAlignmentRouteCutoversChecked fixture.context bothHeld)
  let beforeStream = PeerInput.peerInputPeerStreamState bothHeld
      (routeChanges, harvestedAlignment) = Alignment.takeRouteMarkerChanges (PeerInput.peerInputAlignmentState bothHeld)
      parkedStream = PeerStream.consumeIncomingMarkerDirection PeerStream.RouteCutoverMarkers fixture.direction beforeStream
      expectedHeld =
        replacePeerInputAlignment
          harvestedAlignment
          (replacePeerInputPeerStreamAndControlled parkedStream (PeerInput.peerInputControlledState bothHeld) bothHeld)
  assertEqual "the generation notifications are still pending before release" (Set.fromList [generation, predecessorGeneration]) routeChanges
  assertEqual
    "the earlier publication exposes exactly one route-head candidate"
    (Set.singleton fixture.direction)
    (PeerStream.pendingIncomingMarkerDirections PeerStream.RouteCutoverMarkers beforeStream)
  assertBool
    "marker release only consumes notifications and parks the blocked publication head"
    (notOvertaken == expectedHeld)
  assertEqual
    "the blocked route head has no remaining queued evaluation"
    Set.empty
    (PeerStream.pendingIncomingMarkerDirections PeerStream.RouteCutoverMarkers (PeerInput.peerInputPeerStreamState notOvertaken))
  forM_ [bothHeld, notOvertaken] assertMarkerSchedulingValid
  assertEqual
    "no ordered completion is released out of order"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition releaseResult)
  retained <-
    checked
      "ordered mixed pending prefix"
      ( PeerStream.incomingRetainedItems
          fixture.direction
          (PeerInput.peerInputPeerStreamState notOvertaken)
      )
  assertEqual
    "both prefix items remain pending in order"
    [PeerStream.IncomingReceivedPending, PeerStream.IncomingReceivedPending]
    (fmap snd retained)

casePendingMarkerAllowsLaterPublication :: Assertion
casePendingMarkerAllowsLaterPublication = do
  fixture <- peerFixture
  (_, accepted, generation, _, _) <- routeCutoverAlignmentFixture fixture
  marker <-
    checked
      "leading dependency-held route marker"
      ( alignmentRouteCutoverMarker
          (fixtureGenerationPlan accepted generation)
          generation
          fixture.remoteEpoch
          fixture.localEpoch
      )
  let markerPayload = PeerLogicalRouteCutover marker
      markerItem =
        sequencedItem
          fixture.direction
          firstStreamSequence
          (peerLogicalPayloadDigest markerPayload)
          markerPayload
      publicationItem =
        sequencedItem
          fixture.direction
          (nextStreamSequence firstStreamSequence)
          (sequencedItemDigest fixture.item)
          (sequencedItemPayload fixture.item)
  (markerHeld, _) <-
    checked
      "retain leading marker"
      ( PeerInput.applyPeerLogicalItem
          fixture.context
          fixture.binding
          markerItem
          fixture.state
      )
  (laterApplied, publicationResult) <-
    checked
      "apply later publication independently"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          publicationItem
          markerHeld
      )
  assertEqual
    "the later item is received contiguously"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition publicationResult)
  assertEqual
    "the later publication is semantically applied without head-of-line blocking"
    [checkedPublicationId fixture.publication]
    (PeerInput.peerInputPublicationIds publicationResult)
  acknowledgement <- requireAcknowledgement publicationResult
  assertEqual
    "cumulative completion remains behind the earlier marker"
    (Just (nextStreamSequence firstStreamSequence), Nothing)
    ( streamPrefixSequence
        (PeerInput.peerAcknowledgementReceivedPrefix acknowledgement),
      streamPrefixSequence
        (PeerInput.peerAcknowledgementCompletedPrefix acknowledgement)
    )
  beforeRelease <-
    checked
      "independently completed suffix"
      ( PeerStream.incomingRetainedItems
          fixture.direction
          (PeerInput.peerInputPeerStreamState laterApplied)
      )
  assertEqual
    "the later publication completes independently behind the marker"
    [PeerStream.IncomingReceivedPending]
    (fmap snd beforeRelease)
  slotBeforeRelease <- requireDestinationSlot fixture laterApplied
  assertEqual
    "Store applies the independent later publication immediately"
    (Just Normal)
    ( lookup
        (checkedPublicationId fixture.publication)
        (Store.storeSlotApplicationReceipts slotBeforeRelease)
    )
  let dependenciesInstalled = replacePeerInputAlignment accepted laterApplied
  (completed, releaseResult) <-
    checked
      "release marker then later publication"
      ( releasePendingAlignmentRouteCutoversChecked
          fixture.context
          dependenciesInstalled
      )
  assertEqual
    "the earlier marker releases once its own dependency arrives"
    PeerInput.PeerDependencyReleased
    (PeerInput.peerDependencyReleaseDisposition releaseResult)
  assertEqual
    "marker release does not semantically replay the later publication"
    []
    (PeerInput.peerDependencyReleasedPublications releaseResult)
  retained <-
    checked
      "released marker/publication prefix"
      ( PeerStream.incomingRetainedItems
          fixture.direction
          (PeerInput.peerInputPeerStreamState completed)
      )
  assertEqual
    "the cumulative prefix catches up after marker completion"
    []
    (fmap snd retained)

casePublicationCompletionExposesDisappearanceMarker :: Assertion
casePublicationCompletionExposesDisappearanceMarker = do
  fixture <- controlledFixture
  let probeIndex = controlIndex 167
      direction = sequencedItemDirection fixture.item
      markerSequence = nextStreamSequence (sequencedItemSequence fixture.item)
  markerItem <- disappearanceMarkerItem probeIndex direction markerSequence fixture.initialState
  (publicationHeld, _) <-
    applyControlledFixture fixture fixture.initialContext fixture.initialState
  (beforeOpen, _) <-
    checked
      "retain pre-Open disappearance marker behind publication"
      (PeerInput.applyPeerLogicalItem fixture.initialContext fixture.binding markerItem publicationHeld)
  withOpen <- projectRemoteDisappearanceProbe probeIndex direction fixture.peer beforeOpen
  (stillBlocked, earlyRelease) <-
    checked
      "predecessor publication still blocks disappearance marker"
      (releasePendingDisappearanceProbeMarkersChecked fixture.initialContext withOpen)
  assertBool "early disappearance redrive is state-identical" (sameMarkerSemantics stillBlocked withOpen)
  assertEqual
    "early disappearance redrive reports no progress"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition earlyRelease)
  controlReady <- advancePeerInputStructuralControl (controlIndex 1) stillBlocked
  (publicationReleased, controlRelease) <-
    checked
      "release predecessor publication and exposed disappearance marker"
      (PeerInput.releasePeerControl fixture.acceptedContext controlReady)
  assertEqual
    "the predecessor publication releases exactly once"
    [fixture.identifier]
    (PeerInput.peerControlReleasedPublications controlRelease)
  assertIncomingProgress
    "publication and disappearance marker complete in stream order"
    direction
    []
    publicationReleased
  (replayed, replayResult) <-
    checked
      "completed disappearance marker redrive is idempotent"
      (releasePendingDisappearanceProbeMarkersChecked fixture.acceptedContext publicationReleased)
  assertBool "completed marker redrive preserves state" (sameMarkerSemantics replayed publicationReleased)
  assertEqual "completed marker emits no second release" PeerInput.PeerDependencyStillHeld (PeerInput.peerDependencyReleaseDisposition replayResult)

caseRouteCutoverCompletionExposesDisappearanceMarker :: Assertion
caseRouteCutoverCompletionExposesDisappearanceMarker = do
  fixture <- peerFixture
  (_, accepted, generation, _, _) <- routeCutoverAlignmentFixture fixture
  routeMarker <-
    checked
      "disappearance-wake route-cutover marker"
      ( alignmentRouteCutoverMarker
          (fixtureGenerationPlan accepted generation)
          generation
          fixture.remoteEpoch
          fixture.localEpoch
      )
  let probeIndex = controlIndex 168
      routePayload = PeerLogicalRouteCutover routeMarker
      routeItem =
        sequencedItem fixture.direction firstStreamSequence (peerLogicalPayloadDigest routePayload) routePayload
      probeSequence = nextStreamSequence firstStreamSequence
  probeItem <- disappearanceMarkerItem probeIndex fixture.direction probeSequence fixture.state
  (routeHeld, _) <-
    checked
      "retain dependency-held route marker"
      (PeerInput.applyPeerLogicalItem fixture.context fixture.binding routeItem fixture.state)
  (beforeOpen, _) <-
    checked
      "retain pre-Open disappearance marker behind route marker"
      (PeerInput.applyPeerLogicalItem fixture.context fixture.binding probeItem routeHeld)
  withOpen <- projectRemoteDisappearanceProbe probeIndex fixture.direction fixture beforeOpen
  (stillBlocked, earlyRelease) <-
    checked
      "predecessor route marker still blocks disappearance marker"
      (releasePendingDisappearanceProbeMarkersChecked fixture.context withOpen)
  assertBool "early route-held disappearance redrive is state-identical" (sameMarkerSemantics stillBlocked withOpen)
  assertEqual
    "route-held disappearance redrive reports no progress"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition earlyRelease)
  let alignmentReady = replacePeerInputAlignment accepted stillBlocked
  (routeReleased, routeRelease) <-
    checked
      "release predecessor route-cutover marker"
      (releasePendingAlignmentRouteCutoversChecked fixture.context alignmentReady)
  assertEqual "route marker releases" PeerInput.PeerDependencyReleased (PeerInput.peerDependencyReleaseDisposition routeRelease)
  -- The live disappearance driver follows structural/route progression. Its
  -- PeerInput release consumes the newly exposed prefix without requiring a
  -- label-specific pass in StructuralCoordinator.
  (probeReleased, probeRelease) <-
    checked
      "release disappearance marker exposed by route completion"
      (releasePendingDisappearanceProbeMarkersChecked fixture.context routeReleased)
  assertEqual "disappearance marker releases" PeerInput.PeerDependencyReleased (PeerInput.peerDependencyReleaseDisposition probeRelease)
  assertIncomingProgress
    "route marker and disappearance marker complete in stream order"
    fixture.direction
    []
    probeReleased

caseLaterPayloadPrerequisite :: Assertion
caseLaterPayloadPrerequisite = do
  fixture <- peerFixture
  let prerequisite = controlIndex 1
  assertBool
    "the payload prerequisite is later than its source writer creation"
    (prerequisite > fixture.controlPrerequisite)
  batch <-
    checked
      "later-prerequisite publication batch"
      ( mkPublicationBatch
          (checkedPublicationId fixture.publication)
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (checkedPublicationCanonicalValue fixture.publication)
          Normal
          (peerFixtureTopologyCut fixture)
          prerequisite
          (fixture.destination :| [])
      )
  let item =
        ordinarySequencedItem
          fixture.direction
          firstStreamSequence
          batch
  (held, result) <- applyFixture fixture item fixture.state
  record <- requireIncomingById (publicationBatchId batch) held
  assertEqual
    "the exact later control prerequisite is the sole waiter"
    (Set.singleton (Publication.ControlIndexDependency prerequisite))
    (Publication.incomingPublicationDependencies record)
  assertEqual "the item remains dependency-held" Publication.DependencyHeld (Publication.incomingPublicationDisposition record)
  acknowledgement <- requireAcknowledgement result
  assertEqual
    "the complete item advances received"
    (Just firstStreamSequence)
    (streamPrefixSequence (PeerInput.peerAcknowledgementReceivedPrefix acknowledgement))
  assertEqual
    "control-independent source admission does not advance completed"
    emptyStreamPrefix
    (PeerInput.peerAcknowledgementCompletedPrefix acknowledgement)
  assertBool
    "held work is invisible in Store"
    (PeerInput.peerInputStoreState fixture.state == PeerInput.peerInputStoreState held)
  assertBool
    "held work is invisible in Graph"
    (PeerInput.peerInputGraphState fixture.state == PeerInput.peerInputGraphState held)

caseRetiredPeerDefinition :: Assertion
caseRetiredPeerDefinition = do
  controlFixture <- controlledFixture
  let fixture = controlFixture.peer
      resolveIndex = controlIndex 1
      firstSequence = firstStreamSequence
      staleSequence = nextStreamSequence firstSequence
      successorSequence = nextStreamSequence staleSequence
      descriptor = regularPeerDescriptor
      sortId = descriptorSortId descriptor
      genesisOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixture.genesis)
          sortId
          Genesis
  (firstPublication, _firstBatch, firstItem) <-
    peerDefinitionItem fixture 20 (controlIndex 0) firstSequence descriptor
  (defined, firstResult) <-
    checked
      "initial peer sort definition"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          firstItem
          fixture.state
      )
  assertEqual
    "the initial peer definition completes"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition firstResult)
  initialEntry <- requireEffectiveSort sortId defined
  assertEqual
    "the first definition induces the Genesis occurrence"
    genesisOccurrence
    (SortRegistry.registryEntryOccurrenceId initialEntry)
  assertEqual
    "the induced entry retains its definition publication"
    (checkedPublicationId firstPublication)
    (SortRegistry.registryEntryPublicationId initialEntry)

  successorBase <-
    checked
      "regular retirement successor base"
      (resolvedRetirementOccurrenceBase resolveIndex)
  let successorOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixture.genesis)
          sortId
          successorBase
      subject =
        regularSortDefinitionDisappearanceSubject
          ( deriveRegularSortOccurrenceClaim
              (checkedSystemId fixture.genesis)
              descriptor
              Genesis
          )
  preparedRegistry <-
    checked
      "retire peer-induced sort occurrence"
      ( SortRegistry.prepareExactRegularSortRetirement
          (checkedSystemId fixture.genesis)
          initialEntry
          resolveIndex
          successorOccurrence
          (PeerInput.peerInputSortRegistryState defined)
      )
  preparedStore <-
    checked
      "install peer-definition Store suppression"
      ( Store.prepareRegularDefinitionRetirement
          subject
          EmptyHeraldPublicationPrefix
          resolveIndex
          (PeerInput.peerInputStoreState defined)
      )
  let retiredRegistry = SortRegistry.commitRegularSortRetirement preparedRegistry
      (retiredStore, _) = Store.commitRegularDefinitionRetirement preparedStore
      retired = replacePeerInputStoreAndRegistry retiredStore retiredRegistry defined
  controlReady <- advancePeerInputStructuralControl resolveIndex retired

  (stalePublication, staleBatch, staleItem) <-
    peerDefinitionItem fixture 21 (controlIndex 0) staleSequence descriptor
  (afterStale, staleResult) <-
    checked
      "stale peer definition after retirement"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          staleItem
          controlReady
      )
  assertEqual
    "the stale peer item completes instead of wedging"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition staleResult)
  staleRecord <- requireIncomingById (checkedPublicationId stalePublication) afterStale
  assertEqual
    "the stale destination is terminally ignored"
    (Just Publication.DestinationTerminallyIgnored)
    ( Map.lookup
        fixture.destination
        (Publication.incomingPublicationDestinationOutcomes staleRecord)
    )
  assertEqual
    "the stale publication is terminally settled"
    Publication.TerminallyIgnored
    (Publication.incomingPublicationDisposition staleRecord)
  staleAcknowledgement <- requireAcknowledgement staleResult
  assertEqual
    "terminal settlement advances the completed stream prefix"
    (Just staleSequence)
    ( streamPrefixSequence
        (PeerInput.peerAcknowledgementCompletedPrefix staleAcknowledgement)
    )
  assertBool
    "terminal ignore performs no Store work"
    ( PeerInput.peerInputStoreState afterStale
        == PeerInput.peerInputStoreState controlReady
    )
  assertBool
    "terminal ignore does not attempt registry induction"
    ( PeerInput.peerInputSortRegistryState afterStale
        == PeerInput.peerInputSortRegistryState controlReady
    )
  assertEqual
    "the retired sort remains absent after stale traffic"
    Nothing
    ( SortRegistry.lookupEffectiveSort
        sortId
        (PeerInput.peerInputSortRegistryState afterStale)
    )
  assertEqual
    "stale traffic retains the primordial carrier occurrence"
    fixture.occurrence
    (publicationBatchOccurrenceId (Publication.incomingPublicationBatch staleRecord))
  assertEqual
    "the stale fixture carries the intended lower prerequisite"
    (controlIndex 0)
    (publicationBatchControlPrerequisite staleBatch)

  (successorPublication, successorBatch, successorItem) <-
    peerDefinitionItem fixture 22 resolveIndex successorSequence descriptor
  (redefined, successorResult) <-
    checked
      "Resolve-qualified peer redefinition"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          successorItem
          afterStale
      )
  assertEqual
    "the Resolve-qualified peer item completes"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition successorResult)
  successorRecord <-
    requireIncomingById (checkedPublicationId successorPublication) redefined
  assertEqual
    "the Resolve-qualified destination is applied"
    (Just Publication.DestinationApplied)
    ( Map.lookup
        fixture.destination
        (Publication.incomingPublicationDestinationOutcomes successorRecord)
    )
  assertEqual
    "the Resolve-qualified publication is applied"
    Publication.Applied
    (Publication.incomingPublicationDisposition successorRecord)
  successorAcknowledgement <- requireAcknowledgement successorResult
  assertEqual
    "the redefinition advances the completed stream prefix"
    (Just successorSequence)
    ( streamPrefixSequence
        (PeerInput.peerAcknowledgementCompletedPrefix successorAcknowledgement)
    )
  successorSlot <- requireDestinationSlot fixture redefined
  assertEqual
    "the Store retains the Resolve-qualified definition receipt"
    (Just Normal)
    ( lookup
        (checkedPublicationId successorPublication)
        (Store.storeSlotApplicationReceipts successorSlot)
    )
  successorEntry <- requireEffectiveSort sortId redefined
  assertEqual
    "the successor Registry entry retains the equal descriptor"
    descriptor
    (SortRegistry.registryEntryDescriptor successorEntry)
  assertEqual
    "the redefinition induces the Resolve-derived successor occurrence"
    successorOccurrence
    (SortRegistry.registryEntryOccurrenceId successorEntry)
  assertEqual
    "the successor entry retains the new definition publication"
    (checkedPublicationId successorPublication)
    (SortRegistry.registryEntryPublicationId successorEntry)
  assertEqual
    "the peer batch still uses the primordial sort-definition carrier occurrence"
    fixture.occurrence
    (publicationBatchOccurrenceId (Publication.incomingPublicationBatch successorRecord))
  assertEqual
    "the successor fixture carries the exact Resolve prerequisite"
    resolveIndex
    (publicationBatchControlPrerequisite successorBatch)
  assertBool
    "the public successor coordinate differs from its carrier coordinate"
    (successorOccurrence /= fixture.occurrence)
  let nablaStaleStructuralSequence = firstStructuralSequence
      nablaSuccessorStructuralSequence =
        nextStructuralSequence nablaStaleStructuralSequence
      deltaStaleStructuralSequence =
        nextStructuralSequence nablaSuccessorStructuralSequence
      deltaSuccessorStructuralSequence =
        nextStructuralSequence deltaStaleStructuralSequence
  (afterNabla, nextSequence) <-
    exerciseRetiredStructuralReference
      controlFixture.acceptedContext
      fixture
      sortId
      successorOccurrence
      resolveIndex
      NablaRole
      NablaCarrier
      (structuralOccurrenceId fixture.remoteEpoch nablaStaleStructuralSequence)
      (structuralOccurrenceId fixture.remoteEpoch nablaSuccessorStructuralSequence)
      (nextStreamSequence successorSequence)
      redefined
  _ <-
    exerciseRetiredStructuralReference
      controlFixture.acceptedContext
      fixture
      sortId
      successorOccurrence
      resolveIndex
      DeltaRole
      DeltaCarrier
      (structuralOccurrenceId fixture.remoteEpoch deltaStaleStructuralSequence)
      (structuralOccurrenceId fixture.remoteEpoch deltaSuccessorStructuralSequence)
      nextSequence
      afterNabla
  pure ()

caseRetiredOrdinaryPeerValue :: Assertion
caseRetiredOrdinaryPeerValue = do
  controlFixture <- controlledFixture
  let fixture = controlFixture.peer
      descriptor = labelControlledPeerDescriptor
      sortId = descriptorSortId descriptor
      resolveIndex = controlIndex 1
      oldOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixture.genesis)
          sortId
          Genesis
      streamSequences = iterate nextStreamSequence firstStreamSequence
      definitionSequence = streamSequences !! 0
      nablaSequenceNumber = streamSequences !! 1
      deltaSequenceNumber = streamSequences !! 2
      heldValueSequence = streamSequences !! 3
      staleBeforeSequence = streamSequences !! 4
      redefinitionSequence = streamSequences !! 5
      staleAfterSequence = streamSequences !! 6
      rejectedSequence = streamSequences !! 7
      futureDefinitionSequence = streamSequences !! 8
      nablaStructuralOccurrence =
        structuralOccurrenceId fixture.remoteEpoch firstStructuralSequence
      deltaStructuralOccurrence =
        structuralOccurrenceId
          fixture.remoteEpoch
          (nextStructuralSequence firstStructuralSequence)

  (_, _, definitionItem) <-
    peerDefinitionItem fixture 60 (controlIndex 0) definitionSequence descriptor
  (defined, _) <-
    checked
      "define ordinary peer-value sort"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          definitionItem
          fixture.state
      )
  retiredEntry <- requireEffectiveSort sortId defined

  writerNabla <-
    checked
      "retired ordinary writer Nabla"
      (mkNablaId (fixtureIdentifierBytes 0xe3))
  writerValue <-
    regularEndpointValue
      fixture.sourceProcess
      NablaRole
      (globalObjectIdFromNablaId writerNabla)
      sortId
  writerRoot <-
    requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap NablaRole
  (_, writerItem) <-
    structuralReferenceItem
      fixture
      writerRoot
      NablaRole
      NablaCarrier
      60
      (controlIndex 0)
      nablaSequenceNumber
      nablaStructuralOccurrence
      ( GraphProgress.structuralAppliedVector
          (PeerInput.peerInputStructuralProgressState defined)
      )
      writerValue
  (withWriter, _) <-
    checked
      "establish ordinary peer-value writer"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          writerItem
          defined
      )

  readerDelta <-
    checked
      "retired ordinary reader Delta"
      (mkDeltaId (fixtureIdentifierBytes 0xe4))
  readerValue <-
    regularEndpointValue
      (appliedProcessEpochId fixture.localSource)
      DeltaRole
      (globalObjectIdFromDeltaId readerDelta)
      sortId
  readerRoot <-
    requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap DeltaRole
  (readerIdentifier, readerItem) <-
    structuralReferenceItem
      fixture
      readerRoot
      DeltaRole
      DeltaCarrier
      60
      (controlIndex 0)
      deltaSequenceNumber
      deltaStructuralOccurrence
      ( GraphProgress.structuralAppliedVector
          (PeerInput.peerInputStructuralProgressState withWriter)
      )
      readerValue
  possessionDelta <-
    case [ delta
         | root <- appliedProcessRoots fixture.localSource,
           appliedRootCatalogueRole root == DeltaRole,
           ReaderRoot delta <- [appliedRootRole root]
         ] of
      delta : _ -> pure delta
      [] -> assertFailure "ordinary peer-value fixture has no local Delta reader"
  readerPublication <-
    checked
      "ordinary peer-value reader controlled publication"
      ( mkCheckedPublication
          (predefinedCatalogueDescriptor (profileEntryFor DeltaRole))
          readerIdentifier
          readerValue
      )
  readerObservation <-
    checked
      "ordinary peer-value reader controlled observation"
      ( Controlled.checkControlledObservation
          (predefinedCatalogueDescriptor (profileEntryFor DeltaRole))
          (appliedRootOccurrenceId readerRoot)
          readerPublication
      )
  preparedReaderObservation <-
    checked
      "retain ordinary peer-value reader observation"
      ( Controlled.prepareControlledPeerObservation
          readerObservation
          (PeerInput.peerInputControlledState withWriter)
      )
  let (withReaderObservation, _) =
        Controlled.commitControlledPeerObservation preparedReaderObservation
  preparedReaderPossession <-
    checked
      "retain ordinary peer-value reader possession"
      ( Controlled.prepareControlledStoreRead
          (appliedProcessEpochId fixture.localSource)
          possessionDelta
          Normal
          readerObservation
          withReaderObservation
      )
  let withReaderPossession =
        replacePeerInputControlled
          (Controlled.commitControlledStoreObservation preparedReaderPossession)
          withWriter
  (withEndpoints, _) <-
    checked
      "establish ordinary peer-value reader"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          readerItem
          withReaderPossession
      )
  readerRecord <- requireIncomingById readerIdentifier withEndpoints
  assertEqual
    "the ordinary peer-value reader carrier applies"
    Publication.Applied
    (Publication.incomingPublicationDisposition readerRecord)
  readerSlot <-
    maybe
      (assertFailure "ordinary peer-value reader has no local Store slot")
      pure
      ( Store.lookupStoreSlot
          readerDelta
          (PeerInput.peerInputStoreState withEndpoints)
      )
  let destination =
        publicationDestination
          readerDelta
          (Store.storeSlotIncarnation readerSlot)
          Normal
  (withTopology, authorityCut) <-
    establishPeerInputTopologyCut fixture withEndpoints
  let writerAuthority =
        structuralAuthorityEpoch nablaStructuralOccurrence authorityCut
      controlledObject = globalObjectIdFromNablaId writerNabla
  controlledWriterRecord <-
    maybe
      (assertFailure "the controlled ordinary target has no retained owner")
      pure
      ( Controlled.controlledLocalRecord
          controlledObject
          (PeerInput.peerInputControlledState withTopology)
      )
  assertBool
    "the controlled ordinary target deliberately conflicts with its carrier sort"
    (Controlled.controlledRecordSortId controlledWriterRecord /= sortId)

  (heldIdentifier, _, heldItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      oldOccurrence
      resolveIndex
      authorityCut
      destination
      1
      heldValueSequence
  (held, _) <-
    checked
      "hold ordinary value before regular retirement"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          heldItem
          withTopology
      )
  heldRecord <- requireIncomingById heldIdentifier held
  assertEqual
    "the pre-retirement value waits only for its future control prerequisite"
    (Set.singleton (Publication.ControlIndexDependency resolveIndex))
    (Publication.incomingPublicationDependencies heldRecord)

  successorBase <-
    checked
      "ordinary peer-value retirement successor base"
      (resolvedRetirementOccurrenceBase resolveIndex)
  let successorOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixture.genesis)
          sortId
          successorBase
      subject =
        regularSortDefinitionDisappearanceSubject
          ( deriveRegularSortOccurrenceClaim
              (checkedSystemId fixture.genesis)
              descriptor
              Genesis
          )
  preparedRegistry <-
    checked
      "retire ordinary peer-value registry occurrence"
      ( SortRegistry.prepareExactRegularSortRetirement
          (checkedSystemId fixture.genesis)
          retiredEntry
          resolveIndex
          successorOccurrence
          (PeerInput.peerInputSortRegistryState held)
      )
  preparedStore <-
    checked
      "retire ordinary peer-value Store occurrence"
      ( Store.prepareRegularDefinitionRetirement
          subject
          EmptyHeraldPublicationPrefix
          resolveIndex
          (PeerInput.peerInputStoreState held)
      )
  let retiredRegistry = SortRegistry.commitRegularSortRetirement preparedRegistry
      (retiredStore, _) = Store.commitRegularDefinitionRetirement preparedStore
      retired = replacePeerInputStoreAndRegistry retiredStore retiredRegistry held
  controlReady <- advancePeerInputStructuralControl resolveIndex retired
  (released, releaseResult) <-
    checked
      "release value retained across regular retirement"
      (PeerInput.releasePeerControl controlFixture.acceptedContext controlReady)
  assertTerminalOrdinary
    "value retained across retirement"
    heldIdentifier
    destination
    released
  assertRetiredOrdinarySemanticOwnersUnchanged
    "value retained across retirement"
    controlReady
    released
  assertEqual
    "the retained ordinary value is released exactly once"
    [heldIdentifier]
    (PeerInput.peerControlReleasedPublications releaseResult)

  wrongOccurrence <-
    checked
      "non-historical ordinary occurrence"
      (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xe5))
  futureTopologyCut <-
    checked
      "future ordinary source topology cut"
      (mkTopologyCutId (fixtureIdentifierBytes 0xe8))
  (gapSuccessorIdentifier, _, gapSuccessorItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      successorOccurrence
      resolveIndex
      futureTopologyCut
      destination
      8
      staleBeforeSequence
  (gapSuccessorHeld, _) <-
    checked
      "hold the expected successor while its definition and topology are missing"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          gapSuccessorItem
          released
      )
  gapSuccessorRecord <-
    requireIncomingById gapSuccessorIdentifier gapSuccessorHeld
  assertEqual
    "the expected successor occurrence remains eligible for definition catch-up"
    ( Set.fromList
        [ Publication.EffectiveSortDependency sortId successorOccurrence,
          Publication.SourceTopologyDependency futureTopologyCut
        ]
    )
    (Publication.incomingPublicationDependencies gapSuccessorRecord)

  (_, _, gapMismatchItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      wrongOccurrence
      resolveIndex
      futureTopologyCut
      destination
      9
      staleBeforeSequence
  assertProtocolProblem
    "a decidable non-historical occurrence cannot hide behind definition catch-up"
    ( isProtocol
        ( \case
            PeerInput.PeerInputEffectiveOccurrenceMismatch
              observedSort
              expected
              observed ->
                observedSort == sortId
                  && expected == successorOccurrence
                  && observed == wrongOccurrence
            _ -> False
        )
    )
    ( PeerInput.applyPeerPublication
        controlFixture.acceptedContext
        fixture.binding
        gapMismatchItem
        released
    )

  (futureSuccessorIdentifier, _, futureSuccessorItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      successorOccurrence
      resolveIndex
      authorityCut
      destination
      10
      staleBeforeSequence
  (futureSuccessorHeld, _) <-
    checked
      "hold the expected successor ahead of its control prerequisite"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          futureSuccessorItem
          released
      )
  futureSuccessorRecord <-
    requireIncomingById futureSuccessorIdentifier futureSuccessorHeld
  assertEqual
    "future control leaves the expected successor speculative undecided"
    ( Set.fromList
        [ Publication.EffectiveSortDependency sortId successorOccurrence,
          Publication.ControlIndexDependency resolveIndex
        ]
    )
    (Publication.incomingPublicationDependencies futureSuccessorRecord)
  (futureSuccessorAfterControl, _) <-
    checked
      "release expected-successor control while its definition remains missing"
      ( PeerInput.releasePeerControl
          controlFixture.acceptedContext
          futureSuccessorHeld
      )
  futureSuccessorAfterControlRecord <-
    requireIncomingById futureSuccessorIdentifier futureSuccessorAfterControl
  assertEqual
    "control release retains only expected-successor definition catch-up"
    ( Set.singleton
        (Publication.EffectiveSortDependency sortId successorOccurrence)
    )
    (Publication.incomingPublicationDependencies futureSuccessorAfterControlRecord)

  (staleBeforeIdentifier, _, staleBeforeItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      oldOccurrence
      (controlIndex 0)
      authorityCut
      destination
      2
      staleBeforeSequence
  (staleBefore, staleBeforeResult) <-
    checked
      "retired ordinary value before equal redefinition"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          staleBeforeItem
          released
      )
  assertEqual
    "the directly received retired value completes"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition staleBeforeResult)
  assertTerminalOrdinary
    "retired ordinary value before redefinition"
    staleBeforeIdentifier
    destination
    staleBefore
  assertRetiredOrdinarySemanticOwnersUnchanged
    "retired ordinary value before redefinition"
    released
    staleBefore

  (_, _, redefinitionItem) <-
    peerDefinitionItem
      fixture
      61
      resolveIndex
      redefinitionSequence
      descriptor
  (redefined, _) <-
    checked
      "equal peer redefinition for ordinary stale ingress"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          redefinitionItem
          staleBefore
      )
  successorEntry <- requireEffectiveSort sortId redefined
  assertEqual
    "the equal redefinition installs the expected successor occurrence"
    successorOccurrence
    (SortRegistry.registryEntryOccurrenceId successorEntry)

  (staleAfterIdentifier, _, staleAfterItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      oldOccurrence
      (controlIndex 0)
      authorityCut
      destination
      3
      staleAfterSequence
  (staleAfter, staleAfterResult) <-
    checked
      "retired ordinary value after equal redefinition"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          staleAfterItem
          redefined
      )
  assertEqual
    "the old occurrence does not protocol-fault after redefinition"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition staleAfterResult)
  assertTerminalOrdinary
    "retired ordinary value after redefinition"
    staleAfterIdentifier
    destination
    staleAfter
  assertRetiredOrdinarySemanticOwnersUnchanged
    "retired ordinary value after redefinition"
    redefined
    staleAfter

  (futureMismatchIdentifier, _, futureMismatchItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      wrongOccurrence
      resolveIndex
      authorityCut
      destination
      4
      rejectedSequence
  (futureMismatchHeld, _) <-
    checked
      "hold a non-historical occurrence ahead of its control prefix"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          futureMismatchItem
          staleAfter
      )
  futureMismatchRecord <-
    requireIncomingById futureMismatchIdentifier futureMismatchHeld
  assertEqual
    "the not-yet-decidable occurrence retains sort and control dependencies"
    ( Set.fromList
        [ Publication.EffectiveSortDependency sortId wrongOccurrence,
          Publication.ControlIndexDependency resolveIndex
        ]
    )
    (Publication.incomingPublicationDependencies futureMismatchRecord)
  (futureMismatchRejected, mismatchReleaseResult) <-
    checked
      "reject the non-historical occurrence once its control prefix is covered"
      ( PeerInput.releasePeerControl
          controlFixture.acceptedContext
          futureMismatchHeld
      )
  futureMismatchRejectedRecord <-
    requireIncomingById futureMismatchIdentifier futureMismatchRejected
  assertEqual
    "control release records a transport-level protocol rejection"
    Publication.ProtocolRejected
    (Publication.incomingPublicationDisposition futureMismatchRejectedRecord)
  mismatchRejection <- case PeerInput.peerControlDeferredRejections mismatchReleaseResult of
    [value] -> pure value
    actual -> assertFailure ("expected one control-release rejection, got " <> show actual)
  assertEqual
    "control release attributes the rejection to the retained publication"
    (Just futureMismatchIdentifier)
    (PeerInput.deferredPeerRejectionPublicationId mismatchRejection)
  assertEqual
    "control release attributes the rejection to the source direction"
    fixture.direction
    (PeerInput.deferredPeerRejectionDirection mismatchRejection)
  assertEqual
    "control release sends no acknowledgement on the rejected direction"
    []
    (PeerInput.peerControlAcknowledgements mismatchReleaseResult)
  (afterRejectedReplay, rejectedReplayResult) <-
    checked
      "replay the exact rejected stream assignment"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          futureMismatchItem
          futureMismatchRejected
      )
  assertBool
    "an exact rejected assignment replay changes no owner"
    (afterRejectedReplay == futureMismatchRejected)
  assertEqual
    "an exact rejected assignment remains a transport duplicate"
    (PeerInput.PeerInputReceived ReceiveDuplicate)
    (PeerInput.peerInputDisposition rejectedReplayResult)
  assertBool
    "rejected assignment replay is covered by compact completion"
    ( PeerStream.incomingSequenceCompleted
        (fixture.direction, sequencedItemSequence futureMismatchItem)
        (PeerInput.peerInputPeerStreamState afterRejectedReplay)
    )
  let reassignedMismatch =
        sequencedItem
          (sequencedItemDirection futureMismatchItem)
          (nextStreamSequence (sequencedItemSequence futureMismatchItem))
          (sequencedItemDigest futureMismatchItem)
          (sequencedItemPayload futureMismatchItem)
  case PeerInput.applyPeerPublication controlFixture.acceptedContext fixture.binding reassignedMismatch futureMismatchRejected of
    Left problem ->
      assertEqual
        "a fresh assignment still cannot revive the rejected publication"
        (PeerInput.PeerInputPublicationProblem (Publication.IncomingPublicationRejectedSemanticReassignment futureMismatchIdentifier))
        problem
    Right _ -> assertFailure "a fresh stream assignment revived a rejected semantic publication"

  (endedWriterIdentifier, _, endedWriterItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      oldOccurrence
      (controlIndex 0)
      authorityCut
      destination
      5
      rejectedSequence
  let endedWriterState =
        replacePeerInputControlled
          ( Controlled.commitControlledProcessRetirement
              ( Controlled.prepareControlledProcessRetirement
                  fixture.sourceProcess
                  (PeerInput.peerInputControlledState staleAfter)
              )
          )
          staleAfter
  (afterEndedWriter, _) <-
    checked
      "pre-End publication still settles against its retired sort"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          endedWriterItem
          endedWriterState
      )
  assertTerminalOrdinary
    "later writer End does not recall the frozen publication"
    endedWriterIdentifier
    destination
    afterEndedWriter
  assertRetiredOrdinarySemanticOwnersUnchanged
    "retired-sort suppression still applies after writer End"
    endedWriterState
    afterEndedWriter

  (_, _, wrongItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      sortId
      wrongOccurrence
      (controlIndex 0)
      authorityCut
      destination
      6
      rejectedSequence
  assertProtocolProblem
    "a non-historical occurrence of the known sort still faults"
    ( isProtocol
        ( \case
            PeerInput.PeerInputSourceOccurrenceMismatch source expected observed ->
              source == writerNabla
                && expected == oldOccurrence
                && observed == wrongOccurrence
            _ -> False
        )
    )
    ( PeerInput.applyPeerPublication
        controlFixture.acceptedContext
        fixture.binding
        wrongItem
        staleAfter
    )

  let futureDescriptor = labelControlledPeerDescriptorWithRetention 1
      unknownSort = descriptorSortId futureDescriptor
  unknownOccurrence <-
    checked
      "unknown ordinary sort occurrence"
      (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xe7))
  (unknownIdentifier, _, unknownItem) <-
    labelControlledOrdinaryPeerItem
      fixture
      descriptor
      writerNabla
      writerAuthority
      unknownSort
      unknownOccurrence
      resolveIndex
      authorityCut
      destination
      7
      rejectedSequence
  (unknownHeld, _) <-
    checked
      "retain truly unknown ordinary sort"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          unknownItem
          staleAfter
      )
  unknownRecord <- requireIncomingById unknownIdentifier unknownHeld
  assertEqual
    "a truly unknown sort remains dependency-held"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition unknownRecord)
  assertEqual
    "a truly unknown sort retains its exact sort and control dependencies"
    ( Set.fromList
        [ Publication.EffectiveSortDependency unknownSort unknownOccurrence,
          Publication.ControlIndexDependency resolveIndex
        ]
    )
    (Publication.incomingPublicationDependencies unknownRecord)
  (unknownAfterControl, _) <-
    checked
      "release control while the sort remains truly unknown"
      ( PeerInput.releasePeerControl
          controlFixture.acceptedContext
          unknownHeld
      )
  unknownAfterControlRecord <-
    requireIncomingById unknownIdentifier unknownAfterControl
  assertEqual
    "a truly unknown sort remains held after control catches up"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition unknownAfterControlRecord)
  assertEqual
    "control release retains only the exact unknown-sort dependency"
    (Set.singleton (Publication.EffectiveSortDependency unknownSort unknownOccurrence))
    (Publication.incomingPublicationDependencies unknownAfterControlRecord)

  let expectedFutureOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixture.genesis)
          unknownSort
          Genesis
  (_, _, futureDefinitionItem) <-
    peerDefinitionItem
      fixture
      62
      resolveIndex
      futureDefinitionSequence
      futureDescriptor
  (futureInstalled, futureResult) <-
    checked
      "install a sort which exposes a covered nonhistorical waiter"
      ( PeerInput.applyPeerPublication
          controlFixture.acceptedContext
          fixture.binding
          futureDefinitionItem
          unknownAfterControl
      )
  effectiveFuture <-
    maybe
      (assertFailure "the valid future descriptor was not installed")
      pure
      ( SortRegistry.lookupEffectiveSort
          unknownSort
          (PeerInput.peerInputSortRegistryState futureInstalled)
      )
  assertEqual
    "the valid descriptor installs its canonical occurrence"
    expectedFutureOccurrence
    (SortRegistry.registryEntryOccurrenceId effectiveFuture)
  rejectedRecord <- requireIncomingById unknownIdentifier futureInstalled
  assertEqual
    "the malformed retained publication becomes transport-terminal"
    Publication.ProtocolRejected
    (Publication.incomingPublicationDisposition rejectedRecord)
  assertEqual
    "protocol rejection removes every executable waiter"
    Set.empty
    (Publication.incomingPublicationDependencies rejectedRecord)
  assertBool
    "protocol rejection applies no destination"
    ( all
        (== Publication.DestinationPending)
        (Map.elems (Publication.incomingPublicationDestinationOutcomes rejectedRecord))
    )
  rejection <- case PeerInput.peerInputDeferredRejections futureResult of
    [value] -> pure value
    actual -> assertFailure ("expected one attributed deferred rejection, got " <> show actual)
  assertEqual
    "the deferred rejection names the retained publication"
    (Just unknownIdentifier)
    (PeerInput.deferredPeerRejectionPublicationId rejection)
  assertEqual
    "the deferred rejection is charged to the retained source direction"
    fixture.direction
    (PeerInput.deferredPeerRejectionDirection rejection)
  assertEqual
    "the rejected direction receives no acknowledgement"
    []
    (PeerInput.peerInputAcknowledgements futureResult)
  assertIncomingProgress
    "the rejected assignment is transport-complete"
    fixture.direction
    []
    futureInstalled
  (afterReplay, replayResult) <-
    checked
      "repeat the exposed sort dependency"
      ( PeerInput.releasePeerDependency
          controlFixture.acceptedContext
          (Publication.EffectiveSortDependency unknownSort unknownOccurrence)
          futureInstalled
      )
  assertBool "rejected dependency replay is state-idempotent" (afterReplay == futureInstalled)
  assertEqual
    "rejected dependency replay emits no second close"
    []
    (PeerInput.peerDependencyDeferredRejections replayResult)

assertTerminalOrdinary ::
  String ->
  PublicationId ->
  PublicationDestination ->
  PeerInput.PeerInputState ->
  Assertion
assertTerminalOrdinary label identifier destination state = do
  record <- requireIncomingById identifier state
  assertEqual
    (label <> " settles terminally")
    Publication.TerminallyIgnored
    (Publication.incomingPublicationDisposition record)
  assertEqual
    (label <> " terminally ignores its destination")
    (Just Publication.DestinationTerminallyIgnored)
    ( Map.lookup
        destination
        (Publication.incomingPublicationDestinationOutcomes record)
    )

assertRetiredOrdinarySemanticOwnersUnchanged ::
  String ->
  PeerInput.PeerInputState ->
  PeerInput.PeerInputState ->
  Assertion
assertRetiredOrdinarySemanticOwnersUnchanged label predecessor successor = do
  assertBool
    (label <> " leaves Store state byte-identical")
    ( PeerInput.peerInputStoreState successor
        == PeerInput.peerInputStoreState predecessor
    )
  assertBool
    (label <> " leaves Controlled state byte-identical")
    ( PeerInput.peerInputControlledState successor
        == PeerInput.peerInputControlledState predecessor
    )
  assertBool
    (label <> " changes no semantic owner")
    ( PeerInput.peerInputStoreState successor
        == PeerInput.peerInputStoreState predecessor
        && PeerInput.peerInputGraphState successor
          == PeerInput.peerInputGraphState predecessor
        && PeerInput.peerInputIdGeneratorState successor
          == PeerInput.peerInputIdGeneratorState predecessor
        && PeerInput.peerInputStructuralProgressState successor
          == PeerInput.peerInputStructuralProgressState predecessor
        && PeerInput.peerInputAlignmentState successor
          == PeerInput.peerInputAlignmentState predecessor
        && PeerInput.peerInputPlacementState successor
          == PeerInput.peerInputPlacementState predecessor
        && PeerInput.peerInputControlledState successor
          == PeerInput.peerInputControlledState predecessor
        && PeerInput.peerInputSortRegistryState successor
          == PeerInput.peerInputSortRegistryState predecessor
        && PeerInput.peerInputApplicationState successor
          == PeerInput.peerInputApplicationState predecessor
        && PeerInput.peerInputWaitState successor
          == PeerInput.peerInputWaitState predecessor
        && PeerInput.peerInputLabelBarrierState successor
          == PeerInput.peerInputLabelBarrierState predecessor
    )

exerciseRetiredStructuralReference ::
  PeerInput.PeerInputContext ->
  PeerFixture ->
  SortId ->
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  PredefinedSortRole ->
  StructuralCarrierRole ->
  StructuralOccurrenceId ->
  StructuralOccurrenceId ->
  StreamSequence ->
  PeerInput.PeerInputState ->
  IO (PeerInput.PeerInputState, StreamSequence)
exerciseRetiredStructuralReference
  context
  fixture
  referencedSort
  successorOccurrence
  resolveIndex
  role
  carrierRole
  staleStructuralOccurrence
  successorStructuralOccurrence
  staleSequence
  predecessor = do
    root <- requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap role
    object <- case role of
      NablaRole ->
        globalObjectIdFromNablaId
          <$> checked
            "retired-reference Nabla object"
            (mkNablaId (fixtureIdentifierBytes 0xe1))
      DeltaRole ->
        globalObjectIdFromDeltaId
          <$> checked
            "retired-reference Delta object"
            (mkDeltaId (fixtureIdentifierBytes 0xe2))
      _ -> assertFailure "retired structural reference test requires Nabla or Delta"
    value <- structuralReferenceValue fixture role object referencedSort
    let predecessorVector =
          GraphProgress.structuralAppliedVector
            (PeerInput.peerInputStructuralProgressState predecessor)
        successorSequence = nextStreamSequence staleSequence
        publicationOrdinal = case role of
          NablaRole -> 40
          DeltaRole -> 50
          _ -> error "retired structural reference test requires Nabla or Delta"
    (staleIdentifier, staleItem) <-
      structuralReferenceItem
        fixture
        root
        role
        carrierRole
        publicationOrdinal
        (controlIndex 0)
        staleSequence
        staleStructuralOccurrence
        predecessorVector
        value
    (afterStale, staleResult) <-
      checked
        (show role <> " stale structural reference")
        ( PeerInput.applyPeerPublication
            context
            fixture.binding
            staleItem
            predecessor
        )
    assertEqual
      (show role <> " stale structural reference completes")
      (PeerInput.PeerInputReceived ReceiveContiguous)
      (PeerInput.peerInputDisposition staleResult)
    staleRecord <- requireIncomingById staleIdentifier afterStale
    assertEqual
      (show role <> " stale structural reference is terminally ignored")
      Publication.TerminallyIgnored
      (Publication.incomingPublicationDisposition staleRecord)
    assertBool
      (show role <> " marks every stale destination terminal")
      ( all
          (== Publication.DestinationTerminallyIgnored)
          ( Map.elems
              (Publication.incomingPublicationDestinationOutcomes staleRecord)
          )
      )
    assertBool
      (show role <> " stale reference performs no Store work")
      ( PeerInput.peerInputStoreState afterStale
          == PeerInput.peerInputStoreState predecessor
      )
    assertBool
      (show role <> " stale reference performs no Graph work")
      ( PeerInput.peerInputGraphState afterStale
          == PeerInput.peerInputGraphState predecessor
      )
    assertBool
      (show role <> " stale reference performs no Placement work")
      ( PeerInput.peerInputPlacementState afterStale
          == PeerInput.peerInputPlacementState predecessor
      )
    assertBool
      (show role <> " stale reference retains its causal occurrence")
      ( GraphProgress.lookupAppliedStructuralOccurrence
          staleStructuralOccurrence
          (PeerInput.peerInputStructuralProgressState afterStale)
          /= Nothing
      )
    assertBool
      (show role <> " stale reference performs no Controlled work")
      ( PeerInput.peerInputControlledState afterStale
          == PeerInput.peerInputControlledState predecessor
      )
    assertBool
      (show role <> " stale reference allocates no identity")
      ( PeerInput.peerInputIdGeneratorState afterStale
          == PeerInput.peerInputIdGeneratorState predecessor
      )
    assertBool
      (show role <> " stale reference does not change the Registry")
      ( PeerInput.peerInputSortRegistryState afterStale
          == PeerInput.peerInputSortRegistryState predecessor
      )

    (successorIdentifier, successorItem) <-
      structuralReferenceItem
        fixture
        root
        role
        carrierRole
        (publicationOrdinal + 1)
        resolveIndex
        successorSequence
        successorStructuralOccurrence
        ( GraphProgress.structuralAppliedVector
            (PeerInput.peerInputStructuralProgressState afterStale)
        )
        value
    (successor, successorResult) <-
      checked
        (show role <> " Resolve-qualified structural reference")
        ( PeerInput.applyPeerPublication
            context
            fixture.binding
            successorItem
            afterStale
        )
    assertEqual
      (show role <> " Resolve-qualified structural reference completes")
      (PeerInput.PeerInputReceived ReceiveContiguous)
      (PeerInput.peerInputDisposition successorResult)
    successorRecord <- requireIncomingById successorIdentifier successor
    assertEqual
      (show role <> " Resolve-qualified structural reference applies")
      Publication.Applied
      (Publication.incomingPublicationDisposition successorRecord)
    assertEqual
      (show role <> " successor carrier retains the Resolve prerequisite")
      resolveIndex
      ( publicationBatchControlPrerequisite
          (Publication.incomingPublicationBatch successorRecord)
      )
    assertEqual
      (show role <> " retains its immutable predefined carrier occurrence")
      (appliedRootOccurrenceId root)
      ( publicationBatchOccurrenceId
          (Publication.incomingPublicationBatch successorRecord)
      )
    let reconciliation =
          GraphProgress.structuralProgressReconciliation
            (PeerInput.peerInputStructuralProgressState successor)
    appliedSort <- case role of
      NablaRole -> do
        projected <-
          checked
            "Resolve-qualified Nabla historical projection"
            ( Reconciliation.structuralNablaProjectionAt
                (nablaIdFromGlobalObjectId object)
                (GraphProgress.structuralAppliedVector (PeerInput.peerInputStructuralProgressState successor))
                (GraphProgress.structuralAppliedControlPrefix (PeerInput.peerInputStructuralProgressState successor))
                reconciliation
            )
        projection <-
          maybe
            (assertFailure "Resolve-qualified Nabla projection missing")
            pure
            projected
        pure (Reconciliation.historicalNablaProjectionSort projection)
      DeltaRole -> do
        coordinate <-
          maybe
            (assertFailure "Resolve-qualified Delta coordinate missing")
            pure
            ( find
                ((== object) . Reconciliation.appliedDynamicDeltaObject)
                (Reconciliation.structuralAppliedDynamicDeltaCoordinates reconciliation)
            )
        pure (Reconciliation.appliedDynamicDeltaSort coordinate)
      _ -> assertFailure "retired structural reference test requires Nabla or Delta"
    assertEqual
      (show role <> " binds the current Resolve-derived occurrence")
      successorOccurrence
      (sortOccurrenceDefinition appliedSort)
    pure (successor, nextStreamSequence successorSequence)

structuralReferenceValue ::
  PeerFixture ->
  PredefinedSortRole ->
  GlobalObjectId ->
  SortId ->
  IO Value
structuralReferenceValue fixture role object referencedSort = do
  objectField <- checked "retired-reference object field" (mkFieldName "object_id")
  labelField <- checked "retired-reference label field" (mkFieldName "label")
  sortField <- checked "retired-reference sort field" (mkFieldName "sort_id")
  sequencingField <-
    checked
      "retired-reference sequencing field"
      (mkFieldName "sequencing_object")
  let common =
        [ ( objectField,
            globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)
          ),
          ( labelField,
            labelValue
              ( case role of
                  DeltaRole -> (ProcessLabel fixture.sourceProcess, 0)
                  _ -> (VoidLabel, 0)
              )
          ),
          (sortField, bytesValue (sortIdBytes referencedSort))
        ]
      fields = case role of
        NablaRole ->
          (sequencingField, optionalGlobalUniqueIdValue Nothing) : common
        _ -> common
  checked "retired structural reference value" (recordValue fields)

structuralReferenceItem ::
  PeerFixture ->
  AppliedRoot ->
  PredefinedSortRole ->
  StructuralCarrierRole ->
  Word64 ->
  ControlIndex ->
  StreamSequence ->
  StructuralOccurrenceId ->
  StructuralVersionVector ->
  Value ->
  IO (PublicationId, SequencedItem PeerPublication)
structuralReferenceItem
  fixture
  root
  role
  carrierRole
  publicationOrdinal
  controlPrerequisite
  streamSequence
  structuralOccurrence
  predecessor
  value = do
    let sourceNabla = case appliedRootRole root of
          WriterRoot writer _ -> writer
          _ -> error "requireWriterRoot returned a reader"
        identifier =
          publicationId
            sourceNabla
            (appliedRootAuthority root)
            fixture.remoteEpoch
            (nablaSequence publicationOrdinal)
        descriptor = predefinedCatalogueDescriptor (profileEntryFor role)
        destination =
          publicationDestination
            ( deriveSystemViewDeltaId
                (checkedSystemId fixture.genesis)
                fixture.localEpoch
                role
            )
            ( deriveSystemViewStoreIncarnationId
                (checkedSystemId fixture.genesis)
                fixture.localEpoch
                role
            )
            Normal
    publication <-
      checked
        "retired structural reference publication"
        (mkCheckedPublication descriptor identifier value)
    batch <-
      checked
        "retired structural reference batch"
        ( mkPublicationBatch
            identifier
            fixture.sourceProcess
            (appliedRootSortId root)
            (appliedRootOccurrenceId root)
            (checkedPublicationCanonicalValue publication)
            Normal
            fixture.topologyCut
            controlPrerequisite
            (destination :| [])
        )
    digest <-
      checked
        "retired structural reference digest"
        ( structuralPublicationDigestFor
            descriptor
            (appliedRootOccurrenceId root)
            publication
            batch
        )
    stamp <-
      checked
        "retired structural reference stamp"
        ( mkStructuralOccurrenceStamp
            structuralOccurrence
            predecessor
            identifier
            digest
            carrierRole
        )
    peerPublication <-
      checked
        "retired structural peer publication"
        ( mkStructuralPeerPublication
            descriptor
            (appliedRootOccurrenceId root)
            publication
            stamp
            batch
        )
    pure
      ( identifier,
        sequencedItem
          fixture.direction
          streamSequence
          (peerPublicationDigest peerPublication)
          peerPublication
      )

caseRepeatedSuccessorWait :: Assertion
caseRepeatedSuccessorWait = do
  let members = checkedActiveHeralds fixtureCheckedGenesis <> [fixtureThirdMember, extraMember 0xc1 0xc2, extraMember 0xc3 0xc4]
      deployment =
        fixtureDeploymentManifest
          { deploymentActiveHeralds = members,
            deploymentOracleGenesis = (deploymentOracleGenesis fixtureDeploymentManifest) {oracleGenesisActiveHeralds = members}
          }
  genesis <- checked "five-member structural peer fixture" (checkHeraldGenesis deployment)
  base <- peerFixtureFor genesis (firstLocalBootstrapId : [fixtureRemoteBootstrapId]) fixtureRemoteMember
  active <- activeDeltaFixtureFrom base
  oracle0 <- checked "five-member peer Oracle" (initialOracle (checkedOracleGenesisFor genesis base.bootstraps))
  (oracle1, projection1) <- retireTarget base (heraldMemberEpoch (extraMember 0xc1 0xc2)) (oracle0, base.oracle)
  discovery1 <- advanceDiscovery projection1 base.discovery
  control1 <- advancePeerInputStructuralControl (Oracle.oracleGreatestControlIndex oracle1) active.state
  let context1 = PeerInput.peerInputContext genesis projection1 discovery1 base.placement
      generation1 = OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView projection1)
      original = sequencedItemPayload active.item
      originalBatch = peerPublicationBatch original
      descriptor = predefinedCatalogueDescriptor (profileEntryFor DeltaRole)
      futureControl = Oracle.oracleGreatestControlIndex oracle1
  value <- checked "decode ahead-stamped carrier" (decodeCanonicalValue (canonicalValueByteString (publicationBatchCanonicalValue originalBatch)))
  publication <- checked "checked ahead-stamped carrier" (mkCheckedPublication descriptor active.identifier value)
  futureBatch <- checked "sender observes retirement before receiver watch" (mkPublicationBatch active.identifier (publicationBatchSourceProcess originalBatch) (publicationBatchSortId originalBatch) (publicationBatchOccurrenceId originalBatch) (checkedPublicationCanonicalValue publication) (publicationBatchSourceStrength originalBatch) (publicationBatchSourceTopologyPrerequisite originalBatch) futureControl (publicationBatchDestinations originalBatch))
  digest <- checked "ahead-stamped canonical digest" (structuralPublicationDigestFor descriptor (publicationBatchOccurrenceId originalBatch) publication futureBatch)
  futureStamp <- checked "sender's genuine G1 structural coordinate" (mkStructuralOccurrenceStamp (structuralOccurrenceId base.remoteEpoch firstStructuralSequence) (emptyStructuralVersionVector (Oracle.oracleCurrentMembership oracle1)) active.identifier digest DeltaCarrier)
  futurePublication <- checked "future stamped peer publication" (mkStructuralPeerPublication descriptor (publicationBatchOccurrenceId originalBatch) publication futureStamp futureBatch)
  let futureItem = sequencedItem (sequencedItemDirection active.item) firstStreamSequence (peerPublicationDigest futurePublication) futurePublication
      generation0 = OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView base.oracle)
  (aheadHeld, _) <- checked "G1 stamp reaches receiver still at Oracle G0" (PeerInput.applyPeerPublication base.context active.binding futureItem active.state)
  aheadBefore <- requireIncomingById active.identifier aheadHeld
  assertEqual "the future stamp initially waits on the sender's control prerequisite" (Set.singleton (Publication.ControlIndexDependency futureControl)) (Publication.incomingPublicationDependencies aheadBefore)
  assertEqual "early ingress retains original G0 admission" generation0 (Publication.incomingPublicationMembershipGenerationId aheadBefore)
  aheadReady <- advancePeerInputStructuralControl futureControl aheadHeld
  (aheadBaseHeld, _) <- checked "Oracle catches sender generation before Graph base" (PeerInput.releasePeerControl context1 aheadReady)
  aheadAfter <- requireIncomingById active.identifier aheadBaseHeld
  assertEqual "checked G1 stamp requires its successor base despite earlier admission" (Set.singleton (Publication.SuccessorStructuralBaseDependency generation1)) (Publication.incomingPublicationDependencies aheadAfter)
  assertEqual "watch catch-up cannot restamp admission" generation0 (Publication.incomingPublicationMembershipGenerationId aheadAfter)
  assertEqual "watch catch-up preserves the exact checked structural transcript" futurePublication (Publication.incomingPeerPublication aheadAfter)
  (held, _) <- checked "admit old stamp under current G1 while Graph remains G0" (PeerInput.applyPeerPublication context1 active.binding active.item control1)
  before <- requireIncomingById active.identifier held
  assertEqual "the first wait names G1" (Set.singleton (Publication.SuccessorStructuralBaseDependency generation1)) (Publication.incomingPublicationDependencies before)
  assertEqual "admission permanently records G1" generation1 (Publication.incomingPublicationMembershipGenerationId before)
  (oracle2, projection2) <- retireTarget base (heraldMemberEpoch (extraMember 0xc3 0xc4)) (oracle1, projection1)
  discovery2 <- advanceDiscovery projection2 discovery1
  control2 <- advancePeerInputStructuralControl (Oracle.oracleGreatestControlIndex oracle2) held
  let context2 = PeerInput.peerInputContext genesis projection2 discovery2 base.placement
      generation2 = OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView projection2)
  aheadSecondControl <- advancePeerInputStructuralControl (Oracle.oracleGreatestControlIndex oracle2) aheadBaseHeld
  (aheadRetargeted, _) <- checked "later contraction retargets an ahead-stamped wait" (PeerInput.releasePeerControl context2 aheadSecondControl)
  aheadFinal <- requireIncomingById active.identifier aheadRetargeted
  assertEqual "ahead-stamped wait now names exact G2" (Set.singleton (Publication.SuccessorStructuralBaseDependency generation2)) (Publication.incomingPublicationDependencies aheadFinal)
  assertEqual "two retirements preserve the original pre-watch admission" generation0 (Publication.incomingPublicationMembershipGenerationId aheadFinal)
  assertEqual "two retirements preserve the sender's original G1 stamp" futurePublication (Publication.incomingPeerPublication aheadFinal)
  (retargeted, result) <- checked "advance retained structural wait through second contraction" (PeerInput.releasePeerControl context2 control2)
  after <- requireIncomingById active.identifier retargeted
  assertEqual "the execution wait now names exact current G2" (Set.singleton (Publication.SuccessorStructuralBaseDependency generation2)) (Publication.incomingPublicationDependencies after)
  assertEqual "membership admission is not restamped" generation1 (Publication.incomingPublicationMembershipGenerationId after)
  assertEqual "the structural payload and its origin stamp are unchanged" (Publication.incomingPeerPublication before) (Publication.incomingPeerPublication after)
  assertEqual "physical assignments remain exact" (Publication.incomingPublicationAssignments before) (Publication.incomingPublicationAssignments after)
  assertEqual "no destination is fabricated by retargeting" (Publication.incomingPublicationDestinationOutcomes before) (Publication.incomingPublicationDestinationOutcomes after)
  assertEqual "retargeting cannot release publication before Graph's base" [] (PeerInput.peerControlReleasedPublications result)
  (replayed, _) <- checked "repeat exact control redrive" (PeerInput.releasePeerControl context2 retargeted)
  assertBool "repeat redrive preserves the same owner state" (PeerInput.peerInputPublicationState replayed == PeerInput.peerInputPublicationState retargeted)
  let wrongLineage =
        OracleProjection.oracleViewHeraldMembershipLineage
          (OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView base.oracle))
          generation2
          (OracleProjection.oracleView projection2)
  lineage <- maybe (assertFailure "missing complete checked lineage") pure wrongLineage
  assertBool "a checked lineage with the wrong old wait coordinate cannot retarget" (case Publication.prepareIncomingSuccessorBaseRetarget lineage active.identifier (PeerInput.peerInputPublicationState retargeted) of Left _ -> True; Right _ -> False)
  where
    extraMember identifier epoch =
      HeraldMember
        (checkedEither "additional Herald ID" (mkHeraldId (fixtureIdentifierBytes identifier)))
        (checkedEither "additional Herald epoch" (mkHeraldEpoch (fixtureIdentifierBytes epoch)))
    advanceDiscovery projection discovery = do
      prepared <- checked "contract discovery alongside Oracle" (DiscoveryState.prepareMembershipAdvance (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView projection)) discovery)
      pure (fst (DiscoveryState.commitMembershipAdvance prepared))
    retireTarget base target initial = do
      let generation = Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership (fst initial))
      (opened, receipt) <- submit base.localEpoch (Oracle.openHeraldFailureProbeCommand target generation (Voter.voterConfigurationId (Voter.oracleVoterConfiguration (fst initial)))) initial
      probe <- case Oracle.oracleReceiptFailureResult receipt of
        Just (Oracle.FailureProbeOpened value) -> pure value
        other -> assertFailure ("missing accepted failure Open: " <> show other)
      reported <- foldM (\pair reporter -> fst <$> submit reporter (Oracle.reportHeraldFailureProbeCommand probe (Voter.voterConfigurationId (Voter.oracleVoterConfiguration (fst opened))) Oracle.ProbeUnreachable) pair) opened [base.localEpoch, base.remoteEpoch]
      let resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget
      fst <$> submit base.localEpoch (Oracle.retireHeraldEpochCommand resolution target) reported
    submit home command (oracle, projection) = do
      let ordinal = 30_000 + controlIndexWord64 (Oracle.oracleGreatestControlIndex oracle)
          (successor, entry) = committedOracleEntry oracle (oracleEnvelope (oracleClientRequestId home ordinal) Nothing home command)
      appliedCommand <- maybe (assertFailure "fixture command must produce a command entry") pure (Oracle.appliedEntryCommand entry)
      let receipt = Oracle.appliedEntryReceipt appliedCommand
      assertEqual "real Oracle authority admits the fixture command" Oracle.OracleAccepted (Oracle.oracleReceiptResult receipt)
      prepared <- checked "consume exact canonical Oracle entry" (OracleProjection.prepareAppliedEntry (canonicalizeAppliedOracleEntry entry) projection)
      pure ((successor, OracleProjection.commitAppliedEntry prepared), receipt)

caseRetiredSourceDependencyRelease :: Assertion
caseRetiredSourceDependencyRelease = do
  initialFixture <- peerFixture
  let prefix =
        projectedAfterEnvelope
          initialFixture.bootstraps
          ( oracleEnvelope
              (oracleClientRequestId initialFixture.localEpoch 1)
              Nothing
              initialFixture.localEpoch
              (Voter.cancelVoterChangeCommand (checkedEither "unknown voter change" (Voter.mkVoterChangeId (controlIndex 999999))))
          )
      fixture = initialFixture {oracle = prefix}
      retirementIndex = controlIndex 2
      dependency =
        Publication.EffectiveSortDependency
          fixture.sortId
          fixture.occurrence
  held <- holdPeerFixtureOnDependency fixture dependency
  controlReady <- advancePeerInputStructuralControl retirementIndex held
  let predecessorMembership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView fixture.oracle)
  successorMembership <-
    checked
      "retired-source membership successor"
      ( retireHeraldMembershipGeneration
          retirementIndex
          (fixtureRetirementResolution retirementIndex)
          fixture.remoteEpoch
          predecessorMembership
      )
  let ends =
        [ OracleProjection.membershipAdvanceProcessEnd
            process
            retirementIndex
            fixtureHeraldRetirementReason
        | process <- OracleProjection.projectedProcessEpochs fixture.oracle,
          OracleProjection.oracleViewProcessResidence process oracleView
            == Just fixture.remoteEpoch,
          OracleProjection.oracleViewProcessIsLive process oracleView
        ]
      oracleView = OracleProjection.oracleView fixture.oracle
  preparedOracle <-
    checked
      "retired-source Oracle projection"
      ( OracleProjection.prepareMembershipAdvance
          retirementIndex
          successorMembership
          ends
          fixture.oracle
      )
  preparedDiscovery <-
    checked
      "retired-source discovery projection"
      (DiscoveryState.prepareMembershipAdvance successorMembership fixture.discovery)
  preparedPeerStream <-
    checked
      "retired-source peer stream"
      ( PeerStream.preparePeerRetirement
          fixture.remoteEpoch
          (PeerInput.peerInputPeerStreamState controlReady)
      )
  let oracle = OracleProjection.commitMembershipAdvance preparedOracle
      (discovery, _) = DiscoveryState.commitMembershipAdvance preparedDiscovery
      (peerStream, _) = PeerStream.commitPeerRetirement preparedPeerStream
      controlled =
        Controlled.commitControlledProcessRetirement
          ( Controlled.prepareControlledProcessRetirement
              fixture.sourceProcess
              (PeerInput.peerInputControlledState controlReady)
          )
      retired =
        replacePeerInputPeerStreamAndControlled
          peerStream
          controlled
          controlReady
      retiredContext =
        PeerInput.peerInputContext
          fixture.genesis
          oracle
          discovery
          fixture.placement
  assertBool
    "the source is absent from current membership"
    ( not
        ( OracleProjection.oracleViewIsActiveHerald
            fixture.remoteEpoch
            (OracleProjection.oracleView oracle)
        )
    )
  assertBool
    "the source writer has no current controlled authority"
    (Controlled.controlledProcessEnded fixture.sourceProcess controlled)
  assertBool
    "the source peer is terminally retired"
    (PeerStream.peerStreamPeerRetired fixture.remoteEpoch peerStream)
  (released, result) <-
    checked
      "release retained publication after source retirement"
      (PeerInput.releasePeerDependency retiredContext dependency retired)
  record <- requireIncomingById (publicationBatchId fixture.batch) released
  assertEqual
    "the publication retains its predecessor membership authority"
    (OracleProjection.oracleViewCurrentHeraldMembershipId oracleView)
    (Publication.incomingPublicationMembershipGenerationId record)
  assertEqual
    "retained predecessor-authorized work completes"
    Publication.Applied
    (Publication.incomingPublicationDisposition record)
  assertEqual
    "the semantic publication is released once"
    [publicationBatchId fixture.batch]
    (PeerInput.peerDependencyReleasedPublications result)
  assertEqual
    "no acknowledgement or resume is emitted toward the retired source"
    []
    (PeerInput.peerDependencyAcknowledgements result)
  retained <-
    checked
      "retired-source incoming stream"
      ( PeerStream.incomingRetainedItems
          fixture.direction
          (PeerInput.peerInputPeerStreamState released)
      )
  assertEqual
    "local incoming stream state still completes"
    []
    (fmap snd retained)
  (afterQueuedStale, staleResult) <-
    checked
      "queued retired-source batch through the composed ingress"
      ( PeerInput.applyPeerPublication
          retiredContext
          fixture.binding
          fixture.item
          released
      )
  assertBool
    "a queued batch from the retired binding is state-identical"
    (afterQueuedStale == released)
  assertEqual
    "the composed boundary classifies the queued batch as stale"
    PeerInput.PeerInputStaleBinding
    (PeerInput.peerInputDisposition staleResult)
  assertEqual
    "the queued stale batch emits no acknowledgement"
    []
    (PeerInput.peerInputAcknowledgements staleResult)

caseControlledRelease :: Assertion
caseControlledRelease = do
  fixture <- controlledFixture
  (held, _) <- applyControlledFixture fixture fixture.initialContext fixture.initialState
  heldRecord <- requireIncomingById fixture.identifier held
  assertEqual
    "early controlled bytes wait on their exact prefix"
    (Set.singleton (Publication.ControlIndexDependency (controlIndex 1)))
    (Publication.incomingPublicationDependencies heldRecord)
  assertBool
    "the early object is absent from Graph"
    (not (Graph.graphHasVertex (NeutralVertex fixture.object) (PeerInput.peerInputGraphState held)))
  assertEqual
    "dependency-held bytes establish no controlled observation"
    Nothing
    ( Controlled.controlledLocalRecord
        fixture.object
        (PeerInput.peerInputControlledState held)
    )
  controlReady <- advancePeerInputStructuralControl (controlIndex 1) held
  (released, releaseResult) <-
    checked
      "release matching controlled publication"
      (PeerInput.releasePeerControl fixture.acceptedContext controlReady)
  releasedRecord <- requireIncomingById fixture.identifier released
  assertEqual "matching controlled work applies" Publication.Applied (Publication.incomingPublicationDisposition releasedRecord)
  assertEqual
    "the exact semantic publication releases once"
    [fixture.identifier]
    (PeerInput.peerControlReleasedPublications releaseResult)
  assertBool
    "an applied neutral object is reconciled into Graph"
    (Graph.graphHasVertex (NeutralVertex fixture.object) (PeerInput.peerInputGraphState released))
  controlledRecord <-
    maybe
      (assertFailure "missing peer-established controlled record")
      pure
      ( Controlled.controlledLocalRecord
          fixture.object
          (PeerInput.peerInputControlledState released)
      )
  assertEqual
    "the peer observation establishes current local lifecycle knowledge"
    Controlled.ControlledCurrent
    (Controlled.controlledRecordLifecycle controlledRecord)
  slot <- requireControlledDestinationSlot fixture released
  assertEqual
    "Store records the exact applied publication"
    (Just Normal)
    (lookup fixture.identifier (Store.storeSlotApplicationReceipts slot))
  acknowledgement <- case PeerInput.peerControlAcknowledgements releaseResult of
    [value] -> pure value
    values -> assertFailure ("expected one release acknowledgement, got " <> show values)
  assertEqual
    "completion follows semantic release"
    (Just firstStreamSequence)
    (streamPrefixSequence (PeerInput.peerAcknowledgementCompletedPrefix acknowledgement))
  (duplicate, duplicateResult) <-
    checked
      "repeat exact control release"
      (PeerInput.releasePeerControl fixture.acceptedContext released)
  assertBool "repeated release is state-idempotent" (duplicate == released)
  assertEqual "repeated release has no semantic work" [] (PeerInput.peerControlReleasedPublications duplicateResult)
  assertEqual "repeated release has no acknowledgement" [] (PeerInput.peerControlAcknowledgements duplicateResult)

caseControlledRejection :: Assertion
caseControlledRejection = do
  fixture <- controlledFixture
  (held, _) <- applyControlledFixture fixture fixture.initialContext fixture.initialState
  controlReady <- advancePeerInputStructuralControl (controlIndex 1) held
  (ignored, releaseResult) <-
    checked
      "release rejected controlled publication"
      (PeerInput.releasePeerControl fixture.rejectedContext controlReady)
  record <- requireIncomingById fixture.identifier ignored
  assertEqual
    "a rejected unrelated Oracle operation cannot reject target-local data"
    Publication.Applied
    (Publication.incomingPublicationDisposition record)
  slot <- requireControlledDestinationSlot fixture ignored
  assertEqual
    "Store records the locally checked publication"
    (Just Normal)
    (lookup fixture.identifier (Store.storeSlotApplicationReceipts slot))
  assertBool
    "Graph records the current neutral object"
    (Graph.graphHasVertex (NeutralVertex fixture.object) (PeerInput.peerInputGraphState ignored))
  assertEqual
    "the advanced generic prerequisite releases the semantic publication"
    [fixture.identifier]
    (PeerInput.peerControlReleasedPublications releaseResult)
  acknowledgement <- case PeerInput.peerControlAcknowledgements releaseResult of
    [value] -> pure value
    values -> assertFailure ("expected one release acknowledgement, got " <> show values)
  assertEqual
    "local application advances completion"
    (Just firstStreamSequence)
    (streamPrefixSequence (PeerInput.peerAcknowledgementCompletedPrefix acknowledgement))

caseControlledDeliveryConvergence :: Assertion
caseControlledDeliveryConvergence = do
  fixture <- controlledFixture

  (held, _) <- applyControlledFixture fixture fixture.initialContext fixture.initialState
  controlReadyHeld <- advancePeerInputStructuralControl (controlIndex 1) held
  (dataThenControl, dataFirstRelease) <-
    checked
      "release data-first controlled publication"
      (PeerInput.releasePeerControl fixture.acceptedContext controlReadyHeld)

  controlReady <-
    advancePeerInputStructuralControl (controlIndex 1) fixture.initialState
  (controlOnly, controlOnlyResult) <-
    checked
      "apply control before publication"
      (PeerInput.releasePeerControl fixture.acceptedContext controlReady)
  assertBool "control release changes no further peer-input owner" (controlOnly == controlReady)
  assertEqual "control alone releases no publication" [] (PeerInput.peerControlReleasedPublications controlOnlyResult)
  assertEqual "control alone emits no acknowledgement" [] (PeerInput.peerControlAcknowledgements controlOnlyResult)
  assertEqual "control alone emits no wake" [] (PeerInput.peerControlWakes controlOnlyResult)

  (controlThenData, controlFirstResult) <-
    applyControlledFixture fixture fixture.acceptedContext controlOnly
  let direction = sequencedItemDirection fixture.item
      controlFirstStream = PeerInput.peerInputPeerStreamState controlThenData
      dataFirstStream = PeerInput.peerInputPeerStreamState dataThenControl
      remainingFamilies = [PeerStream.RouteCutoverMarkers]
      drainedDataFirst = foldl' (\stream family -> PeerStream.consumeIncomingMarkerDirection family direction stream) dataFirstStream remainingFamilies
  -- Immediate admission never exposes a pending head. Data-first admission
  -- does, so its completion leaves one wake for each later marker family.
  forM_ remainingFamilies $ \family -> do
    assertEqual "immediately completed data never queues a marker head" Set.empty (PeerStream.pendingIncomingMarkerDirections family controlFirstStream)
    assertEqual "completing previously pending data wakes exactly its marker direction" (Set.singleton direction) (PeerStream.pendingIncomingMarkerDirections family dataFirstStream)
  forM_ [controlFirstStream, dataFirstStream] $ \stream ->
    assertEqual "both ingress paths already drain disappearance heads" Set.empty (PeerStream.pendingIncomingMarkerDirections PeerStream.DisappearanceProbeMarkers stream)
  assertBool
    "control-first and data-first delivery differ only by the route-marker completion wake"
    ( controlThenData
        == replacePeerInputPeerStreamAndControlled drainedDataFirst (PeerInput.peerInputControlledState dataThenControl) dataThenControl
    )
  forM_ [controlThenData, dataThenControl] assertMarkerSchedulingValid
  assertEqual
    "both orders settle the same publication"
    (PeerInput.peerControlReleasedPublications dataFirstRelease)
    (PeerInput.peerInputPublicationIds controlFirstResult)
  assertEqual
    "both orders prepare the same cumulative acknowledgement"
    (PeerInput.peerControlAcknowledgements dataFirstRelease)
    (PeerInput.peerInputAcknowledgements controlFirstResult)
  assertEqual
    "both orders prepare the same application wakes"
    (PeerInput.peerControlWakes dataFirstRelease)
    (PeerInput.peerInputWakes controlFirstResult)

  (afterPublicationReplay, publicationReplayResult) <-
    applyControlledFixture fixture fixture.acceptedContext controlThenData
  assertBool "equal publication replay changes no owner" (afterPublicationReplay == controlThenData)
  assertEqual
    "equal replay is classified as a stream duplicate"
    (PeerInput.PeerInputReceived ReceiveDuplicate)
    (PeerInput.peerInputDisposition publicationReplayResult)
  assertEqual
    "equal replay reoffers the same cumulative acknowledgement"
    (PeerInput.peerInputAcknowledgements controlFirstResult)
    (PeerInput.peerInputAcknowledgements publicationReplayResult)

  (afterControlReplay, controlReplayResult) <-
    checked
      "replay control after completed publication"
      (PeerInput.releasePeerControl fixture.acceptedContext controlThenData)
  assertBool "equal control replay changes no owner" (afterControlReplay == controlThenData)
  assertEqual "equal control replay releases nothing" [] (PeerInput.peerControlReleasedPublications controlReplayResult)
  assertEqual "equal control replay emits no acknowledgement" [] (PeerInput.peerControlAcknowledgements controlReplayResult)

caseDualDependencyReleaseOrder :: Assertion
caseDualDependencyReleaseOrder = do
  fixture <- controlledFixture
  held <- dualHeldControlledState fixture
  controlReadyHeld <- advancePeerInputStructuralControl (controlIndex 1) held
  let sortDependency =
        Publication.EffectiveSortDependency
          (publicationBatchSortId fixture.batch)
          (publicationBatchOccurrenceId fixture.batch)

  (afterControl, _) <-
    checked
      "release control before sort"
      (PeerInput.releasePeerControl fixture.acceptedContext controlReadyHeld)
  controlFirstRecord <- requireIncomingById fixture.identifier afterControl
  assertEqual
    "control-first revalidates the already-effective sort"
    Publication.Applied
    (Publication.incomingPublicationDisposition controlFirstRecord)
  (controlThenSort, redundantSortRelease) <-
    checked
      "repeat the now-redundant sort release after control"
      (PeerInput.releasePeerDependency fixture.acceptedContext sortDependency afterControl)
  assertBool
    "the redundant sort release changes no owner"
    (controlThenSort == afterControl)
  assertEqual
    "the redundant sort release performs no semantic work"
    []
    (PeerInput.peerDependencyReleasedPublications redundantSortRelease)

  (afterSort, _) <-
    checked
      "release sort before control"
      (PeerInput.releasePeerDependency fixture.acceptedContext sortDependency held)
  sortFirstRecord <- requireIncomingById fixture.identifier afterSort
  assertEqual
    "sort-first retains only the control dependency"
    (Set.singleton (Publication.ControlIndexDependency (controlIndex 1)))
    (Publication.incomingPublicationDependencies sortFirstRecord)
  controlReadyAfterSort <-
    advancePeerInputStructuralControl (controlIndex 1) afterSort
  (sortThenControl, _) <-
    checked
      "release control after sort"
      (PeerInput.releasePeerControl fixture.acceptedContext controlReadyAfterSort)

  assertBool
    "both dependency orders install the identical atomic successor"
    (controlThenSort == sortThenControl)
  finalRecord <- requireIncomingById fixture.identifier controlThenSort
  assertEqual "both orders apply the publication" Publication.Applied (Publication.incomingPublicationDisposition finalRecord)
  assertBool
    "both orders reconcile the neutral object"
    ( Graph.graphHasVertex
        (NeutralVertex fixture.object)
        (PeerInput.peerInputGraphState controlThenSort)
    )

caseDefinitionTriggeredDeferredRejection :: Assertion
caseDefinitionTriggeredDeferredRejection = do
  let bootstrapIds = [firstLocalBootstrapId, fixtureRemoteBootstrapId]
      genesis = causalReceiverGenesis
      descriptor = labelControlledPeerDescriptorWithRetention 7
      sortId = descriptorSortId descriptor
      expectedOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId genesis)
          sortId
          Genesis
  bootstraps <-
    checked
      "deferred-rejection causal bootstraps"
      (checkInitialBootstraps genesis (PrimordialProcessManifest bootstrapIds))
  offenderFixture <- peerFixtureFor genesis bootstrapIds fixtureLocalMember
  triggerFixture <- peerFixtureFor genesis bootstrapIds fixtureRemoteMember
  (initial, initialEffects) <-
    checked
      "initialize deferred-rejection receiver"
      ( initialHerald
          (monotonicInstant 0)
          genesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  oracleAttempt <- case [ attempt
                        | RunOracleClientAction (ConnectAndHelloOracle attempt _) <-
                            effectBatchMembers initialEffects
                        ] of
    [attempt] -> pure attempt
    actual ->
      assertFailure
        ("expected one initial Oracle connection attempt, got " <> show actual)
  let oracleNode = oracleContactNode (oracleConnectAttemptContact oracleAttempt)
      oracleAcceptance =
        oracleHelloAcceptance
          oracleNode
          (oracleObservedTerm 1)
          (oracleConnectAttemptFromExclusive oracleAttempt)
          (Just oracleNode)
          True
  (oracleBound, oracleBindEffects) <-
    checked
      "bind Oracle before deferred-rejection ingress"
      ( verifiedStepHerald
          ( HeraldInput.heraldInput
              (monotonicInstant 1)
              (HeraldInput.OracleInput (OracleHelloReceived oracleAttempt oracleAcceptance))
          )
          initial
      )
  oracleBinding <- case [ binding
                        | RunOracleClientAction (BindOracleConnection observed binding) <-
                            effectBatchMembers oracleBindEffects,
                          observed == oracleAttempt
                        ] of
    [binding] -> pure binding
    actual ->
      assertFailure
        ("expected one established Oracle binding, got " <> show actual)
  let acceptPeer description observed nonce member predecessor = do
        let epoch = heraldMemberEpoch member
            candidate =
              Discovery.peerCandidate
                (heraldMemberId member)
                epoch
                (Discovery.connectionNonce nonce)
            hello =
              Discovery.peerHello
                (checkedSystemId genesis)
                (heraldMemberId member)
                epoch
                (Discovery.connectionNonce nonce)
                Set.empty
                (controlIndex 0)
                (checkedCatalogueDigest genesis)
                (checkedInitialProjectionDigest bootstraps)
                Nothing
        (successor, effects) <-
          checked
            description
            ( verifiedStepHerald
                ( HeraldInput.heraldInput
                    (monotonicInstant observed)
                    ( HeraldInput.PeerInput
                        (currentPeerHelloReceived predecessor candidate Set.empty hello)
                    )
                )
                predecessor
            )
        binding <- case [ accepted
                        | SetPeerCandidateDisposition
                            _
                            (Discovery.PeerHelloAccepted _ accepted) <-
                            effectBatchMembers effects
                        ] of
          [accepted] -> pure accepted
          actual ->
            assertFailure
              (description <> ": expected one accepted binding, got " <> show actual)
        pure (successor, binding)
  (withOffender, offenderBinding) <-
    acceptPeer
      "accept deferred-rejection offender"
      2
      71
      fixtureLocalMember
      oracleBound
  (connected, triggerBinding) <-
    acceptPeer
      "accept deferred-rejection trigger"
      3
      72
      fixtureRemoteMember
      withOffender
  wrongOccurrence <-
    checked
      "deferred-rejection nonhistorical occurrence"
      (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xee))
  assertBool
    "the retained claim deliberately names a nonhistorical occurrence"
    (wrongOccurrence /= expectedOccurrence)
  (offenderIdentifier, _, offenderItem) <-
    labelControlledOrdinaryPeerItem
      offenderFixture
      descriptor
      offenderFixture.sourceNabla
      offenderFixture.sourceAuthority
      sortId
      wrongOccurrence
      (controlIndex 1)
      offenderFixture.topologyCut
      offenderFixture.destination
      90
      firstStreamSequence
  (held, heldEffects) <-
    checked
      "retain unknown-sort claim from offender"
      ( verifiedStepHerald
          ( HeraldInput.heraldInput
              (monotonicInstant 4)
              ( HeraldInput.PeerInput
                  ( HeraldInput.peerPublicationReceived
                      offenderBinding
                      (peerPublicationItem offenderItem)
                  )
              )
          )
          connected
      )
  heldRecord <-
    maybe
      ( assertFailure
          ( "missing retained offender publication; effects: "
              <> show (effectBatchMembers heldEffects)
          )
      )
      pure
      (Publication.lookupIncomingPublication offenderIdentifier (startupPublicationState held))
  assertEqual
    "the unknown sort retains the offender publication"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition heldRecord)
  let bufferedSequence =
        nextStreamSequence (nextStreamSequence firstStreamSequence)
      bufferedDuplicateItem =
        sequencedItem
          (sequencedItemDirection offenderItem)
          bufferedSequence
          (sequencedItemDigest offenderItem)
          (sequencedItemPayload offenderItem)
  (buffered, _) <-
    checked
      "buffer a later semantic duplicate behind a stream gap"
      ( verifiedStepHerald
          ( HeraldInput.heraldInput
              (monotonicInstant 5)
              ( HeraldInput.PeerInput
                  ( HeraldInput.peerPublicationReceived
                      offenderBinding
                      (peerPublicationItem bufferedDuplicateItem)
                  )
              )
          )
          held
      )
  bufferedRecord <-
    maybe
      (assertFailure "the buffered duplicate lost its semantic incumbent")
      pure
      (Publication.lookupIncomingPublication offenderIdentifier (startupPublicationState buffered))
  assertEqual
    "a gap-buffered semantic duplicate is not yet an owner assignment"
    1
    (Set.size (Publication.incomingPublicationAssignments bufferedRecord))
  (_, _, bufferedInvalidTriggerItem) <-
    labelControlledOrdinaryPeerItem
      triggerFixture
      descriptor
      triggerFixture.sourceNabla
      triggerFixture.sourceAuthority
      sortId
      wrongOccurrence
      (controlIndex 1)
      triggerFixture.topologyCut
      triggerFixture.destination
      93
      bufferedSequence
  (withBufferedInvalidTrigger, bufferedInvalidTriggerEffects) <-
    checked
      "buffer a first-seen invalid occurrence behind the trigger stream gap"
      ( verifiedStepHerald
          ( HeraldInput.heraldInput
              (monotonicInstant 6)
              ( HeraldInput.PeerInput
                  ( HeraldInput.peerPublicationReceived
                      triggerBinding
                      (peerPublicationItem bufferedInvalidTriggerItem)
                  )
              )
          )
          buffered
      )
  bufferedTriggerItems <-
    checked
      "inspect the undecidable trigger stream"
      ( PeerStream.incomingRetainedItems
          triggerFixture.direction
          (startupPeerStreamState withBufferedInvalidTrigger)
      )
  assertEqual
    ( "the undecidable trigger item remains gap-buffered; effects: "
        <> show (effectBatchMembers bufferedInvalidTriggerEffects)
    )
    [PeerStream.GapRetained]
    (fmap snd bufferedTriggerItems)
  startBootstrap <-
    maybe
      (assertFailure "missing configured process for the control advance")
      pure
      (lookupConfiguredProcessBootstrap secondLocalBootstrapId genesis)
  oracleState <-
    checked
      "initialize deferred-rejection Oracle"
      (initialOracle (checkedOracleGenesisFor genesis bootstraps))
  let (_, appliedEntry) =
        committedOracleEntry
          oracleState
          (startEnvelope offenderFixture startBootstrap Nothing)
      canonicalEntry = canonicalizeAppliedOracleEntry appliedEntry
  (controlReady, _) <-
    checked
      "advance control while the offender sort remains unknown"
      ( verifiedStepHerald
          ( HeraldInput.heraldInput
              (monotonicInstant 7)
              ( HeraldInput.OracleInput
                  (OracleEntriesReceived oracleBinding (canonicalEntry :| []))
              )
          )
          withBufferedInvalidTrigger
      )
  controlReadyRecord <-
    maybe
      (assertFailure "the control advance lost the offender publication")
      pure
      ( Publication.lookupIncomingPublication
          offenderIdentifier
          (startupPublicationState controlReady)
      )
  assertEqual
    "control readiness leaves only the unknown-sort dependency"
    (Set.singleton (Publication.EffectiveSortDependency sortId wrongOccurrence))
    (Publication.incomingPublicationDependencies controlReadyRecord)
  (triggerPublication, _, triggerItem) <-
    peerDefinitionItem
      triggerFixture
      91
      (controlIndex 0)
      firstStreamSequence
      descriptor
  (successor, effects) <-
    checked
      "apply definition which exposes the offender"
      ( verifiedStepHerald
          ( HeraldInput.heraldInput
              (monotonicInstant 8)
              ( HeraldInput.PeerInput
                  ( HeraldInput.peerPublicationReceived
                      triggerBinding
                      (peerPublicationItem triggerItem)
                  )
              )
          )
          controlReady
      )
  assertEqual
    "the deferred protocol rejection targets only the original offender"
    [(offenderBinding, ClosePeerPublicationProtocol)]
    [ (binding, disposition)
    | RejectPeerConnection binding disposition <- effectBatchMembers effects
    ]
  assertBool
    "the rejected binding receives no later direct control send"
    ( all
        ( \case
            SendPeerControl binding _ -> binding /= offenderBinding
            _ -> True
        )
        (effectBatchMembers effects)
    )
  assertBool
    "the trigger binding still receives its successful progress control"
    ( any
        ( \case
            SendPeerControl binding _ -> binding == triggerBinding
            _ -> False
        )
        (effectBatchMembers effects)
    )
  effective <-
    maybe
      (assertFailure "the triggering definition did not become effective")
      pure
      (SortRegistry.lookupEffectiveSort sortId (startupSortRegistryState successor))
  assertEqual
    "the valid triggering definition installs its canonical occurrence"
    expectedOccurrence
    (SortRegistry.registryEntryOccurrenceId effective)
  assertEqual
    "the triggering publication owns the effective definition"
    (checkedPublicationId triggerPublication)
    (SortRegistry.registryEntryPublicationId effective)
  triggerRecord <-
    maybe
      (assertFailure "missing triggering definition publication")
      pure
      ( Publication.lookupIncomingPublication
          (checkedPublicationId triggerPublication)
          (startupPublicationState successor)
      )
  assertEqual
    "the triggering definition applies"
    Publication.Applied
    (Publication.incomingPublicationDisposition triggerRecord)
  offenderRecord <-
    maybe
      (assertFailure "missing rejected offender publication")
      pure
      (Publication.lookupIncomingPublication offenderIdentifier (startupPublicationState successor))
  assertEqual
    "the original offender publication becomes transport-terminal"
    Publication.ProtocolRejected
    (Publication.incomingPublicationDisposition offenderRecord)
  gapCloserItem <-
    disappearanceMarkerItem
      (controlIndex 177)
      offenderFixture.direction
      (nextStreamSequence firstStreamSequence)
      offenderFixture.state
  let (gapCloserContext, gapCloserOwners) = peerInputSliceForHerald successor
  gapCloserCandidate <-
    checked
      "prepare the gap-closing marker stream candidate"
      ( PeerStream.prepareReceiveCandidate
          gapCloserItem
          (startupPeerStreamState successor)
      )
  assertEqual
    "the candidate exposes the new marker followed by the retained bad tail"
    [nextStreamSequence firstStreamSequence, bufferedSequence]
    ( fmap
        sequencedItemSequence
        (PeerStream.preparedContiguousItems gapCloserCandidate)
    )
  _ <-
    checked
      "discard the retained bad tail from the prepared candidate"
      ( PeerStream.discardPreviouslyRetainedContiguousAt
          bufferedSequence
          gapCloserCandidate
      )
  case PeerInput.applyPeerLogicalItem
    gapCloserContext
    offenderBinding
    gapCloserItem
    gapCloserOwners of
    Left problem ->
      assertFailure
        ("the gap-closing marker failed direct admission: " <> show problem)
    Right _ -> pure ()
  (afterGapClosure, gapClosureEffects) <-
    checked
      "close the prefix ahead of the buffered rejected duplicate"
      ( verifiedStepHerald
          ( HeraldInput.heraldInput
              (monotonicInstant 9)
              ( HeraldInput.PeerInput
                  ( HeraldInput.peerPublicationReceived
                      offenderBinding
                      (peerLogicalItem gapCloserItem)
                  )
              )
          )
          successor
      )
  rejectedAfterGapClosure <-
    maybe
      (assertFailure "gap closure lost the rejected semantic incumbent")
      pure
      (Publication.lookupIncomingPublication offenderIdentifier (startupPublicationState afterGapClosure))
  assertEqual
    "discarding the buffered duplicate does not restore a completed assignment"
    0
    (Set.size (Publication.incomingPublicationAssignments rejectedAfterGapClosure))
  retainedAfterGapClosure <-
    checked
      "inspect the reopened offender stream gap"
      ( PeerStream.incomingRetainedItems
          offenderFixture.direction
          (startupPeerStreamState afterGapClosure)
      )
  assertEqual
    "the valid pending prefix item is retained before the rejected buffered tail"
    [PeerStream.IncomingReceivedPending]
    (fmap snd retainedAfterGapClosure)
  assertBool
    "the rejected buffered sequence is discarded so a corrected retry can fill it"
    ( all
        ((/= bufferedSequence) . sequencedItemSequence . fst)
        retainedAfterGapClosure
    )
  assertEqual
    "the discarded buffered protocol failure is attributed to its source binding"
    [(offenderBinding, ClosePeerPublicationProtocol)]
    [ (binding, disposition)
    | RejectPeerConnection binding disposition <- effectBatchMembers gapClosureEffects
    ]
  triggerGapCloserItem <-
    disappearanceMarkerItem
      (controlIndex 178)
      triggerFixture.direction
      (nextStreamSequence firstStreamSequence)
      triggerFixture.state
  (afterRawGapClosure, rawGapClosureEffects) <-
    checked
      "close the prefix ahead of the first-seen invalid occurrence"
      ( verifiedStepHerald
          ( HeraldInput.heraldInput
              (monotonicInstant 10)
              ( HeraldInput.PeerInput
                  ( HeraldInput.peerPublicationReceived
                      triggerBinding
                      (peerLogicalItem triggerGapCloserItem)
                  )
              )
          )
          afterGapClosure
      )
  retainedAfterRawGapClosure <-
    checked
      "inspect the reopened trigger stream gap"
      ( PeerStream.incomingRetainedItems
          triggerFixture.direction
          (startupPeerStreamState afterRawGapClosure)
      )
  assertEqual
    "the valid trigger prefix item remains pending before the rejected buffered tail"
    [PeerStream.IncomingReceivedPending]
    (fmap snd retainedAfterRawGapClosure)
  assertBool
    "the first-seen invalid buffered sequence is discarded"
    ( all
        ((/= bufferedSequence) . sequencedItemSequence . fst)
        retainedAfterRawGapClosure
    )
  assertEqual
    "the newly decidable buffered occurrence mismatch is attributed to its source binding"
    [(triggerBinding, ClosePeerPublicationProtocol)]
    [ (binding, disposition)
    | RejectPeerConnection binding disposition <- effectBatchMembers rawGapClosureEffects
    ]
  assertBool
    "the rejected trigger binding receives no later direct control send"
    ( all
        ( \case
            SendPeerControl binding _ -> binding /= triggerBinding
            _ -> True
        )
        (effectBatchMembers rawGapClosureEffects)
    )

caseSourceTopologyHeld :: Assertion
caseSourceTopologyHeld = do
  fixture <- peerFixture
  futureCut <-
    checked
      "future source topology cut"
      (mkTopologyCutId (fixtureIdentifierBytes 0xce))
  batch <-
    checked
      "future source topology batch"
      ( mkPublicationBatch
          (checkedPublicationId fixture.publication)
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (checkedPublicationCanonicalValue fixture.publication)
          Normal
          futureCut
          fixture.controlPrerequisite
          (publicationBatchDestinations fixture.batch)
      )
  let item =
        ordinarySequencedItem
          fixture.direction
          firstStreamSequence
          batch
      dependency = Publication.SourceTopologyDependency futureCut
  (held, result) <-
    checked
      "retain future source topology publication"
      (PeerInput.applyPeerPublication fixture.context fixture.binding item fixture.state)
  record <- requireIncomingById (publicationBatchId batch) held
  assertEqual
    "the exact source cut is the sole waiter"
    (Set.singleton dependency)
    (Publication.incomingPublicationDependencies record)
  assertEqual
    "the publication is dependency-held"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition record)
  acknowledgement <- requireAcknowledgement result
  assertEqual
    "the held item advances receipt"
    (Just firstStreamSequence)
    (streamPrefixSequence (PeerInput.peerAcknowledgementReceivedPrefix acknowledgement))
  assertEqual
    "the held item does not advance completion"
    Nothing
    (streamPrefixSequence (PeerInput.peerAcknowledgementCompletedPrefix acknowledgement))
  assertBool
    "holding the source cut mutates no semantic owner"
    ( PeerInput.peerInputStoreState held == PeerInput.peerInputStoreState fixture.state
        && PeerInput.peerInputGraphState held == PeerInput.peerInputGraphState fixture.state
        && PeerInput.peerInputControlledState held == PeerInput.peerInputControlledState fixture.state
    )
  (stillHeld, early) <-
    checked
      "early source topology release"
      (PeerInput.releasePeerTopology fixture.context futureCut held)
  assertBool "an early release is state-identical" (stillHeld == held)
  assertEqual
    "an early release remains held"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition early)
  (replayed, replay) <-
    checked
      "replayed early source topology release"
      (PeerInput.releasePeerTopology fixture.context futureCut stillHeld)
  assertBool "a repeated early release is state-identical" (replayed == held)
  assertEqual
    "a repeated early release remains held"
    PeerInput.PeerDependencyStillHeld
    (PeerInput.peerDependencyReleaseDisposition replay)

casePreAuthenticationToStructuralHold :: Assertion
casePreAuthenticationToStructuralHold = do
  fixture <- peerFixture
  activeDelta <- activeDeltaFixtureFrom fixture
  (withStructuralChange, _) <-
    checked
      "apply the structural change used by the future topology cut"
      ( PeerInput.applyPeerPublication
          activeDelta.context
          activeDelta.binding
          activeDelta.item
          activeDelta.state
      )
  (_, futureCut) <- establishPeerInputTopologyCut fixture withStructuralChange
  originalItem <- fixturePublicationItemForTopologyCut futureCut
  originalStamp <-
    maybe
      (assertFailure "the topology-held fixture has no structural stamp")
      pure
      (peerPublicationStructuralStamp (sequencedItemPayload originalItem))
  let membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView fixture.oracle)
      missingOccurrence =
        structuralOccurrenceId fixture.localEpoch firstStructuralSequence
      successorOccurrence =
        structuralOccurrenceId
          fixture.remoteEpoch
          (nextStructuralSequence firstStructuralSequence)
      currentVector =
        GraphProgress.structuralAppliedVector
          (PeerInput.peerInputStructuralProgressState withStructuralChange)
  claimedPredecessor <-
    checked
      "cross-source predecessor vector for topology-held publication"
      ( mkStructuralVersionVector
          membership
          [ ( epoch,
              if epoch == fixture.localEpoch
                then structuralPrefixThrough firstStructuralSequence
                else
                  maybe
                    emptyStructuralPrefix
                    id
                    (structuralVersionVectorComponent epoch currentVector)
            )
          | member <- checkedActiveHeralds fixture.genesis,
            let epoch = heraldMemberEpoch member
          ]
      )
  modifiedStamp <-
    checked
      "topology-held structural stamp with an absent causal predecessor"
      ( mkStructuralOccurrenceStamp
          successorOccurrence
          claimedPredecessor
          (structuralOccurrenceStampPublication originalStamp)
          (structuralOccurrenceStampPublicationDigest originalStamp)
          (structuralOccurrenceStampCarrierRole originalStamp)
      )
  peerPublication <-
    checked
      "topology-held publication with an absent causal predecessor"
      ( presentedStructuralPeerPublication
          modifiedStamp
          (peerPublicationBatch (sequencedItemPayload originalItem))
      )
  let item =
        sequencedItem
          (sequencedItemDirection originalItem)
          (nextStreamSequence (sequencedItemSequence originalItem))
          (peerPublicationDigest peerPublication)
          peerPublication
      identifier = publicationBatchId (peerPublicationBatch peerPublication)
      topologyDependency = Publication.SourceTopologyDependency futureCut
      causalDependency =
        Publication.StructuralApplicationDependency
          (Reconciliation.StructuralPredecessorDependency missingOccurrence)
  (held, _) <-
    checked
      "retain a structural publication at its final pre-authentication hold"
      ( PeerInput.applyPeerPublication
          activeDelta.context
          fixture.binding
          item
          withStructuralChange
      )
  heldRecord <- requireIncomingById identifier held
  assertEqual
    "the unavailable source cut is the sole pre-authentication hold"
    (Set.singleton topologyDependency)
    (Publication.incomingPublicationDependencies heldRecord)
  (withTopology, establishedCut) <- establishPeerInputTopologyCut fixture held
  assertEqual
    "the installed topology cut is the publication prerequisite"
    futureCut
    establishedCut
  (causallyHeld, _) <-
    checked
      "replace the final topology hold with the revealed causal hold"
      (PeerInput.releasePeerTopology activeDelta.context futureCut withTopology)
  causallyHeldRecord <- requireIncomingById identifier causallyHeld
  assertEqual
    "the released pre-authentication hold becomes the exact causal dependency"
    (Set.singleton causalDependency)
    (Publication.incomingPublicationDependencies causallyHeldRecord)
  assertEqual
    "the structurally blocked publication remains dependency-held"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition causallyHeldRecord)
  assertBool
    "the causal hold is semantically authenticated"
    (Publication.incomingPublicationSemanticallyAuthenticated causallyHeldRecord)

caseStructuralDestinationAdmission :: Assertion
caseStructuralDestinationAdmission = do
  fixture <- activeDeltaFixture
  mapM_
    ( \(label, item) ->
        case PeerInput.applyPeerPublication fixture.context fixture.binding item fixture.state of
          Left
            ( PeerInput.PeerInputProtocolProblem
                PeerInput.PeerInputStructuralDestinationMismatch {}
              ) -> pure ()
          Left problem ->
            assertFailure (label <> " returned the wrong problem: " <> show problem)
          Right _ -> assertFailure (label <> " was admitted")
    )
    fixture.invalidDestinationItems
  (weaklyApplied, _) <-
    checked
      "source-attenuated structural destination"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          fixture.weakItem
          fixture.state
      )
  record <- requireIncomingById fixture.identifier weaklyApplied
  assertEqual
    "a Weak structural Forward destination remains admissible"
    Publication.Applied
    (Publication.incomingPublicationDisposition record)
  (readerApplied, _) <-
    checked
      "structural destination with a frozen application reader"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          fixture.readerItem
          fixture.state
      )
  readerRecord <- requireIncomingById fixture.identifier readerApplied
  assertEqual
    "the mandatory system view applies while an unavailable frozen reader terminates normally"
    Publication.Applied
    (Publication.incomingPublicationDisposition readerRecord)
  assertEqual
    "the retained placement witness admits the stale reader as an exact terminally ignored destination"
    (Just Publication.DestinationTerminallyIgnored)
    ( Map.lookup
        fixture.readerDestination
        (Publication.incomingPublicationDestinationOutcomes readerRecord)
    )

caseTerminalStructuralStoreSuppression :: Assertion
caseTerminalStructuralStoreSuppression = do
  fixture <- activeDeltaFixture
  decision <-
    checked
      "terminal Delta label decision"
      (mkLabelDecisionId (fixtureIdentifierBytes 0xd7))
  (cause, deleted) <-
    installTerminalLabelDeletion
      fixture.bootstraps
      decision
      (globalObjectIdFromDeltaId fixture.delta)
      fixture.state
  (successor, _) <-
    checked
      "apply delayed terminal Delta"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          fixture.item
          deleted
      )
  let object = globalObjectIdFromDeltaId fixture.delta
      progress = PeerInput.peerInputStructuralProgressState successor
      reconciliation = GraphProgress.structuralProgressReconciliation progress
      beforeStore = PeerInput.peerInputStoreState deleted
      afterStore = PeerInput.peerInputStoreState successor
  occurrence <- structuralItemOccurrence fixture.item
  record <- requireIncomingById fixture.identifier successor
  assertEqual
    "the delayed Delta occurrence is retained"
    [occurrence]
    (Reconciliation.structuralAppliedHistoryOccurrences reconciliation)
  assertBool
    "the delayed Delta advances the structural progress vector"
    (GraphProgress.lookupAppliedStructuralOccurrence occurrence progress /= Nothing)
  assertEqual
    "the exact deletion cause becomes the structural terminal overlay"
    (Just (Reconciliation.StructuralDeletedOverlayView cause))
    (lookup object (Reconciliation.structuralAppliedControlOverlays reconciliation))
  assertBool
    "the deleted Delta has no effective structural vertex"
    ( Map.notMember
        object
        (Reconciliation.structuralAppliedVertexProjections reconciliation)
        && not
          ( Graph.graphHasVertex
              (DeltaVertex fixture.delta)
              (PeerInput.peerInputGraphState successor)
          )
    )
  assertEqual
    "the terminal destination is acknowledged without Store application"
    (Just Publication.DestinationTerminallyIgnored)
    ( Map.lookup
        ( NonEmpty.head
            ( publicationBatchDestinations
                (peerPublicationBatch (sequencedItemPayload fixture.item))
            )
        )
        (Publication.incomingPublicationDestinationOutcomes record)
    )
  assertBool
    "neither the structural patch nor ordinary delivery inserts a Store fact"
    (afterStore == beforeStore)
  assertBool
    "terminal reconciliation allocates no Store incarnation"
    ( PeerInput.peerInputIdGeneratorState successor
        == PeerInput.peerInputIdGeneratorState deleted
    )
  assertBool
    "terminal reconciliation installs no local placement"
    ( PeerInput.peerInputPlacementState successor
        == PeerInput.peerInputPlacementState deleted
    )

caseHiddenBootstrapWriterRejected :: Assertion
caseHiddenBootstrapWriterRejected =
  mapM_
    exercise
    [ (NablaCarrier, NablaRole),
      (DeltaCarrier, DeltaRole)
    ]
  where
    exercise (carrierRole, carrierCatalogueRole) = do
      base <-
        peerFixtureFor
          causalReceiverGenesis
          [firstLocalBootstrapId, fixtureRemoteBootstrapId]
          fixtureLocalMember
      bootstrap <-
        maybe
          (assertFailure "missing unstarted configured-root process")
          pure
          (lookupConfiguredProcessBootstrap secondLocalBootstrapId base.genesis)
      root <- case configuredProcessBootstrapRoots bootstrap of
        first : _ -> pure first
        [] -> assertFailure "configured-root process has no root"
      objectField <- checked "configured-root object field" (mkFieldName "object_id")
      labelField <- checked "configured-root label field" (mkFieldName "label")
      sortField <- checked "configured-root sort field" (mkFieldName "sort_id")
      sequencingField <-
        checked "configured-root sequencing field" (mkFieldName "sequencing_object")
      let process = configuredProcessBootstrapProcessEpochId bootstrap
          carriedSort =
            predefinedCatalogueSortId
              (profileEntryFor (configuredRootBootstrapCatalogueRole root))
          (object, targetFields) = case carrierRole of
            NablaCarrier ->
              ( globalObjectIdFromNablaId
                  (configuredRootBootstrapWriterNabla root),
                [ ( sequencingField,
                    optionalGlobalUniqueIdValue
                      ( case configuredRootBootstrapWriterSequencing root of
                          UnsequencedNabla -> Nothing
                          NablaSequencedBy sequencingObject ->
                            Just
                              ( globalUniqueIdFromGlobalObjectId
                                  sequencingObject
                              )
                      )
                  )
                ]
              )
            DeltaCarrier ->
              ( globalObjectIdFromDeltaId
                  (configuredRootBootstrapReaderDelta root),
                []
              )
            _ -> error "terminal configured-root fixture has a non-root carrier"
      value <-
        checked
          "configured-root value"
          ( recordValue
              ( [ ( objectField,
                    globalUniqueIdValue
                      (globalUniqueIdFromGlobalObjectId object)
                  ),
                  (labelField, labelValue ((ProcessLabel process, 0))),
                  (sortField, bytesValue (sortIdBytes carriedSort))
                ]
                  <> targetFields
              )
          )
      source <-
        maybe
          (assertFailure "configured-root hidden source is absent")
          pure
          (lookupSystemBootstrapWriter base.remoteEpoch carrierRole base.genesis)
      let descriptor =
            predefinedCatalogueDescriptor (profileEntryFor carrierCatalogueRole)
          identifier =
            publicationId
              (systemBootstrapWriterNablaId source)
              (genesisAuthorityEpoch (systemBootstrapWriterAuthority source))
              base.remoteEpoch
              (nablaSequence 0)
          carrierOccurrence =
            predefinedOccurrenceFor
              carrierCatalogueRole
              (checkedPredefinedOccurrenceSet base.genesis)
          destination =
            publicationDestination
              ( deriveSystemViewDeltaId
                  (checkedSystemId base.genesis)
                  base.localEpoch
                  carrierCatalogueRole
              )
              ( deriveSystemViewStoreIncarnationId
                  (checkedSystemId base.genesis)
                  base.localEpoch
                  carrierCatalogueRole
              )
              Normal
      publication <-
        checked
          "configured-root checked publication"
          (mkCheckedPublication descriptor identifier value)
      batch <-
        checked
          "configured-root batch"
          ( mkPublicationBatch
              identifier
              process
              (predefinedCatalogueSortId (profileEntryFor carrierCatalogueRole))
              carrierOccurrence
              (checkedPublicationCanonicalValue publication)
              Normal
              base.topologyCut
              (controlIndex 1)
              (destination :| [])
          )
      digest <-
        checked
          "configured-root structural digest"
          ( structuralPublicationDigestFor
              descriptor
              carrierOccurrence
              publication
              batch
          )
      let occurrence =
            structuralOccurrenceId base.remoteEpoch firstStructuralSequence
      stamp <-
        checked
          "configured-root structural stamp"
          ( mkStructuralOccurrenceStamp
              occurrence
              ( GraphProgress.structuralAppliedVector
                  (PeerInput.peerInputStructuralProgressState base.state)
              )
              identifier
              digest
              carrierRole
          )
      peerPublication <-
        checked
          "configured-root peer publication"
          ( mkStructuralPeerPublication
              descriptor
              carrierOccurrence
              publication
              stamp
              batch
          )
      let item =
            sequencedItem
              base.direction
              firstStreamSequence
              (peerPublicationDigest peerPublication)
              peerPublication
          startedOracle =
            projectedAfterEnvelopeFor
              base.genesis
              base.bootstraps
              ( oracleEnvelope
                  (oracleClientRequestId base.remoteEpoch 1)
                  Nothing
                  base.remoteEpoch
                  (fixtureStartCommand bootstrap)
              )
          context =
            PeerInput.peerInputContext
              base.genesis
              startedOracle
              base.discovery
              base.placement
      assertBool
        "the source dynamic Start is accepted before checking writer authority"
        (OracleProjection.oracleViewProcessIsLive process (OracleProjection.oracleView startedOracle))
      preparedProcess <-
        checked
          "install the same root-free process fact as OracleAdvance"
          ( Controlled.prepareControlledBootstrap
              ( Controlled.processFact
                  (configuredProcessBootstrapProcessId bootstrap)
                  process
                  base.remoteEpoch
                  (labelAuthorityEpoch (controlIndex 1))
              )
              []
              (PeerInput.peerInputControlledState base.state)
          )
      withStartControl <-
        advancePeerInputStructuralControl
          (controlIndex 1)
          (replacePeerInputControlled (Controlled.commitControlledBootstrap preparedProcess) base.state)
      assertProtocolProblem
        "an applied dynamic process fact cannot authorize a hidden genesis writer"
        ( \case
            PeerInput.PeerInputProtocolProblem PeerInput.PeerInputSourceWriterUnknown {} -> True
            _ -> False
        )
        (PeerInput.applyPeerPublication context base.binding item withStartControl)

caseTerminalStructuralEdgeSuppression :: Assertion
caseTerminalStructuralEdgeSuppression = do
  fixture <- terminalEdgeFixture
  (successor, _) <-
    checked
      "apply delayed terminal Edge"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          fixture.item
          fixture.state
      )
  occurrence <- structuralItemOccurrence fixture.item
  let progress = PeerInput.peerInputStructuralProgressState successor
      reconciliation = GraphProgress.structuralProgressReconciliation progress
  assertEqual
    "the delayed Edge occurrence is retained"
    [occurrence]
    (Reconciliation.structuralAppliedHistoryOccurrences reconciliation)
  assertBool
    "the delayed Edge advances the structural progress vector"
    (GraphProgress.lookupAppliedStructuralOccurrence occurrence progress /= Nothing)
  assertEqual
    "the delayed Edge retains the exact deletion overlay"
    (Just (Reconciliation.StructuralDeletedOverlayView fixture.cause))
    (lookup fixture.object (Reconciliation.structuralAppliedControlOverlays reconciliation))
  assertBool
    "the terminal Edge creates no effective edge projection"
    ( Map.notMember
        fixture.object
        (Reconciliation.structuralAppliedEdgeProjections reconciliation)
    )
  assertBool
    "the terminal Edge changes neither Graph nor Placement nor Store"
    ( PeerInput.peerInputGraphState successor == PeerInput.peerInputGraphState fixture.state
        && PeerInput.peerInputPlacementState successor
          == PeerInput.peerInputPlacementState fixture.state
        && PeerInput.peerInputStoreState successor
          == PeerInput.peerInputStoreState fixture.state
    )

caseTerminalReplayAfterRepeatedRetirement :: Assertion
caseTerminalReplayAfterRepeatedRetirement = do
  base <- peerFixtureFor fixtureOracleHeraldGenesis (firstLocalBootstrapId : [fixtureRemoteBootstrapId]) fixtureRemoteMember
  fixture <- activeDeltaFixtureFrom base
  let publication = sequencedItemPayload fixture.item
  stamp <- maybe (assertFailure "terminal replay fixture needs a stamp") pure (peerPublicationStructuralStamp publication)
  payload <- maybe (assertFailure "terminal replay fixture needs canonical bytes") pure (peerPublicationStructuralCanonicalBytes publication)
  occurrence <- checked "retained terminal replay payload" (terminalStructuralOccurrence stamp payload)
  (materialized, _) <- checked "initial exact terminal materialization" (PeerInput.materializeTerminalStructuralOccurrence fixture.context occurrence fixture.state)
  let initialMembership = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView base.oracle)
      prefix =
        projectedAfterEnvelopeFor
          base.genesis
          base.bootstraps
          (oracleEnvelope (oracleClientRequestId base.localEpoch 1) Nothing base.localEpoch (Voter.cancelVoterChangeCommand (checkedEither "unknown voter change" (Voter.mkVoterChangeId (controlIndex 999999)))))
  _ <-
    foldM
      ( \(state, projection, discovery) (index, retired) -> do
          let view = OracleProjection.oracleView projection
              predecessorMembership = OracleProjection.oracleViewCurrentHeraldMembership view
          successorMembership <- checked "next terminal replay retirement" (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) retired predecessorMembership)
          let ends =
                [ OracleProjection.membershipAdvanceProcessEnd process index fixtureHeraldRetirementReason
                | process <- OracleProjection.projectedProcessEpochs projection,
                  OracleProjection.oracleViewProcessResidence process view == Just retired,
                  OracleProjection.oracleViewProcessIsLive process view
                ]
          projected <- OracleProjection.commitMembershipAdvance <$> checked "project terminal replay retirement" (OracleProjection.prepareMembershipAdvance index successorMembership ends projection)
          preparedDiscovery <- checked "retire original terminal source binding" (DiscoveryState.prepareMembershipAdvance successorMembership discovery)
          let (contractedDiscovery, _) = DiscoveryState.commitMembershipAdvance preparedDiscovery
              currentView = OracleProjection.oracleView projected
              target = Membership.heraldMembershipGenerationId successorMembership
          fullLineage <- maybe (assertFailure "missing complete retirement history") pure (OracleProjection.oracleViewHeraldMembershipLineage (Membership.heraldMembershipGenerationId initialMembership) target currentView)
          anchorLineage <- maybe (assertFailure "missing installed anchor lineage") pure (OracleProjection.oracleViewHeraldMembershipLineage (Membership.heraldMembershipGenerationId predecessorMembership) target currentView)
          ready <- advancePeerInputStructuralControl index state
          let progress = PeerInput.peerInputStructuralProgressState ready
          (cut, successorBase) <- baseForLineageWithPayloads fullLineage anchorLineage progress (deriveTopologyOccurrenceDigest "terminal-replay-history") [occurrence]
          preparedBase <- checked "install checked terminal membership base" (GraphProgress.prepareMembershipSuccessorBaseInstallation successorMembership successorBase cut progress)
          let installedProgress = GraphProgress.commitMembershipSuccessorBaseInstallation preparedBase
              installed = replacePeerInputStructuralProgress installedProgress ready
              context = PeerInput.peerInputContext base.genesis projected contractedDiscovery (PeerInput.peerInputPlacementState installed)
          assertEqual "Graph installs the exact descendant membership" target (GraphProgress.structuralProgressMembershipGenerationId installedProgress)
          assertEqual "the original source remains retired" Nothing (structuralVersionVectorComponent base.remoteEpoch (GraphProgress.structuralAppliedVector installedProgress))
          (replayed, result) <- checked "replay old retired payload after installed base" (PeerInput.materializeTerminalStructuralOccurrence context occurrence installed)
          assertBool "retired-source replay changes no owner" (replayed == installed)
          assertEqual "the retained receipt remains materialized" PeerInput.TerminalStructuralMaterialized (PeerInput.terminalStructuralMaterializationDisposition result)
          assertEqual "replay emits no placement" [] (PeerInput.terminalStructuralMaterializationPlacementSnapshots result)
          assertEqual "replay emits no application wake" [] (PeerInput.terminalStructuralMaterializationWakes result)
          pure (replayed, projected, contractedDiscovery)
      )
      (materialized, prefix, base.discovery)
      [(controlIndex 2, base.remoteEpoch), (controlIndex 3, heraldMemberEpoch fixtureThirdMember)]
  pure ()

caseTerminalStructuralMaterialization :: Assertion
caseTerminalStructuralMaterialization = do
  fixture <- activeDeltaFixture
  let peerPublication = sequencedItemPayload fixture.item
      batch = peerPublicationBatch peerPublication
  stamp <-
    maybe
      (assertFailure "active Delta fixture has no structural stamp")
      pure
      (peerPublicationStructuralStamp peerPublication)
  payload <-
    maybe
      (assertFailure "active Delta fixture has no structural semantic transcript")
      pure
      (peerPublicationStructuralCanonicalBytes peerPublication)
  occurrence <-
    checked
      "terminal repair occurrence"
      (terminalStructuralOccurrence stamp payload)
  (materialized, result) <-
    checked
      "materialize terminal repair"
      ( PeerInput.materializeTerminalStructuralOccurrence
          fixture.context
          occurrence
          fixture.state
      )
  assertEqual
    "the exact repair is materialized"
    PeerInput.TerminalStructuralMaterialized
    (PeerInput.terminalStructuralMaterializationDisposition result)
  assertBool
    "the repair installs the original structural occurrence"
    ( GraphProgress.lookupAppliedStructuralOccurrence
        (structuralOccurrenceStampOccurrence stamp)
        (PeerInput.peerInputStructuralProgressState materialized)
        /= Nothing
    )
  (replayed, replayResult) <-
    checked
      "replay terminal repair"
      ( PeerInput.materializeTerminalStructuralOccurrence
          fixture.context
          occurrence
          materialized
      )
  assertBool "an exact terminal repair replay is owner-idempotent" (replayed == materialized)
  assertEqual
    "the replay remains a successful materialization"
    PeerInput.TerminalStructuralMaterialized
    (PeerInput.terminalStructuralMaterializationDisposition replayResult)

  alternateNabla <- checked "wrong terminal-source Nabla" (mkNablaId (fixtureIdentifierBytes 0xed))
  let originalPublication = publicationBatchId batch
      originalOccurrence = structuralOccurrenceStampOccurrence stamp
      originalSource = publicationSourceHeraldEpoch originalPublication
      otherSource =
        case [ herald
             | (herald, _) <-
                 structuralVersionVectorEntries
                   (structuralOccurrenceStampPredecessor stamp),
               herald /= originalSource
             ] of
          firstOther : _ -> firstOther
          [] -> error "terminal materialization fixture has no other membership source"
      wrongSourceOccurrence = structuralOccurrenceId otherSource firstStructuralSequence
      wrongSourcePublication =
        publicationId
          alternateNabla
          ( structuralAuthorityEpoch
              wrongSourceOccurrence
              (publicationBatchSourceTopologyPrerequisite batch)
          )
          otherSource
          (nablaSequence 1)
  wrongSourceStamp <-
    checked
      "wrong retired-source stamp"
      ( mkStructuralOccurrenceStamp
          wrongSourceOccurrence
          (structuralOccurrenceStampPredecessor stamp)
          wrongSourcePublication
          (deriveTerminalStructuralPayloadDigest payload)
          (structuralOccurrenceStampCarrierRole stamp)
      )
  wrongSourceOccurrencePayload <-
    checked
      "wrong retired-source occurrence payload"
      (terminalStructuralOccurrence wrongSourceStamp payload)
  assertProtocolProblem
    "a payload cannot borrow another retired source/stamp"
    ( \case
        PeerInput.PeerInputPeerPublicationProblem
          (StructuralStampPublicationMismatch expected observed) ->
            expected == originalPublication && observed == wrongSourcePublication
        _ -> False
    )
    ( PeerInput.materializeTerminalStructuralOccurrence
        fixture.context
        wrongSourceOccurrencePayload
        fixture.state
    )

  entry <-
    maybe
      (assertFailure "active Delta carrier is absent from the registry")
      pure
      ( SortRegistry.lookupEffectiveSort
          (publicationBatchSortId batch)
          (PeerInput.peerInputSortRegistryState fixture.state)
      )
  value <-
    checked
      "decode terminal repair value"
      (decodeCanonicalValue (canonicalValueByteString (publicationBatchCanonicalValue batch)))
  let unknownPublication =
        publicationId
          alternateNabla
          ( structuralAuthorityEpoch
              ( structuralOccurrenceId
                  originalSource
                  (nextStructuralSequence firstStructuralSequence)
              )
              (publicationBatchSourceTopologyPrerequisite batch)
          )
          originalSource
          (nablaSequence 2)
  unknownChecked <-
    checked
      "unknown-authority terminal publication"
      (mkCheckedPublication (SortRegistry.registryEntryDescriptor entry) unknownPublication value)
  unknownAuthorityPayload <-
    checked
      "unknown-authority terminal transcript"
      ( structuralPublicationCanonicalBytesForSemantics
          (SortRegistry.registryEntryDescriptor entry)
          (publicationBatchOccurrenceId batch)
          unknownChecked
          (publicationBatchSourceProcess batch)
          (publicationBatchSourceStrength batch)
          (publicationBatchSourceTopologyPrerequisite batch)
          (publicationBatchControlPrerequisite batch)
      )
  unknownAuthorityStamp <-
    checked
      "unknown-authority terminal stamp"
      ( mkStructuralOccurrenceStamp
          originalOccurrence
          (structuralOccurrenceStampPredecessor stamp)
          unknownPublication
          (deriveTerminalStructuralPayloadDigest unknownAuthorityPayload)
          (structuralOccurrenceStampCarrierRole stamp)
      )
  unknownAuthorityOccurrence <-
    checked
      "unknown-authority terminal occurrence"
      (terminalStructuralOccurrence unknownAuthorityStamp unknownAuthorityPayload)
  assertProtocolProblem
    "an unknown retained authority is rejected by the serialized owner"
    ( \case
        PeerInput.PeerInputProtocolProblem PeerInput.PeerInputSourceWriterUnknown {} -> True
        _ -> False
    )
    ( PeerInput.materializeTerminalStructuralOccurrence
        fixture.context
        unknownAuthorityOccurrence
        fixture.state
    )
  assertProtocolProblem
    "an applied occurrence cannot replay a different checked payload and digest"
    ( \case
        PeerInput.PeerInputProtocolProblem (PeerInput.PeerInputTerminalStructuralOccurrenceConflict observed) -> observed == originalOccurrence
        _ -> False
    )
    (PeerInput.materializeTerminalStructuralOccurrence fixture.context unknownAuthorityOccurrence materialized)

  let unknownSortBytes = fixtureIdentifierBytes 0xee
      unknownSortPayload =
        replaceCanonicalField
          (sortIdBytes (publicationBatchSortId batch))
          unknownSortBytes
          payload
  unknownSortStamp <-
    checked
      "unknown-sort terminal stamp"
      ( mkStructuralOccurrenceStamp
          originalOccurrence
          (structuralOccurrenceStampPredecessor stamp)
          originalPublication
          (deriveTerminalStructuralPayloadDigest unknownSortPayload)
          (structuralOccurrenceStampCarrierRole stamp)
      )
  unknownSortOccurrence <-
    checked
      "unknown-sort terminal occurrence"
      (terminalStructuralOccurrence unknownSortStamp unknownSortPayload)
  assertProtocolProblem
    "a nominal but unknown structural sort is rejected"
    ( \case
        PeerInput.PeerInputProtocolProblem
          (PeerInput.PeerInputTerminalStructuralSortUnknown observed) ->
            sortIdBytes observed == unknownSortBytes
        _ -> False
    )
    ( PeerInput.materializeTerminalStructuralOccurrence
        fixture.context
        unknownSortOccurrence
        fixture.state
    )

replaceCanonicalField :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString
replaceCanonicalField needle replacement supplied =
  case ByteString.breakSubstring needle supplied of
    (prefix, suffix)
      | not (ByteString.null suffix) ->
          prefix <> replacement <> ByteString.drop (ByteString.length needle) suffix
    _ -> error "terminal transcript fixture field is absent"

caseActiveDeltaReceiverAllocation :: Assertion
caseActiveDeltaReceiverAllocation = do
  fixture <- activeDeltaFixture
  assertEqual
    "the receiver generator starts unused"
    0
    (generatorCounter fixture.state)
  (applied, result) <-
    checked
      "apply active Delta structural publication"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          fixture.item
          fixture.state
      )
  record <- requireIncomingById fixture.identifier applied
  assertEqual
    "the active Delta publication applies"
    Publication.Applied
    (Publication.incomingPublicationDisposition record)
  assertEqual
    "the receiver consumes exactly one generated identity"
    1
    (generatorCounter applied)
  placement <-
    maybe
      (assertFailure "missing generated active-Delta placement")
      pure
      ( Placement.lookupLocalPlacement
          fixture.delta
          (PeerInput.peerInputPlacementState applied)
      )
  assertEqual
    "Placement retains the generated incarnation"
    fixture.expectedIncarnation
    (Placement.localPlacementStoreIncarnation placement)
  slot <-
    maybe
      (assertFailure "missing generated active-Delta Store")
      pure
      ( Store.lookupStoreSlot
          fixture.delta
          (PeerInput.peerInputStoreState applied)
      )
  assertEqual
    "Store and Placement share the generated incarnation"
    fixture.expectedIncarnation
    (Store.storeSlotIncarnation slot)
  assertEqual
    "first assignment is contiguous"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition result)
  assertEqual
    "structural application does not advertise placement before a topology cut"
    []
    (PeerInput.peerInputPlacementSnapshots result)

  let owner = Placement.localPlacementHeraldEpoch placement
      indexedBootstraps =
        [ (appliedProcessEpochId bootstrap, bootstrap)
        | bootstrap <- checkedInitialBootstraps fixture.bootstraps
        ]
      routeWithPrerequisite prerequisite =
        PlacementMessage.applicationDeltaRoute
          (Placement.localPlacementDelta placement)
          (Placement.localPlacementSortId placement)
          (Placement.localPlacementOccurrenceId placement)
          (Placement.localPlacementControllerObject placement)
          (Placement.localPlacementProcessEpoch placement)
          (Placement.localPlacementStoreIncarnation placement)
          prerequisite
      exactRoute =
        routeWithPrerequisite
          (Placement.localPlacementControlPrerequisite placement)
      routeIsValid owners =
        PeerPlacement.remotePlacementRouteIsValid
          (checkedSystemId fixtureCheckedGenesis)
          owner
          indexedBootstraps
          (PeerInput.peerInputGraphState owners)
          (PeerInput.peerInputStructuralProgressState owners)
  assertBool
    "the generated route is not startup-authorized before its structural occurrence"
    (not (routeIsValid fixture.state exactRoute))
  assertBool
    "the retained structural occurrence authorizes the exact generated route"
    (routeIsValid applied exactRoute)
  assertBool
    "the retained occurrence rejects a fabricated control prerequisite"
    (not (routeIsValid applied (routeWithPrerequisite (controlIndex 777))))

  (retried, retryResult) <-
    checked
      "retry active Delta structural publication"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          fixture.item
          applied
      )
  assertBool "exact retry changes no peer-input owner" (retried == applied)
  assertEqual
    "exact retry remains a stream duplicate"
    (PeerInput.PeerInputReceived ReceiveDuplicate)
    (PeerInput.peerInputDisposition retryResult)
  assertEqual
    "exact retry cannot allocate another incarnation"
    1
    (generatorCounter retried)

caseHeldStructuralPredecessor :: Assertion
caseHeldStructuralPredecessor = do
  fixture <- peerFixture
  deltaRoot <- requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap DeltaRole
  nablaRoot <- requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap NablaRole
  startBootstrap <-
    maybe
      (assertFailure "missing configured process reserved for dependency release")
      pure
      (lookupConfiguredProcessBootstrap secondLocalBootstrapId fixtureCheckedGenesis)
  delta <- checked "held predecessor Delta" (mkDeltaId (fixtureIdentifierBytes 0xd1))
  nabla <- checked "causal successor Nabla" (mkNablaId (fixtureIdentifierBytes 0xd2))
  objectField <- checked "held structural object field" (mkFieldName "object_id")
  labelField <- checked "held structural label field" (mkFieldName "label")
  sortField <- checked "held structural sort field" (mkFieldName "sort_id")
  sequencingObjectField <-
    checked "held structural sequencing-object field" (mkFieldName "sequencing_object")
  let process = configuredProcessBootstrapProcessEpochId startBootstrap
      carriedSort = fixture.sortId
  deltaValue <-
    checked
      "held predecessor Delta value"
      ( recordValue
          [ ( objectField,
              globalUniqueIdValue
                (globalUniqueIdFromGlobalObjectId (globalObjectIdFromDeltaId delta))
            ),
            (labelField, labelValue ((ProcessLabel process, 0))),
            (sortField, bytesValue (sortIdBytes carriedSort))
          ]
      )
  nablaValue <-
    checked
      "causal successor Nabla value"
      ( recordValue
          [ ( objectField,
              globalUniqueIdValue
                (globalUniqueIdFromGlobalObjectId (globalObjectIdFromNablaId nabla))
            ),
            (labelField, labelValue (VoidLabel, 0)),
            (sequencingObjectField, optionalGlobalUniqueIdValue Nothing),
            (sortField, bytesValue (sortIdBytes carriedSort))
          ]
      )
  let firstOccurrence =
        structuralOccurrenceId fixture.remoteEpoch firstStructuralSequence
      secondStructuralSequence = nextStructuralSequence firstStructuralSequence
      secondOccurrence =
        structuralOccurrenceId fixture.remoteEpoch secondStructuralSequence
      initialVector =
        GraphProgress.structuralAppliedVector
          (PeerInput.peerInputStructuralProgressState fixture.state)
  members <-
    maybe
      (assertFailure "held structural fixture has no Herald membership")
      pure
      (NonEmpty.nonEmpty (fmap heraldMemberEpoch (checkedActiveHeralds fixtureCheckedGenesis)))
  membership <-
    checked
      "held structural membership generation"
      ( genesisHeraldMembershipGeneration
          (checkedSystemId fixtureCheckedGenesis)
          members
      )
  secondPredecessor <-
    checked
      "causal successor predecessor vector"
      ( mkStructuralVersionVector
          membership
          [ ( member,
              if member == fixture.remoteEpoch
                then structuralPrefixThrough firstStructuralSequence
                else emptyStructuralPrefix
            )
          | member <- NonEmpty.toList members
          ]
      )
  (firstIdentifier, firstItem) <-
    structuralRootItem
      fixture
      deltaRoot
      DeltaRole
      DeltaCarrier
      firstStreamSequence
      firstOccurrence
      initialVector
      deltaValue
  (secondIdentifier, secondItem) <-
    structuralRootItem
      fixture
      nablaRoot
      NablaRole
      NablaCarrier
      (nextStreamSequence firstStreamSequence)
      secondOccurrence
      secondPredecessor
      nablaValue
  let dependency =
        Publication.StructuralApplicationDependency
          (Reconciliation.StructuralProcessDependency process)
      expectedDependencies = Set.singleton dependency
      successorDependency =
        Publication.StructuralApplicationDependency
          (Reconciliation.StructuralPredecessorDependency firstOccurrence)
  (firstHeld, _) <-
    checked
      "retain process-dependent structural predecessor"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          firstItem
          fixture.state
      )
  firstHeldRecord <- requireIncomingById firstIdentifier firstHeld
  assertEqual
    "the predecessor waits on the absent process residence"
    expectedDependencies
    (Publication.incomingPublicationDependencies firstHeldRecord)
  (bothHeld, secondResult) <-
    checked
      "retain causal successor behind its held predecessor"
      ( PeerInput.applyPeerPublication
          fixture.context
          fixture.binding
          secondItem
          firstHeld
      )
  secondHeldRecord <- requireIncomingById secondIdentifier bothHeld
  assertEqual
    "the successor waits on the exact predecessor occurrence"
    (Set.singleton successorDependency)
    (Publication.incomingPublicationDependencies secondHeldRecord)
  secondAcknowledgement <- requireAcknowledgement secondResult
  assertEqual
    "both stream items are received while semantic completion remains held"
    (Just (nextStreamSequence firstStreamSequence), Nothing)
    ( streamPrefixSequence
        (PeerInput.peerAcknowledgementReceivedPrefix secondAcknowledgement),
      streamPrefixSequence
        (PeerInput.peerAcknowledgementCompletedPrefix secondAcknowledgement)
    )
  let liveOracle =
        projectedAfterEnvelope
          fixture.bootstraps
          (startEnvelope fixture startBootstrap Nothing)
      liveContext =
        PeerInput.peerInputContext
          fixtureCheckedGenesis
          liveOracle
          fixture.discovery
          fixture.placement
  (released, releaseResult) <-
    checked
      "release predecessor and causal successor at the live process cut"
      (PeerInput.releasePeerDependency liveContext dependency bothHeld)
  assertEqual
    "fixed-point release applies predecessor before successor"
    [firstIdentifier, secondIdentifier]
    (PeerInput.peerDependencyReleasedPublications releaseResult)
  firstReleasedRecord <- requireIncomingById firstIdentifier released
  secondReleasedRecord <- requireIncomingById secondIdentifier released
  assertEqual
    "the predecessor applies"
    Publication.Applied
    (Publication.incomingPublicationDisposition firstReleasedRecord)
  assertEqual
    "the causal successor applies after its predecessor dot"
    Publication.Applied
    (Publication.incomingPublicationDisposition secondReleasedRecord)
  assertEqual
    "the reconciliation owner retains both causal occurrences in order"
    [firstOccurrence, secondOccurrence]
    ( Reconciliation.structuralAppliedHistoryOccurrences
        ( GraphProgress.structuralProgressReconciliation
            (PeerInput.peerInputStructuralProgressState released)
        )
    )
  retained <-
    checked
      "released causal structural stream"
      ( PeerStream.incomingRetainedItems
          fixture.direction
          (PeerInput.peerInputPeerStreamState released)
      )
  assertEqual
    "fixed-point release completes both ordered stream items"
    []
    (fmap snd retained)

structuralRootItem ::
  PeerFixture ->
  AppliedRoot ->
  PredefinedSortRole ->
  StructuralCarrierRole ->
  StreamSequence ->
  StructuralOccurrenceId ->
  StructuralVersionVector ->
  Value ->
  IO (PublicationId, SequencedItem PeerPublication)
structuralRootItem
  fixture
  root =
    structuralRootItemFrom
      fixture
      fixture.remoteEpoch
      fixture.sourceProcess
      fixture.direction
      root
      (appliedRootControlPrerequisite root)

structuralRootItemFrom ::
  PeerFixture ->
  HeraldEpoch ->
  ProcessEpochId ->
  StreamDirection ->
  AppliedRoot ->
  ControlIndex ->
  PredefinedSortRole ->
  StructuralCarrierRole ->
  StreamSequence ->
  StructuralOccurrenceId ->
  StructuralVersionVector ->
  Value ->
  IO (PublicationId, SequencedItem PeerPublication)
structuralRootItemFrom
  fixture
  sourceEpoch
  sourceProcess
  direction
  root
  controlPrerequisite
  role
  carrierRole
  streamSequence
  structuralOccurrence
  predecessor
  value = do
    let sourceNabla = case appliedRootRole root of
          WriterRoot writer _ -> writer
          _ -> error "requireWriterRoot returned a reader"
        identifier =
          publicationId
            sourceNabla
            (appliedRootAuthority root)
            sourceEpoch
            (nablaSequence 1)
        descriptor = predefinedCatalogueDescriptor (profileEntryFor role)
        destination =
          publicationDestination
            ( deriveSystemViewDeltaId
                (checkedSystemId fixture.genesis)
                fixture.localEpoch
                role
            )
            ( deriveSystemViewStoreIncarnationId
                (checkedSystemId fixture.genesis)
                fixture.localEpoch
                role
            )
            Normal
    publication <-
      checked
        "causal structural checked publication"
        (mkCheckedPublication descriptor identifier value)
    batch <-
      checked
        "causal structural publication batch"
        ( mkPublicationBatch
            identifier
            sourceProcess
            (appliedRootSortId root)
            (appliedRootOccurrenceId root)
            (checkedPublicationCanonicalValue publication)
            Normal
            fixture.topologyCut
            controlPrerequisite
            (destination :| [])
        )
    digest <-
      checked
        "causal structural publication digest"
        ( structuralPublicationDigestFor
            descriptor
            (appliedRootOccurrenceId root)
            publication
            batch
        )
    stamp <-
      checked
        "causal structural occurrence stamp"
        ( mkStructuralOccurrenceStamp
            structuralOccurrence
            predecessor
            identifier
            digest
            carrierRole
        )
    peerPublication <-
      checked
        "causal structural peer publication"
        ( mkStructuralPeerPublication
            descriptor
            (appliedRootOccurrenceId root)
            publication
            stamp
            batch
        )
    pure
      ( identifier,
        sequencedItem
          direction
          streamSequence
          (peerPublicationDigest peerPublication)
          peerPublication
      )

data CausalPredecessorMode
  = ReadyNeutralPredecessor
  | ControlHeldNeutralPredecessor
  | ProcessHeldDeltaPredecessor

data CausalPeerFixture = CausalPeerFixture
  { base :: PeerFixture,
    context :: PeerInput.PeerInputContext,
    predecessorBinding :: Discovery.PeerBinding,
    successorBinding :: Discovery.PeerBinding,
    predecessorSource :: AppliedProcessBootstrap,
    successorSource :: AppliedProcessBootstrap,
    predecessorDirection :: StreamDirection,
    successorDirection :: StreamDirection,
    missingProcessBootstrap :: ConfiguredProcessBootstrap
  }

data CausalStructuralItems = CausalStructuralItems
  { predecessorIdentifier :: PublicationId,
    predecessorItem :: SequencedItem PeerPublication,
    predecessorOccurrence :: StructuralOccurrenceId,
    predecessorNonStructuralDependency :: Maybe Publication.PublicationDependency,
    successorIdentifier :: PublicationId,
    successorItem :: SequencedItem PeerPublication,
    successorOccurrence :: StructuralOccurrenceId
  }

-- Whole-owner audits belong in this test boundary. Exercise both direct
-- application and dependency-held work without asking ingress to revalidate
-- the previously admitted history on every delivery or exact retry.
propStructuralIngressInvariant :: QC.Property
propStructuralIngressInvariant =
  QC.forAll deliverySchedule $ \schedule -> QC.ioProperty $ do
    fixture <- causalPeerFixture
    items <- causalStructuralItems fixture ReadyNeutralPredecessor
    validateOwner fixture.base.state
    completed <- foldM (receiveOne fixture items) fixture.base.state schedule
    assertCausalItemsApplied fixture items completed
    pure True
  where
    deliverySchedule = do
      duplicateCount <- QC.chooseInt (0, 12)
      duplicates <- QC.vectorOf duplicateCount QC.arbitrary
      QC.shuffle (False : True : duplicates)

    validateOwner state =
      assertEqual
        "admitted structural owner satisfies the complete invariant"
        (Right ())
        (GraphProgress.validateStructuralProgressState (PeerInput.peerInputStructuralProgressState state))

    receiveOne fixture items state deliverSuccessor = do
      let (binding, item) =
            if deliverSuccessor
              then (fixture.successorBinding, items.successorItem)
              else (fixture.predecessorBinding, items.predecessorItem)
      (successor, _) <- applyCausalItem "generated causal structural delivery" fixture binding item state
      validateOwner successor
      pure successor

caseAbsentStructuralPredecessor :: Assertion
caseAbsentStructuralPredecessor = do
  fixture <- causalPeerFixture
  items <- causalStructuralItems fixture ReadyNeutralPredecessor
  (successorFirst, firstResult) <-
    applyCausalItem
      "retain successor with absent structural predecessor"
      fixture
      fixture.successorBinding
      items.successorItem
      fixture.base.state
  successorRecord <- requireIncomingById items.successorIdentifier successorFirst
  assertEqual
    "the successor names the exact absent predecessor occurrence"
    (Set.singleton (predecessorDependency items.predecessorOccurrence))
    (Publication.incomingPublicationDependencies successorRecord)
  firstAcknowledgement <- requireAcknowledgement firstResult
  assertEqual
    "the independent source stream receives but cannot complete its first item"
    (Just firstStreamSequence, Nothing)
    ( streamPrefixSequence
        (PeerInput.peerAcknowledgementReceivedPrefix firstAcknowledgement),
      streamPrefixSequence
        (PeerInput.peerAcknowledgementCompletedPrefix firstAcknowledgement)
    )
  (bothApplied, secondResult) <-
    applyCausalItem
      "apply absent structural predecessor"
      fixture
      fixture.predecessorBinding
      items.predecessorItem
      successorFirst
  assertEqual
    "predecessor commit wakes its cross-source successor in the same fixed point"
    [items.predecessorIdentifier, items.successorIdentifier]
    (PeerInput.peerInputPublicationIds secondResult)
  assertCausalItemsApplied fixture items bothApplied

caseNonStructuralHeldPredecessor :: Assertion
caseNonStructuralHeldPredecessor = do
  fixture <- causalPeerFixture
  items <- causalStructuralItems fixture ControlHeldNeutralPredecessor
  nonStructuralDependency <-
    maybe
      (assertFailure "control-held predecessor has no non-structural dependency")
      pure
      items.predecessorNonStructuralDependency
  (predecessorHeld, _) <-
    applyCausalItem
      "retain control-held structural predecessor"
      fixture
      fixture.predecessorBinding
      items.predecessorItem
      fixture.base.state
  predecessorRecord <- requireIncomingById items.predecessorIdentifier predecessorHeld
  assertEqual
    "the predecessor retains its own control dependency"
    (Set.singleton nonStructuralDependency)
    (Publication.incomingPublicationDependencies predecessorRecord)
  (bothHeld, _) <-
    applyCausalItem
      "retain successor behind control-held predecessor"
      fixture
      fixture.successorBinding
      items.successorItem
      predecessorHeld
  successorRecord <- requireIncomingById items.successorIdentifier bothHeld
  assertEqual
    "the successor does not copy the predecessor's unrelated wait set"
    (Set.singleton (predecessorDependency items.predecessorOccurrence))
    (Publication.incomingPublicationDependencies successorRecord)
  assertEqual
    "the predecessor remains held"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition predecessorRecord)
  assertEqual
    "the successor remains held on the causal occurrence"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition successorRecord)

caseReverseDirectionStructuralPredecessor :: Assertion
caseReverseDirectionStructuralPredecessor = do
  fixture <- causalPeerFixture
  items <- causalStructuralItems fixture ProcessHeldDeltaPredecessor
  nonStructuralDependency <-
    maybe
      (assertFailure "process-held predecessor has no non-structural dependency")
      pure
      items.predecessorNonStructuralDependency
  assertBool
    "the successor direction sorts before the predecessor direction"
    (fixture.successorDirection < fixture.predecessorDirection)
  (predecessorHeld, _) <-
    applyCausalItem
      "retain process-held structural predecessor"
      fixture
      fixture.predecessorBinding
      items.predecessorItem
      fixture.base.state
  (bothHeld, _) <-
    applyCausalItem
      "retain reverse-direction successor"
      fixture
      fixture.successorBinding
      items.successorItem
      predecessorHeld
  predecessorRecord <- requireIncomingById items.predecessorIdentifier bothHeld
  successorRecord <- requireIncomingById items.successorIdentifier bothHeld
  assertEqual
    "the predecessor waits on the absent process"
    (Set.singleton nonStructuralDependency)
    (Publication.incomingPublicationDependencies predecessorRecord)
  assertEqual
    "the reverse-sorted successor waits only on its predecessor occurrence"
    (Set.singleton (predecessorDependency items.predecessorOccurrence))
    (Publication.incomingPublicationDependencies successorRecord)
  let liveOracle =
        projectedAfterEnvelopeFor
          fixture.base.genesis
          fixture.base.bootstraps
          ( oracleEnvelope
              (oracleClientRequestId (heraldMemberEpoch fixtureLocalMember) 1)
              Nothing
              (heraldMemberEpoch fixtureLocalMember)
              (fixtureStartCommand fixture.missingProcessBootstrap)
          )
      liveContext =
        PeerInput.peerInputContext
          fixture.base.genesis
          liveOracle
          fixture.base.discovery
          fixture.base.placement
  assertBool
    "the released process dependency is live in the supplied Oracle cut"
    ( OracleProjection.oracleViewProcessIsLive
        ( configuredProcessBootstrapProcessEpochId
            fixture.missingProcessBootstrap
        )
        (OracleProjection.oracleView liveOracle)
    )
  (released, result) <-
    checked
      "release process-held predecessor and reverse-direction successor"
      ( PeerInput.releasePeerDependency
          liveContext
          nonStructuralDependency
          bothHeld
      )
  assertCausalItemsApplied fixture items released
  assertEqual
    "fixed-point release follows the causal edge rather than direction ordering"
    [items.predecessorIdentifier, items.successorIdentifier]
    (PeerInput.peerDependencyReleasedPublications result)

predecessorDependency ::
  StructuralOccurrenceId -> Publication.PublicationDependency
predecessorDependency =
  Publication.StructuralApplicationDependency
    . Reconciliation.StructuralPredecessorDependency

applyCausalItem ::
  String ->
  CausalPeerFixture ->
  Discovery.PeerBinding ->
  SequencedItem PeerPublication ->
  PeerInput.PeerInputState ->
  IO (PeerInput.PeerInputState, PeerInput.PeerInputResult)
applyCausalItem description fixture binding item state =
  checked
    description
    (PeerInput.applyPeerPublication fixture.context binding item state)

assertCausalItemsApplied ::
  CausalPeerFixture ->
  CausalStructuralItems ->
  PeerInput.PeerInputState ->
  Assertion
assertCausalItemsApplied fixture items state = do
  predecessorRecord <- requireIncomingById items.predecessorIdentifier state
  successorRecord <- requireIncomingById items.successorIdentifier state
  assertEqual
    "the predecessor retains no dependency after application"
    Set.empty
    (Publication.incomingPublicationDependencies predecessorRecord)
  assertEqual
    "the successor retains no dependency after application"
    Set.empty
    (Publication.incomingPublicationDependencies successorRecord)
  assertEqual
    "the predecessor applies"
    Publication.Applied
    (Publication.incomingPublicationDisposition predecessorRecord)
  assertEqual
    "the successor applies"
    Publication.Applied
    (Publication.incomingPublicationDisposition successorRecord)
  assertEqual
    "the reconciliation owner retains both causal occurrences"
    (Set.fromList [items.predecessorOccurrence, items.successorOccurrence])
    ( Set.fromList
        ( Reconciliation.structuralAppliedHistoryOccurrences
            ( GraphProgress.structuralProgressReconciliation
                (PeerInput.peerInputStructuralProgressState state)
            )
        )
    )
  predecessorStream <-
    checked
      "causal predecessor stream"
      ( PeerStream.incomingRetainedItems
          fixture.predecessorDirection
          (PeerInput.peerInputPeerStreamState state)
      )
  successorStream <-
    checked
      "causal successor stream"
      ( PeerStream.incomingRetainedItems
          fixture.successorDirection
          (PeerInput.peerInputPeerStreamState state)
      )
  assertEqual
    "the predecessor stream completes"
    []
    (fmap snd predecessorStream)
  assertEqual
    "the successor stream completes"
    []
    (fmap snd successorStream)

causalPeerFixture :: IO CausalPeerFixture
causalPeerFixture = do
  base <-
    peerFixtureFor
      causalReceiverGenesis
      (firstLocalBootstrapId : [fixtureRemoteBootstrapId])
      fixtureRemoteMember
  successorSource <-
    requireProcessAt (heraldMemberEpoch fixtureLocalMember) base.bootstraps
  successorDirection <-
    checked
      "causal successor stream direction"
      ( mkStreamDirection
          (heraldMemberEpoch fixtureLocalMember)
          base.localEpoch
      )
  let projectionDigest = checkedInitialProjectionDigest base.bootstraps
      candidate =
        Discovery.peerCandidate
          (heraldMemberId fixtureLocalMember)
          (heraldMemberEpoch fixtureLocalMember)
          (Discovery.connectionNonce 8)
      hello =
        Discovery.peerHello
          (checkedSystemId base.genesis)
          (heraldMemberId fixtureLocalMember)
          (heraldMemberEpoch fixtureLocalMember)
          (Discovery.connectionNonce 8)
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest base.genesis)
          projectionDigest
          Nothing
  preparedBinding <-
    checked
      "causal successor peer binding"
      (DiscoveryState.preparePeerHello candidate hello base.discovery)
  let (discovery, bindingDisposition) =
        DiscoveryState.commitPeerHello preparedBinding
  successorBinding <- case bindingDisposition of
    Discovery.PeerHelloAccepted Discovery.FirstBinding admitted -> pure admitted
    observed ->
      assertFailure
        ("unexpected causal successor binding disposition: " <> show observed)
  missingProcessBootstrap <-
    maybe
      (assertFailure "missing configured causal process dependency")
      pure
      (lookupConfiguredProcessBootstrap secondLocalBootstrapId base.genesis)
  let context =
        PeerInput.peerInputContext
          base.genesis
          base.oracle
          discovery
          base.placement
  pure
    CausalPeerFixture
      { base,
        context,
        predecessorBinding = base.binding,
        successorBinding,
        predecessorSource = base.sourceBootstrap,
        successorSource,
        predecessorDirection = base.direction,
        successorDirection,
        missingProcessBootstrap
      }

causalReceiverGenesis :: CheckedHeraldGenesis
causalReceiverGenesis =
  checkedEither "causal receiver Herald genesis" (checkHeraldGenesis deployment)
  where
    members = deploymentActiveHeralds fixtureDeploymentManifest <> [fixtureThirdMember]
    deployment =
      fixtureDeploymentManifest
        { deploymentLocalHeraldId = heraldMemberId fixtureThirdMember,
          deploymentLocalHeraldEpoch = heraldMemberEpoch fixtureThirdMember,
          deploymentActiveHeralds = members,
          deploymentOracleGenesis =
            (deploymentOracleGenesis fixtureDeploymentManifest)
              { oracleGenesisActiveHeralds = members
              }
        }

causalStructuralItems ::
  CausalPeerFixture ->
  CausalPredecessorMode ->
  IO CausalStructuralItems
causalStructuralItems fixture mode = do
  (predecessorRoot, predecessorRole, predecessorCarrier, controlPrerequisite) <-
    case mode of
      ReadyNeutralPredecessor -> do
        root <-
          requireWriterRoot
            fixture.base.remoteEpoch
            fixture.predecessorSource
            NeutralVertexRole
        pure
          ( root,
            NeutralVertexRole,
            NeutralVertexCarrier,
            appliedRootControlPrerequisite root
          )
      ControlHeldNeutralPredecessor -> do
        root <-
          requireWriterRoot
            fixture.base.remoteEpoch
            fixture.predecessorSource
            NeutralVertexRole
        pure (root, NeutralVertexRole, NeutralVertexCarrier, controlIndex 1)
      ProcessHeldDeltaPredecessor -> do
        root <-
          requireWriterRoot
            fixture.base.remoteEpoch
            fixture.predecessorSource
            DeltaRole
        pure
          ( root,
            DeltaRole,
            DeltaCarrier,
            appliedRootControlPrerequisite root
          )
  successorRoot <-
    requireWriterRoot
      (heraldMemberEpoch fixtureLocalMember)
      fixture.successorSource
      NeutralVertexRole
  predecessorValue <- case mode of
    ProcessHeldDeltaPredecessor ->
      structuralDeltaValue
        0xd4
        ( configuredProcessBootstrapProcessEpochId
            fixture.missingProcessBootstrap
        )
        fixture.base.sortId
    _ -> structuralNeutralValue 0xd3
  successorValue <- structuralNeutralValue 0xd5
  let predecessorOccurrence =
        structuralOccurrenceId
          fixture.base.remoteEpoch
          firstStructuralSequence
      successorOccurrence =
        structuralOccurrenceId
          (heraldMemberEpoch fixtureLocalMember)
          firstStructuralSequence
      initialVector =
        GraphProgress.structuralAppliedVector
          (PeerInput.peerInputStructuralProgressState fixture.base.state)
  members <-
    maybe
      (assertFailure "causal structural fixture has no Herald membership")
      pure
      ( NonEmpty.nonEmpty
          (fmap heraldMemberEpoch (checkedActiveHeralds fixture.base.genesis))
      )
  membership <-
    checked
      "causal structural membership generation"
      ( genesisHeraldMembershipGeneration
          (checkedSystemId fixture.base.genesis)
          members
      )
  successorPredecessor <-
    checked
      "cross-source causal predecessor vector"
      ( mkStructuralVersionVector
          membership
          [ ( member,
              if member == fixture.base.remoteEpoch
                then structuralPrefixThrough firstStructuralSequence
                else emptyStructuralPrefix
            )
          | member <- NonEmpty.toList members
          ]
      )
  (predecessorIdentifier, predecessorItem) <-
    structuralRootItemFrom
      fixture.base
      fixture.base.remoteEpoch
      fixture.base.sourceProcess
      fixture.predecessorDirection
      predecessorRoot
      controlPrerequisite
      predecessorRole
      predecessorCarrier
      firstStreamSequence
      predecessorOccurrence
      initialVector
      predecessorValue
  (successorIdentifier, successorItem) <-
    structuralRootItemFrom
      fixture.base
      (heraldMemberEpoch fixtureLocalMember)
      (appliedProcessEpochId fixture.successorSource)
      fixture.successorDirection
      successorRoot
      (appliedRootControlPrerequisite successorRoot)
      NeutralVertexRole
      NeutralVertexCarrier
      firstStreamSequence
      successorOccurrence
      successorPredecessor
      successorValue
  let predecessorNonStructuralDependency = case mode of
        ReadyNeutralPredecessor -> Nothing
        ControlHeldNeutralPredecessor ->
          Just (Publication.ControlIndexDependency (controlIndex 1))
        ProcessHeldDeltaPredecessor ->
          Just
            ( Publication.StructuralApplicationDependency
                ( Reconciliation.StructuralProcessDependency
                    ( configuredProcessBootstrapProcessEpochId
                        fixture.missingProcessBootstrap
                    )
                )
            )
  pure
    CausalStructuralItems
      { predecessorIdentifier,
        predecessorItem,
        predecessorOccurrence,
        predecessorNonStructuralDependency,
        successorIdentifier,
        successorItem,
        successorOccurrence
      }

structuralNeutralValue :: Word8 -> IO Value
structuralNeutralValue seed = do
  object <-
    checked
      "causal neutral object"
      (mkGlobalObjectId (fixtureIdentifierBytes seed))
  objectField <- checked "causal neutral object field" (mkFieldName "object_id")
  labelField <- checked "causal neutral label field" (mkFieldName "label")
  checked
    "causal neutral value"
    ( recordValue
        [ ( objectField,
            globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)
          ),
          (labelField, labelValue (VoidLabel, 0))
        ]
    )

structuralDeltaValue :: Word8 -> ProcessEpochId -> SortId -> IO Value
structuralDeltaValue seed process carriedSort = do
  delta <- checked "causal Delta" (mkDeltaId (fixtureIdentifierBytes seed))
  objectField <- checked "causal Delta object field" (mkFieldName "object_id")
  labelField <- checked "causal Delta label field" (mkFieldName "label")
  sortField <- checked "causal Delta sort field" (mkFieldName "sort_id")
  checked
    "causal Delta value"
    ( recordValue
        [ ( objectField,
            globalUniqueIdValue
              ( globalUniqueIdFromGlobalObjectId
                  (globalObjectIdFromDeltaId delta)
              )
          ),
          (labelField, labelValue ((ProcessLabel process, 0))),
          (sortField, bytesValue (sortIdBytes carriedSort))
        ]
    )

-- Fixed semantic state: one genuinely pending publication, one completed
-- publication repeatedly assigned a fresh stream sequence. Receipt ownership
-- must scale with those live consumers rather than the completed assignment tail.
propSparseReceiptOwnership :: QC.Positive Int -> QC.Property
propSparseReceiptOwnership (QC.Positive generated) = QC.ioProperty $ do
  fixture <- peerFixture
  (_, heldBatch, heldItem) <- peerDefinitionItem fixture 200 (controlIndex 1) firstStreamSequence regularPeerDescriptor
  (held, heldResult) <- applyFixture fixture heldItem fixture.state
  heldRecord <- requireIncomingById (publicationBatchId heldBatch) held
  assertEqual "the fixed first publication stays pending" Publication.DependencyHeld (Publication.incomingPublicationDisposition heldRecord)
  source <- sendAndAcknowledge fixture heldItem heldResult (PeerStream.initialState fixture.remoteEpoch)
  _ <- foldM (cycleOne fixture) (source, held, Nothing) [1 .. (1 + generated `mod` 100)]
  pure True
  where
    cycleOne fixture (source, destination, expectedStore) _ = do
      sequenceNumber <- checked "next independent stream allocation" (PeerStream.outgoingNextSequence fixture.direction source)
      let item = ordinarySequencedItem fixture.direction sequenceNumber fixture.batch
      (received, result) <- applyFixture fixture item destination
      acknowledged <- sendAndAcknowledge fixture item result source
      let publication = PeerInput.peerInputPublicationState received
          receiverStream = PeerInput.peerInputPeerStreamState received
          actualStore = PeerInput.peerInputStoreState received
      assertEqual "source retains only the pending assignment" (1, 0, 0) (PeerStream.peerStreamRetentionCounts acknowledged)
      assertEqual "destination retains only the pending assignment" (0, 1, 0) (PeerStream.peerStreamRetentionCounts receiverStream)
      assertEqual "publication index retains only the pending consumer" 1 (Publication.publicationAssignmentRetentionCount publication)
      assertEqual "no receipt tombstones were moved into semantic records" 2 (length (Publication.incomingPublicationEntries publication))
      case expectedStore of
        Nothing -> pure ()
        Just expected -> assertBool "completed reassignment cannot reapply Store" (expected == actualStore)
      pure (acknowledged, received, Just actualStore)
    sendAndAcknowledge fixture item result source = do
      enqueue <- checked "assign exact peer publication" (PeerStream.prepareEnqueue (Map.singleton fixture.localEpoch (peerLogicalPublicationItem (sequencedItemPayload item) :| [])) source)
      let (assigned, _) = PeerStream.commitEnqueue enqueue
      acknowledgement <- requireAcknowledgement result
      completed <- checked "sparse semantic completion" (PeerStream.prepareCompletedProgress fixture.direction (PeerInput.peerAcknowledgementCompletion acknowledgement) assigned)
      pure (fst (PeerStream.commitCompletedAck completed))

caseDeduplication :: Assertion
caseDeduplication = do
  fixture <- peerFixture
  (afterFirst, _) <- applyFixture fixture fixture.item fixture.state
  (afterRetry, retryResult) <- applyFixture fixture fixture.item afterFirst
  assertBool "exact retry is state-identical" (afterFirst == afterRetry)
  assertEqual
    "exact retry reoffers duplicate progress"
    (PeerInput.PeerInputReceived ReceiveDuplicate)
    (PeerInput.peerInputDisposition retryResult)

  let secondItem =
        ordinarySequencedItem
          fixture.direction
          (nextStreamSequence firstStreamSequence)
          fixture.batch
      storeAfterFirst = PeerInput.peerInputStoreState afterFirst
  (afterSecond, secondResult) <- applyFixture fixture secondItem afterFirst
  assertEqual
    "fresh assignment is contiguous"
    (PeerInput.PeerInputReceived ReceiveContiguous)
    (PeerInput.peerInputDisposition secondResult)
  assertBool
    "semantic duplicate does not touch Store"
    (storeAfterFirst == PeerInput.peerInputStoreState afterSecond)
  record <- requireIncoming fixture afterSecond
  assertEqual
    "both completed stream assignments release their semantic record links"
    0
    (Set.size (Publication.incomingPublicationAssignments record))
  acknowledgement <- requireAcknowledgement secondResult
  assertEqual
    "duplicate assignment completes through two"
    (Just (nextStreamSequence firstStreamSequence))
    (streamPrefixSequence (PeerInput.peerAcknowledgementCompletedPrefix acknowledgement))

caseGapSuffix :: Assertion
caseGapSuffix = do
  fixture <- peerFixture
  let second = nextStreamSequence firstStreamSequence
      future =
        ordinarySequencedItem
          fixture.direction
          second
          fixture.batch
  (held, gapResult) <- applyFixture fixture future fixture.state
  assertEqual
    "future item reports the first gap"
    (PeerInput.PeerInputReceived (ReceiveGap firstStreamSequence))
    (PeerInput.peerInputDisposition gapResult)
  assertBool
    "gap retention creates no semantic publication"
    ( PeerInput.peerInputPublicationState fixture.state
        == PeerInput.peerInputPublicationState held
    )
  assertBool
    "gap retention does not touch Store"
    (PeerInput.peerInputStoreState fixture.state == PeerInput.peerInputStoreState held)
  gapAck <- requireAcknowledgement gapResult
  assertEqual "received remains empty" emptyStreamPrefix (PeerInput.peerAcknowledgementReceivedPrefix gapAck)
  assertEqual "completed remains empty" emptyStreamPrefix (PeerInput.peerAcknowledgementCompletedPrefix gapAck)

  (closed, closeResult) <- applyFixture fixture fixture.item held
  closeAck <- requireAcknowledgement closeResult
  assertEqual
    "the maximal contiguous suffix is received"
    (Just second)
    (streamPrefixSequence (PeerInput.peerAcknowledgementReceivedPrefix closeAck))
  assertEqual
    "the maximal contiguous suffix is completed"
    (Just second)
    (streamPrefixSequence (PeerInput.peerAcknowledgementCompletedPrefix closeAck))
  record <- requireIncoming fixture closed
  assertEqual "both completed assignments have been released" 0 (Set.size (Publication.incomingPublicationAssignments record))
  slot <- requireDestinationSlot fixture closed
  assertEqual
    "the suffix applies the semantic publication once"
    1
    (length (filter ((== checkedPublicationId fixture.publication) . fst) (Store.storeSlotApplicationReceipts slot)))

caseGapRepairOfferDeduplication :: Assertion
caseGapRepairOfferDeduplication = do
  fixture <- peerFixture
  let second = nextStreamSequence firstStreamSequence
      third = nextStreamSequence second
      future sequenceNumber =
        ordinarySequencedItem
          fixture.direction
          sequenceNumber
          fixture.batch
  (afterFirstGap, firstResult) <- applyFixture fixture (future second) fixture.state
  firstAcknowledgement <- requireAcknowledgement firstResult
  firstOffer <-
    maybe
      (assertFailure "the first retained gap did not produce a repair offer")
      pure
      (PeerInput.peerAcknowledgementRepairOffer firstAcknowledgement)

  (afterExactRetry, retryResult) <-
    applyFixture fixture (future second) afterFirstGap
  retryAcknowledgement <- requireAcknowledgement retryResult
  assertEqual
    "an exact future-item retry leaves peer-stream state unchanged"
    (PeerInput.peerInputPeerStreamState afterFirstGap)
    (PeerInput.peerInputPeerStreamState afterExactRetry)
  assertEqual
    "an unchanged gap does not repeat its full repair offer"
    Nothing
    (PeerInput.peerAcknowledgementRepairOffer retryAcknowledgement)

  (_, changedResult) <- applyFixture fixture (future third) afterExactRetry
  changedAcknowledgement <- requireAcknowledgement changedResult
  changedOffer <-
    maybe
      (assertFailure "new gap evidence did not produce a changed repair offer")
      pure
      (PeerInput.peerAcknowledgementRepairOffer changedAcknowledgement)
  assertBool
    "new gap evidence changes the repair offer"
    (changedOffer /= firstOffer)

-- | A valid typed payload held at sequence two by a fresh incoming stream.
-- Transition-level tests reuse it to exercise the acknowledgement/repair
-- boundary without duplicating the substantial semantic publication fixture.
fixtureFuturePublicationItem :: IO (SequencedItem PeerPublication)
fixtureFuturePublicationItem = do
  fixture <- peerFixture
  root <- requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap ProcessEpochRole
  object <- checked "future ProcessEpoch object" (mkGlobalObjectId (fixtureIdentifierBytes 0xd1))
  objectField <- checked "ProcessEpoch object field" (mkFieldName "object_id")
  labelField <- checked "ProcessEpoch label field" (mkFieldName "label")
  processField <- checked "ProcessEpoch process field" (mkFieldName "process_id")
  residenceField <- checked "ProcessEpoch residence field" (mkFieldName "residence")
  liveField <- checked "ProcessEpoch live field" (mkFieldName "live")
  value <-
    checked
      "future ProcessEpoch value"
      ( recordValue
          [ (objectField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
            (labelField, labelValue (VoidLabel, 0)),
            (processField, bytesValue (fixtureIdentifierBytes 0xd2)),
            (residenceField, bytesValue (fixtureIdentifierBytes 0xd3)),
            (liveField, boolValue True)
          ]
      )
  let sourceNabla = case appliedRootRole root of
        WriterRoot nabla _ -> nabla
        _ -> error "requireWriterRoot returned a reader"
      identifier =
        publicationId
          sourceNabla
          (appliedRootAuthority root)
          fixture.remoteEpoch
          (nablaSequence 0)
      descriptor = predefinedCatalogueDescriptor (profileEntryFor ProcessEpochRole)
      destination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) fixture.localEpoch ProcessEpochRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) fixture.localEpoch ProcessEpochRole)
          Normal
  publication <-
    checked
      "future ProcessEpoch publication"
      (mkCheckedPublication descriptor identifier value)
  batch <-
    checked
      "future ProcessEpoch batch"
      ( mkPublicationBatch
          identifier
          fixture.sourceProcess
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          (checkedPublicationCanonicalValue publication)
          Normal
          fixture.topologyCut
          (appliedRootControlPrerequisite root)
          (destination :| [])
      )
  peerPublication <-
    checked
      "ordinary ProcessEpoch peer publication"
      ( mkOrdinaryPeerPublication
          descriptor
          (appliedRootOccurrenceId root)
          publication
          batch
      )
  pure
    ( sequencedItem
        fixture.direction
        (nextStreamSequence firstStreamSequence)
        (peerPublicationDigest peerPublication)
        peerPublication
    )

-- | A valid first incoming structural publication whose source-topology claim
-- is supplied by the caller. Whole-Herald cut coordination tests use this to
-- retain work against an actually proposed, not-yet-installed cut without
-- duplicating the substantial authenticated peer fixture.
fixturePublicationItemForTopologyCut ::
  TopologyCutId ->
  IO (SequencedItem PeerPublication)
fixturePublicationItemForTopologyCut sourceTopology = do
  fixture <- peerFixture
  root <- requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap NeutralVertexRole
  object <- checked "cut-held neutral object" (mkGlobalObjectId (fixtureIdentifierBytes 0xdf))
  objectField <- checked "cut-held neutral object field" (mkFieldName "object_id")
  labelField <- checked "cut-held neutral label field" (mkFieldName "label")
  value <-
    checked
      "cut-held neutral value"
      ( recordValue
          [ (objectField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
            (labelField, labelValue (VoidLabel, 0))
          ]
      )
  let sourceNabla = case appliedRootRole root of
        WriterRoot nabla _ -> nabla
        _ -> error "requireWriterRoot returned a reader"
      identifier =
        publicationId
          sourceNabla
          (appliedRootAuthority root)
          fixture.remoteEpoch
          (nablaSequence 1)
      descriptor = predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole)
      destination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) fixture.localEpoch NeutralVertexRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) fixture.localEpoch NeutralVertexRole)
          Normal
  publication <-
    checked
      "cut-held neutral publication"
      (mkCheckedPublication descriptor identifier value)
  batch <-
    checked
      "caller-selected source topology batch"
      ( mkPublicationBatch
          identifier
          fixture.sourceProcess
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          (checkedPublicationCanonicalValue publication)
          Normal
          sourceTopology
          (appliedRootControlPrerequisite root)
          (destination :| [])
      )
  structuralDigest <-
    checked
      "cut-held neutral structural digest"
      ( structuralPublicationDigestFor
          descriptor
          (appliedRootOccurrenceId root)
          publication
          batch
      )
  stamp <-
    checked
      "cut-held neutral structural stamp"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceId fixture.remoteEpoch firstStructuralSequence)
          ( GraphProgress.structuralAppliedVector
              (PeerInput.peerInputStructuralProgressState fixture.state)
          )
          identifier
          structuralDigest
          NeutralVertexCarrier
      )
  peerPublication <-
    checked
      "cut-held neutral peer publication"
      ( mkStructuralPeerPublication
          descriptor
          (appliedRootOccurrenceId root)
          publication
          stamp
          batch
      )
  pure
    ( sequencedItem
        fixture.direction
        firstStreamSequence
        (peerPublicationDigest peerPublication)
        peerPublication
    )

caseMixedDestinations :: Assertion
caseMixedDestinations = do
  fixture <- peerFixture
  absentDelta <- checked "absent DeltaId" (mkDeltaId (fixtureIdentifierBytes 221))
  absentIncarnation <-
    checked
      "absent StoreIncarnationId"
      (mkStoreIncarnationId (fixtureIdentifierBytes 222))
  let absent = publicationDestination absentDelta absentIncarnation Normal
  mixedBatch <-
    checked
      "mixed publication batch"
      ( mkPublicationBatch
          (checkedPublicationId fixture.publication)
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (checkedPublicationCanonicalValue fixture.publication)
          Normal
          (peerFixtureTopologyCut fixture)
          fixture.controlPrerequisite
          (fixture.destination :| [absent])
      )
  let mixedItem =
        ordinarySequencedItem
          fixture.direction
          firstStreamSequence
          mixedBatch
  (successor, _) <- applyFixture fixture mixedItem fixture.state
  record <- requireIncomingById (publicationBatchId mixedBatch) successor
  assertEqual
    "current and absent destinations have exact outcomes"
    ( Map.fromList
        [ (fixture.destination, Publication.DestinationApplied),
          (absent, Publication.DestinationTerminallyIgnored)
        ]
    )
    (Publication.incomingPublicationDestinationOutcomes record)
  assertEqual
    "an absent destination does not create a Store slot"
    Nothing
    (Store.lookupStoreSlot absentDelta (PeerInput.peerInputStoreState successor))

caseDigestMismatch :: Assertion
caseDigestMismatch = do
  fixture <- peerFixture
  wrongDigest <-
    checked
      "wrong peer digest"
      (mkPeerItemDigest (ByteString.replicate 32 0xff))
  assertBool
    "the fixture digest differs"
    (peerItemDigestBytes wrongDigest /= peerItemDigestBytes (ordinaryBatchDigest fixture.batch))
  let conflicting =
        sequencedItem
          fixture.direction
          firstStreamSequence
          wrongDigest
          (presentedOrdinaryPeerPublication fixture.batch)
  case PeerInput.applyPeerPublication
    fixture.context
    fixture.binding
    conflicting
    fixture.state of
    Left (PeerInput.PeerInputPublicationProblem (Publication.IncomingPublicationDigestMismatch _ _)) -> pure ()
    Left problem -> assertFailure ("unexpected digest problem: " <> show problem)
    Right _ -> assertFailure "digest disagreement was accepted"

caseMalformedFutureCorrection :: Assertion
caseMalformedFutureCorrection = do
  fixture <- peerFixture
  wrongDigest <- checked "wrong future digest" (mkPeerItemDigest (ByteString.replicate 32 0xff))
  let second = nextStreamSequence firstStreamSequence
      malformed =
        sequencedItem
          fixture.direction
          second
          wrongDigest
          (presentedOrdinaryPeerPublication fixture.batch)
      corrected = ordinarySequencedItem fixture.direction second fixture.batch
  assertProtocolProblem
    "malformed future digest"
    isDigestMismatch
    (PeerInput.applyPeerPublication fixture.context fixture.binding malformed fixture.state)
  (held, result) <- applyFixture fixture corrected fixture.state
  assertEqual
    "the corrected item can occupy the formerly attempted sequence"
    (PeerInput.PeerInputReceived (ReceiveGap firstStreamSequence))
    (PeerInput.peerInputDisposition result)
  assertBool
    "semantic owners remain unchanged while the corrected item is held"
    ( PeerInput.peerInputPublicationState held == PeerInput.peerInputPublicationState fixture.state
        && PeerInput.peerInputStoreState held == PeerInput.peerInputStoreState fixture.state
    )
  where
    isDigestMismatch problem = case problem of
      PeerInput.PeerInputPublicationProblem Publication.IncomingPublicationDigestMismatch {} -> True
      _ -> False

caseProtocolViolationMatrix :: Assertion
caseProtocolViolationMatrix = do
  fixture <- peerFixture
  cases <- protocolViolationCases fixture
  mapM_ (runProtocolCase fixture firstStreamSequence) cases
  assertUnreachableProtocolDisposition fixture

-- | Every protocol category constructible through the opaque typed ingress.
-- Transition properties reuse these exact cases to guard the final close
-- policy, rather than testing only the coordinator's classification.
fixtureProtocolViolationItems :: IO [(String, SequencedItem PeerPublication)]
fixtureProtocolViolationItems = do
  fixture <- peerFixture
  cases <- protocolViolationCases fixture
  alternateDigest <-
    checked
      "alternate matrix digest"
      (mkPeerItemDigest (ByteString.replicate 32 0xff))
  let validDigest = ordinaryBatchDigest fixture.batch
      wrongDigest
        | alternateDigest /= validDigest = alternateDigest
        | otherwise =
            either
              (error . ("alternate zero digest: " <>) . show)
              id
              (mkPeerItemDigest (ByteString.replicate 32 0x00))
  pure
    ( [ (label, ordinarySequencedItem direction firstStreamSequence batch)
      | (label, direction, batch, _) <- cases
      ]
        <> [ ( "publication digest",
               sequencedItem
                 fixture.direction
                 firstStreamSequence
                 wrongDigest
                 (presentedOrdinaryPeerPublication fixture.batch)
             )
           ]
    )

protocolViolationCases ::
  PeerFixture ->
  IO
    [ ( String,
        StreamDirection,
        PublicationBatch,
        PeerInput.PeerInputProblem -> Bool
      )
    ]
protocolViolationCases fixture = do
  foreignEpoch <- checked "foreign HeraldEpoch" (mkHeraldEpoch (fixtureIdentifierBytes 230))
  unknownProcess <- checked "unknown ProcessEpochId" (mkProcessEpochId (fixtureIdentifierBytes 231))
  unknownNabla <- checked "unknown NablaId" (mkNablaId (fixtureIdentifierBytes 232))
  foreignOccurrence <-
    checked "foreign occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 233))
  neutralRoot <- requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap NeutralVertexRole
  wrongDestination <- checked "wrong destination" (mkDeltaId (fixtureIdentifierBytes 234))
  wrongIncarnation <- checked "wrong incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 235))

  let changedDirection source destination = checkedEither "changed direction" (mkStreamDirection source destination)
      changedBatch identifier source sortId occurrence value prerequisite destination =
        checkedEither
          "changed batch"
          ( mkPublicationBatch
              identifier
              source
              sortId
              occurrence
              value
              Normal
              (peerFixtureTopologyCut fixture)
              prerequisite
              (destination :| [])
          )
      identifierFor nabla authority source = publicationId nabla authority source (nablaSequence 0)
      validIdentifier = checkedPublicationId fixture.publication
      alternateDestination = publicationDestination wrongDestination wrongIncarnation Normal
      neutralDestination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) fixture.localEpoch NeutralVertexRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) fixture.localEpoch NeutralVertexRole)
          Normal
  pure
    [ ( "binding source",
        changedDirection foreignEpoch fixture.localEpoch,
        fixture.batch,
        isProtocol (\case PeerInput.PeerInputBindingSourceMismatch {} -> True; _ -> False)
      ),
      ( "binding destination",
        changedDirection fixture.remoteEpoch foreignEpoch,
        fixture.batch,
        isProtocol (\case PeerInput.PeerInputBindingDestinationMismatch {} -> True; _ -> False)
      ),
      ( "publication source epoch",
        fixture.direction,
        changedBatch
          (identifierFor fixture.sourceNabla fixture.sourceAuthority foreignEpoch)
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (publicationBatchCanonicalValue fixture.batch)
          fixture.controlPrerequisite
          fixture.destination,
        isProtocol (\case PeerInput.PeerInputPublicationSourceEpochMismatch {} -> True; _ -> False)
      ),
      ( "unknown source process",
        fixture.direction,
        changedBatch
          validIdentifier
          unknownProcess
          fixture.sortId
          fixture.occurrence
          (publicationBatchCanonicalValue fixture.batch)
          fixture.controlPrerequisite
          fixture.destination,
        -- An ordinary claim cannot attribute this static writer to the
        -- substituted process, so writer ownership is the earliest closed
        -- authority failure.
        isProtocol (\case PeerInput.PeerInputSourceWriterUnknown {} -> True; _ -> False)
      ),
      ( "source process residence",
        fixture.direction,
        changedBatch
          validIdentifier
          (appliedProcessEpochId fixture.localSource)
          fixture.sortId
          fixture.occurrence
          (publicationBatchCanonicalValue fixture.batch)
          fixture.controlPrerequisite
          fixture.destination,
        isProtocol (\case PeerInput.PeerInputSourceProcessResidenceMismatch {} -> True; _ -> False)
      ),
      ( "unknown source writer",
        fixture.direction,
        changedBatch
          (identifierFor unknownNabla fixture.sourceAuthority fixture.remoteEpoch)
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (publicationBatchCanonicalValue fixture.batch)
          fixture.controlPrerequisite
          fixture.destination,
        isProtocol (\case PeerInput.PeerInputSourceWriterUnknown {} -> True; _ -> False)
      ),
      ( "source authority",
        fixture.direction,
        changedBatch
          ( identifierFor
              fixture.sourceNabla
              ( structuralAuthorityEpoch
                  (structuralOccurrenceId fixture.remoteEpoch firstStructuralSequence)
                  fixture.topologyCut
              )
              fixture.remoteEpoch
          )
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (publicationBatchCanonicalValue fixture.batch)
          fixture.controlPrerequisite
          fixture.destination,
        isProtocol (\case PeerInput.PeerInputSourceAuthorityMismatch {} -> True; _ -> False)
      ),
      ( "source sort",
        fixture.direction,
        changedBatch
          validIdentifier
          fixture.sourceProcess
          (appliedRootSortId neutralRoot)
          fixture.occurrence
          (canonicalValueBytes (textValue "not a neutral vertex"))
          fixture.controlPrerequisite
          alternateDestination,
        isProtocol (\case PeerInput.PeerInputSourceSortMismatch {} -> True; _ -> False)
      ),
      ( "source occurrence",
        fixture.direction,
        changedBatch
          validIdentifier
          fixture.sourceProcess
          fixture.sortId
          foreignOccurrence
          (publicationBatchCanonicalValue fixture.batch)
          fixture.controlPrerequisite
          fixture.destination,
        isProtocol (\case PeerInput.PeerInputSourceOccurrenceMismatch {} -> True; _ -> False)
      ),
      ( "current destination sort",
        fixture.direction,
        changedBatch
          validIdentifier
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (publicationBatchCanonicalValue fixture.batch)
          fixture.controlPrerequisite
          neutralDestination,
        isProtocol (\case PeerInput.PeerInputCurrentDestinationSortMismatch {} -> True; _ -> False)
      )
    ]

assertUnreachableProtocolDisposition :: PeerFixture -> Assertion
assertUnreachableProtocolDisposition fixture = do
  wrongDestination <- checked "wrong destination" (mkDeltaId (fixtureIdentifierBytes 234))
  foreignOccurrence <-
    checked "foreign occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 233))
  neutralRoot <- requireWriterRoot fixture.remoteEpoch fixture.sourceBootstrap NeutralVertexRole
  let validIdentifier = checkedPublicationId fixture.publication
  -- These categories require checked owner facts to contradict one another, or
  -- a canonical value whose successful decoder does not round-trip. Source-root
  -- admission catches any claimed occurrence mismatch before registry lookup,
  -- and a current Store slot can only carry the genesis occurrence for its sort.
  -- They cannot be induced through today's opaque checked owners; retain their
  -- close policy so a representation change cannot silently drift the boundary.
  let unreachable =
        [ PeerInput.PeerInputEffectiveOccurrenceMismatch
            (appliedRootSortId neutralRoot)
            (appliedRootOccurrenceId neutralRoot)
            foreignOccurrence,
          PeerInput.PeerInputCurrentDestinationOccurrenceMismatch wrongDestination fixture.occurrence foreignOccurrence,
          PeerInput.PeerInputCanonicalValueMismatch validIdentifier,
          PeerInput.PeerInputTerminalStructuralSortUnknown fixture.sortId
        ]
  mapM_
    ( \violation ->
        assertEqual
          ("typed-unreachable protocol disposition: " <> show violation)
          PeerInput.RejectCurrentPeerBinding
          (PeerInput.peerInputProblemDisposition (PeerInput.PeerInputProtocolProblem violation))
    )
    unreachable
  assertEqual
    "a rejected semantic reassignment closes the current binding"
    PeerInput.RejectCurrentPeerBinding
    ( PeerInput.peerInputProblemDisposition
        ( PeerInput.PeerInputPublicationProblem
            ( Publication.IncomingPublicationRejectedSemanticReassignment
                (checkedPublicationId fixture.publication)
            )
        )
    )

runProtocolCase ::
  PeerFixture ->
  StreamSequence ->
  (String, StreamDirection, PublicationBatch, PeerInput.PeerInputProblem -> Bool) ->
  Assertion
runProtocolCase fixture sequenceNumber (label, direction, batch, matches) = do
  let item = ordinarySequencedItem direction sequenceNumber batch
  assertProtocolProblem label matches (PeerInput.applyPeerPublication fixture.context fixture.binding item fixture.state)

assertProtocolProblem ::
  String ->
  (PeerInput.PeerInputProblem -> Bool) ->
  Either PeerInput.PeerInputProblem value ->
  Assertion
assertProtocolProblem label matches result = case result of
  Left problem -> do
    assertBool (label <> ": wrong problem " <> show problem) (matches problem)
    assertEqual
      (label <> ": close disposition")
      PeerInput.RejectCurrentPeerBinding
      (PeerInput.peerInputProblemDisposition problem)
  Right _ -> assertFailure (label <> ": malformed input was admitted")

isProtocol ::
  (PeerInput.PeerInputProtocolViolation -> Bool) ->
  PeerInput.PeerInputProblem ->
  Bool
isProtocol matches problem = case problem of
  PeerInput.PeerInputProtocolProblem violation -> matches violation
  _ -> False

caseOwnerReceiptJoin :: Assertion
caseOwnerReceiptJoin = do
  fixture <- peerFixture
  let destination =
        Store.peerStoreDestination
          (publicationDestinationDelta fixture.destination)
          (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) fixture.localEpoch SortDefinitionRole)
          Normal
  prepared <-
    checked
      "preexisting owner receipt"
      ( Store.preparePeerStoreApplication
          ( Store.storeObservationEnvelope
              fixture.sourceProcess
              fixture.occurrence
              (peerFixtureTopologyCut fixture)
              fixture.controlPrerequisite
              Nothing
          )
          (destination :| [])
          fixture.publication
          (PeerInput.peerInputStoreState fixture.state)
      )
  let (storeWithReceipt, _) = Store.commitPeerStoreApplication prepared
      contradictoryState =
        PeerInput.peerInputState
          (PeerInput.peerInputPublicationState fixture.state)
          (PeerInput.peerInputPeerStreamState fixture.state)
          storeWithReceipt
          (PeerInput.peerInputGraphState fixture.state)
          (PeerInput.peerInputIdGeneratorState fixture.state)
          (PeerInput.peerInputStructuralProgressState fixture.state)
          (PeerInput.peerInputAlignmentState fixture.state)
          (PeerInput.peerInputPlacementState fixture.state)
          (PeerInput.peerInputControlledState fixture.state)
          (PeerInput.peerInputSortRegistryState fixture.state)
          (PeerInput.peerInputApplicationState fixture.state)
          (PeerInput.peerInputWaitState fixture.state)
          (PeerInput.peerInputLabelBarrierState fixture.state)
          (PeerInput.peerInputDisappearanceState fixture.state)
      weakDestination =
        publicationDestination
          (publicationDestinationDelta fixture.destination)
          (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) fixture.localEpoch SortDefinitionRole)
          Weak
  weakBatch <-
    checked
      "conflicting receipt batch"
      ( mkPublicationBatch
          (checkedPublicationId fixture.publication)
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (checkedPublicationCanonicalValue fixture.publication)
          Normal
          (peerFixtureTopologyCut fixture)
          fixture.controlPrerequisite
          (weakDestination :| [])
      )
  let item =
        ordinarySequencedItem
          fixture.direction
          firstStreamSequence
          weakBatch
  (successor, _) <-
    checked
      "weaker duplicate delivery"
      (PeerInput.applyPeerPublication fixture.context fixture.binding item contradictoryState)
  slot <- requireDestinationSlot fixture successor
  assertEqual
    "the stronger receipt is retained"
    (Just Normal)
    (lookup (checkedPublicationId fixture.publication) (Store.storeSlotApplicationReceipts slot))

caseStaleBinding :: Assertion
caseStaleBinding = do
  fixture <- peerFixture
  loss <-
    checked
      "binding loss"
      (DiscoveryState.prepareBindingLoss fixture.binding fixture.discovery)
  let (withoutBinding, wasCurrent) = DiscoveryState.commitBindingLoss loss
      staleContext =
        PeerInput.peerInputContext
          fixtureCheckedGenesis
          fixture.oracle
          withoutBinding
          fixture.placement
  assertBool "fixture binding was current" wasCurrent
  (successor, result) <-
    checked
      "stale peer input"
      ( PeerInput.applyPeerPublication
          staleContext
          fixture.binding
          fixture.item
          fixture.state
      )
  assertBool "stale observation is state-identical" (fixture.state == successor)
  assertEqual "stale disposition" PeerInput.PeerInputStaleBinding (PeerInput.peerInputDisposition result)
  assertEqual "stale observation emits no acknowledgement" [] (PeerInput.peerInputAcknowledgements result)

data PeerFixture = PeerFixture
  { genesis :: CheckedHeraldGenesis,
    context :: PeerInput.PeerInputContext,
    state :: PeerInput.PeerInputState,
    bootstraps :: CheckedInitialBootstraps,
    discovery :: DiscoveryState.State,
    placement :: Placement.State,
    oracle :: OracleProjection.State,
    sortRegistry :: SortRegistry.State,
    binding :: Discovery.PeerBinding,
    item :: SequencedItem PeerPublication,
    batch :: PublicationBatch,
    publication :: CheckedPublication,
    destination :: PublicationDestination,
    direction :: StreamDirection,
    sourceProcess :: ProcessEpochId,
    sourceNabla :: NablaId,
    sourceAuthority :: AuthorityEpoch,
    sourceBootstrap :: AppliedProcessBootstrap,
    localSource :: AppliedProcessBootstrap,
    localEpoch :: HeraldEpoch,
    remoteEpoch :: HeraldEpoch,
    sortId :: SortId,
    occurrence :: SortDefinitionOccurrenceId,
    controlPrerequisite :: ControlIndex,
    topologyCut :: TopologyCutId
  }

disappearanceMarkerSubject :: StreamDirection -> IO DomainDisappearance.DisappearanceSubject
disappearanceMarkerSubject direction = do
  object <- checked "marker subject object" (mkGlobalObjectId (fixtureIdentifierBytes 0xa7))
  checked
    "marker controlled subject"
    ( DomainDisappearance.controlledPredefinedDisappearanceSubject
        NeutralVertexRole
        object
        (structuralOccurrenceId (streamDirectionSource direction) firstStructuralSequence)
        Nothing
    )

disappearanceMarkerItem ::
  ControlIndex ->
  StreamDirection ->
  StreamSequence ->
  PeerInput.PeerInputState ->
  IO (SequencedItem PeerLogicalPayload)
disappearanceMarkerItem probeIndex direction sequenceNumber state = do
  probe <- checked "dependency-wake disappearance probe" (DomainDisappearance.deriveDisappearanceProbeId probeIndex)
  subject <- disappearanceMarkerSubject direction
  let progress = PeerInput.peerInputStructuralProgressState state
      coordinate =
        DomainDisappearance.admitDisappearanceSubjectMembershipCoordinate
          (DomainDisappearance.deriveDisappearanceSubjectDigest subject)
          (GraphProgress.structuralProgressMembershipGenerationId progress)
          (GraphProgress.structuralProgressMemberSetDigest progress)
      payload =
        PeerLogicalDisappearanceProbeMarker
          (DisappearanceProtocol.disappearancePublicationMarker probe coordinate direction sequenceNumber)
  pure (sequencedItem direction sequenceNumber (peerLogicalPayloadDigest payload) payload)

-- Isolated owner projection: these marker ordering tests exercise the real
-- checked probe/evidence transition, without inventing an Oracle input event.
projectRemoteDisappearanceProbe ::
  ControlIndex ->
  StreamDirection ->
  PeerFixture ->
  PeerInput.PeerInputState ->
  IO PeerInput.PeerInputState
projectRemoteDisappearanceProbe probeIndex direction fixture state = do
  probe <- checked "project dependency-wake probe identity" (DomainDisappearance.deriveDisappearanceProbeId probeIndex)
  subject <- disappearanceMarkerSubject direction
  let membership = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView fixture.oracle)
      coordinate = DomainDisappearance.disappearanceSubjectMembershipCoordinate subject membership
  projected <- checked "checked disappearance projection" (DisappearanceProtocol.projectedDisappearanceProbe probe subject coordinate membership)
  composed <- fullHeraldStateForPeerInput fixture fixture.oracle state
  snapshot <- checked "project dependency-wake probe evidence" (DisappearanceEvidence.disappearanceEvidenceSnapshotForHerald subject composed)
  prepared <- checked "project dependency-wake disappearance probe" (DisappearanceOwner.prepareProjectedOpen projected snapshot (PeerInput.peerInputDisappearanceState state))
  pure (replacePeerInputDisappearance (fst (DisappearanceOwner.commitDisappearanceTransition prepared)) state)

assertIncomingProgress ::
  String ->
  StreamDirection ->
  [PeerStream.IncomingItemStatus] ->
  PeerInput.PeerInputState ->
  Assertion
assertIncomingProgress context direction expected state = do
  retained <-
    checked
      context
      ( PeerStream.incomingRetainedItems
          direction
          (PeerInput.peerInputPeerStreamState state)
      )
  assertEqual context expected (fmap snd retained)

peerInputSliceForHerald ::
  HeraldState ->
  (PeerInput.PeerInputContext, PeerInput.PeerInputState)
peerInputSliceForHerald state =
  ( PeerInput.peerInputContext
      (startupGenesis state)
      (startupOracleProjectionState state)
      (startupDiscoveryState state)
      (startupPlacementState state),
    PeerInput.peerInputState
      (startupPublicationState state)
      (startupPeerStreamState state)
      (startupStoreState state)
      (startupGraphState state)
      (startupIdGeneratorState state)
      (startupStructuralProgressState state)
      (startupAlignmentState state)
      (startupPlacementState state)
      (startupControlledState state)
      (startupSortRegistryState state)
      (startupApplicationState state)
      (startupWaitState state)
      (startupLabelBarrierState state)
      (startupDisappearanceState state)
  )

-- Retain an unanswered cause query so marker-release tests detect accidental
-- installation of unchanged structural owners. No placement or historical
-- projection is needed by this marker-only scope.
withMarkerReleaseCutQueries :: HeraldState -> HeraldState
withMarkerReleaseCutQueries state =
  replaceStartupAlignmentCutQueryCache cache state
  where
    local = checkedLocalHeraldEpoch (startupGenesis state)
    cause = structuralOccurrenceCause (structuralOccurrenceId local firstStructuralSequence)
    cache =
      CutQueries.prepareCutQueryCache
        (Reconciliation.reconciliationViews local Map.empty Set.empty Map.empty Set.empty)
        (startupStructuralProgressState state)
        (startupPlacementState state)
        (Set.singleton cause)
        Set.empty
        Set.empty
        CutQueries.emptyCutQueryCache

fullHeraldStateForPeerControlItem ::
  PeerFixture ->
  OracleProjection.State ->
  PeerInput.PeerInputState ->
  IO HeraldState
fullHeraldStateForPeerControlItem fixture oracle peerInput = do
  (initial, _) <-
    checked
      "initialize composed peer-control-item fixture"
      ( initialHerald
          (monotonicInstant 0)
          fixture.genesis
          fixture.bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  outgoingDirection <-
    checked
      "compose peer-control-item dispatch direction"
      (mkStreamDirection fixture.localEpoch fixture.remoteEpoch)
  dispatchGeneration <-
    checked
      "compose peer-control-item dispatch generation"
      ( mkPeerDispatchBindingGeneration
          ( Discovery.peerBindingGenerationWord64
              (Discovery.peerBindingGeneration fixture.binding)
          )
      )
  preparedDispatch <-
    checked
      "compose peer-control-item dispatch binding"
      ( PeerStream.prepareDispatchBinding
          outgoingDirection
          dispatchGeneration
          (PeerInput.peerInputPeerStreamState peerInput)
      )
  let (peerStream, _) = PeerStream.commitDispatchBinding preparedDispatch
  pure
    ( replaceStartupLabelBarrierState
        (PeerInput.peerInputLabelBarrierState peerInput)
        . replaceStartupAlignmentState
          (PeerInput.peerInputAlignmentState peerInput)
        . replaceStartupPeerStreamState
          peerStream
        . replaceStartupDiscoveryState fixture.discovery
        . replaceStartupOracleProjectionState oracle
        $ initial
    )

fullHeraldStateForPeerInput ::
  PeerFixture ->
  OracleProjection.State ->
  PeerInput.PeerInputState ->
  IO HeraldState
fullHeraldStateForPeerInput fixture oracle peerInput = do
  (initial, _) <-
    checked
      "initialize composed pending-marker fixture"
      ( initialHerald
          (monotonicInstant 0)
          fixture.genesis
          fixture.bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  pure
    ( replaceStartupLabelBarrierState
        (PeerInput.peerInputLabelBarrierState peerInput)
        . replaceStartupWaitState
          (PeerInput.peerInputWaitState peerInput)
        . replaceStartupApplicationState
          (PeerInput.peerInputApplicationState peerInput)
        . replaceStartupSortRegistryState
          (PeerInput.peerInputSortRegistryState peerInput)
        . replaceStartupControlledState
          (PeerInput.peerInputControlledState peerInput)
        . replaceStartupPlacementState
          (PeerInput.peerInputPlacementState peerInput)
        . replaceStartupAlignmentState
          (PeerInput.peerInputAlignmentState peerInput)
        . replaceStartupStructuralProgressState
          (PeerInput.peerInputStructuralProgressState peerInput)
        . replaceStartupIdGeneratorState
          (PeerInput.peerInputIdGeneratorState peerInput)
        . replaceStartupGraphState
          (PeerInput.peerInputGraphState peerInput)
        . replaceStartupStoreState
          (PeerInput.peerInputStoreState peerInput)
        . replaceStartupPeerStreamState
          (PeerInput.peerInputPeerStreamState peerInput)
        . replaceStartupPublicationState
          (PeerInput.peerInputPublicationState peerInput)
        . replaceStartupDiscoveryState fixture.discovery
        . replaceStartupOracleProjectionState oracle
        $ initial
    )

replacePeerInputDisappearance ::
  DisappearanceOwner.State ->
  PeerInput.PeerInputState ->
  PeerInput.PeerInputState
replacePeerInputDisappearance disappearance state =
  PeerInput.peerInputState
    (PeerInput.peerInputPublicationState state)
    (PeerInput.peerInputPeerStreamState state)
    (PeerInput.peerInputStoreState state)
    (PeerInput.peerInputGraphState state)
    (PeerInput.peerInputIdGeneratorState state)
    (PeerInput.peerInputStructuralProgressState state)
    (PeerInput.peerInputAlignmentState state)
    (PeerInput.peerInputPlacementState state)
    (PeerInput.peerInputControlledState state)
    (PeerInput.peerInputSortRegistryState state)
    (PeerInput.peerInputApplicationState state)
    (PeerInput.peerInputWaitState state)
    (PeerInput.peerInputLabelBarrierState state)
    disappearance

replacePeerInputPeerStreamAndControlled ::
  PeerStream.State PeerLogicalPayload ->
  Controlled.State ->
  PeerInput.PeerInputState ->
  PeerInput.PeerInputState
replacePeerInputPeerStreamAndControlled peerStream controlled state =
  PeerInput.peerInputState
    (PeerInput.peerInputPublicationState state)
    peerStream
    (PeerInput.peerInputStoreState state)
    (PeerInput.peerInputGraphState state)
    (PeerInput.peerInputIdGeneratorState state)
    (PeerInput.peerInputStructuralProgressState state)
    (PeerInput.peerInputAlignmentState state)
    (PeerInput.peerInputPlacementState state)
    controlled
    (PeerInput.peerInputSortRegistryState state)
    (PeerInput.peerInputApplicationState state)
    (PeerInput.peerInputWaitState state)
    (PeerInput.peerInputLabelBarrierState state)
    (PeerInput.peerInputDisappearanceState state)

holdPeerFixtureOnDependency ::
  PeerFixture ->
  Publication.PublicationDependency ->
  IO PeerInput.PeerInputState
holdPeerFixtureOnDependency fixture dependency = do
  let item = fixture.item
      direction = sequencedItemDirection item
      sequenceNumber = sequencedItemSequence item
      outcomes =
        Map.fromList
          [ (destination, Publication.DestinationPending)
          | destination <- NonEmpty.toList (publicationBatchDestinations fixture.batch)
          ]
      membershipGenerationId =
        OracleProjection.oracleViewCurrentHeraldMembershipId
          (OracleProjection.oracleView fixture.oracle)
  resolution <-
    checked
      "retained publication dependency"
      ( Publication.incomingPublicationResolution
          (Set.singleton dependency)
          outcomes
      )
  publicationCandidate <-
    checked
      "retained publication candidate"
      ( Publication.prepareIncomingPublicationCandidate
          membershipGenerationId
          direction
          sequenceNumber
          (sequencedItemDigest item)
          (sequencedItemPayload item)
          (PeerInput.peerInputPublicationState fixture.state)
      )
  preparedPublication <-
    checked
      "retained dependency-held publication"
      (Publication.finalizeIncomingPublication (Just resolution) publicationCandidate)
  streamCandidate <-
    checked
      "retained publication stream candidate"
      ( PeerStream.prepareReceiveCandidate
          (logicalPublicationItem item)
          (PeerInput.peerInputPeerStreamState fixture.state)
      )
  preparedStream <-
    checked
      "retained publication stream"
      ( PeerStream.finalizeReceive
          (Map.singleton sequenceNumber ReceivedPending)
          streamCandidate
      )
  let (publication, _) = Publication.commitIncomingPublication preparedPublication
      (peerStream, _) = PeerStream.commitReceive preparedStream
  pure
    ( PeerInput.peerInputState
        publication
        peerStream
        (PeerInput.peerInputStoreState fixture.state)
        (PeerInput.peerInputGraphState fixture.state)
        (PeerInput.peerInputIdGeneratorState fixture.state)
        (PeerInput.peerInputStructuralProgressState fixture.state)
        (PeerInput.peerInputAlignmentState fixture.state)
        (PeerInput.peerInputPlacementState fixture.state)
        (PeerInput.peerInputControlledState fixture.state)
        (PeerInput.peerInputSortRegistryState fixture.state)
        (PeerInput.peerInputApplicationState fixture.state)
        (PeerInput.peerInputWaitState fixture.state)
        (PeerInput.peerInputLabelBarrierState fixture.state)
        (PeerInput.peerInputDisappearanceState fixture.state)
    )

replacePeerInputAlignment ::
  Alignment.State -> PeerInput.PeerInputState -> PeerInput.PeerInputState
replacePeerInputAlignment alignment state =
  PeerInput.peerInputState
    (PeerInput.peerInputPublicationState state)
    (PeerInput.peerInputPeerStreamState state)
    (PeerInput.peerInputStoreState state)
    (PeerInput.peerInputGraphState state)
    (PeerInput.peerInputIdGeneratorState state)
    (PeerInput.peerInputStructuralProgressState state)
    alignment
    (PeerInput.peerInputPlacementState state)
    (PeerInput.peerInputControlledState state)
    (PeerInput.peerInputSortRegistryState state)
    (PeerInput.peerInputApplicationState state)
    (PeerInput.peerInputWaitState state)
    (PeerInput.peerInputLabelBarrierState state)
    (PeerInput.peerInputDisappearanceState state)

-- A real retained successor generation whose frozen predecessor contains an
-- exact member hosted by the local peer.  The two returned states differ only
-- by the source Herald's immutable acceptance of the successor cut.
routeCutoverAlignmentFixture ::
  PeerFixture ->
  IO
    ( Alignment.State,
      Alignment.State,
      ContextClassGenerationId,
      ContextClassGenerationId,
      StoreIncarnationId
    )
routeCutoverAlignmentFixture fixture = do
  members <-
    case NonEmpty.nonEmpty
      (fmap heraldMemberEpoch (checkedActiveHeralds fixtureCheckedGenesis)) of
      Just retained -> pure retained
      Nothing -> assertFailure "route-cutover fixture has no active Heralds"
  membership <-
    checked
      "route-cutover membership generation"
      ( genesisHeraldMembershipGeneration
          (checkedSystemId fixtureCheckedGenesis)
          members
      )
  placementVector <-
    checked
      "route-cutover placement vector"
      ( physicalPlacementRevisionVector
          membership
          [ (member, firstPlacementRevision)
          | member <- NonEmpty.toList members
          ]
      )
  deltaLocal <- checked "route local delta" (mkDeltaId (fixtureIdentifierBytes 0xeb))
  deltaRemote <- checked "route remote delta" (mkDeltaId (fixtureIdentifierBytes 0xec))
  storeLocal <-
    checked "route local store" (mkStoreIncarnationId (fixtureIdentifierBytes 0xed))
  storeRemote <-
    checked "route remote store" (mkStoreIncarnationId (fixtureIdentifierBytes 0xee))
  context <-
    checked
      "route-cutover context graph"
      ( deriveContextGraph
          [DeltaVertex deltaLocal, DeltaVertex deltaRemote]
          [ edgePayload (DeltaVertex deltaLocal) (DeltaVertex deltaRemote) Preserve,
            edgePayload (DeltaVertex deltaRemote) (DeltaVertex deltaLocal) Preserve
          ]
          [deltaLocal, deltaRemote]
      )
  predecessorTopologyId <-
    checked
      "route-cutover topology predecessor"
      (mkTopologyCutId (fixtureIdentifierBytes 0xef))
  let emptyVector = emptyStructuralVersionVector membership
      successorVector = emptyStructuralVersionVector membership
  firstSequence <-
    checked "route-cutover structural sequence" (mkStructuralSequence 41)
  successorSequence <-
    checked "route-cutover structural sequence" (mkStructuralSequence 42)
  let generationMembers =
        [ generationMemberInput
            deltaLocal
            storeLocal
            fixture.localEpoch
            initialStoreRevision,
          generationMemberInput
            deltaRemote
            storeRemote
            fixture.remoteEpoch
            initialStoreRevision
        ]
      affectedSort = sortOccurrence fixture.sortId fixture.occurrence
  firstTopology <-
    checked
      "route-cutover first topology"
      ( topologyCut
          (sameGenerationPredecessor predecessorTopologyId)
          (topologyFrontier emptyVector (controlIndex 0))
          (deriveTopologyOccurrenceDigest "route-cutover-first")
      )
  let firstTopologyId = deriveTopologyCutId firstTopology
  successorTopology <-
    checked
      "route-cutover successor topology"
      ( topologyCut
          (sameGenerationPredecessor firstTopologyId)
          (topologyFrontier successorVector (controlIndex 0))
          (deriveTopologyOccurrenceDigest "route-cutover-successor")
      )
  let
    successorTopologyId = deriveTopologyCutId successorTopology
    firstOccurrence = structuralOccurrenceId fixture.remoteEpoch firstSequence
    successorOccurrence = structuralOccurrenceId fixture.remoteEpoch successorSequence
    planFor topology predecessors certificates =
      case prepareAlignmentGenerationPlan
        affectedSort
        topology
        placementVector
        context
        generationMembers
        predecessors
        []
        certificates of
        Right (AlignmentGenerationReady plan) -> pure plan
        result -> assertFailure ("route-cutover generation plan held: " <> show result)
    onlyGeneration label plan =
      case alignmentGenerationPlanGenerations plan of
        [generation] -> pure generation
        generations ->
          assertFailure
            (label <> " expected one context class, got " <> show (length generations))
    retainDebt occurrence state = do
      prepared <-
        checked
          "retain route-cutover structural debt"
          ( Alignment.prepareStructuralDebtRetention
              (structuralOccurrenceCause occurrence)
              ( normalizeStructuralDebts
                  [ structuralConsequenceDebt
                      ( structuralDebtKey
                          (structuralOccurrenceCause occurrence)
                          TopologyAlignmentDebt
                          affectedSort
                          Nothing
                      )
                      (structuralDebtEvidence Set.empty Set.empty Set.empty)
                  ]
              )
              state
          )
      pure (fst (Alignment.commitStructuralDebtRetention prepared))
  firstPlan <- planFor firstTopology [] Set.empty
  predecessorGeneration <- onlyGeneration "route-cutover predecessor" firstPlan
  withFirstDebt <- retainDebt firstOccurrence Alignment.emptyState
  predecessorPromotion <-
    checked
      "promote route-cutover predecessor"
      ( Alignment.prepareAlignmentPromotion
          fixture.localEpoch
          (structuralOccurrenceCause firstOccurrence)
          affectedSort
          firstTopologyId
          placementVector
          firstPlan
          withFirstDebt
      )
  let (promotedPredecessor, _) = Alignment.commitAlignmentPromotion predecessorPromotion
      predecessorId = alignmentGenerationId predecessorGeneration
      predecessorAcceptance =
        alignmentCutAccepted
          predecessorId
          fixture.remoteEpoch
          firstTopologyId
          placementVector
          EmptyHeraldPublicationPrefix
  retainedPredecessorAcceptance <-
    checked
      "retain predecessor source acceptance"
      ( Alignment.prepareAlignmentCutAcceptance
          predecessorAcceptance
          promotedPredecessor
      )
  let (predecessorAccepted, _) =
        Alignment.commitAlignmentCutAcceptance retainedPredecessorAcceptance
      localReady =
        classMemberReady
          firstAlignmentEvidenceSequence
          predecessorId
          storeLocal
          (deriveMemberReadyEvidenceDigest "route-local-ready")
          initialStoreRevision
      remoteReady =
        classMemberReady
          (nextAlignmentEvidenceSequence firstAlignmentEvidenceSequence)
          predecessorId
          storeRemote
          (deriveMemberReadyEvidenceDigest "route-remote-ready")
          initialStoreRevision
  retainedLocalReady <-
    checked
      "retain predecessor local readiness"
      (Alignment.prepareClassMemberReady localReady predecessorAccepted)
  let (withLocalReady, _) = Alignment.commitClassMemberReady retainedLocalReady
  retainedRemoteReady <-
    checked
      "retain predecessor remote readiness"
      (Alignment.prepareClassMemberReady remoteReady withLocalReady)
  let (withRemoteReady, _) = Alignment.commitClassMemberReady retainedRemoteReady
      remoteCertificate =
        historicalCertificate
          predecessorId
          storeRemote
          (classMemberReadySetDigest [localReady, remoteReady])
          ( deriveBootstrapEvidenceDigest
              ( memberReadyEvidenceDigestBytes
                  (classMemberReadyPredecessorAndBasePrefixDigest remoteReady)
              )
          )
          initialStoreRevision
  retainedRemoteCertificate <-
    checked
      "retain predecessor remote certificate"
      (Alignment.prepareHistoricalCertificate remoteCertificate withRemoteReady)
  let (certifiedPredecessor, _) =
        Alignment.commitHistoricalCertificate retainedRemoteCertificate
  successorPlan <-
    planFor
      successorTopology
      [predecessorGeneration]
      (Set.singleton (alignmentGenerationId predecessorGeneration))
  successorGeneration <- onlyGeneration "route-cutover successor" successorPlan
  withSuccessorDebt <- retainDebt successorOccurrence certifiedPredecessor
  successorPromotion <-
    checked
      "promote route-cutover successor"
      ( Alignment.prepareAlignmentPromotion
          fixture.localEpoch
          (structuralOccurrenceCause successorOccurrence)
          affectedSort
          successorTopologyId
          placementVector
          successorPlan
          withSuccessorDebt
      )
  let (promoted, _) = Alignment.commitAlignmentPromotion successorPromotion
      generationId = alignmentGenerationId successorGeneration
      destinationAcceptance =
        alignmentCutAccepted
          generationId
          fixture.localEpoch
          successorTopologyId
          placementVector
          EmptyHeraldPublicationPrefix
  retainedDestinationAcceptance <-
    checked
      "retain route-cutover destination acceptance"
      (Alignment.prepareAlignmentCutAcceptance destinationAcceptance promoted)
  let (destinationAccepted, _) =
        Alignment.commitAlignmentCutAcceptance retainedDestinationAcceptance
      sourceAcceptance =
        alignmentCutAccepted
          generationId
          fixture.remoteEpoch
          successorTopologyId
          placementVector
          EmptyHeraldPublicationPrefix
  retainedSourceAcceptance <-
    checked
      "retain route-cutover source acceptance"
      (Alignment.prepareAlignmentCutAcceptance sourceAcceptance destinationAccepted)
  let (accepted, _) = Alignment.commitAlignmentCutAcceptance retainedSourceAcceptance
  pure (promoted, accepted, generationId, predecessorId, storeRemote)

data ControlledFixture = ControlledFixture
  { peer :: PeerFixture,
    acceptedOracle :: OracleProjection.State,
    initialContext :: PeerInput.PeerInputContext,
    acceptedContext :: PeerInput.PeerInputContext,
    rejectedContext :: PeerInput.PeerInputContext,
    initialState :: PeerInput.PeerInputState,
    binding :: Discovery.PeerBinding,
    item :: SequencedItem PeerPublication,
    batch :: PublicationBatch,
    identifier :: PublicationId,
    object :: GlobalObjectId,
    destination :: PublicationDestination,
    membershipGenerationId :: HeraldMembershipGenerationId
  }

data ActiveDeltaFixture = ActiveDeltaFixture
  { context :: PeerInput.PeerInputContext,
    state :: PeerInput.PeerInputState,
    bootstraps :: CheckedInitialBootstraps,
    binding :: Discovery.PeerBinding,
    item :: SequencedItem PeerPublication,
    invalidDestinationItems :: [(String, SequencedItem PeerPublication)],
    weakItem :: SequencedItem PeerPublication,
    readerItem :: SequencedItem PeerPublication,
    readerDestination :: PublicationDestination,
    identifier :: PublicationId,
    delta :: DeltaId,
    expectedIncarnation :: StoreIncarnationId
  }

data TerminalEdgeFixture = TerminalEdgeFixture
  { context :: PeerInput.PeerInputContext,
    state :: PeerInput.PeerInputState,
    binding :: Discovery.PeerBinding,
    item :: SequencedItem PeerPublication,
    object :: GlobalObjectId,
    cause :: StructuralConsequenceCause
  }

activeDeltaFixture :: IO ActiveDeltaFixture
activeDeltaFixture = peerFixture >>= activeDeltaFixtureFrom

terminalEdgeFixture :: IO TerminalEdgeFixture
terminalEdgeFixture = do
  base <- peerFixture
  root <- requireWriterRoot base.remoteEpoch base.sourceBootstrap EdgeRole
  object <- checked "terminal Edge object" (mkGlobalObjectId (fixtureIdentifierBytes 0xd8))
  objectField <- checked "Edge object field" (mkFieldName "object_id")
  labelField <- checked "Edge label field" (mkFieldName "label")
  sourceField <- checked "Edge source field" (mkFieldName "source_vertex")
  destinationField <- checked "Edge destination field" (mkFieldName "destination_vertex")
  strengthField <- checked "Edge strength field" (mkFieldName "strength")
  (sourceObject, destinationObject) <-
    case fmap appliedRootObjectId (appliedProcessRoots base.localSource) of
      source : destination : _ -> pure (source, destination)
      _ -> assertFailure "terminal Edge fixture has fewer than two genesis vertices"
  value <-
    checked
      "terminal Edge value"
      ( recordValue
          [ (objectField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
            (labelField, labelValue (VoidLabel, 0)),
            (sourceField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId sourceObject)),
            (destinationField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId destinationObject)),
            (strengthField, enumValue (edgeStrengthSymbol Preserve))
          ]
      )
  let sourceNabla = case appliedRootRole root of
        WriterRoot nabla _ -> nabla
        _ -> error "requireWriterRoot returned a reader"
      identifier =
        publicationId
          sourceNabla
          (appliedRootAuthority root)
          base.remoteEpoch
          (nablaSequence 1)
      descriptor = predefinedCatalogueDescriptor (profileEntryFor EdgeRole)
      destination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId base.genesis) base.localEpoch EdgeRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId base.genesis) base.localEpoch EdgeRole)
          Normal
  publication <-
    checked
      "terminal Edge publication"
      (mkCheckedPublication descriptor identifier value)
  observation <-
    checked
      "terminal Edge controlled observation"
      ( Controlled.checkControlledObservation
          descriptor
          (appliedRootOccurrenceId root)
          publication
      )
  preparedObservation <-
    checked
      "seed terminal Edge controlled winner"
      ( Controlled.prepareControlledPeerObservation
          observation
          (PeerInput.peerInputControlledState base.state)
      )
  let (withWinner, _) = Controlled.commitControlledPeerObservation preparedObservation
      seeded = replacePeerInputControlled withWinner base.state
  decision <-
    checked
      "terminal Edge label decision"
      (mkLabelDecisionId (fixtureIdentifierBytes 0xd9))
  (cause, deleted) <-
    installTerminalLabelDeletion base.bootstraps decision object seeded
  batch <-
    checked
      "terminal Edge batch"
      ( mkPublicationBatch
          identifier
          base.sourceProcess
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          (checkedPublicationCanonicalValue publication)
          Normal
          base.topologyCut
          (appliedRootControlPrerequisite root)
          (destination :| [])
      )
  structuralDigest <-
    checked
      "terminal Edge structural digest"
      ( structuralPublicationDigestFor
          descriptor
          (appliedRootOccurrenceId root)
          publication
          batch
      )
  stamp <-
    checked
      "terminal Edge structural stamp"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceId base.remoteEpoch firstStructuralSequence)
          ( GraphProgress.structuralAppliedVector
              (PeerInput.peerInputStructuralProgressState deleted)
          )
          identifier
          structuralDigest
          EdgeCarrier
      )
  peerPublication <-
    checked
      "terminal Edge peer publication"
      ( mkStructuralPeerPublication
          descriptor
          (appliedRootOccurrenceId root)
          publication
          stamp
          batch
      )
  pure
    TerminalEdgeFixture
      { context = base.context,
        state = deleted,
        binding = base.binding,
        item =
          sequencedItem
            base.direction
            firstStreamSequence
            (peerPublicationDigest peerPublication)
            peerPublication,
        object,
        cause
      }

activeDeltaFixtureFrom :: PeerFixture -> IO ActiveDeltaFixture
activeDeltaFixtureFrom base = do
  sourceRoot <- requireWriterRoot base.remoteEpoch base.sourceBootstrap DeltaRole
  delta <- checked "active application Delta" (mkDeltaId (fixtureIdentifierBytes 0xc1))
  possessionDelta <- checked "active Delta possession reader" (mkDeltaId (fixtureIdentifierBytes 0xc2))
  objectField <- checked "Delta object field" (mkFieldName "object_id")
  labelField <- checked "Delta label field" (mkFieldName "label")
  sortField <- checked "Delta sort field" (mkFieldName "sort_id")
  let process = appliedProcessEpochId base.localSource
      object = globalObjectIdFromDeltaId delta
      valueResult =
        recordValue
          [ (objectField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
            (labelField, labelValue ((ProcessLabel process, 0))),
            (sortField, bytesValue (sortIdBytes base.sortId))
          ]
  value <- checked "active Delta value" valueResult
  let sourceNabla = case appliedRootRole sourceRoot of
        WriterRoot nabla _ -> nabla
        _ -> error "requireWriterRoot returned a reader"
      descriptor = predefinedCatalogueDescriptor (profileEntryFor DeltaRole)
      seedIdentifier =
        publicationId
          sourceNabla
          (appliedRootAuthority sourceRoot)
          base.remoteEpoch
          (nablaSequence 0)
  seedPublication <-
    checked
      "seed active Delta publication"
      (mkCheckedPublication descriptor seedIdentifier value)
  seedObservation <-
    checked
      "seed active Delta observation"
      ( Controlled.checkControlledObservation
          descriptor
          (appliedRootOccurrenceId sourceRoot)
          seedPublication
      )
  bootstrapControlled <-
    Controlled.commitControlledBootstrap
      <$> checked
        "active Delta possession bootstrap"
        ( Controlled.prepareControlledBootstrap
            ( Controlled.processFact
                (appliedProcessId base.localSource)
                process
                base.localEpoch
                (appliedProcessAuthority base.localSource)
            )
            [ Controlled.readerRootFact
                process
                DeltaRole
                (appliedRootSortId sourceRoot)
                (appliedRootOccurrenceId sourceRoot)
                (appliedProcessAuthority base.localSource)
                (appliedRootControlPrerequisite sourceRoot)
                possessionDelta
            ]
            Controlled.emptyState
        )
  preparedSeed <-
    checked
      "retain current Delta-role winner"
      (Controlled.prepareControlledPeerObservation seedObservation bootstrapControlled)
  let (withWinner, _) = Controlled.commitControlledPeerObservation preparedSeed
  preparedPossession <-
    checked
      "retain active Delta normal possession"
      ( Controlled.prepareControlledStoreRead
          process
          possessionDelta
          Normal
          seedObservation
          withWinner
      )
  let controlled = Controlled.commitControlledStoreObservation preparedPossession
      identifier =
        publicationId
          sourceNabla
          (appliedRootAuthority sourceRoot)
          base.remoteEpoch
          (nablaSequence 1)
      destination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId base.genesis) base.localEpoch DeltaRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId base.genesis) base.localEpoch DeltaRole)
          Normal
  publication <-
    checked
      "active Delta publication"
      (mkCheckedPublication descriptor identifier value)
  batch <-
    checked
      "active Delta batch"
      ( mkPublicationBatch
          identifier
          base.sourceProcess
          (appliedRootSortId sourceRoot)
          (appliedRootOccurrenceId sourceRoot)
          (checkedPublicationCanonicalValue publication)
          Normal
          base.topologyCut
          (appliedRootControlPrerequisite sourceRoot)
          (destination :| [])
      )
  structuralDigest <-
    checked
      "active Delta structural digest"
      ( structuralPublicationDigestFor
          descriptor
          (appliedRootOccurrenceId sourceRoot)
          publication
          batch
      )
  stamp <-
    checked
      "active Delta structural stamp"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceId base.remoteEpoch firstStructuralSequence)
          ( GraphProgress.structuralAppliedVector
              (PeerInput.peerInputStructuralProgressState base.state)
          )
          identifier
          structuralDigest
          DeltaCarrier
      )
  peerPublication <-
    checked
      "active Delta peer publication"
      ( mkStructuralPeerPublication
          descriptor
          (appliedRootOccurrenceId sourceRoot)
          publication
          stamp
          batch
      )
  readerDelta <- checked "retained structural reader Delta" (mkDeltaId (fixtureIdentifierBytes 0xc3))
  readerIncarnation <-
    checked
      "retained structural reader incarnation"
      (mkStoreIncarnationId (fixtureIdentifierBytes 0xc4))
  fabricatedDelta <-
    checked "fabricated structural reader Delta" (mkDeltaId (fixtureIdentifierBytes 0xc5))
  fabricatedIncarnation <-
    checked
      "fabricated structural reader incarnation"
      (mkStoreIncarnationId (fixtureIdentifierBytes 0xc6))
  wrongIncarnation <-
    checked
      "wrong structural destination incarnation"
      (mkStoreIncarnationId (fixtureIdentifierBytes 0xc7))
  let readerRoute =
        PlacementMessage.applicationDeltaRoute
          readerDelta
          (appliedRootSortId sourceRoot)
          (appliedRootOccurrenceId sourceRoot)
          (globalObjectIdFromDeltaId readerDelta)
          process
          readerIncarnation
          (appliedRootControlPrerequisite sourceRoot)
  retainedReaderSnapshot <-
    checked
      "retain one cut-qualified structural reader route"
      ( Placement.prepareLocalSnapshotForRoutes
          [readerRoute]
          (PeerInput.peerInputPlacementState base.state)
      )
  let (withReaderHistory, _) = Placement.commitLocalSnapshot retainedReaderSnapshot
  withdrawnReaderSnapshot <-
    checked
      "withdraw the retained structural reader route"
      (Placement.prepareLocalSnapshotForRoutes [] withReaderHistory)
  let (placement, _) = Placement.commitLocalSnapshot withdrawnReaderSnapshot
      context =
        PeerInput.peerInputContext
          base.genesis
          base.oracle
          base.discovery
          placement
      readerDestination =
        publicationDestination
          readerDelta
          readerIncarnation
          Normal
      fabricatedDestination =
        publicationDestination
          fabricatedDelta
          fabricatedIncarnation
          Normal
      wrongIncarnationDestination =
        publicationDestination
          (publicationDestinationDelta destination)
          wrongIncarnation
          Normal
      wrongStrengthDestination =
        publicationDestination
          (publicationDestinationDelta destination)
          (publicationDestinationStoreIncarnation destination)
          Weak
      otherLocalSystemViewDestination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId base.genesis) base.localEpoch EdgeRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId base.genesis) base.localEpoch EdgeRole)
          Normal
      foreignSystemViewDestination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId base.genesis) base.remoteEpoch DeltaRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId base.genesis) base.remoteEpoch DeltaRole)
          Normal
      makeBatch label strength destinations =
        checked
          label
          ( mkPublicationBatch
              identifier
              base.sourceProcess
              (appliedRootSortId sourceRoot)
              (appliedRootOccurrenceId sourceRoot)
              (checkedPublicationCanonicalValue publication)
              strength
              base.topologyCut
              (appliedRootControlPrerequisite sourceRoot)
              destinations
          )
  wrongDeltaBatch <-
    makeBatch "wrong structural destination Delta batch" Normal (fabricatedDestination :| [])
  wrongIncarnationBatch <-
    makeBatch
      "wrong structural destination incarnation batch"
      Normal
      (wrongIncarnationDestination :| [])
  wrongStrengthBatch <-
    makeBatch
      "wrong structural destination strength batch"
      Normal
      (wrongStrengthDestination :| [])
  extraLocalSystemViewBatch <-
    makeBatch
      "extra local structural system-view destination batch"
      Normal
      (destination :| [otherLocalSystemViewDestination])
  extraForeignSystemViewBatch <-
    makeBatch
      "extra foreign structural system-view destination batch"
      Normal
      (destination :| [foreignSystemViewDestination])
  fabricatedReaderBatch <-
    makeBatch
      "fabricated application-reader destination batch"
      Normal
      (destination :| [fabricatedDestination])
  readerDestinationBatch <-
    makeBatch
      "frozen application-reader destination batch"
      Normal
      (destination :| [readerDestination])
  invalidDestinationItems <-
    traverse
      ( \(label, invalidBatch) -> do
          invalidPeerPublication <-
            checked
              label
              ( mkStructuralPeerPublication
                  descriptor
                  (appliedRootOccurrenceId sourceRoot)
                  publication
                  stamp
                  invalidBatch
              )
          pure
            ( label,
              sequencedItem
                base.direction
                firstStreamSequence
                (peerPublicationDigest invalidPeerPublication)
                invalidPeerPublication
            )
      )
      [ ("wrong structural destination Delta", wrongDeltaBatch),
        ("wrong structural destination incarnation", wrongIncarnationBatch),
        ("wrong structural destination strength", wrongStrengthBatch),
        ("extra local structural system-view destination", extraLocalSystemViewBatch),
        ("extra foreign structural system-view destination", extraForeignSystemViewBatch),
        ("fabricated application-reader destination", fabricatedReaderBatch)
      ]
  let weakDestination =
        publicationDestination
          (publicationDestinationDelta destination)
          (publicationDestinationStoreIncarnation destination)
          Weak
  weakBatch <-
    makeBatch
      "source-attenuated structural batch"
      Weak
      (weakDestination :| [])
  weakStructuralDigest <-
    checked
      "source-attenuated structural digest"
      ( structuralPublicationDigestFor
          descriptor
          (appliedRootOccurrenceId sourceRoot)
          publication
          weakBatch
      )
  weakStamp <-
    checked
      "source-attenuated structural stamp"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceId base.remoteEpoch firstStructuralSequence)
          ( GraphProgress.structuralAppliedVector
              (PeerInput.peerInputStructuralProgressState base.state)
          )
          identifier
          weakStructuralDigest
          DeltaCarrier
      )
  weakPeerPublication <-
    checked
      "source-attenuated structural peer publication"
      ( mkStructuralPeerPublication
          descriptor
          (appliedRootOccurrenceId sourceRoot)
          publication
          weakStamp
          weakBatch
      )
  readerStructuralDigest <-
    checked
      "frozen application-reader structural digest"
      ( structuralPublicationDigestFor
          descriptor
          (appliedRootOccurrenceId sourceRoot)
          publication
          readerDestinationBatch
      )
  readerStamp <-
    checked
      "frozen application-reader structural stamp"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceId base.remoteEpoch firstStructuralSequence)
          ( GraphProgress.structuralAppliedVector
              (PeerInput.peerInputStructuralProgressState base.state)
          )
          identifier
          readerStructuralDigest
          DeltaCarrier
      )
  readerPeerPublication <-
    checked
      "frozen application-reader structural peer publication"
      ( mkStructuralPeerPublication
          descriptor
          (appliedRootOccurrenceId sourceRoot)
          publication
          readerStamp
          readerDestinationBatch
      )
  generated <-
    checked
      "first receiver-generated identity"
      ( IdGenerator.prepareGeneratedId
          (PeerInput.peerInputIdGeneratorState base.state)
      )
  expectedIncarnation <-
    checked
      "receiver-generated Store incarnation"
      ( mkStoreIncarnationId
          (globalUniqueIdBytes (IdGenerator.preparedGlobalUniqueId generated))
      )
  let state =
        PeerInput.peerInputState
          (PeerInput.peerInputPublicationState base.state)
          (PeerInput.peerInputPeerStreamState base.state)
          (PeerInput.peerInputStoreState base.state)
          (PeerInput.peerInputGraphState base.state)
          (PeerInput.peerInputIdGeneratorState base.state)
          (PeerInput.peerInputStructuralProgressState base.state)
          (PeerInput.peerInputAlignmentState base.state)
          placement
          controlled
          (PeerInput.peerInputSortRegistryState base.state)
          (PeerInput.peerInputApplicationState base.state)
          (PeerInput.peerInputWaitState base.state)
          (PeerInput.peerInputLabelBarrierState base.state)
          (PeerInput.peerInputDisappearanceState base.state)
      item =
        sequencedItem
          base.direction
          firstStreamSequence
          (peerPublicationDigest peerPublication)
          peerPublication
      weakItem =
        sequencedItem
          base.direction
          firstStreamSequence
          (peerPublicationDigest weakPeerPublication)
          weakPeerPublication
      readerItem =
        sequencedItem
          base.direction
          firstStreamSequence
          (peerPublicationDigest readerPeerPublication)
          readerPeerPublication
  pure
    ActiveDeltaFixture
      { context,
        state,
        bootstraps = base.bootstraps,
        binding = base.binding,
        item,
        invalidDestinationItems,
        weakItem,
        readerItem,
        readerDestination,
        identifier,
        delta,
        expectedIncarnation
      }

fixtureSuccessorGenerationStructuralItem :: IO (SequencedItem PeerPublication)
fixtureSuccessorGenerationStructuralItem = do
  base <-
    peerFixtureFor
      fixtureStep14CheckedGenesis
      (firstLocalBootstrapId : [fixtureRemoteBootstrapId])
      fixtureRemoteMember
  (.item) <$> activeDeltaFixtureFrom base

fixtureSuccessorGenerationOrdinaryItem :: IO (SequencedItem PeerPublication)
fixtureSuccessorGenerationOrdinaryItem =
  (.item)
    <$> peerFixtureFor
      fixtureStep14CheckedGenesis
      (firstLocalBootstrapId : [fixtureRemoteBootstrapId])
      fixtureRemoteMember

controlledFixture :: IO ControlledFixture
controlledFixture = do
  base <- peerFixture
  root <- requireWriterRoot base.remoteEpoch base.sourceBootstrap NeutralVertexRole
  object <- checked "controlled neutral object" (mkGlobalObjectId (fixtureIdentifierBytes 0xa9))
  objectField <- checked "neutral object field" (mkFieldName "object_id")
  labelField <- checked "neutral label field" (mkFieldName "label")
  value <-
    checked
      "controlled neutral value"
      ( recordValue
          [ (objectField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
            (labelField, labelValue (VoidLabel, 0))
          ]
      )
  let sourceNabla = case appliedRootRole root of
        WriterRoot nabla _ -> nabla
        _ -> error "requireWriterRoot returned a reader"
      identifier =
        publicationId
          sourceNabla
          (appliedRootAuthority root)
          base.remoteEpoch
          (nablaSequence 1)
      descriptor = predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole)
      destination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) base.localEpoch NeutralVertexRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) base.localEpoch NeutralVertexRole)
          Normal
  publication <-
    checked
      "controlled neutral publication"
      (mkCheckedPublication descriptor identifier value)
  batch <-
    checked
      "controlled neutral batch"
      ( mkPublicationBatch
          identifier
          base.sourceProcess
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          (checkedPublicationCanonicalValue publication)
          Normal
          base.topologyCut
          (controlIndex 1)
          (destination :| [])
      )
  structuralDigest <-
    checked
      "controlled neutral structural digest"
      ( structuralPublicationDigestFor
          descriptor
          (appliedRootOccurrenceId root)
          publication
          batch
      )
  stamp <-
    checked
      "controlled neutral structural stamp"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceId base.remoteEpoch firstStructuralSequence)
          ( GraphProgress.structuralAppliedVector
              (PeerInput.peerInputStructuralProgressState base.state)
          )
          identifier
          structuralDigest
          NeutralVertexCarrier
      )
  peerPublication <-
    checked
      "controlled neutral peer publication"
      ( mkStructuralPeerPublication
          descriptor
          (appliedRootOccurrenceId root)
          publication
          stamp
          batch
      )
  startBootstrap <-
    maybe
      (assertFailure "missing configured process reserved for live Start")
      pure
      (lookupConfiguredProcessBootstrap secondLocalBootstrapId fixtureCheckedGenesis)
  let item =
        sequencedItem
          base.direction
          firstStreamSequence
          (peerPublicationDigest peerPublication)
          peerPublication
      acceptedProjection =
        projectedAfterEnvelope base.bootstraps (startEnvelope base startBootstrap Nothing)
      rejectedProjection =
        projectedAfterEnvelope
          base.bootstraps
          (startEnvelope base startBootstrap (Just (controlIndex 99)))
      contextFor projection =
        PeerInput.peerInputContext
          fixtureCheckedGenesis
          projection
          base.discovery
          base.placement
  pure
    ControlledFixture
      { peer = base,
        acceptedOracle = acceptedProjection,
        initialContext = base.context,
        acceptedContext = contextFor acceptedProjection,
        rejectedContext = contextFor rejectedProjection,
        initialState = base.state,
        binding = base.binding,
        item,
        batch,
        identifier,
        object,
        destination,
        membershipGenerationId =
          OracleProjection.oracleViewCurrentHeraldMembershipId
            (OracleProjection.oracleView base.oracle)
      }

startEnvelope ::
  PeerFixture ->
  ConfiguredProcessBootstrap ->
  Maybe ControlIndex ->
  OracleEnvelope
startEnvelope fixture bootstrap expected =
  oracleEnvelope
    (oracleClientRequestId fixture.localEpoch 1)
    expected
    fixture.localEpoch
    (fixtureStartCommand bootstrap)

projectedAfterEnvelope ::
  CheckedInitialBootstraps ->
  OracleEnvelope ->
  OracleProjection.State
projectedAfterEnvelope bootstraps envelope =
  OracleProjection.commitAppliedEntry prepared
  where
    checkedGenesis = checkedOracleGenesis
    initialOracleState = checkedEither "initial Oracle" (initialOracle checkedGenesis)
    (_, entry) = committedOracleEntry initialOracleState envelope
    canonical = canonicalizeAppliedOracleEntry entry
    initialProjection = OracleProjection.initialState fixtureCheckedGenesis bootstraps
    prepared =
      checkedEither
        "prepare controlled projection entry"
        (OracleProjection.prepareAppliedEntry canonical initialProjection)

projectedAfterEnvelopeFor ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleEnvelope ->
  OracleProjection.State
projectedAfterEnvelopeFor genesis bootstraps envelope =
  OracleProjection.commitAppliedEntry prepared
  where
    checkedGenesis = checkedOracleGenesisFor genesis bootstraps
    initialOracleState = checkedEither "causal initial Oracle" (initialOracle checkedGenesis)
    (_, entry) = committedOracleEntry initialOracleState envelope
    canonical = canonicalizeAppliedOracleEntry entry
    initialProjection = OracleProjection.initialState genesis bootstraps
    prepared =
      checkedEither
        "prepare causal projection entry"
        (OracleProjection.prepareAppliedEntry canonical initialProjection)

checkedOracleGenesisFor ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  CheckedOracleGenesis
checkedOracleGenesisFor genesis bootstraps =
  checkedEither "causal checked Oracle genesis" (checkOracleGenesis raw)
  where
    members = checkedActiveHeralds genesis
    bindings =
      zipWith raftVoterBinding fixtureRaftNodes (fmap heraldMemberEpoch members)
    configuration = fixtureRaftConfiguration
    raw =
      oracleGenesis
        (checkedSystemId genesis)
        members
        (checkedCatalogueDigest genesis)
        (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
        (checkedInitialBootstraps bootstraps)
        (checkedConfigurationDigest genesis)
        (checkedInitialTopologyProjection bootstraps)
        (checkedInitialProjectionDigest bootstraps)
        bindings
        configuration
        ( deriveRaftConfigurationDigest
            (checkedSystemId genesis)
            bindings
            configuration
        )

checkedOracleGenesis :: CheckedOracleGenesis
checkedOracleGenesis =
  checkedEither "checked Oracle genesis" (checkOracleGenesis raw)
  where
    members = checkedActiveHeralds fixtureOracleHeraldGenesis
    bindings =
      zipWith raftVoterBinding fixtureRaftNodes (fmap heraldMemberEpoch members)
    configuration = fixtureRaftConfiguration
    raw =
      oracleGenesis
        (checkedSystemId fixtureCheckedGenesis)
        members
        (checkedCatalogueDigest fixtureCheckedGenesis)
        (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
        (checkedInitialBootstraps fixtureOracleBootstraps)
        (checkedConfigurationDigest fixtureOracleHeraldGenesis)
        (checkedInitialTopologyProjection fixtureOracleBootstraps)
        (checkedInitialProjectionDigest fixtureOracleBootstraps)
        bindings
        configuration
        ( deriveRaftConfigurationDigest
            (checkedSystemId fixtureCheckedGenesis)
            bindings
            configuration
        )

fixtureOracleHeraldGenesis :: CheckedHeraldGenesis
fixtureOracleHeraldGenesis =
  checkedEither "three-Herald Oracle fixture genesis" (checkHeraldGenesis deployment)
  where
    members = deploymentActiveHeralds fixtureDeploymentManifest <> [fixtureThirdMember]
    deployment =
      fixtureDeploymentManifest
        { deploymentActiveHeralds = members,
          deploymentOracleGenesis =
            (deploymentOracleGenesis fixtureDeploymentManifest)
              { oracleGenesisActiveHeralds = members
              }
        }

fixtureOracleBootstraps :: CheckedInitialBootstraps
fixtureOracleBootstraps =
  checkedEither
    "three-Herald Oracle fixture bootstraps"
    ( checkInitialBootstraps
        fixtureOracleHeraldGenesis
        (PrimordialProcessManifest (firstLocalBootstrapId : [fixtureRemoteBootstrapId]))
    )

fixtureThirdMember :: HeraldMember
fixtureThirdMember =
  HeraldMember
    (checkedEither "third Herald id" (mkHeraldId (fixtureIdentifierBytes 0xb3)))
    (checkedEither "third Herald epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0xb4)))

fixtureRaftNodes :: [RaftNodeId]
fixtureRaftNodes =
  [ checkedEither "fixture Raft node" (mkRaftNodeId (fixtureIdentifierBytes byte))
  | byte <- [0xb0, 0xb1, 0xb2]
  ]

fixtureRaftConfiguration :: RaftNativeConfiguration
fixtureRaftConfiguration =
  checkedRaftNativeConfiguration
    ( checkedEither
        "fixture Raft genesis"
        ( checkRaftGenesis
            ( raftGenesis
                firstRaftNode
                fixtureRaftNodes
                (duration 100000)
                (duration 300000)
                (duration 500000)
            )
        )
    )
  where
    firstRaftNode = case fixtureRaftNodes of
      first : _ -> first
      [] -> error "fixed Raft voter set is empty"
    duration :: Word64 -> RaftDurationMicros
    duration value =
      checkedEither "fixture Raft duration" (mkRaftDurationMicros value)

committedOracleEntry ::
  OracleState ->
  OracleEnvelope ->
  (OracleState, AppliedOracleEntry)
committedOracleEntry state envelope =
  case checkedEither "commit Oracle fixture" (stepOracle envelope state) of
    (successor, OracleCommitted _, effects) -> case oracleEffects effects of
      [EmitAppliedOracleEntry entry] -> (successor, entry)
      other -> error ("expected one applied Oracle entry, got " <> show other)
    (_, outcome, _) -> error ("expected committed Oracle outcome, got " <> show outcome)

applyControlledFixture ::
  ControlledFixture ->
  PeerInput.PeerInputContext ->
  PeerInput.PeerInputState ->
  IO (PeerInput.PeerInputState, PeerInput.PeerInputResult)
applyControlledFixture fixture context state =
  checked
    "controlled peer input"
    (PeerInput.applyPeerPublication context fixture.binding fixture.item state)

installTerminalLabelDeletion ::
  CheckedInitialBootstraps ->
  LabelDecisionId ->
  GlobalObjectId ->
  PeerInput.PeerInputState ->
  IO (StructuralConsequenceCause, PeerInput.PeerInputState)
installTerminalLabelDeletion bootstraps decision object state = do
  let controlled = PeerInput.peerInputControlledState state
      progress = PeerInput.peerInputStructuralProgressState state
      resolveIndex = controlIndex 2
      releaseIndex = controlIndex 2
  prior <-
    checked
      "terminal structural label prior"
      ( Controlled.checkControlledLabelPrior
          (checkedInitialProjectionDigest bootstraps)
          object
          controlled
      )
  priorLabel <-
    case releasedLabelStateView (Controlled.controlledLabelPriorEffectiveState prior) of
      ReleasedLabelView label -> pure label
      (ReleasedDeletedView _) -> assertFailure "terminal structural prior is already deleted"
  facts <-
    checked
      "terminal structural prepared label facts"
      ( mkPreparedLabelFacts
          decision
          resolveIndex
          object
          (releasedDeleted (snd priorLabel + 1))
          (Controlled.controlledLabelPriorEffectiveState prior)
          (Controlled.controlledLabelPriorRevision prior)
          profileCatalogueDigest
          (Controlled.controlledLabelPriorAuthority prior)
          (Controlled.controlledLabelPriorAuthorityJustification prior)
          ( preparedAuthorityDisposition
              (Controlled.controlledLabelPriorAuthority prior)
              priorLabel
              (releasedDeleted (snd priorLabel + 1))
          )
          (GraphProgress.structuralProgressMemberSetDigest progress)
      )
  preparedRelease <-
    checked
      "terminal structural label release"
      ( Controlled.prepareControlledLabelRelease
          (checkedInitialProjectionDigest bootstraps)
          facts
          releaseIndex
          controlled
      )
  cause <- checked "terminal structural label cause" (labelReleaseCause decision releaseIndex)
  preparedRemoval <-
    checked
      "terminal structural Controlled removal"
      ( Controlled.prepareControlledRemoval
          cause
          object
          (Controlled.commitControlledLabelRelease preparedRelease)
      )
  advanced <-
    advancePeerInputStructuralControl
      releaseIndex
      (replacePeerInputControlled (Controlled.commitControlledRemoval preparedRemoval) state)
  pure (cause, advanced)

replacePeerInputControlled ::
  Controlled.State ->
  PeerInput.PeerInputState ->
  PeerInput.PeerInputState
replacePeerInputControlled controlled state =
  PeerInput.peerInputState
    (PeerInput.peerInputPublicationState state)
    (PeerInput.peerInputPeerStreamState state)
    (PeerInput.peerInputStoreState state)
    (PeerInput.peerInputGraphState state)
    (PeerInput.peerInputIdGeneratorState state)
    (PeerInput.peerInputStructuralProgressState state)
    (PeerInput.peerInputAlignmentState state)
    (PeerInput.peerInputPlacementState state)
    controlled
    (PeerInput.peerInputSortRegistryState state)
    (PeerInput.peerInputApplicationState state)
    (PeerInput.peerInputWaitState state)
    (PeerInput.peerInputLabelBarrierState state)
    (PeerInput.peerInputDisappearanceState state)

structuralItemOccurrence ::
  SequencedItem PeerPublication ->
  IO StructuralOccurrenceId
structuralItemOccurrence item =
  maybe
    (assertFailure "structural fixture has no stamp")
    (pure . structuralOccurrenceStampOccurrence)
    (peerPublicationStructuralStamp (sequencedItemPayload item))

-- Direct PeerInput tests must install the same structural-control successor
-- that OracleAdvance commits atomically with its Oracle projection. Supplying
-- an advanced read-only Oracle context beside a stale progress owner describes
-- no reachable production state and is correctly rejected by Graph.Progress.
advancePeerInputStructuralControl ::
  ControlIndex ->
  PeerInput.PeerInputState ->
  IO PeerInput.PeerInputState
advancePeerInputStructuralControl prefix state = do
  prepared <-
    checked
      "advance direct PeerInput structural control"
      ( GraphProgress.prepareStructuralControlProgress
          prefix
          (PeerInput.peerInputStructuralProgressState state)
      )
  let (progress, _) = GraphProgress.commitStructuralControlProgress prepared
  pure
    ( PeerInput.peerInputState
        (PeerInput.peerInputPublicationState state)
        (PeerInput.peerInputPeerStreamState state)
        (PeerInput.peerInputStoreState state)
        (PeerInput.peerInputGraphState state)
        (PeerInput.peerInputIdGeneratorState state)
        progress
        (PeerInput.peerInputAlignmentState state)
        (PeerInput.peerInputPlacementState state)
        (PeerInput.peerInputControlledState state)
        (PeerInput.peerInputSortRegistryState state)
        (PeerInput.peerInputApplicationState state)
        (PeerInput.peerInputWaitState state)
        (PeerInput.peerInputLabelBarrierState state)
        (PeerInput.peerInputDisappearanceState state)
    )

establishPeerInputTopologyCut ::
  PeerFixture ->
  PeerInput.PeerInputState ->
  IO (PeerInput.PeerInputState, TopologyCutId)
establishPeerInputTopologyCut fixture state = do
  let progress0 = PeerInput.peerInputStructuralProgressState state
      report =
        GraphProtocol.structuralAppliedReport
          fixture.remoteEpoch
          (GraphProgress.structuralAppliedVector progress0)
          (GraphProgress.structuralAppliedControlPrefix progress0)
  preparedReport <-
    checked
      "ordinary writer remote structural report"
      (GraphProgress.prepareStructuralReport report progress0)
  let (progress1, _) = GraphProgress.commitStructuralReport preparedReport
      registry = PeerInput.peerInputSortRegistryState state
      graph = PeerInput.peerInputGraphState state
      controlled = PeerInput.peerInputControlledState state
      oracle = fixture.oracle
      views =
        Reconciliation.reconciliationViews
          fixture.localEpoch
          ( Map.fromList
              [ ( SortRegistry.registryEntrySortId entry,
                  sortOccurrence
                    (SortRegistry.registryEntrySortId entry)
                    (SortRegistry.registryEntryOccurrenceId entry)
                )
              | entry <- SortRegistry.registryEntries registry
              ]
          )
          (Set.fromList (Graph.graphNonStructuralBaselineVertices graph))
          ( Map.fromList
              [ (process, residence)
              | process <- OracleProjection.projectedProcessEpochs oracle,
                OracleProjection.oracleViewProcessIsLive
                  process
                  (OracleProjection.oracleView oracle),
                Just residence <-
                  [ OracleProjection.oracleViewProcessResidence
                      process
                      (OracleProjection.oracleView oracle)
                  ]
              ]
          )
          (Set.fromList (Controlled.controlledNormalPossessionEntries controlled))
  proposal <-
    checked
      "ordinary writer topology proposal"
      ( GraphProgress.prepareTopologyCutProposal
          views
          ( GraphProgress.topologyControlCheckpoint
              (GraphProgress.structuralAppliedControlPrefix progress1)
              "retired-ordinary-writer"
          )
          progress1
      )
  preparedProposal <- case proposal of
    GraphProgress.TopologyCutProposalReady ready -> pure ready
    GraphProgress.TopologyCutProposalUnavailable ->
      assertFailure "ordinary writer topology proposal unavailable"
    GraphProgress.TopologyCutProposalHeld dependencies ->
      assertFailure
        ("ordinary writer topology proposal held: " <> show dependencies)
  let announce = GraphProgress.preparedTopologyCutProposalAnnounce preparedProposal
      cut = GraphProtocol.topologyCutAnnounceId announce
      progress2 = GraphProgress.commitTopologyCutProposal preparedProposal
  preparedAcceptance <-
    checked
      "ordinary writer topology acceptance"
      ( GraphProgress.prepareTopologyCutAcceptance
          (GraphProtocol.topologyCutAcceptance cut report)
          progress2
      )
  let progress3 = GraphProgress.commitTopologyCutAcceptance preparedAcceptance
  preparedEstablishment <-
    checked
      "ordinary writer topology establishment"
      (GraphProgress.prepareTopologyCutEstablishment progress3)
  let progress4 = GraphProgress.commitTopologyCutEstablishment preparedEstablishment
  pure (replacePeerInputStructuralProgress progress4 state, cut)

replacePeerInputStructuralProgress ::
  GraphProgress.StructuralProgressState ->
  PeerInput.PeerInputState ->
  PeerInput.PeerInputState
replacePeerInputStructuralProgress progress state =
  PeerInput.peerInputState
    (PeerInput.peerInputPublicationState state)
    (PeerInput.peerInputPeerStreamState state)
    (PeerInput.peerInputStoreState state)
    (PeerInput.peerInputGraphState state)
    (PeerInput.peerInputIdGeneratorState state)
    progress
    (PeerInput.peerInputAlignmentState state)
    (PeerInput.peerInputPlacementState state)
    (PeerInput.peerInputControlledState state)
    (PeerInput.peerInputSortRegistryState state)
    (PeerInput.peerInputApplicationState state)
    (PeerInput.peerInputWaitState state)
    (PeerInput.peerInputLabelBarrierState state)
    (PeerInput.peerInputDisappearanceState state)

dualHeldControlledState :: ControlledFixture -> IO PeerInput.PeerInputState
dualHeldControlledState fixture = do
  let predecessorPublication =
        PeerInput.peerInputPublicationState fixture.initialState
      direction = sequencedItemDirection fixture.item
      sequenceNumber = sequencedItemSequence fixture.item
      digest = sequencedItemDigest fixture.item
      dependencies =
        Set.fromList
          [ Publication.EffectiveSortDependency
              (publicationBatchSortId fixture.batch)
              (publicationBatchOccurrenceId fixture.batch),
            Publication.ControlIndexDependency (controlIndex 1)
          ]
      outcomes =
        Map.fromList
          [ (destination, Publication.DestinationPending)
          | destination <- NonEmpty.toList (publicationBatchDestinations fixture.batch)
          ]
  resolution <-
    checked
      "dual held resolution"
      (Publication.incomingPublicationResolution dependencies outcomes)
  publicationCandidate <-
    checked
      "dual held publication candidate"
      ( Publication.prepareIncomingPublicationCandidate
          fixture.membershipGenerationId
          direction
          sequenceNumber
          digest
          (sequencedItemPayload fixture.item)
          predecessorPublication
      )
  preparedPublication <-
    checked
      "dual held publication"
      (Publication.finalizeIncomingPublication (Just resolution) publicationCandidate)
  let (publication, _) = Publication.commitIncomingPublication preparedPublication
  streamCandidate <-
    checked
      "dual held stream candidate"
      ( PeerStream.prepareReceiveCandidate
          (logicalPublicationItem fixture.item)
          (PeerInput.peerInputPeerStreamState fixture.initialState)
      )
  preparedStream <-
    checked
      "dual held stream"
      ( PeerStream.finalizeReceive
          (Map.singleton sequenceNumber ReceivedPending)
          streamCandidate
      )
  let (peerStream, _) = PeerStream.commitReceive preparedStream
  pure
    ( PeerInput.peerInputState
        publication
        peerStream
        (PeerInput.peerInputStoreState fixture.initialState)
        (PeerInput.peerInputGraphState fixture.initialState)
        (PeerInput.peerInputIdGeneratorState fixture.initialState)
        (PeerInput.peerInputStructuralProgressState fixture.initialState)
        (PeerInput.peerInputAlignmentState fixture.initialState)
        (PeerInput.peerInputPlacementState fixture.initialState)
        (PeerInput.peerInputControlledState fixture.initialState)
        (PeerInput.peerInputSortRegistryState fixture.initialState)
        (PeerInput.peerInputApplicationState fixture.initialState)
        (PeerInput.peerInputWaitState fixture.initialState)
        (PeerInput.peerInputLabelBarrierState fixture.initialState)
        (PeerInput.peerInputDisappearanceState fixture.initialState)
    )

logicalPublicationItem ::
  SequencedItem PeerPublication -> SequencedItem PeerLogicalPayload
logicalPublicationItem item =
  sequencedItem
    (sequencedItemDirection item)
    (sequencedItemSequence item)
    (sequencedItemDigest item)
    (PeerLogicalPublication (sequencedItemPayload item))

peerFixture :: IO PeerFixture
peerFixture =
  peerFixtureFor
    fixtureCheckedGenesis
    (firstLocalBootstrapId : [fixtureRemoteBootstrapId])
    fixtureRemoteMember

peerFixtureFor ::
  CheckedHeraldGenesis ->
  [BootstrapManifestId] ->
  HeraldMember ->
  IO PeerFixture
peerFixtureFor genesis bootstrapIds sourceMember = do
  bootstraps <-
    checked
      "remote checked bootstrap"
      ( checkInitialBootstraps
          genesis
          (PrimordialProcessManifest bootstrapIds)
      )
  source <- requireProcessAt (heraldMemberEpoch sourceMember) bootstraps
  root <- requireWriterRoot (heraldMemberEpoch sourceMember) source SortDefinitionRole
  carrier <-
    maybe
      (assertFailure "missing sort-definition carrier")
      pure
      ( find
          ((== SortDefinitionRole) . primordialReplicaRole)
          (checkedPrimordialReplicas genesis)
      )
  let localEpoch = checkedLocalHeraldEpoch genesis
      remoteEpoch = heraldMemberEpoch sourceMember
      sourceNabla = case appliedRootRole root of
        WriterRoot nabla _ -> nabla
        _ -> error "requireWriterRoot returned a reader"
      sourceAuthority = appliedRootAuthority root
      identifier =
        publicationId
          sourceNabla
          sourceAuthority
          remoteEpoch
          (nablaSequence 0)
      carrierDescriptor = primordialReplicaDescriptor carrier
  publication <-
    checked
      "incoming checked publication"
      ( mkCheckedPublication
          carrierDescriptor
          identifier
          (sortDefinitionValue carrierDescriptor)
      )
  store <- checked "initial Store" (Store.initialState genesis)
  members <-
    maybe
      (assertFailure "fixture has no active Herald epochs")
      pure
      ( NonEmpty.nonEmpty
          (fmap heraldMemberEpoch (checkedActiveHeralds genesis))
      )
  initialMembership <-
    checked
      "initial peer-input membership"
      ( genesisHeraldMembershipGeneration
          (checkedSystemId genesis)
          members
      )
  let membershipVector = emptyStructuralVersionVector initialMembership
  let initialGraph =
        Graph.initialTopologyState
          (checkedInitialTopologyProjection bootstraps)
      sortRegistry = SortRegistry.initialState genesis
      oracle = OracleProjection.initialState genesis bootstraps
      allBootstraps = checkedInitialBootstraps bootstraps
      localConfiguredSource =
        maybe
          source
          id
          (find ((== localEpoch) . appliedProcessResidence) allBootstraps)
      residentBootstraps =
        filter ((== localEpoch) . appliedProcessResidence) allBootstraps
      bootstrapViews =
        Bootstrap.bootstrapViews
          (OracleProjection.oracleView oracle)
          (SortRegistry.sortView sortRegistry)
  initialOwners <-
    checked
      "initial bootstrap owners"
      (Bootstrap.initialBootstrapOwners genesis)
  processOwners <-
    foldM
      ( \owners bootstrap -> do
          prepared <-
            checked
              "resident process bootstrap"
              (Bootstrap.prepareProcessBootstrap bootstrapViews bootstrap owners)
          pure (fst (Bootstrap.commitProcessBootstrap prepared))
      )
      initialOwners
      residentBootstraps
  let controlled = Bootstrap.bootstrapControlledState processOwners
      reconciliationViews =
        Reconciliation.reconciliationViews
          localEpoch
          ( Map.fromList
              [ ( SortRegistry.registryEntrySortId entry,
                  sortOccurrence
                    (SortRegistry.registryEntrySortId entry)
                    (SortRegistry.registryEntryOccurrenceId entry)
                )
              | entry <- SortRegistry.registryEntries sortRegistry
              ]
          )
          (Set.fromList (Graph.graphBaselineVertices initialGraph))
          ( Map.fromList
              [ (appliedProcessEpochId bootstrap, appliedProcessResidence bootstrap)
              | bootstrap <- allBootstraps
              ]
          )
          ( Set.fromList
              (Controlled.controlledNormalPossessionEntries controlled)
          )
  reconciliation <-
    case Reconciliation.structuralAppliedStateWithGenesis
      (checkedSystemId genesis)
      reconciliationViews
      membershipVector
      allBootstraps of
      Left problem ->
        assertFailure ("genesis structural seed: " <> show problem)
      Right (Left dependencies) ->
        assertFailure ("genesis structural seed held: " <> show dependencies)
      Right (Right seeded) -> pure seeded
  preparedGraphBaseline <-
    checked
      "initial structural graph baseline"
      (Graph.prepareStructuralGraphBaseline reconciliation initialGraph)
  let graph = Graph.commitStructuralGraphBaseline preparedGraphBaseline
  structuralProgress <-
    checked
      "initial structural progress"
      ( GraphProgress.initialStructuralProgressState
          (checkedSystemId genesis)
          localEpoch
          initialMembership
          (checkedInitialProjectionDigest bootstraps)
          reconciliation
      )
  idGenerator <-
    checked
      "initial generated-identity owner"
      (IdGenerator.initialState fixtureGeneratorSeed)
  let system = checkedSystemId genesis
      sourceTopology =
        GraphProgress.structuralLastInstalledCutId structuralProgress
      destination =
        publicationDestination
          (deriveSystemViewDeltaId system localEpoch SortDefinitionRole)
          (deriveSystemViewStoreIncarnationId system localEpoch SortDefinitionRole)
          Normal
  batch <-
    checked
      "incoming publication batch"
      ( mkPublicationBatch
          identifier
          (appliedProcessEpochId source)
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          (checkedPublicationCanonicalValue publication)
          Normal
          sourceTopology
          (appliedRootControlPrerequisite root)
          (destination :| [])
      )
  direction <- checked "incoming direction" (mkStreamDirection remoteEpoch localEpoch)
  let item = ordinarySequencedItem direction firstStreamSequence batch
      projectionDigest = checkedInitialProjectionDigest bootstraps
      staticView =
        discoveryStatic
          system
          (checkedLocalHeraldId genesis)
          localEpoch
          (checkedActiveHeralds genesis)
          (checkedCatalogueDigest genesis)
          projectionDigest
          initialMembership
      discoveryInitial = DiscoveryState.initialState staticView
      candidate =
        Discovery.peerCandidate
          (heraldMemberId sourceMember)
          remoteEpoch
          (Discovery.connectionNonce 7)
      hello =
        Discovery.peerHello
          system
          (heraldMemberId sourceMember)
          remoteEpoch
          (Discovery.connectionNonce 7)
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest genesis)
          projectionDigest
          Nothing
  preparedBinding <-
    checked
      "peer binding"
      (DiscoveryState.preparePeerHello candidate hello discoveryInitial)
  let (discovery, bindingDisposition) = DiscoveryState.commitPeerHello preparedBinding
  binding <- case bindingDisposition of
    Discovery.PeerHelloAccepted Discovery.FirstBinding admitted -> pure admitted
    observed -> assertFailure ("unexpected binding disposition: " <> show observed)
  let placement = Placement.initialState localEpoch
      context =
        PeerInput.peerInputContext
          genesis
          oracle
          discovery
          placement
      state =
        PeerInput.peerInputState
          (Publication.initialState localEpoch)
          (PeerStream.initialState localEpoch)
          store
          graph
          idGenerator
          structuralProgress
          Alignment.emptyState
          placement
          controlled
          sortRegistry
          Application.emptyState
          Wait.emptyState
          (LabelBarrier.initialState localEpoch)
          (DisappearanceOwner.initialState localEpoch)
  pure
    PeerFixture
      { genesis,
        context,
        state,
        bootstraps,
        discovery,
        placement,
        oracle,
        sortRegistry,
        binding,
        item,
        batch,
        publication,
        destination,
        direction,
        sourceProcess = appliedProcessEpochId source,
        sourceNabla,
        sourceAuthority,
        sourceBootstrap = source,
        localSource = localConfiguredSource,
        localEpoch,
        remoteEpoch,
        sortId = appliedRootSortId root,
        occurrence = appliedRootOccurrenceId root,
        controlPrerequisite = appliedRootControlPrerequisite root,
        topologyCut = sourceTopology
      }

requireWriterRoot ::
  HeraldEpoch ->
  AppliedProcessBootstrap ->
  PredefinedSortRole ->
  IO AppliedRoot
requireWriterRoot expectedResidence process role = do
  assertEqual "source resides at the expected Herald" expectedResidence (appliedProcessResidence process)
  assertEqual "process and root authority agree" (appliedProcessAuthority process) (authorityForRole role)
  maybe
    (assertFailure "missing applied writer root")
    pure
    ( find
        ( \root -> case appliedRootRole root of
            WriterRoot _ _ ->
              appliedRootCatalogueRole root == role
                && appliedRootAuthority root == authorityForRole role
            _ -> False
        )
        (appliedProcessRoots process)
    )
  where
    authorityForRole _ = appliedProcessAuthority process

requireProcessAt :: HeraldEpoch -> CheckedInitialBootstraps -> IO AppliedProcessBootstrap
requireProcessAt residence bootstraps =
  case filter ((== residence) . appliedProcessResidence) (checkedInitialBootstraps bootstraps) of
    [process] -> pure process
    observed -> assertFailure ("expected one process at residence, got " <> show observed)

firstLocalBootstrapId :: BootstrapManifestId
firstLocalBootstrapId = case fixtureLocalBootstrapIds of
  first : _ -> first
  [] -> error "fixture has no local bootstrap"

secondLocalBootstrapId :: BootstrapManifestId
secondLocalBootstrapId = case fixtureLocalBootstrapIds of
  _ : second : _ -> second
  _ -> error "fixture has no second local bootstrap"

applyFixture ::
  PeerFixture ->
  SequencedItem PeerPublication ->
  PeerInput.PeerInputState ->
  IO (PeerInput.PeerInputState, PeerInput.PeerInputResult)
applyFixture fixture item state =
  checked
    "peer input"
    (PeerInput.applyPeerPublication fixture.context fixture.binding item state)

peerFixtureTopologyCut :: PeerFixture -> TopologyCutId
peerFixtureTopologyCut fixture = fixture.topologyCut

peerDefinitionItem ::
  PeerFixture ->
  Word64 ->
  ControlIndex ->
  StreamSequence ->
  CanonicalDescriptor ->
  IO (CheckedPublication, PublicationBatch, SequencedItem PeerPublication)
peerDefinitionItem fixture publicationSequence prerequisite streamSequence descriptor = do
  let identifier =
        publicationId
          fixture.sourceNabla
          fixture.sourceAuthority
          fixture.remoteEpoch
          (nablaSequence publicationSequence)
      carrierDescriptor =
        predefinedCatalogueDescriptor (profileEntryFor SortDefinitionRole)
  publication <-
    checked
      "checked peer sort-definition publication"
      ( mkCheckedPublication
          carrierDescriptor
          identifier
          (sortDefinitionValue descriptor)
      )
  batch <-
    checked
      "peer sort-definition batch"
      ( mkPublicationBatch
          identifier
          fixture.sourceProcess
          fixture.sortId
          fixture.occurrence
          (checkedPublicationCanonicalValue publication)
          Normal
          fixture.topologyCut
          prerequisite
          (fixture.destination :| [])
      )
  pure
    ( publication,
      batch,
      ordinarySequencedItem fixture.direction streamSequence batch
    )

requireEffectiveSort ::
  SortId -> PeerInput.PeerInputState -> IO SortRegistry.RegistryEntry
requireEffectiveSort sortId state =
  maybe
    (assertFailure "missing effective peer-induced sort")
    pure
    ( SortRegistry.lookupEffectiveSort
        sortId
        (PeerInput.peerInputSortRegistryState state)
    )

replacePeerInputStoreAndRegistry ::
  Store.State ->
  SortRegistry.State ->
  PeerInput.PeerInputState ->
  PeerInput.PeerInputState
replacePeerInputStoreAndRegistry store registry state =
  PeerInput.peerInputState
    (PeerInput.peerInputPublicationState state)
    (PeerInput.peerInputPeerStreamState state)
    store
    (PeerInput.peerInputGraphState state)
    (PeerInput.peerInputIdGeneratorState state)
    (PeerInput.peerInputStructuralProgressState state)
    (PeerInput.peerInputAlignmentState state)
    (PeerInput.peerInputPlacementState state)
    (PeerInput.peerInputControlledState state)
    registry
    (PeerInput.peerInputApplicationState state)
    (PeerInput.peerInputWaitState state)
    (PeerInput.peerInputLabelBarrierState state)
    (PeerInput.peerInputDisappearanceState state)

regularEndpointValue ::
  ProcessEpochId ->
  PredefinedSortRole ->
  GlobalObjectId ->
  SortId ->
  IO Value
regularEndpointValue process role object referencedSort = do
  objectField <- checked "ordinary endpoint object field" (mkFieldName "object_id")
  labelField <- checked "ordinary endpoint label field" (mkFieldName "label")
  sortField <- checked "ordinary endpoint sort field" (mkFieldName "sort_id")
  sequencingField <-
    checked "ordinary endpoint sequencing field" (mkFieldName "sequencing_object")
  let common =
        [ ( objectField,
            globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)
          ),
          (labelField, labelValue ((ProcessLabel process, 0))),
          (sortField, bytesValue (sortIdBytes referencedSort))
        ]
      fields = case role of
        NablaRole ->
          (sequencingField, optionalGlobalUniqueIdValue Nothing) : common
        DeltaRole -> common
        _ -> error "regularEndpointValue requires a Nabla or Delta role"
  checked "ordinary endpoint value" (recordValue fields)

labelControlledOrdinaryPeerItem ::
  PeerFixture ->
  CanonicalDescriptor ->
  NablaId ->
  AuthorityEpoch ->
  SortId ->
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  TopologyCutId ->
  PublicationDestination ->
  Word64 ->
  StreamSequence ->
  IO (PublicationId, PublicationBatch, SequencedItem PeerPublication)
labelControlledOrdinaryPeerItem
  fixture
  descriptor
  sourceNabla
  sourceAuthority
  sortId
  occurrence
  prerequisite
  sourceTopology
  destination
  publicationSequence
  streamSequence = do
    objectField <-
      checked "controlled ordinary object field" (mkFieldName "object_id")
    labelField <- checked "controlled ordinary label field" (mkFieldName "label")
    let controlledObject = globalObjectIdFromNablaId sourceNabla
    value <-
      checked
        "label-controlled ordinary peer value"
        ( recordValue
            [ ( objectField,
                globalUniqueIdValue
                  (globalUniqueIdFromGlobalObjectId controlledObject)
              ),
              (labelField, labelValue ((ProcessLabel fixture.sourceProcess, 0)))
            ]
        )
    let identifier =
          publicationId
            sourceNabla
            sourceAuthority
            fixture.remoteEpoch
            (nablaSequence publicationSequence)
    publication <-
      checked
        "checked ordinary peer value"
        (mkCheckedPublication descriptor identifier value)
    batch <-
      checked
        "ordinary peer value batch"
        ( mkPublicationBatch
            identifier
            fixture.sourceProcess
            sortId
            occurrence
            (checkedPublicationCanonicalValue publication)
            Normal
            sourceTopology
            prerequisite
            (destination :| [])
        )
    pure
      ( identifier,
        batch,
        ordinarySequencedItem fixture.direction streamSequence batch
      )

regularPeerDescriptor :: CanonicalDescriptor
regularPeerDescriptor = regularPeerDescriptorWithRetention 0

regularPeerDescriptorWithRetention :: Word64 -> CanonicalDescriptor
regularPeerDescriptorWithRetention retention =
  admittedApplicationSortDescriptor
    ( checkedEither
        "regular peer sort definition"
        ( admitApplicationSortDefinition
            ( DeclaredSortDefinition
                ApplicationSortDescriptor
                  { SortSyntax.sortKind = RegularSort,
                    SortSyntax.valueSchema =
                      RecordSchema (Map.singleton "key" BoolSchema),
                    SortSyntax.keyProjections =
                      [ApplicationProjection ("key" :| [])],
                    SortSyntax.validityPredicate = AlwaysPredicate,
                    SortSyntax.obsolescencePredicate = NeverPredicate,
                    SortSyntax.rankTerms =
                      RankApplicationValue Ascending :| [],
                    SortSyntax.minimumRetentionMicros = retention,
                    SortSyntax.isImmutable = False,
                    SortSyntax.labelField = Nothing
                  }
                Nothing
            )
        )
    )

labelControlledPeerDescriptor :: CanonicalDescriptor
labelControlledPeerDescriptor = labelControlledPeerDescriptorWithRetention 0

labelControlledPeerDescriptorWithRetention :: Word64 -> CanonicalDescriptor
labelControlledPeerDescriptorWithRetention retention =
  admittedApplicationSortDescriptor
    ( checkedEither
        "label-controlled peer sort definition"
        ( admitApplicationSortDefinition
            ( DeclaredSortDefinition
                ApplicationSortDescriptor
                  { SortSyntax.sortKind = ControlledSort,
                    SortSyntax.valueSchema =
                      RecordSchema
                        ( Map.fromList
                            [ ("label", LabelSchema),
                              ("object_id", UniqueIdSchema)
                            ]
                        ),
                    SortSyntax.keyProjections =
                      [ApplicationProjection ("object_id" :| [])],
                    SortSyntax.validityPredicate = AlwaysPredicate,
                    SortSyntax.obsolescencePredicate = NeverPredicate,
                    SortSyntax.rankTerms =
                      RankApplicationValue Ascending :| [],
                    SortSyntax.minimumRetentionMicros = retention,
                    SortSyntax.isImmutable = False,
                    SortSyntax.labelField = Just "label"
                  }
                Nothing
            )
        )
    )

ordinaryBatchDigest :: PublicationBatch -> PeerItemDigest
ordinaryBatchDigest = peerPublicationDigest . presentedOrdinaryPeerPublication

generatorCounter :: PeerInput.PeerInputState -> Word64
generatorCounter =
  IdGenerator.witnessedNextGeneratorCounter
    . IdGenerator.idGeneratorStateWitness
    . PeerInput.peerInputIdGeneratorState

ordinarySequencedItem ::
  StreamDirection ->
  StreamSequence ->
  PublicationBatch ->
  SequencedItem PeerPublication
ordinarySequencedItem direction sequenceNumber batch =
  sequencedItem
    direction
    sequenceNumber
    (ordinaryBatchDigest batch)
    (presentedOrdinaryPeerPublication batch)

requireAcknowledgement ::
  PeerInput.PeerInputResult ->
  IO PeerInput.PeerAcknowledgementPlan
requireAcknowledgement result =
  case PeerInput.peerInputAcknowledgements result of
    [acknowledgement] -> pure acknowledgement
    [] -> assertFailure "missing acknowledgement plan"
    acknowledgements ->
      assertFailure
        ("expected one acknowledgement plan, got " <> show (length acknowledgements))

requireIncoming :: PeerFixture -> PeerInput.PeerInputState -> IO Publication.IncomingPublicationRecord
requireIncoming fixture =
  requireIncomingById (checkedPublicationId fixture.publication)

requireIncomingById ::
  PublicationId ->
  PeerInput.PeerInputState ->
  IO Publication.IncomingPublicationRecord
requireIncomingById identifier state =
  maybe
    (assertFailure "missing incoming publication record")
    pure
    ( Publication.lookupIncomingPublication
        identifier
        (PeerInput.peerInputPublicationState state)
    )

requireDestinationSlot :: PeerFixture -> PeerInput.PeerInputState -> IO Store.StoreSlot
requireDestinationSlot fixture state =
  maybe
    (assertFailure "missing exact destination Store slot")
    pure
    ( Store.lookupStoreSlot
        (publicationDestinationDelta fixture.destination)
        (PeerInput.peerInputStoreState state)
    )

requireControlledDestinationSlot ::
  ControlledFixture ->
  PeerInput.PeerInputState ->
  IO Store.StoreSlot
requireControlledDestinationSlot fixture state =
  maybe
    (assertFailure "missing controlled destination Store slot")
    pure
    ( Store.lookupStoreSlot
        (publicationDestinationDelta fixture.destination)
        (PeerInput.peerInputStoreState state)
    )

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedEither :: (Show problem) => String -> Either problem value -> value
checkedEither context = either (error . ((context <> ": ") <>) . show) id

fixtureStartCommand :: ConfiguredProcessBootstrap -> OracleCommand
fixtureStartCommand bootstrap =
  startProcessEpochCommand
    ( processStart
        (configuredProcessBootstrapProcessId bootstrap)
        (configuredProcessBootstrapProcessEpochId bootstrap)
        (configuredProcessBootstrapResidence bootstrap)
    )

fixtureGenerationPlan :: Alignment.State -> ContextClassGenerationId -> Plan.AlignmentPlanId
fixtureGenerationPlan state identifier = case Alignment.lookupAlignmentGeneration identifier state of
  Just generation -> Alignment.generationBirthPlanId generation
  Nothing -> error "route-cutover fixture generation missing"
