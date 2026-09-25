{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Step16RegularRetirementAcceptanceProperties
  ( tests,
    liveRegularDisappearanceFixture,
    liveRegularDisappearanceFixtureFor,
    liveRegularDisappearanceRewriteInput,
    liveRegularDisappearancePeerRewrite,
    liveRegularAlignmentGateFixture,
    liveRegularMalformedAlignmentGateFixture,
  )
where

import GenesisFixtures (fixtureHeraldAfterProbePrefix, fixtureRetirementResolution)

import Control.Monad (foldM)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Identity (PrivateDeltaId, PrivateNablaId)
import Eclips.Application.Types.Query
  ( ApplicationQuery (ApplicationQuery),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (LocalTakeCompleted),
  )
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
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write (ApplicationWriteValue (PublishValue))
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceProbeId,
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    admitDisappearanceEvidenceClaim,
    deriveDisappearanceEvidenceDigest,
    deriveDisappearanceProbeId,
    deriveRegularSortOccurrenceClaim,
    disappearanceSubjectMembershipCoordinate,
    regularSortDefinitionDisappearanceSubject,
    resolveDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    HeraldEpoch,
    NablaId,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SortId,
    TopologyCutId,
    controlIndex,
    controlIndexWord64,
    firstStructuralSequence,
    genesisAuthorityEpoch,
    globalObjectIdFromGlobalUniqueId,
    mkDeltaId,
    mkGlobalUniqueId,
    mkHeraldId,
    mkSortDefinitionOccurrenceId,
    mkStoreIncarnationId,
    mkSystemId,
    nablaIdFromGlobalObjectId,
    nablaSequence,
    publicationId,
    sortIdBytes,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.ProcessStart qualified as ProcessStart
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationLogicalState,
    checkedPublicationSort,
    mkCheckedPublication,
  )
import Eclips.Domain.Route
  ( ReplicaStrength (Normal, Weak),
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, NablaCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (DeltaRole, NablaRole, SortDefinitionRole),
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
  )
import Eclips.Domain.Startup qualified as ProcessBootstrap
import Eclips.Domain.Store
  ( lookupVisible,
    retainedInstances,
    retainedSnapshot,
    retainedSnapshotLogicalStrengthFacts,
    visibleInstances,
  )
import Eclips.Domain.StructuralConsequence qualified as StructuralConsequence
import Eclips.Domain.Value
  ( FieldName,
    LabelOwner (ProcessLabel, VoidLabel),
    boolValue,
    bytesValue,
    canonicalValueBytes,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.Alignment.Disappearance qualified as AlignmentDisappearance
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as AlignmentTransferState
import Eclips.Herald.Application.PublicationEvidence
  ( ApplicationPublicationOrigin (RegularApplicationPublicationOrigin),
  )
import Eclips.Herald.Application.Request
  ( ApplicationOperation (LocalTakeApplication, WriteApplication),
    ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (Completed),
  )
import Eclips.Herald.Application.Request.Internal
  ( requestId,
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.SortDefinition
  ( AdmittedApplicationSortDefinition,
    admitApplicationSortDefinition,
    admittedApplicationSortDescriptor,
    admittedApplicationSortValue,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Evidence qualified as Evidence
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery
  ( PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    peerCandidate,
    peerHello,
  )
import Eclips.Herald.Discovery.Internal
  ( ConnectionNonce (ConnectionNonce),
    PeerBinding (PeerBinding),
    PeerCandidate (PeerCandidate),
    firstPeerBindingGeneration,
  )
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect
      ( RunOracleClientAction,
        SendApplicationReply,
        SendPeerControl,
        SetPeerCandidateDisposition
      ),
    effectBatchIsEmpty,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis.Internal
  ( AppliedRootRole (WriterRoot),
    CheckedHeraldGenesis,
    HeraldMember (..),
    PrimordialDefinitionReplica,
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootOccurrenceId,
    appliedRootRole,
    appliedRootSortId,
    checkedActiveHeraldEpochs,
    checkedCatalogueDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
    checkedSystemId,
    lookupConfiguredProcessBootstrap,
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaRole,
    primordialReplicaSortId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol qualified as GraphProtocol
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Initialization (HeraldState, initialHerald)
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
    HeraldInputBody (ApplicationRequestInput, PeerInput),
    PeerControl (PeerAlignmentControl, PeerStreamCompleted),
    heraldInput,
    peerPublicationReceived,
  )
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction (BindOracleConnection, ConnectAndHelloOracle),
    OracleClientIngress (OracleEntriesReceived, OracleHelloReceived),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDispatch.Internal (peerPublicationItem)
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PublicationDestination,
    mkOrdinaryPeerPublication,
    mkPublicationBatch,
    mkStructuralOccurrenceStamp,
    peerPublicationDigest,
    publicationBatchControlPrerequisite,
    publicationDestination,
    structuralPublicationDigestForSemantics,
  )
import Eclips.Herald.PeerStream
  ( SequencedItem,
    StreamDirection,
    StreamSequence,
    firstStreamSequence,
    mkStreamDirection,
    nextStreamSequence,
    sequencedItem,
    sequencedItemPayload,
    streamCompletedPrefix,
    streamCompletionPrefix,
    streamDirectionSource,
    streamPrefixSequence,
    streamReceivedPrefix,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Publication.Route qualified as PublicationRoute
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldStartupInvariant),
    StartupInvariantSubject (StartupProcess),
    StartupInvariantViolation (PublicationRecordInvariant),
    validateHeraldState,
  )
import Eclips.Herald.Startup.State
  ( replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupOracleProjectionState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDiscoveryState,
    startupGraphState,
    startupInitialBootstraps,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ApplicationCall qualified as ApplicationCall
import Eclips.Herald.UseCase.Disappearance qualified as DisappearanceUseCase
import Eclips.Herald.UseCase.OracleAdvance qualified as OracleAdvance
import Eclips.Herald.UseCase.RegularRetirement qualified as RegularRetirement
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalizeAppliedOracleEntry,
  )
import Eclips.Oracle.Command
  ( oracleEnvelope,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import GenesisFixtures
  ( currentPeerHelloReceived,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureCheckedInitialBootstrapsFor,
    fixtureCheckedOracleGenesis,
    fixtureGeneratorSeed,
    fixtureHeraldMembershipGeneration,
    fixtureHeraldRetirementReason,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteMember,
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
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "Step-16 regular-retirement acceptance"
    [ testCase
        "fresh resolution retires atomically, replay is inert, and equal redefinition advances coordinates"
        caseFreshReplayAndRedefinition,
      testCase
        "complete exact authority replay remains inert after a coherent membership advance"
        caseExactReplayAfterMembershipAdvance,
      testCase
        "a never-applied predecessor-membership authority is rejected after a coherent membership advance"
        caseUnappliedAuthorityAfterMembershipAdvance,
      testCase
        "a resolve-time owner blocker rejects before either successor commits"
        caseOwnerBlockerIsAtomic,
      testCase
        "alignment terminally ignores stale definition evidence but applies successor evidence"
        caseAlignmentDefinitionEpochDisposition,
      testCase
        "alignment terminally ignores a retired ordinary occurrence before and after redefinition"
        caseAlignmentOrdinaryRetiredOccurrence,
      testCase
        "alignment rolls back stale Nabla and Delta controlled observations rejected by Store"
        caseAlignmentStructuralTerminalRollback,
      testCase
        "controlled suppression settles only its exact alignment destination transcript"
        caseAlignmentControlledSuppressedEvidence,
      testCase
        "a matching definition written after the frozen cut rejects old retirement authority"
        casePostCutMatchingWriteRejected,
      testCase
        "an authority resolved under another SystemId cannot retire local state"
        caseForeignSystemAuthorityRejected,
      testCase
        "Resolve-held peer definition survives retirement and releases into the successor epoch"
        caseResolveHeldSuccessorRelease,
      testCase
        "a post-Resolve carrier cannot invent a later aggregate control floor"
        caseUnexplainedRedefinitionControlRejected
    ]

caseFreshReplayAndRedefinition :: Assertion
caseFreshReplayAndRedefinition = do
  let fixture = acceptanceFixture
      leafPrepared =
        checkedPure
          "prepare leaf retirement authority"
          ( Disappearance.prepareProjectedRegularResolution
              fixture.terminal
              fixture.leaf
          )
      authority =
        required
          "fresh leaf retirement authority"
          (Disappearance.preparedProjectedRegularResolutionAuthority leafPrepared)
      directPrepared =
        checkedPure
          "prepare direct Herald regular retirement"
          ( RegularRetirement.prepareDisappearanceRegularRetirement
              authority
              fixture.herald
          )
      (directHerald, directSummary) =
        RegularRetirement.commitRegularRetirement directPrepared
      directReplayPrepared =
        checkedPure
          "prepare direct exact-authority replay"
          ( RegularRetirement.prepareDisappearanceRegularRetirement
              authority
              directHerald
          )
      (directReplayHerald, directReplaySummary) =
        RegularRetirement.commitRegularRetirement directReplayPrepared
      (retiredLeaf, retiredHerald, effects, coordination) =
        checkedPure
          "coordinate regular retirement"
          ( DisappearanceUseCase.coordinateProjectedRegularResolution
              fixture.terminal
              fixture.leaf
              fixture.herald
          )
      summary =
        required
          "fresh coordinated retirement summary"
          ( DisappearanceUseCase.projectedRegularResolutionCoordinationSummary
              coordination
          )
      retiredRegistry = startupSortRegistryState retiredHerald
      retiredStore = startupStoreState retiredHerald
      retiredSlot = carrierStoreSlot retiredHerald
      suppression =
        required
          "retained regular-retirement suppression"
          (Store.lookupRegularRetirementSuppression regularSubject retiredStore)
      removedStoreFacts =
        removedEffectiveStoreFacts fixture.herald retiredHerald

  assertBool
    "the direct authority applicator and composed coordinator install the same Herald"
    (directHerald == retiredHerald)
  assertEqual
    "the coordinator exposes the direct applicator summary"
    directSummary
    summary
  assertBool
    "direct exact-authority replay is whole-Herald-identical"
    (directReplayHerald == directHerald)
  assertEqual
    "direct exact-authority replay is classified explicitly"
    RegularRetirement.RegularRetirementExactReplay
    ( RegularRetirement.regularRetirementSummaryDisposition
        directReplaySummary
    )
  assertEqual
    "direct exact-authority replay repeats no matching work"
    0
    ( RegularRetirement.regularRetirementSummaryMatchingWork
        directReplaySummary
    )
  assertEqual
    "direct exact-authority replay purges no Store facts"
    0
    ( RegularRetirement.regularRetirementSummaryPurgedStoreFacts
        directReplaySummary
    )
  assertEqual
    "direct exact-authority replay has no variable fallout"
    0
    ( RegularRetirement.regularRetirementSummaryFalloutCount
        directReplaySummary
    )
  assertEqual
    "direct exact-authority replay performs zero logical work"
    0
    ( RegularRetirement.regularRetirementSummaryLogicalWork
        directReplaySummary
    )
  assertEqual
    "fresh leaf resolution is classified once"
    Disappearance.ProjectedRegularResolutionFresh
    ( DisappearanceUseCase.projectedRegularResolutionCoordinationDisposition
        coordination
    )
  assertEqual
    "the dynamic sort occurrence is no longer effective"
    Nothing
    (SortRegistry.lookupEffectiveSort regularSortId retiredRegistry)
  assertBool
    "the captured definition is absent from the effective Store projection"
    (not (definitionVisible fixture.firstDefinition retiredSlot))
  assertEqual
    "the suppression binds the exact disappearance subject"
    regularSubject
    (Store.regularRetirementSuppressionSubject suppression)
  assertEqual
    "the suppression retains the Open-time publication cut"
    (Disappearance.regularRetirementAuthorityLocalPublicationCut authority)
    (Store.regularRetirementSuppressionLocalPublicationCut suppression)
  assertEqual
    "the structural control owner advances through Resolve"
    fixture.resolveIndex
    ( GraphProgress.structuralAppliedControlPrefix
        (startupStructuralProgressState retiredHerald)
    )
  assertEqual
    "the real Publication owner contributes exactly one captured matching write"
    1
    (RegularRetirement.regularRetirementSummaryMatchingWork summary)
  assertBool
    "the predecessor/successor Store diff contains removed effective facts"
    (removedStoreFacts > 0)
  assertEqual
    "the Store retirement summary counts the independently reconstructed removals"
    removedStoreFacts
    (RegularRetirement.regularRetirementSummaryPurgedStoreFacts summary)
  assertEqual
    "logical work is the four fixed owner actions plus matching writes and purged facts"
    ( 4
        + RegularRetirement.regularRetirementSummaryMatchingWork summary
        + removedStoreFacts
    )
    (RegularRetirement.regularRetirementSummaryLogicalWork summary)
  assertBool "regular retirement emits no runtime effect" (effectBatchIsEmpty effects)
  assertEqual
    "the retired whole-Herald owner product validates"
    (Right ())
    (validateHeraldState retiredHerald)

  let (replayedLeaf, replayedHerald, replayEffects, replayCoordination) =
        checkedPure
          "replay coordinated regular retirement"
          ( DisappearanceUseCase.coordinateProjectedRegularResolution
              fixture.terminal
              retiredLeaf
              retiredHerald
          )
  assertEqual
    "terminal replay is classified duplicate"
    Disappearance.ProjectedRegularResolutionDuplicate
    ( DisappearanceUseCase.projectedRegularResolutionCoordinationDisposition
        replayCoordination
    )
  assertEqual
    "terminal replay carries no reusable owner summary"
    Nothing
    ( DisappearanceUseCase.projectedRegularResolutionCoordinationSummary
        replayCoordination
    )
  assertBool "terminal replay is leaf-state-identical" (replayedLeaf == retiredLeaf)
  assertBool "terminal replay is whole-Herald-identical" (replayedHerald == retiredHerald)
  assertBool "terminal replay emits no effect" (effectBatchIsEmpty replayEffects)

  let stalePosition =
        Publication.applicationPublicationHeraldPosition fixture.firstRecord
      stalePublication =
        definitionPublicationAt fixture.application.fixtureApplicationWriter 500
      staleEnvelope =
        Store.storeObservationEnvelope
          fixture.application.fixtureApplicationProcess
          carrierOccurrence
          ( GraphProgress.structuralLastInstalledCutId
              (startupStructuralProgressState retiredHerald)
          )
          (controlIndex 0)
          Nothing
      staleDestination = storeDestination retiredSlot
  assertEqual
    "an old write at the captured cut is classified as cut-covered"
    (Store.RegularDefinitionWriteCoveredByCut suppression)
    ( Store.classifyRegularDefinitionWrite
        regularSubject
        (Just stalePosition)
        (controlIndex 0)
        retiredStore
    )
  let (afterStaleStore, staleOutcomes) =
        Store.commitPeerStoreApplication
          ( checkedPure
              "apply stale pre-Resolve definition"
              ( Store.preparePeerStoreApplication
                  staleEnvelope
                  (staleDestination :| [])
                  stalePublication
                  retiredStore
              )
          )
  case staleOutcomes of
    outcome :| [] ->
      assertEqual
        "the stale Store write is terminally ignored"
        Store.PeerStoreTerminallyIgnored
        (Store.peerStoreOutcomeDisposition outcome)
    outcomes -> assertFailure ("expected one stale Store outcome, got " <> show outcomes)
  assertBool "a stale write is Store-state-identical" (afterStaleStore == retiredStore)

  let successorInstallation = installDefinition fixture.application 2 retiredHerald
      redefinedHerald = successorInstallation.installedHerald
      successorPlan = successorInstallation.installedPlan
      successorRecord = successorInstallation.installedRecord
      successorEnvelope = successorInstallation.installedEnvelope
      successorChecked = Publication.applicationPublicationChecked successorRecord
      effectiveSuccessor =
        required
          "effective successor definition"
          ( SortRegistry.lookupEffectiveSort
              regularSortId
              (startupSortRegistryState redefinedHerald)
          )
      redefinedSlot = carrierStoreSlot redefinedHerald
  assertEqual
    "the post-Resolve plan selects the authority-derived successor occurrence"
    fixture.successorOccurrence
    (SortRegistry.sortInductionPlanOccurrenceId successorPlan)
  assertEqual
    "the post-Resolve plan requires the exact Resolve index"
    fixture.resolveIndex
    (SortRegistry.sortInductionPlanControlPrerequisite successorPlan)
  assertEqual
    "the outer publication remains typed by primordial sort-sort"
    (primordialReplicaSortId sortDefinitionCarrier)
    (checkedPublicationSort successorChecked)
  assertEqual
    "the outgoing carrier record retains primordial sort-sort occurrence"
    carrierOccurrence
    (Publication.applicationPublicationSortOccurrenceId successorRecord)
  assertEqual
    "the Store envelope also retains primordial sort-sort occurrence"
    carrierOccurrence
    (Store.storeObservationEnvelopeSortOccurrence successorEnvelope)
  assertEqual
    "the outer carrier Store itself remains in the primordial occurrence"
    carrierOccurrence
    (Store.storeSlotOccurrenceId redefinedSlot)
  assertEqual
    "the contained definition becomes effective in the Resolve-derived occurrence"
    fixture.successorOccurrence
    (SortRegistry.registryEntryOccurrenceId effectiveSuccessor)
  assertBool
    "the outer and contained coordinates are deliberately distinct"
    (carrierOccurrence /= fixture.successorOccurrence)
  assertEqual
    "the outgoing redefinition carries the Resolve prerequisite"
    fixture.resolveIndex
    (Publication.applicationPublicationControlPrerequisite successorRecord)
  assertEqual
    "the Store redefinition carries the Resolve prerequisite"
    fixture.resolveIndex
    (Store.storeObservationEnvelopeControlPrerequisite successorEnvelope)
  assertEqual
    "the equal redefinition is admitted beyond terminal suppression"
    (Store.RegularDefinitionWriteAdmissibleAfterResolve suppression)
    ( Store.classifyRegularDefinitionWrite
        regularSubject
        (Just (Publication.applicationPublicationHeraldPosition successorRecord))
        fixture.resolveIndex
        (startupStoreState redefinedHerald)
    )
  assertBool
    "the equal successor definition is visible again"
    (definitionVisible successorChecked redefinedSlot)
  assertEqual
    "the redefined whole-Herald owner product validates"
    (Right ())
    (validateHeraldState redefinedHerald)

  let lateReplayPrepared =
        checkedPure
          "prepare exact-authority replay after equal redefinition"
          ( RegularRetirement.prepareDisappearanceRegularRetirement
              authority
              redefinedHerald
          )
      (lateReplayHerald, lateReplaySummary) =
        RegularRetirement.commitRegularRetirement lateReplayPrepared
  assertBool
    "exact-authority replay after redefinition is whole-Herald-identical"
    (lateReplayHerald == redefinedHerald)
  assertEqual
    "exact-authority replay after redefinition remains explicit"
    RegularRetirement.RegularRetirementExactReplay
    (RegularRetirement.regularRetirementSummaryDisposition lateReplaySummary)
  assertEqual
    "exact-authority replay after redefinition repeats no matching work"
    0
    (RegularRetirement.regularRetirementSummaryMatchingWork lateReplaySummary)
  assertEqual
    "exact-authority replay after redefinition purges no Store facts"
    0
    (RegularRetirement.regularRetirementSummaryPurgedStoreFacts lateReplaySummary)
  assertEqual
    "exact-authority replay after redefinition performs zero logical work"
    0
    (RegularRetirement.regularRetirementSummaryLogicalWork lateReplaySummary)

caseExactReplayAfterMembershipAdvance :: Assertion
caseExactReplayAfterMembershipAdvance = do
  let fixture = acceptanceFixtureFrom (fixtureHeraldAfterProbePrefix initialHeraldState)
      authority =
        required
          "membership-advance exact-replay authority"
          ( Disappearance.preparedProjectedRegularResolutionAuthority
              ( checkedPure
                  "prepare membership-advance exact-replay authority"
                  ( Disappearance.prepareProjectedRegularResolution
                      fixture.terminal
                      fixture.leaf
                  )
              )
          )
      (retiredHerald, _) =
        RegularRetirement.commitRegularRetirement
          ( checkedPure
              "apply authority before membership advance"
              ( RegularRetirement.prepareDisappearanceRegularRetirement
                  authority
                  fixture.herald
              )
          )
      (advancedHerald, successorMembership) =
        advanceFixtureOracleMembership retiredHerald
      replayPrepared =
        checkedPure
          "prepare exact authority replay after membership advance"
          ( RegularRetirement.prepareDisappearanceRegularRetirement
              authority
              advancedHerald
          )
      (replayedHerald, replaySummary) =
        RegularRetirement.commitRegularRetirement replayPrepared
      currentMembership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState advancedHerald))
  assertEqual
    "the checked Oracle projection exposes the coherent successor membership"
    successorMembership
    currentMembership
  assertBool
    "complete exact authority replay is whole-Herald-identical after membership advances"
    (replayedHerald == advancedHerald)
  assertEqual
    "cross-membership replay remains explicitly exact"
    RegularRetirement.RegularRetirementExactReplay
    (RegularRetirement.regularRetirementSummaryDisposition replaySummary)
  assertEqual
    "cross-membership replay repeats no matching work"
    0
    (RegularRetirement.regularRetirementSummaryMatchingWork replaySummary)
  assertEqual
    "cross-membership replay purges no Store facts"
    0
    (RegularRetirement.regularRetirementSummaryPurgedStoreFacts replaySummary)
  assertEqual
    "cross-membership replay has no variable fallout"
    0
    (RegularRetirement.regularRetirementSummaryFalloutCount replaySummary)
  assertEqual
    "cross-membership replay performs zero logical work"
    0
    (RegularRetirement.regularRetirementSummaryLogicalWork replaySummary)

caseUnappliedAuthorityAfterMembershipAdvance :: Assertion
caseUnappliedAuthorityAfterMembershipAdvance = do
  let fixture = acceptanceFixtureFrom (fixtureHeraldAfterProbePrefix initialHeraldState)
      authority =
        required
          "never-applied predecessor-membership authority"
          ( Disappearance.preparedProjectedRegularResolutionAuthority
              ( checkedPure
                  "prepare never-applied predecessor-membership authority"
                  ( Disappearance.prepareProjectedRegularResolution
                      fixture.terminal
                      fixture.leaf
                  )
              )
          )
      (advancedHerald, successorMembership) =
        advanceFixtureOracleMembership fixture.herald
      capturedCoordinate =
        Disappearance.regularRetirementAuthorityCoordinate authority
      currentCoordinate =
        disappearanceSubjectMembershipCoordinate
          regularSubject
          successorMembership
      result =
        RegularRetirement.prepareDisappearanceRegularRetirement
          authority
          advancedHerald
  assertBool
    "the coherent successor has a different subject-membership coordinate"
    (currentCoordinate /= capturedCoordinate)
  case result of
    Left problem ->
      assertEqual
        "never-applied old authority is rejected by the exact captured-membership check"
        ( RegularRetirement.RegularRetirementCapturedMembershipMismatch
            currentCoordinate
            capturedCoordinate
        )
        problem
    Right _ ->
      assertFailure
        "never-applied predecessor-membership authority crossed the membership advance"

casePostCutMatchingWriteRejected :: Assertion
casePostCutMatchingWriteRejected = do
  let fixture = acceptanceFixture
      authority =
        required
          "post-cut matching-write retirement authority"
          ( Disappearance.preparedProjectedRegularResolutionAuthority
              ( checkedPure
                  "prepare post-cut matching-write retirement authority"
                  ( Disappearance.prepareProjectedRegularResolution
                      fixture.terminal
                      fixture.leaf
                  )
              )
          )
      secondInstallation =
        installDefinition fixture.application 2 fixture.herald
      secondRecord = secondInstallation.installedRecord
      secondPublication = Publication.applicationPublicationChecked secondRecord
      secondPosition = Publication.applicationPublicationHeraldPosition secondRecord
      beforeTake = secondInstallation.installedHerald
      beforeTakeEvidence =
        checkedPure
          "capture the post-cut application-copy blocker"
          (Evidence.disappearanceEvidenceSnapshotForHerald regularSubject beforeTake)
      afterTake =
        takeApplicationVisibleDefinition
          901
          fixture.application
          secondPublication
          beforeTake
      afterTakeEvidence =
        checkedPure
          "capture the post-cut matching write after local take"
          (Evidence.disappearanceEvidenceSnapshotForHerald regularSubject afterTake)
      result =
        RegularRetirement.prepareDisappearanceRegularRetirement authority afterTake
      rejectedHerald = case result of
        Left _ -> afterTake
        Right prepared -> fst (RegularRetirement.commitRegularRetirement prepared)
  assertEqual
    "the second matching definition initially restores exactly the application-copy blocker"
    [Protocol.VisibleApplicationCopyBlocker]
    ( fmap
        Protocol.disappearanceBlockerClass
        (Evidence.disappearanceEvidenceSnapshotBlockers beforeTakeEvidence)
    )
  assertEqual
    "the real LocalTake application call removes that blocker again"
    []
    (Evidence.disappearanceEvidenceSnapshotBlockers afterTakeEvidence)
  case result of
    Left problem ->
      assertEqual
        "old authority rejects at the second matching Herald publication position"
        (RegularRetirement.RegularRetirementPostCutMatchingWrite secondPosition)
        problem
    Right _ -> assertFailure "old retirement authority accepted a post-cut matching write"
  assertBool
    "rejecting the post-cut write is whole-Herald-state-identical"
    (rejectedHerald == afterTake)

caseUnexplainedRedefinitionControlRejected :: Assertion
caseUnexplainedRedefinitionControlRejected = do
  let fixture = acceptanceFixture
      authority =
        required
          "unexplained-floor retirement authority"
          ( Disappearance.preparedProjectedRegularResolutionAuthority
              ( checkedPure
                  "prepare unexplained-floor retirement authority"
                  ( Disappearance.prepareProjectedRegularResolution
                      fixture.terminal
                      fixture.leaf
                  )
              )
          )
      (retiredHerald, _) =
        RegularRetirement.commitRegularRetirement
          ( checkedPure
              "retire unexplained-floor fixture"
              ( RegularRetirement.prepareDisappearanceRegularRetirement
                  authority
                  fixture.herald
              )
          )
      unexplainedControl = controlIndex 8
      (advancedProgress, _) =
        GraphProgress.commitStructuralControlProgress
          ( checkedPure
              "advance structural progress beyond Resolve"
              ( GraphProgress.prepareStructuralControlProgress
                  unexplainedControl
                  (startupStructuralProgressState retiredHerald)
              )
          )
      advancedHerald =
        replaceStartupStructuralProgressState advancedProgress retiredHerald
      corrupted =
        ( installDefinitionAtControl
            (Just unexplainedControl)
            fixture.application
            2
            advancedHerald
        ).installedHerald
  assertEqual
    "an applied but unexplained later coordinate is not a valid retained aggregate"
    ( Left
        ( HeraldStartupInvariant
            (StartupProcess fixture.application.fixtureApplicationProcess)
            PublicationRecordInvariant
        )
    )
    (validateHeraldState corrupted)

caseAlignmentDefinitionEpochDisposition :: Assertion
caseAlignmentDefinitionEpochDisposition = do
  let authorityFixture = acceptanceFixture
      alignmentInitial = initialHeraldStateFor fixtureStep14CheckedGenesis
      alignmentApplication = fixtureApplication alignmentInitial
      alignmentInstallation =
        installDefinition
          alignmentApplication
          1
          alignmentApplication.fixtureApplicationHerald
      withoutVisibleDefinition =
        takeApplicationVisibleDefinition
          900
          alignmentApplication
          (Publication.applicationPublicationChecked alignmentInstallation.installedRecord)
          alignmentInstallation.installedHerald
      atResolve =
        replaceStartupOracleProjectionState
          ( advanceOracleProjectionTo
              authorityFixture.resolveIndex
              (startupOracleProjectionState withoutVisibleDefinition)
          )
          withoutVisibleDefinition
      alignmentOracleView =
        OracleProjection.oracleView (startupOracleProjectionState atResolve)
      alignmentMembership =
        OracleProjection.oracleViewCurrentHeraldMembership alignmentOracleView
      alignmentEvidence =
        checkedPure
          "capture reachable alignment retirement evidence"
          ( Evidence.disappearanceEvidenceSnapshotForHerald
              regularSubject
              atResolve
          )
      alignmentLeaf =
        reportedLeafFor
          alignmentMembership
          (OracleProjection.oracleViewActiveHeraldEpochs alignmentOracleView)
          alignmentEvidence
      alignmentOutcome =
        checkedPure
          "resolve the reachable alignment retirement subject"
          ( resolveDisappearanceSubject
              (checkedSystemId fixtureStep14CheckedGenesis)
              authorityFixture.resolveIndex
              regularSubject
          )
      alignmentTerminal =
        Protocol.ProjectedDisappearanceResolved
          fixtureProbe
          alignmentOutcome
          authorityFixture.resolveIndex
      authority =
        required
          "alignment fixture retirement authority"
          ( Disappearance.preparedProjectedRegularResolutionAuthority
              ( checkedPure
                  "prepare alignment fixture retirement authority"
                  ( Disappearance.prepareProjectedRegularResolution
                      alignmentTerminal
                      alignmentLeaf
                  )
              )
          )
      retirementPrepared =
        checkedPure
          "retire the reachable alignment epoch fixture"
          ( RegularRetirement.prepareDisappearanceRegularRetirement
              authority
              atResolve
          )
      (retired, _) = RegularRetirement.commitRegularRetirement retirementPrepared
      fixture =
        authorityFixture
          { herald = atResolve,
            application = alignmentApplication
          }
      publication =
        definitionPublicationAt fixture.application.fixtureApplicationWriter 701
      (stale, staleSourceBeforeAck, staleSource) =
        applyAlignmentDefinitionEvidence
          fixture
          (controlIndex 0)
          publication
          retired
      (successor, _, successorSource) =
        applyAlignmentDefinitionEvidence
          fixture
          fixture.resolveIndex
          publication
          retired
      staleEvidence = alignmentAppliedEvidence stale
      successorEvidence = alignmentAppliedEvidence successor
      staleDestinationView =
        AlignmentDisappearance.alignmentRegularRetirementDisappearanceView
          regularSubject
          fixture.resolveIndex
          (startupAlignmentState stale)
      staleUnsettledSourceView =
        AlignmentDisappearance.alignmentRegularRetirementDisappearanceView
          regularSubject
          fixture.resolveIndex
          (Alignment.replaceAlignmentTransferState staleSourceBeforeAck Alignment.emptyState)
      staleSettledSourceView =
        AlignmentDisappearance.alignmentRegularRetirementDisappearanceView
          regularSubject
          fixture.resolveIndex
          (Alignment.replaceAlignmentTransferState staleSource Alignment.emptyState)
      staleStrictSourceView =
        AlignmentDisappearance.alignmentDisappearanceView
          regularSubject
          (Alignment.replaceAlignmentTransferState staleSource Alignment.emptyState)
      successorSourceView =
        AlignmentDisappearance.alignmentRegularRetirementDisappearanceView
          regularSubject
          fixture.resolveIndex
          (Alignment.replaceAlignmentTransferState successorSource Alignment.emptyState)
      subscription = case AlignmentTransferState.sourceSubscriptionEntries staleSourceBeforeAck of
        [(identifier, _)] -> identifier
        observed ->
          error
            ( "expected one alignment fixture source subscription, got "
                <> show (length observed)
            )
      cancellation =
        AlignmentProtocol.alignmentCancel
          subscription
          AlignmentProtocol.AlignmentRelationRemoved
      cancelledSource = cancelAlignmentTransfer cancellation staleSourceBeforeAck
      cancelledDestination =
        cancelAlignmentTransfer
          cancellation
          ( Alignment.alignmentTransferState
              (startupAlignmentState stale)
          )
      cancelledSourceView =
        AlignmentDisappearance.alignmentDisappearanceView
          regularSubject
          (Alignment.replaceAlignmentTransferState cancelledSource Alignment.emptyState)
      cancelledDestinationView =
        AlignmentDisappearance.alignmentDisappearanceView
          regularSubject
          (Alignment.replaceAlignmentTransferState cancelledDestination Alignment.emptyState)
  assertBool
    "all-terminally-ignored alignment evidence leaves Store state unchanged"
    (startupStoreState stale == startupStoreState retired)
  assertEqual
    "the continuing Store revision retains one applied evidence fact"
    1
    (length staleEvidence)
  assertBool
    "stale pre-Resolve alignment evidence converges as terminally ignored"
    ( all
        ( (== AlignmentTransferState.StoreTerminallyIgnored)
            . AlignmentTransferState.appliedDestinationEvidenceDisposition
        )
        staleEvidence
    )
  assertEqual
    "the terminal disposition is tied to the exact continuing revision"
    [ ( DomainAlignment.nextStoreRevision DomainAlignment.initialStoreRevision,
        AlignmentTransferState.AppliedContinuingChange
      )
    ]
    [ ( AlignmentTransferState.appliedDestinationEvidenceSourceRevision applied,
        AlignmentTransferState.appliedDestinationEvidenceKind applied
      )
    | applied <- staleEvidence
    ]
  assertBool
    "unacknowledged stale source evidence retains its continuing-change blocker"
    ( any
        ( (== Protocol.AlignmentChangeLogBlocker)
            . Protocol.disappearanceBlockerClass
        )
        ( AlignmentDisappearance.alignmentDisappearanceBlockers
            staleUnsettledSourceView
        )
    )
  assertEqual
    "terminally ignored destination evidence is immutable history, not live stale work"
    []
    (AlignmentDisappearance.alignmentDisappearanceBlockers staleDestinationView)
  assertEqual
    "the source stops blocking only after the terminal destination acknowledgement"
    []
    (AlignmentDisappearance.alignmentDisappearanceBlockers staleSettledSourceView)
  assertBool
    "the ordinary live disappearance view still exposes the immutable stale transcript"
    ( not
        ( null
            ( AlignmentDisappearance.alignmentDisappearanceBlockers
                staleStrictSourceView
            )
        )
    )
  assertEqual
    "the successor continuing revision retains one applied evidence fact"
    1
    (length successorEvidence)
  assertBool
    "Resolve-qualified alignment evidence follows the ordinary Store path"
    ( all
        ( (== AlignmentTransferState.StoreAppliedOrAlreadyApplied)
            . AlignmentTransferState.appliedDestinationEvidenceDisposition
        )
        successorEvidence
    )
  assertBool
    "Resolve-qualified definition evidence becomes visible in the carrier Store"
    (definitionVisible publication (carrierStoreSlot successor))
  assertEqual
    "Resolve-qualified acknowledged source evidence is successor history"
    []
    (AlignmentDisappearance.alignmentDisappearanceBlockers successorSourceView)
  assertEqual
    "a canceled source transcript is inert even before its stale change is acknowledged"
    []
    (AlignmentDisappearance.alignmentDisappearanceBlockers cancelledSourceView)
  assertEqual
    "a canceled destination transcript is immutable inert history"
    []
    (AlignmentDisappearance.alignmentDisappearanceBlockers cancelledDestinationView)

caseAlignmentOrdinaryRetiredOccurrence :: Assertion
caseAlignmentOrdinaryRetiredOccurrence = do
  let fixture = acceptanceFixture
      authority =
        required
          "ordinary alignment retirement authority"
          ( Disappearance.preparedProjectedRegularResolutionAuthority
              ( checkedPure
                  "prepare ordinary alignment retirement authority"
                  ( Disappearance.prepareProjectedRegularResolution
                      fixture.terminal
                      fixture.leaf
                  )
              )
          )
      (retired, _) =
        RegularRetirement.commitRegularRetirement
          ( checkedPure
              "retire ordinary alignment fixture"
              ( RegularRetirement.prepareDisappearanceRegularRetirement
                  authority
                  fixture.herald
              )
          )
      historicalWriter =
        historicalRegularWriter fixture.application.fixtureApplicationProcess fixture.herald
      destinationDelta =
        checkedPure
          "ordinary alignment destination Delta"
          (mkDeltaId (fixtureIdentifierBytes 0xf4))
      destinationIncarnation =
        checkedPure
          "ordinary alignment destination incarnation"
          (mkStoreIncarnationId (fixtureIdentifierBytes 0xf5))
      unknownOccurrence =
        checkedPure
          "unknown ordinary alignment occurrence"
          (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xf6))
      unknownDelta =
        checkedPure
          "unknown ordinary alignment destination Delta"
          (mkDeltaId (fixtureIdentifierBytes 0xf7))
      unknownIncarnation =
        checkedPure
          "unknown ordinary alignment destination incarnation"
          (mkStoreIncarnationId (fixtureIdentifierBytes 0xf8))
      withDestination =
        replaceStartupStoreState
          ( Store.commitStoreBootstrap
              ( checkedPure
                  "bootstrap ordinary alignment destination"
                  ( Store.prepareStoreBootstrap
                      [ Store.localStoreSpec
                          ( Store.ApplicationReader
                              fixture.application.fixtureApplicationProcess
                              SortDefinitionRole
                          )
                          destinationDelta
                          regularSortId
                          genesisRegularOccurrence
                          destinationIncarnation,
                        Store.localStoreSpec
                          ( Store.ApplicationReader
                              fixture.application.fixtureApplicationProcess
                              SortDefinitionRole
                          )
                          unknownDelta
                          regularSortId
                          unknownOccurrence
                          unknownIncarnation
                      ]
                      (startupStoreState retired)
                  )
              )
          )
          retired
      destinationSlot =
        required
          "ordinary alignment destination Store"
          (Store.lookupStoreSlot destinationDelta (startupStoreState withDestination))
      unknownDestinationSlot =
        required
          "unknown ordinary alignment destination Store"
          (Store.lookupStoreSlot unknownDelta (startupStoreState withDestination))
      ordinaryValue =
        checkedPure
          "retired ordinary alignment value"
          (recordValue [(regularKeyField, boolValue True)])
      publicationAt sequenceNumber =
        checkedPure
          "checked retired ordinary alignment publication"
          ( mkCheckedPublication
              regularDescriptor
              ( publicationId
                  historicalWriter.historicalRegularWriterNabla
                  historicalWriter.historicalRegularWriterAuthority
                  localHerald
                  (nablaSequence sequenceNumber)
              )
              ordinaryValue
          )
      evidenceWith source occurrence strength publication =
        checkedPure
          "retired ordinary alignment evidence"
          ( AlignmentProtocol.retainedPublicationEvidence
              (checkedPublicationId publication)
              source
              regularSortId
              occurrence
              (checkedPublicationCanonicalValue publication)
              strength
              historicalWriter.historicalRegularWriterTopology
              (controlIndex 0)
              Nothing
          )
      evidenceFor =
        evidenceWith
          fixture.application.fixtureApplicationProcess
          genesisRegularOccurrence
      continuingEvidence = evidenceFor Normal (publicationAt 705)
      snapshotRepresentative = evidenceFor Weak (publicationAt 706)
      snapshotStrengthWitness = evidenceFor Normal (publicationAt 707)
      snapshotEvidence =
        checkedPure
          "retired ordinary alignment snapshot evidence"
          ( AlignmentProtocol.retainedStateEvidence
              snapshotRepresentative
              snapshotStrengthWitness
              Normal
          )
      unknownEvidence =
        evidenceWith
          fixture.application.fixtureApplicationProcess
          unknownOccurrence
          Normal
          (publicationAt 708)
      malformedEvidence =
        evidenceFor
          Normal
          (definitionPublicationAt fixture.application.fixtureApplicationWriter 709)
      unknownResult =
        applyAlignmentTranscriptResult
          []
          (Just unknownEvidence)
          unknownDestinationSlot
          withDestination
      malformedResult =
        applyAlignmentTranscriptResult
          []
          (Just malformedEvidence)
          destinationSlot
          withDestination
      (continuingBeforeRedefinition, _, _) =
        applyAlignmentEvidence continuingEvidence destinationSlot withDestination
      (snapshotBeforeRedefinition, _, _) =
        applyAlignmentSnapshotEvidence [snapshotEvidence] destinationSlot withDestination
      redefinition = installDefinition fixture.application 2 withDestination
      redefined = redefinition.installedHerald
      redefinedDestinationSlot =
        required
          "ordinary alignment destination after redefinition"
          (Store.lookupStoreSlot destinationDelta (startupStoreState redefined))
      (continuingAfterRedefinition, _, _) =
        applyAlignmentEvidence continuingEvidence redefinedDestinationSlot redefined
      (snapshotAfterRedefinition, _, _) =
        applyAlignmentSnapshotEvidence [snapshotEvidence] redefinedDestinationSlot redefined
      continuingBeforeApplied = alignmentAppliedEvidence continuingBeforeRedefinition
      snapshotBeforeApplied = alignmentAppliedEvidence snapshotBeforeRedefinition
      continuingAfterApplied = alignmentAppliedEvidence continuingAfterRedefinition
      snapshotAfterApplied = alignmentAppliedEvidence snapshotAfterRedefinition
  assertBool
    "the source fixture retains the checked original writer cut"
    ( isJust
        ( GraphProgress.lookupInstalledTopologyCut
            historicalWriter.historicalRegularWriterTopology
            historicalWriter.historicalRegularWriterProgress
        )
    )
  assertBool
    "the receiving Herald does not import the original writer graph"
    (historicalWriter.historicalRegularWriterGraph /= startupGraphState withDestination)
  assertBool
    "alignment applies retained data without the original writer cut at the receiver"
    ( not
        ( isJust
            ( GraphProgress.lookupInstalledTopologyCut
                historicalWriter.historicalRegularWriterTopology
                (startupStructuralProgressState withDestination)
            )
        )
    )
  assertEqual
    "the historical ordinary occurrence is no longer effective before redefinition"
    Nothing
    (SortRegistry.lookupEffectiveSort regularSortId (startupSortRegistryState withDestination))
  assertEqual
    "alignment retains one terminal ordinary change before redefinition"
    [AlignmentTransferState.StoreTerminallyIgnored]
    ( AlignmentTransferState.appliedDestinationEvidenceDisposition
        <$> continuingBeforeApplied
    )
  assertEqual
    "alignment retains terminal snapshot representative and strength witness before redefinition"
    [ ( AlignmentTransferState.AppliedSnapshotRepresentative,
        AlignmentTransferState.StoreTerminallyIgnored
      ),
      ( AlignmentTransferState.AppliedSnapshotStrengthWitness,
        AlignmentTransferState.StoreTerminallyIgnored
      )
    ]
    [ ( AlignmentTransferState.appliedDestinationEvidenceKind applied,
        AlignmentTransferState.appliedDestinationEvidenceDisposition applied
      )
    | applied <- snapshotBeforeApplied
    ]
  assertBool
    "terminal ordinary change and snapshot evidence leave owners unchanged before redefinition"
    ( startupStoreState continuingBeforeRedefinition == startupStoreState withDestination
        && startupControlledState continuingBeforeRedefinition
          == startupControlledState withDestination
        && startupStoreState snapshotBeforeRedefinition == startupStoreState withDestination
        && startupControlledState snapshotBeforeRedefinition
          == startupControlledState withDestination
    )
  assertEqual
    "equal redefinition installs only the successor occurrence"
    (Just fixture.successorOccurrence)
    ( SortRegistry.registryEntryOccurrenceId
        <$> SortRegistry.lookupEffectiveSort
          regularSortId
          (startupSortRegistryState redefined)
    )
  assertEqual
    "alignment retains one terminal old-occurrence change after redefinition"
    [AlignmentTransferState.StoreTerminallyIgnored]
    ( AlignmentTransferState.appliedDestinationEvidenceDisposition
        <$> continuingAfterApplied
    )
  assertEqual
    "alignment retains terminal snapshot representative and strength witness after redefinition"
    [ ( AlignmentTransferState.AppliedSnapshotRepresentative,
        AlignmentTransferState.StoreTerminallyIgnored
      ),
      ( AlignmentTransferState.AppliedSnapshotStrengthWitness,
        AlignmentTransferState.StoreTerminallyIgnored
      )
    ]
    [ ( AlignmentTransferState.appliedDestinationEvidenceKind applied,
        AlignmentTransferState.appliedDestinationEvidenceDisposition applied
      )
    | applied <- snapshotAfterApplied
    ]
  assertBool
    "terminal old-occurrence change and snapshot evidence leave owners unchanged after redefinition"
    ( startupStoreState continuingAfterRedefinition == startupStoreState redefined
        && startupControlledState continuingAfterRedefinition
          == startupControlledState redefined
        && startupStoreState snapshotAfterRedefinition == startupStoreState redefined
        && startupControlledState snapshotAfterRedefinition
          == startupControlledState redefined
    )
  mapM_
    ( \(label, result) -> case result of
        Left problem ->
          assertEqual
            label
            AlignmentTransfer.AlignmentTransferRemoteProtocolViolation
            problem
        Right _ -> assertFailure (label <> ": invalid evidence was accepted")
    )
    [ ("a nonhistorical occurrence remains a remote violation", unknownResult),
      ("a malformed historical value remains a remote violation", malformedResult)
    ]

caseAlignmentStructuralTerminalRollback :: Assertion
caseAlignmentStructuralTerminalRollback =
  mapM_
    exercise
    [ (NablaRole, NablaCarrier, 703, 0xe1),
      (DeltaRole, DeltaCarrier, 704, 0xe2)
    ]
  where
    fixture = acceptanceFixture
    authority =
      required
        "structural alignment rollback retirement authority"
        ( Disappearance.preparedProjectedRegularResolutionAuthority
            ( checkedPure
                "prepare structural alignment rollback retirement authority"
                ( Disappearance.prepareProjectedRegularResolution
                    fixture.terminal
                    fixture.leaf
                )
            )
        )
    (retired, _) =
      RegularRetirement.commitRegularRetirement
        ( checkedPure
            "retire structural alignment rollback fixture"
            ( RegularRetirement.prepareDisappearanceRegularRetirement
                authority
                fixture.herald
            )
        )

    exercise (role, carrierRole, sequenceNumber, objectByte) = do
      let carrier = predefinedCarrier role
          descriptor = primordialReplicaDescriptor carrier
          occurrence = primordialReplicaOccurrenceId carrier
          destinationSlot = carrierStoreSlotFor role retired
          writerFact = structuralWriterFact role retired
          topology =
            GraphProgress.structuralLastInstalledCutId
              (startupStructuralProgressState retired)
          prerequisite = Controlled.rootFactControlPrerequisite writerFact
          identifier =
            publicationId
              (structuralWriterNabla writerFact)
              (Controlled.rootFactAuthority writerFact)
              localHerald
              (nablaSequence sequenceNumber)
          structuralCandidate =
            Publication.proposeStructuralOccurrence
              ( GraphProgress.structuralAppliedVector
                  (startupStructuralProgressState retired)
              )
              (startupPublicationState retired)
          structuralOccurrence =
            Publication.structuralCandidateOccurrence structuralCandidate
          structuralPredecessor =
            Publication.structuralCandidatePredecessor structuralCandidate
          objectField = checkedPure "alignment structural object field" (mkFieldName "object_id")
          labelField = checkedPure "alignment structural label field" (mkFieldName "label")
          sortField = checkedPure "alignment structural sort field" (mkFieldName "sort_id")
          sequencingField =
            checkedPure
              "alignment structural sequencing field"
              (mkFieldName "sequencing_object")
          value =
            checkedPure
              "alignment stale structural reference value"
              ( recordValue
                  ( [ (objectField, globalUniqueIdValue (checkedPure "alignment structural object" (mkGlobalUniqueId (fixtureIdentifierBytes objectByte)))),
                      (labelField, labelValue (VoidLabel, 0)),
                      (sortField, bytesValue (sortIdBytes regularSortId))
                    ]
                      <> case carrierRole of
                        NablaCarrier ->
                          [(sequencingField, optionalGlobalUniqueIdValue Nothing)]
                        DeltaCarrier -> []
                        _ -> error "alignment rollback fixture has a non-Nabla/Delta carrier"
                  )
              )
          publication =
            checkedPure
              "alignment stale structural checked publication"
              (mkCheckedPublication descriptor identifier value)
          digest =
            checkedPure
              "alignment stale structural publication digest"
              ( structuralPublicationDigestForSemantics
                  descriptor
                  occurrence
                  publication
                  fixture.application.fixtureApplicationProcess
                  Normal
                  topology
                  prerequisite
              )
          stamp =
            checkedPure
              "alignment stale structural occurrence stamp"
              ( mkStructuralOccurrenceStamp
                  structuralOccurrence
                  structuralPredecessor
                  identifier
                  digest
                  carrierRole
              )
          evidence =
            checkedPure
              "alignment stale structural retained evidence"
              ( AlignmentProtocol.retainedPublicationEvidence
                  identifier
                  fixture.application.fixtureApplicationProcess
                  (checkedPublicationSort publication)
                  occurrence
                  (checkedPublicationCanonicalValue publication)
                  Normal
                  topology
                  prerequisite
                  (Just stamp)
              )
          controlledBefore = startupControlledState retired
          controlledObservation =
            checkedPure
              "check speculative alignment controlled observation"
              (Controlled.checkControlledObservation descriptor occurrence publication)
          (speculativeControlled, speculativeDisposition) =
            Controlled.commitControlledPeerObservation
              ( checkedPure
                  "prepare speculative alignment controlled observation"
                  ( Controlled.prepareControlledPeerObservation
                      controlledObservation
                      controlledBefore
                  )
              )
          (successor, _, _) =
            applyAlignmentEvidence evidence destinationSlot retired
          applied = alignmentAppliedEvidence successor
      assertBool
        (show role <> " structural evidence is genuinely pre-Resolve")
        (prerequisite < fixture.resolveIndex)
      assertEqual
        (show role <> " standalone controlled admission establishes new state")
        Controlled.ControlledPeerEstablished
        speculativeDisposition
      assertBool
        (show role <> " standalone controlled admission is not state-identical")
        (speculativeControlled /= controlledBefore)
      assertBool
        (show role <> " terminal Store rejection leaves Store unchanged")
        (startupStoreState successor == startupStoreState retired)
      assertBool
        (show role <> " terminal Store rejection rolls back speculative Controlled state")
        (startupControlledState successor == controlledBefore)
      assertEqual
        (show role <> " retains exact terminal alignment evidence")
        [AlignmentTransferState.StoreTerminallyIgnored]
        (fmap AlignmentTransferState.appliedDestinationEvidenceDisposition applied)

    structuralWriterFact role state =
      required
        ("controlled writer fact for " <> show role)
        ( find
            ( \fact ->
                Controlled.rootFactProcessEpoch fact
                  == fixture.application.fixtureApplicationProcess
                  && Controlled.rootFactCatalogueRole fact == role
                  && case Controlled.rootFactRole fact of
                    Controlled.ControlledWriter {} -> True
                    Controlled.ControlledReader {} -> False
            )
            (Controlled.controlledRootFacts (startupControlledState state))
        )

    structuralWriterNabla fact = case Controlled.rootFactRole fact of
      Controlled.ControlledWriter writer _ -> writer
      Controlled.ControlledReader {} -> error "structural writer fixture selected a reader"

caseAlignmentControlledSuppressedEvidence :: Assertion
caseAlignmentControlledSuppressedEvidence = do
  let fixture = acceptanceFixture
      publication =
        definitionPublicationAt fixture.application.fixtureApplicationWriter 702
      (prototypeHerald, prototypeSource, _) =
        applyAlignmentDefinitionEvidence
          fixture
          (controlIndex 0)
          publication
          fixture.herald
  prototypeEvidence <- case alignmentAppliedEvidence prototypeHerald of
    [evidence] -> pure evidence
    observed ->
      assertFailure
        ( "expected one prototype destination-evidence fact, got "
            <> show (length observed)
        )
  (subscription, sourceTranscript) <-
    case AlignmentTransferState.sourceSubscriptionEntries prototypeSource of
      [entry] -> pure entry
      observed ->
        assertFailure
          ( "expected one prototype source transcript, got "
              <> show (length observed)
          )
  destinationTranscript <-
    case AlignmentTransferState.destinationSubscriptionEntries
      ( Alignment.alignmentTransferState
          (startupAlignmentState prototypeHerald)
      ) of
      [(observedSubscription, transcript)] -> do
        assertEqual
          "prototype source and destination retain the same subscription"
          subscription
          observedSubscription
        pure transcript
      observed ->
        assertFailure
          ( "expected one prototype destination transcript, got "
              <> show (length observed)
          )
  attempt <-
    maybe
      (assertFailure "prototype destination transcript has no attempt")
      pure
      (AlignmentTransferState.destinationSubscriptionAttempt destinationTranscript)
  let preparedAttempt =
        checkedPure
          "rebuild exact controlled-suppression destination attempt"
          ( AlignmentTransferState.prepareDestinationAttempt
              attempt
              AlignmentTransferState.emptyState
          )
      (attemptedDestination, _) =
        AlignmentTransferState.commitDestinationAttempt preparedAttempt
      preparedSourceBase =
        checkedPure
          "rebuild exact controlled-suppression source base"
          ( AlignmentTransferState.prepareSourceSubscription
              (AlignmentTransferState.sourceSubscriptionSubscribe sourceTranscript)
              (AlignmentTransferState.sourceSubscriptionBaseRevision sourceTranscript)
              (AlignmentTransferState.sourceSubscriptionSnapshotFacts sourceTranscript)
              AlignmentTransferState.SourceLiveImmediate
              AlignmentTransferState.emptyState
          )
      (rebuiltSourceBase, _) =
        AlignmentTransferState.commitSourceSubscription preparedSourceBase
      preparedSourceChanges =
        checkedPure
          "rebuild exact controlled-suppression source changes"
          ( AlignmentTransferState.prepareSourceChanges
              subscription
              (AlignmentTransferState.sourceSubscriptionSentChanges sourceTranscript)
              ( AlignmentProtocol.alignmentLive
                  subscription
                  (AlignmentTransferState.sourceSubscriptionSentThrough sourceTranscript)
              )
              rebuiltSourceBase
          )
      (rebuiltSource, _) =
        AlignmentTransferState.commitSourceChanges preparedSourceChanges
      unsettledTransfer =
        foldl
          applyAlignmentDestinationControl
          attemptedDestination
          (AlignmentTransferState.sourceRetryControls subscription rebuiltSource)
      controlledEvidence =
        AlignmentTransferState.appliedDestinationEvidence
          subscription
          ( AlignmentTransferState.appliedDestinationEvidenceDestination
              prototypeEvidence
          )
          ( AlignmentTransferState.appliedDestinationEvidencePublication
              prototypeEvidence
          )
          (AlignmentTransferState.appliedDestinationEvidenceStrength prototypeEvidence)
          ( AlignmentTransferState.appliedDestinationEvidenceSourceRevision
              prototypeEvidence
          )
          (AlignmentTransferState.appliedDestinationEvidenceKind prototypeEvidence)
          AlignmentTransferState.ControlledSuppressed
      preparedEvidence =
        checkedPure
          "retain exact controlled-suppression destination evidence"
          ( AlignmentTransferState.prepareAppliedDestinationEvidence
              [controlledEvidence]
              unsettledTransfer
          )
      (settledTransfer, evidenceDisposition) =
        AlignmentTransferState.commitAppliedDestinationEvidence preparedEvidence
      unsettledView =
        AlignmentDisappearance.alignmentRegularRetirementDisappearanceView
          regularSubject
          fixture.resolveIndex
          (Alignment.replaceAlignmentTransferState unsettledTransfer Alignment.emptyState)
      settledView =
        AlignmentDisappearance.alignmentRegularRetirementDisappearanceView
          regularSubject
          fixture.resolveIndex
          (Alignment.replaceAlignmentTransferState settledTransfer Alignment.emptyState)
      unsettledClasses =
        fmap
          Protocol.disappearanceBlockerClass
          (AlignmentDisappearance.alignmentDisappearanceBlockers unsettledView)
  assertBool
    "the equivalent transcript blocks while its exact change lacks terminal evidence"
    (Protocol.AlignmentChangeLogBlocker `elem` unsettledClasses)
  assertEqual
    "retaining controlled suppression advances only the evidence ledger"
    AlignmentTransferState.AlignmentTransferAdvanced
    evidenceDisposition
  assertEqual
    "controlled suppression preserves the exact destination transcript"
    (AlignmentTransferState.destinationSubscriptionEntries unsettledTransfer)
    (AlignmentTransferState.destinationSubscriptionEntries settledTransfer)
  assertEqual
    "the rebuilt settled destination owner is internally valid"
    (Right ())
    (AlignmentTransferState.validateAlignmentTransferState settledTransfer)
  assertEqual
    "exact ControlledSuppressed evidence makes the retired transcript inert"
    []
    (AlignmentDisappearance.alignmentDisappearanceBlockers settledView)

applyAlignmentDestinationControl ::
  AlignmentTransferState.State ->
  AlignmentProtocol.AlignmentControl ->
  AlignmentTransferState.State
applyAlignmentDestinationControl predecessor control = case control of
  AlignmentProtocol.AlignmentSnapshotStarted start ->
    commit (AlignmentTransferState.prepareDestinationSnapshotStart start predecessor)
  AlignmentProtocol.AlignmentSnapshotChunkTransferred chunk ->
    commit (AlignmentTransferState.prepareDestinationSnapshotChunk chunk predecessor)
  AlignmentProtocol.AlignmentSnapshotEnded end ->
    commit (AlignmentTransferState.prepareDestinationSnapshotEnd end predecessor)
  AlignmentProtocol.AlignmentChangeTransferred change ->
    commit (AlignmentTransferState.prepareDestinationChange change predecessor)
  AlignmentProtocol.AlignmentLiveAdvertised live ->
    commit (AlignmentTransferState.prepareDestinationLive live predecessor)
  _ -> predecessor
  where
    commit prepared =
      fst
        ( AlignmentTransferState.commitDestinationWork
            (checkedPure "rebuild exact alignment destination transcript" prepared)
        )

cancelAlignmentTransfer ::
  AlignmentProtocol.AlignmentCancel ->
  AlignmentTransferState.State ->
  AlignmentTransferState.State
cancelAlignmentTransfer cancellation predecessor =
  fst
    ( AlignmentTransferState.commitAlignmentCancellation
        ( checkedPure
            "cancel alignment retirement fixture transcript"
            ( AlignmentTransferState.prepareAlignmentCancellation
                cancellation
                predecessor
            )
        )
    )

applyAlignmentDefinitionEvidence ::
  AcceptanceFixture ->
  ControlIndex ->
  CheckedPublication ->
  HeraldState ->
  (HeraldState, AlignmentTransferState.State, AlignmentTransferState.State)
applyAlignmentDefinitionEvidence fixture prerequisite publication predecessor =
  applyAlignmentEvidence evidence destinationSlot predecessor
  where
    destinationSlot = carrierStoreSlot predecessor
    evidence =
      checkedPure
        "alignment fixture retained publication evidence"
        ( AlignmentProtocol.retainedPublicationEvidence
            (checkedPublicationId publication)
            fixture.application.fixtureApplicationProcess
            (checkedPublicationSort publication)
            (Store.storeSlotOccurrenceId destinationSlot)
            (checkedPublicationCanonicalValue publication)
            Normal
            topology
            prerequisite
            Nothing
        )
    topology =
      GraphProgress.structuralLastInstalledCutId
        (startupStructuralProgressState predecessor)

applyAlignmentEvidence ::
  AlignmentProtocol.RetainedPublicationEvidence ->
  Store.StoreSlot ->
  HeraldState ->
  (HeraldState, AlignmentTransferState.State, AlignmentTransferState.State)
applyAlignmentEvidence evidence destinationSlot predecessor =
  applyAlignmentTranscript [] (Just evidence) destinationSlot predecessor

applyAlignmentSnapshotEvidence ::
  [AlignmentProtocol.RetainedStateEvidence] ->
  Store.StoreSlot ->
  HeraldState ->
  (HeraldState, AlignmentTransferState.State, AlignmentTransferState.State)
applyAlignmentSnapshotEvidence snapshot destinationSlot predecessor =
  applyAlignmentTranscript snapshot Nothing destinationSlot predecessor

applyAlignmentTranscript ::
  [AlignmentProtocol.RetainedStateEvidence] ->
  Maybe AlignmentProtocol.RetainedPublicationEvidence ->
  Store.StoreSlot ->
  HeraldState ->
  (HeraldState, AlignmentTransferState.State, AlignmentTransferState.State)
applyAlignmentTranscript snapshot continuing destinationSlot predecessor =
  checkedPure
    "apply alignment evidence transcript"
    (applyAlignmentTranscriptResult snapshot continuing destinationSlot predecessor)

applyAlignmentTranscriptResult ::
  [AlignmentProtocol.RetainedStateEvidence] ->
  Maybe AlignmentProtocol.RetainedPublicationEvidence ->
  Store.StoreSlot ->
  HeraldState ->
  Either
    AlignmentTransfer.AlignmentTransferCoordinatorProblem
    (HeraldState, AlignmentTransferState.State, AlignmentTransferState.State)
applyAlignmentTranscriptResult snapshot continuing destinationSlot predecessor = do
  (destinationAtBase, baseReplies) <-
    foldM applyControl (withAttempt, []) baseControls
  let baseAcknowledgements =
        [ acknowledgement
        | AlignmentProtocol.AlignmentAcknowledged acknowledgement <- baseReplies
        ]
      sourceAtBase =
        foldl applyAcknowledgement sourceAtBaseBeforeAcknowledgement baseAcknowledgements
  case continuing of
    Nothing ->
      Right
        ( destinationAtBase,
          sourceAtBaseBeforeAcknowledgement,
          sourceAtBase
        )
    Just evidence -> do
      let change = AlignmentProtocol.alignmentChange subscription revision evidence
          preparedChanges =
            checkedPure
              "prepare alignment retirement continuing change"
              ( AlignmentTransferState.prepareSourceChanges
                  subscription
                  [change]
                  (AlignmentProtocol.alignmentLive subscription revision)
                  sourceAtBase
              )
          changeControls =
            AlignmentTransferState.preparedSourceChangeControls preparedChanges
          (sourceBeforeAcknowledgement, _) =
            AlignmentTransferState.commitSourceChanges preparedChanges
      (destinationSuccessor, changeReplies) <-
        foldM applyControl (destinationAtBase, []) changeControls
      let changeAcknowledgements =
            [ acknowledgement
            | AlignmentProtocol.AlignmentAcknowledged acknowledgement <- changeReplies,
              AlignmentProtocol.alignmentAckAppliedSourceStoreRevision acknowledgement
                == revision
            ]
          sourceAfterAcknowledgement =
            foldl applyAcknowledgement sourceBeforeAcknowledgement changeAcknowledgements
      Right
        (destinationSuccessor, sourceBeforeAcknowledgement, sourceAfterAcknowledgement)
  where
    local = checkedLocalHeraldEpoch fixtureCheckedGenesis
    remote =
      case filter (/= local) (checkedActiveHeraldEpochs fixtureCheckedGenesis) of
        candidate : _ -> candidate
        [] -> error "alignment retirement fixture has no remote Herald"
    remoteId = checkedPure "alignment fixture remote HeraldId" (mkHeraldId (fixtureIdentifierBytes 231))
    binding =
      PeerBinding
        remoteId
        remote
        firstPeerBindingGeneration
        (PeerCandidate remoteId remote (ConnectionNonce 1))
    destination =
      AlignmentProtocol.destinationStore
        (Store.storeSlotDelta destinationSlot)
        (Store.storeSlotIncarnation destinationSlot)
    sourceGeneration =
      checkedPure
        "alignment fixture source generation"
        (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 232))
    destinationGeneration =
      checkedPure
        "alignment fixture destination generation"
        (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 233))
    sourceStore =
      checkedPure
        "alignment fixture source Store"
        (mkStoreIncarnationId (fixtureIdentifierBytes 234))
    obligation =
      checkedPure
        "alignment fixture obligation"
        ( AlignmentProtocol.alignmentObligation
            ( AlignmentProtocol.alignmentObligationId
                local
                AlignmentProtocol.firstAlignmentObligationSequence
            )
            ( StructuralConsequence.structuralOccurrenceCause
                (structuralOccurrenceId local firstStructuralSequence)
            )
            (Store.storeSlotSortId destinationSlot)
            (Store.storeSlotOccurrenceId destinationSlot)
            destinationGeneration
            (destination :| [])
            sourceGeneration
            Normal
        )
    subscription =
      AlignmentProtocol.alignmentSubscriptionId
        local
        AlignmentProtocol.firstAlignmentSubscriptionSequence
    certificate =
      AlignmentProtocol.historicalCertificate
        sourceGeneration
        sourceStore
        (DomainAlignment.deriveMemberReadyEvidenceDigest "alignment-retirement-ready")
        (DomainAlignment.deriveBootstrapEvidenceDigest "alignment-retirement-bootstrap")
        DomainAlignment.initialStoreRevision
    attempt =
      checkedPure
        "alignment fixture attempt"
        (AlignmentProtocol.alignmentAttempt obligation subscription remote certificate)
    alignment0 = startupAlignmentState predecessor
    transfer0 = Alignment.alignmentTransferState alignment0
    preparedAttempt =
      checkedPure
        "retain alignment fixture destination attempt"
        (AlignmentTransferState.prepareDestinationAttempt attempt transfer0)
    (transfer1, _) = AlignmentTransferState.commitDestinationAttempt preparedAttempt
    subscribe = AlignmentTransferState.preparedDestinationAttemptSubscribe preparedAttempt
    withAttempt =
      replaceStartupAlignmentState
        (Alignment.replaceAlignmentTransferState transfer1 alignment0)
        predecessor
    revision = DomainAlignment.nextStoreRevision DomainAlignment.initialStoreRevision
    preparedSource =
      checkedPure
        "prepare alignment retirement source base"
        ( AlignmentTransferState.prepareSourceSubscription
            subscribe
            DomainAlignment.initialStoreRevision
            snapshot
            AlignmentTransferState.SourceLiveImmediate
            AlignmentTransferState.emptyState
        )
    baseControls = AlignmentTransferState.preparedSourceSubscriptionControls preparedSource
    (sourceAtBaseBeforeAcknowledgement, _) =
      AlignmentTransferState.commitSourceSubscription preparedSource
    applyControl (state, accumulatedReplies) control =
      ( \(successor, effects) ->
          (successor, accumulatedReplies <> alignmentReplies effects)
      )
        <$> AlignmentTransfer.applyAlignmentTransferControl binding control state
    applyAcknowledgement source acknowledgement =
      fst
        ( AlignmentTransferState.commitSourceAcknowledgement
            ( checkedPure
                "apply terminal alignment acknowledgement at the source"
                ( AlignmentTransferState.prepareSourceAcknowledgement
                    acknowledgement
                    source
                )
            )
        )

alignmentReplies :: EffectBatch -> [AlignmentProtocol.AlignmentControl]
alignmentReplies effects =
  [ control
  | SendPeerControl _ (PeerAlignmentControl control) <- effectBatchMembers effects
  ]

alignmentAppliedEvidence :: HeraldState -> [AlignmentTransferState.AppliedDestinationEvidence]
alignmentAppliedEvidence =
  AlignmentTransferState.appliedDestinationEvidenceEntries
    . Alignment.alignmentTransferState
    . startupAlignmentState

data HistoricalRegularWriter = HistoricalRegularWriter
  { historicalRegularWriterGraph :: Graph.State,
    historicalRegularWriterProgress :: GraphProgress.StructuralProgressState,
    historicalRegularWriterNabla :: NablaId,
    historicalRegularWriterAuthority :: AuthorityEpoch,
    historicalRegularWriterTopology :: TopologyCutId
  }

-- Build one fully installed source Nabla tenure for the regular fixture sort.
-- The receiver deliberately lacks this original writer history: its admitted
-- alignment transfer carries checked Store data, while its Registry/Store
-- product still enforces the exact retired occurrence and suppression rules.
historicalRegularWriter :: ProcessEpochId -> HeraldState -> HistoricalRegularWriter
historicalRegularWriter process predecessor =
  HistoricalRegularWriter
    { historicalRegularWriterGraph = graph1,
      historicalRegularWriterProgress = advancedProgress,
      historicalRegularWriterNabla = dynamicNabla,
      historicalRegularWriterAuthority = structuralAuthorityEpoch occurrence cutId,
      historicalRegularWriterTopology = cutId
    }
  where
    registry = startupSortRegistryState predecessor
    projection = startupOracleProjectionState predecessor
    oracleView = OracleProjection.oracleView projection
    controlled = startupControlledState predecessor
    progress0 = startupStructuralProgressState predecessor
    graph0 = startupGraphState predecessor
    views =
      Reconciliation.reconciliationViews
        localHerald
        ( Map.fromList
            [ ( SortRegistry.registryEntrySortId entry,
                sortOccurrence
                  (SortRegistry.registryEntrySortId entry)
                  (SortRegistry.registryEntryOccurrenceId entry)
              )
            | entry <- SortRegistry.registryEntries registry
            ]
        )
        (Set.fromList (Graph.graphNonStructuralBaselineVertices graph0))
        ( Map.fromList
            [ (candidate, residence)
            | candidate <- OracleProjection.projectedProcessEpochs projection,
              OracleProjection.oracleViewProcessIsLive candidate oracleView,
              Just residence <-
                [OracleProjection.oracleViewProcessResidence candidate oracleView]
            ]
        )
        (Set.fromList (Controlled.controlledNormalPossessionEntries controlled))
    writerFact =
      required
        "historical regular Nabla carrier writer"
        ( find
            ( \fact ->
                Controlled.rootFactProcessEpoch fact == process
                  && Controlled.rootFactCatalogueRole fact == NablaRole
                  && case Controlled.rootFactRole fact of
                    Controlled.ControlledWriter {} -> True
                    Controlled.ControlledReader {} -> False
            )
            (Controlled.controlledRootFacts controlled)
        )
    carrierWriter = case Controlled.rootFactRole writerFact of
      Controlled.ControlledWriter writer _ -> writer
      Controlled.ControlledReader {} ->
        error "historical regular writer fixture selected a reader"
    carrier = predefinedCarrier NablaRole
    carrierDescriptor = primordialReplicaDescriptor carrier
    historicalCarrierOccurrence = primordialReplicaOccurrenceId carrier
    carrierSort =
      sortOccurrence
        (primordialReplicaSortId carrier)
        historicalCarrierOccurrence
    prerequisite = Controlled.rootFactControlPrerequisite writerFact
    sourceTopology = GraphProgress.structuralLastInstalledCutId progress0
    rootObjectValue =
      checkedPure
        "historical regular Nabla object"
        (mkGlobalUniqueId (fixtureIdentifierBytes 0xf2))
    rootObject = globalObjectIdFromGlobalUniqueId rootObjectValue
    dynamicNabla = nablaIdFromGlobalObjectId rootObject
    objectField = checkedPure "historical Nabla object field" (mkFieldName "object_id")
    labelField = checkedPure "historical Nabla label field" (mkFieldName "label")
    sortField = checkedPure "historical Nabla sort field" (mkFieldName "sort_id")
    sequencingField =
      checkedPure
        "historical Nabla sequencing field"
        (mkFieldName "sequencing_object")
    carrierValue =
      checkedPure
        "historical regular Nabla carrier value"
        ( recordValue
            [ (objectField, globalUniqueIdValue rootObjectValue),
              (labelField, labelValue ((ProcessLabel process, 0))),
              (sortField, bytesValue (sortIdBytes regularSortId)),
              (sequencingField, optionalGlobalUniqueIdValue Nothing)
            ]
        )
    carrierIdentifier =
      publicationId
        carrierWriter
        (Controlled.rootFactAuthority writerFact)
        localHerald
        (nablaSequence 704)
    carrierPublication =
      checkedPure
        "historical regular Nabla carrier publication"
        (mkCheckedPublication carrierDescriptor carrierIdentifier carrierValue)
    occurrence = structuralOccurrenceId localHerald firstStructuralSequence
    predecessorVector = GraphProgress.structuralAppliedVector progress0
    application =
      Reconciliation.structuralApplication
        occurrence
        predecessorVector
        carrierSort
        carrierPublication
        prerequisite
    preparedReconciliation =
      case checkedPure
        "prepare historical regular Nabla reconciliation"
        ( Reconciliation.prepareStructuralReconciliation
            views
            application
            Nothing
            (GraphProgress.structuralProgressReconciliation progress0)
        ) of
        Reconciliation.StructuralReady prepared -> prepared
        Reconciliation.StructuralHeld dependencies ->
          error
            ( "historical regular Nabla reconciliation held: "
                <> show dependencies
            )
    carrierDigest =
      checkedPure
        "historical regular Nabla digest"
        ( structuralPublicationDigestForSemantics
            carrierDescriptor
            historicalCarrierOccurrence
            carrierPublication
            process
            Normal
            sourceTopology
            prerequisite
        )
    stamp =
      checkedPure
        "historical regular Nabla stamp"
        ( mkStructuralOccurrenceStamp
            occurrence
            predecessorVector
            carrierIdentifier
            carrierDigest
            NablaCarrier
        )
    applied =
      checkedPure
        "apply historical regular Nabla"
        ( GraphProgress.prepareStructuralApplication
            stamp
            sourceTopology
            preparedReconciliation
            graph0
            progress0
        )
    (graph1, progress1, _) = GraphProgress.commitStructuralApplication applied
    reportFor reporter =
      GraphProtocol.structuralAppliedReport
        reporter
        (GraphProgress.structuralAppliedVector progress1)
        (GraphProgress.structuralAppliedControlPrefix progress1)
    remoteHeralds = filter (/= localHerald) activeHeralds
    progress2 = foldl retainReport progress1 remoteHeralds
    retainReport current reporter =
      fst
        ( GraphProgress.commitStructuralReport
            ( checkedPure
                "retain historical regular writer structural report"
                (GraphProgress.prepareStructuralReport (reportFor reporter) current)
            )
        )
    preparedProposal =
      case checkedPure
        "propose historical regular writer topology cut"
        ( GraphProgress.prepareTopologyCutProposal
            views
            ( GraphProgress.topologyControlCheckpoint
                (GraphProgress.structuralAppliedControlPrefix progress2)
                "historical-regular-writer"
            )
            progress2
        ) of
        GraphProgress.TopologyCutProposalReady prepared -> prepared
        GraphProgress.TopologyCutProposalUnavailable ->
          error "historical regular writer topology proposal unavailable"
        GraphProgress.TopologyCutProposalHeld dependencies ->
          error
            ( "historical regular writer topology proposal held: "
                <> show dependencies
            )
    announced = GraphProgress.preparedTopologyCutProposalAnnounce preparedProposal
    cutId = GraphProtocol.topologyCutAnnounceId announced
    progress3 = GraphProgress.commitTopologyCutProposal preparedProposal
    progress4 = foldl retainAcceptance progress3 remoteHeralds
    retainAcceptance current reporter =
      GraphProgress.commitTopologyCutAcceptance
        ( checkedPure
            "retain historical regular writer topology acceptance"
            ( GraphProgress.prepareTopologyCutAcceptance
                (GraphProtocol.topologyCutAcceptance cutId (reportFor reporter))
                current
            )
        )
    installedProgress =
      GraphProgress.commitTopologyCutEstablishment
        ( checkedPure
            "establish historical regular writer topology cut"
            (GraphProgress.prepareTopologyCutEstablishment progress4)
        )
    advancedProgress =
      fst
        ( GraphProgress.commitStructuralControlProgress
            ( checkedPure
                "advance historical writer through regular Resolve"
                ( GraphProgress.prepareStructuralControlProgress
                    acceptanceFixture.resolveIndex
                    installedProgress
                )
            )
        )

caseForeignSystemAuthorityRejected :: Assertion
caseForeignSystemAuthorityRejected = do
  let fixture = acceptanceFixture
      foreignSystem =
        checkedPure
          "foreign regular-retirement SystemId"
          (mkSystemId (fixtureIdentifierBytes 241))
      foreignOutcome =
        checkedPure
          "resolve regular disappearance subject under another SystemId"
          ( resolveDisappearanceSubject
              foreignSystem
              fixture.resolveIndex
              regularSubject
          )
      foreignTerminal =
        Protocol.ProjectedDisappearanceResolved
          fixtureProbe
          foreignOutcome
          fixture.resolveIndex
      foreignLeafPrepared =
        checkedPure
          "prepare foreign-SystemId retirement authority"
          ( Disappearance.prepareProjectedRegularResolution
              foreignTerminal
              fixture.leaf
          )
      foreignAuthority =
        required
          "foreign-SystemId regular-retirement authority"
          ( Disappearance.preparedProjectedRegularResolutionAuthority
              foreignLeafPrepared
          )
      foreignSuccessor =
        Disappearance.regularRetirementAuthoritySuccessorOccurrence
          foreignAuthority
      result =
        RegularRetirement.prepareDisappearanceRegularRetirement
          foreignAuthority
          fixture.herald
  assertBool
    "the two deployment identities derive distinct successor occurrences"
    (foreignSuccessor /= fixture.successorOccurrence)
  case result of
    Left problem ->
      assertEqual
        "the Registry boundary rejects the foreign successor before commit"
        ( RegularRetirement.RegularRetirementSortRegistryProblem
            ( SortRegistry.RegularSortRetirementSuccessorOccurrenceMismatch
                regularSortId
                fixture.successorOccurrence
                foreignSuccessor
            )
        )
        problem
    Right _ -> assertFailure "foreign-SystemId retirement authority was accepted"

caseResolveHeldSuccessorRelease :: Assertion
caseResolveHeldSuccessorRelease = do
  let fixture = acceptanceFixture
      authority =
        required
          "Resolve-held retirement authority"
          ( Disappearance.preparedProjectedRegularResolutionAuthority
              ( checkedPure
                  "prepare Resolve-held retirement authority"
                  ( Disappearance.prepareProjectedRegularResolution
                      fixture.terminal
                      fixture.leaf
                  )
              )
          )
      (oracleBound, oracleBinding) = bindResolveHeldOracle fixture.herald
      (preResolveEntries, resolveEntry) =
        splitResolveEntry
          (canonicalRejectedOracleEntriesThrough fixture.resolveIndex)
      preResolveBatch = case preResolveEntries of
        first : remaining -> first :| remaining
        [] -> error "Resolve fixture has no pre-Resolve Oracle entries"
      (atPreResolve, _) =
        checkedPure
          "advance real Oracle ingress to immediately before Resolve"
          ( OracleAdvance.applyOracleIngress
              (OracleEntriesReceived oracleBinding preResolveBatch)
              oracleBound
          )
      (connected, peerBinding) = connectResolveHeldPeer atPreResolve
      (incomingDefinition, _, direction, incomingItem) =
        resolveHeldDefinitionItem fixture.resolveIndex connected
      incomingIdentifier = checkedPublicationId incomingDefinition
      (held, heldEffects) =
        checkedPure
          "admit Resolve-qualified equal definition before Oracle Resolve"
          ( verifiedStepHerald
              ( heraldInput
                  (monotonicInstant 2)
                  ( PeerInput
                      ( peerPublicationReceived
                          peerBinding
                          (peerPublicationItem incomingItem)
                      )
                  )
              )
              connected
          )
      retirementEvidence =
        checkedPure
          "capture Resolve-aware retirement evidence with held successor work"
          ( Evidence.regularRetirementEvidenceSnapshotForHerald
              regularSubject
              fixture.resolveIndex
              held
          )
      retirementPrepared =
        checkedPure
          "prepare retirement around Resolve-held successor work"
          ( RegularRetirement.prepareDisappearanceRegularRetirement
              authority
              held
          )
      (retiredHeld, retirementSummary) =
        RegularRetirement.commitRegularRetirement retirementPrepared
      (released, releaseEffects) =
        checkedPure
          "apply the exact canonical Resolve entry and release held work"
          ( OracleAdvance.applyOracleIngress
              (OracleEntriesReceived oracleBinding (resolveEntry :| []))
              retiredHeld
          )
      expectedPreResolve =
        controlIndex (controlIndexWord64 fixture.resolveIndex - 1)

  assertEqual
    "the real Oracle client cursor is immediately before Resolve"
    expectedPreResolve
    ( OracleProjection.oracleViewControlIndex
        (OracleProjection.oracleView (startupOracleProjectionState atPreResolve))
    )
  assertResolveHeldDefinition
    "before retirement"
    fixture.resolveIndex
    incomingDefinition
    direction
    held
  assertBool
    "holding the definition certifies receipt while preserving its pending exception"
    ( any
        ( \effect -> case effect of
            SendPeerControl binding (PeerStreamCompleted observedDirection progress) ->
              binding == peerBinding
                && observedDirection == direction
                && Receipt.receiptRetirementHighWater progress == Just 1
                && Receipt.receiptRetirementExceptions progress == Set.singleton 1
            _ -> False
        )
        (effectBatchMembers heldEffects)
    )
  assertBool
    "holding the definition does not acknowledge semantic completion"
    ( not
        ( any
            ( \effect -> case effect of
                SendPeerControl binding (PeerStreamCompleted observedDirection prefix) ->
                  binding == peerBinding
                    && observedDirection == direction
                    && streamPrefixSequence (streamCompletionPrefix prefix) == Just firstStreamSequence
                _ -> False
            )
            (effectBatchMembers heldEffects)
        )
    )
  assertBool
    "the dependency hold mutates no semantic owner"
    ( startupStoreState held == startupStoreState connected
        && startupSortRegistryState held == startupSortRegistryState connected
        && startupControlledState held == startupControlledState connected
        && startupGraphState held == startupGraphState connected
        && startupStructuralProgressState held
          == startupStructuralProgressState connected
        && startupAlignmentState held == startupAlignmentState connected
        && startupPlacementState held == startupPlacementState connected
        && startupApplicationState held == startupApplicationState connected
    )
  assertEqual
    "Resolve-aware retirement evidence excludes successor-held blockers"
    []
    (Evidence.disappearanceEvidenceSnapshotBlockers retirementEvidence)
  assertEqual
    "Resolve-aware matching work contains only the frozen local definition"
    [Publication.applicationPublicationHeraldPosition fixture.firstRecord]
    ( Protocol.matchingPublicationPosition
        <$> Evidence.disappearanceEvidenceSnapshotMatchingPublications
          retirementEvidence
    )
  assertEqual
    "the held remote definition cannot advance the frozen local cut"
    (Disappearance.regularRetirementAuthorityLocalPublicationCut authority)
    (Evidence.disappearanceEvidenceSnapshotLocalPublicationCut retirementEvidence)

  assertEqual
    "retirement counts only the frozen local matching write"
    1
    (RegularRetirement.regularRetirementSummaryMatchingWork retirementSummary)
  assertEqual
    "retirement advances the structural owner through Resolve"
    fixture.resolveIndex
    ( GraphProgress.structuralAppliedControlPrefix
        (startupStructuralProgressState retiredHeld)
    )
  assertEqual
    "retirement does not counterfeit Oracle receipt of Resolve"
    expectedPreResolve
    ( OracleProjection.oracleViewControlIndex
        (OracleProjection.oracleView (startupOracleProjectionState retiredHeld))
    )
  assertResolveHeldDefinition
    "immediately after retirement"
    fixture.resolveIndex
    incomingDefinition
    direction
    retiredHeld
  assertEqual
    "the retired state with successor-held work remains whole-state valid"
    (Right ())
    (validateHeraldState retiredHeld)

  releasedRecord <-
    maybe
      (assertFailure "released definition disappeared from Publication state")
      pure
      ( Publication.lookupIncomingPublication
          incomingIdentifier
          (startupPublicationState released)
      )
  assertEqual
    "the exact Resolve entry advances the real Oracle client projection"
    fixture.resolveIndex
    ( OracleProjection.oracleViewControlIndex
        (OracleProjection.oracleView (startupOracleProjectionState released))
    )
  assertEqual
    "the existing fixed point applies the formerly held definition"
    Publication.Applied
    (Publication.incomingPublicationDisposition releasedRecord)
  assertEqual
    "the released definition has no residual dependency"
    Set.empty
    (Publication.incomingPublicationDependencies releasedRecord)
  assertEqual
    "the released batch retains the exact Resolve floor"
    fixture.resolveIndex
    ( publicationBatchControlPrerequisite
        (Publication.incomingPublicationBatch releasedRecord)
    )
  assertEqual
    "the released equal definition establishes the derived successor occurrence"
    (Just fixture.successorOccurrence)
    ( SortRegistry.registryEntryOccurrenceId
        <$> SortRegistry.lookupEffectiveSort
          regularSortId
          (startupSortRegistryState released)
    )
  assertEqual
    "the successor Registry entry is induced by the released peer definition"
    (Just incomingIdentifier)
    ( SortRegistry.registryEntryPublicationId
        <$> SortRegistry.lookupEffectiveSort
          regularSortId
          (startupSortRegistryState released)
    )
  case resolveHeldDefinitionEnvelopes incomingDefinition released of
    [envelope] -> do
      assertEqual
        "the Store observation retains the exact Resolve floor"
        fixture.resolveIndex
        (Store.storeObservationEnvelopeControlPrerequisite envelope)
      assertEqual
        "the Store observation retains the exact carrier occurrence"
        carrierOccurrence
        (Store.storeObservationEnvelopeSortOccurrence envelope)
    observed ->
      assertFailure
        ( "expected one released routed Store observation, got "
            <> show (length observed)
        )
  releasedWatermarks <-
    pure
      ( checkedPure
          "read released Resolve-held stream watermarks"
          (PeerStream.incomingWatermarks direction (startupPeerStreamState released))
      )
  assertEqual
    "the released item remains received"
    (Just firstStreamSequence)
    (streamPrefixSequence (streamReceivedPrefix releasedWatermarks))
  assertEqual
    "the released item advances semantic completion"
    (Just firstStreamSequence)
    (streamPrefixSequence (streamCompletedPrefix releasedWatermarks))
  assertBool
    "the Resolve release emits the exact cumulative completion"
    ( any
        ( \effect -> case effect of
            SendPeerControl binding (PeerStreamCompleted observedDirection prefix) ->
              binding == peerBinding
                && observedDirection == direction
                && streamPrefixSequence (streamCompletionPrefix prefix) == Just firstStreamSequence
            _ -> False
        )
        (effectBatchMembers releaseEffects)
    )
  assertEqual
    "the released successor state remains whole-state valid"
    (Right ())
    (validateHeraldState released)

bindResolveHeldOracle :: HeraldState -> (HeraldState, OracleBinding)
bindResolveHeldOracle predecessor =
  (successor, binding)
  where
    attempt = case OracleClient.oracleClientActions (startupOracleClientState predecessor) of
      [ConnectAndHelloOracle candidate _] -> candidate
      observed ->
        error
          ( "expected one initial Oracle connection action, got "
              <> show observed
          )
    node = oracleContactNode (oracleConnectAttemptContact attempt)
    acceptance =
      oracleHelloAcceptance
        node
        (oracleObservedTerm 1)
        (controlIndex 7)
        (Just node)
        True
    (successor, effects) =
      checkedPure
        "bind the real Oracle client for Resolve-held work"
        ( OracleAdvance.applyOracleIngress
            (OracleHelloReceived attempt acceptance)
            predecessor
        )
    binding = case [ accepted
                   | RunOracleClientAction (BindOracleConnection observed accepted) <-
                       effectBatchMembers effects,
                     observed == attempt
                   ] of
      [accepted] -> accepted
      observed ->
        error
          ( "expected one Oracle binding effect, got "
              <> show observed
          )

connectResolveHeldPeer :: HeraldState -> (HeraldState, PeerBinding)
connectResolveHeldPeer predecessor =
  (successor, binding)
  where
    remoteId = heraldMemberId fixtureRemoteMember
    remoteEpoch = heraldMemberEpoch fixtureRemoteMember
    nonce = connectionNonce 16_001
    candidate = peerCandidate remoteId remoteEpoch nonce
    hello =
      peerHello
        (checkedSystemId fixtureCheckedGenesis)
        remoteId
        remoteEpoch
        nonce
        Set.empty
        (controlIndex 0)
        (checkedCatalogueDigest fixtureCheckedGenesis)
        (checkedInitialProjectionDigest (startupInitialBootstraps predecessor))
        Nothing
    (successor, effects) =
      checkedPure
        "establish the real remote peer for Resolve-held work"
        ( verifiedStepHerald
            ( heraldInput
                (monotonicInstant 1)
                ( PeerInput
                    ( currentPeerHelloReceived
                        predecessor
                        candidate
                        Set.empty
                        hello
                    )
                )
            )
            predecessor
        )
    binding = case [ accepted
                   | SetPeerCandidateDisposition _ (PeerHelloAccepted _ accepted) <-
                       effectBatchMembers effects
                   ] of
      [accepted] -> accepted
      observed ->
        error
          ( "expected one accepted peer binding, got "
              <> show observed
          )

resolveHeldDefinitionItem ::
  ControlIndex ->
  HeraldState ->
  ( CheckedPublication,
    PublicationDestination,
    StreamDirection,
    SequencedItem PeerPublication
  )
resolveHeldDefinitionItem prerequisite state =
  (publication, destination, direction, item)
  where
    remoteEpoch = heraldMemberEpoch fixtureRemoteMember
    source =
      required
        "one remote applied process for Resolve-held work"
        ( find
            ((== remoteEpoch) . appliedProcessResidence)
            (checkedInitialBootstraps (startupInitialBootstraps state))
        )
    root =
      required
        "remote sort-definition writer for Resolve-held work"
        ( find
            ( \candidate ->
                appliedRootCatalogueRole candidate == SortDefinitionRole
                  && case appliedRootRole candidate of
                    WriterRoot {} -> True
                    _ -> False
            )
            (appliedProcessRoots source)
        )
    sourceNabla = case appliedRootRole root of
      WriterRoot nabla _ -> nabla
      _ -> error "Resolve-held sort-definition root unexpectedly became a reader"
    identifier =
      publicationId
        sourceNabla
        (appliedRootAuthority root)
        remoteEpoch
        (nablaSequence 806)
    descriptor = primordialReplicaDescriptor sortDefinitionCarrier
    publication =
      checkedPure
        "construct checked Resolve-held equal definition"
        ( mkCheckedPublication
            descriptor
            identifier
            (admittedApplicationSortValue regularDefinition)
        )
    slot = carrierStoreSlot state
    destination =
      publicationDestination
        (Store.storeSlotDelta slot)
        (Store.storeSlotIncarnation slot)
        Normal
    batch =
      checkedPure
        "construct Resolve-qualified equal-definition batch"
        ( mkPublicationBatch
            identifier
            (appliedProcessEpochId source)
            (appliedRootSortId root)
            (appliedRootOccurrenceId root)
            (checkedPublicationCanonicalValue publication)
            Normal
            ( GraphProgress.structuralLastInstalledCutId
                (startupStructuralProgressState state)
            )
            prerequisite
            (destination :| [])
        )
    peerPublication =
      checkedPure
        "construct ordinary Resolve-held peer definition"
        ( mkOrdinaryPeerPublication
            descriptor
            (appliedRootOccurrenceId root)
            publication
            batch
        )
    direction =
      checkedPure
        "construct Resolve-held incoming direction"
        (mkStreamDirection remoteEpoch localHerald)
    item =
      sequencedItem
        direction
        firstStreamSequence
        (peerPublicationDigest peerPublication)
        peerPublication

assertResolveHeldDefinition ::
  String ->
  ControlIndex ->
  CheckedPublication ->
  StreamDirection ->
  HeraldState ->
  Assertion
assertResolveHeldDefinition context prerequisite publication direction state = do
  record <-
    maybe
      (assertFailure (context <> ": incoming definition was not retained"))
      pure
      ( Publication.lookupIncomingPublication
          (checkedPublicationId publication)
          (startupPublicationState state)
      )
  assertEqual
    (context <> ": publication remains dependency-held")
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition record)
  assertEqual
    (context <> ": publication retains only its exact control dependency")
    (Set.singleton (Publication.ControlIndexDependency prerequisite))
    (Publication.incomingPublicationDependencies record)
  retained <-
    pure
      ( checkedPure
          (context <> ": read incoming retained stream item")
          (PeerStream.incomingRetainedItems direction (startupPeerStreamState state))
      )
  assertEqual
    (context <> ": stream item is received but pending")
    [PeerStream.IncomingReceivedPending]
    (snd <$> retained)
  watermarks <-
    pure
      ( checkedPure
          (context <> ": read incoming stream watermarks")
          (PeerStream.incomingWatermarks direction (startupPeerStreamState state))
      )
  assertEqual
    (context <> ": received prefix covers the item")
    (Just firstStreamSequence)
    (streamPrefixSequence (streamReceivedPrefix watermarks))
  assertEqual
    (context <> ": completed prefix remains empty")
    Nothing
    (streamPrefixSequence (streamCompletedPrefix watermarks))
  assertEqual
    (context <> ": held definition has no Store observation")
    []
    (resolveHeldDefinitionEnvelopes publication state)

resolveHeldDefinitionEnvelopes ::
  CheckedPublication -> HeraldState -> [Store.StoreObservationEnvelope]
resolveHeldDefinitionEnvelopes publication state =
  [ envelope
  | slot <- Store.storeSlots (startupStoreState state),
    observation <- Store.storeSlotApplicationObservations slot,
    checkedPublicationId (Store.retainedStoreObservationPublication observation)
      == checkedPublicationId publication,
    Store.RoutedStoreObservation envelope <-
      [Store.retainedStoreObservationOrigin observation]
  ]

splitResolveEntry ::
  [CanonicalAppliedOracleEntry] ->
  ([CanonicalAppliedOracleEntry], CanonicalAppliedOracleEntry)
splitResolveEntry entries = case reverse entries of
  resolve : reversedPredecessors -> (reverse reversedPredecessors, resolve)
  [] -> error "Resolve fixture produced no canonical Oracle entries"

canonicalRejectedOracleEntriesThrough ::
  ControlIndex -> [CanonicalAppliedOracleEntry]
canonicalRejectedOracleEntriesThrough target =
  reverse entries
  where
    (_, entries) =
      foldl
        advance
        ( initialOracleState,
          []
        )
        [1 .. controlIndexWord64 target]
    initialOracleState =
      checkedPure
        "initialize Resolve-held fixture Oracle"
        (initialOracle fixtureCheckedOracleGenesis)
    bootstrap =
      required
        "Resolve-held fixture unstarted configured process"
        ( lookupConfiguredProcessBootstrap
            secondLocalBootstrap
            fixtureStep14CheckedGenesis
        )
    secondLocalBootstrap = case fixtureLocalBootstrapIds of
      _ : identifier : _ -> identifier
      _ -> error "Resolve-held fixture has fewer than two local bootstraps"
    advance (oracle, accumulated) ordinal =
      let envelope =
            oracleEnvelope
              (oracleClientRequestId localHerald ordinal)
              (Just (controlIndex 999))
              localHerald
              (startProcessEpochCommand (ProcessStart.processStart (ProcessBootstrap.configuredProcessBootstrapProcessId bootstrap) (ProcessBootstrap.configuredProcessBootstrapProcessEpochId bootstrap) (ProcessBootstrap.configuredProcessBootstrapResidence bootstrap)))
          (successor, applied) =
            case checkedPure
              "commit rejected Resolve-held fixture Oracle entry"
              (stepOracle envelope oracle) of
              (next, OracleCommitted _, effects) ->
                case oracleEffects effects of
                  [EmitAppliedOracleEntry entry] -> (next, entry)
                  observed ->
                    error
                      ( "Resolve-held fixture Oracle emitted unexpected effects: "
                          <> show observed
                      )
              (_, outcome, _) ->
                error
                  ( "Resolve-held fixture Oracle did not commit: "
                      <> show outcome
                  )
       in (successor, canonicalizeAppliedOracleEntry applied : accumulated)

caseOwnerBlockerIsAtomic :: Assertion
caseOwnerBlockerIsAtomic = do
  let fixture = acceptanceFixture
      blockedHerald =
        installApplicationStoreBlocker
          fixture.application
          fixture.herald
      registryBefore = startupSortRegistryState blockedHerald
      storeBefore = startupStoreState blockedHerald
      progressBefore = startupStructuralProgressState blockedHerald
      result =
        DisappearanceUseCase.coordinateProjectedRegularResolution
          fixture.terminal
          fixture.leaf
          blockedHerald
  case result of
    Left
      ( DisappearanceUseCase.DisappearanceRegularRetirementProblem
          (RegularRetirement.RegularRetirementEvidenceBlocked blockers)
        ) ->
        assertBool
          "the resolve-time recheck reports the representative visible Store blocker"
          ( any
              ( (== Protocol.VisibleApplicationCopyBlocker)
                  . Protocol.disappearanceBlockerClass
              )
              blockers
          )
    Left problem -> assertFailure ("unexpected owner-blocker problem: " <> show problem)
    Right _ -> assertFailure "resolve-time owner blocker was accepted"
  assertBool
    "rejection leaves the dynamic Registry occurrence effective"
    ( isJust
        ( SortRegistry.lookupEffectiveSort
            regularSortId
            registryBefore
        )
    )
  assertEqual
    "rejection installs no Store suppression"
    Nothing
    (Store.lookupRegularRetirementSuppression regularSubject storeBefore)
  assertBool
    "rejection cannot advance structural control"
    ( GraphProgress.structuralAppliedControlPrefix progressBefore
        < fixture.resolveIndex
    )

-- | Reachable live-cutover seed: a real admitted definition and its accepted
-- application LocalTake, under the same membership as the real Oracle fixture.
liveRegularDisappearanceFixture :: (HeraldState, DisappearanceSubject)
liveRegularDisappearanceFixture = (taken, regularSubject)
  where
    app = fixtureApplication (initialHeraldStateFor fixtureStep14CheckedGenesis)
    installed = installDefinition app 1 app.fixtureApplicationHerald
    taken =
      takeApplicationVisibleDefinition
        900
        app
        (Publication.applicationPublicationChecked installed.installedRecord)
        installed.installedHerald

-- | Multiple independent candidates created by admitted application writes and
-- one actual LocalTake. Descriptor retention differences give distinct SortIds.
liveRegularDisappearanceFixtureFor :: CheckedHeraldGenesis -> Int -> (HeraldState, [DisappearanceSubject])
liveRegularDisappearanceFixtureFor genesis count = (_checkedTake `seq` taken, subjects)
  where
    app =
      fixtureApplication
        ( fst
            ( checkedPure
                "initialize exact membership fixture"
                ( initialHerald
                    (monotonicInstant 0)
                    genesis
                    (fixtureCheckedInitialBootstrapsFor genesis)
                    fixtureOracleContacts
                    fixtureGeneratorSeed
                    fixtureApplicationRecoveryConfiguration
                    fixturePeerRecoveryConfiguration
                )
            )
        )
    definitions =
      [ case regularApplicationSortDefinition of
          DeclaredSortDefinition descriptor _ ->
            DeclaredSortDefinition (descriptor {SortSyntax.minimumRetentionMicros = fromIntegral ordinal}) Nothing
          other -> other
      | ordinal <- [1 .. count]
      ]
    subjects =
      [ regularSortDefinitionDisappearanceSubject
          (deriveRegularSortOccurrenceClaim (checkedSystemId genesis) (admittedApplicationSortDescriptor admitted) Genesis)
      | definition <- definitions,
        let admitted = checkedPure "admit live membership definition" (admitApplicationSortDefinition definition)
      ]
    installed = foldl write app.fixtureApplicationHerald (zip [1 ..] definitions)
    write state (ordinal, definition) =
      fst
        ( checkedPure
            "publish membership candidate definition"
            ( ApplicationCall.applyApplicationRequest
                ( CallApplicationRequest
                    app.fixtureApplicationBinding
                    app.fixtureApplicationSession
                    (requestId ordinal)
                    (WriteApplication app.fixtureApplicationPrivateWriter (PublishValue (ApplicationValue.SortDefinitionValue definition)))
                )
                state
            )
        )
    (taken, takeEffects) =
      checkedPure
        "take all membership candidate definitions"
        ( ApplicationCall.applyApplicationRequest
            ( CallApplicationRequest
                app.fixtureApplicationBinding
                app.fixtureApplicationSession
                (requestId 900)
                (LocalTakeApplication (ApplicationQuery (Set.singleton app.fixtureApplicationPrivateReader) QueryAlways))
            )
            installed
        )
    _checkedTake = case [values | SendApplicationReply _ (RetainedRequestReply _ (Completed _ (LocalTakeCompleted values))) <- effectBatchMembers takeEffects] of
      [values] | length [() | ApplicationValue.SortDefinitionValue DeclaredSortDefinition {} <- values] == count -> ()
      _ -> error ("membership LocalTake did not remove each admitted definition: " <> show (effectBatchMembers takeEffects))

-- The same deterministic private writer as the live seed, with a fresh request.
liveRegularDisappearancePeerRewrite :: HeraldState -> (HeraldInputBody, CheckedPublication, StreamDirection, StreamSequence)
liveRegularDisappearancePeerRewrite state = (input, publication, direction, sequenceNumber)
  where
    (publication, _, direction, prototype) = resolveHeldDefinitionItem (controlIndex 0) state
    peer = sequencedItemPayload prototype
    watermarks = checkedPure "live post-marker peer prefix" (PeerStream.incomingWatermarks direction (startupPeerStreamState state))
    sequenceNumber = maybe firstStreamSequence nextStreamSequence (streamPrefixSequence (streamReceivedPrefix watermarks))
    item = sequencedItem direction sequenceNumber (peerPublicationDigest peer) peer
    binding = required "live post-marker peer binding" (DiscoveryState.currentPeerBinding (streamDirectionSource direction) (startupDiscoveryState state))
    input = PeerInput (peerPublicationReceived binding (peerPublicationItem item))

liveRegularDisappearanceRewriteInput :: HeraldInputBody
liveRegularDisappearanceRewriteInput =
  ApplicationRequestInput
    ( CallApplicationRequest
        app.fixtureApplicationBinding
        app.fixtureApplicationSession
        (requestId 901)
        (WriteApplication app.fixtureApplicationPrivateWriter (PublishValue (ApplicationValue.SortDefinitionValue regularApplicationSortDefinition)))
    )
  where
    app = fixtureApplication (initialHeraldStateFor fixtureStep14CheckedGenesis)

-- A real checked destination subscription with either an unapplied snapshot
-- or a complete empty base followed by one definition change.
liveRegularAlignmentGateFixture ::
  Bool ->
  (HeraldState, PeerBinding, HeraldEpoch, AlignmentProtocol.AlignmentSubscriptionId, [AlignmentProtocol.AlignmentControl])
liveRegularAlignmentGateFixture snapshotMode = regularAlignmentGateFixture snapshotMode False

liveRegularMalformedAlignmentGateFixture ::
  (HeraldState, PeerBinding, HeraldEpoch, AlignmentProtocol.AlignmentSubscriptionId, [AlignmentProtocol.AlignmentControl])
liveRegularMalformedAlignmentGateFixture = regularAlignmentGateFixture True True

regularAlignmentGateFixture ::
  Bool ->
  Bool ->
  (HeraldState, PeerBinding, HeraldEpoch, AlignmentProtocol.AlignmentSubscriptionId, [AlignmentProtocol.AlignmentControl])
regularAlignmentGateFixture snapshotMode malformed = (ready, binding, remote, subscription, controls)
  where
    (seed, _) = liveRegularDisappearanceFixture
    app = fixtureApplication (initialHeraldStateFor fixtureStep14CheckedGenesis)
    (prototype, _, _) = applyAlignmentTranscript [] Nothing (carrierStoreSlot seed) seed
    (subscription, destination) = case AlignmentTransferState.destinationSubscriptionEntries (Alignment.alignmentTransferState (startupAlignmentState prototype)) of
      [entry] -> entry
      _ -> error "expected one alignment gate destination"
    attempt = required "alignment gate attempt" (AlignmentTransferState.destinationSubscriptionAttempt destination)
    remote = AlignmentProtocol.alignmentAttemptSourceHerald attempt
    remoteId = checkedPure "alignment gate remote HeraldId" (mkHeraldId (fixtureIdentifierBytes 231))
    binding = PeerBinding remoteId remote firstPeerBindingGeneration (PeerCandidate remoteId remote (ConnectionNonce 1))
    attempted = fst (AlignmentTransferState.commitDestinationAttempt (checkedPure "alignment gate destination" (AlignmentTransferState.prepareDestinationAttempt attempt AlignmentTransferState.emptyState)))
    withAttempt = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState attempted (startupAlignmentState seed)) seed
    subscribe = AlignmentTransferState.destinationSubscriptionSubscribe destination
    revision = DomainAlignment.nextStoreRevision DomainAlignment.initialStoreRevision
    publication = definitionPublicationAt app.fixtureApplicationWriter 910
    retained =
      checkedPure
        "alignment gate definition evidence"
        ( AlignmentProtocol.retainedPublicationEvidence
            (checkedPublicationId publication)
            app.fixtureApplicationProcess
            (checkedPublicationSort publication)
            (Store.storeSlotOccurrenceId (carrierStoreSlot seed))
            (if malformed then canonicalValueBytes (boolValue True) else checkedPublicationCanonicalValue publication)
            Normal
            (GraphProgress.structuralLastInstalledCutId (startupStructuralProgressState seed))
            (controlIndex 0)
            Nothing
        )
    fact = checkedPure "alignment gate snapshot state" (AlignmentProtocol.retainedStateEvidence retained retained Normal)
    preparedBase =
      checkedPure
        "alignment gate source snapshot"
        ( AlignmentTransferState.prepareSourceSubscription
            subscribe
            (if snapshotMode then revision else DomainAlignment.initialStoreRevision)
            (if snapshotMode then [fact] else [])
            AlignmentTransferState.SourceLiveImmediate
            AlignmentTransferState.emptyState
        )
    (sourceBase, _) = AlignmentTransferState.commitSourceSubscription preparedBase
    baseControls = AlignmentTransferState.preparedSourceSubscriptionControls preparedBase
    preparedChanges =
      checkedPure
        "alignment gate source change"
        ( AlignmentTransferState.prepareSourceChanges
            subscription
            [AlignmentProtocol.alignmentChange subscription revision retained]
            (AlignmentProtocol.alignmentLive subscription revision)
            sourceBase
        )
    controls = if snapshotMode then baseControls else AlignmentTransferState.preparedSourceChangeControls preparedChanges
    ready = if snapshotMode then withAttempt else foldl apply withAttempt baseControls
    apply current control = fst (checkedPure "alignment gate empty base application" (AlignmentTransfer.applyAlignmentTransferControl binding control current))

data AcceptanceFixture = AcceptanceFixture
  { herald :: HeraldState,
    application :: FixtureApplication,
    leaf :: Disappearance.State,
    terminal :: Protocol.ProjectedDisappearanceTerminal,
    resolveIndex :: ControlIndex,
    successorOccurrence :: SortDefinitionOccurrenceId,
    firstDefinition :: CheckedPublication,
    firstRecord :: Publication.ApplicationPublicationRecord
  }

acceptanceFixture :: AcceptanceFixture
acceptanceFixture = acceptanceFixtureFrom initialHeraldState

acceptanceFixtureFrom :: HeraldState -> AcceptanceFixture
acceptanceFixtureFrom initial =
  AcceptanceFixture
    { herald = withDefinition,
      application,
      leaf = completeLeaf,
      terminal,
      resolveIndex,
      successorOccurrence,
      firstDefinition = Publication.applicationPublicationChecked firstRecord,
      firstRecord
    }
  where
    application = fixtureApplication initial
    firstInstallation =
      installDefinition application 1 application.fixtureApplicationHerald
    firstRecord = firstInstallation.installedRecord
    beforeApplicationTake = firstInstallation.installedHerald
    beforeTakeEvidence =
      checkedPure
        "capture disappearance evidence before the application takes its definition"
        ( Evidence.disappearanceEvidenceSnapshotForHerald
            regularSubject
            beforeApplicationTake
        )
    afterApplicationTake =
      takeApplicationVisibleDefinition
        900
        application
        (Publication.applicationPublicationChecked firstRecord)
        beforeApplicationTake
    afterTakeEvidence =
      checkedPure
        "capture actual eight-owner disappearance evidence"
        ( Evidence.disappearanceEvidenceSnapshotForHerald
            regularSubject
            afterApplicationTake
        )
    applicationTakeChecked =
      case ( fmap
               Protocol.disappearanceBlockerClass
               (Evidence.disappearanceEvidenceSnapshotBlockers beforeTakeEvidence),
             Evidence.disappearanceEvidenceSnapshotBlockers afterTakeEvidence
           ) of
        ([Protocol.VisibleApplicationCopyBlocker], []) -> ()
        observed ->
          error
            ( "taking the application-visible definition did not remove precisely its blocker: "
                <> show observed
            )
    withDefinition = applicationTakeChecked `seq` afterApplicationTake
    evidenceSnapshot = applicationTakeChecked `seq` afterTakeEvidence
    completeLeaf = reportedLeaf evidenceSnapshot
    resolveIndex = controlIndex 7
    outcome =
      checkedPure
        "resolve regular disappearance subject"
        ( resolveDisappearanceSubject
            (checkedSystemId fixtureCheckedGenesis)
            resolveIndex
            regularSubject
        )
    terminal =
      Protocol.ProjectedDisappearanceResolved fixtureProbe outcome resolveIndex
    preparedAuthority =
      checkedPure
        "read successor occurrence from checked leaf authority"
        (Disappearance.prepareProjectedRegularResolution terminal completeLeaf)
    successorOccurrence =
      Disappearance.regularRetirementAuthoritySuccessorOccurrence
        ( required
            "fixture regular-retirement authority"
            (Disappearance.preparedProjectedRegularResolutionAuthority preparedAuthority)
        )

data DefinitionInstallation = DefinitionInstallation
  { installedHerald :: HeraldState,
    installedPlan :: SortRegistry.SortInductionPlan,
    installedRecord :: Publication.ApplicationPublicationRecord,
    installedEnvelope :: Store.StoreObservationEnvelope
  }

data FixtureApplication = FixtureApplication
  { fixtureApplicationHerald :: HeraldState,
    fixtureApplicationProcess :: ProcessEpochId,
    fixtureApplicationWriter :: NablaId,
    fixtureApplicationPrivateWriter :: PrivateNablaId,
    fixtureApplicationPrivateReader :: PrivateDeltaId,
    fixtureApplicationSession :: Session.ApplicationSessionId,
    fixtureApplicationBinding :: Session.ApplicationSessionBinding
  }

fixtureApplication :: HeraldState -> FixtureApplication
fixtureApplication predecessor =
  FixtureApplication
    { fixtureApplicationHerald =
        replaceStartupApplicationState openedApplication predecessor,
      fixtureApplicationProcess = process,
      fixtureApplicationWriter = writer,
      fixtureApplicationPrivateWriter = privateWriter,
      fixtureApplicationPrivateReader = privateReader,
      fixtureApplicationSession = session,
      fixtureApplicationBinding = Session.sessionAcceptanceBinding acceptance
    }
  where
    application = startupApplicationState predecessor
    (attachment, process, privateWriter, privateReader) =
      case [ (candidateAttachment, candidateProcess, candidateWriter, candidateReader)
           | (candidateAttachment, candidateProcess) <-
               Application.applicationWitnessAttachments
                 (Application.applicationStateWitness application),
             access <-
               maybe
                 []
                 pure
                 (Application.applicationBootstrapAccess candidateProcess application),
             Application.WriterAccess role candidateWriter _ <-
               Application.bootstrapAccessRoots access,
             role == SortDefinitionRole,
             Application.ReaderAccess readerRole candidateReader <-
               Application.bootstrapAccessRoots access,
             readerRole == SortDefinitionRole
           ] of
        [candidate] -> candidate
        candidates ->
          error
            ( "expected one resident sort-definition writer, got "
                <> show (length candidates)
            )
    writer =
      case [ candidate
           | fact <- Controlled.controlledRootFacts (startupControlledState predecessor),
             Controlled.rootFactProcessEpoch fact == process,
             Controlled.rootFactCatalogueRole fact == SortDefinitionRole,
             Controlled.ControlledWriter candidate _ <- [Controlled.rootFactRole fact]
           ] of
        [candidate] -> candidate
        candidates ->
          error
            ( "expected one controlled sort-definition writer, got "
                <> show (length candidates)
            )
    (openedApplication, acceptance) =
      Application.commitApplicationSessionAcceptance
        ( checkedPure
            "open fixture application session"
            ( Application.prepareApplicationSessionOpen
                localHerald
                attachment
                (Session.clientNonce 1)
                application
            )
        )
    session = case Session.sessionAcceptanceReply acceptance of
      Session.SessionOpened opened _ _ -> opened
      reply -> error ("expected fixture application session, got " <> show reply)

installDefinition ::
  FixtureApplication -> Word64 -> HeraldState -> DefinitionInstallation
installDefinition = installDefinitionAtControl Nothing

installDefinitionAtControl ::
  Maybe ControlIndex ->
  FixtureApplication ->
  Word64 ->
  HeraldState ->
  DefinitionInstallation
installDefinitionAtControl retainedControlOverride fixture ordinal predecessor =
  DefinitionInstallation
    { installedHerald = successor,
      installedPlan = plan,
      installedRecord = record,
      installedEnvelope = envelope
    }
  where
    registry0 = startupSortRegistryState predecessor
    store0 = startupStoreState predecessor
    publication0 = startupPublicationState predecessor
    application0 = startupApplicationState predecessor
    progress = startupStructuralProgressState predecessor
    route =
      checkedPure
        "freeze checked sort-definition writer route"
        ( PublicationRoute.freezeWriterRoute
            fixture.fixtureApplicationWriter
            (primordialReplicaSortId sortDefinitionCarrier)
            (startupGraphState predecessor)
            (startupPlacementState predecessor)
        )
    plan =
      checkedPure
        "plan regular sort induction"
        ( SortRegistry.planSortInduction
            (checkedSystemId fixtureCheckedGenesis)
            regularDescriptor
            registry0
        )
    prerequisite = SortRegistry.sortInductionPlanControlPrerequisite plan
    retainedControl = case retainedControlOverride of
      Nothing -> prerequisite
      Just override -> override
    request =
      case checkedPure
        "classify fixture sort-definition request"
        ( Application.classifyApplicationPublicationRequest
            fixture.fixtureApplicationSession
            fixture.fixtureApplicationBinding
            (requestId ordinal)
            ( Application.applicationPublicationCall
                fixture.fixtureApplicationPrivateWriter
                (ApplicationValue.SortDefinitionValue regularApplicationSortDefinition)
            )
            application0
        ) of
        Application.FirstApplicationPublicationRequest candidate -> candidate
        _ -> error "expected a first fixture sort-definition request"
    application =
      Application.commitApplicationPublicationRequest
        (Application.prepareApplicationPublicationRequestAcceptance request)
    preparedPublication =
      checkedPure
        "prepare local regular sort-definition publication"
        ( Publication.prepareApplicationPublication
            fixture.fixtureApplicationWriter
            Nothing
            genesisAuthorityEpoch
            (primordialReplicaDescriptor sortDefinitionCarrier)
            ( Publication.admittedSortDefinitionApplicationPublicationValue
                regularDefinition
            )
            RegularApplicationPublicationOrigin
            ( OracleProjection.oracleViewCurrentHeraldMembershipId
                (OracleProjection.oracleView (startupOracleProjectionState predecessor))
            )
            fixture.fixtureApplicationProcess
            (Application.applicationPublicationRequestPosition request)
            carrierOccurrence
            Normal
            (GraphProgress.structuralLastInstalledCutId progress)
            retainedControl
            route
            publication0
        )
    (publication, _, record) =
      Publication.commitApplicationPublication preparedPublication
    checked = Publication.applicationPublicationChecked record
    envelope =
      Store.storeObservationEnvelope
        fixture.fixtureApplicationProcess
        carrierOccurrence
        (GraphProgress.structuralLastInstalledCutId progress)
        retainedControl
        Nothing
    store =
      Store.commitStoreApplication
        ( checkedPure
            "apply regular definition to private Store"
            (Store.prepareStoreApplication envelope route checked store0)
        )
    registry =
      SortRegistry.commitSortInduction
        ( checkedPure
            "induce regular definition into effective Registry"
            ( SortRegistry.prepareSortInduction
                regularDescriptor
                (SortRegistry.sortInductionPlanOccurrenceId plan)
                checked
                registry0
            )
        )
    successor =
      replaceStartupApplicationState application
        . replaceStartupPublicationState publication
        . replaceStartupStoreState store
        . replaceStartupSortRegistryState registry
        $ predecessor

reportedLeaf :: Evidence.DisappearanceEvidenceSnapshot -> Disappearance.State
reportedLeaf =
  reportedLeafFor fixtureHeraldMembershipGeneration activeHeralds

reportedLeafFor ::
  HeraldMembershipGeneration ->
  [HeraldEpoch] ->
  Evidence.DisappearanceEvidenceSnapshot ->
  Disappearance.State
reportedLeafFor membership heralds snapshot =
  foldl installClaim locallyReported remoteClaims
  where
    coordinate =
      disappearanceSubjectMembershipCoordinate regularSubject membership
    projected =
      checkedPure
        "project regular disappearance probe"
        ( Protocol.projectedDisappearanceProbe
            fixtureProbe
            regularSubject
            coordinate
            membership
        )
    (opened, _) =
      commitLeaf
        "install regular disappearance probe"
        (Disappearance.prepareProjectedOpen projected snapshot (Disappearance.initialState localHerald))
    outgoingMarkers =
      [ assignedMarker coordinate localHerald destination
      | destination <- heralds,
        destination /= localHerald
      ]
    (assigned, _) =
      commitLeaf
        "install outgoing disappearance markers"
        (Disappearance.preparePublicationMarkerAssignments fixtureProbe outgoingMarkers opened)
    incomingMarkers =
      [ assignedMarker coordinate source localHerald
      | source <- heralds,
        source /= localHerald
      ]
    cutComplete = foldl completeIncoming assigned incomingMarkers
    completeIncoming state marker =
      let (received, _) =
            commitLeaf
              "receive disappearance marker"
              (Disappearance.preparePublicationMarkerReceipt marker state)
       in fst
            ( commitLeaf
                "complete disappearance marker"
                (Disappearance.preparePublicationMarkerCompletion marker received)
            )
    (withLocalCut, _) =
      commitLeaf
        "complete local disappearance publication cut"
        ( Disappearance.prepareLocalPublicationCutCompletion
            fixtureProbe
            (Evidence.disappearanceEvidenceSnapshotLocalPublicationCut snapshot)
            cutComplete
        )
    withAlignmentCuts =
      foldl completeIncomingAlignment withLocalCut incomingAlignmentCuts
    incomingAlignmentCuts =
      Evidence.disappearanceEvidenceSnapshotIncomingAlignmentCuts snapshot
    completeIncomingAlignment state cut =
      let source = Protocol.incomingAlignmentCutSource cut
          marker =
            Protocol.disappearanceAlignmentMarker
              fixtureProbe
              ( Protocol.outgoingAlignmentCut
                  (Protocol.incomingAlignmentCutSubscription cut)
                  DomainAlignment.initialStoreRevision
              )
          (received, _) =
            commitLeaf
              "receive disappearance alignment marker"
              (Disappearance.prepareAlignmentMarkerReceipt source marker state)
       in fst
            ( commitLeaf
                "complete disappearance alignment marker"
                ( Disappearance.prepareAlignmentMarkerCompletion
                    marker
                    DomainAlignment.initialStoreRevision
                    received
                )
            )
    (withReport, reportDisposition) =
      commitLeaf
        "prepare owner-derived local absence report"
        ( Disappearance.prepareLocalAbsenceReportFromEvidence
            fixtureProbe
            snapshot
            withAlignmentCuts
        )
    localReport = case reportDisposition of
      Disappearance.LocalReportPrepared report -> report
      other -> error ("expected fresh local Report, got " <> show other)
    localClaim = Protocol.localAbsenceReportClaim localReport
    (locallyReported, _) =
      commitLeaf
        "project local absence claim"
        (Disappearance.prepareProjectedReport localClaim withReport)
    remoteClaims =
      [ evidenceClaim membership reporter byte
      | (reporter, byte) <- zip (filter (/= localHerald) heralds) [201 ..]
      ]
    installClaim state claim =
      fst
        ( commitLeaf
            "project remote absence claim"
            (Disappearance.prepareProjectedReport claim state)
        )

assignedMarker ::
  DisappearanceSubjectMembershipCoordinate ->
  HeraldEpoch ->
  HeraldEpoch ->
  Protocol.DisappearancePublicationMarker
assignedMarker coordinate source destination =
  case Map.lookup destination assignments of
    Just (item :| []) -> sequencedItemPayload item
    other -> error ("expected one assigned marker, got " <> show (fmap length other))
  where
    prepared =
      checkedPure
        "sequence disappearance marker"
        ( PeerStream.prepareSequenceBoundEnqueue
            ( Map.singleton
                destination
                ( Protocol.disappearancePublicationMarkerItem
                    fixtureProbe
                    coordinate
                    :| []
                )
            )
            (PeerStream.initialState source)
        )
    (_, assignments) = PeerStream.commitEnqueue prepared

evidenceClaim ::
  HeraldMembershipGeneration ->
  HeraldEpoch ->
  Word8 ->
  DisappearanceEvidenceClaim
evidenceClaim membership reporter byte =
  checkedPure
    "admit remote disappearance evidence claim"
    ( admitDisappearanceEvidenceClaim
        regularSubject
        membership
        fixtureProbe
        reporter
        (deriveDisappearanceEvidenceDigest (fixtureIdentifierBytes byte))
    )

installApplicationStoreBlocker :: FixtureApplication -> HeraldState -> HeraldState
installApplicationStoreBlocker fixture predecessor =
  replaceStartupStoreState withPublication predecessor
  where
    process = fixture.fixtureApplicationProcess
    delta = checkedPure "blocker Delta" (mkDeltaId (fixtureIdentifierBytes 231))
    incarnation =
      checkedPure
        "blocker Store incarnation"
        (mkStoreIncarnationId (fixtureIdentifierBytes 232))
    bootstrapped =
      Store.commitStoreBootstrap
        ( checkedPure
            "install application-visible blocker Store"
            ( Store.prepareStoreBootstrap
                [ Store.localStoreSpec
                    (Store.ApplicationReader process SortDefinitionRole)
                    delta
                    regularSortId
                    genesisRegularOccurrence
                    incarnation
                ]
                (startupStoreState predecessor)
            )
        )
    blockerValue =
      checkedPure
        "regular blocker value"
        (recordValue [(regularKeyField, boolValue True)])
    blockerPublication =
      checkedPure
        "checked regular blocker publication"
        ( mkCheckedPublication
            regularDescriptor
            ( publicationId
                fixture.fixtureApplicationWriter
                genesisAuthorityEpoch
                localHerald
                (nablaSequence 600)
            )
            blockerValue
        )
    (withPublication, _) =
      Store.commitPeerStoreApplication
        ( checkedPure
            "apply application-visible blocker"
            ( Store.preparePeerStoreApplication
                ( Store.storeObservationEnvelope
                    process
                    genesisRegularOccurrence
                    ( GraphProgress.structuralLastInstalledCutId
                        (startupStructuralProgressState predecessor)
                    )
                    (controlIndex 0)
                    Nothing
                )
                (Store.peerStoreDestination delta incarnation Normal :| [])
                blockerPublication
                bootstrapped
            )
        )

-- A definition write is routed to the publishing application's regular
-- sort-definition reader as well as the private Herald system view.  Only the
-- hidden system-view copy is purgeable at Resolve, so exercise the real local
-- application take before it can truthfully report absence.
takeApplicationVisibleDefinition ::
  Word64 ->
  FixtureApplication ->
  CheckedPublication ->
  HeraldState ->
  HeraldState
takeApplicationVisibleDefinition ordinal fixture publication predecessor =
  case ApplicationCall.applyApplicationRequest ingress predecessor of
    Left problem ->
      error
        ( "application-visible definition LocalTake failed: "
            <> show problem
        )
    Right (successor, effects) ->
      case effectBatchMembers effects of
        [ SendApplicationReply
            observedBinding
            ( RetainedRequestReply
                _
                (Completed observedRequest (LocalTakeCompleted values))
              )
          ]
            | observedBinding == fixture.fixtureApplicationBinding,
              observedRequest == requestId ordinal,
              expectedValue `elem` values,
              not (applicationCopyVisible successor) ->
                successor
        observed ->
          error
            ( "application-visible definition LocalTake did not complete exactly: "
                <> show observed
            )
  where
    ingress =
      CallApplicationRequest
        fixture.fixtureApplicationBinding
        fixture.fixtureApplicationSession
        (requestId ordinal)
        ( LocalTakeApplication
            ( ApplicationQuery
                (Set.singleton fixture.fixtureApplicationPrivateReader)
                QueryAlways
            )
        )
    expectedValue =
      ApplicationValue.SortDefinitionValue
        ( case regularApplicationSortDefinition of
            DeclaredSortDefinition descriptor _ ->
              DeclaredSortDefinition descriptor (Just regularSortId)
            SortSyntax.PredefinedSortDefinition {} ->
              error "regular fixture unexpectedly uses a predefined definition"
        )
    applicationCopyVisible state =
      any
        (definitionVisible publication)
        [ slot
        | slot <- Store.storeSlots (startupStoreState state),
          Store.storeSlotProvenance slot
            == Store.ApplicationReader
              fixture.fixtureApplicationProcess
              SortDefinitionRole
        ]

definitionPublicationAt :: NablaId -> Word64 -> CheckedPublication
definitionPublicationAt writer sequenceNumber =
  checkedPure
    "checked stale sort-definition publication"
    ( mkCheckedPublication
        (primordialReplicaDescriptor sortDefinitionCarrier)
        ( publicationId
            writer
            genesisAuthorityEpoch
            localHerald
            (nablaSequence sequenceNumber)
        )
        (admittedApplicationSortValue regularDefinition)
    )

definitionVisible :: CheckedPublication -> Store.StoreSlot -> Bool
definitionVisible publication slot =
  isJust
    (lookupVisible (checkedPublicationKey publication) (Store.storeSlotContents slot))

storeDestination :: Store.StoreSlot -> Store.PeerStoreDestination
storeDestination slot =
  Store.peerStoreDestination
    (Store.storeSlotDelta slot)
    (Store.storeSlotIncarnation slot)
    Normal

carrierStoreSlot :: HeraldState -> Store.StoreSlot
carrierStoreSlot = carrierStoreSlotFor SortDefinitionRole

carrierStoreSlotFor :: PredefinedSortRole -> HeraldState -> Store.StoreSlot
carrierStoreSlotFor role state =
  required
    ("private " <> show role <> " Store")
    ( find
        ( (== Store.HeraldSystemView localHerald role)
            . Store.storeSlotProvenance
        )
        (Store.storeSlots (startupStoreState state))
    )

predefinedCarrier :: PredefinedSortRole -> PrimordialDefinitionReplica
predefinedCarrier role =
  required
    ("primordial " <> show role <> " carrier")
    (find ((== role) . primordialReplicaRole) (checkedPrimordialReplicas fixtureCheckedGenesis))

removedEffectiveStoreFacts :: HeraldState -> HeraldState -> Word64
removedEffectiveStoreFacts predecessor successor =
  sum
    [ removedStoreSlotFacts before after
    | before <- Store.retainedStoreSlots (startupStoreState predecessor),
      let after =
            required
              "retirement successor retained Store incarnation"
              ( Store.lookupRetainedStoreSlot
                  (Store.storeSlotIncarnation before)
                  (startupStoreState successor)
              )
    ]

removedStoreSlotFacts :: Store.StoreSlot -> Store.StoreSlot -> Word64
removedStoreSlotFacts before after =
  removedBy fst (visibleInstances beforeContents) (visibleInstances afterContents)
    + removedBy fst (retainedInstances beforeContents) (retainedInstances afterContents)
    + removedBy
      (checkedPublicationLogicalState . fst)
      (retainedSnapshotLogicalStrengthFacts (retainedSnapshot beforeContents))
      (retainedSnapshotLogicalStrengthFacts (retainedSnapshot afterContents))
  where
    beforeContents = Store.storeSlotContents before
    afterContents = Store.storeSlotContents after

removedBy :: (Ord key, Eq value) => (value -> key) -> [value] -> [value] -> Word64
removedBy keyOf before after =
  fromIntegral
    ( length
        [ ()
        | fact <- before,
          Map.lookup (keyOf fact) afterByKey /= Just fact
        ]
    )
  where
    afterByKey = Map.fromList [(keyOf fact, fact) | fact <- after]

initialHeraldState :: HeraldState
initialHeraldState = initialHeraldStateFor fixtureCheckedGenesis

initialHeraldStateFor :: CheckedHeraldGenesis -> HeraldState
initialHeraldStateFor genesis =
  fst
    ( checkedPure
        "initialize regular-retirement Herald"
        ( initialHerald
            (monotonicInstant 0)
            genesis
            (fixtureCheckedInitialBootstrapsFor genesis)
            fixtureOracleContacts
            fixtureGeneratorSeed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    )

advanceFixtureOracleMembership ::
  HeraldState -> (HeraldState, HeraldMembershipGeneration)
advanceFixtureOracleMembership predecessor =
  ( replaceStartupOracleProjectionState
      (OracleProjection.commitMembershipAdvance prepared)
      predecessor,
    successorMembership
  )
  where
    projection = startupOracleProjectionState predecessor
    view = OracleProjection.oracleView projection
    retirementIndex =
      controlIndex
        (controlIndexWord64 (OracleProjection.oracleViewControlIndex view) + 1)
    retiredHerald =
      required
        "remote Herald for coherent membership advance"
        (find (/= localHerald) (checkedActiveHeraldEpochs fixtureCheckedGenesis))
    predecessorMembership =
      OracleProjection.oracleViewCurrentHeraldMembership view
    successorMembership =
      checkedPure
        "derive coherent successor membership"
        ( retireHeraldMembershipGeneration
            retirementIndex
            (fixtureRetirementResolution retirementIndex)
            retiredHerald
            predecessorMembership
        )
    residentProcessEnds =
      [ OracleProjection.membershipAdvanceProcessEnd
          process
          retirementIndex
          fixtureHeraldRetirementReason
      | process <- OracleProjection.projectedProcessEpochs projection,
        OracleProjection.oracleViewProcessIsLive process view,
        OracleProjection.oracleViewProcessResidence process view == Just retiredHerald
      ]
    prepared =
      checkedPure
        "prepare coherent Oracle membership advance"
        ( OracleProjection.prepareMembershipAdvance
            retirementIndex
            successorMembership
            residentProcessEnds
            projection
        )

advanceOracleProjectionTo ::
  ControlIndex ->
  OracleProjection.State ->
  OracleProjection.State
advanceOracleProjectionTo target initialProjection =
  snd
    ( foldl
        advance
        (initialOracleState, initialProjection)
        [1 .. controlIndexWord64 target]
    )
  where
    initialOracleState =
      checkedPure
        "initialize alignment fixture Oracle"
        (initialOracle fixtureCheckedOracleGenesis)
    bootstrap =
      required
        "alignment fixture unstarted configured process"
        ( lookupConfiguredProcessBootstrap
            secondLocalBootstrap
            fixtureStep14CheckedGenesis
        )
    secondLocalBootstrap = case fixtureLocalBootstrapIds of
      _ : identifier : _ -> identifier
      _ -> error "alignment fixture has fewer than two local bootstraps"
    advance (oracle, projection) ordinal =
      let envelope =
            oracleEnvelope
              (oracleClientRequestId localHerald ordinal)
              (Just (controlIndex 999))
              localHerald
              (startProcessEpochCommand (ProcessStart.processStart (ProcessBootstrap.configuredProcessBootstrapProcessId bootstrap) (ProcessBootstrap.configuredProcessBootstrapProcessEpochId bootstrap) (ProcessBootstrap.configuredProcessBootstrapResidence bootstrap)))
          (successorOracle, applied) =
            case checkedPure
              "commit rejected alignment fixture Oracle entry"
              (stepOracle envelope oracle) of
              (successor, OracleCommitted _, effects) ->
                case oracleEffects effects of
                  [EmitAppliedOracleEntry entry] -> (successor, entry)
                  observed ->
                    error
                      ( "alignment fixture Oracle emitted unexpected effects: "
                          <> show observed
                      )
              (_, outcome, _) ->
                error
                  ( "alignment fixture Oracle did not commit: "
                      <> show outcome
                  )
          prepared =
            checkedPure
              "project rejected alignment fixture Oracle entry"
              ( OracleProjection.prepareAppliedEntry
                  (canonicalizeAppliedOracleEntry applied)
                  projection
              )
       in (successorOracle, OracleProjection.commitAppliedEntry prepared)

regularDefinition :: AdmittedApplicationSortDefinition
regularDefinition =
  checkedPure
    "admit regular application sort definition"
    (admitApplicationSortDefinition regularApplicationSortDefinition)

regularApplicationSortDefinition :: ApplicationSortDefinition
regularApplicationSortDefinition =
  DeclaredSortDefinition
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
        SortSyntax.minimumRetentionMicros = 0,
        SortSyntax.isImmutable = False,
        SortSyntax.labelField = Nothing
      }
    Nothing

regularDescriptor :: CanonicalDescriptor
regularDescriptor = admittedApplicationSortDescriptor regularDefinition

regularSortId :: SortId
regularSortId = descriptorSortId regularDescriptor

regularSubject :: DisappearanceSubject
regularSubject =
  regularSortDefinitionDisappearanceSubject
    ( deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureCheckedGenesis)
        regularDescriptor
        Genesis
    )

genesisRegularOccurrence :: SortDefinitionOccurrenceId
genesisRegularOccurrence =
  deriveSortDefinitionOccurrenceId
    (checkedSystemId fixtureCheckedGenesis)
    regularSortId
    Genesis

sortDefinitionCarrier :: PrimordialDefinitionReplica
sortDefinitionCarrier = predefinedCarrier SortDefinitionRole

carrierOccurrence :: SortDefinitionOccurrenceId
carrierOccurrence = primordialReplicaOccurrenceId sortDefinitionCarrier

localHerald :: HeraldEpoch
localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis

activeHeralds :: [HeraldEpoch]
activeHeralds = checkedActiveHeraldEpochs fixtureCheckedGenesis

fixtureProbe :: DisappearanceProbeId
fixtureProbe =
  checkedPure
    "regular-retirement probe"
    (deriveDisappearanceProbeId (controlIndex 1))

regularKeyField :: FieldName
regularKeyField = checkedPure "regular key field" (mkFieldName "key")

commitLeaf ::
  String ->
  Either
    Disappearance.DisappearanceProblem
    (Disappearance.PreparedDisappearanceTransition output) ->
  (Disappearance.State, output)
commitLeaf context =
  Disappearance.commitDisappearanceTransition . checkedPure context

required :: String -> Maybe value -> value
required context = maybe (error (context <> ": missing value")) id

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure context = either (error . ((context <> ": ") <>) . show) id
