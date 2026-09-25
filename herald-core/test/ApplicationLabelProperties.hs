{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ApplicationLabelProperties
  ( tests,
    regularRetirementHeldOrdinaryFixture,
    regularRetirementLocallyTakenOrdinaryFixture,
    heldFixtureStateForOwnerProperties,
    heldStructuralFixtureStateForEvidenceProperties,
    membershipHeldLabelClosureFixture,
    predecessorAdmittedApplicationPublicationFixture,
    settledStructuralControlledFixture,
    locallyTakenNeutralDisappearanceFixture,
    locallyTakenNeutralDisappearanceFixtureWithRequests,
    regularDeltaControlledRemovalRecoveryFixture,
    settledNeutralControlledFixture,
    settledNeutralControlledFixtureWithApplicationRequests,
    settledNeutralControlledFixtureWithForwardRequest,
    settledDeltaControlledFixtureWithPendingWait,
    settledNablaControlledFixtureWithPristineReservations,
    settleStructuralPublication,
    completeAssignedPeerPublications,
    commitNextLabelRequest,
    commitCompleteLabelWorkflow,
  )
where

import GenesisFixtures (fixtureHeraldAfterProbePrefix, fixtureRetirementResolution)

import Control.Monad (foldM, forM, forM_, void)
import Data.Bits (shiftR)
import Data.ByteString qualified as ByteString
import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole,
    ApplicationStartupAccess,
    predefinedAccessRole,
    predefinedReader,
    predefinedWriter,
    startupAccessProcess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    PrivateNablaId,
    PrivateObjectId,
    PrivateProcessId,
    PrivateUniqueId,
    SortId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    privateDeltaUniqueId,
    privateNablaUniqueId,
    privateObjectUniqueId,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToDelete, LabelToProcess, LabelToVoid),
    LabelResult (LabelApplied, LabelNotApplied),
  )
import Eclips.Application.Types.NewId
  ( NewIdTarget (BareNewId, ControlledNewId),
  )
import Eclips.Application.Types.Operation
  ( ApplicationOperation
      ( ForwardApplication,
        LabelApplication,
        LocalTakeApplication,
        NewIdApplication,
        ReadApplication,
        WaitApplication,
        WriteApplication
      ),
  )
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Rejection
  ( ApplicationRejection
      ( ApplicationLabelTransitionNotPermitted,
        ApplicationObjectNotLabelable,
        ApplicationOperateNotPermitted
      ),
  )
import Eclips.Application.Types.Result
  ( OperationPendingReason (LabelSettlementPending, StructuralStabilizationPending),
    RegularCallResult
      ( ForwardCompleted,
        LabelCompleted,
        LocalTakeCompleted,
        NewIdCompleted,
        ReadCompleted,
        WriteCompleted
      ),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (ControlledSort, RegularSort),
    ApplicationValueSchema (LabelSchema, RecordSchema, TextSchema, UniqueIdSchema),
  )
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten, WriteAccepted),
  )
import Eclips.Domain.Alignment
  ( HeraldPublicationPrefix (EmptyHeraldPublicationPrefix),
  )
import Eclips.Domain.Graph (VertexId (DeltaVertex, NeutralVertex))
import Eclips.Domain.Identity
  ( AuthorityEpochView (..),
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    LabelDecisionId,
    NablaId,
    NablaSequencing (NablaSequencedBy, UnsequencedNabla),
    ProcessEpochId,
    PublicationId,
    authorityEpochView,
    controlIndex,
    controlIndexWord64,
    deltaIdFromGlobalObjectId,
    genesisAuthorityEpoch,
    globalObjectIdBytes,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalUniqueIdFromGlobalObjectId,
    labelAuthorityEpoch,
    mkBootstrapManifestId,
    mkDeltaId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    nablaIdFromGlobalObjectId,
    sortIdBytes,
    structuralAuthorityEpoch,
  )
import Eclips.Domain.Label
  ( ReleasedLabelStateView (ReleasedDeletedView, ReleasedLabelView),
    firstLabelProcessAcceptancePosition,
    homeLabelAcceptanceCut,
    initialBootstrapLabelEvidence,
    initialLabelEvidenceLabel,
    initialLabelEvidenceObject,
    labelRecordReleasedState,
    releasedLabelStateView,
    targetVoid,
  )
import Eclips.Domain.Label qualified as DomainLabel
import Eclips.Domain.Membership
  ( FailureProbeResolution (RetireFailureProbeTarget),
    HeraldMembershipGeneration,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart qualified as ProcessStart
import Eclips.Domain.Publication
  ( checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationSort,
    checkedPublicationValue,
  )
import Eclips.Domain.Route
  ( ReplicaStrength (Normal),
    routeDestinations,
  )
import Eclips.Domain.Sort.Descriptor (controlledObjectKey)
import Eclips.Domain.Sort.Profile
  ( predefinedCatalogueDescriptor,
    predefinedCatalogueSortId,
    profileEntryFor,
  )
import Eclips.Domain.Startup
  ( PredefinedSortRole (NeutralVertexRole),
    allPredefinedSortRoles,
  )
import Eclips.Domain.Startup qualified as ProcessBootstrap
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    StructuralConsequenceCauseView (LabelReleaseCauseView),
    labelReleaseCause,
    structuralConsequenceCauseView,
  )
import Eclips.Domain.Topology
  ( TopologyOccurrenceDigest,
    mkTopologyOccurrenceDigest,
  )
import Eclips.Domain.Value
  ( Label,
    LabelOwner (ProcessLabel, VoidLabel),
    Value,
    ValueView (LabelValue),
    canonicalValueByteString,
    canonicalValueBytes,
    directProjection,
    labelValue,
    mkFieldName,
    valueAt,
    viewValue,
  )
import Eclips.Herald.Alignment.CutQueries qualified as CutQueries
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Request
  ( ApplicationReplyCursor,
    ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (Completed, OperationAccepted, Rejected, WaitAccepted),
    WaitId,
    requestId,
  )
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened),
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Authority qualified as Authority
import Eclips.Herald.Controlled.Operate qualified as ControlledOperate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchIsEmpty,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( HeraldMember (heraldMemberEpoch, heraldMemberId),
  )
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( structuralAppliedReport,
    topologyCutAcceptance,
    topologyCutAnnounceId,
    topologyCutEstablishedAck,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource
  ( TerminalSourceProblem,
    successorStructuralBaseCutId,
    terminalSourceInventory,
    terminalSourceUnionAcceptance,
    terminalSourceUnionAnnounceUnion,
    terminalSourceUnionReady,
  )
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest, GetApplicationRequestResult),
    ApplicationSessionIngress
      ( EndApplicationSession,
        OpenApplicationSession,
        ResumeApplicationSession
      ),
    HeraldInputBody
      ( ApplicationRequestInput,
        ApplicationSessionInput,
        OracleInput,
        PeerInput,
        RuntimeObserved
      ),
    PeerControl (PeerLabelInstalled, PeerStreamCompleted),
    PeerIngress (PeerControlReceived),
    RuntimeObservation (ApplicationBindingLost),
    candidateApplicationLane,
    heraldInput,
    peerPublicationReceived,
  )
import Eclips.Herald.Label.Collection qualified as Collection
import Eclips.Herald.LabelBarrier.State qualified as LabelBarrier
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction
      ( CancelOracleRetry,
        ConnectAndHelloOracle,
        ScheduleOracleProgress,
        ScheduleOracleRetry,
        SubmitOracleRequest
      ),
    OracleClientIngress
      ( OracleEntriesReceived,
        OracleHelloReceived,
        OracleRetryElapsed,
        OracleSubmissionNotReadyReceived
      ),
    OracleRetryPurpose (OracleSubmissionRetry),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
    oracleRequestDispatchEnvelope,
    oracleRequestDispatchRef,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDispatch.Internal (peerLogicalItem)
import Eclips.Herald.PeerPayload (PeerLogicalPayload (PeerLogicalPublication))
import Eclips.Herald.PeerPublication (peerPublicationBatch, publicationBatchId)
import Eclips.Herald.PeerStream qualified as Stream
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementProtocol
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.Groups qualified as PublicationGroups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupLastObservedTime,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralBaseCoordinator,
    replaceStartupStructuralProgressState,
    startupAlignmentCutQueryCache,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupInitialBootstraps,
    startupLabelBarrierState,
    startupLabelPatchState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( StructuralDebtKind (CutoverDebt, StoreReplacementDebt, TopologyAlignmentDebt),
    sortOccurrence,
    structuralConsequenceDebtKey,
    structuralDebtKeyKind,
    structuralDebtSetEntries,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.Alignment qualified as AlignmentCoordinator
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ApplicationCall
  ( ApplicationLabelWorkDisposition (..),
    advanceApplicationLabelWork,
    applyApplicationRequest,
  )
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Herald.UseCase.OracleAdvance
  ( advanceLocalLabelWork,
    applyOracleIngress,
  )
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalAppliedOracleEntryValue,
    canonicalOracleEnvelopeValue,
    canonicalizeAppliedOracleEntry,
  )
import Eclips.Oracle.Command
  ( OracleCommand,
    OracleEnvelope,
    ProbeResult (ProbeUnreachable),
    completeLabelDecisionCommand,
    decideLabelCommand,
    endProcessEpochCommand,
    openHeraldFailureProbeCommand,
    oracleEnvelope,
    reportHeraldFailureProbeCommand,
    retireHeraldEpochCommand,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label
  ( LiveTerminalOutcome,
    LiveTerminalOutcomeView (..),
    deriveLabelDecisionId,
    deriveLabelOutcomeDigest,
    labelCompletionAttestation,
    labelCompletionDecisionId,
    liveDecisionCapturedHeralds,
    liveDecisionHome,
    liveDecisionId,
    liveTerminalOutcomeView,
  )
import Eclips.Oracle.Progress (oracleProgressLabelsThrough)
import Eclips.Oracle.Projection qualified as OracleEvents
import Eclips.Oracle.State
  ( OracleState,
    oracleCurrentMembership,
    oracleLabelCollector,
    oracleOpenDecision,
    oracleOpenDecisions,
    oracleTerminalOutcome,
  )
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedInitialBootstraps,
    fixtureGeneratorSeed,
    fixtureHeraldRetirementReason,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureStep14CheckedGenesis,
    initialEffectsAreOracleWatchAndGrace,
  )
import GenesisFixtures qualified as MembershipFixtures
import OracleAdvanceProperties (fixtureFailureHeraldGenesis)
import PrimordialTestAccess (conventionalStartupPairs, fixtureEnvironmentEdges, fixtureEnvironmentHub)
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
    "live application label owner"
    [ testCase
        "a label Open is admitted before a membership transition"
        caseLabelOpenBeforeMembershipTransition,
      testCase
        "a label Open remains pre-acceptance during a membership transition"
        caseLabelOpenHeldDuringMembershipTransition,
      testCase
        "successor-base installation releases the retained label Open"
        caseLabelOpenReleasedAfterSuccessorBase,
      testCase
        "exact transition and request replay cannot bypass the label hold"
        caseLabelMembershipHoldExactReplay,
      testCase
        "retirement preserves the immutable decision and releases its installation slot"
        caseRetirementPreservesDecision,
      testCase
        "provider-blocked candidates retain canonical promotion and rejection order"
        caseCandidatePromotionAndRejection,
      testCase
        "qualifying write and forward holds publish nothing and exact retry only reoffers"
        caseQualifyingHoldsAndExactRetry,
      testCase
        "a pending label gates its caller's forwards but permits another local caller's forward of the same object"
        caseCallerQualifiedForwardFence,
      testCase
        "another caller's real forward retains an unseen intermediate generation beneath the current overlay"
        caseCallerForwardIntermediateGeneration,
      testCase "a fresh dynamic Nabla label waits for its own immutable topology provenance" caseLabelWaitsForInitialNablaProvider,
      testCase "a source-draining label waits without a home barrier and submits on actual peer completion" caseSourceDrainPeerCompletion,
      testCase "completed labels reclaim Oracle requests while preserving exact application retries" caseCompletedLabelRequestReclamation,
      testCase "same-home unrelated labels complete independently while one installation waits" caseIndependentSameHomeLabels,
      testCase "a foreign workflow preserves a source-draining label and releases unrelated queued work" caseSourceDrainForeignWorkflow,
      testCase "canonical caller End cancels an unsent source-draining label" caseSourceDrainCallerEnd,
      testCase "caller End preserves independent submitted labels and another local caller" caseConcurrentLabelsCallerEnd,
      testCase "real delayed source publication after End settles receivers before closing its source group" caseSourceGroupDelayedDeliveryAfterEnd,
      testCase
        "terminal completion replays held work before gated ingress with the released label"
        caseGateAndTerminalRedrive,
      testCase
        "application label progress exactly reports held, gated, and fixed-point changes"
        caseApplicationLabelWorkDisposition,
      testCase
        "application label progress reports blocked, accepted, and rejected candidates"
        caseApplicationLabelCandidateDisposition,
      testCase
        "an expected-label mismatch is decided canonically without installation collection"
        caseExpectedLabelMismatchIsCanonical,
      testCase
        "a stale retained candidate receives the same canonical CAS outcome"
        caseRetainedExpectedLabelMismatchIsCanonical,
      testCase
        "released delete rejects held update and forward without publishing either"
        caseDeleteRejectsFenceHeldPublications,
      testCase
        "void-labelled dynamic Nabla rejects its own held write without publishing it"
        caseVoidDynamicNablaRejectsOwnHeldWrite,
      testCase
        "an unrouted ordinary controlled delete is inherited by the first later Store"
        caseUnroutedControlledDeleteInheritedByFirstStore,
      testCase
        "session and process retirement detach reply reachability but preserve semantic work"
        caseDetachedReplyLinks,
      testCase
        "atomic decision progress cancels a pending NotReady retry"
        caseDecisionDuringNotReadyCancelsRetry,
      testCase
        "early installation reports and old completion retries preserve the active workflow"
        caseInstallationCollectionOrdering,
      testCase
        "bootstrap hub and spokes use ordinary label semantics with no anonymous graph fallback"
        caseGenesisWiringLabels,
      testCase
        "a genesis delta handoff updates every live topology owner and retains its old Store"
        caseGenesisDeltaHandoff,
      testCase
        "a genesis delta delete removes live topology and cannot be resurrected"
        caseGenesisDeltaDelete,
      testCase
        "a same-controller release advances only the structural control prefix"
        caseSameControllerReleaseIsTopologyNeutral,
      testCase
        "genesis Nabla authority resolves handoff history and current applicability"
        caseGenesisNablaAuthorityHandoff,
      testCase
        "genesis Nabla deletion retires current authority without erasing history"
        caseGenesisNablaAuthorityDelete,
      testCase
        "dynamic Nabla handoff retains structural history and installs label authority"
        caseDynamicNablaAuthorityHandoff
    ]

-- Retire a captured participant after the atomic decision. The decision stays
-- applied, while collection uses only surviving members and permits the next
-- same-object decision after canonical completion.
caseRetirementPreservesDecision :: Assertion
caseRetirementPreservesDecision = do
  let genesis = fixtureFailureHeraldGenesis
      bootstraps = checked "cancellation fixture local bootstrap" (Genesis.checkInitialBootstraps genesis (Genesis.PrimordialProcessManifest (take 1 fixtureLocalBootstrapIds)))
      captured = fmap heraldMemberEpoch (Genesis.checkedActiveHeralds genesis)
      local = Genesis.checkedLocalHeraldEpoch genesis
      remotes = filter (/= local) captured
  retired <- case reverse remotes of
    member : _ -> pure member
    [] -> assertFailure "cancellation fixture has no remote member"
  let survivors = filter (/= retired) captured
      remoteSurvivors = filter (/= local) survivors
  (oracle, initial, opened) <- initializeOpenedFixtureFor genesis bootstraps
  privateObject <- case Map.lookup Access.environmentHubKey (Access.accessEntries (Access.startupAccessPrimordial (openedAccess opened))) of
    Just (Access.Object object) -> pure object
    _ -> assertFailure "cancellation fixture has no bootstrap hub grant"
  let process = requiredProcess (openedSessionId opened) (startupApplicationState initial)
      operation = LabelApplication privateObject (openedProcessLabel opened) LabelToVoid
  object <- globalObjectIdFromGlobalUniqueId <$> checkedIO "resolve cancellation hub" (Application.resolveApplicationPrivateUniqueId process (privateObjectUniqueId privateObject) (startupApplicationState initial))
  (accepted, _) <- call 3 opened 1 operation initial
  let decision = activeDecision accepted
  (decidedOracle, decided, _) <- commitNextLabelRequest 4 oracle accepted
  installed <- maybe (assertFailure "atomic decision installs its target") pure (LabelPatch.lookupRetainedLabelPatch decision (startupLabelPatchState decided))
  assertBool "direct installation retains historical completion" (LabelPatch.retainedPatchViewInstallation installed /= Nothing)
  let view = OracleProjection.oracleView (startupOracleProjectionState decided)
      generation = OracleProjection.oracleViewCurrentHeraldMembershipId view
      configuration = Voter.voterConfigurationId (Voter.oracleVoterConfiguration decidedOracle)
      probeIndex = controlIndex (1 + controlIndexWord64 (OracleProjection.oracleViewControlIndex view))
  probe <- checkedIO "canonical cancellation failure probe" (deriveHeraldFailureProbeId probeIndex)
  reporter <- case remoteSurvivors of
    first : _ -> pure first
    [] -> assertFailure "cancellation fixture has no surviving remote voter"
  (probeOracle, probeOpened, _) <- commitRemoteLabelCommand 11 reporter (openHeraldFailureProbeCommand retired generation configuration) decidedOracle decided
  (reportedOracle, reported) <-
    foldM
      ( \(currentOracle, current) (observed, voter) -> do
          (nextOracle, next, _) <- commitRemoteLabelCommand observed voter (reportHeraldFailureProbeCommand probe configuration ProbeUnreachable) currentOracle current
          pure (nextOracle, next)
      )
      (probeOracle, probeOpened)
      (zip [12 ..] remoteSurvivors)
  let resolution = deriveFailureProbeResolutionId probe RetireFailureProbeTarget
  (retiredOracle, retiredState, _) <- commitRemoteLabelCommand 15 reporter (retireHeraldEpochCommand resolution retired) reportedOracle reported
  assertBool "canonical retirement removes H4" (retired `notElem` heraldMembershipGenerationActiveHeraldEpochs (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState retiredState))))
  assertEqual "retirement cannot rewrite the already applied decision" (Just (ReleasedLabelView (VoidLabel, 1))) (releasedLabelStateView <$> Controlled.controlledEffectiveLabelState object (startupControlledState retiredState))
  assertEqual "retirement retains the installed decision" (Just installed) (LabelPatch.lookupRetainedLabelPatch decision (startupLabelPatchState retiredState))
  (completedOracle, completed, _) <- commitLabelTerminalCollection 16 retiredOracle retiredState
  assertEqual "survivor collection retires installation evidence" Nothing (fixtureBarrierDecision (startupLabelBarrierState completed))
  base <- maybe (assertFailure "retirement retained no structural base") pure (startupStructuralBaseCoordinator completed)
  established <- establishLabelStructuralBase base completed
  (installedProgress, installedCoordinator) <- checkedIO "install successor base" (StructuralBase.installSuccessorStructuralBase (startupStructuralProgressState completed) established)
  baseEvidence <- maybe (assertFailure "successor base is not installed") pure (StructuralBase.structuralBaseEvidence installedCoordinator)
  let withBase = replaceStartupStructuralBaseCoordinator (Just installedCoordinator) . replaceStartupStructuralProgressState installedProgress $ completed
  (ready, _) <- checkedIO "settle successor base" (StructuralCoordinator.settleInstalledStructuralCuts [successorStructuralBaseCutId baseEvidence] withBase)
  (nextAccepted, _) <- call 30 opened 2 (LabelApplication privateObject (ApplicationValue.VoidLabel, 1) LabelToVoid) ready
  let nextDecision = activeDecision nextAccepted
  assertBool "a later label has a fresh decision identity" (nextDecision /= decision)
  (_, nextDecided, _) <- commitNextLabelRequest 31 completedOracle nextAccepted
  assertEqual "the next decision increments generation exactly once" (Just (ReleasedLabelView (VoidLabel, 2))) (releasedLabelStateView <$> Controlled.controlledEffectiveLabelState object (startupControlledState nextDecided))
  assertEqual "retirement and later decision preserve invariants" (Right ()) (validateHeraldState nextDecided)

caseLabelOpenBeforeMembershipTransition :: Assertion
caseLabelOpenBeforeMembershipTransition = do
  fixture <- membershipLabelTransitionFixture
  assertMembershipCoordinatesEqual
    "predecessor membership coordinates"
    fixture.predecessor
  (accepted, effects) <-
    call
      93
      fixture.opened
      3
      fixture.operation
      fixture.predecessor
  assertBool
    "the ordinary pre-transition Open owns the label workflow"
    (Application.applicationActiveLabel (startupApplicationState accepted) /= Nothing)
  assertEqual
    "the ordinary pre-transition Open is accepted"
    [OperationAccepted (requestId 3) LabelSettlementPending]
    (applicationReplyBodies effects)

caseLabelOpenHeldDuringMembershipTransition :: Assertion
caseLabelOpenHeldDuringMembershipTransition = do
  fixture <- membershipLabelTransitionFixture
  assertMembershipCoordinatesDiffer
    "membership advance precedes successor-base installation"
    fixture.duringTransition
  (held, effects) <- holdMembershipTransitionLabel fixture
  assertHeldMembershipTransitionLabel fixture held effects

caseLabelOpenReleasedAfterSuccessorBase :: Assertion
caseLabelOpenReleasedAfterSuccessorBase = do
  fixture <- membershipLabelTransitionFixture
  (held, heldEffects) <- holdMembershipTransitionLabel fixture
  assertHeldMembershipTransitionLabel fixture held heldEffects
  afterBase <- installMembershipLabelSuccessorBase fixture held
  assertMembershipCoordinatesEqual
    "successor base restores one coherent membership coordinate"
    afterBase
  (promoted, effects, disposition) <-
    checkedIO
      "release membership-held label after successor base"
      (advanceApplicationLabelWork afterBase)
  assertEqual
    "the retained pre-acceptance owner changes exactly once"
    ApplicationLabelWorkChanged
    disposition
  assertBool
    "the retained Open owns the workflow after the base"
    (Application.applicationActiveLabel (startupApplicationState promoted) /= Nothing)
  assertEqual
    "the retained Open receives its first acceptance only after the base"
    [OperationAccepted (requestId 3) LabelSettlementPending]
    (applicationReplyBodies effects)
  assertEqual "candidate promotion leaves source submission to the label driver" Nothing (fixtureBarrierDecision (startupLabelBarrierState promoted))
  (submitted, submissionEffects) <- checkedIO "submit the promoted source-complete label" (advanceLocalLabelWork promoted)
  assertSingleLabelSubmit "successor-base label submission" submitted submissionEffects
  assertEqual
    "the promoted whole-Herald state remains coherent"
    (Right ())
    (validateHeraldState submitted)

caseLabelMembershipHoldExactReplay :: Assertion
caseLabelMembershipHoldExactReplay = do
  fixture <- membershipLabelTransitionFixture
  (held, heldEffects) <- holdMembershipTransitionLabel fixture
  assertHeldMembershipTransitionLabel fixture held heldEffects
  (requestReplay, requestReplayEffects) <-
    checkedIO
      "replay membership-held label request"
      ( applyApplicationRequest
          ( CallApplicationRequest
              (openedBinding fixture.opened)
              (openedSessionId fixture.opened)
              (requestId 3)
              fixture.operation
          )
          held
      )
  assertBool "exact held request replay is state-identical" (held == requestReplay)
  assertBool
    "exact held request replay is silent"
    (effectBatchIsEmpty requestReplayEffects)
  replay <-
    checkedIO
      "replay membership transition around retained label"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successorMembership
          fixture.processEnds
          held
      )
  assertEqual
    "the exact membership transition is recognized"
    MembershipAdvance.MembershipAdvanceExactDuplicate
    (MembershipAdvance.preparedMembershipAdvanceDisposition replay)
  assertBool
    "membership replay cannot move the label candidate"
    (held == MembershipAdvance.commitMembershipAdvance replay)
  assertBool
    "membership replay emits no acceptance or control work"
    (effectBatchIsEmpty (MembershipAdvance.preparedMembershipAdvanceEffects replay))
  afterBase <- installMembershipLabelSuccessorBase fixture held
  (promoted, _, firstDisposition) <-
    checkedIO
      "promote membership-held label exactly once"
      (advanceApplicationLabelWork afterBase)
  (replayed, replayedEffects, replayedDisposition) <-
    checkedIO
      "re-drive promoted membership-held label"
      (advanceApplicationLabelWork promoted)
  assertEqual "the first post-base drive changes the owner" ApplicationLabelWorkChanged firstDisposition
  assertBool "the exact post-base replay is state-identical" (promoted == replayed)
  assertBool "the exact post-base replay is silent" (effectBatchIsEmpty replayedEffects)
  assertEqual
    "the exact post-base replay reports no progress"
    ApplicationLabelWorkUnchanged
    replayedDisposition

data MembershipLabelTransitionFixture = MembershipLabelTransitionFixture
  { opened :: Opened,
    operation :: ApplicationOperation,
    predecessor :: HeraldState,
    duringTransition :: HeraldState,
    index :: ControlIndex,
    successorMembership :: HeraldMembershipGeneration,
    processEnds :: [OracleProjection.MembershipAdvanceProcessEnd],
    membershipAdvance :: MembershipAdvance.PreparedMembershipAdvance,
    structuralBase :: StructuralBase.MembershipBaseClosure
  }

membershipLabelTransitionFixture :: IO MembershipLabelTransitionFixture
membershipLabelTransitionFixture = do
  (_, openedState, opened) <- initializeOpenedFixture
  writer <- writerFor Access.NeutralVertexRole (openedAccess opened)
  (reserved, newIdEffects) <-
    call
      90
      opened
      1
      (NewIdApplication (ControlledNewId writer))
      openedState
  privateObject <- completedNewId 1 newIdEffects
  let initialValue = neutralValue privateObject (startupAccessProcess (openedAccess opened))
  (pendingMaterialization, _) <-
    call
      91
      opened
      2
      (WriteApplication writer (PublishValue initialValue))
      reserved
  pendingPrefix <- settleStructuralPublication pendingMaterialization
  beforePrefix <- completeAssignedPeerPublications pendingPrefix
  let predecessor = fixtureHeraldAfterProbePrefix beforePrefix
  retired <- case reverse fixtureRemoteHeralds of
    remote : _ -> pure remote
    [] -> assertFailure "membership-label fixture has no remote Herald"
  let index = controlIndex 2
      projection = startupOracleProjectionState predecessor
      view = OracleProjection.oracleView projection
      predecessorMembership = OracleProjection.oracleViewCurrentHeraldMembership view
      successorMembership =
        checked
          "membership-label successor"
          (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) retired predecessorMembership)
      processEnds =
        [ OracleProjection.membershipAdvanceProcessEnd
            process
            index
            fixtureHeraldRetirementReason
        | process <- OracleProjection.projectedProcessEpochs projection,
          OracleProjection.oracleViewProcessResidence process view == Just retired,
          OracleProjection.oracleViewProcessIsLive process view
        ]
      operation =
        LabelApplication
          (asPrivateObjectId privateObject)
          (openedProcessLabel opened)
          LabelToVoid
  membershipAdvance <-
    checkedIO
      "membership-label transition"
      ( MembershipAdvance.prepareMembershipAdvance
          index
          successorMembership
          processEnds
          predecessor
      )
  let duringTransition = MembershipAdvance.commitMembershipAdvance membershipAdvance
  structuralBase <-
    checkedIO
      "begin membership-label structural base"
      ( StructuralBase.beginStructuralBaseAfterAppliedMembershipAdvance
          []
          predecessor
          duringTransition
      )
  assertEqual
    "membership-label transition remains composed-valid before its base"
    (Right ())
    (validateHeraldState duringTransition)
  pure
    MembershipLabelTransitionFixture
      { opened,
        operation,
        predecessor,
        duringTransition,
        index,
        successorMembership,
        processEnds,
        membershipAdvance,
        structuralBase
      }

-- | Reusable whole-Herald closure fixture.  The current state is strictly
-- between membership projection and successor-base installation and contains
-- one validated label Open in the ordinary pre-acceptance candidate owner.
membershipHeldLabelClosureFixture ::
  IO
    ( HeraldState,
      HeraldState,
      MembershipAdvance.PreparedMembershipAdvance,
      StructuralBase.MembershipBaseClosure
    )
membershipHeldLabelClosureFixture = do
  fixture <- membershipLabelTransitionFixture
  (held, effects) <- holdMembershipTransitionLabel fixture
  assertHeldMembershipTransitionLabel fixture held effects
  established <- establishMembershipLabelStructuralBase fixture held
  pure
    ( fixture.predecessor,
      held,
      fixture.membershipAdvance,
      established
    )

holdMembershipTransitionLabel ::
  MembershipLabelTransitionFixture ->
  IO (HeraldState, EffectBatch)
holdMembershipTransitionLabel fixture =
  call
    92
    fixture.opened
    3
    fixture.operation
    fixture.duringTransition

assertHeldMembershipTransitionLabel ::
  MembershipLabelTransitionFixture ->
  HeraldState ->
  EffectBatch ->
  Assertion
assertHeldMembershipTransitionLabel fixture held effects = do
  assertBool "membership-held Open emits no effects" (effectBatchIsEmpty effects)
  assertEqual
    "membership-held Open has no active label owner"
    Nothing
    (Application.applicationActiveLabel (startupApplicationState held))
  assertEqual
    "membership-held Open retains one pre-acceptance candidate"
    1
    (length (Application.applicationLabelCandidateEntries (startupApplicationState held)))
  assertEqual
    "membership-held Open allocates no process position"
    (acceptanceCounters (startupApplicationState fixture.duringTransition))
    (acceptanceCounters (startupApplicationState held))
  assertEqual
    "membership-held Open owns no fence"
    Nothing
    (fixtureBarrierDecision (startupLabelBarrierState held))
  assertEqual
    "membership-held Open owns no Oracle intention"
    []
    (OracleClient.oracleClientLabelRequestActions (startupOracleClientState held))
  assertEqual
    "membership-held whole-Herald state remains coherent"
    (Right ())
    (validateHeraldState held)

installMembershipLabelSuccessorBase ::
  MembershipLabelTransitionFixture ->
  HeraldState ->
  IO HeraldState
installMembershipLabelSuccessorBase fixture state = do
  established <- establishMembershipLabelStructuralBase fixture state
  (successorProgress, _) <-
    checkedIO
      "install membership-label successor base"
      ( StructuralBase.installSuccessorStructuralBase
          (startupStructuralProgressState state)
          established
      )
  let successor = replaceStartupStructuralProgressState successorProgress state
  assertEqual
    "membership-label successor base remains composed-valid"
    (Right ())
    (validateHeraldState successor)
  pure successor

establishMembershipLabelStructuralBase ::
  MembershipLabelTransitionFixture ->
  HeraldState ->
  IO StructuralBase.MembershipBaseClosure
establishMembershipLabelStructuralBase fixture = establishLabelStructuralBase fixture.structuralBase

establishLabelStructuralBase ::
  StructuralBase.MembershipBaseClosure ->
  HeraldState ->
  IO StructuralBase.MembershipBaseClosure
establishLabelStructuralBase base state = do
  let progress = startupStructuralProgressState state
      lineage = StructuralBase.structuralBaseLineage base
      successorMembership =
        StructuralBase.structuralBaseSuccessorMembership base
      retiredSources = Set.toAscList (StructuralBase.structuralBaseRetiredSources base)
      local = StructuralBase.structuralBaseLocalHerald base
      reporters = case heraldMembershipGenerationActiveHeraldEpochs successorMembership of
        first :| remaining -> first : remaining
  remoteInventories <-
    traverse
      ( \(retired, reporter) ->
          checkedIO
            "construct membership-label survivor inventory"
            (terminalSourceInventory lineage retired reporter [] [] [])
      )
      [(retired, reporter) | retired <- retiredSources, reporter <- filter (/= local) reporters]
  withInventories <-
    foldM
      ( \coordinator inventory ->
          checkedIO
            "receive membership-label survivor inventory"
            (StructuralBase.receiveStructuralBaseInventory inventory coordinator)
      )
      base
      remoteInventories
  (withUnion, announce) <-
    checkedIO
      "announce membership-label structural union"
      ( StructuralBase.prepareStructuralBaseUnionAnnouncement
          membershipLabelTopologyDigest
          withInventories
      )
  ready <-
    checkedIO
      "check membership-label structural union readiness"
      ( terminalSourceUnionReady
          (GraphProgress.structuralAppliedVector progress)
          (GraphProgress.structuralAppliedControlPrefix progress)
          (terminalSourceUnionAnnounceUnion announce)
      )
  acceptances <-
    traverse
      ( \reporter ->
          checkedIO
            "accept membership-label structural union"
            (terminalSourceUnionAcceptance successorMembership reporter ready)
      )
      reporters
  withAcceptances <-
    foldM
      ( \coordinator acceptance ->
          checkedIO
            "receive membership-label structural acceptance"
            ( StructuralBase.receiveStructuralBaseUnionAcceptance
                acceptance
                coordinator
            )
      )
      withUnion
      acceptances
  (established, _) <-
    checkedIO
      "establish membership-label structural union"
      (StructuralBase.establishStructuralBaseUnion withAcceptances)
  pure established

membershipLabelTopologyDigest ::
  vector ->
  ControlIndex ->
  Either TerminalSourceProblem TopologyOccurrenceDigest
membershipLabelTopologyDigest _ _ =
  Right
    ( checked
        "membership-label topology digest"
        (mkTopologyOccurrenceDigest (ByteString.replicate 32 0x6d))
    )

assertMembershipCoordinatesEqual :: String -> HeraldState -> Assertion
assertMembershipCoordinatesEqual context state =
  assertEqual
    context
    ( OracleProjection.oracleViewCurrentHeraldMembershipId
        (OracleProjection.oracleView (startupOracleProjectionState state))
    )
    ( GraphProgress.structuralProgressMembershipGenerationId
        (startupStructuralProgressState state)
    )

assertMembershipCoordinatesDiffer :: String -> HeraldState -> Assertion
assertMembershipCoordinatesDiffer context state =
  assertBool
    context
    ( OracleProjection.oracleViewCurrentHeraldMembershipId
        (OracleProjection.oracleView (startupOracleProjectionState state))
        /= GraphProgress.structuralProgressMembershipGenerationId
          (startupStructuralProgressState state)
    )

caseQualifyingHoldsAndExactRetry :: Assertion
caseQualifyingHoldsAndExactRetry = do
  fixture <- heldFixture
  let before = heldBeforeFence fixture
      afterWrite = heldAfterWrite fixture
      afterForward = heldState fixture
  assertOwnersUnchangedByHold "write hold" before afterWrite
  assertOwnersUnchangedByHold "forward hold" afterWrite afterForward
  assertSingleLabelSubmit
    "initial Label"
    (heldBeforeFence fixture)
    (heldLabelEffects fixture)
  assertEqual "write hold emits no application reply" [] (applicationReplyBodies (heldWriteEffects fixture))
  assertEqual "forward hold emits no application reply" [] (applicationReplyBodies (heldForwardEffects fixture))
  held <- exactlyTwoHeld afterForward
  case held of
    [writeView, forwardView] -> do
      let writeWork = Application.applicationFenceHeldSemanticWork writeView
          forwardWork = Application.applicationFenceHeldSemanticWork forwardView
      assertEqual "first held call" (heldWriteOperation fixture) (Application.applicationFenceHeldOperation writeView)
      assertEqual "second held call" (heldForwardOperation fixture) (Application.applicationFenceHeldOperation forwardView)
      assertEqual "write retains its writer" (heldGlobalWriter fixture) (heldWriter writeView)
      assertEqual "forward retains its writer" (heldGlobalWriter fixture) (heldWriter forwardView)
      assertBool
        "write and forward retain distinct semantic origins"
        (heldOrigin writeView /= heldOrigin forwardView)
      assertEqual
        "both held operations retain the exact acceptance-time source sort"
        (Application.applicationFenceHeldWorkSortId writeWork)
        (Application.applicationFenceHeldWorkSortId forwardWork)
      assertEqual
        "both held operations retain the exact acceptance-time source occurrence"
        (Application.applicationFenceHeldWorkSortOccurrenceId writeWork)
        (Application.applicationFenceHeldWorkSortOccurrenceId forwardWork)
      assertEqual
        "both held operations retain their acceptance-time control floor"
        (Application.applicationFenceHeldWorkControlPrerequisite writeWork)
        (Application.applicationFenceHeldWorkControlPrerequisite forwardWork)
    _ -> assertFailure "expected exactly two canonical held entries"
  (retried, retryEffects) <-
    call 8 (heldOpened fixture) 4 (heldWriteOperation fixture) afterForward
  assertEqual "held exact retry emits no application reply" [] (applicationReplyBodies retryEffects)
  assertBool
    "held exact retry changes no application owner fact"
    (startupApplicationState afterForward == startupApplicationState retried)
  assertOwnersUnchangedByHold "held exact retry" afterForward retried

-- Both processes acquire their capabilities through checked startup and real
-- application calls. The cross-environment edge makes P's first publication
-- readable by Q; no possession or private identity is injected into an owner.
data CallerForwardFixture = CallerForwardFixture
  { callerForwardOracle :: OracleState,
    callerForwardState :: HeraldState,
    callerForwardFirst :: Opened,
    callerForwardSecond :: Opened,
    callerForwardFirstWriter :: PrivateNablaId,
    callerForwardSecondWriter :: PrivateNablaId,
    callerForwardFirstObject :: PrivateUniqueId,
    callerForwardSecondObject :: PrivateUniqueId,
    callerForwardGlobal :: GlobalUniqueId
  }

callerForwardFixture :: IO CallerForwardFixture
callerForwardFixture = do
  (firstBootstrap, secondBootstrap) <- case fixtureLocalBootstrapIds of
    first : second : _ -> pure (first, second)
    _ -> assertFailure "caller-qualified fixture needs two local bootstraps"
  bootstraps <-
    checkedIO
      "admit two local environments with a normal publication route"
      ( Genesis.checkInitialBootstrapsWithTopology
          fixtureStep14CheckedGenesis
          (Genesis.PrimordialProcessManifest (fixtureLocalBootstrapIds <> [MembershipFixtures.fixtureRemoteBootstrapId]))
          (Genesis.InitialTopologyManifest [Genesis.InitialTopologyProcessEdge firstBootstrap secondBootstrap NeutralVertexRole])
      )
  (oracle, firstOpened, first) <- initializeOpenedFixtureWithBootstraps bootstraps
  secondAttachment <-
    maybe
      (assertFailure "second primordial application attachment missing")
      pure
      (primordialApplicationAttachment fixtureStep14CheckedGenesis bootstraps secondBootstrap)
  (bothOpened, second) <- open 3 secondAttachment firstOpened
  firstWriter <- writerFor Access.NeutralVertexRole (openedAccess first)
  secondWriter <- writerFor Access.NeutralVertexRole (openedAccess second)
  secondReader <- readerFor Access.NeutralVertexRole (openedAccess second)
  (reserved, identityEffects) <- call 4 first 1 (NewIdApplication (ControlledNewId firstWriter)) bothOpened
  privateObject <- completedNewId 1 identityEffects
  (pending, _) <-
    call
      5
      first
      2
      (WriteApplication firstWriter (PublishValue (neutralValue privateObject (startupAccessProcess (openedAccess first)))))
      reserved
  published <- settleStructuralPublication pending
  (readState, readEffects) <- call 6 second 1 (ReadApplication (ApplicationQuery (Set.singleton secondReader) QueryAlways)) published
  values <- case applicationReplyBodies readEffects of
    [Completed request (ReadCompleted observed)] | request == requestId 1 -> pure observed
    other -> assertFailure ("second caller did not read the routed object: " <> show other)
  let firstProcess = requiredProcess (openedSessionId first) (startupApplicationState readState)
      secondProcess = requiredProcess (openedSessionId second) (startupApplicationState readState)
  global <-
    checkedIO
      "resolve the first caller's published object"
      (Application.resolveApplicationPrivateUniqueId firstProcess privateObject (startupApplicationState readState))
  secondObject <- case [ private
                       | ApplicationValue.RecordValue fields <- values,
                         Just (ApplicationValue.UniqueIdValue private) <- [Map.lookup "object_id" fields],
                         Right observed <- [Application.resolveApplicationPrivateUniqueId secondProcess private (startupApplicationState readState)],
                         observed == global
                       ] of
    [private] -> pure private
    matches -> assertFailure ("expected one real read capability for the shared object, got " <> show (length matches))
  assertBool "the regression uses distinct local process incarnations" (firstProcess /= secondProcess)
  firstPublication <- case Publication.applicationPublicationEntries (startupPublicationState published) of
    [(_, record)] -> pure record
    records -> assertFailure ("expected one initial controlled publication, got " <> show (length records))
  assertEqual
    "first publication retains its original issuer's self-object group"
    (Set.singleton (PublicationGroups.groupKey firstProcess (globalObjectIdFromGlobalUniqueId global)))
    (PublicationGroups.membershipKeys (Publication.applicationPublicationGroupMembership firstPublication))
  pure
    CallerForwardFixture
      { callerForwardOracle = oracle,
        callerForwardState = readState,
        callerForwardFirst = first,
        callerForwardSecond = second,
        callerForwardFirstWriter = firstWriter,
        callerForwardSecondWriter = secondWriter,
        callerForwardFirstObject = privateObject,
        callerForwardSecondObject = secondObject,
        callerForwardGlobal = global
      }

caseCallerQualifiedForwardFence :: Assertion
caseCallerQualifiedForwardFence = do
  fixture <- callerForwardFixture
  let readState = callerForwardState fixture
      first = callerForwardFirst fixture
      second = callerForwardSecond fixture
      firstWriter = callerForwardFirstWriter fixture
      secondWriter = callerForwardSecondWriter fixture
      privateObject = callerForwardFirstObject fixture
      secondObject = callerForwardSecondObject fixture
      global = callerForwardGlobal fixture
      firstProcess = requiredProcess (openedSessionId first) (startupApplicationState readState)
      secondProcess = requiredProcess (openedSessionId second) (startupApplicationState readState)
  (labelPending, _) <- call 7 first 3 (LabelApplication (asPrivateObjectId privateObject) (openedProcessLabel first) LabelToVoid) readState
  case Application.applicationGatePhase (startupApplicationState labelPending) of
    Application.ApplicationGateOpen {} -> pure ()
    _ -> assertFailure "the test must exercise caller selection before Ready closes the global gate"
  let secondForward = ForwardApplication secondWriter (asPrivateObjectId secondObject)
      firstForward = ForwardApplication firstWriter (asPrivateObjectId privateObject)
      beforeIds = Set.fromList (Publication.applicationPublicationId . snd <$> Publication.applicationPublicationEntries (startupPublicationState labelPending))
  (afterSecond, secondEffects) <- call 8 second 2 secondForward labelPending
  assertEqual
    "another caller's same-object forward is accepted before Ready"
    [OperationAccepted (requestId 2) StructuralStabilizationPending]
    (applicationReplyBodies secondEffects)
  assertEqual "another caller is not retained behind this label" [] (Application.applicationFenceHeldEntries (startupApplicationState afterSecond))
  secondRecord <- case [ record
                       | (_, record) <- Publication.applicationPublicationEntries (startupPublicationState afterSecond),
                         Publication.applicationPublicationId record `Set.notMember` beforeIds
                       ] of
    [record] -> pure record
    records -> assertFailure ("expected one admitted second-caller publication, got " <> show (length records))
  assertEqual
    "the admitted forward records only its actual issuer's object group"
    (Set.singleton (PublicationGroups.groupKey secondProcess (globalObjectIdFromGlobalUniqueId global)))
    (PublicationGroups.membershipKeys (Publication.applicationPublicationGroupMembership secondRecord))
  (afterFirst, firstEffects) <- call 9 first 4 firstForward afterSecond
  assertEqual "the label caller's forward still waits" [] (applicationReplyBodies firstEffects)
  case Application.applicationFenceHeldEntries (startupApplicationState afterFirst) of
    [held] -> do
      assertEqual "the held call belongs to the label caller" firstProcess (Request.processAcceptancePositionProcess (Application.applicationFenceHeldPosition held))
      assertEqual "the exact first-caller forward is held" firstForward (Application.applicationFenceHeldOperation held)
    held -> assertFailure ("expected only the first caller to be held, got " <> show (length held))
  assertEqual "holding the caller adds no publication" (Publication.publicationStateWitness (startupPublicationState afterSecond)) (Publication.publicationStateWitness (startupPublicationState afterFirst))

-- This combines production application admission with the authenticated
-- Controlled receiver seam. The receiver owner is taken from a separate valid
-- branch that has never seen F. It isolates historical receiver admission;
-- the complete source/receiver completion path is exercised separately.
caseCallerForwardIntermediateGeneration :: Assertion
caseCallerForwardIntermediateGeneration = do
  fixture <- callerForwardFixture
  let first = callerForwardFirst fixture
      second = callerForwardSecond fixture
      firstPrivate = callerForwardFirstObject fixture
      object = globalObjectIdFromGlobalUniqueId (callerForwardGlobal fixture)
      owner = requiredProcess (openedSessionId first) (startupApplicationState (callerForwardState fixture))
      issuer = requiredProcess (openedSessionId second) (startupApplicationState (callerForwardState fixture))
  (oracleAtOne, atOne) <-
    completeSameOwnerLabel 20 3 0 first firstPrivate (callerForwardOracle fixture) (callerForwardState fixture)
  assertControlledReleasedState object (ReleasedLabelView (ProcessLabel owner, 1)) atOne
  let beforeIds = Set.fromList (Publication.applicationPublicationId . snd <$> Publication.applicationPublicationEntries (startupPublicationState atOne))
  (forwarded, forwardEffects) <-
    call
      40
      second
      2
      (ForwardApplication (callerForwardSecondWriter fixture) (asPrivateObjectId (callerForwardSecondObject fixture)))
      atOne
  assertEqual
    "the real second-caller forward is admitted"
    [OperationAccepted (requestId 2) StructuralStabilizationPending]
    (applicationReplyBodies forwardEffects)
  retained <- case [ record
                   | (_, record) <- Publication.applicationPublicationEntries (startupPublicationState forwarded),
                     Publication.applicationPublicationId record `Set.notMember` beforeIds
                   ] of
    [record] -> pure record
    records -> assertFailure ("expected one second-caller generation-one forward, got " <> show (length records))
  let frozen = Publication.applicationPublicationChecked retained
      identifier = checkedPublicationId frozen
  assertEqual
    "forward embeds the effective label at its actual source admission"
    (Just (ProcessLabel owner, 1))
    (labelFromValue (checkedPublicationValue frozen))
  assertEqual "forward retains its actual issuing process" issuer (Publication.applicationPublicationSourceProcess retained)
  assertEqual
    "forward belongs only to the forwarding caller's object group"
    (Set.singleton (PublicationGroups.groupKey issuer object))
    (PublicationGroups.membershipKeys (Publication.applicationPublicationGroupMembership retained))

  -- Branch from the state before F's admission: no manual replacement of a
  -- Herald owner or possession fact is used to construct this receiver state.
  (_, receiver) <- completeSameOwnerLabel 60 4 1 first firstPrivate oracleAtOne atOne
  assertControlledReleasedState object (ReleasedLabelView (ProcessLabel owner, 2)) receiver
  let controlled = startupControlledState receiver
  original <- maybe (assertFailure "receiver lost the shared object") pure (Controlled.controlledLocalRecord object controlled)
  assertEqual
    "receiver retains the original embedded generation"
    (Just (ProcessLabel owner, 0))
    (Controlled.controlledRecordObservedLabel original)
  assertEqual
    "the receiver has never observed the intermediate publication"
    Nothing
    (Controlled.controlledRecordObservation identifier original)
  observation <-
    checkedIO
      "derive the frozen source's checked controlled observation"
      ( Controlled.checkControlledObservation
          (predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole))
          (Publication.applicationPublicationSortOccurrenceId retained)
          frozen
      )
  prepared <-
    checkedIO
      "admit unseen frozen intermediate generation beneath a later overlay"
      (Controlled.prepareControlledPeerObservation observation controlled)
  let (observed, _) = Controlled.commitControlledPeerObservation prepared
  record <- maybe (assertFailure "observation lost the shared object") pure (Controlled.controlledLocalRecord object observed)
  assertEqual
    "receiver retains the exact immutable forwarded evidence"
    (Just frozen)
    (Controlled.controlledRecordObservation identifier record)
  assertEqual
    "the later released generation remains authoritative"
    (Just (ReleasedLabelView (ProcessLabel owner, 2)))
    (releasedLabelStateView <$> Controlled.controlledEffectiveLabelState object observed)
  replay <-
    checkedIO
      "replay the same historical observation"
      (Controlled.prepareControlledPeerObservation observation observed)
  let (afterReplay, replayResult) = Controlled.commitControlledPeerObservation replay
  assertEqual "intermediate-generation exact replay is recognized" Controlled.ControlledPeerReplay replayResult
  assertBool "intermediate-generation exact replay is state-idempotent" (afterReplay == observed)

completeSameOwnerLabel ::
  Word -> Word -> Word64 -> Opened -> PrivateUniqueId -> OracleState -> HeraldState -> IO (OracleState, HeraldState)
completeSameOwnerLabel observed request generation opened privateObject oracle pendingPredecessor = do
  predecessor <- completeAssignedPeerPublications pendingPredecessor
  let owner = startupAccessProcess (openedAccess opened)
      operation =
        LabelApplication
          (asPrivateObjectId privateObject)
          (ApplicationValue.ProcessLabel owner, generation)
          (LabelToProcess owner)
  (accepted, _) <- call observed opened request operation predecessor
  (afterOpenOracle, afterOpen, _) <- commitNextLabelRequest (observed + 1) oracle accepted
  (ready, _) <- call (observed + 2) opened request operation (afterOpen)
  (successorOracle, successor, effects) <- commitCompleteLabelWorkflow (observed + 3) afterOpenOracle ready
  assertBool
    "same-owner label completes through the real Oracle workflow"
    (Completed (requestId (fromIntegral request)) (LabelCompleted LabelApplied) `elem` concatMap applicationReplyBodies effects)
  assertEqual "completed same-owner workflow preserves whole-Herald invariants" (Right ()) (validateHeraldState successor)
  pure (successorOracle, successor)

-- The structural cut is established, but its actual assigned peer items are
-- deliberately unacknowledged. The label is accepted with an immutable request
-- identity while only its caller-qualified source group waits.
sourceDrainingFixture :: IO (CallerForwardFixture, HeraldState, Application.ActiveLabelApplicationView, OracleClient.OracleRequestWitness)
sourceDrainingFixture = do
  fixture <- callerForwardFixture
  let first = callerForwardFirst fixture
      operation = LabelApplication (asPrivateObjectId (callerForwardFirstObject fixture)) (openedProcessLabel first) LabelToVoid
  (draining, effects) <- call 20 first 3 operation (callerForwardState fixture)
  active <- maybe (assertFailure "source-draining label has no accepted owner") pure (Application.applicationActiveLabel (startupApplicationState draining))
  witness <- drainingWitness active draining
  case OracleClient.oracleRequestWitnessIntent witness of
    OracleClient.DecideLabelIntent _ caller object expected (Just evidence) _ _ _ _ -> do
      assertEqual "draining Open binds the independently observed object" object (initialLabelEvidenceObject evidence)
      assertEqual "draining Open retains the checked initial owner" (ProcessLabel caller, 0) (initialLabelEvidenceLabel evidence)
      assertEqual "local admission still compares the requested pair" expected (initialLabelEvidenceLabel evidence)
    _ -> assertFailure "first source-draining Open did not retain independent initial evidence"
  assertEqual
    "accepted label reports its stable pending status"
    [OperationAccepted (requestId 3) LabelSettlementPending]
    (applicationReplyBodies effects)
  assertEqual
    "the source group retains actual uncompleted peer work"
    False
    (PublicationGroups.groupReady (Application.sequencingSelectionGroup (Application.activeLabelApplicationSelection active)) (Publication.publicationGroups (startupPublicationState draining)))
  assertEqual "uncompleted source work retains an unsent request" OracleClient.OracleRequestAwaitingSourceDrain (OracleClient.oracleRequestWitnessStatus witness)
  assertEqual "source drain creates no home barrier" Nothing (fixtureBarrierDecision (startupLabelBarrierState draining))
  assertEqual "source drain emits no Oracle submit" [] (OracleClient.oracleClientLabelRequestActions (startupOracleClientState draining))
  pure (fixture, draining, active, witness)

caseLabelWaitsForInitialNablaProvider :: Assertion
caseLabelWaitsForInitialNablaProvider = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  writer <- writerFor Access.NablaRole (openedAccess opened)
  (reserved, identityEffects) <- call 80 opened 1 (NewIdApplication (ControlledNewId writer)) openedState
  privateNabla <- completedNewId 1 identityEffects
  let value = dynamicNablaValue privateNabla (startupAccessProcess (openedAccess opened))
      label = LabelApplication (asPrivateObjectId privateNabla) (openedProcessLabel opened) LabelToVoid
  (pending, _) <- call 81 opened 2 (WriteApplication writer (PublishValue value)) reserved
  (waiting, replies) <- call 82 opened 3 label pending
  assertEqual "missing immutable topology provenance retains an unaccepted candidate" [] (applicationReplyBodies replies)
  assertEqual "provider wait allocates no label decision" Nothing (Application.applicationActiveLabel (startupApplicationState waiting))
  assertEqual "only the exact label request is retained" 1 (length (Application.applicationLabelCandidateEntries (startupApplicationState waiting)))
  assertEqual "provider wait preserves the caller acceptance counter" (Application.applicationRequestNextAcceptancePositions (Application.applicationRequestStateWitness (startupApplicationState pending))) (Application.applicationRequestNextAcceptancePositions (Application.applicationRequestStateWitness (startupApplicationState waiting)))
  (retried, retryReplies) <- call 82 opened 3 label waiting
  assertEqual "exact retry cannot allocate authority before installation" [] (applicationReplyBodies retryReplies)
  assertBool "exact retry preserves the retained candidate" (startupApplicationState waiting == startupApplicationState retried)
  installed <- settleStructuralPublication retried
  (ready, _) <- call 83 opened 3 label installed
  active <- maybe (assertFailure "the provider's real topology installation did not promote the original candidate") pure (Application.applicationActiveLabel (startupApplicationState ready))
  witness <- drainingWitness active ready
  assertEqual "installed provenance still preserves the selected source drain" OracleClient.OracleRequestAwaitingSourceDrain (OracleClient.oracleRequestWitnessStatus witness)
  drained <- completeAssignedPeerPublications ready
  (submitted, _) <- checkedIO "submit after exact assigned source completion" (advanceLocalLabelWork drained)
  (_, completed, _) <- commitCompleteLabelWorkflow 84 oracle submitted
  assertEqual "the original label completes after its provider" Nothing (Application.applicationActiveLabel (startupApplicationState completed))
  assertEqual "provider promotion preserves whole-Herald invariants" (Right ()) (validateHeraldState completed)

caseSourceDrainPeerCompletion :: Assertion
caseSourceDrainPeerCompletion = do
  (fixture, draining, active, original) <- sourceDrainingFixture
  let first = callerForwardFirst fixture
      operation = LabelApplication (asPrivateObjectId (callerForwardFirstObject fixture)) (openedProcessLabel first) LabelToVoid
      key = Application.sequencingSelectionGroup (Application.activeLabelApplicationSelection active)
  (retried, _) <- call 21 first 3 operation draining
  repeated <- drainingWitness active retried
  assertEqual "exact retry preserves the reserved Oracle identity" (OracleClient.oracleRequestWitnessRef original) (OracleClient.oracleRequestWitnessRef repeated)
  assertEqual "exact retry preserves the immutable Open bytes" (OracleClient.oracleRequestWitnessEnvelope original) (OracleClient.oracleRequestWitnessEnvelope repeated)
  (held, heldEffects) <- call 22 first 4 (ForwardApplication (callerForwardFirstWriter fixture) (asPrivateObjectId (callerForwardFirstObject fixture))) retried
  assertEqual "matching later publication remains held while source drains" [] (applicationReplyBodies heldEffects)
  assertEqual "exactly the matching later call is retained" 1 (length (Application.applicationFenceHeldEntries (startupApplicationState held)))
  (ready, completionEffects) <- completeSourceGroupThroughPeerIngress 30 key held
  submitted <- drainingWitness active ready
  assertEqual "actual contiguous peer completion makes the request dispatchable" OracleClient.OracleRequestAwaitingProjection (OracleClient.oracleRequestWitnessStatus submitted)
  assertEqual "drain submission preserves the original canonical envelope" (OracleClient.oracleRequestWitnessEnvelope original) (OracleClient.oracleRequestWitnessEnvelope submitted)
  assertEqual "submission precedes local installation evidence" Nothing (fixtureBarrierDecision (startupLabelBarrierState ready))
  assertEqual
    "the completion event submits the exact original Open once"
    [OracleClient.oracleRequestWitnessRef original]
    [oracleRequestDispatchRef dispatch | batch <- completionEffects, RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers batch]
  assertEqual "matching future work is still held after submission" 1 (length (Application.applicationFenceHeldEntries (startupApplicationState ready)))
  assertEqual "peer-triggered source submission preserves whole-Herald invariants" (Right ()) (validateHeraldState ready)

-- Application owns the exact final reply after settlement. Its retry path must
-- not depend on the Oracle command, semantic key, or collection request which
-- served the completed workflow. Repeat on one object to exercise successive
-- generations without retaining the earlier commands.
caseCompletedLabelRequestReclamation :: Assertion
caseCompletedLabelRequestReclamation = do
  (oracle, initial, opened) <- initializeOpenedFixture
  reader <- readerFor Access.NeutralVertexRole (openedAccess opened)
  let object = asPrivateObjectId (privateDeltaUniqueId reader)
      owner = startupAccessProcess (openedAccess opened)
      retainedRequests = OracleClient.oracleClientRequestEntries . startupOracleClientState
      completionRequests = OracleClient.oracleClientWitnessLabelCompletionRequests . OracleClient.oracleClientStateWitness . startupOracleClientState
      baselineCount = length (retainedRequests initial)
      run (priorOracle, predecessor, previousDecisions) ordinal = do
        let at = 10 * ordinal
            operation = LabelApplication object (ApplicationValue.ProcessLabel owner, fromIntegral (ordinal - 1)) (LabelToProcess owner)
            result = Completed (requestId (fromIntegral ordinal)) (LabelCompleted LabelApplied)
        (accepted, _) <- call at opened ordinal operation predecessor
        active <- maybe (assertFailure "reclamation fixture label was not accepted") pure (Application.applicationActiveLabel (startupApplicationState accepted))
        original <- drainingWitness active accepted
        let reference = OracleClient.oracleRequestWitnessRef original
            key = OracleClient.oracleRequestWitnessSemanticKey original
            lookupRequest state = OracleClient.lookupOracleRequest reference (startupOracleClientState state)
            lookupKey state = OracleClient.lookupOracleRequestBySemanticKey key (startupOracleClientState state)
        assertEqual "accepted workflow retains one request above the settled baseline" (baselineCount + 1) (length (retainedRequests accepted))
        (decidedOracle, installed, _) <- commitNextLabelRequest (at + 1) priorOracle accepted
        let index = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState installed))
        canonical <- maybe (assertFailure "projected label decision has no exact archive entry") pure (OracleProjection.appliedEntryEvidence index (startupOracleProjectionState installed))
        assertBool "an unfinished installation still pins the original decision" (labelRetirementReady installed < index)
        assertBool "the projected command survives while its installation consumer remains" (isJust (lookupRequest installed) && isJust (lookupKey installed))
        (completedOracle, completed, effects) <- commitLabelTerminalCollection (at + 2) decidedOracle installed
        assertBool "canonical settlement returns the application result" (result `elem` concatMap applicationReplyBodies effects)
        assertEqual "completed workflow releases the decision retirement pin" index (labelRetirementReady completed)
        assertEqual "the settled command is reclaimed by exact request identity" Nothing (lookupRequest completed)
        assertEqual "the settled application semantic key no longer pins the command" Nothing (lookupKey completed)
        assertEqual "settled decide and completion commands return to the retained baseline" baselineCount (length (retainedRequests completed))
        assertEqual "the settled collector request leaves its retirement index" [] (completionRequests completed)
        assertEqual "the current decision remains the exact client retirement anchor" (Just canonical) (OracleClient.oracleClientRetainedEntry index (startupOracleClientState completed))
        assertEqual
          "only the exact current plain label decision remains in the client archive"
          [index]
          [ retainedIndex
          | (retainedIndex, _) <- OracleClient.oracleClientWitnessRetainedEntryBytes (OracleClient.oracleClientStateWitness (startupOracleClientState completed)),
            Just retained <- [OracleClient.oracleClientRetainedEntry retainedIndex (startupOracleClientState completed)],
            [OracleEvents.LabelDecidedView {}] <- [map OracleEvents.oracleProjectionEventView (OracleEvents.appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue retained))]
          ]
        forM_ previousDecisions $ \(oldIndex, _) -> do
          assertEqual "completed prior decision relinquishes its duplicate client copy" Nothing (OracleClient.oracleClientRetainedEntry oldIndex (startupOracleClientState completed))
          assertEqual "Projection relinquishes the covered prior decision after its consumers finish" Nothing (OracleProjection.appliedEntryEvidence oldIndex (startupOracleProjectionState completed))
        (retried, retryEffects) <- call (at + 4) opened ordinal operation completed
        assertEqual "exact retry returns the same final application reply" [result] (applicationReplyBodies retryEffects)
        assertEqual "exact retry allocates no Oracle request identity" (OracleClient.oracleClientNextRequestSequence (startupOracleClientState completed)) (OracleClient.oracleClientNextRequestSequence (startupOracleClientState retried))
        assertEqual "exact retry preserves the compact Oracle owner" (startupOracleClientState completed) (startupOracleClientState retried)
        assertEqual "exact retry submits no command" [] [dispatch | RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers retryEffects]
        assertEqual "reclamation and exact retry preserve whole-Herald invariants" (Right ()) (validateHeraldState retried)
        forM_ previousDecisions $ \(_, oldCanonical) -> do
          binding <- maybe (assertFailure "reclaimed overlap fixture lost its Oracle binding") pure (OracleClient.oracleClientCurrentBinding (startupOracleClientState retried))
          (replayed, _) <- checkedIO "replay old exact decision without its client copy" (verifiedStepHerald (heraldInput (startupLastObservedTime retried) (OracleInput (OracleEntriesReceived binding (oldCanonical :| [])))) retried)
          assertBool "exact old watch overlap preserves the complete owner product" (replayed == retried)
        pure (completedOracle, retried, previousDecisions <> [(index, canonical)])
  _ <- foldM run (oracle, initial, []) [1 .. 3]
  pure ()

-- Keep the first immutable decision incompletely installed. A second object
-- at the same home and from the same application must still cross the Oracle,
-- collect its own reports and return. A second invocation on the first object
-- remains a candidate until its own object's installation completes.
caseIndependentSameHomeLabels :: Assertion
caseIndependentSameHomeLabels = do
  (oracle, initial, opened) <- initializeOpenedFixture
  firstReader <- readerFor Access.NeutralVertexRole (openedAccess opened)
  secondReader <- readerFor Access.DeltaRole (openedAccess opened)
  let firstObject = asPrivateObjectId (privateDeltaUniqueId firstReader)
      secondObject = asPrivateObjectId (privateDeltaUniqueId secondReader)
      owner = startupAccessProcess (openedAccess opened)
      relabel object generation = LabelApplication object (ApplicationValue.ProcessLabel owner, generation) (LabelToProcess owner)
  (firstAccepted, _) <- call 10 opened 1 (relabel firstObject 0) initial
  let firstDecision = activeDecision firstAccepted
  (firstOracle, firstInstalled, _) <- commitNextLabelRequest 11 oracle firstAccepted
  let firstIndex = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState firstInstalled))
      beforeFirst = controlIndex (controlIndexWord64 firstIndex - 1)
  assertEqual "installation pins its original decision from retirement" beforeFirst (labelRetirementReady firstInstalled)
  firstActive <- maybe (assertFailure "first installation lost application owner") pure (Application.applicationActiveLabelForDecision firstDecision (startupApplicationState firstInstalled))
  (secondAccepted, secondAcceptedEffects) <- call 12 opened 2 (relabel secondObject 0) firstInstalled
  assertEqual "second object is accepted while first installation waits" [OperationAccepted (requestId 2) LabelSettlementPending] (applicationReplyBodies secondAcceptedEffects)
  secondActive <- case filter ((/= firstDecision) . Application.activeLabelApplicationDecision) (Application.applicationActiveLabels (startupApplicationState secondAccepted)) of
    [active] -> pure active
    active -> assertFailure ("expected second independent label, got " <> show (length active))
  let secondDecision = Application.activeLabelApplicationDecision secondActive
  assertBool "caller positions remain strictly ordered across objects" (Application.activeLabelApplicationPosition firstActive < Application.activeLabelApplicationPosition secondActive)
  (bothOracle, bothInstalled, _) <- commitNextLabelRequest 13 firstOracle secondAccepted
  let secondIndex = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState bothInstalled))
  assertEqual "both objects own independent installation slots" 2 (length (oracleOpenDecisions bothOracle))
  (sameObjectWaiting, sameObjectEffects) <- call 14 opened 3 (relabel firstObject 1) bothInstalled
  assertEqual "same object remains unaccepted while its installation waits" [] (applicationReplyBodies sameObjectEffects)
  assertEqual "only same-object invocation is a candidate" 1 (length (Application.applicationLabelCandidateEntries (startupApplicationState sameObjectWaiting)))
  (secondDoneOracle, secondDone, secondEffects) <- commitLabelTerminalCollectionFor secondDecision 15 bothOracle sameObjectWaiting
  assertBool "second label returns Applied without first completion" (Completed (requestId 2) (LabelCompleted LabelApplied) `elem` concatMap applicationReplyBodies secondEffects)
  assertEqual "first immutable decision still owns its object slot" [firstDecision] (liveDecisionId <$> oracleOpenDecisions secondDoneOracle)
  assertEqual "first invocation and its terminal evidence survive second completion" (Just firstActive) (Application.applicationActiveLabelForDecision firstDecision (startupApplicationState secondDone))
  assertEqual "second invocation is independently reclaimed" Nothing (Application.applicationActiveLabelForDecision secondDecision (startupApplicationState secondDone))
  assertEqual "out-of-order completion cannot retire past the older installation" beforeFirst (labelRetirementReady secondDone)
  assertEqual "same-object candidate remains blocked" 1 (length (Application.applicationLabelCandidateEntries (startupApplicationState secondDone)))
  (_, firstDone, firstEffects) <- commitLabelTerminalCollectionFor firstDecision 20 secondDoneOracle secondDone
  assertBool "first completion finally returns its own result" (Completed (requestId 1) (LabelCompleted LabelApplied) `elem` concatMap applicationReplyBodies firstEffects)
  assertEqual "first completion releases its waiting same-object invocation" [] (Application.applicationLabelCandidateEntries (startupApplicationState firstDone))
  assertEqual "last consumer release retires through the latest actual decision" secondIndex (labelRetirementReady firstDone)
  assertBool "every source index follows independent admission and completion" (all (Application.applicationLabelIndexesValid . startupApplicationState) [firstAccepted, firstInstalled, secondAccepted, bothInstalled, sameObjectWaiting, secondDone, firstDone])
  assertEqual "independent completion preserves the whole-Herald invariants" (Right ()) (validateHeraldState firstDone)

labelRetirementReady :: HeraldState -> ControlIndex
labelRetirementReady = oracleProgressLabelsThrough . OracleClient.oracleClientProgressReady . startupOracleClientState

caseSourceDrainForeignWorkflow :: Assertion
caseSourceDrainForeignWorkflow = do
  (fixture, draining, active, original) <- sourceDrainingFixture
  remoteBootstrap <-
    maybe
      (assertFailure "foreign workflow has no remote bootstrap")
      pure
      ( find
          ((/= Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis) . ProcessBootstrap.appliedProcessResidence)
          (Genesis.checkedInitialBootstraps (startupInitialBootstraps draining))
      )
  root <- case [root | root <- ProcessBootstrap.appliedProcessRoots remoteBootstrap, ProcessBootstrap.ReaderRoot {} <- [ProcessBootstrap.appliedRootRole root]] of
    first : _ -> pure first
    [] -> assertFailure "foreign bootstrap has no checked reader root"
  let remote = ProcessBootstrap.appliedProcessResidence remoteBootstrap
      caller = ProcessBootstrap.appliedProcessEpochId remoteBootstrap
      object = ProcessBootstrap.appliedRootObjectId root
      request = oracleClientRequestId remote 1030
      foreignDecision = deriveLabelDecisionId (Genesis.checkedSystemId fixtureStep14CheckedGenesis) request
  -- A remote private reader need not be present in this Herald's Controlled
  -- owner. Its original (P,0) pair comes from checked primordial evidence;
  -- fresh Delta roots have no earlier label revision or writer authority.
  let foreignCommand =
        decideLabelCommand
          foreignDecision
          caller
          object
          (ProcessLabel caller, 0)
          (Just (initialBootstrapLabelEvidence object caller))
          Nothing
          Nothing
          targetVoid
          (homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition caller) EmptyHeraldPublicationPrefix)
  (foreignOracle, decided, _) <- commitRemoteLabelCommand 30 remote foreignCommand (callerForwardOracle fixture) draining
  assertEqual "foreign decision owns installation evidence" (Just foreignDecision) (fixtureBarrierDecision (startupLabelBarrierState decided))
  assertEqual "foreign decision preserves the local source drain" (Just active) (Application.applicationActiveLabel (startupApplicationState decided))
  assertBool "foreign installation never closes global admission" (not (Application.applicationMembershipGateClosed (startupApplicationState decided)))
  providerWriter <- writerFor Access.SortDefinitionRole (openedAccess (callerForwardSecond fixture))
  let provider = WriteApplication providerWriter (PublishValue (ApplicationValue.SortDefinitionValue (DeclaredSortDefinition heldControlledDescriptor Nothing)))
      matching = ForwardApplication (callerForwardFirstWriter fixture) (asPrivateObjectId (callerForwardFirstObject fixture))
  (withProvider, providerEffects) <- call 40 (callerForwardSecond fixture) 2 provider decided
  (withMatching, matchingEffects) <- call 41 (callerForwardFirst fixture) 4 matching withProvider
  assertBool "an unrelated provider progresses immediately" (any isProviderReply (applicationReplyBodies providerEffects))
  assertEqual "matching future work still waits for the local group" [] (applicationReplyBodies matchingEffects)
  assertEqual "no universal ingress gate is retained" [] (Application.applicationGatedIngressEntries (startupApplicationState withMatching))
  (_, completed, _) <- commitLabelTerminalCollection 51 foreignOracle withMatching
  retained <- drainingWitness active completed
  assertEqual "foreign completion preserves the original local request" (OracleClient.oracleRequestWitnessRef original) (OracleClient.oracleRequestWitnessRef retained)
  assertEqual "local source remains unsent" OracleClient.OracleRequestAwaitingSourceDrain (OracleClient.oracleRequestWitnessStatus retained)
  assertEqual "only matching future work remains held" [matching] (Application.applicationFenceHeldOperation <$> Application.applicationFenceHeldEntries (startupApplicationState completed))
  let key = Application.sequencingSelectionGroup (Application.activeLabelApplicationSelection active)
  (submitted, _) <- completeSourceGroupThroughPeerIngress 70 key completed
  afterDrain <- drainingWitness active submitted
  assertEqual "source completion submits the exact original request" (OracleClient.oracleRequestWitnessEnvelope original) (OracleClient.oracleRequestWitnessEnvelope afterDrain)
  assertEqual "source completion makes the request dispatchable" OracleClient.OracleRequestAwaitingProjection (OracleClient.oracleRequestWitnessStatus afterDrain)
  assertEqual "foreign interleaving preserves invariants" (Right ()) (validateHeraldState submitted)
  where
    isProviderReply (Completed request (WriteCompleted (SortDefinitionWritten _))) = request == requestId 2
    isProviderReply _ = False

caseSourceDrainCallerEnd :: Assertion
caseSourceDrainCallerEnd = do
  (fixture, draining, active, original) <- sourceDrainingFixture
  let caller = requiredProcess (openedSessionId (callerForwardFirst fixture)) (startupApplicationState draining)
      local = Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis
      sourceGroup = Application.sequencingSelectionGroup (Application.activeLabelApplicationSelection active)
      precedingFrontiers = PublicationGroups.groupFrontiers sourceGroup (Publication.publicationGroups (startupPublicationState draining))
  command <- checkedIO "canonical End for the draining caller" (endProcessEpochCommand caller ExplicitAdministrativeEnd)
  (_, ended, endEffects) <- commitRemoteLabelCommand 30 local command (callerForwardOracle fixture) draining
  canonicalEnd <-
    maybe
      (assertFailure "ended caller has no canonical End projection")
      pure
      (OracleProjection.oracleViewEndedProcess caller (OracleProjection.oracleView (startupOracleProjectionState ended)))
  retained <- drainingWitness active ended
  assertBool "End permanently retires the caller" (Controlled.controlledProcessEnded caller (startupControlledState ended))
  assertEqual
    "canceled Open retains the exact canonical End coordinate"
    (OracleClient.OracleRequestEndedBeforeSubmission (OracleProjection.projectedEndedProcessControlIndex canonicalEnd))
    (OracleClient.oracleRequestWitnessStatus retained)
  assertEqual "End preserves the unsent request's original bytes" (OracleClient.oracleRequestWitnessEnvelope original) (OracleClient.oracleRequestWitnessEnvelope retained)
  assertEqual
    "End retains every preceding peer publication obligation"
    precedingFrontiers
    (PublicationGroups.groupFrontiers sourceGroup (Publication.publicationGroups (startupPublicationState ended)))
  assertEqual
    "End cannot fabricate source completion"
    False
    (PublicationGroups.groupReady sourceGroup (Publication.publicationGroups (startupPublicationState ended)))
  assertEqual "provably unsent label no longer owns the application" Nothing (Application.applicationActiveLabel (startupApplicationState ended))
  assertEqual
    "canonical End detaches the dead application session"
    Nothing
    (Application.applicationSessionProcess (openedSessionId (callerForwardFirst fixture)) (startupApplicationState ended))
  assertEqual "unsent cancellation sends no reply to the ended application" [] (applicationReplyBodies endEffects)
  assertEqual "unsent label never acquired a distributed barrier" Nothing (fixtureBarrierDecision (startupLabelBarrierState ended))
  assertEqual "unsent End emits no late Open" [] (OracleClient.oracleClientLabelRequestActions (startupOracleClientState ended))
  assertEqual
    "the canceled Open did not create a projected workflow"
    Nothing
    (OracleProjection.oracleViewLabelWorkflow (Application.activeLabelApplicationDecision active) (OracleProjection.oracleView (startupOracleProjectionState ended)))
  assertBool
    "the unsent request cannot re-enter dispatch"
    (all ((/= OracleClient.oracleRequestWitnessRef original) . oracleRequestDispatchRef) (OracleClient.oracleClientRequestDispatches (startupOracleClientState ended)))
  assertEqual "unsent caller End preserves whole-Herald invariants" (Right ()) (validateHeraldState ended)

-- Mix the three distinct lifetime owners at one Herald: an unsent source
-- drain, an immutable decision whose caller ends, and another caller's submitted
-- request. End may cancel only the provably unsent invocation.
caseConcurrentLabelsCallerEnd :: Assertion
caseConcurrentLabelsCallerEnd = do
  (fixture, draining, unsent, original) <- sourceDrainingFixture
  let first = callerForwardFirst fixture
      second = callerForwardSecond fixture
      caller = requiredProcess (openedSessionId first) (startupApplicationState draining)
      local = Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis
      unsentDecision = Application.activeLabelApplicationDecision unsent
      sourceGroup = Application.sequencingSelectionGroup (Application.activeLabelApplicationSelection unsent)
      frontiers = PublicationGroups.groupFrontiers sourceGroup (Publication.publicationGroups (startupPublicationState draining))
  firstReader <- readerFor Access.DeltaRole (openedAccess first)
  secondReader <- readerFor Access.DeltaRole (openedAccess second)
  let operation reader opened = LabelApplication (asPrivateObjectId (privateDeltaUniqueId reader)) (openedProcessLabel opened) (LabelToProcess (startupAccessProcess (openedAccess opened)))
  (firstAccepted, _) <- call 21 first 4 (operation firstReader first) draining
  firstActive <- case filter ((/= unsentDecision) . Application.activeLabelApplicationDecision) (Application.applicationActiveLabels (startupApplicationState firstAccepted)) of
    [active] -> pure active
    active -> assertFailure ("expected one independent decision, got " <> show (length active))
  let decided = Application.activeLabelApplicationDecision firstActive
  (firstOracle, installed, _) <- commitNextLabelRequest 22 (callerForwardOracle fixture) firstAccepted
  report <- maybe (assertFailure "installed decision has no retained report") pure (Collection.currentReport decided (LabelBarrier.barrierCollectionState (startupLabelBarrierState installed)))
  (bothCallers, _) <- call 23 second 2 (operation secondReader second) installed
  secondActive <- case filter (\active -> Application.activeLabelApplicationDecision active `notElem` [unsentDecision, decided]) (Application.applicationActiveLabels (startupApplicationState bothCallers)) of
    [active] -> pure active
    active -> assertFailure ("expected another caller's label, got " <> show (length active))
  secondRequest <- drainingWitness secondActive bothCallers
  assertEqual "other caller's independent request was submitted" OracleClient.OracleRequestAwaitingProjection (OracleClient.oracleRequestWitnessStatus secondRequest)
  command <- checkedIO "End caller with mixed label lifetimes" (endProcessEpochCommand caller ExplicitAdministrativeEnd)
  (endedOracle, ended, endEffects) <- commitRemoteLabelCommand 24 local command firstOracle bothCallers
  canonicalEnd <- maybe (assertFailure "canonical End missing") pure (OracleProjection.oracleViewEndedProcess caller (OracleProjection.oracleView (startupOracleProjectionState ended)))
  endedUnsent <- drainingWitness unsent ended
  assertEqual "only the provably unsent invocation is ended locally" (OracleClient.OracleRequestEndedBeforeSubmission (OracleProjection.projectedEndedProcessControlIndex canonicalEnd)) (OracleClient.oracleRequestWitnessStatus endedUnsent)
  assertEqual "unsent cancellation retains original bytes" (OracleClient.oracleRequestWitnessEnvelope original) (OracleClient.oracleRequestWitnessEnvelope endedUnsent)
  assertEqual "source drain obligations survive End" frontiers (PublicationGroups.groupFrontiers sourceGroup (Publication.publicationGroups (startupPublicationState ended)))
  assertEqual "unsent invocation releases its object index" Nothing (Application.applicationActiveLabelForDecision unsentDecision (startupApplicationState ended))
  assertEqual "submitted decision and unrelated caller both remain active" (Set.fromList [decided, Application.activeLabelApplicationDecision secondActive]) (Set.fromList (Application.activeLabelApplicationDecision <$> Application.applicationActiveLabels (startupApplicationState ended)))
  assertEqual "End retains the immutable historical installation report" (Just report) (Collection.currentReport decided (LabelBarrier.barrierCollectionState (startupLabelBarrierState ended)))
  afterEndSecond <- drainingWitness secondActive ended
  assertEqual "another caller retains its exact request and status" secondRequest afterEndSecond
  assertEqual "End detaches the dead caller's application session" Nothing (Application.applicationSessionProcess (openedSessionId first) (startupApplicationState ended))
  assertEqual "End produces no result for the dead caller" [] (applicationReplyBodies endEffects)
  (completedOracle, completed, completionEffects) <- commitLabelTerminalCollectionFor decided 25 endedOracle ended
  assertEqual "collection continues after caller End without replying to its dead session" [] (concatMap applicationReplyBodies completionEffects)
  assertEqual "only the unrelated caller remains after collection" [secondActive] (Application.applicationActiveLabels (startupApplicationState completed))
  (secondOracle, secondInstalled, _) <- commitNextLabelRequest 30 completedOracle completed
  (_, allCompleted, secondEffects) <- commitLabelTerminalCollectionFor (Application.activeLabelApplicationDecision secondActive) 31 secondOracle secondInstalled
  assertBool "unrelated live caller receives its successful result" (Completed (requestId 2) (LabelCompleted LabelApplied) `elem` concatMap applicationReplyBodies secondEffects)
  assertEqual "all three invocations leave the active owner" [] (Application.applicationActiveLabels (startupApplicationState allCompleted))
  assertBool "source indexes survive every lifetime transition" (all (Application.applicationLabelIndexesValid . startupApplicationState) [bothCallers, ended, completed, allCompleted])
  assertEqual "mixed caller lifetimes preserve whole-Herald invariants" (Right ()) (validateHeraldState allCompleted)

-- Exercise the complete evidence path: application acceptance and actual
-- assignment, a canonical End overtaking peer delivery, receiver settlement,
-- and that receiver's emitted completion returning to the original source.
-- No synthetic structural report or peer receipt establishes this prefix.
caseSourceGroupDelayedDeliveryAfterEnd :: Assertion
caseSourceGroupDelayedDeliveryAfterEnd = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  writer <- writerFor Access.NeutralVertexRole (openedAccess opened)
  (reserved, reservedEffects) <- call 3 opened 1 (NewIdApplication (ControlledNewId writer)) openedState
  privateObject <- completedNewId 1 reservedEffects
  (assigned, writeEffects) <-
    call
      4
      opened
      2
      (WriteApplication writer (PublishValue (neutralValue privateObject (startupAccessProcess (openedAccess opened)))))
      reserved
  assertEqual
    "the source publication is genuinely accepted before structural stabilization"
    [OperationAccepted (requestId 2) StructuralStabilizationPending]
    (applicationReplyBodies writeEffects)
  let caller = requiredProcess (openedSessionId opened) (startupApplicationState assigned)
      local = Genesis.checkedLocalHeraldEpoch (startupGenesis assigned)
  object <- checkedIO "source group's accepted object" (Application.resolveApplicationPrivateUniqueId caller privateObject (startupApplicationState assigned))
  let key = PublicationGroups.groupKey caller (globalObjectIdFromGlobalUniqueId object)
      groups = Publication.publicationGroups (startupPublicationState assigned)
      frontiers = Map.toAscList (PublicationGroups.groupFrontiers key groups)
  publication <- case Publication.applicationPublicationEntries (startupPublicationState assigned) of
    [(_, record)] -> pure (Publication.applicationPublicationId record)
    other -> assertFailure ("expected one actual accepted publication, got " <> show (length other))
  assertEqual "the accepted publication has completed its local transaction" 0 (PublicationGroups.groupLocalPendingCount key groups)
  assertBool "actual peer assignments retain the source group" (not (null frontiers) && not (PublicationGroups.groupReady key groups))
  deliveries <- forM frontiers $ \(peer, frontier) -> do
    direction <- checkedIO "source's assigned direction" (Stream.mkStreamDirection local peer)
    retained <- checkedIO "source's actual outbox" (PeerStream.activeOutgoingItems direction (startupPeerStreamState assigned))
    let items = filter ((<= frontier) . Stream.sequencedItemSequence) retained
    assertBool "the group frontier names actual immutable outbox items" (not (null items))
    assertBool
      "the group's publication is really present in this destination's outbox"
      (any (\item -> case Stream.sequencedItemPayload item of PeerLogicalPublication value -> publicationBatchId (peerPublicationBatch value) == publication; _ -> False) items)
    pure (peer, direction, frontier, items)
  command <- checkedIO "End the caller after publication assignment" (endProcessEpochCommand caller ExplicitAdministrativeEnd)
  (_, ended, _, endEntry) <- commitRemoteLabelCommandWithEntry 30 local command oracle assigned
  end <- maybe (assertFailure "source has no canonical caller End") pure (OracleProjection.oracleViewEndedProcess caller (OracleProjection.oracleView (startupOracleProjectionState ended)))
  assertEqual "the delayed receiver gets the original emitted End" (OracleProjection.projectedEndedProcessControlIndex end) (OracleEvents.appliedEntryControlIndex (canonicalAppliedOracleEntryValue endEntry))
  assertEqual "End preserves every undelivered source obligation" frontiers (Map.toAscList (PublicationGroups.groupFrontiers key (Publication.publicationGroups (startupPublicationState ended))))
  completed <- foldM (deliverAfterEnd publication caller local endEntry) ended (zip [100, 200 ..] deliveries)
  assertBool "only real receiver settlement closes the original source group" (PublicationGroups.groupReady key (Publication.publicationGroups (startupPublicationState completed)))
  assertEqual "real returned completions retire all group frontiers" Map.empty (PublicationGroups.groupFrontiers key (Publication.publicationGroups (startupPublicationState completed)))
  assertEqual "the original source remains valid after real delayed completion" (Right ()) (validateHeraldState completed)
  where
    deliverAfterEnd publication caller local endEntry source (observed, (peer, direction, frontier, items)) = do
      receiver <- initializeDelayedPublicationReceiver peer (startupInitialBootstraps source)
      oracleBinding <- maybe (assertFailure "receiver has no Oracle binding") pure (OracleClient.oracleClientCurrentBinding (startupOracleClientState receiver))
      (receiverEnded, _) <- step 2 (OracleInput (OracleEntriesReceived oracleBinding (endEntry :| []))) receiver
      assertBool "the receiver knows End before seeing the source publication" (Controlled.controlledProcessEnded caller (startupControlledState receiverEnded))
      assertEqual "the delayed publication is not already present at the receiver" Nothing (Publication.lookupIncomingPublication publication (startupPublicationState receiverEnded))
      (receiverConnected, receiverBinding) <- connectFixturePeer 3 local receiverEnded
      (sourceConnected, sourceBinding) <- connectFixturePeer observed peer source
      (received, effects) <-
        foldM
          ( \(current, batches) (offset, item) -> do
              (successor, batch) <- step offset (PeerInput (peerPublicationReceived receiverBinding (peerLogicalItem item))) current
              assertEqual "legal delayed delivery never rejects the source binding" [] [binding | RejectPeerConnection binding _ <- effectBatchMembers batch]
              pure (successor, batches <> [batch])
          )
          (receiverConnected, [])
          (zip [4 ..] items)
      record <- maybe (assertFailure "receiver lost the actual source publication") pure (Publication.lookupIncomingPublication publication (startupPublicationState received))
      assertBool
        "the accepted source publication settles semantically after End"
        (Publication.incomingPublicationDisposition record `elem` [Publication.Applied, Publication.TerminallyIgnored])
      assertBool
        "the receiver has no pending or rejected destination hidden behind completion"
        (all (/= Publication.DestinationPending) (Map.elems (Publication.incomingPublicationDestinationOutcomes record)))
      watermarks <- checkedIO "receiver's checked completed prefix" (PeerStream.incomingWatermarks direction (startupPeerStreamState received))
      assertBool "receiver completion covers the actual source-group frontier" (Stream.streamCompletedPrefix watermarks >= Stream.streamPrefixThrough frontier)
      let acknowledgements =
            [ control
            | batch <- effects,
              SendPeerControl binding control@(PeerStreamCompleted actualDirection _) <- effectBatchMembers batch,
              binding == receiverBinding,
              actualDirection == direction
            ]
      assertBool "receiver emitted real cumulative completion evidence" (not (null acknowledgements))
      assertEqual "the receiver remains valid after delayed delivery" (Right ()) (validateHeraldState received)
      foldM
        (\current (offset, control) -> fst <$> step offset (PeerInput (PeerControlReceived sourceBinding control)) current)
        sourceConnected
        (zip [observed + 1 ..] acknowledgements)

initializeDelayedPublicationReceiver :: HeraldEpoch -> Genesis.CheckedInitialBootstraps -> IO HeraldState
initializeDelayedPublicationReceiver peer sourceBootstraps = do
  member <- maybe (assertFailure "delayed receiver is not a genesis member") pure (find ((== peer) . heraldMemberEpoch) (Genesis.checkedActiveHeralds fixtureStep14CheckedGenesis))
  let manifest = MembershipFixtures.fixtureDeploymentAt member
      members = Genesis.checkedActiveHeralds fixtureStep14CheckedGenesis
      oracleManifest = manifest.deploymentOracleGenesis
  genesis <-
    checkedIO
      "receiver's own checked genesis"
      (Genesis.checkHeraldGenesis (manifest {Genesis.deploymentActiveHeralds = members, Genesis.deploymentOracleGenesis = oracleManifest {Genesis.oracleGenesisActiveHeralds = members}}))
  let bootstraps = MembershipFixtures.fixtureCheckedInitialBootstrapsFor genesis
      seed = if peer == heraldMemberEpoch MembershipFixtures.fixtureRemoteMember then MembershipFixtures.fixtureGeneratorSeedH2 else MembershipFixtures.fixtureGeneratorSeedH3
  assertEqual "source and receiver agree on immutable bootstrap projection" (Genesis.checkedInitialProjectionDigest sourceBootstraps) (Genesis.checkedInitialProjectionDigest bootstraps)
  (initial, effects) <- checkedIO "initialize actual receiving Herald" (initialHerald (monotonicInstant 0) genesis bootstraps fixtureOracleContacts seed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)
  attempt <- case [candidate | RunOracleClientAction (ConnectAndHelloOracle candidate _) <- effectBatchMembers effects] of
    [candidate] -> pure candidate
    other -> assertFailure ("receiver has no unique Oracle connect, got " <> show (length other))
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
  fst <$> step 1 (OracleInput (OracleHelloReceived attempt (oracleHelloAcceptance node (oracleObservedTerm 1) (controlIndex 0) (Just node) True))) initial

drainingWitness :: Application.ActiveLabelApplicationView -> HeraldState -> IO OracleClient.OracleRequestWitness
drainingWitness active state =
  maybe
    (assertFailure "accepted source-draining Open lost its Oracle witness")
    pure
    ( OracleClient.lookupOracleRequestBySemanticKey
        (Application.applicationRequestKeyCanonicalBytes (Application.activeLabelApplicationKey active))
        (startupOracleClientState state)
    )

completeSourceGroupThroughPeerIngress :: Word -> PublicationGroups.GroupKey -> HeraldState -> IO (HeraldState, [EffectBatch])
completeSourceGroupThroughPeerIngress first key initial = do
  let frontiers = Map.toAscList (PublicationGroups.groupFrontiers key (Publication.publicationGroups (startupPublicationState initial)))
  assertBool "source-drain fixture has actual pending peer frontiers" (not (null frontiers))
  foldM complete (initial, []) (zip [first ..] frontiers)
  where
    complete (state, effects) (observed, (peer, sequenceNumber)) = do
      (connected, binding) <- connectFixturePeer observed peer state
      direction <- checkedIO "source completion direction" (Stream.mkStreamDirection (Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis) peer)
      (successor, batch) <-
        step
          (observed + 1)
          (PeerInput (PeerControlReceived binding (PeerStreamCompleted direction (Stream.streamPrefixCompletion (Stream.streamPrefixThrough sequenceNumber)))))
          connected
      pure (successor, effects <> [batch])

connectFixturePeer :: Word -> HeraldEpoch -> HeraldState -> IO (HeraldState, Discovery.PeerBinding)
connectFixturePeer observed peer state = case DiscoveryState.currentPeerBinding peer (startupDiscoveryState state) of
  Just binding -> pure (state, binding)
  Nothing -> do
    member <-
      maybe
        (assertFailure "fixture completion peer is not a member")
        pure
        (find ((== peer) . heraldMemberEpoch) (Genesis.checkedActiveHeralds fixtureStep14CheckedGenesis))
    let nonce = Discovery.connectionNonce (fromIntegral observed + 1700)
        candidate = Discovery.peerCandidate (heraldMemberId member) peer nonce
        hello =
          Discovery.peerHello
            (Genesis.checkedSystemId fixtureStep14CheckedGenesis)
            (heraldMemberId member)
            peer
            nonce
            Set.empty
            (OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState state)))
            (Genesis.checkedCatalogueDigest fixtureStep14CheckedGenesis)
            (Genesis.checkedInitialProjectionDigest (startupInitialBootstraps state))
            Nothing
    (connected, effects) <- step observed (PeerInput (MembershipFixtures.currentPeerHelloReceived state candidate Set.empty hello)) state
    binding <- case [binding | SetPeerCandidateDisposition candidate' (Discovery.PeerHelloAccepted _ binding) <- effectBatchMembers effects, candidate' == candidate] of
      [binding] -> pure binding
      bindings -> assertFailure ("expected one accepted fixture peer, got " <> show (length bindings))
    pure (connected, binding)

-- Existing immediate-Open fixtures explicitly provide checked completed-prefix
-- evidence for all assigned source publications, preserving peer/group agreement.
completeAssignedPeerPublications :: HeraldState -> IO HeraldState
completeAssignedPeerPublications state = do
  directions <- checkedIO "fixture outgoing directions" (PeerStream.peerStreamOutgoingDirections (startupPeerStreamState state))
  foldM complete state directions
  where
    complete predecessor direction = do
      items <- checkedIO "fixture outgoing assignments" (PeerStream.activeOutgoingItems direction (startupPeerStreamState predecessor))
      case items of
        [] -> pure predecessor
        _ -> do
          let prefix = Stream.streamPrefixThrough (maximum (Stream.sequencedItemSequence <$> items))
          prepared <- checkedIO "fixture completed-prefix evidence" (PeerStream.prepareCompletedAck direction prefix (startupPeerStreamState predecessor))
          let (stream, _) = PeerStream.commitCompletedAck prepared
              completedPrefix = Stream.streamCompletedPrefix (PeerStream.preparedCompletedAckWatermarks prepared)
              publication = Publication.advancePublicationGroupCompletion (Stream.streamDirectionDestination direction) completedPrefix (startupPublicationState predecessor)
          pure (replaceStartupPublicationState publication (replaceStartupPeerStreamState stream predecessor))

caseGateAndTerminalRedrive :: Assertion
caseGateAndTerminalRedrive = do
  fixture <- heldFixture
  (withUnrelated, unrelatedEffects) <- call 19 (heldOpened fixture) 6 (NewIdApplication BareNewId) (heldState fixture)
  assertBool "unrelated work is admitted while label drains" (not (null (applicationReplyBodies unrelatedEffects)))
  assertEqual "the label creates no universal queued ingress" [] (Application.applicationGatedIngressEntries (startupApplicationState withUnrelated))
  trace <- commitCompleteLabelWorkflowTrace 20 (heldOracle fixture) withUnrelated
  let finalState = workflowCompleted trace
      application = startupApplicationState finalState
  assertEqual "decision installation releases selected held work" [] (Application.applicationFenceHeldEntries application)
  assertEqual "canonical completion retires the active invocation" Nothing (Application.applicationActiveLabel application)
  assertCanonicalTerminalReplies (applicationReplyBodies unrelatedEffects <> concatMap applicationReplyBodies (workflowEffects trace))
  heldViews <- exactlyTwoHeld (heldState fixture)
  mapM_ (assertReleasedPublicationProvenance finalState) heldViews

caseApplicationLabelWorkDisposition :: Assertion
caseApplicationLabelWorkDisposition = do
  fixture <- heldFixture
  void
    ( assertApplicationLabelDisposition
        "nonterminal held work"
        ApplicationLabelWorkUnchanged
        (heldState fixture)
    )
  (oracleBeforeRelease, beforeRelease) <- terminalRedrivePredecessor fixture
  (_, beforeApplication, _) <-
    commitNextLabelRequestBeforeApplicationPass
      35
      oracleBeforeRelease
      beforeRelease
  let pendingApplication = startupApplicationState beforeApplication
  assertBool
    "terminal projection leaves held work for the application fallout pass"
    (not (null (Application.applicationFenceHeldEntries pendingApplication)))
  assertBool
    "terminal projection leaves gated ingress for the application fallout pass"
    (not (null (Application.applicationGatedIngressEntries pendingApplication)))
  afterApplication <-
    assertApplicationLabelDisposition
      "terminal held and gated redrive"
      ApplicationLabelWorkChanged
      beforeApplication
  assertEqual
    "the changed pass consumes held work"
    []
    (Application.applicationFenceHeldEntries (startupApplicationState afterApplication))
  assertEqual
    "the changed pass consumes gated ingress"
    []
    (Application.applicationGatedIngressEntries (startupApplicationState afterApplication))
  void
    ( assertApplicationLabelDisposition
        "terminal redrive fixed point"
        ApplicationLabelWorkUnchanged
        afterApplication
    )

caseApplicationLabelCandidateDisposition :: Assertion
caseApplicationLabelCandidateDisposition = do
  activeFixture <- heldFixture
  let blocked =
        retainDetachedLabelCandidate
          (heldOpened activeFixture)
          7
          (heldLabelOperation activeFixture)
          (heldState activeFixture)
  void
    ( assertApplicationLabelDisposition
        "candidate behind a nonterminal active label"
        ApplicationLabelWorkUnchanged
        blocked
    )
  (eligible, opened) <- eligibleCandidateFixture
  assertEqual
    "one eligible detached candidate is retained"
    1
    (length (Application.applicationLabelCandidateEntries (startupApplicationState eligible)))
  promoted <-
    assertApplicationLabelDisposition
      "newly eligible candidate"
      ApplicationLabelWorkChanged
      eligible
  assertBool
    "eligible candidate is promoted"
    ( Application.applicationActiveLabel (startupApplicationState promoted)
        /= Nothing
    )
  let process =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState eligible)
      retired =
        replaceStartupControlledState
          ( Controlled.commitControlledProcessRetirement
              ( Controlled.prepareControlledProcessRetirement
                  process
                  (startupControlledState eligible)
              )
          )
          eligible
  rejected <-
    assertApplicationLabelDisposition
      "candidate rejected after provider loss"
      ApplicationLabelWorkChanged
      retired
  assertEqual
    "rejected candidate is consumed"
    []
    (Application.applicationLabelCandidateEntries (startupApplicationState rejected))

caseExpectedLabelMismatchIsCanonical :: Assertion
caseExpectedLabelMismatchIsCanonical = caseCanonicalLabelMismatch False

caseRetainedExpectedLabelMismatchIsCanonical :: Assertion
caseRetainedExpectedLabelMismatchIsCanonical = caseCanonicalLabelMismatch True

caseCanonicalLabelMismatch :: Bool -> Assertion
caseCanonicalLabelMismatch retained = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  writer <- writerFor Access.NeutralVertexRole (openedAccess opened)
  (reserved, newIdEffects) <- call 3 opened 1 (NewIdApplication (ControlledNewId writer)) openedState
  privateObject <- completedNewId 1 newIdEffects
  let initialValue = neutralValue privateObject (startupAccessProcess (openedAccess opened))
      operation = LabelApplication (asPrivateObjectId privateObject) (ApplicationValue.VoidLabel, 0) LabelToDelete
  (pending, _) <- call 4 opened 2 (WriteApplication writer (PublishValue initialValue)) reserved
  materialized <- settleStructuralPublication pending >>= completeAssignedPeerPublications
  (accepted, effects) <-
    if retained
      then do
        (promoted, replies, _) <- checkedIO "promote stale expected candidate" (advanceApplicationLabelWork (retainDetachedLabelCandidate opened 3 operation materialized))
        (submitted, submission) <- checkedIO "submit original conditional request" (advanceLocalLabelWork promoted)
        pure (submitted, replies <> submission)
      else call 5 opened 3 operation materialized
  assertEqual "a local mismatch remains a conditional request" [OperationAccepted (requestId 3) LabelSettlementPending] (applicationReplyBodies effects)
  active <- maybe (assertFailure "canonical mismatch fixture lost its accepted label") pure (Application.applicationActiveLabel (startupApplicationState accepted))
  original <- drainingWitness active accepted
  (decidedOracle, completed, outcome) <- commitNextLabelRequest 6 oracle accepted
  assertEqual "only the canonical decision returns NotApplied" [Completed (requestId 3) (LabelCompleted LabelNotApplied)] (applicationReplyBodies outcome)
  assertEqual "failed CAS owns no installation slot" [] (oracleOpenDecisions decidedOracle)
  assertEqual "failed CAS creates no collector" [] (Collection.reports (LabelBarrier.barrierCollectionState (startupLabelBarrierState completed)))
  assertEqual "failure retires the local invocation" Nothing (Application.applicationActiveLabel (startupApplicationState completed))
  assertEqual
    "NotApplied becomes retirable after its local application result settles"
    (OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState completed)))
    (labelRetirementReady completed)
  assertBool "failure preserves controlled state" (startupControlledState materialized == startupControlledState completed)
  assertEqual "canonical NotApplied releases the settled Oracle request" Nothing (OracleClient.lookupOracleRequest (OracleClient.oracleRequestWitnessRef original) (startupOracleClientState completed))
  assertEqual "canonical NotApplied releases the semantic request key" Nothing (OracleClient.lookupOracleRequestBySemanticKey (OracleClient.oracleRequestWitnessSemanticKey original) (startupOracleClientState completed))
  (retried, retryEffects) <- call 7 opened 3 operation completed
  assertEqual "exact retry retains canonical failure" (applicationReplyBodies outcome) (applicationReplyBodies retryEffects)
  assertEqual "exact retry creates no new Oracle request" (startupOracleClientState completed) (startupOracleClientState retried)
  assertEqual "exact retry of reclaimed NotApplied submits nothing" [] [dispatch | RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers retryEffects]

assertApplicationLabelDisposition ::
  String ->
  ApplicationLabelWorkDisposition ->
  HeraldState ->
  IO HeraldState
assertApplicationLabelDisposition context expected predecessor = do
  (successor, _, disposition) <-
    checkedIO context (advanceApplicationLabelWork predecessor)
  assertEqual (context <> ": disposition") expected disposition
  assertEqual
    (context <> ": exact whole-state correspondence")
    (successor /= predecessor)
    (disposition == ApplicationLabelWorkChanged)
  pure successor

caseDeleteRejectsFenceHeldPublications :: Assertion
caseDeleteRejectsFenceHeldPublications = do
  fixture <- heldFixtureFor LabelToDelete
  heldViews <- exactlyTwoHeld (heldState fixture)
  target <- case heldViews of
    [_, forwardView] -> case heldOrigin forwardView of
      Publication.ForwardApplicationPublicationOrigin object _ -> pure object
      other -> assertFailure ("held forward has the wrong origin: " <> show other)
    _ -> assertFailure "expected exactly two held publications"
  let process =
        requiredProcess
          (openedSessionId (heldOpened fixture))
          (startupApplicationState (heldState fixture))
  trace <- commitCompleteLabelWorkflowTrace 60 (heldOracle fixture) (heldState fixture)
  let completed = workflowCompleted trace
      deletionEffects = workflowEffects trace
  assertEqual
    "delete consumes both held semantic entries"
    []
    (Application.applicationFenceHeldEntries (startupApplicationState completed))
  mapM_
    ( \view ->
        assertEqual
          "delete publishes no retained fence-held value"
          Nothing
          ( Publication.lookupApplicationPublication
              (Application.applicationFenceHeldPosition view)
              (startupPublicationState completed)
          )
    )
    heldViews
  assertEqual
    "delete suppresses the target's retained possession evidence"
    Nothing
    ( Controlled.controlledEffectivePossessionStrength
        process
        target
        (startupControlledState completed)
    )
  case concatMap applicationReplyBodies deletionEffects of
    [ Rejected writeRequest ApplicationOperateNotPermitted,
      Rejected forwardRequest ApplicationOperateNotPermitted,
      Completed labelRequest (LabelCompleted _)
      ] -> do
        assertEqual "held update rejection request" (requestId 4) writeRequest
        assertEqual "held forward rejection request" (requestId 5) forwardRequest
        assertEqual "delete Label completion request" (requestId 3) labelRequest
    replies -> assertFailure ("unexpected delete-redrive replies: " <> show replies)

caseVoidDynamicNablaRejectsOwnHeldWrite :: Assertion
caseVoidDynamicNablaRejectsOwnHeldWrite = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  sortWriter <- writerFor Access.SortDefinitionRole (openedAccess opened)
  (pendingSort, sortEffects) <-
    call
      70
      opened
      1
      ( WriteApplication
          sortWriter
          ( PublishValue
              ( ApplicationValue.SortDefinitionValue
                  (DeclaredSortDefinition heldWriteDescriptor Nothing)
              )
          )
      )
      openedState
  writtenSort <- completedSortDefinition 1 sortEffects
  carrierWriter <- writerFor Access.NablaRole (openedAccess opened)
  (reservedNabla, nablaIdEffects) <-
    call
      71
      opened
      2
      (NewIdApplication (ControlledNewId carrierWriter))
      pendingSort
  privateNablaIdentity <- completedNewId 2 nablaIdEffects
  let privateProcess = startupAccessProcess (openedAccess opened)
      privateNabla = asPrivateNablaId privateNablaIdentity
      carrier =
        selfSequencingDynamicNablaValue
          privateNablaIdentity
          privateProcess
          writtenSort
  (pendingInstallation, _) <-
    call
      72
      opened
      3
      (WriteApplication carrierWriter (PublishValue carrier))
      reservedNabla
  installed <- settleStructuralPublication pendingInstallation
  let labelOperation =
        LabelApplication
          (asPrivateObjectId privateNablaIdentity)
          (openedProcessLabel opened)
          LabelToVoid
      heldOperation =
        WriteApplication
          privateNabla
          ( PublishValue
              ( ApplicationValue.RecordValue
                  (Map.singleton "message" (ApplicationValue.TextValue "held"))
              )
          )
  (published, publishedEffects) <- call 73 opened 700 heldOperation installed
  assertEqual
    "an ordinary write before the label is accepted through the sequenced writer"
    [Completed (requestId 700) (WriteCompleted WriteAccepted)]
    (applicationReplyBodies publishedEffects)
  let beforePublications =
        Set.fromList
          (Publication.applicationPublicationId . snd <$> Publication.applicationPublicationEntries (startupPublicationState installed))
  ordinaryRecord <- case [ record
                         | (_, record) <- Publication.applicationPublicationEntries (startupPublicationState published),
                           Publication.applicationPublicationId record `Set.notMember` beforePublications
                         ] of
    [record] -> pure record
    records -> assertFailure ("expected one accepted ordinary publication, got " <> show (length records))
  (retriedPublication, _) <- call 73 opened 700 heldOperation published
  assertEqual
    "retrying the ordinary write retains the exact group obligations"
    (Publication.publicationGroups (startupPublicationState published))
    (Publication.publicationGroups (startupPublicationState retriedPublication))
  sourceCompleted <- completeAssignedPeerPublications retriedPublication
  (accepted, _) <- call 73 opened 4 labelOperation sourceCompleted
  (held, heldEffects) <- call 74 opened 5 heldOperation accepted
  assertEqual
    "the source-labelled write waits without an early reply"
    []
    (applicationReplyBodies heldEffects)
  assertEqual "endpoint handoff holds unaccepted raw ingress" [heldOperation] (Application.applicationGatedIngressOperation <$> Application.applicationGatedIngressEntries (startupApplicationState held))
  assertEqual "endpoint hold acquires no process position" (Application.applicationRequestNextAcceptancePositions (Application.applicationRequestStateWitness (startupApplicationState accepted))) (Application.applicationRequestNextAcceptancePositions (Application.applicationRequestStateWitness (startupApplicationState held)))
  let localProcess =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState held)
  globalIdentity <-
    checkedIO
      "resolve the source-labelled Nabla identity"
      ( Application.resolveApplicationPrivateUniqueId
          localProcess
          privateNablaIdentity
          (startupApplicationState held)
      )
  let globalNabla =
        nablaIdFromGlobalObjectId
          (globalObjectIdFromGlobalUniqueId globalIdentity)
  assertEqual "the held endpoint is the labelled Nabla" (Just (activeDecision held, globalNabla)) (Application.applicationEndpointAdmissionHold (startupApplicationState held))
  let ordinaryGroups = Publication.publicationGroups (startupPublicationState published)
      sequencingGroup = PublicationGroups.groupKey localProcess (globalObjectIdFromNablaId globalNabla)
  assertEqual
    "a regular publication belongs to its caller's sequencing group without a self-object group"
    (Set.singleton sequencingGroup)
    (PublicationGroups.membershipKeys (Publication.applicationPublicationGroupMembership ordinaryRecord))
  assertEqual
    "ordinary publication has no structural stage"
    Nothing
    (Publication.applicationPublicationUnsequencedStage ordinaryRecord)
  assertEqual
    "the ordinary fixture exercises settlement without destination assignments"
    []
    (routeDestinations (Publication.applicationPublicationRoute ordinaryRecord))
  assertBool
    "local ordinary processing retires its pending publication token even without a remote route"
    (not (PublicationGroups.publicationPending (Publication.applicationPublicationId ordinaryRecord) ordinaryGroups))
  assertEqual
    "completed local ordinary work leaves no local group dependency"
    0
    (PublicationGroups.groupLocalPendingCount sequencingGroup ordinaryGroups)
  checkedWriter <-
    checkedIO
      "capture the dynamic Nabla sequencing association"
      ( ControlledOperate.checkControlledOperate
          localProcess
          globalNabla
          (startupControlledState installed)
          (startupStructuralProgressState installed)
      )
  assertEqual
    "accepted dynamic authority retains its explicit self-sequencing object"
    (Just (globalObjectIdFromNablaId globalNabla))
    (ControlledOperate.controlledOperateSequencingObject checkedWriter)
  trace <- commitCompleteLabelWorkflowTrace 75 oracle held
  let completed = workflowCompleted trace
      endpointEffects = workflowEffects trace
  assertEqual "void installation consumes raw endpoint ingress" [] (Application.applicationGatedIngressEntries (startupApplicationState completed))
  assertEqual "void installation cannot create a publication for the queued call" (Publication.applicationPublicationEntries (startupPublicationState held)) (Publication.applicationPublicationEntries (startupPublicationState completed))
  case concatMap applicationReplyBodies endpointEffects of
    [ Rejected writeRequest ApplicationOperateNotPermitted,
      Completed labelRequest (LabelCompleted _)
      ] -> do
        assertEqual "held source rejection request" (requestId 5) writeRequest
        assertEqual "void Label completion request" (requestId 4) labelRequest
    replies -> assertFailure ("unexpected void-source redrive replies: " <> show replies)

caseUnroutedControlledDeleteInheritedByFirstStore :: Assertion
caseUnroutedControlledDeleteInheritedByFirstStore = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  let process =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      privateProcess = startupAccessProcess (openedAccess opened)

  sortWriter <- writerFor Access.SortDefinitionRole (openedAccess opened)
  (pendingSort, sortEffects) <-
    call
      70
      opened
      1
      ( WriteApplication
          sortWriter
          ( PublishValue
              ( ApplicationValue.SortDefinitionValue
                  (DeclaredSortDefinition unroutedControlledDescriptor Nothing)
              )
          )
      )
      openedState
  writtenSort <- completedSortDefinition 1 sortEffects

  nablaCarrierWriter <- writerFor Access.NablaRole (openedAccess opened)
  (reservedNabla, nablaIdEffects) <-
    call
      71
      opened
      2
      (NewIdApplication (ControlledNewId nablaCarrierWriter))
      pendingSort
  privateNablaIdentity <- completedNewId 2 nablaIdEffects
  let privateNabla = asPrivateNablaId privateNablaIdentity
      nablaValue =
        dynamicNablaValueSequencedBy
          privateNablaIdentity
          privateProcess
          writtenSort
          Nothing
  (pendingNabla, _) <-
    call
      72
      opened
      3
      (WriteApplication nablaCarrierWriter (PublishValue nablaValue))
      reservedNabla
  writerReady <- settleStructuralPublication pendingNabla
  assertEqual
    "the application-defined controlled sort begins without a Store"
    []
    (matchingStoreSlots writtenSort (startupStoreState writerReady))

  (reservedObject, objectIdEffects) <-
    call
      73
      opened
      4
      (NewIdApplication (ControlledNewId privateNabla))
      writerReady
  privateObject <- completedNewId 4 objectIdEffects
  let value = neutralValue privateObject privateProcess
  (published, _) <-
    call
      74
      opened
      5
      (WriteApplication privateNabla (PublishValue value))
      reservedObject
  globalIdentity <-
    checkedIO
      "resolve unrouted controlled identity"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateObject
          (startupApplicationState published)
      )
  let object = globalObjectIdFromGlobalUniqueId globalIdentity
  controlledRecord <-
    maybe
      (assertFailure "unrouted controlled publication installed no live record")
      pure
      (Controlled.controlledLocalRecord object (startupControlledState published))
  let publication = Controlled.controlledRecordLatestPublication controlledRecord
      publicationKey = checkedPublicationKey publication
  outgoing <-
    maybe
      (assertFailure "unrouted controlled publication retained no outgoing record")
      pure
      ( Publication.lookupOutgoingPublication
          (checkedPublicationId publication)
          (startupPublicationState published)
      )
  assertEqual "the controlled publication uses the declared sort" writtenSort (checkedPublicationSort publication)
  assertEqual
    "the controlled publication has an empty frozen route"
    []
    (routeDestinations (Publication.outgoingPublicationRoute outgoing))
  assertEqual
    "the empty-route publication creates no Store opportunistically"
    []
    (matchingStoreSlots writtenSort (startupStoreState published))
  assertEqual "the unrouted publication leaves a valid whole Herald" (Right ()) (validateHeraldState published)

  registryEntry <- maybe (assertFailure "unrouted source lost its controlled descriptor") pure (SortRegistry.lookupEffectiveSort writtenSort (startupSortRegistryState published))
  delayedObservation <- checkedIO "retain the actual unrouted source publication" (Controlled.checkControlledObservation (SortRegistry.registryEntryDescriptor registryEntry) (Publication.outgoingPublicationOccurrenceId outgoing) publication)

  -- A separate legitimate first-label branch retains the same empty source
  -- route. No publication or possession is fabricated at the receiving Herald.
  (acceptedVoid, _) <- call 75 opened 6 (LabelApplication (asPrivateObjectId privateObject) (openedProcessLabel opened) LabelToVoid) published
  (readyVoid, _) <- call 77 opened 6 (LabelApplication (asPrivateObjectId privateObject) (openedProcessLabel opened) LabelToVoid) acceptedVoid
  _ <- commitLabelWorkflowWithAbsentReceiver False object delayedObservation 78 oracle readyVoid

  let deleteOperation =
        LabelApplication
          (asPrivateObjectId privateObject)
          (openedProcessLabel opened)
          LabelToDelete
  (acceptedDelete, _) <- call 75 opened 6 deleteOperation published
  (readyDelete, _) <- call 77 opened 6 deleteOperation acceptedDelete
  workflow <- commitLabelWorkflowWithAbsentReceiver True object delayedObservation 78 oracle readyDelete
  let deleted = workflowCompleted workflow
  deletion <-
    maybe
      (assertFailure "live ordinary controlled delete installed no tombstone")
      pure
      (Controlled.controlledTerminalDeletion object (startupControlledState deleted))
  let cause = Controlled.controlledTerminalDeletionCause deletion
  assertEqual
    "the live delete still has no matching Store to purge"
    []
    (matchingStoreSlots writtenSort (startupStoreState deleted))
  assertEqual
    "the zero-slot delete retains its authoritative Store suppression"
    [((writtenSort, publicationKey), cause)]
    (Store.storeTerminalObjectPurgeEntries (startupStoreState deleted))
  assertEqual "the zero-slot deletion leaves a valid whole Herald" (Right ()) (validateHeraldState deleted)

  deltaCarrierWriter <- writerFor Access.DeltaRole (openedAccess opened)
  (reservedDelta, deltaIdEffects) <-
    call
      100
      opened
      7
      (NewIdApplication (ControlledNewId deltaCarrierWriter))
      deleted
  privateDeltaIdentity <- completedNewId 7 deltaIdEffects
  (pendingDelta, _) <-
    call
      101
      opened
      8
      ( WriteApplication
          deltaCarrierWriter
          ( PublishValue
              (dynamicDeltaValueForSort privateDeltaIdentity privateProcess writtenSort)
          )
      )
      reservedDelta
  withFirstStore <- settleStructuralPublication pendingDelta
  globalDeltaIdentity <-
    checkedIO
      "resolve first same-sort Delta identity"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateDeltaIdentity
          (startupApplicationState withFirstStore)
      )
  let delta =
        deltaIdFromGlobalObjectId
          (globalObjectIdFromGlobalUniqueId globalDeltaIdentity)
      storeState = startupStoreState withFirstStore
  slot <-
    maybe
      (assertFailure "the later same-sort Delta installed no active Store")
      pure
      (Store.lookupStoreSlot delta storeState)
  assertEqual "the later Delta creates the first Store of the sort" [slot] (matchingStoreSlots writtenSort storeState)
  assertEqual
    "the first later Store inherits the exact terminal cause"
    (Just cause)
    (Store.storeSlotTerminalObjectPurgeCause publicationKey slot)
  assertEqual "the inherited suppression leaves a valid whole Herald" (Right ()) (validateHeraldState withFirstStore)

  preparedStale <-
    checkedIO
      "prepare stale observation for the first later Store"
      ( Store.preparePeerStoreApplication
          ( Store.storeObservationEnvelope
              (Publication.outgoingPublicationSourceProcess outgoing)
              (Publication.outgoingPublicationOccurrenceId outgoing)
              (Publication.outgoingPublicationSourceTopologyPrerequisite outgoing)
              (Publication.outgoingPublicationControlPrerequisite outgoing)
              Nothing
          )
          ( Store.peerStoreDestination
              delta
              (Store.storeSlotIncarnation slot)
              Normal
              :| []
          )
          publication
          storeState
      )
  let (afterStale, staleOutcomes) = Store.commitPeerStoreApplication preparedStale
  assertEqual
    "the inherited terminal suppression rejects the stale observation"
    (Store.PeerStoreTerminallyIgnored :| [])
    (Store.peerStoreOutcomeDisposition <$> staleOutcomes)
  assertBool "terminally ignored stale traffic is Store-idempotent" (afterStale == storeState)
  assertEqual
    "the first inherited Store retains valid history"
    (Right ())
    (Store.validateStoreHistoryState afterStale)
  where
    matchingStoreSlots sortId =
      filter ((== sortId) . Store.storeSlotSortId) . Store.retainedStoreSlots

-- Drive one actual remote Herald alongside the source. Its reports use the
-- exact retained client requests, so canonical acknowledgement has real local
-- request/receipt provenance. The other captured reporters use the ordinary
-- source fixture's checked Oracle commands.
commitLabelWorkflowWithAbsentReceiver :: Bool -> GlobalObjectId -> Controlled.CheckedControlledObservation -> Word -> OracleState -> HeraldState -> IO CompleteLabelWorkflowTrace
commitLabelWorkflowWithAbsentReceiver deleted object delayedObservation observed oracle source = do
  peer <- case fixtureRemoteHeralds of
    first : _ -> pure first
    [] -> assertFailure "absent receiver fixture needs another captured Herald"
  initial <- initializeDelayedPublicationReceiver peer (startupInitialBootstraps source)
  let sourceProjection = startupOracleProjectionState source
      sourceIndex = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView sourceProjection)
  prefix <- traverse (\index -> maybe (assertFailure "source canonical history has a gap") pure (OracleProjection.appliedEntryEvidence (controlIndex index) sourceProjection)) [1 .. controlIndexWord64 sourceIndex]
  receiver <- foldM (\state entry -> fst <$> applyAbsentReceiverEntry deleted "initial Open prefix" state entry) initial prefix
  let beforeDecision = source
      receiverBeforeDecision = receiver
  (decidedOracle, decidedSource, decisionEffect, decisionEntry) <- commitNextLabelRequestWithEntry observed oracle source
  (decidedReceiver, _) <- applyAbsentReceiverEntry deleted "source request" receiver decisionEntry
  assertAbsentReceiverCanonicalRelease deleted object delayedObservation decisionEntry decidedSource receiverBeforeDecision decidedReceiver
  report <-
    maybe
      (assertFailure "actual receiver retained no installation fact")
      pure
      (Collection.currentReport (activeDecision source) (LabelBarrier.barrierCollectionState (startupLabelBarrierState decidedReceiver)))
  assertEqual "the real receiver owns its installation report" peer (DomainLabel.labelInstallationReporter report)
  (reportedSource, actualReportEffects) <- deliverInstallationReport (observed + 1) report decidedSource
  assertBool
    "actual receiver's peer report satisfies its collector obligation"
    (Set.notMember peer (Collection.outstanding (activeDecision source) (LabelBarrier.barrierCollectionState (startupLabelBarrierState reportedSource))))
  (completedOracle, completed, completionEffects, completionEntry) <- commitLabelTerminalCollectionForWithEntry (activeDecision source) (observed + 1) decidedOracle reportedSource
  (completedReceiver, _) <- applyAbsentReceiverEntry deleted "canonical collector completion" decidedReceiver completionEntry
  let effects = [decisionEffect, actualReportEffects] <> completionEffects
  assertEqual "the real receiving Herald remains valid through workflow completion" (Right ()) (validateHeraldState completedReceiver)
  pure
    CompleteLabelWorkflowTrace
      { workflowOracle = completedOracle,
        workflowBeforeDecision = beforeDecision,
        workflowDecided = decidedSource,
        workflowCompleted = completed,
        workflowEffects = effects
      }
assertAbsentReceiverCanonicalRelease :: Bool -> GlobalObjectId -> Controlled.CheckedControlledObservation -> CanonicalAppliedOracleEntry -> HeraldState -> HeraldState -> HeraldState -> Assertion
assertAbsentReceiverCanonicalRelease deleted object delayedObservation release source before released = do
  sourceRecord <- maybe (assertFailure "source did not install its canonical release") pure (Controlled.controlledReleasedLabelRecord object (startupControlledState source))
  let projected = startupOracleProjectionState source
      releaseIndex = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView projected)
  assertEqual
    "the supplied source snapshot ends at its actual canonical Release"
    [releaseIndex]
    [index | OracleEvents.LabelDecidedView _ terminal _ <- fmap OracleEvents.oracleProjectionEventView (OracleEvents.appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue release)), LiveReleasedOutcomeView _ _ index _ <- [liveTerminalOutcomeView terminal]]
  assertEqual "canonical preparation did not materialize absent payload" Nothing (Controlled.controlledLocalRecord object (startupControlledState before))
  assertEqual "first absent installation has no previous released record" Nothing (Controlled.controlledReleasedLabelRecord object (startupControlledState before))
  installed <- case [installation | retained <- Map.elems (LabelPatch.retainedLabelPatches (startupLabelPatchState released)), Just installation <- [LabelPatch.retainedPatchViewInstallation retained], LabelPatch.installedLabelObject installation == object] of
    [installation] -> pure installation
    _ -> assertFailure "absent receiver did not retain the atomic installation"
  assertEqual "the receiver installed an absent target" LabelPatch.AbsentOrdinaryTargetView (LabelPatch.targetRoleView (LabelPatch.installedLabelPreparationRole installed))
  assertEqual "atomic installation uses the decision coordinate" releaseIndex (LabelPatch.installedLabelReleaseIndex installed)
  let controlled = startupControlledState released
      expected = releasedLabelStateView (labelRecordReleasedState sourceRecord)
  assertEqual "canonical Release installs the same authoritative outcome at an absent receiver" (Just sourceRecord) (Controlled.controlledReleasedLabelRecord object controlled)
  assertEqual "installation never fabricates the absent source publication" Nothing (Controlled.controlledLocalRecord object controlled)
  assertEqual "terminal suppression follows only a deleted outcome" deleted (Controlled.controlledObjectTerminallyDeleted object controlled)
  assertEqual "absent installation preserves whole-Herald invariants" (Right ()) (validateHeraldState released)
  (replayed, _) <- applyAbsentReceiverEntry deleted "Release replay" released release
  assertBool "canonical replay preserves the exact Controlled owner" (controlled == startupControlledState replayed)
  assertBool "canonical replay allocates no identity" (startupIdGeneratorState released == startupIdGeneratorState replayed)
  -- The immutable publication is checked with its actual source descriptor and
  -- occurrence. This exercises the receiving leaf, without inventing a peer
  -- destination for a publication whose admitted route was empty.
  prepared <- checkedIO "apply the actual delayed publication beneath its canonical overlay" (Controlled.prepareControlledPeerObservation delayedObservation controlled)
  let (observed, disposition) = Controlled.commitControlledPeerObservation prepared
  assertEqual "delayed materialization cannot replace the canonical label" (Just expected) (releasedLabelStateView <$> Controlled.controlledEffectiveLabelState object observed)
  if deleted
    then do
      assertEqual "deleted delayed publication is terminally suppressed" Controlled.ControlledPeerCurrentSuppressed disposition
      assertEqual "terminal suppression creates no payload" Nothing (Controlled.controlledLocalRecord object observed)
    else assertBool "a nonterminal overlay permits later ordinary materialization" (Controlled.controlledLocalRecord object observed /= Nothing)
  repeated <- checkedIO "repeat the actual delayed publication" (Controlled.prepareControlledPeerObservation delayedObservation observed)
  assertBool "delayed-publication replay preserves the receiving leaf" (observed == fst (Controlled.commitControlledPeerObservation repeated))

applyAbsentReceiverEntry :: Bool -> String -> HeraldState -> CanonicalAppliedOracleEntry -> IO (HeraldState, EffectBatch)
applyAbsentReceiverEntry deleted phase state entry = do
  binding <- maybe (assertFailure "absent receiver has no Oracle binding") pure (OracleClient.oracleClientCurrentBinding (startupOracleClientState state))
  let value = canonicalAppliedOracleEntryValue entry
  checkedIO
    ( "absent receiver "
        <> (if deleted then "Delete" else "Void")
        <> " "
        <> phase
        <> " at canonical index "
        <> show (OracleEvents.appliedEntryControlIndex value)
        <> "; events="
        <> show (fmap OracleEvents.oracleProjectionEventView (OracleEvents.appliedEntryProjectionEvents value))
        <> "; prior barrier="
        <> show (LabelBarrier.barrierTerminalEvidences (startupLabelBarrierState state))
    )
    ( verifiedStepHerald
        (heraldInput (startupLastObservedTime state) (OracleInput (OracleEntriesReceived binding (entry :| []))))
        state
    )

caseDetachedReplyLinks :: Assertion
caseDetachedReplyLinks = do
  fixture <- heldFixture
  originalViews <- exactlyTwoHeld (heldState fixture)
  let originalPayloads = heldPayloads originalViews
  (bindingLost, bindingLossEffects) <-
    step
      29
      (RuntimeObserved (ApplicationBindingLost (openedBinding (heldOpened fixture))))
      (heldState fixture)
  assertEqual "binding loss emits no application reply" [] (applicationReplyBodies bindingLossEffects)
  disconnectedViews <- exactlyTwoHeld bindingLost
  assertEqual "binding loss preserves semantic payloads" originalPayloads (heldPayloads disconnectedViews)
  assertBool
    "binding loss immediately detaches each dead reply owner"
    (all (noHeldReply . Application.applicationFenceHeldReplyKey) disconnectedViews)
  mapM_ (assertDetachedRedrive (startupApplicationState bindingLost)) disconnectedViews
  assertEqual
    "binding loss forgets the application session immediately"
    Nothing
    (Application.applicationSessionProcess (openedSessionId (heldOpened fixture)) (startupApplicationState bindingLost))
  (disconnectedOracleAfterOpen, disconnectedAfterOpen, _) <-
    commitNextLabelRequest 30 (heldOracle fixture) bindingLost
  let disconnectedWithEvidence =
        disconnectedAfterOpen
  (disconnectedReady, _) <-
    step
      31
      (RuntimeObserved (ApplicationBindingLost (openedBinding (heldOpened fixture))))
      disconnectedWithEvidence
  (_, disconnectedCompleted, disconnectedEffects) <-
    commitCompleteLabelWorkflow
      32
      disconnectedOracleAfterOpen
      disconnectedReady
  assertEqual
    "disconnected terminal replay emits no direct application reply"
    []
    (concatMap applicationReplyBodies disconnectedEffects)
  (afterRejectedResume, resumeEffects) <-
    step
      43
      ( ApplicationSessionInput
          ( ResumeApplicationSession
              (candidateApplicationLane 2)
              (openedSessionId (heldOpened fixture))
              (openedToken (heldOpened fixture))
              (openedCursor (heldOpened fixture))
          )
      )
      disconnectedCompleted
  assertEqual
    "an expired session cannot recover its result owner"
    [Session.ApplicationSessionNotLive]
    [reason | RejectApplicationConnection _ reason <- effectBatchMembers resumeEffects]
  (_, getEffects) <-
    step
      44
      ( ApplicationRequestInput
          ( GetApplicationRequestResult
              (openedBinding (heldOpened fixture))
              (openedSessionId (heldOpened fixture))
              (requestId 4)
          )
      )
      afterRejectedResume
  assertEqual
    "Get after terminal loss cannot recreate an application receipt"
    [Session.ApplicationSessionNotLive]
    [reason | RejectApplicationConnection _ reason <- effectBatchMembers getEffects]
  assertEqual
    "accepted semantic held work completes after the application is gone"
    []
    (Application.applicationFenceHeldEntries (startupApplicationState disconnectedCompleted))

  (sessionEnded, _) <-
    step
      40
      ( ApplicationSessionInput
          (EndApplicationSession (openedBinding (heldOpened fixture)) (openedSessionId (heldOpened fixture)))
      )
      (heldState fixture)
  sessionViews <- exactlyTwoHeld sessionEnded
  assertEqual "session retirement preserves semantic payloads" originalPayloads (heldPayloads sessionViews)
  assertBool "session retirement detaches every reply key" (all (noHeldReply . Application.applicationFenceHeldReplyKey) sessionViews)
  let application = startupApplicationState (heldState fixture)
      process = requiredProcess (openedSessionId (heldOpened fixture)) application
      (processRetired, _) =
        Application.commitApplicationProcessRetirement
          (Application.prepareApplicationProcessRetirement process application)
      processViews = Application.applicationFenceHeldEntries processRetired
  assertEqual "process retirement preserves semantic payloads" originalPayloads (heldPayloads processViews)
  assertBool "process retirement detaches every reply key" (all (noHeldReply . Application.applicationFenceHeldReplyKey) processViews)
  mapM_ (assertDetachedRedrive processRetired) processViews
  (oracleAfterOpen, afterOpen, _) <-
    commitNextLabelRequest 41 (heldOracle fixture) sessionEnded
  let withProjectedOpen = afterOpen
  (ready, _) <-
    step
      42
      (RuntimeObserved (ApplicationBindingLost (openedBinding (heldOpened fixture))))
      withProjectedOpen
  (_, completed, completionEffects) <-
    commitCompleteLabelWorkflow 43 oracleAfterOpen ready
  assertEqual "detached terminal replay emits no application reply" [] (concatMap applicationReplyBodies completionEffects)
  assertEqual "detached semantic work still executes" [] (Application.applicationFenceHeldEntries (startupApplicationState completed))

caseDecisionDuringNotReadyCancelsRetry :: Assertion
caseDecisionDuringNotReadyCancelsRetry = do
  fixture <- heldFixture
  let beforeDecision = heldState fixture
  (binding, dispatch) <- case OracleClient.oracleClientLabelRequestActions (startupOracleClientState beforeDecision) of
    [SubmitOracleRequest b request] -> pure (b, request)
    actions -> assertFailure ("expected conditional decision request: " <> show actions)
  (waiting, notReadyEffects) <- step 105 (OracleInput (OracleSubmissionNotReadyReceived binding (OracleClient.oracleRequestRefRequestId (oracleRequestDispatchRef dispatch)) (oracleObservedTerm 2))) beforeDecision
  retry <- case [token | RunOracleClientAction (ScheduleOracleRetry OracleSubmissionRetry token) <- effectBatchMembers notReadyEffects] of
    [token] -> pure token
    effects -> assertFailure ("expected one NotReady retry: " <> show effects)
  (_, decided, decisionEffects) <- commitOracleEnvelope 106 binding (canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch)) (heldOracle fixture) waiting
  assertBool "canonical decision cancels the pending submission retry" (CancelOracleRetry retry `elem` [action | RunOracleClientAction action <- effectBatchMembers decisionEffects])
  assertEqual "no per-member prepared command follows the decision" [] (OracleClient.oracleClientLabelRequestActions (startupOracleClientState decided))
  assertBool "installed decision retains its peer collection fact" (Collection.currentReport (heldDecision fixture) (LabelBarrier.barrierCollectionState (startupLabelBarrierState decided)) /= Nothing)
  (afterLateRetry, retryEffects) <- step 107 (OracleInput (OracleRetryElapsed retry)) decided
  assertEqual "cancelled retry preserves Oracle client" (startupOracleClientState decided) (startupOracleClientState afterLateRetry)
  assertEqual "cancelled retry offers no command" [] [action | RunOracleClientAction action@SubmitOracleRequest {} <- effectBatchMembers retryEffects]

caseInstallationCollectionOrdering :: Assertion
caseInstallationCollectionOrdering = do
  fixture <- heldFixture
  let ready = heldState fixture
      decision = heldDecision fixture
  dispatch <- case OracleClient.oracleClientLabelRequestActions (startupOracleClientState ready) of
    [SubmitOracleRequest _ request] -> pure request
    actions -> assertFailure ("expected withheld decision request: " <> show actions)
  reporter <- case fixtureRemoteHeralds of
    first : _ -> pure first
    [] -> assertFailure "ordering fixture needs a remote installer"
  (terminalOracle, _, oracleBatch) <-
    checkedIO
      "commit withheld canonical decision"
      (stepOracle (canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch)) (heldOracle fixture))
  terminal <- maybe (assertFailure "withheld decision has no terminal") pure (oracleTerminalOutcome decision terminalOracle)
  entry <- case oracleEffects oracleBatch of
    [EmitAppliedOracleEntry retained] -> pure retained
    effects -> assertFailure ("withheld decision has unexpected effects: " <> show effects)
  let report = remoteInstallationReport decision terminal reporter
      reportIndex = DomainLabel.labelInstallationControlIndex report
  (early, earlyEffects) <- deliverInstallationReport 52 report ready
  assertEqual
    "future report does not create local installation"
    Nothing
    (Collection.currentReport decision (LabelBarrier.barrierCollectionState (startupLabelBarrierState early)))
  assertEqual
    "future report remains staged until its canonical prerequisite"
    [report]
    (snd (Collection.takeReady reportIndex (LabelBarrier.barrierCollectionState (startupLabelBarrierState early))))
  assertEqual
    "legal early report preserves the peer binding"
    []
    [binding | RejectPeerConnection binding _ <- effectBatchMembers earlyEffects]
  binding <- maybe (assertFailure "ordering fixture lost Oracle binding") pure (OracleClient.oracleClientCurrentBinding (startupOracleClientState early))
  (installed, _) <- step 53 (OracleInput (OracleEntriesReceived binding (canonicalizeAppliedOracleEntry entry :| []))) early
  let installedCollection = LabelBarrier.barrierCollectionState (startupLabelBarrierState installed)
  assertBool "canonical terminal creates the local historical fact" (Collection.currentReport decision installedCollection /= Nothing)
  assertBool "staged peer report is consumed after local installation" (Set.notMember reporter (Collection.outstanding decision installedCollection))
  (duplicated, _) <- deliverInstallationReport 54 report installed
  assertEqual
    "duplicate direct reports do not change collection"
    installedCollection
    (LabelBarrier.barrierCollectionState (startupLabelBarrierState duplicated))
  (completedOracle, completed, _) <- commitLabelTerminalCollection 55 terminalOracle duplicated
  assertEqual
    "canonical completion retires the local historical fact"
    Nothing
    (Collection.currentReport decision (LabelBarrier.barrierCollectionState (startupLabelBarrierState completed)))
  settled <- completeAssignedPeerPublications completed
  let nextOperation = case heldLabelOperation fixture of
        LabelApplication object _ _ -> LabelApplication object (ApplicationValue.VoidLabel, 1) LabelToVoid
        _ -> error "label fixture operation"
  (acceptedNext, _) <- call 60 (heldOpened fixture) 6 nextOperation settled
  nextActive <- maybe (assertFailure "second label was not accepted") pure (Application.applicationActiveLabel (startupApplicationState acceptedNext))
  let nextDecision = Application.activeLabelApplicationDecision nextActive
  (nextOracle, nextOpened, _) <- commitNextLabelRequest 61 completedOracle acceptedNext
  assertBool "the successor owns a different decision" (nextDecision /= decision)
  let claim =
        labelCompletionAttestation
          decision
          reportIndex
          (deriveLabelOutcomeDigest terminal)
          reporter
          (heraldMembershipGenerationId (oracleCurrentMembership nextOracle))
  (_, replayed, replayEffects) <- commitRemoteLabelCommand 62 reporter (completeLabelDecisionCommand claim) nextOracle nextOpened
  assertEqual "old completion replay preserves the newer barrier" (startupLabelBarrierState nextOpened) (startupLabelBarrierState replayed)
  assertEqual "old completion replay preserves the newer application invocation" (Application.applicationActiveLabel (startupApplicationState nextOpened)) (Application.applicationActiveLabel (startupApplicationState replayed))
  assertEqual "old completion replay preserves the newer retirement pin" (labelRetirementReady nextOpened) (labelRetirementReady replayed)
  assertEqual "old completion replay emits no duplicate application result" [] (applicationReplyBodies replayEffects)
  (late, lateEffects) <- deliverInstallationReport 63 report replayed
  assertEqual "old installation report preserves the newer barrier" (startupLabelBarrierState replayed) (startupLabelBarrierState late)
  assertEqual
    "old installation report cannot close a valid peer"
    []
    [peer | RejectPeerConnection peer _ <- effectBatchMembers lateEffects]
  assertEqual "reordered collection schedule remains valid" (Right ()) (validateHeraldState late)

caseGenesisDeltaHandoff :: Assertion
caseGenesisDeltaHandoff = do
  fixture <- baselineLabelWorkflow GenesisDeltaHandoff
  let before = workflowBeforeDecision (baselineWorkflow fixture)
      released = workflowDecided (baselineWorkflow fixture)
      object = baselineObject fixture
      delta = baselineDelta fixture
  assertLabelWorkIncrements
    [ ("projection_reuses", 0),
      ("full_preparations", 1),
      ("graph_preparations", 1),
      ("debt_preparations", 1),
      ("graph_changes", 1),
      ("placement_changes", 1),
      ("store_activations", 0),
      ("store_passivations", 1),
      ("store_replacements", 0),
      ("store_deletions", 0)
    ]
    before
    released
  (remoteProcess, remoteHerald) <-
    maybe
      (assertFailure "handoff fixture has no remote target")
      pure
      (baselineRemoteTarget fixture)
  beforeProjection <- requiredGenesisDeltaProjection object before
  releasedProjection <- requiredGenesisDeltaProjection object released
  case (beforeProjection, releasedProjection) of
    ( Reconciliation.DeltaVertexProjection
        beforeProvenance
        beforeDelta
        beforeSort
        beforeController
        beforeActivity,
      Reconciliation.DeltaVertexProjection
        afterProvenance
        afterDelta
        afterSort
        afterController
        afterActivity
      ) -> do
        assertEqual "handoff starts from genesis provenance" (Reconciliation.GenesisStructuralProvenance object) beforeProvenance
        assertEqual "handoff preserves genesis provenance" beforeProvenance afterProvenance
        assertEqual "handoff projection names the selected delta" delta beforeDelta
        assertEqual "handoff preserves the delta" beforeDelta afterDelta
        assertEqual "handoff preserves the exact sort occurrence" beforeSort afterSort
        assertEqual
          "handoff starts at the local controller"
          (Reconciliation.LiveProcessController (baselineLocalProcess fixture) (baselineLocalHerald fixture))
          beforeController
        assertEqual
          "handoff starts as a locally active root"
          (Reconciliation.ActiveRootHere (baselineLocalProcess fixture))
          beforeActivity
        assertEqual
          "handoff installs the remote live controller"
          (Reconciliation.LiveProcessController remoteProcess remoteHerald)
          afterController
        assertEqual
          "handoff makes the root remotely controlled"
          (Reconciliation.RemoteController remoteProcess remoteHerald)
          afterActivity
    projections ->
      assertFailure ("expected genesis delta projections around handoff, got " <> show projections)
  oldPlacement <- requiredLocalPlacement delta before
  oldSlot <- requiredStoreSlot delta before
  assertEqual "handoff removes the active local placement" Nothing (Placement.lookupLocalPlacement delta (startupPlacementState released))
  assertEqual "handoff removes the active Store" Nothing (Store.lookupStoreSlot delta (startupStoreState released))
  assertEqual
    "handoff retains the exact retired Store incarnation"
    (Just oldSlot)
    ( Store.lookupRetainedStoreSlot
        (Store.storeSlotIncarnation oldSlot)
        (startupStoreState released)
    )
  assertEqual
    "handoff preserves the immutable bootstrap-placement basis"
    (Placement.bootstrapLocalPlacements (startupPlacementState before))
    (Placement.bootstrapLocalPlacements (startupPlacementState released))
  assertBool
    "handoff changes the active placement that existed before Release"
    ( Placement.localPlacementDelta oldPlacement
        == delta
    )
  assertBool
    "handoff retains the delta vertex in the live graph"
    (Graph.graphHasVertex (DeltaVertex delta) (startupGraphState released))
  (releaseIndex, cause, controller) <- requiredControllerOverlay object (baselineDecision fixture) released
  assertEqual
    "handoff overlay selects the exact remote controller"
    (Reconciliation.LiveProcessController remoteProcess remoteHerald)
    controller
  assertReleaseControlPrefix before released releaseIndex
  assertExactRemovalDebts cause released
  assertControlledReleasedState
    object
    (ReleasedLabelView ((ProcessLabel remoteProcess, 1)))
    released

caseGenesisWiringLabels :: Assertion
caseGenesisWiringLabels = forM_ [(Access.environmentHubKey, False), (Access.environmentHubKey, True), (Access.environmentEdgeKey Access.NeutralVertexRole Access.WriterToHub, False), (Access.environmentEdgeKey Access.NeutralVertexRole Access.WriterToHub, True)] $ \(key, delete) -> do
  (oracle, initial, opened) <- initializeOpenedFixture
  let entries = Access.accessEntries (Access.startupAccessPrimordial (openedAccess opened))
      privateObject = case Map.lookup key entries of
        Just (Access.Object value) -> value
        _ -> error "bootstrap fixture omitted its wiring grant"
      process = requiredProcess (openedSessionId opened) (startupApplicationState initial)
      target = if delete then LabelToDelete else LabelToVoid
      operation = LabelApplication privateObject (openedProcessLabel opened) target
  object <- globalObjectIdFromGlobalUniqueId <$> checkedIO "resolve bootstrap wiring" (Application.resolveApplicationPrivateUniqueId process (privateObjectUniqueId privateObject) (startupApplicationState initial))
  writer <- writerFor Access.NeutralVertexRole (openedAccess opened)
  reader <- readerFor Access.NeutralVertexRole (openedAccess opened)
  globalWriter <- nablaIdFromGlobalObjectId . globalObjectIdFromGlobalUniqueId <$> checkedIO "resolve bootstrap writer" (Application.resolveApplicationPrivateUniqueId process (privateNablaUniqueId writer) (startupApplicationState initial))
  globalReader <- deltaIdFromGlobalObjectId . globalObjectIdFromGlobalUniqueId <$> checkedIO "resolve bootstrap reader" (Application.resolveApplicationPrivateUniqueId process (privateDeltaUniqueId reader) (startupApplicationState initial))
  let path state = globalReader `elem` fmap fst (Graph.graphReachableDeltas globalWriter (startupGraphState state))
      graphObject state =
        if key == Access.environmentHubKey
          then Graph.graphHasVertex (NeutralVertex object) (startupGraphState state)
          else Map.member object (Graph.graphStructuralEdgeProjections (startupGraphState state))
  assertBool "ordinary bootstrap graph contains the named wiring object" (graphObject initial)
  assertBool "bootstrap writer reaches its matching reader through the hub" (path initial)
  (accepted, acceptedEffects) <- call 3 opened 1 operation initial
  assertEqual "wiring uses ordinary distributed label admission" [OperationAccepted (requestId 1) LabelSettlementPending] (applicationReplyBodies acceptedEffects)
  (retry, _) <- call 5 opened 1 operation accepted
  workflow <- commitCompleteLabelWorkflowTrace 6 oracle retry
  let released = workflowDecided workflow
      completed = workflowCompleted workflow
  assertBool "normal label workflow returns LabelApplied" (Completed (requestId 1) (LabelCompleted LabelApplied) `elem` concatMap applicationReplyBodies (workflowEffects workflow))
  if delete
    then do
      assertBool "deletion removes the named object from the active graph" (not (graphObject released))
      assertBool "no anonymous bootstrap edge preserves the removed path" (not (path released))
      assertControlledReleasedState object (ReleasedDeletedView 1) released
      (_, retryEffects) <- call 30 opened 2 (LabelApplication privateObject (openedProcessLabel opened) LabelToVoid) completed
      assertEqual "a later call cannot resurrect deleted wiring" [Rejected (requestId 2) (ApplicationObjectNotLabelable privateObject)] (applicationReplyBodies retryEffects)
    else do
      assertBool "wiring with a void label still participates in topology" (graphObject released && path released)
      assertEqual "relabeling wiring does not change graph edges" (Graph.graphEdges (startupGraphState initial)) (Graph.graphEdges (startupGraphState released))
      assertControlledReleasedState object (ReleasedLabelView (VoidLabel, 1)) released
      assertNoOpLabelRefresh (activeDecision accepted) object workflow

caseGenesisDeltaDelete :: Assertion
caseGenesisDeltaDelete = do
  fixture <- baselineLabelWorkflow GenesisDeltaDelete
  let before = workflowBeforeDecision (baselineWorkflow fixture)
      released = workflowDecided (baselineWorkflow fixture)
      completed = workflowCompleted (baselineWorkflow fixture)
      object = baselineObject fixture
      delta = baselineDelta fixture
  assertLabelWorkIncrements
    [ ("projection_reuses", 0),
      ("full_preparations", 1),
      ("graph_preparations", 1),
      ("debt_preparations", 1),
      ("graph_changes", 1),
      ("placement_changes", 1),
      ("store_activations", 0),
      ("store_passivations", 0),
      ("store_replacements", 0),
      ("store_deletions", 1)
    ]
    before
    released
  _ <- requiredGenesisDeltaProjection object before
  _ <- requiredLocalPlacement delta before
  oldSlot <- requiredStoreSlot delta before
  assertEqual
    "delete removes the genesis projection from the reconciliation owner"
    Nothing
    ( Map.lookup
        object
        ( Reconciliation.structuralAppliedVertexProjections
            ( GraphProgress.structuralProgressReconciliation
                (startupStructuralProgressState released)
            )
        )
    )
  assertBool
    "delete removes the genesis delta from the live graph"
    (not (Graph.graphHasVertex (DeltaVertex delta) (startupGraphState released)))
  assertEqual "delete removes the active local placement" Nothing (Placement.lookupLocalPlacement delta (startupPlacementState released))
  assertEqual "delete removes the active Store" Nothing (Store.lookupStoreSlot delta (startupStoreState released))
  (releaseIndex, cause) <- requiredDeletedOverlay object (baselineDecision fixture) released
  preparedExpectedPurge <-
    either
      (assertFailure . ("prepare expected genesis Delta purge: " <>) . show)
      pure
      ( Store.prepareControlledObjectPurge
          cause
          (Store.storeSlotSortId oldSlot)
          (controlledObjectKey (globalUniqueIdFromGlobalObjectId object))
          (startupStoreState before)
      )
  let (expectedPurgeStore, _) = Store.commitControlledObjectPurge preparedExpectedPurge
  assertEqual
    "delete retains the exact retired Store incarnation plus its terminal purge"
    (Store.lookupStoreSlot delta expectedPurgeStore)
    ( Store.lookupRetainedStoreSlot
        (Store.storeSlotIncarnation oldSlot)
        (startupStoreState released)
    )
  assertEqual
    "delete preserves the immutable bootstrap-placement basis"
    (Placement.bootstrapLocalPlacements (startupPlacementState before))
    (Placement.bootstrapLocalPlacements (startupPlacementState released))
  assertEqual
    "delete suppresses retained direct and Store possession evidence"
    Nothing
    ( Controlled.controlledEffectivePossessionStrength
        (baselineLocalProcess fixture)
        object
        (startupControlledState released)
    )
  assertBool
    "delete removes the object from effective Normal possession enumeration"
    ( object
        `notElem` ( snd
                      <$> Controlled.controlledNormalPossessionEntries
                        (startupControlledState released)
                  )
    )
  assertReleaseControlPrefix before released releaseIndex
  assertExactRemovalDebts cause released
  assertControlledReleasedState object (ReleasedDeletedView 1) released
  assertReleasedCheckpointGeneration object (ReleasedDeletedView 1) released
  (_, retryEffects) <-
    call
      30
      (baselineOpened fixture)
      2
      ( LabelApplication
          (baselinePrivateObject fixture)
          (openedProcessLabel (baselineOpened fixture))
          LabelToVoid
      )
      completed
  assertEqual
    "a later Label call cannot resurrect the deleted baseline object"
    [ Rejected
        (requestId 2)
        (ApplicationObjectNotLabelable (baselinePrivateObject fixture))
    ]
    (applicationReplyBodies retryEffects)

caseSameControllerReleaseIsTopologyNeutral :: Assertion
caseSameControllerReleaseIsTopologyNeutral = do
  fixture <- baselineLabelWorkflow GenesisDeltaSameController
  assertNoOpLabelRefresh (baselineDecision fixture) (baselineObject fixture) (baselineWorkflow fixture)
  let before = workflowBeforeDecision (baselineWorkflow fixture)
      released = workflowDecided (baselineWorkflow fixture)
      object = baselineObject fixture
      delta = baselineDelta fixture
      beforeProgress = startupStructuralProgressState before
      releasedProgress = startupStructuralProgressState released
  _ <- requiredGenesisDeltaProjection object before
  _ <- requiredGenesisDeltaProjection object released
  assertBool "same-controller Release preserves Graph" (startupGraphState before == startupGraphState released)
  assertBool "same-controller Release preserves Placement" (startupPlacementState before == startupPlacementState released)
  assertBool "same-controller Release preserves Store" (startupStoreState before == startupStoreState released)
  assertBool "same-controller Release preserves Alignment" (startupAlignmentState before == startupAlignmentState released)
  let (retainedCauses, _, _) = CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache before)
  assertBool "same-controller fixture retains a negative cause-coverage query" (not (Set.null retainedCauses))
  forM_ (Set.toList retainedCauses) $ \cause ->
    assertEqual "the retained label consequence is not covered by an installed cut" (Just Nothing) (CutQueries.cutQueryCauseCoverage cause (startupAlignmentCutQueryCache before))
  assertBool
    "same-controller Release preserves structural reconciliation"
    ( GraphProgress.structuralProgressReconciliation beforeProgress
        == GraphProgress.structuralProgressReconciliation releasedProgress
    )
  assertEqual
    "same-controller Release creates no structural overlay"
    ( Reconciliation.structuralAppliedControlOverlays
        (GraphProgress.structuralProgressReconciliation beforeProgress)
    )
    ( Reconciliation.structuralAppliedControlOverlays
        (GraphProgress.structuralProgressReconciliation releasedProgress)
    )
  assertBool
    "same-controller Release retains the live delta"
    (Graph.graphHasVertex (DeltaVertex delta) (startupGraphState released))
  releaseIndex <- requiredReleaseIndex (baselineDecision fixture) released
  assertReleaseControlPrefix before released releaseIndex
  assertEqual
    "same-controller Release retains no Alignment consequence"
    []
    ( Alignment.structuralDebtsForCause
        (requiredReleaseCause (baselineDecision fixture) releaseIndex)
        (startupAlignmentState released)
    )
  assertControlledReleasedState
    object
    (ReleasedLabelView ((ProcessLabel (baselineLocalProcess fixture), 1)))
    released
  assertReleasedCheckpointGeneration
    object
    (ReleasedLabelView (ProcessLabel (baselineLocalProcess fixture), 1))
    released
  let completed = workflowCompleted (baselineWorkflow fixture)
      staleOperation =
        LabelApplication
          (baselinePrivateObject fixture)
          (openedProcessLabel (baselineOpened fixture))
          LabelToVoid
  (staleAccepted, _) <- call 30 (baselineOpened fixture) 2 staleOperation completed
  (_, staleRejected, staleEffects) <- commitNextLabelRequest 31 (workflowOracle (baselineWorkflow fixture)) staleAccepted
  assertEqual
    "the unchanged owner does not make the old expected generation reusable"
    [Completed (requestId 2) (LabelCompleted LabelNotApplied)]
    (applicationReplyBodies staleEffects)
  assertControlledReleasedState
    object
    (ReleasedLabelView (ProcessLabel (baselineLocalProcess fixture), 1))
    staleRejected

-- Compare the optimized release with the former full leaf-application path.
-- These fixtures have other live genesis Stores, so preserving every owner
-- checks that an authority-only label leaves unrelated resources untouched too.
assertNoOpLabelRefresh :: LabelDecisionId -> GlobalObjectId -> CompleteLabelWorkflowTrace -> Assertion
assertNoOpLabelRefresh decision object workflow = do
  let before = workflowBeforeDecision workflow
      released = workflowDecided workflow
      beforeProgress = startupStructuralProgressState before
      reconciliation = GraphProgress.structuralProgressReconciliation beforeProgress
      beforeWork = Map.fromList (LabelPatch.labelStructuralWorkCounts (startupLabelPatchState before))
      releasedWork = Map.fromList (LabelPatch.labelStructuralWorkCounts (startupLabelPatchState released))
  assertEqual
    "production installation records one projection reuse and no full, graph, debt or Store work"
    (Map.adjust (+ 1) "label.structural.projection_reuses" beforeWork)
    releasedWork
  releaseIndex <- requiredReleaseIndex decision released
  let cause = requiredReleaseCause decision releaseIndex
  record <-
    maybe
      (assertFailure "no-op fixture lacks its installed label")
      pure
      (Controlled.controlledReleasedLabelRecord object (startupControlledState released))
  preparation <-
    checkedIO
      "derive the full label reconciliation reference"
      ( Reconciliation.prepareStructuralLabelControlRefresh
          (labelRefreshReferenceViews before)
          cause
          object
          (labelRecordReleasedState record)
          Nothing
          reconciliation
      )
  prepared <- case preparation of
    Reconciliation.StructuralReady value -> pure value
    Reconciliation.StructuralHeld dependencies ->
      assertFailure ("no-op fixture unexpectedly held: " <> show dependencies)
  assertBool "reference certifies no reconciliation mutation" (not (Reconciliation.preparedStructuralChangesState prepared))
  optimized <-
    checkedIO
      "derive unchanged label from target-local evidence"
      ( Reconciliation.prepareUnchangedStructuralLabelControlRefresh
          (labelRefreshTargetReferenceViews object before)
          cause
          object
          (labelRecordReleasedState record)
          Nothing
          reconciliation
      )
  assertEqual "target-local preparation equals the full independent reference" (Just prepared) optimized
  assertEqual "reference preserves hidden reconciliation facts" reconciliation (Reconciliation.preparedStructuralSuccessor prepared)
  assertEqual "reference retains no active Store changes" Map.empty (Reconciliation.storePatchChanges (Reconciliation.preparedStructuralStorePatch prepared))
  assertEqual "reference retains no hidden Store resources" Map.empty (Reconciliation.storePatchRetainedResourceChanges (Reconciliation.preparedStructuralStorePatch prepared))
  assertEqual "reference retains no debts" [] (structuralDebtSetEntries (Reconciliation.preparedStructuralDebts prepared))
  fullStore <-
    Store.commitStructuralStorePatch
      <$> checkedIO "apply reference Store patch" (Store.prepareStructuralStorePatch cause (Reconciliation.preparedStructuralStorePatch prepared) (startupStoreState before))
  (fullPlacement, placementReport) <-
    Placement.commitStructuralPlacementPatch
      <$> checkedIO "apply reference Placement patch" (Placement.prepareStructuralPlacementPatch releaseIndex (Reconciliation.preparedStructuralPlacementPatch prepared) (startupPlacementState before))
  (fullGraph, fullProgress) <-
    GraphProgress.commitStructuralProjectionRefresh
      <$> checkedIO "apply reference Graph patch" (GraphProgress.prepareStructuralProjectionRefresh prepared (startupGraphState before) beforeProgress)
  (fullAlignment, _) <-
    Alignment.commitStructuralDebtRetention
      <$> checkedIO "apply reference Alignment debts" (Alignment.prepareStructuralDebtRetention cause (Reconciliation.preparedStructuralDebts prepared) (startupAlignmentState before))
  assertBool "full Store application is a no-op" (fullStore == startupStoreState before)
  assertBool "optimized Store equals full application" (fullStore == startupStoreState released)
  assertBool "full Placement application is a no-op" (fullPlacement == startupPlacementState before)
  assertBool "optimized Placement equals full application" (fullPlacement == startupPlacementState released)
  assertEqual "empty Placement patch emits no receipt" Nothing placementReport
  assertBool "full Graph application is a no-op" (fullGraph == startupGraphState before && fullProgress == beforeProgress)
  assertBool "optimized Graph equals full application" (fullGraph == startupGraphState released)
  assertBool "full Alignment application is a no-op" (fullAlignment == startupAlignmentState before)
  assertBool "optimized Alignment equals full application" (fullAlignment == startupAlignmentState released)
  assertEqual "authority-only installation preserves retained alignment query coordinates" (CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache before)) (CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache released))
  assertEqual "authority-only installation preserves prepared structural cut positions" (CutQueries.cutQueryPositions (startupAlignmentCutQueryCache before)) (CutQueries.cutQueryPositions (startupAlignmentCutQueryCache released))
  assertBool "decision installation leaves unrelated application observations admitted" (not (Application.applicationMembershipGateClosed (startupApplicationState before)))
  (_, lossEffects) <-
    checkedIO "run unchanged loss delivery reference" (AlignmentTransfer.deliverAlignmentLossTransferDelta before before)
  assertEqual "unchanged owners produce no loss or wait effects" [] (effectBatchMembers lossEffects)
  assertReleaseControlPrefix before released releaseIndex
  assertEqual "optimized released Herald remains valid" (Right ()) (validateHeraldState released)

assertLabelWorkIncrements :: [(String, Word64)] -> HeraldState -> HeraldState -> Assertion
assertLabelWorkIncrements expected before after = do
  let beforeWork = Map.fromList (LabelPatch.labelStructuralWorkCounts (startupLabelPatchState before))
      afterWork = Map.fromList (LabelPatch.labelStructuralWorkCounts (startupLabelPatchState after))
  forM_ expected $ \(name, increment) -> do
    let key = "label.structural." <> name
    assertEqual ("production label work: " <> name) ((+ increment) <$> Map.lookup key beforeWork) (Map.lookup key afterWork)

labelRefreshTargetReferenceViews :: GlobalObjectId -> HeraldState -> Reconciliation.ReconciliationViews
labelRefreshTargetReferenceViews object state =
  Reconciliation.reconciliationViewsWithEndedProcesses
    ( Map.fromList
        [ (process, (residence, OracleProjection.projectedEndedProcessControlIndex ended))
        | (process, ended) <- OracleProjection.projectedEndedProcesses oracle,
          Just residence <- [OracleProjection.oracleViewProcessResidence process view]
        ]
    )
    $ Reconciliation.reconciliationViews
      (Genesis.checkedLocalHeraldEpoch (startupGenesis state))
      (error "authority-only label constructed the full sort registry")
      (error "authority-only label constructed the full graph view")
      ( Map.fromList
          [ (process, residence)
          | process <- OracleProjection.projectedProcessEpochs oracle,
            OracleProjection.oracleViewProcessIsLive process view,
            Just residence <- [OracleProjection.oracleViewProcessResidence process view]
          ]
      )
      ( Set.fromList
          [ (process, subject)
          | (process, subject) <- Controlled.controlledNormalPossessionEntries (startupControlledState state),
            subject == object
          ]
      )
  where
    oracle = startupOracleProjectionState state
    view = OracleProjection.oracleView oracle

labelRefreshReferenceViews :: HeraldState -> Reconciliation.ReconciliationViews
labelRefreshReferenceViews state =
  Reconciliation.reconciliationViewsWithEndedProcesses
    ( Map.fromList
        [ (process, (residence, OracleProjection.projectedEndedProcessControlIndex ended))
        | (process, ended) <- OracleProjection.projectedEndedProcesses oracle,
          Just residence <- [OracleProjection.oracleViewProcessResidence process view]
        ]
    )
    $ Reconciliation.reconciliationViews
      (Genesis.checkedLocalHeraldEpoch (startupGenesis state))
      ( Map.fromList
          [ ( SortRegistry.registryEntrySortId entry,
              sortOccurrence (SortRegistry.registryEntrySortId entry) (SortRegistry.registryEntryOccurrenceId entry)
            )
          | entry <- SortRegistry.registryEntries (startupSortRegistryState state)
          ]
      )
      (Set.fromList (Graph.graphNonStructuralBaselineVertices (startupGraphState state)))
      ( Map.fromList
          [ (process, residence)
          | process <- OracleProjection.projectedProcessEpochs oracle,
            OracleProjection.oracleViewProcessIsLive process view,
            Just residence <- [OracleProjection.oracleViewProcessResidence process view]
          ]
      )
      (Set.fromList (Controlled.controlledNormalPossessionEntries (startupControlledState state)))
  where
    oracle = startupOracleProjectionState state
    view = OracleProjection.oracleView oracle

caseGenesisNablaAuthorityHandoff :: Assertion
caseGenesisNablaAuthorityHandoff = do
  fixture <- genesisNablaAuthorityWorkflow LabelToRemoteProcess
  let released = workflowDecided fixture.nablaWorkflow
      progress = startupStructuralProgressState released
      oracle = startupOracleProjectionState released
      controlled = startupControlledState released
      genesisCut = GraphProgress.structuralGenesisCutId progress
      zero = controlIndex 0
  historical <-
    checkedIO
      "resolve delayed genesis Nabla authority"
      ( Authority.resolveNablaAuthorityAt
          fixture.nablaId
          genesisAuthorityEpoch
          genesisCut
          zero
          progress
          oracle
      )
  assertEqual
    "pre-handoff coordinate retains its original controller"
    fixture.nablaLocalProcess
    (Authority.nablaAuthorityResolutionController historical)
  assertEqual
    "a later handoff alone does not invalidate delayed pre-fence work"
    (Right ())
    (Authority.checkCurrentAuthorityApplicability historical controlled)
  releaseIndex <- requiredReleaseIndex fixture.nablaDecision released
  let releasedAuthority = labelAuthorityEpoch releaseIndex
  current <-
    checkedIO
      "resolve released genesis Nabla authority"
      ( Authority.resolveNablaAuthorityAt
          fixture.nablaId
          releasedAuthority
          genesisCut
          releaseIndex
          progress
          oracle
      )
  assertEqual
    "handoff derives the release-index authority"
    (LabelAuthorityEpochView releaseIndex)
    (authorityEpochView (Authority.nablaAuthorityResolutionAuthority current))
  assertEqual
    "handoff authority belongs to the selected process"
    fixture.nablaRemoteProcess
    (Authority.nablaAuthorityResolutionController current)
  assertEqual
    "handoff raises the minimum source-control prerequisite"
    releaseIndex
    (Authority.nablaAuthorityResolutionMinimumControlPrerequisite current)
  case Authority.resolveNablaAuthorityAt
    fixture.nablaId
    genesisAuthorityEpoch
    genesisCut
    releaseIndex
    progress
    oracle of
    Left (Authority.NablaAuthorityClaimMismatch observed expected claimed) -> do
      assertEqual "bad-claim Nabla" fixture.nablaId observed
      assertEqual "bad-claim expected authority" releasedAuthority expected
      assertEqual "bad-claim supplied authority" genesisAuthorityEpoch claimed
    other -> assertFailure ("post-handoff genesis claim was not rejected exactly: " <> show other)
  let retired =
        Controlled.commitControlledProcessRetirement
          ( Controlled.prepareControlledProcessRetirement
              fixture.nablaLocalProcess
              controlled
          )
  assertEqual
    "ending the historical controller prevents current writer use"
    ( Left
        ( Authority.CurrentAuthorityControllerEnded
            fixture.nablaId
            fixture.nablaLocalProcess
        )
    )
    (Authority.checkCurrentAuthorityApplicability historical retired)

caseGenesisNablaAuthorityDelete :: Assertion
caseGenesisNablaAuthorityDelete = do
  fixture <- genesisNablaAuthorityWorkflow DeleteNabla
  let beforeRelease = workflowBeforeDecision fixture.nablaWorkflow
      released = workflowDecided fixture.nablaWorkflow
      progress = startupStructuralProgressState released
      oracle = startupOracleProjectionState released
      controlled = startupControlledState released
      genesisCut = GraphProgress.structuralGenesisCutId progress
  assertEqual
    "the live deletion fixture retains one pristine Nabla child reservation"
    1
    (length fixture.nablaRetiredReservations)
  forM_ fixture.nablaRetiredReservations $ \generated -> do
    before <-
      maybe
        (assertFailure "genesis Nabla child reservation is absent before deletion")
        pure
        ( Controlled.controlledReservationWitness
            generated
            (startupControlledState beforeRelease)
        )
    assertEqual
      "genesis Nabla child reservation is pristine before deletion"
      Controlled.Reserved
      (Controlled.reservationWitnessPhase before)
    assertEqual
      "live label deletion delegates child-reservation cancellation to shared removal"
      Nothing
      (Controlled.controlledReservationWitness generated controlled)
  historical <-
    checkedIO
      "resolve pre-delete genesis Nabla authority"
      ( Authority.resolveNablaAuthorityAt
          fixture.nablaId
          genesisAuthorityEpoch
          genesisCut
          (controlIndex 0)
          progress
          oracle
      )
  assertEqual
    "delete is a monotone current applicability failure"
    (Left (Authority.CurrentAuthorityDeleted fixture.nablaId))
    (Authority.checkCurrentAuthorityApplicability historical controlled)
  releaseIndex <- requiredReleaseIndex fixture.nablaDecision released
  assertEqual
    "the terminal coordinate cannot publish with the retained tenure"
    (Left (Authority.NablaAuthorityReleasedDeleted fixture.nablaId))
    ( Authority.resolveNablaAuthorityAt
        fixture.nablaId
        genesisAuthorityEpoch
        genesisCut
        releaseIndex
        progress
        oracle
    )

caseDynamicNablaAuthorityHandoff :: Assertion
caseDynamicNablaAuthorityHandoff = do
  fixture <- dynamicNablaAuthorityWorkflow
  let before = fixture.dynamicBeforeRelease
      released = workflowDecided fixture.dynamicWorkflow
      beforeProgress = startupStructuralProgressState before
      releasedProgress = startupStructuralProgressState released
      releasedOracle = startupOracleProjectionState released
      releasedControlled = startupControlledState released
      beforeControlled = startupControlledState before
      sourceTopology = GraphProgress.structuralLastInstalledCutId beforeProgress
  forM_ [before, workflowBeforeDecision fixture.dynamicWorkflow, released, workflowCompleted fixture.dynamicWorkflow] $ \state -> do
    let progress = startupStructuralProgressState state
        controlled = startupControlledState state
        structuralQueries = GraphProgress.prepareStructuralQueries (startupGraphState state) progress
        operations = ControlledOperate.prepareControlledQueries controlled (startupGraphState state) progress
        writer = fixture.dynamicNablaId
    assertEqual
      "prepared current authority retains the exact historical coordinate through handoff"
      (Authority.resolveCurrentNablaAuthority writer progress controlled)
      (Authority.resolvePreparedCurrentNablaAuthority writer structuralQueries controlled)
    assertEqual
      "retained historical projection matches fresh reconstruction through handoff"
      (GraphProgress.structuralNablaProjectionAtCoordinate writer (GraphProgress.structuralLastInstalledCutId progress) (GraphProgress.structuralAppliedControlPrefix progress) progress)
      (GraphProgress.lookupPreparedCurrentNablaProjection writer structuralQueries)
    forM_ [fixture.dynamicLocalProcess, fixture.dynamicRemoteProcess] $ \process ->
      assertEqual
        "prepared writer operation preserves complete result and error ordering through handoff"
        (fmap ControlledOperate.controlledOperateWriterBinding (ControlledOperate.checkControlledOperate process writer controlled progress))
        (fmap ControlledOperate.controlledOperateWriterBinding (ControlledOperate.checkPreparedControlledOperate process writer operations))
    forM_ [fixture.dynamicLocalProcess, fixture.dynamicRemoteProcess] $ \process ->
      assertEqual
        "prepared writer capture preserves its immutable sequencing association through handoff"
        (fmap ControlledOperate.controlledOperateSequencingObject (ControlledOperate.checkControlledOperate process writer controlled progress))
        (fmap ControlledOperate.controlledOperateSequencingObject (ControlledOperate.checkPreparedControlledOperate process writer operations))
  installation <-
    maybe
      (assertFailure "settled Nabla publication has no dynamic installation")
      pure
      (GraphProgress.lookupInstalledDynamicNabla fixture.dynamicNablaId beforeProgress)
  let occurrence = GraphProgress.dynamicVertexInstallationOccurrence installation
      authorityCut = GraphProgress.dynamicVertexInstallationEarliestCut installation
      structuralAuthority = structuralAuthorityEpoch occurrence authorityCut
  initial <-
    checkedIO
      "resolve installed dynamic Nabla authority"
      ( Authority.resolveCurrentNablaAuthority
          fixture.dynamicNablaId
          beforeProgress
          beforeControlled
      )
  assertEqual
    "installed dynamic Nabla starts in its structural tenure"
    (StructuralAuthorityEpochView occurrence authorityCut)
    (authorityEpochView (Authority.nablaAuthorityResolutionAuthority initial))
  assertEqual
    "installed dynamic Nabla starts under its publishing process"
    fixture.dynamicLocalProcess
    (Authority.nablaAuthorityResolutionController initial)
  let authorityMinimum =
        Authority.nablaAuthorityResolutionMinimumControlPrerequisite initial
      cutControl =
        GraphProgress.structuralLastInstalledCutControlPrefix beforeProgress
      sourceControl =
        GraphProgress.structuralPublicationControlPrerequisite
          authorityMinimum
          beforeProgress
  assertEqual
    "publication stamping uses the maximum of writer minimum and selected-cut frontier"
    (max authorityMinimum cutControl)
    sourceControl
  historical <-
    checkedIO
      "resolve delayed dynamic Nabla authority"
      ( Authority.resolveNablaAuthorityAt
          fixture.dynamicNablaId
          structuralAuthority
          sourceTopology
          sourceControl
          releasedProgress
          releasedOracle
      )
  assertEqual
    "installed dynamic Nabla derives its structural tenure from the first covering cut"
    (StructuralAuthorityEpochView occurrence authorityCut)
    (authorityEpochView (Authority.nablaAuthorityResolutionAuthority historical))
  assertEqual
    "delayed pre-release work retains the dynamic Nabla's original controller"
    fixture.dynamicLocalProcess
    (Authority.nablaAuthorityResolutionController historical)
  assertEqual
    "handoff alone leaves delayed pre-release authority applicable"
    (Right ())
    (Authority.checkCurrentAuthorityApplicability historical releasedControlled)
  releaseIndex <- requiredReleaseIndex fixture.dynamicDecision released
  let releasedAuthority = labelAuthorityEpoch releaseIndex
      releasedTopology =
        GraphProgress.structuralLastInstalledCutId releasedProgress
  current <-
    checkedIO
      "resolve released dynamic Nabla authority"
      ( Authority.resolveNablaAuthorityAt
          fixture.dynamicNablaId
          releasedAuthority
          releasedTopology
          releaseIndex
          releasedProgress
          releasedOracle
      )
  assertEqual
    "dynamic handoff derives label authority at the exact release index"
    (LabelAuthorityEpochView releaseIndex)
    (authorityEpochView (Authority.nablaAuthorityResolutionAuthority current))
  assertEqual
    "dynamic handoff installs the selected controller"
    fixture.dynamicRemoteProcess
    (Authority.nablaAuthorityResolutionController current)
  assertEqual
    "dynamic handoff raises the publication prerequisite to Release"
    releaseIndex
    (Authority.nablaAuthorityResolutionMinimumControlPrerequisite current)
  resolvedCurrent <-
    checkedIO
      "resolve current dynamic Nabla authority"
      ( Authority.resolveCurrentNablaAuthority
          fixture.dynamicNablaId
          releasedProgress
          releasedControlled
      )
  assertEqual
    "current dynamic resolution selects the released tenure"
    releasedAuthority
    (Authority.nablaAuthorityResolutionAuthority resolvedCurrent)
  assertEqual
    "current dynamic resolution selects the remote controller"
    fixture.dynamicRemoteProcess
    (Authority.nablaAuthorityResolutionController resolvedCurrent)
  case ControlledOperate.checkControlledOperate
    fixture.dynamicLocalProcess
    fixture.dynamicNablaId
    releasedControlled
    releasedProgress of
    Left problem ->
      assertEqual
        "the former controller cannot operate after handoff"
        ( ControlledOperate.ControlledOperateDynamicControllerMismatch
            fixture.dynamicLocalProcess
            fixture.dynamicRemoteProcess
        )
        problem
    Right _ -> assertFailure "the former controller remained operable after handoff"
  case ControlledOperate.checkControlledOperate
    fixture.dynamicRemoteProcess
    fixture.dynamicNablaId
    releasedControlled
    releasedProgress of
    Left problem ->
      assertEqual
        "the remote controller is not locally operable on its former residence"
        ControlledOperate.ControlledOperateDynamicResidenceMismatch
        problem
    Right _ -> assertFailure "the remote controller was operable on the old residence"

data DynamicNablaAuthorityFixture = DynamicNablaAuthorityFixture
  { dynamicNablaId :: NablaId,
    dynamicDecision :: LabelDecisionId,
    dynamicLocalProcess :: ProcessEpochId,
    dynamicRemoteProcess :: ProcessEpochId,
    dynamicBeforeRelease :: HeraldState,
    dynamicWorkflow :: CompleteLabelWorkflowTrace
  }

dynamicNablaAuthorityWorkflow :: IO DynamicNablaAuthorityFixture
dynamicNablaAuthorityWorkflow = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  writer <- writerFor Access.NablaRole (openedAccess opened)
  (reserved, newIdEffects) <-
    call
      80
      opened
      1
      (NewIdApplication (ControlledNewId writer))
      openedState
  privateNablaIdentity <- completedNewId 1 newIdEffects
  let localPrivateProcess = startupAccessProcess (openedAccess opened)
      localProcess =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      privateObject = asPrivateObjectId privateNablaIdentity
      initialValue = dynamicNablaValue privateNablaIdentity localPrivateProcess
  (pendingInstallation, _) <-
    call
      81
      opened
      2
      (WriteApplication writer (PublishValue initialValue))
      reserved
  pendingSource <- settleStructuralPublication pendingInstallation
  installed <- completeAssignedPeerPublications pendingSource
  globalIdentity <-
    checkedIO
      "resolve dynamic Nabla private identity"
      ( Application.resolveApplicationPrivateUniqueId
          localProcess
          privateNablaIdentity
          (startupApplicationState installed)
      )
  let nabla =
        nablaIdFromGlobalObjectId
          (globalObjectIdFromGlobalUniqueId globalIdentity)
  (targetState, target, remoteTarget) <-
    baselineTarget
      GenesisDeltaHandoff
      localPrivateProcess
      localProcess
      installed
  (remoteProcess, _) <-
    maybe
      (assertFailure "dynamic Nabla fixture has no remote target")
      pure
      remoteTarget
  let operation =
        LabelApplication privateObject (openedProcessLabel opened) target
  (accepted, _) <- call 82 opened 3 operation targetState
  active <-
    maybe
      (assertFailure "dynamic Nabla Label did not install its application owner")
      pure
      (Application.applicationActiveLabel (startupApplicationState accepted))
  (retry, _) <- call 84 opened 3 operation accepted
  workflow <- commitCompleteLabelWorkflowTrace 85 oracle retry
  pure
    DynamicNablaAuthorityFixture
      { dynamicNablaId = nabla,
        dynamicDecision = Application.activeLabelApplicationDecision active,
        dynamicLocalProcess = localProcess,
        dynamicRemoteProcess = remoteProcess,
        dynamicBeforeRelease = installed,
        dynamicWorkflow = workflow
      }

data GenesisNablaAuthorityTarget
  = LabelToRemoteProcess
  | DeleteNabla

data GenesisNablaAuthorityFixture = GenesisNablaAuthorityFixture
  { nablaId :: NablaId,
    nablaDecision :: LabelDecisionId,
    nablaLocalProcess :: ProcessEpochId,
    nablaRemoteProcess :: ProcessEpochId,
    nablaRetiredReservations :: [GlobalUniqueId],
    nablaWorkflow :: CompleteLabelWorkflowTrace
  }

genesisNablaAuthorityWorkflow ::
  GenesisNablaAuthorityTarget -> IO GenesisNablaAuthorityFixture
genesisNablaAuthorityWorkflow requestedTarget = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  privateNabla <- writerFor Access.DeltaRole (openedAccess opened)
  let privateLocalProcess = startupAccessProcess (openedAccess opened)
      localProcess =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      privateObject = asPrivateObjectId (privateNablaUniqueId privateNabla)
  globalIdentity <-
    checkedIO
      "resolve genesis Nabla private identity"
      ( Application.resolveApplicationPrivateUniqueId
          localProcess
          (privateNablaUniqueId privateNabla)
          (startupApplicationState openedState)
      )
  let nabla =
        nablaIdFromGlobalObjectId
          (globalObjectIdFromGlobalUniqueId globalIdentity)
  (targetState, target, remoteTarget, labelRequest, retiredReservations) <-
    case requestedTarget of
      LabelToRemoteProcess -> do
        (targetState, target, remoteTarget) <-
          baselineTarget GenesisDeltaHandoff privateLocalProcess localProcess openedState
        pure (targetState, target, remoteTarget, 1, [])
      DeleteNabla -> do
        (withReservation, childIdEffects) <-
          call
            69
            opened
            1
            (NewIdApplication (ControlledNewId privateNabla))
            openedState
        privateChildIdentity <- completedNewId 1 childIdEffects
        globalChildIdentity <-
          checkedIO
            "resolve genesis Nabla child reservation identity"
            ( Application.resolveApplicationPrivateUniqueId
                localProcess
                privateChildIdentity
                (startupApplicationState withReservation)
            )
        pure
          ( withReservation,
            LabelToDelete,
            Nothing,
            2,
            [globalChildIdentity]
          )
  let operation =
        LabelApplication privateObject (openedProcessLabel opened) target
  (accepted, _) <- call 70 opened labelRequest operation targetState
  active <-
    maybe
      (assertFailure "genesis Nabla Label did not install its application owner")
      pure
      (Application.applicationActiveLabel (startupApplicationState accepted))
  (retry, _) <- call 72 opened labelRequest operation accepted
  workflow <- commitCompleteLabelWorkflowTrace 73 oracle retry
  let remoteProcess = maybe localProcess fst remoteTarget
  pure
    GenesisNablaAuthorityFixture
      { nablaId = nabla,
        nablaDecision = Application.activeLabelApplicationDecision active,
        nablaLocalProcess = localProcess,
        nablaRemoteProcess = remoteProcess,
        nablaRetiredReservations = retiredReservations,
        nablaWorkflow = workflow
      }

data BaselineLabelTarget
  = GenesisDeltaHandoff
  | GenesisDeltaDelete
  | GenesisDeltaSameController

data BaselineLabelFixture = BaselineLabelFixture
  { baselineOpened :: Opened,
    baselinePrivateObject :: PrivateObjectId,
    baselineObject :: GlobalObjectId,
    baselineDelta :: DeltaId,
    baselineDecision :: LabelDecisionId,
    baselineLocalProcess :: ProcessEpochId,
    baselineLocalHerald :: HeraldEpoch,
    baselineRemoteTarget :: Maybe (ProcessEpochId, HeraldEpoch),
    baselineWorkflow :: CompleteLabelWorkflowTrace
  }

baselineLabelWorkflow :: BaselineLabelTarget -> IO BaselineLabelFixture
baselineLabelWorkflow requestedTarget = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  privateDelta <- readerFor Access.DeltaRole (openedAccess opened)
  let privateLocalProcess = startupAccessProcess (openedAccess opened)
      localProcess =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      privateObject = asPrivateObjectId (privateDeltaUniqueId privateDelta)
  globalIdentity <-
    checkedIO
      "resolve genesis delta private identity"
      ( Application.resolveApplicationPrivateUniqueId
          localProcess
          (privateDeltaUniqueId privateDelta)
          (startupApplicationState openedState)
      )
  let object = globalObjectIdFromGlobalUniqueId globalIdentity
      delta = deltaIdFromGlobalObjectId object
  (targetState, target, remoteTarget) <-
    baselineTarget requestedTarget privateLocalProcess localProcess openedState
  let operation =
        LabelApplication privateObject (openedProcessLabel opened) target
  (accepted, _) <- call 3 opened 1 operation targetState
  active <-
    maybe
      (assertFailure "baseline Label did not install its application owner")
      pure
      (Application.applicationActiveLabel (startupApplicationState accepted))
  (retry, _) <- call 5 opened 1 operation accepted
  -- Cache a real pending consequence's uncovered-cut result. This fixture has
  -- no installed structural cut: retaining a negative query is the observable
  -- proof that cursor-only installation did not discard the prepared cache.
  let withQueries = case requestedTarget of
        GenesisDeltaSameController ->
          let nextIndex = controlIndex (1 + controlIndexWord64 (OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState retry))))
              futureCause = requiredReleaseCause (Application.activeLabelApplicationDecision active) nextIndex
           in replaceStartupAlignmentCutQueryCache
                (CutQueries.prepareCutQueryCache (labelRefreshReferenceViews retry) (startupStructuralProgressState retry) (startupPlacementState retry) (Set.singleton futureCause) Set.empty Set.empty (startupAlignmentCutQueryCache retry))
                retry
        _ -> retry
  workflow <- commitCompleteLabelWorkflowTrace 6 oracle withQueries
  pure
    BaselineLabelFixture
      { baselineOpened = opened,
        baselinePrivateObject = privateObject,
        baselineObject = object,
        baselineDelta = delta,
        baselineDecision = Application.activeLabelApplicationDecision active,
        baselineLocalProcess = localProcess,
        baselineLocalHerald = Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis,
        baselineRemoteTarget = remoteTarget,
        baselineWorkflow = workflow
      }

baselineTarget ::
  BaselineLabelTarget ->
  PrivateProcessId ->
  ProcessEpochId ->
  HeraldState ->
  IO (HeraldState, ApplicationLabelTarget, Maybe (ProcessEpochId, HeraldEpoch))
baselineTarget requestedTarget privateLocalProcess localProcess state = case requestedTarget of
  GenesisDeltaDelete -> pure (state, LabelToDelete, Nothing)
  GenesisDeltaSameController ->
    pure
      ( state,
        LabelToProcess privateLocalProcess,
        Nothing
      )
  GenesisDeltaHandoff -> do
    remote <-
      maybe
        (assertFailure "checked initial bootstraps contain no remote process")
        pure
        ( find
            ( (/= Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis)
                . Genesis.appliedProcessResidence
            )
            (Genesis.checkedInitialBootstraps fixtureCheckedInitialBootstraps)
        )
    let remoteProcess = Genesis.appliedProcessEpochId remote
        remoteHerald = Genesis.appliedProcessResidence remote
        roles =
          Application.processRoleView
            ( OracleProjection.projectedProcessEpochs
                (startupOracleProjectionState state)
            )
    prepared <-
      checkedIO
        "localize remote process target"
        ( Application.prepareOrdinaryApplicationValueLocalization
            roles
            localProcess
            (labelValue ((ProcessLabel remoteProcess, 0)))
            (startupApplicationState state)
        )
    let (application, localized) =
          Application.commitOrdinaryApplicationValueLocalization prepared
    privateProcess <- case localized of
      ApplicationValue.LabelValue ((ApplicationValue.ProcessLabel process, _)) -> pure process
      other -> assertFailure ("remote process localization changed shape: " <> show other)
    pure
      ( replaceStartupApplicationState application state,
        LabelToProcess privateProcess,
        Just (remoteProcess, remoteHerald)
      )

requiredGenesisDeltaProjection ::
  GlobalObjectId -> HeraldState -> IO Reconciliation.StructuralVertexProjection
requiredGenesisDeltaProjection object state =
  case Map.lookup
    object
    ( Reconciliation.structuralAppliedVertexProjections
        ( GraphProgress.structuralProgressReconciliation
            (startupStructuralProgressState state)
        )
    ) of
    Just projection@Reconciliation.DeltaVertexProjection {} -> pure projection
    other -> assertFailure ("missing genesis delta projection: " <> show other)

requiredLocalPlacement :: DeltaId -> HeraldState -> IO Placement.LocalPlacement
requiredLocalPlacement delta state =
  maybe
    (assertFailure "genesis delta has no active local placement")
    pure
    (Placement.lookupLocalPlacement delta (startupPlacementState state))

requiredStoreSlot :: DeltaId -> HeraldState -> IO Store.StoreSlot
requiredStoreSlot delta state =
  maybe
    (assertFailure "genesis delta has no active Store")
    pure
    (Store.lookupStoreSlot delta (startupStoreState state))

requiredReleaseIndex :: LabelDecisionId -> HeraldState -> IO ControlIndex
requiredReleaseIndex decision state = do
  workflow <-
    maybe
      (assertFailure "released workflow is absent from the Oracle projection")
      pure
      ( OracleProjection.oracleViewLabelWorkflow
          decision
          (OracleProjection.oracleView (startupOracleProjectionState state))
      )
  case OracleProjection.projectedLabelWorkflowTerminal workflow of
    Just (OracleProjection.ProjectedLabelReleased index _ _) -> pure index
    other -> assertFailure ("workflow has no released terminal: " <> show other)

requiredReleaseCause :: LabelDecisionId -> ControlIndex -> StructuralConsequenceCause
requiredReleaseCause decision index =
  checked "construct exact Label Release cause" (labelReleaseCause decision index)

requiredControllerOverlay ::
  GlobalObjectId ->
  LabelDecisionId ->
  HeraldState ->
  IO (ControlIndex, StructuralConsequenceCause, Reconciliation.ControllerProjection)
requiredControllerOverlay object decision state = do
  index <- requiredReleaseIndex decision state
  case Reconciliation.structuralAppliedControlOverlays
    ( GraphProgress.structuralProgressReconciliation
        (startupStructuralProgressState state)
    ) of
    [(actualObject, Reconciliation.StructuralControllerOverlayView cause controller)] -> do
      assertEqual "controller overlay object" object actualObject
      assertReleaseCause decision index cause
      pure (index, cause, controller)
    overlays -> assertFailure ("expected one controller overlay, got " <> show overlays)

requiredDeletedOverlay ::
  GlobalObjectId ->
  LabelDecisionId ->
  HeraldState ->
  IO (ControlIndex, StructuralConsequenceCause)
requiredDeletedOverlay object decision state = do
  index <- requiredReleaseIndex decision state
  case Reconciliation.structuralAppliedControlOverlays
    ( GraphProgress.structuralProgressReconciliation
        (startupStructuralProgressState state)
    ) of
    [(actualObject, Reconciliation.StructuralDeletedOverlayView cause)] -> do
      assertEqual "delete overlay object" object actualObject
      assertReleaseCause decision index cause
      pure (index, cause)
    overlays -> assertFailure ("expected one deleted overlay, got " <> show overlays)

assertReleaseCause ::
  LabelDecisionId -> ControlIndex -> StructuralConsequenceCause -> Assertion
assertReleaseCause decision index cause = do
  assertEqual "exact retained Label Release cause" (requiredReleaseCause decision index) cause
  assertEqual
    "Label Release cause view"
    (LabelReleaseCauseView decision index)
    (structuralConsequenceCauseView cause)

assertReleaseControlPrefix :: HeraldState -> HeraldState -> ControlIndex -> Assertion
assertReleaseControlPrefix before released releaseIndex = do
  let beforePrefix =
        GraphProgress.structuralAppliedControlPrefix
          (startupStructuralProgressState before)
      releasedPrefix =
        GraphProgress.structuralAppliedControlPrefix
          (startupStructuralProgressState released)
  assertBool "Release strictly advances the structural control prefix" (beforePrefix < releaseIndex)
  assertEqual "Release advances to its exact Oracle index" releaseIndex releasedPrefix

assertExactRemovalDebts :: StructuralConsequenceCause -> HeraldState -> Assertion
assertExactRemovalDebts cause state = do
  let debts = Alignment.structuralDebtsForCause cause (startupAlignmentState state)
      kinds =
        Set.fromList
          (structuralDebtKeyKind . structuralConsequenceDebtKey <$> debts)
  assertEqual "removal retains exactly three Alignment debts" 3 (length debts)
  assertEqual
    "removal retains topology, cutover, and old-Store replacement work"
    (Set.fromList [TopologyAlignmentDebt, CutoverDebt, StoreReplacementDebt])
    kinds

assertControlledReleasedState ::
  GlobalObjectId -> ReleasedLabelStateView -> HeraldState -> Assertion
assertControlledReleasedState object expected state = do
  record <-
    maybe
      (assertFailure "released object has no Controlled label record")
      pure
      (Controlled.controlledReleasedLabelRecord object (startupControlledState state))
  assertEqual
    "Controlled retains the exact released label state"
    expected
    (releasedLabelStateView (labelRecordReleasedState record))

-- These fixtures end at one actual Release. Compare the final object payload
-- against two encodings that differ only in generation, so owner-only live
-- encoding and generation-free terminal encoding cannot satisfy the checkpoint.
assertReleasedCheckpointGeneration ::
  GlobalObjectId -> ReleasedLabelStateView -> HeraldState -> Assertion
assertReleasedCheckpointGeneration object expected state = do
  let projection = startupOracleProjectionState state
      index = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView projection)
  checkpoint <- checkedIO "released topology checkpoint" (OracleProjection.topologyControlCheckpointAt index projection)
  assertBool "the topology checkpoint commits the released successor generation" (payload expected `ByteString.isSuffixOf` checkpoint)
  let predecessor = case expected of
        ReleasedDeletedView generation -> ReleasedDeletedView (generation - 1)
        ReleasedLabelView (owner, generation) -> ReleasedLabelView (owner, generation - 1)
  assertBool "changing only generation changes the checkpoint payload" (not (payload predecessor `ByteString.isSuffixOf` checkpoint))
  where
    payload view =
      globalObjectIdBytes object <> case view of
        ReleasedDeletedView generation -> ByteString.singleton 0 <> word64 generation
        ReleasedLabelView label ->
          let bytes = canonicalValueByteString (canonicalValueBytes (labelValue label))
           in ByteString.singleton 1 <> word64 (fromIntegral (ByteString.length bytes)) <> bytes
    word64 :: Word64 -> ByteString.ByteString
    word64 value = ByteString.pack [fromIntegral (value `shiftR` offset) | offset <- [56, 48 .. 0]]

noHeldReply :: Maybe value -> Bool
noHeldReply Nothing = True
noHeldReply (Just _) = False

data HeldFixture = HeldFixture
  { heldOracle :: OracleState,
    heldOpened :: Opened,
    heldBeforeFence :: HeraldState,
    heldAfterWrite :: HeraldState,
    heldState :: HeraldState,
    heldLabelEffects :: EffectBatch,
    heldWriteEffects :: EffectBatch,
    heldForwardEffects :: EffectBatch,
    heldDecision :: LabelDecisionId,
    heldGlobalWriter :: NablaId,
    heldLabelOperation :: ApplicationOperation,
    heldWriteOperation :: ApplicationOperation,
    heldForwardOperation :: ApplicationOperation
  }

heldFixture :: IO HeldFixture
heldFixture = heldFixtureFor LabelToVoid

heldFixtureFor :: ApplicationLabelTarget -> IO HeldFixture
heldFixtureFor target = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  (writerReady, writer) <- installMutableControlledWriter opened openedState
  (reserved, newIdEffects) <-
    call 3 opened 1 (NewIdApplication (ControlledNewId writer)) writerReady
  privateObject <- completedNewId 1 newIdEffects
  let privateProcess = startupAccessProcess (openedAccess opened)
      initialValue = heldControlledValue privateObject privateProcess "initial"
      updatedValue = heldControlledValue privateObject privateProcess "updated"
  (materialized, firstEffects) <-
    call 4 opened 2 (WriteApplication writer (PublishValue initialValue)) reserved
  assertEqual
    "mutable controlled fixture publishes its initial value"
    [Completed (requestId 2) (WriteCompleted WriteAccepted)]
    (applicationReplyBodies firstEffects)
  holdFixturePublications
    oracle
    materialized
    opened
    writer
    privateObject
    (WriteApplication writer (PublishValue updatedValue))
    target

-- Structural disappearance only applies to predefined objects. Its evidence
-- fixture therefore retains forwards of an immutable neutral definition, while
-- heldFixtureFor exercises actual application-state updates separately. Keep
-- the actual preceding outbox assignments so all disappearance owner classes
-- remain represented; the accepted label consequently waits for source drain.
heldStructuralFixtureStateForEvidenceProperties :: IO HeraldState
heldStructuralFixtureStateForEvidenceProperties = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  writer <- writerFor Access.NeutralVertexRole (openedAccess opened)
  (reserved, newIdEffects) <-
    call 3 opened 1 (NewIdApplication (ControlledNewId writer)) openedState
  privateObject <- completedNewId 1 newIdEffects
  let initialValue = neutralValue privateObject (startupAccessProcess (openedAccess opened))
      initialWrite = WriteApplication writer (PublishValue initialValue)
  (pendingMaterialization, _) <- call 4 opened 2 initialWrite reserved
  materialized <- settleStructuralPublication pendingMaterialization
  heldState
    <$> holdFixturePublicationsWithPendingSource
      oracle
      materialized
      opened
      writer
      privateObject
      (ForwardApplication writer (asPrivateObjectId privateObject))
      LabelToVoid

holdFixturePublications ::
  OracleState ->
  HeraldState ->
  Opened ->
  PrivateNablaId ->
  PrivateUniqueId ->
  ApplicationOperation ->
  ApplicationLabelTarget ->
  IO HeldFixture
holdFixturePublications oracle pendingMaterialized opened writer privateObject heldWrite target = do
  materialized <- completeAssignedPeerPublications pendingMaterialized
  holdFixturePublicationsWithPendingSource oracle materialized opened writer privateObject heldWrite target

holdFixturePublicationsWithPendingSource ::
  OracleState ->
  HeraldState ->
  Opened ->
  PrivateNablaId ->
  PrivateUniqueId ->
  ApplicationOperation ->
  ApplicationLabelTarget ->
  IO HeldFixture
holdFixturePublicationsWithPendingSource oracle materialized opened writer privateObject heldWrite target = do
  let label =
        LabelApplication
          (asPrivateObjectId privateObject)
          (openedProcessLabel opened)
          target
  (beforeFence, labelEffects) <- call 5 opened 3 label materialized
  active <-
    maybe
      (assertFailure "accepted label call did not install its application owner")
      pure
      (Application.applicationActiveLabel (startupApplicationState beforeFence))
  let heldForward = ForwardApplication writer (asPrivateObjectId privateObject)
  (afterWrite, writeEffects) <- call 6 opened 4 heldWrite beforeFence
  writeView <- case Application.applicationFenceHeldEntries (startupApplicationState afterWrite) of
    [view] -> pure view
    views -> assertFailure ("expected one held write, got " <> show (length views))
  (afterForward, forwardEffects) <- call 7 opened 5 heldForward afterWrite
  pure
    HeldFixture
      { heldOracle = oracle,
        heldOpened = opened,
        heldBeforeFence = beforeFence,
        heldAfterWrite = afterWrite,
        heldState = afterForward,
        heldLabelEffects = labelEffects,
        heldWriteEffects = writeEffects,
        heldForwardEffects = forwardEffects,
        heldDecision = Application.activeLabelApplicationDecision active,
        heldGlobalWriter = heldWriter writeView,
        heldLabelOperation = label,
        heldWriteOperation = heldWrite,
        heldForwardOperation = heldForward
      }

installMutableControlledWriter ::
  Opened -> HeraldState -> IO (HeraldState, PrivateNablaId)
installMutableControlledWriter opened predecessor = do
  sortWriter <- writerFor Access.SortDefinitionRole (openedAccess opened)
  (defined, sortEffects) <-
    call
      3
      opened
      100
      ( WriteApplication
          sortWriter
          (PublishValue (ApplicationValue.SortDefinitionValue (DeclaredSortDefinition heldControlledDescriptor Nothing)))
      )
      predecessor
  writtenSort <- completedSortDefinition 100 sortEffects
  carrierWriter <- writerFor Access.NablaRole (openedAccess opened)
  (reserved, identityEffects) <-
    call 3 opened 101 (NewIdApplication (ControlledNewId carrierWriter)) defined
  privateIdentity <- completedNewId 101 identityEffects
  let carrier =
        dynamicNablaValueSequencedBy
          privateIdentity
          (startupAccessProcess (openedAccess opened))
          writtenSort
          Nothing
  (pending, _) <- call 3 opened 102 (WriteApplication carrierWriter (PublishValue carrier)) reserved
  installed <- settleStructuralPublication pending
  pure (installed, asPrivateNablaId privateIdentity)

initializeOpenedFixture :: IO (OracleState, HeraldState, Opened)
initializeOpenedFixture = initializeOpenedFixtureWithBootstraps fixtureCheckedInitialBootstraps

initializeOpenedFixtureWithBootstraps :: Genesis.CheckedInitialBootstraps -> IO (OracleState, HeraldState, Opened)
initializeOpenedFixtureWithBootstraps = initializeOpenedFixtureFor fixtureStep14CheckedGenesis

initializeOpenedFixtureFor :: Genesis.CheckedHeraldGenesis -> Genesis.CheckedInitialBootstraps -> IO (OracleState, HeraldState, Opened)
initializeOpenedFixtureFor genesis bootstraps = do
  bootstrap <- case fixtureLocalBootstrapIds of
    first : _ -> pure first
    [] -> assertFailure "fixture has no local bootstrap"
  (initial, initialEffects) <-
    checkedIO
      "initialize live-label Herald"
      ( initialHerald
          (monotonicInstant 0)
          genesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  assertBool "initial Oracle watch and isolation grace" (initialEffectsAreOracleWatchAndGrace initialEffects)
  attempt <- case [candidate | RunOracleClientAction (ConnectAndHelloOracle candidate _) <- effectBatchMembers initialEffects] of
    [observed] -> pure observed
    effects -> assertFailure ("expected one initial Oracle connect, got " <> show effects)
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (Genesis.checkedOracleControlIndex genesis)
          (Just node)
          True
  (bound, _) <- step 1 (OracleInput (OracleHelloReceived attempt acceptance)) initial
  oracle <- checkedIO "initialize matching Oracle" (initialOracle (MembershipFixtures.fixtureCheckedOracleGenesisWithBootstraps genesis bootstraps))
  attachment <-
    maybe
      (assertFailure "primordial application attachment missing")
      pure
      (primordialApplicationAttachment genesis bootstraps bootstrap)
  (openedState, opened) <- open 2 attachment bound
  pure (oracle, openedState, opened)

heldFixtureStateForOwnerProperties :: IO HeraldState
heldFixtureStateForOwnerProperties = heldState <$> heldFixture

-- | Build three whole-Herald states using only ordinary application ingress:
-- one publication held against the original occurrence of @descriptor@, one
-- held against the occurrence installed after the caller's retirement
-- transition, and one held against an unrelated regular sort. Keeping the
-- retirement transition as a callback lets whole-state invariant tests use
-- their real cross-owner coordinator without exporting session internals.
regularRetirementHeldOrdinaryFixture ::
  ApplicationSortDescriptor ->
  ApplicationSortDescriptor ->
  (HeraldState -> IO HeraldState) ->
  IO (HeraldState, HeraldState, HeraldState)
regularRetirementHeldOrdinaryFixture descriptor unrelatedDescriptor retire = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  sortWriter <- writerFor Access.SortDefinitionRole (openedAccess opened)
  (defined, definitionEffects) <-
    call
      10
      opened
      1
      ( WriteApplication
          sortWriter
          ( PublishValue
              ( ApplicationValue.SortDefinitionValue
                  (DeclaredSortDefinition descriptor Nothing)
              )
          )
      )
      openedState
  definedSort <- completedSortDefinition 1 definitionEffects

  oldHeldBeforeResolve <-
    holdOrdinaryPublication
      20
      2
      opened
      definedSort
      defined

  (_, definedAtResolve, _) <-
    positionRegularRetirementResolve 25 oracle defined
  (_, oldHeld, _) <-
    positionRegularRetirementResolve 25 oracle oldHeldBeforeResolve
  retired <- retire definedAtResolve
  (redefined, redefinitionEffects) <-
    call
      30
      opened
      2
      ( WriteApplication
          sortWriter
          ( PublishValue
              ( ApplicationValue.SortDefinitionValue
                  (DeclaredSortDefinition descriptor Nothing)
              )
          )
      )
      retired
  redefinedSort <- completedSortDefinition 2 redefinitionEffects
  assertEqual
    "equal redefinition preserves the regular SortId"
    definedSort
    redefinedSort
  successorHeld <-
    holdOrdinaryPublication
      40
      3
      opened
      redefinedSort
      redefined

  (withUnrelatedDefinition, unrelatedDefinitionEffects) <-
    call
      50
      opened
      3
      ( WriteApplication
          sortWriter
          ( PublishValue
              ( ApplicationValue.SortDefinitionValue
                  (DeclaredSortDefinition unrelatedDescriptor Nothing)
              )
          )
      )
      redefined
  unrelatedSort <- completedSortDefinition 3 unrelatedDefinitionEffects
  unrelatedHeld <-
    holdOrdinaryPublication
      60
      4
      opened
      unrelatedSort
      withUnrelatedDefinition
  pure (oldHeld, successorHeld, unrelatedHeld)

-- | Build a genuinely reachable locally-taken ordinary-publication transcript
-- for a dynamic regular sort.  The application installs a matching Delta,
-- Nabla, and Edge, publishes through that route, locally takes the value, and
-- then deletes the three structural objects through complete Oracle label
-- workflows.  Hidden Store retention and unprocessed distributed consequence
-- work deliberately remain, so retirement tests can prove that local
-- visibility is not counterfeit global absence.
regularRetirementLocallyTakenOrdinaryFixture ::
  ApplicationSortDescriptor ->
  IO (HeraldState, PublicationId)
regularRetirementLocallyTakenOrdinaryFixture descriptor = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  let access = openedAccess opened
      privateProcess = startupAccessProcess access
  sortWriter <- writerFor Access.SortDefinitionRole access
  (defined, definitionEffects) <-
    call
      10
      opened
      1
      ( WriteApplication
          sortWriter
          ( PublishValue
              ( ApplicationValue.SortDefinitionValue
                  (DeclaredSortDefinition descriptor Nothing)
              )
          )
      )
      openedState
  regularSort <- completedSortDefinition 1 definitionEffects

  deltaCarrierWriter <- writerFor Access.DeltaRole access
  (reservedDelta, deltaIdEffects) <-
    call
      20
      opened
      2
      (NewIdApplication (ControlledNewId deltaCarrierWriter))
      defined
  privateDeltaIdentity <- completedNewId 2 deltaIdEffects
  (pendingDelta, _) <-
    call
      21
      opened
      3
      ( WriteApplication
          deltaCarrierWriter
          ( PublishValue
              ( dynamicDeltaValueForSort
                  privateDeltaIdentity
                  privateProcess
                  regularSort
              )
          )
      )
      reservedDelta
  installedDelta <- settleStructuralPublication pendingDelta

  nablaCarrierWriter <- writerFor Access.NablaRole access
  (reservedNabla, nablaIdEffects) <-
    call
      30
      opened
      4
      (NewIdApplication (ControlledNewId nablaCarrierWriter))
      installedDelta
  privateNablaIdentity <- completedNewId 4 nablaIdEffects
  let privateNabla = asPrivateNablaId privateNablaIdentity
  (pendingNabla, _) <-
    call
      31
      opened
      5
      ( WriteApplication
          nablaCarrierWriter
          ( PublishValue
              ( selfSequencingDynamicNablaValue
                  privateNablaIdentity
                  privateProcess
                  regularSort
              )
          )
      )
      reservedNabla
  installedNabla <- settleStructuralPublication pendingNabla

  edgeCarrierWriter <- writerFor Access.EdgeRole access
  (reservedEdge, edgeIdEffects) <-
    call
      40
      opened
      6
      (NewIdApplication (ControlledNewId edgeCarrierWriter))
      installedNabla
  privateEdgeIdentity <- completedNewId 6 edgeIdEffects
  (pendingEdge, _) <-
    call
      41
      opened
      7
      ( WriteApplication
          edgeCarrierWriter
          ( PublishValue
              ( dynamicDirectedEdgeValue
                  privateEdgeIdentity
                  privateNablaIdentity
                  privateDeltaIdentity
                  privateProcess
              )
          )
      )
      reservedEdge
  installedEdge <- settleStructuralPublication pendingEdge

  let ordinaryValue =
        ApplicationValue.RecordValue
          (Map.singleton "key" (ApplicationValue.TextValue "retained transcript"))
      beforeOrdinary =
        Set.fromList
          ( checkedPublicationId
              . Publication.outgoingPublicationChecked
              <$> Publication.outgoingPublications
                (startupPublicationState installedEdge)
          )
  (published, _) <-
    call
      50
      opened
      8
      (WriteApplication privateNabla (PublishValue ordinaryValue))
      installedEdge
  ordinaryRecord <-
    case [ record
         | record <-
             Publication.outgoingPublications
               (startupPublicationState published),
           checkedPublicationId (Publication.outgoingPublicationChecked record)
             `Set.notMember` beforeOrdinary
         ] of
      [record] -> pure record
      records ->
        assertFailure
          ( "expected one ordinary outgoing publication, got "
              <> show (length records)
          )
  let ordinaryPublication = Publication.outgoingPublicationChecked ordinaryRecord
      ordinaryIdentifier = checkedPublicationId ordinaryPublication
  assertEqual
    "reachable ordinary publication uses the dynamic regular sort"
    regularSort
    (checkedPublicationSort ordinaryPublication)
  assertBool
    "reachable ordinary publication has a local destination"
    (not (null (routeDestinations (Publication.outgoingPublicationRoute ordinaryRecord))))

  let query =
        ApplicationQuery
          { applicationQueryDeltas = Set.singleton (asPrivateDeltaId privateDeltaIdentity),
            applicationQueryPredicate = QueryAlways
          }
  (taken, takeEffects) <-
    call
      51
      opened
      9
      (LocalTakeApplication query)
      published
  case applicationReplyBodies takeEffects of
    [Completed actual (LocalTakeCompleted values)] -> do
      assertEqual "reachable ordinary LocalTake request" (requestId 9) actual
      assertEqual "reachable ordinary LocalTake value" [ordinaryValue] values
    actual ->
      assertFailure
        ("unexpected reachable ordinary LocalTake reply: " <> show actual)

  (afterEdgeOracle, withoutEdge) <-
    deleteFixtureObject
      100
      10
      opened
      privateEdgeIdentity
      oracle
      taken
  (afterNablaOracle, withoutNabla) <-
    deleteFixtureObject
      200
      11
      opened
      privateNablaIdentity
      afterEdgeOracle
      withoutEdge
  (_, afterStructuralDeletion) <-
    deleteFixtureObject
      300
      12
      opened
      privateDeltaIdentity
      afterNablaOracle
      withoutNabla
  assertEqual
    "locally taken ordinary transcript leaves a valid whole Herald"
    (Right ())
    (validateHeraldState afterStructuralDeletion)
  pure (afterStructuralDeletion, ordinaryIdentifier)

deleteFixtureObject ::
  Word ->
  Word ->
  Opened ->
  PrivateUniqueId ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState)
deleteFixtureObject firstObserved request opened privateObject oracle pendingPredecessor = do
  predecessor <- completeAssignedPeerPublications pendingPredecessor
  let operation =
        LabelApplication
          (asPrivateObjectId privateObject)
          (openedProcessLabel opened)
          LabelToDelete
  (accepted, _) <- call firstObserved opened request operation predecessor
  (oracleAfterOpen, afterOpen, _) <-
    commitNextLabelRequest (firstObserved + 1) oracle accepted
  let withProjectedOpen = afterOpen
  (ready, _) <-
    call
      (firstObserved + 2)
      opened
      request
      operation
      withProjectedOpen
  (successorOracle, completed, _) <-
    commitCompleteLabelWorkflow
      (firstObserved + 3)
      oracleAfterOpen
      ready
  assertEqual
    "controlled fixture deletion leaves a valid whole Herald"
    (Right ())
    (validateHeraldState completed)
  pure (successorOracle, completed)

-- The rejected command is still an ordinary committed Oracle entry, so this
-- drives the projection, client cursor, and structural-control owner together
-- to the first post-genesis coordinate without introducing another live
-- process into the fixture.
positionRegularRetirementResolve ::
  Word ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState, EffectBatch)
positionRegularRetirementResolve observed oracle herald = do
  binding <-
    maybe
      (assertFailure "regular-retirement fixture lost its Oracle binding")
      pure
      (OracleClient.oracleClientCurrentBinding (startupOracleClientState herald))
  bootstrap <- case fixtureLocalBootstrapIds of
    _ : second : _ ->
      maybe
        (assertFailure "regular-retirement fixture bootstrap is absent")
        pure
        (Genesis.lookupConfiguredProcessBootstrap second fixtureStep14CheckedGenesis)
    _ -> assertFailure "regular-retirement fixture has fewer than two bootstraps"
  reporter <- case fixtureRemoteHeralds of
    first : _ -> pure first
    [] -> assertFailure "regular-retirement fixture has no remote Herald"
  committed <-
    commitOracleEnvelope
      observed
      binding
      ( oracleEnvelope
          (oracleClientRequestId reporter 1)
          (Just (controlIndex 2))
          reporter
          (startProcessEpochCommand (ProcessStart.processStart (ProcessBootstrap.configuredProcessBootstrapProcessId bootstrap) (ProcessBootstrap.configuredProcessBootstrapProcessEpochId bootstrap) (ProcessBootstrap.configuredProcessBootstrapResidence bootstrap)))
      )
      oracle
      herald
  let (_, successor, _) = committed
  assertEqual
    "regular-retirement fixture reaches the first post-genesis control index"
    (controlIndex 1)
    ( OracleProjection.oracleViewControlIndex
        (OracleProjection.oracleView (startupOracleProjectionState successor))
    )
  pure committed

holdOrdinaryPublication ::
  Word ->
  Word ->
  Opened ->
  SortId ->
  HeraldState ->
  IO HeraldState
holdOrdinaryPublication firstObserved baseRequest opened sort predecessor = do
  carrierWriter <- writerFor Access.NablaRole (openedAccess opened)
  (reservedNabla, nablaIdEffects) <-
    call
      firstObserved
      opened
      baseRequest
      (NewIdApplication (ControlledNewId carrierWriter))
      predecessor
  privateNablaIdentity <- completedNewId baseRequest nablaIdEffects
  let privateProcess = startupAccessProcess (openedAccess opened)
      privateNabla = asPrivateNablaId privateNablaIdentity
      carrier =
        selfSequencingDynamicNablaValue
          privateNablaIdentity
          privateProcess
          sort
  (pendingInstallation, _) <-
    call
      (firstObserved + 1)
      opened
      (baseRequest + 1)
      (WriteApplication carrierWriter (PublishValue carrier))
      reservedNabla
  installed <- settleStructuralPublication pendingInstallation
  let labelOperation =
        LabelApplication
          (asPrivateObjectId privateNablaIdentity)
          (openedProcessLabel opened)
          (LabelToProcess privateProcess)
      heldOperation =
        WriteApplication
          privateNabla
          (PublishValue heldOrdinaryFixtureValue)
  (accepted, _) <-
    call
      (firstObserved + 2)
      opened
      (baseRequest + 2)
      labelOperation
      installed
  (held, heldEffects) <-
    call
      (firstObserved + 3)
      opened
      (baseRequest + 3)
      heldOperation
      accepted
  assertEqual
    "ordinary publication waits behind its exact source fence"
    []
    (applicationReplyBodies heldEffects)
  case Application.applicationFenceHeldEntries (startupApplicationState held) of
    [_] -> pure held
    entries ->
      assertFailure
        ("expected one ordinary fence-held publication, got " <> show (length entries))

heldOrdinaryFixtureValue :: ApplicationValue.ApplicationValue
heldOrdinaryFixtureValue =
  ApplicationValue.RecordValue
    (Map.singleton "key" (ApplicationValue.TextValue "held by source fence"))

eligibleCandidateFixture :: IO (HeraldState, Opened)
eligibleCandidateFixture = do
  (_, openedState, opened) <- initializeOpenedFixture
  writer <- writerFor Access.NeutralVertexRole (openedAccess opened)
  (reserved, newIdEffects) <-
    call 3 opened 1 (NewIdApplication (ControlledNewId writer)) openedState
  privateObject <- completedNewId 1 newIdEffects
  let initialValue = neutralValue privateObject (startupAccessProcess (openedAccess opened))
  (pendingStructural, _) <-
    call
      4
      opened
      2
      (WriteApplication writer (PublishValue initialValue))
      reserved
  let operation =
        LabelApplication
          (asPrivateObjectId privateObject)
          (openedProcessLabel opened)
          LabelToVoid
      eligible = retainDetachedLabelCandidate opened 3 operation pendingStructural
  pure (eligible, opened)

-- | A whole-Herald fixture with one local structural application publication
-- whose immutable admission authority names the genesis membership.
predecessorAdmittedApplicationPublicationFixture :: IO HeraldState
predecessorAdmittedApplicationPublicationFixture = do
  (_, openedState, opened) <- initializeOpenedFixture
  writer <- writerFor Access.NeutralVertexRole (openedAccess opened)
  (reserved, newIdEffects) <-
    call 3 opened 1 (NewIdApplication (ControlledNewId writer)) (MembershipFixtures.fixtureHeraldAfterProbePrefix openedState)
  privateObject <- completedNewId 1 newIdEffects
  let value = neutralValue privateObject (startupAccessProcess (openedAccess opened))
  candidate <- case Application.classifyApplicationRequest
    (openedSessionId opened)
    (openedBinding opened)
    (requestId 2)
    (WriteApplication writer (PublishValue value))
    (startupApplicationState reserved) of
    Right (Application.FirstApplicationRequest accepted) -> pure accepted
    _ -> assertFailure "expected first structural request"
  prepared <-
    checkedIO
      "retain predecessor-admitted structural application stage"
      ( ApplicationPublication.prepareLocalApplicationPublication
          (startupGenesis reserved)
          (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState reserved)))
          (Application.processRoleView (OracleProjection.projectedProcessEpochs (startupOracleProjectionState reserved)))
          candidate
          writer
          value
          (startupControlledState reserved)
          (startupSortRegistryState reserved)
          (startupGraphState reserved)
          (startupStructuralProgressState reserved)
          (startupPlacementState reserved)
          (startupPublicationState reserved)
          (startupStoreState reserved)
          (startupPeerStreamState reserved)
      )
  let (application, controlled, registry, publication, stores, stream, _) =
        ApplicationPublication.commitLocalApplicationPublication prepared
  pure
    ( replaceStartupApplicationState application
        . replaceStartupControlledState controlled
        . replaceStartupSortRegistryState registry
        . replaceStartupPublicationState publication
        . replaceStartupStoreState stores
        . replaceStartupPeerStreamState stream
        $ reserved
    )

-- | A compact, fully settled dynamic Neutral controlled object for composed
-- owner tests.  The returned process is the publisher which initially owns
-- the object, so callers can check both the direct and effective possession
-- fallout without reconstructing application-private identity state.
settledNeutralControlledFixture ::
  IO (HeraldState, GlobalObjectId, ProcessEpochId)
settledNeutralControlledFixture =
  settledStructuralControlledFixture Access.NeutralVertexRole

-- | A fully settled Neutral together with two fresh, ordinary application
-- requests which target that exact private object.  The requests intentionally
-- share their identifier because callers exercise them on independent
-- successors of the returned state.
settledNeutralControlledFixtureWithApplicationRequests ::
  IO
    ( HeraldState,
      GlobalObjectId,
      ProcessEpochId,
      ApplicationRequestIngress,
      ApplicationRequestIngress
    )
settledNeutralControlledFixtureWithApplicationRequests = do
  settledNeutralControlledFixtureWithPublicationRequest
    (\writer _ value -> WriteApplication writer (PublishValue value))

-- | Retained structural history may advance through forwarding even though
-- the neutral definition cannot be written again.
settledNeutralControlledFixtureWithForwardRequest ::
  IO
    ( HeraldState,
      GlobalObjectId,
      ProcessEpochId,
      ApplicationRequestIngress,
      ApplicationRequestIngress
    )
settledNeutralControlledFixtureWithForwardRequest =
  settledNeutralControlledFixtureWithPublicationRequest
    (\writer privateObject _ -> ForwardApplication writer (asPrivateObjectId privateObject))

settledNeutralControlledFixtureWithPublicationRequest ::
  (PrivateNablaId -> PrivateUniqueId -> ApplicationValue.ApplicationValue -> ApplicationOperation) ->
  IO
    ( HeraldState,
      GlobalObjectId,
      ProcessEpochId,
      ApplicationRequestIngress,
      ApplicationRequestIngress
    )
settledNeutralControlledFixtureWithPublicationRequest publicationOperation = do
  (_, openedState, opened) <- initializeOpenedFixture
  let process =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      privateProcess = startupAccessProcess (openedAccess opened)
  writer <- writerFor Access.NeutralVertexRole (openedAccess opened)
  (reserved, newIdEffects) <-
    call 3 opened 1 (NewIdApplication (ControlledNewId writer)) openedState
  privateObject <- completedNewId 1 newIdEffects
  let value = neutralValue privateObject privateProcess
  (pending, _) <-
    call
      4
      opened
      2
      (WriteApplication writer (PublishValue value))
      reserved
  settled <- settleStructuralPublication pending
  globalIdentity <-
    checkedIO
      "resolve settled Neutral identity with application requests"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateObject
          (startupApplicationState settled)
      )
  let ingress operation =
        CallApplicationRequest
          (openedBinding opened)
          (openedSessionId opened)
          (requestId 3)
          operation
  pure
    ( settled,
      globalObjectIdFromGlobalUniqueId globalIdentity,
      process,
      ingress
        ( LabelApplication
            (asPrivateObjectId privateObject)
            (openedProcessLabel opened)
            LabelToVoid
        ),
      ingress (publicationOperation writer privateObject value)
    )

-- | A fully settled dynamic Delta with one real pending application wait on
-- its exact, initially empty Store incarnation.  Removing the controlled
-- Delta passivates that incarnation, so callers can exercise the documented
-- wait-release consequence without fabricating application or Wait owner
-- state.
settledDeltaControlledFixtureWithPendingWait ::
  IO
    ( HeraldState,
      GlobalObjectId,
      ProcessEpochId,
      WaitId,
      ApplicationSessionBinding
    )
settledDeltaControlledFixtureWithPendingWait = do
  (_, openedState, opened) <- initializeOpenedFixture
  let process =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      privateProcess = startupAccessProcess (openedAccess opened)
  writer <- writerFor Access.DeltaRole (openedAccess opened)
  (reserved, newIdEffects) <-
    call 3 opened 1 (NewIdApplication (ControlledNewId writer)) openedState
  privateObject <- completedNewId 1 newIdEffects
  (pendingPublication, _) <-
    call
      4
      opened
      2
      ( WriteApplication
          writer
          (PublishValue (dynamicDeltaValue privateObject privateProcess))
      )
      reserved
  settled <- settleStructuralPublication pendingPublication
  let query =
        ApplicationQuery
          { applicationQueryDeltas = Set.singleton (asPrivateDeltaId privateObject),
            applicationQueryPredicate = QueryAlways
          }
  (withWait, waitEffects) <-
    call
      5
      opened
      3
      (WaitApplication [query])
      settled
  wait <- case applicationReplyBodies waitEffects of
    [WaitAccepted actual retained] -> do
      assertEqual "pending Delta wait request" (requestId 3) actual
      pure retained
    actual ->
      assertFailure
        ("expected one pending Delta Wait acceptance, got " <> show actual)
  globalIdentity <-
    checkedIO
      "resolve settled Delta identity with pending wait"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateObject
          (startupApplicationState withWait)
      )
  pure
    ( withWait,
      globalObjectIdFromGlobalUniqueId globalIdentity,
      process,
      wait,
      openedBinding opened
    )

-- | Fully settle one application-created predefined structural carrier.  An
-- Edge uses its established startup-writer Nabla as a real current endpoint,
-- so every role needs exactly one fresh structural cut.
settledStructuralControlledFixture ::
  ApplicationPredefinedSortRole ->
  IO (HeraldState, GlobalObjectId, ProcessEpochId)
settledStructuralControlledFixture = structuralControlledFixtureWithTake False

locallyTakenNeutralDisappearanceFixture :: IO (HeraldState, GlobalObjectId, ProcessEpochId)
locallyTakenNeutralDisappearanceFixture = structuralControlledFixtureWithTake True Access.NeutralVertexRole

structuralControlledFixtureWithTake :: Bool -> ApplicationPredefinedSortRole -> IO (HeraldState, GlobalObjectId, ProcessEpochId)
structuralControlledFixtureWithTake takeVisible role = do
  (state, object, process, _, _) <- structuralControlledFixtureWithRequests takeVisible role
  pure (state, object, process)

-- | The exact post-LocalTake object and fresh ordinary relabel/forward calls.
-- Each call uses the next request identifier and is exercised on a separate
-- successor, so the race never invents an application-private identity.
locallyTakenNeutralDisappearanceFixtureWithRequests ::
  IO (HeraldState, GlobalObjectId, ProcessEpochId, ApplicationRequestIngress, ApplicationRequestIngress)
locallyTakenNeutralDisappearanceFixtureWithRequests =
  structuralControlledFixtureWithRequests True Access.NeutralVertexRole

structuralControlledFixtureWithRequests ::
  Bool ->
  ApplicationPredefinedSortRole ->
  IO (HeraldState, GlobalObjectId, ProcessEpochId, ApplicationRequestIngress, ApplicationRequestIngress)
structuralControlledFixtureWithRequests takeVisible role = do
  (_, openedState, opened) <- initializeOpenedFixture
  let process =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      privateProcess = startupAccessProcess (openedAccess opened)
  case role of
    Access.SortDefinitionRole ->
      assertFailure "SortDefinition is not a controlled structural fixture role"
    Access.ProcessEpochRole ->
      assertFailure "ProcessEpoch is Herald-managed and cannot be an application fixture"
    _ -> pure ()
  writer <- writerFor role (openedAccess opened)
  (reserved, newIdEffects) <-
    call 3 opened 1 (NewIdApplication (ControlledNewId writer)) openedState
  privateObject <- completedNewId 1 newIdEffects
  value <- case role of
    Access.NeutralVertexRole ->
      pure (neutralValue privateObject privateProcess)
    Access.EdgeRole ->
      pure
        ( dynamicEdgeValue
            privateObject
            (privateNablaUniqueId writer)
            privateProcess
        )
    Access.NablaRole ->
      pure (dynamicNablaValue privateObject privateProcess)
    Access.DeltaRole ->
      pure (dynamicDeltaValue privateObject privateProcess)
    Access.SortDefinitionRole ->
      assertFailure "SortDefinition entered structural fixture publication"
    Access.ProcessEpochRole ->
      assertFailure "ProcessEpoch entered structural fixture publication"
  (pending, _) <-
    call
      4
      opened
      2
      (WriteApplication writer (PublishValue value))
      reserved
  settled <- settleStructuralPublication pending
  globalIdentity <-
    checkedIO
      "resolve settled structural controlled identity"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateObject
          (startupApplicationState settled)
      )
  taken <-
    if takeVisible
      then do
        reader <- readerFor role (openedAccess opened)
        (successor, effects) <-
          call
            5
            opened
            3
            (LocalTakeApplication (ApplicationQuery (Set.singleton reader) QueryAlways))
            settled
        case applicationReplyBodies effects of
          [Completed _ (LocalTakeCompleted values)]
            | value `elem` values -> pure successor
          other -> assertFailure ("live controlled LocalTake did not remove its value: " <> show other)
      else pure settled
  pure
    ( taken,
      globalObjectIdFromGlobalUniqueId globalIdentity,
      process,
      ingressFor opened (LabelApplication (asPrivateObjectId privateObject) (openedProcessLabel opened) LabelToVoid),
      ingressFor opened (ForwardApplication writer (asPrivateObjectId privateObject))
    )
  where
    ingressFor opened operation =
      CallApplicationRequest (openedBinding opened) (openedSessionId opened) (requestId (if takeVisible then 4 else 3)) operation

-- | Test-only whole-Herald path from a live promoted regular-Delta plan
-- through an ordinary Oracle-mediated controlled deletion. Returning the
-- pre- and post-removal states lets Alignment tests install the pending
-- deletion cut without forging either authority or owner state.
regularDeltaControlledRemovalRecoveryFixture ::
  IO (HeraldState, HeraldState)
regularDeltaControlledRemovalRecoveryFixture = do
  (oracle, opened, unprojected, privateObject, _) <- regularDeltaControlledFixture
  let membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState unprojected))
      local = Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis
      remotes =
        filter
          (/= local)
          ( case heraldMembershipGenerationActiveHeraldEpochs membership of
              first :| remaining -> first : remaining
          )
  let withRemotePlacements =
        foldl
          (flip installEmptyRemotePlacement)
          unprojected
          remotes
  (promoted, _, promotionDisposition) <-
    checkedIO
      "promote regular Delta plan before Oracle-mediated removal"
      (AlignmentCoordinator.advanceAlignmentGenerationsWithDisposition withRemotePlacements)
  assertEqual
    "regular Delta Oracle fixture promotes a live plan before removal"
    AlignmentCoordinator.AlignmentGenerationWorkChanged
    promotionDisposition
  (_, removed) <-
    deleteFixtureObject
      7950
      4
      opened
      privateObject
      oracle
      promoted
  pure (promoted, removed)
  where
    installEmptyRemotePlacement remote state =
      replaceStartupPlacementState successor state
      where
        snapshot =
          checked
            "construct empty remote placement for Oracle-mediated removal"
            ( PlacementProtocol.placementSnapshot
                remote
                PlacementProtocol.firstPlacementSequence
                []
            )
        (successor, _) =
          Placement.commitRemotePlacement
            ( checked
                "install empty remote placement for Oracle-mediated removal"
                ( Placement.prepareRemotePlacement
                    (PlacementProtocol.FullPlacementSnapshot snapshot)
                    (startupPlacementState state)
                )
            )

regularDeltaControlledFixture ::
  IO (OracleState, Opened, HeraldState, PrivateUniqueId, ProcessEpochId)
regularDeltaControlledFixture = do
  (oracle, openedState, opened) <- initializeOpenedFixture
  let process =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      access = openedAccess opened
      privateProcess = startupAccessProcess access
  sortWriter <- writerFor Access.SortDefinitionRole access
  (defined, definitionEffects) <-
    call
      10
      opened
      1
      ( WriteApplication
          sortWriter
          ( PublishValue
              ( ApplicationValue.SortDefinitionValue
                  (DeclaredSortDefinition removalRecoveryDescriptor Nothing)
              )
          )
      )
      openedState
  regularSort <- completedSortDefinition 1 definitionEffects
  deltaWriter <- writerFor Access.DeltaRole access
  (reserved, newIdEffects) <-
    call 20 opened 2 (NewIdApplication (ControlledNewId deltaWriter)) defined
  privateObject <- completedNewId 2 newIdEffects
  (pending, _) <-
    call
      21
      opened
      3
      ( WriteApplication
          deltaWriter
          ( PublishValue
              (dynamicDeltaValueForSort privateObject privateProcess regularSort)
          )
      )
      reserved
  settled <- settleStructuralPublication pending
  pure (oracle, opened, settled, privateObject, process)

-- | Fully settle one dynamic Nabla and then make two real application-level
-- controlled-ID reservations against it.  The completed NewId requests and
-- their private/global bindings keep the returned whole-Herald fixture valid,
-- unlike direct insertion into the Controlled owner.
settledNablaControlledFixtureWithPristineReservations ::
  IO (HeraldState, GlobalObjectId, ProcessEpochId, [GlobalUniqueId])
settledNablaControlledFixtureWithPristineReservations = do
  (_, openedState, opened) <- initializeOpenedFixture
  let process =
        requiredProcess
          (openedSessionId opened)
          (startupApplicationState openedState)
      privateProcess = startupAccessProcess (openedAccess opened)
  carrierWriter <- writerFor Access.NablaRole (openedAccess opened)
  (reservedNabla, nablaIdEffects) <-
    call
      3
      opened
      1
      (NewIdApplication (ControlledNewId carrierWriter))
      openedState
  privateNablaIdentity <- completedNewId 1 nablaIdEffects
  (pendingNabla, _) <-
    call
      4
      opened
      2
      ( WriteApplication
          carrierWriter
          (PublishValue (dynamicNablaValue privateNablaIdentity privateProcess))
      )
      reservedNabla
  settledNabla <- settleStructuralPublication pendingNabla
  (withFirstReservation, firstChildIdEffects) <-
    call
      5
      opened
      3
      ( NewIdApplication
          (ControlledNewId (asPrivateNablaId privateNablaIdentity))
      )
      settledNabla
  privateFirstChildIdentity <- completedNewId 3 firstChildIdEffects
  (withReservations, secondChildIdEffects) <-
    call
      6
      opened
      4
      ( NewIdApplication
          (ControlledNewId (asPrivateNablaId privateNablaIdentity))
      )
      withFirstReservation
  privateSecondChildIdentity <- completedNewId 4 secondChildIdEffects
  globalNablaIdentity <-
    checkedIO
      "resolve settled Nabla identity with pristine child reservation"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateNablaIdentity
          (startupApplicationState withReservations)
      )
  globalFirstChildIdentity <-
    checkedIO
      "resolve first pristine child reservation identity"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateFirstChildIdentity
          (startupApplicationState withReservations)
      )
  globalSecondChildIdentity <-
    checkedIO
      "resolve second pristine child reservation identity"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateSecondChildIdentity
          (startupApplicationState withReservations)
      )
  pure
    ( withReservations,
      globalObjectIdFromGlobalUniqueId globalNablaIdentity,
      process,
      [globalFirstChildIdentity, globalSecondChildIdentity]
    )

retainDetachedLabelCandidate ::
  Opened ->
  Word ->
  ApplicationOperation ->
  HeraldState ->
  HeraldState
retainDetachedLabelCandidate opened request operation predecessor =
  replaceStartupApplicationState
    ( Application.commitApplicationDetachedRequest
        ( checked
            "retain detached label candidate"
            (Application.prepareApplicationLabelCandidate candidate)
        )
    )
    predecessor
  where
    candidate =
      case Application.classifyApplicationRequest
        (openedSessionId opened)
        (openedBinding opened)
        (requestId (fromIntegral request))
        operation
        (startupApplicationState predecessor) of
        Right (Application.FirstApplicationRequest retained) -> retained
        _ -> error "expected a first detached label candidate"

terminalRedrivePredecessor :: HeldFixture -> IO (OracleState, HeraldState)
terminalRedrivePredecessor fixture = do
  let predecessor = heldState fixture
      closed = replaceStartupApplicationState (Application.setApplicationMembershipGate True (startupApplicationState predecessor)) predecessor
  (gated, _) <- checkedIO "retain membership-held ingress" (applyApplicationRequest (CallApplicationRequest (openedBinding (heldOpened fixture)) (openedSessionId (heldOpened fixture)) (requestId 6) (NewIdApplication BareNewId)) closed)
  let reopened = replaceStartupApplicationState (Application.setApplicationMembershipGate False (startupApplicationState gated)) gated
  pure (heldOracle fixture, reopened)

data Opened = Opened
  { openedSessionId :: ApplicationSessionId,
    openedBinding :: ApplicationSessionBinding,
    openedToken :: ApplicationResumeToken,
    openedCursor :: ApplicationReplyCursor,
    openedAccess :: ApplicationStartupAccess
  }

openedProcessLabel :: Opened -> ApplicationValue.ApplicationLabel
openedProcessLabel =
  (\owner -> (ApplicationValue.ProcessLabel owner, 0)) . startupAccessProcess . openedAccess

open :: Word -> ApplicationAttachment -> HeraldState -> IO (HeraldState, Opened)
open observed attachment predecessor = do
  (successor, effects) <-
    step
      observed
      ( ApplicationSessionInput
          ( OpenApplicationSession
              (candidateApplicationLane 1)
              attachment
              (Session.clientNonce 1)
          )
      )
      predecessor
  acceptance <- case [value | SetApplicationConnectionDisposition _ value <- effectBatchMembers effects] of
    [value] -> pure value
    values -> assertFailure ("expected one application acceptance, got " <> show (length values))
  case sessionAcceptanceReply acceptance of
    SessionOpened session token access ->
      pure
        ( successor,
          Opened
            session
            (sessionAcceptanceBinding acceptance)
            token
            (sessionAcceptanceCursor acceptance)
            access
        )
    reply -> assertFailure ("expected opened application session, got " <> show reply)

writerFor :: ApplicationPredefinedSortRole -> ApplicationStartupAccess -> IO PrivateNablaId
writerFor role access =
  predefinedWriter
    <$> maybe
      (assertFailure ("startup writer missing for " <> show role))
      pure
      (find ((== role) . predefinedAccessRole) (conventionalStartupPairs access))

readerFor :: ApplicationPredefinedSortRole -> ApplicationStartupAccess -> IO PrivateDeltaId
readerFor role access =
  predefinedReader
    <$> maybe
      (assertFailure ("startup reader missing for " <> show role))
      pure
      (find ((== role) . predefinedAccessRole) (conventionalStartupPairs access))

call ::
  Word ->
  Opened ->
  Word ->
  ApplicationOperation ->
  HeraldState ->
  IO (HeraldState, EffectBatch)
call observed opened request operation =
  step
    observed
    ( ApplicationRequestInput
        ( CallApplicationRequest
            (openedBinding opened)
            (openedSessionId opened)
            (requestId (fromIntegral request))
            operation
        )
    )

step :: Word -> HeraldInputBody -> HeraldState -> IO (HeraldState, EffectBatch)
step observed body state =
  checkedIO
    "live application-label Herald step"
    (verifiedStepHerald (heraldInput (monotonicInstant (fromIntegral observed)) body) state)

commitNextLabelRequest ::
  Word ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState, EffectBatch)
commitNextLabelRequest observed oracle herald = do
  (nextOracle, successor, effects, _) <- commitNextLabelRequestWithEntry observed oracle herald
  pure (nextOracle, successor, effects)

commitNextLabelRequestWithEntry ::
  Word ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState, EffectBatch, CanonicalAppliedOracleEntry)
commitNextLabelRequestWithEntry observed oracle herald = do
  (binding, dispatch) <- case OracleClient.oracleClientLabelRequestActions (startupOracleClientState herald) of
    [SubmitOracleRequest retainedBinding retainedDispatch] -> pure (retainedBinding, retainedDispatch)
    actions -> assertFailure ("expected one live label request action, got " <> show actions)
  commitOracleEnvelopeWithEntry
    observed
    binding
    (canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch))
    oracle
    herald

-- Project one retained label request through the Oracle and local-label
-- owners, stopping at the exact point where the top-level application fallout
-- driver normally runs. This exposes both sides of its progress disposition
-- without manufacturing an unchecked Application owner state.
commitNextLabelRequestBeforeApplicationPass ::
  Word ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState, EffectBatch)
commitNextLabelRequestBeforeApplicationPass observed oracle herald = do
  (binding, dispatch) <- case OracleClient.oracleClientLabelRequestActions (startupOracleClientState herald) of
    [SubmitOracleRequest retainedBinding retainedDispatch] -> pure (retainedBinding, retainedDispatch)
    actions -> assertFailure ("expected one live label request action, got " <> show actions)
  (oracleSuccessor, outcome, oracleEffectsBatch) <-
    checkedIO
      "commit pre-application label request in matching Oracle"
      ( stepOracle
          (canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch))
          oracle
      )
  case outcome of
    OracleCommitted {} -> pure ()
    other -> assertFailure ("expected committed Oracle label request, got " <> show other)
  entry <- case oracleEffects oracleEffectsBatch of
    [EmitAppliedOracleEntry retained] -> pure retained
    effects -> assertFailure ("expected one applied Oracle entry, got " <> show effects)
  let observedState =
        replaceStartupLastObservedTime
          (monotonicInstant (fromIntegral observed))
          herald
      ingress =
        OracleEntriesReceived
          binding
          (canonicalizeAppliedOracleEntry entry :| [])
  (afterIngress, ingressEffects) <-
    checkedIO "apply pre-application Oracle entry" (applyOracleIngress ingress observedState)
  (afterLabel, labelEffects) <-
    checkedIO "advance pre-application label work" (advanceLocalLabelWork afterIngress)
  pure (oracleSuccessor, afterLabel, ingressEffects <> labelEffects)

commitOracleEnvelope ::
  Word ->
  OracleBinding ->
  OracleEnvelope ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState, EffectBatch)
commitOracleEnvelope observed binding envelope oracle herald = do
  (nextOracle, successor, effects, _) <- commitOracleEnvelopeWithEntry observed binding envelope oracle herald
  pure (nextOracle, successor, effects)

-- Fixtures delivering an entry to another receiver retain the original Oracle
-- effect themselves; a completed Herald batch may already reclaim its copy.
commitOracleEnvelopeWithEntry ::
  Word ->
  OracleBinding ->
  OracleEnvelope ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState, EffectBatch, CanonicalAppliedOracleEntry)
commitOracleEnvelopeWithEntry observed binding envelope oracle herald = do
  (oracleSuccessor, outcome, oracleEffectsBatch) <-
    checkedIO
      "commit live label request in matching Oracle"
      (stepOracle envelope oracle)
  case outcome of
    OracleCommitted {} -> pure ()
    other -> assertFailure ("expected committed Oracle label request, got " <> show other)
  entry <- case oracleEffects oracleEffectsBatch of
    [EmitAppliedOracleEntry retained] -> pure retained
    effects -> assertFailure ("expected one applied Oracle entry, got " <> show effects)
  (heraldSuccessor, effects) <-
    step
      observed
      ( OracleInput
          ( OracleEntriesReceived
              binding
              (canonicalizeAppliedOracleEntry entry :| [])
          )
      )
      herald
  pure (oracleSuccessor, heraldSuccessor, effects, canonicalizeAppliedOracleEntry entry)

commitRemoteLabelCommand ::
  Word ->
  HeraldEpoch ->
  OracleCommand ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState, EffectBatch)
commitRemoteLabelCommand observed reporter command oracle herald = do
  (nextOracle, successor, effects, _) <- commitRemoteLabelCommandWithEntry observed reporter command oracle herald
  pure (nextOracle, successor, effects)

commitRemoteLabelCommandWithEntry ::
  Word ->
  HeraldEpoch ->
  OracleCommand ->
  OracleState ->
  HeraldState ->
  IO (OracleState, HeraldState, EffectBatch, CanonicalAppliedOracleEntry)
commitRemoteLabelCommandWithEntry observed reporter command oracle herald = do
  binding <-
    maybe
      (assertFailure "live Oracle client lost its binding")
      pure
      (OracleClient.oracleClientCurrentBinding (startupOracleClientState herald))
  commitOracleEnvelopeWithEntry
    observed
    binding
    ( oracleEnvelope
        (oracleClientRequestId reporter (1000 + fromIntegral observed))
        Nothing
        reporter
        command
    )
    oracle
    herald

data CompleteLabelWorkflowTrace = CompleteLabelWorkflowTrace
  { workflowOracle :: OracleState,
    workflowBeforeDecision :: HeraldState,
    workflowDecided :: HeraldState,
    workflowCompleted :: HeraldState,
    workflowEffects :: [EffectBatch]
  }

commitCompleteLabelWorkflow ::
  Word -> OracleState -> HeraldState -> IO (OracleState, HeraldState, [EffectBatch])
commitCompleteLabelWorkflow first oracle herald =
  case oracleOpenDecisions oracle of
    _ : _ -> commitLabelTerminalCollection first oracle herald
    [] -> do
      trace <- commitCompleteLabelWorkflowTrace first oracle herald
      pure (workflowOracle trace, workflowCompleted trace, workflowEffects trace)

commitCompleteLabelWorkflowTrace ::
  Word -> OracleState -> HeraldState -> IO CompleteLabelWorkflowTrace
commitCompleteLabelWorkflowTrace first oracle herald = do
  (decidedOracle, decided, decisionEffects) <- commitNextLabelRequest first oracle herald
  (completedOracle, completed, completionEffects) <-
    case oracleOpenDecisions decidedOracle of
      [] -> pure (decidedOracle, decided, [])
      _ : _ -> commitLabelTerminalCollection (first + 1) decidedOracle decided
  pure
    CompleteLabelWorkflowTrace
      { workflowOracle = completedOracle,
        workflowBeforeDecision = herald,
        workflowDecided = decided,
        workflowCompleted = completed,
        workflowEffects = decisionEffects : completionEffects
      }

fixtureBarrierDecision :: LabelBarrier.State -> Maybe LabelDecisionId
fixtureBarrierDecision = Set.lookupMin . LabelBarrier.barrierActiveDecisions

activeDecision :: HeraldState -> LabelDecisionId
activeDecision state =
  case Application.applicationActiveLabel (startupApplicationState state) of
    Just active -> Application.activeLabelApplicationDecision active
    Nothing -> error "live label fixture has no active application owner"

fixtureCapturedHeralds :: NonEmpty HeraldEpoch
fixtureCapturedHeralds =
  case [ heraldMemberEpoch member
       | member <- Genesis.checkedActiveHeralds fixtureStep14CheckedGenesis
       ] of
    first : remaining -> first :| remaining
    [] -> error "fixture has no active Herald"

fixtureRemoteHeralds :: [HeraldEpoch]
fixtureRemoteHeralds =
  filter
    (/= Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis)
    (case fixtureCapturedHeralds of first :| remaining -> first : remaining)

-- Single-owner fixtures model remote installation with reports derived from
-- the immutable canonical decision and admit them through peer control.

remoteInstallationReport :: LabelDecisionId -> LiveTerminalOutcome -> HeraldEpoch -> DomainLabel.LabelInstallationReport
remoteInstallationReport decision terminal reporter =
  DomainLabel.labelInstallationReport decision reporter index (deriveLabelOutcomeDigest terminal)
  where
    index = case liveTerminalOutcomeView terminal of
      LiveNotAppliedOutcomeView _ at _ _ _ _ -> at
      LiveReleasedOutcomeView _ _ at _ -> at

deliverInstallationReport :: Word -> DomainLabel.LabelInstallationReport -> HeraldState -> IO (HeraldState, EffectBatch)
deliverInstallationReport observed report state = do
  (connected, binding) <- connectFixturePeer observed (DomainLabel.labelInstallationReporter report) state
  step observed (PeerInput (PeerControlReceived binding (PeerLabelInstalled report))) connected

commitLabelTerminalCollection :: Word -> OracleState -> HeraldState -> IO (OracleState, HeraldState, [EffectBatch])
commitLabelTerminalCollection observed oracle herald = case oracleOpenDecisions oracle of
  [opened] -> commitLabelTerminalCollectionFor (liveDecisionId opened) observed oracle herald
  opened -> assertFailure ("singleton fixture has " <> show (length opened) <> " open labels")

commitLabelTerminalCollectionFor :: LabelDecisionId -> Word -> OracleState -> HeraldState -> IO (OracleState, HeraldState, [EffectBatch])
commitLabelTerminalCollectionFor decision observed oracle herald = do
  (nextOracle, successor, effects, _) <- commitLabelTerminalCollectionForWithEntry decision observed oracle herald
  pure (nextOracle, successor, effects)

commitLabelTerminalCollectionForWithEntry :: LabelDecisionId -> Word -> OracleState -> HeraldState -> IO (OracleState, HeraldState, [EffectBatch], CanonicalAppliedOracleEntry)
commitLabelTerminalCollectionForWithEntry decision observed oracle herald = do
  opened <- maybe (assertFailure "terminal collector has no canonical decision") pure (oracleOpenDecision decision oracle)
  terminal <- maybe (assertFailure "terminal collector has no immutable outcome") pure (oracleTerminalOutcome decision oracle)
  collector <- maybe (assertFailure "terminal collector has no captured survivor") pure (oracleLabelCollector decision oracle)
  capturedHeralds <- maybe (assertFailure "collector decision has no captured membership") pure (liveDecisionCapturedHeralds (OracleProjection.oracleViewHeraldMembershipHistoryChecked (OracleProjection.oracleView (startupOracleProjectionState herald))) opened)
  let local = Genesis.checkedLocalHeraldEpoch (startupGenesis herald)
      membership = oracleCurrentMembership oracle
      active = Set.fromList (toList (heraldMembershipGenerationActiveHeraldEpochs membership))
      captured = Set.fromList capturedHeralds
      survivors = Set.toAscList (Set.intersection captured active)
      generation = heraldMembershipGenerationId membership
  if collector == local
    then do
      (collected, reports) <-
        foldM
          ( \(current, effects) reporter -> do
              (next, batch) <- deliverInstallationReport observed (remoteInstallationReport decision terminal reporter) current
              pure (next, effects <> [batch])
          )
          (herald, [])
          (filter (`Set.member` Collection.outstanding decision (LabelBarrier.barrierCollectionState (startupLabelBarrierState herald))) (filter (/= local) survivors))
      witness <- case [request | (_, request) <- OracleClient.oracleClientRequestEntries (startupOracleClientState collected), OracleClient.CompleteLabelDecisionIntent attestation <- [OracleClient.oracleRequestWitnessIntent request], labelCompletionDecisionId attestation == decision] of
        [request] -> pure request
        requests -> assertFailure ("collector retained " <> show (length requests) <> " completion requests")
      binding <- maybe (assertFailure "collector lost Oracle binding") pure (OracleClient.oracleClientCurrentBinding (startupOracleClientState collected))
      (nextOracle, completed, batch, entry) <- commitOracleEnvelopeWithEntry (observed + 1) binding (canonicalOracleEnvelopeValue (OracleClient.oracleRequestWitnessEnvelope witness)) oracle collected
      pure (nextOracle, completed, reports <> [batch], entry)
    else do
      actual <-
        maybe
          (assertFailure "local participant retained no installation report")
          pure
          (Collection.currentReport decision (LabelBarrier.barrierCollectionState (startupLabelBarrierState herald)))
      remote <-
        checkedIO
          "initialize fixture's remote collector"
          (Collection.begin (remoteInstallationReport decision terminal collector) (liveDecisionHome opened) captured generation active Collection.emptyState)
      collected <-
        foldM
          (\current report -> checkedIO "collect fixture's survivor report" (Collection.observe report current))
          remote
          (actual : [remoteInstallationReport decision terminal member | member <- survivors, member /= local, member /= collector])
      assertEqual "remote collector requires every captured survivor" (Just generation) (Collection.completionReady decision collected)
      let attestation = labelCompletionAttestation decision (DomainLabel.labelInstallationControlIndex actual) (deriveLabelOutcomeDigest terminal) collector generation
      (nextOracle, completed, batch, entry) <- commitRemoteLabelCommandWithEntry (observed + 1) collector (completeLabelDecisionCommand attestation) oracle herald
      pure (nextOracle, completed, [batch], entry)

assertSingleLabelSubmit :: String -> HeraldState -> EffectBatch -> Assertion
assertSingleLabelSubmit context = assertLabelSubmissions context 1

assertLabelSubmissions :: String -> Int -> HeraldState -> EffectBatch -> Assertion
assertLabelSubmissions context expectedCount state effects = do
  let retained =
        OracleClient.oracleClientLabelRequestActions
          (startupOracleClientState state)
      emitted =
        [ action
        | RunOracleClientAction action@SubmitOracleRequest {} <- effectBatchMembers effects
        ]
      maintenance =
        [ action
        | RunOracleClientAction action@ScheduleOracleProgress {} <- effectBatchMembers effects
        ]
  assertEqual (context <> ": emitted retained label action") retained emitted
  assertEqual (context <> ": exact submission count") expectedCount (length emitted)
  assertEqual
    (context <> ": only the pending retirement is scheduled alongside submission")
    [ action
    | action@ScheduleOracleProgress {} <- OracleClient.oracleClientRequestActions (startupOracleClientState state)
    ]
    maintenance
  assertEqual
    (context <> ": no other Oracle action")
    (length emitted + length maintenance)
    (length [() | RunOracleClientAction _ <- effectBatchMembers effects])

assertOwnersUnchangedByHold :: String -> HeraldState -> HeraldState -> Assertion
assertOwnersUnchangedByHold context before after = do
  assertBool (context <> ": Publication") (startupPublicationState before == startupPublicationState after)
  assertBool (context <> ": Store") (startupStoreState before == startupStoreState after)
  assertEqual (context <> ": peer stream") (startupPeerStreamState before) (startupPeerStreamState after)
  assertBool (context <> ": Controlled") (startupControlledState before == startupControlledState after)

exactlyTwoHeld :: HeraldState -> IO [Application.ApplicationFenceHeldView]
exactlyTwoHeld state =
  case Application.applicationFenceHeldEntries (startupApplicationState state) of
    entries@[_, _] -> pure entries
    entries -> assertFailure ("expected two fence-held entries, got " <> show (length entries))

heldWriter :: Application.ApplicationFenceHeldView -> NablaId
heldWriter =
  Application.applicationFenceHeldWorkWriter
    . Application.applicationFenceHeldSemanticWork

heldOrigin :: Application.ApplicationFenceHeldView -> Publication.ApplicationPublicationOrigin
heldOrigin =
  Application.applicationFenceHeldWorkOrigin
    . Application.applicationFenceHeldSemanticWork

heldPayloads ::
  [Application.ApplicationFenceHeldView] ->
  [ ( Request.ProcessAcceptancePosition,
      ApplicationOperation,
      Application.ApplicationFenceHeldWork
    )
  ]
heldPayloads =
  fmap
    ( \view ->
        ( Application.applicationFenceHeldPosition view,
          Application.applicationFenceHeldOperation view,
          Application.applicationFenceHeldSemanticWork view
        )
    )

requiredProcess :: ApplicationSessionId -> Application.State -> ProcessEpochId
requiredProcess session application =
  case Application.applicationSessionProcess session application of
    Just process -> process
    Nothing -> error "held fixture lost its process owner"

assertDetachedRedrive :: Application.State -> Application.ApplicationFenceHeldView -> Assertion
assertDetachedRedrive application view = do
  let (candidate, work) =
        checked
          "reconstruct retired held work"
          ( Application.applicationFenceHeldForRedrive
              (Application.applicationFenceHeldPosition view)
              application
          )
  assertBool
    "retired replay has no reply owner"
    (not (Application.applicationRequestCandidateReplyOwned candidate))
  assertBool
    "retired replay has no delivery path"
    (not (Application.applicationRequestCandidateReplyDeliverable candidate))
  assertEqual
    "retired replay keeps the global semantic payload"
    (Application.applicationFenceHeldSemanticWork view)
    work

assertReleasedPublicationLabel :: HeraldState -> Request.ProcessAcceptancePosition -> Assertion
assertReleasedPublicationLabel state position = do
  record <-
    maybe
      (assertFailure "terminal redrive did not install the held publication")
      pure
      (Publication.lookupApplicationPublication position (startupPublicationState state))
  assertEqual
    "delayed publication refreshes the current released label"
    (Just (VoidLabel, 1))
    ( labelFromValue
        (checkedPublicationValue (Publication.applicationPublicationChecked record))
    )

assertReleasedPublicationProvenance ::
  HeraldState -> Application.ApplicationFenceHeldView -> Assertion
assertReleasedPublicationProvenance state view = do
  let position = Application.applicationFenceHeldPosition view
      held = Application.applicationFenceHeldSemanticWork view
  assertReleasedPublicationLabel state position
  record <-
    maybe
      (assertFailure "terminal redrive did not retain publication provenance")
      pure
      (Publication.lookupApplicationPublication position (startupPublicationState state))
  assertEqual
    "terminal redrive keeps the acceptance-time carrier sort"
    (Application.applicationFenceHeldWorkSortId held)
    (checkedPublicationSort (Publication.applicationPublicationChecked record))
  assertEqual
    "terminal redrive cannot cross the acceptance-time carrier occurrence"
    (Application.applicationFenceHeldWorkSortOccurrenceId held)
    (Publication.applicationPublicationSortOccurrenceId record)
  assertBool
    "terminal redrive preserves the acceptance-time control lower bound"
    ( Publication.applicationPublicationControlPrerequisite record
        >= Application.applicationFenceHeldWorkControlPrerequisite held
    )

labelFromValue :: Value -> Maybe Label
labelFromValue value = do
  field <- either (const Nothing) Just (mkFieldName "label")
  projected <- either (const Nothing) Just (valueAt (directProjection field) value)
  case viewValue projected of
    LabelValue label -> Just label
    _ -> Nothing

assertCanonicalTerminalReplies :: [RetainedRequestReplyBody] -> Assertion
assertCanonicalTerminalReplies replies = case replies of
  [ Completed gatedRequest (NewIdCompleted _),
    Completed writeRequest (WriteCompleted WriteAccepted),
    Completed forwardRequest (ForwardCompleted ForwardAccepted),
    Completed labelRequest (LabelCompleted _)
    ] -> do
      assertEqual "held write completion request" (requestId 4) writeRequest
      assertEqual "held forward completion request" (requestId 5) forwardRequest
      assertEqual "gated completion request" (requestId 6) gatedRequest
      assertEqual "label completion request" (requestId 3) labelRequest
  other -> assertFailure ("unexpected terminal application reply order: " <> show other)

applicationReplyBodies :: EffectBatch -> [RetainedRequestReplyBody]
applicationReplyBodies effects =
  [body | SendApplicationReply _ (RetainedRequestReply _ body) <- effectBatchMembers effects]

completedNewId :: Word -> EffectBatch -> IO PrivateUniqueId
completedNewId expected effects =
  case applicationReplyBodies effects of
    [Completed actual (NewIdCompleted privateIdentity)] -> do
      assertEqual "NewId request" (requestId (fromIntegral expected)) actual
      pure privateIdentity
    actual -> assertFailure ("unexpected NewId replies: " <> show actual)

completedSortDefinition :: Word -> EffectBatch -> IO SortId
completedSortDefinition expected effects =
  case applicationReplyBodies effects of
    [Completed actual (WriteCompleted (SortDefinitionWritten sortId))] -> do
      assertEqual "SortDefinition request" (requestId (fromIntegral expected)) actual
      pure sortId
    actual -> assertFailure ("unexpected SortDefinition replies: " <> show actual)

heldWriteDescriptor :: ApplicationSortDescriptor
heldWriteDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "message" TextSchema),
      keyProjections = [ApplicationProjection ("message" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

heldControlledDescriptor :: ApplicationSortDescriptor
heldControlledDescriptor =
  ApplicationSortDescriptor
    { sortKind = ControlledSort,
      valueSchema =
        RecordSchema
          ( Map.fromList
              [("label", LabelSchema), ("object_id", UniqueIdSchema), ("message", TextSchema)]
          ),
      keyProjections = [ApplicationProjection ("object_id" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Just "label"
    }

heldControlledValue :: PrivateUniqueId -> PrivateProcessId -> Text.Text -> ApplicationValue.ApplicationValue
heldControlledValue object process message =
  ApplicationValue.RecordValue
    ( Map.fromList
        [ ("label", ApplicationValue.LabelValue (ApplicationValue.ProcessLabel process, 0)),
          ("object_id", ApplicationValue.UniqueIdValue object),
          ("message", ApplicationValue.TextValue message)
        ]
    )

removalRecoveryDescriptor :: ApplicationSortDescriptor
removalRecoveryDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "removal_recovery_key" TextSchema),
      keyProjections = [ApplicationProjection ("removal_recovery_key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

unroutedControlledDescriptor :: ApplicationSortDescriptor
unroutedControlledDescriptor =
  ApplicationSortDescriptor
    { sortKind = ControlledSort,
      valueSchema =
        RecordSchema
          ( Map.fromList
              [ ("label", LabelSchema),
                ("object_id", UniqueIdSchema)
              ]
          ),
      keyProjections = [ApplicationProjection ("object_id" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Just "label"
    }

neutralValue :: PrivateUniqueId -> PrivateProcessId -> ApplicationValue.ApplicationValue
neutralValue object process =
  ApplicationValue.RecordValue
    ( Map.fromList
        [ ("label", ApplicationValue.LabelValue ((ApplicationValue.ProcessLabel process, 0))),
          ("object_id", ApplicationValue.UniqueIdValue object)
        ]
    )

dynamicEdgeValue ::
  PrivateUniqueId ->
  PrivateUniqueId ->
  PrivateProcessId ->
  ApplicationValue.ApplicationValue
dynamicEdgeValue object endpoint process =
  dynamicDirectedEdgeValue object endpoint endpoint process

dynamicDirectedEdgeValue ::
  PrivateUniqueId ->
  PrivateUniqueId ->
  PrivateUniqueId ->
  PrivateProcessId ->
  ApplicationValue.ApplicationValue
dynamicDirectedEdgeValue object source destination process =
  ApplicationValue.RecordValue
    ( Map.fromList
        [ ("destination_vertex", ApplicationValue.UniqueIdValue destination),
          ("label", ApplicationValue.LabelValue ((ApplicationValue.ProcessLabel process, 0))),
          ("object_id", ApplicationValue.UniqueIdValue object),
          ("source_vertex", ApplicationValue.UniqueIdValue source),
          ("strength", ApplicationValue.EnumValue "preserve")
        ]
    )

dynamicNablaValue ::
  PrivateUniqueId -> PrivateProcessId -> ApplicationValue.ApplicationValue
dynamicNablaValue object process =
  dynamicNablaValueSequencedBy
    object
    process
    (predefinedCatalogueSortId (profileEntryFor NeutralVertexRole))
    Nothing

selfSequencingDynamicNablaValue ::
  PrivateUniqueId ->
  PrivateProcessId ->
  SortId ->
  ApplicationValue.ApplicationValue
selfSequencingDynamicNablaValue object process sort =
  dynamicNablaValueSequencedBy object process sort (Just object)

dynamicNablaValueSequencedBy ::
  PrivateUniqueId ->
  PrivateProcessId ->
  SortId ->
  Maybe PrivateUniqueId ->
  ApplicationValue.ApplicationValue
dynamicNablaValueSequencedBy object process sort sequencingObject =
  ApplicationValue.RecordValue
    ( Map.fromList
        [ ("label", ApplicationValue.LabelValue ((ApplicationValue.ProcessLabel process, 0))),
          ("object_id", ApplicationValue.UniqueIdValue object),
          ("sequencing_object", ApplicationValue.OptionalUniqueIdValue sequencingObject),
          ( "sort_id",
            ApplicationValue.BytesValue (sortIdBytes sort)
          )
        ]
    )

dynamicDeltaValue ::
  PrivateUniqueId ->
  PrivateProcessId ->
  ApplicationValue.ApplicationValue
dynamicDeltaValue object process =
  dynamicDeltaValueForSort
    object
    process
    (predefinedCatalogueSortId (profileEntryFor NeutralVertexRole))

dynamicDeltaValueForSort ::
  PrivateUniqueId ->
  PrivateProcessId ->
  SortId ->
  ApplicationValue.ApplicationValue
dynamicDeltaValueForSort object process sort =
  ApplicationValue.RecordValue
    ( Map.fromList
        [ ("label", ApplicationValue.LabelValue ((ApplicationValue.ProcessLabel process, 0))),
          ("object_id", ApplicationValue.UniqueIdValue object),
          ( "sort_id",
            ApplicationValue.BytesValue
              (sortIdBytes sort)
          )
        ]
    )

settleStructuralPublication :: HeraldState -> IO HeraldState
settleStructuralPublication predecessor = do
  let initialProgress = startupStructuralProgressState predecessor
      local = Genesis.checkedLocalHeraldEpoch fixtureStep14CheckedGenesis
      remotes =
        [ heraldMemberEpoch member
        | member <- Genesis.checkedActiveHeralds fixtureStep14CheckedGenesis,
          heraldMemberEpoch member /= local
        ]
      reportFor remote progress =
        structuralAppliedReport
          remote
          (GraphProgress.structuralAppliedVector progress)
          (GraphProgress.structuralAppliedControlPrefix progress)
  (reportedProgress, reports) <-
    foldM
      ( \(progress, retained) remote -> do
          let report = reportFor remote progress
          prepared <-
            checkedIO
              "prepare remote structural report"
              (GraphProgress.prepareStructuralReport report progress)
          let (successor, _) = GraphProgress.commitStructuralReport prepared
          pure (successor, retained <> [(remote, report)])
      )
      (initialProgress, [])
      remotes
  let reported = replaceStartupStructuralProgressState reportedProgress predecessor
  (announced, _, prematurelyInstalled) <-
    checkedIO
      "open deterministic topology cut"
      (PeerControl.advanceLocalTopologyCutIfReady reported)
  assertEqual "reports alone cannot install the topology cut" [] prematurelyInstalled
  announce <-
    maybe
      (assertFailure "complete structural report matrix did not open a cut")
      pure
      (GraphProgress.structuralOpenCut (startupStructuralProgressState announced))
  let cut = topologyCutAnnounceId announce
  acceptedProgress <-
    foldM
      ( \progress (_, report) -> do
          prepared <-
            checkedIO
              "prepare remote topology-cut acceptance"
              ( GraphProgress.prepareTopologyCutAcceptance
                  (topologyCutAcceptance cut report)
                  progress
              )
          pure (GraphProgress.commitTopologyCutAcceptance prepared)
      )
      (startupStructuralProgressState announced)
      reports
  established <-
    checkedIO
      "establish accepted topology cut"
      (GraphProgress.prepareTopologyCutEstablishment acceptedProgress)
  let installedProgress = GraphProgress.commitTopologyCutEstablishment established
      membershipId =
        heraldMembershipGenerationId
          ( OracleProjection.oracleViewCurrentHeraldMembership
              (OracleProjection.oracleView (startupOracleProjectionState predecessor))
          )
  acknowledgedProgress <-
    foldM
      ( \progress remote -> do
          prepared <-
            checkedIO
              "prepare remote topology-cut established acknowledgement"
              ( GraphProgress.prepareTopologyCutEstablishedAck
                  (topologyCutEstablishedAck cut remote membershipId)
                  progress
              )
          pure (GraphProgress.commitTopologyCutEstablishedAck prepared)
      )
      installedProgress
      remotes
  let installed =
        replaceStartupStructuralProgressState
          acknowledgedProgress
          announced
  fst
    <$> checkedIO
      "settle initial structural publication"
      (StructuralCoordinator.settleInstalledStructuralCuts [cut] installed)

caseCandidatePromotionAndRejection :: Assertion
caseCandidatePromotionAndRejection = do
  let fixture = applicationFixture
      inserted =
        foldl
          (\state ordinal -> retainLabelCandidate fixture ordinal state)
          (fixtureState fixture)
          [3, 1, 2]
      expectedKeys = fmap (requestKey fixture) [1, 2, 3]
      observedKeys =
        Application.applicationLabelCandidateKey
          <$> Application.applicationLabelCandidateEntries inserted
  assertEqual
    "candidate iteration is canonical rather than insertion ordered"
    expectedKeys
    observedKeys
  let rejectedKey = requestKey fixture 2
      counterBefore = acceptanceCounters inserted
      (afterRejection, rejectionReply) =
        Application.commitApplicationLabelCandidateRejection
          ( checked
              "reject retained candidate"
              ( Application.prepareApplicationLabelCandidateRejection
                  rejectedKey
                  ApplicationLabelTransitionNotPermitted
                  inserted
              )
          )
  assertRejected 2 rejectionReply
  assertEqual
    "rejection removes only the revalidated candidate"
    [requestKey fixture 1, requestKey fixture 3]
    ( Application.applicationLabelCandidateKey
        <$> Application.applicationLabelCandidateEntries afterRejection
    )
  assertEqual
    "candidate rejection consumes no process position"
    counterBefore
    (acceptanceCounters afterRejection)
  let winnerKey = requestKey fixture 1
      reconsidered =
        checked
          "reconstruct canonical candidate"
          ( Application.applicationLabelCandidateForReconsideration
              winnerKey
              afterRejection
          )
  assertEqual
    "reconsideration preserves request identity"
    (requestId 1)
    (Application.applicationRequestCandidateId reconsidered)
  assertEqual
    "a blocked candidate still proposes the first unconsumed position"
    (firstProcessPosition (fixtureProcess fixture) (fixtureState fixture))
    (Application.applicationRequestCandidatePosition reconsidered)
  let (promoted, acceptanceReply) =
        Application.commitApplicationLabelAcceptance
          ( checked
              "promote canonical candidate"
              ( Application.prepareRetainedApplicationLabelAcceptance
                  fixtureDecision
                  (fixtureSelection fixture)
                  winnerKey
                  afterRejection
              )
          )
  assertAccepted 1 acceptanceReply
  assertEqual
    "the canonical eligible winner alone owns the workflow"
    (Just winnerKey)
    (Application.activeLabelApplicationKey <$> Application.applicationActiveLabel promoted)
  assertEqual
    "a later candidate remains detached"
    [requestKey fixture 3]
    ( Application.applicationLabelCandidateKey
        <$> Application.applicationLabelCandidateEntries promoted
    )

data ApplicationFixture = ApplicationFixture
  { fixtureState :: Application.State,
    fixtureProcess :: ProcessEpochId,
    fixturePrivateProcess :: PrivateProcessId,
    fixtureSession :: ApplicationSessionId,
    fixtureBinding :: ApplicationSessionBinding,
    fixturePrivateObject :: PrivateObjectId,
    fixtureSelection :: Application.SequencingSelection
  }

applicationFixture :: ApplicationFixture
applicationFixture =
  ApplicationFixture
    { fixtureState = opened,
      fixtureProcess = process,
      fixturePrivateProcess = Application.bootstrapAccessProcess access,
      fixtureSession = session,
      fixtureBinding = sessionAcceptanceBinding acceptance,
      fixturePrivateObject = asPrivateObjectId (privateNablaUniqueId privateWriter),
      fixtureSelection =
        Application.sequencingSelection
          process
          target
    }
  where
    herald = identity "Herald epoch" mkHeraldEpoch 0x11
    process = identity "process epoch" mkProcessEpochId 0x12
    manifest = identity "bootstrap manifest" mkBootstrapManifestId 0x13
    nabla = identity "writer nabla" mkNablaId 0x14
    target = globalObjectIdFromNablaId nabla
    attachment = Session.applicationAttachmentForBootstrap manifest
    roots =
      concat
        [ [ Application.ApplicationWriterRoot role writer sequencing,
            Application.ApplicationReaderRoot
              role
              (identity "application reader root" mkDeltaId (0x40 + ordinal))
          ]
        | (ordinal, role) <- zip [(0 :: Word8) ..] allPredefinedSortRoles,
          let (writer, sequencing) =
                if role == NeutralVertexRole
                  then (nabla, NablaSequencedBy target)
                  else
                    ( identity
                        "application writer root"
                        mkNablaId
                        (0x30 + ordinal),
                      UnsequencedNabla
                    )
        ]
    (bootstrapped, access) =
      Application.commitApplicationBootstrap
        ( checked
            "bootstrap application owner"
            ( Application.prepareApplicationBootstrap
                attachment
                process
                roots
                fixtureEnvironmentHub
                (fixtureEnvironmentEdges roots)
                Application.emptyState
            )
        )
    privateWriter = case [ (writer, sequencing)
                         | Application.WriterAccess role writer sequencing <-
                             Application.bootstrapAccessRoots access,
                           role == NeutralVertexRole
                         ] of
      [(writer, NablaSequencedBy observed)]
        | observed == target -> writer
      observedRoots ->
        error ("unexpected localized roots: " <> show observedRoots)
    (opened, acceptance) =
      Application.commitApplicationSessionAcceptance
        ( checked
            "open application session"
            ( Application.prepareApplicationSessionOpen
                herald
                attachment
                (Session.clientNonce 1)
                bootstrapped
            )
        )
    session = openedSession acceptance

fixtureDecision :: LabelDecisionId
fixtureDecision = identity "label decision" mkLabelDecisionId 0x21

fixtureLabelOperation :: ApplicationFixture -> ApplicationOperation
fixtureLabelOperation fixture =
  LabelApplication
    (fixturePrivateObject fixture)
    ((ApplicationValue.ProcessLabel (fixturePrivateProcess fixture), 0))
    LabelToVoid

retainLabelCandidate :: ApplicationFixture -> Word -> Application.State -> Application.State
retainLabelCandidate fixture ordinal state =
  Application.commitApplicationDetachedRequest
    ( checked
        "retain label candidate"
        ( Application.prepareApplicationLabelCandidate
            (firstRequest fixture ordinal (fixtureLabelOperation fixture) state)
        )
    )

firstRequest ::
  ApplicationFixture ->
  Word ->
  ApplicationOperation ->
  Application.State ->
  Application.ApplicationRequestCandidate
firstRequest fixture ordinal operation state =
  case Application.classifyApplicationRequest
    (fixtureSession fixture)
    (fixtureBinding fixture)
    (requestId (fromIntegral ordinal))
    operation
    state of
    Right (Application.FirstApplicationRequest candidate) -> candidate
    other -> error ("expected first application request, got " <> classificationName other)

requestKey :: ApplicationFixture -> Word -> Application.ApplicationRequestKey
requestKey fixture ordinal =
  Application.applicationRequestKey
    (fixtureSession fixture)
    (requestId (fromIntegral ordinal))

acceptanceCounters :: Application.State -> [(ProcessEpochId, Word64)]
acceptanceCounters =
  Application.applicationRequestNextAcceptancePositions
    . Application.applicationRequestStateWitness

firstProcessPosition ::
  ProcessEpochId -> Application.State -> Request.ProcessAcceptancePosition
firstProcessPosition process state =
  case acceptanceCounters state of
    [(owner, ordinal)]
      | owner == process -> Request.processAcceptancePosition process ordinal
    positions -> error ("expected one process acceptance counter, got " <> show positions)

openedSession :: ApplicationSessionAcceptance -> ApplicationSessionId
openedSession acceptance = case sessionAcceptanceReply acceptance of
  SessionOpened session _ _ -> session
  reply -> error ("expected opened session, got " <> show reply)

assertRejected :: Word -> ApplicationRequestReply -> Assertion
assertRejected ordinal reply = case reply of
  RetainedRequestReply _ (Rejected request ApplicationLabelTransitionNotPermitted) ->
    assertEqual "rejected request" (requestId (fromIntegral ordinal)) request
  other -> assertFailure ("unexpected candidate rejection reply: " <> show other)

assertAccepted :: Word -> ApplicationRequestReply -> Assertion
assertAccepted ordinal reply = case reply of
  RetainedRequestReply _ (OperationAccepted request LabelSettlementPending) ->
    assertEqual "accepted request" (requestId (fromIntegral ordinal)) request
  other -> assertFailure ("unexpected label acceptance reply: " <> show other)

classificationName ::
  Either problem Application.ApplicationRequestClassification -> String
classificationName classification = case classification of
  Left _ -> "classification failure"
  Right Application.FirstApplicationRequest {} -> "first request"
  Right Application.RetainedApplicationRequest {} -> "retained request"
  Right Application.AttachedApplicationRequest {} -> "attached request"
  Right Application.ConflictingApplicationRequest {} -> "conflicting request"

identity ::
  (Show problem) =>
  String ->
  (ByteString.ByteString -> Either problem identifier) ->
  Word8 ->
  identifier
identity context constructor byte =
  checked context (constructor (ByteString.replicate 32 byte))

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context result = case result of
  Left problem -> assertFailure (context <> ": " <> show problem)
  Right value -> pure value

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
