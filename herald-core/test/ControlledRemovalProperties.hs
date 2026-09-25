{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlledRemovalProperties
  ( tests,
    settledDeltaControlledRemovalFixture,
    emptyOwnerSnapshot,
  )
where

import GenesisFixtures (fixtureRetirementResolution)

import ApplicationLabelProperties
  ( regularDeltaControlledRemovalRecoveryFixture,
    settledDeltaControlledFixtureWithPendingWait,
    settledNablaControlledFixtureWithPristineReservations,
    settledNeutralControlledFixture,
    settledNeutralControlledFixtureWithApplicationRequests,
    settledStructuralControlledFixture,
  )
import Control.Monad (foldM, forM, forM_)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Rejection
  ( ApplicationRejection
      ( ApplicationObjectNotLabelable,
        ApplicationReservationUnavailable
      ),
  )
import Eclips.Application.Types.Result (WaitResult (WaitReady))
import Eclips.Domain.Alignment
  ( HeraldPublicationPrefix (EmptyHeraldPublicationPrefix),
    initialStoreRevision,
  )
import Eclips.Domain.Disappearance
  ( ControlledDisappearanceSubjectProblem (ControlledDisappearanceSubjectIneligibleRole),
    DisappearanceEvidenceClaim,
    DisappearanceProbeId,
    DisappearanceSubject,
    controlledPredefinedDisappearanceSubject,
    deriveDisappearanceProbeId,
    disappearanceSubjectMembershipCoordinate,
    resolveDisappearanceSubject,
  )
import Eclips.Domain.Disappearance qualified as DomainDisappearance
import Eclips.Domain.Graph
  ( VertexId (DeltaVertex, NablaVertex, NeutralVertex),
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    controlIndex,
    controlIndexWord64,
    deltaIdFromGlobalObjectId,
    mkLabelDecisionId,
    mkStructuralSequence,
    nablaIdFromGlobalObjectId,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label
  ( LabelRevision,
    ReleasedLabelStateView (ReleasedDeletedView, ReleasedLabelView),
    derivePreparedLabelDigest,
    labelRecordReleasedState,
    labelRecordRevision,
    mkLabelRevision,
    mkPreparedLabelFacts,
    preparedAuthorityDisposition,
    releasedDeleted,
    releasedLabel,
    releasedLabelStateView,
  )
import Eclips.Domain.Label qualified as DomainLabel
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationSort,
  )
import Eclips.Domain.Route (ReplicaStrength (Normal))
import Eclips.Domain.Sort.Descriptor
  ( ObjectKey,
    StructuralCarrierRole (DeltaCarrier, EdgeCarrier, NablaCarrier, NeutralVertexCarrier, ProcessEpochCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (DeltaRole, EdgeRole, NablaRole, NeutralVertexRole, ProcessEpochRole),
  )
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Domain.StructuralConsequence (StructuralConsequenceCause)
import Eclips.Domain.StructuralConsequence qualified as Consequence
import Eclips.Domain.Value (LabelOwner (ProcessLabel, ZombieLabel))
import Eclips.Herald.Alignment.Disappearance qualified as Alignment
import Eclips.Herald.Alignment.State qualified as AlignmentState
import Eclips.Herald.Application.Disappearance qualified as Application
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (Rejected),
    requestId,
  )
import Eclips.Herald.Application.State qualified as ApplicationState
import Eclips.Herald.Controlled.Disappearance qualified as Controlled
import Eclips.Herald.Controlled.State qualified as ControlledState
import Eclips.Herald.Disappearance.Evidence qualified as Evidence
import Eclips.Herald.Disappearance.Protocol
  ( DisappearancePublicationMarker,
    LocalAbsenceReport,
    ProjectedAbortCause (ProjectedAuthorizedAbort),
    ProjectedDisappearanceProbe,
    ProjectedDisappearanceTerminal (..),
    localAbsenceReportClaim,
    localAbsenceReportForOwner,
    projectedDisappearanceProbe,
  )
import Eclips.Herald.Disappearance.State qualified as DisappearanceState
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (SendApplicationReply, SendApplicationWaitWake),
    effectBatchIsEmpty,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedCatalogueDigest,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Disappearance qualified as GraphDisappearance
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as GraphState
import Eclips.Herald.Initialization
  ( HeraldInvariantFault (HeraldStartupInvariant),
    HeraldState,
    StartupInvariantSubject (StartupStatic),
    StartupInvariantViolation
      ( ControlledTerminalDeletionInvariant,
        PlacementSetInvariant,
        StoreSetInvariant
      ),
  )
import Eclips.Herald.Input
  ( HeraldInputBody (RuntimeObserved),
    RuntimeObservation (ApplicationBindingLost),
    heraldInput,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerStream (SequencedItem)
import Eclips.Herald.PeerStream.Disappearance qualified as PeerStreamDisappearance
import Eclips.Herald.PeerStream.State qualified as PeerStreamState
import Eclips.Herald.Placement.Disappearance qualified as Placement
import Eclips.Herald.Placement.State qualified as PlacementState
import Eclips.Herald.Publication.Disappearance qualified as Publication
import Eclips.Herald.Publication.State qualified as PublicationState
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( heraldPhase,
    replaceStartupControlledState,
    replaceStartupGraphState,
    replaceStartupIdGeneratorState,
    replaceStartupLabelPatchState,
    replaceStartupPlacementState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupAdministrationState,
    startupAlignmentState,
    startupApplicationRecoveryConfiguration,
    startupApplicationState,
    startupConfiguredProcessState,
    startupControlledState,
    startupDiscoveryState,
    startupFailureDetectionState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupInitialBootstraps,
    startupIsolationState,
    startupLabelBarrierState,
    startupLabelPatchState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerLivenessState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
    startupTerminalSourceHoldState,
    startupVisibilityState,
    startupWaitState,
  )
import Eclips.Herald.Store.Disappearance qualified as StoreDisappearance
import Eclips.Herald.Store.State qualified as StoreState
import Eclips.Herald.Structural.Debt qualified as StructuralDebt
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.ApplicationCall qualified as ApplicationCall
import Eclips.Herald.UseCase.ControlledRemoval qualified as ControlledRemoval
import Eclips.Herald.UseCase.Disappearance qualified as DisappearanceUseCase
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Herald.Wait.State qualified as WaitState
import Eclips.Oracle.Canonical (canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Disappearance qualified as OracleDisappearance
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label qualified as Live
import GenesisFixtures
  ( fixtureIdentifierBytes,
    fixtureStep14CheckedGenesis,
  )
import GenesisFixtures qualified as Fixtures
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
    "controlled disappearance removal"
    [ testCase
        "Resolve removes every live owner atomically and exact replay is inert"
        caseComposedControlledRemoval,
      testCase
        "all four eligible structural roles share the terminal removal contract"
        caseAllEligibleStructuralRoles,
      testCase
        "disappearance authority authenticates its exact captured occurrence"
        caseCapturedOccurrenceAuthentication,
      testCase
        "Nabla removal counts and cancels every pristine child reservation"
        caseNablaReservationFallout,
      testCase
        "Delta removal wakes a wait on its passivated Store incarnation"
        caseDeltaPassivationWakesWait,
      testCase
        "Delta removal does not count a wait already removed by connection loss"
        caseDisconnectedDeltaPassivationCompletesWait,
      testCase
        "post-disappearance Label and republish calls cannot resurrect the object"
        casePostDisappearanceApplicationCalls,
      testCase
        "a terminal tombstone rejects restored live structural ownership"
        caseTerminalTombstoneLiveStructuralContradiction,
      testCase
        "matching released-label revision authorizes removal and stale revision is inert"
        caseReleasedLabelRevisionAuthentication,
      testCase
        "label-delete and disappearance share one normalized removal applicator"
        caseLabelDisappearanceDifferential,
      QC.testProperty
        "projected control bases match label, disappearance and End leaf consequences after history reclamation"
        propProjectedControlBase
    ]

-- The reference applies each actual Oracle event through the ordinary
-- Controlled leaf transitions. The candidate reconstructs current facts once,
-- after every exact canonical entry has been reclaimed from Projection.
propProjectedControlBase :: QC.NonNegative Int -> QC.Property
propProjectedControlBase (QC.NonNegative seed) =
  QC.conjoin
    ( [ QC.counterexample ("at canonical cut " <> show (OracleProjection.oracleViewControlIndex (OracleProjection.oracleView projection)))
          $ QC.conjoin
            [ ControlledState.controlledControlBaseFromProjection projection QC.=== Right expected,
              ControlledState.controlledControlBaseFromProjection reclaimed QC.=== Right expected,
              QC.counterexample "canonical prefix remains reclaimed" (null (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness reclaimed)))
            ]
      | (_, projection, controlled) <- history,
        let expected = ControlledState.captureControlledControlBase controlled
            reclaimed = checked "reclaim projected control prefix" (OracleProjection.advanceReplayBase projection >>= OracleProjection.reclaimAppliedPrefixThroughBase Set.empty Nothing)
      ]
        <> [ (ControlledState.controlledTerminalDeletionGeneration <$> ControlledState.controlledTerminalDeletion deletionObject imported) QC.=== Just 1,
             (ControlledState.controlledTerminalDeletionGeneration <$> ControlledState.controlledTerminalDeletion originalObject imported) QC.=== Just 0,
             (ControlledState.controlledTerminalDeletionGeneration <$> ControlledState.controlledTerminalDeletion labelledObject imported) QC.=== Just generations,
             (labelRecordReleasedState <$> ControlledState.controlledReleasedLabelRecord endedObject imported) QC.=== Just (releasedLabel (ProcessLabel caller, 1)),
             ControlledState.controlledEffectiveLabelState endedObject imported QC.=== Just (releasedLabel (ZombieLabel caller, 1)),
             ControlledState.controlledProcessFacts imported QC.=== [],
             ControlledState.controlledLocalRecords imported QC.=== [],
             ControlledState.controlledNormalPossessionEntries imported QC.=== [],
             ControlledState.controlledReservationWitnesses imported QC.=== []
           ]
    )
  where
    generations = fromIntegral (1 + seed `mod` 4)
    initialProjection = Fixtures.fixtureOracleProjectionState
    local = OracleProjection.oracleViewLocalHeraldEpoch (OracleProjection.oracleView initialProjection)
    caller = case [process | process <- OracleProjection.projectedProcessEpochs initialProjection, OracleProjection.oracleViewProcessResidence process (OracleProjection.oracleView initialProjection) == Just local] of
      process : _ -> process
      [] -> error "controlled base fixture has no local process"
    system = OracleProjection.oracleViewSystemId (OracleProjection.oracleView initialProjection)
    initialDigest = OracleProjection.oracleViewInitialProjectionDigest (OracleProjection.oracleView initialProjection)
    object seedByte = checked "controlled base object" (Identity.mkGlobalObjectId (fixtureIdentifierBytes seedByte))
    labelledObject = object 0xe8
    deletionObject = object 0xe9
    originalObject = object 0xea
    endedObject = object 0xeb
    initial = (Live.initialOracleState Fixtures.fixtureCheckedOracleGenesis, initialProjection, ControlledState.emptyState)
    labelled = foldl (\retained generation -> label retained labelledObject generation (DomainLabel.targetProcess caller)) [initial] [0 .. generations - 1]
    -- An outdated expected pair produces NotApplied without replacing the last
    -- released record. The next successful label acts on a different object.
    notApplied = label labelled labelledObject 0 (DomainLabel.targetProcess caller)
    deleted = label notApplied deletionObject 0 DomainLabel.targetDelete
    originalRemoved = disappear deleted originalObject
    labelledRemoved = disappear originalRemoved labelledObject
    beforeEnd = label labelledRemoved endedObject 0 (DomainLabel.targetProcess caller)
    history = append local (checked "controlled base End" (Live.endProcessEpochCommand caller ExplicitAdministrativeEnd)) beforeEnd
    (_, finalProjection, _) = last history
    base = checked "derive imported controls" (ControlledState.controlledControlBaseFromProjection finalProjection)
    imported = ControlledState.commitControlledControlBaseImport (checked "import derived controls" (ControlledState.prepareControlledControlBaseImport base ControlledState.emptyState))

    label retained target generation destination =
      let (oracle, _, _) = last retained
          ordinal = nextOrdinal oracle
          request = oracleClientRequestId local ordinal
          decision = Live.deriveLabelDecisionId system request
          evidence = if generation == 0 then Just (DomainLabel.initialBootstrapLabelEvidence target caller) else Nothing
          acceptance = checked "controlled base acceptance" (DomainLabel.mkLabelProcessAcceptancePosition caller ordinal)
          command = Live.decideLabelCommand decision caller target (ProcessLabel caller, generation) evidence Nothing Nothing destination (DomainLabel.homeLabelAcceptanceCut acceptance EmptyHeraldPublicationPrefix)
          decided = append local command retained
          (afterDecision, _, _) = last decided
       in case Live.oracleOpenDecision decision afterDecision of
            Nothing -> decided
            Just opened ->
              let terminal = maybe (error "released decision lacks terminal") id (Live.oracleTerminalOutcome decision afterDecision)
                  completion = Live.completeLabelDecisionCommand (Live.labelCompletionAttestation decision (Live.liveDecisionRequestControlIndex opened) (Live.deriveLabelOutcomeDigest terminal) local (Live.liveDecisionMembershipGeneration opened))
               in append local completion decided

    disappear retained target =
      let (oracle, _, controlled) = last retained
          membership = Live.oracleCurrentMembership oracle
          occurrence = structuralOccurrenceId local (checked "controlled base occurrence" (mkStructuralSequence (nextOrdinal oracle)))
          revision = labelRecordRevision <$> ControlledState.controlledReleasedLabelRecord target controlled
          subject = checked "controlled base disappearance subject" (controlledPredefinedDisappearanceSubject NeutralVertexRole target occurrence revision)
          coordinate = disappearanceSubjectMembershipCoordinate subject membership
          opened = append local (Live.openDisappearanceProbeCommand subject coordinate) retained
          (afterOpen, _, _) = last opened
          probe = checked "controlled base probe" (deriveDisappearanceProbeId (Live.oracleGreatestControlIndex afterOpen))
          claim reporter = checked "controlled base absence claim" (DomainDisappearance.admitDisappearanceEvidenceClaim subject membership probe reporter (DomainDisappearance.deriveDisappearanceEvidenceDigest (Identity.heraldEpochBytes reporter)))
          claims = map claim (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))
          reported = foldl (\current evidence -> append (DomainDisappearance.disappearanceEvidenceClaimReporter evidence) (Live.reportPredefinedAbsenceCommand probe evidence) current) opened claims
       in append local (Live.resolveDisappearanceProbeCommand probe (OracleDisappearance.completeDisappearanceEvidenceDigest claims)) reported

    nextOrdinal oracle = 1 + controlIndexWord64 (Live.oracleGreatestControlIndex oracle)
    append home command retained =
      let (oracle, projection, controlled) = last retained
          envelope = Live.oracleEnvelope (oracleClientRequestId home (nextOrdinal oracle)) Nothing home command
          (successor, outcome) = Live.submitOracleState envelope oracle
       in case Live.oracleSubmissionOutcomeView outcome of
            Live.OracleSubmissionCommittedView _ entry ->
              let nextProjection = OracleProjection.commitAppliedEntry (checked "project real control event" (OracleProjection.prepareAppliedEntry (canonicalizeAppliedOracleEntry entry) projection))
                  nextControlled = foldl applyEvent controlled (Live.appliedEntryProjectionEvents entry)
               in retained <> [(successor, nextProjection, nextControlled)]
            other -> error ("controlled base command did not commit: " <> show other)

    applyEvent controlled event = case Live.oracleProjectionEventView event of
      Live.LabelDecidedView decision outcome _ -> case Live.liveTerminalOutcomeView outcome of
        Live.LiveReleasedOutcomeView facts _ index overlay ->
          let released = ControlledState.commitControlledLabelRelease (checked "ordinary controlled label release" (ControlledState.prepareControlledLabelRelease initialDigest facts index controlled))
           in if DomainLabel.releasedLabelStateIsDeleted (Live.releasedLabelOverlayState overlay)
                then remove (Live.liveDecisionObject decision) (checked "ordinary label deletion cause" (Consequence.labelReleaseCause (Live.liveDecisionId decision) index)) released
                else released
        _ -> controlled
      Live.ProcessEpochEndedEventView process _ _ -> ControlledState.commitControlledProcessRetirement (ControlledState.prepareControlledProcessRetirement process controlled)
      Live.DisappearanceProbeResolvedView probe outcome -> case DomainDisappearance.disappearanceResolutionOutcomeView outcome of
        DomainDisappearance.ControlledDisappearanceResolved target _ _ index -> remove target (checked "ordinary disappearance cause" (Consequence.predefinedDisappearanceCause probe index)) controlled
        _ -> controlled
      _ -> controlled
    remove target cause controlled = ControlledState.commitControlledRemoval (checked "ordinary controlled terminal removal" (ControlledState.prepareControlledRemoval cause target controlled))

caseComposedControlledRemoval :: Assertion
caseComposedControlledRemoval = do
  (predecessor, object, publisher) <- settledNeutralControlledFixture
  assertEqual "settled predecessor is whole-state valid" (Right ()) (validateHeraldState predecessor)

  let controlledBefore = startupControlledState predecessor
  record <-
    maybe
      (assertFailure "settled Neutral object has no Controlled record")
      pure
      (ControlledState.controlledLocalRecord object controlledBefore)
  occurrence <-
    maybe
      (assertFailure "settled Neutral object has no structural occurrence")
      pure
      ( Map.lookup
          object
          (GraphState.graphStructuralVertexProjections (startupGraphState predecessor))
          >>= Reconciliation.structuralVertexProjectionOccurrence
      )
  let publication = ControlledState.controlledRecordLatestPublication record
      payloadSort = checkedPublicationSort publication
      payloadKey = checkedPublicationKey publication
      storeBefore = startupStoreState predecessor
      carryingSlots =
        filter
          (slotCarries payloadSort payloadKey)
          (StoreState.storeSlots storeBefore)
  carryingSlot <- case carryingSlots of
    first : _ -> pure first
    [] -> assertFailure "settled Neutral payload reached no active local Store"
  assertBool
    "the publisher initially has direct possession"
    (ControlledState.controlledHasDirectPossession publisher object controlledBefore)
  assertBool
    "the publisher initially has effective Normal possession"
    (ControlledState.controlledHasNormalPossession publisher object controlledBefore)
  assertBool
    "the Neutral vertex is initially projected"
    (GraphState.graphHasVertex (NeutralVertex object) (startupGraphState predecessor))

  let membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState predecessor))
      subject =
        checked
          "controlled-removal subject"
          ( controlledPredefinedDisappearanceSubject
              NeutralVertexRole
              object
              occurrence
              Nothing
          )
      probe = checked "controlled-removal probe" (deriveDisappearanceProbeId (controlIndex 7301))
      resolveIndex = controlIndex 7302
  (readyLeaf, _) <- reportCompleteLeaf probe subject membership predecessor
  let outcome =
        checked
          "controlled-removal Resolve outcome"
          ( resolveDisappearanceSubject
              (checkedSystemId (startupGenesis predecessor))
              resolveIndex
              subject
          )
      terminal = ProjectedDisappearanceResolved probe outcome resolveIndex
      authority = freshAuthority terminal readyLeaf

  assertAbortCannotEnterRemoval terminal readyLeaf predecessor
  assertOwnerMismatchCannotEnterRemoval terminal membership predecessor
  assertMembershipMismatchCannotEnterRemoval subject object membership predecessor
  case DisappearanceUseCase.coordinateProjectedTerminal terminal readyLeaf of
    Left
      ( DisappearanceUseCase.DisappearanceControlledResolutionRequiresOwnerCoordination
          actualProbe
        ) ->
        assertEqual
          "the leaf-only coordinator identifies the controlled Resolve probe"
          probe
          actualProbe
    Left problem ->
      assertFailure
        ("unexpected leaf-only controlled Resolve rejection: " <> show problem)
    Right _ ->
      assertFailure "the leaf-only coordinator bypassed atomic owner removal"

  (resolvedLeaf, successor, effects, coordination) <-
    checkedIO
      "coordinate fresh controlled disappearance removal"
      ( DisappearanceUseCase.coordinateProjectedControlledResolution
          terminal
          readyLeaf
          predecessor
      )
  assertEqual
    "the leaf classifies the first Resolve as fresh"
    DisappearanceState.ProjectedControlledResolutionFresh
    (DisappearanceUseCase.projectedControlledResolutionCoordinationDisposition coordination)
  summary <-
    maybe
      (assertFailure "fresh controlled Resolve returned no composed summary")
      pure
      (DisappearanceUseCase.projectedControlledResolutionCoordinationSummary coordination)
  assertEqual
    "the composed removal is applied"
    ControlledRemoval.ControlledRemovalApplied
    (ControlledRemoval.controlledRemovalSummaryDisposition summary)
  assertEqual "the summary names the exact object" object (ControlledRemoval.controlledRemovalSummaryObject summary)
  assertEqual
    "the summary retains the authenticated Neutral role"
    (Just NeutralVertexCarrier)
    (ControlledRemoval.controlledRemovalSummaryRole summary)
  assertBool
    "the fixture exercises an actual retained Store purge"
    (ControlledRemoval.controlledRemovalSummaryStorePurgedFacts summary > 0)

  assertEqual "the resolved disappearance leaf remains valid" (Right ()) (DisappearanceState.validateState resolvedLeaf)
  assertEqual "the removed whole Herald remains valid" (Right ()) (validateHeraldState successor)
  assertBool
    "detached Resolve advances the structural control prefix through its index"
    ( GraphProgress.structuralAppliedControlPrefix
        (startupStructuralProgressState successor)
        >= resolveIndex
    )
  let controlledAfter = startupControlledState successor
  deletion <-
    maybe
      (assertFailure "controlled removal installed no terminal tombstone")
      pure
      (ControlledState.controlledTerminalDeletion object controlledAfter)
  assertEqual
    "the tombstone names the removed object"
    object
    (ControlledState.controlledTerminalDeletionObject deletion)
  assertEqual
    "the live Controlled record is purged"
    Nothing
    (ControlledState.controlledLocalRecord object controlledAfter)
  assertBool
    "direct publisher possession is revoked"
    (not (ControlledState.controlledHasDirectPossession publisher object controlledAfter))
  assertBool
    "no effective Normal possession survives"
    (not (ControlledState.controlledHasAnyNormalPossession object controlledAfter))
  assertBool
    "the structural Neutral vertex is removed"
    (not (GraphState.graphHasVertex (NeutralVertex object) (startupGraphState successor)))
  assertEqual
    "the current structural projection is removed"
    Nothing
    (Map.lookup object (GraphState.graphStructuralVertexProjections (startupGraphState successor)))

  let storeAfter = startupStoreState successor
      matchingSlots =
        filter
          ((== payloadSort) . StoreState.storeSlotSortId)
          (StoreState.retainedStoreSlots storeAfter)
      cause = ControlledState.controlledTerminalDeletionCause deletion
      falloutTerms =
        observableRemovalFallout
          object
          payloadSort
          payloadKey
          cause
          predecessor
          successor
          effects
      expectedFallout = sum (snd <$> falloutTerms)
  assertEqual
    "the summary F independently matches observable owner fallout"
    expectedFallout
    (ControlledRemoval.controlledRemovalSummaryFalloutCount summary)
  assertEqual
    "Increment-7 work is independently exactly 4 + F"
    (4 + expectedFallout)
    (ControlledRemoval.controlledRemovalSummaryLogicalWork summary)
  assertPositiveFallout "purged Store facts" falloutTerms
  assertPositiveFallout "revoked direct possessions" falloutTerms
  assertPositiveFallout "graph changes" falloutTerms
  assertBool "the payload sort retains at least one Store resource" (not (null matchingSlots))
  assertBool
    "every Store of the payload sort suppresses the terminal object"
    ( all
        (\slot -> StoreState.storeSlotTerminalObjectPurgeCause payloadKey slot == Just cause)
        matchingSlots
    )
  assertBool
    "no visible or retained payload survives in any matching Store"
    (all (slotOmits payloadKey) matchingSlots)
  assertStoreSuppressesStalePublication carryingSlot publication storeAfter

  directReplayPrepared <-
    checkedIO
      "prepare exact shared-applicator replay"
      (ControlledRemoval.prepareDisappearanceControlledRemoval authority successor)
  let (directReplayHerald, directReplayEffects, directReplaySummary) =
        ControlledRemoval.commitControlledRemoval directReplayPrepared
  assertEqual
    "shared-applicator replay is classified exact"
    ControlledRemoval.ControlledRemovalExactReplay
    (ControlledRemoval.controlledRemovalSummaryDisposition directReplaySummary)
  assertBool "shared-applicator replay preserves the Herald bit-for-bit" (successor == directReplayHerald)
  assertBool "shared-applicator replay emits no effect" (effectBatchIsEmpty directReplayEffects)
  assertEqual
    "shared-applicator replay reports zero fallout"
    0
    (ControlledRemoval.controlledRemovalSummaryFalloutCount directReplaySummary)
  assertEqual
    "shared-applicator replay reports zero logical work"
    0
    (ControlledRemoval.controlledRemovalSummaryLogicalWork directReplaySummary)

  (replayedLeaf, replayedHerald, replayedEffects, replayedCoordination) <-
    checkedIO
      "replay exact controlled disappearance removal"
      ( DisappearanceUseCase.coordinateProjectedControlledResolution
          terminal
          resolvedLeaf
          successor
      )
  assertEqual
    "exact replay is classified at the disappearance leaf"
    DisappearanceState.ProjectedControlledResolutionDuplicate
    ( DisappearanceUseCase.projectedControlledResolutionCoordinationDisposition
        replayedCoordination
    )
  assertEqual
    "exact replay cannot reuse removal authority"
    Nothing
    (DisappearanceUseCase.projectedControlledResolutionCoordinationSummary replayedCoordination)
  assertBool "exact replay preserves the leaf bit-for-bit" (resolvedLeaf == replayedLeaf)
  assertBool "exact replay preserves the Herald bit-for-bit" (successor == replayedHerald)
  assertBool "exact replay emits no effect" (effectBatchIsEmpty replayedEffects)
  assertEqual "exact replay remains whole-state valid" (Right ()) (validateHeraldState replayedHerald)

  assertEqual
    "the report-complete leaf uses the Herald's current membership coordinate"
    (disappearanceSubjectMembershipCoordinate subject membership)
    (DisappearanceState.controlledRemovalAuthorityCoordinate authority)
  assertBool
    "the setup captured more than one member"
    (length (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)) > 1)

data EligibleRoleCase
  = EligibleRoleCase
      Access.ApplicationPredefinedSortRole
      PredefinedSortRole
      StructuralCarrierRole

eligibleRoleCases :: [EligibleRoleCase]
eligibleRoleCases =
  [ EligibleRoleCase Access.NeutralVertexRole NeutralVertexRole NeutralVertexCarrier,
    EligibleRoleCase Access.EdgeRole EdgeRole EdgeCarrier,
    EligibleRoleCase Access.NablaRole NablaRole NablaCarrier,
    EligibleRoleCase Access.DeltaRole DeltaRole DeltaCarrier
  ]

caseAllEligibleStructuralRoles :: Assertion
caseAllEligibleStructuralRoles = do
  results <-
    sequence
      [ exerciseEligibleRole (7400 + offset * 10) roleCase
      | (offset, roleCase) <- zip [0 ..] eligibleRoleCases
      ]
  (object, occurrence, _, _) <- case results of
    first : _ -> pure first
    [] -> assertFailure "controlled-removal eligible-role table is empty"
  case controlledPredefinedDisappearanceSubject
    ProcessEpochRole
    object
    occurrence
    Nothing of
    Left (ControlledDisappearanceSubjectIneligibleRole ProcessEpochRole) -> pure ()
    Left problem -> assertFailure ("unexpected ProcessEpoch subject rejection: " <> show problem)
    Right _ -> assertFailure "ProcessEpoch entered controlled disappearance"

caseCapturedOccurrenceAuthentication :: Assertion
caseCapturedOccurrenceAuthentication = do
  (predecessor, object, _) <- settledNeutralControlledFixture
  occurrence <-
    maybe
      (assertFailure "settled Neutral object has no current structural occurrence")
      pure
      (currentStructuralOccurrence NeutralVertexCarrier object predecessor)
  let fakeOccurrence =
        structuralOccurrenceId
          (structuralOccurrenceSourceHeraldEpoch occurrence)
          (checked "absent structural sequence" (mkStructuralSequence 7899))
      reconciliation =
        GraphProgress.structuralProgressReconciliation
          (startupStructuralProgressState predecessor)
  assertBool
    "the forged captured occurrence is absent from immutable structural history"
    (not (Reconciliation.structuralAppliedHasOccurrence fakeOccurrence reconciliation))
  assertBool
    "the real current occurrence remains available for the same live object"
    (Reconciliation.structuralAppliedHasOccurrence occurrence reconciliation)

  let membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState predecessor))
      subject =
        checked
          "absent-occurrence disappearance subject"
          ( controlledPredefinedDisappearanceSubject
              NeutralVertexRole
              object
              fakeOccurrence
              Nothing
          )
      probe =
        checked
          "absent-occurrence disappearance probe"
          (deriveDisappearanceProbeId (controlIndex 7891))
      resolveIndex = controlIndex 7892
  (readyLeaf, _) <- reportCompleteLeaf probe subject membership predecessor
  let terminal =
        ProjectedDisappearanceResolved
          probe
          ( checked
              "absent-occurrence Resolve outcome"
              ( resolveDisappearanceSubject
                  (checkedSystemId (startupGenesis predecessor))
                  resolveIndex
                  subject
              )
          )
          resolveIndex
      authority = freshAuthority terminal readyLeaf
  assertEqual
    "the opaque authority retains the exact absent occurrence"
    fakeOccurrence
    (DisappearanceState.controlledRemovalAuthorityStructuralOccurrence authority)
  case ControlledRemoval.prepareDisappearanceControlledRemoval authority predecessor of
    Left (ControlledRemoval.ControlledRemovalCapturedOccurrenceMissing actual) ->
      assertEqual
        "occurrence authentication rejects the exact absent identity"
        fakeOccurrence
        actual
    Left problem ->
      assertFailure ("unexpected captured-occurrence rejection: " <> show problem)
    Right _ ->
      assertFailure
        "the applicator authenticated a live object instead of the authority's captured occurrence"
  assertEqual
    "failed occurrence authentication leaves the live projection current"
    (Just occurrence)
    (currentStructuralOccurrence NeutralVertexCarrier object predecessor)
  assertBool
    "failed occurrence authentication leaves the live Controlled record present"
    (isJust (ControlledState.controlledLocalRecord object (startupControlledState predecessor)))
  assertEqual
    "failed occurrence authentication installs no terminal tombstone"
    Nothing
    (ControlledState.controlledTerminalDeletion object (startupControlledState predecessor))
  assertEqual
    "failed occurrence authentication leaves the whole Herald valid"
    (Right ())
    (validateHeraldState predecessor)

caseNablaReservationFallout :: Assertion
caseNablaReservationFallout = do
  fixture <- settledNablaControlledFixtureWithPristineReservations
  let (_, _, _, reservations) = fixture
  assertEqual
    "the composed Nabla fixture retains two pristine child reservations"
    2
    (length reservations)
  _ <-
    exerciseEligibleRoleFixture
      7860
      (EligibleRoleCase Access.NablaRole NablaRole NablaCarrier)
      fixture
  pure ()

caseDeltaPassivationWakesWait :: Assertion
caseDeltaPassivationWakesWait = do
  (predecessor, object, _publisher, wait, binding) <-
    settledDeltaControlledFixtureWithPendingWait
  assertBool
    "the application wait is genuinely pending before disappearance"
    (isJust (WaitState.lookupWaitRegistration wait (startupWaitState predecessor)))
  assertBool
    "the controlled Delta has an active Store before disappearance"
    ( isJust
        ( StoreState.lookupStoreSlot
            (deltaIdFromGlobalObjectId object)
            (startupStoreState predecessor)
        )
    )
  (publication, resolvedLeaf, terminal, successor, effects, summary) <-
    resolvePendingWaitDelta 7871 7872 predecessor object
  let payloadSort = checkedPublicationSort publication
      payloadKey = checkedPublicationKey publication
  assertEqual
    "Delta passivation emits exactly its pending wait wake"
    [(binding, requestId 3, wait, WaitReady)]
    [ (actualBinding, actualRequest, actualWait, result)
    | SendApplicationWaitWake
        actualBinding
        _cursor
        actualRequest
        actualWait
        result <-
        effectBatchMembers effects
    ]
  assertEqual
    "the wake atomically removes the exact registration"
    Nothing
    (WaitState.lookupWaitRegistration wait (startupWaitState successor))
  assertEqual
    "the pending-wait removal successor remains whole-state valid"
    (Right ())
    (validateHeraldState successor)
  deletion <-
    maybe
      (assertFailure "pending-wait Delta removal installed no tombstone")
      pure
      (ControlledState.controlledTerminalDeletion object (startupControlledState successor))
  let falloutTerms =
        observableRemovalFallout
          object
          payloadSort
          payloadKey
          (ControlledState.controlledTerminalDeletionCause deletion)
          predecessor
          successor
          effects
      nonWakeFallout =
        sum
          [ count
          | (name, count) <- falloutTerms,
            name /= "completed wait registrations"
          ]
  assertEqual
    "the independently observed wait contribution is one"
    (Just 1)
    (lookup "completed wait registrations" falloutTerms)
  assertEqual
    "the summary F is the independent non-wait fallout plus one completion"
    (nonWakeFallout + 1)
    (ControlledRemoval.controlledRemovalSummaryFalloutCount summary)
  assertEqual
    "pending-wait removal work remains exactly 4 + F"
    (5 + nonWakeFallout)
    (ControlledRemoval.controlledRemovalSummaryLogicalWork summary)

  (_, replayedHerald, replayedEffects, replayedCoordination) <-
    checkedIO
      "replay pending-wait Delta removal"
      ( DisappearanceUseCase.coordinateProjectedControlledResolution
          terminal
          resolvedLeaf
          successor
      )
  assertEqual
    "the exact replay returns no reusable removal summary"
    Nothing
    (DisappearanceUseCase.projectedControlledResolutionCoordinationSummary replayedCoordination)
  assertBool "the exact replay preserves the Herald" (replayedHerald == successor)
  assertBool "the exact replay emits no second wake" (effectBatchIsEmpty replayedEffects)

caseDisconnectedDeltaPassivationCompletesWait :: Assertion
caseDisconnectedDeltaPassivationCompletesWait = do
  (predecessor, object, _publisher, wait, binding) <-
    settledDeltaControlledFixtureWithPendingWait
  (disconnected, _) <-
    checkedIO
      "disconnect pending-wait application session"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 7873)
              (RuntimeObserved (ApplicationBindingLost binding))
          )
          predecessor
      )
  assertEqual
    "connection loss immediately removes its application wait"
    Nothing
    (WaitState.lookupWaitRegistration wait (startupWaitState disconnected))
  (publication, _resolvedLeaf, _terminal, successor, effects, summary) <-
    resolvePendingWaitDelta 7874 7875 disconnected object
  assertEqual
    "later Delta removal emits no wake for the dead application"
    []
    [ ()
    | SendApplicationWaitWake {} <- effectBatchMembers effects
    ]
  assertEqual
    "later Delta removal keeps the dead wait absent"
    Nothing
    (WaitState.lookupWaitRegistration wait (startupWaitState successor))
  assertEqual
    "later Delta removal preserves the whole-state invariant"
    (Right ())
    (validateHeraldState successor)
  deletion <-
    maybe
      (assertFailure "disconnected pending-wait removal installed no tombstone")
      pure
      (ControlledState.controlledTerminalDeletion object (startupControlledState successor))
  let falloutTerms =
        observableRemovalFallout
          object
          (checkedPublicationSort publication)
          (checkedPublicationKey publication)
          (ControlledState.controlledTerminalDeletionCause deletion)
          disconnected
          successor
          effects
      nonWaitFallout =
        sum
          [ count
          | (name, count) <- falloutTerms,
            name /= "completed wait registrations"
          ]
  assertEqual
    "the already removed wait contributes no new fallout"
    (Just 0)
    (lookup "completed wait registrations" falloutTerms)
  assertEqual
    "F includes only semantic fallout that remains after connection loss"
    nonWaitFallout
    (ControlledRemoval.controlledRemovalSummaryFalloutCount summary)
  assertEqual
    "post-loss removal work remains exactly 4 + F"
    (4 + nonWaitFallout)
    (ControlledRemoval.controlledRemovalSummaryLogicalWork summary)

resolvePendingWaitDelta ::
  Word64 ->
  Word64 ->
  HeraldState ->
  GlobalObjectId ->
  IO
    ( CheckedPublication,
      DisappearanceState.State,
      ProjectedDisappearanceTerminal,
      HeraldState,
      EffectBatch,
      ControlledRemoval.ControlledRemovalSummary
    )
resolvePendingWaitDelta probeWord resolveWord predecessor object = do
  record <-
    maybe
      (assertFailure "pending-wait Delta has no Controlled record")
      pure
      (ControlledState.controlledLocalRecord object (startupControlledState predecessor))
  occurrence <-
    maybe
      (assertFailure "pending-wait Delta has no current structural occurrence")
      pure
      (currentStructuralOccurrence DeltaCarrier object predecessor)
  let publication = ControlledState.controlledRecordLatestPublication record
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState predecessor))
      subject =
        checked
          "pending-wait Delta disappearance subject"
          ( controlledPredefinedDisappearanceSubject
              DeltaRole
              object
              occurrence
              Nothing
          )
      probe =
        checked
          "pending-wait Delta disappearance probe"
          (deriveDisappearanceProbeId (controlIndex probeWord))
      resolveIndex = controlIndex resolveWord
  (readyLeaf, _) <- reportCompleteLeaf probe subject membership predecessor
  let terminal =
        ProjectedDisappearanceResolved
          probe
          ( checked
              "pending-wait Delta Resolve outcome"
              ( resolveDisappearanceSubject
                  (checkedSystemId (startupGenesis predecessor))
                  resolveIndex
                  subject
              )
          )
          resolveIndex
  (resolvedLeaf, successor, effects, coordination) <-
    checkedIO
      "coordinate pending-wait Delta removal"
      ( DisappearanceUseCase.coordinateProjectedControlledResolution
          terminal
          readyLeaf
          predecessor
      )
  summary <-
    maybe
      (assertFailure "pending-wait Delta removal returned no summary")
      pure
      (DisappearanceUseCase.projectedControlledResolutionCoordinationSummary coordination)
  pure (publication, resolvedLeaf, terminal, successor, effects, summary)

casePostDisappearanceApplicationCalls :: Assertion
casePostDisappearanceApplicationCalls = do
  (predecessor, object, publisher, labelIngress, republishIngress) <-
    settledNeutralControlledFixtureWithApplicationRequests
  (_, _, _, removed) <-
    exerciseEligibleRoleFixture
      7880
      (EligibleRoleCase Access.NeutralVertexRole NeutralVertexRole NeutralVertexCarrier)
      (predecessor, object, publisher, [])

  let replyBodies effects =
        [ body
        | SendApplicationReply _ (RetainedRequestReply _ body) <-
            effectBatchMembers effects
        ]
      assertRemovalPreserved context successor = do
        assertBool
          (context <> ": Controlled owner remains terminal")
          (startupControlledState removed == startupControlledState successor)
        assertBool
          (context <> ": graph owner cannot recreate the vertex")
          (startupGraphState removed == startupGraphState successor)
        assertBool
          (context <> ": Store owner cannot recreate the payload")
          (startupStoreState removed == startupStoreState successor)
        assertBool
          (context <> ": placement owner remains removed")
          (startupPlacementState removed == startupPlacementState successor)
        assertBool
          (context <> ": structural progress remains removed")
          (startupStructuralProgressState removed == startupStructuralProgressState successor)
        assertBool
          (context <> ": publication owner is unchanged")
          (startupPublicationState removed == startupPublicationState successor)
        assertBool
          (context <> ": peer stream is unchanged")
          (startupPeerStreamState removed == startupPeerStreamState successor)
        assertBool
          (context <> ": terminal tombstone remains authoritative")
          ( ControlledState.controlledObjectTerminallyDeleted
              object
              (startupControlledState successor)
          )
        assertEqual
          (context <> ": successor remains whole-state valid")
          (Right ())
          (validateHeraldState successor)

  (afterLabel, labelEffects) <-
    checkedIO
      "apply a real Label request after controlled disappearance"
      (ApplicationCall.applyApplicationRequest labelIngress removed)
  case replyBodies labelEffects of
    [Rejected actual (ApplicationObjectNotLabelable _)] ->
      assertEqual
        "post-disappearance Label rejection names its fresh request"
        (requestId 3)
        actual
    replies ->
      assertFailure
        ("unexpected post-disappearance Label replies: " <> show replies)
  assertRemovalPreserved "post-disappearance Label" afterLabel

  (afterRepublish, republishEffects) <-
    checkedIO
      "apply a real republish request after controlled disappearance"
      (ApplicationCall.applyApplicationRequest republishIngress removed)
  case replyBodies republishEffects of
    [Rejected actual (ApplicationReservationUnavailable _ _)] ->
      assertEqual
        "post-disappearance republish rejection names its fresh request"
        (requestId 3)
        actual
    replies ->
      assertFailure
        ("unexpected post-disappearance republish replies: " <> show replies)
  assertRemovalPreserved "post-disappearance republish" afterRepublish

caseTerminalTombstoneLiveStructuralContradiction :: Assertion
caseTerminalTombstoneLiveStructuralContradiction = do
  (object, _, predecessor, successor) <-
    exerciseEligibleRole 7900 (EligibleRoleCase Access.DeltaRole DeltaRole DeltaCarrier)
  deletion <-
    maybe
      (assertFailure "Delta removal retained no terminal tombstone")
      pure
      ( ControlledState.controlledTerminalDeletion
          object
          (startupControlledState successor)
      )
  resource <-
    case filter
      ((== object) . Reconciliation.dynamicStoreController)
      ( Map.elems
          ( Reconciliation.structuralAppliedRetainedStoreResources
              ( GraphProgress.structuralProgressReconciliation
                  (startupStructuralProgressState successor)
              )
          )
      ) of
      [retained] -> pure retained
      retained ->
        assertFailure
          ( "expected one retained Delta resource for the tombstone, got "
              <> show (length retained)
          )
  let delta = Reconciliation.dynamicStoreDelta resource
      record =
        requiredControlledRecord
          "pre-removal Delta"
          object
          (startupControlledState predecessor)
      publication = ControlledState.controlledRecordLatestPublication record
      cause = ControlledState.controlledTerminalDeletionCause deletion
      restoredProgress =
        GraphProgress.restoreStructuralLocalStoreForInvariantTest
          resource
          (startupStructuralProgressState successor)
  preparedPurge <-
    checkedIO
      "purge the restored active Store without passivating it"
      ( StoreState.prepareControlledObjectPurge
          cause
          (checkedPublicationSort publication)
          (checkedPublicationKey publication)
          (startupStoreState predecessor)
      )
  let (restoredStore, _) = StoreState.commitControlledObjectPurge preparedPurge
      missingPurgeStore =
        StoreState.removeTerminalObjectPurgeForInvariantTest
          (checkedPublicationSort publication)
          (checkedPublicationKey publication)
          (startupStoreState successor)
      restoredGraphOnly =
        replaceStartupGraphState (startupGraphState predecessor) successor
      restoredProgressOnly =
        replaceStartupStructuralProgressState restoredProgress successor
      restoredPlacementOnly =
        replaceStartupPlacementState (startupPlacementState predecessor) successor
      restoredStoreOnly = replaceStartupStoreState restoredStore successor
      missingPurgeLedgerOnly =
        replaceStartupStoreState missingPurgeStore successor
      corrupted =
        replaceStartupStructuralProgressState restoredProgress
          . replaceStartupPlacementState (startupPlacementState predecessor)
          . replaceStartupStoreState restoredStore
          $ successor
      independentlyCorrupted =
        [ ( "restored Graph projection",
            restoredGraphOnly,
            ControlledTerminalDeletionInvariant
          ),
          ( "restored active structural resource",
            restoredProgressOnly,
            PlacementSetInvariant
          ),
          ( "restored controller-qualified placement",
            restoredPlacementOnly,
            PlacementSetInvariant
          ),
          ( "restored active Store",
            restoredStoreOnly,
            StoreSetInvariant
          ),
          ( "restored owner-coherent resource set",
            corrupted,
            ControlledTerminalDeletionInvariant
          ),
          ( "missing authoritative Store purge ledger",
            missingPurgeLedgerOnly,
            ControlledTerminalDeletionInvariant
          )
        ]
  assertEqual
    "the coherently omitted purge leaves Store history internally valid"
    (Right ())
    (StoreState.validateStoreHistoryState missingPurgeStore)
  assertEqual
    "the Store owner no longer retains the tombstone's purge coordinate"
    Nothing
    ( lookup
        (checkedPublicationSort publication, checkedPublicationKey publication)
        (StoreState.storeTerminalObjectPurgeEntries missingPurgeStore)
    )
  assertEqual
    "the retained resource is active in structural progress"
    [resource]
    ( filter
        ((== object) . Reconciliation.dynamicStoreController)
        ( Reconciliation.structuralAppliedLocalStores
            ( GraphProgress.structuralProgressReconciliation
                (startupStructuralProgressState corrupted)
            )
        )
    )
  assertEqual
    "the independently restored structural resource is live"
    [resource]
    ( filter
        ((== object) . Reconciliation.dynamicStoreController)
        ( Reconciliation.structuralAppliedLocalStores
            ( GraphProgress.structuralProgressReconciliation
                (startupStructuralProgressState restoredProgressOnly)
            )
        )
    )
  assertBool
    "the controller-qualified placement was restored"
    ( maybe
        False
        ((== object) . PlacementState.localPlacementControllerObject)
        (PlacementState.lookupLocalPlacement delta (startupPlacementState corrupted))
    )
  assertBool
    "the semantically related Store is active despite retaining the purge"
    (isJust (StoreState.lookupStoreSlot delta (startupStoreState corrupted)))
  forM_ independentlyCorrupted $ \(context, state, violation) ->
    assertEqual
      ("whole-state validation rejects the " <> context)
      ( Left
          ( HeraldStartupInvariant
              StartupStatic
              violation
          )
      )
      (validateHeraldState state)

caseReleasedLabelRevisionAuthentication :: Assertion
caseReleasedLabelRevisionAuthentication = do
  (unlabelled, object, _) <- settledNeutralControlledFixture
  (predecessor, revision) <- installMatchingReleasedLabel object unlabelled
  assertEqual
    "the released-label predecessor is whole-state valid"
    (Right ())
    (validateHeraldState predecessor)
  occurrence <-
    maybe
      (assertFailure "released-label fixture has no current structural occurrence")
      pure
      ( Map.lookup
          object
          (GraphState.graphStructuralVertexProjections (startupGraphState predecessor))
          >>= Reconciliation.structuralVertexProjectionOccurrence
      )
  let membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState predecessor))
      staleRevision =
        checked
          "stale disappearance label revision"
          (mkLabelRevision (controlIndex 7799))
      staleSubject =
        checked
          "stale-revision disappearance subject"
          ( controlledPredefinedDisappearanceSubject
              NeutralVertexRole
              object
              occurrence
              (Just staleRevision)
          )
      staleProbe =
        checked
          "stale-revision disappearance probe"
          (deriveDisappearanceProbeId (controlIndex 7811))
      staleResolveIndex = controlIndex 7812
  (staleLeaf, _) <- reportCompleteLeaf staleProbe staleSubject membership predecessor
  let staleTerminal =
        ProjectedDisappearanceResolved
          staleProbe
          ( checked
              "stale-revision disappearance outcome"
              ( resolveDisappearanceSubject
                  (checkedSystemId (startupGenesis predecessor))
                  staleResolveIndex
                  staleSubject
              )
          )
          staleResolveIndex
  case DisappearanceUseCase.coordinateProjectedControlledResolution staleTerminal staleLeaf predecessor of
    Left
      ( DisappearanceUseCase.DisappearanceControlledRemovalProblem
          ( ControlledRemoval.ControlledRemovalExpectedLabelRevisionMismatch
              actualObject
              expectedRevision
              actualRevision
            )
        ) -> do
        assertEqual "stale revision rejection names the object" object actualObject
        assertEqual "stale revision rejection names the captured revision" (Just staleRevision) expectedRevision
        assertEqual "stale revision rejection names the current revision" (Just revision) actualRevision
    Left problem ->
      assertFailure ("unexpected stale-revision rejection: " <> show problem)
    Right _ ->
      assertFailure "a stale released-label revision authorized removal"
  assertEqual
    "stale revision leaves the Controlled record live"
    Nothing
    (ControlledState.controlledTerminalDeletion object (startupControlledState predecessor))
  assertBool
    "stale revision leaves the Neutral projection live"
    (GraphState.graphHasVertex (NeutralVertex object) (startupGraphState predecessor))
  assertEqual
    "stale revision leaves the whole Herald valid"
    (Right ())
    (validateHeraldState predecessor)

  let matchingSubject =
        checked
          "matching-revision disappearance subject"
          ( controlledPredefinedDisappearanceSubject
              NeutralVertexRole
              object
              occurrence
              (Just revision)
          )
      matchingProbe =
        checked
          "matching-revision disappearance probe"
          (deriveDisappearanceProbeId (controlIndex 7821))
      matchingResolveIndex = controlIndex 7822
  (matchingLeaf, _) <- reportCompleteLeaf matchingProbe matchingSubject membership predecessor
  let matchingTerminal =
        ProjectedDisappearanceResolved
          matchingProbe
          ( checked
              "matching-revision disappearance outcome"
              ( resolveDisappearanceSubject
                  (checkedSystemId (startupGenesis predecessor))
                  matchingResolveIndex
                  matchingSubject
              )
          )
          matchingResolveIndex
  (_, successor, _, coordination) <-
    checkedIO
      "coordinate matching-revision controlled removal"
      ( DisappearanceUseCase.coordinateProjectedControlledResolution
          matchingTerminal
          matchingLeaf
          predecessor
      )
  summary <-
    maybe
      (assertFailure "matching revision returned no controlled-removal summary")
      pure
      (DisappearanceUseCase.projectedControlledResolutionCoordinationSummary coordination)
  assertEqual
    "matching revision authorizes fresh controlled removal"
    ControlledRemoval.ControlledRemovalApplied
    (ControlledRemoval.controlledRemovalSummaryDisposition summary)
  assertBool
    "matching revision installs the terminal tombstone"
    (ControlledState.controlledObjectTerminallyDeleted object (startupControlledState successor))
  assertEqual
    "automatic disappearance preserves the generation of the preceding same-owner release"
    (Just 1)
    (ControlledState.controlledTerminalDeletionGeneration <$> ControlledState.controlledTerminalDeletion object (startupControlledState successor))
  assertEqual
    "matching-revision successor is whole-state valid"
    (Right ())
    (validateHeraldState successor)

installMatchingReleasedLabel ::
  GlobalObjectId -> HeraldState -> IO (HeraldState, LabelRevision)
installMatchingReleasedLabel object state = do
  let genesis = startupGenesis state
      initialProjection = checkedInitialProjectionDigest (startupInitialBootstraps state)
      controlled = startupControlledState state
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState state))
      decision =
        checked
          "matching-revision label decision"
          (mkLabelDecisionId (fixtureIdentifierBytes 0xd9))
      resolveIndex = controlIndex 7802
      releaseIndex = controlIndex 7802
  prior <-
    checkedIO
      "derive matching-revision Controlled label prior"
      (ControlledState.checkControlledLabelPrior initialProjection object controlled)
  priorLabel <- case releasedLabelStateView (ControlledState.controlledLabelPriorEffectiveState prior) of
    ReleasedLabelView label -> pure label
    (ReleasedDeletedView _) -> assertFailure "matching-revision predecessor is already deleted"
  let retainedState = ControlledState.controlledLabelPriorEffectiveState prior
      facts =
        checked
          "construct matching-revision PreparedLabelFacts"
          ( mkPreparedLabelFacts
              decision
              resolveIndex
              object
              (releasedLabel (fst priorLabel, snd priorLabel + 1))
              retainedState
              (ControlledState.controlledLabelPriorRevision prior)
              (checkedCatalogueDigest genesis)
              (ControlledState.controlledLabelPriorAuthority prior)
              (ControlledState.controlledLabelPriorAuthorityJustification prior)
              ( preparedAuthorityDisposition
                  (ControlledState.controlledLabelPriorAuthority prior)
                  priorLabel
                  retainedState
              )
              (heraldMembershipGenerationActiveMemberSetDigest membership)
          )
      digest = derivePreparedLabelDigest facts
  specification <-
    checkedIO
      "derive matching-revision label patch"
      ( LabelPatch.checkedPatchSpecification
          (checkedLocalHeraldEpoch genesis)
          facts
          digest
          (startupStructuralProgressState state)
          controlled
          (startupSortRegistryState state)
          (startupPlacementState state)
          (startupStoreState state)
          (startupOracleProjectionState state)
      )
  preparedPatch <-
    checkedIO
      "prepare matching-revision label patch"
      ( LabelPatch.prepareLabelPatch
          specification
          (startupIdGeneratorState state)
          (startupLabelPatchState state)
      )
  let (patchOwner, generator) = LabelPatch.commitLabelPatch preparedPatch
  patchRelease <-
    checkedIO
      "release matching-revision label patch"
      (LabelPatch.preparePatchRelease specification releaseIndex generator patchOwner)
  preparedControlled <-
    checkedIO
      "install matching-revision Controlled label record"
      ( ControlledState.prepareControlledLabelRelease
          initialProjection
          facts
          releaseIndex
          controlled
      )
  preparedProgress <-
    checkedIO
      "advance matching-revision structural control"
      ( GraphProgress.prepareStructuralControlProgress
          releaseIndex
          (startupStructuralProgressState state)
      )
  let (progress, _) = GraphProgress.commitStructuralControlProgress preparedProgress
      (releasedPatchOwner, releaseGenerator) = LabelPatch.commitPatchRelease patchRelease
      successor =
        replaceStartupControlledState
          (ControlledState.commitControlledLabelRelease preparedControlled)
          . replaceStartupLabelPatchState releasedPatchOwner
          . replaceStartupIdGeneratorState releaseGenerator
          . replaceStartupStructuralProgressState progress
          $ state
  record <-
    maybe
      (assertFailure "matching-revision label record was not retained")
      pure
      (ControlledState.controlledReleasedLabelRecord object (startupControlledState successor))
  assertEqual
    "matching-revision release preserves the owner and advances generation"
    (ReleasedLabelView (fst priorLabel, snd priorLabel + 1))
    (releasedLabelStateView (labelRecordReleasedState record))
  pure (successor, labelRecordRevision record)

caseLabelDisappearanceDifferential :: Assertion
caseLabelDisappearanceDifferential = do
  (predecessor, object, _) <- settledNeutralControlledFixture
  record <-
    maybe
      (assertFailure "differential fixture has no settled Controlled record")
      pure
      (ControlledState.controlledLocalRecord object (startupControlledState predecessor))
  occurrence <-
    maybe
      (assertFailure "differential fixture has no current structural occurrence")
      pure
      ( Map.lookup object (GraphState.graphStructuralVertexProjections (startupGraphState predecessor))
          >>= Reconciliation.structuralVertexProjectionOccurrence
      )
  let membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState predecessor))
      consequenceIndex = controlIndex 7702
      subject =
        checked
          "differential disappearance subject"
          ( controlledPredefinedDisappearanceSubject
              NeutralVertexRole
              object
              occurrence
              Nothing
          )
      probe = checked "differential disappearance probe" (deriveDisappearanceProbeId (controlIndex 7700))
  (readyLeaf, _) <- reportCompleteLeaf probe subject membership predecessor
  let disappearanceTerminal =
        ProjectedDisappearanceResolved
          probe
          ( checked
              "differential disappearance outcome"
              ( resolveDisappearanceSubject
                  (checkedSystemId (startupGenesis predecessor))
                  consequenceIndex
                  subject
              )
          )
          consequenceIndex
  (_, disappearanceSuccessor, disappearanceEffects, disappearanceCoordination) <-
    checkedIO
      "apply differential disappearance removal"
      ( DisappearanceUseCase.coordinateProjectedControlledResolution
          disappearanceTerminal
          readyLeaf
          predecessor
      )
  disappearanceSummary <-
    maybe
      (assertFailure "differential disappearance returned no summary")
      pure
      ( DisappearanceUseCase.projectedControlledResolutionCoordinationSummary
          disappearanceCoordination
      )

  (labelInput, recipe) <-
    prepareSyntheticLabelDelete consequenceIndex object predecessor
  labelPrepared <-
    checkedIO
      "apply differential label-delete removal"
      ( ControlledRemoval.prepareLabelControlledRemoval
          consequenceIndex
          recipe
          labelInput
      )
  let (labelSuccessor, labelEffects, labelSummary) =
        ControlledRemoval.commitControlledRemoval labelPrepared

  assertEqual
    "differential inputs start from the same current publication"
    (ControlledState.controlledRecordLatestPublication record)
    ( ControlledState.controlledRecordLatestPublication
        ( requiredControlledRecord
            "synthetic label input"
            object
            (startupControlledState labelInput)
        )
    )
  assertBool
    "the two entry points retain distinct terminal authority provenance"
    ( ControlledRemoval.controlledRemovalSummaryOrigin labelSummary
        /= ControlledRemoval.controlledRemovalSummaryOrigin disappearanceSummary
    )
  assertNormalizedSummaryEqual labelSummary disappearanceSummary
  assertNormalizedControlledEqual object labelSuccessor disappearanceSuccessor
  assertBool
    "normalized Graph result"
    (startupGraphState labelSuccessor == startupGraphState disappearanceSuccessor)
  assertEqual
    "normalized Placement result"
    (startupPlacementState labelSuccessor)
    (startupPlacementState disappearanceSuccessor)
  assertNormalizedStoreEqual labelSuccessor disappearanceSuccessor
  assertNormalizedStructuralProgressEqual labelSuccessor disappearanceSuccessor
  assertBool
    "both entry points advance structural control through the shared consequence index"
    ( all
        (>= consequenceIndex)
        [ GraphProgress.structuralAppliedControlPrefix
            (startupStructuralProgressState labelSuccessor),
          GraphProgress.structuralAppliedControlPrefix
            (startupStructuralProgressState disappearanceSuccessor)
        ]
    )
  assertNormalizedAlignmentEqual labelSuccessor disappearanceSuccessor
  assertBool
    "normalized Wait result"
    (startupWaitState labelSuccessor == startupWaitState disappearanceSuccessor)
  assertUnchangedOwnerProductEqual labelSuccessor disappearanceSuccessor
  assertEqual "normalized applicator effects" labelEffects disappearanceEffects
  assertEqual
    "disappearance differential successor remains whole-state valid"
    (Right ())
    (validateHeraldState disappearanceSuccessor)
  assertEqual
    "label and disappearance Store histories remain owner-valid"
    (Right (), Right ())
    ( StoreState.validateStoreHistoryState (startupStoreState labelSuccessor),
      StoreState.validateStoreHistoryState (startupStoreState disappearanceSuccessor)
    )
  assertEqual
    "label and disappearance Alignment owners remain valid"
    (Right (), Right ())
    ( AlignmentState.validateAlignmentState (startupAlignmentState labelSuccessor),
      AlignmentState.validateAlignmentState (startupAlignmentState disappearanceSuccessor)
    )

prepareSyntheticLabelDelete ::
  ControlIndex ->
  GlobalObjectId ->
  HeraldState ->
  IO (HeraldState, LabelPatch.LabelPatchRecipe)
prepareSyntheticLabelDelete releaseIndex object state = do
  let genesis = startupGenesis state
      controlled = startupControlledState state
      initialProjection = checkedInitialProjectionDigest (startupInitialBootstraps state)
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState state))
  prior <-
    checkedIO
      "derive actual differential Controlled label prior"
      (ControlledState.checkControlledLabelPrior initialProjection object controlled)
  priorLabel <- case releasedLabelStateView (ControlledState.controlledLabelPriorEffectiveState prior) of
    ReleasedLabelView label -> pure label
    (ReleasedDeletedView _) -> assertFailure "differential predecessor is already deleted"
  let decision = differentialLabelDecision
      resolveIndex = releaseIndex
      facts =
        checked
          "construct differential PreparedLabelFacts"
          ( mkPreparedLabelFacts
              decision
              resolveIndex
              object
              (releasedDeleted (snd priorLabel + 1))
              (ControlledState.controlledLabelPriorEffectiveState prior)
              (ControlledState.controlledLabelPriorRevision prior)
              (checkedCatalogueDigest genesis)
              (ControlledState.controlledLabelPriorAuthority prior)
              (ControlledState.controlledLabelPriorAuthorityJustification prior)
              ( preparedAuthorityDisposition
                  (ControlledState.controlledLabelPriorAuthority prior)
                  priorLabel
                  (releasedDeleted (snd priorLabel + 1))
              )
              (heraldMembershipGenerationActiveMemberSetDigest membership)
          )
      digest = derivePreparedLabelDigest facts
      retainedPrior = ControlledState.controlledLabelPriorEffectiveState prior
      nonDeleteFacts =
        checked
          "construct differential non-delete PreparedLabelFacts"
          ( mkPreparedLabelFacts
              differentialNonDeleteLabelDecision
              resolveIndex
              object
              (releasedLabel (fst priorLabel, snd priorLabel + 1))
              retainedPrior
              (ControlledState.controlledLabelPriorRevision prior)
              (checkedCatalogueDigest genesis)
              (ControlledState.controlledLabelPriorAuthority prior)
              (ControlledState.controlledLabelPriorAuthorityJustification prior)
              ( preparedAuthorityDisposition
                  (ControlledState.controlledLabelPriorAuthority prior)
                  priorLabel
                  retainedPrior
              )
              (heraldMembershipGenerationActiveMemberSetDigest membership)
          )
      nonDeleteDigest = derivePreparedLabelDigest nonDeleteFacts
  nonDeleteSpecification <-
    checkedIO
      "derive differential non-delete patch specification"
      ( LabelPatch.checkedPatchSpecification
          (checkedLocalHeraldEpoch genesis)
          nonDeleteFacts
          nonDeleteDigest
          (startupStructuralProgressState state)
          controlled
          (startupSortRegistryState state)
          (startupPlacementState state)
          (startupStoreState state)
          (startupOracleProjectionState state)
      )
  nonDeletePrepared <-
    checkedIO
      "prepare differential non-delete patch recipe"
      ( LabelPatch.prepareLabelPatch
          nonDeleteSpecification
          (startupIdGeneratorState state)
          (startupLabelPatchState state)
      )
  let (nonDeletePending, nonDeleteGenerator) = LabelPatch.commitLabelPatch nonDeletePrepared
  nonDeleteRelease <- checkedIO "derive current non-delete plan" (LabelPatch.preparePatchRelease nonDeleteSpecification releaseIndex nonDeleteGenerator nonDeletePending)
  nonDeleteRecipe <- maybe (assertFailure "non-delete Release lacks its transient recipe") pure (LabelPatch.preparedReleasedPatchRecipe nonDeleteRelease)
  assertEqual
    "differential non-delete path selects Neutral overlay"
    LabelPatch.OverlayNeutralVertex
    (LabelPatch.labelPatchRecipeForm nonDeleteRecipe)
  case ControlledRemoval.prepareLabelControlledRemoval releaseIndex nonDeleteRecipe state of
    Left (ControlledRemoval.ControlledRemovalLabelRoleNotDeleted actualObject) ->
      assertEqual "non-delete rejection names the object" object actualObject
    Left problem -> assertFailure ("unexpected non-delete label recipe rejection: " <> show problem)
    Right _ -> assertFailure "a non-delete LabelPatch recipe entered controlled removal"
  specification <-
    checkedIO
      "derive differential checked label patch specification"
      ( LabelPatch.checkedPatchSpecification
          (checkedLocalHeraldEpoch genesis)
          facts
          digest
          (startupStructuralProgressState state)
          controlled
          (startupSortRegistryState state)
          (startupPlacementState state)
          (startupStoreState state)
          (startupOracleProjectionState state)
      )
  preparedPatch <-
    checkedIO
      "prepare differential label patch recipe"
      ( LabelPatch.prepareLabelPatch
          specification
          (startupIdGeneratorState state)
          (startupLabelPatchState state)
      )
  let (patchOwner, generator) = LabelPatch.commitLabelPatch preparedPatch
      preparedPatchInput =
        replaceStartupLabelPatchState patchOwner
          . replaceStartupIdGeneratorState generator
          $ state
  patchRelease <-
    checkedIO
      "release the differential LabelPatch recipe"
      ( LabelPatch.preparePatchRelease
          specification
          releaseIndex
          generator
          patchOwner
      )
  recipe <- maybe (assertFailure "first delete Release lacks its transient recipe") pure (LabelPatch.preparedReleasedPatchRecipe patchRelease)
  case ControlledRemoval.prepareLabelControlledRemoval releaseIndex recipe preparedPatchInput of
    Left (ControlledRemoval.ControlledRemovalLabelPatchNotReleased actualDecision) ->
      assertEqual
        "unreleased recipe rejection names the exact decision"
        decision
        actualDecision
    Left problem ->
      assertFailure ("unexpected unreleased label recipe rejection: " <> show problem)
    Right _ -> assertFailure "an unreleased LabelPatch recipe entered controlled removal"
  let (releasedPatchOwner, releaseGenerator) = LabelPatch.commitPatchRelease patchRelease
  assertEqual
    "differential label path selects Neutral deletion"
    LabelPatch.DeleteNeutralVertex
    (LabelPatch.labelPatchRecipeForm recipe)
  retainedPatch <-
    maybe
      (assertFailure "differential label path retained no released recipe")
      pure
      (LabelPatch.lookupRetainedLabelPatch decision releasedPatchOwner)
  assertEqual
    "differential label path discards the installed recipe"
    Nothing
    (LabelPatch.retainedPatchViewPreparation retainedPatch)
  assertEqual
    "differential label path retains exact installation evidence"
    (Just (LabelPatch.preparedReleasedPatchInstallation patchRelease))
    (LabelPatch.retainedPatchViewInstallation retainedPatch)
  assertEqual
    "differential label path is released at the applicator index"
    (LabelPatch.PatchReleasedView releaseIndex)
    (LabelPatch.retainedPatchViewPhase retainedPatch)
  released <-
    checkedIO
      "install only the differential Controlled released-label record"
      ( ControlledState.prepareControlledLabelRelease
          initialProjection
          facts
          releaseIndex
          controlled
      )
  let labelInput =
        replaceStartupControlledState (ControlledState.commitControlledLabelRelease released)
          . replaceStartupLabelPatchState releasedPatchOwner
          . replaceStartupIdGeneratorState releaseGenerator
          $ state
      wrongReleaseIndex = controlIndex (controlIndexWord64 releaseIndex + 1)
  case ControlledRemoval.prepareLabelControlledRemoval wrongReleaseIndex recipe labelInput of
    Left
      ( ControlledRemoval.ControlledRemovalLabelPatchReleaseMismatch
          actualDecision
          suppliedIndex
          retainedIndex
        ) -> do
        assertEqual "wrong-index rejection names the decision" decision actualDecision
        assertEqual "wrong-index rejection names the supplied index" wrongReleaseIndex suppliedIndex
        assertEqual "wrong-index rejection names the retained index" releaseIndex retainedIndex
    Left problem -> assertFailure ("unexpected wrong label release-index rejection: " <> show problem)
    Right _ -> assertFailure "a mismatched LabelPatch release index entered controlled removal"
  pure (labelInput, recipe)

differentialLabelDecision :: LabelDecisionId
differentialLabelDecision =
  checked
    "differential label decision"
    (mkLabelDecisionId (fixtureIdentifierBytes 0xd7))

differentialNonDeleteLabelDecision :: LabelDecisionId
differentialNonDeleteLabelDecision =
  checked
    "differential non-delete label decision"
    (mkLabelDecisionId (fixtureIdentifierBytes 0xd8))

assertNormalizedSummaryEqual ::
  ControlledRemoval.ControlledRemovalSummary ->
  ControlledRemoval.ControlledRemovalSummary ->
  Assertion
assertNormalizedSummaryEqual labelSummary disappearanceSummary = do
  assertEqual
    "normalized summary object"
    (ControlledRemoval.controlledRemovalSummaryObject labelSummary)
    (ControlledRemoval.controlledRemovalSummaryObject disappearanceSummary)
  assertEqual
    "normalized summary role"
    (ControlledRemoval.controlledRemovalSummaryRole labelSummary)
    (ControlledRemoval.controlledRemovalSummaryRole disappearanceSummary)
  assertEqual
    "normalized summary disposition"
    (ControlledRemoval.controlledRemovalSummaryDisposition labelSummary)
    (ControlledRemoval.controlledRemovalSummaryDisposition disappearanceSummary)
  assertEqual
    "normalized Store fallout"
    (ControlledRemoval.controlledRemovalSummaryStorePurgedFacts labelSummary)
    (ControlledRemoval.controlledRemovalSummaryStorePurgedFacts disappearanceSummary)
  assertEqual
    "normalized total fallout"
    (ControlledRemoval.controlledRemovalSummaryFalloutCount labelSummary)
    (ControlledRemoval.controlledRemovalSummaryFalloutCount disappearanceSummary)
  assertEqual
    "normalized logical work"
    (ControlledRemoval.controlledRemovalSummaryLogicalWork labelSummary)
    (ControlledRemoval.controlledRemovalSummaryLogicalWork disappearanceSummary)

assertNormalizedControlledEqual ::
  GlobalObjectId ->
  HeraldState ->
  HeraldState ->
  Assertion
assertNormalizedControlledEqual object labelState disappearanceState = do
  let label = startupControlledState labelState
      disappearance = startupControlledState disappearanceState
  assertEqual
    "normalized live Controlled records"
    (ControlledState.controlledLocalRecords label)
    (ControlledState.controlledLocalRecords disappearance)
  assertEqual
    "normalized Controlled Normal possessions"
    (ControlledState.controlledNormalPossessionEntries label)
    (ControlledState.controlledNormalPossessionEntries disappearance)
  assertEqual
    "normalized Controlled reservations"
    (ControlledState.controlledReservationWitnesses label)
    (ControlledState.controlledReservationWitnesses disappearance)
  assertEqual
    "normalized Controlled process facts"
    (ControlledState.controlledProcessFacts label)
    (ControlledState.controlledProcessFacts disappearance)
  assertEqual
    "normalized Controlled root facts"
    (ControlledState.controlledRootFacts label)
    (ControlledState.controlledRootFacts disappearance)
  assertEqual
    "normalized Controlled Store possessions"
    (ControlledState.controlledStorePossessionWitnesses label)
    (ControlledState.controlledStorePossessionWitnesses disappearance)
  assertEqual
    "normalized Controlled terminal identities"
    (fmap fst (ControlledState.controlledTerminalDeletionEntries label))
    (fmap fst (ControlledState.controlledTerminalDeletionEntries disappearance))
  assertEqual
    "unrelated released-label history is identical"
    (filter ((/= object) . fst) (ControlledState.controlledReleasedLabelEntries label))
    (filter ((/= object) . fst) (ControlledState.controlledReleasedLabelEntries disappearance))
  assertBool
    "both entry points install terminal suppression"
    ( ControlledState.controlledObjectTerminallyDeleted object label
        && ControlledState.controlledObjectTerminallyDeleted object disappearance
    )
  labelDeletion <-
    maybe
      (assertFailure "label differential tombstone missing")
      pure
      (ControlledState.controlledTerminalDeletion object label)
  disappearanceDeletion <-
    maybe
      (assertFailure "disappearance differential tombstone missing")
      pure
      (ControlledState.controlledTerminalDeletion object disappearance)
  assertEqual
    "successful label deletion retains one successor; automatic removal preserves the prior"
    (ControlledState.controlledTerminalDeletionGeneration disappearanceDeletion + 1)
    (ControlledState.controlledTerminalDeletionGeneration labelDeletion)
  assertBool
    "normalization deliberately excludes the distinct tombstone causes"
    ( ControlledState.controlledTerminalDeletionCause labelDeletion
        /= ControlledState.controlledTerminalDeletionCause disappearanceDeletion
    )
  assertBool
    "label history remains exclusive to the label entry point"
    ( isJust (ControlledState.controlledReleasedLabelRecord object label)
        && ControlledState.controlledReleasedLabelRecord object disappearance == Nothing
    )

assertNormalizedStoreEqual :: HeraldState -> HeraldState -> Assertion
assertNormalizedStoreEqual labelState disappearanceState = do
  let label = startupStoreState labelState
      disappearance = startupStoreState disappearanceState
      slotShape slot =
        ( StoreState.storeSlotProvenance slot,
          StoreState.storeSlotDelta slot,
          StoreState.storeSlotIncarnation slot,
          StoreState.storeSlotSortId slot,
          StoreState.storeSlotOccurrenceId slot,
          StoreState.storeSlotContents slot,
          StoreState.storeSlotRevision slot,
          StoreState.storeSlotAppliedPublicationIds slot,
          StoreState.storeSlotApplicationReceipts slot,
          fmap fst (StoreState.storeSlotTerminalObjectPurges slot)
        )
      retainedShape state slot =
        ( slotShape slot,
          StoreState.retainedStoreSnapshotAt
            (StoreState.storeSlotIncarnation slot)
            initialStoreRevision
            state,
          StoreState.retainedStoreChangesAfter
            (StoreState.storeSlotIncarnation slot)
            initialStoreRevision
            state
        )
  assertEqual
    "normalized active Store shape"
    (fmap slotShape (StoreState.storeSlots label))
    (fmap slotShape (StoreState.storeSlots disappearance))
  assertEqual
    "normalized retained Store shape"
    (fmap (retainedShape label) (StoreState.retainedStoreSlots label))
    (fmap (retainedShape disappearance) (StoreState.retainedStoreSlots disappearance))

assertNormalizedStructuralProgressEqual :: HeraldState -> HeraldState -> Assertion
assertNormalizedStructuralProgressEqual labelState disappearanceState = do
  let label =
        GraphProgress.structuralProgressReconciliation
          (startupStructuralProgressState labelState)
      disappearance =
        GraphProgress.structuralProgressReconciliation
          (startupStructuralProgressState disappearanceState)
  assertEqual
    "normalized current structural vertices"
    (Reconciliation.structuralAppliedVertexProjections label)
    (Reconciliation.structuralAppliedVertexProjections disappearance)
  assertEqual
    "normalized current structural edges"
    (Reconciliation.structuralAppliedEdgeProjections label)
    (Reconciliation.structuralAppliedEdgeProjections disappearance)

assertNormalizedAlignmentEqual :: HeraldState -> HeraldState -> Assertion
assertNormalizedAlignmentEqual labelState disappearanceState = do
  let label = startupAlignmentState labelState
      disappearance = startupAlignmentState disappearanceState
      debtShape debt =
        let key = StructuralDebt.structuralConsequenceDebtKey debt
         in ( StructuralDebt.structuralDebtKeyKind key,
              StructuralDebt.structuralDebtKeySort key,
              StructuralDebt.structuralDebtKeyDestination key,
              StructuralDebt.structuralConsequenceDebtEvidence debt
            )
      debts =
        fmap debtShape
          . StructuralDebt.structuralDebtSetEntries
          . AlignmentState.alignmentStructuralDebts
  assertBool
    "normalized Alignment debt multiset"
    (multisetEqual (debts label) (debts disappearance))
  assertEqual
    "normalized Alignment generations"
    (AlignmentState.alignmentGenerationEntries label)
    (AlignmentState.alignmentGenerationEntries disappearance)
  assertEqual
    "normalized active Alignment obligations"
    (AlignmentState.alignmentObligationEntries label)
    (AlignmentState.alignmentObligationEntries disappearance)
  assertEqual
    "normalized Alignment attempts"
    (AlignmentState.alignmentAttemptEntries label)
    (AlignmentState.alignmentAttemptEntries disappearance)
  assertBool
    "every cause-independent Alignment owner facet is identical"
    ( and
        [ AlignmentState.alignmentPromotionEntries label
            == AlignmentState.alignmentPromotionEntries disappearance,
          AlignmentState.alignmentGenerationRelationEntries label
            == AlignmentState.alignmentGenerationRelationEntries disappearance,
          AlignmentState.alignmentClosingObligationIds label
            == AlignmentState.alignmentClosingObligationIds disappearance,
          AlignmentState.alignmentClosedObligationEntries label
            == AlignmentState.alignmentClosedObligationEntries disappearance,
          AlignmentState.alignmentFailedSourceCoordinates label
            == AlignmentState.alignmentFailedSourceCoordinates disappearance,
          AlignmentState.alignmentUnavailableSources label
            == AlignmentState.alignmentUnavailableSources disappearance,
          AlignmentState.alignmentPlanInvalidationEntries label
            == AlignmentState.alignmentPlanInvalidationEntries disappearance,
          AlignmentState.alignmentAttemptInvalidationEntries label
            == AlignmentState.alignmentAttemptInvalidationEntries disappearance,
          AlignmentState.pendingGenerationEvidenceEntries label
            == AlignmentState.pendingGenerationEvidenceEntries disappearance,
          AlignmentState.bootstrapImportEntries label
            == AlignmentState.bootstrapImportEntries disappearance,
          AlignmentState.bootstrapImportAttemptEntries label
            == AlignmentState.bootstrapImportAttemptEntries disappearance,
          AlignmentState.bootstrapImportAttemptInvalidationEntries label
            == AlignmentState.bootstrapImportAttemptInvalidationEntries disappearance,
          AlignmentState.bootstrapImportFulfillmentEntries label
            == AlignmentState.bootstrapImportFulfillmentEntries disappearance,
          AlignmentState.alignmentCutAcceptanceEntries label
            == AlignmentState.alignmentCutAcceptanceEntries disappearance,
          AlignmentState.alignmentMemberReadinessEntries label
            == AlignmentState.alignmentMemberReadinessEntries disappearance,
          AlignmentState.alignmentLocalMemberReadinessEntries label
            == AlignmentState.alignmentLocalMemberReadinessEntries disappearance,
          AlignmentState.alignmentHistoricalCertificateEntries label
            == AlignmentState.alignmentHistoricalCertificateEntries disappearance,
          AlignmentState.alignmentTransferState label
            == AlignmentState.alignmentTransferState disappearance
        ]
    )

assertUnchangedOwnerProductEqual :: HeraldState -> HeraldState -> Assertion
assertUnchangedOwnerProductEqual label disappearance =
  assertBool
    "every unaffected Herald owner is identical"
    ( and
        [ heraldPhase label == heraldPhase disappearance,
          startupApplicationRecoveryConfiguration label
            == startupApplicationRecoveryConfiguration disappearance,
          startupLastObservedTime label == startupLastObservedTime disappearance,
          startupGenesis label == startupGenesis disappearance,
          startupInitialBootstraps label == startupInitialBootstraps disappearance,
          startupIdGeneratorState label == startupIdGeneratorState disappearance,
          startupOracleClientState label == startupOracleClientState disappearance,
          startupOracleProjectionState label == startupOracleProjectionState disappearance,
          startupSortRegistryState label == startupSortRegistryState disappearance,
          startupApplicationState label == startupApplicationState disappearance,
          startupAdministrationState label == startupAdministrationState disappearance,
          startupConfiguredProcessState label == startupConfiguredProcessState disappearance,
          startupDiscoveryState label == startupDiscoveryState disappearance,
          startupPeerLivenessState label == startupPeerLivenessState disappearance,
          startupFailureDetectionState label == startupFailureDetectionState disappearance,
          startupIsolationState label == startupIsolationState disappearance,
          startupTerminalSourceHoldState label == startupTerminalSourceHoldState disappearance,
          startupStructuralBaseCoordinator label == startupStructuralBaseCoordinator disappearance,
          startupPeerStreamState label == startupPeerStreamState disappearance,
          startupPublicationState label == startupPublicationState disappearance,
          startupLabelBarrierState label == startupLabelBarrierState disappearance,
          startupVisibilityState label == startupVisibilityState disappearance
        ]
    )

multisetEqual :: (Eq value) => [value] -> [value] -> Bool
multisetEqual left right =
  length left == length right
    && all
      (\value -> occurrences value left == occurrences value right)
      left
  where
    occurrences value = length . filter (== value)

requiredControlledRecord ::
  String ->
  GlobalObjectId ->
  ControlledState.State ->
  ControlledState.ControlledLocalRecord
requiredControlledRecord context object state =
  case ControlledState.controlledLocalRecord object state of
    Just record -> record
    Nothing -> error (context <> ": Controlled record missing")

exerciseEligibleRole ::
  Word64 ->
  EligibleRoleCase ->
  IO (GlobalObjectId, StructuralOccurrenceId, HeraldState, HeraldState)
exerciseEligibleRole seed roleCase@(EligibleRoleCase applicationRole _ _) = do
  (predecessor, object, publisher) <-
    settledStructuralControlledFixture applicationRole
  exerciseEligibleRoleFixture
    seed
    roleCase
    (predecessor, object, publisher, [])

-- | Test-only whole-Herald fixture spanning a real dynamic-Delta removal.
settledDeltaControlledRemovalFixture :: IO (HeraldState, HeraldState)
settledDeltaControlledRemovalFixture =
  regularDeltaControlledRemovalRecoveryFixture

exerciseEligibleRoleFixture ::
  Word64 ->
  EligibleRoleCase ->
  (HeraldState, GlobalObjectId, ProcessEpochId, [GlobalUniqueId]) ->
  IO (GlobalObjectId, StructuralOccurrenceId, HeraldState, HeraldState)
exerciseEligibleRoleFixture
  seed
  (EligibleRoleCase _ profileRole carrierRole)
  (predecessor, object, publisher, pristineReservations) = do
    assertEqual
      (context <> ": settled predecessor is whole-state valid")
      (Right ())
      (validateHeraldState predecessor)
    forM_ pristineReservations $ \generated -> do
      reservation <-
        maybe
          (assertFailure (context <> ": expected pristine child reservation is absent"))
          pure
          ( ControlledState.controlledReservationWitness
              generated
              (startupControlledState predecessor)
          )
      assertEqual
        (context <> ": child reservation starts pristine")
        ControlledState.Reserved
        (ControlledState.reservationWitnessPhase reservation)
    record <-
      maybe
        (assertFailure (context <> ": settled object has no Controlled record"))
        pure
        (ControlledState.controlledLocalRecord object (startupControlledState predecessor))
    assertEqual
      (context <> ": Controlled authenticates the expected structural role")
      (Just carrierRole)
      (ControlledState.controlledRecordStructuralRole record)
    occurrence <-
      maybe
        (assertFailure (context <> ": settled object has no structural occurrence"))
        pure
        (currentStructuralOccurrence carrierRole object predecessor)
    assertRoleProjectionPresent context carrierRole object predecessor
    let publication = ControlledState.controlledRecordLatestPublication record
        payloadSort = checkedPublicationSort publication
        payloadKey = checkedPublicationKey publication
        carryingSlots =
          filter
            (slotCarries payloadSort payloadKey)
            (StoreState.storeSlots (startupStoreState predecessor))
    assertBool
      (context <> ": payload reached a real active Store")
      (not (null carryingSlots))
    assertBool
      (context <> ": publisher starts with direct possession")
      ( ControlledState.controlledHasDirectPossession
          publisher
          object
          (startupControlledState predecessor)
      )

    let membership =
          OracleProjection.oracleViewCurrentHeraldMembership
            (OracleProjection.oracleView (startupOracleProjectionState predecessor))
        subject =
          checked
            (context <> ": disappearance subject")
            ( controlledPredefinedDisappearanceSubject
                profileRole
                object
                occurrence
                Nothing
            )
        probe =
          checked
            (context <> ": disappearance probe")
            (deriveDisappearanceProbeId (controlIndex (seed + 1)))
        resolveIndex = controlIndex (seed + 2)
    (readyLeaf, _) <- reportCompleteLeaf probe subject membership predecessor
    let terminal =
          ProjectedDisappearanceResolved
            probe
            ( checked
                (context <> ": Resolve outcome")
                ( resolveDisappearanceSubject
                    (checkedSystemId (startupGenesis predecessor))
                    resolveIndex
                    subject
                )
            )
            resolveIndex
    (_, successor, effects, coordination) <-
      checkedIO
        (context <> ": coordinate controlled removal")
        ( DisappearanceUseCase.coordinateProjectedControlledResolution
            terminal
            readyLeaf
            predecessor
        )
    summary <-
      maybe
        (assertFailure (context <> ": fresh removal returned no summary"))
        pure
        (DisappearanceUseCase.projectedControlledResolutionCoordinationSummary coordination)
    assertEqual
      (context <> ": summary role")
      (Just carrierRole)
      (ControlledRemoval.controlledRemovalSummaryRole summary)
    assertEqual
      (context <> ": removal disposition")
      ControlledRemoval.ControlledRemovalApplied
      (ControlledRemoval.controlledRemovalSummaryDisposition summary)
    assertEqual
      (context <> ": cancellation summary counts every pristine child reservation")
      (fromIntegral (length pristineReservations))
      (ControlledRemoval.controlledRemovalSummaryCancelledReservations summary)
    assertBool
      (context <> ": retained payload was purged")
      (ControlledRemoval.controlledRemovalSummaryStorePurgedFacts summary > 0)
    assertEqual
      (context <> ": successor is whole-state valid")
      (Right ())
      (validateHeraldState successor)
    assertBool
      (context <> ": structural control covers the Resolve index")
      ( GraphProgress.structuralAppliedControlPrefix
          (startupStructuralProgressState successor)
          >= resolveIndex
      )
    let controlledAfter = startupControlledState successor
    deletion <-
      maybe
        (assertFailure (context <> ": no terminal tombstone"))
        pure
        (ControlledState.controlledTerminalDeletion object controlledAfter)
    assertEqual
      (context <> ": live Controlled record is absent")
      Nothing
      (ControlledState.controlledLocalRecord object controlledAfter)
    assertBool
      (context <> ": direct possession is revoked")
      (not (ControlledState.controlledHasDirectPossession publisher object controlledAfter))
    assertBool
      (context <> ": no Normal possession survives")
      (not (ControlledState.controlledHasAnyNormalPossession object controlledAfter))
    forM_ pristineReservations $ \generated ->
      assertEqual
        (context <> ": pristine child reservation is cancelled")
        Nothing
        (ControlledState.controlledReservationWitness generated controlledAfter)
    assertRoleProjectionRemoved context carrierRole object successor
    let matchingSlots =
          filter
            ((== payloadSort) . StoreState.storeSlotSortId)
            (StoreState.retainedStoreSlots (startupStoreState successor))
        cause = ControlledState.controlledTerminalDeletionCause deletion
        falloutTerms =
          observableRemovalFallout
            object
            payloadSort
            payloadKey
            cause
            predecessor
            successor
            effects
        expectedFallout = sum (snd <$> falloutTerms)
    assertEqual
      (context <> ": summary F independently matches observable owner fallout")
      expectedFallout
      (ControlledRemoval.controlledRemovalSummaryFalloutCount summary)
    assertEqual
      (context <> ": work is independently exactly 4 + F")
      (4 + expectedFallout)
      (ControlledRemoval.controlledRemovalSummaryLogicalWork summary)
    case carrierRole of
      NablaCarrier
        | not (null pristineReservations) ->
            assertPositiveFallout "cancelled reservations" falloutTerms
      DeltaCarrier -> do
        assertPositiveFallout "placement changes" falloutTerms
        assertPositiveFallout "structural debts" falloutTerms
        assertPositiveFallout "local placement losses" falloutTerms
      _ -> pure ()
    assertBool
      (context <> ": the payload sort retains a Store resource")
      (not (null matchingSlots))
    assertBool
      (context <> ": every matching Store retains terminal suppression")
      ( all
          (\slot -> StoreState.storeSlotTerminalObjectPurgeCause payloadKey slot == Just cause)
          matchingSlots
      )
    assertBool
      (context <> ": no matching Store retains the payload")
      (all (slotOmits payloadKey) matchingSlots)
    pure (object, occurrence, predecessor, successor)
    where
      context = "controlled removal " <> show carrierRole

currentStructuralOccurrence ::
  StructuralCarrierRole ->
  GlobalObjectId ->
  HeraldState ->
  Maybe StructuralOccurrenceId
currentStructuralOccurrence role object state =
  case role of
    EdgeCarrier ->
      Map.lookup object (GraphState.graphStructuralEdgeProjections graph)
        >>= Reconciliation.ownedEdgeProjectionOccurrence
    ProcessEpochCarrier -> Nothing
    _ ->
      Map.lookup object (GraphState.graphStructuralVertexProjections graph)
        >>= Reconciliation.structuralVertexProjectionOccurrence
  where
    graph = startupGraphState state

assertRoleProjectionPresent ::
  String ->
  StructuralCarrierRole ->
  GlobalObjectId ->
  HeraldState ->
  Assertion
assertRoleProjectionPresent context role object state = case role of
  NeutralVertexCarrier ->
    assertBool
      (context <> ": Neutral vertex is projected")
      (GraphState.graphHasVertex (NeutralVertex object) graph)
  EdgeCarrier ->
    assertBool
      (context <> ": Edge projection is retained")
      (Map.member object (GraphState.graphStructuralEdgeProjections graph))
  NablaCarrier ->
    assertBool
      (context <> ": Nabla vertex is projected")
      (GraphState.graphHasVertex (NablaVertex (nablaIdFromGlobalObjectId object)) graph)
  DeltaCarrier -> do
    let delta = deltaIdFromGlobalObjectId object
    assertBool
      (context <> ": Delta vertex is projected")
      (GraphState.graphHasVertex (DeltaVertex delta) graph)
    assertBool
      (context <> ": Delta has a current local placement")
      (isJust (PlacementState.lookupLocalPlacement delta (startupPlacementState state)))
    assertBool
      (context <> ": Delta has an active Store incarnation")
      (isJust (StoreState.lookupStoreSlot delta (startupStoreState state)))
  ProcessEpochCarrier ->
    assertFailure (context <> ": ProcessEpoch unexpectedly entered eligible-role assertions")
  where
    graph = startupGraphState state

assertRoleProjectionRemoved ::
  String ->
  StructuralCarrierRole ->
  GlobalObjectId ->
  HeraldState ->
  Assertion
assertRoleProjectionRemoved context role object state = case role of
  NeutralVertexCarrier ->
    assertBool
      (context <> ": Neutral vertex is removed")
      (not (GraphState.graphHasVertex (NeutralVertex object) graph))
  EdgeCarrier ->
    assertBool
      (context <> ": Edge projection is removed")
      (Map.notMember object (GraphState.graphStructuralEdgeProjections graph))
  NablaCarrier ->
    assertBool
      (context <> ": Nabla vertex is removed")
      (not (GraphState.graphHasVertex (NablaVertex (nablaIdFromGlobalObjectId object)) graph))
  DeltaCarrier -> do
    let delta = deltaIdFromGlobalObjectId object
    assertBool
      (context <> ": Delta vertex is removed")
      (not (GraphState.graphHasVertex (DeltaVertex delta) graph))
    assertEqual
      (context <> ": local placement is removed")
      Nothing
      (PlacementState.lookupLocalPlacement delta (startupPlacementState state))
    assertEqual
      (context <> ": Store incarnation is no longer active")
      Nothing
      (StoreState.lookupStoreSlot delta (startupStoreState state))
  ProcessEpochCarrier ->
    assertFailure (context <> ": ProcessEpoch unexpectedly entered eligible-role assertions")
  where
    graph = startupGraphState state

-- | Reconstruct F from independently observable owner state. This deliberately
-- does not inspect any per-owner count retained
-- in 'ControlledRemovalSummary'.
observableRemovalFallout ::
  GlobalObjectId ->
  SortId ->
  ObjectKey ->
  StructuralConsequenceCause ->
  HeraldState ->
  HeraldState ->
  EffectBatch ->
  [(String, Word64)]
observableRemovalFallout object sortId key cause predecessor successor _effects =
  [ ("purged Store facts", purgedStoreFacts),
    ("revoked direct possessions", revokedDirectPossessions),
    ("revoked Store possessions", revokedStorePossessions),
    ("cancelled reservations", cancelledReservations),
    ("graph changes", graphChanges),
    ("placement changes", placementChanges),
    ("structural debts", structuralDebts),
    ("local placement losses", localPlacementLosses),
    ("completed wait registrations", completedWaitRegistrations)
  ]
  where
    controlledBefore = startupControlledState predecessor
    controlledAfter = startupControlledState successor
    storeBefore = startupStoreState predecessor
    graphBefore = startupGraphState predecessor
    graphAfter = startupGraphState successor
    placementBefore = startupPlacementState predecessor
    placementAfter = startupPlacementState successor
    alignmentBefore = startupAlignmentState predecessor
    alignmentAfter = startupAlignmentState successor

    purgedStoreFacts =
      sum
        [ snd (DomainStore.purgeObjectKey key (StoreState.storeSlotContents slot))
        | slot <- StoreState.retainedStoreSlots storeBefore,
          StoreState.storeSlotSortId slot == sortId
        ]
    revokedDirectPossessions =
      removedSetCount
        (directPossessionProcesses object controlledBefore)
        (directPossessionProcesses object controlledAfter)
    revokedStorePossessions =
      removedSetCount
        (storePossessionCoordinates object controlledBefore)
        (storePossessionCoordinates object controlledAfter)
    cancelledReservations =
      removedSetCount
        (pristineNablaReservations object controlledBefore)
        (pristineNablaReservations object controlledAfter)
    graphChanges =
      changedMapCount
        (GraphState.graphStructuralVertexProjections graphBefore)
        (GraphState.graphStructuralVertexProjections graphAfter)
        + changedMapCount
          (GraphState.graphStructuralEdgeProjections graphBefore)
          (GraphState.graphStructuralEdgeProjections graphAfter)
    placementChanges =
      changedMapCount
        (localPlacementMap placementBefore)
        (localPlacementMap placementAfter)
    structuralDebts =
      fromIntegral
        ( length (AlignmentState.structuralDebtsForCause cause alignmentAfter)
            - length (AlignmentState.structuralDebtsForCause cause alignmentBefore)
        )
    localPlacementLosses =
      removedSetCount
        (localPlacementCoordinates placementBefore)
        (localPlacementCoordinates placementAfter)
    completedWaitRegistrations =
      removedSetCount
        (waitRegistrationIds predecessor)
        (waitRegistrationIds successor)

    waitRegistrationIds state =
      Set.fromList
        ( fmap
            WaitState.waitRegistrationId
            (WaitState.waitRegistrations (startupWaitState state))
        )

directPossessionProcesses ::
  GlobalObjectId -> ControlledState.State -> Set.Set ProcessEpochId
directPossessionProcesses object controlled =
  Set.fromList
    [ process
    | fact <- ControlledState.controlledProcessFacts controlled,
      let process = ControlledState.processFactProcessEpoch fact,
      ControlledState.controlledHasDirectPossession process object controlled
    ]

storePossessionCoordinates ::
  GlobalObjectId ->
  ControlledState.State ->
  Set.Set (ProcessEpochId, DeltaId, ReplicaStrength)
storePossessionCoordinates object controlled =
  Set.fromList
    [ ( ControlledState.controlledStorePossessionWitnessProcess witness,
        ControlledState.controlledStorePossessionWitnessDelta witness,
        ControlledState.controlledStorePossessionWitnessStrength witness
      )
    | witness <- ControlledState.controlledStorePossessionWitnesses controlled,
      ControlledState.controlledStorePossessionWitnessObject witness == object
    ]

pristineNablaReservations ::
  GlobalObjectId -> ControlledState.State -> Set.Set GlobalUniqueId
pristineNablaReservations object controlled =
  Set.fromList
    [ ControlledState.reservationWitnessGlobalUniqueId witness
    | witness <- ControlledState.controlledReservationWitnesses controlled,
      ControlledState.reservationWitnessNabla witness
        == nablaIdFromGlobalObjectId object,
      ControlledState.reservationWitnessPhase witness == ControlledState.Reserved
    ]

localPlacementMap ::
  PlacementState.State -> Map.Map DeltaId PlacementState.LocalPlacement
localPlacementMap placement =
  Map.fromList
    [ (PlacementState.localPlacementDelta retained, retained)
    | retained <- PlacementState.localPlacements placement
    ]

localPlacementCoordinates ::
  PlacementState.State -> Set.Set (DeltaId, StoreIncarnationId)
localPlacementCoordinates placement =
  Set.fromList
    [ ( PlacementState.localPlacementDelta retained,
        PlacementState.localPlacementStoreIncarnation retained
      )
    | retained <- PlacementState.localPlacements placement
    ]

removedSetCount :: (Ord value) => Set.Set value -> Set.Set value -> Word64
removedSetCount before after =
  fromIntegral (Set.size (before Set.\\ after))

changedMapCount ::
  (Ord key, Eq value) => Map.Map key value -> Map.Map key value -> Word64
changedMapCount before after =
  fromIntegral
    ( Set.size
        ( Set.filter
            (\key' -> Map.lookup key' before /= Map.lookup key' after)
            (Map.keysSet before `Set.union` Map.keysSet after)
        )
    )

assertPositiveFallout :: String -> [(String, Word64)] -> Assertion
assertPositiveFallout name terms =
  case lookup name terms of
    Just count -> assertBool (name <> " is exercised") (count > 0)
    Nothing -> assertFailure ("observable fallout term is absent: " <> name)

slotCarries :: SortId -> ObjectKey -> StoreState.StoreSlot -> Bool
slotCarries sortId key slot =
  StoreState.storeSlotSortId slot == sortId
    && isJust (DomainStore.lookupRetained key (StoreState.storeSlotContents slot))

slotOmits :: ObjectKey -> StoreState.StoreSlot -> Bool
slotOmits key slot =
  DomainStore.lookupVisible key contents == Nothing
    && DomainStore.lookupRetained key contents == Nothing
  where
    contents = StoreState.storeSlotContents slot

assertStoreSuppressesStalePublication ::
  StoreState.StoreSlot ->
  CheckedPublication ->
  StoreState.State ->
  Assertion
assertStoreSuppressesStalePublication originalSlot publication state = do
  retainedSlot <-
    maybe
      (assertFailure "removed payload Store incarnation was not retained")
      pure
      ( StoreState.lookupRetainedStoreSlot
          (StoreState.storeSlotIncarnation originalSlot)
          state
      )
  transcript <-
    checkedIO
      "read the purged Store incarnation's immutable transcript"
      ( StoreState.retainedStoreChangesAfter
          (StoreState.storeSlotIncarnation retainedSlot)
          initialStoreRevision
          state
      )
  observation <-
    maybe
      (assertFailure "the purged publication is absent from the Store transcript")
      (pure . StoreState.retainedStoreChangeObservation)
      ( find
          ( (== checkedPublicationId publication)
              . checkedPublicationId
              . StoreState.retainedStoreObservationPublication
              . StoreState.retainedStoreChangeObservation
          )
          transcript
      )
  assertEqual
    "the delayed traffic replays the exact immutable publication"
    publication
    (StoreState.retainedStoreObservationPublication observation)
  assertEqual
    "the purged publication still has an immutable Store receipt"
    (Just Normal)
    (lookup (checkedPublicationId publication) (StoreState.storeSlotApplicationReceipts retainedSlot))
  assertBool
    "the publication's current visible and retained winner is already gone"
    (slotOmits (checkedPublicationKey publication) retainedSlot)
  prepared <-
    checkedIO
      "prepare post-terminal stale Store traffic"
      ( StoreState.prepareAlignmentStoreApplication
          ( StoreState.peerStoreDestination
              (StoreState.storeSlotDelta retainedSlot)
              (StoreState.storeSlotIncarnation retainedSlot)
              Normal
              :| []
          )
          observation
          state
      )
  let (successor, outcomes) = StoreState.commitPeerStoreApplication prepared
  assertEqual
    "post-terminal traffic is terminally ignored"
    [StoreState.PeerStoreTerminallyIgnored]
    (fmap StoreState.peerStoreOutcomeDisposition (NonEmpty.toList outcomes))
  assertBool "terminally ignored traffic changes no Store fact" (state == successor)
  assertEqual "terminally ignored traffic preserves Store history validity" (Right ()) (StoreState.validateStoreHistoryState successor)

reportCompleteLeaf ::
  DisappearanceProbeId ->
  DisappearanceSubject ->
  HeraldMembershipGeneration ->
  HeraldState ->
  IO (DisappearanceState.State, ProjectedDisappearanceProbe)
reportCompleteLeaf probe subject membership herald = do
  let local = checkedLocalHeraldEpoch (startupGenesis herald)
      coordinate = disappearanceSubjectMembershipCoordinate subject membership
      projected =
        checked
          "report-complete projected disappearance probe"
          (projectedDisappearanceProbe probe subject coordinate membership)
      localSnapshot = emptyOwnerSnapshot local subject
      members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)
      remotes = filter (/= local) members
  (openedLeaf, openedStream, _) <-
    checkedIO
      "open report-complete local disappearance leaf"
      ( DisappearanceUseCase.coordinateProjectedOpen
          projected
          localSnapshot
          (DisappearanceState.initialState local)
          (PeerStreamState.initialState local)
      )
  incomingMarkers <-
    forM remotes (remoteMarkerFor projected subject local)
  (receivedLeaf, receivedStream) <-
    foldM
      receiveMarker
      (openedLeaf, openedStream)
      incomingMarkers
  (completedLeaf, _) <-
    foldM
      completeMarker
      (receivedLeaf, receivedStream)
      incomingMarkers
  (cutComplete, _) <-
    checkedIO
      "complete report-complete local publication cut"
      ( DisappearanceUseCase.coordinateLocalPublicationCutCompletion
          probe
          (Evidence.disappearanceEvidenceSnapshotLocalPublicationCut localSnapshot)
          completedLeaf
      )
  let (reported, reportDisposition) =
        DisappearanceState.commitDisappearanceTransition
          ( checked
              "prepare report-complete local absence report"
              ( DisappearanceState.prepareLocalAbsenceReportFromEvidence
                  probe
                  localSnapshot
                  cutComplete
              )
          )
      localReport = reportFromDisposition reportDisposition
  attestation <-
    maybe
      (assertFailure "empty owner snapshot produced no absence attestation")
      pure
      (Evidence.disappearanceEvidenceSnapshotAbsenceAttestation localSnapshot)
  reports <-
    forM members $ \reporter ->
      if reporter == local
        then pure localReport
        else
          checkedIO
            "construct remote report claim from empty test owners"
            ( localAbsenceReportForOwner
                projected
                reporter
                EmptyHeraldPublicationPrefix
                []
                []
                attestation
            )
  complete <-
    foldM
      (\leaf report -> projectReportClaim (localAbsenceReportClaim report) leaf)
      reported
      reports
  assertEqual "report-complete disappearance leaf is valid" (Right ()) (DisappearanceState.validateState complete)
  pure (complete, projected)
  where
    receiveMarker (leaf, stream) item = do
      (successorLeaf, successorStream, _) <-
        checkedIO
          "receive captured publication marker"
          (DisappearanceUseCase.coordinatePublicationMarkerReceipt item leaf stream)
      pure (successorLeaf, successorStream)
    completeMarker (leaf, stream) item = do
      (successorLeaf, successorStream, _) <-
        checkedIO
          "complete captured publication marker"
          (DisappearanceUseCase.coordinatePublicationMarkerCompletion item leaf stream)
      pure (successorLeaf, successorStream)

remoteMarkerFor ::
  ProjectedDisappearanceProbe ->
  DisappearanceSubject ->
  HeraldEpoch ->
  HeraldEpoch ->
  IO (SequencedItem DisappearancePublicationMarker)
remoteMarkerFor projected subject destination source = do
  (_, _, coordination) <-
    checkedIO
      "open remote disappearance marker source"
      ( DisappearanceUseCase.coordinateProjectedOpen
          projected
          (emptyOwnerSnapshot source subject)
          (DisappearanceState.initialState source)
          (PeerStreamState.initialState source)
      )
  maybe
    (assertFailure "remote projected Open did not assign its local marker")
    pure
    ( Map.lookup
        destination
        (DisappearanceUseCase.projectedOpenCoordinationPublicationAssignments coordination)
    )

projectReportClaim ::
  DisappearanceEvidenceClaim ->
  DisappearanceState.State ->
  IO DisappearanceState.State
projectReportClaim claim state =
  fst
    . DisappearanceState.commitDisappearanceTransition
    <$> checkedIO
      "project report-complete disappearance claim"
      (DisappearanceState.prepareProjectedReport claim state)

reportFromDisposition :: DisappearanceState.ReportDisposition -> LocalAbsenceReport
reportFromDisposition disposition = case disposition of
  DisappearanceState.LocalReportPrepared report -> report
  DisappearanceState.LocalReportDuplicate report -> report

emptyOwnerSnapshot ::
  HeraldEpoch ->
  DisappearanceSubject ->
  Evidence.DisappearanceEvidenceSnapshot
emptyOwnerSnapshot local subject =
  checked
    "empty-owner controlled-removal evidence"
    ( Evidence.disappearanceEvidenceSnapshot
        ( StoreDisappearance.storeDisappearanceView
            subject
            (checked "initial controlled-removal Store owner" (StoreState.initialState fixtureStep14CheckedGenesis))
        )
        (Application.applicationDisappearanceView subject ApplicationState.emptyState)
        (Publication.publicationDisappearanceView subject (PublicationState.initialState local))
        ( checked
            "initial controlled-removal PeerStream owner"
            ( PeerStreamDisappearance.peerStreamDisappearanceView
                subject
                (PeerStreamState.initialState local)
            )
        )
        (Alignment.alignmentDisappearanceView subject AlignmentState.emptyState)
        (Controlled.controlledDisappearanceView subject ControlledState.emptyState)
        (GraphDisappearance.graphDisappearanceView subject GraphState.emptyState)
        (Placement.placementDisappearanceView subject PlacementState.emptyState)
    )

assertAbortCannotEnterRemoval ::
  ProjectedDisappearanceTerminal ->
  DisappearanceState.State ->
  HeraldState ->
  Assertion
assertAbortCannotEnterRemoval resolved readyLeaf herald = do
  let probe = case resolved of
        ProjectedDisappearanceResolved retained _ _ -> retained
        _ -> error "controlled-removal test expected a resolved terminal"
      aborted =
        ProjectedDisappearanceAborted
          probe
          ProjectedAuthorizedAbort
          (controlIndex 7303)
  case DisappearanceUseCase.coordinateProjectedControlledResolution aborted readyLeaf herald of
    Left
      ( DisappearanceUseCase.DisappearanceLeafProblem
          (DisappearanceState.DisappearanceControlledResolutionTerminalAborted actual)
        ) -> assertEqual "Abort rejection names the exact probe" probe actual
    Left problem -> assertFailure ("unexpected controlled-removal Abort rejection: " <> show problem)
    Right _ -> assertFailure "Abort entered the controlled-removal applicator"
  assertEqual "Abort leaves the source Herald valid and live" (Right ()) (validateHeraldState herald)

assertOwnerMismatchCannotEnterRemoval ::
  ProjectedDisappearanceTerminal ->
  HeraldMembershipGeneration ->
  HeraldState ->
  Assertion
assertOwnerMismatchCannotEnterRemoval terminal membership herald = do
  let heraldOwner = checkedLocalHeraldEpoch (startupGenesis herald)
      foreignOwners =
        filter
          (/= heraldOwner)
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))
  foreignOwner <- case foreignOwners of
    first : _ -> pure first
    [] -> assertFailure "controlled-removal fixture has no foreign leaf owner"
  let foreignLeaf = DisappearanceState.initialState foreignOwner
  case DisappearanceUseCase.coordinateProjectedControlledResolution terminal foreignLeaf herald of
    Left
      ( DisappearanceUseCase.DisappearanceControlledRemovalOwnerMismatch
          actualLeafOwner
          actualHeraldOwner
        ) -> do
        assertEqual "owner mismatch names the detached leaf owner" foreignOwner actualLeafOwner
        assertEqual "owner mismatch names the Herald owner" heraldOwner actualHeraldOwner
    Left problem -> assertFailure ("unexpected controlled-removal owner mismatch: " <> show problem)
    Right _ -> assertFailure "a foreign disappearance leaf entered controlled removal"
  assertEqual "owner mismatch leaves the Herald valid and live" (Right ()) (validateHeraldState herald)

assertMembershipMismatchCannotEnterRemoval ::
  DisappearanceSubject ->
  GlobalObjectId ->
  HeraldMembershipGeneration ->
  HeraldState ->
  Assertion
assertMembershipMismatchCannotEnterRemoval subject object membership herald = do
  let local = checkedLocalHeraldEpoch (startupGenesis herald)
      remotes =
        filter
          (/= local)
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))
  retired <- case remotes of
    first : _ -> pure first
    [] -> assertFailure "controlled-removal fixture has no remote member to retire"
  let successorMembership =
        checked
          "controlled-removal mismatched membership"
          (retireHeraldMembershipGeneration (controlIndex 7310) (fixtureRetirementResolution (controlIndex 7310)) retired membership)
      probe = checked "mismatched-membership probe" (deriveDisappearanceProbeId (controlIndex 7311))
      resolveIndex = controlIndex 7312
  (readyLeaf, _) <- reportCompleteLeaf probe subject successorMembership herald
  let outcome =
        checked
          "mismatched-membership Resolve outcome"
          (resolveDisappearanceSubject (checkedSystemId (startupGenesis herald)) resolveIndex subject)
      terminal = ProjectedDisappearanceResolved probe outcome resolveIndex
      supplied = disappearanceSubjectMembershipCoordinate subject successorMembership
      current = disappearanceSubjectMembershipCoordinate subject membership
  case DisappearanceUseCase.coordinateProjectedControlledResolution terminal readyLeaf herald of
    Left
      ( DisappearanceUseCase.DisappearanceControlledRemovalProblem
          (ControlledRemoval.ControlledRemovalCapturedMembershipMismatch actualCurrent actualSupplied)
        ) -> do
        assertEqual "mismatch reports the Herald coordinate" current actualCurrent
        assertEqual "mismatch reports the captured coordinate" supplied actualSupplied
    Left problem -> assertFailure ("unexpected membership mismatch rejection: " <> show problem)
    Right _ -> assertFailure "mismatched membership entered controlled removal"
  assertBool
    "membership mismatch cannot remove the live object"
    (GraphState.graphHasVertex (NeutralVertex object) (startupGraphState herald))

freshAuthority ::
  ProjectedDisappearanceTerminal ->
  DisappearanceState.State ->
  DisappearanceState.ControlledRemovalAuthority
freshAuthority terminal state =
  case DisappearanceState.prepareProjectedControlledResolution terminal state of
    Left problem -> error ("could not reproduce fresh authority: " <> show problem)
    Right prepared ->
      case DisappearanceState.preparedProjectedControlledResolutionAuthority prepared of
        Just authority -> authority
        Nothing -> error "fresh controlled resolution returned no authority"

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure
