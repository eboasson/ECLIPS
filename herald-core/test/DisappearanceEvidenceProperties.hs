{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module DisappearanceEvidenceProperties
  ( tests,
  )
where

import ApplicationLabelProperties
  ( heldStructuralFixtureStateForEvidenceProperties,
    membershipHeldLabelClosureFixture,
    predecessorAdmittedApplicationPublicationFixture,
  )
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Context (deriveContextGraph)
import Eclips.Domain.Disappearance
  ( DisappearanceProbeId,
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    controlledPredefinedDisappearanceSubject,
    deriveDisappearanceEvidenceDigest,
    deriveDisappearanceProbeId,
    deriveRegularSortOccurrenceClaim,
    disappearanceSubjectMembershipCoordinate,
    regularSortDefinitionDisappearanceSubject,
    regularSortOccurrenceClaimOccurrenceId,
    regularSortOccurrenceClaimSortId,
    resolveDisappearanceSubject,
  )
import Eclips.Domain.Graph (VertexId (DeltaVertex))
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    controlIndex,
    firstStructuralSequence,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    mkDeltaId,
    mkGlobalObjectId,
    mkGlobalUniqueId,
    mkLabelDecisionId,
    mkProcessEpochId,
    mkStoreIncarnationId,
    mkStructuralSequence,
    mkTopologyCutId,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationSourceHeraldEpoch,
    sortIdBytes,
    structuralOccurrenceId,
  )
import Eclips.Domain.Label (LabelRevision, mkLabelRevision)
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication qualified as DomainPublication
import Eclips.Domain.Sort.Canonical (CanonicalDescriptor, descriptorSortId)
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, NablaCarrier),
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (DeltaRole, NablaRole, NeutralVertexRole),
    predefinedCatalogueDescriptor,
    profileEntryFor,
  )
import Eclips.Domain.SortOccurrence (SortOccurrenceBase (Genesis))
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.StructuralConsequence qualified as StructuralConsequence
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value
  ( LabelOwner (VoidLabel),
    bytesValue,
    canonicalValueBytes,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.Alignment.Disappearance qualified as Alignment
import Eclips.Herald.Alignment.Generation qualified as AlignmentGeneration
import Eclips.Herald.Alignment.Plan qualified as AlignmentPlan
import Eclips.Herald.Alignment.Plan.Identity qualified as PlanId
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as AlignmentState
import Eclips.Herald.Application.Disappearance qualified as Application
import Eclips.Herald.Application.PublicationEvidence qualified as ApplicationPublication
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortDescriptor,
  )
import Eclips.Herald.Application.State qualified as ApplicationState
import Eclips.Herald.Controlled.Disappearance qualified as Controlled
import Eclips.Herald.Controlled.State qualified as ControlledState
import Eclips.Herald.Disappearance.Evidence qualified as Evidence
import Eclips.Herald.Disappearance.OwnerEvidence qualified as OwnerEvidence
import Eclips.Herald.Disappearance.Protocol
  ( AbsenceAttestationClass (..),
    DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
    DisappearanceInvalidationReason (MatchingPublicationObserved),
    LocalAbsenceReport,
    ProjectedAbortCause (ProjectedAuthorizedAbort),
    ProjectedDisappearanceTerminal (..),
    ProjectedInvalidationCause
      ( ProjectedCommandInvalidation,
        ProjectedLabelInvalidation
      ),
    disappearanceAbsenceAttestationEntries,
    disappearanceBlockerClass,
    disappearanceOpenContextMembership,
    localAbsenceReportClaim,
    matchingPublicationObservation,
    projectedDisappearanceProbe,
  )
import Eclips.Herald.Disappearance.State qualified as DisappearanceState
import Eclips.Herald.Genesis.Internal
  ( checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
    checkedSystemId,
    primordialReplicaOccurrenceId,
    primordialReplicaRole,
  )
import Eclips.Herald.Graph.Disappearance qualified as Graph
import Eclips.Herald.Graph.DisappearanceReadiness qualified as Readiness
import Eclips.Herald.Graph.State qualified as GraphState
import Eclips.Herald.Initialization (HeraldState, initialHerald)
import Eclips.Herald.PeerPayload (peerLogicalPublicationItem)
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PublicationDestination,
    mkPublicationBatch,
    mkStructuralOccurrenceStamp,
    mkStructuralPeerPublication,
    peerPublicationDigest,
    publicationDestination,
    structuralPublicationDigestFor,
  )
import Eclips.Herald.PeerStream
  ( firstStreamSequence,
    mkStreamDirection,
  )
import Eclips.Herald.PeerStream.Disappearance qualified as PeerStream
import Eclips.Herald.PeerStream.State qualified as PeerStreamState
import Eclips.Herald.Placement qualified as PlacementProtocol
import Eclips.Herald.Placement.Disappearance qualified as Placement
import Eclips.Herald.Placement.State qualified as PlacementState
import Eclips.Herald.Publication.Disappearance qualified as Publication
import Eclips.Herald.Publication.State qualified as PublicationState
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( replaceStartupPublicationState,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupGraphState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Store.Disappearance qualified as Store
import Eclips.Herald.Store.State qualified as StoreState
import Eclips.Herald.Structural.Debt qualified as StructuralDebt
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.Disappearance qualified as DisappearanceUseCase
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureCheckedInitialBootstraps,
    fixtureGeneratorSeed,
    fixtureHeraldMembershipGeneration,
    fixtureIdentifierBytes,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
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

tests :: TestTree
tests =
  testGroup
    "disappearance owner evidence"
    [ testCase
        "actual empty owner views compose the controlled and regular attestations"
        caseOwnerComposedAttestations,
      testCase
        "actual non-empty owners expose their exact blocker and matching-write classes"
        caseNonEmptyOwnerMatrix,
      testCase
        "an ordinary accepted owner write feeds matching invalidation"
        caseOrdinaryMatchingOwnerObservation,
      testCase
        "accepted structural work is an unsequenced publication blocker"
        caseStructuralPublicationBlocker,
      testCase
        "a structural carrier exposes its embedded regular-sort dependency"
        caseCarriedRegularSortDependency,
      testCase
        "checked Nabla and Delta peer carriers expose their embedded regular-sort dependency"
        caseCarriedRegularSortPeerOwners,
      testCase
        "retained pending generation evidence blocks every subject it may still name"
        casePendingGenerationEvidenceBlockers,
      testCase
        "unsettled current plans block exact subjects until owner-local acceptance"
        caseUnsettledPlanEvidence,
      testCase
        "bootstrap imports and active attempts expose their retained alignment work"
        caseBootstrapImportEvidence,
      testCase
        "only uncovered debt and current certificates remain live alignment evidence"
        caseAlignmentPromotedHistory,
      testCase
        "superseded placement routes remain retained without blocking disappearance"
        casePlacementSupersededRouteHistory,
      testCase
        "owner composition rejects a mixed-subject snapshot"
        caseOwnerSubjectMismatch,
      testCase
        "Open readiness joins actual Oracle membership and established Graph base"
        caseOwnerBackedOpenReadiness,
      testCase
        "Open readiness rejects successor membership before its Graph base exists"
        caseOwnerBackedOpenReadinessMismatch,
      testCase
        "controlled Resolve mints authority once and exact replay is state-identical"
        caseControlledResolutionAuthority,
      testCase
        "matching publication and relabel races obey projected terminal order"
        caseControlledResolutionProjectionRaces,
      testCase
        "regular Resolve, Invalidated, and Abort cannot mint controlled-removal authority"
        caseControlledResolutionRejectsOtherTerminals
    ]

caseControlledResolutionAuthority :: Assertion
caseControlledResolutionAuthority = do
  let fixture = controlledResolutionFixture
      prepared =
        checked
          "prepare fresh controlled Resolve"
          ( DisappearanceState.prepareProjectedControlledResolution
              (resolutionFixtureTerminal fixture)
              (resolutionFixtureReadyState fixture)
          )
  assertEqual
    "the first controlled Resolve is fresh"
    DisappearanceState.ProjectedControlledResolutionFresh
    (DisappearanceState.preparedProjectedControlledResolutionDisposition prepared)
  authority <-
    case DisappearanceState.preparedProjectedControlledResolutionAuthority prepared of
      Just retained -> pure retained
      Nothing -> assertFailure "fresh controlled Resolve returned no removal authority"
  assertEqual
    "authority names the exact projected probe"
    (resolutionFixtureProbe fixture)
    (DisappearanceState.controlledRemovalAuthorityProbe authority)
  assertEqual
    "authority retains the exact controlled subject"
    (resolutionFixtureSubject fixture)
    (DisappearanceState.controlledRemovalAuthoritySubject authority)
  assertEqual
    "authority retains the exact subject/membership coordinate"
    (resolutionFixtureCoordinate fixture)
    (DisappearanceState.controlledRemovalAuthorityCoordinate authority)
  assertEqual
    "authority retains the subject object"
    (resolutionFixtureObject fixture)
    (DisappearanceState.controlledRemovalAuthorityObject authority)
  assertEqual
    "authority retains the subject structural occurrence"
    (resolutionFixtureOccurrence fixture)
    (DisappearanceState.controlledRemovalAuthorityStructuralOccurrence authority)
  assertEqual
    "authority retains the expected label revision"
    (Just (resolutionFixtureRevision fixture))
    (DisappearanceState.controlledRemovalAuthorityExpectedLabelRevision authority)
  assertEqual
    "authority retains the Resolve control index"
    (resolutionFixtureResolveIndex fixture)
    (DisappearanceState.controlledRemovalAuthorityResolveIndex authority)

  let (resolved, freshDisposition, committedAuthority) =
        DisappearanceState.commitProjectedControlledResolution prepared
      replayPrepared =
        checked
          "prepare duplicate controlled Resolve"
          ( DisappearanceState.prepareProjectedControlledResolution
              (resolutionFixtureTerminal fixture)
              resolved
          )
      (replayed, replayDisposition, replayAuthority) =
        DisappearanceState.commitProjectedControlledResolution replayPrepared
  assertEqual
    "commit preserves the fresh disposition"
    DisappearanceState.ProjectedControlledResolutionFresh
    freshDisposition
  assertEqual
    "commit returns the same fresh authority"
    (Just authority)
    committedAuthority
  assertEqual
    "the exact replay is classified as duplicate"
    DisappearanceState.ProjectedControlledResolutionDuplicate
    (DisappearanceState.preparedProjectedControlledResolutionDisposition replayPrepared)
  assertEqual
    "a duplicate cannot mint a second authority"
    Nothing
    (DisappearanceState.preparedProjectedControlledResolutionAuthority replayPrepared)
  assertEqual
    "duplicate commit preserves the duplicate disposition"
    DisappearanceState.ProjectedControlledResolutionDuplicate
    replayDisposition
  assertEqual "duplicate commit returns no authority" Nothing replayAuthority
  assertEqual "exact replay is state-identical" resolved replayed
  assertEqual
    "the resolved leaf remains valid"
    (Right ())
    (DisappearanceState.validateState resolved)

caseControlledResolutionProjectionRaces :: Assertion
caseControlledResolutionProjectionRaces = do
  let fixture = controlledResolutionFixture
      probe = resolutionFixtureProbe fixture
      subject = resolutionFixtureSubject fixture
      ready = resolutionFixtureReadyState fixture
      earlyResolvedTerminal = resolutionFixtureTerminal fixture
      witness = deriveDisappearanceEvidenceDigest "controlled-resolution-late-publication"
      publicationPosition =
        checked
          "post-cut matching publication position"
          (DomainAlignment.mkHeraldPublicationPosition 1)
      observation =
        matchingPublicationObservation
          subject
          publicationPosition
          witness
      gate =
        maybe
          (error "report-complete probe lost its matching-write gate")
          id
          (DisappearanceState.matchingWriteGateForProbe probe ready)
      matchingInvalidatedTerminal =
        ProjectedDisappearanceInvalidated
          probe
          ( ProjectedCommandInvalidation
              (resolutionFixtureLocalHerald fixture)
              MatchingPublicationObserved
              witness
          )
          (controlIndex 999)
      labelDecision =
        checked
          "racing label decision"
          (mkLabelDecisionId (fixtureIdentifierBytes 223))
      labelInvalidatedTerminal =
        ProjectedDisappearanceInvalidated
          probe
          ( ProjectedLabelInvalidation
              labelDecision
              (resolutionFixtureObject fixture)
          )
          (controlIndex 1000)
      lateResolveIndex = controlIndex 1001
      lateResolvedTerminal =
        ProjectedDisappearanceResolved
          probe
          ( checked
              "Resolve outcome ordered after racing invalidations"
              ( resolveDisappearanceSubject
                  (checkedSystemId fixtureCheckedGenesis)
                  lateResolveIndex
                  subject
              )
          )
          lateResolveIndex

  (matchingObserved, matchingIntentions) <-
    checkedIO
      "observe a matching publication before Resolve"
      ( DisappearanceUseCase.coordinateMatchingPublicationObservation
          gate
          observation
          ready
      )
  assertEqual
    "a fresh post-cut publication retains one invalidation intention"
    1
    (length matchingIntentions)
  matchingInvalidated <-
    installProjectedTerminal
      "install matching-publication invalidation before Resolve"
      matchingInvalidatedTerminal
      matchingObserved
  assertTerminalOrderRejectsResolve
    "matching publication before Resolve"
    probe
    lateResolvedTerminal
    matchingInvalidated

  labelInvalidated <-
    installProjectedTerminal
      "install relabel invalidation before Resolve"
      labelInvalidatedTerminal
      ready
  assertTerminalOrderRejectsResolve
    "relabel before Resolve"
    probe
    lateResolvedTerminal
    labelInvalidated

  let (resolved, _, _) = commitControlledResolution earlyResolvedTerminal ready
  (afterLatePublication, latePublicationIntentions) <-
    checkedIO
      "observe a matching publication after Resolve"
      ( DisappearanceUseCase.coordinateMatchingPublicationObservation
          gate
          observation
          resolved
      )
  assertEqual
    "Resolve-first suppresses a late matching-publication intention"
    []
    latePublicationIntentions
  assertEqual
    "Resolve-first leaves the terminal leaf unchanged on late publication"
    resolved
    afterLatePublication
  assertTerminalOrderRejectsInvalidation
    "Resolve before matching-publication invalidation"
    probe
    matchingInvalidatedTerminal
    resolved
  assertTerminalOrderRejectsInvalidation
    "Resolve before relabel invalidation"
    probe
    labelInvalidatedTerminal
    resolved
  assertEqual
    "both winning terminal orders preserve a valid disappearance leaf"
    (Right ())
    (DisappearanceState.validateState resolved)

installProjectedTerminal ::
  String ->
  ProjectedDisappearanceTerminal ->
  DisappearanceState.State ->
  IO DisappearanceState.State
installProjectedTerminal context terminal state = do
  prepared <-
    checkedIO
      context
      (DisappearanceState.prepareProjectedTerminal terminal state)
  let (successor, disposition) =
        DisappearanceState.commitDisappearanceTransition prepared
  assertEqual (context <> ": disposition") DisappearanceState.TerminalInstalled disposition
  pure successor

assertTerminalOrderRejectsResolve ::
  String ->
  DisappearanceProbeId ->
  ProjectedDisappearanceTerminal ->
  DisappearanceState.State ->
  Assertion
assertTerminalOrderRejectsResolve context probe terminal state =
  case DisappearanceState.prepareProjectedControlledResolution terminal state of
    Left (DisappearanceState.DisappearanceTerminalConflict actual) ->
      assertEqual (context <> ": exact probe") probe actual
    Left problem ->
      assertFailure (context <> ": unexpected Resolve rejection: " <> show problem)
    Right _ -> assertFailure (context <> ": losing Resolve minted removal authority")

assertTerminalOrderRejectsInvalidation ::
  String ->
  DisappearanceProbeId ->
  ProjectedDisappearanceTerminal ->
  DisappearanceState.State ->
  Assertion
assertTerminalOrderRejectsInvalidation context probe terminal state =
  case DisappearanceState.prepareProjectedTerminal terminal state of
    Left (DisappearanceState.DisappearanceTerminalConflict actual) ->
      assertEqual (context <> ": exact probe") probe actual
    Left problem ->
      assertFailure (context <> ": unexpected invalidation rejection: " <> show problem)
    Right _ -> assertFailure (context <> ": losing invalidation replaced Resolve")

caseControlledResolutionRejectsOtherTerminals :: Assertion
caseControlledResolutionRejectsOtherTerminals = do
  let fixture = controlledResolutionFixture
      regularSubject = fixtureRegularSubject
      regularOutcome =
        checked
          "resolve the regular subject"
          ( resolveDisappearanceSubject
              (checkedSystemId fixtureCheckedGenesis)
              (resolutionFixtureResolveIndex fixture)
              regularSubject
          )
      regularTerminal =
        ProjectedDisappearanceResolved
          (resolutionFixtureProbe fixture)
          regularOutcome
          (resolutionFixtureResolveIndex fixture)
      invalidatedTerminal =
        ProjectedDisappearanceInvalidated
          (resolutionFixtureProbe fixture)
          ( ProjectedCommandInvalidation
              (resolutionFixtureLocalHerald fixture)
              MatchingPublicationObserved
              (deriveDisappearanceEvidenceDigest "controlled-resolution-rejection")
          )
          (resolutionFixtureResolveIndex fixture)
      abortedTerminal =
        ProjectedDisappearanceAborted
          (resolutionFixtureProbe fixture)
          ProjectedAuthorizedAbort
          (resolutionFixtureResolveIndex fixture)
  assertControlledResolutionRejected
    "regular Resolve"
    ( DisappearanceState.DisappearanceControlledResolutionSubjectRegular
        (resolutionFixtureProbe fixture)
        regularSubject
    )
    regularTerminal
    (resolutionFixtureReadyState fixture)
  assertControlledResolutionRejected
    "Invalidated"
    ( DisappearanceState.DisappearanceControlledResolutionTerminalInvalidated
        (resolutionFixtureProbe fixture)
    )
    invalidatedTerminal
    (resolutionFixtureReadyState fixture)
  assertControlledResolutionRejected
    "Abort"
    ( DisappearanceState.DisappearanceControlledResolutionTerminalAborted
        (resolutionFixtureProbe fixture)
    )
    abortedTerminal
    (resolutionFixtureReadyState fixture)

  let (resolved, acceptedDisposition, acceptedAuthority) =
        commitControlledResolution
          (resolutionFixtureTerminal fixture)
          (resolutionFixtureReadyState fixture)
  assertEqual
    "the rejected source remains valid"
    (Right ())
    (DisappearanceState.validateState (resolutionFixtureReadyState fixture))
  assertEqual
    "the same source still accepts the controlled Resolve as fresh"
    DisappearanceState.ProjectedControlledResolutionFresh
    acceptedDisposition
  assertBool
    "the accepted retry still carries authority"
    (case acceptedAuthority of Just _ -> True; Nothing -> False)
  assertEqual
    "the accepted successor remains valid"
    (Right ())
    (DisappearanceState.validateState resolved)

assertControlledResolutionRejected ::
  String ->
  DisappearanceState.DisappearanceProblem ->
  ProjectedDisappearanceTerminal ->
  DisappearanceState.State ->
  Assertion
assertControlledResolutionRejected context expected terminal state =
  case DisappearanceState.prepareProjectedControlledResolution terminal state of
    Left problem -> assertEqual (context <> " rejection") expected problem
    Right _ -> assertFailure (context <> " unexpectedly prepared controlled removal")

commitControlledResolution ::
  ProjectedDisappearanceTerminal ->
  DisappearanceState.State ->
  ( DisappearanceState.State,
    DisappearanceState.ProjectedControlledResolutionDisposition,
    Maybe DisappearanceState.ControlledRemovalAuthority
  )
commitControlledResolution terminal state =
  DisappearanceState.commitProjectedControlledResolution
    ( checked
        "prepare accepted controlled Resolve after rejected specialized terminals"
        (DisappearanceState.prepareProjectedControlledResolution terminal state)
    )

data ControlledResolutionFixture = ControlledResolutionFixture
  { resolutionFixtureLocalHerald :: HeraldEpoch,
    resolutionFixtureProbe :: DisappearanceProbeId,
    resolutionFixtureSubject :: DisappearanceSubject,
    resolutionFixtureCoordinate :: DisappearanceSubjectMembershipCoordinate,
    resolutionFixtureObject :: GlobalObjectId,
    resolutionFixtureOccurrence :: StructuralOccurrenceId,
    resolutionFixtureRevision :: LabelRevision,
    resolutionFixtureResolveIndex :: ControlIndex,
    resolutionFixtureTerminal :: ProjectedDisappearanceTerminal,
    resolutionFixtureReadyState :: DisappearanceState.State
  }

controlledResolutionFixture :: ControlledResolutionFixture
controlledResolutionFixture =
  ControlledResolutionFixture
    { resolutionFixtureLocalHerald = localHerald,
      resolutionFixtureProbe = probe,
      resolutionFixtureSubject = subject,
      resolutionFixtureCoordinate = coordinate,
      resolutionFixtureObject = object,
      resolutionFixtureOccurrence = occurrence,
      resolutionFixtureRevision = revision,
      resolutionFixtureResolveIndex = resolveIndex,
      resolutionFixtureTerminal = terminal,
      resolutionFixtureReadyState = readyState
    }
  where
    -- The specialized seam cares about an already-complete report set.  A
    -- one-member generation reaches that real leaf state without introducing
    -- unrelated peer-marker scheduling into this focused regression.
    localHerald = checkedLocalHeraldEpoch fixtureStep14CheckedGenesis
    membership =
      checked
        "single-Herald controlled-resolution membership"
        ( Membership.genesisHeraldMembershipGeneration
            (checkedSystemId fixtureCheckedGenesis)
            (localHerald :| [])
        )
    object = checked "controlled-resolution object" (mkGlobalObjectId (fixtureIdentifierBytes 222))
    occurrence =
      structuralOccurrenceId
        localHerald
        (checked "controlled-resolution structural sequence" (mkStructuralSequence 995))
    revision = checked "controlled-resolution label revision" (mkLabelRevision (controlIndex 996))
    subject =
      checked
        "controlled-resolution subject"
        ( controlledPredefinedDisappearanceSubject
            NeutralVertexRole
            object
            occurrence
            (Just revision)
        )
    probe = checked "controlled-resolution probe" (deriveDisappearanceProbeId (controlIndex 997))
    coordinate = disappearanceSubjectMembershipCoordinate subject membership
    projected =
      checked
        "controlled-resolution projected probe"
        (projectedDisappearanceProbe probe subject coordinate membership)
    snapshot = emptyOwnerSnapshot localHerald subject
    (opened, _) =
      DisappearanceState.commitDisappearanceTransition
        ( checked
            "open controlled-resolution probe"
            ( DisappearanceState.prepareProjectedOpen
                projected
                snapshot
                (DisappearanceState.initialState localHerald)
            )
        )
    (withAssignments, ()) =
      DisappearanceState.commitDisappearanceTransition
        ( checked
            "complete empty controlled-resolution marker assignment set"
            (DisappearanceState.preparePublicationMarkerAssignments probe [] opened)
        )
    (cutComplete, _) =
      DisappearanceState.commitDisappearanceTransition
        ( checked
            "complete controlled-resolution local publication cut"
            ( DisappearanceState.prepareLocalPublicationCutCompletion
                probe
                (Evidence.disappearanceEvidenceSnapshotLocalPublicationCut snapshot)
                withAssignments
            )
        )
    (reported, reportDisposition) =
      DisappearanceState.commitDisappearanceTransition
        ( checked
            "prepare controlled-resolution local absence report"
            ( DisappearanceState.prepareLocalAbsenceReportFromEvidence
                probe
                snapshot
                cutComplete
            )
        )
    report = reportFromDisposition reportDisposition
    (readyState, _) =
      DisappearanceState.commitDisappearanceTransition
        ( checked
            "project controlled-resolution local report"
            ( DisappearanceState.prepareProjectedReport
                (localAbsenceReportClaim report)
                reported
            )
        )
    resolveIndex = controlIndex 998
    outcome =
      checked
        "resolve controlled-resolution subject"
        ( resolveDisappearanceSubject
            (checkedSystemId fixtureCheckedGenesis)
            resolveIndex
            subject
        )
    terminal = ProjectedDisappearanceResolved probe outcome resolveIndex

reportFromDisposition :: DisappearanceState.ReportDisposition -> LocalAbsenceReport
reportFromDisposition disposition = case disposition of
  DisappearanceState.LocalReportPrepared report -> report
  DisappearanceState.LocalReportDuplicate report -> report

emptyOwnerSnapshot ::
  HeraldEpoch ->
  DisappearanceSubject ->
  Evidence.DisappearanceEvidenceSnapshot
emptyOwnerSnapshot localHerald subject =
  checked
    "empty-owner disappearance evidence"
    ( Evidence.disappearanceEvidenceSnapshot
        ( Store.storeDisappearanceView
            subject
            (checked "initial Store owner" (StoreState.initialState fixtureStep14CheckedGenesis))
        )
        (Application.applicationDisappearanceView subject ApplicationState.emptyState)
        ( Publication.publicationDisappearanceView
            subject
            (PublicationState.initialState localHerald)
        )
        ( checked
            "empty PeerStream disappearance view"
            ( PeerStream.peerStreamDisappearanceView
                subject
                (PeerStreamState.initialState localHerald)
            )
        )
        (Alignment.alignmentDisappearanceView subject AlignmentState.emptyState)
        (Controlled.controlledDisappearanceView subject ControlledState.emptyState)
        (Graph.graphDisappearanceView subject GraphState.emptyState)
        (Placement.placementDisappearanceView subject PlacementState.emptyState)
    )

caseOwnerComposedAttestations :: Assertion
caseOwnerComposedAttestations = do
  let state = fixtureHeraldState
      controlled = ownerSnapshot fixtureControlledSubject state
      regular = ownerSnapshot fixtureRegularSubject state
  assertEqual
    "the unrelated controlled subject has no live owner fact"
    []
    (Evidence.disappearanceEvidenceSnapshotBlockers controlled)
  assertEqual
    "the fresh regular occurrence has no live owner fact"
    []
    (Evidence.disappearanceEvidenceSnapshotBlockers regular)
  assertEqual
    "controlled evidence has exactly the four common owner classes"
    [ ApplicationStoreAbsence,
      PublicationWorkAbsence,
      PeerStreamWorkAbsence,
      AlignmentWorkAbsence
    ]
    (attestationClasses controlled)
  assertEqual
    "regular evidence adds Graph/Placement and Controlled owner classes"
    [ ApplicationStoreAbsence,
      PublicationWorkAbsence,
      PeerStreamWorkAbsence,
      AlignmentWorkAbsence,
      GraphDependencyAbsence,
      ControlledUseAbsence
    ]
    (attestationClasses regular)

caseNonEmptyOwnerMatrix :: Assertion
caseNonEmptyOwnerMatrix = do
  state <- heldStructuralFixtureStateForEvidenceProperties
  assertEqual "pending source and held work form a valid composed Herald" (Right ()) (validateHeraldState state)
  let controlledSubject = heldControlledSubject state
      storeView = Store.storeDisappearanceView controlledSubject (startupStoreState state)
      applicationView =
        Application.applicationDisappearanceView
          controlledSubject
          (startupApplicationState state)
      publicationView =
        Publication.publicationDisappearanceView
          controlledSubject
          (startupPublicationState state)
      peerView =
        checked
          "non-empty PeerStream disappearance view"
          ( PeerStream.peerStreamDisappearanceView
              controlledSubject
              (startupPeerStreamState state)
          )
      alignmentView =
        Alignment.alignmentDisappearanceView
          controlledSubject
          (startupAlignmentState state)
      controlledView =
        Controlled.controlledDisappearanceView
          controlledSubject
          (startupControlledState state)
      graphView =
        Graph.graphDisappearanceView
          controlledSubject
          (startupGraphState state)
      placementView =
        Placement.placementDisappearanceView
          controlledSubject
          (startupPlacementState state)
      composed = ownerSnapshot controlledSubject state
      expectedComposedBlockers =
        Store.storeDisappearanceBlockers storeView
          <> Application.applicationDisappearanceBlockers applicationView
          <> Publication.publicationDisappearanceBlockers publicationView
          <> PeerStream.peerStreamDisappearanceBlockers peerView
          <> Alignment.alignmentDisappearanceBlockers alignmentView
          <> Controlled.controlledDisappearanceBlockers controlledView
          <> Graph.graphDisappearanceBlockers graphView
          <> Placement.placementDisappearanceBlockers placementView
      alignmentClasses = blockerClasses (Alignment.alignmentDisappearanceBlockers alignmentView)
  assertEqual
    "the application-visible Store copy is classified from the retained Store slot"
    [VisibleApplicationCopyBlocker]
    (blockerClasses (Store.storeDisappearanceBlockers storeView))
  assertEqual
    "both actually fence-held operations are classified by Application"
    [HeldPublicationBlocker, HeldPublicationBlocker]
    (blockerClasses (Application.applicationDisappearanceBlockers applicationView))
  assertEqual
    "completed Publication history is not itself a blocker"
    []
    (Publication.publicationDisappearanceBlockers publicationView)
  assertEqual
    "Publication nevertheless exposes the retained matching write"
    1
    (Publication.publicationDisappearanceMatchingObservationCount publicationView)
  assertEqual
    "both active remote assignments are classified from PeerStream"
    [PeerOutboxBlocker, PeerOutboxBlocker]
    (blockerClasses (PeerStream.peerStreamDisappearanceBlockers peerView))
  assertBool
    "the actual unsettled structural debts are all alignment blockers for this object"
    (not (null alignmentClasses) && all (== AlignmentDebtBlocker) alignmentClasses)
  assertEqual
    "composition contains exactly the facts returned by the eight opaque owner views"
    (sort expectedComposedBlockers)
    (Evidence.disappearanceEvidenceSnapshotBlockers composed)
  assertEqual
    "a non-empty actual owner read cannot manufacture an absence attestation"
    Nothing
    (Evidence.disappearanceEvidenceSnapshotAbsenceAttestation composed)
  assertEqual
    "composition preserves Publication's actual positioned matching observations"
    (Publication.publicationDisappearanceMatchingObservations publicationView)
    (Evidence.disappearanceEvidenceSnapshotMatchingPublications composed)

  -- Eligibility is Oracle-owned.  The disappearance adapters themselves are
  -- coordinate classifiers, so the exact initialized Neutral-root occurrence
  -- provides a compact real-state check of the three regular-only owners.
  let occurrenceSubject = fixtureNeutralRootOccurrenceSubject
      controlledClasses =
        blockerClasses
          ( Controlled.controlledDisappearanceBlockers
              ( Controlled.controlledDisappearanceView
                  occurrenceSubject
                  (startupControlledState state)
              )
          )
      graphClasses =
        blockerClasses
          ( Graph.graphDisappearanceBlockers
              ( Graph.graphDisappearanceView
                  occurrenceSubject
                  (startupGraphState state)
              )
          )
      placementClasses =
        blockerClasses
          ( Placement.placementDisappearanceBlockers
              ( Placement.placementDisappearanceView
                  occurrenceSubject
                  (startupPlacementState state)
              )
          )
  assertEqual
    "Controlled reports the bootstrap hub and the created vertex as current uses of the exact occurrence"
    [CurrentControlledUseBlocker, CurrentControlledUseBlocker]
    controlledClasses
  assertEqual "Graph reports both local and remote active Nabla roots" 2 (countBlocker ActiveNablaBlocker graphClasses)
  assertEqual "Graph reports both local and remote active Delta roots" 2 (countBlocker ActiveDeltaBlocker graphClasses)
  assertEqual "Graph emits no unrelated blocker class" 4 (length graphClasses)
  assertEqual "Placement reports both current qualified route dependencies" 2 (countBlocker RouteDependencyBlocker placementClasses)
  assertEqual "Placement reports both current physical dependencies" 2 (countBlocker PlacementDependencyBlocker placementClasses)
  assertEqual "Placement emits no unrelated blocker class" 4 (length placementClasses)

caseOrdinaryMatchingOwnerObservation :: Assertion
caseOrdinaryMatchingOwnerObservation = do
  state <- heldStructuralFixtureStateForEvidenceProperties
  let subject = heldControlledSubject state
      sourcePublication = case PublicationState.stampedStructuralStageEntries (startupPublicationState state) of
        [(_, stage)] -> stage
        stages -> error ("expected one materialized publication stage, got " <> show (length stages))
      applicationRecord = case PublicationState.stampedStructuralApplicationRecordMaybe sourcePublication of
        Nothing -> error "materialized fixture stage lost its application provenance"
        Just retained -> retained
      checkedPublication = PublicationState.stampedStructuralChecked sourcePublication
      identifier = DomainPublication.checkedPublicationId checkedPublication
      initialPublication =
        PublicationState.initialState (checkedLocalHeraldEpoch fixtureStep14CheckedGenesis)
      candidate =
        PublicationState.proposeOutgoingPublication
          (publicationNabla identifier)
          (publicationAuthorityEpoch identifier)
          initialPublication
      acceptedPublication =
        fst
          ( PublicationState.commitOutgoingPublication
              ( checked
                  "prepare the ordinary matching publication owner"
                  ( PublicationState.prepareOutgoingPublication
                      candidate
                      checkedPublication
                      (PublicationState.stampedStructuralSourceProcess sourcePublication)
                      (PublicationState.applicationPublicationAcceptancePosition applicationRecord)
                      (PublicationState.stampedStructuralSortOccurrenceId sourcePublication)
                      (PublicationState.stampedStructuralSourceStrength sourcePublication)
                      (PublicationState.stampedStructuralSourceTopologyPrerequisite sourcePublication)
                      (PublicationState.stampedStructuralControlPrerequisite sourcePublication)
                      (PublicationState.stampedStructuralRoute sourcePublication)
                      initialPublication
                  )
              )
          )
      beforeOwners = replaceStartupPublicationState initialPublication state
      afterOwners = replaceStartupPublicationState acceptedPublication state
      beforeSnapshot = ownerSnapshot subject beforeOwners
      afterSnapshot = ownerSnapshot subject afterOwners
      identifierProbe = checked "owner matching probe" (deriveDisappearanceProbeId (controlIndex 91))
      coordinate = disappearanceSubjectMembershipCoordinate subject fixtureHeraldMembershipGeneration
      projected =
        checked
          "owner matching projected probe"
          ( projectedDisappearanceProbe
              identifierProbe
              subject
              coordinate
              fixtureHeraldMembershipGeneration
          )
      (opened, _) =
        DisappearanceState.commitDisappearanceTransition
          ( checked
              "open against the pre-publication owner read"
              ( DisappearanceState.prepareProjectedOpen
                  projected
                  beforeSnapshot
                  (DisappearanceState.initialState (checkedLocalHeraldEpoch fixtureStep14CheckedGenesis))
              )
          )
      (invalidated, intentions) =
        checked
          "coordinate the post-cut owner matching read"
          ( DisappearanceUseCase.coordinateEvidenceObservation
              identifierProbe
              afterSnapshot
              opened
          )
      logicalWork =
        DisappearanceState.disappearanceLogicalWork
          . DisappearanceState.disappearanceWorkLedger
  assertEqual
    "the generic outgoing owner retains the exact checked publication identity"
    identifier
    (PublicationState.outgoingCandidatePublicationId candidate)
  assertEqual
    "the pre-publication owner read has no matching write"
    []
    (Evidence.disappearanceEvidenceSnapshotMatchingPublications beforeSnapshot)
  assertEqual
    "the accepted ordinary owner record emits one positioned matching write"
    1
    (length (Evidence.disappearanceEvidenceSnapshotMatchingPublications afterSnapshot))
  assertEqual "the real owner observation is immediately invalidation-ready" 1 (length intentions)
  assertEqual
    "one actual matching write charges observation plus stable Invalidate"
    2
    (logicalWork invalidated - logicalWork opened)

caseStructuralPublicationBlocker :: Assertion
caseStructuralPublicationBlocker = do
  state <- predecessorAdmittedApplicationPublicationFixture
  let applicationRecord = case PublicationState.applicationPublicationEntries (startupPublicationState state) of
        [(_, retained)] -> retained
        records -> error ("expected one accepted application publication, got " <> show (length records))
      object = case applicationOriginObject (PublicationState.applicationPublicationOrigin applicationRecord) of
        Nothing -> error "accepted structural fixture publication has no controlled object"
        Just retained -> retained
      subject = controlledSubjectAt object 992
      freshOwner = PublicationState.initialState (checkedLocalHeraldEpoch fixtureStep14CheckedGenesis)
      (unstampedOwner, _, _) =
        PublicationState.commitApplicationPublication
          ( checked
              "re-admit an actually accepted structural publication without stamping it"
              ( PublicationState.prepareApplicationPublication
                  (publicationNabla (PublicationState.applicationPublicationId applicationRecord))
                  Nothing
                  (publicationAuthorityEpoch (PublicationState.applicationPublicationId applicationRecord))
                  (predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole))
                  (PublicationState.applicationPublicationAdmittedValue applicationRecord)
                  (PublicationState.applicationPublicationOrigin applicationRecord)
                  (PublicationState.applicationPublicationMembershipGenerationId applicationRecord)
                  (PublicationState.applicationPublicationSourceProcess applicationRecord)
                  (PublicationState.applicationPublicationAcceptancePosition applicationRecord)
                  (PublicationState.applicationPublicationSortOccurrenceId applicationRecord)
                  (PublicationState.applicationPublicationSourceStrength applicationRecord)
                  (PublicationState.applicationPublicationSourceTopologyPrerequisite applicationRecord)
                  (PublicationState.applicationPublicationControlPrerequisite applicationRecord)
                  (PublicationState.applicationPublicationRoute applicationRecord)
                  freshOwner
              )
          )
      view = Publication.publicationDisappearanceView subject unstampedOwner
  assertEqual
    "the actual accepted-but-unstamped source stage is a publication blocker"
    [UnsequencedPublicationBlocker]
    (blockerClasses (Publication.publicationDisappearanceBlockers view))

caseCarriedRegularSortDependency :: Assertion
caseCarriedRegularSortDependency = do
  state <- predecessorAdmittedApplicationPublicationFixture
  let template = case PublicationState.applicationPublicationEntries (startupPublicationState state) of
        [(_, retained)] -> retained
        records -> error ("expected one accepted application publication, got " <> show (length records))
      carrierObject = checked "carrier object" (mkGlobalUniqueId (fixtureIdentifierBytes 231))
      carrierDescriptor = predefinedCatalogueDescriptor (profileEntryFor NablaRole)
      carrierValue =
        checked
          "Nabla carrier with a regular target sort"
          ( recordValue
              [ (checked "object_id field" (mkFieldName "object_id"), globalUniqueIdValue carrierObject),
                (checked "label field" (mkFieldName "label"), labelValue (VoidLabel, 0)),
                ( checked "sequencing_object field" (mkFieldName "sequencing_object"),
                  optionalGlobalUniqueIdValue Nothing
                ),
                ( checked "sort_id field" (mkFieldName "sort_id"),
                  bytesValue (sortIdBytes (descriptorSortId fixtureRegularDescriptor))
                )
              ]
          )
      initialOwner = PublicationState.initialState (checkedLocalHeraldEpoch fixtureStep14CheckedGenesis)
      (carrierOwner, _, carrierRecord) =
        PublicationState.commitApplicationPublication
          ( checked
              "retain the carried-sort structural source"
              ( PublicationState.prepareApplicationPublication
                  (publicationNabla (PublicationState.applicationPublicationId template))
                  Nothing
                  (publicationAuthorityEpoch (PublicationState.applicationPublicationId template))
                  carrierDescriptor
                  (ApplicationPublication.admittedOrdinaryApplicationPublicationValue carrierValue)
                  (ApplicationPublication.ControlledFirstUseApplicationPublicationOrigin carrierObject)
                  (PublicationState.applicationPublicationMembershipGenerationId template)
                  (PublicationState.applicationPublicationSourceProcess template)
                  (PublicationState.applicationPublicationAcceptancePosition template)
                  (PublicationState.applicationPublicationSortOccurrenceId template)
                  (PublicationState.applicationPublicationSourceStrength template)
                  (PublicationState.applicationPublicationSourceTopologyPrerequisite template)
                  (PublicationState.applicationPublicationControlPrerequisite template)
                  (PublicationState.applicationPublicationRoute template)
                  initialOwner
              )
          )
      publication = PublicationState.applicationPublicationChecked carrierRecord
      targetView = Publication.publicationDisappearanceView fixtureRegularSubject carrierOwner
      unrelatedView = Publication.publicationDisappearanceView fixtureOtherRegularSubject carrierOwner
  assertBool
    "the central checked-publication predicate sees the embedded target sort"
    (OwnerEvidence.checkedPublicationReferencesSubjectSort fixtureRegularSubject publication)
  assertBool
    "the same carrier does not reference an unrelated regular sort"
    (not (OwnerEvidence.checkedPublicationReferencesSubjectSort fixtureOtherRegularSubject publication))
  assertBool
    "a user record with a sort_id field is not structural evidence when its outer sort is regular"
    ( not
        ( OwnerEvidence.canonicalPublicationReferencesSubjectSort
            fixtureRegularSubject
            (descriptorSortId fixtureOtherRegularDescriptor)
            (canonicalValueBytes carrierValue)
        )
    )
  assertEqual
    "the actual local unsequenced owner blocks disappearance of the carried sort"
    [UnsequencedPublicationBlocker]
    (blockerClasses (Publication.publicationDisappearanceBlockers targetView))
  assertEqual
    "the owner does not over-classify an unrelated carried sort"
    []
    (Publication.publicationDisappearanceBlockers unrelatedView)

caseCarriedRegularSortPeerOwners :: Assertion
caseCarriedRegularSortPeerOwners =
  mapM_
    assertCarrier
    [ (NablaRole, NablaCarrier),
      (DeltaRole, DeltaCarrier)
    ]
  where
    assertCarrier (role, carrierRole) = do
      (publication, destination, source, peer, sourceProcess) <-
        carriedRegularSortPeerPublication role carrierRole
      let direction =
            checked
              ("stream direction for " <> show role)
              (mkStreamDirection source peer)
          resolution =
            checked
              ("held incoming resolution for " <> show role)
              ( PublicationState.incomingPublicationResolution
                  ( Set.singleton
                      ( PublicationState.StructuralApplicationDependency
                          (Reconciliation.StructuralProcessDependency sourceProcess)
                      )
                  )
                  (Map.singleton destination PublicationState.DestinationPending)
              )
          candidate =
            checked
              ("incoming Publication candidate for " <> show role)
              ( PublicationState.prepareIncomingPublicationCandidate
                  (Membership.heraldMembershipGenerationId fixtureHeraldMembershipGeneration)
                  direction
                  firstStreamSequence
                  (peerPublicationDigest publication)
                  publication
                  (PublicationState.initialState peer)
              )
          prepared =
            checked
              ("dependency-held incoming Publication for " <> show role)
              (PublicationState.finalizeIncomingPublication (Just resolution) candidate)
          (incomingOwner, _) = PublicationState.commitIncomingPublication prepared
          targetPublicationView =
            Publication.publicationDisappearanceView fixtureRegularSubject incomingOwner
          unrelatedPublicationView =
            Publication.publicationDisappearanceView fixtureOtherRegularSubject incomingOwner
          streamRequest = peerLogicalPublicationItem publication :| []
          preparedEnqueue =
            checked
              ("active peer outbox for " <> show role)
              ( PeerStreamState.prepareEnqueue
                  (Map.singleton peer streamRequest)
                  (PeerStreamState.initialState source)
              )
          (streamOwner, _) = PeerStreamState.commitEnqueue preparedEnqueue
          targetStreamView =
            checked
              ("target PeerStream view for " <> show role)
              (PeerStream.peerStreamDisappearanceView fixtureRegularSubject streamOwner)
          unrelatedStreamView =
            checked
              ("unrelated PeerStream view for " <> show role)
              (PeerStream.peerStreamDisappearanceView fixtureOtherRegularSubject streamOwner)
      assertEqual
        (show role <> " incoming Publication retains the carried target sort")
        [UnresolvedStructuralReferenceBlocker]
        (blockerClasses (Publication.publicationDisappearanceBlockers targetPublicationView))
      assertEqual
        (show role <> " incoming Publication ignores an unrelated carried sort")
        []
        (Publication.publicationDisappearanceBlockers unrelatedPublicationView)
      assertEqual
        (show role <> " active peer outbox retains the carried target sort")
        [PeerOutboxBlocker]
        (blockerClasses (PeerStream.peerStreamDisappearanceBlockers targetStreamView))
      assertEqual
        (show role <> " active peer outbox ignores an unrelated carried sort")
        []
        (PeerStream.peerStreamDisappearanceBlockers unrelatedStreamView)

casePendingGenerationEvidenceBlockers :: Assertion
casePendingGenerationEvidenceBlockers = do
  let cut = pendingEvidenceAlignmentCut
      generation = DomainAlignment.deriveContextClassGenerationId cut
      memberDelta = pendingEvidenceDelta
      memberStore = pendingEvidenceStore
      remote = pendingEvidenceHerald
      announcedOwner =
        retainPendingGenerationEvidence
          remote
          ( AlignmentProtocol.AlignmentCutAnnounced
              (AlignmentProtocol.alignmentCutAnnounce cut)
          )
          AlignmentState.emptyState
      announcedClasses subject =
        blockerClasses
          ( Alignment.alignmentDisappearanceBlockers
              (Alignment.alignmentDisappearanceView subject announcedOwner)
          )
      memberSubject =
        controlledSubjectAt (globalObjectIdFromDeltaId memberDelta) 993
      placement = pendingEvidencePlacement
      topologyId = Topology.deriveTopologyCutId pendingEvidenceTopologyCut
      planId =
        checked
          "pending plan identifier"
          ( PlanId.alignmentPlanIdAtTopology
              (StructuralDebt.sortOccurrence (DomainAlignment.alignmentCutSortId cut) (DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut))
              pendingEvidenceTopologyCut
              placement
          )
      planBinding = checked "pending plan binding" (AlignmentProtocol.alignmentPlanBindingClaim (memberDelta :| []) AlignmentProtocol.AlignmentCreated generation)
      planAnnounce = checked "pending plan announcement" (AlignmentProtocol.alignmentPlanAnnounce planId pendingEvidenceTopologyCut Nothing AlignmentPlan.AlignmentPredecessorUsable [planBinding] [] [AlignmentProtocol.alignmentCutAnnounce cut])
      pendingPlanOwner = retainPendingGenerationEvidence remote (AlignmentProtocol.AlignmentPlanAnnounced planAnnounce) AlignmentState.emptyState
      planClasses subject = blockerClasses (Alignment.alignmentDisappearanceBlockers (Alignment.alignmentDisappearanceView subject pendingPlanOwner))
      ready =
        AlignmentProtocol.classMemberReady
          AlignmentProtocol.firstAlignmentEvidenceSequence
          generation
          memberStore
          (DomainAlignment.deriveMemberReadyEvidenceDigest "pending-member-ready")
          DomainAlignment.initialStoreRevision
      unresolvedControls =
        [ AlignmentProtocol.AlignmentCutAcceptanceAdvertised
            ( AlignmentProtocol.alignmentCutAccepted
                generation
                remote
                topologyId
                placement
                DomainAlignment.EmptyHeraldPublicationPrefix
            ),
          AlignmentProtocol.AlignmentPlanAcceptanceAdvertised
            (checked "pending plan acceptance" (AlignmentProtocol.alignmentPlanAccepted planId remote DomainAlignment.EmptyHeraldPublicationPrefix)),
          AlignmentProtocol.AlignmentMemberReadyAdvertised ready,
          AlignmentProtocol.AlignmentHistoricalCertificateAdvertised
            ( AlignmentProtocol.historicalCertificate
                generation
                memberStore
                (AlignmentProtocol.classMemberReadySetDigest [ready])
                (DomainAlignment.deriveBootstrapEvidenceDigest "pending-certificate")
                DomainAlignment.initialStoreRevision
            )
        ]
      unresolvedOwner =
        foldl
          (\owner control -> retainPendingGenerationEvidence remote control owner)
          AlignmentState.emptyState
          unresolvedControls
      unresolvedClasses subject =
        blockerClasses
          ( Alignment.alignmentDisappearanceBlockers
              (Alignment.alignmentDisappearanceView subject unresolvedOwner)
          )
      fourUncertain = replicate 4 UncertainSemanticPayloadBlocker
  assertEqual
    "the retained full-cut pending owner remains valid"
    (Right ())
    (AlignmentState.validateAlignmentState announcedOwner)
  assertEqual
    "the retained generation-only pending owner remains valid"
    (Right ())
    (AlignmentState.validateAlignmentState unresolvedOwner)
  assertEqual "the retained pending plan owner remains valid" (Right ()) (AlignmentState.validateAlignmentState pendingPlanOwner)
  assertEqual "pending plan binds its exact regular occurrence" [AlignmentObligationBlocker] (planClasses fixtureRegularSubject)
  assertEqual "pending plan excludes unrelated regular occurrences" [] (planClasses fixtureOtherRegularSubject)
  assertEqual "pending plan binds its exact controlled Delta member" [AlignmentObligationBlocker] (planClasses memberSubject)
  assertEqual "pending plan excludes unrelated controlled objects" [] (planClasses fixtureControlledSubject)
  assertEqual
    "the full pending cut is an exact regular occurrence dependency"
    [AlignmentObligationBlocker]
    (announcedClasses fixtureRegularSubject)
  assertEqual
    "the full pending cut does not implicate another regular occurrence"
    []
    (announcedClasses fixtureOtherRegularSubject)
  assertEqual
    "the full pending cut names its exact controlled Delta member"
    [AlignmentObligationBlocker]
    (announcedClasses memberSubject)
  assertEqual
    "the full pending cut does not implicate an unrelated controlled object"
    []
    (announcedClasses fixtureControlledSubject)
  assertEqual
    "generation-only evidence cannot exclude the requested regular occurrence"
    fourUncertain
    (unresolvedClasses fixtureRegularSubject)
  assertEqual
    "generation-only evidence cannot exclude any other regular occurrence"
    fourUncertain
    (unresolvedClasses fixtureOtherRegularSubject)
  assertEqual
    "an opaque generation may still name the controlled object as a Delta member"
    fourUncertain
    (unresolvedClasses fixtureControlledSubject)

caseUnsettledPlanEvidence :: Assertion
caseUnsettledPlanEvidence = do
  let withDebt = alignmentHistoryWithDebt alignmentPromotedHistoryFixture
      debtKey = case AlignmentState.liveAlignmentStructuralDebtEntries withDebt of
        [debt] -> StructuralDebt.structuralConsequenceDebtKey debt
        _ -> error "expected one unsettled-plan fixture debt"
      cause = StructuralDebt.structuralDebtKeyCause debtKey
      occurrence = StructuralDebt.structuralDebtKeySort debtKey
      context = checked "unsettled plan context" (deriveContextGraph [DeltaVertex pendingEvidenceDelta] [] [pendingEvidenceDelta])
      member = AlignmentGeneration.generationMemberInput pendingEvidenceDelta pendingEvidenceStore secondFixtureHerald DomainAlignment.initialStoreRevision
      plan = case AlignmentPlan.prepareAlignmentPlan occurrence pendingEvidenceTopologyCut pendingEvidencePlacement context [member] Nothing Set.empty of
        Right (AlignmentPlan.AlignmentPlanReady retained) -> retained
        other -> error ("unsettled plan: " <> show other)
      prepared = checked "unsettled plan promotion" (AlignmentState.prepareAlignmentPlanPromotion pendingEvidenceHerald cause plan withDebt)
      owner = AlignmentState.initializeTransferMemberWork pendingEvidenceHerald (fst (AlignmentState.commitAlignmentPromotion prepared))
      classes subject retained = blockerClasses (Alignment.alignmentDisappearanceBlockers (Alignment.alignmentDisappearanceView subject retained))
      memberSubject = controlledSubjectAt (globalObjectIdFromDeltaId pendingEvidenceDelta) 997
      accepted = checked "unsettled plan acceptance" (AlignmentProtocol.alignmentPlanAccepted (AlignmentPlan.alignmentPlanId plan) pendingEvidenceHerald DomainAlignment.EmptyHeraldPublicationPrefix)
      settled = fst (checked "retain unsettled plan acceptance" (AlignmentState.retainAlignmentPlanAcceptance accepted owner))
  assertEqual "current plan acceptance blocks its regular occurrence" [AlignmentObligationBlocker] (classes fixtureRegularSubject owner)
  assertEqual "current plan acceptance blocks its exact Delta member" [AlignmentObligationBlocker] (classes memberSubject owner)
  assertEqual "unrelated regular occurrence has no plan blocker" [] (classes fixtureOtherRegularSubject owner)
  assertEqual "unrelated controlled object has no plan blocker" [] (classes fixtureControlledSubject owner)
  assertEqual "remote member historical readiness is not required by the local owner" [] (AlignmentState.alignmentLocalMemberReadinessEntries settled)
  assertEqual "acceptance settles a plan with no local members" [] (AlignmentState.unsettledAlignmentPlans settled)
  assertEqual "settled current plan releases the regular subject blocker" [] (classes fixtureRegularSubject settled)
  assertEqual "settled current plan releases the Delta subject blocker" [] (classes memberSubject settled)
  assertEqual "unsettled owner is internally valid" (Right ()) (AlignmentState.validateAlignmentState owner)
  assertEqual "settled owner is internally valid" (Right ()) (AlignmentState.validateAlignmentState settled)

caseBootstrapImportEvidence :: Assertion
caseBootstrapImportEvidence = do
  let (importOwner, attemptedOwner) = bootstrapImportEvidenceOwners
      classes subject owner =
        blockerClasses
          ( Alignment.alignmentDisappearanceBlockers
              (Alignment.alignmentDisappearanceView subject owner)
          )
  assertEqual
    "the promoted fresh-base import retains only its live obligation"
    [AlignmentObligationBlocker]
    (classes fixtureRegularSubject importOwner)
  assertEqual
    "the import's occurrence does not implicate another regular definition"
    []
    (classes fixtureOtherRegularSubject importOwner)
  assertEqual
    "the selected bootstrap source adds its attempt and live destination subscription"
    [ AlignmentObligationBlocker,
      AlignmentAttemptBlocker,
      AlignmentSubscriptionBlocker
    ]
    (classes fixtureRegularSubject attemptedOwner)
  assertEqual
    "the bootstrap attempt remains occurrence-qualified"
    []
    (classes fixtureOtherRegularSubject attemptedOwner)
  assertEqual
    "the bootstrap import owner remains internally valid"
    (Right ())
    (AlignmentState.validateAlignmentState importOwner)
  assertEqual
    "the bootstrap attempt owner remains internally valid"
    (Right ())
    (AlignmentState.validateAlignmentState attemptedOwner)

caseAlignmentPromotedHistory :: Assertion
caseAlignmentPromotedHistory = do
  let fixture = alignmentPromotedHistoryFixture
      classes owner =
        blockerClasses
          ( Alignment.alignmentDisappearanceBlockers
              (Alignment.alignmentDisappearanceView fixtureRegularSubject owner)
          )
      resolveClasses owner =
        blockerClasses
          ( Alignment.alignmentDisappearanceBlockers
              ( Alignment.alignmentRegularRetirementDisappearanceView
                  fixtureRegularSubject
                  (controlIndex 997)
                  owner
              )
          )
  assertEqual
    "an exact unpromoted debt remains live"
    [AlignmentDebtBlocker]
    (classes (alignmentHistoryWithDebt fixture))
  assertEqual
    "an exact non-invalidated promotion discharges its retained debt"
    []
    (classes (alignmentHistoryPromoted fixture))
  assertEqual
    "a certificate for the current generation remains live"
    [AlignmentCertificateBlocker]
    (classes (alignmentHistoryWithCertificate fixture))
  assertEqual
    "superseding the generation makes its retained certificate inert"
    []
    (classes (alignmentHistorySuperseded fixture))
  assertEqual
    "invalidating the sole covering coordinate revives the retained debt"
    [AlignmentDebtBlocker]
    (classes (alignmentHistoryInvalidated fixture))
  mapM_
    ( \(context, owner) ->
        assertEqual context (classes owner) (resolveClasses owner)
    )
    [ ("Open and Resolve agree on unpromoted debt", alignmentHistoryWithDebt fixture),
      ("Open and Resolve agree on promoted debt", alignmentHistoryPromoted fixture),
      ("Open and Resolve agree on a current certificate", alignmentHistoryWithCertificate fixture),
      ("Open and Resolve agree on superseded history", alignmentHistorySuperseded fixture),
      ("Open and Resolve agree on invalidated history", alignmentHistoryInvalidated fixture)
    ]
  assertEqual
    "supersession preserves the historical certificate"
    1
    ( length
        ( AlignmentState.alignmentHistoricalCertificateEntries
            (alignmentHistorySuperseded fixture)
        )
    )
  mapM_
    ( \(context, owner) ->
        assertEqual context (Right ()) (AlignmentState.validateAlignmentState owner)
    )
    [ ("unpromoted Alignment state remains valid", alignmentHistoryWithDebt fixture),
      ("promoted Alignment state remains valid", alignmentHistoryPromoted fixture),
      ("certified Alignment state remains valid", alignmentHistoryWithCertificate fixture),
      ("superseded Alignment state remains valid", alignmentHistorySuperseded fixture),
      ("invalidated Alignment state remains valid", alignmentHistoryInvalidated fixture)
    ]

casePlacementSupersededRouteHistory :: Assertion
casePlacementSupersededRouteHistory = do
  let claim =
        deriveRegularSortOccurrenceClaim
          (checkedSystemId fixtureCheckedGenesis)
          fixtureRegularDescriptor
          Genesis
      sortId = regularSortOccurrenceClaimSortId claim
      occurrence = regularSortOccurrenceClaimOccurrenceId claim
      local = pendingEvidenceHerald
      remote = secondFixtureHerald
      delta = checked "historical placement Delta" (mkDeltaId (fixtureIdentifierBytes 245))
      store =
        checked
          "historical placement Store incarnation"
          (mkStoreIncarnationId (fixtureIdentifierBytes 246))
      controller =
        checked
          "historical placement controller"
          (mkGlobalObjectId (fixtureIdentifierBytes 247))
      process =
        checked
          "historical placement process"
          (mkProcessEpochId (fixtureIdentifierBytes 248))
      route =
        PlacementProtocol.applicationDeltaRoute
          delta
          sortId
          occurrence
          controller
          process
          store
          (controlIndex 0)
      localCurrent =
        fst
          ( PlacementState.commitLocalSnapshot
              ( checked
                  "advertise current local historical route"
                  ( PlacementState.prepareLocalSnapshotForRoutes
                      [route]
                      (PlacementState.initialState local)
                  )
              )
          )
      remoteSnapshot =
        checked
          "advertise current remote historical route"
          ( PlacementProtocol.placementSnapshot
              remote
              PlacementProtocol.firstPlacementSequence
              [route]
          )
      bothCurrent =
        fst
          ( PlacementState.commitRemotePlacement
              ( checked
                  "project current remote historical route"
                  ( PlacementState.prepareRemotePlacement
                      (PlacementProtocol.FullPlacementSnapshot remoteSnapshot)
                      localCurrent
                  )
              )
          )
      localSuperseded =
        fst
          ( PlacementState.commitLocalSnapshot
              ( checked
                  "supersede the local historical route"
                  (PlacementState.prepareLocalSnapshotForRoutes [] bothCurrent)
              )
          )
      emptyRemoteSnapshot =
        checked
          "advertise an empty successor remote route set"
          ( PlacementProtocol.placementSnapshot
              remote
              ( PlacementProtocol.nextPlacementSequence
                  PlacementProtocol.firstPlacementSequence
              )
              []
          )
      bothSuperseded =
        fst
          ( PlacementState.commitRemotePlacement
              ( checked
                  "supersede the remote historical route"
                  ( PlacementState.prepareRemotePlacement
                      (PlacementProtocol.FullPlacementSnapshot emptyRemoteSnapshot)
                      localSuperseded
                  )
              )
          )
      classes owner =
        blockerClasses
          ( Placement.placementDisappearanceBlockers
              (Placement.placementDisappearanceView fixtureRegularSubject owner)
          )
  assertEqual
    "current local and remote qualified routes both block"
    [RouteDependencyBlocker, RouteDependencyBlocker]
    (classes bothCurrent)
  assertEqual
    "the remote current route still blocks after only local supersession"
    [RouteDependencyBlocker]
    (classes localSuperseded)
  assertEqual
    "superseded local and remote history is not live route work"
    []
    (classes bothSuperseded)
  assertEqual
    "both superseded route entries remain retained"
    2
    (length (PlacementState.retainedPlacementRouteEntries bothSuperseded))
  assertBool
    "the old local route remains available for frozen-cut authentication"
    ( PlacementState.retainedPlacementRouteMatches
        local
        delta
        store
        sortId
        occurrence
        bothSuperseded
    )
  assertBool
    "the old remote route remains available for frozen-cut authentication"
    ( PlacementState.retainedPlacementRouteMatches
        remote
        delta
        store
        sortId
        occurrence
        bothSuperseded
    )
  assertEqual
    "the superseded Placement state remains valid"
    (Right ())
    (PlacementState.validatePlacementState bothSuperseded)

caseOwnerSubjectMismatch :: Assertion
caseOwnerSubjectMismatch = do
  let state = fixtureHeraldState
      controlled = fixtureControlledSubject
      regular = fixtureRegularSubject
      peer =
        checked
          "controlled PeerStream view"
          (PeerStream.peerStreamDisappearanceView controlled (startupPeerStreamState state))
      result =
        Evidence.disappearanceEvidenceSnapshot
          (Store.storeDisappearanceView controlled (startupStoreState state))
          (Application.applicationDisappearanceView regular (startupApplicationState state))
          (Publication.publicationDisappearanceView controlled (startupPublicationState state))
          peer
          (Alignment.alignmentDisappearanceView controlled (startupAlignmentState state))
          (Controlled.controlledDisappearanceView controlled (startupControlledState state))
          (Graph.graphDisappearanceView controlled (startupGraphState state))
          (Placement.placementDisappearanceView controlled (startupPlacementState state))
  assertEqual
    "the composer identifies the disagreeing owner and both exact subjects"
    ( Left
        ( Evidence.DisappearanceEvidenceSubjectMismatch
            Evidence.ApplicationEvidenceOwner
            controlled
            regular
        )
    )
    result

caseOwnerBackedOpenReadiness :: Assertion
caseOwnerBackedOpenReadiness = do
  let state = fixtureHeraldState
      membershipCapture =
        Readiness.captureCurrentMembership (startupOracleProjectionState state)
      baseCapture =
        checked
          "established structural base capture"
          (Readiness.captureEstablishedStructuralBase (startupStructuralProgressState state))
      context =
        checked
          "joined disappearance Open context"
          (Readiness.disappearanceOpenContextFromOwnerCaptures membershipCapture baseCapture)
      membership = disappearanceOpenContextMembership context
  assertEqual
    "the joined context retains the Oracle's complete current member body"
    (Readiness.currentMembershipCaptureGenerationId membershipCapture)
    (Membership.heraldMembershipGenerationId membership)
  assertEqual
    "both owner captures agree on the complete member set"
    (NonEmpty.toList (Readiness.currentMembershipCaptureMembers membershipCapture))
    (NonEmpty.toList (Readiness.structuralBaseCaptureMembers baseCapture))
  assertBool
    "the exact local Herald participates in the established base"
    ( Readiness.currentMembershipCaptureLocalHerald membershipCapture
        `elem` NonEmpty.toList (Readiness.structuralBaseCaptureMembers baseCapture)
    )

caseOwnerBackedOpenReadinessMismatch :: Assertion
caseOwnerBackedOpenReadinessMismatch = do
  (_, transitioning, _, _) <- membershipHeldLabelClosureFixture
  let membershipCapture =
        Readiness.captureCurrentMembership
          (startupOracleProjectionState transitioning)
      baseCapture =
        checked
          "pre-successor established Graph base"
          ( Readiness.captureEstablishedStructuralBase
              (startupStructuralProgressState transitioning)
          )
      expected =
        Readiness.DisappearanceOpenGenerationMismatch
          (Readiness.currentMembershipCaptureGenerationId membershipCapture)
          (Readiness.structuralBaseCaptureGenerationId baseCapture)
  assertEqual
    "the Oracle successor cannot borrow its predecessor's still-established Graph base"
    (Left expected)
    (Readiness.disappearanceOpenContextFromOwnerCaptures membershipCapture baseCapture)

blockerClasses :: [DisappearanceBlockerWitness] -> [DisappearanceBlockerClass]
blockerClasses = fmap disappearanceBlockerClass

countBlocker :: DisappearanceBlockerClass -> [DisappearanceBlockerClass] -> Int
countBlocker wanted = length . filter (== wanted)

applicationOriginObject ::
  ApplicationPublication.ApplicationPublicationOrigin ->
  Maybe GlobalObjectId
applicationOriginObject origin = case origin of
  ApplicationPublication.RegularApplicationPublicationOrigin -> Nothing
  ApplicationPublication.ControlledFirstUseApplicationPublicationOrigin generated ->
    Just (globalObjectIdFromGlobalUniqueId generated)
  ApplicationPublication.ControlledUpdateApplicationPublicationOrigin object ->
    Just object
  ApplicationPublication.ForwardApplicationPublicationOrigin object _ ->
    Just object

heldControlledSubject :: HeraldState -> DisappearanceSubject
heldControlledSubject state =
  checked
    "held controlled disappearance subject"
    ( controlledPredefinedDisappearanceSubject
        NeutralVertexRole
        object
        occurrence
        Nothing
    )
  where
    object = case heldObjects of
      [] -> error "held fixture has no controlled publication"
      firstObject : remaining
        | all (== firstObject) remaining -> firstObject
        | otherwise -> error "held fixture publications refer to different controlled objects"
    heldObjects =
      [ retained
      | held <- ApplicationState.applicationFenceHeldEntries (startupApplicationState state),
        Just retained <-
          [ applicationOriginObject
              ( ApplicationState.applicationFenceHeldWorkOrigin
                  (ApplicationState.applicationFenceHeldSemanticWork held)
              )
          ]
      ]
    occurrence =
      case Map.lookup object (GraphState.graphStructuralVertexProjections (startupGraphState state))
        >>= Reconciliation.structuralVertexProjectionOccurrence of
        Nothing -> error "held controlled object has no projected structural occurrence"
        Just retained -> retained

controlledSubjectAt :: GlobalObjectId -> Word64 -> DisappearanceSubject
controlledSubjectAt object sequenceNumber =
  checked
    "controlled disappearance subject at structural sequence"
    ( controlledPredefinedDisappearanceSubject
        NeutralVertexRole
        object
        ( structuralOccurrenceId
            (checkedLocalHeraldEpoch fixtureStep14CheckedGenesis)
            (checked "controlled structural sequence" (mkStructuralSequence sequenceNumber))
        )
        Nothing
    )

fixtureNeutralRootOccurrenceSubject :: DisappearanceSubject
fixtureNeutralRootOccurrenceSubject =
  regularSortDefinitionDisappearanceSubject
    ( deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureStep14CheckedGenesis)
        (predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole))
        Genesis
    )

carriedRegularSortPeerPublication ::
  PredefinedSortRole ->
  StructuralCarrierRole ->
  IO
    ( PeerPublication,
      PublicationDestination,
      HeraldEpoch,
      HeraldEpoch,
      ProcessEpochId
    )
carriedRegularSortPeerPublication role carrierRole = do
  state <- predecessorAdmittedApplicationPublicationFixture
  template <- case PublicationState.applicationPublicationEntries (startupPublicationState state) of
    [(_, retained)] -> pure retained
    records -> error ("expected one accepted application publication, got " <> show (length records))
  let descriptor = predefinedCatalogueDescriptor (profileEntryFor role)
      identifier = PublicationState.applicationPublicationId template
      source = publicationSourceHeraldEpoch identifier
      sourceProcess = PublicationState.applicationPublicationSourceProcess template
      sourceStrength = PublicationState.applicationPublicationSourceStrength template
      occurrence = case filter ((== role) . primordialReplicaRole) (checkedPrimordialReplicas fixtureStep14CheckedGenesis) of
        [replica] -> primordialReplicaOccurrenceId replica
        replicas -> error ("expected one primordial replica for " <> show role <> ", got " <> show (length replicas))
      carrierObject = checked ("peer carrier object for " <> show role) (mkGlobalUniqueId (fixtureIdentifierBytes 232))
      carrierValue =
        checked
          ("peer carrier value for " <> show role)
          ( recordValue
              ( [ (checked "object_id field" (mkFieldName "object_id"), globalUniqueIdValue carrierObject),
                  (checked "label field" (mkFieldName "label"), labelValue (VoidLabel, 0)),
                  ( checked "sort_id field" (mkFieldName "sort_id"),
                    bytesValue (sortIdBytes (descriptorSortId fixtureRegularDescriptor))
                  )
                ]
                  <> case role of
                    NablaRole ->
                      [ ( checked "sequencing_object field" (mkFieldName "sequencing_object"),
                          optionalGlobalUniqueIdValue Nothing
                        )
                      ]
                    DeltaRole -> []
                    _ -> error ("unsupported structural peer carrier " <> show role)
              )
          )
      checkedPublication =
        checked
          ("checked peer carrier for " <> show role)
          (DomainPublication.mkCheckedPublication descriptor identifier carrierValue)
      delta = checked ("peer destination Delta for " <> show role) (mkDeltaId (fixtureIdentifierBytes 233))
      incarnation =
        checked
          ("peer destination Store incarnation for " <> show role)
          (mkStoreIncarnationId (fixtureIdentifierBytes 234))
      destination = publicationDestination delta incarnation sourceStrength
      peer = case filter (/= source) (NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs fixtureHeraldMembershipGeneration)) of
        first : _ -> first
        [] -> error "fixture membership has no remote peer"
      batch =
        checked
          ("peer publication batch for " <> show role)
          ( mkPublicationBatch
              identifier
              sourceProcess
              (DomainPublication.checkedPublicationSort checkedPublication)
              occurrence
              (DomainPublication.checkedPublicationCanonicalValue checkedPublication)
              sourceStrength
              (PublicationState.applicationPublicationSourceTopologyPrerequisite template)
              (PublicationState.applicationPublicationControlPrerequisite template)
              (destination :| [])
          )
      structuralDigest =
        checked
          ("peer structural publication digest for " <> show role)
          (structuralPublicationDigestFor descriptor occurrence checkedPublication batch)
      stamp =
        checked
          ("peer structural occurrence stamp for " <> show role)
          ( mkStructuralOccurrenceStamp
              (structuralOccurrenceId source firstStructuralSequence)
              (emptyStructuralVersionVector fixtureHeraldMembershipGeneration)
              identifier
              structuralDigest
              carrierRole
          )
      publication =
        checked
          ("checked structural peer publication for " <> show role)
          (mkStructuralPeerPublication descriptor occurrence checkedPublication stamp batch)
  pure (publication, destination, source, peer, sourceProcess)

retainPendingGenerationEvidence ::
  HeraldEpoch ->
  AlignmentProtocol.AlignmentControl ->
  AlignmentState.State ->
  AlignmentState.State
retainPendingGenerationEvidence remote control owner =
  fst
    ( AlignmentState.commitPendingGenerationEvidence
        ( checked
            "retain pending generation evidence"
            ( AlignmentState.preparePendingGenerationEvidence
                remote
                control
                owner
            )
        )
    )

pendingEvidenceAlignmentCut :: DomainAlignment.AlignmentCut
pendingEvidenceAlignmentCut =
  checked
    "pending-evidence alignment cut"
    ( DomainAlignment.alignmentCut
        (regularSortOccurrenceClaimSortId claim)
        (regularSortOccurrenceClaimOccurrenceId claim)
        pendingEvidenceTopologyCut
        pendingEvidencePlacement
        ( DomainAlignment.alignmentMember
            pendingEvidenceDelta
            pendingEvidenceStore
            local
            :| []
        )
        []
        [ DomainAlignment.freshMemberBaseEvidence
            pendingEvidenceDelta
            pendingEvidenceStore
            DomainAlignment.initialStoreRevision
        ]
    )
  where
    claim =
      deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureCheckedGenesis)
        fixtureRegularDescriptor
        Genesis
    local = pendingEvidenceHerald

pendingEvidenceTopologyCut :: Topology.TopologyCut
pendingEvidenceTopologyCut =
  checked
    "pending-evidence topology cut"
    ( Topology.topologyCut
        ( Topology.sameGenerationPredecessor
            ( checked
                "pending-evidence predecessor cut"
                (mkTopologyCutId (fixtureIdentifierBytes 243))
            )
        )
        ( Topology.topologyFrontier
            (emptyStructuralVersionVector fixtureHeraldMembershipGeneration)
            (controlIndex 0)
        )
        (Topology.deriveTopologyOccurrenceDigest "pending-generation-evidence")
    )

pendingEvidencePlacement :: DomainAlignment.PhysicalPlacementRevisionVector
pendingEvidencePlacement =
  checked
    "pending-evidence placement vector"
    ( DomainAlignment.physicalPlacementRevisionVector
        fixtureHeraldMembershipGeneration
        [ (herald, DomainAlignment.firstPlacementRevision)
        | herald <-
            NonEmpty.toList
              ( Membership.heraldMembershipGenerationActiveHeraldEpochs
                  fixtureHeraldMembershipGeneration
              )
        ]
    )

pendingEvidenceHerald :: HeraldEpoch
pendingEvidenceHerald =
  NonEmpty.head
    ( Membership.heraldMembershipGenerationActiveHeraldEpochs
        fixtureHeraldMembershipGeneration
    )

pendingEvidenceDelta :: DeltaId
pendingEvidenceDelta =
  checked
    "pending-evidence Delta"
    (mkDeltaId (fixtureIdentifierBytes 241))

pendingEvidenceStore :: StoreIncarnationId
pendingEvidenceStore =
  checked
    "pending-evidence Store incarnation"
    (mkStoreIncarnationId (fixtureIdentifierBytes 242))

secondFixtureHerald :: HeraldEpoch
secondFixtureHerald =
  case NonEmpty.toList
    (Membership.heraldMembershipGenerationActiveHeraldEpochs fixtureHeraldMembershipGeneration) of
    _ : retained : _ -> retained
    _ -> error "fixture membership has fewer than two Heralds"

data AlignmentPromotedHistoryFixture = AlignmentPromotedHistoryFixture
  { alignmentHistoryWithDebt :: AlignmentState.State,
    alignmentHistoryPromoted :: AlignmentState.State,
    alignmentHistoryWithCertificate :: AlignmentState.State,
    alignmentHistorySuperseded :: AlignmentState.State,
    alignmentHistoryInvalidated :: AlignmentState.State
  }

alignmentPromotedHistoryFixture :: AlignmentPromotedHistoryFixture
alignmentPromotedHistoryFixture =
  AlignmentPromotedHistoryFixture
    { alignmentHistoryWithDebt = withDebt,
      alignmentHistoryPromoted = promoted,
      alignmentHistoryWithCertificate = withCertificate,
      alignmentHistorySuperseded = superseded,
      alignmentHistoryInvalidated = invalidated
    }
  where
    claim =
      deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureCheckedGenesis)
        fixtureRegularDescriptor
        Genesis
    affectedSort =
      StructuralDebt.sortOccurrence
        (regularSortOccurrenceClaimSortId claim)
        (regularSortOccurrenceClaimOccurrenceId claim)
    local = pendingEvidenceHerald
    remote = secondFixtureHerald
    context =
      checked
        "promoted-history context"
        ( deriveContextGraph
            [DeltaVertex pendingEvidenceDelta]
            []
            [pendingEvidenceDelta]
        )
    member =
      AlignmentGeneration.generationMemberInput
        pendingEvidenceDelta
        pendingEvidenceStore
        remote
        DomainAlignment.initialStoreRevision
    plan =
      readyAlignmentPlan
        "promoted-history generation plan"
        affectedSort
        context
        [member]
        []
        Set.empty
    generation = case AlignmentGeneration.alignmentGenerationPlanGenerations plan of
      [retained] -> retained
      retained ->
        error
          ( "promoted-history plan generated "
              <> show (length retained)
              <> " generations"
          )
    generationId = AlignmentGeneration.alignmentGenerationId generation
    cause =
      StructuralConsequence.structuralOccurrenceCause
        ( structuralOccurrenceId
            local
            (checked "promoted-history sequence" (mkStructuralSequence 995))
        )
    withDebt = retainHistoryDebt "retain promoted-history debt" cause AlignmentState.emptyState
    promoted =
      promoteHistoryDebt
        "promote history debt"
        local
        cause
        affectedSort
        plan
        withDebt
    readyPrefix =
      DomainAlignment.deriveMemberReadyEvidenceDigest "promoted-history-ready"
    ready =
      AlignmentProtocol.classMemberReady
        AlignmentProtocol.firstAlignmentEvidenceSequence
        generationId
        pendingEvidenceStore
        readyPrefix
        DomainAlignment.initialStoreRevision
    withReady =
      fst
        ( AlignmentState.commitClassMemberReady
            ( checked
                "retain promoted-history member readiness"
                (AlignmentState.prepareClassMemberReady ready promoted)
            )
        )
    certificate =
      AlignmentProtocol.historicalCertificate
        generationId
        pendingEvidenceStore
        (AlignmentProtocol.classMemberReadySetDigest [ready])
        ( DomainAlignment.deriveBootstrapEvidenceDigest
            (DomainAlignment.memberReadyEvidenceDigestBytes readyPrefix)
        )
        DomainAlignment.initialStoreRevision
    withCertificate =
      fst
        ( AlignmentState.commitHistoricalCertificate
            ( checked
                "retain promoted-history certificate"
                (AlignmentState.prepareHistoricalCertificate certificate withReady)
            )
        )
    invalidated =
      fst
        ( AlignmentState.commitAlignmentPlanInvalidation
            ( checked
                "invalidate promoted-history plan"
                ( AlignmentState.prepareAlignmentPlanInvalidation
                    ( AlignmentState.AlignmentPlanDestinationStoreLost
                        generationId
                        ( AlignmentProtocol.destinationStore
                            pendingEvidenceDelta
                            pendingEvidenceStore
                        )
                    )
                    withCertificate
                )
            )
        )
    removalCause =
      StructuralConsequence.structuralOccurrenceCause
        ( structuralOccurrenceId
            local
            (checked "removal-history sequence" (mkStructuralSequence 996))
        )
    withRemovalDebt =
      retainHistoryDebt
        "retain removal-history debt"
        removalCause
        withCertificate
    removalContext =
      checked "removal-history context" (deriveContextGraph [] [] [])
    removalPlan =
      readyAlignmentPlan
        "removal-history generation plan"
        affectedSort
        removalContext
        []
        [generation]
        (Set.singleton generationId)
    superseded =
      promoteHistoryDebt
        "promote removal-history debt"
        local
        removalCause
        affectedSort
        removalPlan
        withRemovalDebt

    retainHistoryDebt contextLabel debtCause owner =
      fst
        ( AlignmentState.commitStructuralDebtRetention
            ( checked
                contextLabel
                ( AlignmentState.prepareStructuralDebtRetention
                    debtCause
                    ( StructuralDebt.normalizeStructuralDebts
                        [ StructuralDebt.structuralConsequenceDebt
                            ( StructuralDebt.structuralDebtKey
                                debtCause
                                StructuralDebt.TopologyAlignmentDebt
                                affectedSort
                                Nothing
                            )
                            ( StructuralDebt.structuralDebtEvidence
                                Set.empty
                                Set.empty
                                Set.empty
                            )
                        ]
                    )
                    owner
                )
            )
        )

    promoteHistoryDebt contextLabel ownerHerald debtCause debtSort debtPlan owner =
      fst
        ( AlignmentState.commitAlignmentPromotion
            ( checked
                contextLabel
                ( AlignmentState.prepareAlignmentPromotion
                    ownerHerald
                    debtCause
                    debtSort
                    (Topology.deriveTopologyCutId pendingEvidenceTopologyCut)
                    pendingEvidencePlacement
                    debtPlan
                    owner
                )
            )
        )

    readyAlignmentPlan contextLabel debtSort debtContext members prior certificates =
      case AlignmentGeneration.prepareAlignmentGenerationPlan
        debtSort
        pendingEvidenceTopologyCut
        pendingEvidencePlacement
        debtContext
        members
        prior
        []
        certificates of
        Right (AlignmentGeneration.AlignmentGenerationReady retained) -> retained
        other -> error (contextLabel <> ": " <> show other)

bootstrapImportEvidenceOwners :: (AlignmentState.State, AlignmentState.State)
bootstrapImportEvidenceOwners = (promoted, attempted)
  where
    claim =
      deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureCheckedGenesis)
        fixtureRegularDescriptor
        Genesis
    affectedSort =
      StructuralDebt.sortOccurrence
        (regularSortOccurrenceClaimSortId claim)
        (regularSortOccurrenceClaimOccurrenceId claim)
    causeOccurrence =
      structuralOccurrenceId
        pendingEvidenceHerald
        (checked "bootstrap evidence sequence" (mkStructuralSequence 994))
    cause = StructuralConsequence.structuralOccurrenceCause causeOccurrence
    context =
      checked
        "bootstrap evidence context"
        ( deriveContextGraph
            [DeltaVertex pendingEvidenceDelta]
            []
            [pendingEvidenceDelta]
        )
    member =
      AlignmentGeneration.generationMemberInput
        pendingEvidenceDelta
        pendingEvidenceStore
        pendingEvidenceHerald
        DomainAlignment.initialStoreRevision
    plan =
      case AlignmentGeneration.prepareAlignmentGenerationPlan
        affectedSort
        pendingEvidenceTopologyCut
        pendingEvidencePlacement
        context
        [member]
        []
        []
        Set.empty of
        Right (AlignmentGeneration.AlignmentGenerationReady retained) -> retained
        other -> error ("prepare bootstrap evidence plan: " <> show other)
    generation =
      case AlignmentGeneration.alignmentGenerationPlanGenerations plan of
        [retained] -> retained
        retained ->
          error
            ( "bootstrap evidence plan generated "
                <> show (length retained)
                <> " generations"
            )
    debt =
      StructuralDebt.normalizeStructuralDebts
        [ StructuralDebt.structuralConsequenceDebt
            ( StructuralDebt.structuralDebtKey
                cause
                StructuralDebt.TopologyAlignmentDebt
                affectedSort
                Nothing
            )
            (StructuralDebt.structuralDebtEvidence Set.empty Set.empty Set.empty)
        ]
    withDebt =
      fst
        ( AlignmentState.commitStructuralDebtRetention
            ( checked
                "retain bootstrap evidence debt"
                ( AlignmentState.prepareStructuralDebtRetention
                    cause
                    debt
                    AlignmentState.emptyState
                )
            )
        )
    promoted =
      fst
        ( AlignmentState.commitAlignmentPromotion
            ( checked
                "promote bootstrap evidence generation"
                ( AlignmentState.prepareAlignmentPromotion
                    pendingEvidenceHerald
                    cause
                    affectedSort
                    (Topology.deriveTopologyCutId pendingEvidenceTopologyCut)
                    pendingEvidencePlacement
                    plan
                    withDebt
                )
            )
        )
    bootstrapKey =
      case fmap fst (AlignmentState.bootstrapImportEntries promoted) of
        [retained] -> retained
        retained ->
          error
            ( "bootstrap evidence promotion retained "
                <> show (length retained)
                <> " imports"
            )
    generationCut = AlignmentGeneration.alignmentGenerationCut generation
    withAcceptance =
      fst
        ( AlignmentState.commitAlignmentCutAcceptance
            ( checked
                "retain bootstrap evidence cut acceptance"
                ( AlignmentState.prepareAlignmentCutAcceptance
                    ( AlignmentProtocol.alignmentCutAccepted
                        (AlignmentGeneration.alignmentGenerationId generation)
                        pendingEvidenceHerald
                        ( Topology.deriveTopologyCutId
                            (DomainAlignment.alignmentCutTopologyCut generationCut)
                        )
                        (DomainAlignment.alignmentCutPhysicalPlacementRevisionVector generationCut)
                        DomainAlignment.EmptyHeraldPublicationPrefix
                    )
                    promoted
                )
            )
        )
    attempted =
      fst
        ( AlignmentState.commitBootstrapImportAttempt
            ( checked
                "select bootstrap evidence source"
                ( AlignmentState.prepareBootstrapImportAttempt
                    bootstrapKey
                    withAcceptance
                )
            )
        )

ownerSnapshot ::
  DisappearanceSubject ->
  HeraldState ->
  Evidence.DisappearanceEvidenceSnapshot
ownerSnapshot subject state =
  checked
    "owner-composed disappearance evidence"
    ( Evidence.disappearanceEvidenceSnapshot
        (Store.storeDisappearanceView subject (startupStoreState state))
        (Application.applicationDisappearanceView subject (startupApplicationState state))
        (Publication.publicationDisappearanceView subject (startupPublicationState state))
        peer
        (Alignment.alignmentDisappearanceView subject (startupAlignmentState state))
        (Controlled.controlledDisappearanceView subject (startupControlledState state))
        (Graph.graphDisappearanceView subject (startupGraphState state))
        (Placement.placementDisappearanceView subject (startupPlacementState state))
    )
  where
    peer =
      checked
        "PeerStream disappearance view"
        (PeerStream.peerStreamDisappearanceView subject (startupPeerStreamState state))

attestationClasses ::
  Evidence.DisappearanceEvidenceSnapshot -> [AbsenceAttestationClass]
attestationClasses snapshot =
  case Evidence.disappearanceEvidenceSnapshotAbsenceAttestation snapshot of
    Nothing -> error "empty owner snapshot did not produce an attestation"
    Just attestation -> fmap fst (disappearanceAbsenceAttestationEntries attestation)

fixtureHeraldState :: HeraldState
fixtureHeraldState =
  fst
    ( checked
        "initial Herald for disappearance evidence"
        ( initialHerald
            (monotonicInstant 0)
            fixtureCheckedGenesis
            fixtureCheckedInitialBootstraps
            fixtureOracleContacts
            fixtureGeneratorSeed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    )

fixtureControlledSubject :: DisappearanceSubject
fixtureControlledSubject =
  checked
    "controlled disappearance subject"
    ( controlledPredefinedDisappearanceSubject
        NeutralVertexRole
        (checked "controlled object" (mkGlobalObjectId (fixtureIdentifierBytes 221)))
        ( structuralOccurrenceId
            (checkedLocalHeraldEpoch fixtureCheckedGenesis)
            (checked "structural sequence" (mkStructuralSequence 991))
        )
        Nothing
    )

fixtureRegularSubject :: DisappearanceSubject
fixtureRegularSubject =
  regularSortDefinitionDisappearanceSubject
    ( deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureCheckedGenesis)
        fixtureRegularDescriptor
        Genesis
    )

fixtureOtherRegularSubject :: DisappearanceSubject
fixtureOtherRegularSubject =
  regularSortDefinitionDisappearanceSubject
    ( deriveRegularSortOccurrenceClaim
        (checkedSystemId fixtureCheckedGenesis)
        fixtureOtherRegularDescriptor
        Genesis
    )

fixtureRegularDescriptor :: CanonicalDescriptor
fixtureRegularDescriptor =
  admittedApplicationSortDescriptor
    ( checked
        "regular descriptor admission"
        ( admitApplicationSortDefinition
            (DeclaredSortDefinition descriptor Nothing)
        )
    )
  where
    descriptor =
      ApplicationSortDescriptor
        { sortKind = RegularSort,
          valueSchema = RecordSchema (Map.singleton "key" TextSchema),
          keyProjections = [ApplicationProjection ("key" :| [])],
          validityPredicate = AlwaysPredicate,
          obsolescencePredicate = NeverPredicate,
          rankTerms = RankApplicationValue Ascending :| [],
          minimumRetentionMicros = 0,
          isImmutable = False,
          labelField = Nothing
        }

fixtureOtherRegularDescriptor :: CanonicalDescriptor
fixtureOtherRegularDescriptor =
  admittedApplicationSortDescriptor
    ( checked
        "other regular descriptor admission"
        (admitApplicationSortDefinition (DeclaredSortDefinition descriptor Nothing))
    )
  where
    descriptor =
      ApplicationSortDescriptor
        { sortKind = RegularSort,
          valueSchema = RecordSchema (Map.singleton "other" TextSchema),
          keyProjections = [ApplicationProjection ("other" :| [])],
          validityPredicate = AlwaysPredicate,
          obsolescencePredicate = NeverPredicate,
          rankTerms = RankApplicationValue Ascending :| [],
          minimumRetentionMicros = 0,
          isImmutable = False,
          labelField = Nothing
        }

checked :: (Show problem) => String -> Either problem value -> value
checked context result = case result of
  Left problem -> error (context <> ": " <> show problem)
  Right value -> value

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure
