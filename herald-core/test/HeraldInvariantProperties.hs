{-# LANGUAGE OverloadedStrings #-}

module HeraldInvariantProperties
  ( tests,
  )
where

import GenesisFixtures (fixtureHeraldAfterProbePrefix, fixtureRetirementResolution)

import ApplicationLabelProperties
  ( heldFixtureStateForOwnerProperties,
    regularRetirementHeldOrdinaryFixture,
    regularRetirementLocallyTakenOrdinaryFixture,
  )
import Control.Monad (foldM)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity (PrivateNablaId)
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Result (RegularCallResult (NewIdCompleted))
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
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write (ApplicationWriteValue (PublishValue))
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Disappearance
  ( DisappearanceSubject,
    deriveRegularSortOccurrenceClaim,
    regularSortDefinitionDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    StoreIncarnationId,
    controlIndex,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    mkDeltaId,
    mkGlobalObjectId,
    mkHeraldId,
    mkProcessEpochId,
    mkProcessId,
    mkStoreIncarnationId,
    mkSystemId,
    nablaSequence,
  )
import Eclips.Domain.Membership
  ( heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Publication
  ( checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationSort,
    checkedPublicationValue,
    mkCheckedPublication,
  )
import Eclips.Domain.Route
  ( ReplicaStrength (Normal),
    destinationDelta,
    routeDestinations,
  )
import Eclips.Domain.Sort.Canonical (CanonicalDescriptor)
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRoot,
    AppliedRootRole (..),
    HeraldMember (..),
    PredefinedSortRole (SortDefinitionRole),
    appliedBootstrapManifestId,
    appliedProcessAuthority,
    appliedProcessEnvironmentEdges,
    appliedProcessEnvironmentHub,
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootControlPrerequisite,
    appliedRootObjectId,
    appliedRootOccurrenceId,
    appliedRootPlacement,
    appliedRootRole,
    appliedRootSortId,
    deriveSystemViewDeltaId,
  )
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.Disappearance qualified as ApplicationDisappearance
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (Completed),
    requestId,
  )
import Eclips.Herald.Application.Request.Internal qualified as ApplicationRequest
import Eclips.Herald.Application.Session
  ( ApplicationSessionReply (SessionOpened),
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.Session.Internal qualified as ApplicationSession
import Eclips.Herald.Application.SortDefinition qualified as ApplicationSortDefinition
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Evidence qualified as DisappearanceEvidence
import Eclips.Herald.Disappearance.OwnerEvidence qualified as OwnerEvidence
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceBlockerClass (..),
    disappearanceBlockerClass,
  )
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.EffectBatch
  ( HeraldEffect (SendApplicationReply, SetApplicationConnectionDisposition),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (..),
    DeploymentManifest (..),
    OracleGenesisManifest (..),
    PrimordialDefinitionManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( PrimordialDefinitionReplica,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedLocalHeraldId,
    checkedOracleControlIndex,
    checkedPrimordialReplicas,
    checkedSystemId,
    primordialReplicaOccurrenceId,
    primordialReplicaPublicationId,
    primordialReplicaSortId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Initialization
  ( HeraldInvariantFault (..),
    StartupInvariantSubject (..),
    StartupInvariantViolation (..),
    initialHerald,
  )
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
    ApplicationSessionIngress (OpenApplicationSession),
    HeraldInputBody (ApplicationRequestInput, ApplicationSessionInput, OracleInput),
    candidateApplicationLane,
    heraldInput,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (ConnectAndHelloOracle),
    OracleClientIngress (OracleHelloReceived),
    oracleConnectAttemptContact,
    oracleConnectAttemptFromExclusive,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleHelloClaims,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (PeerLogicalPublication),
    peerLogicalPublicationItem,
  )
import Eclips.Herald.PeerPublication
  ( mkPublicationBatch,
    mkStructuralOccurrenceStamp,
    mkStructuralPublicationDigest,
    peerPublicationBatch,
    peerPublicationDigest,
    peerPublicationStructuralStamp,
    presentedOrdinaryPeerPublication,
    presentedStructuralPeerPublication,
    publicationBatchControlPrerequisite,
    publicationBatchId,
    publicationBatchSourceProcess,
    publicationDestination,
    structuralOccurrenceStampCarrierRole,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
    structuralOccurrenceStampPublicationDigest,
  )
import Eclips.Herald.PeerStream
  ( ReceiveProgress (ReceivedPending),
    firstStreamSequence,
    mkPeerDispatchBindingGeneration,
    mkStreamDirection,
    sequencedItem,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamPrefixThrough,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementRoute
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant
  ( validateHeraldState,
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupDiscoveryState,
    replaceStartupGraphState,
    replaceStartupOracleClientState,
    replaceStartupOracleProjectionState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDiscoveryState,
    startupGraphState,
    startupLastObservedTime,
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
import Eclips.Herald.Structural.Debt qualified as StructuralDebt
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureDeploymentAt,
    fixtureDeploymentManifest,
    fixtureGeneratorSeed,
    fixtureHeraldMembershipGeneration,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteMember,
    fixtureStep14CheckedGenesis,
  )
import HeraldTransitionProperties qualified
import PrimordialTestAccess (conventionalStartupPairs)
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
    "whole Herald startup invariant"
    [ testCase "the composed initializer produces a valid state" caseValidState,
      testCase "a live generic request is linked to its exact semantic publication" caseGenericPublicationInvariant,
      testCase "a crossed different-sort generic publication is rejected before raw-link validation" caseGenericPublicationRequestMismatch,
      testCase "session retirement may drop the raw request while preserving publication" caseGenericPublicationSessionRetirement,
      testCase "a live controlled structural request is linked to its current semantic publication" caseStructuralPublicationInvariant,
      testCase "a controlled structural retry preserves the exact raw-to-semantic link" caseStructuralPublicationRetry,
      testCase "structural session retirement drops only the raw request" caseStructuralPublicationSessionRetirement,
      testCase "every checked bootstrap operate role retains normal possession" caseBootstrapOperatePossession,
      testCase "independently valid Oracle Hello claims are bound to genesis" caseOracleClientClaimsContradiction,
      testCase "an unsent label Open must belong to its exact local invocation" caseDrainingLabelInvocationContradiction,
      testCase "an independently valid empty Oracle projection is detected" caseOracleContradiction,
      testCase "an independently valid remote Oracle identity is detected" caseOracleLocalIdentityContradiction,
      testCase "equal Herald epochs cannot disguise different member identities" caseOracleMembershipContradiction,
      testCase "a coordinated successor membership satisfies the whole-state invariant" caseSuccessorMembershipInvariant,
      testCase "a pre-retirement structural publication replays under its captured membership" caseHistoricalStructuralPublicationMembershipInvariant,
      testCase "a stale Discovery membership is rejected after projection advances" caseDiscoveryMembershipContradiction,
      testCase "an independently valid but different applied Oracle fact is detected" caseOracleAppliedContradiction,
      testCase "an independently valid sort projection from another genesis is detected" caseSortRegistryContradiction,
      testCase "an independently valid sort provenance projection is detected" caseSortRegistryProvenanceContradiction,
      testCase "an independently valid empty application owner is detected" caseApplicationContradiction,
      testCase "an independently valid incomplete application mapping is detected" caseApplicationAccessContradiction,
      testCase "an independently valid empty controlled owner is detected" caseControlledContradiction,
      testCase "an independently valid conflicting controlled fact is detected" caseControlledFactContradiction,
      testCase "an independently valid empty graph owner is detected" caseGraphContradiction,
      testCase "an independently valid graph pairing is detected" caseGraphEdgeContradiction,
      testCase "an uninduced neutral vertex is detected" caseUninducedNeutralVertexContradiction,
      testCase "an independently valid system-view-only placement owner is detected" casePlacementContradiction,
      testCase "an independently valid conflicting placement fact is detected" casePlacementFactContradiction,
      testCase "another Herald's private system views are detected" caseSystemViewStoreContradiction,
      testCase "another Herald's publication owner is detected" casePublicationOwnerContradiction,
      testCase "a pending request without its Wait owner is detected" caseMissingWaitRegistration,
      testCase "an independently valid genesis-only store owner is detected" caseStoreContradiction,
      testCase "an independently valid conflicting store fact is detected" caseStoreFactContradiction,
      testGroup
        "Increment-8 regular retirement invariant"
        [ testCase "a composed regular retirement remains whole-state valid" caseRegularRetirementInvariant,
          testCase "a post-Resolve equal redefinition remains whole-state valid" caseRegularRetirementSuccessorRedefinition,
          testCase "Store receipt suppression is exact below Resolve" caseRegularRetirementMixedObservationSuppression,
          testCase "application-held ordinary work is occurrence-qualified after Resolve" caseRegularRetirementApplicationHeldOccurrences,
          testCase "LocalTake and structural deletion do not counterfeit regular-sort absence" caseRegularRetirementLocallyTakenOrdinaryResiduals,
          testCase "a registry retirement without its Store suppression is rejected" caseRegularRetirementMissingSuppression,
          testCase "a mismatched Store suppression Resolve index is rejected" caseRegularRetirementMismatchedSuppression,
          testCase "a Store suppression cut that misses its local definition is rejected" caseRegularRetirementTooSmallCut,
          testCase "a retirement ahead of structural control is rejected" caseRegularRetirementLaggingControl,
          testCase "a redefinition below its Resolve prerequisite is rejected" caseRegularRetirementLowRedefinitionControl,
          testCase "delayed old-occurrence Placement evidence is rejected" caseRegularRetirementDelayedPlacement
        ],
      testGroup
        "Step-7 relationship mutation matrix"
        [ testCase "a publication owned by the wrong process is classified precisely" casePublicationRecordRelationship,
          testCase "a routed local publication without its Store receipt is classified precisely" casePublicationStoreReceiptRelationship,
          testCase "a Store observation cannot borrow another payload's receipt identity" caseStoreObservationPublicationRelationship,
          testCase "a remote batch without checked dispatch provenance is classified precisely" casePublicationRemoteAssignmentRelationship,
          testCase "an archive-eligible structural hold still authenticates its stamp" caseArchiveEligibleStructuralHoldAuthentication,
          testCase "an incoming assignment index without a record is classified precisely" caseIncomingAssignmentRelationship,
          testCase "a dependency waiter index without a record is classified precisely" caseDependencyWaiterRelationship,
          testCase "a control waiter index without a record is classified precisely" caseControlWaiterRelationship,
          testCase "a dispatch attempt tied to a stale binding is classified precisely" caseDispatchBindingRelationship,
          testCase "a Discovery binding without dispatch ownership is classified precisely" casePeerBindingContradiction
        ]
    ]

caseValidState :: Assertion
caseValidState = do
  state <- validState
  assertEqual "valid composed state" (Right ()) (validateHeraldState state)

caseGenericPublicationInvariant :: Assertion
caseGenericPublicationInvariant = do
  fixture <- genericInvariantFixture invariantFixtureDescriptor
  assertEqual
    "valid live generic publication"
    (Right ())
    (validateHeraldState (state fixture))

caseGenericPublicationRequestMismatch :: Assertion
caseGenericPublicationRequestMismatch = do
  first <- genericInvariantFixture invariantFixtureDescriptor
  second <-
    genericInvariantFixture
      (invariantFixtureDescriptor {minimumRetentionMicros = 1})
  let crossed =
        replaceStartupApplicationState
          (startupApplicationState (state first))
          (state second)
  assertFault
    ( HeraldStartupInvariant
        (StartupProcess (process second))
        PublicationRecordInvariant
    )
    crossed

caseGenericPublicationSessionRetirement :: Assertion
caseGenericPublicationSessionRetirement = do
  fixture <- genericInvariantFixture invariantFixtureDescriptor
  prepared <-
    checkedIO
      "end generic invariant session"
      ( Application.prepareApplicationSessionEnd
          (session fixture)
          (binding fixture)
          (startupApplicationState (state fixture))
      )
  let endedApplication = Application.commitApplicationSessionEnd prepared
      ended = replaceStartupApplicationState endedApplication (state fixture)
  assertEqual
    "session retirement preserves a valid semantic publication"
    (Right ())
    (validateHeraldState ended)
  assertEqual
    "raw generic owner was retired"
    []
    (Application.applicationRequestEntries (session fixture) endedApplication)
  assertEqual
    "semantic publication remains retained"
    1
    (length (Publication.applicationPublicationEntries (startupPublicationState ended)))

caseRegularRetirementInvariant :: Assertion
caseRegularRetirementInvariant = do
  fixture <- regularRetirementInvariantFixture
  assertEqual
    "retired regular occurrence is composed-valid"
    (Right ())
    (validateHeraldState (regularRetirementSuccessor fixture))

caseRegularRetirementSuccessorRedefinition :: Assertion
caseRegularRetirementSuccessorRedefinition = do
  fixture <- regularRetirementInvariantFixture
  redefined <- commitRegularRetirementRedefinition fixture
  residualBlockers <-
    checkedIO
      "read post-Resolve regular-retirement residual work"
      ( DisappearanceEvidence.regularRetirementResidualBlockersForHerald
          (regularRetirementSubject fixture)
          (regularRetirementResolveIndex fixture)
          redefined
      )
  let effective =
        SortRegistry.lookupEffectiveSort
          (SortRegistry.registryEntrySortId (regularRetirementEntry fixture))
          (startupSortRegistryState redefined)
  assertEqual
    "the equal definition is re-established at the derived successor occurrence"
    (Just (regularRetirementSuccessorOccurrence fixture))
    (SortRegistry.registryEntryOccurrenceId <$> effective)
  assertEqual
    "successor-epoch owner work is not a retired-epoch residual"
    []
    residualBlockers
  assertEqual
    "the post-Resolve owner product remains whole-state valid"
    (Right ())
    (validateHeraldState redefined)

caseRegularRetirementMixedObservationSuppression :: Assertion
caseRegularRetirementMixedObservationSuppression = do
  fixture <- regularRetirementInvariantFixture
  let retired = regularRetirementSuccessor fixture
      entry = regularRetirementEntry fixture
      identifier = SortRegistry.registryEntryPublicationId entry
      store = startupStoreState retired
  record <-
    checkedMaybeIO
      "retired definition publication for mixed-observation mutant"
      (Publication.lookupOutgoingPublication identifier (startupPublicationState retired))
  slot <-
    case [ retained
         | retained <- Store.storeSlots store,
           any ((== identifier) . fst) (Store.storeSlotApplicationReceipts retained)
         ] of
      retained : _ -> pure retained
      [] -> assertFailure "retired definition has no receipted Store slot"
  let delta = Store.storeSlotDelta slot
      publication = Publication.outgoingPublicationChecked record
      destination =
        Store.peerStoreDestination
          delta
          (Store.storeSlotIncarnation slot)
          Normal
      belowResolvePrerequisites =
        [ Store.storeObservationEnvelopeControlPrerequisite envelope
        | observation <- Store.storeSlotApplicationObservations slot,
          Store.retainedStoreObservationPublication observation == publication,
          Store.RoutedStoreObservation envelope <-
            [Store.retainedStoreObservationOrigin observation]
        ]
      exerciseUnsuppressedObservation prerequisite description = do
        let envelope =
              Store.storeObservationEnvelope
                (Publication.outgoingPublicationSourceProcess record)
                (Publication.outgoingPublicationOccurrenceId record)
                (Publication.outgoingPublicationSourceTopologyPrerequisite record)
                prerequisite
                Nothing
        prepared <-
          checkedIO
            ("retain " <> description <> " Store observation")
            ( Store.preparePeerStoreApplication
                envelope
                (destination :| [])
                publication
                store
            )
        let (withObservation, outcomes) =
              Store.commitPeerStoreApplication prepared
            successor = replaceStartupStoreState withObservation retired
        assertEqual
          (description <> " observation restores the effective fact")
          [Store.PeerStoreApplied]
          (Store.peerStoreOutcomeDisposition <$> NonEmpty.toList outcomes)
        retainedSlot <-
          checkedMaybeIO
            ("Store slot after " <> description <> " observation")
            (Store.lookupStoreSlot delta withObservation)
        assertEqual
          (description <> " observation is retained beside the stale one")
          2
          ( length
              [ observation
              | observation <- Store.storeSlotApplicationObservations retainedSlot,
                Store.retainedStoreObservationPublication observation == publication
              ]
          )
        assertEqual
          (description <> " receipt remains whole-state valid while effective")
          (Right ())
          (validateHeraldState successor)
        let corruptedStore =
              Store.removeStoreEffectivePublicationForInvariantTest
                delta
                identifier
                withObservation
            corrupted = replaceStartupStoreState corruptedStore retired
        assertFault
          (HeraldStartupInvariant (StartupDelta delta) StoreContentsInvariant)
          corrupted

  assertBool
    "the retirement fixture retains routed evidence below Resolve"
    ( not (null belowResolvePrerequisites)
        && all
          (< regularRetirementResolveIndex fixture)
          belowResolvePrerequisites
    )
  assertBool
    "the below-Resolve receipt has no effective fact"
    ( store
        == Store.removeStoreEffectivePublicationForInvariantTest
          delta
          identifier
          store
    )
  assertEqual
    "the below-Resolve observation is valid without an effective fact"
    (Right ())
    (validateHeraldState retired)
  exerciseUnsuppressedObservation
    (regularRetirementResolveIndex fixture)
    "equal-Resolve"
  exerciseUnsuppressedObservation
    (controlIndex 212)
    "above-Resolve"

caseRegularRetirementApplicationHeldOccurrences :: Assertion
caseRegularRetirementApplicationHeldOccurrences = do
  let descriptor = invariantFixtureDescriptor
      unrelatedDescriptor =
        invariantFixtureDescriptor {minimumRetentionMicros = 1}
      resolveIndex = controlIndex 1
      retire =
        \predecessor ->
          completeFixtureOutgoingWork predecessor
            >>= retireRegularDefinitionInState fixtureStep14CheckedGenesis descriptor resolveIndex
  (oldHeld, successorHeld, unrelatedHeld) <-
    regularRetirementHeldOrdinaryFixture
      descriptor
      unrelatedDescriptor
      retire
  oldEntry <- dynamicEntryForDescriptor descriptor oldHeld
  successorEntry <- dynamicEntryForDescriptor descriptor successorHeld
  unrelatedEntry <- dynamicEntryForDescriptor unrelatedDescriptor unrelatedHeld
  oldWork <- singleApplicationHeldWork oldHeld
  successorWork <- singleApplicationHeldWork successorHeld
  unrelatedWork <- singleApplicationHeldWork unrelatedHeld
  canonicalDescriptor <- applicationCanonicalDescriptor descriptor
  let subject =
        regularSortDefinitionDisappearanceSubject
          ( deriveRegularSortOccurrenceClaim
              (checkedSystemId fixtureStep14CheckedGenesis)
              canonicalDescriptor
              Genesis
          )
      oldOccurrence = SortRegistry.registryEntryOccurrenceId oldEntry
      successorOccurrence = SortRegistry.registryEntryOccurrenceId successorEntry
  successorBase <-
    checkedIO
      "derive held-work successor occurrence base"
      (resolvedRetirementOccurrenceBase resolveIndex)
  let expectedSuccessorOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixtureStep14CheckedGenesis)
          (SortRegistry.registryEntrySortId oldEntry)
          successorBase
  assertEqual
    "pre-Resolve held work retains the exact original carrier sort"
    (SortRegistry.registryEntrySortId oldEntry)
    (Application.applicationFenceHeldWorkSortId oldWork)
  assertEqual
    "pre-Resolve held work retains the exact original carrier occurrence"
    oldOccurrence
    (Application.applicationFenceHeldWorkSortOccurrenceId oldWork)
  assertBool
    "pre-Resolve held work predates the retirement floor"
    ( Application.applicationFenceHeldWorkControlPrerequisite oldWork
        < resolveIndex
    )
  attemptedOldRetirement <- retire oldHeld
  let oldApplicationView =
        ApplicationDisappearance.applicationRegularRetirementDisappearanceView
          subject
          resolveIndex
          (startupApplicationState attemptedOldRetirement)
  assertEqual
    "the exact original occurrence remains an Application blocker"
    [HeldPublicationBlocker]
    ( disappearanceBlockerClass
        <$> ApplicationDisappearance.applicationDisappearanceBlockers
          oldApplicationView
    )
  assertRegularRetirementFault attemptedOldRetirement

  assertEqual
    "equal redefinition installs the derived successor occurrence"
    expectedSuccessorOccurrence
    successorOccurrence
  assertEqual
    "post-Resolve held work retains the same regular sort"
    (SortRegistry.registryEntrySortId successorEntry)
    (Application.applicationFenceHeldWorkSortId successorWork)
  assertEqual
    "post-Resolve held work retains the exact successor occurrence"
    successorOccurrence
    (Application.applicationFenceHeldWorkSortOccurrenceId successorWork)
  assertBool
    "successor held work is admitted at or beyond Resolve"
    ( Application.applicationFenceHeldWorkControlPrerequisite successorWork
        >= resolveIndex
    )
  assertEqual
    "successor-occurrence held work is not retired-epoch residual work"
    (Right [])
    ( DisappearanceEvidence.regularRetirementResidualBlockersForHerald
        subject
        resolveIndex
        successorHeld
    )
  assertEqual
    "the successor-occurrence held state remains whole-state valid"
    (Right ())
    (validateHeraldState successorHeld)

  assertEqual
    "unrelated held work retains its own regular sort"
    (SortRegistry.registryEntrySortId unrelatedEntry)
    (Application.applicationFenceHeldWorkSortId unrelatedWork)
  assertBool
    "unrelated held work does not borrow the retired sort"
    ( Application.applicationFenceHeldWorkSortId unrelatedWork
        /= SortRegistry.registryEntrySortId oldEntry
    )
  assertEqual
    "unrelated held work is not retired-epoch residual work"
    (Right [])
    ( DisappearanceEvidence.regularRetirementResidualBlockersForHerald
        subject
        resolveIndex
        unrelatedHeld
    )
  assertEqual
    "the unrelated held state remains whole-state valid"
    (Right ())
    (validateHeraldState unrelatedHeld)

caseRegularRetirementLocallyTakenOrdinaryResiduals :: Assertion
caseRegularRetirementLocallyTakenOrdinaryResiduals = do
  (predecessor, identifier) <-
    regularRetirementLocallyTakenOrdinaryFixture invariantFixtureDescriptor
  entry <- dynamicEntryForDescriptor invariantFixtureDescriptor predecessor
  canonicalDescriptor <- applicationCanonicalDescriptor invariantFixtureDescriptor
  recordBefore <-
    checkedMaybeIO
      "reachable old-sort ordinary publication"
      ( Publication.lookupOutgoingPublication
          identifier
          (startupPublicationState predecessor)
      )
  let sortId = SortRegistry.registryEntrySortId entry
      occurrence = SortRegistry.registryEntryOccurrenceId entry
      receipts herald =
        [ (Store.storeSlotDelta slot, strength)
        | slot <- Store.retainedStoreSlots (startupStoreState herald),
          (retainedIdentifier, strength) <-
            Store.storeSlotApplicationReceipts slot,
          retainedIdentifier == identifier
        ]
      receiptsBefore = receipts predecessor
      resolveIndex = controlIndex 211
      subject =
        regularSortDefinitionDisappearanceSubject
          ( deriveRegularSortOccurrenceClaim
              (checkedSystemId fixtureStep14CheckedGenesis)
              canonicalDescriptor
              Genesis
          )
  assertEqual
    "ordinary transcript retains the old regular sort"
    sortId
    (checkedPublicationSort (Publication.outgoingPublicationChecked recordBefore))
  assertEqual
    "ordinary transcript retains the old regular occurrence"
    occurrence
    (Publication.outgoingPublicationOccurrenceId recordBefore)
  assertEqual
    "the blocked regular occurrence remains effective"
    (Just occurrence)
    ( SortRegistry.registryEntryOccurrenceId
        <$> SortRegistry.lookupEffectiveSort
          sortId
          (startupSortRegistryState predecessor)
    )
  assertBool
    "ordinary transcript retains a Store application receipt after LocalTake"
    (not (null receiptsBefore))
  outgoingDirections <-
    checkedIO
      "read locally-taken transcript outgoing peer directions"
      (PeerStream.peerStreamOutgoingDirections (startupPeerStreamState predecessor))
  activeOutgoing <-
    concat
      <$> traverse
        ( \direction ->
            checkedIO
              "read locally-taken transcript active peer outbox"
              ( PeerStream.activeOutgoingItems
                  direction
                  (startupPeerStreamState predecessor)
              )
        )
        outgoingDirections
  blockers <-
    checkedIO
      "read locally-taken regular-retirement residual work"
      ( DisappearanceEvidence.regularRetirementResidualBlockersForHerald
          subject
          resolveIndex
          predecessor
      )
  let alignment = startupAlignmentState predecessor
      retainedDebtKeys =
        Set.fromList
          [ key
          | debt <-
              StructuralDebt.structuralDebtSetEntries
                (Alignment.alignmentStructuralDebts alignment),
            let key = StructuralDebt.structuralConsequenceDebtKey debt,
            StructuralDebt.structuralDebtKeySort key
              == StructuralDebt.sortOccurrence sortId occurrence
          ]
      promotedDebtKeys =
        Set.unions
          [ Alignment.alignmentPromotionReceiptDebtKeys receipt
          | (_, receipt) <- Alignment.alignmentPromotionEntries alignment
          ]
      matchingOutbox =
        [ ( sequencedItemDirection item,
            sequencedItemSequence item,
            publicationBatchId batch,
            publicationBatchControlPrerequisite batch
          )
        | item <- activeOutgoing,
          PeerLogicalPublication peerPublication <- [sequencedItemPayload item],
          OwnerEvidence.peerPublicationIsRegularRetirementResidual
            subject
            resolveIndex
            peerPublication,
          let batch = peerPublicationBatch peerPublication
        ]
      matchingRetainedValues =
        [ ( Store.storeSlotDelta slot,
            Store.storeSlotIncarnation slot,
            checkedPublicationId publication
          )
        | slot <- Store.retainedStoreSlots (startupStoreState predecessor),
          (_, stored) <- DomainStore.retainedInstances (Store.storeSlotContents slot),
          let publication = DomainStore.storedPublication stored,
          OwnerEvidence.checkedPublicationCarriesSubjectValue
            (Store.storeSlotOccurrenceId slot)
            subject
            publication
        ]
      matchingRoutes =
        [ ( owner,
            revision,
            PlacementRoute.deltaRouteDelta route,
            PlacementRoute.deltaRouteStoreIncarnation route
          )
        | (owner, revision, route) <-
            Placement.retainedPlacementRouteEntries (startupPlacementState predecessor),
          PlacementRoute.deltaRouteSortId route == sortId,
          PlacementRoute.deltaRouteOccurrenceId route == occurrence
        ]
      blockerClassCounts =
        Map.fromListWith
          (+)
          [(disappearanceBlockerClass blocker, 1 :: Int) | blocker <- blockers]
  assertEqual
    "the exact residual-owner classes reject counterfeit absence"
    ( Map.fromList
        [ (VisibleApplicationCopyBlocker, 1),
          (RetainedStoreValueBlocker, 1),
          (RouteDependencyBlocker, 1),
          (AlignmentDebtBlocker, 20)
        ]
    )
    blockerClassCounts
  assertEqual
    "all old-sort Alignment debt is still raw and unpromoted"
    (20, 0, 20)
    ( Set.size retainedDebtKeys,
      Set.size (Set.intersection retainedDebtKeys promotedDebtKeys),
      Set.size (Set.difference retainedDebtKeys promotedDebtKeys)
    )
  assertEqual
    "raw debt has not yet produced any active, closing, or closed obligation"
    (0, 0, 0)
    ( length (Alignment.activeAlignmentObligationEntries alignment),
      length (Alignment.alignmentClosingObligationIds alignment),
      length (Alignment.alignmentClosedObligationEntries alignment)
    )
  assertEqual
    "completion before structural deletion settles the preceding peer outbox"
    (0, 0)
    ( length matchingOutbox,
      Set.size
        ( Set.fromList
            [ publication
            | (_, _, publication, _) <- matchingOutbox
            ]
        )
    )
  assertBool
    "all retained structural-reference outbox work predates Resolve"
    ( all
        (\(_, _, _, prerequisite) -> prerequisite < resolveIndex)
        matchingOutbox
    )
  assertEqual
    "LocalTake leaves exactly the ordinary publication in hidden Store retention"
    [identifier]
    [publication | (_, _, publication) <- matchingRetainedValues]
  assertEqual
    "the frozen ordinary route remains exactly where its hidden value is retained"
    [ (delta, incarnation)
    | (delta, incarnation, _) <- matchingRetainedValues
    ]
    [ (delta, incarnation)
    | (_, _, delta, incarnation) <- matchingRoutes
    ]
  assertEqual
    "the reachable predecessor remains whole-state valid"
    (Right ())
    (validateHeraldState predecessor)

caseRegularRetirementMissingSuppression :: Assertion
caseRegularRetirementMissingSuppression = do
  fixture <- regularRetirementInvariantFixture
  let predecessor = regularRetirementPredecessor fixture
      corrupted =
        replaceStartupStoreState
          (startupStoreState predecessor)
          (regularRetirementSuccessor fixture)
  assertRegularRetirementFault corrupted

caseRegularRetirementMismatchedSuppression :: Assertion
caseRegularRetirementMismatchedSuppression = do
  fixture <- regularRetirementInvariantFixture
  let predecessor = regularRetirementPredecessor fixture
      mismatchedIndex = controlIndex 1
  assertBool
    "mismatched fixture uses a different Resolve index"
    (mismatchedIndex /= regularRetirementResolveIndex fixture)
  prepared <-
    checkedIO
      "prepare mismatched regular-retirement suppression"
      ( Store.prepareRegularDefinitionRetirement
          (regularRetirementSubject fixture)
          ( Publication.publicationCurrentHeraldPrefix
              (startupPublicationState predecessor)
          )
          mismatchedIndex
          (startupStoreState predecessor)
      )
  let (store, _) = Store.commitRegularDefinitionRetirement prepared
      corrupted =
        replaceStartupStoreState store (regularRetirementSuccessor fixture)
  assertRegularRetirementFault corrupted

caseRegularRetirementTooSmallCut :: Assertion
caseRegularRetirementTooSmallCut = do
  fixture <- regularRetirementInvariantFixture
  let predecessor = regularRetirementPredecessor fixture
  prepared <-
    checkedIO
      "prepare regular-retirement suppression with an empty local cut"
      ( Store.prepareRegularDefinitionRetirement
          (regularRetirementSubject fixture)
          DomainAlignment.EmptyHeraldPublicationPrefix
          (regularRetirementResolveIndex fixture)
          (startupStoreState predecessor)
      )
  let (store, _) = Store.commitRegularDefinitionRetirement prepared
      corrupted =
        replaceStartupStoreState store (regularRetirementSuccessor fixture)
  assertRegularRetirementFault corrupted

caseRegularRetirementLaggingControl :: Assertion
caseRegularRetirementLaggingControl = do
  fixture <- regularRetirementInvariantFixture
  let corrupted =
        replaceStartupStructuralProgressState
          ( startupStructuralProgressState
              (regularRetirementPredecessor fixture)
          )
          (regularRetirementSuccessor fixture)
  assertRegularRetirementFault corrupted

caseRegularRetirementLowRedefinitionControl :: Assertion
caseRegularRetirementLowRedefinitionControl = do
  fixture <- regularRetirementInvariantFixture
  let retired = regularRetirementSuccessor fixture
      entry = regularRetirementEntry fixture
      identifier = SortRegistry.registryEntryPublicationId entry
  record <-
    checkedMaybeIO
      "retired definition publication"
      ( Publication.lookupOutgoingPublication
          identifier
          (startupPublicationState retired)
      )
  assertBool
    "original definition predates its Resolve prerequisite"
    ( Publication.outgoingPublicationControlPrerequisite record
        < regularRetirementResolveIndex fixture
    )
  prepared <-
    checkedIO
      "re-induce successor occurrence with stale publication provenance"
      ( SortRegistry.prepareSortInduction
          (SortRegistry.registryEntryDescriptor entry)
          (regularRetirementSuccessorOccurrence fixture)
          (Publication.outgoingPublicationChecked record)
          (startupSortRegistryState retired)
      )
  let corrupted =
        replaceStartupSortRegistryState
          (SortRegistry.commitSortInduction prepared)
          retired
  assertRegularRetirementFault corrupted

caseRegularRetirementDelayedPlacement :: Assertion
caseRegularRetirementDelayedPlacement = do
  fixture <- regularRetirementInvariantFixture
  let retired = regularRetirementSuccessor fixture
      entry = regularRetirementEntry fixture
      placementState = startupPlacementState retired
      oldOccurrence = SortRegistry.registryEntryOccurrenceId entry
      oldSort = SortRegistry.registryEntrySortId entry
      process = processFromRetirementFixture fixture
      delayed =
        Placement.localPlacement
          (checked "delayed old-occurrence DeltaId" (mkDeltaId (fixtureIdentifierBytes 242)))
          oldSort
          oldOccurrence
          (globalObjectIdFromProcessEpochId process)
          process
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
          ( checked
              "delayed old-occurrence StoreIncarnationId"
              (mkStoreIncarnationId (fixtureIdentifierBytes 243))
          )
          (regularRetirementResolveIndex fixture)
  applicationPrepared <-
    checkedIO
      "prepare delayed old-occurrence Placement evidence"
      ( Placement.preparePlacementBootstrap
          (delayed : Placement.localPlacements placementState)
          Placement.emptyState
      )
  systemPrepared <-
    checkedIO
      "restore system-view Placement evidence"
      ( Placement.prepareSystemViewPlacements
          (Placement.systemViewPlacements placementState)
          (Placement.commitPlacementBootstrap applicationPrepared)
      )
  let corrupted =
        replaceStartupPlacementState
          (Placement.commitSystemViewPlacements systemPrepared)
          retired
  assertRegularRetirementFault corrupted

assertRegularRetirementFault :: HeraldState -> Assertion
assertRegularRetirementFault =
  assertFault
    (HeraldStartupInvariant StartupStatic DynamicSortRegistryInvariant)

processFromRetirementFixture :: RegularRetirementInvariantFixture -> ProcessEpochId
processFromRetirementFixture fixture =
  case Controlled.controlledProcessFacts
    (startupControlledState (regularRetirementPredecessor fixture)) of
    fact : _ -> Controlled.processFactProcessEpoch fact
    [] -> error "regular retirement fixture has no resident process"

caseStructuralPublicationInvariant :: Assertion
caseStructuralPublicationInvariant = do
  fixture <- structuralInvariantFixture
  assertEqual
    "valid live controlled structural publication"
    (Right ())
    (validateHeraldState (structuralState fixture))
  case Controlled.controlledLocalRecord (structuralObject fixture) (startupControlledState (structuralState fixture)) of
    Just record ->
      assertEqual
        "the linked structural first use is current"
        Controlled.ControlledCurrent
        (Controlled.controlledRecordLifecycle record)
    Nothing -> assertFailure "the published object has no controlled record"

caseStructuralPublicationRetry :: Assertion
caseStructuralPublicationRetry = do
  fixture <- structuralInvariantFixture
  retained <-
    checkedIO
      "classify controlled structural retry"
      ( Application.classifyApplicationRequest
          (structuralSession fixture)
          (structuralBinding fixture)
          (structuralRequest fixture)
          ( WriteApplication
              (structuralPrivateNabla fixture)
              (PublishValue (structuralValue fixture))
          )
          (startupApplicationState (structuralState fixture))
      )
  case retained of
    Application.RetainedApplicationRequest _ -> pure ()
    Application.FirstApplicationRequest _ ->
      assertFailure "controlled structural retry classified as first"
    Application.AttachedApplicationRequest _ ->
      assertFailure "controlled structural retry classified as attached"
    Application.ConflictingApplicationRequest _ ->
      assertFailure "controlled structural retry classified as conflict"
  assertEqual
    "retried structural raw-to-semantic link remains valid"
    (Right ())
    (validateHeraldState (structuralState fixture))

caseStructuralPublicationSessionRetirement :: Assertion
caseStructuralPublicationSessionRetirement = do
  fixture <- structuralInvariantFixture
  prepared <-
    checkedIO
      "end structural invariant session"
      ( Application.prepareApplicationSessionEnd
          (structuralSession fixture)
          (structuralBinding fixture)
          (startupApplicationState (structuralState fixture))
      )
  let endedApplication = Application.commitApplicationSessionEnd prepared
      ended =
        replaceStartupApplicationState
          endedApplication
          (structuralState fixture)
  assertEqual
    "structural session retirement preserves a valid semantic publication"
    (Right ())
    (validateHeraldState ended)
  assertEqual
    "structural raw request was retired"
    []
    ( Application.applicationRequestEntries
        (structuralSession fixture)
        endedApplication
    )
  assertEqual
    "controlled structural publication remains retained"
    1
    ( length
        ( Publication.applicationPublicationEntries
            (startupPublicationState ended)
        )
    )

caseBootstrapOperatePossession :: Assertion
caseBootstrapOperatePossession = do
  state <- validState
  let controlled = startupControlledState state
  mapM_
    ( \process ->
        assertBool
          "bootstrap process role retains normal possession"
          ( Controlled.controlledHasNormalPossession
              (Controlled.processFactProcessEpoch process)
              ( globalObjectIdFromProcessEpochId
                  (Controlled.processFactProcessEpoch process)
              )
              controlled
          )
    )
    (Controlled.controlledProcessFacts controlled)
  mapM_
    ( \root ->
        let process = Controlled.rootFactProcessEpoch root
            object = case Controlled.rootFactRole root of
              Controlled.ControlledWriter nabla _ ->
                globalObjectIdFromNablaId nabla
              Controlled.ControlledReader delta ->
                globalObjectIdFromDeltaId delta
         in assertBool
              "bootstrap root role retains normal possession"
              (Controlled.controlledHasNormalPossession process object controlled)
    )
    (Controlled.controlledRootFacts controlled)

data StructuralInvariantFixture = StructuralInvariantFixture
  { structuralState :: HeraldState,
    structuralObject :: GlobalObjectId,
    structuralSession :: ApplicationSession.ApplicationSessionId,
    structuralBinding :: ApplicationSession.ApplicationSessionBinding,
    structuralRequest :: ApplicationRequest.RequestId,
    structuralPrivateNabla :: PrivateNablaId,
    structuralValue :: ApplicationValue.ApplicationValue
  }

data GenericInvariantFixture = GenericInvariantFixture
  { state :: HeraldState,
    process :: ProcessEpochId,
    session :: ApplicationSession.ApplicationSessionId,
    binding :: ApplicationSession.ApplicationSessionBinding,
    definitionWriter :: PrivateNablaId
  }

data RegularRetirementInvariantFixture = RegularRetirementInvariantFixture
  { regularRetirementPredecessor :: HeraldState,
    regularRetirementSuccessor :: HeraldState,
    regularRetirementEntry :: SortRegistry.RegistryEntry,
    regularRetirementSubject :: DisappearanceSubject,
    regularRetirementResolveIndex :: ControlIndex,
    regularRetirementSuccessorOccurrence :: SortDefinitionOccurrenceId,
    regularRetirementApplication :: GenericInvariantFixture
  }

dynamicEntryForDescriptor ::
  ApplicationSortDescriptor ->
  HeraldState ->
  IO SortRegistry.RegistryEntry
dynamicEntryForDescriptor descriptor herald =
  applicationCanonicalDescriptor descriptor >>= \canonicalDescriptor ->
    case [ entry
         | entry <-
             SortRegistry.registryEntries
               (startupSortRegistryState herald),
           SortRegistry.registryEntryPredefinedRole entry == Nothing,
           SortRegistry.registryEntryDescriptor entry == canonicalDescriptor
         ] of
      [entry] -> pure entry
      entries ->
        assertFailure
          ( "expected one dynamic registry entry for the descriptor, got "
              <> show (length entries)
          )

singleApplicationHeldWork ::
  HeraldState ->
  IO Application.ApplicationFenceHeldWork
singleApplicationHeldWork herald =
  case Application.applicationFenceHeldEntries
    (startupApplicationState herald) of
    [entry] -> pure (Application.applicationFenceHeldSemanticWork entry)
    entries ->
      assertFailure
        ( "expected one application-held publication, got "
            <> show (length entries)
        )

retireRegularDefinitionInState ::
  CheckedHeraldGenesis ->
  ApplicationSortDescriptor ->
  ControlIndex ->
  HeraldState ->
  IO HeraldState
retireRegularDefinitionInState genesis descriptor resolveIndex predecessor = do
  entry <- dynamicEntryForDescriptor descriptor predecessor
  canonicalDescriptor <- applicationCanonicalDescriptor descriptor
  let subject =
        regularSortDefinitionDisappearanceSubject
          ( deriveRegularSortOccurrenceClaim
              (checkedSystemId genesis)
              canonicalDescriptor
              Genesis
          )
      sortId = SortRegistry.registryEntrySortId entry
      localCut =
        Publication.publicationCurrentHeraldPrefix
          (startupPublicationState predecessor)
  successorBase <-
    checkedIO
      "derive regular-retirement fixture successor base"
      (resolvedRetirementOccurrenceBase resolveIndex)
  let successorOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId genesis)
          sortId
          successorBase
  registryPrepared <-
    checkedIO
      "prepare regular-retirement fixture registry transition"
      ( SortRegistry.prepareExactRegularSortRetirement
          (checkedSystemId genesis)
          entry
          resolveIndex
          successorOccurrence
          (startupSortRegistryState predecessor)
      )
  storePrepared <-
    checkedIO
      "prepare regular-retirement fixture Store suppression"
      ( Store.prepareRegularDefinitionRetirement
          subject
          localCut
          resolveIndex
          (startupStoreState predecessor)
      )
  progressPrepared <-
    checkedIO
      "prepare regular-retirement fixture structural control advance"
      ( GraphProgress.prepareStructuralControlProgress
          resolveIndex
          (startupStructuralProgressState predecessor)
      )
  let registry = SortRegistry.commitRegularSortRetirement registryPrepared
      (store, _) = Store.commitRegularDefinitionRetirement storePrepared
      (progress, _) =
        GraphProgress.commitStructuralControlProgress progressPrepared
  pure
    ( replaceStartupStructuralProgressState progress
        . replaceStartupStoreState store
        . replaceStartupSortRegistryState registry
        $ predecessor
    )

applicationCanonicalDescriptor ::
  ApplicationSortDescriptor ->
  IO CanonicalDescriptor
applicationCanonicalDescriptor descriptor =
  ApplicationSortDefinition.admittedApplicationSortDescriptor
    <$> checkedIO
      "admit application descriptor for invariant fixture"
      ( ApplicationSortDefinition.admitApplicationSortDefinition
          (DeclaredSortDefinition descriptor Nothing)
      )

regularRetirementInvariantFixture :: IO RegularRetirementInvariantFixture
regularRetirementInvariantFixture = do
  generic <- genericInvariantFixture invariantFixtureDescriptor
  predecessor <- completeFixtureOutgoingWork (state generic)
  let dynamicEntries =
        [ entry
        | entry <- SortRegistry.registryEntries (startupSortRegistryState predecessor),
          SortRegistry.registryEntryPredefinedRole entry == Nothing
        ]
  entry <- case dynamicEntries of
    [one] -> pure one
    entries ->
      assertFailure
        ( "expected one dynamic registry entry, got "
            <> show (length entries)
        )
  let descriptor = SortRegistry.registryEntryDescriptor entry
      sortId = SortRegistry.registryEntrySortId entry
      subject =
        regularSortDefinitionDisappearanceSubject
          ( deriveRegularSortOccurrenceClaim
              (checkedSystemId fixtureCheckedGenesis)
              descriptor
              Genesis
          )
      resolveIndex = controlIndex 211
  successorBase <-
    checkedIO
      "derive regular-retirement successor base"
      (resolvedRetirementOccurrenceBase resolveIndex)
  let successorOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixtureCheckedGenesis)
          sortId
          successorBase
      localCut =
        Publication.publicationCurrentHeraldPrefix
          (startupPublicationState predecessor)
  registryPrepared <-
    checkedIO
      "prepare invariant-fixture registry retirement"
      ( SortRegistry.prepareExactRegularSortRetirement
          (checkedSystemId fixtureCheckedGenesis)
          entry
          resolveIndex
          successorOccurrence
          (startupSortRegistryState predecessor)
      )
  storePrepared <-
    checkedIO
      "prepare invariant-fixture Store suppression"
      ( Store.prepareRegularDefinitionRetirement
          subject
          localCut
          resolveIndex
          (startupStoreState predecessor)
      )
  progressPrepared <-
    checkedIO
      "prepare invariant-fixture structural control advance"
      ( GraphProgress.prepareStructuralControlProgress
          resolveIndex
          (startupStructuralProgressState predecessor)
      )
  let registry = SortRegistry.commitRegularSortRetirement registryPrepared
      (store, _) = Store.commitRegularDefinitionRetirement storePrepared
      (progress, _) =
        GraphProgress.commitStructuralControlProgress progressPrepared
      successor =
        replaceStartupStructuralProgressState progress
          . replaceStartupStoreState store
          . replaceStartupSortRegistryState registry
          $ predecessor
  pure
    RegularRetirementInvariantFixture
      { regularRetirementPredecessor = predecessor,
        regularRetirementSuccessor = successor,
        regularRetirementEntry = entry,
        regularRetirementSubject = subject,
        regularRetirementResolveIndex = resolveIndex,
        regularRetirementSuccessorOccurrence = successorOccurrence,
        regularRetirementApplication = generic
      }

-- | The direct retirement fixture starts after real cumulative completion of
-- its ready source publications. A retained definition outbox item is an
-- absence blocker even when no application reader was reachable at admission.
completeFixtureOutgoingWork :: HeraldState -> IO HeraldState
completeFixtureOutgoingWork predecessor = do
  directions <- checkedIO "fixture outgoing directions" (PeerStream.peerStreamOutgoingDirections (startupPeerStreamState predecessor))
  streams <- foldM complete (startupPeerStreamState predecessor) directions
  pure (replaceStartupPeerStreamState streams predecessor)
  where
    complete streams direction = do
      items <- checkedIO "fixture retained outgoing items" (PeerStream.activeOutgoingItems direction streams)
      case items of
        [] -> pure streams
        _ -> do
          let prefix = streamPrefixThrough (maximum (map sequencedItemSequence items))
          prepared <- checkedIO "fixture cumulative completed prefix" (PeerStream.prepareCompletedAck direction prefix streams)
          pure (fst (PeerStream.commitCompletedAck prepared))

commitRegularRetirementRedefinition ::
  RegularRetirementInvariantFixture ->
  IO HeraldState
commitRegularRetirementRedefinition fixture = do
  let predecessor = regularRetirementSuccessor fixture
      application = regularRetirementApplication fixture
      rawValue =
        ApplicationValue.SortDefinitionValue
          (DeclaredSortDefinition invariantFixtureDescriptor Nothing)
      call =
        WriteApplication
          (definitionWriter application)
          (PublishValue rawValue)
  classified <-
    checkedIO
      "classify post-Resolve equal redefinition"
      ( Application.classifyApplicationRequest
          (session application)
          (binding application)
          (requestId 720)
          call
          (startupApplicationState predecessor)
      )
  request <- case classified of
    Application.FirstApplicationRequest candidate -> pure candidate
    Application.RetainedApplicationRequest _ ->
      assertFailure "fresh post-Resolve redefinition classified as retained"
    Application.AttachedApplicationRequest _ ->
      assertFailure "fresh post-Resolve redefinition classified as attached"
    Application.ConflictingApplicationRequest _ ->
      assertFailure "fresh post-Resolve redefinition classified as conflict"
  prepared <-
    checkedIO
      "prepare post-Resolve equal redefinition"
      ( ApplicationPublication.prepareLocalApplicationPublication
          fixtureCheckedGenesis
          fixtureHeraldMembershipGeneration
          ( Application.processRoleView
              ( OracleProjection.projectedProcessEpochs
                  (startupOracleProjectionState predecessor)
              )
          )
          request
          (definitionWriter application)
          rawValue
          (startupControlledState predecessor)
          (startupSortRegistryState predecessor)
          (startupGraphState predecessor)
          (startupStructuralProgressState predecessor)
          (startupPlacementState predecessor)
          (startupPublicationState predecessor)
          (startupStoreState predecessor)
          (startupPeerStreamState predecessor)
      )
  let (applicationState, controlled, registry, publication, store, peerStream, _) =
        ApplicationPublication.commitLocalApplicationPublication prepared
  pure
    ( replaceStartupPeerStreamState peerStream
        . replaceStartupStoreState store
        . replaceStartupPublicationState publication
        . replaceStartupSortRegistryState registry
        . replaceStartupControlledState controlled
        . replaceStartupApplicationState applicationState
        $ predecessor
    )

genericInvariantFixture :: ApplicationSortDescriptor -> IO GenericInvariantFixture
genericInvariantFixture descriptor = do
  bootstraps <- selectedBootstraps fixtureCheckedGenesis
  bootstrap <- case checkedInitialBootstraps bootstraps of
    [one] -> pure one
    others -> assertFailure ("expected one local bootstrap, got " <> show (length others))
  (initial, _) <-
    checkedIO
      "initialize generic invariant fixture"
      ( initialHerald
          (monotonicInstant 71)
          fixtureCheckedGenesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  attachment <-
    checkedMaybeIO
      "generic invariant attachment"
      ( primordialApplicationAttachment
          fixtureCheckedGenesis
          bootstraps
          (appliedBootstrapManifestId bootstrap)
      )
  (opened, openEffects) <-
    checkedIO
      "open generic invariant session"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 72)
              ( ApplicationSessionInput
                  ( OpenApplicationSession
                      (candidateApplicationLane 717)
                      attachment
                      (ApplicationSession.clientNonce 718)
                  )
              )
          )
          initial
      )
  acceptance <- case effectBatchMembers openEffects of
    [SetApplicationConnectionDisposition _ one] -> pure one
    effects -> assertFailure ("unexpected generic session-open effects: " <> show effects)
  (session, startupAccess) <- case sessionAcceptanceReply acceptance of
    SessionOpened openedSession _ access -> pure (openedSession, access)
    reply -> assertFailure ("expected SessionOpened, got " <> show reply)
  sortAccess <-
    checkedMaybeIO
      "generic sort-definition access"
      ( find
          ((== Access.SortDefinitionRole) . Access.predefinedAccessRole)
          (conventionalStartupPairs startupAccess)
      )
  let applicationState = startupApplicationState opened
      requestIdValue = requestId 719
      rawValue =
        ApplicationValue.SortDefinitionValue
          (DeclaredSortDefinition descriptor Nothing)
      writer = Access.predefinedWriter sortAccess
      call = WriteApplication writer (PublishValue rawValue)
  classified <-
    checkedIO
      "classify generic invariant publication"
      ( Application.classifyApplicationRequest
          session
          (sessionAcceptanceBinding acceptance)
          requestIdValue
          call
          applicationState
      )
  request <- case classified of
    Application.FirstApplicationRequest candidate -> pure candidate
    Application.RetainedApplicationRequest _ ->
      assertFailure "fresh generic invariant request classified as retained"
    Application.AttachedApplicationRequest _ ->
      assertFailure "fresh generic invariant request classified as attached"
    Application.ConflictingApplicationRequest _ ->
      assertFailure "fresh generic invariant request classified as conflict"
  prepared <-
    checkedIO
      "prepare generic invariant publication"
      ( ApplicationPublication.prepareLocalApplicationPublication
          fixtureCheckedGenesis
          fixtureHeraldMembershipGeneration
          ( Application.processRoleView
              ( OracleProjection.projectedProcessEpochs
                  (startupOracleProjectionState opened)
              )
          )
          request
          writer
          rawValue
          (startupControlledState opened)
          (startupSortRegistryState opened)
          (startupGraphState opened)
          (startupStructuralProgressState opened)
          (startupPlacementState opened)
          (startupPublicationState opened)
          (startupStoreState opened)
          (startupPeerStreamState opened)
      )
  let (application, controlled, registry, publication, store, peerStream, _) =
        ApplicationPublication.commitLocalApplicationPublication prepared
      successor =
        replaceStartupPeerStreamState peerStream
          . replaceStartupStoreState store
          . replaceStartupPublicationState publication
          . replaceStartupSortRegistryState registry
          . replaceStartupControlledState controlled
          . replaceStartupApplicationState application
          $ opened
  pure
    GenericInvariantFixture
      { state = successor,
        process = appliedProcessEpochId bootstrap,
        session,
        binding = sessionAcceptanceBinding acceptance,
        definitionWriter = writer
      }

structuralInvariantFixture :: IO StructuralInvariantFixture
structuralInvariantFixture = do
  bootstraps <- selectedBootstraps fixtureCheckedGenesis
  bootstrap <- case checkedInitialBootstraps bootstraps of
    [one] -> pure one
    others ->
      assertFailure
        ("expected one local structural bootstrap, got " <> show (length others))
  (initial, _) <-
    checkedIO
      "initialize structural invariant fixture"
      ( initialHerald
          (monotonicInstant 81)
          fixtureCheckedGenesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  attachment <-
    checkedMaybeIO
      "structural invariant attachment"
      ( primordialApplicationAttachment
          fixtureCheckedGenesis
          bootstraps
          (appliedBootstrapManifestId bootstrap)
      )
  (opened, openEffects) <-
    checkedIO
      "open structural invariant session"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 82)
              ( ApplicationSessionInput
                  ( OpenApplicationSession
                      (candidateApplicationLane 817)
                      attachment
                      (ApplicationSession.clientNonce 818)
                  )
              )
          )
          initial
      )
  acceptance <- case effectBatchMembers openEffects of
    [SetApplicationConnectionDisposition _ one] -> pure one
    effects ->
      assertFailure
        ("unexpected structural session-open effects: " <> show effects)
  (session, startupAccess) <- case sessionAcceptanceReply acceptance of
    SessionOpened openedSession _ access -> pure (openedSession, access)
    reply -> assertFailure ("expected SessionOpened, got " <> show reply)
  neutralAccess <-
    checkedMaybeIO
      "neutral-vertex access"
      ( find
          ((== Access.NeutralVertexRole) . Access.predefinedAccessRole)
          (conventionalStartupPairs startupAccess)
      )
  let binding = sessionAcceptanceBinding acceptance
      privateNabla = Access.predefinedWriter neutralAccess
      newIdRequest = requestId 819
  (reserved, newIdEffects) <-
    checkedIO
      "reserve structural identity"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 83)
              ( ApplicationRequestInput
                  ( CallApplicationRequest
                      binding
                      session
                      newIdRequest
                      (NewIdApplication (ControlledNewId privateNabla))
                  )
              )
          )
          opened
      )
  privateObject <- case effectBatchMembers newIdEffects of
    [ SendApplicationReply
        _
        ( RetainedRequestReply
            _
            (Completed actual (NewIdCompleted generated))
          )
      ] -> do
        assertEqual "structural NewId request" newIdRequest actual
        pure generated
    effects ->
      assertFailure
        ("unexpected structural NewId effects: " <> show effects)
  let rawValue =
        ApplicationValue.RecordValue
          ( Map.fromList
              [ ( "label",
                  ApplicationValue.LabelValue (ApplicationValue.VoidLabel, 0)
                ),
                ( "object_id",
                  ApplicationValue.UniqueIdValue privateObject
                )
              ]
          )
      publicationRequest = requestId 820
  classified <-
    checkedIO
      "classify structural invariant publication"
      ( Application.classifyApplicationRequest
          session
          binding
          publicationRequest
          (WriteApplication privateNabla (PublishValue rawValue))
          (startupApplicationState reserved)
      )
  request <- case classified of
    Application.FirstApplicationRequest candidate -> pure candidate
    Application.RetainedApplicationRequest _ ->
      assertFailure "fresh structural invariant request classified as retained"
    Application.AttachedApplicationRequest _ ->
      assertFailure "fresh structural invariant request classified as attached"
    Application.ConflictingApplicationRequest _ ->
      assertFailure "fresh structural invariant request classified as conflict"
  successor <- commitStructuralInvariantPublication request reserved
  object <-
    fmap
      globalObjectIdFromGlobalUniqueId
      ( checkedIO
          "resolve published structural object"
          ( Application.resolveApplicationPrivateUniqueId
              (appliedProcessEpochId bootstrap)
              privateObject
              (startupApplicationState successor)
          )
      )
  pure
    StructuralInvariantFixture
      { structuralState = successor,
        structuralObject = object,
        structuralSession = session,
        structuralBinding = binding,
        structuralRequest = publicationRequest,
        structuralPrivateNabla = privateNabla,
        structuralValue = rawValue
      }

commitStructuralInvariantPublication ::
  Application.ApplicationRequestCandidate ->
  HeraldState ->
  IO HeraldState
commitStructuralInvariantPublication request predecessor = do
  (privateNabla, value) <-
    case Application.applicationRequestCandidateCall request of
      WriteApplication writer (PublishValue supplied) -> pure (writer, supplied)
      other -> assertFailure ("unexpected structural invariant call: " <> show other)
  prepared <-
    checkedIO
      "prepare structural invariant publication"
      ( ApplicationPublication.prepareLocalApplicationPublication
          fixtureCheckedGenesis
          fixtureHeraldMembershipGeneration
          ( Application.processRoleView
              ( OracleProjection.projectedProcessEpochs
                  (startupOracleProjectionState predecessor)
              )
          )
          request
          privateNabla
          value
          (startupControlledState predecessor)
          (startupSortRegistryState predecessor)
          (startupGraphState predecessor)
          (startupStructuralProgressState predecessor)
          (startupPlacementState predecessor)
          (startupPublicationState predecessor)
          (startupStoreState predecessor)
          (startupPeerStreamState predecessor)
      )
  let (application, controlled, registry, publication, store, peerStream, _) =
        ApplicationPublication.commitLocalApplicationPublication prepared
  pure
    ( replaceStartupPeerStreamState peerStream
        . replaceStartupStoreState store
        . replaceStartupPublicationState publication
        . replaceStartupSortRegistryState registry
        . replaceStartupControlledState controlled
        . replaceStartupApplicationState application
        $ predecessor
    )

caseOracleClientClaimsContradiction :: Assertion
caseOracleClientClaimsContradiction = do
  state <- validState
  bootstraps <- selectedBootstraps fixtureCheckedGenesis
  let replacement =
        OracleClient.initialState
          ( oracleHelloClaims
              (checked "mismatched Oracle client SystemId" (mkSystemId (fixtureIdentifierBytes 99)))
              (checkedCatalogueDigest fixtureCheckedGenesis)
              (checkedConfigurationDigest fixtureCheckedGenesis)
              (checkedInitialProjectionDigest bootstraps)
              (checkedLocalHeraldId fixtureCheckedGenesis)
              (checkedLocalHeraldEpoch fixtureCheckedGenesis)
              ( heraldMembershipGenerationId
                  ( OracleProjection.oracleViewCurrentHeraldMembership
                      (OracleProjection.oracleView (startupOracleProjectionState state))
                  )
              )
          )
          fixtureOracleContacts
          (checkedOracleControlIndex fixtureCheckedGenesis)
  assertFault
    (HeraldStartupInvariant StartupStatic OracleClientClaimsInvariant)
    (replaceStartupOracleClientState replacement state)

caseDrainingLabelInvocationContradiction :: Assertion
caseDrainingLabelInvocationContradiction = do
  state <- heldFixtureStateForOwnerProperties
  assertEqual "the source fixture is whole-state valid" (Right ()) (validateHeraldState state)
  let client = startupOracleClientState state
  prepared <- case [ intent
                   | (_, witness) <- OracleClient.oracleClientRequestEntries client,
                     let intent = OracleClient.oracleRequestWitnessIntent witness,
                     OracleClient.DecideLabelIntent {} <- [intent]
                   ] of
    OracleClient.DecideLabelIntent _ process object expected evidence authority justification target cut : _ ->
      checkedIO
        "reserve an independently valid but unowned source invocation"
        ( OracleClient.prepareDrainingLabelDecisionRequest
            "unowned-draining-label-invariant"
            process
            object
            expected
            evidence
            authority
            justification
            target
            cut
            client
        )
    _ -> assertFailure "expected a retained source label decision"
  let (replacement, _, _, reference) = OracleClient.commitLabelDecisionRequest prepared
  assertEqual "the client alone accepts its checked reservation" (Right ()) (OracleClient.validateState replacement)
  assertEqual
    "the new reservation is not dispatchable"
    (Just OracleClient.OracleRequestAwaitingSourceDrain)
    (OracleClient.oracleRequestWitnessStatus <$> OracleClient.lookupOracleRequest reference replacement)
  assertFault
    (HeraldStartupInvariant StartupStatic LabelOracleRequestInvariant)
    (replaceStartupOracleClientState replacement state)

casePeerBindingContradiction :: Assertion
casePeerBindingContradiction = do
  state <- validState
  bootstraps <- selectedBootstraps fixtureCheckedGenesis
  let candidate =
        Discovery.peerCandidate
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          (Discovery.connectionNonce 81)
      hello =
        Discovery.peerHello
          (checkedSystemId fixtureCheckedGenesis)
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          (Discovery.connectionNonce 81)
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixtureCheckedGenesis)
          (checkedInitialProjectionDigest bootstraps)
          Nothing
  prepared <-
    checkedIO
      "prepare a Discovery-only peer binding"
      (DiscoveryState.preparePeerHello candidate hello (startupDiscoveryState state))
  let (discovery, _) = DiscoveryState.commitPeerHello prepared
  assertFault
    (HeraldStartupInvariant StartupStatic PeerStreamOwnerInvariant)
    (replaceStartupDiscoveryState discovery state)

caseOracleContradiction :: Assertion
caseOracleContradiction = do
  state <- validState
  emptyBootstraps <-
    checkedIO
      "check empty applied fixture"
      (checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest []))
  let replacement = OracleProjection.initialState fixtureCheckedGenesis emptyBootstraps
  assertFault
    (HeraldStartupInvariant StartupStatic OracleInitialProjectionDigestInvariant)
    (replaceStartupOracleProjectionState replacement state)

caseOracleLocalIdentityContradiction :: Assertion
caseOracleLocalIdentityContradiction = do
  state <- validState
  remoteGenesis <- checkedIO "check remote perspective" (checkHeraldGenesis (fixtureDeploymentAt fixtureRemoteMember))
  remoteBootstraps <- selectedBootstraps remoteGenesis
  let replacement = OracleProjection.initialState remoteGenesis remoteBootstraps
  assertFault
    (HeraldStartupInvariant StartupStatic OracleLocalIdentityInvariant)
    (replaceStartupOracleProjectionState replacement state)

caseOracleMembershipContradiction :: Assertion
caseOracleMembershipContradiction = case deploymentActiveHeralds fixtureDeploymentManifest of
  local : remaining -> do
    state <- validState
    alternateId <- checkedIO "alternate local HeraldId" (mkHeraldId (fixtureIdentifierBytes 247))
    let alternateLocal = local {heraldMemberId = alternateId}
        alternateMembers = alternateLocal : remaining
        manifest =
          fixtureDeploymentManifest
            { deploymentLocalHeraldId = alternateId,
              deploymentActiveHeralds = alternateMembers,
              deploymentOracleGenesis =
                (deploymentOracleGenesis fixtureDeploymentManifest)
                  { oracleGenesisActiveHeralds = alternateMembers
                  }
            }
    alternateGenesis <- checkedIO "check alternate Herald membership" (checkHeraldGenesis manifest)
    alternateBootstraps <- selectedBootstraps alternateGenesis
    let replacement = OracleProjection.initialState alternateGenesis alternateBootstraps
    assertFault
      (HeraldStartupInvariant StartupStatic OracleMembershipInvariant)
      (replaceStartupOracleProjectionState replacement state)
  [] -> assertFailure "fixture has no active Herald"

caseSuccessorMembershipInvariant :: Assertion
caseSuccessorMembershipInvariant = do
  (_, advanced) <- successorMembershipFixture
  assertEqual
    "the coordinated successor membership preserves every whole-state relation"
    (Right ())
    (validateHeraldState advanced)

caseHistoricalStructuralPublicationMembershipInvariant :: Assertion
caseHistoricalStructuralPublicationMembershipInvariant = do
  fixture <- structuralInvariantFixture
  beforePrefix <- bindOracleForMembership (structuralState fixture)
  let predecessor = fixtureHeraldAfterProbePrefix beforePrefix
  assertEqual
    "the frozen structural publication starts in a valid predecessor"
    (Right ())
    (validateHeraldState predecessor)
  let index = controlIndex 2
      target = heraldMemberEpoch fixtureRemoteMember
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState predecessor))
      successor =
        checked
          "historical-publication successor membership"
          (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) target membership)
      retainedMembershipIds =
        fmap
          (Publication.applicationPublicationMembershipGenerationId . snd)
          (Publication.applicationPublicationEntries (startupPublicationState predecessor))
  assertBool
    "the retained structural publication captures the genesis membership generation"
    ( not (null retainedMembershipIds)
        && all (== heraldMembershipGenerationId membership) retainedMembershipIds
    )
  prepared <-
    checkedIO
      "advance membership with frozen structural publication"
      (MembershipAdvance.prepareMembershipAdvance index successor [] predecessor)
  let advanced = MembershipAdvance.commitMembershipAdvance prepared
  assertBool
    "the membership cut does not rewrite retained publication membership evidence"
    ( all
        (/= heraldMembershipGenerationId successor)
        ( fmap
            (Publication.applicationPublicationMembershipGenerationId . snd)
            (Publication.applicationPublicationEntries (startupPublicationState advanced))
        )
    )
  assertEqual
    "the pre-retirement route remains valid until loss-aware closure"
    (Right ())
    (validateHeraldState advanced)

caseDiscoveryMembershipContradiction :: Assertion
caseDiscoveryMembershipContradiction = do
  (initial, advanced) <- successorMembershipFixture
  assertFault
    (HeraldStartupInvariant StartupStatic DiscoveryOwnerInvariant)
    ( replaceStartupDiscoveryState
        (startupDiscoveryState initial)
        advanced
    )

successorMembershipFixture :: IO (HeraldState, HeraldState)
successorMembershipFixture = do
  unbound <- validState
  beforePrefix <- bindOracleForMembership unbound
  let initial = fixtureHeraldAfterProbePrefix beforePrefix
  let index = controlIndex 2
      target = heraldMemberEpoch fixtureRemoteMember
      successor =
        checked
          "whole-state successor membership"
          ( retireHeraldMembershipGeneration
              index
              (fixtureRetirementResolution index)
              target
              fixtureHeraldMembershipGeneration
          )
  prepared <-
    checkedIO
      "coordinated whole-state membership advance"
      (MembershipAdvance.prepareMembershipAdvance index successor [] initial)
  pure (initial, MembershipAdvance.commitMembershipAdvance prepared)

bindOracleForMembership :: HeraldState -> IO HeraldState
bindOracleForMembership state = do
  attempt <- case OracleClient.oracleClientActions (startupOracleClientState state) of
    [ConnectAndHelloOracle candidate _] -> pure candidate
    actions -> assertFailure ("expected one initial Oracle connection attempt, got " <> show actions)
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (oracleConnectAttemptFromExclusive attempt)
          (Just node)
          True
  fst
    <$> checkedIO
      "bind Oracle before membership advance"
      ( verifiedStepHerald
          ( heraldInput
              (startupLastObservedTime state)
              (OracleInput (OracleHelloReceived attempt acceptance))
          )
          state
      )

caseOracleAppliedContradiction :: Assertion
caseOracleAppliedContradiction = case deploymentConfiguredProcesses fixtureDeploymentManifest of
  configured : remaining -> do
    state <- validState
    alternateProcessId <- checkedIO "alternate stable ProcessId" (mkProcessId (fixtureIdentifierBytes 249))
    let manifest =
          fixtureDeploymentManifest
            { deploymentConfiguredProcesses =
                configured {configuredProcessId = alternateProcessId} : remaining
            }
    alternateGenesis <- checkedIO "check alternate applied fact genesis" (checkHeraldGenesis manifest)
    alternateBootstraps <- selectedBootstraps alternateGenesis
    let replacement = OracleProjection.initialState alternateGenesis alternateBootstraps
    assertFault
      ( HeraldStartupInvariant
          (StartupProcess (configuredProcessEpochId configured))
          OracleAppliedProcessInvariant
      )
      (replaceStartupOracleProjectionState replacement state)
  [] -> assertFailure "fixture has no configured process"

caseSortRegistryContradiction :: Assertion
caseSortRegistryContradiction = do
  state <- validState
  alternateGenesis <- checkedIO "check alternate valid genesis" (checkHeraldGenesis alternateSystemManifest)
  assertFault
    (HeraldStartupInvariant (StartupSort SortDefinitionRole) SortRegistryIdentityInvariant)
    (replaceStartupSortRegistryState (SortRegistry.initialState alternateGenesis) state)

caseSortRegistryProvenanceContradiction :: Assertion
caseSortRegistryProvenanceContradiction = case deploymentPrimordialDefinitions fixtureDeploymentManifest of
  definition : remaining -> do
    state <- validState
    let manifest =
          fixtureDeploymentManifest
            { deploymentPrimordialDefinitions =
                definition {primordialManifestPublicationSequence = nablaSequence 100}
                  : remaining
            }
    alternateGenesis <- checkedIO "check alternate valid provenance" (checkHeraldGenesis manifest)
    assertFault
      (HeraldStartupInvariant (StartupSort SortDefinitionRole) SortRegistryIdentityInvariant)
      (replaceStartupSortRegistryState (SortRegistry.initialState alternateGenesis) state)
  [] -> assertFailure "fixture has no primordial definitions"

caseApplicationContradiction :: Assertion
caseApplicationContradiction = do
  state <- validState
  assertFault
    (HeraldStartupInvariant StartupStatic ResidentApplicationSetInvariant)
    (replaceStartupApplicationState Application.emptyState state)

caseApplicationAccessContradiction :: Assertion
caseApplicationAccessContradiction = do
  (state, bootstrap) <- validStateAndBootstrap
  let process = appliedProcessEpochId bootstrap
  prepared <-
    checkedIO
      "prepare valid empty selected owner in place of genesis access"
      ( Application.prepareApplicationProcessRegistration
          ( ApplicationSession.applicationAttachmentForBootstrap
              (appliedBootstrapManifestId bootstrap)
          )
          process
          Application.emptyState
      )
  assertFault
    (HeraldStartupInvariant (StartupProcess process) ApplicationAccessInvariant)
    (replaceStartupApplicationState (fst (Application.commitApplicationBootstrap prepared)) state)

caseControlledContradiction :: Assertion
caseControlledContradiction = do
  state <- validState
  assertFault
    (HeraldStartupInvariant StartupStatic ResidentControlledSetInvariant)
    (replaceStartupControlledState Controlled.emptyState state)

caseControlledFactContradiction :: Assertion
caseControlledFactContradiction = do
  (state, bootstrap) <- validStateAndBootstrap
  alternateProcessId <- checkedIO "alternate controlled ProcessId" (mkProcessId (fixtureIdentifierBytes 248))
  let process = appliedProcessEpochId bootstrap
      processFact =
        Controlled.processFact
          alternateProcessId
          process
          (appliedProcessResidence bootstrap)
          (appliedProcessAuthority bootstrap)
      roots = fmap (controlledRoot process) (appliedProcessRoots bootstrap)
  prepared <-
    checkedIO
      "prepare conflicting controlled owner"
      (Controlled.prepareControlledBootstrap processFact roots Controlled.emptyState)
  assertFault
    (HeraldStartupInvariant (StartupProcess process) ControlledProcessInvariant)
    (replaceStartupControlledState (Controlled.commitControlledBootstrap prepared) state)

caseGraphContradiction :: Assertion
caseGraphContradiction = do
  state <- validState
  assertFault
    (HeraldStartupInvariant StartupStatic GraphVertexSetInvariant)
    (replaceStartupGraphState Graph.emptyState state)

caseGraphEdgeContradiction :: Assertion
caseGraphEdgeContradiction = do
  (state, bootstrap) <- validStateAndBootstrap
  let writers = [nabla | root <- appliedProcessRoots bootstrap, WriterRoot nabla _ <- [appliedRootRole root]]
      readers = [delta | root <- appliedProcessRoots bootstrap, ReaderRoot delta <- [appliedRootRole root]]
      differentPairs = zip writers (rotateOne readers)
      systemViewDelta =
        deriveSystemViewDeltaId
          (checkedSystemId fixtureCheckedGenesis)
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
      systemViewDeltas = fmap systemViewDelta [minBound .. maxBound]
      writerViewPairs =
        [ (nabla, systemViewDelta (appliedRootCatalogueRole root))
        | root <- appliedProcessRoots bootstrap,
          WriterRoot nabla _ <- [appliedRootRole root]
        ]
  processPrepared <- checkedIO "prepare alternate graph pairing" (Graph.prepareGraphBootstrap differentPairs (appliedProcessEnvironmentHub bootstrap) (drop 1 (map snd (appliedProcessEnvironmentEdges bootstrap))) Graph.emptyState)
  systemViewPrepared <-
    checkedIO
      "add the required system-view routing"
      ( Graph.prepareGraphSystemViews
          systemViewDeltas
          writerViewPairs
          (Graph.commitGraphBootstrap processPrepared)
      )
  assertFault
    (HeraldStartupInvariant StartupStatic GraphVertexSetInvariant)
    (replaceStartupGraphState (Graph.commitGraphSystemViews systemViewPrepared) state)

caseUninducedNeutralVertexContradiction :: Assertion
caseUninducedNeutralVertexContradiction = do
  state <- validState
  object <- uninducedObject
  let graph =
        Graph.commitNeutralVertexInsertion
          (Graph.prepareNeutralVertexInsertion object (startupGraphState state))
  assertFault
    (HeraldStartupInvariant StartupStatic GraphVertexSetInvariant)
    (replaceStartupGraphState graph state)
  where
    uninducedObject :: IO GlobalObjectId
    uninducedObject =
      checkedIO
        "uninduced neutral object"
        (mkGlobalObjectId (fixtureIdentifierBytes 247))

casePlacementContradiction :: Assertion
casePlacementContradiction = do
  state <- validState
  prepared <-
    checkedIO
      "retain only the required system-view placements"
      ( Placement.prepareSystemViewPlacements
          (Placement.systemViewPlacements (startupPlacementState state))
          Placement.emptyState
      )
  assertFault
    (HeraldStartupInvariant StartupStatic PlacementSetInvariant)
    (replaceStartupPlacementState (Placement.commitSystemViewPlacements prepared) state)

casePlacementFactContradiction :: Assertion
casePlacementFactContradiction = do
  (state, bootstrap) <- validStateAndBootstrap
  let placements = zipWith (placementFor bootstrap) [0 :: Int ..] (readerRoots bootstrap)
  firstDelta <- firstReaderDelta bootstrap
  prepared <- checkedIO "prepare conflicting placement owner" (Placement.preparePlacementBootstrap placements Placement.emptyState)
  systemViewPrepared <-
    checkedIO
      "add the required system-view placements"
      ( Placement.prepareSystemViewPlacements
          (Placement.systemViewPlacements (startupPlacementState state))
          (Placement.commitPlacementBootstrap prepared)
      )
  assertFault
    (HeraldStartupInvariant (StartupDelta firstDelta) PlacementFactInvariant)
    (replaceStartupPlacementState (Placement.commitSystemViewPlacements systemViewPrepared) state)

caseStoreContradiction :: Assertion
caseStoreContradiction = do
  state <- validState
  genesisOnlyStore <-
    checkedIO
      "construct genesis-only store owner"
      (Store.initialState fixtureCheckedGenesis)
  assertFault
    (HeraldStartupInvariant StartupStatic StoreSetInvariant)
    (replaceStartupStoreState genesisOnlyStore state)

caseSystemViewStoreContradiction :: Assertion
caseSystemViewStoreContradiction = do
  state <- validState
  remoteGenesis <-
    checkedIO
      "check remote Herald genesis"
      (checkHeraldGenesis (fixtureDeploymentAt fixtureRemoteMember))
  remoteStore <-
    checkedIO
      "construct remote private system views"
      (Store.initialState remoteGenesis)
  assertFault
    (HeraldStartupInvariant StartupStatic SystemViewStoreSetInvariant)
    (replaceStartupStoreState remoteStore state)

casePublicationOwnerContradiction :: Assertion
casePublicationOwnerContradiction = do
  state <- validState
  remoteGenesis <-
    checkedIO
      "check remote Herald genesis"
      (checkHeraldGenesis (fixtureDeploymentAt fixtureRemoteMember))
  assertFault
    (HeraldStartupInvariant StartupStatic PublicationOwnerInvariant)
    ( replaceStartupPublicationState
        (Publication.initialState (checkedLocalHeraldEpoch remoteGenesis))
        state
    )

casePublicationRecordRelationship :: Assertion
casePublicationRecordRelationship = do
  (state, record) <- localWriteFixture
  replacement <-
    checkedIO
      "wrong publication ProcessEpochId"
      (mkProcessEpochId (fixtureIdentifierBytes 246))
  let identifier = Publication.outgoingPublicationId record
      corrupted =
        Publication.replaceOutgoingPublicationSourceProcessForInvariantTest
          identifier
          replacement
          (startupPublicationState state)
  assertFault
    (HeraldStartupInvariant (StartupProcess replacement) PublicationAllocationInvariant)
    (replaceStartupPublicationState corrupted state)

casePublicationStoreReceiptRelationship :: Assertion
casePublicationStoreReceiptRelationship = do
  (state, record) <- localWriteFixture
  destination <- case routeDestinations (Publication.outgoingPublicationRoute record) of
    first : _ -> pure first
    [] -> assertFailure "local write fixture produced an empty route"
  let identifier = Publication.outgoingPublicationId record
      delta = destinationDelta destination
      corrupted =
        Store.removeStorePublicationReceiptForInvariantTest
          delta
          identifier
          (startupStoreState state)
  assertFault
    (HeraldStartupInvariant (StartupDelta delta) PublicationRouteReceiptInvariant)
    (replaceStartupStoreState corrupted state)

caseStoreObservationPublicationRelationship :: Assertion
caseStoreObservationPublicationRelationship = do
  fixture <- regularRetirementInvariantFixture
  alternate <-
    genericInvariantFixture
      (invariantFixtureDescriptor {minimumRetentionMicros = 1})
  let predecessorState = regularRetirementPredecessor fixture
      originalEntry = regularRetirementEntry fixture
      originalIdentifier = SortRegistry.registryEntryPublicationId originalEntry
  originalRecord <-
    checkedMaybeIO
      "original definition publication for Store observation mutation"
      ( Publication.lookupOutgoingPublication
          originalIdentifier
          (startupPublicationState predecessorState)
      )
  alternateEntry <- dynamicEntryForDescriptor (invariantFixtureDescriptor {minimumRetentionMicros = 1}) (state alternate)
  alternateRecord <-
    checkedMaybeIO
      "alternate definition publication for Store observation mutation"
      ( Publication.lookupOutgoingPublication
          (SortRegistry.registryEntryPublicationId alternateEntry)
          (startupPublicationState (state alternate))
      )
  carrierEntry <-
    checkedMaybeIO
      "sort-definition carrier descriptor for Store observation mutation"
      ( SortRegistry.lookupEffectiveSort
          (checkedPublicationSort (Publication.outgoingPublicationChecked originalRecord))
          (startupSortRegistryState predecessorState)
      )
  conflictingPublication <-
    checkedIO
      "same-identity different-payload Store publication"
      ( mkCheckedPublication
          (SortRegistry.registryEntryDescriptor carrierEntry)
          originalIdentifier
          (checkedPublicationValue (Publication.outgoingPublicationChecked alternateRecord))
      )
  assertEqual
    "the mutant deliberately reuses the exact PublicationId"
    originalIdentifier
    (checkedPublicationId conflictingPublication)
  assertBool
    "the mutant payload differs from the authenticated publication"
    ( conflictingPublication
        /= Publication.outgoingPublicationChecked originalRecord
    )
  (slot, retainedObservation) <-
    case [ (retained, observation)
         | retained <- Store.storeSlots (startupStoreState predecessorState),
           observation <- Store.storeSlotApplicationObservations retained,
           Store.retainedStoreObservationPublication observation
             == Publication.outgoingPublicationChecked originalRecord
         ] of
      pair : _ -> pure pair
      [] -> assertFailure "original definition has no retained Store observation"
  conflictingObservation <-
    checkedIO
      "same-identity different-payload retained observation"
      ( Store.retainedStoreObservation
          conflictingPublication
          (Store.retainedStoreObservationIncomingStrength retainedObservation)
          (Store.retainedStoreObservationSortOccurrence retainedObservation)
          (Store.retainedStoreObservationOrigin retainedObservation)
      )
  let delta = Store.storeSlotDelta slot
      corruptedStore =
        Store.insertStoreObservationForInvariantTest
          delta
          conflictingObservation
          (startupStoreState predecessorState)
      corrupted = replaceStartupStoreState corruptedStore predecessorState
  assertFault
    (HeraldStartupInvariant (StartupDelta delta) StoreContentsInvariant)
    corrupted

casePublicationRemoteAssignmentRelationship :: Assertion
casePublicationRemoteAssignmentRelationship = do
  (state, record) <- localWriteFixture
  assertBool
    "the definition owns real mandatory remote assignments"
    (not (Map.null (Publication.outgoingPublicationRemoteBatches record)))
  assertBool
    "the original publication crossed checked dispatch admission"
    (Publication.outgoingPublicationDispatchCertified record)
  (peer, batch) <- case Map.toAscList (Publication.outgoingPublicationRemoteBatches record) of
    first : _ -> pure first
    [] -> assertFailure "remote dispatch fixture has no remote batch"
  let unassigned =
        Publication.insertOutgoingPublicationRemoteBatchForInvariantTest
          (publicationBatchId batch)
          peer
          batch
          (startupPublicationState state)
  assertFault
    (HeraldStartupInvariant StartupStatic PublicationRemoteAssignmentInvariant)
    (replaceStartupPublicationState unassigned state)

caseArchiveEligibleStructuralHoldAuthentication :: Assertion
caseArchiveEligibleStructuralHoldAuthentication = do
  fixture <- HeraldTransitionProperties.successorStructuralHeldFixture
  let predecessor =
        HeraldTransitionProperties.successorStructuralHeldPredecessor fixture
      held = HeraldTransitionProperties.successorStructuralHeldState fixture
      originalItem = HeraldTransitionProperties.successorStructuralHeldItem fixture
      originalPeerPublication = sequencedItemPayload originalItem
      identifier = publicationBatchId (peerPublicationBatch originalPeerPublication)
  originalRecord <-
    checkedMaybeIO
      "archive-eligible incoming publication"
      ( Publication.lookupIncomingPublication
          identifier
          (startupPublicationState held)
      )
  originalStamp <-
    checkedMaybeIO
      "archive-eligible structural stamp"
      (peerPublicationStructuralStamp originalPeerPublication)
  assertEqual
    "the regression fixture is held solely after semantic authentication"
    ( Set.singleton
        ( Publication.SuccessorStructuralBaseDependency
            (Publication.incomingPublicationMembershipGenerationId originalRecord)
        )
    )
    (Publication.incomingPublicationDependencies originalRecord)
  assertBool
    "the structural-only hold is eligible for terminal-source archival"
    (Publication.incomingPublicationSemanticallyAuthenticated originalRecord)
  corruptDigest <-
    checkedIO
      "corrupt structural publication digest"
      (mkStructuralPublicationDigest (fixtureIdentifierBytes 238))
  assertBool
    "the replacement digest differs from the authenticated digest"
    (corruptDigest /= structuralOccurrenceStampPublicationDigest originalStamp)
  corruptStamp <-
    checkedIO
      "structural stamp with a corrupt semantic digest"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceStampOccurrence originalStamp)
          (structuralOccurrenceStampPredecessor originalStamp)
          (structuralOccurrenceStampPublication originalStamp)
          corruptDigest
          (structuralOccurrenceStampCarrierRole originalStamp)
      )
  corruptPeerPublication <-
    checkedIO
      "shape-valid structural publication with a corrupt stamp"
      ( presentedStructuralPeerPublication
          corruptStamp
          (peerPublicationBatch originalPeerPublication)
      )
  let corruptItem =
        sequencedItem
          (sequencedItemDirection originalItem)
          (sequencedItemSequence originalItem)
          (peerPublicationDigest corruptPeerPublication)
          corruptPeerPublication
      corruptLogicalItem =
        sequencedItem
          (sequencedItemDirection corruptItem)
          (sequencedItemSequence corruptItem)
          (sequencedItemDigest corruptItem)
          (PeerLogicalPublication corruptPeerPublication)
  resolution <-
    checkedIO
      "archive-eligible held resolution"
      ( Publication.incomingPublicationResolution
          (Publication.incomingPublicationDependencies originalRecord)
          (Publication.incomingPublicationDestinationOutcomes originalRecord)
      )
  candidate <-
    checkedIO
      "retain corrupt archive-eligible publication candidate"
      ( Publication.prepareIncomingPublicationCandidate
          (Publication.incomingPublicationMembershipGenerationId originalRecord)
          (sequencedItemDirection corruptItem)
          (sequencedItemSequence corruptItem)
          (sequencedItemDigest corruptItem)
          corruptPeerPublication
          (startupPublicationState predecessor)
      )
  preparedPublication <-
    checkedIO
      "retain corrupt archive-eligible publication"
      (Publication.finalizeIncomingPublication (Just resolution) candidate)
  let (publication, corruptRecord) =
        Publication.commitIncomingPublication preparedPublication
  assertBool
    "the corrupt record still crosses the archive-eligibility predicate"
    (Publication.incomingPublicationSemanticallyAuthenticated corruptRecord)
  receiveCandidate <-
    checkedIO
      "retain corrupt archive-eligible stream item candidate"
      ( PeerStream.prepareReceiveCandidate
          corruptLogicalItem
          (startupPeerStreamState predecessor)
      )
  preparedReceive <-
    checkedIO
      "retain corrupt archive-eligible stream item"
      ( PeerStream.finalizeReceive
          (Map.singleton (sequencedItemSequence corruptItem) ReceivedPending)
          receiveCandidate
      )
  let (peerStream, _) = PeerStream.commitReceive preparedReceive
      corrupted =
        replaceStartupPeerStreamState peerStream
          . replaceStartupPublicationState publication
          $ predecessor
  assertFault
    ( HeraldStartupInvariant
        (StartupProcess (publicationBatchSourceProcess (peerPublicationBatch originalPeerPublication)))
        PublicationIncomingInvariant
    )
    corrupted
  let mixedDependencies =
        Set.insert
          ( Publication.ControlIndexDependency
              (publicationBatchControlPrerequisite (peerPublicationBatch originalPeerPublication))
          )
          (Publication.incomingPublicationDependencies originalRecord)
  mixedResolution <-
    checkedIO
      "mixed pre-/post-authentication held resolution"
      ( Publication.incomingPublicationResolution
          mixedDependencies
          (Publication.incomingPublicationDestinationOutcomes originalRecord)
      )
  mixedCandidate <-
    checkedIO
      "retain mixed-phase corrupt publication candidate"
      ( Publication.prepareIncomingPublicationCandidate
          (Publication.incomingPublicationMembershipGenerationId originalRecord)
          (sequencedItemDirection corruptItem)
          (sequencedItemSequence corruptItem)
          (sequencedItemDigest corruptItem)
          corruptPeerPublication
          (startupPublicationState predecessor)
      )
  mixedPreparedPublication <-
    checkedIO
      "retain mixed-phase corrupt publication"
      (Publication.finalizeIncomingPublication (Just mixedResolution) mixedCandidate)
  let (mixedPublication, mixedRecord) =
        Publication.commitIncomingPublication mixedPreparedPublication
      mixedCorrupted =
        replaceStartupPeerStreamState peerStream
          . replaceStartupPublicationState mixedPublication
          $ predecessor
  assertBool
    "a mixed pre-/post-authentication hold is not archive-eligible"
    (not (Publication.incomingPublicationSemanticallyAuthenticated mixedRecord))
  assertFault
    ( HeraldStartupInvariant
        (StartupProcess (publicationBatchSourceProcess (peerPublicationBatch originalPeerPublication)))
        PublicationIncomingInvariant
    )
    mixedCorrupted

caseIncomingAssignmentRelationship :: Assertion
caseIncomingAssignmentRelationship = do
  state <- validState
  direction <-
    checkedIO
      "incoming invariant-test direction"
      ( mkStreamDirection
          (heraldMemberEpoch fixtureRemoteMember)
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
      )
  let identifier = primordialIdentifier
      corrupted =
        Publication.insertIncomingAssignmentOwnerForInvariantTest
          direction
          firstStreamSequence
          identifier
          (startupPublicationState state)
  assertFault
    (HeraldStartupInvariant StartupStatic PublicationIncomingInvariant)
    (replaceStartupPublicationState corrupted state)

caseDependencyWaiterRelationship :: Assertion
caseDependencyWaiterRelationship = do
  state <- validState
  replica <- firstPrimordialReplica
  let dependency =
        Publication.EffectiveSortDependency
          (primordialReplicaSortId replica)
          (primordialReplicaOccurrenceId replica)
      corrupted =
        Publication.insertIncomingDependencyWaiterForInvariantTest
          dependency
          (primordialReplicaPublicationId replica)
          (startupPublicationState state)
  assertFault
    (HeraldStartupInvariant StartupStatic PublicationIncomingInvariant)
    (replaceStartupPublicationState corrupted state)

caseControlWaiterRelationship :: Assertion
caseControlWaiterRelationship = do
  state <- validState
  replica <- firstPrimordialReplica
  let corrupted =
        Publication.insertIncomingDependencyWaiterForInvariantTest
          (Publication.ControlIndexDependency (controlIndex 1))
          (primordialReplicaPublicationId replica)
          (startupPublicationState state)
  assertFault
    (HeraldStartupInvariant StartupStatic PublicationIncomingInvariant)
    (replaceStartupPublicationState corrupted state)

caseDispatchBindingRelationship :: Assertion
caseDispatchBindingRelationship = do
  (state, record) <- localWriteFixture
  remoteDelta <- checkedIO "dispatch fixture DeltaId" (mkDeltaId (fixtureIdentifierBytes 243))
  remoteIncarnation <-
    checkedIO
      "dispatch fixture StoreIncarnationId"
      (mkStoreIncarnationId (fixtureIdentifierBytes 242))
  let publication = Publication.outgoingPublicationChecked record
  remoteBatch <-
    checkedIO
      "dispatch fixture remote batch"
      ( mkPublicationBatch
          (Publication.outgoingPublicationId record)
          (Publication.outgoingPublicationSourceProcess record)
          (checkedPublicationSort publication)
          (Publication.outgoingPublicationOccurrenceId record)
          (checkedPublicationCanonicalValue publication)
          (Publication.outgoingPublicationSourceStrength record)
          (Publication.outgoingPublicationSourceTopologyPrerequisite record)
          (Publication.outgoingPublicationControlPrerequisite record)
          (publicationDestination remoteDelta remoteIncarnation Normal :| [])
      )
  direction <-
    checkedIO
      "outgoing dispatch invariant-test direction"
      ( mkStreamDirection
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
          (heraldMemberEpoch fixtureRemoteMember)
      )
  binding <-
    checkedIO
      "initial dispatch binding generation"
      (mkPeerDispatchBindingGeneration 1)
  replacement <-
    checkedIO
      "stale dispatch binding generation"
      (mkPeerDispatchBindingGeneration 2)
  boundPrepared <-
    checkedIO
      "bind invariant-test dispatch direction"
      (PeerStream.prepareDispatchBinding direction binding (startupPeerStreamState state))
  let (bound, _) = PeerStream.commitDispatchBinding boundPrepared
  enqueuePrepared <-
    checkedIO
      "enqueue invariant-test dispatch item"
      ( PeerStream.prepareEnqueue
          ( Map.singleton
              (heraldMemberEpoch fixtureRemoteMember)
              ( peerLogicalPublicationItem
                  (presentedOrdinaryPeerPublication remoteBatch)
                  :| []
              )
          )
          bound
      )
  ticket <- case maybe [] pure (PeerStream.preparedDispatchBindingTicket boundPrepared) <> PeerStream.preparedEnqueueDispatchTickets enqueuePrepared of
    [one] -> pure one
    tickets -> assertFailure ("expected one dispatch ticket, got " <> show (length tickets))
  let (ready, _) = PeerStream.commitEnqueue enqueuePrepared
  selectedPrepared <-
    checkedIO
      "select invariant-test dispatch attempt"
      (PeerStream.prepareDispatchSelection ticket binding ready)
  let (selected, _) = PeerStream.commitDispatchSelection selectedPrepared
      corrupted =
        PeerStream.replaceDispatchBindingForInvariantTest
          direction
          replacement
          selected
  assertEqual
    "the admitted dispatch owner is valid before mutation"
    (Right ())
    (PeerStream.validatePeerStreamState selected)
  assertEqual
    "the mutation isolates the stale dispatch binding"
    (Left (PeerStream.PeerStreamDispatchInvariant direction))
    (PeerStream.validatePeerStreamState corrupted)
  assertFault
    (HeraldStartupInvariant StartupStatic PeerStreamOwnerInvariant)
    (replaceStartupPeerStreamState corrupted state)

caseMissingWaitRegistration :: Assertion
caseMissingWaitRegistration = do
  (state, bootstrap) <- validStateAndBootstrap
  preparedOpen <-
    checkedIO
      "prepare invariant-fixture session"
      ( Application.prepareApplicationSessionOpen
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
          ( ApplicationSession.applicationAttachmentForBootstrap
              (appliedBootstrapManifestId bootstrap)
          )
          (ApplicationSession.clientNonce 901)
          (startupApplicationState state)
      )
  let (openedState, acceptance) =
        Application.commitApplicationSessionAcceptance preparedOpen
      binding = ApplicationSession.sessionAcceptanceBinding acceptance
  session <- case ApplicationSession.sessionAcceptanceReply acceptance of
    ApplicationSession.SessionOpened openedSession _ _ -> pure openedSession
    reply -> assertFailure ("expected opened session, got " <> show reply)
  candidate <-
    case Application.classifyApplicationRequest
      session
      binding
      (ApplicationRequest.requestId 902)
      (WaitApplication [])
      openedState of
      Right (Application.FirstApplicationRequest first) -> pure first
      Right _ -> assertFailure "expected a first request candidate"
      Left problem -> assertFailure ("classify invariant-fixture wait: " <> show problem)
  let (pendingApplication, _) =
        Application.commitApplicationRequest
          (Application.prepareApplicationWaitAcceptance candidate)
  assertFault
    (HeraldStartupInvariant StartupStatic ApplicationWaitRegistrationInvariant)
    (replaceStartupApplicationState pendingApplication state)

caseStoreFactContradiction :: Assertion
caseStoreFactContradiction = do
  (state, bootstrap) <- validStateAndBootstrap
  alternateIncarnation <-
    checkedIO
      "alternate StoreIncarnationId"
      (mkStoreIncarnationId (fixtureIdentifierBytes 247))
  firstRoot <- case readerRoots bootstrap of
    root : _ -> pure root
    [] -> assertFailure "bootstrap has no reader roots"
  assertEqual "the replaced reader contains the shared catalogue" SortDefinitionRole (appliedRootCatalogueRole firstRoot)
  firstDelta <- firstReaderDelta bootstrap
  let initialStore = Store.passivateStoreForInvariantTest firstDelta (startupStoreState state)
  prepared <-
    checkedIO
      "prepare conflicting store owner"
      (Store.prepareStoreBootstrap [storeSpecFor bootstrap alternateIncarnation 0 firstRoot] initialStore)
  let conflicting = Store.commitStoreBootstrap prepared
  assertEqual "the modified store owner still has valid retained history" (Right ()) (Store.validateStoreHistoryState conflicting)
  assertFault
    (HeraldStartupInvariant (StartupDelta firstDelta) StoreFactInvariant)
    (replaceStartupStoreState conflicting state)

assertFault :: HeraldInvariantFault -> HeraldState -> Assertion
assertFault expected state =
  assertEqual "only the closed Herald invariant fault escapes" (Left expected) (validateHeraldState state)

validState :: IO HeraldState
validState = fst <$> validStateAndBootstrap

localWriteFixture :: IO (HeraldState, Publication.OutgoingPublicationRecord)
localWriteFixture = do
  bootstraps <- selectedBootstraps fixtureCheckedGenesis
  bootstrap <- case checkedInitialBootstraps bootstraps of
    [one] -> pure one
    others -> assertFailure ("expected one local bootstrap, got " <> show (length others))
  (initial, _) <-
    checkedIO
      "initialize local write fixture"
      (initialHerald (monotonicInstant 61) fixtureCheckedGenesis bootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration)
  attachment <-
    checkedMaybeIO
      "local write fixture attachment"
      ( primordialApplicationAttachment
          fixtureCheckedGenesis
          bootstraps
          (appliedBootstrapManifestId bootstrap)
      )
  (opened, openEffects) <-
    checkedIO
      "open local write fixture session"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 62)
              ( ApplicationSessionInput
                  ( OpenApplicationSession
                      (candidateApplicationLane 707)
                      attachment
                      (ApplicationSession.clientNonce 708)
                  )
              )
          )
          initial
      )
  acceptance <- case effectBatchMembers openEffects of
    [SetApplicationConnectionDisposition _ one] -> pure one
    effects -> assertFailure ("unexpected session-open effects: " <> show effects)
  (session, startupAccess) <- case sessionAcceptanceReply acceptance of
    SessionOpened openedSession _ access -> pure (openedSession, access)
    reply -> assertFailure ("expected SessionOpened, got " <> show reply)
  sortAccess <-
    checkedMaybeIO
      "sort-definition access"
      ( find
          ((== Access.SortDefinitionRole) . Access.predefinedAccessRole)
          (conventionalStartupPairs startupAccess)
      )
  (written, _) <-
    checkedIO
      "write local invariant fixture publication"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 63)
              ( ApplicationRequestInput
                  ( CallApplicationRequest
                      (sessionAcceptanceBinding acceptance)
                      session
                      (requestId 709)
                      ( WriteApplication
                          (Access.predefinedWriter sortAccess)
                          ( PublishValue
                              ( ApplicationValue.SortDefinitionValue
                                  (DeclaredSortDefinition invariantFixtureDescriptor Nothing)
                              )
                          )
                      )
                  )
              )
          )
          opened
      )
  assertEqual "valid local write fixture" (Right ()) (validateHeraldState written)
  record <- case Publication.outgoingPublications (startupPublicationState written) of
    [one] -> pure one
    records -> assertFailure ("expected one outgoing publication, got " <> show (length records))
  pure (written, record)

invariantFixtureDescriptor :: ApplicationSortDescriptor
invariantFixtureDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections =
        [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

firstPrimordialReplica :: IO PrimordialDefinitionReplica
firstPrimordialReplica = case checkedPrimordialReplicas fixtureCheckedGenesis of
  first : _ -> pure first
  [] -> assertFailure "checked fixture has no primordial replicas"

primordialIdentifier :: PublicationId
primordialIdentifier = case checkedPrimordialReplicas fixtureCheckedGenesis of
  first : _ -> primordialReplicaPublicationId first
  [] -> error "checked fixture has no primordial replicas"

validStateAndBootstrap :: IO (HeraldState, AppliedProcessBootstrap)
validStateAndBootstrap = do
  bootstraps <- selectedBootstraps fixtureCheckedGenesis
  bootstrap <- case checkedInitialBootstraps bootstraps of
    [one] -> pure one
    others -> assertFailure ("expected one selected bootstrap, got " <> show (length others))
  state <-
    fst
      <$> checkedIO
        "initialize valid Herald"
        ( initialHerald
            (monotonicInstant 51)
            fixtureCheckedGenesis
            bootstraps
            fixtureOracleContacts
            fixtureGeneratorSeed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
  pure (state, bootstrap)

selectedBootstraps :: CheckedHeraldGenesis -> IO CheckedInitialBootstraps
selectedBootstraps genesis = do
  selected <- case fixtureLocalBootstrapIds of
    first : _ -> pure [first]
    [] -> assertFailure "fixture has no local process"
  checkedIO
    "check one selected bootstrap"
    (checkInitialBootstraps genesis (PrimordialProcessManifest selected))

controlledRoot :: ProcessEpochId -> AppliedRoot -> Controlled.RootFact
controlledRoot process root = case appliedRootRole root of
  WriterRoot nabla sequencing ->
    Controlled.writerRootFact
      process
      (appliedRootCatalogueRole root)
      (appliedRootSortId root)
      (appliedRootOccurrenceId root)
      (appliedRootAuthority root)
      (appliedRootControlPrerequisite root)
      nabla
      sequencing
  ReaderRoot delta ->
    Controlled.readerRootFact
      process
      (appliedRootCatalogueRole root)
      (appliedRootSortId root)
      (appliedRootOccurrenceId root)
      (appliedRootAuthority root)
      (appliedRootControlPrerequisite root)
      delta

readerRoots :: AppliedProcessBootstrap -> [AppliedRoot]
readerRoots bootstrap =
  [ root
  | root <- appliedProcessRoots bootstrap,
    ReaderRoot _ <- [appliedRootRole root]
  ]

placementFor :: AppliedProcessBootstrap -> Int -> AppliedRoot -> Placement.LocalPlacement
placementFor bootstrap ordinal root = case (appliedRootRole root, appliedRootPlacement root) of
  (ReaderRoot delta, Just (herald, incarnation)) ->
    Placement.localPlacement
      delta
      (appliedRootSortId root)
      (appliedRootOccurrenceId root)
      (appliedRootObjectId root)
      (appliedProcessEpochId bootstrap)
      herald
      incarnation
      (if ordinal == 0 then controlIndex 1 else appliedRootControlPrerequisite root)
  _ -> error "readerRoots supplied a non-reader or missing placement"

storeSpecFor ::
  AppliedProcessBootstrap ->
  StoreIncarnationId ->
  Int ->
  AppliedRoot ->
  Store.LocalStoreSpec
storeSpecFor bootstrap alternateIncarnation ordinal root = case (appliedRootRole root, appliedRootPlacement root) of
  (ReaderRoot delta, Just (_, incarnation)) ->
    Store.localStoreSpec
      ( Store.ApplicationReader
          (appliedProcessEpochId bootstrap)
          (appliedRootCatalogueRole root)
      )
      delta
      (appliedRootSortId root)
      (appliedRootOccurrenceId root)
      (if ordinal == 0 then alternateIncarnation else incarnation)
  _ -> error "readerRoots supplied a non-reader or missing placement"

firstReaderDelta :: AppliedProcessBootstrap -> IO DeltaId
firstReaderDelta bootstrap = case readerRoots bootstrap of
  root : _ -> case appliedRootRole root of
    ReaderRoot delta -> pure delta
    WriterRoot _ _ -> assertFailure "readerRoots returned a writer"
  [] -> assertFailure "bootstrap has no reader roots"

rotateOne :: [value] -> [value]
rotateOne [] = []
rotateOne (first : remaining) = remaining <> [first]

alternateSystemManifest :: DeploymentManifest
alternateSystemManifest =
  fixtureDeploymentManifest
    { deploymentSystemId = alternateSystem,
      deploymentOracleGenesis =
        (deploymentOracleGenesis fixtureDeploymentManifest)
          { oracleGenesisSystemId = alternateSystem
          }
    }
  where
    alternateSystem =
      checked
        "alternate SystemId"
        (mkSystemId (fixtureIdentifierBytes 250))

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedMaybeIO :: String -> Maybe value -> IO value
checkedMaybeIO context = maybe (assertFailure (context <> ": missing")) pure

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
