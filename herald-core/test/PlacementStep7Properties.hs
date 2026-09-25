{-# LANGUAGE ImportQualifiedPost #-}

module PlacementStep7Properties
  ( tests,
  )
where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word8)
import Eclips.Domain.Alignment
  ( physicalPlacementRevisionMembershipGenerationId,
  )
import Eclips.Domain.Graph (VertexId (DeltaVertex))
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    SystemId,
    controlIndex,
    controlIndexWord64,
    genesisAuthorityEpoch,
    globalObjectIdFromDeltaId,
    globalUniqueIdFromGlobalObjectId,
    mkDeltaId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkStoreIncarnationId,
    mkStructuralSequence,
    nablaSequence,
    publicationId,
    sortIdBytes,
    structuralOccurrenceId,
  )
import Eclips.Domain.Label
  ( ReleasedLabelState,
    releasedDeleted,
    releasedLabel,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Publication
  ( checkedPublicationId,
    mkCheckedPublication,
  )
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (DeltaCarrier))
import Eclips.Domain.Sort.Profile
  ( allPredefinedSortRoles,
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profileSortFor,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRootRole (ReaderRoot),
    HeraldMember (..),
    PredefinedSortRole (..),
    appliedProcessEnvironmentObjects,
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootControlPrerequisite,
    appliedRootObjectId,
    appliedRootOccurrenceId,
    appliedRootPlacement,
    appliedRootRole,
    appliedRootSortId,
    checkedInitialTopologyVertices,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    predefinedOccurrenceFor,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    emptyStructuralVersionVector,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    labelReleaseCause,
    processEndCause,
  )
import Eclips.Domain.Value
  ( FieldName,
    LabelOwner (ProcessLabel, VoidLabel),
    Value,
    bytesValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    recordValue,
  )
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.Genesis.Internal
  ( CheckedInitialBootstraps,
    PrimordialProcessManifest (..),
    checkInitialBootstraps,
    checkedActiveHeralds,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedPredefinedOccurrenceSet,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.PeerPublication
  ( mkStructuralOccurrenceStamp,
    mkStructuralPublicationDigest,
  )
import Eclips.Herald.Placement qualified as Placement
import Eclips.Herald.Placement.State qualified as PlacementState
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    sortOccurrence,
    sortOccurrenceDefinition,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.PeerPlacement qualified as PeerPlacement
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck (Property, testProperty, (===))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "sequenced placement projection"
    [ testProperty "diagnostic modes preserve placement histories, replay, protocol rejection and retirement" propDiagnosticModes,
      testCase "the first complete snapshot is sequence one even when empty" caseLocalSnapshot,
      testCase "reconnect replays only the retained cut-qualified snapshot" caseCutQualifiedReplay,
      testCase "local acknowledgement is monotone and bounded by advertisement" caseAcknowledgement,
      testCase "application and private-view routes have distinct payloads" caseTypedRouteArms,
      testCase "private system-view routes derive every identity" caseSystemViewRouteAdmission,
      testCase "remote route admission checks authority but accepts a fresh incarnation" caseRemoteRouteAdmission,
      testCase "genesis handoff retains both exact old and new route coordinates" caseGenesisHandoffRouteHistory,
      testProperty "preparation placement changes follow exact local slot lifetime" propPreparationPlacementChanges,
      testCase "factored route admission preserves history through passive, ended and deleted states" caseFactoredRouteLifetimes,
      testCase "a later occurrence is not hidden by the immutable genesis shortcut" caseGenesisShortcutFallsThrough,
      testCase "old dynamic routes remain admissible after controller handoff" caseDynamicHandoffRouteHistory,
      testCase "unchanged structural control refresh preserves preparation scheduling" caseNoopStructuralControlRefresh,
      testProperty "snapshot normalization ignores presentation order" propSnapshotNormalization,
      testCase "a newer full snapshot atomically replaces an older projection" caseSnapshotReplacement,
      testCase "alignment plan and loss notifications distinguish live and historical snapshot changes" caseAlignmentPlacementChanges,
      testCase "same-sequence conflict faults while exact repeat is idempotent" caseSequenceConflict,
      testCase "remote owner projections remain isolated" caseOwnerIsolation,
      testCase "remote retirement removes only live projection and permanently tombstones input" caseRemoteRetirement,
      testCase "exact revision vectors reconstruct retained local and remote cuts" caseRevisionVectorHistory,
      testProperty "historical snapshot retention is passive, commutative, and idempotent" propHistoricalPlacementRetention,
      testCase "historical snapshots cannot reactivate a retired owner" caseRetiredHistoricalPlacement,
      testCase "historical local snapshots only corroborate exact retained coordinates" caseLocalHistoricalPlacement,
      testCase "live snapshots preserve previously retained historical coordinates" caseHistoricalPlacementAdmission
    ]

propDiagnosticModes :: [Bool] -> Property
propDiagnosticModes nonemptyRevisions = QC.ioProperty $ do
  enabled <- runMode DiagnosticChecksEnabled
  disabled <- runMode DiagnosticChecksDisabled
  pure (enabled === disabled)
  where
    local = checkedLocalHeraldEpoch fixtureCheckedGenesis
    remote = heraldMemberEpoch fixtureRemoteMember
    localRoute = systemViewRoute local SortDefinitionRole
    remoteRoute = systemViewRoute remote SortDefinitionRole
    revisions = iterate Placement.nextPlacementSequence Placement.firstPlacementSequence
    runMode diagnostics = do
      first <- checked "initial diagnostic snapshot" (PlacementState.prepareLocalSnapshotWithDiagnostics diagnostics (PlacementState.initialState local))
      let (initial, initialResult) = PlacementState.commitLocalSnapshot first
      (beforeRetirement, history) <- foldM (advance diagnostics) (initial, []) (zip revisions (False : nonemptyRevisions))
      retirement <- checked "diagnostic placement retirement" (PlacementState.prepareRemotePlacementRetirementWithDiagnostics diagnostics remote beforeRetirement)
      let (retired, result) = PlacementState.commitRemotePlacementRetirement retirement
      repeated <- checked "duplicate diagnostic retirement" (PlacementState.prepareRemotePlacementRetirementWithDiagnostics diagnostics remote retired)
      let (afterRepeat, repeatedResult) = PlacementState.commitRemotePlacementRetirement repeated
      assertEqual "retirement preserves the placement owner invariant" (Right ()) (PlacementState.validatePlacementState retired)
      assertEqual
        "retired input rejection does not depend on diagnostic scans"
        (Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementRetiredRemoteOwner remote)))
        (PlacementState.commitRemotePlacement <$> PlacementState.prepareRemotePlacementWithDiagnostics diagnostics (Placement.FullPlacementSnapshot (snapshotAt remote Placement.firstPlacementSequence [])) retired)
      pure (initialResult, history, retired, result, afterRepeat, repeatedResult)
    advance diagnostics (state, history) (revision, nonempty) = do
      offered <- checked "qualified diagnostic snapshot" (PlacementState.prepareLocalSnapshotForRoutesWithDiagnostics diagnostics [localRoute | nonempty] state)
      let (qualified, offeredResult) = PlacementState.commitLocalSnapshot offered
          localRevision = Placement.placementSnapshotSequence (PlacementState.localSnapshotMessage offeredResult)
          acknowledgement = Placement.placementAcknowledgement local localRevision
          update = Placement.FullPlacementSnapshot (snapshotAt remote revision [remoteRoute | nonempty])
          conflict = Placement.FullPlacementSnapshot (snapshotAt remote revision [remoteRoute | not nonempty])
      retained <- checked "retained diagnostic snapshot" (PlacementState.prepareRetainedLocalSnapshotWithDiagnostics diagnostics qualified)
      let (reoffered, retainedResult) = PlacementState.commitLocalSnapshot retained
      acknowledged <- checked "diagnostic acknowledgement" (PlacementState.preparePlacementAcknowledgementWithDiagnostics diagnostics remote acknowledgement reoffered)
      let (afterAck, ackResult) = PlacementState.commitPlacementAcknowledgement acknowledged
      ackRepeated <- checked "diagnostic duplicate acknowledgement" (PlacementState.preparePlacementAcknowledgementWithDiagnostics diagnostics remote acknowledgement afterAck)
      let (afterAckRepeat, repeatedAckResult) = PlacementState.commitPlacementAcknowledgement ackRepeated
      received <- checked "diagnostic remote snapshot" (PlacementState.prepareRemotePlacementWithDiagnostics diagnostics update afterAckRepeat)
      let (afterRemote, remoteResult) = PlacementState.commitRemotePlacement received
      replay <- checked "diagnostic duplicate remote snapshot" (PlacementState.prepareRemotePlacementWithDiagnostics diagnostics update afterRemote)
      let (successor, replayResult) = PlacementState.commitRemotePlacement replay
      assertEqual "every resulting owner validates independently of its mode" (Right ()) (PlacementState.validatePlacementState successor)
      assertEqual
        "same-revision disagreement remains a protocol rejection"
        (Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementSequenceConflict remote revision)))
        (PlacementState.commitRemotePlacement <$> PlacementState.prepareRemotePlacementWithDiagnostics diagnostics conflict successor)
      let ahead = Placement.nextPlacementSequence localRevision
      assertEqual
        "acknowledgement bounds remain unconditional"
        (Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementAcknowledgementAhead ahead localRevision)))
        (PlacementState.commitPlacementAcknowledgement <$> PlacementState.preparePlacementAcknowledgementWithDiagnostics diagnostics remote (Placement.placementAcknowledgement local ahead) successor)
      pure (successor, (offeredResult, retainedResult, ackResult, repeatedAckResult, remoteResult, replayResult) : history)

caseLocalSnapshot :: Assertion
caseLocalSnapshot = do
  let owner = checkedLocalHeraldEpoch fixtureCheckedGenesis
      initial = PlacementState.initialState owner
  firstPrepared <- checked "first local snapshot" (PlacementState.prepareLocalSnapshot initial)
  let (afterFirst, firstResult) = PlacementState.commitLocalSnapshot firstPrepared
      firstSnapshot = PlacementState.localSnapshotMessage firstResult
  assertEqual "first disposition" PlacementState.InitialLocalSnapshot (PlacementState.localSnapshotDisposition firstResult)
  assertEqual "first sequence" 1 (sequenceValue (Placement.placementSnapshotSequence firstSnapshot))
  assertEqual "empty route set" [] (Placement.placementSnapshotRoutes firstSnapshot)

  retryPrepared <- checked "snapshot retry" (PlacementState.prepareLocalSnapshot afterFirst)
  let (afterRetry, retryResult) = PlacementState.commitLocalSnapshot retryPrepared
  assertEqual "retry disposition" PlacementState.ReofferedLocalSnapshot (PlacementState.localSnapshotDisposition retryResult)
  assertEqual "retry reuses exact snapshot" firstSnapshot (PlacementState.localSnapshotMessage retryResult)
  assertEqual "retry is state-identical" afterFirst afterRetry

  let route = systemViewRoute owner SortDefinitionRole
      placement = systemViewPlacement owner SortDefinitionRole
  installation <- checked "changed local placement" (PlacementState.prepareSystemViewPlacements [placement] afterFirst)
  let changed = PlacementState.commitSystemViewPlacements installation
  changedPrepared <- checked "advanced local snapshot" (PlacementState.prepareLocalSnapshot changed)
  let (afterChanged, changedResult) = PlacementState.commitLocalSnapshot changedPrepared
      changedSnapshot = PlacementState.localSnapshotMessage changedResult
  assertEqual "changed route advances" PlacementState.AdvancedLocalSnapshot (PlacementState.localSnapshotDisposition changedResult)
  assertEqual "changed sequence" 2 (sequenceValue (Placement.placementSnapshotSequence changedSnapshot))
  assertEqual "changed snapshot has the exact route" [route] (Placement.placementSnapshotRoutes changedSnapshot)
  assertEqual "private-view routes and snapshot bookkeeping do not wake application preparation" Set.empty (fst (PlacementState.takePreparationPlacementChanges afterChanged))
  assertEqual
    "the empty revision-one projection remains retained"
    (Just [])
    (PlacementState.placementRoutesAtRevision owner Placement.firstPlacementSequence afterChanged)
  assertEqual
    "the revision-two projection remains retained"
    (Just [route])
    ( PlacementState.placementRoutesAtRevision
        owner
        (Placement.nextPlacementSequence Placement.firstPlacementSequence)
        afterChanged
    )

caseCutQualifiedReplay :: Assertion
caseCutQualifiedReplay = do
  let owner = checkedLocalHeraldEpoch fixtureCheckedGenesis
      routeA = systemViewRoute owner SortDefinitionRole
      routeB = systemViewRoute owner EdgeRole
      routeC = systemViewRoute owner NablaRole
      initial = PlacementState.initialState owner
  installedA <-
    checked
      "install initial fixed route"
      ( PlacementState.prepareSystemViewPlacements
          [systemViewPlacement owner SortDefinitionRole]
          initial
      )
  initialSnapshot <-
    checked
      "initial advertised cut"
      ( PlacementState.prepareLocalSnapshot
          (PlacementState.commitSystemViewPlacements installedA)
      )
  let (advertisedA, _) = PlacementState.commitLocalSnapshot initialSnapshot
  installedB <-
    checked
      "install live-ahead fixed route"
      ( PlacementState.prepareSystemViewPlacements
          [systemViewPlacement owner EdgeRole]
          advertisedA
      )
  let liveAheadB = PlacementState.commitSystemViewPlacements installedB
  reconnect <-
    checked
      "retained reconnect snapshot"
      (PlacementState.prepareRetainedLocalSnapshot liveAheadB)
  let (afterReconnect, reconnectResult) = PlacementState.commitLocalSnapshot reconnect
      reconnectSnapshot = PlacementState.localSnapshotMessage reconnectResult
  assertEqual "reconnect keeps revision one" Placement.firstPlacementSequence (Placement.placementSnapshotSequence reconnectSnapshot)
  assertEqual "reconnect excludes live-ahead B" [routeA] (Placement.placementSnapshotRoutes reconnectSnapshot)
  assertEqual "reconnect is state-identical" liveAheadB afterReconnect

  qualified <-
    checked
      "qualify the exact installed-cut routes"
      (PlacementState.prepareLocalSnapshotForRoutes [routeA, routeB] afterReconnect)
  let (advertisedAB, qualifiedResult) = PlacementState.commitLocalSnapshot qualified
      qualifiedSnapshot = PlacementState.localSnapshotMessage qualifiedResult
  assertEqual "cut qualification advances" PlacementState.AdvancedLocalSnapshot (PlacementState.localSnapshotDisposition qualifiedResult)
  assertEqual "cut qualification advertises the exact route set" [routeA, routeB] (Placement.placementSnapshotRoutes qualifiedSnapshot)

  installedC <-
    checked
      "install another live-ahead route"
      ( PlacementState.prepareSystemViewPlacements
          [systemViewPlacement owner NablaRole]
          advertisedAB
      )
  let liveAheadC = PlacementState.commitSystemViewPlacements installedC
  replay <-
    checked
      "retained reconnect snapshot"
      (PlacementState.prepareRetainedLocalSnapshot liveAheadC)
  let (afterReplay, replayResult) = PlacementState.commitLocalSnapshot replay
      replaySnapshot = PlacementState.localSnapshotMessage replayResult
  assertEqual "replay retains the qualified revision" (Placement.placementSnapshotSequence qualifiedSnapshot) (Placement.placementSnapshotSequence replaySnapshot)
  assertEqual "replay excludes live-ahead C" [routeA, routeB] (Placement.placementSnapshotRoutes replaySnapshot)
  assertEqual "replay is state-identical" liveAheadC afterReplay

  qualifiedC <-
    checked
      "qualify the successive installed-cut routes"
      ( PlacementState.prepareLocalSnapshotForRoutes
          [routeA, routeB, routeC]
          afterReplay
      )
  let (_, qualifiedCResult) = PlacementState.commitLocalSnapshot qualifiedC
      qualifiedCSnapshot = PlacementState.localSnapshotMessage qualifiedCResult
  assertEqual
    "a successive cut advances exactly once"
    (Placement.nextPlacementSequence (Placement.placementSnapshotSequence qualifiedSnapshot))
    (Placement.placementSnapshotSequence qualifiedCSnapshot)
  assertEqual
    "the successive cut exposes C only when qualified"
    (sortOn Placement.deltaRouteDelta [routeA, routeB, routeC])
    (Placement.placementSnapshotRoutes qualifiedCSnapshot)

caseAcknowledgement :: Assertion
caseAcknowledgement = do
  let owner = checkedLocalHeraldEpoch fixtureCheckedGenesis
      peer = heraldMemberEpoch fixtureRemoteMember
      initial = PlacementState.initialState owner
  offered <- checked "local snapshot" (PlacementState.prepareLocalSnapshot initial)
  let (advertised, _) = PlacementState.commitLocalSnapshot offered
      acknowledgement = Placement.placementAcknowledgement owner Placement.firstPlacementSequence
  prepared <- checked "placement acknowledgement" (PlacementState.preparePlacementAcknowledgement peer acknowledgement advertised)
  let (afterAck, disposition) = PlacementState.commitPlacementAcknowledgement prepared
  assertEqual "first acknowledgement advances" PlacementState.PlacementAckAdvanced disposition
  assertEqual "acknowledged prefix" (Just Placement.firstPlacementSequence) (PlacementState.placementAcknowledgedThrough peer afterAck)
  retry <- checked "placement acknowledgement retry" (PlacementState.preparePlacementAcknowledgement peer acknowledgement afterAck)
  let (afterRetry, retryDisposition) = PlacementState.commitPlacementAcknowledgement retry
  assertEqual "lower or equal acknowledgement is stale" PlacementState.PlacementAckStale retryDisposition
  assertEqual "stale acknowledgement is state-identical" afterAck afterRetry
  let ahead = Placement.placementAcknowledgement owner (Placement.nextPlacementSequence Placement.firstPlacementSequence)
  case PlacementState.preparePlacementAcknowledgement peer ahead afterAck of
    Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementAcknowledgementAhead _ _)) -> pure ()
    Left problem -> assertFailure ("unexpected ahead-ack problem: " <> show problem)
    Right _ -> assertFailure "acknowledgement ahead of the advertisement was accepted"

caseTypedRouteArms :: Assertion
caseTypedRouteArms = do
  delta <- checked "application DeltaId" (mkDeltaId (fixtureIdentifierBytes 213))
  sortId <- checked "application SortId" (mkSortId (fixtureIdentifierBytes 214))
  occurrence <- checked "application occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 215))
  controller <- checked "application controller" (mkGlobalObjectId (fixtureIdentifierBytes 216))
  process <- checked "application process epoch" (mkProcessEpochId (fixtureIdentifierBytes 217))
  incarnation <- checked "application incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 218))
  let prerequisite = controlIndex 19
      route =
        Placement.applicationDeltaRoute
          delta
          sortId
          occurrence
          controller
          process
          incarnation
          prerequisite
  case Placement.viewDeltaRoute route of
    Placement.ApplicationDeltaRouteView
      observedDelta
      observedSort
      observedOccurrence
      observedController
      observedProcess
      observedIncarnation
      observedPrerequisite -> do
        assertEqual "application delta" delta observedDelta
        assertEqual "application sort" sortId observedSort
        assertEqual "application occurrence" occurrence observedOccurrence
        assertEqual "application controller" controller observedController
        assertEqual "application process" process observedProcess
        assertEqual "application incarnation" incarnation observedIncarnation
        assertEqual "application prerequisite" prerequisite observedPrerequisite
    Placement.PrivateSystemViewDeltaRouteView {} ->
      assertFailure "application route became a private system-view route"

caseSystemViewRouteAdmission :: Assertion
caseSystemViewRouteAdmission = do
  let system = checkedSystemId fixtureCheckedGenesis
      owner = heraldMemberEpoch fixtureRemoteMember
      route = systemViewRoute owner EdgeRole
  assertEqual
    "all private fields derive from system, owner, and role"
    (Right ())
    (Placement.validatePrivateSystemViewDeltaRoute system owner route)
  fakeDelta <- checked "other DeltaId" (mkDeltaId (fixtureIdentifierBytes 210))
  let malformed =
        Placement.privateSystemViewDeltaRoute
          EdgeRole
          fakeDelta
          (profileSortFor EdgeRole)
          (deriveSortDefinitionOccurrenceId system (profileSortFor EdgeRole) Genesis)
          (deriveSystemViewStoreIncarnationId system owner EdgeRole)
  assertEqual
    "a supplied private delta cannot be fabricated"
    (Left Placement.PrivateSystemViewDeltaMismatch)
    (Placement.validatePrivateSystemViewDeltaRoute system owner malformed)

caseRemoteRouteAdmission :: Assertion
caseRemoteRouteAdmission = do
  members <-
    maybe
      (assertFailure "checked genesis has no active Herald" >> error "unreachable")
      pure
      (NonEmpty.nonEmpty (fmap heraldMemberEpoch (checkedActiveHeralds fixtureCheckedGenesis)))
  let membership = placementMembership members
      emptyVector = emptyStructuralVersionVector membership
  progress <-
    checked
      "initial structural progress"
      ( GraphProgress.initialStructuralProgressState
          system
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
          membership
          (checkedInitialProjectionDigest checkedProjection)
          (Reconciliation.emptyStructuralAppliedState emptyVector)
      )
  let owner = heraldMemberEpoch fixtureRemoteMember
      bootstraps = checkedInitialBootstraps checkedProjection
      indexedBootstraps = [(appliedProcessEpochId bootstrap, bootstrap) | bootstrap <- bootstraps]
      graph = Graph.initialTopologyState (checkedInitialTopologyProjection checkedProjection)
      remoteBootstrap = case filter ((== owner) . appliedProcessResidence) bootstraps of
        [bootstrap] -> bootstrap
        actual -> error ("expected one remote bootstrap, got " <> show (length actual))
      (delta, root) = case [ (observed, candidate)
                           | candidate <- appliedProcessRoots remoteBootstrap,
                             ReaderRoot observed <- [appliedRootRole candidate]
                           ] of
        first : _ -> first
        [] -> error "remote bootstrap has no reader root"
      (_, startupIncarnation) = case appliedRootPlacement root of
        Just placement -> placement
        Nothing -> error "remote reader has no checked placement"
      validApplication incarnation controller =
        Placement.applicationDeltaRoute
          delta
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          controller
          (appliedProcessEpochId remoteBootstrap)
          incarnation
          (appliedRootControlPrerequisite root)
      valid = validApplication startupIncarnation (appliedRootObjectId root)
  assertBool
    "the exact checked application route is admitted"
    (PeerPlacement.remotePlacementRouteIsValid system owner indexedBootstraps graph progress valid)

  freshIncarnation <-
    checked "fresh host-authoritative incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 219))
  let reactivated = validApplication freshIncarnation (appliedRootObjectId root)
  assertBool
    "checked residence does not freeze the host-authoritative incarnation"
    (PeerPlacement.remotePlacementRouteIsValid system owner indexedBootstraps graph progress reactivated)

  wrongController <-
    checked "wrong application controller" (mkGlobalObjectId (fixtureIdentifierBytes 220))
  assertBool
    "a fabricated application controller is rejected"
    ( not
        ( PeerPlacement.remotePlacementRouteIsValid
            system
            owner
            indexedBootstraps
            graph
            progress
            (validApplication startupIncarnation wrongController)
        )
    )

  wrongSort <- checked "wrong private-view sort" (mkSortId (fixtureIdentifierBytes 221))
  let validPrivate = systemViewRoute owner SortDefinitionRole
      malformedPrivate =
        Placement.privateSystemViewDeltaRoute
          SortDefinitionRole
          (Placement.deltaRouteDelta validPrivate)
          wrongSort
          (Placement.deltaRouteOccurrenceId validPrivate)
          (Placement.deltaRouteStoreIncarnation validPrivate)
  assertBool
    "a private-view route with a checked vertex but fabricated sort is rejected"
    ( not
        ( PeerPlacement.remotePlacementRouteIsValid
            system
            owner
            indexedBootstraps
            graph
            progress
            malformedPrivate
        )
    )
  forM_ [indexedBootstraps, reverse indexedBootstraps, indexedBootstraps <> indexedBootstraps] $ \suppliedBootstraps ->
    assertRemoteRouteAdmission system owner suppliedBootstraps graph progress [valid, reactivated, validApplication startupIncarnation wrongController, validPrivate, malformedPrivate]
  where
    system = checkedSystemId fixtureCheckedGenesis
    checkedProjection =
      checkedValue
        "global checked bootstrap projection"
        ( checkInitialBootstraps
            fixtureCheckedGenesis
            ( PrimordialProcessManifest
                (fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId])
            )
        )

data GenesisPlacementFixture = GenesisPlacementFixture
  { fixtureSystem :: SystemId,
    fixtureLocalOwner :: HeraldEpoch,
    fixtureRemoteOwner :: HeraldEpoch,
    fixtureIndexedBootstraps :: [(ProcessEpochId, AppliedProcessBootstrap)],
    fixtureViews :: Reconciliation.ReconciliationViews,
    fixtureGraph :: Graph.State,
    fixtureProgress :: GraphProgress.StructuralProgressState,
    fixtureDelta :: DeltaId,
    fixtureObject :: GlobalObjectId,
    fixtureSort :: SortId,
    fixtureOccurrence :: SortDefinitionOccurrenceId,
    fixtureLocalProcess :: ProcessEpochId,
    fixtureRemoteProcess :: ProcessEpochId,
    fixtureIncarnation :: StoreIncarnationId,
    fixturePrerequisite :: ControlIndex
  }

-- Generate same-controller releases and cross-Herald handoffs. The expected
-- readiness change follows local slot presence, not wire snapshot revision or
-- the release's advancing control prerequisite.
propPreparationPlacementChanges :: [Bool] -> Property
propPreparationPlacementChanges rawOperations = QC.ioProperty $ do
  fixture <- genesisPlacementFixture
  let delta = fixtureDelta fixture
      initialPlacement =
        PlacementState.localPlacement
          delta
          (fixtureSort fixture)
          (fixtureOccurrence fixture)
          (fixtureObject fixture)
          (fixtureLocalProcess fixture)
          (fixtureLocalOwner fixture)
          (fixtureIncarnation fixture)
          (fixturePrerequisite fixture)
  bootstrap <-
    checked
      "preparation placement bootstrap"
      (PlacementState.preparePlacementBootstrap [initialPlacement] (PlacementState.initialState (fixtureLocalOwner fixture)))
  let installed = PlacementState.commitPlacementBootstrap bootstrap
      initial = PlacementState.clearPreparationPlacementChanges installed
      operations = zip [1 :: Int ..] ([True, False, False, True, True] <> take 12 rawOperations)
      step (state, reconciliation, active, pending) (ordinal, desiredLocal) = do
        let index = controlIndex (fromIntegral ordinal + 20)
            labelProcess = if desiredLocal then fixtureLocalProcess fixture else fixtureRemoteProcess fixture
            fresh =
              if desiredLocal && not active
                then
                  Just
                    ( Reconciliation.freshStoreIncarnationEvidence
                        Reconciliation.GeneratedStoreIncarnation
                        delta
                        (checkedValue "preparation fresh incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes (fromIntegral ordinal + 180))))
                    )
                else Nothing
            cause =
              checkedValue
                "preparation placement release cause"
                (labelReleaseCause (checkedValue "preparation placement decision" (mkLabelDecisionId (fixtureIdentifierBytes (fromIntegral ordinal + 210)))) index)
        ready <- case Reconciliation.prepareStructuralLabelControlRefresh
          (fixtureViews fixture)
          cause
          (fixtureObject fixture)
          (releasedLabel (ProcessLabel labelProcess, fromIntegral ordinal))
          fresh
          reconciliation of
          Left problem -> assertFailure ("preparation placement reconciliation: " <> show problem)
          Right (Reconciliation.StructuralHeld dependencies) -> assertFailure ("preparation placement held: " <> show dependencies)
          Right (Reconciliation.StructuralReady result) -> pure result
        let apply predecessor = do
              prepared <-
                checked
                  "preparation placement patch"
                  (PlacementState.prepareStructuralPlacementPatch index (Reconciliation.preparedStructuralPlacementPatch ready) predecessor)
              pure (fst (PlacementState.commitStructuralPlacementPatch prepared))
            expectedStep = if desiredLocal /= active then Set.singleton delta else Set.empty
            expectedTotal = Set.union pending expectedStep
        successor <- apply state
        isolated <- apply (PlacementState.clearPreparationPlacementChanges state)
        assertEqual "changed physical coordinates wake; advancing only authority control does not" expectedStep (fst (PlacementState.takePreparationPlacementChanges isolated))
        assertEqual "unconsumed slot changes coalesce" expectedTotal (fst (PlacementState.takePreparationPlacementChanges successor))
        assertEqual "the independent local-presence model agrees with the owner" desiredLocal (PlacementState.lookupLocalPlacement delta successor /= Nothing)
        let (_, consumed) = PlacementState.takePreparationPlacementChanges successor
        assertEqual "taking agrees with clear and is idempotent" (PlacementState.clearPreparationPlacementChanges successor) consumed
        assertEqual "taking twice is empty" Set.empty (fst (PlacementState.takePreparationPlacementChanges consumed))
        assertFactoredRouteAdmission delta (Reconciliation.preparedStructuralSuccessor ready)
        pure (successor, Reconciliation.preparedStructuralSuccessor ready, desiredLocal, expectedTotal)
  assertEqual "fresh application slot wakes its Delta" (Set.singleton delta) (fst (PlacementState.takePreparationPlacementChanges installed))
  _ <- foldM step (initial, GraphProgress.structuralProgressReconciliation (fixtureProgress fixture), True, Set.empty) operations
  pure True

caseGenesisHandoffRouteHistory :: Assertion
caseGenesisHandoffRouteHistory = do
  fixture <- genesisPlacementFixture
  let releaseIndex = controlIndex 7
      releaseCause =
        checkedValue
          "genesis handoff cause"
          ( labelReleaseCause
              (checkedValue "genesis handoff decision" (mkLabelDecisionId (fixtureIdentifierBytes 222)))
              releaseIndex
          )
  withRelease <- advanceControlTo "genesis handoff" releaseIndex (fixtureProgress fixture)
  (_, afterHandoff) <-
    applyStructuralControlRefresh
      "genesis handoff"
      (fixtureViews fixture)
      releaseCause
      (fixtureObject fixture)
      (releasedLabel ((ProcessLabel (fixtureRemoteProcess fixture), 1)))
      Nothing
      (fixtureGraph fixture)
      withRelease
  freshRemoteIncarnation <-
    checked "fresh remote handoff incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 223))
  let route process incarnation prerequisite =
        Placement.applicationDeltaRoute
          (fixtureDelta fixture)
          (fixtureSort fixture)
          (fixtureOccurrence fixture)
          (fixtureObject fixture)
          process
          incarnation
          prerequisite
      oldRoute =
        route
          (fixtureLocalProcess fixture)
          (fixtureIncarnation fixture)
          (fixturePrerequisite fixture)
      currentRoute =
        route
          (fixtureRemoteProcess fixture)
          freshRemoteIncarnation
          releaseIndex
      admitted owner candidate =
        PeerPlacement.remotePlacementRouteIsValid
          (fixtureSystem fixture)
          owner
          (fixtureIndexedBootstraps fixture)
          Graph.emptyState
          afterHandoff
          candidate
  assertBool
    "the original genesis owner remains an exact historical route coordinate"
    (admitted (fixtureLocalOwner fixture) oldRoute)
  assertBool
    "the handoff owner is admitted from the retained controller overlay"
    (admitted (fixtureRemoteOwner fixture) currentRoute)

  wrongController <- checked "wrong handoff controller" (mkGlobalObjectId (fixtureIdentifierBytes 224))
  wrongProcess <- checked "wrong handoff process" (mkProcessEpochId (fixtureIdentifierBytes 225))
  wrongSort <- checked "wrong handoff sort" (mkSortId (fixtureIdentifierBytes 226))
  wrongOccurrence <-
    checked "wrong handoff occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 227))
  let mutations =
        [ ( "controller",
            Placement.applicationDeltaRoute
              (fixtureDelta fixture)
              (fixtureSort fixture)
              (fixtureOccurrence fixture)
              wrongController
              (fixtureRemoteProcess fixture)
              freshRemoteIncarnation
              releaseIndex
          ),
          ( "process",
            Placement.applicationDeltaRoute
              (fixtureDelta fixture)
              (fixtureSort fixture)
              (fixtureOccurrence fixture)
              (fixtureObject fixture)
              wrongProcess
              freshRemoteIncarnation
              releaseIndex
          ),
          ( "sort",
            Placement.applicationDeltaRoute
              (fixtureDelta fixture)
              wrongSort
              (fixtureOccurrence fixture)
              (fixtureObject fixture)
              (fixtureRemoteProcess fixture)
              freshRemoteIncarnation
              releaseIndex
          ),
          ( "occurrence",
            Placement.applicationDeltaRoute
              (fixtureDelta fixture)
              (fixtureSort fixture)
              wrongOccurrence
              (fixtureObject fixture)
              (fixtureRemoteProcess fixture)
              freshRemoteIncarnation
              releaseIndex
          ),
          ( "control prerequisite",
            route
              (fixtureRemoteProcess fixture)
              freshRemoteIncarnation
              (controlIndex 8)
          )
        ]
  mapM_
    ( \(field, mutated) ->
        assertBool
          ("the retained handoff coordinate binds " <> field)
          (not (admitted (fixtureRemoteOwner fixture) mutated))
    )
    mutations
  assertBool
    "the retained handoff coordinate binds its owning Herald"
    (not (admitted (fixtureLocalOwner fixture) currentRoute))
  forM_ [fixtureLocalOwner fixture, fixtureRemoteOwner fixture] $ \owner ->
    assertRemoteRouteAdmission (fixtureSystem fixture) owner (fixtureIndexedBootstraps fixture) Graph.emptyState afterHandoff (oldRoute : currentRoute : map snd mutations)

caseGenesisShortcutFallsThrough :: Assertion
caseGenesisShortcutFallsThrough = do
  fixture <- genesisPlacementFixture
  let dynamicPrerequisite = controlIndex 3
  progressed <-
    checked
      "advance control before same-genesis dynamic occurrence"
      ( GraphProgress.prepareStructuralControlProgress
          dynamicPrerequisite
          (fixtureProgress fixture)
      )
  let (withControl, _) = GraphProgress.commitStructuralControlProgress progressed
      application =
        deltaApplication
          (fixtureRemoteOwner fixture)
          (fixtureLocalProcess fixture)
          (fixtureDelta fixture)
          (fixtureSort fixture)
          fixtureEmptyVector
          dynamicPrerequisite
          228
  (graphAfter, progressAfter) <-
    applyStructuralApplication
      "same-genesis dynamic occurrence"
      (fixtureViews fixture)
      application
      Nothing
      (fixtureGraph fixture)
      withControl
  laterIncarnation <-
    checked "same-genesis later incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 229))
  let laterRoute =
        Placement.applicationDeltaRoute
          (fixtureDelta fixture)
          (fixtureSort fixture)
          (fixtureOccurrence fixture)
          (fixtureObject fixture)
          (fixtureLocalProcess fixture)
          laterIncarnation
          dynamicPrerequisite
  assertBool
    "a matching retained occurrence is considered after the immutable root rejects its later prerequisite"
    ( PeerPlacement.remotePlacementRouteIsValid
        (fixtureSystem fixture)
        (fixtureLocalOwner fixture)
        (fixtureIndexedBootstraps fixture)
        graphAfter
        progressAfter
        laterRoute
    )

caseDynamicHandoffRouteHistory :: Assertion
caseDynamicHandoffRouteHistory = do
  let localOwner = checkedLocalHeraldEpoch fixtureCheckedGenesis
      remoteOwner = heraldMemberEpoch fixtureRemoteMember
      processBefore = checkedValue "dynamic remote process" (mkProcessEpochId (fixtureIdentifierBytes 230))
      processAfter = checkedValue "dynamic local process" (mkProcessEpochId (fixtureIdentifierBytes 231))
      delta = checkedValue "dynamic handoff delta" (mkDeltaId (fixtureIdentifierBytes 232))
      object = globalObjectIdFromDeltaId delta
      views =
        Reconciliation.reconciliationViews
          localOwner
          fixtureEffectiveSorts
          Set.empty
          (Map.fromList [(processBefore, remoteOwner), (processAfter, localOwner)])
          (Set.singleton (processAfter, object))
      initialReconciliation =
        Reconciliation.emptyStructuralAppliedState fixtureEmptyVector
      application =
        deltaApplication
          remoteOwner
          processBefore
          delta
          (profileSortFor SortDefinitionRole)
          fixtureEmptyVector
          (controlIndex 0)
          233
  initialProgress <- initialProgressWith initialReconciliation
  (graphBeforeHandoff, progressBeforeHandoff) <-
    applyStructuralApplication
      "dynamic route before handoff"
      views
      application
      Nothing
      Graph.emptyState
      initialProgress
  let releaseIndex = controlIndex 9
      releaseCause =
        checkedValue
          "dynamic handoff cause"
          ( labelReleaseCause
              (checkedValue "dynamic handoff decision" (mkLabelDecisionId (fixtureIdentifierBytes 234)))
              releaseIndex
          )
      freshLocalIncarnation =
        checkedValue "dynamic handoff incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 235))
      freshEvidence =
        Reconciliation.freshStoreIncarnationEvidence
          Reconciliation.GeneratedStoreIncarnation
          delta
          freshLocalIncarnation
  withRelease <-
    advanceControlTo "dynamic handoff" releaseIndex progressBeforeHandoff
  (_, afterHandoff) <-
    applyStructuralControlRefresh
      "dynamic handoff"
      views
      releaseCause
      object
      (releasedLabel ((ProcessLabel processAfter, 1)))
      (Just freshEvidence)
      graphBeforeHandoff
      withRelease
  staleIncarnation <-
    checked "stale dynamic incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 236))
  let sortOccurrence' = fixtureSortOccurrence SortDefinitionRole
      route process incarnation prerequisite =
        Placement.applicationDeltaRoute
          delta
          (profileSortFor SortDefinitionRole)
          (sortOccurrenceDefinition sortOccurrence')
          object
          process
          incarnation
          prerequisite
      oldRoute = route processBefore staleIncarnation (controlIndex 0)
      currentRoute = route processAfter freshLocalIncarnation releaseIndex
      admitted owner candidate =
        PeerPlacement.remotePlacementRouteIsValid
          (checkedSystemId fixtureCheckedGenesis)
          owner
          []
          Graph.emptyState
          afterHandoff
          candidate
  assertBool
    "the old dynamic controller remains an exact historical route coordinate"
    (admitted remoteOwner oldRoute)
  assertBool
    "the current dynamic controller is admitted at the release prerequisite"
    (admitted localOwner currentRoute)
  assertBool
    "the old dynamic coordinate rejects the new prerequisite"
    (not (admitted remoteOwner (route processBefore staleIncarnation releaseIndex)))
  assertBool
    "the current dynamic coordinate rejects the old prerequisite"
    (not (admitted localOwner (route processAfter freshLocalIncarnation (controlIndex 0))))
  forM_ [localOwner, remoteOwner] $ \owner ->
    assertRemoteRouteAdmission (checkedSystemId fixtureCheckedGenesis) owner [] Graph.emptyState afterHandoff [oldRoute, currentRoute, route processBefore freshLocalIncarnation (controlIndex 0), route processAfter staleIncarnation releaseIndex, route processBefore staleIncarnation releaseIndex, route processAfter freshLocalIncarnation (controlIndex 0)]

caseNoopStructuralControlRefresh :: Assertion
caseNoopStructuralControlRefresh = do
  fixture <- genesisPlacementFixture
  let cause = checkedValue "no-op refresh cause" (labelReleaseCause (checkedValue "no-op refresh decision" (mkLabelDecisionId (fixtureIdentifierBytes 241))) (controlIndex 1))
      localLabel = releasedLabel ((ProcessLabel (fixtureLocalProcess fixture), 1))
      remoteLabel = releasedLabel ((ProcessLabel (fixtureRemoteProcess fixture), 2))
  controlled <- advanceControlTo "no-op refresh control" (controlIndex 1) (fixtureProgress fixture)
  let clean = GraphProgress.clearPreparationReadinessChange controlled
  (seededGraph, unchanged) <- applyStructuralControlRefresh "same genesis controller" (fixtureViews fixture) cause (fixtureObject fixture) localLabel Nothing (fixtureGraph fixture) clean
  assertEqual "unchanged projection does not notify readiness" False (fst (GraphProgress.takePreparationReadinessChange unchanged))
  assertEqual "unchanged projection preserves the complete progress owner" clean unchanged
  (handoffGraph, handoff) <- applyStructuralControlRefresh "actual genesis controller handoff" (fixtureViews fixture) cause (fixtureObject fixture) remoteLabel Nothing seededGraph unchanged
  assertEqual "a changed projection still notifies readiness" True (fst (GraphProgress.takePreparationReadinessChange handoff))
  let parked = GraphProgress.clearPreparationReadinessChange handoff
  forM_ [1 :: Int .. 5] $ \_ -> do
    (_, replayed) <- applyStructuralControlRefresh "repeated unchanged controller" (fixtureViews fixture) cause (fixtureObject fixture) remoteLabel Nothing handoffGraph parked
    assertEqual "repeated no-op refresh does not wake preparation work" False (fst (GraphProgress.takePreparationReadinessChange replayed))
    assertEqual "repeated no-op refresh retains the complete progress owner" parked replayed

-- The pre-index admission rule is kept as a test oracle. It deliberately
-- performs the historical enumeration and bootstrap list searches so snapshot
-- sharing, the uncontrolled shortcut and host-owned incarnation stay covered.
referenceRemoteRouteIsValid ::
  SystemId ->
  HeraldEpoch ->
  [(ProcessEpochId, AppliedProcessBootstrap)] ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  Placement.DeltaRoute ->
  Bool
referenceRemoteRouteIsValid system owner bootstraps graph progress route =
  case Placement.viewDeltaRoute route of
    Placement.PrivateSystemViewDeltaRouteView _ delta _ _ _ ->
      Graph.graphHasVertex (DeltaVertex delta) graph
        && Placement.validatePrivateSystemViewDeltaRoute system owner route == Right ()
    Placement.ApplicationDeltaRouteView delta sortId occurrence object process _incarnation prerequisite ->
      uncontrolledRoot || any matches (Reconciliation.structuralAppliedDeltaRouteCoordinates reconciliation)
      where
        uncontrolledRoot = case lookup process bootstraps of
          Just bootstrap
            | appliedProcessEpochId bootstrap == process && appliedProcessResidence bootstrap == owner ->
                case find ((== ReaderRoot delta) . appliedRootRole) (appliedProcessRoots bootstrap) of
                  Just root ->
                    not (Reconciliation.structuralAppliedObjectHasControlHistory object reconciliation)
                      && Graph.graphHasVertex (DeltaVertex delta) graph
                      && appliedRootSortId root == sortId
                      && appliedRootOccurrenceId root == occurrence
                      && appliedRootObjectId root == object
                      && maybe False ((== owner) . fst) (appliedRootPlacement root)
                      && appliedRootControlPrerequisite root == prerequisite
                  Nothing -> False
          _ -> False
        matches coordinate =
          Reconciliation.appliedDeltaRouteDelta coordinate == delta
            && Reconciliation.appliedDeltaRouteObject coordinate == object
            && Reconciliation.appliedDeltaRouteSort coordinate == sortOccurrence sortId occurrence
            && Reconciliation.appliedDeltaRouteController coordinate == Reconciliation.LiveProcessController process owner
            && Reconciliation.appliedDeltaRouteControlPrerequisite coordinate == prerequisite
  where
    reconciliation = GraphProgress.structuralProgressReconciliation progress

assertRemoteRouteAdmission ::
  SystemId ->
  HeraldEpoch ->
  [(ProcessEpochId, AppliedProcessBootstrap)] ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  [Placement.DeltaRoute] ->
  Assertion
assertRemoteRouteAdmission system owner bootstraps graph progress routes =
  forM_ routes $ \route -> do
    let expected = referenceRemoteRouteIsValid system owner bootstraps graph progress route
        supplied = [route, systemViewRoute owner EdgeRole]
    assertEqual "single route preserves exhaustive authority and incarnation policy" expected (PeerPlacement.remotePlacementRouteIsValid system owner bootstraps graph progress route)
    assertEqual "snapshot index preserves single route authority" expected (PeerPlacement.placementUpdateClaimsAreValid system owner bootstraps graph progress (fullSnapshot owner Placement.firstPlacementSequence [route]))
    -- Application routes and the closed private system route use distinct
    -- Deltas, so this is one legitimate complete multi-route snapshot.
    case Placement.viewDeltaRoute route of
      Placement.ApplicationDeltaRouteView {} ->
        assertEqual "snapshot shares authority across all advertised routes" (all (referenceRemoteRouteIsValid system owner bootstraps graph progress) supplied) (PeerPlacement.placementUpdateClaimsAreValid system owner bootstraps graph progress (fullSnapshot owner Placement.firstPlacementSequence supplied))
      Placement.PrivateSystemViewDeltaRouteView {} -> pure ()

-- Compare the factored lookup with the retained exhaustive projection rather
-- than reimplementing the max/prerequisite identity in the test oracle. The
-- generated handoff property above also runs this at every retained state.
assertFactoredRouteAdmission :: DeltaId -> Reconciliation.StructuralAppliedState -> Assertion
assertFactoredRouteAdmission delta state = do
  let prepared = Reconciliation.prepareDeltaRouteAdmission state
      coordinates =
        filter
          ((== delta) . Reconciliation.appliedDeltaRouteDelta)
          (Reconciliation.structuralAppliedDeltaRouteCoordinates state)
      keys = Set.toList (Set.fromList (map key coordinates))
      wrongObject = checkedValue "unrelated route object" (mkGlobalObjectId (fixtureIdentifierBytes 237))
      wrongSort = sortOccurrence (checkedValue "unrelated route sort" (mkSortId (fixtureIdentifierBytes 238))) (sortOccurrenceDefinition (fixtureSortOccurrence DeltaRole))
      mutatedKeys = [(wrongObject, typedSort, controller) | (_, typedSort, controller) <- keys] <> [(object, wrongSort, controller) | (object, _, controller) <- keys]
      prerequisites =
        Set.toList
          $ Set.fromList
          $ controlIndex 0
            : [ controlIndex adjacent
              | coordinate <- coordinates,
                let index = controlIndexWord64 (Reconciliation.appliedDeltaRouteControlPrerequisite coordinate),
                adjacent <- [index, index + 1] <> [index - 1 | index > 0]
              ]
  assertBool "the differential fixture retains historical live coordinates" (not (null keys))
  forM_ (keys <> mutatedKeys) $ \(object, typedSort, controller) -> do
    let expected = [Reconciliation.appliedDeltaRouteControlPrerequisite coordinate | coordinate <- coordinates, key coordinate == (object, typedSort, controller)]
    assertEqual
      "factored latest prerequisite equals the exhaustive historical maximum"
      (case expected of [] -> Nothing; first : rest -> Just (foldl max first rest))
      (Reconciliation.preparedDeltaRouteLatestPrerequisite delta object typedSort controller prepared)
    forM_ prerequisites $ \prerequisite ->
      assertEqual
        "factored exact prerequisite equals exhaustive historical membership"
        (prerequisite `elem` expected)
        (Reconciliation.preparedDeltaRouteIsValid delta object typedSort controller prerequisite prepared)
    assertEqual
      "every control kind excludes the uncontrolled genesis shortcut"
      (Reconciliation.structuralAppliedObjectHasControlHistory object state)
      (Reconciliation.preparedDeltaRouteObjectHasControlHistory object prepared)
  where
    key coordinate =
      ( Reconciliation.appliedDeltaRouteObject coordinate,
        Reconciliation.appliedDeltaRouteSort coordinate,
        Reconciliation.appliedDeltaRouteController coordinate
      )

caseFactoredRouteLifetimes :: Assertion
caseFactoredRouteLifetimes = do
  fixture <- genesisPlacementFixture
  let delta = fixtureDelta fixture
      subject = fixtureObject fixture
      process = fixtureRemoteProcess fixture
      views = fixtureViews fixture
      initial = GraphProgress.structuralProgressReconciliation (fixtureProgress fixture)
      cause index = checkedValue "factored route label cause" (labelReleaseCause (checkedValue "factored route decision" (mkLabelDecisionId (fixtureIdentifierBytes (fromIntegral index + 170)))) (controlIndex index))
      refresh index label predecessor = do
        preparation <- checked "factored route control" (Reconciliation.prepareStructuralLabelControlRefresh views (cause index) subject label Nothing predecessor)
        case preparation of
          Reconciliation.StructuralHeld dependencies -> assertFailure ("factored route control held: " <> show dependencies)
          Reconciliation.StructuralReady ready -> do
            let successor = Reconciliation.preparedStructuralSuccessor ready
            assertFactoredRouteAdmission delta successor
            pure successor
  assertFactoredRouteAdmission delta initial
  handedOff <- refresh 2 (releasedLabel (ProcessLabel process, 1)) initial
  -- The carrier's prerequisite exceeds the already retained handoff index:
  -- this exercises the other max branch as well as the equality boundary.
  -- Its original controller is Void, so the earlier control is the only
  -- possible authority for a live route at the later prerequisite.
  let later = deltaApplicationWithOwner (fixtureRemoteOwner fixture) VoidLabel delta (fixtureSort fixture) fixtureEmptyVector (controlIndex 4) 240
  laterPreparation <- checked "carrier after control" (Reconciliation.prepareStructuralReconciliation views later Nothing handedOff)
  withLater <- case laterPreparation of
    Reconciliation.StructuralHeld dependencies -> assertFailure ("late carrier held: " <> show dependencies)
    Reconciliation.StructuralReady ready -> pure (Reconciliation.preparedStructuralSuccessor ready)
  assertFactoredRouteAdmission delta withLater
  let typedSort = sortOccurrence (fixtureSort fixture) (fixtureOccurrence fixture)
      controller = Reconciliation.LiveProcessController process (fixtureRemoteOwner fixture)
      indexed = Reconciliation.prepareDeltaRouteAdmission withLater
  assertBool "control earlier than carrier admits the carrier's prerequisite" (Reconciliation.preparedDeltaRouteIsValid delta subject typedSort controller (controlIndex 4) indexed)
  assertBool "a gap in historical prerequisites stays inadmissible" (not (Reconciliation.preparedDeltaRouteIsValid delta subject typedSort controller (controlIndex 3) indexed))
  passive <- refresh 6 (releasedLabel (VoidLabel, 2)) withLater
  active <- refresh 8 (releasedLabel (ProcessLabel process, 3)) passive
  endCause <- checked "factored route End cause" (processEndCause process (controlIndex 10))
  let endedViews = Reconciliation.reconciliationViewsWithEndedProcesses (Map.singleton process (fixtureRemoteOwner fixture, controlIndex 10)) views
  endPreparations <- checked "factored route End" (Reconciliation.prepareStructuralProcessEndRefreshes endedViews endCause process active)
  ended <-
    foldM
      ( \_ (_, ready) -> do
          let successor = Reconciliation.preparedStructuralSuccessor ready
          assertFactoredRouteAdmission delta successor
          pure successor
      )
      active
      endPreparations
  deleted <- refresh 12 (releasedDeleted 4) ended
  assertEqual "deletion leaves every exact historical live route intact" (Reconciliation.structuralAppliedDeltaRouteCoordinates ended) (Reconciliation.structuralAppliedDeltaRouteCoordinates deleted)
  -- An imported publication baseline has no dynamic occurrence and its own
  -- original prerequisite, but uses the same factored route authority.
  let baselineApplication = deltaApplication (fixtureRemoteOwner fixture) process delta (fixtureSort fixture) fixtureEmptyVector (controlIndex 4) 240
  baselinePreparation <- checked "factored publication baseline" (Reconciliation.structuralAppliedStateWithBaseline views fixtureEmptyVector (Map.singleton subject (Reconciliation.baselineStructuralWinner (fixtureSortOccurrence DeltaRole) (Reconciliation.structuralApplicationPublication baselineApplication))) Map.empty)
  baseline <- case baselinePreparation of
    Left dependencies -> assertFailure ("factored publication baseline held: " <> show dependencies)
    Right state -> pure state
  assertFactoredRouteAdmission delta baseline

genesisPlacementFixture :: IO GenesisPlacementFixture
genesisPlacementFixture = do
  seeded <-
    case Reconciliation.structuralAppliedStateWithGenesis
      (checkedSystemId fixtureCheckedGenesis)
      views
      fixtureEmptyVector
      bootstraps of
      Left problem -> assertFailure ("genesis reconciliation seed: " <> show problem)
      Right (Left dependencies) ->
        assertFailure ("genesis reconciliation seed held: " <> show dependencies)
      Right (Right state) -> pure state
  progress <- initialProgressWith seeded
  let (localBootstrap, root, delta, incarnation) = localReader
  pure
    GenesisPlacementFixture
      { fixtureSystem = checkedSystemId fixtureCheckedGenesis,
        fixtureLocalOwner = localOwner,
        fixtureRemoteOwner = remoteOwner,
        fixtureIndexedBootstraps =
          [(appliedProcessEpochId bootstrap, bootstrap) | bootstrap <- bootstraps],
        fixtureViews = views,
        fixtureGraph =
          Graph.initialTopologyState
            (checkedInitialTopologyProjection fixtureInitialProjection),
        fixtureProgress = progress,
        fixtureDelta = delta,
        fixtureObject = appliedRootObjectId root,
        fixtureSort = appliedRootSortId root,
        fixtureOccurrence = appliedRootOccurrenceId root,
        fixtureLocalProcess = appliedProcessEpochId localBootstrap,
        fixtureRemoteProcess = appliedProcessEpochId remoteBootstrap,
        fixtureIncarnation = incarnation,
        fixturePrerequisite = appliedRootControlPrerequisite root
      }
  where
    bootstraps = checkedInitialBootstraps fixtureInitialProjection
    localOwner = checkedLocalHeraldEpoch fixtureCheckedGenesis
    remoteOwner = heraldMemberEpoch fixtureRemoteMember
    localReader =
      case [ (bootstrap, root, delta, incarnation)
           | bootstrap <- bootstraps,
             appliedProcessResidence bootstrap == localOwner,
             root <- appliedProcessRoots bootstrap,
             ReaderRoot delta <- [appliedRootRole root],
             Just (placementOwner, incarnation) <- [appliedRootPlacement root],
             placementOwner == localOwner
           ] of
        first : _ -> first
        [] -> error "checked genesis has no locally placed reader root"
    remoteBootstrap =
      case filter ((== remoteOwner) . appliedProcessResidence) bootstraps of
        first : _ -> first
        [] -> error "checked genesis has no remote process target"
    views =
      Reconciliation.reconciliationViews
        localOwner
        fixtureEffectiveSorts
        ( Set.fromList
            ( checkedInitialTopologyVertices
                (checkedInitialTopologyProjection fixtureInitialProjection)
            )
        )
        ( Map.fromList
            [ (appliedProcessEpochId bootstrap, appliedProcessResidence bootstrap)
            | bootstrap <- bootstraps
            ]
        )
        ( Set.fromList
            [ (appliedProcessEpochId bootstrap, object)
            | bootstrap <- bootstraps,
              object <- appliedProcessEnvironmentObjects bootstrap
            ]
        )

fixtureInitialProjection :: CheckedInitialBootstraps
fixtureInitialProjection =
  checkedValue
    "global checked bootstrap projection"
    ( checkInitialBootstraps
        fixtureCheckedGenesis
        ( PrimordialProcessManifest
            (fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId])
        )
    )

fixtureMembership :: NonEmpty HeraldEpoch
fixtureMembership =
  case NonEmpty.nonEmpty (fmap heraldMemberEpoch (checkedActiveHeralds fixtureCheckedGenesis)) of
    Just members -> members
    Nothing -> error "checked genesis has no structural membership"

fixtureEmptyVector :: StructuralVersionVector
fixtureEmptyVector = emptyStructuralVersionVector fixtureMembershipGeneration

fixtureMembershipGeneration :: HeraldMembershipGeneration
fixtureMembershipGeneration = placementMembership fixtureMembership

fixtureEffectiveSorts :: Map.Map SortId SortOccurrence
fixtureEffectiveSorts =
  Map.fromList
    [ (profileSortFor role, fixtureSortOccurrence role)
    | role <- allPredefinedSortRoles
    ]

fixtureSortOccurrence :: PredefinedSortRole -> SortOccurrence
fixtureSortOccurrence role =
  sortOccurrence
    (profileSortFor role)
    (predefinedOccurrenceFor role (checkedPredefinedOccurrenceSet fixtureCheckedGenesis))

initialProgressWith ::
  Reconciliation.StructuralAppliedState ->
  IO GraphProgress.StructuralProgressState
initialProgressWith reconciliation =
  checked
    "fixture structural progress"
    ( GraphProgress.initialStructuralProgressState
        (checkedSystemId fixtureCheckedGenesis)
        (checkedLocalHeraldEpoch fixtureCheckedGenesis)
        fixtureMembershipGeneration
        (checkedInitialProjectionDigest fixtureInitialProjection)
        reconciliation
    )

advanceControlTo ::
  String ->
  ControlIndex ->
  GraphProgress.StructuralProgressState ->
  IO GraphProgress.StructuralProgressState
advanceControlTo context index progress = do
  prepared <-
    checked
      (context <> " control progress")
      (GraphProgress.prepareStructuralControlProgress index progress)
  pure (fst (GraphProgress.commitStructuralControlProgress prepared))

deltaApplication ::
  HeraldEpoch ->
  ProcessEpochId ->
  DeltaId ->
  SortId ->
  StructuralVersionVector ->
  ControlIndex ->
  Word8 ->
  Reconciliation.StructuralApplication
deltaApplication source process delta carriedSort predecessor prerequisite seed =
  deltaApplicationWithOwner source (ProcessLabel process) delta carriedSort predecessor prerequisite seed

deltaApplicationWithOwner ::
  HeraldEpoch ->
  LabelOwner ->
  DeltaId ->
  SortId ->
  StructuralVersionVector ->
  ControlIndex ->
  Word8 ->
  Reconciliation.StructuralApplication
deltaApplicationWithOwner source owner delta carriedSort predecessor prerequisite seed =
  Reconciliation.structuralApplication
    ( structuralOccurrenceId
        source
        (checkedValue "fixture structural sequence" (mkStructuralSequence 1))
    )
    predecessor
    (fixtureSortOccurrence DeltaRole)
    publication
    prerequisite
  where
    publication =
      checkedValue
        "fixture delta publication"
        ( mkCheckedPublication
            (predefinedCatalogueDescriptor (profileEntryFor DeltaRole))
            ( publicationId
                (checkedValue "fixture publication nabla" (mkNablaId (fixtureIdentifierBytes seed)))
                genesisAuthorityEpoch
                source
                (nablaSequence 1)
            )
            (deltaRootValue (globalObjectIdFromDeltaId delta) owner carriedSort)
        )

deltaRootValue :: GlobalObjectId -> LabelOwner -> SortId -> Value
deltaRootValue object owner carriedSort =
  checkedValue
    "fixture delta value"
    ( recordValue
        [ (objectIdField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
          (labelField, labelValue ((owner, 0))),
          (sortIdField, bytesValue (sortIdBytes carriedSort))
        ]
    )

objectIdField, labelField, sortIdField :: FieldName
objectIdField = fixtureField "object_id"
labelField = fixtureField "label"
sortIdField = fixtureField "sort_id"

fixtureField :: String -> FieldName
fixtureField name = checkedValue "fixture field" (mkFieldName (Text.pack name))

applyStructuralApplication ::
  String ->
  Reconciliation.ReconciliationViews ->
  Reconciliation.StructuralApplication ->
  Maybe Reconciliation.FreshStoreIncarnationEvidence ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  IO (Graph.State, GraphProgress.StructuralProgressState)
applyStructuralApplication context views application fresh graph progress = do
  reconciled <-
    case Reconciliation.prepareStructuralReconciliation
      views
      application
      fresh
      (GraphProgress.structuralProgressReconciliation progress) of
      Left problem -> assertFailure (context <> " reconciliation: " <> show problem)
      Right (Reconciliation.StructuralHeld dependencies) ->
        assertFailure (context <> " held: " <> show dependencies)
      Right (Reconciliation.StructuralReady ready) -> pure ready
  digest <-
    checked
      (context <> " digest")
      (mkStructuralPublicationDigest (ByteString.replicate 32 0x5a))
  stamp <-
    checked
      (context <> " stamp")
      ( mkStructuralOccurrenceStamp
          (Reconciliation.structuralApplicationOccurrence application)
          (Reconciliation.structuralApplicationPredecessor application)
          ( checkedPublicationId
              (Reconciliation.structuralApplicationPublication application)
          )
          digest
          DeltaCarrier
      )
  prepared <-
    checked
      (context <> " progress")
      ( GraphProgress.prepareStructuralApplication
          stamp
          (GraphProgress.structuralGenesisCutId progress)
          reconciled
          graph
          progress
      )
  let (graphAfter, progressAfter, _) =
        GraphProgress.commitStructuralApplication prepared
  pure (graphAfter, progressAfter)

applyStructuralControlRefresh ::
  String ->
  Reconciliation.ReconciliationViews ->
  StructuralConsequenceCause ->
  GlobalObjectId ->
  ReleasedLabelState ->
  Maybe Reconciliation.FreshStoreIncarnationEvidence ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  IO (Graph.State, GraphProgress.StructuralProgressState)
applyStructuralControlRefresh context views cause object released fresh graph progress = do
  reconciled <-
    case Reconciliation.prepareStructuralLabelControlRefresh
      views
      cause
      object
      released
      fresh
      (GraphProgress.structuralProgressReconciliation progress) of
      Left problem -> assertFailure (context <> " reconciliation: " <> show problem)
      Right (Reconciliation.StructuralHeld dependencies) ->
        assertFailure (context <> " held: " <> show dependencies)
      Right (Reconciliation.StructuralReady ready) -> pure ready
  prepared <-
    checked
      (context <> " progress")
      (GraphProgress.prepareStructuralProjectionRefresh reconciled graph progress)
  pure (GraphProgress.commitStructuralProjectionRefresh prepared)

propSnapshotNormalization :: Bool -> Property
propSnapshotNormalization reverseInput =
  Placement.placementSnapshot owner Placement.firstPlacementSequence supplied
    === Placement.placementSnapshot owner Placement.firstPlacementSequence canonical
  where
    owner = heraldMemberEpoch fixtureRemoteMember
    routes = [systemViewRoute owner SortDefinitionRole, systemViewRoute owner EdgeRole]
    supplied = if reverseInput then reverse routes else routes
    canonical = routes

caseSnapshotReplacement :: Assertion
caseSnapshotReplacement = do
  let owner = heraldMemberEpoch fixtureRemoteMember
      routeA = systemViewRoute owner SortDefinitionRole
      routeB = systemViewRoute owner EdgeRole
      initial = PlacementState.initialState (checkedLocalHeraldEpoch fixtureCheckedGenesis)
      snapshotOne = fullSnapshot owner Placement.firstPlacementSequence [routeA]
      sequenceTwo = Placement.nextPlacementSequence Placement.firstPlacementSequence
      sequenceThree = Placement.nextPlacementSequence sequenceTwo
      replacement = fullSnapshot owner sequenceThree [routeB]
      delayed = fullSnapshot owner sequenceTwo [routeA, routeB]
  (afterSnapshot, snapshotResult) <- applyRemote "initial snapshot" snapshotOne initial
  assertEqual "initial snapshot accepted" PlacementState.RemotePlacementAccepted (PlacementState.remotePlacementDisposition snapshotResult)
  (afterReplacement, replacementResult) <-
    applyRemote "newer complete snapshot" replacement afterSnapshot
  assertEqual
    "the newer complete snapshot is accepted without an intermediate revision"
    PlacementState.RemotePlacementAccepted
    (PlacementState.remotePlacementDisposition replacementResult)
  assertEqual "the owner advances directly to the newer sequence" (Just sequenceThree) (PlacementState.remotePlacementSequence owner afterReplacement)
  assertEqual "omission withdraws the old route atomically" [routeB] (PlacementState.remotePlacementRoutes owner afterReplacement)
  assertEqual
    "the older cut remains reconstructable"
    (Just [routeA])
    (PlacementState.placementRoutesAtRevision owner Placement.firstPlacementSequence afterReplacement)
  assertEqual
    "the replacement cut remains reconstructable"
    (Just [routeB])
    (PlacementState.placementRoutesAtRevision owner sequenceThree afterReplacement)
  (afterDelayed, delayedResult) <-
    applyRemote "delayed older snapshot" delayed afterReplacement
  assertEqual
    "a delayed older complete snapshot is stale"
    PlacementState.RemotePlacementStale
    (PlacementState.remotePlacementDisposition delayedResult)
  assertEqual "the delayed snapshot is state-identical" afterReplacement afterDelayed

caseAlignmentPlacementChanges :: Assertion
caseAlignmentPlacementChanges = do
  let owner = heraldMemberEpoch fixtureRemoteMember
      initial = PlacementState.initialState (checkedLocalHeraldEpoch fixtureCheckedGenesis)
      first = Placement.firstPlacementSequence
      second = Placement.nextPlacementSequence first
      third = Placement.nextPlacementSequence second
      route = systemViewRoute owner SortDefinitionRole
      snapshot = fullSnapshot owner first [route]
      clean = PlacementState.clearAlignmentLossChange . PlacementState.clearAlignmentPlanChange
      notifications state = (fst (PlacementState.takeAlignmentPlanChange state), fst (PlacementState.takeAlignmentLossChange state))
  (admitted, _) <- applyRemote "alignment initial snapshot" snapshot initial
  assertEqual "live snapshot wakes both independent consumers" (True, True) (notifications admitted)
  assertEqual "draining plans leaves loss pending" (False, True) (notifications (snd (PlacementState.takeAlignmentPlanChange admitted)))
  (duplicate, _) <- applyRemote "alignment snapshot replay" snapshot (clean admitted)
  assertEqual "equal live evidence is silent" (False, False) (notifications duplicate)
  (advanced, _) <- applyRemote "alignment equal routes at a newer revision" (fullSnapshot owner second [route]) duplicate
  assertEqual "a crossed frozen revision may establish a previously unknown loss" (True, True) (notifications advanced)
  historical <- checked "alignment historical placement" (PlacementState.retainHistoricalPlacementSnapshot (snapshotAt owner third []) (clean advanced))
  assertEqual "new exact history wakes plans but proves no live loss" (True, False) (notifications historical)
  replayed <- checked "alignment historical replay" (PlacementState.retainHistoricalPlacementSnapshot (snapshotAt owner third []) (clean historical))
  assertEqual "historical replay is silent" (False, False) (notifications replayed)
  retired <- checked "alignment remote retirement" (PlacementState.prepareRemotePlacementRetirement owner replayed)
  let (afterRetirement, _) = PlacementState.commitRemotePlacementRetirement retired
  assertEqual "current projection removal wakes both consumers" (True, True) (notifications afterRetirement)
  repeated <- checked "alignment retirement replay" (PlacementState.prepareRemotePlacementRetirement owner (clean afterRetirement))
  assertEqual "retirement replay is silent" (False, False) (notifications (fst (PlacementState.commitRemotePlacementRetirement repeated)))

caseSequenceConflict :: Assertion
caseSequenceConflict = do
  let owner = heraldMemberEpoch fixtureRemoteMember
      routeA = systemViewRoute owner SortDefinitionRole
      routeB = systemViewRoute owner EdgeRole
      initial = PlacementState.initialState (checkedLocalHeraldEpoch fixtureCheckedGenesis)
      retained = fullSnapshot owner Placement.firstPlacementSequence [routeA]
      conflicting = fullSnapshot owner Placement.firstPlacementSequence [routeB]
  (afterFirst, _) <- applyRemote "first snapshot" retained initial
  (afterRetry, retryResult) <- applyRemote "snapshot retry" retained afterFirst
  assertEqual "exact retry is duplicate" PlacementState.RemotePlacementDuplicate (PlacementState.remotePlacementDisposition retryResult)
  assertEqual "exact retry is state-identical" afterFirst afterRetry
  case PlacementState.prepareRemotePlacement conflicting afterFirst of
    Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementSequenceConflict observedOwner observedSequence)) -> do
      assertEqual "conflict owner" owner observedOwner
      assertEqual "conflict sequence" Placement.firstPlacementSequence observedSequence
    Left problem -> assertFailure ("unexpected sequence-conflict problem: " <> show problem)
    Right _ -> assertFailure "unequal content at one sequence was accepted"

caseOwnerIsolation :: Assertion
caseOwnerIsolation = do
  let ownerA = heraldMemberEpoch fixtureRemoteMember
      routeA = systemViewRoute ownerA SortDefinitionRole
      localOwner = checkedLocalHeraldEpoch fixtureCheckedGenesis
  ownerB <- checked "second remote owner" (mkHeraldEpoch (fixtureIdentifierBytes 212))
  let routeB = systemViewRoute ownerB EdgeRole
      initial = PlacementState.initialState localOwner
  (afterA, _) <- applyRemote "owner A snapshot" (fullSnapshot ownerA Placement.firstPlacementSequence [routeA]) initial
  (afterB, _) <- applyRemote "owner B snapshot" (fullSnapshot ownerB Placement.firstPlacementSequence [routeB]) afterA
  assertEqual "owner A retains only A" [routeA] (PlacementState.remotePlacementRoutes ownerA afterB)
  assertEqual "owner B retains only B" [routeB] (PlacementState.remotePlacementRoutes ownerB afterB)
  assertBool "both owners have independent sequence one" (all ((== Just Placement.firstPlacementSequence) . (`PlacementState.remotePlacementSequence` afterB)) [ownerA, ownerB])

caseRemoteRetirement :: Assertion
caseRemoteRetirement = do
  let localOwner = checkedLocalHeraldEpoch fixtureCheckedGenesis
      owner = heraldMemberEpoch fixtureRemoteMember
      route = systemViewRoute owner SortDefinitionRole
      snapshot = fullSnapshot owner Placement.firstPlacementSequence [route]
      initial = PlacementState.initialState localOwner
  localCut <- checked "local cut" (PlacementState.prepareLocalSnapshot initial)
  let (advertised, _) = PlacementState.commitLocalSnapshot localCut
      acknowledgement = Placement.placementAcknowledgement localOwner Placement.firstPlacementSequence
  ack <- checked "peer acknowledgement" (PlacementState.preparePlacementAcknowledgement owner acknowledgement advertised)
  let (acknowledged, _) = PlacementState.commitPlacementAcknowledgement ack
  (projected, _) <- applyRemote "remote projection" snapshot acknowledged
  retirement <- checked "remote retirement" (PlacementState.prepareRemotePlacementRetirement owner projected)
  assertEqual
    "prepared loss is exactly the live projection"
    [route]
    (PlacementState.retiredRemotePlacementRoutes (PlacementState.preparedRemotePlacementRetirement retirement))
  let (retired, loss) = PlacementState.commitRemotePlacementRetirement retirement
  assertEqual "loss names the retired owner" owner (PlacementState.retiredRemotePlacementOwner loss)
  assertEqual "live projection is removed" [] (PlacementState.remotePlacementRoutes owner retired)
  assertEqual "live sequence is removed" Nothing (PlacementState.remotePlacementSequence owner retired)
  assertEqual "owner is absent from live owners" [] (PlacementState.remotePlacementOwners retired)
  assertEqual "peer acknowledgement is removed" Nothing (PlacementState.placementAcknowledgedThrough owner retired)
  assertEqual
    "historical projection remains reconstructable"
    (Just [route])
    (PlacementState.placementRoutesAtRevision owner Placement.firstPlacementSequence retired)
  duplicate <- checked "duplicate retirement" (PlacementState.prepareRemotePlacementRetirement owner retired)
  let (afterDuplicate, duplicateLoss) = PlacementState.commitRemotePlacementRetirement duplicate
  assertEqual "duplicate retirement is state-identical" retired afterDuplicate
  assertEqual "duplicate retirement emits no repeated loss" [] (PlacementState.retiredRemotePlacementRoutes duplicateLoss)
  case PlacementState.prepareRemotePlacement snapshot retired of
    Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementRetiredRemoteOwner actual)) ->
      assertEqual "late snapshot owner" owner actual
    Left problem -> assertFailure ("unexpected late-snapshot problem: " <> show problem)
    Right _ -> assertFailure "late snapshot was accepted"
  case PlacementState.preparePlacementAcknowledgement owner acknowledgement retired of
    Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementRetiredRemoteOwner actual)) ->
      assertEqual "late acknowledgement owner" owner actual
    Left problem -> assertFailure ("unexpected late-acknowledgement problem: " <> show problem)
    Right _ -> assertFailure "late acknowledgement was accepted"

caseRevisionVectorHistory :: Assertion
caseRevisionVectorHistory = do
  let localOwner = checkedLocalHeraldEpoch fixtureCheckedGenesis
      remoteOwner = heraldMemberEpoch fixtureRemoteMember
      remoteRouteOne = systemViewRoute remoteOwner SortDefinitionRole
      remoteRouteTwo = systemViewRoute remoteOwner EdgeRole
      localRoute = systemViewRoute localOwner NeutralVertexRole
      membership = placementMembership (localOwner :| [remoteOwner])
      initial = PlacementState.initialState localOwner
  preparedInitial <-
    checked "initial local cut" (PlacementState.prepareLocalSnapshot initial)
  let (withLocalOne, _) = PlacementState.commitLocalSnapshot preparedInitial
  (withRemoteOne, _) <-
    applyRemote
      "remote revision one"
      (fullSnapshot remoteOwner Placement.firstPlacementSequence [remoteRouteOne])
      withLocalOne
  firstVector <-
    checked
      "first placement vector"
      ( PlacementState.currentPhysicalPlacementRevisionVector
          membership
          withRemoteOne
      )
  assertEqual
    "the placement cut retains its exact membership generation"
    (heraldMembershipGenerationId membership)
    (physicalPlacementRevisionMembershipGenerationId firstVector)
  firstProjection <-
    checked
      "first placement projection"
      (PlacementState.placementRoutesAtVector firstVector withRemoteOne)
  assertEqual
    "the first vector reconstructs both exact owner projections"
    (sortOn fst [(localOwner, []), (remoteOwner, [remoteRouteOne])])
    firstProjection

  preparedLocalRoute <-
    checked
      "install changed local route"
      ( PlacementState.prepareSystemViewPlacements
          [systemViewPlacement localOwner NeutralVertexRole]
          withRemoteOne
      )
  let withChangedLocal =
        PlacementState.commitSystemViewPlacements preparedLocalRoute
  preparedLocalTwo <-
    checked
      "local revision two"
      (PlacementState.prepareLocalSnapshot withChangedLocal)
  let (withLocalTwo, localTwoResult) =
        PlacementState.commitLocalSnapshot preparedLocalTwo
  assertEqual
    "the changed local route advances exactly once"
    (Placement.nextPlacementSequence Placement.firstPlacementSequence)
    ( Placement.placementSnapshotSequence
        (PlacementState.localSnapshotMessage localTwoResult)
    )
  let remoteRevisionTwo =
        Placement.nextPlacementSequence Placement.firstPlacementSequence
  (withRemoteTwo, _) <-
    applyRemote
      "remote revision two"
      ( fullSnapshot
          remoteOwner
          remoteRevisionTwo
          [remoteRouteOne, remoteRouteTwo]
      )
      withLocalTwo
  retainedFirst <-
    checked
      "retained first placement projection"
      (PlacementState.placementRoutesAtVector firstVector withRemoteTwo)
  assertEqual
    "later revisions do not overwrite the first vector"
    firstProjection
    retainedFirst
  secondVector <-
    checked
      "second placement vector"
      ( PlacementState.currentPhysicalPlacementRevisionVector
          membership
          withRemoteTwo
      )
  secondProjection <-
    checked
      "second placement projection"
      (PlacementState.placementRoutesAtVector secondVector withRemoteTwo)
  assertEqual
    "the current vector reconstructs both revision-two projections"
    ( sortOn
        fst
        [ (localOwner, [localRoute]),
          (remoteOwner, sortOn Placement.deltaRouteDelta [remoteRouteOne, remoteRouteTwo])
        ]
    )
    secondProjection

propHistoricalPlacementRetention :: [Bool] -> Bool -> Property
propHistoricalPlacementRetention nonemptyRevisions reverseInput =
  case prepare of
    Left problem -> QC.counterexample (show problem) False
    Right (before, retained, replayed, reordered, localSnapshot) ->
      QC.conjoin
        [ QC.counterexample "history never changes current route entries"
            $ PlacementState.currentPlacementRouteEntries retained
              === PlacementState.currentPlacementRouteEntries before,
          QC.counterexample "history never changes the current placement vector"
            $ PlacementState.currentPhysicalPlacementRevisionVector membership retained
              === PlacementState.currentPhysicalPlacementRevisionVector membership before,
          QC.counterexample "history never changes live owners"
            $ PlacementState.remotePlacementOwners retained
              === PlacementState.remotePlacementOwners before,
          QC.counterexample "remote snapshots, history and wire advertisement do not change application-slot readiness"
            $ QC.conjoin [fst (PlacementState.takePreparationPlacementChanges state) === Set.empty | state <- [before, retained, replayed, reordered]],
          QC.counterexample "exact history replay changes no state" (replayed === retained),
          QC.counterexample "retention order does not matter" (reordered === retained),
          QC.counterexample "capture includes every coordinate, including empty cuts"
            $ PlacementState.retainedPlacementSnapshots retained
              === sortOn snapshotCoordinate (localSnapshot : liveSnapshot : historical),
          QC.counterexample "the retained placement state validates"
            $ PlacementState.validatePlacementState retained === Right ()
        ]
  where
    localOwner = checkedLocalHeraldEpoch fixtureCheckedGenesis
    remoteOwner = heraldMemberEpoch fixtureRemoteMember
    membership = placementMembership (localOwner :| [remoteOwner])
    revisions = iterate Placement.nextPlacementSequence Placement.firstPlacementSequence
    -- Always include an empty historical snapshot: route-only export would
    -- silently drop its exact revision even though an old vector needs it.
    historical =
      zipWith
        (\revision nonempty -> snapshotAt remoteOwner revision [systemViewRoute remoteOwner SortDefinitionRole | nonempty])
        revisions
        (False : nonemptyRevisions)
    liveSnapshot =
      snapshotAt
        remoteOwner
        (revisions !! length historical)
        [systemViewRoute remoteOwner EdgeRole]
    supplied = if reverseInput then reverse historical else historical
    snapshotCoordinate snapshot =
      (Placement.placementSnapshotOwner snapshot, Placement.placementSnapshotSequence snapshot)
    prepare = do
      local <- PlacementState.prepareLocalSnapshot (PlacementState.initialState localOwner)
      let (advertised, localResult) = PlacementState.commitLocalSnapshot local
      remote <-
        PlacementState.prepareRemotePlacement
          (Placement.FullPlacementSnapshot liveSnapshot)
          advertised
      let (before, _) = PlacementState.commitRemotePlacement remote
      retained <- foldM (flip PlacementState.retainHistoricalPlacementSnapshot) before supplied
      replayed <- foldM (flip PlacementState.retainHistoricalPlacementSnapshot) retained supplied
      reordered <- foldM (flip PlacementState.retainHistoricalPlacementSnapshot) before (reverse supplied)
      Right (before, retained, replayed, reordered, PlacementState.localSnapshotMessage localResult)

caseRetiredHistoricalPlacement :: Assertion
caseRetiredHistoricalPlacement = do
  let localOwner = checkedLocalHeraldEpoch fixtureCheckedGenesis
      owner = heraldMemberEpoch fixtureRemoteMember
      first = snapshotAt owner Placement.firstPlacementSequence [systemViewRoute owner SortDefinitionRole]
      historical =
        snapshotAt
          owner
          (Placement.nextPlacementSequence Placement.firstPlacementSequence)
          [systemViewRoute owner EdgeRole]
  (projected, _) <-
    applyRemote "initial remote snapshot" (Placement.FullPlacementSnapshot first) (PlacementState.initialState localOwner)
  retirement <- checked "retire remote owner" (PlacementState.prepareRemotePlacementRetirement owner projected)
  let (retired, _) = PlacementState.commitRemotePlacementRetirement retirement
  retained <- checked "retain withdrawn owner history" (PlacementState.retainHistoricalPlacementSnapshot historical retired)
  assertBool "owner remains permanently retired" (PlacementState.remotePlacementOwnerRetired owner retained)
  assertEqual "history does not restore a sequence" Nothing (PlacementState.remotePlacementSequence owner retained)
  assertEqual "history does not restore live routes" [] (PlacementState.remotePlacementRoutes owner retained)
  assertEqual "history does not restore live owners" [] (PlacementState.remotePlacementOwners retained)
  assertEqual "retired owner history is exportable" [first, historical] (PlacementState.retainedPlacementSnapshots retained)
  assertEqual "remote retirement and retained history do not change local application-slot readiness" Set.empty (fst (PlacementState.takePreparationPlacementChanges retained))
  assertEqual
    "history is available at its exact coordinate"
    (Just (Placement.placementSnapshotRoutes historical))
    (PlacementState.placementRoutesAtRevision owner (Placement.placementSnapshotSequence historical) retained)
  case PlacementState.prepareRemotePlacement (Placement.FullPlacementSnapshot historical) retained of
    Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementRetiredRemoteOwner actual)) ->
      assertEqual "live admission still rejects the retired owner" owner actual
    Left problem -> assertFailure ("unexpected retired history problem: " <> show problem)
    Right _ -> assertFailure "retained history reactivated a retired owner"
  checked "retired history invariant" (PlacementState.validatePlacementState retained)

caseLocalHistoricalPlacement :: Assertion
caseLocalHistoricalPlacement = do
  let owner = checkedLocalHeraldEpoch fixtureCheckedGenesis
  prepared <- checked "local empty snapshot" (PlacementState.prepareLocalSnapshot (PlacementState.initialState owner))
  let (advertised, result) = PlacementState.commitLocalSnapshot prepared
      local = PlacementState.localSnapshotMessage result
      conflicting = snapshotAt owner Placement.firstPlacementSequence [systemViewRoute owner EdgeRole]
      unknown = snapshotAt owner (Placement.nextPlacementSequence Placement.firstPlacementSequence) []
  assertEqual
    "exact local history is corroborated without mutation"
    (Right advertised)
    (PlacementState.retainHistoricalPlacementSnapshot local advertised)
  assertEqual
    "foreign history cannot replace local content"
    (Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementSequenceConflict owner Placement.firstPlacementSequence)))
    (PlacementState.retainHistoricalPlacementSnapshot conflicting advertised)
  assertEqual
    "foreign history cannot create a local coordinate"
    (Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementRemoteOwnerIsLocal owner)))
    (PlacementState.retainHistoricalPlacementSnapshot unknown advertised)

caseHistoricalPlacementAdmission :: Assertion
caseHistoricalPlacementAdmission = do
  let owner = heraldMemberEpoch fixtureRemoteMember
      initial = PlacementState.initialState (checkedLocalHeraldEpoch fixtureCheckedGenesis)
      revision = Placement.nextPlacementSequence Placement.firstPlacementSequence
      route = systemViewRoute owner SortDefinitionRole
      historical = snapshotAt owner revision [route]
      conflicting = snapshotAt owner revision [systemViewRoute owner EdgeRole]
  retained <- checked "retain history before live admission" (PlacementState.retainHistoricalPlacementSnapshot historical initial)
  assertEqual "history does not admit an owner" [] (PlacementState.remotePlacementOwners retained)
  assertEqual "history does not admit a sequence" Nothing (PlacementState.remotePlacementSequence owner retained)
  assertEqual "history does not advertise routes" [] (PlacementState.currentPlacementRouteEntries retained)
  assertEqual
    "unequal history at an existing coordinate is rejected"
    (Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementSequenceConflict owner revision)))
    (PlacementState.retainHistoricalPlacementSnapshot conflicting retained)
  (live, _) <- applyRemote "first live projection" (fullSnapshot owner Placement.firstPlacementSequence []) retained
  case PlacementState.prepareRemotePlacement (Placement.FullPlacementSnapshot conflicting) live of
    Left (PlacementState.PlacementProtocolProblem (PlacementState.PlacementSequenceConflict actualOwner actualRevision)) ->
      assertEqual "live admission cannot overwrite a retained coordinate" (owner, revision) (actualOwner, actualRevision)
    Left problem -> assertFailure ("unexpected historical admission problem: " <> show problem)
    Right _ -> assertFailure "live admission overwrote exact historical evidence"
  (advanced, result) <- applyRemote "exact retained coordinate becomes live" (Placement.FullPlacementSnapshot historical) live
  assertEqual "ordinary admission advances the live projection" PlacementState.RemotePlacementAccepted (PlacementState.remotePlacementDisposition result)
  assertEqual "exact admitted history becomes live" [route] (PlacementState.remotePlacementRoutes owner advanced)
  checked "historical admission invariant" (PlacementState.validatePlacementState advanced)

placementMembership :: NonEmpty HeraldEpoch -> HeraldMembershipGeneration
placementMembership members =
  checkedValue
    "placement membership generation"
    ( genesisHeraldMembershipGeneration
        (checkedSystemId fixtureCheckedGenesis)
        members
    )

systemViewRoute :: HeraldEpoch -> PredefinedSortRole -> Placement.DeltaRoute
systemViewRoute owner role =
  Placement.privateSystemViewDeltaRoute
    role
    (deriveSystemViewDeltaId system owner role)
    sortId
    (deriveSortDefinitionOccurrenceId system sortId Genesis)
    (deriveSystemViewStoreIncarnationId system owner role)
  where
    system = checkedSystemId fixtureCheckedGenesis
    sortId = profileSortFor role

systemViewPlacement :: HeraldEpoch -> PredefinedSortRole -> PlacementState.SystemViewPlacement
systemViewPlacement owner role =
  PlacementState.systemViewPlacement
    role
    (Placement.deltaRouteDelta route)
    (Placement.deltaRouteSortId route)
    (Placement.deltaRouteOccurrenceId route)
    owner
    (Placement.deltaRouteStoreIncarnation route)
  where
    route = systemViewRoute owner role

fullSnapshot :: HeraldEpoch -> Placement.PlacementSequence -> [Placement.DeltaRoute] -> Placement.PlacementUpdate
fullSnapshot owner sequenceNumber routes =
  Placement.FullPlacementSnapshot (snapshotAt owner sequenceNumber routes)

snapshotAt :: HeraldEpoch -> Placement.PlacementSequence -> [Placement.DeltaRoute] -> Placement.PlacementSnapshot
snapshotAt owner sequenceNumber routes =
  checkedValue "placement snapshot" (Placement.placementSnapshot owner sequenceNumber routes)

applyRemote ::
  String ->
  Placement.PlacementUpdate ->
  PlacementState.State ->
  IO (PlacementState.State, PlacementState.RemotePlacementResult)
applyRemote context update state = do
  prepared <- checked context (PlacementState.prepareRemotePlacement update state)
  pure (PlacementState.commitRemotePlacement prepared)

sequenceValue :: Placement.PlacementSequence -> Integer
sequenceValue = fromIntegral . Placement.placementSequenceWord64

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id
