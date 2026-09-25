{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Step15MembershipAdvanceProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( HeraldPublicationPrefix (EmptyHeraldPublicationPrefix),
    firstPlacementRevision,
    mkContextClassGenerationId,
    physicalPlacementRevisionVector,
  )
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Context (ContextGraph, deriveContextGraph)
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve),
    VertexId (DeltaVertex),
    edgePayload,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    PublicationId,
    TopologyCutId,
    controlIndex,
    mkHeraldEpoch,
    mkHeraldId,
    mkStructuralSequence,
    mkTopologyCutId,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Startup
  ( appliedBootstrapManifestId,
    appliedProcessEpochId,
    appliedProcessResidence,
  )
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationPlan,
    AlignmentGenerationPreparation (AlignmentGenerationReady),
    GenerationMemberInput,
    alignmentGenerationCut,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    generationMemberInput,
    prepareAlignmentGenerationPlan,
  )
import Eclips.Herald.Alignment.Loss qualified as AlignmentLoss
import Eclips.Herald.Alignment.Protocol
  ( AlignmentControl (AlignmentCancelled, AlignmentCutAcceptanceAdvertised),
    alignmentCutAccepted,
  )
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Discovery
  ( BindingAdmission (FirstBinding),
    PeerBinding,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    knownHerald,
    peerAddress,
    peerBindingRemoteHeraldEpoch,
    peerCandidate,
    peerDialIntentHeraldEpoch,
    peerHello,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedInitialBootstraps,
    DeploymentManifest (..),
    HeraldMember (..),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
    heraldMemberEpoch,
    heraldMemberId,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    checkedCatalogueDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input (PeerControl (PeerAlignmentControl))
import Eclips.Herald.OracleClient
  ( OracleClientAction (CancelOracleRetry, ConnectAndHelloOracle),
    OracleClientIngress (OracleHelloReceived),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerPayload qualified as PeerPayload
import Eclips.Herald.PeerStream
  ( mkPeerDispatchBindingGeneration,
    mkStreamDirection,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementMessage
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldStartupInvariant),
    StartupInvariantSubject (StartupStatic),
    StartupInvariantViolation (OracleClientOwnerInvariant, PlacementFactInvariant),
    validateHeraldState,
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupDiscoveryState,
    replaceStartupOracleClientState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    startupAlignmentState,
    startupApplicationState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    StructuralDebtKind (TopologyAlignmentDebt),
    normalizeStructuralDebts,
    sortOccurrence,
    structuralConsequenceDebt,
    structuralDebtEvidence,
    structuralDebtKey,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureCheckedInitialBootstrapsFor,
    fixtureDeploymentManifest,
    fixtureGeneratorSeed,
    fixtureHeraldAfterProbePrefix,
    fixtureHeraldRetirementReason,
    fixtureLocalMember,
    fixtureMembers,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteMember,
    fixtureRetirementResolution,
  )
import OracleAdvanceProperties (heldControlIndexTwoForMembershipAdvance)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Step-15 membership advance composition"
    [ testCase "survivor retirement advances every pure owner atomically and replay is inert" caseRetirementComposition,
      testCase "retirement projects one final surviving-peer alignment cancellation" caseSurvivingPeerLossCancellation,
      testCase "retirement explicitly cancels a pending destination dial" casePendingDialCancellation,
      testCase "retirement releases a survivor control wait in its installed origin generation" caseRetirementReleasesSurvivorStructuralDependency,
      testCase "local retirement retains control waiters and emits cleanup only" caseLocalRetirementRetainsControlDependency,
      testCase "local retirement drains Oracle work and applies resident Ends" caseLocalRetirement
    ]

caseRetirementReleasesSurvivorStructuralDependency :: Assertion
caseRetirementReleasesSurvivorStructuralDependency = do
  (held, publication, _, retiredPeer) <- heldControlIndexTwoForMembershipAdvance
  assertPublicationDisposition "publication starts held" Publication.DependencyHeld publication held
  let index = controlIndex 2
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState held))
      successorMembership =
        checkedValue
          "control-release membership successor"
          (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) retiredPeer membership)
  prepared <-
    checked
      "retire member at the held publication prerequisite"
      (MembershipAdvance.prepareMembershipAdvance index successorMembership [] held)
  let successor = MembershipAdvance.commitMembershipAdvance prepared
  record <- case Publication.lookupIncomingPublication publication (startupPublicationState successor) of
    Nothing -> assertFailure "survivor structural publication disappeared"
    Just retained -> pure retained
  assertEqual
    "the satisfied control wait leaves no invented base dependency for an origin stamp"
    Set.empty
    (Publication.incomingPublicationDependencies record)
  assertEqual
    "retained survivor work materializes in its still-installed origin generation"
    Publication.Applied
    (Publication.incomingPublicationDisposition record)
  assertEqual "released successor remains composed-valid" (Right ()) (validateHeraldState successor)

caseLocalRetirementRetainsControlDependency :: Assertion
caseLocalRetirementRetainsControlDependency = do
  (held, publication, _, _) <- heldControlIndexTwoForMembershipAdvance
  assertPublicationDisposition "publication starts held" Publication.DependencyHeld publication held
  let index = controlIndex 2
      local = checkedLocalHeraldEpoch (startupGenesis held)
      projection = startupOracleProjectionState held
      view = OracleProjection.oracleView projection
      membership = OracleProjection.oracleViewCurrentHeraldMembership view
      successorMembership =
        checkedValue
          "local control-release membership successor"
          (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) local membership)
      ends = residentEndsAt index local projection
  prepared <-
    checked
      "retire local member with a held control dependency"
      (MembershipAdvance.prepareMembershipAdvance index successorMembership ends held)
  let successor = MembershipAdvance.commitMembershipAdvance prepared
      effects = effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects prepared)
  assertPublicationDisposition "local retirement does not release semantic work" Publication.DependencyHeld publication successor
  assertBool "local retirement still emits cleanup/cancellation only" (all isLocalCleanup effects)
  assertEqual "self-fenced successor remains composed-valid" (Right ()) (validateHeraldState successor)

residentEndsAt ::
  ControlIndex ->
  HeraldEpoch ->
  OracleProjection.State ->
  [OracleProjection.MembershipAdvanceProcessEnd]
residentEndsAt index residence projection =
  [ OracleProjection.membershipAdvanceProcessEnd process index fixtureHeraldRetirementReason
  | process <- OracleProjection.projectedProcessEpochs projection,
    OracleProjection.oracleViewProcessResidence process view == Just residence,
    OracleProjection.oracleViewProcessIsLive process view
  ]
  where
    view = OracleProjection.oracleView projection

isLocalCleanup :: HeraldEffect -> Bool
isLocalCleanup = \case
  DisposeApplicationSession {} -> True
  CancelTimer {} -> True
  CancelPeerDial {} -> True
  ClosePeerBinding {} -> True
  CancelPeerDestination {} -> True
  RunOracleClientAction (CancelOracleRetry _) -> True
  _ -> False

assertPublicationDisposition ::
  String ->
  Publication.IncomingDisposition ->
  PublicationId ->
  HeraldState ->
  Assertion
assertPublicationDisposition context expected publication state =
  case Publication.lookupIncomingPublication publication (startupPublicationState state) of
    Nothing -> assertFailure (context <> ": incoming publication is absent")
    Just record ->
      assertEqual context expected (Publication.incomingPublicationDisposition record)

caseLocalRetirement :: Assertion
caseLocalRetirement = do
  let bootstraps = fixtureCheckedInitialBootstrapsFor fixtureCheckedGenesis
      initial =
        fixtureHeraldAfterProbePrefix
          $ fst
            ( checkedValue
                "local-retirement initialization"
                ( initialHerald
                    (monotonicInstant 0)
                    fixtureCheckedGenesis
                    bootstraps
                    fixtureOracleContacts
                    fixtureGeneratorSeed
                    fixtureApplicationRecoveryConfiguration
                    fixturePeerRecoveryConfiguration
                )
            )
      local = heraldMemberEpoch fixtureLocalMember
      remote = heraldMemberEpoch fixtureRemoteMember
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState initial))
      index = controlIndex 2
      successor = checkedValue "local retirement successor" (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) local membership)
      localBootstraps =
        filter
          ((== local) . appliedProcessResidence)
          (checkedInitialBootstraps bootstraps)
  bootstrap <- case localBootstraps of
    [value] -> pure value
    observed -> assertFailure ("expected one local resident bootstrap, got " <> show (length observed))
  bound <- bindOracle initial
  (withRemoteBinding, remoteBinding) <- bindFixturePeer fixtureRemoteMember fixtureCheckedGenesis bootstraps bound
  let process = appliedProcessEpochId bootstrap
      attachment = Session.applicationAttachmentForBootstrap (appliedBootstrapManifestId bootstrap)
  opened <-
    checked
      "open local resident application session"
      ( Application.prepareApplicationSessionOpen
          local
          attachment
          (Session.clientNonce 1)
          (startupApplicationState withRemoteBinding)
      )
  let (application, acceptance) = Application.commitApplicationSessionAcceptance opened
      session = Session.sessionBindingSessionId (Session.sessionAcceptanceBinding acceptance)
      predecessor = replaceStartupApplicationState application withRemoteBinding
      ends = [OracleProjection.membershipAdvanceProcessEnd process index fixtureHeraldRetirementReason]
      remoteDirection = checkedValue "local-retirement peer direction" (mkStreamDirection local remote)
  assertEqual
    "the local-retirement predecessor is a valid composed state"
    (Right ())
    (validateHeraldState predecessor)
  prepared <-
    checked
      "prepare local membership retirement"
      (MembershipAdvance.prepareMembershipAdvance index successor ends predecessor)
  assertEqual
    "local retirement disposition"
    (MembershipAdvance.MembershipAdvanceApplied local True)
    (MembershipAdvance.preparedMembershipAdvanceDisposition prepared)
  let effects = effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects prepared)
      retired = MembershipAdvance.commitMembershipAdvance prepared
      clientWitness = OracleClient.oracleClientStateWitness (startupOracleClientState retired)
  assertBool "Oracle client is permanently retired" (OracleClient.oracleClientWitnessRetired clientWitness)
  assertEqual "retired Oracle client offers no work" [] (OracleClient.oracleClientActions (startupOracleClientState retired))
  assertEqual "resident process session is removed" Nothing (Application.applicationSessionProcess session (startupApplicationState retired))
  assertEqual "resident process attachment is removed" Nothing (Application.applicationAttachmentProcess attachment (startupApplicationState retired))
  assertEqual
    "the exact supplied resident End is retained"
    [process]
    (fmap fst (OracleProjection.projectedEndedProcesses (startupOracleProjectionState retired)))
  assertBool
    "the exact application session is disposed"
    (any (\case DisposeApplicationSession observed _ -> observed == session; _ -> False) effects)
  assertBool "survivor destination cancellation is explicit" (CancelPeerDestination remote `elem` effects)
  assertBool "the retired local epoch is not an outgoing destination" (CancelPeerDestination local `notElem` effects)
  assertBool "every current peer binding is explicitly closed" (ClosePeerBinding remoteBinding `elem` effects)
  assertEqual
    "the old local epoch retains no live remote binding"
    Nothing
    (Discovery.currentPeerBinding (heraldMemberEpoch fixtureRemoteMember) (startupDiscoveryState retired))
  assertEqual
    "the old local epoch retains no peer-stream binding"
    Nothing
    =<< checked
      "local-retirement peer-stream binding"
      (PeerStream.currentDispatchBinding remoteDirection (startupPeerStreamState retired))
  assertEqual
    "the old local epoch retains no peer-stream ticket"
    Nothing
    =<< checked
      "local-retirement peer-stream ticket"
      (PeerStream.currentDispatchTicket remoteDirection (startupPeerStreamState retired))
  assertEqual
    "the old local epoch retains no peer-stream attempt"
    Nothing
    =<< checked
      "local-retirement peer-stream attempt"
      (PeerStream.currentDispatchAttempt remoteDirection (startupPeerStreamState retired))
  assertEqual
    "the surviving remote epoch is not falsely tombstoned"
    False
    (PeerStream.peerStreamPeerRetired remote (startupPeerStreamState retired))
  assertBool "local retirement emits only cleanup/cancellation" (all localCleanupEffect effects)
  assertEqual
    "the retired structural-progress owner remains internally coherent"
    (Right ())
    (GraphProgress.validateStructuralProgressState (startupStructuralProgressState retired))
  assertEqual
    "retired graph vertices match structural reconciliation"
    (Graph.graphStructuralVertexProjections (startupGraphState retired))
    (Reconciliation.structuralAppliedVertexProjections (GraphProgress.structuralProgressReconciliation (startupStructuralProgressState retired)))
  assertEqual
    "retired graph edges match structural reconciliation"
    (Graph.graphStructuralEdgeProjections (startupGraphState retired))
    (Reconciliation.structuralAppliedEdgeProjections (GraphProgress.structuralProgressReconciliation (startupStructuralProgressState retired)))
  assertEqual
    "the End advances the structural control prefix"
    index
    (GraphProgress.structuralAppliedControlPrefix (startupStructuralProgressState retired))
  assertEqual
    "a predecessor Bound Oracle client is invalid after local retirement"
    (Left (HeraldStartupInvariant StartupStatic OracleClientOwnerInvariant))
    ( validateHeraldState
        ( replaceStartupOracleClientState
            (startupOracleClientState predecessor)
            retired
        )
    )
  assertEqual "the locally retired whole state remains coherent" (Right ()) (validateHeraldState retired)
  where
    localCleanupEffect = \case
      DisposeApplicationSession {} -> True
      CancelTimer {} -> True
      CancelPeerDial {} -> True
      ClosePeerBinding {} -> True
      CancelPeerDestination {} -> True
      RunOracleClientAction (CancelOracleRetry _) -> True
      _ -> False

bindFixturePeer ::
  HeraldMember ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  HeraldState ->
  IO (HeraldState, PeerBinding)
bindFixturePeer member genesis bootstraps state = do
  let peerId = heraldMemberId member
      peer = heraldMemberEpoch member
      nonce = connectionNonce 9
      candidate = peerCandidate peerId peer nonce
      hello =
        peerHello
          (checkedSystemId genesis)
          peerId
          peer
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest genesis)
          (checkedInitialProjectionDigest bootstraps)
          Nothing
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState state))
  (successor, _) <-
    checked
      "apply fixture peer Hello through the composed owner"
      ( PeerControl.applyPeerHello
          candidate
          Set.empty
          hello
          (heraldMembershipGenerationId membership)
          (heraldMembershipGenerationActiveMemberSetDigest membership)
          Nothing
          state
      )
  case Discovery.currentPeerBinding peer (startupDiscoveryState successor) of
    Just binding -> pure (successor, binding)
    Nothing -> assertFailure "fixture peer binding was not admitted"

caseRetirementComposition :: Assertion
caseRetirementComposition = do
  let fixture = fourHeraldFixture
  base <- bindOracle fixture.initial
  (boundH4, binding) <- bindPeer fixture.h4 base
  let withH4 = replaceStartupDiscoveryState boundH4 base
  (boundDiscovery, _) <- bindPeer fixture.h2 withH4
  streamWithWork <- peerWork fixture
  preparedLocalPlacement <-
    checked
      "prepare fixture local placement snapshot"
      (Placement.prepareLocalSnapshot (startupPlacementState base))
  let (withLocalPlacement, localPlacementResult) =
        Placement.commitLocalSnapshot preparedLocalPlacement
      localPlacementSnapshot = Placement.localSnapshotMessage localPlacementResult
      remoteRoutes = PlacementMessage.placementSnapshotRoutes localPlacementSnapshot
      remotePlacementSnapshot =
        checkedValue
          "prepare nonempty H4 placement snapshot"
          ( PlacementMessage.placementSnapshot
              fixture.h4
              PlacementMessage.firstPlacementSequence
              remoteRoutes
          )
  preparedRemotePlacement <-
    checked
      "retain nonempty H4 placement snapshot"
      ( Placement.prepareRemotePlacement
          (PlacementMessage.FullPlacementSnapshot remotePlacementSnapshot)
          withLocalPlacement
      )
  let (withRemotePlacement, _) =
        Placement.commitRemotePlacement preparedRemotePlacement
  let membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState base))
      evidencePlacement =
        checkedValue
          "pending-evidence membership vector"
          ( physicalPlacementRevisionVector
              membership
              [ (member, firstPlacementRevision)
              | member <-
                  NonEmpty.toList
                    (heraldMembershipGenerationActiveHeraldEpochs membership)
              ]
          )
      evidenceGeneration =
        checkedValue
          "pending-evidence generation"
          (mkContextClassGenerationId (bytes 0xa5))
      evidenceTopology =
        GraphProgress.structuralGenesisCutId (startupStructuralProgressState base)
      h4EvidenceControl =
        AlignmentCutAcceptanceAdvertised
          ( alignmentCutAccepted
              evidenceGeneration
              fixture.h4
              evidenceTopology
              evidencePlacement
              EmptyHeraldPublicationPrefix
          )
      h2EvidenceControl =
        AlignmentCutAcceptanceAdvertised
          ( alignmentCutAccepted
              evidenceGeneration
              fixture.h2
              evidenceTopology
              evidencePlacement
              EmptyHeraldPublicationPrefix
          )
      withH4Evidence =
        fst
          ( Alignment.commitPendingGenerationEvidence
              ( checkedValue
                  "retain H4-authored pending generation evidence"
                  ( Alignment.preparePendingGenerationEvidence
                      fixture.h4
                      h4EvidenceControl
                      (startupAlignmentState base)
                  )
              )
          )
      withSurvivorEvidence =
        fst
          ( Alignment.commitPendingGenerationEvidence
              ( checkedValue
                  "retain H2-authored pending generation evidence"
                  ( Alignment.preparePendingGenerationEvidence
                      fixture.h2
                      h2EvidenceControl
                      withH4Evidence
                  )
              )
          )
      predecessor =
        replaceStartupAlignmentState withSurvivorEvidence
          . replaceStartupPlacementState withRemotePlacement
          . replaceStartupPeerStreamState streamWithWork
          . replaceStartupDiscoveryState boundDiscovery
          $ base
      h2Direction = checkedValue "H2 direction" (mkStreamDirection fixture.h1 fixture.h2)
      h4Direction = checkedValue "H4 direction" (mkStreamDirection fixture.h1 fixture.h4)
      expectedLostStores =
        Set.toAscList
          ( Set.fromList
              [ AlignmentLoss.qualifiedStoreCoordinate
                  fixture.h4
                  (PlacementMessage.deltaRouteDelta route)
                  (PlacementMessage.deltaRouteStoreIncarnation route)
              | route <- remoteRoutes
              ]
          )
      evidenceCoordinates evidence =
        ( Alignment.pendingGenerationEvidenceRemoteHerald evidence,
          Alignment.pendingGenerationEvidenceControl evidence
        )
  h2WorkBefore <- checked "H2 retained work before retirement" (PeerStream.activeOutgoingItems h2Direction streamWithWork)
  h4WorkBefore <- checked "H4 retained work before retirement" (PeerStream.activeOutgoingItems h4Direction streamWithWork)
  assertEqual "Oracle client retains the genuine probe prefix before retirement" (controlIndex 1) (OracleClient.oracleClientAppliedCursor (startupOracleClientState predecessor))
  prepared <-
    checked
      "prepare membership contraction"
      (MembershipAdvance.prepareMembershipAdvance fixture.index fixture.successor [] predecessor)
  assertEqual
    "fresh disposition"
    (MembershipAdvance.MembershipAdvanceApplied fixture.h4 False)
    (MembershipAdvance.preparedMembershipAdvanceDisposition prepared)
  let effects = effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects prepared)
      successor = MembershipAdvance.commitMembershipAdvance prepared
      projected = OracleProjection.oracleView (startupOracleProjectionState successor)
  assertEqual "Oracle membership advances" (heraldMembershipGenerationId fixture.successor) (OracleProjection.oracleViewCurrentHeraldMembershipId projected)
  assertEqual "Discovery membership advances" fixture.successor (Discovery.currentMembership (startupDiscoveryState successor))
  assertEqual "retired binding is absent" Nothing (Discovery.currentPeerBinding fixture.h4 (startupDiscoveryState successor))
  assertBool "PeerStream tombstones H4" (PeerStream.peerStreamPeerRetired fixture.h4 (startupPeerStreamState successor))
  assertBool "Placement tombstones H4" (Placement.remotePlacementOwnerRetired fixture.h4 (startupPlacementState successor))
  assertEqual
    "membership retirement receipts exactly the unactioned H4 generation evidence"
    [(fixture.h4, h4EvidenceControl)]
    ( fmap
        evidenceCoordinates
        (MembershipAdvance.preparedMembershipAdvanceRetiredGenerationEvidence prepared)
    )
  assertEqual
    "survivor-authored generation evidence remains pending"
    [(fixture.h2, h2EvidenceControl)]
    ( fmap
        evidenceCoordinates
        (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState successor))
    )
  assertEqual
    "the retired destination has no executable stream work"
    []
    =<< checked "H4 active work after retirement" (PeerStream.activeOutgoingItems h4Direction (startupPeerStreamState successor))
  assertEqual
    "the exact retired destination assignments are terminally settled"
    (length h4WorkBefore)
    (length (MembershipAdvance.preparedMembershipAdvanceSettledAssignments prepared))
  assertBool "the retirement fixture contains exact Store loss" (not (null expectedLostStores))
  assertEqual
    "membership retirement derives every exact H4 Store coordinate once"
    expectedLostStores
    (MembershipAdvance.preparedMembershipAdvanceLostStores prepared)
  assertEqual
    "terminal settlement remains distinct from peer completion"
    mempty
    =<< checked "H4 peer completions after retirement" (PeerStream.outgoingCompletionProgress h4Direction (startupPeerStreamState successor))
  case MembershipAdvance.preparedMembershipAdvanceRetiredPlacement prepared of
    Nothing -> assertFailure "fresh survivor retirement lost its placement-loss receipt"
    Just retiredPlacement ->
      do
        assertEqual
          "the exact retired placement owner is retained in the prepared transition"
          fixture.h4
          (Placement.retiredRemotePlacementOwner retiredPlacement)
        assertEqual
          "the complete retired projection is retained as loss evidence"
          remoteRoutes
          (Placement.retiredRemotePlacementRoutes retiredPlacement)
  assertEqual "the uncorrupted survivor successor is composed-valid" (Right ()) (validateHeraldState successor)
  assertEqual
    "omitting the permanent Placement cut is a whole-state contradiction"
    (Left (HeraldStartupInvariant StartupStatic PlacementFactInvariant))
    ( validateHeraldState
        ( replaceStartupPlacementState
            (startupPlacementState predecessor)
            successor
        )
    )
  assertBool "binding close is explicit" (ClosePeerBinding binding `elem` effects)
  assertBool "destination cancellation is explicit" (CancelPeerDestination fixture.h4 `elem` effects)
  assertBool
    "the survivor reoffers every post-prefix Oracle action"
    ( all
        (\action -> RunOracleClientAction action `elem` effects)
        (OracleClient.oracleClientActions (startupOracleClientState successor))
    )
  h2WorkAfter <- checked "H2 retained work after retirement" (PeerStream.activeOutgoingItems h2Direction (startupPeerStreamState successor))
  assertEqual "surviving peer work is unchanged" h2WorkBefore h2WorkAfter
  replay <- checked "exact membership replay" (MembershipAdvance.prepareMembershipAdvance fixture.index fixture.successor [] successor)
  assertEqual "replay disposition" MembershipAdvance.MembershipAdvanceExactDuplicate (MembershipAdvance.preparedMembershipAdvanceDisposition replay)
  assertEqual "replay emits no effects" [] (effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects replay))
  assertEqual
    "membership replay retires no generation evidence twice"
    []
    (MembershipAdvance.preparedMembershipAdvanceRetiredGenerationEvidence replay)
  assertBool "replay state is identical" (successor == MembershipAdvance.commitMembershipAdvance replay)

caseSurvivingPeerLossCancellation :: Assertion
caseSurvivingPeerLossCancellation = do
  let fixture = fourHeraldFixture
  base <- bindOracle fixture.initial
  (withH4Discovery, _) <- bindPeer fixture.h4 base
  let withH4 = replaceStartupDiscoveryState withH4Discovery base
  (boundDiscovery, _) <- bindPeer fixture.h2 withH4
  preparedLocalPlacement <-
    checked
      "prepare loss-effect local placement snapshot"
      (Placement.prepareLocalSnapshot (startupPlacementState base))
  let (withLocalPlacement, localPlacementResult) =
        Placement.commitLocalSnapshot preparedLocalPlacement
      localPlacementSnapshot = Placement.localSnapshotMessage localPlacementResult
      retiredRoutes = PlacementMessage.placementSnapshotRoutes localPlacementSnapshot
      retiredPlacementSnapshot =
        checkedValue
          "prepare loss-effect H4 placement snapshot"
          ( PlacementMessage.placementSnapshot
              fixture.h4
              PlacementMessage.firstPlacementSequence
              retiredRoutes
          )
  preparedRemotePlacement <-
    checked
      "retain loss-effect H4 placement snapshot"
      ( Placement.prepareRemotePlacement
          (PlacementMessage.FullPlacementSnapshot retiredPlacementSnapshot)
          withLocalPlacement
      )
  let (withRemotePlacement, _) =
        Placement.commitRemotePlacement preparedRemotePlacement
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState base))
  alignment <-
    lossEffectAlignmentOwner fixture membership retiredRoutes
  let predecessor =
        replaceStartupAlignmentState alignment
          . replaceStartupPlacementState withRemotePlacement
          . replaceStartupDiscoveryState boundDiscovery
          $ base
  assertEqual
    "the focused composed fixture starts from a valid alignment owner"
    (Right ())
    (Alignment.validateAlignmentState alignment)
  prepared <-
    checked
      "prepare surviving-peer loss-effect membership contraction"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successor
          []
          predecessor
      )
  let successor = MembershipAdvance.commitMembershipAdvance prepared
      effects = effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects prepared)
      alignmentCancellations =
        [ (peerBindingRemoteHeraldEpoch binding, cancellation)
        | SendPeerControl binding (PeerAlignmentControl (AlignmentCancelled cancellation)) <- effects
        ]
  assertEqual
    "the final batched loss delta projects exactly one cancellation to surviving H2"
    [fixture.h2]
    (fst <$> alignmentCancellations)
  assertBool
    "no alignment cancellation is projected to retired H4"
    (all ((/= fixture.h4) . fst) alignmentCancellations)
  assertBool
    "the owner-composed loss retires at least one exact alignment plan"
    (not (null (MembershipAdvance.preparedMembershipAdvanceAlignmentLossPlans prepared)))
  replay <-
    checked
      "replay surviving-peer loss-effect membership contraction"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successor
          []
          successor
      )
  assertEqual
    "loss-effect membership replay is silent"
    []
    (effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects replay))
  assertBool
    "loss-effect membership replay is state-identical"
    (successor == MembershipAdvance.commitMembershipAdvance replay)

casePendingDialCancellation :: Assertion
casePendingDialCancellation = do
  let fixture = fourHeraldFixture
  base <- bindOracle fixture.initial
  (boundH2, h2Binding) <- bindPeer fixture.h2 base
  let withH2 = replaceStartupDiscoveryState boundH2 base
      discovery = startupDiscoveryState withH2
      observation = knownHerald fixture.h4Id fixture.h4 (Set.singleton (peerAddress "h4.example:4040"))
  learned <- checked "learn H4 contact" (Discovery.prepareKnownContacts h2Binding [observation] discovery)
  let withDial = replaceStartupDiscoveryState (fst (Discovery.commitKnownContacts learned)) withH2
      h4Dials = filter ((== fixture.h4) . peerDialIntentHeraldEpoch) (Discovery.peerDialIntents (startupDiscoveryState withDial))
  assertEqual "one H4 dial is pending" 1 (length h4Dials)
  prepared <- checked "retire pending dial target" (MembershipAdvance.prepareMembershipAdvance fixture.index fixture.successor [] withDial)
  let effects = effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects prepared)
  assertBool "the exact pending dial is cancelled" (all (\dial -> CancelPeerDial dial `elem` effects) h4Dials)
  assertBool "destination cancellation remains explicit" (CancelPeerDestination fixture.h4 `elem` effects)

-- This fixture is deliberately owner-composed rather than whole-state
-- topology-composed. A surviving Herald cannot own an H4 destination
-- transcript: destination obligations are owned by their destination Herald,
-- while H4 takes the local-retirement path on H4 itself. The strongest
-- survivor-side schedule is therefore a local destination transcript sourced
-- from H2 whose shared plan coordinate also owns an H4 fresh-base import. H4
-- placement loss invalidates that coordinate and the final transfer delta must
-- project the one resulting cancellation to H2.
lossEffectAlignmentOwner ::
  FourHeraldFixture ->
  HeraldMembershipGeneration ->
  [PlacementMessage.DeltaRoute] ->
  IO Alignment.State
lossEffectAlignmentOwner fixture membership suppliedRoutes = do
  (localRoute, retiredRoute, survivorRoute) <-
    case take 3 distinctRoutes of
      [first, second, third] -> pure (first, second, third)
      observed ->
        assertFailure
          ( "loss-effect fixture requires three distinct Delta routes, got "
              <> show (length observed)
          )
  let localDelta = PlacementMessage.deltaRouteDelta localRoute
      localStore = PlacementMessage.deltaRouteStoreIncarnation localRoute
      retiredDelta = PlacementMessage.deltaRouteDelta retiredRoute
      retiredStore = PlacementMessage.deltaRouteStoreIncarnation retiredRoute
      survivorDelta = PlacementMessage.deltaRouteDelta survivorRoute
      survivorStore = PlacementMessage.deltaRouteStoreIncarnation survivorRoute
      affectedSort =
        sortOccurrence
          (PlacementMessage.deltaRouteSortId localRoute)
          (PlacementMessage.deltaRouteOccurrenceId localRoute)
      placement =
        checkedValue
          "loss-effect physical placement"
          ( physicalPlacementRevisionVector
              membership
              [ (member, firstPlacementRevision)
              | member <-
                  NonEmpty.toList
                    (heraldMembershipGenerationActiveHeraldEpochs membership)
              ]
          )
      topology =
        checkedValue
          "loss-effect topology cut"
          ( Topology.topologyCut
              ( Topology.sameGenerationPredecessor
                  ( checkedValue
                      "loss-effect topology predecessor"
                      (mkTopologyCutId (bytes 0xe1))
                  )
              )
              ( Topology.topologyFrontier
                  (Structural.emptyStructuralVersionVector membership)
                  (controlIndex 0)
              )
              (Topology.deriveTopologyOccurrenceDigest "step-15-membership-loss-effect")
          )
      topologyId = Topology.deriveTopologyCutId topology
      members =
        [ generationMemberInput
            localDelta
            localStore
            fixture.h1
            DomainAlignment.initialStoreRevision,
          generationMemberInput
            retiredDelta
            retiredStore
            fixture.h4
            DomainAlignment.initialStoreRevision,
          generationMemberInput
            survivorDelta
            survivorStore
            fixture.h2
            DomainAlignment.initialStoreRevision
        ]
      mergedContext =
        checkedValue
          "loss-effect merged context"
          ( deriveContextGraph
              [DeltaVertex localDelta, DeltaVertex retiredDelta, DeltaVertex survivorDelta]
              [ edgePayload (DeltaVertex localDelta) (DeltaVertex retiredDelta) Preserve,
                edgePayload (DeltaVertex retiredDelta) (DeltaVertex localDelta) Preserve,
                edgePayload (DeltaVertex retiredDelta) (DeltaVertex survivorDelta) Preserve,
                edgePayload (DeltaVertex survivorDelta) (DeltaVertex retiredDelta) Preserve
              ]
              [localDelta, retiredDelta, survivorDelta]
          )
      cause =
        structuralOccurrenceCause
          ( structuralOccurrenceId
              fixture.h1
              (checkedValue "loss-effect occurrence" (mkStructuralSequence 1))
          )
      plan =
        readyPlan affectedSort topology placement mergedContext members [] Set.empty
      generation = case alignmentGenerationPlanGenerations plan of
        [single] -> single
        observed -> error ("expected one merged loss-effect generation, got " <> show (length observed))
      withDebt = retainDebt cause affectedSort Alignment.emptyState
      promoted =
        commitPromotion
          fixture.h1
          cause
          affectedSort
          topologyId
          placement
          plan
          withDebt
      generationMembers =
        NonEmpty.toList
          (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation))
      withAcceptances =
        foldl'
          (\state member -> retainAcceptance generation member state)
          promoted
          generationMembers
      candidates =
        [ key
        | (key, _) <- Alignment.bootstrapImportEntries withAcceptances,
          Alignment.BootstrapFromFreshBase fresh <- [Alignment.bootstrapImportKeySource key],
          DomainAlignment.freshMemberBaseStoreIncarnation fresh == survivorStore
        ]
  key <- case candidates of
    [single] -> pure single
    observed ->
      assertFailure
        ( "loss-effect promotion produced an unexpected number of H2 fresh-base imports: "
            <> show (length observed)
        )
  preparedAttempt <-
    checked
      "select exact surviving H2 bootstrap source"
      (Alignment.prepareBootstrapImportAttempt key withAcceptances)
  let attempt = Alignment.preparedBootstrapImportAttempt preparedAttempt
  assertEqual
    "loss-effect fixture selects surviving H2"
    fixture.h2
    (Alignment.bootstrapImportAttemptSourceHerald attempt)
  let (owner, _) = Alignment.commitBootstrapImportAttempt preparedAttempt
      lost =
        AlignmentLoss.qualifiedStoreCoordinate
          fixture.h4
          retiredDelta
          retiredStore
  assertBool
    "the exact H4 placement loss reaches the H2-sourced plan"
    (not (Set.null (AlignmentLoss.alignmentLossRelevantCauses lost owner)))
  pure owner
  where
    distinctRoutes =
      Map.elems
        ( Map.fromList
            [ (PlacementMessage.deltaRouteDelta route, route)
            | route <- suppliedRoutes
            ]
        )

    readyPlan ::
      SortOccurrence ->
      Topology.TopologyCut ->
      DomainAlignment.PhysicalPlacementRevisionVector ->
      ContextGraph ->
      [GenerationMemberInput] ->
      [AlignmentGeneration] ->
      Set.Set DomainAlignment.ContextClassGenerationId ->
      AlignmentGenerationPlan
    readyPlan affectedSort topology placement context members prior certificates =
      case prepareAlignmentGenerationPlan
        affectedSort
        topology
        placement
        context
        members
        prior
        []
        certificates of
        Right (AlignmentGenerationReady plan) -> plan
        other -> error ("expected ready loss-effect generation plan, got " <> show other)

    retainDebt ::
      StructuralConsequenceCause ->
      SortOccurrence ->
      Alignment.State ->
      Alignment.State
    retainDebt cause affectedSort state =
      fst
        ( Alignment.commitStructuralDebtRetention
            ( checkedValue
                "retain loss-effect structural debt"
                ( Alignment.prepareStructuralDebtRetention
                    cause
                    ( normalizeStructuralDebts
                        [ structuralConsequenceDebt
                            ( structuralDebtKey
                                cause
                                TopologyAlignmentDebt
                                affectedSort
                                Nothing
                            )
                            (structuralDebtEvidence Set.empty Set.empty Set.empty)
                        ]
                    )
                    state
                )
            )
        )

    commitPromotion ::
      HeraldEpoch ->
      StructuralConsequenceCause ->
      SortOccurrence ->
      TopologyCutId ->
      DomainAlignment.PhysicalPlacementRevisionVector ->
      AlignmentGenerationPlan ->
      Alignment.State ->
      Alignment.State
    commitPromotion local cause affectedSort topology placement plan state =
      fst
        ( Alignment.commitAlignmentPromotion
            ( checkedValue
                "promote loss-effect generation plan"
                ( Alignment.prepareAlignmentPromotion
                    local
                    cause
                    affectedSort
                    topology
                    placement
                    plan
                    state
                )
            )
        )

    retainAcceptance ::
      AlignmentGeneration ->
      DomainAlignment.AlignmentMember ->
      Alignment.State ->
      Alignment.State
    retainAcceptance generation member state =
      fst
        ( Alignment.commitAlignmentCutAcceptance
            ( checkedValue
                "retain loss-effect cut acceptance"
                ( Alignment.prepareAlignmentCutAcceptance
                    ( AlignmentProtocol.alignmentCutAccepted
                        (alignmentGenerationId generation)
                        (DomainAlignment.alignmentMemberHerald member)
                        ( Topology.deriveTopologyCutId
                            (DomainAlignment.alignmentCutTopologyCut cut)
                        )
                        (DomainAlignment.alignmentCutPhysicalPlacementRevisionVector cut)
                        DomainAlignment.EmptyHeraldPublicationPrefix
                    )
                    state
                )
            )
        )
      where
        cut = alignmentGenerationCut generation

data FourHeraldFixture = FourHeraldFixture
  { initial :: HeraldState,
    genesis :: CheckedHeraldGenesis,
    h1 :: HeraldEpoch,
    h2 :: HeraldEpoch,
    h2Id :: HeraldId,
    h4Id :: HeraldId,
    h4 :: HeraldEpoch,
    index :: ControlIndex,
    successor :: HeraldMembershipGeneration
  }

fourHeraldFixture :: FourHeraldFixture
fourHeraldFixture =
  FourHeraldFixture
    { initial,
      genesis,
      h1,
      h2,
      h2Id,
      h4Id,
      h4,
      index,
      successor
    }
  where
    (h1Member, h2Member) = case fixtureMembers of
      [first, second] -> (first, second)
      _ -> error "base fixture must contain exactly two Heralds"
    h1 = heraldMemberEpoch h1Member
    h2 = heraldMemberEpoch h2Member
    h2Id = heraldMemberId h2Member
    h3 = HeraldMember (checkedValue "H3 id" (mkHeraldId (bytes 0x73))) (checkedValue "H3 epoch" (mkHeraldEpoch (bytes 0x83)))
    h4Id = checkedValue "H4 id" (mkHeraldId (bytes 0x74))
    h4 = checkedValue "H4 epoch" (mkHeraldEpoch (bytes 0x84))
    h4Member = HeraldMember h4Id h4
    members = fixtureMembers <> [h3, h4Member]
    baseOracle = deploymentOracleGenesis fixtureDeploymentManifest
    manifest =
      fixtureDeploymentManifest
        { deploymentActiveHeralds = members,
          deploymentOracleGenesis = baseOracle {oracleGenesisActiveHeralds = members}
        }
    genesis = checkedValue "four-Herald genesis" (checkHeraldGenesis manifest)
    bootstraps = checkedValue "empty four-Herald bootstraps" (checkInitialBootstraps genesis (PrimordialProcessManifest []))
    initial =
      fixtureHeraldAfterProbePrefix
        $ fst
          ( checkedValue
              "four-Herald initialization"
              ( initialHerald
                  (monotonicInstant 0)
                  genesis
                  bootstraps
                  fixtureOracleContacts
                  fixtureGeneratorSeed
                  fixtureApplicationRecoveryConfiguration
                  fixturePeerRecoveryConfiguration
              )
          )
    membership = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState initial))
    index = controlIndex 2
    successor = checkedValue "H4 retirement successor" (retireHeraldMembershipGeneration index (fixtureRetirementResolution index) h4 membership)

bindPeer :: HeraldEpoch -> HeraldState -> IO (Discovery.State, PeerBinding)
bindPeer peer state = do
  let fixture = fourHeraldFixture
      peerId
        | peer == fixture.h2 = fixture.h2Id
        | peer == fixture.h4 = fixture.h4Id
        | otherwise = error "unsupported bound peer"
      nonce = connectionNonce 7
      candidate = peerCandidate peerId peer nonce
      hello =
        peerHello
          (checkedSystemId fixture.genesis)
          peerId
          peer
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixture.genesis)
          (checkedInitialProjectionDigest (checkedValue "fixture bootstrap projection" (checkInitialBootstraps fixture.genesis (PrimordialProcessManifest []))))
          Nothing
  prepared <- checked "prepare peer Hello" (Discovery.preparePeerHello candidate hello (startupDiscoveryState state))
  let (discovery, disposition) = Discovery.commitPeerHello prepared
  case disposition of
    PeerHelloAccepted FirstBinding binding -> pure (discovery, binding)
    other -> assertFailure ("peer binding was not admitted: " <> show other)

peerWork :: FourHeraldFixture -> IO (PeerStream.State PeerPayload.PeerLogicalPayload)
peerWork fixture = do
  let initial = startupPeerStreamState fixture.initial
      binding = checkedValue "dispatch generation" (mkPeerDispatchBindingGeneration 1)
      probe = checkedValue "marker probe" (Disappearance.deriveDisappearanceProbeId (controlIndex 7))
      membership = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState fixture.initial))
      coordinate =
        Disappearance.admitDisappearanceSubjectMembershipCoordinate
          (checkedValue "marker subject" (Disappearance.mkDisappearanceSubjectDigest (bytes 0x93)))
          (heraldMembershipGenerationId membership)
          (heraldMembershipGenerationActiveMemberSetDigest membership)
      item = PeerPayload.peerLogicalDisappearanceProbeMarkerItem probe coordinate :| []
  bound <- foldBindings binding initial [fixture.h2, fixture.h4]
  prepared <- checked "enqueue surviving and retiring work" (PeerStream.prepareSequenceBoundEnqueue (Map.fromList [(fixture.h2, item), (fixture.h4, item)]) bound)
  pure (fst (PeerStream.commitEnqueue prepared))
  where
    foldBindings _ state [] = pure state
    foldBindings binding state (peer : rest) = do
      direction <- checked "dispatch direction" (mkStreamDirection fixture.h1 peer)
      prepared <- checked "dispatch binding" (PeerStream.prepareDispatchBinding direction binding state)
      foldBindings binding (fst (PeerStream.commitDispatchBinding prepared)) rest

bindOracle :: HeraldState -> IO HeraldState
bindOracle state
  | Just _ <- OracleClient.oracleClientCurrentBinding (startupOracleClientState state) = pure state
  | otherwise = do
      let client = startupOracleClientState state
      attempt <- case OracleClient.oracleClientActions client of
        [ConnectAndHelloOracle current _] -> pure current
        actions -> assertFailure ("expected one initial Oracle connection attempt, got " <> show actions)
      let node = oracleContactNode (oracleConnectAttemptContact attempt)
          acceptance = oracleHelloAcceptance node (oracleObservedTerm 1) (controlIndex 0) (Just node) True
      prepared <- checked "bind Oracle client" (OracleClient.prepareClientIngress (OracleHelloReceived attempt acceptance) client)
      pure (replaceStartupOracleClientState (OracleClient.commitClientIngress prepared) state)

bytes :: Word -> ByteString.ByteString
bytes byte = ByteString.replicate 32 (fromIntegral byte)

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id
