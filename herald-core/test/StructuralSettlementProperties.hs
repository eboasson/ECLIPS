{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module StructuralSettlementProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole,
    ApplicationStartupAccess,
    predefinedAccessRole,
    predefinedWriter,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateUniqueId,
    asPrivateNablaId,
  )
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Result
  ( OperationPendingReason (StructuralStabilizationPending),
    RegularCallResult (NewIdCompleted, WriteCompleted),
  )
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (WriteAccepted),
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    GlobalObjectId,
    PublicationId,
    StructuralOccurrenceId,
    controlIndex,
    globalObjectIdFromGlobalUniqueId,
    mkHeraldEpoch,
    mkHeraldId,
    nablaIdFromGlobalObjectId,
    publicationAuthorityEpoch,
    publicationNabla,
    sortIdBytes,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
  )
import Eclips.Domain.Membership
  ( heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Publication
  ( checkedPublicationId,
    checkedPublicationSort,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (EdgeCarrier, NeutralVertexCarrier),
  )
import Eclips.Domain.Sort.Profile qualified as Profile
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Request
  ( ApplicationOperation (NewIdApplication, WriteApplication),
    ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (Completed, OperationAccepted),
    requestId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationAttachment,
    ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened),
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    peerCandidate,
    peerHello,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchIsEmpty,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (configuredBootstrapManifestId),
    DeploymentManifest (..),
    HeraldMember (heraldMemberEpoch, heraldMemberId),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (PrimordialProcessManifest),
    checkHeraldGenesis,
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedCatalogueDigest,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( structuralAppliedReport,
    topologyCutAcceptance,
    topologyCutAnnounceId,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
    ApplicationSessionIngress (EndApplicationSession, OpenApplicationSession),
    HeraldInputBody (ApplicationRequestInput, ApplicationSessionInput, PeerInput),
    PeerControl (..),
    PeerIngress (PeerControlReceived, PeerHelloReceived),
    candidateApplicationLane,
    heraldInput,
    peerPublicationReceived,
  )
import Eclips.Herald.Join.State qualified as JoinState
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDispatch.Internal (peerPublicationItem)
import Eclips.Herald.PeerPublication
  ( peerPublicationBatch,
    publicationBatchId,
  )
import Eclips.Herald.PeerStream
  ( StreamDirection,
    StreamSequence,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamCompletionPrefix,
    streamPrefixSequence,
  )
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupJoinState,
    replaceStartupOracleProjectionState,
    replaceStartupPeerStreamState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupApplicationState,
    startupControlledState,
    startupGenesis,
    startupGraphState,
    startupJoinState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.OracleAdvance (structuralReconciliationViews)
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Canonical (canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Command (heraldAdmissionCommand, oracleEnvelope)
import Eclips.Oracle.Effect (OracleEffect (EmitAppliedOracleEntry), oracleEffects)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.State qualified as Oracle
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureConfiguredProcesses,
    fixtureDeploymentManifest,
    fixtureGeneratorSeed,
    fixtureHeraldMembershipGeneration,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
  )
import GenesisFixtures qualified as Fixtures
import PeerInputProperties qualified
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
    "structural application settlement"
    [ testCase "an established passive writer can be selected before topology-cut operation" caseSelectedPassiveWriter,
      testCase
        "a singleton write is pending, stamped, cut-stabilized, terminal, and retry-idempotent"
        caseSingletonSettlement,
      testCase
        "session end removes reply reachability without discarding later structural stabilization"
        caseSessionEndBeforeSettlement,
      testCase
        "installing a source topology cut releases held peer input exactly once"
        caseInstalledCutReleasesHeldPeerInput,
      testCase
        "a held Edge consumes no dot and sequences once its later endpoint applies"
        caseDeferredEdgeSequencesAfterEndpoint
    ]

data Opened = Opened
  { session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    access :: ApplicationStartupAccess
  }

caseSingletonSettlement :: Assertion
caseSingletonSettlement = do
  (genesis, bootstraps, bootstrap) <- singletonGenesisFixture
  initial <- initialize genesis bootstraps
  applicationAttachment <- attachment genesis bootstraps bootstrap
  (openedState, opened) <- open 1 applicationAttachment initial
  writer <- writerFor Access.NeutralVertexRole opened.access
  (reserved, newIdEffects) <-
    call
      2
      opened
      1
      (NewIdApplication (ControlledNewId writer))
      openedState
  privateObject <- completedNewId 1 newIdEffects
  let write =
        WriteApplication
          writer
          (PublishValue (neutralValue privateObject))
  (settled, writeEffects) <- call 3 opened 2 write reserved
  assertEqual
    "one transition first reports semantic acceptance and then terminal stabilization"
    [ OperationAccepted (requestId 2) StructuralStabilizationPending,
      Completed (requestId 2) (WriteCompleted WriteAccepted)
    ]
    (applicationReplyBodies writeEffects)
  occurrence <- soleStampedOccurrence settled
  stabilization <-
    maybe
      (assertFailure "singleton cut did not retain structural stabilization")
      pure
      ( Publication.lookupStructuralStabilization
          occurrence
          (startupPublicationState settled)
      )
  let progress = startupStructuralProgressState settled
      installedCut = Publication.structuralStabilizationCut stabilization
  assertBool
    "the stabilized cut is a dynamic successor of genesis"
    (installedCut /= GraphProgress.structuralGenesisCutId progress)
  assertEqual
    "the installed cut newly covers the stamped source occurrence"
    [occurrence]
    (GraphProgress.topologyCutNewlyCoveredOccurrences installedCut progress)
  assertEqual "settled whole state validates" (Right ()) (validateHeraldState settled)

  (retried, retryEffects) <- call 4 opened 2 write settled
  assertEqual
    "exact retry reoffers only the retained terminal result"
    [Completed (requestId 2) (WriteCompleted WriteAccepted)]
    (applicationReplyBodies retryEffects)
  assertBool
    "exact retry allocated no second structural dot or stabilization"
    (startupPublicationState settled == startupPublicationState retried)
  assertBool
    "exact retry emitted no second structural stream item"
    (startupPeerStreamState settled == startupPeerStreamState retried)
  assertBool
    "exact retry did not mutate the reconciled graph"
    (startupGraphState settled == startupGraphState retried)
  assertEqual
    "exact retry did not advance the applied structural vector"
    (GraphProgress.structuralAppliedVector progress)
    ( GraphProgress.structuralAppliedVector
        (startupStructuralProgressState retried)
    )

caseSessionEndBeforeSettlement :: Assertion
caseSessionEndBeforeSettlement = do
  bootstraps <- checkedBootstraps fixtureCheckedGenesis fixtureLocalBootstrapIds
  initial <- initialize fixtureCheckedGenesis bootstraps
  bootstrap <- case fixtureLocalBootstrapIds of
    first : _ -> pure first
    [] -> assertFailure "two-member fixture has no local bootstrap"
  applicationAttachment <- attachment fixtureCheckedGenesis bootstraps bootstrap
  (openedState, opened) <- open 10 applicationAttachment initial
  writer <- writerFor Access.NeutralVertexRole opened.access
  (reserved, newIdEffects) <-
    call
      11
      opened
      1
      (NewIdApplication (ControlledNewId writer))
      openedState
  privateObject <- completedNewId 1 newIdEffects
  (pending, pendingEffects) <-
    call
      12
      opened
      2
      ( WriteApplication
          writer
          (PublishValue (neutralValue privateObject))
      )
      reserved
  assertEqual
    "two-member write remains pending before the remote report"
    [OperationAccepted (requestId 2) StructuralStabilizationPending]
    (applicationReplyBodies pendingEffects)
  occurrence <- soleStampedOccurrence pending
  assertEqual
    "no topology cut has stabilized the occurrence yet"
    Nothing
    ( Publication.lookupStructuralStabilization
        occurrence
        (startupPublicationState pending)
    )
  (ended, endEffects) <-
    step
      13
      (ApplicationSessionInput (EndApplicationSession opened.binding opened.session))
      pending
  assertBool "session end is silent" (effectBatchIsEmpty endEffects)
  assertEqual
    "session end retires request reachability"
    []
    ( Application.applicationRequestEntries
        opened.session
        (startupApplicationState ended)
    )

  let beforeReport = startupStructuralProgressState ended
      remote = heraldMemberEpoch fixtureRemoteMember
      remoteReport =
        structuralAppliedReport
          remote
          (GraphProgress.structuralAppliedVector beforeReport)
          (GraphProgress.structuralAppliedControlPrefix beforeReport)
  preparedReport <-
    checkedIO
      "remote structural report"
      (GraphProgress.prepareStructuralReport remoteReport beforeReport)
  let (reportedProgress, _) = GraphProgress.commitStructuralReport preparedReport
      reportedState =
        replaceStartupStructuralProgressState reportedProgress ended
  (announcedState, _, prematurelyInstalled) <-
    checkedIO
      "open deterministic topology cut"
      (PeerControl.advanceLocalTopologyCutIfReady reportedState)
  assertEqual "two-member proposal cannot install before acceptance" [] prematurelyInstalled
  announce <-
    maybe
      (assertFailure "complete report matrix did not open a topology cut")
      pure
      (GraphProgress.structuralOpenCut (startupStructuralProgressState announcedState))
  let cutId = topologyCutAnnounceId announce
      remoteAcceptance = topologyCutAcceptance cutId remoteReport
  accepted <-
    checkedIO
      "remote topology acceptance"
      ( GraphProgress.prepareTopologyCutAcceptance
          remoteAcceptance
          (startupStructuralProgressState announcedState)
      )
  assertEqual
    "the remote acceptance completes fixed-member evidence"
    GraphProgress.TopologyCutAcceptanceCompletesSet
    (GraphProgress.preparedTopologyCutAcceptanceClassification accepted)
  established <-
    checkedIO
      "establish accepted topology cut"
      ( GraphProgress.prepareTopologyCutEstablishment
          (GraphProgress.commitTopologyCutAcceptance accepted)
      )
  let installedProgress =
        GraphProgress.commitTopologyCutEstablishment established
      installedState =
        replaceStartupStructuralProgressState installedProgress announcedState
  (settled, settlementEffects) <-
    checkedIO
      "settle installed cut after session end"
      ( StructuralCoordinator.settleInstalledStructuralCuts
          [cutId]
          installedState
      )
  assertBool
    "durable settlement emits no reply after its application session ended"
    (effectBatchIsEmpty settlementEffects)
  assertBool
    "the source publication remains durably stabilized"
    ( Publication.lookupStructuralStabilization
        occurrence
        (startupPublicationState settled)
        /= Nothing
    )
  (retried, retryEffects) <-
    checkedIO
      "idempotent settlement retry"
      ( StructuralCoordinator.settleInstalledStructuralCuts
          [cutId]
          settled
      )
  assertBool "settlement retry changes no owner" (settled == retried)
  assertBool "settlement retry emits no reply" (effectBatchIsEmpty retryEffects)
  assertEqual "ended-session settlement validates" (Right ()) (validateHeraldState settled)

caseInstalledCutReleasesHeldPeerInput :: Assertion
caseInstalledCutReleasesHeldPeerInput = do
  localBootstrap <- case fixtureLocalBootstrapIds of
    first : _ -> pure first
    [] -> assertFailure "source-topology fixture has no local bootstrap"
  bootstraps <-
    checkedBootstraps
      fixtureCheckedGenesis
      [localBootstrap, fixtureRemoteBootstrapId]
  initial <- initialize fixtureCheckedGenesis bootstraps
  (connected, peerBinding) <- connectRemotePeer bootstraps initial
  applicationAttachment <-
    attachment fixtureCheckedGenesis bootstraps localBootstrap
  (openedState, opened) <- open 2 applicationAttachment connected
  writer <- writerFor Access.NeutralVertexRole opened.access
  (reserved, newIdEffects) <-
    call
      3
      opened
      1
      (NewIdApplication (ControlledNewId writer))
      openedState
  privateObject <- completedNewId 1 newIdEffects
  (pending, _) <-
    call
      4
      opened
      2
      ( WriteApplication
          writer
          (PublishValue (neutralValue privateObject))
      )
      reserved

  let beforeReport = startupStructuralProgressState pending
      remote = heraldMemberEpoch fixtureRemoteMember
      remoteReport =
        structuralAppliedReport
          remote
          (GraphProgress.structuralAppliedVector beforeReport)
          (GraphProgress.structuralAppliedControlPrefix beforeReport)
  (announced, _) <-
    step
      5
      (PeerInput (PeerControlReceived peerBinding (PeerStructuralAppliedReported remoteReport)))
      pending
  announce <-
    maybe
      (assertFailure "complete report matrix did not open a topology cut")
      pure
      (GraphProgress.structuralOpenCut (startupStructuralProgressState announced))
  let cut = topologyCutAnnounceId announce
  item <- PeerInputProperties.fixturePublicationItemForTopologyCut cut
  (held, heldEffects) <-
    step
      6
      (PeerInput (peerPublicationReceived peerBinding (peerPublicationItem item)))
      announced
  let batch = peerPublicationBatch (sequencedItemPayload item)
      identifier = publicationBatchId batch
      direction = sequencedItemDirection item
  heldRecord <- requireIncomingPublication identifier held
  assertEqual
    "the not-yet-installed source cut is the sole dependency"
    (Set.singleton (Publication.SourceTopologyDependency cut))
    (Publication.incomingPublicationDependencies heldRecord)
  assertEqual
    "source-topology-held work remains semantically invisible"
    Publication.DependencyHeld
    (Publication.incomingPublicationDisposition heldRecord)
  assertBool
    "holding mutates no semantic destination owner"
    ( startupStoreState held == startupStoreState announced
        && startupGraphState held == startupGraphState announced
        && startupSortRegistryState held == startupSortRegistryState announced
        && startupStructuralProgressState held == startupStructuralProgressState announced
    )
  assertEqual
    "the sparse completion certificate replaces the separate receipt"
    []
    (receivedPrefixes peerBinding heldEffects)
  assertEqual
    "holding certifies receipt through the pending exception"
    [(direction, Just 1, Set.singleton 1)]
    [ (observedDirection, Receipt.receiptRetirementHighWater progress, Receipt.receiptRetirementExceptions progress)
    | SendPeerControl observedBinding (PeerStreamCompleted observedDirection progress) <- effectBatchMembers heldEffects,
      observedBinding == peerBinding
    ]
  assertEqual
    "holding cannot advance cumulative completion"
    [(direction, Nothing)]
    (completedPrefixes peerBinding heldEffects)

  let acceptance = topologyCutAcceptance cut remoteReport
  (installed, installEffects) <-
    step
      7
      (PeerInput (PeerControlReceived peerBinding (PeerTopologyCutAccepted acceptance)))
      held
  installedRecord <- requireIncomingPublication identifier installed
  assertEqual
    "cut installation releases and applies the held publication"
    Publication.Applied
    (Publication.incomingPublicationDisposition installedRecord)
  assertBool
    "the released structural publication becomes visible atomically"
    ( startupStoreState installed /= startupStoreState held
        && startupGraphState installed /= startupGraphState held
        && GraphProgress.structuralAppliedVector
          (startupStructuralProgressState installed)
          /= GraphProgress.structuralAppliedVector
            (startupStructuralProgressState held)
    )
  assertBool
    "the named source cut is installed before release escapes"
    ( GraphProgress.lookupInstalledTopologyCut
        cut
        (startupStructuralProgressState installed)
        /= Nothing
    )
  assertEqual
    "equal completion subsumes the cumulative receipt"
    []
    (receivedPrefixes peerBinding installEffects)
  assertEqual
    "successful release advances cumulative completion exactly once"
    [(direction, Just (sequencedItemSequence item))]
    (completedPrefixes peerBinding installEffects)
  assertEqual
    "a contiguous release needs no repair offer"
    0
    (repairOfferCount peerBinding installEffects)
  assertEqual
    "this publication releases no application wait"
    0
    (waitWakeCount installEffects)

  (retried, retryEffects) <-
    step
      8
      (PeerInput (PeerControlReceived peerBinding (PeerTopologyCutAccepted acceptance)))
      installed
  assertBool
    "an exact cut-acceptance retry changes no semantic owner"
    ( startupPublicationState retried == startupPublicationState installed
        && startupPeerStreamState retried == startupPeerStreamState installed
        && startupStoreState retried == startupStoreState installed
        && startupGraphState retried == startupGraphState installed
        && startupStructuralProgressState retried == startupStructuralProgressState installed
    )
  assertEqual
    "an exact cut retry cannot release another acknowledgement"
    []
    (receivedPrefixes peerBinding retryEffects <> completedPrefixes peerBinding retryEffects)
  assertEqual "an exact cut retry cannot release another wake" 0 (waitWakeCount retryEffects)

  (redriven, redriveEffects) <-
    checkedIO
      "level-triggered installed-cut re-drive"
      (StructuralCoordinator.settleInstalledStructuralCuts [cut] retried)
  assertBool "an installed-cut re-drive is state-idempotent" (redriven == retried)
  assertEqual
    "an installed-cut re-drive cannot repeat publication completion"
    []
    (receivedPrefixes peerBinding redriveEffects <> completedPrefixes peerBinding redriveEffects)
  assertEqual "an installed-cut re-drive cannot repeat a wake" 0 (waitWakeCount redriveEffects)
  assertEqual "installed-cut release validates the whole Herald" (Right ()) (validateHeraldState installed)

-- This fixture deliberately separates semantic acceptance from source
-- sequencing.  The endpoint is admitted first so Controlled owns the exact
-- name/role fact required by Edge admission, while the Publication owner is
-- composed with the Edge at the earlier Herald position.  The resulting
-- whole state is valid and represents the reachable cross-source ordering:
-- the Edge is first in the source scan, but its endpoint has the later source
-- position and is not yet present in the applied graph.

-- | Exercise the source-owner seam after a delayed endpoint makes a retained
-- Edge ready. A capture receipt is only a correlation fact here: this test
-- does not manufacture a topology certificate or use the bytes as authority.
assertCapturedSourceHoldsReleasedEdge :: HeraldState -> Assertion
assertCapturedSourceHoldsReleasedEdge staged = do
  endpoint <- checkedIO "apply delayed endpoint" (StructuralProgress.advanceNextReadyApplicationStructuralStage staged)
  released <- maybe (assertFailure "delayed endpoint did not apply") (pure . fst) endpoint
  uncaptured <- checkedIO "released Edge is now ready" (StructuralProgress.advanceNextReadyApplicationStructuralStage released)
  assertBool "dependency arrival releases the existing Edge" (isJust uncaptured)
  anchor <- checkedIO "join anchor before source freeze" (GraphProgress.structuralAdmissionAnchorClaim (structuralReconciliationViews released) (startupStructuralProgressState released))
  oracle0 <- checkedIO "source freeze Oracle" (initialOracle (Fixtures.fixtureCheckedOracleGenesisFor (startupGenesis released)))
  newId <- checkedIO "source freeze applicant id" (mkHeraldId (ByteString.replicate 32 0xc7))
  newEpoch <- checkedIO "source freeze applicant epoch" (mkHeraldEpoch (ByteString.replicate 32 0xc8))
  let local = heraldMemberEpoch fixtureLocalMember
      manifest = Admission.heraldAdmissionManifest (checkedSystemId (startupGenesis released)) newId newEpoch
      envelope = oracleEnvelope (oracleClientRequestId local 1) Nothing local (heraldAdmissionCommand (Admission.BeginHeraldAdmission manifest anchor))
  (oracle1, _, effects) <- checkedIO "canonical source freeze Begin" (stepOracle envelope oracle0)
  entry <- case [value | EmitAppliedOracleEntry value <- oracleEffects effects] of
    [one] -> pure (canonicalizeAppliedOracleEntry one)
    _ -> assertFailure "Begin did not produce one canonical control entry"
  record <- maybe (assertFailure "Begin did not retain admission") pure (Oracle.oraclePendingHeraldAdmission oracle1)
  projected <- checkedIO "project source freeze Begin" (OracleProjection.prepareAppliedEntry entry (startupOracleProjectionState released))
  let begun = replaceStartupOracleProjectionState (OracleProjection.commitAppliedEntry projected) released
      admission = Admission.admissionRecordId record
      attempt = Admission.admissionRecordAttempt record
      capture forAttempt = replaceStartupJoinState (JoinState.retainCapture admission forAttempt "source-capture-receipt" (startupJoinState begun)) begun
  frozen <- checkedIO "captured suffix remains held" (StructuralProgress.advanceNextReadyApplicationStructuralStage (capture attempt))
  assertBool "a newly ready retained stage cannot extend the captured source prefix" (not (isJust frozen))
  stale <- checkedIO "old attempt cannot freeze current source" (StructuralProgress.advanceNextReadyApplicationStructuralStage (capture (attempt + 1)))
  assertBool "only the exact current attempt owns the source freeze" (isJust stale)

caseDeferredEdgeSequencesAfterEndpoint :: Assertion
caseDeferredEdgeSequencesAfterEndpoint = do
  bootstraps <- checkedBootstraps fixtureCheckedGenesis fixtureLocalBootstrapIds
  initial <- initialize fixtureCheckedGenesis bootstraps
  bootstrap <- case fixtureLocalBootstrapIds of
    first : _ -> pure first
    [] -> assertFailure "deferred-Edge fixture has no local bootstrap"
  applicationAttachment <- attachment fixtureCheckedGenesis bootstraps bootstrap
  (openedState, opened) <- open 20 applicationAttachment initial
  endpointWriter <- writerFor Access.NeutralVertexRole opened.access
  edgeWriter <- writerFor Access.EdgeRole opened.access
  (withEndpointId, endpointIdEffects) <-
    call
      21
      opened
      1
      (NewIdApplication (ControlledNewId endpointWriter))
      openedState
  privateEndpoint <- completedNewId 1 endpointIdEffects
  (reserved, edgeIdEffects) <-
    call
      22
      opened
      2
      (NewIdApplication (ControlledNewId edgeWriter))
      withEndpointId
  privateEdge <- completedNewId 2 edgeIdEffects
  process <-
    maybe
      (assertFailure "deferred-Edge attachment has no process")
      pure
      ( Application.applicationAttachmentProcess
          applicationAttachment
          (startupApplicationState reserved)
      )
  endpointIdentity <-
    checkedIO
      "resolve deferred-Edge endpoint"
      ( Application.resolveApplicationPrivateUniqueId
          process
          privateEndpoint
          (startupApplicationState reserved)
      )
  let endpointObject = globalObjectIdFromGlobalUniqueId endpointIdentity
      endpointWrite =
        WriteApplication
          endpointWriter
          (PublishValue (neutralValue privateEndpoint))
      roles =
        Application.processRoleView
          ( OracleProjection.projectedProcessEpochs
              (startupOracleProjectionState reserved)
          )
  endpointCandidate <-
    firstRequestCandidate
      "deferred endpoint"
      opened
      3
      endpointWrite
      (startupApplicationState reserved)
  preparedEndpoint <-
    checkedIO
      "prepare deferred endpoint"
      ( ApplicationPublication.prepareLocalApplicationPublication
          fixtureCheckedGenesis
          fixtureHeraldMembershipGeneration
          roles
          endpointCandidate
          endpointWriter
          (neutralValue privateEndpoint)
          (startupControlledState reserved)
          (startupSortRegistryState reserved)
          (startupGraphState reserved)
          (startupStructuralProgressState reserved)
          (startupPlacementState reserved)
          (startupPublicationState reserved)
          (startupStoreState reserved)
          (startupPeerStreamState reserved)
      )
  let ( endpointApplication,
        endpointControlled,
        endpointRegistry,
        _,
        endpointStore,
        endpointPeerStream,
        endpointOutcome
        ) = ApplicationPublication.commitLocalApplicationPublication preparedEndpoint
      originalEndpointRecord =
        ApplicationPublication.localApplicationPublicationRecord endpointOutcome
      edgeValue = deferredEdgeValue privateEdge privateEndpoint
      edgeWrite = WriteApplication edgeWriter (PublishValue edgeValue)
  assertPendingOutcome "endpoint admission" 3 endpointOutcome

  edgeCandidate <-
    firstRequestCandidate
      "earlier deferred Edge"
      opened
      4
      edgeWrite
      endpointApplication
  preparedEdge <-
    checkedIO
      "prepare earlier deferred Edge"
      ( ApplicationPublication.prepareLocalApplicationPublication
          fixtureCheckedGenesis
          fixtureHeraldMembershipGeneration
          roles
          edgeCandidate
          edgeWriter
          edgeValue
          endpointControlled
          endpointRegistry
          (startupGraphState reserved)
          (startupStructuralProgressState reserved)
          (startupPlacementState reserved)
          -- Rewinding only this immutable source owner gives the Edge the
          -- earlier Herald position; all semantic owners still retain the
          -- admitted endpoint name and current Neutral role.
          (startupPublicationState reserved)
          endpointStore
          endpointPeerStream
      )
  let ( edgeApplication,
        edgeControlled,
        edgeRegistry,
        edgePublication,
        edgeStore,
        edgePeerStream,
        edgeOutcome
        ) = ApplicationPublication.commitLocalApplicationPublication preparedEdge
      edgeRecord = ApplicationPublication.localApplicationPublicationRecord edgeOutcome
  assertPendingOutcome "Edge admission" 4 edgeOutcome

  endpointEntry <-
    maybe
      (assertFailure "deferred endpoint carrier sort disappeared")
      pure
      ( SortRegistry.lookupEffectiveSort
          ( checkedPublicationSort
              (Publication.applicationPublicationChecked originalEndpointRecord)
          )
          edgeRegistry
      )
  preparedPositionedEndpoint <-
    checkedIO
      "retain later-position endpoint source"
      ( Publication.prepareApplicationPublication
          ( publicationNabla
              (Publication.applicationPublicationId originalEndpointRecord)
          )
          Nothing
          ( publicationAuthorityEpoch
              (Publication.applicationPublicationId originalEndpointRecord)
          )
          (SortRegistry.registryEntryDescriptor endpointEntry)
          (Publication.applicationPublicationAdmittedValue originalEndpointRecord)
          (Publication.applicationPublicationOrigin originalEndpointRecord)
          (Publication.applicationPublicationMembershipGenerationId originalEndpointRecord)
          (Publication.applicationPublicationSourceProcess originalEndpointRecord)
          (Publication.applicationPublicationAcceptancePosition originalEndpointRecord)
          (Publication.applicationPublicationSortOccurrenceId originalEndpointRecord)
          (Publication.applicationPublicationSourceStrength originalEndpointRecord)
          (Publication.applicationPublicationSourceTopologyPrerequisite originalEndpointRecord)
          (Publication.applicationPublicationControlPrerequisite originalEndpointRecord)
          (Publication.applicationPublicationRoute originalEndpointRecord)
          edgePublication
      )
  let ( positionedPublication,
        positionedClassification,
        positionedEndpointRecord
        ) = Publication.commitApplicationPublication preparedPositionedEndpoint
      staged =
        replaceStartupPeerStreamState edgePeerStream
          . replaceStartupStoreState edgeStore
          . replaceStartupPublicationState positionedPublication
          . replaceStartupSortRegistryState edgeRegistry
          . replaceStartupControlledState edgeControlled
          . replaceStartupApplicationState edgeApplication
          $ reserved
  assertEqual
    "later endpoint is a first source-owner insertion"
    Publication.ApplicationPublicationFirstAccepted
    positionedClassification
  assertEqual "deferred source fixture validates" (Right ()) (validateHeraldState staged)
  assertEqual
    "semantic acceptance reserves no structural dot"
    []
    (Publication.stampedStructuralStageEntries positionedPublication)
  assertBool
    "semantic acceptance enqueues no structural outbox item"
    (startupPeerStreamState staged == startupPeerStreamState reserved)
  assertEqual
    "semantic acceptance leaves the applied vector unchanged"
    ( GraphProgress.structuralAppliedVector
        (startupStructuralProgressState reserved)
    )
    ( GraphProgress.structuralAppliedVector
        (startupStructuralProgressState staged)
    )
  assertPendingRequest "endpoint before sequencing" opened 3 staged
  assertPendingRequest "Edge before sequencing" opened 4 staged

  (edgeStage, endpointStage) <- case Publication.unstampedStructuralSourceStageEntries positionedPublication of
    [first, second] -> pure (first, second)
    actual ->
      assertFailure
        ("expected exactly two deferred structural stages, got " <> show (length actual))
  assertEqual "the held Edge has the earlier source position" EdgeCarrier (Publication.structuralSourceRole edgeStage)
  assertEqual
    "the missing endpoint has the later source position"
    NeutralVertexCarrier
    (Publication.structuralSourceRole endpointStage)
  assertBool
    "source positions are strictly ordered Edge before endpoint"
    ( Publication.heraldPublicationPositionWord64
        (Publication.structuralSourceHeraldPosition edgeStage)
        < Publication.heraldPublicationPositionWord64
          (Publication.structuralSourceHeraldPosition endpointStage)
    )
  assertEqual
    "the retained Edge stage is the admitted Edge publication"
    (checkedPublicationId (Publication.applicationPublicationChecked edgeRecord))
    (checkedPublicationId (Publication.structuralSourceChecked edgeStage))
  assertEqual
    "the retained endpoint stage is the admitted endpoint publication"
    ( checkedPublicationId
        (Publication.applicationPublicationChecked positionedEndpointRecord)
    )
    (checkedPublicationId (Publication.structuralSourceChecked endpointStage))
  assertEdgeHeldForEndpoint endpointObject edgeStage staged
  assertCapturedSourceHoldsReleasedEdge staged

  (sequenced, sequenceEffects) <-
    checkedIO
      "sequence endpoint-released Edge"
      (StructuralCoordinator.advanceLocalStructuralWork staged)
  assertEqual
    "sequencing before a two-member cut emits no application completion"
    []
    (applicationReplyBodies sequenceEffects)
  assertEqual "endpoint-released source state validates" (Right ()) (validateHeraldState sequenced)
  (endpointOccurrence, edgeOccurrence) <-
    case Publication.stampedStructuralStageEntries (startupPublicationState sequenced) of
      [(firstOccurrence, firstStage), (secondOccurrence, secondStage)] -> do
        assertEqual
          "the later source endpoint receives the first available dot"
          (Publication.applicationPublicationAcceptancePosition positionedEndpointRecord)
          ( Publication.applicationPublicationAcceptancePosition
              (Publication.stampedStructuralApplicationRecord firstStage)
          )
        assertEqual
          "the released earlier Edge receives the next dot"
          (Publication.applicationPublicationAcceptancePosition edgeRecord)
          ( Publication.applicationPublicationAcceptancePosition
              (Publication.stampedStructuralApplicationRecord secondStage)
          )
        assertEqual
          "endpoint uses structural sequence one"
          1
          ( structuralSequenceWord64
              (structuralOccurrenceSourceSequence firstOccurrence)
          )
        assertEqual
          "Edge uses structural sequence two"
          2
          ( structuralSequenceWord64
              (structuralOccurrenceSourceSequence secondOccurrence)
          )
        assertEqual
          "endpoint owns exactly one remote structural item"
          1
          (Map.size (Publication.stampedStructuralPeerPublications firstStage))
        assertEqual
          "Edge owns exactly one remote structural item"
          1
          (Map.size (Publication.stampedStructuralPeerPublications secondStage))
        pure (firstOccurrence, secondOccurrence)
      actual ->
        assertFailure
          ("expected endpoint and Edge stamps, got " <> show (length actual))
  assertBool
    "both ordered occurrences are applied before the coordinator returns"
    ( GraphProgress.lookupAppliedStructuralOccurrence
        endpointOccurrence
        (startupStructuralProgressState sequenced)
        /= Nothing
        && GraphProgress.lookupAppliedStructuralOccurrence
          edgeOccurrence
          (startupStructuralProgressState sequenced)
          /= Nothing
    )
  assertBool
    "one-time sequencing enqueues the remote structural items"
    (startupPeerStreamState sequenced /= startupPeerStreamState staged)
  assertPendingRequest "endpoint after sequencing" opened 3 sequenced
  assertPendingRequest "Edge after sequencing" opened 4 sequenced

  (redriven, redriveEffects) <-
    checkedIO
      "idempotent endpoint-released source re-drive"
      (StructuralCoordinator.advanceLocalStructuralWork sequenced)
  assertBool "source re-drive changes no owner" (redriven == sequenced)
  assertBool "source re-drive emits no effect" (effectBatchIsEmpty redriveEffects)

  let (pendingReadiness, withoutReadinessNotification) =
        GraphProgress.takePreparationReadinessChange (startupStructuralProgressState sequenced)
      (pendingPlans, drainedProgress) = GraphProgress.takeAlignmentPlanChange withoutReadinessNotification
  assertBool "direct sequencing leaves its preparation readiness notification pending" pendingReadiness
  assertBool "direct sequencing leaves its alignment plan notification pending" pendingPlans
  (retried, retryEffects) <- call 23 opened 4 edgeWrite sequenced
  assertBool
    "the full retry consumes the existing preparation readiness notification"
    (not (fst (GraphProgress.takePreparationReadinessChange (startupStructuralProgressState retried))))
  assertBool
    "the full retry consumes the existing alignment plan notification"
    (not (fst (GraphProgress.takeAlignmentPlanChange (startupStructuralProgressState retried))))
  assertEqual
    "exact pending Edge retry reoffers only its retained acceptance"
    [OperationAccepted (requestId 4) StructuralStabilizationPending]
    (applicationReplyBodies retryEffects)
  assertBool
    "exact Edge retry allocates no publication dot"
    (startupPublicationState retried == startupPublicationState sequenced)
  assertBool
    "exact Edge retry allocates no peer outbox item"
    (startupPeerStreamState retried == startupPeerStreamState sequenced)
  assertBool
    "exact Edge retry leaves the Graph owner unchanged"
    (startupGraphState retried == startupGraphState sequenced)
  assertBool
    "exact Edge retry only drains the two structural scheduling notifications"
    (startupStructuralProgressState retried == drainedProgress)

firstRequestCandidate ::
  String ->
  Opened ->
  Word ->
  ApplicationOperation ->
  Application.State ->
  IO Application.ApplicationRequestCandidate
firstRequestCandidate context opened request operation state = do
  classification <-
    checkedIO
      ("classify " <> context)
      ( Application.classifyApplicationRequest
          opened.session
          opened.binding
          (requestId (fromIntegral request))
          operation
          state
      )
  case classification of
    Application.FirstApplicationRequest candidate -> pure candidate
    Application.AttachedApplicationRequest _ ->
      assertFailure (context <> " unexpectedly classified as attached")
    Application.RetainedApplicationRequest _ ->
      assertFailure (context <> " unexpectedly classified as a retry")
    Application.ConflictingApplicationRequest _ ->
      assertFailure (context <> " unexpectedly classified as a conflict")

assertPendingOutcome ::
  String ->
  Word ->
  ApplicationPublication.LocalApplicationPublicationOutcome ->
  Assertion
assertPendingOutcome context request outcome =
  assertEqual
    (context <> " remains pending structural stabilization")
    (OperationAccepted (requestId (fromIntegral request)) StructuralStabilizationPending)
    =<< retainedReplyBody
      context
      (ApplicationPublication.localApplicationPublicationReply outcome)

assertPendingRequest :: String -> Opened -> Word -> HeraldState -> Assertion
assertPendingRequest context opened request state = do
  reply <-
    checkedIO
      ("read " <> context)
      ( Application.applicationRequestResult
          opened.session
          opened.binding
          (requestId (fromIntegral request))
          (startupApplicationState state)
      )
  body <- retainedReplyBody context reply
  assertEqual
    (context <> " has no premature completion")
    (OperationAccepted (requestId (fromIntegral request)) StructuralStabilizationPending)
    body

retainedReplyBody :: String -> ApplicationRequestReply -> IO RetainedRequestReplyBody
retainedReplyBody _ (RetainedRequestReply _ body) = pure body
retainedReplyBody context reply =
  assertFailure (context <> ": expected retained request reply, got " <> show reply)

deferredEdgeValue ::
  PrivateUniqueId ->
  PrivateUniqueId ->
  ApplicationValue.ApplicationValue
deferredEdgeValue object endpoint =
  ApplicationValue.RecordValue
    ( Map.fromList
        [ ("destination_vertex", ApplicationValue.UniqueIdValue endpoint),
          ("label", ApplicationValue.LabelValue (ApplicationValue.VoidLabel, 0)),
          ("object_id", ApplicationValue.UniqueIdValue object),
          ("source_vertex", ApplicationValue.UniqueIdValue endpoint),
          ("strength", ApplicationValue.EnumValue "preserve")
        ]
    )

assertEdgeHeldForEndpoint ::
  GlobalObjectId ->
  Publication.StructuralSourceStage ->
  HeraldState ->
  Assertion
assertEdgeHeldForEndpoint endpoint edgeStage state = do
  let progress = startupStructuralProgressState state
      candidate =
        Publication.proposeStructuralOccurrence
          (GraphProgress.structuralAppliedVector progress)
          (startupPublicationState state)
      checked = Publication.structuralSourceChecked edgeStage
      application =
        Reconciliation.structuralApplication
          (Publication.structuralCandidateOccurrence candidate)
          (Publication.structuralCandidatePredecessor candidate)
          ( sortOccurrence
              (checkedPublicationSort checked)
              (Publication.structuralSourceSortOccurrenceId edgeStage)
          )
          checked
          (Publication.structuralSourceControlPrerequisite edgeStage)
      oracle = startupOracleProjectionState state
      views =
        Reconciliation.reconciliationViews
          (checkedLocalHeraldEpoch (startupGenesis state))
          ( Map.fromList
              [ ( SortRegistry.registryEntrySortId entry,
                  sortOccurrence
                    (SortRegistry.registryEntrySortId entry)
                    (SortRegistry.registryEntryOccurrenceId entry)
                )
              | entry <- SortRegistry.registryEntries (startupSortRegistryState state)
              ]
          )
          (Set.fromList (Graph.graphBaselineVertices (startupGraphState state)))
          ( Map.fromList
              [ (process, residence)
              | process <- OracleProjection.projectedProcessEpochs oracle,
                Just residence <-
                  [ OracleProjection.oracleViewProcessResidence
                      process
                      (OracleProjection.oracleView oracle)
                  ]
              ]
          )
          ( Set.fromList
              (Controlled.controlledNormalPossessionEntries (startupControlledState state))
          )
  preparation <-
    checkedIO
      "prepare held earlier Edge"
      ( Reconciliation.prepareStructuralReconciliation
          views
          application
          Nothing
          (GraphProgress.structuralProgressReconciliation progress)
      )
  case preparation of
    Reconciliation.StructuralHeld dependencies ->
      assertEqual
        "the earlier Edge is held solely on its unapplied endpoint"
        (Set.singleton (Reconciliation.StructuralEndpointDependency endpoint))
        dependencies
    Reconciliation.StructuralReady _ ->
      assertFailure "the earlier Edge became ready before its endpoint occurrence"

connectRemotePeer ::
  CheckedInitialBootstraps ->
  HeraldState ->
  IO (HeraldState, PeerBinding)
connectRemotePeer bootstraps predecessor = do
  let nonce = connectionNonce 141
      candidate =
        peerCandidate
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          nonce
      hello =
        peerHello
          (checkedSystemId fixtureCheckedGenesis)
          (heraldMemberId fixtureRemoteMember)
          (heraldMemberEpoch fixtureRemoteMember)
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixtureCheckedGenesis)
          (checkedInitialProjectionDigest bootstraps)
          Nothing
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState predecessor))
  (successor, effects) <-
    step
      1
      ( PeerInput
          ( PeerHelloReceived
              candidate
              Set.empty
              hello
              (heraldMembershipGenerationId membership)
              (heraldMembershipGenerationActiveMemberSetDigest membership)
              Nothing
          )
      )
      predecessor
  binding <- case [ accepted
                  | SetPeerCandidateDisposition _ (PeerHelloAccepted _ accepted) <- effectBatchMembers effects
                  ] of
    [accepted] -> pure accepted
    actual -> assertFailure ("expected one peer acceptance, got " <> show (length actual))
  pure (successor, binding)

requireIncomingPublication ::
  PublicationId ->
  HeraldState ->
  IO Publication.IncomingPublicationRecord
requireIncomingPublication identifier state =
  maybe
    (assertFailure "missing retained incoming publication")
    pure
    (Publication.lookupIncomingPublication identifier (startupPublicationState state))

receivedPrefixes :: PeerBinding -> EffectBatch -> [(StreamDirection, Maybe StreamSequence)]
receivedPrefixes binding effects =
  [ (direction, streamPrefixSequence prefix)
  | SendPeerControl observed (PeerStreamReceived direction prefix) <- effectBatchMembers effects,
    observed == binding
  ]

completedPrefixes :: PeerBinding -> EffectBatch -> [(StreamDirection, Maybe StreamSequence)]
completedPrefixes binding effects =
  [ (direction, streamPrefixSequence (streamCompletionPrefix prefix))
  | SendPeerControl observed (PeerStreamCompleted direction prefix) <- effectBatchMembers effects,
    observed == binding
  ]

repairOfferCount :: PeerBinding -> EffectBatch -> Int
repairOfferCount binding effects =
  length
    [ ()
    | SendPeerControl observed (PeerStreamResumeOffered _) <- effectBatchMembers effects,
      observed == binding
    ]

waitWakeCount :: EffectBatch -> Int
waitWakeCount effects =
  length
    [ ()
    | SendApplicationWaitWake _ _ _ _ _ <- effectBatchMembers effects
    ]

singletonGenesisFixture ::
  IO (CheckedHeraldGenesis, CheckedInitialBootstraps, BootstrapManifestId)
singletonGenesisFixture = do
  configured <- case fixtureConfiguredProcesses of
    first : _ -> pure first
    [] -> assertFailure "fixture has no configured process"
  let singletonMembers = [fixtureLocalMember]
      baseOracle = deploymentOracleGenesis fixtureDeploymentManifest
      manifest =
        fixtureDeploymentManifest
          { deploymentActiveHeralds = singletonMembers,
            deploymentConfiguredProcesses = [configured],
            deploymentOracleGenesis =
              baseOracle {oracleGenesisActiveHeralds = singletonMembers}
          }
      bootstrap = configuredBootstrapManifestId configured
  genesis <- checkedIO "singleton checked genesis" (checkHeraldGenesis manifest)
  bootstraps <- checkedBootstraps genesis [bootstrap]
  pure (genesis, bootstraps, bootstrap)

initialize :: CheckedHeraldGenesis -> CheckedInitialBootstraps -> IO HeraldState
initialize genesis bootstraps =
  fst
    <$> checkedIO
      "initialize structural settlement Herald"
      ( initialHerald
          (monotonicInstant 0)
          genesis
          bootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )

checkedBootstraps ::
  CheckedHeraldGenesis ->
  [BootstrapManifestId] ->
  IO CheckedInitialBootstraps
checkedBootstraps genesis bootstraps =
  checkedIO
    "checked structural settlement bootstraps"
    (checkInitialBootstraps genesis (PrimordialProcessManifest bootstraps))

attachment ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  BootstrapManifestId ->
  IO ApplicationAttachment
attachment genesis bootstraps bootstrap =
  maybe
    (assertFailure "primordial structural settlement attachment missing")
    pure
    (primordialApplicationAttachment genesis bootstraps bootstrap)

open ::
  Word ->
  ApplicationAttachment ->
  HeraldState ->
  IO (HeraldState, Opened)
open observed applicationAttachment predecessor = do
  (successor, effects) <-
    step
      observed
      ( ApplicationSessionInput
          ( OpenApplicationSession
              (candidateApplicationLane 1)
              applicationAttachment
              (Session.clientNonce 1)
          )
      )
      predecessor
  acceptance <- soleAcceptance effects
  case sessionAcceptanceReply acceptance of
    SessionOpened session _ access ->
      pure
        ( successor,
          Opened session (sessionAcceptanceBinding acceptance) access
        )
    reply -> assertFailure ("expected opened application session, got " <> show reply)

soleAcceptance :: EffectBatch -> IO ApplicationSessionAcceptance
soleAcceptance effects =
  case [ acceptance
       | SetApplicationConnectionDisposition _ acceptance <- effectBatchMembers effects
       ] of
    [acceptance] -> pure acceptance
    actual -> assertFailure ("expected one session acceptance, got " <> show (length actual))

writerFor ::
  ApplicationPredefinedSortRole ->
  ApplicationStartupAccess ->
  IO PrivateNablaId
writerFor role access =
  predefinedWriter
    <$> maybe
      (assertFailure ("startup writer missing for " <> show role))
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
            opened.binding
            opened.session
            (requestId (fromIntegral request))
            operation
        )
    )

step :: Word -> HeraldInputBody -> HeraldState -> IO (HeraldState, EffectBatch)
step observed body state =
  checkedIO
    "structural settlement Herald step"
    ( verifiedStepHerald
        (heraldInput (monotonicInstant (fromIntegral observed)) body)
        state
    )

completedNewId :: Word -> EffectBatch -> IO PrivateUniqueId
completedNewId expected effects =
  case applicationReplyBodies effects of
    [Completed actual (NewIdCompleted privateIdentity)] -> do
      assertEqual "newid request" (requestId (fromIntegral expected)) actual
      pure privateIdentity
    actual -> assertFailure ("unexpected newid replies: " <> show actual)

applicationReplyBodies :: EffectBatch -> [RetainedRequestReplyBody]
applicationReplyBodies effects =
  [ body
  | SendApplicationReply _ (RetainedRequestReply _ body) <- effectBatchMembers effects
  ]

neutralValue :: PrivateUniqueId -> ApplicationValue.ApplicationValue
neutralValue object =
  ApplicationValue.RecordValue
    ( Map.fromList
        [ ("label", ApplicationValue.LabelValue (ApplicationValue.VoidLabel, 0)),
          ("object_id", ApplicationValue.UniqueIdValue object)
        ]
    )

soleStampedOccurrence :: HeraldState -> IO StructuralOccurrenceId
soleStampedOccurrence state =
  case Publication.stampedStructuralStageEntries (startupPublicationState state) of
    [(occurrence, _)] -> pure occurrence
    actual -> assertFailure ("expected one stamped structural stage, got " <> show (length actual))

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context result = case result of
  Left problem -> assertFailure (context <> ": " <> show problem)
  Right value -> pure value

caseSelectedPassiveWriter :: Assertion
caseSelectedPassiveWriter = do
  bootstraps <- checkedBootstraps fixtureCheckedGenesis fixtureLocalBootstrapIds
  initial <- initialize fixtureCheckedGenesis bootstraps
  bootstrap <- case fixtureLocalBootstrapIds of
    first : _ -> pure first
    [] -> assertFailure "selected fixture has no local bootstrap"
  applicationAttachment <- attachment fixtureCheckedGenesis bootstraps bootstrap
  (openedState, opened) <- open 10 applicationAttachment initial
  source <- writerFor Access.NablaRole opened.access
  (reserved, newIdEffects) <- call 11 opened 1 (NewIdApplication (ControlledNewId source)) openedState
  privateObject <- completedNewId 1 newIdEffects
  let value =
        ApplicationValue.RecordValue
          ( Map.fromList
              [ ("label", ApplicationValue.LabelValue (ApplicationValue.VoidLabel, 0)),
                ("object_id", ApplicationValue.UniqueIdValue privateObject),
                ("sort_id", ApplicationValue.BytesValue (sortIdBytes (Profile.profileSortFor Profile.NeutralVertexRole))),
                ("sequencing_object", ApplicationValue.OptionalUniqueIdValue Nothing)
              ]
          )
  (pending, _) <- call 12 opened 2 (WriteApplication source (PublishValue value)) reserved
  process <- maybe (assertFailure "parent process missing") pure (Application.applicationAttachmentProcess applicationAttachment (startupApplicationState pending))
  global <- checkedIO "resolve selected passive writer" (Application.resolveApplicationPrivateUniqueId process privateObject (startupApplicationState pending))
  let writer = nablaIdFromGlobalObjectId (globalObjectIdFromGlobalUniqueId global)
  assertEqual
    "the selected endpoint is not yet operational at an installed cut"
    Nothing
    (GraphProgress.lookupInstalledDynamicNabla writer (startupStructuralProgressState pending))
  selection <- checkedIO "select passive writer" (Access.primordialSelection [("passive", Access.Writer (asPrivateNablaId privateObject))] Set.empty Nothing)
  admitted <-
    checkedIO
      "admit current writer before its operation cut"
      (Primordial.checkPrimordialSelection process selection (Application.applicationPrivateIdentity (startupApplicationState pending)) (startupControlledState pending) (startupStructuralProgressState pending))
  assertEqual "the passive writer receives exactly one normal possession grant" 1 (length (Primordial.primordialGrantPossessions admitted))
