{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Step15RetirementClosureProperties
  ( tests,
    establishEmptyStructuralBase,
  )
where

import ApplicationLabelProperties qualified
import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Operation
  ( ApplicationOperation (NewEnvironmentApplication),
  )
import Eclips.Application.Types.Result
  ( OperationPendingReason
      ( EnvironmentStabilizationPending,
        LabelSettlementPending
      ),
  )
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Context (ContextGraph, deriveContextGraph)
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve),
    VertexId (DeltaVertex),
    edgePayload,
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    ControlIndex,
    HeraldEpoch,
    ProcessEpochId,
    PublicationId,
    controlIndex,
    mkDeltaId,
    mkHeraldEpoch,
    mkHeraldId,
    mkLabelDecisionId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkStoreIncarnationId,
    structuralOccurrenceSourceHeraldEpoch,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Structural
  ( emptyStructuralVersionVector,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    labelReleaseCause,
  )
import Eclips.Domain.Topology
  ( TopologyCut,
    TopologyOccurrenceDigest,
    deriveTopologyCutId,
    mkTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyFrontier,
  )
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationPreparation (AlignmentGenerationReady),
    GenerationMemberInput,
    alignmentGenerationAnnouncer,
    alignmentGenerationCut,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    alignmentGenerationRelationDestination,
    alignmentGenerationRelationSource,
    generationMemberInput,
    prepareAlignmentGenerationPlan,
  )
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as AlignmentTransferState
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (OperationAccepted),
  )
import Eclips.Herald.Application.Request.Internal qualified as ApplicationRequest
import Eclips.Herald.Application.Session.Internal
  ( ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened),
    clientNonce,
    sessionAcceptanceBinding,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Discovery (peerBindingRemoteHeraldEpoch)
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect
      ( RunOracleClientAction,
        SendApplicationReply,
        SendPeerControl
      ),
    effectBatchIsEmpty,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( ConfiguredProcessManifest (..),
    DeploymentManifest (..),
    HeraldMember (..),
    InitialTopologyEdgeManifest (InitialTopologyProcessEdge),
    InitialTopologyManifest (..),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstrapsWithTopology,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( structuralAppliedReport,
  )
import Eclips.Herald.Graph.TerminalSource
  ( deriveTerminalSourceUnion,
    emptyTerminalSourcePayloadArchive,
    successorStructuralBaseCutId,
    terminalSourceGenesisPredecessorBase,
    terminalSourceInventory,
    terminalSourceUnionAcceptance,
    terminalSourceUnionEstablished,
    terminalSourceUnionEstablishedUnion,
    terminalSourceUnionReady,
    terminalSourceUnionTerminalControlPrefix,
  )
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.IdGenerator (GeneratorSeed)
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( ApplicationRequestIngress (CallApplicationRequest),
    HeraldInputBody (OracleInput, PeerInput),
    PeerControl
      ( PeerStructuralAppliedReported,
        PeerTerminalSourceInventoryAdvertised,
        PeerTerminalSourcePayloadRelayed,
        PeerTerminalSourcePayloadRequested,
        PeerTerminalSourceUnionEstablished
      ),
    PeerIngress (PeerControlReceived),
    heraldInput,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (ConnectAndHelloOracle, SubmitOracleRequest),
    OracleClientIngress (OracleEntriesReceived, OracleHelloReceived),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (..),
    peerLogicalPayloadDigest,
  )
import Eclips.Herald.PeerPublication (peerPublicationBatch, publicationBatchId)
import Eclips.Herald.PeerStream
  ( ReceiveProgress (ReceivedPending),
    SequencedItem,
    StreamDirection,
    firstStreamSequence,
    mkStreamDirection,
    nextStreamSequence,
    sequencedItem,
    sequencedItemPayload,
    sequencedItemSequence,
    streamCompletedPrefix,
    streamPrefixThrough,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementMessage
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupOracleClientState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupStructuralBaseCoordinator,
    replaceStartupStructuralProgressState,
    startupAlignmentState,
    startupApplicationState,
    startupDiscoveryState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
    startupTerminalSourceHoldState,
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
import Eclips.Herald.TerminalSourceHold.State qualified as TerminalSourceHold
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ApplicationCall qualified as ApplicationCall
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Herald.UseCase.Step15RetirementClosure
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureConfiguredProcesses,
    fixtureDeploymentManifest,
    fixtureGeneratorSeedH1,
    fixtureGeneratorSeedH2,
    fixtureGeneratorSeedH3,
    fixtureHeraldRetirementReason,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
  )
import GenesisFixtures qualified as MembershipFixtures
import HeraldTransitionProperties qualified as TransitionFixture
import OracleAdvanceProperties (acceptedVoterFailureHeldFixtureWithEntry, heldControlIndexTwoForMembershipAdvance, terminalPayloadReadinessFixture)
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
    "Step-15 deterministic retirement closure"
    [ testCase
        "pending terminal payloads survive Oracle progress without repeated requests and retry on a fresh binding"
        casePendingTerminalPayloadReadiness,
      testCase
        "an empty exact retirement closure installs the successor base atomically"
        caseEmptyExactClosure,
      testCase
        "successor-base installation redrives one public newenv candidate exactly once"
        caseSuccessorBaseRedrivesPublicEnvironment,
      testCase
        "reconnect repair sends Established before a successor report to a lagging peer"
        caseReconnectTerminalBeforeSuccessorReport,
      testCase
        "local successor-base progress replays one held successor report without another callback"
        caseLocalBaseProgressReplaysHeldSuccessorReport,
      testCase
        "the composed closure reaches a fixed point and its exact replay is inert"
        caseFixedPointAndReplay,
      testCase
        "the closure resumes both before and immediately after successor-base installation"
        caseResumesPartialClosure,
      testCase
        "successor-base installation preserves the earlier old-generation publication release"
        caseReleasesHeldPublication,
      testCase
        "the retained live successor base level-triggers held publication release"
        caseLiveSuccessorBaseReleasesHeldPublication,
      testCase
        "base release exposes and completes the next ordered route cutover"
        caseBaseReleaseExposesRouteCutover,
      testCase
        "a label admitted during the union period is preserved and promoted after the base"
        casePromotesUnionPeriodLabel,
      testCase
        "a retired selected source is re-driven only after the successor base"
        caseRetiredSelectedSourceRedrive,
      testCase
        "retirement closure terminally settles retained route markers before whole-state validation"
        caseTerminallySettlesRetiredSourceMarkers,
      testCase
        "accepted voter failure closes held source markers and redrives newenv across the real retirement base gap"
        caseAcceptedVoterFailureClosesPendingWork
    ]

casePendingTerminalPayloadReadiness :: Assertion
casePendingTerminalPayloadReadiness = do
  (beforeRetirement, binding, occurrence, retirementEntry, progressEntry, reconnect) <- terminalPayloadReadinessFixture
  let applyEntry entry state = do
        oracleBinding <- maybe (assertFailure "terminal payload fixture lost its Oracle binding") pure (OracleClient.oracleClientCurrentBinding (startupOracleClientState state))
        checked "apply genuine terminal-fixture Oracle entry" (stepHerald (heraldInput (startupLastObservedTime state) (OracleInput (OracleEntriesReceived oracleBinding (entry NonEmpty.:| [])))) state)
      receive supplied control state =
        checked "receive terminal fixture control" (stepHerald (heraldInput (startupLastObservedTime state) (PeerInput (PeerControlReceived supplied control))) state)
      requests effects = [(supplied, request) | SendPeerControl supplied (PeerTerminalSourcePayloadRequested request) <- effectBatchMembers effects]
      coordinator state = maybe (assertFailure "terminal fixture lost its coordinator") pure (startupStructuralBaseCoordinator state)
  (afterRetirement, _) <- applyEntry retirementEntry beforeRetirement
  initial <- coordinator afterRetirement
  let lineage = StructuralBase.structuralBaseLineage initial
      retired = structuralOccurrenceSourceHeraldEpoch (TerminalSource.terminalStructuralOccurrenceId occurrence)
      reporter = peerBindingRemoteHeraldEpoch binding
  (inventory, _) <- checked "seal the sole survivor's real payload inventory" (TerminalSource.sealTerminalSourceInventory lineage lineage retired reporter [] [] [occurrence])
  let control = PeerTerminalSourceInventoryAdvertised inventory
  (waiting, initialEffects) <- receive binding control afterRetirement
  request <- case requests initialEffects of
    [(supplied, value)] | supplied == binding -> pure value
    other -> assertFailure ("first inventory admission did not request its one missing payload: " <> show other)
  assertEqual "the missing payload keeps exactly its readiness hold" [(binding, control)] (TerminalSourceHold.heldTerminalSourceControls (startupTerminalSourceHoldState waiting))

  (advanced, progressEffects) <- applyEntry progressEntry waiting
  assertBool "the unrelated canonical command really advances the control prefix" (GraphProgress.structuralAppliedControlPrefix (startupStructuralProgressState advanced) > GraphProgress.structuralAppliedControlPrefix (startupStructuralProgressState waiting))
  assertEqual "Oracle progress does not resend an already-pending payload request" [] (requests progressEffects)
  assertEqual "readiness rechecks retain the same unresolved inventory" [(binding, control)] (TerminalSourceHold.heldTerminalSourceControls (startupTerminalSourceHoldState advanced))
  advancedCoordinator <- coordinator advanced
  assertEqual "unrelated progress invents no terminal payload" [] (StructuralBase.structuralBasePayloadEntries advancedCoordinator)

  -- Releasing an Oracle-ahead hold is first admission, not a local-readiness
  -- retry. It must still emit the request even within the same Oracle input.
  (ahead, aheadEffects) <- receive binding control beforeRetirement
  assertEqual "an inventory ahead of membership sends no premature request" [] (requests aheadEffects)
  (released, releaseEffects) <- applyEntry retirementEntry ahead
  assertEqual "actual membership projection releases the first request" [(binding, request)] (requests releaseEffects)
  assertEqual "membership release leaves the unresolved payload hold" [(binding, control)] (TerminalSourceHold.heldTerminalSourceControls (startupTerminalSourceHoldState released))

  -- The original transport may have lost the request or response. The new
  -- authenticated binding must be allowed to request the same exact bytes.
  (reconnected, freshBinding) <- reconnect advanced
  assertBool "reconnection produces a different physical binding" (freshBinding /= binding)
  (retried, retryEffects) <- receive freshBinding control reconnected
  assertEqual "fresh-binding inventory admission retries the exact request" [(freshBinding, request)] (requests retryEffects)
  relay <- checked "relay the checked original structural payload" (TerminalSource.terminalSourceEvidencePayloadRelay lineage reporter request inventory occurrence)
  (repaired, repairEffects) <- receive freshBinding (PeerTerminalSourcePayloadRelayed relay) retried
  repairedCoordinator <- coordinator repaired
  assertEqual "the response retains the exact original payload" [(TerminalSource.terminalStructuralOccurrenceId occurrence, occurrence)] (StructuralBase.structuralBasePayloadEntries repairedCoordinator)
  assertEqual "completed repair emits no further payload request" [] (requests repairEffects)
  assertEqual "the complete repaired receiver remains valid" (Right ()) (validateHeraldState repaired)

-- F04 composition uses the canonical native-failure transcript through final
-- exclusion, then the real retirement watch entry. Only the existing terminal
-- source protocol fixture supplies the survivor union needed to close its base.
caseAcceptedVoterFailureClosesPendingWork :: Assertion
caseAcceptedVoterFailureClosesPendingWork = do
  (held, publication, retired, applyRetirement, retirementEntry) <- acceptedVoterFailureHeldFixtureWithEntry
  assertPublicationDisposition "the survivor publication waits for actual semantic retirement" Publication.DependencyHeld publication held
  (opened, session, binding, process) <- openFirstApplicationSession held
  (predecessor, direction, routeItem) <- retainRetiredSourceMarkers retired opened
  beforeMarkers <- checked "inspect pending failed-source markers" (PeerStream.incomingRetainedItems direction (startupPeerStreamState predecessor))
  assertEqual
    "the failed-source route marker is pending before the canonical retirement"
    [(routeItem, PeerStream.IncomingReceivedPending)]
    beforeMarkers

  afterRetirement <- applyRetirement predecessor
  let membership = currentMembership afterRetirement
  assertBool
    "only the real retirement entry removes the failed voter from semantic membership"
    ( retired `elem` heraldMembershipGenerationActiveHeraldEpochs (currentMembership predecessor)
        && retired `notElem` heraldMembershipGenerationActiveHeraldEpochs membership
    )
  assertEqual "canonical retirement preserves whole-state validity" (Right ()) (validateHeraldState afterRetirement)
  -- The test owns this original Oracle effect for replay. Completed canonical
  -- processing may already have released the Herald's unpinned archive copy.
  let replayRetirement state = do
        oracleBinding <- maybe (assertFailure "actual retirement lost its Oracle binding") pure (OracleClient.oracleClientCurrentBinding (startupOracleClientState state))
        checked
          "replay the genuine retirement through the live Oracle input"
          (stepHerald (heraldInput (startupLastObservedTime state) (OracleInput (OracleEntriesReceived oracleBinding (retirementEntry NonEmpty.:| [])))) state)
  coordinator <- maybe (assertFailure "actual retirement did not retain its structural-base coordinator") pure (startupStructuralBaseCoordinator afterRetirement)
  assertBool "actual retirement leaves the successor base uninstalled" (StructuralBase.structuralBaseEvidence coordinator == Nothing)

  let request = ApplicationRequest.requestId 1
      call = CallApplicationRequest binding session request NewEnvironmentApplication
      positions = applicationAcceptancePositions afterRetirement
  (deferred, deferredEffects) <- checked "submit public newenv during the real retirement base gap" (ApplicationCall.applyApplicationRequest call afterRetirement)
  assertEqual "base-gap newenv emits no accepted reply" [] (repliesFor binding deferredEffects)
  assertEqual "base-gap newenv retains one cursorless candidate" 1 (length (Application.applicationEnvironmentCandidateKeys (startupApplicationState deferred)))
  assertEqual "base-gap newenv owns no accepted manifest" [] (Application.applicationPendingEnvironmentEntries (startupApplicationState deferred))
  assertEqual "base-gap newenv consumes no acceptance position" positions (applicationAcceptancePositions deferred)
  assertEqual "pending source closure and newenv compose into a valid Herald" (Right ()) (validateHeraldState deferred)

  established <- establishEmptyStructuralBaseForStates predecessor afterRetirement coordinator
  (installedProgress, installedCoordinator) <- checked "install the actual retirement's live successor base" (StructuralBase.installSuccessorStructuralBase (startupStructuralProgressState deferred) established)
  installedBase <- maybe (assertFailure "actual retirement's live base did not install") pure (StructuralBase.structuralBaseEvidence installedCoordinator)
  let withLiveBase = replaceStartupStructuralBaseCoordinator (Just installedCoordinator) . replaceStartupStructuralProgressState installedProgress $ deferred
  (settled, settlementEffects) <- checked "settle the actual retirement's live successor cut" (StructuralCoordinator.settleInstalledStructuralCuts [successorStructuralBaseCutId installedBase] withLiveBase)
  -- Canonical watch evidence and detached membership evidence are distinct.
  -- Replaying the real watch entry runs the ordinary transition tail after the
  -- live base installation, including reconsideration of cursorless newenv.
  (successor, redriveEffects) <- replayRetirement settled
  let effects = settlementEffects <> redriveEffects
  assertPublicationDisposition "the survivor publication is applied after actual retirement closure" Publication.Applied publication successor
  markers <- checked "inspect terminal failed-source marker settlement" (PeerStream.incomingRetainedItems direction (startupPeerStreamState successor))
  assertEqual
    "retirement closure releases the failed-source route payload receipt"
    []
    markers
  assertCompletedStreamItems "failed-source closure completion" direction (routeItem :| []) (startupPeerStreamState successor)
  pendingReply <- case repliesFor binding effects of
    [reply@(RetainedRequestReply _ (OperationAccepted observed EnvironmentStabilizationPending))] -> do
      assertEqual "closure accepts the exact retained newenv request" request observed
      pure reply
    observed -> assertFailure ("actual retirement closure newenv replies: " <> show observed)
  assertEqual "closure consumes the cursorless candidate" [] (Application.applicationEnvironmentCandidateKeys (startupApplicationState successor))
  assertEqual "closure retains exactly one accepted newenv manifest" 1 (length (Application.applicationPendingEnvironmentEntries (startupApplicationState successor)))
  assertEqual
    "closure consumes exactly one acceptance position for the live survivor application"
    (fmap (\(owner, ordinal) -> if owner == process then (owner, ordinal + 1) else (owner, ordinal)) positions)
    (applicationAcceptancePositions successor)
  retained <- maybe (assertFailure "closure lost its installed coordinator") pure (startupStructuralBaseCoordinator successor)
  base <- maybe (assertFailure "closure did not install the successor base") pure (StructuralBase.structuralBaseEvidence retained)
  assertBool "Graph and retained coordinator agree on the installed base" (GraphProgress.structuralSuccessorBaseInstalled base (startupStructuralProgressState successor))
  assertEqual "all closure owners remain valid" (Right ()) (validateHeraldState successor)

  (retried, retryEffects) <- checked "retry newenv after accepted voter failure closure" (ApplicationCall.applyApplicationRequest call successor)
  assertBool "newenv exact retry preserves all owners" (retried == successor)
  assertEqual "newenv exact retry reoffers its original pending result" [pendingReply] (repliesFor binding retryEffects)
  (repeated, replayEffects) <- replayRetirement successor
  assertBool "full canonical retirement replay is whole-state identical" (repeated == successor)
  assertEqual
    "full canonical retirement replay only reoffers its retained Oracle watch and submissions"
    (fmap RunOracleClientAction (OracleClient.oracleClientActions (startupOracleClientState successor)))
    (effectBatchMembers replayEffects)
  (redriven, baseEffects) <- checked "redrive the installed actual-retirement base" (StructuralCoordinator.advanceLocalStructuralWork repeated)
  assertBool "live base redrive is whole-state identical" (redriven == successor)
  assertEqual
    "the local announcer does not broadcast its retained structural report"
    []
    (effectBatchMembers baseEffects)

caseSuccessorBaseRedrivesPublicEnvironment :: Assertion
caseSuccessorBaseRedrivesPublicEnvironment = do
  fixture <- retirementClosureFixture
  (openedPredecessor, session, binding, process) <-
    openFirstApplicationSession fixture.predecessor
  membershipAdvance <-
    checked
      "prepare membership advance with a live application session"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successorMembership
          []
          openedPredecessor
      )
  let membershipOnly = MembershipAdvance.commitMembershipAdvance membershipAdvance
      blocked =
        replaceStartupStructuralBaseCoordinator
          (Just fixture.establishedBase)
          membershipOnly
      request = ApplicationRequest.requestId 1
      call =
        CallApplicationRequest
          binding
          session
          request
          NewEnvironmentApplication
      beforePositions = applicationAcceptancePositions blocked
  (deferred, deferredEffects) <-
    checked
      "submit public newenv in the successor-base gap"
      (ApplicationCall.applyApplicationRequest call blocked)
  assertEqual "deferred public newenv emits no reply" [] (repliesFor binding deferredEffects)
  assertEqual
    "deferred public newenv retains exactly one cursorless candidate"
    1
    (length (Application.applicationEnvironmentCandidateKeys (startupApplicationState deferred)))
  assertEqual
    "deferral retains no accepted environment manifest"
    []
    (Application.applicationPendingEnvironmentEntries (startupApplicationState deferred))
  assertEqual
    "deferral consumes no process acceptance position"
    beforePositions
    (applicationAcceptancePositions deferred)

  preparedClosure <-
    checked
      "install the successor base and redrive public newenv"
      ( prepareRetirementClosure
          openedPredecessor
          deferred
          membershipAdvance
          fixture.establishedBase
      )
  let released = commitRetirementClosure preparedClosure
      releaseEffects = preparedRetirementClosureEffects preparedClosure
  retainedCoordinator <-
    maybe
      (assertFailure "base installation lost the retained closure")
      pure
      (startupStructuralBaseCoordinator released)
  retainedBase <-
    maybe
      (assertFailure "the retained closure was not marked installed")
      pure
      (StructuralBase.structuralBaseEvidence retainedCoordinator)
  assertBool
    "the retained closure and Graph commit the same installed base"
    (GraphProgress.structuralSuccessorBaseInstalled retainedBase (startupStructuralProgressState released))
  pendingReply <- case repliesFor binding releaseEffects of
    [reply@(RetainedRequestReply _ (OperationAccepted retained EnvironmentStabilizationPending))] -> do
      assertEqual "redriven pending reply request" request retained
      pure reply
    replies ->
      assertFailure
        ("successor-base redrive emitted unexpected replies: " <> show replies)
  assertEqual
    "redrive consumes the sole cursorless candidate"
    []
    (Application.applicationEnvironmentCandidateKeys (startupApplicationState released))
  assertEqual
    "redrive retains exactly one accepted environment manifest"
    1
    (length (Application.applicationPendingEnvironmentEntries (startupApplicationState released)))
  assertEqual
    "redrive consumes exactly one process acceptance position"
    (fmap (\(owner, ordinal) -> if owner == process then (owner, ordinal + 1) else (owner, ordinal)) beforePositions)
    (applicationAcceptancePositions released)
  assertEqual "the released state is valid" (Right ()) (validateHeraldState released)

  (retried, retryEffects) <-
    checked
      "retry redriven public newenv"
      (ApplicationCall.applyApplicationRequest call released)
  assertEqual "exact retry reoffers the retained pending reply" [pendingReply] (repliesFor binding retryEffects)
  assertBool "exact retry is whole-state identical" (retried == released)
  assertEqual
    "exact retry retains one manifest and one consumed position"
    ( Application.applicationPendingEnvironmentEntries (startupApplicationState released),
      applicationAcceptancePositions released
    )
    ( Application.applicationPendingEnvironmentEntries (startupApplicationState retried),
      applicationAcceptancePositions retried
    )

openFirstApplicationSession ::
  HeraldState ->
  IO (HeraldState, ApplicationSessionId, ApplicationSessionBinding, ProcessEpochId)
openFirstApplicationSession predecessor = do
  (attachment, process) <- case Application.applicationWitnessAttachments witness of
    first : _ -> pure first
    [] -> assertFailure "retirement fixture has no local application attachment"
  prepared <-
    checked
      "open retirement-gap application session"
      ( Application.prepareApplicationSessionOpen
          (GraphProgress.structuralProgressLocalHerald (startupStructuralProgressState predecessor))
          attachment
          (clientNonce 0x1604)
          application
      )
  let (successorApplication, acceptance) =
        Application.commitApplicationSessionAcceptance prepared
      successor = replaceStartupApplicationState successorApplication predecessor
      binding = sessionAcceptanceBinding acceptance
  case sessionAcceptanceReply acceptance of
    SessionOpened session _ _ -> pure (successor, session, binding, process)
    reply -> assertFailure ("retirement-gap session did not open: " <> show reply)
  where
    application = startupApplicationState predecessor
    witness = Application.applicationStateWitness application

applicationAcceptancePositions :: HeraldState -> [(ProcessEpochId, Word64)]
applicationAcceptancePositions =
  Application.applicationRequestNextAcceptancePositions
    . Application.applicationRequestStateWitness
    . startupApplicationState

repliesFor :: ApplicationSessionBinding -> EffectBatch -> [ApplicationRequestReply]
repliesFor expectedBinding effects =
  [ reply
  | SendApplicationReply binding reply <- effectBatchMembers effects,
    binding == expectedBinding
  ]

caseEmptyExactClosure :: Assertion
caseEmptyExactClosure = do
  fixture <- retirementClosureFixture
  prepared <- prepareFreshClosure fixture
  let receipt = preparedRetirementClosureReceipt prepared
      successor = commitRetirementClosure prepared
      progress = startupStructuralProgressState successor
  assertEqual "fresh disposition" RetirementClosureApplied (retirementClosureDisposition receipt)
  assertEqual "exact retired Herald" fixture.retired (retirementClosureRetiredHerald receipt)
  assertEqual "the fixture has no retired placement" [] (retirementClosureLostStores receipt)
  assertEqual "the fixture settles no outgoing assignment" [] (retirementClosureSettledAssignments receipt)
  assertEqual "an empty loss set has no loss plans" [] (retirementClosureAlignmentLossPlans receipt)
  assertEqual
    "Graph switched to the exact successor generation"
    (heraldMembershipGenerationId fixture.successorMembership)
    ( GraphProgress.structuralProgressMembershipGenerationId
        progress
    )
  assertBool
    "the receipt's exact successor base is installed"
    ( GraphProgress.structuralSuccessorBaseInstalled
        (retirementClosureSuccessorBase receipt)
        progress
    )
  assertEqual "the composed successor remains valid" (Right ()) (validateHeraldState successor)

caseReconnectTerminalBeforeSuccessorReport :: Assertion
caseReconnectTerminalBeforeSuccessorReport = do
  fixture <- retirementClosureFixture
  prepared <- prepareFreshClosure fixture
  expectedCut <-
    checked
      "derive installed reconnect successor cut"
      (StructuralBase.expectedSuccessorStructuralCut fixture.establishedBase)
  installedCoordinator <-
    checked
      "retain installed reconnect terminal coordinator"
      ( StructuralBase.recordInstalledSuccessorStructuralCut
          expectedCut
          fixture.establishedBase
      )
  let successor =
        replaceStartupStructuralBaseCoordinator
          (Just installedCoordinator)
          (commitRetirementClosure prepared)
  binding <- case Discovery.currentPeerBindings (startupDiscoveryState successor) of
    [single] -> pure single
    observed ->
      assertFailure
        ( "expected one surviving reconnect binding, got "
            <> show (length observed)
        )
  effects <-
    checked
      "compose membership-successor reconnect repair"
      (PeerControl.reconnectRepairEffects binding successor)
  let controls =
        [ control
        | SendPeerControl observed control <- effects,
          observed == binding
        ]
  case controls of
    PeerTerminalSourceInventoryAdvertised _
      : PeerTerminalSourceUnionEstablished _
      : remaining ->
        assertEqual
          "announcer repair needs no structural-report marker"
          []
          [report | PeerStructuralAppliedReported report <- remaining]
    observed ->
      assertFailure
        ( "expected terminal inventory and Established leading successor repair, got "
            <> show observed
        )

caseLocalBaseProgressReplaysHeldSuccessorReport :: Assertion
caseLocalBaseProgressReplaysHeldSuccessorReport = do
  fixture <- retirementClosureFixture
  let membershipOnly =
        MembershipAdvance.commitMembershipAdvance fixture.membershipAdvance
      withCoordinator =
        replaceStartupStructuralBaseCoordinator
          (Just fixture.establishedBase)
          membershipOnly
  binding <- case Discovery.currentPeerBindings (startupDiscoveryState withCoordinator) of
    [single] -> pure single
    observed ->
      assertFailure
        ( "expected one successor-report binding, got "
            <> show (length observed)
        )
  established <-
    maybe
      (assertFailure "fixture structural base is not established")
      pure
      (StructuralBase.structuralBaseEstablishedUnion fixture.establishedBase)
  let union = terminalSourceUnionEstablishedUnion established
      reporter =
        case filter
          (/= GraphProgress.structuralProgressLocalHerald (startupStructuralProgressState withCoordinator))
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs fixture.successorMembership)) of
          remote : _ -> remote
          [] -> error "successor fixture has no remote reporter"
      report =
        structuralAppliedReport
          reporter
          (emptyStructuralVersionVector fixture.successorMembership)
          (terminalSourceUnionTerminalControlPrefix union)
      control = PeerStructuralAppliedReported report
  (withHeldReport, _, _, _) <-
    checked
      "retain successor report before local base installation"
      (PeerControl.applyPeerControlWithInstalledCuts binding control withCoordinator)
  assertEqual
    "the successor report is retained exactly once"
    [(binding, control)]
    ( TerminalSourceHold.heldTerminalSourceControls
        (startupTerminalSourceHoldState withHeldReport)
    )

  (installedProgress, installedCoordinator) <-
    checked
      "install local successor base for readiness redrive"
      ( StructuralBase.installSuccessorStructuralBase
          (startupStructuralProgressState withHeldReport)
          fixture.establishedBase
      )
  let afterLocalProgress =
        replaceStartupStructuralProgressState installedProgress
          . replaceStartupStructuralBaseCoordinator (Just installedCoordinator)
          $ withHeldReport
  (replayed, replayEffects, replayCuts) <-
    checked
      "redrive held successor report after local base progress"
      ( PeerControl.redriveTerminalSourceReadinessIfProgressed
          withHeldReport
          afterLocalProgress
      )
  assertBool
    "the earlier admitted predecessor has no terminal coordinator"
    (startupStructuralBaseCoordinator fixture.predecessor == Nothing)
  (fromBeforeCoordinator, startEffects, startCuts) <-
    checked
      "a coordinator introduced after the predecessor still redrives readiness"
      ( PeerControl.redriveTerminalSourceReadinessIfProgressed
          fixture.predecessor
          afterLocalProgress
      )
  assertBool
    "the successor coordinator, even when absent from the predecessor, determines the complete readiness result"
    (fromBeforeCoordinator == replayed)
  assertEqual "coordinator-start readiness effects" (effectBatchMembers replayEffects) (effectBatchMembers startEffects)
  assertEqual "coordinator-start installed cuts" replayCuts startCuts
  assertEqual
    "local Graph progress consumes the hold without another wire callback"
    []
    ( TerminalSourceHold.heldTerminalSourceControls
        (startupTerminalSourceHoldState replayed)
    )
  assertBool
    "the held remote report is admitted exactly into successor progress"
    ( report
        `elem` fmap
          snd
          ( GraphProgress.structuralReportEntries
              (startupStructuralProgressState replayed)
          )
    )

caseFixedPointAndReplay :: Assertion
caseFixedPointAndReplay = do
  fixture <- retirementClosureFixture
  first <- prepareFreshClosure fixture
  let successor = commitRetirementClosure first
      installedBase = retirementClosureSuccessorBase (preparedRetirementClosureReceipt first)
  (afterSettlement, _) <-
    checked
      "re-settle the membership-successor cut"
      ( StructuralCoordinator.settleInstalledStructuralCuts
          [successorStructuralBaseCutId installedBase]
          successor
      )
  assertBool "successor-base settlement is state-identical on retry" (successor == afterSettlement)
  (afterStructural, _) <-
    checked
      "re-drive structural fixed point"
      (StructuralCoordinator.advanceLocalStructuralWork afterSettlement)
  assertBool "structural re-drive is state-identical" (successor == afterStructural)
  (afterAlignment, alignmentEffects) <-
    checked
      "re-drive alignment fixed point"
      (AlignmentTransfer.advanceAlignmentTransfers afterStructural)
  assertBool "alignment re-drive is state-identical" (successor == afterAlignment)
  assertBool "alignment re-drive is silent" (effectBatchIsEmpty alignmentEffects)
  (afterLabels, labelEffects, _) <-
    checked
      "re-drive application-label fixed point"
      (ApplicationCall.advanceApplicationLabelWork afterAlignment)
  assertBool "application-label re-drive is state-identical" (successor == afterLabels)
  assertBool "application-label re-drive is silent" (effectBatchIsEmpty labelEffects)

  duplicateAdvance <-
    checked
      "prepare exact membership replay"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successorMembership
          []
          successor
      )
  assertEqual
    "membership owner recognizes the exact replay"
    MembershipAdvance.MembershipAdvanceExactDuplicate
    (MembershipAdvance.preparedMembershipAdvanceDisposition duplicateAdvance)
  replay <-
    checked
      "prepare exact closure replay"
      ( prepareRetirementClosure
          fixture.predecessor
          successor
          duplicateAdvance
          fixture.establishedBase
      )
  assertEqual
    "closure replay disposition"
    RetirementClosureExactReplay
    (retirementClosureDisposition (preparedRetirementClosureReceipt replay))
  assertBool
    "closure replay is state-identical"
    (successor == commitRetirementClosure replay)
  assertBool
    "closure replay emits no effects"
    (effectBatchIsEmpty (preparedRetirementClosureEffects replay))

caseResumesPartialClosure :: Assertion
caseResumesPartialClosure = do
  fixture <- retirementClosureFixture
  uninterrupted <- prepareFreshClosure fixture
  let expected = commitRetirementClosure uninterrupted
      expectedEffects = preparedRetirementClosureEffects uninterrupted
      membershipOnly =
        MembershipAdvance.commitMembershipAdvance fixture.membershipAdvance
  membershipReplay <-
    checked
      "prepare membership replay before base installation"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successorMembership
          []
          membershipOnly
      )
  beforeBase <-
    checked
      "resume retirement closure before base installation"
      ( prepareRetirementClosure
          fixture.predecessor
          membershipOnly
          membershipReplay
          fixture.establishedBase
      )
  assertEqual
    "pre-base resume performs the missing closure"
    RetirementClosureApplied
    (retirementClosureDisposition (preparedRetirementClosureReceipt beforeBase))
  assertBool
    "pre-base resume converges to the uninterrupted successor"
    (commitRetirementClosure beforeBase == expected)
  assertEqual
    "pre-base resume emits the same deterministic tail effects"
    expectedEffects
    (preparedRetirementClosureEffects beforeBase)

  (installedProgress, _) <-
    checked
      "install only the resumable successor base"
      ( StructuralBase.installSuccessorStructuralBase
          (startupStructuralProgressState membershipOnly)
          fixture.establishedBase
      )
  let baseOnly =
        replaceStartupStructuralProgressState
          installedProgress
          membershipOnly
  baseReplay <-
    checked
      "prepare membership replay after base-only installation"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successorMembership
          []
          baseOnly
      )
  afterBase <-
    checked
      "resume retirement closure after base-only installation"
      ( prepareRetirementClosure
          fixture.predecessor
          baseOnly
          baseReplay
          fixture.establishedBase
      )
  assertBool
    "base-only resume converges to the uninterrupted successor"
    (commitRetirementClosure afterBase == expected)
  assertEqual
    "base-only resume classifies work from exact state change"
    ( if baseOnly == expected
        then RetirementClosureExactReplay
        else RetirementClosureApplied
    )
    (retirementClosureDisposition (preparedRetirementClosureReceipt afterBase))

  finalMembershipReplay <-
    checked
      "prepare membership replay after resumed closure"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successorMembership
          []
          expected
      )
  finalReplay <-
    checked
      "replay the fully resumed closure"
      ( prepareRetirementClosure
          fixture.predecessor
          expected
          finalMembershipReplay
          fixture.establishedBase
      )
  assertEqual
    "fully resumed closure is an exact replay"
    RetirementClosureExactReplay
    (retirementClosureDisposition (preparedRetirementClosureReceipt finalReplay))
  assertBool
    "fully resumed closure replay is state-identical"
    (commitRetirementClosure finalReplay == expected)
  assertBool
    "fully resumed closure replay is effect-silent"
    (effectBatchIsEmpty (preparedRetirementClosureEffects finalReplay))

caseReleasesHeldPublication :: Assertion
caseReleasesHeldPublication = do
  fixture <- retirementClosureFixture
  assertPublicationDisposition
    "publication starts held"
    Publication.DependencyHeld
    fixture.publication
    fixture.predecessor
  assertPublicationDisposition
    "the old stamped publication applies when its control dependency releases"
    Publication.Applied
    fixture.publication
    (MembershipAdvance.commitMembershipAdvance fixture.membershipAdvance)
  prepared <- prepareFreshClosure fixture
  let successor = commitRetirementClosure prepared
      receipt = preparedRetirementClosureReceipt prepared
  assertEqual
    "the base does not repeat work already released under its original stamp"
    []
    (retirementClosureReleasedPublications receipt)
  assertPublicationDisposition
    "the earlier applied survivor publication remains applied in the composed closure"
    Publication.Applied
    fixture.publication
    successor
  assertEqual "the released successor remains valid" (Right ()) (validateHeraldState successor)

caseLiveSuccessorBaseReleasesHeldPublication :: Assertion
caseLiveSuccessorBaseReleasesHeldPublication = do
  (fixture, afterMembership) <- successorHeldClosureFixture
  (installedProgress, installedCoordinator) <-
    checked
      "install the retained live successor base"
      ( StructuralBase.installSuccessorStructuralBase
          (startupStructuralProgressState afterMembership)
          fixture.establishedBase
      )
  base <-
    maybe
      (assertFailure "the retained live coordinator has no installed base")
      pure
      (StructuralBase.structuralBaseEvidence installedCoordinator)
  let withLiveBase =
        replaceStartupStructuralBaseCoordinator (Just installedCoordinator)
          . replaceStartupStructuralProgressState installedProgress
          $ afterMembership
  (successor, _) <-
    checked
      "settle the live successor base and its level-triggered work"
      ( StructuralCoordinator.settleInstalledStructuralCuts
          [successorStructuralBaseCutId base]
          withLiveBase
      )
  assertPublicationDisposition
    "the live coordinator releases the held publication"
    Publication.Applied
    fixture.publication
    successor
  assertEqual "the live successor remains valid" (Right ()) (validateHeraldState successor)
  (replayed, _) <-
    checked
      "re-drive the retained live successor base"
      (StructuralCoordinator.advanceLocalStructuralWork successor)
  assertBool "the live successor-base re-drive is state-identical" (replayed == successor)

caseBaseReleaseExposesRouteCutover :: Assertion
caseBaseReleaseExposesRouteCutover = do
  (fixture, afterMembership) <- successorHeldClosureFixture
  let local = heraldMemberEpoch fixtureLocalMember
      survivor = heraldMemberEpoch fixtureRemoteMember
      direction = checkedValue "survivor route direction" (mkStreamDirection survivor local)
  retainedPublication <-
    checked
      "inspect the base-held publication stream"
      (PeerStream.incomingRetainedItems direction (startupPeerStreamState afterMembership))
  publicationItem <- case retainedPublication of
    [(item, PeerStream.IncomingReceivedPending)] -> pure item
    observed ->
      assertFailure
        ( "expected one base-held survivor publication, got "
            <> show observed
        )
  case sequencedItemPayload publicationItem of
    PeerLogicalPublication _ -> pure ()
    payload ->
      assertFailure
        ("the base-held stream item is not a publication: " <> show payload)
  (installedProgress, installedCoordinator) <-
    checked
      "install the route-ordering successor base"
      ( StructuralBase.installSuccessorStructuralBase
          (startupStructuralProgressState afterMembership)
          fixture.establishedBase
      )
  base <-
    maybe
      (assertFailure "the route-ordering coordinator has no installed base")
      pure
      (StructuralBase.structuralBaseEvidence installedCoordinator)
  let withBase =
        replaceStartupStructuralProgressState installedProgress afterMembership
  (withReadyRoute, routeGeneration) <-
    retainReadySurvivorRouteGeneration survivor withBase
  routeMarker <-
    checked
      "ordered survivor route marker"
      ( AlignmentProtocol.alignmentRouteCutoverMarker
          (Plan.alignmentGenerationBirthPlanId routeGeneration)
          (alignmentGenerationId routeGeneration)
          survivor
          local
      )
  let routePayload = PeerLogicalRouteCutover routeMarker
      routeItem =
        sequencedItem
          direction
          (nextStreamSequence firstStreamSequence)
          (peerLogicalPayloadDigest routePayload)
          routePayload
  withRouteStream <-
    retainPendingRouteItem routeItem withReadyRoute
  retainedBefore <-
    checked
      "inspect the route blocked behind the base-held publication"
      (PeerStream.incomingRetainedItems direction (startupPeerStreamState withRouteStream))
  assertEqual
    "the route is pending immediately behind the base-held publication"
    [ (publicationItem, PeerStream.IncomingReceivedPending),
      (routeItem, PeerStream.IncomingReceivedPending)
    ]
    retainedBefore
  (successor, releasedPublications, _) <-
    checked
      "release the base-held publication and exposed route"
      ( StructuralCoordinator.advanceSuccessorStructuralBaseWork
          base
          withRouteStream
      )
  retainedAfter <-
    checked
      "inspect the route after successor-base release"
      (PeerStream.incomingRetainedItems direction (startupPeerStreamState successor))
  assertEqual
    "base release reclaims both completed payload receipts in one fixed point"
    []
    retainedAfter
  assertCompletedStreamItems "base-and-route fixed-point completion" direction (publicationItem :| [routeItem]) (startupPeerStreamState successor)
  assertEqual
    "the common base seam reports the exact released publication"
    [fixture.publication]
    releasedPublications
  assertBool
    "the exposed route marker is retained by the Alignment owner"
    ( routeMarker
        `elem` fmap
          snd
          ( AlignmentTransferState.routeCutoverEntries
              (Alignment.alignmentTransferState (startupAlignmentState successor))
          )
    )
  (replayed, replayedPublications, replayEffects) <-
    checked
      "re-drive the base-and-route fixed point"
      (StructuralCoordinator.advanceSuccessorStructuralBaseWork base successor)
  assertBool "the base-and-route re-drive is state-identical" (replayed == successor)
  assertEqual "the base-and-route re-drive releases no publication" [] replayedPublications
  assertBool "the base-and-route re-drive is silent" (effectBatchIsEmpty replayEffects)
  where
    retainPendingRouteItem item state = do
      candidate <-
        checked
          "retain the route behind the base-held publication"
          (PeerStream.prepareReceiveCandidate item (startupPeerStreamState state))
      prepared <-
        checked
          "finalize the pending route behind the base-held publication"
          ( PeerStream.finalizeReceive
              (Map.singleton (sequencedItemSequence item) ReceivedPending)
              candidate
          )
      pure
        ( replaceStartupPeerStreamState
            (fst (PeerStream.commitReceive prepared))
            state
        )

retainReadySurvivorRouteGeneration ::
  HeraldEpoch ->
  HeraldState ->
  IO (HeraldState, AlignmentGeneration)
retainReadySurvivorRouteGeneration survivor state = do
  placementVector <-
    checked
      "derive the survivor-route placement vector"
      ( DomainAlignment.physicalPlacementRevisionVector
          membership
          [ (member, DomainAlignment.firstPlacementRevision)
          | member <-
              NonEmpty.toList
                (heraldMembershipGenerationActiveHeraldEpochs membership)
          ]
      )
  context <-
    checked
      "derive the survivor-route context graph"
      ( deriveContextGraph
          [DeltaVertex localDelta, DeltaVertex survivorDelta]
          [ edgePayload (DeltaVertex localDelta) (DeltaVertex survivorDelta) Preserve,
            edgePayload (DeltaVertex survivorDelta) (DeltaVertex localDelta) Preserve
          ]
          [localDelta, survivorDelta]
      )
  firstTopology <-
    checked
      "derive the survivor-route predecessor topology"
      ( topologyCut
          (sameGenerationPredecessor (GraphProgress.structuralLastInstalledCutId progress))
          frontier
          ( checkedValue
              "survivor-route predecessor topology digest"
              (mkTopologyOccurrenceDigest (fixtureIdentifierBytes 0xf1))
          )
      )
  firstPlan <-
    readyPlan
      "prepare the survivor-route predecessor generation"
      firstTopology
      []
      Set.empty
      placementVector
      context
  firstGeneration <- oneGeneration "survivor-route predecessor" firstPlan
  let withFirstDebt = retainAlignmentDebt firstCause typedSort state
  preparedFirst <-
    checked
      "promote the survivor-route predecessor generation"
      ( Alignment.prepareAlignmentPromotion
          (alignmentGenerationAnnouncer firstGeneration)
          firstCause
          typedSort
          (deriveTopologyCutId firstTopology)
          placementVector
          firstPlan
          (startupAlignmentState withFirstDebt)
      )
  let (firstAlignment, _) = Alignment.commitAlignmentPromotion preparedFirst
      afterFirst = replaceStartupAlignmentState firstAlignment withFirstDebt
  secondTopology <-
    checked
      "derive the survivor-route successor topology"
      ( topologyCut
          (sameGenerationPredecessor (deriveTopologyCutId firstTopology))
          frontier
          ( checkedValue
              "survivor-route successor topology digest"
              (mkTopologyOccurrenceDigest (fixtureIdentifierBytes 0xf2))
          )
      )
  secondPlan <-
    readyPlan
      "prepare the survivor-route successor generation"
      secondTopology
      [firstGeneration]
      (Set.singleton (alignmentGenerationId firstGeneration))
      placementVector
      context
  successorGeneration <- oneGeneration "survivor-route successor" secondPlan
  let withSecondDebt = retainAlignmentDebt secondCause typedSort afterFirst
  preparedSecond <-
    checked
      "promote the survivor-route successor generation"
      ( Alignment.prepareAlignmentPromotion
          (alignmentGenerationAnnouncer successorGeneration)
          secondCause
          typedSort
          (deriveTopologyCutId secondTopology)
          placementVector
          secondPlan
          (startupAlignmentState withSecondDebt)
      )
  let (secondAlignment, _) = Alignment.commitAlignmentPromotion preparedSecond
      accepted =
        retainCutAcceptanceFrom
          successorGeneration
          survivor
          secondAlignment
  pure
    ( replaceStartupAlignmentState accepted withSecondDebt,
      successorGeneration
    )
  where
    membership = currentMembership state
    progress = startupStructuralProgressState state
    frontier =
      topologyFrontier
        (GraphProgress.structuralAppliedVector progress)
        (GraphProgress.structuralAppliedControlPrefix progress)
    local = heraldMemberEpoch fixtureLocalMember
    typedSort =
      sortOccurrence
        (checkedValue "survivor-route sort" (mkSortId (fixtureIdentifierBytes 0xf3)))
        ( checkedValue
            "survivor-route sort occurrence"
            (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xf4))
        )
    localDelta =
      checkedValue "survivor-route local delta" (mkDeltaId (fixtureIdentifierBytes 0xf5))
    survivorDelta =
      checkedValue "survivor-route remote delta" (mkDeltaId (fixtureIdentifierBytes 0xf6))
    localStore =
      checkedValue
        "survivor-route local store"
        (mkStoreIncarnationId (fixtureIdentifierBytes 0xf7))
    survivorStore =
      checkedValue
        "survivor-route remote store"
        (mkStoreIncarnationId (fixtureIdentifierBytes 0xf8))
    members =
      [ generationMemberInput
          localDelta
          localStore
          local
          DomainAlignment.initialStoreRevision,
        generationMemberInput
          survivorDelta
          survivorStore
          survivor
          DomainAlignment.initialStoreRevision
      ]
    firstCause =
      checkedValue
        "survivor-route predecessor cause"
        ( labelReleaseCause
            (checkedValue "survivor-route predecessor decision" (mkLabelDecisionId (fixtureIdentifierBytes 0xf9)))
            (GraphProgress.structuralAppliedControlPrefix progress)
        )
    secondCause =
      checkedValue
        "survivor-route successor cause"
        ( labelReleaseCause
            (checkedValue "survivor-route successor decision" (mkLabelDecisionId (fixtureIdentifierBytes 0xfa)))
            (GraphProgress.structuralAppliedControlPrefix progress)
        )

    readyPlan contextName topology predecessors closed placement context =
      case prepareAlignmentGenerationPlan
        typedSort
        topology
        placement
        context
        members
        predecessors
        []
        closed of
        Right (AlignmentGenerationReady plan) -> pure plan
        observed -> assertFailure (contextName <> ": " <> show observed)

    oneGeneration contextName plan =
      case alignmentGenerationPlanGenerations plan of
        [generation] -> pure generation
        observed ->
          assertFailure
            ( contextName
                <> ": expected one generation, got "
                <> show (length observed)
            )

caseTerminallySettlesRetiredSourceMarkers :: Assertion
caseTerminallySettlesRetiredSourceMarkers = do
  fixture <- retirementClosureFixture
  (withMarkers, direction, routeItem) <- retainRetiredSourceMarkers fixture.retired fixture.predecessor
  membershipAdvance <-
    checked
      "prepare membership advance with retained retired-source markers"
      ( MembershipAdvance.prepareMembershipAdvance
          fixture.index
          fixture.successorMembership
          []
          withMarkers
      )
  let membershipOnly = MembershipAdvance.commitMembershipAdvance membershipAdvance
  retainedAfterRetirement <-
    checked
      "inspect retained markers immediately after source retirement"
      (PeerStream.incomingRetainedItems direction (startupPeerStreamState membershipOnly))
  assertEqual
    "membership retirement reclaims the retired-source route receipt"
    []
    retainedAfterRetirement
  assertCompletedStreamItems "membership-only route completion" direction (routeItem :| []) (startupPeerStreamState membershipOnly)
  baseCoordinator <-
    checked
      "begin structural base with retained retired-source markers"
      ( StructuralBase.beginStructuralBaseAfterAppliedMembershipAdvance
          []
          withMarkers
          membershipOnly
      )
  establishedBase <-
    establishEmptyStructuralBase
      withMarkers
      membershipAdvance
      baseCoordinator
  prepared <-
    checked
      "prepare retirement closure with retained retired-source markers"
      ( prepareRetirementClosure
          withMarkers
          membershipOnly
          membershipAdvance
          establishedBase
      )
  let successor = commitRetirementClosure prepared
  retained <-
    checked
      "inspect terminally settled retired-source markers"
      (PeerStream.incomingRetainedItems direction (startupPeerStreamState successor))
  assertEqual
    "the completed route-marker payload receipt remains reclaimed"
    []
    retained
  assertCompletedStreamItems "retirement closure marker completion" direction (routeItem :| []) (startupPeerStreamState successor)
  assertEqual
    "the final marker-settled Herald remains composed-valid"
    (Right ())
    (validateHeraldState successor)

assertCompletedStreamItems :: String -> StreamDirection -> NonEmpty (SequencedItem payload) -> PeerStream.State payload -> Assertion
assertCompletedStreamItems contextName direction items@(first :| rest) streams = do
  forM_ items $ \item ->
    assertBool
      (contextName <> ": compact progress remembers the exact assignment")
      (PeerStream.incomingSequenceCompleted (direction, sequencedItemSequence item) streams)
  watermarks <- checked (contextName <> ": read causal barrier") (PeerStream.incomingWatermarks direction streams)
  let through = foldr (max . sequencedItemSequence) (sequencedItemSequence first) rest
  assertEqual
    (contextName <> ": causal barrier covers exactly the completed positions")
    (streamPrefixThrough through)
    (streamCompletedPrefix watermarks)

retainRetiredSourceMarkers ::
  HeraldEpoch ->
  HeraldState ->
  IO (HeraldState, StreamDirection, SequencedItem PeerLogicalPayload)
retainRetiredSourceMarkers retired predecessor = do
  let local = GraphProgress.structuralProgressLocalHerald (startupStructuralProgressState predecessor)
      firstSequence = firstStreamSequence
      membership = currentMembership predecessor
      markerPlan =
        Plan.alignmentPlanIdFromClaimedCoordinates
          (sortOccurrence (checkedValue "marker sort" (mkSortId (fixtureIdentifierBytes 0xf3))) (checkedValue "marker occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xf4))))
          (GraphProgress.structuralLastInstalledCutId (startupStructuralProgressState predecessor))
          (checkedValue "marker placement" (DomainAlignment.physicalPlacementRevisionVector membership [(herald, DomainAlignment.firstPlacementRevision) | herald <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)]))
  direction <-
    checked
      "retired-source marker direction"
      (mkStreamDirection retired local)
  generation <-
    checked
      "terminal route-cutover generation identifier"
      (DomainAlignment.mkContextClassGenerationId (fixtureIdentifierBytes 0xe1))
  routeMarker <-
    checked
      "retained retired-source route-cutover marker"
      (AlignmentProtocol.alignmentRouteCutoverMarker markerPlan generation retired local)
  let routePayload = PeerLogicalRouteCutover routeMarker
      routeItem =
        sequencedItem
          direction
          firstSequence
          (peerLogicalPayloadDigest routePayload)
          routePayload
  retainedStream <-
    foldM
      retainPendingMarker
      (startupPeerStreamState predecessor)
      [routeItem]
  let withMarkers = replaceStartupPeerStreamState retainedStream predecessor
  assertEqual
    "the retained marker predecessor is a valid whole Herald"
    (Right ())
    (validateHeraldState withMarkers)

  pure (withMarkers, direction, routeItem)
  where
    retainPendingMarker ::
      PeerStream.State PeerLogicalPayload ->
      SequencedItem PeerLogicalPayload ->
      IO (PeerStream.State PeerLogicalPayload)
    retainPendingMarker state item = do
      candidate <-
        checked
          "retain pending retired-source marker candidate"
          (PeerStream.prepareReceiveCandidate item state)
      prepared <-
        checked
          "finalize pending retired-source marker"
          ( PeerStream.finalizeReceive
              (Map.singleton (sequencedItemSequence item) ReceivedPending)
              candidate
          )
      pure (fst (PeerStream.commitReceive prepared))

casePromotesUnionPeriodLabel :: Assertion
casePromotesUnionPeriodLabel = do
  (predecessor, current, membershipAdvance, establishedBase) <-
    ApplicationLabelProperties.membershipHeldLabelClosureFixture
  assertEqual
    "the union-period Open remains in the pre-acceptance owner"
    1
    (length (Application.applicationLabelCandidateEntries (startupApplicationState current)))
  assertEqual
    "the union-period Open owns no active workflow before the base"
    Nothing
    (Application.applicationActiveLabel (startupApplicationState current))
  prepared <-
    checked
      "prepare closure with one union-period label candidate"
      ( prepareRetirementClosure
          predecessor
          current
          membershipAdvance
          establishedBase
      )
  let successor = commitRetirementClosure prepared
      effects = effectBatchMembers (preparedRetirementClosureEffects prepared)
      acceptanceReplies =
        [ body
        | SendApplicationReply _ (RetainedRequestReply _ body) <- effects,
          OperationAccepted _ LabelSettlementPending <- [body]
        ]
      labelSubmissions =
        [ action
        | RunOracleClientAction action@SubmitOracleRequest {} <- effects
        ]
  assertEqual
    "the closure consumes the single pre-acceptance candidate"
    []
    (Application.applicationLabelCandidateEntries (startupApplicationState successor))
  assertBool
    "the union-period Open owns the active workflow after the base"
    (Application.applicationActiveLabel (startupApplicationState successor) /= Nothing)
  assertEqual "the Open is accepted exactly once" 1 (length acceptanceReplies)
  assertEqual "the accepted Open is submitted exactly once" 1 (length labelSubmissions)
  assertEqual "the promoted successor remains valid" (Right ()) (validateHeraldState successor)

caseRetiredSelectedSourceRedrive :: Assertion
caseRetiredSelectedSourceRedrive = do
  fixture <- selectedSourceRetirementFixture
  let beforeMembership = fixture.predecessor
      beforeAlignment = startupAlignmentState beforeMembership
      oldAttempt = fixture.retiredAttempt
      obligation = AlignmentProtocol.alignmentAttemptObligation oldAttempt
      obligationId = AlignmentProtocol.alignmentObligationIdValue obligation
      oldSubscription = AlignmentProtocol.alignmentAttemptSubscriptionId oldAttempt
      fulfilledBefore = Alignment.bootstrapImportFulfillmentEntries beforeAlignment
  sourceGeneration <-
    maybe
      (assertFailure "the selected source generation is absent from the fixture")
      pure
      ( Alignment.lookupAlignmentGeneration
          (AlignmentProtocol.alignmentObligationSourceGeneration obligation)
          beforeAlignment
      )
  retiredSourceMember <-
    generationMemberFor
      "retiring H4 source member"
      fixture.retired
      sourceGeneration
  let retiredSource =
        Alignment.unavailableAlignmentSource
          fixture.retired
          (DomainAlignment.alignmentMemberDelta retiredSourceMember)
          (DomainAlignment.alignmentMemberStoreIncarnation retiredSourceMember)
      latestGenerations =
        Alignment.latestAlignmentGenerationsForSort
          ( sortOccurrence
              (AlignmentProtocol.alignmentObligationSortId obligation)
              (AlignmentProtocol.alignmentObligationSortDefinitionOccurrenceId obligation)
          )
          beforeAlignment
  assertBool
    "the retiring source belongs only to a superseded predecessor generation"
    ( all
        ( all ((/= fixture.retired) . DomainAlignment.alignmentMemberHerald)
            . NonEmpty.toList
            . DomainAlignment.alignmentCutExactMembers
            . alignmentGenerationCut
        )
        latestGenerations
        && alignmentGenerationId sourceGeneration
          `notElem` fmap alignmentGenerationId latestGenerations
    )
  assertEqual
    "the pre-retirement attempt deliberately selects the retiring source"
    fixture.retired
    (AlignmentProtocol.alignmentAttemptSourceHerald oldAttempt)
  assertBool
    "the fixture contains fulfilled-import history independent of the retired attempt"
    (not (null fulfilledBefore))

  let afterMembership = MembershipAdvance.commitMembershipAdvance fixture.membershipAdvance
      membershipAlignment = startupAlignmentState afterMembership
  assertBool
    "membership retirement preserves the stable ordinary obligation"
    (Alignment.lookupAlignmentObligation obligationId membershipAlignment == Just obligation)
  assertEqual
    "membership retirement removes the selected attempt without predecessor-generation reselection"
    []
    [ attempt
    | (identifier, attempt) <- Alignment.alignmentAttemptEntries membershipAlignment,
      identifier == obligationId
    ]
  assertEqual
    "membership retirement preserves fulfilled imports byte-for-byte"
    fulfilledBefore
    (Alignment.bootstrapImportFulfillmentEntries membershipAlignment)
  assertBool
    "membership retirement durably excludes the exact H4 source incarnation"
    (Set.member retiredSource (Alignment.alignmentUnavailableSources membershipAlignment))

  (installedProgress, installedCoordinator) <-
    checked
      "install the exact source-redrive successor base"
      ( StructuralBase.installSuccessorStructuralBase
          (startupStructuralProgressState afterMembership)
          fixture.establishedBase
      )
  installedBase <-
    maybe
      (assertFailure "the established source-redrive coordinator lost its base evidence")
      pure
      (StructuralBase.structuralBaseEvidence installedCoordinator)
  let withBase =
        replaceStartupStructuralProgressState
          installedProgress
          afterMembership
  (withSettledBase, _) <-
    checked
      "settle the exact source-redrive successor base"
      ( StructuralCoordinator.settleInstalledStructuralCuts
          [successorStructuralBaseCutId installedBase]
          withBase
      )
  (successor, _) <-
    checked
      "run the closure alignment fixed point after successor-base installation"
      (AlignmentTransfer.advanceAlignmentTransfers withSettledBase)
  let
    successorAlignment = startupAlignmentState successor
    replacements =
      [ attempt
      | (identifier, attempt) <- Alignment.alignmentAttemptEntries successorAlignment,
        identifier == obligationId
      ]
  replacement <- case replacements of
    [attempt] -> pure attempt
    observed ->
      assertFailure
        ( "expected one post-base replacement attempt, got "
            <> show (length observed)
        )
  assertEqual
    "successor-base advancement selects the exact surviving source"
    fixture.survivor
    (AlignmentProtocol.alignmentAttemptSourceHerald replacement)
  assertBool
    "successor-base reselection allocates a fresh subscription"
    (AlignmentProtocol.alignmentAttemptSubscriptionId replacement /= oldSubscription)
  assertEqual
    "the retired source owns no active post-closure attempt"
    []
    [ attempt
    | (_, attempt) <- Alignment.alignmentAttemptEntries successorAlignment,
      AlignmentProtocol.alignmentAttemptSourceHerald attempt == fixture.retired
    ]
  assertBool
    "the pre-retirement fulfilled import survives the complete closure unchanged"
    ( all
        (`elem` Alignment.bootstrapImportFulfillmentEntries successorAlignment)
        fulfilledBefore
    )
  assertEqual
    "the source-redrive alignment owner remains valid"
    (Right ())
    (Alignment.validateAlignmentState successorAlignment)
  (replayed, replayEffects) <-
    checked
      "replay the post-base source-redrive fixed point"
      (AlignmentTransfer.advanceAlignmentTransfers successor)
  assertBool "post-base source-redrive replay is state-identical" (replayed == successor)
  assertBool "post-base source-redrive replay is silent" (effectBatchIsEmpty replayEffects)
  assertEqual
    "post-base replay retains exactly the one H2 replacement"
    [replacement]
    [ attempt
    | (identifier, attempt) <-
        Alignment.alignmentAttemptEntries (startupAlignmentState replayed),
      identifier == obligationId
    ]

data SelectedSourceRetirementFixture = SelectedSourceRetirementFixture
  { predecessor :: HeraldState,
    retired :: HeraldEpoch,
    survivor :: HeraldEpoch,
    retiredAttempt :: AlignmentProtocol.AlignmentAttempt,
    membershipAdvance :: MembershipAdvance.PreparedMembershipAdvance,
    establishedBase :: StructuralBase.MembershipBaseClosure
  }

selectedSourceRetirementFixture :: IO SelectedSourceRetirementFixture
selectedSourceRetirementFixture = do
  let local = heraldMemberEpoch fixtureLocalMember
      survivor = heraldMemberEpoch fixtureRemoteMember
      spare = heraldMemberEpoch sourceRedriveSpareMember
      retired = heraldMemberEpoch sourceRedriveRetiredMember
      localInitial = sourceRedriveInitial fixtureLocalMember fixtureGeneratorSeedH1
      survivorInitial = sourceRedriveInitial fixtureRemoteMember fixtureGeneratorSeedH2
      spareInitial = sourceRedriveInitial sourceRedriveSpareMember fixtureGeneratorSeedH3
      retiredInitial = sourceRedriveInitial sourceRedriveRetiredMember fixtureGeneratorSeedH3
      survivorSnapshot = retainedPlacementSnapshot survivorInitial
      spareSnapshot = retainedPlacementSnapshot spareInitial
      retiredSnapshot = retainedPlacementSnapshot retiredInitial
  bound <- bindFixtureOracle localInitial
  withPlacements <-
    foldM
      installRemotePlacementSnapshot
      (MembershipFixtures.fixtureHeraldAfterProbePrefix bound)
      [survivorSnapshot, spareSnapshot, retiredSnapshot]
  let predecessorCause =
        checkedValue
          "source-redrive predecessor consequence"
          ( labelReleaseCause
              (checkedValue "source-redrive predecessor decision" (mkLabelDecisionId (fixtureIdentifierBytes 0xd1)))
              (controlIndex 2)
          )
      successorCause =
        checkedValue
          "source-redrive successor consequence"
          ( labelReleaseCause
              (checkedValue "source-redrive successor decision" (mkLabelDecisionId (fixtureIdentifierBytes 0xd2)))
              (controlIndex 2)
          )
      expectedActiveMembers = Set.fromList [local, survivor, spare, retired]
      expectedAlignmentMembers = Set.fromList [local, survivor, retired]
  assertEqual
    "the fixture retains H1/H2/H3 and the non-voter-equivalent H4 until its sole retirement"
    expectedActiveMembers
    ( Set.fromList
        ( NonEmpty.toList
            (heraldMembershipGenerationActiveHeraldEpochs (currentMembership withPlacements))
        )
    )
  (affectedSort, firstGeneration, firstPromoted) <-
    promoteSourceRedrivePredecessor
      predecessorCause
      expectedAlignmentMembers
      withPlacements
  let firstAlignment = startupAlignmentState firstPromoted
      firstGenerations = generationsForSort affectedSort firstAlignment
      withEvidence =
        retainSourceRedriveEvidence
          [local, survivor, retired]
          firstGenerations
          firstAlignment
      evidencedPredecessor = replaceStartupAlignmentState withEvidence firstPromoted
  assertBool
    "the predecessor source class contains exactly surviving H2 and retiring H4"
    (generationHasHeralds (Set.fromList [survivor, retired]) firstGeneration)
  advancedPlacement <-
    advanceRemotePlacementSnapshot survivor survivorSnapshot evidencedPredecessor
  (secondGenerations, rawSecondPromoted) <-
    promoteSourceRedriveSuccessor
      successorCause
      affectedSort
      (Set.fromList [local, survivor])
      advancedPlacement
  secondGeneration <-
    generationForHeralds
      "local successor destination generation"
      (Set.singleton local)
      secondGenerations
  let secondAlignment =
        retainSourceRedriveEvidence
          [local, survivor, retired]
          secondGenerations
          (startupAlignmentState rawSecondPromoted)
      secondPromoted =
        replaceStartupAlignmentState secondAlignment rawSecondPromoted
      ordinaryEntries =
        [ entry
        | entry@(_, obligation) <- Alignment.activeAlignmentObligationEntries secondAlignment,
          AlignmentProtocol.alignmentObligationSourceGeneration obligation
            == alignmentGenerationId firstGeneration
        ]
      predecessorImports =
        [ key
        | (key, _) <- Alignment.bootstrapImportEntries secondAlignment,
          Alignment.bootstrapImportKeyGeneration key
            == alignmentGenerationId secondGeneration,
          Alignment.BootstrapFromPredecessor sourceGeneration <-
            [Alignment.bootstrapImportKeySource key],
          Just source <- [Alignment.lookupAlignmentGeneration sourceGeneration secondAlignment],
          generationHasHeralds (Set.fromList [survivor, retired]) source
        ]
  (obligationId, _) <-
    case ordinaryEntries of
      first : _ -> pure first
      [] -> assertFailure "successor placement promotion lost the predecessor ordinary obligation"
  predecessorImport <-
    case predecessorImports of
      first : _ -> pure first
      [] -> assertFailure "successor placement promotion created no predecessor import"
  withRetiredAttempt <-
    installOrdinaryAttemptFrom
      survivor
      retired
      obligationId
      secondAlignment
  retiredAttempt <-
    case [ attempt
         | (identifier, attempt) <- Alignment.alignmentAttemptEntries withRetiredAttempt,
           identifier == obligationId
         ] of
      [attempt] -> pure attempt
      observed ->
        assertFailure
          ( "expected one retiring-source attempt, got "
              <> show (length observed)
          )
  withFulfilledImport <-
    fulfillPredecessorImportFrom
      survivor
      predecessorImport
      withRetiredAttempt
  let predecessor = replaceStartupAlignmentState withFulfilledImport secondPromoted
      index = controlIndex 2
      predecessorMembership = currentMembership predecessor
      successorMembership =
        checkedValue
          "source-redrive successor membership"
          (retireHeraldMembershipGeneration index (MembershipFixtures.fixtureRetirementResolution index) retired predecessorMembership)
      processEnds = projectedResidentEnds index retired predecessor
  assertEqual
    "the added H4 is application-free, so this is a pure Herald retirement"
    []
    processEnds
  assertEqual
    "the source-redrive predecessor alignment owner is internally valid"
    (Right ())
    (Alignment.validateAlignmentState (startupAlignmentState predecessor))
  membershipAdvance <-
    checked
      "prepare source-redrive membership advance"
      ( MembershipAdvance.prepareMembershipAdvance
          index
          successorMembership
          processEnds
          predecessor
      )
  baseCoordinator <-
    checked
      "begin source-redrive structural base"
      ( StructuralBase.beginStructuralBaseAfterAppliedMembershipAdvance
          []
          predecessor
          (MembershipAdvance.commitMembershipAdvance membershipAdvance)
      )
  establishedBase <-
    establishEmptyStructuralBase predecessor membershipAdvance baseCoordinator
  pure
    SelectedSourceRetirementFixture
      { predecessor,
        retired,
        survivor,
        retiredAttempt,
        membershipAdvance,
        establishedBase
      }

-- The checked startup manifest can express writer-to-reader edges, but not the
-- Delta-to-Delta SCC needed to give H2 and H4 interchangeable source stores.
-- This fixture therefore keeps the real four-member placement, MembershipAdvance,
-- structural-base, and transfer owners, while its generations use a checked
-- synthetic same-generation topology cuts and are certified at the Alignment-owner
-- boundary. The successor topology excludes H4 before membership retirement, so
-- its loss affects only a still-closing predecessor relation, not a current plan.
-- The surrounding closure tests retain the whole-state replay coverage.

sourceRedriveRetiredMember :: HeraldMember
sourceRedriveRetiredMember =
  HeraldMember
    (checkedValue "source-redrive H4 id" (mkHeraldId (fixtureIdentifierBytes 0x73)))
    (checkedValue "source-redrive H4 epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0x74)))

sourceRedriveSpareMember :: HeraldMember
sourceRedriveSpareMember =
  HeraldMember
    (checkedValue "source-redrive H3 id" (mkHeraldId (fixtureIdentifierBytes 0x71)))
    (checkedValue "source-redrive H3 epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0x72)))

sourceRedriveInitial :: HeraldMember -> GeneratorSeed -> HeraldState
sourceRedriveInitial localMember seed =
  fst
    ( checkedValue
        "initialize source-redrive Herald"
        ( initialHerald
            (monotonicInstant 0)
            genesis
            bootstraps
            fixtureOracleContacts
            seed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    )
  where
    members =
      [ fixtureLocalMember,
        fixtureRemoteMember,
        sourceRedriveSpareMember,
        sourceRedriveRetiredMember
      ]
    baseOracle = deploymentOracleGenesis fixtureDeploymentManifest
    manifest =
      fixtureDeploymentManifest
        { deploymentLocalHeraldId = heraldMemberId localMember,
          deploymentLocalHeraldEpoch = heraldMemberEpoch localMember,
          deploymentActiveHeralds = members,
          deploymentConfiguredProcesses = sourceRedriveConfiguredProcesses,
          deploymentOracleGenesis = baseOracle {oracleGenesisActiveHeralds = members}
        }
    genesis = checkedValue "check source-redrive genesis" (checkHeraldGenesis manifest)
    bootstraps =
      checkedValue
        "check source-redrive bootstraps"
        ( checkInitialBootstrapsWithTopology
            genesis
            (PrimordialProcessManifest sourceRedriveBootstrapIds)
            sourceRedriveTopology
        )

sourceRedriveConfiguredProcesses :: [ConfiguredProcessManifest]
sourceRedriveConfiguredProcesses = fixtureConfiguredProcesses

sourceRedriveBootstrapIds :: [BootstrapManifestId]
sourceRedriveBootstrapIds = fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]

sourceRedriveTopology :: InitialTopologyManifest
sourceRedriveTopology =
  InitialTopologyManifest
    [ InitialTopologyProcessEdge localBootstrap survivorBootstrap minBound,
      InitialTopologyProcessEdge survivorBootstrap retiredBootstrap minBound,
      InitialTopologyProcessEdge retiredBootstrap localBootstrap minBound
    ]
  where
    (localBootstrap, retiredBootstrap) = case fixtureLocalBootstrapIds of
      [first, second] -> (first, second)
      _ -> error "source-redrive fixture requires two local bootstrap ids"
    survivorBootstrap = fixtureRemoteBootstrapId

retainedPlacementSnapshot :: HeraldState -> PlacementMessage.PlacementSnapshot
retainedPlacementSnapshot state =
  Placement.localSnapshotMessage result
  where
    (_, result) =
      Placement.commitLocalSnapshot
        ( checkedValue
            "reoffer source-redrive placement snapshot"
            (Placement.prepareRetainedLocalSnapshot (startupPlacementState state))
        )

installRemotePlacementSnapshot ::
  HeraldState ->
  PlacementMessage.PlacementSnapshot ->
  IO HeraldState
installRemotePlacementSnapshot state snapshot = do
  prepared <-
    checked
      "install source-redrive remote placement"
      ( Placement.prepareRemotePlacement
          (PlacementMessage.FullPlacementSnapshot snapshot)
          (startupPlacementState state)
      )
  let (placement, _) = Placement.commitRemotePlacement prepared
  pure (replaceStartupPlacementState placement state)

advanceRemotePlacementSnapshot ::
  HeraldEpoch ->
  PlacementMessage.PlacementSnapshot ->
  HeraldState ->
  IO HeraldState
advanceRemotePlacementSnapshot owner predecessorSnapshot state = do
  successorSnapshot <-
    checked
      "advance surviving source placement revision"
      ( PlacementMessage.placementSnapshot
          owner
          ( PlacementMessage.nextPlacementSequence
              (PlacementMessage.placementSnapshotSequence predecessorSnapshot)
          )
          (PlacementMessage.placementSnapshotRoutes predecessorSnapshot)
      )
  installRemotePlacementSnapshot state successorSnapshot

bindFixtureOracle :: HeraldState -> IO HeraldState
bindFixtureOracle state = do
  let client = startupOracleClientState state
  attempt <- case OracleClient.oracleClientActions client of
    [ConnectAndHelloOracle current _] -> pure current
    actions ->
      assertFailure
        ( "expected one initial Oracle connection attempt, got "
            <> show (length actions)
        )
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (controlIndex 0)
          (Just node)
          True
  prepared <-
    checked
      "bind source-redrive Oracle client"
      (OracleClient.prepareClientIngress (OracleHelloReceived attempt acceptance) client)
  pure
    ( replaceStartupOracleClientState
        (OracleClient.commitClientIngress prepared)
        state
    )

promoteSourceRedrivePredecessor ::
  StructuralConsequenceCause ->
  Set.Set HeraldEpoch ->
  HeraldState ->
  IO (SortOccurrence, AlignmentGeneration, HeraldState)
promoteSourceRedrivePredecessor cause expectedMembers state = do
  (placementVector, routesAtVector) <- sourceRedrivePlacementCut state
  (affectedSort, routes) <-
    sourceRedriveThreeMemberRoutes expectedMembers routesAtVector
  topology <- sourceRedriveSyntheticTopology 0x75 state
  let context = sourceRedriveContext routes
      members = sourceRedriveGenerationMembers routes
  plan <-
    case prepareAlignmentGenerationPlan
      affectedSort
      topology
      placementVector
      context
      members
      []
      []
      Set.empty of
      Right (AlignmentGenerationReady ready) -> pure ready
      observed ->
        assertFailure
          ("source-redrive predecessor generation plan was not ready: " <> show observed)
  let generations = alignmentGenerationPlanGenerations plan
      local = heraldMemberEpoch fixtureLocalMember
  generation <-
    generationForHeralds
      "predecessor remote source generation"
      (Set.delete local expectedMembers)
      generations
  assertEqual
    "the synthetic predecessor plan partitions H1 from the H2/H4 source SCC"
    expectedMembers
    ( Set.unions
        ( fmap
            ( Set.fromList
                . fmap DomainAlignment.alignmentMemberHerald
                . NonEmpty.toList
                . DomainAlignment.alignmentCutExactMembers
                . alignmentGenerationCut
            )
            generations
        )
    )
  let withDebt = retainAlignmentDebt cause affectedSort state
  prepared <-
    checked
      "promote source-redrive predecessor generation"
      ( Alignment.prepareAlignmentPromotion
          (heraldMemberEpoch fixtureLocalMember)
          cause
          affectedSort
          (deriveTopologyCutId topology)
          placementVector
          plan
          (startupAlignmentState withDebt)
      )
  let (alignment, _) = Alignment.commitAlignmentPromotion prepared
  pure
    ( affectedSort,
      generation,
      replaceStartupAlignmentState alignment withDebt
    )

promoteSourceRedriveSuccessor ::
  StructuralConsequenceCause ->
  SortOccurrence ->
  Set.Set HeraldEpoch ->
  HeraldState ->
  IO ([AlignmentGeneration], HeraldState)
promoteSourceRedriveSuccessor cause affectedSort expectedMembers state = do
  (placementVector, routesAtVector) <- sourceRedrivePlacementCut state
  let alignment = startupAlignmentState state
      predecessors = Alignment.latestAlignmentGenerationsForSort affectedSort alignment
      predecessorIds = Set.fromList (fmap alignmentGenerationId predecessors)
      predecessorRelations =
        [ relation
        | relation <- Alignment.alignmentGenerationRelationEntries alignment,
          Set.member
            (alignmentGenerationRelationSource relation)
            predecessorIds,
          Set.member
            (alignmentGenerationRelationDestination relation)
            predecessorIds
        ]
      routes =
        sourceRedriveRoutesForSort
          expectedMembers
          affectedSort
          routesAtVector
  assertEqual
    "the successor source selection retains exactly H1 and H2"
    expectedMembers
    (Set.fromList (fmap fst routes))
  topology <- sourceRedriveSyntheticTopology 0x76 state
  let context = sourceRedriveContext routes
      members = sourceRedriveGenerationMembers routes
  plan <-
    case prepareAlignmentGenerationPlan
      affectedSort
      topology
      placementVector
      context
      members
      predecessors
      predecessorRelations
      predecessorIds of
      Right (AlignmentGenerationReady ready) -> pure ready
      observed ->
        assertFailure
          ("source-redrive successor generation plan was not ready: " <> show observed)
  let generations = alignmentGenerationPlanGenerations plan
  assertEqual
    "the synthetic successor plan supersedes H4 with exactly H1 and H2"
    expectedMembers
    ( Set.unions
        ( fmap
            ( Set.fromList
                . fmap DomainAlignment.alignmentMemberHerald
                . NonEmpty.toList
                . DomainAlignment.alignmentCutExactMembers
                . alignmentGenerationCut
            )
            generations
        )
    )
  let withDebt = retainAlignmentDebt cause affectedSort state
  prepared <-
    checked
      "promote source-redrive successor generation"
      ( Alignment.prepareAlignmentPromotion
          (heraldMemberEpoch fixtureLocalMember)
          cause
          affectedSort
          (deriveTopologyCutId topology)
          placementVector
          plan
          (startupAlignmentState withDebt)
      )
  let (successorAlignment, _) = Alignment.commitAlignmentPromotion prepared
  pure (generations, replaceStartupAlignmentState successorAlignment withDebt)

sourceRedrivePlacementCut ::
  HeraldState ->
  IO
    ( DomainAlignment.PhysicalPlacementRevisionVector,
      [(HeraldEpoch, [PlacementMessage.DeltaRoute])]
    )
sourceRedrivePlacementCut state = do
  placementVector <-
    checked
      "derive source-redrive placement vector"
      ( Placement.currentPhysicalPlacementRevisionVector
          (currentMembership state)
          (startupPlacementState state)
      )
  routes <-
    checked
      "read source-redrive placement cut"
      (Placement.placementRoutesAtVector placementVector (startupPlacementState state))
  pure (placementVector, routes)

sourceRedriveThreeMemberRoutes ::
  Set.Set HeraldEpoch ->
  [(HeraldEpoch, [PlacementMessage.DeltaRoute])] ->
  IO (SortOccurrence, [(HeraldEpoch, PlacementMessage.DeltaRoute)])
sourceRedriveThreeMemberRoutes expectedMembers routesAtVector =
  case [ (affectedSort, routes)
       | affectedSort <- candidateSorts,
         let routes =
               sourceRedriveRoutesForSort
                 expectedMembers
                 affectedSort
                 routesAtVector,
         Set.fromList (fmap fst routes) == expectedMembers,
         length routes == Set.size expectedMembers
       ] of
    first : _ -> pure first
    [] -> assertFailure "no private system-view sort has exactly one route on each of H1, H2, and H4"
  where
    candidateSorts =
      Set.toAscList
        ( Set.fromList
            [ sortOccurrence
                (PlacementMessage.deltaRouteSortId route)
                (PlacementMessage.deltaRouteOccurrenceId route)
            | (_, ownerRoutes) <- routesAtVector,
              route <- ownerRoutes,
              PlacementMessage.PrivateSystemViewDeltaRouteView {} <-
                [PlacementMessage.viewDeltaRoute route]
            ]
        )

sourceRedriveRoutesForSort ::
  Set.Set HeraldEpoch ->
  SortOccurrence ->
  [(HeraldEpoch, [PlacementMessage.DeltaRoute])] ->
  [(HeraldEpoch, PlacementMessage.DeltaRoute)]
sourceRedriveRoutesForSort expectedMembers affectedSort routesAtVector =
  [ (owner, route)
  | (owner, ownerRoutes) <- routesAtVector,
    Set.member owner expectedMembers,
    route <- ownerRoutes,
    PlacementMessage.PrivateSystemViewDeltaRouteView {} <-
      [PlacementMessage.viewDeltaRoute route],
    sortOccurrence
      (PlacementMessage.deltaRouteSortId route)
      (PlacementMessage.deltaRouteOccurrenceId route)
      == affectedSort
  ]

sourceRedriveSyntheticTopology :: Word8 -> HeraldState -> IO TopologyCut
sourceRedriveSyntheticTopology digestByte state =
  checked
    "derive the checked source-redrive topology cut rooted at genesis"
    ( topologyCut
        ( sameGenerationPredecessor
            (GraphProgress.structuralLastInstalledCutId progress)
        )
        ( topologyFrontier
            (GraphProgress.structuralAppliedVector progress)
            (GraphProgress.structuralAppliedControlPrefix progress)
        )
        ( checkedValue
            "source-redrive synthetic topology digest"
            (mkTopologyOccurrenceDigest (ByteString.replicate 32 digestByte))
        )
    )
  where
    progress = startupStructuralProgressState state

sourceRedriveContext ::
  [(HeraldEpoch, PlacementMessage.DeltaRoute)] ->
  ContextGraph
sourceRedriveContext routes =
  checkedValue
    "derive the source-redrive remote source class and H1 destination"
    (deriveContextGraph vertices edges deltas)
  where
    local = heraldMemberEpoch fixtureLocalMember
    deltas = fmap (PlacementMessage.deltaRouteDelta . snd) routes
    vertices = fmap DeltaVertex deltas
    localDeltas =
      [ PlacementMessage.deltaRouteDelta route
      | (owner, route) <- routes,
        owner == local
      ]
    remoteDeltas =
      [ PlacementMessage.deltaRouteDelta route
      | (owner, route) <- routes,
        owner /= local
      ]
    edges = case (localDeltas, remoteDeltas) of
      ([destination], [source]) ->
        [edgePayload (DeltaVertex source) (DeltaVertex destination) Preserve]
      ([destination], [firstSource, secondSource]) ->
        [ edgePayload
            (DeltaVertex firstSource)
            (DeltaVertex secondSource)
            Preserve,
          edgePayload
            (DeltaVertex secondSource)
            (DeltaVertex firstSource)
            Preserve,
          edgePayload
            (DeltaVertex firstSource)
            (DeltaVertex destination)
            Preserve
        ]
      _ -> error "source-redrive context requires one H1 and one or two remote routes"

sourceRedriveGenerationMembers ::
  [(HeraldEpoch, PlacementMessage.DeltaRoute)] ->
  [GenerationMemberInput]
sourceRedriveGenerationMembers routes =
  [ generationMemberInput
      (PlacementMessage.deltaRouteDelta route)
      (PlacementMessage.deltaRouteStoreIncarnation route)
      owner
      DomainAlignment.initialStoreRevision
  | (owner, route) <- routes
  ]

generationForHeralds ::
  String ->
  Set.Set HeraldEpoch ->
  [AlignmentGeneration] ->
  IO AlignmentGeneration
generationForHeralds context expected generations =
  case filter (generationHasHeralds expected) generations of
    [generation] -> pure generation
    observed ->
      assertFailure
        ( "expected one source-redrive "
            <> context
            <> " generation, got "
            <> show (length observed)
        )

generationsForSort :: SortOccurrence -> Alignment.State -> [AlignmentGeneration]
generationsForSort affectedSort state =
  [ generation
  | (_, generation) <- Alignment.alignmentGenerationEntries state,
    let cut = alignmentGenerationCut generation,
    sortOccurrence
      (DomainAlignment.alignmentCutSortId cut)
      (DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut)
      == affectedSort
  ]

retainAlignmentDebt ::
  StructuralConsequenceCause ->
  SortOccurrence ->
  HeraldState ->
  HeraldState
retainAlignmentDebt cause affectedSort state =
  replaceStartupAlignmentState successor state
  where
    debt =
      normalizeStructuralDebts
        [ structuralConsequenceDebt
            (structuralDebtKey cause TopologyAlignmentDebt affectedSort Nothing)
            (structuralDebtEvidence Set.empty Set.empty Set.empty)
        ]
    successor =
      fst
        ( Alignment.commitStructuralDebtRetention
            ( checkedValue
                "retain source-redrive alignment debt"
                ( Alignment.prepareStructuralDebtRetention
                    cause
                    debt
                    (startupAlignmentState state)
                )
            )
        )

generationHasHeralds :: Set.Set HeraldEpoch -> AlignmentGeneration -> Bool
generationHasHeralds expected generation =
  Set.fromList
    ( fmap
        DomainAlignment.alignmentMemberHerald
        (NonEmpty.toList (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation)))
    )
    == expected

generationMemberFor ::
  String ->
  HeraldEpoch ->
  AlignmentGeneration ->
  IO DomainAlignment.AlignmentMember
generationMemberFor context herald generation =
  case filter
    ((== herald) . DomainAlignment.alignmentMemberHerald)
    (NonEmpty.toList (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation))) of
    [member] -> pure member
    observed ->
      assertFailure
        (context <> ": expected one member, got " <> show (length observed))

retainSourceRedriveEvidence ::
  [HeraldEpoch] ->
  [AlignmentGeneration] ->
  Alignment.State ->
  Alignment.State
retainSourceRedriveEvidence certifiedHeralds generations initial =
  foldl retainCertificate withReadiness certificateInputs
  where
    members =
      [ (generation, member)
      | generation <- generations,
        member <-
          NonEmpty.toList
            (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation))
      ]
    withAcceptances =
      foldl
        (\state (generation, herald) -> retainCutAcceptanceFrom generation herald state)
        initial
        [ (generation, herald)
        | generation <- generations,
          herald <- certifiedHeralds
        ]
    readiness =
      [ ( generation,
          member,
          AlignmentProtocol.classMemberReady
            (evidenceSequenceAt index)
            (alignmentGenerationId generation)
            (DomainAlignment.alignmentMemberStoreIncarnation member)
            ( DomainAlignment.deriveMemberReadyEvidenceDigest
                (ByteString.pack [fromIntegral index, 0x9a])
            )
            DomainAlignment.initialStoreRevision
        )
      | (index, (generation, member)) <- zip [0 ..] members
      ]
    withReadiness =
      foldl
        ( \state (_, _, ready) ->
            fst
              ( Alignment.commitClassMemberReady
                  ( checkedValue
                      "retain source-redrive member readiness"
                      (Alignment.prepareClassMemberReady ready state)
                  )
              )
        )
        withAcceptances
        readiness
    certificateInputs =
      [ (generation, ready)
      | (generation, member, ready) <- readiness,
        DomainAlignment.alignmentMemberHerald member `elem` certifiedHeralds
      ]
    retainCertificate state (generation, ready) =
      fst
        ( Alignment.commitHistoricalCertificate
            ( checkedValue
                "retain source-redrive historical certificate"
                (Alignment.prepareHistoricalCertificate certificate state)
            )
        )
      where
        generationReadiness =
          [ retainedReady
          | (retainedGeneration, _, retainedReady) <- readiness,
            alignmentGenerationId retainedGeneration == alignmentGenerationId generation
          ]
        certificate =
          AlignmentProtocol.historicalCertificate
            (alignmentGenerationId generation)
            (AlignmentProtocol.classMemberReadyStoreIncarnation ready)
            (AlignmentProtocol.classMemberReadySetDigest generationReadiness)
            ( DomainAlignment.deriveBootstrapEvidenceDigest
                ( DomainAlignment.memberReadyEvidenceDigestBytes
                    (AlignmentProtocol.classMemberReadyPredecessorAndBasePrefixDigest ready)
                )
            )
            DomainAlignment.initialStoreRevision

retainCutAcceptanceFrom ::
  AlignmentGeneration ->
  HeraldEpoch ->
  Alignment.State ->
  Alignment.State
retainCutAcceptanceFrom generation herald state =
  fst
    ( Alignment.commitAlignmentCutAcceptance
        ( checkedValue
            "retain source-redrive cut acceptance"
            (Alignment.prepareAlignmentCutAcceptance acceptance state)
        )
    )
  where
    cut = alignmentGenerationCut generation
    acceptance =
      AlignmentProtocol.alignmentCutAccepted
        (alignmentGenerationId generation)
        herald
        (deriveTopologyCutId (DomainAlignment.alignmentCutTopologyCut cut))
        (DomainAlignment.alignmentCutPhysicalPlacementRevisionVector cut)
        DomainAlignment.EmptyHeraldPublicationPrefix

evidenceSequenceAt :: Int -> AlignmentProtocol.AlignmentEvidenceSequence
evidenceSequenceAt index =
  iterate
    AlignmentProtocol.nextAlignmentEvidenceSequence
    AlignmentProtocol.firstAlignmentEvidenceSequence
    !! index

installOrdinaryAttemptFrom ::
  HeraldEpoch ->
  HeraldEpoch ->
  AlignmentProtocol.AlignmentObligationId ->
  Alignment.State ->
  IO Alignment.State
installOrdinaryAttemptFrom survivor retired identifier state = do
  obligation <-
    maybe
      (assertFailure "source-redrive ordinary obligation is absent")
      pure
      (Alignment.lookupAlignmentObligation identifier state)
  sourceGeneration <-
    maybe
      (assertFailure "source-redrive source generation is absent")
      pure
      ( Alignment.lookupAlignmentGeneration
          (AlignmentProtocol.alignmentObligationSourceGeneration obligation)
          state
      )
  survivingMember <- generationMemberFor "surviving source member" survivor sourceGeneration
  prepared <-
    checked
      "select retiring ordinary source"
      ( Alignment.prepareAlignmentAttemptExcluding
          ( Set.singleton
              ( Alignment.alignmentSourceCoordinate
                  survivor
                  (DomainAlignment.alignmentMemberStoreIncarnation survivingMember)
              )
          )
          identifier
          state
      )
  let attempt = Alignment.preparedAlignmentAttempt prepared
  assertEqual
    "the exclusion fixture selects the exact retiring Herald"
    retired
    (AlignmentProtocol.alignmentAttemptSourceHerald attempt)
  let (withAttempt, _) = Alignment.commitAlignmentAttempt prepared
  preparedTransfer <-
    checked
      "retain retiring ordinary destination transcript"
      ( AlignmentTransferState.prepareDestinationAttempt
          attempt
          (Alignment.alignmentTransferState withAttempt)
      )
  let (transfer, _) = AlignmentTransferState.commitDestinationAttempt preparedTransfer
  pure (Alignment.replaceAlignmentTransferState transfer withAttempt)

fulfillPredecessorImportFrom ::
  HeraldEpoch ->
  Alignment.BootstrapImportKey ->
  Alignment.State ->
  IO Alignment.State
fulfillPredecessorImportFrom selectedSource key state = do
  sourceGenerationId <- case Alignment.bootstrapImportKeySource key of
    Alignment.BootstrapFromPredecessor generation -> pure generation
    Alignment.BootstrapFromFreshBase _ ->
      assertFailure "source-redrive fixture selected a fresh-base import"
  sourceGeneration <-
    maybe
      (assertFailure "source-redrive predecessor import generation is absent")
      pure
      (Alignment.lookupAlignmentGeneration sourceGenerationId state)
  let excludedSources =
        Set.fromList
          [ Alignment.alignmentSourceCoordinate
              (DomainAlignment.alignmentMemberHerald member)
              (DomainAlignment.alignmentMemberStoreIncarnation member)
          | member <-
              NonEmpty.toList
                (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut sourceGeneration)),
            DomainAlignment.alignmentMemberHerald member /= selectedSource
          ]
  preparedAttempt <-
    checked
      "select the exact fulfilled-import source"
      ( Alignment.prepareBootstrapImportAttemptExcluding
          excludedSources
          key
          state
      )
  let attempt = Alignment.preparedBootstrapImportAttempt preparedAttempt
  assertEqual
    "the fulfilled import selects its exact independent source"
    selectedSource
    (Alignment.bootstrapImportAttemptSourceHerald attempt)
  let (attempted, _) = Alignment.commitBootstrapImportAttempt preparedAttempt
      subscribe = Alignment.bootstrapImportAttemptSubscribe attempt
      subscription = AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe
      sourceStore = AlignmentProtocol.alignmentSubscribeSourceStoreIncarnation subscribe
      digest =
        AlignmentProtocol.alignmentSemanticSnapshotDigest
          sourceGenerationId
          sourceStore
          DomainAlignment.initialStoreRevision
          []
      controls =
        [ AlignmentProtocol.AlignmentSnapshotStarted
            ( AlignmentProtocol.alignmentSnapshotStart
                subscription
                DomainAlignment.initialStoreRevision
                digest
                1
            ),
          AlignmentProtocol.AlignmentSnapshotChunkTransferred
            (AlignmentProtocol.alignmentSnapshotChunk subscription 0 []),
          AlignmentProtocol.AlignmentSnapshotEnded
            ( AlignmentProtocol.alignmentSnapshotEnd
                subscription
                DomainAlignment.initialStoreRevision
                digest
            ),
          AlignmentProtocol.AlignmentLiveAdvertised
            (AlignmentProtocol.alignmentLive subscription DomainAlignment.initialStoreRevision)
        ]
      completedTransfer =
        foldl
          applyDestinationControl
          (Alignment.alignmentTransferState attempted)
          controls
      completed = Alignment.replaceAlignmentTransferState completedTransfer attempted
  preparedFulfillment <-
    checked
      "fulfill surviving-source predecessor import"
      ( Alignment.prepareBootstrapImportFulfillment
          attempt
          DomainAlignment.initialStoreRevision
          completed
      )
  pure (fst (Alignment.commitBootstrapImportFulfillment preparedFulfillment))

applyDestinationControl ::
  AlignmentTransferState.State ->
  AlignmentProtocol.AlignmentControl ->
  AlignmentTransferState.State
applyDestinationControl predecessor control = case control of
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
            (checkedValue "apply source-redrive destination control" prepared)
        )

projectedResidentEnds ::
  ControlIndex ->
  HeraldEpoch ->
  HeraldState ->
  [OracleProjection.MembershipAdvanceProcessEnd]
projectedResidentEnds index residence state =
  [ OracleProjection.membershipAdvanceProcessEnd
      process
      index
      fixtureHeraldRetirementReason
  | process <- OracleProjection.projectedProcessEpochs projection,
    OracleProjection.oracleViewProcessResidence process view == Just residence,
    OracleProjection.oracleViewProcessIsLive process view
  ]
  where
    projection = startupOracleProjectionState state
    view = OracleProjection.oracleView projection

data RetirementClosureFixture = RetirementClosureFixture
  { predecessor :: HeraldState,
    publication :: PublicationId,
    retired :: HeraldEpoch,
    index :: ControlIndex,
    successorMembership :: HeraldMembershipGeneration,
    membershipAdvance :: MembershipAdvance.PreparedMembershipAdvance,
    establishedBase :: StructuralBase.MembershipBaseClosure
  }

retirementClosureFixture :: IO RetirementClosureFixture
retirementClosureFixture = do
  (predecessor, publication, _, retired) <- heldControlIndexTwoForMembershipAdvance
  retirementClosureFixtureFrom predecessor publication retired

successorHeldClosureFixture :: IO (RetirementClosureFixture, HeraldState)
successorHeldClosureFixture = do
  source <- TransitionFixture.successorStructuralHeldFixture
  let predecessor = TransitionFixture.successorStructuralHeldMembershipPredecessor source
      held = TransitionFixture.successorStructuralHeldState source
      membership = TransitionFixture.successorStructuralHeldMembership source
      publication = publicationBatchId (peerPublicationBatch (sequencedItemPayload (TransitionFixture.successorStructuralHeldItem source)))
  retired <- case Set.toList (Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (currentMembership predecessor))) Set.\\ Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))) of
    [one] -> pure one
    _ -> assertFailure "successor-held fixture needs one actual retired origin"
  fixture <- retirementClosureFixtureFrom predecessor publication retired
  assertPublicationDisposition "the G1 publication is genuinely held before its base" Publication.DependencyHeld publication held
  pure (fixture, held)

retirementClosureFixtureFrom :: HeraldState -> PublicationId -> HeraldEpoch -> IO RetirementClosureFixture
retirementClosureFixtureFrom predecessor publication retired = do
  let index = controlIndex 2
      predecessorMembership = currentMembership predecessor
      successorMembership =
        checkedValue
          "successor membership"
          (retireHeraldMembershipGeneration index (MembershipFixtures.fixtureRetirementResolution index) retired predecessorMembership)
  membershipAdvance <-
    checked
      "prepare fresh membership advance"
      ( MembershipAdvance.prepareMembershipAdvance
          index
          successorMembership
          []
          predecessor
      )
  baseCoordinator <-
    checked
      "begin structural-base composition"
      ( StructuralBase.beginStructuralBaseAfterAppliedMembershipAdvance
          []
          predecessor
          (MembershipAdvance.commitMembershipAdvance membershipAdvance)
      )
  establishedBase <-
    establishEmptyStructuralBase
      predecessor
      membershipAdvance
      baseCoordinator
  pure
    RetirementClosureFixture
      { predecessor,
        publication,
        retired,
        index,
        successorMembership,
        membershipAdvance,
        establishedBase
      }

prepareFreshClosure :: RetirementClosureFixture -> IO PreparedRetirementClosure
prepareFreshClosure fixture =
  checked
    "prepare fresh retirement closure"
    ( prepareRetirementClosure
        fixture.predecessor
        (MembershipAdvance.commitMembershipAdvance fixture.membershipAdvance)
        fixture.membershipAdvance
        fixture.establishedBase
    )

establishEmptyStructuralBase ::
  HeraldState ->
  MembershipAdvance.PreparedMembershipAdvance ->
  StructuralBase.MembershipBaseClosure ->
  IO StructuralBase.MembershipBaseClosure
establishEmptyStructuralBase predecessor membershipAdvance =
  establishEmptyStructuralBaseForStates predecessor (MembershipAdvance.commitMembershipAdvance membershipAdvance)

establishEmptyStructuralBaseForStates ::
  HeraldState ->
  HeraldState ->
  StructuralBase.MembershipBaseClosure ->
  IO StructuralBase.MembershipBaseClosure
establishEmptyStructuralBaseForStates predecessor successor initial = do
  let predecessorMembership = StructuralBase.structuralBasePredecessorMembership initial
      successorMembership = StructuralBase.structuralBaseSuccessorMembership initial
      retired = case Set.toList (StructuralBase.structuralBaseRetiredSources initial) of
        [origin] -> origin
        _ -> error "single-retirement fixture has a compound source set"
      reporters =
        NonEmpty.toList
          (heraldMembershipGenerationActiveHeraldEpochs successorMembership)
      inventories =
        [ checkedValue
            "empty survivor terminal inventory"
            ( terminalSourceInventory
                (MembershipFixtures.fixtureGenesisRetirementLineage predecessorMembership successorMembership)
                retired
                reporter
                []
                []
                []
            )
        | reporter <- reporters
        ]
      predecessorProgress = startupStructuralProgressState predecessor
      successorProgress = startupStructuralProgressState successor
      predecessorBase =
        checkedValue
          "fixture genesis predecessor base"
          ( terminalSourceGenesisPredecessorBase
              predecessorMembership
              (GraphProgress.structuralLastInstalledCutId predecessorProgress)
          )
      union =
        checkedValue
          "empty terminal-source union"
          ( deriveTerminalSourceUnion
              (MembershipFixtures.fixtureGenesisRetirementLineage predecessorMembership successorMembership)
              (MembershipFixtures.fixtureGenesisRetirementLineage predecessorMembership successorMembership)
              predecessorBase
              inventories
              emptyTerminalSourcePayloadArchive
              (constantDigest terminalTopologyDigest)
          )
      ready =
        checkedValue
          "empty terminal-source readiness"
          ( terminalSourceUnionReady
              (GraphProgress.structuralAppliedVector successorProgress)
              (GraphProgress.structuralAppliedControlPrefix successorProgress)
              union
          )
      acceptances =
        [ checkedValue
            "successor terminal-source acceptance"
            (terminalSourceUnionAcceptance successorMembership reporter ready)
        | reporter <- reporters
        ]
      established =
        checkedValue
          "established empty terminal-source union"
          (terminalSourceUnionEstablished successorMembership union acceptances)
  withInventories <-
    foldM
      ( \coordinator inventory ->
          checked
            "receive exact terminal inventory"
            (StructuralBase.receiveStructuralBaseInventory inventory coordinator)
      )
      initial
      inventories
  checked
    "receive established empty terminal-source union"
    ( StructuralBase.receiveStructuralBaseUnionEstablished
        (\vector index -> Right (constantDigest terminalTopologyDigest vector index))
        (GraphProgress.structuralAppliedVector successorProgress)
        (GraphProgress.structuralAppliedControlPrefix successorProgress)
        established
        withInventories
    )

terminalTopologyDigest :: TopologyOccurrenceDigest
terminalTopologyDigest =
  checkedValue
    "terminal topology digest"
    (mkTopologyOccurrenceDigest (ByteString.replicate 32 0x72))

constantDigest ::
  TopologyOccurrenceDigest ->
  structuralVector ->
  ControlIndex ->
  TopologyOccurrenceDigest
constantDigest digest _ _ = digest

currentMembership :: HeraldState -> HeraldMembershipGeneration
currentMembership =
  OracleProjection.oracleViewCurrentHeraldMembership
    . OracleProjection.oracleView
    . startupOracleProjectionState

assertPublicationDisposition ::
  String ->
  Publication.IncomingDisposition ->
  PublicationId ->
  HeraldState ->
  Assertion
assertPublicationDisposition context expected identifier state =
  case Publication.lookupIncomingPublication identifier (startupPublicationState state) of
    Nothing -> assertFailure (context <> ": publication is absent")
    Just record ->
      assertEqual context expected (Publication.incomingPublicationDisposition record)

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id
