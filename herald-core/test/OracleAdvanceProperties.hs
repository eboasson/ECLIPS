{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OracleAdvanceProperties
  ( tests,
    heldControlIndexTwoForMembershipAdvance,
    acceptedVoterFailureHeldFixture,
    acceptedVoterFailureHeldFixtureWithEntry,
    terminalPayloadReadinessFixture,
    dynamicStartRetirementFixture,
    fixtureFailureHeraldGenesis,
  )
where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Result (RegularCallResult (NewIdCompleted))
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write (ApplicationWriteValue (PublishValue))
import Eclips.Domain.Graph (VertexId (NeutralVertex))
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    PublicationId,
    controlIndex,
    firstStructuralSequence,
    globalObjectIdFromGlobalUniqueId,
    globalUniqueIdFromGlobalObjectId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkHeraldId,
    mkProcessEpochId,
    mkProcessId,
    mkTopologyCutId,
    nablaSequence,
    publicationId,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( FailureProbeResolution (RetireFailureProbeTarget),
    HeraldMembershipGeneration,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
  )
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart (ProcessStart, processStart, processStartProcessEpochId, processStartProcessId, processStartResidence)
import Eclips.Domain.Publication
  ( checkedPublicationCanonicalValue,
    mkCheckedPublication,
  )
import Eclips.Domain.Route (ReplicaStrength (Normal))
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (NeutralVertexCarrier))
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (NeutralVertexRole),
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.Startup
  ( AppliedRootRole (WriterRoot),
    HeraldMember (..),
    appliedBootstrapManifestId,
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootOccurrenceId,
    appliedRootRole,
    appliedRootSortId,
    configuredProcessBootstrapProcessEpochId,
    configuredProcessBootstrapProcessId,
    configuredProcessBootstrapResidence,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
  )
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value
  ( LabelOwner (VoidLabel),
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    recordValue,
  )
import Eclips.Herald.Administration
  ( adminCorrelationId,
    adminCorrelationIdForBinding,
    checkedInitialAdministrationBinding,
    startProcessRequest,
  )
import Eclips.Herald.Application.Request qualified as ApplicationRequest
import Eclips.Herald.Application.Session
  ( ApplicationSessionUnavailableReason (ApplicationHeraldRetired),
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
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (..),
    DeploymentManifest (..),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
    checkJoiningHeraldGenesis,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedSystemId,
    lookupConfiguredProcessBootstrap,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.Initialization
  ( HeraldState,
    initialHerald,
    initialHeraldWithIsolation,
    initialHeraldWithOracleVoters,
  )
import Eclips.Herald.Input
  ( AdministrationIngress
      ( OpenAdministrationConnection,
        OrderlyHeraldShutdown,
        StartProcessEpoch
      ),
    ApplicationRequestIngress (CallApplicationRequest),
    HeraldInputBody (AdministrationInput, ApplicationRequestInput, JoinInput, OracleInput, PeerInput, RuntimeObserved),
    PeerControl (..),
    PeerIngress (PeerControlReceived, PeerHelloReceived),
    RuntimeObservation (PeerBindingLost, TimerObserved),
    candidateAdministrationLane,
    heraldInput,
    peerPublicationReceived,
  )
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.Join qualified as Join
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction (..),
    OracleClientDiagnostic (StaleOracleClientIngress),
    OracleClientIngress (..),
    OracleConnectAttempt,
    OracleContactProblem (..),
    OracleRetryPurpose (..),
    checkOracleContactSet,
    oracleBindingContact,
    oracleCandidateLane,
    oracleConnectAttemptContact,
    oracleConnectAttemptFromExclusive,
    oracleContact,
    oracleContactNode,
    oracleContacts,
    oracleEstablishedLane,
    oracleHelloAcceptance,
    oracleHelloConfigurationDigest,
    oracleHelloMembershipGeneration,
    oracleNodeClaim,
    oracleObservedTerm,
    oracleRedirect,
    oracleRequestDispatchRef,
  )
import Eclips.Herald.OracleClient.State
  ( OracleClientProblem (OracleMembershipCursorEntryMismatch),
    State,
    commitClientIngress,
    commitCursorAdvance,
    commitOracleRequest,
    oracleClientActions,
    oracleClientCurrentBinding,
    oracleClientHelloClaims,
    oracleClientRequestActions,
    oracleClientStateWitness,
    oracleClientWitnessAppliedCursor,
    oracleClientWitnessMembershipAdvances,
    oracleClientWitnessRequestedCursor,
    oracleClientWitnessRetainedEntryBytes,
    oracleClientWitnessRetired,
    oracleRequestRefRequestId,
    prepareCanonicalMembershipCursorAdvance,
    prepareClientIngress,
    prepareCursorAdvance,
    prepareStartProcessEpochRequest,
    preparedClientIngressActions,
  )
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection qualified as OracleProjection
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.PeerDispatch.Internal (peerPublicationItem)
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PublicationDestination,
    mkPublicationBatch,
    mkStructuralOccurrenceStamp,
    mkStructuralPeerPublication,
    peerPublicationDigest,
    peerPublicationStructuralCanonicalBytes,
    peerPublicationStructuralStamp,
    publicationDestination,
    publicationDestinationDelta,
    structuralPublicationDigestFor,
  )
import Eclips.Herald.PeerStream
  ( SequencedItem,
    emptyStreamPrefix,
    firstStreamSequence,
    mkStreamDirection,
    nextAfterStreamPrefix,
    resumeResponse,
    sequencedItem,
    sequencedItemDirection,
    sequencedItemPayload,
    streamCompletedPrefix,
    streamCompletionPrefix,
    streamPrefixSequence,
    streamPrefixThrough,
    streamReceivedPrefix,
    streamSequenceWord64,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Publication.Groups qualified as PublicationGroups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldStartupInvariant, HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldOracleTransitionContradiction),
    StartupInvariantSubject (StartupStatic),
    StartupInvariantViolation (OracleClientEvidenceInvariant),
    validateHeraldState,
  )
import Eclips.Herald.Startup.State
  ( StartupStateWitness (..),
    replaceStartupApplicationState,
    replaceStartupLastObservedTime,
    replaceStartupOracleClientState,
    replaceStartupProcessPreparationState,
    startupApplicationState,
    startupControlOracleProjectionState,
    startupControlledState,
    startupGraphState,
    startupIsolationState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupProcessPreparationState,
    startupPublicationState,
    startupStateWitness,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Time (monotonicInstant, monotonicInstantWord64)
import Eclips.Herald.Timer
  ( TimerOutcome (TimerFired),
    timerSpecAbsoluteDeadline,
  )
import Eclips.Herald.Timer.Internal (timerAttemptIsolationDrain)
import Eclips.Herald.Transition (HeraldPhase (HeraldDraining), heraldPhase)
import Eclips.Oracle.Admission qualified as Admission
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
    beginHeraldAdmissionCommand,
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
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    RaftVoterBinding,
    checkOracleGenesis,
    deriveRaftConfigurationDigest,
    oracleGenesis,
    raftVoterBinding,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection (AppliedOracleEntry, appliedEntryControlIndex)
import Eclips.Oracle.State (OracleState, oracleCurrentMembership, oracleGreatestControlIndex, oraclePendingHeraldAdmission)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import Eclips.Raft.Genesis
  ( RaftNativeConfiguration,
    checkRaftGenesis,
    checkedRaftNativeConfiguration,
    raftGenesis,
  )
import Eclips.Raft.Identity
  ( RaftNodeId,
    mkRaftDurationMicros,
    mkRaftNodeId,
  )
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureDeploymentManifest,
    fixtureGeneratorSeed,
    fixtureHeraldAfterProbePrefix,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
    initialEffectsAreOracleWatchAndGrace,
  )
import NativeVoterFailureFixtures qualified as NativeFailure
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
    "Step-13 Oracle watch and process projection"
    [ testCase "Oracle contacts are exact-width, nonempty, unique, and normalized" caseContactAdmission,
      testCase "index-zero Herald and Oracle genesis projections have exact parity" caseGenesisParity,
      testCase "topology control checkpoints retain only accepted Start semantics" caseTopologyControlCheckpoint,
      testCase "accepted Starts cannot reuse any retained stable process identity" caseStartedProcessIdAlreadyUsed,
      testCase "startup, retry, and stale callbacks retain one watch intention" caseRetry,
      testCase "a not-ready preferred leader waits before fixed-contact retry" caseHintedRetryRotation,
      testCase "a rejected candidate waits before following its admitted hint" caseRejectedCandidateRetry,
      testCase "fresh distinct redirect hints connect immediately while repeated or regressed hints wait" caseRedirectRetryHints,
      testCase "binding loss retains a distinct leader hint for one exact attempt" caseDistinctHintAfterBindingLoss,
      testCase "submission NotReady retains one lane and one deterministic reoffer timer" caseSubmissionNotReady,
      testCase "Oracle lifecycle fast paths reoffer one retained submission without driving other owners" caseLifecycleFastPath,
      testCase "ConnectFailed fast path changes only the Oracle owner" caseConnectFailedFastPath,
      testCase "Redirect fast path changes only the Oracle owner" caseRedirectFastPath,
      testCase "BindingLost fast path changes only the Oracle owner" caseBindingLostFastPath,
      testCase "a contiguous entry advances client and projection atomically" caseContiguous,
      testCase "a fresh Start entry releases generic control waiters and updates the local report" caseControlledRelease,
      testCase "a local announcer retains changed structural progress without broadcasting reports" caseStructuralReportPerBinding,
      testCase "same-generation structural advances emit only the final snapshot" caseDistinctStructuralReportOrder,
      testCase "a gap leaves generic control work held and rolls every semantic owner back" caseControlledGap,
      testCase "a rejected Start still releases target-local controlled data" caseControlledRejection,
      testCase "a rejected Start advances the cursor without a process delta" caseRejectedEntry,
      testCase "reclaimed foreign rejections use checked prefix coverage" caseSparseDuplicate,
      testCase "an exact overlapping entry is idempotent" caseDuplicate,
      testCase "an equal overlapping prefix advances only its fresh suffix" caseOverlap,
      testCase "whole-state validation binds client evidence to projection evidence" caseEvidenceInvariant,
      testCase "a gap changes neither cursor and rewatches the retained prefix" caseGap,
      testCase "an unequal pinned already-applied entry is an invariant fault" caseConflict,
      testCase "connection loss reconnects from the exact applied cursor" caseReconnect,
      testCase "peer completion and resume progress clear real publication group frontiers" casePublicationGroupPeerProgress,
      testCase "a live membership cut atomically advances survivor reconnect claims" caseMembershipCutoverReconnect,
      testCase "self-fencing keeps sparse live control evidence separate from frozen semantics" caseSelfFencedSparseDuplicate,
      testCase "local retirement freezes one coherent read-only branch then applies End" caseLocalMembershipCutoverDrain,
      testCase "joining native-failure replay retains no local retirement request" caseJoiningNativeFailureReplay,
      testCase "active native-failure projection retains one local retirement request" caseActiveNativeFailureFollowUp,
      testCase "drain retires Oracle retry and watch intention" caseDrain
    ]

caseContactAdmission :: Assertion
caseContactAdmission = do
  assertEqual
    "node claims require exactly 32 bytes"
    (Left (OracleNodeClaimWrongByteCount 31))
    (oracleNodeClaim (ByteString.replicate 31 0x91))
  firstNode <- checkedIO "first Oracle node" (oracleNodeClaim (ByteString.replicate 32 0x91))
  secondNode <- checkedIO "second Oracle node" (oracleNodeClaim (ByteString.replicate 32 0x92))
  first <- checkedIO "first Oracle contact" (oracleContact firstNode "oracle-b" 29101)
  second <- checkedIO "second Oracle contact" (oracleContact secondNode "oracle-a" 29102)
  forward <- checkedIO "forward contact set" (checkOracleContactSet [first, second])
  reverseOrder <- checkedIO "reverse contact set" (checkOracleContactSet [second, first])
  assertEqual "presentation order is normalized" forward reverseOrder
  assertEqual "the set must be nonempty" (Left OracleContactSetEmpty) (checkOracleContactSet [])
  assertEqual
    "one node has one endpoint"
    (Left (OracleContactNodeDuplicate firstNode))
    (checkOracleContactSet [first, first])
  sameEndpoint <- checkedIO "same endpoint" (oracleContact secondNode "oracle-b" 29101)
  assertEqual
    "one endpoint has one node"
    (Left (OracleContactEndpointDuplicate "oracle-b" 29101))
    (checkOracleContactSet [first, sameEndpoint])
  assertEqual "empty hosts reject" (Left OracleContactHostEmpty) (oracleContact firstNode "" 29101)
  assertEqual "port zero rejects" (Left OracleContactPortZero) (oracleContact firstNode "oracle-a" 0)

caseGenesisParity :: Assertion
caseGenesisParity = do
  (initial, effects) <- initialFixture
  case OracleProjection.validateOracleGenesisParity
    fixtureCheckedOracleGenesis
    (startupOracleProjectionState initial) of
    Right _ -> pure ()
    Left fault -> assertFailure ("matching genesis projections disagree: " <> show fault)
  assertBool "initial Oracle watch and isolation grace" (initialEffectsAreOracleWatchAndGrace effects)
  case [hello | RunOracleClientAction (ConnectAndHelloOracle _ hello) <- effectBatchMembers effects] of
    [claims] ->
      do
        assertEqual
          "the outbound Hello carries the checked deployment configuration digest"
          (checkedConfigurationDigest fixtureHeraldGenesis)
          (oracleHelloConfigurationDigest claims)
    unexpected -> assertFailure ("expected initial Oracle Hello, got " <> show unexpected)

caseTopologyControlCheckpoint :: Assertion
caseTopologyControlCheckpoint = do
  let initial =
        OracleProjection.initialState fixtureHeraldGenesis fixtureHeraldBootstraps
  genesisCheckpoint <-
    checkedIO
      "genesis topology checkpoint"
      (OracleProjection.topologyControlCheckpointAt (controlIndex 0) initial)
  firstPrepared <-
    checkedIO
      "first Start projection"
      (OracleProjection.prepareAppliedEntry fixtureEntryOne initial)
  let afterFirst = OracleProjection.commitAppliedEntry firstPrepared
  firstCheckpoint <-
    checkedIO
      "first Start topology checkpoint"
      (OracleProjection.topologyControlCheckpointAt (controlIndex 1) afterFirst)
  assertBool
    "an accepted Start changes the semantic checkpoint"
    (genesisCheckpoint /= firstCheckpoint)
  rejectedPrepared <-
    checkedIO
      "rejected successor projection"
      (OracleProjection.prepareAppliedEntry fixtureRejectedEntryTwo afterFirst)
  let afterRejected = OracleProjection.commitAppliedEntry rejectedPrepared
  olderCheckpoint <-
    checkedIO
      "older retained topology checkpoint"
      (OracleProjection.topologyControlCheckpointAt (controlIndex 1) afterRejected)
  rejectedCheckpoint <-
    checkedIO
      "rejected-prefix topology checkpoint"
      (OracleProjection.topologyControlCheckpointAt (controlIndex 2) afterRejected)
  assertEqual
    "later rejection/request metadata changes no semantic checkpoint"
    firstCheckpoint
    rejectedCheckpoint
  assertEqual
    "an older prefix reconstructs exactly"
    firstCheckpoint
    olderCheckpoint
  assertEqual
    "a requested prefix cannot exceed retained control progress"
    ( Left
        ( OracleProjection.TopologyControlCheckpointAhead
            (controlIndex 3)
            (controlIndex 2)
        )
    )
    (OracleProjection.topologyControlCheckpointAt (controlIndex 3) afterRejected)

caseStartedProcessIdAlreadyUsed :: Assertion
caseStartedProcessIdAlreadyUsed = do
  let processId = processStartProcessId fixtureRepeatedProcessSecondStart
      genesisProjection =
        OracleProjection.initialState
          fixtureRepeatedProcessGenesis
          fixtureRepeatedProcessGenesisBootstraps
  assertProjectionFault
    "a Start cannot reuse a genesis process's stable identity"
    (OracleProjection.StartedProcessIdAlreadyUsed processId)
    (OracleProjection.prepareAppliedEntry fixtureRepeatedProcessSecondEntryAtOne genesisProjection)

  firstPrepared <-
    checkedIO
      "project the first live epoch"
      ( OracleProjection.prepareAppliedEntry
          fixtureRepeatedProcessFirstEntry
          ( OracleProjection.initialState
              fixtureRepeatedProcessGenesis
              fixtureRepeatedProcessEmptyBootstraps
          )
      )
  assertProjectionFault
    "a Start cannot reuse a dynamically started process's stable identity"
    (OracleProjection.StartedProcessIdAlreadyUsed processId)
    ( OracleProjection.prepareAppliedEntry
        fixtureRepeatedProcessSecondEntryAtTwo
        (OracleProjection.commitAppliedEntry firstPrepared)
    )

  let firstStart = fixtureRepeatedProcessFirstStart
      home = processStartResidence firstStart
      (oracleStarted, _) = committed fixtureRepeatedProcessOracleInitialState (fixtureEnvelope 1 firstStart)
      (_, endEntry) =
        committed
          oracleStarted
          ( oracleEnvelope
              (oracleClientRequestId home 2)
              Nothing
              home
              (checked "end first dynamic process" (endProcessEpochCommand (processStartProcessEpochId firstStart) ExplicitAdministrativeEnd))
          )
      rejectedAt ordinal oracle = fst (committed oracle (fixtureEnvelopeAt ordinal fixtureRepeatedProcessSecondStart (Just (controlIndex 99))))
      independentOracle = rejectedAt 2 (rejectedAt 1 fixtureRepeatedProcessOracleInitialState)
      (_, repeatedAtThree) = committed independentOracle (fixtureEnvelope 3 fixtureRepeatedProcessSecondStart)
  endedPrepared <-
    checkedIO
      "retain End before checking process identity freshness"
      (OracleProjection.prepareAppliedEntry (canonicalizeAppliedOracleEntry endEntry) (OracleProjection.commitAppliedEntry firstPrepared))
  assertProjectionFault
    "End retains the stable process identity and cannot admit a different epoch for it"
    (OracleProjection.StartedProcessIdAlreadyUsed processId)
    (OracleProjection.prepareAppliedEntry (canonicalizeAppliedOracleEntry repeatedAtThree) (OracleProjection.commitAppliedEntry endedPrepared))

assertProjectionFault ::
  String ->
  OracleProjection.OracleProjectionFault ->
  Either OracleProjection.OracleProjectionFault value ->
  Assertion
assertProjectionFault context expected result =
  case result of
    Left observed -> assertEqual context expected observed
    Right _ -> assertFailure (context <> ": projection unexpectedly succeeded")

caseRetry :: Assertion
caseRetry = do
  (initial, initialEffects) <- initialFixture
  firstAttempt <- initialAttempt initialEffects
  (retrying, retryEffects) <-
    checkedIO
      "observe failed Oracle connect"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 1) (OracleInput (OracleConnectFailed firstAttempt)))
          initial
      )
  retry <- case effectBatchMembers retryEffects of
    [RunOracleClientAction (ScheduleOracleRetry OracleConnectionRetry value)] -> pure value
    effects -> assertFailure ("expected one scheduled retry, got " <> show effects)
  (connecting, connectEffects) <-
    checkedIO
      "observe Oracle retry timer"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 2) (OracleInput (OracleRetryElapsed retry)))
          retrying
      )
  secondAttempt <- case effectBatchMembers connectEffects of
    [ RunOracleClientAction (CancelOracleRetry cancelled),
      RunOracleClientAction (ConnectAndHelloOracle value _)
      ]
        | cancelled == retry -> pure value
    effects -> assertFailure ("expected cancel then reconnect, got " <> show effects)
  assertBool
    "retry rotates through the fixed contact set"
    (oracleConnectAttemptContact firstAttempt /= oracleConnectAttemptContact secondAttempt)
  (afterStale, staleEffects) <-
    checkedIO
      "observe stale Oracle callback"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 2) (OracleInput (OracleConnectFailed firstAttempt)))
          connecting
      )
  assertBool "stale callback changes no state" (connecting == afterStale)
  assertEqual
    "stale callback has one typed diagnostic"
    [RunOracleClientAction (ReportOracleClientDiagnostic StaleOracleClientIngress)]
    (effectBatchMembers staleEffects)

caseHintedRetryRotation :: Assertion
caseHintedRetryRotation = do
  (initial, _) <- initialFixture
  (r1, r2, r3) <- case oracleContacts fixtureOracleContacts of
    first :| [second, third] -> pure (first, second, third)
    contacts -> assertFailure ("expected three fixed Oracle contacts, got " <> show contacts)
  let initialClient = startupOracleClientState initial
  firstAttempt <- case oracleClientActions initialClient of
    [ConnectAndHelloOracle attempt _] -> pure attempt
    actions -> assertFailure ("expected initial Oracle connect, got " <> show actions)
  assertEqual "normalized rotation starts at R1" r1 (oracleConnectAttemptContact firstAttempt)

  let r1Node = oracleContactNode r1
      notReadyAtR1 =
        oracleHelloAcceptance
          r1Node
          (oracleObservedTerm 1)
          (controlIndex 0)
          (Just r1Node)
          False
  preferredPrepared <-
    checkedIO
      "follow preferred R1 leader hint"
      (prepareClientIngress (OracleHelloReceived firstAttempt notReadyAtR1) initialClient)
  preferredRetry <- case preparedClientIngressActions preferredPrepared of
    [RejectOracleConnection _, ScheduleOracleRetry OracleConnectionRetry retry] -> pure retry
    actions -> assertFailure ("expected reject then delayed retry, got " <> show actions)
  let waitingForPreferred = commitClientIngress preferredPrepared
  preferredElapsed <-
    checkedIO
      "observe preferred R1 retry timer"
      (prepareClientIngress (OracleRetryElapsed preferredRetry) waitingForPreferred)
  preferredAttempt <- case preparedClientIngressActions preferredElapsed of
    [CancelOracleRetry cancelled, ConnectAndHelloOracle attempt _]
      | cancelled == preferredRetry -> pure attempt
    actions -> assertFailure ("expected retry cancel then preferred connect, got " <> show actions)
  assertEqual "the admitted leader hint selects R1" r1 (oracleConnectAttemptContact preferredAttempt)

  let preferredClient = commitClientIngress preferredElapsed
  (afterR2, r2Attempt) <- retryFailedAttempt "failed preferred R1" preferredAttempt preferredClient
  assertEqual "failed preferred R1 advances to R2" r2 (oracleConnectAttemptContact r2Attempt)
  (afterR3, r3Attempt) <- retryFailedAttempt "failed fallback R2" r2Attempt afterR2
  assertEqual "failed fallback R2 advances to R3" r3 (oracleConnectAttemptContact r3Attempt)
  (_, cycledAttempt) <- retryFailedAttempt "failed fallback R3" r3Attempt afterR3
  assertEqual "fixed-contact rotation cycles from R3 to R1" r1 (oracleConnectAttemptContact cycledAttempt)
  assertEqual
    "hint failover preserves the exact applied cursor"
    (replicate 4 (controlIndex 0))
    ( fmap
        oracleConnectAttemptFromExclusive
        [preferredAttempt, r2Attempt, r3Attempt, cycledAttempt]
    )

caseRejectedCandidateRetry :: Assertion
caseRejectedCandidateRetry = do
  (initial, _) <- initialFixture
  (r1, r2, r3) <- case oracleContacts fixtureOracleContacts of
    first :| [second, third] -> pure (first, second, third)
    contacts -> assertFailure ("expected three fixed Oracle contacts, got " <> show contacts)
  let initialClient = startupOracleClientState initial
  firstAttempt <- case oracleClientActions initialClient of
    [ConnectAndHelloOracle attempt _] -> pure attempt
    actions -> assertFailure ("expected initial Oracle connect, got " <> show actions)
  assertEqual "the rejected candidate starts at R1" r1 (oracleConnectAttemptContact firstAttempt)

  let rejectedAcceptance =
        oracleHelloAcceptance
          (oracleContactNode r2)
          (oracleObservedTerm 2)
          (controlIndex 0)
          (Just (oracleContactNode r3))
          True
  rejectedPrepared <-
    checkedIO
      "reject a candidate whose accepted node mismatches its contact"
      (prepareClientIngress (OracleHelloReceived firstAttempt rejectedAcceptance) initialClient)
  retry <- case preparedClientIngressActions rejectedPrepared of
    [RejectOracleConnection _, ScheduleOracleRetry OracleConnectionRetry value] -> pure value
    actions -> assertFailure ("expected candidate reject then delayed retry, got " <> show actions)
  elapsedPrepared <-
    checkedIO
      "observe rejected-candidate retry timer"
      (prepareClientIngress (OracleRetryElapsed retry) (commitClientIngress rejectedPrepared))
  hintedAttempt <- case preparedClientIngressActions elapsedPrepared of
    [CancelOracleRetry cancelled, ConnectAndHelloOracle attempt _]
      | cancelled == retry -> pure attempt
    actions -> assertFailure ("expected retry cancel then hinted connect, got " <> show actions)
  assertEqual
    "the fresh admitted hint is followed only after the delay"
    r3
    (oracleConnectAttemptContact hintedAttempt)

caseRedirectRetryHints :: Assertion
caseRedirectRetryHints = do
  (bound, binding) <- boundFixture
  (r1, r2, r3) <- case oracleContacts fixtureOracleContacts of
    first :| [second, third] -> pure (first, second, third)
    contacts -> assertFailure ("expected three fixed Oracle contacts, got " <> show contacts)
  let client = startupOracleClientState bound
      fresh = oracleRedirect (Just (oracleContactNode r3)) (oracleObservedTerm 2)
      self = oracleRedirect (Just (oracleContactNode r1)) (oracleObservedTerm 2)
      regressed = oracleRedirect (Just (oracleContactNode r3)) (oracleObservedTerm 0)

  freshPrepared <-
    checkedIO
      "observe a fresh established redirect"
      (prepareClientIngress (OracleRedirectReceived (oracleEstablishedLane binding) fresh) client)
  freshAttempt <- case preparedClientIngressActions freshPrepared of
    [RejectOracleConnection lane, ConnectAndHelloOracle attempt _]
      | lane == oracleEstablishedLane binding -> pure attempt
    actions -> assertFailure ("expected established reject then immediate hinted connect, got " <> show actions)
  assertEqual
    "a fresh distinct fixed-contact hint is followed immediately"
    r3
    (oracleConnectAttemptContact freshAttempt)
  let repeatedSameTerm =
        oracleHelloAcceptance
          (oracleContactNode r3)
          (oracleObservedTerm 2)
          (controlIndex 0)
          (Just (oracleContactNode r2))
          False
  repeatedPrepared <-
    checkedIO
      "observe a repeated same-term candidate redirect"
      ( prepareClientIngress
          (OracleHelloReceived freshAttempt repeatedSameTerm)
          (commitClientIngress freshPrepared)
      )
  case preparedClientIngressActions repeatedPrepared of
    [ RejectOracleConnection rejected,
      ScheduleOracleRetry OracleConnectionRetry _
      ]
        | rejected == oracleCandidateLane freshAttempt -> pure ()
    actions ->
      assertFailure
        ("expected repeated same-term redirect to remain paced, got " <> show actions)

  selfPrepared <-
    checkedIO
      "observe a fresh established self-redirect"
      (prepareClientIngress (OracleRedirectReceived (oracleEstablishedLane binding) self) client)
  (selfRetry, selfWaiting) <- case preparedClientIngressActions selfPrepared of
    [RejectOracleConnection lane, ScheduleOracleRetry OracleConnectionRetry retry]
      | lane == oracleEstablishedLane binding -> pure (retry, commitClientIngress selfPrepared)
    actions -> assertFailure ("expected self-redirect reject then delayed retry, got " <> show actions)
  selfElapsed <-
    checkedIO
      "observe self-redirect retry timer"
      (prepareClientIngress (OracleRetryElapsed selfRetry) selfWaiting)
  selfAttempt <- case preparedClientIngressActions selfElapsed of
    [CancelOracleRetry cancelled, ConnectAndHelloOracle attempt _]
      | cancelled == selfRetry -> pure attempt
    actions -> assertFailure ("expected self-redirect retry connect, got " <> show actions)
  assertEqual
    "a self hint is discarded in favour of fixed rotation"
    r2
    (oracleConnectAttemptContact selfAttempt)

  regressedPrepared <-
    checkedIO
      "observe a term-regressed established redirect"
      (prepareClientIngress (OracleRedirectReceived (oracleEstablishedLane binding) regressed) client)
  (regressedRetry, regressedWaiting) <- case preparedClientIngressActions regressedPrepared of
    [RejectOracleConnection lane, ScheduleOracleRetry OracleConnectionRetry retry]
      | lane == oracleEstablishedLane binding -> pure (retry, commitClientIngress regressedPrepared)
    actions -> assertFailure ("expected regressed redirect reject then delayed retry, got " <> show actions)
  regressedElapsed <-
    checkedIO
      "observe regressed redirect retry timer"
      (prepareClientIngress (OracleRetryElapsed regressedRetry) regressedWaiting)
  regressedAttempt <- case preparedClientIngressActions regressedElapsed of
    [CancelOracleRetry cancelled, ConnectAndHelloOracle attempt _]
      | cancelled == regressedRetry -> pure attempt
    actions -> assertFailure ("expected regressed redirect retry connect, got " <> show actions)
  assertEqual
    "a regressed-term hint is discarded in favour of fixed rotation"
    r2
    (oracleConnectAttemptContact regressedAttempt)

retryFailedAttempt ::
  String ->
  OracleConnectAttempt ->
  State ->
  IO (State, OracleConnectAttempt)
retryFailedAttempt context failedAttempt =
  retryAfterIngress context (OracleConnectFailed failedAttempt)

retryAfterIngress ::
  String ->
  OracleClientIngress ->
  State ->
  IO (State, OracleConnectAttempt)
retryAfterIngress context ingress predecessor = do
  failedPrepared <-
    checkedIO
      (context <> ": observe retry cause")
      (prepareClientIngress ingress predecessor)
  retry <- case preparedClientIngressActions failedPrepared of
    [ScheduleOracleRetry OracleConnectionRetry value] -> pure value
    actions -> assertFailure (context <> ": expected retry schedule, got " <> show actions)
  let retrying = commitClientIngress failedPrepared
  elapsedPrepared <-
    checkedIO
      (context <> ": observe retry timer")
      (prepareClientIngress (OracleRetryElapsed retry) retrying)
  nextAttempt <- case preparedClientIngressActions elapsedPrepared of
    [CancelOracleRetry cancelled, ConnectAndHelloOracle attempt _]
      | cancelled == retry -> pure attempt
    actions -> assertFailure (context <> ": expected retry cancel then connect, got " <> show actions)
  pure (commitClientIngress elapsedPrepared, nextAttempt)

caseDistinctHintAfterBindingLoss :: Assertion
caseDistinctHintAfterBindingLoss = do
  (initial, _) <- initialFixture
  (r1, r3) <- case oracleContacts fixtureOracleContacts of
    first :| [_, third] -> pure (first, third)
    contacts -> assertFailure ("expected three fixed Oracle contacts, got " <> show contacts)
  let initialClient = startupOracleClientState initial
  firstAttempt <- case oracleClientActions initialClient of
    [ConnectAndHelloOracle attempt _] -> pure attempt
    actions -> assertFailure ("expected initial Oracle connect, got " <> show actions)
  assertEqual "the initial candidate is R1" r1 (oracleConnectAttemptContact firstAttempt)

  let acceptance =
        oracleHelloAcceptance
          (oracleContactNode r1)
          (oracleObservedTerm 1)
          (controlIndex 2)
          (Just (oracleContactNode r3))
          True
  boundPrepared <-
    checkedIO
      "bind R1 while retaining its distinct R3 hint"
      (prepareClientIngress (OracleHelloReceived firstAttempt acceptance) initialClient)
  binding <- case preparedClientIngressActions boundPrepared of
    [BindOracleConnection observedAttempt value, WatchOracle observedBinding cursor]
      | observedAttempt == firstAttempt,
        observedBinding == value,
        cursor == controlIndex 0 ->
          pure value
    actions -> assertFailure ("expected R1 binding then watch, got " <> show actions)
  let boundClient = commitClientIngress boundPrepared
  advancedClient <-
    commitCursorAdvance
      <$> checkedIO
        "advance the bound client cursor"
        (prepareCursorAdvance fixtureEntryOne boundClient)

  (atR3, r3Attempt) <-
    retryAfterIngress
      "lose R1 with a distinct retained R3 hint"
      (OracleBindingLost binding)
      advancedClient
  assertEqual "binding loss follows the distinct R3 hint" r3 (oracleConnectAttemptContact r3Attempt)
  assertEqual
    "the hinted failover retains the exact applied cursor"
    (controlIndex 1)
    (oracleConnectAttemptFromExclusive r3Attempt)

  stalePrepared <-
    checkedIO
      "observe stale loss of the old R1 binding"
      (prepareClientIngress (OracleBindingLost binding) atR3)
  assertBool
    "stale old binding loss is state-identical"
    (commitClientIngress stalePrepared == atR3)
  assertEqual
    "stale old binding loss reports only its typed diagnostic"
    [ReportOracleClientDiagnostic StaleOracleClientIngress]
    (preparedClientIngressActions stalePrepared)

  (_, fallbackAttempt) <- retryFailedAttempt "fail retained R3 hint" r3Attempt atR3
  assertEqual "failed R3 wraps to canonical successor R1" r1 (oracleConnectAttemptContact fallbackAttempt)
  assertEqual
    "fixed rotation retains the exact applied cursor"
    (controlIndex 1)
    (oracleConnectAttemptFromExclusive fallbackAttempt)

caseSubmissionNotReady :: Assertion
caseSubmissionNotReady = do
  (bound, binding) <- boundFixture
  let boundClient = startupOracleClientState bound
  firstPrepared <-
    checkedIO
      "prepare first retained Oracle request"
      (prepareStartProcessEpochRequest "not-ready-first" fixtureStart boundClient)
  let (withFirst, _, firstRef) = commitOracleRequest firstPrepared
  secondPrepared <-
    checkedIO
      "prepare second retained Oracle request"
      (prepareStartProcessEpochRequest "not-ready-second" fixtureStart withFirst)
  let (withRequests, _, secondRef) = commitOracleRequest secondPrepared
      initialSubmissions = oracleClientRequestActions withRequests
      submittedRefs =
        [ oracleRequestDispatchRef dispatch
        | SubmitOracleRequest observedBinding dispatch <- initialSubmissions,
          observedBinding == binding
        ]
  assertEqual
    "retained requests dispatch in request-id order"
    [firstRef, secondRef]
    submittedRefs

  let unknownRequest =
        oracleClientRequestId
          (processStartResidence fixtureStart)
          999
  staleRequestPrepared <-
    checkedIO
      "observe NotReady for an unknown request"
      ( prepareClientIngress
          (OracleSubmissionNotReadyReceived binding unknownRequest (oracleObservedTerm 2))
          withRequests
      )
  assertBool
    "an unknown NotReady correlation changes no client fact"
    (commitClientIngress staleRequestPrepared == withRequests)
  assertEqual
    "an unknown NotReady correlation is diagnosed as stale"
    [ReportOracleClientDiagnostic StaleOracleClientIngress]
    (preparedClientIngressActions staleRequestPrepared)

  firstNotReady <-
    checkedIO
      "observe correlated NotReady for the first pending request"
      ( prepareClientIngress
          ( OracleSubmissionNotReadyReceived
              binding
              (oracleRequestRefRequestId firstRef)
              (oracleObservedTerm 2)
          )
          withRequests
      )
  retry <- case preparedClientIngressActions firstNotReady of
    [ AssociateOracleSubmissionRetry observedBinding observedRequest value,
      ScheduleOracleRetry OracleSubmissionRetry scheduled
      ]
        | observedBinding == binding,
          observedRequest == oracleRequestRefRequestId firstRef,
          scheduled == value ->
            pure value
    actions -> assertFailure ("expected ordered request association and reoffer timer, got " <> show actions)
  let waiting = commitClientIngress firstNotReady
  assertEqual
    "NotReady keeps the established logical binding"
    (Just binding)
    (oracleClientCurrentBinding waiting)
  assertEqual
    "level-triggered submission is suppressed while its delay is active"
    []
    (oracleClientRequestActions waiting)

  duplicateNotReady <-
    checkedIO
      "coalesce duplicate NotReady for the same request"
      ( prepareClientIngress
          ( OracleSubmissionNotReadyReceived
              binding
              (oracleRequestRefRequestId firstRef)
              (oracleObservedTerm 2)
          )
          waiting
      )
  assertEqual
    "the same pending request reasserts its exact association without a second timer"
    [ AssociateOracleSubmissionRetry
        binding
        (oracleRequestRefRequestId firstRef)
        retry
    ]
    (preparedClientIngressActions duplicateNotReady)
  anotherNotReady <-
    checkedIO
      "coalesce NotReady for another pending request"
      ( prepareClientIngress
          ( OracleSubmissionNotReadyReceived
              binding
              (oracleRequestRefRequestId secondRef)
              (oracleObservedTerm 3)
          )
          (commitClientIngress duplicateNotReady)
      )
  assertEqual
    "multiple pending requests join the one deterministic reoffer timer"
    [ AssociateOracleSubmissionRetry
        binding
        (oracleRequestRefRequestId secondRef)
        retry
    ]
    (preparedClientIngressActions anotherNotReady)
  let coalesced = commitClientIngress anotherNotReady

  let waitingHerald = replaceStartupOracleClientState coalesced bound
  (progressed, progressEffects) <-
    applyEntriesAt
      2
      waitingHerald
      binding
      (fixtureEntryOne :| [])
  let progressActions =
        [ action
        | RunOracleClientAction action <- effectBatchMembers progressEffects
        ]
      progressSubmissions =
        [ oracleRequestDispatchRef dispatch
        | SubmitOracleRequest observedBinding dispatch <- progressActions,
          observedBinding == binding
        ]
  assertEqual
    "strict committed progress cancels the exact fallback before watch and reoffer"
    (Just (CancelOracleRetry retry, WatchOracle binding (controlIndex 1)))
    ( case progressActions of
        cancellation@CancelOracleRetry {} : watch@WatchOracle {} : _ ->
          Just (cancellation, watch)
        _ -> Nothing
    )
  assertEqual
    "strict committed progress settles its matching request and reoffers only the remainder"
    [secondRef]
    progressSubmissions

  progressNotReady <-
    checkedIO
      "retain another fallback after committed progress"
      ( prepareClientIngress
          ( OracleSubmissionNotReadyReceived
              binding
              (oracleRequestRefRequestId secondRef)
              (oracleObservedTerm 3)
          )
          (startupOracleClientState progressed)
      )
  progressRetry <- case preparedClientIngressActions progressNotReady of
    [ AssociateOracleSubmissionRetry observedBinding observedRequest value,
      ScheduleOracleRetry OracleSubmissionRetry scheduled
      ]
        | observedBinding == binding,
          observedRequest == oracleRequestRefRequestId secondRef,
          scheduled == value ->
            pure value
    actions -> assertFailure ("expected a post-progress association and fallback, got " <> show actions)
  let duplicateWaiting =
        replaceStartupOracleClientState
          (commitClientIngress progressNotReady)
          progressed
  (afterDuplicateProgress, duplicateProgressEffects) <-
    applyEntriesAt
      3
      duplicateWaiting
      binding
      (fixtureEntryOne :| [])
  let duplicateProgressActions =
        [ action
        | RunOracleClientAction action <- effectBatchMembers duplicateProgressEffects
        ]
  assertEqual
    "an exact duplicate prefix neither cancels nor schedules any retry"
    []
    (oracleRetryControlActions duplicateProgressActions)
  assertEqual
    "an exact duplicate prefix cannot reoffer while the fallback remains pending"
    []
    [ dispatch
    | SubmitOracleRequest _ dispatch <- duplicateProgressActions
    ]

  (_, gapEffects) <-
    applyEntriesAt
      2
      waitingHerald
      binding
      (fixtureEntryTwo :| [])
  let gapActions =
        [ action
        | RunOracleClientAction action <- effectBatchMembers gapEffects
        ]
  assertEqual
    "a gap neither cancels nor schedules any retry"
    []
    (oracleRetryControlActions gapActions)

  (_, fallbackEffects) <-
    checkedIO
      "the duplicate/no-progress fallback still expires normally"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 4)
              (OracleInput (OracleRetryElapsed progressRetry))
          )
          afterDuplicateProgress
      )
  let fallbackActions =
        [ action
        | RunOracleClientAction action <- effectBatchMembers fallbackEffects
        ]
  assertBool
    "no-progress fallback cancels itself"
    (CancelOracleRetry progressRetry `elem` fallbackActions)
  assertEqual
    "no-progress fallback reoffers the exact remaining request"
    [secondRef]
    [ oracleRequestDispatchRef dispatch
    | SubmitOracleRequest observedBinding dispatch <- fallbackActions,
      observedBinding == binding
    ]

  lossPrepared <-
    checkedIO
      "lose the established lane while request reoffer is pending"
      (prepareClientIngress (OracleBindingLost binding) coalesced)
  case preparedClientIngressActions lossPrepared of
    [CancelOracleRetry cancelled, ScheduleOracleRetry OracleConnectionRetry _]
      | cancelled == retry -> pure ()
    actions -> assertFailure ("expected request-timer cancellation then reconnect retry, got " <> show actions)
  staleBindingPrepared <-
    checkedIO
      "observe late NotReady after binding loss"
      ( prepareClientIngress
          ( OracleSubmissionNotReadyReceived
              binding
              (oracleRequestRefRequestId firstRef)
              (oracleObservedTerm 3)
          )
          (commitClientIngress lossPrepared)
      )
  assertEqual
    "late NotReady from the lost lane is stale"
    [ReportOracleClientDiagnostic StaleOracleClientIngress]
    (preparedClientIngressActions staleBindingPrepared)

  let redirect =
        oracleRedirect
          (Just (oracleContactNode (oracleBindingContact binding)))
          (oracleObservedTerm 4)
  redirectPrepared <-
    checkedIO
      "redirect the established lane while request reoffer is pending"
      ( prepareClientIngress
          (OracleRedirectReceived (oracleEstablishedLane binding) redirect)
          coalesced
      )
  case preparedClientIngressActions redirectPrepared of
    [ RejectOracleConnection lane,
      CancelOracleRetry cancelled,
      ScheduleOracleRetry OracleConnectionRetry _
      ]
        | lane == oracleEstablishedLane binding,
          cancelled == retry ->
            pure ()
    actions -> assertFailure ("expected redirect, request-timer cancellation, then reconnect retry, got " <> show actions)

  elapsedPrepared <-
    checkedIO
      "observe the coalesced request-reoffer timer"
      (prepareClientIngress (OracleRetryElapsed retry) coalesced)
  assertEqual
    "timer expiry cancels itself then reoffers every still-awaiting request in order"
    (CancelOracleRetry retry : initialSubmissions)
    (preparedClientIngressActions elapsedPrepared)
  let reoffered = commitClientIngress elapsedPrepared
  assertEqual
    "delayed reoffer still uses the established binding"
    (Just binding)
    (oracleClientCurrentBinding reoffered)
  staleTimerPrepared <-
    checkedIO
      "observe duplicate request-reoffer timer expiry"
      (prepareClientIngress (OracleRetryElapsed retry) reoffered)
  assertBool
    "duplicate timer expiry changes no client fact"
    (commitClientIngress staleTimerPrepared == reoffered)
  assertEqual
    "duplicate timer expiry is diagnosed as stale"
    [ReportOracleClientDiagnostic StaleOracleClientIngress]
    (preparedClientIngressActions staleTimerPrepared)

oracleRetryControlActions :: [OracleClientAction] -> [OracleClientAction]
oracleRetryControlActions = filter isRetryControl
  where
    isRetryControl ScheduleOracleRetry {} = True
    isRetryControl CancelOracleRetry {} = True
    isRetryControl _ = False

caseLifecycleFastPath :: Assertion
caseLifecycleFastPath = do
  (initial, initialEffects) <- initialFixture
  attempt <- initialAttempt initialEffects
  let candidate = candidateAdministrationLane 41
      system = checkedSystemId fixtureHeraldGenesis
  (opened, openEffects) <-
    checkedIO
      "open administration connection before Oracle binding"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 1)
              (AdministrationInput (OpenAdministrationConnection candidate system))
          )
          initial
      )
  administrationBinding <- case effectBatchMembers openEffects of
    [SetAdministrationConnectionDisposition observedCandidate binding]
      | observedCandidate == candidate -> pure binding
    effects -> assertFailure ("expected one administration binding, got " <> show effects)
  let request =
        startProcessRequest
          (adminCorrelationIdForBinding administrationBinding 411)
  (retained, startEffects) <-
    checkedIO
      "retain a submission while Oracle is connecting"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 2)
              ( AdministrationInput
                  (StartProcessEpoch administrationBinding request)
              )
          )
          opened
      )
  assertEqual
    "an unbound accepted Start retains its request without a submission"
    0
    ( length
        [ ()
        | RunOracleClientAction (SubmitOracleRequest _ _) <- effectBatchMembers startEffects
        ]
        + length
          [ ()
          | RunOracleClientAction (WatchOracle _ _) <- effectBatchMembers startEffects
          ]
    )
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (oracleConnectAttemptFromExclusive attempt)
          (Just node)
          True
  (bound, bindEffects) <-
    checkedIO
      "accept ready Oracle with retained submission"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 3)
              (OracleInput (OracleHelloReceived attempt acceptance))
          )
          retained
      )
  (binding, dispatch) <- case effectBatchMembers bindEffects of
    [ RunOracleClientAction (BindOracleConnection observedAttempt observedBinding),
      RunOracleClientAction (WatchOracle watchBinding _),
      RunOracleClientAction (SubmitOracleRequest submitBinding submitted)
      ]
        | observedAttempt == attempt,
          watchBinding == observedBinding,
          submitBinding == observedBinding ->
            pure (observedBinding, submitted)
    effects ->
      assertFailure
        ("expected bind, watch, and one retained submission, got " <> show effects)
  assertAcceptedHelloOwnersChanged retained bound

  (waiting, notReadyEffects) <-
    checkedIO
      "delay the retained submission"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 4)
              ( OracleInput
                  ( OracleSubmissionNotReadyReceived
                      binding
                      (oracleRequestRefRequestId (oracleRequestDispatchRef dispatch))
                      (oracleObservedTerm 2)
                  )
              )
          )
          bound
      )
  retry <- case effectBatchMembers notReadyEffects of
    [ RunOracleClientAction
        (AssociateOracleSubmissionRetry observedBinding observedRequest observed),
      RunOracleClientAction (ScheduleOracleRetry OracleSubmissionRetry scheduled)
      ]
        | observedBinding == binding,
          observedRequest
            == oracleRequestRefRequestId (oracleRequestDispatchRef dispatch),
          scheduled == observed ->
            pure observed
    effects -> assertFailure ("expected ordered request association and reoffer timer, got " <> show effects)
  assertOnlyOracleClientAndTimeChanged
    "NotReady fast path"
    bound
    waiting

  (reoffered, retryEffects) <-
    checkedIO
      "reoffer retained submission after delay"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 5)
              (OracleInput (OracleRetryElapsed retry))
          )
          waiting
      )
  assertEqual
    "bound retry cancels itself and reoffers the retained submission exactly once"
    [ RunOracleClientAction (CancelOracleRetry retry),
      RunOracleClientAction (SubmitOracleRequest binding dispatch)
    ]
    (effectBatchMembers retryEffects)
  assertOnlyOracleClientAndTimeChanged
    "bound RetryElapsed fast path"
    waiting
    reoffered

caseConnectFailedFastPath :: Assertion
caseConnectFailedFastPath = do
  (initial, initialEffects) <- initialFixture
  attempt <- initialAttempt initialEffects
  (successor, effects) <-
    checkedIO
      "apply ConnectFailed through the Herald transition"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 1) (OracleInput (OracleConnectFailed attempt)))
          initial
      )
  case effectBatchMembers effects of
    [RunOracleClientAction (ScheduleOracleRetry OracleConnectionRetry _)] -> pure ()
    observed -> assertFailure ("expected one reconnect timer, got " <> show observed)
  assertOracleFastPathChanged "ConnectFailed fast path" initial successor

caseRedirectFastPath :: Assertion
caseRedirectFastPath = do
  (bound, binding) <- boundFixture
  redirectedContact <- case oracleContacts fixtureOracleContacts of
    _ :| (_ : contact : _) -> pure contact
    contacts -> assertFailure ("expected three fixed Oracle contacts, got " <> show contacts)
  let lane = oracleEstablishedLane binding
      redirect =
        oracleRedirect
          (Just (oracleContactNode redirectedContact))
          (oracleObservedTerm 2)
  (successor, effects) <-
    checkedIO
      "apply Redirect through the Herald transition"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 2)
              (OracleInput (OracleRedirectReceived lane redirect))
          )
          bound
      )
  case effectBatchMembers effects of
    [ RunOracleClientAction (RejectOracleConnection observedLane),
      RunOracleClientAction (ConnectAndHelloOracle attempt _)
      ]
        | observedLane == lane ->
            assertEqual
              "fresh redirect immediately selects its fixed contact"
              redirectedContact
              (oracleConnectAttemptContact attempt)
    observed -> assertFailure ("expected reject then immediate hinted connect, got " <> show observed)
  assertOracleFastPathChanged "Redirect fast path" bound successor

caseBindingLostFastPath :: Assertion
caseBindingLostFastPath = do
  (bound, binding) <- boundFixture
  (successor, effects) <-
    checkedIO
      "apply BindingLost through the Herald transition"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 2) (OracleInput (OracleBindingLost binding)))
          bound
      )
  case effectBatchMembers effects of
    [RunOracleClientAction (ScheduleOracleRetry OracleConnectionRetry _)] -> pure ()
    observed -> assertFailure ("expected one reconnect timer, got " <> show observed)
  assertOracleFastPathChanged "BindingLost fast path" bound successor

assertOracleFastPathChanged :: String -> HeraldState -> HeraldState -> Assertion
assertOracleFastPathChanged context predecessor successor = do
  assertBool
    (context <> " changes the Oracle-client owner")
    (startupOracleClientState predecessor /= startupOracleClientState successor)
  assertOnlyOracleClientAndTimeChanged context predecessor successor

-- Accepting a new Oracle binding now synchronizes Preparation's compact
-- delivery-context stamp, even when this fixture has no preparation candidates.
-- Assert the exact permitted field change; do not erase schedules or joins.
assertAcceptedHelloOwnersChanged :: HeraldState -> HeraldState -> Assertion
assertAcceptedHelloOwnersChanged predecessor successor = do
  let before = startupProcessPreparationState predecessor
      after = startupProcessPreparationState successor
      binding = oracleClientCurrentBinding (startupOracleClientState successor)
  context <- case Preparation.preparationContext before of
    Just (Preparation.PreparationContext membership active gate membershipGate _) ->
      pure (Preparation.PreparationContext membership active gate membershipGate binding)
    Nothing -> assertFailure "accepted-Hello fixture has no seeded preparation context"
  assertBool
    "accepted Hello changes only the preparation context's observed Oracle binding"
    (after == Preparation.seedPreparationContext context before)
  assertEqual "predecessor preparation indexes validate" (Right ()) (Preparation.validateState before)
  assertEqual "successor preparation indexes validate" (Right ()) (Preparation.validateState after)
  assertOnlyOracleClientAndTimeChanged
    "accepted Hello after accounting for its exact preparation binding stamp"
    predecessor
    (replaceStartupProcessPreparationState before successor)

assertOnlyOracleClientAndTimeChanged ::
  String ->
  HeraldState ->
  HeraldState ->
  Assertion
assertOnlyOracleClientAndTimeChanged context predecessor successor =
  assertBool
    (context <> " changes only observed time and the Oracle-client owner")
    ( predecessor
        == ( replaceStartupLastObservedTime (startupLastObservedTime predecessor)
               . replaceStartupOracleClientState (startupOracleClientState predecessor)
               $ successor
           )
    )

caseContiguous :: Assertion
caseContiguous = do
  (bound, binding) <- boundFixture
  (advanced, effects) <- applyEntries bound binding (fixtureEntryOne :| [])
  let witness = startupStateWitness advanced
      client = startupWitnessOracleClient witness
      projection = startupOracleProjectionState advanced
  assertEqual "projection cursor" (controlIndex 1) (startupWitnessOracleControlIndex witness)
  assertEqual "client applied cursor" (controlIndex 1) (oracleClientWitnessAppliedCursor client)
  assertEqual "client requested cursor" (controlIndex 1) (oracleClientWitnessRequestedCursor client)
  assertEqual "the unpinned canonical entry is reclaimed" [] (oracleClientWitnessRetainedEntryBytes client)
  assertEqual "Client covers the completed prefix" (controlIndex 1) (Client.oracleClientCanonicalCoveredThrough (startupOracleClientState advanced))
  assertEqual "Projection covers the same completed prefix" (controlIndex 1) (Projection.projectionCanonicalCoveredThrough projection)
  assertBool
    "the accepted Start process is projected at its exact index"
    ( case OracleProjection.oracleViewStartedProcess
        fixtureStartedProcessEpoch
        (OracleProjection.oracleView projection) of
        Just started ->
          OracleProjection.projectedStartedProcessControlIndex started
            == controlIndex 1
        Nothing -> False
    )
  assertEqual
    "the Start index does not materialize or authorize configured roots"
    Nothing
    (lookup fixtureStartedProcessEpoch (OracleProjection.projectedBootstraps projection))
  assertEqual
    "the next watch starts after the atomically applied cursor"
    [RunOracleClientAction (WatchOracle binding (controlIndex 1))]
    (effectBatchMembers effects)

caseControlledRelease :: Assertion
caseControlledRelease = do
  fixture <- heldControlledFixture
  assertHeldControlled fixture fixture.heldState
  case effectBatchMembers fixture.heldEffects of
    [SendPeerControl completedBinding (PeerStreamCompleted completedDirection completed)] -> do
      assertEqual "held completion acknowledgement binding" fixture.peerBinding completedBinding
      assertEqual "held completion direction" (sequencedItemDirection fixture.item) completedDirection
      assertEqual "completion certificate alone acknowledges receipt through one" (Just 1) (Receipt.receiptRetirementHighWater completed)
      assertEqual "held work remains a pending exception" (Set.singleton 1) (Receipt.receiptRetirementExceptions completed)
      assertEqual "held completion remains empty" emptyStreamPrefix (streamCompletionPrefix completed)
      assertEqual
        "held work leaves the local structural frontier unchanged"
        (GraphProgress.structuralLocalReport (startupStructuralProgressState fixture.predecessorState))
        (GraphProgress.structuralLocalReport (startupStructuralProgressState fixture.heldState))
    observed -> assertFailure ("unexpected held-control acknowledgement: " <> show observed)

  (advanced, effects) <-
    applyEntriesAt
      4
      fixture.heldState
      fixture.oracleBinding
      (fixtureEntryOne :| [])
  assertAppliedControlled fixture advanced
  assertBool
    "the released occurrence changes the exact local structural report"
    ( GraphProgress.structuralLocalReport (startupStructuralProgressState fixture.heldState)
        /= GraphProgress.structuralLocalReport (startupStructuralProgressState advanced)
    )
  case effectBatchMembers effects of
    [ SendPeerControl completedBinding (PeerStreamCompleted completedDirection completed),
      RunOracleClientAction (WatchOracle watchBinding cursor)
      ] -> do
        assertEqual "completed acknowledgement binding" fixture.peerBinding completedBinding
        assertEqual "released completed direction" (sequencedItemDirection fixture.item) completedDirection
        assertEqual "completion advances through one" (Just firstStreamSequence) (streamPrefixSequence (streamCompletionPrefix completed))
        assertEqual "release precedes the next watch on the same Oracle binding" fixture.oracleBinding watchBinding
        assertEqual "the next watch observes the complete successor" (controlIndex 1) cursor
    observed -> assertFailure ("unexpected controlled-release effect order: " <> show observed)

  (duplicate, duplicateEffects) <-
    applyEntriesAt
      4
      advanced
      fixture.oracleBinding
      (fixtureEntryOne :| [])
  assertBool "equal Oracle replay does not reapply any owner" (duplicate == advanced)
  assertEqual
    "equal Oracle replay emits no second peer completion"
    [RunOracleClientAction (WatchOracle fixture.oracleBinding (controlIndex 1))]
    (effectBatchMembers duplicateEffects)

caseStructuralReportPerBinding :: Assertion
caseStructuralReportPerBinding = do
  (bound, oracleBinding) <- boundFixture
  (afterFirstPeer, _) <-
    connectPeerAt 2 12_701 fixtureRemoteMember bound
  (connected, _) <-
    connectPeerAt 3 12_702 fixtureThirdMember afterFirstPeer
  (advanced, effects) <-
    applyEntriesAt
      4
      connected
      oracleBinding
      (fixtureEntryOne :| [])
  let expectedReport =
        GraphProgress.structuralLocalReport
          (startupStructuralProgressState advanced)
      reports =
        [ (binding, report)
        | SendPeerControl binding (PeerStructuralAppliedReported report) <- effectBatchMembers effects
        ]
  assertBool
    "the Oracle advance changes the exact local structural report"
    ( GraphProgress.structuralLocalReport (startupStructuralProgressState connected)
        /= expectedReport
    )
  assertEqual
    "the announcer keeps its own progress without sending reports to other peers"
    []
    reports

caseDistinctStructuralReportOrder :: Assertion
caseDistinctStructuralReportOrder = do
  (bound, oracleBinding) <- boundFixture
  (connected, peerBinding) <- connectRemotePeerAt 2 bound
  (afterOne, _) <-
    applyEntriesAt
      3
      connected
      oracleBinding
      (fixtureEntryOne :| [])
  (afterTwo, _) <-
    applyEntriesAt
      4
      afterOne
      oracleBinding
      (fixtureEntryTwo :| [])
  (_, batchedEffects) <-
    applyEntriesAt
      3
      connected
      oracleBinding
      (fixtureEntryOne :| [fixtureEntryTwo])
  let reportAfterOne =
        GraphProgress.structuralLocalReport
          (startupStructuralProgressState afterOne)
      reportAfterTwo =
        GraphProgress.structuralLocalReport
          (startupStructuralProgressState afterTwo)
      observedReports =
        [ report
        | SendPeerControl binding (PeerStructuralAppliedReported report) <- effectBatchMembers batchedEffects,
          binding == peerBinding
        ]
  assertBool
    "the two contiguous Oracle entries produce distinct covering reports"
    (reportAfterOne /= reportAfterTwo)
  assertEqual
    "the announcer does not broadcast intermediate or final local reports"
    []
    observedReports

caseControlledGap :: Assertion
caseControlledGap = do
  fixture <- heldControlledFixture
  (afterGap, effects) <-
    applyEntriesAt
      4
      fixture.heldState
      fixture.oracleBinding
      (fixtureEntryTwo :| [])
  assertBool
    "a gap changes only the already-admitted observation timestamp"
    ( replaceStartupLastObservedTime
        (startupLastObservedTime fixture.heldState)
        afterGap
        == fixture.heldState
    )
  assertHeldControlled fixture afterGap
  assertEqual
    "a gap emits only a rewatch from the retained cursor"
    [RunOracleClientAction (WatchOracle fixture.oracleBinding (controlIndex 0))]
    (effectBatchMembers effects)

caseControlledRejection :: Assertion
caseControlledRejection = do
  fixture <- heldControlIndexTwoFixture
  assertHeldControlled fixture fixture.heldState
  (settled, effects) <-
    applyEntriesAt
      5
      fixture.heldState
      fixture.oracleBinding
      (fixtureRejectedEntryTwo :| [])
  assertAppliedControlled fixture settled
  assertEqual
    "the rejected Start contributes no additional process projection"
    1
    (length (OracleProjection.projectedStartedProcesses (startupOracleProjectionState settled)))
  case effectBatchMembers effects of
    [ SendPeerControl completedBinding (PeerStreamCompleted completedDirection completed),
      RunOracleClientAction (WatchOracle watchBinding cursor)
      ] -> do
        assertEqual "release completion binding" fixture.peerBinding completedBinding
        assertEqual "release completed direction" (sequencedItemDirection fixture.item) completedDirection
        assertEqual "release completion advances through one" (Just firstStreamSequence) (streamPrefixSequence (streamCompletionPrefix completed))
        assertEqual "release retains the Oracle binding" fixture.oracleBinding watchBinding
        assertEqual "rewatch follows the rejected index" (controlIndex 2) cursor
    observed -> assertFailure ("unexpected rejected-Start release order: " <> show observed)

caseRejectedEntry :: Assertion
caseRejectedEntry = do
  (bound, binding) <- boundFixture
  (advanced, effects) <-
    applyEntries bound binding (fixtureEntryOne :| [fixtureRejectedEntryTwo])
  let projection = startupOracleProjectionState advanced
  assertEqual
    "a committed rejection advances both owners"
    (controlIndex 2)
    (startupWitnessOracleControlIndex (startupStateWitness advanced))
  assertBool
    "the rejected Start has no fabricated process projection"
    (length (OracleProjection.projectedStartedProcesses projection) == 1)
  assertEqual
    "the rejection retains the next watch intention"
    [RunOracleClientAction (WatchOracle binding (controlIndex 2))]
    (effectBatchMembers effects)

caseSparseDuplicate :: Assertion
caseSparseDuplicate = do
  (bound, binding) <- boundFixture
  let home = heraldMemberEpoch fixtureRemoteMember
      rejection ordinal = oracleEnvelope (oracleClientRequestId home ordinal) (Just (controlIndex 99)) home (startProcessEpochCommand fixtureStart)
      (_, entry) = committed fixtureOracleInitialState (rejection 1)
      (_, conflicting) = committed fixtureOracleInitialState (rejection 2)
      canonical = canonicalizeAppliedOracleEntry entry
      index = appliedEntryControlIndex entry
  (advanced, effects) <- applyEntries bound binding (canonical :| [])
  assertEqual "completed batch removes the duplicate client copy" Nothing (Client.oracleClientRetainedEntry index (startupOracleClientState advanced))
  assertEqual "the unpinned projection copy is reclaimed" Nothing (Projection.appliedEntryEvidence index (startupOracleProjectionState advanced))
  assertEqual "the projection covers the completed prefix" (Projection.AppliedEntryCoveredByBase index) (Projection.classifyAppliedEntry canonical (startupOracleProjectionState advanced))
  assertEqual "compaction reaches the observed cursor" index (Client.oracleClientEvidenceCompactedThrough (startupOracleClientState advanced))
  (duplicate, duplicateEffects) <- applyEntries advanced binding (canonical :| [])
  assertBool "the old exact duplicate is atomic and idempotent" (advanced == duplicate)
  assertEqual "exact duplication repeats only the same watch" effects duplicateEffects
  let alternate = canonicalizeAppliedOracleEntry conflicting
  assertBool "the alternate entry has different canonical bytes" (alternate /= canonical)
  assertEqual "coverage does not claim byte equality" (Projection.AppliedEntryCoveredByBase index) (Projection.classifyAppliedEntry alternate (startupOracleProjectionState advanced))
  (covered, coveredEffects) <- applyEntries advanced binding (alternate :| [])
  assertBool "covered history cannot change the applied state" (advanced == covered)
  assertEqual "covered history repeats only the same watch" effects coveredEffects
  assertEqual "the sparse composed owner remains valid" (Right ()) (validateHeraldState advanced)

caseDuplicate :: Assertion
caseDuplicate = do
  (bound, binding) <- boundFixture
  (advanced, firstEffects) <- applyEntries bound binding (fixtureEntryOne :| [])
  (duplicate, duplicateEffects) <- applyEntries advanced binding (fixtureEntryOne :| [])
  assertBool "exact overlap changes no state" (advanced == duplicate)
  assertEqual "exact overlap repeats the same watch" firstEffects duplicateEffects

caseOverlap :: Assertion
caseOverlap = do
  (bound, binding) <- boundFixture
  (afterOne, _) <- applyEntries bound binding (fixtureEntryOne :| [])
  (overlap, effects) <- applyEntries afterOne binding (fixtureEntryOne :| [fixtureEntryTwo])
  (direct, _) <- applyEntries bound binding (fixtureEntryOne :| [fixtureEntryTwo])
  assertBool "the equal prefix is skipped and only the suffix advances" (overlap == direct)
  assertEqual
    "overlap continues watching after the complete prefix"
    [RunOracleClientAction (WatchOracle binding (controlIndex 2))]
    (effectBatchMembers effects)

caseEvidenceInvariant :: Assertion
caseEvidenceInvariant = do
  (bound, binding) <- boundFixture
  (advanced, _) <- applyEntries bound binding (fixtureEntryOne :| [])
  mismatchedClient <-
    commitCursorAdvance
      <$> checkedIO
        "prepare independently valid unequal client evidence"
        (prepareCursorAdvance fixtureConflictingEntryOne (startupOracleClientState bound))
  assertEqual
    "equal cursors with unequal canonical evidence are rejected"
    (Left (HeraldStartupInvariant StartupStatic OracleClientEvidenceInvariant))
    (validateHeraldState (replaceStartupOracleClientState mismatchedClient advanced))

caseGap :: Assertion
caseGap = do
  (bound, binding) <- boundFixture
  (afterGap, effects) <- applyEntries bound binding (fixtureEntryTwo :| [])
  let witness = startupStateWitness afterGap
      client = startupWitnessOracleClient witness
  assertEqual "projection cursor remains zero" (controlIndex 0) (startupWitnessOracleControlIndex witness)
  assertEqual "client cursor remains zero" (controlIndex 0) (oracleClientWitnessAppliedCursor client)
  assertEqual
    "gap rewatches the retained cursor"
    [RunOracleClientAction (WatchOracle binding (controlIndex 0))]
    (effectBatchMembers effects)

caseConflict :: Assertion
caseConflict = do
  (bound, binding) <- boundFixture
  prepared <- checkedIO "prepare a local request which pins its result" (prepareStartProcessEpochRequest "pinned-conflict" fixtureStart (startupOracleClientState bound))
  let (client, _, reference) = commitOracleRequest prepared
  witness <- maybe (assertFailure "prepared local request is absent") pure (Client.lookupOracleRequest reference client)
  let (_, entry) = committed fixtureOracleInitialState (canonicalOracleEnvelopeValue (Client.oracleRequestWitnessEnvelope witness))
      canonical = canonicalizeAppliedOracleEntry entry
      index = appliedEntryControlIndex entry
      requested = replaceStartupOracleClientState client bound
  (advanced, _) <- applyEntries requested binding (canonical :| [])
  assertEqual "the retained local result pins Client evidence" (Just canonical) (Client.oracleClientRetainedEntry index (startupOracleClientState advanced))
  assertEqual "the same local result pins Projection evidence" (Just canonical) (Projection.appliedEntryEvidence index (startupOracleProjectionState advanced))
  assertEqual "exact sparse evidence takes precedence over coverage" (Projection.AppliedEntryUnequalConflict index) (Projection.classifyAppliedEntry fixtureConflictingEntryOne (startupOracleProjectionState advanced))
  case verifiedStepHerald
    ( heraldInput
        (monotonicInstant 2)
        (OracleInput (OracleEntriesReceived binding (fixtureConflictingEntryOne :| [])))
    )
    advanced of
    Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction) -> pure ()
    Left fault -> assertFailure ("expected Oracle contradiction, got " <> show fault)
    Right _ -> assertFailure "unequal reuse unexpectedly advanced the Herald"

caseReconnect :: Assertion
caseReconnect = do
  expectedNext <- case oracleContacts fixtureOracleContacts of
    _ :| (next : _) -> pure next
    contacts -> assertFailure ("expected multiple fixed Oracle contacts, got " <> show contacts)
  (bound, binding) <- boundFixture
  (advanced, _) <- applyEntries bound binding (fixtureEntryOne :| [])
  (retrying, retryEffects) <-
    checkedIO
      "observe established Oracle loss"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 3) (OracleInput (OracleBindingLost binding)))
          advanced
      )
  retry <- case effectBatchMembers retryEffects of
    [RunOracleClientAction (ScheduleOracleRetry OracleConnectionRetry value)] -> pure value
    members -> assertFailure ("expected reconnect retry, got " <> show members)
  (connecting, connectEffects) <-
    checkedIO
      "observe reconnect retry"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 4) (OracleInput (OracleRetryElapsed retry)))
          retrying
      )
  reconnect <- case effectBatchMembers connectEffects of
    [ RunOracleClientAction (CancelOracleRetry cancelled),
      RunOracleClientAction (ConnectAndHelloOracle attempt _)
      ]
        | cancelled == retry -> pure attempt
    members -> assertFailure ("expected cancel then reconnect, got " <> show members)
  assertEqual
    "the fresh Hello and following watch start after the retained prefix"
    (controlIndex 1)
    (oracleConnectAttemptFromExclusive reconnect)
  assertEqual
    "binding loss invalidates its matching hint and advances to the next fixed contact"
    expectedNext
    (oracleConnectAttemptContact reconnect)
  (afterStale, staleEffects) <-
    checkedIO
      "observe stale Oracle binding loss"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 4) (OracleInput (OracleBindingLost binding)))
          connecting
      )
  assertBool "stale binding loss changes no state" (connecting == afterStale)
  assertEqual
    "stale binding loss has one typed diagnostic"
    [RunOracleClientAction (ReportOracleClientDiagnostic StaleOracleClientIngress)]
    (effectBatchMembers staleEffects)

-- Exercise membership retirement with a real accepted, locally installed and
-- remotely assigned structural publication. No peer completion is supplied, so
-- only canonical retirement can discharge the selected destination frontiers.
retainPublicationGroupFixture ::
  Session.ApplicationSessionAcceptance ->
  HeraldState ->
  IO (HeraldState, PublicationGroups.GroupKey)
retainPublicationGroupFixture acceptance predecessor = do
  (session, access) <- case Session.sessionAcceptanceReply acceptance of
    Session.SessionOpened session _ access -> pure (session, access)
    reply -> assertFailure ("expected open publication session, got " <> show reply)
  process <-
    maybe
      (assertFailure "grouped publication session has no process")
      pure
      (Application.applicationSessionProcess session (startupApplicationState predecessor))
  writer <-
    Access.predefinedWriter
      <$> maybe
        (assertFailure "grouped publication has no neutral writer")
        pure
        (find ((== Access.NeutralVertexRole) . Access.predefinedAccessRole) (conventionalStartupPairs access))
  let call ordinal operation state =
        checkedIO
          "grouped publication application call"
          ( verifiedStepHerald
              ( heraldInput
                  (startupLastObservedTime state)
                  ( ApplicationRequestInput
                      ( CallApplicationRequest
                          (Session.sessionAcceptanceBinding acceptance)
                          session
                          (ApplicationRequest.requestId ordinal)
                          operation
                      )
                  )
              )
              state
          )
  (reserved, newIdEffects) <-
    call
      1
      (ApplicationRequest.NewIdApplication (ControlledNewId writer))
      predecessor
  object <- case [ value
                 | SendApplicationReply
                     _
                     ( ApplicationRequest.RetainedRequestReply
                         _
                         (ApplicationRequest.Completed _ (NewIdCompleted value))
                       ) <-
                     effectBatchMembers newIdEffects
                 ] of
    [value] -> pure value
    values -> assertFailure ("expected one grouped-publication object, got " <> show (length values))
  global <-
    checkedIO
      "resolve grouped publication object"
      (Application.resolveApplicationPrivateUniqueId process object (startupApplicationState reserved))
  let value =
        ApplicationValue.RecordValue
          ( Map.fromList
              [ ("label", ApplicationValue.LabelValue (ApplicationValue.VoidLabel, 0)),
                ("object_id", ApplicationValue.UniqueIdValue object)
              ]
          )
      group = PublicationGroups.groupKey process (globalObjectIdFromGlobalUniqueId global)
  (published, _) <-
    call
      2
      (ApplicationRequest.WriteApplication writer (PublishValue value))
      reserved
  let groups = Publication.publicationGroups (startupPublicationState published)
  assertEqual
    "grouped publication has completed local installation"
    0
    (PublicationGroups.groupLocalPendingCount group groups)
  assertBool
    "grouped publication awaits remote completion"
    (PublicationGroups.groupPendingPeerCount group groups > 0)
  assertBool "grouped publication accounting is valid" (PublicationGroups.valid groups)
  pure (published, group)

casePublicationGroupPeerProgress :: Assertion
casePublicationGroupPeerProgress = do
  (bound, _) <- boundFixture
  bootstrap <- case filter
    ((== checkedLocalHeraldEpoch fixtureHeraldGenesis) . appliedProcessResidence)
    (checkedInitialBootstraps fixtureHeraldBootstraps) of
    [value] -> pure value
    values -> assertFailure ("expected one local bootstrap, got " <> show (length values))
  preparedOpen <-
    checkedIO
      "open peer-progress publication application"
      ( Application.prepareApplicationSessionOpen
          (checkedLocalHeraldEpoch fixtureHeraldGenesis)
          (Session.applicationAttachmentForBootstrap (appliedBootstrapManifestId bootstrap))
          (Session.clientNonce 74)
          (startupApplicationState bound)
      )
  let (application, acceptance) = Application.commitApplicationSessionAcceptance preparedOpen
  (connected, binding) <- connectRemotePeerAt 2 (replaceStartupApplicationState application bound)
  (published, group) <- retainPublicationGroupFixture acceptance connected
  let peer = heraldMemberEpoch fixtureRemoteMember
      groups = Publication.publicationGroups (startupPublicationState published)
      frontiers = PublicationGroups.groupFrontiers group groups
  frontier <- maybe (assertFailure "connected peer has no publication frontier") pure (Map.lookup peer frontiers)
  direction <-
    checkedIO
      "publication group outgoing direction"
      (mkStreamDirection (checkedLocalHeraldEpoch fixtureHeraldGenesis) peer)
  let prefix = streamPrefixThrough frontier
      deliver control state =
        checkedIO
          "deliver group completion control"
          ( verifiedStepHerald
              ( heraldInput
                  (startupLastObservedTime state)
                  (PeerInput (PeerControlReceived binding control))
              )
              state
          )
  (received, _) <- deliver (PeerStreamReceived direction prefix) published
  assertEqual
    "receipt without completion does not release a group frontier"
    groups
    (Publication.publicationGroups (startupPublicationState received))
  forM_
    [ ("completed acknowledgement", PeerStreamCompleted direction (Receipt.receiptRetirementPrefix (Just (streamSequenceWord64 frontier)))),
      ("resume response", PeerStreamResumeAccepted (resumeResponse prefix prefix (nextAfterStreamPrefix prefix)))
    ]
    $ \(context, control) -> do
      (completed, _) <- deliver control received
      let completedGroups = Publication.publicationGroups (startupPublicationState completed)
      assertEqual
        (context <> " clears only the completed destination")
        (Map.delete peer frontiers)
        (PublicationGroups.groupFrontiers group completedGroups)
      assertBool (context <> " preserves valid group indexing") (PublicationGroups.valid completedGroups)
      (duplicate, _) <- deliver control completed
      assertEqual
        (context <> " replay leaves group accounting unchanged")
        completedGroups
        (Publication.publicationGroups (startupPublicationState duplicate))

caseMembershipCutoverReconnect :: Assertion
caseMembershipCutoverReconnect = do
  (bound, binding) <- failureBoundFixture
  bootstrap <- case filter
    ((== checkedLocalHeraldEpoch fixtureFailureHeraldGenesis) . appliedProcessResidence)
    (checkedInitialBootstraps fixtureFailureHeraldBootstraps) of
    [value] -> pure value
    values -> assertFailure ("expected one local bootstrap, got " <> show (length values))
  preparedOpen <-
    checkedIO
      "open grouped-publication application"
      ( Application.prepareApplicationSessionOpen
          (checkedLocalHeraldEpoch fixtureFailureHeraldGenesis)
          (Session.applicationAttachmentForBootstrap (appliedBootstrapManifestId bootstrap))
          (Session.clientNonce 73)
          (startupApplicationState bound)
      )
  let (application, acceptance) = Application.commitApplicationSessionAcceptance preparedOpen
  (withPublication, group) <- retainPublicationGroupFixture acceptance (replaceStartupApplicationState application bound)
  let (entries, successorMembership) = fixtureFailureRetirement
      successorGeneration = heraldMembershipGenerationId successorMembership
      retirementIndex = controlIndex 4
      retired = heraldMemberEpoch fixtureFourthMember
      beforeFrontiers = PublicationGroups.groupFrontiers group (Publication.publicationGroups (startupPublicationState withPublication))
  assertBool "the retiring peer owns a real grouped publication frontier" (Map.member retired beforeFrontiers)
  (advanced, _) <- applyEntriesAt 2 withPublication binding entries
  assertEqual
    "canonical remote retirement removes only the retired peer frontier"
    (Map.delete retired beforeFrontiers)
    (PublicationGroups.groupFrontiers group (Publication.publicationGroups (startupPublicationState advanced)))
  assertBool
    "remote retirement keeps the group wait index valid"
    (PublicationGroups.valid (Publication.publicationGroups (startupPublicationState advanced)))
  let projection = startupOracleProjectionState advanced
      client = startupOracleClientState advanced
      clientWitness = oracleClientStateWitness client
  assertEqual
    "the canonical retirement installs its exact successor membership"
    successorMembership
    ( OracleProjection.oracleViewCurrentHeraldMembership
        (OracleProjection.oracleView projection)
    )
  assertEqual
    "the unpinned live membership prefix is reclaimed"
    []
    (oracleClientWitnessRetainedEntryBytes clientWitness)
  assertEqual "Client covers the completed membership prefix" retirementIndex (Client.oracleClientCanonicalCoveredThrough client)
  assertEqual "Projection covers the same membership prefix" retirementIndex (Projection.projectionCanonicalCoveredThrough projection)
  assertEqual
    "the same control coordinate retains its checked membership evidence"
    [(retirementIndex, successorGeneration)]
    (oracleClientWitnessMembershipAdvances clientWitness)
  assertEqual
    "the owner claim advances atomically with the projected membership"
    successorGeneration
    (oracleHelloMembershipGeneration (oracleClientHelloClaims client))
  assertEqual
    "the survivor is whole-state valid immediately after applying the cut"
    (Right ())
    (validateHeraldState advanced)

  predecessorGeneration <-
    maybe
      (assertFailure "the retirement successor has no predecessor generation")
      pure
      (heraldMembershipGenerationPredecessor successorMembership)
  let retirementEntry = NonEmpty.last entries
  case prepareCanonicalMembershipCursorAdvance retirementEntry predecessorGeneration client of
    Left OracleMembershipCursorEntryMismatch -> pure ()
    Left other ->
      assertFailure
        ("predecessor claim failed for the wrong reason: " <> show other)
    Right _ ->
      assertFailure "the canonical successor entry accepted its predecessor claim"
  (duplicate, _) <-
    applyEntriesAt 3 advanced binding (retirementEntry :| [])
  let duplicateClient = startupOracleClientState duplicate
      duplicateWitness = oracleClientStateWitness duplicateClient
  assertEqual
    "exact membership-entry replay appends no second membership advancement"
    [(retirementIndex, successorGeneration)]
    (oracleClientWitnessMembershipAdvances duplicateWitness)
  assertEqual
    "exact membership-entry replay cannot regress the Hello claim"
    successorGeneration
    (oracleHelloMembershipGeneration (oracleClientHelloClaims duplicateClient))
  assertBool
    "exact membership-entry replay changes neither projection nor Oracle owner"
    ( startupOracleProjectionState duplicate
        == startupOracleProjectionState advanced
        && duplicateClient == client
    )

  (retrying, retryEffects) <-
    checkedIO
      "lose the predecessor-generation Oracle lane after membership application"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 3) (OracleInput (OracleBindingLost binding)))
          duplicate
      )
  retry <- case effectBatchMembers retryEffects of
    [RunOracleClientAction (ScheduleOracleRetry OracleConnectionRetry value)] -> pure value
    observed -> assertFailure ("expected one post-cut Oracle retry, got " <> show observed)
  (connecting, reconnectEffects) <-
    checkedIO
      "reconnect after the applied membership cut"
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 4) (OracleInput (OracleRetryElapsed retry)))
          retrying
      )
  (attempt, reconnectClaims) <- case effectBatchMembers reconnectEffects of
    [ RunOracleClientAction (CancelOracleRetry cancelled),
      RunOracleClientAction (ConnectAndHelloOracle observedAttempt claims)
      ]
        | cancelled == retry -> pure (observedAttempt, claims)
    observed -> assertFailure ("expected one post-cut replacement Hello, got " <> show observed)
  assertEqual
    "the replacement Hello resumes after the retirement coordinate"
    retirementIndex
    (oracleConnectAttemptFromExclusive attempt)
  assertEqual
    "a survivor advertises the successor, not the catch-up predecessor, on replacement Hello"
    successorGeneration
    (oracleHelloMembershipGeneration reconnectClaims)
  assertEqual
    "the reconnecting successor remains whole-state valid"
    (Right ())
    (validateHeraldState connecting)

-- Canonical retirement closes the Oracle client permanently. Local self-
-- fencing instead leaves the control shell reachable while semantic state is
-- frozen, so a genuine grace expiry is the relevant sparse-cache fixture.
caseSelfFencedSparseDuplicate :: Assertion
caseSelfFencedSparseDuplicate = do
  let isolation = checked "sparse self-fence configuration" (checkIsolationConfiguration 1_000 1_000)
      voters = Set.fromList (take 3 (fmap heraldMemberEpoch (checkedActiveHeralds fixtureFailureH4HeraldGenesis)))
      (initial, initialEffects) = checked "initialize sparse self-fence fixture" (initialHeraldWithIsolation (monotonicInstant 0) fixtureFailureH4HeraldGenesis fixtureFailureH4HeraldBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration voters isolation)
  (bound, binding) <- bindFixtureInitial "bind the self-fencing control shell" initial initialEffects
  let grace = Isolation.stateWitness (startupIsolationState bound)
  attempt <- maybe (assertFailure "self-fence grace timer missing") pure (Isolation.isolationWitnessCurrentTimer grace)
  deadline <- maybe (assertFailure "self-fence grace deadline missing") pure (Isolation.isolationWitnessCurrentDeadline grace)
  (fenced, _) <- checkedIO "expire grace through the ordinary transition" (verifiedStepHerald (heraldInput deadline (RuntimeObserved (TimerObserved attempt (TimerFired deadline)))) bound)
  assertEqual "local self-fencing reaches its terminal control-only phase" Isolation.IsolationTerminalView (Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState fenced)))
  assertEqual "self-fencing preserves the live Oracle control binding" (Just binding) (Client.oracleClientCurrentBinding (startupOracleClientState fenced))
  let home = checkedLocalHeraldEpoch fixtureFailureHeraldGenesis
      rejectedEnvelope = failureEnvelope home 900 (Just (controlIndex 99)) (startProcessEpochCommand fixtureStart)
      oracle = checked "self-fence Oracle genesis" (initialOracle fixtureFailureCheckedOracleGenesis)
      (_, rejected) = committed oracle rejectedEnvelope
      rejectedCanonical = canonicalizeAppliedOracleEntry rejected
      rejectedIndex = appliedEntryControlIndex rejected
      now = monotonicInstantWord64 deadline
  (withControlSuffix, _) <- applyEntriesAt now fenced binding (rejectedCanonical :| [])
  assertBool "fenced control suffix keeps the semantic owner frozen" (startupOracleProjectionState fenced == startupOracleProjectionState withControlSuffix)
  assertEqual "the live control owner retains exact suffix evidence" (Just rejectedCanonical) (Projection.appliedEntryEvidence rejectedIndex (startupControlOracleProjectionState withControlSuffix))
  assertEqual "the fenced client removes its duplicate foreign rejection" Nothing (Client.oracleClientRetainedEntry rejectedIndex (startupOracleClientState withControlSuffix))
  assertEqual "fenced compaction reaches the actual live control cursor" rejectedIndex (Client.oracleClientEvidenceCompactedThrough (startupOracleClientState withControlSuffix))
  (withDuplicate, _) <- applyEntriesAt now withControlSuffix binding (rejectedCanonical :| [])
  assertBool "fenced duplicate checks use the live control owner" (withControlSuffix == withDuplicate)
  assertEqual "the sparse fenced whole state remains coherent" (Right ()) (validateHeraldState withControlSuffix)

caseLocalMembershipCutoverDrain :: Assertion
caseLocalMembershipCutoverDrain = do
  let isolation =
        checked
          "local-retirement isolation configuration"
          (checkIsolationConfiguration 1_000 1_000)
      voterHosts =
        Set.fromList
          ( take
              3
              (fmap heraldMemberEpoch (checkedActiveHeralds fixtureFailureH4HeraldGenesis))
          )
      (initial, initialEffects) =
        checked
          "initialize the locally retiring non-voter"
          ( initialHeraldWithIsolation
              (monotonicInstant 0)
              fixtureFailureH4HeraldGenesis
              fixtureFailureH4HeraldBootstraps
              fixtureOracleContacts
              fixtureGeneratorSeed
              fixtureApplicationRecoveryConfiguration
              fixturePeerRecoveryConfiguration
              voterHosts
              isolation
          )
      local = checkedLocalHeraldEpoch fixtureFailureH4HeraldGenesis
      localBootstraps =
        filter
          ((== local) . appliedProcessResidence)
          (checkedInitialBootstraps fixtureFailureH4HeraldBootstraps)
  bootstrap <- case localBootstraps of
    [value] -> pure value
    observed ->
      assertFailure
        ("expected one H4 resident bootstrap, got " <> show (length observed))
  (bound, binding) <-
    bindFixtureInitial
      "bind the locally retiring H4 Oracle client"
      initial
      initialEffects
  let process = appliedProcessEpochId bootstrap
      attachment =
        Session.applicationAttachmentForBootstrap
          (appliedBootstrapManifestId bootstrap)
  preparedOpen <-
    checkedIO
      "open the locally retiring resident application"
      ( Application.prepareApplicationSessionOpen
          local
          attachment
          (Session.clientNonce 71)
          (startupApplicationState bound)
      )
  let (application, acceptance) =
        Application.commitApplicationSessionAcceptance preparedOpen
      session =
        Session.sessionBindingSessionId
          (Session.sessionAcceptanceBinding acceptance)
      withApplication = replaceStartupApplicationState application bound
      (entries, _) = fixtureFailureRetirement
  (withPublication, group) <- retainPublicationGroupFixture acceptance withApplication
  assertBool
    "local retirement starts with real remote publication frontiers"
    (PublicationGroups.groupPendingPeerCount group (Publication.publicationGroups (startupPublicationState withPublication)) > 0)
  assertEqual
    "the live locally retiring predecessor is whole-state valid"
    (Right ())
    (validateHeraldState withPublication)
  (draining, drainEffects) <-
    applyEntriesAt 2 withPublication binding entries
  assertEqual
    "canonical local retirement clears every abandoned remote group frontier"
    Map.empty
    (PublicationGroups.groupFrontiers group (Publication.publicationGroups (startupPublicationState draining)))
  assertBool
    "local retirement keeps the group wait index valid"
    (PublicationGroups.valid (Publication.publicationGroups (startupPublicationState draining)))
  assertEqual
    "the canonical local retirement enters the bounded read-only drain"
    Isolation.IsolationReadOnlyDrainView
    ( Isolation.isolationWitnessPhase
        (Isolation.stateWitness (startupIsolationState draining))
    )
  assertEqual
    "the application session survives until the drain terminal point"
    (Just process)
    (Application.applicationSessionProcess session (startupApplicationState draining))
  assertBool
    "the complete resident branch remains live while reads can use it"
    (not (Controlled.controlledProcessEnded process (startupControlledState draining)))
  assertBool
    "the notice carries the authoritative retirement reason"
    ( any
        ( \case
            SendApplicationIsolationBegun _ _ observedSession reason ->
              observedSession == session && reason == ApplicationHeraldRetired
            _ -> False
        )
        (effectBatchMembers drainEffects)
    )
  assertBool
    "the application is not disposed before the read-only drain terminates"
    ( all
        (\case DisposeApplicationSession {} -> False; _ -> True)
        (effectBatchMembers drainEffects)
    )
  (drainAttempt, drainSpec) <-
    case [ (attempt, specification)
         | ArmTimer attempt specification <- effectBatchMembers drainEffects,
           Just _ <- [timerAttemptIsolationDrain attempt]
         ] of
      [timer] -> pure timer
      observed ->
        assertFailure
          ("expected one isolation-drain timer, got " <> show observed)
  let deadline = timerSpecAbsoluteDeadline drainSpec
  (terminal, terminalEffects) <-
    checkedIO
      "expire the locally retired read-only drain"
      ( verifiedStepHerald
          ( heraldInput
              deadline
              (RuntimeObserved (TimerObserved drainAttempt (TimerFired deadline)))
          )
          draining
      )
  assertEqual
    "the expired local-retirement drain is irreversible"
    Isolation.IsolationTerminalView
    ( Isolation.isolationWitnessPhase
        (Isolation.stateWitness (startupIsolationState terminal))
    )
  assertEqual
    "terminal retirement removes the application session"
    Nothing
    (Application.applicationSessionProcess session (startupApplicationState terminal))
  assertBool
    "terminal retirement applies the deferred complete resident End"
    (Controlled.controlledProcessEnded process (startupControlledState terminal))
  assertBool
    "terminal cleanup disposes the session with the same retirement reason"
    ( any
        ( \case
            DisposeApplicationSession observedSession reason ->
              observedSession == session && reason == ApplicationHeraldRetired
            _ -> False
        )
        (effectBatchMembers terminalEffects)
    )
  assertBool
    "the terminal branch asks the runtime to finish isolation"
    (any (\case FinishIsolation {} -> True; _ -> False) (effectBatchMembers terminalEffects))
  assertEqual
    "the terminal locally retired whole state remains coherent"
    (Right ())
    (validateHeraldState terminal)

caseDrain :: Assertion
caseDrain = do
  (initial, _) <- initialFixture
  (draining, effects) <-
    checkedIO
      "begin drain with active Oracle connect"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant 1)
              ( AdministrationInput
                  ( OrderlyHeraldShutdown
                      (checkedInitialAdministrationBinding fixtureHeraldGenesis)
                      (adminCorrelationId 1)
                  )
              )
          )
          initial
      )
  assertEqual "draining phase" HeraldDraining (heraldPhase draining)
  assertBool
    "client has no retained retry/watch action after the cut"
    (oracleClientWitnessRetired (startupWitnessOracleClient (startupStateWitness draining)))
  assertBool
    "the ordinary drain effect remains the sole runtime cut"
    (case effectBatchMembers effects of [BeginDrain _] -> True; _ -> False)

initialFixture :: IO (HeraldState, EffectBatch)
initialFixture =
  checkedIO
    "initialize Step-12 Herald"
    ( initialHerald
        (monotonicInstant 0)
        fixtureHeraldGenesis
        fixtureHeraldBootstraps
        fixtureOracleContacts
        fixtureGeneratorSeed
        fixtureApplicationRecoveryConfiguration
        fixturePeerRecoveryConfiguration
    )

initialAttempt :: EffectBatch -> IO OracleConnectAttempt
initialAttempt effects =
  case [ attempt
       | RunOracleClientAction (ConnectAndHelloOracle attempt _) <- effectBatchMembers effects
       ] of
    [attempt] -> pure attempt
    attempts ->
      assertFailure
        ( "expected one initial Oracle connect-and-Hello, got "
            <> show attempts
            <> " in "
            <> show (effectBatchMembers effects)
        )

boundFixture :: IO (HeraldState, OracleBinding)
boundFixture = do
  (initial, effects) <- initialFixture
  bindFixtureInitial "accept ready Oracle leader" initial effects

failureBoundFixture :: IO (HeraldState, OracleBinding)
failureBoundFixture = do
  (initial, effects) <-
    checkedIO
      "initialize four-Herald failure fixture"
      ( initialHerald
          (monotonicInstant 0)
          fixtureFailureHeraldGenesis
          fixtureFailureHeraldBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  bindFixtureInitial "accept failure-fixture Oracle leader" initial effects

bindFixtureInitial ::
  String ->
  HeraldState ->
  EffectBatch ->
  IO (HeraldState, OracleBinding)
bindFixtureInitial context initial effects = do
  attempt <- initialAttempt effects
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (controlIndex 2)
          (Just node)
          True
  (bound, bindEffects) <-
    checkedIO
      context
      ( verifiedStepHerald
          (heraldInput (monotonicInstant 1) (OracleInput (OracleHelloReceived attempt acceptance)))
          initial
      )
  binding <- case effectBatchMembers bindEffects of
    [ RunOracleClientAction (BindOracleConnection observedAttempt value),
      RunOracleClientAction (WatchOracle observedBinding cursor)
      ]
        | observedAttempt == attempt,
          observedBinding == value,
          cursor == controlIndex 0 ->
            pure value
    members -> assertFailure ("expected binding then watch, got " <> show members)
  assertEqual
    "binding retains the selected fixed contact"
    (oracleConnectAttemptContact attempt)
    (oracleBindingContact binding)
  pure (bound, binding)

applyEntries ::
  HeraldState ->
  OracleBinding ->
  NonEmpty CanonicalAppliedOracleEntry ->
  IO (HeraldState, EffectBatch)
applyEntries = applyEntriesAt 2

applyEntriesAt ::
  Word64 ->
  HeraldState ->
  OracleBinding ->
  NonEmpty CanonicalAppliedOracleEntry ->
  IO (HeraldState, EffectBatch)
applyEntriesAt observed state binding entries =
  checkedIO
    "apply Oracle entries"
    ( verifiedStepHerald
        (heraldInput (monotonicInstant observed) (OracleInput (OracleEntriesReceived binding entries)))
        state
    )

data HeldControlledFixture = HeldControlledFixture
  { predecessorState :: HeraldState,
    heldState :: HeraldState,
    oracleBinding :: OracleBinding,
    peerBinding :: PeerBinding,
    publicationId :: PublicationId,
    destination :: PublicationDestination,
    item :: SequencedItem PeerPublication,
    controlledObject :: GlobalObjectId,
    controlPrerequisite :: ControlIndex,
    heldEffects :: EffectBatch
  }

heldControlledFixture :: IO HeldControlledFixture
heldControlledFixture = do
  (bound, oracleBinding) <- boundFixture
  retainControlledFixture
    2
    3
    fixtureObjectOne
    (controlIndex 1)
    bound
    oracleBinding

-- | A composed state with one incoming publication retained solely on control
-- index two after a genuine canonical probe prefix, plus an otherwise-unused
-- member suitable for retirement there.
heldControlIndexTwoForMembershipAdvance ::
  IO (HeraldState, PublicationId, HeraldEpoch, HeraldEpoch)
heldControlIndexTwoForMembershipAdvance = do
  (bound, oracleBinding) <- boundFixture
  fixture <-
    retainControlledFixture
      2
      3
      fixtureObjectOne
      (controlIndex 2)
      (fixtureHeraldAfterProbePrefix bound)
      oracleBinding
  pure
    ( fixture.heldState,
      fixture.publicationId,
      heraldMemberEpoch fixtureRemoteMember,
      heraldMemberEpoch fixtureThirdMember
    )

-- | A new applicant replays a real native failure which predates its own
-- admission. Historical exclusion must not create an applicant-owned command
-- which would escape only when its Oracle connection starts after activation.
caseJoiningNativeFailureReplay :: Assertion
caseJoiningNativeFailureReplay = do
  let original = fixtureOracleInitialState
      failed = heraldMemberEpoch fixtureThirdMember
      transcript = NativeFailure.nativeVoterFailureTranscript fixtureCheckedOracleGenesis [] failed
      beforeExclusion = fmap snd (NativeFailure.nativeFailureEvidence transcript <> [NativeFailure.nativeFailureJoint transcript])
      (_, exclusion) = NativeFailure.nativeFailureExcluded transcript
      (retired, _) = NativeFailure.nativeFailureRetired transcript
      applicant = checked "joining failure applicant" (mkHeraldEpoch (fixtureIdentifierBytes 0xf0))
      manifest = Admission.heraldAdmissionManifest (checkedSystemId fixtureHeraldGenesis) (checked "joining failure Herald ID" (mkHeraldId (fixtureIdentifierBytes 0xef))) applicant
      anchor = checked "post-failure admission anchor" (Topology.topologyCut (Topology.sameGenerationPredecessor (checked "post-failure anchor ID" (mkTopologyCutId (fixtureIdentifierBytes 0xee)))) (Topology.topologyFrontier (emptyStructuralVersionVector (oracleCurrentMembership retired)) (oracleGreatestControlIndex retired)) (checked "post-failure anchor digest" (Topology.mkTopologyOccurrenceDigest (fixtureIdentifierBytes 0xed))))
      (begun, _) = committed retired (oracleEnvelope (oracleClientRequestId (heraldMemberEpoch fixtureLocalMember) 990001) Nothing (heraldMemberEpoch fixtureLocalMember) (beginHeraldAdmissionCommand manifest anchor))
  record <- maybe (assertFailure "the retired native transcript must admit a later applicant") pure (oraclePendingHeraldAdmission begun)
  genesis <- checkedIO "joining failure replay genesis" (checkJoiningHeraldGenesis fixtureHeraldGenesis record)
  (initial, _) <- checkedIO "initialize native joining observer" (initialHeraldWithOracleVoters (Voter.oracleVoterConfiguration original) (Voter.oracleReplicaRegistrations original) (monotonicInstant 0) genesis fixtureHeraldBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration Nothing)
  projected <- replay 1 beforeExclusion initial
  assertEqual "historical probe evidence creates no applicant request" [] (Client.oracleClientRequestEntries (startupOracleClientState projected))
  excluded <- replay 2 [exclusion] projected
  let client = startupOracleClientState excluded
      view = Projection.oracleView (startupOracleProjectionState excluded)
  assertEqual "the canonical native exclusion is projected" (Just Voter.VoterExcludedAwaitingHeraldRetirement) (Voter.voterChangePhase <$> Projection.oracleViewPendingVoterChange view)
  assertEqual "the observer reaches the exact native exclusion cursor" (appliedEntryControlIndex (canonicalAppliedOracleEntryValue exclusion)) (Projection.oracleViewControlIndex view)
  assertEqual "historical exclusion retains no local retirement request" [] (Client.oracleClientRequestEntries client)
  assertEqual "passive replay consumes no request identity" 1 (Client.oracleClientNextRequestSequence client)
  assertEqual "passive replay retains no receipt obligation" 0 (Client.oracleClientReceiptRetirementReady client)
  assertBool "the applicant remains a passive observer" (GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState excluded) && Application.applicationMembershipGateClosed (startupApplicationState excluded))
  repeated <- replay 2 [exclusion] excluded
  assertBool "an exact exclusion replay is unchanged" (repeated == excluded)
  where
    replay at entries state = do
      (successor, effects) <- checkedIO "replay native history over the applicant Join lane" (verifiedStepHerald (heraldInput (monotonicInstant at) (JoinInput at (Join.encodeJoinRequest (Join.InstallJoinControlHistory entries)))) state)
      replies <- traverse (checkedIO "decode joining native failure reply") [Join.decodeJoinReply bytes | SendJoinReply correlation bytes <- effectBatchMembers effects, correlation == at]
      assertEqual "the checked historical suffix is installed" [Join.JoinHistoryInstalled] replies
      assertEqual "passive native failure replay emits no Oracle action" [] [action | RunOracleClientAction action <- effectBatchMembers effects]
      pure successor

caseActiveNativeFailureFollowUp :: Assertion
caseActiveNativeFailureFollowUp = do
  let original = fixtureOracleInitialState
      failed = heraldMemberEpoch fixtureThirdMember
      transcript = NativeFailure.nativeVoterFailureTranscript fixtureCheckedOracleGenesis [] failed
      entries = fmap snd (NativeFailure.nativeFailureEvidence transcript <> [NativeFailure.nativeFailureJoint transcript, NativeFailure.nativeFailureExcluded transcript])
      (_, exclusion) = NativeFailure.nativeFailureExcluded transcript
  (initial, initialEffects) <- checkedIO "initialize active native failure survivor" (initialHeraldWithOracleVoters (Voter.oracleVoterConfiguration original) (Voter.oracleReplicaRegistrations original) (monotonicInstant 0) fixtureHeraldGenesis fixtureHeraldBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration Nothing)
  (bound, binding) <- bindFixtureInitial "bind active native failure survivor" initial initialEffects
  (excluded, effects) <- applyEntriesAt 2 bound binding (NonEmpty.fromList entries)
  let client = startupOracleClientState excluded
      retirements = [reference | (reference, witness) <- Client.oracleClientRequestEntries client, Client.RetireHeraldEpochIntent _ target <- [Client.oracleRequestWitnessIntent witness], target == failed]
  assertEqual "a serving survivor retains one semantic retirement intention" 1 (length retirements)
  assertEqual "the new retirement is offered on the live Oracle binding" retirements [oracleRequestDispatchRef dispatch | RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers effects, oracleRequestDispatchRef dispatch `elem` retirements]
  (repeated, _) <- applyEntriesAt 3 excluded binding (exclusion :| [])
  assertEqual "replaying native exclusion cannot allocate another request" (Client.oracleClientNextRequestSequence client) (Client.oracleClientNextRequestSequence (startupOracleClientState repeated))

-- | Full watch composition through accepted voter failure and native exclusion.
-- The survivor publication is held throughout that prefix; the returned callback
-- applies the genuine semantic retirement to any additional staged owner work.
acceptedVoterFailureHeldFixture ::
  IO (HeraldState, PublicationId, HeraldEpoch, HeraldState -> IO HeraldState)
acceptedVoterFailureHeldFixture = do
  (state, publication, failed, retire, _) <- acceptedVoterFailureHeldFixtureWithEntry
  pure (state, publication, failed, retire)

acceptedVoterFailureHeldFixtureWithEntry ::
  IO (HeraldState, PublicationId, HeraldEpoch, HeraldState -> IO HeraldState, CanonicalAppliedOracleEntry)
acceptedVoterFailureHeldFixtureWithEntry = do
  let oracle = fixtureOracleInitialState
      failed = heraldMemberEpoch fixtureThirdMember
      transcript = NativeFailure.nativeVoterFailureTranscript fixtureCheckedOracleGenesis [] failed
      (_, retirement) = NativeFailure.nativeFailureRetired transcript
      retirementIndex = appliedEntryControlIndex (canonicalAppliedOracleEntryValue retirement)
      prefix = NativeFailure.nativeFailurePrelude transcript <> NativeFailure.nativeFailureEvidence transcript <> [NativeFailure.nativeFailureJoint transcript, NativeFailure.nativeFailureExcluded transcript]
  (initial, initialEffects) <- checkedIO "initialize voter-aware control fixture" (initialHeraldWithOracleVoters (Voter.oracleVoterConfiguration oracle) (Voter.oracleReplicaRegistrations oracle) (monotonicInstant 0) fixtureHeraldGenesis fixtureHeraldBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration Nothing)
  (bound, binding) <- bindFixtureInitial "bind voter-aware control fixture" initial initialEffects
  held <- retainControlledFixture 2 3 fixtureObjectOne retirementIndex bound binding
  excluded <- foldM advance held.heldState prefix
  assertHeldControlled held excluded
  let projection = OracleProjection.oracleView (startupOracleProjectionState excluded)
  assertEqual "native exclusion retains old semantic membership" (oracleCurrentMembership oracle) (OracleProjection.oracleViewCurrentHeraldMembership projection)
  assertEqual "accepted failure remains pending semantic retirement" (Just Voter.VoterExcludedAwaitingHeraldRetirement) (Voter.voterChangePhase <$> OracleProjection.oracleViewPendingVoterChange projection)
  let retire state = do
        (successor, _) <- applyEntriesAt (monotonicInstantWord64 (startupLastObservedTime state) + 1) state binding (retirement :| [])
        assertAppliedControlled held successor
        assertEqual "actual canonical retirement is whole-state valid" (Right ()) (validateHeraldState successor)
        pure successor
  pure (excluded, held.publicationId, failed, retire, retirement)
  where
    advance state (_, entry) = do
      binding <- maybe (assertFailure "native-failure fixture lost its Oracle binding") pure (oracleClientCurrentBinding (startupOracleClientState state))
      fst <$> applyEntriesAt (monotonicInstantWord64 (startupLastObservedTime state) + 1) state binding (entry :| [])

-- | A real H2 structural publication is retained only by the surviving H3
-- supplier. H1 follows the complete checked failure transcript but has not yet
-- projected H2's retirement. The next entry is a genuine unrelated rejection.
terminalPayloadReadinessFixture ::
  IO
    ( HeraldState,
      PeerBinding,
      TerminalSource.TerminalStructuralOccurrence,
      CanonicalAppliedOracleEntry,
      CanonicalAppliedOracleEntry,
      HeraldState -> IO (HeraldState, PeerBinding)
    )
terminalPayloadReadinessFixture = do
  let oracle = fixtureOracleInitialState
      failed = heraldMemberEpoch fixtureRemoteMember
      transcript = NativeFailure.nativeVoterFailureTranscript fixtureCheckedOracleGenesis [] failed
      (retiredOracle, retirement) = NativeFailure.nativeFailureRetired transcript
      prefix = NativeFailure.nativeFailurePrelude transcript <> NativeFailure.nativeFailureEvidence transcript <> [NativeFailure.nativeFailureJoint transcript, NativeFailure.nativeFailureExcluded transcript]
      local = heraldMemberEpoch fixtureLocalMember
      (_, noop) = committed retiredOracle (oracleEnvelope (oracleClientRequestId local 990001) Nothing local (Voter.cancelVoterChangeCommand (checked "absent replay-test voter change" (Voter.mkVoterChangeId (controlIndex 999999)))))
  (initial, initialEffects) <- checkedIO "initialize terminal-request receiver" (initialHeraldWithOracleVoters (Voter.oracleVoterConfiguration oracle) (Voter.oracleReplicaRegistrations oracle) (monotonicInstant 0) fixtureHeraldGenesis fixtureHeraldBootstraps fixtureOracleContacts fixtureGeneratorSeed fixtureApplicationRecoveryConfiguration fixturePeerRecoveryConfiguration Nothing)
  (bound, oracleBinding) <- bindFixtureInitial "bind terminal-request receiver" initial initialEffects
  (connected, supplier) <- connectPeerAt 2 12703 fixtureThirdMember bound
  (_, _, item) <- controlledPublicationItem connected fixtureObjectOne (controlIndex 0)
  let publication = sequencedItemPayload item
  stamp <- maybe (assertFailure "terminal fixture publication is not structural") pure (peerPublicationStructuralStamp publication)
  bytes <- maybe (assertFailure "terminal fixture publication has no canonical bytes") pure (peerPublicationStructuralCanonicalBytes publication)
  occurrence <- checkedIO "retain actual terminal publication bytes" (TerminalSource.terminalStructuralOccurrence stamp bytes)
  excluded <- foldM (\state (_, entry) -> fst <$> applyEntriesAt (monotonicInstantWord64 (startupLastObservedTime state) + 1) state oracleBinding (entry :| [])) connected prefix
  let reconnect state = do
        (lost, _) <- checkedIO "lose terminal supplier binding" (verifiedStepHerald (heraldInput (startupLastObservedTime state) (RuntimeObserved (PeerBindingLost supplier))) state)
        connectPeerAt (monotonicInstantWord64 (startupLastObservedTime lost) + 1) 12704 fixtureThirdMember lost
  pure (excluded, supplier, occurrence, retirement, canonicalizeAppliedOracleEntry noop, reconnect)

heldControlIndexTwoFixture :: IO HeldControlledFixture
heldControlIndexTwoFixture = do
  (bound, oracleBinding) <- boundFixture
  (afterOne, _) <-
    applyEntriesAt
      2
      bound
      oracleBinding
      (fixtureEntryOne :| [])
  retainControlledFixture
    3
    4
    fixtureObjectTwo
    (controlIndex 2)
    afterOne
    oracleBinding

retainControlledFixture ::
  Word64 ->
  Word64 ->
  GlobalObjectId ->
  ControlIndex ->
  HeraldState ->
  OracleBinding ->
  IO HeldControlledFixture
retainControlledFixture peerObserved publicationObserved controlledObject controlPrerequisite predecessor oracleBinding = do
  (connected, peerBinding) <- connectRemotePeerAt peerObserved predecessor
  (identifier, destination, item) <-
    controlledPublicationItem connected controlledObject controlPrerequisite
  (heldState, heldEffects) <-
    checkedIO
      "retain controlled publication before Oracle progress"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant publicationObserved)
              (PeerInput (peerPublicationReceived peerBinding (peerPublicationItem item)))
          )
          connected
      )
  pure
    HeldControlledFixture
      { predecessorState = connected,
        heldState,
        oracleBinding,
        peerBinding,
        publicationId = identifier,
        destination,
        item,
        controlledObject,
        controlPrerequisite,
        heldEffects
      }

connectRemotePeerAt :: Word64 -> HeraldState -> IO (HeraldState, PeerBinding)
connectRemotePeerAt observedAt =
  connectPeerAt observedAt 12_701 fixtureRemoteMember

connectPeerAt :: Word64 -> Word64 -> HeraldMember -> HeraldState -> IO (HeraldState, PeerBinding)
connectPeerAt observedAt nonceValue member predecessor = do
  let remoteId = heraldMemberId member
      remoteEpoch = heraldMemberEpoch member
      nonce = connectionNonce nonceValue
      candidate = peerCandidate remoteId remoteEpoch nonce
      hello =
        peerHello
          (checkedSystemId fixtureHeraldGenesis)
          remoteId
          remoteEpoch
          nonce
          Set.empty
          (controlIndex 0)
          (checkedCatalogueDigest fixtureHeraldGenesis)
          (checkedInitialProjectionDigest fixtureHeraldBootstraps)
          Nothing
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState predecessor))
  (connected, effects) <-
    checkedIO
      "admit remote peer before controlled publication"
      ( verifiedStepHerald
          ( heraldInput
              (monotonicInstant observedAt)
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
          )
          predecessor
      )
  binding <- case [value | SetPeerCandidateDisposition _ (PeerHelloAccepted _ value) <- effectBatchMembers effects] of
    [value] -> pure value
    observed -> assertFailure ("expected one admitted remote peer binding, got " <> show observed)
  pure (connected, binding)

controlledPublicationItem ::
  HeraldState ->
  GlobalObjectId ->
  ControlIndex ->
  IO (PublicationId, PublicationDestination, SequencedItem PeerPublication)
controlledPublicationItem state controlledObject controlPrerequisite = do
  source <- case filter ((== remoteEpoch) . appliedProcessResidence) (checkedInitialBootstraps fixtureHeraldBootstraps) of
    [value] -> pure value
    observed -> assertFailure ("expected one remote source process, got " <> show observed)
  root <-
    maybe
      (assertFailure "remote source has no neutral-vertex writer")
      pure
      ( find
          ( \candidate ->
              appliedRootCatalogueRole candidate == NeutralVertexRole
                && case appliedRootRole candidate of
                  WriterRoot _ _ -> True
                  _ -> False
          )
          (appliedProcessRoots source)
      )
  sourceNabla <- case appliedRootRole root of
    WriterRoot value _ -> pure value
    _ -> assertFailure "neutral-vertex root unexpectedly became a reader"
  let identifier =
        publicationId
          sourceNabla
          (appliedRootAuthority root)
          remoteEpoch
          (nablaSequence 0)
      descriptor = predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole)
      objectField = checked "controlled object field" (mkFieldName "object_id")
      labelField = checked "controlled label field" (mkFieldName "label")
  value <-
    checkedIO
      "controlled neutral-vertex value"
      ( recordValue
          [ (objectField, globalUniqueIdValue (globalUniqueIdFromGlobalObjectId controlledObject)),
            (labelField, labelValue (VoidLabel, 0))
          ]
      )
  publication <-
    checkedIO
      "checked controlled publication"
      (mkCheckedPublication descriptor identifier value)
  let destination =
        publicationDestination
          (deriveSystemViewDeltaId (checkedSystemId fixtureHeraldGenesis) localEpoch NeutralVertexRole)
          ( deriveSystemViewStoreIncarnationId
              (checkedSystemId fixtureHeraldGenesis)
              localEpoch
              NeutralVertexRole
          )
          Normal
  batch <-
    checkedIO
      "controlled publication batch"
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
          controlPrerequisite
          (destination :| [])
      )
  structuralDigest <-
    checkedIO
      "controlled structural publication digest"
      ( structuralPublicationDigestFor
          descriptor
          (appliedRootOccurrenceId root)
          publication
          batch
      )
  stamp <-
    checkedIO
      "controlled structural occurrence stamp"
      ( mkStructuralOccurrenceStamp
          (structuralOccurrenceId remoteEpoch firstStructuralSequence)
          ( GraphProgress.structuralAppliedVector
              (startupStructuralProgressState state)
          )
          identifier
          structuralDigest
          NeutralVertexCarrier
      )
  peerPublication <-
    checkedIO
      "controlled peer publication"
      ( mkStructuralPeerPublication
          descriptor
          (appliedRootOccurrenceId root)
          publication
          stamp
          batch
      )
  direction <- checkedIO "controlled stream direction" (mkStreamDirection remoteEpoch localEpoch)
  pure
    ( identifier,
      destination,
      sequencedItem
        direction
        firstStreamSequence
        (peerPublicationDigest peerPublication)
        peerPublication
    )
  where
    remoteEpoch = heraldMemberEpoch fixtureRemoteMember
    localEpoch = checkedLocalHeraldEpoch fixtureHeraldGenesis

assertHeldControlled ::
  HeldControlledFixture ->
  HeraldState ->
  Assertion
assertHeldControlled fixture state = do
  record <- requireIncomingRecord state fixture.publicationId
  assertEqual "controlled publication remains dependency-held" Publication.DependencyHeld (Publication.incomingPublicationDisposition record)
  assertEqual
    "the exact control dependency is retained"
    (Set.singleton (Publication.ControlIndexDependency fixture.controlPrerequisite))
    (Publication.incomingPublicationDependencies record)
  slot <- requireDestinationStore state fixture.destination
  assertEqual
    "held publication has no Store receipt"
    Nothing
    (lookup fixture.publicationId (Store.storeSlotApplicationReceipts slot))
  assertBool
    "held publication has no neutral Graph vertex"
    (not (Graph.graphHasVertex (NeutralVertex fixture.controlledObject) (startupGraphState state)))
  watermarks <-
    checkedIO
      "held incoming watermarks"
      (PeerStream.incomingWatermarks (sequencedItemDirection fixture.item) (startupPeerStreamState state))
  assertEqual "received prefix advances while held" (Just firstStreamSequence) (streamPrefixSequence (streamReceivedPrefix watermarks))
  assertEqual "completed prefix remains empty while held" emptyStreamPrefix (streamCompletedPrefix watermarks)

assertAppliedControlled ::
  HeldControlledFixture ->
  HeraldState ->
  Assertion
assertAppliedControlled fixture state = do
  record <- requireIncomingRecord state fixture.publicationId
  assertEqual "controlled publication is applied" Publication.Applied (Publication.incomingPublicationDisposition record)
  assertEqual "released publication has no dependencies" Set.empty (Publication.incomingPublicationDependencies record)
  slot <- requireDestinationStore state fixture.destination
  assertEqual
    "released publication has one exact Store receipt"
    (Just Normal)
    (lookup fixture.publicationId (Store.storeSlotApplicationReceipts slot))
  assertBool
    "released publication induces its neutral Graph vertex"
    (Graph.graphHasVertex (NeutralVertex fixture.controlledObject) (startupGraphState state))
  watermarks <-
    checkedIO
      "released incoming watermarks"
      (PeerStream.incomingWatermarks (sequencedItemDirection fixture.item) (startupPeerStreamState state))
  assertEqual "received prefix remains through one" (Just firstStreamSequence) (streamPrefixSequence (streamReceivedPrefix watermarks))
  assertEqual "completed prefix advances through one" (Just firstStreamSequence) (streamPrefixSequence (streamCompletedPrefix watermarks))

requireIncomingRecord :: HeraldState -> PublicationId -> IO Publication.IncomingPublicationRecord
requireIncomingRecord state identifier =
  maybe
    (assertFailure "controlled incoming publication record is missing")
    pure
    (Publication.lookupIncomingPublication identifier (startupPublicationState state))

requireDestinationStore :: HeraldState -> PublicationDestination -> IO Store.StoreSlot
requireDestinationStore state destination =
  maybe
    (assertFailure "controlled destination Store slot is missing")
    pure
    (Store.lookupStoreSlot (publicationDestinationDelta destination) (startupStoreState state))

fixtureHeraldGenesis :: CheckedHeraldGenesis
fixtureHeraldGenesis = checked "three-Herald genesis" (checkHeraldGenesis fixtureThreeHeraldDeployment)

fixtureHeraldBootstraps :: CheckedInitialBootstraps
fixtureHeraldBootstraps =
  checked
    "Step-12 initial bootstraps"
    ( checkInitialBootstraps
        fixtureHeraldGenesis
        (PrimordialProcessManifest (take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]))
    )

fixtureFailureHeraldGenesis :: CheckedHeraldGenesis
fixtureFailureHeraldGenesis =
  checked "four-Herald failure genesis" (checkHeraldGenesis fixtureFailureDeployment)

fixtureFailureH4HeraldGenesis :: CheckedHeraldGenesis
fixtureFailureH4HeraldGenesis =
  checked
    "four-Herald local-retirement H4 genesis"
    ( checkHeraldGenesis
        fixtureFailureDeployment
          { deploymentLocalHeraldId = heraldMemberId fixtureFourthMember,
            deploymentLocalHeraldEpoch = heraldMemberEpoch fixtureFourthMember
          }
    )

fixtureFailureHeraldBootstraps :: CheckedInitialBootstraps
fixtureFailureHeraldBootstraps =
  checked
    "four-Herald failure bootstraps"
    ( checkInitialBootstraps
        fixtureFailureHeraldGenesis
        (PrimordialProcessManifest (take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]))
    )

fixtureFailureH4HeraldBootstraps :: CheckedInitialBootstraps
fixtureFailureH4HeraldBootstraps =
  checked
    "four-Herald local-retirement H4 bootstraps"
    ( checkInitialBootstraps
        fixtureFailureH4HeraldGenesis
        (PrimordialProcessManifest (take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId]))
    )

fixtureFailureDeployment :: DeploymentManifest
fixtureFailureDeployment =
  fixtureThreeHeraldDeployment
    { deploymentActiveHeralds = members,
      deploymentConfiguredProcesses =
        fmap relocateH4Process (deploymentConfiguredProcesses fixtureThreeHeraldDeployment),
      deploymentOracleGenesis =
        (deploymentOracleGenesis fixtureThreeHeraldDeployment)
          { oracleGenesisActiveHeralds = members
          }
    }
  where
    members = deploymentActiveHeralds fixtureThreeHeraldDeployment <> [fixtureFourthMember]
    relocateH4Process configured@ConfiguredProcessManifest {configuredBootstrapManifestId = manifest}
      | manifest == fixtureRemoteBootstrapId =
          configured
            { configuredProcessResidence = heraldMemberEpoch fixtureFourthMember
            }
      | otherwise = configured

fixtureThreeHeraldDeployment :: DeploymentManifest
fixtureThreeHeraldDeployment =
  fixtureDeploymentManifest
    { deploymentActiveHeralds = members,
      deploymentOracleGenesis =
        (deploymentOracleGenesis fixtureDeploymentManifest)
          { oracleGenesisActiveHeralds = members
          }
    }
  where
    members = deploymentActiveHeralds fixtureDeploymentManifest <> [fixtureThirdMember]

fixtureRepeatedProcessGenesis :: CheckedHeraldGenesis
fixtureRepeatedProcessGenesis =
  checked
    "genesis with two configured epochs for one stable process"
    (checkHeraldGenesis fixtureRepeatedProcessDeployment)

fixtureRepeatedProcessDeployment :: DeploymentManifest
fixtureRepeatedProcessDeployment =
  fixtureThreeHeraldDeployment
    { deploymentConfiguredProcesses = repeatedProcessManifests
    }
  where
    repeatedProcessManifests =
      case deploymentConfiguredProcesses fixtureThreeHeraldDeployment of
        first : second : remaining ->
          first
            : second {configuredProcessId = configuredProcessId first}
            : remaining
        _ -> error "repeated-process fixture requires two configured processes"

fixtureRepeatedProcessEmptyBootstraps :: CheckedInitialBootstraps
fixtureRepeatedProcessEmptyBootstraps =
  checked
    "empty initial bootstraps for repeated-process projection"
    (checkInitialBootstraps fixtureRepeatedProcessGenesis (PrimordialProcessManifest []))

fixtureRepeatedProcessGenesisBootstraps :: CheckedInitialBootstraps
fixtureRepeatedProcessGenesisBootstraps =
  checked
    "one genesis bootstrap for repeated-process projection"
    ( checkInitialBootstraps
        fixtureRepeatedProcessGenesis
        (PrimordialProcessManifest (take 1 fixtureLocalBootstrapIds))
    )

fixtureThirdMember :: HeraldMember
fixtureThirdMember =
  HeraldMember
    (checked "third Herald id" (mkHeraldId (fixtureIdentifierBytes 0x71)))
    (checked "third Herald epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0x72)))

fixtureFourthMember :: HeraldMember
fixtureFourthMember =
  HeraldMember
    (checked "fourth Herald id" (mkHeraldId (fixtureIdentifierBytes 0x73)))
    (checked "fourth Herald epoch" (mkHeraldEpoch (fixtureIdentifierBytes 0x74)))

fixtureEntryOne :: CanonicalAppliedOracleEntry
fixtureEntryOne = canonicalizeAppliedOracleEntry entry
  where
    (_, entry) = committed fixtureOracleInitialState (fixtureEnvelope 1 fixtureStart)

fixtureEntryTwo :: CanonicalAppliedOracleEntry
fixtureEntryTwo = canonicalizeAppliedOracleEntry entry
  where
    (afterOne, _) = committed fixtureOracleInitialState (fixtureEnvelope 1 fixtureStart)
    (_, entry) = committed afterOne (fixtureEnvelope 2 fixtureStart)

fixtureRejectedEntryTwo :: CanonicalAppliedOracleEntry
fixtureRejectedEntryTwo = canonicalizeAppliedOracleEntry entry
  where
    (afterOne, _) = committed fixtureOracleInitialState (fixtureEnvelope 1 fixtureStart)
    rejectedEnvelope =
      fixtureEnvelopeAt
        2
        fixtureStart
        (Just (controlIndex 99))
    (_, entry) = committed afterOne rejectedEnvelope

fixtureConflictingEntryOne :: CanonicalAppliedOracleEntry
fixtureConflictingEntryOne = canonicalizeAppliedOracleEntry entry
  where
    (_, entry) = committed fixtureOracleInitialState (fixtureEnvelope 3 fixtureStart)

fixtureFailureRetirement ::
  (NonEmpty CanonicalAppliedOracleEntry, HeraldMembershipGeneration)
fixtureFailureRetirement =
  (snd fixtureFailureRetirementOracle, oracleCurrentMembership (fst fixtureFailureRetirementOracle))

-- | A non-voting resident can accept local Start before learning the exact
-- canonical history in which the two surviving voters retire that resident.
dynamicStartRetirementFixture ::
  (CheckedHeraldGenesis, CheckedInitialBootstraps, OracleState, NonEmpty CanonicalAppliedOracleEntry)
dynamicStartRetirementFixture =
  ( fixtureFailureH4HeraldGenesis,
    fixtureFailureH4HeraldBootstraps,
    fst fixtureFailureRetirementOracle,
    snd fixtureFailureRetirementOracle
  )

fixtureFailureRetirementOracle :: (OracleState, NonEmpty CanonicalAppliedOracleEntry)
fixtureFailureRetirementOracle =
  ( retiredOracle,
    canonicalizeAppliedOracleEntry openEntry
      :| fmap
        canonicalizeAppliedOracleEntry
        [firstReportEntry, secondReportEntry, retirementEntry]
  )
  where
    initial = checked "failure Oracle initial state" (initialOracle fixtureFailureCheckedOracleGenesis)
    predecessorGeneration = heraldMembershipGenerationId (oracleCurrentMembership initial)
    target = heraldMemberEpoch fixtureFourthMember
    local = checkedLocalHeraldEpoch fixtureFailureHeraldGenesis
    remote = heraldMemberEpoch fixtureRemoteMember
    probe = checked "retirement probe" (deriveHeraldFailureProbeId (controlIndex 1))
    resolution = deriveFailureProbeResolutionId probe RetireFailureProbeTarget
    (openedOracle, openEntry) =
      committed
        initial
        ( failureEnvelope
            local
            1
            (Just (controlIndex 0))
            (openHeraldFailureProbeCommand target predecessorGeneration (Voter.voterConfigurationId (Voter.oracleVoterConfiguration initial)))
        )
    (reportedOnce, firstReportEntry) =
      committed
        openedOracle
        ( failureEnvelope
            local
            2
            (Just (controlIndex 1))
            (reportHeraldFailureProbeCommand probe (Voter.voterConfigurationId (Voter.oracleVoterConfiguration initial)) ProbeUnreachable)
        )
    (reportedTwice, secondReportEntry) =
      committed
        reportedOnce
        ( failureEnvelope
            remote
            1
            (Just (controlIndex 2))
            (reportHeraldFailureProbeCommand probe (Voter.voterConfigurationId (Voter.oracleVoterConfiguration initial)) ProbeUnreachable)
        )
    (retiredOracle, retirementEntry) =
      committed
        reportedTwice
        ( failureEnvelope
            local
            3
            Nothing
            (retireHeraldEpochCommand resolution target)
        )

failureEnvelope ::
  HeraldEpoch ->
  Word64 ->
  Maybe ControlIndex ->
  OracleCommand ->
  OracleEnvelope
failureEnvelope home sequenceNumber expected command =
  oracleEnvelope
    (oracleClientRequestId home sequenceNumber)
    expected
    home
    command

fixtureRepeatedProcessFirstEntry :: CanonicalAppliedOracleEntry
fixtureRepeatedProcessFirstEntry = canonicalizeAppliedOracleEntry entry
  where
    (_, entry) =
      committed
        fixtureRepeatedProcessOracleInitialState
        (fixtureEnvelope 1 fixtureRepeatedProcessFirstStart)

fixtureRepeatedProcessSecondEntryAtOne :: CanonicalAppliedOracleEntry
fixtureRepeatedProcessSecondEntryAtOne = canonicalizeAppliedOracleEntry entry
  where
    (_, entry) =
      committed
        fixtureRepeatedProcessOracleInitialState
        (fixtureEnvelope 1 fixtureRepeatedProcessSecondStart)

fixtureRepeatedProcessSecondEntryAtTwo :: CanonicalAppliedOracleEntry
fixtureRepeatedProcessSecondEntryAtTwo = canonicalizeAppliedOracleEntry entry
  where
    (afterRejection, _) =
      committed
        fixtureRepeatedProcessOracleInitialState
        ( fixtureEnvelopeAt
            1
            fixtureRepeatedProcessSecondStart
            (Just (controlIndex 99))
        )
    (_, entry) =
      committed
        afterRejection
        (fixtureEnvelope 2 fixtureRepeatedProcessSecondStart)

fixtureObjectOne :: GlobalObjectId
fixtureObjectOne = checked "controlled object one" (mkGlobalObjectId (fixtureIdentifierBytes 0xa1))

fixtureObjectTwo :: GlobalObjectId
fixtureObjectTwo = checked "controlled object two" (mkGlobalObjectId (fixtureIdentifierBytes 0xa2))

fixtureEnvelope :: Word64 -> ProcessStart -> OracleEnvelope
fixtureEnvelope sequenceNumber bootstrap =
  fixtureEnvelopeAt sequenceNumber bootstrap Nothing

fixtureEnvelopeAt ::
  Word64 ->
  ProcessStart ->
  Maybe ControlIndex ->
  OracleEnvelope
fixtureEnvelopeAt sequenceNumber bootstrap expected =
  oracleEnvelope
    (oracleClientRequestId home sequenceNumber)
    expected
    home
    (startProcessEpochCommand bootstrap)
  where
    home = processStartResidence bootstrap

fixtureStart :: ProcessStart
fixtureStart =
  processStart
    (checked "fresh process identity" (mkProcessId (fixtureIdentifierBytes 0xf1)))
    (checked "fresh process epoch" (mkProcessEpochId (fixtureIdentifierBytes 0xf2)))
    (checkedLocalHeraldEpoch fixtureHeraldGenesis)

fixtureRepeatedProcessFirstStart :: ProcessStart
fixtureRepeatedProcessFirstStart =
  repeatedProcessBootstrap "first" firstManifest
  where
    firstManifest = case fixtureLocalBootstrapIds of
      first : _ -> first
      [] -> error "repeated-process fixture requires a first manifest"

fixtureRepeatedProcessSecondStart :: ProcessStart
fixtureRepeatedProcessSecondStart =
  repeatedProcessBootstrap "second" secondManifest
  where
    secondManifest = case fixtureLocalBootstrapIds of
      _ : second : _ -> second
      _ -> error "repeated-process fixture requires a second manifest"

repeatedProcessBootstrap :: String -> BootstrapManifestId -> ProcessStart
repeatedProcessBootstrap context manifest =
  case lookupConfiguredProcessBootstrap manifest fixtureRepeatedProcessGenesis of
    Just configured -> processStart (configuredProcessBootstrapProcessId configured) (configuredProcessBootstrapProcessEpochId configured) (configuredProcessBootstrapResidence configured)
    Nothing -> error (context <> " repeated-process bootstrap is absent from checked genesis")

fixtureStartedProcessEpoch :: ProcessEpochId
fixtureStartedProcessEpoch =
  processStartProcessEpochId fixtureStart

fixtureOracleInitialState :: OracleState
fixtureOracleInitialState = checked "initial Oracle state" (initialOracle fixtureCheckedOracleGenesis)

fixtureRepeatedProcessOracleInitialState :: OracleState
fixtureRepeatedProcessOracleInitialState =
  checked
    "initial Oracle state for repeated-process projection"
    (initialOracle fixtureRepeatedProcessCheckedOracleGenesis)

fixtureRepeatedProcessCheckedOracleGenesis :: CheckedOracleGenesis
fixtureRepeatedProcessCheckedOracleGenesis =
  checkedOracleGenesisFor
    "Oracle genesis for repeated-process projection"
    fixtureRepeatedProcessGenesis
    fixtureRepeatedProcessEmptyBootstraps

fixtureCheckedOracleGenesis :: CheckedOracleGenesis
fixtureCheckedOracleGenesis =
  checkedOracleGenesisWith
    "matching Oracle genesis"

-- The focused retirement fixture adds non-voting H4 to the ordinary
-- three-voter deployment.
fixtureFailureCheckedOracleGenesis :: CheckedOracleGenesis
fixtureFailureCheckedOracleGenesis =
  checked "failure Oracle genesis" (checkOracleGenesis rawGenesis)
  where
    rawGenesis =
      oracleGenesis
        (checkedSystemId fixtureFailureHeraldGenesis)
        (checkedActiveHeralds fixtureFailureHeraldGenesis)
        (checkedCatalogueDigest fixtureFailureHeraldGenesis)
        (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
        (checkedInitialBootstraps fixtureFailureHeraldBootstraps)
        (checkedConfigurationDigest fixtureFailureHeraldGenesis)
        (checkedInitialTopologyProjection fixtureFailureHeraldBootstraps)
        (checkedInitialProjectionDigest fixtureFailureHeraldBootstraps)
        failureVoterBindings
        raftConfiguration
        ( deriveRaftConfigurationDigest
            (checkedSystemId fixtureFailureHeraldGenesis)
            failureVoterBindings
            raftConfiguration
        )

failureVoterBindings :: [RaftVoterBinding]
failureVoterBindings =
  zipWith
    raftVoterBinding
    raftNodes
    (take 3 (fmap heraldMemberEpoch (checkedActiveHeralds fixtureFailureHeraldGenesis)))

checkedOracleGenesisWith ::
  String ->
  CheckedOracleGenesis
checkedOracleGenesisWith context =
  checkedOracleGenesisFor context fixtureHeraldGenesis fixtureHeraldBootstraps

checkedOracleGenesisFor ::
  String ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  CheckedOracleGenesis
checkedOracleGenesisFor context genesis bootstraps = checked context (checkOracleGenesis rawGenesis)
  where
    rawGenesis =
      oracleGenesis
        (checkedSystemId genesis)
        (checkedActiveHeralds genesis)
        (checkedCatalogueDigest genesis)
        (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
        (checkedInitialBootstraps bootstraps)
        (checkedConfigurationDigest genesis)
        (checkedInitialTopologyProjection bootstraps)
        (checkedInitialProjectionDigest bootstraps)
        (voterBindingsFor genesis)
        raftConfiguration
        (deriveRaftConfigurationDigest (checkedSystemId genesis) (voterBindingsFor genesis) raftConfiguration)

voterBindingsFor :: CheckedHeraldGenesis -> [RaftVoterBinding]
voterBindingsFor genesis =
  zipWith raftVoterBinding raftNodes (fmap heraldMemberEpoch (checkedActiveHeralds genesis))

raftNodes :: [RaftNodeId]
raftNodes =
  [ checked "Raft node" (mkRaftNodeId (fixtureIdentifierBytes byte))
  | byte <- [0x91, 0x92, 0x93]
  ]

raftConfiguration :: RaftNativeConfiguration
raftConfiguration =
  checkedRaftNativeConfiguration
    ( checked
        "Raft genesis"
        ( checkRaftGenesis
            ( raftGenesis
                firstRaftNode
                raftNodes
                (duration 100000)
                (duration 300000)
                (duration 500000)
            )
        )
    )
  where
    duration value = checked "Raft duration" (mkRaftDurationMicros value)
    firstRaftNode = case raftNodes of
      first : _ -> first
      [] -> error "fixed voter set is empty"

committed ::
  OracleState ->
  OracleEnvelope ->
  (OracleState, AppliedOracleEntry)
committed state envelope =
  case checked "commit Oracle fixture" (stepOracle envelope state) of
    (successor, OracleCommitted _, effects) -> case oracleEffects effects of
      [EmitAppliedOracleEntry entry] -> (successor, entry)
      other -> error ("expected one Oracle entry, got " <> show other)
    (_, outcome, _) -> error ("expected committed Oracle outcome, got " <> show outcome)

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure
