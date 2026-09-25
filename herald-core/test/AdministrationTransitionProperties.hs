{-# LANGUAGE OverloadedStrings #-}

module AdministrationTransitionProperties
  ( tests,
  )
where

import ConfiguredProcessProperties (bindInitial, commitEntry, submitted)
import Control.Monad (forM_)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( ProcessEpochId,
    SystemId,
    controlIndex,
    mkProcessEpochId,
    mkSystemId,
  )
import Eclips.Domain.Membership (heraldMembershipGenerationActiveHeraldEpochs, heraldMembershipGenerationId)
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ExplicitAdministrativeEnd),
  )
import Eclips.Herald.Administration qualified as Administration
import Eclips.Herald.Administration.Internal (drainId)
import Eclips.Herald.Administration.RPC (AdministrationOutbound (SendEstablishedAdministration), projectFinalAdministrationReply)
import Eclips.Herald.Administration.State qualified as AdministrationState
import Eclips.Herald.Discovery
  ( connectionNonce,
    peerCandidateOpened,
  )
import Eclips.Herald.EffectBatch
  ( AdministrationDispositionTarget (..),
    EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedInitialBootstraps,
    PrimordialProcessManifest (..),
    checkInitialBootstraps,
    checkedActiveHeraldEpochs,
    checkedLocalHeraldEpoch,
    checkedOracleControlIndex,
    checkedSystemId,
  )
import Eclips.Herald.Initialization (HeraldState, initialHerald, initialHeraldWithIsolation)
import Eclips.Herald.Input
  ( AdministrationIngress (..),
    HeraldInputBody (..),
    PeerIngress (PeerCandidateOpened),
    RuntimeObservation (AdministrationBindingLost, DrainBarrierObserved, TimerObserved),
    candidateAdministrationLane,
    heraldInput,
  )
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.Join (JoinReply (JoinRejected, OracleStatusReply), JoinRequest (InstallJoinControlHistory, ReadOracleStatus), decodeJoinReply, encodeJoinRequest)
import Eclips.Herald.OracleClient
  ( OracleClientAction (..),
    OracleClientIngress
      ( OracleBindingLost,
        OracleEntriesReceived,
        OracleHelloReceived,
        OracleLocalReplicaConfigured,
        OracleRetryElapsed
      ),
    OracleRetryPurpose (OracleConnectionRetry),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.State
  ( HeraldPhase (HeraldStopped),
    heraldPhase,
    replaceStartupLastObservedTime,
    startupAdministrationState,
    startupIsolationState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer (TimerOutcome (TimerFired))
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue, canonicalizeOracleReceipt)
import Eclips.Oracle.Genesis (checkedOracleRaftVoterBindings, raftVoterBindingHeraldEpoch, raftVoterBindingNode)
import Eclips.Oracle.Projection (appliedEntryCommand, appliedEntryReceipt)
import Eclips.Oracle.Transition qualified as Oracle
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Admin.Types qualified as Protocol
import Eclips.Raft.Identity (RaftNodeId)
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureIdentifierBytes,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
import GenesisFixtures qualified as Fixtures
import ProcessPreparationProperties qualified as PreparationFixture
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
    "Start/End administration transition"
    [ testCase "AdminHello authenticates deployment without touching private shutdown" caseHelloAuthentication,
      testCase "operator status and preparation snapshots are read-only and binding-scoped" caseOperatorSnapshot,
      testCase "terminal isolation still permits an operator status snapshot" caseIsolatedOperatorSnapshot,
      testCase "operator lists an actual retained local preparation without mutating it" caseRetainedPreparationSnapshot,
      testCase "public drain retains completion and routes only a live final binding" casePublicDrain,
      testCase "Start retry, conflict, query, reconnect, and loss are exact" caseRetainedLifecycle,
      testCase "End retry and cross-command correlation conflicts are exact" caseEndRetainedLifecycle,
      testCase "missing local replica endpoints retain refusal without allocating Oracle identity" caseVoterEndpointRefusal,
      QC.testProperty "live Prepare retries retain identity; replacement operators own independent receipts" propVoterPrepareRetry,
      testCase "voter correlations cannot be reused for other voter or process mutations" caseVoterCorrelationConflict,
      testCase "committed registration settles its canonical administration receipt once" caseVoterRegistrationReceipt,
      testCase "loss, timer, or peer observations never synthesize semantic requests" caseObservationsDoNotSynthesizeRequests
    ]

caseVoterEndpointRefusal :: Assertion
caseVoterEndpointRefusal = do
  bound <- boundInitialHerald
  (opened, binding) <- openAt 2 701 bound
  let fixtureCorrelation = Administration.adminCorrelationIdForBinding binding 71
  let originalClient = startupOracleClientState opened
      refusal =
        Administration.RetainedAdminResult
          fixtureCorrelation
          (Administration.AdminRequestRejected Administration.AdminOracleReplicaEndpointsNotConfigured)
  (rejected, effects) <- stepAt 3 (AdministrationInput (PrepareOracleReplica binding fixtureCorrelation)) opened
  assertEqual "explicit local configuration refusal" [SendAdministrationReply binding refusal] (effectBatchMembers effects)
  assertEqual "endpoint refusal allocates no Oracle request identity" originalClient (startupOracleClientState rejected)
  assertEqual
    "refusal is retained independently of an Oracle request"
    [(fixtureCorrelation, Administration.AdminOracleReplicaEndpointsNotConfigured)]
    (AdministrationState.voterPreparationRejectionEntries (startupAdministrationState rejected))
  assertEqual "no voter request reference exists" [] (AdministrationState.voterAdministrationEntries (startupAdministrationState rejected))
  (retried, retryEffects) <- stepAt 4 (AdministrationInput (PrepareOracleReplica binding fixtureCorrelation)) rejected
  assertReadOnly rejected retried
  assertEqual "exact refusal retry returns the original result" (effectBatchMembers effects) (effectBatchMembers retryEffects)
  (reconnected, current) <- openAt 5 702 retried
  let currentCorrelation = Administration.adminCorrelationIdForBinding current 71
      currentRefusal = Administration.RetainedAdminResult currentCorrelation (Administration.AdminRequestRejected Administration.AdminOracleReplicaEndpointsNotConfigured)
  (queried, queryEffects) <- stepAt 6 (AdministrationInput (GetAdministrationResult current currentCorrelation)) reconnected
  assertReadOnly reconnected queried
  assertEqual "a new operator owns a fresh receipt lifetime" [SendAdministrationReply current (Administration.AbsentAdminResult currentCorrelation)] (effectBatchMembers queryEffects)
  (fresh, freshEffects) <- stepAt 7 (AdministrationInput (PrepareOracleReplica current currentCorrelation)) queried
  assertEqual "same ordinal is independently admitted on the new lifetime" [SendAdministrationReply current currentRefusal] (effectBatchMembers freshEffects)
  (conflicted, conflictEffects) <- stepAt 8 (AdministrationInput (StartProcessEpoch current (Administration.startProcessRequest currentCorrelation))) fresh
  assertReadOnly fresh conflicted
  assertEqual "the new refused Prepare owns its own correlation" [SendAdministrationReply current (Administration.ConflictingAdminResult currentCorrelation)] (effectBatchMembers conflictEffects)
  assertEqual "refusal, closure, and queries allocate no Oracle identity" originalClient (startupOracleClientState conflicted)

propVoterPrepareRetry :: Word64 -> Bool -> QC.Property
propVoterPrepareRetry ordinal reconnect = QC.ioProperty $ do
  configured <- configuredVoterHerald
  (opened, binding) <- openAt 3 711 configured
  let correlation = Administration.adminCorrelationIdForBinding binding ordinal
      beforeSequence = OracleClient.oracleClientNextRequestSequence (startupOracleClientState opened)
  (accepted, effects) <- stepAt 4 (AdministrationInput (PrepareOracleReplica binding correlation)) opened
  assertAcceptedWithSubmit binding correlation effects
  envelope <- submitted effects
  assertEqual
    "first Prepare allocates one Oracle identity"
    (beforeSequence + 1)
    (OracleClient.oracleClientNextRequestSequence (startupOracleClientState accepted))
  (retried, retryEffects) <- stepAt 5 (AdministrationInput (PrepareOracleReplica binding correlation)) accepted
  assertReadOnly accepted retried
  assertAcceptedWithSubmit binding correlation retryEffects
  assertEqual "live Prepare retry submits the byte-identical canonical envelope" envelope =<< submitted retryEffects
  if reconnect
    then do
      (replacement, current) <- openAt 6 712 retried
      let freshCorrelation = Administration.adminCorrelationIdForBinding current ordinal
      (absent, absentEffects) <- stepAt 7 (AdministrationInput (GetAdministrationResult current freshCorrelation)) replacement
      assertReadOnly replacement absent
      assertEqual "replaced delivery lifetime is absent" [SendAdministrationReply current (Administration.AbsentAdminResult freshCorrelation)] (effectBatchMembers absentEffects)
      (fresh, freshEffects) <- stepAt 8 (AdministrationInput (PrepareOracleReplica current freshCorrelation)) absent
      freshReference <- case [reference | (owner, reference, Nothing) <- AdministrationState.voterAdministrationEntries (startupAdministrationState fresh), owner == freshCorrelation] of
        [reference] -> pure reference
        _ -> assertFailure "fresh binding must own one pending Oracle request"
      let reoffers = [dispatch | RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers freshEffects]
      freshEnvelope <- case [OracleClient.oracleRequestDispatchEnvelope dispatch | dispatch <- reoffers, OracleClient.oracleRequestDispatchRef dispatch == freshReference] of
        [value] -> pure value
        _ -> assertFailure "fresh binding's pending request must be reoffered once"
      assertEqual "only the current binding receives acceptance" [SendAdministrationReply current (Administration.RetainedAdminResult freshCorrelation Administration.AdminRequestAccepted)] [reply | reply@SendAdministrationReply {} <- effectBatchMembers freshEffects]
      assertEqual "new work also reoffers the older still-pending intention" [envelope, freshEnvelope] (map OracleClient.oracleRequestDispatchEnvelope reoffers)
      assertBool "fresh lifetime allocates a distinct Oracle identity" (freshEnvelope /= envelope)
      assertEqual "both pending semantic requests survive" 2 (length (AdministrationState.voterAdministrationEntries (startupAdministrationState fresh)))
    else do
      (queried, queryEffects) <- stepAt 6 (AdministrationInput (GetAdministrationResult binding correlation)) retried
      assertReadOnly retried queried
      assertRetainedAccepted binding correlation queryEffects
  pure True

caseVoterCorrelationConflict :: Assertion
caseVoterCorrelationConflict = do
  configured <- configuredVoterHerald
  (opened, binding) <- openAt 3 721 configured
  let fixtureCorrelation = Administration.adminCorrelationIdForBinding binding 71
  (accepted, _) <- stepAt 4 (AdministrationInput (PrepareOracleReplica binding fixtureCorrelation)) opened
  oracle <- checkedIO (Oracle.initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps fixtureCheckedGenesis fixtureInitialBootstraps))
  let configuration = Voter.oracleVoterConfiguration oracle
  bindings <- checkedIO (Voter.oracleVoterBindings (Voter.voterConfigurationBindings configuration))
  change <- checkedIO (Voter.mkVoterChangeId (controlIndex 1))
  let conflicting =
        [ StartProcessEpoch binding (Administration.startProcessRequest fixtureCorrelation),
          EndProcessEpoch binding (Administration.endProcessEpochRequest fixtureCorrelation fixtureProcessEpoch ExplicitAdministrativeEnd),
          BeginVoterChange binding fixtureCorrelation (Voter.voterConfigurationId configuration) Voter.ExplicitCommission bindings,
          CancelVoterChange binding fixtureCorrelation change
        ]
  forM_ conflicting $ \request -> do
    (unchanged, effects) <- stepAt 5 (AdministrationInput request) accepted
    assertReadOnly accepted unchanged
    assertEqual
      "different mutation cannot overwrite a voter-owned correlation"
      [SendAdministrationReply binding (Administration.ConflictingAdminResult fixtureCorrelation)]
      (effectBatchMembers effects)
  -- The reservation is symmetric: a voter operation cannot appropriate a
  -- correlation already retained for a process lifecycle operation either.
  (started, _) <- stepAt 4 (AdministrationInput (StartProcessEpoch binding (Administration.startProcessRequest fixtureCorrelation))) opened
  (unchanged, effects) <- stepAt 5 (AdministrationInput (PrepareOracleReplica binding fixtureCorrelation)) started
  assertReadOnly started unchanged
  assertEqual
    "Prepare cannot overwrite a Start-owned correlation"
    [SendAdministrationReply binding (Administration.ConflictingAdminResult fixtureCorrelation)]
    (effectBatchMembers effects)

caseVoterRegistrationReceipt :: Assertion
caseVoterRegistrationReceipt = do
  configured <- configuredVoterHerald
  (opened, binding) <- openAt 3 731 configured
  let fixtureCorrelation = Administration.adminCorrelationIdForBinding binding 71
  (accepted, acceptedEffects) <- stepAt 4 (AdministrationInput (PrepareOracleReplica binding fixtureCorrelation)) opened
  envelope <- submitted acceptedEffects
  oracle <- checkedIO (Oracle.initialOracle (Fixtures.fixtureCheckedOracleGenesisWithBootstraps fixtureCheckedGenesis fixtureInitialBootstraps))
  (_, entry) <- commitEntry oracle envelope
  command <- maybe (assertFailure "registration emitted no command entry") pure (appliedEntryCommand (canonicalAppliedOracleEntryValue entry))
  let receipt = canonicalizeOracleReceipt (appliedEntryReceipt command)
      result = Administration.RetainedAdminResult fixtureCorrelation (Administration.AdminOracleVoterResult receipt)
  oracleBinding <-
    maybe
      (assertFailure "configured voter has no Oracle binding")
      pure
      (OracleClient.oracleClientCurrentBinding (startupOracleClientState accepted))
  (applied, _) <- stepAt 5 (OracleInput (OracleEntriesReceived oracleBinding (NonEmpty.singleton entry))) accepted
  assertEqual
    "registration settles the exact retained canonical receipt"
    (Just (Administration.AdminOracleVoterResult receipt))
    (AdministrationState.lookupAdministrationResultStatus fixtureCorrelation (startupAdministrationState applied))
  let registrations = Projection.oracleViewOracleReplicas (Projection.oracleView (startupOracleProjectionState applied))
  case find ((== fixtureVoterNode) . Voter.replicaRegistrationNode) registrations of
    Nothing -> assertFailure "registered replica missing from the applied owner projection"
    Just registration -> do
      assertEqual "Prepare uses the checked local Herald epoch" (checkedLocalHeraldEpoch fixtureCheckedGenesis) (Voter.replicaRegistrationHost registration)
      assertEqual "Prepare uses the configured local transport facts" (Just fixtureReplicaContact) (Voter.replicaRegistrationContact registration)
  (queried, queryEffects) <- stepAt 6 (AdministrationInput (GetAdministrationResult binding fixtureCorrelation)) applied
  assertReadOnly applied queried
  assertEqual "result query returns canonical receipt" [SendAdministrationReply binding result] (effectBatchMembers queryEffects)
  (retried, retryEffects) <- stepAt 7 (AdministrationInput (PrepareOracleReplica binding fixtureCorrelation)) queried
  assertReadOnly queried retried
  assertEqual
    "settled Prepare retry returns receipt and schedules its retirement without resubmitting"
    [SendAdministrationReply binding result, RunOracleClientAction (ScheduleOracleProgress oracleBinding)]
    (effectBatchMembers retryEffects)
  (duplicate, _) <- stepAt 8 (OracleInput (OracleEntriesReceived oracleBinding (NonEmpty.singleton entry))) retried
  assertBool
    "duplicate watch cannot rewrite the retained result"
    (startupAdministrationState retried == startupAdministrationState duplicate)

configuredVoterHerald :: IO HeraldState
configuredVoterHerald = do
  bound <- boundInitialHerald
  (configured, effects) <- stepAt 2 (OracleInput (OracleLocalReplicaConfigured fixtureVoterNode fixtureReplicaContact)) bound
  assertEqual "local endpoint configuration emits no network or Oracle work" [] (effectBatchMembers effects)
  assertEqual
    "local endpoint configuration mints no request"
    (OracleClient.oracleClientNextRequestSequence (startupOracleClientState bound))
    (OracleClient.oracleClientNextRequestSequence (startupOracleClientState configured))
  pure configured

fixtureVoterNode :: RaftNodeId
fixtureVoterNode = case find
  ((== checkedLocalHeraldEpoch fixtureCheckedGenesis) . raftVoterBindingHeraldEpoch)
  (checkedOracleRaftVoterBindings (Fixtures.fixtureCheckedOracleGenesisWithBootstraps fixtureCheckedGenesis fixtureInitialBootstraps)) of
  Just binding -> raftVoterBindingNode binding
  Nothing -> error "administration fixture has no local voter binding"

fixtureReplicaContact :: Voter.OracleReplicaContact
fixtureReplicaContact = checked $ do
  oracle <- Voter.oracleReplicaEndpoint "127.0.0.1" 41003
  raft <- Voter.oracleReplicaEndpoint "127.0.0.1" 41004
  discovery <- Voter.oracleReplicaEndpoint "127.0.0.1" 41000
  pure (Voter.oracleReplicaContact oracle raft discovery)

caseOperatorSnapshot :: Assertion
caseOperatorSnapshot = do
  bound <- boundInitialHerald
  (opened, binding) <- openAt 2 601 bound
  let fixtureCorrelation = Administration.adminCorrelationIdForBinding binding 71
  (observed, effects) <- stepAt 3 (AdministrationInput (GetHeraldStatus binding fixtureCorrelation)) opened
  assertReadOnly opened observed
  let view = Projection.oracleView (startupOracleProjectionState opened)
      membership = Projection.oracleViewCurrentHeraldMembership view
  case effectBatchMembers effects of
    [SendAdministrationReply actualBinding (Administration.AdminHeraldStatusReply correlation (Administration.AdminHeraldStatus system local members voters generation index phase connected hint))] -> do
      assertEqual "snapshot binding" binding actualBinding
      assertEqual "snapshot correlation" fixtureCorrelation correlation
      assertEqual "deployment" fixtureSystem system
      assertEqual "local epoch" (checkedLocalHeraldEpoch fixtureCheckedGenesis) local
      assertEqual "membership projection" (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)) members
      assertEqual "voter hosts" (Set.toAscList (Isolation.isolationWitnessVoterHosts (Isolation.stateWitness (startupIsolationState opened)))) voters
      assertEqual "membership generation" (heraldMembershipGenerationId membership) generation
      assertEqual "applied control" (Projection.oracleViewControlIndex view) index
      assertEqual "serving phase" Administration.AdminServing phase
      assertBool "Oracle connection is reported" (connected /= Nothing)
      assertEqual "known leader" connected hint
    other -> assertFailure ("operator snapshot: " <> show other)
  (listed, listedEffects) <- stepAt 4 (AdministrationInput (ListChildPreparations binding fixtureCorrelation)) observed
  assertReadOnly observed listed
  assertEqual "empty preparation snapshot" [SendAdministrationReply binding (Administration.AdminPreparationsReply fixtureCorrelation [])] (effectBatchMembers listedEffects)
  (rebound, _) <- openAt 5 602 listed
  (stale, staleEffects) <- stepAt 6 (AdministrationInput (GetHeraldStatus binding fixtureCorrelation)) rebound
  assertReadOnly rebound stale
  assertEqual "stale operator has no read access" [RejectAdministrationConnection (EstablishedAdministrationDisposition binding)] (effectBatchMembers staleEffects)

caseRetainedPreparationSnapshot :: Assertion
caseRetainedPreparationSnapshot = do
  fixture <- PreparationFixture.fixture
  (pending, _, reference) <- PreparationFixture.begin fixture PreparationFixture.emptySelection
  process <- PreparationFixture.childProcess reference pending
  binding <-
    maybe
      (assertFailure "preparation fixture has no current operator")
      pure
      (AdministrationState.currentConfiguredAdministrationBinding (startupAdministrationState pending))
  let fixtureCorrelation = Administration.adminCorrelationIdForBinding binding 71
  (listed, effects) <- stepAt 100 (AdministrationInput (ListChildPreparations binding fixtureCorrelation)) pending
  assertReadOnly pending listed
  assertEqual
    "real retained preparation handle and generated process"
    [ SendAdministrationReply
        binding
        ( Administration.AdminPreparationsReply
            fixtureCorrelation
            [Administration.AdminPreparationInspection reference (Just process) Administration.AdminPreparationPreparing Nothing]
        )
    ]
    (effectBatchMembers effects)

caseIsolatedOperatorSnapshot :: Assertion
caseIsolatedOperatorSnapshot = do
  let genesis = Fixtures.fixtureStep14CheckedGenesis
      local = checkedLocalHeraldEpoch genesis
      voters = Set.delete local (Set.fromList (checkedActiveHeraldEpochs genesis))
  configuration <- checkedIO (checkIsolationConfiguration 1000 1000)
  (initial, _) <-
    checkedIO
      ( initialHeraldWithIsolation
          (monotonicInstant 0)
          genesis
          (Fixtures.fixtureCheckedInitialBootstrapsFor genesis)
          fixtureOracleContacts
          Fixtures.fixtureGeneratorSeedH1
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
          voters
          configuration
      )
  (connected, _, _) <- bindInitial genesis initial
  let witness = Isolation.stateWitness (startupIsolationState connected)
  timer <- maybe (assertFailure "isolation timer missing") pure (Isolation.isolationWitnessCurrentTimer witness)
  deadline <- maybe (assertFailure "isolation deadline missing") pure (Isolation.isolationWitnessCurrentDeadline witness)
  (fenced, _) <- checkedIO (verifiedStepHerald (heraldInput deadline (RuntimeObserved (TimerObserved timer (TimerFired deadline)))) connected)
  let stepNow body = checkedIO . verifiedStepHerald (heraldInput deadline body)
      lane = candidateAdministrationLane 604
  (rebound, bindingEffects) <- stepNow (AdministrationInput (OpenAdministrationConnection lane (checkedSystemId genesis))) fenced
  binding <- case effectBatchMembers bindingEffects of
    [SetAdministrationConnectionDisposition actualLane admitted] | actualLane == lane -> pure admitted
    other -> assertFailure ("self-fenced owner rejected its new operator binding: " <> show other)
  let fixtureCorrelation = Administration.adminCorrelationIdForBinding binding 71
  (observed, effects) <- stepNow (AdministrationInput (GetHeraldStatus binding fixtureCorrelation)) rebound
  assertReadOnly rebound observed
  case effectBatchMembers effects of
    [SendAdministrationReply actualBinding (Administration.AdminHeraldStatusReply correlation (Administration.AdminHeraldStatus _ _ _ _ _ _ phase _ _))] -> do
      assertEqual "isolated snapshot binding" binding actualBinding
      assertEqual "isolated snapshot correlation" fixtureCorrelation correlation
      assertEqual "terminal local isolation remains visible" Administration.AdminIsolated phase
    other -> assertFailure ("isolated operator snapshot: " <> show other)
  (queried, queryEffects) <- stepNow (JoinInput 1 (encodeJoinRequest ReadOracleStatus)) observed
  assertReadOnly observed queried
  case effectBatchMembers queryEffects of
    [SendJoinReply 1 bytes] -> case decodeJoinReply bytes of
      Right OracleStatusReply {} -> pure ()
      other -> assertFailure ("self-fenced Oracle status: " <> show other)
    other -> assertFailure ("self-fenced Oracle query effects: " <> show other)
  (rejected, rejectionEffects) <- stepNow (JoinInput 2 (encodeJoinRequest (InstallJoinControlHistory []))) queried
  assertReadOnly queried rejected
  case effectBatchMembers rejectionEffects of
    [SendJoinReply 2 bytes] -> assertEqual "self-fenced onboarding mutation is rejected" (Right JoinRejected) (decodeJoinReply bytes)
    other -> assertFailure ("self-fenced onboarding rejection: " <> show other)
  (draining, drainEffects) <- stepNow (AdministrationInput (DrainConfiguredHerald binding fixtureCorrelation)) rejected
  assertBool "self-fenced operator can explicitly begin orderly shutdown" (BeginDrain (drainId 1) `elem` effectBatchMembers drainEffects)
  (stopped, completion) <- stepNow (RuntimeObserved (DrainBarrierObserved (drainId 1))) draining
  assertEqual "self-fenced control shell stops only after explicit drain" HeraldStopped (heraldPhase stopped)
  assertEqual "self-fenced orderly completion retains the operator route" [FinishDrain (drainId 1) (Just (Administration.ConfiguredHeraldShutdownCompleted binding fixtureCorrelation))] (effectBatchMembers completion)

casePublicDrain :: Assertion
casePublicDrain = do
  bound <- boundInitialHerald
  (opened, binding) <- openAt 2 603 bound
  let fixtureCorrelation = Administration.adminCorrelationIdForBinding binding 71
  (draining, effects) <- stepAt 3 (AdministrationInput (DrainConfiguredHerald binding fixtureCorrelation)) opened
  assertBool "public drain acknowledges its exact request" (SendAdministrationReply binding (Administration.AdminDrainAccepted fixtureCorrelation) `elem` effectBatchMembers effects)
  assertBool "public drain enters the existing runtime barrier" (BeginDrain (drainId 1) `elem` effectBatchMembers effects)
  (stopped, completed) <- stepAt 4 (RuntimeObserved (DrainBarrierObserved (drainId 1))) draining
  let final = Administration.ConfiguredHeraldShutdownCompleted binding fixtureCorrelation
  assertEqual "public drain stops" HeraldStopped (heraldPhase stopped)
  assertEqual "final reply shares the indivisible drain effect" [FinishDrain (drainId 1) (Just final)] (effectBatchMembers completed)
  assertEqual "final reply uses the established operator route" (Just (SendEstablishedAdministration binding (Protocol.HeraldDrained (Protocol.adminCorrelationIdClaim 71)))) (projectFinalAdministrationReply final)
  assertEqual "completion is immutable in its owner" (Just (fixtureCorrelation, drainId 1, AdministrationState.AdministrationShutdownCompleted)) (AdministrationState.administrationWitnessShutdown (administrationWitness stopped))
  (lost, _) <- stepAt 4 (RuntimeObserved (AdministrationBindingLost binding)) draining
  (lostStopped, lostCompleted) <- stepAt 5 (RuntimeObserved (DrainBarrierObserved (drainId 1))) lost
  assertEqual "lost operator cannot receive final reply" [FinishDrain (drainId 1) Nothing] (effectBatchMembers lostCompleted)
  assertEqual "lost reply still retains completion" (AdministrationState.administrationWitnessShutdown (administrationWitness stopped)) (AdministrationState.administrationWitnessShutdown (administrationWitness lostStopped))

assertReadOnly :: HeraldState -> HeraldState -> Assertion
assertReadOnly before after = assertBool "inspection changes only observed time" (before == replaceStartupLastObservedTime (startupLastObservedTime before) after)

caseHelloAuthentication :: Assertion
caseHelloAuthentication = do
  bound <- boundInitialHerald
  let lane = candidateAdministrationLane 3
      before = administrationWitness bound
  (rejected, rejectionEffects) <-
    stepAt
      2
      (AdministrationInput (OpenAdministrationConnection lane foreignSystem))
      bound
  assertEqual
    "invalid deployment rejects the candidate"
    [ RejectAdministrationConnection
        (CandidateAdministrationDisposition lane)
    ]
    (effectBatchMembers rejectionEffects)
  assertEqual
    "invalid Hello mutates no administration owner fact"
    before
    (administrationWitness rejected)
  (accepted, acceptanceEffects) <-
    stepAt
      3
      (AdministrationInput (OpenAdministrationConnection lane fixtureSystem))
      rejected
  binding <- case effectBatchMembers acceptanceEffects of
    [SetAdministrationConnectionDisposition observedLane observedBinding]
      | observedLane == lane -> pure observedBinding
    other -> assertFailure ("expected one administration acceptance, got " <> show other)
  let witness = administrationWitness accepted
  assertEqual
    "external binding becomes current"
    (Just binding)
    (AdministrationState.administrationWitnessConfiguredBinding witness)
  assertEqual
    "private orderly-shutdown binding remains generation zero"
    (Administration.checkedInitialAdministrationBinding fixtureCheckedGenesis)
    (AdministrationState.administrationWitnessBinding witness)
  assertEqual
    "first external generation advances exactly once"
    2
    (AdministrationState.administrationWitnessNextConfiguredBindingGeneration witness)

caseRetainedLifecycle :: Assertion
caseRetainedLifecycle = do
  bound <- boundInitialHerald
  (opened, binding1) <- openAt 2 10 bound
  let fixtureCorrelation = Administration.adminCorrelationIdForBinding binding1 71
  let request =
        Administration.startProcessRequest fixtureCorrelation
  (started, startEffects) <-
    stepAt
      3
      (AdministrationInput (StartProcessEpoch binding1 request))
      opened
  assertAcceptedWithSubmit binding1 fixtureCorrelation startEffects
  let startedAdministration = startupAdministrationState started
      startedOracle = startupOracleClientState started
  assertEqual
    "one accepted record is retained"
    1
    (length (AdministrationState.configuredStartEntries startedAdministration))
  assertEqual
    "acceptance ordinal advances once"
    2
    (AdministrationState.configuredStartNextAcceptanceOrdinal startedAdministration)
  assertEqual
    "Oracle request sequence advances once"
    2
    (OracleClient.oracleClientNextRequestSequence startedOracle)

  (retried, retryEffects) <-
    stepAt
      4
      (AdministrationInput (StartProcessEpoch binding1 request))
      started
  assertAcceptedWithSubmit binding1 fixtureCorrelation retryEffects
  assertBool
    "exact retry is administration-state identical"
    (startedAdministration == startupAdministrationState retried)
  assertEqual
    "exact retry is Oracle-state identical"
    startedOracle
    (startupOracleClientState retried)

  let conflicted = retried

  let crossCommand =
        Administration.endProcessEpochRequest
          fixtureCorrelation
          fixtureProcessEpoch
          ExplicitAdministrativeEnd
  (crossConflicted, crossConflictEffects) <-
    stepAt
      6
      (AdministrationInput (EndProcessEpoch binding1 crossCommand))
      conflicted
  assertEqual
    "reusing a Start correlation for End is an immutable conflict"
    [ SendAdministrationReply
        binding1
        (Administration.ConflictingAdminResult fixtureCorrelation)
    ]
    (effectBatchMembers crossConflictEffects)
  assertEqual
    "cross-command conflict allocates no Oracle request"
    startedOracle
    (startupOracleClientState crossConflicted)

  (queried, queryEffects) <-
    stepAt
      7
      (AdministrationInput (GetAdministrationResult binding1 fixtureCorrelation))
      crossConflicted
  assertRetainedAccepted binding1 fixtureCorrelation queryEffects
  let unknown = Administration.adminCorrelationIdForBinding binding1 999
      beforeAbsent = administrationWitness queried
  (absent, absentEffects) <-
    stepAt
      8
      (AdministrationInput (GetAdministrationResult binding1 unknown))
      queried
  assertEqual
    "unknown query returns absence"
    [ SendAdministrationReply
        binding1
        (Administration.AbsentAdminResult unknown)
    ]
    (effectBatchMembers absentEffects)
  assertEqual
    "absence allocates no record or counter"
    beforeAbsent
    (administrationWitness absent)

  (reconnected, binding2) <- openAt 9 11 absent
  assertBool "reconnect allocates a distinct binding" (binding2 /= binding1)
  assertEqual
    "closing delivery preserves the pending semantic Start"
    1
    ( length
        ( AdministrationState.configuredStartEntries
            (startupAdministrationState reconnected)
        )
    )
  let beforeStale = administrationWitness reconnected
  (stale, staleEffects) <-
    stepAt
      10
      (AdministrationInput (GetAdministrationResult binding1 fixtureCorrelation))
      reconnected
  assertEqual
    "superseded binding is rejected"
    [ RejectAdministrationConnection
        (EstablishedAdministrationDisposition binding1)
    ]
    (effectBatchMembers staleEffects)
  assertEqual
    "stale binding mutates no administration fact"
    beforeStale
    (administrationWitness stale)

  let replacementCorrelation = Administration.adminCorrelationIdForBinding binding2 71
  (requeried, reconnectQueryEffects) <-
    stepAt
      11
      (AdministrationInput (GetAdministrationResult binding2 replacementCorrelation))
      stale
  assertEqual "new operator cannot query the old delivery lifetime" [SendAdministrationReply binding2 (Administration.AbsentAdminResult replacementCorrelation)] (effectBatchMembers reconnectQueryEffects)
  (lost, lossEffects) <-
    stepAt
      12
      (RuntimeObserved (AdministrationBindingLost binding2))
      requeried
  assertEqual "binding loss emits no semantic reply" [] (effectBatchMembers lossEffects)
  assertEqual
    "current external binding is cleared"
    Nothing
    ( AdministrationState.currentConfiguredAdministrationBinding
        (startupAdministrationState lost)
    )
  (_, afterLossEffects) <-
    stepAt
      13
      (AdministrationInput (GetAdministrationResult binding2 replacementCorrelation))
      lost
  assertEqual
    "lost binding cannot query retained state"
    [ RejectAdministrationConnection
        (EstablishedAdministrationDisposition binding2)
    ]
    (effectBatchMembers afterLossEffects)

caseEndRetainedLifecycle :: Assertion
caseEndRetainedLifecycle = do
  bound <- boundInitialHerald
  (opened, binding) <- openAt 2 20 bound
  let correlation = Administration.adminCorrelationIdForBinding binding 72
      request =
        Administration.endProcessEpochRequest
          correlation
          fixtureProcessEpoch
          ExplicitAdministrativeEnd
  (accepted, acceptedEffects) <-
    stepAt
      3
      (AdministrationInput (EndProcessEpoch binding request))
      opened
  assertAcceptedWithSubmit binding correlation acceptedEffects
  let acceptedAdministration = startupAdministrationState accepted
      acceptedOracle = startupOracleClientState accepted
  assertEqual
    "one accepted End record is retained"
    1
    (length (AdministrationState.endProcessEpochEntries acceptedAdministration))
  assertEqual
    "End consumes the shared administration acceptance ordinal once"
    2
    (AdministrationState.configuredStartNextAcceptanceOrdinal acceptedAdministration)
  assertEqual
    "End mints one Oracle request once"
    2
    (OracleClient.oracleClientNextRequestSequence acceptedOracle)

  (retried, retryEffects) <-
    stepAt
      4
      (AdministrationInput (EndProcessEpoch binding request))
      accepted
  assertEqual
    "exact End retry reoffers the same accepted reply and owner envelope"
    (effectBatchMembers acceptedEffects)
    (effectBatchMembers retryEffects)
  assertBool
    "exact End retry changes no administration fact"
    (acceptedAdministration == startupAdministrationState retried)
  assertEqual
    "exact End retry changes no Oracle fact"
    acceptedOracle
    (startupOracleClientState retried)

  let crossCommand =
        Administration.startProcessRequest correlation
  (conflicted, conflictEffects) <-
    stepAt
      5
      (AdministrationInput (StartProcessEpoch binding crossCommand))
      retried
  assertEqual
    "reusing an End correlation for Start is an immutable conflict"
    [ SendAdministrationReply
        binding
        (Administration.ConflictingAdminResult correlation)
    ]
    (effectBatchMembers conflictEffects)
  assertEqual
    "cross-command conflict changes no Oracle fact"
    acceptedOracle
    (startupOracleClientState conflicted)

  (_, queryEffects) <-
    stepAt
      6
      (AdministrationInput (GetAdministrationResult binding correlation))
      conflicted
  assertRetainedAccepted binding correlation queryEffects

caseObservationsDoNotSynthesizeRequests :: Assertion
caseObservationsDoNotSynthesizeRequests = do
  bound <- boundInitialHerald
  (afterPeer, peerEffects) <-
    stepAt
      3
      ( PeerInput
          ( PeerCandidateOpened
              (peerCandidateOpened (connectionNonce 81) Set.empty Nothing)
          )
      )
      bound
  assertNoOracleRequest "peer observation" afterPeer peerEffects

  binding <-
    maybe
      (assertFailure "fixture Oracle client is not bound")
      pure
      (OracleClient.oracleClientCurrentBinding (startupOracleClientState afterPeer))
  (afterLoss, lossEffects) <-
    stepAt
      4
      (OracleInput (OracleBindingLost binding))
      afterPeer
  assertNoOracleRequest "Oracle binding loss" afterLoss lossEffects
  retry <- case effectBatchMembers lossEffects of
    [RunOracleClientAction (ScheduleOracleRetry OracleConnectionRetry observed)] -> pure observed
    other -> assertFailure ("expected one retry after Oracle loss, got " <> show other)

  (afterTimer, timerEffects) <-
    stepAt
      5
      (OracleInput (OracleRetryElapsed retry))
      afterLoss
  assertNoOracleRequest "Oracle retry timer" afterTimer timerEffects

assertNoOracleRequest :: String -> HeraldState -> EffectBatch -> Assertion
assertNoOracleRequest context state effects = do
  assertEqual
    (context <> " retains no semantic Oracle request")
    []
    (OracleClient.oracleClientRequestEntries (startupOracleClientState state))
  assertBool
    (context <> " emits no Oracle submission")
    (all (not . isOracleSubmission) (effectBatchMembers effects))
  where
    isOracleSubmission (RunOracleClientAction (SubmitOracleRequest _ _)) = True
    isOracleSubmission _ = False

assertAcceptedWithSubmit ::
  Administration.AdministrationBinding ->
  Administration.AdminCorrelationId ->
  EffectBatch ->
  Assertion
assertAcceptedWithSubmit binding correlation effects =
  case effectBatchMembers effects of
    [ SendAdministrationReply
        observedBinding
        (Administration.RetainedAdminResult observedCorrelation Administration.AdminRequestAccepted),
      RunOracleClientAction (SubmitOracleRequest _ _)
      ] -> do
        assertEqual "accepted reply binding" binding observedBinding
        assertEqual "accepted reply correlation" correlation observedCorrelation
    other ->
      assertFailure
        ("expected Accepted plus one owner-derived Oracle submission, got " <> show other)

assertRetainedAccepted ::
  Administration.AdministrationBinding ->
  Administration.AdminCorrelationId ->
  EffectBatch ->
  Assertion
assertRetainedAccepted binding correlation effects =
  assertEqual
    "query returns the retained accepted status"
    [ SendAdministrationReply
        binding
        ( Administration.RetainedAdminResult
            correlation
            Administration.AdminRequestAccepted
        )
    ]
    (effectBatchMembers effects)

openAt ::
  Integer ->
  Integer ->
  HeraldState ->
  IO (HeraldState, Administration.AdministrationBinding)
openAt instant laneOrdinal predecessor = do
  let lane = candidateAdministrationLane (fromIntegral laneOrdinal)
  (successor, effects) <-
    stepAt
      instant
      (AdministrationInput (OpenAdministrationConnection lane fixtureSystem))
      predecessor
  case effectBatchMembers effects of
    [SetAdministrationConnectionDisposition observedLane binding]
      | observedLane == lane -> pure (successor, binding)
    other -> assertFailure ("expected one administration acceptance, got " <> show other)

stepAt ::
  Integer ->
  HeraldInputBody ->
  HeraldState ->
  IO (HeraldState, EffectBatch)
stepAt instant body =
  checkedIO
    . verifiedStepHerald
      (heraldInput (monotonicInstant (fromIntegral instant)) body)

boundInitialHerald :: IO HeraldState
boundInitialHerald = do
  (initial, _) <-
    checkedIO
      ( initialHerald
          (monotonicInstant 0)
          fixtureCheckedGenesis
          fixtureInitialBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )
  let client = startupOracleClientState initial
      attempt = case OracleClient.oracleClientActions client of
        [ConnectAndHelloOracle observed _] -> observed
        other -> error ("expected one initial Oracle connect, got " <> show other)
      node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          node
          (oracleObservedTerm 1)
          (checkedOracleControlIndex fixtureCheckedGenesis)
          (Just node)
          True
  fst
    <$> stepAt
      1
      (OracleInput (OracleHelloReceived attempt acceptance))
      initial

administrationWitness :: HeraldState -> AdministrationState.AdministrationStateWitness
administrationWitness =
  AdministrationState.administrationStateWitness
    . startupAdministrationState

fixtureInitialBootstraps :: CheckedInitialBootstraps
fixtureInitialBootstraps =
  checked
    ( checkInitialBootstraps
        fixtureCheckedGenesis
        (PrimordialProcessManifest [])
    )

fixtureSystem :: SystemId
fixtureSystem = checkedSystemId fixtureCheckedGenesis

foreignSystem :: SystemId
foreignSystem = checked (mkSystemId (fixtureIdentifierBytes 0xee))

fixtureProcessEpoch :: ProcessEpochId
fixtureProcessEpoch = checked (mkProcessEpochId (fixtureIdentifierBytes 0xa7))

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

checkedIO :: (Show problem) => Either problem value -> IO value
checkedIO = either (assertFailure . show) pure
