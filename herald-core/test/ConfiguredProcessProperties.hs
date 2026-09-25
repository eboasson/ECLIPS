{-# LANGUAGE OverloadedStrings #-}

module ConfiguredProcessProperties (tests, bound, bindInitial, step, submitted, commitEntry) where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as Bytes
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Domain.Identity (ProcessEpochId, controlIndex, globalObjectIdFromProcessEpochId, heraldEpochBytes, processEpochIdBytes, systemIdBytes)
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart qualified as ProcessStart
import Eclips.Domain.Startup (heraldMemberEpoch, heraldMemberId)
import Eclips.Herald.Administration qualified as Admin
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.ConfiguredProcess.Start qualified as Start
import Eclips.Herald.ConfiguredProcess.State qualified as Configured
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.EffectBatch (ApplicationDispositionTarget (..), EffectBatch, HeraldEffect (..), effectBatchMembers)
import Eclips.Herald.Genesis (DeploymentManifest (..), OracleGenesisManifest (..), checkHeraldGenesis)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.IdGenerator.Internal (GeneratorSeed)
import Eclips.Herald.IdGenerator.State qualified as Generator
import Eclips.Herald.Initialization (initialHerald, initialHeraldWithIsolation)
import Eclips.Herald.Input
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.State
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer (TimerOutcome (TimerFired))
import Eclips.Herald.UseCase.Administration qualified as AdministrationUseCase
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command qualified as OracleCommand
import Eclips.Oracle.Effect (OracleEffect (EmitAppliedOracleEntry), OracleStepOutcome (OracleCommitted, OracleRequestRetired), oracleEffects)
import Eclips.Oracle.Receipt (OracleRequestRetirement (OracleRequestHomeRetired))
import Eclips.Oracle.State qualified as Oracle
import Eclips.Oracle.Transition
import Eclips.Public.Types.ReceiptRetirement (receiptRetirementPrefix)
import GenesisFixtures qualified as Fixtures
import OracleAdvanceProperties (dynamicStartRetirementFixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "dynamic logical process Start"
    [ QC.testProperty "membership admission holds administrative Starts without semantic acceptance" propMembershipStartGate,
      QC.testProperty "fresh local correlations generate distinct identities and exact retries consume none" propFreshAndRetry,
      testCase "two starts apply at one active Herald with only normal self possession" caseSameHerald,
      testCase "independent active Herald generators admit starts without future manifests" caseDifferentHeralds,
      testCase "Start then End revokes the new process and retains the completed Start identity" caseStartEnd,
      testCase "End consumes configured process provenance after explicit retirement or loss of the Start caller" caseStartRetiredThenEnd,
      testCase "local self-fencing retires dynamic readiness before Oracle End, with or without a session" caseLocalSelfFence,
      QC.testProperty "fenced read-only and terminal candidates receive exact dispositions without semantic reactivation" (QC.withNumTests 20 propFencedCandidateDispositions),
      testCase "pending Start after resident retirement is obsolete and retains one local identity" caseRetirementFencesPendingStart,
      testCase "End after applied Start but before attachment prevents later readiness" caseEndBeforeAttachment
    ]

propMembershipStartGate :: Word64 -> QC.Property
propMembershipStartGate ordinal = QC.ioProperty $ do
  let genesis = Fixtures.fixtureStep14CheckedGenesis
  (initial, _, binding) <- bound genesis Fixtures.fixtureGeneratorSeedH1
  let request = Admin.startProcessRequest (Admin.adminCorrelationIdForBinding binding ordinal)
  prepared <- checked (Administration.prepareGatedStartProcessEpoch genesis request (startupAdministrationState initial))
  let (pendingOwner, outcome) = Administration.commitStartProcessEpoch prepared
      pending = replaceStartupAdministrationState pendingOwner (replaceStartupApplicationState (Application.setApplicationMembershipGate True (startupApplicationState initial)) initial)
  retried <- checked (Administration.prepareGatedStartProcessEpoch genesis request pendingOwner)
  let (retriedOwner, retryOutcome) = Administration.commitStartProcessEpoch retried
  (held, heldEffects) <- checked (AdministrationUseCase.advanceGatedStarts pending)
  (opened, _) <- checked (AdministrationUseCase.advanceGatedStarts (replaceStartupApplicationState (Application.setApplicationMembershipGate False (startupApplicationState held)) held))
  (replayed, replayEffects) <- checked (AdministrationUseCase.advanceGatedStarts opened)
  pure
    $ QC.counterexample "gated administrative Start consumed identities, lost correlation, or allocated twice"
    $ outcome == Administration.ConfiguredStartFirst Administration.ConfiguredStartGated
      && retryOutcome == Administration.ConfiguredStartExactRetry Administration.ConfiguredStartGated
      && retriedOwner == pendingOwner
      && Administration.configuredStartNextAcceptanceOrdinal pendingOwner == Administration.configuredStartNextAcceptanceOrdinal (startupAdministrationState initial)
      && counter held == counter initial
      && startupOracleClientState held == startupOracleClientState initial
      && null (effectBatchMembers heldEffects)
      && counter opened == counter initial + 2
      && Administration.configuredStartNextAcceptanceOrdinal (startupAdministrationState opened) == Administration.configuredStartNextAcceptanceOrdinal pendingOwner + 1
      && counter replayed == counter opened
      && startupOracleClientState replayed == startupOracleClientState opened
      && null (effectBatchMembers replayEffects)

propFreshAndRetry :: QC.Positive Int -> QC.Property
propFreshAndRetry (QC.Positive n) = QC.ioProperty $ do
  let count = 1 + n `mod` 6
  (initial, _, binding) <- bound Fixtures.fixtureStep14CheckedGenesis Fixtures.fixtureGeneratorSeedH1
  final <- foldM (admitAndRetry binding) initial [1 .. fromIntegral count]
  let witnesses = map snd (Client.oracleClientRequestEntries (startupOracleClientState final))
      starts = [start | witness <- witnesses, Just start <- [Client.oracleRequestWitnessBootstrap witness]]
      epochs = map ProcessStart.processStartProcessEpochId starts
      ids = map ProcessStart.processStartProcessId starts
  pure
    ( length starts == count
        && Set.size (Set.fromList epochs) == count
        && Set.size (Set.fromList ids) == count
        && counter final == counter initial + fromIntegral (2 * count)
    )
  where
    admitAndRetry binding state ordinal = do
      let request = Admin.startProcessRequest (Admin.adminCorrelationIdForBinding binding ordinal)
      (accepted, effects) <- step (AdministrationInput (StartProcessEpoch binding request)) state
      (retried, retryEffects) <- step (AdministrationInput (StartProcessEpoch binding request)) accepted
      assertEqual "exact retry consumes no generator entries" (counter accepted) (counter retried)
      assertEqual "exact retry repeats exact retained canonical submission" (effectBatchMembers effects) (effectBatchMembers retryEffects)
      assertEqual "exact retry retains one Oracle intention" (startupOracleClientState accepted) (startupOracleClientState retried)
      pure retried

caseSameHerald :: Assertion
caseSameHerald = do
  (initial, oracleBinding, adminBinding) <- bound Fixtures.fixtureStep14CheckedGenesis Fixtures.fixtureGeneratorSeedH1
  oracle <- checked (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (one, oracle1, firstEntry, process1) <- startAndApply 100 oracleBinding adminBinding initial oracle
  (two, _, _, process2) <- startAndApply 101 oracleBinding adminBinding one oracle1
  assertBool "each accepted call owns a different process" (process1 /= process2)
  assertEqual "two dynamic starts consume four generated identities" (counter initial + 4) (counter two)
  assertEqual "no root authority was synthesized" (Controlled.controlledRootFacts (startupControlledState initial)) (Controlled.controlledRootFacts (startupControlledState two))
  assertEqual "no structural publication stages were minted" (Publication.unstampedStructuralSourceStageEntries (startupPublicationState initial)) (Publication.unstampedStructuralSourceStageEntries (startupPublicationState two))
  assertInstalled process1 two
  assertInstalled process2 two
  let correlation = Admin.adminCorrelationIdForBinding adminBinding 100
  intention <- case Administration.lookupConfiguredStartStatus correlation (startupAdministrationState two) of
    Just (Administration.ConfiguredStartReady retained _ _ _) -> pure retained
    other -> assertFailure ("expected retained first Start identity, got " <> show other)
  assertEqual "later unrelated control preserves exact local Start result lookup" (Just firstEntry) (Client.oracleClientRequestEvidence (Administration.startProcessIntentionOracleRequest intention) (startupOracleClientState two))
  (replayed, _) <- step (OracleInput (OracleEntriesReceived oracleBinding (firstEntry :| []))) two
  assertBool "old Start replay preserves the complete later owner product" (replayed == two)

caseDifferentHeralds :: Assertion
caseDifferentHeralds = do
  let localGenesis = Fixtures.fixtureStep14CheckedGenesis
      base = Fixtures.fixtureDeploymentAt Fixtures.fixtureRemoteMember
      members = Genesis.checkedActiveHeralds localGenesis
  remoteGenesis <-
    checked
      ( checkHeraldGenesis
          base
            { deploymentActiveHeralds = members,
              deploymentOracleGenesis = (deploymentOracleGenesis base) {oracleGenesisActiveHeralds = members}
            }
      )
  (local, localOracleBinding, localAdminBinding) <- bound localGenesis Fixtures.fixtureGeneratorSeedH1
  (remote, remoteOracleBinding, remoteAdminBinding) <- bound remoteGenesis Fixtures.fixtureGeneratorSeedH2
  oracle <- checked (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (localStarted, oracle1, entry1, process1) <- startAndApply 200 localOracleBinding localAdminBinding local oracle
  (remoteAdvanced, _) <- step (OracleInput (OracleEntriesReceived remoteOracleBinding (entry1 :| []))) remote
  (remoteStarted, _, entry2, process2) <- startAndApply 200 remoteOracleBinding remoteAdminBinding remoteAdvanced oracle1
  (localAdvanced, _) <- step (OracleInput (OracleEntriesReceived localOracleBinding (entry2 :| []))) localStarted
  assertBool "target-local same correlation names independent generated identities" (process1 /= process2)
  assertInstalled process1 localAdvanced
  assertInstalled process2 remoteStarted
  assertEqual "local residence is retained" (Just (Genesis.checkedLocalHeraldEpoch localGenesis)) (Projection.oracleViewProcessResidence process1 (Projection.oracleView (startupOracleProjectionState remoteStarted)))
  assertEqual "remote residence is retained" (Just (Genesis.checkedLocalHeraldEpoch remoteGenesis)) (Projection.oracleViewProcessResidence process2 (Projection.oracleView (startupOracleProjectionState localAdvanced)))

caseStartEnd :: Assertion
caseStartEnd = do
  (initial, oracleBinding, adminBinding) <- bound Fixtures.fixtureStep14CheckedGenesis Fixtures.fixtureGeneratorSeedH1
  oracle <- checked (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (started, oracle1, _, process) <- startAndApply 300 oracleBinding adminBinding initial oracle
  let request = Admin.endProcessEpochRequest (Admin.adminCorrelationIdForBinding adminBinding 301) process ExplicitAdministrativeEnd
  (accepted, effects) <- step (AdministrationInput (EndProcessEpoch adminBinding request)) started
  envelope <- submitted effects
  (oracle2, entry) <- commitEntry oracle1 envelope
  let _ = oracle2
  (ended, _) <- step (OracleInput (OracleEntriesReceived oracleBinding (entry :| []))) accepted
  assertBool "End revokes normal self possession" (not (Controlled.controlledHasNormalPossession process (globalObjectIdFromProcessEpochId process) (startupControlledState ended)))
  assertEqual "End records the exact epoch" (Just (Admin.AdminEndRequestCompleted process)) (Administration.lookupAdministrationResultStatus (Admin.adminCorrelationIdForBinding adminBinding 301) (startupAdministrationState ended))
  (retried, _) <- step (AdministrationInput (StartProcessEpoch adminBinding (Admin.startProcessRequest (Admin.adminCorrelationIdForBinding adminBinding 300)))) ended
  assertEqual "Start retry after End never allocates a second epoch" (counter ended) (counter retried)
  assertEqual "Start scaffold retains one identity" 1 (length (Configured.configuredProcessScaffoldEntries (startupConfiguredProcessState retried)))

caseStartRetiredThenEnd :: Assertion
caseStartRetiredThenEnd = forM_ [False, True] $ \loseConnection -> do
  let genesis = Fixtures.fixtureStep14CheckedGenesis
  (initial, oracleBinding, startBinding) <- bound genesis Fixtures.fixtureGeneratorSeedH1
  oracle <- checked (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (started, oracle1, startEntry, process) <- startAndApply 300 oracleBinding startBinding initial oracle
  (released, endBinding) <-
    if loseConnection
      then do
        (lost, _) <- step (RuntimeObserved (AdministrationBindingLost startBinding)) started
        (opened, _) <- step (AdministrationInput (OpenAdministrationConnection (candidateAdministrationLane 2) (Genesis.checkedSystemId genesis))) lost
        binding <- maybe (assertFailure "replacement administration binding missing") pure (Administration.currentConfiguredAdministrationBinding (startupAdministrationState opened))
        pure (opened, binding)
      else do
        (retired, _) <- step (AdministrationInput (RetireAdministrationReceipts startBinding (receiptRetirementPrefix (Just 300)) Nothing)) started
        pure (retired, startBinding)
  assertEqual "the original Start RPC result is released" Nothing (Administration.lookupAdministrationResultStatus (Admin.adminCorrelationIdForBinding startBinding 300) (startupAdministrationState released))
  assertEqual "the active Start RPC table is empty" [] (Administration.configuredStartEntries (startupAdministrationState released))
  assertEqual "one semantic configured process origin remains" 1 (length (Administration.configuredStartProcessOrigins process (startupAdministrationState released)))
  (startReplayed, _) <- step (OracleInput (OracleEntriesReceived oracleBinding (startEntry :| []))) released
  assertBool "replaying Start cannot restore its retired RPC result" (startupAdministrationState startReplayed == startupAdministrationState released)
  let endCorrelation = Admin.adminCorrelationIdForBinding endBinding 301
      request = Admin.endProcessEpochRequest endCorrelation process ExplicitAdministrativeEnd
  (accepted, effects) <- step (AdministrationInput (EndProcessEpoch endBinding request)) startReplayed
  envelope <- submitted effects
  (_, endEntry) <- commitEntry oracle1 envelope
  (ended, _) <- step (OracleInput (OracleEntriesReceived oracleBinding (endEntry :| []))) accepted
  assertEqual "End completes in the current caller's scope" (Just (Admin.AdminEndRequestCompleted process)) (Administration.lookupAdministrationResultStatus endCorrelation (startupAdministrationState ended))
  assertBool "End revokes normal self possession" (not (Controlled.controlledHasNormalPossession process (globalObjectIdFromProcessEpochId process) (startupControlledState ended)))
  assertEqual "End releases dynamic application access" Nothing (Application.applicationBootstrapAccess process (startupApplicationState ended))
  (replayed, _) <- step (OracleInput (OracleEntriesReceived oracleBinding (endEntry :| []))) ended
  assertBool "End replay preserves the settled owners" (startupAdministrationState replayed == startupAdministrationState ended && startupConfiguredProcessState replayed == startupConfiguredProcessState ended)
  assertEqual "retired Start never returns to the RPC table" [] (Administration.configuredStartEntries (startupAdministrationState replayed))

caseLocalSelfFence :: Assertion
caseLocalSelfFence = forM_ [False, True] $ \hasSession -> do
  (started, process, _, fenced) <- selfFenceFixture hasSession
  terminal <-
    if hasSession
      then do
        assertEqual
          "an attached application receives its bounded read-only drain"
          Isolation.IsolationReadOnlyDrainView
          (Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState fenced)))
        assertBool
          "the dynamic access remains until the read-only drain ends"
          (Application.applicationBootstrapAccess process (startupApplicationState fenced) /= Nothing)
        fst <$> expireIsolationTimer fenced
      else pure fenced
  assertEqual
    "self-fencing reaches the irreversible terminal phase"
    Isolation.IsolationTerminalView
    (Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState terminal)))
  assertBool
    "local isolation keeps the canonical Oracle process projection unchanged"
    (startupOracleProjectionState started == startupOracleProjectionState terminal)
  assertBool
    "Oracle still retains the live dynamic epoch"
    (Projection.oracleViewProcessIsLive process (Projection.oracleView (startupOracleProjectionState terminal)))
  assertEqual
    "terminal local isolation removes the dynamic private access"
    Nothing
    (Application.applicationBootstrapAccess process (startupApplicationState terminal))
  assertEqual
    "terminal local isolation removes attachment authorization"
    Nothing
    (Application.applicationAttachmentProcess (Session.applicationAttachmentForProcess process) (startupApplicationState terminal))
  adminBinding <- maybe (assertFailure "self-fence fixture administration binding missing") pure (Administration.currentConfiguredAdministrationBinding (startupAdministrationState started))
  let correlation = Admin.adminCorrelationIdForBinding adminBinding 500
  assertEqual
    "completed Start keeps its one generated identity across local isolation"
    (Administration.lookupAdministrationResultStatus correlation (startupAdministrationState started))
    (Administration.lookupAdministrationResultStatus correlation (startupAdministrationState terminal))

selfFenceFixture :: Bool -> IO (HeraldState, ProcessEpochId, HeraldState, HeraldState)
selfFenceFixture hasSession = do
  let genesis = Fixtures.fixtureStep14CheckedGenesis
      local = Genesis.checkedLocalHeraldEpoch genesis
      voters = Set.delete local (Set.fromList (Genesis.checkedActiveHeraldEpochs genesis))
  isolation <- checked (checkIsolationConfiguration 1000 1000)
  (initial, _) <-
    checked
      ( initialHeraldWithIsolation
          (monotonicInstant 0)
          genesis
          (Fixtures.fixtureCheckedInitialBootstrapsFor genesis)
          Fixtures.fixtureOracleContacts
          Fixtures.fixtureGeneratorSeedH1
          Fixtures.fixtureApplicationRecoveryConfiguration
          Fixtures.fixturePeerRecoveryConfiguration
          voters
          isolation
      )
  (ready, oracleBinding, adminBinding) <- bindInitial genesis initial
  oracle <- checked (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (started, _, _, process) <- startAndApply 500 oracleBinding adminBinding ready oracle
  withSession <-
    if hasSession
      then
        fst
          <$> step
            ( ApplicationSessionInput
                ( OpenApplicationSession
                    (candidateApplicationLane 501)
                    (Session.applicationAttachmentForProcess process)
                    (Session.clientNonce 501)
                )
            )
            started
      else pure started
  (fenced, _) <- expireIsolationTimer withSession
  pure (started, process, withSession, fenced)

expireIsolationTimer :: HeraldState -> IO (HeraldState, EffectBatch)
expireIsolationTimer state = do
  let witness = Isolation.stateWitness (startupIsolationState state)
  attempt <- maybe (assertFailure "isolation has no retained timer") pure (Isolation.isolationWitnessCurrentTimer witness)
  deadline <- maybe (assertFailure "isolation has no retained deadline") pure (Isolation.isolationWitnessCurrentDeadline witness)
  checked (verifiedStepHerald (heraldInput deadline (RuntimeObserved (TimerObserved attempt (TimerFired deadline)))) state)

-- Match every input family that pauses a runtime source for a disposition.
-- Both states come from actual grace/drain expiry with a live application, so
-- the test cannot bypass the local self-fence or retire its semantic owners by
-- constructing an isolated state directly.
propFencedCandidateDispositions :: Word64 -> QC.Property
propFencedCandidateDispositions generated = QC.ioProperty $ do
  (_, process, attached, fenced) <- selfFenceFixture True
  (terminal, _) <- expireIsolationTimer fenced
  sessionWitness <- case Application.applicationWitnessSessions (Application.applicationStateWitness (startupApplicationState attached)) of
    [witness] -> pure witness
    _ -> assertFailure "self-fence fixture requires one live application session"
  let genesis = startupGenesis attached
      ordinal = 1000 + generated `mod` 10000
      lane = candidateApplicationLane ordinal
      adminLane = candidateAdministrationLane ordinal
      attachment = Session.applicationAttachmentForProcess process
      session = Application.applicationSessionWitnessId sessionWitness
      token = Application.applicationSessionWitnessResumeToken sessionWitness
      remote = Fixtures.fixtureRemoteMember
      nonce = Discovery.connectionNonce ordinal
      peer = Discovery.peerCandidate (heraldMemberId remote) (heraldMemberEpoch remote) nonce
      hello =
        Discovery.peerHello
          (Genesis.checkedSystemId genesis)
          (heraldMemberId remote)
          (heraldMemberEpoch remote)
          nonce
          Set.empty
          (Genesis.checkedOracleControlIndex genesis)
          (Genesis.checkedCatalogueDigest genesis)
          (Genesis.checkedInitialProjectionDigest (Fixtures.fixtureCheckedInitialBootstrapsFor genesis))
          Nothing
      rejectedApplication = RejectApplicationConnection (CandidateApplicationDisposition lane) Session.ApplicationSessionNotLive
  cursor <- checked (Request.applicationReplyCursorFromClaimWord64 1)
  locator <- checked (Lifecycle.heraldLocator "localhost" 35001)
  descriptor <- checked (Lifecycle.connectionDescriptor locator (systemIdBytes (Genesis.checkedSystemId genesis)) (heraldEpochBytes (Genesis.checkedLocalHeraldEpoch genesis)) (processEpochIdBytes process))
  claim <- checked (Lifecycle.initialClaimId (Bytes.replicate 32 0x84))
  request <- checked (Lifecycle.lifecycleRequestId (heraldEpochBytes (Session.applicationSessionIdHeraldEpoch session)) (Session.applicationSessionIdOrdinal session) ordinal)
  forM_ [(Isolation.IsolationReadOnlyDrainView, fenced), (Isolation.IsolationTerminalView, terminal)] $ \(expectedPhase, state) -> do
    assertEqual "fixture reaches the real isolation phase" expectedPhase (Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState state)))
    let candidates =
          [ ("open application", ApplicationSessionInput (OpenApplicationSession lane attachment (Session.clientNonce ordinal)), (== rejectedApplication)),
            ("resume application", ApplicationSessionInput (ResumeApplicationSession lane session token cursor), (== rejectedApplication)),
            ("initial application claim", ApplicationSessionInput (ClaimInitialApplicationSession lane descriptor claim Nothing), (== RejectInitialClaim lane claim Lifecycle.StartupNotLive)),
            ("restricted lifecycle recovery", ApplicationLifecycleInput (RecoverApplicationLifecycleResult lane attachment request), (== SendApplicationLifecycleReply (CandidateApplicationDisposition lane) (Lifecycle.LifecycleAbsent request))),
            ("operator connection", AdministrationInput (OpenAdministrationConnection adminLane (Genesis.checkedSystemId genesis)), \case SetAdministrationConnectionDisposition observed _ -> observed == adminLane; _ -> False),
            ("local peer candidate", PeerInput (PeerCandidateOpened (Discovery.peerCandidateOpened nonce Set.empty Nothing)), (== RejectPeerCandidateOpened nonce)),
            ("remote peer Hello", PeerInput (Fixtures.currentPeerHelloReceived state peer Set.empty hello), (== SetPeerCandidateDisposition peer (Discovery.PeerHelloRejected Discovery.HelloInactiveHerald)))
          ]
    forM_ candidates $ \(label, body, matches) -> do
      (successor, effects) <- step body state
      case effectBatchMembers effects of
        [effect] -> assertBool (label <> " receives its exact candidate disposition") (matches effect)
        other -> assertFailure (label <> " lacks one exact disposition: " <> show other)
      assertBool (label <> " preserves semantic application ownership") (startupApplicationState state == startupApplicationState successor)
      assertBool (label <> " preserves semantic control") (startupOracleProjectionState state == startupOracleProjectionState successor)
      assertBool (label <> " preserves controlled graph ownership") (startupControlledState state == startupControlledState successor)
      assertBool (label <> " cannot establish a peer binding") (startupDiscoveryState state == startupDiscoveryState successor)
      assertBool (label <> " cannot submit Oracle work") (startupOracleClientState state == startupOracleClientState successor)
      assertBool (label <> " cannot reopen the fenced epoch") (startupIsolationState state == startupIsolationState successor)
      assertEqual (label <> " cannot allocate semantic identities") (counter state) (counter successor)
  pure True

caseRetirementFencesPendingStart :: Assertion
caseRetirementFencesPendingStart = do
  let (genesis, bootstraps, retiredOracle, retirementEntries) = dynamicStartRetirementFixture
  (initial, _) <-
    checked
      ( initialHerald
          (monotonicInstant 0)
          genesis
          bootstraps
          Fixtures.fixtureOracleContacts
          Fixtures.fixtureGeneratorSeedH3
          Fixtures.fixtureApplicationRecoveryConfiguration
          Fixtures.fixturePeerRecoveryConfiguration
      )
  (ready, oracleBinding, adminBinding) <- bindInitial genesis initial
  let correlation = Admin.adminCorrelationIdForBinding adminBinding 600
      request = Admin.startProcessRequest correlation
  (pending, effects) <- step (AdministrationInput (StartProcessEpoch adminBinding request)) ready
  envelope <- submitted effects
  let supplied = canonicalOracleEnvelopeValue envelope
      oracleRequest = OracleCommand.oracleEnvelopeRequestId supplied
      retainedStatus = Administration.lookupAdministrationResultStatus correlation (startupAdministrationState pending)
  (sameOracle, outcome, oracleEffectsOnce) <- checked (stepOracle supplied retiredOracle)
  (replayedOracle, replayOutcome, oracleEffectsAgain) <- checked (stepOracle supplied sameOracle)
  assertEqual "closed-home Start has an explicit obsolete result" (OracleRequestRetired OracleRequestHomeRetired) outcome
  assertEqual "closed-home Start retry has the same result" outcome replayOutcome
  assertEqual "obsolete Start and replay cannot allocate Oracle history" retiredOracle replayedOracle
  assertEqual "obsolete Start emits no new control entry" [] (oracleEffects oracleEffectsOnce <> oracleEffects oracleEffectsAgain)
  assertEqual "query classifies the closed-home request as retired" (Just OracleRequestHomeRetired) (Oracle.oracleRequestRetirement oracleRequest sameOracle)
  assertEqual "closed-home request has no fabricated rejection receipt" Nothing (Oracle.oracleRequestReceipt oracleRequest sameOracle)
  (closed, _) <- step (OracleInput (OracleRequestRetiredReceived oracleBinding oracleRequest OracleRequestHomeRetired)) pending
  assertEqual "home-retired result stops all submissions while the watch catches up" [] (Client.oracleClientRequestActions (startupOracleClientState closed))
  assertEqual "home-retired result preserves the catch-up binding" (Just oracleBinding) (Client.oracleClientCurrentBinding (startupOracleClientState closed))
  (terminal, _) <- step (OracleInput (OracleEntriesReceived oracleBinding retirementEntries)) closed
  assertBool "committed membership retirement permanently fences the Oracle owner" (Client.oracleClientWitnessRetired (Client.oracleClientStateWitness (startupOracleClientState terminal)))
  assertEqual "terminal Oracle owner offers no work" [] (Client.oracleClientActions (startupOracleClientState terminal))
  assertEqual "retirement adds no Start scaffold" [] (Configured.configuredProcessScaffoldEntries (startupConfiguredProcessState terminal))
  assertEqual "local acceptance is retained without inventing an indexed Oracle result" retainedStatus (Administration.lookupAdministrationResultStatus correlation (startupAdministrationState terminal))
  assertEqual "projection retains no result for an uncommitted Start" Nothing (Projection.appliedEntryForRequest oracleRequest (startupOracleProjectionState terminal))
  assertEqual "one generated identity survives the retirement race" (counter pending) (counter terminal)
  (replayed, _) <- step (OracleInput (OracleEntriesReceived oracleBinding retirementEntries)) terminal
  assertBool "retirement watch replay preserves every terminal owner" (replayed == terminal)
  let owners =
        Start.configuredStartOwners
          (startupAdministrationState terminal)
          (startupOracleClientState terminal)
          (startupOracleProjectionState terminal)
          (startupConfiguredProcessState terminal)
          (startupIdGeneratorState terminal)
  retry <- checked (Start.prepareConfiguredStartRequest genesis request owners)
  let (retried, retryOutcome) = Start.commitConfiguredStartRequest retry
  case retryOutcome of
    Administration.ConfiguredStartExactRetry _ -> pure ()
    other -> assertFailure ("expected retained local Start identity, got " <> show other)
  assertBool "local retry cannot allocate another identity or reactivate the terminal owner" (retried == owners)

caseEndBeforeAttachment :: Assertion
caseEndBeforeAttachment = do
  let genesis = Fixtures.fixtureStep14CheckedGenesis
  (state, _, adminBinding) <- bound genesis Fixtures.fixtureGeneratorSeedH1
  let correlation = Admin.adminCorrelationIdForBinding adminBinding 400
  oracle <- checked (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  let owners =
        Start.configuredStartOwners
          (startupAdministrationState state)
          (startupOracleClientState state)
          (startupOracleProjectionState state)
          (startupConfiguredProcessState state)
          (startupIdGeneratorState state)
  prepared <- checked (Start.prepareConfiguredStartRequest genesis (Admin.startProcessRequest correlation) owners)
  let (accepted, _) = Start.commitConfiguredStartRequest prepared
  witness <- case Client.oracleClientRequestEntries (Start.configuredStartOracleClient accepted) of
    [(_, retained)] -> pure retained
    _ -> assertFailure "expected one owned Start request"
  (_, entry) <- commitEntry oracle (Client.oracleRequestWitnessEnvelope witness)
  projection <- Projection.commitAppliedEntry <$> checked (Projection.prepareAppliedEntry entry (Start.configuredStartOracleProjection accepted))
  client <- Client.commitCursorAdvance <$> checked (Client.prepareCursorAdvance entry (Start.configuredStartOracleClient accepted))
  let originalRequest = Client.oracleRequestWitnessRef witness
  assertEqual "unobserved Start has no canonical result" Nothing (Client.oracleClientRequestEvidence originalRequest (Start.configuredStartOracleClient accepted))
  assertEqual "cursor admission exposes the exact result before first settlement" (Just entry) (Client.oracleClientRequestEvidence originalRequest client)
  assertEqual "evidence observation does not prematurely settle the command" (Just Client.OracleRequestAwaitingProjection) (Client.oracleRequestWitnessStatus <$> Client.lookupOracleRequest originalRequest client)
  let projected =
        Start.configuredStartOwners
          (Start.configuredStartAdministration accepted)
          client
          projection
          (Start.configuredStartConfiguredProcesses accepted)
          (Start.configuredStartIdGenerator accepted)
  installed <- checked (Start.prepareConfiguredStartOracleResult genesis correlation projected)
  let (settled, outcome) = Start.commitConfiguredStartOracleResult installed
  assertEqual "first settlement preserves exact directly indexed evidence" (Just entry) (Client.oracleClientRequestEvidence originalRequest (Start.configuredStartOracleClient settled))
  reference <- case outcome of
    Start.ConfiguredStartProcessInstalled value -> pure value
    other -> assertFailure ("expected applied process scaffold, got " <> show other)
  let process = Configured.liveProcessScaffoldRefProcessEpoch reference
      endOwners =
        Start.configuredProcessEndOwners
          (Start.configuredStartAdministration settled)
          (Start.configuredStartConfiguredProcesses settled)
  preparedEnd <- checked (Start.prepareConfiguredStartProcessEnd process (controlIndex 2) ExplicitAdministrativeEnd endOwners)
  let (ended, endOutcome) = Start.commitConfiguredStartProcessEnd preparedEnd
  assertEqual "the accepted identity terminalizes before attachment" Start.ConfiguredStartEndedBeforeAttachment endOutcome
  readiness <- checked (Configured.prepareConfiguredAttachmentReady process (Start.configuredProcessEndConfiguredProcesses ended))
  assertEqual
    "late local installation cannot mark a dead epoch ready"
    (Configured.ConfiguredAttachmentReadySuppressedByEnd (controlIndex 2) ExplicitAdministrativeEnd)
    (Configured.preparedConfiguredAttachmentReadyClassification readiness)
  completion <-
    checked
      ( Administration.prepareConfiguredStartCompletion
          correlation
          reference
          (Session.applicationAttachmentForProcess process)
          (Start.configuredProcessEndAdministration ended)
      )
  case Administration.preparedConfiguredStartCompletionClassification completion of
    Administration.ConfiguredStartCompletionSuppressedByEnd _ -> pure ()
    other -> assertFailure ("expected End-suppressed result, got " <> show other)
  retiredAdministration <- checked (Administration.retireAdministrationReceipts adminBinding (receiptRetirementPrefix (Just 400)) (Start.configuredProcessEndAdministration ended))
  let retiredOwners = Start.configuredProcessEndOwners retiredAdministration (Start.configuredProcessEndConfiguredProcesses ended)
  assertEqual "the End-before-attachment RPC receipt is released" Nothing (Administration.lookupAdministrationResultStatus correlation retiredAdministration)
  repeatedEnd <- checked (Start.prepareConfiguredStartProcessEnd process (controlIndex 2) ExplicitAdministrativeEnd retiredOwners)
  let (replayed, replayOutcome) = Start.commitConfiguredStartProcessEnd repeatedEnd
  assertEqual "the configured owner still identifies the exact terminal settlement" Start.ConfiguredStartProcessEndExactRetry replayOutcome
  assertBool "End replay cannot recreate the released Start receipt" (replayed == retiredOwners)
  case Start.prepareConfiguredStartProcessEnd process (controlIndex 3) ExplicitAdministrativeEnd retiredOwners of
    Left (Start.ConfiguredStartScaffoldProblem Configured.ConfiguredScaffoldEndEvidenceConflict {}) -> pure ()
    Left problem -> assertFailure ("expected retained configured End evidence conflict, got " <> show problem)
    Right _ -> assertFailure "a released RPC receipt must not erase the configured owner's exact End evidence"

assertInstalled :: ProcessEpochId -> HeraldState -> Assertion
assertInstalled process state = do
  assertBool "applied Start installs the resident process" (Controlled.controlledProcessFact process (startupControlledState state) /= Nothing)
  assertBool "self object is normally possessed" (Controlled.controlledHasNormalPossession process (globalObjectIdFromProcessEpochId process) (startupControlledState state))
  access <- maybe (assertFailure "dynamic application bootstrap missing") pure (Application.applicationBootstrapAccess process (startupApplicationState state))
  assertEqual "dynamic startup selection is empty" Map.empty (Access.accessEntries (Application.bootstrapAccessPrimordial access))
  assertEqual "no hidden newenv source writers" Nothing (Application.bootstrapAccessEnvironmentSources access)

startAndApply :: Word64 -> OracleBinding -> Admin.AdministrationBinding -> HeraldState -> Oracle.OracleState -> IO (HeraldState, Oracle.OracleState, CanonicalAppliedOracleEntry, ProcessEpochId)
startAndApply ordinal oracleBinding adminBinding state oracle = do
  let correlation = Admin.adminCorrelationIdForBinding adminBinding ordinal
  (accepted, effects) <- step (AdministrationInput (StartProcessEpoch adminBinding (Admin.startProcessRequest correlation))) state
  envelope <- submitted effects
  (oracle1, entry) <- commitEntry oracle envelope
  (applied, _) <- step (OracleInput (OracleEntriesReceived oracleBinding (entry :| []))) accepted
  process <- case Administration.lookupAdministrationResultStatus correlation (startupAdministrationState applied) of
    Just (Admin.AdminStartRequestCompleted epoch _) -> pure epoch
    other -> assertFailure ("expected completed dynamic Start, got " <> show other)
  pure (applied, oracle1, entry, process)

submitted :: EffectBatch -> IO CanonicalOracleEnvelope
submitted effects = case [oracleRequestDispatchEnvelope dispatch | RunOracleClientAction (SubmitOracleRequest _ dispatch) <- effectBatchMembers effects] of
  [envelope] -> pure envelope
  other -> assertFailure ("expected one retained Oracle submission, got " <> show (length other))

commitEntry :: Oracle.OracleState -> CanonicalOracleEnvelope -> IO (Oracle.OracleState, CanonicalAppliedOracleEntry)
commitEntry oracle envelope = do
  result <- checked (stepOracle (canonicalOracleEnvelopeValue envelope) oracle)
  case result of
    (successor, OracleCommitted _, effects) -> case oracleEffects effects of
      [EmitAppliedOracleEntry entry] -> pure (successor, canonicalizeAppliedOracleEntry entry)
      other -> assertFailure ("expected one applied entry, got " <> show other)
    (_, outcome, _) -> assertFailure ("expected committed outcome, got " <> show outcome)

bound :: Genesis.CheckedHeraldGenesis -> GeneratorSeed -> IO (HeraldState, OracleBinding, Admin.AdministrationBinding)
bound genesis seed = do
  (initial, _) <- checked (initialHerald (monotonicInstant 0) genesis (Fixtures.fixtureCheckedInitialBootstrapsFor genesis) Fixtures.fixtureOracleContacts seed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration)
  bindInitial genesis initial

bindInitial :: Genesis.CheckedHeraldGenesis -> HeraldState -> IO (HeraldState, OracleBinding, Admin.AdministrationBinding)
bindInitial genesis initial = do
  attempt <- case Client.oracleClientActions (startupOracleClientState initial) of
    [ConnectAndHelloOracle value _] -> pure value
    other -> assertFailure ("expected one Oracle connect, got " <> show other)
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance = oracleHelloAcceptance node (oracleObservedTerm 1) (Genesis.checkedOracleControlIndex genesis) (Just node) True
  (connected, _) <- step (OracleInput (OracleHelloReceived attempt acceptance)) initial
  oracleBinding <- maybe (assertFailure "Oracle binding missing") pure (Client.oracleClientCurrentBinding (startupOracleClientState connected))
  (opened, _) <- step (AdministrationInput (OpenAdministrationConnection (candidateAdministrationLane 1) (Genesis.checkedSystemId genesis))) connected
  adminBinding <- maybe (assertFailure "admin binding missing") pure (Administration.currentConfiguredAdministrationBinding (startupAdministrationState opened))
  pure (opened, oracleBinding, adminBinding)

counter :: HeraldState -> Word64
counter = Generator.witnessedNextGeneratorCounter . Generator.idGeneratorStateWitness . startupIdGeneratorState

step :: HeraldInputBody -> HeraldState -> IO (HeraldState, EffectBatch)
step body state = checked (verifiedStepHerald (heraldInput (startupLastObservedTime state) body) state)

checked :: (Show problem) => Either problem value -> IO value
checked = either (assertFailure . show) pure
