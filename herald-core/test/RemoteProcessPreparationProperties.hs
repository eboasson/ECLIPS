{-# LANGUAGE OverloadedStrings #-}

module RemoteProcessPreparationProperties (tests) where

import ApplicationLabelProperties (commitNextLabelRequest)
import ConfiguredProcessProperties (bindInitial, bound, commitEntry, step, submitted)
import Control.Monad (foldM)
import Data.ByteString qualified as Bytes
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Private
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToVoid))
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.Operation (ApplicationOperation (LabelApplication))
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Domain.Identity
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart
import Eclips.Domain.Startup
import Eclips.Herald.Administration qualified as Admin
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
import Eclips.Herald.Genesis (DeploymentManifest (..), checkHeraldGenesis)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.IdGenerator.State qualified as Generator
import Eclips.Herald.Initialization (initialHeraldWithIsolation)
import Eclips.Herald.Input
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.ProcessPreparation.Protocol
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.State
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer.Internal (TimerOutcome (TimerFired))
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue)
import Eclips.Oracle.Projection (appliedEntryControlIndex)
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import GenesisFixtures qualified as Fixtures
import OracleAdvanceProperties (dynamicStartRetirementFixture, fixtureFailureHeraldGenesis)
import ProcessPreparationProperties qualified as Local
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "remote prepared-child lifecycle"
    [ testCase "exact handoffs allocate once and wait for the reverse canonical process-name grant" caseHandoff,
      testCase "cancellation overtaking the offer seals without allocating a child" caseOvertakingCancel,
      testCase "pending Start cancellation retains one ordered End through lost replies" casePendingCancel,
      testCase "accepted selected grant survives parent End before target delivery" caseParentEnd,
      testCase "target retains an offer until its exact control prerequisite arrives" caseControlPrerequisite,
      testCase "new peer bindings reoffer the same handoff after a lost offer" caseReconnect,
      testCase "new peer bindings recover a lost Prepared reply and query a known child exactly" casePreparedReconnect,
      testCase "target process End settles a waiting parent with a typed terminal result" caseTargetEnd,
      testCase "target Herald retirement terminates an accepted remote wait" caseTargetRetirement,
      testCase "later cancellation preserves a target self-fence failure" caseCancellationAfterSelfFence,
      testCase "a follower observes a canonical founder root label before any preparation transfer" caseGenesisLabelObservation
    ]

data Pair = Pair Local.Fixture HeraldState OracleBinding Discovery.PeerBinding Discovery.PeerBinding
localLocator, remoteLocator :: HeraldLocator
localLocator = checked (heraldLocator "localhost" 35001)
remoteLocator = checked (heraldLocator "localhost" 35002)
pair :: IO Pair
pair = pairUsing (\genesis -> bound genesis Fixtures.fixtureGeneratorSeedH2)
pairUsing :: (Genesis.CheckedHeraldGenesis -> IO (HeraldState, OracleBinding, Admin.AdministrationBinding)) -> IO Pair
pairUsing initialize = do
  local@(Local.Fixture source _ _ _ _ _) <- Local.fixture
  let base = Fixtures.fixtureDeploymentAt Fixtures.fixtureRemoteMember
      members = Genesis.checkedActiveHeralds (startupGenesis source)
  genesis <- checkedIO (checkHeraldGenesis base {deploymentActiveHeralds = members, deploymentOracleGenesis = (deploymentOracleGenesis base) {oracleGenesisActiveHeralds = members}})
  (target, oracle, _) <- initialize genesis
  (connectedSource, connectedTarget, sourceBinding, targetBinding) <- connect 600 source target
  pure (Pair (replaceFixture connectedSource local) connectedTarget oracle sourceBinding targetBinding)

caseCancellationAfterSelfFence :: Assertion
caseCancellationAfterSelfFence = do
  value@(Pair fixture target _ sourcePeer targetPeer) <- pairUsing initialize
  (accepted, offer, reference) <- begin value Local.emptySelection
  (targetPending, _) <- deliver targetPeer offer target
  (cancelling, cancellation, _) <- cancelSource fixture reference accepted
  (targetCancelling, _) <- deliver targetPeer cancellation targetPending
  let witness = Isolation.stateWitness (startupIsolationState targetCancelling)
  timer <- require "target isolation timer" (Isolation.isolationWitnessCurrentTimer witness)
  deadline <- require "target isolation deadline" (Isolation.isolationWitnessCurrentDeadline witness)
  (targetFenced, failureReply) <- checkedIO (verifiedStepHerald (heraldInput deadline (RuntimeObserved (TimerObserved timer (TimerFired deadline)))) targetCancelling)
  assertBool "self-fence leaves canonical target membership active" (Projection.oracleViewLocalHeraldIsCurrent (Projection.oracleView (startupOracleProjectionState targetFenced)))
  (failed, _) <- deliver sourcePeer failureReply cancelling
  let expected = LifecycleRejected (LifecycleStartupFailed StartupNotLive)
  assertEqual "uncertain cancellation reports target failure" expected =<< Local.status fixture 2 failed
  (later, laterEffects) <- Local.call fixture 3 (CancelChild reference) failed
  assertEqual "fresh cancellation retains terminal failure" expected =<< Local.status fixture 3 later
  assertEqual "terminal failure does not reopen peer cleanup" [] (remoteControls laterEffects)
  (_, retryEffects) <- Local.call fixture 3 (CancelChild reference) later
  assertEqual "exact cancellation retry remains local" [] (remoteControls retryEffects)
  where
    initialize genesis = do
      configuration <- checkedIO (checkIsolationConfiguration 1000 1000)
      let voters = Set.delete (Genesis.checkedLocalHeraldEpoch genesis) (Set.fromList (Genesis.checkedActiveHeraldEpochs genesis))
      (initial, _) <- checkedIO (initialHeraldWithIsolation (monotonicInstant 0) genesis (Fixtures.fixtureCheckedInitialBootstrapsFor genesis) Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH2 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration voters configuration)
      bindInitial genesis initial
replaceFixture :: HeraldState -> Local.Fixture -> Local.Fixture
replaceFixture state (Local.Fixture _ oracle binding session attachment parent) = Local.Fixture state oracle binding session attachment parent
connect :: Word64 -> HeraldState -> HeraldState -> IO (HeraldState, HeraldState, Discovery.PeerBinding, Discovery.PeerBinding)
connect = connectAt remoteLocator

connectAt :: HeraldLocator -> Word64 -> HeraldState -> HeraldState -> IO (HeraldState, HeraldState, Discovery.PeerBinding, Discovery.PeerBinding)
connectAt targetLocator nonce source target = do
  (offered, offerEffects) <- step (PeerInput (PeerCandidateOpened (Discovery.peerCandidateOpened (Discovery.connectionNonce nonce) Set.empty (Just localLocator)))) source
  (candidate, hello) <- one "source Hello" [(candidate, hello) | SendPeerCandidate candidate _ _ hello <- effectBatchMembers offerEffects]
  (acceptedTarget, targetEffects) <- step (PeerInput (helloAt targetLocator target candidate hello)) target
  (responseCandidate, responseHello) <- one "target Hello" [(remoteCandidate, remoteHello) | SendPeerCandidate remoteCandidate _ _ remoteHello <- effectBatchMembers targetEffects]
  (acceptedSource, sourceEffects) <- step (PeerInput (helloAt localLocator offered responseCandidate responseHello)) offered
  sourceBinding <- require "source peer binding" (Discovery.currentPeerBinding (Genesis.checkedLocalHeraldEpoch (startupGenesis target)) (startupDiscoveryState acceptedSource))
  targetBinding <- require "target peer binding" (Discovery.currentPeerBinding (Genesis.checkedLocalHeraldEpoch (startupGenesis source)) (startupDiscoveryState acceptedTarget))
  -- Physical snapshots carried by the ordinary Hello response remain ordinary
  -- peer controls; preparation transfer never copies another Herald's owner.
  placedSource <- relayPlacements sourceBinding targetEffects acceptedSource
  placedTarget <- relayPlacements targetBinding sourceEffects acceptedTarget
  pure (placedSource, placedTarget, sourceBinding, targetBinding)
  where
    helloAt locator state candidate hello = case Fixtures.currentPeerHelloReceived state candidate Set.empty hello of
      PeerHelloReceived a b c d e _ -> PeerHelloReceived a b c d e (Just locator)
      _ -> error "fixture Hello constructor"
    relayPlacements binding effects initial = foldM (\state (progress, control) -> fst <$> step (PeerInput (PeerControlReceivedWithProgress binding progress control)) state) initial [(progress, control) | (progress, control@PeerPlacementUpdate {}) <- peerControlMessages effects]

begin :: Pair -> Access.PrimordialSelection -> IO (HeraldState, EffectBatch, ChildPreparation)
begin (Pair fixture@(Local.Fixture source _ _ _ _ _) _ _ _ _) selection = do
  (accepted, effects) <- Local.call fixture 1 (BeginChild remoteLocator selection) source
  status <- Local.status fixture 1 accepted
  reference <- case status of LifecycleCompleted (ChildPreparationAccepted ref) -> pure ref; other -> assertFailure (show other)
  pure (accepted, effects, reference)
remoteControls :: EffectBatch -> [PreparationControl]
remoteControls = map snd . remoteControlMessages

remoteControlMessages :: EffectBatch -> [(ReceiptRetirement, PreparationControl)]
remoteControlMessages effects = [(progress, control) | (progress, PeerPreparationControl control) <- peerControlMessages effects]

peerControlMessages :: EffectBatch -> [(ReceiptRetirement, PeerControl)]
peerControlMessages = concatMap message . effectBatchMembers
  where
    message (SendPeerControl _ control) = [(mempty, control)]
    message (SendPeerControlWithProgress _ progress control) = [(progress, control)]
    message _ = []
deliver :: Discovery.PeerBinding -> EffectBatch -> HeraldState -> IO (HeraldState, EffectBatch)
deliver binding effects initial = foldM apply (initial, mempty) (remoteControlMessages effects)
  where
    apply (state, emitted) (progress, control) = do
      Local.assertPreparationWorkReference state
      (next, current) <- step (PeerInput (PeerControlReceivedWithProgress binding progress (PeerPreparationControl control))) state
      Local.assertPreparationWorkReference next
      pure (next, emitted <> current)

caseHandoff :: Assertion
caseHandoff = do
  value@(Pair fixture@(Local.Fixture source sourceOracle _ _ _ parent) target targetOracle sourcePeer targetPeer) <- pair
  (accepted, offer, reference) <- begin value Local.emptySelection
  assertEqual "source generates no child identities" (counter source) (counter accepted)
  (targetAccepted, targetEffects) <- deliver targetPeer offer target
  assertEqual "target allocates exactly process and epoch" (counter target + 2) (counter targetAccepted)
  (retriedTarget, retryEffects) <- deliver targetPeer offer targetAccepted
  assertEqual "reoffer retains one identity" (counter targetAccepted) (counter retriedTarget)
  assertEqual "reoffer submits no second Start" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers retryEffects]
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  envelope <- submitted targetEffects
  (_, entry) <- commitEntry oracle envelope
  (preparedTarget, reply) <- step (OracleInput (OracleEntriesReceived targetOracle (entry :| []))) retriedTarget
  child <- require "target preparation" (Preparation.lookupPreparation reference (startupProcessPreparationState preparedTarget))
  let process = processStartProcessEpochId (Preparation.preparationStart child)
  assertBool "target namespace installed before reply" (isJust (Application.applicationBootstrapAccess process (startupApplicationState preparedTarget)))
  assertEqual "remote parent has no target private alias" Nothing (Preparation.preparationResult child)
  assertEqual "minimal target creates no roots" (Controlled.controlledRootFacts (startupControlledState target)) (Controlled.controlledRootFacts (startupControlledState preparedTarget))
  (aheadReply, _) <- deliver sourcePeer reply accepted
  (waiting, _) <- Local.call fixture 2 (AwaitPreparedChild reference) aheadReply
  assertEqual "reply cannot fabricate a canonical process fact" (LifecyclePending []) =<< Local.status fixture 2 waiting
  (sourceApplied, _) <- step (OracleInput (OracleEntriesReceived sourceOracle (entry :| []))) waiting
  result <- Local.status fixture 2 sourceApplied
  prepared <- case result of LifecycleCompleted (ChildPrepared childValue) -> pure childValue; other -> assertFailure (show other)
  assertEqual "target descriptor carries target location" remoteLocator (connectionDescriptorLocator (preparedChildConnection prepared))
  assertBool "parent has ordinary process-name grant" (either (const False) (const True) (Controlled.checkControlledGrant parent (globalObjectIdFromProcessEpochId process) (startupControlledState sourceApplied)))
  (ready, _) <- Local.call fixture 3 (AwaitChildReady reference) sourceApplied
  assertEqual "empty selected child is ready" (LifecycleCompleted ChildReady) =<< Local.status fixture 3 ready
  let claim = checked (initialClaimId (Bytes.replicate 32 0x91))
  (claimed, _) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 991) (preparedChildConnection prepared) claim (Just remoteLocator))) preparedTarget
  assertEqual "claim uses the same target preparation" (Just Preparation.Attached) (Preparation.preparationPhase <$> Preparation.lookupPreparation reference (startupProcessPreparationState claimed))

caseOvertakingCancel :: Assertion
caseOvertakingCancel = do
  value@(Pair fixture target _ sourcePeer targetPeer) <- pair
  (accepted, offer, reference) <- begin value Local.emptySelection
  (cancelling, cancel, _) <- cancelSource fixture reference accepted
  (sealedTarget, cancelledReply) <- deliver targetPeer cancel target
  assertEqual "overtaking cancellation consumes no identities" (counter target) (counter sealedTarget)
  (lateTarget, _) <- deliver targetPeer offer sealedTarget
  assertEqual "late offer cannot create a child" [] (Preparation.preparationEntries (startupProcessPreparationState lateTarget))
  (completed, _) <- deliver sourcePeer cancelledReply cancelling
  assertEqual "source observes exact cancellation" (LifecycleCompleted ChildCancelled) =<< Local.status fixture 2 completed
  (repeated, _) <- Local.call fixture 2 (CancelChild reference) completed
  assertEqual "exact request replay retains cancellation" (LifecycleCompleted ChildCancelled) =<< Local.status fixture 2 repeated

casePendingCancel :: Assertion
casePendingCancel = do
  value@(Pair fixture@(Local.Fixture _ sourceOracle _ _ _ _) target targetOracle sourcePeer targetPeer) <- pair
  (accepted, offer, reference) <- begin value Local.emptySelection
  (targetPending, startEffects) <- deliver targetPeer offer target
  (cancelling, cancel, _) <- cancelSource fixture reference accepted
  (targetSealed, _) <- deliver targetPeer cancel targetPending
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  start <- submitted startEffects
  (oracleStarted, startEntry) <- commitEntry oracle start
  (targetStarted, endEffects) <- step (OracleInput (OracleEntriesReceived targetOracle (startEntry :| []))) targetSealed
  process <- Local.childProcess reference targetStarted
  assertEqual "cancelled-before-install child has no application access" Nothing (Application.applicationBootstrapAccess process (startupApplicationState targetStarted))
  ending <- submitted endEffects
  (_, endEntry) <- commitEntry oracleStarted ending
  (targetEnded, lostReply) <- step (OracleInput (OracleEntriesReceived targetOracle (endEntry :| []))) targetStarted
  assertBool "ordered End reply exists" (not (null (remoteControls lostReply)))
  -- Drop the first terminal reply, then query the retained exact target record.
  (_, replay) <- step (PeerInput (PeerControlReceived targetPeer (PeerPreparationControl (PreparationQueried reference)))) targetEnded
  (sourceProjected, _) <- step (OracleInput (OracleEntriesReceived sourceOracle (startEntry :| [endEntry]))) cancelling
  (completed, _) <- deliver sourcePeer replay sourceProjected
  assertEqual "query recovers ordered cancellation" (LifecycleCompleted ChildCancelled) =<< Local.status fixture 2 completed
  assertEqual "one Start and one End remain retained" 2 (length (Client.oracleClientRequestEntries (startupOracleClientState targetEnded)))

caseParentEnd :: Assertion
caseParentEnd = do
  value@(Pair fixture@(Local.Fixture source sourceOracle _ _ _ parent) target targetOracle _ targetPeer) <- pair
  access <- require "parent access" (Application.applicationBootstrapAccess parent (startupApplicationState source))
  writer <- require "selected writer" (Map.lookup (Access.environmentWriterKey Access.NeutralVertexRole) (Access.accessEntries (Application.bootstrapAccessPrimordial access)))
  selection <- checkedIO (Access.primordialSelection [("writer", writer)] Set.empty Nothing)
  (accepted, offer, reference) <- begin value selection
  (ending, endEffects) <- Local.call fixture 2 EndOwnProcess accepted
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  endEnvelope <- submitted endEffects
  (oracleEnded, endEntry) <- commitEntry oracle endEnvelope
  (sourceEnded, _) <- step (OracleInput (OracleEntriesReceived sourceOracle (endEntry :| []))) ending
  (targetObserved, _) <- step (OracleInput (OracleEntriesReceived targetOracle (endEntry :| []))) target
  assertEqual "parent private namespace is gone" Nothing (Application.applicationBootstrapAccess parent (startupApplicationState sourceEnded))
  (targetPending, startEffects) <- deliver targetPeer offer targetObserved
  startEnvelope <- submitted startEffects
  (_, startEntry) <- commitEntry oracleEnded startEnvelope
  (targetPrepared, _) <- step (OracleInput (OracleEntriesReceived targetOracle (startEntry :| []))) targetPending
  process <- Local.childProcess reference targetPrepared
  childAccess <- require "child installed despite parent End" (Application.applicationBootstrapAccess process (startupApplicationState targetPrepared))
  assertEqual "immutable selected namespace survived transport" ["writer"] (Map.keys (Access.accessEntries (Application.bootstrapAccessPrimordial childAccess)))
  assertBool "child stays live independently" (Projection.oracleViewProcessIsLive process (Projection.oracleView (startupOracleProjectionState targetPrepared)))

caseGenesisLabelObservation :: Assertion
caseGenesisLabelObservation = do
  value@(Pair fixture@(Local.Fixture source sourceOracle binding session _ parent) target targetOracle _ targetPeer) <- pair
  access <- require "founder access" (Application.applicationBootstrapAccess parent (startupApplicationState source))
  writer <- case Map.lookup (Access.environmentWriterKey Access.NeutralVertexRole) (Access.accessEntries (Application.bootstrapAccessPrimordial access)) of
    Just (Access.Writer writerValue) -> pure writerValue
    _ -> assertFailure "founder writer"
  identity <- checkedIO (Application.resolveApplicationPrivateUniqueId parent (Private.privateNablaUniqueId writer) (startupApplicationState source))
  let object = globalObjectIdFromGlobalUniqueId identity
      operation = LabelApplication (Private.asPrivateObjectId (Private.privateNablaUniqueId writer)) ((ApplicationValue.ProcessLabel (Application.bootstrapAccessProcess access), 0)) LabelToVoid
      ingress = ApplicationRequestInput (CallApplicationRequest binding session (Request.requestId 1) operation)
  (accepted, _) <- step ingress source
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (openedOracle, opened, decisionEffects) <- commitNextLabelRequest 10 oracle accepted
  openedEntry <- require "canonical Open" (Projection.appliedEntryEvidence (controlIndex 1) (startupOracleProjectionState opened))
  let thirdBase = Fixtures.fixtureDeploymentAt Fixtures.fixtureStep14ThirdMember
      members = Genesis.checkedActiveHeralds (startupGenesis source)
  thirdGenesis <- checkedIO (checkHeraldGenesis thirdBase {deploymentActiveHeralds = members, deploymentOracleGenesis = (deploymentOracleGenesis thirdBase) {oracleGenesisActiveHeralds = members}})
  (third, thirdOracle, _) <- bound thirdGenesis Fixtures.fixtureGeneratorSeedH3
  (openedTarget, targetEffects) <- step (OracleInput (OracleEntriesReceived targetOracle (openedEntry :| []))) target
  (ready, _) <- step ingress opened
  thirdLocator <- checkedIO (heraldLocator "localhost" 35003)
  (connectedSource, connectedThird, _, _) <- connectAt thirdLocator 610 ready third
  (openedThird, thirdEffects) <- step (OracleInput (OracleEntriesReceived thirdOracle (openedEntry :| []))) connectedThird
  completed <- finishLabel openedOracle [(sourceOracle, connectedSource), (targetOracle, openedTarget), (thirdOracle, openedThird)] (installations opened decisionEffects <> installations openedTarget targetEffects <> installations openedThird thirdEffects)
  (releasedSource, observedTarget) <- case completed of
    [(_, sourceState), (_, targetState), _] -> pure (sourceState, targetState)
    _ -> assertFailure "three-Herald label trace"
  assertBool "all retained label intentions have settled" (all (null . Client.oracleClientLabelRequestActions . startupOracleClientState . snd) completed)
  assertBool "source label completes through real owner reports" (Application.applicationActiveLabel (startupApplicationState releasedSource) == Nothing)
  assertEqual "observer learns only the one canonical root" (length (Controlled.controlledRootFacts (startupControlledState target)) + 1) (length (Controlled.controlledRootFacts (startupControlledState observedTarget)))
  assertEqual "global label release retained before grant" (Controlled.controlledReleasedLabelRecord object (startupControlledState releasedSource)) (Controlled.controlledReleasedLabelRecord object (startupControlledState observedTarget))
  assertBool "label metadata observation does not grant parent possession" (not (Controlled.controlledHasDirectPossession parent object (startupControlledState observedTarget)))
  selection <- checkedIO (Access.primordialSelection [("writer", Access.Writer writer)] Set.empty Nothing)
  let Pair _ _ _ sourcePeer _ = value
  (_, offer, _) <- begin (Pair (replaceFixture releasedSource fixture) observedTarget targetOracle sourcePeer targetPeer) selection
  (offeredTarget, _) <- deliver targetPeer offer observedTarget
  assertEqual "later transfer preserves released label overlay" (Controlled.controlledReleasedLabelRecord object (startupControlledState observedTarget)) (Controlled.controlledReleasedLabelRecord object (startupControlledState offeredTarget))
  where
    -- The closed schedule carries both canonical entries and actual direct
    -- installation controls. Absence of Oracle requests alone is not quiescence.
    finishLabel oracle states [] = case [oracleRequestDispatchEnvelope dispatch | (_, state) <- states, SubmitOracleRequest _ dispatch <- Client.oracleClientLabelRequestActions (startupOracleClientState state)] of
      [] -> pure states
      envelope : _ -> do
        (nextOracle, entry) <- commitEntry oracle envelope
        projected <- traverse (applyEntry entry) states
        finishLabel nextOracle (map fst projected) (concatMap snd projected)
    finishLabel oracle states (message : rest) = do
      delivered <- traverse (applyInstallation message) states
      finishLabel oracle (map fst delivered) (rest <> concatMap snd delivered)
    applyEntry entry (binding, state) = case verifiedStepHerald (heraldInput (startupLastObservedTime state) (OracleInput (OracleEntriesReceived binding (entry :| [])))) state of
      Left failure -> assertFailure ("label entry " <> show (appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)) <> " at " <> show (Genesis.checkedLocalHeraldEpoch (startupGenesis state)) <> ": " <> show failure)
      Right (next, effects) -> pure ((binding, next), installations next effects)
    installations state effects = concatMap message (effectBatchMembers effects)
      where
        origin = Genesis.checkedLocalHeraldEpoch (startupGenesis state)
        message (SendPeerControl binding control@PeerLabelInstalled {}) = [(origin, Discovery.peerBindingRemoteHeraldEpoch binding, mempty, control)]
        message (SendPeerControlWithProgress binding progress control@PeerLabelInstalled {}) = [(origin, Discovery.peerBindingRemoteHeraldEpoch binding, progress, control)]
        message _ = []
    applyInstallation (origin, destination, progress, control) retained@(oracleBinding, state)
      | destination /= Genesis.checkedLocalHeraldEpoch (startupGenesis state) = pure (retained, [])
      | otherwise = do
          binding <- require "installation receiver's current peer binding" (Discovery.currentPeerBinding origin (startupDiscoveryState state))
          (next, effects) <- step (PeerInput (PeerControlReceivedWithProgress binding progress control)) state
          assertEqual "the checked installation control is accepted" [] [rejected | RejectPeerConnection rejected _ <- effectBatchMembers effects]
          pure ((oracleBinding, next), installations next effects)

caseTargetRetirement :: Assertion
caseTargetRetirement = do
  let (targetGenesis, _, _, retirementEntries) = dynamicStartRetirementFixture
  (initialSource, sourceOracle, _) <- bound fixtureFailureHeraldGenesis Fixtures.fixtureGeneratorSeedH1
  fixture <- Local.openFixture initialSource sourceOracle
  (initialTarget, targetOracle, _) <- bound targetGenesis Fixtures.fixtureGeneratorSeedH2
  let Local.Fixture openedSource _ _ _ _ _ = fixture
  (source, target, sourcePeer, targetPeer) <- connect 800 openedSource initialTarget
  let value = Pair (replaceFixture source fixture) target targetOracle sourcePeer targetPeer
  (accepted, _, reference) <- begin value Local.emptySelection
  (waiting, _) <- Local.call fixture 2 (AwaitPreparedChild reference) accepted
  assertEqual "parent retains remote wait" (LifecyclePending []) =<< Local.status fixture 2 waiting
  (retired, _) <- step (OracleInput (OracleEntriesReceived sourceOracle retirementEntries)) waiting
  assertEqual "canonical target retirement rejects retained wait" (LifecycleRejected (LifecycleStartupFailed StartupNotLive)) =<< Local.status fixture 2 retired
  assertEqual "target absence created no local replacement child" [] (Preparation.preparationEntries (startupProcessPreparationState retired))

caseTargetEnd :: Assertion
caseTargetEnd = do
  value@(Pair fixture@(Local.Fixture source sourceOracle _ _ _ parent) target targetOracle sourcePeer targetPeer) <- pair
  access <- require "parent access" (Application.applicationBootstrapAccess parent (startupApplicationState source))
  writer <- require "selected writer" (Map.lookup (Access.environmentWriterKey Access.NeutralVertexRole) (Access.accessEntries (Application.bootstrapAccessPrimordial access)))
  selection <- checkedIO (Access.primordialSelection [("writer", writer)] (Set.singleton "writer") Nothing)
  (accepted, offer, reference) <- begin value selection
  (targetPending, startEffects) <- deliver targetPeer offer target
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  envelope <- submitted startEffects
  (oracleStarted, startEntry) <- commitEntry oracle envelope
  (targetPrepared, preparedReply) <- step (OracleInput (OracleEntriesReceived targetOracle (startEntry :| []))) targetPending
  preparation <- require "selected target preparation" (Preparation.lookupPreparation reference (startupProcessPreparationState targetPrepared))
  selectedObject <- one "one selected writer" [globalObjectIdFromGlobalUniqueId (Primordial.globalPrimordialIdentity entry) | entry <- Map.elems (Primordial.primordialGrantEntries (Preparation.preparationGrant preparation))]
  assertEqual "target observes exactly one existing genesis root" (length (Controlled.controlledRootFacts (startupControlledState target)) + 1) (length (Controlled.controlledRootFacts (startupControlledState targetPrepared)))
  assertBool "observing source metadata grants no source possession" (not (Controlled.controlledHasDirectPossession parent selectedObject (startupControlledState targetPrepared)))
  (sourceObserved, _) <- step (OracleInput (OracleEntriesReceived sourceOracle (startEntry :| []))) accepted
  (sourcePrepared, _) <- deliver sourcePeer preparedReply sourceObserved
  (waiting, _) <- Local.call fixture 2 (AwaitChildReady reference) sourcePrepared
  assertEqual "parent waits for required writer ownership" (LifecyclePending ["writer"]) =<< Local.status fixture 2 waiting
  process <- Local.childProcess reference targetPrepared
  admin <- require "target administration binding" (Administration.administrationWitnessConfiguredBinding (Administration.administrationStateWitness (startupAdministrationState targetPrepared)))
  let ending = Admin.endProcessEpochRequest (Admin.adminCorrelationIdForBinding admin 900) process ExplicitAdministrativeEnd
  (targetEnding, endEffects) <- step (AdministrationInput (EndProcessEpoch admin ending)) targetPrepared
  endEnvelope <- submitted endEffects
  (_, endEntry) <- commitEntry oracleStarted endEnvelope
  (targetEnded, terminalReply) <- step (OracleInput (OracleEntriesReceived targetOracle (endEntry :| []))) targetEnding
  assertEqual "target preparation records End" (Just Preparation.Terminal) (Preparation.preparationPhase <$> Preparation.lookupPreparation reference (startupProcessPreparationState targetEnded))
  assertBool ("target emits typed End notification: " <> show (remoteControls terminalReply)) (PreparationReplied reference (RemotePreparationFailed StartupNotLive) `elem` remoteControls terminalReply)
  (notified, _) <- deliver sourcePeer terminalReply waiting
  assertEqual "remote End rejects the retained readiness wait" (LifecycleRejected (LifecycleStartupFailed StartupNotLive)) =<< Local.status fixture 2 notified
  assertEqual "target namespace retired" Nothing (Application.applicationBootstrapAccess process (startupApplicationState targetEnded))

caseControlPrerequisite :: Assertion
caseControlPrerequisite = do
  Pair fixture@(Local.Fixture source sourceOracle _ _ _ _) target targetOracle sourcePeer targetPeer <- pair
  (localPending, localStartEffects) <- Local.call fixture 50 (BeginChild localLocator Local.emptySelection) source
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  envelope <- submitted localStartEffects
  (_, entry) <- commitEntry oracle envelope
  (advancedSource, _) <- step (OracleInput (OracleEntriesReceived sourceOracle (entry :| []))) localPending
  (accepted, offer, reference) <- begin (Pair (replaceFixture advancedSource fixture) target targetOracle sourcePeer targetPeer) Local.emptySelection
  assertEqual "accepted request retained" 1 (length (Preparation.outgoingEntries (startupProcessPreparationState accepted)))
  (waiting, pendingReply) <- deliver targetPeer offer target
  assertEqual "missing prerequisite allocates no identities" (counter target) (counter waiting)
  assertEqual "waiting offer is retained" 1 (length (Preparation.incomingEntries (startupProcessPreparationState waiting)))
  assertEqual "missing prerequisite creates no target preparation" Nothing (Preparation.lookupPreparation reference (startupProcessPreparationState waiting))
  assertBool "target reports pending" (PreparationReplied reference (RemotePreparationPending []) `elem` remoteControls pendingReply)
  (readyToStart, startEffects) <- step (OracleInput (OracleEntriesReceived targetOracle (entry :| []))) waiting
  assertEqual "ordinary control arrival admits one child" (counter target + 2) (counter readyToStart)
  _ <- submitted startEffects
  pure ()

caseReconnect :: Assertion
caseReconnect = do
  value@(Pair fixture@(Local.Fixture _ _ _ _ _ _) target _ sourcePeer targetPeer) <- pair
  (accepted, firstOffer, reference) <- begin value Local.emptySelection
  (newSource, newTarget, _, targetBinding, replay) <- reconnect 700 accepted target sourcePeer targetPeer
  assertEqual "new binding resends exact transfer bytes" (remoteControls firstOffer) (remoteControls replay)
  (received, _) <- deliver targetBinding replay newTarget
  assertEqual "reoffered handoff creates one target preparation" [reference] (map fst (Preparation.preparationEntries (startupProcessPreparationState received)))
  assertEqual "source retains original accepted result" (LifecycleCompleted (ChildPreparationAccepted reference)) =<< Local.status fixture 1 newSource

casePreparedReconnect :: Assertion
casePreparedReconnect = do
  value@(Pair fixture@(Local.Fixture _ sourceOracle _ _ _ _) target targetOracle sourcePeer targetPeer) <- pair
  (accepted, firstOffer, reference) <- begin value Local.emptySelection
  (targetPending, startEffects) <- deliver targetPeer firstOffer target
  envelope <- submitted startEffects
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (_, entry) <- commitEntry oracle envelope
  (targetPrepared, lostReply) <- step (OracleInput (OracleEntriesReceived targetOracle (entry :| []))) targetPending
  assertBool "first Prepared reply is emitted before losing the lane" (any isPrepared (remoteControls lostReply))
  (sourceObserved, _) <- step (OracleInput (OracleEntriesReceived sourceOracle (entry :| []))) accepted
  assertEqual "lost reply leaves source without a child identity" Nothing =<< retainedChild reference sourceObserved
  (sourceReconnected, targetReconnected, sourceBinding, targetBinding, reoffer) <- reconnect 710 sourceObserved targetPrepared sourcePeer targetPeer
  assertEqual "unknown child reoffers the exact accepted transfer" (remoteControls firstOffer) (remoteControls reoffer)
  (targetRecovered, preparedReply) <- deliver targetBinding reoffer targetReconnected
  assertSameTarget targetPrepared targetRecovered
  (sourceRecovered, _) <- deliver sourceBinding preparedReply sourceReconnected
  result <- retainedResult reference sourceRecovered
  (queriedSource, _) <- Local.call fixture 2 (AwaitPreparedChild reference) sourceRecovered
  assertEqual "recovered Prepared reply localizes the child handle" (LifecycleCompleted (ChildPrepared result)) =<< Local.status fixture 2 queriedSource
  let privateIdentity = Application.applicationPrivateIdentity (startupApplicationState queriedSource)
  (knownSource, knownTarget, knownSourceBinding, knownTargetBinding, query) <- reconnect 720 queriedSource targetRecovered sourceBinding targetBinding
  assertEqual "known child uses retained status query" [PreparationQueried reference] (remoteControls query)
  (targetQueried, exactReply) <- deliver knownTargetBinding query knownTarget
  assertSameTarget targetPrepared targetQueried
  (sourceQueried, _) <- deliver knownSourceBinding exactReply knownSource
  assertEqual "known-child query retains exact parent handle" result =<< retainedResult reference sourceQueried
  assertBool "known-child query allocates no second private alias" (Application.applicationPrivateIdentity (startupApplicationState sourceQueried) == privateIdentity)
  (newWait, _) <- Local.call fixture 3 (AwaitPreparedChild reference) sourceQueried
  assertEqual "fresh wait receives the original child handle" (LifecycleCompleted (ChildPrepared result)) =<< Local.status fixture 3 newWait
  where
    isPrepared (PreparationReplied _ RemotePreparationAvailable {}) = True
    isPrepared _ = False
    retainedChild reference state = Preparation.outgoingChild <$> require "source preparation" (Preparation.lookupOutgoing reference (startupProcessPreparationState state))
    retainedResult reference state = require "source child handle" . Preparation.outgoingResult =<< require "source preparation" (Preparation.lookupOutgoing reference (startupProcessPreparationState state))
    assertSameTarget before after = do
      assertEqual "reconnect preserves target generator" (counter before) (counter after)
      assertEqual "reconnect preserves the exact target preparation" (Preparation.preparationEntries (startupProcessPreparationState before)) (Preparation.preparationEntries (startupProcessPreparationState after))
      assertEqual "reconnect retains exactly one Oracle Start" (Client.oracleClientRequestEntries (startupOracleClientState before)) (Client.oracleClientRequestEntries (startupOracleClientState after))

reconnect :: Word64 -> HeraldState -> HeraldState -> Discovery.PeerBinding -> Discovery.PeerBinding -> IO (HeraldState, HeraldState, Discovery.PeerBinding, Discovery.PeerBinding, EffectBatch)
reconnect nonce source target sourcePeer targetPeer = do
  (lostSource, _) <- step (RuntimeObserved (PeerBindingLost sourcePeer)) source
  (lostTarget, _) <- step (RuntimeObserved (PeerBindingLost targetPeer)) target
  -- This candidate has a new binding generation. The driver must resend the
  -- exact retained offer, without allocating a second source preparation.
  (offered, effects) <- step (PeerInput (PeerCandidateOpened (Discovery.peerCandidateOpened (Discovery.connectionNonce nonce) Set.empty (Just localLocator)))) lostSource
  (candidate, hello) <- one "reconnect Hello" [(candidate, hello) | SendPeerCandidate candidate _ _ hello <- effectBatchMembers effects]
  let withLocator locator state candidateValue helloValue = case Fixtures.currentPeerHelloReceived state candidateValue Set.empty helloValue of
        PeerHelloReceived a b c d e _ -> PeerHelloReceived a b c d e (Just locator)
        _ -> error "fixture Hello"
  (newTarget, targetEffects) <- step (PeerInput (withLocator remoteLocator lostTarget candidate hello)) lostTarget
  (responseCandidate, response) <- one "reconnect response" [(candidateValue, helloValue) | SendPeerCandidate candidateValue _ _ helloValue <- effectBatchMembers targetEffects]
  (newSource, replay) <- step (PeerInput (withLocator localLocator offered responseCandidate response)) offered
  sourceBinding <- require "reconnected source binding" (Discovery.currentPeerBinding (Genesis.checkedLocalHeraldEpoch (startupGenesis newTarget)) (startupDiscoveryState newSource))
  targetBinding <- require "reconnected target binding" (Discovery.currentPeerBinding (Genesis.checkedLocalHeraldEpoch (startupGenesis newSource)) (startupDiscoveryState newTarget))
  pure (newSource, newTarget, sourceBinding, targetBinding, replay)

cancelSource :: Local.Fixture -> ChildPreparation -> HeraldState -> IO (HeraldState, EffectBatch, LifecycleStatus)
cancelSource fixture reference state = do
  (next, effects) <- Local.call fixture 2 (CancelChild reference) state
  status <- Local.status fixture 2 next
  pure (next, effects, status)
counter :: HeraldState -> Word64
counter = Generator.witnessedNextGeneratorCounter . Generator.idGeneratorStateWitness . startupIdGeneratorState
one :: String -> [value] -> IO value
one _ [value] = pure value
one label _ = assertFailure label
require :: String -> Maybe value -> IO value
require label = maybe (assertFailure label) pure
checkedIO :: (Show problem) => Either problem value -> IO value
checkedIO = either (assertFailure . show) pure
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
