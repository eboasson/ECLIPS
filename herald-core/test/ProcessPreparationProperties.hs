{-# LANGUAGE OverloadedStrings #-}

module ProcessPreparationProperties (tests, Fixture (..), fixture, openFixture, prepare, emptySelection, apply, begin, childProcess, call, status, request, assertPreparationWorkReference) where

import ApplicationLabelProperties (commitCompleteLabelWorkflow, commitNextLabelRequest, completeAssignedPeerPublications, settleStructuralPublication, settledDeltaControlledFixtureWithPendingWait)
import ConfiguredProcessProperties (bindInitial, bound, commitEntry, step, submitted)
import Control.Monad (foldM, forM_)
import Data.ByteString qualified as Bytes
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Private
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToProcess))
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.Operation (ApplicationOperation (LabelApplication, NewEnvironmentApplication))
import Eclips.Application.Types.Result (RegularCallResult (NewEnvironmentCompleted))
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Context qualified as Context
import Eclips.Domain.Identity
import Eclips.Domain.Label qualified as DomainLabel
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart
import Eclips.Domain.Value qualified as DomainValue
import Eclips.Herald.Alignment.Generation qualified as Generation
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.Operate qualified as Operate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectBatch
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Initialization (initialHerald, initialHeraldWithIsolation, initialHeraldWithTiming, primordialApplicationAttachment)
import Eclips.Herald.Input
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Placement qualified as PlacementProtocol
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.ProcessPreparation.Readiness qualified as Readiness
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.Structural.Debt (normalizeStructuralDebts)
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer (TimerOutcome (TimerFired))
import Eclips.Herald.UseCase.Alignment qualified as AlignmentUseCase
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ProcessPreparation qualified as Lifecycle
import Eclips.Oracle.Canonical (canonicalOracleEnvelopeValue, canonicalizeOracleEnvelope)
import Eclips.Oracle.Command qualified as OracleCommand
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.State qualified as Oracle
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Public.Types.Timing
import GenesisFixtures qualified as Fixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "local prepared-child lifecycle"
    [ testCase "prepared children inherit a nondefault takeover target" caseChildTiming,
      testCase "preparation completes before required writer ownership and installs exact grants" caseSelected,
      testCase "a child labels fresh newenv readers created through handed-off source writers" caseChildEnvironmentLabel,
      testCase "initial claim retries one session and cancellation cannot kill an attached child" caseClaim,
      testCase "cancellation before an unsent Start seals without Oracle submission" caseCancelUnsent,
      testCase "cancellation after submitted Start orders one End and never exposes child access" caseCancelPending,
      testCase "parent End preserves unclaimed child and emits its final result before receipt retirement" caseParentEnd,
      testCase "own-End waits for accepted environments and dispatches once after their final cut" caseOwnEndAfterEnvironment,
      testCase "forced End completes an own-End waiting on construction without another Oracle request" caseForcedEndDuringOwnEnd,
      testCase "prepared child own-End retires its session and releases its terminal receipt" caseChildOwnEnd,
      testCase "request correlation distinguishes sessions and exact retries generate no identities" caseSessionScope,
      testCase "invalid selections and other target locators reject before identity allocation" caseInvalid,
      testCase "membership gate retains BeginChild before selection and identity allocation" caseMembershipBeginGate,
      testCase "membership gate holds name-only first child attachment" caseMembershipClaimGate,
      testCase "preparation readiness work stays parked until a relevant gate change" caseSelectiveReadinessGate,
      testCase "successful genesis claims retire their pending work before the next drive" caseGenesisClaimWorkRetirement,
      testCase "Oracle Hello alone dispatches an accepted disconnected preparation" caseHelloDispatch,
      testCase "self-fence cannot acknowledge cancellation while submitted Start is unresolved" caseCancelSelfFence,
      QC.testProperty "required reader needs current owner-local alignment completion" (QC.ioProperty caseDeltaReadiness),
      testCase "cancellation after a partial transfer preserves the committed label and ordinary zombie outcome" casePartialTransferCancellation
    ]

data Fixture = Fixture HeraldState OracleBinding Session.ApplicationSessionBinding Session.ApplicationSessionId Session.ApplicationAttachment ProcessEpochId
fixture :: IO Fixture
fixture = do
  let genesis = Fixtures.fixtureStep14CheckedGenesis
  (state, oracleBinding, _) <- bound genesis Fixtures.fixtureGeneratorSeedH1
  openFixture state oracleBinding

openFixture :: HeraldState -> OracleBinding -> IO Fixture
openFixture state oracleBinding = do
  let genesis = startupGenesis state
  bootstrap <- case Fixtures.fixtureLocalBootstrapIds of first : _ -> pure first; _ -> assertFailure "missing founder"
  attachment <- maybe (assertFailure "founder attachment") pure (primordialApplicationAttachment genesis (Fixtures.fixtureCheckedInitialBootstrapsFor genesis) bootstrap)
  (opened, effects) <- step (ApplicationSessionInput (OpenApplicationSession (candidateApplicationLane 10) attachment (Session.clientNonce 10))) state
  (binding, session) <- openedSession effects
  process <- maybe (assertFailure "founder process") pure (Application.applicationSessionProcess session (startupApplicationState opened))
  pure (Fixture opened oracleBinding binding session attachment process)

locator :: HeraldLocator
locator = checked (heraldLocator "localhost" 35001)
emptySelection :: Access.PrimordialSelection
emptySelection = checked (Access.primordialSelection [] Set.empty Nothing)
request :: Session.ApplicationSessionId -> Word64 -> LifecycleRequestId
request session ordinal = checked (lifecycleRequestId (heraldEpochBytes (Session.applicationSessionIdHeraldEpoch session)) (Session.applicationSessionIdOrdinal session) ordinal)
call :: Fixture -> Word64 -> LifecycleCommand -> HeraldState -> IO (HeraldState, EffectBatch)
call (Fixture _ _ binding session _ _) ordinal command state = do
  assertPreparationWorkReference state
  result@(successor, _) <- step (ApplicationLifecycleInput (CallApplicationLifecycle binding session (request session ordinal) locator command)) state
  assertPreparationWorkReference successor
  pure result
status :: Fixture -> Word64 -> HeraldState -> IO LifecycleStatus
status (Fixture _ _ _ session _ process) ordinal state =
  maybe
    (assertFailure "lifecycle result missing")
    (pure . Preparation.requestStatus)
    (Preparation.lookupRequest (Preparation.lifecycleKey process (request session ordinal)) (startupProcessPreparationState state))
begin :: Fixture -> Access.PrimordialSelection -> IO (HeraldState, EffectBatch, ChildPreparation)
begin fixtureValue@(Fixture initial _ _ _ _ _) selection = do
  (accepted, effects) <- call fixtureValue 1 (BeginChild locator selection) initial
  observed <- status fixtureValue 1 accepted
  reference <- case observed of LifecycleCompleted (ChildPreparationAccepted value) -> pure value; other -> assertFailure (show other)
  pure (accepted, effects, reference)
apply :: OracleBinding -> Oracle.OracleState -> EffectBatch -> HeraldState -> IO (HeraldState, Oracle.OracleState, EffectBatch)
apply binding oracle effects state = do
  envelope <- submitted effects
  original <- maybe (assertFailure "preparation dispatch lost its request witness") pure (Client.lookupOracleRequestById (OracleCommand.oracleEnvelopeRequestId (canonicalOracleEnvelopeValue envelope)) (startupOracleClientState state))
  let reference = Client.oracleRequestWitnessRef original
  assertEqual "pending preparation result is not inferred from unrelated control" Nothing (Client.oracleClientRequestEvidence reference (startupOracleClientState state))
  (nextOracle, entry) <- commitEntry oracle envelope
  (next, emitted) <- step (OracleInput (OracleEntriesReceived binding (entry :| []))) state
  assertEqual "preparation retains exact local result evidence after settlement" (Just entry) (Client.oracleClientRequestEvidence reference (startupOracleClientState next))
  assertPreparationWorkReference next
  pure (next, nextOracle, emitted)
prepare :: Fixture -> Access.PrimordialSelection -> IO (HeraldState, Oracle.OracleState, ChildPreparation, PreparedChild)
prepare fixtureValue@(Fixture _ oracleBinding _ _ _ _) selection = do
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (pending, effects, reference) <- begin fixtureValue selection
  (applied, nextOracle, _) <- apply oracleBinding oracle effects pending
  (queried, _) <- call fixtureValue 2 (AwaitPreparedChild reference) applied
  result <- status fixtureValue 2 queried
  child <- case result of LifecycleCompleted (ChildPrepared value) -> pure value; other -> assertFailure (show other)
  pure (queried, nextOracle, reference, child)

caseChildTiming :: Assertion
caseChildTiming = do
  let target = checked (takeoverTarget 8_000_000)
      genesis = Fixtures.fixtureStep14CheckedGenesis
  (initial, _) <- checkedIO (initialHeraldWithTiming target Nothing (monotonicInstant 0) genesis (Fixtures.fixtureCheckedInitialBootstrapsFor genesis) Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH1)
  (state, oracleBinding, _) <- bindInitial genesis initial
  fixtureValue <- openFixture state oracleBinding
  (_, _, _, child) <- prepare fixtureValue emptySelection
  assertEqual "child connection carries the deployment target" target (connectionDescriptorTakeoverTarget (preparedChildConnection child))

caseSelected :: Assertion
caseSelected = do
  fixtureValue@(Fixture base _ _ _ _ parent) <- fixture
  access <- maybe (assertFailure "founder startup") pure (Application.applicationBootstrapAccess parent (startupApplicationState base))
  writer <- maybe (assertFailure "writer") pure (Map.lookup (Access.environmentWriterKey Access.NeutralVertexRole) (Access.accessEntries (Application.bootstrapAccessPrimordial access)))
  let selection = checked (Access.primordialSelection [("writer", writer)] (Set.singleton "writer") Nothing)
  (prepared, _, reference, child) <- prepare fixtureValue selection
  process <- childProcess reference prepared
  assertEqual "preparation does not await label transfer" (Right (Just ["writer"])) (Lifecycle.childReadiness reference prepared)
  assertBool "parent receives normal child process name possession" (Controlled.controlledHasNormalPossession parent (globalObjectIdFromProcessEpochId process) (startupControlledState prepared))
  assertEqual
    "parent-private child name resolves to applied process"
    (Right (globalUniqueIdFromGlobalObjectId (globalObjectIdFromProcessEpochId process)))
    (Application.resolveApplicationPrivateUniqueId parent (Private.privateProcessUniqueId (preparedChildProcess child)) (startupApplicationState prepared))
  childAccess <- maybe (assertFailure "child startup") pure (Application.applicationBootstrapAccess process (startupApplicationState prepared))
  assertEqual "child receives exactly selected key" (Set.singleton "writer") (Map.keysSet (Access.accessEntries (Application.bootstrapAccessPrimordial childAccess)))
  assertEqual "no child session is created by preparation" [] [session | session <- Application.applicationWitnessSessions (Application.applicationStateWitness (startupApplicationState prepared)), Application.applicationSessionWitnessProcess session == process]

caseClaim :: Assertion
caseClaim = do
  fixtureValue <- fixture
  (prepared, _, reference, child) <- prepare fixtureValue emptySelection
  let claim = checked (initialClaimId (Bytes.replicate 32 1))
      other = checked (initialClaimId (Bytes.replicate 32 2))
  (claimed, effects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 20) (preparedChildConnection child) claim Nothing)) prepared
  (binding, session) <- openedSession effects
  (retried, retryEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 21) (preparedChildConnection child) claim Nothing)) claimed
  assertEqual "lost first reply recovers same binding/session" (binding, session) =<< openedSession retryEffects
  (distinct, distinctEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 22) (preparedChildConnection child) other Nothing)) retried
  assertBool "distinct claim is rejected" (RejectInitialClaim (candidateApplicationLane 22) other StartupAlreadyClaimed `elem` effectBatchMembers distinctEffects)
  (cancelled, cancelEffects) <- call fixtureValue 3 (CancelChild reference) distinct
  assertEqual "claim wins cancellation" (LifecycleCompleted ChildAlreadyAttached) =<< status fixtureValue 3 cancelled
  assertEqual "cancellation after attachment sends no End" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers cancelEffects]

caseCancelUnsent :: Assertion
caseCancelUnsent = do
  fixtureValue@(Fixture initial oracleBinding binding session attachment parent) <- fixture
  (offline, _) <- step (OracleInput (OracleBindingLost oracleBinding)) initial
  let offlineFixture = Fixture offline oracleBinding binding session attachment parent
  (pending, effects, reference) <- begin offlineFixture emptySelection
  assertEqual "unbound acceptance does not submit Start" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers effects]
  (cancelled, cancelEffects) <- call fixtureValue 2 (CancelChild reference) pending
  assertEqual "unsent cancellation completes without logical process" (LifecycleCompleted ChildCancelled) =<< status fixtureValue 2 cancelled
  assertEqual "no Start or End request was minted" 0 (length (Client.oracleClientRequestEntries (startupOracleClientState cancelled)))
  assertEqual "no cancellation Oracle effects" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers cancelEffects]

caseCancelSelfFence :: Assertion
caseCancelSelfFence = do
  let genesis = Fixtures.fixtureStep14CheckedGenesis
      local = Genesis.checkedLocalHeraldEpoch genesis
      voters = Set.delete local (Set.fromList (Genesis.checkedActiveHeraldEpochs genesis))
  isolation <- checkedIO (checkIsolationConfiguration 1000 1000)
  (initial, _) <-
    checkedIO
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
  (connected, binding, _) <- bindInitial genesis initial
  fixtureValue@(Fixture _ _ applicationBinding session attachment _) <- openFixture connected binding
  (pending, _, reference) <- begin fixtureValue emptySelection
  (cancelling, _) <- call fixtureValue 2 (CancelChild reference) pending
  let witness = Isolation.stateWitness (startupIsolationState cancelling)
  timer <- maybe (assertFailure "isolation timer missing") pure (Isolation.isolationWitnessCurrentTimer witness)
  deadline <- maybe (assertFailure "isolation deadline missing") pure (Isolation.isolationWitnessCurrentDeadline witness)
  (fenced, effects) <- checkedIO (verifiedStepHerald (heraldInput deadline (RuntimeObserved (TimerObserved timer (TimerFired deadline)))) cancelling)
  assertEqual
    "unresolved submitted Start has no ordered cancellation acknowledgment"
    (LifecycleRejected (LifecycleStartupFailed StartupNotLive))
    =<< status fixtureValue 2 fenced
  assertEqual "self-fence makes no new lifecycle Oracle request" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers effects]
  assertEqual "self-fence retains the one uncertain Start" 1 (length (Client.oracleClientRequestEntries (startupOracleClientState fenced)))
  process <- childProcess reference fenced
  let descriptor = checked (connectionDescriptor locator (systemIdBytes (Genesis.checkedSystemId genesis)) (heraldEpochBytes local) (processEpochIdBytes process))
      claim = checked (initialClaimId (Bytes.replicate 32 0x84))
  (_, rejectedClaim) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 84) descriptor claim Nothing)) fenced
  assertBool "isolation rejects with the original claim correlation and typed startup result" (RejectInitialClaim (candidateApplicationLane 84) claim StartupNotLive `elem` effectBatchMembers rejectedClaim)
  (_, queried) <- step (ApplicationLifecycleInput (GetApplicationLifecycleResult applicationBinding session (request session 2))) fenced
  assertBool "isolation permits the known cancellation result query" (SendApplicationLifecycleReply (EstablishedApplicationDisposition applicationBinding) (LifecycleReply (request session 2) (LifecycleRejected (LifecycleStartupFailed StartupNotLive))) `elem` effectBatchMembers queried)
  (_, absent) <- step (ApplicationLifecycleInput (RecoverApplicationLifecycleResult (candidateApplicationLane 85) attachment (request session 2))) fenced
  assertBool "restricted recovery of a non-End request returns correlated absence" (SendApplicationLifecycleReply (CandidateApplicationDisposition (candidateApplicationLane 85)) (LifecycleAbsent (request session 2)) `elem` effectBatchMembers absent)
  (refused, refusedEffects) <- call fixtureValue 3 EndOwnProcess fenced
  assertBool "isolation rejects a fresh lifecycle call with its correlation" (SendApplicationLifecycleReply (EstablishedApplicationDisposition applicationBinding) (LifecycleReply (request session 3) (LifecycleRejected LifecycleTargetUnavailable)) `elem` effectBatchMembers refusedEffects)
  assertBool "rejected isolated call allocates no retained lifecycle request" (startupProcessPreparationState refused == startupProcessPreparationState fenced)

caseHelloDispatch :: Assertion
caseHelloDispatch = do
  Fixture initial oracleBinding binding session attachment parent <- fixture
  (offline, lossEffects) <- step (OracleInput (OracleBindingLost oracleBinding)) initial
  retry <- case [value | RunOracleClientAction (ScheduleOracleRetry OracleConnectionRetry value) <- effectBatchMembers lossEffects] of
    [one] -> pure one
    _ -> assertFailure "expected connection retry"
  let offlineFixture = Fixture offline oracleBinding binding session attachment parent
  (pending, _, reference) <- begin offlineFixture emptySelection
  (connecting, connectEffects) <- step (OracleInput (OracleRetryElapsed retry)) pending
  attempt <- case [value | RunOracleClientAction (ConnectAndHelloOracle value _) <- effectBatchMembers connectEffects] of
    [one] -> pure one
    _ -> assertFailure "expected connection attempt"
  let node = oracleContactNode (oracleConnectAttemptContact attempt)
      hello = oracleHelloAcceptance node (oracleObservedTerm 1) (controlIndex 0) (Just node) True
  (connected, effects) <- step (OracleInput (OracleHelloReceived attempt hello)) connecting
  _ <- submitted effects
  retained <- maybe (assertFailure "preparation missing") pure (Preparation.lookupPreparation reference (startupProcessPreparationState connected))
  assertBool "Hello retained the one Start reference" (isJust (Preparation.preparationStartReference retained))
  assertBool "Hello consumes no additional generated identity" (startupIdGeneratorState pending == startupIdGeneratorState connected)

caseCancelPending :: Assertion
caseCancelPending = do
  fixtureValue@(Fixture _ binding _ _ _ _) <- fixture
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (pending, startEffects, reference) <- begin fixtureValue emptySelection
  (cancelling, _) <- call fixtureValue 2 (CancelChild reference) pending
  assertEqual "submitted cancellation waits ordered outcome" (LifecyclePending []) =<< status fixtureValue 2 cancelling
  (started, oracle1, endEffects) <- apply binding oracle startEffects cancelling
  process <- childProcess reference started
  assertEqual "cancelled accepted Start never exposes child namespace" Nothing (Application.applicationBootstrapAccess process (startupApplicationState started))
  (ended, _, _) <- apply binding oracle1 endEffects started
  assertEqual "ordered End completes cancellation" (LifecycleCompleted ChildCancelled) =<< status fixtureValue 2 ended
  assertBool "child is ended" (not (Projection.oracleViewProcessIsLive process (Projection.oracleView (startupOracleProjectionState ended))))

caseParentEnd :: Assertion
caseParentEnd = do
  fixtureValue@(Fixture _ binding applicationBinding session attachment parent) <- fixture
  (prepared, oracle, reference, _) <- prepare fixtureValue emptySelection
  child <- childProcess reference prepared
  (ending, effects) <- call fixtureValue 3 EndOwnProcess prepared
  (ended, _, endEffects) <- apply binding oracle effects ending
  assertBool "parent ended" (not (Projection.oracleViewProcessIsLive parent (Projection.oracleView (startupOracleProjectionState ended))))
  assertBool "parent death is not child death" (Projection.oracleViewProcessIsLive child (Projection.oracleView (startupOracleProjectionState ended)))
  assertBool "unclaimed child retains exact startup" (isJust (Application.applicationBootstrapAccess child (startupApplicationState ended)))
  assertEqual "all terminal receipts of the ended parent session are released" [] [key | (key, record) <- Preparation.requestEntries (startupProcessPreparationState ended), Preparation.requestSession record == session]
  assertBool "ordered End carries the exact correlated result" (SendApplicationLifecycleReply (EstablishedApplicationDisposition applicationBinding) (LifecycleReply (request session 3) (LifecycleCompleted ProcessEnded)) `elem` effectBatchMembers endEffects)
  (recovered, recoveryEffects) <- step (ApplicationLifecycleInput (RecoverApplicationLifecycleResult (candidateApplicationLane 44) attachment (request session 3))) ended
  assertBool "recovery observes absence after terminal receipt retirement" (SendApplicationLifecycleReply (CandidateApplicationDisposition (candidateApplicationLane 44)) (LifecycleAbsent (request session 3)) `elem` effectBatchMembers recoveryEffects)
  assertEqual "recovery creates no parent session" Nothing (Application.applicationSessionProcess session (startupApplicationState recovered))

caseOwnEndAfterEnvironment :: Assertion
caseOwnEndAfterEnvironment = do
  fixtureValue@(Fixture initial _ binding session _ process) <- fixture
  (environmentPending, _) <- step (ApplicationRequestInput (CallApplicationRequest binding session (Request.requestId 1) NewEnvironmentApplication)) initial
  assertBool
    "newenv has accepted work before its common cut"
    (not (null (Application.applicationPendingEnvironmentEntries (startupApplicationState environmentPending))))
  let initialClient = startupOracleClientState environmentPending
      key = Preparation.lifecycleKey process (request session 1)
      ownEndReference state = Preparation.lookupRequest key (startupProcessPreparationState state) >>= Preparation.requestEndReference
  (ending, endEffects) <- call fixtureValue 1 EndOwnProcess environmentPending
  assertEqual "the caller's explicit End remains pending" (LifecyclePending []) =<< status fixtureValue 1 ending
  assertEqual "pending environment preserves its writer authority" Nothing (ownEndReference ending)
  assertEqual "End has not entered the Oracle client" (Client.oracleClientNextRequestSequence initialClient) (Client.oracleClientNextRequestSequence (startupOracleClientState ending))
  assertEqual "waiting End emits no Oracle mutation" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers endEffects]
  (retried, retryEffects) <- call fixtureValue 1 EndOwnProcess ending
  assertEqual "exact retry has no Oracle reference" Nothing (ownEndReference retried)
  assertEqual "exact retry emits no Oracle mutation" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers retryEffects]
  assertBool "exact retry preserves accepted environment identities" (startupIdGeneratorState ending == startupIdGeneratorState retried)
  -- The root cut establishes the fresh writers; the second cut establishes
  -- the hub and edges. A settled phase needs no further synthetic reports.
  completed <- foldM settleEnvironmentCut retried [(), ()]
  assertEqual "all environment phases completed before End dispatch" [] (Application.applicationPendingEnvironmentEntries (startupApplicationState completed))
  assertPreparationWorkReference completed
  (dispatched, dispatchEffects) <- checkedIO (Lifecycle.advanceProcessPreparations completed)
  assertBool "completion releases the retained End without another application call" (isJust (ownEndReference dispatched))
  assertEqual "completion creates one Oracle End intention" (Client.oracleClientNextRequestSequence initialClient + 1) (Client.oracleClientNextRequestSequence (startupOracleClientState dispatched))
  assertEqual "completion dispatches exactly once" 1 (length [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers dispatchEffects])
  (redriven, redriveEffects) <- checkedIO (Lifecycle.advanceProcessPreparations dispatched)
  assertEqual "redrive preserves the exact End reference" (ownEndReference dispatched) (ownEndReference redriven)
  assertEqual "redrive emits no duplicate Oracle submission" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers redriveEffects]
  assertEqual "waiting and dispatch preserve composed invariants" (Right ()) (validateHeraldState dispatched)
  where
    settleEnvironmentCut state ()
      | null (Application.applicationPendingEnvironmentEntries (startupApplicationState state)) = pure state
      | otherwise = settleStructuralPublication state

caseForcedEndDuringOwnEnd :: Assertion
caseForcedEndDuringOwnEnd = do
  fixtureValue@(Fixture initial oracleBinding binding session _ process) <- fixture
  (environmentPending, _) <- step (ApplicationRequestInput (CallApplicationRequest binding session (Request.requestId 1) NewEnvironmentApplication)) initial
  (ending, _) <- call fixtureValue 1 EndOwnProcess environmentPending
  let local = Genesis.checkedLocalHeraldEpoch (startupGenesis initial)
      key = Preparation.lifecycleKey process (request session 1)
      nextRequest = Client.oracleClientNextRequestSequence (startupOracleClientState ending)
      command = checked (OracleCommand.endProcessEpochCommand process ExplicitAdministrativeEnd)
      envelope = canonicalizeOracleEnvelope (OracleCommand.oracleEnvelope (oracleClientRequestId local 701) Nothing local command)
  oracle <- checkedIO (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (_, entry) <- commitEntry oracle envelope
  (ended, effects) <- step (OracleInput (OracleEntriesReceived oracleBinding (entry :| []))) ending
  assertBool "the independent canonical End is observed" (isJust (Projection.oracleViewEndedProcess process (Projection.oracleView (startupOracleProjectionState ended))))
  assertEqual "waiting own-End consumes no new Oracle request" nextRequest (Client.oracleClientNextRequestSequence (startupOracleClientState ended))
  assertEqual "waiting own-End emits no redundant Oracle mutation" [] [() | RunOracleClientAction SubmitOracleRequest {} <- effectBatchMembers effects]
  assertBool
    "the accepted own-End reports the already observed terminal result"
    (SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleReply (request session 1) (LifecycleCompleted ProcessEnded)) `elem` effectBatchMembers effects)
  assertEqual "the ended session's terminal lifecycle receipt is released" Nothing (Preparation.lookupRequest key (startupProcessPreparationState ended))
  settled <- settleStructuralPublication ended
  assertEqual "the interrupted prefix settles without pending construction" [] (Application.applicationPendingEnvironmentEntries (startupApplicationState settled))
  assertEqual "prefix settlement does not resurrect the End request" nextRequest (Client.oracleClientNextRequestSequence (startupOracleClientState settled))
  assertPreparationWorkReference settled
  assertEqual "forced-End overlap preserves composed invariants" (Right ()) (validateHeraldState settled)

caseMembershipBeginGate :: Assertion
caseMembershipBeginGate = do
  fixtureValue@(Fixture initial _ binding session _ _) <- fixture
  let gated = replaceStartupApplicationState (Application.setApplicationMembershipGate True (startupApplicationState initial)) initial
      ingress = CallApplicationLifecycle binding session (request session 1) locator (BeginChild locator emptySelection)
  (pending, _) <- checkedIO (Lifecycle.applyProcessPreparationIngress ingress gated)
  assertEqual "gated request remains pending" (LifecyclePending []) =<< status fixtureValue 1 pending
  assertEqual "one exact deferred call" 1 (length (Preparation.deferredBeginEntries (startupProcessPreparationState pending)))
  assertEqual "no child preparation yet" [] (Preparation.preparationEntries (startupProcessPreparationState pending))
  assertEqual "no outgoing source grant captured" [] (Preparation.outgoingEntries (startupProcessPreparationState pending))
  assertBool "no generated identity consumed" (startupIdGeneratorState pending == startupIdGeneratorState initial)
  assertBool "no private identity or startup access allocated" (Application.applicationPrivateIdentity (startupApplicationState pending) == Application.applicationPrivateIdentity (startupApplicationState initial))
  (retried, _) <- checkedIO (Lifecycle.applyProcessPreparationIngress ingress pending)
  assertBool "pending exact retry has the same retained call" (startupProcessPreparationState retried == startupProcessPreparationState pending)
  let opened = replaceStartupApplicationState (Application.setApplicationMembershipGate False (startupApplicationState retried)) retried
  (admitted, _) <- checkedIO (Lifecycle.advanceProcessPreparations opened)
  reference <-
    status fixtureValue 1 admitted >>= \caseResult -> case caseResult of
      LifecycleCompleted (ChildPreparationAccepted value) -> pure value
      other -> assertFailure (show other)
  assertEqual "deferred call consumed once" [] (Preparation.deferredBeginEntries (startupProcessPreparationState admitted))
  assertEqual "one prepared child" [reference] (map fst (Preparation.preparationEntries (startupProcessPreparationState admitted)))
  (again, _) <- checkedIO (Lifecycle.advanceProcessPreparations admitted)
  assertBool "redrive does not generate another child" (startupIdGeneratorState again == startupIdGeneratorState admitted)

caseMembershipClaimGate :: Assertion
caseMembershipClaimGate = do
  fixtureValue <- fixture
  (prepared, _, reference, child) <- prepare fixtureValue emptySelection
  let gated = replaceStartupApplicationState (Application.setApplicationMembershipGate True (startupApplicationState prepared)) prepared
      claim = checked (initialClaimId (Bytes.replicate 32 0x72))
  assertEqual "name-only first attachment waits on membership" (Right (Just [])) (Lifecycle.childReadiness reference gated)
  (pending, effects) <- checkedIO (Lifecycle.applyInitialProcessClaim (candidateApplicationLane 72) (preparedChildConnection child) claim Nothing gated)
  assertBool "first claim receives pending" (SendInitialClaimPending (candidateApplicationLane 72) claim [] `elem` effectBatchMembers effects)
  assertEqual "first pending claim retains a redrive path" 1 (length (Preparation.pendingClaimEntries (startupProcessPreparationState pending)))
  let removed =
        replaceStartupProcessPreparationState
          (Preparation.observeCandidateLoss (candidateApplicationLane 72) (startupProcessPreparationState pending))
          pending
      reopened = replaceStartupApplicationState (Application.setApplicationMembershipGate False (startupApplicationState removed)) removed
  (afterClose, closeEffects) <- checkedIO (Lifecycle.advanceProcessPreparations reopened)
  assertEqual "closed claim lane is not redriven when the gate opens" [] (Preparation.pendingClaimEntries (startupProcessPreparationState afterClose))
  assertEqual "closed claim consumes no attachment grant" [] [() | SetApplicationConnectionDisposition {} <- effectBatchMembers closeEffects]
  (_, replacementEffects) <- checkedIO (Lifecycle.applyInitialProcessClaim (candidateApplicationLane 73) (preparedChildConnection child) claim Nothing afterClose)
  _ <- openedSession replacementEffects
  pure ()

-- The reference bypasses dirty selection, cached readiness and keyed result
-- lookup, but shares corrected checked atomic handlers and exact effect order.
assertPreparationWorkReference :: HeraldState -> Assertion
assertPreparationWorkReference predecessor =
  case (Lifecycle.advanceProcessPreparations predecessor, Lifecycle.advanceProcessPreparationsExhaustive predecessor) of
    (Left actual, Left expected) -> assertEqual "sparse preparation work preserves the exact invariant fault" expected actual
    (Right (actual, actualEffects), Right (expected, expectedEffects)) -> do
      assertBool "sparse preparation work matches independent full inventories and uncached answers" (Lifecycle.sameProcessPreparationWorkState actual expected)
      assertEqual "sparse preparation work preserves ordered effects" (effectBatchMembers expectedEffects) (effectBatchMembers actualEffects)
      forM_ [actual, expected] $ \state -> assertEqual "preparation joins, watches and schedules validate independently" (Right ()) (Preparation.validateState (startupProcessPreparationState state))
    (Left problem, Right _) -> assertFailure ("only sparse preparation work faulted: " <> show problem)
    (Right _, Left problem) -> assertFailure ("only exhaustive preparation work faulted: " <> show problem)

caseSelectiveReadinessGate :: Assertion
caseSelectiveReadinessGate = do
  fixtureValue@(Fixture _ _ binding session attachment parent) <- fixture
  (prepared, _, reference, _) <- prepare fixtureValue emptySelection
  let key = Preparation.lifecycleKey parent (request session 3)
      gated = replaceStartupApplicationState (Application.setApplicationMembershipGate True (startupApplicationState prepared)) prepared
      owner = checked (Preparation.retainRequest key (AwaitChildReady reference) (LifecyclePending []) session binding attachment (startupProcessPreparationState gated))
      pending = replaceStartupProcessPreparationState owner gated
  assertPreparationWorkReference pending
  (parked, _) <- checkedIO (Lifecycle.advanceProcessPreparations pending)
  assertEqual "a blocked required-readiness request remains pending" (LifecyclePending []) =<< status fixtureValue 3 parked
  assertEqual "one demanded answer is retained compactly" (Just (Right (Just []))) (Preparation.lookupReadiness reference (startupProcessPreparationState parked))
  assertEqual "the blocked request consumed its work" Set.empty (Preparation.pendingWork Preparation.RequestStage (startupProcessPreparationState parked))
  forM_ [1 .. 5 :: Int] $ \tick -> do
    let observed = replaceStartupLastObservedTime (monotonicInstant (fromIntegral tick)) parked
    assertPreparationWorkReference observed
    (quiet, effects) <- checkedIO (Lifecycle.advanceProcessPreparations observed)
    assertBool "unrelated observations preserve the entire prepared owner" (startupProcessPreparationState quiet == startupProcessPreparationState observed)
    assertEqual "unchanged dependencies select no request work" Set.empty (Preparation.pendingWork Preparation.RequestStage (startupProcessPreparationState quiet))
    assertEqual "unchanged dependencies emit no lifecycle replies" [] (effectBatchMembers effects)
  let reopened = replaceStartupApplicationState (Application.setApplicationMembershipGate False (startupApplicationState parked)) parked
  assertPreparationWorkReference reopened
  (ready, effects) <- checkedIO (Lifecycle.advanceProcessPreparations reopened)
  assertEqual "the actual gate change settles the waiting request" (LifecycleCompleted ChildReady) =<< status fixtureValue 3 ready
  assertEqual "the invalidated answer is replaced with current readiness" (Just (Right Nothing)) (Preparation.lookupReadiness reference (startupProcessPreparationState ready))
  assertBool "a completed request leaves active work" (Set.notMember key (Preparation.workIds Preparation.RequestStage (startupProcessPreparationState ready)))
  assertEqual "exactly one final lifecycle result is sent" 1 (length [() | SendApplicationLifecycleReply _ (LifecycleReply identifier (LifecycleCompleted ChildReady)) <- effectBatchMembers effects, identifier == request session 3])

caseGenesisClaimWorkRetirement :: Assertion
caseGenesisClaimWorkRetirement = do
  let genesis = Fixtures.fixtureStep14CheckedGenesis
  bootstrap <- case Fixtures.fixtureLocalBootstrapIds of first : _ -> pure first; _ -> assertFailure "missing genesis bootstrap"
  -- Uniform genesis claims require the sole primordial process in the system.
  -- The general Step-14 fixture includes a remote process and cannot use this path.
  let bootstraps = checked (Genesis.checkInitialBootstraps genesis (Genesis.PrimordialProcessManifest [bootstrap]))
  (unbound, _) <- checkedIO (initialHerald (monotonicInstant 0) genesis bootstraps Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeedH1 Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration)
  (initial, _, _) <- bindInitial genesis unbound
  assertEqual "fixture has exactly one projected genesis process" 1 (length (Projection.projectedBootstraps (startupOracleProjectionState initial)))
  attachment <- maybe (assertFailure "missing genesis attachment") pure (primordialApplicationAttachment genesis bootstraps bootstrap)
  let descriptor = checked (connectionDescriptorWithTiming (startupTakeoverTarget initial) locator (systemIdBytes (Genesis.checkedSystemId genesis)) (heraldEpochBytes (Genesis.checkedLocalHeraldEpoch genesis)) (Session.applicationAttachmentBytes attachment))
      claim = checked (initialClaimId (Bytes.replicate 32 0x75))
      lane = candidateApplicationLane 75
      gated = replaceStartupApplicationState (Application.setApplicationMembershipGate True (startupApplicationState initial)) initial
  (pending, pendingEffects) <- checkedIO (Lifecycle.applyInitialProcessClaim lane descriptor claim (Just locator) gated)
  assertBool "the genesis claim first waits on its real membership gate" (SendInitialClaimPending lane claim [] `elem` effectBatchMembers pendingEffects)
  assertBool "the pending genesis lane is registered" (Set.member (attachment, claim) (Preparation.workIds Preparation.ClaimStage (startupProcessPreparationState pending)))
  assertEqual "pending genesis claim indexes are valid" (Right ()) (Preparation.validateState (startupProcessPreparationState pending))
  let reopened = replaceStartupApplicationState (Application.setApplicationMembershipGate False (startupApplicationState pending)) pending
  assertPreparationWorkReference reopened
  (opened, openedEffects) <- checkedIO (Lifecycle.advanceProcessPreparations reopened)
  _ <- openedSession openedEffects
  assertEqual "successful genesis admission removes the pending owner record" Nothing (Preparation.lookupPendingClaim attachment claim (startupProcessPreparationState opened))
  assertBool "successful genesis admission removes its work registration" (Set.notMember (attachment, claim) (Preparation.workIds Preparation.ClaimStage (startupProcessPreparationState opened)))
  assertPreparationWorkReference opened
  (quiet, quietEffects) <- checkedIO (Lifecycle.advanceProcessPreparations opened)
  assertEqual "no automatic acceptance repeats on the next drive" [] [() | SetApplicationConnectionDisposition {} <- effectBatchMembers quietEffects]
  (_, retryEffects) <- checkedIO (Lifecycle.applyInitialProcessClaim (candidateApplicationLane 76) descriptor claim (Just locator) quiet)
  _ <- openedSession retryEffects
  assertEqual "retired genesis claim indexes remain valid" (Right ()) (Preparation.validateState (startupProcessPreparationState quiet))

caseSessionScope :: Assertion
caseSessionScope = do
  fixtureValue@(Fixture initial oracleBinding _ _ attachment parent) <- fixture
  (first, _, _) <- begin fixtureValue emptySelection
  (retry, _) <- call fixtureValue 1 (BeginChild locator emptySelection) first
  assertEqual "retry retains exactly one preparation" 1 (length (Preparation.preparationEntries (startupProcessPreparationState retry)))
  (secondSession, effects) <- step (ApplicationSessionInput (OpenApplicationSession (candidateApplicationLane 50) attachment (Session.clientNonce 50))) retry
  (binding, session) <- openedSession effects
  let secondFixture = Fixture secondSession oracleBinding binding session attachment parent
  (second, _, _) <- begin secondFixture emptySelection
  assertEqual "same ordinal from a second session owns another preparation" 2 (length (Preparation.preparationEntries (startupProcessPreparationState second)))
  assertBool "original fixture remains valid" (initial /= second)

caseChildOwnEnd :: Assertion
caseChildOwnEnd = do
  fixtureValue@(Fixture _ oracleBinding _ _ _ parent) <- fixture
  (prepared, oracle, reference, child) <- prepare fixtureValue emptySelection
  process <- childProcess reference prepared
  let claim = checked (initialClaimId (Bytes.replicate 32 0x81))
      attachment = Session.applicationAttachmentForProcess process
  (claimed, claimEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 81) (preparedChildConnection child) claim Nothing)) prepared
  (binding, session) <- openedSession claimEffects
  let childFixture = Fixture claimed oracleBinding binding session attachment process
      endRequest = request session 1
  (ending, endEffects) <- call childFixture 1 EndOwnProcess claimed
  (ended, _, resultEffects) <- apply oracleBinding oracle endEffects ending
  assertEqual "child own-End receipt is released at terminal settlement" Nothing (Preparation.lookupRequest (Preparation.lifecycleKey process endRequest) (startupProcessPreparationState ended))
  assertBool "prepared child is ended" (not (Projection.oracleViewProcessIsLive process (Projection.oracleView (startupOracleProjectionState ended))))
  assertBool "child End does not end its parent" (Projection.oracleViewProcessIsLive parent (Projection.oracleView (startupOracleProjectionState ended)))
  assertEqual "child startup access retired" Nothing (Application.applicationBootstrapAccess process (startupApplicationState ended))
  let terminalReply = SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleReply endRequest (LifecycleCompleted ProcessEnded))
  assertBool "terminal own-End result reaches the established connection" (terminalReply `elem` effectBatchMembers resultEffects)
  (_, recovered) <- step (ApplicationLifecycleInput (RecoverApplicationLifecycleResult (candidateApplicationLane 82) attachment endRequest)) ended
  assertBool "recovery cannot resurrect the retired child receipt" (SendApplicationLifecycleReply (CandidateApplicationDisposition (candidateApplicationLane 82)) (LifecycleAbsent endRequest) `elem` effectBatchMembers recovered)
  (_, retryEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 83) (preparedChildConnection child) claim Nothing)) ended
  assertBool "terminal child cannot replay its initial session" (RejectInitialClaim (candidateApplicationLane 83) claim StartupNotLive `elem` effectBatchMembers retryEffects)

caseInvalid :: Assertion
caseInvalid = do
  fixtureValue@(Fixture initial _ _ _ _ _) <- fixture
  let unknown = checked (Private.mkPrivateUniqueId 99999)
      selection = checked (Access.primordialSelection [("unknown", Access.Identity unknown)] Set.empty Nothing)
  (invalid, _) <- call fixtureValue 1 (BeginChild locator selection) initial
  assertEqual "unsupported private reference rejects" (LifecycleRejected LifecycleSelectionNotAdmitted) =<< status fixtureValue 1 invalid
  (remote, _) <- call fixtureValue 2 (BeginChild (checked (heraldLocator "other" 35001)) emptySelection) invalid
  assertEqual "P03 remote target is explicit" (LifecycleRejected LifecycleUnknownTarget) =<< status fixtureValue 2 remote
  assertBool "rejection consumes no generated identities" (startupIdGeneratorState initial == startupIdGeneratorState remote)

casePartialTransferCancellation :: Assertion
casePartialTransferCancellation = do
  fixtureValue@(Fixture base oracleBinding binding session _ parent) <- fixture
  access <- maybe (assertFailure "founder startup") pure (Application.applicationBootstrapAccess parent (startupApplicationState base))
  let entries = Access.accessEntries (Application.bootstrapAccessPrimordial access)
  writer <- case Map.lookup (Access.environmentWriterKey Access.NeutralVertexRole) entries of
    Just (Access.Writer value) -> pure value
    other -> assertFailure ("expected selected writer: " <> show other)
  otherWriter <- maybe (assertFailure "second selected writer") pure (Map.lookup (Access.environmentWriterKey Access.EdgeRole) entries)
  let selection = checked (Access.primordialSelection [("transferred", Access.Writer writer), ("untransferred", otherWriter)] (Set.fromList ["transferred", "untransferred"]) Nothing)
  (prepared, oracle, reference, child) <- prepare fixtureValue selection
  process <- childProcess reference prepared
  identity <- checkedIO (Application.resolveApplicationPrivateUniqueId parent (Private.privateNablaUniqueId writer) (startupApplicationState prepared))
  let object = globalObjectIdFromGlobalUniqueId identity
      label = LabelApplication (Private.asPrivateObjectId (Private.privateNablaUniqueId writer)) ((ApplicationValue.ProcessLabel (Application.bootstrapAccessProcess access), 0)) (LabelToProcess (preparedChildProcess child))
      ingress = ApplicationRequestInput (CallApplicationRequest binding session (Request.requestId 1) label)
  (accepted, _) <- step ingress prepared
  (afterOpenOracle, afterOpen, _) <- commitNextLabelRequest 10 oracle accepted
  (ready, _) <- step ingress (afterOpen)
  (transferredOracle, transferred, _) <- commitCompleteLabelWorkflow 11 afterOpenOracle ready
  committed <- maybe (assertFailure "completed transfer record missing") pure (Controlled.controlledReleasedLabelRecord object (startupControlledState transferred))
  assertEqual "the first selected root is transferred to the child" (DomainLabel.ReleasedLabelView ((DomainValue.ProcessLabel process, 1))) (DomainLabel.releasedLabelStateView (DomainLabel.labelRecordReleasedState committed))
  assertEqual "the second selected root still holds initial claim readiness" (Right (Just ["untransferred"])) (Lifecycle.childReadiness reference transferred)
  let firstClaim = checked (initialClaimId (Bytes.replicate 32 0x91))
      secondClaim = checked (initialClaimId (Bytes.replicate 32 0x92))
  (firstPending, firstEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 91) (preparedChildConnection child) firstClaim Nothing)) transferred
  (bothPending, secondEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 92) (preparedChildConnection child) secondClaim Nothing)) firstPending
  assertBool "first early claim is pending" (SendInitialClaimPending (candidateApplicationLane 91) firstClaim ["untransferred"] `elem` effectBatchMembers firstEffects)
  assertBool "a distinct early claim also remains pending without sealing a winner" (SendInitialClaimPending (candidateApplicationLane 92) secondClaim ["untransferred"] `elem` effectBatchMembers secondEffects)
  assertEqual "first pending claim emits exactly one candidate disposition" 1 (length [() | SendInitialClaimPending lane _ _ <- effectBatchMembers firstEffects, lane == candidateApplicationLane 91])
  assertEqual "background recheck does not emit the first candidate's Pending again" [] [() | SendInitialClaimPending lane _ _ <- effectBatchMembers secondEffects, lane == candidateApplicationLane 91]
  (cancelling, endEffects) <- call fixtureValue 3 (CancelChild reference) bothPending
  assertBool "cancellation seals both pending claim routes" (all (`elem` effectBatchMembers endEffects) [RejectInitialClaim (candidateApplicationLane 91) firstClaim StartupCancelled, RejectInitialClaim (candidateApplicationLane 92) secondClaim StartupCancelled])
  (ended, _, _) <- apply oracleBinding transferredOracle endEffects cancelling
  assertEqual "cancellation completes after the ordered End" (LifecycleCompleted ChildCancelled) =<< status fixtureValue 3 ended
  assertEqual "cancellation preserves the exact committed transfer record" (Just committed) (Controlled.controlledReleasedLabelRecord object (startupControlledState ended))
  assertEqual "the transferred root follows normal zombie semantics" (Just (DomainLabel.ReleasedLabelView ((DomainValue.ZombieLabel process, 1)))) (DomainLabel.releasedLabelStateView <$> Controlled.controlledEffectiveLabelState object (startupControlledState ended))
  assertBool "cancelled child has no startup access" (Application.applicationBootstrapAccess process (startupApplicationState ended) == Nothing)

childProcess :: ChildPreparation -> HeraldState -> IO ProcessEpochId
childProcess reference state =
  maybe
    (assertFailure "preparation missing")
    (pure . processStartProcessEpochId . Preparation.preparationStart)
    (Preparation.lookupPreparation reference (startupProcessPreparationState state))

-- The fixture includes a dynamic reader, bootstrap writers/readers, wrong-role
-- aliases and a missing object. Some variants replace only the checked Alignment
-- owner to isolate readiness from the transfer driver. Each batch compares the
-- complete scalar results in generated, repeated orders.
assertPreparedEndpointQueries :: [HeraldState] -> ProcessEpochId -> GlobalObjectId -> IO QC.Property
assertPreparedEndpointQueries states process object = do
  let unknown = checked (mkGlobalObjectId (Bytes.replicate 32 253))
      otherProcess = checked (mkProcessEpochId (Bytes.replicate 32 252))
      inputs state =
        [ (requester, subject)
        | requester <- [process, otherProcess],
          subject <- object : unknown : map rootObject (Controlled.controlledRootFacts (startupControlledState state))
        ]
      rootObject root = case Controlled.rootFactRole root of
        Controlled.ControlledWriter writer _ -> globalObjectIdFromNablaId writer
        Controlled.ControlledReader reader -> globalObjectIdFromDeltaId reader
      prepared =
        [ (state, Readiness.prepareReadiness state, Operate.prepareControlledQueries (startupControlledState state) (startupGraphState state) (startupStructuralProgressState state))
        | state <- states
        ]
      compareQuery (state, readiness, operations) (requester, subject) =
        let controlled = startupControlledState state
            progress = startupStructuralProgressState state
            writer = nablaIdFromGlobalObjectId subject
            reader = deltaIdFromGlobalObjectId subject
            readerFacts result = (Operate.controlledReadSortId result, Operate.controlledReadOccurrenceId result, Operate.controlledReadHeraldEpoch result)
         in QC.conjoin
              [ fmap Operate.controlledOperateWriterBinding (Operate.checkControlledOperate requester writer controlled progress)
                  QC.=== fmap Operate.controlledOperateWriterBinding (Operate.checkPreparedControlledOperate requester writer operations),
                fmap readerFacts (Operate.checkControlledRead requester reader controlled progress)
                  QC.=== fmap readerFacts (Operate.checkPreparedControlledRead requester reader operations),
                Readiness.requiredDeltaReady requester reader state
                  QC.=== Readiness.preparedRequiredDeltaReady requester reader readiness,
                GraphProgress.structuralNablaProjectionAtCoordinate writer (GraphProgress.structuralLastInstalledCutId progress) (GraphProgress.structuralAppliedControlPrefix progress) progress
                  QC.=== GraphProgress.lookupPreparedCurrentNablaProjection writer (GraphProgress.prepareStructuralQueries (startupGraphState state) progress),
                GraphProgress.lookupInstalledDynamicNabla writer progress
                  QC.=== GraphProgress.lookupPreparedDynamicNabla writer (GraphProgress.prepareStructuralQueries (startupGraphState state) progress),
                GraphProgress.lookupInstalledDynamicDelta reader progress
                  QC.=== GraphProgress.lookupPreparedDynamicDelta reader (GraphProgress.prepareStructuralQueries (startupGraphState state) progress)
              ]
  forM_ states $ \state -> assertEqual "fixture Alignment invariant" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState state))
  pure
    ( QC.forAll (QC.shuffle ([0 .. length prepared - 1] <> [0, 2])) $ \order ->
        QC.conjoin
          [ QC.forAll (QC.shuffle (inputs state)) $ \questions ->
              QC.conjoin [compareQuery batch question | question <- questions]
          | index <- order,
            Just batch@(state, _, _) <- [Map.lookup index (Map.fromList (zip [0 ..] prepared))]
          ]
    )
caseDeltaReadiness :: IO QC.Property
caseDeltaReadiness = do
  (structurallySettled, object, process, _, _) <- settledDeltaControlledFixtureWithPendingWait
  let localEpoch = Genesis.checkedLocalHeraldEpoch (startupGenesis structurallySettled)
      remotes = filter ((/= localEpoch) . Genesis.heraldMemberEpoch) (Genesis.checkedActiveHeralds (startupGenesis structurallySettled))
  withPlacements <- foldM installRemotePlacement structurallySettled remotes
  (withGenerations, _) <- checkedIO (AlignmentUseCase.advanceAlignmentGenerations withPlacements)
  (settled, _) <- checkedIO (AlignmentTransfer.advanceAlignmentTransfers withGenerations)
  let delta = deltaIdFromGlobalObjectId object
      alignment = startupAlignmentState settled
      local = Genesis.checkedLocalHeraldEpoch (startupGenesis settled)
  assertBool "the complete placement cut promotes alignment generations" (not (null (Alignment.alignmentGenerationEntries alignment)))
  assertBool "the local transfer driver completes current local readiness" (not (null (Alignment.alignmentLocalMemberReadinessEntries alignment)))
  assertEqual "the promoted alignment owner has no remaining structural debts" [] (Alignment.liveAlignmentStructuralDebtEntries alignment)
  assertBool "settled current local Delta is operationally ready" (Readiness.requiredDeltaReady process delta settled)
  (generationId, generation) <- case [ (identifier, value)
                                     | (identifier, value) <- Alignment.alignmentGenerationEntries alignment,
                                       any ((== delta) . DomainAlignment.alignmentMemberDelta) (DomainAlignment.alignmentCutExactMembers (Generation.alignmentGenerationCut value))
                                     ] of
    [value] -> pure value
    values -> assertFailure ("expected one initial Delta generation, got " <> show (length values))
  key <- case [ keyValue
              | (keyValue, receipt) <- Alignment.alignmentPromotionEntries alignment,
                generationId `elem` Alignment.alignmentPromotionReceiptGenerationIds receipt
              ] of
    [value] -> pure value
    values -> assertFailure ("expected one promotion, got " <> show (length values))
  let cause = Alignment.alignmentPromotionKeyCause key
      sort = Alignment.alignmentPromotionKeySort key
  currentPlan <- maybe (assertFailure "expected current Delta plan") pure (Alignment.currentAlignmentPlanForSort sort alignment)
  plan <- checkedIO (AlignmentUseCase.replayAlignmentPlanAt settled (Plan.alignmentPlanId currentPlan))
  debt <- checkedIO (Alignment.prepareStructuralDebtRetention cause (normalizeStructuralDebts (Alignment.structuralDebtsForCause cause alignment)) Alignment.emptyState)
  promoted <- checkedIO (Alignment.prepareAlignmentPlanPromotion local cause plan (fst (Alignment.commitStructuralDebtRetention debt)))
  let withoutReady = fst (Alignment.commitAlignmentPromotion promoted)
      withAlignment owner = replaceStartupAlignmentState owner settled
  assertEqual "same exact generation is retained" (Just generation) (Alignment.lookupAlignmentGeneration generationId withoutReady)
  assertBool "installed placement without local completion remains pending" (not (Readiness.requiredDeltaReady process delta (withAlignment withoutReady)))
  readiness <- case [ ready
                    | ((identifier, _), ready) <- Alignment.alignmentLocalMemberReadinessEntries alignment,
                      identifier == generationId
                    ] of
    [value] -> pure value
    values -> assertFailure ("expected one owner-local readiness, got " <> show (length values))
  advertised <- checkedIO (Alignment.prepareClassMemberReady readiness withoutReady)
  let onlyAdvertised = fst (Alignment.commitClassMemberReady advertised)
  assertBool "advertised member evidence cannot replace owner-local completion" (not (Readiness.requiredDeltaReady process delta (withAlignment onlyAdvertised)))
  preparedReady <-
    checkedIO
      ( Alignment.prepareLocalClassMemberReady
          generationId
          (AlignmentProtocol.classMemberReadyStoreIncarnation readiness)
          (AlignmentProtocol.classMemberReadyPredecessorAndBasePrefixDigest readiness)
          (AlignmentProtocol.classMemberReadyStoreRevision readiness)
          withoutReady
      )
  let completedWithoutAcceptance = fst (Alignment.commitLocalClassMemberReady preparedReady)
  assertBool "historical local completion without current plan acceptance remains pending" (not (Readiness.requiredDeltaReady process delta (withAlignment completedWithoutAcceptance)))
  accepted <- checkedIO (AlignmentProtocol.alignmentPlanAccepted (Plan.alignmentPlanId plan) local DomainAlignment.EmptyHeraldPublicationPrefix)
  (completed, _) <- checkedIO (Alignment.retainAlignmentPlanAcceptance accepted completedWithoutAcceptance)
  assertBool "current local completion and current plan acceptance make the same exact Delta ready" (Readiness.requiredDeltaReady process delta (withAlignment completed))
  let localIncomplete = Alignment.initializeTransferMemberWork local withoutReady
      localComplete = Alignment.initializeTransferMemberWork local alignment
  assertBool "local plan work blocks disappearance before readiness" (plan `elem` Alignment.unsettledAlignmentPlans localIncomplete)
  assertBool "remote member readiness is not an owner-local settlement requirement" (plan `notElem` Alignment.unsettledAlignmentPlans localComplete)
  emptyContext <- checkedIO (Context.deriveContextGraph [] [] [])
  emptyPreparation <- checkedIO (Plan.prepareAlignmentPlan sort (Plan.alignmentPlanTopologyCut plan) (Plan.alignmentPlanIdPlacement (Plan.alignmentPlanId plan)) emptyContext [] Nothing Set.empty)
  emptyPlan <- case emptyPreparation of
    Plan.AlignmentPlanReady value -> pure value
    Plan.AlignmentPlanHeld missing -> assertFailure ("empty plan unexpectedly held on " <> show missing)
  emptyPromotion <- checkedIO (Alignment.prepareAlignmentPlanPromotion local cause emptyPlan (fst (Alignment.commitStructuralDebtRetention debt)))
  let unknownEmptyOwner = fst (Alignment.commitAlignmentPromotion emptyPromotion)
      emptyOwner = Alignment.initializeTransferMemberWork local unknownEmptyOwner
  assertEqual "uninitialized owner retains an empty plan as unsettled" [emptyPlan] (Alignment.unsettledAlignmentPlans unknownEmptyOwner)
  assertEqual "plan acceptance remains necessary when there are no local members" [emptyPlan] (Alignment.unsettledAlignmentPlans emptyOwner)
  emptyAccepted <- checkedIO (AlignmentProtocol.alignmentPlanAccepted (Plan.alignmentPlanId emptyPlan) local DomainAlignment.EmptyHeraldPublicationPrefix)
  (emptySettled, _) <- checkedIO (Alignment.retainAlignmentPlanAcceptance emptyAccepted emptyOwner)
  assertEqual "accepted empty plan needs no member readiness" [] (Alignment.unsettledAlignmentPlans emptySettled)
  assertEqual "empty settled owner invariant" (Right ()) (Alignment.validateAlignmentState emptySettled)
  forM_ [structurallySettled, settled] $ \state -> assertEqual "fixture Herald invariant" (Right ()) (validateHeraldState state)
  assertPreparedEndpointQueries
    [ structurallySettled,
      settled,
      withAlignment withoutReady,
      withAlignment onlyAdvertised,
      withAlignment completedWithoutAcceptance,
      withAlignment completed
    ]
    process
    object
  where
    installRemotePlacement state member = do
      let members = Genesis.checkedActiveHeralds (startupGenesis state)
          original = Fixtures.fixtureDeploymentManifest
          manifest =
            original
              { Genesis.deploymentLocalHeraldId = Genesis.heraldMemberId member,
                Genesis.deploymentLocalHeraldEpoch = Genesis.heraldMemberEpoch member,
                Genesis.deploymentActiveHeralds = members,
                Genesis.deploymentOracleGenesis =
                  (Genesis.deploymentOracleGenesis original)
                    { Genesis.oracleGenesisActiveHeralds = members
                    }
              }
      genesis <- checkedIO (Genesis.checkHeraldGenesis manifest)
      (remote, _) <- checkedIO (initialHerald (monotonicInstant 0) genesis (Fixtures.fixtureCheckedInitialBootstrapsFor genesis) Fixtures.fixtureOracleContacts Fixtures.fixtureGeneratorSeed Fixtures.fixtureApplicationRecoveryConfiguration Fixtures.fixturePeerRecoveryConfiguration)
      localSnapshot <- checkedIO (Placement.prepareRetainedLocalSnapshot (startupPlacementState remote))
      let snapshot = Placement.localSnapshotMessage (snd (Placement.commitLocalSnapshot localSnapshot))
      prepared <- checkedIO (Placement.prepareRemotePlacement (PlacementProtocol.FullPlacementSnapshot snapshot) (startupPlacementState state))
      pure (replaceStartupPlacementState (fst (Placement.commitRemotePlacement prepared)) state)

openedSession :: EffectBatch -> IO (Session.ApplicationSessionBinding, Session.ApplicationSessionId)
openedSession effects = case [(Session.sessionAcceptanceBinding acceptance, session) | SetApplicationConnectionDisposition _ acceptance <- effectBatchMembers effects, Session.SessionOpened session _ _ <- [Session.sessionAcceptanceReply acceptance]] of
  [one] -> pure one
  _ -> assertFailure "expected one opened session"
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
checkedIO :: (Show problem) => Either problem value -> IO value
checkedIO = either (assertFailure . show) pure

-- A child's source writers already have label-release tenures. Fresh reader
-- objects published through them have no prior label decision of their own.
caseChildEnvironmentLabel :: Assertion
caseChildEnvironmentLabel = do
  parentFixture@(Fixture seed oracleBinding parentBinding parentSession _ parent) <- fixture
  parentAccess <- maybe (assertFailure "parent access") pure (Application.applicationBootstrapAccess parent (startupApplicationState seed))
  let parentEntries = Access.accessEntries (Application.bootstrapAccessPrimordial parentAccess)
      nablaKey = Access.environmentWriterKey Access.NablaRole
      deltaKey = Access.environmentWriterKey Access.DeltaRole
      selected = [(key, value) | key <- [nablaKey, deltaKey], Just value <- [Map.lookup key parentEntries]]
      selection = checked (Access.primordialSelection selected (Set.fromList [nablaKey, deltaKey]) (Just (nablaKey, deltaKey)))
  (prepared, oracle0, reference, child) <- prepare parentFixture selection
  let transfer (oracle, state) (ordinal, (_, entry)) = case entry of
        Access.Writer writer -> do
          let ingress = ApplicationRequestInput (CallApplicationRequest parentBinding parentSession (Request.requestId ordinal) (LabelApplication (Private.asPrivateObjectId (Private.privateNablaUniqueId writer)) ((ApplicationValue.ProcessLabel (Application.bootstrapAccessProcess parentAccess), 0)) (LabelToProcess (preparedChildProcess child))))
          (requested, _) <- step ingress state
          (openOracle, opened, _) <- commitNextLabelRequest (fromIntegral ordinal * 100) oracle requested
          (ready, _) <- step ingress (opened)
          (finishedOracle, finished, _) <- commitCompleteLabelWorkflow (fromIntegral ordinal * 100 + 1) openOracle ready
          pure (finishedOracle, finished)
        _ -> assertFailure "selected environment source must be writer"
  (oracle1, transferred) <- foldM transfer (oracle0, prepared) (zip [1, 2] selected)
  assertEqual "both source writers transferred" (Right Nothing) (Lifecycle.childReadiness reference transferred)
  let claim = checked (initialClaimId (Bytes.replicate 32 0xa1))
  (claimed, claimEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 61) (preparedChildConnection child) claim Nothing)) transferred
  (childBinding, childSession) <- openedSession claimEffects
  childEpoch <- childProcess reference claimed
  (pending, _) <- step (ApplicationRequestInput (CallApplicationRequest childBinding childSession (Request.requestId 1) NewEnvironmentApplication)) claimed
  withWiring <- settleStructuralPublication pending
  settled <- settleStructuralPublication withWiring
  (_, environmentEffects) <- step (ApplicationRequestInput (GetApplicationRequestResult childBinding childSession (Request.requestId 1))) settled
  environment <- case [value | SendApplicationReply _ (Request.RetainedRequestReply _ (Request.Completed _ (NewEnvironmentCompleted value))) <- effectBatchMembers environmentEffects] of
    [value] -> pure value
    _ -> assertFailure "expected completed child environment"
  childAccess <- maybe (assertFailure "child access") pure (Application.applicationBootstrapAccess childEpoch (startupApplicationState settled))
  reader <- case Access.environmentAccessPredefined environment of
    first : _ -> pure (Access.predefinedReader first)
    [] -> assertFailure "complete environment"
  let grandchildCall ordinal command = step (ApplicationLifecycleInput (CallApplicationLifecycle childBinding childSession (request childSession ordinal) locator command))
      grandchildStatus ordinal state = maybe (error "grandchild lifecycle result") Preparation.requestStatus (Preparation.lookupRequest (Preparation.lifecycleKey childEpoch (request childSession ordinal)) (startupProcessPreparationState state))
  (grandchildPending, grandchildEffects) <- grandchildCall 1 (BeginChild locator emptySelection) settled
  grandchildReference <- case grandchildStatus 1 grandchildPending of
    LifecycleCompleted (ChildPreparationAccepted value) -> pure value
    other -> assertFailure (show other)
  (grandchildStarted, oracle2, _) <- apply oracleBinding oracle1 grandchildEffects grandchildPending
  (grandchildNamed, _) <- grandchildCall 2 (AwaitPreparedChild grandchildReference) grandchildStarted
  grandchild <- case grandchildStatus 2 grandchildNamed of
    LifecycleCompleted (ChildPrepared value) -> pure value
    other -> assertFailure (show other)
  -- The source writers were transferred, but these newenv readers are fresh
  -- child-labelled objects and therefore start at generation zero.
  let labelIngress = ApplicationRequestInput (CallApplicationRequest childBinding childSession (Request.requestId 2) (LabelApplication (Private.asPrivateObjectId (Private.privateDeltaUniqueId reader)) ((ApplicationValue.ProcessLabel (Application.bootstrapAccessProcess childAccess), 0)) (LabelToProcess (preparedChildProcess grandchild))))
  sourceCompleted <- completeAssignedPeerPublications grandchildNamed
  (accepted, _) <- step labelIngress sourceCompleted
  (openOracle, opened, _) <- commitNextLabelRequest 300 oracle2 accepted
  (ready, _) <- step labelIngress (opened)
  (_, completed, _) <- commitCompleteLabelWorkflow 301 openOracle ready
  assertEqual "nested child label retains whole owner invariants" (Right ()) (validateHeraldState completed)
  assertBool "Oracle binding retained across nested child work" (Client.oracleClientCurrentBinding (startupOracleClientState completed) == Just oracleBinding)
