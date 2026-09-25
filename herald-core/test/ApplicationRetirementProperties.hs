{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}

module ApplicationRetirementProperties (tests) where

import ConfiguredProcessProperties (step)
import Control.Monad (foldM)
import Data.ByteString qualified as Bytes
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Client qualified as Client
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.Lifetime
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Application.Types.Operation (ApplicationOperation (NewIdApplication, ReadApplication, WaitApplication))
import Eclips.Application.Types.Query (ApplicationQuery (..), ApplicationQueryPredicate (QueryNever))
import Eclips.Herald.Application.RPC qualified as RPC
import Eclips.Herald.Application.RPC.Internal qualified as RpcTypes
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.EffectBatch
import Eclips.Herald.Input
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.ApplicationLiveness qualified as ApplicationLiveness
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Protocol.Application.Types qualified as Protocol
import Eclips.Public.Types.ReceiptRetirement qualified as Retirement
import GenesisFixtures qualified as Fixtures
import ProcessPreparationProperties (Fixture (..), apply, begin, call, emptySelection, fixture, prepare, request, status)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Property, ioProperty, testProperty)

tests :: TestTree
tests =
  testGroup
    "application retirement composition"
    [ testCase "same-connection empty acknowledgement seals the initial claim" caseInitialClaimConfirmation,
      testCase "pending lifecycle receipt rejects ordinary pruning and attached work atomically" casePendingLifecyclePinsComposition,
      testCase "consumed Begin receipt leaves child preparation independently executable" caseBeginReceiptOwnership,
      testCase "binding loss reclaims terminal receipts and observation waits without the serving driver" caseDirectBindingLossReclaimsReceipts,
      testProperty "permanent ordinary and lifecycle holes keep both receipt owners bounded" propPermanentHoles
    ]

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

openedSession :: EffectBatch -> IO (Session.ApplicationSessionBinding, Session.ApplicationSessionId)
openedSession effects = case [ (Session.sessionAcceptanceBinding acceptance, session)
                             | SetApplicationConnectionDisposition _ acceptance <- effectBatchMembers effects,
                               Session.SessionOpened session _ _ <- [Session.sessionAcceptanceReply acceptance]
                             ] of
  [one] -> pure one
  _ -> assertFailure "expected exactly one opened session"

caseInitialClaimConfirmation :: Assertion
caseInitialClaimConfirmation = do
  fixtureValue <- fixture
  (prepared, _, _, child) <- prepare fixtureValue emptySelection
  let claim = checked (initialClaimId (Bytes.replicate 32 0xb7))
      descriptor = preparedChildConnection child
  (claimed, openingEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 81) descriptor claim Nothing)) prepared
  (binding, session) <- openedSession openingEffects
  (confirmed, effects) <- step (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding session mempty Nothing)) claimed
  assertEqual
    "empty acknowledgement uses the existing binding"
    [SendApplicationReply binding (Request.ApplicationReceiptsRetired mempty)]
    (effectBatchMembers effects)
  assertEqual
    "confirmation preserves the established binding"
    (Just binding)
    (Application.applicationSessionCurrentBinding session (startupApplicationState confirmed))
  (retried, retryEffects) <- step (ApplicationSessionInput (ClaimInitialApplicationSession (candidateApplicationLane 82) descriptor claim Nothing)) confirmed
  assertBool
    "confirmed initial claim cannot be attached again"
    (RejectInitialClaim (candidateApplicationLane 82) claim StartupAlreadyClaimed `elem` effectBatchMembers retryEffects)
  assertBool
    "a rejected repeated claim cannot mint a second session"
    (startupApplicationState confirmed == startupApplicationState retried)
  assertEqual "confirmation leaves all owner invariants true" (Right ()) (validateHeraldState confirmed)

casePendingLifecyclePinsComposition :: Assertion
casePendingLifecyclePinsComposition = do
  Fixture initial oracleBinding binding session attachment process <- fixture
  (ordinary, _) <- step (ApplicationRequestInput (CallApplicationRequest binding session (Request.requestId 0) (NewIdApplication BareNewId))) initial
  let fixtureValue = Fixture ordinary oracleBinding binding session attachment process
  (begun, _, reference) <- begin fixtureValue emptySelection
  (pending, _) <- call fixtureValue 2 (AwaitPreparedChild reference) begun
  pendingStatus <- status fixtureValue 2 pending
  assertBool "fixture awaits its still-unapplied Start" (case pendingStatus of LifecyclePending _ -> True; _ -> False)
  let progress = applicationReceiptRetirement (Just 0) (Just 2)
      work = RetiringApplicationRequest (Request.requestId 1) (NewIdApplication BareNewId)
  (rejected, effects) <- step (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding session progress (Just work))) pending
  assertEqual
    "retirement emits one typed rejection and no semantic effects"
    [RejectApplicationConnection (EstablishedApplicationDisposition binding) Session.ApplicationReceiptRetirementNotReady]
    (effectBatchMembers effects)
  assertBool "ordinary receipt owner is unchanged" (startupApplicationState pending == startupApplicationState rejected)
  assertBool "lifecycle receipt and Begin provenance owners are unchanged" (startupProcessPreparationState pending == startupProcessPreparationState rejected)
  assertBool "attached newid never executes" (startupIdGeneratorState pending == startupIdGeneratorState rejected)
  assertBool "rejection leaves the complete predecessor unchanged" (pending == rejected)
  let exception = applicationReceiptRetirementWithExceptions mempty (checked (Retirement.receiptRetirement (Just 2) (Set.singleton 2)))
  (announced, _) <- step (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding session exception Nothing)) pending
  (rejectedAgain, repeatedEffects) <- step (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding session progress (Just work))) announced
  assertBool "removing a pending exception at the same high-water mark is checked atomically" (announced == rejectedAgain)
  assertEqual "same-high-water rejection emits no attached work" (effectBatchMembers effects) (effectBatchMembers repeatedEffects)

caseBeginReceiptOwnership :: Assertion
caseBeginReceiptOwnership = do
  fixtureValue@(Fixture _ oracleBinding binding session _ process) <- fixture
  (begun, startEffects, reference) <- begin fixtureValue emptySelection
  let progress = applicationReceiptRetirement Nothing (Just 1)
      key = Preparation.lifecycleKey process (request session 1)
  (retired, _) <- step (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding session progress Nothing)) begun
  assertEqual "Begin result is no longer retained" Nothing (Preparation.lookupRequest key (startupProcessPreparationState retired))
  assertBool "semantic preparation survives receipt consumption" (Preparation.lookupPreparation reference (startupProcessPreparationState retired) /= Nothing)
  (waiting, _) <- call fixtureValue 2 (AwaitPreparedChild reference) retired
  oracle <- either (assertFailure . show) pure (initialOracle Fixtures.fixtureCheckedOracleGenesis)
  (started, _, _) <- apply oracleBinding oracle startEffects waiting
  result <- status fixtureValue 2 started
  assertBool
    "later Start still completes the independently owned preparation"
    (case result of LifecycleCompleted (ChildPrepared child) -> preparedChildPreparation child == reference; _ -> False)
  assertEqual "semantic owner validates without the retired Begin receipt" (Right ()) (validateHeraldState started)

caseDirectBindingLossReclaimsReceipts :: Assertion
caseDirectBindingLossReclaimsReceipts = do
  fixtureValue@(Fixture _ _ binding session _ process) <- fixture
  (begun, _, reference) <- begin fixtureValue emptySelection
  (waitingPrepared, _) <- call fixtureValue 2 (AwaitPreparedChild reference) begun
  (waitingReady, _) <- call fixtureValue 3 (AwaitChildReady reference) waitingPrepared
  assertEqual "preparation observation is pending" (LifecyclePending []) =<< status fixtureValue 2 waitingReady
  assertEqual "readiness observation is pending" (LifecyclePending []) =<< status fixtureValue 3 waitingReady
  before <- maybe (assertFailure "preparation missing before loss") pure (Preparation.lookupPreparation reference (startupProcessPreparationState waitingReady))
  -- Call the liveness coordinator directly: isolated/read-only dispatch does
  -- not subsequently run the serving preparation driver.
  (expired, effects) <-
    either
      (assertFailure . show)
      pure
      (ApplicationLiveness.applyApplicationBindingLoss (startupLastObservedTime waitingReady) binding waitingReady)
  assertEqual "lost session is immediately expired" Nothing (Application.applicationSessionProcess session (startupApplicationState expired))
  mapM_
    (\ordinal -> assertEqual ("expired lifecycle receipt " <> show ordinal) Nothing (Preparation.lookupRequest (Preparation.lifecycleKey process (request session ordinal)) (startupProcessPreparationState expired)))
    [1, 2, 3]
  after <- maybe (assertFailure "accepted semantic preparation was discarded") pure (Preparation.lookupPreparation reference (startupProcessPreparationState expired))
  assertEqual "accepted Start still belongs to the preparation" (Preparation.preparationStartReference before) (Preparation.preparationStartReference after)
  assertEqual "receipt reclamation does not drive semantic preparation" (Preparation.preparationPhase before) (Preparation.preparationPhase after)
  assertEqual "only session disposition transfers to the lost application" [session] [ended | DisposeApplicationSession ended _ <- effectBatchMembers effects]
  assertEqual "abandoned observations do not synthesize terminal replies" [] [reply | SendApplicationLifecycleReply _ reply <- effectBatchMembers effects]
  assertEqual "direct cleanup preserves owner invariants" (Right ()) (validateHeraldState expired)
  (repeated, repeatedEffects) <-
    either
      (assertFailure . show)
      pure
      (ApplicationLiveness.applyApplicationBindingLoss (startupLastObservedTime expired) binding expired)
  assertBool "duplicate loss is state-exact" (expired == repeated)
  assertEqual "duplicate loss emits no effects" [] (effectBatchMembers repeatedEffects)

-- Drive the real client and Herald through their DTO adapters. Runtime FIFO
-- result transfer is represented by consuming each client effect batch before
-- passing its next send to the server; idle timers are fired explicitly.
propPermanentHoles :: Word8 -> Property
propPermanentHoles seed = ioProperty $ do
  Fixture initial _ binding session attachment process <- fixture
  let recovery = checked (Client.applicationClientRecoveryConfiguration 10 1000)
      client0 = Client.initialApplicationClient recovery (checked (RpcTypes.attachmentToClaim attachment)) (Protocol.applicationClientNonce 10)
      (started, _) = clientStep Client.Start client0
      (opening, _) = clientStep (Client.TransportAvailable (clientAttempt started) clientNow) started
  (_, acceptance) <- step (ApplicationSessionInput (OpenApplicationSession (candidateApplicationLane 91) attachment (Session.clientNonce 10))) initial
  opened <- consumeServerEffects opening acceptance
  waiting <- exchange binding (Client.Submit (Client.applicationInvocationId 0) (WaitApplication [neverQuery])) (opened, initial)
  begun@(afterBegin, _) <- exchange binding (Client.SubmitLifecycle (Client.applicationInvocationId 1) (BeginChild testLocator emptySelection)) waiting
  -- Begin(1) creates the same checked preparation identity as its request.
  let beginRequest = request session 1
      reference = checked (childPreparation (lifecycleRequestIdScopeBytes beginRequest) (lifecycleRequestIdSessionOrdinal beginRequest) 1)
  assertEqual "Begin terminal result left the client map" (1, 0, 2) (Client.applicationClientRetentionCounts afterBegin)
  withHoles <- exchange binding (Client.SubmitLifecycle (Client.applicationInvocationId 2) (AwaitPreparedChild reference)) begun
  let count = 1 + fromIntegral (seed `mod` 48) :: Word64
      unknown = checked (childPreparation (lifecycleRequestIdScopeBytes beginRequest) (lifecycleRequestIdSessionOrdinal beginRequest) 999)
      one pair index = do
        readDone <- exchange binding (Client.Submit (Client.applicationInvocationId (3 + 2 * index)) (ReadApplication neverQuery)) pair
        lifecycleDone <- exchange binding (Client.SubmitLifecycle (Client.applicationInvocationId (4 + 2 * index)) (AwaitChildReady unknown)) readDone
        flushed@(client, herald) <- exchange binding (Client.ReceiptRetirementFlush (clientAttempt (fst lifecycleDone))) lifecycleDone
        let expected =
              applicationReceiptRetirementWithExceptions
                (checked (Retirement.receiptRetirement (Just (index + 1)) (Set.singleton 0)))
                (checked (Retirement.receiptRetirement (Just (4 + 2 * index)) (Set.singleton 2)))
        assertEqual "client storage depends only on the two outstanding calls" (1, 1, 2) (Client.applicationClientRetentionCounts client)
        assertEqual "ordinary server keeps only wait zero" [Request.requestId 0] (map fst (Application.applicationRequestEntries session (startupApplicationState herald)))
        assertEqual "lifecycle server keeps only the accepted observation" [Preparation.lifecycleKey process (request session 2)] (map fst (Preparation.requestEntries (startupProcessPreparationState herald)))
        assertEqual "both owners agree on high-water marks and unresolved exceptions" (Just expected) (Application.applicationReceiptRetirementProgress session (startupApplicationState herald))
        assertEqual "client consumed only completed results" expected (Client.applicationClientReceiptRetirement client)
        assertEqual "reply cursor accounting remains valid around holes" (Right ()) (validateHeraldState herald)
        pure flushed
  (client, herald) <- foldM one withHoles [0 .. count - 1]
  let sessionClaim = checked (RpcTypes.sessionToClaim session)
      stale =
        applicationReceiptRetirementWithExceptions
          (checked (Retirement.receiptRetirement (Just count) (Set.fromList [0, 1])))
          (checked (Retirement.receiptRetirement (Just (2 + 2 * count)) (Set.fromList [2, 4])))
  (afterStale, staleEffects) <- step (ApplicationReceiptRetirementInput (RetireApplicationReceipts binding session stale Nothing)) herald
  assertBool "an exception cannot reopen a released ordinary or lifecycle result" (herald == afterStale)
  clientConfirmed <- consumeServerEffects client staleEffects
  assertEqual "stale acknowledgement cannot change client consumption" (Client.applicationClientReceiptRetirement client) (Client.applicationClientReceiptRetirement clientConfirmed)
  retainedInput <- either (assertFailure . show) pure (RPC.admitApplicationClientDto (RPC.EstablishedApplicationIngress binding) (Protocol.GetRequestResult sessionClaim (Protocol.applicationRequestIdClaim 0)))
  (_, retainedEffects) <- step retainedInput afterStale
  clientQueried <- consumeServerEffects clientConfirmed retainedEffects
  assertEqual "querying the unresolved exception preserves cursor history" clientConfirmed clientQueried
  cancelled <- exchange binding (Client.Cancel (Client.applicationInvocationId 0)) (clientQueried, afterStale)
  (finished, reclaimed) <- exchange binding (Client.ReceiptRetirementFlush (clientAttempt (fst cancelled))) cancelled
  assertEqual "later cancellation releases the exceptional receipt" [] (Application.applicationRequestEntries session (startupApplicationState reclaimed))
  assertEqual "only the lifecycle obligation remains live" (0, 1, 1) (Client.applicationClientRetentionCounts finished)
  assertEqual "cancellation and hole removal preserve Herald invariants" (Right ()) (validateHeraldState reclaimed)
  (_, oldEffects) <- step (ApplicationRequestInput (GetApplicationRequestResult binding session (Request.requestId 0))) reclaimed
  assertBool "old exception lookup is now explicitly retired" (SendApplicationReply binding (Request.RequestRetired (Request.requestId 0) count) `elem` effectBatchMembers oldEffects)
  pure True

neverQuery :: ApplicationQuery
neverQuery = ApplicationQuery Set.empty QueryNever

testLocator :: HeraldLocator
testLocator = checked (heraldLocator "localhost" 35001)

clientNow :: Client.ApplicationClientMonotonicInstant
clientNow = Client.applicationClientMonotonicInstant 0

clientAttempt :: Client.ApplicationClientState -> Client.ApplicationTransportAttemptGeneration
clientAttempt = maybe (error "fixture client has no established attempt") id . Client.applicationClientCurrentTransportAttempt

clientStep :: Client.ApplicationClientInput -> Client.ApplicationClientState -> (Client.ApplicationClientState, [Client.ApplicationClientEffect])
clientStep input state = case checked (Client.stepApplicationClient input state) of
  Client.ApplicationClientAdvanced successor effects -> (successor, Client.applicationClientEffects effects)
  other -> error (show other)

consumeServerEffects :: Client.ApplicationClientState -> EffectBatch -> IO Client.ApplicationClientState
consumeServerEffects initial effects = foldM consume initial (effectBatchMembers effects)
  where
    consume client effect = case checked (RPC.projectApplicationEffect effect) of
      Nothing -> pure client
      Just (RPC.AcceptApplicationCandidate _ _ dto) -> receive client dto
      Just (RPC.SendEstablishedApplication _ dto) -> receive client dto
      other -> assertFailure ("unexpected application projection " <> show other)
    receive client dto = do
      let (successor, emitted) = clientStep (Client.ServerDtoReceived (clientAttempt client) clientNow dto) client
      assertEqual "receiving a fixture result needs no semantic resend" [] [send | Client.SendApplicationDto send <- emitted]
      pure successor

exchange :: Session.ApplicationSessionBinding -> Client.ApplicationClientInput -> (Client.ApplicationClientState, HeraldState) -> IO (Client.ApplicationClientState, HeraldState)
exchange binding input (client, herald) = do
  let (pending, effects) = clientStep input client
  foldM send (pending, herald) [dto | Client.SendApplicationDto dto <- effects]
  where
    send (pending, current) dto = do
      body <- either (assertFailure . show) pure (RPC.admitApplicationClientDto (RPC.EstablishedApplicationLifecycleIngress binding testLocator) dto)
      (successor, effects) <- step body current
      consumed <- consumeServerEffects pending effects
      pure (consumed, successor)
