module ReceiptRetirementProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Client qualified as Client
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Identity
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.Lifetime
import Eclips.Application.Types.Operation (ApplicationOperation (ReadApplication))
import Eclips.Application.Types.Query (ApplicationQuery (..), ApplicationQueryPredicate (QueryAlways))
import Eclips.Application.Types.Result (RegularCallResult (ReadCompleted))
import Eclips.Protocol.Application.Types qualified as Protocol
import Eclips.Public.Types.ReceiptRetirement qualified as Retirement
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (NonNegative (..), Property, conjoin, counterexample, forAll, shuffle, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "application receipt retirement"
    [ testProperty "flowing requests carry the latest consumed prefix and keep constant client history" propFlowing,
      testProperty "out-of-order terminal results release later receipts while preserving pending holes" propCompletionOrder,
      testProperty "mixed namespaces retain independent frontiers under arbitrary completion and duplicate order" propMixedCompletionOrder,
      testProperty "mixed flowing calls piggyback both sparse lifecycle and ordinary progress" propMixedFlowing,
      testCase "one idle flush coalesces terminal results and confirmation stops it" caseIdleFlush,
      testCase "fresh invocations increase strictly and retired cancellation is typed" caseInvocationFloor,
      testCase "retirement confirmation cannot exceed locally consumed results" caseInvalidConfirmation,
      testCase "ordinary and lifecycle consumption have independent prefixes" caseIndependentFamilies,
      testCase "connection loss settles pending work and makes delayed traffic inert" caseTerminalLoss
    ]

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

scope :: Bytes.ByteString
scope = Bytes.replicate 32 0x61

session :: Protocol.ApplicationSessionClaim
session = checked (Protocol.applicationSessionClaim scope 1)

request :: Word64 -> Protocol.ApplicationRequestIdClaim
request = Protocol.applicationRequestIdClaim

cursor :: Word64 -> Protocol.ApplicationReplyCursorClaim
cursor = checked . Protocol.applicationReplyCursorClaim

invocation :: Word64 -> Client.ApplicationInvocationId
invocation = Client.applicationInvocationId

operation :: ApplicationOperation
operation = ReadApplication (ApplicationQuery Set.empty QueryAlways)

active :: Client.ApplicationClientState
active = fst (server (Protocol.SessionOpened (cursor 1) session token startup) opening)
  where
    initial = Client.initialApplicationClient recovery attachment (Protocol.applicationClientNonce 7)
    recovery = checked (Client.applicationClientRecoveryConfiguration 10 1000)
    attachment = checked (Protocol.applicationAttachmentClaim scope)
    (started, _) = advance Client.Start initial
    (opening, _) = advance (Client.TransportAvailable (attempt started) (Client.applicationClientMonotonicInstant 0)) started
    token = checked (Protocol.applicationResumeTokenClaim scope 1)
    startup =
      Access.applicationStartupAccess
        (Identity.asPrivateProcessId (checked (Identity.mkPrivateUniqueId 1)))
        (checked (Access.primordialAccess [] Set.empty Nothing))

attempt :: Client.ApplicationClientState -> Client.ApplicationTransportAttemptGeneration
attempt = maybe (error "missing transport") id . Client.applicationClientCurrentTransportAttempt

advance :: Client.ApplicationClientInput -> Client.ApplicationClientState -> (Client.ApplicationClientState, [Client.ApplicationClientEffect])
advance input state = case checked (Client.stepApplicationClient input state) of
  Client.ApplicationClientAdvanced successor effects -> (successor, Client.applicationClientEffects effects)
  other -> error (show other)

server :: Protocol.ApplicationServerDto -> Client.ApplicationClientState -> (Client.ApplicationClientState, [Client.ApplicationClientEffect])
server dto state = advance (Client.ServerDtoReceived (attempt state) (Client.applicationClientMonotonicInstant 1) dto) state

complete :: Word64 -> Word64 -> Client.ApplicationClientState -> (Client.ApplicationClientState, [Client.ApplicationClientEffect])
complete word position = server (Protocol.RequestRetained session (cursor position) (request word) (Protocol.Completed (ReadCompleted [])))

sent :: [Client.ApplicationClientEffect] -> [Protocol.ApplicationClientDto]
sent effects = [dto | Client.SendApplicationDto dto <- effects]

schedules :: [Client.ApplicationClientEffect] -> Int
schedules effects = length [() | Client.ScheduleReceiptRetirement {} <- effects]

propFlowing :: NonNegative Int -> Property
propFlowing (NonNegative raw) =
  let count = fromIntegral (raw `mod` 128 + 1)
      (_, checks) = foldl' step (active, []) [0 .. count - 1]
   in conjoin checks
  where
    step (state, checks) word =
      let (pending, effects) = advance (Client.Submit (invocation word) operation) state
          progress = applicationReceiptRetirement (if word == 0 then Nothing else Just (word - 1)) Nothing
          expected =
            if word == 0
              then Protocol.Call session (request word) operation
              else Protocol.CallWithRetirement session (request word) operation progress
          (settled, resultEffects) = complete word (word + 2) pending
          result = [value | Client.CallFinished _ value <- resultEffects]
          test =
            counterexample (show (word, effects, resultEffects))
              $ conjoin
                [ sent effects === [expected],
                  result === [Client.ApplicationCallSucceeded (ReadCompleted [])],
                  Client.applicationClientRetentionCounts settled === (0, 0, 1),
                  Client.applicationClientReceiptRetirement settled === applicationReceiptRetirement (Just word) Nothing,
                  Client.validateApplicationClientState settled === Right ()
                ]
       in (settled, checks <> [test])

propCompletionOrder :: NonNegative Int -> Property
propCompletionOrder (NonNegative raw) =
  let count = fromIntegral (raw `mod` 32 + 1)
      identities = [0 .. count - 1]
      submitted = foldl' (\state word -> fst (advance (Client.Submit (invocation word) operation) state)) active identities
   in forAll (shuffle identities) $ \order ->
        let (_, _, checks) = foldl' step (submitted, identities, []) (zip [2 ..] order)
         in conjoin checks
  where
    step (state, remaining, checks) (position, word) =
      let (settled, _) = complete word position state
          pending = filter (/= word) remaining
          high = ordinaryReceiptRetirementThrough (Client.applicationClientReceiptRetirement state)
          expected = applicationReceiptRetirementWithExceptions (checked (Retirement.receiptRetirement high (Set.fromList pending))) mempty
          (retained, _, observations) = Client.applicationClientRetentionCounts settled
          test =
            counterexample (show (word, pending))
              $ conjoin
                [ Client.applicationClientReceiptRetirement settled === expected,
                  retained === length pending,
                  observations === 1
                ]
       in (settled, pending, checks <> [test])

caseIdleFlush :: IO ()
caseIdleFlush = do
  let (pending0, submitEffects) = advance (Client.Submit (invocation 0) operation) active
      (settled0, effects0) = complete 0 2 pending0
      (pending1, dispatch) = advance (Client.Submit (invocation 1) operation) settled0
      (settled1, effects1) = complete 1 3 pending1
      progress = applicationReceiptRetirement (Just 1) Nothing
      (flushing, flushEffects) = advance (Client.ReceiptRetirementFlush (attempt settled1)) settled1
      (confirmed, ackEffects) = server (Protocol.ReceiptsRetired session progress) flushing
      (_, staleTimerEffects) = advance (Client.ReceiptRetirementFlush (attempt confirmed)) confirmed
  assertEqual "one timer covers allocation and result transfer" 1 (schedules submitEffects + schedules effects0)
  assertEqual "existing timer is not postponed by later completions" 0 (schedules effects1)
  assertEqual "flow carries previous result" [Protocol.CallWithRetirement session (request 1) operation (applicationReceiptRetirement (Just 0) Nothing)] (sent dispatch)
  assertEqual "idle flush reads latest progress" [Protocol.RetireReceipts session progress] (sent flushEffects)
  assertEqual "confirmation is quiet" [] ackEffects
  assertEqual "late duplicate timer sends nothing" [] staleTimerEffects
  assertEqual "no history remains after idle acknowledgement" (0, 0, 1) (Client.applicationClientRetentionCounts confirmed)

caseInvocationFloor :: IO ()
caseInvocationFloor = do
  let (pending, _) = advance (Client.Submit (invocation 10) operation) active
      (settled, _) = complete 0 2 pending
      rejected value = Client.stepApplicationClient (Client.Submit (invocation value) operation) settled
  assertEqual "old invocation cannot execute again" (Right (Client.ApplicationClientCommandRejected (Client.ApplicationInvocationAlreadyUsed (invocation 10)))) (rejected 10)
  assertEqual "unused older invocation cannot bypass the compact floor" (Right (Client.ApplicationClientCommandRejected (Client.ApplicationInvocationAlreadyUsed (invocation 5)))) (rejected 5)
  assertEqual "cancel identifies local retirement" (Right (Client.ApplicationClientCommandRejected (Client.ApplicationInvocationRetired (invocation 10)))) (Client.stepApplicationClient (Client.Cancel (invocation 10)) settled)
  assertEqual "all completed routing state reclaimed" (0, 0, 1) (Client.applicationClientRetentionCounts settled)

caseInvalidConfirmation :: IO ()
caseInvalidConfirmation = do
  let (pending, _) = advance (Client.Submit (invocation 0) operation) active
      progress = applicationReceiptRetirement (Just 0) Nothing
      input = Client.ServerDtoReceived (attempt pending) (Client.applicationClientMonotonicInstant 1) (Protocol.ReceiptsRetired session progress)
  assertEqual "pending result is not retireable" (Left (Client.ApplicationClientInvalidReceiptRetirement progress)) (Client.stepApplicationClient input pending)

caseIndependentFamilies :: IO ()
caseIndependentFamilies = do
  let (ordinaryPending, _) = advance (Client.Submit (invocation 0) operation) active
      (lifecyclePending, _) = advance (Client.SubmitLifecycle (invocation 1) Lifecycle.EndOwnProcess) ordinaryPending
      lifecycleRequest = checked (Lifecycle.lifecycleRequestId scope 1 1)
      status = Lifecycle.LifecycleRejected Lifecycle.LifecycleTargetUnavailable
      (lifecycleDone, _) = server (Protocol.LifecycleReply lifecycleRequest status) lifecyclePending
      lifecycleOnly = applicationReceiptRetirementWithExceptions (checked (Retirement.receiptRetirement (Just 0) (Set.singleton 0))) (Retirement.receiptRetirementPrefix (Just 1))
      (allDone, _) = complete 0 2 lifecycleDone
  assertEqual "ordinary hole does not pin lifecycle receipts" lifecycleOnly (Client.applicationClientReceiptRetirement lifecycleDone)
  assertEqual "only pending ordinary routing remains" (1, 0, 1) (Client.applicationClientRetentionCounts lifecycleDone)
  assertEqual "both results eventually consumed" (applicationReceiptRetirement (Just 0) (Just 1)) (Client.applicationClientReceiptRetirement allDone)

caseTerminalLoss :: IO ()
caseTerminalLoss = do
  let (pending, _) = advance (Client.Submit (invocation 0) operation) active
      generation = attempt pending
      (lost, effects) = advance (Client.TransportLost generation (Client.applicationClientMonotonicInstant 2)) pending
      (late, lateEffects) = advance (Client.ServerDtoReceived generation (Client.applicationClientMonotonicInstant 3) (Protocol.RequestRetained session (cursor 2) (request 0) (Protocol.Completed (ReadCompleted [])))) lost
  assertEqual
    "unresolved result has typed terminal uncertainty"
    [Client.CallUnknown (invocation 0) Client.SessionNoLongerLive, Client.HeraldPermanentlyUnavailableObserved Client.SessionNoLongerLive, Client.CloseTransport]
    effects
  assertEqual "no pending or completed correlations kept" (0, 0, 1) (Client.applicationClientRetentionCounts lost)
  assertEqual "late completion cannot revive the owner" lost late
  assertBool "late completion emits no effects" (null lateEffects)

-- The reference retains every delivered result deliberately. The production
-- owner must agree while keeping only pending work, including when a low pending
-- request pins the remote prefix and higher terminal records have disappeared.
data ModelWork
  = OrdinaryWork Word64 Word64
  | LifecycleWork Word64
  deriving stock (Eq, Show)

modelWorks :: [NonNegative Int] -> [ModelWork]
modelWorks samples = third (foldl' add (0, 0, []) (NonNegative 0 : NonNegative 1 : take 24 samples))
  where
    add (nextInvocation, nextOrdinary, works) (NonNegative value) =
      let invocationWord = nextInvocation + fromIntegral (value `mod` 3)
          ordinary = even value
          work = if ordinary then OrdinaryWork nextOrdinary invocationWord else LifecycleWork invocationWord
       in (invocationWord + 1, nextOrdinary + if ordinary then 1 else 0, works <> [work])
    third (_, _, value) = value

modelLifecycleRequest :: Word64 -> Lifecycle.LifecycleRequestId
modelLifecycleRequest word = checked (Lifecycle.lifecycleRequestId scope 1 word)

modelLifecycleCommand :: Lifecycle.LifecycleCommand
modelLifecycleCommand = Lifecycle.AwaitChildReady (checked (Lifecycle.childPreparation scope 1 42))

submitWork :: ModelWork -> Client.ApplicationClientInput
submitWork = \case
  OrdinaryWork _ word -> Client.Submit (invocation word) operation
  LifecycleWork word -> Client.SubmitLifecycle (invocation word) modelLifecycleCommand

modelCompletion :: Word64 -> ModelWork -> (Word64, Protocol.ApplicationServerDto)
modelCompletion prior = \case
  OrdinaryWork word _ ->
    (prior + 1, Protocol.RequestRetained session (cursor (prior + 1)) (request word) (Protocol.Completed (ReadCompleted [])))
  LifecycleWork word ->
    (prior, Protocol.LifecycleReply (modelLifecycleRequest word) (Lifecycle.LifecycleRejected Lifecycle.LifecycleUnknownPreparation))

modelProgress :: [ModelWork] -> [ModelWork] -> ApplicationReceiptRetirement
modelProgress admitted pending = applicationReceiptRetirementWithExceptions ordinary lifecycle
  where
    ordinaryWords = [word | OrdinaryWork word _ <- admitted]
    pendingOrdinary = [word | OrdinaryWork word _ <- pending]
    lifecycleWords = [word | LifecycleWork word <- admitted]
    pendingLifecycle = [word | LifecycleWork word <- pending]
    ordinary = checked (Retirement.receiptRetirement (greatest ordinaryWords) (Set.fromList pendingOrdinary))
    lifecycle = checked (Retirement.receiptRetirement (greatest lifecycleWords) (Set.fromList pendingLifecycle))
    greatest [] = Nothing
    greatest values = Just (maximum values)

propMixedCompletionOrder :: [NonNegative Int] -> Property
propMixedCompletionOrder samples =
  let works = modelWorks samples
      submitted = foldl' (\state work -> fst (advance (submitWork work) state)) active works
   in forAll (shuffle works) $ \order ->
        let (_, _, _, _, checks) = foldl' (step works) (submitted, 1, works, [], []) order
         in conjoin checks
  where
    step admitted (state, priorCursor, remaining, delivered, checks) work =
      let (nextCursor, dto) = modelCompletion priorCursor work
          (settled, effects) = server dto state
          pending = filter (/= work) remaining
          progress = modelProgress admitted pending
          replies = dto : delivered
          replayChecks =
            [ let (after, emitted) = server previous settled
               in counterexample ("duplicate " <> show previous) (after == settled && null emitted)
            | previous <- reverse replies
            ]
          (confirmed, _) = server (Protocol.ReceiptsRetired session progress) settled
          (oldAckState, oldAckEffects) = server (Protocol.ReceiptsRetired session (Client.applicationClientReceiptRetirement state)) confirmed
          expectedCounts = (length [() | OrdinaryWork {} <- pending], length [() | LifecycleWork {} <- pending], 1)
          completions = [effect | effect <- effects, case effect of Client.CallFinished {} -> True; Client.LifecycleChanged {} -> True; _ -> False]
          expectedCompletion = case work of
            OrdinaryWork _ word -> Client.CallFinished (invocation word) (Client.ApplicationCallSucceeded (ReadCompleted []))
            LifecycleWork word -> Client.LifecycleChanged (invocation word) (Lifecycle.LifecycleRejected Lifecycle.LifecycleUnknownPreparation)
          test =
            counterexample (show (work, pending, effects))
              $ conjoin
                ( [ Client.applicationClientReceiptRetirement settled === progress,
                    Client.applicationClientRetentionCounts settled === expectedCounts,
                    completions === [expectedCompletion],
                    counterexample "regressed acknowledgement changed state" (oldAckState == confirmed && null oldAckEffects),
                    Client.validateApplicationClientState confirmed === Right ()
                  ]
                    <> replayChecks
                )
       in (confirmed, nextCursor, pending, replies, checks <> [test])

propMixedFlowing :: [NonNegative Int] -> Property
propMixedFlowing samples =
  let (_, _, _, checks) = foldl' step (active, 1, [], []) (modelWorks samples)
   in conjoin checks
  where
    step (state, priorCursor, admitted, checks) work =
      let progress = modelProgress admitted []
          (pending, effects) = advance (submitWork work) state
          expected = case work of
            OrdinaryWork word _ ->
              if progress == mempty
                then Protocol.Call session (request word) operation
                else Protocol.CallWithRetirement session (request word) operation progress
            LifecycleWork word ->
              if progress == mempty
                then Protocol.LifecycleCall session (modelLifecycleRequest word) modelLifecycleCommand
                else Protocol.LifecycleCallWithRetirement session (modelLifecycleRequest word) modelLifecycleCommand progress
          (nextCursor, dto) = modelCompletion priorCursor work
          (settled, _) = server dto pending
          test =
            counterexample (show (work, effects, progress))
              $ conjoin
                [ sent effects === [expected],
                  Client.applicationClientReceiptRetirement settled === modelProgress (admitted <> [work]) [],
                  Client.applicationClientRetentionCounts settled === (0, 0, 1)
                ]
       in (settled, nextCursor, admitted <> [work], checks <> [test])
