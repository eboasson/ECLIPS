module ClientProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Client hiding
  ( ServerDtoReceived,
    TransportAvailable,
    TransportLost,
    stepApplicationClient,
  )
import Eclips.Application.Client qualified as Client
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (SortDefinitionRole),
    ApplicationStartupAccess,
    EnvironmentAccess,
    allApplicationPredefinedSortRoles,
    applicationStartupAccess,
    environmentAccess,
    predefinedAccess,
    primordialAccessFromSelection,
    selectEnvironment,
  )
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateObjectId,
    PrivateUniqueId,
    SortId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    mkPrivateUniqueId,
    mkSortId,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToProcess, LabelToVoid),
    LabelResult (..),
  )
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Application.Types.Result
  ( OperationPendingReason (..),
    RegularCallResult (..),
    WaitResult (..),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationSortDefinition (PredefinedSortDefinition),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (..),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
import Eclips.Protocol.Application.Types
import Eclips.Protocol.Application.Types qualified as Protocol
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( NonNegative (..),
    Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )

data CurrentTransportObservation
  = TransportAvailable
  | TransportLost

newtype CurrentServerObservation
  = ServerDtoReceived ApplicationServerDto

class TestClientInput input where
  currentClientInput ::
    ApplicationClientState ->
    input ->
    ApplicationClientInput

instance TestClientInput ApplicationClientInput where
  currentClientInput _ = id

instance TestClientInput CurrentTransportObservation where
  currentClientInput state = \case
    TransportAvailable ->
      Client.TransportAvailable (currentAttempt state) (instant 101)
    TransportLost ->
      Client.TransportLost (currentAttempt state) (instant 100)

instance TestClientInput CurrentServerObservation where
  currentClientInput state (ServerDtoReceived dto) =
    Client.ServerDtoReceived (currentAttempt state) (instant 101) dto

stepApplicationClient ::
  (TestClientInput input) =>
  input ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
stepApplicationClient input state =
  Client.stepApplicationClient (currentClientInput state input) state

currentAttempt :: ApplicationClientState -> ApplicationTransportAttemptGeneration
currentAttempt state =
  fromMaybe fixtureFirstAttempt (applicationClientCurrentTransportAttempt state)

fixtureFirstAttempt :: ApplicationTransportAttemptGeneration
fixtureFirstAttempt =
  case applicationClientCurrentTransportAttempt fixtureOpening of
    Just attempt -> attempt
    Nothing -> error "opening fixture has no transport attempt"

instant :: Word64 -> ApplicationClientMonotonicInstant
instant = applicationClientMonotonicInstant

tests :: TestTree
tests =
  testGroup
    "pure application client"
    [ testCase "recovery configuration requires positive microsecond durations" caseRecoveryConfiguration,
      testCase "open loss paces the exact retained nonce without a terminal grace" caseOpenRetry,
      testCase "current submission helpers construct the exact eight-operation calls" caseSubmissionHelpers,
      testCase "all eight operation results pair payload-exactly" caseCurrentResultMatrix,
      testCase "all three pending states advance to their operation-specific terminals" caseStructuralPendingResults,
      testCase "request allocation is gap-free and invocation reuse is rejected" caseAllocationAndReuse,
      testCase "wrong-phase submission consumes neither correlation" caseWrongPhaseSubmission,
      testCase "a new pending status cannot regress a terminal request" caseNewPendingAfterTerminal,
      testCase "an accepted wait cannot later become rejected" caseRejectedAfterWaitAccepted,
      testCase "a retained request observation owns one cursor" caseRequestObservationCursorUniqueness,
      testCase "same-cursor duplicates are idempotent and conflicts fault" caseCursorDuplicate,
      testCase "cancel before wait acceptance converges on retained cancellation" caseCancellationBeforeAcceptance,
      testCase "cancel after wait acceptance can still lose to a wake" caseCancellationAfterAcceptance,
      testCase "wake before cancellation completes exactly once" caseWakeBeforeCancellation,
      testCase "absence resends and conflict faults" caseRequestBookkeeping,
      testCase "foreign and unknown correlations fault" caseCorrelationFaults,
      testCase "explicit permanent loss reports unknown outcomes in request order" casePermanentLoss,
      testCase "terminal session disposition settles the current lane without redial" caseTerminalSessionDisposition,
      testCase "isolation notice admits only local reads before the isolated terminal" caseIsolationLifecycle,
      testCase "separate client owners isolate equal local request correlations" caseClientIsolation,
      testCase "wrong-phase server messages fault while terminal late messages are inert" caseServerPhaseMatrix,
      testCase "opening admits only the attachment rejection category" caseOpeningRejectionMatrix,
      testProperty "equal state and input have an equal transition" propDeterministic,
      testProperty "generated stale transport observations are exact no-ops" propStaleTransportObservations,
      testProperty "generated submissions retain a contiguous request prefix" propGeneratedAllocation
    ]

caseRecoveryConfiguration :: Assertion
caseRecoveryConfiguration = do
  assertEqual
    "zero retry delay"
    (Left ApplicationClientRetryDelayMustBePositive)
    (applicationClientRecoveryConfiguration 0 1)
  assertEqual
    "zero recovery grace"
    (Left ApplicationClientRecoveryGraceMustBePositive)
    (applicationClientRecoveryConfiguration 1 0)
  let configuration = checked (applicationClientRecoveryConfiguration 7 19)
  assertEqual "retry microseconds" 7 (applicationClientRetryDelayMicroseconds configuration)
  assertEqual "grace microseconds" 19 (applicationClientRecoveryGraceMicroseconds configuration)

caseSubmissionHelpers :: Assertion
caseSubmissionHelpers = do
  let invocation = applicationInvocationId 80
      writer = asPrivateNablaId (privateId 8)
      object = asPrivateObjectId (privateId 9)
      target = asPrivateProcessId (privateId 10)
  assertEqual
    "bare newid helper"
    (Submit invocation (NewIdApplication BareNewId))
    (submitNewId invocation BareNewId)
  assertEqual
    "controlled newid helper"
    (Submit invocation (NewIdApplication (ControlledNewId writer)))
    (submitNewId invocation (ControlledNewId writer))
  assertEqual
    "delete write helper"
    (Submit invocation (WriteApplication writer (DeleteReserved object)))
    (submitWrite invocation writer (DeleteReserved object))
  assertEqual
    "sort-definition write helper"
    (Submit invocation (WriteApplication writer (PublishValue (SortDefinitionValue fixtureDefinition))))
    (submitWrite invocation writer (PublishValue (SortDefinitionValue fixtureDefinition)))
  assertEqual
    "generic value write helper"
    (Submit invocation (WriteApplication writer (PublishValue (BoolValue True))))
    (submitWrite invocation writer (PublishValue (BoolValue True)))
  assertEqual
    "forward helper"
    (Submit invocation (ForwardApplication writer object))
    (submitForward invocation writer object)
  assertEqual
    "read helper"
    (Submit invocation (ReadApplication fixtureQuery))
    (submitRead invocation fixtureQuery)
  assertEqual
    "local-take helper"
    (Submit invocation (LocalTakeApplication fixtureQuery))
    (submitLocalTake invocation fixtureQuery)
  assertEqual
    "wait helper"
    (Submit invocation (WaitApplication [fixtureQuery]))
    (submitWait invocation [fixtureQuery])
  assertEqual
    "label helper carries the exact expected-label CAS operand"
    ( Submit
        invocation
        (LabelApplication object (ZombieLabel target, 7) (LabelToProcess target))
    )
    (submitLabel invocation object (ZombieLabel target, 7) (LabelToProcess target))
  assertEqual
    "new-environment helper"
    (Submit invocation NewEnvironmentApplication)
    (submitNewEnvironment invocation)

caseCurrentResultMatrix :: Assertion
caseCurrentResultMatrix =
  mapM_
    ( \(operation, matchingIndexes) ->
        mapM_
          (assertPair operation matchingIndexes)
          (zip [0 :: Int ..] currentResults)
    )
    currentOperations
  where
    assertPair operation matchingIndexes (resultIndex, result) = do
      let (withCall, _) =
            advance
              (Submit (applicationInvocationId 81) operation)
              fixtureActive
          transition =
            stepApplicationClient
              ( ServerDtoReceived
                  ( RequestRetained
                      fixtureSession
                      cursor2
                      (requestClaim 0)
                      (Completed result)
                  )
              )
              withCall
      if resultIndex `elem` matchingIndexes
        then assertBool ("matching pair rejected: " <> show (operation, result)) (isAdvanced transition)
        else
          assertEqual
            ("mismatched pair admitted: " <> show (operation, result))
            (Left (ApplicationClientResultMismatch (requestClaim 0)))
            transition
    isAdvanced (Right (ApplicationClientAdvanced _ _)) = True
    isAdvanced _ = False

caseStructuralPendingResults :: Assertion
caseStructuralPendingResults =
  mapM_
    check
    [ ( WriteApplication fixtureWriter (PublishValue (BoolValue True)),
        StructuralStabilizationPending,
        WriteCompleted WriteAccepted
      ),
      ( ForwardApplication fixtureWriter fixtureObject,
        StructuralStabilizationPending,
        ForwardCompleted ForwardAccepted
      ),
      ( LabelApplication fixtureObject (VoidLabel, 0) LabelToVoid,
        LabelSettlementPending,
        LabelCompleted LabelApplied
      ),
      ( NewEnvironmentApplication,
        EnvironmentStabilizationPending,
        NewEnvironmentCompleted fixtureEnvironmentAccess
      )
    ]
  where
    check (operation, pendingReason, result) = do
      let invocation = applicationInvocationId 83
          (withCall, _) = advance (Submit invocation operation) fixtureActive
          (pending, pendingEffects) =
            advance
              ( ServerDtoReceived
                  ( RequestRetained
                      fixtureSession
                      cursor2
                      (requestClaim 0)
                      (OperationAccepted pendingReason)
                  )
              )
              withCall
          (_, completedEffects) =
            advance
              ( ServerDtoReceived
                  ( RequestRetained
                      fixtureSession
                      cursor3
                      (requestClaim 0)
                      (Completed result)
                  )
              )
              pending
      assertEqual "pending is hidden from the caller" [] pendingEffects
      assertEqual
        "the operation-specific terminal completes once"
        [CallFinished invocation (ApplicationCallSucceeded result)]
        completedEffects

caseOpenRetry :: Assertion
caseOpenRetry = do
  let initial = fixtureInitial
      (opening, firstEffects) = advanceRaw Start initial
      firstAttempt = requestedAttempt firstEffects
      (awaitingOpen, openEffects) = advance TransportAvailable opening
      (retrying, lostEffects) = advanceRaw TransportLost awaitingOpen
      retryAt = instant 110
      (openingRetry, retryTimerEffects) =
        advanceRaw
          ( Client.TransportRetryElapsed
              ApplicationOpeningRetry
              firstAttempt
              retryAt
          )
          retrying
      (awaitingRetry, retryEffects) = advance TransportAvailable openingRetry
      (active, acceptedEffects) =
        advance
          (ServerDtoReceived (SessionOpened cursor1 fixtureSession fixtureToken fixtureStartup))
          awaitingRetry
      openDto = OpenSession fixtureAttachment fixtureNonce
  assertEqual "start requests one qualified transport" [RequestTransport firstAttempt] firstEffects
  assertEqual "first transport sends open" [SendApplicationDto openDto] openEffects
  assertEqual
    "loss arms one paced opening retry"
    [ArmTransportRetry ApplicationOpeningRetry firstAttempt retryAt]
    lostEffects
  assertBool
    "elapsed retry requests a fresh transport generation"
    (case retryTimerEffects of [RequestTransport next] -> next /= firstAttempt; _ -> False)
  assertEqual "retry sends byte-equivalent typed open" [SendApplicationDto openDto] retryEffects
  assertEqual "open reports startup access once" [SessionReady fixtureStartup] acceptedEffects
  assertEqual "client is active" ApplicationClientActive (applicationClientPhase active)
  assertEqual "accepted state validates" (Right ()) (validateApplicationClientState active)

caseAllocationAndReuse :: Assertion
caseAllocationAndReuse = do
  let invocation0 = applicationInvocationId 90
      invocation1 = applicationInvocationId 91
      operation = WaitApplication []
      (afterFirst, firstEffects) = advance (Submit invocation0 operation) fixtureActive
      rejected = stepApplicationClient (Submit invocation0 operation) afterFirst
      (afterSecond, secondEffects) = advance (Submit invocation1 operation) afterFirst
  assertEqual
    "first request is zero"
    [SendApplicationDto (callDto 0 operation)]
    firstEffects
  assertEqual
    "used invocation has a typed rejection and no successor"
    (Right (ApplicationClientCommandRejected (ApplicationInvocationAlreadyUsed invocation0)))
    rejected
  assertEqual
    "rejection did not consume request one"
    [SendApplicationDto (CallWithRetirement fixtureSession (applicationRequestIdClaim 1) operation (Client.applicationClientReceiptRetirement afterFirst))]
    secondEffects
  assertEqual "successor validates" (Right ()) (validateApplicationClientState afterSecond)

caseWrongPhaseSubmission :: Assertion
caseWrongPhaseSubmission = do
  let invocation = applicationInvocationId 20
      input = Submit invocation (WaitApplication [])
  assertEqual
    "unopened submission rejects locally"
    ( Right
        ( ApplicationClientCommandRejected
            (ApplicationClientWrongPhase invocation ApplicationClientUnopened)
        )
    )
    (stepApplicationClient input fixtureInitial)
  let (_, effects) = advance input fixtureActive
  assertEqual
    "the invocation still obtains request zero after open"
    [SendApplicationDto (callDto 0 (WaitApplication []))]
    effects

caseNewPendingAfterTerminal :: Assertion
caseNewPendingAfterTerminal = do
  let invocation = applicationInvocationId 54
      (withCall, _) = advance (Submit invocation (WaitApplication [])) fixtureActive
      wait = waitClaim 0
      (pending, _) =
        advance
          ( ServerDtoReceived
              ( RequestRetained
                  fixtureSession
                  cursor2
                  (requestClaim 0)
                  (WaitAccepted wait)
              )
          )
          withCall
      (terminal, _) =
        advance
          ( ServerDtoReceived
              (WaitWake fixtureSession cursor3 (requestClaim 0) wait WaitReady)
          )
          pending
  assertEqual
    "the retained pending observation cannot acquire a later cursor"
    (Left (ApplicationClientCursorConflict (cursorClaim 4)))
    ( stepApplicationClient
        ( ServerDtoReceived
            ( RequestRetained
                fixtureSession
                (cursorClaim 4)
                (requestClaim 0)
                (WaitAccepted wait)
            )
        )
        terminal
    )

caseRejectedAfterWaitAccepted :: Assertion
caseRejectedAfterWaitAccepted = do
  let (withCall, _) =
        advance
          (Submit (applicationInvocationId 56) (WaitApplication []))
          fixtureActive
      (pending, _) =
        advance
          ( ServerDtoReceived
              ( RequestRetained
                  fixtureSession
                  cursor2
                  (requestClaim 0)
                  (WaitAccepted (waitClaim 0))
              )
          )
          withCall
  assertEqual
    "pending wait cannot transition to semantic rejection"
    (Left (ApplicationClientSummaryContradiction (requestClaim 0)))
    ( stepApplicationClient
        ( ServerDtoReceived
            ( RequestRetained
                fixtureSession
                cursor3
                (requestClaim 0)
                (Rejected ApplicationQueryPredicateMismatch)
            )
        )
        pending
    )

caseRequestObservationCursorUniqueness :: Assertion
caseRequestObservationCursorUniqueness = do
  let (withCall, _) =
        advance
          (Submit (applicationInvocationId 57) (WaitApplication []))
          fixtureActive
      body = WaitAccepted (waitClaim 0)
      (pending, _) =
        advance
          (ServerDtoReceived (RequestRetained fixtureSession cursor2 (requestClaim 0) body))
          withCall
  assertEqual
    "the same retained reply cannot allocate another cursor"
    (Left (ApplicationClientCursorConflict cursor3))
    ( stepApplicationClient
        (ServerDtoReceived (RequestRetained fixtureSession cursor3 (requestClaim 0) body))
        pending
    )

caseCursorDuplicate :: Assertion
caseCursorDuplicate = do
  let (withCall, _) =
        advance (Submit (applicationInvocationId 6) (WaitApplication [])) fixtureActive
      wait = waitClaim 0
      accepted =
        ServerDtoReceived
          (RequestRetained fixtureSession cursor2 (requestClaim 0) (WaitAccepted wait))
      (withAcceptance, firstEffects) = advance accepted withCall
      (duplicate, duplicateEffects) = advance accepted withAcceptance
      conflict =
        stepApplicationClient
          ( ServerDtoReceived
              ( RequestRetained
                  fixtureSession
                  cursor2
                  (requestClaim 0)
                  (Completed (WaitCompleted WaitReady))
              )
          )
          duplicate
  assertEqual "wait acceptance itself has no completion" [] firstEffects
  assertEqual "exact duplicate is effect-idempotent" [] duplicateEffects
  assertEqual
    "same cursor with another observation faults"
    (Left (ApplicationClientCursorConflict cursor2))
    conflict

caseCancellationBeforeAcceptance :: Assertion
caseCancellationBeforeAcceptance = do
  let invocation = applicationInvocationId 7
      (withCall, _) = advance (Submit invocation (WaitApplication [])) fixtureActive
      (cancelledLocally, cancelEffects) = advance (Cancel invocation) withCall
      wait = waitClaim 0
      (cancelling, acceptedEffects) =
        advance
          ( ServerDtoReceived
              (RequestRetained fixtureSession cursor2 (requestClaim 0) (WaitAccepted wait))
          )
          cancelledLocally
      (terminal, terminalEffects) =
        advance
          ( ServerDtoReceived
              (RequestRetained fixtureSession cursor3 (requestClaim 0) (Cancelled wait))
          )
          cancelling
      (_, duplicateEffects) =
        advance
          ( ServerDtoReceived
              (RequestRetained fixtureSession cursor3 (requestClaim 0) (Cancelled wait))
          )
          terminal
  assertEqual "pre-acceptance cancellation records intent only" [] cancelEffects
  assertEqual
    "acceptance reveals the exact cancellation tuple"
    [ SendApplicationDto
        (CancelPendingWait fixtureSession (requestClaim 0) wait)
    ]
    acceptedEffects
  assertEqual "retained cancellation completes once" [CallCancelled invocation] terminalEffects
  assertEqual "duplicate cancellation is silent" [] duplicateEffects

caseCancellationAfterAcceptance :: Assertion
caseCancellationAfterAcceptance = do
  let invocation = applicationInvocationId 71
      (withCall, _) = advance (Submit invocation (WaitApplication [])) fixtureActive
      wait = waitClaim 0
      (pending, _) =
        advance
          ( ServerDtoReceived
              (RequestRetained fixtureSession cursor2 (requestClaim 0) (WaitAccepted wait))
          )
          withCall
      (cancelling, cancelEffects) = advance (Cancel invocation) pending
      (terminal, wakeEffects) =
        advance
          (ServerDtoReceived (WaitWake fixtureSession cursor3 (requestClaim 0) wait WaitReady))
          cancelling
      (_, duplicateEffects) =
        advance
          (ServerDtoReceived (WaitWake fixtureSession cursor3 (requestClaim 0) wait WaitReady))
          terminal
  assertEqual
    "known wait cancellation sends the exact tuple"
    [SendApplicationDto (CancelPendingWait fixtureSession (requestClaim 0) wait)]
    cancelEffects
  assertEqual
    "wake wins with one semantic completion"
    [CallFinished invocation (ApplicationCallSucceeded (WaitCompleted WaitReady))]
    wakeEffects
  assertEqual "a consistent spurious duplicate wake is silent" [] duplicateEffects

caseWakeBeforeCancellation :: Assertion
caseWakeBeforeCancellation = do
  let invocation = applicationInvocationId 8
      (withCall, _) = advance (Submit invocation (WaitApplication [])) fixtureActive
      wait = waitClaim 0
      (pending, _) =
        advance
          ( ServerDtoReceived
              (RequestRetained fixtureSession cursor2 (requestClaim 0) (WaitAccepted wait))
          )
          withCall
      (terminal, wakeEffects) =
        advance
          ( ServerDtoReceived
              (WaitWake fixtureSession cursor3 (requestClaim 0) wait WaitReady)
          )
          pending
      cancelOutcome = stepApplicationClient (Cancel invocation) terminal
  assertEqual
    "wake completes with the semantic wait result"
    [CallFinished invocation (ApplicationCallSucceeded (WaitCompleted WaitReady))]
    wakeEffects
  assertEqual
    "cancellation after retirement is typed and never resubmits"
    (Right (ApplicationClientCommandRejected (ApplicationInvocationRetired invocation)))
    cancelOutcome

caseRequestBookkeeping :: Assertion
caseRequestBookkeeping = do
  let invocation = applicationInvocationId 9
      operation = WaitApplication []
      (withCall, _) = advance (Submit invocation operation) fixtureActive
      (retried, absentEffects) =
        advance
          (ServerDtoReceived (RequestAbsent fixtureSession (requestClaim 0)))
          withCall
      conflict =
        stepApplicationClient
          (ServerDtoReceived (RequestConflict fixtureSession (requestClaim 0)))
          retried
  assertEqual "absence resends the immutable exact call" [SendApplicationDto (callDto 0 operation)] absentEffects
  assertEqual
    "typed request conflict is an owner contradiction"
    (Left (ApplicationClientRequestConflict (requestClaim 0)))
    conflict

caseCorrelationFaults :: Assertion
caseCorrelationFaults = do
  let (withCall, _) =
        advance (Submit (applicationInvocationId 10) (WaitApplication [])) fixtureActive
      foreignSession = checked (applicationSessionClaim fixtureOtherScope 2)
      foreignResult =
        stepApplicationClient
          (ServerDtoReceived (RequestAbsent foreignSession (requestClaim 0)))
          withCall
      unknownResult =
        stepApplicationClient
          (ServerDtoReceived (RequestAbsent fixtureSession (requestClaim 44)))
          withCall
      wrongWait = checked (applicationWaitClaim fixtureScope 99 (requestClaim 0))
      waitResult =
        stepApplicationClient
          ( ServerDtoReceived
              ( RequestRetained
                  fixtureSession
                  cursor2
                  (requestClaim 0)
                  (WaitAccepted wrongWait)
              )
          )
          withCall
      prematureWake =
        stepApplicationClient
          ( ServerDtoReceived
              (WaitWake fixtureSession cursor2 (requestClaim 0) (waitClaim 0) WaitReady)
          )
          withCall
  assertEqual
    "foreign session faults"
    (Left (ApplicationClientForeignSession fixtureSession foreignSession))
    foreignResult
  assertEqual
    "unknown request faults"
    (Left (ApplicationClientUnknownRequest (requestClaim 44)))
    unknownResult
  assertEqual
    "wait/session mismatch faults"
    (Left (ApplicationClientWaitMismatch (requestClaim 0)))
    waitResult
  assertEqual
    "a wake cannot mint the owner-issued wait correlation"
    (Left (ApplicationClientWaitMismatch (requestClaim 0)))
    prematureWake

casePermanentLoss :: Assertion
casePermanentLoss = do
  let invocation0 = applicationInvocationId 11
      invocation1 = applicationInvocationId 12
      (afterFirst, _) = advance (Submit invocation0 (WaitApplication [])) fixtureActive
      (afterSecond, _) = advance (Submit invocation1 (WaitApplication [])) afterFirst
      (unavailable, effects) =
        advance
          (Client.HeraldPermanentlyUnavailable HeraldPermanentlyLost)
          afterSecond
  assertEqual
    "unknown outcomes preserve request order"
    [ CallUnknown invocation0 HeraldPermanentlyLost,
      CallUnknown invocation1 HeraldPermanentlyLost,
      HeraldPermanentlyUnavailableObserved HeraldPermanentlyLost,
      CloseTransport
    ]
    effects
  assertEqual
    "owner is permanently unavailable"
    ApplicationClientPermanentlyUnavailable
    (applicationClientPhase unavailable)

caseTerminalSessionDisposition :: Assertion
caseTerminalSessionDisposition = do
  let invocation0 = applicationInvocationId 740
      invocation1 = applicationInvocationId 741
      operation = WaitApplication []
      (afterFirst, _) = advance (Submit invocation0 operation) fixtureActive
      (withCalls, _) = advance (Submit invocation1 operation) afterFirst
      attempt = currentAttempt withCalls
      terminalDto =
        Protocol.HeraldPermanentlyUnavailable
          fixtureSession
          ApplicationSessionNoLongerLiveDto
      (unavailable, effects) =
        advanceRaw
          (Client.ServerDtoReceived attempt (instant 101) terminalDto)
          withCalls
  assertEqual
    "every unresolved accepted call becomes transport-level unknown"
    [ CallUnknown invocation0 SessionNoLongerLive,
      CallUnknown invocation1 SessionNoLongerLive,
      HeraldPermanentlyUnavailableObserved SessionNoLongerLive,
      CloseTransport
    ]
    effects
  assertEqual
    "the terminal frame leaves no client transport owner to redial"
    Nothing
    (applicationClientCurrentTransportAttempt unavailable)
  assertEqual
    "the session disposition is terminal"
    ApplicationClientPermanentlyUnavailable
    (applicationClientPhase unavailable)
  let (repeated, repeatedEffects) =
        advanceRaw
          (Client.ServerDtoReceived attempt (instant 102) terminalDto)
          unavailable
  assertEqual "a repeated terminal frame is state-exact" unavailable repeated
  assertEqual "a repeated terminal frame emits nothing" [] repeatedEffects

caseIsolationLifecycle :: Assertion
caseIsolationLifecycle = do
  let attempt = currentAttempt fixtureActive
      notice =
        HeraldIsolationBegun
          cursor2
          fixtureSession
          ResidentProcessesZombie
          HeraldIsolatedDto
      (isolated, noticeEffects) =
        advanceRaw
          (Client.ServerDtoReceived attempt (instant 200) notice)
          fixtureActive
  assertEqual
    "the ordered notice enters the explicit read-only phase"
    ApplicationClientIsolationReadOnly
    (applicationClientPhase isolated)
  assertEqual
    "the application observes the abandoned resident-process overlay"
    [HeraldIsolationObserved ResidentProcessesZombie]
    noticeEffects

  let readInvocation = applicationInvocationId 743
      readOperation = ReadApplication fixtureQuery
      (withRead, readEffects) = advance (Submit readInvocation readOperation) isolated
  assertEqual
    "a local read retains ordinary request identity and is sent"
    [SendApplicationDto (Call fixtureSession (requestClaim 0) readOperation)]
    readEffects
  let mutationInvocation = applicationInvocationId 744
      (afterMutation, mutationEffects) =
        advanceRaw
          (Submit mutationInvocation (WaitApplication []))
          withRead
  assertEqual
    "a mutating call consumes no protocol request or client state"
    withRead
    afterMutation
  assertEqual
    "a post-notice mutation has the same semantic rejection as a raced server call"
    [ CallFinished
        mutationInvocation
        (ApplicationCallRejected ApplicationOperateNotPermitted)
    ]
    mutationEffects

  let terminal =
        Protocol.HeraldPermanentlyUnavailable
          fixtureSession
          HeraldIsolatedDto
      (unavailable, terminalEffects) =
        advanceRaw
          (Client.ServerDtoReceived attempt (instant 201) terminal)
          withRead
  assertEqual
    "the terminal disposition settles the outstanding read with the exact reason"
    [ CallUnknown readInvocation HeraldIsolated,
      HeraldPermanentlyUnavailableObserved HeraldIsolated,
      CloseTransport
    ]
    terminalEffects
  assertEqual
    "isolation is terminal and owns no reconnect attempt"
    (ApplicationClientPermanentlyUnavailable, Nothing)
    ( applicationClientPhase unavailable,
      applicationClientCurrentTransportAttempt unavailable
    )

  let retiredNotice =
        HeraldIsolationBegun
          cursor2
          fixtureSession
          ResidentProcessesZombie
          HeraldRetiredDto
      (retiredDrain, _) =
        advanceRaw
          (Client.ServerDtoReceived attempt (instant 202) retiredNotice)
          fixtureActive
      (_, retiredLossEffects) =
        advanceRaw
          (Client.TransportLost attempt (instant 203))
          retiredDrain
  assertEqual
    "transport loss after the drain notice preserves the authoritative retirement reason"
    [ HeraldPermanentlyUnavailableObserved HeraldRetired,
      CloseTransport
    ]
    retiredLossEffects

caseClientIsolation :: Assertion
caseClientIsolation = do
  let secondSession = checked (applicationSessionClaim fixtureOtherScope 8)
      secondToken = checked (applicationResumeTokenClaim fixtureOtherScope 8)
      secondClient = activeFor secondSession secondToken
      invocation = applicationInvocationId 75
      operation = WaitApplication []
      (firstAfter, firstEffects) = advance (Submit invocation operation) fixtureActive
      (secondAfter, secondEffects) = advance (Submit invocation operation) secondClient
      (_, firstLossEffects) =
        advance (Client.HeraldPermanentlyUnavailable HeraldPermanentlyLost) firstAfter
  assertEqual
    "first owner allocates in its own session namespace"
    [SendApplicationDto (Call fixtureSession (requestClaim 0) operation)]
    firstEffects
  assertEqual
    "second owner may reuse the same local correlations independently"
    [SendApplicationDto (Call secondSession (requestClaim 0) operation)]
    secondEffects
  assertEqual
    "loss of the first owner does not mutate the second"
    (Right ())
    (validateApplicationClientState secondAfter)
  assertEqual
    "first loss still reports only its own invocation"
    [ CallUnknown invocation HeraldPermanentlyLost,
      HeraldPermanentlyUnavailableObserved HeraldPermanentlyLost,
      CloseTransport
    ]
    firstLossEffects

caseServerPhaseMatrix :: Assertion
caseServerPhaseMatrix = do
  assertEqual
    "server DTO before start faults"
    (Left (ApplicationClientUnexpectedServerDto ApplicationClientUnopened))
    ( stepApplicationClient
        (ServerDtoReceived (SessionRejected ApplicationAttachmentNotAdmittedDto))
        fixtureInitial
    )
  let (closed, _) = advance End fixtureActive
      (afterLateDto, lateDtoEffects) =
        advanceRaw
          (ServerDtoReceived (RequestAbsent fixtureSession (requestClaim 0)))
          closed
  assertEqual "server DTO after close is stale" closed afterLateDto
  assertEqual "stale terminal DTO emits nothing" [] lateDtoEffects
  let (opening, _) = advance Start fixtureInitial
      (awaiting, _) = advance TransportAvailable opening
      unrelatedToken = checked (applicationResumeTokenClaim fixtureOtherScope 2)
  assertEqual
    "session and token correlations must describe one owner allocation"
    ( Left
        (ApplicationClientSessionTokenMismatch fixtureSession unrelatedToken)
    )
    ( stepApplicationClient
        (ServerDtoReceived (SessionOpened cursor1 fixtureSession unrelatedToken fixtureStartup))
        awaiting
    )

caseOpeningRejectionMatrix :: Assertion
caseOpeningRejectionMatrix = do
  let (openingTransport, _) = advance Start fixtureInitial
      (openingDisposition, _) = advance TransportAvailable openingTransport
      ordinary =
        stepApplicationClient
          (ServerDtoReceived (SessionRejected ApplicationAttachmentNotAdmittedDto))
          openingDisposition
  assertBool "attachment rejection is an ordinary open outcome" (isAdvanced ordinary)
  mapM_
    ( \rejection ->
        assertEqual
          ("unexpected opening rejection " <> show rejection)
          (Left (ApplicationClientUnexpectedSessionRejection rejection))
          ( stepApplicationClient
              (ServerDtoReceived (SessionRejected rejection))
              openingDisposition
          )
    )
    [ ApplicationSessionNotLiveDto,
      ApplicationResumeTokenMismatchDto,
      ApplicationReplyCursorNotIssuedDto,
      ApplicationRequestSessionMismatchDto,
      ApplicationRequestBindingMismatchDto,
      ApplicationRequestBindingNotLiveDto,
      ApplicationEndSessionBindingMismatchDto
    ]
  where
    isAdvanced (Right (ApplicationClientAdvanced _ _)) = True
    isAdvanced _ = False

isExactTransitionNoOp ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition ->
  Bool
isExactTransitionNoOp expected = \case
  Right (ApplicationClientAdvanced actual batch) ->
    actual == expected && null (applicationClientEffects batch)
  _ -> False

propDeterministic :: Word64 -> Property
propDeterministic value =
  let input = Submit (applicationInvocationId value) (WaitApplication [])
   in stepApplicationClient input fixtureActive
        === stepApplicationClient input fixtureActive

propStaleTransportObservations :: Word64 -> Property
propStaleTransportObservations observedMicroseconds =
  let attachedAttempt = currentAttempt fixtureActive
      (recovering, _) =
        advanceRaw
          (Client.TransportLost attachedAttempt (instant 0))
          fixtureActive
      observedAt = instant observedMicroseconds
      staleInputs =
        [ Client.TransportAvailable attachedAttempt observedAt,
          Client.TransportAttemptFailed attachedAttempt observedAt,
          Client.TransportLost attachedAttempt observedAt,
          Client.ServerDtoReceived
            attachedAttempt
            observedAt
            (RequestAbsent fixtureSession (requestClaim 99))
        ]
   in conjoin
        [ counterexample
            ("stale input changed recovery: " <> show input)
            (isExactTransitionNoOp recovering (Client.stepApplicationClient input recovering) === True)
        | input <- staleInputs
        ]

propGeneratedAllocation :: NonNegative Int -> Property
propGeneratedAllocation (NonNegative rawCount) =
  let count = rawCount `mod` 64
      inputs =
        [ Submit (applicationInvocationId (fromIntegral invocation)) (WaitApplication [])
        | invocation <- [0 .. count - 1]
        ]
      (_, effectLists) = mapAccumL apply fixtureActive inputs
      requestWords =
        [ applicationRequestIdClaimWord64 request
        | effects <- effectLists,
          SendApplicationDto dto <- effects,
          request <- case dto of
            Call _ identifier _ -> [identifier]
            CallWithRetirement _ identifier _ _ -> [identifier]
            _ -> []
        ]
      expected = fmap fromIntegral [0 .. count - 1]
   in counterexample
        ("allocated request IDs: " <> show requestWords)
        (requestWords === expected)
  where
    apply state input = advance input state

fixtureInitial :: ApplicationClientState
fixtureInitial =
  initialApplicationClient fixtureRecoveryConfiguration fixtureAttachment fixtureNonce

fixtureOpening :: ApplicationClientState
fixtureOpening = fst (advance Start fixtureInitial)

fixtureRecoveryConfiguration :: ApplicationClientRecoveryConfiguration
fixtureRecoveryConfiguration =
  checked (applicationClientRecoveryConfiguration 10 1000)

fixtureActive :: ApplicationClientState
fixtureActive = activeFor fixtureSession fixtureToken

activeFor ::
  ApplicationSessionClaim ->
  ApplicationResumeTokenClaim ->
  ApplicationClientState
activeFor session token =
  let (opening, _) = advance Start fixtureInitial
      (awaiting, _) = advance TransportAvailable opening
      (active, _) =
        advance
          (ServerDtoReceived (SessionOpened cursor1 session token fixtureStartup))
          awaiting
   in active

fixtureScope :: ByteString.ByteString
fixtureScope = ByteString.replicate 32 2

fixtureOtherScope :: ByteString.ByteString
fixtureOtherScope = ByteString.replicate 32 3

fixtureAttachment :: ApplicationAttachmentClaim
fixtureAttachment = checked (applicationAttachmentClaim (ByteString.replicate 32 1))

fixtureSession :: ApplicationSessionClaim
fixtureSession = checked (applicationSessionClaim fixtureScope 2)

fixtureToken :: ApplicationResumeTokenClaim
fixtureToken = checked (applicationResumeTokenClaim fixtureScope 2)

fixtureNonce :: ApplicationClientNonce
fixtureNonce = applicationClientNonce 55

fixtureStartup :: ApplicationStartupAccess
fixtureStartup =
  applicationStartupAccess
    (asPrivateProcessId (privateId 1))
    ( primordialAccessFromSelection
        ( selectEnvironment
            ( checked
                ( environmentAccess
                    [ predefinedAccess role (asPrivateNablaId (privateId writer)) (asPrivateDeltaId (privateId reader))
                    | (role, writer, reader) <- zip3 allApplicationPredefinedSortRoles [2 ..] [20 ..]
                    ]
                    (asPrivateObjectId (privateId 1000))
                    [asPrivateObjectId (privateId value) | value <- [1001 .. 1018]]
                )
            )
        )
    )

fixtureDefinition :: ApplicationSortDefinition
fixtureDefinition = PredefinedSortDefinition SortDefinitionRole fixtureSortId

fixtureSortId :: SortId
fixtureSortId = checked (mkSortId (ByteString.pack [0 .. 31]))

currentOperations :: [(ApplicationOperation, [Int])]
currentOperations =
  [ (NewIdApplication BareNewId, [0]),
    (NewIdApplication (ControlledNewId fixtureWriter), [0]),
    (WriteApplication fixtureWriter (PublishValue (SortDefinitionValue fixtureDefinition)), [1]),
    (WriteApplication fixtureWriter (PublishValue (BoolValue True)), [2]),
    (WriteApplication fixtureWriter (DeleteReserved fixtureObject), [2]),
    (ForwardApplication fixtureWriter fixtureObject, [3]),
    (ReadApplication fixtureQuery, [4]),
    (LocalTakeApplication fixtureQuery, [5]),
    (WaitApplication [fixtureQuery], [6]),
    ( LabelApplication
        fixtureObject
        (ProcessLabel (asPrivateProcessId (privateId 11)), 0)
        (LabelToProcess (asPrivateProcessId (privateId 10))),
      [7, 8]
    ),
    (NewEnvironmentApplication, [9])
  ]

currentResults :: [RegularCallResult]
currentResults =
  [ NewIdCompleted (privateId 40),
    WriteCompleted (SortDefinitionWritten fixtureSortId),
    WriteCompleted WriteAccepted,
    ForwardCompleted ForwardAccepted,
    ReadCompleted [],
    LocalTakeCompleted [],
    WaitCompleted WaitReady,
    LabelCompleted LabelApplied,
    LabelCompleted LabelNotApplied,
    NewEnvironmentCompleted fixtureEnvironmentAccess
  ]

fixtureEnvironmentAccess :: EnvironmentAccess
fixtureEnvironmentAccess =
  checked
    ( environmentAccess
        [ predefinedAccess
            role
            (asPrivateNablaId (privateId writer))
            (asPrivateDeltaId (privateId reader))
        | (role, writer, reader) <- zip3 allApplicationPredefinedSortRoles [42 ..] [52 ..]
        ]
        (asPrivateObjectId (privateId 1000))
        [asPrivateObjectId (privateId value) | value <- [1001 .. 1018]]
    )

fixtureWriter :: PrivateNablaId
fixtureWriter = asPrivateNablaId (privateId 8)

fixtureObject :: PrivateObjectId
fixtureObject = asPrivateObjectId (privateId 9)

fixtureQuery :: ApplicationQuery
fixtureQuery = ApplicationQuery Set.empty QueryAlways

privateId :: Word64 -> PrivateUniqueId
privateId = checked . mkPrivateUniqueId

requestClaim :: Word64 -> ApplicationRequestIdClaim
requestClaim = applicationRequestIdClaim

waitClaim :: Word64 -> ApplicationWaitClaim
waitClaim request =
  checked (applicationWaitClaim fixtureScope 2 (requestClaim request))

cursor1 :: ApplicationReplyCursorClaim
cursor1 = checked (applicationReplyCursorClaim 1)

cursor2 :: ApplicationReplyCursorClaim
cursor2 = checked (applicationReplyCursorClaim 2)

cursor3 :: ApplicationReplyCursorClaim
cursor3 = checked (applicationReplyCursorClaim 3)

cursorClaim :: Word64 -> ApplicationReplyCursorClaim
cursorClaim = checked . applicationReplyCursorClaim

callDto :: Word64 -> ApplicationOperation -> ApplicationClientDto
callDto request operation = Call fixtureSession (requestClaim request) operation

advance ::
  (TestClientInput input) =>
  input ->
  ApplicationClientState ->
  (ApplicationClientState, [ApplicationClientEffect])
advance input state = case stepApplicationClient input state of
  Left fault -> error ("unexpected client fault: " <> show fault)
  Right (ApplicationClientCommandRejected rejection) ->
    error ("unexpected command rejection: " <> show rejection)
  Right (ApplicationClientAdvanced successor batch) ->
    (successor, visibleTestEffects (applicationClientEffects batch))

advanceRaw ::
  (TestClientInput input) =>
  input ->
  ApplicationClientState ->
  (ApplicationClientState, [ApplicationClientEffect])
advanceRaw input state = case stepApplicationClient input state of
  Left fault -> error ("unexpected client fault: " <> show fault)
  Right (ApplicationClientCommandRejected rejection) ->
    error ("unexpected command rejection: " <> show rejection)
  Right (ApplicationClientAdvanced successor batch) ->
    (successor, applicationClientEffects batch)

visibleTestEffects :: [ApplicationClientEffect] -> [ApplicationClientEffect]
visibleTestEffects = filter $ \case
  ArmTransportRetry {} -> False
  CancelTransportRetry {} -> False
  ArmRecoveryDeadline {} -> False
  CancelRecoveryDeadline {} -> False
  SessionEstablishedOnTransport {} -> False
  ScheduleReceiptRetirement {} -> False
  _ -> True

requestedAttempt :: [ApplicationClientEffect] -> ApplicationTransportAttemptGeneration
requestedAttempt = \case
  [RequestTransport attempt] -> attempt
  effects -> error ("expected one transport request, got: " <> show effects)

checked :: (Show error) => Either error value -> value
checked = either (error . ("invalid test fixture: " <>) . show) id
