{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ApplicationRequestProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.Set qualified as Set
import Data.Word (Word32, Word64, Word8)
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( asPrivateNablaId,
    asPrivateObjectId,
    mkPrivateUniqueId,
  )
import Eclips.Application.Types.Lifetime (applicationReceiptRetirement)
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (..),
  )
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Application.Types.Result
  ( OperationPendingReason (StructuralStabilizationPending),
    RegularCallResult (..),
    WaitResult (..),
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    HeraldEpoch,
    ProcessEpochId,
    mkGlobalUniqueId,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    appliedBootstrapManifestId,
    appliedProcessEpochId,
  )
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Application.Request.Internal
  ( ApplicationReplyCursor,
    ApplicationRequestReply (..),
    ApplicationRequestStatus (..),
    ApplicationRequestSummary,
    ProcessAcceptancePosition,
    RequestId,
    RetainedRequestReplyBody (..),
    WaitId,
    processAcceptancePositionOrdinal,
    requestId,
    requestSummaryEntries,
    requestSummaryEntryRequestId,
    requestSummaryEntryStatus,
  )
import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionRejection (..),
    ApplicationSessionReply (..),
    applicationAttachmentForBootstrap,
    clientNonce,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.State
  ( ApplicationOperationCompletionOutcome (..),
    ApplicationRequestCandidate,
    ApplicationRequestClassification (..),
    ApplicationRequestStateWitness,
    ApplicationSessionTransitionError (..),
    ApplicationWaitCancellation (..),
    PreparedApplicationWaitCancellation,
    State,
    applicationOperationCompletionBinding,
    applicationOperationCompletionCursor,
    applicationOperationCompletionRequestId,
    applicationOperationCompletionResult,
    applicationRequestCandidatePosition,
    applicationRequestCandidateWaitId,
    applicationRequestNextAcceptancePositions,
    applicationRequestOwnerRequests,
    applicationRequestOwnerSession,
    applicationRequestOwnerWitnesses,
    applicationRequestResult,
    applicationRequestStateWitness,
    applicationRequestWitnessId,
    applicationRequestWitnessPosition,
    applicationWaitWakeCursor,
    applicationWaitWakeRequestId,
    applicationWaitWakeResult,
    applicationWaitWakeWaitId,
    classifyApplicationRequest,
    commitApplicationBindingLoss,
    commitApplicationOperationCompletion,
    commitApplicationRequest,
    commitApplicationSessionAcceptance,
    commitApplicationSessionEnd,
    commitApplicationWaitCancellation,
    commitApplicationWaitCompletions,
    commitOrdinaryApplicationValueLocalization,
    globalizeOrdinaryApplicationValue,
    prepareApplicationBindingLoss,
    prepareApplicationOperationAcceptance,
    prepareApplicationOperationCompletion,
    prepareApplicationRequestCompletion,
    prepareApplicationRequestCompletionFromLocalizedState,
    prepareApplicationRequestRejection,
    prepareApplicationSessionEnd,
    prepareApplicationSessionOpen,
    prepareApplicationSessionResume,
    prepareApplicationWaitAcceptance,
    prepareApplicationWaitCancellation,
    prepareApplicationWaitCompletions,
    prepareOrdinaryApplicationValueLocalization,
    preparedApplicationOperationCompletionOutcome,
    preparedApplicationSessionEndWaitIds,
    preparedApplicationWaitWakes,
    processRoleView,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Bootstrap
  ( BootstrapInvariant,
    BootstrapOwners,
    BootstrapViews,
    bootstrapApplicationState,
    bootstrapViews,
    commitProcessBootstrap,
    initialBootstrapOwners,
    prepareProcessBootstrap,
  )
import Eclips.Herald.Genesis
  ( CheckedInitialBootstraps,
    PrimordialProcessManifest (..),
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedInitialBootstraps,
    checkedLocalHeraldEpoch,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Time (monotonicInstant)
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureLocalBootstrapIds,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    forAll,
    shuffle,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "application request ownership"
    [ testProperty "receipt retirement seals completed prefixes and pending holes pin shuffled completions" propReceiptRetirement,
      testProperty "flowing completed request receipts retain constant live size" propFlowingReceiptRetirement,
      testCase "unexpected disconnect detaches accepted work before late completion" caseImmediateDisconnectedOperationCompletion,
      testCase "retirement covers cancellation, rejection, skipped IDs and resumed summary copies" caseRetirementTerminalBodies,
      testProperty "exact typed retry reuses one retained reply" propExactRetry,
      testCase "accepted positions are process-wide and rejections consume none" caseAcceptancePositions,
      testCase "accepted operation completes monotonically at its exact position" caseOperationLifecycle,
      testCase "disconnected operation completion is retained without delivery" caseDisconnectedOperationCompletion,
      testCase "completion after session end is a typed no-op" caseEndedOperationCompletion,
      testCase "resume reoffers its exact sorted summary snapshot" caseResumeSnapshot,
      testCase "localized aliases and their request result commit together" caseLocalizedCompletion,
      testCase "wake, cancellation, and session end preserve exact correlations" caseWaitLifecycle,
      testCase "request ingress requires the separately named current live binding" caseRequestBindingChecks
    ]

propReceiptRetirement :: Word8 -> Property
propReceiptRetirement generated = forAll (shuffle [1 .. count]) $ \completionOrder ->
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 991 fixture.state
      (session, _, binding, _) = opened acceptance
      low = requestId 0
      (withHole, waitReply) =
        commitApplicationRequest
          (prepareApplicationWaitAcceptance (firstCandidate session binding low (WaitApplication []) afterOpen))
      wait = acceptedWait waitReply
      admit (state, previousPositions) number =
        let candidate = firstCandidate session binding (requestId number) structuralCall state
            (successor, _) = commitApplicationRequest (prepareApplicationOperationAcceptance candidate StructuralStabilizationPending)
         in (successor, (number, applicationRequestCandidatePosition candidate) : previousPositions)
      (allPending, positions) = foldl admit (withHole, []) [1 .. count]
      finish state number =
        fst
          ( commitApplicationOperationCompletion
              ( checkedPure
                  "complete shuffled operation"
                  ( prepareApplicationOperationCompletion
                      (checkedMaybe "shuffled acceptance position" (lookup number positions))
                      structuralResult
                      state
                  )
              )
          )
      checkpoints = scanl finish allPending completionOrder
      blocked state = case Application.prepareApplicationReceiptRetirement binding session (applicationReceiptRetirement (Just count) Nothing) state of
        Left (ApplicationSessionRejected ApplicationReceiptRetirementNotReady) -> True
        _ -> False
      completed =
        fst
          ( commitApplicationWaitCompletions
              (checkedPure "complete pinned wait" (prepareApplicationWaitCompletions [wait] (last checkpoints)))
          )
      retire through lifecycle state =
        Application.commitApplicationReceiptRetirement
          ( checkedPure
              "retire completed receipts"
              ( Application.prepareApplicationReceiptRetirement
                  binding
                  session
                  (applicationReceiptRetirement (Just through) lifecycle)
                  state
              )
          )
      zeroRetired = retire 0 Nothing completed
      retired = retire count (Just 7) zeroRetired
      exactRetries =
        and
          [ isRetained
              (RequestRetired (requestId number) count)
              (classifyApplicationRequest session binding (requestId number) (ReadApplication emptyQuery) retired)
              && applicationRequestResult session binding (requestId number) retired == Right (RequestRetired (requestId number) count)
          | number <- [0 .. count]
          ]
      staleCancel = case prepareApplicationWaitCancellation session binding low wait retired of
        Right (ApplicationWaitCancellationReply (RequestRetired actual through)) -> actual == low && through == count
        _ -> False
   in counterexample
        "retirement lost an unresolved owner, forgot its seal, or changed on repeated progress"
        ( all blocked checkpoints
            && length (Application.applicationRequestEntries session zeroRetired) == fromIntegral count
            && null (Application.applicationRequestEntries session retired)
            && exactRetries
            && staleCancel
            && retire count (Just 7) retired == retired
            && retire 0 (Just 2) retired == retired
            && Application.applicationReceiptRetirementProgress session retired == Just (applicationReceiptRetirement (Just count) (Just 7))
        )
  where
    count = 1 + fromIntegral (generated `mod` 24)

propFlowingReceiptRetirement :: Word8 -> Property
propFlowingReceiptRetirement generated =
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 992 fixture.state
      (session, _, binding, _) = opened acceptance
      advance state number =
        let (completed, _) = complete session binding (requestId number) (ReadApplication emptyQuery) (ReadCompleted []) state
         in Application.commitApplicationReceiptRetirement
              ( checkedPure
                  "flowing retirement"
                  ( Application.prepareApplicationReceiptRetirement
                      binding
                      session
                      (applicationReceiptRetirement (Just number) Nothing)
                      completed
                  )
              )
      checkpoints = scanl advance afterOpen [0 .. fromIntegral generated]
   in counterexample
        "flowing receipt history did not stay empty after each acknowledged completion"
        (all (null . Application.applicationRequestEntries session) checkpoints)

caseImmediateDisconnectedOperationCompletion :: Assertion
caseImmediateDisconnectedOperationCompletion = do
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 993 fixture.state
      (session, _, binding, _) = opened acceptance
      candidate = firstCandidate session binding (requestId 0) structuralCall afterOpen
      position = applicationRequestCandidatePosition candidate
      (accepted, _) = commitApplicationRequest (prepareApplicationOperationAcceptance candidate StructuralStabilizationPending)
      termination = checkedPure "immediate unexpected disconnect" (Application.prepareApplicationBindingTermination binding accepted)
      lost = Application.commitApplicationRecoverySweep termination
      completed = checkedPure "late detached semantic completion" (prepareApplicationOperationCompletion position structuralResult lost)
  assertEqual "disconnect immediately forgets the session" Nothing (Application.applicationSessionProcess session lost)
  assertEqual "accepted work has no surviving application result route" (ApplicationOperationNoLiveRequest position) (preparedApplicationOperationCompletionOutcome completed)
  assertBool "late completion does not recreate the owner" (lost == fst (commitApplicationOperationCompletion completed))

caseRetirementTerminalBodies :: Assertion
caseRetirementTerminalBodies = do
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 994 fixture.state
      (session, token, binding, openCursor) = opened acceptance
      cancelledId = requestId 0
      (pending, waitReply) =
        commitApplicationRequest
          ( prepareApplicationWaitAcceptance
              (firstCandidate session binding cancelledId (WaitApplication []) afterOpen)
          )
      wait = acceptedWait waitReply
      cancellation =
        exactCancellation
          "retireable cancellation"
          ( checkedPure
              "cancel retireable wait"
              (prepareApplicationWaitCancellation session binding cancelledId wait pending)
          )
      (cancelled, _) = commitApplicationWaitCancellation cancellation
      (rejected, _) =
        commitApplicationRequest
          ( prepareApplicationRequestRejection
              (firstCandidate session binding (requestId 2) (ReadApplication emptyQuery) cancelled)
              ApplicationQueryPredicateMismatch
          )
      (completed, _) = complete session binding (requestId 4) (ReadApplication emptyQuery) (ReadCompleted []) rejected
      (resumed, resumedAcceptance) =
        commitApplicationSessionAcceptance
          (checkedPure "take a retained resume summary" (prepareApplicationSessionResume session token openCursor completed))
      resumedBinding = sessionAcceptanceBinding resumedAcceptance
      retired =
        Application.commitApplicationReceiptRetirement
          ( checkedPure
              "retire all terminal bodies"
              ( Application.prepareApplicationReceiptRetirement
                  resumedBinding
                  session
                  (applicationReceiptRetirement (Just 4) Nothing)
                  resumed
              )
          )
      (_, reoffered) =
        commitApplicationSessionAcceptance
          (checkedPure "reoffer pruned summary" (prepareApplicationSessionResume session token openCursor retired))
  assertEqual "retired terminal bodies are absent" [] (Application.applicationRequestEntries session retired)
  assertEqual "retained resume snapshot releases the same rows" [] (requestSummaryEntries (resumedSummary reoffered))
  assertEqual
    "an unused ID in the retired prefix remains sealed"
    (Right (RequestRetired (requestId 1) 4))
    (applicationRequestResult session resumedBinding (requestId 1) retired)

propExactRetry :: Word32 -> Property
propExactRetry generated =
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 1 fixture.state
      (session, _, binding, _) = opened acceptance
      request = requestId (fromIntegral generated)
      call = ReadApplication emptyQuery
      candidate = firstCandidate session binding request call afterOpen
      (_, reply) =
        commitApplicationRequest
          (prepareApplicationRequestCompletion candidate (ReadCompleted []))
      retained = classifyApplicationRequest session binding request call afterOpenWithRequest
      conflict =
        classifyApplicationRequest
          session
          binding
          request
          (LocalTakeApplication emptyQuery)
          afterOpenWithRequest
      lookedUp = applicationRequestResult session binding request afterOpenWithRequest
      afterOpenWithRequest =
        fst
          ( commitApplicationRequest
              (prepareApplicationRequestCompletion candidate (ReadCompleted []))
          )
   in counterexample
        "retry, conflict, or lookup changed the retained identity"
        ( isRetained reply retained
            && isConflict request conflict
            && lookedUp == Right reply
        )

caseAcceptancePositions :: Assertion
caseAcceptancePositions = do
  let fixture = requestFixture
      (afterFirstOpen, firstAcceptance) = open fixture 10 fixture.state
      (afterSecondOpen, secondAcceptance) = open fixture 11 afterFirstOpen
      (firstSession, _, firstBinding, _) = opened firstAcceptance
      (secondSession, _, secondBinding, _) = opened secondAcceptance
      firstRequest = requestId 7
      rejectedRequest = requestId 8
      secondRequest = requestId 7
      (afterFirst, _) =
        complete
          firstSession
          firstBinding
          firstRequest
          (ReadApplication emptyQuery)
          (ReadCompleted [])
          afterSecondOpen
      rejectedCandidate =
        firstCandidate
          firstSession
          firstBinding
          rejectedRequest
          (LocalTakeApplication emptyQuery)
          afterFirst
      (afterRejected, _) =
        commitApplicationRequest
          ( prepareApplicationRequestRejection
              rejectedCandidate
              ApplicationQueryPredicateMismatch
          )
      (afterSecond, _) =
        complete
          secondSession
          secondBinding
          secondRequest
          (ReadApplication emptyQuery)
          (ReadCompleted [])
          afterRejected
      witness = applicationRequestStateWitness afterSecond
      firstPosition = requestPosition firstSession firstRequest witness
      rejectedPosition = requestPosition firstSession rejectedRequest witness
      secondPosition = requestPosition secondSession secondRequest witness
  assertEqual "first accepted request starts at one" (Just 1) (positionOrdinal <$> firstPosition)
  assertEqual "semantic rejection has no process position" Nothing rejectedPosition
  assertEqual "another session shares the process counter" (Just 2) (positionOrdinal <$> secondPosition)
  assertEqual
    "only accepted requests advanced the process counter"
    (Just 3)
    (lookup fixture.process (applicationRequestNextAcceptancePositions witness))

caseOperationLifecycle :: Assertion
caseOperationLifecycle = do
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 15 fixture.state
      (session, _, binding, _) = opened acceptance
      request = requestId 14
      call = structuralCall
      candidate = firstCandidate session binding request call afterOpen
      position = applicationRequestCandidatePosition candidate
      reservedWait = applicationRequestCandidateWaitId candidate
      (afterAcceptance, acceptedReply) =
        commitApplicationRequest
          ( prepareApplicationOperationAcceptance
              candidate
              StructuralStabilizationPending
          )
      retainedAcceptance =
        classifyApplicationRequest
          session
          binding
          request
          call
          afterAcceptance
      lookedUpAcceptance =
        applicationRequestResult session binding request afterAcceptance
      result = structuralResult
      preparedCompletion =
        checkedPure
          "prepare exact operation completion"
          (prepareApplicationOperationCompletion position result afterAcceptance)
      (afterCompletion, completionOutcome) =
        commitApplicationOperationCompletion preparedCompletion
      completedReply =
        checkedPure
          "look up completed operation"
          (applicationRequestResult session binding request afterCompletion)
      retainedCompletion =
        classifyApplicationRequest
          session
          binding
          request
          call
          afterCompletion
  assertEqual
    "first admission retains the nonterminal operation result"
    (OperationAccepted request StructuralStabilizationPending)
    (retainedBody acceptedReply)
  assertBool
    "an exact retry reoffers the accepted reply"
    (isRetained acceptedReply retainedAcceptance)
  assertEqual
    "GetResult reoffers the accepted reply"
    (Right acceptedReply)
    lookedUpAcceptance
  assertCancellationReply
    "an accepted operation cannot be cancelled as a wait"
    acceptedReply
    ( checkedPure
        "check operation cancellation"
        ( prepareApplicationWaitCancellation
            session
            binding
            request
            reservedWait
            afterAcceptance
        )
    )
  case completionOutcome of
    ApplicationOperationCompleted (Just observation) -> do
      assertEqual
        "live completion targets the current binding"
        binding
        (applicationOperationCompletionBinding observation)
      assertEqual
        "live completion names the exact request"
        request
        (applicationOperationCompletionRequestId observation)
      assertEqual
        "live completion carries the terminal result"
        result
        (applicationOperationCompletionResult observation)
      case completedReply of
        RetainedRequestReply cursor (Completed actualRequest actualResult) -> do
          assertEqual "completion retains the exact request" request actualRequest
          assertEqual "completion retains the exact result" result actualResult
          assertEqual
            "delivery and retained completion share one cursor"
            cursor
            (applicationOperationCompletionCursor observation)
        reply -> assertFailure ("expected retained terminal reply, got " <> show reply)
    outcome -> assertFailure ("expected live operation completion, got " <> show outcome)
  assertBool
    "an exact retry reoffers the completed reply"
    (isRetained completedReply retainedCompletion)
  case prepareApplicationOperationCompletion position result afterCompletion of
    Left (ApplicationSessionInvariantFault _) -> pure ()
    Left problem -> assertFailure ("terminal replay produced the wrong fault: " <> show problem)
    Right _ -> assertFailure "a completed operation was allowed to complete again"

caseDisconnectedOperationCompletion :: Assertion
caseDisconnectedOperationCompletion = do
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 16 fixture.state
      (session, token, binding, _) = opened acceptance
      request = requestId 15
      call = structuralCall
      candidate = firstCandidate session binding request call afterOpen
      position = applicationRequestCandidatePosition candidate
      (afterAcceptance, acceptedReply) =
        commitApplicationRequest
          ( prepareApplicationOperationAcceptance
              candidate
              StructuralStabilizationPending
          )
      acceptedCursor = retainedCursor acceptedReply
      afterLoss =
        commitApplicationBindingLoss
          (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) binding afterAcceptance)
      result = structuralResult
      preparedCompletion =
        checkedPure
          "prepare disconnected operation completion"
          (prepareApplicationOperationCompletion position result afterLoss)
      (afterCompletion, completionOutcome) =
        commitApplicationOperationCompletion preparedCompletion
      (afterResume, resumed) =
        commitApplicationSessionAcceptance
          ( checkedPure
              "resume after disconnected completion"
              ( prepareApplicationSessionResume
                  session
                  token
                  acceptedCursor
                  afterCompletion
              )
          )
      resumedBinding = sessionAcceptanceBinding resumed
      currentResult =
        checkedPure
          "look up retained disconnected completion"
          (applicationRequestResult session resumedBinding request afterResume)
  assertEqual
    "a disconnected completion retains no delivery observation"
    (ApplicationOperationCompleted Nothing)
    completionOutcome
  assertEqual
    "resume summary exposes the retained terminal status"
    [ApplicationRequestCompleted result]
    (fmap requestSummaryEntryStatus (requestSummaryEntries (resumedSummary resumed)))
  assertEqual
    "GetResult after resume reoffers the retained completion body"
    (Completed request result)
    (retainedBody currentResult)

caseEndedOperationCompletion :: Assertion
caseEndedOperationCompletion = do
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 17 fixture.state
      (session, _, binding, _) = opened acceptance
      request = requestId 16
      candidate =
        firstCandidate
          session
          binding
          request
          structuralCall
          afterOpen
      position = applicationRequestCandidatePosition candidate
      (afterAcceptance, _) =
        commitApplicationRequest
          ( prepareApplicationOperationAcceptance
              candidate
              StructuralStabilizationPending
          )
      preparedEnd =
        checkedPure
          "prepare end with accepted operation"
          (prepareApplicationSessionEnd session binding afterAcceptance)
      afterEnd = commitApplicationSessionEnd preparedEnd
      beforeCompletionWitness = applicationRequestStateWitness afterEnd
      preparedCompletion =
        checkedPure
          "prepare completion after session end"
          (prepareApplicationOperationCompletion position structuralResult afterEnd)
      (afterCompletion, completionOutcome) =
        commitApplicationOperationCompletion preparedCompletion
  assertEqual
    "accepted operations are not handed off as cancellable waits"
    []
    (preparedApplicationSessionEndWaitIds preparedEnd)
  assertEqual
    "a late completion has an explicit no-live-request outcome"
    (ApplicationOperationNoLiveRequest position)
    (preparedApplicationOperationCompletionOutcome preparedCompletion)
  assertEqual
    "commit preserves the same no-live-request outcome"
    (ApplicationOperationNoLiveRequest position)
    completionOutcome
  assertEqual
    "late completion leaves the application owner unchanged"
    beforeCompletionWitness
    (applicationRequestStateWitness afterCompletion)

caseResumeSnapshot :: Assertion
caseResumeSnapshot = do
  let fixture = requestFixture
      (afterOpen, openAcceptance) = open fixture 20 fixture.state
      (session, token, binding, openCursor) = opened openAcceptance
      pendingRequest = requestId 9
      completedRequest = requestId 3
      pendingCall = WaitApplication []
      pendingCandidate =
        firstCandidate session binding pendingRequest pendingCall afterOpen
      (afterPending, pendingReply) =
        commitApplicationRequest
          (prepareApplicationWaitAcceptance pendingCandidate)
      wait = acceptedWait pendingReply
      (afterCompleted, _) =
        complete
          session
          binding
          completedRequest
          (ReadApplication emptyQuery)
          (ReadCompleted [])
          afterPending
      (afterResume, resumed) =
        commitApplicationSessionAcceptance
          ( checkedPure
              "prepare summary resume"
              ( prepareApplicationSessionResume
                  session
                  token
                  openCursor
                  afterCompleted
              )
          )
      resumedBinding = sessionAcceptanceBinding resumed
      originalSummary = resumedSummary resumed
      preparedWake =
        checkedPure
          "prepare wait completion"
          (prepareApplicationWaitCompletions [wait] afterResume)
      wakes = preparedApplicationWaitWakes preparedWake
      (afterWake, _) = commitApplicationWaitCompletions preparedWake
      (_, reoffered) =
        commitApplicationSessionAcceptance
          ( checkedPure
              "prepare exact resume reoffer"
              ( prepareApplicationSessionResume
                  session
                  token
                  openCursor
                  afterWake
              )
          )
      currentPendingResult =
        checkedPure
          "lookup completed wait"
          (applicationRequestResult session resumedBinding pendingRequest afterWake)
  assertEqual
    "summary entries are RequestId-sorted"
    [completedRequest, pendingRequest]
    (fmap requestSummaryEntryRequestId (requestSummaryEntries originalSummary))
  assertEqual
    "the retained snapshot saw the wait as pending"
    (ApplicationRequestPending wait)
    ( requestSummaryEntryStatus
        (last (requestSummaryEntries originalSummary))
    )
  assertEqual
    "reoffer retains the exact old summary and cursor"
    resumed
    reoffered
  case wakes of
    [wake] -> do
      assertEqual "wake names the exact request" pendingRequest (applicationWaitWakeRequestId wake)
      assertEqual "wake names the exact wait" wait (applicationWaitWakeWaitId wake)
      assertEqual "wake reports readiness" WaitReady (applicationWaitWakeResult wake)
      case currentPendingResult of
        RetainedRequestReply cursor _ ->
          assertEqual
            "wake and cached completion share one cursor"
            cursor
            (applicationWaitWakeCursor wake)
        reply -> assertFailure ("expected retained completed lookup, got " <> show reply)
    actual -> assertFailure ("expected one wake, got " <> show actual)
  case currentPendingResult of
    RetainedRequestReply _ (Completed actualRequest (WaitCompleted WaitReady)) ->
      assertEqual "individual lookup sees the newer completion" pendingRequest actualRequest
    reply -> assertFailure ("expected completed wait lookup, got " <> show reply)

caseLocalizedCompletion :: Assertion
caseLocalizedCompletion = do
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 25 fixture.state
      (session, _, binding, _) = opened acceptance
      request = requestId 12
      candidate =
        firstCandidate
          session
          binding
          request
          (ReadApplication emptyQuery)
          afterOpen
      internalValue =
        Domain.globalUniqueIdValue
          ( checkedPure
              "localized completion identity"
              (mkGlobalUniqueId (ByteString.replicate 32 0xca))
          )
      preparedLocalization =
        checkedPure
          "prepare localized completion value"
          ( prepareOrdinaryApplicationValueLocalization
              (processRoleView [])
              fixture.process
              internalValue
              afterOpen
          )
      (localizedState, localizedValue) =
        commitOrdinaryApplicationValueLocalization preparedLocalization
      preparedRequest =
        checkedPure
          "combine localized state with request"
          ( prepareApplicationRequestCompletionFromLocalizedState
              candidate
              localizedState
              (ReadCompleted [localizedValue])
          )
      (successor, _) = commitApplicationRequest preparedRequest
  assertEqual
    "the localized alias remains resolvable after retaining the result"
    (Right internalValue)
    ( globalizeOrdinaryApplicationValue
        (processRoleView [])
        fixture.process
        localizedValue
        successor
    )
  let (changedOwnership, _) = open fixture 26 afterOpen
  case prepareApplicationRequestCompletionFromLocalizedState
    candidate
    changedOwnership
    (ReadCompleted []) of
    Left _ -> pure ()
    Right _ -> assertFailure "changed session ownership was accepted as localization-only"

caseWaitLifecycle :: Assertion
caseWaitLifecycle = do
  let fixture = requestFixture
      (afterOpen, acceptance) = open fixture 30 fixture.state
      (session, _, binding, _) = opened acceptance
      firstRequest = requestId 1
      secondRequest = requestId 2
      firstWaitCandidate =
        firstRequestCandidate session binding firstRequest afterOpen
      (afterFirst, firstReply) =
        commitApplicationRequest
          (prepareApplicationWaitAcceptance firstWaitCandidate)
      firstWait = acceptedWait firstReply
      secondCandidate =
        firstRequestCandidate session binding secondRequest afterFirst
      (afterSecond, secondReply) =
        commitApplicationRequest
          (prepareApplicationWaitAcceptance secondCandidate)
      secondWait = acceptedWait secondReply
      preparedCancellation =
        exactCancellation
          "prepare exact cancellation"
          ( checkedPure
              "check exact cancellation"
              ( prepareApplicationWaitCancellation
                  session
                  binding
                  firstRequest
                  firstWait
                  afterSecond
              )
          )
      (afterCancellation, cancellationReply) =
        commitApplicationWaitCancellation preparedCancellation
  assertEqual
    "successful cancellation echoes the exact request and wait"
    (Cancelled firstRequest firstWait)
    (retainedBody cancellationReply)
  assertCancellationReply
    "duplicate cancellation reoffers the retained terminal reply"
    cancellationReply
    ( checkedPure
        "check duplicate cancellation"
        ( prepareApplicationWaitCancellation
            session
            binding
            firstRequest
            firstWait
            afterCancellation
        )
    )
  assertCancellationReply
    "a wait from another request conflicts with the pending request"
    (RequestConflict secondRequest)
    ( checkedPure
        "check wrong cancellation"
        ( prepareApplicationWaitCancellation
            session
            binding
            secondRequest
            firstWait
            afterCancellation
        )
    )
  let preparedEnd =
        checkedPure
          "prepare end with pending wait"
          (prepareApplicationSessionEnd session binding afterCancellation)
  assertEqual
    "session end hands off only its remaining live wait"
    [secondWait]
    (preparedApplicationSessionEndWaitIds preparedEnd)
  let afterEnd = commitApplicationSessionEnd preparedEnd
  assertRejected
    "ended request namespace is no longer queryable"
    ApplicationSessionNotLive
    (applicationRequestResult session binding secondRequest afterEnd)

caseRequestBindingChecks :: Assertion
caseRequestBindingChecks = do
  let fixture = requestFixture
      (afterFirst, first) = open fixture 40 fixture.state
      (afterSecond, second) = open fixture 41 afterFirst
      (firstSession, firstToken, firstBinding, firstCursor) = opened first
      (secondSession, _, secondBinding, _) = opened second
      request = requestId 4
      call = ReadApplication emptyQuery
  assertRejected
    "binding and separately supplied session must agree"
    ApplicationRequestSessionMismatch
    (classifyApplicationRequest firstSession secondBinding request call afterSecond)
  let (afterResume, _) =
        commitApplicationSessionAcceptance
          ( checkedPure
              "prepare binding advance"
              ( prepareApplicationSessionResume
                  firstSession
                  firstToken
                  firstCursor
                  afterSecond
              )
          )
  assertRejected
    "an old same-session binding is not current"
    ApplicationRequestBindingMismatch
    (classifyApplicationRequest firstSession firstBinding request call afterResume)
  let currentBinding =
        case applicationRequestResult secondSession secondBinding (requestId 99) afterResume of
          Right _ -> secondBinding
          Left problem -> error ("second binding unexpectedly invalid: " <> show problem)
      afterLoss =
        commitApplicationBindingLoss
          (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) currentBinding afterResume)
  assertRejected
    "a lost current binding cannot accept request ingress"
    ApplicationRequestBindingNotLive
    (classifyApplicationRequest secondSession currentBinding request call afterLoss)

emptyQuery :: ApplicationQuery
emptyQuery = ApplicationQuery Set.empty QueryAlways

structuralCall :: ApplicationOperation
structuralCall =
  ForwardApplication
    (asPrivateNablaId (checkedPure "structural writer" (mkPrivateUniqueId 1)))
    (asPrivateObjectId (checkedPure "forwarded object" (mkPrivateUniqueId 2)))

structuralResult :: RegularCallResult
structuralResult = ForwardCompleted ForwardAccepted

isRetained ::
  ApplicationRequestReply ->
  Either ApplicationSessionTransitionError ApplicationRequestClassification ->
  Bool
isRetained expected = \case
  Right (RetainedApplicationRequest actual) -> actual == expected
  _ -> False

isConflict ::
  RequestId ->
  Either ApplicationSessionTransitionError ApplicationRequestClassification ->
  Bool
isConflict expected = \case
  Right (ConflictingApplicationRequest (RequestConflict actual)) ->
    actual == expected
  _ -> False

firstRequestCandidate ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  State ->
  ApplicationRequestCandidate
firstRequestCandidate session binding request =
  firstCandidate session binding request (WaitApplication [])

firstCandidate ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  ApplicationOperation ->
  State ->
  ApplicationRequestCandidate
firstCandidate session binding request call state =
  case classifyApplicationRequest session binding request call state of
    Right (FirstApplicationRequest candidate) -> candidate
    Right classification -> error ("expected first request, got " <> classificationName classification)
    Left problem -> error ("request classification failed: " <> show problem)

classificationName :: ApplicationRequestClassification -> String
classificationName = \case
  FirstApplicationRequest _ -> "first"
  RetainedApplicationRequest reply -> "retained " <> show reply
  AttachedApplicationRequest requestClass -> "attached " <> show requestClass
  ConflictingApplicationRequest reply -> "conflicting " <> show reply

complete ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  ApplicationOperation ->
  RegularCallResult ->
  State ->
  (State, ApplicationRequestReply)
complete session binding request call result state =
  commitApplicationRequest
    ( prepareApplicationRequestCompletion
        (firstCandidate session binding request call state)
        result
    )

acceptedWait :: ApplicationRequestReply -> WaitId
acceptedWait = \case
  RetainedRequestReply _ (WaitAccepted _ wait) -> wait
  reply -> error ("expected accepted wait, got " <> show reply)

retainedBody :: ApplicationRequestReply -> RetainedRequestReplyBody
retainedBody = \case
  RetainedRequestReply _ body -> body
  reply -> error ("expected retained reply, got " <> show reply)

retainedCursor :: ApplicationRequestReply -> ApplicationReplyCursor
retainedCursor = \case
  RetainedRequestReply cursor _ -> cursor
  reply -> error ("expected retained reply, got " <> show reply)

resumedSummary :: ApplicationSessionAcceptance -> ApplicationRequestSummary
resumedSummary acceptance = case sessionAcceptanceReply acceptance of
  SessionResumed _ summary -> summary
  reply -> error ("expected resumed acknowledgement, got " <> show reply)

positionOrdinal :: ProcessAcceptancePosition -> Word64
positionOrdinal = processAcceptancePositionOrdinal

requestPosition ::
  ApplicationSessionId ->
  RequestId ->
  ApplicationRequestStateWitness ->
  Maybe ProcessAcceptancePosition
requestPosition session request witness = do
  owner <- find ((== session) . applicationRequestOwnerSession) (applicationRequestOwnerWitnesses witness)
  requestWitness <- find ((== request) . applicationRequestWitnessId) (applicationRequestOwnerRequests owner)
  applicationRequestWitnessPosition requestWitness

data RequestFixture = RequestFixture
  { herald :: HeraldEpoch,
    process :: ProcessEpochId,
    attachment :: ApplicationAttachment,
    state :: State
  }

requestFixture :: RequestFixture
requestFixture =
  case bootstrapped (checkedBootstraps fixtureLocalBootstrapIds) of
    (first : _, state) ->
      RequestFixture
        { herald = checkedLocalHeraldEpoch fixtureCheckedGenesis,
          process = appliedProcessEpochId first,
          attachment =
            applicationAttachmentForBootstrap
              (appliedBootstrapManifestId first),
          state = state
        }
    _ -> error "request fixture has no resident process"

open ::
  RequestFixture ->
  Word64 ->
  State ->
  (State, ApplicationSessionAcceptance)
open fixture nonceWord state =
  commitApplicationSessionAcceptance
    ( checkedPure
        "prepare application session open"
        ( prepareApplicationSessionOpen
            fixture.herald
            fixture.attachment
            (clientNonce nonceWord)
            state
        )
    )

opened ::
  ApplicationSessionAcceptance ->
  ( ApplicationSessionId,
    ApplicationResumeToken,
    ApplicationSessionBinding,
    ApplicationReplyCursor
  )
opened acceptance = case sessionAcceptanceReply acceptance of
  SessionOpened session token _ ->
    ( session,
      token,
      sessionAcceptanceBinding acceptance,
      sessionAcceptanceCursor acceptance
    )
  reply -> error ("expected opened acknowledgement, got " <> show reply)

checkedBootstraps :: [BootstrapManifestId] -> CheckedInitialBootstraps
checkedBootstraps manifests =
  checkedPure
    "checked initial bootstraps"
    ( checkInitialBootstraps
        fixtureCheckedGenesis
        (PrimordialProcessManifest manifests)
    )

bootstrapped :: CheckedInitialBootstraps -> ([AppliedProcessBootstrap], State)
bootstrapped checkedInitial =
  (resident, bootstrapApplicationState owners)
  where
    resident =
      filter
        (\bootstrap -> appliedBootstrapManifestId bootstrap `elem` fixtureLocalBootstrapIds)
        (checkedInitialBootstraps checkedInitial)
    oracle = OracleProjection.initialState fixtureCheckedGenesis checkedInitial
    sorts = SortRegistry.initialState fixtureCheckedGenesis
    views =
      bootstrapViews
        (OracleProjection.oracleView oracle)
        (SortRegistry.sortView sorts)
    initialOwners =
      checkedPure
        "initial bootstrap owners"
        (initialBootstrapOwners fixtureCheckedGenesis)
    owners = checkedPure "resident process bootstrap" (foldM (install views) initialOwners resident)

install ::
  BootstrapViews ->
  BootstrapOwners ->
  AppliedProcessBootstrap ->
  Either BootstrapInvariant BootstrapOwners
install views owners bootstrap = do
  prepared <- prepareProcessBootstrap views bootstrap owners
  Right (fst (commitProcessBootstrap prepared))

assertRejected ::
  String ->
  ApplicationSessionRejection ->
  Either ApplicationSessionTransitionError value ->
  Assertion
assertRejected description expected = \case
  Left (ApplicationSessionRejected actual) ->
    assertEqual description expected actual
  Left problem -> assertFailure (description <> ": wrong fault " <> show problem)
  Right _ -> assertFailure (description <> ": unexpectedly succeeded")

exactCancellation ::
  String ->
  ApplicationWaitCancellation ->
  PreparedApplicationWaitCancellation
exactCancellation _ (ExactApplicationWaitCancellation prepared) = prepared
exactCancellation context classification =
  error (context <> ": got " <> cancellationName classification)

cancellationName :: ApplicationWaitCancellation -> String
cancellationName = \case
  ApplicationWaitCancellationReply reply -> "reply " <> show reply
  ExactApplicationWaitCancellation _ -> "exact cancellation"

assertCancellationReply ::
  String ->
  ApplicationRequestReply ->
  ApplicationWaitCancellation ->
  Assertion
assertCancellationReply description expected = \case
  ApplicationWaitCancellationReply actual ->
    assertEqual description expected actual
  ExactApplicationWaitCancellation _ ->
    assertFailure (description <> ": unexpectedly prepared cancellation")

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure context = either (error . ((context <> ": ") <>) . show) id

checkedMaybe :: String -> Maybe value -> value
checkedMaybe description = maybe (error description) id
