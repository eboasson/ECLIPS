{-# LANGUAGE OverloadedStrings #-}

module LifecycleProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Client qualified as Client
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Identity
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.Operation (ApplicationOperation (WaitApplication))
import Eclips.Application.Types.Rejection (ApplicationRejection (ApplicationOperateNotPermitted))
import Eclips.Protocol.Application.Types qualified as Protocol
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (NonNegative (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "lifecycle client"
    [ testProperty "prepared initial retry retains the same claim and descriptor" propClaimRetry,
      testCase "prepared claim confirms on the same connection and later loss is terminal" casePreparedHandoff,
      testCase "prepared receipt confirmation preserves an intervening isolation drain" casePreparedHandoffDuringIsolation,
      testProperty "two sessions have distinct lifecycle request correlations" propSessionScope,
      testCase "lost own-End result uses restricted recovery, never resume" caseEndRecovery,
      testCase "multiple pending own-End requests each retain terminal recovery" caseMultipleEndRecovery,
      testCase "standalone own-End query opens no session" caseRestrictedRecovery,
      testCase "preparation connection loss is terminal and never resubmits" casePreparationRetry,
      testCase "lifecycle result kind mismatch faults at the client boundary" caseResultMismatch
    ]

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
scope :: Bytes.ByteString
scope = Bytes.replicate 32 0x51
attachment :: Protocol.ApplicationAttachmentClaim
attachment = checked (Protocol.applicationAttachmentClaim scope)
recovery :: Client.ApplicationClientRecoveryConfiguration
recovery = checked (Client.applicationClientRecoveryConfiguration 10 1000)
instant :: Word64 -> Client.ApplicationClientMonotonicInstant
instant = Client.applicationClientMonotonicInstant
advance :: Client.ApplicationClientInput -> Client.ApplicationClientState -> (Client.ApplicationClientState, [Client.ApplicationClientEffect])
advance input state = case checked (Client.stepApplicationClient input state) of
  Client.ApplicationClientAdvanced successor effects -> (successor, Client.applicationClientEffects effects)
  other -> error (show other)
attemptOf :: Client.ApplicationClientState -> Client.ApplicationTransportAttemptGeneration
attemptOf = maybe (error "missing attempt") id . Client.applicationClientCurrentTransportAttempt
server :: Word64 -> Protocol.ApplicationServerDto -> Client.ApplicationClientState -> (Client.ApplicationClientState, [Client.ApplicationClientEffect])
server time dto state = advance (Client.ServerDtoReceived (attemptOf state) (instant time) dto) state
initial :: Client.ApplicationClientState
initial = Client.initialApplicationClient recovery attachment (Protocol.applicationClientNonce 7)
connected :: Client.ApplicationClientState -> (Client.ApplicationClientState, [Client.ApplicationClientEffect])
connected initialState =
  let (started, _) = advance Client.Start initialState
   in advance (Client.TransportAvailable (attemptOf started) (instant 0)) started
active :: Word64 -> Client.ApplicationClientState
active ordinal = fst (server 1 opened opening)
  where
    (opening, _) = connected initial
    opened = Protocol.SessionOpened (checked (Protocol.applicationReplyCursorClaim 1)) (session ordinal) (checked (Protocol.applicationResumeTokenClaim scope ordinal)) startup
session :: Word64 -> Protocol.ApplicationSessionClaim
session ordinal = checked (Protocol.applicationSessionClaim scope ordinal)
startup :: Access.ApplicationStartupAccess
startup = Access.applicationStartupAccess (Identity.asPrivateProcessId (checked (Identity.mkPrivateUniqueId 1))) (checked (Access.primordialAccess [] Set.empty Nothing))
selection :: Access.PrimordialSelection
selection = checked (Access.primordialSelection [] Set.empty Nothing)
locator :: Lifecycle.HeraldLocator
locator = checked (Lifecycle.heraldLocator "127.0.0.1" 7000)
descriptor :: Lifecycle.ConnectionDescriptor
descriptor = checked (Lifecycle.connectionDescriptor locator (Bytes.replicate 32 1) (Bytes.replicate 32 2) scope)
invocation :: Client.ApplicationInvocationId
invocation = Client.applicationInvocationId 4
requestFor :: Word64 -> Lifecycle.LifecycleRequestId
requestFor ordinal = checked (Lifecycle.lifecycleRequestId scope ordinal 4)

propClaimRetry :: Word8 -> Bool
propClaimRetry byte = all (== Protocol.ClaimInitial descriptor claim) sent && length sent == 2
  where
    claim = checked (Lifecycle.initialClaimId (Bytes.replicate 32 byte))
    (opening, firstEffects) = connected (Client.initialPreparedApplicationClient recovery descriptor claim)
    (lost, lossEffects) = advance (Client.TransportLost (attemptOf opening) (instant 1)) opening
    (retryScope, oldAttempt, due) = case lossEffects of
      [Client.ArmTransportRetry a b c] -> (a, b, c)
      _ -> error "no opening retry"
    (retrying, _) = advance (Client.TransportRetryElapsed retryScope oldAttempt due) lost
    (_, secondEffects) = advance (Client.TransportAvailable (attemptOf retrying) due) retrying
    sent = [dto | Client.SendApplicationDto dto <- firstEffects <> secondEffects]

propSessionScope :: NonNegative Int -> Bool
propSessionScope (NonNegative sample) = first /= second && expected first ordinal && expected second (ordinal + 1)
  where
    ordinal = fromIntegral (sample `mod` 10000)
    command = Lifecycle.BeginChild locator selection
    effectFor n = snd (advance (Client.SubmitLifecycle invocation command) (active n))
    assigned effects = case [request | Client.LifecycleAssigned _ request <- effects] of
      [request] -> request
      _ -> error "missing lifecycle assignment"
    first = assigned (effectFor ordinal)
    second = assigned (effectFor (ordinal + 1))
    expected request n = request == requestFor n

casePreparedHandoff :: IO ()
casePreparedHandoff = do
  let claim = checked (Lifecycle.initialClaimId scope)
      (opening, _) = connected (Client.initialPreparedApplicationClient recovery descriptor claim)
      originalAttempt = attemptOf opening
      (confirming, openingEffects) = server 1 (Protocol.SessionOpened (cursor 1) (session 1) (checked (Protocol.applicationResumeTokenClaim scope 1)) startup) opening
      (ready, readyEffects) = server 2 (Protocol.ReceiptsRetired (session 1) mempty) confirming
      (lost, lossEffects) = advance (Client.TransportLost originalAttempt (instant 3)) ready
      startups effects = [access | Client.SessionReady access <- effects]
  assertEqual "SessionOpened alone does not expose startup" [] (startups openingEffects)
  assertEqual "initial result is acknowledged on the original connection" [Protocol.RetireReceipts (session 1) mempty] [dto | Client.SendApplicationDto dto <- openingEffects]
  assertEqual "no deliberate close or redial" [] [effect | effect <- openingEffects, case effect of Client.ResetTransport -> True; Client.RequestTransport _ -> True; _ -> False]
  assertEqual "same connection remains established" originalAttempt (attemptOf ready)
  assertEqual "acknowledged startup is exposed once" [startup] (startups readyEffects)
  assertEqual "physical loss ends the application lifetime" Client.ApplicationClientPermanentlyUnavailable (Client.applicationClientPhase lost)
  assertEqual "no session recovery" [Client.HeraldPermanentlyUnavailableObserved Client.SessionNoLongerLive, Client.CloseTransport] lossEffects
  where
    cursor = checked . Protocol.applicationReplyCursorClaim

casePreparedHandoffDuringIsolation :: IO ()
casePreparedHandoffDuringIsolation = do
  let claim = checked (Lifecycle.initialClaimId scope)
      (opening, _) = connected (Client.initialPreparedApplicationClient recovery descriptor claim)
      originalAttempt = attemptOf opening
      (confirming, openingEffects) = server 1 (Protocol.SessionOpened (cursor 1) (session 1) (checked (Protocol.applicationResumeTokenClaim scope 1)) startup) opening
      notice = Protocol.HeraldIsolationBegun (cursor 2) (session 1) Protocol.ResidentProcessesZombie Protocol.HeraldIsolatedDto
      (isolated, noticeEffects) = server 2 notice confirming
      (ready, readyEffects) = server 3 (Protocol.ReceiptsRetired (session 1) mempty) isolated
      (duplicate, duplicateEffects) = server 4 (Protocol.ReceiptsRetired (session 1) mempty) ready
      (afterMutation, mutationEffects) = advance (Client.Submit invocation (WaitApplication [])) duplicate
      (lost, lossEffects) = advance (Client.TransportLost originalAttempt (instant 5)) afterMutation
      startups effects = [access | Client.SessionReady access <- effects]
  assertEqual "the isolation notice cannot bypass the startup receipt barrier" [] (startups (openingEffects <> noticeEffects))
  assertEqual "the intervening notice remains observable" [Client.HeraldIsolationObserved Protocol.ResidentProcessesZombie] noticeEffects
  assertEqual "receipt confirmation exposes startup exactly once" [Client.SessionReady startup] readyEffects
  assertEqual "startup confirmation preserves read-only isolation" Client.ApplicationClientIsolationReadOnly (Client.applicationClientPhase ready)
  assertEqual "startup uses the same established connection" originalAttempt (attemptOf ready)
  assertEqual "duplicate confirmation cannot repeat startup" [] duplicateEffects
  assertEqual "startup confirmation does not restore mutating operations" [Client.CallFinished invocation (Client.ApplicationCallRejected ApplicationOperateNotPermitted)] mutationEffects
  assertEqual "loss remains terminal with the isolation reason" Client.ApplicationClientPermanentlyUnavailable (Client.applicationClientPhase lost)
  assertEqual "terminal isolation never starts session recovery" [Client.HeraldPermanentlyUnavailableObserved Client.HeraldIsolated, Client.CloseTransport] lossEffects
  where
    cursor = checked . Protocol.applicationReplyCursorClaim

caseEndRecovery :: IO ()
caseEndRecovery = do
  let (submitted, effects) = advance (Client.SubmitLifecycle invocation Lifecycle.EndOwnProcess) (active 1)
      request = requestFor 1
      (pending, _) = server 2 (Protocol.LifecycleReply request (Lifecycle.LifecyclePending [])) submitted
      (recovering, _) = advance (Client.TransportLost (attemptOf pending) (instant 3)) pending
      (querying, queryEffects) = advance (Client.TransportAvailable (attemptOf recovering) (instant 4)) recovering
      (completed, resultEffects) = server 5 (Protocol.LifecycleReply request (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded)) querying
  assertEqual "exact session request" [Protocol.LifecycleCall (session 1) request Lifecycle.EndOwnProcess] [dto | Client.SendApplicationDto dto <- effects]
  assertEqual "recovery has only the restricted query" [Protocol.RecoverLifecycleResult attachment request] [dto | Client.SendApplicationDto dto <- queryEffects]
  assertEqual "ordered End is the correlated terminal" [Client.LifecycleChanged invocation (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded), Client.CloseTransport] resultEffects
  assertEqual "terminal recovery closes" Client.ApplicationClientClosed (Client.applicationClientPhase completed)

caseRestrictedRecovery :: IO ()
caseRestrictedRecovery = do
  let request = requestFor 19
      (querying, effects) = connected (Client.initialLifecycleRecoveryClient recovery attachment request)
      (_, absent) = server 1 (Protocol.LifecycleAbsent request) querying
  assertEqual "no session handshake" [Client.SendApplicationDto (Protocol.RecoverLifecycleResult attachment request)] effects
  assertEqual "absent is never successful End" [Client.LifecycleUnknown (Client.applicationInvocationId 0) Client.LifecycleResultNotRetained, Client.CloseTransport] absent

casePreparationRetry :: IO ()
casePreparationRetry = do
  let command = Lifecycle.BeginChild locator selection
      (submitted, _) = advance (Client.SubmitLifecycle invocation command) (active 1)
      attempt = attemptOf submitted
      (lost, effects) = advance (Client.TransportLost attempt (instant 2)) submitted
      (stillLost, lateEffects) = advance (Client.TransportAvailable attempt (instant 3)) lost
  assertEqual "pending lifecycle work becomes unknown" [Client.LifecycleUnknown invocation Client.SessionNoLongerLive, Client.HeraldPermanentlyUnavailableObserved Client.SessionNoLongerLive, Client.CloseTransport] effects
  assertEqual "late physical progress cannot revive the owner" lost stillLost
  assertEqual "no retry or resume" [] lateEffects

caseResultMismatch :: IO ()
caseResultMismatch = do
  let (submitted, _) = advance (Client.SubmitLifecycle invocation Lifecycle.EndOwnProcess) (active 1)
      request = requestFor 1
  case Client.stepApplicationClient (Client.ServerDtoReceived (attemptOf submitted) (instant 2) (Protocol.LifecycleReply request (Lifecycle.LifecycleCompleted Lifecycle.ChildReady))) submitted of
    Left (Client.ApplicationClientLifecycleContradiction actual) -> assertEqual "fault keeps exact correlation" request actual
    other -> assertFailure ("wrong lifecycle result admitted: " <> show other)

caseMultipleEndRecovery :: IO ()
caseMultipleEndRecovery = do
  let (first, _) = advance (Client.SubmitLifecycle invocation Lifecycle.EndOwnProcess) (active 1)
      secondInvocation = Client.applicationInvocationId 5
      (both, _) = advance (Client.SubmitLifecycle secondInvocation Lifecycle.EndOwnProcess) first
      firstRequest = requestFor 1
      secondRequest = checked (Lifecycle.lifecycleRequestId scope 1 5)
      (lost, lossEffects) = advance (Client.TransportLost (attemptOf both) (instant 2)) both
      (queryingFirst, _) = advance (Client.TransportAvailable (attemptOf lost) (instant 3)) lost
      (between, firstEffects) = server 4 (Protocol.LifecycleReply firstRequest (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded)) queryingFirst
      (queryingSecond, queryEffects) = advance (Client.TransportAvailable (attemptOf between) (instant 5)) between
      (_, secondEffects) = server 6 (Protocol.LifecycleReply secondRequest (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded)) queryingSecond
  assertEqual "neither accepted End becomes unknown" [] [inv | Client.LifecycleUnknown inv _ <- lossEffects <> firstEffects]
  assertEqual "second recovery preserves second exact correlation" [Protocol.RecoverLifecycleResult attachment secondRequest] [dto | Client.SendApplicationDto dto <- queryEffects]
  assertEqual "second terminal settles its original invocation" [Client.LifecycleChanged secondInvocation (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded), Client.CloseTransport] secondEffects
