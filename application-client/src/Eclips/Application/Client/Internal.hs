{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Private representation and transition machinery of the application client.
module Eclips.Application.Client.Internal
  ( ApplicationClientState,
    initialApplicationClient,
    initialPreparedApplicationClient,
    initialLifecycleRecoveryClient,
    ApplicationClientPhase (..),
    applicationClientPhase,
    applicationClientCurrentTransportAttempt,
    applicationClientRetentionCounts,
    applicationClientReceiptRetirement,
    ApplicationClientCommandError (..),
    ApplicationClientInvariantFault (..),
    ApplicationClientTransition (..),
    stepApplicationClient,
    validateApplicationClientState,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Client.Effect
  ( ApplicationCallOutcome (..),
    ApplicationClientEffect (..),
    ApplicationClientEffectBatch,
  )
import Eclips.Application.Client.Effect qualified as ProtocolEffects
import Eclips.Application.Client.Effect.Internal
  ( applicationClientEffectBatch,
  )
import Eclips.Application.Client.Input
  ( ApplicationClientInput (..),
    ApplicationInvocationId,
    ApplicationUnavailableReason (..),
  )
import Eclips.Application.Client.Input qualified as ClientInput
import Eclips.Application.Client.Recovery.Internal
  ( ApplicationClientMonotonicInstant (..),
    ApplicationClientRecoveryConfiguration,
    ApplicationClientRecoveryDeadline (..),
    ApplicationSessionRecoveryGeneration (..),
    ApplicationTransportAttemptGeneration (..),
    ApplicationTransportRetryScope (..),
    applicationClientMonotonicInstantMicroseconds,
    applicationClientRecoveryDeadlineMicroseconds,
    applicationClientRetryDelayMicroseconds,
    applicationSessionRecoveryGenerationWord64,
    applicationTransportAttemptGenerationWord64,
  )
import Eclips.Application.Types.Access (ApplicationStartupAccess)
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.Lifetime
  ( ApplicationReceiptRetirement,
    applicationReceiptRetirement,
    applicationReceiptRetirementWithExceptions,
    lifecycleReceiptRetirement,
    lifecycleReceiptRetirementThrough,
    ordinaryReceiptRetirement,
    ordinaryReceiptRetirementThrough,
  )
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Rejection
  ( ApplicationRejection (ApplicationOperateNotPermitted),
  )
import Eclips.Application.Types.Result
  ( OperationPendingReason (..),
    RegularCallResult (..),
    WaitResult (..),
  )
import Eclips.Application.Types.Value (ApplicationValue (SortDefinitionValue))
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    ApplicationClientDto (..),
    ApplicationClientNonce,
    ApplicationIsolationOverlayDto (..),
    ApplicationReplyCursorClaim,
    ApplicationRequestIdClaim,
    ApplicationRequestReplyBodyDto (..),
    ApplicationRequestSummaryDto,
    ApplicationRequestSummaryEntryDto,
    ApplicationRequestSummaryStatusDto (..),
    ApplicationResumeTokenClaim,
    ApplicationServerDto (..),
    ApplicationSessionClaim,
    ApplicationSessionErrorDto (..),
    ApplicationSessionUnavailableReasonDto (..),
    ApplicationWaitClaim,
    applicationReplyCursorClaimWord64,
    applicationRequestIdClaim,
    applicationRequestIdClaimWord64,
    applicationRequestSummaryEntries,
    applicationResumeTokenClaimOrdinal,
    applicationResumeTokenClaimScopeBytes,
    applicationSessionClaimOrdinal,
    applicationSessionClaimScopeBytes,
    applicationWaitClaimRequestId,
    applicationWaitClaimScopeBytes,
    applicationWaitClaimSessionOrdinal,
    summaryEntryRequestId,
    summaryEntryStatus,
  )
import Eclips.Protocol.Application.Types qualified as Protocol
import Eclips.Public.Types.ReceiptRetirement qualified as Retirement

-- | Caller-visible lifecycle phase, without physical-transport details.
data ApplicationClientPhase
  = ApplicationClientUnopened
  | ApplicationClientOpening
  | ApplicationClientActive
  | ApplicationClientIsolationReadOnly
  | ApplicationClientRecoveringLifecycle
  | ApplicationClientReconnecting
  | ApplicationClientEnding
  | ApplicationClientClosed
  | ApplicationClientPermanentlyUnavailable
  deriving stock (Eq, Show)

data ConnectionAttempt
  = AwaitingTransport ApplicationTransportAttemptGeneration
  | AwaitingTransportRetry
      ApplicationTransportAttemptGeneration
      ApplicationClientMonotonicInstant
  | AwaitingServerDisposition ApplicationTransportAttemptGeneration
  deriving stock (Eq, Show)

data SessionRecovery = SessionRecovery
  { generation :: ApplicationSessionRecoveryGeneration,
    deadline :: ApplicationClientRecoveryDeadline
  }
  deriving stock (Eq, Show)

data ClientPhase
  = UnopenedPhase
  | OpeningPhase ConnectionAttempt
  | EndRecoveryPhase ConnectionAttempt
  | ActivePhase ApplicationTransportAttemptGeneration
  | IsolationReadOnlyPhase
      ApplicationTransportAttemptGeneration
      ApplicationUnavailableReason
  | ReconnectingPhase SessionRecovery ConnectionAttempt
  | EndingPhase SessionRecovery ConnectionAttempt
  | ClosedPhase
  | PermanentlyUnavailablePhase
  deriving stock (Eq, Show)

data SessionFacts = SessionFacts
  { session :: ApplicationSessionClaim,
    token :: ApplicationResumeTokenClaim
  }
  deriving stock (Eq, Show)

data RequestTerminal
  = RequestSucceeded RegularCallResult
  | RequestRejected ApplicationRejection
  | RequestCancelled
  | RequestUnknown ApplicationUnavailableReason
  | RequestClosed
  deriving stock (Eq, Show)

data ClientRequest = ClientRequest
  { invocation :: ApplicationInvocationId,
    call :: ApplicationClientDto,
    wait :: Maybe ApplicationWaitClaim,
    cancellationRequested :: Bool,
    terminal :: Maybe RequestTerminal
  }
  deriving stock (Eq, Show)

data CursorObservation
  = ObservedSessionOpen
  | ObservedSessionResume ApplicationRequestSummaryDto
  | ObservedIsolationBegin
      ApplicationIsolationOverlayDto
      ApplicationUnavailableReason
  | ObservedRequest
      ApplicationRequestIdClaim
      ApplicationRequestSummaryStatusDto
  deriving stock (Eq, Show)

data ClientLifecycleRequest = ClientLifecycleRequest
  { invocation :: ApplicationInvocationId,
    command :: Lifecycle.LifecycleCommand,
    status :: Maybe Lifecycle.LifecycleStatus,
    closed :: Bool
  }
  deriving stock (Eq, Show)

-- | Opaque state owned by one pure client machine.
data ApplicationClientState = ApplicationClientState
  { recoveryConfiguration :: ApplicationClientRecoveryConfiguration,
    attachment :: ApplicationAttachmentClaim,
    nonce :: ApplicationClientNonce,
    preparedClaim :: Maybe (Lifecycle.ConnectionDescriptor, Lifecycle.InitialClaimId),
    pendingStartup :: Maybe ApplicationStartupAccess,
    endRecovery :: Maybe Lifecycle.LifecycleRequestId,
    lifecycleRequests :: Map Lifecycle.LifecycleRequestId ClientLifecycleRequest,
    phase :: ClientPhase,
    nextTransportAttemptGeneration :: Word64,
    nextSessionRecoveryGeneration :: Word64,
    sessionFacts :: Maybe SessionFacts,
    lastObservedReply :: Maybe ApplicationReplyCursorClaim,
    nextRequest :: Word64,
    latestInvocation :: Maybe ApplicationInvocationId,
    latestLifecycle :: Maybe Word64,
    latestLifecycleCompleted :: Maybe Word64,
    retirementReady :: ApplicationReceiptRetirement,
    retirementConfirmed :: ApplicationReceiptRetirement,
    retirementOffered :: ApplicationReceiptRetirement,
    retirementFlushScheduled :: Bool,
    invocationRequests :: Map ApplicationInvocationId Word64,
    requests :: Map Word64 ClientRequest,
    cursorHistory :: Map Word64 CursorObservation
  }
  deriving stock (Eq, Show)

-- | Construct an unopened owner from injected stable open correlations.
initialApplicationClient ::
  ApplicationClientRecoveryConfiguration ->
  ApplicationAttachmentClaim ->
  ApplicationClientNonce ->
  ApplicationClientState
initialApplicationClient recoveryConfiguration attachment nonce =
  ApplicationClientState
    { recoveryConfiguration,
      attachment,
      nonce,
      preparedClaim = Nothing,
      pendingStartup = Nothing,
      endRecovery = Nothing,
      lifecycleRequests = Map.empty,
      phase = UnopenedPhase,
      nextTransportAttemptGeneration = 1,
      nextSessionRecoveryGeneration = 1,
      sessionFacts = Nothing,
      lastObservedReply = Nothing,
      nextRequest = 0,
      latestInvocation = Nothing,
      latestLifecycle = Nothing,
      latestLifecycleCompleted = Nothing,
      retirementReady = mempty,
      retirementConfirmed = mempty,
      retirementOffered = mempty,
      retirementFlushScheduled = False,
      invocationRequests = Map.empty,
      requests = Map.empty,
      cursorHistory = Map.empty
    }

-- | Stable initial-claim identity is injected once and reused across transports.
initialPreparedApplicationClient :: ApplicationClientRecoveryConfiguration -> Lifecycle.ConnectionDescriptor -> Lifecycle.InitialClaimId -> ApplicationClientState
initialPreparedApplicationClient recovery descriptor claim =
  (initialApplicationClient recovery attachment (Protocol.applicationClientNonce 0))
    { preparedClaim = Just (descriptor, claim)
    }
  where
    attachment = either (error . show) id (Protocol.applicationAttachmentClaim (Lifecycle.connectionDescriptorAttachmentBytes descriptor))

-- | Restricted finite-run own-End recovery never creates or resumes a session.
initialLifecycleRecoveryClient :: ApplicationClientRecoveryConfiguration -> ApplicationAttachmentClaim -> Lifecycle.LifecycleRequestId -> ApplicationClientState
initialLifecycleRecoveryClient recovery attachment request =
  (initialApplicationClient recovery attachment (Protocol.applicationClientNonce 0))
    { endRecovery = Just request,
      lifecycleRequests = Map.singleton request (ClientLifecycleRequest (ClientInput.applicationInvocationId 0) Lifecycle.EndOwnProcess Nothing False)
    }

-- | A synchronous local command rejection. No successor state accompanies it.
data ApplicationClientCommandError
  = ApplicationClientWrongPhase
      ApplicationInvocationId
      ApplicationClientPhase
  | ApplicationInvocationAlreadyUsed ApplicationInvocationId
  | ApplicationInvocationUnknown ApplicationInvocationId
  | ApplicationInvocationRetired ApplicationInvocationId
  deriving stock (Eq, Show)

-- | A contradiction in server correlation, ordering, or private owner state.
data ApplicationClientInvariantFault
  = ApplicationClientInvalidState
  | ApplicationClientUnexpectedInput ApplicationClientPhase
  | ApplicationClientUnexpectedServerDto ApplicationClientPhase
  | ApplicationClientUnexpectedSessionRejection ApplicationSessionErrorDto
  | ApplicationClientForeignSession
      ApplicationSessionClaim
      ApplicationSessionClaim
  | ApplicationClientSessionTokenMismatch
      ApplicationSessionClaim
      ApplicationResumeTokenClaim
  | ApplicationClientInvalidReceiptRetirement ApplicationReceiptRetirement
  | ApplicationClientUnknownRequest ApplicationRequestIdClaim
  | ApplicationClientWaitMismatch ApplicationRequestIdClaim
  | ApplicationClientCursorConflict ApplicationReplyCursorClaim
  | ApplicationClientCursorGap
      ApplicationReplyCursorClaim
      ApplicationReplyCursorClaim
  | ApplicationClientResumeCursorRegressed
      ApplicationReplyCursorClaim
      ApplicationReplyCursorClaim
  | ApplicationClientSummaryContradiction ApplicationRequestIdClaim
  | ApplicationClientRequestConflict ApplicationRequestIdClaim
  | ApplicationClientResultMismatch ApplicationRequestIdClaim
  | ApplicationClientLifecycleContradiction Lifecycle.LifecycleRequestId
  | ApplicationClientRetryTimerObservedEarly
      ApplicationClientMonotonicInstant
      ApplicationClientMonotonicInstant
  | ApplicationClientRecoveryTimerObservedEarly
      ApplicationClientRecoveryDeadline
      ApplicationClientMonotonicInstant
  deriving stock (Eq, Show)

-- | One total successful transition or one state-preserving command rejection.
data ApplicationClientTransition
  = ApplicationClientAdvanced
      ApplicationClientState
      ApplicationClientEffectBatch
  | ApplicationClientCommandRejected ApplicationClientCommandError
  deriving stock (Eq, Show)

-- | Observe the logical lifecycle without exposing connection-attempt or owner
-- bookkeeping.
applicationClientPhase :: ApplicationClientState -> ApplicationClientPhase
applicationClientPhase state = phaseView state.phase

phaseView :: ClientPhase -> ApplicationClientPhase
phaseView = \case
  UnopenedPhase -> ApplicationClientUnopened
  OpeningPhase _ -> ApplicationClientOpening
  EndRecoveryPhase _ -> ApplicationClientRecoveringLifecycle
  ActivePhase _ -> ApplicationClientActive
  IsolationReadOnlyPhase _ _ -> ApplicationClientIsolationReadOnly
  ReconnectingPhase _ _ -> ApplicationClientReconnecting
  EndingPhase _ _ -> ApplicationClientEnding
  ClosedPhase -> ApplicationClientClosed
  PermanentlyUnavailablePhase -> ApplicationClientPermanentlyUnavailable

-- | Observe the attempt currently being dialled, handshaken, used, or paced.
-- Terminal and unopened owners have no current physical attempt.
applicationClientCurrentTransportAttempt ::
  ApplicationClientState ->
  Maybe ApplicationTransportAttemptGeneration
applicationClientCurrentTransportAttempt state = phaseAttempt state.phase

phaseAttempt :: ClientPhase -> Maybe ApplicationTransportAttemptGeneration
phaseAttempt = \case
  OpeningPhase attempt -> Just (connectionAttemptGeneration attempt)
  EndRecoveryPhase attempt -> Just (connectionAttemptGeneration attempt)
  ActivePhase attempt -> Just attempt
  IsolationReadOnlyPhase attempt _ -> Just attempt
  ReconnectingPhase _ attempt -> Just (connectionAttemptGeneration attempt)
  EndingPhase _ attempt -> Just (connectionAttemptGeneration attempt)
  _ -> Nothing

connectionAttemptGeneration :: ConnectionAttempt -> ApplicationTransportAttemptGeneration
connectionAttemptGeneration = \case
  AwaitingTransport attempt -> attempt
  AwaitingTransportRetry attempt _ -> attempt
  AwaitingServerDisposition attempt -> attempt

-- | Advance the client by one command or observation.
stepApplicationClient ::
  ApplicationClientInput ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
stepApplicationClient input predecessor = do
  validateApplicationClientState predecessor
  transition <- stepChecked input predecessor
  case transition of
    ApplicationClientCommandRejected _ -> pure transition
    ApplicationClientAdvanced successor _ -> do
      validateApplicationClientState successor
      pure transition

stepChecked ::
  ApplicationClientInput ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
stepChecked input state = case input of
  Start -> startClient state
  TransportAvailable attempt observedAt -> transportAvailable attempt observedAt state
  TransportAttemptFailed attempt observedAt -> transportAttemptFailed attempt observedAt state
  TransportLost attempt observedAt -> transportLost attempt observedAt state
  TransportRetryElapsed scope attempt observedAt ->
    transportRetryElapsed scope attempt observedAt state
  RecoveryDeadlineElapsed generation deadline observedAt ->
    recoveryDeadlineElapsed generation deadline observedAt state
  ServerDtoReceived attempt observedAt dto -> receiveServerDto attempt observedAt dto state
  Submit invocation operation -> submitCall invocation operation state
  SubmitLifecycle invocation command -> submitLifecycle invocation command state
  Cancel invocation -> cancelInvocation invocation state
  ReceiptRetirementFlush attempt -> flushReceiptRetirement attempt state
  End -> endClient state
  ClientInput.HeraldPermanentlyUnavailable reason -> permanentlyUnavailable reason state

startClient ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
startClient state = case state.phase of
  UnopenedPhase -> beginOpeningAttempt state
  OpeningPhase _ -> advanced state []
  EndRecoveryPhase _ -> advanced state []
  _ -> unexpectedInput state

transportAvailable ::
  ApplicationTransportAttemptGeneration ->
  ApplicationClientMonotonicInstant ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
transportAvailable attempt observedAt state = case state.phase of
  OpeningPhase (AwaitingTransport current)
    | attempt == current ->
        advanced
          state {phase = OpeningPhase (AwaitingServerDisposition attempt)}
          [SendApplicationDto (maybe (OpenSession state.attachment state.nonce) (uncurry ClaimInitial) state.preparedClaim)]
  EndRecoveryPhase (AwaitingTransport current)
    | attempt == current,
      Just request <- state.endRecovery ->
        advanced
          state {phase = EndRecoveryPhase (AwaitingServerDisposition attempt)}
          [SendApplicationDto (RecoverLifecycleResult state.attachment request)]
  ReconnectingPhase recovery (AwaitingTransport current)
    | attempt == current ->
        withinRecovery
          observedAt
          recovery
          state
          (sendResume ReconnectingPhase recovery attempt state)
  EndingPhase recovery (AwaitingTransport current)
    | attempt == current ->
        withinRecovery
          observedAt
          recovery
          state
          (sendResume EndingPhase recovery attempt state)
  _ -> advanced state []

sendResume ::
  (SessionRecovery -> ConnectionAttempt -> ClientPhase) ->
  SessionRecovery ->
  ApplicationTransportAttemptGeneration ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
sendResume phaseConstructor recovery attempt state = do
  facts <- requireSession state
  cursor <- requireCursor state
  advanced
    state {phase = phaseConstructor recovery (AwaitingServerDisposition attempt)}
    [SendApplicationDto (ResumeSession facts.session facts.token cursor)]

transportAttemptFailed ::
  ApplicationTransportAttemptGeneration ->
  ApplicationClientMonotonicInstant ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
transportAttemptFailed attempt observedAt state = case state.phase of
  EndRecoveryPhase (AwaitingTransport current)
    | attempt == current -> paceOpeningRetry attempt observedAt state
  OpeningPhase (AwaitingTransport current)
    | attempt == current -> paceOpeningRetry attempt observedAt state
  ReconnectingPhase recovery (AwaitingTransport current)
    | attempt == current ->
        withinRecovery
          observedAt
          recovery
          state
          (paceRecoveryRetry ReconnectingPhase recovery attempt observedAt state)
  EndingPhase recovery (AwaitingTransport current)
    | attempt == current ->
        withinRecovery
          observedAt
          recovery
          state
          (paceRecoveryRetry EndingPhase recovery attempt observedAt state)
  _ -> advanced state []

transportLost ::
  ApplicationTransportAttemptGeneration ->
  ApplicationClientMonotonicInstant ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
transportLost attempt observedAt state = case state.phase of
  OpeningPhase (AwaitingServerDisposition current)
    | attempt == current -> paceOpeningRetry attempt observedAt state
  EndRecoveryPhase (AwaitingServerDisposition current)
    | attempt == current -> paceOpeningRetry attempt observedAt state
  ActivePhase current
    | attempt == current, Just request <- pendingOwnEnd state -> beginEndRecovery request state
  ActivePhase current
    | attempt == current -> settleUnavailable SessionNoLongerLive state
  IsolationReadOnlyPhase current reason
    | attempt == current -> settleUnavailable reason state
  ReconnectingPhase recovery (AwaitingServerDisposition current)
    | attempt == current ->
        withinRecovery
          observedAt
          recovery
          state
          (paceRecoveryRetry ReconnectingPhase recovery attempt observedAt state)
  EndingPhase recovery (AwaitingServerDisposition current)
    | attempt == current ->
        withinRecovery
          observedAt
          recovery
          state
          (paceRecoveryRetry EndingPhase recovery attempt observedAt state)
  _ -> advanced state []

transportRetryElapsed ::
  ApplicationTransportRetryScope ->
  ApplicationTransportAttemptGeneration ->
  ApplicationClientMonotonicInstant ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
transportRetryElapsed scope attempt observedAt state = case state.phase of
  OpeningPhase (AwaitingTransportRetry current retryAt)
    | scope == ApplicationOpeningRetry && attempt == current -> do
        ensureRetryDue retryAt observedAt
        beginOpeningAttempt state
  EndRecoveryPhase (AwaitingTransportRetry current retryAt)
    | scope == ApplicationOpeningRetry && attempt == current -> do
        ensureRetryDue retryAt observedAt
        beginOpeningAttempt state
  ReconnectingPhase recovery (AwaitingTransportRetry current retryAt)
    | scope == ApplicationSessionRecoveryRetry recovery.generation
        && attempt == current ->
        withinRecovery observedAt recovery state $ do
          ensureRetryDue retryAt observedAt
          beginRecoveryAttempt ReconnectingPhase recovery state
  EndingPhase recovery (AwaitingTransportRetry current retryAt)
    | scope == ApplicationSessionRecoveryRetry recovery.generation
        && attempt == current ->
        withinRecovery observedAt recovery state $ do
          ensureRetryDue retryAt observedAt
          beginRecoveryAttempt EndingPhase recovery state
  _ -> advanced state []

recoveryDeadlineElapsed ::
  ApplicationSessionRecoveryGeneration ->
  ApplicationClientRecoveryDeadline ->
  ApplicationClientMonotonicInstant ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
recoveryDeadlineElapsed generation deadline observedAt state =
  case phaseRecovery state.phase of
    Just recovery
      | generation == recovery.generation && deadline == recovery.deadline -> do
          ensureRecoveryDeadlineDue deadline observedAt
          settleUnavailable HeraldPermanentlyLost state
    _ -> advanced state []

beginOpeningAttempt ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
beginOpeningAttempt state =
  let (allocated, attempt) = allocateTransportAttempt state
   in advanced
        allocated {phase = openingPhase allocated (AwaitingTransport attempt)}
        [RequestTransport attempt]

openingPhase :: ApplicationClientState -> ConnectionAttempt -> ClientPhase
openingPhase state = if isJust state.endRecovery then EndRecoveryPhase else OpeningPhase

beginRecoveryAttempt ::
  (SessionRecovery -> ConnectionAttempt -> ClientPhase) ->
  SessionRecovery ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
beginRecoveryAttempt phaseConstructor recovery state =
  let (allocated, attempt) = allocateTransportAttempt state
   in advanced
        allocated {phase = phaseConstructor recovery (AwaitingTransport attempt)}
        [RequestTransport attempt]

allocateTransportAttempt ::
  ApplicationClientState ->
  (ApplicationClientState, ApplicationTransportAttemptGeneration)
allocateTransportAttempt state =
  ( state
      { nextTransportAttemptGeneration =
          state.nextTransportAttemptGeneration + 1
      },
    ApplicationTransportAttemptGeneration state.nextTransportAttemptGeneration
  )

paceOpeningRetry ::
  ApplicationTransportAttemptGeneration ->
  ApplicationClientMonotonicInstant ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
paceOpeningRetry attempt observedAt state =
  let retryAt = retryInstant observedAt state.recoveryConfiguration
   in advanced
        state {phase = openingPhase state (AwaitingTransportRetry attempt retryAt)}
        [ArmTransportRetry ApplicationOpeningRetry attempt retryAt]

paceRecoveryRetry ::
  (SessionRecovery -> ConnectionAttempt -> ClientPhase) ->
  SessionRecovery ->
  ApplicationTransportAttemptGeneration ->
  ApplicationClientMonotonicInstant ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
paceRecoveryRetry phaseConstructor recovery attempt observedAt state =
  let retryAt = retryInstant observedAt state.recoveryConfiguration
      scope = ApplicationSessionRecoveryRetry recovery.generation
   in advanced
        state
          { phase =
              phaseConstructor recovery (AwaitingTransportRetry attempt retryAt)
          }
        [ArmTransportRetry scope attempt retryAt]

retryInstant ::
  ApplicationClientMonotonicInstant ->
  ApplicationClientRecoveryConfiguration ->
  ApplicationClientMonotonicInstant
retryInstant observedAt configuration =
  ApplicationClientMonotonicInstant
    ( applicationClientMonotonicInstantMicroseconds observedAt
        + applicationClientRetryDelayMicroseconds configuration
    )

withinRecovery ::
  ApplicationClientMonotonicInstant ->
  SessionRecovery ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
withinRecovery observedAt recovery state beforeDeadline
  | recoveryExpired observedAt recovery = settleUnavailable HeraldPermanentlyLost state
  | otherwise = beforeDeadline

recoveryExpired :: ApplicationClientMonotonicInstant -> SessionRecovery -> Bool
recoveryExpired observedAt recovery =
  applicationClientMonotonicInstantMicroseconds observedAt
    >= applicationClientRecoveryDeadlineMicroseconds recovery.deadline

ensureRetryDue ::
  ApplicationClientMonotonicInstant ->
  ApplicationClientMonotonicInstant ->
  Either ApplicationClientInvariantFault ()
ensureRetryDue retryAt observedAt
  | observedAt < retryAt =
      Left (ApplicationClientRetryTimerObservedEarly retryAt observedAt)
  | otherwise = pure ()

ensureRecoveryDeadlineDue ::
  ApplicationClientRecoveryDeadline ->
  ApplicationClientMonotonicInstant ->
  Either ApplicationClientInvariantFault ()
ensureRecoveryDeadlineDue deadline observedAt
  | applicationClientMonotonicInstantMicroseconds observedAt
      < applicationClientRecoveryDeadlineMicroseconds deadline =
      Left (ApplicationClientRecoveryTimerObservedEarly deadline observedAt)
  | otherwise = pure ()

phaseRecovery :: ClientPhase -> Maybe SessionRecovery
phaseRecovery = \case
  ReconnectingPhase recovery _ -> Just recovery
  EndingPhase recovery _ -> Just recovery
  _ -> Nothing

cancelRecoveryEffect :: SessionRecovery -> ApplicationClientEffect
cancelRecoveryEffect recovery =
  CancelRecoveryDeadline recovery.generation recovery.deadline

receiveServerDto ::
  ApplicationTransportAttemptGeneration ->
  ApplicationClientMonotonicInstant ->
  ApplicationServerDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveServerDto attempt observedAt dto state = case state.phase of
  EndRecoveryPhase (AwaitingServerDisposition current)
    | attempt == current -> receiveLifecycle dto state
  OpeningPhase (AwaitingServerDisposition current)
    | attempt == current -> receiveOpening attempt observedAt dto state
  ReconnectingPhase recovery (AwaitingServerDisposition current)
    | attempt == current ->
        withinRecovery
          observedAt
          recovery
          state
          (receiveResume False recovery attempt dto state)
  EndingPhase recovery (AwaitingServerDisposition current)
    | attempt == current ->
        withinRecovery
          observedAt
          recovery
          state
          (receiveResume True recovery attempt dto state)
  ActivePhase current
    | attempt == current -> receiveActive dto state
  IsolationReadOnlyPhase current _
    | attempt == current -> receiveIsolationReadOnly dto state
  OpeningPhase (AwaitingTransport current)
    | attempt == current -> unexpectedServer state
  EndRecoveryPhase (AwaitingTransport current)
    | attempt == current -> unexpectedServer state
  ReconnectingPhase _ (AwaitingTransport current)
    | attempt == current -> unexpectedServer state
  EndingPhase _ (AwaitingTransport current)
    | attempt == current -> unexpectedServer state
  UnopenedPhase -> unexpectedServer state
  _ -> advanced state []

receiveOpening ::
  ApplicationTransportAttemptGeneration ->
  ApplicationClientMonotonicInstant ->
  ApplicationServerDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveOpening attempt _observedAt dto state = case dto of
  SessionOpened cursor session token startup -> do
    ensureSessionTokenCoherent session token
    let connected =
          state
            { phase = ActivePhase attempt,
              sessionFacts = Just SessionFacts {session, token},
              lastObservedReply = Just cursor,
              cursorHistory =
                Map.singleton
                  (applicationReplyCursorClaimWord64 cursor)
                  ObservedSessionOpen
            }
    if isJust state.preparedClaim
      then
        -- Confirm the initial claim on this same physical connection. Once a
        -- session exists, physical loss ends its lifetime; startup never closes
        -- a successful connection merely to prove receipt of the result.
        advanced
          connected {pendingStartup = Just startup}
          [SessionEstablishedOnTransport attempt, SendApplicationDto (RetireReceipts session mempty)]
      else advanced connected [SessionEstablishedOnTransport attempt, SessionReady startup]
  InitialClaimPending claim _
    | Just (_, expected) <- state.preparedClaim, claim == expected -> advanced state [SessionEstablishedOnTransport attempt]
  InitialClaimRejected claim problem
    | Just (_, expected) <- state.preparedClaim,
      claim == expected ->
        advanced state {phase = ClosedPhase} [InitialClaimFailed problem, CloseTransport]
  SessionRejected ApplicationAttachmentNotAdmittedDto ->
    advanced
      state {phase = ClosedPhase}
      [SessionOpenFailed ApplicationAttachmentNotAdmittedDto, CloseTransport]
  SessionRejected rejection ->
    Left (ApplicationClientUnexpectedSessionRejection rejection)
  _ -> unexpectedServer state

receiveResume ::
  Bool ->
  SessionRecovery ->
  ApplicationTransportAttemptGeneration ->
  ApplicationServerDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveResume ending recovery attempt dto state = case dto of
  SessionResumed cursor session summary -> do
    ensureSession session state
    priorCursor <- requireCursor state
    if cursor < priorCursor
      then Left (ApplicationClientResumeCursorRegressed priorCursor cursor)
      else do
        let cursorWord = applicationReplyCursorClaimWord64 cursor
            observation = ObservedSessionResume summary
        case Map.lookup cursorWord state.cursorHistory of
          Just oldObservation
            | oldObservation /= observation ->
                Left (ApplicationClientCursorConflict cursor)
          _ -> pure ()
        let withCursor =
              state
                { lastObservedReply = Just cursor,
                  cursorHistory =
                    Map.insert cursorWord observation state.cursorHistory
                }
        (reconciled, reconciliationEffects) <-
          reconcileSummary (not ending) summary withCursor
        if ending
          then
            closeAfterResume
              (cancelRecoveryEffect recovery : reconciliationEffects)
              reconciled
          else
            advanced
              reconciled {phase = ActivePhase attempt, pendingStartup = Nothing}
              ( cancelRecoveryEffect recovery
                  : SessionEstablishedOnTransport attempt
                  : reconciliationEffects
                    <> lifecycleQueries reconciled
                    <> maybe [] (pure . SessionReady) reconciled.pendingStartup
              )
  SessionRejected ApplicationSessionNotLiveDto
    | Just request <- pendingOwnEnd state -> beginEndRecovery request state
    | otherwise -> settleUnavailable SessionNoLongerLive state
  SessionRejected rejection ->
    Left (ApplicationClientUnexpectedSessionRejection rejection)
  Protocol.HeraldPermanentlyUnavailable session reason ->
    receiveSessionUnavailable session reason state
  _ -> unexpectedServer state

closeAfterResume ::
  [ApplicationClientEffect] ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
closeAfterResume prefix state = do
  facts <- requireSession state
  let (closed, closingEffects) = closeUnresolved CallClosed RequestClosed state
  advanced
    closed {phase = ClosedPhase}
    ( prefix
        <> [SendApplicationDto (EndSession facts.session)]
        <> closingEffects
        <> [CloseTransport]
    )

receiveActive ::
  ApplicationServerDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveActive dto state = case dto of
  ReceiptsRetired session progress -> do
    ensureSession session state
    confirmReceiptRetirement progress state
  RequestRetired session request through -> do
    ensureSession session state
    if applicationRequestIdClaimWord64 request <= through
      && maybe False (through <=) (ordinaryReceiptRetirementThrough state.retirementReady)
      && Retirement.receiptIsRetired (applicationRequestIdClaimWord64 request) (ordinaryReceiptRetirement state.retirementReady)
      && Map.notMember (applicationRequestIdClaimWord64 request) state.requests
      then advanced state []
      else Left (ApplicationClientInvalidReceiptRetirement (applicationReceiptRetirement (Just through) Nothing))
  LifecycleRetired request through -> receiveLifecycle (LifecycleRetired request through) state
  LifecycleReply {} -> receiveLifecycle dto state
  LifecycleAbsent {} -> receiveLifecycle dto state
  LifecycleConflict {} -> receiveLifecycle dto state
  RequestRetained session cursor request body
    | locallyRetiredRequest request state -> retiredObservation session cursor state
    | otherwise -> do
        ensureSession session state
        ensureKnownRequest request state
        case body of
          Cancelled wait -> ensureLearnedWait request wait state
          _ -> pure ()
        let (status, directWait) = retainedStatus body
        receiveRequestObservation False cursor request status directWait state
  RequestAbsent session request -> do
    ensureSession session state
    clientRequest <- lookupRequest request state
    if isJust clientRequest.terminal || isJust clientRequest.wait
      then Left (ApplicationClientSummaryContradiction request)
      else advanced state [SendApplicationDto clientRequest.call]
  RequestConflict session request -> do
    ensureSession session state
    ensureKnownRequest request state
    Left (ApplicationClientRequestConflict request)
  WaitWake session cursor request _wait WaitReady
    | locallyRetiredRequest request state -> retiredObservation session cursor state
  WaitWake session cursor request wait WaitReady -> do
    ensureSession session state
    ensureKnownRequest request state
    ensureLearnedWait request wait state
    receiveRequestObservation
      True
      cursor
      request
      (CompletedDto (WaitCompleted WaitReady))
      (Just wait)
      state
  HeraldIsolationBegun cursor session overlay reason ->
    receiveIsolationBegin cursor session overlay reason state
  Protocol.HeraldPermanentlyUnavailable session reason ->
    receiveSessionUnavailable session reason state
  _ -> unexpectedServer state

receiveIsolationReadOnly ::
  ApplicationServerDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveIsolationReadOnly dto state = case dto of
  ReceiptsRetired {} -> receiveActive dto state
  RequestRetired {} -> receiveActive dto state
  LifecycleRetired {} -> receiveActive dto state
  RequestRetained {} -> receiveActive dto state
  RequestAbsent {} -> receiveActive dto state
  RequestConflict {} -> receiveActive dto state
  Protocol.HeraldPermanentlyUnavailable session reason ->
    receiveSessionUnavailable session reason state
  _ -> unexpectedServer state

receiveIsolationBegin ::
  ApplicationReplyCursorClaim ->
  ApplicationSessionClaim ->
  ApplicationIsolationOverlayDto ->
  ApplicationSessionUnavailableReasonDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveIsolationBegin cursor session overlay unavailableDto state = do
  ensureSession session state
  prior <- requireCursor state
  attempt <- requireEstablishedAttempt state
  let cursorWord = applicationReplyCursorClaimWord64 cursor
      priorWord = applicationReplyCursorClaimWord64 prior
      unavailable = unavailableReasonFromDto unavailableDto
      observation = ObservedIsolationBegin overlay unavailable
  case Map.lookup cursorWord state.cursorHistory of
    Just retained
      | retained == observation -> advanced state []
      | otherwise -> Left (ApplicationClientCursorConflict cursor)
    Nothing
      | cursorWord /= priorWord + 1 ->
          Left (ApplicationClientCursorGap prior cursor)
      | otherwise ->
          advanced
            state
              { phase = IsolationReadOnlyPhase attempt unavailable,
                lastObservedReply = Just cursor,
                cursorHistory = Map.insert cursorWord observation state.cursorHistory
              }
            [HeraldIsolationObserved overlay]

requireEstablishedAttempt ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationTransportAttemptGeneration
requireEstablishedAttempt state = case state.phase of
  ActivePhase attempt -> Right attempt
  IsolationReadOnlyPhase attempt _ -> Right attempt
  _ -> Left ApplicationClientInvalidState

receiveSessionUnavailable ::
  ApplicationSessionClaim ->
  ApplicationSessionUnavailableReasonDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveSessionUnavailable session reason state = do
  ensureSession session state
  permanentlyUnavailable (unavailableReasonFromDto reason) state

unavailableReasonFromDto ::
  ApplicationSessionUnavailableReasonDto ->
  ApplicationUnavailableReason
unavailableReasonFromDto = \case
  ApplicationSessionNoLongerLiveDto -> SessionNoLongerLive
  HeraldIsolatedDto -> HeraldIsolated
  HeraldRetiredDto -> HeraldRetired

retainedStatus ::
  ApplicationRequestReplyBodyDto ->
  (ApplicationRequestSummaryStatusDto, Maybe ApplicationWaitClaim)
retainedStatus = \case
  WaitAccepted wait -> (PendingDto wait, Just wait)
  OperationAccepted reason -> (OperationAcceptedDto reason, Nothing)
  Completed result -> (CompletedDto result, Nothing)
  Rejected rejection -> (RejectedDto rejection, Nothing)
  Cancelled wait -> (CancelledDto, Just wait)

receiveRequestObservation ::
  Bool ->
  ApplicationReplyCursorClaim ->
  ApplicationRequestIdClaim ->
  ApplicationRequestSummaryStatusDto ->
  Maybe ApplicationWaitClaim ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveRequestObservation isWake cursor request status directWait state = do
  maybe (pure ()) (\wait -> ensureWait request wait state) directWait
  let cursorWord = applicationReplyCursorClaimWord64 cursor
      observation = ObservedRequest request status
  case Map.lookup cursorWord state.cursorHistory of
    Just oldObservation
      | oldObservation == observation -> advanced state []
      | otherwise -> Left (ApplicationClientCursorConflict cursor)
    Nothing -> do
      ensureRequestObservationIsFresh cursor observation state
      prior <- requireCursor state
      let priorWord = applicationReplyCursorClaimWord64 prior
      if isWake && cursorWord /= priorWord + 1
        then Left (ApplicationClientCursorGap prior cursor)
        else
          if cursorWord > priorWord + 1
            then Left (ApplicationClientCursorGap prior cursor)
            else
              if cursorWord <= priorWord
                then receiveHistoricalObservation cursorWord observation request status state
                else do
                  ensureForwardSummaryCompatibility cursorWord request status state
                  let withHistory =
                        state
                          { lastObservedReply = Just cursor,
                            cursorHistory =
                              Map.insert cursorWord observation state.cursorHistory
                          }
                  (successor, effects) <- applyRequestStatus request status directWait withHistory
                  followUp <- directPendingCancellation request status successor
                  advanced successor (effects <> followUp)

ensureRequestObservationIsFresh ::
  ApplicationReplyCursorClaim ->
  CursorObservation ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ()
ensureRequestObservationIsFresh cursor observation state
  | observation `elem` Map.elems state.cursorHistory =
      Left (ApplicationClientCursorConflict cursor)
  | otherwise = pure ()

receiveHistoricalObservation ::
  Word64 ->
  CursorObservation ->
  ApplicationRequestIdClaim ->
  ApplicationRequestSummaryStatusDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
receiveHistoricalObservation cursorWord observation request status state = do
  ensureHistoricalSummaryCompatibility cursorWord request status state
  retained <- lookupRequest request state
  let withHistory =
        state
          { cursorHistory =
              Map.insert cursorWord observation state.cursorHistory
          }
  case status of
    PendingDto wait -> do
      case retained.terminal of
        Nothing -> retainHistoricalWait retained withHistory wait
        Just RequestCancelled -> retainHistoricalWait retained withHistory wait
        Just (RequestSucceeded (WaitCompleted WaitReady)) ->
          retainHistoricalWait retained withHistory wait
        _ -> Left (ApplicationClientSummaryContradiction request)
    OperationAcceptedDto reason ->
      case retained.terminal of
        Nothing
          | operationMayStabilize retained.call reason -> advanced withHistory []
        Just (RequestSucceeded _)
          | operationMayStabilize retained.call reason -> advanced withHistory []
        _ -> Left (ApplicationClientSummaryContradiction request)
    _
      | historicalTerminalMatches status retained.terminal -> advanced withHistory []
      | otherwise -> Left (ApplicationClientSummaryContradiction request)
  where
    retainHistoricalWait retained withHistory wait = do
      updated <- mergeWait request wait retained
      let successor =
            withHistory
              { requests =
                  Map.insert
                    (applicationRequestIdClaimWord64 request)
                    updated
                    withHistory.requests
              }
      followUp <- directPendingCancellation request status successor
      advanced successor followUp

ensureHistoricalSummaryCompatibility ::
  Word64 ->
  ApplicationRequestIdClaim ->
  ApplicationRequestSummaryStatusDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ()
ensureHistoricalSummaryCompatibility cursorWord request status state
  | all compatible laterResumeSummaries = pure ()
  | otherwise = Left (ApplicationClientSummaryContradiction request)
  where
    laterResumeSummaries =
      [ summary
      | (laterCursor, ObservedSessionResume summary) <- Map.toAscList state.cursorHistory,
        laterCursor > cursorWord
      ]
    compatible summary =
      maybe False (historicalStatusPrecedes status) (summaryStatusFor request summary)

ensureForwardSummaryCompatibility ::
  Word64 ->
  ApplicationRequestIdClaim ->
  ApplicationRequestSummaryStatusDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ()
ensureForwardSummaryCompatibility cursorWord request status state =
  case latestPriorResumeSummary cursorWord state.cursorHistory of
    Nothing -> pure ()
    Just summary -> case summaryStatusFor request summary of
      Nothing -> pure ()
      Just prior
        | statusAdvancesPending prior status -> pure ()
      Just _ -> Left (ApplicationClientSummaryContradiction request)

latestPriorResumeSummary ::
  Word64 ->
  Map Word64 CursorObservation ->
  Maybe ApplicationRequestSummaryDto
latestPriorResumeSummary cursorWord = go . reverse . Map.toAscList . Map.filterWithKey (\key _ -> key < cursorWord)
  where
    go [] = Nothing
    go ((_, ObservedSessionResume summary) : _) = Just summary
    go (_ : observations) = go observations

statusAdvancesPending ::
  ApplicationRequestSummaryStatusDto ->
  ApplicationRequestSummaryStatusDto ->
  Bool
statusAdvancesPending prior later = case (prior, later) of
  (PendingDto _, CompletedDto _) -> True
  (PendingDto _, CancelledDto) -> True
  (OperationAcceptedDto _, CompletedDto _) -> True
  _ -> False

summaryStatusFor ::
  ApplicationRequestIdClaim ->
  ApplicationRequestSummaryDto ->
  Maybe ApplicationRequestSummaryStatusDto
summaryStatusFor request = go . applicationRequestSummaryEntries
  where
    go [] = Nothing
    go (entry : entries)
      | summaryEntryRequestId entry == request = Just (summaryEntryStatus entry)
      | otherwise = go entries

historicalStatusPrecedes ::
  ApplicationRequestSummaryStatusDto ->
  ApplicationRequestSummaryStatusDto ->
  Bool
historicalStatusPrecedes historical later = case (historical, later) of
  (PendingDto historicalWait, PendingDto laterWait) -> historicalWait == laterWait
  (PendingDto _, CompletedDto _) -> True
  (PendingDto _, CancelledDto) -> True
  (OperationAcceptedDto historicalReason, OperationAcceptedDto laterReason) ->
    historicalReason == laterReason
  (OperationAcceptedDto _, CompletedDto _) -> True
  (CompletedDto historicalResult, CompletedDto laterResult) ->
    historicalResult == laterResult
  (RejectedDto historicalRejection, RejectedDto laterRejection) ->
    historicalRejection == laterRejection
  (CancelledDto, CancelledDto) -> True
  _ -> False

historicalTerminalMatches ::
  ApplicationRequestSummaryStatusDto ->
  Maybe RequestTerminal ->
  Bool
historicalTerminalMatches status terminal = case status of
  PendingDto _ -> False
  OperationAcceptedDto _ -> False
  CompletedDto result -> terminal == Just (RequestSucceeded result)
  RejectedDto rejection -> terminal == Just (RequestRejected rejection)
  CancelledDto -> terminal == Just RequestCancelled

directPendingCancellation ::
  ApplicationRequestIdClaim ->
  ApplicationRequestSummaryStatusDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault [ApplicationClientEffect]
directPendingCancellation request status state = case status of
  PendingDto wait -> do
    retained <- lookupRequest request state
    facts <- requireSession state
    pure
      [ SendApplicationDto (CancelPendingWait facts.session request wait)
      | retained.cancellationRequested
      ]
  OperationAcceptedDto _ -> pure []
  _ -> pure []

submitCall ::
  ApplicationInvocationId ->
  ApplicationOperation ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
submitCall invocation operation state
  | invocationUsed invocation state =
      pure
        ( ApplicationClientCommandRejected
            (ApplicationInvocationAlreadyUsed invocation)
        )
  | otherwise = case state.phase of
      ActivePhase _ -> admit True
      IsolationReadOnlyPhase _ _ -> case operation of
        ReadApplication {} -> admit True
        _ ->
          advanced
            state
            [ CallFinished
                invocation
                (ApplicationCallRejected ApplicationOperateNotPermitted)
            ]
      ReconnectingPhase _ _ -> admit False
      _ -> rejectWrongPhase
  where
    rejectWrongPhase =
      pure
        ( ApplicationClientCommandRejected
            (ApplicationClientWrongPhase invocation (applicationClientPhase state))
        )
    admit sendNow = do
      facts <- requireSession state
      let request = applicationRequestIdClaim state.nextRequest
          call = Call facts.session request operation
          retained =
            ClientRequest
              { invocation,
                call,
                wait = Nothing,
                cancellationRequested = False,
                terminal = Nothing
              }
          successor =
            state
              { nextRequest = state.nextRequest + 1,
                latestInvocation = Just invocation,
                invocationRequests =
                  Map.insert invocation state.nextRequest state.invocationRequests,
                requests = Map.insert state.nextRequest retained state.requests
              }
          effects = [SendApplicationDto (withReceiptRetirement state call) | sendNow]
      advanced successor effects

cancelInvocation ::
  ApplicationInvocationId ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
cancelInvocation invocation state =
  case Map.lookup invocation state.invocationRequests of
    Nothing ->
      pure
        ( ApplicationClientCommandRejected
            (if invocationUsed invocation state then ApplicationInvocationRetired invocation else ApplicationInvocationUnknown invocation)
        )
    Just requestWord -> do
      request <- lookupRequest (applicationRequestIdClaim requestWord) state
      case request.terminal of
        Just _ -> advanced state []
        Nothing
          | not (isWaitCall request.call) ->
              pure
                ( ApplicationClientCommandRejected
                    (ApplicationClientWrongPhase invocation (applicationClientPhase state))
                )
          | otherwise -> case state.phase of
              ActivePhase _ -> recordCancellation requestWord request True
              ReconnectingPhase _ _ -> recordCancellation requestWord request False
              _ ->
                pure
                  ( ApplicationClientCommandRejected
                      (ApplicationClientWrongPhase invocation (applicationClientPhase state))
                  )
  where
    recordCancellation requestWord request sendNow = do
      facts <- requireSession state
      let updated = request {cancellationRequested = True}
          successor = state {requests = Map.insert requestWord updated state.requests}
          effects = case (sendNow, request.wait) of
            (True, Just wait) ->
              [ SendApplicationDto
                  ( CancelPendingWait
                      facts.session
                      (applicationRequestIdClaim requestWord)
                      wait
                  )
              ]
            _ -> []
      advanced successor effects

endClient ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
endClient state = case state.phase of
  UnopenedPhase -> closeWithoutSession
  OpeningPhase _ -> closeWithoutSession
  EndRecoveryPhase _ -> closeDisconnected
  ActivePhase _ -> do
    facts <- requireSession state
    let (closed, effects) = closeUnresolved CallClosed RequestClosed state
    advanced
      closed {phase = ClosedPhase}
      ([SendApplicationDto (EndSession facts.session)] <> effects <> [CloseTransport])
  IsolationReadOnlyPhase _ _ -> do
    facts <- requireSession state
    let (closed, effects) = closeUnresolved CallClosed RequestClosed state
    advanced
      closed {phase = ClosedPhase}
      ([SendApplicationDto (EndSession facts.session)] <> effects <> [CloseTransport])
  ReconnectingPhase _ _ -> closeDisconnected
  EndingPhase _ _ -> closeDisconnected
  ClosedPhase -> advanced state []
  PermanentlyUnavailablePhase -> advanced state []
  where
    closeWithoutSession =
      advanced
        state {phase = ClosedPhase}
        (timerCancellationEffects state <> [CloseTransport])
    closeDisconnected =
      let (closed, effects) = closeUnresolved CallClosed RequestClosed state
       in advanced
            closed {phase = ClosedPhase}
            (timerCancellationEffects state <> effects <> [CloseTransport])

permanentlyUnavailable ::
  ApplicationUnavailableReason ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
permanentlyUnavailable reason state = case state.phase of
  ClosedPhase -> advanced state []
  PermanentlyUnavailablePhase -> advanced state []
  _ -> settleUnavailable reason state

settleUnavailable ::
  ApplicationUnavailableReason ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
settleUnavailable reason state
  | Just request <- pendingOwnEnd state = beginEndRecovery request state
  | otherwise = settleUnavailableOrdinary reason state

settleUnavailableOrdinary :: ApplicationUnavailableReason -> ApplicationClientState -> Either ApplicationClientInvariantFault ApplicationClientTransition
settleUnavailableOrdinary reason state =
  let terminal = RequestUnknown reason
      effect invocation = CallUnknown invocation reason
      (settled, effects) = closeUnresolved effect terminal state
   in advanced
        settled {phase = PermanentlyUnavailablePhase}
        ( timerCancellationEffects state
            <> effects
            <> [HeraldPermanentlyUnavailableObserved reason, CloseTransport]
        )

timerCancellationEffects :: ApplicationClientState -> [ApplicationClientEffect]
timerCancellationEffects state =
  recoveryCancellation <> retryCancellation
  where
    recoveryCancellation = case phaseRecovery state.phase of
      Nothing -> []
      Just recovery -> [cancelRecoveryEffect recovery]
    retryCancellation = case state.phase of
      OpeningPhase (AwaitingTransportRetry attempt _) ->
        [CancelTransportRetry ApplicationOpeningRetry attempt]
      EndRecoveryPhase (AwaitingTransportRetry attempt _) ->
        [CancelTransportRetry ApplicationOpeningRetry attempt]
      ReconnectingPhase recovery (AwaitingTransportRetry attempt _) ->
        [ CancelTransportRetry
            (ApplicationSessionRecoveryRetry recovery.generation)
            attempt
        ]
      EndingPhase recovery (AwaitingTransportRetry attempt _) ->
        [ CancelTransportRetry
            (ApplicationSessionRecoveryRetry recovery.generation)
            attempt
        ]
      _ -> []

closeUnresolved ::
  (ApplicationInvocationId -> ApplicationClientEffect) ->
  RequestTerminal ->
  ApplicationClientState ->
  (ApplicationClientState, [ApplicationClientEffect])
closeUnresolved makeEffect terminal state =
  let settle (retainedRequests, retainedEffects) (requestWord, request)
        | isJust request.terminal = (retainedRequests, retainedEffects)
        | otherwise =
            ( Map.insert
                requestWord
                request
                  { cancellationRequested = False,
                    terminal = Just terminal
                  }
                retainedRequests,
              retainedEffects <> [makeEffect request.invocation]
            )
      (requests, effects) =
        foldl' settle (state.requests, []) (Map.toAscList state.requests)
      lifecycleEffects =
        [ case terminal of
            RequestUnknown reason -> LifecycleUnknown request.invocation reason
            _ -> LifecycleClosed request.invocation
        | request <- Map.elems state.lifecycleRequests,
          not (lifecycleTerminal request)
        ]
      lifecycleRequests = fmap (\request -> if lifecycleTerminal request then request else request {closed = True}) state.lifecycleRequests
   in (state {requests, lifecycleRequests, pendingStartup = Nothing}, effects <> lifecycleEffects)

reconcileSummary ::
  Bool ->
  ApplicationRequestSummaryDto ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault (ApplicationClientState, [ApplicationClientEffect])
reconcileSummary sendFollowUps summary state = do
  let entries = applicationRequestSummaryEntries summary
      byRequest =
        Map.fromList
          [ (applicationRequestIdClaimWord64 (summaryEntryRequestId entry), entry)
          | entry <- entries
          ]
  mapM_ (ensureSummaryRequest state) entries
  foldRequests sendFollowUps byRequest state (Map.toAscList state.requests)

ensureSummaryRequest ::
  ApplicationClientState ->
  ApplicationRequestSummaryEntryDto ->
  Either ApplicationClientInvariantFault ()
ensureSummaryRequest state entry =
  ensureKnownRequest (summaryEntryRequestId entry) state

foldRequests ::
  Bool ->
  Map Word64 ApplicationRequestSummaryEntryDto ->
  ApplicationClientState ->
  [(Word64, ClientRequest)] ->
  Either ApplicationClientInvariantFault (ApplicationClientState, [ApplicationClientEffect])
foldRequests sendFollowUps entries initial requests =
  foldl' step (Right (initial, [])) requests
  where
    step accumulated (requestWord, _) = do
      (state, effects) <- accumulated
      case Map.lookup requestWord entries of
        Nothing -> do
          request <- lookupRequest (applicationRequestIdClaim requestWord) state
          case (request.wait, request.terminal) of
            (Just _, _) ->
              Left
                ( ApplicationClientSummaryContradiction
                    (applicationRequestIdClaim requestWord)
                )
            (_, Just _) ->
              Left
                ( ApplicationClientSummaryContradiction
                    (applicationRequestIdClaim requestWord)
                )
            (Nothing, Nothing) ->
              pure
                ( state,
                  effects <> [SendApplicationDto request.call | sendFollowUps]
                )
        Just entry -> do
          let request = summaryEntryRequestId entry
              status = summaryEntryStatus entry
          (successor, statusEffects) <- applyRequestStatus request status Nothing state
          refreshed <- lookupRequest request successor
          facts <- requireSession successor
          let followUp = case status of
                PendingDto wait
                  | sendFollowUps && refreshed.cancellationRequested ->
                      [SendApplicationDto (CancelPendingWait facts.session request wait)]
                  | sendFollowUps ->
                      [SendApplicationDto (GetRequestResult facts.session request)]
                  | otherwise -> []
                OperationAcceptedDto _
                  | sendFollowUps ->
                      [SendApplicationDto (GetRequestResult facts.session request)]
                  | otherwise -> []
                _ -> []
          pure (successor, effects <> statusEffects <> followUp)

applyRequestStatus ::
  ApplicationRequestIdClaim ->
  ApplicationRequestSummaryStatusDto ->
  Maybe ApplicationWaitClaim ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault (ApplicationClientState, [ApplicationClientEffect])
applyRequestStatus request status directWait state = do
  initial <- lookupRequest request state
  case status of
    PendingDto wait -> ensureWait request wait state
    OperationAcceptedDto reason ->
      if operationMayStabilize initial.call reason
        then pure ()
        else Left (ApplicationClientSummaryContradiction request)
    _ -> pure ()
  withWait <- case status of
    PendingDto wait -> mergeWait request wait initial
    _ -> maybe (pure initial) (\wait -> mergeWait request wait initial) directWait
  let requestWord = applicationRequestIdClaimWord64 request
      stateWithWait = state {requests = Map.insert requestWord withWait state.requests}
  case status of
    PendingDto _ ->
      case withWait.terminal of
        Nothing -> pure (stateWithWait, [])
        _ -> Left (ApplicationClientSummaryContradiction request)
    OperationAcceptedDto _ ->
      case withWait.terminal of
        Nothing -> pure (stateWithWait, [])
        _ -> Left (ApplicationClientSummaryContradiction request)
    CompletedDto result -> do
      ensureResultMatches request result withWait.call
      settleRequest request (RequestSucceeded result) stateWithWait
    RejectedDto rejection ->
      if isJust withWait.wait
        then Left (ApplicationClientSummaryContradiction request)
        else settleRequest request (RequestRejected rejection) stateWithWait
    CancelledDto ->
      if isWaitCall withWait.call
        then settleRequest request RequestCancelled stateWithWait
        else Left (ApplicationClientSummaryContradiction request)

mergeWait ::
  ApplicationRequestIdClaim ->
  ApplicationWaitClaim ->
  ClientRequest ->
  Either ApplicationClientInvariantFault ClientRequest
mergeWait request wait clientRequest
  | not (isWaitCall clientRequest.call) =
      Left (ApplicationClientWaitMismatch request)
  | otherwise = case clientRequest.wait of
      Nothing -> pure clientRequest {wait = Just wait}
      Just oldWait
        | oldWait == wait -> pure clientRequest
        | otherwise -> Left (ApplicationClientWaitMismatch request)

settleRequest ::
  ApplicationRequestIdClaim ->
  RequestTerminal ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault (ApplicationClientState, [ApplicationClientEffect])
settleRequest request terminal state = do
  retained <- lookupRequest request state
  case retained.terminal of
    Nothing ->
      let updated =
            retained
              { cancellationRequested = False,
                terminal = Just terminal
              }
          successor =
            state
              { requests =
                  Map.insert
                    (applicationRequestIdClaimWord64 request)
                    updated
                    state.requests
              }
       in pure (successor, terminalEffects retained.invocation terminal)
    Just oldTerminal
      | oldTerminal == terminal -> pure (state, [])
      | otherwise -> Left (ApplicationClientSummaryContradiction request)

terminalEffects ::
  ApplicationInvocationId ->
  RequestTerminal ->
  [ApplicationClientEffect]
terminalEffects invocation = \case
  RequestSucceeded result ->
    [CallFinished invocation (ApplicationCallSucceeded result)]
  RequestRejected rejection ->
    [CallFinished invocation (ApplicationCallRejected rejection)]
  RequestCancelled -> [CallCancelled invocation]
  RequestUnknown reason -> [CallUnknown invocation reason]
  RequestClosed -> [CallClosed invocation]

ensureResultMatches ::
  ApplicationRequestIdClaim ->
  RegularCallResult ->
  ApplicationClientDto ->
  Either ApplicationClientInvariantFault ()
ensureResultMatches request result call
  | resultMatches call result = pure ()
  | otherwise = Left (ApplicationClientResultMismatch request)

resultMatches :: ApplicationClientDto -> RegularCallResult -> Bool
resultMatches (Call _ _ operation) result = case (operation, result) of
  (NewIdApplication _, NewIdCompleted _) -> True
  (WriteApplication _ (PublishValue (SortDefinitionValue _)), WriteCompleted (SortDefinitionWritten _)) -> True
  (WriteApplication _ (PublishValue value), WriteCompleted WriteAccepted)
    | not (isSortDefinitionValue value) -> True
  (WriteApplication _ (DeleteReserved _), WriteCompleted WriteAccepted) -> True
  (ForwardApplication _ _, ForwardCompleted ForwardAccepted) -> True
  (ReadApplication _, ReadCompleted _) -> True
  (LocalTakeApplication _, LocalTakeCompleted _) -> True
  (WaitApplication _, WaitCompleted _) -> True
  (LabelApplication _ _ _, LabelCompleted _) -> True
  (NewEnvironmentApplication, NewEnvironmentCompleted _) -> True
  _ -> False
resultMatches _ _ = False

isSortDefinitionValue :: ApplicationValue -> Bool
isSortDefinitionValue (SortDefinitionValue _) = True
isSortDefinitionValue _ = False

operationMayStabilize :: ApplicationClientDto -> OperationPendingReason -> Bool
operationMayStabilize call StructuralStabilizationPending = case call of
  Call _ _ (WriteApplication _ (PublishValue _)) -> True
  Call _ _ (ForwardApplication _ _) -> True
  _ -> False
operationMayStabilize call LabelSettlementPending = case call of
  Call _ _ LabelApplication {} -> True
  _ -> False
operationMayStabilize call EnvironmentStabilizationPending = case call of
  Call _ _ NewEnvironmentApplication -> True
  _ -> False

isWaitCall :: ApplicationClientDto -> Bool
isWaitCall (Call _ _ (WaitApplication _)) = True
isWaitCall _ = False

ensureSession ::
  ApplicationSessionClaim ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ()
ensureSession actual state = do
  expected <- requireSession state
  if actual == expected.session
    then pure ()
    else Left (ApplicationClientForeignSession expected.session actual)

ensureSessionTokenCoherent ::
  ApplicationSessionClaim ->
  ApplicationResumeTokenClaim ->
  Either ApplicationClientInvariantFault ()
ensureSessionTokenCoherent session token
  | applicationSessionClaimScopeBytes session
      == applicationResumeTokenClaimScopeBytes token
      && applicationSessionClaimOrdinal session
        == applicationResumeTokenClaimOrdinal token =
      pure ()
  | otherwise = Left (ApplicationClientSessionTokenMismatch session token)

ensureKnownRequest ::
  ApplicationRequestIdClaim ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ()
ensureKnownRequest request state = do
  _ <- lookupRequest request state
  pure ()

lookupRequest ::
  ApplicationRequestIdClaim ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ClientRequest
lookupRequest request state =
  maybe
    (Left (ApplicationClientUnknownRequest request))
    pure
    (Map.lookup (applicationRequestIdClaimWord64 request) state.requests)

ensureWait ::
  ApplicationRequestIdClaim ->
  ApplicationWaitClaim ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ()
ensureWait request wait state = do
  facts <- requireSession state
  if applicationWaitClaimRequestId wait == request
    && applicationWaitClaimScopeBytes wait
      == applicationSessionClaimScopeBytes facts.session
    && applicationWaitClaimSessionOrdinal wait
      == applicationSessionClaimOrdinal facts.session
    then pure ()
    else Left (ApplicationClientWaitMismatch request)

ensureLearnedWait ::
  ApplicationRequestIdClaim ->
  ApplicationWaitClaim ->
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ()
ensureLearnedWait request wait state = do
  ensureWait request wait state
  retained <- lookupRequest request state
  if retained.wait == Just wait
    then pure ()
    else Left (ApplicationClientWaitMismatch request)

requireSession ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault SessionFacts
requireSession state =
  maybe (Left ApplicationClientInvalidState) pure state.sessionFacts

requireCursor ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ApplicationReplyCursorClaim
requireCursor state =
  maybe (Left ApplicationClientInvalidState) pure state.lastObservedReply

advanced ::
  ApplicationClientState ->
  [ApplicationClientEffect] ->
  Either ApplicationClientInvariantFault ApplicationClientTransition
advanced state effects = do
  compacted <- compactClientReceipts state
  let needsFlush = compacted.retirementReady /= compacted.retirementConfirmed && compacted.retirementReady /= compacted.retirementOffered && not compacted.retirementFlushScheduled
      attempt = case compacted.phase of
        ActivePhase current -> Just current
        IsolationReadOnlyPhase current _ -> Just current
        _ -> Nothing
      scheduling = case attempt of
        Just current | needsFlush -> [ScheduleReceiptRetirement current (applicationClientRetryDelayMicroseconds compacted.recoveryConfiguration)]
        _ -> []
      successor = compacted {retirementFlushScheduled = compacted.retirementFlushScheduled || not (null scheduling)}
  pure (ApplicationClientAdvanced successor (applicationClientEffectBatch (effects <> scheduling)))

unexpectedInput ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault a
unexpectedInput =
  Left . ApplicationClientUnexpectedInput . applicationClientPhase

unexpectedServer ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault a
unexpectedServer =
  Left . ApplicationClientUnexpectedServerDto . applicationClientPhase

-- | Check compact structural relations of the private owner representation.
validateApplicationClientState ::
  ApplicationClientState ->
  Either ApplicationClientInvariantFault ()
validateApplicationClientState state
  | not phaseFactsAgree = invalid
  | state.nextTransportAttemptGeneration == 0 = invalid
  | state.nextSessionRecoveryGeneration == 0 = invalid
  | not currentAttemptPrecedesNext = invalid
  | not currentRecoveryPrecedesNext = invalid
  | any (>= state.nextRequest) (Map.keys state.requests) = invalid
  | state.retirementReady <> state.retirementConfirmed /= state.retirementReady = invalid
  | any (\key -> Retirement.receiptIsRetired key (ordinaryReceiptRetirement state.retirementReady)) (Map.keys state.requests) = invalid
  | any (\key -> Retirement.receiptIsRetired (Lifecycle.lifecycleRequestIdWord64 key) (lifecycleReceiptRetirement state.retirementReady)) (Map.keys state.lifecycleRequests) = invalid
  | Map.size state.invocationRequests /= Map.size state.requests = invalid
  | not (all requestsValid (Map.toAscList state.requests)) = invalid
  | not historyValid = invalid
  | isJust state.pendingStartup
      && (not (isJust state.preparedClaim) || not hasSession || not startupHandoffPhase) =
      invalid
  | not (all lifecycleEntryValid (Map.elems state.lifecycleRequests)) = invalid
  | any (`Map.member` state.invocationRequests) (fmap (.invocation) (Map.elems state.lifecycleRequests)) = invalid
  | case state.endRecovery of
      Nothing -> False
      Just request -> maybe (not terminalPhase) ((/= Lifecycle.EndOwnProcess) . (.command)) (Map.lookup request state.lifecycleRequests) =
      invalid
  | terminalPhase && any (not . lifecycleTerminal) (Map.elems state.lifecycleRequests) = invalid
  | terminalPhase && any (not . isJust . (.terminal)) (Map.elems state.requests) = invalid
  | otherwise = pure ()
  where
    invalid = Left ApplicationClientInvalidState
    hasSession = isJust state.sessionFacts
    hasCursor = isJust state.lastObservedReply
    startupHandoffPhase = case state.phase of
      ActivePhase _ -> True
      IsolationReadOnlyPhase _ _ -> True
      ReconnectingPhase _ _ -> True
      EndingPhase _ _ -> True
      ClosedPhase -> True
      PermanentlyUnavailablePhase -> True
      _ -> False
    phaseFactsAgree =
      sessionTokenAgree && case state.phase of
        UnopenedPhase -> not hasSession && not hasCursor && Map.null state.requests
        OpeningPhase _ -> not hasSession && not hasCursor && Map.null state.requests
        EndRecoveryPhase _ -> hasSession == hasCursor && isJust state.endRecovery
        ActivePhase _ -> hasSession && hasCursor
        IsolationReadOnlyPhase _ _ -> hasSession && hasCursor
        ReconnectingPhase _ _ -> hasSession && hasCursor
        EndingPhase _ _ -> hasSession && hasCursor
        ClosedPhase -> hasSession == hasCursor
        PermanentlyUnavailablePhase -> hasSession == hasCursor
    currentAttemptPrecedesNext = case phaseAttempt state.phase of
      Nothing -> True
      Just attempt ->
        applicationTransportAttemptGenerationWord64 attempt
          < state.nextTransportAttemptGeneration
    currentRecoveryPrecedesNext = case phaseRecovery state.phase of
      Nothing -> True
      Just recovery ->
        applicationSessionRecoveryGenerationWord64 recovery.generation
          < state.nextSessionRecoveryGeneration
    sessionTokenAgree = case state.sessionFacts of
      Nothing -> True
      Just facts ->
        applicationSessionClaimScopeBytes facts.session
          == applicationResumeTokenClaimScopeBytes facts.token
          && applicationSessionClaimOrdinal facts.session
            == applicationResumeTokenClaimOrdinal facts.token
    terminalPhase = case state.phase of
      ClosedPhase -> True
      PermanentlyUnavailablePhase -> True
      _ -> False
    lifecycleEntryValid entry = case entry.status of
      Just (Lifecycle.LifecycleCompleted result) -> Lifecycle.lifecycleResultMatches entry.command result
      _ -> True
    requestsValid (requestWord, request) =
      Map.lookup request.invocation state.invocationRequests == Just requestWord
        && callMatches requestWord request.call
        && waitValid requestWord request
        && not (request.cancellationRequested && isJust request.terminal)
        && (not request.cancellationRequested || isWaitCall request.call)
        && terminalValid request.call request.terminal
    callMatches requestWord (Call session request _) =
      applicationRequestIdClaimWord64 request == requestWord
        && maybe False ((== session) . (.session)) state.sessionFacts
    callMatches _ _ = False
    waitValid requestWord request = case request.wait of
      Nothing -> True
      Just wait ->
        isWaitCall request.call
          && applicationRequestIdClaimWord64 (applicationWaitClaimRequestId wait)
            == requestWord
          && maybe False (waitMatchesSession wait) state.sessionFacts
    waitMatchesSession wait facts =
      applicationWaitClaimScopeBytes wait
        == applicationSessionClaimScopeBytes facts.session
        && applicationWaitClaimSessionOrdinal wait
          == applicationSessionClaimOrdinal facts.session
    terminalValid _ Nothing = True
    terminalValid call (Just terminal) = case terminal of
      RequestSucceeded result -> resultMatches call result
      RequestCancelled -> isWaitCall call
      RequestRejected _ -> True
      RequestUnknown _ -> True
      RequestClosed -> True
    historyValid = case state.lastObservedReply of
      Nothing -> Map.null state.cursorHistory
      Just cursor ->
        all
          ( \(word, observation) ->
              word > 0
                && word <= applicationReplyCursorClaimWord64 cursor
                && observationValid observation
          )
          (Map.toAscList state.cursorHistory)
    observationValid ObservedSessionOpen = True
    observationValid (ObservedIsolationBegin _ _) = True
    observationValid (ObservedSessionResume summary) =
      all
        ( \entry ->
            case Map.lookup
              (applicationRequestIdClaimWord64 (summaryEntryRequestId entry))
              state.requests of
              Nothing -> False
              Just retained ->
                summaryStatusValid
                  (summaryEntryRequestId entry)
                  retained
                  (summaryEntryStatus entry)
        )
        (applicationRequestSummaryEntries summary)
    observationValid (ObservedRequest request status) =
      case Map.lookup (applicationRequestIdClaimWord64 request) state.requests of
        Nothing -> False
        Just retained -> summaryStatusValid request retained status
    summaryStatusValid request retained = \case
      PendingDto wait ->
        isWaitCall retained.call
          && applicationWaitClaimRequestId wait == request
          && maybe False (waitMatchesSession wait) state.sessionFacts
      OperationAcceptedDto reason -> operationMayStabilize retained.call reason
      CompletedDto result -> resultMatches retained.call result
      RejectedDto _ -> True
      CancelledDto -> isWaitCall retained.call

invocationUsed :: ApplicationInvocationId -> ApplicationClientState -> Bool
invocationUsed invocation state = maybe False (invocation <=) state.latestInvocation

lifecycleTerminal :: ClientLifecycleRequest -> Bool
lifecycleTerminal request =
  request.closed || case request.status of
    Just Lifecycle.LifecycleCompleted {} -> True
    Just Lifecycle.LifecycleRejected {} -> True
    _ -> False

submitLifecycle :: ApplicationInvocationId -> Lifecycle.LifecycleCommand -> ApplicationClientState -> Either ApplicationClientInvariantFault ApplicationClientTransition
submitLifecycle invocation command state
  | invocationUsed invocation state = pure (ApplicationClientCommandRejected (ApplicationInvocationAlreadyUsed invocation))
  | otherwise = case state.phase of
      ActivePhase _ -> admit True
      ReconnectingPhase _ _ -> admit False
      _ -> pure (ApplicationClientCommandRejected (ApplicationClientWrongPhase invocation (applicationClientPhase state)))
  where
    admit sendNow = do
      facts <- requireSession state
      let request = either (error . show) id (Lifecycle.lifecycleRequestId (applicationSessionClaimScopeBytes facts.session) (applicationSessionClaimOrdinal facts.session) (ClientInput.applicationInvocationIdWord64 invocation))
          entry = ClientLifecycleRequest invocation command Nothing False
      advanced
        state {lifecycleRequests = Map.insert request entry state.lifecycleRequests, latestInvocation = Just invocation, latestLifecycle = Just (Lifecycle.lifecycleRequestIdWord64 request)}
        (LifecycleAssigned invocation request : [SendApplicationDto (withReceiptRetirement state (LifecycleCall facts.session request command)) | sendNow])

lifecycleQueries :: ApplicationClientState -> [ApplicationClientEffect]
lifecycleQueries state = case state.sessionFacts of
  Nothing -> []
  Just facts -> [SendApplicationDto (GetLifecycleResult facts.session request) | (request, entry) <- Map.toAscList state.lifecycleRequests, not (lifecycleTerminal entry)]

receiveLifecycle :: ApplicationServerDto -> ApplicationClientState -> Either ApplicationClientInvariantFault ApplicationClientTransition
receiveLifecycle dto state = case dto of
  LifecycleReply request _ | locallyRetiredLifecycle request state -> advanced state []
  LifecycleRetired request through
    | state.endRecovery == Just request,
      Lifecycle.lifecycleRequestIdWord64 request <= through -> case Map.lookup request state.lifecycleRequests of
        Nothing -> advanced state []
        Just entry -> finishEndRecovery state {lifecycleRequests = Map.insert request entry {closed = True} state.lifecycleRequests} [LifecycleUnknown entry.invocation LifecycleResultNotRetained]
    | locallyRetiredLifecycle request state,
      Lifecycle.lifecycleRequestIdWord64 request <= through,
      Retirement.receiptIsRetired (Lifecycle.lifecycleRequestIdWord64 request) (lifecycleReceiptRetirement state.retirementReady),
      maybe False (through <=) (lifecycleReceiptRetirementThrough state.retirementReady) ->
        advanced state []
    | otherwise -> Left (ApplicationClientInvalidReceiptRetirement (applicationReceiptRetirement Nothing (Just through)))
  LifecycleReply request status -> do
    entry <- lookupLifecycle request state
    case status of
      Lifecycle.LifecycleCompleted result
        | not (Lifecycle.lifecycleResultMatches entry.command result) -> contradiction request
      _ -> pure ()
    if lifecycleTerminal entry
      then if entry.status == Just status then advanced state [] else contradiction request
      else do
        let successor = state {lifecycleRequests = Map.insert request entry {status = Just status} state.lifecycleRequests}
            effects = [LifecycleChanged entry.invocation status]
        case (state.endRecovery, status) of
          (Just _, Lifecycle.LifecyclePending _) -> advanced successor effects
          (Just _, _) -> finishEndRecovery successor effects
          _ -> advanced successor effects
  LifecycleAbsent request -> do
    entry <- lookupLifecycle request state
    if lifecycleTerminal entry
      then contradiction request
      else case state.endRecovery of
        Just expected
          | request == expected ->
              finishEndRecovery
                state {lifecycleRequests = Map.insert request entry {closed = True} state.lifecycleRequests}
                [LifecycleUnknown entry.invocation LifecycleResultNotRetained]
        _ -> do
          facts <- requireSession state
          advanced state [SendApplicationDto (LifecycleCall facts.session request entry.command)]
  LifecycleConflict request -> contradiction request
  _ -> unexpectedServer state
  where
    contradiction request = Left (ApplicationClientLifecycleContradiction request)

lookupLifecycle :: Lifecycle.LifecycleRequestId -> ApplicationClientState -> Either ApplicationClientInvariantFault ClientLifecycleRequest
lookupLifecycle request state = maybe (Left (ApplicationClientLifecycleContradiction request)) Right (Map.lookup request state.lifecycleRequests)

pendingOwnEnd :: ApplicationClientState -> Maybe Lifecycle.LifecycleRequestId
pendingOwnEnd state = case [request | (request, entry) <- Map.toAscList state.lifecycleRequests, entry.command == Lifecycle.EndOwnProcess, not (lifecycleTerminal entry)] of
  request : _ -> Just request
  [] -> Nothing

beginEndRecovery :: Lifecycle.LifecycleRequestId -> ApplicationClientState -> Either ApplicationClientInvariantFault ApplicationClientTransition
beginEndRecovery request state = do
  _ <- lookupLifecycle request state
  let pendingEnds = Map.filter (\entry -> entry.command == Lifecycle.EndOwnProcess && not (lifecycleTerminal entry)) state.lifecycleRequests
      (closed, effects) = closeUnresolved (\invocation -> CallUnknown invocation SessionNoLongerLive) (RequestUnknown SessionNoLongerLive) state
      retained = closed {endRecovery = Just request, lifecycleRequests = Map.union pendingEnds closed.lifecycleRequests}
      (allocated, attempt) = allocateTransportAttempt retained
      otherEffects = [effect | effect <- effects, case effect of LifecycleUnknown invocation _ -> not (any ((== invocation) . (.invocation)) (Map.elems pendingEnds)); _ -> True]
  advanced
    allocated {phase = EndRecoveryPhase (AwaitingTransport attempt)}
    (timerCancellationEffects state <> otherEffects <> [ResetTransport, RequestTransport attempt])

finishEndRecovery :: ApplicationClientState -> [ApplicationClientEffect] -> Either ApplicationClientInvariantFault ApplicationClientTransition
finishEndRecovery state prefix = case pendingOwnEnd state of
  Nothing -> advanced state {phase = ClosedPhase} (prefix <> [CloseTransport])
  Just next -> do
    transition <- beginEndRecovery next state
    case transition of
      ApplicationClientAdvanced successor effects -> advanced successor (prefix <> ProtocolEffects.applicationClientEffects effects)
      ApplicationClientCommandRejected _ -> Left ApplicationClientInvalidState

-- | Retained correlations owned by the client, excluding result handles owned
-- by callers. Completed requests do not remain in any of these maps.
applicationClientRetentionCounts :: ApplicationClientState -> (Int, Int, Int)
applicationClientRetentionCounts state = (Map.size state.requests, Map.size state.lifecycleRequests, Map.size state.cursorHistory)

applicationClientReceiptRetirement :: ApplicationClientState -> ApplicationReceiptRetirement
applicationClientReceiptRetirement state = state.retirementReady

-- | Transfer terminal payload ownership into the ordered effect batch, then
-- retain only pending work and compact sequence facts. Runtime effect batches
-- fill caller-owned cells before transmitting the associated acknowledgement.
compactClientReceipts :: ApplicationClientState -> Either ApplicationClientInvariantFault ApplicationClientState
compactClientReceipts state = do
  let pending = Map.filter (not . isJust . (.terminal)) state.requests
      pendingLifecycle = Map.filter (not . lifecycleTerminal) state.lifecycleRequests
      completedLifecycle = foldl' max state.latestLifecycleCompleted [Just (Lifecycle.lifecycleRequestIdWord64 request) | (request, entry) <- Map.toAscList state.lifecycleRequests, lifecycleTerminal entry]
      retainedObservation = \case
        ObservedRequest request _ -> Map.member (applicationRequestIdClaimWord64 request) pending
        ObservedSessionResume _ -> False
        _ -> True
  ordinary <- checkedRetirement (predecessor state.nextRequest) (Map.keysSet pending)
  lifecycle <- case state.latestLifecycle of
    -- A restricted End-result query does not own the original session's
    -- receipt namespace and cannot advertise its consumption.
    Nothing -> pure mempty
    Just high -> checkedRetirement (Just high) (Set.map Lifecycle.lifecycleRequestIdWord64 (Map.keysSet pendingLifecycle))
  pure
    state
      { requests = pending,
        invocationRequests = Map.filter (`Map.member` pending) state.invocationRequests,
        lifecycleRequests = pendingLifecycle,
        latestLifecycleCompleted = completedLifecycle,
        cursorHistory = Map.filter retainedObservation state.cursorHistory,
        retirementReady = state.retirementReady <> applicationReceiptRetirementWithExceptions ordinary lifecycle
      }
  where
    predecessor 0 = Nothing
    predecessor value = Just (value - 1)
    -- All exception identities were allocated by this owner at or below its
    -- high water mark; the checked constructor also governs wire admission.
    checkedRetirement high exceptions = case Retirement.receiptRetirement high exceptions of
      Right progress -> Right progress
      Left _ -> Left ApplicationClientInvalidState

withReceiptRetirement :: ApplicationClientState -> ApplicationClientDto -> ApplicationClientDto
withReceiptRetirement state dto
  | state.retirementReady == mempty = dto
  | otherwise = case dto of
      Call session request operation -> CallWithRetirement session request operation state.retirementReady
      LifecycleCall session request command -> LifecycleCallWithRetirement session request command state.retirementReady
      _ -> dto

confirmReceiptRetirement :: ApplicationReceiptRetirement -> ApplicationClientState -> Either ApplicationClientInvariantFault ApplicationClientTransition
confirmReceiptRetirement progress state
  | state.retirementReady <> progress /= state.retirementReady = Left (ApplicationClientInvalidReceiptRetirement progress)
  | otherwise =
      advanced
        state {retirementConfirmed = state.retirementConfirmed <> progress, pendingStartup = Nothing}
        [SessionReady startup | Just startup <- [state.pendingStartup]]

flushReceiptRetirement :: ApplicationTransportAttemptGeneration -> ApplicationClientState -> Either ApplicationClientInvariantFault ApplicationClientTransition
flushReceiptRetirement attempt state
  | phaseAttempt state.phase /= Just attempt = advanced state []
  | otherwise = case state.phase of
      ActivePhase _ -> flush
      IsolationReadOnlyPhase _ _ -> flush
      _ -> advanced state []
  where
    flush = do
      facts <- requireSession state
      let needed = state.retirementReady /= state.retirementConfirmed && state.retirementReady /= state.retirementOffered
      advanced
        state {retirementFlushScheduled = False, retirementOffered = state.retirementReady}
        [SendApplicationDto (RetireReceipts facts.session state.retirementReady) | needed]

locallyRetiredRequest :: ApplicationRequestIdClaim -> ApplicationClientState -> Bool
locallyRetiredRequest request state =
  let word = applicationRequestIdClaimWord64 request
   in word < state.nextRequest && Map.notMember word state.requests

retiredObservation :: ApplicationSessionClaim -> ApplicationReplyCursorClaim -> ApplicationClientState -> Either ApplicationClientInvariantFault ApplicationClientTransition
retiredObservation session cursor state = do
  ensureSession session state
  prior <- requireCursor state
  if cursor <= prior then advanced state [] else Left (ApplicationClientCursorConflict cursor)

locallyRetiredLifecycle :: Lifecycle.LifecycleRequestId -> ApplicationClientState -> Bool
locallyRetiredLifecycle request state =
  Map.notMember request state.lifecycleRequests
    && maybe False (Lifecycle.lifecycleRequestIdWord64 request <=) state.latestLifecycle
    && case state.sessionFacts of
      Nothing -> False
      Just facts ->
        Lifecycle.lifecycleRequestIdScopeBytes request == applicationSessionClaimScopeBytes facts.session
          && Lifecycle.lifecycleRequestIdSessionOrdinal request == applicationSessionClaimOrdinal facts.session
