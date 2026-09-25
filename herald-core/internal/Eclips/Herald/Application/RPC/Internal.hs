-- | Exhaustive conversion between structurally checked application-protocol
-- claims and Herald-owned nominal correlations.
--
-- These conversions establish representation shape only.  They neither mint
-- owner state nor prove that a session, request, wait, or cursor exists.
module Eclips.Herald.Application.RPC.Internal
  ( ApplicationRpcClaimError (..),
    attachmentFromClaim,
    attachmentToClaim,
    sessionFromClaim,
    sessionToClaim,
    resumeTokenFromClaim,
    resumeTokenToClaim,
    requestFromClaim,
    requestToClaim,
    waitFromClaim,
    waitToClaim,
    cursorFromClaim,
    cursorToClaim,
    nonceFromClaim,
    nonceToClaim,
    sessionErrorToDto,
    sessionUnavailableReasonToDto,
    summaryToDto,
    requestReplyToDto,
    waitWakeToDto,
    sessionAcceptanceToDto,
  )
where

import Eclips.Application.Types.Result (WaitResult)
import Eclips.Domain.Identity (heraldEpochBytes)
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Protocol.Application.Types qualified as Protocol

-- | A structurally checked protocol claim contradicted the Herald nominal
-- representation.  With the current shared widths this is unreachable after
-- protocol decoding and is therefore an adapter fault, not a stateful rejection.
data ApplicationRpcClaimError
  = ApplicationRpcClaimShapeContradiction
  | ApplicationRpcSummaryOrderContradiction
  deriving stock (Eq, Show)

attachmentFromClaim ::
  Protocol.ApplicationAttachmentClaim ->
  Either ApplicationRpcClaimError Session.ApplicationAttachment
attachmentFromClaim claim =
  mapClaimError
    (Session.applicationAttachmentFromClaimBytes (Protocol.applicationAttachmentClaimBytes claim))

attachmentToClaim ::
  Session.ApplicationAttachment ->
  Either ApplicationRpcClaimError Protocol.ApplicationAttachmentClaim
attachmentToClaim attachment =
  mapProtocolClaimError
    (Protocol.applicationAttachmentClaim (Session.applicationAttachmentBytes attachment))

sessionFromClaim ::
  Protocol.ApplicationSessionClaim ->
  Either ApplicationRpcClaimError Session.ApplicationSessionId
sessionFromClaim claim =
  mapClaimError
    ( Session.applicationSessionIdFromClaimParts
        (Protocol.applicationSessionClaimScopeBytes claim)
        (Protocol.applicationSessionClaimOrdinal claim)
    )

sessionToClaim ::
  Session.ApplicationSessionId ->
  Either ApplicationRpcClaimError Protocol.ApplicationSessionClaim
sessionToClaim session =
  mapProtocolClaimError
    ( Protocol.applicationSessionClaim
        (heraldEpochBytes (Session.applicationSessionIdHeraldEpoch session))
        (Session.applicationSessionIdOrdinal session)
    )

resumeTokenFromClaim ::
  Protocol.ApplicationResumeTokenClaim ->
  Either ApplicationRpcClaimError Session.ApplicationResumeToken
resumeTokenFromClaim claim =
  mapClaimError
    ( Session.applicationResumeTokenFromClaimParts
        (Protocol.applicationResumeTokenClaimScopeBytes claim)
        (Protocol.applicationResumeTokenClaimOrdinal claim)
    )

resumeTokenToClaim ::
  Session.ApplicationResumeToken ->
  Either ApplicationRpcClaimError Protocol.ApplicationResumeTokenClaim
resumeTokenToClaim token =
  mapProtocolClaimError
    ( Protocol.applicationResumeTokenClaim
        (heraldEpochBytes (Session.applicationResumeTokenHeraldEpoch token))
        (Session.applicationResumeTokenOrdinal token)
    )

requestFromClaim :: Protocol.ApplicationRequestIdClaim -> Request.RequestId
requestFromClaim = Request.requestId . Protocol.applicationRequestIdClaimWord64

requestToClaim :: Request.RequestId -> Protocol.ApplicationRequestIdClaim
requestToClaim = Protocol.applicationRequestIdClaim . Request.requestIdWord64

waitFromClaim ::
  Protocol.ApplicationWaitClaim ->
  Either ApplicationRpcClaimError Request.WaitId
waitFromClaim claim =
  mapClaimError
    ( Request.applicationWaitIdFromClaimParts
        (Protocol.applicationWaitClaimScopeBytes claim)
        (Protocol.applicationWaitClaimSessionOrdinal claim)
        (requestFromClaim (Protocol.applicationWaitClaimRequestId claim))
    )

waitToClaim ::
  Request.WaitId ->
  Either ApplicationRpcClaimError Protocol.ApplicationWaitClaim
waitToClaim wait =
  mapProtocolClaimError
    ( Protocol.applicationWaitClaim
        (heraldEpochBytes (Request.waitIdHeraldEpoch wait))
        (Request.waitIdSessionOrdinal wait)
        (requestToClaim (Request.waitIdRequestId wait))
    )

cursorFromClaim ::
  Protocol.ApplicationReplyCursorClaim ->
  Either ApplicationRpcClaimError Request.ApplicationReplyCursor
cursorFromClaim =
  mapClaimError
    . Request.applicationReplyCursorFromClaimWord64
    . Protocol.applicationReplyCursorClaimWord64

cursorToClaim ::
  Request.ApplicationReplyCursor ->
  Either ApplicationRpcClaimError Protocol.ApplicationReplyCursorClaim
cursorToClaim =
  mapProtocolClaimError
    . Protocol.applicationReplyCursorClaim
    . Request.applicationReplyCursorWord64

nonceFromClaim :: Protocol.ApplicationClientNonce -> Session.ClientNonce
nonceFromClaim = Session.clientNonce . Protocol.applicationClientNonceWord64

nonceToClaim :: Session.ClientNonce -> Protocol.ApplicationClientNonce
nonceToClaim = Protocol.applicationClientNonce . Session.clientNonceWord64

sessionErrorToDto ::
  Session.ApplicationSessionRejection ->
  Protocol.ApplicationSessionErrorDto
sessionErrorToDto = \case
  Session.ApplicationAttachmentNotAdmitted -> Protocol.ApplicationAttachmentNotAdmittedDto
  Session.ApplicationSessionNotLive -> Protocol.ApplicationSessionNotLiveDto
  Session.ApplicationResumeTokenMismatch -> Protocol.ApplicationResumeTokenMismatchDto
  Session.ApplicationReplyCursorNotIssued -> Protocol.ApplicationReplyCursorNotIssuedDto
  Session.ApplicationRequestSessionMismatch -> Protocol.ApplicationRequestSessionMismatchDto
  Session.ApplicationRequestBindingMismatch -> Protocol.ApplicationRequestBindingMismatchDto
  Session.ApplicationRequestBindingNotLive -> Protocol.ApplicationRequestBindingNotLiveDto
  Session.ApplicationEndSessionBindingMismatch -> Protocol.ApplicationEndSessionBindingMismatchDto
  Session.ApplicationReceiptRetirementNotReady -> Protocol.ApplicationReceiptRetirementNotReadyDto
  Session.ApplicationRequestAlreadyRetired -> Protocol.ApplicationRequestAlreadyRetiredDto

sessionUnavailableReasonToDto ::
  Session.ApplicationSessionUnavailableReason ->
  Protocol.ApplicationSessionUnavailableReasonDto
sessionUnavailableReasonToDto = \case
  Session.ApplicationSessionNoLongerLive ->
    Protocol.ApplicationSessionNoLongerLiveDto
  Session.ApplicationHeraldIsolated -> Protocol.HeraldIsolatedDto
  Session.ApplicationHeraldRetired -> Protocol.HeraldRetiredDto

summaryToDto ::
  Request.ApplicationRequestSummary ->
  Either ApplicationRpcClaimError Protocol.ApplicationRequestSummaryDto
summaryToDto summary = do
  entries <- traverse summaryEntryToDto (Request.requestSummaryEntries summary)
  case Protocol.applicationRequestSummaryDto entries of
    Left _ -> Left ApplicationRpcSummaryOrderContradiction
    Right checked -> Right checked

requestReplyToDto ::
  Session.ApplicationSessionBinding ->
  Request.ApplicationRequestReply ->
  Either ApplicationRpcClaimError Protocol.ApplicationServerDto
requestReplyToDto binding reply = do
  session <- sessionToClaim (Session.sessionBindingSessionId binding)
  case reply of
    Request.RetainedRequestReply cursor body -> do
      checkedCursor <- cursorToClaim cursor
      checkedBody <- retainedBodyToDto body
      pure
        ( Protocol.RequestRetained
            session
            checkedCursor
            (requestToClaim (Request.retainedReplyBodyRequestId body))
            checkedBody
        )
    Request.ApplicationReceiptsRetired progress ->
      pure (Protocol.ReceiptsRetired session progress)
    Request.RequestRetired request through ->
      pure (Protocol.RequestRetired session (requestToClaim request) through)
    Request.RequestAbsent request ->
      pure (Protocol.RequestAbsent session (requestToClaim request))
    Request.RequestConflict request ->
      pure (Protocol.RequestConflict session (requestToClaim request))

waitWakeToDto ::
  Session.ApplicationSessionBinding ->
  Request.ApplicationReplyCursor ->
  Request.RequestId ->
  Request.WaitId ->
  WaitResult ->
  Either ApplicationRpcClaimError Protocol.ApplicationServerDto
waitWakeToDto binding cursor request wait result = do
  session <- sessionToClaim (Session.sessionBindingSessionId binding)
  checkedCursor <- cursorToClaim cursor
  checkedWait <- waitToClaim wait
  pure
    ( Protocol.WaitWake
        session
        checkedCursor
        (requestToClaim request)
        checkedWait
        result
    )

sessionAcceptanceToDto ::
  Session.ApplicationSessionAcceptance ->
  Either ApplicationRpcClaimError Protocol.ApplicationServerDto
sessionAcceptanceToDto acceptance = do
  cursor <- cursorToClaim (Session.sessionAcceptanceCursor acceptance)
  case Session.sessionAcceptanceReply acceptance of
    Session.SessionOpened session token access -> do
      checkedSession <- sessionToClaim session
      checkedToken <- resumeTokenToClaim token
      pure (Protocol.SessionOpened cursor checkedSession checkedToken access)
    Session.SessionResumed session summary -> do
      checkedSession <- sessionToClaim session
      checkedSummary <- summaryToDto summary
      pure (Protocol.SessionResumed cursor checkedSession checkedSummary)

summaryEntryToDto ::
  Request.ApplicationRequestSummaryEntry ->
  Either ApplicationRpcClaimError Protocol.ApplicationRequestSummaryEntryDto
summaryEntryToDto entry = do
  status <- summaryStatusToDto (Request.requestSummaryEntryStatus entry)
  pure
    ( Protocol.ApplicationRequestSummaryEntryDto
        (requestToClaim (Request.requestSummaryEntryRequestId entry))
        status
    )

summaryStatusToDto ::
  Request.ApplicationRequestStatus ->
  Either ApplicationRpcClaimError Protocol.ApplicationRequestSummaryStatusDto
summaryStatusToDto = \case
  Request.ApplicationRequestPending wait -> Protocol.PendingDto <$> waitToClaim wait
  Request.ApplicationOperationAccepted reason ->
    pure (Protocol.OperationAcceptedDto reason)
  Request.ApplicationRequestCompleted result -> pure (Protocol.CompletedDto result)
  Request.ApplicationRequestRejected rejection -> pure (Protocol.RejectedDto rejection)
  Request.ApplicationRequestCancelled -> pure Protocol.CancelledDto

retainedBodyToDto ::
  Request.RetainedRequestReplyBody ->
  Either ApplicationRpcClaimError Protocol.ApplicationRequestReplyBodyDto
retainedBodyToDto = \case
  Request.WaitAccepted _ wait -> Protocol.WaitAccepted <$> waitToClaim wait
  Request.OperationAccepted _ reason -> pure (Protocol.OperationAccepted reason)
  Request.Completed _ result -> pure (Protocol.Completed result)
  Request.Rejected _ rejection -> pure (Protocol.Rejected rejection)
  Request.Cancelled _ wait -> Protocol.Cancelled <$> waitToClaim wait

mapClaimError ::
  Either Request.ApplicationClaimShapeError value ->
  Either ApplicationRpcClaimError value
mapClaimError = either (const (Left ApplicationRpcClaimShapeContradiction)) Right

mapProtocolClaimError ::
  Either Protocol.ApplicationClaimError value ->
  Either ApplicationRpcClaimError value
mapProtocolClaimError = either (const (Left ApplicationRpcClaimShapeContradiction)) Right
