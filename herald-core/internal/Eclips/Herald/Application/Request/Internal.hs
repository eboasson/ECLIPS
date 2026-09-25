-- | Package-private construction of application request correlations and
-- retained reply observations.
--
-- The safe facade exposes client-selected request construction and the closed
-- call/result vocabulary.  Owner-minted wait IDs, reply cursors, process
-- positions, summaries, and reply envelopes are constructed only here.
module Eclips.Herald.Application.Request.Internal
  ( RequestId,
    requestId,
    requestIdWord64,
    ApplicationClaimShapeError (..),
    WaitId,
    waitIdForSessionParts,
    applicationWaitIdFromClaimParts,
    waitIdHeraldEpoch,
    waitIdSessionOrdinal,
    waitIdRequestId,
    ApplicationReplyCursor,
    firstApplicationReplyCursor,
    nextApplicationReplyCursor,
    applicationReplyCursorFromClaimWord64,
    applicationReplyCursorWord64,
    ProcessAcceptancePosition,
    processAcceptancePosition,
    processAcceptancePositionProcess,
    processAcceptancePositionOrdinal,
    ApplicationRequestStatus (..),
    ApplicationRequestSummaryEntry,
    applicationRequestSummaryEntry,
    requestSummaryEntryRequestId,
    requestSummaryEntryStatus,
    ApplicationRequestSummary,
    applicationRequestSummary,
    requestSummaryEntries,
    RetainedRequestReplyBody (..),
    retainedReplyBodyRequestId,
    retainedReplyBodyStatus,
    ApplicationRequestReply (..),
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word64)
import Eclips.Application.Types.Lifetime (ApplicationReceiptRetirement)
import Eclips.Application.Types.Rejection (ApplicationRejection)
import Eclips.Application.Types.Result
  ( OperationPendingReason,
    RegularCallResult,
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    ProcessEpochId,
    mkHeraldEpoch,
  )
import Eclips.Domain.Label
  ( LabelProcessAcceptancePosition,
    labelProcessAcceptancePositionOrdinal,
    labelProcessAcceptancePositionProcess,
    mkLabelProcessAcceptancePosition,
  )

-- | Client-selected idempotence key, scoped to one application session.
newtype RequestId = RequestId Word64
  deriving stock (Eq, Ord, Show)

requestId :: Word64 -> RequestId
requestId = RequestId

requestIdWord64 :: RequestId -> Word64
requestIdWord64 (RequestId value) = value

-- | Structural failures while translating already decoded protocol claims into
-- their nominal Herald correlation types.  These failures establish shape only;
-- the application owner still decides whether a claim names live state.
data ApplicationClaimShapeError
  = ApplicationClaimWrongIdentityByteCount
  | ApplicationClaimZeroReplyCursor
  deriving stock (Eq, Show)

-- | Owner-derived identity of the one possible wait occurrence for a request.
--
-- The representation repeats the session's stable components rather than
-- importing the session type, keeping the request/session module dependency
-- acyclic.  'Application.State' is the sole caller of the constructor and
-- supplies the components of the containing session.
data WaitId = WaitId HeraldEpoch Word64 RequestId
  deriving stock (Eq, Ord, Show)

waitIdForSessionParts :: HeraldEpoch -> Word64 -> RequestId -> WaitId
waitIdForSessionParts = WaitId

-- | Reconstruct a nominal wait claim without minting or admitting a wait.
--
-- The RPC adapter is the sole non-owner caller.  'Application.State' continues
-- to use 'waitIdForSessionParts' for the distinct owner-derived construction.
applicationWaitIdFromClaimParts ::
  ByteString ->
  Word64 ->
  RequestId ->
  Either ApplicationClaimShapeError WaitId
applicationWaitIdFromClaimParts scope ordinal request = do
  herald <-
    either
      (const (Left ApplicationClaimWrongIdentityByteCount))
      Right
      (mkHeraldEpoch scope)
  Right (WaitId herald ordinal request)

waitIdHeraldEpoch :: WaitId -> HeraldEpoch
waitIdHeraldEpoch (WaitId herald _ _) = herald

waitIdSessionOrdinal :: WaitId -> Word64
waitIdSessionOrdinal (WaitId _ ordinal _) = ordinal

waitIdRequestId :: WaitId -> RequestId
waitIdRequestId (WaitId _ _ request) = request

-- | Session-local position of one newly retained outbound reply.
newtype ApplicationReplyCursor = ApplicationReplyCursor Word64
  deriving stock (Eq, Ord, Show)

firstApplicationReplyCursor :: ApplicationReplyCursor
firstApplicationReplyCursor = ApplicationReplyCursor 1

-- | Advance under the finite-run no-wrap premise.
nextApplicationReplyCursor :: ApplicationReplyCursor -> ApplicationReplyCursor
nextApplicationReplyCursor (ApplicationReplyCursor cursor) =
  ApplicationReplyCursor (cursor + 1)

-- | Reconstruct a positive cursor claim without allocating a reply.
applicationReplyCursorFromClaimWord64 ::
  Word64 ->
  Either ApplicationClaimShapeError ApplicationReplyCursor
applicationReplyCursorFromClaimWord64 0 = Left ApplicationClaimZeroReplyCursor
applicationReplyCursorFromClaimWord64 cursor =
  Right (ApplicationReplyCursor cursor)

applicationReplyCursorWord64 :: ApplicationReplyCursor -> Word64
applicationReplyCursorWord64 (ApplicationReplyCursor cursor) = cursor

-- | One accepted call position in a process-scoped Herald order.
--
-- The live request owner and the label-control domain deliberately share one
-- nominal position.  Keeping the established private name as an alias avoids
-- making the position application-visible while preventing a second counter
-- from drifting away from the home label acceptance cut.
type ProcessAcceptancePosition = LabelProcessAcceptancePosition

processAcceptancePosition :: ProcessEpochId -> Word64 -> ProcessAcceptancePosition
processAcceptancePosition process ordinal =
  case mkLabelProcessAcceptancePosition process ordinal of
    Right position -> position
    Left _ ->
      error
        "Eclips.Herald.Application.Request.Internal: zero process acceptance position"

processAcceptancePositionProcess :: ProcessAcceptancePosition -> ProcessEpochId
processAcceptancePositionProcess = labelProcessAcceptancePositionProcess

processAcceptancePositionOrdinal :: ProcessAcceptancePosition -> Word64
processAcceptancePositionOrdinal = labelProcessAcceptancePositionOrdinal

-- | Authoritative retained state of one admitted request.
data ApplicationRequestStatus
  = ApplicationRequestPending WaitId
  | ApplicationOperationAccepted OperationPendingReason
  | ApplicationRequestCompleted RegularCallResult
  | ApplicationRequestRejected ApplicationRejection
  | ApplicationRequestCancelled
  deriving stock (Eq, Show)

data ApplicationRequestSummaryEntry
  = ApplicationRequestSummaryEntry RequestId ApplicationRequestStatus
  deriving stock (Eq, Show)

applicationRequestSummaryEntry ::
  RequestId ->
  ApplicationRequestStatus ->
  ApplicationRequestSummaryEntry
applicationRequestSummaryEntry = ApplicationRequestSummaryEntry

requestSummaryEntryRequestId :: ApplicationRequestSummaryEntry -> RequestId
requestSummaryEntryRequestId (ApplicationRequestSummaryEntry request _) = request

requestSummaryEntryStatus ::
  ApplicationRequestSummaryEntry ->
  ApplicationRequestStatus
requestSummaryEntryStatus (ApplicationRequestSummaryEntry _ status) = status

-- | Request statuses in ascending 'RequestId' order.
newtype ApplicationRequestSummary
  = ApplicationRequestSummary [ApplicationRequestSummaryEntry]
  deriving stock (Eq, Show)

-- | Construct a summary from an already canonically ordered owner projection.
applicationRequestSummary ::
  [ApplicationRequestSummaryEntry] ->
  ApplicationRequestSummary
applicationRequestSummary = ApplicationRequestSummary

requestSummaryEntries ::
  ApplicationRequestSummary ->
  [ApplicationRequestSummaryEntry]
requestSummaryEntries (ApplicationRequestSummary entries) = entries

-- | One stable retained reply body for an admitted request.
data RetainedRequestReplyBody
  = WaitAccepted RequestId WaitId
  | OperationAccepted RequestId OperationPendingReason
  | Completed RequestId RegularCallResult
  | Rejected RequestId ApplicationRejection
  | Cancelled RequestId WaitId
  deriving stock (Eq, Show)

retainedReplyBodyRequestId :: RetainedRequestReplyBody -> RequestId
retainedReplyBodyRequestId = \case
  WaitAccepted request _ -> request
  OperationAccepted request _ -> request
  Completed request _ -> request
  Rejected request _ -> request
  Cancelled request _ -> request

retainedReplyBodyStatus ::
  RetainedRequestReplyBody ->
  ApplicationRequestStatus
retainedReplyBodyStatus = \case
  WaitAccepted _ wait -> ApplicationRequestPending wait
  OperationAccepted _ reason -> ApplicationOperationAccepted reason
  Completed _ result -> ApplicationRequestCompleted result
  Rejected _ rejection -> ApplicationRequestRejected rejection
  Cancelled _ _ -> ApplicationRequestCancelled

-- | Direct application request disposition.
--
-- An admitted request owns its cursor and body together.  Lookup absence and a
-- conflicting call are protocol/bookkeeping observations and allocate no cursor.
data ApplicationRequestReply
  = RetainedRequestReply ApplicationReplyCursor RetainedRequestReplyBody
  | RequestAbsent RequestId
  | RequestConflict RequestId
  | RequestRetired RequestId Word64
  | ApplicationReceiptsRetired ApplicationReceiptRetirement
  deriving stock (Eq, Show)
