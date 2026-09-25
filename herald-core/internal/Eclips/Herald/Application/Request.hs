-- | Safe application request, result, and reply observations.
--
-- Request IDs are client-selected.  Wait IDs and reply cursors are opaque values
-- obtained only from Herald replies; process acceptance positions and all owner
-- constructors remain package-private.
module Eclips.Herald.Application.Request
  ( RequestId,
    requestId,
    requestIdWord64,
    WaitId,
    ApplicationReplyCursor,
    applicationReplyCursorWord64,
    ApplicationOperation (..),
    ApplicationRejection (..),
    ApplicationRequestStatus (..),
    ApplicationRequestSummaryEntry,
    requestSummaryEntryRequestId,
    requestSummaryEntryStatus,
    ApplicationRequestSummary,
    requestSummaryEntries,
    RetainedRequestReplyBody (..),
    retainedReplyBodyRequestId,
    retainedReplyBodyStatus,
    ApplicationRequestReply (..),
  )
where

import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Herald.Application.Request.Internal
  ( ApplicationReplyCursor,
    ApplicationRequestReply (..),
    ApplicationRequestStatus (..),
    ApplicationRequestSummary,
    ApplicationRequestSummaryEntry,
    RequestId,
    RetainedRequestReplyBody (..),
    WaitId,
    applicationReplyCursorWord64,
    requestId,
    requestIdWord64,
    requestSummaryEntries,
    requestSummaryEntryRequestId,
    requestSummaryEntryStatus,
    retainedReplyBodyRequestId,
    retainedReplyBodyStatus,
  )
