{-# LANGUAGE DeriveAnyClass #-}

-- | Structurally checked application-protocol claims and the closed current DTO
-- vocabulary.
--
-- Decoding these values establishes representation shape only.  Stateful
-- attachment, session, request, wait, and cursor admission remains the Herald's
-- responsibility.
module Eclips.Protocol.Application.Types
  ( ApplicationClaimError (..),
    ApplicationAttachmentClaim,
    applicationAttachmentClaim,
    applicationAttachmentClaimBytes,
    ApplicationSessionClaim,
    applicationSessionClaim,
    applicationSessionClaimScopeBytes,
    applicationSessionClaimOrdinal,
    ApplicationResumeTokenClaim,
    applicationResumeTokenClaim,
    applicationResumeTokenClaimScopeBytes,
    applicationResumeTokenClaimOrdinal,
    ApplicationRequestIdClaim,
    applicationRequestIdClaim,
    applicationRequestIdClaimWord64,
    ApplicationWaitClaim,
    applicationWaitClaim,
    applicationWaitClaimScopeBytes,
    applicationWaitClaimSessionOrdinal,
    applicationWaitClaimRequestId,
    ApplicationReplyCursorClaim,
    applicationReplyCursorClaim,
    applicationReplyCursorClaimWord64,
    ApplicationClientNonce,
    applicationClientNonce,
    applicationClientNonceWord64,
    ApplicationHeartbeatNonce,
    applicationHeartbeatNonce,
    applicationHeartbeatNonceWord64,
    ApplicationRequestSummaryStatusDto (..),
    ApplicationRequestSummaryEntryDto (..),
    summaryEntryRequestId,
    summaryEntryStatus,
    ApplicationRequestSummaryError (..),
    ApplicationRequestSummaryDto,
    applicationRequestSummaryDto,
    applicationRequestSummaryEntries,
    ApplicationSessionErrorDto (..),
    ApplicationIsolationOverlayDto (..),
    ApplicationSessionUnavailableReasonDto (..),
    ApplicationRequestReplyBodyDto (..),
    ApplicationClientDto (..),
    ApplicationServerDto (..),
    ApplicationEnvelope (..),
  )
where

import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Types.Access (ApplicationStartupAccess)
import Eclips.Application.Types.Lifecycle hiding (LifecycleReply (..))
import Eclips.Application.Types.Lifetime (ApplicationReceiptRetirement)
import Eclips.Application.Types.Operation (ApplicationOperation)
import Eclips.Application.Types.Rejection (ApplicationRejection)
import Eclips.Application.Types.Result
  ( OperationPendingReason,
    RegularCallResult,
    WaitResult,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import GHC.Generics (Generic)

applicationClaimScopeByteCount :: Int
applicationClaimScopeByteCount = 32

-- | A malformed structural application-protocol claim.
data ApplicationClaimError
  = ApplicationClaimWrongScopeByteCount Int
  | ApplicationReplyCursorIsZero
  deriving stock (Eq, Show)

-- | Opaque attachment scope supplied while opening a session.
newtype ApplicationAttachmentClaim
  = ApplicationAttachmentClaim ByteString
  deriving stock (Eq, Ord)

instance Show ApplicationAttachmentClaim where
  show = renderGroupedHex . applicationAttachmentClaimBytes

-- | Check the exact 32-byte attachment representation.
applicationAttachmentClaim :: ByteString -> Either ApplicationClaimError ApplicationAttachmentClaim
applicationAttachmentClaim bytes =
  ApplicationAttachmentClaim <$> checkedScopeBytes bytes

-- | Observe the uninterpreted attachment bytes.
applicationAttachmentClaimBytes :: ApplicationAttachmentClaim -> ByteString
applicationAttachmentClaimBytes (ApplicationAttachmentClaim bytes) = bytes

instance Binary ApplicationAttachmentClaim where
  put = put . applicationAttachmentClaimBytes
  get = get >>= either (fail . show) pure . applicationAttachmentClaim

-- | Opaque session scope and its Herald-owned ordinal.
data ApplicationSessionClaim
  = ApplicationSessionClaim ByteString Word64
  deriving stock (Eq, Ord)

instance Show ApplicationSessionClaim where
  show (ApplicationSessionClaim scope ordinal) =
    "ApplicationSessionClaim "
      <> renderGroupedHex scope
      <> " "
      <> show ordinal

-- | Check a session claim's exact-width scope.
applicationSessionClaim :: ByteString -> Word64 -> Either ApplicationClaimError ApplicationSessionClaim
applicationSessionClaim bytes ordinal =
  (`ApplicationSessionClaim` ordinal) <$> checkedScopeBytes bytes

-- | Observe a session claim's uninterpreted scope.
applicationSessionClaimScopeBytes :: ApplicationSessionClaim -> ByteString
applicationSessionClaimScopeBytes (ApplicationSessionClaim bytes _) = bytes

-- | Observe a session claim's ordinal.
applicationSessionClaimOrdinal :: ApplicationSessionClaim -> Word64
applicationSessionClaimOrdinal (ApplicationSessionClaim _ ordinal) = ordinal

instance Binary ApplicationSessionClaim where
  put claim = do
    put (applicationSessionClaimScopeBytes claim)
    put (applicationSessionClaimOrdinal claim)
  get = do
    bytes <- get
    ordinal <- get
    either (fail . show) pure (applicationSessionClaim bytes ordinal)

-- | Opaque resume-token scope and its nominally separate ordinal.
data ApplicationResumeTokenClaim
  = ApplicationResumeTokenClaim ByteString Word64
  deriving stock (Eq, Ord)

instance Show ApplicationResumeTokenClaim where
  show (ApplicationResumeTokenClaim scope ordinal) =
    "ApplicationResumeTokenClaim "
      <> renderGroupedHex scope
      <> " "
      <> show ordinal

-- | Check a resume token's exact-width scope.
applicationResumeTokenClaim :: ByteString -> Word64 -> Either ApplicationClaimError ApplicationResumeTokenClaim
applicationResumeTokenClaim bytes ordinal =
  (`ApplicationResumeTokenClaim` ordinal) <$> checkedScopeBytes bytes

-- | Observe a resume token's uninterpreted scope.
applicationResumeTokenClaimScopeBytes :: ApplicationResumeTokenClaim -> ByteString
applicationResumeTokenClaimScopeBytes (ApplicationResumeTokenClaim bytes _) = bytes

-- | Observe a resume token's ordinal.
applicationResumeTokenClaimOrdinal :: ApplicationResumeTokenClaim -> Word64
applicationResumeTokenClaimOrdinal (ApplicationResumeTokenClaim _ ordinal) = ordinal

instance Binary ApplicationResumeTokenClaim where
  put claim = do
    put (applicationResumeTokenClaimScopeBytes claim)
    put (applicationResumeTokenClaimOrdinal claim)
  get = do
    bytes <- get
    ordinal <- get
    either (fail . show) pure (applicationResumeTokenClaim bytes ordinal)

-- | A client-selected request identifier scoped to one session.
newtype ApplicationRequestIdClaim
  = ApplicationRequestIdClaim Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

-- | Construct a request identifier.  Zero is valid.
applicationRequestIdClaim :: Word64 -> ApplicationRequestIdClaim
applicationRequestIdClaim = ApplicationRequestIdClaim

-- | Observe the request identifier's unsigned representation.
applicationRequestIdClaimWord64 :: ApplicationRequestIdClaim -> Word64
applicationRequestIdClaimWord64 (ApplicationRequestIdClaim value) = value

-- | The nominal identity of one accepted pending wait.
data ApplicationWaitClaim
  = ApplicationWaitClaim ByteString Word64 ApplicationRequestIdClaim
  deriving stock (Eq, Ord)

instance Show ApplicationWaitClaim where
  show (ApplicationWaitClaim scope ordinal request) =
    "ApplicationWaitClaim "
      <> renderGroupedHex scope
      <> " "
      <> show ordinal
      <> " "
      <> show request

-- | Check a wait claim's exact-width session scope.
applicationWaitClaim :: ByteString -> Word64 -> ApplicationRequestIdClaim -> Either ApplicationClaimError ApplicationWaitClaim
applicationWaitClaim bytes ordinal request =
  (\scope -> ApplicationWaitClaim scope ordinal request) <$> checkedScopeBytes bytes

-- | Observe a wait claim's uninterpreted session scope.
applicationWaitClaimScopeBytes :: ApplicationWaitClaim -> ByteString
applicationWaitClaimScopeBytes (ApplicationWaitClaim bytes _ _) = bytes

-- | Observe a wait claim's session ordinal.
applicationWaitClaimSessionOrdinal :: ApplicationWaitClaim -> Word64
applicationWaitClaimSessionOrdinal (ApplicationWaitClaim _ ordinal _) = ordinal

-- | Observe the request named by a wait claim.
applicationWaitClaimRequestId :: ApplicationWaitClaim -> ApplicationRequestIdClaim
applicationWaitClaimRequestId (ApplicationWaitClaim _ _ request) = request

instance Binary ApplicationWaitClaim where
  put claim = do
    put (applicationWaitClaimScopeBytes claim)
    put (applicationWaitClaimSessionOrdinal claim)
    put (applicationWaitClaimRequestId claim)
  get = do
    bytes <- get
    ordinal <- get
    request <- get
    either (fail . show) pure (applicationWaitClaim bytes ordinal request)

-- | A positive session-local retained-reply position.
newtype ApplicationReplyCursorClaim
  = ApplicationReplyCursorClaim Word64
  deriving stock (Eq, Ord, Show)

-- | Check the positive reply-cursor representation.
applicationReplyCursorClaim :: Word64 -> Either ApplicationClaimError ApplicationReplyCursorClaim
applicationReplyCursorClaim value
  | value == 0 = Left ApplicationReplyCursorIsZero
  | otherwise = Right (ApplicationReplyCursorClaim value)

-- | Observe the positive cursor representation.
applicationReplyCursorClaimWord64 :: ApplicationReplyCursorClaim -> Word64
applicationReplyCursorClaimWord64 (ApplicationReplyCursorClaim value) = value

instance Binary ApplicationReplyCursorClaim where
  put = put . applicationReplyCursorClaimWord64
  get = get >>= either (fail . show) pure . applicationReplyCursorClaim

-- | A client-selected nonce used for exact open retry.
newtype ApplicationClientNonce
  = ApplicationClientNonce Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

-- | Construct a client nonce.  Zero is valid.
applicationClientNonce :: Word64 -> ApplicationClientNonce
applicationClientNonce = ApplicationClientNonce

-- | Observe the nonce's unsigned representation.
applicationClientNonceWord64 :: ApplicationClientNonce -> Word64
applicationClientNonceWord64 (ApplicationClientNonce value) = value

-- | One lane-local EAPP heartbeat correlation.
--
-- It has no session or semantic identity. Zero is valid. Keeping it nominally
-- distinct prevents application heartbeat correlations from crossing another
-- protocol-family boundary accidentally.
newtype ApplicationHeartbeatNonce
  = ApplicationHeartbeatNonce Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

-- | Construct a lane-local heartbeat nonce. Zero is valid.
applicationHeartbeatNonce :: Word64 -> ApplicationHeartbeatNonce
applicationHeartbeatNonce = ApplicationHeartbeatNonce

-- | Observe the heartbeat nonce's unsigned representation.
applicationHeartbeatNonceWord64 :: ApplicationHeartbeatNonce -> Word64
applicationHeartbeatNonceWord64 (ApplicationHeartbeatNonce value) = value

-- | One status in the complete frozen resume summary.
data ApplicationRequestSummaryStatusDto
  = PendingDto ApplicationWaitClaim
  | OperationAcceptedDto OperationPendingReason
  | CompletedDto RegularCallResult
  | RejectedDto ApplicationRejection
  | CancelledDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | One request and its frozen status.
data ApplicationRequestSummaryEntryDto
  = ApplicationRequestSummaryEntryDto
      ApplicationRequestIdClaim
      ApplicationRequestSummaryStatusDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Observe the request named by one summary entry.
summaryEntryRequestId :: ApplicationRequestSummaryEntryDto -> ApplicationRequestIdClaim
summaryEntryRequestId (ApplicationRequestSummaryEntryDto request _) = request

-- | Observe one summary entry's status.
summaryEntryStatus :: ApplicationRequestSummaryEntryDto -> ApplicationRequestSummaryStatusDto
summaryEntryStatus (ApplicationRequestSummaryEntryDto _ status) = status

-- | A malformed request-summary ordering.
data ApplicationRequestSummaryError
  = ApplicationRequestSummaryNotStrictlyAscending
      ApplicationRequestIdClaim
      ApplicationRequestIdClaim
  deriving stock (Eq, Show)

-- | A complete request summary in strictly ascending request order.
newtype ApplicationRequestSummaryDto
  = ApplicationRequestSummaryDto [ApplicationRequestSummaryEntryDto]
  deriving stock (Eq, Show)

-- | Check strict ascending order and uniqueness.
applicationRequestSummaryDto :: [ApplicationRequestSummaryEntryDto] -> Either ApplicationRequestSummaryError ApplicationRequestSummaryDto
applicationRequestSummaryDto entries = do
  checkSummaryOrder entries
  pure (ApplicationRequestSummaryDto entries)

-- | Observe a checked summary's canonical request order.
applicationRequestSummaryEntries :: ApplicationRequestSummaryDto -> [ApplicationRequestSummaryEntryDto]
applicationRequestSummaryEntries (ApplicationRequestSummaryDto entries) = entries

instance Binary ApplicationRequestSummaryDto where
  put = put . applicationRequestSummaryEntries
  get = get >>= either (fail . show) pure . applicationRequestSummaryDto

-- | Current stateful session rejection categories mirrored for the wire.
data ApplicationSessionErrorDto
  = ApplicationAttachmentNotAdmittedDto
  | ApplicationSessionNotLiveDto
  | ApplicationResumeTokenMismatchDto
  | ApplicationReplyCursorNotIssuedDto
  | ApplicationRequestSessionMismatchDto
  | ApplicationRequestBindingMismatchDto
  | ApplicationRequestBindingNotLiveDto
  | ApplicationEndSessionBindingMismatchDto
  | ApplicationReceiptRetirementNotReadyDto
  | ApplicationRequestAlreadyRetiredDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The only abandoned-branch label overlay exposed during the bounded local
-- isolation drain. This is a closed lifecycle fact, not a fabricated list of
-- affected objects.
data ApplicationIsolationOverlayDto
  = ResidentProcessesZombie
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The closed current reasons for a terminal Herald disposition.
data ApplicationSessionUnavailableReasonDto
  = ApplicationSessionNoLongerLiveDto
  | HeraldIsolatedDto
  | HeraldRetiredDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The stable body retained for one admitted request.
data ApplicationRequestReplyBodyDto
  = WaitAccepted ApplicationWaitClaim
  | OperationAccepted OperationPendingReason
  | Completed RegularCallResult
  | Rejected ApplicationRejection
  | Cancelled ApplicationWaitClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The complete current client-to-server application vocabulary.
data ApplicationClientDto
  = OpenSession ApplicationAttachmentClaim ApplicationClientNonce
  | ResumeSession
      ApplicationSessionClaim
      ApplicationResumeTokenClaim
      ApplicationReplyCursorClaim
  | EndSession ApplicationSessionClaim
  | Call
      ApplicationSessionClaim
      ApplicationRequestIdClaim
      ApplicationOperation
  | GetRequestResult
      ApplicationSessionClaim
      ApplicationRequestIdClaim
  | CancelPendingWait
      ApplicationSessionClaim
      ApplicationRequestIdClaim
      ApplicationWaitClaim
  | Ping ApplicationHeartbeatNonce
  | ClaimInitial ConnectionDescriptor InitialClaimId
  | LifecycleCall ApplicationSessionClaim LifecycleRequestId LifecycleCommand
  | GetLifecycleResult ApplicationSessionClaim LifecycleRequestId
  | RecoverLifecycleResult ApplicationAttachmentClaim LifecycleRequestId
  | CallWithRetirement ApplicationSessionClaim ApplicationRequestIdClaim ApplicationOperation ApplicationReceiptRetirement
  | LifecycleCallWithRetirement ApplicationSessionClaim LifecycleRequestId LifecycleCommand ApplicationReceiptRetirement
  | RetireReceipts ApplicationSessionClaim ApplicationReceiptRetirement
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The complete current server-to-client application vocabulary.
data ApplicationServerDto
  = SessionOpened
      ApplicationReplyCursorClaim
      ApplicationSessionClaim
      ApplicationResumeTokenClaim
      ApplicationStartupAccess
  | SessionResumed
      ApplicationReplyCursorClaim
      ApplicationSessionClaim
      ApplicationRequestSummaryDto
  | SessionRejected ApplicationSessionErrorDto
  | RequestRetained
      ApplicationSessionClaim
      ApplicationReplyCursorClaim
      ApplicationRequestIdClaim
      ApplicationRequestReplyBodyDto
  | RequestAbsent
      ApplicationSessionClaim
      ApplicationRequestIdClaim
  | RequestConflict
      ApplicationSessionClaim
      ApplicationRequestIdClaim
  | WaitWake
      ApplicationSessionClaim
      ApplicationReplyCursorClaim
      ApplicationRequestIdClaim
      ApplicationWaitClaim
      WaitResult
  | Pong ApplicationHeartbeatNonce
  | HeraldIsolationBegun
      ApplicationReplyCursorClaim
      ApplicationSessionClaim
      ApplicationIsolationOverlayDto
      ApplicationSessionUnavailableReasonDto
  | InitialClaimPending InitialClaimId [Text]
  | InitialClaimRejected InitialClaimId StartupError
  | LifecycleReply LifecycleRequestId LifecycleStatus
  | LifecycleAbsent LifecycleRequestId
  | LifecycleConflict LifecycleRequestId
  | HeraldPermanentlyUnavailable
      ApplicationSessionClaim
      ApplicationSessionUnavailableReasonDto
  | ReceiptsRetired ApplicationSessionClaim ApplicationReceiptRetirement
  | RequestRetired ApplicationSessionClaim ApplicationRequestIdClaim Word64
  | LifecycleRetired LifecycleRequestId Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | One closed generically serialized application-protocol payload.
data ApplicationEnvelope
  = ApplicationClientEnvelope ApplicationClientDto
  | ApplicationServerEnvelope ApplicationServerDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

checkedScopeBytes :: ByteString -> Either ApplicationClaimError ByteString
checkedScopeBytes bytes
  | ByteString.length bytes == applicationClaimScopeByteCount = Right bytes
  | otherwise = Left (ApplicationClaimWrongScopeByteCount (ByteString.length bytes))

checkSummaryOrder :: [ApplicationRequestSummaryEntryDto] -> Either ApplicationRequestSummaryError ()
checkSummaryOrder entries = go (fmap summaryEntryRequestId entries)
  where
    go (first : second : rest)
      | first < second = go (second : rest)
      | otherwise = Left (ApplicationRequestSummaryNotStrictlyAscending first second)
    go _ = Right ()
