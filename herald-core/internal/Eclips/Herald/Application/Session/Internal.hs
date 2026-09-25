-- | Package-private construction and observation of application session
-- correlations.  The public facade intentionally omits owner-only minting and
-- representation accessors.
module Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    applicationAttachmentForBootstrap,
    applicationAttachmentForProcess,
    applicationAttachmentFromClaimBytes,
    applicationAttachmentBytes,
    primordialApplicationAttachment,
    ClientNonce,
    clientNonce,
    clientNonceWord64,
    ApplicationSessionId,
    applicationSessionIdFromClaimParts,
    applicationSessionIdHeraldEpoch,
    applicationSessionIdOrdinal,
    ApplicationResumeToken,
    applicationResumeTokenFromClaimParts,
    applicationResumeTokenHeraldEpoch,
    applicationResumeTokenOrdinal,
    SessionBindingGeneration,
    sessionBindingGenerationWord64,
    ApplicationSessionBinding,
    sessionBindingSessionId,
    sessionBindingGeneration,
    initialSessionAllocation,
    advanceSessionBinding,
    ApplicationSessionReply (..),
    sessionReplySessionId,
    ApplicationSessionAcceptance,
    applicationSessionAcceptance,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
    ApplicationSessionRejection (..),
    ApplicationSessionUnavailableReason (..),
  )
where

import Data.ByteString (ByteString)
import Data.List (find)
import Data.Word (Word64)
import Eclips.Application.Types.Access (ApplicationStartupAccess)
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    HeraldEpoch,
    ProcessEpochId,
    bootstrapManifestIdBytes,
    mkBootstrapManifestId,
    mkHeraldEpoch,
    processEpochIdBytes,
  )
import Eclips.Domain.Startup
  ( appliedBootstrapManifestId,
    appliedProcessResidence,
  )
import Eclips.Herald.Application.Request.Internal
  ( ApplicationClaimShapeError (..),
    ApplicationReplyCursor,
    ApplicationRequestSummary,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    checkedInitialBootstraps,
    checkedLocalHeraldEpoch,
    lookupConfiguredProcessBootstrap,
  )

-- | Correlation for one admitted genesis bootstrap or dynamic child attachment.
newtype ApplicationAttachment
  = ApplicationAttachment ByteString
  deriving stock (Eq, Ord, Show)

-- | Construct the attachment installed by one already checked bootstrap.
applicationAttachmentForBootstrap ::
  BootstrapManifestId ->
  ApplicationAttachment
applicationAttachmentForBootstrap = ApplicationAttachment . bootstrapManifestIdBytes

-- | The applied dynamic epoch is the resident attachment correlation.
applicationAttachmentForProcess :: ProcessEpochId -> ApplicationAttachment
applicationAttachmentForProcess = ApplicationAttachment . processEpochIdBytes

-- | Reconstruct the nominal attachment carried by an admitted protocol claim.
-- This proves only its exact byte shape; bootstrap ownership remains a stateful
-- Herald decision.
applicationAttachmentFromClaimBytes ::
  ByteString ->
  Either ApplicationClaimShapeError ApplicationAttachment
applicationAttachmentFromClaimBytes bytes =
  applicationAttachmentForBootstrap
    <$> either
      (const (Left ApplicationClaimWrongIdentityByteCount))
      Right
      (mkBootstrapManifestId bytes)

applicationAttachmentBytes :: ApplicationAttachment -> ByteString
applicationAttachmentBytes (ApplicationAttachment bytes) = bytes

-- | Obtain a primordial attachment only for a selected process resident at the
-- supplied checked genesis's local Herald.
--
-- This is a deterministic fixture and driver seam, not authentication evidence.
primordialApplicationAttachment ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  BootstrapManifestId ->
  Maybe ApplicationAttachment
primordialApplicationAttachment genesis checkedBootstraps manifest = do
  _ <- lookupConfiguredProcessBootstrap manifest genesis
  bootstrap <-
    find
      ((== manifest) . appliedBootstrapManifestId)
      (checkedInitialBootstraps checkedBootstraps)
  if appliedProcessResidence bootstrap == checkedLocalHeraldEpoch genesis
    then Just (applicationAttachmentForBootstrap manifest)
    else Nothing

-- | Client-selected retry correlation scoped to one resolved process epoch.
newtype ClientNonce = ClientNonce Word64
  deriving stock (Eq, Ord, Show)

clientNonce :: Word64 -> ClientNonce
clientNonce = ClientNonce

clientNonceWord64 :: ClientNonce -> Word64
clientNonceWord64 (ClientNonce value) = value

-- | Herald-epoch-qualified application session allocation.
data ApplicationSessionId
  = ApplicationSessionId HeraldEpoch Word64
  deriving stock (Eq, Ord, Show)

-- | Reconstruct a nominal session claim without allocating a session.
applicationSessionIdFromClaimParts ::
  ByteString ->
  Word64 ->
  Either ApplicationClaimShapeError ApplicationSessionId
applicationSessionIdFromClaimParts scope ordinal = do
  herald <-
    either
      (const (Left ApplicationClaimWrongIdentityByteCount))
      Right
      (mkHeraldEpoch scope)
  Right (ApplicationSessionId herald ordinal)

applicationSessionIdHeraldEpoch :: ApplicationSessionId -> HeraldEpoch
applicationSessionIdHeraldEpoch (ApplicationSessionId herald _) = herald

applicationSessionIdOrdinal :: ApplicationSessionId -> Word64
applicationSessionIdOrdinal (ApplicationSessionId _ ordinal) = ordinal

-- | Opaque benign-profile resume correlation.
--
-- Its nominal type is the separate domain tag; its deterministic components are
-- deliberately the same Herald epoch and allocation ordinal as the session.
data ApplicationResumeToken
  = ApplicationResumeToken HeraldEpoch Word64
  deriving stock (Eq, Ord, Show)

-- | Reconstruct a nominal resume-token claim without proving it belongs to a
-- live session.
applicationResumeTokenFromClaimParts ::
  ByteString ->
  Word64 ->
  Either ApplicationClaimShapeError ApplicationResumeToken
applicationResumeTokenFromClaimParts scope ordinal = do
  herald <-
    either
      (const (Left ApplicationClaimWrongIdentityByteCount))
      Right
      (mkHeraldEpoch scope)
  Right (ApplicationResumeToken herald ordinal)

applicationResumeTokenHeraldEpoch :: ApplicationResumeToken -> HeraldEpoch
applicationResumeTokenHeraldEpoch (ApplicationResumeToken herald _) = herald

applicationResumeTokenOrdinal :: ApplicationResumeToken -> Word64
applicationResumeTokenOrdinal (ApplicationResumeToken _ ordinal) = ordinal

-- | Session-local logical binding generation.
newtype SessionBindingGeneration = SessionBindingGeneration Word64
  deriving stock (Eq, Ord, Show)

sessionBindingGenerationWord64 :: SessionBindingGeneration -> Word64
sessionBindingGenerationWord64 (SessionBindingGeneration generation) = generation

-- | Logical application delivery binding.
data ApplicationSessionBinding
  = ApplicationSessionBinding ApplicationSessionId SessionBindingGeneration
  deriving stock (Eq, Ord, Show)

sessionBindingSessionId :: ApplicationSessionBinding -> ApplicationSessionId
sessionBindingSessionId (ApplicationSessionBinding session _) = session

sessionBindingGeneration ::
  ApplicationSessionBinding ->
  SessionBindingGeneration
sessionBindingGeneration (ApplicationSessionBinding _ generation) = generation

-- | Allocate the session ID, distinct resume-token domain, and generation-zero
-- binding from one owner ordinal.
initialSessionAllocation ::
  HeraldEpoch ->
  Word64 ->
  (ApplicationSessionId, ApplicationResumeToken, ApplicationSessionBinding)
initialSessionAllocation herald ordinal =
  (session, token, ApplicationSessionBinding session (SessionBindingGeneration 0))
  where
    session = ApplicationSessionId herald ordinal
    token = ApplicationResumeToken herald ordinal

-- | Advance one current logical binding exactly once.
--
-- Counter exhaustion is outside the finite profile-0.1 model.
advanceSessionBinding ::
  ApplicationSessionBinding ->
  ApplicationSessionBinding
advanceSessionBinding (ApplicationSessionBinding session (SessionBindingGeneration generation)) =
  ApplicationSessionBinding session (SessionBindingGeneration (generation + 1))

-- | The complete current session acknowledgement vocabulary.
data ApplicationSessionReply
  = SessionOpened
      ApplicationSessionId
      ApplicationResumeToken
      ApplicationStartupAccess
  | SessionResumed ApplicationSessionId ApplicationRequestSummary
  deriving stock (Eq, Show)

sessionReplySessionId :: ApplicationSessionReply -> ApplicationSessionId
sessionReplySessionId (SessionOpened session _ _) = session
sessionReplySessionId (SessionResumed session _) = session

-- | One indivisible logical binding, stable reply cursor, and first reply.
data ApplicationSessionAcceptance
  = ApplicationSessionAcceptance
      ApplicationSessionBinding
      ApplicationReplyCursor
      ApplicationSessionReply
  deriving stock (Eq, Show)

applicationSessionAcceptance ::
  ApplicationSessionBinding ->
  ApplicationReplyCursor ->
  ApplicationSessionReply ->
  ApplicationSessionAcceptance
applicationSessionAcceptance = ApplicationSessionAcceptance

sessionAcceptanceBinding ::
  ApplicationSessionAcceptance ->
  ApplicationSessionBinding
sessionAcceptanceBinding (ApplicationSessionAcceptance binding _ _) = binding

sessionAcceptanceCursor ::
  ApplicationSessionAcceptance ->
  ApplicationReplyCursor
sessionAcceptanceCursor (ApplicationSessionAcceptance _ cursor _) = cursor

sessionAcceptanceReply ::
  ApplicationSessionAcceptance ->
  ApplicationSessionReply
sessionAcceptanceReply (ApplicationSessionAcceptance _ _ reply) = reply

-- | Redacted ordinary application-plane session failures.
data ApplicationSessionRejection
  = ApplicationAttachmentNotAdmitted
  | ApplicationSessionNotLive
  | ApplicationResumeTokenMismatch
  | ApplicationReplyCursorNotIssued
  | ApplicationRequestSessionMismatch
  | ApplicationRequestBindingMismatch
  | ApplicationRequestBindingNotLive
  | ApplicationEndSessionBindingMismatch
  | ApplicationReceiptRetirementNotReady
  | ApplicationRequestAlreadyRetired
  deriving stock (Eq, Show)

-- | Authoritative terminal disposition for an already admitted session.
-- Handshake rejection remains a separate phase-exact protocol outcome.
data ApplicationSessionUnavailableReason
  = ApplicationSessionNoLongerLive
  | ApplicationHeraldIsolated
  | ApplicationHeraldRetired
  deriving stock (Eq, Ord, Show)
