-- | Opaque application session correlations and acknowledgements.
--
-- Profile 0.1 does not treat an attachment or resume token as a credential.  The
-- owner-only constructors and representation accessors remain package-private in
-- "Eclips.Herald.Application.Session.Internal".
module Eclips.Herald.Application.Session
  ( ApplicationAttachment,
    primordialApplicationAttachment,
    ClientNonce,
    clientNonce,
    clientNonceWord64,
    ApplicationSessionId,
    ApplicationResumeToken,
    ApplicationSessionBinding,
    sessionBindingSessionId,
    ApplicationSessionReply (..),
    sessionReplySessionId,
    ApplicationSessionAcceptance,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
    ApplicationSessionRejection (..),
    ApplicationSessionUnavailableReason (..),
  )
where

import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionRejection (..),
    ApplicationSessionReply (..),
    ApplicationSessionUnavailableReason (..),
    ClientNonce,
    clientNonce,
    clientNonceWord64,
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
    sessionBindingSessionId,
    sessionReplySessionId,
  )
