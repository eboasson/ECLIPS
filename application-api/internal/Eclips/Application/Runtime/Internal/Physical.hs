-- | Small pure state machine for one physical EAPP binding.
module Eclips.Application.Runtime.Internal.Physical
  ( ApplicationPhysicalPhase (..),
    ApplicationEstablishmentStatus (..),
    ApplicationHeartbeatWriteStatus (..),
    ApplicationHeartbeatActivity,
    initialApplicationHeartbeatActivity,
    applicationHeartbeatReceived,
    applicationHeartbeatWritten,
    applicationHeartbeatBidirectionalProgress,
    applicationServerDispositionQueued,
    applicationPhysicalEstablished,
    applicationHandshakeTimeoutEligible,
    applicationEstablishmentStatus,
    applicationHeartbeatWriteStatus,
  )
where

import Data.Word (Word64)

-- | Inbound progress proves server liveness; completed client writes prove
-- client liveness to the passive EAPP server. Neither substitutes for the other.
data ApplicationHeartbeatActivity = ApplicationHeartbeatActivity !Word64 !Word64
  deriving stock (Eq, Show)

initialApplicationHeartbeatActivity :: ApplicationHeartbeatActivity
initialApplicationHeartbeatActivity = ApplicationHeartbeatActivity 0 0

applicationHeartbeatReceived :: ApplicationHeartbeatActivity -> ApplicationHeartbeatActivity
applicationHeartbeatReceived (ApplicationHeartbeatActivity received written) = ApplicationHeartbeatActivity (received + 1) written

applicationHeartbeatWritten :: ApplicationHeartbeatActivity -> ApplicationHeartbeatActivity
applicationHeartbeatWritten (ApplicationHeartbeatActivity received written) = ApplicationHeartbeatActivity received (written + 1)

-- | Suppress a probe only after both directions made physical progress during
-- its idle window. One-way streams must still exercise the reverse direction.
applicationHeartbeatBidirectionalProgress :: ApplicationHeartbeatActivity -> ApplicationHeartbeatActivity -> Bool
applicationHeartbeatBidirectionalProgress (ApplicationHeartbeatActivity priorReceived priorWritten) (ApplicationHeartbeatActivity received written) =
  received /= priorReceived && written /= priorWritten

-- | Physical establishment state for one EAPP binding.
--
-- A complete server disposition stops the physical handshake clock before the
-- pure owner admits that disposition. The owner must then establish, close, or
-- fail the scope; local scheduling delay cannot turn an already-observed reply
-- into a missed handshake.
data ApplicationPhysicalPhase
  = ApplicationPhysicalHandshaking
  | ApplicationPhysicalDispositionQueued
  | ApplicationPhysicalEstablished
  | ApplicationPhysicalClosed
  deriving stock (Eq, Show)

-- | One decision made by the physical handshake waiter.
data ApplicationEstablishmentStatus
  = ApplicationEstablishmentWaiting
  | ApplicationEstablishmentReady
  | ApplicationEstablishmentTimedOut
  | ApplicationEstablishmentStopped
  deriving stock (Eq, Show)

-- | Whether a queued heartbeat may begin its reply timeout.
data ApplicationHeartbeatWriteStatus
  = ApplicationHeartbeatWriteWaiting
  | ApplicationHeartbeatWritten
  | ApplicationHeartbeatWriteStopped
  deriving stock (Eq, Show)

-- | Record that one complete non-heartbeat server disposition reached the
-- current binding's ordered owner queue.
applicationServerDispositionQueued :: ApplicationPhysicalPhase -> ApplicationPhysicalPhase
applicationServerDispositionQueued ApplicationPhysicalHandshaking =
  ApplicationPhysicalDispositionQueued
applicationServerDispositionQueued phase = phase

-- | Promote a pure-owner-admitted Open or Resume disposition.
applicationPhysicalEstablished :: ApplicationPhysicalPhase -> ApplicationPhysicalPhase
applicationPhysicalEstablished ApplicationPhysicalClosed = ApplicationPhysicalClosed
applicationPhysicalEstablished _ = ApplicationPhysicalEstablished

-- | A handshake timeout may win only while no complete disposition has been
-- queued. The runtime combines this predicate with its current-connection
-- check and loss enqueue in one STM transaction.
applicationHandshakeTimeoutEligible :: ApplicationPhysicalPhase -> Bool
applicationHandshakeTimeoutEligible ApplicationPhysicalHandshaking = True
applicationHandshakeTimeoutEligible _ = False

-- | Decide whether the handshake waiter may close the binding.
applicationEstablishmentStatus ::
  ApplicationPhysicalPhase ->
  Bool ->
  ApplicationEstablishmentStatus
applicationEstablishmentStatus phase timerElapsed = case phase of
  ApplicationPhysicalHandshaking
    | timerElapsed -> ApplicationEstablishmentTimedOut
    | otherwise -> ApplicationEstablishmentWaiting
  ApplicationPhysicalDispositionQueued -> ApplicationEstablishmentWaiting
  ApplicationPhysicalEstablished -> ApplicationEstablishmentReady
  ApplicationPhysicalClosed -> ApplicationEstablishmentStopped

-- | A heartbeat reply interval begins only after the sole socket writer has
-- completed the Ping. Queue admission is deliberately not a wire observation.
applicationHeartbeatWriteStatus ::
  ApplicationPhysicalPhase ->
  Bool ->
  ApplicationHeartbeatWriteStatus
applicationHeartbeatWriteStatus ApplicationPhysicalClosed _ =
  ApplicationHeartbeatWriteStopped
applicationHeartbeatWriteStatus _ True = ApplicationHeartbeatWritten
applicationHeartbeatWriteStatus _ False = ApplicationHeartbeatWriteWaiting
