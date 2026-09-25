-- | Typed, byte-free connection handlers for one Herald runtime.
module Eclips.Herald.Runtime.Handler
  ( HeraldRuntimeHandlers,
    heraldRuntimeHandlers,
    RuntimeApplicationConnectionHandlers,
    runtimeApplicationConnectionHandlers,
    runtimeApplicationConnectionHandlersAt,
    RuntimeAdministrationConnectionHandlers,
    runtimeAdministrationConnectionHandlers,
    RuntimeConfiguredAdministrationConnectionHandlers,
    runtimeConfiguredAdministrationConnectionHandlers,
    RuntimePeerConnectionHandlers,
    runtimePeerConnectionHandlers,
    runtimePeerConnectionHandlersWithRetirement,
    RuntimeLaneOffer (..),
  )
where

import Control.Concurrent.STM (STM)
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Herald.Administration (FinalAdminReply)
import Eclips.Herald.Administration.RPC (AdministrationOutbound)
import Eclips.Herald.Application.RPC (ApplicationOutbound)
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerCandidate,
    PeerHelloDisposition,
  )
import Eclips.Herald.EffectBatch (PeerProtocolDisposition)
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome,
    PeerLogicalAttempt,
  )
import Eclips.Herald.Runtime.Internal.Types
  ( HeraldRuntimeHandlers (..),
    RuntimeAdministrationConnectionHandlers (..),
    RuntimeApplicationConnectionHandlers (..),
    RuntimeConfiguredAdministrationConnectionHandlers (..),
    RuntimeDiagnostic,
    RuntimeLaneOffer (..),
    RuntimePeerConnectionHandlers (..),
  )
import Eclips.Protocol.Peer.Types (PeerEnvelope)

-- | Configure redacted, non-essential diagnostics. If this callback throws, the
-- runtime records that delivery was disabled, suppresses further diagnostics,
-- and keeps the Herald role running.
heraldRuntimeHandlers :: (RuntimeDiagnostic -> IO ()) -> HeraldRuntimeHandlers
heraldRuntimeHandlers = HeraldRuntimeHandlers

-- | Configure one application-plane writer and its close action.
--
-- The writer receives exact typed 'ApplicationOutbound' values in connection
-- order and reports whether the lane remained usable. 'LaneLost' (or a thrown
-- exception) invalidates only that current physical lease. The close action runs
-- exactly once after invalidation and is joined before runtime exit.
runtimeApplicationConnectionHandlers ::
  (ApplicationOutbound -> IO RuntimeLaneOffer) ->
  IO () ->
  RuntimeApplicationConnectionHandlers
runtimeApplicationConnectionHandlers = RuntimeApplicationConnectionHandlers Nothing

-- | Attach the actual locally bound application listener for lifecycle admission.
runtimeApplicationConnectionHandlersAt :: HeraldLocator -> (ApplicationOutbound -> IO RuntimeLaneOffer) -> IO () -> RuntimeApplicationConnectionHandlers
runtimeApplicationConnectionHandlersAt locator = RuntimeApplicationConnectionHandlers (Just locator)

-- | Configure the administration-plane writer and its close action. The writer
-- is offered the optional final drain reply before the lane is closed. Its
-- 'RuntimeLaneOffer' cannot acknowledge a kernel transition. The close action
-- runs exactly once and is joined before runtime exit. A thrown final-offer
-- callback is treated as lane loss and cannot prevent drain from terminating.
runtimeAdministrationConnectionHandlers ::
  (Maybe FinalAdminReply -> IO RuntimeLaneOffer) ->
  IO () ->
  RuntimeAdministrationConnectionHandlers
runtimeAdministrationConnectionHandlers = RuntimeAdministrationConnectionHandlers

-- | Configure one reconnectable configured-administration writer and close
-- action. Acceptance/rejection routing metadata and result DTOs are offered in
-- exact kernel order; a lost lane clears only its current logical binding.
runtimeConfiguredAdministrationConnectionHandlers ::
  (AdministrationOutbound -> IO RuntimeLaneOffer) ->
  IO () ->
  RuntimeConfiguredAdministrationConnectionHandlers
runtimeConfiguredAdministrationConnectionHandlers =
  RuntimeConfiguredAdministrationConnectionHandlers

-- | Configure the five typed peer-plane writers and the connection close action.
-- Candidate, disposition, control, and rejection writers report only whether the
-- lane remained usable. The publication writer instead returns the exact
-- 'PeerDispatchOutcome' for its handed attempt. Callbacks are serialized for one
-- physical lease; results from stale callbacks are suppressed and the close
-- action runs exactly once after invalidation. A thrown non-publication callback
-- loses the lane. A thrown publication callback also loses the lane, and the
-- close path claims @PeerDispatchDeferred@ exactly when that attempt's outcome
-- cell is still empty.
runtimePeerConnectionHandlers ::
  (PeerCandidate -> PeerEnvelope -> IO RuntimeLaneOffer) ->
  (PeerCandidate -> PeerHelloDisposition -> IO RuntimeLaneOffer) ->
  (PeerBinding -> PeerEnvelope -> IO RuntimeLaneOffer) ->
  (PeerBinding -> PeerProtocolDisposition -> IO RuntimeLaneOffer) ->
  (PeerBinding -> PeerLogicalAttempt -> PeerEnvelope -> IO PeerDispatchOutcome) ->
  IO () ->
  RuntimePeerConnectionHandlers
runtimePeerConnectionHandlers candidate disposition control rejection publication close =
  RuntimePeerConnectionHandlers
    candidate
    disposition
    control
    rejection
    publication
    (pure ())
    close

-- | Configure peer handlers whose transport keeps a separate STM projection
-- of the current physical lease.  The retirement action runs in the same STM
-- transaction that removes the owner registry slot.  It must synchronously
-- make later writer callbacks stale; the eventually invoked 'IO' close action
-- remains responsible for releasing the physical resource.
runtimePeerConnectionHandlersWithRetirement ::
  (PeerCandidate -> PeerEnvelope -> IO RuntimeLaneOffer) ->
  (PeerCandidate -> PeerHelloDisposition -> IO RuntimeLaneOffer) ->
  (PeerBinding -> PeerEnvelope -> IO RuntimeLaneOffer) ->
  (PeerBinding -> PeerProtocolDisposition -> IO RuntimeLaneOffer) ->
  (PeerBinding -> PeerLogicalAttempt -> PeerEnvelope -> IO PeerDispatchOutcome) ->
  STM () ->
  IO () ->
  RuntimePeerConnectionHandlers
runtimePeerConnectionHandlersWithRetirement = RuntimePeerConnectionHandlers
