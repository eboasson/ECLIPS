-- | Typed registration and ingress operations.
module Eclips.Herald.Runtime.Ingress
  ( RuntimeSubmission (..),
    RuntimeRegistration (..),
    RuntimeAdministrationRegistration (..),
    registerApplicationConnection,
    registerAdministrationConnection,
    registerConfiguredAdministrationConnection,
    registerPeerConnection,
    closeRuntimeConnection,
    submitApplicationDto,
    submitConfiguredAdministrationDto,
    openPeerCandidate,
    submitPeerHello,
    submitPeerControl,
    submitPeerControlWithProgress,
    submitPeerPublication,
    submitPeerPublicationWithProgress,
    submitOracleClientIngress,
    submitOracleHealthRoundObserved,
    submitOracleContactHints,
    configureLocalOracleReplica,
    exchangeHeraldJoin,
    submitDisappearanceProbeAbort,
  )
where

import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Discovery
  ( PeerAddress,
    PeerBinding,
    PeerCandidate,
    PeerHello,
  )
import Eclips.Herald.Input (PeerControl)
import Eclips.Herald.OracleClient (OracleClientIngress (OracleContactsDiscovered, OracleLocalReplicaConfigured), OracleContactSet)
import Eclips.Herald.OracleHealth (OracleHealthObservation)
import Eclips.Herald.PeerDispatch (PeerLogicalItem)
import Eclips.Herald.Runtime.Internal.Types
  ( ApplicationPlane,
    ConfiguredAdministrationPlane,
    ConnectionRef,
    HeraldRuntime (..),
    PeerPlane,
    RuntimeAdministrationConnectionHandlers,
    RuntimeAdministrationRegistration (..),
    RuntimeApplicationConnectionHandlers,
    RuntimeConfiguredAdministrationConnectionHandlers,
    RuntimePeerConnectionHandlers,
    RuntimeRegistration (..),
    RuntimeSubmission (..),
  )
import Eclips.Oracle.Voter (OracleReplicaContact)
import Eclips.Protocol.Admin.Types (AdminClientDto)
import Eclips.Protocol.Application.Types (ApplicationClientDto)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import Eclips.Raft.Identity (RaftNodeId)

-- | Register a fresh application-plane physical lease and its handlers.
-- Registration does not semantically open an application session; submit an
-- application Open DTO through the returned lease for that transition.
registerApplicationConnection ::
  HeraldRuntime ->
  RuntimeApplicationConnectionHandlers ->
  IO (RuntimeRegistration ApplicationPlane)
registerApplicationConnection = runtimeRegisterApplication

-- | Publish one completed physical health round for the sole Herald owner.
submitOracleHealthRoundObserved :: HeraldRuntime -> Word64 -> [OracleHealthObservation] -> IO RuntimeSubmission
submitOracleHealthRoundObserved = runtimeSubmitOracleHealthRound

-- | Register the sole administration-plane lease. A runtime admits this role at
-- most once, even after the original physical lease has closed.
registerAdministrationConnection ::
  HeraldRuntime ->
  RuntimeAdministrationConnectionHandlers ->
  IO RuntimeAdministrationRegistration
registerAdministrationConnection = runtimeRegisterAdministration

-- | Register a fresh reconnectable configured-process administration lease.
-- The returned candidate accepts only an authorized 'AdminHello' before it is
-- promoted to Start/End/query ingress.
registerConfiguredAdministrationConnection ::
  HeraldRuntime ->
  RuntimeConfiguredAdministrationConnectionHandlers ->
  IO (RuntimeRegistration ConfiguredAdministrationPlane)
registerConfiguredAdministrationConnection = runtimeRegisterConfiguredAdministration

-- | Register a fresh peer-plane physical lease. The lease starts without a
-- logical peer binding; use 'openPeerCandidate' or 'submitPeerHello' to establish
-- its candidate route.
registerPeerConnection ::
  HeraldRuntime ->
  RuntimePeerConnectionHandlers ->
  IO (RuntimeRegistration PeerPlane)
registerPeerConnection = runtimeRegisterPeer

-- | Invalidate exactly this generation-qualified physical lease. Closing an
-- already stale or foreign lease cannot affect the current connection. A current
-- established lane may enqueue exactly one logical binding-loss observation.
closeRuntimeConnection ::
  HeraldRuntime ->
  ConnectionRef plane ->
  IO RuntimeSubmission
closeRuntimeConnection runtime = case runtime of
  HeraldRuntime {runtimeCloseConnection = closeConnection} -> closeConnection

-- | Queue a checked application DTO on this application source. 'Queued' means
-- FIFO handoff only; protocol admission occurs when the owner selects the item.
submitApplicationDto ::
  HeraldRuntime ->
  ConnectionRef ApplicationPlane ->
  ApplicationClientDto ->
  IO RuntimeSubmission
submitApplicationDto = runtimeSubmitApplication

-- | Queue one checked EADM client DTO on this physical administration lease.
-- Candidate authorization and established Start/End/query admission are selected
-- by the serialized owner.
submitConfiguredAdministrationDto ::
  HeraldRuntime ->
  ConnectionRef ConfiguredAdministrationPlane ->
  AdminClientDto ->
  IO RuntimeSubmission
submitConfiguredAdministrationDto = runtimeSubmitConfiguredAdministration

-- | Queue a local peer-candidate open for the supplied addresses. The runtime
-- mints the connection nonce; callers cannot inject a runtime-owned selection or
-- loss observation through this operation.
openPeerCandidate ::
  HeraldRuntime ->
  ConnectionRef PeerPlane ->
  Set PeerAddress ->
  Maybe HeraldLocator ->
  IO RuntimeSubmission
openPeerCandidate = runtimeOpenPeer

-- | Queue a received peer Hello and its physical candidate context. The runtime
-- checks the candidate and connection correlations before the pure owner sees the
-- resulting ingress.
submitPeerHello ::
  HeraldRuntime ->
  ConnectionRef PeerPlane ->
  PeerCandidate ->
  Set PeerAddress ->
  PeerHello ->
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  Maybe HeraldLocator ->
  IO RuntimeSubmission
submitPeerHello = runtimeSubmitPeerHello

-- | Queue peer control for the exact established binding owned by this physical
-- lease.
submitPeerControl ::
  HeraldRuntime ->
  ConnectionRef PeerPlane ->
  PeerBinding ->
  PeerControl ->
  IO RuntimeSubmission
submitPeerControl = runtimeSubmitPeerControl

-- | Queue progress and control together under one established binding check.
submitPeerControlWithProgress :: HeraldRuntime -> ConnectionRef PeerPlane -> PeerBinding -> ReceiptRetirement -> PeerControl -> IO RuntimeSubmission
submitPeerControlWithProgress = runtimeSubmitPeerControlWithProgress

-- | Queue one received peer publication item for the exact established binding.
-- Dispatch selections and physical dispatch outcomes remain runtime-owned and
-- cannot be submitted through this public seam.
submitPeerPublication ::
  HeraldRuntime ->
  ConnectionRef PeerPlane ->
  PeerBinding ->
  PeerLogicalItem ->
  IO RuntimeSubmission
submitPeerPublication = runtimeSubmitPeerPublication

-- | Queue progress and data together under one established binding check.
submitPeerPublicationWithProgress :: HeraldRuntime -> ConnectionRef PeerPlane -> PeerBinding -> ReceiptRetirement -> PeerLogicalItem -> IO RuntimeSubmission
submitPeerPublicationWithProgress = runtimeSubmitPeerPublicationWithProgress

-- | Queue one already checked EORC observation on the runtime-owned Oracle
-- source. Physical framing, attempt/binding correlation, and canonical entry
-- decoding remain responsibilities of the TCP adapter and pure owner.
submitOracleClientIngress ::
  HeraldRuntime ->
  OracleClientIngress ->
  IO RuntimeSubmission
submitOracleClientIngress = runtimeSubmitOracle

-- | Publish discovered routes to the exclusive Herald owner. This input cannot
-- change committed replica bindings or voting roles.
submitOracleContactHints :: HeraldRuntime -> OracleContactSet -> IO RuntimeSubmission
submitOracleContactHints runtime = runtimeSubmitOracle runtime . OracleContactsDiscovered

-- | Explicit authorized disappearance management. Runtime failure observers
-- never call this seam, and no application or administration wire arm maps here.
submitDisappearanceProbeAbort :: HeraldRuntime -> DisappearanceProbeId -> IO RuntimeSubmission
submitDisappearanceProbeAbort = runtimeSubmitDisappearanceAbort

-- | Submit a restricted onboarding payload to the exclusive kernel owner and
-- await its correlated reply. This does not register an ordinary peer binding.
exchangeHeraldJoin :: HeraldRuntime -> ByteString -> IO (Either RuntimeSubmission ByteString)
exchangeHeraldJoin = runtimeExchangeJoin

-- | Install runtime-owned local endpoints before operator readiness. This is a
-- contact fact; only a later committed registration can start the replica role.
configureLocalOracleReplica :: HeraldRuntime -> RaftNodeId -> OracleReplicaContact -> IO RuntimeSubmission
configureLocalOracleReplica runtime node contact = runtimeSubmitOracle runtime (OracleLocalReplicaConfigured node contact)
