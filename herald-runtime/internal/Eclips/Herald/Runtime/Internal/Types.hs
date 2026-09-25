{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Package-private representations for the safe runtime facade.
module Eclips.Herald.Runtime.Internal.Types
  ( ApplicationPlane,
    AdministrationPlane,
    ConfiguredAdministrationPlane,
    PeerPlane,
    RuntimeToken (..),
    ConnectionRef (..),
    connectionRefToken,
    connectionRefOrdinal,
    HeraldRuntime (..),
    HeraldRuntimeConfiguration (..),
    HeraldRuntimeHandlers (..),
    HeraldRuntimeFailure (..),
    HeraldRuntimeFailureClass (..),
    HeraldRuntimeExit (..),
    RuntimeGeneratorSeedSource (..),
    RuntimeMonotonicClock (..),
    RuntimePeerWorkDelay (..),
    RuntimeSubmission (..),
    RuntimeRegistration (..),
    RuntimeAdministrationRegistration (..),
    RuntimeLaneOffer (..),
    RuntimeApplicationConnectionHandlers (..),
    RuntimeAdministrationConnectionHandlers (..),
    RuntimeConfiguredAdministrationConnectionHandlers (..),
    RuntimePeerConnectionHandlers (..),
    RuntimeDiagnostic (..),
    RuntimeDiagnosticClass (..),
  )
where

import Control.Concurrent.STM (STM)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Set (Set)
import Data.Unique (Unique)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Administration
  ( AdminCorrelationId,
    FinalAdminReply,
  )
import Eclips.Herald.Administration.RPC (AdministrationOutbound)
import Eclips.Herald.Application.RPC (ApplicationOutbound)
import Eclips.Herald.Application.Recovery (ApplicationRecoveryConfiguration)
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks)
import Eclips.Herald.Discovery
  ( PeerAddress,
    PeerBinding,
    PeerCandidate,
    PeerDialIntent,
    PeerHello,
    PeerHelloDisposition,
  )
import Eclips.Herald.EffectBatch (PeerProtocolDisposition)
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Input (PeerControl)
import Eclips.Herald.Isolation (IsolationConfiguration)
import Eclips.Herald.OracleClient
  ( OracleClientAction,
    OracleClientIngress,
    OracleContact,
    OracleContactSet,
    OracleHelloClaims,
  )
import Eclips.Herald.OracleHealth (OracleHealthObservation)
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome,
    PeerLogicalAttempt,
    PeerLogicalItem,
  )
import Eclips.Herald.PeerLiveness (PeerRecoveryConfiguration)
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Oracle.Voter (OracleReplicaRegistration, VoterConfiguration)
import Eclips.Protocol.Admin.Types (AdminClientDto)
import Eclips.Protocol.Application.Types (ApplicationClientDto)
import Eclips.Protocol.Peer.Types (PeerEnvelope)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import Eclips.Public.Types.Timing (TakeoverTarget)
import Eclips.Raft.Identity (RaftNodeId)

-- | Phantom witness for an application-plane physical lease.
data ApplicationPlane

-- | Phantom witness for the sole administration-plane physical lease.
data AdministrationPlane

-- | Phantom witness for a reconnectable configured-process administration lease.
data ConfiguredAdministrationPlane

-- | Phantom witness for a peer-plane physical lease.
data PeerPlane

-- | Opaque identity of one scoped runtime instance.
newtype RuntimeToken = RuntimeToken Unique
  deriving stock (Eq, Ord)

-- | An opaque, generation-qualified physical lease minted by exactly one
-- runtime. The nominal @plane@ role prevents a lease for one protocol plane from
-- being reused for another.
data ConnectionRef (plane :: Type) = ConnectionRef RuntimeToken Word64
  deriving stock (Eq, Ord)

type role ConnectionRef nominal

instance Show (ConnectionRef plane) where
  show _ = "ConnectionRef"

connectionRefToken :: ConnectionRef plane -> RuntimeToken
connectionRefToken (ConnectionRef token _) = token

connectionRefOrdinal :: ConnectionRef plane -> Word64
connectionRefOrdinal (ConnectionRef _ ordinal) = ordinal

-- | Stable, redacted classification of terminal runtime failures.
data HeraldRuntimeFailureClass
  = -- | Checked runtime initialization failed before a usable handle existed.
    HeraldRuntimeInitializationFailure
  | -- | The pure Herald transition reported an invariant contradiction.
    HeraldRuntimeKernelInvariantFailure
  | -- | A supervised worker required for correct runtime operation failed.
    HeraldRuntimeEssentialWorkerFailure
  | -- | The shell's ownership or coordination machinery failed.
    HeraldRuntimeCoordinationFailure
  deriving stock (Eq, Show)

-- | Public failures expose a stable class, not worker exceptions or payloads.
data HeraldRuntimeFailure = HeraldRuntimeFailure HeraldRuntimeFailureClass
  deriving stock (Eq)

instance Show HeraldRuntimeFailure where
  show (HeraldRuntimeFailure failureClass) = show failureClass

-- | Successful terminal reason for a runtime scope.
data HeraldRuntimeExit
  = -- | Semantic drain completed; any current final administration offer was settled before close.
    HeraldRuntimeDrained
  | -- | The enclosing scope performed a non-semantic physical shutdown.
    HeraldRuntimeScopeClosed
  deriving stock (Eq, Show)

-- | Opaque monotonic observation capability shared serially by the pure owner
-- and logical timer manager.
newtype RuntimeMonotonicClock = RuntimeMonotonicClock (IO MonotonicInstant)

-- | Opaque capability that supplies the exact bytes for one generator key.
newtype RuntimeGeneratorSeedSource = RuntimeGeneratorSeedSource (IO ByteString)

-- | Opaque physical peer-work delay, expressed in microseconds.
newtype RuntimePeerWorkDelay = RuntimePeerWorkDelay {runtimePeerWorkDelayMicros :: Word64}

-- | Redacted class of a non-essential runtime diagnostic.
data RuntimeDiagnosticClass
  = -- | A physical connection was registered, promoted, lost, or closed.
    RuntimeConnectionDiagnostic
  | -- | A stale generation, callback, or result was suppressed.
    RuntimeSuppressionDiagnostic
  | -- | Orderly drain advanced through a lifecycle boundary.
    RuntimeDrainDiagnostic
  deriving stock (Eq, Show)

-- | Redacted notification. Exact correlations remain in the private trace.
newtype RuntimeDiagnostic = RuntimeDiagnostic RuntimeDiagnosticClass
  deriving stock (Eq)

instance Show RuntimeDiagnostic where
  show (RuntimeDiagnostic diagnosticClass) = show diagnosticClass

-- | Opaque process-wide runtime handlers.
newtype HeraldRuntimeHandlers
  = HeraldRuntimeHandlers (RuntimeDiagnostic -> IO ())

-- | Opaque startup configuration for one Herald runtime.
data HeraldRuntimeConfiguration
  = HeraldRuntimeConfiguration
      CheckedHeraldGenesis
      CheckedInitialBootstraps
      OracleContactSet
      RuntimeGeneratorSeedSource
      RuntimeMonotonicClock
      RuntimePeerWorkDelay
      ApplicationRecoveryConfiguration
      PeerRecoveryConfiguration
      (Maybe (Set HeraldEpoch, IsolationConfiguration))
      HeraldRuntimeHandlers
      (Maybe (PeerDialIntent -> IO ()))
      (Maybe (PeerDialIntent -> IO ()))
      (Maybe (OracleClientAction -> IO ()))
      (Maybe (VoterConfiguration, [OracleReplicaRegistration]))
      (Maybe (Word64 -> Word64 -> OracleHelloClaims -> [OracleContact] -> IO ()))
      (Maybe TakeoverTarget)
      (Maybe (Bool -> [(String, Word64)] -> IO ()))
      DiagnosticChecks

-- | Physical pre-admission result of a runtime ingress operation.
--
-- No constructor represents semantic acceptance by the pure Herald kernel.
data RuntimeSubmission
  = -- | The immutable item entered its source FIFO for later owner selection.
    Queued
  | -- | Close was accepted for the exact current lease; an owned final offer may complete before invalidation.
    ConnectionClosed
  | -- | One earlier administration request already owns the semantic drain.
    DrainRequestAlreadyPending
  | -- | The lease is foreign, superseded, or already closed.
    StalePhysicalGeneration
  | -- | The runtime has installed its drain cut and rejects new ingress.
    IngressClosedForDrain
  | -- | The runtime has already reached a terminal state.
    RuntimeStopped
  deriving stock (Eq, Show)

-- | Result of registering an application- or peer-plane connection.
data RuntimeRegistration plane
  = -- | A fresh physical lease with its handlers installed.
    ConnectionRegistered (ConnectionRef plane)
  | -- | Registration was attempted after the runtime stopped accepting work.
    RegistrationStopped
  deriving stock (Eq, Show)

-- | Result of registering the runtime's one administration connection.
data RuntimeAdministrationRegistration
  = -- | The sole administration lease was registered.
    AdministrationConnectionRegistered (ConnectionRef AdministrationPlane)
  | -- | This runtime has already consumed its one administration registration.
    AdministrationRegistrationUnavailable
  | -- | Registration was attempted after the runtime stopped accepting work.
    AdministrationRegistrationStopped
  deriving stock (Eq, Show)

-- | Physical result of offering a typed value to a non-publication writer.
data RuntimeLaneOffer
  = -- | The handler accepted the offer and the physical lane remains usable.
    LaneOffered
  | -- | The offer failed because the physical lane is no longer usable.
    LaneLost
  deriving stock (Eq, Show)

-- | Opaque callbacks installed for one application-plane lease.
data RuntimeApplicationConnectionHandlers
  = RuntimeApplicationConnectionHandlers
      (Maybe HeraldLocator)
      (ApplicationOutbound -> IO RuntimeLaneOffer)
      (IO ())

-- | Opaque callbacks installed for the administration-plane lease.
data RuntimeAdministrationConnectionHandlers
  = RuntimeAdministrationConnectionHandlers
      (Maybe FinalAdminReply -> IO RuntimeLaneOffer)
      (IO ())

-- | Opaque callbacks installed for one configured administration lease.
data RuntimeConfiguredAdministrationConnectionHandlers
  = RuntimeConfiguredAdministrationConnectionHandlers
      (AdministrationOutbound -> IO RuntimeLaneOffer)
      (IO ())

-- | Opaque callbacks installed for one peer-plane lease.
data RuntimePeerConnectionHandlers
  = RuntimePeerConnectionHandlers
      (PeerCandidate -> PeerEnvelope -> IO RuntimeLaneOffer)
      (PeerCandidate -> PeerHelloDisposition -> IO RuntimeLaneOffer)
      (PeerBinding -> PeerEnvelope -> IO RuntimeLaneOffer)
      (PeerBinding -> PeerProtocolDisposition -> IO RuntimeLaneOffer)
      (PeerBinding -> PeerLogicalAttempt -> PeerEnvelope -> IO PeerDispatchOutcome)
      (STM ())
      (IO ())

-- | Opaque capability handle for one scoped runtime. It contains no kernel or
-- STM state accessible through the public facade.
data HeraldRuntime = HeraldRuntime
  { -- Package-private mode observation; the public facade exposes no ledger.
    runtimeCapturesTraceHistory :: Bool,
    runtimeRegisterApplication :: RuntimeApplicationConnectionHandlers -> IO (RuntimeRegistration ApplicationPlane),
    runtimeRegisterAdministration :: RuntimeAdministrationConnectionHandlers -> IO RuntimeAdministrationRegistration,
    runtimeRegisterConfiguredAdministration :: RuntimeConfiguredAdministrationConnectionHandlers -> IO (RuntimeRegistration ConfiguredAdministrationPlane),
    runtimeRegisterPeer :: RuntimePeerConnectionHandlers -> IO (RuntimeRegistration PeerPlane),
    runtimeCloseConnection :: forall (plane :: Type). ConnectionRef plane -> IO RuntimeSubmission,
    runtimeSubmitApplication :: ConnectionRef ApplicationPlane -> ApplicationClientDto -> IO RuntimeSubmission,
    runtimeSubmitConfiguredAdministration :: ConnectionRef ConfiguredAdministrationPlane -> AdminClientDto -> IO RuntimeSubmission,
    runtimeOpenPeer :: ConnectionRef PeerPlane -> Set PeerAddress -> Maybe HeraldLocator -> IO RuntimeSubmission,
    runtimeSubmitPeerHello :: ConnectionRef PeerPlane -> PeerCandidate -> Set PeerAddress -> PeerHello -> HeraldMembershipGenerationId -> MemberSetDigest -> Maybe HeraldLocator -> IO RuntimeSubmission,
    runtimeSubmitPeerControlWithProgress :: ConnectionRef PeerPlane -> PeerBinding -> ReceiptRetirement -> PeerControl -> IO RuntimeSubmission,
    runtimeSubmitPeerPublicationWithProgress :: ConnectionRef PeerPlane -> PeerBinding -> ReceiptRetirement -> PeerLogicalItem -> IO RuntimeSubmission,
    runtimeSubmitPeerControl :: ConnectionRef PeerPlane -> PeerBinding -> PeerControl -> IO RuntimeSubmission,
    runtimeSubmitPeerPublication :: ConnectionRef PeerPlane -> PeerBinding -> PeerLogicalItem -> IO RuntimeSubmission,
    runtimeSubmitOracle :: OracleClientIngress -> IO RuntimeSubmission,
    runtimeSubmitOracleHealthRound :: Word64 -> [OracleHealthObservation] -> IO RuntimeSubmission,
    runtimeExchangeJoin :: ByteString -> IO (Either RuntimeSubmission ByteString),
    runtimeAwaitOracleReplicaRegistration :: RaftNodeId -> IO (Either RuntimeSubmission OracleReplicaRegistration),
    runtimeSubmitDisappearanceAbort :: DisappearanceProbeId -> IO RuntimeSubmission,
    runtimeReportRetiredEpochRejection :: HeraldEpoch -> HeraldMembershipGenerationId -> IO RuntimeSubmission,
    runtimeRequestDrain :: ConnectionRef AdministrationPlane -> AdminCorrelationId -> IO RuntimeSubmission,
    runtimeAwaitExit :: IO (Either HeraldRuntimeFailure HeraldRuntimeExit),
    runtimeTryExit :: IO (Maybe (Either HeraldRuntimeFailure HeraldRuntimeExit)),
    runtimeCloseScope :: IO ()
  }
