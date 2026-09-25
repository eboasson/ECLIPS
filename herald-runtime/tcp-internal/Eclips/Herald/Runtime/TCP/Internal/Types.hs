{-# LANGUAGE NoFieldSelectors #-}

-- | Private socket-scope representation for the public TCP facade.
module Eclips.Herald.Runtime.TCP.Internal.Types
  ( ConfiguredSeedProbeEvent (..),
    ConfiguredPeerTarget (..),
    ConfiguredPeerSeed (..),
    HeartbeatConfiguration (..),
    HeartbeatIdleDelay (..),
    HeartbeatLane (..),
    HeartbeatProbeEvent (..),
    HeartbeatProbePhase (..),
    HeartbeatReplyTimeout (..),
    HeartbeatScheduler (..),
    HeraldTcp (..),
    HeraldTcpConfiguration (..),
    HeraldTcpConfigurationError (..),
    HeraldTcpEndpoints (..),
    HeraldTcpFailure (..),
    OracleManagerProbeEvent (..),
    OracleSubmissionProgressEvent (..),
    PeerTransportGate (..),
    PeerTransportLease (..),
    PeerDialProbeEvent (..),
    PeerDialProbePhase (..),
    PeerEnvelopeProbePhase (..),
    PeerDialRetryDelay (..),
    PeerDialScheduler (..),
    ResolvedTcpEndpoint (..),
    TcpContext (..),
    TcpEndpointError (..),
    TcpListenEndpoint (..),
    TcpSocketKey (..),
    TcpTrackedThread (..),
  )
where

import Control.Concurrent (MVar, ThreadId)
import Control.Concurrent.STM
  ( TMVar,
    TQueue,
    TVar,
  )
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Text (Text)
import Data.Unique (Unique)
import Data.Word (Word16, Word64)
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, HeraldId)
import Eclips.Herald.Discovery
  ( PeerAddress,
    PeerBinding,
    PeerCandidate,
    PeerDialIntent,
    PeerHello,
  )
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction,
    OracleConnectAttempt,
    OracleContact,
    OracleHelloClaims,
    OracleLane,
    OracleObservedTerm,
    OracleRetry,
    OracleRetryPurpose,
  )
import Eclips.Herald.PeerDispatch (PeerLogicalAttempt)
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
  )
import Eclips.Herald.Runtime.Connection
  ( AdministrationPlane,
    ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Trace (HeraldRuntimeFailure)
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Protocol.Peer.Types (PeerEnvelope)
import Eclips.Public.Types.Timing (TimingPolicy)
import Network.Socket (Socket)

-- | Numeric listener endpoint. Port zero requests an ephemeral bound port.
data TcpListenEndpoint = TcpListenEndpoint Text Word16
  deriving stock (Eq, Show)

-- | Numeric resolved endpoint reported after a listener binds.
data ResolvedTcpEndpoint = ResolvedTcpEndpoint Text Word16
  deriving stock (Eq, Show)

-- | One identity-free bootstrap endpoint used before peer Hello.
newtype ConfiguredPeerSeed = ConfiguredPeerSeed ResolvedTcpEndpoint
  deriving stock (Eq, Show)

-- | Shape errors for listener and seed endpoints.
data TcpEndpointError
  = TcpEndpointHostEmpty
  | TcpSeedPortZero
  deriving stock (Eq, Show)

-- | Checked TCP timing-configuration failures.
data HeraldTcpConfigurationError
  = PeerDialRetryDelayMustBePositive
  | HeartbeatIdleDelayMustBePositive
  | HeartbeatReplyTimeoutMustBePositive
  deriving stock (Eq, Show)

-- | Physical retry wait shared by peer and outbound Oracle jobs.
newtype PeerDialRetryDelay = PeerDialRetryDelay Word64
  deriving stock (Eq, Show)

-- | Positive quiet interval before one physical lane probes for liveness.
newtype HeartbeatIdleDelay = HeartbeatIdleDelay Word64
  deriving stock (Eq, Show)

-- | Positive interval allowed for heartbeat progress after the idle boundary.
newtype HeartbeatReplyTimeout = HeartbeatReplyTimeout Word64
  deriving stock (Eq, Show)

-- | Complete checked physical heartbeat timing policy.
data HeartbeatConfiguration
  = HeartbeatConfiguration HeartbeatIdleDelay HeartbeatReplyTimeout
  deriving stock (Eq, Show)

-- | Complete resolved listener endpoints for one running node.
data HeraldTcpEndpoints
  = HeraldTcpEndpoints
      ResolvedTcpEndpoint
      ResolvedTcpEndpoint
      ResolvedTcpEndpoint
  deriving stock (Eq, Show)

-- | Composite startup/runtime failure class.
data HeraldTcpFailure
  = HeraldTcpRuntimeFailure HeraldRuntimeFailure
  | HeraldTcpApplicationListenerFailure
  | HeraldTcpAdministrationListenerFailure
  | HeraldTcpPeerListenerFailure
  | HeraldTcpScopeFailure
  deriving stock (Eq, Show)

-- | Immutable TCP/runtime startup configuration.
data HeraldTcpConfiguration
  = HeraldTcpConfiguration
      HeraldRuntimeConfiguration
      TcpListenEndpoint
      TcpListenEndpoint
      TcpListenEndpoint
      [ConfiguredPeerSeed]
      PeerDialRetryDelay
      HeartbeatConfiguration
      HeartbeatConfiguration
      (Maybe ResolvedTcpEndpoint, Maybe ResolvedTcpEndpoint)
      (Maybe (HeraldRuntime -> ByteString -> IO (Maybe ByteString)))

-- | Physical lane classes observed by package-private heartbeat probes.
data HeartbeatLane
  = ApplicationHeartbeatLane
  | PeerHeartbeatLane
  deriving stock (Eq, Show)

-- | Bounded-work heartbeat milestones. 'Nothing' names the passive EAPP
-- server deadline; EPRP reply milestones carry their exact nonce.
data HeartbeatProbePhase
  = HeartbeatEstablishmentScheduled
  | HeartbeatEstablishmentTimedOut
  | HeartbeatIdleScheduled
  | HeartbeatPingWritten Word64
  | HeartbeatReplyScheduled (Maybe Word64)
  | HeartbeatPongReceived Word64 Bool
  | HeartbeatPongMatched Word64
  | HeartbeatTimedOut (Maybe Word64)
  deriving stock (Eq, Show)

data HeartbeatProbeEvent
  = HeartbeatProbeEvent HeartbeatLane HeartbeatProbePhase
  deriving stock (Eq, Show)

-- | One tracked TCP child and its join witness.
data TcpTrackedThread = TcpTrackedThread ThreadId (MVar ())

-- | Package-private progress points for deterministically controlling a
-- configured-seed socket claim in transport tests. Ordinary runtimes leave the
-- optional observer absent.
data ConfiguredSeedProbeEvent
  = ConfiguredSeedAwaitingAdmission
  | ConfiguredSeedDialAttempted
  | ConfiguredSeedBeforeSocketClaim
  | ConfiguredSeedSocketClaimResult Bool
  deriving stock (Eq, Show)

-- | One configured endpoint whose exact target incarnation has been admitted
-- by a successful Hello.  It enriches a later owner intent but never grants
-- dial authority by itself.
data ConfiguredPeerTarget
  = ConfiguredPeerTarget HeraldId HeraldEpoch PeerAddress
  deriving stock (Eq, Ord, Show)

-- | Package-private physical reconnect observations.  The retained pure
-- intent supplies the exact target, class, and generation; phases add no
-- timestamps or socket identities.
data PeerDialProbePhase
  = PeerDialFallbackScheduled
  | PeerDialAttempted
  | PeerDialSuppressed
  | PeerDialCancelled
  | PeerDialSatisfied
  deriving stock (Eq, Ord, Show)

data PeerDialProbeEvent
  = PeerDialProbeEvent PeerDialIntent PeerDialProbePhase
  deriving stock (Eq, Show)

-- | Monotone physical Oracle evidence which is strong enough to forgive a
-- fruitless submission round. NotReady, redirect, ordinary owner input, and an
-- accepted binding are deliberately absent from this vocabulary.
data OracleSubmissionProgressEvent
  = OracleCommittedPrefixProgress ControlIndex
  | OracleTerminalRequestProgress OracleClientRequestId
  | OracleServiceReadyLeaderProgress OracleObservedTerm
  deriving stock (Eq, Show)

-- | Package-private attribution for physical attempts, retry pacing, and useful-progress resets.
-- It contains only owner-minted semantic identities and the interpreted delay;
-- no socket identity or timestamp enters the trace.
data OracleManagerProbeEvent
  = OracleRetryPaced
      OracleRetryPurpose
      OracleRetry
      Word64
      Int
  | OracleProgressObserved OracleSubmissionProgressEvent Bool
  | OraclePhysicalConnectAttempt OracleConnectAttempt
  | OraclePhysicalSubmitAttempt OracleBinding OracleClientRequestId
  | OraclePhysicalRedirect OracleLane
  deriving stock (Eq, Show)

-- | Runtime interpretation of a bounded peer-dial wait.  Production uses an
-- STM delay token; deterministic tests inject tokens they release explicitly.
newtype PeerDialScheduler
  = PeerDialScheduler (Word64 -> IO (TVar Bool))

-- | Runtime interpretation of one heartbeat deadline. Production uses an STM
-- delay token; deterministic tests inject tokens they release explicitly.
newtype HeartbeatScheduler
  = HeartbeatScheduler (Word64 -> IO (TVar Bool))

-- | Two-phase ownership for one peer transport generation. A cut requests
-- every shutdown before it waits for any quiescence witness, so one slow
-- worker cannot delay invalidating the remaining sockets. The witness is
-- published only after the socket owner has joined its transport workers and
-- the runtime writer, then performed the sole physical descriptor close.
data PeerTransportLease
  = PeerTransportLease
      (IO ())
      (IO ())

-- | Identity of one socket ownership registration, independent of a reused
-- descriptor or a worker thread which may own several successive sockets.
newtype TcpSocketKey = TcpSocketKey Unique
  deriving stock (Eq)

-- | Peer-only physical admission cut plus the leases for every socket claimed
-- before that cut. This is package-private test control: ordinary TCP startup
-- creates it open and never changes it.
data PeerTransportGate
  = PeerTransportGate
      (TVar Bool)
      (TVar [(TcpSocketKey, PeerTransportLease)])

-- | Package-private physical envelope observation boundaries.
data PeerEnvelopeProbePhase = PeerEnvelopeReceiving | PeerEnvelopeWriting | PeerEnvelopeWritten
  deriving stock (Eq, Ord, Show)

-- | Physical coordination shared by listeners, readers, and dial managers.
data TcpContext = TcpContext
  { tcpTimingPolicy :: Maybe TimingPolicy,
    tcpDiscoveryHandler :: Maybe (HeraldRuntime -> ByteString -> IO (Maybe ByteString)),
    tcpStopping :: TVar Bool,
    tcpAccepting :: TVar Bool,
    tcpFailure :: TMVar HeraldTcpFailure,
    tcpListeners :: TVar [Socket],
    tcpSocketCloses :: TVar [(TcpSocketKey, IO ())],
    tcpThreads :: TVar [TcpTrackedThread],
    tcpPeerTransportGate :: PeerTransportGate,
    tcpConfiguredSeedProbe :: TVar (Maybe (ConfiguredSeedProbeEvent -> IO ())),
    tcpPeerPublicationProbe :: TVar (Maybe (PeerBinding -> PeerLogicalAttempt -> IO ())),
    tcpPeerEnvelopeProbe :: TVar (Maybe (PeerEnvelopeProbePhase -> PeerBinding -> PeerEnvelope -> IO ())),
    tcpConfiguredPeerAddresses :: Set PeerAddress,
    tcpApplicationLocator :: Maybe HeraldLocator,
    tcpConfiguredPeerTargets :: TVar [ConfiguredPeerTarget],
    tcpConfiguredPeerCandidates :: TVar [(ConnectionRef PeerPlane, PeerCandidate, TMVar (Maybe PeerHello))],
    tcpSatisfiedPeerDialGenerations :: TVar [(HeraldId, HeraldEpoch, Word64)],
    tcpPeerDialScheduler :: TVar PeerDialScheduler,
    tcpPeerDialProbe :: TVar (Maybe (PeerDialProbeEvent -> IO ())),
    tcpHeartbeatScheduler :: TVar HeartbeatScheduler,
    tcpHeartbeatProbe :: TVar (Maybe (HeartbeatProbeEvent -> IO ())),
    tcpDialIntents :: TQueue PeerDialIntent,
    tcpCancelledDialGenerations :: TVar [(HeraldId, HeraldEpoch, Word64)],
    tcpOracleActions :: TQueue OracleClientAction,
    tcpOracleHealthRounds :: TQueue (Word64, Word64, OracleHelloClaims, [OracleContact]),
    tcpOracleActionProbe :: TVar (Maybe (OracleClientAction -> IO ())),
    tcpOracleManagerProbe :: TVar (Maybe (OracleManagerProbeEvent -> IO ())),
    tcpCurrentPeerContacts :: TVar [(PeerHello, ConnectionRef PeerPlane)],
    tcpCurrentPeerConnections :: TVar [(PeerBinding, ConnectionRef PeerPlane)]
  }

-- | Opaque running composite handle.
data HeraldTcp
  = HeraldTcp
      HeraldRuntime
      (ConnectionRef AdministrationPlane)
      HeraldTcpEndpoints
      TcpContext
