-- | Scoped concurrent ownership of one pure Herald kernel.
module Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    RuntimeMonotonicClock,
    RuntimePeerWorkDelay,
    ApplicationRecoveryConfiguration,
    ApplicationRecoveryConfigurationError (..),
    checkApplicationRecoveryConfiguration,
    applicationRecoveryGraceMicroseconds,
    PeerRecoveryConfiguration,
    PeerRecoveryConfigurationError (..),
    checkPeerRecoveryConfiguration,
    peerRecoveryGraceMicroseconds,
    systemRuntimeMonotonicClock,
    runtimeGeneratorSeedSource,
    systemRuntimeGeneratorSeedSource,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
    heraldRuntimeConfiguration,
    DiagnosticChecks (..),
    configureHeraldRuntimeDiagnosticChecks,
    heraldRuntimeDiagnosticChecks,
    configureHeraldRuntimeTiming,
    heraldRuntimeTakeoverTarget,
    configureHeraldRuntimeIsolation,
    configureHeraldRuntimeOracleVoters,
    configureHeraldRuntimePeerDialSink,
    configureHeraldRuntimePeerDialCancellationSink,
    configureHeraldRuntimeOracleActionSink,
    configureHeraldRuntimeOracleHealthSink,
    configureHeraldRuntimeWorkCountsSink,
    withHeraldRuntime,
    exchangeHeraldJoin,
    HeraldOracleStatus (..),
    HeraldOracleQueryProblem (..),
    readHeraldOracleStatus,
    awaitHeraldOracleReplicaRegistration,
    readHeraldVoterChange,
    requestHeraldRuntimeDrain,
    reportHeraldRuntimeRetiredEpochRejection,
    awaitHeraldRuntimeExit,
  )
where

import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Herald.Administration (AdminCorrelationId)
import Eclips.Herald.Application.Recovery
  ( ApplicationRecoveryConfiguration,
    ApplicationRecoveryConfigurationError (..),
    applicationRecoveryGraceMicroseconds,
    checkApplicationRecoveryConfiguration,
  )
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.Discovery (PeerDialIntent)
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Isolation (IsolationConfiguration, checkIsolationConfiguration)
import Eclips.Herald.Join
  ( HeraldOracleStatus (..),
    JoinReply (..),
    JoinRequest (..),
    decodeJoinReply,
    encodeJoinRequest,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction,
    OracleContact,
    OracleContactSet,
    OracleHelloClaims,
  )
import Eclips.Herald.PeerLiveness
  ( PeerRecoveryConfiguration,
    PeerRecoveryConfigurationError (..),
    checkPeerRecoveryConfiguration,
    peerRecoveryGraceMicroseconds,
  )
import Eclips.Herald.Runtime.Internal.GeneratorSeed
  ( runtimeGeneratorSeedSource,
    systemRuntimeGeneratorSeedSource,
  )
import Eclips.Herald.Runtime.Internal.Owner (withHeraldRuntimeWithoutTraceHistory)
import Eclips.Herald.Runtime.Internal.Types
  ( AdministrationPlane,
    ConnectionRef,
    HeraldRuntime (..),
    HeraldRuntimeConfiguration (..),
    HeraldRuntimeExit (..),
    HeraldRuntimeFailure,
    HeraldRuntimeHandlers,
    RuntimeGeneratorSeedSource,
    RuntimeMonotonicClock (..),
    RuntimePeerWorkDelay (..),
    RuntimeSubmission,
  )
import Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstant,
  )
import Eclips.Oracle.Voter (OracleReplicaRegistration, VoterChange, VoterChangeId, VoterConfiguration)
import Eclips.Public.Types.Timing
import Eclips.Raft.Identity (RaftNodeId)
import GHC.Clock (getMonotonicTimeNSec)

-- | Use the host monotonic clock for runtime observations, normalized from the
-- host's nanoseconds to the kernel's monotonic microseconds. The runtime
-- serializes samples shared by the kernel owner and absolute-timer manager;
-- physical peer-work delays use the separate 'RuntimePeerWorkDelay'
-- configuration.
systemRuntimeMonotonicClock :: RuntimeMonotonicClock
systemRuntimeMonotonicClock =
  RuntimeMonotonicClock
    (monotonicInstant . (`div` 1_000) <$> getMonotonicTimeNSec)

-- | Supply a monotonic observation action. This is primarily useful for a
-- controlled embedding or test. The runtime serializes calls from its owner and
-- timer manager; observations must not move backwards during one finite runtime.
runtimeMonotonicClock :: IO MonotonicInstant -> RuntimeMonotonicClock
runtimeMonotonicClock = RuntimeMonotonicClock

-- | Configure the physical wait before an eligible peer work ticket is released.
-- Zero disables the wait. This delay is neither a logical timer nor a kernel
-- result.
runtimePeerWorkDelayMicroseconds :: Word64 -> RuntimePeerWorkDelay
runtimePeerWorkDelayMicroseconds = RuntimePeerWorkDelay

-- | Construct the opaque configuration for one runtime. Startup acquires and
-- checks the generator seed before allocating runtime state or publishing a
-- handle; genesis and initial bootstraps are already checked inputs.
heraldRuntimeConfiguration ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  RuntimeGeneratorSeedSource ->
  RuntimeMonotonicClock ->
  RuntimePeerWorkDelay ->
  HeraldRuntimeHandlers ->
  ApplicationRecoveryConfiguration ->
  PeerRecoveryConfiguration ->
  HeraldRuntimeConfiguration
heraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay handlers applicationRecovery peerRecovery =
  HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery Nothing handlers Nothing Nothing Nothing Nothing Nothing Nothing Nothing DiagnosticChecksEnabled

-- | Select redundant internal audits without changing external admission.
-- Checked configurations are the default. Set before starting the runtime.
configureHeraldRuntimeDiagnosticChecks :: DiagnosticChecks -> HeraldRuntimeConfiguration -> HeraldRuntimeConfiguration
configureHeraldRuntimeDiagnosticChecks checks (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink oracleSink oracleVoters healthSink timing workCountsSink _) =
  HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink oracleSink oracleVoters healthSink timing workCountsSink checks

heraldRuntimeDiagnosticChecks :: HeraldRuntimeConfiguration -> DiagnosticChecks
heraldRuntimeDiagnosticChecks (HeraldRuntimeConfiguration _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ checks) = checks

-- | Select one immutable deployment policy. Pure initialization derives the
-- complete failure policy, including independent probe and health durations.
-- This setter also keeps the recorded application and peer configurations exact.
-- A subsequent configureHeraldRuntimeIsolation can override grace and drain.
configureHeraldRuntimeTiming :: TakeoverTarget -> HeraldRuntimeConfiguration -> HeraldRuntimeConfiguration
configureHeraldRuntimeTiming target (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay _ _ isolation handlers peerSink cancelSink oracleSink oracleVoters healthSink _ workCountsSink checks) =
  HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery configuredIsolation handlers peerSink cancelSink oracleSink oracleVoters healthSink (Just target) workCountsSink checks
  where
    policy = deriveTimingPolicy target
    configuredIsolation = fmap (\(hosts, _) -> (hosts, derivedIsolation)) isolation
    derivedIsolation = case checkIsolationConfiguration (timingIsolationGraceMicroseconds policy) defaultReadOnlyDrainMicroseconds of
      Right checked -> checked
      Left _ -> error "checked timing policy produced an invalid isolation configuration"
    applicationRecovery = case checkApplicationRecoveryConfiguration (timingApplicationRecoveryGraceMicroseconds policy) of
      Right checked -> checked
      Left _ -> error "checked timing policy produced an invalid application grace"
    peerRecovery = case checkPeerRecoveryConfiguration (timingPeerRecoveryGraceMicroseconds policy) of
      Right checked -> checked
      Left _ -> error "checked timing policy produced an invalid peer grace"

-- | The target supplied at startup, when this runtime uses derived timing.
heraldRuntimeTakeoverTarget :: HeraldRuntimeConfiguration -> Maybe TakeoverTarget
heraldRuntimeTakeoverTarget (HeraldRuntimeConfiguration _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ timing _ _) = timing

-- | Install immutable Raft voter-host placement facts and the checked local
-- isolation policy. Pure startup rejects an empty set or any epoch outside the
-- checked genesis membership before the runtime becomes visible.
configureHeraldRuntimeIsolation ::
  Set HeraldEpoch ->
  IsolationConfiguration ->
  HeraldRuntimeConfiguration ->
  HeraldRuntimeConfiguration
configureHeraldRuntimeIsolation
  voterHosts
  isolation
  (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery _ handlers peerSink cancelSink oracleSink oracleVoters healthSink timing workCountsSink checks) =
    HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery (Just (voterHosts, isolation)) handlers peerSink cancelSink oracleSink oracleVoters healthSink timing workCountsSink checks

-- | Supply index-zero voter configuration and replica registration from checked
-- immutable Oracle genesis. These select initial voter roles and routing before
-- the sole kernel owner starts. Existing isolation durations and runtime sinks
-- are preserved; subsequent role changes arrive through the committed watch.
configureHeraldRuntimeOracleVoters ::
  VoterConfiguration ->
  [OracleReplicaRegistration] ->
  HeraldRuntimeConfiguration ->
  HeraldRuntimeConfiguration
configureHeraldRuntimeOracleVoters
  configuration
  registrations
  (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink oracleSink _ healthSink timing workCountsSink checks) =
    HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink oracleSink (Just (configuration, registrations)) healthSink timing workCountsSink checks

-- | Immutably install the sole interpreter for Discovery-owned direct-dial
-- intentions before starting the runtime.  The runtime delivers each value on
-- its own supervised FIFO worker; this callback must not feed a dial result
-- back into the pure kernel.
configureHeraldRuntimePeerDialSink ::
  (PeerDialIntent -> IO ()) ->
  HeraldRuntimeConfiguration ->
  HeraldRuntimeConfiguration
configureHeraldRuntimePeerDialSink
  sink
  (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers _ cancelSink oracleSink oracleVoters healthSink timing workCountsSink checks) =
    HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers (Just sink) cancelSink oracleSink oracleVoters healthSink timing workCountsSink checks

configureHeraldRuntimePeerDialCancellationSink ::
  (PeerDialIntent -> IO ()) ->
  HeraldRuntimeConfiguration ->
  HeraldRuntimeConfiguration
configureHeraldRuntimePeerDialCancellationSink
  sink
  (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink _ oracleSink oracleVoters healthSink timing workCountsSink checks) =
    HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink (Just sink) oracleSink oracleVoters healthSink timing workCountsSink checks

-- | Immutably install the outbound Oracle action interpreter before startup.
-- The TCP component uses this narrow callback to enqueue watch and configured-
-- request physical work; a byte-free runtime may leave it absent while retaining
-- the pure client's level-triggered intentions.
configureHeraldRuntimeOracleActionSink ::
  (OracleClientAction -> IO ()) ->
  HeraldRuntimeConfiguration ->
  HeraldRuntimeConfiguration
configureHeraldRuntimeOracleActionSink
  sink
  (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink _ oracleVoters healthSink timing workCountsSink checks) =
    HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink (Just sink) oracleVoters healthSink timing workCountsSink checks

-- | Install the nonblocking interpreter handoff for owner-minted health rounds.
-- The cadence and complete contact snapshot come from the pure owner.
configureHeraldRuntimeOracleHealthSink ::
  (Word64 -> Word64 -> OracleHelloClaims -> [OracleContact] -> IO ()) ->
  HeraldRuntimeConfiguration ->
  HeraldRuntimeConfiguration
configureHeraldRuntimeOracleHealthSink
  sink
  (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink oracleSink oracleVoters _ timing workCountsSink checks) =
    HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink oracleSink oracleVoters (Just sink) timing workCountsSink checks

-- | Opt in to a finite constructor census without retaining trace payloads.
-- The sink runs once after worker joins, before public runtime completion. Its
-- flag reports whether all tracked workers joined. Filesystem and environment
-- interpretation belong to the embedding shell; the sink handles output errors.
configureHeraldRuntimeWorkCountsSink ::
  (Bool -> [(String, Word64)] -> IO ()) ->
  HeraldRuntimeConfiguration ->
  HeraldRuntimeConfiguration
configureHeraldRuntimeWorkCountsSink
  sink
  (HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink oracleSink oracleVoters healthSink timing _ checks) =
    HeraldRuntimeConfiguration genesis bootstraps contacts seedSource clock delay applicationRecovery peerRecovery isolation handlers peerSink cancelSink oracleSink oracleVoters healthSink timing (Just sink) checks

-- | Run one Herald runtime within a structured scope.
--
-- If the callback returns while the runtime is still running, the scope performs
-- a physical shutdown, joins every child, and reports 'HeraldRuntimeScopeClosed'.
-- For orderly semantic shutdown, request a drain and await
-- 'HeraldRuntimeDrained' before returning from the callback. A runtime failure
-- cancels the callback and is returned as 'Left'; a callback exception shuts down
-- and joins the runtime, then is rethrown.
withHeraldRuntime ::
  HeraldRuntimeConfiguration ->
  (HeraldRuntime -> IO result) ->
  IO (Either HeraldRuntimeFailure (result, HeraldRuntimeExit))
withHeraldRuntime = withHeraldRuntimeWithoutTraceHistory

-- | Request the single orderly drain through the current administration lease.
-- The returned 'RuntimeSubmission' reports physical pre-admission only; @Queued@
-- does not itself mean that the kernel has selected the request.
requestHeraldRuntimeDrain ::
  HeraldRuntime ->
  ConnectionRef AdministrationPlane ->
  AdminCorrelationId ->
  IO RuntimeSubmission
requestHeraldRuntimeDrain = runtimeRequestDrain

-- | Wait for the retained terminal result. This operation is repeatable and
-- returns the same exit or failure to every waiter.
awaitHeraldRuntimeExit ::
  HeraldRuntime ->
  IO (Either HeraldRuntimeFailure HeraldRuntimeExit)
awaitHeraldRuntimeExit = runtimeAwaitExit

-- | Report a checked current-generation rejection issued by a configured
-- voter-host Herald. The pure isolation owner validates both facts before the
-- irreversible fence can begin.
reportHeraldRuntimeRetiredEpochRejection ::
  HeraldRuntime ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  IO RuntimeSubmission
reportHeraldRuntimeRetiredEpochRejection = runtimeReportRetiredEpochRejection

-- | Route discovery/onboarding through the same serialized pure owner as all
-- other ingress. No connection or active membership authority is implied.
exchangeHeraldJoin :: HeraldRuntime -> ByteString -> IO (Either RuntimeSubmission ByteString)
exchangeHeraldJoin = runtimeExchangeJoin

-- | Distinguish physical admission failure from a selected owner's refusal or
-- a reply that contradicts the requested public query.
data HeraldOracleQueryProblem
  = HeraldOracleQueryNotSubmitted RuntimeSubmission
  | HeraldOracleQueryRejected
  | HeraldOracleQueryInvalidReply
  deriving stock (Eq, Show)

-- | Select one current projection on the Herald owner's ordinary serialized
-- queue. This reports its applied prefix, not a new distributed quorum read.
readHeraldOracleStatus ::
  HeraldRuntime -> IO (Either HeraldOracleQueryProblem HeraldOracleStatus)
readHeraldOracleStatus runtime =
  exchangeOracleQuery runtime ReadOracleStatus $ \case
    OracleStatusReply status -> Just status
    _ -> Nothing

-- | Wait without polling for an owner-applied registration or immutable
-- genesis registration. Only the small current registration projection is
-- shared; kernel state remains private. Scope exit also releases the wait.
awaitHeraldOracleReplicaRegistration ::
  HeraldRuntime -> RaftNodeId -> IO (Either HeraldOracleQueryProblem OracleReplicaRegistration)
awaitHeraldOracleReplicaRegistration runtime node =
  either (Left . HeraldOracleQueryNotSubmitted) Right <$> runtimeAwaitOracleReplicaRegistration runtime node

-- | Look up one retained change identity without scanning watch history. An
-- absent result means this Herald has not applied a record for that identity.
readHeraldVoterChange ::
  HeraldRuntime ->
  VoterChangeId ->
  IO (Either HeraldOracleQueryProblem (Maybe VoterChange))
readHeraldVoterChange runtime identifier =
  exchangeOracleQuery runtime (ReadOracleVoterChange identifier) $ \case
    OracleVoterChangeReply change -> Just change
    _ -> Nothing

exchangeOracleQuery ::
  HeraldRuntime ->
  JoinRequest ->
  (JoinReply -> Maybe result) ->
  IO (Either HeraldOracleQueryProblem result)
exchangeOracleQuery runtime request select = do
  exchanged <- exchangeHeraldJoin runtime (encodeJoinRequest request)
  pure $ case exchanged of
    Left submission -> Left (HeraldOracleQueryNotSubmitted submission)
    Right bytes -> case decodeJoinReply bytes of
      Left _ -> Left HeraldOracleQueryInvalidReply
      Right JoinRejected -> Left HeraldOracleQueryRejected
      Right reply -> maybe (Left HeraldOracleQueryInvalidReply) Right (select reply)
