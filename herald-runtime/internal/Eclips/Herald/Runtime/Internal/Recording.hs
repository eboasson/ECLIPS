-- | Exact successful-run snapshots, closed parity normalization, and typed
-- kernel replay.  This is package-private conformance machinery: it is neither
-- a public diagnostic format nor a second runtime interpreter.
module Eclips.Herald.Runtime.Internal.Recording
  ( RuntimeTrace,
    RuntimeTraceTerminal (..),
    RuntimeTraceFinalEvidence (..),
    runtimeTraceSnapshot,
    runtimeTraceWithTiming,
    runtimeTraceWithDiagnosticChecks,
    runtimeTraceDiagnosticChecks,
    runtimeTraceInitialObservation,
    runtimeTraceInitialOracleContacts,
    runtimeTraceInitialGeneratorSeed,
    runtimeTraceApplicationRecoveryConfiguration,
    runtimeTracePeerRecoveryConfiguration,
    runtimeTraceInitialBatch,
    runtimeTraceEvents,
    runtimeTraceTerminal,
    runtimeTraceFinalEvidence,
    NormalizedRuntimeEventOrdinal (..),
    NormalizedSourceName (..),
    NormalizedPhysicalName (..),
    NormalizedTicketName (..),
    NormalizedAttemptName (..),
    NormalizedConnectionPromotionClass (..),
    NormalizedHeraldEffectClass (..),
    NormalizedShellTraceEvent (..),
    NormalizedRuntimeTraceEvent (..),
    NormalizedRuntimeTrace (..),
    NormalizedRuntimeTraceTerminal (..),
    normalizeRuntimeTrace,
    TypedReplayMismatch (..),
    TypedReplayResult (..),
    replayRuntimeTrace,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Administration (DrainId)
import Eclips.Herald.Application.Recovery (ApplicationRecoveryConfiguration)
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.IdGenerator (GeneratorSeed)
import Eclips.Herald.Initialization
  ( HeraldInvariantFault,
    HeraldState,
    configureHeraldDiagnosticChecks,
    initialHerald,
    initialHeraldWithTimingAndIsolation,
  )
import Eclips.Herald.Input (HeraldInput)
import Eclips.Herald.Isolation (IsolationConfiguration)
import Eclips.Herald.OracleClient (OracleContactSet)
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome,
    PeerDispatchTicket,
    PeerLogicalAttempt,
  )
import Eclips.Herald.PeerLiveness (PeerRecoveryConfiguration)
import Eclips.Herald.Runtime.Internal.Arbiter
  ( SourceFamily (..),
    SourceId (..),
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( StepOrdinal (..),
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( ConnectionPromotion (..),
    KernelTraceStep (..),
    PhysicalConnectionName (..),
    PhysicalConnectionRef (..),
    PhysicalLaneClass,
    RuntimeEventOrdinal (..),
    RuntimeTraceEvent (..),
    ShellTraceEvent (..),
    TraceDriverEvent,
  )
import Eclips.Herald.Runtime.Internal.Types
  ( HeraldRuntimeExit,
    HeraldRuntimeFailure (..),
    HeraldRuntimeFailureClass,
    RuntimeLaneOffer (..),
    connectionRefOrdinal,
    connectionRefToken,
  )
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Oracle.Voter (OracleReplicaRegistration, VoterConfiguration)
import Eclips.Public.Types.Timing (TakeoverTarget)

-- | One exact in-memory conformance snapshot.  The final state is retained only
-- by an already stopped deterministic/owning conformance run; the live runtime
-- never places a 'HeraldState' in STM to make this value.
data RuntimeTrace
  = RuntimeTrace
      MonotonicInstant
      OracleContactSet
      GeneratorSeed
      ApplicationRecoveryConfiguration
      PeerRecoveryConfiguration
      EffectBatch
      [RuntimeTraceEvent]
      RuntimeTraceTerminal
      RuntimeTraceFinalEvidence
      (Maybe (TakeoverTarget, Maybe (VoterConfiguration, [OracleReplicaRegistration]), Maybe (Set HeraldEpoch, IsolationConfiguration)))
      DiagnosticChecks
  deriving stock (Eq)

instance Show RuntimeTrace where
  showsPrec precedence (RuntimeTrace observation contacts seed applicationRecovery peerRecovery batch events terminal evidence timing checks) =
    showParen (precedence > 10)
      $ showString "RuntimeTrace "
        . showsPrec 11 observation
        . showChar ' '
        . showsPrec 11 contacts
        . showChar ' '
        . showsPrec 11 seed
        . showChar ' '
        . showsPrec 11 applicationRecovery
        . showChar ' '
        . showsPrec 11 peerRecovery
        . showChar ' '
        . showsPrec 11 batch
        . showChar ' '
        . showsPrec 11 events
        . showChar ' '
        . showsPrec 11 terminal
        . showChar ' '
        . showsPrec 11 evidence
        . showChar ' '
        . showsPrec 11 timing
        . showChar ' '
        . showsPrec 11 checks

data RuntimeTraceTerminal
  = RuntimeTraceExited HeraldRuntimeExit
  | RuntimeTraceFailed HeraldRuntimeFailure
  deriving stock (Eq, Show)

-- | Exact final owner evidence for typed conformance runs.  'HeraldState' stays
-- opaque and its rendering is deliberately redacted.
data RuntimeTraceFinalEvidence
  = RuntimeTraceFinalState HeraldState
  | RuntimeTraceFinalInvariantFault HeraldInvariantFault
  deriving stock (Eq)

instance Show RuntimeTraceFinalEvidence where
  show (RuntimeTraceFinalState _) = "RuntimeTraceFinalState"
  show (RuntimeTraceFinalInvariantFault fault) =
    "RuntimeTraceFinalInvariantFault " <> show fault

runtimeTraceSnapshot ::
  MonotonicInstant ->
  OracleContactSet ->
  GeneratorSeed ->
  ApplicationRecoveryConfiguration ->
  PeerRecoveryConfiguration ->
  EffectBatch ->
  [RuntimeTraceEvent] ->
  RuntimeTraceTerminal ->
  RuntimeTraceFinalEvidence ->
  RuntimeTrace
runtimeTraceSnapshot observation contacts seed applicationRecovery peerRecovery batch events terminal evidence =
  RuntimeTrace observation contacts seed applicationRecovery peerRecovery batch events terminal evidence Nothing DiagnosticChecksEnabled

-- | Retain the pure startup options needed to replay a derived timing policy.
-- This records checked configuration, never a live owner state.
runtimeTraceWithTiming :: TakeoverTarget -> Maybe (VoterConfiguration, [OracleReplicaRegistration]) -> Maybe (Set HeraldEpoch, IsolationConfiguration) -> RuntimeTrace -> RuntimeTrace
runtimeTraceWithTiming target voters hosts (RuntimeTrace observation contacts seed applicationRecovery peerRecovery batch events terminal evidence _ checks) =
  RuntimeTrace observation contacts seed applicationRecovery peerRecovery batch events terminal evidence (Just (target, voters, hosts)) checks

-- | Retain the local audit policy so exact replay uses the same pure state.
runtimeTraceWithDiagnosticChecks :: DiagnosticChecks -> RuntimeTrace -> RuntimeTrace
runtimeTraceWithDiagnosticChecks checks (RuntimeTrace observation contacts seed applicationRecovery peerRecovery batch events terminal evidence timing _) =
  RuntimeTrace observation contacts seed applicationRecovery peerRecovery batch events terminal evidence timing checks

runtimeTraceDiagnosticChecks :: RuntimeTrace -> DiagnosticChecks
runtimeTraceDiagnosticChecks (RuntimeTrace _ _ _ _ _ _ _ _ _ _ checks) = checks

runtimeTraceTiming :: RuntimeTrace -> Maybe (TakeoverTarget, Maybe (VoterConfiguration, [OracleReplicaRegistration]), Maybe (Set HeraldEpoch, IsolationConfiguration))
runtimeTraceTiming (RuntimeTrace _ _ _ _ _ _ _ _ _ timing _) = timing

runtimeTraceInitialObservation :: RuntimeTrace -> MonotonicInstant
runtimeTraceInitialObservation (RuntimeTrace observation _ _ _ _ _ _ _ _ _ _) = observation

runtimeTraceInitialOracleContacts :: RuntimeTrace -> OracleContactSet
runtimeTraceInitialOracleContacts (RuntimeTrace _ contacts _ _ _ _ _ _ _ _ _) = contacts

runtimeTraceInitialGeneratorSeed :: RuntimeTrace -> GeneratorSeed
runtimeTraceInitialGeneratorSeed (RuntimeTrace _ _ seed _ _ _ _ _ _ _ _) = seed

runtimeTraceApplicationRecoveryConfiguration ::
  RuntimeTrace -> ApplicationRecoveryConfiguration
runtimeTraceApplicationRecoveryConfiguration
  (RuntimeTrace _ _ _ recovery _ _ _ _ _ _ _) = recovery

runtimeTracePeerRecoveryConfiguration :: RuntimeTrace -> PeerRecoveryConfiguration
runtimeTracePeerRecoveryConfiguration (RuntimeTrace _ _ _ _ recovery _ _ _ _ _ _) = recovery

runtimeTraceInitialBatch :: RuntimeTrace -> EffectBatch
runtimeTraceInitialBatch (RuntimeTrace _ _ _ _ _ batch _ _ _ _ _) = batch

runtimeTraceEvents :: RuntimeTrace -> [RuntimeTraceEvent]
runtimeTraceEvents (RuntimeTrace _ _ _ _ _ _ events _ _ _ _) = events

runtimeTraceTerminal :: RuntimeTrace -> RuntimeTraceTerminal
runtimeTraceTerminal (RuntimeTrace _ _ _ _ _ _ _ terminal _ _ _) = terminal

runtimeTraceFinalEvidence :: RuntimeTrace -> RuntimeTraceFinalEvidence
runtimeTraceFinalEvidence (RuntimeTrace _ _ _ _ _ _ _ _ evidence _ _) = evidence

newtype NormalizedRuntimeEventOrdinal = NormalizedRuntimeEventOrdinal Word64
  deriving stock (Eq, Ord, Show)

-- | The stable source class plus a deterministic first-appearance name.
data NormalizedSourceName = NormalizedSourceName SourceFamily Word64
  deriving stock (Eq, Ord, Show)

newtype NormalizedPhysicalName = NormalizedPhysicalName Word64
  deriving stock (Eq, Ord, Show)

newtype NormalizedTicketName = NormalizedTicketName Word64
  deriving stock (Eq, Ord, Show)

newtype NormalizedAttemptName = NormalizedAttemptName Word64
  deriving stock (Eq, Ord, Show)

data NormalizedConnectionPromotionClass
  = NormalizedApplicationPromotion
  | NormalizedConfiguredAdministrationPromotion
  | NormalizedPeerPromotion
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Closed routing classes.  Adding a kernel effect deliberately makes this
-- match incomplete, forcing the parity contract to be revised in lockstep.
data NormalizedHeraldEffectClass
  = NormalizedApplicationDisposition
  | NormalizedApplicationDeferral
  | NormalizedApplicationRejection
  | NormalizedInitialClaimPending
  | NormalizedInitialClaimRejection
  | NormalizedApplicationLifecycleReply
  | NormalizedApplicationReply
  | NormalizedApplicationWaitWake
  | NormalizedApplicationIsolationBegun
  | NormalizedApplicationSessionDisposal
  | NormalizedAdministrationDisposition
  | NormalizedAdministrationRejection
  | NormalizedJoinReply
  | NormalizedAdministrationReply
  | NormalizedPeerCandidate
  | NormalizedPeerCandidateRejection
  | NormalizedPeerCandidateDisposition
  | NormalizedPeerControl
  | NormalizedPeerDial
  | NormalizedPeerBindingClose
  | NormalizedPeerDialCancellation
  | NormalizedPeerDestinationCancellation
  | NormalizedPeerDispatchSchedule
  | NormalizedPeerItem
  | NormalizedPeerRejection
  | NormalizedOracleClientAction
  | NormalizedOracleHealthRound
  | NormalizedTimerArm
  | NormalizedTimerCancel
  | NormalizedDrainBegin
  | NormalizedDrainFinish
  | NormalizedIsolationFinish
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data NormalizedShellTraceEvent
  = NormalizedConnectionRegistered NormalizedSourceName
  | NormalizedConnectionPromoted NormalizedSourceName NormalizedConnectionPromotionClass
  | NormalizedConnectionClosed NormalizedSourceName
  | NormalizedStalePhysicalSuppressed NormalizedPhysicalName
  | NormalizedStaleSourceSuppressed NormalizedSourceName
  | NormalizedBatchHanded
  | NormalizedEffectRouted NormalizedHeraldEffectClass
  | NormalizedLaneOffered NormalizedSourceName PhysicalLaneClass RuntimeLaneOffer
  | NormalizedDelayedWorkEligible NormalizedTicketName
  | NormalizedDelayedWorkReleased NormalizedTicketName
  | NormalizedDelayedWorkSuppressed NormalizedTicketName
  | NormalizedPeerOutcomeWon NormalizedSourceName NormalizedAttemptName PeerDispatchOutcome
  | NormalizedPeerOutcomeSuppressed NormalizedSourceName NormalizedAttemptName PeerDispatchOutcome
  | NormalizedDrainCut DrainId
  | NormalizedDrainBarrierQueued DrainId
  | NormalizedDrainFinalized DrainId
  | NormalizedDriverEvent TraceDriverEvent
  deriving stock (Eq, Show)

data NormalizedRuntimeTraceEvent
  = NormalizedKernelEvent
      NormalizedRuntimeEventOrdinal
      NormalizedSourceName
      HeraldInput
      (Either HeraldInvariantFault EffectBatch)
  | NormalizedShellEvent
      NormalizedRuntimeEventOrdinal
      NormalizedShellTraceEvent
  deriving stock (Eq, Show)

data NormalizedRuntimeTrace
  = NormalizedRuntimeTrace
      OracleContactSet
      GeneratorSeed
      ApplicationRecoveryConfiguration
      PeerRecoveryConfiguration
      EffectBatch
      [NormalizedRuntimeTraceEvent]
      NormalizedRuntimeTraceTerminal
      (Maybe (TakeoverTarget, Maybe (VoterConfiguration, [OracleReplicaRegistration]), Maybe (Set HeraldEpoch, IsolationConfiguration)))
  deriving stock (Eq, Show)

data NormalizedRuntimeTraceTerminal
  = NormalizedRuntimeTraceExited HeraldRuntimeExit
  | NormalizedRuntimeTraceFailed HeraldRuntimeFailureClass
  deriving stock (Eq, Show)

-- | Drop nonsemantic diagnostics and terminal ledger duplication, rename
-- source identities, and assign fresh contiguous ordinals in retained order.
normalizeRuntimeTrace :: RuntimeTrace -> NormalizedRuntimeTrace
normalizeRuntimeTrace trace =
  NormalizedRuntimeTrace
    (runtimeTraceInitialOracleContacts trace)
    (runtimeTraceInitialGeneratorSeed trace)
    (runtimeTraceApplicationRecoveryConfiguration trace)
    (runtimeTracePeerRecoveryConfiguration trace)
    (runtimeTraceInitialBatch trace)
    normalizedEvents
    (normalizeTerminal (runtimeTraceTerminal trace))
    (runtimeTraceTiming trace)
  where
    normalizedEvents = reverse (normalizationRetained completed)
    completed = foldl' normalizeOne emptyNormalization (runtimeTraceEvents trace)

data Normalization = Normalization
  { normalizationSources :: Map SourceId NormalizedSourceName,
    normalizationPhysicals :: [(PhysicalConnectionName, NormalizedPhysicalName)],
    normalizationTickets :: Map PeerDispatchTicket NormalizedTicketName,
    normalizationAttempts :: [(PeerLogicalAttempt, NormalizedAttemptName)],
    normalizationNextOrdinal :: Word64,
    normalizationRetained :: [NormalizedRuntimeTraceEvent]
  }

emptyNormalization :: Normalization
emptyNormalization = Normalization Map.empty [] Map.empty [] 0 []

normalizeOne ::
  Normalization ->
  RuntimeTraceEvent ->
  Normalization
normalizeOne accumulator event = case event of
  KernelEvent _ (KernelTraceInitialized _ _) -> accumulator
  KernelEvent _ (KernelTraceStepped _ source input result) ->
    retainWithSource accumulator source $ \ordinal normalizedSource ->
      NormalizedKernelEvent ordinal normalizedSource input result
  ShellEvent _ shellEvent -> normalizeShell accumulator shellEvent

normalizeShell ::
  Normalization ->
  ShellTraceEvent ->
  Normalization
normalizeShell accumulator = \case
  ShellConnectionRegistered reference source ->
    retainWithSource (registerPhysical accumulator reference) source $ \ordinal normalizedSource ->
      NormalizedShellEvent ordinal (NormalizedConnectionRegistered normalizedSource)
  ShellConnectionPromoted reference source promotion ->
    retainWithSource (registerPhysical accumulator reference) source $ \ordinal normalizedSource ->
      NormalizedShellEvent
        ordinal
        (NormalizedConnectionPromoted normalizedSource (promotionClass promotion))
  ShellConnectionClosed reference source ->
    retainWithSource (registerPhysical accumulator reference) source $ \ordinal normalizedSource ->
      NormalizedShellEvent ordinal (NormalizedConnectionClosed normalizedSource)
  ShellStaleSuppressed (Just physical) _ ->
    retainWithPhysical accumulator physical $ \ordinal normalizedPhysical ->
      NormalizedShellEvent ordinal (NormalizedStalePhysicalSuppressed normalizedPhysical)
  ShellStaleSuppressed Nothing (Just source) ->
    retainWithSource accumulator source $ \ordinal normalizedSource ->
      NormalizedShellEvent ordinal (NormalizedStaleSourceSuppressed normalizedSource)
  ShellStaleSuppressed Nothing Nothing -> accumulator
  ShellBatchHanded _ ->
    retain accumulator $ \ordinal -> NormalizedShellEvent ordinal NormalizedBatchHanded
  ShellEffectRouted _ _ effect ->
    retain accumulator $ \ordinal ->
      NormalizedShellEvent ordinal (NormalizedEffectRouted (effectClass effect))
  ShellLaneOffered reference _ LaneOffered ->
    registerPhysical accumulator reference
  ShellLaneOffered reference lane LaneLost ->
    retainWithSource (registerPhysical accumulator reference) (physicalSource reference) $ \ordinal normalizedSource ->
      NormalizedShellEvent ordinal (NormalizedLaneOffered normalizedSource lane LaneLost)
  ShellDelayedWorkEligible ticket ->
    retainWithTicket accumulator ticket $ \ordinal normalizedTicket ->
      NormalizedShellEvent ordinal (NormalizedDelayedWorkEligible normalizedTicket)
  ShellDelayedWorkReleased ticket ->
    retainWithTicket accumulator ticket $ \ordinal normalizedTicket ->
      NormalizedShellEvent ordinal (NormalizedDelayedWorkReleased normalizedTicket)
  ShellDelayedWorkSuppressed ticket ->
    retainWithTicket accumulator ticket $ \ordinal normalizedTicket ->
      NormalizedShellEvent ordinal (NormalizedDelayedWorkSuppressed normalizedTicket)
  ShellPeerDialQueued {} -> accumulator
  ShellPeerDialHanded {} -> accumulator
  ShellPeerDialDelivered {} -> accumulator
  ShellPeerDialSuppressed {} -> accumulator
  ShellTimerArmed {} -> accumulator
  ShellTimerCancelled {} -> accumulator
  ShellTimerOutcomeQueued {} -> accumulator
  ShellTimerOutcomeSuppressed {} -> accumulator
  ShellPeerOutcomeWon reference attempt outcome ->
    retainWithSourceAttempt (registerPhysical accumulator reference) (physicalSource reference) attempt $ \ordinal source attemptName ->
      NormalizedShellEvent ordinal (NormalizedPeerOutcomeWon source attemptName outcome)
  ShellPeerOutcomeSuppressed reference attempt outcome ->
    retainWithSourceAttempt (registerPhysical accumulator reference) (physicalSource reference) attempt $ \ordinal source attemptName ->
      NormalizedShellEvent ordinal (NormalizedPeerOutcomeSuppressed source attemptName outcome)
  ShellDrainCut drainId _ ->
    retain accumulator $ \ordinal -> NormalizedShellEvent ordinal (NormalizedDrainCut drainId)
  ShellDrainBarrierQueued drainId ->
    retain accumulator $ \ordinal -> NormalizedShellEvent ordinal (NormalizedDrainBarrierQueued drainId)
  ShellDrainFinalized drainId ->
    retain accumulator $ \ordinal -> NormalizedShellEvent ordinal (NormalizedDrainFinalized drainId)
  ShellDriverEvent driverEvent ->
    retain accumulator $ \ordinal -> NormalizedShellEvent ordinal (NormalizedDriverEvent driverEvent)
  ShellChildStarted {} -> accumulator
  ShellChildFailed {} -> accumulator
  ShellChildCancellationRequested {} -> accumulator
  ShellChildJoined {} -> accumulator
  ShellConnectionWriterFailed {} -> accumulator
  ShellDiagnosticDisabled -> accumulator
  ShellDiagnosticEmitted _ -> accumulator
  ShellRuntimeExited _ -> accumulator
  ShellRuntimeFailed _ -> accumulator

retainWithSource ::
  Normalization ->
  SourceId ->
  (NormalizedRuntimeEventOrdinal -> NormalizedSourceName -> NormalizedRuntimeTraceEvent) ->
  Normalization
retainWithSource normalization source construct =
  normalization
    { normalizationSources = successorNames,
      normalizationNextOrdinal = nextOrdinal + 1,
      normalizationRetained = construct normalizedOrdinal normalizedSource : normalizationRetained normalization
    }
  where
    names = normalizationSources normalization
    nextOrdinal = normalizationNextOrdinal normalization
    normalizedOrdinal = NormalizedRuntimeEventOrdinal nextOrdinal
    (normalizedSource, successorNames) = normalizeSource names source

retain ::
  Normalization ->
  (NormalizedRuntimeEventOrdinal -> NormalizedRuntimeTraceEvent) ->
  Normalization
retain normalization construct =
  normalization
    { normalizationNextOrdinal = nextOrdinal + 1,
      normalizationRetained = construct (NormalizedRuntimeEventOrdinal nextOrdinal) : normalizationRetained normalization
    }
  where
    nextOrdinal = normalizationNextOrdinal normalization

retainWithPhysical ::
  Normalization ->
  PhysicalConnectionName ->
  (NormalizedRuntimeEventOrdinal -> NormalizedPhysicalName -> NormalizedRuntimeTraceEvent) ->
  Normalization
retainWithPhysical normalization physical construct =
  retain successor (\ordinal -> construct ordinal normalizedPhysical)
  where
    (normalizedPhysical, successor) = normalizePhysical normalization physical

retainWithTicket ::
  Normalization ->
  PeerDispatchTicket ->
  (NormalizedRuntimeEventOrdinal -> NormalizedTicketName -> NormalizedRuntimeTraceEvent) ->
  Normalization
retainWithTicket normalization ticket construct =
  (retain successor (\ordinal -> construct ordinal normalizedTicket))
  where
    (normalizedTicket, successor) = normalizeTicket normalization ticket

retainWithSourceAttempt ::
  Normalization ->
  SourceId ->
  PeerLogicalAttempt ->
  (NormalizedRuntimeEventOrdinal -> NormalizedSourceName -> NormalizedAttemptName -> NormalizedRuntimeTraceEvent) ->
  Normalization
retainWithSourceAttempt normalization source attempt construct =
  retainWithSource successor source $ \ordinal normalizedSource ->
    construct ordinal normalizedSource normalizedAttempt
  where
    (normalizedAttempt, successor) = normalizeAttempt normalization attempt

normalizeSource :: Map SourceId NormalizedSourceName -> SourceId -> (NormalizedSourceName, Map SourceId NormalizedSourceName)
normalizeSource names source@(SourceId family _) =
  case Map.lookup source names of
    Just normalized -> (normalized, names)
    Nothing ->
      let normalized = NormalizedSourceName family (fromIntegral (Map.size names))
       in (normalized, Map.insert source normalized names)

normalizePhysical :: Normalization -> PhysicalConnectionName -> (NormalizedPhysicalName, Normalization)
normalizePhysical normalization physical =
  case lookup physical (normalizationPhysicals normalization) of
    Just normalized -> (normalized, normalization)
    Nothing ->
      let normalized = NormalizedPhysicalName (fromIntegral (length (normalizationPhysicals normalization)))
       in ( normalized,
            normalization
              { normalizationPhysicals = (physical, normalized) : normalizationPhysicals normalization
              }
          )

registerPhysical :: Normalization -> PhysicalConnectionRef -> Normalization
registerPhysical normalization reference = snd (normalizePhysical normalization (physicalName reference))

physicalName :: PhysicalConnectionRef -> PhysicalConnectionName
physicalName = \case
  PhysicalApplicationRef reference -> PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  PhysicalAdministrationRef reference -> PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  PhysicalConfiguredAdministrationRef reference ->
    PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  PhysicalPeerRef reference -> PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  PhysicalOpaqueRef token ordinal -> PhysicalConnectionName token ordinal

normalizeTicket :: Normalization -> PeerDispatchTicket -> (NormalizedTicketName, Normalization)
normalizeTicket normalization ticket =
  case Map.lookup ticket (normalizationTickets normalization) of
    Just normalized -> (normalized, normalization)
    Nothing ->
      let normalized = NormalizedTicketName (fromIntegral (Map.size (normalizationTickets normalization)))
       in ( normalized,
            normalization
              { normalizationTickets = Map.insert ticket normalized (normalizationTickets normalization)
              }
          )

normalizeAttempt :: Normalization -> PeerLogicalAttempt -> (NormalizedAttemptName, Normalization)
normalizeAttempt normalization attempt =
  case lookup attempt (normalizationAttempts normalization) of
    Just normalized -> (normalized, normalization)
    Nothing ->
      let normalized = NormalizedAttemptName (fromIntegral (length (normalizationAttempts normalization)))
       in ( normalized,
            normalization
              { normalizationAttempts = (attempt, normalized) : normalizationAttempts normalization
              }
          )

physicalSource :: PhysicalConnectionRef -> SourceId
physicalSource = \case
  PhysicalApplicationRef reference -> SourceId ApplicationSources (connectionRefOrdinal reference)
  PhysicalAdministrationRef reference -> SourceId AdministrationSources (connectionRefOrdinal reference)
  PhysicalConfiguredAdministrationRef reference ->
    SourceId ConfiguredAdministrationSources (connectionRefOrdinal reference)
  PhysicalPeerRef reference -> SourceId PeerSources (connectionRefOrdinal reference)
  PhysicalOpaqueRef _ ordinal -> SourceId RuntimeCompletionSources ordinal

promotionClass :: ConnectionPromotion -> NormalizedConnectionPromotionClass
promotionClass = \case
  ApplicationConnectionPromoted {} -> NormalizedApplicationPromotion
  ConfiguredAdministrationConnectionPromoted {} ->
    NormalizedConfiguredAdministrationPromotion
  PeerConnectionPromoted {} -> NormalizedPeerPromotion

effectClass :: HeraldEffect -> NormalizedHeraldEffectClass
effectClass = \case
  DeferApplicationSession {} -> NormalizedApplicationDeferral
  SetApplicationConnectionDisposition {} -> NormalizedApplicationDisposition
  RejectApplicationConnection {} -> NormalizedApplicationRejection
  SendInitialClaimPending {} -> NormalizedInitialClaimPending
  RejectInitialClaim {} -> NormalizedInitialClaimRejection
  SendApplicationLifecycleReply {} -> NormalizedApplicationLifecycleReply
  SendApplicationReply {} -> NormalizedApplicationReply
  SendApplicationWaitWake {} -> NormalizedApplicationWaitWake
  SendApplicationIsolationBegun {} -> NormalizedApplicationIsolationBegun
  DisposeApplicationSession {} -> NormalizedApplicationSessionDisposal
  SetAdministrationConnectionDisposition {} -> NormalizedAdministrationDisposition
  RejectAdministrationConnection {} -> NormalizedAdministrationRejection
  SendJoinReply {} -> NormalizedJoinReply
  SendAdministrationReply {} -> NormalizedAdministrationReply
  SendPeerCandidate {} -> NormalizedPeerCandidate
  RejectPeerCandidateOpened {} -> NormalizedPeerCandidateRejection
  SetPeerCandidateDisposition {} -> NormalizedPeerCandidateDisposition
  QueuePeerAlignmentEvidence {} -> error "unnormalized peer alignment evidence escaped the Herald transition"
  SendPeerControl {} -> NormalizedPeerControl
  SendPeerControlWithProgress {} -> NormalizedPeerControl
  DialPeer {} -> NormalizedPeerDial
  ClosePeerBinding {} -> NormalizedPeerBindingClose
  CancelPeerDial {} -> NormalizedPeerDialCancellation
  CancelPeerDestination {} -> NormalizedPeerDestinationCancellation
  SchedulePeerDispatch {} -> NormalizedPeerDispatchSchedule
  SendPeerItem {} -> NormalizedPeerItem
  SendPeerItemWithProgress {} -> NormalizedPeerItem
  RejectPeerConnection {} -> NormalizedPeerRejection
  RunOracleClientAction {} -> NormalizedOracleClientAction
  RunOracleHealthRound {} -> NormalizedOracleHealthRound
  ArmTimer {} -> NormalizedTimerArm
  CancelTimer {} -> NormalizedTimerCancel
  BeginDrain {} -> NormalizedDrainBegin
  FinishDrain {} -> NormalizedDrainFinish
  FinishIsolation {} -> NormalizedIsolationFinish

normalizeTerminal :: RuntimeTraceTerminal -> NormalizedRuntimeTraceTerminal
normalizeTerminal = \case
  RuntimeTraceExited runtimeExit -> NormalizedRuntimeTraceExited runtimeExit
  RuntimeTraceFailed (HeraldRuntimeFailure failureClass) ->
    NormalizedRuntimeTraceFailed failureClass

data TypedReplayMismatch
  = TypedReplayOrdinalDiscontinuity RuntimeEventOrdinal RuntimeEventOrdinal
  | TypedReplayInitializationEventMissing
  | TypedReplayInitializationEventDuplicated
  | TypedReplayInitializationBatchMismatch EffectBatch EffectBatch
  | TypedReplayInitializationFailed HeraldInvariantFault
  | TypedReplayStepOrdinalMismatch StepOrdinal StepOrdinal
  | TypedReplayStepResultMismatch
      StepOrdinal
      (Either HeraldInvariantFault EffectBatch)
      (Either HeraldInvariantFault EffectBatch)
  | TypedReplayStepAfterInvariantFault StepOrdinal
  | TypedReplayFinalStateMismatch
  | TypedReplayFinalFaultMismatch HeraldInvariantFault HeraldInvariantFault
  | TypedReplayExpectedInvariantFault HeraldInvariantFault
  | TypedReplayUnexpectedInvariantFault HeraldInvariantFault
  deriving stock (Eq, Show)

data TypedReplayResult
  = TypedReplayFinalState HeraldState
  | TypedReplayInvariantFault HeraldInvariantFault
  deriving stock (Eq)

instance Show TypedReplayResult where
  show (TypedReplayFinalState _) = "TypedReplayFinalState"
  show (TypedReplayInvariantFault fault) = "TypedReplayInvariantFault " <> show fault

-- | Re-run only the logical inputs selected by the owner.  Physical shell
-- events are deliberately ignored: stale callbacks can therefore never become
-- replayed kernel work.
replayRuntimeTrace ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeTrace ->
  Either TypedReplayMismatch TypedReplayResult
replayRuntimeTrace genesis bootstraps trace = do
  checkEventOrdinals (runtimeTraceEvents trace)
  recordedInitialBatch <- recordedInitializationBatch (runtimeTraceEvents trace)
  (initialState, actualInitialBatch) <-
    either (Left . TypedReplayInitializationFailed) Right
      $ case runtimeTraceTiming trace of
        Nothing ->
          initialHerald
            (runtimeTraceInitialObservation trace)
            genesis
            bootstraps
            (runtimeTraceInitialOracleContacts trace)
            (runtimeTraceInitialGeneratorSeed trace)
            (runtimeTraceApplicationRecoveryConfiguration trace)
            (runtimeTracePeerRecoveryConfiguration trace)
        Just (target, voters, hosts) ->
          initialHeraldWithTimingAndIsolation
            hosts
            target
            voters
            (runtimeTraceInitialObservation trace)
            genesis
            bootstraps
            (runtimeTraceInitialOracleContacts trace)
            (runtimeTraceInitialGeneratorSeed trace)
  ensureEqualBatch recordedInitialBatch (runtimeTraceInitialBatch trace)
  ensureEqualBatch recordedInitialBatch actualInitialBatch
  replayed <- replaySteps (configureHeraldDiagnosticChecks (runtimeTraceDiagnosticChecks trace) initialState) (kernelSteps (runtimeTraceEvents trace))
  checkFinalEvidence (runtimeTraceFinalEvidence trace) replayed

checkEventOrdinals :: [RuntimeTraceEvent] -> Either TypedReplayMismatch ()
checkEventOrdinals = go 0
  where
    go _ [] = Right ()
    go expected (event : events) =
      let actual = traceEventOrdinal event
          wanted = RuntimeEventOrdinal expected
       in if actual == wanted
            then go (expected + 1) events
            else Left (TypedReplayOrdinalDiscontinuity wanted actual)

recordedInitializationBatch :: [RuntimeTraceEvent] -> Either TypedReplayMismatch EffectBatch
recordedInitializationBatch events = case [batch | KernelEvent _ (KernelTraceInitialized _ batch) <- events] of
  [] -> Left TypedReplayInitializationEventMissing
  [batch] -> Right batch
  _ -> Left TypedReplayInitializationEventDuplicated

ensureEqualBatch :: EffectBatch -> EffectBatch -> Either TypedReplayMismatch ()
ensureEqualBatch recorded actual
  | recorded == actual = Right ()
  | otherwise = Left (TypedReplayInitializationBatchMismatch recorded actual)

kernelSteps :: [RuntimeTraceEvent] -> [KernelTraceStep]
kernelSteps events = [step | KernelEvent _ step@KernelTraceStepped {} <- events]

replaySteps ::
  HeraldState ->
  [KernelTraceStep] ->
  Either TypedReplayMismatch TypedReplayResult
replaySteps = go (StepOrdinal 0)
  where
    go _ state [] = Right (TypedReplayFinalState state)
    go expected state (KernelTraceStepped actual _ input recorded : steps)
      | actual /= expected = Left (TypedReplayStepOrdinalMismatch expected actual)
      | otherwise = case stepHerald input state of
          Left fault
            | recorded /= Left fault ->
                Left (TypedReplayStepResultMismatch actual recorded (Left fault))
            | otherwise -> case steps of
                [] -> Right (TypedReplayInvariantFault fault)
                KernelTraceStepped next _ _ _ : _ -> Left (TypedReplayStepAfterInvariantFault next)
                KernelTraceInitialized _ _ : _ -> Left TypedReplayInitializationEventDuplicated
          Right (successor, batch)
            | recorded /= Right batch ->
                Left (TypedReplayStepResultMismatch actual recorded (Right batch))
            | otherwise -> go (successorStep expected) successor steps
    go _ _ (KernelTraceInitialized _ _ : _) = Left TypedReplayInitializationEventDuplicated

successorStep :: StepOrdinal -> StepOrdinal
successorStep (StepOrdinal current) = StepOrdinal (current + 1)

checkFinalEvidence ::
  RuntimeTraceFinalEvidence ->
  TypedReplayResult ->
  Either TypedReplayMismatch TypedReplayResult
checkFinalEvidence evidence replayed = case (evidence, replayed) of
  (RuntimeTraceFinalState expected, TypedReplayFinalState actual)
    | expected == actual -> Right replayed
    | otherwise -> Left TypedReplayFinalStateMismatch
  (RuntimeTraceFinalInvariantFault expected, TypedReplayInvariantFault actual)
    | expected == actual -> Right replayed
    | otherwise -> Left (TypedReplayFinalFaultMismatch expected actual)
  (RuntimeTraceFinalState _, TypedReplayInvariantFault actual) ->
    Left (TypedReplayUnexpectedInvariantFault actual)
  (RuntimeTraceFinalInvariantFault expected, TypedReplayFinalState _) ->
    Left (TypedReplayExpectedInvariantFault expected)

traceEventOrdinal :: RuntimeTraceEvent -> RuntimeEventOrdinal
traceEventOrdinal = \case
  KernelEvent ordinal _ -> ordinal
  ShellEvent ordinal _ -> ordinal
