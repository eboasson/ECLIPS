-- | Exact typed, in-memory conformance trace.
module Eclips.Herald.Runtime.Internal.Trace
  ( RuntimeEventOrdinal (..),
    KernelTraceStep (..),
    PhysicalConnectionRef (..),
    PhysicalConnectionName (..),
    ConnectionPromotion (..),
    PhysicalLaneClass (..),
    RuntimeWorkerClass (..),
    TraceDriverEvent (..),
    ShellTraceEvent (..),
    RuntimeTraceEvent (..),
    TraceCaptureMode (..),
    TraceLedger,
    TraceCursor,
    newTraceLedger,
    newTraceLedgerWithMode,
    newTraceLedgerWithWorkCounts,
    traceLedgerCapturesHistory,
    appendKernelTrace,
    appendShellTrace,
    snapshotTraceLedger,
    traceLedgerCursor,
    snapshotTraceLedgerSince,
    snapshotTraceWorkCounts,
    addTraceWorkCounts,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
  ( STM,
    TVar,
    modifyTVar',
    newTVar,
    readTVar,
    writeTVar,
  )
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Herald.Administration
  ( AdministrationBinding,
    DrainId,
  )
import Eclips.Herald.Application.Session (ApplicationSessionBinding)
import Eclips.Herald.Discovery (PeerBinding, PeerDialIntent)
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Initialization (HeraldInvariantFault)
import Eclips.Herald.Input
  ( HeraldInput,
    HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (..),
    RuntimeObservation (..),
    inputBody,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome,
    PeerDispatchTicket,
    PeerLogicalAttempt,
  )
import Eclips.Herald.Runtime.Internal.Arbiter (SourceId (..))
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchOrdinal,
    EffectMemberOrdinal,
    StepOrdinal,
  )
import Eclips.Herald.Runtime.Internal.Types
  ( AdministrationPlane,
    ApplicationPlane,
    ConfiguredAdministrationPlane,
    ConnectionRef (..),
    HeraldRuntimeExit,
    HeraldRuntimeFailureClass,
    PeerPlane,
    RuntimeDiagnosticClass,
    RuntimeLaneOffer,
    RuntimeToken,
  )
import Eclips.Herald.Timer
  ( TimerAttempt,
    TimerOutcome,
    TimerSpec,
  )

newtype RuntimeEventOrdinal = RuntimeEventOrdinal Word64
  deriving stock (Eq, Ord, Show)

data KernelTraceStep
  = KernelTraceInitialized BatchOrdinal EffectBatch
  | KernelTraceStepped StepOrdinal SourceId HeraldInput (Either HeraldInvariantFault EffectBatch)
  deriving stock (Eq, Show)

-- | The exact role-indexed physical lease.  This sum is private to the
-- conformance ledger, so retaining the phantom role does not expose a generic
-- public connection capability.
data PhysicalConnectionRef
  = PhysicalApplicationRef (ConnectionRef ApplicationPlane)
  | PhysicalAdministrationRef (ConnectionRef AdministrationPlane)
  | PhysicalConfiguredAdministrationRef (ConnectionRef ConfiguredAdministrationPlane)
  | PhysicalPeerRef (ConnectionRef PeerPlane)
  | PhysicalOpaqueRef RuntimeToken Word64
  deriving stock (Eq)

instance Show PhysicalConnectionRef where
  show _ = "PhysicalConnectionRef"

-- | Role-independent identity used only when the polymorphic public close
-- capability cannot recover the phantom plane.  It is deliberately not a
-- semantic 'SourceId': a stale physical lease must never masquerade as runtime
-- completion ingress.
data PhysicalConnectionName = PhysicalConnectionName RuntimeToken Word64
  deriving stock (Eq, Ord)

instance Show PhysicalConnectionName where
  show _ = "PhysicalConnectionName"

data ConnectionPromotion
  = ApplicationConnectionPromoted ApplicationSessionBinding
  | ConfiguredAdministrationConnectionPromoted AdministrationBinding
  | PeerConnectionPromoted PeerBinding
  deriving stock (Eq, Show)

data PhysicalLaneClass
  = ApplicationLane
  | AdministrationLane
  | ConfiguredAdministrationLane
  | PeerCandidateLane
  | PeerDispositionLane
  | PeerControlLane
  | PeerRejectionLane
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data RuntimeWorkerClass
  = OwnerWorker
  | DispatcherWorker
  | DelayedWorkWorker
  | PeerDialSinkWorker
  | TimerWorker
  | DiagnosticWorker
  | ConnectionWriterWorker
  | SupervisorWorker
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Test-driver observations retained by package-private parity runs.  These
-- describe calls through the public seam; they are not runtime inputs and are
-- never replayed as kernel work.
data TraceDriverEvent
  = DriverApplicationRegistered
  | DriverApplicationSubmitted
  | DriverApplicationClosed
  | DriverStaleCloseProbed
  | DriverScopeClosed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data ShellTraceEvent
  = ShellConnectionRegistered PhysicalConnectionRef SourceId
  | ShellConnectionPromoted PhysicalConnectionRef SourceId ConnectionPromotion
  | ShellConnectionClosed PhysicalConnectionRef SourceId
  | ShellStaleSuppressed (Maybe PhysicalConnectionName) (Maybe SourceId)
  | ShellBatchHanded BatchOrdinal
  | ShellEffectRouted BatchOrdinal EffectMemberOrdinal HeraldEffect
  | ShellLaneOffered PhysicalConnectionRef PhysicalLaneClass RuntimeLaneOffer
  | ShellDelayedWorkEligible PeerDispatchTicket
  | ShellDelayedWorkReleased PeerDispatchTicket
  | ShellDelayedWorkSuppressed PeerDispatchTicket
  | ShellPeerDialQueued PeerDialIntent
  | ShellPeerDialHanded PeerDialIntent
  | ShellPeerDialDelivered PeerDialIntent
  | ShellPeerDialSuppressed PeerDialIntent
  | ShellTimerArmed TimerAttempt TimerSpec
  | ShellTimerCancelled TimerAttempt
  | ShellTimerOutcomeQueued TimerAttempt TimerOutcome
  | ShellTimerOutcomeSuppressed TimerAttempt TimerOutcome
  | ShellPeerOutcomeWon PhysicalConnectionRef PeerLogicalAttempt PeerDispatchOutcome
  | ShellPeerOutcomeSuppressed PhysicalConnectionRef PeerLogicalAttempt PeerDispatchOutcome
  | ShellConnectionWriterFailed PhysicalConnectionRef
  | ShellDrainCut DrainId BatchOrdinal
  | ShellDrainBarrierQueued DrainId
  | ShellDrainFinalized DrainId
  | ShellChildStarted RuntimeWorkerClass ThreadId
  | ShellChildFailed RuntimeWorkerClass ThreadId
  | ShellChildCancellationRequested RuntimeWorkerClass ThreadId
  | ShellChildJoined RuntimeWorkerClass ThreadId
  | ShellDriverEvent TraceDriverEvent
  | ShellDiagnosticDisabled
  | ShellDiagnosticEmitted RuntimeDiagnosticClass
  | ShellRuntimeExited HeraldRuntimeExit
  | ShellRuntimeFailed HeraldRuntimeFailureClass
  deriving stock (Eq, Show)

data RuntimeTraceEvent
  = KernelEvent RuntimeEventOrdinal KernelTraceStep
  | ShellEvent RuntimeEventOrdinal ShellTraceEvent
  deriving stock (Eq, Show)

-- | Exact payloads support private conformance recording. Ordinary runtimes
-- allocate the same causal ordinals without retaining a lifetime transcript.
data TraceCaptureMode = CaptureExactHistory | OrdinalsOnly
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data TraceLedger
  = TraceLedger
      (TVar Word64)
      (Maybe (TVar [RuntimeTraceEvent]))
      (Maybe (TVar (Map String Word64)))

newtype TraceCursor = TraceCursor Word64
  deriving stock (Eq, Ord, Show)

newTraceLedger :: STM TraceLedger
newTraceLedger = newTraceLedgerWithMode CaptureExactHistory

newTraceLedgerWithMode :: TraceCaptureMode -> STM TraceLedger
newTraceLedgerWithMode mode = newTraceLedgerConfigured mode False

-- | Opt-in aggregate measurement, independent of exact history capture.
-- Category keys come only from closed constructor vocabularies; correlations,
-- payloads and individual events never become retained keys or values.
newTraceLedgerWithWorkCounts :: TraceCaptureMode -> STM TraceLedger
newTraceLedgerWithWorkCounts mode = newTraceLedgerConfigured mode True

newTraceLedgerConfigured :: TraceCaptureMode -> Bool -> STM TraceLedger
newTraceLedgerConfigured mode countWork = do
  ordinal <- newTVar 0
  history <- case mode of
    CaptureExactHistory -> Just <$> newTVar []
    OrdinalsOnly -> pure Nothing
  counts <- if countWork then Just <$> newTVar Map.empty else pure Nothing
  pure (TraceLedger ordinal history counts)

traceLedgerCapturesHistory :: TraceLedger -> Bool
traceLedgerCapturesHistory (TraceLedger _ events _) = case events of
  Just _ -> True
  Nothing -> False

appendKernelTrace :: TraceLedger -> KernelTraceStep -> STM RuntimeEventOrdinal
appendKernelTrace ledger event = appendTrace ledger (`KernelEvent` event)

appendShellTrace :: TraceLedger -> ShellTraceEvent -> STM RuntimeEventOrdinal
appendShellTrace ledger event =
  -- Compact physical identities may be lazy projections from a connection
  -- slot or ingress envelope. Materialize them only when retaining a history,
  -- so a diagnostic identity does not keep its old queue and handlers alive.
  (if traceLedgerCapturesHistory ledger then forceShellIdentity event else ())
    `seq` appendTrace ledger (`ShellEvent` event)

forceShellIdentity :: ShellTraceEvent -> ()
forceShellIdentity = \case
  ShellConnectionRegistered reference source -> forcePhysical reference `seq` forceSource source
  ShellConnectionPromoted reference source _ -> forcePhysical reference `seq` forceSource source
  ShellConnectionClosed reference source -> forcePhysical reference `seq` forceSource source
  ShellStaleSuppressed physical source -> maybe () forceName physical `seq` maybe () forceSource source
  ShellLaneOffered reference _ _ -> forcePhysical reference
  ShellPeerOutcomeWon reference _ _ -> forcePhysical reference
  ShellPeerOutcomeSuppressed reference _ _ -> forcePhysical reference
  ShellConnectionWriterFailed reference -> forcePhysical reference
  _ -> ()
  where
    forceSource (SourceId family ordinal) = family `seq` ordinal `seq` ()
    forceName (PhysicalConnectionName token ordinal) = token `seq` ordinal `seq` ()
    forcePhysical = \case
      PhysicalApplicationRef (ConnectionRef token ordinal) -> token `seq` ordinal `seq` ()
      PhysicalAdministrationRef (ConnectionRef token ordinal) -> token `seq` ordinal `seq` ()
      PhysicalConfiguredAdministrationRef (ConnectionRef token ordinal) -> token `seq` ordinal `seq` ()
      PhysicalPeerRef (ConnectionRef token ordinal) -> token `seq` ordinal `seq` ()
      PhysicalOpaqueRef token ordinal -> token `seq` ordinal `seq` ()

snapshotTraceLedger :: TraceLedger -> STM [RuntimeTraceEvent]
snapshotTraceLedger (TraceLedger _ events _) =
  maybe (pure []) (fmap reverse . readTVar) events

-- | Capture the first ordinal that a subsequent suffix snapshot should
-- include. The cursor and event list are observed transactionally. An
-- ordinal-only ledger advances this cursor without retaining event payloads.
traceLedgerCursor :: TraceLedger -> STM TraceCursor
traceLedgerCursor (TraceLedger nextOrdinal _ _) =
  TraceCursor <$> readTVar nextOrdinal

-- | Read only events appended at or after the supplied cursor and return the
-- next cursor. The ledger is newest-first internally, so the ordered suffix
-- requires one reversal whose cost is proportional only to that suffix.
snapshotTraceLedgerSince ::
  TraceCursor ->
  TraceLedger ->
  STM (TraceCursor, [RuntimeTraceEvent])
snapshotTraceLedgerSince (TraceCursor first) (TraceLedger nextOrdinal events _) = do
  next <- readTVar nextOrdinal
  newestFirst <- maybe (pure []) readTVar events
  let suffix = takeWhile ((>= first) . eventOrdinal) newestFirst
  pure (TraceCursor next, reverse suffix)
  where
    eventOrdinal (KernelEvent (RuntimeEventOrdinal ordinal) _) = ordinal
    eventOrdinal (ShellEvent (RuntimeEventOrdinal ordinal) _) = ordinal

appendTrace ::
  TraceLedger ->
  (RuntimeEventOrdinal -> RuntimeTraceEvent) ->
  STM RuntimeEventOrdinal
appendTrace (TraceLedger nextOrdinal events counts) construct = do
  value <- readTVar nextOrdinal
  -- With no retained event to demand an ordinal, force the counter here.
  let !next = value + 1
      ordinal = RuntimeEventOrdinal value
  writeTVar nextOrdinal next
  case events of
    Just retained -> modifyTVar' retained (construct ordinal :)
    Nothing -> pure ()
  case counts of
    Just retained -> modifyTVar' retained (addWorkCounts (traceWorkCountKeys (construct ordinal)))
    Nothing -> pure ()
  pure ordinal

snapshotTraceWorkCounts :: TraceLedger -> STM (Maybe (Map String Word64))
snapshotTraceWorkCounts (TraceLedger _ _ counts) = traverse readTVar counts

-- | Add finite diagnostic categories computed by the private kernel owner.
-- No event or state is retained, and a disabled ledger never demands the input.
-- The owner materializes state-derived counts before entering STM.
addTraceWorkCounts :: TraceLedger -> [(String, Word64)] -> STM ()
addTraceWorkCounts (TraceLedger _ _ counts) additions = case counts of
  Nothing -> pure ()
  Just retained -> modifyTVar' retained (\current -> foldl' addCount current additions)
  where
    addCount current (key, count) =
      foldl' (\() character -> character `seq` ()) () key
        `seq` count
        `seq` Map.insertWith (+) key count current

addWorkCounts :: [String] -> Map String Word64 -> Map String Word64
addWorkCounts keys initial = foldl' add initial keys
  where
    add counts key =
      -- A partially evaluated constructor tag could otherwise retain the Show
      -- closure and its event. Force the complete small key before insertion.
      foldl' (\() character -> character `seq` ()) () key
        `seq` Map.insertWith (+) key 1 counts

traceWorkCountKeys :: RuntimeTraceEvent -> [String]
traceWorkCountKeys = \case
  KernelEvent _ (KernelTraceInitialized _ batch) ->
    "kernel.initialized" : concatMap (effectWorkCountKeys "effect.emitted") (effectBatchMembers batch)
  KernelEvent _ (KernelTraceStepped _ (SourceId family _) input result) ->
    "kernel.input.total"
      : ("kernel.source." <> constructorTag family)
      : inputWorkCountKeys (inputBody input)
        <> case result of
          Left _ -> ["kernel.result.fault"]
          Right batch ->
            "kernel.result.success" : concatMap (effectWorkCountKeys "effect.emitted") (effectBatchMembers batch)
  ShellEvent _ event ->
    ("shell.event." <> constructorTag event) : case event of
      ShellEffectRouted _ _ effect -> effectWorkCountKeys "effect.routed" effect
      ShellLaneOffered _ lane outcome -> ["lane.offer." <> constructorTag lane <> "." <> constructorTag outcome]
      ShellPeerOutcomeWon _ _ outcome -> ["dispatch.outcome.won." <> constructorTag outcome]
      ShellPeerOutcomeSuppressed _ _ outcome -> ["dispatch.outcome.suppressed." <> constructorTag outcome]
      ShellTimerArmed attempt _ -> ["timer.armed." <> constructorTag attempt]
      ShellTimerCancelled attempt -> ["timer.cancelled." <> constructorTag attempt]
      ShellTimerOutcomeQueued attempt outcome -> ["timer.queued." <> constructorTag attempt <> "." <> constructorTag outcome]
      ShellTimerOutcomeSuppressed attempt outcome -> ["timer.suppressed." <> constructorTag attempt <> "." <> constructorTag outcome]
      ShellChildStarted worker _ -> ["worker.started." <> constructorTag worker]
      ShellChildJoined worker _ -> ["worker.joined." <> constructorTag worker]
      _ -> []

inputWorkCountKeys :: HeraldInputBody -> [String]
inputWorkCountKeys body =
  let family = "kernel.input." <> constructorTag body
   in family : fmap ((family <> ".") <>) (details body)
  where
    details = \case
      ApplicationSessionInput ingress -> [constructorTag ingress]
      ApplicationRequestInput ingress -> [constructorTag ingress]
      ApplicationLifecycleInput ingress -> [constructorTag ingress]
      ApplicationReceiptRetirementInput ingress -> [constructorTag ingress]
      AdministrationInput ingress -> [constructorTag ingress]
      OracleInput ingress -> [constructorTag ingress]
      PeerInput ingress ->
        constructorTag ingress : case ingress of
          PeerControlReceived _ control -> fmap ("PeerControlReceived." <>) (peerControlTags control)
          PeerControlReceivedWithProgress _ _ control -> fmap ("PeerControlReceivedWithProgress." <>) ("ReceiptProgress" : peerControlTags control)
          PeerPublicationReceivedWithProgress {} -> ["PeerPublicationReceivedWithProgress.ReceiptProgress"]
          _ -> []
      RuntimeObserved observation ->
        constructorTag observation : case observation of
          TimerObserved attempt outcome -> ["TimerObserved." <> constructorTag attempt <> "." <> constructorTag outcome]
          PeerDispatchObserved _ outcome -> ["PeerDispatchObserved." <> constructorTag outcome]
          _ -> []
      -- Management authority and opaque join bytes need only their outer
      -- family count; diagnostics neither inspect nor originate those inputs.
      _ -> []

effectWorkCountKeys :: String -> HeraldEffect -> [String]
effectWorkCountKeys prefix effect =
  let family = prefix <> "." <> constructorTag effect
   in [prefix <> ".total", family]
        <> fmap ((family <> ".") <>) (details effect)
  where
    details = \case
      SendPeerControl _ control -> peerControlTags control
      SendPeerControlWithProgress _ _ control -> "ReceiptProgress" : peerControlTags control
      SendPeerItemWithProgress {} -> ["ReceiptProgress"]
      RunOracleClientAction action -> [constructorTag action]
      ArmTimer attempt _ -> [constructorTag attempt]
      CancelTimer attempt -> [constructorTag attempt]
      _ -> []

peerControlTags :: PeerControl -> [String]
peerControlTags control =
  constructorTag control : case control of
    PeerAlignmentControl message -> ["PeerAlignmentControl." <> constructorTag message]
    PeerAlignmentEvidenceDelivered _ message -> ["PeerAlignmentEvidenceDelivered." <> constructorTag message, "PeerAlignmentControl." <> constructorTag message]
    PeerAlignmentDeliveryProgress -> []
    PeerPreparationControl message -> ["PeerPreparationControl." <> constructorTag message]
    _ -> []

-- All uses are closed algebraic constructors with constructor-first Show
-- instances. Stop at their first delimiter: no payload is rendered or retained.
constructorTag :: (Show value) => value -> String
constructorTag = takeWhile (\character -> isAlphaNum character || character == '_') . show
