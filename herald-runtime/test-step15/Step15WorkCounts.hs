{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Transactional, attributable work evidence for the Step-15 acceptance tour.
--
-- Cursors retain typed runtime and physical-probe frontiers. They deliberately
-- contain no wall-clock measurements: later crash/partition scenarios can
-- state exact logical cardinalities and use timeouts only as hang guards.
module Step15WorkCounts
  ( Step15WorkLedger,
    newStep15WorkLedger,
    captureStep15RuntimeHooks,
    installStep15WorkProbes,
    awaitStep15WorkLedgerReady,
    awaitStep15FinalEvidence,
    Step15WorkCursor,
    snapshotStep15WorkCursors,
    snapshotStep15WorkBaselines,
    Step15WorkEvidence,
    step15RuntimeEvidence,
    step15ConfiguredSeedEvidence,
    step15PeerDialEvidence,
    step15HeartbeatEvidence,
    step15OracleActionEvidence,
    step15OracleManagerEvidence,
    step15OracleProjectionViews,
    snapshotStep15WorkEvidence,
    snapshotStep15WorkEvidenceSince,
    awaitStep15WorkEvidenceSince,
    step15KernelFaultEvidence,
    Step15AttributableWork (..),
    step15TimerLifecycleEvents,
    step15PeerDialLifecycleEvents,
    step15HeartbeatLifecycleEvents,
    step15OracleLifecycleEvents,
    step15TerminalControlEvents,
    summarizeStep15WorkEvidence,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    check,
    modifyTVar',
    newEmptyTMVarIO,
    newTVarIO,
    readTMVar,
    readTVar,
    tryPutTMVar,
  )
import Control.Monad (void)
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Herald.EffectBatch
  ( HeraldEffect (FinishIsolation),
  )
import Eclips.Herald.Input
  ( HeraldInputBody (OracleInput, PeerInput),
    PeerControl (..),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (SubmitOracleRequest),
    OracleClientIngress (OracleEntriesReceived),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
  )
import Eclips.Herald.Runtime.Internal.Recording
  ( RuntimeTraceFinalEvidence,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent, ShellEvent),
    ShellTraceEvent (..),
    TraceCursor,
    TraceLedger,
    snapshotTraceLedger,
    snapshotTraceLedgerSince,
    traceLedgerCursor,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( setHeraldTcpOracleActionProbe,
    setHeraldTcpOracleManagerProbe,
  )
import Eclips.Herald.Runtime.TCP.Internal.Heartbeat
  ( setHeartbeatProbeForTest,
  )
import Eclips.Herald.Runtime.TCP.Internal.Peer
  ( setConfiguredSeedProbeForTest,
    setPeerDialProbeForTest,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( ConfiguredSeedProbeEvent (..),
    HeartbeatProbeEvent (HeartbeatProbeEvent),
    HeartbeatProbePhase (..),
    HeraldTcp,
    OracleManagerProbeEvent (..),
    PeerDialProbeEvent (PeerDialProbeEvent),
    PeerDialProbePhase (..),
  )
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue)
import Eclips.Oracle.Projection
  ( OracleProjectionEventView,
    appliedEntryProjectionEvents,
    oracleProjectionEventView,
  )
import PeerEvidence (data SemanticPeerControlReceived, data SemanticSendPeerControl)

data Step15WorkLedger
  = Step15WorkLedger
      (TMVar TraceLedger)
      (TVar [ConfiguredSeedProbeEvent])
      (TVar [PeerDialProbeEvent])
      (TVar [HeartbeatProbeEvent])
      (TVar [OracleClientAction])
      (TVar [OracleManagerProbeEvent])
      (TMVar RuntimeTraceFinalEvidence)

newStep15WorkLedger :: IO Step15WorkLedger
newStep15WorkLedger =
  Step15WorkLedger
    <$> newEmptyTMVarIO
    <*> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO []
    <*> newEmptyTMVarIO

captureStep15RuntimeHooks ::
  Step15WorkLedger ->
  RuntimeHooks ->
  RuntimeHooks
captureStep15RuntimeHooks
  (Step15WorkLedger traceCell _ _ _ _ _ finalEvidence)
  hooks =
    hooks
      { hookRuntimeInitialized = \instant contacts seed applicationRecovery peerRecovery initialEffects trace -> do
          void (atomically (tryPutTMVar traceCell trace))
          hookRuntimeInitialized
            hooks
            instant
            contacts
            seed
            applicationRecovery
            peerRecovery
            initialEffects
            trace,
        hookPublishFinalEvidence = \evidence -> do
          void (atomically (tryPutTMVar finalEvidence evidence))
          hookPublishFinalEvidence hooks evidence
      }

installStep15WorkProbes :: Step15WorkLedger -> HeraldTcp -> IO ()
installStep15WorkProbes
  (Step15WorkLedger _ configuredSeeds peerDials heartbeats oracleActions oracleManager _)
  tcp = do
    setConfiguredSeedProbeForTest tcp
      $ Just
      $ \event -> atomically (modifyTVar' configuredSeeds (event :))
    setPeerDialProbeForTest tcp
      $ Just
      $ \event -> atomically (modifyTVar' peerDials (event :))
    setHeartbeatProbeForTest tcp
      $ Just
      $ \event -> atomically (modifyTVar' heartbeats (event :))
    setHeraldTcpOracleActionProbe tcp
      $ Just
      $ \event -> atomically (modifyTVar' oracleActions (event :))
    setHeraldTcpOracleManagerProbe tcp
      $ Just
      $ \event -> atomically (modifyTVar' oracleManager (event :))

awaitStep15WorkLedgerReady :: Step15WorkLedger -> IO ()
awaitStep15WorkLedgerReady (Step15WorkLedger traceCell _ _ _ _ _ _) =
  void (atomically (readTMVar traceCell))

awaitStep15FinalEvidence :: Step15WorkLedger -> IO RuntimeTraceFinalEvidence
awaitStep15FinalEvidence (Step15WorkLedger _ _ _ _ _ _ finalEvidence) =
  atomically (readTMVar finalEvidence)

data Step15WorkCursor
  = Step15WorkCursor TraceCursor Int Int Int Int Int
  deriving stock (Eq, Show)

data Step15WorkEvidence
  = Step15WorkEvidence
      [RuntimeTraceEvent]
      [ConfiguredSeedProbeEvent]
      [PeerDialProbeEvent]
      [HeartbeatProbeEvent]
      [OracleClientAction]
      [OracleManagerProbeEvent]
  deriving stock (Eq, Show)

step15RuntimeEvidence :: Step15WorkEvidence -> [RuntimeTraceEvent]
step15RuntimeEvidence (Step15WorkEvidence events _ _ _ _ _) = events

step15ConfiguredSeedEvidence :: Step15WorkEvidence -> [ConfiguredSeedProbeEvent]
step15ConfiguredSeedEvidence (Step15WorkEvidence _ events _ _ _ _) = events

step15PeerDialEvidence :: Step15WorkEvidence -> [PeerDialProbeEvent]
step15PeerDialEvidence (Step15WorkEvidence _ _ events _ _ _) = events

step15HeartbeatEvidence :: Step15WorkEvidence -> [HeartbeatProbeEvent]
step15HeartbeatEvidence (Step15WorkEvidence _ _ _ events _ _) = events

step15OracleActionEvidence :: Step15WorkEvidence -> [OracleClientAction]
step15OracleActionEvidence (Step15WorkEvidence _ _ _ _ events _) = events

step15OracleManagerEvidence :: Step15WorkEvidence -> [OracleManagerProbeEvent]
step15OracleManagerEvidence (Step15WorkEvidence _ _ _ _ _ events) = events

-- | Canonical Oracle projection observations installed by one Herald. A
-- cluster-wide acceptance test may deduplicate equal views across survivor
-- ledgers to distinguish semantic work from fan-out delivery.
step15OracleProjectionViews :: Step15WorkEvidence -> [OracleProjectionEventView]
step15OracleProjectionViews evidence =
  [ oracleProjectionEventView projection
  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- step15RuntimeEvidence evidence,
    OracleInput (OracleEntriesReceived _ entries) <- [inputBody input],
    canonical <- NonEmpty.toList entries,
    projection <-
      appliedEntryProjectionEvents
        (canonicalAppliedOracleEntryValue canonical)
  ]

snapshotStep15WorkCursors :: [Step15WorkLedger] -> IO [Step15WorkCursor]
snapshotStep15WorkCursors ledgers =
  atomically (traverse step15WorkCursor ledgers)

-- | Capture both the complete prefix and its exact suffix cursor in one STM
-- transaction. This lets an acceptance failure distinguish work already in
-- flight before a scenario from work caused after its semantic cut.
snapshotStep15WorkBaselines ::
  [Step15WorkLedger] ->
  IO ([Step15WorkEvidence], [Step15WorkCursor])
snapshotStep15WorkBaselines ledgers =
  atomically $ do
    baselines <-
      traverse
        (\ledger -> (,) <$> step15WorkEvidence ledger <*> step15WorkCursor ledger)
        ledgers
    pure (fmap fst baselines, fmap snd baselines)

snapshotStep15WorkEvidenceSince ::
  [Step15WorkCursor] ->
  [Step15WorkLedger] ->
  IO [(Step15WorkCursor, Step15WorkEvidence)]
snapshotStep15WorkEvidenceSince cursors ledgers
  | length cursors == length ledgers =
      atomically (traverse (uncurry step15WorkEvidenceSince) (zip cursors ledgers))
  | otherwise =
      ioError (userError "Step-15 work cursor/ledger cardinality mismatch")

-- | Block transactionally until the evidence accumulated after the supplied
-- cursors satisfies a semantic completion predicate. Because the transaction
-- reads every underlying trace/probe TVar, it is retried by STM only when new
-- attributable work arrives; callers need neither a polling delay nor a
-- wall-clock notion of progress.
awaitStep15WorkEvidenceSince ::
  [Step15WorkCursor] ->
  [Step15WorkLedger] ->
  ([Step15WorkEvidence] -> Bool) ->
  IO [Step15WorkEvidence]
awaitStep15WorkEvidenceSince cursors ledgers complete
  | length cursors == length ledgers =
      atomically $ do
        evidence <-
          traverse
            (fmap snd . uncurry step15WorkEvidenceSince)
            (zip cursors ledgers)
        check (complete evidence)
        pure evidence
  | otherwise =
      ioError (userError "Step-15 work cursor/ledger cardinality mismatch")

snapshotStep15WorkEvidence :: Step15WorkLedger -> IO Step15WorkEvidence
snapshotStep15WorkEvidence ledger = atomically (step15WorkEvidence ledger)

step15WorkEvidence :: Step15WorkLedger -> STM Step15WorkEvidence
step15WorkEvidence
  (Step15WorkLedger traceCell configuredSeeds peerDials heartbeats oracleActions oracleManager _) =
    do
      trace <- readTMVar traceCell
      Step15WorkEvidence
        <$> snapshotTraceLedger trace
        <*> (reverse <$> readTVar configuredSeeds)
        <*> (reverse <$> readTVar peerDials)
        <*> (reverse <$> readTVar heartbeats)
        <*> (reverse <$> readTVar oracleActions)
        <*> (reverse <$> readTVar oracleManager)

step15KernelFaultEvidence :: Step15WorkEvidence -> [RuntimeTraceEvent]
step15KernelFaultEvidence evidence =
  filter isKernelFault (step15RuntimeEvidence evidence)

step15WorkCursor :: Step15WorkLedger -> STM Step15WorkCursor
step15WorkCursor
  (Step15WorkLedger traceCell configuredSeeds peerDials heartbeats oracleActions oracleManager _) = do
    trace <- readTMVar traceCell
    Step15WorkCursor
      <$> traceLedgerCursor trace
      <*> (length <$> readTVar configuredSeeds)
      <*> (length <$> readTVar peerDials)
      <*> (length <$> readTVar heartbeats)
      <*> (length <$> readTVar oracleActions)
      <*> (length <$> readTVar oracleManager)

step15WorkEvidenceSince ::
  Step15WorkCursor ->
  Step15WorkLedger ->
  STM (Step15WorkCursor, Step15WorkEvidence)
step15WorkEvidenceSince
  (Step15WorkCursor traceCursor configuredSeedCursor peerCursor heartbeatCursor oracleActionCursor oracleCursor)
  (Step15WorkLedger traceCell configuredSeeds peerDials heartbeats oracleActions oracleManager _) = do
    trace <- readTMVar traceCell
    (nextTraceCursor, traceEvents) <- snapshotTraceLedgerSince traceCursor trace
    configuredSeedEvents <- readTVar configuredSeeds
    peerEvents <- readTVar peerDials
    heartbeatEvents <- readTVar heartbeats
    oracleActionEvents <- readTVar oracleActions
    oracleEvents <- readTVar oracleManager
    let (nextConfiguredSeedCursor, configuredSeedSuffix) =
          newestFirstSuffix configuredSeedCursor configuredSeedEvents
        (nextPeerCursor, peerSuffix) = newestFirstSuffix peerCursor peerEvents
        (nextHeartbeatCursor, heartbeatSuffix) =
          newestFirstSuffix heartbeatCursor heartbeatEvents
        (nextOracleActionCursor, oracleActionSuffix) =
          newestFirstSuffix oracleActionCursor oracleActionEvents
        (nextOracleCursor, oracleSuffix) =
          newestFirstSuffix oracleCursor oracleEvents
    pure
      ( Step15WorkCursor
          nextTraceCursor
          nextConfiguredSeedCursor
          nextPeerCursor
          nextHeartbeatCursor
          nextOracleActionCursor
          nextOracleCursor,
        Step15WorkEvidence
          traceEvents
          configuredSeedSuffix
          peerSuffix
          heartbeatSuffix
          oracleActionSuffix
          oracleSuffix
      )

newestFirstSuffix :: Int -> [event] -> (Int, [event])
newestFirstSuffix cursor newestFirst
  | cursor <= next =
      (next, reverse (take (next - cursor) newestFirst))
  | otherwise =
      error "Step-15 work evidence cursor moved beyond its monotone ledger"
  where
    next = length newestFirst

data Step15AttributableWork = Step15AttributableWork
  { step15KernelInputs :: Int,
    step15KernelFaults :: Int,
    step15TimersArmed :: Int,
    step15TimersCancelled :: Int,
    step15TimerOutcomesQueued :: Int,
    step15TimerOutcomesSuppressed :: Int,
    step15IsolationFinishes :: Int,
    step15ConfiguredSeedDialAttempts :: Int,
    step15ConfiguredSeedClaims :: Int,
    step15PeerDialFallbacks :: Int,
    step15PeerDialAttempts :: Int,
    step15PeerDialSuppressions :: Int,
    step15PeerDialCancellations :: Int,
    step15PeerDialSatisfactions :: Int,
    step15HeartbeatSchedules :: Int,
    step15HeartbeatWrites :: Int,
    step15HeartbeatTimeouts :: Int,
    step15OracleActions :: Int,
    step15OracleSubmissions :: Int,
    step15OracleRetries :: Int,
    step15OracleProgressEvents :: Int,
    step15FailureProbeRequests :: Int,
    step15FailureProbeResponses :: Int,
    step15StructuralAppliedReports :: Int,
    step15TerminalSourceInventories :: Int,
    step15TerminalPayloadRequests :: Int,
    step15TerminalOccurrenceReplays :: Int,
    step15TerminalUnionMessages :: Int,
    step15TerminalUnionEstablishedMessages :: Int,
    step15TerminalUnionEstablishedInputs :: Int
  }
  deriving stock (Eq, Show)

step15TimerLifecycleEvents :: Step15AttributableWork -> Int
step15TimerLifecycleEvents work =
  step15TimersArmed work
    + step15TimersCancelled work
    + step15TimerOutcomesQueued work
    + step15TimerOutcomesSuppressed work

step15PeerDialLifecycleEvents :: Step15AttributableWork -> Int
step15PeerDialLifecycleEvents work =
  step15ConfiguredSeedDialAttempts work
    + step15ConfiguredSeedClaims work
    + step15PeerDialFallbacks work
    + step15PeerDialAttempts work
    + step15PeerDialSuppressions work
    + step15PeerDialCancellations work
    + step15PeerDialSatisfactions work

step15HeartbeatLifecycleEvents :: Step15AttributableWork -> Int
step15HeartbeatLifecycleEvents work =
  step15HeartbeatSchedules work
    + step15HeartbeatWrites work
    + step15HeartbeatTimeouts work

step15OracleLifecycleEvents :: Step15AttributableWork -> Int
step15OracleLifecycleEvents work =
  step15OracleActions work
    + step15OracleRetries work
    + step15OracleProgressEvents work

step15TerminalControlEvents :: Step15AttributableWork -> Int
step15TerminalControlEvents work =
  step15TerminalSourceInventories work
    + step15TerminalPayloadRequests work
    + step15TerminalOccurrenceReplays work
    + step15TerminalUnionMessages work

summarizeStep15WorkEvidence :: Step15WorkEvidence -> Step15AttributableWork
summarizeStep15WorkEvidence
  (Step15WorkEvidence traceEvents configuredSeeds peerDials heartbeats oracleActions oracleManager) =
    Step15AttributableWork
      { step15KernelInputs = count isKernelInput traceEvents,
        step15KernelFaults = count isKernelFault traceEvents,
        step15TimersArmed = count isTimerArmed traceEvents,
        step15TimersCancelled = count isTimerCancelled traceEvents,
        step15TimerOutcomesQueued = count isTimerOutcomeQueued traceEvents,
        step15TimerOutcomesSuppressed = count isTimerOutcomeSuppressed traceEvents,
        step15IsolationFinishes = count isIsolationFinish traceEvents,
        step15ConfiguredSeedDialAttempts =
          count (== ConfiguredSeedDialAttempted) configuredSeeds,
        step15ConfiguredSeedClaims = count isConfiguredSeedClaim configuredSeeds,
        step15PeerDialFallbacks = count (peerPhaseIs PeerDialFallbackScheduled) peerDials,
        step15PeerDialAttempts = count (peerPhaseIs PeerDialAttempted) peerDials,
        step15PeerDialSuppressions = count (peerPhaseIs PeerDialSuppressed) peerDials,
        step15PeerDialCancellations = count (peerPhaseIs PeerDialCancelled) peerDials,
        step15PeerDialSatisfactions = count (peerPhaseIs PeerDialSatisfied) peerDials,
        step15HeartbeatSchedules = count isHeartbeatSchedule heartbeats,
        step15HeartbeatWrites = count isHeartbeatWrite heartbeats,
        step15HeartbeatTimeouts = count isHeartbeatTimeout heartbeats,
        step15OracleActions = length oracleActions,
        step15OracleSubmissions = count isOracleSubmission oracleActions,
        step15OracleRetries = count isOracleRetry oracleManager,
        step15OracleProgressEvents = count isOracleProgress oracleManager,
        step15FailureProbeRequests = count (peerControlSent isFailureProbeRequest) traceEvents,
        step15FailureProbeResponses = count (peerControlSent isFailureProbeResponse) traceEvents,
        step15StructuralAppliedReports = count (peerControlSent isStructuralAppliedReport) traceEvents,
        step15TerminalSourceInventories = count (peerControlSent isTerminalSourceInventory) traceEvents,
        step15TerminalPayloadRequests = count (peerControlSent isTerminalPayloadRequest) traceEvents,
        step15TerminalOccurrenceReplays = count (peerControlSent isTerminalPayloadRelay) traceEvents,
        step15TerminalUnionMessages = count (peerControlSent isTerminalUnionMessage) traceEvents,
        step15TerminalUnionEstablishedMessages =
          count (peerControlSent isTerminalUnionEstablished) traceEvents,
        step15TerminalUnionEstablishedInputs =
          count isTerminalUnionEstablishedInput traceEvents
      }

count :: (value -> Bool) -> [value] -> Int
count predicate = length . filter predicate

isKernelInput :: RuntimeTraceEvent -> Bool
isKernelInput = \case
  KernelEvent _ (KernelTraceStepped _ _ _ _) -> True
  _ -> False

isKernelFault :: RuntimeTraceEvent -> Bool
isKernelFault = \case
  KernelEvent _ (KernelTraceStepped _ _ _ (Left _)) -> True
  _ -> False

isTimerArmed :: RuntimeTraceEvent -> Bool
isTimerArmed = \case
  ShellEvent _ (ShellTimerArmed _ _) -> True
  _ -> False

isTimerCancelled :: RuntimeTraceEvent -> Bool
isTimerCancelled = \case
  ShellEvent _ (ShellTimerCancelled _) -> True
  _ -> False

isTimerOutcomeQueued :: RuntimeTraceEvent -> Bool
isTimerOutcomeQueued = \case
  ShellEvent _ (ShellTimerOutcomeQueued _ _) -> True
  _ -> False

isTimerOutcomeSuppressed :: RuntimeTraceEvent -> Bool
isTimerOutcomeSuppressed = \case
  ShellEvent _ (ShellTimerOutcomeSuppressed _ _) -> True
  _ -> False

isIsolationFinish :: RuntimeTraceEvent -> Bool
isIsolationFinish = \case
  ShellEvent _ (ShellEffectRouted _ _ (FinishIsolation _)) -> True
  _ -> False

isConfiguredSeedClaim :: ConfiguredSeedProbeEvent -> Bool
isConfiguredSeedClaim = \case
  ConfiguredSeedSocketClaimResult True -> True
  _ -> False

peerPhaseIs :: PeerDialProbePhase -> PeerDialProbeEvent -> Bool
peerPhaseIs expected (PeerDialProbeEvent _ observed) = expected == observed

isHeartbeatSchedule :: HeartbeatProbeEvent -> Bool
isHeartbeatSchedule (HeartbeatProbeEvent _ phase) = case phase of
  HeartbeatEstablishmentScheduled -> True
  HeartbeatIdleScheduled -> True
  HeartbeatReplyScheduled _ -> True
  _ -> False

isHeartbeatWrite :: HeartbeatProbeEvent -> Bool
isHeartbeatWrite (HeartbeatProbeEvent _ phase) = case phase of
  HeartbeatPingWritten _ -> True
  _ -> False

isHeartbeatTimeout :: HeartbeatProbeEvent -> Bool
isHeartbeatTimeout (HeartbeatProbeEvent _ phase) = case phase of
  HeartbeatEstablishmentTimedOut -> True
  HeartbeatTimedOut _ -> True
  _ -> False

isOracleRetry :: OracleManagerProbeEvent -> Bool
isOracleRetry = \case
  OracleRetryPaced _ _ _ _ -> True
  _ -> False

isOracleProgress :: OracleManagerProbeEvent -> Bool
isOracleProgress = \case
  OracleProgressObserved _ _ -> True
  _ -> False

isOracleSubmission :: OracleClientAction -> Bool
isOracleSubmission = \case
  SubmitOracleRequest {} -> True
  _ -> False

peerControlSent :: (PeerControl -> Bool) -> RuntimeTraceEvent -> Bool
peerControlSent predicate = \case
  ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerControl _ control)) ->
    predicate control
  _ -> False

isFailureProbeRequest :: PeerControl -> Bool
isFailureProbeRequest = \case
  PeerDirectFailureProbeRequested {} -> True
  _ -> False

isFailureProbeResponse :: PeerControl -> Bool
isFailureProbeResponse = \case
  PeerDirectFailureProbeResponded {} -> True
  _ -> False

isStructuralAppliedReport :: PeerControl -> Bool
isStructuralAppliedReport = \case
  PeerStructuralAppliedReported {} -> True
  _ -> False

isTerminalSourceInventory :: PeerControl -> Bool
isTerminalSourceInventory = \case
  PeerTerminalSourceInventoryAdvertised {} -> True
  _ -> False

isTerminalPayloadRequest :: PeerControl -> Bool
isTerminalPayloadRequest = \case
  PeerTerminalSourcePayloadRequested {} -> True
  _ -> False

isTerminalPayloadRelay :: PeerControl -> Bool
isTerminalPayloadRelay = \case
  PeerTerminalSourcePayloadRelayed {} -> True
  _ -> False

isTerminalUnionMessage :: PeerControl -> Bool
isTerminalUnionMessage = \case
  PeerTerminalSourceUnionAnnounced {} -> True
  PeerTerminalSourceUnionAccepted {} -> True
  PeerTerminalSourceUnionEstablished {} -> True
  _ -> False

isTerminalUnionEstablished :: PeerControl -> Bool
isTerminalUnionEstablished = \case
  PeerTerminalSourceUnionEstablished {} -> True
  _ -> False

isTerminalUnionEstablishedInput :: RuntimeTraceEvent -> Bool
isTerminalUnionEstablishedInput = \case
  KernelEvent _ (KernelTraceStepped _ _ input (Right _)) ->
    case inputBody input of
      PeerInput (SemanticPeerControlReceived _ PeerTerminalSourceUnionEstablished {}) -> True
      _ -> False
  _ -> False
