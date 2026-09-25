{-# LANGUAGE OverloadedRecordDot #-}

-- | Outbound-only EORC watch manager. The pure Herald owner mints every
-- attempt, retry, and binding; this module owns only sockets, physical delays,
-- framing, and scoped child cancellation.
module Eclips.Herald.Runtime.TCP.Internal.Oracle
  ( OracleSubmissionTracker,
    emptyOracleSubmissionTracker,
    claimOracleSubmission,
    retireOracleSubmissions,
    oracleSubmissionTrackerRetainedCounts,
    associateOracleSubmissionRetry,
    releaseOracleSubmissionRetries,
    releaseDeferredOracleSubmission,
    claimOracleProgress,
    releaseOracleProgress,
    OracleRetryPacing,
    initialOracleRetryPacing,
    resetOracleRetryPacing,
    advanceOracleRetryPacing,
    advanceOracleRetryPacingWithTiming,
    OracleSubmissionProgress,
    OracleSubmissionProgressEvent (..),
    initialOracleSubmissionProgress,
    observeOracleSubmissionProgress,
    retireOracleSubmissionProgress,
    oracleSubmissionProgressRetainedCounts,
    OracleRetryWorkers,
    newOracleRetryWorkersIO,
    scheduleOracleRetryWorker,
    cancelOracleRetryWorker,
    runOracleManager,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent
  ( ThreadId,
    killThread,
    myThreadId,
    threadDelay,
  )
import Control.Concurrent.MVar
  ( newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTQueue,
    readTVar,
    writeTVar,
  )
import Control.Concurrent.STM qualified as STM
import Control.Exception
  ( IOException,
    finally,
    mask,
    mask_,
    try,
  )
import Control.Monad
  ( forM_,
    unless,
    void,
    when,
  )
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, controlIndex)
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction,
    OracleClientIngress (..),
    OracleConnectAttempt,
    OracleHelloClaims,
    OracleLane,
    OracleObservedTerm,
    OracleRetry,
    OracleRetryPurpose (..),
    oracleCandidateLane,
    oracleConnectAttemptContact,
    oracleContactHost,
    oracleContactPort,
    oracleEstablishedLane,
    oracleHelloAcceptedServiceReady,
    oracleHelloAcceptedTerm,
    oracleHelloMembershipGeneration,
    oracleRequestDispatchEnvelope,
  )
import Eclips.Herald.OracleClient qualified as Client
import Eclips.Herald.Runtime (HeraldRuntime)
import Eclips.Herald.Runtime.Ingress
  ( RuntimeSubmission (Queued),
    submitOracleClientIngress,
  )
import Eclips.Herald.Runtime.Oracle
  ( oracleConnectHelloEnvelope,
    oracleEntriesIngress,
    oracleHelloIngress,
    oracleProgressConfirmedIngress,
    oracleProgressEnvelope,
    oracleProgressNotReadyIngress,
    oracleRedirectIngress,
    oracleRequestEnvelope,
    oracleRequestRetiredIngress,
    oracleSubmissionDeferredIngress,
    oracleSubmissionNotReadyIngress,
    oracleWatchEnvelope,
  )
import Eclips.Herald.Runtime.TCP.Internal.Scope (claimTcpSocket, newTcpSocketKey, releaseTcpSocket)
import Eclips.Herald.Runtime.TCP.Internal.Socket
  ( closeSocketOnce,
    connectTcpEndpoint,
    receiveTcpSocketBytes,
    retireSocketQuietly,
    sendTcpSocketBytesDirect,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( OracleManagerProbeEvent (..),
    OracleSubmissionProgressEvent (..),
    PeerDialRetryDelay (..),
    ResolvedTcpEndpoint (..),
    TcpContext (..),
  )
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    CanonicalOracleEnvelope,
    OracleCanonicalError,
    canonicalOracleEnvelopeValue,
    decodeCanonicalAppliedOracleEntry,
  )
import Eclips.Oracle.Command
  ( oracleCommandProgress,
    oracleCommandReceiptRetirementProgress,
    oracleEnvelopeCommand,
    oracleEnvelopeHomeHeraldEpoch,
    oracleEnvelopeReceiptRetirementProgress,
    oracleEnvelopeRequestId,
  )
import Eclips.Oracle.Identity (OracleClientRequestId, oracleClientRequestHome, oracleClientRequestSequence)
import Eclips.Oracle.Progress (OracleProgress)
import Eclips.Oracle.Receipt
  ( oracleReceiptControlIndex,
    oracleReceiptRequestId,
  )
import Eclips.Protocol.Oracle.Codec (canonicalOracleReceiptDtoValue)
import Eclips.Protocol.Oracle.Frame
  ( OracleIngressContinuation (..),
    OracleIngressDecoder,
    OracleIngressFeedResult (..),
    encodeOracleFrame,
    feedOracleIngress,
    initialOracleIngressDecoder,
    oracleClientIngressContext,
  )
import Eclips.Protocol.Oracle.Types
  ( OracleProtocolEnvelope (..),
    OracleServerMessage,
    canonicalAppliedOracleEntryDtoBytes,
    committedOracleEntryDtos,
  )
import Eclips.Protocol.Oracle.Types qualified as Protocol
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Public.Types.Timing
import Network.Socket (Socket)

data OraclePhysicalPhase
  = OracleCandidate OracleConnectAttempt
  | OracleEstablished OracleBinding
  | OracleRetired OracleLane
  deriving stock (Eq)

data OraclePhysicalLane = OraclePhysicalLane
  { physicalPhase :: TVar OraclePhysicalPhase,
    physicalSubmissions :: TVar (OracleSubmissionTracker OracleRetry),
    physicalProgress :: TVar OracleProgress,
    physicalSocket :: Socket,
    physicalRetire :: IO (),
    physicalOwnerClose :: IO ()
  }

-- | Full progress, rather than its receipt high water, identifies an offer.
-- A delayed NotReady cannot release a newer same-high-water announcement.
claimOracleProgress :: OracleProgress -> OracleProgress -> (OracleProgress, Bool)
claimOracleProgress offered previous = (combined, combined /= previous)
  where
    combined = previous <> offered

releaseOracleProgress :: OracleProgress -> OracleProgress -> OracleProgress
releaseOracleProgress offered previous
  | offered == previous = mempty
  | otherwise = previous

-- | Physical-lane coalescing for level-triggered semantic submissions.
--
-- A request remains submitted until its owner certifies local settlement or
-- this lane is replaced. A monotone per-home floor excludes old queued sends
-- after their individual records are released. A correlated
-- NotReady owner action associates only that request with the exact retry that
-- will release it. Receipts and committed entries deliberately do not reopen
-- the slot; the pure projection simply stops offering that intention.
data OracleSubmissionTracker retry = OracleSubmissionTracker
  { submittedRequests :: Set OracleClientRequestId,
    retryRequests :: [(retry, Set OracleClientRequestId)],
    retiredSubmissions :: Map HeraldEpoch ReceiptRetirement
  }
  deriving stock (Eq, Show)

emptyOracleSubmissionTracker :: OracleSubmissionTracker retry
emptyOracleSubmissionTracker = OracleSubmissionTracker Set.empty [] Map.empty

claimOracleSubmission ::
  OracleClientRequestId ->
  OracleSubmissionTracker retry ->
  (OracleSubmissionTracker retry, Bool)
claimOracleSubmission request tracker
  | requestCovered tracker.retiredSubmissions request
      || Set.member request tracker.submittedRequests =
      (tracker, False)
  | otherwise =
      ( tracker
          { submittedRequests = Set.insert request tracker.submittedRequests
          },
        True
      )

-- | Consume only the pure owner's certified ready frontier, carried by an
-- ordinary dispatch or its standalone idle flush. This floor controls local
-- send eligibility independently of the Oracle's retirement acknowledgement.
retireOracleSubmissions ::
  CanonicalOracleEnvelope ->
  OracleSubmissionTracker retry ->
  OracleSubmissionTracker retry
retireOracleSubmissions canonical tracker =
  case advancingRetirement canonical tracker.retiredSubmissions of
    Nothing -> tracker
    Just frontiers ->
      tracker
        { retiredSubmissions = frontiers,
          submittedRequests = Set.filter (not . requestCovered frontiers) tracker.submittedRequests,
          retryRequests =
            [ (retry, remaining)
            | (retry, requests) <- tracker.retryRequests,
              let remaining = Set.filter (not . requestCovered frontiers) requests,
              not (Set.null remaining)
            ]
        }

-- | Submitted IDs, retry associations and per-home retirement floors.
oracleSubmissionTrackerRetainedCounts :: OracleSubmissionTracker retry -> (Int, Int, Int)
oracleSubmissionTrackerRetainedCounts tracker =
  (Set.size tracker.submittedRequests, sum (map (Set.size . snd) tracker.retryRequests), Map.size tracker.retiredSubmissions)

advancingRetirement :: CanonicalOracleEnvelope -> Map HeraldEpoch ReceiptRetirement -> Maybe (Map HeraldEpoch ReceiptRetirement)
advancingRetirement canonical frontiers = do
  let envelope = canonicalOracleEnvelopeValue canonical
      home = oracleEnvelopeHomeHeraldEpoch envelope
  through <- oracleEnvelopeReceiptRetirementProgress envelope <|> oracleCommandReceiptRetirementProgress (oracleEnvelopeCommand envelope)
  let previous = Map.findWithDefault mempty home frontiers
      combined = previous <> through
  if combined /= previous
    then Just (Map.insert home combined frontiers)
    else Nothing

requestCovered :: Map HeraldEpoch ReceiptRetirement -> OracleClientRequestId -> Bool
requestCovered frontiers request =
  case Map.lookup (oracleClientRequestHome request) frontiers of
    Just through -> Lifetime.receiptIsRetired (oracleClientRequestSequence request) through
    Nothing -> False

associateOracleSubmissionRetry ::
  (Eq retry) =>
  OracleClientRequestId ->
  retry ->
  OracleSubmissionTracker retry ->
  OracleSubmissionTracker retry
associateOracleSubmissionRetry request retry tracker
  | Set.member request tracker.submittedRequests =
      tracker
        { retryRequests = associate tracker.retryRequests
        }
  | otherwise = tracker
  where
    associate [] = [(retry, Set.singleton request)]
    associate (current@(associatedRetry, requests) : remaining)
      | associatedRetry == retry =
          (associatedRetry, Set.insert request requests) : remaining
      | otherwise = current : associate remaining

releaseOracleSubmissionRetries ::
  (Eq retry) =>
  retry ->
  OracleSubmissionTracker retry ->
  OracleSubmissionTracker retry
releaseOracleSubmissionRetries retry tracker
  | Set.null released = tracker
  | otherwise =
      tracker
        { submittedRequests = tracker.submittedRequests `Set.difference` released,
          retryRequests = retained
        }
  where
    (released, retained) = foldr partition (Set.empty, []) tracker.retryRequests
    partition current@(associatedRetry, requests) (releasedRequests, retainedRetries)
      | associatedRetry == retry =
          (Set.union requests releasedRequests, retainedRetries)
      | otherwise = (releasedRequests, current : retainedRetries)

releaseDeferredOracleSubmission ::
  OracleClientRequestId ->
  OracleSubmissionTracker retry ->
  OracleSubmissionTracker retry
releaseDeferredOracleSubmission request tracker =
  tracker
    { submittedRequests = Set.delete request tracker.submittedRequests,
      retryRequests =
        [ (retry, retained)
        | (retry, requests) <- tracker.retryRequests,
          let retained = Set.delete request requests,
          not (Set.null retained)
        ]
    }

data OracleManagerState = OracleManagerState
  { managerLanes :: TVar [OraclePhysicalLane],
    managerRetries :: OracleRetryWorkers OracleRetry,
    managerProgressRetries :: OracleRetryWorkers OracleBinding,
    managerReportedMissingBindings :: TVar (Set OracleBinding),
    managerRetryPacing :: TVar OracleRetryPacing,
    managerSubmissionProgress :: TVar OracleSubmissionProgress
  }

-- | Live physical timer workers indexed by their owner-minted retry token.
--
-- Firing is a claimed transition: the worker removes its exact lease before
-- exposing the elapsed ingress. Consequently the owner's FIFO Cancel action
-- cannot synchronously throw to the worker which produced that input and
-- stall the sole Oracle manager ahead of the following reconnect action.
newtype OracleRetryWorkers retry
  = OracleRetryWorkers (TVar [(retry, ThreadId)])

newOracleRetryWorkersIO :: IO (OracleRetryWorkers retry)
newOracleRetryWorkersIO = OracleRetryWorkers <$> newTVarIO []

-- | Install one cancellable delay behind a start gate, then expose its elapsed
-- action only if that exact worker wins the race with cancellation. The gate
-- closes the child-start-before-registration window. Direct scope cancellation
-- and delay exceptions remove the same lease in the worker finalizer.
scheduleOracleRetryWorker ::
  (Eq retry) =>
  (IO () -> IO (Maybe ThreadId)) ->
  IO () ->
  IO () ->
  OracleRetryWorkers retry ->
  retry ->
  IO ()
scheduleOracleRetryWorker spawn waitForDelay fire workers retry = mask_ $ do
  cancelOracleRetryWorker workers retry
  start <- newEmptyMVar
  started <-
    spawn $ do
      thread <- myThreadId
      ( do
          takeMVar start
          waitForDelay
          claimed <- atomically (claimOracleRetryWorker workers retry thread)
          when claimed fire
        )
        `finally` atomically (releaseOracleRetryWorker workers retry thread)
  forM_ started $ \thread -> do
    atomically (registerOracleRetryWorker workers retry thread)
    putMVar start ()

-- | Cancel every still-live worker for one exact logical retry. Removing the
-- leases and only then throwing makes cancellation idempotent with a worker
-- which has already claimed its elapsed event.
cancelOracleRetryWorker ::
  (Eq retry) =>
  OracleRetryWorkers retry ->
  retry ->
  IO ()
cancelOracleRetryWorker (OracleRetryWorkers registry) retry = do
  threads <- atomically $ do
    retained <- readTVar registry
    writeTVar registry (filter ((/= retry) . fst) retained)
    pure [thread | (current, thread) <- retained, current == retry]
  mapM_ killThread threads

registerOracleRetryWorker ::
  OracleRetryWorkers retry ->
  retry ->
  ThreadId ->
  STM ()
registerOracleRetryWorker (OracleRetryWorkers registry) retry thread =
  modifyTVar' registry ((retry, thread) :)

claimOracleRetryWorker ::
  (Eq retry) =>
  OracleRetryWorkers retry ->
  retry ->
  ThreadId ->
  STM Bool
claimOracleRetryWorker workers retry thread = do
  claimed <- oracleRetryWorkerRegistered workers retry thread
  when claimed (releaseOracleRetryWorker workers retry thread)
  pure claimed

releaseOracleRetryWorker ::
  (Eq retry) =>
  OracleRetryWorkers retry ->
  retry ->
  ThreadId ->
  STM ()
releaseOracleRetryWorker (OracleRetryWorkers registry) retry thread =
  modifyTVar'
    registry
    (filter (\(current, worker) -> current /= retry || worker /= thread))

oracleRetryWorkerRegistered ::
  (Eq retry) =>
  OracleRetryWorkers retry ->
  retry ->
  ThreadId ->
  STM Bool
oracleRetryWorkerRegistered (OracleRetryWorkers registry) retry thread =
  elem (retry, thread) <$> readTVar registry

-- | Independent fruitless-round streaks for physical connection and semantic
-- submission retries. Neither class can accelerate or forgive the other.
data OracleRetryPacing = OracleRetryPacing Word64 Word64
  deriving stock (Eq, Show)

initialOracleRetryPacing :: OracleRetryPacing
initialOracleRetryPacing = OracleRetryPacing 0 0

resetOracleRetryPacing :: OracleRetryPurpose -> OracleRetryPacing -> OracleRetryPacing
resetOracleRetryPacing retryClass (OracleRetryPacing connectionStreak submissionStreak) =
  case retryClass of
    OracleConnectionRetry -> OracleRetryPacing 0 submissionStreak
    OracleSubmissionRetry -> OracleRetryPacing connectionStreak 0

-- | Bound repeated physical work while an Oracle cluster makes no useful
-- progress. This is prototype work coalescing, not endpoint scoring or a
-- production retry policy. Each retry class advances only its own streak. The
-- connection class retains the configured delay with a one-microsecond floor.
-- Submission retry is a no-progress fallback behind the exact watch, so its
-- 100-millisecond floor prevents ordinary Raft apply latency from becoming a
-- ten-hertz poll. Both classes retain the same one-second-or-configured cap.
advanceOracleRetryPacing ::
  OracleRetryPurpose ->
  PeerDialRetryDelay ->
  OracleRetryPacing ->
  (OracleRetryPacing, Word64)
advanceOracleRetryPacing retryClass (PeerDialRetryDelay configured) pacing =
  advanceRetryWithBounds (max 1 configured) (max 100_000 configured) (max 1_000_000 configured) retryClass pacing

-- | Scale both independent retry classes with the deployment target. The
-- configured cap replaces the historical one-second fallback.
advanceOracleRetryPacingWithTiming :: TimingPolicy -> OracleRetryPurpose -> OracleRetryPacing -> (OracleRetryPacing, Word64)
advanceOracleRetryPacingWithTiming policy =
  advanceRetryWithBounds (timingReconnectInitialMicroseconds policy) (timingSubmissionInitialMicroseconds policy) (timingRetryCapMicroseconds policy)

advanceRetryWithBounds :: Word64 -> Word64 -> Word64 -> OracleRetryPurpose -> OracleRetryPacing -> (OracleRetryPacing, Word64)
advanceRetryWithBounds connectionBase submissionBase cap retryClass pacing =
  (successor, fromInteger delay)
  where
    base = case retryClass of
      OracleConnectionRetry -> connectionBase
      OracleSubmissionRetry -> submissionBase
    baseInteger = toInteger base
    maximumDelay = toInteger cap
    OracleRetryPacing connectionStreak submissionStreak = pacing
    streak = case retryClass of
      OracleConnectionRetry -> connectionStreak
      OracleSubmissionRetry -> submissionStreak
    -- Twenty doublings reach the cap even for a one-microsecond base;
    -- bounding the exponent also keeps the calculation small.
    multiplier = 2 ^ min 20 streak :: Integer
    delay = min maximumDelay (baseInteger * multiplier)
    successor = case retryClass of
      OracleConnectionRetry ->
        OracleRetryPacing (connectionStreak + 1) submissionStreak
      OracleSubmissionRetry ->
        OracleRetryPacing connectionStreak (submissionStreak + 1)

data OracleSubmissionProgress = OracleSubmissionProgress
  { greatestCommittedPrefix :: Maybe ControlIndex,
    settledRequests :: Set OracleClientRequestId,
    retiredSettlements :: Map HeraldEpoch ReceiptRetirement,
    greatestServiceReadyLeaderTerm :: Maybe OracleObservedTerm
  }
  deriving stock (Eq, Show)

initialOracleSubmissionProgress :: OracleSubmissionProgress
initialOracleSubmissionProgress =
  OracleSubmissionProgress
    { greatestCommittedPrefix = Just (controlIndex 0),
      settledRequests = Set.empty,
      retiredSettlements = Map.empty,
      greatestServiceReadyLeaderTerm = Nothing
    }

-- | Admit an event only when it adds new useful Oracle evidence. Duplicate or
-- regressed prefixes and terms, and duplicate exact settlements, retain the
-- tracker unchanged and therefore cannot reset submission pacing.
observeOracleSubmissionProgress ::
  OracleSubmissionProgressEvent ->
  OracleSubmissionProgress ->
  (OracleSubmissionProgress, Bool)
observeOracleSubmissionProgress event predecessor = case event of
  OracleCommittedPrefixProgress prefix
    | maybe True (< prefix) predecessor.greatestCommittedPrefix ->
        (predecessor {greatestCommittedPrefix = Just prefix}, True)
  OracleTerminalRequestProgress request
    | not (requestCovered predecessor.retiredSettlements request),
      Set.notMember request predecessor.settledRequests ->
        ( predecessor
            { settledRequests = Set.insert request predecessor.settledRequests
            },
          True
        )
  OracleServiceReadyLeaderProgress term
    | maybe True (< term) predecessor.greatestServiceReadyLeaderTerm ->
        (predecessor {greatestServiceReadyLeaderTerm = Just term}, True)
  _ -> (predecessor, False)

-- | Forget per-request progress evidence without letting a delayed receipt
-- count as new progress. Control-prefix and service-ready-term evidence keep
-- their independent meanings; retiring local records is not itself progress.
retireOracleSubmissionProgress :: CanonicalOracleEnvelope -> OracleSubmissionProgress -> OracleSubmissionProgress
retireOracleSubmissionProgress canonical progress =
  case advancingRetirement canonical progress.retiredSettlements of
    Nothing -> progress
    Just frontiers ->
      progress
        { retiredSettlements = frontiers,
          settledRequests = Set.filter (not . requestCovered frontiers) progress.settledRequests
        }

-- | Settled request IDs and per-home retirement floors.
oracleSubmissionProgressRetainedCounts :: OracleSubmissionProgress -> (Int, Int)
oracleSubmissionProgressRetainedCounts progress =
  (Set.size progress.settledRequests, Map.size progress.retiredSettlements)

-- | Interpret the FIFO action stream until the surrounding TCP scope stops.
runOracleManager ::
  (IO () -> IO (Maybe ThreadId)) ->
  TcpContext ->
  HeraldRuntime ->
  PeerDialRetryDelay ->
  IO ()
runOracleManager spawn context runtime retryDelay = do
  state <-
    OracleManagerState
      <$> newTVarIO []
      <*> newOracleRetryWorkersIO
      <*> newOracleRetryWorkersIO
      <*> newTVarIO Set.empty
      <*> newTVarIO initialOracleRetryPacing
      <*> newTVarIO initialOracleSubmissionProgress
  let loop = do
        action <- atomically (readTQueue context.tcpOracleActions)
        probe <- atomically (readTVar context.tcpOracleActionProbe)
        forM_ probe ($ action)
        handleAction spawn context runtime retryDelay state action
        loop
  loop

handleAction ::
  (IO () -> IO (Maybe ThreadId)) ->
  TcpContext ->
  HeraldRuntime ->
  PeerDialRetryDelay ->
  OracleManagerState ->
  OracleClientAction ->
  IO ()
handleAction spawn context runtime retryDelay state = \case
  Client.ConnectAndHelloOracle attempt claims ->
    void (spawn (runConnect context runtime state attempt claims))
  Client.BindOracleConnection attempt binding ->
    atomically $ do
      lanes <- readTVar state.managerLanes
      candidate <- findLaneSTM (== OracleCandidate attempt) lanes
      forM_ candidate $ \lane -> do
        writeTVar lane.physicalPhase (OracleEstablished binding)
        modifyTVar'
          state.managerRetryPacing
          (resetOracleRetryPacing OracleConnectionRetry)
        -- Only a physical candidate which actually became established renews
        -- this logical binding's missing-lane reporting lease. A stale Bind
        -- action must not clear the loss already claimed by finalization.
        modifyTVar' state.managerReportedMissingBindings (Set.delete binding)
  Client.RejectOracleConnection lane -> closeLane state lane
  Client.WatchOracle binding cursor -> do
    -- The pure owner emits its watch only after applying the corresponding
    -- contiguous prefix. Reset pacing here, in owner-effect order, rather than
    -- when the socket reader merely queues an entry frame.
    recordOracleSubmissionProgress
      context
      state
      [OracleCommittedPrefixProgress cursor]
    offered <- withEstablishedLane state binding $ \physical ->
      do
        sent <-
          try
            ( sendEnvelope
                physical.physicalSocket
                (oracleWatchEnvelope cursor)
            ) ::
            IO (Either IOException ())
        case sent of
          Left _ -> physical.physicalRetire
          Right () -> pure ()
    unless offered (reportMissingBinding runtime state binding)
  Client.SubmitOracleRequest binding dispatch -> do
    let canonical = oracleRequestDispatchEnvelope dispatch
        request =
          oracleEnvelopeRequestId
            (canonicalOracleEnvelopeValue canonical)
    offered <- withEstablishedLane state binding $ \physical -> do
      shouldSend <- atomically $ do
        retirePhysicalSubmissions state physical canonical
        tracker <- readTVar physical.physicalSubmissions
        let (successor, claimed) = claimOracleSubmission request tracker
        writeTVar physical.physicalSubmissions successor
        pure claimed
      if shouldSend
        then do
          notifyOracleManagerProbe context (OraclePhysicalSubmitAttempt binding request)
          sent <-
            try
              ( sendEnvelope
                  physical.physicalSocket
                  (oracleRequestEnvelope dispatch)
              ) ::
              IO (Either IOException ())
          case sent of
            Left _ -> physical.physicalRetire
            Right () -> pure ()
        else pure ()
    unless offered (reportMissingBinding runtime state binding)
  Client.ReleaseDeferredOracleSubmission binding request -> do
    offered <- withEstablishedLane state binding $ \physical ->
      atomically
        ( modifyTVar'
            physical.physicalSubmissions
            (releaseDeferredOracleSubmission request)
        )
    unless offered (reportMissingBinding runtime state binding)
  Client.SubmitOracleProgress binding canonical -> do
    let through = maybe mempty id (oracleCommandProgress (oracleEnvelopeCommand (canonicalOracleEnvelopeValue canonical)))
    offered <- withEstablishedLane state binding $ \physical -> do
      shouldSend <- atomically $ do
        retirePhysicalSubmissions state physical canonical
        previous <- readTVar physical.physicalProgress
        let (updated, send) = claimOracleProgress through previous
        writeTVar physical.physicalProgress updated
        pure send
      when shouldSend $ do
        sent <- try (sendEnvelope physical.physicalSocket (oracleProgressEnvelope canonical)) :: IO (Either IOException ())
        case sent of
          Left _ -> physical.physicalRetire
          Right () -> pure ()
    unless offered (reportMissingBinding runtime state binding)
  Client.ScheduleOracleProgress binding -> do
    -- One outstanding flush per binding. Later requests piggyback the newer
    -- frontier without postponing this idle fallback indefinitely.
    let workers@(OracleRetryWorkers registry) = state.managerProgressRetries
        PeerDialRetryDelay microseconds = retryDelay
    pending <- atomically (any ((== binding) . fst) <$> readTVar registry)
    unless pending
      $ scheduleOracleRetryWorker
        spawn
        (threadDelay (fromIntegral microseconds))
        (void (submitOracleClientIngress runtime (OracleProgressFlush binding)))
        workers
        binding
  Client.ReleaseOracleProgress binding through -> do
    offered <- withEstablishedLane state binding $ \physical -> atomically $ do
      modifyTVar' physical.physicalProgress (releaseOracleProgress through)
    unless offered (reportMissingBinding runtime state binding)
  Client.AssociateOracleSubmissionRetry binding request retry -> do
    offered <- withEstablishedLane state binding $ \physical ->
      atomically
        ( modifyTVar'
            physical.physicalSubmissions
            (associateOracleSubmissionRetry request retry)
        )
    unless offered (reportMissingBinding runtime state binding)
  Client.ScheduleOracleRetry purpose retry -> do
    (pacedDelay, coalescedRequests) <- atomically $ do
      pacing <- readTVar state.managerRetryPacing
      let (successor, microseconds) =
            case context.tcpTimingPolicy of
              Nothing -> advanceOracleRetryPacing purpose retryDelay pacing
              Just policy -> advanceOracleRetryPacingWithTiming policy purpose pacing
      writeTVar state.managerRetryPacing successor
      associated <- associatedOracleSubmissionCount state retry
      pure (PeerDialRetryDelay microseconds, associated)
    let PeerDialRetryDelay microseconds = pacedDelay
    notifyOracleManagerProbe
      context
      (OracleRetryPaced purpose retry microseconds coalescedRequests)
    scheduleRetry spawn runtime pacedDelay state.managerRetries retry
  Client.CancelOracleRetry retry -> do
    cancelOracleRetryWorker state.managerRetries retry
    atomically (releaseNotReadySubmissions state retry)
  Client.ReportOracleClientDiagnostic {} -> pure ()

retirePhysicalSubmissions :: OracleManagerState -> OraclePhysicalLane -> CanonicalOracleEnvelope -> STM ()
retirePhysicalSubmissions state physical canonical = do
  modifyTVar' physical.physicalSubmissions (retireOracleSubmissions canonical)
  modifyTVar' state.managerSubmissionProgress (retireOracleSubmissionProgress canonical)

runConnect ::
  TcpContext ->
  HeraldRuntime ->
  OracleManagerState ->
  OracleConnectAttempt ->
  OracleHelloClaims ->
  IO ()
runConnect context runtime state attempt claims = mask $ \restore -> do
  let contact = oracleConnectAttemptContact attempt
      endpoint = ResolvedTcpEndpoint (oracleContactHost contact) (oracleContactPort contact)
  notifyOracleManagerProbe context (OraclePhysicalConnectAttempt attempt)
  connected <- try (restore (connectTcpEndpoint endpoint)) :: IO (Either IOException Socket)
  case connected of
    Left _ -> submit (OracleConnectFailed attempt)
    Right socketHandle -> do
      closed <- newTVarIO False
      socketKey <- newTcpSocketKey
      done <- STM.newEmptyTMVarIO
      phase <- newTVarIO (OracleCandidate attempt)
      submissions <- newTVarIO emptyOracleSubmissionTracker
      retirement <- newTVarIO mempty
      let ownerClose = closeSocketOnce closed socketHandle
          retire = do
            retireSocketQuietly socketHandle
            atomically (STM.readTMVar done)
            -- The reader normally closes first. This retained retry covers an
            -- interrupted owner close, but can only release the descriptor
            -- after that reader's complete finalizer has left the lane.
            ownerClose
            releaseTcpSocket context socketKey
          retireForScope = do
            atomically $ do
              stopping <- readTVar context.tcpStopping
              -- Whole-scope stop is an intentional retirement, and changing
              -- the phase also wakes a candidate parked in Hello disposition.
              -- Closing this retained lease while the scope is still live is
              -- instead a physical loss; its established phase must reach the
              -- finalizer so the pure owner receives OracleBindingLost.
              when stopping $ do
                observed <- readTVar phase
                writeTVar phase (OracleRetired (phaseLane observed))
            retire
          lane = OraclePhysicalLane phase submissions retirement socketHandle retire ownerClose
      claimed <- claimTcpSocket context socketKey retireForScope
      if not claimed
        then ownerClose >> submit (OracleConnectFailed attempt)
        else do
          atomically (modifyTVar' state.managerLanes (lane :))
          restore (runInstalledLane socketHandle lane)
            `finally` ( finalizeLane runtime state lane
                          `finally` do
                            physicallyClosed <- atomically (readTVar closed)
                            when physicallyClosed (releaseTcpSocket context socketKey)
                            atomically (void (STM.tryPutTMVar done ()))
                      )
  where
    submit ingress = void (submitOracleClientIngress runtime ingress)

    runInstalledLane socketHandle lane = do
      let helloBytes =
            encodeOracleFrame
              ( oracleConnectHelloEnvelope
                  attempt
                  (oracleHelloMembershipGeneration claims)
                  claims
              )
      sent <-
        try
          (sendTcpSocketBytesDirect socketHandle helloBytes) ::
          IO (Either IOException ())
      case sent of
        Left _ -> pure ()
        Right () ->
          runReader
            context
            runtime
            state
            lane
            (initialOracleIngressDecoder oracleClientIngressContext)

runReader ::
  TcpContext ->
  HeraldRuntime ->
  OracleManagerState ->
  OraclePhysicalLane ->
  OracleIngressDecoder ->
  IO ()
runReader context runtime state lane decoder = do
  chunk <- receiveTcpSocketBytes lane.physicalSocket 65_536
  if ByteString.null chunk
    then pure ()
    else case feedOracleIngress decoder chunk of
      OracleIngressFeedResult envelopes continuation -> do
        accepted <- processEnvelopes context runtime state lane envelopes
        if not accepted
          then pure ()
          else case continuation of
            NeedOracleIngressBytes successor -> runReader context runtime state lane successor
            OracleIngressRedirected _ -> pure ()
            OracleIngressFailed _ -> pure ()

processEnvelopes ::
  TcpContext ->
  HeraldRuntime ->
  OracleManagerState ->
  OraclePhysicalLane ->
  [OracleProtocolEnvelope] ->
  IO Bool
processEnvelopes _ _ _ _ [] = pure True
processEnvelopes context runtime state lane (envelope : remaining) = do
  accepted <- processEnvelope context runtime state lane envelope
  if accepted
    then processEnvelopes context runtime state lane remaining
    else pure False

processEnvelope ::
  TcpContext ->
  HeraldRuntime ->
  OracleManagerState ->
  OraclePhysicalLane ->
  OracleProtocolEnvelope ->
  IO Bool
processEnvelope context runtime state lane = \case
  OracleServerEnvelope message -> processServerMessage context runtime state lane message
  OracleClientEnvelope _ -> pure False

processServerMessage ::
  TcpContext ->
  HeraldRuntime ->
  OracleManagerState ->
  OraclePhysicalLane ->
  OracleServerMessage ->
  IO Bool
processServerMessage context runtime state lane = \case
  Protocol.OracleHealthReply _ -> pure False
  Protocol.OracleHelloAccepted accepted ->
    withCandidatePhase lane $ \attempt ->
      case oracleHelloIngress attempt accepted of
        Left _ -> pure False
        Right ingress@(OracleHelloReceived _ acceptance) -> do
          submitted <- submitIngress runtime ingress
          if not submitted
            then pure False
            else do
              admitted <- awaitHelloDisposition lane attempt
              when (admitted && oracleHelloAcceptedServiceReady acceptance) $ do
                recordOracleSubmissionProgress
                  context
                  state
                  [OracleServiceReadyLeaderProgress (oracleHelloAcceptedTerm acceptance)]
              pure admitted
        Right _ -> pure False
  Protocol.OracleRedirect redirected ->
    withAnyPhase lane $ \phase -> do
      case oracleRedirectIngress (phaseLane phase) redirected of
        Left _ -> pure False
        Right ingress -> do
          retired <- atomically (retireForRedirect lane phase)
          if retired
            then do
              notifyOracleManagerProbe context (OraclePhysicalRedirect (phaseLane phase))
              submitIngress runtime ingress
            else pure False
  Protocol.CommittedOracleEntries entries ->
    withEstablishedPhase lane $ \binding -> case decodeEntries entries of
      Left _ -> pure False
      Right decoded -> do
        submitIngress runtime (oracleEntriesIngress binding decoded)
  Protocol.OracleReceipt dto -> case canonicalOracleReceiptDtoValue dto of
    Left _ -> pure False
    Right receipt -> do
      tracked <- atomically $ do
        submissions <- readTVar lane.physicalSubmissions
        pure
          ( Set.member
              (oracleReceiptRequestId receipt)
              submissions.submittedRequests
          )
      when tracked
        $ recordOracleSubmissionProgress
          context
          state
          [ OracleCommittedPrefixProgress (oracleReceiptControlIndex receipt),
            OracleTerminalRequestProgress (oracleReceiptRequestId receipt)
          ]
      pure True
  Protocol.OracleRequestAbsent {} -> pure True
  Protocol.OracleProgressRetired request through ->
    withEstablishedPhase lane $ \binding ->
      either (const (pure False)) (submitIngress runtime) (oracleProgressConfirmedIngress binding request through)
  Protocol.OracleProgressNotReady request offered term ->
    withEstablishedPhase lane $ \binding ->
      either (const (pure False)) (submitIngress runtime) (oracleProgressNotReadyIngress binding request offered term)
  Protocol.OracleRequestRetired request reason ->
    withEstablishedPhase lane $ \binding ->
      either (const (pure False)) (submitIngress runtime) (oracleRequestRetiredIngress binding request reason)
  Protocol.OracleSubmissionDeferred request decision observedIndex ->
    withEstablishedPhase lane $ \binding ->
      case oracleSubmissionDeferredIngress binding request decision observedIndex of
        Left _ -> pure False
        Right ingress@(OracleSubmissionDeferredReceived _ identifier _ _) ->
          submitUnlessRetired lane identifier (submitIngress runtime ingress)
        Right _ -> pure False
  Protocol.OracleSubmissionNotReady request term ->
    withEstablishedPhase lane $ \binding ->
      case oracleSubmissionNotReadyIngress binding request term of
        Left _ -> pure False
        Right ingress@(OracleSubmissionNotReadyReceived _ identifier _) ->
          submitUnlessRetired lane identifier (submitIngress runtime ingress)
        Right _ -> pure False

-- Obsolete ordinary outcomes cannot rearm a physical retry. A concurrent
-- frontier advance after this check is still safe: the owner ignores settled
-- requests, and the monotone floor rejects any already-queued release/reoffer.
-- Maintenance NotReady remains separate because its request ID aliases the
-- retired frontier and must still release the standalone offer.
submitUnlessRetired :: OraclePhysicalLane -> OracleClientRequestId -> IO Bool -> IO Bool
submitUnlessRetired lane request submit = do
  retired <- atomically $ do
    tracker <- readTVar lane.physicalSubmissions
    pure (requestCovered tracker.retiredSubmissions request)
  if retired then pure True else submit

withCandidatePhase :: OraclePhysicalLane -> (OracleConnectAttempt -> IO Bool) -> IO Bool
withCandidatePhase lane action = do
  phase <- atomically (readTVar lane.physicalPhase)
  case phase of
    OracleCandidate attempt -> action attempt
    OracleEstablished _ -> pure False
    OracleRetired _ -> pure False

withEstablishedPhase :: OraclePhysicalLane -> (OracleBinding -> IO Bool) -> IO Bool
withEstablishedPhase lane action = do
  phase <- atomically (readTVar lane.physicalPhase)
  case phase of
    OracleCandidate _ -> pure False
    OracleEstablished binding -> action binding
    OracleRetired _ -> pure False

withAnyPhase :: OraclePhysicalLane -> (OraclePhysicalPhase -> IO Bool) -> IO Bool
withAnyPhase lane action = atomically (readTVar lane.physicalPhase) >>= action

-- Claim retirement before exposing the redirect to the owner. This makes the
-- reader finalizer observe Retired even if the owner immediately interprets
-- the resulting Reject action and closes the socket.
retireForRedirect :: OraclePhysicalLane -> OraclePhysicalPhase -> STM Bool
retireForRedirect lane expected = do
  current <- readTVar lane.physicalPhase
  case expected of
    OracleRetired _ -> pure False
    _
      | current == expected -> do
          writeTVar lane.physicalPhase (OracleRetired (phaseLane expected))
          pure True
      | otherwise -> pure False

-- The server may coalesce its accepted Hello with later frames. Those frames
-- belong to the established logical lane only after the serialized owner has
-- accepted the Hello and returned its bind action.
awaitHelloDisposition :: OraclePhysicalLane -> OracleConnectAttempt -> IO Bool
awaitHelloDisposition lane expected =
  atomically $ do
    phase <- readTVar lane.physicalPhase
    case phase of
      OracleCandidate current
        | current == expected -> STM.retry
        | otherwise -> pure False
      OracleEstablished _ -> pure True
      OracleRetired _ -> pure False

submitIngress :: HeraldRuntime -> OracleClientIngress -> IO Bool
submitIngress runtime ingress = do
  result <- submitOracleClientIngress runtime ingress
  pure (result == Queued)

recordOracleSubmissionProgress ::
  TcpContext ->
  OracleManagerState ->
  [OracleSubmissionProgressEvent] ->
  IO ()
recordOracleSubmissionProgress context state events = do
  observed <- atomically $ do
    predecessor <- readTVar state.managerSubmissionProgress
    let (successor, useful, decisions) =
          foldl observe (predecessor, False, []) events
    writeTVar state.managerSubmissionProgress successor
    when useful
      $ modifyTVar'
        state.managerRetryPacing
        (resetOracleRetryPacing OracleSubmissionRetry)
    pure (reverse decisions)
  forM_ observed
    $ \(event, newlyUseful) ->
      notifyOracleManagerProbe
        context
        (OracleProgressObserved event newlyUseful)
  where
    observe (progress, anyUseful, decisions) event =
      let (successor, newlyUseful) =
            observeOracleSubmissionProgress event progress
       in ( successor,
            anyUseful || newlyUseful,
            (event, newlyUseful) : decisions
          )

associatedOracleSubmissionCount ::
  OracleManagerState ->
  OracleRetry ->
  STM Int
associatedOracleSubmissionCount state retry = do
  lanes <- readTVar state.managerLanes
  sum <$> traverse associated lanes
  where
    associated lane = do
      phase <- readTVar lane.physicalPhase
      case phase of
        OracleEstablished _ -> do
          tracker <- readTVar lane.physicalSubmissions
          pure
            ( sum
                [ Set.size requests
                | (associatedRetry, requests) <- tracker.retryRequests,
                  associatedRetry == retry
                ]
            )
        _ -> pure 0

notifyOracleManagerProbe ::
  TcpContext ->
  OracleManagerProbeEvent ->
  IO ()
notifyOracleManagerProbe context event = do
  probe <- atomically (readTVar context.tcpOracleManagerProbe)
  forM_ probe ($ event)

decodeEntries :: Protocol.CommittedOracleEntriesDto -> Either OracleCanonicalError (NonEmpty CanonicalAppliedOracleEntry)
decodeEntries =
  traverse
    (decodeCanonicalAppliedOracleEntry . canonicalAppliedOracleEntryDtoBytes)
    . committedOracleEntryDtos

sendEnvelope :: Socket -> OracleProtocolEnvelope -> IO ()
sendEnvelope socketHandle envelope =
  sendTcpSocketBytesDirect
    socketHandle
    (encodeOracleFrame envelope)

finalizeLane ::
  HeraldRuntime ->
  OracleManagerState ->
  OraclePhysicalLane ->
  IO ()
finalizeLane runtime state lane = do
  lane.physicalOwnerClose
  ingress <- atomically $ do
    observed <- readTVar lane.physicalPhase
    modifyTVar' state.managerLanes (filter ((/= lane.physicalSocket) . physicalSocket))
    case observed of
      OracleCandidate candidate ->
        pure (Just (OracleConnectFailed candidate))
      OracleEstablished binding -> do
        firstReport <- claimMissingBindingReport state binding
        pure (if firstReport then Just (OracleBindingLost binding) else Nothing)
      OracleRetired _ -> pure Nothing
  forM_ ingress (void . submitOracleClientIngress runtime)

closeLane :: OracleManagerState -> OracleLane -> IO ()
closeLane state target = do
  matching <- atomically $ do
    lanes <- readTVar state.managerLanes
    retireMatching target lanes
  mapM_ physicalRetire matching

retireMatching :: OracleLane -> [OraclePhysicalLane] -> STM [OraclePhysicalLane]
retireMatching _ [] = pure []
retireMatching target (lane : remaining) = do
  phase <- readTVar lane.physicalPhase
  retained <- retireMatching target remaining
  if phaseLane phase == target
    then writeTVar lane.physicalPhase (OracleRetired target) >> pure (lane : retained)
    else pure retained

withEstablishedLane :: OracleManagerState -> OracleBinding -> (OraclePhysicalLane -> IO ()) -> IO Bool
withEstablishedLane state binding action = do
  lane <- atomically $ do
    lanes <- readTVar state.managerLanes
    findLaneSTM (== OracleEstablished binding) lanes
  case lane of
    Nothing -> pure False
    Just physical -> action physical >> pure True

reportMissingBinding :: HeraldRuntime -> OracleManagerState -> OracleBinding -> IO ()
reportMissingBinding runtime state binding = do
  firstReport <- atomically (claimMissingBindingReport state binding)
  whenFirst firstReport
  where
    whenFirst True = void (submitOracleClientIngress runtime (OracleBindingLost binding))
    whenFirst False = pure ()

-- | Claim the one physical-loss report for a logical binding. Lane removal and
-- this claim share the finalizer transaction, so a queued post-close action
-- cannot race the finalizer into submitting the same owner input twice.
claimMissingBindingReport :: OracleManagerState -> OracleBinding -> STM Bool
claimMissingBindingReport state binding = do
  reported <- readTVar state.managerReportedMissingBindings
  if Set.member binding reported
    then pure False
    else do
      writeTVar state.managerReportedMissingBindings (Set.insert binding reported)
      pure True

findLaneSTM ::
  (OraclePhysicalPhase -> Bool) ->
  [OraclePhysicalLane] ->
  STM (Maybe OraclePhysicalLane)
findLaneSTM _ [] = pure Nothing
findLaneSTM matches (lane : remaining) = do
  phase <- readTVar lane.physicalPhase
  if matches phase then pure (Just lane) else findLaneSTM matches remaining

phaseLane :: OraclePhysicalPhase -> OracleLane
phaseLane = \case
  OracleCandidate attempt -> oracleCandidateLane attempt
  OracleEstablished binding -> oracleEstablishedLane binding
  OracleRetired lane -> lane

scheduleRetry ::
  (IO () -> IO (Maybe ThreadId)) ->
  HeraldRuntime ->
  PeerDialRetryDelay ->
  OracleRetryWorkers OracleRetry ->
  OracleRetry ->
  IO ()
scheduleRetry spawn runtime (PeerDialRetryDelay microseconds) workers retry =
  scheduleOracleRetryWorker
    spawn
    (threadDelay (fromIntegral microseconds))
    (void (submitOracleClientIngress runtime (OracleRetryElapsed retry)))
    workers
    retry

releaseNotReadySubmissions :: OracleManagerState -> OracleRetry -> STM ()
releaseNotReadySubmissions state retry = do
  lanes <- readTVar state.managerLanes
  forM_ lanes $ \lane -> do
    phase <- readTVar lane.physicalPhase
    case phase of
      OracleEstablished _ ->
        modifyTVar'
          lane.physicalSubmissions
          (releaseOracleSubmissionRetries retry)
      _ -> pure ()
