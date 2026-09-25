-- | Small STM-only proposal and result correlations.
module Eclips.Oracle.Runtime.Internal.Coordination
  ( newRuntimeCoordination,
    registerProposal,
    observeProposalStatus,
    completeProposalAt,
    abandonRuntimeProposals,
    submitCanonicalOracle,
    queryPublishedOracleRequest,
    watchPublishedOracleEntries,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    atomically,
    check,
    modifyTVar',
    newEmptyTMVar,
    newTVar,
    orElse,
    readTMVar,
    readTVar,
    retry,
    stateTVar,
    tryPutTMVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (onException)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Eclips.Domain.Identity (ControlIndex, controlIndex)
import Eclips.Oracle.Canonical
  ( CanonicalOracleEnvelope,
    canonicalOracleEnvelopeBytes,
    canonicalOracleEnvelopeDigest,
    canonicalOracleEnvelopeValue,
  )
import Eclips.Oracle.Command
  ( oracleCommandProgress,
    oracleCommandVoterChangeCancellation,
    oracleEnvelopeCommand,
    oracleEnvelopeRequestId,
  )
import Eclips.Oracle.Effect (OracleProtocolDisposition (..))
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Receipt (oracleReceiptCommandDigest)
import Eclips.Oracle.Runtime.Internal.Scope
  ( runtimeScopeObservedFailure,
    signalRuntimeFailure,
  )
import Eclips.Oracle.Runtime.Internal.Submission
  ( SubmissionRegistrationDecision (..),
    decideSubmissionRegistration,
    mayCancelVoterPreparation,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( AdapterCommand (QueryPublishedOracleResult),
    OracleOwnerCommand (PreflightOracleOwner),
    OracleRequestResult (..),
    OracleRuntime (..),
    OracleRuntimeConfiguration (..),
    OracleRuntimeFailure (RuntimeSubmissionPreflightInvariantFault),
    OracleRuntimeStatus (..),
    OracleSubmissionPreflight (..),
    OracleSubmitResult (..),
    OracleWatchResult (..),
    ProposalWaiter (..),
    RaftOwnerCommand (CancelRaftPreparation, ReconcileOracleVoters, StepRaftOwner),
    RuntimeCoordination (..),
    RuntimeSubmissionCheckpoint (..),
    WatchLedgerCommand (CancelWatch, ReadWatchSuffix),
  )
import Eclips.Oracle.Runtime.Internal.WatchAvailability
  ( OracleWatchAvailability (..),
    awaitWatchAuthorityGrace,
    classifyOracleWatchAvailability,
  )
import Eclips.Raft.Effect
  ( RaftProposalStatus (..),
  )
import Eclips.Raft.Genesis
  ( checkedRaftNativeConfiguration,
    raftNativeElectionTimeoutUpper,
  )
import Eclips.Raft.Identity
  ( RaftLogIndex,
    RaftProposalId,
    RaftTerm,
    mkRaftProposalId,
  )
import Eclips.Raft.Input (proposeRaftApplication)
import Eclips.Raft.State (RaftRole (RaftLeader))

data RegisteredWatchWait
  = RegisteredWatchCompleted OracleWatchResult
  | RegisteredWatchAuthorityLost

data WatchAuthorityGrace
  = WatchAuthorityGraceCompleted OracleWatchResult
  | WatchAuthorityRecovered

newRuntimeCoordination :: STM RuntimeCoordination
newRuntimeCoordination =
  RuntimeCoordination
    <$> newTVar 1
    <*> newTVar 1
    <*> newTVar Map.empty
    <*> newTVar Map.empty
    <*> newTVar (controlIndex 0)
    <*> newTVar False
    <*> newTVar True

registerProposal :: RuntimeCoordination -> STM (RaftProposalId, TMVar OracleSubmitResult)
registerProposal coordination = do
  sequenceNumber <- stateTVar coordination.coordinationNextProposal (\current -> (current, current + 1))
  proposal <- case mkRaftProposalId sequenceNumber of
    Right checked -> pure checked
    Left fault -> error ("positive runtime proposal rejected: " <> show fault)
  result <- newEmptyTMVar
  modifyTVar'
    coordination.coordinationProposals
    (Map.insert proposal (ProposalWaiter result Nothing))
  pure (proposal, result)

observeProposalStatus :: RuntimeCoordination -> RaftProposalId -> RaftProposalStatus -> STM ()
observeProposalStatus coordination proposal status = do
  waiters <- readTVar coordination.coordinationProposals
  case Map.lookup proposal waiters of
    Nothing -> pure ()
    Just waiter -> case status of
      RaftProposalAppended index -> do
        writeTVar
          coordination.coordinationProposals
          (Map.insert proposal waiter {proposalWaiterLogIndex = Just index} waiters)
        modifyTVar' coordination.coordinationProposalByIndex (Map.insert index proposal)
      RaftProposalRedirected leader term ->
        settle proposal waiter (OracleSubmitRedirect leader term)
      RaftProposalNotReady term ->
        settle proposal waiter (OracleSubmitNotReady term)
      RaftProposalCommitted _ -> pure ()
      RaftConfigurationWaiting {} -> configurationStatusInvariantFault
      RaftConfigurationPrepared {} -> configurationStatusInvariantFault
      RaftConfigurationRejected -> configurationStatusInvariantFault
      RaftConfigurationCancelled -> configurationStatusInvariantFault
  where
    -- This composition submits applications only. P08 owns the separate
    -- retained configuration workflow; its statuses cannot settle an Oracle
    -- application's waiter merely because both use native proposal identities.
    configurationStatusInvariantFault =
      error "native configuration status reached an Oracle application waiter"

    settle proposalId waiter result = do
      _ <- tryPutTMVar waiter.proposalWaiterResult result
      modifyTVar' coordination.coordinationProposals (Map.delete proposalId)
      case waiter.proposalWaiterLogIndex of
        Nothing -> pure ()
        Just index -> modifyTVar' coordination.coordinationProposalByIndex (Map.delete index)

completeProposalAt :: RuntimeCoordination -> RaftLogIndex -> OracleSubmitResult -> STM ()
completeProposalAt coordination index result = do
  byIndex <- readTVar coordination.coordinationProposalByIndex
  case Map.lookup index byIndex of
    Nothing -> pure ()
    Just proposal -> do
      waiters <- readTVar coordination.coordinationProposals
      case Map.lookup proposal waiters of
        Nothing -> pure ()
        Just waiter -> do
          _ <- tryPutTMVar waiter.proposalWaiterResult result
          writeTVar coordination.coordinationProposals (Map.delete proposal waiters)
      writeTVar coordination.coordinationProposalByIndex (Map.delete index byIndex)

abandonRuntimeProposals :: RuntimeCoordination -> STM ()
abandonRuntimeProposals coordination = do
  waiters <- readTVar coordination.coordinationProposals
  mapM_ (\waiter -> tryPutTMVar waiter.proposalWaiterResult OracleSubmitUnavailable) (Map.elems waiters)
  writeTVar coordination.coordinationProposals Map.empty
  writeTVar coordination.coordinationProposalByIndex Map.empty

submitCanonicalOracle :: OracleRuntime -> CanonicalOracleEnvelope -> IO OracleSubmitResult
submitCanonicalOracle runtime canonical = do
  let maintenance = isJust (oracleCommandProgress (oracleEnvelopeCommand (canonicalOracleEnvelopeValue canonical)))
  preparing <- atomically (isJust . statusConfigurationFrontier <$> readTVar runtime.runtimeStatusCell)
  retained <-
    if preparing && not maintenance
      then queryPublishedOracleRequest runtime (oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical))
      else pure OracleRequestUnavailable
  case retained of
    OracleRequestFound receipt
      | oracleReceiptCommandDigest receipt == canonicalOracleEnvelopeDigest canonical -> pure (OracleSubmitReceipt receipt)
      | otherwise ->
          pure
            ( OracleSubmitConflict
                ( ConflictingOracleRequestId
                    (oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical))
                    (oracleReceiptCommandDigest receipt)
                    (canonicalOracleEnvelopeDigest canonical)
                )
            )
    OracleRequestRetired reason -> pure (OracleSubmitRetired reason)
    _ -> submitUnretainedCanonicalOracle runtime canonical

-- A retained preparation receipt stays retrievable while its learner frontier
-- gates new work. The ordinary preflight/reservation path remains unchanged.
submitUnretainedCanonicalOracle :: OracleRuntime -> CanonicalOracleEnvelope -> IO OracleSubmitResult
submitUnretainedCanonicalOracle runtime canonical = do
  reserved <- atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    semanticReady <- readTVar runtime.runtimeCoordination.coordinationSemanticReady
    let cancellation = oracleCommandVoterChangeCancellation (oracleEnvelopeCommand (canonicalOracleEnvelopeValue canonical))
        cancelPreparation =
          maybe False (\identifier -> mayCancelVoterPreparation identifier status.statusPendingVoterChange) cancellation
            && isJust status.statusConfigurationFrontier
    if not status.statusAccepting
      then pure (Left OracleSubmitUnavailable)
      else
        if status.statusRole /= RaftLeader
          then pure (Left (OracleSubmitRedirect status.statusLeaderHint status.statusTerm))
          else
            if (not status.statusServiceReady && not cancelPreparation)
              || not semanticReady
              || (isJust status.statusConfigurationFrontier && not cancelPreparation)
              then pure (Left (OracleSubmitNotReady status.statusTerm))
              else do
                writeTVar runtime.runtimeCoordination.coordinationSemanticReady False
                writeTVar runtime.runtimeCoordination.coordinationSubmissionReserved True
                result <- newEmptyTMVar
                writeTQueue
                  runtime.runtimeOracleCommands
                  (PreflightOracleOwner canonical result)
                preparation <-
                  case cancellation of
                    Just identifier | cancelPreparation -> do
                      settled <- newEmptyTMVar
                      writeTQueue runtime.runtimeRaftCommands (CancelRaftPreparation identifier settled)
                      pure (Just settled)
                    _ -> pure Nothing
                pure (Right (status.statusTerm, result, preparation))
  case reserved of
    Left immediate -> pure immediate
    Right (reservedTerm, result, preparation) -> do
      settled <- case preparation of
        Nothing -> pure True
        Just done ->
          atomically
            ( readTMVar done
                `orElse` (runtimeScopeObservedFailure runtime.runtimeScope >> pure False)
                `orElse` (unavailableWhenClosed runtime >> pure False)
            )
      if settled
        then awaitReservedPreflight runtime canonical reservedTerm result
        else do
          atomically (releaseRuntimeSubmission runtime)
          pure (OracleSubmitNotReady reservedTerm)

awaitReservedPreflight ::
  OracleRuntime ->
  CanonicalOracleEnvelope ->
  RaftTerm ->
  TMVar OracleSubmissionPreflight ->
  IO OracleSubmitResult
awaitReservedPreflight runtime canonical reservedTerm result = do
  observed <-
    atomically
      ( (Just <$> readTMVar result)
          `orElse` (runtimeScopeObservedFailure runtime.runtimeScope >> pure Nothing)
          `orElse` (unavailableWhenClosed runtime >> pure Nothing)
      )
  case observed of
    Nothing -> do
      atomically (releaseRuntimeSubmission runtime)
      pure OracleSubmitUnavailable
    Just (OracleSubmissionMayProceed observedIndex) -> do
      -- This shell-supplied observation point still precedes proposal
      -- registration. Preserve any exception for its caller, but first release
      -- the reservation: no Raft proposal exists whose completion could do so.
      runRuntimeSubmissionCheckpoint
        runtime.runtimeConfiguration.configuredSubmissionCheckpoint
        (oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical))
        (oracleEnvelopeCommand (canonicalOracleEnvelopeValue canonical))
        observedIndex
        `onException` atomically
          (releaseRuntimeSubmission runtime)
      proposeAfterPreflight runtime canonical reservedTerm observedIndex
    Just (OracleSubmissionMustDefer decision observedIndex) -> do
      atomically (releaseRuntimeSubmission runtime)
      pure
        ( OracleSubmitDeferred
            (oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical))
            decision
            observedIndex
        )

data ProposalRegistration
  = ProposalPreflightInvariantFault OracleRuntimeFailure
  | ProposalRejected OracleSubmitResult
  | ProposalRegistered (TMVar OracleSubmitResult)

proposeAfterPreflight ::
  OracleRuntime ->
  CanonicalOracleEnvelope ->
  RaftTerm ->
  ControlIndex ->
  IO OracleSubmitResult
proposeAfterPreflight runtime canonical reservedTerm observedIndex = do
  registered <- atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    if not status.statusAccepting
      then reject OracleSubmitUnavailable
      else
        if status.statusRole /= RaftLeader
          then reject (OracleSubmitRedirect status.statusLeaderHint status.statusTerm)
          else do
            currentIndex <-
              readTVar
                runtime.runtimeCoordination.coordinationOracleControlIndex
            case decideSubmissionRegistration
              RuntimeSubmissionPreflightInvariantFault
              reservedTerm
              status.statusTerm
              observedIndex
              currentIndex of
              RetryStaleSubmissionReservation ->
                reject (OracleSubmitNotReady status.statusTerm)
              FailSubmissionRegistration failure -> do
                releaseRuntimeSubmission runtime
                pure (ProposalPreflightInvariantFault failure)
              RegisterSubmissionProposal ->
                if not status.statusServiceReady
                  then reject (OracleSubmitNotReady status.statusTerm)
                  else do
                    (proposal, result) <- registerProposal runtime.runtimeCoordination
                    writeTQueue
                      runtime.runtimeRaftCommands
                      ( StepRaftOwner
                          (proposeRaftApplication proposal (canonicalOracleEnvelopeBytes canonical))
                          Nothing
                      )
                    pure (ProposalRegistered result)
  case registered of
    ProposalPreflightInvariantFault failure -> do
      -- Admission reserves semantic readiness and the leadership term before
      -- asking the sole Oracle owner for this token. A different term retries
      -- before this invariant is considered. Raft service readiness excludes a
      -- pending proposal and a committed but unapplied entry, so no installed
      -- Oracle prefix can advance while this same-term leader reservation survives.
      -- An observed change is therefore an invariant fault, not a request race.
      signalRuntimeFailure runtime.runtimeScope failure
      pure OracleSubmitUnavailable
    ProposalRejected immediate -> pure immediate
    ProposalRegistered result ->
      atomically $ do
        outcome <-
          readTMVar result
            `orElse` (runtimeScopeObservedFailure runtime.runtimeScope >> pure OracleSubmitUnavailable)
            `orElse` unavailableWhenClosed runtime
        releaseRuntimeSubmission runtime
        pure outcome
  where
    reject result = do
      releaseRuntimeSubmission runtime
      pure (ProposalRejected result)

releaseRuntimeSubmission :: OracleRuntime -> STM ()
releaseRuntimeSubmission runtime = do
  releaseSubmission runtime.runtimeCoordination
  writeTQueue runtime.runtimeRaftCommands ReconcileOracleVoters

releaseSubmission :: RuntimeCoordination -> STM ()
releaseSubmission coordination = do
  writeTVar coordination.coordinationSubmissionReserved False
  writeTVar coordination.coordinationSemanticReady True

queryPublishedOracleRequest :: OracleRuntime -> OracleClientRequestId -> IO OracleRequestResult
queryPublishedOracleRequest runtime requestId = do
  result <- atomically newEmptyTMVar
  offered <- atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    if status.statusAccepting
      then writeTQueue runtime.runtimeAdapterCommands (QueryPublishedOracleResult requestId result) >> pure True
      else pure False
  if not offered
    then pure OracleRequestUnavailable
    else do
      observed <-
        atomically
          ( (Just <$> readTMVar result)
              `orElse` (runtimeScopeObservedFailure runtime.runtimeScope >> pure Nothing)
              `orElse` (unavailableWhenClosed runtime >> pure Nothing)
          )
      case observed of
        Nothing -> pure OracleRequestUnavailable
        Just (found, retired, status, semanticReady) ->
          case (retired, found) of
            (Just reason, _) -> pure (OracleRequestRetired reason)
            (Nothing, Just receipt) -> pure (OracleRequestFound receipt)
            (Nothing, Nothing) ->
              pure
                ( if not status.statusAccepting
                    then OracleRequestUnavailable
                    else
                      if status.statusRole /= RaftLeader
                        then OracleRequestRedirect status.statusLeaderHint status.statusTerm
                        else
                          if status.statusServiceReady && semanticReady
                            then OracleRequestAbsent status.statusAppliedControlIndex
                            else OracleRequestNotReady status.statusTerm
                )

watchPublishedOracleEntries :: OracleRuntime -> ControlIndex -> IO OracleWatchResult
watchPublishedOracleEntries runtime cursor = do
  result <- atomically newEmptyTMVar
  offered <- atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    if not status.statusAccepting
      then pure (Left OracleWatchUnavailable)
      else
        if cursor > status.statusAppliedControlIndex
          then pure (Left (OracleWatchCursorAhead status.statusAppliedControlIndex))
          else do
            let atTip = cursor == status.statusAppliedControlIndex
                authority = classifyStatus status
            case (atTip, authority) of
              (True, OracleWatchAuthorityDistinct) ->
                pure (Left (OracleWatchRedirect status.statusLeaderHint status.statusTerm))
              (True, OracleWatchAuthorityUnresolved) ->
                pure (Left (OracleWatchRedirect status.statusLeaderHint status.statusTerm))
              _ -> do
                watchId <-
                  stateTVar
                    runtime.runtimeCoordination.coordinationNextWatch
                    (\current -> (current, current + 1))
                writeTQueue runtime.runtimeWatchCommands (ReadWatchSuffix watchId cursor result)
                pure (Right (watchId, atTip && authority == OracleWatchAuthorityLocal))
  case offered of
    Left immediate -> pure immediate
    Right (watchId, acceptedAtLeaderTip) ->
      awaitRegisteredWatch result watchId acceptedAtLeaderTip
        `onException` atomically
          (writeTQueue runtime.runtimeWatchCommands (CancelWatch watchId))
  where
    classifyStatus status =
      classifyOracleWatchAvailability
        status.statusAccepting
        (status.statusRole == RaftLeader)
        status.statusNode
        status.statusLeaderHint

    awaitRegisteredWatch result watchId acceptedAtLeaderTip = do
      observed <- atomically $ waitForRegisteredWatch result watchId acceptedAtLeaderTip
      case observed of
        RegisteredWatchCompleted completed -> pure completed
        RegisteredWatchAuthorityLost -> awaitAuthorityGrace result watchId acceptedAtLeaderTip

    waitForRegisteredWatch result watchId acceptedAtLeaderTip =
      (RegisteredWatchCompleted <$> readTMVar result)
        `orElse` ( runtimeScopeObservedFailure runtime.runtimeScope
                     >> pure (RegisteredWatchCompleted OracleWatchUnavailable)
                 )
        `orElse` do
          status <- readTVar runtime.runtimeStatusCell
          let authority = classifyStatus status
          case authority of
            OracleWatchAuthorityStopped -> do
              writeTQueue runtime.runtimeWatchCommands (CancelWatch watchId)
              pure (RegisteredWatchCompleted OracleWatchUnavailable)
            OracleWatchAuthorityLocal -> retry
            OracleWatchAuthorityDistinct
              | acceptedAtLeaderTip
                  && cursor == status.statusAppliedControlIndex -> do
                  writeTQueue runtime.runtimeWatchCommands (CancelWatch watchId)
                  pure
                    ( RegisteredWatchCompleted
                        (OracleWatchRedirect status.statusLeaderHint status.statusTerm)
                    )
            OracleWatchAuthorityUnresolved
              | acceptedAtLeaderTip
                  && cursor == status.statusAppliedControlIndex ->
                  pure RegisteredWatchAuthorityLost
            _ -> retry

    awaitAuthorityGrace result watchId acceptedAtLeaderTip = do
      observed <-
        awaitWatchAuthorityGrace
          runtime.runtimeConfiguration.configuredWatchAuthorityDelay
          watchAuthorityGraceDuration
          (waitThroughAuthorityGrace result watchId)
          (expireAuthorityGrace result watchId)
      case observed of
        WatchAuthorityGraceCompleted completed -> pure completed
        WatchAuthorityRecovered ->
          awaitRegisteredWatch result watchId acceptedAtLeaderTip

    waitThroughAuthorityGrace result watchId =
      (WatchAuthorityGraceCompleted <$> readTMVar result)
        `orElse` ( runtimeScopeObservedFailure runtime.runtimeScope
                     >> pure (WatchAuthorityGraceCompleted OracleWatchUnavailable)
                 )
        `orElse` do
          status <- readTVar runtime.runtimeStatusCell
          case classifyStatus status of
            OracleWatchAuthorityStopped -> do
              writeTQueue runtime.runtimeWatchCommands (CancelWatch watchId)
              pure (WatchAuthorityGraceCompleted OracleWatchUnavailable)
            OracleWatchAuthorityLocal -> pure WatchAuthorityRecovered
            _
              | cursor < status.statusAppliedControlIndex ->
                  pure WatchAuthorityRecovered
            OracleWatchAuthorityDistinct -> do
              writeTQueue runtime.runtimeWatchCommands (CancelWatch watchId)
              pure
                ( WatchAuthorityGraceCompleted
                    (OracleWatchRedirect status.statusLeaderHint status.statusTerm)
                )
            OracleWatchAuthorityUnresolved -> retry

    expireAuthorityGrace result watchId =
      (WatchAuthorityGraceCompleted <$> readTMVar result)
        `orElse` do
          status <- readTVar runtime.runtimeStatusCell
          case classifyStatus status of
            OracleWatchAuthorityStopped -> do
              writeTQueue runtime.runtimeWatchCommands (CancelWatch watchId)
              pure (WatchAuthorityGraceCompleted OracleWatchUnavailable)
            OracleWatchAuthorityLocal -> pure WatchAuthorityRecovered
            _
              | cursor < status.statusAppliedControlIndex ->
                  pure WatchAuthorityRecovered
              | otherwise -> do
                  writeTQueue runtime.runtimeWatchCommands (CancelWatch watchId)
                  pure
                    ( WatchAuthorityGraceCompleted
                        (OracleWatchRedirect status.statusLeaderHint status.statusTerm)
                    )

    watchAuthorityGraceDuration =
      raftNativeElectionTimeoutUpper
        ( checkedRaftNativeConfiguration
            runtime.runtimeConfiguration.configuredRaftGenesis
        )

unavailableWhenClosed :: OracleRuntime -> STM OracleSubmitResult
unavailableWhenClosed runtime = do
  status <- readTVar runtime.runtimeStatusCell
  check (not status.statusAccepting)
  pure OracleSubmitUnavailable
