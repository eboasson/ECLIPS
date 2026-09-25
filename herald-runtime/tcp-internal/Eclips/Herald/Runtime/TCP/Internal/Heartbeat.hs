{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Bounded physical heartbeat scheduling shared by EAPP and EPRP sockets.
module Eclips.Herald.Runtime.TCP.Internal.Heartbeat
  ( HeartbeatPhase (..),
    HeartbeatState,
    newHeartbeatState,
    noteHeartbeatActivity,
    discardHeartbeatAttribution,
    matchHeartbeatPong,
    recordHeartbeatPong,
    runActiveHeartbeat,
    runEstablishmentDeadline,
    runPassiveHeartbeat,
    setHeartbeatProbeForTest,
    setHeartbeatSchedulerForTest,
  )
where

import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Monad (forM_)
import Data.Word (Word64)
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeartbeatConfiguration (..),
    HeartbeatIdleDelay (..),
    HeartbeatLane,
    HeartbeatProbeEvent (..),
    HeartbeatProbePhase (..),
    HeartbeatReplyTimeout (..),
    HeartbeatScheduler (..),
    HeraldTcp (..),
    TcpContext (..),
  )

-- | The only physical phases relevant to a heartbeat worker.
data HeartbeatPhase
  = HeartbeatNotEstablished
  | HeartbeatEstablished
  | HeartbeatQuiescing
  | HeartbeatClosed
  deriving stock (Eq, Show)

-- | Per-socket heartbeat facts. One worker owns nonce allocation; the reader
-- alone refreshes activity, satisfies the outstanding liveness probe, and
-- attributes an exact Pong when applicable.
data HeartbeatState = HeartbeatState
  { heartbeatActivityGeneration :: TVar Word64,
    heartbeatOutstandingNonce :: TVar (Maybe Word64),
    heartbeatAttributableNonce :: TVar (Maybe Word64),
    heartbeatNextNonce :: TVar Word64
  }

data IdleOutcome
  = IdleRestart
  | IdleElapsed
  | IdleStopped

data PingClaim
  = PingClaimed Word64
  | PingClaimRestart
  | PingClaimStopped

data ReplyOutcome
  = ReplyMatched
  | ReplyElapsed
  | ReplyStopped

-- | Allocate empty heartbeat state for one physical socket.
newHeartbeatState :: IO HeartbeatState
newHeartbeatState =
  HeartbeatState
    <$> newTVarIO 0
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO 0

-- | Record one nonempty established-lane receive chunk. Any such byte progress
-- satisfies an outstanding liveness probe; only a subsequently decoded exact
-- Pong receives Pong-specific attribution.
noteHeartbeatActivity :: HeartbeatState -> STM ()
noteHeartbeatActivity state = do
  outstanding <- readTVar state.heartbeatOutstandingNonce
  modifyTVar' state.heartbeatActivityGeneration (+ 1)
  -- Preserve the token across further fragments until one complete envelope
  -- decides whether it was the matching Pong. A later complete non-Pong (or
  -- mismatched Pong) discards it before subsequent traffic can claim credit.
  case outstanding of
    Just nonce -> writeTVar state.heartbeatAttributableNonce (Just nonce)
    Nothing -> pure ()
  writeTVar state.heartbeatOutstandingNonce Nothing

-- | Complete non-Pong traffic already proved liveness, so any retained token
-- from its first fragment is no longer attributable to a later Pong.
discardHeartbeatAttribution :: HeartbeatState -> STM ()
discardHeartbeatAttribution state =
  writeTVar state.heartbeatAttributableNonce Nothing

-- | Attribute one complete Pong, consuming the retained fragment token even
-- when the nonce does not match.
matchHeartbeatPong :: HeartbeatState -> Word64 -> STM Bool
matchHeartbeatPong state nonce = do
  attributable <- readTVar state.heartbeatAttributableNonce
  writeTVar state.heartbeatAttributableNonce Nothing
  writeTVar state.heartbeatOutstandingNonce Nothing
  pure (attributable == Just nonce)

-- | Consume one Pong physically and attribute only an exact outstanding match.
recordHeartbeatPong ::
  TcpContext ->
  HeartbeatLane ->
  HeartbeatState ->
  Word64 ->
  IO Bool
recordHeartbeatPong context lane state nonce = do
  matched <- atomically (matchHeartbeatPong state nonce)
  notifyHeartbeatProbe context lane (HeartbeatPongReceived nonce matched)
  if matched
    then notifyHeartbeatProbe context lane (HeartbeatPongMatched nonce)
    else pure ()
  pure matched

-- | Bound the pre-establishment phase of one physical socket. Logical
-- establishment or exact-socket close wins a racing deadline. Each connection
-- owner supplies its own application or peer heartbeat policy.
runEstablishmentDeadline ::
  TcpContext ->
  HeartbeatConfiguration ->
  HeartbeatLane ->
  STM HeartbeatPhase ->
  IO () ->
  IO ()
runEstablishmentDeadline context (HeartbeatConfiguration _ (HeartbeatReplyTimeout microseconds)) lane readPhase closeAction = do
  elapsed <- scheduleHeartbeat context microseconds
  notifyHeartbeatProbe context lane HeartbeatEstablishmentScheduled
  timedOut <-
    atomically $ do
      phase <- readPhase
      case phase of
        HeartbeatEstablished -> pure False
        HeartbeatQuiescing -> pure False
        HeartbeatClosed -> pure False
        HeartbeatNotEstablished -> do
          completed <- readTVar elapsed
          if completed then pure True else retry
  if timedOut
    then do
      notifyHeartbeatProbe context lane HeartbeatEstablishmentTimedOut
      closeAction
    else pure ()

-- | Run one active heartbeat loop. The supplied phase action makes candidate
-- lanes park without scheduling work and makes exact-socket close terminate the
-- worker. The send callback is already serialized with every other socket send.
runActiveHeartbeat ::
  TcpContext ->
  HeartbeatConfiguration ->
  HeartbeatLane ->
  STM HeartbeatPhase ->
  HeartbeatState ->
  (Word64 -> IO Bool) ->
  IO () ->
  IO ()
runActiveHeartbeat context configuration lane readPhase state sendPing closeAction =
  loop
  where
    loop = do
      started <- atomically (awaitEstablished readPhase state)
      case started of
        Nothing -> pure ()
        Just generation -> do
          idle <- awaitIdle context configuration lane readPhase state generation
          case idle of
            IdleStopped -> pure ()
            IdleRestart -> loop
            IdleElapsed -> do
              claim <- atomically (claimPing readPhase state generation)
              case claim of
                PingClaimStopped -> pure ()
                PingClaimRestart -> loop
                PingClaimed nonce -> do
                  written <- sendPing nonce
                  if not written
                    then closeAction
                    else do
                      notifyHeartbeatProbe context lane (HeartbeatPingWritten nonce)
                      reply <- awaitReply context configuration lane readPhase state nonce
                      case reply of
                        ReplyStopped -> pure ()
                        ReplyMatched -> loop
                        ReplyElapsed -> do
                          notifyHeartbeatProbe context lane (HeartbeatTimedOut (Just nonce))
                          closeAction

-- | Run the passive EAPP-server side. Once established, lack of any valid
-- client frame for the idle interval plus reply grace closes only this socket.
-- A client Ping is ordinary activity and is answered by the reader path.
runPassiveHeartbeat ::
  TcpContext ->
  HeartbeatConfiguration ->
  HeartbeatLane ->
  STM HeartbeatPhase ->
  HeartbeatState ->
  IO () ->
  IO ()
runPassiveHeartbeat context configuration lane readPhase state closeAction =
  loop
  where
    loop = do
      started <- atomically (awaitEstablished readPhase state)
      case started of
        Nothing -> pure ()
        Just generation -> do
          idle <- awaitIdle context configuration lane readPhase state generation
          case idle of
            IdleStopped -> pure ()
            IdleRestart -> loop
            IdleElapsed -> do
              reply <- awaitPassiveReply context configuration lane readPhase state generation
              case reply of
                ReplyStopped -> pure ()
                ReplyMatched -> loop
                ReplyElapsed -> do
                  notifyHeartbeatProbe context lane (HeartbeatTimedOut Nothing)
                  closeAction

-- | Install or remove deterministic heartbeat observations.
setHeartbeatProbeForTest ::
  HeraldTcp ->
  Maybe (HeartbeatProbeEvent -> IO ()) ->
  IO ()
setHeartbeatProbeForTest (HeraldTcp _ _ _ context) probe =
  atomically (writeTVar context.tcpHeartbeatProbe probe)

-- | Replace the heartbeat deadline interpreter for deterministic tests.
setHeartbeatSchedulerForTest :: HeraldTcp -> HeartbeatScheduler -> IO ()
setHeartbeatSchedulerForTest (HeraldTcp _ _ _ context) scheduler =
  atomically (writeTVar context.tcpHeartbeatScheduler scheduler)

awaitEstablished ::
  STM HeartbeatPhase ->
  HeartbeatState ->
  STM (Maybe Word64)
awaitEstablished readPhase state = do
  phase <- readPhase
  case phase of
    HeartbeatNotEstablished -> retry
    HeartbeatQuiescing -> retry
    HeartbeatClosed -> pure Nothing
    HeartbeatEstablished -> Just <$> readTVar state.heartbeatActivityGeneration

awaitIdle ::
  TcpContext ->
  HeartbeatConfiguration ->
  HeartbeatLane ->
  STM HeartbeatPhase ->
  HeartbeatState ->
  Word64 ->
  IO IdleOutcome
awaitIdle context (HeartbeatConfiguration (HeartbeatIdleDelay microseconds) _) lane readPhase state generation = do
  elapsed <- scheduleHeartbeat context microseconds
  notifyHeartbeatProbe context lane HeartbeatIdleScheduled
  atomically $ do
    phase <- readPhase
    case phase of
      HeartbeatClosed -> pure IdleStopped
      HeartbeatNotEstablished -> retry
      HeartbeatQuiescing -> retry
      HeartbeatEstablished -> do
        current <- readTVar state.heartbeatActivityGeneration
        completed <- readTVar elapsed
        if not completed
          then retry
          else
            if current /= generation
              then pure IdleRestart
              else pure IdleElapsed

claimPing ::
  STM HeartbeatPhase ->
  HeartbeatState ->
  Word64 ->
  STM PingClaim
claimPing readPhase state generation = do
  phase <- readPhase
  case phase of
    HeartbeatClosed -> pure PingClaimStopped
    HeartbeatNotEstablished -> pure PingClaimRestart
    HeartbeatQuiescing -> pure PingClaimRestart
    HeartbeatEstablished -> do
      current <- readTVar state.heartbeatActivityGeneration
      outstanding <- readTVar state.heartbeatOutstandingNonce
      if current /= generation || outstanding /= Nothing
        then pure PingClaimRestart
        else do
          nonce <- readTVar state.heartbeatNextNonce
          writeTVar state.heartbeatNextNonce (nonce + 1)
          writeTVar state.heartbeatOutstandingNonce (Just nonce)
          writeTVar state.heartbeatAttributableNonce Nothing
          pure (PingClaimed nonce)

awaitReply ::
  TcpContext ->
  HeartbeatConfiguration ->
  HeartbeatLane ->
  STM HeartbeatPhase ->
  HeartbeatState ->
  Word64 ->
  IO ReplyOutcome
awaitReply context (HeartbeatConfiguration _ (HeartbeatReplyTimeout microseconds)) lane readPhase state nonce = do
  elapsed <- scheduleHeartbeat context microseconds
  notifyHeartbeatProbe context lane (HeartbeatReplyScheduled (Just nonce))
  atomically $ do
    phase <- readPhase
    case phase of
      HeartbeatClosed -> pure ReplyStopped
      HeartbeatNotEstablished -> retry
      HeartbeatQuiescing -> retry
      HeartbeatEstablished -> do
        outstanding <- readTVar state.heartbeatOutstandingNonce
        if outstanding /= Just nonce
          then pure ReplyMatched
          else do
            completed <- readTVar elapsed
            if completed then pure ReplyElapsed else retry

awaitPassiveReply ::
  TcpContext ->
  HeartbeatConfiguration ->
  HeartbeatLane ->
  STM HeartbeatPhase ->
  HeartbeatState ->
  Word64 ->
  IO ReplyOutcome
awaitPassiveReply context (HeartbeatConfiguration _ (HeartbeatReplyTimeout microseconds)) lane readPhase state generation = do
  elapsed <- scheduleHeartbeat context microseconds
  notifyHeartbeatProbe context lane (HeartbeatReplyScheduled Nothing)
  atomically $ do
    phase <- readPhase
    case phase of
      HeartbeatClosed -> pure ReplyStopped
      HeartbeatNotEstablished -> retry
      HeartbeatQuiescing -> retry
      HeartbeatEstablished -> do
        current <- readTVar state.heartbeatActivityGeneration
        if current /= generation
          then pure ReplyMatched
          else do
            completed <- readTVar elapsed
            if completed then pure ReplyElapsed else retry

scheduleHeartbeat :: TcpContext -> Word64 -> IO (TVar Bool)
scheduleHeartbeat context microseconds = do
  HeartbeatScheduler schedule <- atomically (readTVar context.tcpHeartbeatScheduler)
  schedule microseconds

notifyHeartbeatProbe :: TcpContext -> HeartbeatLane -> HeartbeatProbePhase -> IO ()
notifyHeartbeatProbe context lane phase = do
  probe <- atomically (readTVar context.tcpHeartbeatProbe)
  forM_ probe (\observe -> observe (HeartbeatProbeEvent lane phase))
