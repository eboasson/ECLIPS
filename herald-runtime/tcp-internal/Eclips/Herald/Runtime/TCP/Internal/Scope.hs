{-# LANGUAGE OverloadedRecordDot #-}

-- | Structured ownership for listeners, dialers, and socket readers.
module Eclips.Herald.Runtime.TCP.Internal.Scope
  ( newTcpContext,
    newTcpSocketKey,
    claimTcpSocket,
    releaseTcpSocket,
    claimPeerTcpSocket,
    claimPeerTcpSocketIf,
    closePeerTransportGate,
    openPeerTransportGate,
    peerTransportGateIsOpen,
    spawnTcpChild,
    spawnTcpTrackedChild,
    spawnTcpFiniteChild,
    spawnTcpTrackedFiniteChild,
    stopTcpAdmission,
    requestTcpContextStop,
    stopTcpContext,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkFinally,
    killThread,
    myThreadId,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Concurrent.STM
  ( STM,
    atomically,
    modifyTVar',
    newEmptyTMVarIO,
    newTQueueIO,
    newTVarIO,
    readTVar,
    registerDelay,
    tryPutTMVar,
    writeTVar,
  )
import Control.Exception
  ( SomeException,
    mask,
    mask_,
    try,
  )
import Control.Monad (forM_, unless, void)
import Data.Unique (newUnique)
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeartbeatScheduler (..),
    HeraldTcpFailure,
    PeerDialScheduler (..),
    PeerTransportGate (..),
    PeerTransportLease (..),
    TcpContext (..),
    TcpSocketKey (..),
    TcpTrackedThread (..),
  )
import Network.Socket (close)

-- | Allocate an accepting but otherwise empty TCP scope.
newTcpContext :: IO TcpContext
newTcpContext = do
  peerGate <- PeerTransportGate <$> newTVarIO True <*> newTVarIO []
  TcpContext Nothing Nothing
    <$> newTVarIO False
    <*> newTVarIO True
    <*> newEmptyTMVarIO
    <*> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO []
    <*> pure peerGate
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> pure mempty
    <*> pure Nothing
    <*> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO (PeerDialScheduler (registerDelay . fromIntegral))
    <*> newTVarIO Nothing
    <*> newTVarIO (HeartbeatScheduler (registerDelay . fromIntegral))
    <*> newTVarIO Nothing
    <*> newTQueueIO
    <*> newTVarIO []
    <*> newTQueueIO
    <*> newTQueueIO
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO []
    <*> newTVarIO []

-- | Allocate an identity before the socket owner starts, so its finalizer can
-- release the claim even when shutdown races physical installation.
newTcpSocketKey :: IO TcpSocketKey
newTcpSocketKey = TcpSocketKey <$> newUnique

-- | Forget an ownership claim after the descriptor closes or transfers to a
-- different registered owner. A stop or peer cut which already captured the
-- lease keeps its own completion witness.
releaseTcpSocket :: TcpContext -> TcpSocketKey -> IO ()
releaseTcpSocket context key = atomically $ do
  modifyTVar' context.tcpSocketCloses (filterRegistry ((/= key) . fst))
  let PeerTransportGate _ leases = context.tcpPeerTransportGate
  modifyTVar' leases (filterRegistry ((/= key) . fst))

-- Force the surviving spine before storing it: a lazy filter underneath a
-- still-live head otherwise retains the removed closures until a later walk.
filterRegistry :: (a -> Bool) -> [a] -> [a]
filterRegistry keep entries =
  let retained = filter keep entries
   in length retained `seq` retained

-- | Linearize one accepted/connected socket against the TCP admission cut.
-- The close action already shares its per-socket close-once cell with the
-- reader and writer paths.
claimTcpSocket :: TcpContext -> TcpSocketKey -> IO () -> IO Bool
claimTcpSocket context key closeAction = claimTcpSocketWith context key Nothing closeAction

-- | Linearize one peer socket against both the composite admission cut and the
-- reversible peer-only transport cut. A socket admitted before a peer cut has
-- its idempotent close action in the same transaction observed by that cut.
claimPeerTcpSocket :: TcpContext -> TcpSocketKey -> PeerTransportLease -> IO Bool
claimPeerTcpSocket context key lease =
  claimTcpSocketWith
    context
    key
    (Just (context.tcpPeerTransportGate, lease))
    (closePeerTransportLease lease)

-- | Linearize a generation-qualified peer socket against the ordinary socket
-- gates and one additional STM ownership predicate.  A dial which completed
-- after its pure permission became stale cannot cross this claim. The request
-- action is also retained by the ordinary global scope; only the reversible
-- peer cut additionally awaits the lease's quiescence witness.
claimPeerTcpSocketIf :: TcpContext -> TcpSocketKey -> STM Bool -> PeerTransportLease -> IO Bool
claimPeerTcpSocketIf context key permitted lease =
  atomically $ do
    allowed <- permitted
    if allowed
      then
        claimTcpSocketWithSTM
          context
          key
          (Just (context.tcpPeerTransportGate, lease))
          (closePeerTransportLease lease)
      else pure False

claimTcpSocketWith ::
  TcpContext ->
  TcpSocketKey ->
  Maybe (PeerTransportGate, PeerTransportLease) ->
  IO () ->
  IO Bool
claimTcpSocketWith context key peerGate closeAction =
  atomically (claimTcpSocketWithSTM context key peerGate closeAction)

claimTcpSocketWithSTM ::
  TcpContext ->
  TcpSocketKey ->
  Maybe (PeerTransportGate, PeerTransportLease) ->
  IO () ->
  STM Bool
claimTcpSocketWithSTM context key peerGate closeAction = do
  stopping <- readTVar context.tcpStopping
  accepting <- readTVar context.tcpAccepting
  peerAccepting <- case peerGate of
    Nothing -> pure True
    Just (PeerTransportGate enabled _, _) -> readTVar enabled
  if stopping || not accepting || not peerAccepting
    then pure False
    else do
      modifyTVar' context.tcpSocketCloses ((key, closeAction) :)
      forM_ peerGate $ \(PeerTransportGate _ leases, lease) ->
        modifyTVar' leases ((key, lease) :)
      pure True

-- | Close the reversible peer-only cut. First request shutdown for every
-- admitted socket under one masked phase, then await every socket owner's
-- quiescence witness. Application, administration, Oracle, and listener
-- admission stay open.
closePeerTransportGate :: PeerTransportGate -> IO ()
closePeerTransportGate (PeerTransportGate enabled leases) = mask $ \restore -> do
  retained <-
    atomically $ do
      writeTVar enabled False
      map snd <$> readTVar leases
  forM_ retained $ \(PeerTransportLease requestClose _) -> requestClose
  restore
    (forM_ retained $ \(PeerTransportLease _ awaitQuiescent) -> awaitQuiescent)

closePeerTransportLease :: PeerTransportLease -> IO ()
closePeerTransportLease (PeerTransportLease requestClose awaitQuiescent) =
  mask $ \restore -> requestClose >> restore awaitQuiescent

-- | Reopen peer socket admission. Retained configured and learned managers may
-- reconnect normally without restarting the Herald or its TCP listeners.
openPeerTransportGate :: PeerTransportGate -> IO ()
openPeerTransportGate (PeerTransportGate enabled _) =
  atomically (writeTVar enabled True)

-- | Observe the exact peer-only admission fact for deterministic test latches.
peerTransportGateIsOpen :: PeerTransportGate -> IO Bool
peerTransportGateIsOpen (PeerTransportGate enabled _) =
  atomically (readTVar enabled)

-- | Start one tracked child behind a registry gate. An essential failure is
-- reported only while the scope is still accepting ordinary work.
spawnTcpChild ::
  TcpContext ->
  Maybe HeraldTcpFailure ->
  IO () ->
  IO (Maybe ThreadId)
spawnTcpChild context essentialFailure =
  fmap trackedThreadId
    . spawnTcpTrackedChild context essentialFailure

-- | Tracked variant used when a socket owner must join the exact child wrapper,
-- including its completion-policy bookkeeping, before releasing the descriptor.
spawnTcpTrackedChild ::
  TcpContext ->
  Maybe HeraldTcpFailure ->
  IO () ->
  IO (Maybe TcpTrackedThread)
spawnTcpTrackedChild context essentialFailure =
  spawnTcpChildWith
    context
    (maybe IgnoreTcpChildCompletion RequireTcpChildLiveness essentialFailure)

-- | Start one finite tracked child. Normal completion is part of the child's
-- protocol, while an exception before that completion is still an essential
-- TCP-scope failure. This is used for generation-qualified dial jobs and
-- configured-seed managers after durable handoff made their successful return
-- finite.
spawnTcpFiniteChild ::
  TcpContext ->
  HeraldTcpFailure ->
  IO () ->
  IO (Maybe ThreadId)
spawnTcpFiniteChild context failure =
  fmap trackedThreadId
    . spawnTcpTrackedFiniteChild context failure

-- | Tracked finite child whose exact completion witness is retained by its
-- caller. Normal return remains an accepted part of this worker's protocol.
spawnTcpTrackedFiniteChild ::
  TcpContext ->
  HeraldTcpFailure ->
  IO () ->
  IO (Maybe TcpTrackedThread)
spawnTcpTrackedFiniteChild context failure =
  spawnTcpChildWith context (AllowTcpChildCompletion failure)

data TcpChildCompletionPolicy
  = IgnoreTcpChildCompletion
  | RequireTcpChildLiveness HeraldTcpFailure
  | AllowTcpChildCompletion HeraldTcpFailure

spawnTcpChildWith ::
  TcpContext ->
  TcpChildCompletionPolicy ->
  IO () ->
  IO (Maybe TcpTrackedThread)
spawnTcpChildWith context completionPolicy action = mask_ $ do
  start <- newEmptyMVar
  done <- newEmptyMVar
  thread <-
    forkFinally
      (takeMVar start >>= \permitted -> if permitted then run else pure ())
      ( \_ -> do
          finished <- myThreadId
          -- Completion policy and action finalizers have settled before this
          -- wrapper leaves the live registry. A captured shutdown list still
          -- joins the same witness after removal.
          atomically (modifyTVar' context.tcpThreads (filterRegistry (\(TcpTrackedThread worker _) -> worker /= finished)))
          putMVar done ()
      )
  accepted <-
    atomically $ do
      stopping <- readTVar context.tcpStopping
      if stopping
        then pure False
        else do
          modifyTVar' context.tcpThreads (TcpTrackedThread thread done :)
          pure True
  putMVar start accepted
  if accepted
    then pure (Just (TcpTrackedThread thread done))
    else do
      killThread thread
      void (readMVar done)
      pure Nothing
  where
    run = do
      result <- try action :: IO (Either SomeException ())
      atomically $ do
        stopping <- readTVar context.tcpStopping
        accepting <- readTVar context.tcpAccepting
        unless (stopping || not accepting)
          $ forM_ (completionFailure completionPolicy result)
          $ \failure -> void (tryPutTMVar context.tcpFailure failure)
      case result of
        Left _ -> pure ()
        Right () -> pure ()

completionFailure ::
  TcpChildCompletionPolicy ->
  Either SomeException () ->
  Maybe HeraldTcpFailure
completionFailure policy result = case policy of
  IgnoreTcpChildCompletion -> Nothing
  RequireTcpChildLiveness failure -> Just failure
  AllowTcpChildCompletion failure -> case result of
    Left _ -> Just failure
    Right () -> Nothing

trackedThreadId :: Maybe TcpTrackedThread -> Maybe ThreadId
trackedThreadId = fmap (\(TcpTrackedThread thread _) -> thread)

-- | Stop installing physical sources while retaining current socket readers.
stopTcpAdmission :: TcpContext -> IO ()
stopTcpAdmission context = do
  listeners <-
    atomically $ do
      writeTVar context.tcpAccepting False
      readTVar context.tcpListeners
  forM_ listeners close

-- | Cut admission and request every peer transport's retirement without
-- waiting for its owner.
--
-- The request phase is separate because peer socket owners join their runtime
-- writers before releasing a descriptor.  The Herald runtime encloses this TCP
-- scope, so a callback must be able to return after requesting transport
-- shutdown; the enclosing scope can then cancel and join those writers before
-- the final TCP join waits for the socket owners.
requestTcpContextStop :: TcpContext -> IO ()
requestTcpContextStop context = mask_ $ do
  (listeners, peerLeases) <-
    atomically $ do
      writeTVar context.tcpStopping True
      writeTVar context.tcpAccepting False
      listeners <- readTVar context.tcpListeners
      let PeerTransportGate _ leases = context.tcpPeerTransportGate
      peerLeases <- map snd <$> readTVar leases
      pure (listeners, peerLeases)
  forM_ listeners close
  forM_ peerLeases $ \(PeerTransportLease requestClose _) -> requestClose

-- | Complete a requested stop after the enclosing Herald runtime has quiesced
-- its connection writers, then stop and join every TCP-owned child.
stopTcpContext :: TcpContext -> IO ()
stopTcpContext context = mask_ $ do
  (listeners, socketCloses, children) <-
    atomically $ do
      writeTVar context.tcpStopping True
      writeTVar context.tcpAccepting False
      listeners <- readTVar context.tcpListeners
      socketCloses <- map snd <$> readTVar context.tcpSocketCloses
      children <- readTVar context.tcpThreads
      pure (listeners, socketCloses, children)
  forM_ listeners close
  sequence_ socketCloses
  forM_ children $ \(TcpTrackedThread thread _) -> killThread thread
  forM_ children $ \(TcpTrackedThread _ done) -> void (readMVar done)
