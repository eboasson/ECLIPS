{-# LANGUAGE OverloadedStrings #-}

-- | Scoped TCP cuts for the three inter-host protocols. Initial checked wire
-- claims identify the source; the advertised listener identifies the target.
-- Closing a gate retires existing lanes and refuses subsequent lanes in both
-- directions. EDSC remains available for read-only fixture observations.
module Partition (withPartitionProxies, casePartitionConnectionScope) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.STM
import Control.Exception (MaskingState (Unmasked), SomeException, bracket, bracketOnError, evaluate, finally, getMaskingState, mask_, onException, throwIO, try)
import Control.Monad (forM_, forever, replicateM_, unless, void)
import Data.ByteString qualified as Bytes
import Data.Text qualified as Text
import Data.Word (Word64)
import Eclips.Deployment.Configuration
import Eclips.Domain.Identity (HeraldEpoch, heraldEpochBytes)
import Eclips.Protocol.Frame qualified as Frame
import Eclips.Protocol.Oracle.Codec qualified as Oracle
import Eclips.Protocol.Oracle.Frame qualified as Oracle
import Eclips.Protocol.Oracle.Types qualified as Oracle
import Eclips.Protocol.Peer.Codec qualified as Peer
import Eclips.Protocol.Peer.Frame qualified as Peer
import Eclips.Protocol.Peer.Types qualified as Peer
import Eclips.Protocol.Raft.Codec qualified as Raft
import Eclips.Protocol.Raft.Frame qualified as Raft
import Eclips.Protocol.Raft.Types qualified as Raft
import Eclips.Raft.Identity (RaftNodeId, raftNodeIdBytes)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)
import Test.Tasty.HUnit (Assertion, assertEqual)

data Plane = PeerPlane | OraclePlane | RaftPlane

data Source = HeraldSource Bytes.ByteString | RaftSource Bytes.ByteString | DiscoverySource
  deriving stock (Eq)

-- The setter is an external network condition, never a kernel input. Listener
-- allocation and every accepted bridge remain nested under this scope.
withPartitionProxies :: [(HeraldEpoch, DeploymentEndpoints)] -> ((HeraldEpoch -> RaftNodeId -> Bool -> IO ()) -> (HeraldEpoch -> RaftNodeId -> IO ()) -> IO result) -> IO result
withPartitionProxies residents use = do
  blocked <- newTVarIO []
  generation <- newTVarIO (0 :: Word64)
  forwarded <- newTVarIO []
  let configure epoch node closed = atomically $ do
        modifyTVar' blocked $ \current ->
          let remaining = filter ((/= heraldEpochBytes epoch) . fst) current
           in if closed then (heraldEpochBytes epoch, raftNodeIdBytes node) : remaining else remaining
        modifyTVar' generation (+ 1)
        writeTVar forwarded []
      awaitReconnection epoch node = do
        after <- readTVarIO generation
        atomically $ do
          observed <- readTVar forwarded
          check (any (\(seen, target, source) -> seen >= after && target /= epoch && source `elem` [HeraldSource (heraldEpochBytes epoch), RaftSource (raftNodeIdBytes node)]) observed)
      listeners = concatMap lanes residents
      lanes (epoch, planes) =
        [ (epoch, PeerPlane, peerAdvertised planes, peerListen planes),
          (epoch, OraclePlane, oracleAdvertised planes, oracleListen planes),
          (epoch, RaftPlane, raftAdvertised planes, raftListen planes)
        ]
      start [] = use configure awaitReconnection
      start ((epoch, plane, advertised, upstream) : rest) = case advertised of
        Nothing -> fail "partition fixture requires advertised proxy endpoints"
        Just listening -> withProxy blocked generation forwarded epoch plane listening upstream (start rest)
  start listeners

withProxy :: TVar [(Bytes.ByteString, Bytes.ByteString)] -> TVar Word64 -> TVar [(Word64, HeraldEpoch, Source)] -> HeraldEpoch -> Plane -> Endpoint -> Endpoint -> IO result -> IO result
withProxy blocked generation forwarded target plane listening upstream use = bracket (openListener listening) close $ \listener -> do
  connections <- newTVarIO []
  let acceptLoop = forever $ mask_ $ do
        (client, _) <- accept listener
        void (startTrackedJoined connections (close client) (bridge client))
      bridge client = do
        (source, prefix) <- readSource plane client
        (allowed, observedGeneration) <- atomically $ (,) <$> (not . blockedLane target source <$> readTVar blocked) <*> readTVar generation
        if allowed
          then
            raceJoined
              ( bracket (openConnection upstream) close $ \server -> do
                  sendAll server prefix
                  retainedSource <- copySource source
                  atomically $ do
                    currentGeneration <- readTVar generation
                    -- A pre-cut lane can finish its first write after the gate
                    -- changes. It is no evidence of a newly healed connection.
                    if observedGeneration == currentGeneration
                      then modifyTVar' forwarded (rememberForwarded (observedGeneration, target, retainedSource))
                      else pure ()
                  raceJoined (pump client server) (pump server client)
              )
              (atomically (readTVar blocked >>= check . blockedLane target source))
          else pure ()
      cleanup worker = do
        stopJoined worker
        active <- readTVarIO connections
        mapM_ stopJoined active
  bracket (startJoined acceptLoop) cleanup (const use)

blockedLane :: HeraldEpoch -> Source -> [(Bytes.ByteString, Bytes.ByteString)] -> Bool
blockedLane target source blocked = case source of
  DiscoverySource -> False
  HeraldSource epoch -> targetBlocked || epoch `elem` map fst blocked
  RaftSource node -> targetBlocked || node `elem` map snd blocked
  where
    targetBlocked = heraldEpochBytes target `elem` map fst blocked

readSource :: Plane -> Socket -> IO (Source, Bytes.ByteString)
readSource plane connection = prefix Bytes.empty
  where
    prefix retained
      | Bytes.length retained < 8 = more retained >>= prefix
      | PeerPlane <- plane, Bytes.take 4 (Bytes.drop 4 retained) == "EDSC" = pure (DiscoverySource, retained)
      | otherwise = frame Bytes.empty (Frame.initialFrameDecoder magic) retained
    magic = case plane of PeerPlane -> Peer.peerFrameMagic; OraclePlane -> Oracle.oracleFrameMagic; RaftPlane -> Raft.raftFrameMagic
    frame retained decoder bytes = case Frame.feedFrame decoder bytes of
      Frame.FrameFeedResult (body : _) _ -> (,retained <> bytes) <$> classify body
      Frame.FrameFeedResult [] (Frame.NeedFrameBytes next) -> more Bytes.empty >>= frame (retained <> bytes) next
      Frame.FrameFeedResult [] (Frame.FrameFailed failure) -> fail (show failure)
    classify body = case plane of
      PeerPlane -> case Peer.decodePeerEnvelope body of
        Right (Peer.PeerHelloEnvelope hello) -> pure (HeraldSource (Peer.heraldEpochClaimBytes (Peer.peerHelloHeraldEpoch hello)))
        _ -> fail "partition peer lane did not begin with Hello"
      OraclePlane -> case Oracle.decodeOracleEnvelope body of
        Right (Oracle.OracleClientEnvelope (Oracle.OracleHello hello)) -> oracleSource hello
        Right (Oracle.OracleClientEnvelope (Oracle.OracleHealthQuery _ hello)) -> oracleSource hello
        _ -> fail "partition Oracle lane did not begin with Hello or health query"
      RaftPlane -> case Raft.decodeRaftEnvelope body of
        Right (Raft.RaftHello hello) -> pure (RaftSource (Raft.raftNodeIdDtoBytes (Raft.raftHelloSource hello)))
        _ -> fail "partition Raft lane did not begin with Hello"
    oracleSource = pure . HeraldSource . Oracle.heraldEpochClaimBytes . Oracle.oracleHelloHeraldEpoch
    more retained = do
      bytes <- recv connection 32768
      if Bytes.null bytes then fail "partition lane ended before Hello" else pure (retained <> bytes)

pump :: Socket -> Socket -> IO ()
pump source target = do
  bytes <- recv source 32768
  unless (Bytes.null bytes) (sendAll target bytes >> pump source target)

startJoined :: IO () -> IO (ThreadId, TMVar (Either SomeException ()))
startJoined action = mask_ $ do
  done <- newEmptyTMVarIO
  worker <- forkJoined action (pure ()) (putTMVar done)
  pure (worker, done)

-- Acquisitions run masked under bracket, and the accept loop also masks its
-- registration window. Only the supervised action is unmasked; completion is
-- published masked even when cancellation interrupts that action. Install the
-- finalizer before unmasking so resources acquired by the parent are owned even
-- if cancellation arrives before the child reaches its registration gate.
forkJoined :: IO () -> IO () -> (Either SomeException () -> STM ()) -> IO ThreadId
forkJoined action finalize complete = mask_
  $ forkIOWithUnmask
  $ \unmask -> do
    result <- try (unmask action `finally` finalize)
    atomically (complete result)

-- The gate keeps a fast bridge from completing before its scope registers it.
-- Completion removes both its ThreadId and result cell; retaining finished
-- ThreadIds keeps their thread storage alive throughout the polling fixture.
startTrackedJoined :: TVar [(ThreadId, TMVar (Either SomeException ()))] -> IO () -> IO () -> IO (ThreadId, TMVar (Either SomeException ()))
startTrackedJoined connections finalize action = mask_ $ do
  started <- newEmptyTMVarIO
  done <- newEmptyTMVarIO
  worker <-
    forkJoined
      (atomically (takeTMVar started) >> action)
      finalize
      ( \result -> do
          modifyTVar' connections (forgetCompleted done)
          putTMVar done result
      )
  atomically $ do
    modifyTVar' connections ((worker, done) :)
    putTMVar started ()
  pure (worker, done)
  where
    forgetCompleted done workers =
      let retained = filter ((/= done) . snd) workers
       in length retained `seq` retained

-- Decoded claims can be slices of the socket receive buffer. Copy and force
-- just their identity bytes before retaining a route observation.
copySource :: Source -> IO Source
copySource = \case
  HeraldSource bytes -> HeraldSource <$> evaluate (Bytes.copy bytes)
  RaftSource bytes -> RaftSource <$> evaluate (Bytes.copy bytes)
  DiscoverySource -> pure DiscoverySource

-- Only the existence of a reopened route matters, not the history of repeated
-- health requests and discovery polls over that same route.
rememberForwarded :: (Word64, HeraldEpoch, Source) -> [(Word64, HeraldEpoch, Source)] -> [(Word64, HeraldEpoch, Source)]
rememberForwarded observation recorded
  | observation `elem` recorded = recorded
  | otherwise = observation : recorded

casePartitionConnectionScope :: Assertion
casePartitionConnectionScope = do
  completed <- timeout 5_000_000 $ do
    connections <- newTVarIO []
    forM_ [("ordinary", startJoined), ("tracked", startTrackedJoined connections (pure ()))] $ \(kind, start) -> do
      observed <- newEmptyTMVarIO
      bracket
        (start (getMaskingState >>= atomically . putTMVar observed))
        stopJoined
        $ \(_, done) -> do
          assertEqual (kind <> " bracket-acquired action runs unmasked") Unmasked =<< atomically (readTMVar observed)
          atomically (readTMVar done) >>= either throwIO pure
    release <- newEmptyTMVarIO
    bracket
      (startTrackedJoined connections (pure ()) (atomically (readTMVar release)))
      (\active -> atomically (void (tryPutTMVar release ())) >> stopJoined active)
      $ \active -> do
        replicateM_ 256 $ do
          (_, done) <- startTrackedJoined connections (pure ()) (pure ())
          atomically (readTMVar done) >>= either throwIO pure
          assertEqual "completed bridges leave only the live bridge" 1 . length =<< readTVarIO connections
        stopJoined active
        assertEqual "cancellation releases the final bridge before join returns" 0 . length =<< readTVarIO connections
    -- Hold the same child start gate used above closed: cancellation must run
    -- the parent-acquired socket finalizer before the bridge action can begin.
    atGate <- newEmptyTMVarIO
    startGate <- newEmptyTMVarIO
    entered <- newTVarIO False
    closes <- newTVarIO (0 :: Int)
    done <- newEmptyTMVarIO
    bracket
      ( forkJoined
          (atomically (putTMVar atGate ()) >> atomically (readTMVar startGate) >> atomically (writeTVar entered True))
          (atomically (modifyTVar' closes (+ 1)))
          (putTMVar done)
      )
      (\worker -> atomically (void (tryPutTMVar startGate ())) >> stopJoined (worker, done))
      $ \worker -> do
        atomically (readTMVar atGate)
        stopJoined (worker, done)
        assertEqual "cancellation at the start gate closes the acquired socket exactly once" 1 =<< readTVarIO closes
        assertEqual "cancellation never entered the bridge action" False =<< readTVarIO entered
  assertEqual "short bridge completion and shutdown both finish" (Just ()) completed

stopJoined :: (ThreadId, TMVar (Either SomeException ())) -> IO ()
stopJoined (worker, done) = killThread worker >> atomically (void (readTMVar done))

raceJoined :: IO () -> IO () -> IO ()
raceJoined first second =
  bracket (startJoined first) stopJoined $ \(_, firstDone) ->
    bracket (startJoined second) stopJoined $ \(_, secondDone) ->
      atomically (readTMVar firstDone `orElse` readTMVar secondDone) >>= either throwIO pure

openListener :: Endpoint -> IO Socket
openListener endpointValue = do
  address <- addressFor endpointValue
  bracketOnError (socket (addrFamily address) Stream defaultProtocol) close $ \listener -> do
    setSocketOption listener ReuseAddr 1
    bind listener (addrAddress address)
    listen listener 16
    pure listener

openConnection :: Endpoint -> IO Socket
openConnection endpointValue = do
  address <- addressFor endpointValue
  connection <- socket (addrFamily address) Stream defaultProtocol
  connect connection (addrAddress address) `onException` close connection
  pure connection

addressFor :: Endpoint -> IO AddrInfo
addressFor target = do
  addresses <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just (Text.unpack (endpointHost target))) (Just (show (endpointPort target)))
  case addresses of { first : _ -> pure first; [] -> fail "partition endpoint did not resolve" }
