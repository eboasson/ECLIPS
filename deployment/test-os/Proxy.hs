{-# LANGUAGE OverloadedStrings #-}

-- | A transport-only fault injector. The first descriptor is the launcher;
-- a distinct later ClaimInitial identifies a prepared child lane. Drop exactly
-- that child's first session-open reply and close its upstream connection.
-- Keep the client connected until the fixture observes that exact child's
-- terminal preparation, so its retry cannot race the Herald's loss observation.
module Proxy (withReplyLossProxy) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, bracket, bracketOnError, finally, mask, onException, throwIO, try)
import Control.Monad (forever, unless, void)
import Data.ByteString qualified as Bytes
import Data.Text qualified as Text
import Eclips.Application.Types.Lifecycle (ConnectionDescriptor)
import Eclips.Deployment.Configuration
import Eclips.Protocol.Application.Frame
import Eclips.Protocol.Application.Types (ApplicationClientDto (ClaimInitial), ApplicationEnvelope (..), ApplicationServerDto (SessionOpened))
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

withReplyLossProxy :: Endpoint -> Endpoint -> (ConnectionDescriptor -> IO ()) -> (IO Bool -> IO result) -> IO result
withReplyLossProxy listening upstream awaitTerminal use = bracket (openListener listening) close $ \listener -> do
  dropped <- newTVarIO Nothing
  faultChild <- newTVarIO Nothing
  lossObserved <- newTVarIO False
  terminalFailure <- newEmptyTMVarIO
  launcherDescriptor <- newTVarIO Nothing
  connections <- newTVarIO []
  acceptDone <- newEmptyTMVarIO
  let worker = forever $ mask $ \restore -> do
        (client, _) <- accept listener
        done <- newEmptyTMVarIO
        thread <- forkFinally (restore (bridge launcherDescriptor faultChild dropped lossObserved terminalFailure client) `finally` close client) (\_ -> atomically (putTMVar done ()))
        -- Scope cancellation cannot land between accepting a socket and
        -- registering its supervised bridge for the final join.
        atomically (modifyTVar' connections ((thread, done) :))
  bracket
    (forkFinally worker (const (atomically (putTMVar acceptDone ()))))
    ( \acceptThread -> do
        killThread acceptThread
        atomically (readTMVar acceptDone)
        active <- readTVarIO connections
        mapM_ (killThread . fst) active
        atomically (mapM_ (readTMVar . snd) active)
    )
    $ \_ -> raceJoined (use (readTVarIO lossObserved)) (atomically (readTMVar terminalFailure) >>= throwIO)
  where
    bridge launcherDescriptor faultChild dropped lossObserved terminalFailure client = do
      faultLane <- newTVarIO False
      (lost, pumpOutcome) <- bracket (openConnection upstream) close $ \server -> do
        childLane <- newTVarIO Nothing
        droppedHere <- newTVarIO Nothing
        outcome <- try @IOException (void (raceJoined (Nothing <$ clientPump launcherDescriptor faultChild faultLane childLane lossObserved client server (initialApplicationFrameDecoder ApplicationClientFrames)) (serverPump childLane faultLane dropped droppedHere server client (initialApplicationFrameDecoder ApplicationServerFrames))))
        -- The client can hit its handshake deadline as the server reply arrives.
        -- Retain the actual drop even if that client's pump wins the join race
        -- with a reset. Async scope cancellation still interrupts this bridge.
        actualDrop <- readTVarIO droppedHere
        pure (actualDrop, outcome)
      case lost of
        Nothing -> do
          reserved <- readTVarIO faultLane
          if reserved
            then reportFailure terminalFailure (either throwIO (const (ioError (userError "the reserved child lane closed before its opening reply could be dropped"))) pumpOutcome)
            else either throwIO pure pumpOutcome
        Just descriptor -> do
          reportFailure terminalFailure (awaitTerminal descriptor)
          atomically (writeTVar lossObserved True)
    reportFailure terminalFailure action = do
      outcome <- try action
      case outcome of
        Left failure -> atomically (putTMVar terminalFailure (failure :: SomeException)) >> throwIO failure
        Right () -> pure ()
    clientPump launcherDescriptor faultChild faultLane childLane lossObserved source target decoder = do
      bytes <- recv source 32768
      unless (Bytes.null bytes) $ case feedApplicationFrame decoder bytes of
        ApplicationFrameFeedResult frames continuation -> do
          atomically $ do
            mapM_ (observeClaim launcherDescriptor faultChild faultLane childLane) frames
            child <- readTVar childLane
            reserved <- readTVar faultChild
            ownsFault <- readTVar faultLane
            -- Holding the first socket open is insufficient: the application
            -- has its own handshake deadline. Reserve its first claim before
            -- forwarding it, and gate every later lane for that exact child.
            case (child, reserved) of
              (Just current, Just selected) | current == selected && not ownsFault -> readTVar lossObserved >>= check
              _ -> pure ()
          sendAll target bytes
          case continuation of
            NeedApplicationFrameBytes next -> clientPump launcherDescriptor faultChild faultLane childLane lossObserved source target next
            ApplicationFrameFailed failure -> ioError (userError (show failure))
    observeClaim launcherDescriptor faultChild faultLane childLane = \case
      ApplicationClientEnvelope (ClaimInitial descriptor _) -> do
        first <- readTVar launcherDescriptor
        case first of
          Nothing -> writeTVar launcherDescriptor (Just descriptor)
          Just launcher -> do
            writeTVar childLane (if descriptor /= launcher then Just descriptor else Nothing)
            reserved <- readTVar faultChild
            if descriptor /= launcher && reserved == Nothing
              then writeTVar faultChild (Just descriptor) >> writeTVar faultLane True
              else pure ()
      _ -> pure ()
    serverPump childLane faultLane dropped droppedHere server client decoder = do
      bytes <- recv server 32768
      if Bytes.null bytes
        then pure Nothing
        else case feedApplicationFrame decoder bytes of
          ApplicationFrameFeedResult frames continuation -> do
            discard <- atomically $ do
              child <- readTVar childLane
              ownsFault <- readTVar faultLane
              already <- readTVar dropped
              case child of
                Just descriptor | ownsFault && already == Nothing && any opened frames -> do
                  writeTVar dropped (Just descriptor)
                  writeTVar droppedHere (Just descriptor)
                  pure (Just descriptor)
                _ -> pure Nothing
            case discard of
              Just descriptor -> pure (Just descriptor)
              Nothing -> do
                sendAll client bytes
                case continuation of
                  NeedApplicationFrameBytes next -> serverPump childLane faultLane dropped droppedHere server client next
                  ApplicationFrameFailed failure -> ioError (userError (show failure))
    opened (ApplicationServerEnvelope SessionOpened {}) = True
    opened _ = False

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
  case addresses of { first : _ -> pure first; [] -> ioError (userError "proxy endpoint did not resolve") }

raceJoined :: forall result. IO result -> IO result -> IO result
raceJoined first second = do
  complete <- newEmptyTMVarIO
  let start action = do
        done <- newEmptyTMVarIO
        worker <- forkFinally action $ \outcome -> atomically (putTMVar done () >> void (tryPutTMVar complete (outcome :: Either SomeException result)))
        pure (worker, done)
      stop (worker, done) = killThread worker >> atomically (readTMVar done)
  bracket (start first) stop $ \_ -> bracket (start second) stop $ \_ -> atomically (takeTMVar complete) >>= either throwIO pure
