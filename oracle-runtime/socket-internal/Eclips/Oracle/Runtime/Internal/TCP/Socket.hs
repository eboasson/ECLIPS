{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | Small progress-accounted socket helpers. Framing admission remains in the
-- protocol packages; this module only preserves exact TCP byte boundaries.
module Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( ResolvedTcpEndpoint (..),
    BoundTcpListener (..),
    TcpClientConnection,
    bindLoopbackListener,
    bindTcpListener,
    connectTcpEndpoint,
    connectTcpClientEndpoint,
    prepareConnectedTcpSocket,
    sendSocketBytes,
    receiveSocketBytes,
    sendTcpClientBytes,
    receiveTcpClientRawFrame,
    closeTcpClientConnection,
    TcpSendAttempt (..),
    TcpReceiveAttempt (..),
    sendAllWithProgress,
    receiveWithProgress,
    waitForReadinessOrWatchdogWith,
    readinessWatchdogMicroseconds,
    TcpReadProbe (..),
    probeTcpSocketReadiness,
    receiveExact,
    receiveRawFrame,
    rawFrameBody,
    closeSocketQuietly,
  )
where

import Control.Concurrent.STM
  ( STM,
    atomically,
  )
import Control.Exception
  ( IOException,
    bracket,
    bracketOnError,
    mask,
    onException,
    try,
  )
import Control.Monad (void)
import Data.Bits
  ( shiftL,
    (.|.),
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word
  ( Word16,
    Word32,
  )
#if !defined(mingw32_HOST_OS)
import Data.ByteString.Internal qualified as ByteString.Internal
import Data.ByteString.Unsafe qualified as ByteString.Unsafe
import Data.Word (Word8)
import Foreign.C.Error
  ( eAGAIN,
    eINTR,
    eWOULDBLOCK,
    errnoToIOError,
    getErrno,
  )
import Foreign.C.Types
  ( CInt (..),
    CSize (..),
  )
import Foreign.Ptr
  ( Ptr,
    castPtr,
  )
import Foreign.Marshal.Alloc (allocaBytes)
import System.Posix.Types (CSsize (..))
#endif
import Network.Socket
  ( AddrInfo (..),
    AddrInfoFlag (AI_PASSIVE),
    HostName,
    NameInfoFlag (NI_NUMERICHOST, NI_NUMERICSERV),
    Socket,
    SocketOption (NoDelay, ReuseAddr),
    SocketType (Stream),
    bind,
    close,
    connect,
    defaultHints,
    getAddrInfo,
    getNameInfo,
    getSocketName,
    listen,
    setSocketOption,
    socket,
    waitAndCancelReadSocketSTM,
    waitAndCancelWriteSocketSTM,
    withFdSocket,
  )
#if defined(mingw32_HOST_OS)
import Network.Socket.ByteString qualified as SocketByteString
#endif
import System.Timeout (timeout)

data ResolvedTcpEndpoint = ResolvedTcpEndpoint
  { resolvedTcpHost :: String,
    resolvedTcpPort :: Word16
  }
  deriving stock (Eq, Ord, Show)

data BoundTcpListener = BoundTcpListener
  { boundTcpSocket :: Socket,
    boundTcpEndpoint :: ResolvedTcpEndpoint
  }

newtype TcpClientConnection = TcpClientConnection Socket

data TcpSendAttempt
  = TcpSendWouldBlock
  | TcpSentBytes Word
  deriving stock (Eq, Show)

data TcpReceiveAttempt
  = TcpReceiveWouldBlock
  | TcpReceivedBytes ByteString
  deriving stock (Eq, Show)

waitForReadinessOrWatchdogWith ::
  Int ->
  IO (STM value, IO ()) ->
  IO (Maybe value)
waitForReadinessOrWatchdogWith watchdogMicroseconds register =
  bracket register (snd) $ \(ready, _) ->
    timeout watchdogMicroseconds (atomically ready)

sendAllWithProgress ::
  IO () ->
  (ByteString -> IO TcpSendAttempt) ->
  ByteString ->
  IO ()
sendAllWithProgress waitReady sendAttempt initial =
  mask $ \restore -> loop restore initial
  where
    loop _ remaining | ByteString.null remaining = pure ()
    loop restore remaining =
      sendAttempt remaining >>= \case
        TcpSendWouldBlock -> restore waitReady >> loop restore remaining
        TcpSentBytes 0 -> restore waitReady >> loop restore remaining
        TcpSentBytes sent ->
          loop restore (ByteString.drop (fromIntegral sent) remaining)

receiveWithProgress ::
  IO () ->
  (Int -> IO TcpReceiveAttempt) ->
  Int ->
  IO ByteString
receiveWithProgress waitReady receiveAttempt count =
  mask $ \restore -> loop restore
  where
    loop restore =
      receiveAttempt count >>= \case
        TcpReceiveWouldBlock -> restore waitReady >> loop restore
        TcpReceivedBytes bytes -> pure bytes

readinessWatchdogMicroseconds :: Int
readinessWatchdogMicroseconds = 1_000_000

waitForReadReadiness :: Socket -> IO ()
waitForReadReadiness connection =
  void
    $ waitForReadinessOrWatchdogWith
      readinessWatchdogMicroseconds
      (waitAndCancelReadSocketSTM connection)

waitForWriteReadiness :: Socket -> IO ()
waitForWriteReadiness connection =
  void
    $ waitForReadinessOrWatchdogWith
      readinessWatchdogMicroseconds
      (waitAndCancelWriteSocketSTM connection)

sendSocketBytes :: Socket -> ByteString -> IO ()
#if defined(mingw32_HOST_OS)
sendSocketBytes = SocketByteString.sendAll
#else
sendSocketBytes connection =
  sendAllWithProgress
    (waitForWriteReadiness connection)
    (sendSocketBytesNoWait connection)
#endif

-- Runtime reader owners never catch an asynchronous exception and resume the
-- same stream: an exception aborts the physical lane, and reconnect replay is
-- the recovery boundary. The direct no-wait implementation is POSIX-specific;
-- Windows retains @network@'s blocking fallback without the watchdog guarantee.
receiveSocketBytes :: Socket -> Int -> IO ByteString
#if defined(mingw32_HOST_OS)
receiveSocketBytes = SocketByteString.recv
#else
receiveSocketBytes connection =
  receiveWithProgress
    (waitForReadReadiness connection)
    (receiveSocketBytesNoWait connection)
#endif

bindLoopbackListener :: IO BoundTcpListener
bindLoopbackListener = bindTcpListener (ResolvedTcpEndpoint "127.0.0.1" 0)

-- | Resolve and bind the selected listener at the physical boundary. The
-- resulting numeric address is what callers may advertise after readiness.
bindTcpListener :: ResolvedTcpEndpoint -> IO BoundTcpListener
bindTcpListener endpoint = do
  address <- resolveEndpoint [AI_PASSIVE] endpoint
  bracketOnError
    (socket address.addrFamily Stream address.addrProtocol)
    close
    ( \listener -> do
        setSocketOption listener ReuseAddr 1
        bind listener address.addrAddress
        listen listener 64
        actual <- getSocketName listener
        (host, service) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True actual
        case (host, service >>= readPort) of
          (Just hostname, Just port) ->
            pure (BoundTcpListener listener (ResolvedTcpEndpoint hostname port))
          _ -> ioError (userError "listener has no numeric TCP endpoint")
    )
  where
    readPort value = case reads value of [(port, "")] -> Just port; _ -> Nothing

connectTcpEndpoint :: ResolvedTcpEndpoint -> IO Socket
connectTcpEndpoint endpoint = do
  address <- resolveEndpoint [] endpoint
  bracketOnError
    (socket address.addrFamily Stream address.addrProtocol)
    close
    ( \connection -> do
        connect connection address.addrAddress
        prepareConnectedTcpSocket connection
        pure connection
    )

resolveEndpoint :: [AddrInfoFlag] -> ResolvedTcpEndpoint -> IO AddrInfo
resolveEndpoint flags endpoint = do
  addresses <- getAddrInfo (Just defaultHints {addrSocketType = Stream, addrFlags = flags}) (Just endpoint.resolvedTcpHost :: Maybe HostName) (Just (show endpoint.resolvedTcpPort))
  case addresses of
    first : _ -> pure first
    [] -> ioError (userError "TCP endpoint resolved to no addresses")

-- | Install the latency policy required by every framed TCP lane before the
-- socket can be handed to a protocol reader or writer.  Failure retires this
-- physical attempt rather than leaving a partially configured connection.
prepareConnectedTcpSocket :: Socket -> IO ()
prepareConnectedTcpSocket connection =
  setSocketOption connection NoDelay 1 `onException` close connection

connectTcpClientEndpoint :: ResolvedTcpEndpoint -> IO TcpClientConnection
connectTcpClientEndpoint = fmap TcpClientConnection . connectTcpEndpoint

sendTcpClientBytes :: TcpClientConnection -> ByteString -> IO ()
sendTcpClientBytes (TcpClientConnection connection) = sendSocketBytes connection

receiveTcpClientRawFrame :: TcpClientConnection -> IO ByteString
receiveTcpClientRawFrame (TcpClientConnection connection) = receiveRawFrame connection

closeTcpClientConnection :: TcpClientConnection -> IO ()
closeTcpClientConnection (TcpClientConnection connection) = closeSocketQuietly connection

receiveExact :: Socket -> Int -> IO ByteString
receiveExact connection count = go count []
  where
    go remaining reversed
      | remaining == 0 = pure (ByteString.concat (reverse reversed))
      | otherwise = do
          chunk <- receiveSocketBytes connection remaining
          if ByteString.null chunk
            then ioError (userError "TCP stream ended inside a frame")
            else do
              go (remaining - ByteString.length chunk) (chunk : reversed)

receiveRawFrame :: Socket -> IO ByteString
receiveRawFrame connection = do
  prefix <- receiveExact connection 4
  let declaredWord = decodeWord32 prefix
      declared = fromIntegral declaredWord
  if declared < 4
    then ioError (userError "invalid common frame length")
    else do
      body <- receiveExact connection declared
      pure (prefix <> body)

rawFrameBody :: Word32 -> ByteString -> Either String ByteString
rawFrameBody expected raw
  | ByteString.length raw < 8 = Left "truncated common frame"
  | declared < 4 = Left "invalid common frame length"
  | ByteString.length raw /= declared + 4 = Left "common frame length mismatch"
  | magic /= expected = Left "wrong frame family"
  | otherwise = Right (ByteString.drop 8 raw)
  where
    declared = fromIntegral (decodeWord32 (ByteString.take 4 raw))
    magic = decodeWord32 (ByteString.take 4 (ByteString.drop 4 raw))

closeSocketQuietly :: Socket -> IO ()
closeSocketQuietly connection = void (try @IOException (close connection))

data TcpReadProbe
  = TcpReadPending
  | TcpReadAvailable
  deriving stock (Eq, Show)

-- | Probe for a pending byte or EOF without consuming protocol input. Sockets
-- created and accepted by @network@ are nonblocking on POSIX, so @MSG_PEEK@
-- cannot park this direct syscall.
probeTcpSocketReadiness :: Socket -> IO TcpReadProbe
#if !defined(mingw32_HOST_OS)
probeTcpSocketReadiness connection =
  allocaBytes 1 $ \pointer ->
    fmap (maybe TcpReadPending (const TcpReadAvailable))
      . nonBlockingSocketCall "Eclips.Oracle.Runtime.TCP.peek"
      . withFdSocket connection
      $ \descriptor ->
        c_recv
          descriptor
          pointer
          1
          c_MSG_PEEK
#else
probeTcpSocketReadiness _ =
  ioError (userError "the direct TCP readiness probe is unsupported on Windows")
#endif

#if !defined(mingw32_HOST_OS)
sendSocketBytesNoWait :: Socket -> ByteString -> IO TcpSendAttempt
sendSocketBytesNoWait connection bytes =
  ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(pointer, count) ->
    fmap (maybe TcpSendWouldBlock (TcpSentBytes . fromIntegral))
      . nonBlockingSocketCall "Eclips.Oracle.Runtime.TCP.send"
      . withFdSocket connection
      $ \descriptor ->
        c_send descriptor (castPtr pointer) (fromIntegral count) 0

receiveSocketBytesNoWait :: Socket -> Int -> IO TcpReceiveAttempt
receiveSocketBytesNoWait connection count = do
  (bytes, received) <-
    ByteString.Internal.createAndTrim' count $ \pointer -> do
      outcome <-
        nonBlockingSocketCall "Eclips.Oracle.Runtime.TCP.recv"
          . withFdSocket connection
          $ \descriptor ->
            c_recv descriptor pointer (fromIntegral count) 0
      pure $ case outcome of
        Nothing -> (0, 0, False)
        Just receivedCount -> (0, receivedCount, True)
  pure $
    if received
      then TcpReceivedBytes bytes
      else TcpReceiveWouldBlock

nonBlockingSocketCall :: String -> IO CSsize -> IO (Maybe Int)
nonBlockingSocketCall location action = do
  result <- action
  if result >= 0
    then pure (Just (fromIntegral result))
    else do
      errno <- getErrno
      if errno == eINTR
        then nonBlockingSocketCall location action
        else
          if errno == eAGAIN || errno == eWOULDBLOCK
            then pure Nothing
            else ioError (errnoToIOError location errno Nothing Nothing)

foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSsize

foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSsize

foreign import capi unsafe "sys/socket.h value MSG_PEEK"
  c_MSG_PEEK :: CInt
#endif

decodeWord32 :: ByteString -> Word32
decodeWord32 = ByteString.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0
