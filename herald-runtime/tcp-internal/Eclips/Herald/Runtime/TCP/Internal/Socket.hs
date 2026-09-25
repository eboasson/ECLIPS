{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | Numeric listener construction, progress-accounted stream IO, and
-- idempotent socket close.
module Eclips.Herald.Runtime.TCP.Internal.Socket
  ( bindTcpListener,
    connectTcpEndpoint,
    prepareConnectedTcpSocket,
    prepareConnectedTcpSocketWith,
    TcpSocketWriter,
    newTcpSocketWriter,
    sendTcpSocketBytesDirect,
    sendTcpSocketBytes,
    sendTcpSocketBytesClaimed,
    sendTcpSocketBytesWith,
    receiveTcpSocketBytes,
    TcpSendAttempt (..),
    TcpReceiveAttempt (..),
    sendAllWithProgress,
    receiveWithProgress,
    waitForReadinessOrWatchdogWith,
    retireSocketQuietly,
    closeSocketOnce,
    closeSocketOnceWithProbe,
  )
where

import Control.Concurrent
  ( MVar,
    newMVar,
    withMVar,
  )
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    readTVar,
    writeTVar,
  )
import Control.Exception
  ( IOException,
    bracket,
    mask,
    onException,
    try,
  )
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
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
import System.Posix.Types (CSsize (..))
#endif
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( ResolvedTcpEndpoint (..),
    TcpListenEndpoint (..),
  )
import Network.Socket
  ( AddrInfo (..),
    AddrInfoFlag (..),
    ShutdownCmd (ShutdownBoth),
    SockAddr (..),
    Socket,
    SocketOption (..),
    SocketType (..),
    addrAddress,
    addrFamily,
    addrProtocol,
    addrSocketType,
    bind,
    close,
    connect,
    defaultHints,
    getAddrInfo,
    getSocketName,
    listen,
    setSocketOption,
    shutdown,
    socket,
    waitAndCancelReadSocketSTM,
    waitAndCancelWriteSocketSTM,
    withFdSocket,
  )
#if defined(mingw32_HOST_OS)
import Network.Socket.ByteString qualified as SocketBytes
#endif
import System.Timeout (timeout)

-- | The one write-serialization lock owned by a physical stream socket.
newtype TcpSocketWriter = TcpSocketWriter (MVar ())

-- | Allocate a writer before any protocol path can use the socket.
newTcpSocketWriter :: IO TcpSocketWriter
newTcpSocketWriter = TcpSocketWriter <$> newMVar ()

-- | Result of one nonblocking stream write. A positive progress result is
-- committed exactly once before the remaining suffix is retried.
data TcpSendAttempt
  = TcpSendWouldBlock
  | TcpSentBytes Word
  deriving stock (Eq, Show)

-- | Result of one nonblocking stream read. An empty received chunk is the
-- ordinary TCP EOF indication; it is distinct from would-block.
data TcpReceiveAttempt
  = TcpReceiveWouldBlock
  | TcpReceivedBytes ByteString
  deriving stock (Eq, Show)

-- | The IO-manager readiness registration is the only operation subject to
-- the watchdog. A timeout merely cancels that registration and returns so the
-- caller can retry a nonblocking system call; it never interrupts an
-- operation which may already have transferred bytes.
waitForReadinessOrWatchdogWith ::
  Int ->
  IO (STM value, IO ()) ->
  IO (Maybe value)
waitForReadinessOrWatchdogWith watchdogMicroseconds register =
  bracket register (snd) $ \(ready, _) ->
    timeout watchdogMicroseconds (atomically ready)

-- | Send a complete byte string while retaining exact progress across
-- would-block retries. The send attempt runs masked, so an asynchronous
-- exception cannot land between a successful system call and advancing the
-- retained suffix. Only the readiness wait is restored to interruptibility.
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

-- | Retry one nonblocking receive after readiness or watchdog expiry. No byte
-- transfer is wrapped in an asynchronous timeout.
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
waitForReadReadiness socketHandle =
  void
    $ waitForReadinessOrWatchdogWith
      readinessWatchdogMicroseconds
      (waitAndCancelReadSocketSTM socketHandle)

waitForWriteReadiness :: Socket -> IO ()
waitForWriteReadiness socketHandle =
  void
    $ waitForReadinessOrWatchdogWith
      readinessWatchdogMicroseconds
      (waitAndCancelWriteSocketSTM socketHandle)

-- | Write a complete stream payload without a writer lock. Callers sharing a
-- physical socket must normally prefer 'sendTcpSocketBytes'.
sendTcpSocketBytesDirect :: Socket -> ByteString -> IO ()
#if defined(mingw32_HOST_OS)
sendTcpSocketBytesDirect = SocketBytes.sendAll
#else
sendTcpSocketBytesDirect socketHandle =
  sendAllWithProgress
    (waitForWriteReadiness socketHandle)
    (sendSocketBytesNoWait socketHandle)
#endif

-- | Read at most the requested number of bytes, returning an empty string at
-- EOF. The ordinary path is a direct nonblocking read; the IO manager is only
-- consulted after the kernel reports would-block. Runtime reader owners must
-- not catch an asynchronous exception and resume this same stream: every such
-- exception aborts the physical lane, after which protocol replay or observed
-- connection loss supplies the recovery boundary.
--
-- The direct no-wait implementation is POSIX-specific. The Windows fallback
-- retains @network@'s ordinary blocking operation and does not claim the
-- readiness-watchdog guarantee.
receiveTcpSocketBytes :: Socket -> Int -> IO ByteString
#if defined(mingw32_HOST_OS)
receiveTcpSocketBytes = SocketBytes.recv
#else
receiveTcpSocketBytes socketHandle =
  receiveWithProgress
    (waitForReadReadiness socketHandle)
    (receiveSocketBytesNoWait socketHandle)
#endif

-- | Write one complete framed message without interleaving it with semantic,
-- publication, or heartbeat traffic on the same socket.
sendTcpSocketBytes :: TcpSocketWriter -> Socket -> ByteString -> IO ()
sendTcpSocketBytes = sendTcpSocketBytesWith sendTcpSocketBytesDirect

-- | Serialize a phase-sensitive write and recheck its claim only after the
-- physical writer lock is held. A terminal writer can therefore quiesce the
-- phase while an older heartbeat writer is waiting without allowing that
-- heartbeat to overtake the terminal frame.
sendTcpSocketBytesClaimed ::
  STM Bool ->
  STM () ->
  TcpSocketWriter ->
  Socket ->
  ByteString ->
  IO Bool
sendTcpSocketBytesClaimed claim complete (TcpSocketWriter lock) socketHandle bytes =
  withMVar lock $ \_ -> do
    permitted <- atomically claim
    if permitted
      then do
        sendTcpSocketBytesDirect socketHandle bytes
        atomically complete
        pure True
      else pure False

-- | Package-private send seam used to prove that every physical writer path
-- shares the same non-interleaving critical section.
sendTcpSocketBytesWith ::
  (Socket -> ByteString -> IO ()) ->
  TcpSocketWriter ->
  Socket ->
  ByteString ->
  IO ()
sendTcpSocketBytesWith sendBytes (TcpSocketWriter lock) socketHandle bytes =
  withMVar lock (const (sendBytes socketHandle bytes))

#if !defined(mingw32_HOST_OS)
sendSocketBytesNoWait :: Socket -> ByteString -> IO TcpSendAttempt
sendSocketBytesNoWait socketHandle bytes =
  ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(pointer, count) ->
    fmap (maybe TcpSendWouldBlock (TcpSentBytes . fromIntegral))
      . nonBlockingSocketCall "Eclips.Herald.Runtime.TCP.send"
      . withFdSocket socketHandle
      $ \descriptor ->
        c_send
          descriptor
          (castPtr pointer)
          (fromIntegral count)
          0

receiveSocketBytesNoWait :: Socket -> Int -> IO TcpReceiveAttempt
receiveSocketBytesNoWait socketHandle count = do
  (bytes, received) <-
    ByteString.Internal.createAndTrim' count $ \pointer -> do
      outcome <-
        nonBlockingSocketCall "Eclips.Herald.Runtime.TCP.recv"
          . withFdSocket socketHandle
          $ \descriptor ->
            c_recv
              descriptor
              pointer
              (fromIntegral count)
              0
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
#endif

-- | Bind one numeric listener and report its resolved port.
bindTcpListener :: TcpListenEndpoint -> IO (Socket, ResolvedTcpEndpoint)
bindTcpListener (TcpListenEndpoint host port) = do
  addresses <-
    getAddrInfo
      ( Just
          defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV, AI_PASSIVE],
              addrSocketType = Stream
            }
      )
      (Just (Text.unpack host))
      (Just (show port))
  case addresses of
    [] -> ioError (userError "numeric TCP listener resolved to no address")
    address : _ -> do
      listener <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
      ( do
          setSocketOption listener ReuseAddr 1
          bind listener (addrAddress address)
          listen listener 128
        )
        `onException` close listener
      bound <- getSocketName listener
      resolvedPort <- case bound of
        SockAddrInet actual _ -> pure (fromIntegral actual)
        SockAddrInet6 actual _ _ _ -> pure (fromIntegral actual)
        _ -> ioError (userError "TCP listener did not bind an IP endpoint")
      pure (listener, ResolvedTcpEndpoint host resolvedPort)

-- | Connect one already numeric, resolved endpoint without invoking DNS.
connectTcpEndpoint :: ResolvedTcpEndpoint -> IO Socket
connectTcpEndpoint (ResolvedTcpEndpoint host port) = do
  addresses <-
    getAddrInfo
      ( Just
          defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
              addrSocketType = Stream
            }
      )
      (Just (Text.unpack host))
      (Just (show port))
  case addresses of
    [] -> ioError (userError "numeric TCP endpoint resolved to no address")
    address : _ -> do
      connection <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
      connect connection (addrAddress address) `onException` close connection
      prepareConnectedTcpSocket connection
      pure connection

-- | Configure one connected or accepted stream before protocol traffic begins.
-- Failure closes the physical attempt, so no caller can accidentally promote a
-- socket whose latency policy was not installed.
prepareConnectedTcpSocket :: Socket -> IO ()
prepareConnectedTcpSocket connection =
  prepareConnectedTcpSocketWith
    (\socketHandle -> setSocketOption socketHandle NoDelay 1)
    connection

-- | Package-private option-installation seam. It keeps failure cleanup
-- executable without depending on a platform-specific way to make NoDelay
-- fail on an otherwise healthy TCP socket.
prepareConnectedTcpSocketWith :: (Socket -> IO ()) -> Socket -> IO ()
prepareConnectedTcpSocketWith configure connection =
  configure connection `onException` close connection

-- | Make an owned stream unusable and wake its blocked reader without
-- releasing the descriptor.  Foreign workers retire a lane this way; the
-- reader's enclosing owner performs the eventual physical close.
retireSocketQuietly :: Socket -> IO ()
retireSocketQuietly socketHandle =
  void (try (shutdown socketHandle ShutdownBoth) :: IO (Either IOException ()))

-- | Idempotently close a socket across reader, writer, and supervisor races.
-- The completion fact is published only after the interruptible descriptor
-- close returns, so an interrupted closer leaves the retained close action
-- able to retry. 'Network.Socket.close' itself is idempotent, which makes the
-- small race between concurrent closers harmless without holding an
-- uninterruptible lock across a potentially blocking system call.
closeSocketOnce :: TVar Bool -> Socket -> IO ()
closeSocketOnce = closeSocketOnceWithProbe (pure ())

-- | Package-private interruption seam for the descriptor-close property.
-- Production callers use 'closeSocketOnce', whose probe is empty.
closeSocketOnceWithProbe :: IO () -> TVar Bool -> Socket -> IO ()
closeSocketOnceWithProbe beforeClose closed socketHandle =
  mask $ \restore -> do
    alreadyClosed <- atomically (readTVar closed)
    unless alreadyClosed $ do
      beforeClose
      restore $ do
        -- Wake a reader blocked in recv before releasing the descriptor. Some
        -- platforms do not promptly interrupt another thread's recv on close
        -- alone, which can retain its runtime lease until scope teardown.
        retireSocketQuietly socketHandle
        close socketHandle
      atomically (writeTVar closed True)
