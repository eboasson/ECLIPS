{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | Progress-accounted application-side socket IO.  The POSIX backend tries
-- each transfer directly and uses the event manager only after EAGAIN; a
-- one-second watchdog then refreshes a readiness registration which failed to
-- wake despite kernel progress.
module Eclips.Application.Runtime.Internal.Socket
  ( receiveApplicationSocketBytes,
    sendApplicationSocketBytes,
  )
where

import Data.ByteString (ByteString)
#if defined(mingw32_HOST_OS)
import Network.Socket.ByteString qualified as SocketBytes
#else
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (bracket, mask)
import Control.Monad (void)
import Data.ByteString qualified as ByteString
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
import System.Timeout (timeout)
#endif
import Network.Socket (Socket)
#if !defined(mingw32_HOST_OS)
import Network.Socket
  ( waitAndCancelReadSocketSTM,
    waitAndCancelWriteSocketSTM,
    withFdSocket,
  )
#endif

-- | Send one complete application frame.  Windows currently retains
-- @network@'s ordinary blocking implementation and does not claim the
-- readiness-watchdog guarantee.
sendApplicationSocketBytes :: Socket -> ByteString -> IO ()
#if defined(mingw32_HOST_OS)
sendApplicationSocketBytes = SocketBytes.sendAll
#else
sendApplicationSocketBytes connection =
  sendAllWithProgress
    (waitForWriteReadiness connection)
    (sendSocketBytesNoWait connection)
#endif

-- | Read at most the requested bytes, returning an empty string only for EOF.
-- Runtime owners abort this physical attempt on asynchronous exception and
-- never resume a decoder whose last syscall outcome was not observed.
receiveApplicationSocketBytes :: Socket -> Int -> IO ByteString
#if defined(mingw32_HOST_OS)
receiveApplicationSocketBytes = SocketBytes.recv
#else
receiveApplicationSocketBytes connection =
  receiveWithProgress
    (waitForReadReadiness connection)
    (receiveSocketBytesNoWait connection)
#endif

#if !defined(mingw32_HOST_OS)
data SocketSendAttempt
  = SocketSendWouldBlock
  | SocketSentBytes Word

data SocketReceiveAttempt
  = SocketReceiveWouldBlock
  | SocketReceivedBytes ByteString

sendAllWithProgress ::
  IO () ->
  (ByteString -> IO SocketSendAttempt) ->
  ByteString ->
  IO ()
sendAllWithProgress waitReady sendAttempt initial =
  mask $ \restore -> loop restore initial
  where
    loop _ remaining | ByteString.null remaining = pure ()
    loop restore remaining =
      sendAttempt remaining >>= \case
        SocketSendWouldBlock -> restore waitReady >> loop restore remaining
        SocketSentBytes 0 -> restore waitReady >> loop restore remaining
        SocketSentBytes sent ->
          loop restore (ByteString.drop (fromIntegral sent) remaining)

receiveWithProgress ::
  IO () ->
  (Int -> IO SocketReceiveAttempt) ->
  Int ->
  IO ByteString
receiveWithProgress waitReady receiveAttempt count =
  mask $ \restore -> loop restore
  where
    loop restore =
      receiveAttempt count >>= \case
        SocketReceiveWouldBlock -> restore waitReady >> loop restore
        SocketReceivedBytes bytes -> pure bytes

readinessWatchdogMicroseconds :: Int
readinessWatchdogMicroseconds = 1_000_000

waitForReadReadiness :: Socket -> IO ()
waitForReadReadiness connection =
  waitForReadinessOrWatchdog
    (waitAndCancelReadSocketSTM connection)

waitForWriteReadiness :: Socket -> IO ()
waitForWriteReadiness connection =
  waitForReadinessOrWatchdog
    (waitAndCancelWriteSocketSTM connection)

waitForReadinessOrWatchdog :: IO (STM (), IO ()) -> IO ()
waitForReadinessOrWatchdog register =
  bracket register snd $ \(ready, _) ->
    void
      ( timeout
          readinessWatchdogMicroseconds
          (atomically ready)
      )

sendSocketBytesNoWait :: Socket -> ByteString -> IO SocketSendAttempt
sendSocketBytesNoWait connection bytes =
  ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(pointer, count) ->
    fmap (maybe SocketSendWouldBlock (SocketSentBytes . fromIntegral))
      . nonBlockingSocketCall "Eclips.Application.Runtime.send"
      . withFdSocket connection
      $ \descriptor ->
        c_send descriptor (castPtr pointer) (fromIntegral count) 0

receiveSocketBytesNoWait :: Socket -> Int -> IO SocketReceiveAttempt
receiveSocketBytesNoWait connection count = do
  (bytes, received) <-
    ByteString.Internal.createAndTrim' count $ \pointer -> do
      outcome <-
        nonBlockingSocketCall "Eclips.Application.Runtime.recv"
          . withFdSocket connection
          $ \descriptor ->
            c_recv descriptor pointer (fromIntegral count) 0
      pure $ case outcome of
        Nothing -> (0, 0, False)
        Just receivedCount -> (0, receivedCount, True)
  pure $
    if received
      then SocketReceivedBytes bytes
      else SocketReceiveWouldBlock

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
