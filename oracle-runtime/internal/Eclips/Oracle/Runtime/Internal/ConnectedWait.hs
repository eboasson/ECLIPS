{-# LANGUAGE CPP #-}

-- | A scoped race between an STM-owned state transition and physical peer
-- readability.  No worker performs a blocking socket receive.
module Eclips.Oracle.Runtime.Internal.ConnectedWait
  ( ConnectedWaitOutcome (..),
    awaitWhilePeerConnected,
    awaitWhilePeerConnectedAfterRegistration,
  )
where

import Control.Concurrent.STM
  ( STM,
    atomically,
    orElse,
  )
import Control.Exception
  ( mask,
    onException,
  )
#if defined(mingw32_HOST_OS)
import Control.Exception (bracket)
#endif
#if !defined(mingw32_HOST_OS)
import Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( TcpReadProbe (..),
    probeTcpSocketReadiness,
    readinessWatchdogMicroseconds,
    waitForReadinessOrWatchdogWith,
  )
#endif
import Network.Socket
  ( Socket,
    waitAndCancelReadSocketSTM,
  )

data ConnectedWaitOutcome value
  = ConnectedPeerEnded
  | ConnectedStateChanged value
  deriving stock (Eq, Show)

-- | Prefer an already-actionable state without registering any socket wait.
-- Only a state that really retries installs the event-manager readiness token.
-- Readability means EOF or invalid input before the parked Hello response, so
-- either condition retires the connection without consuming bytes.
awaitWhilePeerConnected :: STM value -> Socket -> IO (ConnectedWaitOutcome value)
awaitWhilePeerConnected =
  awaitWhilePeerConnectedAfterRegistration (pure ())

-- | Variant which reports that the physical readability token has been
-- installed.  This is useful to coordinate an owner that must know a wait is
-- genuinely parked; the callback runs with asynchronous exceptions masked and
-- before the combined STM wait begins.
awaitWhilePeerConnectedAfterRegistration ::
  IO () ->
  STM value ->
  Socket ->
  IO (ConnectedWaitOutcome value)
#if defined(mingw32_HOST_OS)
awaitWhilePeerConnectedAfterRegistration registered awaitState connection =
  mask $ \restore -> do
    immediate <- restore (pollConnectedState awaitState)
    case immediate of
      Just value -> pure (ConnectedStateChanged value)
      Nothing ->
        bracket
          (do
             registration@(_, unregister) <- waitAndCancelReadSocketSTM connection
             registered `onException` unregister
             pure registration
          )
          snd
          ( \(peerReadable, _) ->
              restore
                ( atomically
                    ( (peerReadable >> pure ConnectedPeerEnded)
                        `orElse` (ConnectedStateChanged <$> awaitState)
                    )
                )
          )
#else
awaitWhilePeerConnectedAfterRegistration registered awaitState connection =
  awaitWhilePeerConnectedAfterRegistrationWithWatchdog
    readinessWatchdogMicroseconds
    (waitAndCancelReadSocketSTM connection)
    (probeTcpSocketReadiness connection)
    registered
    awaitState
#endif

#if !defined(mingw32_HOST_OS)
-- | POSIX watchdog loop with injectable registration and probe seams.  A timed
-- out readiness token is cancelled before the direct non-consuming probe.  A
-- pending probe rechecks state before it installs the next token; a proven byte
-- or EOF retains the peer-first result of the combined STM race.
awaitWhilePeerConnectedAfterRegistrationWithWatchdog ::
  Int ->
  IO (STM (), IO ()) ->
  IO TcpReadProbe ->
  IO () ->
  STM value ->
  IO (ConnectedWaitOutcome value)
awaitWhilePeerConnectedAfterRegistrationWithWatchdog watchdogMicroseconds registerPeer probePeer registered awaitState =
  mask $ \restore ->
    let loop = do
          immediate <- restore (pollConnectedState awaitState)
          case immediate of
            Just value -> pure (ConnectedStateChanged value)
            Nothing -> do
              observed <-
                restore
                  $ waitForReadinessOrWatchdogWith
                    watchdogMicroseconds
                  $ do
                    (peerReadable, unregister) <- registerPeer
                    registered `onException` unregister
                    pure
                      ( (peerReadable >> pure ConnectedPeerEnded)
                          `orElse` (ConnectedStateChanged <$> awaitState),
                        unregister
                      )
              case observed of
                Just outcome -> pure outcome
                Nothing -> do
                  probePeer >>= \case
                    TcpReadAvailable -> pure ConnectedPeerEnded
                    TcpReadPending -> loop
     in loop
#endif

pollConnectedState :: STM value -> IO (Maybe value)
pollConnectedState awaitState =
  atomically ((Just <$> awaitState) `orElse` pure Nothing)
