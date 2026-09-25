-- | The restricted EDSC family carries opaque onboarding payloads. It never
-- constructs an ordinary peer candidate, binding, or membership fact.
module Eclips.Herald.Runtime.TCP.Internal.Discovery
  ( discoveryFrameMagic,
    receiveFamilyPrefix,
    isDiscoveryPrefix,
    serveDiscoveryExchange,
    exchangeDiscovery,
  )
where

import Control.Exception (IOException, bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.Word (Word32)
import Eclips.Herald.Runtime.TCP.Internal.Socket
  ( connectTcpEndpoint,
    receiveTcpSocketBytes,
    sendTcpSocketBytesDirect,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types (ResolvedTcpEndpoint)
import Eclips.Protocol.Frame
  ( FrameContinuation (..),
    FrameFeedResult (..),
    encodeFrame,
    feedFrame,
    finishFrameDecoder,
    initialFrameDecoder,
  )
import Network.Socket (Socket, close)
import System.Timeout (timeout)

-- | ASCII EDSC distinguishes discovery from ordinary EPRP peer traffic.
discoveryFrameMagic :: Word32
discoveryFrameMagic = 0x45445343

receiveFamilyPrefix :: Socket -> IO ByteString
receiveFamilyPrefix connection = loop Bytes.empty
  where
    loop retained
      | Bytes.length retained == 8 = pure retained
      | otherwise = do
          chunk <- receiveTcpSocketBytes connection (8 - Bytes.length retained)
          if Bytes.null chunk then pure retained else loop (retained <> chunk)

isDiscoveryPrefix :: ByteString -> Bool
isDiscoveryPrefix bytes = Bytes.drop 4 bytes == Bytes.pack [0x45, 0x44, 0x53, 0x43]

serveDiscoveryExchange :: (ByteString -> IO (Maybe ByteString)) -> Socket -> ByteString -> IO ()
serveDiscoveryExchange respond connection prefix = do
  request <- receiveOneFrame connection prefix
  case request of
    Nothing -> pure ()
    Just body -> respond body >>= maybe (pure ()) (sendTcpSocketBytesDirect connection . encodeFrame discoveryFrameMagic)

-- | One attempt has a physical timeout; callers retain the exact logical
-- request and pace retries independently. DNS and sockets remain in this shell.
exchangeDiscovery :: ResolvedTcpEndpoint -> ByteString -> IO (Either String ByteString)
exchangeDiscovery endpoint request = do
  attempted <- try @IOException
    $ timeout 5_000_000
    $ bracket (connectTcpEndpoint endpoint) close
    $ \connection -> do
      sendTcpSocketBytesDirect connection (encodeFrame discoveryFrameMagic request)
      receiveOneFrame connection Bytes.empty
  pure $ case attempted of
    Left problem -> Left (show problem)
    Right Nothing -> Left "discovery exchange timed out"
    Right (Just Nothing) -> Left "discovery stream closed or contained an invalid frame"
    Right (Just (Just response)) -> Right response

receiveOneFrame :: Socket -> ByteString -> IO (Maybe ByteString)
receiveOneFrame connection = feed (initialFrameDecoder discoveryFrameMagic)
  where
    feed decoder chunk = case feedFrame decoder chunk of
      FrameFeedResult [body] (NeedFrameBytes next) ->
        pure (case finishFrameDecoder next of Right () -> Just body; Left _ -> Nothing)
      FrameFeedResult [] (NeedFrameBytes next) -> do
        bytes <- receiveTcpSocketBytes connection 32768
        if Bytes.null bytes then pure Nothing else feed next bytes
      _ -> pure Nothing
