{-# LANGUAGE OverloadedRecordDot #-}

-- | Fresh, one-shot native-owner health queries. A round owns its endpoint
-- workers and sockets; it neither observes Herald state nor uses EPRP lanes.
module Eclips.Herald.Runtime.TCP.Internal.OracleHealth
  ( runOracleHealthManager,
    collectOracleHealthRound,
    queryOracleHealth,
  ) where

import Control.Concurrent (killThread, readMVar)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTQueue, readTVar, registerDelay)
import Control.Exception (IOException, bracket, finally, mask, try)
import Control.Monad (forM, forM_, forever, void)
import Data.ByteString qualified as Bytes
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Word (Word64)
import Eclips.Herald.OracleClient (OracleContact, OracleHelloClaims, oracleContactHost, oracleContactNode, oracleContactPort, oracleNodeClaimBytes)
import Eclips.Herald.OracleHealth (OracleHealthObservation)
import Eclips.Herald.Runtime (HeraldRuntime)
import Eclips.Herald.Runtime.Ingress (submitOracleHealthRoundObserved)
import Eclips.Herald.Runtime.Oracle (oracleHealthQueryEnvelope, oracleHealthReplyObservation)
import Eclips.Herald.Runtime.TCP.Internal.Scope (spawnTcpTrackedChild)
import Eclips.Herald.Runtime.TCP.Internal.Socket (connectTcpEndpoint, receiveTcpSocketBytes, sendTcpSocketBytesDirect)
import Eclips.Herald.Runtime.TCP.Internal.Types (ResolvedTcpEndpoint (..), TcpContext (..), TcpTrackedThread (..))
import Eclips.Protocol.Oracle.Frame
  ( OracleIngressContinuation (..),
    OracleIngressFeedResult (..),
    encodeOracleFrame,
    feedOracleIngress,
    initialOracleIngressDecoder,
    oracleClientIngressContext,
  )
import Eclips.Protocol.Oracle.Types qualified as Protocol
import Eclips.Public.Types.Timing (timingHealthQueryMicroseconds)
import Network.Socket (close)
import System.Timeout (timeout)

runOracleHealthManager :: TcpContext -> HeraldRuntime -> IO ()
runOracleHealthManager context runtime = forever $ do
  (roundNumber, cadence, claims, contacts) <- atomically (readTQueue context.tcpOracleHealthRounds)
  observations <- collectOracleHealthRound context roundNumber cadence claims contacts
  void (submitOracleHealthRoundObserved runtime roundNumber observations)

-- | Query every captured contact concurrently, including a local owner through
-- its normal TCP endpoint. The round always waits for its owner-selected cadence
-- and reports an empty collection when none respond. All endpoint children are
-- cancelled and joined before its one completion becomes visible.
collectOracleHealthRound :: TcpContext -> Word64 -> Word64 -> OracleHelloClaims -> [OracleContact] -> IO [OracleHealthObservation]
collectOracleHealthRound context roundNumber cadence claims contacts = mask $ \restore -> do
  deadline <- registerDelay (fromIntegral cadence)
  replies <- newTVarIO Map.empty
  workers <- fmap catMaybes $ forM (zip [0 :: Int ..] contacts) $ \(ordinal, contact) ->
    spawnTcpTrackedChild context Nothing $ restore $ do
      observed <- timeout (fromIntegral (min (maybe 100_000 timingHealthQueryMicroseconds context.tcpTimingPolicy) cadence)) (try @IOException (queryOracleHealth roundNumber claims contact))
      case observed of
        Just (Right (Just reply)) -> atomically (modifyTVar' replies (Map.insert ordinal reply))
        _ -> pure ()
  restore (atomically (readTVar deadline >>= check))
    `finally` do
      forM_ workers $ \(TcpTrackedThread thread _) -> killThread thread
      forM_ workers $ \(TcpTrackedThread _ done) -> void (readMVar done)
  Map.elems <$> atomically (readTVar replies)

queryOracleHealth :: Word64 -> OracleHelloClaims -> OracleContact -> IO (Maybe OracleHealthObservation)
queryOracleHealth roundNumber claims contact =
  bracket
    (connectTcpEndpoint (ResolvedTcpEndpoint (oracleContactHost contact) (oracleContactPort contact)))
    close
    $ \connection -> do
      sendTcpSocketBytesDirect connection (encodeOracleFrame (oracleHealthQueryEnvelope roundNumber claims))
      let receive decoder = do
            bytes <- receiveTcpSocketBytes connection 4096
            if Bytes.null bytes
              then pure Nothing
              else case feedOracleIngress decoder bytes of
                OracleIngressFeedResult [Protocol.OracleServerEnvelope (Protocol.OracleHealthReply reply)] (NeedOracleIngressBytes _)
                  | Protocol.oracleHealthReplyRound reply == roundNumber,
                    Protocol.raftNodeIdClaimBytes (Protocol.oracleHealthReplyNode reply) == oracleNodeClaimBytes (oracleContactNode contact) ->
                      pure (either (const Nothing) Just (oracleHealthReplyObservation reply))
                OracleIngressFeedResult [] (NeedOracleIngressBytes next) -> receive next
                _ -> pure Nothing
      receive (initialOracleIngressDecoder oracleClientIngressContext)
