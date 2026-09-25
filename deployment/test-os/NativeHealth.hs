{-# LANGUAGE OverloadedStrings #-}

-- | OS assertions use the public one-shot EORC health protocol. This observes
-- the native owner independently of the Herald's committed watch projection.
module NativeHealth (awaitNativeConfiguration, readNativeHealth, readNativeHealthAt) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, onException, try)
import Control.Monad (forM)
import Data.ByteString qualified as Bytes
import Data.List (find)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import Eclips.Deployment.Manifest (CheckedDeployment, deploymentCheckedOracleGenesis)
import Eclips.Domain.Membership (heraldMembershipGenerationId, heraldMembershipHistoryCurrent)
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Herald.OracleClient (oracleHelloClaims)
import Eclips.Herald.OracleHealth
import Eclips.Herald.Runtime.Oracle (oracleHealthQueryEnvelope, oracleHealthReplyObservation)
import Eclips.Oracle.Genesis
import Eclips.Oracle.State (oracleCheckedMembershipHistory)
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Oracle.Codec (raftNodeIdClaimToCore)
import Eclips.Protocol.Oracle.Frame
import Eclips.Protocol.Oracle.Types qualified as Protocol
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)
import Test.Tasty.HUnit (assertFailure)

awaitNativeConfiguration :: CheckedDeployment -> HeraldMember -> [Voter.OracleReplicaRegistration] -> Voter.VoterConfiguration -> IO ()
awaitNativeConfiguration plan member registrations configuration = await 1
  where
    await roundNumber = do
      observations <- forM (Voter.voterConfigurationBindings configuration) $ \binding ->
        case find ((== raftVoterBindingNode binding) . Voter.replicaRegistrationNode) registrations of
          Nothing -> assertFailure "native configuration voter has no committed registration"
          Just registration -> readNativeHealth plan member roundNumber registration
      if all (maybe False matches) observations
        then pure ()
        else threadDelay 10_000 >> await (roundNumber + 1)
    matches observation = oracleHealthObservationReference observation == Voter.voterConfigurationNativeRef configuration && oracleHealthObservationConfiguration observation == Voter.voterConfigurationNative configuration

readNativeHealth :: CheckedDeployment -> HeraldMember -> Word64 -> Voter.OracleReplicaRegistration -> IO (Maybe OracleHealthObservation)
readNativeHealth plan member roundNumber registration = case Voter.replicaRegistrationContact registration of
  Nothing -> assertFailure "native voter registration has no advertised contact"
  Just contact -> readNativeHealthAt plan member roundNumber (Voter.replicaContactOracle contact) registration

-- The partition fixture can inspect the native owner through its actual local
-- listener without restoring the advertised route used by any Herald worker.
readNativeHealthAt :: CheckedDeployment -> HeraldMember -> Word64 -> Voter.OracleReplicaEndpoint -> Voter.OracleReplicaRegistration -> IO (Maybe OracleHealthObservation)
readNativeHealthAt plan member roundNumber endpoint registration = do
  outcome <- timeout 100_000 (try @IOException query)
  pure $ case outcome of Just (Right value) -> value; _ -> Nothing
  where
    genesis = deploymentCheckedOracleGenesis plan
    initial = either (error . show) id (initialOracle genesis)
    claims =
      oracleHelloClaims
        (checkedOracleSystemId genesis)
        (checkedOracleCatalogueDigest genesis)
        (checkedOracleConfigurationDigest genesis)
        (checkedOracleInitialProjectionDigest genesis)
        (heraldMemberId member)
        (heraldMemberEpoch member)
        (heraldMembershipGenerationId (heraldMembershipHistoryCurrent (oracleCheckedMembershipHistory initial)))
    acquire = do
      addresses <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just (Text.unpack (TextEncoding.decodeUtf8 (Voter.replicaEndpointHost endpoint)))) (Just (show (Voter.replicaEndpointPort endpoint)))
      case addresses of
        address : _ -> do
          connection <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
          connect connection (addrAddress address) `onException` close connection
          pure connection
        [] -> fail "native health endpoint did not resolve"
    query = bracket acquire close $ \connection -> do
      sendAll connection (encodeOracleFrame (oracleHealthQueryEnvelope roundNumber claims))
      let receive decoder = do
            bytes <- recv connection 4096
            if Bytes.null bytes
              then pure Nothing
              else case feedOracleIngress decoder bytes of
                OracleIngressFeedResult [Protocol.OracleServerEnvelope (Protocol.OracleHealthReply reply)] (NeedOracleIngressBytes _)
                  | Protocol.oracleHealthReplyRound reply == roundNumber,
                    raftNodeIdClaimToCore (Protocol.oracleHealthReplyNode reply) == Right (Voter.replicaRegistrationNode registration) ->
                      pure (either (const Nothing) Just (oracleHealthReplyObservation reply))
                OracleIngressFeedResult [] (NeedOracleIngressBytes next) -> receive next
                _ -> pure Nothing
      receive (initialOracleIngressDecoder oracleClientIngressContext)
