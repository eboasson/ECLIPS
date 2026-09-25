-- | Fresh native-owner observations over the actual one-shot EORC lane.
module HealthRuntimeProperties (tests, queryHealth, queryHealthAs) where

import Control.Exception (IOException, bracket, try)
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch, HeraldId, controlIndex)
import Eclips.Domain.Membership (heraldMembershipGenerationId, heraldMembershipHistoryCurrent)
import Eclips.Oracle.Genesis
import Eclips.Oracle.Runtime
import Eclips.Oracle.Runtime.Internal.TCP.Socket
import Eclips.Oracle.Runtime.TCP
import Eclips.Oracle.State (oracleCheckedMembershipHistory)
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Protocol.Oracle.Codec
import Eclips.Protocol.Oracle.Frame
import Eclips.Protocol.Oracle.Types
import Eclips.Raft.Configuration
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestFixtures

tests :: TestTree
tests =
  testGroup
    "native owner health"
    [ testCase "p09-health-owner: unavailable quorum still replies freshly and stopped owner does not" caseOwner,
      testCase "p09-health-tcp: leaderless EORC returns native health before ordinary Hello readiness" caseTcp
    ]

within :: IO a -> IO a
within action = timeout 2_000_000 action >>= maybe (assertFailure "native health observation did not complete") pure

caseOwner :: Assertion
caseOwner = do
  runtime <- startOracleRuntime fixtureFirstRuntimeConfiguration >>= either (assertFailure . show) pure
  bracket (pure runtime) stopOracleRuntime $ \live -> do
    observation <- within (oracleRuntimeHealth live) >>= maybe (assertFailure "live owner returned no health") pure
    oracleHealthNode observation @?= fixtureInitialLeaderNode
    oracleHealthGeneration observation @?= 0
    let expected = stableRaftConfiguration (checked "voters" (raftVoterSet fixtureRaftNodes))
    oracleHealthEffectiveConfiguration observation @?= (genesisRaftConfigurationRef, expected)
    status <- oracleRuntimeStatus live
    oracleRuntimeServiceReady status @?= False
    next <- within (oracleRuntimeHealth live) >>= maybe (assertFailure "second owner response absent") pure
    oracleHealthGeneration next @?= oracleHealthGeneration observation
  within (oracleRuntimeHealth runtime) >>= (@?= Nothing)

caseTcp :: Assertion
caseTcp = withUnavailablePeer $ \second -> withUnavailablePeer $ \third -> do
  single <- either (assertFailure . show) pure (oracleTcpClusterConfiguration [fixtureFirstRuntimeConfiguration])
  configuration <-
    either
      (assertFailure . show)
      pure
      (configureOracleTcpReplicaContacts (zip (drop 1 fixtureRaftNodes) [second, third]) single)
  result <- withOracleTcpCluster configuration $ \cluster -> do
    endpoint <- maybe (assertFailure "local EORC endpoint missing") pure (lookup fixtureInitialLeaderNode (oracleTcpOracleContacts cluster))
    observed <- within (queryHealth fixtureCheckedGenesis endpoint 23)
    oracleHealthReplyRound observed @?= 23
    raftNodeIdClaimToCore (oracleHealthReplyNode observed) @?= Right fixtureInitialLeaderNode
    oracleHealthReplyGeneration observed @?= 0
    let expected = stableRaftConfiguration (checked "voters" (raftVoterSet fixtureRaftNodes))
    oracleHealthReplyConfiguration observed @?= (genesisRaftConfigurationRef, expected)
  either (assertFailure . show) pure result

-- Reserve the other native endpoints without starting native owners. Their
-- transport can connect, but no owner can supply health, vote or append evidence.
-- The cluster scope joins those blocked transports before these listeners close.
withUnavailablePeer :: (OracleTcpListenEndpoint -> IO a) -> IO a
withUnavailablePeer use = bracket bindLoopbackListener (closeSocketQuietly . boundTcpSocket) $ \listener ->
  let endpoint = boundTcpEndpoint listener
   in either (assertFailure . show) use (oracleTcpListenEndpoint endpoint.resolvedTcpHost endpoint.resolvedTcpPort)

queryHealth :: CheckedOracleGenesis -> OracleTcpEndpoint -> Word64 -> IO OracleHealthReplyDto
queryHealth genesis endpoint roundNumber =
  queryHealthAs genesis fixtureHeraldId fixtureHeraldEpoch endpoint roundNumber
    >>= maybe (assertFailure "active source health query refused") pure

queryHealthAs :: CheckedOracleGenesis -> HeraldId -> HeraldEpoch -> OracleTcpEndpoint -> Word64 -> IO (Maybe OracleHealthReplyDto)
queryHealthAs genesis herald epoch endpoint roundNumber = bracket
  (connectTcpEndpoint (ResolvedTcpEndpoint (oracleTcpEndpointHost endpoint) (oracleTcpEndpointPort endpoint)))
  closeSocketQuietly
  $ \connection -> do
    let initial = checked "health Oracle genesis" (initialOracle genesis)
        membership = heraldMembershipGenerationId (heraldMembershipHistoryCurrent (oracleCheckedMembershipHistory initial))
        hello =
          oracleHelloDto
            (systemIdClaimFromDomain (checkedOracleSystemId genesis))
            (catalogueDigestClaimFromDomain (checkedOracleCatalogueDigest genesis))
            (configurationDigestClaimFromDomain (checkedOracleConfigurationDigest genesis))
            (initialProjectionDigestClaimFromDomain (checkedOracleInitialProjectionDigest genesis))
            (heraldIdClaimFromDomain herald)
            (heraldEpochClaimFromDomain epoch)
            (controlIndexDtoFromDomain (controlIndex 0))
            (heraldMembershipGenerationClaimFromDomain membership)
    sendSocketBytes connection (encodeOracleFrame (OracleClientEnvelope (OracleHealthQuery roundNumber hello)))
    received <- try @IOException (receiveRawFrame connection)
    case received of
      Left _ -> pure Nothing
      Right bytes -> case feedOracleIngress (initialOracleIngressDecoder oracleClientIngressContext) bytes of
        OracleIngressFeedResult [OracleServerEnvelope (OracleHealthReply reply)] (NeedOracleIngressBytes _) -> do
          oracleHealthReplyRound reply @?= roundNumber
          pure (Just reply)
        other -> assertFailure ("unexpected health response: " <> show other)
