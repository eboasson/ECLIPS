{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OracleHealthTcpProperties (tests) where

import Control.Concurrent (MVar, forkFinally, killThread, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, readTVar)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString qualified as Bytes
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Eclips.Domain.Membership (heraldMembershipGenerationId, heraldMembershipHistoryCurrent)
import Eclips.Herald.OracleClient qualified as Client
import Eclips.Herald.OracleHealth
import Eclips.Herald.Runtime.TCP.Internal.OracleHealth (collectOracleHealthRound, queryOracleHealth)
import Eclips.Herald.Runtime.TCP.Internal.Scope (newTcpContext, stopTcpContext)
import Eclips.Herald.Runtime.TCP.Internal.Types (TcpContext (tcpThreads))
import Eclips.Oracle.Genesis
import Eclips.Oracle.Runtime.TCP (withOracleTcpCluster)
import Eclips.Oracle.State (oracleCheckedMembershipHistory)
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Identity (mkRaftNodeId)
import Network.Socket
import Network.Socket.ByteString (recv)
import Step12Fixtures
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "fresh native health rounds"
    [ testCase "each configured Oracle owner answers through TCP with its exact native configuration" caseAllOwners,
      testCase "a silent endpoint times out without withholding healthy replies or retaining workers" caseSilentEndpoint,
      testCase "a round with no endpoint replies still completes" caseEmptyRound
    ]

claims :: Client.OracleHelloClaims
claims =
  Client.oracleHelloClaims
    (checkedOracleSystemId step12CheckedOracleGenesis)
    (checkedOracleCatalogueDigest step12CheckedOracleGenesis)
    (checkedOracleConfigurationDigest step12CheckedOracleGenesis)
    (checkedOracleInitialProjectionDigest step12CheckedOracleGenesis)
    step12SubmittingHeraldId
    step12SubmittingHeraldEpoch
    (heraldMembershipGenerationId (heraldMembershipHistoryCurrent (oracleCheckedMembershipHistory initial)))
  where
    initial = checked (initialOracle step12CheckedOracleGenesis)

caseAllOwners :: Assertion
caseAllOwners = do
  outcome <- withOracleTcpCluster step12OracleTcpConfiguration $ \cluster ->
    bracket newTcpContext stopTcpContext $ \context -> do
      let contacts = NE.toList (Client.oracleContacts (step12OracleContacts cluster))
          expected = Voter.oracleVoterConfiguration (checked (initialOracle step12CheckedOracleGenesis))
      observations <- within "healthy owner round" (collectOracleHealthRound context 31 100_000 claims contacts)
      sort (map oracleHealthObservationNode observations) @?= sort (map (checked . node) contacts)
      map oracleHealthObservationConfiguration observations @?= replicate (length contacts) (Voter.voterConfigurationNative expected)
      map oracleHealthObservationReference observations @?= replicate (length contacts) (Voter.voterConfigurationNativeRef expected)
      case contacts of
        first : second : _ -> do
          let mismatched = checked (Client.oracleContact (Client.oracleContactNode second) (Client.oracleContactHost first) (Client.oracleContactPort first))
          within "wrong endpoint identity" (queryOracleHealth 32 claims mismatched) >>= (@?= Nothing)
        _ -> assertFailure "health fixture requires three native owners"
      assertNoWorkers context
  either (assertFailure . show) pure outcome
  where
    node = mkRaftNodeId . Client.oracleNodeClaimBytes . Client.oracleContactNode

caseSilentEndpoint :: Assertion
caseSilentEndpoint = do
  outcome <- withOracleTcpCluster step12OracleTcpConfiguration $ \cluster ->
    bracket newTcpContext stopTcpContext $ \context ->
      withSilentEndpoint $ \silent closed -> do
        let healthy = NE.toList (Client.oracleContacts (step12OracleContacts cluster))
        observations <- within "round with a silent endpoint" (collectOracleHealthRound context 41 150_000 claims (silent : healthy))
        length observations @?= length healthy
        within "timed-out socket physically closed" (readMVar closed)
        assertNoWorkers context
  either (assertFailure . show) pure outcome

caseEmptyRound :: Assertion
caseEmptyRound = bracket newTcpContext stopTcpContext $ \context -> do
  within "empty round completion" (collectOracleHealthRound context 51 1000 claims []) >>= (@?= [])
  assertNoWorkers context

assertNoWorkers :: TcpContext -> Assertion
assertNoWorkers context = do
  retained <- atomically (readTVar context.tcpThreads)
  assertBool "completed endpoint workers have been joined and removed" (null retained)

withSilentEndpoint :: (Client.OracleContact -> MVar () -> IO value) -> IO value
withSilentEndpoint use = bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  port <- getSocketName listener >>= \case SockAddrInet value _ -> pure (fromIntegral value); _ -> assertFailure "health listener did not bind IPv4"
  closed <- newEmptyMVar
  done <- newEmptyMVar
  let serve = bracket (fst <$> accept listener) close $ \connection -> do
        let drain = recv connection 4096 >>= \bytes -> if Bytes.null bytes then putMVar closed () else drain
        drain
      contact = checked (Client.oracleContact (checked (Client.oracleNodeClaim (Bytes.replicate 32 249))) "127.0.0.1" port)
  bracket (forkFinally serve (putMVar done)) (\thread -> killThread thread >> void (readMVar done)) $ \_ -> use contact closed

within :: String -> IO value -> IO value
within label action = timeout 2_000_000 action >>= maybe (assertFailure (label <> " timed out")) pure

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
