{-# LANGUAGE OverloadedStrings #-}

module AdministrationTcpProperties
  ( tests,
  )
where

import Control.Exception
  ( IOException,
    try,
  )
import Data.ByteString qualified as ByteString
import Eclips.Domain.Identity
  ( systemIdBytes,
  )
import Eclips.Herald.Runtime
  ( HeraldRuntimeConfiguration,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    heraldRuntimeConfiguration,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.TCP
  ( HeraldTcp,
    HeraldTcpConfiguration,
    ResolvedTcpEndpoint,
    awaitHeraldTcpExit,
    heartbeatConfigurationMicroseconds,
    heraldTcpAdministrationEndpoint,
    heraldTcpConfiguration,
    heraldTcpEndpoints,
    peerDialRetryDelayMicroseconds,
    tcpListenEndpoint,
    withHeraldTcpRuntime,
  )
import Eclips.Herald.Runtime.TCP.Internal.Socket (connectTcpEndpoint)
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (HeraldRuntimeDrained, HeraldRuntimeScopeClosed),
  )
import Eclips.Protocol.Admin.Frame
  ( AdminFrameContinuation (..),
    AdminFrameDirection (AdminServerFrames),
    AdminFrameFeedResult (..),
    encodeAdminFrame,
    feedAdminFrame,
    initialAdminFrameDecoder,
  )
import Eclips.Protocol.Admin.Types
  ( AdminClientDto (..),
    AdminCorrelationIdClaim,
    AdminEnvelope (..),
    AdminProcessEndReason (AdminExplicitAdministrativeEnd),
    AdminResultStatusDto (AdminAccepted),
    AdminRoleClaim (ProcessAdministrator),
    AdminServerDto (..),
    adminCorrelationIdClaim,
    adminDeploymentIdClaim,
    adminProcessEpochIdClaim,
  )
import Network.Socket
  ( Socket,
    close,
  )
import Network.Socket.ByteString qualified as SocketBytes
import RuntimeFixtures
  ( fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeedSource,
    fixtureIdentifierBytes,
    fixtureOracleContacts,
    fixtureSystemId,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "process administration TCP"
    [ testCase
        "EADM retains same-binding Start results and ends their delivery lifetime on loss"
        caseReconnectableAdministration,
      testCase
        "EADM rejects a foreign deployment before Start admission"
        caseUnauthorizedHello,
      testCase "EADM drain flushes accepted and final receipts before closing" casePublicDrain
    ]

caseReconnectableAdministration :: Assertion
caseReconnectableAdministration = do
  observed <-
    timeout 5_000_000
      $ withHeraldTcpRuntime fixtureTcpConfiguration
      $ \tcp -> do
        let endpoint = administrationEndpoint tcp
        first <- connectTcpEndpoint endpoint
        sendClient first authorizedHello
        sendClient first firstStart
        assertEqual "first Start result crosses EADM" expectedAccepted
          =<< receiveServer first
        sendClient first firstQuery
        assertEqual "same-binding query retains the exact result" expectedAccepted
          =<< receiveServer first
        close first

        second <- connectTcpEndpoint endpoint
        sendClient second authorizedHello
        sendClient second firstQuery
        assertEqual "a fresh binding cannot query the old lifetime" (AdminAbsent firstCorrelation)
          =<< receiveServer second
        sendClient second firstStart
        assertEqual "new-scope Start is admitted independently" expectedAccepted
          =<< receiveServer second
        sendClient second firstStart
        assertEqual "same-binding retry reoffers the same result" expectedAccepted
          =<< receiveServer second
        sendClient second conflictingStart
        assertEqual "different request under the retained correlation conflicts" expectedConflict
          =<< receiveServer second
        close second
  case observed of
    Nothing -> assertFailure "process administration TCP scenario timed out"
    Just (Left failure) -> assertFailure ("process administration TCP failed: " <> show failure)
    Just (Right ((), HeraldRuntimeScopeClosed)) -> pure ()
    Just (Right (_, other)) -> assertFailure ("unexpected process administration TCP exit: " <> show other)

casePublicDrain :: Assertion
casePublicDrain = do
  observed <- timeout 5_000_000 $ withHeraldTcpRuntime fixtureTcpConfiguration $ \tcp -> do
    connection <- connectTcpEndpoint (administrationEndpoint tcp)
    sendClient connection authorizedHello
    sendClient connection (DrainHerald firstCorrelation)
    let gather decoder accumulated = do
          chunk <- SocketBytes.recv connection 32_768
          if ByteString.null chunk
            then pure accumulated
            else case feedAdminFrame decoder chunk of
              AdminFrameFeedResult envelopes (NeedAdminFrameBytes next) -> gather next (accumulated <> envelopes)
              AdminFrameFeedResult _ (AdminFrameFailed failure) -> assertFailure (show failure)
    replies <- gather (initialAdminFrameDecoder AdminServerFrames) []
    close connection
    assertEqual "both receipts precede EOF" [AdminServerEnvelope (HeraldDrainAccepted firstCorrelation), AdminServerEnvelope (HeraldDrained firstCorrelation)] replies
    -- EOF follows the final physical write; the runtime still has to join its
    -- administration writers and publish the terminal semantic disposition.
    -- Keep the owning callback alive until that completion, as the scope API
    -- requires, instead of racing it with a callback-induced physical close.
    assertEqual "the autonomous drain completes after its final receipts" (Right HeraldRuntimeDrained)
      =<< awaitHeraldTcpExit tcp
  case observed of
    Just (Right ((), HeraldRuntimeDrained)) -> pure ()
    other -> assertFailure ("public drain did not complete: " <> show other)

caseUnauthorizedHello :: Assertion
caseUnauthorizedHello = do
  observed <-
    timeout 5_000_000
      $ withHeraldTcpRuntime fixtureTcpConfiguration
      $ \tcp -> do
        connection <- connectTcpEndpoint (administrationEndpoint tcp)
        sendClient connection unauthorizedHello
        closed <-
          timeout
            2_000_000
            (try (SocketBytes.recv connection 1) :: IO (Either IOException ByteString.ByteString))
        case closed of
          Just (Left _) -> pure ()
          Just (Right bytes) ->
            assertEqual "foreign deployment candidate is closed without a server DTO" ByteString.empty bytes
          Nothing -> assertFailure "foreign deployment candidate remained open"
        close connection
  case observed of
    Nothing -> assertFailure "unauthorized EADM scenario timed out"
    Just (Left failure) -> assertFailure ("unauthorized EADM failed the TCP scope: " <> show failure)
    Just (Right ((), HeraldRuntimeScopeClosed)) -> pure ()
    Just (Right (_, other)) -> assertFailure ("unexpected unauthorized EADM exit: " <> show other)

fixtureTcpConfiguration :: HeraldTcpConfiguration
fixtureTcpConfiguration =
  heraldTcpConfiguration
    fixtureRuntimeConfiguration
    listener
    listener
    listener
    []
    (checked "peer retry delay" (peerDialRetryDelayMicroseconds 1000))
    (checked "heartbeat configuration" (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))
  where
    listener = checked "TCP listener" (tcpListenEndpoint "127.0.0.1" 0)

fixtureRuntimeConfiguration :: HeraldRuntimeConfiguration
fixtureRuntimeConfiguration =
  heraldRuntimeConfiguration
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeedSource
    systemRuntimeMonotonicClock
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    (checked "application recovery" (checkApplicationRecoveryConfiguration 30_000_000))
    (checked "peer recovery" (checkPeerRecoveryConfiguration 30_000_000))

administrationEndpoint :: HeraldTcp -> ResolvedTcpEndpoint
administrationEndpoint = heraldTcpAdministrationEndpoint . heraldTcpEndpoints

sendClient :: Socket -> AdminClientDto -> IO ()
sendClient socketHandle =
  SocketBytes.sendAll socketHandle . encodeAdminFrame . AdminClientEnvelope

receiveServer :: Socket -> IO AdminServerDto
receiveServer socketHandle = go (initialAdminFrameDecoder AdminServerFrames)
  where
    go decoder = do
      chunk <- SocketBytes.recv socketHandle 32_768
      if ByteString.null chunk
        then assertFailure "EADM lane closed before its retained result"
        else case feedAdminFrame decoder chunk of
          AdminFrameFeedResult envelopes continuation ->
            case envelopes of
              [AdminServerEnvelope dto] -> pure dto
              [] -> case continuation of
                NeedAdminFrameBytes successor -> go successor
                AdminFrameFailed problem ->
                  assertFailure ("EADM server frame failed: " <> show problem)
              unexpected ->
                assertFailure ("unexpected EADM server envelope batch: " <> show unexpected)

authorizedHello :: AdminClientDto
authorizedHello =
  AdminHello
    (checked "deployment claim" (adminDeploymentIdClaim (systemIdBytes fixtureSystemId)))
    ProcessAdministrator

unauthorizedHello :: AdminClientDto
unauthorizedHello =
  AdminHello
    (checked "foreign deployment claim" (adminDeploymentIdClaim (fixtureIdentifierBytes 0xee)))
    ProcessAdministrator

firstStart :: AdminClientDto
firstStart = StartProcessEpoch firstCorrelation

firstQuery :: AdminClientDto
firstQuery = GetAdminResult firstCorrelation

conflictingStart :: AdminClientDto
conflictingStart =
  EndProcessEpoch
    firstCorrelation
    (checked "process claim" (adminProcessEpochIdClaim (fixtureIdentifierBytes 0xef)))
    AdminExplicitAdministrativeEnd

firstCorrelation :: AdminCorrelationIdClaim
firstCorrelation = adminCorrelationIdClaim 902

expectedAccepted :: AdminServerDto
expectedAccepted = AdminResult firstCorrelation AdminAccepted

expectedConflict :: AdminServerDto
expectedConflict = AdminConflict firstCorrelation

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
