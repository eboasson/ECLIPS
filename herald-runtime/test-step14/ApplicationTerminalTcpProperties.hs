{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ApplicationTerminalTcpProperties
  ( tests,
  )
where

import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    tryPutMVar,
  )
import Control.Exception (finally)
import Control.Monad (forM_, void, when)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Eclips.Domain.Identity
  ( bootstrapManifestIdBytes,
    controlIndex,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ApplicationPermanentlyLost),
  )
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.EffectBatch
  ( HeraldEffect (RunOracleClientAction),
    effectBatchMembers,
  )
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction (BindOracleConnection, ConnectAndHelloOracle),
    OracleClientIngress (OracleEntriesReceived, OracleHelloReceived),
    OracleConnectAttempt,
    OracleContactSet,
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.Runtime
  ( checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    heraldRuntimeConfiguration,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.Ingress
  ( RuntimeSubmission (Queued),
    submitOracleClientIngress,
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (batchEnvelopeEffects),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    withHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.TCP
  ( HeraldTcpFailure,
    awaitHeraldTcpExit,
    heartbeatConfigurationMicroseconds,
    heraldTcpApplicationEndpoint,
    heraldTcpConfiguration,
    heraldTcpEndpoints,
    peerDialRetryDelayMicroseconds,
    requestHeraldTcpDrain,
    tcpListenEndpoint,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( HeraldRuntimeScope (HeraldRuntimeScope),
    setHeraldTcpOracleActionProbe,
    withHeraldTcpRuntimeInScopeInternal,
  )
import Eclips.Herald.Runtime.TCP.Internal.Socket (connectTcpEndpoint)
import Eclips.Herald.Runtime.TCP.Internal.Types (HeraldTcp (HeraldTcp))
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (HeraldRuntimeDrained))
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalizeAppliedOracleEntry,
  )
import Eclips.Oracle.Command
  ( endProcessEpochCommand,
    oracleEnvelope,
  )
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Protocol.Application.Frame
  ( ApplicationFrameContinuation (NeedApplicationFrameBytes),
    ApplicationFrameDirection (ApplicationServerFrames),
    ApplicationFrameFeedResult (ApplicationFrameFeedResult),
    encodeApplicationFrame,
    feedApplicationFrame,
    initialApplicationFrameDecoder,
  )
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (OpenSession),
    ApplicationEnvelope (ApplicationClientEnvelope, ApplicationServerEnvelope),
    ApplicationServerDto (HeraldPermanentlyUnavailable, SessionOpened),
    ApplicationSessionClaim,
    ApplicationSessionUnavailableReasonDto (ApplicationSessionNoLongerLiveDto),
    applicationAttachmentClaim,
    applicationClientNonce,
  )
import Network.Socket (Socket, close)
import Network.Socket.ByteString qualified as SocketBytes
import Step12Fixtures
  ( step12CheckedOracleGenesis,
    step12H1Bootstraps,
    step12H1GeneratorSeedSource,
    step12H1Genesis,
    step12H1HeraldEpoch,
    step12H1ProcessBootstrapId,
    step12H1ProcessEpochId,
    withReservedUnavailableOracleContacts,
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
    "application terminal TCP"
    [ testCase
        "canonical resident End sends one terminal EAPP frame before EOF"
        caseCanonicalEndClosesLiveSession
    ]

-- Keep one raw established lane physically live from Open through the canonical
-- End.  The only close is therefore the runtime's terminal CloseWriter Nothing;
-- no client recovery loop, synthetic binding loss, or redial can cause success.
caseCanonicalEndClosesLiveSession :: Assertion
caseCanonicalEndClosesLiveSession =
  withReservedUnavailableOracleContacts caseWithUnavailableOracleContacts

caseWithUnavailableOracleContacts :: OracleContactSet -> IO () -> Assertion
caseWithUnavailableOracleContacts oracleContacts _releaseFirstContact = do
  initialAttempt <- newEmptyMVar
  releaseInitialDispatch <- newEmptyMVar
  boundOracle <- newEmptyMVar
  connectAtProbe <- newEmptyMVar
  releasePhysicalConnect <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookBeforeDispatchBatch =
              captureOracleActions
                initialAttempt
                releaseInitialDispatch
                boundOracle
          }
      listener = checked "terminal listener" (tcpListenEndpoint "127.0.0.1" 0)
      runtimeConfiguration =
        heraldRuntimeConfiguration
          step12H1Genesis
          step12H1Bootstraps
          oracleContacts
          step12H1GeneratorSeedSource
          systemRuntimeMonotonicClock
          (runtimePeerWorkDelayMicroseconds 0)
          (heraldRuntimeHandlers (const (pure ())))
          (checked "application recovery" (checkApplicationRecoveryConfiguration 30_000_000))
          (checked "peer recovery" (checkPeerRecoveryConfiguration 30_000_000))
      tcpConfiguration =
        heraldTcpConfiguration
          runtimeConfiguration
          listener
          listener
          listener
          []
          (checked "peer retry" (peerDialRetryDelayMicroseconds 1_000))
          (checked "heartbeat" (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))
      scope = HeraldRuntimeScope (withHeraldRuntimeWithHooks hooks)
      scenario tcp@(HeraldTcp runtime _ _ _) =
        ( do
            setHeraldTcpOracleActionProbe tcp
              $ Just
              $ holdFirstConnect connectAtProbe releasePhysicalConnect
            putMVar releaseInitialDispatch ()
            ownerAttempt <- readMVar initialAttempt
            probedAttempt <- readMVar connectAtProbe
            assertEqual "the held physical action is the owner-minted attempt" ownerAttempt probedAttempt

            socketHandle <-
              connectTcpEndpoint
                (heraldTcpApplicationEndpoint (heraldTcpEndpoints tcp))
            let open =
                  OpenSession
                    ( checked
                        "terminal application attachment"
                        (applicationAttachmentClaim (bootstrapManifestIdBytes step12H1ProcessBootstrapId))
                    )
                    (applicationClientNonce 15_301)
            SocketBytes.sendAll
              socketHandle
              (encodeApplicationFrame (ApplicationClientEnvelope open))
            session <- receiveOpenedSession socketHandle

            let node = oracleContactNode (oracleConnectAttemptContact ownerAttempt)
                acceptance =
                  oracleHelloAcceptance
                    node
                    (oracleObservedTerm 1)
                    (controlIndex 0)
                    (Just node)
                    True
            assertEqual "the checked Oracle Hello is queued" Queued
              =<< submitOracleClientIngress runtime (OracleHelloReceived ownerAttempt acceptance)
            binding <- readMVar boundOracle
            assertEqual "the canonical process End is queued" Queued
              =<< submitOracleClientIngress
                runtime
                (OracleEntriesReceived binding (canonicalEndEntry :| []))

            terminal <- receiveApplicationServerEnvelope socketHandle
            assertEqual
              "the live session receives its exact terminal disposition"
              ( ApplicationServerEnvelope
                  ( HeraldPermanentlyUnavailable
                      session
                      ApplicationSessionNoLongerLiveDto
                  )
              )
              terminal
            closed <- timeout 2_000_000 (SocketBytes.recv socketHandle 1)
            assertEqual "the terminal frame is followed by EOF" (Just ByteString.empty) closed
            close socketHandle

            releaseOracleProbe tcp releasePhysicalConnect
            assertEqual "terminal acceptance leaves semantic drain available" Queued
              =<< requestHeraldTcpDrain tcp (adminCorrelationId 15_301)
            assertEqual "the terminal TCP scope drains" (Right HeraldRuntimeDrained)
              =<< awaitHeraldTcpExit tcp
        )
          `finally` releaseOracleProbe tcp releasePhysicalConnect
  observed <-
    timeout
      10_000_000
      (withHeraldTcpRuntimeInScopeInternal scope tcpConfiguration scenario)
  case observed of
    Nothing -> assertFailure "canonical-End terminal TCP schedule timed out"
    Just (Left failure) -> assertFailure (show (failure :: HeraldTcpFailure))
    Just (Right ((), HeraldRuntimeDrained)) -> pure ()
    Just (Right ((), exit)) ->
      assertFailure ("unexpected terminal TCP runtime exit: " <> show exit)

captureOracleActions ::
  MVar OracleConnectAttempt ->
  MVar () ->
  MVar OracleBinding ->
  BatchEnvelope ->
  IO ()
captureOracleActions initialAttempt releaseInitialDispatch boundOracle batch =
  forM_ (effectBatchMembers (batchEnvelopeEffects batch)) $ \case
    RunOracleClientAction (ConnectAndHelloOracle attempt _) -> do
      first <- tryPutMVar initialAttempt attempt
      when first (readMVar releaseInitialDispatch)
    RunOracleClientAction (BindOracleConnection _ binding) ->
      void (tryPutMVar boundOracle binding)
    _ -> pure ()

holdFirstConnect ::
  MVar OracleConnectAttempt ->
  MVar () ->
  OracleClientAction ->
  IO ()
holdFirstConnect observed release = \case
  ConnectAndHelloOracle attempt _ -> do
    first <- tryPutMVar observed attempt
    when first (readMVar release)
  _ -> pure ()

releaseOracleProbe ::
  HeraldTcp ->
  MVar () ->
  IO ()
releaseOracleProbe tcp release = do
  setHeraldTcpOracleActionProbe tcp Nothing
  void (tryPutMVar release ())

receiveOpenedSession :: Socket -> IO ApplicationSessionClaim
receiveOpenedSession socketHandle =
  receiveApplicationServerEnvelope socketHandle >>= \case
    ApplicationServerEnvelope (SessionOpened _ session _ _) -> pure session
    other -> assertFailure ("unexpected application Open reply: " <> show other)

receiveApplicationServerEnvelope :: Socket -> IO ApplicationEnvelope
receiveApplicationServerEnvelope socketHandle =
  loop (initialApplicationFrameDecoder ApplicationServerFrames)
  where
    loop decoder = do
      chunk <- SocketBytes.recv socketHandle 32768
      if ByteString.null chunk
        then assertFailure "application socket closed before one complete server frame"
        else case feedApplicationFrame decoder chunk of
          ApplicationFrameFeedResult [envelope] (NeedApplicationFrameBytes _) -> pure envelope
          ApplicationFrameFeedResult [] (NeedApplicationFrameBytes successor) -> loop successor
          ApplicationFrameFeedResult envelopes continuation ->
            assertFailure
              ( "unexpected application server frame batch: "
                  <> show envelopes
                  <> ", continuation: "
                  <> show continuation
              )

canonicalEndEntry :: CanonicalAppliedOracleEntry
canonicalEndEntry =
  case checked "initialize matching Oracle" (initialOracle step12CheckedOracleGenesis) of
    predecessor ->
      case checked "commit application-loss End" (stepOracle envelope predecessor) of
        (_, OracleCommitted _, effects) -> case oracleEffects effects of
          [EmitAppliedOracleEntry entry] -> canonicalizeAppliedOracleEntry entry
          other -> error ("canonical End emitted unexpected effects: " <> show other)
        (_, outcome, _) -> error ("canonical End was not committed: " <> show outcome)
  where
    envelope =
      oracleEnvelope
        (oracleClientRequestId step12H1HeraldEpoch 15_301)
        Nothing
        step12H1HeraldEpoch
        ( checked
            "live End command"
            (endProcessEpochCommand step12H1ProcessEpochId ApplicationPermanentlyLost)
        )

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
