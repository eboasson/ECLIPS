{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module VerificationClosureProperties
  ( tests,
  )
where

import Control.Concurrent.Chan
  ( Chan,
    newChan,
    readChan,
    writeChan,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( atomically,
    check,
  )
import Control.Exception (finally)
import Control.Monad (void, when)
import Data.List (find)
import Eclips.Herald.Discovery
  ( PeerCandidate,
    PeerHelloDisposition (..),
    peerCandidateConnectionNonce,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (..),
    PeerIngress (..),
    inputBody,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome (PeerDispatchWritten),
  )
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    heraldRuntimeConfiguration,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
  )
import Eclips.Herald.Runtime.Connection
  ( ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (..),
    RuntimePeerConnectionHandlers,
    heraldRuntimeHandlers,
    runtimePeerConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeRegistration (..),
    RuntimeSubmission (..),
    openPeerCandidate,
    registerPeerConnection,
    submitPeerHello,
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (..),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    startHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (..),
    RuntimeTraceEvent (..),
    ShellTraceEvent (..),
    TraceLedger,
    snapshotTraceLedger,
  )
import Eclips.Herald.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Herald.Time (monotonicInstant)
import PairRuntimeFixtures
  ( pairLocalBootstraps,
    pairLocalGeneratorSeedSource,
    pairLocalGenesis,
    pairRemoteHello,
  )
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureMemberSetDigest,
    fixtureMembershipGenerationId,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "step-9 verification closure"
    [testCase "an early matching peer Hello waits for candidate promotion" caseEarlyMatchingHello]

caseEarlyMatchingHello :: Assertion
caseEarlyMatchingHello = do
  trace <- newTraceProbe
  candidateHanded <- newEmptyMVar
  releaseCandidate <- newEmptyMVar
  dispositions <- newChan
  closed <- newEmptyMVar
  let hooks =
        (tracedHooks trace defaultRuntimeHooks)
          { hookBeforeDispatchBatch = gateFirstMatchingBatch hasPeerCandidate candidateHanded releaseCandidate
          }
  withRuntime hooks fixtureConfiguration $ \runtime -> do
    peer <- registerPeer runtime (peerHandlers dispositions closed)
    assertEqual "the local candidate open is queued" Queued
      =<< openPeerCandidate runtime peer mempty Nothing
    candidateBatch <- awaitMVar "local candidate batch handoff" candidateHanded
    candidate <- exactlyOneCandidate candidateBatch
    let reciprocal = pairRemoteHello (peerCandidateConnectionNonce candidate)
    assertEqual
      "the reciprocal Hello is retained in the paused source FIFO"
      Queued
      =<< submitPeerHello
        runtime
        peer
        candidate
        mempty
        reciprocal
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    putMVar releaseCandidate ()
    disposition <- awaitChan "early reciprocal Hello disposition" dispositions
    case disposition of
      PeerHelloAccepted {} -> pure ()
      PeerHelloRejected problem ->
        assertFailure ("the matching early Hello was rejected: " <> show problem)
    events <- awaitTrace "early Hello kernel admission" trace (any isPeerHelloStep)
    assertBool
      "candidate routing precedes the queued reciprocal Hello step"
      (eventIndex isCandidateRoute events < eventIndex isPeerHelloStep events)
  _ <- awaitMVar "early-Hello peer close" closed
  pure ()

data TraceProbe = TraceProbe (MVar TraceLedger)

newTraceProbe :: IO TraceProbe
newTraceProbe = TraceProbe <$> newEmptyMVar

tracedHooks :: TraceProbe -> RuntimeHooks -> RuntimeHooks
tracedHooks (TraceProbe retained) hooks =
  hooks
    { hookRuntimeInitialized = \_ _ _ _ _ _ ledger -> putOnce retained ledger
    }

awaitTrace ::
  String ->
  TraceProbe ->
  ([RuntimeTraceEvent] -> Bool) ->
  IO [RuntimeTraceEvent]
awaitTrace context (TraceProbe retained) ready = do
  ledger <- readMVar retained
  observed <-
    timeout 3_000_000 . atomically $ do
      events <- snapshotTraceLedger ledger
      check (ready events)
      pure events
  case observed of
    Just events -> pure events
    Nothing -> assertFailure (context <> " timed out")

gateFirstMatchingBatch ::
  (BatchEnvelope -> Bool) ->
  MVar BatchEnvelope ->
  MVar () ->
  BatchEnvelope ->
  IO ()
gateFirstMatchingBatch matches entered release batch =
  when (matches batch) $ do
    first <- tryPutMVar entered batch
    when first (readMVar release)

hasPeerCandidate :: BatchEnvelope -> Bool
hasPeerCandidate = any isCandidate . effectBatchMembers . batchEnvelopeEffects
  where
    isCandidate SendPeerCandidate {} = True
    isCandidate _ = False

exactlyOneCandidate :: BatchEnvelope -> IO PeerCandidate
exactlyOneCandidate envelope =
  case [ candidate
       | SendPeerCandidate candidate _ _ _ <- effectBatchMembers (batchEnvelopeEffects envelope)
       ] of
    [candidate] -> pure candidate
    candidates -> assertFailure ("expected one candidate effect, got " <> show candidates)

isCandidateRoute :: RuntimeTraceEvent -> Bool
isCandidateRoute = \case
  ShellEvent _ (ShellEffectRouted _ _ (SendPeerCandidate {})) -> True
  _ -> False

isPeerHelloStep :: RuntimeTraceEvent -> Bool
isPeerHelloStep = \case
  KernelEvent _ (KernelTraceStepped _ _ input _) -> case inputBody input of
    PeerInput (PeerHelloReceived {}) -> True
    _ -> False
  _ -> False

eventIndex :: (RuntimeTraceEvent -> Bool) -> [RuntimeTraceEvent] -> Int
eventIndex matches events =
  case find (matches . snd) (zip [0 ..] events) of
    Just (index, _) -> index
    Nothing -> length events

peerHandlers :: Chan PeerHelloDisposition -> MVar () -> RuntimePeerConnectionHandlers
peerHandlers dispositions closed =
  runtimePeerConnectionHandlers
    (\_ _ -> pure LaneOffered)
    (\_ disposition -> writeChan dispositions disposition >> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ _ -> pure PeerDispatchWritten)
    (putOnce closed ())

registerPeer ::
  HeraldRuntime ->
  RuntimePeerConnectionHandlers ->
  IO (ConnectionRef PeerPlane)
registerPeer runtime handlers = do
  registration <- registerPeerConnection runtime handlers
  case registration of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "running runtime rejected peer registration"

withRuntime ::
  RuntimeHooks ->
  HeraldRuntimeConfiguration ->
  (HeraldRuntime -> IO ()) ->
  Assertion
withRuntime hooks configuration use = do
  started <- startHeraldRuntimeWithHooks hooks configuration
  runtime <- requireRuntime started
  use runtime `finally` RuntimeInternal.runtimeCloseScope runtime

requireRuntime ::
  Either RuntimeInternal.HeraldRuntimeFailure HeraldRuntime ->
  IO HeraldRuntime
requireRuntime = \case
  Left failure -> assertFailure ("runtime initialization failed: " <> show failure)
  Right runtime -> pure runtime

fixtureConfiguration :: HeraldRuntimeConfiguration
fixtureConfiguration =
  heraldRuntimeConfiguration
    pairLocalGenesis
    pairLocalBootstraps
    fixtureOracleContacts
    pairLocalGeneratorSeedSource
    (runtimeMonotonicClock (pure (monotonicInstant 100)))
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration

putOnce :: MVar value -> value -> IO ()
putOnce cell value = void (tryPutMVar cell value)

awaitMVar :: String -> MVar value -> IO value
awaitMVar context cell = do
  result <- timeout 3_000_000 (takeMVar cell)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitChan :: String -> Chan value -> IO value
awaitChan context channel = do
  result <- timeout 3_000_000 (readChan channel)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")
