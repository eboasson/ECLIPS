{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module RuntimeClosureProperties
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
import Control.Monad (forM_, void, when)
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Operation
  ( ApplicationOperation (WriteApplication),
  )
import Eclips.Application.Types.Result (RegularCallResult (WriteCompleted))
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value (ApplicationValue (SortDefinitionValue))
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten),
  )
import Eclips.Domain.Disappearance (deriveDisappearanceProbeId)
import Eclips.Domain.Identity (controlIndex)
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Application.RPC (ApplicationOutbound (..))
import Eclips.Herald.Application.Session (ApplicationSessionBinding)
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerHelloDisposition (..),
    connectionNonce,
    peerCandidateConnectionNonce,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Input
  ( DisappearanceIngress (AbortDisappearanceProbe),
    HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (PeerDispatchSelected),
    RuntimeObservation (..),
    inputBody,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (SubmitOracleRequest),
  )
import Eclips.Herald.Peer.RPC
  ( CandidatePeerContext (..),
    CandidatePeerInbound (CandidatePeerHello),
    EstablishedPeerInbound (EstablishedPeerControl),
    admitCandidatePeerEnvelope,
    admitEstablishedPeerEnvelope,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome (PeerDispatchDeferred, PeerDispatchWritten),
  )
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    awaitHeraldRuntimeExit,
    heraldRuntimeConfiguration,
    requestHeraldRuntimeDrain,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
  )
import Eclips.Herald.Runtime.Connection
  ( AdministrationPlane,
    ApplicationPlane,
    ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeApplicationConnectionHandlers,
    RuntimeLaneOffer (..),
    RuntimePeerConnectionHandlers,
    heraldRuntimeHandlers,
    runtimeAdministrationConnectionHandlers,
    runtimeApplicationConnectionHandlers,
    runtimePeerConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeAdministrationRegistration (..),
    RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    openPeerCandidate,
    registerAdministrationConnection,
    registerApplicationConnection,
    registerPeerConnection,
    submitApplicationDto,
    submitDisappearanceProbeAbort,
    submitPeerControlWithProgress,
    submitPeerHello,
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (..),
    BatchOrdinal (..),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    RuntimeRetentionCounts (..),
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
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (..))
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Protocol.Application.Types qualified as Protocol
import PairRuntimeFixtures
  ( pairLocalBootstraps,
    pairLocalGeneratorSeedSource,
    pairLocalGenesis,
    pairRemoteBootstraps,
    pairRemoteCandidate,
    pairRemoteGeneratorSeedSource,
    pairRemoteGenesis,
    pairRemoteHello,
  )
import PeerEvidence (data SemanticPeerControlReceived, data SemanticSendPeerItem)
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureMemberSetDigest,
    fixtureMembershipGenerationId,
    fixtureOpenDto,
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
    "live runtime closure matrices"
    [ testCase "live handed BatchOrdinal values are contiguous" caseLiveBatchOrdinals,
      testCase "the authorized disappearance handle queues exactly one input" caseExplicitDisappearanceAbortIngress,
      testCase "a pre-cut missing application lane settles before the barrier and post-cut ingress stays out" caseDrainMissingApplicationLane,
      testCase "a pre-cut missing peer target settles exactly one loss before the barrier" caseDrainMissingPeerTarget,
      testCase "a routed peer item without its physical route settles through binding loss" caseMissingPeerItemRoute,
      testCase "written peer items release their physical outcome cells before later socket loss" casePeerOutcomeRetention
    ]

caseLiveBatchOrdinals :: Assertion
caseLiveBatchOrdinals = do
  trace <- newTraceProbe
  let hooks = tracedHooks trace defaultRuntimeHooks
  withClosureRuntime hooks 0 $ \runtime -> do
    output <- newChan
    closed <- newEmptyMVar
    reference <- registerApplication runtime (applicationHandlers output closed)
    assertEqual "the Open is queued" Queued
      =<< submitApplicationDto runtime reference fixtureOpenDto
    _ <- awaitOpened "live ordinal Open" output
    RuntimeInternal.runtimeCloseScope runtime
    events <- snapshotTrace trace
    let handed = [ordinal | ShellEvent _ (ShellBatchHanded ordinal) <- events]
        expected = fmap (BatchOrdinal . fromIntegral) [0 .. length handed - 1]
    assertBool "the live run hands more than only initialization" (length handed > 1)
    assertEqual "every handed batch ordinal is contiguous from zero" expected handed
    _ <- awaitMVar "application close after scope" closed
    pure ()

caseExplicitDisappearanceAbortIngress :: Assertion
caseExplicitDisappearanceAbortIngress = do
  trace <- newTraceProbe
  let hooks = tracedHooks trace defaultRuntimeHooks
      probe = either (error . show) id (deriveDisappearanceProbeId (controlIndex 1))
      isInput event = case event of
        KernelEvent _ (KernelTraceStepped _ _ input _) -> case inputBody input of
          DisappearanceInput (AbortDisappearanceProbe actual) -> actual == probe
          _ -> False
        _ -> False
  withClosureRuntime hooks 0 $ \runtime -> do
    assertEqual "authorized probe queued" Queued =<< submitDisappearanceProbeAbort runtime probe
    events <- awaitTrace "explicit disappearance Abort" trace (any isInput)
    let abortSteps = [effects | KernelEvent _ (KernelTraceStepped _ _ input (Right effects)) <- events, DisappearanceInput (AbortDisappearanceProbe actual) <- [inputBody input], actual == probe]
    assertEqual "one exact authorized input" 1 (length abortSteps)
    assertBool "unknown probe creates no Oracle command" (all (all (not . isOracleSubmission) . effectBatchMembers) abortSteps)

isOracleSubmission :: HeraldEffect -> Bool
isOracleSubmission (RunOracleClientAction (SubmitOracleRequest _ _)) = True
isOracleSubmission _ = False

caseDrainMissingApplicationLane :: Assertion
caseDrainMissingApplicationLane = do
  trace <- newTraceProbe
  reply <- newBatchGate
  finish <- newBatchGate
  let beforeBatch batch
        | hasApplicationReply batch = gateBatch reply batch
        | hasFinishDrain batch = gateBatch finish batch
        | otherwise = pure ()
      hooks = tracedHooks trace defaultRuntimeHooks {hookBeforeDispatchBatch = beforeBatch}
  withClosureRuntime hooks 0 $ \runtime -> do
    administration <- registerAdministration runtime
    output <- newChan
    closed <- newEmptyMVar
    primary <- registerApplication runtime (applicationHandlers output closed)
    assertEqual "the primary Open is queued" Queued
      =<< submitApplicationDto runtime primary fixtureOpenDto
    opened <- awaitOpened "primary Open" output
    postCutProbe <- registerApplication runtime quietApplicationHandlers

    let request = Protocol.applicationRequestIdClaim 804
        query = Protocol.GetRequestResult (openedSession opened) request
    assertEqual "the pre-cut query is queued" Queued
      =<< submitApplicationDto runtime primary query
    _ <- awaitMVar "pre-cut application reply batch" (batchGateEntered reply)
    assertEqual "the drain request is queued behind the handed reply" Queued
      =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 804)
    cutEvents <- awaitTrace "BeginDrain cut" trace hasDrainCut
    assertEqual
      "a post-cut external application call is rejected before the owner"
      IngressClosedForDrain
      =<< submitApplicationDto runtime postCutProbe fixtureOpenDto
    assertEqual "the reply target closes after the cut but before routing" ConnectionClosed
      =<< closeRuntimeConnection runtime primary
    _ <- awaitMVar "closed missing application route" closed
    putMVar (batchGateRelease reply) ()

    _ <- awaitMVar "FinishDrain after missing application loss" (batchGateEntered finish)
    events <- snapshotTrace trace
    assertEqual
      "the missing pre-cut application route creates exactly one loss step"
      1
      (applicationLossCount (openedBinding opened) events)
    assertCutLossBarrierOrder
      "application"
      (isApplicationLoss (openedBinding opened))
      events
    assertEqual
      "the rejected post-cut Open never appears in the kernel trace"
      0
      (postCutApplicationInputs cutEvents events)
    putMVar (batchGateRelease finish) ()
    assertEqual "the missing-lane obligation settles before drain" (Right HeraldRuntimeDrained)
      =<< awaitRuntimeExit "missing application lane drain" runtime

caseDrainMissingPeerTarget :: Assertion
caseDrainMissingPeerTarget = do
  trace <- newTraceProbe
  disposition <- newBatchGate
  finish <- newBatchGate
  let beforeBatch batch
        | hasPeerAcceptance batch = gateBatch disposition batch
        | hasFinishDrain batch = gateBatch finish batch
        | otherwise = pure ()
      hooks = tracedHooks trace defaultRuntimeHooks {hookBeforeDispatchBatch = beforeBatch}
  withClosureRuntime hooks 0 $ \runtime -> do
    administration <- registerAdministration runtime
    closed <- newEmptyMVar
    peer <- registerPeer runtime (quietPeerHandlers closed)
    let candidate = pairRemoteCandidate (connectionNonce 77)
        hello = pairRemoteHello (peerCandidateConnectionNonce candidate)
    assertEqual "the peer Hello is queued" Queued
      =<< submitPeerHello
        runtime
        peer
        candidate
        mempty
        hello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    handed <- awaitMVar "pre-cut peer acceptance batch" (batchGateEntered disposition)
    binding <- acceptedPeerBinding handed
    assertEqual "the drain request is queued behind the handed peer disposition" Queued
      =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 805)
    _ <- awaitTrace "peer-target BeginDrain cut" trace hasDrainCut
    assertEqual "the exact peer promotion target closes after the cut" ConnectionClosed
      =<< closeRuntimeConnection runtime peer
    _ <- awaitMVar "closed missing peer target" closed
    putMVar (batchGateRelease disposition) ()

    _ <- awaitMVar "FinishDrain after missing peer loss" (batchGateEntered finish)
    events <- snapshotTrace trace
    assertEqual
      "the missing pre-cut peer target creates exactly one loss step"
      1
      (peerLossCount binding events)
    assertCutLossBarrierOrder "peer" (isPeerLoss binding) events
    putMVar (batchGateRelease finish) ()
    assertEqual "the missing peer obligation settles before drain" (Right HeraldRuntimeDrained)
      =<< awaitRuntimeExit "missing peer target drain" runtime

caseMissingPeerItemRoute :: Assertion
caseMissingPeerItemRoute = do
  trace <- newTraceProbe
  peerItem <- newBatchGate
  localReference <- newEmptyMVar
  remoteReference <- newEmptyMVar
  localBinding <- newEmptyMVar
  remoteBinding <- newEmptyMVar
  remoteResumeAccepted <- newEmptyMVar
  localPeerClosed <- newEmptyMVar
  remotePeerClosed <- newEmptyMVar
  let beforeBatch batch
        | hasPeerItem batch = gateBatch peerItem batch
        | otherwise = pure ()
      hooks = tracedHooks trace defaultRuntimeHooks {hookBeforeDispatchBatch = beforeBatch}
  withClosureRuntime hooks 0 $ \runtime -> do
    withConfiguredClosureRuntime defaultRuntimeHooks remoteFixtureConfiguration $ \remoteRuntime -> do
      administration <- registerAdministration runtime
      localPeer <-
        registerPeer
          runtime
          ( relayPeerHandlers
              remoteRuntime
              remoteReference
              remoteBinding
              localBinding
              Nothing
              localPeerClosed
          )
      remotePeer <-
        registerPeer
          remoteRuntime
          ( relayPeerHandlers
              runtime
              localReference
              localBinding
              remoteBinding
              (Just remoteResumeAccepted)
              remotePeerClosed
          )
      putMVar localReference localPeer
      putMVar remoteReference remotePeer
      assertEqual "the no-route peer candidate is queued" Queued
        =<< openPeerCandidate runtime localPeer mempty Nothing
      binding <- awaitReadMVar "the no-route local peer acceptance" localBinding
      _ <- awaitReadMVar "the no-route remote peer acceptance" remoteBinding
      (terminal, terminalSubmission) <-
        awaitMVar "the remote terminal resume control" remoteResumeAccepted
      assertEqual "the remote terminal resume control is queued locally" Queued terminalSubmission
      _ <-
        awaitTrace
          "the local terminal resume fence"
          trace
          (any (isPeerControlStep binding terminal))

      output <- newChan
      applicationClosed <- newEmptyMVar
      application <- registerApplication runtime (applicationHandlers output applicationClosed)
      assertEqual "the no-route application Open is queued" Queued
        =<< submitApplicationDto runtime application fixtureOpenDto
      opened <- awaitOpened "no-route application Open" output
      sortAccess <- sortDefinitionAccess opened
      let request = Protocol.applicationRequestIdClaim 806
          operation =
            WriteApplication
              (Access.predefinedWriter sortAccess)
              ( PublishValue
                  ( SortDefinitionValue
                      (DeclaredSortDefinition noRouteSortDescriptor Nothing)
                  )
              )
      assertEqual "the no-route publication is queued" Queued
        =<< submitApplicationDto
          runtime
          application
          (Protocol.Call (openedSession opened) request operation)
      publicationReply <- awaitChan "the no-route publication reply" output
      assertSuccessfulSortPublication request publicationReply
      handed <- awaitMVar "the no-route SendPeerItem batch" (batchGateEntered peerItem)
      assertEqual
        "the held batch contains one item for the accepted logical binding"
        [binding]
        [ itemBinding
        | SemanticSendPeerItem itemBinding _ <- effectBatchMembers (batchEnvelopeEffects handed)
        ]

      assertEqual "the no-route drain request is queued" Queued
        =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 806)
      _ <- awaitTrace "the no-route drain cut" trace hasDrainCut
      assertEqual "the physical peer route closes at the held item" ConnectionClosed
        =<< closeRuntimeConnection runtime localPeer
      _ <- awaitMVar "the no-route peer writer closes" localPeerClosed
      putMVar (batchGateRelease peerItem) ()

      assertEqual "the missing item route settles without hanging drain" (Right HeraldRuntimeDrained)
        =<< awaitRuntimeExit "missing peer-item route drain" runtime
      events <- snapshotTrace trace
      assertEqual
        "the missing item route coalesces to exactly one binding loss"
        [binding]
        (peerLosses events)
      assertEqual
        "the missing item route does not manufacture a Deferred outcome"
        0
        (peerDispatchDeferralCount events)
      assertEqual
        "the retained item is selected exactly once"
        1
        (peerDispatchSelectionCount events)
      assertMissingItemOriginSettled events

casePeerOutcomeRetention :: Assertion
casePeerOutcomeRetention = do
  trace <- newTraceProbe
  readerCell <- newEmptyMVar
  localReference <- newEmptyMVar
  remoteReference <- newEmptyMVar
  localBinding <- newEmptyMVar
  remoteBinding <- newEmptyMVar
  remoteResumeAccepted <- newEmptyMVar
  localClosed <- newEmptyMVar
  remoteClosed <- newEmptyMVar
  let hooks = tracedHooks trace defaultRuntimeHooks {hookRuntimeRetentionReader = putOnce readerCell}
  withClosureRuntime hooks 0 $ \runtime ->
    withConfiguredClosureRuntime defaultRuntimeHooks remoteFixtureConfiguration $ \remoteRuntime -> do
      localPeer <- registerPeer runtime (relayPeerHandlers remoteRuntime remoteReference remoteBinding localBinding Nothing localClosed)
      remotePeer <- registerPeer remoteRuntime (relayPeerHandlers runtime localReference localBinding remoteBinding (Just remoteResumeAccepted) remoteClosed)
      putMVar localReference localPeer
      putMVar remoteReference remotePeer
      assertEqual "the outcome-retention peer candidate is queued" Queued =<< openPeerCandidate runtime localPeer mempty Nothing
      binding <- awaitReadMVar "outcome-retention local binding" localBinding
      _ <- awaitReadMVar "outcome-retention remote binding" remoteBinding
      (terminal, _) <- awaitMVar "outcome-retention resume" remoteResumeAccepted
      _ <- awaitTrace "outcome-retention resume applied" trace (any (isPeerControlStep binding terminal))
      output <- newChan
      applicationClosed <- newEmptyMVar
      application <- registerApplication runtime (applicationHandlers output applicationClosed)
      assertEqual "the outcome-retention application Open is queued" Queued =<< submitApplicationDto runtime application fixtureOpenDto
      opened <- awaitOpened "outcome-retention application" output
      access <- sortDefinitionAccess opened
      readCounts <- awaitReadMVar "outcome-retention reader" readerCell
      initialWritten <- writtenCount <$> snapshotTrace trace
      forM_ [1 .. 40 :: Word64] $ \iteration -> do
        let request = Protocol.applicationRequestIdClaim (900 + iteration)
            operation = WriteApplication (Access.predefinedWriter access) (PublishValue (SortDefinitionValue (DeclaredSortDefinition noRouteSortDescriptor Nothing)))
        assertEqual "publication is queued" Queued =<< submitApplicationDto runtime application (Protocol.Call (openedSession opened) request operation)
        assertSuccessfulSortPublication request =<< awaitChan "publication reply" output
        _ <- awaitTrace "written publication outcome reached its owner" trace ((>= initialWritten + fromIntegral iteration) . writtenCount)
        counts <- readCounts
        assertEqual "completed sends do not remain in the physical outcome registry" 0 (retainedOutcomeCells counts)
      assertEqual "the later socket closes once" ConnectionClosed =<< closeRuntimeConnection runtime localPeer
      _ <- awaitMVar "outcome-retention socket close" localClosed
      _ <- awaitTrace "later loss is applied once" trace ((== 1) . peerLossCount binding)
      assertEqual "the old socket callback stays stale" StalePhysicalGeneration =<< closeRuntimeConnection runtime localPeer
      events <- snapshotTrace trace
      assertEqual "loss does not replay completed sends as Deferred" 0 (peerDispatchDeferralCount events)
      assertEqual "loss does not recreate completed outcome cells" 0 . retainedOutcomeCells =<< readCounts
  where
    writtenCount events =
      length
        [ ()
        | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
          RuntimeObserved (PeerDispatchObserved _ PeerDispatchWritten) <- [inputBody input]
        ]

data TraceProbe = TraceProbe (MVar TraceLedger)

newTraceProbe :: IO TraceProbe
newTraceProbe = TraceProbe <$> newEmptyMVar

tracedHooks :: TraceProbe -> RuntimeHooks -> RuntimeHooks
tracedHooks (TraceProbe retained) hooks =
  hooks
    { hookRuntimeInitialized = \_ _ _ _ _ _ ledger -> putOnce retained ledger
    }

snapshotTrace :: TraceProbe -> IO [RuntimeTraceEvent]
snapshotTrace (TraceProbe retained) = do
  ledger <- readMVar retained
  atomically (snapshotTraceLedger ledger)

awaitTrace ::
  String ->
  TraceProbe ->
  ([RuntimeTraceEvent] -> Bool) ->
  IO [RuntimeTraceEvent]
awaitTrace context (TraceProbe retained) ready = do
  ledger <- readMVar retained
  result <-
    timeout 3000000 . atomically $ do
      events <- snapshotTraceLedger ledger
      check (ready events)
      pure events
  case result of
    Just events -> pure events
    Nothing -> assertFailure (context <> " timed out")

data BatchGate = BatchGate
  { batchGateEntered :: MVar BatchEnvelope,
    batchGateRelease :: MVar ()
  }

newBatchGate :: IO BatchGate
newBatchGate = BatchGate <$> newEmptyMVar <*> newEmptyMVar

gateBatch :: BatchGate -> BatchEnvelope -> IO ()
gateBatch gate batch = do
  first <- tryPutMVar (batchGateEntered gate) batch
  when first (readMVar (batchGateRelease gate))

hasFinishDrain :: BatchEnvelope -> Bool
hasFinishDrain = any isFinish . effectBatchMembers . batchEnvelopeEffects
  where
    isFinish FinishDrain {} = True
    isFinish _ = False

hasApplicationReply :: BatchEnvelope -> Bool
hasApplicationReply = any isReply . effectBatchMembers . batchEnvelopeEffects
  where
    isReply SendApplicationReply {} = True
    isReply _ = False

hasPeerAcceptance :: BatchEnvelope -> Bool
hasPeerAcceptance = any isAcceptance . effectBatchMembers . batchEnvelopeEffects
  where
    isAcceptance (SetPeerCandidateDisposition _ PeerHelloAccepted {}) = True
    isAcceptance _ = False

hasPeerItem :: BatchEnvelope -> Bool
hasPeerItem = any isPeerItem . effectBatchMembers . batchEnvelopeEffects
  where
    isPeerItem SemanticSendPeerItem {} = True
    isPeerItem _ = False

hasDrainCut :: [RuntimeTraceEvent] -> Bool
hasDrainCut = any $ \case
  ShellEvent _ (ShellDrainCut {}) -> True
  _ -> False

data OpenedApplication = OpenedApplication
  { openedBinding :: ApplicationSessionBinding,
    openedSession :: Protocol.ApplicationSessionClaim,
    openedStartup :: Access.ApplicationStartupAccess
  }

awaitOpened :: String -> Chan ApplicationOutbound -> IO OpenedApplication
awaitOpened context output = do
  outbound <- awaitChan context output
  case outbound of
    AcceptApplicationCandidate _ binding (Protocol.SessionOpened _ session _ startup) ->
      pure
        OpenedApplication
          { openedBinding = binding,
            openedSession = session,
            openedStartup = startup
          }
    other -> assertFailure (context <> ": expected SessionOpened acceptance, got " <> show other)

sortDefinitionAccess :: OpenedApplication -> IO Access.PredefinedAccess
sortDefinitionAccess opened =
  case (Map.lookup (Access.environmentWriterKey Access.SortDefinitionRole) (Access.accessEntries (Access.startupAccessPrimordial (openedStartup opened))), Map.lookup (Access.environmentReaderKey Access.SortDefinitionRole) (Access.accessEntries (Access.startupAccessPrimordial (openedStartup opened)))) of
    (Just (Access.Writer writer), Just (Access.Reader reader)) -> pure (Access.predefinedAccess Access.SortDefinitionRole writer reader)
    _ -> assertFailure "SessionOpened omitted SortDefinition startup access"

assertSuccessfulSortPublication ::
  Protocol.ApplicationRequestIdClaim ->
  ApplicationOutbound ->
  Assertion
assertSuccessfulSortPublication expected = \case
  SendEstablishedApplication
    _
    ( Protocol.RequestRetained
        _
        _
        actual
        (Protocol.Completed (WriteCompleted (SortDefinitionWritten _)))
      ) ->
      assertEqual "the publication reply names the submitted request" expected actual
  other ->
    assertFailure
      ( "expected a retained successful sort-definition publication, got "
          <> show other
      )

applicationHandlers :: Chan ApplicationOutbound -> MVar () -> RuntimeApplicationConnectionHandlers
applicationHandlers output closed =
  runtimeApplicationConnectionHandlers
    (\outbound -> writeChan output outbound >> pure LaneOffered)
    (putOnce closed ())

quietApplicationHandlers :: RuntimeApplicationConnectionHandlers
quietApplicationHandlers = runtimeApplicationConnectionHandlers (const (pure LaneOffered)) (pure ())

quietPeerHandlers :: MVar () -> RuntimePeerConnectionHandlers
quietPeerHandlers closed =
  runtimePeerConnectionHandlers
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ _ -> pure PeerDispatchWritten)
    (putOnce closed ())

relayPeerHandlers ::
  HeraldRuntime ->
  MVar (ConnectionRef PeerPlane) ->
  MVar PeerBinding ->
  MVar PeerBinding ->
  Maybe (MVar (PeerControl, RuntimeSubmission)) ->
  MVar () ->
  RuntimePeerConnectionHandlers
relayPeerHandlers targetRuntime targetReference targetBinding ownBinding terminalObserved closed =
  runtimePeerConnectionHandlers
    ( \candidate envelope ->
        let admitted =
              case admitCandidatePeerEnvelope RemoteInitiatedCandidate envelope of
                accepted@(Right (CandidatePeerHello derived _ _ _))
                  | derived == candidate -> accepted
                _ ->
                  admitCandidatePeerEnvelope
                    (LocallyInitiatedCandidate candidate)
                    envelope
         in case admitted of
              Left _ -> pure LaneLost
              Right (CandidatePeerHello admittedCandidate hello generation members) -> do
                reference <- readMVar targetReference
                submission <-
                  submitPeerHello
                    targetRuntime
                    reference
                    admittedCandidate
                    mempty
                    hello
                    generation
                    members
                    Nothing
                pure (submissionLaneOffer submission)
    )
    ( \_ disposition -> do
        case disposition of
          PeerHelloAccepted _ binding -> putOnce ownBinding binding
          PeerHelloRejected _ -> pure ()
        pure LaneOffered
    )
    ( \_ envelope -> case admitEstablishedPeerEnvelope envelope of
        Right (EstablishedPeerControl progress control) -> do
          reference <- readMVar targetReference
          binding <- readMVar targetBinding
          submission <- submitPeerControlWithProgress targetRuntime reference binding progress control
          when
            (isResumeAccepted control)
            ( forM_
                terminalObserved
                (\observed -> putOnce observed (control, submission))
            )
          pure (submissionLaneOffer submission)
        _ -> pure LaneLost
    )
    (\_ _ -> pure LaneOffered)
    (\_ _ _ -> pure PeerDispatchWritten)
    (putOnce closed ())

submissionLaneOffer :: RuntimeSubmission -> RuntimeLaneOffer
submissionLaneOffer Queued = LaneOffered
submissionLaneOffer _ = LaneLost

isResumeAccepted :: PeerControl -> Bool
isResumeAccepted PeerStreamResumeAccepted {} = True
isResumeAccepted _ = False

isPeerControlStep :: PeerBinding -> PeerControl -> RuntimeTraceEvent -> Bool
isPeerControlStep expectedBinding expectedControl = \case
  KernelEvent _ (KernelTraceStepped _ _ input _) ->
    case inputBody input of
      PeerInput (SemanticPeerControlReceived actualBinding actualControl) ->
        actualBinding == expectedBinding && actualControl == expectedControl
      _ -> False
  _ -> False

registerApplication ::
  HeraldRuntime ->
  RuntimeApplicationConnectionHandlers ->
  IO (ConnectionRef ApplicationPlane)
registerApplication runtime handlers = do
  registration <- registerApplicationConnection runtime handlers
  case registration of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "running runtime rejected application registration"

registerPeer ::
  HeraldRuntime ->
  RuntimePeerConnectionHandlers ->
  IO (ConnectionRef PeerPlane)
registerPeer runtime handlers = do
  registration <- registerPeerConnection runtime handlers
  case registration of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "running runtime rejected peer registration"

registerAdministration :: HeraldRuntime -> IO (ConnectionRef AdministrationPlane)
registerAdministration runtime = do
  registration <-
    registerAdministrationConnection
      runtime
      (runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
  case registration of
    AdministrationConnectionRegistered reference -> pure reference
    AdministrationRegistrationUnavailable -> assertFailure "first administration registration was unavailable"
    AdministrationRegistrationStopped -> assertFailure "running runtime rejected administration registration"

acceptedPeerBinding :: BatchEnvelope -> IO PeerBinding
acceptedPeerBinding batch =
  case [ binding
       | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- effectBatchMembers (batchEnvelopeEffects batch)
       ] of
    [binding] -> pure binding
    other -> assertFailure ("expected one accepted peer binding, got " <> show other)

applicationLossCount :: ApplicationSessionBinding -> [RuntimeTraceEvent] -> Int
applicationLossCount expected = length . filter (isApplicationLoss expected)

isApplicationLoss :: ApplicationSessionBinding -> RuntimeTraceEvent -> Bool
isApplicationLoss expected = \case
  KernelEvent _ (KernelTraceStepped _ _ input _) -> case inputBody input of
    RuntimeObserved (ApplicationBindingLost actual) -> actual == expected
    _ -> False
  _ -> False

peerLossCount :: PeerBinding -> [RuntimeTraceEvent] -> Int
peerLossCount expected = length . filter (isPeerLoss expected)

isPeerLoss :: PeerBinding -> RuntimeTraceEvent -> Bool
isPeerLoss expected = \case
  KernelEvent _ (KernelTraceStepped _ _ input _) -> case inputBody input of
    RuntimeObserved (PeerBindingLost actual) -> actual == expected
    _ -> False
  _ -> False

peerLosses :: [RuntimeTraceEvent] -> [PeerBinding]
peerLosses events =
  [ binding
  | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
    RuntimeObserved (PeerBindingLost binding) <- [inputBody input]
  ]

peerDispatchDeferralCount :: [RuntimeTraceEvent] -> Int
peerDispatchDeferralCount events =
  length
    [ ()
    | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
      RuntimeObserved (PeerDispatchObserved _ PeerDispatchDeferred) <- [inputBody input]
    ]

peerDispatchSelectionCount :: [RuntimeTraceEvent] -> Int
peerDispatchSelectionCount events =
  length
    [ ()
    | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
      PeerInput (PeerDispatchSelected _ _) <- [inputBody input]
    ]

assertMissingItemOriginSettled :: [RuntimeTraceEvent] -> Assertion
assertMissingItemOriginSettled events = do
  routed <- eventPosition "missing peer-item effect route" isItemRoute events
  loss <- eventPosition "missing peer-item binding loss" isAnyPeerLoss events
  barrier <- eventPosition "missing peer-item drain barrier" isBarrier events
  assertBool "the missing item effect settles before the drain barrier" (routed < barrier)
  assertBool "the coalesced binding loss settles before the drain barrier" (loss < barrier)
  where
    isItemRoute = \case
      ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerItem _ _)) -> True
      _ -> False
    isAnyPeerLoss = \case
      KernelEvent _ (KernelTraceStepped _ _ input _) -> case inputBody input of
        RuntimeObserved (PeerBindingLost _) -> True
        _ -> False
      _ -> False
    isBarrier = \case
      ShellEvent _ ShellDrainBarrierQueued {} -> True
      _ -> False

assertCutLossBarrierOrder ::
  String ->
  (RuntimeTraceEvent -> Bool) ->
  [RuntimeTraceEvent] ->
  Assertion
assertCutLossBarrierOrder label isLoss events = do
  cut <- eventPosition (label <> " drain cut") isCut events
  loss <- eventPosition (label <> " binding loss") isLoss events
  barrier <- eventPosition (label <> " drain barrier") isBarrier events
  assertBool (label <> " loss is stepped after the cut") (cut < loss)
  assertBool (label <> " loss is stepped before the barrier") (loss < barrier)
  where
    isCut (ShellEvent _ (ShellDrainCut {})) = True
    isCut _ = False
    isBarrier (ShellEvent _ (ShellDrainBarrierQueued {})) = True
    isBarrier _ = False

eventPosition ::
  String ->
  (RuntimeTraceEvent -> Bool) ->
  [RuntimeTraceEvent] ->
  IO Int
eventPosition context matches events =
  case findIndex matches events of
    Just index -> pure index
    Nothing -> assertFailure (context <> " event is missing")

postCutApplicationInputs :: [RuntimeTraceEvent] -> [RuntimeTraceEvent] -> Int
postCutApplicationInputs cutSnapshot finalEvents =
  length
    [ ()
    | KernelEvent _ (KernelTraceStepped _ _ input _) <- drop (length cutSnapshot) finalEvents,
      ApplicationSessionInput {} <- [inputBody input]
    ]

withClosureRuntime ::
  RuntimeHooks ->
  Word64 ->
  (HeraldRuntime -> IO ()) ->
  Assertion
withClosureRuntime hooks delay use = do
  withConfiguredClosureRuntime hooks (fixtureConfiguration delay) use

withConfiguredClosureRuntime ::
  RuntimeHooks ->
  HeraldRuntimeConfiguration ->
  (HeraldRuntime -> IO ()) ->
  Assertion
withConfiguredClosureRuntime hooks configuration use = do
  started <- startHeraldRuntimeWithHooks hooks configuration
  case started of
    Left failure -> assertFailure ("runtime initialization failed: " <> show failure)
    Right runtime -> use runtime `finally` RuntimeInternal.runtimeCloseScope runtime

fixtureConfiguration :: Word64 -> HeraldRuntimeConfiguration
fixtureConfiguration delay =
  fixtureConfigurationFor
    pairLocalGenesis
    pairLocalBootstraps
    pairLocalGeneratorSeedSource
    delay

remoteFixtureConfiguration :: HeraldRuntimeConfiguration
remoteFixtureConfiguration =
  fixtureConfigurationFor
    pairRemoteGenesis
    pairRemoteBootstraps
    pairRemoteGeneratorSeedSource
    0

fixtureConfigurationFor ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  Word64 ->
  HeraldRuntimeConfiguration
fixtureConfigurationFor genesis bootstraps seedSource delay =
  heraldRuntimeConfiguration
    genesis
    bootstraps
    fixtureOracleContacts
    seedSource
    (runtimeMonotonicClock (pure (monotonicInstant 100)))
    (runtimePeerWorkDelayMicroseconds delay)
    (heraldRuntimeHandlers (const (pure ())))
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration

noRouteSortDescriptor :: ApplicationSortDescriptor
noRouteSortDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections = [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

putOnce :: MVar value -> value -> IO ()
putOnce cell value = void (tryPutMVar cell value)

awaitMVar :: String -> MVar value -> IO value
awaitMVar context cell = do
  result <- timeout 3000000 (takeMVar cell)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitReadMVar :: String -> MVar value -> IO value
awaitReadMVar context cell = do
  result <- timeout 3000000 (readMVar cell)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitChan :: String -> Chan value -> IO value
awaitChan context channel = do
  result <- timeout 3000000 (readChan channel)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitRuntimeExit ::
  String ->
  HeraldRuntime ->
  IO (Either RuntimeInternal.HeraldRuntimeFailure HeraldRuntimeExit)
awaitRuntimeExit context runtime = do
  result <- timeout 3000000 (awaitHeraldRuntimeExit runtime)
  case result of
    Just observed -> pure observed
    Nothing -> assertFailure (context <> " timed out")
