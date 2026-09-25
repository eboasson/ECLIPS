module RuntimeProperties
  ( tests,
  )
where

import Control.Concurrent
  ( forkFinally,
    killThread,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
    tryPutMVar,
  )
import Control.Exception
  ( AsyncException (ThreadKilled),
    finally,
    fromException,
  )
import Data.ByteString qualified as ByteString
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( HeraldEpoch,
    mkHeraldEpoch,
    mkHeraldId,
  )
import Eclips.Herald.Administration
  ( FinalAdminReply (..),
    adminCorrelationId,
  )
import Eclips.Herald.Application.RPC (ApplicationOutbound (..))
import Eclips.Herald.Discovery
  ( HelloRejection (HelloInactiveHerald),
    PeerCandidate,
    PeerHello,
    PeerHelloDisposition (..),
    peerCandidateConnectionNonce,
    peerHello,
    peerHelloAppliedControlIndex,
    peerHelloCatalogueDigest,
    peerHelloHeraldEpoch,
    peerHelloInitialProjectionDigest,
    peerHelloSystemId,
  )
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.PeerDispatch (PeerDispatchOutcome (PeerDispatchWritten))
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    awaitHeraldRuntimeExit,
    configureHeraldRuntimeIsolation,
    heraldRuntimeConfiguration,
    readHeraldOracleStatus,
    reportHeraldRuntimeRetiredEpochRejection,
    requestHeraldRuntimeDrain,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
    withHeraldRuntime,
  )
import Eclips.Herald.Runtime.Connection
  ( AdministrationPlane,
    ApplicationPlane,
    ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeAdministrationConnectionHandlers,
    RuntimeApplicationConnectionHandlers,
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
    registerAdministrationConnection,
    registerApplicationConnection,
    registerPeerConnection,
    submitApplicationDto,
    submitPeerHello,
  )
import Eclips.Herald.Runtime.Internal.Owner (startHeraldRuntime)
import Eclips.Herald.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (..))
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (OpenSession),
    applicationAttachmentClaim,
    applicationClientNonce,
  )
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeedSource,
    fixtureMemberSetDigest,
    fixtureMembershipGenerationId,
    fixtureOpenDto,
    fixtureOracleContacts,
    fixturePeerCandidate,
    fixturePeerHello,
    fixturePeerRecoveryConfiguration,
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
    "scoped concurrent runtime"
    [ testCase "normal callback return closes the physical scope" caseScopeReturn,
      testCase "parent cancellation closes and joins the complete runtime scope" caseParentCancellationJoinsScope,
      testCase "one runtime rejects another runtime's generation-qualified lease" caseForeignReference,
      testCase "an application LaneLost closes the admitted binding exactly once" caseApplicationLaneLost,
      testCase "an application handler exception closes only its current lane" caseApplicationHandlerException,
      testCase "a stale application handler exception cannot close its replacement" caseStaleApplicationHandlerException,
      testCase "one blocked writer cannot stop another application lane" caseBlockedWriterIsolation,
      testCase "application rejection LaneLost offers before one close" caseApplicationRejectionLaneLost,
      testCase "application rejection exception offers before one close" caseApplicationRejectionException,
      testCase "peer rejection LaneLost offers before one close" casePeerRejectionLaneLost,
      testCase "peer rejection exception offers before one close" casePeerRejectionException,
      testCase "a final-offer lease reports public close exactly once" caseRepeatedFinalOfferClose,
      testCase "orderly drain is single-shot and offers the final reply before close" caseOrderlyDrain,
      testCase "generic close cannot overtake a gated final administration offer" caseFinalAdministrationCloseRace,
      testCase "LaneLost on the final administration offer still settles drain" caseFinalAdministrationLaneLost,
      testCase "terminal publication waits for a blocked close callback" caseExitWaitsForCloseCallback,
      testCase "isolation completion bounds a blocked final application writer" caseIsolationBoundsBlockedFinalWriter,
      testCase "isolation completion preserves an ordinary final application delivery" caseIsolationDeliversFinalApplicationDisposition,
      testCase "diagnostic callback failure disables only diagnostics" caseDiagnosticFailureIsolated
    ]

caseScopeReturn :: Assertion
caseScopeReturn = do
  outcome <- withHeraldRuntime fixtureConfiguration (const (pure (37 :: Int)))
  assertEqual "the callback result and physical exit are retained" (Right (37, HeraldRuntimeScopeClosed)) outcome

caseParentCancellationJoinsScope :: Assertion
caseParentCancellationJoinsScope = do
  callbackEntered <- newEmptyMVar
  callbackBlocked <- newEmptyMVar :: IO (MVar ())
  callbackFinished <- newEmptyMVar
  closeEntered <- newEmptyMVar
  closeRelease <- newEmptyMVar
  closeFinished <- newEmptyMVar
  completionOrder <- newIORef ([] :: [String])
  parentFinished <- newEmptyMVar
  parent <-
    forkFinally
      ( withHeraldRuntime fixtureConfiguration $ \runtime ->
          ( do
              _ <-
                registerApplication
                  runtime
                  ( runtimeApplicationConnectionHandlers
                      (const (pure LaneOffered))
                      ( do
                          putOnce closeEntered ()
                          takeMVar closeRelease
                          atomicModifyIORef' completionOrder (\events -> (events <> ["writer"], ()))
                          putMVar closeFinished ()
                      )
                  )
              putMVar callbackEntered ()
              takeMVar callbackBlocked
          )
            `finally` putMVar callbackFinished ()
      )
      ( \outcome -> do
          atomicModifyIORef' completionOrder (\events -> (events <> ["parent"], ()))
          putMVar parentFinished outcome
      )
  _ <- awaitMVar "cancellable scoped callback" callbackEntered
  killThread parent
  _ <- awaitMVar "cancelled callback finalizer" callbackFinished
  _ <- awaitMVar "cancelled runtime close callback" closeEntered
  putMVar closeRelease ()
  _ <- awaitMVar "completed runtime close callback" closeFinished
  parentOutcome <- awaitMVar "joined cancelled runtime scope" parentFinished
  observedOrder <- readIORef completionOrder
  assertEqual "the writer is physically closed before the cancelled parent returns" ["writer", "parent"] observedOrder
  case parentOutcome of
    Left exception -> case fromException exception of
      Just ThreadKilled -> pure ()
      Just asyncException -> assertFailure ("parent cancellation rethrew the wrong asynchronous exception: " <> show asyncException)
      Nothing -> assertFailure ("parent cancellation rethrew a non-asynchronous exception: " <> show exception)
    Right outcome -> assertFailure ("parent cancellation unexpectedly returned: " <> show outcome)

caseForeignReference :: Assertion
caseForeignReference = do
  outcome <- withHeraldRuntime fixtureConfiguration $ \leftRuntime -> do
    leftReference <- registerApplication leftRuntime quietApplicationHandlers
    nested <- withHeraldRuntime fixtureConfiguration $ \rightRuntime -> do
      foreignResult <- closeRuntimeConnection rightRuntime leftReference
      current <- closeRuntimeConnection leftRuntime leftReference
      stale <- closeRuntimeConnection leftRuntime leftReference
      assertEqual "foreign runtime token is rejected" StalePhysicalGeneration foreignResult
      assertEqual "the owner runtime closes its current lease" ConnectionClosed current
      assertEqual "the exact lease closes at most once" StalePhysicalGeneration stale
    assertEqual "nested runtime closes normally" (Right ((), HeraldRuntimeScopeClosed)) nested
  assertEqual "outer runtime closes normally" (Right ((), HeraldRuntimeScopeClosed)) outcome

caseApplicationLaneLost :: Assertion
caseApplicationLaneLost = do
  outputSeen <- newEmptyMVar
  closed <- newEmptyMVar
  let handlers =
        runtimeApplicationConnectionHandlers
          (\outbound -> putMVar outputSeen outbound >> pure LaneLost)
          (putOnce closed ())
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <- registerApplication runtime handlers
    submitted <- submitApplicationDto runtime reference fixtureOpenDto
    assertEqual "the immutable DTO enters its source FIFO" Queued submitted
    outbound <- awaitMVar "application disposition" outputSeen
    case outbound of
      AcceptApplicationCandidate {} -> pure ()
      _ -> assertFailure ("expected accepted application candidate, got " <> show outbound)
    _ <- awaitMVar "application lane close" closed
    closeAgain <- closeRuntimeConnection runtime reference
    assertEqual "LaneLost already invalidated the exact lease" StalePhysicalGeneration closeAgain
  assertEqual "lane loss does not fail the Herald role" (Right ((), HeraldRuntimeScopeClosed)) outcome

caseApplicationHandlerException :: Assertion
caseApplicationHandlerException = do
  outputSeen <- newEmptyMVar
  closed <- newEmptyMVar
  let handlers =
        runtimeApplicationConnectionHandlers
          (\outbound -> putOnce outputSeen outbound >> ioError (userError "intentional current handler failure"))
          (putOnce closed ())
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <- registerApplication runtime handlers
    assertEqual "the Open enters the current source FIFO" Queued
      =<< submitApplicationDto runtime reference fixtureOpenDto
    _ <- awaitMVar "failing application offer" outputSeen
    _ <- awaitMVar "failed current lane close" closed
    assertEqual "the failed exact lease is already stale" StalePhysicalGeneration
      =<< closeRuntimeConnection runtime reference
    replacement <- registerApplication runtime quietApplicationHandlers
    assertEqual "the Herald role remains available after lane failure" ConnectionClosed
      =<< closeRuntimeConnection runtime replacement
  assertEqual
    "a current handler exception is connection-scoped"
    (Right ((), HeraldRuntimeScopeClosed))
    outcome

caseStaleApplicationHandlerException :: Assertion
caseStaleApplicationHandlerException = do
  failEntered <- newEmptyMVar
  releaseFailure <- newEmptyMVar
  oldClosed <- newEmptyMVar
  replacementOutput <- newEmptyMVar
  replacementClosed <- newEmptyMVar
  let failingHandlers =
        runtimeApplicationConnectionHandlers
          (\_ -> putOnce failEntered () >> takeMVar releaseFailure >> ioError (userError "intentional stale handler failure"))
          (putOnce oldClosed ())
      replacementHandlers =
        runtimeApplicationConnectionHandlers
          (\outbound -> putOnce replacementOutput outbound >> pure LaneOffered)
          (putOnce replacementClosed ())
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    oldReference <- registerApplication runtime failingHandlers
    assertEqual "the old Open enters its source FIFO" Queued
      =<< submitApplicationDto runtime oldReference fixtureOpenDto
    _ <- awaitMVar "old handler entered" failEntered

    replacement <- registerApplication runtime replacementHandlers
    assertEqual "the exact retry enters the replacement source" Queued
      =<< submitApplicationDto runtime replacement fixtureOpenDto
    _ <- awaitMVar "replacement application acceptance" replacementOutput
    putMVar releaseFailure ()
    _ <- awaitMVar "old route closed after its stale failure returns" oldClosed
    assertEqual "the stale route remains stale after its late exception" StalePhysicalGeneration
      =<< closeRuntimeConnection runtime oldReference
    assertEqual "the replacement binding remains current" ConnectionClosed
      =<< closeRuntimeConnection runtime replacement
    _ <- awaitMVar "replacement close" replacementClosed
    pure ()
  assertEqual
    "a late stale-worker exception cannot fail the role or replacement"
    (Right ((), HeraldRuntimeScopeClosed))
    outcome

caseBlockedWriterIsolation :: Assertion
caseBlockedWriterIsolation = do
  blockedEntered <- newEmptyMVar
  releaseBlocked <- newEmptyMVar
  blockedClosed <- newEmptyMVar
  independentOutput <- newEmptyMVar
  independentClosed <- newEmptyMVar
  let blockedHandlers =
        runtimeApplicationConnectionHandlers
          (\_ -> putOnce blockedEntered () >> takeMVar releaseBlocked >> pure LaneOffered)
          (putOnce blockedClosed ())
      independentHandlers =
        runtimeApplicationConnectionHandlers
          (\outbound -> putOnce independentOutput outbound >> pure LaneOffered)
          (putOnce independentClosed ())
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    blocked <- registerApplication runtime blockedHandlers
    assertEqual "the blocked Open enters its source FIFO" Queued
      =<< submitApplicationDto runtime blocked fixtureOpenDto
    _ <- awaitMVar "blocked application writer" blockedEntered

    independent <- registerApplication runtime independentHandlers
    assertEqual "the unrelated Open enters its own FIFO" Queued
      =<< submitApplicationDto runtime independent fixtureIndependentOpenDto
    outbound <- awaitMVar "unrelated application offer" independentOutput
    case outbound of
      AcceptApplicationCandidate {} -> pure ()
      _ -> assertFailure ("expected unrelated application acceptance, got " <> show outbound)
    assertEqual "the independent route closes while its sibling is blocked" ConnectionClosed
      =<< closeRuntimeConnection runtime independent
    _ <- awaitMVar "independent close" independentClosed

    putMVar releaseBlocked ()
    assertEqual "the formerly blocked route closes normally" ConnectionClosed
      =<< closeRuntimeConnection runtime blocked
    _ <- awaitMVar "blocked route close" blockedClosed
    pure ()
  assertEqual
    "one egress callback cannot stall independent lanes or the owner"
    (Right ((), HeraldRuntimeScopeClosed))
    outcome

caseApplicationRejectionLaneLost :: Assertion
caseApplicationRejectionLaneLost =
  caseApplicationRejection "LaneLost" (pure LaneLost)

caseApplicationRejectionException :: Assertion
caseApplicationRejectionException =
  caseApplicationRejection
    "exception"
    (ioError (userError "intentional application rejection offer failure"))

caseApplicationRejection :: String -> IO RuntimeLaneOffer -> Assertion
caseApplicationRejection failureClass offerResult = do
  events <- newIORef []
  closed <- newEmptyMVar
  let handlers =
        runtimeApplicationConnectionHandlers
          (\outbound -> appendEvent events (ApplicationRejectionOffered outbound) >> offerResult)
          (appendEvent events ApplicationRejectionClosed >> putOnce closed ())
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <- registerApplication runtime handlers
    assertEqual "the state-invalid Open enters its source FIFO" Queued
      =<< submitApplicationDto runtime reference fixtureRejectedOpenDto
    _ <- awaitMVar ("application rejection " <> failureClass <> " close") closed
    assertEqual "the rejected application lease is stale" StalePhysicalGeneration
      =<< closeRuntimeConnection runtime reference
  assertEqual
    ("application rejection " <> failureClass <> " is connection-scoped")
    (Right ((), HeraldRuntimeScopeClosed))
    outcome
  observed <- readIORef events
  case observed of
    [ApplicationRejectionOffered RejectApplicationCandidate {}, ApplicationRejectionClosed] -> pure ()
    _ -> assertFailure ("application rejection did not offer before close: " <> show observed)

casePeerRejectionLaneLost :: Assertion
casePeerRejectionLaneLost = casePeerRejection "LaneLost" (pure LaneLost)

casePeerRejectionException :: Assertion
casePeerRejectionException =
  casePeerRejection
    "exception"
    (ioError (userError "intentional peer rejection offer failure"))

casePeerRejection :: String -> IO RuntimeLaneOffer -> Assertion
casePeerRejection failureClass offerResult = do
  events <- newIORef []
  closed <- newEmptyMVar
  let handlers = peerRejectionHandlers events closed offerResult
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <- registerPeer runtime handlers
    assertEqual "the rejected peer Hello enters its source FIFO" Queued
      =<< submitPeerHello
        runtime
        reference
        fixturePeerCandidate
        mempty
        (fixturePeerHelloInactive (fixturePeerCandidate))
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    _ <- awaitMVar ("peer rejection " <> failureClass <> " close") closed
    assertEqual "the rejected peer lease is stale" StalePhysicalGeneration
      =<< closeRuntimeConnection runtime reference
  assertEqual
    ("peer rejection " <> failureClass <> " is connection-scoped")
    (Right ((), HeraldRuntimeScopeClosed))
    outcome
  assertEqual
    "the peer rejection is offered before one close"
    [PeerRejectionOffered (PeerHelloRejected HelloInactiveHerald), PeerRejectionClosed]
    =<< readIORef events

caseRepeatedFinalOfferClose :: Assertion
caseRepeatedFinalOfferClose = do
  offerEntered <- newEmptyMVar
  releaseOffer <- newEmptyMVar
  closed <- newEmptyMVar
  let handlers =
        runtimeApplicationConnectionHandlers
          (\_ -> putOnce offerEntered () >> takeMVar releaseOffer >> pure LaneOffered)
          (putOnce closed ())
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <- registerApplication runtime handlers
    assertEqual "the rejected Open enters its source FIFO" Queued
      =<< submitApplicationDto runtime reference fixtureRejectedOpenDto
    _ <- awaitMVar "application final offer" offerEntered
    assertEqual "the first close invalidates the public lease" ConnectionClosed
      =<< closeRuntimeConnection runtime reference
    assertEqual "a repeated close observes the invalidated public lease" StalePhysicalGeneration
      =<< closeRuntimeConnection runtime reference
    assertEqual "subsequent ingress also observes the invalidated public lease" StalePhysicalGeneration
      =<< submitApplicationDto runtime reference fixtureOpenDto
    putMVar releaseOffer ()
    _ <- awaitMVar "application final close" closed
    pure ()
  assertEqual
    "the privately writer-owned final marker closes without failing the role"
    (Right ((), HeraldRuntimeScopeClosed))
    outcome

caseOrderlyDrain :: Assertion
caseOrderlyDrain = do
  events <- newIORef []
  closeObserved <- newEmptyMVar
  let correlation = adminCorrelationId 91
      handlers = administrationHandlers events closeObserved
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <- registerAdministration runtime handlers
    first <- requestHeraldRuntimeDrain runtime reference correlation
    duplicate <- requestHeraldRuntimeDrain runtime reference correlation
    different <- requestHeraldRuntimeDrain runtime reference (adminCorrelationId 92)
    assertEqual "the first current request owns the drain" Queued first
    assertEqual "a repeated request cannot retarget the drain" DrainRequestAlreadyPending duplicate
    assertEqual "a different correlation cannot create a second drain" DrainRequestAlreadyPending different
    _ <- awaitMVar "administration close marker" closeObserved
    exit <- awaitRuntimeExit "orderly drain" runtime
    assertEqual "the semantic drain reaches its terminal result" (Right HeraldRuntimeDrained) exit
  assertEqual "the scoped result preserves the semantic exit" (Right ((), HeraldRuntimeDrained)) outcome
  observed <- readIORef events
  assertEqual
    "the optional final reply is offered before the one close action"
    [AdminOffered (Just (OrderlyHeraldShutdownCompleted correlation)), AdminClosed]
    observed

caseFinalAdministrationCloseRace :: Assertion
caseFinalAdministrationCloseRace = do
  gates <- newAdminGates
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <- registerAdministration runtime (gatedAdministrationHandlers gates LaneOffered)
    let correlation = adminCorrelationId 101
    assertEqual "the drain request is admitted" Queued
      =<< requestHeraldRuntimeDrain runtime reference correlation
    offered <- awaitMVar "gated final administration offer" (adminOfferEntered gates)
    assertEqual
      "the final reply is already owned by the writer"
      (Just (OrderlyHeraldShutdownCompleted correlation))
      offered
    assertEqual
      "concurrent generic close invalidates the physical lease once"
      ConnectionClosed
      =<< closeRuntimeConnection runtime reference
    putMVar (adminReleaseOffer gates) ()
    _ <- awaitMVar "administration close callback" (adminCloseEntered gates)
    assertExitUnpublished "blocked final close callback" runtime
    putMVar (adminReleaseClose gates) ()
    assertEqual "the close race retains orderly drain" (Right HeraldRuntimeDrained)
      =<< awaitRuntimeExit "final-offer close race" runtime
  assertEqual "the scoped result retains the drain exit" (Right ((), HeraldRuntimeDrained)) outcome
  assertEqual
    "generic close cannot reorder the final offer and close callback"
    [AdminOffered (Just (OrderlyHeraldShutdownCompleted (adminCorrelationId 101))), AdminClosed]
    =<< readIORef (adminEvents gates)

caseFinalAdministrationLaneLost :: Assertion
caseFinalAdministrationLaneLost = do
  gates <- newAdminGates
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <- registerAdministration runtime (gatedAdministrationHandlers gates LaneLost)
    let correlation = adminCorrelationId 102
    assertEqual "the drain request is admitted" Queued
      =<< requestHeraldRuntimeDrain runtime reference correlation
    offered <- awaitMVar "LaneLost final administration offer" (adminOfferEntered gates)
    assertEqual
      "the optional final reply reaches the lane"
      (Just (OrderlyHeraldShutdownCompleted correlation))
      offered
    putMVar (adminReleaseOffer gates) ()
    _ <- awaitMVar "LaneLost close callback" (adminCloseEntered gates)
    assertEqual
      "LaneLost invalidates the administration generation before close callback completion"
      StalePhysicalGeneration
      =<< closeRuntimeConnection runtime reference
    assertExitUnpublished "blocked LaneLost close callback" runtime
    putMVar (adminReleaseClose gates) ()
    assertEqual "LaneLost settles the already-owned final marker" (Right HeraldRuntimeDrained)
      =<< awaitRuntimeExit "final-offer LaneLost" runtime
  assertEqual "LaneLost does not replace the semantic drain exit" (Right ((), HeraldRuntimeDrained)) outcome

caseExitWaitsForCloseCallback :: Assertion
caseExitWaitsForCloseCallback = do
  closeEntered <- newEmptyMVar
  releaseClose <- newEmptyMVar
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    reference <-
      registerApplication
        runtime
        ( runtimeApplicationConnectionHandlers
            (const (pure LaneOffered))
            (putOnce closeEntered () >> takeMVar releaseClose)
        )
    assertEqual "the physical lease closes" ConnectionClosed
      =<< closeRuntimeConnection runtime reference
    _ <- awaitMVar "blocked application close callback" closeEntered
    assertExitUnpublished "blocked connection child" runtime
    putMVar releaseClose ()
  assertEqual
    "scope exit is published only after the connection worker has joined"
    (Right ((), HeraldRuntimeScopeClosed))
    outcome

caseIsolationBoundsBlockedFinalWriter :: Assertion
caseIsolationBoundsBlockedFinalWriter = do
  accepted <- newEmptyMVar
  isolationBegun <- newEmptyMVar
  finalEntered <- newEmptyMVar
  blockFinal <- newEmptyMVar
  closed <- newEmptyMVar
  let handlers =
        runtimeApplicationConnectionHandlers
          ( \case
              AcceptApplicationCandidate {} ->
                putOnce accepted () >> pure LaneOffered
              SendEstablishedApplication {} ->
                putOnce isolationBegun () >> pure LaneOffered
              DisposeEstablishedApplicationSession {} -> do
                putOnce finalEntered ()
                takeMVar blockFinal
                pure LaneOffered
              _ -> pure LaneOffered
          )
          (putOnce closed ())
  runtime <- startIsolationRuntime
  ( do
      reference <- registerApplication runtime handlers
      assertEqual "the isolation test Open enters the source FIFO" Queued
        =<< submitApplicationDto runtime reference fixtureOpenDto
      _ <- awaitMVar "isolation test application acceptance" accepted
      beginFixtureIsolation runtime
      _ <- awaitMVar "read-only isolation disposition" isolationBegun
      _ <- awaitMVar "blocked terminal application disposition" finalEntered
      _ <- awaitMVar "blocked isolation writer cancellation and close" closed
      assertIsolationControlShell runtime
    )
    `finally` RuntimeInternal.runtimeCloseScope runtime

caseIsolationDeliversFinalApplicationDisposition :: Assertion
caseIsolationDeliversFinalApplicationDisposition = do
  accepted <- newEmptyMVar
  isolationBegun <- newEmptyMVar
  finalDelivered <- newEmptyMVar
  closed <- newEmptyMVar
  events <- newIORef ([] :: [String])
  let record event = appendEvent events event
      handlers =
        runtimeApplicationConnectionHandlers
          ( \case
              AcceptApplicationCandidate {} -> do
                record "accepted"
                putOnce accepted ()
                pure LaneOffered
              SendEstablishedApplication {} -> do
                record "isolation-begun"
                putOnce isolationBegun ()
                pure LaneOffered
              DisposeEstablishedApplicationSession {} -> do
                record "terminal-delivered"
                putOnce finalDelivered ()
                pure LaneOffered
              _ -> pure LaneOffered
          )
          (record "closed" >> putOnce closed ())
  runtime <- startIsolationRuntime
  ( do
      reference <- registerApplication runtime handlers
      assertEqual "the ordinary isolation Open enters the source FIFO" Queued
        =<< submitApplicationDto runtime reference fixtureOpenDto
      _ <- awaitMVar "ordinary isolation application acceptance" accepted
      beginFixtureIsolation runtime
      _ <- awaitMVar "ordinary read-only isolation disposition" isolationBegun
      _ <- awaitMVar "ordinary terminal application delivery" finalDelivered
      _ <- awaitMVar "ordinary isolation application close" closed
      assertEqual
        "successful final delivery precedes application close"
        ["accepted", "isolation-begun", "terminal-delivered", "closed"]
        =<< readIORef events
      assertIsolationControlShell runtime
    )
    `finally` RuntimeInternal.runtimeCloseScope runtime

assertIsolationControlShell :: HeraldRuntime -> Assertion
assertIsolationControlShell runtime = do
  assertExitUnpublished "self-fenced control shell" runtime
  observed <- timeout 2000000 (readHeraldOracleStatus runtime)
  case observed of
    Just (Right _) -> pure ()
    other -> assertFailure ("self-fenced owner status query: " <> show other)
  rejected <- newEmptyMVar
  candidate <-
    registerApplication runtime
      $ runtimeApplicationConnectionHandlers
        (\outbound -> putOnce rejected outbound >> pure LaneOffered)
        (pure ())
  assertEqual "new application candidate reaches the fenced owner" Queued
    =<< submitApplicationDto runtime candidate fixtureOpenDto
  response <- awaitMVar "fenced application rejection" rejected
  case response of
    RejectApplicationCandidate {} -> pure ()
    other -> assertFailure ("self-fenced owner reopened application service: " <> show other)
  administration <- registerAdministration runtime (runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
  assertEqual "the retained control shell accepts explicit orderly shutdown" Queued
    =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 103)
  assertEqual "explicit shutdown joins the retained control shell" (Right HeraldRuntimeDrained)
    =<< awaitRuntimeExit "self-fenced control-shell drain" runtime

caseDiagnosticFailureIsolated :: Assertion
caseDiagnosticFailureIsolated = do
  calls <- newIORef (0 :: Int)
  firstCall <- newEmptyMVar
  let failDiagnostic _ = do
        atomicModifyIORef' calls (\count -> (count + 1, ()))
        putOnce firstCall ()
        ioError (userError "intentional diagnostic callback failure")
      configuration = fixtureConfigurationWithDiagnostic failDiagnostic
  outcome <- withHeraldRuntime configuration $ \runtime -> do
    first <- registerApplication runtime quietApplicationHandlers
    _ <- awaitMVar "first diagnostic callback" firstCall
    second <- registerApplication runtime quietApplicationHandlers
    assertEqual "the first application still closes normally" ConnectionClosed
      =<< closeRuntimeConnection runtime first
    assertEqual "the role accepts work after diagnostics fail" ConnectionClosed
      =<< closeRuntimeConnection runtime second
  assertEqual
    "a nonessential diagnostic failure cannot fail the Herald role"
    (Right ((), HeraldRuntimeScopeClosed))
    outcome
  assertEqual "diagnostics are disabled after the first callback failure" 1 =<< readIORef calls

fixtureConfiguration :: HeraldRuntimeConfiguration
fixtureConfiguration = fixtureConfigurationWithDiagnostic (const (pure ()))

fixtureIsolationConfiguration :: HeraldRuntimeConfiguration
fixtureIsolationConfiguration =
  configureHeraldRuntimeIsolation
    (Set.singleton fixtureVoterHeraldEpoch)
    (checked "runtime isolation configuration" (checkIsolationConfiguration 60_000_000 10_000))
    ( heraldRuntimeConfiguration
        fixtureCheckedGenesis
        fixtureCheckedBootstraps
        fixtureOracleContacts
        fixtureGeneratorSeedSource
        systemRuntimeMonotonicClock
        (runtimePeerWorkDelayMicroseconds 0)
        (heraldRuntimeHandlers (const (pure ())))
        fixtureApplicationRecoveryConfiguration
        fixturePeerRecoveryConfiguration
    )

fixtureVoterHeraldEpoch :: HeraldEpoch
fixtureVoterHeraldEpoch =
  peerHelloHeraldEpoch
    (fixturePeerHello (peerCandidateConnectionNonce fixturePeerCandidate))

startIsolationRuntime :: IO HeraldRuntime
startIsolationRuntime = do
  started <- startHeraldRuntime fixtureIsolationConfiguration
  either
    (assertFailure . ("isolation runtime startup failed: " <>) . show)
    pure
    started

beginFixtureIsolation :: HeraldRuntime -> IO ()
beginFixtureIsolation runtime =
  assertEqual
    "the checked voter rejection enters the runtime"
    Queued
    =<< reportHeraldRuntimeRetiredEpochRejection
      runtime
      fixtureVoterHeraldEpoch
      fixtureMembershipGenerationId

fixtureConfigurationWithDiagnostic ::
  (RuntimeInternal.RuntimeDiagnostic -> IO ()) ->
  HeraldRuntimeConfiguration
fixtureConfigurationWithDiagnostic diagnostic =
  heraldRuntimeConfiguration
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeedSource
    (runtimeMonotonicClock (pure (monotonicInstant 100)))
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers diagnostic)
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration

registerApplication :: HeraldRuntime -> RuntimeApplicationConnectionHandlers -> IO (ConnectionRef ApplicationPlane)
registerApplication runtime handlers = do
  registration <- registerApplicationConnection runtime handlers
  case registration of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "running runtime rejected application registration"

registerAdministration :: HeraldRuntime -> RuntimeAdministrationConnectionHandlers -> IO (ConnectionRef AdministrationPlane)
registerAdministration runtime handlers = do
  registration <- registerAdministrationConnection runtime handlers
  case registration of
    AdministrationConnectionRegistered reference -> pure reference
    AdministrationRegistrationUnavailable -> assertFailure "first administration registration was unavailable"
    AdministrationRegistrationStopped -> assertFailure "running runtime rejected administration registration"

registerPeer :: HeraldRuntime -> RuntimePeerConnectionHandlers -> IO (ConnectionRef PeerPlane)
registerPeer runtime handlers = do
  registration <- registerPeerConnection runtime handlers
  case registration of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "running runtime rejected peer registration"

quietApplicationHandlers :: RuntimeApplicationConnectionHandlers
quietApplicationHandlers = runtimeApplicationConnectionHandlers (const (pure LaneOffered)) (pure ())

fixtureRejectedOpenDto :: ApplicationClientDto
fixtureRejectedOpenDto =
  OpenSession
    (checked "unknown application attachment" (applicationAttachmentClaim (ByteString.replicate 32 0xfe)))
    (applicationClientNonce 702)

fixtureIndependentOpenDto :: ApplicationClientDto
fixtureIndependentOpenDto =
  case fixtureOpenDto of
    OpenSession attachment _ -> OpenSession attachment (applicationClientNonce 703)
    dto -> error ("fixtureOpenDto is not OpenSession: " <> show dto)

fixturePeerHelloInactive :: PeerCandidate -> PeerHello
fixturePeerHelloInactive candidate =
  peerHello
    (peerHelloSystemId base)
    (checked "inactive HeraldId" (mkHeraldId (ByteString.replicate 32 0xed)))
    (checked "inactive HeraldEpoch" (mkHeraldEpoch (ByteString.replicate 32 0xec)))
    (peerCandidateConnectionNonce candidate)
    mempty
    (peerHelloAppliedControlIndex base)
    (peerHelloCatalogueDigest base)
    (peerHelloInitialProjectionDigest base)
    Nothing
  where
    base = fixturePeerHello (peerCandidateConnectionNonce candidate)

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

data ApplicationRejectionEvent
  = ApplicationRejectionOffered ApplicationOutbound
  | ApplicationRejectionClosed
  deriving stock (Eq, Show)

data PeerRejectionEvent
  = PeerRejectionOffered PeerHelloDisposition
  | PeerRejectionClosed
  deriving stock (Eq, Show)

peerRejectionHandlers ::
  IORef [PeerRejectionEvent] ->
  MVar () ->
  IO RuntimeLaneOffer ->
  RuntimePeerConnectionHandlers
peerRejectionHandlers events closed offerResult =
  runtimePeerConnectionHandlers
    (\_ _ -> pure LaneOffered)
    (\_ disposition -> appendEvent events (PeerRejectionOffered disposition) >> offerResult)
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ _ -> pure PeerDispatchWritten)
    (appendEvent events PeerRejectionClosed >> putOnce closed ())

data AdminEvent
  = AdminOffered (Maybe FinalAdminReply)
  | AdminClosed
  deriving stock (Eq, Show)

data AdminGates = AdminGates
  { adminEvents :: IORef [AdminEvent],
    adminOfferEntered :: MVar (Maybe FinalAdminReply),
    adminReleaseOffer :: MVar (),
    adminCloseEntered :: MVar (),
    adminReleaseClose :: MVar ()
  }

newAdminGates :: IO AdminGates
newAdminGates =
  AdminGates
    <$> newIORef []
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar

gatedAdministrationHandlers :: AdminGates -> RuntimeLaneOffer -> RuntimeAdministrationConnectionHandlers
gatedAdministrationHandlers gates offerResult =
  runtimeAdministrationConnectionHandlers
    ( \reply -> do
        appendEvent (adminEvents gates) (AdminOffered reply)
        putOnce (adminOfferEntered gates) reply
        takeMVar (adminReleaseOffer gates)
        pure offerResult
    )
    ( do
        appendEvent (adminEvents gates) AdminClosed
        putOnce (adminCloseEntered gates) ()
        takeMVar (adminReleaseClose gates)
    )

administrationHandlers :: IORef [AdminEvent] -> MVar () -> RuntimeAdministrationConnectionHandlers
administrationHandlers events closeObserved =
  runtimeAdministrationConnectionHandlers
    (\reply -> appendEvent events (AdminOffered reply) >> pure LaneOffered)
    (appendEvent events AdminClosed >> putOnce closeObserved ())

appendEvent :: IORef [event] -> event -> IO ()
appendEvent events event = atomicModifyIORef' events (\retained -> (retained <> [event], ()))

putOnce :: MVar value -> value -> IO ()
putOnce cell value = do
  _ <- tryPutMVar cell value
  pure ()

awaitMVar :: String -> MVar value -> IO value
awaitMVar context cell = do
  result <- timeout 2000000 (takeMVar cell)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitRuntimeExit ::
  String ->
  HeraldRuntime ->
  IO (Either RuntimeInternal.HeraldRuntimeFailure HeraldRuntimeExit)
awaitRuntimeExit context runtime = do
  result <- timeout 2000000 (awaitHeraldRuntimeExit runtime)
  case result of
    Just observed -> pure observed
    Nothing -> assertFailure (context <> " timed out")

assertExitUnpublished :: String -> HeraldRuntime -> Assertion
assertExitUnpublished context runtime = do
  observed <- RuntimeInternal.runtimeTryExit runtime
  assertEqual (context <> " cannot observe a terminal result") Nothing observed
