module TransferProperties
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
    tryReadMVar,
  )
import Control.Concurrent.STM
  ( TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    writeTVar,
  )
import Control.Exception (finally)
import Control.Monad (when)
import Eclips.Herald.Application.RPC
  ( ApplicationOutbound (..),
  )
import Eclips.Herald.Application.Session (ApplicationSessionBinding)
import Eclips.Herald.Discovery
  ( BindingAdmission (ReplacedBinding),
    PeerBinding,
    PeerHello,
    PeerHelloDisposition (..),
    connectionNonce,
    peerCandidate,
    peerCandidateInitiatorHeraldEpoch,
    peerCandidateInitiatorHeraldId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (RuntimeObserved),
    RuntimeObservation (..),
    inputBody,
  )
import Eclips.Herald.PeerDispatch (PeerDispatchOutcome (..))
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    heraldRuntimeConfiguration,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
    withHeraldRuntime,
  )
import Eclips.Herald.Runtime.Connection
  ( ApplicationPlane,
    ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeApplicationConnectionHandlers,
    RuntimeLaneOffer (..),
    RuntimePeerConnectionHandlers,
    heraldRuntimeHandlers,
    runtimeApplicationConnectionHandlers,
    runtimePeerConnectionHandlers,
    runtimePeerConnectionHandlersWithRetirement,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    registerApplicationConnection,
    registerPeerConnection,
    submitApplicationDto,
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
    PhysicalConnectionRef (PhysicalPeerRef),
    RuntimeTraceEvent (..),
    TraceLedger,
    snapshotTraceLedger,
  )
import Eclips.Herald.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (..))
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (..),
    ApplicationServerDto (..),
    ApplicationSessionClaim,
    ApplicationSessionErrorDto (ApplicationSessionNotLiveDto),
    applicationRequestIdClaim,
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
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "same-binding physical transfer"
    [ testCase "exact Open retry rehomes the live binding without old-route loss" caseOpenRetryRehome,
      testCase "Resume reoffer rehomes the live binding without old-route loss" caseResumeRehome,
      testCase "established loss permanently retires its session before a Resume retry" caseResumeAfterOldLoss,
      testCase "equal peer Hello reoffer rehomes the exact binding" casePeerEqualBindingRehome,
      testCase "replacement peer Hello retires the old remote binding" casePeerReplacementRehome,
      testCase
        "peer handoff retires a disposition already dequeued by the old writer"
        caseDequeuedPeerDispositionRetirement,
      testCase "Open retry promotion suppresses a loss queued after owner handoff" caseOpenRetryHandoffFence,
      testCase "failed Open retry target releases exactly one accepted-binding loss" caseOpenRetryFailedTarget,
      testCase "Resume retry promotion suppresses a loss queued after owner handoff" caseResumeRetryHandoffFence,
      testCase "failed Resume retry target releases exactly one accepted-binding loss" caseResumeRetryFailedTarget,
      testCase "equal peer Hello promotion suppresses a loss queued after owner handoff" casePeerRetryHandoffFence,
      testCase "failed equal-Hello target releases exactly one accepted-binding loss" casePeerRetryFailedTarget,
      testCase
        "failed replacement peer target retires the superseded remote epoch"
        casePeerReplacementFailedTarget
    ]

caseOpenRetryRehome :: Assertion
caseOpenRetryRehome = do
  oldOutput <- newChan
  newOutput <- newChan
  oldClosed <- newEmptyMVar
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    oldReference <- registerApplication runtime (applicationHandlers oldOutput oldClosed)
    assertQueued "initial Open" =<< submitApplicationDto runtime oldReference fixtureOpenDto
    initial <- awaitAcceptance "initial Open acceptance" oldOutput

    newClosed <- newEmptyMVar
    newReference <- registerApplication runtime (applicationHandlers newOutput newClosed)
    assertQueued "exact Open retry" =<< submitApplicationDto runtime newReference fixtureOpenDto
    retried <- awaitAcceptance "exact Open retry acceptance" newOutput

    assertEqual "the exact retry retains the semantic binding" (acceptedBinding initial) (acceptedBinding retried)
    assertEqual "the exact retry returns the retained opening result" (acceptedDto initial) (acceptedDto retried)
    awaitClosed "superseded Open route" oldClosed
    assertEqual
      "the superseded lease is already stale"
      StalePhysicalGeneration
      =<< closeRuntimeConnection runtime oldReference
    probeLiveBinding runtime newReference newOutput retried
  assertEqual "transfer keeps the role live" (Right ((), HeraldRuntimeScopeClosed)) outcome

caseResumeRehome :: Assertion
caseResumeRehome = do
  oldOutput <- newChan
  resumedOutput <- newChan
  newOutput <- newChan
  oldClosed <- newEmptyMVar
  resumedClosed <- newEmptyMVar
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    oldReference <- registerApplication runtime (applicationHandlers oldOutput oldClosed)
    assertQueued "initial Open" =<< submitApplicationDto runtime oldReference fixtureOpenDto
    initial <- awaitAcceptance "initial Open acceptance" oldOutput
    resume <- resumeDto (acceptedDto initial)

    resumedReference <- registerApplication runtime (applicationHandlers resumedOutput resumedClosed)
    assertQueued "first Resume" =<< submitApplicationDto runtime resumedReference resume
    firstResume <- awaitAcceptance "first Resume acceptance" resumedOutput
    assertSessionResumed (acceptedDto firstResume)
    awaitClosed "superseded pre-Resume route" oldClosed
    assertEqual
      "the pre-Resume binding cannot coexist with the advanced binding"
      StalePhysicalGeneration
      =<< closeRuntimeConnection runtime oldReference

    newClosed <- newEmptyMVar
    newReference <- registerApplication runtime (applicationHandlers newOutput newClosed)
    assertQueued "exact Resume reoffer" =<< submitApplicationDto runtime newReference resume
    retriedResume <- awaitAcceptance "exact Resume reoffer acceptance" newOutput

    assertEqual "exact Resume retry reuses its accepted binding" (acceptedBinding firstResume) (acceptedBinding retriedResume)
    assertSessionResumed (acceptedDto retriedResume)
    awaitClosed "superseded Resume route" resumedClosed
    assertEqual
      "the superseded lease cannot report a later loss"
      StalePhysicalGeneration
      =<< closeRuntimeConnection runtime resumedReference
    probeLiveBinding runtime newReference newOutput retriedResume
  assertEqual "transfer keeps the role live" (Right ((), HeraldRuntimeScopeClosed)) outcome

caseResumeAfterOldLoss :: Assertion
caseResumeAfterOldLoss = do
  gate <- newTransferGate
  oldOutput <- newChan
  newOutput <- newChan
  oldClosed <- newEmptyMVar
  withTransferRuntime gate hasApplicationAcceptance $ \runtime -> do
    oldReference <- registerApplication runtime (applicationHandlers oldOutput oldClosed)
    assertQueued "initial Open" =<< submitApplicationDto runtime oldReference fixtureOpenDto
    initial <- awaitAcceptance "initial Open acceptance" oldOutput
    probeLiveBinding runtime oldReference oldOutput initial
    resume <- resumeDto (acceptedDto initial)

    assertEqual "established route closes once" ConnectionClosed =<< closeRuntimeConnection runtime oldReference
    awaitClosed "established route before Resume retry" oldClosed
    assertEqual "repeat established close is stale" StalePhysicalGeneration =<< closeRuntimeConnection runtime oldReference
    _ <- awaitTransferTrace "session loss is stepped before its retry" gate ((>= 1) . applicationLossCount (acceptedBinding initial))

    newClosed <- newEmptyMVar
    newReference <- registerApplication runtime (applicationHandlers newOutput newClosed)
    assertQueued "Resume retry after loss" =<< submitApplicationDto runtime newReference resume
    rejected <- awaitChan "post-loss Resume rejection" newOutput
    case rejected of
      RejectApplicationCandidate _ reply -> assertEqual "an established session cannot resume after its owner observes loss" (SessionRejected ApplicationSessionNotLiveDto) reply
      other -> assertFailure ("expected dead-session rejection, got " <> show other)
    awaitClosed "rejected post-loss replacement" newClosed
    assertEqual "the rejected replacement is stale" StalePhysicalGeneration =<< closeRuntimeConnection runtime newReference
    assertEqual "late retries do not repeat the original loss" 1 . applicationLossCount (acceptedBinding initial) =<< snapshotTransferTrace gate

casePeerEqualBindingRehome :: Assertion
casePeerEqualBindingRehome = do
  oldDispositions <- newChan
  newDispositions <- newChan
  oldClosed <- newEmptyMVar
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    oldReference <- registerPeer runtime (peerHandlers oldDispositions oldClosed)
    assertQueued "initial peer Hello"
      =<< submitPeerHello
        runtime
        oldReference
        fixturePeerCandidate
        mempty
        fixtureRemoteHello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    initialBinding <- awaitAcceptedPeer "initial peer acceptance" oldDispositions

    newClosed <- newEmptyMVar
    newReference <- registerPeer runtime (peerHandlers newDispositions newClosed)
    assertQueued "equal peer Hello reoffer"
      =<< submitPeerHello
        runtime
        newReference
        fixturePeerCandidate
        mempty
        fixtureRemoteHello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    retriedBinding <- awaitAcceptedPeer "equal peer reoffer acceptance" newDispositions

    assertEqual "equal Hello retains the exact peer binding" initialBinding retriedBinding
    awaitClosed "superseded peer route" oldClosed
    assertEqual "the superseded peer lease is stale" StalePhysicalGeneration =<< closeRuntimeConnection runtime oldReference
  assertEqual "peer rehome keeps the role live" (Right ((), HeraldRuntimeScopeClosed)) outcome

casePeerReplacementRehome :: Assertion
casePeerReplacementRehome = do
  oldDispositions <- newChan
  replacementDispositions <- newChan
  oldClosed <- newEmptyMVar
  let replacementCandidate = fixturePeerCandidate
      initialNonce = connectionNonce 78
      initialCandidate =
        peerCandidate
          (peerCandidateInitiatorHeraldId replacementCandidate)
          (peerCandidateInitiatorHeraldEpoch replacementCandidate)
          initialNonce
  outcome <- withHeraldRuntime fixtureConfiguration $ \runtime -> do
    oldReference <- registerPeer runtime (peerHandlers oldDispositions oldClosed)
    assertQueued "initial larger peer Hello"
      =<< submitPeerHello
        runtime
        oldReference
        initialCandidate
        mempty
        (fixturePeerHello initialNonce)
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    initialBinding <- awaitAcceptedPeer "initial larger peer acceptance" oldDispositions

    replacementClosed <- newEmptyMVar
    replacementReference <-
      registerPeer runtime (peerHandlers replacementDispositions replacementClosed)
    assertQueued "lower replacement peer Hello"
      =<< submitPeerHello
        runtime
        replacementReference
        replacementCandidate
        mempty
        fixtureRemoteHello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    replacementDisposition <- awaitChan "replacement peer acceptance" replacementDispositions
    replacementBinding <- case replacementDisposition of
      PeerHelloAccepted ReplacedBinding accepted -> pure accepted
      actual -> assertFailure ("expected replacement peer acceptance, got " <> show actual)

    assertBool "replacement advances the logical binding" (replacementBinding /= initialBinding)
    awaitClosed "replaced peer route" oldClosed
    assertEqual
      "the replaced peer lease is stale"
      StalePhysicalGeneration
      =<< closeRuntimeConnection runtime oldReference
    assertEqual
      "the replacement route remains current"
      ConnectionClosed
      =<< closeRuntimeConnection runtime replacementReference
    awaitClosed "replacement peer route close" replacementClosed
  assertEqual "peer replacement keeps the role live" (Right ((), HeraldRuntimeScopeClosed)) outcome

-- The old writer has already removed its disposition from the FIFO when the
-- replacement is admitted. Queueing a close marker at this point is too late:
-- only the synchronous retirement action can keep that callback from
-- resurrecting a transport-local row after the owner registry has detached it.
caseDequeuedPeerDispositionRetirement :: Assertion
caseDequeuedPeerDispositionRetirement = do
  target <- newEmptyMVar
  dispositionDequeued <- newEmptyMVar
  releaseDisposition <- newEmptyMVar
  retired <- newTVarIO False
  projectedRows <- newTVarIO []
  oldClosed <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookAfterWriterCommandDequeued = \physical -> do
              expected <- tryReadMVar target
              when (expected == Just physical) $ do
                putOnce dispositionDequeued ()
                readMVar releaseDisposition
          }
      replacementCandidate = fixturePeerCandidate
      initialNonce = connectionNonce 78
      initialCandidate =
        peerCandidate
          (peerCandidateInitiatorHeraldId replacementCandidate)
          (peerCandidateInitiatorHeraldEpoch replacementCandidate)
          initialNonce
      closeScope runtime = do
        putOnce releaseDisposition ()
        RuntimeInternal.runtimeCloseScope runtime
  started <- startHeraldRuntimeWithHooks hooks fixtureConfiguration
  case started of
    Left failure -> assertFailure ("runtime initialization failed: " <> show failure)
    Right runtime -> (`finally` closeScope runtime) $ do
      oldReference <-
        registerPeer
          runtime
          (retiringPeerHandlers projectedRows retired oldClosed)
      putMVar target (PhysicalPeerRef oldReference)
      assertQueued "initial paused larger peer Hello"
        =<< submitPeerHello
          runtime
          oldReference
          initialCandidate
          mempty
          (fixturePeerHello initialNonce)
          fixtureMembershipGenerationId
          fixtureMemberSetDigest
          Nothing
      _ <- awaitMVar "old peer disposition dequeue" dispositionDequeued

      replacementDispositions <- newChan
      replacementClosed <- newEmptyMVar
      replacementReference <-
        registerPeer runtime (peerHandlers replacementDispositions replacementClosed)
      assertQueued "replacement while old disposition is dequeued"
        =<< submitPeerHello
          runtime
          replacementReference
          replacementCandidate
          mempty
          fixtureRemoteHello
          fixtureMembershipGenerationId
          fixtureMemberSetDigest
          Nothing
      replacementDisposition <-
        awaitChan "replacement behind dequeued old disposition" replacementDispositions
      case replacementDisposition of
        PeerHelloAccepted ReplacedBinding _ -> pure ()
        actual ->
          assertFailure
            ("expected replacement peer acceptance, got " <> show actual)

      assertEqual
        "owner handoff synchronously retires the old transport projection"
        True
        =<< atomically (readTVar retired)
      assertEqual
        "retirement removes every old transport-local row"
        []
        =<< atomically (readTVar projectedRows)

      putOnce releaseDisposition ()
      awaitClosed "retired dequeued-disposition writer" oldClosed
      assertEqual
        "the released stale disposition cannot resurrect its old row"
        []
        =<< atomically (readTVar projectedRows)
      assertEqual
        "the replacement route remains current"
        ConnectionClosed
        =<< closeRuntimeConnection runtime replacementReference
      awaitClosed "dequeued-disposition replacement close" replacementClosed

caseOpenRetryHandoffFence :: Assertion
caseOpenRetryHandoffFence = do
  gate <- newTransferGate
  withTransferRuntime gate hasApplicationAcceptance $ \runtime -> do
    oldOutput <- newChan
    oldClosed <- newEmptyMVar
    oldReference <- registerApplication runtime (applicationHandlers oldOutput oldClosed)
    assertQueued "initial Open" =<< submitApplicationDto runtime oldReference fixtureOpenDto
    initial <- awaitAcceptance "initial Open acceptance" oldOutput

    armTransferGate gate
    replacementOutput <- newChan
    replacementClosed <- newEmptyMVar
    replacement <- registerApplication runtime (applicationHandlers replacementOutput replacementClosed)
    assertQueued "gated exact Open retry" =<< submitApplicationDto runtime replacement fixtureOpenDto
    _ <- awaitMVar "Open retry owner handoff" (transferEntered gate)
    assertEqual "the accepted old route closes behind the transfer reservation" ConnectionClosed
      =<< closeRuntimeConnection runtime oldReference
    awaitClosed "old Open route close" oldClosed
    releaseTransferGate gate

    retried <- awaitAcceptance "gated Open retry acceptance" replacementOutput
    assertEqual "the promoted retry retains the exact binding" (acceptedBinding initial) (acceptedBinding retried)
    assertEqual "the held old-route loss never reaches the owner" 0
      . applicationLossCount (acceptedBinding initial)
      =<< snapshotTransferTrace gate
    probeLiveBinding runtime replacement replacementOutput retried
    assertEqual "the replacement closes normally" ConnectionClosed
      =<< closeRuntimeConnection runtime replacement
    awaitClosed "replacement Open route close" replacementClosed

caseOpenRetryFailedTarget :: Assertion
caseOpenRetryFailedTarget = do
  gate <- newTransferGate
  withTransferRuntime gate hasApplicationAcceptance $ \runtime -> do
    oldOutput <- newChan
    oldClosed <- newEmptyMVar
    oldReference <- registerApplication runtime (applicationHandlers oldOutput oldClosed)
    assertQueued "initial Open" =<< submitApplicationDto runtime oldReference fixtureOpenDto
    initial <- awaitAcceptance "initial Open acceptance" oldOutput

    armTransferGate gate
    missingOutput <- newChan
    missingClosed <- newEmptyMVar
    missing <- registerApplication runtime (applicationHandlers missingOutput missingClosed)
    assertQueued "gated exact Open retry" =<< submitApplicationDto runtime missing fixtureOpenDto
    _ <- awaitMVar "failed Open retry owner handoff" (transferEntered gate)
    assertEqual "the accepted old route closes behind the reservation" ConnectionClosed
      =<< closeRuntimeConnection runtime oldReference
    assertEqual "the exact promotion target also disappears" ConnectionClosed
      =<< closeRuntimeConnection runtime missing
    awaitClosed "old Open route close" oldClosed
    awaitClosed "missing Open target close" missingClosed
    releaseTransferGate gate

    events <-
      awaitTransferTrace
        "failed Open target binding loss"
        gate
        ((>= 1) . applicationLossCount (acceptedBinding initial))
    assertEqual
      "failed promotion releases exactly one accepted-binding loss"
      1
      (applicationLossCount (acceptedBinding initial) events)

caseResumeRetryHandoffFence :: Assertion
caseResumeRetryHandoffFence = do
  gate <- newTransferGate
  withTransferRuntime gate hasApplicationAcceptance $ \runtime -> do
    openOutput <- newChan
    openClosed <- newEmptyMVar
    openReference <- registerApplication runtime (applicationHandlers openOutput openClosed)
    assertQueued "initial Open" =<< submitApplicationDto runtime openReference fixtureOpenDto
    initial <- awaitAcceptance "initial Open acceptance" openOutput
    resume <- resumeDto (acceptedDto initial)

    currentOutput <- newChan
    currentClosed <- newEmptyMVar
    current <- registerApplication runtime (applicationHandlers currentOutput currentClosed)
    assertQueued "first Resume" =<< submitApplicationDto runtime current resume
    firstResume <- awaitAcceptance "first Resume acceptance" currentOutput
    awaitClosed "pre-Resume route retired by first Resume" openClosed
    assertEqual "the pre-Resume route is already stale" StalePhysicalGeneration
      =<< closeRuntimeConnection runtime openReference

    armTransferGate gate
    replacementOutput <- newChan
    replacementClosed <- newEmptyMVar
    replacement <- registerApplication runtime (applicationHandlers replacementOutput replacementClosed)
    assertQueued "gated exact Resume retry" =<< submitApplicationDto runtime replacement resume
    _ <- awaitMVar "Resume retry owner handoff" (transferEntered gate)
    assertEqual "the current Resume route closes behind the transfer reservation" ConnectionClosed
      =<< closeRuntimeConnection runtime current
    awaitClosed "current Resume route close" currentClosed
    releaseTransferGate gate

    retried <- awaitAcceptance "gated Resume retry acceptance" replacementOutput
    assertEqual "the promoted Resume retry retains the exact binding" (acceptedBinding firstResume) (acceptedBinding retried)
    assertEqual "the held Resume-route loss never reaches the owner" 0
      . applicationLossCount (acceptedBinding firstResume)
      =<< snapshotTransferTrace gate
    probeLiveBinding runtime replacement replacementOutput retried
    assertEqual "the replacement Resume route closes normally" ConnectionClosed
      =<< closeRuntimeConnection runtime replacement
    awaitClosed "replacement Resume route close" replacementClosed

caseResumeRetryFailedTarget :: Assertion
caseResumeRetryFailedTarget = do
  gate <- newTransferGate
  withTransferRuntime gate hasApplicationAcceptance $ \runtime -> do
    openOutput <- newChan
    openClosed <- newEmptyMVar
    openReference <- registerApplication runtime (applicationHandlers openOutput openClosed)
    assertQueued "initial Open" =<< submitApplicationDto runtime openReference fixtureOpenDto
    initial <- awaitAcceptance "initial Open acceptance" openOutput
    resume <- resumeDto (acceptedDto initial)

    currentOutput <- newChan
    currentClosed <- newEmptyMVar
    current <- registerApplication runtime (applicationHandlers currentOutput currentClosed)
    assertQueued "first Resume" =<< submitApplicationDto runtime current resume
    firstResume <- awaitAcceptance "first Resume acceptance" currentOutput
    awaitClosed "pre-Resume route retired by first Resume" openClosed
    assertEqual "the pre-Resume route is already stale" StalePhysicalGeneration
      =<< closeRuntimeConnection runtime openReference

    armTransferGate gate
    missingOutput <- newChan
    missingClosed <- newEmptyMVar
    missing <- registerApplication runtime (applicationHandlers missingOutput missingClosed)
    assertQueued "gated exact Resume retry" =<< submitApplicationDto runtime missing resume
    _ <- awaitMVar "failed Resume retry owner handoff" (transferEntered gate)
    assertEqual "the current Resume route closes behind the reservation" ConnectionClosed
      =<< closeRuntimeConnection runtime current
    assertEqual "the exact Resume promotion target also disappears" ConnectionClosed
      =<< closeRuntimeConnection runtime missing
    awaitClosed "current Resume route close" currentClosed
    awaitClosed "missing Resume target close" missingClosed
    releaseTransferGate gate

    events <-
      awaitTransferTrace
        "failed Resume target binding loss"
        gate
        ((>= 1) . applicationLossCount (acceptedBinding firstResume))
    assertEqual
      "failed Resume promotion releases exactly one accepted-binding loss"
      1
      (applicationLossCount (acceptedBinding firstResume) events)

casePeerRetryHandoffFence :: Assertion
casePeerRetryHandoffFence = do
  gate <- newTransferGate
  withTransferRuntime gate hasPeerAcceptance $ \runtime -> do
    oldDispositions <- newChan
    oldClosed <- newEmptyMVar
    oldReference <- registerPeer runtime (peerHandlers oldDispositions oldClosed)
    assertQueued "initial peer Hello"
      =<< submitPeerHello
        runtime
        oldReference
        fixturePeerCandidate
        mempty
        fixtureRemoteHello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    initialBinding <- awaitAcceptedPeer "initial peer acceptance" oldDispositions

    armTransferGate gate
    replacementDispositions <- newChan
    replacementClosed <- newEmptyMVar
    replacement <- registerPeer runtime (peerHandlers replacementDispositions replacementClosed)
    assertQueued "gated equal peer Hello"
      =<< submitPeerHello
        runtime
        replacement
        fixturePeerCandidate
        mempty
        fixtureRemoteHello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    _ <- awaitMVar "equal-Hello owner handoff" (transferEntered gate)
    assertEqual "the old peer route closes behind the transfer reservation" ConnectionClosed
      =<< closeRuntimeConnection runtime oldReference
    awaitClosed "old peer route close" oldClosed
    releaseTransferGate gate

    retriedBinding <- awaitAcceptedPeer "gated equal-Hello acceptance" replacementDispositions
    assertEqual "the promoted peer retry retains the exact binding" initialBinding retriedBinding
    assertEqual "the held peer-route loss never reaches the owner" 0
      . peerLossCount initialBinding
      =<< snapshotTransferTrace gate
    assertEqual "the replacement peer route closes normally" ConnectionClosed
      =<< closeRuntimeConnection runtime replacement
    awaitClosed "replacement peer route close" replacementClosed

casePeerRetryFailedTarget :: Assertion
casePeerRetryFailedTarget = do
  gate <- newTransferGate
  withTransferRuntime gate hasPeerAcceptance $ \runtime -> do
    oldDispositions <- newChan
    oldClosed <- newEmptyMVar
    oldReference <- registerPeer runtime (peerHandlers oldDispositions oldClosed)
    assertQueued "initial peer Hello"
      =<< submitPeerHello
        runtime
        oldReference
        fixturePeerCandidate
        mempty
        fixtureRemoteHello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    initialBinding <- awaitAcceptedPeer "initial peer acceptance" oldDispositions

    armTransferGate gate
    missingDispositions <- newChan
    missingClosed <- newEmptyMVar
    missing <- registerPeer runtime (peerHandlers missingDispositions missingClosed)
    assertQueued "gated equal peer Hello"
      =<< submitPeerHello
        runtime
        missing
        fixturePeerCandidate
        mempty
        fixtureRemoteHello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    _ <- awaitMVar "failed equal-Hello owner handoff" (transferEntered gate)
    assertEqual "the old peer route closes behind the transfer reservation" ConnectionClosed
      =<< closeRuntimeConnection runtime oldReference
    assertEqual "the exact peer promotion target also disappears" ConnectionClosed
      =<< closeRuntimeConnection runtime missing
    awaitClosed "old peer route close" oldClosed
    awaitClosed "missing peer target close" missingClosed
    releaseTransferGate gate

    events <-
      awaitTransferTrace
        "failed peer target binding loss"
        gate
        ((>= 1) . peerLossCount initialBinding)
    assertEqual
      "failed peer promotion releases exactly one accepted-binding loss"
      1
      (peerLossCount initialBinding events)

-- The replacement decision has already changed the pure current binding when
-- its selected physical candidate disappears behind the batch handoff. The
-- old physical generation is therefore superseded even though it was not the
-- failed target, and must retire without contributing a second binding loss.
casePeerReplacementFailedTarget :: Assertion
casePeerReplacementFailedTarget = do
  gate <- newTransferGate
  let replacementCandidate = fixturePeerCandidate
      initialNonce = connectionNonce 78
      initialCandidate =
        peerCandidate
          (peerCandidateInitiatorHeraldId replacementCandidate)
          (peerCandidateInitiatorHeraldEpoch replacementCandidate)
          initialNonce
  withTransferRuntime gate hasPeerAcceptance $ \runtime -> do
    oldDispositions <- newChan
    oldClosed <- newEmptyMVar
    oldReference <- registerPeer runtime (peerHandlers oldDispositions oldClosed)
    assertQueued "initial larger peer Hello"
      =<< submitPeerHello
        runtime
        oldReference
        initialCandidate
        mempty
        (fixturePeerHello initialNonce)
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    initialBinding <- awaitAcceptedPeer "initial larger peer acceptance" oldDispositions

    armTransferGate gate
    missingDispositions <- newChan
    missingClosed <- newEmptyMVar
    missing <- registerPeer runtime (peerHandlers missingDispositions missingClosed)
    assertQueued "gated lower replacement peer Hello"
      =<< submitPeerHello
        runtime
        missing
        replacementCandidate
        mempty
        fixtureRemoteHello
        fixtureMembershipGenerationId
        fixtureMemberSetDigest
        Nothing
    acceptedBatch <-
      awaitMVar "failed replacement peer owner handoff" (transferEntered gate)
    replacementBinding <- acceptedReplacementBinding acceptedBatch
    assertBool
      "the gated decision advances the logical peer binding"
      (replacementBinding /= initialBinding)

    assertEqual
      "the selected replacement target disappears"
      ConnectionClosed
      =<< closeRuntimeConnection runtime missing
    awaitClosed "missing replacement peer target close" missingClosed
    releaseTransferGate gate

    awaitClosed "superseded remote-epoch route close" oldClosed
    assertEqual
      "the superseded remote-epoch lease is stale"
      StalePhysicalGeneration
      =<< closeRuntimeConnection runtime oldReference
    events <-
      awaitTransferTrace
        "failed replacement target binding loss"
        gate
        ((>= 1) . peerLossCount replacementBinding)
    assertEqual
      "failed replacement releases exactly one new-binding loss"
      1
      (peerLossCount replacementBinding events)
    assertEqual
      "superseded old binding contributes no semantic loss"
      0
      (peerLossCount initialBinding events)

acceptedReplacementBinding :: BatchEnvelope -> IO PeerBinding
acceptedReplacementBinding batch =
  case [ binding
       | SetPeerCandidateDisposition _ (PeerHelloAccepted ReplacedBinding binding) <-
           effectBatchMembers (batchEnvelopeEffects batch)
       ] of
    [binding] -> pure binding
    actual ->
      assertFailure
        ("expected one replacement peer acceptance, got " <> show actual)

data TransferGate = TransferGate
  { transferArmed :: MVar (),
    transferEntered :: MVar BatchEnvelope,
    transferRelease :: MVar (),
    transferLedger :: MVar TraceLedger
  }

newTransferGate :: IO TransferGate
newTransferGate =
  TransferGate
    <$> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar

armTransferGate :: TransferGate -> IO ()
armTransferGate gate = putMVar (transferArmed gate) ()

releaseTransferGate :: TransferGate -> IO ()
releaseTransferGate gate = putMVar (transferRelease gate) ()

transferHooks :: TransferGate -> (BatchEnvelope -> Bool) -> RuntimeHooks
transferHooks gate matches =
  defaultRuntimeHooks
    { hookRuntimeInitialized = \_ _ _ _ _ _ ledger -> putOnce (transferLedger gate) ledger,
      hookBeforeDispatchBatch = \batch -> do
        armed <- tryReadMVar (transferArmed gate)
        when (maybe False (const True) armed && matches batch) $ do
          first <- tryPutMVar (transferEntered gate) batch
          when first (readMVar (transferRelease gate))
    }

withTransferRuntime ::
  TransferGate ->
  (BatchEnvelope -> Bool) ->
  (HeraldRuntime -> IO ()) ->
  Assertion
withTransferRuntime gate matches use = do
  started <- startHeraldRuntimeWithHooks (transferHooks gate matches) fixtureConfiguration
  case started of
    Left failure -> assertFailure ("runtime initialization failed: " <> show failure)
    Right runtime -> use runtime `finally` RuntimeInternal.runtimeCloseScope runtime

hasApplicationAcceptance :: BatchEnvelope -> Bool
hasApplicationAcceptance = any isAcceptance . effectBatchMembers . batchEnvelopeEffects
  where
    isAcceptance SetApplicationConnectionDisposition {} = True
    isAcceptance _ = False

hasPeerAcceptance :: BatchEnvelope -> Bool
hasPeerAcceptance = any isAcceptance . effectBatchMembers . batchEnvelopeEffects
  where
    isAcceptance SetPeerCandidateDisposition {} = True
    isAcceptance _ = False

snapshotTransferTrace :: TransferGate -> IO [RuntimeTraceEvent]
snapshotTransferTrace gate = do
  ledger <- readMVar (transferLedger gate)
  atomically (snapshotTraceLedger ledger)

awaitTransferTrace ::
  String ->
  TransferGate ->
  ([RuntimeTraceEvent] -> Bool) ->
  IO [RuntimeTraceEvent]
awaitTransferTrace context gate ready = do
  ledger <- readMVar (transferLedger gate)
  observed <-
    timeout 2000000 . atomically $ do
      events <- snapshotTraceLedger ledger
      check (ready events)
      pure events
  case observed of
    Just events -> pure events
    Nothing -> assertFailure (context <> " timed out")

applicationLossCount :: ApplicationSessionBinding -> [RuntimeTraceEvent] -> Int
applicationLossCount expected events =
  length
    [ ()
    | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
      RuntimeObserved (ApplicationBindingLost actual) <- [inputBody input],
      actual == expected
    ]

peerLossCount :: PeerBinding -> [RuntimeTraceEvent] -> Int
peerLossCount expected events =
  length
    [ ()
    | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
      RuntimeObserved (PeerBindingLost actual) <- [inputBody input],
      actual == expected
    ]

data AcceptedApplication = AcceptedApplication
  { acceptedBinding :: ApplicationSessionBinding,
    acceptedDto :: ApplicationServerDto
  }
  deriving stock (Eq, Show)

applicationHandlers :: Chan ApplicationOutbound -> MVar () -> RuntimeApplicationConnectionHandlers
applicationHandlers output closed =
  runtimeApplicationConnectionHandlers
    (\outbound -> writeChan output outbound >> pure LaneOffered)
    (putOnce closed ())

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

peerHandlers :: Chan PeerHelloDisposition -> MVar () -> RuntimePeerConnectionHandlers
peerHandlers dispositions closed =
  runtimePeerConnectionHandlers
    (\_ _ -> pure LaneOffered)
    (\_ disposition -> writeChan dispositions disposition >> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ _ -> pure PeerDispatchWritten)
    (putOnce closed ())

retiringPeerHandlers ::
  TVar [PeerHelloDisposition] ->
  TVar Bool ->
  MVar () ->
  RuntimePeerConnectionHandlers
retiringPeerHandlers projectedRows retired closed =
  runtimePeerConnectionHandlersWithRetirement
    (\_ _ -> pure LaneOffered)
    ( \_ disposition ->
        atomically $ do
          stale <- readTVar retired
          if stale
            then pure LaneLost
            else do
              modifyTVar' projectedRows (disposition :)
              pure LaneOffered
    )
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ _ -> pure PeerDispatchWritten)
    (writeTVar retired True >> writeTVar projectedRows [])
    (putOnce closed ())

awaitAcceptedPeer :: String -> Chan PeerHelloDisposition -> IO PeerBinding
awaitAcceptedPeer context dispositions = do
  disposition <- awaitChan context dispositions
  case disposition of
    PeerHelloAccepted _ binding -> pure binding
    PeerHelloRejected problem -> assertFailure (context <> ": peer Hello rejected: " <> show problem)

fixtureRemoteHello :: PeerHello
fixtureRemoteHello = fixturePeerHello (connectionNonce 77)

awaitAcceptance :: String -> Chan ApplicationOutbound -> IO AcceptedApplication
awaitAcceptance context output = do
  outbound <- awaitChan context output
  case outbound of
    AcceptApplicationCandidate _ binding dto -> pure (AcceptedApplication binding dto)
    _ -> assertFailure (context <> ": expected candidate acceptance, got " <> show outbound)

resumeDto :: ApplicationServerDto -> IO ApplicationClientDto
resumeDto = \case
  SessionOpened cursor session token _ -> pure (ResumeSession session token cursor)
  dto -> assertFailure ("expected SessionOpened, got " <> show dto)

assertSessionResumed :: ApplicationServerDto -> IO ()
assertSessionResumed = \case
  SessionResumed {} -> pure ()
  dto -> assertFailure ("expected SessionResumed, got " <> show dto)

probeLiveBinding ::
  HeraldRuntime ->
  ConnectionRef ApplicationPlane ->
  Chan ApplicationOutbound ->
  AcceptedApplication ->
  IO ()
probeLiveBinding runtime reference output accepted = do
  session <- sessionClaim (acceptedDto accepted)
  let request = applicationRequestIdClaim 991
  assertQueued "live-binding probe" =<< submitApplicationDto runtime reference (GetRequestResult session request)
  outbound <- awaitChan "live-binding probe reply" output
  case outbound of
    SendEstablishedApplication binding (RequestAbsent actualSession actualRequest) -> do
      assertEqual "the rehomed binding receives the probe" (acceptedBinding accepted) binding
      assertEqual "the reply retains the session claim" session actualSession
      assertEqual "the reply retains the request claim" request actualRequest
    _ -> assertFailure ("expected RequestAbsent on the rehomed binding, got " <> show outbound)

sessionClaim :: ApplicationServerDto -> IO ApplicationSessionClaim
sessionClaim = \case
  SessionOpened _ session _ _ -> pure session
  SessionResumed _ session _ -> pure session
  dto -> assertFailure ("expected session acceptance DTO, got " <> show dto)

assertQueued :: String -> RuntimeSubmission -> IO ()
assertQueued context = assertEqual (context <> " submission") Queued

awaitClosed :: String -> MVar () -> IO ()
awaitClosed context closed = do
  _ <- awaitMVar context closed
  pure ()

putOnce :: MVar value -> value -> IO ()
putOnce cell value = do
  _ <- tryPutMVar cell value
  pure ()

awaitChan :: String -> Chan value -> IO value
awaitChan context channel = do
  result <- timeout 2000000 (readChan channel)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitMVar :: String -> MVar value -> IO value
awaitMVar context cell = do
  result <- timeout 2000000 (takeMVar cell)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

fixtureConfiguration :: HeraldRuntimeConfiguration
fixtureConfiguration =
  heraldRuntimeConfiguration
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeedSource
    (runtimeMonotonicClock (pure (monotonicInstant 100)))
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration
