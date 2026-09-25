module ConnectionProperties
  ( tests,
  )
where

import Control.Concurrent.STM (atomically)
import Data.Unique (newUnique)
import Eclips.Herald.Application.RPC
  ( ApplicationIngressContext (CandidateApplicationIngress),
    ApplicationOutbound (AcceptApplicationCandidate),
    admitApplicationClientDto,
    projectApplicationEffect,
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    ApplicationSessionId,
    sessionBindingSessionId,
  )
import Eclips.Herald.EffectBatch (effectBatchMembers)
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input (candidateApplicationLane, heraldInput)
import Eclips.Herald.PeerDispatch (PeerDispatchOutcome (..))
import Eclips.Herald.Runtime.Handler
  ( HeraldRuntimeHandlers,
    RuntimeAdministrationConnectionHandlers,
    RuntimeApplicationConnectionHandlers,
    RuntimeLaneOffer (..),
    RuntimePeerConnectionHandlers,
    heraldRuntimeHandlers,
    runtimeAdministrationConnectionHandlers,
    runtimeApplicationConnectionHandlers,
    runtimePeerConnectionHandlers,
  )
import Eclips.Herald.Runtime.Internal.Connection
  ( ApplicationPhase (..),
    ConnectionSlot (..),
    Registry,
    allocateAdministration,
    allocateApplication,
    allocatePeer,
    claimApplicationSessionDisposal,
    findApplicationBinding,
    findApplicationCandidate,
    findApplicationSession,
    lookupCurrentSlot,
    newRegistry,
    registrySlots,
    removeCurrentSlot,
    replaceSlot,
    slotSource,
  )
import Eclips.Herald.Runtime.Internal.Types
  ( RuntimeToken (..),
    connectionRefOrdinal,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Protocol.Application.Types qualified as Protocol
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureOpenDto,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
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
    "physical connection registry"
    [ testCase "allocation is shared, monotone, and role separated" caseAllocation,
      testCase "a foreign runtime reference never aliases a local ordinal" caseRuntimeTokenIsolation,
      testCase "close is exact and idempotent" caseExactClose,
      testCase "administration registration is one-shot even after close" caseAdministrationOneShot,
      testCase "phase replacement preserves physical identity and routing" casePhaseReplacement,
      testCase "deferred session disposition cannot retarget after physical close" caseDeferredDisposition,
      testCase "session routing follows the current binding generation" caseCurrentSessionRoute
    ]

caseAllocation :: Assertion
caseAllocation = do
  registry <- registryFixture
  (applicationRef, applicationSlot) <- atomically (allocateApplication registry applicationHandlers)
  administration <- atomically (allocateAdministration registry administrationHandlers)
  (peerRef, peerSlot) <- atomically (allocatePeer registry peerHandlers)
  administrationRef <- case administration of
    Just (reference, _) -> pure reference
    Nothing -> assertFailure "first administration allocation was unavailable"
  assertEqual
    "one ordinal namespace prevents cross-plane physical aliasing"
    [0, 1, 2]
    [ connectionRefOrdinal applicationRef,
      connectionRefOrdinal administrationRef,
      connectionRefOrdinal peerRef
    ]
  assertBool "application source differs from peer source" (slotSource applicationSlot /= slotSource peerSlot)

caseRuntimeTokenIsolation :: Assertion
caseRuntimeTokenIsolation = do
  left <- registryFixture
  right <- registryFixture
  (leftRef, _) <- atomically (allocateApplication left applicationHandlers)
  (rightRef, _) <- atomically (allocateApplication right applicationHandlers)
  foreignLookup <- atomically (lookupCurrentSlot right leftRef)
  localLookup <- atomically (lookupCurrentSlot right rightRef)
  assertEqual "the fixture deliberately shares a local ordinal" (connectionRefOrdinal leftRef) (connectionRefOrdinal rightRef)
  assertBool "a foreign reference is absent" (isNothing foreignLookup)
  assertBool "the local reference remains current" (isJust localLookup)

caseExactClose :: Assertion
caseExactClose = do
  registry <- registryFixture
  (firstRef, _) <- atomically (allocateApplication registry applicationHandlers)
  (_, _) <- atomically (allocatePeer registry peerHandlers)
  firstClose <- atomically (removeCurrentSlot registry firstRef)
  secondClose <- atomically (removeCurrentSlot registry firstRef)
  remaining <- atomically (registrySlots registry)
  assertBool "the current reference closes once" (isJust firstClose)
  assertBool "a repeated close is stale" (isNothing secondClose)
  assertEqual "the unrelated slot survives" 1 (length remaining)

caseAdministrationOneShot :: Assertion
caseAdministrationOneShot = do
  registry <- registryFixture
  first <- atomically (allocateAdministration registry administrationHandlers)
  firstRef <- case first of
    Just (reference, _) -> pure reference
    Nothing -> assertFailure "first administration allocation was unavailable"
  _ <- atomically (removeCurrentSlot registry firstRef)
  second <- atomically (allocateAdministration registry administrationHandlers)
  assertBool "loss cannot invent pure-owner reactivation" (isNothing second)

casePhaseReplacement :: Assertion
casePhaseReplacement = do
  registry <- registryFixture
  (reference, slot) <- atomically (allocateApplication registry applicationHandlers)
  candidate <- case slot of
    ApplicationSlot _ (ApplicationCandidate value) _ _ -> pure value
    _ -> assertFailure "new application slot was not a candidate"
  let pending = case slot of
        ApplicationSlot current _ handlers queue ->
          ApplicationSlot current (ApplicationDispositionPending candidate) handlers queue
        _ -> error "test fixture unexpectedly changed slot role"
  atomically (replaceSlot registry pending)
  byCandidate <- atomically (findApplicationCandidate registry candidate)
  byReference <- atomically (lookupCurrentSlot registry reference)
  assertBool "the pending candidate remains routable by correlation" (isJust byCandidate)
  assertBool "the exact physical reference remains current" (isJust byReference)
  let claimPending = case slot of
        ApplicationSlot current _ handlers queue -> ApplicationSlot current (ApplicationClaimPending candidate) handlers queue
        _ -> error "application allocation changed role"
  atomically (replaceSlot registry claimPending)
  retainedClaim <- atomically (findApplicationCandidate registry candidate)
  assertBool "pending initial claim is routable after its ingress batch" (isJust retainedClaim)
  _ <- atomically (removeCurrentSlot registry reference)
  lostClaim <- atomically (findApplicationCandidate registry candidate)
  assertBool "closed pending claim leaves no deferred acceptance route" (isNothing lostClaim)

caseDeferredDisposition :: Assertion
caseDeferredDisposition = do
  registry <- registryFixture
  (reference, slot) <- atomically (allocateApplication registry applicationHandlers)
  candidate <- case slot of
    ApplicationSlot _ (ApplicationCandidate value) _ _ -> pure value
    _ -> assertFailure "new application slot was not a candidate"
  let deferred = case slot of
        ApplicationSlot current _ handlers queue -> ApplicationSlot current (ApplicationAdmissionDeferred candidate) handlers queue
        _ -> error "application allocation changed role"
  atomically (replaceSlot registry deferred)
  retained <- atomically (findApplicationCandidate registry candidate)
  assertBool "deferred correlation retains its exact physical slot" (isJust retained)
  _ <- atomically (removeCurrentSlot registry reference)
  (replacement, _) <- atomically (allocateApplication registry applicationHandlers)
  old <- atomically (findApplicationCandidate registry candidate)
  fresh <- atomically (lookupCurrentSlot registry replacement)
  assertBool "closed candidate cannot be rediscovered as the replacement" (isNothing old)
  assertBool "unrelated replacement remains live" (isJust fresh)

caseCurrentSessionRoute :: Assertion
caseCurrentSessionRoute = do
  let (staleBinding, currentBinding, session) = reboundSessionFixture
  registry <- registryFixture
  (reference, slot) <- atomically (allocateApplication registry applicationHandlers)
  let established = case slot of
        ApplicationSlot current _ handlers queue ->
          ApplicationSlot current (ApplicationEstablished currentBinding) handlers queue
        _ -> error "application allocation changed role"
  atomically (replaceSlot registry established)
  staleRoute <- atomically (findApplicationBinding registry staleBinding)
  sessionRoute <- atomically (findApplicationSession registry session)
  assertBool "the superseded binding is not routable" (isNothing staleRoute)
  case sessionRoute of
    Just (ApplicationSlot actual (ApplicationEstablished actualBinding) _ _) -> do
      assertEqual "the session names the replacement physical route" reference actual
      assertEqual "the session route carries the current generation" currentBinding actualBinding
    other -> assertFailure ("the current session route was absent: " <> show other)
  claimed <- atomically (claimApplicationSessionDisposal registry session)
  assertBool "the current session route is claimed once" (isJust claimed)
  afterClosing <- atomically (findApplicationSession registry session)
  assertBool "an already-closing session settles as an absent route" (isNothing afterClosing)
  repeated <- atomically (claimApplicationSessionDisposal registry session)
  assertBool "a repeated terminal claim is idempotently absent" (isNothing repeated)
  current <- atomically (lookupCurrentSlot registry reference)
  case current of
    Just (ApplicationSlot _ (ApplicationClosing Nothing) _ _) -> pure ()
    other -> assertFailure ("terminal claim retained loss-producing attribution: " <> show other)

reboundSessionFixture ::
  (ApplicationSessionBinding, ApplicationSessionBinding, ApplicationSessionId)
reboundSessionFixture =
  case initialHerald
    (monotonicInstant 100)
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeed
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration of
    Left fault -> error ("session-route initialization failed: " <> show fault)
    Right (initialState, _) ->
      let openBody =
            admitted
              (CandidateApplicationIngress (candidateApplicationLane 700))
              fixtureOpenDto
       in case stepHerald (heraldInput (monotonicInstant 101) openBody) initialState of
            Left fault -> error ("session-route Open failed: " <> show fault)
            Right (openedState, openedEffects) -> case projectedAcceptances openedEffects of
              [(firstBinding, Protocol.SessionOpened cursor session token _)] ->
                let resumeBody =
                      admitted
                        (CandidateApplicationIngress (candidateApplicationLane 701))
                        (Protocol.ResumeSession session token cursor)
                 in case stepHerald (heraldInput (monotonicInstant 102) resumeBody) openedState of
                      Left fault -> error ("session-route Resume failed: " <> show fault)
                      Right (_, resumedEffects) -> case projectedAcceptances resumedEffects of
                        [(currentBinding, Protocol.SessionResumed _ resumedSession _)]
                          | resumedSession == session,
                            sessionBindingSessionId currentBinding
                              == sessionBindingSessionId firstBinding ->
                              ( firstBinding,
                                currentBinding,
                                sessionBindingSessionId firstBinding
                              )
                        unexpected -> error ("session-route Resume effects were unexpected: " <> show unexpected)
              unexpected -> error ("session-route Open effects were unexpected: " <> show unexpected)
  where
    admitted context dto = case admitApplicationClientDto context dto of
      Left problem -> error ("session-route DTO admission failed: " <> show problem)
      Right body -> body
    projectedAcceptances effects =
      [ (binding, dto)
      | effect <- effectBatchMembers effects,
        Right (Just (AcceptApplicationCandidate _ binding dto)) <-
          [projectApplicationEffect effect]
      ]

quietHandlers :: HeraldRuntimeHandlers
quietHandlers = heraldRuntimeHandlers (const (pure ()))

applicationHandlers :: RuntimeApplicationConnectionHandlers
applicationHandlers = runtimeApplicationConnectionHandlers (const (pure LaneOffered)) (pure ())

administrationHandlers :: RuntimeAdministrationConnectionHandlers
administrationHandlers = runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ())

peerHandlers :: RuntimePeerConnectionHandlers
peerHandlers =
  runtimePeerConnectionHandlers
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ -> pure LaneOffered)
    (\_ _ _ -> pure PeerDispatchWritten)
    (pure ())

registryFixture :: IO Registry
registryFixture = do
  token <- RuntimeToken <$> newUnique
  atomically (newRegistry token quietHandlers)

isJust :: Maybe value -> Bool
isJust = \case
  Just _ -> True
  Nothing -> False

isNothing :: Maybe value -> Bool
isNothing = not . isJust
