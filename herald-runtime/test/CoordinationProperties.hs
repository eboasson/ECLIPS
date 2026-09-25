module CoordinationProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( mkHeraldEpoch,
    mkHeraldId,
  )
import Eclips.Herald.Application.Session (ApplicationSessionRejection (..))
import Eclips.Herald.Discovery
  ( HelloRejection (..),
    PeerCandidate,
    PeerHelloDisposition (..),
    connectionNonce,
    peerCandidate,
    peerCandidateOpened,
  )
import Eclips.Herald.EffectBatch
  ( ApplicationDispositionTarget (..),
    HeraldEffect (..),
    orderedEffectBatch,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (..),
    PeerIngress (..),
    candidateApplicationLane,
  )
import Eclips.Herald.PeerDispatch (PeerDispatchOutcome (..))
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchOrdinal (..),
    CompletionOrdinal (..),
    DispositionExpectation (..),
    DrainLedger,
    EffectMemberOrdinal (..),
    ObligationOrigin (..),
    ObligationState (..),
    OutcomeCellState (..),
    advanceRoutedThrough,
    awaitPhysicalOutcome,
    claimOutcomeCell,
    dispositionExpectationFor,
    drainBarrierReady,
    drainLedgerObligations,
    effectMatchesDisposition,
    emptyDrainLedger,
    markObligationKernelStepped,
    outstandingPhysicalOutcomes,
    queueObligationObservation,
    registerObligation,
    sealDrainLedger,
    settleObligationByOffer,
    validateDispositionBatch,
  )
import RuntimeFixtures
  ( fixtureMemberSetDigest,
    fixtureMembershipGenerationId,
    fixturePeerCandidate,
    fixturePeerHello,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    listOf1,
    shuffle,
    testProperty,
    vectorOf,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "batch and drain coordination"
    [ testGroup
        "paused-source dispositions"
        [ testCase "exactly one application disposition discharges its exact candidate" caseApplicationDisposition,
          testProperty "deferred applications and rejected local peer opens retain exact route identity" propRestrictedDispositions,
          testCase "local-open and remote-Hello routes are distinct and exact" caseLocalAndRemotePeerRoutes,
          testCase "peer disposition matching is exact by candidate" casePeerDisposition,
          testCase "missing, duplicated, and mismatched dispositions are rejected" caseDispositionCardinality
        ],
      testGroup
        "physical outcome cells"
        [ testProperty "the first outcome wins and every later racer is suppressed" propFirstOutcomeWins,
          testCase "drain can claim an outstanding outcome as Deferred" caseDrainClaimsDeferred,
          testCase "sealing finds exactly the physical outcomes that still need Deferred" caseOutstandingAtSeal
        ],
      testGroup
        "drain obligation cut"
        [ testCase "registration is idempotent and cannot reopen a released obligation" caseObligationRegistration,
          testProperty "a pending obligation cannot pin completed routing history" propBoundedObligationRetention,
          testProperty "reordered transfer and physical settlements preserve the exact drain cut" propReorderedObligationSettlement,
          testCase "barrier readiness requires route, completion, and every settlement" caseBarrierConjunction,
          testCase "queued observations become ready only after their kernel step" caseObservationSettlement,
          testProperty "generated obligation schedules agree with the cut model" propGeneratedDrainModel
        ]
    ]

caseApplicationDisposition :: Assertion
caseApplicationDisposition = do
  let candidate = candidateApplicationLane 17
      other = candidateApplicationLane 18
      expectation = ExpectApplicationDisposition candidate
      matching = RejectApplicationConnection (CandidateApplicationDisposition candidate) ApplicationAttachmentNotAdmitted
      mismatching = RejectApplicationConnection (CandidateApplicationDisposition other) ApplicationAttachmentNotAdmitted
  assertBool "the exact target matches" (effectMatchesDisposition expectation matching)
  assertBool "a different candidate cannot resume the source" (not (effectMatchesDisposition expectation mismatching))
  assertBool "one exact disposition validates" (validateDispositionBatch expectation (orderedEffectBatch [matching]))

propRestrictedDispositions :: Word64 -> Property
propRestrictedDispositions ordinal =
  let application = candidateApplicationLane ordinal
      peer = connectionNonce ordinal
      otherApplication = candidateApplicationLane (ordinal + 1)
      otherPeer = connectionNonce (ordinal + 1)
      deferred = DeferApplicationSession application
      rejected = RejectPeerCandidateOpened peer
   in ( validateDispositionBatch (ExpectApplicationDisposition application) (orderedEffectBatch [deferred])
          && not (effectMatchesDisposition (ExpectApplicationDisposition otherApplication) deferred)
          && validateDispositionBatch (ExpectPeerCandidateRoute peer) (orderedEffectBatch [rejected])
          && not (effectMatchesDisposition (ExpectPeerCandidateRoute otherPeer) rejected)
          && not (effectMatchesDisposition (ExpectPeerDisposition fixturePeerCandidate) rejected)
      )
        === True

caseLocalAndRemotePeerRoutes :: Assertion
caseLocalAndRemotePeerRoutes = do
  let nonce = connectionNonce 77
      localExpectation = ExpectPeerCandidateRoute nonce
      remoteExpectation = ExpectPeerDisposition fixturePeerCandidate
      localBody = PeerInput (PeerCandidateOpened (peerCandidateOpened nonce Set.empty Nothing))
      remoteBody =
        PeerInput
          ( PeerHelloReceived
              fixturePeerCandidate
              Set.empty
              (fixturePeerHello nonce)
              fixtureMembershipGenerationId
              fixtureMemberSetDigest
              Nothing
          )
      localEffect =
        SendPeerCandidate
          fixturePeerCandidate
          fixtureMembershipGenerationId
          fixtureMemberSetDigest
          (fixturePeerHello nonce)
      remoteEffect = SetPeerCandidateDisposition fixturePeerCandidate (PeerHelloRejected HelloInactiveHerald)
  assertEqual "local open waits for its nonce-authored Hello route" (Just localExpectation) (dispositionExpectationFor localBody)
  assertEqual "remote Hello waits for its candidate disposition" (Just remoteExpectation) (dispositionExpectationFor remoteBody)
  assertBool "local route matches the exact Hello nonce" (effectMatchesDisposition localExpectation localEffect)
  assertBool "local route cannot be discharged by peer disposition" (not (effectMatchesDisposition localExpectation remoteEffect))
  assertBool "remote route matches the exact candidate" (effectMatchesDisposition remoteExpectation remoteEffect)
  assertBool "remote route cannot be discharged by candidate send" (not (effectMatchesDisposition remoteExpectation localEffect))

casePeerDisposition :: Assertion
casePeerDisposition = do
  let candidate = fixtureCandidate 1
      other = fixtureCandidate 2
      expectation = ExpectPeerDisposition candidate
      matching = SetPeerCandidateDisposition candidate (PeerHelloRejected HelloInactiveHerald)
      mismatching = SetPeerCandidateDisposition other (PeerHelloRejected HelloInactiveHerald)
  assertBool "the exact candidate matches" (effectMatchesDisposition expectation matching)
  assertBool "a different candidate cannot resume the source" (not (effectMatchesDisposition expectation mismatching))

caseDispositionCardinality :: Assertion
caseDispositionCardinality = do
  let candidate = candidateApplicationLane 21
      other = candidateApplicationLane 22
      expectation = ExpectApplicationDisposition candidate
      matching = RejectApplicationConnection (CandidateApplicationDisposition candidate) ApplicationAttachmentNotAdmitted
      mismatching = RejectApplicationConnection (CandidateApplicationDisposition other) ApplicationAttachmentNotAdmitted
  assertBool "missing disposition" (not (validateDispositionBatch expectation (orderedEffectBatch [])))
  assertBool "mismatched disposition" (not (validateDispositionBatch expectation (orderedEffectBatch [mismatching])))
  assertBool "duplicate disposition" (not (validateDispositionBatch expectation (orderedEffectBatch [matching, matching])))

propFirstOutcomeWins :: Property
propFirstOutcomeWins =
  forAll (listOf1 genOutcome) $ \outcomes ->
    case outcomes of
      first : rest ->
        let claims = scanClaims outcomes
            expected = OutcomeCellClaimed first
         in (fmap fst claims, finalCell claims) === (True : replicate (length rest) False, expected)
      [] -> error "listOf1 generated an empty list"

caseDrainClaimsDeferred :: Assertion
caseDrainClaimsDeferred = do
  assertEqual
    "drain wins an unclaimed attempt"
    (True, OutcomeCellClaimed PeerDispatchDeferred)
    (claimOutcomeCell PeerDispatchDeferred OutcomeCellEmpty)
  assertEqual
    "drain cannot overwrite a physical winner"
    (False, OutcomeCellClaimed PeerDispatchWritten)
    (claimOutcomeCell PeerDispatchDeferred (OutcomeCellClaimed PeerDispatchWritten))

caseOutstandingAtSeal :: Assertion
caseOutstandingAtSeal = do
  let offered = fixtureOrigin 2 0
      waiting = fixtureOrigin 2 1
      queued = fixtureOrigin 2 2
      ledger =
        queueObligationObservation queued (CompletionOrdinal 4)
          . awaitPhysicalOutcome waiting
          . settleObligationByOffer offered
          . registerObligation queued
          . registerObligation waiting
          . registerObligation offered
          $ emptyDrainLedger
  assertEqual
    "only the handed physical attempt remains claimable"
    [waiting]
    (outstandingPhysicalOutcomes ledger)

caseObligationRegistration :: Assertion
caseObligationRegistration = do
  let origin = fixtureOrigin 0 2
      settled = settleObligationByOffer origin (registerObligation origin emptyDrainLedger)
      registeredAgain = registerObligation origin settled
  assertEqual
    "duplicate discovery leaves settled storage reclaimed"
    Nothing
    (Map.lookup origin (drainLedgerObligations registeredAgain))

caseBarrierConjunction :: Assertion
caseBarrierConjunction = do
  let first = fixtureOrigin 3 0
      second = fixtureOrigin 3 1
      registered = registerObligation second (registerObligation first emptyDrainLedger)
      physicallyWaiting = awaitPhysicalOutcome second (settleObligationByOffer first registered)
      routedShort = advanceRoutedThrough (BatchOrdinal 2) physicallyWaiting
      routed = advanceRoutedThrough (BatchOrdinal 3) physicallyWaiting
      settled = markObligationKernelStepped second routed
      sealed = sealDrainLedger (BatchOrdinal 3) (CompletionOrdinal 5) settled
  assertBool "an unsealed ledger cannot pass" (not (drainBarrierReady (CompletionOrdinal 5) settled))
  assertBool
    "routing short of the drain batch cannot pass"
    ( not
        ( drainBarrierReady
            (CompletionOrdinal 5)
            (sealDrainLedger (BatchOrdinal 3) (CompletionOrdinal 5) routedShort)
        )
    )
  assertBool
    "a physical outcome still awaited cannot pass"
    ( not
        ( drainBarrierReady
            (CompletionOrdinal 5)
            (sealDrainLedger (BatchOrdinal 3) (CompletionOrdinal 5) routed)
        )
    )
  assertBool "completion short of the cut cannot pass" (not (drainBarrierReady (CompletionOrdinal 4) sealed))
  assertBool "all four conditions together pass" (drainBarrierReady (CompletionOrdinal 5) sealed)

caseObservationSettlement :: Assertion
caseObservationSettlement = do
  let origin = fixtureOrigin 8 4
      queued =
        queueObligationObservation origin (CompletionOrdinal 9)
          . registerObligation origin
          $ emptyDrainLedger
      sealed =
        sealDrainLedger (BatchOrdinal 8) (CompletionOrdinal 9)
          . advanceRoutedThrough (BatchOrdinal 8)
          $ queued
      stepped = markObligationKernelStepped origin sealed
  assertBool "merely queued is not semantically settled" (not (drainBarrierReady (CompletionOrdinal 9) sealed))
  assertBool "the exact successor handoff settles it" (drainBarrierReady (CompletionOrdinal 9) stepped)

data GeneratedObligationState
  = GeneratedOffered
  | GeneratedAwaiting
  | GeneratedQueued
  | GeneratedStepped
  deriving stock (Eq, Show, Enum, Bounded)

data DrainScenario = DrainScenario
  { scenarioStates :: [GeneratedObligationState],
    scenarioCut :: Word64,
    scenarioRoutedThrough :: Word64,
    scenarioCompletionCut :: Word64,
    scenarioSteppedThrough :: Word64
  }
  deriving stock (Show)

propGeneratedDrainModel :: Property
propGeneratedDrainModel =
  forAll genDrainScenario $ \scenario ->
    let states = scenarioStates scenario
        cut = scenarioCut scenario
        routedThrough = scenarioRoutedThrough scenario
        completionCut = scenarioCompletionCut scenario
        steppedThrough = scenarioSteppedThrough scenario
        origins = zipWith (\member _ -> fixtureOrigin cut member) [0 ..] states
        registered = foldl' (flip registerObligation) emptyDrainLedger origins
        transitioned =
          foldl'
            (\ledger (origin, state) -> applyGeneratedState state origin ledger)
            registered
            (zip origins states)
        routed = advanceRoutedThrough (BatchOrdinal routedThrough) transitioned
        sealed = sealDrainLedger (BatchOrdinal cut) (CompletionOrdinal completionCut) routed
        expectedStates = Map.filter (`notElem` [SettledByOffer, KernelStepped]) (Map.fromList (zip origins (fmap generatedObligationState states)))
        expectedOutstanding =
          [ origin
          | (origin, state) <- zip origins states,
            state == GeneratedAwaiting
          ]
        expectedReady =
          routedThrough >= cut
            && steppedThrough >= completionCut
            && all (`elem` [GeneratedOffered, GeneratedStepped]) states
     in ( drainLedgerObligations sealed,
          outstandingPhysicalOutcomes sealed,
          drainBarrierReady (CompletionOrdinal steppedThrough) sealed
        )
          === (expectedStates, expectedOutstanding, expectedReady)

-- One origin stays unresolved while arbitrarily many later effects finish.
-- The routing and registration cursors carry only constant-size history.
propBoundedObligationRetention :: Property
propBoundedObligationRetention =
  forAll (chooseInt (1, 500)) $ \count ->
    let pending = fixtureOrigin 1 0
        initial = awaitPhysicalOutcome pending (registerObligation pending emptyDrainLedger)
        cycleOne ledger index =
          let origin = fixtureOrigin (fromIntegral index + 2) 0
              registered = registerObligation origin ledger
              completed =
                if even index
                  then settleObligationByOffer origin registered
                  else markObligationKernelStepped origin (queueObligationObservation origin (CompletionOrdinal (fromIntegral index + 1)) registered)
              -- Every late callback shape is harmless once that origin is gone.
              replayed =
                markObligationKernelStepped origin
                  . queueObligationObservation origin (CompletionOrdinal (fromIntegral index + 1))
                  . awaitPhysicalOutcome origin
                  . settleObligationByOffer origin
                  . registerObligation origin
                  $ completed
           in advanceRoutedThrough (BatchOrdinal (fromIntegral index + 2)) replayed
        histories = scanl cycleOne initial [0 .. count - 1]
        retained = foldl' cycleOne initial [0 .. count - 1]
        cut = BatchOrdinal (fromIntegral count + 1)
        sealed = sealDrainLedger cut (CompletionOrdinal (fromIntegral count)) retained
        queued = queueObligationObservation pending (CompletionOrdinal (fromIntegral count)) sealed
        finished = markObligationKernelStepped pending queued
     in conjoin
          [ counterexample "completed tail retained cells" (all ((== 1) . Map.size . drainLedgerObligations) histories),
            counterexample "physical head crossed the drain barrier" (not (drainBarrierReady (CompletionOrdinal (fromIntegral count)) sealed)),
            counterexample "queued observation crossed before kernel settlement" (not (drainBarrierReady (CompletionOrdinal (fromIntegral count)) queued)),
            drainLedgerObligations finished === Map.empty,
            counterexample "last semantic settlement did not release the exact drain cut" (drainBarrierReady (CompletionOrdinal (fromIntegral count)) finished)
          ]

propReorderedObligationSettlement :: Property
propReorderedObligationSettlement = forAll (chooseInt (1, 100)) $ \count ->
  let origins = [fixtureOrigin 4 (fromIntegral index) | index <- [0 .. count - 1]]
   in forAll (shuffle origins) $ \order ->
        let registered = foldl' (flip registerObligation) emptyDrainLedger origins
            queued = foldl' (\ledger origin -> queueObligationObservation origin (CompletionOrdinal 7) ledger) registered origins
            sealed = sealDrainLedger (BatchOrdinal 4) (CompletionOrdinal 7) (advanceRoutedThrough (BatchOrdinal 4) queued)
            histories = scanl (flip markObligationKernelStepped) sealed order
            finished = foldl' (flip markObligationKernelStepped) sealed order
            replayed = foldl' (\ledger origin -> registerObligation origin (awaitPhysicalOutcome origin (queueObligationObservation origin (CompletionOrdinal 7) ledger))) finished origins
         in conjoin
              [ fmap (Map.size . drainLedgerObligations) histories === reverse [0 .. count],
                counterexample "completion prefix alone bypassed unresolved origins" (all (not . drainBarrierReady (CompletionOrdinal 7)) (take count histories)),
                counterexample "short completion prefix bypassed cut" (not (drainBarrierReady (CompletionOrdinal 6) finished)),
                counterexample "late origin replay reopened the drained cut" (drainBarrierReady (CompletionOrdinal 7) replayed),
                drainLedgerObligations replayed === Map.empty
              ]

genDrainScenario :: Gen DrainScenario
genDrainScenario = do
  count <- chooseInt (1, 12)
  states <- vectorOf count (elements [minBound .. maxBound])
  cut <- fromIntegral <$> chooseInt (0, 12)
  routed <- fromIntegral <$> chooseInt (0, 12)
  completionCut <- fromIntegral <$> chooseInt (1, 12)
  stepped <- fromIntegral <$> chooseInt (0, 12)
  pure (DrainScenario states cut routed completionCut stepped)

applyGeneratedState :: GeneratedObligationState -> ObligationOrigin -> DrainLedger -> DrainLedger
applyGeneratedState state origin = case state of
  GeneratedOffered -> settleObligationByOffer origin
  GeneratedAwaiting -> awaitPhysicalOutcome origin
  GeneratedQueued -> queueObligationObservation origin (CompletionOrdinal 1)
  GeneratedStepped -> markObligationKernelStepped origin

generatedObligationState :: GeneratedObligationState -> ObligationState
generatedObligationState = \case
  GeneratedOffered -> SettledByOffer
  GeneratedAwaiting -> AwaitingPhysicalOutcome
  GeneratedQueued -> ObservationQueued (CompletionOrdinal 1)
  GeneratedStepped -> KernelStepped

scanClaims :: [PeerDispatchOutcome] -> [(Bool, OutcomeCellState)]
scanClaims = drop 1 . scanl claim (False, OutcomeCellEmpty)
  where
    claim (_, state) outcome = claimOutcomeCell outcome state

finalCell :: [(Bool, OutcomeCellState)] -> OutcomeCellState
finalCell = foldl (\_ (_, state) -> state) OutcomeCellEmpty

genOutcome :: Gen PeerDispatchOutcome
genOutcome = elements [minBound .. maxBound]

fixtureOrigin :: Word64 -> Word64 -> ObligationOrigin
fixtureOrigin batch member = ObligationOrigin (BatchOrdinal batch) (EffectMemberOrdinal member)

fixtureCandidate :: Word8 -> PeerCandidate
fixtureCandidate seed =
  peerCandidate
    (checked (mkHeraldId (identifierBytes seed)))
    (checked (mkHeraldEpoch (identifierBytes (seed + 1))))
    (connectionNonce (fromIntegral seed))

identifierBytes :: Word8 -> ByteString.ByteString
identifierBytes seed = ByteString.pack (take 32 [seed ..])

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
