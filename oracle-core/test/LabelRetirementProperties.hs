{-# LANGUAGE OverloadedStrings #-}

module LabelRetirementProperties (tests) where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Serialize.Put qualified as Put
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Identity
import Eclips.Domain.Label
import Eclips.Domain.Membership
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Oracle.Identity (oracleClientRequestId, oracleClientRequestSequence)
import Eclips.Oracle.Label
import Eclips.Oracle.Progress
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import OracleFixtures
import Step15ReferenceProperties (retireLiveTarget, step15Genesis, targetH4, targetH5)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Positive (..), Property, conjoin, counterexample, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "label witness retirement"
    [ testProperty "indexed retirement agrees with independent append-only history and promise minimum" propRetirement,
      testProperty "combined progress joins independent released sets and frontiers" propProgressJoin,
      testCase "retired witness, retained exact receipt, expired request and unknown decision stay distinct" caseReplayClasses,
      testCase "pending decisions survive floor advancement and complete without re-retention" casePending,
      testCase "NotApplied advances label high water and retires without installation" caseNotApplied,
      testCase "canonical member retirement releases the blocking promise" caseMembership,
      testCase "future progress rejects and unchanged progress makes no entry" caseMaintenance,
      testCase "receipt-only envelope update preserves label promise and request identity" caseEnvelope,
      testCase "checkpoint rejects stale floor and colliding original decision indices" caseCheckpointClosedness
    ]

object :: GlobalObjectId
object = globalObjectId 219

terminalIndex :: LiveTerminalOutcome -> ControlIndex
terminalIndex terminal = case liveTerminalOutcomeView terminal of
  LiveNotAppliedOutcomeView _ index _ _ _ _ -> index
  LiveReleasedOutcomeView _ _ index _ -> index

commit :: OracleEnvelope -> OracleState -> (OracleState, OracleReceipt, AppliedOracleEntry)
commit envelope state = case oracleSubmissionOutcomeView outcome of
  OracleSubmissionCommittedView receipt entry -> (next, receipt, entry)
  other -> error ("expected committed lifetime fixture: " <> show other)
  where
    (next, outcome) = submitOracleState envelope state

decide :: Word64 -> Word64 -> OracleState -> (OracleState, LiveLabelDecision, LiveTerminalOutcome)
decide = decideObject object

decideObject :: GlobalObjectId -> Word64 -> Word64 -> OracleState -> (OracleState, LiveLabelDecision, LiveTerminalOutcome)
decideObject subject generation sequenceNumber state =
  let request = oracleClientRequestId fixtureHeraldEpoch sequenceNumber
      ident = deriveLabelDecisionId fixtureSystemId request
      evidence = if generation == 0 then Just (initialBootstrapLabelEvidence subject fixtureProcessEpoch) else Nothing
      acceptance = checked "acceptance position" (mkLabelProcessAcceptancePosition fixtureProcessEpoch (generation + 1))
      command = decideLabelCommand ident fixtureProcessEpoch subject (ProcessLabel fixtureProcessEpoch, generation) evidence Nothing Nothing (targetProcess fixtureProcessEpoch) (homeLabelAcceptanceCut acceptance EmptyHeraldPublicationPrefix)
      (next, receipt, _) = commit (oracleEnvelope request Nothing fixtureHeraldEpoch command) state
   in if oracleReceiptResult receipt /= OracleAccepted
        then error "label lifetime Decide rejected"
        else
          (next, maybe (error "decision absent") id (oracleOpenDecision ident next), maybe (error "terminal absent") id (oracleTerminalOutcome ident next))

completion :: LiveLabelDecision -> LiveTerminalOutcome -> HeraldEpoch -> OracleCommand
completion decision terminal home = completeLabelDecisionCommand (labelCompletionAttestation (liveDecisionId decision) (liveDecisionRequestControlIndex decision) (deriveLabelOutcomeDigest terminal) home (liveDecisionMembershipGeneration decision))

history :: Word64 -> OracleState -> (OracleState, [(LiveLabelDecision, LiveTerminalOutcome)])
history count initial = foldl' step (initial, []) [0 .. count - 1]
  where
    step (state, prior) generation =
      let number = generation * 2 + 1
          (opened, decision, terminal) = decide generation number state
          (done, receipt, _) = commit (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch (number + 1)) Nothing fixtureHeraldEpoch (completion decision terminal fixtureHeraldEpoch)) opened
       in if oracleReceiptResult receipt /= OracleAccepted then error "label lifetime Complete rejected" else (done, prior <> [(decision, terminal)])

announce :: HeraldEpoch -> OracleProgress -> OracleState -> (OracleState, OracleSubmissionOutcomeView)
announce home progress state =
  let sequenceNumber = maybe 0 id (Lifetime.receiptRetirementHighWater (oracleProgressReceipts progress))
      envelope = oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home (retireOracleProgressCommand progress)
      (next, outcome) = submitOracleState envelope state
   in (next, oracleSubmissionOutcomeView outcome)

homes :: OracleState -> [HeraldEpoch]
homes = NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs . oracleCurrentMembership

-- This reference never deletes its history. It derives the retired prefix by
-- exhaustively taking the minimum of independently retained home promises.
propRetirement :: Positive Word8 -> [Word8] -> Property
propRetirement (Positive seed) schedule = conjoin [run mode | mode <- [OracleStateDigestDisabled, OracleStateDigestEnabled]]
  where
    count = 1 + fromIntegral (seed `mod` 12)
    run mode =
      let (initial, past) = history count (initialOracleStateWithDigestMode mode fixtureCheckedGenesis)
          members = homes initial
          high = liveDecisionRequestControlIndex (fst (last past))
          initialPromises = Map.fromList [(home, controlIndex 0) | home <- members]
          actions = take 30 schedule <> [0, 1, 2]
          step (state, promises, checks, ordinal) byte =
            let home = members !! (fromIntegral byte `mod` length members)
                through = if ordinal >= length (take 30 schedule) then high else controlIndex (fromIntegral byte `mod` (controlIndexWord64 high + 1))
                promises' = Map.adjust (max through) home promises
                retired = minimum (Map.elems promises')
                (advanced, response) = announce home (oracleProgress mempty through) state
                expectedCount = length [() | (historicalDecision, _) <- past, liveDecisionRequestControlIndex historicalDecision > retired]
                selected@(decision, terminal) = past !! (fromIntegral byte `mod` length past)
                request = oracleClientRequestId fixtureHeraldEpoch (2 * count + fromIntegral ordinal + 1)
                (queried, receipt, entry) = commit (oracleEnvelopeWithReceiptRetirement (oracleClientRequestSequence request - 1) (oracleEnvelope request Nothing fixtureHeraldEpoch (uncurry (\d t -> completion d t fixtureHeraldEpoch) selected))) advanced
                expectedResult = if liveDecisionRequestControlIndex decision <= retired then OracleRejected (LabelCompletionObsolete (liveDecisionId decision) (terminalIndex terminal) retired) else OracleAccepted
                semanticTags = map oracleProjectionEventTag (appliedEntryProjectionEvents entry)
                expectedTags = if expectedResult == OracleAccepted then [10] else []
                restored = decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes queried)
                acknowledged = case response of OracleSubmissionProgressRetiredView actual progress _ -> actual == home && oracleProgressLabelsThrough progress == promises' Map.! home; _ -> False
                check =
                  conjoin
                    [ counterexample "independent acknowledged minimum" (oracleLabelRetiredThrough queried === retired),
                      counterexample "only completed prefix reclaimed" (oracleCompletedWorkflowCount queried === expectedCount),
                      counterexample "high water is label-specific" (oracleLatestLabelDecision queried === high),
                      counterexample "promise confirmation" acknowledged,
                      counterexample "fresh semantic completion classification" (oracleReceiptResult receipt === expectedResult),
                      counterexample "obsolete completion emits no semantic event" (semanticTags === expectedTags),
                      counterexample "labels survive history retirement" (oracleLabelRecords queried === oracleLabelRecords initial),
                      counterexample "checkpoint reconstructs derived indices" (restored === Right queried)
                    ]
             in (queried, promises', check : checks, ordinal + 1)
          (_, _, results, _) = foldl' step (initial, initialPromises, [], 0 :: Int) actions
       in conjoin results

propProgressJoin :: Word8 -> Word8 -> Word8 -> Property
propProgressJoin a b c =
  let make n = oracleProgress (Lifetime.receiptRetirementPrefix (Just (fromIntegral n))) (controlIndex (fromIntegral (255 - n)))
      x = make a
      y = make b
      z = make c
   in conjoin [(x <> y) === (y <> x), ((x <> y) <> z) === (x <> (y <> z)), (x <> x) === x, (x <> mempty) === x, counterexample "join covers both promises" (oracleProgressCovers (x <> y) x && oracleProgressCovers (x <> y) y)]

fullyAcknowledge :: ControlIndex -> OracleState -> OracleState
fullyAcknowledge through state = foldl' (\prior home -> fst (announce home (oracleProgress mempty through) prior)) state (homes state)

caseReplayClasses :: Assertion
caseReplayClasses = do
  let (completed, past) = history 1 (initialOracleState fixtureCheckedGenesis)
      (decision, terminal) = case past of [one] -> one; _ -> error "one label expected"
      original = oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 2) Nothing fixtureHeraldEpoch (completion decision terminal fixtureHeraldEpoch)
      retired = fullyAcknowledge (liveDecisionRequestControlIndex decision) completed
      (same, duplicate) = submitOracleState original retired
      oldReceipt = maybe (error "retained receipt missing") id (oracleRequestReceipt (oracleEnvelopeRequestId original) retired)
      (expired, _) = announce fixtureHeraldEpoch (oracleProgress (Lifetime.receiptRetirementPrefix (Just 2)) (liveDecisionRequestControlIndex decision)) retired
      (_, expiredOutcome) = submitOracleState original expired
      (fresh, stale, staleEntry) = commit (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 3) Nothing fixtureHeraldEpoch (completion decision terminal fixtureHeraldEpoch)) expired
      unknown = deriveLabelDecisionId fixtureSystemId (oracleClientRequestId fixtureHeraldEpoch 99)
      (_, unknownReceipt, _) = commit (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 4) Nothing fixtureHeraldEpoch (completeLabelDecisionCommand (labelCompletionAttestation unknown (controlIndex 99) (deriveLabelOutcomeDigest terminal) fixtureHeraldEpoch (liveDecisionMembershipGeneration decision)))) fresh
      (_, mismatch, _) = commit (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 3) Nothing fixtureHeraldEpoch (completeLabelDecisionCommand (labelCompletionAttestation (liveDecisionId decision) (controlIndex 99) (deriveLabelOutcomeDigest terminal) fixtureHeraldEpoch (liveDecisionMembershipGeneration decision)))) completed
  oracleCompletedWorkflowCount retired @?= 0
  same @?= retired
  oracleSubmissionOutcomeView duplicate @?= OracleSubmissionDuplicateView oldReceipt
  oracleSubmissionOutcomeView expiredOutcome @?= OracleSubmissionRetiredView (OracleRequestPrefixRetired 2)
  oracleReceiptResult stale @?= OracleRejected (LabelCompletionObsolete (liveDecisionId decision) (controlIndex 1) (controlIndex 1))
  appliedEntryProjectionEvents staleEntry @?= []
  oracleReceiptResult unknownReceipt @?= OracleRejected (UnknownLabelDecision unknown)
  oracleReceiptResult mismatch @?= OracleRejected (LabelCompletionDecisionIndexMismatch (controlIndex 1) (controlIndex 99))

casePending :: Assertion
casePending = do
  let (pending, decision, terminal) = decide 0 1 (initialOracleState fixtureCheckedGenesis)
      retired = fullyAcknowledge (controlIndex 1) pending
      restored = checked "pending above retired entitlement" (decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes retired))
      (done, receipt, _) = commit (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 2) Nothing fixtureHeraldEpoch (completion decision terminal fixtureHeraldEpoch)) restored
  oracleOpenDecisions restored @?= [decision]
  oracleReceiptResult receipt @?= OracleAccepted
  oracleOpenDecisions done @?= []
  oracleCompletedWorkflowCount done @?= 0
  oracleLabelRecords done @?= oracleLabelRecords retired

caseMembership :: Assertion
caseMembership = do
  let (completed, _) = history 2 (initialOracleState step15Genesis)
      high = oracleLatestLabelDecision completed
      acknowledged = foldl' (\state home -> if home == targetH4 || home == targetH5 then state else fst (announce home (oracleProgress mempty high) state)) completed (homes completed)
      one = retireLiveTarget targetH4 acknowledged
      two = retireLiveTarget targetH5 one
  oracleLabelRetiredThrough acknowledged @?= controlIndex 0
  oracleLabelRetiredThrough one @?= controlIndex 0
  oracleLabelRetiredThrough two @?= high
  oracleCompletedWorkflowCount two @?= 0
  map fst (oracleProgresses two) @?= homes two
  decodeOracleCheckpoint step15Genesis (oracleCheckpointBytes two) @?= Right two

caseMaintenance :: Assertion
caseMaintenance = do
  let initial = initialOracleState fixtureCheckedGenesis
      (same, invalid) = announce fixtureHeraldEpoch (oracleProgress mempty (controlIndex 1)) initial
      (unchanged, noChange) = announce fixtureHeraldEpoch mempty initial
  same @?= initial
  invalid @?= OracleSubmissionProtocolRejectedView (InvalidOracleProgress (oracleClientRequestId fixtureHeraldEpoch 0))
  unchanged @?= initial
  noChange @?= OracleSubmissionProgressRetiredView fixtureHeraldEpoch mempty Nothing

caseEnvelope :: Assertion
caseEnvelope = do
  let ordinary = oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 9) Nothing fixtureHeraldEpoch (startProcessEpochCommand fixtureStart)
      full = oracleEnvelopeWithProgress (oracleProgress mempty (controlIndex 7)) ordinary
      changed = oracleEnvelopeWithReceiptRetirement 8 full
  oracleEnvelopeProgress changed @?= Just (oracleProgress (Lifetime.receiptRetirementPrefix (Just 8)) (controlIndex 7))
  oracleEnvelopeDigest changed @?= oracleEnvelopeDigest ordinary
  fmap decodedOracleEnvelopeValue (decodeOracleEnvelopeCanonicalBytes (oracleEnvelopeCanonicalBytes changed)) @?= Right changed

caseCheckpointClosedness :: Assertion
caseCheckpointClosedness = do
  let initial = initialOracleStateWithDigestMode OracleStateDigestDisabled fixtureCheckedGenesis
      (completed, _) = history 1 initial
      acknowledged = fullyAcknowledge (controlIndex 1) completed
      word = Put.runPut . Put.putWord64be
      lifetime = word 1 <> word 1 <> word (fromIntegral (length (homes acknowledged))) <> foldMap (\home -> heraldEpochBytes home <> ByteString.singleton 0 <> word 0 <> word 1) (homes acknowledged)
      bytes = oracleCheckpointBytes acknowledged
      (prefix, suffix) = ByteString.breakSubstring lifetime bytes
      staleFloor = prefix <> word 1 <> word 0 <> ByteString.drop 16 suffix
      (first, pending, _) = decide 0 1 initial
      (both, other, terminal) = decideObject (globalObjectId 218) 0 2 first
      (onePending, _, _) = commit (oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch 3) Nothing fixtureHeraldEpoch (completion other terminal fixtureHeraldEpoch)) both
      witness = labelDecisionIdBytes (liveDecisionId other) <> word (controlIndexWord64 (liveDecisionRequestControlIndex other)) <> heraldMembershipGenerationIdBytes (liveDecisionMembershipGeneration other) <> labelOutcomeDigestBytes (deriveLabelOutcomeDigest terminal)
      (before, after) = ByteString.breakSubstring witness (oracleCheckpointBytes onePending)
      colliding = before <> labelDecisionIdBytes (liveDecisionId other) <> word (controlIndexWord64 (liveDecisionRequestControlIndex pending)) <> ByteString.drop 40 after
  assertBool "lifetime section located" (not (ByteString.null suffix))
  assertBool "checkpoint floor equals, rather than merely trails, the active minimum" (isLeft (decodeOracleCheckpoint fixtureCheckedGenesis staleFloor))
  assertBool "completed witness located" (not (ByteString.null after))
  assertBool "pending/completed original indices cannot collide" (isLeft (decodeOracleCheckpoint fixtureCheckedGenesis colliding))

caseNotApplied :: Assertion
caseNotApplied = do
  let (completed, _) = history 1 (initialOracleState fixtureCheckedGenesis)
      request = oracleClientRequestId fixtureHeraldEpoch 3
      ident = deriveLabelDecisionId fixtureSystemId request
      acceptance = checked "second invocation" (mkLabelProcessAcceptancePosition fixtureProcessEpoch 2)
      command = decideLabelCommand ident fixtureProcessEpoch object (ProcessLabel fixtureProcessEpoch, 0) Nothing Nothing Nothing (targetProcess fixtureProcessEpoch) (homeLabelAcceptanceCut acceptance EmptyHeraldPublicationPrefix)
      (notApplied, receipt, entry) = commit (oracleEnvelope request Nothing fixtureHeraldEpoch command) completed
      retired = fullyAcknowledge (controlIndex 3) notApplied
  oracleReceiptResult receipt @?= OracleAccepted
  case map oracleProjectionEventView (appliedEntryProjectionEvents entry) of
    [LabelDecidedView _ terminal _] -> case liveTerminalOutcomeView terminal of
      LiveNotAppliedOutcomeView {} -> pure ()
      other -> assertFailure ("expected NotApplied outcome: " <> show other)
    other -> assertFailure ("expected canonical decision: " <> show other)
  oracleLatestLabelDecision notApplied @?= controlIndex 3
  oracleOpenDecisions notApplied @?= []
  oracleCompletedWorkflowCount notApplied @?= 2
  oracleLabelRetiredThrough retired @?= controlIndex 3
  oracleLatestLabelDecision retired @?= controlIndex 3
  oracleCompletedWorkflowCount retired @?= 0
  oracleLabelRecords retired @?= oracleLabelRecords completed
