{-# LANGUAGE OverloadedRecordDot #-}

module PeerStreamProperties
  ( tests,
  )
where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment (mkContextClassGenerationId)
import Eclips.Domain.Identity
  ( HeraldEpoch,
    mkHeraldEpoch,
  )
import Eclips.Herald.PeerStream
  ( PeerDispatchAttempt,
    PeerDispatchBindingGeneration,
    PeerDispatchOutcome (..),
    PeerDispatchTicket,
    PeerItem,
    PeerItemDigest,
    ReceiveDisposition (..),
    ReceiveProgress (..),
    ResumeOffer,
    SequencedItem,
    StreamDirection,
    StreamPrefix,
    StreamSequence,
    StreamShapeProblem (..),
    assignmentReceipt,
    assignmentReceiptDigest,
    assignmentReceiptSequence,
    emptyStreamPrefix,
    firstStreamSequence,
    gapSummaryDirection,
    gapSummaryEntries,
    gapSummaryReceivedPrefix,
    mkGapSummary,
    mkPeerDispatchBindingGeneration,
    mkPeerItemDigest,
    mkResumeOffer,
    mkStreamDirection,
    mkStreamSequence,
    nextAfterStreamPrefix,
    nextStreamSequence,
    peerDispatchAttemptBindingGeneration,
    peerDispatchAttemptDirection,
    peerDispatchAttemptGeneration,
    peerDispatchAttemptGenerationWord64,
    peerDispatchAttemptItem,
    peerDispatchBindingGenerationWord64,
    peerDispatchTicketDirection,
    peerDispatchTicketGenerationWord64,
    peerItem,
    peerItemDigestBytes,
    resumeOfferCompletedPrefix,
    resumeOfferDestination,
    resumeOfferGapSummary,
    resumeOfferNextSourceSequence,
    resumeOfferReceivedPrefix,
    resumeOfferSource,
    resumeResponseCompletedPrefix,
    resumeResponseReceivedPrefix,
    resumeResponseRetransmitFrom,
    reverseStreamDirection,
    sequencedItem,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamCompletedPrefix,
    streamDirectionDestination,
    streamDirectionSource,
    streamPrefixSequence,
    streamPrefixThrough,
    streamReceivedPrefix,
    streamSequenceWord64,
  )
import Eclips.Herald.PeerStream.State
  ( IncomingItemStatus (..),
    PeerStreamInvariantViolation (..),
    PeerStreamProblem (..),
    PeerStreamProtocolViolation (..),
    PreparedReceiveCandidate,
    ResumeAssociationPhase (..),
    State,
    activeOutgoingItems,
    assignmentCompletionKnown,
    assignmentRetirementKnown,
    commitCompletedAck,
    commitCompletion,
    commitDispatchBinding,
    commitDispatchBindingLoss,
    commitDispatchDrainCut,
    commitDispatchOutcome,
    commitDispatchSelection,
    commitDispatchTicket,
    commitDrainingDispatchOutcome,
    commitEnqueue,
    commitFrontierAdvance,
    commitLocalRetirementCut,
    commitPeerRetirement,
    commitReceive,
    commitReceivedAck,
    commitResume,
    currentDispatchAttempt,
    currentDispatchBinding,
    currentDispatchTicket,
    dispatchAwaitingSequences,
    finalizeReceive,
    incomingGapSummary,
    incomingGreatestAdvertisedNextSequence,
    incomingRetainedItems,
    incomingWatermarks,
    initialState,
    outgoingCompletionProgress,
    outgoingNextSequence,
    outgoingPeerGapSummary,
    outgoingWatermarks,
    peerStreamIncomingDirections,
    peerStreamPeerRetired,
    peerStreamRetentionCounts,
    peerStreamStateWitness,
    peerStreamWitnessIncomingDirections,
    prepareCompletedAck,
    prepareCompletion,
    prepareDispatchBinding,
    prepareDispatchBindingLoss,
    prepareDispatchDrainCut,
    prepareDispatchOutcome,
    prepareDispatchSelection,
    prepareDrainingDispatchOutcome,
    prepareEnqueue,
    prepareEnsureDispatchTicket,
    prepareFrontierAdvance,
    prepareLocalRetirementCut,
    preparePeerRetirement,
    prepareReceiveCandidate,
    prepareReceivedAck,
    prepareResume,
    prepareResumeForAssociation,
    prepareSequenceBoundEnqueue,
    preparedContiguousItems,
    preparedDispatchBindingLost,
    preparedDispatchBindingTicket,
    preparedDispatchDrainCutDirections,
    preparedDispatchOutcomeTicket,
    preparedDispatchSelectionAttempt,
    preparedDispatchTicket,
    preparedEnqueueAssignments,
    preparedEnqueueDispatchTickets,
    preparedEnqueueFrontierAdvances,
    preparedLocalRetirementCutDirections,
    preparedPeerRetirementDirection,
    preparedPeerRetirementSettledAssignments,
    preparedReceiveDisposition,
    preparedResumeDispatchTicket,
    preparedResumeResponse,
    preparedResumeRetransmissions,
    resumeOfferForPeer,
    sequenceBoundPeerItem,
    validatePeerStreamState,
  )
import Eclips.Herald.PeerStream.State qualified as MarkerWork
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    choose,
    conjoin,
    counterexample,
    elements,
    forAll,
    ioProperty,
    listOf,
    property,
    shuffle,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "peer stream owner"
    [ testGroup
        "nominal vocabulary"
        [ testCase "sequences are positive and the empty prefix is explicit" caseSequenceAndPrefixShape,
          testCase "directions are ordered and reject equal endpoints" caseDirectionShape,
          testCase "digests have exactly 32 bytes" caseDigestShape,
          testCase "dispatch binding generations are positive" caseDispatchBindingShape,
          testCase "gap summaries are canonical and begin beyond the first missing item" caseGapShape,
          testCase "resume offers bind opposite direction roles" caseResumeShape,
          testProperty "positive sequence construction round-trips" propPositiveSequenceRoundTrip
        ],
      testGroup
        "outgoing and incoming transitions"
        [ testCase "multi-peer enqueue is atomic, ordered, and independently sequenced" caseMultiPeerEnqueue,
          testCase "sequence-bound enqueue uses and retains each exact allocation across reconnect" caseSequenceBoundEnqueue,
          testCase "gaps, duplicates, conflicts, and closure have exact dispositions" caseReceiveGapDuplicateAndClosure,
          testCase "receive refinement requires exactly the newly contiguous run" caseReceiveRefinement,
          testCase "completion advances only across the contiguous completed cut" caseOutOfOrderCompletion,
          testCase "marker head work ignores blocked suffix changes and wakes exact dependencies" caseIncomingMarkerWork,
          testCase "received and completed acknowledgements retain and release at distinct points" caseAcknowledgements,
          testCase "resume validates roles, frontiers, gaps, and exact retained retransmissions" caseResume,
          testCase "acknowledgement and resume rejection matrices are exhaustive" caseProtocolMatrices,
          testCase "one-way source frontiers constrain receipt without changing reverse dispatch" caseFrontierAdvance,
          testProperty "generated same-association resume claims reactivate exactly the request delta" propResumeAssociationDelta,
          testProperty "generated acknowledgement/resume traces agree with the reference model" propModelTrace,
          testProperty "generated receive permutations close gaps and preserve independent completion" propReceivePermutationTrace,
          testProperty "generated several-peer mixed traces agree after every operation" propMixedSeveralPeerTrace,
          testProperty "narrow incoming directions match full witnesses throughout generated mixed traces" propIncomingDirectionWitness,
          testProperty "equal startup and operation traces are deterministic" propTraceDeterminism
        ],
      testGroup
        "logical dispatch"
        [ testCase "binding and ensure coalesce exactly one ticket" caseDispatchTicketCoalescing,
          testCase "selection owns the lowest exact item and one attempt" caseDispatchSelection,
          testCase "outcome retry, duplicate, stale, and contradiction semantics are exact" caseDispatchOutcomes,
          testCase "binding replacement preserves exact work and supersedes the old attempt" caseDispatchBindingReplacement,
          testCase "the drain cut atomically unbinds every direction without changing work" caseDispatchDrainCut,
          testCase "local retirement cuts every dispatch fact without retiring survivor peers" caseLocalRetirementCut,
          testCase "peer retirement tombstones scheduling while retaining terminal evidence" casePeerRetirement,
          testCase "peer retirement preserves completed prefix truth and settles only the active suffix" casePeerRetirementPartition,
          testCase "acknowledgement and resume supersede or re-enable exact attempts" caseDispatchAcknowledgementAndResume,
          testCase "seed-160130: Deferred; replacement writes; first Resume; duplicate; gap repair" (caseFirstResumeDispatch ResumeAfterWrites),
          testCase "seed-160131: Deferred; first Resume; replacement writes; duplicate; gap repair" (caseFirstResumeDispatch ResumeBeforeSelection),
          testCase "seed-160132: Deferred; replacement attempt; first Resume; Written; duplicate; gap repair" (caseFirstResumeDispatch ResumeDuringAttempt),
          testCase "seed-160133: same-binding Hello reoffer repairs old writes and preserves new writes" caseReofferedHelloResume,
          testCase "seed-160134: first Resume receipt supersedes replacement attempt before Written" caseFirstResumeReceipt,
          testCase "seed-160135: normalized gap receipt supersedes repair until raw gap removal" caseNormalizedGapReceipt,
          testProperty "generated failed/deferred traces advance positive attempt generations" propDispatchRetryTrace
        ]
    ]

caseSequenceAndPrefixShape :: Assertion
caseSequenceAndPrefixShape = do
  assertEqual
    "zero cannot be represented as a stream item"
    (Left StreamSequenceMustBePositive)
    (mkStreamSequence 0)
  assertEqual "the first sequence is one" 1 (streamSequenceWord64 firstStreamSequence)
  assertEqual "empty has no hidden sequence" Nothing (streamPrefixSequence emptyStreamPrefix)
  assertEqual "the first item follows empty" firstStreamSequence (nextAfterStreamPrefix emptyStreamPrefix)
  let second = nextStreamSequence firstStreamSequence
  assertEqual "successor advances once" 2 (streamSequenceWord64 second)
  assertEqual
    "a non-empty prefix exposes its exact sequence"
    (Just second)
    (streamPrefixSequence (streamPrefixThrough second))

caseDirectionShape :: Assertion
caseDirectionShape = do
  let local = fixtureHerald 1
      peer = fixtureHerald 2
  assertEqual
    "a self direction is rejected"
    (Left (StreamDirectionEndpointsEqual local))
    (mkStreamDirection local local)
  direction <- checkedIO "direction" (mkStreamDirection local peer)
  assertEqual "source is retained" local (streamDirectionSource direction)
  assertEqual "destination is retained" peer (streamDirectionDestination direction)
  assertEqual "reversal swaps source" peer (streamDirectionSource (reverseStreamDirection direction))
  assertEqual "reversal swaps destination" local (streamDirectionDestination (reverseStreamDirection direction))

caseDigestShape :: Assertion
caseDigestShape = do
  mapM_ rejects [0, 1, 31, 33, 64]
  digest <- checkedIO "digest" (mkPeerItemDigest (ByteString.replicate 32 7))
  assertEqual "all exact bytes are retained" (ByteString.replicate 32 7) (peerItemDigestBytes digest)
  where
    rejects lengthInBytes =
      assertEqual
        ("digest length " <> show lengthInBytes)
        (Left (PeerItemDigestWrongByteCount lengthInBytes))
        (mkPeerItemDigest (ByteString.replicate lengthInBytes 0))

caseDispatchBindingShape :: Assertion
caseDispatchBindingShape = do
  assertEqual
    "zero cannot identify an admitted binding"
    (Left PeerDispatchBindingGenerationMustBePositive)
    (mkPeerDispatchBindingGeneration 0)
  generation <- checkedIO "binding generation" (mkPeerDispatchBindingGeneration 7)
  assertEqual
    "an admitted positive generation round-trips"
    7
    (peerDispatchBindingGenerationWord64 generation)

caseGapShape :: Assertion
caseGapShape = do
  let direction = fixtureDirection 1 2
      first = firstStreamSequence
      second = nextStreamSequence first
      third = nextStreamSequence second
      digest2 = fixtureDigest 2
      digest3 = fixtureDigest 3
  assertEqual
    "the first missing item cannot be called a gap"
    (Left (GapEntryNotBeyondFirstMissing first))
    (mkGapSummary direction emptyStreamPrefix [(first, fixtureDigest 1)])
  assertEqual
    "entries must be presented in canonical order"
    (Left GapEntriesNotStrictlyAscending)
    (mkGapSummary direction emptyStreamPrefix [(third, digest3), (second, digest2)])
  assertEqual
    "duplicate entries are rejected"
    (Left GapEntriesNotStrictlyAscending)
    (mkGapSummary direction emptyStreamPrefix [(second, digest2), (second, digest2)])
  summary <- checkedIO "canonical gap" (mkGapSummary direction emptyStreamPrefix [(second, digest2), (third, digest3)])
  assertEqual "the exact prefix is retained" emptyStreamPrefix (gapSummaryReceivedPrefix summary)
  assertEqual "the canonical entries are retained" [(second, digest2), (third, digest3)] (gapSummaryEntries summary)

caseResumeShape :: Assertion
caseResumeShape = do
  let source = fixtureHerald 1
      destination = fixtureHerald 2
      forward = fixtureDirection 1 2
      reverseDirection = reverseStreamDirection forward
      first = firstStreamSequence
      received = streamPrefixThrough first
  noGap <- checkedIO "reverse gap" (mkGapSummary reverseDirection received [])
  offer <- checkedIO "resume offer" (mkResumeOffer source destination (nextStreamSequence first) received received noGap)
  assertEqual "source role" source (resumeOfferSource offer)
  assertEqual "destination role" destination (resumeOfferDestination offer)
  assertEqual "source frontier" (nextStreamSequence first) (resumeOfferNextSourceSequence offer)
  assertEqual "reverse received prefix" received (resumeOfferReceivedPrefix offer)
  assertEqual "reverse completed prefix" received (resumeOfferCompletedPrefix offer)
  assertEqual "reverse gap transcript" noGap (resumeOfferGapSummary offer)
  wrongGap <- checkedIO "forward gap" (mkGapSummary forward received [])
  assertEqual
    "the gap belongs to the reverse direction"
    (Left (ResumeGapDirectionMismatch reverseDirection forward))
    (mkResumeOffer source destination (nextStreamSequence first) received received wrongGap)
  emptyGap <- checkedIO "empty reverse gap" (mkGapSummary reverseDirection emptyStreamPrefix [])
  assertEqual
    "the gap prefix must equal the offered received prefix"
    (Left (ResumeGapPrefixMismatch received emptyStreamPrefix))
    (mkResumeOffer source destination (nextStreamSequence first) received received emptyGap)
  assertEqual
    "completion cannot exceed receipt"
    (Left (ResumeCompletedBeyondReceived received emptyStreamPrefix))
    (mkResumeOffer source destination (nextStreamSequence first) emptyStreamPrefix received emptyGap)

propPositiveSequenceRoundTrip :: Property
propPositiveSequenceRoundTrip =
  forAll positiveFinite $ \value ->
    case mkStreamSequence value of
      Left problem -> counterexample (show problem) False
      Right sequenceNumber -> streamSequenceWord64 sequenceNumber === value
  where
    positiveFinite = choose (1, 1000000) :: Gen Word64

caseDispatchTicketCoalescing :: Assertion
caseDispatchTicketCoalescing = do
  let binding = fixtureBinding 7
      pristine = initialState fixtureLocal :: State Payload
  bindingPlan <-
    checkedIO
      "bind empty direction"
      (prepareDispatchBinding fixtureOutgoingA binding pristine)
  assertEqual
    "an empty direction has no scheduling work"
    Nothing
    (preparedDispatchBindingTicket bindingPlan)
  let (bound, _) = commitDispatchBinding bindingPlan
  assertEqual
    "the exact admitted generation is retained"
    (Just binding)
    =<< checkedIO "current binding" (currentDispatchBinding fixtureOutgoingA bound)
  enqueuePlan <-
    checkedIO
      "enqueue on bound direction"
      (prepareEnqueue (Map.singleton fixturePeerA (fixtureItem 1 :| [])) bound)
  ticket <- exactlyOne "newly eligible direction ticket" (preparedEnqueueDispatchTickets enqueuePlan)
  let (ready, _) = commitEnqueue enqueuePlan
  assertEqual "ticket direction" fixtureOutgoingA (peerDispatchTicketDirection ticket)
  assertEqual
    "the first ticket generation is positive"
    1
    (peerDispatchTicketGenerationWord64 ticket)
  assertEqual
    "the owner retains the same ticket"
    (Just ticket)
    =<< checkedIO "current ticket" (currentDispatchTicket fixtureOutgoingA ready)
  ensurePlan <-
    checkedIO
      "ensure current ticket"
      (prepareEnsureDispatchTicket fixtureOutgoingA ready)
  assertEqual "ensure does not re-emit a current ticket" Nothing (preparedDispatchTicket ensurePlan)
  assertEqual "ensure is state-idempotent" ready (fst (commitDispatchTicket ensurePlan))
  duplicateBinding <-
    checkedIO
      "repeat current binding"
      (prepareDispatchBinding fixtureOutgoingA binding ready)
  assertEqual
    "repeating a binding does not re-emit its ticket"
    Nothing
    (preparedDispatchBindingTicket duplicateBinding)
  assertEqual
    "repeating a binding is state-identical"
    ready
    (fst (commitDispatchBinding duplicateBinding))
  staleBinding <-
    checkedIO
      "observe stale binding"
      (prepareDispatchBinding fixtureOutgoingA (fixtureBinding 6) ready)
  assertEqual
    "a stale binding is state-identical"
    ready
    (fst (commitDispatchBinding staleBinding))
  assertOwnerInvariant ready

caseDispatchSelection :: Assertion
caseDispatchSelection = do
  let binding = fixtureBinding 1
  (ready, ticket, assigned) <- dispatchFixture binding [10, 11]
  selection <-
    checkedIO
      "select lowest retained item"
      (prepareDispatchSelection ticket binding ready)
  attempt <-
    requiredMaybe
      "selection did not expose its attempt"
      (preparedDispatchSelectionAttempt selection)
  let (selected, committedAttempt) = commitDispatchSelection selection
      expected = NonEmpty.head assigned
  assertEqual "commit returns the installed attempt" (Just attempt) committedAttempt
  assertEqual "attempt binding" binding (peerDispatchAttemptBindingGeneration attempt)
  assertEqual "attempt direction" fixtureOutgoingA (peerDispatchAttemptDirection attempt)
  assertEqual "the first attempt generation is positive" 1 (attemptGenerationWord64 attempt)
  assertEqual "selection chooses the lowest exact retained item" expected (peerDispatchAttemptItem attempt)
  assertEqual
    "selection consumes the ticket"
    Nothing
    =<< checkedIO "ticket after selection" (currentDispatchTicket fixtureOutgoingA selected)
  assertEqual
    "selection installs exactly its attempt"
    (Just attempt)
    =<< checkedIO "attempt after selection" (currentDispatchAttempt fixtureOutgoingA selected)
  duplicate <-
    checkedIO
      "duplicate selection"
      (prepareDispatchSelection ticket binding selected)
  assertEqual
    "duplicate selection exposes no second attempt"
    Nothing
    (preparedDispatchSelectionAttempt duplicate)
  assertEqual
    "duplicate selection is state-identical"
    selected
    (fst (commitDispatchSelection duplicate))
  assertOwnerInvariant selected

caseDispatchOutcomes :: Assertion
caseDispatchOutcomes = do
  let binding = fixtureBinding 1
  (ready, ticket1, _) <- dispatchFixture binding [1]
  (attempting1, attempt1) <- selectDispatch binding ticket1 ready
  deferred <-
    checkedIO
      "defer first attempt"
      (prepareDispatchOutcome attempt1 PeerDispatchDeferred attempting1)
  ticket2 <-
    requiredMaybe
      "deferred attempt did not schedule retry"
      (preparedDispatchOutcomeTicket deferred)
  let (afterDeferred, committedTicket2) = commitDispatchOutcome deferred
  assertEqual "commit returns retry ticket" (Just ticket2) committedTicket2
  assertEqual
    "deferred work remains immediately eligible"
    Set.empty
    =<< checkedIO "deferred awaiting set" (dispatchAwaitingSequences fixtureOutgoingA afterDeferred)
  duplicateDeferred <-
    checkedIO
      "duplicate deferred observation"
      (prepareDispatchOutcome attempt1 PeerDispatchDeferred afterDeferred)
  assertEqual "duplicate outcome emits no ticket" Nothing (preparedDispatchOutcomeTicket duplicateDeferred)
  assertEqual
    "duplicate outcome is state-identical"
    afterDeferred
    (fst (commitDispatchOutcome duplicateDeferred))
  assertDispatchContradiction attempt1 PeerDispatchDeferred PeerDispatchWritten afterDeferred
  (attempting2, attempt2) <- selectDispatch binding ticket2 afterDeferred
  assertEqual "retry generation advances once" 2 (attemptGenerationWord64 attempt2)
  assertEqual
    "retry selects the identical item"
    (peerDispatchAttemptItem attempt1)
    (peerDispatchAttemptItem attempt2)
  staleOld <-
    checkedIO
      "stale first observation"
      (prepareDispatchOutcome attempt1 PeerDispatchWritten attempting2)
  assertEqual
    "a superseded attempt is a no-op"
    attempting2
    (fst (commitDispatchOutcome staleOld))
  failed <-
    checkedIO
      "fail second attempt"
      (prepareDispatchOutcome attempt2 PeerDispatchFailed attempting2)
  ticket3 <-
    requiredMaybe
      "failed attempt did not schedule retry"
      (preparedDispatchOutcomeTicket failed)
  let afterFailed = fst (commitDispatchOutcome failed)
  (attempting3, attempt3) <- selectDispatch binding ticket3 afterFailed
  assertEqual "second retry generation advances once" 3 (attemptGenerationWord64 attempt3)
  written <-
    checkedIO
      "write third attempt"
      (prepareDispatchOutcome attempt3 PeerDispatchWritten attempting3)
  assertEqual
    "written observation waits for protocol progress"
    Nothing
    (preparedDispatchOutcomeTicket written)
  let afterWritten = fst (commitDispatchOutcome written)
  assertEqual
    "written item is marked awaiting peer progress"
    (Set.singleton sequence1)
    =<< checkedIO "written awaiting set" (dispatchAwaitingSequences fixtureOutgoingA afterWritten)
  duplicateWritten <-
    checkedIO
      "duplicate written observation"
      (prepareDispatchOutcome attempt3 PeerDispatchWritten afterWritten)
  assertEqual
    "duplicate written observation is state-identical"
    afterWritten
    (fst (commitDispatchOutcome duplicateWritten))
  assertDispatchContradiction attempt3 PeerDispatchWritten PeerDispatchFailed afterWritten
  assertOwnerInvariant afterWritten

caseDispatchBindingReplacement :: Assertion
caseDispatchBindingReplacement = do
  let binding1 = fixtureBinding 1
      binding2 = fixtureBinding 2
  (ready, ticket1, _) <- dispatchFixture binding1 [42]
  (attempting1, attempt1) <- selectDispatch binding1 ticket1 ready
  replacement <-
    checkedIO
      "replace dispatch binding"
      (prepareDispatchBinding fixtureOutgoingA binding2 attempting1)
  ticket2 <-
    requiredMaybe
      "binding replacement omitted retained work ticket"
      (preparedDispatchBindingTicket replacement)
  let replaced = fst (commitDispatchBinding replacement)
  assertEqual
    "replacement installs the new binding"
    (Just binding2)
    =<< checkedIO "replacement binding" (currentDispatchBinding fixtureOutgoingA replaced)
  assertEqual
    "replacement supersedes the old attempt"
    Nothing
    =<< checkedIO "replacement attempt" (currentDispatchAttempt fixtureOutgoingA replaced)
  assertEqual "replacement mints a later ticket" 2 (peerDispatchTicketGenerationWord64 ticket2)
  (attempting2, attempt2) <- selectDispatch binding2 ticket2 replaced
  assertEqual
    "replacement preserves exact sequence, digest, and payload"
    (peerDispatchAttemptItem attempt1)
    (peerDispatchAttemptItem attempt2)
  assertEqual "new attempt references the new binding" binding2 (peerDispatchAttemptBindingGeneration attempt2)
  assertEqual "attempt generation remains monotone across bindings" 2 (attemptGenerationWord64 attempt2)
  staleOutcome <-
    checkedIO
      "old-binding observation"
      (prepareDispatchOutcome attempt1 PeerDispatchFailed attempting2)
  assertEqual
    "an old-binding observation cannot disturb the replacement"
    attempting2
    (fst (commitDispatchOutcome staleOutcome))
  staleBinding <-
    checkedIO
      "old binding after replacement"
      (prepareDispatchBinding fixtureOutgoingA binding1 attempting2)
  assertEqual
    "an older binding cannot replace the current attempt"
    attempting2
    (fst (commitDispatchBinding staleBinding))
  staleLoss <-
    checkedIO
      "stale binding loss"
      (prepareDispatchBindingLoss fixtureOutgoingA binding1 attempting2)
  assertEqual "stale loss is not admitted" False (preparedDispatchBindingLost staleLoss)
  assertEqual
    "stale loss cannot disturb the current attempt"
    attempting2
    (fst (commitDispatchBindingLoss staleLoss))
  currentLoss <-
    checkedIO
      "current binding loss"
      (prepareDispatchBindingLoss fixtureOutgoingA binding2 attempting2)
  assertEqual "current loss is admitted" True (preparedDispatchBindingLost currentLoss)
  let unbound = fst (commitDispatchBindingLoss currentLoss)
  assertEqual
    "binding loss clears the exact current binding"
    Nothing
    =<< checkedIO "binding after loss" (currentDispatchBinding fixtureOutgoingA unbound)
  assertEqual
    "binding loss supersedes its current attempt"
    Nothing
    =<< checkedIO "attempt after loss" (currentDispatchAttempt fixtureOutgoingA unbound)
  assertEqual
    "binding loss leaves retained work byte-for-byte unchanged"
    [peerDispatchAttemptItem attempt2]
    =<< checkedIO "retained work after loss" (activeOutgoingItems fixtureOutgoingA unbound)
  unscheduled <-
    checkedIO
      "ensure while unbound"
      (prepareEnsureDispatchTicket fixtureOutgoingA unbound)
  assertEqual "unbound retained work has no ticket" Nothing (preparedDispatchTicket unscheduled)
  duplicateLoss <-
    checkedIO
      "duplicate binding loss"
      (prepareDispatchBindingLoss fixtureOutgoingA binding2 unbound)
  assertEqual "duplicate loss is stale" False (preparedDispatchBindingLost duplicateLoss)
  assertEqual
    "duplicate loss is state-identical"
    unbound
    (fst (commitDispatchBindingLoss duplicateLoss))
  assertOwnerInvariant unbound

caseDispatchDrainCut :: Assertion
caseDispatchDrainCut = do
  let bindingA = fixtureBinding 1
      bindingB = fixtureBinding 9
      outgoingB = fixtureOutgoingDirection FixturePeerB
      pristine = initialState fixtureLocal :: State Payload
  bindA <-
    checkedIO
      "bind drain direction A"
      (prepareDispatchBinding fixtureOutgoingA bindingA pristine)
  let afterBindA = fst (commitDispatchBinding bindA)
  bindB <-
    checkedIO
      "bind drain direction B"
      (prepareDispatchBinding outgoingB bindingB afterBindA)
  let bound = fst (commitDispatchBinding bindB)
  enqueue <-
    checkedIO
      "enqueue drain work"
      ( prepareEnqueue
          ( Map.fromList
              [ (fixturePeerA, fixtureItem 1 :| []),
                (fixturePeerB, fixtureItem 2 :| [])
              ]
          )
          bound
      )
  ticketA <-
    uniqueTicketFor
      fixtureOutgoingA
      (preparedEnqueueDispatchTickets enqueue)
  ticketB <-
    uniqueTicketFor
      outgoingB
      (preparedEnqueueDispatchTickets enqueue)
  let ready = fst (commitEnqueue enqueue)
  (attemptingA, attemptA) <- selectDispatch bindingA ticketA ready
  writtenA <-
    checkedIO
      "write drain direction A"
      (prepareDispatchOutcome attemptA PeerDispatchWritten attemptingA)
  let awaitingA = fst (commitDispatchOutcome writtenA)
  selectionB <-
    checkedIO
      "select drain direction B"
      (prepareDispatchSelection ticketB bindingB awaitingA)
  attemptB <-
    requiredMaybe
      "drain direction B omitted attempt"
      (preparedDispatchSelectionAttempt selectionB)
  let attemptingB = fst (commitDispatchSelection selectionB)
  activeA <-
    checkedIO
      "active A before drain"
      (activeOutgoingItems fixtureOutgoingA attemptingB)
  activeB <- checkedIO "active B before drain" (activeOutgoingItems outgoingB attemptingB)
  watermarksA <- checkedIO "watermarks A before drain" (outgoingWatermarks fixtureOutgoingA attemptingB)
  watermarksB <- checkedIO "watermarks B before drain" (outgoingWatermarks outgoingB attemptingB)
  awaitingBeforeA <-
    checkedIO
      "awaiting A before drain"
      (dispatchAwaitingSequences fixtureOutgoingA attemptingB)
  cut <- checkedIO "prepare dispatch drain cut" (prepareDispatchDrainCut attemptingB)
  assertEqual
    "the cut reports every previously bound direction canonically"
    [fixtureOutgoingA, outgoingB]
    (preparedDispatchDrainCutDirections cut)
  let (draining, committedDirections) = commitDispatchDrainCut cut
  assertEqual
    "commit exposes the prepared cut"
    (preparedDispatchDrainCutDirections cut)
    committedDirections
  mapM_
    ( \direction -> do
        assertEqual
          "drain clears binding"
          Nothing
          =<< checkedIO "binding after drain" (currentDispatchBinding direction draining)
        assertEqual
          "drain clears ticket"
          Nothing
          =<< checkedIO "ticket after drain" (currentDispatchTicket direction draining)
        case direction == outgoingB of
          True ->
            assertEqual
              "drain retains the one handed attempt correlation"
              (Just attemptB)
              =<< checkedIO "attempt after drain" (currentDispatchAttempt direction draining)
          False ->
            assertEqual
              "an already settled direction has no current attempt"
              Nothing
              =<< checkedIO "attempt after drain" (currentDispatchAttempt direction draining)
    )
    [fixtureOutgoingA, outgoingB]
  assertEqual
    "drain preserves exact A payload"
    activeA
    =<< checkedIO "active A after drain" (activeOutgoingItems fixtureOutgoingA draining)
  assertEqual
    "drain preserves exact B payload"
    activeB
    =<< checkedIO "active B after drain" (activeOutgoingItems outgoingB draining)
  assertEqual
    "drain preserves A watermarks"
    watermarksA
    =<< checkedIO "watermarks A after drain" (outgoingWatermarks fixtureOutgoingA draining)
  assertEqual
    "drain preserves B watermarks"
    watermarksB
    =<< checkedIO "watermarks B after drain" (outgoingWatermarks outgoingB draining)
  assertEqual
    "drain preserves written protocol wait"
    awaitingBeforeA
    =<< checkedIO "awaiting A after drain" (dispatchAwaitingSequences fixtureOutgoingA draining)
  lateA <-
    checkedIO
      "late written contradiction after drain"
      (prepareDispatchOutcome attemptA PeerDispatchFailed draining)
  assertEqual
    "a handed written attempt is stale after the cut"
    draining
    (fst (commitDispatchOutcome lateA))
  lateB <-
    checkedIO
      "late current attempt after drain"
      (prepareDrainingDispatchOutcome attemptB PeerDispatchDeferred draining)
  let (settledDraining, wasSettled) = commitDrainingDispatchOutcome lateB
  assertEqual "the handed current attempt settles during drain" True wasSettled
  assertEqual
    "settlement clears the handed attempt without scheduling replacement"
    Nothing
    =<< checkedIO "attempt after draining settlement" (currentDispatchAttempt outgoingB settledDraining)
  assertEqual
    "draining settlement creates no ticket"
    Nothing
    =<< checkedIO "ticket after draining settlement" (currentDispatchTicket outgoingB settledDraining)
  repeatCut <- checkedIO "repeat dispatch drain cut" (prepareDispatchDrainCut settledDraining)
  assertEqual
    "a repeated cut reports no newly unbound directions"
    []
    (preparedDispatchDrainCutDirections repeatCut)
  assertEqual
    "a repeated cut is state-identical"
    settledDraining
    (fst (commitDispatchDrainCut repeatCut))
  assertOwnerInvariant settledDraining

caseLocalRetirementCut :: Assertion
caseLocalRetirementCut = do
  let bindingA = fixtureBinding 1
      bindingB = fixtureBinding 9
      outgoingB = fixtureOutgoingDirection FixturePeerB
      initial = initialState fixtureLocal :: State Payload
  bindA <-
    checkedIO
      "bind local-retirement direction A"
      (prepareDispatchBinding fixtureOutgoingA bindingA initial)
  let afterBindA = fst (commitDispatchBinding bindA)
  bindB <-
    checkedIO
      "bind local-retirement direction B"
      (prepareDispatchBinding outgoingB bindingB afterBindA)
  let bound = fst (commitDispatchBinding bindB)
  enqueue <-
    checkedIO
      "enqueue local-retirement work"
      ( prepareEnqueue
          ( Map.fromList
              [ (fixturePeerA, fixtureItem 21 :| []),
                (fixturePeerB, fixtureItem 22 :| [])
              ]
          )
          bound
      )
  ticketB <-
    uniqueTicketFor
      outgoingB
      (preparedEnqueueDispatchTickets enqueue)
  let ready = fst (commitEnqueue enqueue)
  (attempting, attemptB) <- selectDispatch bindingB ticketB ready
  retainedA <-
    checkedIO
      "local-retirement retained A"
      (activeOutgoingItems fixtureOutgoingA attempting)
  retainedB <-
    checkedIO
      "local-retirement retained B"
      (activeOutgoingItems outgoingB attempting)
  cut <-
    checkedIO
      "prepare local retirement cut"
      (prepareLocalRetirementCut attempting)
  assertEqual
    "the terminal cut reports every attached direction canonically"
    [fixtureOutgoingA, outgoingB]
    (preparedLocalRetirementCutDirections cut)
  let (retiredLocal, committedDirections) = commitLocalRetirementCut cut
  assertEqual
    "commit exposes the prepared local-retirement cut"
    (preparedLocalRetirementCutDirections cut)
    committedDirections
  mapM_
    ( \direction -> do
        assertEqual
          "local retirement clears binding"
          Nothing
          =<< checkedIO "binding after local retirement" (currentDispatchBinding direction retiredLocal)
        assertEqual
          "local retirement clears ticket"
          Nothing
          =<< checkedIO "ticket after local retirement" (currentDispatchTicket direction retiredLocal)
        assertEqual
          "local retirement clears handed attempt"
          Nothing
          =<< checkedIO "attempt after local retirement" (currentDispatchAttempt direction retiredLocal)
    )
    [fixtureOutgoingA, outgoingB]
  assertEqual
    "local retirement retains exact A stream evidence"
    retainedA
    =<< checkedIO "retained A after local retirement" (activeOutgoingItems fixtureOutgoingA retiredLocal)
  assertEqual
    "local retirement retains exact B stream evidence"
    retainedB
    =<< checkedIO "retained B after local retirement" (activeOutgoingItems outgoingB retiredLocal)
  assertEqual "survivor A is not tombstoned" False (peerStreamPeerRetired fixturePeerA retiredLocal)
  assertEqual "survivor B is not tombstoned" False (peerStreamPeerRetired fixturePeerB retiredLocal)
  late <-
    checkedIO
      "late local-retirement dispatch outcome"
      (prepareDispatchOutcome attemptB PeerDispatchWritten retiredLocal)
  assertEqual
    "a late handed outcome cannot reactivate the retired local epoch"
    retiredLocal
    (fst (commitDispatchOutcome late))
  repeated <-
    checkedIO
      "repeat local retirement cut"
      (prepareLocalRetirementCut retiredLocal)
  assertEqual
    "a repeated terminal cut retains the complete cancellation set"
    [fixtureOutgoingA, outgoingB]
    (preparedLocalRetirementCutDirections repeated)
  assertEqual
    "a repeated terminal cut is state-identical"
    retiredLocal
    (fst (commitLocalRetirementCut repeated))
  assertOwnerInvariant retiredLocal

casePeerRetirement :: Assertion
casePeerRetirement = do
  let binding = fixtureBinding 1
  (ready, ticket, _) <- dispatchFixture binding [31, 32]
  (attempting, attempt) <- selectDispatch binding ticket ready
  pendingCandidate <-
    checkedIO
      "contiguous pending input before peer retirement"
      (prepareReceiveCandidate (inboundItem 1) attempting)
  withPending <-
    commitCandidate
      (Map.singleton sequence1 ReceivedPending)
      pendingCandidate
  gapCandidate <-
    checkedIO
      "noncontiguous input before peer retirement"
      (prepareReceiveCandidate (inboundItem 3) withPending)
  withIncomingGap <- commitCandidate Map.empty gapCandidate
  assertEqual
    "the fixture has one admitted item and one pre-admission gap item"
    [IncomingReceivedPending, GapRetained]
    . fmap snd
    =<< checkedIO
      "incoming work before peer retirement"
      (incomingRetainedItems fixtureIncomingA withIncomingGap)
  retainedBefore <-
    checkedIO
      "retained work before retirement"
      (activeOutgoingItems fixtureOutgoingA withIncomingGap)
  retirement <-
    checkedIO
      "prepare peer retirement"
      (preparePeerRetirement fixturePeerA withIncomingGap)
  assertEqual
    "the first retirement names the detached direction"
    (Just fixtureOutgoingA)
    (preparedPeerRetirementDirection retirement)
  assertEqual
    "the first retirement truthfully settles every still-active assignment"
    ( fmap
        ( \item ->
            ( sequencedItemSequence item,
              sequencedItemDigest item
            )
        )
        retainedBefore
    )
    ( fmap
        ( \receipt ->
            ( assignmentReceiptSequence receipt,
              assignmentReceiptDigest receipt
            )
        )
        (preparedPeerRetirementSettledAssignments retirement)
    )
  let (retired, committedDirection) = commitPeerRetirement retirement
  assertEqual "commit exposes the prepared direction" (Just fixtureOutgoingA) committedDirection
  assertEqual "peer has a permanent tombstone" True (peerStreamPeerRetired fixturePeerA retired)
  assertEqual "binding is cleared" Nothing =<< checkedIO "retired binding" (currentDispatchBinding fixtureOutgoingA retired)
  assertEqual "ticket is cleared" Nothing =<< checkedIO "retired ticket" (currentDispatchTicket fixtureOutgoingA retired)
  assertEqual "attempt is cleared" Nothing =<< checkedIO "retired attempt" (currentDispatchAttempt fixtureOutgoingA retired)
  assertEqual
    "retired assignments are no longer active"
    []
    =<< checkedIO "retired retained work" (activeOutgoingItems fixtureOutgoingA retired)
  assertEqual
    "terminal retirement evidence remains distinct from peer completion"
    mempty
    =<< checkedIO "peer completion after retirement" (outgoingCompletionProgress fixtureOutgoingA retired)
  forM_ (preparedPeerRetirementSettledAssignments retirement) $ \receipt ->
    assertEqual "compact retirement classifies each released exact assignment" (Right True) (assignmentRetirementKnown receipt retired)
  assertEqual "no outgoing payload or terminal receipt history remains" 0 (let (outgoing, _, receipts) = peerStreamRetentionCounts retired in outgoing + receipts)
  assertEqual
    "retirement preserves contiguous semantic input and drops only the unreachable gap suffix"
    [(Payload 1, IncomingReceivedPending)]
    . fmap (\(item, status) -> (sequencedItemPayload item, status))
    =<< checkedIO
      "incoming work after peer retirement"
      (incomingRetainedItems fixtureIncomingA retired)
  assertEqual
    "the retired source leaves no impossible incoming gap"
    []
    . gapSummaryEntries
    =<< checkedIO
      "incoming gap after peer retirement"
      (incomingGapSummary fixtureIncomingA retired)
  duplicate <- checkedIO "duplicate peer retirement" (preparePeerRetirement fixturePeerA retired)
  assertEqual "duplicate emits no new detachment" Nothing (preparedPeerRetirementDirection duplicate)
  assertEqual
    "duplicate emits no new terminal settlements"
    []
    (preparedPeerRetirementSettledAssignments duplicate)
  assertEqual "duplicate is state-identical" retired (fst (commitPeerRetirement duplicate))
  assertProblem
    "retired binding"
    (PeerStreamProtocolProblem (PeerStreamRetiredPeer fixturePeerA))
    (prepareDispatchBinding fixtureOutgoingA (fixtureBinding 2) retired)
  assertProblem
    "retired scheduling"
    (PeerStreamProtocolProblem (PeerStreamRetiredPeer fixturePeerA))
    (prepareEnsureDispatchTicket fixtureOutgoingA retired)
  assertProblem
    "retired stale selection"
    (PeerStreamProtocolProblem (PeerStreamRetiredPeer fixturePeerA))
    (prepareDispatchSelection ticket binding retired)
  assertProblem
    "retired enqueue"
    (PeerStreamProtocolProblem (PeerStreamRetiredPeer fixturePeerA))
    (prepareEnqueue (Map.singleton fixturePeerA (fixtureItem 33 :| [])) retired)
  assertProblem
    "retired resume offer"
    (PeerStreamProtocolProblem (PeerStreamRetiredPeer fixturePeerA))
    (resumeOfferForPeer fixturePeerA retired)
  stale <- checkedIO "stale retired outcome" (prepareDispatchOutcome attempt PeerDispatchWritten retired)
  assertEqual "stale outcome cannot reactivate retired work" retired (fst (commitDispatchOutcome stale))
  lateReceived <- checkedIO "late retired received acknowledgement" (prepareReceivedAck fixtureOutgoingA (prefix 2) retired)
  assertEqual "late received acknowledgement is terminally stale" retired (fst (commitReceivedAck lateReceived))
  lateCompleted <- checkedIO "late retired completion acknowledgement" (prepareCompletedAck fixtureOutgoingA (prefix 2) retired)
  assertEqual "late completion acknowledgement is terminally stale" retired (fst (commitCompletedAck lateCompleted))
  let lateIncoming =
        sequencedItem
          (reverseStreamDirection fixtureOutgoingA)
          firstStreamSequence
          (fixtureDigest 41)
          (Payload 41)
  assertProblem
    "retired late incoming batch"
    (PeerStreamProtocolProblem (PeerStreamRetiredPeer fixturePeerA))
    (prepareReceiveCandidate lateIncoming retired)
  assertProblem
    "retired late source frontier"
    (PeerStreamProtocolProblem (PeerStreamRetiredPeer fixturePeerA))
    (prepareFrontierAdvance (reverseStreamDirection fixtureOutgoingA) firstStreamSequence retired)
  assertOwnerInvariant retired

casePeerRetirementPartition :: Assertion
casePeerRetirementPartition = do
  let binding = fixtureBinding 1
  (ready, _, _) <- dispatchFixture binding [31, 32]
  completed <-
    checkedIO
      "complete the first assignment before peer retirement"
      (prepareCompletedAck fixtureOutgoingA (prefix 1) ready)
  let withCompletedPrefix = fst (commitCompletedAck completed)
  completedBefore <-
    checkedIO
      "completed prefix before retirement"
      (outgoingCompletionProgress fixtureOutgoingA withCompletedPrefix)
  activeBefore <-
    checkedIO
      "active suffix before retirement"
      (activeOutgoingItems fixtureOutgoingA withCompletedPrefix)
  retirement <-
    checkedIO
      "retire peer with a mixed completed and active partition"
      (preparePeerRetirement fixturePeerA withCompletedPrefix)
  let retired = fst (commitPeerRetirement retirement)
  assertEqual
    "the completed prefix remains peer-completed truth"
    completedBefore
    =<< checkedIO
      "completed prefix after retirement"
      (outgoingCompletionProgress fixtureOutgoingA retired)
  assertEqual
    "only the formerly active suffix becomes retirement-settled"
    (fmap sequencedItemSequence activeBefore)
    (fmap assignmentReceiptSequence (preparedPeerRetirementSettledAssignments retirement))
  forM_ (preparedPeerRetirementSettledAssignments retirement) $ \receipt -> do
    assertEqual "retired suffix is compactly settled" (Right True) (assignmentRetirementKnown receipt retired)
    assertEqual "retired suffix is never falsely peer-completed" (Right False) (assignmentCompletionKnown receipt retired)
  assertOwnerInvariant retired

caseDispatchAcknowledgementAndResume :: Assertion
caseDispatchAcknowledgementAndResume = do
  let binding = fixtureBinding 1
  (ready, ticket, _) <- dispatchFixture binding [1]
  (attempting, attempt) <- selectDispatch binding ticket ready
  received <-
    checkedIO
      "received acknowledgement supersedes attempt"
      (prepareReceivedAck fixtureOutgoingA (prefix 1) attempting)
  let afterReceived = fst (commitReceivedAck received)
  assertEqual
    "received progress clears the physical attempt"
    Nothing
    =<< checkedIO "attempt after received" (currentDispatchAttempt fixtureOutgoingA afterReceived)
  assertEqual
    "received progress suppresses routine resend"
    (Set.singleton sequence1)
    =<< checkedIO "awaiting after received" (dispatchAwaitingSequences fixtureOutgoingA afterReceived)
  staleAfterReceived <-
    checkedIO
      "late outcome after received"
      (prepareDispatchOutcome attempt PeerDispatchWritten afterReceived)
  assertEqual
    "late outcome after received is stale"
    afterReceived
    (fst (commitDispatchOutcome staleAfterReceived))
  completed <-
    checkedIO
      "completed acknowledgement releases item"
      (prepareCompletedAck fixtureOutgoingA (prefix 1) afterReceived)
  let afterCompleted = fst (commitCompletedAck completed)
  assertEqual
    "completion removes dispatch suppression with released payload"
    Set.empty
    =<< checkedIO "awaiting after completion" (dispatchAwaitingSequences fixtureOutgoingA afterCompleted)
  assertEqual
    "completion releases the active payload"
    []
    =<< checkedIO "active after completion" (activeOutgoingItems fixtureOutgoingA afterCompleted)

  (resumeReady, resumeTicket, assigned) <- dispatchFixture binding [9]
  (resumeAttempting, resumeAttempt) <- selectDispatch binding resumeTicket resumeReady
  written <-
    checkedIO
      "written before resume"
      (prepareDispatchOutcome resumeAttempt PeerDispatchWritten resumeAttempting)
  let awaitingResume = fst (commitDispatchOutcome written)
  offer <- emptyResumeOffer fixturePeerA fixtureLocal sequence1
  rebound <- checkedIO "replacement binding retains the old written suffix" (prepareDispatchBinding fixtureOutgoingA (fixtureBinding 2) awaitingResume)
  let afterRebind = fst (commitDispatchBinding rebound)
  resume <- checkedIO "resume re-enables exact item" (prepareResume offer afterRebind)
  assertEqual
    "resume retains the exact typed retransmission"
    (NonEmpty.toList assigned)
    (preparedResumeRetransmissions resume)
  resumeDispatchTicket <-
    requiredMaybe
      "resume omitted dispatch ticket"
      (preparedResumeDispatchTicket resume)
  let resumed = let (successor, _, _) = commitResume resume in successor
  assertEqual
    "resume clears old write suppression"
    Set.empty
    =<< checkedIO "awaiting after resume" (dispatchAwaitingSequences fixtureOutgoingA resumed)
  (resumedAttempting, resumedAttempt) <-
    selectDispatch (fixtureBinding 2) resumeDispatchTicket resumed
  assertEqual
    "resume selects the same exact retained item"
    (peerDispatchAttemptItem resumeAttempt)
    (peerDispatchAttemptItem resumedAttempt)
  resumedWritten <-
    checkedIO
      "write reactivated resume attempt"
      ( prepareDispatchOutcome
          resumedAttempt
          PeerDispatchWritten
          resumedAttempting
      )
  let awaitingRepeatedOffer = fst (commitDispatchOutcome resumedWritten)
  repeatedCurrentAssociation <-
    checkedIO
      "repeat non-advancing resume on the same association"
      ( prepareResumeForAssociation
          ContinuingResumeOnAssociation
          offer
          awaitingRepeatedOffer
      )
  assertEqual
    "same-association repeated evidence schedules no retransmission"
    []
    (preparedResumeRetransmissions repeatedCurrentAssociation)
  assertEqual
    "same-association repeated evidence mints no dispatch ticket"
    Nothing
    (preparedResumeDispatchTicket repeatedCurrentAssociation)
  let repeatedSuccessor =
        let (successor, _, _) = commitResume repeatedCurrentAssociation
         in successor
  assertEqual
    "same-association repeated evidence preserves write suppression"
    (Set.singleton sequence1)
    =<< checkedIO
      "awaiting after repeated same-association resume"
      (dispatchAwaitingSequences fixtureOutgoingA repeatedSuccessor)
  assertOwnerInvariant repeatedSuccessor

  (gapReady, gapTicket1, gapAssigned) <- dispatchFixture binding [10, 11]
  (gapAttempting1, gapAttempt1) <- selectDispatch binding gapTicket1 gapReady
  gapWritten <-
    checkedIO
      "write before peer gap confirmation"
      (prepareDispatchOutcome gapAttempt1 PeerDispatchWritten gapAttempting1)
  gapTicket2 <-
    requiredMaybe
      "written first item did not schedule the second"
      (preparedDispatchOutcomeTicket gapWritten)
  let gapAfterWritten = fst (commitDispatchOutcome gapWritten)
  (gapAttempting2, gapAttempt2) <-
    selectDispatch binding gapTicket2 gapAfterWritten
  let gapItems = NonEmpty.toList gapAssigned
      gapItem2 = gapItems !! 1
  confirmedGap <-
    checkedIO
      "selected item in resume gap"
      ( mkGapSummary
          fixtureOutgoingA
          emptyStreamPrefix
          [(sequence2, sequencedItemDigest gapItem2)]
      )
  confirmedOffer <-
    checkedIO
      "resume confirming selected item"
      ( mkResumeOffer
          fixturePeerA
          fixtureLocal
          sequence1
          emptyStreamPrefix
          emptyStreamPrefix
          confirmedGap
      )
  confirmedResume <-
    checkedIO
      "resume supersedes selected peer gap"
      ( prepareResumeForAssociation
          ContinuingResumeOnAssociation
          confirmedOffer
          gapAttempting2
      )
  assertEqual
    "the peer-confirmed gap is not retransmitted"
    (take 1 gapItems)
    (preparedResumeRetransmissions confirmedResume)
  let gapResumed = let (successor, _, _) = commitResume confirmedResume in successor
  assertEqual
    "resume supersedes an attempt for an item the peer already retained"
    Nothing
    =<< checkedIO
      "attempt after peer gap confirmation"
      (currentDispatchAttempt fixtureOutgoingA gapResumed)
  staleGapOutcome <-
    checkedIO
      "late outcome after peer gap confirmation"
      (prepareDispatchOutcome gapAttempt2 PeerDispatchWritten gapResumed)
  assertEqual
    "the superseded gap attempt has a stale physical outcome"
    gapResumed
    (fst (commitDispatchOutcome staleGapOutcome))
  assertOwnerInvariant gapResumed

data FirstResumeOrder = ResumeBeforeSelection | ResumeDuringAttempt | ResumeAfterWrites
  deriving stock (Eq, Show)

-- A physical write can finish before its callback is delivered. Closing that
-- binding wins Deferred, so replacement dispatch can precede its first Resume.
-- Two replacement writes ensure that remembering only the latest outcome would
-- not suffice to preserve the current association's suppression.
caseFirstResumeDispatch :: FirstResumeOrder -> Assertion
caseFirstResumeDispatch order = do
  let oldBinding = fixtureBinding 1
      replacementBinding = fixtureBinding 2
  (ready, firstTicket, assigned) <- dispatchFixture oldBinding [41, 42]
  (oldAttempting, oldAttempt) <- selectDispatch oldBinding firstTicket ready
  deferred <- checkedIO "close wins the old physical outcome" (prepareDispatchOutcome oldAttempt PeerDispatchDeferred oldAttempting)
  let afterDeferred = fst (commitDispatchOutcome deferred)
  lost <- checkedIO "lose the exact old binding" (prepareDispatchBindingLoss fixtureOutgoingA oldBinding afterDeferred)
  let unbound = fst (commitDispatchBindingLoss lost)
  rebound <- checkedIO "admit replacement association" (prepareDispatchBinding fixtureOutgoingA replacementBinding unbound)
  let replacement = fst (commitDispatchBinding rebound)
      items = NonEmpty.toList assigned
  offer <- emptyResumeOffer fixturePeerA fixtureLocal sequence1
  settled <- case order of
    ResumeBeforeSelection -> do
      resumed <- firstResume offer replacement
      firstWritten <- writeNext replacementBinding resumed
      writeNext replacementBinding firstWritten
    ResumeDuringAttempt -> do
      ticket <- requiredMaybe "replacement ticket" =<< checkedIO "replacement ticket" (currentDispatchTicket fixtureOutgoingA replacement)
      (attempting, attempt) <- selectDispatch replacementBinding ticket replacement
      resumed <- firstResume offer attempting
      assertEqual "first Resume preserves the exact replacement attempt" (Just attempt) =<< checkedIO "current replacement attempt" (currentDispatchAttempt fixtureOutgoingA resumed)
      written <- checkedIO "complete the preserved attempt" (prepareDispatchOutcome attempt PeerDispatchWritten resumed)
      writeNext replacementBinding (fst (commitDispatchOutcome written))
    ResumeAfterWrites -> do
      firstWritten <- writeNext replacementBinding replacement
      bothWritten <- writeNext replacementBinding firstWritten
      firstResume offer bothWritten
  assertEqual "both exact items are awaiting peer progress" (Set.fromList [sequence1, sequence2]) =<< checkedIO "replacement suppression" (dispatchAwaitingSequences fixtureOutgoingA settled)
  assertEqual "first Resume allocates no third replacement dispatch" Nothing =<< checkedIO "settled ticket" (currentDispatchTicket fixtureOutgoingA settled)
  duplicate <- checkedIO "duplicate Resume on current association" (prepareResumeForAssociation ContinuingResumeOnAssociation offer settled)
  assertEqual "duplicate Resume requests no second replay" [] (preparedResumeRetransmissions duplicate)
  let (afterDuplicate, _, _) = commitResume duplicate
  assertEqual "duplicate Resume preserves the full owner state" settled afterDuplicate

  -- Existing canonical gap semantics permit a later raw claim to request a
  -- sequence which the preceding claim had confirmed. That is fresh repair
  -- evidence and must still override this binding's Written suppression.
  second <- requiredMaybe "second retained item" (case items of [_first, item] -> Just item; _ -> Nothing)
  gap <- checkedIO "peer confirms the second item" (mkGapSummary fixtureOutgoingA emptyStreamPrefix [(sequence2, sequencedItemDigest second)])
  confirmedOffer <- checkedIO "gap offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 emptyStreamPrefix emptyStreamPrefix gap)
  confirmed <- checkedIO "retain later gap evidence" (prepareResumeForAssociation ContinuingResumeOnAssociation confirmedOffer settled)
  let (afterConfirmed, _, _) = commitResume confirmed
  repair <- checkedIO "new raw gap evidence requests the second item" (prepareResumeForAssociation ContinuingResumeOnAssociation offer afterConfirmed)
  assertEqual "later changed-gap evidence still repairs its exact newly requested item" [second] (preparedResumeRetransmissions repair)
  let (repairReady, _, _) = commitResume repair
  repairTicket <- requiredMaybe "later repair ticket" (preparedResumeDispatchTicket repair)
  (repairAttempting, repairAttempt) <- selectDispatch replacementBinding repairTicket repairReady
  assertEqual "later repair keeps the original sequence, digest, and payload" second (peerDispatchAttemptItem repairAttempt)
  repairWritten <- checkedIO "settle legitimate later repair" (prepareDispatchOutcome repairAttempt PeerDispatchWritten repairAttempting)
  let afterRepair = fst (commitDispatchOutcome repairWritten)
  repeatedRepair <- checkedIO "repeat the same repair claim" (prepareResumeForAssociation ContinuingResumeOnAssociation offer afterRepair)
  assertEqual "equal later repair evidence emits no retry" [] (preparedResumeRetransmissions repeatedRepair)
  assertEqual "equal later repair evidence is state inert" afterRepair (let (state, _, _) = commitResume repeatedRepair in state)
  assertOwnerInvariant afterRepair
  where
    firstResume offer state = do
      prepared <- checkedIO "first Resume on replacement association" (prepareResumeForAssociation FirstResumeOnAssociation offer state)
      assertEqual
        "first Resume only reactivates items not already attempted on this binding"
        (case order of ResumeBeforeSelection -> [sequence1, sequence2]; ResumeDuringAttempt -> [sequence2]; ResumeAfterWrites -> [])
        (fmap sequencedItemSequence (preparedResumeRetransmissions prepared))
      let (successor, _, _) = commitResume prepared
      assertOwnerInvariant successor
      pure successor
    writeNext binding state = do
      ticket <- requiredMaybe "next exact replacement ticket" =<< checkedIO "next replacement ticket" (currentDispatchTicket fixtureOutgoingA state)
      (attempting, attempt) <- selectDispatch binding ticket state
      prepared <- checkedIO "successful replacement write" (prepareDispatchOutcome attempt PeerDispatchWritten attempting)
      let successor = fst (commitDispatchOutcome prepared)
      assertOwnerInvariant successor
      pure successor

caseFirstResumeReceipt :: Assertion
caseFirstResumeReceipt = do
  let oldBinding = fixtureBinding 1
      replacementBinding = fixtureBinding 2
  (ready, firstTicket, _) <- dispatchFixture oldBinding [61, 62]
  (firstAttempting, firstAttempt) <- selectDispatch oldBinding firstTicket ready
  firstWritten <- checkedIO "first old write succeeds without its peer receipt" (prepareDispatchOutcome firstAttempt PeerDispatchWritten firstAttempting)
  let afterFirst = fst (commitDispatchOutcome firstWritten)
  secondTicket <- requiredMaybe "second old ticket" =<< checkedIO "second old ticket" (currentDispatchTicket fixtureOutgoingA afterFirst)
  (secondAttempting, secondAttempt) <- selectDispatch oldBinding secondTicket afterFirst
  -- Both bytes reached the peer; closing the old socket wins the second
  -- callback before Written, and its receipt has not reached the source.
  deferred <- checkedIO "close wins second old callback" (prepareDispatchOutcome secondAttempt PeerDispatchDeferred secondAttempting)
  lost <- checkedIO "lose old association" (prepareDispatchBindingLoss fixtureOutgoingA oldBinding (fst (commitDispatchOutcome deferred)))
  rebound <- checkedIO "replacement association" (prepareDispatchBinding fixtureOutgoingA replacementBinding (fst (commitDispatchBindingLoss lost)))
  let replacement = fst (commitDispatchBinding rebound)
  ticket <- requiredMaybe "replacement ticket" =<< checkedIO "replacement ticket" (currentDispatchTicket fixtureOutgoingA replacement)
  (attempting, attempt) <- selectDispatch replacementBinding ticket replacement
  assertEqual "the unresolved second write is selected for repair" sequence2 (sequencedItemSequence (peerDispatchAttemptItem attempt))
  gap <- checkedIO "peer receipt without completion" (mkGapSummary fixtureOutgoingA (prefix 2) [])
  offer <- checkedIO "first Resume confirms both old deliveries" (mkResumeOffer fixturePeerA fixtureLocal sequence1 (prefix 2) emptyStreamPrefix gap)
  resumed <- checkedIO "receipt arrives before replacement Written" (prepareResumeForAssociation FirstResumeOnAssociation offer attempting)
  let (afterResume, _, _) = commitResume resumed
  assertEqual "receipt requests no retransmission" [] (preparedResumeRetransmissions resumed)
  assertEqual "receipt supersedes the exact replacement callback" Nothing =<< checkedIO "attempt after receipt" (currentDispatchAttempt fixtureOutgoingA afterResume)
  assertEqual "every newly confirmed active item is suppressed" (Set.fromList [sequence1, sequence2]) =<< checkedIO "confirmed receipt suppression" (dispatchAwaitingSequences fixtureOutgoingA afterResume)
  assertEqual "no already-received item gets another ticket" Nothing =<< checkedIO "ticket after receipt" (currentDispatchTicket fixtureOutgoingA afterResume)
  forM_ [PeerDispatchWritten, PeerDispatchDeferred] $ \outcome -> do
    stale <- checkedIO "superseded replacement callback" (prepareDispatchOutcome attempt outcome afterResume)
    assertEqual "the superseded callback cannot remove peer receipt truth" afterResume (fst (commitDispatchOutcome stale))
  repeated <- checkedIO "equal first receipt on established association" (prepareResumeForAssociation ContinuingResumeOnAssociation offer afterResume)
  assertEqual "equal receipt is state inert" afterResume (let (state, _, _) = commitResume repeated in state)
  lower <- emptyResumeOffer fixturePeerA fixtureLocal sequence1
  repaired <- checkedIO "later raw gap evidence requests the exact retained items" (prepareResumeForAssociation ContinuingResumeOnAssociation lower afterResume)
  assertEqual "fresh raw repair evidence still overrides receipt suppression" [sequence1, sequence2] (fmap sequencedItemSequence (preparedResumeRetransmissions repaired))
  assertOwnerInvariant afterResume
  assertOwnerInvariant (let (state, _, _) = commitResume repaired in state)

caseNormalizedGapReceipt :: Assertion
caseNormalizedGapReceipt = forM_ [prefix 1, prefix 2] $ \received -> do
  let binding = fixtureBinding 1
  (ready, _, assigned) <- dispatchFixture binding [71, 72]
  firstWritten <- writeNext binding ready
  bothWritten <- writeNext binding firstWritten
  acknowledged <- checkedIO "prior monotone receipt" (prepareReceivedAck fixtureOutgoingA received bothWritten)
  reoffered <- checkedIO "same-binding association reoffer" (prepareDispatchBinding fixtureOutgoingA binding (fst (commitReceivedAck acknowledged)))
  lower <- emptyResumeOffer fixturePeerA fixtureLocal sequence1
  repair <- checkedIO "raw prefix regression requests both old items" (prepareResumeForAssociation FirstResumeOnAssociation lower (fst (commitDispatchBinding reoffered)))
  assertEqual "both exact old items require repair" [sequence1, sequence2] (fmap sequencedItemSequence (preparedResumeRetransmissions repair))
  let (repairReady, _, _) = commitResume repair
  repairedFirst <- writeNext binding repairReady
  ticket <- requiredMaybe "second repair ticket" =<< checkedIO "second repair ticket" (currentDispatchTicket fixtureOutgoingA repairedFirst)
  (attempting, attempt) <- selectDispatch binding ticket repairedFirst
  let second = case NonEmpty.toList assigned of
        [_first, item] -> Just item
        _ -> Nothing
  retainedSecond <- requiredMaybe "second exact item" second
  gap <- checkedIO "raw gap confirms the current repair" (mkGapSummary fixtureOutgoingA emptyStreamPrefix [(sequence2, sequencedItemDigest retainedSecond)])
  offer <- checkedIO "gap receipt before repair Written" (mkResumeOffer fixturePeerA fixtureLocal sequence1 emptyStreamPrefix emptyStreamPrefix gap)
  confirmed <- checkedIO "confirmed gap is normalized away" (prepareResumeForAssociation ContinuingResumeOnAssociation offer attempting)
  let (afterGap, _, _) = commitResume confirmed
  assertEqual "the normalized gap cannot retain this raw receipt" [] . gapSummaryEntries =<< checkedIO "normalized gap" (outgoingPeerGapSummary fixtureOutgoingA afterGap)
  assertEqual "raw gap supersedes the exact repair attempt" Nothing =<< checkedIO "attempt after raw receipt" (currentDispatchAttempt fixtureOutgoingA afterGap)
  assertEqual "confirmed raw receipt keeps both items suppressed" (Set.fromList [sequence1, sequence2]) =<< checkedIO "raw receipt suppression" (dispatchAwaitingSequences fixtureOutgoingA afterGap)
  assertEqual "the confirmed item is not immediately rescheduled" Nothing =<< checkedIO "ticket after raw receipt" (currentDispatchTicket fixtureOutgoingA afterGap)
  stale <- checkedIO "late Written after raw gap receipt" (prepareDispatchOutcome attempt PeerDispatchWritten afterGap)
  assertEqual "late callback preserves raw receipt truth" afterGap (fst (commitDispatchOutcome stale))
  duplicate <- checkedIO "equal raw receipt" (prepareResumeForAssociation ContinuingResumeOnAssociation offer afterGap)
  assertEqual "equal raw receipt is inert" afterGap (let (state, _, _) = commitResume duplicate in state)
  removed <- checkedIO "later raw gap removal" (prepareResumeForAssociation ContinuingResumeOnAssociation lower afterGap)
  assertEqual "removing the raw receipt repairs only its newly requested item" [retainedSecond] (preparedResumeRetransmissions removed)
  assertOwnerInvariant afterGap
  assertOwnerInvariant (let (state, _, _) = commitResume removed in state)
  where
    writeNext binding state = do
      ticket <- requiredMaybe "write ticket" =<< checkedIO "write ticket" (currentDispatchTicket fixtureOutgoingA state)
      (attempting, attempt) <- selectDispatch binding ticket state
      written <- checkedIO "successful write" (prepareDispatchOutcome attempt PeerDispatchWritten attempting)
      pure (fst (commitDispatchOutcome written))

caseReofferedHelloResume :: Assertion
caseReofferedHelloResume = do
  let binding = fixtureBinding 1
  (ready, _, _) <- dispatchFixture binding [51, 52]
  firstWritten <- writeNext ready
  bothWritten <- writeNext firstWritten
  reoffered <- checkedIO "same-binding accepted Hello reoffer" (prepareDispatchBinding fixtureOutgoingA binding bothWritten)
  let afterHello = fst (commitDispatchBinding reoffered)
  (withFreshItem, _) <- enqueuePayloads fixturePeerA [53] afterHello
  freshWritten <- writeNext withFreshItem
  offer <- emptyResumeOffer fixturePeerA fixtureLocal sequence1
  resumed <- checkedIO "first Resume after same-binding Hello reoffer" (prepareResumeForAssociation FirstResumeOnAssociation offer freshWritten)
  assertEqual
    "the reoffer repairs both pre-Hello writes but not the post-Hello write"
    [sequence1, sequence2]
    (fmap sequencedItemSequence (preparedResumeRetransmissions resumed))
  let (repairReady, _, _) = commitResume resumed
  repairedFirst <- writeNext repairReady
  repairedBoth <- writeNext repairedFirst
  assertEqual "the fresh item is not written twice" Nothing =<< checkedIO "post-reoffer dispatch ticket" (currentDispatchTicket fixtureOutgoingA repairedBoth)
  duplicate <- checkedIO "duplicate Resume after reoffer repair" (prepareResumeForAssociation ContinuingResumeOnAssociation offer repairedBoth)
  assertEqual "duplicate reoffer Resume preserves all settled repair" repairedBoth (let (state, _, _) = commitResume duplicate in state)
  assertEqual "duplicate reoffer Resume requests no further writes" [] (preparedResumeRetransmissions duplicate)
  assertOwnerInvariant repairedBoth
  where
    writeNext state = do
      ticket <- requiredMaybe "reoffer schedule ticket" =<< checkedIO "reoffer schedule ticket" (currentDispatchTicket fixtureOutgoingA state)
      (attempting, attempt) <- selectDispatch (fixtureBinding 1) ticket state
      prepared <- checkedIO "reoffer schedule write" (prepareDispatchOutcome attempt PeerDispatchWritten attempting)
      pure (fst (commitDispatchOutcome prepared))

propDispatchRetryTrace :: Property
propDispatchRetryTrace =
  forAll (listOf (elements [PeerDispatchDeferred, PeerDispatchFailed])) $ \outcomes ->
    ioProperty $ do
      observed <- runDispatchRetryTrace outcomes
      pure $ case observed of
        Left problem -> counterexample problem False
        Right attempt ->
          counterexample
            "attempt generation did not advance exactly once per selection"
            ( attemptGenerationWord64 attempt
                === fromIntegral (length outcomes + 1)
            )

caseMultiPeerEnqueue :: Assertion
caseMultiPeerEnqueue = do
  let predecessor = initialState fixtureLocal :: State Payload
      requests =
        Map.fromList
          [ (fixturePeerA, fixtureItem 10 :| [fixtureItem 11]),
            (fixturePeerB, fixtureItem 20 :| [])
          ]
  prepared <- checkedIO "multi-peer enqueue" (prepareEnqueue requests predecessor)
  let assigned = preparedEnqueueAssignments prepared
      frontierAdvances = preparedEnqueueFrontierAdvances prepared
      (successor, committed) = commitEnqueue prepared
  assertEqual "preparation and commit expose identical assignments" assigned committed
  assertEqual
    "peer A gets its own consecutive sequence space"
    [1, 2]
    (fmap (streamSequenceWord64 . sequencedItemSequence) (assignedFor fixturePeerA assigned))
  assertEqual
    "peer B independently begins at one"
    [1]
    (fmap (streamSequenceWord64 . sequencedItemSequence) (assignedFor fixturePeerB assigned))
  assertEqual
    "within-direction request order is retained"
    [Payload 10, Payload 11]
    (fmap sequencedItemPayload (assignedFor fixturePeerA assigned))
  assertEqual
    "the exact directions are assigned"
    [fixtureOutgoingA, fixtureOutgoingA]
    (fmap sequencedItemDirection (assignedFor fixturePeerA assigned))
  assertEqual
    "enqueue exposes each advanced exclusive source frontier"
    ( Map.fromList
        [ (fixturePeerA, (fixtureOutgoingA, sequence3)),
          (fixturePeerB, (fixtureOutgoingDirection FixturePeerB, sequence2))
        ]
    )
    frontierAdvances
  assertOwnerInvariant successor
  emptyPrepared <- checkedIO "empty enqueue" (prepareEnqueue Map.empty successor)
  assertEqual "empty enqueue has no assignment" Map.empty (preparedEnqueueAssignments emptyPrepared)
  assertEqual "empty enqueue has no frontier advertisement" Map.empty (preparedEnqueueFrontierAdvances emptyPrepared)
  assertEqual "empty enqueue is the identity" successor (fst (commitEnqueue emptyPrepared))
  assertProblem
    "self enqueue rejects the whole operation"
    (PeerStreamProtocolProblem (PeerStreamSelfDirection fixtureLocal))
    (prepareEnqueue (Map.singleton fixtureLocal (fixtureItem 99 :| [])) predecessor)
  assertEqual "failed enqueue left its predecessor empty" [] =<< checkedIO "active items" (activeOutgoingItems fixtureOutgoingA predecessor)

caseSequenceBoundEnqueue :: Assertion
caseSequenceBoundEnqueue = do
  let pristine = initialState fixtureLocal :: State Payload
      outgoingB = fixtureOutgoingDirection FixturePeerB
  seededPreparation <-
    checkedIO
      "seed sequence-bound direction"
      (prepareEnqueue (Map.singleton fixturePeerA (fixtureItem 9 :| [])) pristine)
  let seeded = fst (commitEnqueue seededPreparation)
      requests =
        Map.fromList
          [ ( fixturePeerA,
              sequenceBoundPeerItem sequenceBoundFixtureItem :| []
            ),
            ( fixturePeerB,
              sequenceBoundPeerItem sequenceBoundFixtureItem :| []
            )
          ]
      expectedA =
        sequencedItem
          fixtureOutgoingA
          sequence2
          (fixtureDigest 42)
          (Payload 42)
      expectedB =
        sequencedItem
          outgoingB
          sequence1
          (fixtureDigest 81)
          (Payload 81)
  prepared <-
    checkedIO
      "sequence-bound enqueue"
      (prepareSequenceBoundEnqueue requests seeded)
  let assignments = preparedEnqueueAssignments prepared
      (enqueued, committedAssignments) = commitEnqueue prepared
  assertEqual
    "the builder sees direction A and its already-advanced allocation"
    [expectedA]
    (assignedFor fixturePeerA assignments)
  assertEqual
    "the same builder sees direction B and its independent allocation"
    [expectedB]
    (assignedFor fixturePeerB assignments)
  assertEqual
    "commit retains the exact prepared assignments"
    assignments
    committedAssignments
  let freshDestination = initialState fixturePeerA :: State Payload
  reconnectOffer <-
    checkedIO
      "fresh destination reconnect offer"
      (resumeOfferForPeer fixtureLocal freshDestination)
  resume <-
    checkedIO
      "source reconnect reconciliation"
      (prepareResume reconnectOffer enqueued)
  assertEqual
    "resume retransmits the exact bound assignment after earlier work"
    [ sequencedItem fixtureOutgoingA sequence1 (fixtureDigest 9) (Payload 9),
      expectedA
    ]
    (preparedResumeRetransmissions resume)
  let (resumed, _, retransmissions) = commitResume resume
  assertEqual
    "resume commit exposes the exact prepared retransmissions"
    (preparedResumeRetransmissions resume)
    retransmissions
  assertEqual
    "reconnect leaves the direction/sequence-bound assignment retained"
    [ sequencedItem fixtureOutgoingA sequence1 (fixtureDigest 9) (Payload 9),
      expectedA
    ]
    =<< checkedIO "active sequence-bound items" (activeOutgoingItems fixtureOutgoingA resumed)
  assertOwnerInvariant resumed

caseReceiveGapDuplicateAndClosure :: Assertion
caseReceiveGapDuplicateAndClosure = do
  let predecessor = initialState fixtureLocal :: State Payload
      third = inboundItem 3
  gapCandidate <- checkedIO "third item gap" (prepareReceiveCandidate third predecessor)
  assertEqual "the repair cursor is the first missing item" (ReceiveGap sequence1) (preparedReceiveDisposition gapCandidate)
  assertEqual "a gap exposes no semantic work" [] (preparedContiguousItems gapCandidate)
  afterGap <- commitCandidate Map.empty gapCandidate
  gap <- checkedIO "incoming gap summary" (incomingGapSummary fixtureIncomingA afterGap)
  assertEqual "the retained gap is canonical" [(sequence3, fixtureDigest 3)] (gapSummaryEntries gap)
  duplicate <- checkedIO "exact duplicate" (prepareReceiveCandidate third afterGap)
  assertEqual "equal sequence and digest are idempotent" ReceiveDuplicate (preparedReceiveDisposition duplicate)
  afterDuplicate <- commitCandidate Map.empty duplicate
  assertEqual "duplicate receipt changes no state" afterGap afterDuplicate
  let conflicting = sequencedItem fixtureIncomingA sequence3 (fixtureDigest 93) (Payload 3)
  assertProblem
    "equal sequence with a different digest is a protocol violation"
    ( PeerStreamProtocolProblem
        (PeerStreamConflictingItem fixtureIncomingA sequence3 (fixtureDigest 3) (fixtureDigest 93))
    )
    (prepareReceiveCandidate conflicting afterGap)
  firstCandidate <- checkedIO "first item" (prepareReceiveCandidate (inboundItem 1) afterGap)
  assertEqual "first arrival is contiguous" ReceiveContiguous (preparedReceiveDisposition firstCandidate)
  assertEqual "only one item is unblocked" [sequence1] (candidateSequences firstCandidate)
  afterFirst <- commitCandidate (Map.singleton sequence1 ReceivedPending) firstCandidate
  secondCandidate <- checkedIO "gap-closing second item" (prepareReceiveCandidate (inboundItem 2) afterFirst)
  assertEqual "closure exposes the complete ascending suffix" [sequence2, sequence3] (candidateSequences secondCandidate)
  successor <-
    commitCandidate
      (Map.fromList [(sequence2, Completed), (sequence3, ReceivedPending)])
      secondCandidate
  watermarks <- checkedIO "incoming watermarks" (incomingWatermarks fixtureIncomingA successor)
  assertEqual "receipt advances over all contiguous items" (prefix 3) (streamReceivedPrefix watermarks)
  assertEqual "completion remains behind the pending head" emptyStreamPrefix (streamCompletedPrefix watermarks)
  retained <- checkedIO "retained incoming" (incomingRetainedItems fixtureIncomingA successor)
  assertEqual
    "only pending incoming payloads remain retained"
    [(Payload 1, IncomingReceivedPending), (Payload 3, IncomingReceivedPending)]
    [(sequencedItemPayload item, status) | (item, status) <- retained]
  assertOwnerInvariant successor

caseReceiveRefinement :: Assertion
caseReceiveRefinement = do
  let predecessor = initialState fixtureLocal :: State Payload
  gapCandidate <- checkedIO "gap" (prepareReceiveCandidate (inboundItem 2) predecessor)
  afterGap <- commitCandidate Map.empty gapCandidate
  closure <- checkedIO "gap closure" (prepareReceiveCandidate (inboundItem 1) afterGap)
  assertEqual "closure contains both items" [sequence1, sequence2] (candidateSequences closure)
  assertProgressMismatch (Set.fromList [sequence1, sequence2]) Set.empty (finalizeReceive Map.empty closure)
  assertProgressMismatch
    (Set.fromList [sequence1, sequence2])
    (Set.fromList [sequence1, sequence2, sequence3])
    ( finalizeReceive
        (Map.fromList [(sequence1, ReceivedPending), (sequence2, ReceivedPending), (sequence3, Completed)])
        closure
    )
  successor <-
    commitCandidate
      (Map.fromList [(sequence1, ReceivedPending), (sequence2, ReceivedPending)])
      closure
  assertOwnerInvariant successor

caseOutOfOrderCompletion :: Assertion
caseOutOfOrderCompletion = do
  afterTwo <- receivePending [1, 2]
  emptyPlan <- checkedIO "empty completion" (prepareCompletion fixtureIncomingA Set.empty afterTwo)
  assertEqual "empty completion is a state identity" afterTwo (fst (commitCompletion emptyPlan))
  completedSecond <- checkedIO "complete second" (prepareCompletion fixtureIncomingA (Set.singleton sequence2) afterTwo)
  let (afterSecond, secondWatermarks) = commitCompletion completedSecond
  assertEqual "later completion is retained without crossing the head" emptyStreamPrefix (streamCompletedPrefix secondWatermarks)
  completedFirst <- checkedIO "complete first" (prepareCompletion fixtureIncomingA (Set.singleton sequence1) afterSecond)
  let (successor, finalWatermarks) = commitCompletion completedFirst
  assertEqual "the completed prefix closes across both items" (prefix 2) (streamCompletedPrefix finalWatermarks)
  assertProblem
    "unknown completion is rejected"
    (PeerStreamProtocolProblem (PeerStreamCompletionUnknown fixtureIncomingA sequence3))
    (prepareCompletion fixtureIncomingA (Set.singleton sequence3) successor)
  gapCandidate <- checkedIO "future gap" (prepareReceiveCandidate (inboundItem 4) successor)
  afterGap <- commitCandidate Map.empty gapCandidate
  assertProblem
    "a retained gap cannot complete before receipt"
    (PeerStreamProtocolProblem (PeerStreamCompletionBeforeReceipt fixtureIncomingA sequence4))
    (prepareCompletion fixtureIncomingA (Set.singleton sequence4) afterGap)
  assertOwnerInvariant successor

caseIncomingMarkerWork :: Assertion
caseIncomingMarkerWork = do
  generation <- checkedIO "marker generation" (mkContextClassGenerationId (ByteString.replicate 32 0x91))
  unrelated <- checkedIO "unrelated marker generation" (mkContextClassGenerationId (ByteString.replicate 32 0x92))
  first <- receivePending [1]
  let families = [MarkerWork.RouteCutoverMarkers, MarkerWork.DisappearanceProbeMarkers]
      pending family = MarkerWork.pendingIncomingMarkerDirections family
      consumeAll state = foldl' (\current family -> MarkerWork.consumeIncomingMarkerDirection family fixtureIncomingA current) state families
      dependency = MarkerWork.RouteMarkerGeneration generation
      watched = MarkerWork.retainIncomingMarkerDependencies MarkerWork.RouteCutoverMarkers fixtureIncomingA (Set.singleton dependency) (consumeAll first)
  forM_ families $ \family -> assertEqual "first incomplete head schedules every family" (Set.singleton fixtureIncomingA) (pending family first)
  let unrelatedNotification = MarkerWork.notifyIncomingMarkerDependencies (Set.singleton (MarkerWork.RouteMarkerGeneration unrelated)) watched
  assertEqual "unrelated dependency does not dirty the head" watched unrelatedNotification
  let once = MarkerWork.notifyIncomingMarkerDependencies (Set.singleton dependency) watched
      twice = MarkerWork.notifyIncomingMarkerDependencies (Set.singleton dependency) once
  assertEqual "several relevant changes coalesce" once twice
  assertEqual "exact dependency selects only its parked family" (Set.singleton fixtureIncomingA) (pending MarkerWork.RouteCutoverMarkers once)
  assertEqual "another family is not selected by the route dependency" Set.empty (pending MarkerWork.DisappearanceProbeMarkers once)
  secondCandidate <- checkedIO "append behind parked head" (prepareReceiveCandidate (inboundItem 2) watched)
  second <- commitCandidate (Map.singleton sequence2 ReceivedPending) secondCandidate
  completedSecond <- checkedIO "complete a suffix exception" (prepareCompletion fixtureIncomingA (Set.singleton sequence2) second)
  let afterSecond = fst (commitCompletion completedSecond)
  gapCandidate <- checkedIO "retain gap behind parked head" (prepareReceiveCandidate (inboundItem 4) afterSecond)
  gap <- commitCandidate Map.empty gapCandidate
  closureCandidate <- checkedIO "close suffix gap" (prepareReceiveCandidate (inboundItem 3) gap)
  closedGap <- commitCandidate (Map.fromList [(sequence3, ReceivedPending), (sequence4, ReceivedPending)]) closureCandidate
  forM_ [second, afterSecond, gap, closedGap] $ \state -> do
    assertOwnerInvariant state
    forM_ families $ \family -> assertEqual "suffix mutation does not reevaluate the blocked head" Set.empty (pending family state)
  completedFirst <- checkedIO "complete the actual blocked head" (prepareCompletion fixtureIncomingA (Set.singleton sequence1) closedGap)
  let exposed = fst (commitCompletion completedFirst)
  forM_ families $ \family -> assertEqual "actual head crossing schedules each family once" (Set.singleton fixtureIncomingA) (pending family exposed)
  next <- checkedIO "skip certified completed exception" (MarkerWork.incomingItemAtOrAfter fixtureIncomingA sequence2 exposed)
  assertEqual "ordered keyed query exposes the first incomplete suffix" (Just sequence3) (sequencedItemSequence . fst <$> next)
  let parkedAgain = consumeAll exposed
      obsoleteNotification = MarkerWork.notifyIncomingMarkerDependencies (Set.singleton dependency) parkedAgain
  assertEqual "head crossing retires the old dependency watch" parkedAgain obsoleteNotification
  frontierPlan <- checkedIO "frontier-only update" (prepareFrontierAdvance fixtureIncomingA (sequenceAt 9) parkedAgain)
  let frontierOnly = commitFrontierAdvance frontierPlan
  forM_ families $ \family -> assertEqual "frontier metadata does not wake marker heads" Set.empty (pending family frontierOnly)
  retirement <- checkedIO "retire marker source" (preparePeerRetirement fixturePeerA frontierOnly)
  let retired = fst (commitPeerRetirement retirement)
  forM_ families $ \family -> assertEqual "source retirement revisits the exact direction" (Set.singleton fixtureIncomingA) (pending family retired)
  repeatedRetirement <- checkedIO "repeat source retirement" (preparePeerRetirement fixturePeerA (consumeAll retired))
  assertEqual "retirement replay does not redirty the head" (consumeAll retired) (fst (commitPeerRetirement repeatedRetirement))
  assertOwnerInvariant retired
  finalCompletions <- checkedIO "settle the retired direction's remaining obligations" (prepareCompletion fixtureIncomingA (Set.fromList [sequence3, sequence4]) retired)
  let fullyRetired = fst (commitCompletion finalCompletions)
  forM_ families $ \family -> do
    assertEqual "retired completed channel retains no scheduled direction" Set.empty (pending family fullyRetired)
    assertBool "retired completed channel leaves the scheduling inventory" (Set.notMember fixtureIncomingA (MarkerWork.incomingMarkerDirections family fullyRetired))
  assertEqual "terminal channel keeps no obsolete inverse watcher" fullyRetired (MarkerWork.notifyIncomingMarkerDependencies (Set.singleton dependency) fullyRetired)
  assertOwnerInvariant fullyRetired
  gapOnlyCandidate <- checkedIO "unresolved gap-only channel" (prepareReceiveCandidate (inboundItem 2) (initialState fixtureLocal))
  gapOnly <- commitCandidate Map.empty gapOnlyCandidate
  gapRetirement <- checkedIO "retire a channel with no admitted obligation" (preparePeerRetirement fixturePeerA gapOnly)
  let retiredGap = fst (commitPeerRetirement gapRetirement)
  forM_ families $ \family -> assertBool "retiring a gap-only channel removes its registration immediately" (Set.notMember fixtureIncomingA (MarkerWork.incomingMarkerDirections family retiredGap))
  assertOwnerInvariant retiredGap

caseAcknowledgements :: Assertion
caseAcknowledgements = do
  let pristine = initialState fixtureLocal :: State Payload
  emptyReceived <- checkedIO "empty received acknowledgement" (prepareReceivedAck fixtureOutgoingA emptyStreamPrefix pristine)
  assertEqual "empty received acknowledgement is a state identity" pristine (fst (commitReceivedAck emptyReceived))
  emptyCompleted <- checkedIO "empty completed acknowledgement" (prepareCompletedAck fixtureOutgoingA emptyStreamPrefix pristine)
  assertEqual "empty completed acknowledgement is a state identity" pristine (fst (commitCompletedAck emptyCompleted))
  (enqueued, _) <- enqueuePayloads fixturePeerA [1, 2, 3] (initialState fixtureLocal)
  receivedPlan <- checkedIO "received through two" (prepareReceivedAck fixtureOutgoingA (prefix 2) enqueued)
  let (receivedState, watermarks) = commitReceivedAck receivedPlan
  assertEqual "received acknowledgement advances receipt" (prefix 2) (streamReceivedPrefix watermarks)
  assertEqual "received acknowledgement does not imply completion" emptyStreamPrefix (streamCompletedPrefix watermarks)
  assertEqual "received acknowledgement retains every payload" [Payload 1, Payload 2, Payload 3] . fmap sequencedItemPayload =<< checkedIO "active after received" (activeOutgoingItems fixtureOutgoingA receivedState)
  stale <- checkedIO "stale received" (prepareReceivedAck fixtureOutgoingA (prefix 1) receivedState)
  assertEqual "ordinary stale received acknowledgement is a no-op" receivedState (fst (commitReceivedAck stale))
  assertAckAhead (prefix 4) (prefix 3) (prepareReceivedAck fixtureOutgoingA (prefix 4) receivedState)
  completedPlan <- checkedIO "completed through two" (prepareCompletedAck fixtureOutgoingA (prefix 2) receivedState)
  let (completedState, completedWatermarks) = commitCompletedAck completedPlan
  assertEqual "completed implies receipt" (prefix 2) (streamReceivedPrefix completedWatermarks)
  assertEqual "completed advances independently" (prefix 2) (streamCompletedPrefix completedWatermarks)
  assertEqual "only uncompleted payload remains active" [Payload 3] . fmap sequencedItemPayload =<< checkedIO "active after completion" (activeOutgoingItems fixtureOutgoingA completedState)
  forM_ [(sequence1, fixtureDigest 1), (sequence2, fixtureDigest 2)] $ \(sequenceNumber, digest) ->
    assertEqual
      "released assignments remain compactly completed"
      (Right True)
      (assignmentCompletionKnown (assignmentReceipt fixtureOutgoingA sequenceNumber digest) completedState)
  assertEqual "completion retains no per-assignment receipt records" (1, 0, 0) (peerStreamRetentionCounts completedState)
  stronger <- checkedIO "completed without received" (prepareCompletedAck fixtureOutgoingA (prefix 3) enqueued)
  let (fullyCompleted, implied) = commitCompletedAck stronger
  assertEqual "strong completion can replace a lost received ack" (prefix 3) (streamReceivedPrefix implied)
  assertEqual "all payloads release" [] =<< checkedIO "fully released" (activeOutgoingItems fixtureOutgoingA fullyCompleted)
  assertOwnerInvariant completedState

caseResume :: Assertion
caseResume = do
  (enqueued, assigned) <- enqueuePayloads fixturePeerA [1, 2, 3, 4] (initialState fixtureLocal)
  receivedPlan <- checkedIO "ordinary received three" (prepareReceivedAck fixtureOutgoingA (prefix 3) enqueued)
  let (sourceState, _) = commitReceivedAck receivedPlan
      assignedItems = NonEmpty.toList assigned
      item3 = assignedItems !! 2
  advertisedGap <-
    checkedIO
      "offered peer gap"
      (mkGapSummary fixtureOutgoingA (prefix 1) [(sequence3, sequencedItemDigest item3)])
  offer <- checkedIO "resume offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 (prefix 1) emptyStreamPrefix advertisedGap)
  prepared <- checkedIO "resume" (prepareResume offer sourceState)
  assertEqual
    "resume reuses exact retained items and excludes the advertised gap"
    [assignedItems !! 1, assignedItems !! 3]
    (preparedResumeRetransmissions prepared)
  let (successor, response, retransmissions) = commitResume prepared
  assertEqual "commit returns the prepared retransmissions" (preparedResumeRetransmissions prepared) retransmissions
  assertEqual "commit returns the prepared response" (preparedResumeResponse prepared) response
  assertEqual "a lower resume receipt does not lower the ordinary watermark" (prefix 3) . streamReceivedPrefix =<< checkedIO "post-resume watermarks" (outgoingWatermarks fixtureOutgoingA successor)
  exactRepeat <- checkedIO "same-predecessor repeat" (prepareResume offer sourceState)
  assertEqual "same predecessor plus same offer is deterministic" (commitResume prepared) (commitResume exactRepeat)
  continuingRepeat <-
    checkedIO
      "same-association repeat"
      ( prepareResumeForAssociation
          ContinuingResumeOnAssociation
          offer
          successor
      )
  assertEqual
    "an exact raw retransmission claim does not reactivate written work on one association"
    []
    (preparedResumeRetransmissions continuingRepeat)
  let continuingSuccessor =
        let (next, _, _) = commitResume continuingRepeat
         in next
      item4 = assignedItems !! 3
  expandedGap <-
    checkedIO
      "expanded same-association gap"
      ( mkGapSummary
          fixtureOutgoingA
          (prefix 1)
          [ (sequence3, sequencedItemDigest item3),
            (sequence4, sequencedItemDigest item4)
          ]
      )
  expandedOffer <-
    checkedIO
      "expanded same-association gap offer"
      ( mkResumeOffer
          fixturePeerA
          fixtureLocal
          sequence1
          (prefix 1)
          emptyStreamPrefix
          expandedGap
      )
  expandedClaim <-
    checkedIO
      "same-association gap confirmation"
      ( prepareResumeForAssociation
          ContinuingResumeOnAssociation
          expandedOffer
          continuingSuccessor
      )
  assertEqual
    "new confirmation does not replay items still requested by both claims"
    []
    (preparedResumeRetransmissions expandedClaim)
  let expandedSuccessor =
        let (next, _, _) = commitResume expandedClaim
         in next
  removedGapClaim <-
    checkedIO
      "remove one same-association gap confirmation"
      ( prepareResumeForAssociation
          ContinuingResumeOnAssociation
          offer
          expandedSuccessor
      )
  assertEqual
    "removing one gap confirmation reactivates only that newly requested item"
    [item4]
    (preparedResumeRetransmissions removedGapClaim)
  let removedGapSuccessor =
        let (next, _, _) = commitResume removedGapClaim
         in next
  lowerGap <-
    checkedIO
      "lower raw retransmission gap"
      ( mkGapSummary
          fixtureOutgoingA
          emptyStreamPrefix
          [(sequence3, sequencedItemDigest item3)]
      )
  lowerOffer <-
    checkedIO
      "lower raw retransmission offer"
      ( mkResumeOffer
          fixturePeerA
          fixtureLocal
          sequence1
          emptyStreamPrefix
          emptyStreamPrefix
          lowerGap
      )
  changedRawClaim <-
    checkedIO
      "changed lower raw claim"
      ( prepareResumeForAssociation
          ContinuingResumeOnAssociation
          lowerOffer
          removedGapSuccessor
      )
  assertEqual
    "a lower raw claim reactivates only the newly requested earlier item even when retained acknowledgement normalization is unchanged"
    [assignedItems !! 0]
    (preparedResumeRetransmissions changedRawClaim)
  let changedRawSuccessor =
        let (next, _, _) = commitResume changedRawClaim
         in next
  assertOwnerInvariant changedRawSuccessor
  badGap <- checkedIO "conflicting gap digest" (mkGapSummary fixtureOutgoingA (prefix 1) [(sequence3, fixtureDigest 99)])
  badOffer <- checkedIO "bad resume offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 (prefix 1) emptyStreamPrefix badGap)
  assertProblem
    "a gap digest must equal the retained item"
    ( PeerStreamProtocolProblem
        (PeerStreamResumeGapDigestMismatch fixtureOutgoingA sequence3 (fixtureDigest 3) (fixtureDigest 99))
    )
    (prepareResume badOffer sourceState)
  wrongDestination <- checkedIO "wrong destination offer" (mkResumeOffer fixturePeerA fixturePeerB sequence1 emptyStreamPrefix emptyStreamPrefix =<< mkGapSummary (fixtureDirection 3 2) emptyStreamPrefix [])
  assertProblem
    "resume endpoint role is exact"
    (PeerStreamProtocolProblem (PeerStreamResumeDestinationMismatch fixtureLocal fixturePeerB))
    (prepareResume wrongDestination sourceState)
  assertOwnerInvariant successor
  let destinationState0 = initialState fixturePeerA :: State Payload
  generatedOffer <- checkedIO "destination-generated resume offer" (resumeOfferForPeer fixtureLocal destinationState0)
  preparedPair <- checkedIO "two-owner resume" (prepareResume generatedOffer enqueued)
  let (_, pairResponse, _) = commitResume preparedPair
  assertEqual "two-owner response reports no received forward prefix" emptyStreamPrefix (resumeResponseReceivedPrefix pairResponse)

caseFrontierAdvance :: Assertion
caseFrontierAdvance = do
  let pristine = initialState fixtureLocal :: State Payload
  reverseWritten <- writtenReverseState pristine
  prepared3 <- checkedIO "advertise three" (prepareFrontierAdvance fixtureIncomingA sequence3 reverseWritten)
  let after3 = commitFrontierAdvance prepared3
  expectedIncoming <- checkedIO "advance before reverse dispatch" (prepareFrontierAdvance fixtureIncomingA sequence3 pristine)
  expected <- writtenReverseState (commitFrontierAdvance expectedIncoming)
  assertEqual
    "frontier advancement commutes with and therefore leaves reverse dispatch exactly unchanged"
    expected
    after3
  assertEqual "the greatest source frontier is retained" (Just sequence3) =<< checkedIO "frontier" (incomingGreatestAdvertisedNextSequence fixtureIncomingA after3)
  assertEqual
    "the reverse written item remains suppressed pending genuine peer progress"
    (Set.singleton sequence1)
    =<< checkedIO "reverse awaiting set" (dispatchAwaitingSequences fixtureOutgoingA after3)
  assertEqual
    "one-way frontier advancement does not schedule reverse retransmission"
    Nothing
    =<< checkedIO "reverse dispatch ticket" (currentDispatchTicket fixtureOutgoingA after3)
  repeated <- checkedIO "repeat frontier" (prepareFrontierAdvance fixtureIncomingA sequence3 after3)
  assertEqual "repeating a frontier is an exact no-op" after3 (commitFrontierAdvance repeated)
  assertProblem
    "an item at the exclusive frontier is rejected"
    (PeerStreamProtocolProblem (PeerStreamItemBeyondAdvertisedFrontier fixtureIncomingA sequence3 sequence3))
    (prepareReceiveCandidate (inboundItem 3) after3)
  assertProblem
    "the same live epoch cannot regress an advertised frontier"
    (PeerStreamProtocolProblem (PeerStreamSourceFrontierRegressed fixtureIncomingA sequence3 sequence2))
    (prepareFrontierAdvance fixtureIncomingA sequence2 after3)
  raised <- checkedIO "raise frontier" (prepareFrontierAdvance fixtureIncomingA sequence4 after3)
  let after4 = commitFrontierAdvance raised
  item3Candidate <- checkedIO "item below raised frontier" (prepareReceiveCandidate (inboundItem 3) after4)
  assertEqual "raising the frontier admits the tail" (ReceiveGap sequence1) (preparedReceiveDisposition item3Candidate)
  assertOwnerInvariant after4
  where
    writtenReverseState predecessor = do
      boundPlan <-
        checkedIO
          "bind reverse dispatch"
          (prepareDispatchBinding fixtureOutgoingA (fixtureBinding 41) predecessor)
      enqueuePlan <-
        checkedIO
          "enqueue reverse item"
          (prepareEnqueue (Map.singleton fixturePeerA (fixtureItem 1 :| [])) (fst (commitDispatchBinding boundPlan)))
      ticket <-
        exactlyOne
          "reverse enqueue dispatch ticket"
          (preparedEnqueueDispatchTickets enqueuePlan)
      let ready = fst (commitEnqueue enqueuePlan)
      (selected, attempt) <- selectDispatch (fixtureBinding 41) ticket ready
      written <-
        checkedIO
          "write reverse item"
          (prepareDispatchOutcome attempt PeerDispatchWritten selected)
      pure (fst (commitDispatchOutcome written))

caseProtocolMatrices :: Assertion
caseProtocolMatrices = do
  (enqueued, assigned) <- enqueuePayloads fixturePeerA [1, 2, 3] (initialState fixtureLocal)
  completedTwo <- checkedIO "complete two" (prepareCompletedAck fixtureOutgoingA (prefix 2) enqueued)
  let (afterCompletedTwo, _) = commitCompletedAck completedTwo
  staleCompleted <- checkedIO "stale completion ack" (prepareCompletedAck fixtureOutgoingA (prefix 1) afterCompletedTwo)
  assertEqual "stale completed acknowledgement is an exact no-op" afterCompletedTwo (fst (commitCompletedAck staleCompleted))
  assertAckAhead (prefix 4) (prefix 3) (prepareCompletedAck fixtureOutgoingA (prefix 4) afterCompletedTwo)
  let retainedItems = NonEmpty.toList assigned
  noGapAtTwo <- checkedIO "no gap at two" (mkGapSummary fixtureOutgoingA (prefix 2) [])
  regression <- checkedIO "completion regression offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 (prefix 2) (prefix 1) noGapAtTwo)
  mergedResume <- checkedIO "stale resume completion is merged" (prepareResume regression afterCompletedTwo)
  let (mergedState, _, _) = commitResume mergedResume
  assertEqual "stale resume cannot reopen released positions" [3] . fmap sequenceInt
    =<< checkedIO "active after stale resume" (activeOutgoingItems fixtureOutgoingA mergedState)
  beyondCompletedGap <- checkedIO "gap beyond allocation" (mkGapSummary fixtureOutgoingA (prefix 4) [])
  beyondCompleted <- checkedIO "completed beyond allocation offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 (prefix 4) (prefix 4) beyondCompletedGap)
  assertProblem
    "resume completed cannot exceed allocation"
    (PeerStreamProtocolProblem (PeerStreamResumeCompletedBeyondAllocated fixtureOutgoingA (prefix 4) (prefix 3)))
    (prepareResume beyondCompleted enqueued)
  beyondReceivedGap <- checkedIO "received beyond allocation gap" (mkGapSummary fixtureOutgoingA (prefix 4) [])
  beyondReceived <- checkedIO "received beyond allocation offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 (prefix 4) emptyStreamPrefix beyondReceivedGap)
  assertProblem
    "resume received cannot exceed allocation"
    (PeerStreamProtocolProblem (PeerStreamResumeReceivedBeyondAllocated fixtureOutgoingA (prefix 4) (prefix 3)))
    (prepareResume beyondReceived enqueued)
  inactiveGap <- checkedIO "inactive gap" (mkGapSummary fixtureOutgoingA emptyStreamPrefix [(sequence4, fixtureDigest 4)])
  inactiveOffer <- checkedIO "inactive gap offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 emptyStreamPrefix emptyStreamPrefix inactiveGap)
  assertProblem
    "a resume gap must name an active allocation"
    (PeerStreamProtocolProblem (PeerStreamResumeGapEntryNotActive fixtureOutgoingA sequence4))
    (prepareResume inactiveOffer enqueued)
  let item3 = retainedItems !! 2
  activeGap <- checkedIO "active gap" (mkGapSummary fixtureOutgoingA emptyStreamPrefix [(sequence3, sequencedItemDigest item3)])
  activeOffer <- checkedIO "active-gap offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 emptyStreamPrefix emptyStreamPrefix activeGap)
  activePrepared <- checkedIO "active-gap resume" (prepareResume activeOffer enqueued)
  assertEqual "valid gap excludes only its exact retained item" [sequence1, sequence2] (fmap sequencedItemSequence (preparedResumeRetransmissions activePrepared))
  afterOne <- receivePending [1]
  let contradictoryGapDirection = fixtureOutgoingA
  emptyReverseGap <- checkedIO "empty reverse claim" (mkGapSummary contradictoryGapDirection emptyStreamPrefix [])
  contradictingHistory <- checkedIO "history-contradicting offer" (mkResumeOffer fixturePeerA fixtureLocal sequence1 emptyStreamPrefix emptyStreamPrefix emptyReverseGap)
  assertProblem
    "source frontier must be beyond retained history"
    (PeerStreamProtocolProblem (PeerStreamSourceFrontierContradictsHistory fixtureIncomingA sequence1 (prefix 1)))
    (prepareResume contradictingHistory afterOne)
  assertOwnerInvariant afterCompletedTwo

propModelTrace :: Property
propModelTrace =
  forAll modelParameters $ \parameters ->
    ioProperty $ do
      observed <- runModelScenario parameters
      pure $ case observed of
        Left problem -> counterexample problem False
        Right () -> property True

data ResumeDeltaParameters = ResumeDeltaParameters
  { resumeAllocated :: Int,
    previousReceived :: Int,
    previousGapFlags :: [Bool],
    currentReceived :: Int,
    currentGapFlags :: [Bool]
  }
  deriving stock (Eq, Show)

propResumeAssociationDelta :: Property
propResumeAssociationDelta =
  forAll resumeDeltaParameters $ \parameters ->
    let previousRequested = resumeRequested parameters.previousReceived parameters.previousGapFlags
        currentRequested = resumeRequested parameters.currentReceived parameters.currentGapFlags
     in case runResumeAssociationDelta parameters of
          Left problem -> counterexample problem False
          Right (firstSequences, continuingSequences) ->
            counterexample
              ("resume delta: " <> show (parameters, firstSequences, continuingSequences))
              ( firstSequences == Set.toAscList currentRequested
                  && continuingSequences
                    == Set.toAscList (currentRequested `Set.difference` previousRequested)
              )

resumeDeltaParameters :: Gen ResumeDeltaParameters
resumeDeltaParameters = do
  resumeAllocated <- choose (1, 8)
  previousReceived <- choose (0, resumeAllocated)
  previousGapFlags <- traverse (const (choose (False, True))) [1 .. resumeAllocated]
  currentReceived <- choose (0, resumeAllocated)
  currentGapFlags <- traverse (const (choose (False, True))) [1 .. resumeAllocated]
  pure ResumeDeltaParameters {resumeAllocated, previousReceived, previousGapFlags, currentReceived, currentGapFlags}

runResumeAssociationDelta :: ResumeDeltaParameters -> Either String ([Int], [Int])
runResumeAssociationDelta parameters = do
  (enqueued, assigned) <- mapLeft show (enqueuePayloadsPure fixturePeerA [1 .. parameters.resumeAllocated] (initialState fixtureLocal))
  let items = NonEmpty.toList assigned
  previousOffer <- resumeDeltaOffer items parameters.previousReceived parameters.previousGapFlags
  previousPrepared <- mapLeft show (prepareResume previousOffer enqueued)
  let (afterPrevious, _, _) = commitResume previousPrepared
  currentOffer <- resumeDeltaOffer items parameters.currentReceived parameters.currentGapFlags
  firstPrepared <-
    mapLeft
      show
      (prepareResumeForAssociation FirstResumeOnAssociation currentOffer afterPrevious)
  continuingPrepared <-
    mapLeft
      show
      (prepareResumeForAssociation ContinuingResumeOnAssociation currentOffer afterPrevious)
  pure
    ( fmap sequenceInt (preparedResumeRetransmissions firstPrepared),
      fmap sequenceInt (preparedResumeRetransmissions continuingPrepared)
    )

resumeDeltaOffer :: [SequencedItem Payload] -> Int -> [Bool] -> Either String ResumeOffer
resumeDeltaOffer items received gapFlags = do
  gap <-
    mapLeft
      show
      ( mkGapSummary
          fixtureOutgoingA
          (prefix received)
          [ (sequenceAt index, sequencedItemDigest (items !! (index - 1)))
          | (index, retained) <- zip [1 ..] gapFlags,
            index > received + 1,
            retained
          ]
      )
  mapLeft
    show
    (mkResumeOffer fixturePeerA fixtureLocal sequence1 (prefix received) emptyStreamPrefix gap)

resumeRequested :: Int -> [Bool] -> Set.Set Int
resumeRequested received gapFlags =
  Set.fromList
    [ index
    | (index, retained) <- zip [1 ..] gapFlags,
      index > received,
      not (index > received + 1 && retained)
    ]

propTraceDeterminism :: Property
propTraceDeterminism =
  forAll modelParameters $ \parameters ->
    ioProperty $ do
      left <- runScenarioResult parameters
      right <- runScenarioResult parameters
      pure (left === right)

propReceivePermutationTrace :: Property
propReceivePermutationTrace =
  forAll receiveTraceParameters $ \(delivery, completed) ->
    ioProperty $ do
      observed <- runReceivePermutation delivery (Set.fromList (fmap sequenceAt completed))
      pure $ case observed of
        Left problem -> counterexample problem False
        Right state ->
          case (incomingWatermarks fixtureIncomingA state, incomingRetainedItems fixtureIncomingA state) of
            (Right watermarks, Right retained) ->
              let itemCount = length delivery
                  expectedCompleted = length (takeWhile (`Set.member` Set.fromList completed) [1 .. itemCount])
                  observedStatuses =
                    [ (sequenceInt item, status)
                    | (item, status) <- retained
                    ]
                  expectedStatuses =
                    [ (index, IncomingReceivedPending)
                    | index <- [1 .. itemCount],
                      index `notElem` completed
                    ]
               in counterexample
                    ("final receive state: " <> show (watermarks, observedStatuses))
                    ( streamReceivedPrefix watermarks == prefix itemCount
                        && streamCompletedPrefix watermarks == prefix expectedCompleted
                        && observedStatuses == expectedStatuses
                        && validatePeerStreamState state == Right ()
                    )
            (Left problem, _) -> counterexample (show problem) False
            (_, Left problem) -> counterexample (show problem) False

receiveTraceParameters :: Gen ([Int], [Int])
receiveTraceParameters = do
  itemCount <- choose (1, 8)
  delivery <- shuffle [1 .. itemCount]
  completionFlags <- traverse (const (choose (False, True))) [1 .. itemCount]
  pure (delivery, [index | (index, shouldComplete) <- zip [1 ..] completionFlags, shouldComplete])

runReceivePermutation :: [Int] -> Set.Set StreamSequence -> IO (Either String (State Payload))
runReceivePermutation delivery completed =
  foldM receiveOne (Right (initialState fixtureLocal)) delivery
  where
    receiveOne (Left problem) _ = pure (Left problem)
    receiveOne (Right state) value =
      case prepareReceiveCandidate (inboundItem value) state of
        Left problem -> pure (Left (show problem))
        Right candidate -> do
          let newlyContiguous = fmap sequencedItemSequence (preparedContiguousItems candidate)
              progress =
                Map.fromList
                  [ (sequenceNumber, if Set.member sequenceNumber completed then Completed else ReceivedPending)
                  | sequenceNumber <- newlyContiguous
                  ]
          case finalizeReceive progress candidate of
            Left problem -> pure (Left (show problem))
            Right finalized -> do
              let (successor, _) = commitReceive finalized
              case validatePeerStreamState successor of
                Left problem -> pure (Left (show problem))
                Right () -> pure (Right successor)

-- The focused properties above make individual failures easy to diagnose. This
-- generated trace is the complementary whole-owner check promised by the Step-6
-- plan: one state contains two directions in each role while receive, completion,
-- acknowledgement, and resume commands are interleaved. A deliberately plain
-- first-order model is checked after every installed successor.
data FixturePeer = FixturePeerA | FixturePeerB
  deriving stock (Eq, Ord, Show)

data MixedPeerParameters = MixedPeerParameters
  { mixedIncomingCount :: Int,
    mixedOutgoingCount :: Int,
    mixedDeliveryOrder :: [Int],
    mixedCompletionTargets :: Set.Set Int,
    mixedReceivedClaim :: Int,
    mixedCompletedClaim :: Int,
    mixedResumeReceived :: Int,
    mixedGapParity :: Bool
  }
  deriving stock (Eq, Show)

data MixedCommand
  = MixedReceive FixturePeer Int
  | MixedReceivedAck FixturePeer
  | MixedCompletedAck FixturePeer
  | MixedResume FixturePeer
  deriving stock (Eq, Show)

data MixedTraceParameters = MixedTraceParameters
  { mixedParametersByPeer :: Map FixturePeer MixedPeerParameters,
    mixedCommands :: [MixedCommand]
  }
  deriving stock (Eq, Show)

data MixedPeerModel = MixedPeerModel
  { modelOutgoingItems :: [SequencedItem Payload],
    modelOutgoingReceived :: Int,
    modelOutgoingCompleted :: Int,
    modelPeerGapEntries :: [(Int, PeerItemDigest)],
    modelIncomingStatuses :: Map Int IncomingItemStatus,
    modelIncomingReceived :: Int,
    modelIncomingCompleted :: Int,
    modelIncomingFrontier :: Maybe Int
  }
  deriving stock (Eq, Show)

type MixedModel = Map FixturePeer MixedPeerModel

propMixedSeveralPeerTrace :: Property
propMixedSeveralPeerTrace =
  forAll mixedTraceParameters $ \parameters ->
    case runMixedTrace parameters of
      Left problem -> counterexample (problem <> "\ntrace: " <> show parameters) False
      Right _ -> property True

propIncomingDirectionWitness :: Property
propIncomingDirectionWitness =
  forAll mixedTraceParameters $ \parameters ->
    case runMixedTrace parameters of
      Left problem -> counterexample (problem <> "\ntrace: " <> show parameters) False
      Right states ->
        conjoin
          [ counterexample
              ("incoming direction witness at mixed snapshot " <> show ordinal)
              ( peerStreamIncomingDirections state
                  === mapLeft
                    PeerStreamInvariantProblem
                    (peerStreamWitnessIncomingDirections <$> peerStreamStateWitness state)
              )
          | (ordinal, state) <- zip [(0 :: Int) ..] states
          ]

mixedTraceParameters :: Gen MixedTraceParameters
mixedTraceParameters = do
  parametersA <- mixedPeerParameters
  parametersB <- mixedPeerParameters
  let byPeer =
        Map.fromList
          [ (FixturePeerA, parametersA),
            (FixturePeerB, parametersB)
          ]
      receiveCommands =
        [ MixedReceive peer sequenceNumber
        | peer <- allFixturePeers,
          sequenceNumber <- mixedDeliveryOrder (mixedParameters peer byPeer)
        ]
      protocolCommands =
        concatMap
          ( \peer ->
              [ MixedReceivedAck peer,
                MixedCompletedAck peer,
                MixedResume peer
              ]
          )
          allFixturePeers
  commands <- shuffle (receiveCommands <> protocolCommands)
  pure
    MixedTraceParameters
      { mixedParametersByPeer = byPeer,
        mixedCommands = commands
      }

mixedPeerParameters :: Gen MixedPeerParameters
mixedPeerParameters = do
  incomingCount <- choose (2, 6)
  outgoingCount <- choose (2, 6)
  deliveryOrder <- shuffle [1 .. incomingCount]
  completionFlags <- traverse (const (choose (False, True))) [1 .. incomingCount]
  receivedClaim <- choose (0, outgoingCount)
  completedClaim <- choose (0, outgoingCount)
  resumeReceived <- choose (completedClaim, outgoingCount)
  gapParity <- choose (False, True)
  pure
    MixedPeerParameters
      { mixedIncomingCount = incomingCount,
        mixedOutgoingCount = outgoingCount,
        mixedDeliveryOrder = deliveryOrder,
        mixedCompletionTargets =
          Set.fromList
            [ sequenceNumber
            | (sequenceNumber, shouldComplete) <- zip [1 ..] completionFlags,
              shouldComplete
            ],
        mixedReceivedClaim = receivedClaim,
        mixedCompletedClaim = completedClaim,
        mixedResumeReceived = resumeReceived,
        mixedGapParity = gapParity
      }

runMixedTrace :: MixedTraceParameters -> Either String [State Payload]
runMixedTrace parameters = do
  let predecessor = initialState fixtureLocal :: State Payload
      requests =
        Map.fromList
          [ ( fixturePeerEpoch peer,
              mixedPeerItem peer 1
                :| [ mixedPeerItem peer sequenceNumber
                   | sequenceNumber <- [2 .. mixedOutgoingCount peerParameters]
                   ]
            )
          | peer <- allFixturePeers,
            let peerParameters = mixedParameters peer parameters.mixedParametersByPeer
          ]
  prepared <- mapLeft show (prepareEnqueue requests predecessor)
  let (afterEnqueue, assignments) = commitEnqueue prepared
      model =
        Map.fromList
          [ (peer, initialMixedPeerModel peer (mixedParameters peer parameters.mixedParametersByPeer))
          | peer <- allFixturePeers
          ]
      expectedAssignments =
        Map.fromList
          [ ( fixturePeerEpoch peer,
              mixedOutgoingItem peer 1
                :| [ mixedOutgoingItem peer sequenceNumber
                   | sequenceNumber <- [2 .. mixedOutgoingCount (mixedParameters peer parameters.mixedParametersByPeer)]
                   ]
            )
          | peer <- allFixturePeers
          ]
  expectEqual "mixed enqueue assignments" expectedAssignments assignments
  checkMixedState "after mixed enqueue" afterEnqueue model
  (_, _, reversedStates) <-
    foldM
      retainSnapshot
      (afterEnqueue, model, [afterEnqueue, predecessor])
      parameters.mixedCommands
  Right (reverse reversedStates)
  where
    retainSnapshot (state, model, snapshots) command = do
      (successor, nextModel) <- runMixedCommand parameters (state, model) command
      Right (successor, nextModel, successor : snapshots)

runMixedCommand ::
  MixedTraceParameters ->
  (State Payload, MixedModel) ->
  MixedCommand ->
  Either String (State Payload, MixedModel)
runMixedCommand parameters current command =
  case command of
    MixedReceive peer sequenceNumber ->
      runMixedReceive parameters peer sequenceNumber current
    MixedReceivedAck peer -> runMixedReceivedAck parameters peer current
    MixedCompletedAck peer -> runMixedCompletedAck parameters peer current
    MixedResume peer -> runMixedResume parameters peer current

runMixedReceive ::
  MixedTraceParameters ->
  FixturePeer ->
  Int ->
  (State Payload, MixedModel) ->
  Either String (State Payload, MixedModel)
runMixedReceive parameters peer sequenceNumber (state, model) = do
  candidate <-
    mapLeft
      show
      (prepareReceiveCandidate (mixedIncomingItem peer sequenceNumber) state)
  let peerModel = mixedModelPeer peer model
      inserted =
        Map.insert
          sequenceNumber
          GapRetained
          peerModel.modelIncomingStatuses
      nextMissing = peerModel.modelIncomingReceived + 1
      newlyContiguous
        | sequenceNumber == nextMissing = contiguousKeysFrom nextMissing inserted
        | otherwise = []
      expectedDisposition
        | sequenceNumber == nextMissing = ReceiveContiguous
        | otherwise = ReceiveGap (sequenceAt nextMissing)
      actualContiguous = fmap sequenceInt (preparedContiguousItems candidate)
  expectEqual "mixed receive disposition" expectedDisposition (preparedReceiveDisposition candidate)
  expectEqual "mixed newly-contiguous run" newlyContiguous actualContiguous
  finalized <-
    mapLeft
      show
      ( finalizeReceive
          (Map.fromList [(sequenceAt index, ReceivedPending) | index <- newlyContiguous])
          candidate
      )
  let (afterReceive, _) = commitReceive finalized
      receivedStatuses =
        foldr
          (`Map.insert` IncomingReceivedPending)
          inserted
          newlyContiguous
      receivedPeerModel =
        peerModel
          { modelIncomingStatuses = receivedStatuses,
            modelIncomingReceived = contiguousStatusPrefix (/= GapRetained) receivedStatuses,
            modelIncomingCompleted = contiguousStatusPrefix (== IncomingCompleted) receivedStatuses
          }
      receivedModel = Map.insert peer receivedPeerModel model
  checkMixedState ("after mixed receive " <> show (peer, sequenceNumber)) afterReceive receivedModel
  foldM
    (runMixedCompletion peer)
    (afterReceive, receivedModel)
    [ index
    | index <- reverse newlyContiguous,
      Set.member
        index
        (mixedParameters peer parameters.mixedParametersByPeer).mixedCompletionTargets
    ]

runMixedCompletion ::
  FixturePeer ->
  (State Payload, MixedModel) ->
  Int ->
  Either String (State Payload, MixedModel)
runMixedCompletion peer (state, model) sequenceNumber = do
  prepared <-
    mapLeft
      show
      ( prepareCompletion
          (fixtureIncomingDirection peer)
          (Set.singleton (sequenceAt sequenceNumber))
          state
      )
  let (successor, _) = commitCompletion prepared
      peerModel = mixedModelPeer peer model
      statuses =
        Map.insert
          sequenceNumber
          IncomingCompleted
          peerModel.modelIncomingStatuses
      nextPeerModel =
        peerModel
          { modelIncomingStatuses = statuses,
            modelIncomingCompleted = contiguousStatusPrefix (== IncomingCompleted) statuses
          }
      nextModel = Map.insert peer nextPeerModel model
  checkMixedState ("after mixed completion " <> show (peer, sequenceNumber)) successor nextModel
  Right (successor, nextModel)

runMixedReceivedAck ::
  MixedTraceParameters ->
  FixturePeer ->
  (State Payload, MixedModel) ->
  Either String (State Payload, MixedModel)
runMixedReceivedAck parameters peer (state, model) = do
  let claim =
        (mixedParameters peer parameters.mixedParametersByPeer).mixedReceivedClaim
  prepared <-
    mapLeft
      show
      (prepareReceivedAck (fixtureOutgoingDirection peer) (prefix claim) state)
  let (successor, _) = commitReceivedAck prepared
      peerModel = mixedModelPeer peer model
      received = max peerModel.modelOutgoingReceived claim
      nextPeerModel =
        peerModel
          { modelOutgoingReceived = received,
            modelPeerGapEntries =
              normalizeModelGaps received peerModel.modelPeerGapEntries
          }
      nextModel = Map.insert peer nextPeerModel model
  checkMixedState ("after mixed received ack " <> show peer) successor nextModel
  Right (successor, nextModel)

runMixedCompletedAck ::
  MixedTraceParameters ->
  FixturePeer ->
  (State Payload, MixedModel) ->
  Either String (State Payload, MixedModel)
runMixedCompletedAck parameters peer (state, model) = do
  let claim =
        (mixedParameters peer parameters.mixedParametersByPeer).mixedCompletedClaim
  prepared <-
    mapLeft
      show
      (prepareCompletedAck (fixtureOutgoingDirection peer) (prefix claim) state)
  let (successor, _) = commitCompletedAck prepared
      peerModel = mixedModelPeer peer model
      completed = max peerModel.modelOutgoingCompleted claim
      received = max peerModel.modelOutgoingReceived completed
      nextPeerModel =
        peerModel
          { modelOutgoingCompleted = completed,
            modelOutgoingReceived = received,
            modelPeerGapEntries =
              normalizeModelGaps received peerModel.modelPeerGapEntries
          }
      nextModel = Map.insert peer nextPeerModel model
  checkMixedState ("after mixed completed ack " <> show peer) successor nextModel
  Right (successor, nextModel)

runMixedResume ::
  MixedTraceParameters ->
  FixturePeer ->
  (State Payload, MixedModel) ->
  Either String (State Payload, MixedModel)
runMixedResume parameters peer (state, model) = do
  let peerParameters = mixedParameters peer parameters.mixedParametersByPeer
      peerModel = mixedModelPeer peer model
      offeredReceived = peerParameters.mixedResumeReceived
      offeredCompleted = peerParameters.mixedCompletedClaim
      offeredGapEntries =
        [ (sequenceAt index, sequencedItemDigest item)
        | item <- peerModel.modelOutgoingItems,
          let index = sequenceInt item,
          index > offeredReceived + 1,
          even index == peerParameters.mixedGapParity
        ]
  offeredGap <-
    mapLeft
      show
      ( mkGapSummary
          (fixtureOutgoingDirection peer)
          (prefix offeredReceived)
          offeredGapEntries
      )
  offer <-
    mapLeft
      show
      ( mkResumeOffer
          (fixturePeerEpoch peer)
          fixtureLocal
          (sequenceAt (peerParameters.mixedIncomingCount + 1))
          (prefix offeredReceived)
          (prefix offeredCompleted)
          offeredGap
      )
  prepared <- mapLeft show (prepareResume offer state)
  let activeAfterCompletion =
        filter
          ((> offeredCompleted) . sequenceInt)
          peerModel.modelOutgoingItems
      offeredGapSequences = Set.fromList (fmap (streamSequenceWord64 . fst) offeredGapEntries)
      mixedExpectedRetransmissions =
        [ item
        | item <- activeAfterCompletion,
          sequenceInt item > offeredReceived,
          Set.notMember
            (fromIntegral (sequenceInt item))
            offeredGapSequences
        ]
      expectedResponseReceived = prefix peerModel.modelIncomingReceived
      expectedResponseCompleted = prefix peerModel.modelIncomingCompleted
  expectEqual "mixed resume retransmissions" mixedExpectedRetransmissions (preparedResumeRetransmissions prepared)
  expectEqual "mixed resume response received" expectedResponseReceived (resumeResponseReceivedPrefix (preparedResumeResponse prepared))
  expectEqual "mixed resume response completed" expectedResponseCompleted (resumeResponseCompletedPrefix (preparedResumeResponse prepared))
  expectEqual
    "mixed resume response cursor"
    (sequenceAt (peerModel.modelIncomingReceived + 1))
    (resumeResponseRetransmitFrom (preparedResumeResponse prepared))
  let (successor, _, _) = commitResume prepared
      received =
        maximum
          [ peerModel.modelOutgoingReceived,
            offeredReceived,
            offeredCompleted
          ]
      retainedGaps =
        normalizeModelGaps
          received
          [ (fromIntegral (streamSequenceWord64 sequenceValue), digest)
          | (sequenceValue, digest) <- offeredGapEntries
          ]
      nextPeerModel =
        peerModel
          { modelOutgoingReceived = received,
            modelOutgoingCompleted = offeredCompleted,
            modelPeerGapEntries = retainedGaps,
            modelIncomingFrontier =
              Just
                ( max
                    (peerParameters.mixedIncomingCount + 1)
                    (maybe 0 id peerModel.modelIncomingFrontier)
                )
          }
      nextModel = Map.insert peer nextPeerModel model
  checkMixedState ("after mixed resume " <> show peer) successor nextModel
  Right (successor, nextModel)

checkMixedState :: String -> State Payload -> MixedModel -> Either String ()
checkMixedState context state model = do
  mapLeft ((context <> ": invariant: ") <>) (mapLeft show (validatePeerStreamState state))
  mapM_ (checkMixedPeer context state model) allFixturePeers

checkMixedPeer ::
  String ->
  State Payload ->
  MixedModel ->
  FixturePeer ->
  Either String ()
checkMixedPeer context state model peer = do
  let peerModel = mixedModelPeer peer model
      outgoingDirection = fixtureOutgoingDirection peer
      incomingDirection = fixtureIncomingDirection peer
      expectedActive =
        filter
          ((> peerModel.modelOutgoingCompleted) . sequenceInt)
          peerModel.modelOutgoingItems
      expectedIncoming =
        [ (mixedIncomingItem peer sequenceNumber, status)
        | (sequenceNumber, status) <- Map.toAscList peerModel.modelIncomingStatuses,
          status /= IncomingCompleted
        ]
      expectedIncomingGaps =
        [ (sequenceAt sequenceNumber, sequencedItemDigest (mixedIncomingItem peer sequenceNumber))
        | (sequenceNumber, status) <- Map.toAscList peerModel.modelIncomingStatuses,
          status == GapRetained
        ]
      label suffix = context <> ": " <> show peer <> " " <> suffix
  active <- mapLeft show (activeOutgoingItems outgoingDirection state)
  outgoingMarks <- mapLeft show (outgoingWatermarks outgoingDirection state)
  outgoingGap <- mapLeft show (outgoingPeerGapSummary outgoingDirection state)
  nextSequence <- mapLeft show (outgoingNextSequence outgoingDirection state)
  incoming <- mapLeft show (incomingRetainedItems incomingDirection state)
  incomingMarks <- mapLeft show (incomingWatermarks incomingDirection state)
  incomingGap <- mapLeft show (incomingGapSummary incomingDirection state)
  frontier <- mapLeft show (incomingGreatestAdvertisedNextSequence incomingDirection state)
  expectEqual (label "active outgoing") expectedActive active
  forM_ peerModel.modelOutgoingItems $ \item -> do
    completed <- mapLeft show (assignmentCompletionKnown (assignmentReceipt outgoingDirection (sequencedItemSequence item) (sequencedItemDigest item)) state)
    expectEqual (label "compact assignment completion") (sequenceInt item <= peerModel.modelOutgoingCompleted) completed
  expectEqual (label "outgoing received") (prefix peerModel.modelOutgoingReceived) (streamReceivedPrefix outgoingMarks)
  expectEqual (label "outgoing completed") (prefix peerModel.modelOutgoingCompleted) (streamCompletedPrefix outgoingMarks)
  expectEqual (label "outgoing next sequence") (sequenceAt (length peerModel.modelOutgoingItems + 1)) nextSequence
  expectEqual (label "outgoing gap direction") outgoingDirection (gapSummaryDirection outgoingGap)
  expectEqual (label "outgoing gap prefix") (prefix peerModel.modelOutgoingReceived) (gapSummaryReceivedPrefix outgoingGap)
  expectEqual
    (label "outgoing gap entries")
    [(sequenceAt index, digest) | (index, digest) <- peerModel.modelPeerGapEntries]
    (gapSummaryEntries outgoingGap)
  expectEqual (label "incoming retained") expectedIncoming incoming
  expectEqual (label "incoming received") (prefix peerModel.modelIncomingReceived) (streamReceivedPrefix incomingMarks)
  expectEqual (label "incoming completed") (prefix peerModel.modelIncomingCompleted) (streamCompletedPrefix incomingMarks)
  expectEqual (label "incoming gap direction") incomingDirection (gapSummaryDirection incomingGap)
  expectEqual (label "incoming gap prefix") (prefix peerModel.modelIncomingReceived) (gapSummaryReceivedPrefix incomingGap)
  expectEqual (label "incoming gap entries") expectedIncomingGaps (gapSummaryEntries incomingGap)
  expectEqual
    (label "incoming advertised frontier")
    (sequenceAt <$> peerModel.modelIncomingFrontier)
    frontier

initialMixedPeerModel :: FixturePeer -> MixedPeerParameters -> MixedPeerModel
initialMixedPeerModel peer parameters =
  MixedPeerModel
    { modelOutgoingItems =
        [ mixedOutgoingItem peer sequenceNumber
        | sequenceNumber <- [1 .. parameters.mixedOutgoingCount]
        ],
      modelOutgoingReceived = 0,
      modelOutgoingCompleted = 0,
      modelPeerGapEntries = [],
      modelIncomingStatuses = Map.empty,
      modelIncomingReceived = 0,
      modelIncomingCompleted = 0,
      modelIncomingFrontier = Nothing
    }

mixedModelPeer :: FixturePeer -> MixedModel -> MixedPeerModel
mixedModelPeer peer model =
  case Map.lookup peer model of
    Nothing -> error "mixed reference model omitted a fixed peer"
    Just peerModel -> peerModel

mixedParameters :: FixturePeer -> Map FixturePeer MixedPeerParameters -> MixedPeerParameters
mixedParameters peer parameters =
  case Map.lookup peer parameters of
    Nothing -> error "mixed trace omitted fixed peer parameters"
    Just peerParameters -> peerParameters

contiguousKeysFrom :: Int -> Map Int status -> [Int]
contiguousKeysFrom start statuses =
  takeWhile (`Map.member` statuses) [start ..]

contiguousStatusPrefix :: (IncomingItemStatus -> Bool) -> Map Int IncomingItemStatus -> Int
contiguousStatusPrefix predicate statuses = go 1
  where
    go sequenceNumber =
      case Map.lookup sequenceNumber statuses of
        Just status
          | predicate status -> go (sequenceNumber + 1)
        _ -> sequenceNumber - 1

normalizeModelGaps :: Int -> [(Int, PeerItemDigest)] -> [(Int, PeerItemDigest)]
normalizeModelGaps received =
  filter ((> received + 1) . fst)

allFixturePeers :: [FixturePeer]
allFixturePeers = [FixturePeerA, FixturePeerB]

fixturePeerEpoch :: FixturePeer -> HeraldEpoch
fixturePeerEpoch FixturePeerA = fixturePeerA
fixturePeerEpoch FixturePeerB = fixturePeerB

fixtureOutgoingDirection :: FixturePeer -> StreamDirection
fixtureOutgoingDirection FixturePeerA = fixtureOutgoingA
fixtureOutgoingDirection FixturePeerB = fixtureDirection 1 3

fixtureIncomingDirection :: FixturePeer -> StreamDirection
fixtureIncomingDirection FixturePeerA = fixtureIncomingA
fixtureIncomingDirection FixturePeerB = fixtureDirection 3 1

mixedPeerSeed :: FixturePeer -> Int
mixedPeerSeed FixturePeerA = 20
mixedPeerSeed FixturePeerB = 100

mixedPeerItem :: FixturePeer -> Int -> PeerItem Payload
mixedPeerItem peer sequenceNumber =
  peerItem
    (fixtureDigest (fromIntegral (mixedPeerSeed peer + sequenceNumber)))
    (Payload (mixedPeerSeed peer + sequenceNumber))

mixedOutgoingItem :: FixturePeer -> Int -> SequencedItem Payload
mixedOutgoingItem peer sequenceNumber =
  sequencedItem
    (fixtureOutgoingDirection peer)
    (sequenceAt sequenceNumber)
    (fixtureDigest (fromIntegral (mixedPeerSeed peer + sequenceNumber)))
    (Payload (mixedPeerSeed peer + sequenceNumber))

mixedIncomingItem :: FixturePeer -> Int -> SequencedItem Payload
mixedIncomingItem peer sequenceNumber =
  sequencedItem
    (fixtureIncomingDirection peer)
    (sequenceAt sequenceNumber)
    (fixtureDigest (fromIntegral (mixedPeerSeed peer + sequenceNumber)))
    (Payload (mixedPeerSeed peer + sequenceNumber))

expectEqual :: (Eq value, Show value) => String -> value -> value -> Either String ()
expectEqual context expected actual
  | expected == actual = Right ()
  | otherwise =
      Left
        ( context
            <> "\nexpected: "
            <> show expected
            <> "\n but got: "
            <> show actual
        )

data ModelParameters = ModelParameters
  { allocated :: Int,
    receivedAck :: Int,
    completedAck :: Int,
    resumeReceived :: Int,
    gapParity :: Bool
  }
  deriving stock (Eq, Show)

modelParameters :: Gen ModelParameters
modelParameters = do
  allocated <- choose (1, 8)
  receivedAck <- choose (0, allocated)
  completedAck <- choose (0, receivedAck)
  resumeReceived <- choose (completedAck, allocated)
  gapParity <- choose (False, True)
  pure ModelParameters {allocated, receivedAck, completedAck, resumeReceived, gapParity}

runModelScenario :: ModelParameters -> IO (Either String ())
runModelScenario parameters = do
  outcome <- runScenarioResult parameters
  pure $ case outcome of
    Left problem -> Left problem
    Right (state, retransmissions) ->
      case (activeOutgoingItems fixtureOutgoingA state, outgoingWatermarks fixtureOutgoingA state) of
        (Right active, Right watermarks)
          | fmap sequenceInt active /= [(parameters.completedAck + 1) .. parameters.allocated] -> Left "active items disagree with model"
          | prefixInt (streamReceivedPrefix watermarks) /= max parameters.receivedAck parameters.resumeReceived -> Left "received watermark disagrees with model"
          | prefixInt (streamCompletedPrefix watermarks) /= parameters.completedAck -> Left "completed watermark disagrees with model"
          | fmap sequenceInt retransmissions /= expectedRetransmissions parameters -> Left "resume retransmissions disagree with model"
          | otherwise -> case validatePeerStreamState state of
              Left problem -> Left (show problem)
              Right () -> Right ()
        (Left problem, _) -> Left (show problem)
        (_, Left problem) -> Left (show problem)

runScenarioResult :: ModelParameters -> IO (Either String (State Payload, [SequencedItem Payload]))
runScenarioResult parameters = pure $ do
  (enqueued, assigned) <- mapLeft show (enqueuePayloadsPure fixturePeerA [1 .. parameters.allocated] (initialState fixtureLocal))
  afterReceived <- commitReceivedPrefix parameters.receivedAck enqueued
  afterCompleted <- commitCompletedPrefix parameters.completedAck afterReceived
  let items = NonEmpty.toList assigned
      gapSequences = modelGapSequences parameters
      gapEntries = [(sequenceAt index, sequencedItemDigest (items !! (index - 1))) | index <- gapSequences]
  gap <- mapLeft show (mkGapSummary fixtureOutgoingA (prefix parameters.resumeReceived) gapEntries)
  offer <- mapLeft show (mkResumeOffer fixturePeerA fixtureLocal sequence1 (prefix parameters.resumeReceived) (prefix parameters.completedAck) gap)
  prepared <- mapLeft show (prepareResume offer afterCompleted)
  let (successor, _, retransmissions) = commitResume prepared
  mapLeft show (validatePeerStreamState successor)
  Right (successor, retransmissions)

expectedRetransmissions :: ModelParameters -> [Int]
expectedRetransmissions parameters =
  [ index
  | index <- [(max parameters.completedAck parameters.resumeReceived + 1) .. parameters.allocated],
    index `notElem` modelGapSequences parameters
  ]

modelGapSequences :: ModelParameters -> [Int]
modelGapSequences parameters =
  [ index
  | index <- [(parameters.resumeReceived + 2) .. parameters.allocated],
    even index == parameters.gapParity
  ]

commitReceivedPrefix :: Int -> State Payload -> Either String (State Payload)
commitReceivedPrefix claimed state = do
  prepared <- mapLeft show (prepareReceivedAck fixtureOutgoingA (prefix claimed) state)
  let (successor, _) = commitReceivedAck prepared
  mapLeft show (validatePeerStreamState successor)
  Right successor

commitCompletedPrefix :: Int -> State Payload -> Either String (State Payload)
commitCompletedPrefix claimed state = do
  prepared <- mapLeft show (prepareCompletedAck fixtureOutgoingA (prefix claimed) state)
  let (successor, _) = commitCompletedAck prepared
  mapLeft show (validatePeerStreamState successor)
  Right successor

dispatchFixture ::
  PeerDispatchBindingGeneration ->
  [Int] ->
  IO
    ( State Payload,
      PeerDispatchTicket,
      NonEmpty (SequencedItem Payload)
    )
dispatchFixture binding values =
  checkedIO "dispatch fixture" (dispatchFixturePure binding values)

dispatchFixturePure ::
  PeerDispatchBindingGeneration ->
  [Int] ->
  Either
    String
    ( State Payload,
      PeerDispatchTicket,
      NonEmpty (SequencedItem Payload)
    )
dispatchFixturePure binding values = do
  requested <- case fmap fixtureItem values of
    [] -> Left "dispatch fixture requires retained work"
    first : rest -> Right (first :| rest)
  bindingPlan <-
    mapLeft
      show
      ( prepareDispatchBinding
          fixtureOutgoingA
          binding
          (initialState fixtureLocal)
      )
  let bound = fst (commitDispatchBinding bindingPlan)
  enqueuePlan <-
    mapLeft show (prepareEnqueue (Map.singleton fixturePeerA requested) bound)
  ticket <-
    case preparedEnqueueDispatchTickets enqueuePlan of
      [scheduled] -> Right scheduled
      tickets -> Left ("expected one dispatch ticket, observed " <> show tickets)
  let (ready, assignments) = commitEnqueue enqueuePlan
  assigned <-
    maybe
      (Left "dispatch enqueue omitted assignments")
      Right
      (Map.lookup fixturePeerA assignments)
  Right (ready, ticket, assigned)

selectDispatch ::
  PeerDispatchBindingGeneration ->
  PeerDispatchTicket ->
  State Payload ->
  IO (State Payload, PeerDispatchAttempt Payload)
selectDispatch binding ticket state =
  checkedIO "dispatch selection" (selectDispatchPure binding ticket state)

selectDispatchPure ::
  PeerDispatchBindingGeneration ->
  PeerDispatchTicket ->
  State Payload ->
  Either String (State Payload, PeerDispatchAttempt Payload)
selectDispatchPure binding ticket state = do
  plan <- mapLeft show (prepareDispatchSelection ticket binding state)
  attempt <-
    maybe
      (Left "current ticket did not select an attempt")
      Right
      (preparedDispatchSelectionAttempt plan)
  let (successor, committed) = commitDispatchSelection plan
  if committed == Just attempt
    then Right (successor, attempt)
    else Left "selection commit disagreed with prepared attempt"

runDispatchRetryTrace ::
  [PeerDispatchOutcome] ->
  IO (Either String (PeerDispatchAttempt Payload))
runDispatchRetryTrace outcomes = pure $ do
  let binding = fixtureBinding 1
  (ready, firstTicket, _) <- dispatchFixturePure binding [1]
  (beforeFinal, finalTicket) <-
    foldM
      (retryOnce binding)
      (ready, firstTicket)
      outcomes
  (attempting, finalAttempt) <-
    selectDispatchPure binding finalTicket beforeFinal
  written <-
    mapLeft
      show
      (prepareDispatchOutcome finalAttempt PeerDispatchWritten attempting)
  let settled = fst (commitDispatchOutcome written)
  mapLeft show (validatePeerStreamState settled)
  awaiting <- mapLeft show (dispatchAwaitingSequences fixtureOutgoingA settled)
  if awaiting == Set.singleton sequence1
    then Right finalAttempt
    else Left "final Written outcome did not retain exact protocol wait"
  where
    retryOnce binding (state, ticket) outcome = do
      (attempting, attempt) <- selectDispatchPure binding ticket state
      observed <- mapLeft show (prepareDispatchOutcome attempt outcome attempting)
      nextTicket <-
        maybe
          (Left "failed/deferred outcome omitted retry ticket")
          Right
          (preparedDispatchOutcomeTicket observed)
      let successor = fst (commitDispatchOutcome observed)
      mapLeft show (validatePeerStreamState successor)
      Right (successor, nextTicket)

attemptGenerationWord64 :: PeerDispatchAttempt payload -> Word64
attemptGenerationWord64 =
  peerDispatchAttemptGenerationWord64 . peerDispatchAttemptGeneration

assertDispatchContradiction ::
  PeerDispatchAttempt Payload ->
  PeerDispatchOutcome ->
  PeerDispatchOutcome ->
  State Payload ->
  Assertion
assertDispatchContradiction attempt retained observed state =
  case prepareDispatchOutcome attempt observed state of
    Left
      ( PeerStreamInvariantProblem
          ( PeerStreamDispatchOutcomeContradiction
              direction
              generation
              actualRetained
              actualObserved
            )
        ) -> do
        assertEqual "contradiction direction" fixtureOutgoingA direction
        assertEqual "contradiction generation" (peerDispatchAttemptGeneration attempt) generation
        assertEqual "retained outcome" retained actualRetained
        assertEqual "observed outcome" observed actualObserved
    Left problem -> assertFailure ("wrong dispatch contradiction: " <> show problem)
    Right _ -> assertFailure "contradictory latest outcome exposed a successor"

exactlyOne :: String -> [value] -> IO value
exactlyOne _ [value] = pure value
exactlyOne context values =
  assertFailure (context <> ": expected one value, observed " <> show (length values))

uniqueTicketFor ::
  StreamDirection ->
  [PeerDispatchTicket] ->
  IO PeerDispatchTicket
uniqueTicketFor direction =
  exactlyOne "dispatch ticket for direction"
    . filter ((== direction) . peerDispatchTicketDirection)

requiredMaybe :: String -> Maybe value -> IO value
requiredMaybe _ (Just value) = pure value
requiredMaybe context Nothing = assertFailure context

newtype Payload = Payload Int
  deriving stock (Eq, Show)

fixtureLocal :: HeraldEpoch
fixtureLocal = fixtureHerald 1

fixturePeerA :: HeraldEpoch
fixturePeerA = fixtureHerald 2

fixturePeerB :: HeraldEpoch
fixturePeerB = fixtureHerald 3

fixtureOutgoingA :: StreamDirection
fixtureOutgoingA = fixtureDirection 1 2

fixtureBinding :: Word64 -> PeerDispatchBindingGeneration
fixtureBinding generation =
  checked
    "dispatch binding generation"
    (mkPeerDispatchBindingGeneration generation)

fixtureIncomingA :: StreamDirection
fixtureIncomingA = fixtureDirection 2 1

sequence1, sequence2, sequence3, sequence4 :: StreamSequence
sequence1 = sequenceAt 1
sequence2 = sequenceAt 2
sequence3 = sequenceAt 3
sequence4 = sequenceAt 4

sequenceAt :: Int -> StreamSequence
sequenceAt value = checked "stream sequence" (mkStreamSequence (fromIntegral value))

prefix :: Int -> StreamPrefix
prefix 0 = emptyStreamPrefix
prefix value = streamPrefixThrough (sequenceAt value)

prefixInt :: StreamPrefix -> Int
prefixInt = maybe 0 (fromIntegral . streamSequenceWord64) . streamPrefixSequence

fixtureItem :: Int -> PeerItem Payload
fixtureItem value = peerItem (fixtureDigest (fromIntegral value)) (Payload value)

sequenceBoundFixtureItem :: StreamDirection -> StreamSequence -> PeerItem Payload
sequenceBoundFixtureItem direction sequenceNumber =
  fixtureItem
    ( directionBase
        + fromIntegral (streamSequenceWord64 sequenceNumber)
    )
  where
    directionBase
      | direction == fixtureOutgoingA = 40
      | otherwise = 80

inboundItem :: Int -> SequencedItem Payload
inboundItem value =
  sequencedItem fixtureIncomingA (sequenceAt value) (fixtureDigest (fromIntegral value)) (Payload value)

assignedFor :: HeraldEpoch -> Map HeraldEpoch (NonEmpty item) -> [item]
assignedFor peer assignments = maybe [] NonEmpty.toList (Map.lookup peer assignments)

candidateSequences :: PreparedReceiveCandidate payload -> [StreamSequence]
candidateSequences = fmap sequencedItemSequence . preparedContiguousItems

commitCandidate ::
  Map StreamSequence ReceiveProgress ->
  PreparedReceiveCandidate Payload ->
  IO (State Payload)
commitCandidate progress candidate = do
  finalized <- checkedIO "receive refinement" (finalizeReceive progress candidate)
  let (successor, _) = commitReceive finalized
  assertOwnerInvariant successor
  pure successor

receivePending :: [Int] -> IO (State Payload)
receivePending = foldM receiveOne (initialState fixtureLocal)
  where
    receiveOne state value = do
      candidate <- checkedIO "contiguous receive" (prepareReceiveCandidate (inboundItem value) state)
      commitCandidate (Map.singleton (sequenceAt value) ReceivedPending) candidate

enqueuePayloads ::
  HeraldEpoch ->
  [Int] ->
  State Payload ->
  IO (State Payload, NonEmpty (SequencedItem Payload))
enqueuePayloads peer values state = checkedIO "enqueue payloads" (enqueuePayloadsPure peer values state)

enqueuePayloadsPure ::
  HeraldEpoch ->
  [Int] ->
  State Payload ->
  Either PeerStreamProblem (State Payload, NonEmpty (SequencedItem Payload))
enqueuePayloadsPure peer values state = do
  requested <- case fmap fixtureItem values of
    [] -> error "test enqueue requires a non-empty batch"
    first : rest -> Right (first :| rest)
  prepared <- prepareEnqueue (Map.singleton peer requested) state
  let (successor, assignments) = commitEnqueue prepared
  case Map.lookup peer assignments of
    Nothing -> error "successful enqueue omitted its peer assignment"
    Just assigned -> Right (successor, assigned)

emptyResumeOffer ::
  HeraldEpoch ->
  HeraldEpoch ->
  StreamSequence ->
  IO ResumeOffer
emptyResumeOffer source destination frontier = do
  reverseDirection <- checkedIO "reverse resume direction" (mkStreamDirection destination source)
  gap <- checkedIO "empty resume gap" (mkGapSummary reverseDirection emptyStreamPrefix [])
  checkedIO "empty resume offer" (mkResumeOffer source destination frontier emptyStreamPrefix emptyStreamPrefix gap)

assertOwnerInvariant :: State payload -> Assertion
assertOwnerInvariant state = do
  assertEqual "owner invariant" (Right ()) (validatePeerStreamState state)
  case peerStreamStateWitness state of
    Left problem -> assertFailure ("owner witness failed: " <> show problem)
    Right witness ->
      assertEqual
        "narrow incoming directions preserve the full witness"
        (Right (peerStreamWitnessIncomingDirections witness))
        (peerStreamIncomingDirections state)

assertProgressMismatch ::
  Set.Set StreamSequence ->
  Set.Set StreamSequence ->
  Either PeerStreamProblem prepared ->
  Assertion
assertProgressMismatch expected supplied result =
  case result of
    Left (PeerStreamInvariantProblem (PeerStreamReceiveProgressMismatch actualExpected actualSupplied)) -> do
      assertEqual "expected refinement keys" expected actualExpected
      assertEqual "supplied refinement keys" supplied actualSupplied
    Left problem -> assertFailure ("wrong receive-refinement error: " <> show problem)
    Right _ -> assertFailure "invalid receive refinement produced a successor"

assertAckAhead ::
  StreamPrefix ->
  StreamPrefix ->
  Either PeerStreamProblem prepared ->
  Assertion
assertAckAhead claimed lastAllocated result =
  case result of
    Left
      ( PeerStreamProtocolProblem
          (PeerStreamAcknowledgementAhead direction actualClaim actualLast)
        ) -> do
        assertEqual "ack direction" fixtureOutgoingA direction
        assertEqual "ack claim" claimed actualClaim
        assertEqual "last allocated" lastAllocated actualLast
    Left problem -> assertFailure ("wrong acknowledgement error: " <> show problem)
    Right _ -> assertFailure "ahead acknowledgement produced a successor"

assertProblem :: String -> PeerStreamProblem -> Either PeerStreamProblem value -> Assertion
assertProblem context expected result =
  case result of
    Left actual -> assertEqual context expected actual
    Right _ -> assertFailure (context <> ": failure exposed a successor")

sequenceInt :: SequencedItem payload -> Int
sequenceInt = fromIntegral . streamSequenceWord64 . sequencedItemSequence

mapLeft :: (problem -> other) -> Either problem value -> Either other value
mapLeft convert = either (Left . convert) Right

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

fixtureHerald :: Word8 -> HeraldEpoch
fixtureHerald seed =
  checked
    "HeraldEpoch"
    (mkHeraldEpoch (ByteString.pack [seed + fromIntegral index | index <- [(0 :: Int) .. 31]]))

fixtureDirection :: Word8 -> Word8 -> StreamDirection
fixtureDirection source destination =
  checked "stream direction" (mkStreamDirection (fixtureHerald source) (fixtureHerald destination))

fixtureDigest :: Word8 -> PeerItemDigest
fixtureDigest seed = checked "peer digest" (mkPeerItemDigest (ByteString.replicate 32 seed))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
