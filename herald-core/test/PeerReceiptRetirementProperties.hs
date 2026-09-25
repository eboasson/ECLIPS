module PeerReceiptRetirementProperties (tests) where

import Control.Monad (foldM, forM_)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity (HeraldEpoch, mkHeraldEpoch)
import Eclips.Herald.PeerStream qualified as Stream
import Eclips.Herald.PeerStream.State qualified as Peer
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "peer sparse receipt retirement"
    [ QC.testProperty "one pending head retains constant receipt work across growing completed suffixes" propPendingHead,
      QC.testProperty "completion alone replaces receipt for arbitrary held items and receive ordering" propCompletionSubsumesReceipt,
      testCase "a reconnect carries sparse completion and stale claims cannot reopen released positions" caseResume,
      testCase "completion includes gaps only after contiguous receipt and semantic admission" caseGap,
      testCase "completed gap certificates do not survive in raw Resume history" caseGapHistory
    ]

propPendingHead :: QC.Positive Int -> QC.Property
propPendingHead (QC.Positive generated) = QC.ioProperty $ do
  let cycles = 1 + generated `mod` 150
  (source, destination, direction, states) <- pendingCycles cycles
  forM_ states $ \(sender, receiver) -> do
    assertEqual "only pending source payload remains" (1, 0, 0) (Peer.peerStreamRetentionCounts sender)
    assertEqual "only pending destination payload remains" (0, 1, 0) (Peer.peerStreamRetentionCounts receiver)
    progress <- checked (Peer.outgoingCompletionProgress direction sender)
    assertEqual "one exception represents the one pending lifetime" (Set.singleton 1) (Receipt.receiptRetirementExceptions progress)
    assertEqual "pending head still blocks a cumulative causal barrier" Stream.emptyStreamPrefix (Stream.streamCompletionPrefix progress)
    checked (Peer.validatePeerStreamState sender)
    checked (Peer.validatePeerStreamState receiver)
  let (sender, receiver) = lastPair states
  complete <- checked (Peer.prepareCompletion direction (Set.singleton Stream.firstStreamSequence) receiver)
  let (completedReceiver, watermarks) = Peer.commitCompletion complete
  acknowledged <- checked (Peer.prepareCompletedProgress direction (Stream.streamCompletionProgress watermarks) sender)
  let (completedSender, _) = Peer.commitCompletedAck acknowledged
  assertEqual "last pending receipt drains source" (0, 0, 0) (Peer.peerStreamRetentionCounts completedSender)
  assertEqual "last pending receipt drains destination" (0, 0, 0) (Peer.peerStreamRetentionCounts completedReceiver)
  assertEqual "allocator survives release" (Right (sequenceAt (cycles + 2))) (Peer.outgoingNextSequence direction completedSender)
  -- Old ordinary and standalone certificates are both release-set union. Neither
  -- can restore the old exception once its consumer has finished.
  forM_ states $ \(oldSender, _) -> do
    old <- checked (Peer.outgoingCompletionProgress direction oldSender)
    replay <- checked (Peer.prepareCompletedProgress direction old completedSender)
    assertEqual "late sparse acknowledgement leaves no rearmed receipt" (0, 0, 0) (Peer.peerStreamRetentionCounts (fst (Peer.commitCompletedAck replay)))
  assertEqual "both directions retain their exact peer identity" (source, destination) (Stream.streamDirectionSource direction, Stream.streamDirectionDestination direction)
  pure True

-- Vary the unresolved positions and delivery order independently. A future gap
-- remains beyond the contiguous received frontier throughout, so neither receipt
-- form may suppress its genuinely missing predecessor during reconnect repair.
propCompletionSubsumesReceipt :: QC.NonEmptyList Bool -> QC.Property
propCompletionSubsumesReceipt (QC.NonEmpty generated) =
  QC.forAll (QC.shuffle [1 .. count]) $ \order -> QC.ioProperty $ do
    enqueued <- checked (Peer.prepareEnqueue (Map.singleton epochB (Stream.peerItem digest () :| replicate (count + 1) (Stream.peerItem digest ()))) (Peer.initialState epochA))
    receiver <- receive Stream.Completed (itemAt directionAB (count + 2)) (Peer.initialState epochB)
    (sender, received) <- foldM receiveAndAcknowledge (fst (Peer.commitEnqueue enqueued), receiver) order
    let pending = [sequenceAt index | (index, completed) <- zip [1 ..] choices, not completed]
    (completedSender, completedReceiver) <- foldM completeAndAcknowledge (sender, received) pending
    assertEqual "only the missing predecessor and future gap remain at source" (2, 0, 0) (Peer.peerStreamRetentionCounts completedSender)
    assertEqual "the future gap remains retained at destination" (0, 1, 0) (Peer.peerStreamRetentionCounts completedReceiver)
    offer <- checked (Peer.resumeOfferForPeer epochA completedReceiver)
    resume <- checked (Peer.prepareResume offer completedSender)
    let (resumed, _, retransmissions) = Peer.commitResume resume
    assertEqual "reconnect repairs exactly the missing predecessor" [sequenceAt (count + 1)] (Stream.sequencedItemSequence <$> retransmissions)
    checked (Peer.validatePeerStreamState resumed)
    pure True
  where
    choices = take 60 generated
    count = length choices
    dispositions = Map.fromList [(sequenceAt index, if completed then Stream.Completed else Stream.ReceivedPending) | (index, completed) <- zip [1 ..] choices]
    receiveAndAcknowledge (sender, receiver) index = do
      candidate <- checked (Peer.prepareReceiveCandidate (itemAt directionAB index) receiver)
      finalized <- checked (Peer.finalizeReceive (Map.restrictKeys dispositions (Set.fromList (Stream.sequencedItemSequence <$> Peer.preparedContiguousItems candidate))) candidate)
      let (received, _) = Peer.commitReceive finalized
      acknowledged <- compareAcknowledgements sender received
      pure (acknowledged, received)
    completeAndAcknowledge (sender, receiver) sequenceNumber = do
      completion <- checked (Peer.prepareCompletion directionAB (Set.singleton sequenceNumber) receiver)
      let (completed, _) = Peer.commitCompletion completion
      acknowledged <- compareAcknowledgements sender completed
      pure (acknowledged, completed)
    compareAcknowledgements sender receiver = do
      watermarks <- checked (Peer.incomingWatermarks directionAB receiver)
      received <- checked (Peer.prepareReceivedAck directionAB (Stream.streamReceivedPrefix watermarks) sender)
      paired <- checked (Peer.prepareCompletedProgress directionAB (Stream.streamCompletionProgress watermarks) (fst (Peer.commitReceivedAck received)))
      single <- checked (Peer.prepareCompletedProgress directionAB (Stream.streamCompletionProgress watermarks) sender)
      assertEqual "completion-only and receipt-plus-completion produce the same sender" (Peer.commitCompletedAck paired) (Peer.commitCompletedAck single)
      let (acknowledged, observed) = Peer.commitCompletedAck single
      assertEqual "completion-only certifies the entire received frontier" (Stream.streamReceivedPrefix watermarks) (Stream.streamReceivedPrefix observed)
      assertEqual "completion-only preserves every pending exception" (Stream.streamCompletionProgress watermarks) (Stream.streamCompletionProgress observed)
      checked (Peer.validatePeerStreamState acknowledged)
      pure acknowledged

caseResume :: IO ()
caseResume = do
  (source, _, direction, states) <- pendingCycles 40
  let (sender, receiver) = lastPair states
  offer <- checked (Peer.resumeOfferForPeer source receiver)
  resume <- checked (Peer.prepareResume offer sender)
  let (resumed, _, retransmissions) = Peer.commitResume resume
  assertEqual "received pending work is not retransmitted" [] retransmissions
  assertEqual "reconnect preserves sparse receipt release" (1, 0, 0) (Peer.peerStreamRetentionCounts resumed)
  assertEqual "reconnect needs no acknowledgement of its completion certificate" Nothing (Peer.preparedResumeDispatchTicket resume)
  forM_ states $ \(_, oldReceiver) -> do
    staleOffer <- checked (Peer.resumeOfferForPeer source oldReceiver)
    stale <- checked (Peer.prepareResume staleOffer resumed)
    let (afterStale, _, _) = Peer.commitResume stale
    assertEqual "stale resume cannot restore completed suffixes" (1, 0, 0) (Peer.peerStreamRetentionCounts afterStale)
  duplicate <- checked (Peer.prepareReceiveCandidate (itemAt direction 2) receiver)
  assertEqual "released sequence is a duplicate without semantic delivery" Stream.ReceiveDuplicate (Peer.preparedReceiveDisposition duplicate)
  assertEqual "released sequence supplies no work to semantic consumers" [] (Peer.preparedContiguousItems duplicate)

caseGap :: IO ()
caseGap = do
  let direction = directionAB
      receiver = Peer.initialState epochB
  gap <- checked (Peer.prepareReceiveCandidate (itemAt direction 3) receiver)
  gapPlan <- checked (Peer.finalizeReceive Map.empty gap)
  let (withGap, _) = Peer.commitReceive gapPlan
  assertEqual "future payload remains until its missing predecessors arrive" (0, 1, 0) (Peer.peerStreamRetentionCounts withGap)
  first <- receive Stream.ReceivedPending (itemAt direction 1) withGap
  second <- checked (Peer.prepareReceiveCandidate (itemAt direction 2) first)
  complete <- checked (Peer.finalizeReceive (Map.fromList [(sequenceAt 2, Stream.Completed), (sequenceAt 3, Stream.Completed)]) second)
  let (finished, watermarks) = Peer.commitReceive complete
  assertEqual "the completed suffix is released across the fixed pending head" (0, 1, 0) (Peer.peerStreamRetentionCounts finished)
  assertEqual "sparse completion retains exactly the head exception" (Set.singleton 1) (Receipt.receiptRetirementExceptions (Stream.streamCompletionProgress (Stream.receiveResultWatermarks watermarks)))

caseGapHistory :: IO ()
caseGapHistory = do
  enqueued <- checked (Peer.prepareEnqueue (Map.singleton epochB (Stream.peerItem digest () :| replicate 39 (Stream.peerItem digest ()))) (Peer.initialState epochA))
  let (source, _) = Peer.commitEnqueue enqueued
  gap <- checked (Stream.mkGapSummary directionAB Stream.emptyStreamPrefix [(sequenceAt index, digest) | index <- [2 .. 40]])
  offer <- checked (Stream.mkResumeOffer epochB epochA Stream.firstStreamSequence Stream.emptyStreamPrefix Stream.emptyStreamPrefix gap)
  resume <- checked (Peer.prepareResume offer source)
  let (resumed, _, _) = Peer.commitResume resume
  assertEqual "the outstanding gap claim has a live consumer" 39 (let (_, _, receipts) = Peer.peerStreamRetentionCounts resumed in receipts)
  completed <- checked (Peer.prepareCompletedProgress directionAB (Receipt.receiptRetirementPrefix (Just 40)) resumed)
  let (released, _) = Peer.commitCompletedAck completed
  assertEqual "no completed raw gap proof survives" (0, 0, 0) (Peer.peerStreamRetentionCounts released)
  duplicate <- checked (Peer.prepareResumeForAssociation Peer.ContinuingResumeOnAssociation offer released)
  let (afterDuplicate, _, retransmissions) = Peer.commitResume duplicate
  assertEqual "stale gap claim cannot reopen released assignments" [] retransmissions
  assertEqual "stale gap claim cannot restore discarded history" (0, 0, 0) (Peer.peerStreamRetentionCounts afterDuplicate)

pendingCycles :: Int -> IO (HeraldEpoch, HeraldEpoch, Stream.StreamDirection, [(Peer.State (), Peer.State ())])
pendingCycles count = do
  sender <- enqueue (Peer.initialState epochA)
  receiver <- receive Stream.ReceivedPending (itemAt directionAB 1) (Peer.initialState epochB)
  first <- acknowledge sender receiver
  states <- foldM advance [first] [2 .. count + 1]
  pure (epochA, epochB, directionAB, states)
  where
    advance history sequenceNumber = do
      let (sender, receiver) = lastPair history
      allocated <- enqueue sender
      received <- receive Stream.Completed (itemAt directionAB sequenceNumber) receiver
      next <- acknowledge allocated received
      pure (next : history)
    acknowledge sender receiver = do
      watermarks <- checked (Peer.incomingWatermarks directionAB receiver)
      prepared <- checked (Peer.prepareCompletedProgress directionAB (Stream.streamCompletionProgress watermarks) sender)
      pure (fst (Peer.commitCompletedAck prepared), receiver)
    enqueue state = do
      prepared <- checked (Peer.prepareEnqueue (Map.singleton epochB (Stream.peerItem digest () :| [])) state)
      pure (fst (Peer.commitEnqueue prepared))

receive :: Stream.ReceiveProgress -> Stream.SequencedItem () -> Peer.State () -> IO (Peer.State ())
receive progress item state = do
  candidate <- checked (Peer.prepareReceiveCandidate item state)
  prepared <- checked (Peer.finalizeReceive (Map.fromList [(Stream.sequencedItemSequence admitted, progress) | admitted <- Peer.preparedContiguousItems candidate]) candidate)
  pure (fst (Peer.commitReceive prepared))

-- History is test-only; production retains only the owners counted above.
lastPair :: [a] -> a
lastPair [] = error "test history is nonempty"
lastPair (value : _) = value

itemAt :: Stream.StreamDirection -> Int -> Stream.SequencedItem ()
itemAt direction index = Stream.sequencedItem direction (sequenceAt index) digest ()

sequenceAt :: Int -> Stream.StreamSequence
sequenceAt = either (error . show) id . Stream.mkStreamSequence . fromIntegral

epochA, epochB :: HeraldEpoch
epochA = either (error . show) id (mkHeraldEpoch (ByteString.replicate 32 1))
epochB = either (error . show) id (mkHeraldEpoch (ByteString.replicate 32 2))

directionAB :: Stream.StreamDirection
directionAB = either (error . show) id (Stream.mkStreamDirection epochA epochB)

digest :: Stream.PeerItemDigest
digest = either (error . show) id (Stream.mkPeerItemDigest (ByteString.replicate 32 3))

checked :: (Show problem) => Either problem value -> IO value
checked = either (\problem -> assertFailure (show problem)) pure
