{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Nominal vocabulary for one logical, epoch-qualified peer stream.
--
-- This module deliberately fixes a typed transcript rather than a wire format.
-- The complete payload remains typed and is never fingerprinted by this owner.
module Eclips.Herald.PeerStream
  ( StreamSequence,
    mkStreamSequence,
    firstStreamSequence,
    nextStreamSequence,
    streamSequenceWord64,
    StreamPrefix,
    emptyStreamPrefix,
    streamPrefixThrough,
    streamPrefixSequence,
    nextAfterStreamPrefix,
    StreamDirection,
    mkStreamDirection,
    reverseStreamDirection,
    streamDirectionSource,
    streamDirectionDestination,
    PeerItemDigest,
    mkPeerItemDigest,
    peerItemDigestBytes,
    PeerItem,
    peerItem,
    peerItemDigest,
    peerItemPayload,
    SequencedItem,
    sequencedItem,
    sequencedItemDirection,
    sequencedItemSequence,
    sequencedItemDigest,
    sequencedItemPayload,
    PeerDispatchBindingGeneration,
    mkPeerDispatchBindingGeneration,
    peerDispatchBindingGenerationWord64,
    PeerDispatchAttemptGeneration,
    peerDispatchAttemptGenerationForOwner,
    peerDispatchAttemptGenerationWord64,
    PeerDispatchTicket,
    peerDispatchTicket,
    peerDispatchTicketDirection,
    peerDispatchTicketGenerationWord64,
    PeerDispatchAttempt,
    peerDispatchAttempt,
    peerDispatchAttemptBindingGeneration,
    peerDispatchAttemptGeneration,
    peerDispatchAttemptDirection,
    peerDispatchAttemptItem,
    PeerDispatchOutcome (..),
    GapSummary,
    mkGapSummary,
    gapSummaryDirection,
    gapSummaryReceivedPrefix,
    gapSummaryEntries,
    ReceiveProgress (..),
    ReceiveDisposition (..),
    StreamWatermarks,
    streamWatermarks,
    streamWatermarksWithCompletion,
    streamCompletionProgress,
    streamCompletionPrefix,
    streamPrefixCompletion,
    checkStreamCompletion,
    streamReceivedPrefix,
    streamCompletedPrefix,
    ReceiveResult,
    receiveResult,
    receiveResultDisposition,
    receiveResultWatermarks,
    receiveResultGapSummary,
    AssignmentReceipt,
    assignmentReceipt,
    assignmentReceiptDirection,
    assignmentReceiptSequence,
    assignmentReceiptDigest,
    ResumeOffer,
    mkResumeOffer,
    mkResumeOfferWithCompletion,
    resumeOfferCompletion,
    resumeOfferSource,
    resumeOfferDestination,
    resumeOfferNextSourceSequence,
    resumeOfferReceivedPrefix,
    resumeOfferCompletedPrefix,
    resumeOfferGapSummary,
    ResumeResponse,
    resumeResponse,
    resumeResponseWithCompletion,
    resumeResponseCompletion,
    resumeResponseReceivedPrefix,
    resumeResponseCompletedPrefix,
    resumeResponseRetransmitFrom,
    StreamShapeProblem (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sortOn)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Public.Types.ReceiptRetirement

-- | A positive sequence in one direction.  Sequence zero is never a sentinel.
newtype StreamSequence = StreamSequence Word64
  deriving stock (Eq, Ord, Show)

-- | Structural failures in nominal stream values.
data StreamShapeProblem
  = StreamSequenceMustBePositive
  | PeerItemDigestWrongByteCount Int
  | StreamDirectionEndpointsEqual HeraldEpoch
  | GapEntriesNotStrictlyAscending
  | GapEntryNotBeyondFirstMissing StreamSequence
  | ResumeGapDirectionMismatch StreamDirection StreamDirection
  | ResumeGapPrefixMismatch StreamPrefix StreamPrefix
  | ResumeCompletedBeyondReceived StreamPrefix StreamPrefix
  | StreamCompletionMustBePositive
  | PeerDispatchBindingGenerationMustBePositive
  deriving stock (Eq, Show)

mkStreamSequence :: Word64 -> Either StreamShapeProblem StreamSequence
mkStreamSequence 0 = Left StreamSequenceMustBePositive
mkStreamSequence value = Right (StreamSequence value)

firstStreamSequence :: StreamSequence
firstStreamSequence = StreamSequence 1

-- | Advance a positive sequence.  Exhaustion is outside profile 0.1.
nextStreamSequence :: StreamSequence -> StreamSequence
nextStreamSequence (StreamSequence value) = StreamSequence (value + 1)

streamSequenceWord64 :: StreamSequence -> Word64
streamSequenceWord64 (StreamSequence value) = value

-- | A cumulative prefix with an explicit empty case.
data StreamPrefix
  = EmptyStreamPrefix
  | StreamPrefixThrough StreamSequence
  deriving stock (Eq, Ord, Show)

emptyStreamPrefix :: StreamPrefix
emptyStreamPrefix = EmptyStreamPrefix

streamPrefixThrough :: StreamSequence -> StreamPrefix
streamPrefixThrough = StreamPrefixThrough

streamPrefixSequence :: StreamPrefix -> Maybe StreamSequence
streamPrefixSequence EmptyStreamPrefix = Nothing
streamPrefixSequence (StreamPrefixThrough sequenceNumber) = Just sequenceNumber

nextAfterStreamPrefix :: StreamPrefix -> StreamSequence
nextAfterStreamPrefix EmptyStreamPrefix = firstStreamSequence
nextAfterStreamPrefix (StreamPrefixThrough sequenceNumber) =
  nextStreamSequence sequenceNumber

-- | Ordered source and destination epochs of one logical direction.
data StreamDirection = StreamDirection HeraldEpoch HeraldEpoch
  deriving stock (Eq, Ord, Show)

mkStreamDirection ::
  HeraldEpoch ->
  HeraldEpoch ->
  Either StreamShapeProblem StreamDirection
mkStreamDirection source destination
  | source == destination = Left (StreamDirectionEndpointsEqual source)
  | otherwise = Right (StreamDirection source destination)

reverseStreamDirection :: StreamDirection -> StreamDirection
reverseStreamDirection (StreamDirection source destination) =
  StreamDirection destination source

streamDirectionSource :: StreamDirection -> HeraldEpoch
streamDirectionSource (StreamDirection source _) = source

streamDirectionDestination :: StreamDirection -> HeraldEpoch
streamDirectionDestination (StreamDirection _ destination) = destination

-- | Stable identity supplied for one complete immutable item.
newtype PeerItemDigest = PeerItemDigest ByteString
  deriving stock (Eq, Ord)

instance Show PeerItemDigest where
  show = renderGroupedHex . peerItemDigestBytes

mkPeerItemDigest :: ByteString -> Either StreamShapeProblem PeerItemDigest
mkPeerItemDigest bytes
  | ByteString.length bytes == 32 = Right (PeerItemDigest bytes)
  | otherwise = Left (PeerItemDigestWrongByteCount (ByteString.length bytes))

peerItemDigestBytes :: PeerItemDigest -> ByteString
peerItemDigestBytes (PeerItemDigest bytes) = bytes

-- | One unassigned complete payload request.
data PeerItem payload = PeerItem PeerItemDigest payload
  deriving stock (Eq, Show)

peerItem :: PeerItemDigest -> payload -> PeerItem payload
peerItem = PeerItem

peerItemDigest :: PeerItem payload -> PeerItemDigest
peerItemDigest (PeerItem digest _) = digest

peerItemPayload :: PeerItem payload -> payload
peerItemPayload (PeerItem _ payload) = payload

-- | One complete payload after allocation in an exact direction.
data SequencedItem payload = SequencedItem
  { direction :: StreamDirection,
    sequenceNumber :: StreamSequence,
    digest :: PeerItemDigest,
    payload :: payload
  }
  deriving stock (Eq, Show)

sequencedItem ::
  StreamDirection ->
  StreamSequence ->
  PeerItemDigest ->
  payload ->
  SequencedItem payload
sequencedItem direction sequenceNumber digest payload =
  SequencedItem {direction, sequenceNumber, digest, payload}

sequencedItemDirection :: SequencedItem payload -> StreamDirection
sequencedItemDirection item = item.direction

sequencedItemSequence :: SequencedItem payload -> StreamSequence
sequencedItemSequence item = item.sequenceNumber

sequencedItemDigest :: SequencedItem payload -> PeerItemDigest
sequencedItemDigest item = item.digest

sequencedItemPayload :: SequencedItem payload -> payload
sequencedItemPayload item = item.payload

-- | Discovery-owned positive logical binding generation observed by this owner.
--
-- This nominal reference is deliberately independent of Discovery's
-- representation, keeping the payload-parametric stream leaf isolated from the
-- discovery owner.  The Step-7 coordinator converts only an already admitted
-- Discovery generation through this checked shape seam.
newtype PeerDispatchBindingGeneration = PeerDispatchBindingGeneration Word64
  deriving stock (Eq, Ord, Show)

mkPeerDispatchBindingGeneration ::
  Word64 ->
  Either StreamShapeProblem PeerDispatchBindingGeneration
mkPeerDispatchBindingGeneration 0 = Left PeerDispatchBindingGenerationMustBePositive
mkPeerDispatchBindingGeneration value = Right (PeerDispatchBindingGeneration value)

peerDispatchBindingGenerationWord64 :: PeerDispatchBindingGeneration -> Word64
peerDispatchBindingGenerationWord64 (PeerDispatchBindingGeneration value) = value

-- | Positive owner-minted attempt generation in one exact direction.
newtype PeerDispatchAttemptGeneration = PeerDispatchAttemptGeneration Word64
  deriving stock (Eq, Ord, Show)

peerDispatchAttemptGenerationWord64 :: PeerDispatchAttemptGeneration -> Word64
peerDispatchAttemptGenerationWord64 (PeerDispatchAttemptGeneration value) = value

-- | Package-private construction seam used only by the stream state owner.
peerDispatchAttemptGenerationForOwner :: Word64 -> PeerDispatchAttemptGeneration
peerDispatchAttemptGenerationForOwner = PeerDispatchAttemptGeneration

-- | One coalesced owner-minted scheduling correlation for a direction.
--
-- The private construction function exists only for the sibling state owner.
-- Its monotonically increasing generation makes a late selection stale after a
-- ticket has been consumed and replaced.
data PeerDispatchTicket = PeerDispatchTicket StreamDirection Word64
  deriving stock (Eq, Ord)

instance Show PeerDispatchTicket where
  show _ = "PeerDispatchTicket"

peerDispatchTicket :: StreamDirection -> Word64 -> PeerDispatchTicket
peerDispatchTicket = PeerDispatchTicket

peerDispatchTicketDirection :: PeerDispatchTicket -> StreamDirection
peerDispatchTicketDirection (PeerDispatchTicket direction _) = direction

peerDispatchTicketGenerationWord64 :: PeerDispatchTicket -> Word64
peerDispatchTicketGenerationWord64 (PeerDispatchTicket _ generation) = generation

-- | One exact retained item selected for an admitted logical binding.
--
-- Direction is retained by the sequenced item and exposed separately so every
-- observation remains direction-qualified without consulting owner state.
data PeerDispatchAttempt payload
  = PeerDispatchAttempt
      PeerDispatchBindingGeneration
      PeerDispatchAttemptGeneration
      (SequencedItem payload)
  deriving stock (Eq)

instance Show (PeerDispatchAttempt payload) where
  show _ = "PeerDispatchAttempt"

peerDispatchAttempt ::
  PeerDispatchBindingGeneration ->
  PeerDispatchAttemptGeneration ->
  SequencedItem payload ->
  PeerDispatchAttempt payload
peerDispatchAttempt = PeerDispatchAttempt

peerDispatchAttemptBindingGeneration ::
  PeerDispatchAttempt payload ->
  PeerDispatchBindingGeneration
peerDispatchAttemptBindingGeneration (PeerDispatchAttempt binding _ _) = binding

peerDispatchAttemptGeneration ::
  PeerDispatchAttempt payload ->
  PeerDispatchAttemptGeneration
peerDispatchAttemptGeneration (PeerDispatchAttempt _ generation _) = generation

peerDispatchAttemptDirection :: PeerDispatchAttempt payload -> StreamDirection
peerDispatchAttemptDirection (PeerDispatchAttempt _ _ item) = sequencedItemDirection item

peerDispatchAttemptItem :: PeerDispatchAttempt payload -> SequencedItem payload
peerDispatchAttemptItem (PeerDispatchAttempt _ _ item) = item

-- | Runtime observation for one exact logical dispatch attempt.
data PeerDispatchOutcome
  = PeerDispatchWritten
  | PeerDispatchDeferred
  | PeerDispatchFailed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Canonical structural statement of retained out-of-order items.
data GapSummary = GapSummary
  { direction :: StreamDirection,
    receivedPrefix :: StreamPrefix,
    entries :: [(StreamSequence, PeerItemDigest)]
  }
  deriving stock (Eq, Show)

mkGapSummary ::
  StreamDirection ->
  StreamPrefix ->
  [(StreamSequence, PeerItemDigest)] ->
  Either StreamShapeProblem GapSummary
mkGapSummary direction receivedPrefix entries
  | entries /= sortOn fst entries || not (strictlyAscending (fmap fst entries)) =
      Left GapEntriesNotStrictlyAscending
  | Just sequenceNumber <- firstInvalid =
      Left (GapEntryNotBeyondFirstMissing sequenceNumber)
  | otherwise = Right GapSummary {direction, receivedPrefix, entries}
  where
    firstMissing = nextAfterStreamPrefix receivedPrefix
    firstInvalid = fst <$> findFirst ((<= firstMissing) . fst) entries

gapSummaryDirection :: GapSummary -> StreamDirection
gapSummaryDirection summary = summary.direction

gapSummaryReceivedPrefix :: GapSummary -> StreamPrefix
gapSummaryReceivedPrefix summary = summary.receivedPrefix

gapSummaryEntries :: GapSummary -> [(StreamSequence, PeerItemDigest)]
gapSummaryEntries summary = summary.entries

data ReceiveProgress = ReceivedPending | Completed
  deriving stock (Eq, Ord, Show)

data ReceiveDisposition
  = ReceiveContiguous
  | ReceiveGap StreamSequence
  | ReceiveDuplicate
  deriving stock (Eq, Show)

-- | The two independent cumulative watermarks for one direction.
data StreamWatermarks = StreamWatermarks StreamPrefix ReceiptRetirement
  deriving stock (Eq, Show)

streamWatermarks :: StreamPrefix -> StreamPrefix -> StreamWatermarks
streamWatermarks received completed = StreamWatermarks received (streamPrefixCompletion completed)

streamWatermarksWithCompletion :: StreamPrefix -> ReceiptRetirement -> StreamWatermarks
streamWatermarksWithCompletion = StreamWatermarks

streamReceivedPrefix :: StreamWatermarks -> StreamPrefix
streamReceivedPrefix (StreamWatermarks received _) = received

streamCompletedPrefix :: StreamWatermarks -> StreamPrefix
streamCompletedPrefix = streamCompletionPrefix . streamCompletionProgress

streamCompletionProgress :: StreamWatermarks -> ReceiptRetirement
streamCompletionProgress (StreamWatermarks _ completed) = completed

streamPrefixCompletion :: StreamPrefix -> ReceiptRetirement
streamPrefixCompletion = receiptRetirementPrefix . fmap streamSequenceWord64 . streamPrefixSequence

-- | Ordered barriers use only the contiguous portion of sparse completion.
streamCompletionPrefix :: ReceiptRetirement -> StreamPrefix
streamCompletionPrefix progress =
  case receiptRetirementHighWater progress of
    Nothing -> emptyStreamPrefix
    Just high -> case Set.lookupMin (receiptRetirementExceptions progress) of
      Just first -> through (min high (if first == 0 then 0 else first - 1))
      Nothing -> through high
  where
    through 0 = emptyStreamPrefix
    through value = streamPrefixThrough (StreamSequence value)

checkStreamCompletion :: ReceiptRetirement -> Either StreamShapeProblem ReceiptRetirement
checkStreamCompletion progress
  | receiptRetirementHighWater progress == Just 0 || Set.member 0 (receiptRetirementExceptions progress) = Left StreamCompletionMustBePositive
  | otherwise = Right progress

data ReceiveResult = ReceiveResult
  { disposition :: ReceiveDisposition,
    watermarks :: StreamWatermarks,
    gapSummary :: GapSummary
  }
  deriving stock (Eq, Show)

receiveResult ::
  ReceiveDisposition ->
  StreamWatermarks ->
  GapSummary ->
  ReceiveResult
receiveResult disposition watermarks gapSummary =
  ReceiveResult {disposition, watermarks, gapSummary}

receiveResultDisposition :: ReceiveResult -> ReceiveDisposition
receiveResultDisposition result = result.disposition

receiveResultWatermarks :: ReceiveResult -> StreamWatermarks
receiveResultWatermarks result = result.watermarks

receiveResultGapSummary :: ReceiveResult -> GapSummary
receiveResultGapSummary result = result.gapSummary

data AssignmentReceipt = AssignmentReceipt
  { direction :: StreamDirection,
    sequenceNumber :: StreamSequence,
    digest :: PeerItemDigest
  }
  deriving stock (Eq, Ord, Show)

assignmentReceipt ::
  StreamDirection ->
  StreamSequence ->
  PeerItemDigest ->
  AssignmentReceipt
assignmentReceipt direction sequenceNumber digest =
  AssignmentReceipt {direction, sequenceNumber, digest}

assignmentReceiptDirection :: AssignmentReceipt -> StreamDirection
assignmentReceiptDirection receipt = receipt.direction

assignmentReceiptSequence :: AssignmentReceipt -> StreamSequence
assignmentReceiptSequence receipt = receipt.sequenceNumber

assignmentReceiptDigest :: AssignmentReceipt -> PeerItemDigest
assignmentReceiptDigest receipt = receipt.digest

-- | Sender @S@'s reconnect claim offered to destination @D@.
data ResumeOffer = ResumeOffer
  { source :: HeraldEpoch,
    destination :: HeraldEpoch,
    nextSourceSequence :: StreamSequence,
    receivedPrefix :: StreamPrefix,
    completion :: ReceiptRetirement,
    gapSummary :: GapSummary
  }
  deriving stock (Eq, Show)

mkResumeOffer ::
  HeraldEpoch ->
  HeraldEpoch ->
  StreamSequence ->
  StreamPrefix ->
  StreamPrefix ->
  GapSummary ->
  Either StreamShapeProblem ResumeOffer
mkResumeOffer source destination nextSourceSequence receivedPrefix completedPrefix gapSummary =
  mkResumeOfferWithCompletion source destination nextSourceSequence receivedPrefix (streamPrefixCompletion completedPrefix) gapSummary

mkResumeOfferWithCompletion :: HeraldEpoch -> HeraldEpoch -> StreamSequence -> StreamPrefix -> ReceiptRetirement -> GapSummary -> Either StreamShapeProblem ResumeOffer
mkResumeOfferWithCompletion source destination nextSourceSequence receivedPrefix completion gapSummary = do
  _ <- checkStreamCompletion completion
  let completedPrefix = streamCompletionPrefix completion

  forward <- mkStreamDirection source destination
  let expectedGapDirection = reverseStreamDirection forward
  if gapSummaryDirection gapSummary /= expectedGapDirection
    then
      Left
        ( ResumeGapDirectionMismatch
            expectedGapDirection
            (gapSummaryDirection gapSummary)
        )
    else
      if gapSummaryReceivedPrefix gapSummary /= receivedPrefix
        then
          Left
            ( ResumeGapPrefixMismatch
                receivedPrefix
                (gapSummaryReceivedPrefix gapSummary)
            )
        else
          if maybe False (> (streamSequenceWord64 (nextAfterStreamPrefix receivedPrefix) - 1)) (receiptRetirementHighWater completion)
            then Left (ResumeCompletedBeyondReceived completedPrefix receivedPrefix)
            else
              Right
                ResumeOffer
                  { source,
                    destination,
                    nextSourceSequence,
                    receivedPrefix,
                    completion,
                    gapSummary
                  }

resumeOfferSource :: ResumeOffer -> HeraldEpoch
resumeOfferSource offer = offer.source

resumeOfferDestination :: ResumeOffer -> HeraldEpoch
resumeOfferDestination offer = offer.destination

resumeOfferNextSourceSequence :: ResumeOffer -> StreamSequence
resumeOfferNextSourceSequence offer = offer.nextSourceSequence

resumeOfferReceivedPrefix :: ResumeOffer -> StreamPrefix
resumeOfferReceivedPrefix offer = offer.receivedPrefix

resumeOfferCompletedPrefix :: ResumeOffer -> StreamPrefix
resumeOfferCompletedPrefix = streamCompletionPrefix . resumeOfferCompletion

resumeOfferCompletion :: ResumeOffer -> ReceiptRetirement
resumeOfferCompletion offer = offer.completion

resumeOfferGapSummary :: ResumeOffer -> GapSummary
resumeOfferGapSummary offer = offer.gapSummary

data ResumeResponse = ResumeResponse
  { receivedPrefix :: StreamPrefix,
    completion :: ReceiptRetirement,
    retransmitFrom :: StreamSequence
  }
  deriving stock (Eq, Show)

resumeResponse ::
  StreamPrefix ->
  StreamPrefix ->
  StreamSequence ->
  ResumeResponse
resumeResponse received completed = resumeResponseWithCompletion received (streamPrefixCompletion completed)

resumeResponseWithCompletion :: StreamPrefix -> ReceiptRetirement -> StreamSequence -> ResumeResponse
resumeResponseWithCompletion receivedPrefix completion retransmitFrom =
  ResumeResponse {receivedPrefix, completion, retransmitFrom}

resumeResponseReceivedPrefix :: ResumeResponse -> StreamPrefix
resumeResponseReceivedPrefix response = response.receivedPrefix

resumeResponseCompletedPrefix :: ResumeResponse -> StreamPrefix
resumeResponseCompletedPrefix = streamCompletionPrefix . resumeResponseCompletion

resumeResponseCompletion :: ResumeResponse -> ReceiptRetirement
resumeResponseCompletion response = response.completion

resumeResponseRetransmitFrom :: ResumeResponse -> StreamSequence
resumeResponseRetransmitFrom response = response.retransmitFrom

strictlyAscending :: (Ord value) => [value] -> Bool
strictlyAscending [] = True
strictlyAscending [_] = True
strictlyAscending (left : right : rest) =
  left < right && strictlyAscending (right : rest)

findFirst :: (value -> Bool) -> [value] -> Maybe value
findFirst _ [] = Nothing
findFirst predicate (value : rest)
  | predicate value = Just value
  | otherwise = findFirst predicate rest
