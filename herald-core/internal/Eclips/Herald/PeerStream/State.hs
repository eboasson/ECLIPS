{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure owner of retained logical peer-stream state.
--
-- All prepared constructors are private.  Preparation performs every semantic
-- check; each corresponding commit is a total installation of its successor.
module Eclips.Herald.PeerStream.State
  ( State,
    initialState,
    peerStreamLocalEpoch,
    IncomingItemStatus (..),
    PeerStreamProtocolViolation (..),
    PeerStreamInvariantViolation (..),
    PeerStreamProblem (..),
    SequenceBoundPeerItem,
    sequenceBoundPeerItem,
    PreparedEnqueue,
    prepareSequenceBoundEnqueue,
    prepareEnqueue,
    preparedEnqueueAssignments,
    preparedEnqueueFrontierAdvances,
    preparedEnqueueDispatchTickets,
    commitEnqueue,
    PreparedDispatchBinding,
    prepareDispatchBinding,
    preparedDispatchBindingTicket,
    commitDispatchBinding,
    PreparedDispatchBindingLoss,
    prepareDispatchBindingLoss,
    preparedDispatchBindingLost,
    commitDispatchBindingLoss,
    PreparedPeerRetirement,
    preparePeerRetirement,
    preparedPeerRetirementDirection,
    preparedPeerRetirementSettledAssignments,
    commitPeerRetirement,
    peerStreamPeerRetired,
    PreparedLocalRetirementCut,
    prepareLocalRetirementCut,
    preparedLocalRetirementCutDirections,
    commitLocalRetirementCut,
    PreparedDispatchDrainCut,
    prepareDispatchDrainCut,
    preparedDispatchDrainCutDirections,
    commitDispatchDrainCut,
    PreparedDrainingDispatchOutcome,
    prepareDrainingDispatchOutcome,
    commitDrainingDispatchOutcome,
    PreparedDispatchTicket,
    prepareEnsureDispatchTicket,
    preparedDispatchTicket,
    commitDispatchTicket,
    PreparedDispatchSelection,
    prepareDispatchSelection,
    preparedDispatchSelectionAttempt,
    commitDispatchSelection,
    PreparedDispatchOutcome,
    prepareDispatchOutcome,
    preparedDispatchOutcomeTicket,
    commitDispatchOutcome,
    PreparedReceiveCandidate,
    prepareReceiveCandidate,
    preparedReceiveDisposition,
    preparedContiguousItems,
    discardPreviouslyRetainedContiguousAt,
    PreparedReceive,
    finalizeReceive,
    preparedReceiveResult,
    commitReceive,
    PreparedCompletion,
    prepareCompletion,
    preparedCompletionWatermarks,
    commitCompletion,
    PreparedReceivedAck,
    prepareReceivedAck,
    preparedReceivedAckWatermarks,
    preparedReceivedAckDispatchTicket,
    commitReceivedAck,
    PreparedCompletedAck,
    prepareCompletedAck,
    prepareCompletedProgress,
    preparedCompletedAckWatermarks,
    preparedCompletedAckDispatchTicket,
    commitCompletedAck,
    PreparedFrontierAdvance,
    prepareFrontierAdvance,
    commitFrontierAdvance,
    PreparedResume,
    ResumeAssociationPhase (..),
    prepareResume,
    prepareResumeForAssociation,
    preparedResumeResponse,
    preparedResumeRetransmissions,
    preparedResumeDispatchTicket,
    commitResume,
    outgoingWatermarks,
    outgoingNextSequence,
    incomingWatermarks,
    incomingCompletionSnapshot,
    peerStreamOutgoingDirections,
    peerStreamIncomingDirections,
    activeOutgoingItems,
    incomingRetainedItems,
    incomingItemAtOrAfter,
    IncomingMarkerFamily (..),
    IncomingMarkerDependency (..),
    incomingMarkerDirections,
    pendingIncomingMarkerDirections,
    nextIncomingMarkerDirection,
    consumeIncomingMarkerDirection,
    retainIncomingMarkerDependencies,
    notifyIncomingMarkerDependencies,
    clearIncomingMarkerWork,
    assignmentCompletionKnown,
    assignmentRetirementKnown,
    incomingAssignmentCompleted,
    incomingSequenceCompleted,
    nextUncompletedIncomingSequence,
    outgoingCompletionProgress,
    peerStreamRetentionCounts,
    outgoingPeerGapSummary,
    incomingGapSummary,
    incomingGreatestAdvertisedNextSequence,
    resumeOfferForPeer,
    currentDispatchBinding,
    currentDispatchTicket,
    currentDispatchAttempt,
    dispatchAwaitingSequences,
    PeerStreamStateWitness,
    peerStreamStateWitness,
    peerStreamWitnessLocalEpoch,
    peerStreamWitnessOutgoingDirections,
    peerStreamWitnessIncomingDirections,
    validatePeerStreamState,
    replaceDispatchBindingForInvariantTest,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment (ContextClassGenerationId)
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.Internal.WorkIndex (WorkIndex)
import Eclips.Herald.Internal.WorkIndex qualified as WorkIndex
import Eclips.Herald.PeerStream
  ( AssignmentReceipt,
    GapSummary,
    PeerDispatchAttempt,
    PeerDispatchAttemptGeneration,
    PeerDispatchBindingGeneration,
    PeerDispatchOutcome (..),
    PeerDispatchTicket,
    PeerItem,
    PeerItemDigest,
    ReceiveDisposition (..),
    ReceiveProgress (..),
    ReceiveResult,
    ResumeOffer,
    ResumeResponse,
    SequencedItem,
    StreamDirection,
    StreamPrefix,
    StreamSequence,
    StreamWatermarks,
    assignmentReceipt,
    assignmentReceiptDirection,
    assignmentReceiptSequence,
    checkStreamCompletion,
    emptyStreamPrefix,
    firstStreamSequence,
    gapSummaryDirection,
    gapSummaryEntries,
    gapSummaryReceivedPrefix,
    mkGapSummary,
    mkResumeOfferWithCompletion,
    mkStreamDirection,
    mkStreamSequence,
    nextAfterStreamPrefix,
    nextStreamSequence,
    peerDispatchAttempt,
    peerDispatchAttemptBindingGeneration,
    peerDispatchAttemptDirection,
    peerDispatchAttemptGeneration,
    peerDispatchAttemptGenerationForOwner,
    peerDispatchAttemptGenerationWord64,
    peerDispatchAttemptItem,
    peerDispatchTicket,
    peerDispatchTicketDirection,
    peerDispatchTicketGenerationWord64,
    peerItemDigest,
    peerItemPayload,
    receiveResult,
    resumeOfferCompletedPrefix,
    resumeOfferCompletion,
    resumeOfferDestination,
    resumeOfferGapSummary,
    resumeOfferNextSourceSequence,
    resumeOfferReceivedPrefix,
    resumeOfferSource,
    resumeResponseWithCompletion,
    reverseStreamDirection,
    sequencedItem,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemSequence,
    streamCompletionPrefix,
    streamDirectionDestination,
    streamDirectionSource,
    streamPrefixCompletion,
    streamPrefixSequence,
    streamPrefixThrough,
    streamSequenceWord64,
    streamWatermarksWithCompletion,
  )
import Eclips.Public.Types.ReceiptRetirement

-- | Destination-side disposition retained for a complete incoming item.
data IncomingItemStatus
  = GapRetained
  | IncomingReceivedPending
  | IncomingCompleted
  deriving stock (Eq, Ord, Show)

data IncomingRecord payload = IncomingRecord
  { item :: SequencedItem payload,
    status :: IncomingItemStatus
  }
  deriving stock (Eq, Show)

-- A successful write suppresses retries only for the association that wrote
-- it. Keeping the exact attempt distinguishes old work needing reconnect
-- repair from a write which outran the new association's first Resume.
data AwaitingPeerProgress
  = AwaitingPeerReceipt
  | AwaitingDispatchWrite PeerDispatchBindingGeneration PeerDispatchAttemptGeneration
  deriving stock (Eq, Show)

awaitingDispatchWrite :: PeerDispatchAttempt payload -> AwaitingPeerProgress
awaitingDispatchWrite attempt =
  AwaitingDispatchWrite
    (peerDispatchAttemptBindingGeneration attempt)
    (peerDispatchAttemptGeneration attempt)

data Outgoing payload = Outgoing
  { nextSequence :: StreamSequence,
    receivedPrefix :: StreamPrefix,
    completedPrefix :: StreamPrefix,
    activeItems :: Map StreamSequence (SequencedItem payload),
    completion :: ReceiptRetirement,
    peerGapSummary :: GapSummary,
    lastResumeGapClaim :: Maybe GapSummary,
    dispatchBinding :: Maybe PeerDispatchBindingGeneration,
    nextDispatchTicketGeneration :: Word64,
    nextDispatchAttemptGeneration :: Word64,
    associationAttemptFloor :: Word64,
    dispatchTicket :: Maybe PeerDispatchTicket,
    dispatchAttempt :: Maybe (PeerDispatchAttempt payload),
    lastDispatchOutcome :: Maybe (PeerDispatchAttempt payload, PeerDispatchOutcome),
    awaitingPeerProgress :: Map StreamSequence AwaitingPeerProgress
  }
  deriving stock (Eq, Show)

data Incoming payload = Incoming
  { greatestAdvertisedNextSequence :: Maybe StreamSequence,
    receivedPrefix :: StreamPrefix,
    completedPrefix :: StreamPrefix,
    completion :: ReceiptRetirement,
    items :: Map StreamSequence (IncomingRecord payload)
  }
  deriving stock (Eq, Show)

data State payload = State
  { localEpoch :: HeraldEpoch,
    outgoing :: Map HeraldEpoch (Outgoing payload),
    incoming :: Map HeraldEpoch (Incoming payload),
    retiredPeers :: Set HeraldEpoch,
    routeMarkerWork :: !(WorkIndex StreamDirection IncomingMarkerDependency),
    disappearanceMarkerWork :: !(WorkIndex StreamDirection IncomingMarkerDependency)
  }
  deriving stock (Eq, Show)

initialState :: HeraldEpoch -> State payload
initialState localEpoch =
  State
    { localEpoch,
      outgoing = Map.empty,
      incoming = Map.empty,
      retiredPeers = Set.empty,
      routeMarkerWork = WorkIndex.empty,
      disappearanceMarkerWork = WorkIndex.empty
    }

peerStreamLocalEpoch :: State payload -> HeraldEpoch
peerStreamLocalEpoch state = state.localEpoch

-- | Scheduling identities contain no retained payload or semantic-owner snapshot.
data IncomingMarkerFamily = RouteCutoverMarkers | DisappearanceProbeMarkers
  deriving stock (Eq, Ord, Show)

data IncomingMarkerDependency
  = RouteMarkerGeneration !ContextClassGenerationId
  | DisappearanceMarkerProbe !DisappearanceProbeId
  deriving stock (Eq, Ord, Show)

markerIndex :: IncomingMarkerFamily -> State payload -> WorkIndex StreamDirection IncomingMarkerDependency
markerIndex family state = case family of
  RouteCutoverMarkers -> state.routeMarkerWork
  DisappearanceProbeMarkers -> state.disappearanceMarkerWork

modifyMarkerIndex :: IncomingMarkerFamily -> (WorkIndex StreamDirection IncomingMarkerDependency -> WorkIndex StreamDirection IncomingMarkerDependency) -> State payload -> State payload
modifyMarkerIndex family update state = case family of
  RouteCutoverMarkers -> state {routeMarkerWork = update state.routeMarkerWork}
  DisappearanceProbeMarkers -> state {disappearanceMarkerWork = update state.disappearanceMarkerWork}

incomingMarkerDirections :: IncomingMarkerFamily -> State payload -> Set StreamDirection
incomingMarkerDirections family = WorkIndex.registeredKeys . markerIndex family

pendingIncomingMarkerDirections :: IncomingMarkerFamily -> State payload -> Set StreamDirection
pendingIncomingMarkerDirections family = WorkIndex.pendingKeys . markerIndex family

nextIncomingMarkerDirection :: IncomingMarkerFamily -> Set StreamDirection -> Maybe StreamDirection -> State payload -> Maybe StreamDirection
nextIncomingMarkerDirection family inventory cursor = WorkIndex.nextWork inventory cursor . markerIndex family

consumeIncomingMarkerDirection :: IncomingMarkerFamily -> StreamDirection -> State payload -> State payload
consumeIncomingMarkerDirection family direction = modifyMarkerIndex family (WorkIndex.consumeWork direction)

retainIncomingMarkerDependencies :: IncomingMarkerFamily -> StreamDirection -> Set IncomingMarkerDependency -> State payload -> State payload
retainIncomingMarkerDependencies family direction dependencies = modifyMarkerIndex family (WorkIndex.replaceDependencies direction dependencies)

notifyIncomingMarkerDependencies :: Set IncomingMarkerDependency -> State payload -> State payload
notifyIncomingMarkerDependencies dependencies state
  | Set.null dependencies = state
  | otherwise = foldl' (\current family -> modifyMarkerIndex family (snd . WorkIndex.notifyDependencies dependencies) current) state markerFamilies

-- | Comparison-only normalization; registrations and both watcher directions
-- remain visible and are checked independently by the owner validator.
clearIncomingMarkerWork :: State payload -> State payload
clearIncomingMarkerWork state = foldl' (\current family -> modifyMarkerIndex family WorkIndex.clearPendingWork current) state markerFamilies

markerFamilies :: [IncomingMarkerFamily]
markerFamilies = [RouteCutoverMarkers, DisappearanceProbeMarkers]

incomingHeadSequence :: Incoming payload -> Maybe StreamSequence
incomingHeadSequence incoming = case Map.lookupMin incoming.items of
  Just (sequenceNumber, record)
    | record.status == IncomingReceivedPending -> Just sequenceNumber
  _ -> Nothing

-- Live incoming directions sleep without historical per-item work. A retired
-- direction leaves all indexes as soon as its last pending item completes.
-- Frontier/resume-only changes,
-- duplicate receipts, and appends behind the current head do not dirty it.
installIncoming :: StreamDirection -> Incoming payload -> State payload -> State payload
installIncoming direction incoming state =
  foldl' updateFamily installed markerFamilies
  where
    peer = streamDirectionSource direction
    changedHead = incomingHeadSequence (incomingFor peer state) /= incomingHeadSequence incoming
    installed = state {incoming = Map.insert peer incoming state.incoming}
    updateFamily current family = modifyMarkerIndex family update current
    update index
      | Set.member peer state.retiredPeers, incomingHeadSequence incoming == Nothing = WorkIndex.removeWork direction index
      | Set.notMember direction (WorkIndex.registeredKeys index) =
          let registered = WorkIndex.registerWork direction Set.empty index
           in if incomingHeadSequence incoming == Nothing then WorkIndex.consumeWork direction registered else registered
      | changedHead = WorkIndex.markDirty (Set.singleton direction) (WorkIndex.replaceDependencies direction Set.empty index)
      | otherwise = index

wakeRetiredIncoming :: StreamDirection -> State payload -> State payload
wakeRetiredIncoming direction state = foldl' (\current family -> modifyMarkerIndex family update current) state markerFamilies
  where
    update
      | incomingHeadSequence (incomingFor (streamDirectionSource direction) state) == Nothing = WorkIndex.removeWork direction
      | otherwise = WorkIndex.markDirty (Set.singleton direction)

-- | Rejectable claims from a peer or caller at the stream boundary.
data PeerStreamProtocolViolation
  = PeerStreamSelfDirection HeraldEpoch
  | PeerStreamDirectionNotOutgoing StreamDirection HeraldEpoch
  | PeerStreamDirectionNotIncoming StreamDirection HeraldEpoch
  | PeerStreamConflictingItem
      StreamDirection
      StreamSequence
      PeerItemDigest
      PeerItemDigest
  | PeerStreamItemBeyondAdvertisedFrontier
      StreamDirection
      StreamSequence
      StreamSequence
  | PeerStreamCompletionUnknown StreamDirection StreamSequence
  | PeerStreamCompletionBeforeReceipt StreamDirection StreamSequence
  | PeerStreamAcknowledgementAhead
      StreamDirection
      StreamPrefix
      StreamPrefix
  | PeerStreamResumeDestinationMismatch HeraldEpoch HeraldEpoch
  | PeerStreamSourceFrontierRegressed
      StreamDirection
      StreamSequence
      StreamSequence
  | PeerStreamSourceFrontierContradictsHistory
      StreamDirection
      StreamSequence
      StreamPrefix
  | PeerStreamResumeCompletedBeyondAllocated
      StreamDirection
      StreamPrefix
      StreamPrefix
  | PeerStreamResumeReceivedBeyondAllocated
      StreamDirection
      StreamPrefix
      StreamPrefix
  | PeerStreamResumeGapEntryNotActive StreamDirection StreamSequence
  | PeerStreamResumeGapDigestMismatch
      StreamDirection
      StreamSequence
      PeerItemDigest
      PeerItemDigest
  | PeerStreamInvalidCompletionProgress StreamDirection ReceiptRetirement
  | PeerStreamRetiredPeer HeraldEpoch
  deriving stock (Eq, Show)

-- | A contradiction in an opaque owner state or owner-prepared refinement.
data PeerStreamInvariantViolation
  = PeerStreamInvalidNominalDirection
  | PeerStreamInvalidMarkerWork
  | PeerStreamInvalidGapTranscript
  | PeerStreamInvalidResumeTranscript
  | PeerStreamReceiveProgressMismatch
      (Set StreamSequence)
      (Set StreamSequence)
  | PeerStreamOutgoingInvariant StreamDirection
  | PeerStreamIncomingInvariant StreamDirection
  | PeerStreamDispatchInvariant StreamDirection
  | PeerStreamDispatchOutcomeContradiction
      StreamDirection
      PeerDispatchAttemptGeneration
      PeerDispatchOutcome
      PeerDispatchOutcome
  deriving stock (Eq, Show)

data PeerStreamProblem
  = PeerStreamProtocolProblem PeerStreamProtocolViolation
  | PeerStreamInvariantProblem PeerStreamInvariantViolation
  deriving stock (Eq, Show)

data EnqueueOutput payload = EnqueueOutput
  { assignments :: Map HeraldEpoch (NonEmpty (SequencedItem payload)),
    frontierAdvances :: Map HeraldEpoch (StreamDirection, StreamSequence),
    tickets :: [PeerDispatchTicket]
  }

newtype PreparedEnqueue payload
  = PreparedEnqueue
      ( Prepared
          (State payload)
          (EnqueueOutput payload)
      )

-- | A pure item recipe that is evaluated only after PeerStream has selected
-- the exact direction and allocated its next sequence.
--
-- Keeping the recipe opaque prevents callers from applying it outside the
-- owner transition while still allowing payloads and digests to bind their
-- outer stream coordinate.
newtype SequenceBoundPeerItem payload
  = SequenceBoundPeerItem
      (StreamDirection -> StreamSequence -> PeerItem payload)

sequenceBoundPeerItem ::
  (StreamDirection -> StreamSequence -> PeerItem payload) ->
  SequenceBoundPeerItem payload
sequenceBoundPeerItem = SequenceBoundPeerItem

-- | Atomically allocate every requested direction in canonical peer-map order.
prepareEnqueue ::
  Map HeraldEpoch (NonEmpty (PeerItem payload)) ->
  State payload ->
  Either PeerStreamProblem (PreparedEnqueue payload)
prepareEnqueue requests =
  prepareSequenceBoundEnqueue
    (fmap (fmap constantSequenceBoundPeerItem) requests)

-- | Atomically allocate every requested direction and construct each item
-- from its exact assigned direction and sequence.
prepareSequenceBoundEnqueue ::
  Map HeraldEpoch (NonEmpty (SequenceBoundPeerItem payload)) ->
  State payload ->
  Either PeerStreamProblem (PreparedEnqueue payload)
prepareSequenceBoundEnqueue requests state = do
  ensureValid state
  PreparedEnqueue <$> prepareTransition install state
  where
    install predecessor = do
      (successor, assignments, frontiers, reverseTickets) <-
        foldEntries
          predecessor
          Map.empty
          Map.empty
          []
          (Map.toAscList requests)
      ensureValid successor
      Right
        ( successor,
          EnqueueOutput assignments frontiers (reverse reverseTickets)
        )

    foldEntries successor assignments frontiers tickets [] =
      Right (successor, assignments, frontiers, tickets)
    foldEntries successor assignments frontiers tickets ((peer, requested) : rest) = do
      ensurePeerActive successor peer
      direction <- outgoingDirection successor peer
      current <- outgoingFor direction successor
      let (updated, assigned) = assignEvery direction current requested
          nextState =
            successor
              { outgoing = Map.insert peer updated successor.outgoing
              }
      (scheduled, ticket) <- ensureDispatchFor direction nextState
      foldEntries
        scheduled
        (Map.insert peer assigned assignments)
        (Map.insert peer (direction, updated.nextSequence) frontiers)
        (maybe tickets (: tickets) ticket)
        rest

constantSequenceBoundPeerItem :: PeerItem payload -> SequenceBoundPeerItem payload
constantSequenceBoundPeerItem request =
  SequenceBoundPeerItem (\_ _ -> request)

preparedEnqueueAssignments ::
  PreparedEnqueue payload ->
  Map HeraldEpoch (NonEmpty (SequencedItem payload))
preparedEnqueueAssignments (PreparedEnqueue prepared) =
  (preparedOutput prepared).assignments

-- | Exact one-way exclusive frontier for each direction advanced by this
-- enqueue. A composed coordinator advertises it before making the corresponding
-- dispatch ticket executable on an established binding.
preparedEnqueueFrontierAdvances ::
  PreparedEnqueue payload ->
  Map HeraldEpoch (StreamDirection, StreamSequence)
preparedEnqueueFrontierAdvances (PreparedEnqueue prepared) =
  (preparedOutput prepared).frontierAdvances

preparedEnqueueDispatchTickets ::
  PreparedEnqueue payload ->
  [PeerDispatchTicket]
preparedEnqueueDispatchTickets (PreparedEnqueue prepared) =
  (preparedOutput prepared).tickets

commitEnqueue ::
  PreparedEnqueue payload ->
  (State payload, Map HeraldEpoch (NonEmpty (SequencedItem payload)))
commitEnqueue (PreparedEnqueue prepared) =
  let (state, output) = commitPrepared prepared
   in (state, output.assignments)

assignEvery ::
  StreamDirection ->
  Outgoing payload ->
  NonEmpty (SequenceBoundPeerItem payload) ->
  (Outgoing payload, NonEmpty (SequencedItem payload))
assignEvery direction initial (firstRequest :| remaining) =
  let (afterFirst, firstAssigned) = assignOne direction initial firstRequest
      (finished, reversedRest) =
        foldl
          ( \(current, accumulated) request ->
              let (next, assigned) = assignOne direction current request
               in (next, assigned : accumulated)
          )
          (afterFirst, [])
          remaining
   in (finished, firstAssigned :| reverse reversedRest)

assignOne ::
  StreamDirection ->
  Outgoing payload ->
  SequenceBoundPeerItem payload ->
  (Outgoing payload, SequencedItem payload)
assignOne direction outgoing (SequenceBoundPeerItem buildRequest) =
  ( outgoing
      { nextSequence = nextStreamSequence sequenceNumber,
        activeItems = Map.insert sequenceNumber assigned outgoing.activeItems
      },
    assigned
  )
  where
    sequenceNumber = outgoing.nextSequence
    request = buildRequest direction sequenceNumber
    assigned =
      sequencedItem
        direction
        sequenceNumber
        (peerItemDigest request)
        (peerItemPayload request)

-- | Establish exactly one scheduling correlation when a bound direction has
-- retained eligible work and no attempt already owns that direction.
--
-- The returned ticket is only a newly minted notification. An already current
-- ticket remains coalesced in owner state and is not re-emitted.
ensureDispatchFor ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem (State payload, Maybe PeerDispatchTicket)
ensureDispatchFor direction state = do
  if Set.member (streamDirectionDestination direction) state.retiredPeers
    then Right (state, Nothing)
    else do
      current <- outgoingFor direction state
      case current.dispatchAttempt of
        Just _ ->
          case current.dispatchTicket of
            Nothing -> Right (state, Nothing)
            Just _ -> dispatchInvariant direction
        Nothing ->
          case (current.dispatchBinding, lowestEligibleItem current) of
            (Just _, Just _) ->
              case current.dispatchTicket of
                Just _ -> Right (state, Nothing)
                Nothing ->
                  let ticket =
                        peerDispatchTicket
                          direction
                          current.nextDispatchTicketGeneration
                      updated =
                        current
                          { nextDispatchTicketGeneration =
                              current.nextDispatchTicketGeneration + 1,
                            dispatchTicket = Just ticket
                          }
                   in Right
                        ( replaceOutgoing direction updated state,
                          Just ticket
                        )
            _ ->
              case current.dispatchTicket of
                Nothing -> Right (state, Nothing)
                Just _ ->
                  Right
                    ( replaceOutgoing
                        direction
                        current {dispatchTicket = Nothing}
                        state,
                      Nothing
                    )

lowestEligibleItem :: Outgoing payload -> Maybe (SequencedItem payload)
lowestEligibleItem outgoing =
  snd
    <$> Map.lookupMin
      ( Map.withoutKeys
          outgoing.activeItems
          (Map.keysSet outgoing.awaitingPeerProgress `Set.union` peerGapSequences outgoing)
      )

peerGapSequences :: Outgoing payload -> Set StreamSequence
peerGapSequences outgoing =
  Set.fromList (fmap fst (gapSummaryEntries outgoing.peerGapSummary))

sameDispatchAttempt ::
  PeerDispatchAttempt left ->
  PeerDispatchAttempt right ->
  Bool
sameDispatchAttempt left right =
  peerDispatchAttemptBindingGeneration left
    == peerDispatchAttemptBindingGeneration right
    && peerDispatchAttemptGeneration left
      == peerDispatchAttemptGeneration right
    && sameSequencedItemIdentity
      (peerDispatchAttemptItem left)
      (peerDispatchAttemptItem right)

sameSequencedItemIdentity :: SequencedItem left -> SequencedItem right -> Bool
sameSequencedItemIdentity left right =
  sequencedItemDirection left == sequencedItemDirection right
    && sequencedItemSequence left == sequencedItemSequence right
    && sequencedItemDigest left == sequencedItemDigest right

dispatchedOnCurrentAssociation :: Outgoing payload -> SequencedItem payload -> Bool
dispatchedOnCurrentAssociation outgoing item = case outgoing.dispatchBinding of
  Nothing -> False
  Just binding ->
    ( case Map.lookup (sequencedItemSequence item) outgoing.awaitingPeerProgress of
        Just (AwaitingDispatchWrite writtenBinding attempt) ->
          writtenBinding == binding && freshAttempt attempt
        _ -> False
    )
      || maybe
        False
        ( \attempt ->
            peerDispatchAttemptBindingGeneration attempt == binding
              && freshAttempt (peerDispatchAttemptGeneration attempt)
              && sameSequencedItemIdentity (peerDispatchAttemptItem attempt) item
        )
        outgoing.dispatchAttempt
  where
    freshAttempt attempt =
      peerDispatchAttemptGenerationWord64 attempt >= outgoing.associationAttemptFloor

isLatestSettled ::
  Outgoing payload ->
  PeerDispatchAttempt other ->
  Bool
isLatestSettled outgoing attempt =
  peerDispatchAttemptGenerationWord64 (peerDispatchAttemptGeneration attempt) + 1
    == outgoing.nextDispatchAttemptGeneration

supersedeDispatchThrough :: StreamPrefix -> Outgoing payload -> Outgoing payload
supersedeDispatchThrough prefix outgoing =
  outgoing
    { dispatchAttempt =
        retainUnless (dispatchAttemptIsThrough prefix) outgoing.dispatchAttempt,
      lastDispatchOutcome =
        retainUnless
          (dispatchAttemptIsThrough prefix . fst)
          outgoing.lastDispatchOutcome
    }

retainDispatchForActive ::
  Map StreamSequence (SequencedItem payload) ->
  Outgoing payload ->
  Outgoing payload
retainDispatchForActive active outgoing =
  outgoing
    { dispatchAttempt =
        retainUnless (dispatchAttemptIsInactive active) outgoing.dispatchAttempt,
      lastDispatchOutcome =
        retainUnless
          (dispatchAttemptIsInactive active . fst)
          outgoing.lastDispatchOutcome
    }

supersedeDispatchSequences ::
  Set StreamSequence ->
  Outgoing payload ->
  Outgoing payload
supersedeDispatchSequences sequences outgoing =
  outgoing
    { dispatchAttempt =
        retainUnless (dispatchAttemptIsSelected sequences) outgoing.dispatchAttempt,
      lastDispatchOutcome =
        retainUnless
          (dispatchAttemptIsSelected sequences . fst)
          outgoing.lastDispatchOutcome
    }

dispatchAttemptIsThrough ::
  StreamPrefix ->
  PeerDispatchAttempt payload ->
  Bool
dispatchAttemptIsThrough prefix attempt =
  streamPrefixThrough
    (sequencedItemSequence (peerDispatchAttemptItem attempt))
    <= prefix

dispatchAttemptIsInactive ::
  Map StreamSequence (SequencedItem payload) ->
  PeerDispatchAttempt other ->
  Bool
dispatchAttemptIsInactive active attempt =
  Map.notMember
    (sequencedItemSequence (peerDispatchAttemptItem attempt))
    active

dispatchAttemptIsSelected ::
  Set StreamSequence ->
  PeerDispatchAttempt payload ->
  Bool
dispatchAttemptIsSelected sequences attempt =
  Set.member
    (sequencedItemSequence (peerDispatchAttemptItem attempt))
    sequences

retainUnless :: (value -> Bool) -> Maybe value -> Maybe value
retainUnless predicate candidate =
  case candidate of
    Just value
      | predicate value -> Nothing
    _ -> candidate

replaceOutgoing ::
  StreamDirection ->
  Outgoing payload ->
  State payload ->
  State payload
replaceOutgoing direction outgoing state =
  state
    { outgoing =
        Map.insert
          (streamDirectionDestination direction)
          outgoing
          state.outgoing
    }

dispatchInvariant ::
  StreamDirection ->
  Either PeerStreamProblem value
dispatchInvariant direction =
  Left
    ( PeerStreamInvariantProblem
        (PeerStreamDispatchInvariant direction)
    )

-- | Binding admission/replacement output. A ticket is returned only when this
-- transition newly creates one and the coordinator must schedule it.
newtype PreparedDispatchBinding payload
  = PreparedDispatchBinding
      (Prepared (State payload) (Maybe PeerDispatchTicket))

-- | Observe one already Discovery-admitted Hello association.
--
-- An accepted or reoffered Hello captures the next attempt ordinal so its first
-- Resume distinguishes pre-Hello work from writes made on this association.
-- Repeating admission without intervening attempts is idempotent. Older binding
-- observations are stale no-ops. A newer binding supersedes the old attempt,
-- leaving its exact item retained under one newly coalesced ticket.
prepareDispatchBinding ::
  StreamDirection ->
  PeerDispatchBindingGeneration ->
  State payload ->
  Either PeerStreamProblem (PreparedDispatchBinding payload)
prepareDispatchBinding direction binding state = do
  ensureValid state
  admitted <- outgoingDirectionExact state direction
  ensurePeerActive state (streamDirectionDestination admitted)
  PreparedDispatchBinding <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      current <- outgoingFor admitted predecessor
      case current.dispatchBinding of
        Just incumbent
          | binding < incumbent -> Right (predecessor, Nothing)
        Just incumbent
          | binding == incumbent -> do
              let withAssociation =
                    replaceOutgoing
                      admitted
                      (current {associationAttemptFloor = current.nextDispatchAttemptGeneration})
                      predecessor
              (successor, ticket) <- ensureDispatchFor admitted withAssociation
              ensureValid successor
              Right (successor, ticket)
        _ -> do
          let peer = streamDirectionDestination admitted
              replaced =
                current
                  { dispatchBinding = Just binding,
                    associationAttemptFloor = current.nextDispatchAttemptGeneration,
                    dispatchTicket = Nothing,
                    dispatchAttempt = Nothing,
                    lastDispatchOutcome = Nothing
                  }
              withBinding =
                predecessor
                  { outgoing = Map.insert peer replaced predecessor.outgoing
                  }
          (successor, ticket) <- ensureDispatchFor admitted withBinding
          ensureValid successor
          Right (successor, ticket)

preparedDispatchBindingTicket ::
  PreparedDispatchBinding payload ->
  Maybe PeerDispatchTicket
preparedDispatchBindingTicket (PreparedDispatchBinding prepared) =
  preparedOutput prepared

commitDispatchBinding ::
  PreparedDispatchBinding payload ->
  (State payload, Maybe PeerDispatchTicket)
commitDispatchBinding (PreparedDispatchBinding prepared) = commitPrepared prepared

newtype PreparedDispatchBindingLoss payload
  = PreparedDispatchBindingLoss
      (Prepared (State payload) Bool)

-- | Observe loss of one exact Discovery-owned binding generation.
--
-- Only the current generation may clear dispatch ownership. Stale or duplicate
-- observations are no-ops. Retained items and protocol progress survive while
-- the direction remains deliberately unscheduled until a replacement binding
-- is admitted.
prepareDispatchBindingLoss ::
  StreamDirection ->
  PeerDispatchBindingGeneration ->
  State payload ->
  Either PeerStreamProblem (PreparedDispatchBindingLoss payload)
prepareDispatchBindingLoss direction binding state = do
  ensureValid state
  admitted <- outgoingDirectionExact state direction
  PreparedDispatchBindingLoss <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      current <- outgoingFor admitted predecessor
      case current.dispatchBinding of
        Just incumbent
          | incumbent == binding -> do
              let successor =
                    replaceOutgoing
                      admitted
                      current
                        { dispatchBinding = Nothing,
                          dispatchTicket = Nothing,
                          dispatchAttempt = Nothing,
                          lastDispatchOutcome = Nothing
                        }
                      predecessor
              ensureValid successor
              Right (successor, True)
        _ -> Right (predecessor, False)

preparedDispatchBindingLost :: PreparedDispatchBindingLoss payload -> Bool
preparedDispatchBindingLost (PreparedDispatchBindingLoss prepared) =
  preparedOutput prepared

commitDispatchBindingLoss ::
  PreparedDispatchBindingLoss payload ->
  (State payload, Bool)
commitDispatchBindingLoss (PreparedDispatchBindingLoss prepared) =
  commitPrepared prepared

-- | Permanently detach dispatch from one retired peer, discard its executable
-- payload/resume state, and retain exact terminal assignment receipts for audit
-- and predecessor-cut reasoning. Contiguous incoming work remains available to
-- its semantic owners under frozen admission authority; noncontiguous gap
-- records were never admitted and are discarded because the retired source can
-- no longer close their missing prefix.
data PeerRetirementOutput = PeerRetirementOutput
  { direction :: Maybe StreamDirection,
    settledAssignments :: [AssignmentReceipt]
  }

newtype PreparedPeerRetirement payload
  = PreparedPeerRetirement
      (Prepared (State payload) PeerRetirementOutput)

preparePeerRetirement ::
  HeraldEpoch ->
  State payload ->
  Either PeerStreamProblem (PreparedPeerRetirement payload)
preparePeerRetirement peer state = do
  ensureValid state
  direction <- outgoingDirection state peer
  PreparedPeerRetirement <$> prepareTransition (install direction) state
  where
    install direction predecessor
      | Set.member peer predecessor.retiredPeers =
          Right
            ( predecessor,
              PeerRetirementOutput
                { direction = Nothing,
                  settledAssignments = []
                }
            )
      | otherwise = do
          current <- outgoingFor direction predecessor
          retiredGap <-
            mapGapProblem (mkGapSummary direction current.receivedPrefix [])
          let newlySettled =
                Map.mapWithKey
                  ( \sequenceNumber item ->
                      assignmentReceipt
                        direction
                        sequenceNumber
                        (sequencedItemDigest item)
                  )
                  current.activeItems
              retiredOutgoing =
                current
                  { activeItems = Map.empty,
                    peerGapSummary = retiredGap,
                    lastResumeGapClaim = Nothing,
                    dispatchBinding = Nothing,
                    dispatchTicket = Nothing,
                    dispatchAttempt = Nothing,
                    lastDispatchOutcome = Nothing,
                    awaitingPeerProgress = Map.empty
                  }
              retiredIncoming =
                case Map.lookup peer predecessor.incoming of
                  Nothing -> predecessor.incoming
                  Just incoming ->
                    Map.insert
                      peer
                      incoming
                        { greatestAdvertisedNextSequence = Nothing,
                          items =
                            Map.filterWithKey
                              (\sequenceNumber _ -> streamPrefixThrough sequenceNumber <= incoming.receivedPrefix)
                              incoming.items
                        }
                      predecessor.incoming
              successor =
                predecessor
                  { outgoing = Map.insert peer retiredOutgoing predecessor.outgoing,
                    incoming = retiredIncoming,
                    retiredPeers = Set.insert peer predecessor.retiredPeers
                  }
          let withMarkerWake = wakeRetiredIncoming (reverseStreamDirection direction) successor
          ensureValid withMarkerWake
          Right
            ( withMarkerWake,
              PeerRetirementOutput
                { direction = Just direction,
                  settledAssignments = Map.elems newlySettled
                }
            )

preparedPeerRetirementDirection ::
  PreparedPeerRetirement payload ->
  Maybe StreamDirection
preparedPeerRetirementDirection (PreparedPeerRetirement prepared) =
  (preparedOutput prepared).direction

preparedPeerRetirementSettledAssignments ::
  PreparedPeerRetirement payload ->
  [AssignmentReceipt]
preparedPeerRetirementSettledAssignments (PreparedPeerRetirement prepared) =
  (preparedOutput prepared).settledAssignments

commitPeerRetirement ::
  PreparedPeerRetirement payload ->
  (State payload, Maybe StreamDirection)
commitPeerRetirement (PreparedPeerRetirement prepared) =
  let (state, output) = commitPrepared prepared
   in (state, output.direction)

peerStreamPeerRetired :: HeraldEpoch -> State payload -> Bool
peerStreamPeerRetired peer state = Set.member peer state.retiredPeers

-- | Irreversibly detach every physical dispatch fact when this State's local
-- Herald epoch has itself been retired.
--
-- Unlike an orderly drain, no already-handed attempt may settle after this
-- cut: the old local epoch can never become active again. Stream transcripts,
-- acknowledgements, and retained payloads remain available as historical
-- evidence, and survivor peers are deliberately not marked retired.
newtype PreparedLocalRetirementCut payload
  = PreparedLocalRetirementCut
      (Prepared (State payload) [StreamDirection])

prepareLocalRetirementCut ::
  State payload ->
  Either PeerStreamProblem (PreparedLocalRetirementCut payload)
prepareLocalRetirementCut state = do
  ensureValid state
  PreparedLocalRetirementCut <$> prepareTransition install state
  where
    install predecessor = do
      cleared <- traverse clearOne (Map.toAscList predecessor.outgoing)
      let successor =
            predecessor
              { outgoing = Map.fromAscList (fmap clearedOutgoing cleared)
              }
          directions =
            [ direction
            | (_, direction, _) <- cleared
            ]
      ensureValid successor
      Right (successor, directions)

    clearOne (peer, outgoing) = do
      direction <- outgoingDirection state peer
      Right
        ( peer,
          direction,
          outgoing
            { dispatchBinding = Nothing,
              dispatchTicket = Nothing,
              dispatchAttempt = Nothing,
              lastDispatchOutcome = Nothing
            }
        )

    clearedOutgoing (peer, _, outgoing) = (peer, outgoing)

preparedLocalRetirementCutDirections ::
  PreparedLocalRetirementCut payload ->
  [StreamDirection]
preparedLocalRetirementCutDirections (PreparedLocalRetirementCut prepared) =
  preparedOutput prepared

commitLocalRetirementCut ::
  PreparedLocalRetirementCut payload ->
  (State payload, [StreamDirection])
commitLocalRetirementCut (PreparedLocalRetirementCut prepared) =
  commitPrepared prepared

newtype PreparedDispatchDrainCut payload
  = PreparedDispatchDrainCut
      (Prepared (State payload) [StreamDirection])

-- | Atomically make every outgoing direction logically unbound at the drain
-- cut, without withdrawing Discovery state or altering retained stream work.
--
-- Current tickets are superseded. A handed attempt remains correlated so one
-- already-selected runtime observation can settle it during Draining, but no
-- replacement ticket is created while the direction is unbound.
prepareDispatchDrainCut ::
  State payload ->
  Either PeerStreamProblem (PreparedDispatchDrainCut payload)
prepareDispatchDrainCut state = do
  ensureValid state
  PreparedDispatchDrainCut <$> prepareTransition install state
  where
    install predecessor = do
      cleared <- traverse clearOne (Map.toAscList predecessor.outgoing)
      let successor =
            predecessor
              { outgoing = Map.fromAscList (fmap clearedOutgoing cleared)
              }
          directions =
            [ direction
            | (_, direction, _, wasBound) <- cleared,
              wasBound
            ]
      ensureValid successor
      Right (successor, directions)

    clearOne (peer, outgoing) = do
      direction <- outgoingDirection state peer
      Right
        ( peer,
          direction,
          outgoing
            { dispatchBinding = Nothing,
              dispatchTicket = Nothing,
              lastDispatchOutcome = Nothing
            },
          case outgoing.dispatchBinding of
            Nothing -> False
            Just _ -> True
        )

    clearedOutgoing (peer, _, outgoing, _) = (peer, outgoing)

preparedDispatchDrainCutDirections ::
  PreparedDispatchDrainCut payload ->
  [StreamDirection]
preparedDispatchDrainCutDirections (PreparedDispatchDrainCut prepared) =
  preparedOutput prepared

commitDispatchDrainCut ::
  PreparedDispatchDrainCut payload ->
  (State payload, [StreamDirection])
commitDispatchDrainCut (PreparedDispatchDrainCut prepared) =
  commitPrepared prepared

newtype PreparedDrainingDispatchOutcome payload
  = PreparedDrainingDispatchOutcome
      (Prepared (State payload) Bool)

-- | Settle exactly one attempt handed before the top-level drain cut.
--
-- The cut has already removed its binding and ticket, so a matching observation
-- may update only the retained protocol-wait bit and terminal outcome memory.
-- Deferred or failed work stays retained and deliberately unscheduled. Stale or
-- duplicate observations are state-identical.
prepareDrainingDispatchOutcome ::
  PeerDispatchAttempt payload ->
  PeerDispatchOutcome ->
  State payload ->
  Either PeerStreamProblem (PreparedDrainingDispatchOutcome payload)
prepareDrainingDispatchOutcome attempt outcome state = do
  ensureValid state
  admitted <- outgoingDirectionExact state (peerDispatchAttemptDirection attempt)
  PreparedDrainingDispatchOutcome <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      current <- outgoingFor admitted predecessor
      case current.dispatchAttempt of
        Just outstanding
          | current.dispatchBinding == Nothing
              && sameDispatchAttempt outstanding attempt -> do
              let sequenceNumber =
                    sequencedItemSequence (peerDispatchAttemptItem attempt)
                  awaiting = case outcome of
                    PeerDispatchWritten ->
                      Map.insert sequenceNumber (awaitingDispatchWrite attempt) current.awaitingPeerProgress
                    PeerDispatchDeferred ->
                      Map.delete sequenceNumber current.awaitingPeerProgress
                    PeerDispatchFailed ->
                      Map.delete sequenceNumber current.awaitingPeerProgress
                  successor =
                    replaceOutgoing
                      admitted
                      current
                        { dispatchAttempt = Nothing,
                          lastDispatchOutcome = Nothing,
                          awaitingPeerProgress = awaiting
                        }
                      predecessor
              ensureValid successor
              Right (successor, True)
        _ -> Right (predecessor, False)

commitDrainingDispatchOutcome ::
  PreparedDrainingDispatchOutcome payload ->
  (State payload, Bool)
commitDrainingDispatchOutcome (PreparedDrainingDispatchOutcome prepared) =
  commitPrepared prepared

newtype PreparedDispatchTicket payload
  = PreparedDispatchTicket
      (Prepared (State payload) (Maybe PeerDispatchTicket))

-- | Coalescing ensure operation for an already observed binding.
prepareEnsureDispatchTicket ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem (PreparedDispatchTicket payload)
prepareEnsureDispatchTicket direction state = do
  ensureValid state
  admitted <- outgoingDirectionExact state direction
  ensurePeerActive state (streamDirectionDestination admitted)
  PreparedDispatchTicket
    <$> prepareTransition
      ( \predecessor -> do
          (successor, ticket) <- ensureDispatchFor admitted predecessor
          ensureValid successor
          Right (successor, ticket)
      )
      state

preparedDispatchTicket ::
  PreparedDispatchTicket payload ->
  Maybe PeerDispatchTicket
preparedDispatchTicket (PreparedDispatchTicket prepared) = preparedOutput prepared

commitDispatchTicket ::
  PreparedDispatchTicket payload ->
  (State payload, Maybe PeerDispatchTicket)
commitDispatchTicket (PreparedDispatchTicket prepared) = commitPrepared prepared

newtype PreparedDispatchSelection payload
  = PreparedDispatchSelection
      (Prepared (State payload) (Maybe (PeerDispatchAttempt payload)))

-- | Select the lowest eligible exact retained item for the current ticket and
-- binding. Stale tickets, stale bindings, and duplicate selection while an
-- attempt is current are state-identical no-ops.
prepareDispatchSelection ::
  PeerDispatchTicket ->
  PeerDispatchBindingGeneration ->
  State payload ->
  Either PeerStreamProblem (PreparedDispatchSelection payload)
prepareDispatchSelection ticket binding state = do
  ensureValid state
  admitted <- outgoingDirectionExact state (peerDispatchTicketDirection ticket)
  ensurePeerActive state (streamDirectionDestination admitted)
  PreparedDispatchSelection <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      current <- outgoingFor admitted predecessor
      case current.dispatchAttempt of
        Just _ -> Right (predecessor, Nothing)
        Nothing
          | current.dispatchBinding /= Just binding
              || current.dispatchTicket /= Just ticket ->
              Right (predecessor, Nothing)
        Nothing -> case lowestEligibleItem current of
          Nothing -> do
            let peer = streamDirectionDestination admitted
                successor =
                  predecessor
                    { outgoing =
                        Map.insert
                          peer
                          current {dispatchTicket = Nothing}
                          predecessor.outgoing
                    }
            ensureValid successor
            Right (successor, Nothing)
          Just item -> do
            let peer = streamDirectionDestination admitted
                generation =
                  peerDispatchAttemptGenerationForOwner
                    current.nextDispatchAttemptGeneration
                attempt = peerDispatchAttempt binding generation item
                updated =
                  current
                    { nextDispatchAttemptGeneration =
                        current.nextDispatchAttemptGeneration + 1,
                      dispatchTicket = Nothing,
                      dispatchAttempt = Just attempt,
                      lastDispatchOutcome = Nothing
                    }
                successor =
                  predecessor
                    { outgoing = Map.insert peer updated predecessor.outgoing
                    }
            ensureValid successor
            Right (successor, Just attempt)

preparedDispatchSelectionAttempt ::
  PreparedDispatchSelection payload ->
  Maybe (PeerDispatchAttempt payload)
preparedDispatchSelectionAttempt (PreparedDispatchSelection prepared) =
  preparedOutput prepared

commitDispatchSelection ::
  PreparedDispatchSelection payload ->
  (State payload, Maybe (PeerDispatchAttempt payload))
commitDispatchSelection (PreparedDispatchSelection prepared) =
  commitPrepared prepared

newtype PreparedDispatchOutcome payload
  = PreparedDispatchOutcome
      (Prepared (State payload) (Maybe PeerDispatchTicket))

-- | Settle one exact attempt observation.
--
-- Written suppresses routine resend of this item until resume or binding
-- recovery makes it eligible again. Deferred and failed retain it as immediately
-- eligible. An exact repeated outcome is idempotent; observations for a stale or
-- superseded attempt are no-ops.
prepareDispatchOutcome ::
  PeerDispatchAttempt payload ->
  PeerDispatchOutcome ->
  State payload ->
  Either PeerStreamProblem (PreparedDispatchOutcome payload)
prepareDispatchOutcome attempt outcome state = do
  ensureValid state
  admitted <- outgoingDirectionExact state (peerDispatchAttemptDirection attempt)
  PreparedDispatchOutcome <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      current <- outgoingFor admitted predecessor
      case current.dispatchAttempt of
        Just outstanding
          | sameDispatchAttempt outstanding attempt ->
              settleCurrent admitted current predecessor
        _ -> case current.lastDispatchOutcome of
          Just (settled, retainedOutcome)
            | isLatestSettled current settled
                && sameDispatchAttempt settled attempt ->
                if retainedOutcome == outcome
                  then Right (predecessor, Nothing)
                  else
                    Left
                      ( PeerStreamInvariantProblem
                          ( PeerStreamDispatchOutcomeContradiction
                              admitted
                              (peerDispatchAttemptGeneration attempt)
                              retainedOutcome
                              outcome
                          )
                      )
          _ -> Right (predecessor, Nothing)

    settleCurrent admitted current predecessor = do
      let peer = streamDirectionDestination admitted
          sequenceNumber =
            sequencedItemSequence (peerDispatchAttemptItem attempt)
          awaiting = case outcome of
            PeerDispatchWritten ->
              Map.insert sequenceNumber (awaitingDispatchWrite attempt) current.awaitingPeerProgress
            PeerDispatchDeferred ->
              Map.delete sequenceNumber current.awaitingPeerProgress
            PeerDispatchFailed ->
              Map.delete sequenceNumber current.awaitingPeerProgress
          settled =
            current
              { dispatchAttempt = Nothing,
                lastDispatchOutcome = Just (attempt, outcome),
                awaitingPeerProgress = awaiting
              }
          withOutcome =
            predecessor
              { outgoing = Map.insert peer settled predecessor.outgoing
              }
      (successor, ticket) <- ensureDispatchFor admitted withOutcome
      ensureValid successor
      Right (successor, ticket)

preparedDispatchOutcomeTicket ::
  PreparedDispatchOutcome payload ->
  Maybe PeerDispatchTicket
preparedDispatchOutcomeTicket (PreparedDispatchOutcome prepared) =
  preparedOutput prepared

commitDispatchOutcome ::
  PreparedDispatchOutcome payload ->
  (State payload, Maybe PeerDispatchTicket)
commitDispatchOutcome (PreparedDispatchOutcome prepared) = commitPrepared prepared

-- | First receive stage.  It deliberately has no commit operation.
data PreparedReceiveCandidate payload = PreparedReceiveCandidate
  { successor :: State payload,
    direction :: StreamDirection,
    disposition :: ReceiveDisposition,
    newlyContiguous :: [SequencedItem payload]
  }

prepareReceiveCandidate ::
  SequencedItem payload ->
  State payload ->
  Either PeerStreamProblem (PreparedReceiveCandidate payload)
prepareReceiveCandidate item state = do
  ensureValid state
  direction <- incomingDirection state (sequencedItemDirection item)
  ensurePeerActive state (streamDirectionSource direction)
  let peer = streamDirectionSource direction
      current = incomingFor peer state
      sequenceNumber = sequencedItemSequence item
  checkAdvertisedFrontier direction sequenceNumber current
  case Map.lookup sequenceNumber current.items of
    Just retained
      | sequencedItemDigest retained.item == sequencedItemDigest item ->
          Right
            PreparedReceiveCandidate
              { successor = state,
                direction,
                disposition = ReceiveDuplicate,
                newlyContiguous = []
              }
      | otherwise ->
          Left
            ( PeerStreamProtocolProblem
                ( PeerStreamConflictingItem
                    direction
                    sequenceNumber
                    (sequencedItemDigest retained.item)
                    (sequencedItemDigest item)
                )
            )
    Nothing -> do
      let nextMissing = nextAfterStreamPrefix current.receivedPrefix
          inserted =
            current
              { items =
                  Map.insert
                    sequenceNumber
                    IncomingRecord {item, status = GapRetained}
                    current.items
              }
          nextState =
            installIncoming direction inserted state
      if sequenceNumber < nextMissing
        then
          if receiptIsRetired (streamSequenceWord64 sequenceNumber) current.completion
            then Right PreparedReceiveCandidate {successor = state, direction, disposition = ReceiveDuplicate, newlyContiguous = []}
            else Left (PeerStreamInvariantProblem (PeerStreamIncomingInvariant direction))
        else
          if sequenceNumber > nextMissing
            then
              Right
                PreparedReceiveCandidate
                  { successor = nextState,
                    direction,
                    disposition = ReceiveGap nextMissing,
                    newlyContiguous = []
                  }
            else
              Right
                PreparedReceiveCandidate
                  { successor = nextState,
                    direction,
                    disposition = ReceiveContiguous,
                    newlyContiguous = contiguousFrom nextMissing inserted.items
                  }

preparedReceiveDisposition ::
  PreparedReceiveCandidate payload ->
  ReceiveDisposition
preparedReceiveDisposition candidate = candidate.disposition

preparedContiguousItems ::
  PreparedReceiveCandidate payload ->
  [SequencedItem payload]
preparedContiguousItems candidate = candidate.newlyContiguous

-- | Remove one previously buffered gap item when semantic planning proves
-- that its retained payload can no longer be admitted.  The newly received
-- head and every item before the target remain contiguous; later buffered
-- items stay gap-retained behind the reopened sequence.
--
-- The target must be in the tail of the candidate's contiguous run.  Its head
-- is the item supplied to 'prepareReceiveCandidate' and therefore was not
-- previously retained. The candidate remains an intentionally intermediate
-- state until 'finalizeReceive' installs progress for that head; finalization
-- performs the complete successor invariant check.
discardPreviouslyRetainedContiguousAt ::
  StreamSequence ->
  PreparedReceiveCandidate payload ->
  Either PeerStreamProblem (PreparedReceiveCandidate payload)
discardPreviouslyRetainedContiguousAt target candidate =
  case break ((== target) . sequencedItemSequence) candidate.newlyContiguous of
    (retainedPrefix@(_ : _), _ : _) -> case Map.lookup target current.items of
      Just record
        | record.status == GapRetained ->
            let updated =
                  current
                    { items = Map.delete target current.items
                    }
                successor =
                  installIncoming candidate.direction updated candidate.successor
             in Right
                  candidate
                    { successor,
                      newlyContiguous = retainedPrefix
                    }
      _ -> invalid
    _ -> invalid
  where
    peer = streamDirectionSource candidate.direction
    current = incomingFor peer candidate.successor
    invalid =
      Left
        ( PeerStreamInvariantProblem
            (PeerStreamIncomingInvariant candidate.direction)
        )

newtype PreparedReceive payload
  = PreparedReceive
      (Prepared (State payload) ReceiveResult)

finalizeReceive ::
  Map StreamSequence ReceiveProgress ->
  PreparedReceiveCandidate payload ->
  Either PeerStreamProblem (PreparedReceive payload)
finalizeReceive progress candidate
  | supplied /= expected =
      Left
        ( PeerStreamInvariantProblem
            (PeerStreamReceiveProgressMismatch expected supplied)
        )
  | otherwise = do
      PreparedReceive
        <$> prepareTransition install candidate.successor
  where
    expected = Set.fromList (fmap sequencedItemSequence candidate.newlyContiguous)
    supplied = Map.keysSet progress
    peer = streamDirectionSource candidate.direction

    install state = do
      let current = incomingFor peer state
          progressed =
            current
              { items =
                  Map.mapWithKey
                    (applyProgress progress)
                    current.items
              }
          updated = compactIncoming progressed
          received = updated.receivedPrefix
          nextState = installIncoming candidate.direction updated state
      gap <- deriveIncomingGap candidate.direction updated
      let output =
            receiveResult
              candidate.disposition
              (streamWatermarksWithCompletion received updated.completion)
              gap
      ensureValid nextState
      Right (nextState, output)

preparedReceiveResult :: PreparedReceive payload -> ReceiveResult
preparedReceiveResult (PreparedReceive prepared) = preparedOutput prepared

commitReceive :: PreparedReceive payload -> (State payload, ReceiveResult)
commitReceive (PreparedReceive prepared) = commitPrepared prepared

applyProgress ::
  Map StreamSequence ReceiveProgress ->
  StreamSequence ->
  IncomingRecord payload ->
  IncomingRecord payload
applyProgress progress sequenceNumber record =
  case Map.lookup sequenceNumber progress of
    Nothing -> record
    Just ReceivedPending -> record {status = IncomingReceivedPending}
    Just Completed -> record {status = IncomingCompleted}

newtype PreparedCompletion payload
  = PreparedCompletion
      (Prepared (State payload) StreamWatermarks)

prepareCompletion ::
  StreamDirection ->
  Set StreamSequence ->
  State payload ->
  Either PeerStreamProblem (PreparedCompletion payload)
prepareCompletion direction sequences state = do
  ensureValid state
  admitted <- incomingDirection state direction
  PreparedCompletion <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      let peer = streamDirectionSource admitted
          current = incomingFor peer predecessor
      if Set.null sequences
        then
          Right
            ( predecessor,
              streamWatermarksWithCompletion current.receivedPrefix current.completion
            )
        else do
          mapM_ (checkCompletable admitted current) (Set.toAscList sequences)
          let updatedItems =
                Map.mapWithKey
                  ( \sequenceNumber record ->
                      if Set.member sequenceNumber sequences
                        then record {status = IncomingCompleted}
                        else record
                  )
                  current.items
              updated = compactIncoming current {items = updatedItems}
              successor =
                installIncoming admitted updated predecessor
              output =
                streamWatermarksWithCompletion updated.receivedPrefix updated.completion
          ensureValid successor
          Right (successor, output)

preparedCompletionWatermarks ::
  PreparedCompletion payload ->
  StreamWatermarks
preparedCompletionWatermarks (PreparedCompletion prepared) = preparedOutput prepared

commitCompletion ::
  PreparedCompletion payload ->
  (State payload, StreamWatermarks)
commitCompletion (PreparedCompletion prepared) = commitPrepared prepared

checkCompletable ::
  StreamDirection ->
  Incoming payload ->
  StreamSequence ->
  Either PeerStreamProblem ()
checkCompletable direction incoming sequenceNumber =
  case Map.lookup sequenceNumber incoming.items of
    Nothing
      | receiptIsRetired (streamSequenceWord64 sequenceNumber) incoming.completion -> Right ()
      | otherwise -> Left (PeerStreamProtocolProblem (PeerStreamCompletionUnknown direction sequenceNumber))
    Just record
      | record.status == GapRetained ->
          Left
            ( PeerStreamProtocolProblem
                (PeerStreamCompletionBeforeReceipt direction sequenceNumber)
            )
      | otherwise -> Right ()

data DispatchWatermarkOutput = DispatchWatermarkOutput
  { watermarks :: StreamWatermarks,
    ticket :: Maybe PeerDispatchTicket
  }

newtype PreparedReceivedAck payload
  = PreparedReceivedAck
      (Prepared (State payload) DispatchWatermarkOutput)

prepareReceivedAck ::
  StreamDirection ->
  StreamPrefix ->
  State payload ->
  Either PeerStreamProblem (PreparedReceivedAck payload)
prepareReceivedAck direction prefix state = do
  ensureValid state
  admitted <- outgoingDirectionExact state direction
  PreparedReceivedAck <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      let peer = streamDirectionDestination admitted
      current <- outgoingFor admitted predecessor
      let lastAllocated = lastAllocatedPrefix current
      if Set.member peer predecessor.retiredPeers
        then
          Right
            ( predecessor,
              DispatchWatermarkOutput
                (streamWatermarksWithCompletion current.receivedPrefix current.completion)
                Nothing
            )
        else
          if prefix > lastAllocated
            then acknowledgementAhead admitted prefix lastAllocated
            else
              if prefix <= current.receivedPrefix
                then do
                  (successor, ticket) <- ensureDispatchFor admitted predecessor
                  ensureValid successor
                  Right
                    ( successor,
                      DispatchWatermarkOutput
                        (streamWatermarksWithCompletion current.receivedPrefix current.completion)
                        ticket
                    )
                else do
                  let received = max current.receivedPrefix prefix
                  gap <-
                    normalizePeerGap
                      admitted
                      received
                      current.peerGapSummary
                      current.activeItems
                  let updated =
                        supersedeDispatchThrough
                          received
                          current
                            { receivedPrefix = received,
                              peerGapSummary = gap,
                              awaitingPeerProgress =
                                Map.union
                                  (Map.fromSet (const AwaitingPeerReceipt) (Set.filter (\sequenceNumber -> streamPrefixThrough sequenceNumber <= received) (Map.keysSet current.activeItems)))
                                  current.awaitingPeerProgress
                            }
                      withAck =
                        predecessor {outgoing = Map.insert peer updated predecessor.outgoing}
                  (successor, ticket) <- ensureDispatchFor admitted withAck
                  ensureValid successor
                  Right
                    ( successor,
                      DispatchWatermarkOutput
                        (streamWatermarksWithCompletion received updated.completion)
                        ticket
                    )

preparedReceivedAckWatermarks ::
  PreparedReceivedAck payload ->
  StreamWatermarks
preparedReceivedAckWatermarks (PreparedReceivedAck prepared) =
  (preparedOutput prepared).watermarks

preparedReceivedAckDispatchTicket ::
  PreparedReceivedAck payload ->
  Maybe PeerDispatchTicket
preparedReceivedAckDispatchTicket (PreparedReceivedAck prepared) =
  (preparedOutput prepared).ticket

commitReceivedAck ::
  PreparedReceivedAck payload ->
  (State payload, StreamWatermarks)
commitReceivedAck (PreparedReceivedAck prepared) =
  let (state, output) = commitPrepared prepared
   in (state, output.watermarks)

newtype PreparedCompletedAck payload
  = PreparedCompletedAck
      (Prepared (State payload) DispatchWatermarkOutput)

prepareCompletedAck ::
  StreamDirection ->
  StreamPrefix ->
  State payload ->
  Either PeerStreamProblem (PreparedCompletedAck payload)
prepareCompletedAck direction prefix = prepareCompletedProgress direction (streamPrefixCompletion prefix)

-- | The destination certifies exactly the semantically completed assignments.
-- Holes remain active; later completed payloads need no per-assignment receipt.
prepareCompletedProgress :: StreamDirection -> ReceiptRetirement -> State payload -> Either PeerStreamProblem (PreparedCompletedAck payload)
prepareCompletedProgress direction progress state = do
  ensureValid state
  admitted <- outgoingDirectionExact state direction
  PreparedCompletedAck <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      let peer = streamDirectionDestination admitted
      current <- outgoingFor admitted predecessor
      if Set.member peer predecessor.retiredPeers
        then Right (predecessor, DispatchWatermarkOutput (streamWatermarksWithCompletion current.receivedPrefix current.completion) Nothing)
        else do
          validateCompletionProgress admitted (lastAllocatedPrefix current) progress
          let completion = current.completion <> progress
              completed = streamCompletionPrefix completion
              received = max current.receivedPrefix (completionHighPrefix completion)
              active = Map.filterWithKey (\sequenceNumber _ -> not (receiptIsRetired (streamSequenceWord64 sequenceNumber) completion)) current.activeItems
          if completion == current.completion && received == current.receivedPrefix
            then Right (predecessor, DispatchWatermarkOutput (streamWatermarksWithCompletion received completion) Nothing)
            else do
              gap <- normalizePeerGap admitted received current.peerGapSummary active
              resumeGap <- traverse (retainActiveGapEntries active) current.lastResumeGapClaim
              let updated =
                    supersedeDispatchThrough received
                      $ retainDispatchForActive
                        active
                        current
                          { completedPrefix = completed,
                            completion,
                            receivedPrefix = received,
                            activeItems = active,
                            peerGapSummary = gap,
                            lastResumeGapClaim = resumeGap,
                            awaitingPeerProgress = Map.union (Map.fromSet (const AwaitingPeerReceipt) (Set.filter (\sequenceNumber -> streamPrefixThrough sequenceNumber <= received) (Map.keysSet active))) (Map.restrictKeys current.awaitingPeerProgress (Map.keysSet active))
                          }
                  withAck = predecessor {outgoing = Map.insert peer updated predecessor.outgoing}
              (successor, ticket) <- ensureDispatchFor admitted withAck
              ensureValid successor
              Right (successor, DispatchWatermarkOutput (streamWatermarksWithCompletion received completion) ticket)

validateCompletionProgress :: StreamDirection -> StreamPrefix -> ReceiptRetirement -> Either PeerStreamProblem ()
validateCompletionProgress direction allocated progress =
  case checkStreamCompletion progress of
    Left _ -> invalid
    Right _ | completionHighPrefix progress > allocated -> acknowledgementAhead direction (completionHighPrefix progress) allocated
    Right _ -> Right ()
  where
    invalid = Left (PeerStreamProtocolProblem (PeerStreamInvalidCompletionProgress direction progress))

preparedCompletedAckWatermarks ::
  PreparedCompletedAck payload ->
  StreamWatermarks
preparedCompletedAckWatermarks (PreparedCompletedAck prepared) =
  (preparedOutput prepared).watermarks

preparedCompletedAckDispatchTicket ::
  PreparedCompletedAck payload ->
  Maybe PeerDispatchTicket
preparedCompletedAckDispatchTicket (PreparedCompletedAck prepared) =
  (preparedOutput prepared).ticket

commitCompletedAck ::
  PreparedCompletedAck payload ->
  (State payload, StreamWatermarks)
commitCompletedAck (PreparedCompletedAck prepared) =
  let (state, output) = commitPrepared prepared
   in (state, output.watermarks)

reconcileIncomingFrontier ::
  StreamDirection ->
  StreamSequence ->
  Incoming payload ->
  Either PeerStreamProblem (Incoming payload)
reconcileIncomingFrontier direction offered current = do
  case current.greatestAdvertisedNextSequence of
    Just previous
      | offered < previous ->
          Left
            ( PeerStreamProtocolProblem
                ( PeerStreamSourceFrontierRegressed
                    direction
                    previous
                    offered
                )
            )
    _ -> Right ()
  if nextAfterStreamPrefix current.receivedPrefix > offered
    || maybe False (>= offered) (largestIncomingSequence current)
    then
      Left
        ( PeerStreamProtocolProblem
            ( PeerStreamSourceFrontierContradictsHistory
                direction
                offered
                current.receivedPrefix
            )
        )
    else
      Right
        current
          { greatestAdvertisedNextSequence =
              Just
                ( maybe
                    offered
                    (max offered)
                    current.greatestAdvertisedNextSequence
                )
          }

newtype PreparedFrontierAdvance payload
  = PreparedFrontierAdvance (Prepared (State payload) ())

-- | Retain only a peer's monotone outgoing allocation frontier. Unlike a full
-- resume, this does not inspect or reactivate the reverse outgoing direction.
prepareFrontierAdvance ::
  StreamDirection ->
  StreamSequence ->
  State payload ->
  Either PeerStreamProblem (PreparedFrontierAdvance payload)
prepareFrontierAdvance direction offered state = do
  ensureValid state
  admitted <- incomingDirection state direction
  ensurePeerActive state (streamDirectionSource admitted)
  PreparedFrontierAdvance <$> prepareTransition (install admitted) state
  where
    install admitted predecessor = do
      let sourcePeer = streamDirectionSource admitted
          current = incomingFor sourcePeer predecessor
      updated <- reconcileIncomingFrontier admitted offered current
      let successor =
            installIncoming admitted updated predecessor
      ensureValid successor
      Right (successor, ())

commitFrontierAdvance :: PreparedFrontierAdvance payload -> State payload
commitFrontierAdvance (PreparedFrontierAdvance prepared) =
  fst (commitPrepared prepared)

data ResumeOutput payload
  = ResumeOutput
      ResumeResponse
      [SequencedItem payload]
      (Maybe PeerDispatchTicket)

-- | Whether this offer is the first one admitted after the current accepted or
-- reoffered Hello association root. The first offer reactivates the exact
-- unacknowledged suffix retained across binding loss, preserving attempts and
-- successful writes already made after that Hello. Its remote snapshot
-- may predate those writes. Later offers reactivate only items newly requested
-- by the change in canonical raw gap evidence.
data ResumeAssociationPhase
  = FirstResumeOnAssociation
  | ContinuingResumeOnAssociation
  deriving stock (Eq, Show)

newtype PreparedResume payload
  = PreparedResume
      (Prepared (State payload) (ResumeOutput payload))

prepareResume ::
  ResumeOffer ->
  State payload ->
  Either PeerStreamProblem (PreparedResume payload)
prepareResume = prepareResumeForAssociation FirstResumeOnAssociation

prepareResumeForAssociation ::
  ResumeAssociationPhase ->
  ResumeOffer ->
  State payload ->
  Either PeerStreamProblem (PreparedResume payload)
prepareResumeForAssociation association offer state = do
  ensureValid state
  ensurePeerActive state (resumeOfferSource offer)
  if resumeOfferDestination offer /= state.localEpoch
    then
      Left
        ( PeerStreamProtocolProblem
            ( PeerStreamResumeDestinationMismatch
                state.localEpoch
                (resumeOfferDestination offer)
            )
        )
    else PreparedResume <$> prepareTransition install state
  where
    install predecessor = do
      forward <-
        mapDirectionProblem
          (mkStreamDirection (resumeOfferSource offer) predecessor.localEpoch)
      let sourcePeer = resumeOfferSource offer
          incomingCurrent = incomingFor sourcePeer predecessor
      reverseDirection <-
        mapDirectionProblem (mkStreamDirection predecessor.localEpoch sourcePeer)
      incomingUpdated <-
        reconcileIncomingFrontier
          forward
          (resumeOfferNextSourceSequence offer)
          incomingCurrent
      outgoingCurrent <- outgoingFor reverseDirection predecessor
      (outgoingUpdated, candidateRetransmissions, confirmedGaps) <-
        reconcileDestinationClaim reverseDirection outgoingCurrent
      let rawGapClaim = resumeOfferGapSummary offer
          retransmissions = case association of
            FirstResumeOnAssociation ->
              filter (not . dispatchedOnCurrentAssociation outgoingCurrent) candidateRetransmissions
            ContinuingResumeOnAssociation ->
              case outgoingCurrent.lastResumeGapClaim of
                Nothing -> candidateRetransmissions
                Just previousClaim ->
                  filter
                    ( (`Set.notMember` requestedSequences previousClaim)
                        . sequencedItemSequence
                    )
                    candidateRetransmissions
          requestedSequences claim =
            let confirmed = Set.fromList (fmap fst (gapSummaryEntries claim))
                received = gapSummaryReceivedPrefix claim
             in Set.fromList
                  [ sequenceNumber
                  | sequenceNumber <- Map.keys outgoingUpdated.activeItems,
                    streamPrefixThrough sequenceNumber > received,
                    Set.notMember sequenceNumber confirmed
                  ]
      retainedGapClaim <- retainActiveGapEntries outgoingUpdated.activeItems rawGapClaim
      let retransmissionSequences =
            Set.fromList (fmap sequencedItemSequence retransmissions)
          peerConfirmedSequences =
            retransmissionSequences `Set.union` confirmedGaps
          resumedOutgoing =
            supersedeDispatchThrough (resumeOfferReceivedPrefix offer)
              . retainDispatchForActive outgoingUpdated.activeItems
              . supersedeDispatchSequences peerConfirmedSequences
              $ outgoingUpdated
                { lastResumeGapClaim = Just retainedGapClaim,
                  awaitingPeerProgress =
                    Map.withoutKeys outgoingUpdated.awaitingPeerProgress retransmissionSequences
                }
          withResume =
            installIncoming
              forward
              incomingUpdated
              predecessor
                { outgoing = Map.insert sourcePeer resumedOutgoing predecessor.outgoing
                }
          response =
            resumeResponseWithCompletion
              incomingUpdated.receivedPrefix
              incomingUpdated.completion
              (nextAfterStreamPrefix incomingUpdated.receivedPrefix)
      (successor, ticket) <- ensureDispatchFor reverseDirection withResume
      ensureValid successor
      Right (successor, ResumeOutput response retransmissions ticket)

    reconcileDestinationClaim direction current = do
      let offeredReceived = resumeOfferReceivedPrefix offer
          offeredCompleted = resumeOfferCompletedPrefix offer
          lastAllocated = lastAllocatedPrefix current
      if offeredCompleted > lastAllocated
        then
          Left
            ( PeerStreamProtocolProblem
                ( PeerStreamResumeCompletedBeyondAllocated
                    direction
                    offeredCompleted
                    lastAllocated
                )
            )
        else Right ()
      if offeredReceived > lastAllocated
        then
          Left
            ( PeerStreamProtocolProblem
                ( PeerStreamResumeReceivedBeyondAllocated
                    direction
                    offeredReceived
                    lastAllocated
                )
            )
        else Right ()
      validateCompletionProgress direction lastAllocated (resumeOfferCompletion offer)
      let completion = current.completion <> resumeOfferCompletion offer
          active = Map.filterWithKey (\sequenceNumber _ -> not (receiptIsRetired (streamSequenceWord64 sequenceNumber) completion)) current.activeItems
      confirmedGaps <- validateOfferedGaps direction completion active
      let retransmissions =
            [ item
            | (sequenceNumber, item) <- Map.toAscList active,
              streamPrefixThrough sequenceNumber > offeredReceived,
              Set.notMember sequenceNumber confirmedGaps
            ]
          received =
            maximum
              [ current.receivedPrefix,
                offeredReceived,
                offeredCompleted
              ]
      peerGap <-
        normalizeOfferedGap
          direction
          received
          active
          (resumeOfferGapSummary offer)
      let confirmedSequences =
            (Set.filter (\sequenceNumber -> streamPrefixThrough sequenceNumber <= offeredReceived) (Map.keysSet active))
              `Set.union` confirmedGaps
      Right
        ( current
            { receivedPrefix = received,
              completedPrefix = streamCompletionPrefix completion,
              completion,
              activeItems = active,
              peerGapSummary = peerGap,
              awaitingPeerProgress =
                Map.union
                  (Map.fromSet (const AwaitingPeerReceipt) confirmedSequences)
                  (Map.restrictKeys current.awaitingPeerProgress (Map.keysSet active))
            },
          retransmissions,
          confirmedGaps
        )

    validateOfferedGaps direction completion active = do
      let offeredGap = resumeOfferGapSummary offer
      Set.fromList
        <$> traverse (validateEntry direction active) (filter (\(sequenceNumber, _) -> not (receiptIsRetired (streamSequenceWord64 sequenceNumber) completion)) (gapSummaryEntries offeredGap))

preparedResumeResponse :: PreparedResume payload -> ResumeResponse
preparedResumeResponse (PreparedResume prepared) =
  case preparedOutput prepared of
    ResumeOutput response _ _ -> response

preparedResumeRetransmissions ::
  PreparedResume payload ->
  [SequencedItem payload]
preparedResumeRetransmissions (PreparedResume prepared) =
  case preparedOutput prepared of
    ResumeOutput _ retransmissions _ -> retransmissions

preparedResumeDispatchTicket ::
  PreparedResume payload ->
  Maybe PeerDispatchTicket
preparedResumeDispatchTicket (PreparedResume prepared) =
  case preparedOutput prepared of
    ResumeOutput _ _ ticket -> ticket

commitResume ::
  PreparedResume payload ->
  (State payload, ResumeResponse, [SequencedItem payload])
commitResume (PreparedResume prepared) =
  case commitPrepared prepared of
    (state, ResumeOutput response retransmissions _) ->
      (state, response, retransmissions)

outgoingWatermarks ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem StreamWatermarks
outgoingWatermarks direction state = do
  admitted <- outgoingDirectionExact state direction
  current <- outgoingFor admitted state
  Right (streamWatermarksWithCompletion current.receivedPrefix current.completion)

-- | Read the next allocation cursor for one exact outgoing direction.
outgoingNextSequence ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem StreamSequence
outgoingNextSequence direction state = do
  admitted <- outgoingDirectionExact state direction
  current <- outgoingFor admitted state
  Right current.nextSequence

incomingWatermarks ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem StreamWatermarks
incomingWatermarks direction state = do
  admitted <- incomingDirection state direction
  let current = incomingFor (streamDirectionSource admitted) state
  Right (streamWatermarksWithCompletion current.receivedPrefix current.completion)

-- | The cumulative completion owned by each incoming direction. Coordinators
-- use this narrow snapshot to wake work hidden behind a newly completed stream
-- predecessor without treating outgoing dispatch churn as incoming progress.
incomingCompletionSnapshot :: State payload -> Map HeraldEpoch StreamPrefix
incomingCompletionSnapshot state = (.completedPrefix) <$> state.incoming

-- | Exact currently retained outgoing directions without running the
-- whole-owner invariant validator.  Narrow owner adapters use this read port
-- to inspect only the streams relevant to their own evidence projection.
peerStreamOutgoingDirections ::
  State payload ->
  Either PeerStreamProblem [StreamDirection]
peerStreamOutgoingDirections state =
  traverse (outgoingDirection state) (Map.keys state.outgoing)

-- | Exact currently retained incoming directions without interpreting any
-- item payload or validating unrelated stream state.
peerStreamIncomingDirections ::
  State payload ->
  Either PeerStreamProblem [StreamDirection]
peerStreamIncomingDirections state =
  traverse
    (mapDirectionProblem . (`mkStreamDirection` state.localEpoch))
    (Map.keys state.incoming)

activeOutgoingItems ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem [SequencedItem payload]
activeOutgoingItems direction state = do
  admitted <- outgoingDirectionExact state direction
  current <- outgoingFor admitted state
  Right (Map.elems current.activeItems)

incomingRetainedItems ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem [(SequencedItem payload, IncomingItemStatus)]
incomingRetainedItems direction state = do
  admitted <- incomingDirection state direction
  let current = incomingFor (streamDirectionSource admitted) state
  Right [(record.item, record.status) | record <- Map.elems current.items]

-- | The retained ordered successor, in logarithmic time. Completed payloads
-- have already been compacted; a GapRetained record still blocks admission.
incomingItemAtOrAfter :: StreamDirection -> StreamSequence -> State payload -> Either PeerStreamProblem (Maybe (SequencedItem payload, IncomingItemStatus))
incomingItemAtOrAfter direction start state = do
  admitted <- incomingDirection state direction
  let current = incomingFor (streamDirectionSource admitted) state
  Right ((\(_, record) -> (record.item, record.status)) <$> Map.lookupGE start current.items)

-- | Completion authority is sequence-qualified. Semantic owners retain their
-- immutable admitted assignment identity; active payloads still check its digest.
assignmentCompletionKnown :: AssignmentReceipt -> State payload -> Either PeerStreamProblem Bool
assignmentCompletionKnown receipt state = do
  direction <- outgoingDirectionExact state (assignmentReceiptDirection receipt)
  current <- outgoingFor direction state
  Right (receiptIsRetired (streamSequenceWord64 (assignmentReceiptSequence receipt)) current.completion)

assignmentRetirementKnown :: AssignmentReceipt -> State payload -> Either PeerStreamProblem Bool
assignmentRetirementKnown receipt state = do
  direction <- outgoingDirectionExact state (assignmentReceiptDirection receipt)
  current <- outgoingFor direction state
  let sequenceNumber = assignmentReceiptSequence receipt
  Right (Set.member (streamDirectionDestination direction) state.retiredPeers && sequenceNumber < current.nextSequence && not (receiptIsRetired (streamSequenceWord64 sequenceNumber) current.completion))

incomingAssignmentCompleted :: AssignmentReceipt -> State payload -> Either PeerStreamProblem Bool
incomingAssignmentCompleted receipt state = do
  direction <- incomingDirection state (assignmentReceiptDirection receipt)
  Right (receiptIsRetired (streamSequenceWord64 (assignmentReceiptSequence receipt)) (incomingFor (streamDirectionSource direction) state).completion)

-- | Compact release test for already admitted semantic assignment indices.
incomingSequenceCompleted :: (StreamDirection, StreamSequence) -> State payload -> Bool
incomingSequenceCompleted (direction, sequenceNumber) state =
  streamDirectionDestination direction == state.localEpoch
    && receiptIsRetired
      (streamSequenceWord64 sequenceNumber)
      (incomingFor (streamDirectionSource direction) state).completion

-- | Skip only certified completed positions, without enumerating their history.
nextUncompletedIncomingSequence :: StreamDirection -> StreamSequence -> State payload -> Either PeerStreamProblem StreamSequence
nextUncompletedIncomingSequence direction start state = do
  admitted <- incomingDirection state direction
  let current = incomingFor (streamDirectionSource admitted) state
  Right
    $ if streamPrefixThrough start > current.receivedPrefix
      then start
      else case Map.lookupGE start current.items of
        Just (sequenceNumber, _) | streamPrefixThrough sequenceNumber <= current.receivedPrefix -> sequenceNumber
        _ -> nextAfterStreamPrefix current.receivedPrefix

outgoingCompletionProgress :: StreamDirection -> State payload -> Either PeerStreamProblem ReceiptRetirement
outgoingCompletionProgress direction state = do
  admitted <- outgoingDirectionExact state direction
  (.completion) <$> outgoingFor admitted state

-- | Active outgoing payloads, pending/gap incoming payloads, and retained raw
-- Resume gap receipts. Completed assignments contribute no historical records.
peerStreamRetentionCounts :: State payload -> (Int, Int, Int)
peerStreamRetentionCounts state = (sum (fmap (Map.size . (.activeItems)) state.outgoing), sum (fmap (Map.size . (.items)) state.incoming), sum [maybe 0 (length . gapSummaryEntries) outgoing.lastResumeGapClaim | outgoing <- Map.elems state.outgoing])

outgoingPeerGapSummary ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem GapSummary
outgoingPeerGapSummary direction state = do
  admitted <- outgoingDirectionExact state direction
  current <- outgoingFor admitted state
  Right current.peerGapSummary

incomingGapSummary ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem GapSummary
incomingGapSummary direction state = do
  admitted <- incomingDirection state direction
  deriveIncomingGap admitted (incomingFor (streamDirectionSource admitted) state)

incomingGreatestAdvertisedNextSequence ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem (Maybe StreamSequence)
incomingGreatestAdvertisedNextSequence direction state = do
  admitted <- incomingDirection state direction
  Right
    ( incomingFor
        (streamDirectionSource admitted)
        state
    ).greatestAdvertisedNextSequence

-- | Build this owner's exact current offer to one peer without mutating either
-- lazily represented direction.
resumeOfferForPeer ::
  HeraldEpoch ->
  State payload ->
  Either PeerStreamProblem ResumeOffer
resumeOfferForPeer peer state = do
  ensureValid state
  ensurePeerActive state peer
  forward <- outgoingDirection state peer
  outgoing <- outgoingFor forward state
  let reverseDirection = reverseStreamDirection forward
      incoming = incomingFor peer state
  gap <- deriveIncomingGap reverseDirection incoming
  mapResumeProblem
    ( mkResumeOfferWithCompletion
        state.localEpoch
        peer
        outgoing.nextSequence
        incoming.receivedPrefix
        incoming.completion
        gap
    )

currentDispatchBinding ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem (Maybe PeerDispatchBindingGeneration)
currentDispatchBinding direction state = do
  admitted <- outgoingDirectionExact state direction
  (.dispatchBinding) <$> outgoingFor admitted state

currentDispatchTicket ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem (Maybe PeerDispatchTicket)
currentDispatchTicket direction state = do
  admitted <- outgoingDirectionExact state direction
  (.dispatchTicket) <$> outgoingFor admitted state

currentDispatchAttempt ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem (Maybe (PeerDispatchAttempt payload))
currentDispatchAttempt direction state = do
  admitted <- outgoingDirectionExact state direction
  (.dispatchAttempt) <$> outgoingFor admitted state

dispatchAwaitingSequences ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem (Set StreamSequence)
dispatchAwaitingSequences direction state = do
  admitted <- outgoingDirectionExact state direction
  (Map.keysSet . (.awaitingPeerProgress)) <$> outgoingFor admitted state

-- | Opaque property witness.  Direction lists suffice to relate a witness to
-- the owner while narrow read ports expose per-direction facts.
data PeerStreamStateWitness
  = PeerStreamStateWitness
      HeraldEpoch
      [StreamDirection]
      [StreamDirection]
  deriving stock (Eq, Show)

peerStreamStateWitness ::
  State payload ->
  Either PeerStreamInvariantViolation PeerStreamStateWitness
peerStreamStateWitness state = do
  validatePeerStreamState state
  outgoingDirections <-
    traverse
      (directionInvariant . mkStreamDirection state.localEpoch)
      (Map.keys state.outgoing)
  incomingDirections <-
    traverse
      (\peer -> directionInvariant (mkStreamDirection peer state.localEpoch))
      (Map.keys state.incoming)
  Right
    ( PeerStreamStateWitness
        state.localEpoch
        outgoingDirections
        incomingDirections
    )

peerStreamWitnessLocalEpoch :: PeerStreamStateWitness -> HeraldEpoch
peerStreamWitnessLocalEpoch (PeerStreamStateWitness local _ _) = local

peerStreamWitnessOutgoingDirections ::
  PeerStreamStateWitness ->
  [StreamDirection]
peerStreamWitnessOutgoingDirections (PeerStreamStateWitness _ directions _) =
  directions

peerStreamWitnessIncomingDirections ::
  PeerStreamStateWitness ->
  [StreamDirection]
peerStreamWitnessIncomingDirections (PeerStreamStateWitness _ _ directions) =
  directions

validatePeerStreamState ::
  State payload ->
  Either PeerStreamInvariantViolation ()
validatePeerStreamState state = do
  let directions =
        traverse
          (\peer -> mkStreamDirection peer state.localEpoch)
          [peer | (peer, incoming) <- Map.toAscList state.incoming, Set.notMember peer state.retiredPeers || incomingHeadSequence incoming /= Nothing]
      validWork index = WorkIndex.valid index && fmap Set.fromList directions == Right (WorkIndex.registeredKeys index)
  if all (\family -> validWork (markerIndex family state)) markerFamilies
    then Right ()
    else Left PeerStreamInvalidMarkerWork
  mapM_ validateOutgoing (Map.toAscList state.outgoing)
  mapM_ validateIncoming (Map.toAscList state.incoming)
  where
    validateOutgoing (peer, outgoing) = do
      direction <- directionInvariant (mkStreamDirection state.localEpoch peer)
      let lastAllocated = lastAllocatedPrefix outgoing
          activeKeys = Map.keysSet outgoing.activeItems
          high = fromMaybe 0 (receiptRetirementHighWater outgoing.completion)
          completedCount = high - fromIntegral (Set.size (receiptRetirementExceptions outgoing.completion))
          allocatedCount = streamSequenceWord64 outgoing.nextSequence - 1
          partitionValid = if Set.member peer state.retiredPeers then Set.null activeKeys else fromIntegral (Map.size outgoing.activeItems) + completedCount == allocatedCount
          validActive sequenceNumber = sequenceNumber < outgoing.nextSequence && not (receiptIsRetired (streamSequenceWord64 sequenceNumber) outgoing.completion)
      if ( Set.member peer state.retiredPeers
             && ( present outgoing.dispatchBinding
                    || present outgoing.dispatchTicket
                    || present outgoing.dispatchAttempt
                    || present outgoing.lastDispatchOutcome
                )
         )
        || outgoing.completedPrefix > outgoing.receivedPrefix
        || outgoing.receivedPrefix > lastAllocated
        || not partitionValid
        || not (all validActive (Set.toAscList activeKeys))
        || completionHighPrefix outgoing.completion > outgoing.receivedPrefix
        || streamCompletionPrefix outgoing.completion /= outgoing.completedPrefix
        || checkStreamCompletion outgoing.completion /= Right outgoing.completion
        || not (all (validOutgoingItem direction) (Map.toAscList outgoing.activeItems))
        || not (validPeerGap direction outgoing)
        || not (validLastResumeGapClaim direction outgoing)
        then Left (PeerStreamOutgoingInvariant direction)
        else validateDispatch direction outgoing

    present Nothing = False
    present (Just _) = True

    validateIncoming (peer, incoming) = do
      direction <- directionInvariant (mkStreamDirection peer state.localEpoch)
      let received = incoming.receivedPrefix
          completed = incoming.completedPrefix
          records = Map.toAscList incoming.items
          pendingKeys = Set.fromList [streamSequenceWord64 sequenceNumber | (sequenceNumber, record) <- records, record.status == IncomingReceivedPending]
          frontierValid =
            case incoming.greatestAdvertisedNextSequence of
              Nothing -> True
              Just frontier ->
                nextAfterStreamPrefix received <= frontier
                  && all ((< frontier) . fst) records
      if completed > received
        || receiptRetirementHighWater incoming.completion /= fmap streamSequenceWord64 (streamPrefixSequence received)
        || receiptRetirementExceptions incoming.completion /= pendingKeys
        || any ((== IncomingCompleted) . (.status) . snd) records
        || not frontierValid
        || not (all (validIncomingRecord direction received) records)
        || advanceReceivedPrefix incoming /= received
        || streamCompletionPrefix incoming.completion /= completed
        then Left (PeerStreamIncomingInvariant direction)
        else case deriveIncomingGapInvariant direction incoming of
          Left _ -> Left (PeerStreamIncomingInvariant direction)
          Right _ -> Right ()

    validateDispatch direction outgoing
      | validDispatch direction outgoing = Right ()
      | otherwise = Left (PeerStreamDispatchInvariant direction)

-- | Replace the binding generation without replacing its current dispatch
-- attempt. This creates the stale-binding contradiction used by the
-- whole-state validator property; production binding transitions replace both
-- facts atomically.
replaceDispatchBindingForInvariantTest ::
  StreamDirection ->
  PeerDispatchBindingGeneration ->
  State payload ->
  State payload
replaceDispatchBindingForInvariantTest direction replacement state =
  state
    { outgoing =
        Map.adjust
          (\owner -> owner {dispatchBinding = Just replacement})
          (streamDirectionDestination direction)
          state.outgoing
    }

validDispatch :: StreamDirection -> Outgoing payload -> Bool
validDispatch direction outgoing =
  outgoing.nextDispatchTicketGeneration /= 0
    && outgoing.nextDispatchAttemptGeneration /= 0
    && outgoing.associationAttemptFloor > 0
    && outgoing.associationAttemptFloor <= outgoing.nextDispatchAttemptGeneration
    && Map.keysSet outgoing.awaitingPeerProgress
      `Set.isSubsetOf` Map.keysSet outgoing.activeItems
    && all validAwaiting (Map.toAscList outgoing.awaitingPeerProgress)
    && validTicket
    && validAttempt
    && validSettled
    && not (hasTicket && hasAttempt)
    && schedulingIsTotal
  where
    validAwaiting (sequenceNumber, AwaitingPeerReceipt) =
      streamPrefixThrough sequenceNumber <= outgoing.receivedPrefix
        || maybe
          False
          (any ((== sequenceNumber) . fst) . gapSummaryEntries)
          outgoing.lastResumeGapClaim
    validAwaiting (_, AwaitingDispatchWrite binding attempt) =
      maybe True (binding <=) outgoing.dispatchBinding
        && peerDispatchAttemptGenerationWord64 attempt < outgoing.nextDispatchAttemptGeneration
    hasTicket = case outgoing.dispatchTicket of
      Nothing -> False
      Just _ -> True
    hasAttempt = case outgoing.dispatchAttempt of
      Nothing -> False
      Just _ -> True
    eligible = case lowestEligibleItem outgoing of
      Nothing -> False
      Just _ -> True
    bound = case outgoing.dispatchBinding of
      Nothing -> False
      Just _ -> True

    validTicket = case outgoing.dispatchTicket of
      Nothing -> True
      Just ticket ->
        bound
          && peerDispatchTicketDirection ticket == direction
          && peerDispatchTicketGenerationWord64 ticket /= 0
          && peerDispatchTicketGenerationWord64 ticket + 1
            == outgoing.nextDispatchTicketGeneration

    validAttempt = case outgoing.dispatchAttempt of
      Nothing -> True
      Just attempt ->
        ( outgoing.dispatchBinding
            == Just (peerDispatchAttemptBindingGeneration attempt)
            || outgoing.dispatchBinding == Nothing
        )
          && validAttemptIdentity direction outgoing attempt
          && dispatchAttemptIsEligible outgoing attempt
          && peerDispatchAttemptGenerationWord64
            (peerDispatchAttemptGeneration attempt)
            /= 0
          && peerDispatchAttemptGenerationWord64
            (peerDispatchAttemptGeneration attempt)
            + 1
            == outgoing.nextDispatchAttemptGeneration

    validSettled = case outgoing.lastDispatchOutcome of
      Nothing -> True
      Just (attempt, outcome) ->
        not hasAttempt
          && outgoing.dispatchBinding
            == Just (peerDispatchAttemptBindingGeneration attempt)
          && validAttemptIdentity direction outgoing attempt
          && isLatestSettled outgoing attempt
          && settledEligibility outcome attempt

    settledEligibility outcome attempt =
      let sequenceNumber =
            sequencedItemSequence (peerDispatchAttemptItem attempt)
       in case outcome of
            PeerDispatchWritten ->
              Map.member sequenceNumber outgoing.awaitingPeerProgress
            PeerDispatchDeferred ->
              Map.notMember sequenceNumber outgoing.awaitingPeerProgress
            PeerDispatchFailed ->
              Map.notMember sequenceNumber outgoing.awaitingPeerProgress

    schedulingIsTotal =
      case outgoing.dispatchBinding of
        Nothing -> not hasTicket
        Just _
          | eligible -> hasTicket /= hasAttempt
          | otherwise -> not hasTicket && not hasAttempt

validAttemptIdentity ::
  StreamDirection ->
  Outgoing payload ->
  PeerDispatchAttempt other ->
  Bool
validAttemptIdentity direction outgoing attempt =
  peerDispatchAttemptDirection attempt == direction
    && case Map.lookup sequenceNumber outgoing.activeItems of
      Nothing -> False
      Just retained -> sameSequencedItemIdentity retained selected
  where
    selected = peerDispatchAttemptItem attempt
    sequenceNumber = sequencedItemSequence selected

dispatchAttemptIsEligible ::
  Outgoing payload ->
  PeerDispatchAttempt other ->
  Bool
dispatchAttemptIsEligible outgoing attempt =
  Map.notMember sequenceNumber outgoing.awaitingPeerProgress
    && Set.notMember sequenceNumber (peerGapSequences outgoing)
  where
    sequenceNumber = sequencedItemSequence (peerDispatchAttemptItem attempt)

validOutgoingItem ::
  StreamDirection ->
  (StreamSequence, SequencedItem payload) ->
  Bool
validOutgoingItem direction (sequenceNumber, item) =
  sequencedItemDirection item == direction
    && sequencedItemSequence item == sequenceNumber

validPeerGap :: StreamDirection -> Outgoing payload -> Bool
validPeerGap direction outgoing =
  gapSummaryDirection outgoing.peerGapSummary == direction
    && gapSummaryReceivedPrefix outgoing.peerGapSummary == outgoing.receivedPrefix
    && all validEntry (gapSummaryEntries outgoing.peerGapSummary)
  where
    validEntry (sequenceNumber, digest) =
      case Map.lookup sequenceNumber outgoing.activeItems of
        Nothing -> False
        Just item -> sequencedItemDigest item == digest

validLastResumeGapClaim :: StreamDirection -> Outgoing payload -> Bool
validLastResumeGapClaim direction outgoing =
  maybe
    True
    (\claim -> gapSummaryDirection claim == direction && all (\(sequenceNumber, digest) -> maybe False ((== digest) . sequencedItemDigest) (Map.lookup sequenceNumber outgoing.activeItems)) (gapSummaryEntries claim))
    outgoing.lastResumeGapClaim

validIncomingRecord ::
  StreamDirection ->
  StreamPrefix ->
  (StreamSequence, IncomingRecord payload) ->
  Bool
validIncomingRecord direction received (sequenceNumber, record) =
  sequencedItemDirection record.item == direction
    && sequencedItemSequence record.item == sequenceNumber
    && if streamPrefixThrough sequenceNumber <= received
      then record.status /= GapRetained
      else record.status == GapRetained

ensureValid :: State payload -> Either PeerStreamProblem ()
ensureValid state =
  case validatePeerStreamState state of
    Left problem -> Left (PeerStreamInvariantProblem problem)
    Right () -> Right ()

ensurePeerActive :: State payload -> HeraldEpoch -> Either PeerStreamProblem ()
ensurePeerActive state peer
  | Set.member peer state.retiredPeers =
      Left (PeerStreamProtocolProblem (PeerStreamRetiredPeer peer))
  | otherwise = Right ()

outgoingDirection ::
  State payload ->
  HeraldEpoch ->
  Either PeerStreamProblem StreamDirection
outgoingDirection state peer
  | peer == state.localEpoch =
      Left
        (PeerStreamProtocolProblem (PeerStreamSelfDirection peer))
  | otherwise = mapDirectionProblem (mkStreamDirection state.localEpoch peer)

outgoingDirectionExact ::
  State payload ->
  StreamDirection ->
  Either PeerStreamProblem StreamDirection
outgoingDirectionExact state direction
  | streamDirectionSource direction /= state.localEpoch =
      Left
        ( PeerStreamProtocolProblem
            (PeerStreamDirectionNotOutgoing direction state.localEpoch)
        )
  | otherwise = outgoingDirection state (streamDirectionDestination direction)

incomingDirection ::
  State payload ->
  StreamDirection ->
  Either PeerStreamProblem StreamDirection
incomingDirection state direction
  | streamDirectionDestination direction /= state.localEpoch
      || streamDirectionSource direction == state.localEpoch =
      Left
        ( PeerStreamProtocolProblem
            (PeerStreamDirectionNotIncoming direction state.localEpoch)
        )
  | otherwise = Right direction

emptyOutgoing ::
  StreamDirection ->
  Either PeerStreamProblem (Outgoing payload)
emptyOutgoing direction = do
  gap <- mapGapProblem (mkGapSummary direction emptyStreamPrefix [])
  Right
    Outgoing
      { nextSequence = firstStreamSequence,
        receivedPrefix = emptyStreamPrefix,
        completedPrefix = emptyStreamPrefix,
        activeItems = Map.empty,
        completion = mempty,
        peerGapSummary = gap,
        lastResumeGapClaim = Nothing,
        dispatchBinding = Nothing,
        nextDispatchTicketGeneration = 1,
        nextDispatchAttemptGeneration = 1,
        associationAttemptFloor = 1,
        dispatchTicket = Nothing,
        dispatchAttempt = Nothing,
        lastDispatchOutcome = Nothing,
        awaitingPeerProgress = Map.empty
      }

emptyIncoming :: Incoming payload
emptyIncoming =
  Incoming
    { greatestAdvertisedNextSequence = Nothing,
      receivedPrefix = emptyStreamPrefix,
      completedPrefix = emptyStreamPrefix,
      completion = mempty,
      items = Map.empty
    }

outgoingFor ::
  StreamDirection ->
  State payload ->
  Either PeerStreamProblem (Outgoing payload)
outgoingFor direction state =
  case Map.lookup (streamDirectionDestination direction) state.outgoing of
    Just outgoing -> Right outgoing
    Nothing -> emptyOutgoing direction

incomingFor :: HeraldEpoch -> State payload -> Incoming payload
incomingFor peer state = fromMaybe emptyIncoming (Map.lookup peer state.incoming)

contiguousFrom ::
  StreamSequence ->
  Map StreamSequence (IncomingRecord payload) ->
  [SequencedItem payload]
contiguousFrom sequenceNumber items =
  case Map.lookup sequenceNumber items of
    Nothing -> []
    Just record -> record.item : contiguousFrom (nextStreamSequence sequenceNumber) items

advanceReceivedPrefix :: Incoming payload -> StreamPrefix
advanceReceivedPrefix incoming =
  advanceWhile
    (\record -> record.status /= GapRetained)
    incoming.receivedPrefix
    incoming.items

-- | Drop completed payloads after semantic completion has installed its owner
-- effects. The high-water mark plus unresolved exceptions preserves both old
-- duplicate suppression and the contiguous causal barrier without history scans.
compactIncoming :: Incoming payload -> Incoming payload
compactIncoming incoming =
  let received = advanceReceivedPrefix incoming
      retained = Map.filter ((/= IncomingCompleted) . (.status)) incoming.items
      exceptions = Set.fromList [streamSequenceWord64 sequenceNumber | (sequenceNumber, record) <- Map.toAscList retained, record.status == IncomingReceivedPending, streamPrefixThrough sequenceNumber <= received]
      completion = either (error . show) id (receiptRetirement (fmap streamSequenceWord64 (streamPrefixSequence received)) exceptions)
   in incoming {receivedPrefix = received, completedPrefix = streamCompletionPrefix completion, completion, items = retained}

completionHighPrefix :: ReceiptRetirement -> StreamPrefix
completionHighPrefix = streamCompletionPrefix . receiptRetirementPrefix . receiptRetirementHighWater

advanceWhile ::
  (IncomingRecord payload -> Bool) ->
  StreamPrefix ->
  Map StreamSequence (IncomingRecord payload) ->
  StreamPrefix
advanceWhile predicate prefix items =
  let next = nextAfterStreamPrefix prefix
   in case Map.lookup next items of
        Just record
          | predicate record -> advanceWhile predicate (streamPrefixThrough next) items
        _ -> prefix

deriveIncomingGap ::
  StreamDirection ->
  Incoming payload ->
  Either PeerStreamProblem GapSummary
deriveIncomingGap direction incoming =
  case deriveIncomingGapInvariant direction incoming of
    Left problem -> Left (PeerStreamInvariantProblem problem)
    Right gap -> Right gap

deriveIncomingGapInvariant ::
  StreamDirection ->
  Incoming payload ->
  Either PeerStreamInvariantViolation GapSummary
deriveIncomingGapInvariant direction incoming =
  gapInvariant
    ( mkGapSummary
        direction
        incoming.receivedPrefix
        [ (sequenceNumber, sequencedItemDigest record.item)
        | (sequenceNumber, record) <- Map.toAscList incoming.items,
          record.status == GapRetained
        ]
    )

lastAllocatedPrefix :: Outgoing payload -> StreamPrefix
lastAllocatedPrefix outgoing =
  let next = streamSequenceWord64 outgoing.nextSequence
   in if next == 1 then emptyStreamPrefix else streamPrefixThrough (either (error . show) id (mkStreamSequence (next - 1)))

-- The raw receive prefix distinguishes duplicate Resume requests, while only
-- active gap entries can still affect request-delta or awaiting-receipt logic.
retainActiveGapEntries :: Map StreamSequence (SequencedItem payload) -> GapSummary -> Either PeerStreamProblem GapSummary
retainActiveGapEntries active claim =
  mapGapProblem
    ( mkGapSummary
        (gapSummaryDirection claim)
        (gapSummaryReceivedPrefix claim)
        (filter (\(sequenceNumber, _) -> Map.member sequenceNumber active) (gapSummaryEntries claim))
    )

normalizePeerGap ::
  StreamDirection ->
  StreamPrefix ->
  GapSummary ->
  Map StreamSequence (SequencedItem payload) ->
  Either PeerStreamProblem GapSummary
normalizePeerGap direction received oldGap active =
  mapGapProblem
    ( mkGapSummary
        direction
        received
        [ (sequenceNumber, digest)
        | (sequenceNumber, digest) <- gapSummaryEntries oldGap,
          streamPrefixThrough sequenceNumber > received,
          sequenceNumber > nextAfterStreamPrefix received,
          Map.member sequenceNumber active
        ]
    )

normalizeOfferedGap ::
  StreamDirection ->
  StreamPrefix ->
  Map StreamSequence (SequencedItem payload) ->
  GapSummary ->
  Either PeerStreamProblem GapSummary
normalizeOfferedGap direction received active offered =
  mapGapProblem
    ( mkGapSummary
        direction
        received
        [ (sequenceNumber, digest)
        | (sequenceNumber, digest) <- gapSummaryEntries offered,
          streamPrefixThrough sequenceNumber > received,
          sequenceNumber > nextAfterStreamPrefix received,
          Map.member sequenceNumber active
        ]
    )

validateEntry ::
  StreamDirection ->
  Map StreamSequence (SequencedItem payload) ->
  (StreamSequence, PeerItemDigest) ->
  Either PeerStreamProblem StreamSequence
validateEntry direction active (sequenceNumber, claimedDigest) =
  case Map.lookup sequenceNumber active of
    Nothing ->
      Left
        ( PeerStreamProtocolProblem
            (PeerStreamResumeGapEntryNotActive direction sequenceNumber)
        )
    Just item
      | sequencedItemDigest item /= claimedDigest ->
          Left
            ( PeerStreamProtocolProblem
                ( PeerStreamResumeGapDigestMismatch
                    direction
                    sequenceNumber
                    (sequencedItemDigest item)
                    claimedDigest
                )
            )
      | otherwise -> Right sequenceNumber

checkAdvertisedFrontier ::
  StreamDirection ->
  StreamSequence ->
  Incoming payload ->
  Either PeerStreamProblem ()
checkAdvertisedFrontier direction sequenceNumber incoming =
  case incoming.greatestAdvertisedNextSequence of
    Just frontier
      | sequenceNumber >= frontier ->
          Left
            ( PeerStreamProtocolProblem
                ( PeerStreamItemBeyondAdvertisedFrontier
                    direction
                    sequenceNumber
                    frontier
                )
            )
    _ -> Right ()

acknowledgementAhead ::
  StreamDirection ->
  StreamPrefix ->
  StreamPrefix ->
  Either PeerStreamProblem value
acknowledgementAhead direction claimed lastAllocated =
  Left
    ( PeerStreamProtocolProblem
        (PeerStreamAcknowledgementAhead direction claimed lastAllocated)
    )

largestIncomingSequence :: Incoming payload -> Maybe StreamSequence
largestIncomingSequence incoming = fst <$> Map.lookupMax incoming.items

mapDirectionProblem ::
  Either shape value ->
  Either PeerStreamProblem value
mapDirectionProblem =
  either
    (const (Left (PeerStreamInvariantProblem PeerStreamInvalidNominalDirection)))
    Right

mapGapProblem ::
  Either shape value ->
  Either PeerStreamProblem value
mapGapProblem =
  either
    (const (Left (PeerStreamInvariantProblem PeerStreamInvalidGapTranscript)))
    Right

mapResumeProblem ::
  Either shape value ->
  Either PeerStreamProblem value
mapResumeProblem =
  either
    (const (Left (PeerStreamInvariantProblem PeerStreamInvalidResumeTranscript)))
    Right

directionInvariant ::
  Either shape value ->
  Either PeerStreamInvariantViolation value
directionInvariant =
  either (const (Left PeerStreamInvalidNominalDirection)) Right

gapInvariant ::
  Either shape value ->
  Either PeerStreamInvariantViolation value
gapInvariant = either (const (Left PeerStreamInvalidGapTranscript)) Right
