{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Real-EAPP and real-peer-TCP witnesses for the asymmetric Store-loss rules.
-- The schedule constructs two same-sort source Deltas in one SCC and observes
-- only public application results plus immutable outbound and received
-- alignment controls.
module Step14AlignmentLossProperties
  ( tests,
    AlignmentLossFixture (..),
    establishAlignmentLossTopology,
    withStep14Applications,
    scenarioTimeoutMicroseconds,
  )
where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, throwIO)
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (..),
    ApplicationStartupAccess,
    PredefinedAccess,
    predefinedReader,
    predefinedWriter,
    startupAccessProcess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    PrivateNablaId,
    PrivateProcessId,
    PrivateUniqueId,
    SortId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    sortIdBytes,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToDelete, LabelToProcess, LabelToVoid),
    LabelResult (LabelApplied),
  )
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Query
  ( ApplicationProjection (..),
    ApplicationQuery (..),
    ApplicationQueryLiteral (QueryUniqueId),
    ApplicationQueryPredicate (QueryAlways, QueryCompare),
  )
import Eclips.Application.Types.Result (RegularCallResult (..))
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationScalarComparison (ScalarEqual),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (ProcessLabel),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten, WriteAccepted),
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    StoreIncarnationId,
    storeIncarnationIdBytes,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (RejectPeerConnection),
    PeerProtocolDisposition (ClosePeerControlProtocol),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (PeerInput),
    PeerControl (PeerPlacementUpdate),
    inputBody,
  )
import Eclips.Herald.Peer.RPC (projectPeerControl)
import Eclips.Herald.Placement
  ( DeltaRoute,
    PlacementSequence,
    PlacementUpdate (FullPlacementSnapshot),
    deltaRouteDelta,
    deltaRouteSortId,
    deltaRouteStoreIncarnation,
    placementSnapshotOwner,
    placementSnapshotRoutes,
    placementSnapshotSequence,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeEventOrdinal,
    RuntimeTraceEvent (KernelEvent),
  )
import Eclips.Protocol.Application.Types (applicationClientNonce)
import Eclips.Protocol.Peer.Types qualified as Peer
import PeerEvidence (data SemanticPeerAlignmentControl, data SemanticPeerControlReceived, data SemanticSendPeerControl)
import Step14Fixtures
  ( Step14Deployment,
    Step14Herald,
    awaitStep14PeerMesh,
    snapshotStep14HeraldTrace,
    step14ApplicationLivenessConfiguration,
    step14H1,
    step14H2,
    step14H3,
    step14H4,
    step14PApplicationAttachment,
    step14PApplicationEndpoint,
    step14QApplicationAttachment,
    step14QApplicationEndpoint,
    withStep14Deployment,
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
    "Step-14 alignment Store loss"
    [ testCase
        "destination Delta loss cancels and tombstones its exact alignment plan"
        caseDestinationDeltaLoss,
      testCase
        "current source member loss replaces the plan and preserves surviving-path history"
        caseCurrentSourceMemberPassivation
    ]

caseDestinationDeltaLoss :: Assertion
caseDestinationDeltaLoss =
  withinScenario "destination-Delta-loss" $ \deployment pApplication pStartup qApplication qStartup -> do
    fixture <-
      establishAlignmentLossTopology
        deployment
        pApplication
        pStartup
        qApplication
        qStartup
    let topology = fixture.lossTopology
        active = fixture.lossActiveSubscribe
        subscription = subscribeId active
        obligation = subscribeObligationId active
        destinationStores = subscribeDestinationStores active
    preDeletionControls <- outboundAlignmentControls (step14H2 deployment)
    let preDeletionSubscribePositions =
          Set.fromList
            [ position
            | (position, Peer.AlignmentSubscribeRequestedDto _) <- preDeletionControls
            ]

    deletion <-
      EAPP.label
        qApplication
        (asPrivateObjectId topology.lossQDestinationObject)
        ((ProcessLabel (startupAccessProcess qStartup), 0))
        LabelToDelete
    expectLabelResult "Q destination-Delta deletion" deletion
    awaitPlacementWithdrawal
      (step14H1 deployment)
      (storeIncarnationIdBytes fixture.lossDestinationStore)
    (_, cancellation) <-
      awaitExactCancellation
        (step14H2 deployment)
        subscription
        Peer.AlignmentDestinationIncarnationLostDto
    _ <-
      awaitInboundCancellation
        (step14H1 deployment)
        subscription
        Peer.AlignmentDestinationIncarnationLostDto
    assertExactCancellationCount
      deployment
      subscription
      Peer.AlignmentDestinationIncarnationLostDto

    awaitCarrierAbsent
      qApplication
      topology.lossQDeltaCarrierReader
      topology.lossQDestinationObject

    postLoss <- publishMessage pApplication topology destinationLossMessage
    expectWriteAccepted "post-destination-loss publication" postLoss
    fence <-
      EAPP.label
        pApplication
        (asPrivateObjectId topology.lossSequencingObject)
        ((ProcessLabel topology.lossPProcess, 0))
        (LabelToProcess topology.lossPProcess)
    expectLabelResult "post-destination-loss causal fence" fence
    afterFence <- outboundAlignmentControls (step14H2 deployment)
    let postDeletionSubscribes =
          [ subscribe
          | (position, Peer.AlignmentSubscribeRequestedDto subscribe) <- afterFence,
            Set.notMember position preDeletionSubscribePositions
          ]
        oldDestinations = destinationStoreKeys destinationStores
    assertBool
      "destination loss creates no replacement that reuses the old obligation"
      ( all
          ((/= obligation) . subscribeObligationId)
          postDeletionSubscribes
      )
    assertBool
      "destination loss creates no replacement that names the inactive Store"
      ( all
          ( Set.disjoint oldDestinations
              . destinationIncarnations
          )
          postDeletionSubscribes
      )
    awaitCarrierAbsent
      qApplication
      topology.lossQDeltaCarrierReader
      topology.lossQDestinationObject
    -- Keep the full cancellation value live in this assertion: a trace match
    -- must be the old subscription, not merely another cancellation reason.
    assertEqual
      "the retained destination cancellation names the old subscription"
      subscription
      (cancelSubscriptionId cancellation)

caseCurrentSourceMemberPassivation :: Assertion
caseCurrentSourceMemberPassivation =
  withinScenario "current-source-member-passivation" $ \deployment pApplication pStartup qApplication qStartup -> do
    fixture <-
      establishAlignmentLossTopology
        deployment
        pApplication
        pStartup
        qApplication
        qStartup
    let topology = fixture.lossTopology
        active = fixture.lossActiveSubscribe
        oldSubscription = subscribeId active
        oldObligation = subscribeObligationId active
        oldDestinations = subscribeDestinationStores active
        oldSource = subscribeSourceStore active
    (selectedObject, survivingStore) <-
      selectedSourceObjectAndAlternative fixture oldSource
    beforePassivation <- outboundAlignmentControls (step14H2 deployment)
    let previousSubscriptions = Set.fromList [subscribeId subscribe | (_, Peer.AlignmentSubscribeRequestedDto subscribe) <- beforePassivation]

    passivation <-
      EAPP.label
        pApplication
        (asPrivateObjectId selectedObject)
        ((ProcessLabel (startupAccessProcess pStartup), 0))
        LabelToVoid
    expectLabelResult "selected source-Delta passivation" passivation
    awaitPlacementWithdrawal
      (step14H2 deployment)
      (Peer.storeIncarnationClaimBytes oldSource)
    (cancelPosition, _) <-
      awaitExactCancellation
        (step14H1 deployment)
        oldSubscription
        Peer.AlignmentSourceIncarnationLostDto
    _ <-
      awaitInboundCancellation
        (step14H2 deployment)
        oldSubscription
        Peer.AlignmentSourceIncarnationLostDto
    _ <-
      awaitExactCancellation
        (step14H2 deployment)
        oldSubscription
        Peer.AlignmentDestinationIncarnationLostDto
    _ <-
      awaitInboundCancellation
        (step14H1 deployment)
        oldSubscription
        Peer.AlignmentDestinationIncarnationLostDto
    assertOwnedCancellation (step14H1 deployment) oldSubscription Peer.AlignmentSourceIncarnationLostDto
    assertOwnedCancellation (step14H2 deployment) oldSubscription Peer.AlignmentDestinationIncarnationLostDto

    -- Losing either SCC member breaks one side of the original producer-to-Q
    -- path. Establish an ordinary path through the survivor after proving that
    -- the old current plan was tombstoned; this must create fresh plan work.
    pEdgeAccess <- accessFor "P" EdgeRole pStartup
    let survivingObject =
          if selectedObject == topology.lossSourceAObject
            then topology.lossSourceBObject
            else topology.lossSourceAObject
    _ <-
      publishEdge
        "producer-to-survivor recovery path"
        pApplication
        (predefinedWriter pEdgeAccess)
        (startupAccessProcess pStartup)
        topology.lossProducerNabla
        survivingObject
    _ <-
      publishEdge
        "survivor-to-Q recovery path"
        pApplication
        (predefinedWriter pEdgeAccess)
        (startupAccessProcess pStartup)
        survivingObject
        topology.lossLocalizedQDestinationObject

    replacement <-
      awaitReplacementPlanSubscription
        deployment
        oldSubscription
        oldObligation
        oldDestinations
        survivingStore
        previousSubscriptions
    assertBool
      "source-loss replacement has a fresh subscription identity"
      (subscribeId replacement /= oldSubscription)
    assertEqual
      "current-member loss never reuses the tombstoned obligation"
      False
      (subscribeObligationId replacement == oldObligation)
    assertEqual
      "source-loss replacement preserves the exact destination Stores"
      oldDestinations
      (subscribeDestinationStores replacement)
    assertEqual
      "source-loss replacement chooses the other SCC member"
      (storeIncarnationIdBytes survivingStore)
      (subscribeSourceStoreKey replacement)

    assertBool
      "current-member loss selects a new destination generation"
      (subscribeDestinationGeneration replacement /= subscribeDestinationGeneration active)
    oldCut <- awaitGenerationCut deployment (subscribeDestinationGeneration active)
    newCut <- awaitGenerationCut deployment (subscribeDestinationGeneration replacement)
    sourceCut <- awaitGenerationCut deployment (subscribeSourceGeneration replacement)
    let Peer.AlignmentCutDto _ _ _ _ oldTopology oldPlacement _ _ _ = oldCut
        Peer.AlignmentCutDto _ _ _ _ newTopology newPlacement _ _ _ = newCut
        Peer.AlignmentCutDto _ _ _ _ sourceTopology sourcePlacement sourceMembers _ _ = sourceCut
        sourceStores = Set.fromList [Peer.storeIncarnationClaimBytes store | Peer.AlignmentMemberDto _ store _ <- NonEmpty.toList sourceMembers]
    assertBool "replacement uses a successor topology" (newTopology /= oldTopology)
    assertBool "replacement uses the withdrawal placement vector" (newPlacement /= oldPlacement)
    assertEqual "replacement source and destination share one topology cut" newTopology sourceTopology
    assertEqual "replacement source and destination share one placement vector" newPlacement sourcePlacement
    assertBool "replacement source class excludes the inactive Store" (Set.notMember (Peer.storeIncarnationClaimBytes oldSource) sourceStores)
    assertBool "replacement source class contains the surviving Store" (Set.member (storeIncarnationIdBytes survivingStore) sourceStores)

    replacementReceived <-
      awaitInboundSubscribe (step14H1 deployment) replacement
    assertBool
      "the source receives the replacement only after sending the old cancellation"
      (tracePositionOrdinal cancelPosition < replacementReceived)

    awaitAlignmentLive deployment (subscribeId replacement)
    _ <- awaitInboundLive (step14H2 deployment) (subscribeId replacement)
    awaitMessage
      "predecessor history survives plan replacement at Q"
      qApplication
      topology.lossQMessageReader
      predecessorMessage
    oldChangesBefore <-
      matchingInboundChangeCount
        (step14H2 deployment)
        oldSubscription
    subsequent <- publishMessage pApplication topology sourceLossMessage
    expectWriteAccepted "post-source-passivation publication" subsequent
    -- The recovered current route may carry later publications directly;
    -- bootstrap transfer completion does not prescribe that delivery lane.
    awaitMessage
      "replacement history at Q"
      qApplication
      topology.lossQMessageReader
      sourceLossMessage
    oldChangesAfter <-
      matchingInboundChangeCount
        (step14H2 deployment)
        oldSubscription
    assertEqual
      "replacement publication emits no change on the cancelled source subscription"
      oldChangesBefore
      oldChangesAfter

withinScenario ::
  String ->
  ( Step14Deployment ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    IO ()
  ) ->
  Assertion
withinScenario name scenario = do
  completed <-
    timeout scenarioTimeoutMicroseconds
      $ withStep14Deployment
      $ \deployment -> do
        awaitStep14PeerMesh deployment
        withStep14Applications deployment (scenario deployment)
  unless (completed == Just ())
    $ assertFailure ("Step-14 " <> name <> " schedule exceeded its failure bound")

withStep14Applications ::
  Step14Deployment ->
  ( EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    IO result
  ) ->
  IO result
withStep14Applications deployment use = do
  pResult <-
    EAPP.withApplication
      ( EAPP.applicationConfiguration
          (step14PApplicationEndpoint deployment)
          step14PApplicationAttachment
          (applicationClientNonce 14_901)
          step14ApplicationLivenessConfiguration
      )
      ( \pApplication pStartup -> do
          qResult <-
            EAPP.withApplication
              ( EAPP.applicationConfiguration
                  (step14QApplicationEndpoint deployment)
                  step14QApplicationAttachment
                  (applicationClientNonce 14_902)
                  step14ApplicationLivenessConfiguration
              )
              (use pApplication pStartup)
          requireApplicationResult "Q" qResult
      )
  requireApplicationResult "P" pResult

requireApplicationResult ::
  String ->
  Either EAPP.ApplicationRuntimeFailure result ->
  IO result
requireApplicationResult name = \case
  Left failure ->
    assertFailure (name <> " application runtime failed: " <> show failure)
  Right result -> pure result

data AlignmentLossTopology = AlignmentLossTopology
  { lossMessageSort :: SortId,
    lossSequencingObject :: PrivateUniqueId,
    lossProducerNabla :: PrivateUniqueId,
    lossSourceAObject :: PrivateUniqueId,
    lossSourceBObject :: PrivateUniqueId,
    lossQDestinationObject :: PrivateUniqueId,
    lossLocalizedQDestinationObject :: PrivateUniqueId,
    lossProducerWriter :: PrivateNablaId,
    lossQMessageReader :: PrivateDeltaId,
    lossQDeltaCarrierReader :: PrivateDeltaId,
    lossPProcess :: PrivateProcessId
  }
  deriving stock (Eq, Show)

data AlignmentLossFixture = AlignmentLossFixture
  { lossTopology :: AlignmentLossTopology,
    lossSourceAStore :: StoreIncarnationId,
    lossSourceBStore :: StoreIncarnationId,
    lossDestinationStore :: StoreIncarnationId,
    lossActiveSubscribe :: Peer.AlignmentSubscribeDto
  }
  deriving stock (Eq, Show)

establishAlignmentLossTopology ::
  Step14Deployment ->
  EAPP.Application ->
  ApplicationStartupAccess ->
  EAPP.Application ->
  ApplicationStartupAccess ->
  IO AlignmentLossFixture
establishAlignmentLossTopology deployment pApplication pStartup qApplication qStartup = do
  pSortAccess <- accessFor "P" SortDefinitionRole pStartup
  qSortAccess <- accessFor "Q" SortDefinitionRole qStartup
  pNablaAccess <- accessFor "P" NablaRole pStartup
  pDeltaAccess <- accessFor "P" DeltaRole pStartup
  qDeltaAccess <- accessFor "Q" DeltaRole qStartup
  pEdgeAccess <- accessFor "P" EdgeRole pStartup

  messageSort <-
    expectSortDefinition "alignment message sort"
      =<< EAPP.write
        pApplication
        (predefinedWriter pSortAccess)
        (PublishValue (SortDefinitionValue (DeclaredSortDefinition messageDescriptor Nothing)))
  awaitSortDefinition qApplication (predefinedReader qSortAccess) messageSort

  sourceA <-
    publishMappedSourceDelta
      "source Delta A"
      deployment
      pApplication
      pStartup
      pDeltaAccess
      messageSort
  let (sourceAObject, sourceAStore) = sourceA
  sourceB <-
    publishMappedSourceDelta
      "source Delta B"
      deployment
      pApplication
      pStartup
      pDeltaAccess
      messageSort
  let (sourceBObject, sourceBStore) = sourceB
  assertBool
    "the two public source Deltas have distinct Store incarnations"
    (sourceAStore /= sourceBStore)
  producer <-
    reserveAndPublishCarrier
      "alignment producer Nabla"
      pApplication
      (predefinedWriter pNablaAccess)
      (nablaCarrier (startupAccessProcess pStartup) messageSort sourceAObject)

  _ <-
    publishEdge
      "A-to-B SCC Edge"
      pApplication
      (predefinedWriter pEdgeAccess)
      (startupAccessProcess pStartup)
      sourceAObject
      sourceBObject
  _ <-
    publishEdge
      "B-to-A SCC Edge"
      pApplication
      (predefinedWriter pEdgeAccess)
      (startupAccessProcess pStartup)
      sourceBObject
      sourceAObject
  _ <-
    publishEdge
      "producer-to-A Edge"
      pApplication
      (predefinedWriter pEdgeAccess)
      (startupAccessProcess pStartup)
      producer
      sourceAObject
  predecessor <-
    EAPP.write
      pApplication
      (asPrivateNablaId producer)
      (PublishValue (messageValue predecessorMessage))
  expectWriteAccepted "pre-merge predecessor publication" predecessor
  -- Make B state-bearing before adding the B-to-Q relation.  Alignment is a
  -- historical topology-change protocol; a relation installed before its
  -- source has history needs no backfill and therefore has no subscription to
  -- exercise in this loss schedule.
  awaitMessage
    "predecessor history at source B"
    pApplication
    (asPrivateDeltaId sourceBObject)
    predecessorMessage
  knownLocalizedDestinations <-
    observedLocalizedDeltaCarriers
      pApplication
      (predefinedReader pDeltaAccess)
      messageSort
  qDestination <-
    do
      knownStores <-
        observedMessageStoreIncarnations
          (step14H1 deployment)
          messageSort
      object <-
        reserveAndPublishCarrier
          "alignment Q destination Delta"
          qApplication
          (predefinedWriter qDeltaAccess)
          (deltaCarrier (startupAccessProcess qStartup) messageSort)
      qStore <-
        awaitFreshMessageStore
          "Q destination Delta"
          (step14H1 deployment)
          messageSort
          knownStores
      pure (object, qStore)
  let (qDestinationObject, qDestinationStore) = qDestination
  (localizedQDestination, _) <-
    awaitFreshLocalizedDeltaCarrier
      pApplication
      (predefinedReader pDeltaAccess)
      messageSort
      knownLocalizedDestinations
  _ <-
    publishEdge
      "B-to-Q Edge"
      pApplication
      (predefinedWriter pEdgeAccess)
      (startupAccessProcess pStartup)
      sourceBObject
      localizedQDestination
  active <-
    awaitCurrentSubscribe
      deployment
      messageSort
      qDestinationStore
      (Set.fromList [sourceAStore, sourceBStore])
  _ <- awaitInboundSubscribe (step14H1 deployment) active
  _ <- awaitInboundLive (step14H2 deployment) (subscribeId active)
  awaitMessage
    "aligned predecessor history at Q"
    qApplication
    (asPrivateDeltaId qDestinationObject)
    predecessorMessage

  let topology =
        AlignmentLossTopology
          { lossMessageSort = messageSort,
            lossSequencingObject = sourceAObject,
            lossProducerNabla = producer,
            lossSourceAObject = sourceAObject,
            lossSourceBObject = sourceBObject,
            lossQDestinationObject = qDestinationObject,
            lossLocalizedQDestinationObject = localizedQDestination,
            lossProducerWriter = asPrivateNablaId producer,
            lossQMessageReader = asPrivateDeltaId qDestinationObject,
            lossQDeltaCarrierReader = predefinedReader qDeltaAccess,
            lossPProcess = startupAccessProcess pStartup
          }
  pure
    AlignmentLossFixture
      { lossTopology = topology,
        lossSourceAStore = sourceAStore,
        lossSourceBStore = sourceBStore,
        lossDestinationStore = qDestinationStore,
        lossActiveSubscribe = active
      }

selectedSourceObjectAndAlternative ::
  AlignmentLossFixture ->
  Peer.StoreIncarnationClaim ->
  IO (PrivateUniqueId, StoreIncarnationId)
selectedSourceObjectAndAlternative fixture selected
  | Peer.storeIncarnationClaimBytes selected
      == storeIncarnationIdBytes fixture.lossSourceAStore =
      pure
        ( fixture.lossTopology.lossSourceAObject,
          fixture.lossSourceBStore
        )
  | Peer.storeIncarnationClaimBytes selected
      == storeIncarnationIdBytes fixture.lossSourceBStore =
      pure
        ( fixture.lossTopology.lossSourceBObject,
          fixture.lossSourceAStore
        )
  | otherwise =
      assertFailure
        ( "the SCC subscription selected an unmapped source Store: "
            <> show selected
        )

data TracePosition = TracePosition RuntimeEventOrdinal Int
  deriving stock (Eq, Ord, Show)

tracePositionOrdinal :: TracePosition -> RuntimeEventOrdinal
tracePositionOrdinal (TracePosition ordinal _) = ordinal

outboundAlignmentControls ::
  Step14Herald ->
  IO [(TracePosition, Peer.AlignmentControlDto)]
outboundAlignmentControls herald = do
  events <- snapshotStep14HeraldTrace herald
  pure
    [ (TracePosition ordinal memberIndex, alignmentControl)
    | KernelEvent ordinal (KernelTraceStepped _ _ _ (Right effects)) <- events,
      (memberIndex, effect) <- zip [0 ..] (effectBatchMembers effects),
      SemanticSendPeerControl _ control@(SemanticPeerAlignmentControl _) <- [effect],
      Right
        ( Peer.PeerControlEnvelope
            _
            (Peer.AlignmentControlDto alignmentControl)
          ) <-
        [projectPeerControl control]
    ]

outboundPeerControls :: Step14Herald -> IO [Peer.PeerControlDto]
outboundPeerControls herald = do
  events <- snapshotStep14HeraldTrace herald
  pure
    [ controlDto
    | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
      SemanticSendPeerControl _ control <- effectBatchMembers effects,
      Right (Peer.PeerControlEnvelope _ controlDto) <- [projectPeerControl control]
    ]

inboundAlignmentControls :: Step14Herald -> IO [Peer.AlignmentControlDto]
inboundAlignmentControls herald = fmap (fmap snd) (inboundAlignmentControlEvents herald)

inboundAlignmentControlEvents ::
  Step14Herald ->
  IO [(RuntimeEventOrdinal, Peer.AlignmentControlDto)]
inboundAlignmentControlEvents herald = do
  events <- snapshotStep14HeraldTrace herald
  pure
    [ (ordinal, alignmentControl)
    | KernelEvent ordinal (KernelTraceStepped _ _ input _) <- events,
      PeerInput
        (SemanticPeerControlReceived _ control@(SemanticPeerAlignmentControl _)) <-
        [inputBody input],
      Right
        ( Peer.PeerControlEnvelope
            _
            (Peer.AlignmentControlDto alignmentControl)
          ) <-
        [projectPeerControl control]
    ]

inboundPeerControls :: Step14Herald -> IO [Peer.PeerControlDto]
inboundPeerControls herald = do
  events <- snapshotStep14HeraldTrace herald
  pure
    [ controlDto
    | KernelEvent _ (KernelTraceStepped _ _ input _) <- events,
      PeerInput (SemanticPeerControlReceived _ control) <- [inputBody input],
      Right (Peer.PeerControlEnvelope _ controlDto) <- [projectPeerControl control]
    ]

peerControlTopologyCut :: Peer.PeerControlDto -> Maybe Peer.TopologyCutClaim
peerControlTopologyCut = \case
  Peer.TopologyCutAnnouncedDto (Peer.TopologyCutAnnounceDto _ cut _) -> Just cut
  Peer.TopologyCutAcceptedDto (Peer.TopologyCutAcceptanceDto cut _ _ _) -> Just cut
  Peer.TopologyCutEstablishedControlDto (Peer.TopologyCutEstablishedDto cut _ _) -> Just cut
  Peer.TopologyCutEstablishedAcknowledgedDto (Peer.TopologyCutEstablishedAckDto cut _ _) -> Just cut
  _ -> Nothing

peerControlTag :: Peer.PeerControlDto -> String
peerControlTag = \case
  Peer.TopologyCutAnnouncedDto {} -> "topology-announced"
  Peer.TopologyCutAcceptedDto {} -> "topology-accepted"
  Peer.TopologyCutEstablishedControlDto {} -> "topology-established"
  Peer.TopologyCutEstablishedAcknowledgedDto {} -> "topology-established-ack"
  _ -> "other"

placementUpdateCoordinate ::
  Peer.PlacementUpdateDto ->
  (Peer.HeraldEpochClaim, Peer.PositivePlacementSequenceDto)
placementUpdateCoordinate (Peer.FullPlacementSnapshotDto snapshot) =
  ( Peer.placementSnapshotOwner snapshot,
    Peer.placementSnapshotSequence snapshot
  )

alignmentControlHistogram :: [Peer.AlignmentControlDto] -> [(String, Int)]
alignmentControlHistogram =
  Map.toAscList
    . Map.fromListWith (+)
    . fmap (\control -> (alignmentControlTag control, 1))

alignmentControlTag :: Peer.AlignmentControlDto -> String
alignmentControlTag = \case
  Peer.AlignmentPlanAnnouncedDto {} -> "plan-announced"
  Peer.AlignmentCutAnnouncedDto {} -> "cut-announced"
  Peer.AlignmentHistoricalCertificateAdvertisedDto {} -> "certificate"
  Peer.AlignmentSubscribeRequestedDto {} -> "subscribe"
  Peer.AlignmentSnapshotStartedDto {} -> "snapshot-start"
  Peer.AlignmentSnapshotChunkTransferredDto {} -> "snapshot-chunk"
  Peer.AlignmentSnapshotEndedDto {} -> "snapshot-end"
  Peer.AlignmentChangeTransferredDto {} -> "change"
  Peer.AlignmentLiveAdvertisedDto {} -> "live"
  Peer.AlignmentAcknowledgedDto {} -> "ack"
  Peer.AlignmentCancelledDto {} -> "cancel"
  Peer.AlignmentProbeMarkerDto {} -> "probe-marker"

-- A carried plan references existing births; only its Created bindings carry
-- definitions. Legacy direct cut messages remain useful in boundary fixtures.
announcedGenerationCuts :: Peer.AlignmentControlDto -> [Peer.AlignmentCutAnnounceDto]
announcedGenerationCuts (Peer.AlignmentCutAnnouncedDto announce) = [announce]
announcedGenerationCuts (Peer.AlignmentPlanAnnouncedDto (Peer.AlignmentPlanAnnounceDto _ _ _ _ _ _ definitions)) = definitions
announcedGenerationCuts _ = []

deploymentAlignmentControls ::
  Step14Deployment ->
  IO [Peer.AlignmentControlDto]
deploymentAlignmentControls deployment =
  concatMap (fmap snd)
    <$> traverse
      outboundAlignmentControls
      [ step14H1 deployment,
        step14H2 deployment,
        step14H3 deployment,
        step14H4 deployment
      ]

publishMappedSourceDelta ::
  String ->
  Step14Deployment ->
  EAPP.Application ->
  ApplicationStartupAccess ->
  PredefinedAccess ->
  SortId ->
  IO (PrivateUniqueId, StoreIncarnationId)
publishMappedSourceDelta context deployment application startup access sortId = do
  knownStores <-
    observedMessageStoreIncarnations
      (step14H2 deployment)
      sortId
  object <-
    reserveAndPublishCarrier
      context
      application
      (predefinedWriter access)
      (deltaCarrier (startupAccessProcess startup) sortId)
  store <-
    awaitFreshMessageStore
      context
      (step14H2 deployment)
      sortId
      knownStores
  pure (object, store)

observedMessageStoreIncarnations ::
  Step14Herald ->
  SortId ->
  IO (Set StoreIncarnationId)
observedMessageStoreIncarnations herald sortId = do
  events <- snapshotStep14HeraldTrace herald
  pure
    ( Set.fromList
        [ deltaRouteStoreIncarnation route
        | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
          PeerInput
            (SemanticPeerControlReceived _ (PeerPlacementUpdate update)) <-
            [inputBody input],
          route <- placementUpdateUpserts update,
          deltaRouteSortId route == sortId
        ]
    )

placementUpdateUpserts :: PlacementUpdate -> [DeltaRoute]
placementUpdateUpserts (FullPlacementSnapshot snapshot) =
  placementSnapshotRoutes snapshot

awaitPlacementWithdrawal ::
  Step14Herald ->
  ByteString ->
  IO ()
awaitPlacementWithdrawal herald expectedStore = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  unless (observed == Just ())
    $ assertFailure
      ( "no exact placement withdrawal appeared for Store "
          <> show expectedStore
      )
  where
    loop = do
      events <- snapshotStep14HeraldTrace herald
      let withdrawn =
            fmap
              storeIncarnationIdBytes
              ( placementUpdateWithdrawals
                  [ update
                  | KernelEvent _ (KernelTraceStepped _ _ input (Right _)) <- events,
                    PeerInput
                      (SemanticPeerControlReceived _ (PeerPlacementUpdate update)) <-
                      [inputBody input]
                  ]
              )
      if expectedStore `elem` withdrawn
        then pure ()
        else threadDelay 10_000 >> loop

-- A complete placement snapshot withdraws every previously advertised Delta
-- route it omits.
placementUpdateWithdrawals :: [PlacementUpdate] -> [StoreIncarnationId]
placementUpdateWithdrawals = snd . foldl advance (Map.empty, [])
  where
    advance (projections, withdrawn) update =
      let owner = placementUpdateOwner update
          sequenceNumber = placementUpdateSequence update
       in case Map.lookup owner projections of
            Just (incumbentSequence, _)
              | incumbentSequence >= sequenceNumber ->
                  (projections, withdrawn)
            incumbent ->
              let previous = maybe Map.empty snd incumbent
                  (current, newlyWithdrawn) = applySnapshot previous update
               in ( Map.insert owner (sequenceNumber, current) projections,
                    withdrawn <> newlyWithdrawn
                  )

    applySnapshot previous (FullPlacementSnapshot snapshot) =
      let current = routeProjection (placementSnapshotRoutes snapshot)
          removed =
            [ store
            | (delta, store) <- Map.toAscList previous,
              Map.lookup delta current /= Just store
            ]
       in (current, removed)

    routeProjection =
      Map.fromList
        . fmap (\route -> (deltaRouteDelta route, deltaRouteStoreIncarnation route))

placementUpdateOwner :: PlacementUpdate -> HeraldEpoch
placementUpdateOwner (FullPlacementSnapshot snapshot) = placementSnapshotOwner snapshot

placementUpdateSequence :: PlacementUpdate -> PlacementSequence
placementUpdateSequence (FullPlacementSnapshot snapshot) =
  placementSnapshotSequence snapshot

awaitFreshMessageStore ::
  String ->
  Step14Herald ->
  SortId ->
  Set StoreIncarnationId ->
  IO StoreIncarnationId
awaitFreshMessageStore context herald sortId knownStores = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  case observed of
    Just store -> pure store
    Nothing ->
      assertFailure
        ( context
            <> ": no fresh Store appeared in the remote placement trace"
        )
  where
    loop = do
      stores <- observedMessageStoreIncarnations herald sortId
      case Set.toList (stores `Set.difference` knownStores) of
        [fresh] -> pure fresh
        [] -> threadDelay 10_000 >> loop
        fresh ->
          assertFailure
            ( context
                <> ": more than one fresh Store appeared: "
                <> show fresh
            )

awaitCurrentSubscribe ::
  Step14Deployment ->
  SortId ->
  StoreIncarnationId ->
  Set StoreIncarnationId ->
  IO Peer.AlignmentSubscribeDto
awaitCurrentSubscribe deployment expectedSort destination sourceStores = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  case observed of
    Just subscribe -> pure subscribe
    Nothing -> do
      namedControls <-
        traverse
          ( \(name, herald) -> do
              controls <- outboundAlignmentControls herald
              pure (name, controls)
          )
          [ (("H1" :: String), step14H1 deployment),
            ("H2", step14H2 deployment),
            ("H3", step14H3 deployment),
            ("H4", step14H4 deployment)
          ]
      namedInbound <-
        traverse
          ( \(name, herald) -> do
              controls <- inboundAlignmentControls herald
              pure (name, controls)
          )
          [ (("H1" :: String), step14H1 deployment),
            ("H2", step14H2 deployment),
            ("H3", step14H3 deployment),
            ("H4", step14H4 deployment)
          ]
      namedOutboundPeer <-
        traverse
          ( \(name, herald) -> do
              controls <- outboundPeerControls herald
              pure (name, controls)
          )
          [ (("H1" :: String), step14H1 deployment),
            ("H2", step14H2 deployment),
            ("H3", step14H3 deployment),
            ("H4", step14H4 deployment)
          ]
      namedInboundPeer <-
        traverse
          ( \(name, herald) -> do
              controls <- inboundPeerControls herald
              pure (name, controls)
          )
          [ (("H1" :: String), step14H1 deployment),
            ("H2", step14H2 deployment),
            ("H3", step14H3 deployment),
            ("H4", step14H4 deployment)
          ]
      namedFaults <-
        traverse
          ( \(name, herald) -> do
              events <- snapshotStep14HeraldTrace herald
              pure
                ( name,
                  [ show fault
                  | KernelEvent _ (KernelTraceStepped _ _ _ (Left fault)) <- events
                  ]
                )
          )
          [ (("H1" :: String), step14H1 deployment),
            ("H2", step14H2 deployment),
            ("H3", step14H3 deployment),
            ("H4", step14H4 deployment)
          ]
      namedProtocolCloses <-
        traverse
          ( \(name, herald) -> do
              events <- snapshotStep14HeraldTrace herald
              pure
                ( name,
                  length
                    [ ()
                    | KernelEvent _ (KernelTraceStepped _ _ _ (Right effects)) <- events,
                      RejectPeerConnection _ ClosePeerControlProtocol <- effectBatchMembers effects
                    ]
                )
          )
          [ (("H1" :: String), step14H1 deployment),
            ("H2", step14H2 deployment),
            ("H3", step14H3 deployment),
            ("H4", step14H4 deployment)
          ]
      allControls <- deploymentAlignmentControls deployment
      let liveSubscriptions = observedLiveSubscriptions allControls
          cancelledSubscriptions = observedCancelledSubscriptions allControls
          relevant =
            [ ( heraldName,
                subscribeId subscribe,
                subscribeSourceStoreKey subscribe,
                destinationIncarnations subscribe,
                subscribeSourceProof subscribe,
                Set.member (subscribeId subscribe) liveSubscriptions,
                Set.member (subscribeId subscribe) cancelledSubscriptions
              )
            | (heraldName, controls) <- namedControls,
              (_, Peer.AlignmentSubscribeRequestedDto subscribe) <- controls,
              subscribeSortId subscribe == expectedSort
            ]
          exactSortCuts =
            Set.toAscList
              ( Set.fromList
                  [ ( heraldName,
                      generation,
                      [ Peer.storeIncarnationClaimBytes store
                      | Peer.AlignmentMemberDto _ store _ <- NonEmpty.toList members
                      ]
                    )
                  | (heraldName, controls) <- namedControls,
                    (_, control) <- controls,
                    announce <- announcedGenerationCuts control,
                    Peer.AlignmentCutAnnounceDto generation cut <- [announce],
                    Peer.AlignmentCutDto _ _ sortId _ _ _ members _ _ <- [cut],
                    sortId == expectedSort
                  ]
              )
          exactSortGenerations =
            Set.fromList
              [ generation
              | (_, generation, _) <- exactSortCuts
              ]
          exactTopologyCuts =
            Set.fromList
              ( [ topologyCut
                | (_, controls) <- namedOutboundPeer,
                  Peer.AlignmentEvidenceDeliveredDto _ (Peer.AlignmentCutAcceptanceEvidenceDto acceptance) <- controls,
                  Peer.AlignmentCutAcceptedDto generation _ topologyCut _ _ <- [acceptance],
                  Set.member generation exactSortGenerations
                ]
                  <> [ topologyCut
                     | (_, controls) <- namedOutboundPeer,
                       Peer.AlignmentEvidenceDeliveredDto _ (Peer.AlignmentPlanAcceptanceEvidenceDto acceptance) <- controls,
                       Peer.AlignmentPlanAcceptedDto (Peer.AlignmentPlanIdDto _ _ sortId _ topologyCut _) _ _ <- [acceptance],
                       sortId == expectedSort
                     ]
              )
          exactSortCutDetails =
            Map.elems
              ( Map.fromList
                  [ ( generation,
                      (generation, placement, predecessors, freshBases)
                    )
                  | (_, controls) <- namedControls,
                    (_, control) <- controls,
                    announce <- announcedGenerationCuts control,
                    Peer.AlignmentCutAnnounceDto generation cut <- [announce],
                    Set.member generation exactSortGenerations,
                    Peer.AlignmentCutDto _ _ _ _ _ placement _ predecessors freshBases <- [cut]
                  ]
              )
          receivedPlacementMaxima =
            [ ( heraldName,
                Map.toAscList
                  ( Map.fromListWith
                      max
                      [ placementUpdateCoordinate update
                      | Peer.PlacementUpdateDto update <- controls
                      ]
                  )
              )
            | (heraldName, controls) <- namedInboundPeer
            ]
          exactTopologyTranscript named =
            Map.toAscList
              ( Map.fromListWith
                  (+)
                  [ ( (heraldName, peerControlTag control),
                      1 :: Int
                    )
                  | (heraldName, controls) <- named,
                    control <- controls,
                    maybe
                      False
                      (`Set.member` exactTopologyCuts)
                      (peerControlTopologyCut control)
                  ]
              )
          exactSortCertificates =
            Set.toAscList
              ( Set.fromList
                  [ ( heraldName,
                      generation,
                      Peer.storeIncarnationClaimBytes store
                    )
                  | (heraldName, controls) <- namedControls,
                    (_, Peer.AlignmentHistoricalCertificateAdvertisedDto certificate) <- controls,
                    Peer.HistoricalCertificateDto generation store _ _ _ <- [certificate],
                    Set.member generation exactSortGenerations
                  ]
              )
          exactSortAcceptances =
            Set.toAscList
              ( Set.fromList
                  [ (heraldName, generation, reporter)
                  | (heraldName, controls) <- namedOutboundPeer,
                    Peer.AlignmentEvidenceDeliveredDto _ (Peer.AlignmentCutAcceptanceEvidenceDto acceptance) <- controls,
                    Peer.AlignmentCutAcceptedDto generation reporter _ _ _ <- [acceptance],
                    Set.member generation exactSortGenerations
                  ]
              )
          exactSortReady =
            Set.toAscList
              ( Set.fromList
                  [ ( heraldName,
                      generation,
                      Peer.storeIncarnationClaimBytes store
                    )
                  | (heraldName, controls) <- namedOutboundPeer,
                    Peer.AlignmentEvidenceDeliveredDto _ (Peer.AlignmentMemberReadyEvidenceDto ready) <- controls,
                    Peer.ClassMemberReadyDto _ generation store _ _ <- [ready],
                    Set.member generation exactSortGenerations
                  ]
              )
          exactSortPlanAcceptances =
            Set.toAscList
              ( Set.fromList
                  [ (heraldName, show plan, reporter)
                  | (heraldName, controls) <- namedOutboundPeer,
                    Peer.AlignmentEvidenceDeliveredDto _ (Peer.AlignmentPlanAcceptanceEvidenceDto acceptance) <- controls,
                    Peer.AlignmentPlanAcceptedDto plan reporter _ <- [acceptance],
                    Peer.AlignmentPlanIdDto _ _ sortId _ _ _ <- [plan],
                    sortId == expectedSort
                  ]
              )
      assertFailure
        ( "the expected source context produced no unique live subscription; expected destination "
            <> show (storeIncarnationIdBytes destination)
            <> "; expected sources "
            <> show (Set.map storeIncarnationIdBytes sourceStores)
            <> "; exact-sort outbound subscriptions "
            <> show relevant
            <> "; exact-sort cut announcements "
            <> show exactSortCuts
            <> "; exact-sort cut coordinates "
            <> show exactSortCutDetails
            <> "; received placement maxima "
            <> show receivedPlacementMaxima
            <> "; exact-generation cut acceptances "
            <> show exactSortAcceptances
            <> "; exact-sort current-plan acceptances "
            <> show exactSortPlanAcceptances
            <> "; exact-generation member-ready "
            <> show exactSortReady
            <> "; outbound historical certificates "
            <> show exactSortCertificates
            <> "; outbound alignment histogram "
            <> show
              [ (name, alignmentControlHistogram (fmap snd controls))
              | (name, controls) <- namedControls
              ]
            <> "; inbound alignment histogram "
            <> show
              [ (name, alignmentControlHistogram controls)
              | (name, controls) <- namedInbound
              ]
            <> "; exact-topology outbound transcript "
            <> show (exactTopologyTranscript namedOutboundPeer)
            <> "; exact-topology inbound transcript "
            <> show (exactTopologyTranscript namedInboundPeer)
            <> "; peer-control protocol closes "
            <> show namedProtocolCloses
            <> "; kernel faults "
            <> show namedFaults
        )
  where
    loop = do
      h2Controls <- outboundAlignmentControls (step14H2 deployment)
      allControls <- deploymentAlignmentControls deployment
      let liveSubscriptions = observedLiveSubscriptions allControls
          cancelledSubscriptions = observedCancelledSubscriptions allControls
          distinctSubscribes =
            Map.elems
              ( Map.fromList
                  [ (subscribeId subscribe, subscribe)
                  | (_, Peer.AlignmentSubscribeRequestedDto subscribe) <- h2Controls
                  ]
              )
          candidates =
            [ subscribe
            | subscribe <- distinctSubscribes,
              isFinalCertificate subscribe,
              destinationIncarnations subscribe
                == Set.singleton (storeIncarnationIdBytes destination),
              Set.member
                (subscribeSourceStoreKey subscribe)
                (Set.map storeIncarnationIdBytes sourceStores),
              Set.member
                (subscribeId subscribe)
                liveSubscriptions,
              Set.notMember
                (subscribeId subscribe)
                cancelledSubscriptions
            ]
      case candidates of
        [current] -> pure current
        _ -> threadDelay 10_000 >> loop

observedLiveSubscriptions ::
  [Peer.AlignmentControlDto] ->
  Set Peer.AlignmentSubscriptionIdDto
observedLiveSubscriptions controls =
  Set.fromList
    [ liveSubscriptionId live
    | Peer.AlignmentLiveAdvertisedDto live <- controls
    ]

observedCancelledSubscriptions ::
  [Peer.AlignmentControlDto] ->
  Set Peer.AlignmentSubscriptionIdDto
observedCancelledSubscriptions controls =
  Set.fromList
    [ cancelSubscriptionId cancellation
    | Peer.AlignmentCancelledDto cancellation <- controls
    ]

isFinalCertificate :: Peer.AlignmentSubscribeDto -> Bool
isFinalCertificate subscribe = case subscribeSourceProof subscribe of
  Peer.FinalCertificateDto {} -> True
  _ -> False

destinationIncarnations :: Peer.AlignmentSubscribeDto -> Set ByteString
destinationIncarnations = destinationStoreKeys . subscribeDestinationStores

subscribeObligation ::
  Peer.AlignmentSubscribeDto ->
  Peer.AlignmentObligationDto
subscribeObligation (Peer.AlignmentSubscribeDto obligation _ _ _) = obligation

subscribeId ::
  Peer.AlignmentSubscribeDto ->
  Peer.AlignmentSubscriptionIdDto
subscribeId (Peer.AlignmentSubscribeDto _ subscription _ _) = subscription

subscribeDestinationGeneration,
  subscribeSourceGeneration ::
    Peer.AlignmentSubscribeDto -> Peer.ContextClassGenerationClaim
subscribeDestinationGeneration subscribe = case subscribeObligation subscribe of
  Peer.AlignmentObligationDto _ _ _ _ generation _ _ _ -> generation
subscribeSourceGeneration subscribe = case subscribeObligation subscribe of
  Peer.AlignmentObligationDto _ _ _ _ _ _ generation _ -> generation

awaitGenerationCut :: Step14Deployment -> Peer.ContextClassGenerationClaim -> IO Peer.AlignmentCutDto
awaitGenerationCut deployment expected = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  maybe (assertFailure ("generation cut unavailable: " <> show expected)) pure observed
  where
    loop = do
      controls <- deploymentAlignmentControls deployment
      case [cut | control <- controls, Peer.AlignmentCutAnnounceDto generation cut <- announcedGenerationCuts control, generation == expected] of
        cut : _ -> pure cut
        [] -> threadDelay 10_000 >> loop

assertOwnedCancellation :: Step14Herald -> Peer.AlignmentSubscriptionIdDto -> Peer.AlignmentCancelReasonDto -> Assertion
assertOwnedCancellation herald subscription expected = do
  controls <- outboundAlignmentControls herald
  assertEqual
    "the subscription has exactly one cancellation from this owner"
    [expected]
    [cancelReason cancellation | (_, Peer.AlignmentCancelledDto cancellation) <- controls, cancelSubscriptionId cancellation == subscription]

subscribeSourceStore ::
  Peer.AlignmentSubscribeDto ->
  Peer.StoreIncarnationClaim
subscribeSourceStore (Peer.AlignmentSubscribeDto _ _ source _) = source

subscribeSourceStoreKey :: Peer.AlignmentSubscribeDto -> ByteString
subscribeSourceStoreKey =
  Peer.storeIncarnationClaimBytes . subscribeSourceStore

subscribeSourceProof ::
  Peer.AlignmentSubscribeDto ->
  Peer.AlignmentSourceProofDto
subscribeSourceProof (Peer.AlignmentSubscribeDto _ _ _ proof) = proof

subscribeObligationId ::
  Peer.AlignmentSubscribeDto ->
  Peer.AlignmentObligationIdDto
subscribeObligationId subscribe = case subscribeObligation subscribe of
  Peer.AlignmentObligationDto obligation _ _ _ _ _ _ _ -> obligation

subscribeSortId :: Peer.AlignmentSubscribeDto -> SortId
subscribeSortId subscribe = case subscribeObligation subscribe of
  Peer.AlignmentObligationDto _ _ sortId _ _ _ _ _ -> sortId

subscribeDestinationStores ::
  Peer.AlignmentSubscribeDto ->
  NonEmpty Peer.DestinationStoreDto
subscribeDestinationStores subscribe = case subscribeObligation subscribe of
  Peer.AlignmentObligationDto _ _ _ _ _ destinations _ _ -> destinations

destinationStoreKeys ::
  NonEmpty Peer.DestinationStoreDto ->
  Set ByteString
destinationStoreKeys destinations =
  Set.fromList
    [ Peer.storeIncarnationClaimBytes store
    | Peer.DestinationStoreDto _ store <- NonEmpty.toList destinations
    ]

cancelSubscriptionId ::
  Peer.AlignmentCancelDto ->
  Peer.AlignmentSubscriptionIdDto
cancelSubscriptionId (Peer.AlignmentCancelDto subscription _) = subscription

cancelReason ::
  Peer.AlignmentCancelDto ->
  Peer.AlignmentCancelReasonDto
cancelReason (Peer.AlignmentCancelDto _ reason) = reason

liveSubscriptionId ::
  Peer.AlignmentLiveDto ->
  Peer.AlignmentSubscriptionIdDto
liveSubscriptionId (Peer.AlignmentLiveDto subscription _) = subscription

changeSubscriptionId ::
  Peer.AlignmentChangeDto ->
  Peer.AlignmentSubscriptionIdDto
changeSubscriptionId (Peer.AlignmentChangeDto subscription _ _) = subscription

awaitExactCancellation ::
  Step14Herald ->
  Peer.AlignmentSubscriptionIdDto ->
  Peer.AlignmentCancelReasonDto ->
  IO (TracePosition, Peer.AlignmentCancelDto)
awaitExactCancellation herald subscription reason = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  maybe
    ( assertFailure
        ( "no exact "
            <> show reason
            <> " cancellation appeared for "
            <> show subscription
        )
    )
    pure
    observed
  where
    loop = do
      controls <- outboundAlignmentControls herald
      case [ (position, cancellation)
           | (position, Peer.AlignmentCancelledDto cancellation) <- controls,
             cancelSubscriptionId cancellation == subscription,
             cancelReason cancellation == reason
           ] of
        first : _ -> pure first
        [] -> threadDelay 10_000 >> loop

awaitInboundCancellation ::
  Step14Herald ->
  Peer.AlignmentSubscriptionIdDto ->
  Peer.AlignmentCancelReasonDto ->
  IO RuntimeEventOrdinal
awaitInboundCancellation herald subscription reason = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  maybe
    ( assertFailure
        ( "no received "
            <> show reason
            <> " cancellation appeared for "
            <> show subscription
        )
    )
    pure
    observed
  where
    loop = do
      controls <- inboundAlignmentControlEvents herald
      case [ ordinal
           | (ordinal, Peer.AlignmentCancelledDto cancellation) <- controls,
             cancelSubscriptionId cancellation == subscription,
             cancelReason cancellation == reason
           ] of
        first : _ -> pure first
        [] -> threadDelay 10_000 >> loop

awaitInboundSubscribe ::
  Step14Herald ->
  Peer.AlignmentSubscribeDto ->
  IO RuntimeEventOrdinal
awaitInboundSubscribe herald expected = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  maybe
    ( assertFailure
        ( "no received exact alignment subscription appeared for "
            <> show (subscribeId expected)
        )
    )
    pure
    observed
  where
    loop = do
      controls <- inboundAlignmentControlEvents herald
      case [ ordinal
           | (ordinal, Peer.AlignmentSubscribeRequestedDto subscribe) <- controls,
             subscribe == expected
           ] of
        first : _ -> pure first
        [] -> threadDelay 10_000 >> loop

awaitInboundLive ::
  Step14Herald ->
  Peer.AlignmentSubscriptionIdDto ->
  IO RuntimeEventOrdinal
awaitInboundLive herald subscription = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  maybe
    ( assertFailure
        ( "no received live alignment control appeared for "
            <> show subscription
        )
    )
    pure
    observed
  where
    loop = do
      controls <- inboundAlignmentControlEvents herald
      case [ ordinal
           | (ordinal, Peer.AlignmentLiveAdvertisedDto live) <- controls,
             liveSubscriptionId live == subscription
           ] of
        first : _ -> pure first
        [] -> threadDelay 10_000 >> loop

assertExactCancellationCount ::
  Step14Deployment ->
  Peer.AlignmentSubscriptionIdDto ->
  Peer.AlignmentCancelReasonDto ->
  Assertion
assertExactCancellationCount deployment subscription reason = do
  controls <- deploymentAlignmentControls deployment
  assertEqual
    ("exact cancellation count for " <> show reason)
    [reason]
    [ cancelReason cancellation
    | Peer.AlignmentCancelledDto cancellation <- controls,
      cancelSubscriptionId cancellation == subscription
    ]

awaitReplacementPlanSubscription ::
  Step14Deployment ->
  Peer.AlignmentSubscriptionIdDto ->
  Peer.AlignmentObligationIdDto ->
  NonEmpty Peer.DestinationStoreDto ->
  StoreIncarnationId ->
  Set Peer.AlignmentSubscriptionIdDto ->
  IO Peer.AlignmentSubscribeDto
awaitReplacementPlanSubscription deployment oldSubscription oldObligation oldDestinations survivingStore previousSubscriptions = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  maybe
    (assertFailure "current-member loss produced no surviving-source replacement plan")
    pure
    observed
  where
    loop = do
      controls <- outboundAlignmentControls (step14H2 deployment)
      case [ subscribe
           | (_, Peer.AlignmentSubscribeRequestedDto subscribe) <- controls,
             subscribeId subscribe /= oldSubscription,
             Set.notMember (subscribeId subscribe) previousSubscriptions,
             subscribeObligationId subscribe /= oldObligation,
             subscribeDestinationStores subscribe == oldDestinations,
             subscribeSourceStoreKey subscribe
               == storeIncarnationIdBytes survivingStore
           ] of
        first : _ -> pure first
        [] -> threadDelay 10_000 >> loop

awaitAlignmentLive ::
  Step14Deployment ->
  Peer.AlignmentSubscriptionIdDto ->
  IO ()
awaitAlignmentLive deployment subscription = do
  observed <- timeout alignmentTimeoutMicroseconds loop
  unless (observed == Just ())
    $ assertFailure ("alignment subscription did not become live: " <> show subscription)
  where
    loop = do
      controls <- deploymentAlignmentControls deployment
      if any
        ( \case
            Peer.AlignmentLiveAdvertisedDto live ->
              liveSubscriptionId live == subscription
            _ -> False
        )
        controls
        then pure ()
        else threadDelay 10_000 >> loop

matchingInboundChangeCount ::
  Step14Herald ->
  Peer.AlignmentSubscriptionIdDto ->
  IO Int
matchingInboundChangeCount herald subscription = do
  controls <- inboundAlignmentControls herald
  pure
    ( length
        [ ()
        | Peer.AlignmentChangeTransferredDto change <- controls,
          changeSubscriptionId change == subscription
        ]
    )

publishMessage ::
  EAPP.Application ->
  AlignmentLossTopology ->
  Text ->
  IO EAPP.ApplicationCall
publishMessage application topology message =
  EAPP.write
    application
    topology.lossProducerWriter
    (PublishValue (messageValue message))

messageValue :: Text -> ApplicationValue
messageValue message =
  RecordValue (Map.singleton "message" (TextValue message))

awaitMessage ::
  String ->
  EAPP.Application ->
  PrivateDeltaId ->
  Text ->
  IO ()
awaitMessage context application reader message = do
  observed <- timeout applicationTimeoutMicroseconds loop
  unless (observed == Just ())
    $ assertFailure (context <> " did not receive " <> show message)
  where
    expected = messageValue message
    loop = do
      values <- expectRead "alignment destination read" =<< EAPP.read application (queryAlways reader)
      if expected `elem` values
        then pure ()
        else threadDelay 10_000 >> loop

awaitCarrierAbsent ::
  EAPP.Application ->
  PrivateDeltaId ->
  PrivateUniqueId ->
  IO ()
awaitCarrierAbsent application reader object = do
  observed <- timeout applicationTimeoutMicroseconds loop
  unless (observed == Just ())
    $ assertFailure "the deleted destination Delta carrier remained visible"
  where
    loop = do
      values <-
        expectRead "deleted destination carrier read"
          =<< EAPP.read application (queryObject reader object)
      if null values
        then pure ()
        else threadDelay 10_000 >> loop

publishEdge ::
  String ->
  EAPP.Application ->
  PrivateNablaId ->
  PrivateProcessId ->
  PrivateUniqueId ->
  PrivateUniqueId ->
  IO PrivateUniqueId
publishEdge context application writer process source destination =
  reserveAndPublishCarrier
    context
    application
    writer
    (preservingEdgeCarrier process source destination)

reserveAndPublishCarrier ::
  String ->
  EAPP.Application ->
  PrivateNablaId ->
  (PrivateUniqueId -> ApplicationValue) ->
  IO PrivateUniqueId
reserveAndPublishCarrier context application writer carrier = do
  object <-
    expectNewId (context <> " reservation")
      =<< EAPP.newid application (ControlledNewId writer)
  expectWriteAccepted (context <> " publication")
    =<< EAPP.write application writer (PublishValue (carrier object))
  pure object

messageDescriptor :: ApplicationSortDescriptor
messageDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "message" TextSchema),
      keyProjections = [ApplicationProjection ("message" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

nablaCarrier ::
  PrivateProcessId ->
  SortId ->
  PrivateUniqueId ->
  PrivateUniqueId ->
  ApplicationValue
nablaCarrier process sortId sequencing object =
  RecordValue
    ( Map.fromList
        [ ("label", LabelValue ((ProcessLabel process, 0))),
          ("object_id", UniqueIdValue object),
          ("sequencing_object", OptionalUniqueIdValue (Just sequencing)),
          ("sort_id", BytesValue (sortIdBytes sortId))
        ]
    )

deltaCarrier ::
  PrivateProcessId ->
  SortId ->
  PrivateUniqueId ->
  ApplicationValue
deltaCarrier process sortId object =
  RecordValue
    ( Map.fromList
        [ ("label", LabelValue ((ProcessLabel process, 0))),
          ("object_id", UniqueIdValue object),
          ("sort_id", BytesValue (sortIdBytes sortId))
        ]
    )

preservingEdgeCarrier ::
  PrivateProcessId ->
  PrivateUniqueId ->
  PrivateUniqueId ->
  PrivateUniqueId ->
  ApplicationValue
preservingEdgeCarrier process source destination object =
  RecordValue
    ( Map.fromList
        [ ("destination_vertex", UniqueIdValue destination),
          ("label", LabelValue ((ProcessLabel process, 0))),
          ("object_id", UniqueIdValue object),
          ("source_vertex", UniqueIdValue source),
          ("strength", EnumValue "preserve")
        ]
    )

awaitSortDefinition ::
  EAPP.Application ->
  PrivateDeltaId ->
  SortId ->
  IO ()
awaitSortDefinition application reader sortId = do
  observed <- timeout applicationTimeoutMicroseconds loop
  unless (observed == Just ())
    $ assertFailure "Q did not observe the alignment message sort"
  where
    expected =
      SortDefinitionValue
        (DeclaredSortDefinition messageDescriptor (Just sortId))
    loop = do
      values <- expectRead "Q sort-definition read" =<< EAPP.read application (queryAlways reader)
      if expected `elem` values
        then pure ()
        else threadDelay 10_000 >> loop

observedLocalizedDeltaCarriers ::
  EAPP.Application ->
  PrivateDeltaId ->
  SortId ->
  IO (Set (PrivateUniqueId, PrivateProcessId))
observedLocalizedDeltaCarriers application reader sortId =
  Set.fromList . mapMaybeLocalized sortId
    <$> (expectRead "P localized Delta read" =<< EAPP.read application (queryAlways reader))

awaitFreshLocalizedDeltaCarrier ::
  EAPP.Application ->
  PrivateDeltaId ->
  SortId ->
  Set (PrivateUniqueId, PrivateProcessId) ->
  IO (PrivateUniqueId, PrivateProcessId)
awaitFreshLocalizedDeltaCarrier application reader sortId known = do
  observed <- timeout localizationTimeoutMicroseconds loop
  maybe
    (assertFailure "P did not localize Q's destination Delta carrier")
    pure
    observed
  where
    loop = do
      values <- expectRead "P localized-Q Delta read" =<< EAPP.read application (queryAlways reader)
      case Set.toList
        ( Set.fromList (mapMaybeLocalized sortId values)
            `Set.difference` known
        ) of
        [identity] -> pure identity
        _ -> threadDelay 10_000 >> loop

mapMaybeLocalized ::
  SortId ->
  [ApplicationValue] ->
  [(PrivateUniqueId, PrivateProcessId)]
mapMaybeLocalized sortId values =
  [ (object, process)
  | RecordValue fields <- values,
    Map.size fields == 3,
    Just (UniqueIdValue object) <- [Map.lookup "object_id" fields],
    Just (LabelValue ((ProcessLabel process, _))) <- [Map.lookup "label" fields],
    Just (BytesValue observedSort) <- [Map.lookup "sort_id" fields],
    observedSort == sortIdBytes sortId
  ]

queryAlways :: PrivateDeltaId -> ApplicationQuery
queryAlways reader =
  ApplicationQuery
    { applicationQueryDeltas = Set.singleton reader,
      applicationQueryPredicate = QueryAlways
    }

queryObject ::
  PrivateDeltaId ->
  PrivateUniqueId ->
  ApplicationQuery
queryObject reader object =
  ApplicationQuery
    { applicationQueryDeltas = Set.singleton reader,
      applicationQueryPredicate =
        QueryCompare
          (ApplicationProjection ("object_id" :| []))
          ScalarEqual
          (QueryUniqueId object)
    }

accessFor ::
  String ->
  ApplicationPredefinedSortRole ->
  ApplicationStartupAccess ->
  IO PredefinedAccess
accessFor process role startup =
  case (Map.lookup (Access.environmentWriterKey role) (Access.accessEntries (Access.startupAccessPrimordial startup)), Map.lookup (Access.environmentReaderKey role) (Access.accessEntries (Access.startupAccessPrimordial startup))) of
    (Just (Access.Writer writer), Just (Access.Reader reader)) -> pure (Access.predefinedAccess role writer reader)
    _ ->
      assertFailure
        (process <> " startup access is missing " <> show role)

expectNewId :: String -> EAPP.ApplicationCall -> IO PrivateUniqueId
expectNewId context call = do
  completion <- awaitApplicationCallWithin context call
  case completion of
    EAPP.ApplicationCallSucceeded (NewIdCompleted identifier) -> pure identifier
    other -> unexpected context other

expectSortDefinition :: String -> EAPP.ApplicationCall -> IO SortId
expectSortDefinition context call = do
  completion <- awaitApplicationCallWithin context call
  case completion of
    EAPP.ApplicationCallSucceeded
      (WriteCompleted (SortDefinitionWritten identifier)) -> pure identifier
    other -> unexpected context other

expectWriteAccepted :: String -> EAPP.ApplicationCall -> IO ()
expectWriteAccepted context call = do
  completion <- awaitApplicationCallWithin context call
  case completion of
    EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted) -> pure ()
    other -> unexpected context other

expectLabelResult :: String -> EAPP.ApplicationCall -> IO ()
expectLabelResult context call = do
  completion <- awaitApplicationCallWithin context call
  case completion of
    EAPP.ApplicationCallSucceeded (LabelCompleted LabelApplied) -> pure ()
    other -> unexpected context other

expectRead :: String -> EAPP.ApplicationCall -> IO [ApplicationValue]
expectRead context call = do
  completion <- awaitApplicationCallWithin context call
  case completion of
    EAPP.ApplicationCallSucceeded (ReadCompleted values) -> pure values
    other -> unexpected context other

-- Awaiting a call directly under 'timeout' is not a bounded test wait:
-- 'EAPP.awaitApplicationCall' deliberately catches cancellation, submits the
-- stable client cancellation command, and waits for the retained terminal
-- result before rethrowing. Observe it in an independent thread instead, so a
-- dead Herald or lost terminal result produces a prompt, contextual failure.
-- Application-scope teardown closes the outstanding call and therefore also
-- releases the observer after this callback fails.
awaitApplicationCallWithin ::
  String ->
  EAPP.ApplicationCall ->
  IO EAPP.ApplicationCallCompletion
awaitApplicationCallWithin context call = do
  observed <-
    newEmptyMVar ::
      IO (MVar (Either SomeException EAPP.ApplicationCallCompletion))
  void
    ( forkFinally
        (EAPP.awaitApplicationCall call)
        (putMVar observed)
    )
  timeout applicationCallTimeoutMicroseconds (readMVar observed) >>= \case
    Nothing -> do
      EAPP.cancelApplicationCall call
      assertFailure
        ( context
            <> ": application call exceeded its failure bound"
        )
    Just (Left exception) -> throwIO exception
    Just (Right completion) -> pure completion

unexpected :: (Show actual) => String -> actual -> IO value
unexpected context actual =
  assertFailure (context <> ": unexpected application result " <> show actual)

predecessorMessage, destinationLossMessage, sourceLossMessage :: Text
predecessorMessage = "00 predecessor readiness"
destinationLossMessage = "02 traffic after destination loss"
sourceLossMessage = "02 history after selected-source passivation"

scenarioTimeoutMicroseconds, alignmentTimeoutMicroseconds, localizationTimeoutMicroseconds, applicationTimeoutMicroseconds, applicationCallTimeoutMicroseconds :: Int
scenarioTimeoutMicroseconds = 900_000_000
alignmentTimeoutMicroseconds = 180_000_000
localizationTimeoutMicroseconds = 180_000_000
applicationTimeoutMicroseconds = 15_000_000
applicationCallTimeoutMicroseconds = 10_000_000
