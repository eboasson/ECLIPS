{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module AlignmentRecoveryProperties
  ( tests,
    freshSubscribe,
    commitDestinationBootstrap,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (scanl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Context (ContextGraph, deriveContextGraph)
import Eclips.Domain.Graph
  ( EdgePayload,
    EdgeStrength (Preserve),
    VertexId (DeltaVertex),
    edgePayload,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence qualified as StructuralConsequence
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationPlan,
    AlignmentGenerationPreparation (AlignmentGenerationReady),
    GenerationMemberInput,
    alignmentGenerationCut,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    generationMemberInput,
    prepareAlignmentGenerationPlan,
  )
import Eclips.Herald.Alignment.Loss qualified as AlignmentLoss
import Eclips.Herald.Alignment.Plan.Identity qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    StructuralConsequenceDebt,
    StructuralDebtKind (TopologyAlignmentDebt),
    normalizeStructuralDebts,
    sortOccurrence,
    structuralConsequenceDebt,
    structuralConsequenceDebtKey,
    structuralDebtEvidence,
    structuralDebtKey,
    structuralDebtSetEntries,
  )
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransferCoordinator
import GenesisFixtures (fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QuickCheck

tests :: TestTree
tests =
  testGroup
    "alignment bootstrap and recovery"
    [ testCase "completed generation candidates retire without losing historical consumers" casePendingGenerationCandidates,
      testCase "split imports every selected predecessor into every child" caseSplitBootstrap,
      testCase "merge imports both predecessors into every exact member" caseMergeBootstrap,
      testCase "passivate/reactivate freezes a fresh incarnation base" casePassivateReactivate,
      testCase "one generation-level bootstrap key survives two active occurrence promotions" caseOccurrenceDeduplication,
      testCase "a fulfilled generation-level bootstrap key survives later occurrence promotion" caseFulfilledOccurrenceDeduplication,
      testCase "fresh rev0 snapshot and contiguous suffix are lossless" caseFreshSnapshotSuffix,
      testCase "duplicate Subscribe retries only the unacknowledged source suffix" caseAcknowledgedSourceRetry,
      testCase "reconnect transfer repair is rooted at the destination Subscribe" caseDestinationRootedReconnect,
      testCase "terminal source retry emits only its retained cancellation" caseTerminalSourceRetry,
      testCase "atomic loss projection emits source cancellation once before replacement Subscribe" caseAtomicLossSourceCancellationOrdering,
      testCase "source loss reselects a fresh predecessor attempt then exhausts" caseSourceLossReselection,
      testCase "destination loss before any attempt tombstones every logical owner" caseDestinationLossBeforeAttempt,
      testCase "destination loss tombstones the whole plan coordinate" caseDestinationLossTombstone,
      testCase "destination loss after fulfillment still tombstones the coordinate" caseDestinationLossAfterFulfillment,
      testCase "retained remote loss invalidates a later admitted current plan" caseRetainedRemoteLossBeforePromotion,
      testCase "a tombstoned coordinate rejects later promotion but a fresh vector promotes" caseTombstonedPromotion,
      testCase "fresh source and destination loss commute with destination loss dominant" caseFreshLossOrderConvergence,
      testCase "one lost fresh source does not exclude another fresh coordinate" caseMultiFreshSourceCoordinateIndependence,
      testCase "input closure records an exact local lower bound without a child gate" caseInputClosure,
      testCase "bootstrap-purpose destinations are excluded from predecessor closure" caseBootstrapPurposeExcludedFromClosure,
      testCase "one terminal predecessor closes both split children without reoffer" caseSplitInputClosure,
      QuickCheck.testProperty "source prefix notifications match an independent active-source scan" propSourcePrefixWorkIndex,
      testCase "source prefix cursor preserves entry membership and same-pass forward wakes" caseSourcePrefixCursor,
      QuickCheck.testProperty "pending predecessor sources preserve scan semantics and terminal exclusions" propPendingPredecessorSources,
      QuickCheck.testProperty "input closure destination indices and wake scopes match transcript scans" propInputClosureDestinationIndex,
      testCase "input closure cursor and membership wake only pending executable sources" caseInputClosureWorkCursor,
      testCase "closure producer notifications are target-local, child-local and duplicate-stable" caseInputClosureScopeNotifications,
      testCase "loopback cancellation retires both source and destination closure relations" caseInputClosureLoopbackCancellation,
      testCase "a repeated Live witnesses new barriers independently of destination progress" caseInputClosureWitnessWake,
      testCase "immediately live and shared predecessor sources retain barriers until cancellation" caseInputClosureRetainedSources,
      testCase "closure barrier replacement and historical receipts preserve indexed lifecycle" caseInputClosureBarrierLifecycle,
      QuickCheck.testProperty "current generation catalogue and loss discovery preserve historical scan semantics" propLatestGenerationCatalogue,
      QuickCheck.testProperty "live debt classification preserves promotion and recovery history" propLiveDebtClassifier,
      QuickCheck.testProperty "bootstrap attempt lookup preserves insertion and invalidation semantics" propBootstrapAttemptLookup
    ]

-- Check the retained index against a separate source-transcript scan after
-- admission, consumption, repeated notifications, exact replay, and loss.
propSourcePrefixWorkIndex :: QuickCheck.Property
propSourcePrefixWorkIndex =
  QuickCheck.forAll (QuickCheck.chooseInt (1, 12)) $ \count ->
    QuickCheck.forAll (QuickCheck.vectorOf 40 ((,) <$> QuickCheck.chooseInt (0, count - 1) <*> QuickCheck.chooseInt (0, 3))) $ \schedule ->
      let identifiers = take count sourcePrefixIdentifiers
          specifications = zip identifiers (cycle [predecessorStore, destinationStore])
          initial = foldl (\state (identifier, incarnation) -> admitSourcePrefixFixture identifier incarnation state) Transfer.emptyState specifications
          step (state, pending) (offset, operation) =
            let (identifier, incarnation) = specifications !! offset
             in case operation of
                  0 -> (Transfer.consumeSourcePrefixWork identifier state, Set.delete identifier pending)
                  1 -> (Transfer.wakeSourcePrefixes (Set.singleton incarnation) state, pending `Set.union` referenceSourcesAt incarnation state)
                  2 ->
                    ( fst
                        ( Transfer.commitAlignmentCancellation
                            (checked "cancel indexed source" (Transfer.prepareAlignmentCancellation (Protocol.alignmentCancel identifier Protocol.AlignmentSourceIncarnationLost) state))
                        ),
                      Set.delete identifier pending
                    )
                  _ -> (admitSourcePrefixFixture identifier incarnation state, pending)
          histories = scanl' step (initial, Set.fromList identifiers) schedule
       in QuickCheck.conjoin
            [ QuickCheck.counterexample ("source-prefix history " <> show index)
                $ QuickCheck.conjoin
                  [ Transfer.validateAlignmentTransferState state QuickCheck.=== Right (),
                    Transfer.pendingSourcePrefixIds state QuickCheck.=== pending,
                    Transfer.sourceSubscriptionIds state
                      QuickCheck.=== (referenceSourcesAt predecessorStore state `Set.union` referenceSourcesAt destinationStore state),
                    Transfer.wakeSourcePrefixes Set.empty state QuickCheck.=== state,
                    Transfer.pendingSourcePrefixIds (Transfer.wakeSourcePrefixes (Set.fromList [predecessorStore, destinationStore]) state)
                      QuickCheck.=== (referenceSourcesAt predecessorStore state `Set.union` referenceSourcesAt destinationStore state)
                  ]
            | (index, (state, pending)) <- zip [0 :: Int ..] histories
            ]
  where
    referenceSourcesAt incarnation state =
      Set.fromList
        [ identifier
        | (identifier, source) <- Transfer.sourceSubscriptionEntries state,
          Transfer.sourceSubscriptionCancellation source == Nothing,
          Protocol.alignmentSubscribeSourceStoreIncarnation (Transfer.sourceSubscriptionSubscribe source) == incarnation
        ]

caseSourcePrefixCursor :: Assertion
caseSourcePrefixCursor = do
  let first = sourcePrefixIdentifiers !! 0
      inserted = sourcePrefixIdentifiers !! 1
      later = sourcePrefixIdentifiers !! 2
      initial = admitSourcePrefixFixture later destinationStore (admitSourcePrefixFixture first predecessorStore Transfer.emptyState)
      entry = Transfer.consumeSourcePrefixWork later initial
      inventory = Transfer.sourceSubscriptionIds entry
      consumed = Transfer.consumeSourcePrefixWork first entry
      loopback =
        Transfer.wakeSourcePrefixes
          (Set.fromList [predecessorStore, destinationStore])
          (admitSourcePrefixFixture inserted destinationStore consumed)
      afterLater = Transfer.consumeSourcePrefixWork later loopback
  assertEqual "first dirty source starts the entry pass" (Just first) (Transfer.nextSourcePrefixWork inventory Nothing entry)
  assertEqual "existing clean source awakened ahead runs in this pass" (Just later) (Transfer.nextSourcePrefixWork inventory (Just first) loopback)
  assertEqual "new identities and earlier reawakening wait for next pass" Nothing (Transfer.nextSourcePrefixWork inventory (Just later) afterLater)
  assertEqual "deferred work remains executable" (Set.fromList [first, inserted]) (Transfer.pendingSourcePrefixIds afterLater)
  assertEqual "the next pass starts with the earlier reawakened source" (Just first) (Transfer.nextSourcePrefixWork (Transfer.sourceSubscriptionIds afterLater) Nothing afterLater)
  assertEqual "cursor bookkeeping remains checked" (Right ()) (Transfer.validateAlignmentTransferState afterLater)

sourcePrefixIdentifiers :: [Protocol.AlignmentSubscriptionId]
sourcePrefixIdentifiers =
  fmap
    (Protocol.alignmentSubscriptionId heraldA)
    (iterate Protocol.nextAlignmentSubscriptionSequence Protocol.firstAlignmentSubscriptionSequence)

admitSourcePrefixFixture :: Protocol.AlignmentSubscriptionId -> Identity.StoreIncarnationId -> Transfer.State -> Transfer.State
admitSourcePrefixFixture identifier incarnation state =
  fst
    ( Transfer.commitSourceSubscription
        ( checked
            "admit indexed source"
            ( Transfer.prepareSourceSubscription
                (bootstrapSubscribeFor identifier newGeneration predecessorGeneration incarnation (DomainAlignment.deriveBootstrapEvidenceDigest "indexed source"))
                DomainAlignment.initialStoreRevision
                []
                Transfer.SourceLiveImmediate
                state
            )
        )
    )

-- The coordinator takes its identifier snapshot before loopback work. Excluding
-- a retained source is safe only if checked progress cannot make it pending
-- later in that same pass; newly admitted identifiers belong to the next pass.
propPendingPredecessorSources :: QuickCheck.Property
propPendingPredecessorSources =
  QuickCheck.forAll (QuickCheck.chooseInt (0, 12)) $ \extraCount ->
    QuickCheck.forAll (QuickCheck.vectorOf extraCount (QuickCheck.chooseInt (0, 5))) $ \extraShapes ->
      QuickCheck.forAll (QuickCheck.shuffle (zip identifiers ([0, 0, 1, 2, 3, 4, 5] <> extraShapes))) $ \specifications ->
        let subscribeFor (identifier, shape)
              | shape `mod` 3 == 2 =
                  Transfer.preparedDestinationAttemptSubscribe
                    ( checked
                        "pending-source final certificate"
                        (Transfer.prepareDestinationAttempt (contextAttempt identifier predecessorGeneration predecessorStore) Transfer.emptyState)
                    )
              | otherwise =
                  bootstrapSubscribeFor
                    identifier
                    newGeneration
                    (if shape `mod` 3 == 0 then predecessorGeneration else newGeneration)
                    destinationStore
                    (DomainAlignment.deriveBootstrapEvidenceDigest "pending-source proof")
            liveModeFor (_, shape) =
              if shape < 3 then Transfer.SourceLiveWithheld else Transfer.SourceLiveImmediate
            admit specification state =
              fst
                ( Transfer.commitSourceSubscription
                    ( checked
                        "pending-source admission or exact replay"
                        (Transfer.prepareSourceSubscription (subscribeFor specification) DomainAlignment.initialStoreRevision [] (liveModeFor specification) state)
                    )
                )
            append (identifier, _) state =
              fst
                ( Transfer.commitSourceChanges
                    ( checked
                        "pending-source contiguous progress"
                        (Transfer.prepareSourceChanges identifier [change identifier revision1 evidenceA] (Protocol.alignmentLive identifier revision1) state)
                    )
                )
            cancel reason (identifier, _) state =
              fst
                ( Transfer.commitAlignmentCancellation
                    ( checked
                        "pending-source cancellation"
                        (Transfer.prepareAlignmentCancellation (Protocol.alignmentCancel identifier reason) state)
                    )
                )
            release (identifier, _) state =
              fst
                ( Transfer.commitSourceLiveRelease
                    ( checked
                        "pending-source live release"
                        (Transfer.prepareSourceLiveRelease newGeneration destinationStore identifier state)
                    )
                )
            acknowledge (identifier, _) state =
              fst
                ( Transfer.commitSourceAcknowledgement
                    ( checked
                        "pending-source acknowledgement"
                        (Transfer.prepareSourceAcknowledgement (Protocol.alignmentAck identifier revision1) state)
                    )
                )
            pending = filter ((== 0) . snd) specifications
            retainReceipt state =
              fst
                ( Transfer.commitPredecessorInputClosureReceipt
                    ( checked
                        "pending-source empty local input closure"
                        (Transfer.preparePredecessorInputClosureReceipt newGeneration heraldA destinationStore revision1 [] state)
                    )
                )
            transitions =
              fmap admit specifications
                <> fmap admit (reverse specifications)
                <> fmap append specifications
                <> fmap (cancel Protocol.AlignmentSourceIncarnationLost) (take 1 pending)
                <> [ commitLocalPrefix,
                     retainReceipt
                   ]
                <> fmap release pending
                <> fmap acknowledge (filter (\(_, shape) -> shape == 0 || shape >= 3) specifications)
                <> fmap (cancel Protocol.AlignmentRelationRemoved) specifications
                <> fmap (cancel Protocol.AlignmentDestinationIncarnationLost) (reverse specifications)
                <> fmap admit specifications
            histories = scanl' (\state transition -> transition state) Transfer.emptyState transitions
            retainedIdentifiers = Set.fromList . fmap fst . Transfer.sourceSubscriptionEntries
            candidates = Set.fromList . Transfer.pendingPredecessorInputClosureSourceIds
         in QuickCheck.conjoin
              ( [ QuickCheck.counterexample
                    ("pending-source checked history " <> show index)
                    ( QuickCheck.conjoin
                        [ Transfer.validateAlignmentTransferState state QuickCheck.=== Right (),
                          Transfer.pendingPredecessorInputClosureSourceIds state QuickCheck.=== referencePendingPredecessorSources state,
                          closureIndexChecks state
                        ]
                    )
                | (index, state) <- zip [0 :: Int ..] histories
                ]
                  <> [ QuickCheck.counterexample
                         ("previously excluded source reentered at transition " <> show index)
                         (Set.intersection (retainedIdentifiers before Set.\\ candidates before) (candidates after) QuickCheck.=== Set.empty)
                     | (index, (before, after)) <- zip [0 :: Int ..] (zip histories (drop 1 histories))
                     ]
                  <> [ length (Transfer.pendingPredecessorInputClosureSourceIds (histories !! length specifications)) QuickCheck.=== length pending,
                       Transfer.pendingPredecessorInputClosureSourceIds (last histories) QuickCheck.=== []
                     ]
              )
  where
    identifiers =
      fmap
        (Protocol.alignmentSubscriptionId heraldA)
        (iterate Protocol.nextAlignmentSubscriptionSequence Protocol.firstAlignmentSubscriptionSequence)

referencePendingPredecessorSources :: Transfer.State -> [Protocol.AlignmentSubscriptionId]
referencePendingPredecessorSources state =
  [ identifier
  | (identifier, source) <- Transfer.sourceSubscriptionEntries state,
    Transfer.sourceSubscriptionCancellation source == Nothing,
    not (Transfer.sourceSubscriptionLiveReleased source),
    let subscribe = Transfer.sourceSubscriptionSubscribe source,
    Protocol.BootstrapProof generation _ <- [Protocol.alignmentSubscribeSourceProof subscribe],
    generation /= Protocol.alignmentObligationSourceGeneration (Protocol.alignmentSubscribeObligation subscribe)
  ]

-- The oracle enumerates retained transcripts, not any production index.
closureIndexChecks :: Transfer.State -> QuickCheck.Property
closureIndexChecks state =
  QuickCheck.conjoin
    ( [ Transfer.validateAlignmentTransferState state QuickCheck.=== Right (),
        Transfer.inputClosureSourceIds state QuickCheck.=== Set.fromList (referencePendingPredecessorSources state),
        QuickCheck.property (Transfer.pendingInputClosureSourceIds state `Set.isSubsetOf` Set.fromList (referencePendingPredecessorSources state))
      ]
        <> [Transfer.predecessorInputClosureCandidateEntries target state QuickCheck.=== referenceCandidates target state | target <- Set.toAscList targets]
        <> [Transfer.availableInputClosureDestination owner state QuickCheck.=== referenceAvailable owner state | owner <- Set.toAscList owners]
        <> [Transfer.completedInputClosureOwners generation state QuickCheck.=== referenceCompleted generation state | generation <- generations]
        <> [Transfer.completedInputClosureSubscription generation owner state QuickCheck.=== referenceWitness generation owner state | generation <- generations, owner <- Set.toAscList owners]
    )
  where
    destinations = Transfer.destinationSubscriptionEntries state
    barriers = Transfer.predecessorInputClosureEntries state
    targets =
      Set.fromList [(predecessorGeneration, destinationStore), (newGeneration, freshStore)]
        <> Set.unions [referenceDestinationTargets destination | (_, destination) <- destinations]
    owners = Set.fromList (fmap (Protocol.alignmentSubscribeObligationId . Transfer.destinationSubscriptionSubscribe . snd) destinations <> fmap (snd . fst) barriers)
    generations = [predecessorGeneration, newGeneration, secondChildGeneration]
    referenceAvailable owner retained = foldl select Nothing (Transfer.destinationSubscriptionEntries retained)
      where
        select selected pair@(_, destination)
          | Protocol.alignmentSubscribeObligationId (Transfer.destinationSubscriptionSubscribe destination) == owner && referenceDestinationAvailable destination = Just pair
          | otherwise = selected
    referenceCompleted generation retained =
      Set.fromList
        [owner | ((child, owner), (_, Just _)) <- Transfer.predecessorInputClosureEntries retained, child == generation]
    referenceWitness generation owner retained =
      listToMaybe
        [Protocol.alignmentSubscribeSubscriptionId subscribe | ((child, retainedOwner), (subscribe, Just _)) <- Transfer.predecessorInputClosureEntries retained, child == generation, retainedOwner == owner]

referenceDestinationTargets :: Transfer.DestinationSubscription -> Set.Set Transfer.InputClosureTarget
referenceDestinationTargets destination =
  Set.fromList
    [(Protocol.alignmentObligationDestinationGeneration obligation, Protocol.destinationStoreIncarnation store) | store <- NonEmpty.toList (Protocol.alignmentObligationDestinationStores obligation)]
  where
    obligation = Protocol.alignmentSubscribeObligation (Transfer.destinationSubscriptionSubscribe destination)

referenceDestinationAvailable :: Transfer.DestinationSubscription -> Bool
referenceDestinationAvailable destination = case Transfer.destinationSubscriptionCancellation destination of
  Nothing -> True
  Just cancellation ->
    Protocol.alignmentCancelReason cancellation == Protocol.AlignmentRelationRemoved
      && case (Transfer.destinationSubscriptionAppliedThrough destination, Transfer.destinationSubscriptionLiveThrough destination, Transfer.destinationSubscriptionAcknowledgedThrough destination) of
        (Just applied, Just live, Just acknowledged) -> applied >= live && acknowledged >= live
        _ -> False

referenceCandidates :: Transfer.InputClosureTarget -> Transfer.State -> [(Protocol.AlignmentSubscriptionId, Transfer.DestinationSubscription)]
referenceCandidates target state =
  [ pair
  | pair@(_, destination) <- Transfer.destinationSubscriptionEntries state,
    Transfer.destinationSubscriptionAttempt destination /= Nothing,
    Set.member target (referenceDestinationTargets destination),
    referenceDestinationAvailable destination
  ]

referenceClosureSourcesAt :: Set.Set Transfer.InputClosureTarget -> Transfer.State -> Set.Set Protocol.AlignmentSubscriptionId
referenceClosureSourcesAt targets state =
  Set.fromList
    [ identifier
    | (identifier, source) <- Transfer.sourceSubscriptionEntries state,
      identifier `elem` referencePendingPredecessorSources state,
      let subscribe = Transfer.sourceSubscriptionSubscribe source,
      Set.member (Protocol.alignmentObligationSourceGeneration (Protocol.alignmentSubscribeObligation subscribe), Protocol.alignmentSubscribeSourceStoreIncarnation subscribe) targets
    ]

admitClosureSource :: Protocol.AlignmentSubscriptionId -> DomainAlignment.ContextClassGenerationId -> DomainAlignment.ContextClassGenerationId -> Identity.StoreIncarnationId -> Transfer.SourceLiveMode -> Transfer.State -> Transfer.State
admitClosureSource identifier child predecessor incarnation mode state =
  fst
    ( Transfer.commitSourceSubscription
        ( checked
            "closure source fixture"
            ( Transfer.prepareSourceSubscription
                (bootstrapSubscribeFor identifier child predecessor incarnation (DomainAlignment.deriveBootstrapEvidenceDigest "selective closure source"))
                DomainAlignment.initialStoreRevision
                []
                mode
                state
            )
        )
    )

consumeAllInputClosures :: Transfer.State -> Transfer.State
consumeAllInputClosures state = foldl (flip Transfer.consumeInputClosureWork) state (referencePendingPredecessorSources state)

cancelClosureSubscription :: Protocol.AlignmentSubscriptionId -> Protocol.AlignmentCancelReason -> Transfer.State -> Transfer.State
cancelClosureSubscription identifier reason state =
  fst
    ( Transfer.commitAlignmentCancellation
        ( checked
            "closure cancellation"
            (Transfer.prepareAlignmentCancellation (Protocol.alignmentCancel identifier reason) state)
        )
    )

propInputClosureDestinationIndex :: QuickCheck.Property
propInputClosureDestinationIndex =
  QuickCheck.forAll (QuickCheck.chooseInt (1, 8)) $ \count ->
    QuickCheck.forAll (QuickCheck.vectorOf 50 ((,) <$> QuickCheck.chooseInt (0, count - 1) <*> QuickCheck.chooseInt (0, 6))) $ \schedule ->
      let attempts = [contextAttempt identifier predecessorGeneration predecessorStore | identifier <- take count sourcePrefixIdentifiers]
          sourceSpecifications =
            zip
              (take 4 (drop 20 sourcePrefixIdentifiers))
              [(newGeneration, predecessorGeneration, destinationStore), (secondChildGeneration, predecessorGeneration, destinationStore), (secondChildGeneration, newGeneration, destinationStore), (newGeneration, predecessorGeneration, freshStore)]
          sourceState = foldl (\state (identifier, (child, predecessor, incarnation)) -> admitClosureSource identifier child predecessor incarnation Transfer.SourceLiveWithheld state) Transfer.emptyState sourceSpecifications
          initial = consumeAllInputClosures (foldl (flip commitDestinationAttempt) sourceState attempts)
          step (state, pending) (offset, operation) =
            let attempt = attempts !! offset
                identifier = Protocol.alignmentAttemptSubscriptionId attempt
                target = (Protocol.alignmentObligationDestinationGeneration (Protocol.alignmentAttemptObligation attempt), destinationStore)
                successor = case operation of
                  0 -> commitDestinationAttempt attempt state
                  1 -> completeEmptyDestination attempt state
                  2 -> cancelClosureSubscription identifier Protocol.AlignmentSourceIncarnationLost state
                  3 -> cancelClosureSubscription identifier Protocol.AlignmentRelationRemoved state
                  4 -> cancelClosureSubscription identifier Protocol.AlignmentDestinationIncarnationLost state
                  5 -> consumeAllInputClosures state
                  _ -> Transfer.wakeInputClosureTargets (Set.singleton target) state
                changed = destinationProgress identifier state /= destinationProgress identifier successor
                notified = if operation == 6 || changed then referenceClosureSourcesAt (Set.singleton target) successor else Set.empty
                expected = if operation == 5 then Set.empty else pending <> notified
             in (successor, expected)
          histories = scanl' step (initial, Set.empty) schedule
       in QuickCheck.conjoin
            [ QuickCheck.counterexample
                ("closure destination history " <> show index)
                (QuickCheck.conjoin [closureIndexChecks state, Transfer.pendingInputClosureSourceIds state QuickCheck.=== pending])
            | (index, (state, pending)) <- zip [0 :: Int ..] histories
            ]
  where
    destinationProgress identifier state =
      fmap
        (\destination -> (Transfer.destinationSubscriptionAppliedThrough destination, Transfer.destinationSubscriptionLiveThrough destination, Transfer.destinationSubscriptionAcknowledgedThrough destination, Transfer.destinationSubscriptionCancellation destination))
        (Transfer.lookupDestinationSubscription identifier state)

caseInputClosureWorkCursor :: Assertion
caseInputClosureWorkCursor = do
  let first = sourcePrefixIdentifiers !! 20
      inserted = sourcePrefixIdentifiers !! 21
      later = sourcePrefixIdentifiers !! 22
      admit identifier = admitClosureSource identifier newGeneration predecessorGeneration destinationStore Transfer.SourceLiveWithheld
      initial = admit later (admit first Transfer.emptyState)
      entry = Transfer.consumeInputClosureWork later initial
      inventory = Transfer.inputClosureSourceIds entry
      loopback = Transfer.wakeInputClosureTargets (Set.singleton (predecessorGeneration, destinationStore)) (admit inserted (Transfer.consumeInputClosureWork first entry))
      afterLater = Transfer.consumeInputClosureWork later loopback
      membershipA = Membership.heraldMembershipGenerationId oneHeraldMembership
      membershipB = Membership.heraldMembershipGenerationId twoHeraldMembership
      seeded = Transfer.seedInputClosureMembership membershipA (consumeAllInputClosures afterLater)
      synchronized = Transfer.synchronizeInputClosureMembership membershipB seeded
  assertEqual "first eligible source starts this stage" (Just first) (Transfer.nextInputClosureWork inventory Nothing entry)
  assertEqual "existing later source wakes in the same pass" (Just later) (Transfer.nextInputClosureWork inventory (Just first) loopback)
  assertEqual "new and earlier identifiers wait for next pass" Nothing (Transfer.nextInputClosureWork inventory (Just later) afterLater)
  assertEqual "deferred keys retain their work" (Set.fromList [first, inserted]) (Transfer.pendingInputClosureSourceIds afterLater)
  assertEqual "equal membership performs no scheduling mutation" seeded (Transfer.synchronizeInputClosureMembership membershipA seeded)
  assertEqual "a new membership wakes all eligible sources" (Transfer.inputClosureSourceIds seeded) (Transfer.pendingInputClosureSourceIds synchronized)
  assertEqual "membership observation is retained" (Just membershipB) (Transfer.inputClosureMembership synchronized)
  assertEqual "normalization erases only work metadata" (Transfer.clearInputClosureWork seeded) (Transfer.clearInputClosureWork synchronized)
  assertEqual "normalization preserves source inventory" (Transfer.inputClosureSourceIds synchronized) (Transfer.inputClosureSourceIds (Transfer.clearInputClosureWork synchronized))
  assertEqual "cursor state remains checked" (Right ()) (Transfer.validateAlignmentTransferState afterLater)

caseInputClosureScopeNotifications :: Assertion
caseInputClosureScopeNotifications = do
  let first = sourcePrefixIdentifiers !! 20
      second = sourcePrefixIdentifiers !! 21
      otherStore = sourcePrefixIdentifiers !! 22
      released = sourcePrefixIdentifiers !! 23
      specs = [(first, newGeneration, destinationStore, Transfer.SourceLiveWithheld), (second, secondChildGeneration, destinationStore, Transfer.SourceLiveWithheld), (otherStore, newGeneration, freshStore, Transfer.SourceLiveWithheld), (released, newGeneration, destinationStore, Transfer.SourceLiveImmediate)]
      sources = consumeAllInputClosures (foldl (\state (identifier, child, incarnation, mode) -> admitClosureSource identifier child predecessorGeneration incarnation mode state) Transfer.emptyState specs)
      replayed = admitClosureSource first newGeneration predecessorGeneration destinationStore Transfer.SourceLiveWithheld sources
      marked = retainRouteMarkers [routeFromB] sources
      duplicateMarker = retainRouteMarkers [routeFromB] (consumeAllInputClosures marked)
      settled = commitLocalPrefixFor secondChildGeneration sources
      duplicateSettlement = commitLocalPrefixFor secondChildGeneration (consumeAllInputClosures settled)
      admitted = commitDestinationAttempt predecessorAttempt sources
      cleared = consumeAllInputClosures admitted
      held = fst (Transfer.commitDestinationWork (checked "hold unmatched prefix" (Transfer.prepareDestinationChange (change predecessorSubscription revision2 evidenceA) cleared)))
      bootstrap = commitDestinationBootstrap closureBootstrapSubscribe cleared
      bootstrapOwner = Protocol.alignmentSubscribeObligationId closureBootstrapSubscribe
      candidates = fmap fst (Transfer.predecessorInputClosureCandidateEntries (predecessorGeneration, destinationStore) bootstrap)
  assertEqual "exact source replay preserves consumed closure work" Set.empty (Transfer.pendingInputClosureSourceIds replayed)
  assertEqual "route marker wakes only eligible sources of its child" (Set.fromList [first, otherStore]) (Transfer.pendingInputClosureSourceIds marked)
  assertEqual "duplicate route marker creates no work" Set.empty (Transfer.pendingInputClosureSourceIds duplicateMarker)
  assertEqual "local settlement wakes only its child" (Set.singleton second) (Transfer.pendingInputClosureSourceIds settled)
  assertEqual "duplicate local settlement creates no work" Set.empty (Transfer.pendingInputClosureSourceIds duplicateSettlement)
  assertEqual "ordinary destination admission wakes its exact target" (Set.fromList [first, second]) (Transfer.pendingInputClosureSourceIds admitted)
  assertEqual "held bytes without destination prefix progress create no work" Set.empty (Transfer.pendingInputClosureSourceIds held)
  assertEqual "bootstrap admission also changes the all-purpose owner choice" (Set.fromList [first, second]) (Transfer.pendingInputClosureSourceIds bootstrap)
  assertEqual "bootstrap-purpose destination is absent from ordinary candidates" [predecessorSubscription] candidates
  assertEqual "bootstrap-purpose destination remains available to owner selection" (Just closureBootstrapSubscription) (fmap fst (Transfer.availableInputClosureDestination bootstrapOwner bootstrap))
  mapM_ (\state -> assertEqual "scoped notification invariant" (Right ()) (Transfer.validateAlignmentTransferState state)) [sources, replayed, marked, duplicateMarker, settled, duplicateSettlement, admitted, held, bootstrap]

caseInputClosureLoopbackCancellation :: Assertion
caseInputClosureLoopbackCancellation = do
  let identifier = sourcePrefixIdentifiers !! 20
      subscribe = bootstrapSubscribeFor identifier newGeneration predecessorGeneration destinationStore (DomainAlignment.deriveBootstrapEvidenceDigest "selective closure source")
      source = admitClosureSource identifier newGeneration predecessorGeneration destinationStore Transfer.SourceLiveWithheld Transfer.emptyState
      both = commitDestinationBootstrap subscribe source
      cancelled = cancelClosureSubscription identifier Protocol.AlignmentSourceIncarnationLost both
      owner = Protocol.alignmentSubscribeObligationId subscribe
      woken = Transfer.wakeInputClosureTargets (Set.singleton (predecessorGeneration, destinationStore)) (Transfer.wakeInputClosureGenerations (Set.singleton newGeneration) cancelled)
  assertBool "loopback fixture contains both roles" (Transfer.lookupSourceSubscription identifier both /= Nothing && Transfer.lookupDestinationSubscription identifier both /= Nothing)
  assertEqual "source cancellation removes its executable inventory" Set.empty (Transfer.inputClosureSourceIds cancelled)
  assertEqual "destination cancellation removes owner availability" Nothing (Transfer.availableInputClosureDestination owner cancelled)
  assertEqual "retired source buckets cannot produce later work" Set.empty (Transfer.pendingInputClosureSourceIds woken)
  assertEqual "both role indices remain checked" (Right ()) (Transfer.validateAlignmentTransferState cancelled)

retainClosureBarrier :: DomainAlignment.ContextClassGenerationId -> Protocol.AlignmentSubscriptionId -> Transfer.State -> Transfer.State
retainClosureBarrier generation identifier state =
  fst
    ( Transfer.commitPredecessorInputClosures
        ( checked
            "retain test barrier"
            (Transfer.preparePredecessorInputClosures generation (Set.singleton identifier) state)
        )
    )

observeClosureLive :: Protocol.AlignmentSubscriptionId -> DomainAlignment.StoreRevision -> Transfer.State -> Transfer.State
observeClosureLive identifier revision state =
  fst
    ( Transfer.commitDestinationWork
        ( checked
            "observe closure Live"
            (Transfer.prepareDestinationLive (Protocol.alignmentLive identifier revision) state)
        )
    )

caseInputClosureWitnessWake :: Assertion
caseInputClosureWitnessWake = do
  let first = sourcePrefixIdentifiers !! 20
      unrelated = sourcePrefixIdentifiers !! 21
      sources =
        admitClosureSource
          unrelated
          secondChildGeneration
          predecessorGeneration
          destinationStore
          Transfer.SourceLiveWithheld
          (admitClosureSource first newGeneration predecessorGeneration destinationStore Transfer.SourceLiveWithheld Transfer.emptyState)
      complete = completeEmptyDestination predecessorAttempt (commitDestinationAttempt predecessorAttempt sources)
      retained = consumeAllInputClosures (retainClosureBarrier newGeneration predecessorSubscription complete)
      owner = Protocol.alignmentAttemptObligationId predecessorAttempt
      witnessed = observeClosureLive predecessorSubscription DomainAlignment.initialStoreRevision retained
      consumed = consumeAllInputClosures witnessed
      duplicate = observeClosureLive predecessorSubscription DomainAlignment.initialStoreRevision consumed
      advanced = observeClosureLive predecessorSubscription revision1 consumed
      ahead = observeClosureLive predecessorSubscription revision1 (consumeAllInputClosures advanced)
      retainedSecond = consumeAllInputClosures (retainClosureBarrier secondChildGeneration predecessorSubscription ahead)
      secondWitnessed = observeClosureLive predecessorSubscription revision1 retainedSecond
  assertEqual "retained barrier has no implicit witness from old Live" Nothing (Transfer.completedInputClosureSubscription newGeneration owner retained)
  assertEqual "equal old Live wakes only the newly witnessed child" (Set.singleton first) (Transfer.pendingInputClosureSourceIds witnessed)
  assertEqual "the witnessed owner is indexed" (Set.singleton owner) (Transfer.completedInputClosureOwners newGeneration witnessed)
  assertEqual "the exact next replay adds no work" Set.empty (Transfer.pendingInputClosureSourceIds duplicate)
  assertEqual "a larger witnessed bound wakes work again" (Set.fromList [first, unrelated]) (Transfer.pendingInputClosureSourceIds advanced)
  assertEqual "already advanced Live witnesses a second new child independently" (Set.singleton unrelated) (Transfer.pendingInputClosureSourceIds secondWitnessed)
  assertEqual "all reverse-barrier projections remain checked" (Right ()) (Transfer.validateAlignmentTransferState secondWitnessed)

caseInputClosureRetainedSources :: Assertion
caseInputClosureRetainedSources = do
  let first = sourcePrefixIdentifiers !! 20
      second = sourcePrefixIdentifiers !! 21
      admit identifier = admitClosureSource identifier newGeneration predecessorGeneration destinationStore Transfer.SourceLiveImmediate
      initial = retainClosureBarrier newGeneration predecessorSubscription (commitDestinationAttempt predecessorAttempt (admit first Transfer.emptyState))
      owner = Protocol.alignmentAttemptObligationId predecessorAttempt
      shared = admit second initial
      afterFirst = cancelClosureSubscription first Protocol.AlignmentSourceIncarnationLost shared
      afterBoth = cancelClosureSubscription second Protocol.AlignmentSourceIncarnationLost afterFirst
  assertEqual "immediately-live sources have no executable closure work" Set.empty (Transfer.inputClosureSourceIds initial)
  assertBool "one immediately-live source still justifies the barrier" (Transfer.lookupPredecessorInputClosure newGeneration owner initial /= Nothing)
  assertBool "another uncancelled relation keeps the shared barrier" (Transfer.lookupPredecessorInputClosure newGeneration owner afterFirst /= Nothing)
  assertEqual "last cancellation removes the executable barrier" Nothing (Transfer.lookupPredecessorInputClosure newGeneration owner afterBoth)
  assertEqual "empty buckets are removed by exact index validation" (Right ()) (Transfer.validateAlignmentTransferState afterBoth)

caseInputClosureBarrierLifecycle :: Assertion
caseInputClosureBarrierLifecycle = do
  let source = sourcePrefixIdentifiers !! 20
      laterId = sourcePrefixIdentifiers !! 10
      laterAttempt = contextAttempt laterId predecessorGeneration predecessorStore
      owner = Protocol.alignmentAttemptObligationId predecessorAttempt
      admitted = admitClosureSource source newGeneration predecessorGeneration destinationStore Transfer.SourceLiveWithheld Transfer.emptyState
      first = retainClosureBarrier newGeneration predecessorSubscription (commitDestinationAttempt predecessorAttempt admitted)
      lost = cancelClosureSubscription predecessorSubscription Protocol.AlignmentSourceIncarnationLost first
      replaced = retainClosureBarrier newGeneration laterId (commitDestinationAttempt laterAttempt lost)
      witnessed = completeEmptyDestination laterAttempt replaced
      terminal = cancelClosureSubscription laterId Protocol.AlignmentSourceIncarnationLost witnessed
      reusable = cancelClosureSubscription laterId Protocol.AlignmentRelationRemoved terminal
      removed = cancelClosureSubscription laterId Protocol.AlignmentDestinationIncarnationLost reusable
      replayedBarrier = retainClosureBarrier newGeneration predecessorSubscription witnessed
      withoutReceipt = fst (Transfer.commitIncompletePredecessorInputClosureRemoval (checked "remove witnessed barrier without receipt" (Transfer.prepareIncompletePredecessorInputClosureRemoval (Set.singleton owner) witnessed)))
      exactPrefix = Transfer.memberReadinessPrefix laterId DomainAlignment.initialStoreRevision DomainAlignment.initialStoreRevision DomainAlignment.initialStoreRevision
      withReceipt = fst (Transfer.commitPredecessorInputClosureReceipt (checked "retain closure receipt" (Transfer.preparePredecessorInputClosureReceipt newGeneration heraldA destinationStore DomainAlignment.initialStoreRevision [exactPrefix] (commitLocalPrefix witnessed))))
      released = fst (Transfer.commitSourceLiveRelease (checked "release predecessor source" (Transfer.prepareSourceLiveRelease newGeneration destinationStore source withReceipt)))
      cancelled = cancelClosureSubscription source Protocol.AlignmentSourceIncarnationLost released
      historical = fst (Transfer.commitIncompletePredecessorInputClosureRemoval (checked "preserve receipt barrier" (Transfer.prepareIncompletePredecessorInputClosureRemoval (Set.singleton owner) cancelled)))
  assertEqual "source-loss removes availability" Nothing (Transfer.availableInputClosureDestination owner terminal)
  assertEqual "stronger relation removal re-adds a covered destination" (Just laterId) (fmap fst (Transfer.availableInputClosureDestination owner reusable))
  assertEqual "destination loss removes it again" Nothing (Transfer.availableInputClosureDestination owner removed)
  assertEqual "completed barrier wins over a supplied old candidate" (Just laterId) (Transfer.completedInputClosureSubscription newGeneration owner replayedBarrier)
  assertEqual "witnessed barrier without whole receipt is removable" Nothing (Transfer.lookupPredecessorInputClosure newGeneration owner withoutReceipt)
  assertEqual "Live release retires scheduling work" Set.empty (Transfer.inputClosureSourceIds released)
  assertEqual "receipt retains barrier after source cancellation and plan removal" (Just laterId) (Transfer.completedInputClosureSubscription newGeneration owner historical)
  mapM_ (\state -> assertEqual "barrier lifecycle index invariant" (Right ()) (Transfer.validateAlignmentTransferState state)) [first, lost, replaced, witnessed, terminal, reusable, removed, withoutReceipt, withReceipt, released, cancelled, historical]

-- Compare the indexed catalogue and all four loss families with the frozen
-- historical scan on generated checked histories. Old generations deliberately
-- keep unfinished imports, and invalidation/remote-loss records are exercised.
propLatestGenerationCatalogue :: QuickCheck.Property
propLatestGenerationCatalogue =
  QuickCheck.forAll (QuickCheck.chooseInt (1, 12)) $ \count ->
    QuickCheck.forAll (QuickCheck.vectorOf count (QuickCheck.chooseInt (0, 2))) $ \shapes ->
      QuickCheck.ioProperty $ do
        let revisions = take count (iterate DomainAlignment.nextPlacementRevision DomainAlignment.firstPlacementRevision)
            placements =
              [ checked
                  "indexed catalogue placement"
                  (DomainAlignment.physicalPlacementRevisionVector twoHeraldMembership [(heraldA, revision), (heraldB, revision)])
              | revision <- revisions
              ]
            selectedContext shape = case shape of
              0 -> singletonContext
              1 -> splitContext
              _ -> mergedContext
            selectedMembers shape = if shape == 0 then [oldMember] else [oldMember, memberBRemote]
            occurrences =
              [ Identity.structuralOccurrenceId
                  heraldA
                  (checked "indexed catalogue occurrence" (Identity.mkStructuralSequence (fromIntegral ordinal)))
              | ordinal <- [1 .. count]
              ]
        plans <-
          sequence
            [ readyPlanAt twoHeraldTopology placement (selectedContext shape) (selectedMembers shape) [] Set.empty
            | (placement, shape) <- zip placements shapes
            ]
        let append owner (occurrence, placement, plan) =
              commitPromotionAt heraldA occurrence twoHeraldTopologyId placement plan (retainDebt occurrence owner)
            histories = scanl' append Alignment.emptyState (zip3 occurrences placements plans)
            current = last histories
            withRemoteLoss =
              fst
                ( Alignment.commitUnavailableAlignmentSourceRetention
                    ( Alignment.prepareUnavailableAlignmentSourceRetention
                        (Alignment.unavailableAlignmentSource heraldB deltaB storeB)
                        current
                    )
                )
            invalidated =
              [ fst
                  ( Alignment.commitAlignmentPlanInvalidation
                      ( checked
                          "indexed catalogue historical invalidation"
                          ( Alignment.prepareAlignmentPlanInvalidation
                              (Alignment.AlignmentPlanDestinationStoreLost identifier (referenceDestinationFor member))
                              withRemoteLoss
                          )
                      )
                  )
              | (identifier, generation) <- Alignment.alignmentGenerationEntries withRemoteLoss,
                let member = NonEmpty.head (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation))
              ]
            states = histories <> [withRemoteLoss] <> invalidated
            active mask _ destination =
              (mask `mod` 2 == 1 && Protocol.destinationStoreDelta destination == deltaA)
                || (mask >= 2 && Protocol.destinationStoreDelta destination == deltaB)
        pure
          ( QuickCheck.conjoin
              [ QuickCheck.counterexample
                  ("current catalogue state " <> show index)
                  ( QuickCheck.conjoin
                      ( [ Alignment.validateAlignmentState owner QuickCheck.=== Right (),
                          Alignment.latestAlignmentGenerationEntries owner QuickCheck.=== referenceLatestGenerationEntries owner,
                          Alignment.pendingAlignmentMemberProgress heraldA owner QuickCheck.=== referencePendingMemberProgress heraldA owner,
                          Alignment.pendingAlignmentMemberProgress heraldB owner QuickCheck.=== referencePendingMemberProgress heraldB owner,
                          Alignment.pendingAlignmentRouteCutoverEntries heraldA (fst (Alignment.retireCompletedAlignmentRouteCutovers heraldA owner))
                            QuickCheck.=== referencePendingRouteEntries heraldA owner,
                          Alignment.pendingAlignmentRouteCutoverEntries heraldB (fst (Alignment.retireCompletedAlignmentRouteCutovers heraldB owner))
                            QuickCheck.=== referencePendingRouteEntries heraldB owner
                        ]
                          <> [ AlignmentTransferCoordinator.alignmentPlanLossCauses heraldA (active mask) owner
                                 QuickCheck.=== referenceAlignmentPlanLossCauses heraldA (active mask) owner
                             | mask <- [0 :: Int .. 3]
                             ]
                      )
                  )
              | (index, owner) <- zip [0 :: Int ..] states
              ]
          )

-- Original current-generation filter; intentionally scans the full retained
-- catalogue and uses the existing per-sort accessor rather than the new one.
referenceLatestGenerationEntries ::
  Alignment.State -> [(DomainAlignment.ContextClassGenerationId, AlignmentGeneration)]
referenceLatestGenerationEntries owner =
  [ entry
  | entry@(_, generation) <- Alignment.alignmentGenerationEntries owner,
    let cut = alignmentGenerationCut generation,
    let affectedSort = sortOccurrence (DomainAlignment.alignmentCutSortId cut) (DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut),
    any
      ((== alignmentGenerationId generation) . alignmentGenerationId)
      (Alignment.latestAlignmentGenerationsForSort affectedSort owner)
  ]

referenceAlignmentPlanLossCauses ::
  Identity.HeraldEpoch ->
  (SortOccurrence -> Protocol.DestinationStore -> Bool) ->
  Alignment.State ->
  Set.Set Alignment.AlignmentPlanInvalidationCause
referenceAlignmentPlanLossCauses local destinationIsActive alignment =
  Set.fromList
    ( ordinaryDestinationLosses
        <> bootstrapDestinationLosses
        <> currentGenerationDestinationLosses
        <> bootstrapFreshBaseSourceLosses
    )
  where
    ordinaryDestinationLosses =
      [ Alignment.AlignmentPlanDestinationStoreLost generation lostDestination
      | (_, obligation) <- Alignment.activeAlignmentObligationEntries alignment,
        let generation = Protocol.alignmentObligationDestinationGeneration obligation,
        let affectedSort =
              sortOccurrence
                (Protocol.alignmentObligationSortId obligation)
                (Protocol.alignmentObligationSortDefinitionOccurrenceId obligation),
        lostDestination <- NonEmpty.toList (Protocol.alignmentObligationDestinationStores obligation),
        not (destinationIsActive affectedSort lostDestination)
      ]

    bootstrapDestinationLosses =
      [ Alignment.AlignmentPlanDestinationStoreLost generation lostDestination
      | (_, bootstrapImport) <- Alignment.bootstrapImportEntries alignment,
        let key = Alignment.bootstrapImportKeyValue bootstrapImport,
        let generation = Alignment.bootstrapImportKeyGeneration key,
        let lostDestination = Alignment.bootstrapImportKeyDestination key,
        let obligation = Alignment.bootstrapImportSubscribeEnvelope bootstrapImport,
        let affectedSort =
              sortOccurrence
                (Protocol.alignmentObligationSortId obligation)
                (Protocol.alignmentObligationSortDefinitionOccurrenceId obligation),
        not (destinationIsActive affectedSort lostDestination)
      ]

    -- The latest generation set still owns its physical members after one-time
    -- imports finish. Retained exact loss also applies to foreign members and
    -- to a historical anchor admitted after its placement loss was observed.
    -- Superseded generations disappear from this set and remain historical.
    currentGenerationDestinationLosses =
      [ Alignment.AlignmentPlanDestinationStoreLost
          (alignmentGenerationId generation)
          (referenceDestinationFor member)
      | (_, generation) <- Alignment.alignmentGenerationEntries alignment,
        generationIsCurrent generation,
        let affectedSort = generationSort generation,
        member <- NonEmpty.toList (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation)),
        ( DomainAlignment.alignmentMemberHerald member == local
            && not (destinationIsActive affectedSort (referenceDestinationFor member))
        )
          || Set.member
            ( Alignment.unavailableAlignmentSource
                (DomainAlignment.alignmentMemberHerald member)
                (DomainAlignment.alignmentMemberDelta member)
                (DomainAlignment.alignmentMemberStoreIncarnation member)
            )
            (Alignment.alignmentUnavailableSources alignment)
      ]

    bootstrapFreshBaseSourceLosses =
      [ Alignment.AlignmentPlanFreshBaseStoreLost generation fresh source
      | (_, bootstrapImport) <- Alignment.bootstrapImportEntries alignment,
        let key = Alignment.bootstrapImportKeyValue bootstrapImport,
        let generation = Alignment.bootstrapImportKeyGeneration key,
        Alignment.BootstrapFromFreshBase fresh <- [Alignment.bootstrapImportKeySource key],
        Just retainedGeneration <-
          [Alignment.lookupAlignmentGeneration generation alignment],
        let affectedSort = generationSort retainedGeneration,
        sourceMember <-
          NonEmpty.toList
            (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut retainedGeneration)),
        DomainAlignment.alignmentMemberDelta sourceMember == DomainAlignment.freshMemberBaseDelta fresh,
        DomainAlignment.alignmentMemberStoreIncarnation sourceMember == DomainAlignment.freshMemberBaseStoreIncarnation fresh,
        let sourceHerald = DomainAlignment.alignmentMemberHerald sourceMember,
        sourceHerald == local,
        not (destinationIsActive affectedSort (referenceDestinationFor sourceMember)),
        let source =
              Alignment.alignmentSourceCoordinate
                sourceHerald
                (DomainAlignment.freshMemberBaseStoreIncarnation fresh)
      ]

    generationIsCurrent generation =
      any
        ((== alignmentGenerationId generation) . alignmentGenerationId)
        ( Alignment.latestAlignmentGenerationsForSort
            (generationSort generation)
            alignment
        )

    generationSort generation =
      let cut = alignmentGenerationCut generation
       in sortOccurrence
            (DomainAlignment.alignmentCutSortId cut)
            (DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut)

referenceDestinationFor :: DomainAlignment.AlignmentMember -> Protocol.DestinationStore
referenceDestinationFor member =
  Protocol.destinationStore
    (DomainAlignment.alignmentMemberDelta member)
    (DomainAlignment.alignmentMemberStoreIncarnation member)

-- Independent historical projections for operational work: these intentionally
-- ignore both new candidate indexes and the current-generation catalogue.
referencePendingMemberProgress ::
  Identity.HeraldEpoch -> Alignment.State -> [(AlignmentGeneration, DomainAlignment.AlignmentMember)]
referencePendingMemberProgress local owner =
  [ (generation, member)
  | (_, generation) <- Alignment.alignmentGenerationEntries owner,
    not (referenceGenerationInvalidated generation owner),
    member <- NonEmpty.toList (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation)),
    DomainAlignment.alignmentMemberHerald member == local,
    let identifier = alignmentGenerationId generation,
    let store = DomainAlignment.alignmentMemberStoreIncarnation member,
    not
      ( Alignment.lookupAlignmentLocalMemberReadiness identifier store owner /= Nothing
          && Alignment.lookupAlignmentHistoricalCertificate identifier store owner /= Nothing
      )
  ]

referencePendingRouteEntries ::
  Identity.HeraldEpoch -> Alignment.State -> [(DomainAlignment.ContextClassGenerationId, AlignmentGeneration)]
referencePendingRouteEntries source owner =
  [ entry
  | entry@(identifier, generation) <- Alignment.alignmentGenerationEntries owner,
    not (referenceGenerationInvalidated generation owner),
    let cut = alignmentGenerationCut generation,
    let predecessors = DomainAlignment.alignmentCutPredecessorGenerationIds cut,
    not (null predecessors),
    source `elem` fmap fst (NonEmpty.toList (DomainAlignment.physicalPlacementRevisionEntries (DomainAlignment.alignmentCutPhysicalPlacementRevisionVector cut))),
    not (all (predecessorCompleted identifier) predecessors)
  ]
  where
    transfer = Alignment.alignmentTransferState owner
    predecessorCompleted identifier predecessor =
      case Alignment.lookupAlignmentGeneration predecessor owner of
        Nothing -> False
        Just generation ->
          all
            (hostCompleted identifier . DomainAlignment.alignmentMemberHerald)
            (NonEmpty.toList (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation)))
    hostCompleted identifier host
      | source == host = Transfer.localRoutePrefixSettled identifier host transfer
      | otherwise = Transfer.lookupRouteCutover identifier source host transfer /= Nothing

referenceGenerationInvalidated :: AlignmentGeneration -> Alignment.State -> Bool
referenceGenerationInvalidated generation owner =
  let cut = alignmentGenerationCut generation
   in Alignment.alignmentPlanIsInvalidated
        (sortOccurrence (DomainAlignment.alignmentCutSortId cut) (DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut))
        (Topology.deriveTopologyCutId (DomainAlignment.alignmentCutTopologyCut cut))
        (DomainAlignment.alignmentCutPhysicalPlacementRevisionVector cut)
        owner

casePendingGenerationCandidates :: Assertion
casePendingGenerationCandidates = do
  (_, generation, _, _, fulfilled) <- fulfilledSingletonFixture
  let identifier = alignmentGenerationId generation
      localReady =
        checked
          "pending catalogue local readiness"
          (Alignment.prepareLocalClassMemberReady identifier storeA readyDigestA DomainAlignment.initialStoreRevision fulfilled)
      ready = Alignment.preparedLocalClassMemberReady localReady
      (withReady, _) = Alignment.commitLocalClassMemberReady localReady
      certificate =
        Protocol.historicalCertificate
          identifier
          storeA
          (Protocol.classMemberReadySetDigest [ready])
          (DomainAlignment.deriveBootstrapEvidenceDigest (DomainAlignment.memberReadyEvidenceDigestBytes readyDigestA))
          DomainAlignment.initialStoreRevision
      certified = retainHistoricalCertificate certificate withReady
  assertEqual
    "readiness alone leaves certification work"
    1
    (length (Alignment.pendingAlignmentMemberProgress heraldA withReady))
  assertEqual
    "both immutable facts retire the completed member"
    []
    (Alignment.pendingAlignmentMemberProgress heraldA certified)
  assertEqual
    "retirement retains the historical generation"
    (Just generation)
    (Alignment.lookupAlignmentGeneration identifier certified)
  assertEqual
    "certificate replay cannot reinsert a completed member"
    certified
    (retainHistoricalCertificate certificate certified)
  successorPlan <- readyPlanAt topology nextOneHeraldPlacement singletonContext [oldMember] [generation] (Set.singleton identifier)
  successorGeneration <- only "pending successor generation" (alignmentGenerationPlanGenerations successorPlan)
  let successorId = alignmentGenerationId successorGeneration
      successor = commitPromotionAt heraldA occurrenceB topologyId nextOneHeraldPlacement successorPlan (retainDebt occurrenceB certified)
      (stillPending, pendingChanged) = Alignment.retireCompletedAlignmentRouteCutovers heraldA successor
      preparedSettlement =
        checked
          "pending catalogue local settlement"
          (Transfer.prepareLocalRoutePrefixSettlement successorId heraldA (Alignment.alignmentTransferState stillPending))
      (settledTransfer, _) = Transfer.commitLocalRoutePrefixSettlement preparedSettlement
      withSettlement = Alignment.replaceAlignmentTransferState settledTransfer stillPending
      (retired, retiredChanged) = Alignment.retireCompletedAlignmentRouteCutovers heraldA withSettlement
      invalidated =
        fst
          ( Alignment.commitAlignmentPlanInvalidation
              ( checked
                  "pending successor invalidation"
                  ( Alignment.prepareAlignmentPlanInvalidation
                      (Alignment.AlignmentPlanDestinationStoreLost successorId (Protocol.destinationStore deltaA storeA))
                      retired
                  )
              )
          )
  assertEqual
    "superseded generations keep their exact lookup"
    (Just generation)
    (Alignment.lookupAlignmentGeneration identifier successor)
  assertEqual "unfinished cutover retirement reports no change" False pendingChanged
  assertEqual "completed cutover retirement reports change" True retiredChanged
  assertEqual
    "unfinished closure remains a candidate"
    [successorId]
    (fmap fst (Alignment.pendingAlignmentRouteCutoverEntries heraldA stillPending))
  assertEqual
    "local settlement retires cutover work"
    []
    (Alignment.pendingAlignmentRouteCutoverEntries heraldA retired)
  assertEqual
    "cutover retirement is idempotent and reports no change"
    (retired, False)
    (Alignment.retireCompletedAlignmentRouteCutovers heraldA retired)
  assertBool
    "retiring cutover does not finish the child's import"
    (not (null (Alignment.bootstrapImportEntries retired)))
  assertEqual
    "child readiness remains pending after predecessor settlement"
    [successorId]
    (fmap (alignmentGenerationId . fst) (Alignment.pendingAlignmentMemberProgress heraldA retired))
  assertEqual
    "invalidation retires the child's remaining progress candidate"
    []
    (Alignment.pendingAlignmentMemberProgress heraldA invalidated)
  assertEqual
    "the complete historical generation catalogue is retained"
    2
    (length (Alignment.alignmentGenerationEntries invalidated))
  mapM_
    ( \owner -> do
        let (afterRetirement, retirementChanged) = Alignment.retireCompletedAlignmentRouteCutovers heraldA owner
        assertEqual
          "the retirement witness exactly matches owner change"
          (afterRetirement /= owner)
          retirementChanged
        assertEqual "pending index owner invariant" (Right ()) (Alignment.validateAlignmentState owner)
        assertEqual
          "pending member projection matches the historical reference"
          (referencePendingMemberProgress heraldA owner)
          (Alignment.pendingAlignmentMemberProgress heraldA owner)
        assertEqual
          "retired cutover projection matches the historical reference"
          (referencePendingRouteEntries heraldA owner)
          (Alignment.pendingAlignmentRouteCutoverEntries heraldA (fst (Alignment.retireCompletedAlignmentRouteCutovers heraldA owner)))
    )
    [fulfilled, withReady, certified, successor, stillPending, withSettlement, retired, invalidated]

propLiveDebtClassifier :: QuickCheck.Property
propLiveDebtClassifier =
  QuickCheck.forAll (QuickCheck.chooseInt (1, 16)) $ \count ->
    QuickCheck.forAll (QuickCheck.vectorOf (count - 1) QuickCheck.arbitrary) $ \remainingPromotions ->
      let occurrences = fmap fixtureOccurrence [1 .. count]
       in QuickCheck.forAll (QuickCheck.shuffle (zip occurrences (True : remainingPromotions))) $ \promotionOrder ->
            QuickCheck.ioProperty $ do
              plan <- readyPlan singletonContext [oldMember] [] Set.empty
              replacementPlan <- readyPlanAt topology nextOneHeraldPlacement singletonContext [newMember] [] Set.empty
              generation <- only "classifier promotion generation" (alignmentGenerationPlanGenerations plan)
              let withDebts = foldl' (flip retainDebt) Alignment.emptyState occurrences
                  promotedStates =
                    scanl'
                      (\owner (occurrence, selected) -> if selected then commitPromotion occurrence plan owner else owner)
                      withDebts
                      promotionOrder
                  promoted = foldl' (\_ owner -> owner) withDebts promotedStates
                  invalidated =
                    fst
                      ( Alignment.commitAlignmentPlanInvalidation
                          ( checked
                              "invalidate generated promotion history"
                              ( Alignment.prepareAlignmentPlanInvalidation
                                  ( Alignment.AlignmentPlanDestinationStoreLost
                                      (alignmentGenerationId generation)
                                      (Protocol.destinationStore deltaA storeA)
                                  )
                                  promoted
                              )
                          )
                      )
                  recoveredStates =
                    scanl'
                      (\owner occurrence -> commitPromotionAt heraldA occurrence topologyId nextOneHeraldPlacement replacementPlan owner)
                      invalidated
                      occurrences
                  recovered = foldl' (\_ owner -> owner) invalidated recoveredStates
                  states = promotedStates <> recoveredStates
                  liveCount = length . Alignment.liveAlignmentStructuralDebtEntries
              pure
                ( QuickCheck.conjoin
                    ( [ QuickCheck.counterexample
                          ("classifier transition " <> show index)
                          (Alignment.liveAlignmentStructuralDebtEntries owner QuickCheck.=== referenceLiveDebts owner)
                      | (index, owner) <- zip [0 :: Int ..] states
                      ]
                        <> [ liveCount withDebts QuickCheck.=== count,
                             liveCount promoted QuickCheck.=== length (filter (not . snd) promotionOrder),
                             liveCount invalidated QuickCheck.=== count,
                             liveCount recovered QuickCheck.=== 0,
                             Alignment.validateAlignmentState recovered QuickCheck.=== Right ()
                           ]
                    )
                )
  where
    fixtureOccurrence ordinal =
      Identity.structuralOccurrenceId
        heraldA
        (checked "classifier occurrence" (Identity.mkStructuralSequence (fromIntegral ordinal)))

-- Preserve the previous per-debt definition as an independent equivalence
-- oracle; production now shares one union across all debt queries in this pass.
referenceLiveDebts :: Alignment.State -> [StructuralConsequenceDebt]
referenceLiveDebts owner =
  filter
    (not . covered . structuralConsequenceDebtKey)
    (structuralDebtSetEntries (Alignment.alignmentStructuralDebts owner))
  where
    covered debtKey =
      any
        ( \(key, receipt) ->
            Set.member debtKey (Alignment.alignmentPromotionReceiptDebtKeys receipt)
              && not
                ( Alignment.alignmentPlanIsInvalidated
                    (Alignment.alignmentPromotionKeySort key)
                    (Alignment.alignmentPromotionKeyTopologyCut key)
                    (Alignment.alignmentPromotionKeyPlacementVector key)
                    owner
                )
        )
        (Alignment.alignmentPromotionEntries owner)

propBootstrapAttemptLookup :: QuickCheck.Property
propBootstrapAttemptLookup = QuickCheck.ioProperty $ do
  plan <- readyPlan mergedContext allMembers [] Set.empty
  let generations = alignmentGenerationPlanGenerations plan
      promoted = promote occurrenceA plan
      accepted = foldl' (flip retainCutAcceptance) promoted generations
      keys = fmap fst (Alignment.bootstrapImportEntries accepted)
  generation <- generationHosting "lookup invalidation generation" storeA generations
  pure
    $ QuickCheck.forAll (QuickCheck.shuffle keys)
    $ \order ->
      let attempts = scanl' createBootstrapAttempt accepted order
          attempted = foldl' (\_ owner -> owner) accepted attempts
          invalidated =
            fst
              ( Alignment.commitAlignmentPlanInvalidation
                  ( checked
                      "invalidate generated bootstrap attempts"
                      ( Alignment.prepareAlignmentPlanInvalidation
                          (Alignment.AlignmentPlanDestinationStoreLost (alignmentGenerationId generation) (Protocol.destinationStore deltaA storeA))
                          attempted
                      )
                  )
              )
          states = Alignment.emptyState : promoted : attempts <> [invalidated]
       in QuickCheck.conjoin
            ( (not (null keys) QuickCheck.=== True)
                : [ QuickCheck.counterexample
                      ("bootstrap query transition " <> show index)
                      (Alignment.lookupBootstrapImportAttempt key owner QuickCheck.=== lookup key (Alignment.bootstrapImportAttemptEntries owner))
                  | (index, owner) <- zip [0 :: Int ..] states,
                    key <- keys
                  ]
                  <> [ Alignment.bootstrapImportAttemptEntries invalidated QuickCheck.=== [],
                       Alignment.validateAlignmentState invalidated QuickCheck.=== Right ()
                     ]
            )

caseSplitBootstrap :: Assertion
caseSplitBootstrap = do
  prior <- readyPlan mergedContext allMembers [] Set.empty
  let priorGenerations = alignmentGenerationPlanGenerations prior
      predecessorIds = Set.fromList (fmap alignmentGenerationId priorGenerations)
  assertEqual "merged predecessor" 1 (length priorGenerations)
  split <- readyPlan splitContext allMembers priorGenerations predecessorIds
  let generations = alignmentGenerationPlanGenerations split
      bootstrap = Alignment.bootstrapImportEntries (promote occurrenceA split)
  assertEqual "two split generations" 2 (length generations)
  assertEqual "one predecessor import per child member" 2 (length bootstrap)
  assertBool
    "every split import names the sole predecessor"
    ( all
        ( \(key, _) -> case Alignment.bootstrapImportKeySource key of
            Alignment.BootstrapFromPredecessor predecessor ->
              Set.member predecessor predecessorIds
            Alignment.BootstrapFromFreshBase _ -> False
        )
        bootstrap
    )

caseMergeBootstrap :: Assertion
caseMergeBootstrap = do
  prior <- readyPlan splitContext allMembers [] Set.empty
  let priorGenerations = alignmentGenerationPlanGenerations prior
      predecessorIds = Set.fromList (fmap alignmentGenerationId priorGenerations)
  assertEqual "two split predecessors" 2 (length priorGenerations)
  merged <- readyPlan mergedContext allMembers priorGenerations predecessorIds
  let generations = alignmentGenerationPlanGenerations merged
      bootstrap = Alignment.bootstrapImportEntries (promote occurrenceA merged)
  assertEqual "one merged generation" 1 (length generations)
  assertEqual "two members import both predecessors" 4 (length bootstrap)
  assertEqual
    "the Cartesian source set is exact"
    predecessorIds
    ( Set.fromList
        [ predecessor
        | (key, _) <- bootstrap,
          Alignment.BootstrapFromPredecessor predecessor <-
            [Alignment.bootstrapImportKeySource key]
        ]
    )

casePassivateReactivate :: Assertion
casePassivateReactivate = do
  active <- readyPlan singletonContext [oldMember] [] Set.empty
  let oldGenerations = alignmentGenerationPlanGenerations active
      oldIds = Set.fromList (fmap alignmentGenerationId oldGenerations)
  passivated <- readyPlan emptyContext [] oldGenerations oldIds
  assertEqual
    "passivation has no current class generation"
    []
    (alignmentGenerationPlanGenerations passivated)
  reactivated <- readyPlan singletonContext [newMember] [] Set.empty
  generation <- only "reactivated generation" (alignmentGenerationPlanGenerations reactivated)
  let cut = alignmentGenerationCut generation
      fresh = DomainAlignment.alignmentCutFreshMemberBaseEvidence cut
      promoted = promote occurrenceA reactivated
      bootstrap = Alignment.bootstrapImportEntries promoted
  assertEqual
    "the new incarnation contributes its retained rev0 base"
    [DomainAlignment.freshMemberBaseEvidence deltaA storeA2 DomainAlignment.initialStoreRevision]
    fresh
  case bootstrap of
    [(key, _)] -> do
      assertEqual
        "bootstrap destination is the new incarnation"
        storeA2
        ( Protocol.destinationStoreIncarnation
            (Alignment.bootstrapImportKeyDestination key)
        )
      case Alignment.bootstrapImportKeySource key of
        Alignment.BootstrapFromFreshBase evidence -> do
          assertEqual "the old incarnation is never retargeted" storeA2 (DomainAlignment.freshMemberBaseStoreIncarnation evidence)
          assertEqual
            "bootstrap source evidence cannot bypass destination-generation acceptance"
            (Left (Alignment.BootstrapImportDestinationAcceptanceUnavailable key))
            (fmap (const ()) (Alignment.prepareBootstrapImportAttempt key promoted))
          let accepted = retainCutAcceptance generation promoted
              preparedAttempt =
                checked
                  "select fresh bootstrap source"
                  (Alignment.prepareBootstrapImportAttempt key accepted)
              attempt = Alignment.preparedBootstrapImportAttempt preparedAttempt
              subscribe = Alignment.preparedBootstrapImportSubscribe preparedAttempt
              (bootstrapping, _) =
                Alignment.commitBootstrapImportAttempt preparedAttempt
          assertEqual
            "fresh proof selects the frozen incarnation's exact host"
            heraldA
            (Alignment.bootstrapImportAttemptSourceHerald attempt)
          assertEqual
            "fresh proof selects the frozen incarnation itself"
            storeA2
            (Protocol.alignmentSubscribeSourceStoreIncarnation subscribe)
          case Protocol.alignmentSubscribeSourceProof subscribe of
            Protocol.BootstrapProof proofGeneration _ ->
              assertEqual
                "the proof authenticates the destination generation"
                (alignmentGenerationId generation)
                proofGeneration
            otherProof -> assertFailure ("expected bootstrap proof, got " <> show otherProof)
          assertEqual
            "selected bootstrap source owns a destination transcript"
            [Just heraldA]
            [ Transfer.destinationSubscriptionBootstrapSourceHerald destination
            | (_, destination) <-
                Transfer.destinationSubscriptionEntries
                  (Alignment.alignmentTransferState bootstrapping)
            ]
        other -> assertFailure ("expected fresh-base import, got " <> show other)
    other -> assertFailure ("expected one reactivation import, got " <> show (length other))

caseOccurrenceDeduplication :: Assertion
caseOccurrenceDeduplication = do
  plan <- readyPlan singletonContext [oldMember] [] Set.empty
  let withDebts = retainDebt occurrenceB (retainDebt occurrenceA Alignment.emptyState)
      first = commitPromotion occurrenceA plan withDebts
      second = commitPromotion occurrenceB plan first
      receipts = Alignment.alignmentPromotionEntries second
      bootstrap = Alignment.bootstrapImportEntries second
  assertEqual "one immutable generation-level import" 1 (length bootstrap)
  assertEqual
    "active occurrence deduplication remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState second)
  assertEqual
    "the first serialized occurrence owns the only active key"
    [1, 0]
    ( fmap
        ( length
            . Alignment.alignmentPromotionReceiptBootstrapImportKeys
            . snd
        )
        receipts
    )
  case bootstrap of
    [(_, retained)] ->
      assertEqual
        "the active envelope cause remains the first occurrence"
        (StructuralConsequence.structuralOccurrenceCause occurrenceA)
        ( Protocol.alignmentObligationCause
            (Alignment.bootstrapImportSubscribeEnvelope retained)
        )
    _ -> assertFailure "expected exactly one retained active bootstrap envelope"

caseFulfilledOccurrenceDeduplication :: Assertion
caseFulfilledOccurrenceDeduplication = do
  plan <- readyPlan singletonContext [oldMember] [] Set.empty
  let withDebts = retainDebt occurrenceB (retainDebt occurrenceA Alignment.emptyState)
      first = commitPromotion occurrenceA plan withDebts
  generation <- only "fulfilled bootstrap generation" (alignmentGenerationPlanGenerations plan)
  key <- only "fulfilled bootstrap key" (fmap fst (Alignment.bootstrapImportEntries first))
  let accepted = retainCutAcceptance generation first
      withAttempt = createBootstrapAttempt accepted key
  attempt <- only "fulfilled bootstrap attempt" (fmap snd (Alignment.bootstrapImportAttemptEntries withAttempt))
  let subscribe = Alignment.bootstrapImportAttemptSubscribe attempt
      subscription = Protocol.alignmentSubscribeSubscriptionId subscribe
      sourceStore = Protocol.alignmentSubscribeSourceStoreIncarnation subscribe
      snapshotDigest =
        Protocol.alignmentSemanticSnapshotDigest
          (alignmentGenerationId generation)
          sourceStore
          DomainAlignment.initialStoreRevision
          []
      controls =
        [ Protocol.AlignmentSnapshotStarted
            ( Protocol.alignmentSnapshotStart
                subscription
                DomainAlignment.initialStoreRevision
                snapshotDigest
                1
            ),
          Protocol.AlignmentSnapshotChunkTransferred
            (Protocol.alignmentSnapshotChunk subscription 0 []),
          Protocol.AlignmentSnapshotEnded
            ( Protocol.alignmentSnapshotEnd
                subscription
                DomainAlignment.initialStoreRevision
                snapshotDigest
            ),
          Protocol.AlignmentLiveAdvertised
            (Protocol.alignmentLive subscription DomainAlignment.initialStoreRevision)
        ]
      (completedTransfer, _) =
        applyDestinationControls controls (Alignment.alignmentTransferState withAttempt)
      completed = Alignment.replaceAlignmentTransferState completedTransfer withAttempt
      fulfillment =
        checked
          "retain one-time bootstrap fulfillment"
          ( Alignment.prepareBootstrapImportFulfillment
              attempt
              DomainAlignment.initialStoreRevision
              completed
          )
      (fulfilled, _) = Alignment.commitBootstrapImportFulfillment fulfillment
      second = commitPromotion occurrenceB plan fulfilled
      receipts = Alignment.alignmentPromotionEntries second
      bootstrap = Alignment.bootstrapImportEntries second
  assertEqual "active bootstrap key resolves its exact checked attempt" (Just attempt) (Alignment.lookupBootstrapImportAttempt key withAttempt)
  assertEqual "fulfillment removes the live bootstrap lookup" Nothing (Alignment.lookupBootstrapImportAttempt key fulfilled)
  assertEqual "later occurrence promotion cannot resurrect a fulfilled attempt" Nothing (Alignment.lookupBootstrapImportAttempt key second)
  assertEqual "fulfilled import is no longer active" 0 (length bootstrap)
  assertEqual
    "one immutable fulfillment reserves the generation-level key"
    1
    (length (Alignment.bootstrapImportFulfillmentEntries second))
  assertEqual
    "deduplicated promotion state remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState second)
  assertEqual
    "the first serialized occurrence owns the only key"
    [1, 0]
    ( fmap
        ( length
            . Alignment.alignmentPromotionReceiptBootstrapImportKeys
            . snd
        )
        receipts
    )
  let fulfilledEnvelope =
        Alignment.bootstrapImportSubscribeEnvelope
          (Alignment.bootstrapImportAttemptImport attempt)
  assertEqual
    "the fulfilled envelope cause remains the first occurrence"
    (StructuralConsequence.structuralOccurrenceCause occurrenceA)
    (Protocol.alignmentObligationCause fulfilledEnvelope)

caseFreshSnapshotSuffix :: Assertion
caseFreshSnapshotSuffix = do
  let freshPrepared =
        checked
          "fresh source snapshot"
          ( Transfer.prepareSourceSubscription
              freshSubscribe
              DomainAlignment.initialStoreRevision
              []
              Transfer.SourceLiveImmediate
              (commitDestinationBootstrap freshSubscribe Transfer.emptyState)
          )
      initialControls = Transfer.preparedSourceSubscriptionControls freshPrepared
      (freshSource, _) = Transfer.commitSourceSubscription freshPrepared
      afterInitial = applyDestinationControls initialControls freshSource
      changes = [change freshSubscription revision1 evidenceA, change freshSubscription revision2 evidenceB]
      changesPrepared =
        checked
          "fresh source suffix"
          ( Transfer.prepareSourceChanges
              freshSubscription
              changes
              (Protocol.alignmentLive freshSubscription revision2)
              (fst afterInitial)
          )
      suffixControls = Transfer.preparedSourceChangeControls changesPrepared
      (withSuffix, _) = Transfer.commitSourceChanges changesPrepared
      (freshComplete, suffixWork) = applyDestinationControls suffixControls withSuffix
      retry = Transfer.sourceRetryControls freshSubscription freshComplete
  assertEqual
    "fresh bootstrap snapshot stays at frozen rev0"
    [DomainAlignment.initialStoreRevision]
    [ Protocol.alignmentSnapshotStartBaseRevision start
    | Protocol.AlignmentSnapshotStarted start <- retry
    ]
  assertEqual
    "nonzero current source contributes the complete contiguous suffix"
    [revision1, revision2]
    [ Protocol.alignmentChangeStoreRevision transferred
    | Protocol.AlignmentChangeTransferred transferred <- retry
    ]
  assertEqual
    "the destination applies both post-base revisions"
    [revision1, revision2]
    [ Protocol.alignmentChangeStoreRevision applied
    | work <- suffixWork,
      applied <- Transfer.destinationWorkChanges work
    ]
  assertEqual
    "Live through the nonzero source revision yields an Ack at that revision"
    [revision2]
    [ Protocol.alignmentAckAppliedSourceStoreRevision acknowledgement
    | work <- suffixWork,
      Just acknowledgement <- [Transfer.destinationWorkAcknowledgement work],
      Protocol.alignmentAckAppliedSourceStoreRevision acknowledgement == revision2
    ]

caseAcknowledgedSourceRetry :: Assertion
caseAcknowledgedSourceRetry = do
  let freshPrepared =
        checked
          "acknowledged retry source snapshot"
          ( Transfer.prepareSourceSubscription
              freshSubscribe
              DomainAlignment.initialStoreRevision
              []
              Transfer.SourceLiveImmediate
              (commitDestinationBootstrap freshSubscribe Transfer.emptyState)
          )
      initialControls = Transfer.preparedSourceSubscriptionControls freshPrepared
      (freshSource, _) = Transfer.commitSourceSubscription freshPrepared
      (afterInitial, initialWork) = applyDestinationControls initialControls freshSource
  initialAcknowledgement <-
    only
      "initial snapshot acknowledgement"
      [ acknowledgement
      | work <- initialWork,
        Just acknowledgement <- [Transfer.destinationWorkAcknowledgement work]
      ]
  let acknowledgedInitial =
        fst
          ( Transfer.commitSourceAcknowledgement
              ( checked
                  "retain initial snapshot acknowledgement"
                  (Transfer.prepareSourceAcknowledgement initialAcknowledgement afterInitial)
              )
          )
      changes = [change freshSubscription revision1 evidenceA, change freshSubscription revision2 evidenceB]
      changesPrepared =
        checked
          "retain source suffix after acknowledgement"
          ( Transfer.prepareSourceChanges
              freshSubscription
              changes
              (Protocol.alignmentLive freshSubscription revision2)
              acknowledgedInitial
          )
      suffixControls = Transfer.preparedSourceChangeControls changesPrepared
      (withSuffix, _) = Transfer.commitSourceChanges changesPrepared
      duplicateBeforeSuffixAck =
        checked
          "retry partially acknowledged source"
          ( Transfer.prepareSourceSubscription
              freshSubscribe
              DomainAlignment.initialStoreRevision
              []
              Transfer.SourceLiveImmediate
              withSuffix
          )
      partialRetry = Transfer.preparedSourceSubscriptionControls duplicateBeforeSuffixAck
      (afterSuffix, suffixWork) = applyDestinationControls suffixControls withSuffix
  assertEqual
    "an acknowledged snapshot and change prefix are not replayed"
    suffixControls
    partialRetry
  assertEqual
    "the retry contains no acknowledged snapshot controls"
    []
    [ start
    | Protocol.AlignmentSnapshotStarted start <- partialRetry
    ]
  suffixAcknowledgement <-
    only
      "suffix acknowledgement"
      [ acknowledgement
      | work <- suffixWork,
        Just acknowledgement <- [Transfer.destinationWorkAcknowledgement work],
        Protocol.alignmentAckAppliedSourceStoreRevision acknowledgement == revision2
      ]
  let fullyAcknowledged =
        fst
          ( Transfer.commitSourceAcknowledgement
              ( checked
                  "retain suffix acknowledgement"
                  (Transfer.prepareSourceAcknowledgement suffixAcknowledgement afterSuffix)
              )
          )
      duplicateAfterSuffixAck =
        checked
          "retry fully acknowledged source"
          ( Transfer.prepareSourceSubscription
              freshSubscribe
              DomainAlignment.initialStoreRevision
              []
              Transfer.SourceLiveImmediate
              fullyAcknowledged
          )
  assertEqual
    "a fully acknowledged live source has no retry controls"
    []
    (Transfer.preparedSourceSubscriptionControls duplicateAfterSuffixAck)

caseDestinationRootedReconnect :: Assertion
caseDestinationRootedReconnect = do
  let preparedSource =
        checked
          "destination-rooted reconnect source"
          ( Transfer.prepareSourceSubscription
              freshSubscribe
              DomainAlignment.initialStoreRevision
              []
              Transfer.SourceLiveImmediate
              (commitDestinationBootstrap freshSubscribe Transfer.emptyState)
          )
      sourceControls = Transfer.preparedSourceSubscriptionControls preparedSource
      (withSource, _) = Transfer.commitSourceSubscription preparedSource
      (withDestinationAcknowledgement, _) =
        applyDestinationControls sourceControls withSource
      subscribeControl = Protocol.AlignmentSubscribeRequested freshSubscribe
      acknowledgementControl =
        Protocol.AlignmentAcknowledged
          (Protocol.alignmentAck freshSubscription DomainAlignment.initialStoreRevision)
      terminalState ::
        Protocol.AlignmentCancelReason ->
        (Protocol.AlignmentCancel, Transfer.State)
      terminalState reason =
        let cancellation = Protocol.alignmentCancel freshSubscription reason
            preparedCancellation =
              checked
                ("terminalize destination reconnect: " <> show reason)
                ( Transfer.prepareAlignmentCancellation
                    cancellation
                    withDestinationAcknowledgement
                )
         in ( cancellation,
              fst (Transfer.commitAlignmentCancellation preparedCancellation)
            )
      (sourceLossCancellation, sourceLost) =
        terminalState Protocol.AlignmentSourceIncarnationLost
      (relationRemovedCancellation, relationRemoved) =
        terminalState Protocol.AlignmentRelationRemoved
      (destinationLossCancellation, destinationLost) =
        terminalState Protocol.AlignmentDestinationIncarnationLost
  assertBool
    "the colocated source retains a bulk snapshot/Live repair transcript"
    (length (Transfer.sourceRetryControls freshSubscription withDestinationAcknowledgement) > 1)
  assertEqual
    "the complete destination catalogue retains its exact reactive acknowledgement"
    [subscribeControl, acknowledgementControl]
    (Transfer.destinationRetryControls freshSubscription withDestinationAcknowledgement)
  assertEqual
    "reconnect sends only the destination-owned Subscribe root"
    [subscribeControl]
    ( AlignmentTransferCoordinator.alignmentTransferReconnectControls
        heraldA
        withDestinationAcknowledgement
    )
  assertEqual
    "a destination transcript is not offered to a different source Herald"
    []
    ( AlignmentTransferCoordinator.alignmentTransferReconnectControls
        heraldB
        withDestinationAcknowledgement
    )
  assertEqual
    "the complete destination catalogue retains a received source-loss cancellation"
    [Protocol.AlignmentCancelled sourceLossCancellation]
    (Transfer.destinationRetryControls freshSubscription sourceLost)
  assertEqual
    "reconnect does not reflect a received source-loss cancellation back to its source"
    []
    ( AlignmentTransferCoordinator.alignmentTransferReconnectControls
        heraldA
        sourceLost
    )
  assertEqual
    "reconnect retries a destination-owned relation-removed cancellation"
    [Protocol.AlignmentCancelled relationRemovedCancellation]
    ( AlignmentTransferCoordinator.alignmentTransferReconnectControls
        heraldA
        relationRemoved
    )
  assertEqual
    "reconnect retries a destination-owned incarnation-loss cancellation"
    [Protocol.AlignmentCancelled destinationLossCancellation]
    ( AlignmentTransferCoordinator.alignmentTransferReconnectControls
        heraldA
        destinationLost
    )

caseTerminalSourceRetry :: Assertion
caseTerminalSourceRetry = do
  let preparedSource =
        checked
          "retain source transcript before terminal loss"
          ( Transfer.prepareSourceSubscription
              freshSubscribe
              DomainAlignment.initialStoreRevision
              []
              Transfer.SourceLiveImmediate
              Transfer.emptyState
          )
      (withSource, _) = Transfer.commitSourceSubscription preparedSource
      cancellation =
        Protocol.alignmentCancel
          freshSubscription
          Protocol.AlignmentSourceIncarnationLost
      mismatchedSubscribe =
        bootstrapSubscribe
          freshSubscription
          newGeneration
          storeA
          (DomainAlignment.deriveBootstrapEvidenceDigest "fresh-base-proof")
      preparedCancellation =
        checked
          "terminalize source transcript"
          (Transfer.prepareAlignmentCancellation cancellation withSource)
      (terminal, _) = Transfer.commitAlignmentCancellation preparedCancellation
  assertBool
    "the active transcript had replayable snapshot and Live controls"
    (length (Transfer.sourceRetryControls freshSubscription withSource) > 1)
  assertEqual
    "terminal retry cannot replay snapshot, change, or Live before Cancel"
    [Protocol.AlignmentCancelled cancellation]
    (Transfer.sourceRetryControls freshSubscription terminal)
  assertEqual
    "the coordinator replays the terminal Cancel without consulting lost Store history"
    (Right (Just [Protocol.AlignmentCancelled cancellation]))
    ( AlignmentTransferCoordinator.terminalSourceRetryControls
        heraldB
        freshSubscribe
        terminal
    )
  assertEqual
    "unequal Subscribe bytes under the terminal identifier remain malformed"
    (Left AlignmentTransferCoordinator.AlignmentTransferRemoteProtocolViolation)
    ( AlignmentTransferCoordinator.terminalSourceRetryControls
        heraldB
        mismatchedSubscribe
        terminal
    )

caseAtomicLossSourceCancellationOrdering :: Assertion
caseAtomicLossSourceCancellationOrdering = do
  let preparedSource =
        checked
          "retain source before atomic loss projection"
          ( Transfer.prepareSourceSubscription
              freshSubscribe
              DomainAlignment.initialStoreRevision
              []
              Transfer.SourceLiveImmediate
              Transfer.emptyState
          )
      (withSource, _) = Transfer.commitSourceSubscription preparedSource
      cancellation =
        Protocol.alignmentCancel
          freshSubscription
          Protocol.AlignmentSourceIncarnationLost
      sourceOnlyOwner =
        Alignment.replaceAlignmentTransferState withSource Alignment.emptyState
      predecessorCoordinator =
        checked
          "source-only loss coordinator"
          (AlignmentLoss.alignmentLossCoordinatorState heraldA sourceOnlyOwner)
      lostStore =
        AlignmentLoss.qualifiedStoreCoordinate heraldA deltaA freshStore
      preparedLoss =
        checked
          "terminalize exact source-only Store loss"
          ( AlignmentLoss.prepareAlignmentLoss
              (StructuralConsequence.structuralOccurrenceCause occurrenceA)
              lostStore
              predecessorCoordinator
          )
      successorCoordinator = AlignmentLoss.commitAlignmentLoss preparedLoss
      sourceLostOwner =
        AlignmentLoss.alignmentLossCoordinatorOwner successorCoordinator
      sourceLost = Alignment.alignmentTransferState sourceLostOwner
      withReplacement =
        commitDestinationBootstrap closureBootstrapSubscribe sourceLost
      expected =
        [ (heraldB, Protocol.AlignmentCancelled cancellation),
          (heraldA, Protocol.AlignmentSubscribeRequested closureBootstrapSubscribe)
        ]
      replay =
        checked
          "replay exact source-only Store loss"
          ( AlignmentLoss.prepareAlignmentLoss
              (StructuralConsequence.structuralOccurrenceCause occurrenceA)
              lostStore
              successorCoordinator
          )
  assertEqual
    "source-only loss retains the exact cancellation in its atomic plan"
    [cancellation]
    ( AlignmentLoss.alignmentLossPlanCancellations
        (AlignmentLoss.preparedAlignmentLossPlan preparedLoss)
    )
  assertEqual
    "source-only loss terminalizes the exact retained source transcript"
    (Just cancellation)
    ( Transfer.sourceSubscriptionCancellation
        =<< Transfer.lookupSourceSubscription freshSubscription sourceLost
    )
  assertEqual
    "exact source-only loss replay is unchanged"
    AlignmentLoss.AlignmentLossUnchanged
    (AlignmentLoss.preparedAlignmentLossDisposition replay)
  assertEqual
    "exact source-only loss replay preserves the final coordinator"
    successorCoordinator
    (AlignmentLoss.commitAlignmentLoss replay)
  assertEqual
    "the final delta emits the newly retained source cancellation before replacement work"
    (Right expected)
    ( AlignmentTransferCoordinator.alignmentLossTransferDeltaControls
        heraldA
        withSource
        withReplacement
    )
  assertEqual
    "reprojecting the retained final state emits neither cancellation nor replacement"
    (Right [])
    ( AlignmentTransferCoordinator.alignmentLossTransferDeltaControls
        heraldA
        withReplacement
        withReplacement
    )

caseSourceLossReselection :: Assertion
caseSourceLossReselection = do
  prior <-
    readyPlanAt
      twoHeraldTopology
      twoHeraldPlacement
      mergedContext
      recoveryMembers
      []
      Set.empty
  let priorGenerations = alignmentGenerationPlanGenerations prior
      predecessorIds = Set.fromList (fmap alignmentGenerationId priorGenerations)
  predecessor <- only "two-member predecessor" priorGenerations
  split <-
    readyPlanAt
      twoHeraldTopology
      twoHeraldPlacement
      splitContext
      recoveryMembers
      priorGenerations
      predecessorIds
  let withDebts = retainDebt occurrenceB (retainDebt occurrenceA Alignment.emptyState)
      withPrior =
        commitPromotionAt
          heraldA
          occurrenceA
          twoHeraldTopologyId
          twoHeraldPlacement
          prior
          withDebts
      promoted =
        commitPromotionAt
          heraldA
          occurrenceB
          twoHeraldTopologyId
          twoHeraldPlacement
          split
          withPrior
      predecessorImports =
        [ key
        | (key, _) <- Alignment.bootstrapImportEntries promoted,
          Alignment.BootstrapFromPredecessor _ <-
            [Alignment.bootstrapImportKeySource key]
        ]
  key <- only "local predecessor bootstrap" predecessorImports
  successor <-
    only
      "successor generation"
      [ generation
      | generation <- alignmentGenerationPlanGenerations split,
        alignmentGenerationId generation
          == Alignment.bootstrapImportKeyGeneration key
      ]
  let accepted =
        foldl
          (\state (accepting, generation) -> retainCutAcceptanceFor accepting generation state)
          promoted
          [ (heraldA, predecessor),
            (heraldB, predecessor),
            (heraldA, successor),
            (heraldB, successor)
          ]
      readyA =
        Protocol.classMemberReady
          Protocol.firstAlignmentEvidenceSequence
          (alignmentGenerationId predecessor)
          storeA
          readyDigestA
          DomainAlignment.initialStoreRevision
      readyB =
        Protocol.classMemberReady
          (Protocol.nextAlignmentEvidenceSequence Protocol.firstAlignmentEvidenceSequence)
          (alignmentGenerationId predecessor)
          storeB
          readyDigestB
          DomainAlignment.initialStoreRevision
      readiness = [readyA, readyB]
      withReadiness = foldl (flip retainMemberReadiness) accepted readiness
      certificate sourceStore prefixDigest =
        Protocol.historicalCertificate
          (alignmentGenerationId predecessor)
          sourceStore
          (Protocol.classMemberReadySetDigest readiness)
          ( DomainAlignment.deriveBootstrapEvidenceDigest
              (DomainAlignment.memberReadyEvidenceDigestBytes prefixDigest)
          )
          DomainAlignment.initialStoreRevision
      withCertificates =
        retainHistoricalCertificate
          (certificate storeB readyDigestB)
          (retainHistoricalCertificate (certificate storeA readyDigestA) withReadiness)
      unavailableA =
        Alignment.unavailableAlignmentSource heraldA deltaA storeA
      unavailableB =
        Alignment.unavailableAlignmentSource heraldB deltaB storeB
      retainedBeforeAttempt =
        Alignment.prepareUnavailableAlignmentSourceRetention
          unavailableA
          withCertificates
      (withPreselectionLoss, preselectionDisposition) =
        Alignment.commitUnavailableAlignmentSourceRetention retainedBeforeAttempt
      retainedBeforeAttemptReplay =
        Alignment.prepareUnavailableAlignmentSourceRetention
          unavailableA
          withPreselectionLoss
      (afterPreselectionReplay, replayDisposition) =
        Alignment.commitUnavailableAlignmentSourceRetention retainedBeforeAttemptReplay
      preselectionPrepared =
        checked
          "select around a pre-attempt source loss"
          (Alignment.prepareBootstrapImportAttempt key afterPreselectionReplay)
      preselectionAttempt =
        Alignment.preparedBootstrapImportAttempt preselectionPrepared
      allUnavailable =
        fst
          ( Alignment.commitUnavailableAlignmentSourceRetention
              ( Alignment.prepareUnavailableAlignmentSourceRetention
                  unavailableB
                  afterPreselectionReplay
              )
          )
      firstPrepared =
        checked
          "select first predecessor representative"
          (Alignment.prepareBootstrapImportAttempt key withCertificates)
      firstAttempt = Alignment.preparedBootstrapImportAttempt firstPrepared
      firstSubscription =
        Protocol.alignmentSubscribeSubscriptionId
          (Alignment.bootstrapImportAttemptSubscribe firstAttempt)
      (withFirst, _) = Alignment.commitBootstrapImportAttempt firstPrepared
  assertEqual
    "the least certified representative is selected first"
    heraldA
    (Alignment.bootstrapImportAttemptSourceHerald firstAttempt)
  assertEqual
    "a checked loss is retained before any attempt exists"
    Alignment.AlignmentSourceRetainedUnavailable
    preselectionDisposition
  assertEqual
    "pre-attempt source-loss replay is owner-idempotent"
    Alignment.AlignmentSourceRetentionUnchanged
    replayDisposition
  assertEqual
    "pre-attempt loss selects the exact alternate source"
    heraldB
    (Alignment.bootstrapImportAttemptSourceHerald preselectionAttempt)
  assertEqual
    "pre-attempt source exhaustion leaves the import explicitly pending"
    (Left (Alignment.BootstrapImportSourceEvidenceUnavailable key))
    (fmap (const ()) (Alignment.prepareBootstrapImportAttempt key allUnavailable))
  assertBool
    "pre-attempt source exhaustion retains the logical import"
    (any ((== key) . fst) (Alignment.bootstrapImportEntries allUnavailable))
  assertEqual
    "pre-attempt source-loss retention remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState allUnavailable)

  let firstLoss =
        checked
          "invalidate the first source incarnation"
          ( Alignment.prepareBootstrapImportAttemptInvalidation
              firstAttempt
              Protocol.AlignmentSourceIncarnationLost
              withFirst
          )
      (afterFirstLoss, _) =
        Alignment.commitBootstrapImportAttemptInvalidation firstLoss
      firstLossReplay =
        checked
          "replay the first source loss"
          ( Alignment.prepareBootstrapImportAttemptInvalidation
              firstAttempt
              Protocol.AlignmentSourceIncarnationLost
              afterFirstLoss
          )
      (afterFirstReplay, _) =
        Alignment.commitBootstrapImportAttemptInvalidation firstLossReplay
  assertEqual
    "source-loss replay is owner-idempotent"
    afterFirstLoss
    afterFirstReplay

  let secondPrepared =
        checked
          "select the alternate predecessor representative"
          (Alignment.prepareBootstrapImportAttempt key afterFirstReplay)
      secondAttempt = Alignment.preparedBootstrapImportAttempt secondPrepared
      secondSubscription =
        Protocol.alignmentSubscribeSubscriptionId
          (Alignment.bootstrapImportAttemptSubscribe secondAttempt)
      (withSecond, _) = Alignment.commitBootstrapImportAttempt secondPrepared
  assertEqual
    "the failed coordinate is skipped globally"
    heraldB
    (Alignment.bootstrapImportAttemptSourceHerald secondAttempt)
  assertBool
    "reselection allocates a fresh subscription"
    (firstSubscription /= secondSubscription)

  let secondLoss =
        checked
          "invalidate the alternate source incarnation"
          ( Alignment.prepareBootstrapImportAttemptInvalidation
              secondAttempt
              Protocol.AlignmentSourceIncarnationLost
              withSecond
          )
      (exhausted, _) =
        Alignment.commitBootstrapImportAttemptInvalidation secondLoss
      failedCoordinates =
        Set.fromList
          [ ( Alignment.alignmentSourceCoordinateHerald coordinate,
              Alignment.alignmentSourceCoordinateStoreIncarnation coordinate
            )
          | coordinate <- Set.toAscList (Alignment.alignmentFailedSourceCoordinates exhausted)
          ]
  assertEqual
    "both exact failed incarnations are globally excluded"
    (Set.fromList [(heraldA, storeA), (heraldB, storeB)])
    failedCoordinates
  assertEqual
    "the owner retains delta-qualified unavailable source coordinates"
    (Set.fromList [unavailableA, unavailableB])
    (Alignment.alignmentUnavailableSources exhausted)
  assertEqual
    "source exhaustion leaves the stable bootstrap import explicitly unsourced"
    (Left (Alignment.BootstrapImportSourceEvidenceUnavailable key))
    (fmap (const ()) (Alignment.prepareBootstrapImportAttempt key exhausted))
  assertBool
    "source loss does not discard the semantic import"
    (any ((== key) . fst) (Alignment.bootstrapImportEntries exhausted))
  assertEqual
    "source-loss state remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState exhausted)

caseDestinationLossBeforeAttempt :: Assertion
caseDestinationLossBeforeAttempt = do
  plan <- readyPlan chainContext allMembers [] Set.empty
  let promoted = promote occurrenceA plan
      activeObligations = Alignment.activeAlignmentObligationEntries promoted
      activeImports = Alignment.bootstrapImportEntries promoted
  lostGeneration <-
    generationHosting "unattempted lost destination" storeA (alignmentGenerationPlanGenerations plan)
  lostFresh <- freshBaseForStore "unattempted lost fresh source" storeA lostGeneration
  let destinationCause =
        Alignment.AlignmentPlanDestinationStoreLost
          (alignmentGenerationId lostGeneration)
          (Protocol.destinationStore deltaA storeA)
      sourceCause =
        Alignment.AlignmentPlanFreshBaseStoreLost
          (alignmentGenerationId lostGeneration)
          lostFresh
          (Alignment.alignmentSourceCoordinate heraldA storeA)
      discoveredCauses =
        AlignmentTransferCoordinator.alignmentPlanLossCauses
          heraldA
          ( \affectedSort destination ->
              affectedSort == typedSort
                && destination == Protocol.destinationStore deltaB storeB
          )
          promoted
      prepared =
        checked
          "invalidate unattempted destination plan"
          (Alignment.prepareAlignmentPlanInvalidation destinationCause promoted)
      receipt = Alignment.preparedAlignmentPlanInvalidationReceipt prepared
      (invalidated, _) = Alignment.commitAlignmentPlanInvalidation prepared
  assertBool "fixture has ordinary logical work before any attempt" (not (null activeObligations))
  assertBool "fixture has bootstrap logical work before any attempt" (not (null activeImports))
  assertEqual "fixture has no ordinary attempt" [] (Alignment.alignmentAttemptEntries promoted)
  assertEqual "fixture has no bootstrap attempt" [] (Alignment.bootstrapImportAttemptEntries promoted)
  assertEqual
    "pre-attempt discovery reports only the absent destination and its exact fresh source"
    (Set.fromList [destinationCause, sourceCause])
    discoveredCauses
  assertEqual
    "unattempted destination loss needs no transcript cancellation"
    []
    (Alignment.preparedAlignmentPlanInvalidationCancellations prepared)
  assertEqual
    "the tombstone discards every ordinary owner at the coordinate"
    []
    (Alignment.activeAlignmentObligationEntries invalidated)
  assertEqual
    "the tombstone discards every bootstrap owner at the coordinate"
    []
    (Alignment.bootstrapImportEntries invalidated)
  assertEqual
    "the receipt retains all discarded ordinary owners"
    (length activeObligations)
    (length (Alignment.alignmentPlanInvalidationReceiptObligations receipt))
  assertEqual
    "the receipt retains all discarded bootstrap owners"
    (length activeImports)
    (length (Alignment.alignmentPlanInvalidationReceiptBootstrapImports receipt))
  assertEqual
    "unattempted destination loss remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState invalidated)

caseDestinationLossTombstone :: Assertion
caseDestinationLossTombstone = do
  plan <- readyPlan splitContext allMembers [] Set.empty
  let generations = alignmentGenerationPlanGenerations plan
      promoted = promote occurrenceA plan
      accepted = foldl (flip retainCutAcceptance) promoted generations
      keys = fmap fst (Alignment.bootstrapImportEntries accepted)
      withAttempts = foldl createBootstrapAttempt accepted keys
      attempts = fmap snd (Alignment.bootstrapImportAttemptEntries withAttempts)
  targetGeneration <-
    only
      "lost destination generation"
      [ Alignment.bootstrapImportKeyGeneration key
      | key <- keys,
        Protocol.destinationStoreIncarnation
          (Alignment.bootstrapImportKeyDestination key)
          == storeA
      ]
  let lostDestination = Protocol.destinationStore deltaA storeA
      cause =
        Alignment.AlignmentPlanDestinationStoreLost
          targetGeneration
          lostDestination
      prepared =
        checked
          "invalidate the destination plan coordinate"
          (Alignment.prepareAlignmentPlanInvalidation cause withAttempts)
      tombstone = Alignment.preparedAlignmentPlanInvalidationReceipt prepared
      (invalidated, _) = Alignment.commitAlignmentPlanInvalidation prepared
  assertEqual
    "one tombstone covers the exact loss cause"
    (Set.singleton cause)
    (Alignment.alignmentPlanInvalidationReceiptCauses tombstone)
  assertEqual
    "the whole coordinate loses every active bootstrap import"
    []
    (Alignment.bootstrapImportEntries invalidated)
  assertEqual
    "the whole coordinate loses every active bootstrap attempt"
    []
    (Alignment.bootstrapImportAttemptEntries invalidated)
  assertEqual
    "historical generations remain available as ancestry"
    (length generations)
    (length (Alignment.alignmentGenerationEntries invalidated))
  assertEqual
    "one plan-coordinate tombstone is retained"
    1
    (length (Alignment.alignmentPlanInvalidationEntries invalidated))

  let replay =
        checked
          "replay destination plan invalidation"
          (Alignment.prepareAlignmentPlanInvalidation cause invalidated)
      (afterReplay, _) = Alignment.commitAlignmentPlanInvalidation replay
  assertEqual
    "destination-loss replay is owner-idempotent"
    invalidated
    afterReplay
  assertEqual
    "replay emits no duplicate cancellation"
    []
    (Alignment.preparedAlignmentPlanInvalidationCancellations replay)

  mapM_
    (\attempt -> assertTerminalDestination attempt (Alignment.alignmentTransferState invalidated))
    attempts
  assertEqual
    "destination-loss state remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState invalidated)

caseDestinationLossAfterFulfillment :: Assertion
caseDestinationLossAfterFulfillment = do
  (_, generation, key, attempt, fulfilled) <- fulfilledSingletonFixture
  let subscription =
        Protocol.alignmentSubscribeSubscriptionId
          (Alignment.bootstrapImportAttemptSubscribe attempt)
      cause =
        Alignment.AlignmentPlanDestinationStoreLost
          (alignmentGenerationId generation)
          (Alignment.bootstrapImportKeyDestination key)
      discoveredCauses =
        AlignmentTransferCoordinator.alignmentPlanLossCauses
          heraldA
          (\_ _ -> False)
          fulfilled
      prepared =
        checked
          "invalidate fulfilled destination coordinate"
          (Alignment.prepareAlignmentPlanInvalidation cause fulfilled)
      receipt = Alignment.preparedAlignmentPlanInvalidationReceipt prepared
      (invalidated, _) = Alignment.commitAlignmentPlanInvalidation prepared
  assertEqual "fulfilled fixture has no active bootstrap owner" [] (Alignment.bootstrapImportEntries fulfilled)
  assertEqual "fulfilled fixture has no ordinary owner" [] (Alignment.activeAlignmentObligationEntries fulfilled)
  assertEqual
    "current-generation discovery survives one-time owner fulfillment"
    (Set.singleton cause)
    discoveredCauses
  assertEqual
    "destination loss is retained even when only fulfillment history remains"
    (Set.singleton cause)
    (Alignment.alignmentPlanInvalidationReceiptCauses receipt)
  assertEqual
    "the historical fulfillment remains immutable"
    [key]
    (fmap fst (Alignment.bootstrapImportFulfillmentEntries invalidated))
  assertEqual
    "DestinationLost upgrades the fulfilled terminal transcript"
    [Just (Protocol.alignmentCancel subscription Protocol.AlignmentDestinationIncarnationLost)]
    [ Transfer.destinationSubscriptionCancellation destination
    | (identifier, destination) <-
        Transfer.destinationSubscriptionEntries
          (Alignment.alignmentTransferState invalidated),
      identifier == subscription
    ]
  assertEqual
    "post-fulfillment destination loss remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState invalidated)

caseRetainedRemoteLossBeforePromotion :: Assertion
caseRetainedRemoteLossBeforePromotion = do
  plan <- readyPlanAt twoHeraldTopology twoHeraldPlacement singletonContext [oldMember] [] Set.empty
  generation <- only "remote singleton generation" (alignmentGenerationPlanGenerations plan)
  let promoteRemote =
        commitPromotionAt heraldB occurrenceA twoHeraldTopologyId twoHeraldPlacement plan
          . retainDebt occurrenceA
      retainLoss source owner =
        fst
          ( Alignment.commitUnavailableAlignmentSourceRetention
              (Alignment.prepareUnavailableAlignmentSourceRetention source owner)
          )
      exactLoss = Alignment.unavailableAlignmentSource heraldA deltaA storeA
      noLoss = promoteRemote Alignment.emptyState
      lossBefore = promoteRemote (retainLoss exactLoss Alignment.emptyState)
      lossAfter = retainLoss exactLoss noLoss
      discover = AlignmentTransferCoordinator.alignmentPlanLossCauses heraldB (\_ _ -> False)
      expected =
        Alignment.AlignmentPlanDestinationStoreLost
          (alignmentGenerationId generation)
          (Protocol.destinationStore deltaA storeA)
  assertEqual "a remote member has no local bootstrap owner" [] (Alignment.bootstrapImportEntries noLoss)
  assertEqual "an unknown remote Store is not inferred lost" Set.empty (discover noLoss)
  assertEqual "loss retained before an old anchor arrives remains executable" (Set.singleton expected) (discover lossBefore)
  assertEqual "loss and promotion arrival order preserve the exact cause" (discover lossBefore) (discover lossAfter)
  mapM_
    ( \other ->
        assertEqual
          "only the complete exact remote coordinate is lost"
          Set.empty
          (discover (retainLoss other noLoss))
    )
    [ Alignment.unavailableAlignmentSource heraldC deltaA storeA,
      Alignment.unavailableAlignmentSource heraldA deltaB storeA,
      Alignment.unavailableAlignmentSource heraldA deltaA storeA2
    ]
  let prepared = checked "invalidate delayed remote plan" (Alignment.prepareAlignmentPlanInvalidation expected lossBefore)
      (invalidated, _) = Alignment.commitAlignmentPlanInvalidation prepared
      replay = checked "replay remote invalidation" (Alignment.prepareAlignmentPlanInvalidation expected invalidated)
  assertBool "the exact plan is tombstoned" (Alignment.alignmentPlanIsInvalidated typedSort twoHeraldTopologyId twoHeraldPlacement invalidated)
  assertEqual "no subscription is needed to invalidate a remote-only plan" [] (Alignment.preparedAlignmentPlanInvalidationCancellations prepared)
  assertEqual "remote invalidation replay is unchanged" invalidated (fst (Alignment.commitAlignmentPlanInvalidation replay))
  assertEqual "remote-only loss preserves the checked owner invariant" (Right ()) (Alignment.validateAlignmentState invalidated)

caseTombstonedPromotion :: Assertion
caseTombstonedPromotion = do
  plan <- readyPlan singletonContext [oldMember] [] Set.empty
  generation <- only "tombstoned generation" (alignmentGenerationPlanGenerations plan)
  let promoted = promote occurrenceA plan
      cause =
        Alignment.AlignmentPlanDestinationStoreLost
          (alignmentGenerationId generation)
          (Protocol.destinationStore deltaA storeA)
      preparedInvalidation =
        checked
          "tombstone promotion coordinate"
          (Alignment.prepareAlignmentPlanInvalidation cause promoted)
      coordinate =
        Alignment.alignmentPlanInvalidationReceiptCoordinate
          (Alignment.preparedAlignmentPlanInvalidationReceipt preparedInvalidation)
      (invalidated, _) = Alignment.commitAlignmentPlanInvalidation preparedInvalidation
      exactReplay =
        checked
          "replay original tombstoned promotion"
          ( Alignment.prepareAlignmentPromotion
              heraldA
              (StructuralConsequence.structuralOccurrenceCause occurrenceA)
              typedSort
              topologyId
              oneHeraldPlacement
              plan
              invalidated
          )
      (afterExactReplay, replayDisposition) = Alignment.commitAlignmentPromotion exactReplay
  assertEqual
    "the exact original promotion replay is immutable"
    Alignment.AlignmentPromotionUnchanged
    replayDisposition
  assertEqual
    "the exact original replay cannot resurrect executable work"
    invalidated
    afterExactReplay

  let withLaterDebt = retainDebt occurrenceB afterExactReplay
      sameCoordinate =
        Alignment.prepareAlignmentPromotion
          heraldA
          (StructuralConsequence.structuralOccurrenceCause occurrenceB)
          typedSort
          topologyId
          oneHeraldPlacement
          plan
          withLaterDebt
  case sameCoordinate of
    Left (Alignment.AlignmentPromotionPlanInvalidated observed) ->
      assertEqual "later same-coordinate promotion names the exact tombstone" coordinate observed
    Left other -> assertFailure ("expected exact tombstone rejection, got " <> show other)
    Right _ -> assertFailure "expected later same-coordinate promotion to be rejected"

  replacementPlan <-
    readyPlanAt
      topology
      nextOneHeraldPlacement
      singletonContext
      [newMember]
      []
      Set.empty
  let rePromoted =
        commitPromotionAt
          heraldA
          occurrenceB
          topologyId
          nextOneHeraldPlacement
          replacementPlan
          withLaterDebt
      replacementImports = Alignment.bootstrapImportEntries rePromoted
  assertEqual "a fresh placement vector creates one replacement import" 1 (length replacementImports)
  assertEqual
    "fresh-vector re-promotion targets only the replacement incarnation"
    [storeA2]
    [ Protocol.destinationStoreIncarnation (Alignment.bootstrapImportKeyDestination key)
    | (key, _) <- replacementImports
    ]
  assertEqual
    "fresh-vector re-promotion preserves the old coordinate tombstone"
    1
    (length (Alignment.alignmentPlanInvalidationEntries rePromoted))
  assertEqual
    "fresh-vector re-promotion remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState rePromoted)

caseFreshLossOrderConvergence :: Assertion
caseFreshLossOrderConvergence = do
  plan <- readyPlan singletonContext [oldMember] [] Set.empty
  generation <- only "fresh-loss generation" (alignmentGenerationPlanGenerations plan)
  fresh <-
    only
      "fresh-loss base"
      ( DomainAlignment.alignmentCutFreshMemberBaseEvidence
          (alignmentGenerationCut generation)
      )
  let promoted = promote occurrenceA plan
      accepted = retainCutAcceptance generation promoted
  key <-
    fst
      <$> only
        "fresh-loss bootstrap import"
        (Alignment.bootstrapImportEntries accepted)
  let
    withAttempt = createBootstrapAttempt accepted key
  attempt <-
    snd
      <$> only
        "fresh-loss bootstrap attempt"
        (Alignment.bootstrapImportAttemptEntries withAttempt)
  let
    subscription =
      Protocol.alignmentSubscribeSubscriptionId
        (Alignment.bootstrapImportAttemptSubscribe attempt)
    source = Alignment.alignmentSourceCoordinate heraldA storeA
    sourceCause =
      Alignment.AlignmentPlanFreshBaseStoreLost
        (alignmentGenerationId generation)
        fresh
        source
    destinationCause =
      Alignment.AlignmentPlanDestinationStoreLost
        (alignmentGenerationId generation)
        (Protocol.destinationStore deltaA storeA)
    retainCause cause predecessor =
      fst
        ( Alignment.commitAlignmentPlanInvalidation
            ( checked
                "join fresh-base plan loss"
                (Alignment.prepareAlignmentPlanInvalidation cause predecessor)
            )
        )
    sourceThenDestination =
      retainCause destinationCause (retainCause sourceCause withAttempt)
    destinationThenSource =
      retainCause sourceCause (retainCause destinationCause withAttempt)
    expectedCauses = Set.fromList [sourceCause, destinationCause]
  assertEqual
    "fresh source/destination loss order converges to identical retained state"
    sourceThenDestination
    destinationThenSource
  receipt <-
    only
      "joined fresh-loss tombstone"
      (fmap snd (Alignment.alignmentPlanInvalidationEntries sourceThenDestination))
  assertEqual
    "both exact causes survive the commutative join"
    expectedCauses
    (Alignment.alignmentPlanInvalidationReceiptCauses receipt)
  assertBool
    "the failed fresh source coordinate remains globally excluded"
    (Set.member source (Alignment.alignmentFailedSourceCoordinates sourceThenDestination))
  assertEqual
    "DestinationLost dominates the terminal bootstrap transcript"
    [Just (Protocol.alignmentCancel subscription Protocol.AlignmentDestinationIncarnationLost)]
    [ Transfer.destinationSubscriptionCancellation destination
    | (identifier, destination) <-
        Transfer.destinationSubscriptionEntries
          (Alignment.alignmentTransferState sourceThenDestination),
      identifier == subscription
    ]
  assertEqual
    "the joined fresh-loss state remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState sourceThenDestination)

caseMultiFreshSourceCoordinateIndependence :: Assertion
caseMultiFreshSourceCoordinateIndependence = do
  plan <- readyPlan mergedContext allMembers [] Set.empty
  generation <- only "multi-fresh generation" (alignmentGenerationPlanGenerations plan)
  freshA <- freshBaseForStore "first fresh source" storeA generation
  freshB <- freshBaseForStore "independent fresh source" storeB generation
  let promoted = promote occurrenceA plan
      sourceA = Alignment.alignmentSourceCoordinate heraldA storeA
      sourceB = Alignment.alignmentSourceCoordinate heraldA storeB
      causeA =
        Alignment.AlignmentPlanFreshBaseStoreLost
          (alignmentGenerationId generation)
          freshA
          sourceA
      destinationCauseA =
        Alignment.AlignmentPlanDestinationStoreLost
          (alignmentGenerationId generation)
          (Protocol.destinationStore deltaA storeA)
      discoveredCauses =
        AlignmentTransferCoordinator.alignmentPlanLossCauses
          heraldA
          ( \affectedSort destination ->
              affectedSort == typedSort
                && destination == Protocol.destinationStore deltaB storeB
          )
          promoted
      prepared =
        checked
          "invalidate one exact fresh source"
          (Alignment.prepareAlignmentPlanInvalidation causeA promoted)
      receipt = Alignment.preparedAlignmentPlanInvalidationReceipt prepared
      (invalidated, _) = Alignment.commitAlignmentPlanInvalidation prepared
      failed = Alignment.alignmentFailedSourceCoordinates invalidated
  assertBool "fixture retains two distinct fresh bases" (freshA /= freshB)
  assertEqual
    "one live exact fresh coordinate excludes every false B loss"
    (Set.fromList [destinationCauseA, causeA])
    discoveredCauses
  assertEqual
    "only the observed exact fresh-source cause is retained"
    (Set.singleton causeA)
    (Alignment.alignmentPlanInvalidationReceiptCauses receipt)
  assertEqual
    "only the lost source coordinate enters global exclusion"
    (Set.singleton sourceA)
    failed
  assertBool
    "the independent fresh coordinate is not falsely excluded"
    (Set.notMember sourceB failed)
  assertEqual
    "fresh-source loss discards the invalid immutable plan"
    []
    (Alignment.bootstrapImportEntries invalidated)
  assertEqual
    "multi-fresh exact-source invalidation remains internally valid"
    (Right ())
    (Alignment.validateAlignmentState invalidated)

caseInputClosure :: Assertion
caseInputClosure = do
  let withDestination = commitDestinationAttempt predecessorAttempt Transfer.emptyState
      closurePrepared =
        checked
          "retain input-closure owner"
          ( Transfer.preparePredecessorInputClosures
              newGeneration
              (Set.singleton predecessorSubscription)
              withDestination
          )
      (withClosureOwner, _) =
        Transfer.commitPredecessorInputClosures closurePrepared
      withClosedPrefix = completeEmptyDestination predecessorAttempt withClosureOwner
      exactPrefix =
        Transfer.memberReadinessPrefix
          predecessorSubscription
          DomainAlignment.initialStoreRevision
          DomainAlignment.initialStoreRevision
          DomainAlignment.initialStoreRevision
      prepareReceipt =
        Transfer.preparePredecessorInputClosureReceipt
          newGeneration
          heraldA
          destinationStore
          DomainAlignment.initialStoreRevision
          [exactPrefix]
  assertEqual
    "the predecessor's local publication prefix must settle first"
    ( Left
        ( Transfer.AlignmentTransferInputClosureReceiptConflict
            newGeneration
            destinationStore
        )
    )
    (fmap (const ()) (prepareReceipt withClosedPrefix))

  let withLocalSettlement = commitLocalPrefix withClosedPrefix
      forwardMarkers = retainRouteMarkers [routeFromB, routeFromC] withLocalSettlement
      reverseMarkers = retainRouteMarkers [routeFromC, routeFromB] withLocalSettlement
  assertEqual
    "ordered remote route evidence is arrival-order independent in retained state"
    (Transfer.routeCutoverEntries forwardMarkers)
    (Transfer.routeCutoverEntries reverseMarkers)
  let uncoveredPrefix =
        Transfer.memberReadinessPrefix
          predecessorSubscription
          DomainAlignment.initialStoreRevision
          revision1
          DomainAlignment.initialStoreRevision
  assertEqual
    "applied and acknowledged revisions must cover Live"
    ( Left
        ( Transfer.AlignmentTransferInputClosureReceiptConflict
            newGeneration
            destinationStore
        )
    )
    ( fmap
        (const ())
        ( Transfer.preparePredecessorInputClosureReceipt
            newGeneration
            heraldA
            destinationStore
            DomainAlignment.initialStoreRevision
            [uncoveredPrefix]
            forwardMarkers
        )
    )

  let receiptPrepared = checked "record predecessor input closure" (prepareReceipt forwardMarkers)
      (withReceipt, _) =
        Transfer.commitPredecessorInputClosureReceipt receiptPrepared
  assertEqual
    "one exact lower-bound receipt is retained"
    [(newGeneration, destinationStore)]
    [ ( Transfer.predecessorInputClosureReceiptGeneration receipt,
        Transfer.predecessorInputClosureReceiptSourceStore receipt
      )
    | (_, receipt) <- Transfer.predecessorInputClosureReceiptEntries withReceipt
    ]

  let cancellation =
        Protocol.alignmentCancel
          predecessorSubscription
          Protocol.AlignmentRelationRemoved
      (cancelled, _) =
        Transfer.commitAlignmentCancellation
          ( checked
              "cancel closure owner after its local receipt"
              (Transfer.prepareAlignmentCancellation cancellation withReceipt)
          )
  assertTerminalSubscription predecessorSubscription cancelled
  assertEqual
    "input closure needs no child acknowledgement gate"
    (Right ())
    (Transfer.validateAlignmentTransferState cancelled)

caseBootstrapPurposeExcludedFromClosure :: Assertion
caseBootstrapPurposeExcludedFromClosure = do
  let withOrdinary = commitDestinationAttempt predecessorAttempt Transfer.emptyState
      withBoth = commitDestinationBootstrap closureBootstrapSubscribe withOrdinary
      ordinaryCandidates =
        Set.fromList
          [ identifier
          | (identifier, _) <-
              AlignmentTransferCoordinator.predecessorInputClosureCandidates
                predecessorGeneration
                destinationStore
                withBoth
          ]
      prepared =
        checked
          "retain ordinary-purpose closure candidates"
          ( Transfer.preparePredecessorInputClosures
              newGeneration
              ordinaryCandidates
              withBoth
          )
      (withClosures, _) = Transfer.commitPredecessorInputClosures prepared
      retainedSubscriptions =
        [ Protocol.alignmentSubscribeSubscriptionId subscribe
        | (_, (subscribe, _)) <- Transfer.predecessorInputClosureEntries withClosures
        ]
  assertEqual
    "the exported purpose discriminator selects only the ordinary attempt"
    (Set.singleton predecessorSubscription)
    ordinaryCandidates
  assertEqual
    "the bootstrap-purpose destination never becomes a predecessor closure owner"
    [predecessorSubscription]
    retainedSubscriptions
  assertBool
    "the bootstrap transcript remains separately identifiable"
    ( any
        ( \(identifier, destination) ->
            identifier == closureBootstrapSubscription
              && Transfer.destinationSubscriptionAttempt destination == Nothing
              && Transfer.destinationSubscriptionBootstrapSourceHerald destination == Just heraldA
        )
        (Transfer.destinationSubscriptionEntries withClosures)
    )
  assertEqual
    "purpose-filtered closure state remains internally valid"
    (Right ())
    (Transfer.validateAlignmentTransferState withClosures)

caseSplitInputClosure :: Assertion
caseSplitInputClosure = do
  let withDestination = commitDestinationAttempt predecessorAttempt Transfer.emptyState
      retainFor generation predecessor =
        Transfer.commitPredecessorInputClosures
          ( checked
              "retain split input-closure owner"
              ( Transfer.preparePredecessorInputClosures
                  generation
                  (Set.singleton predecessorSubscription)
                  predecessor
              )
          )
      (withFirstChild, _) = retainFor newGeneration withDestination
      withClosedPrefix = completeEmptyDestination predecessorAttempt withFirstChild
      withSettledPrefixes =
        commitLocalPrefixFor
          secondChildGeneration
          (commitLocalPrefixFor newGeneration withClosedPrefix)
      exactPrefix =
        Transfer.memberReadinessPrefix
          predecessorSubscription
          DomainAlignment.initialStoreRevision
          DomainAlignment.initialStoreRevision
          DomainAlignment.initialStoreRevision
      retainReceipt generation predecessor =
        Transfer.commitPredecessorInputClosureReceipt
          ( checked
              "record split predecessor input closure"
              ( Transfer.preparePredecessorInputClosureReceipt
                  generation
                  heraldA
                  destinationStore
                  DomainAlignment.initialStoreRevision
                  [exactPrefix]
                  predecessor
              )
          )
      (withFirstReceipt, _) = retainReceipt newGeneration withSettledPrefixes
      cancellation =
        Protocol.alignmentCancel
          predecessorSubscription
          Protocol.AlignmentRelationRemoved
      (terminal, _) =
        Transfer.commitAlignmentCancellation
          ( checked
              "terminalize shared split predecessor"
              (Transfer.prepareAlignmentCancellation cancellation withFirstReceipt)
          )
  assertTerminalSubscription predecessorSubscription terminal

  let secondChildPrepared =
        checked
          "reuse terminal split input-closure owner"
          ( Transfer.preparePredecessorInputClosures
              secondChildGeneration
              (Set.singleton predecessorSubscription)
              terminal
          )
      (withSecondChild, _) =
        Transfer.commitPredecessorInputClosures secondChildPrepared
      (withBothReceipts, _) =
        retainReceipt secondChildGeneration withSecondChild
      retainedGenerations =
        Set.fromList
          [ Transfer.predecessorInputClosureReceiptGeneration receipt
          | (_, receipt) <- Transfer.predecessorInputClosureReceiptEntries withBothReceipts
          ]
      replayFor generation =
        checked
          "replay terminal split input-closure owner"
          ( Transfer.preparePredecessorInputClosures
              generation
              (Set.singleton predecessorSubscription)
              withBothReceipts
          )
  assertEqual
    "the second child reuses the terminal covered prefix without reoffering"
    []
    (Transfer.preparedPredecessorInputClosureSubscribes secondChildPrepared)
  assertEqual
    "both child generations retain an exact closure receipt"
    (Set.fromList [newGeneration, secondChildGeneration])
    retainedGenerations
  assertEqual
    "terminal predecessor is not reoffered to either child"
    []
    ( concatMap
        (Transfer.preparedPredecessorInputClosureSubscribes . replayFor)
        [newGeneration, secondChildGeneration]
    )
  assertEqual
    "split closure state remains internally valid"
    (Right ())
    (Transfer.validateAlignmentTransferState withBothReceipts)

promote :: Identity.StructuralOccurrenceId -> AlignmentGenerationPlan -> Alignment.State
promote occurrence plan = commitPromotion occurrence plan (retainDebt occurrence Alignment.emptyState)

commitPromotion ::
  Identity.StructuralOccurrenceId ->
  AlignmentGenerationPlan ->
  Alignment.State ->
  Alignment.State
commitPromotion occurrence plan predecessor =
  commitPromotionAt
    heraldA
    occurrence
    topologyId
    oneHeraldPlacement
    plan
    predecessor

commitPromotionAt ::
  Identity.HeraldEpoch ->
  Identity.StructuralOccurrenceId ->
  Identity.TopologyCutId ->
  DomainAlignment.PhysicalPlacementRevisionVector ->
  AlignmentGenerationPlan ->
  Alignment.State ->
  Alignment.State
commitPromotionAt localHerald occurrence selectedTopology selectedPlacement plan predecessor =
  fst
    ( Alignment.commitAlignmentPromotion
        ( checked
            "promote generation plan"
            ( Alignment.prepareAlignmentPromotion
                localHerald
                (StructuralConsequence.structuralOccurrenceCause occurrence)
                typedSort
                selectedTopology
                selectedPlacement
                plan
                predecessor
            )
        )
    )

retainDebt :: Identity.StructuralOccurrenceId -> Alignment.State -> Alignment.State
retainDebt occurrence predecessor =
  fst
    ( Alignment.commitStructuralDebtRetention
        ( checked
            "retain alignment debt"
            ( Alignment.prepareStructuralDebtRetention
                (StructuralConsequence.structuralOccurrenceCause occurrence)
                ( normalizeStructuralDebts
                    [ structuralConsequenceDebt
                        ( structuralDebtKey
                            (StructuralConsequence.structuralOccurrenceCause occurrence)
                            TopologyAlignmentDebt
                            typedSort
                            Nothing
                        )
                        (structuralDebtEvidence Set.empty Set.empty Set.empty)
                    ]
                )
                predecessor
            )
        )
    )

retainCutAcceptance :: AlignmentGeneration -> Alignment.State -> Alignment.State
retainCutAcceptance = retainCutAcceptanceFor heraldA

retainCutAcceptanceFor ::
  Identity.HeraldEpoch ->
  AlignmentGeneration ->
  Alignment.State ->
  Alignment.State
retainCutAcceptanceFor acceptingHerald generation predecessor =
  fst
    ( Alignment.commitAlignmentCutAcceptance
        ( checked
            "retain local cut acceptance"
            ( Alignment.prepareAlignmentCutAcceptance
                ( Protocol.alignmentCutAccepted
                    (alignmentGenerationId generation)
                    acceptingHerald
                    (Topology.deriveTopologyCutId (DomainAlignment.alignmentCutTopologyCut cut))
                    (DomainAlignment.alignmentCutPhysicalPlacementRevisionVector cut)
                    DomainAlignment.EmptyHeraldPublicationPrefix
                )
                predecessor
            )
        )
    )
  where
    cut = alignmentGenerationCut generation

readyPlan ::
  ContextGraph ->
  [GenerationMemberInput] ->
  [AlignmentGeneration] ->
  Set.Set DomainAlignment.ContextClassGenerationId ->
  IO AlignmentGenerationPlan
readyPlan selectedContext members prior certificates =
  readyPlanAt topology oneHeraldPlacement selectedContext members prior certificates

readyPlanAt ::
  Topology.TopologyCut ->
  DomainAlignment.PhysicalPlacementRevisionVector ->
  ContextGraph ->
  [GenerationMemberInput] ->
  [AlignmentGeneration] ->
  Set.Set DomainAlignment.ContextClassGenerationId ->
  IO AlignmentGenerationPlan
readyPlanAt selectedTopology selectedPlacement selectedContext members prior certificates =
  case prepareAlignmentGenerationPlan
    typedSort
    selectedTopology
    selectedPlacement
    selectedContext
    members
    prior
    []
    certificates of
    Right (AlignmentGenerationReady plan) -> pure plan
    other -> assertFailure ("expected ready plan, got " <> show other)

retainMemberReadiness :: Protocol.ClassMemberReady -> Alignment.State -> Alignment.State
retainMemberReadiness ready predecessor =
  fst
    ( Alignment.commitClassMemberReady
        ( checked
            "retain member readiness"
            (Alignment.prepareClassMemberReady ready predecessor)
        )
    )

retainHistoricalCertificate ::
  Protocol.HistoricalCertificate -> Alignment.State -> Alignment.State
retainHistoricalCertificate certificate predecessor =
  fst
    ( Alignment.commitHistoricalCertificate
        ( checked
            "retain historical certificate"
            (Alignment.prepareHistoricalCertificate certificate predecessor)
        )
    )

createBootstrapAttempt ::
  Alignment.State -> Alignment.BootstrapImportKey -> Alignment.State
createBootstrapAttempt predecessor key =
  fst
    ( Alignment.commitBootstrapImportAttempt
        ( checked
            "create bootstrap attempt"
            (Alignment.prepareBootstrapImportAttempt key predecessor)
        )
    )

fulfilledSingletonFixture ::
  IO
    ( AlignmentGenerationPlan,
      AlignmentGeneration,
      Alignment.BootstrapImportKey,
      Alignment.BootstrapImportAttempt,
      Alignment.State
    )
fulfilledSingletonFixture = do
  plan <- readyPlan singletonContext [oldMember] [] Set.empty
  generation <- only "fulfilled singleton generation" (alignmentGenerationPlanGenerations plan)
  let promoted = promote occurrenceA plan
      accepted = retainCutAcceptance generation promoted
  key <- only "fulfilled singleton key" (fmap fst (Alignment.bootstrapImportEntries accepted))
  let withAttempt = createBootstrapAttempt accepted key
  attempt <-
    only
      "fulfilled singleton attempt"
      (fmap snd (Alignment.bootstrapImportAttemptEntries withAttempt))
  let subscribe = Alignment.bootstrapImportAttemptSubscribe attempt
      subscription = Protocol.alignmentSubscribeSubscriptionId subscribe
      sourceStore = Protocol.alignmentSubscribeSourceStoreIncarnation subscribe
      snapshotDigest =
        Protocol.alignmentSemanticSnapshotDigest
          (alignmentGenerationId generation)
          sourceStore
          DomainAlignment.initialStoreRevision
          []
      controls =
        [ Protocol.AlignmentSnapshotStarted
            ( Protocol.alignmentSnapshotStart
                subscription
                DomainAlignment.initialStoreRevision
                snapshotDigest
                1
            ),
          Protocol.AlignmentSnapshotChunkTransferred
            (Protocol.alignmentSnapshotChunk subscription 0 []),
          Protocol.AlignmentSnapshotEnded
            ( Protocol.alignmentSnapshotEnd
                subscription
                DomainAlignment.initialStoreRevision
                snapshotDigest
            ),
          Protocol.AlignmentLiveAdvertised
            (Protocol.alignmentLive subscription DomainAlignment.initialStoreRevision)
        ]
      (completedTransfer, _) =
        applyDestinationControls controls (Alignment.alignmentTransferState withAttempt)
      completed = Alignment.replaceAlignmentTransferState completedTransfer withAttempt
      preparedFulfillment =
        checked
          "fulfill singleton bootstrap"
          ( Alignment.prepareBootstrapImportFulfillment
              attempt
              DomainAlignment.initialStoreRevision
              completed
          )
      (fulfilled, _) = Alignment.commitBootstrapImportFulfillment preparedFulfillment
  pure (plan, generation, key, attempt, fulfilled)

freshBaseForStore ::
  String ->
  Identity.StoreIncarnationId ->
  AlignmentGeneration ->
  IO DomainAlignment.FreshMemberBaseEvidence
freshBaseForStore label store generation =
  only
    label
    [ fresh
    | fresh <-
        DomainAlignment.alignmentCutFreshMemberBaseEvidence
          (alignmentGenerationCut generation),
      DomainAlignment.freshMemberBaseStoreIncarnation fresh == store
    ]

allMembers, recoveryMembers :: [GenerationMemberInput]
allMembers = [oldMember, memberB]
recoveryMembers = [oldMember, memberBRemote]

oldMember, newMember, memberB, memberBRemote :: GenerationMemberInput
oldMember = generationMemberInput deltaA storeA heraldA DomainAlignment.initialStoreRevision
newMember = generationMemberInput deltaA storeA2 heraldA DomainAlignment.initialStoreRevision
memberB = generationMemberInput deltaB storeB heraldA DomainAlignment.initialStoreRevision
memberBRemote = generationMemberInput deltaB storeB heraldB DomainAlignment.initialStoreRevision

splitContext, mergedContext, chainContext, singletonContext, emptyContext :: ContextGraph
splitContext = context "split context" [DeltaVertex deltaA, DeltaVertex deltaB] [] [deltaA, deltaB]
mergedContext =
  context
    "merged context"
    [DeltaVertex deltaA, DeltaVertex deltaB]
    [ edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve,
      edgePayload (DeltaVertex deltaB) (DeltaVertex deltaA) Preserve
    ]
    [deltaA, deltaB]
chainContext =
  context
    "one-way context"
    [DeltaVertex deltaA, DeltaVertex deltaB]
    [edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve]
    [deltaA, deltaB]
singletonContext = context "singleton context" [DeltaVertex deltaA] [] [deltaA]
emptyContext = context "empty context" [] [] []

context ::
  String ->
  [VertexId] ->
  [EdgePayload] ->
  [Identity.DeltaId] ->
  ContextGraph
context label vertices edges active =
  checked label (deriveContextGraph vertices edges active)

commitDestinationBootstrap ::
  Protocol.AlignmentSubscribe -> Transfer.State -> Transfer.State
commitDestinationBootstrap subscribe predecessor =
  fst
    ( Transfer.commitDestinationBootstrap
        ( checked
            "retain bootstrap destination"
            (Transfer.prepareDestinationBootstrap heraldA subscribe predecessor)
        )
    )

applyDestinationControls ::
  [Protocol.AlignmentControl] ->
  Transfer.State ->
  (Transfer.State, [Transfer.DestinationWork])
applyDestinationControls supplied initial = foldl applyOne (initial, []) supplied
  where
    applyOne (state, work) control = case control of
      Protocol.AlignmentSnapshotStarted start -> commit (Transfer.prepareDestinationSnapshotStart start state)
      Protocol.AlignmentSnapshotChunkTransferred chunk -> commit (Transfer.prepareDestinationSnapshotChunk chunk state)
      Protocol.AlignmentSnapshotEnded end -> commit (Transfer.prepareDestinationSnapshotEnd end state)
      Protocol.AlignmentChangeTransferred transferred -> commit (Transfer.prepareDestinationChange transferred state)
      Protocol.AlignmentLiveAdvertised live -> commit (Transfer.prepareDestinationLive live state)
      _ -> (state, work)
      where
        commit prepared =
          let checkedPrepared = checked "apply destination control" prepared
              destinationWork = Transfer.preparedDestinationWork checkedPrepared
              (successor, _) = Transfer.commitDestinationWork checkedPrepared
           in (successor, work <> [destinationWork])

assertTerminalDestination ::
  Alignment.BootstrapImportAttempt -> Transfer.State -> Assertion
assertTerminalDestination attempt =
  assertTerminalSubscription
    ( Protocol.alignmentSubscribeSubscriptionId
        (Alignment.bootstrapImportAttemptSubscribe attempt)
    )

assertTerminalSubscription ::
  Protocol.AlignmentSubscriptionId -> Transfer.State -> Assertion
assertTerminalSubscription subscription predecessor = do
  let changePrepared =
        checked
          "admit late terminal change"
          ( Transfer.prepareDestinationChange
              (change subscription revision1 evidenceA)
              predecessor
          )
      changeWork = Transfer.preparedDestinationWork changePrepared
      (afterChange, changeDisposition) =
        Transfer.commitDestinationWork changePrepared
      livePrepared =
        checked
          "admit late terminal Live"
          ( Transfer.prepareDestinationLive
              (Protocol.alignmentLive subscription revision2)
              afterChange
          )
      liveWork = Transfer.preparedDestinationWork livePrepared
      (afterLive, liveDisposition) = Transfer.commitDestinationWork livePrepared
  assertEqual "late terminal change has no Store work" [] (Transfer.destinationWorkChanges changeWork)
  assertEqual "late terminal change has no snapshot work" Nothing (Transfer.destinationWorkSnapshot changeWork)
  assertEqual "late terminal change has no acknowledgement" Nothing (Transfer.destinationWorkAcknowledgement changeWork)
  assertEqual "late terminal change does not mutate retained transfer state" predecessor afterChange
  assertEqual "late terminal change is unchanged" Transfer.AlignmentTransferUnchanged changeDisposition
  assertEqual "late terminal Live has no Store work" [] (Transfer.destinationWorkChanges liveWork)
  assertEqual "late terminal Live has no acknowledgement" Nothing (Transfer.destinationWorkAcknowledgement liveWork)
  assertEqual "late terminal Live does not mutate retained transfer state" afterChange afterLive
  assertEqual "late terminal Live is unchanged" Transfer.AlignmentTransferUnchanged liveDisposition

commitDestinationAttempt :: Protocol.AlignmentAttempt -> Transfer.State -> Transfer.State
commitDestinationAttempt attempt predecessor =
  fst
    ( Transfer.commitDestinationAttempt
        ( checked
            "retain context destination"
            (Transfer.prepareDestinationAttempt attempt predecessor)
        )
    )

completeEmptyDestination :: Protocol.AlignmentAttempt -> Transfer.State -> Transfer.State
completeEmptyDestination attempt predecessor =
  fst
    ( applyDestinationControls
        [ Protocol.AlignmentSnapshotStarted
            (Protocol.alignmentSnapshotStart identifier DomainAlignment.initialStoreRevision digest 1),
          Protocol.AlignmentSnapshotChunkTransferred
            (Protocol.alignmentSnapshotChunk identifier 0 []),
          Protocol.AlignmentSnapshotEnded
            (Protocol.alignmentSnapshotEnd identifier DomainAlignment.initialStoreRevision digest),
          Protocol.AlignmentLiveAdvertised
            (Protocol.alignmentLive identifier DomainAlignment.initialStoreRevision)
        ]
        predecessor
    )
  where
    identifier = Protocol.alignmentAttemptSubscriptionId attempt
    certificate = Protocol.alignmentAttemptHistoricalCertificate attempt
    digest =
      Protocol.alignmentSemanticSnapshotDigest
        (Protocol.historicalCertificateClassGeneration certificate)
        (Protocol.historicalCertificateSourceStoreIncarnation certificate)
        DomainAlignment.initialStoreRevision
        []

commitLocalPrefix :: Transfer.State -> Transfer.State
commitLocalPrefix = commitLocalPrefixFor newGeneration

commitLocalPrefixFor ::
  DomainAlignment.ContextClassGenerationId -> Transfer.State -> Transfer.State
commitLocalPrefixFor generation predecessor =
  fst
    ( Transfer.commitLocalRoutePrefixSettlement
        ( checked
            "settle local cutover prefix"
            (Transfer.prepareLocalRoutePrefixSettlement generation heraldA predecessor)
        )
    )

retainRouteMarkers :: [Protocol.AlignmentRouteCutoverMarker] -> Transfer.State -> Transfer.State
retainRouteMarkers markers initial = foldl retain initial markers
  where
    retain predecessor marker =
      fst
        ( Transfer.commitRouteCutoverMarker
            ( checked
                "retain route cutover"
                (Transfer.prepareRouteCutoverMarker marker predecessor)
            )
        )

change ::
  Protocol.AlignmentSubscriptionId ->
  DomainAlignment.StoreRevision ->
  Protocol.RetainedPublicationEvidence ->
  Protocol.AlignmentChange
change = Protocol.alignmentChange

freshSubscribe :: Protocol.AlignmentSubscribe
freshSubscribe =
  bootstrapSubscribe
    freshSubscription
    newGeneration
    freshStore
    (DomainAlignment.deriveBootstrapEvidenceDigest "fresh-base-proof")

closureBootstrapSubscribe :: Protocol.AlignmentSubscribe
closureBootstrapSubscribe =
  bootstrapSubscribeFor
    closureBootstrapSubscription
    predecessorGeneration
    predecessorGeneration
    predecessorStore
    (DomainAlignment.deriveBootstrapEvidenceDigest "closure-bootstrap-proof")

bootstrapSubscribe ::
  Protocol.AlignmentSubscriptionId ->
  DomainAlignment.ContextClassGenerationId ->
  Identity.StoreIncarnationId ->
  DomainAlignment.BootstrapEvidenceDigest ->
  Protocol.AlignmentSubscribe
bootstrapSubscribe subscription = bootstrapSubscribeFor subscription newGeneration

bootstrapSubscribeFor ::
  Protocol.AlignmentSubscriptionId ->
  DomainAlignment.ContextClassGenerationId ->
  DomainAlignment.ContextClassGenerationId ->
  Identity.StoreIncarnationId ->
  DomainAlignment.BootstrapEvidenceDigest ->
  Protocol.AlignmentSubscribe
bootstrapSubscribeFor subscription destinationGeneration sourceGeneration sourceStore proof =
  checked
    "bootstrap subscribe"
    ( Protocol.alignmentSubscribe
        envelope
        subscription
        sourceStore
        (Protocol.BootstrapProof destinationGeneration proof)
    )
  where
    envelope =
      checked
        "bootstrap envelope"
        ( Protocol.alignmentObligation
            (Protocol.alignmentObligationId (Protocol.alignmentSubscriptionIdDestinationHerald subscription) Protocol.firstAlignmentObligationSequence)
            (StructuralConsequence.structuralOccurrenceCause occurrenceA)
            sortId
            sortDefinitionOccurrence
            destinationGeneration
            (Protocol.destinationStore deltaA destinationStore :| [])
            sourceGeneration
            Route.Normal
        )

predecessorAttempt :: Protocol.AlignmentAttempt
predecessorAttempt = contextAttempt predecessorSubscription predecessorGeneration predecessorStore

contextAttempt ::
  Protocol.AlignmentSubscriptionId ->
  DomainAlignment.ContextClassGenerationId ->
  Identity.StoreIncarnationId ->
  Protocol.AlignmentAttempt
contextAttempt subscription generation sourceStore =
  checked
    "context attempt"
    (Protocol.alignmentAttempt obligation subscription heraldB certificate)
  where
    obligation =
      checked
        "context obligation"
        ( Protocol.alignmentObligation
            (Protocol.alignmentObligationId heraldA (Protocol.nextAlignmentObligationSequence Protocol.firstAlignmentObligationSequence))
            (StructuralConsequence.structuralOccurrenceCause occurrenceA)
            sortId
            sortDefinitionOccurrence
            generation
            (Protocol.destinationStore deltaA destinationStore :| [])
            generation
            Route.Normal
        )
    readyDigest = DomainAlignment.deriveMemberReadyEvidenceDigest "ready"
    certificate =
      Protocol.historicalCertificate
        generation
        sourceStore
        readyDigest
        (DomainAlignment.deriveBootstrapEvidenceDigest "complete")
        DomainAlignment.initialStoreRevision

freshSubscription, predecessorSubscription, closureBootstrapSubscription :: Protocol.AlignmentSubscriptionId
freshSubscription = Protocol.alignmentSubscriptionId heraldB Protocol.firstAlignmentSubscriptionSequence
predecessorSubscription = Protocol.alignmentSubscriptionId heraldA Protocol.firstAlignmentSubscriptionSequence
closureBootstrapSubscription =
  Protocol.alignmentSubscriptionId
    heraldA
    (Protocol.nextAlignmentSubscriptionSequence Protocol.firstAlignmentSubscriptionSequence)

routeFromB, routeFromC :: Protocol.AlignmentRouteCutoverMarker
routeFromB = checked "route B" (Protocol.alignmentRouteCutoverMarker (Plan.alignmentPlanIdFromClaimedCoordinates typedSort topologyId oneHeraldPlacement) newGeneration heraldB heraldA)
routeFromC = checked "route C" (Protocol.alignmentRouteCutoverMarker (Plan.alignmentPlanIdFromClaimedCoordinates typedSort topologyId oneHeraldPlacement) newGeneration heraldC heraldA)

evidenceA, evidenceB :: Protocol.RetainedPublicationEvidence
evidenceA = retainedEvidence publicationA (Value.canonicalValueBytes (Value.boolValue True))
evidenceB = retainedEvidence publicationB (Value.canonicalValueBytes (Value.boolValue False))

retainedEvidence :: Identity.PublicationId -> Value.CanonicalValueBytes -> Protocol.RetainedPublicationEvidence
retainedEvidence publication canonical =
  Protocol.primordialRetainedPublicationEvidence
    publication
    sortId
    sortDefinitionOccurrence
    canonical
    Route.Normal

publicationA, publicationB :: Identity.PublicationId
publicationA = Identity.publicationId nabla Identity.genesisAuthorityEpoch heraldA (Identity.nablaSequence 1)
publicationB = Identity.publicationId nabla Identity.genesisAuthorityEpoch heraldA (Identity.nablaSequence 2)

revision1, revision2 :: DomainAlignment.StoreRevision
revision1 = DomainAlignment.nextStoreRevision DomainAlignment.initialStoreRevision
revision2 = DomainAlignment.nextStoreRevision revision1

typedSort :: SortOccurrence
typedSort = sortOccurrence sortId sortDefinitionOccurrence

topology :: Topology.TopologyCut
topology =
  checked
    "one-Herald topology cut"
    ( Topology.topologyCut
        (Topology.sameGenerationPredecessor predecessorTopologyCut)
        ( Topology.topologyFrontier
            (Structural.emptyStructuralVersionVector oneHeraldMembership)
            (Identity.controlIndex 0)
        )
        (Topology.deriveTopologyOccurrenceDigest "alignment-recovery-topology")
    )

topologyId :: Identity.TopologyCutId
topologyId = Topology.deriveTopologyCutId topology

twoHeraldTopology :: Topology.TopologyCut
twoHeraldTopology =
  checked
    "two-Herald topology cut"
    ( Topology.topologyCut
        (Topology.sameGenerationPredecessor predecessorTopologyCut)
        ( Topology.topologyFrontier
            (Structural.emptyStructuralVersionVector twoHeraldMembership)
            (Identity.controlIndex 0)
        )
        (Topology.deriveTopologyOccurrenceDigest "alignment-source-recovery-topology")
    )

twoHeraldTopologyId :: Identity.TopologyCutId
twoHeraldTopologyId = Topology.deriveTopologyCutId twoHeraldTopology

oneHeraldPlacement, nextOneHeraldPlacement, twoHeraldPlacement :: DomainAlignment.PhysicalPlacementRevisionVector
oneHeraldPlacement =
  checked
    "one-Herald placement"
    ( DomainAlignment.physicalPlacementRevisionVector
        oneHeraldMembership
        [(heraldA, DomainAlignment.firstPlacementRevision)]
    )
nextOneHeraldPlacement =
  checked
    "next one-Herald placement"
    ( DomainAlignment.physicalPlacementRevisionVector
        oneHeraldMembership
        [ ( heraldA,
            DomainAlignment.nextPlacementRevision DomainAlignment.firstPlacementRevision
          )
        ]
    )
twoHeraldPlacement =
  checked
    "two-Herald placement"
    ( DomainAlignment.physicalPlacementRevisionVector
        twoHeraldMembership
        [ (heraldA, DomainAlignment.firstPlacementRevision),
          (heraldB, DomainAlignment.firstPlacementRevision)
        ]
    )

oneHeraldMembership, twoHeraldMembership :: Membership.HeraldMembershipGeneration
oneHeraldMembership =
  checked
    "one-Herald membership"
    (Membership.genesisHeraldMembershipGeneration topologySystemId (heraldA :| []))
twoHeraldMembership =
  checked
    "two-Herald membership"
    ( Membership.genesisHeraldMembershipGeneration
        topologySystemId
        (heraldA :| [heraldB])
    )

topologySystemId :: Identity.SystemId
topologySystemId = identity "topology system" Identity.mkSystemId 0x50

readyDigestA, readyDigestB :: DomainAlignment.MemberReadyEvidenceDigest
readyDigestA = DomainAlignment.deriveMemberReadyEvidenceDigest "recovery-ready-a"
readyDigestB = DomainAlignment.deriveMemberReadyEvidenceDigest "recovery-ready-b"

occurrenceA, occurrenceB :: Identity.StructuralOccurrenceId
occurrenceA = Identity.structuralOccurrenceId heraldA (checked "occurrence A" (Identity.mkStructuralSequence 1))
occurrenceB = Identity.structuralOccurrenceId heraldA (checked "occurrence B" (Identity.mkStructuralSequence 2))

predecessorGeneration, newGeneration, secondChildGeneration :: DomainAlignment.ContextClassGenerationId
predecessorGeneration = identity "predecessor generation" DomainAlignment.mkContextClassGenerationId 0x71
newGeneration = identity "new generation" DomainAlignment.mkContextClassGenerationId 0x72
secondChildGeneration = identity "second child generation" DomainAlignment.mkContextClassGenerationId 0x73

heraldA, heraldB, heraldC :: Identity.HeraldEpoch
heraldA = identity "herald A" Identity.mkHeraldEpoch 0x11
heraldB = identity "herald B" Identity.mkHeraldEpoch 0x12
heraldC = identity "herald C" Identity.mkHeraldEpoch 0x13

deltaA, deltaB :: Identity.DeltaId
deltaA = identity "delta A" Identity.mkDeltaId 0x21
deltaB = identity "delta B" Identity.mkDeltaId 0x22

nabla :: Identity.NablaId
nabla = identity "nabla" Identity.mkNablaId 0x23

sortId :: Identity.SortId
sortId = identity "sort" Identity.mkSortId 0x31

sortDefinitionOccurrence :: Identity.SortDefinitionOccurrenceId
sortDefinitionOccurrence = identity "sort occurrence" Identity.mkSortDefinitionOccurrenceId 0x32

storeA, storeA2, storeB, freshStore, predecessorStore, destinationStore :: Identity.StoreIncarnationId
storeA = identity "store A" Identity.mkStoreIncarnationId 0x41
storeA2 = identity "store A2" Identity.mkStoreIncarnationId 0x42
storeB = identity "store B" Identity.mkStoreIncarnationId 0x43
freshStore = identity "fresh source store" Identity.mkStoreIncarnationId 0x44
predecessorStore = identity "predecessor store" Identity.mkStoreIncarnationId 0x45
destinationStore = identity "destination store" Identity.mkStoreIncarnationId 0x47

predecessorTopologyCut :: Identity.TopologyCutId
predecessorTopologyCut = identity "predecessor topology" Identity.mkTopologyCutId 0x51

identity ::
  (Show problem) =>
  String ->
  (ByteString.ByteString -> Either problem value) ->
  Word ->
  value
identity label constructor marker =
  checked label (constructor (fixtureIdentifierBytes (fromIntegral marker)))

only :: String -> [value] -> IO value
only _ [value] = pure value
only label values = error (label <> ": expected one, got " <> show (length values))

generationHosting ::
  String ->
  Identity.StoreIncarnationId ->
  [AlignmentGeneration] ->
  IO AlignmentGeneration
generationHosting label store =
  only label
    . filter
      ( any
          ((== store) . DomainAlignment.alignmentMemberStoreIncarnation)
          . DomainAlignment.alignmentCutExactMembers
          . alignmentGenerationCut
      )

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
