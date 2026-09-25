{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Step15TerminalRepairProperties (tests) where

import Control.Concurrent
  ( MVar,
    newEmptyMVar,
    readMVar,
    threadDelay,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
  )
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.List (group, nub, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (NeutralVertexRole),
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    controlIndexWord64,
    heraldEpochBytes,
  )
import Eclips.Domain.MemberSet (memberSetDigestBytes)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipGenerationRetirementControlIndex,
  )
import Eclips.Herald.Discovery
  ( PeerHelloDisposition (PeerHelloAccepted),
    peerBindingRemoteHeraldEpoch,
    peerHelloHeraldEpoch,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect
      ( ClosePeerBinding,
        RejectPeerConnection,
        SetPeerCandidateDisposition
      ),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (..),
    RuntimeObservation (..),
    inputBody,
  )
import Eclips.Herald.Peer.RPC
  ( admitTerminalSourceUnionOccurrenceClaim,
    projectPeerControl,
    projectPeerLogicalAttempt,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( ConnectionPromotion (PeerConnectionPromoted),
    KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent, ShellEvent),
    ShellTraceEvent (ShellConnectionPromoted, ShellEffectRouted),
  )
import Eclips.Herald.Runtime.TCP.Internal.Peer
  ( setPeerPublicationProbeForTest,
  )
import Eclips.Oracle.Runtime
  ( oracleRuntimeMembershipGeneration,
  )
import Eclips.Oracle.Runtime.TCP (oracleTcpNodeStatuses)
import Eclips.Protocol.Peer.Types qualified as Protocol
import PeerEvidence (data SemanticPeerAlignmentControl, data SemanticPeerControlReceived, data SemanticPeerPublicationReceived, data SemanticSendPeerControl, data SemanticSendPeerItem)
import Step15Applications
  ( publishStep15NeutralCarrier,
    readStep15PredefinedStore,
    step15PredefinedAccess,
    withStep15Applications,
  )
import Step15Configuration
  ( Step15HeraldName (..),
    step15HeraldEpoch,
  )
import Step15Fixtures
  ( Step15Deployment,
    Step15Herald,
    awaitStep15PeerMesh,
    awaitStep15SurvivorMesh,
    crashStep15H4,
    step15DeploymentOracleCluster,
    step15H1,
    step15H2,
    step15H3,
    step15H4,
    step15ManagedHeraldTcp,
    step15ManagedHeraldWorkLedger,
    withStep15CrashDeployment,
  )
import Step15WorkCounts
  ( Step15AttributableWork (..),
    Step15WorkCursor,
    Step15WorkEvidence,
    Step15WorkLedger,
    awaitStep15WorkEvidenceSince,
    snapshotStep15WorkBaselines,
    snapshotStep15WorkEvidenceSince,
    step15HeartbeatLifecycleEvents,
    step15OracleLifecycleEvents,
    step15PeerDialLifecycleEvents,
    step15RuntimeEvidence,
    step15TimerLifecycleEvents,
    summarizeStep15WorkEvidence,
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
    "Step-15 terminal-source repair"
    [ testCase
        "one-survivor H4 payload repairs private history without retargeting H1's read"
        caseTerminalSourceUnionRepairsH1
    ]

caseTerminalSourceUnionRepairsH1 :: Assertion
caseTerminalSourceUnionRepairsH1 = do
  observed <- timeout scenarioTimeoutMicroseconds $ withStep15CrashDeployment $ \deployment -> do
    awaitStep15PeerMesh deployment
    withStep15Applications deployment
      $ \h1Application h1Startup h2Application h2Startup h4Application h4Startup -> do
        h1Neutral <- step15PredefinedAccess "H1" NeutralVertexRole h1Startup
        h2Neutral <- step15PredefinedAccess "H2" NeutralVertexRole h2Startup
        h4Neutral <- step15PredefinedAccess "H4" NeutralVertexRole h4Startup
        h1Before <- readStep15PredefinedStore "H1 terminal baseline" h1Application h1Neutral
        h2Before <- readStep15PredefinedStore "H2 terminal baseline" h2Application h2Neutral
        let heralds = deploymentHeralds deployment
            ledgers = fmap step15ManagedHeraldWorkLedger heralds
        (baselineEvidence, cursors) <- snapshotStep15WorkBaselines ledgers

        (h2After, preCrashEvidence, preCrashCursors) <-
          withHeldH4Destinations deployment $ \deliveryGate -> do
            (_, _pendingPublication) <-
              publishStep15NeutralCarrier
                "H4 terminal-source occurrence"
                h4Application
                h4Startup
                h4Neutral
            -- The call remains intentionally pending: the source crashes with
            -- the H1 and H3 physical handoffs held below the protocol boundary.
            awaitHeldDestinations deliveryGate
            h2After <-
              awaitStoreGrowth
                "H2 sole-survivor archive"
                (length h2Before)
                (readStep15PredefinedStore "H2 sole-survivor archive" h2Application h2Neutral)
            assertBool
              "H2 admits the H4 occurrence before the crash"
              (length h2After == length h2Before + 1)
            h1StillBefore <-
              readStep15PredefinedStore "H1 pre-crash exclusion" h1Application h1Neutral
            assertEqual
              "H1 still lacks the occurrence while its exact physical handoff is held"
              (length h1Before)
              (length h1StillBefore)

            -- This cursor is the explicit causal boundary immediately before
            -- the deliberate crash.  Retaining the prefix lets the final,
            -- joined-ledger assertions distinguish a pre-existing link loss
            -- from one caused by H4's structured cancellation.
            preCrashSnapshots <- snapshotStep15WorkEvidenceSince cursors ledgers
            crashStep15H4 deployment
            -- H4's structured cancellation has joined all physical writer
            -- children. Releasing the test latch now cannot deliver the held
            -- source messages; it only makes failure cleanup total.
            releaseHeldDestinations deliveryGate
            pure
              ( h2After,
                fmap snd preCrashSnapshots,
                fmap fst preCrashSnapshots
              )

        awaitStep15SurvivorMesh deployment
        successorMembership <- awaitH4Retirement deployment
        readyEvidence <- awaitTerminalEvidence successorMembership cursors ledgers
        assertTerminalCausalChain successorMembership readyEvidence
        h1After <-
          readStep15PredefinedStore
            "H1 post-terminal-repair serving read"
            h1Application
            h1Neutral
        assertEqual
          "destination-free repair cannot retarget H1's lost destination-qualified publication"
          h1Before
          h1After
        h2Repaired <-
          readStep15PredefinedStore
            "H2 post-terminal-repair serving read"
            h2Application
            h2Neutral
        assertEqual
          "terminal replay does not duplicate H2's already materialized occurrence"
          (length h2After)
          (length h2Repaired)

        -- Capture the second phase boundary as the final action in the
        -- scenario callback.  The fixture starts its ordered H3/H2/H1 drains
        -- immediately after this callback returns.  The prefix and suffix are
        -- checked only after every scope has joined, while the complete
        -- baseline-to-final interval remains the source of all work bounds.
        -- The STM evidence barrier makes each survivor-side H4 loss part of
        -- the crash phase instead of letting ordinary scheduler delay move it
        -- into the fixture-teardown suffix.
        _ <-
          awaitStep15WorkEvidenceSince
            preCrashCursors
            ledgers
            allSurvivorsObservedH4Loss
        postCrashSnapshots <-
          snapshotStep15WorkEvidenceSince preCrashCursors ledgers
        pure
          ( successorMembership,
            baselineEvidence,
            cursors,
            preCrashEvidence,
            fmap snd postCrashSnapshots,
            fmap fst postCrashSnapshots,
            ledgers
          )

  case observed of
    Nothing -> assertFailure "Step-15 terminal-source repair tour exceeded its failure bound"
    Just
      ( successorMembership,
        baselineEvidence,
        cursors,
        preCrashEvidence,
        postCrashEvidence,
        teardownCursors,
        ledgers
        ) -> do
        -- Returning from the scoped fixture has semantically drained and joined
        -- every surviving Herald (H4 was already joined by the crash).  The
        -- ledgers remain readable, so this is a closed observation interval:
        -- delayed timers, queued peer deliveries, reconnect attempts, and
        -- connection closes cannot land beyond the snapshot and evade the work
        -- assertions below.
        evidence <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
        teardownEvidence <-
          fmap (fmap snd) (snapshotStep15WorkEvidenceSince teardownCursors ledgers)
        assertNoH4BindingLossBeforeCrash preCrashEvidence
        assertCrashBindingLosses postCrashEvidence
        assertNoSurvivorBindingLossBeforeTeardown preCrashEvidence postCrashEvidence
        assertFinalTeardownBindingLosses teardownEvidence
        assertTerminalCausalChain successorMembership evidence
        let work = fmap summarizeStep15WorkEvidence evidence
        assertEqual
          "terminal-source repair introduces no kernel invariant fault"
          [0, 0, 0, 0]
          (fmap step15KernelFaults work)
        assertBool
          "a survivor requests at least one payload missing from its terminal archive"
          (sum (fmap step15TerminalPayloadRequests work) >= 1)
        assertBool
          "H2 relays at least one retained terminal occurrence"
          (sum (fmap step15TerminalOccurrenceReplays work) >= 1)
        assertBool
          "the survivors establish a terminal-source union for the successor cut"
          (sum (fmap step15TerminalUnionEstablishedMessages work) >= 2)
        assertBool
          "the announcer and both remote survivors install the successor structural base"
          (survivorBasesInstalled successorMembership evidence)
        assertTerminalWorkBounds
          successorMembership
          baselineEvidence
          evidence
          work

-- | Tie the real transport tour to one exact H4 occurrence instead of accepting
-- independent aggregate counters. The protocol projection is the observation
-- boundary here: it proves that the same coordinates and digests actually
-- crossed EPRP, while the pure protocol/core properties retain the canonical
-- decoding and algebraic details.
assertTerminalCausalChain ::
  HeraldMembershipGeneration ->
  [Step15WorkEvidence] ->
  Assertion
assertTerminalCausalChain successor evidence = do
  predecessor <-
    maybe
      (assertFailure "the Step-15 successor has no predecessor generation")
      pure
      (heraldMembershipGenerationPredecessor successor)
  retired <-
    maybe
      (assertFailure "the Step-15 successor names no retired Herald")
      pure
      (heraldMembershipGenerationRetiredHeraldEpoch successor)
  _retirementIndex <-
    maybe
      (assertFailure "the Step-15 successor has no retirement control index")
      pure
      (heraldMembershipGenerationRetirementControlIndex successor)
  let predecessorBytes = heraldMembershipGenerationIdBytes predecessor
      successorBytes =
        heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor)
      retiredBytes = heraldEpochBytes retired
      survivorNames = [Step15H1, Step15H2, Step15H3]
      survivorEpochBytes = sort (fmap (heraldEpochBytes . step15HeraldEpoch) survivorNames)
      named = zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence
      controls =
        [ (name, remote, control)
        | (name, observed) <- named,
          (remote, control) <- projectedPeerControls observed
        ]
      exactCoordinates predecessorClaim successorClaim retiredClaim =
        Protocol.heraldMembershipGenerationClaimBytes predecessorClaim == predecessorBytes
          && Protocol.heraldMembershipGenerationClaimBytes successorClaim == successorBytes
          && Protocol.heraldEpochClaimBytes retiredClaim == retiredBytes

  (occurrence, publicationDigest) <-
    case nub
      [ claim
      | (Step15H4, observed) <- named,
        claim <- projectedNeutralStructuralClaims observed
      ] of
      [claim] -> pure claim
      claims ->
        assertFailure
          ( "expected one exact H4 neutral-carrier occurrence after the work cursor; observed="
              <> show claims
          )

  let inventories =
        nub
          [ inventory
          | (_, _, Protocol.TerminalSourceInventoryAdvertisedDto inventory) <- controls
          ]
      inventoryCoordinatesValid = \case
        Protocol.TerminalSourceInventoryDto predecessorClaim successorClaim retiredClaim _ _ _ _ ->
          exactCoordinates predecessorClaim successorClaim retiredClaim
      inventoryReporter = \case
        Protocol.TerminalSourceInventoryDto _ _ _ reporter _ _ _ ->
          Protocol.heraldEpochClaimBytes reporter
  assertEqual
    "the three survivors advertise exactly one coordinate-consistent inventory each"
    survivorEpochBytes
    (sort (fmap inventoryReporter inventories))
  assertBool
    "every survivor inventory names the exact predecessor, successor, and retired H4"
    (all inventoryCoordinatesValid inventories)

  h2InventoryDigest <-
    case [ digest
         | Protocol.TerminalSourceInventoryDto _ _ _ reporter _ digest _ <- inventories,
           Protocol.heraldEpochClaimBytes reporter
             == heraldEpochBytes (step15HeraldEpoch Step15H2)
         ] of
      [digest] -> pure digest
      digests ->
        assertFailure
          ("expected one H2 inventory digest; observed=" <> show digests)

  let requests =
        nub
          [ (name, remote, request)
          | (name, remote, Protocol.TerminalSourcePayloadRequestedDto request) <- controls
          ]
      exactRequests =
        [ (name, remote)
        | ( name,
            remote,
            Protocol.TerminalSourcePayloadRequestDto
              predecessorClaim
              successorClaim
              retiredClaim
              requestedOccurrence
              supportingInventory
            ) <-
            requests,
          exactCoordinates predecessorClaim successorClaim retiredClaim,
          requestedOccurrence == occurrence,
          supportingInventory == h2InventoryDigest
        ]
  assertEqual
    "the two missing survivors request the exact H4 occurrence from H2's inventory"
    [(Step15H1, step15HeraldEpoch Step15H2), (Step15H3, step15HeraldEpoch Step15H2)]
    (sort exactRequests)

  let relays =
        nub
          [ (name, remote, relay)
          | (name, remote, Protocol.TerminalSourcePayloadRelayedDto relay) <- controls
          ]
      exactRelays =
        [ (name, remote)
        | ( name,
            remote,
            Protocol.TerminalSourcePayloadRelayDto
              predecessorClaim
              successorClaim
              retiredClaim
              relayHerald
              supportingInventory
              relayedOccurrence
              relayedDigest
              _
            ) <-
            relays,
          exactCoordinates predecessorClaim successorClaim retiredClaim,
          Protocol.heraldEpochClaimBytes relayHerald
            == heraldEpochBytes (step15HeraldEpoch Step15H2),
          supportingInventory == h2InventoryDigest,
          relayedOccurrence == occurrence,
          relayedDigest == publicationDigest
        ]
  assertEqual
    "H2 relays the identical occurrence and publication digest to both missing survivors"
    [(Step15H2, step15HeraldEpoch Step15H1), (Step15H2, step15HeraldEpoch Step15H3)]
    (sort exactRelays)

  (announcer, announcedUnion) <-
    case nub
      [ (announcer, union)
      | (_, _, Protocol.TerminalSourceUnionAnnouncedDto (Protocol.TerminalSourceUnionAnnounceDto announcer union)) <- controls
      ] of
      [announcement] -> pure announcement
      announcements ->
        assertFailure
          ("expected one exact terminal union announcement; observed=" <> show announcements)
  assertEqual
    "H1 is the deterministic successor announcer"
    (heraldEpochBytes (step15HeraldEpoch Step15H1))
    (Protocol.heraldEpochClaimBytes announcer)
  let Protocol.TerminalSourceUnionDto
        unionPredecessor
        unionSuccessor
        unionRetired
        unionDigest
        _ = announcedUnion
  assertBool
    "the announced union retains the exact retirement coordinates"
    (all (exactCoordinates unionPredecessor unionSuccessor) (NonEmpty.toList (Protocol.retiredHeraldEntries unionRetired)) && length (Protocol.retiredHeraldEntries unionRetired) == 1)
  unionContainsRepairedOccurrence <-
    case admitTerminalSourceUnionOccurrenceClaim
      announcedUnion
      occurrence
      publicationDigest of
      Left problem -> do
        assertFailure
          ("the announced terminal union failed authentication: " <> show problem)
        pure False
      Right present -> pure present
  assertBool
    "the authenticated union retains the exact repaired H4 occurrence and publication digest"
    unionContainsRepairedOccurrence

  established <-
    case nub
      [ value
      | (_, _, Protocol.TerminalSourceUnionEstablishedControlDto value) <- controls
      ] of
      [value] -> pure value
      values ->
        assertFailure
          ("expected one exact established terminal union; observed=" <> show values)
  let Protocol.TerminalSourceUnionEstablishedDto establishedUnion acceptances = established
      acceptanceEntries =
        NonEmpty.toList (Protocol.terminalSourceUnionAcceptanceEntries acceptances)
      acceptanceReporter =
        Protocol.heraldEpochClaimBytes
          . Protocol.terminalSourceUnionAcceptanceReporter
      acceptanceMatches = \case
        Protocol.TerminalSourceUnionAcceptanceDto
          predecessorClaim
          successorClaim
          retiredClaim
          acceptedUnionDigest
          _
          _ ->
            all (exactCoordinates predecessorClaim successorClaim) (NonEmpty.toList (Protocol.retiredHeraldEntries retiredClaim))
              && length (Protocol.retiredHeraldEntries retiredClaim) == 1
              && acceptedUnionDigest == unionDigest
  assertEqual
    "the established message contains the exact announced union"
    announcedUnion
    establishedUnion
  assertEqual
    "H1, H2, and H3 each accept that exact union"
    survivorEpochBytes
    (sort (fmap acceptanceReporter acceptanceEntries))
  assertBool
    "every acceptance names the exact union digest and retirement coordinates"
    (all acceptanceMatches acceptanceEntries)

  assertEqual
    "each survivor supplies exact successor installation evidence"
    survivorNames
    (exactSuccessorStructuralWitnesses successor evidence)

projectedPeerControls ::
  Step15WorkEvidence ->
  [(HeraldEpoch, Protocol.PeerControlDto)]
projectedPeerControls evidence =
  [ (peerBindingRemoteHeraldEpoch binding, dto)
  | ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerControl binding control)) <-
      step15RuntimeEvidence evidence,
    Right (Protocol.PeerControlEnvelope _ dto) <- [projectPeerControl control]
  ]

projectedNeutralStructuralClaims ::
  Step15WorkEvidence ->
  [(Protocol.StructuralOccurrenceIdDto, Protocol.StructuralPublicationDigestClaim)]
projectedNeutralStructuralClaims evidence =
  [ (occurrence, digest)
  | ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerItem _ attempt)) <-
      step15RuntimeEvidence evidence,
    Right
      ( Protocol.PeerPublicationEnvelope
          _
          ( Protocol.PeerPublicationDto
              _
              _
              _
              ( Protocol.StructuralPublicationDto
                  (Protocol.StructuralOccurrenceStampDto occurrence _ _ digest Protocol.NeutralVertexCarrierDto)
                  _
                )
            )
        ) <-
      [projectPeerLogicalAttempt attempt]
  ]

data HeldDestinations
  = HeldDestinations
      (TVar (Set HeraldEpoch))
      (MVar ())

withHeldH4Destinations ::
  Step15Deployment ->
  (HeldDestinations -> IO result) ->
  IO result
withHeldH4Destinations deployment use =
  bracket install cleanup use
  where
    tcp = step15ManagedHeraldTcp (step15H4 deployment)
    blocked = Set.fromList (fmap step15HeraldEpoch [Step15H1, Step15H3])
    install = do
      observed <- newTVarIO Set.empty
      release <- newEmptyMVar
      let hold binding _ =
            when (Set.member (peerBindingRemoteHeraldEpoch binding) blocked) $ do
              atomically
                (modifyTVar' observed (Set.insert (peerBindingRemoteHeraldEpoch binding)))
              readMVar release
      setPeerPublicationProbeForTest tcp (Just hold)
      pure (HeldDestinations observed release)
    cleanup gate = do
      releaseHeldDestinations gate
      setPeerPublicationProbeForTest tcp Nothing

awaitHeldDestinations :: HeldDestinations -> IO ()
awaitHeldDestinations (HeldDestinations observed _) = do
  held <- timeout progressTimeoutMicroseconds loop
  case held of
    Just actual ->
      assertEqual
        "the source publication reaches the H1 and H3 handoff boundaries"
        (Set.fromList (fmap step15HeraldEpoch [Step15H1, Step15H3]))
        actual
    Nothing -> assertFailure "H4 did not reach both held destination handoffs"
  where
    loop = do
      current <- atomically (readTVar observed)
      if Set.size current == 2 then pure current else threadDelay 10_000 >> loop

releaseHeldDestinations :: HeldDestinations -> IO ()
releaseHeldDestinations (HeldDestinations _ release) =
  void (tryPutMVar release ())

deploymentHeralds :: Step15Deployment -> [Step15Herald]
deploymentHeralds deployment =
  [step15H1 deployment, step15H2 deployment, step15H3 deployment, step15H4 deployment]

awaitH4Retirement :: Step15Deployment -> IO HeraldMembershipGeneration
awaitH4Retirement deployment = do
  observed <- timeout progressTimeoutMicroseconds loop
  maybe (assertFailure "Oracle did not retire H4 for terminal-source repair") pure observed
  where
    loop = do
      statuses <- oracleTcpNodeStatuses (step15DeploymentOracleCluster deployment)
      case nub (fmap (oracleRuntimeMembershipGeneration . snd) statuses) of
        [successor]
          | heraldMembershipGenerationRetiredHeraldEpoch successor
              == Just (step15HeraldEpoch Step15H4),
            sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor))
              == sort (fmap step15HeraldEpoch [Step15H1, Step15H2, Step15H3]) ->
              pure successor
        _ -> threadDelay 10_000 >> loop

awaitTerminalEvidence ::
  HeraldMembershipGeneration ->
  [Step15WorkCursor] ->
  [Step15WorkLedger] ->
  IO [Step15WorkEvidence]
awaitTerminalEvidence successor cursors ledgers = do
  observed <-
    timeout progressTimeoutMicroseconds
      $ awaitStep15WorkEvidenceSince cursors ledgers (terminalEvidenceComplete successor)
  case observed of
    Just evidence -> pure evidence
    Nothing -> do
      evidence <- fmap (fmap snd) (snapshotStep15WorkEvidenceSince cursors ledgers)
      assertFailure
        ( "terminal-source request/relay/union evidence did not complete; per-Herald work="
            <> show (fmap summarizeStep15WorkEvidence evidence)
            <> "; semantic diagnostic="
            <> show (terminalEvidenceDiagnostic successor evidence)
        )

terminalEvidenceComplete ::
  HeraldMembershipGeneration ->
  [Step15WorkEvidence] ->
  Bool
terminalEvidenceComplete successor evidence =
  sum (fmap step15TerminalPayloadRequests work) >= 1
    && sum (fmap step15TerminalOccurrenceReplays work) >= 1
    && sum (fmap step15TerminalUnionEstablishedMessages work) >= 2
    && requiredAlignmentEvidencePresent evidence
    && exactSuccessorStructuralWitnesses successor evidence
      == [Step15H1, Step15H2, Step15H3]
  where
    work = fmap summarizeStep15WorkEvidence evidence

exactSuccessorStructuralWitnesses ::
  HeraldMembershipGeneration ->
  [Step15WorkEvidence] ->
  [Step15HeraldName]
exactSuccessorStructuralWitnesses successor evidence =
  case heraldMembershipGenerationRetirementControlIndex successor of
    Nothing -> []
    Just minimumControl ->
      sort
        . nub
        $ [ name
          | (name, observed) <- zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence,
            (_, control) <- projectedPeerControls observed,
            name `elem` survivorNames,
            report <- installationReports name control,
            reportIsSuccessor minimumControl name report
          ]
  where
    survivorNames = [Step15H1, Step15H2, Step15H3]
    successorBytes =
      heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor)
    successorMemberDigest =
      memberSetDigestBytes
        (heraldMembershipGenerationActiveMemberSetDigest successor)
    -- Remote survivors report to H1. H1 retains its own report locally and
    -- carries that same report in its acceptance when establishing a later
    -- topology cut. Only H1's own acceptance witnesses H1's installation;
    -- remote acceptances do not substitute for their emitted reports.
    installationReports name = \case
      Protocol.StructuralAppliedReportedDto report -> [report]
      Protocol.TopologyCutEstablishedControlDto (Protocol.TopologyCutEstablishedDto establishedCut _ acceptances)
        | name == Step15H1 ->
            [ Protocol.StructuralAppliedReportDto reporter vector control
            | Protocol.TopologyCutAcceptanceDto acceptedCut reporter vector control <-
                NonEmpty.toList (Protocol.topologyCutAcceptanceEntries acceptances),
              acceptedCut == establishedCut,
              Protocol.heraldEpochClaimBytes reporter == heraldEpochBytes (step15HeraldEpoch Step15H1)
            ]
      _ -> []
    reportIsSuccessor minimumControl name = \case
      Protocol.StructuralAppliedReportDto reporter vector control ->
        Protocol.heraldEpochClaimBytes reporter
          == heraldEpochBytes (step15HeraldEpoch name)
          && Protocol.heraldMembershipGenerationClaimBytes
            (Protocol.structuralVersionVectorMembershipGeneration vector)
            == successorBytes
          && Protocol.memberSetDigestClaimBytes
            (Protocol.structuralVersionVectorMemberSetDigest vector)
            == successorMemberDigest
          && Protocol.controlIndexDtoWord64 control
            >= controlIndexWord64 minimumControl

data TerminalEvidenceDiagnostic = TerminalEvidenceDiagnostic
  { exactSuccessorWitnesses :: [Step15HeraldName],
    terminalOutputCounts :: [((Step15HeraldName, String), Int)],
    terminalInputCounts :: [((Step15HeraldName, String), Int)],
    terminalPayloadRequestCauses :: [((Step15HeraldName, String, String), Int)],
    alignmentOutputCounts :: [((Step15HeraldName, String), Int)],
    alignmentInputCounts :: [((Step15HeraldName, String), Int)],
    peerBindingLossCounts :: [((Step15HeraldName, HeraldEpoch), Int)],
    peerClosureCounts :: [((Step15HeraldName, HeraldEpoch, String), Int)],
    peerRejectionTriggerCounts ::
      [((Step15HeraldName, HeraldEpoch, String, String), Int)],
    rejectedSuccessorReportHistories ::
      [ ( (Step15HeraldName, HeraldEpoch),
          [([Protocol.StructuralVectorEntryDto], Word64, String)]
        )
      ],
    h1OrderedStructuralHistory :: [(String, String, HeraldEpoch, String)],
    kernelInputCategoryCounts :: [((Step15HeraldName, String), Int)]
  }
  deriving stock (Show)

terminalEvidenceDiagnostic ::
  HeraldMembershipGeneration ->
  [Step15WorkEvidence] ->
  TerminalEvidenceDiagnostic
terminalEvidenceDiagnostic successor evidence =
  TerminalEvidenceDiagnostic
    { exactSuccessorWitnesses = exactSuccessorStructuralWitnesses successor evidence,
      terminalOutputCounts = counted terminalOutputs,
      terminalInputCounts = counted terminalInputs,
      terminalPayloadRequestCauses = counted payloadRequestCauses,
      alignmentOutputCounts = counted alignmentOutputs,
      alignmentInputCounts = counted alignmentInputs,
      peerBindingLossCounts = counted peerBindingLosses,
      peerClosureCounts = counted peerClosures,
      peerRejectionTriggerCounts = counted peerRejectionTriggers,
      rejectedSuccessorReportHistories =
        [ ( pair,
            [ (entries, control, outcome)
            | (candidatePair, entries, control, outcome) <- successorReportEvents,
              candidatePair == pair
            ]
          )
        | pair <- rejectedSuccessorReportPairs
        ],
      h1OrderedStructuralHistory = case evidence of
        h1 : _ -> concatMap causalPeerEvents (step15RuntimeEvidence h1)
        [] -> [],
      kernelInputCategoryCounts = counted kernelInputCategories
    }
  where
    named = zip [Step15H1, Step15H2, Step15H3, Step15H4] evidence
    terminalOutputs =
      [ (name, phase)
      | (name, observed) <- named,
        ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerControl _ control)) <-
          step15RuntimeEvidence observed,
        Just phase <- [terminalControlPhase control]
      ]
    terminalInputs =
      [ (name, phase)
      | (name, observed) <- named,
        KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed,
        PeerInput (SemanticPeerControlReceived _ control) <- [inputBody input],
        Just phase <- [terminalControlPhase control]
      ]
    alignmentOutputs =
      [ (name, constructorName control)
      | (name, observed) <- named,
        ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerControl _ (SemanticPeerAlignmentControl control))) <-
          step15RuntimeEvidence observed
      ]
    payloadRequestCauses =
      [ (name, show ordinal, kernelInputCategory (inputBody input))
      | (name, observed) <- named,
        KernelEvent ordinal (KernelTraceStepped _ _ input (Right effects)) <- step15RuntimeEvidence observed,
        SemanticSendPeerControl _ PeerTerminalSourcePayloadRequested {} <- effectBatchMembers effects
      ]
    alignmentInputs =
      [ (name, constructorName control)
      | (name, observed) <- named,
        KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed,
        PeerInput (SemanticPeerControlReceived _ (SemanticPeerAlignmentControl control)) <- [inputBody input]
      ]
    peerBindingLosses =
      [ (name, peerBindingRemoteHeraldEpoch binding)
      | (name, observed) <- named,
        KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed,
        RuntimeObserved (PeerBindingLost binding) <- [inputBody input]
      ]
    peerClosures =
      [ (name, peerBindingRemoteHeraldEpoch binding, reason)
      | (name, observed) <- named,
        ShellEvent _ (ShellEffectRouted _ _ effect) <- step15RuntimeEvidence observed,
        (binding, reason) <- case effect of
          RejectPeerConnection rejected disposition -> [(rejected, show disposition)]
          ClosePeerBinding closed -> [(closed, "ClosePeerBinding")]
          _ -> []
      ]
    peerRejectionTriggers =
      [ ( name,
          peerBindingRemoteHeraldEpoch binding,
          peerControlCoordinates successor (peerBindingRemoteHeraldEpoch binding) control,
          show disposition
        )
      | (name, observed) <- named,
        KernelEvent _ (KernelTraceStepped _ _ input (Right effects)) <-
          step15RuntimeEvidence observed,
        PeerInput (SemanticPeerControlReceived binding control) <- [inputBody input],
        RejectPeerConnection _ disposition <- effectBatchMembers effects
      ]
    rejectedSuccessorReportPairs =
      nub
        [ pair
        | (pair, _, _, "rejected") <- successorReportEvents
        ]
    successorReportEvents =
      [ ( (name, peerBindingRemoteHeraldEpoch binding),
          NonEmpty.toList (Protocol.structuralVersionVectorEntries vector),
          Protocol.controlIndexDtoWord64 control,
          case result of
            Left _ -> "kernel-fault"
            Right effects
              | any isPeerRejection (effectBatchMembers effects) -> "rejected"
              | otherwise -> "admitted"
        )
      | (name, observed) <- named,
        KernelEvent _ (KernelTraceStepped _ _ input result) <- step15RuntimeEvidence observed,
        PeerInput (SemanticPeerControlReceived binding peerControl) <- [inputBody input],
        Right
          ( Protocol.PeerControlEnvelope
              _
              ( Protocol.StructuralAppliedReportedDto
                  (Protocol.StructuralAppliedReportDto _ vector control)
                )
            ) <-
          [projectPeerControl peerControl],
        Protocol.heraldMembershipGenerationClaimBytes
          (Protocol.structuralVersionVectorMembershipGeneration vector)
          == successorBytes
      ]
    successorBytes =
      heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor)
    isPeerRejection = \case
      RejectPeerConnection {} -> True
      _ -> False
    causalPeerEvents event = case event of
      KernelEvent ordinal (KernelTraceStepped _ _ input _) ->
        case inputBody input of
          PeerInput (SemanticPeerControlReceived binding control)
            | structuralCausalControl control ->
                [ ( show ordinal,
                    "input",
                    peerBindingRemoteHeraldEpoch binding,
                    peerControlCoordinates successor (peerBindingRemoteHeraldEpoch binding) control
                  )
                ]
          _ -> []
      ShellEvent ordinal (ShellEffectRouted _ _ (SemanticSendPeerControl binding control))
        | structuralCausalControl control ->
            [ ( show ordinal,
                "output",
                peerBindingRemoteHeraldEpoch binding,
                peerControlCoordinates successor (step15HeraldEpoch Step15H1) control
              )
            ]
      _ -> []
    kernelInputCategories =
      [ (name, kernelInputCategory (inputBody input))
      | (name, observed) <- named,
        KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed
      ]

terminalControlPhase :: PeerControl -> Maybe String
terminalControlPhase = \case
  PeerStructuralAppliedReported {} -> Just "StructuralAppliedReport"
  PeerTerminalSourceInventoryAdvertised {} -> Just "TerminalInventory"
  PeerTerminalSourcePayloadRequested {} -> Just "TerminalPayloadRequest"
  PeerTerminalSourcePayloadRelayed {} -> Just "TerminalPayloadRelay"
  PeerTerminalSourceUnionAnnounced {} -> Just "TerminalUnionAnnounce"
  PeerTerminalSourceUnionAccepted {} -> Just "TerminalUnionAccept"
  PeerTerminalSourceUnionEstablished {} -> Just "TerminalUnionEstablished"
  _ -> Nothing

peerControlConstructor :: PeerControl -> String
peerControlConstructor control =
  case control of
    SemanticPeerAlignmentControl alignment ->
      "PeerAlignmentControl:" <> constructorName alignment
    _ -> case terminalControlPhase control of
      Just phase -> phase
      Nothing -> takeWhile (/= ' ') (show control)

structuralCausalControl :: PeerControl -> Bool
structuralCausalControl = \case
  PeerStructuralAppliedReported {} -> True
  PeerTopologyCutAnnounced {} -> True
  PeerTopologyCutAccepted {} -> True
  PeerTopologyCutEstablished {} -> True
  PeerTopologyCutEstablishedAcknowledged {} -> True
  PeerTerminalSourceInventoryAdvertised {} -> True
  PeerTerminalSourcePayloadRequested {} -> True
  PeerTerminalSourcePayloadRelayed {} -> True
  PeerTerminalSourceUnionAnnounced {} -> True
  PeerTerminalSourceUnionAccepted {} -> True
  PeerTerminalSourceUnionEstablished {} -> True
  _ -> False

peerControlCoordinates ::
  HeraldMembershipGeneration ->
  HeraldEpoch ->
  PeerControl ->
  String
peerControlCoordinates successor remote control =
  case projectPeerControl control of
    Right (Protocol.PeerControlEnvelope _ dto) -> case dto of
      Protocol.StructuralAppliedReportedDto
        (Protocol.StructuralAppliedReportDto reporter vector appliedControl) ->
          "StructuralAppliedReport{reporterMatchesRemote="
            <> show (reporterMatches reporter)
            <> ",membership="
            <> membershipRelation
              (Protocol.structuralVersionVectorMembershipGeneration vector)
            <> ",vector="
            <> show (NonEmpty.toList (Protocol.structuralVersionVectorEntries vector))
            <> ",control="
            <> show (Protocol.controlIndexDtoWord64 appliedControl)
            <> "}"
      Protocol.TopologyCutAcceptedDto
        (Protocol.TopologyCutAcceptanceDto cut reporter vector appliedControl) ->
          "TopologyCutAccepted{reporterMatchesRemote="
            <> show (reporterMatches reporter)
            <> ",membership="
            <> membershipRelation
              (Protocol.structuralVersionVectorMembershipGeneration vector)
            <> ",cut="
            <> show (Protocol.topologyCutClaimBytes cut)
            <> ",control="
            <> show (Protocol.controlIndexDtoWord64 appliedControl)
            <> "}"
      Protocol.TopologyCutAnnouncedDto
        (Protocol.TopologyCutAnnounceDto announcer cut _) ->
          "TopologyCutAnnounced{announcer="
            <> show (Protocol.heraldEpochClaimBytes announcer)
            <> ",cut="
            <> show (Protocol.topologyCutClaimBytes cut)
            <> "}"
      Protocol.TopologyCutEstablishedControlDto
        (Protocol.TopologyCutEstablishedDto cut _ acceptances) ->
          "TopologyCutEstablished{cut="
            <> show (Protocol.topologyCutClaimBytes cut)
            <> ",acceptanceControls="
            <> show
              [ Protocol.controlIndexDtoWord64 appliedControl
              | Protocol.TopologyCutAcceptanceDto _ _ _ appliedControl <-
                  NonEmpty.toList (Protocol.topologyCutAcceptanceEntries acceptances)
              ]
            <> "}"
      Protocol.TopologyCutEstablishedAcknowledgedDto
        (Protocol.TopologyCutEstablishedAckDto cut reporter generation) ->
          "TopologyCutEstablishedAck{reporterMatchesRemote="
            <> show (reporterMatches reporter)
            <> ",membership="
            <> membershipRelation generation
            <> ",cut="
            <> show (Protocol.topologyCutClaimBytes cut)
            <> "}"
      _ -> peerControlConstructor control
    Right _ -> "non-control-envelope:" <> peerControlConstructor control
    Left problem -> "unprojectable:" <> show problem
  where
    reporterMatches reporter =
      Protocol.heraldEpochClaimBytes reporter == heraldEpochBytes remote
    membershipRelation generation
      | claimed == successorBytes = "successor"
      | Just claimed == predecessorBytes = "predecessor"
      | otherwise = "other:" <> show claimed
      where
        claimed = Protocol.heraldMembershipGenerationClaimBytes generation
    successorBytes =
      heraldMembershipGenerationIdBytes (heraldMembershipGenerationId successor)
    predecessorBytes =
      fmap
        heraldMembershipGenerationIdBytes
        (heraldMembershipGenerationPredecessor successor)

counted :: (Ord value) => [value] -> [(value, Int)]
counted = concatMap countGroup . group . sort
  where
    countGroup [] = []
    countGroup (first : remaining) = [(first, 1 + length remaining)]

kernelInputCategory :: HeraldInputBody -> String
kernelInputCategory = \case
  ApplicationReceiptRetirementInput {} -> "application-receipt-retirement"
  ApplicationSessionInput ingress -> "ApplicationSession:" <> constructorName ingress
  ApplicationRequestInput ingress -> "ApplicationRequest:" <> constructorName ingress
  ApplicationLifecycleInput ingress -> "ApplicationLifecycle:" <> constructorName ingress
  AdministrationInput ingress -> "Administration:" <> constructorName ingress
  DisappearanceInput ingress -> "Disappearance:" <> constructorName ingress
  PeerInput ingress -> case ingress of
    PeerCandidateOpened {} -> "PeerCandidateOpened"
    PeerHelloReceived {} -> "PeerHelloReceived"
    SemanticPeerControlReceived _ control -> "PeerControl:" <> peerControlConstructor control
    SemanticPeerPublicationReceived {} -> "PeerPublicationReceived"
    PeerDispatchSelected {} -> "PeerDispatchSelected"
  JoinInput {} -> "Join"
  OracleInput ingress -> "Oracle:" <> constructorName ingress
  RuntimeObserved observation -> case observation of
    OracleHealthRoundObserved {} -> "Runtime:OracleHealthRoundObserved"
    ApplicationCandidateLost {} -> "Runtime:ApplicationCandidateLost"
    ApplicationBindingLost {} -> "Runtime:ApplicationBindingLost"
    AdministrationBindingLost {} -> "Runtime:AdministrationBindingLost"
    DrainBarrierObserved {} -> "Runtime:DrainBarrierObserved"
    PeerBindingLost {} -> "Runtime:PeerBindingLost"
    RetiredLocalHeraldEpochRejected {} -> "Runtime:RetiredLocalHeraldEpochRejected"
    PeerDispatchObserved {} -> "Runtime:PeerDispatchObserved"
    TimerObserved {} -> "Runtime:TimerObserved"

constructorName :: (Show value) => value -> String
constructorName = takeWhile (\character -> character /= ' ' && character /= '{') . show

-- The fixture orders ledgers H1-H4, with H1 the deterministic announcer.
-- H1's emitted topology certificate carries its local installation report;
-- H2 and H3 send their reports to H1. Check the exact successor coordinates
-- rather than treating a received or held Established control as installation.
survivorBasesInstalled :: HeraldMembershipGeneration -> [Step15WorkEvidence] -> Bool
survivorBasesInstalled successor evidence@(h1 : _ : _ : _) =
  step15TerminalUnionEstablishedMessages (summarizeStep15WorkEvidence h1) >= 2
    && exactSuccessorStructuralWitnesses successor evidence
      == [Step15H1, Step15H2, Step15H3]
survivorBasesInstalled _ _ = False

expectedAlignmentInputTotal :: Int
expectedAlignmentInputTotal = 600

data AlignmentControlKind
  = PlanAnnouncedKind
  | PlanAcceptanceAdvertisedKind
  | CutAnnouncedKind
  | CutAcceptanceAdvertisedKind
  | MemberReadyAdvertisedKind
  | PlanObsoleteAdvertisedKind
  | HistoricalCertificateAdvertisedKind
  | SubscribeRequestedKind
  | SnapshotStartedKind
  | SnapshotChunkTransferredKind
  | SnapshotEndedKind
  | ChangeTransferredKind
  | LiveAdvertisedKind
  | AlignmentAcknowledgedKind
  | AlignmentCancelledKind
  | AlignmentProbeMarkerKind
  deriving stock (Eq, Ord, Show)

peerAlignmentControlKind :: PeerControl -> Maybe AlignmentControlKind
peerAlignmentControlKind control =
  case projectPeerControl control of
    Right (Protocol.PeerControlEnvelope _ (Protocol.AlignmentEvidenceDeliveredDto _ evidence)) ->
      Just $ case evidence of
        Protocol.AlignmentPlanAcceptanceEvidenceDto {} -> PlanAcceptanceAdvertisedKind
        Protocol.AlignmentCutAcceptanceEvidenceDto {} -> CutAcceptanceAdvertisedKind
        Protocol.AlignmentMemberReadyEvidenceDto {} -> MemberReadyAdvertisedKind
        Protocol.AlignmentPlanObsoleteEvidenceDto {} -> PlanObsoleteAdvertisedKind
    Right
      ( Protocol.PeerControlEnvelope
          _
          (Protocol.AlignmentControlDto alignment)
        ) ->
        Just $ case alignment of
          Protocol.AlignmentPlanAnnouncedDto {} -> PlanAnnouncedKind
          Protocol.AlignmentCutAnnouncedDto {} -> CutAnnouncedKind
          Protocol.AlignmentHistoricalCertificateAdvertisedDto {} ->
            HistoricalCertificateAdvertisedKind
          Protocol.AlignmentSubscribeRequestedDto {} -> SubscribeRequestedKind
          Protocol.AlignmentSnapshotStartedDto {} -> SnapshotStartedKind
          Protocol.AlignmentSnapshotChunkTransferredDto {} -> SnapshotChunkTransferredKind
          Protocol.AlignmentSnapshotEndedDto {} -> SnapshotEndedKind
          Protocol.AlignmentChangeTransferredDto {} -> ChangeTransferredKind
          Protocol.AlignmentLiveAdvertisedDto {} -> LiveAdvertisedKind
          Protocol.AlignmentAcknowledgedDto {} -> AlignmentAcknowledgedKind
          Protocol.AlignmentCancelledDto {} -> AlignmentCancelledKind
          Protocol.AlignmentProbeMarkerDto {} -> AlignmentProbeMarkerKind
    _ -> Nothing

-- H1 is the least successor and announces the coordinated plans; it does not
-- receive its own announcements. The other evidence families reach all three
-- survivors. Physical counts may shrink when receipt progress suppresses replay.
requiredAlignmentInputFamilies :: [(Step15HeraldName, AlignmentControlKind)]
requiredAlignmentInputFamilies =
  [(herald, PlanAnnouncedKind) | herald <- [Step15H2, Step15H3]]
    <> [(herald, family) | herald <- [Step15H1, Step15H2, Step15H3], family <- [PlanAcceptanceAdvertisedKind, MemberReadyAdvertisedKind, HistoricalCertificateAdvertisedKind]]

requiredAlignmentEvidencePresent :: [Step15WorkEvidence] -> Bool
requiredAlignmentEvidencePresent evidence =
  all (`elem` fmap fst (alignmentInputHistogram evidence)) requiredAlignmentInputFamilies
    && or
      [ peerAlignmentControlKind control == Just PlanAnnouncedKind
      | (Step15H1, observed) <- namedEvidence evidence,
        ShellEvent _ (ShellEffectRouted _ _ (SemanticSendPeerControl _ control)) <-
          step15RuntimeEvidence observed
      ]

alignmentInputHistogram ::
  [Step15WorkEvidence] ->
  [((Step15HeraldName, AlignmentControlKind), Int)]
alignmentInputHistogram evidence =
  counted
    [ (name, kind)
    | (name, observed) <- namedEvidence evidence,
      KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed,
      PeerInput (SemanticPeerControlReceived _ control@SemanticPeerAlignmentControl {}) <- [inputBody input],
      Just kind <- [peerAlignmentControlKind control]
    ]

nonAlignmentKernelInputCount :: [Step15WorkEvidence] -> Int
nonAlignmentKernelInputCount evidence =
  length
    [ ()
    | (_, observed) <- namedEvidence evidence,
      KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed,
      not (isAlignmentInput (inputBody input))
    ]
  where
    isAlignmentInput = \case
      PeerInput (SemanticPeerControlReceived _ (SemanticPeerAlignmentControl _)) -> True
      _ -> False

postCursorPeerPromotions ::
  [Step15WorkEvidence] ->
  [(Step15HeraldName, HeraldEpoch)]
postCursorPeerPromotions evidence =
  [ (name, peerBindingRemoteHeraldEpoch binding)
  | (name, observed) <- namedEvidence evidence,
    ShellEvent _ (ShellConnectionPromoted _ _ (PeerConnectionPromoted binding)) <-
      step15RuntimeEvidence observed
  ]

postCursorPeerAdmissions ::
  [Step15WorkEvidence] ->
  [(Step15HeraldName, HeraldEpoch, String)]
postCursorPeerAdmissions evidence =
  [ (name, peerBindingRemoteHeraldEpoch binding, constructorName admission)
  | (name, observed) <- namedEvidence evidence,
    ShellEvent
      _
      ( ShellEffectRouted
          _
          _
          (SetPeerCandidateDisposition _ (PeerHelloAccepted admission binding))
        ) <-
      step15RuntimeEvidence observed
  ]

postCursorPeerHellos ::
  [Step15WorkEvidence] ->
  [(Step15HeraldName, HeraldEpoch)]
postCursorPeerHellos evidence =
  [ (name, peerHelloHeraldEpoch hello)
  | (name, observed) <- namedEvidence evidence,
    KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed,
    PeerInput (PeerHelloReceived _ _ hello _ _ _) <- [inputBody input]
  ]

bindingLosses ::
  [Step15WorkEvidence] ->
  [(Step15HeraldName, HeraldEpoch)]
bindingLosses evidence =
  [ (name, peerBindingRemoteHeraldEpoch binding)
  | (name, observed) <- namedEvidence evidence,
    KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed,
    RuntimeObserved (PeerBindingLost binding) <- [inputBody input]
  ]

h4BindingLosses ::
  [Step15WorkEvidence] ->
  [(Step15HeraldName, HeraldEpoch)]
h4BindingLosses = filter involvesH4 . bindingLosses
  where
    involvesH4 (name, remote) =
      name == Step15H4 || remote == step15HeraldEpoch Step15H4

survivorBindingLosses ::
  [Step15WorkEvidence] ->
  [(Step15HeraldName, HeraldEpoch)]
survivorBindingLosses = filter isSurvivorLink . bindingLosses
  where
    isSurvivorLink (name, remote) =
      name `elem` survivorNames && remote `elem` survivorEpochs
    survivorNames = [Step15H1, Step15H2, Step15H3]
    survivorEpochs = fmap step15HeraldEpoch survivorNames

assertNoH4BindingLossBeforeCrash :: [Step15WorkEvidence] -> Assertion
assertNoH4BindingLossBeforeCrash evidence =
  assertEqual
    "no H4 link was lost before the deliberate crash boundary"
    []
    (h4BindingLosses evidence)

allSurvivorsObservedH4Loss :: [Step15WorkEvidence] -> Bool
allSurvivorsObservedH4Loss evidence =
  all (`elem` bindingLosses evidence) expected
  where
    expected =
      [ (name, step15HeraldEpoch Step15H4)
      | name <- [Step15H1, Step15H2, Step15H3]
      ]

assertCrashBindingLosses :: [Step15WorkEvidence] -> Assertion
assertCrashBindingLosses evidence = do
  let losses = h4BindingLosses evidence
      h4Epoch = step15HeraldEpoch Step15H4
      survivorObserved =
        sort
          [ pair
          | pair@(name, remote) <- losses,
            name `elem` survivorNames,
            remote == h4Epoch
          ]
      h4Observed =
        sort
          [ pair
          | pair@(name, remote) <- losses,
            name == Step15H4,
            remote `elem` survivorEpochs
          ]
      expectedSurvivorObserved =
        sort [(name, h4Epoch) | name <- survivorNames]
      allowedH4Observed =
        [(Step15H4, remote) | remote <- survivorEpochs]
  assertEqual
    "each live survivor observes its exact H4 binding once after the deliberate crash"
    expectedSurvivorObserved
    survivorObserved
  assertBool
    ( "H4's cancellation-side loss observations name only its three survivor links; observed="
        <> show h4Observed
    )
    (all (`elem` allowedH4Observed) h4Observed)
  assertEqual
    "H4's structured-cancellation race admits each reciprocal link loss at most once"
    (sort (nub h4Observed))
    h4Observed
  assertEqual
    "the crash phase contains no other H4-related binding-loss shape"
    (sort (survivorObserved <> h4Observed))
    (sort losses)
  where
    survivorNames = [Step15H1, Step15H2, Step15H3]
    survivorEpochs = fmap step15HeraldEpoch survivorNames

assertNoSurvivorBindingLossBeforeTeardown ::
  [Step15WorkEvidence] ->
  [Step15WorkEvidence] ->
  Assertion
assertNoSurvivorBindingLossBeforeTeardown preCrash postCrash = do
  assertEqual
    "the survivor mesh remains intact before the deliberate H4 crash"
    []
    (survivorBindingLosses preCrash)
  assertEqual
    "the survivor mesh remains intact from the H4 crash through semantic completion"
    []
    (survivorBindingLosses postCrash)

assertFinalTeardownBindingLosses :: [Step15WorkEvidence] -> Assertion
assertFinalTeardownBindingLosses evidence = do
  let allLosses = bindingLosses evidence
      losses = survivorBindingLosses evidence
      allowed =
        [ (Step15H1, step15HeraldEpoch Step15H2),
          (Step15H1, step15HeraldEpoch Step15H3),
          (Step15H2, step15HeraldEpoch Step15H3)
        ]
  assertEqual
    "all H4 link losses complete before survivor fixture teardown begins"
    []
    (h4BindingLosses evidence)
  assertEqual
    "the final teardown suffix contains no binding loss outside the survivor mesh"
    (sort losses)
    (sort allLosses)
  assertBool
    ( "survivor teardown reports only a still-running Herald's loss of an earlier-drained peer; observed="
        <> show losses
    )
    (all (`elem` allowed) losses)
  assertEqual
    "survivor teardown admits each drain-induced directed binding loss at most once"
    (sort (nub losses))
    (sort losses)

postCursorSurvivorClosures ::
  [Step15WorkEvidence] ->
  [(Step15HeraldName, HeraldEpoch, String)]
postCursorSurvivorClosures evidence =
  [ (name, remote, reason)
  | (name, observed) <- namedEvidence evidence,
    name `elem` survivorNames,
    ShellEvent _ (ShellEffectRouted _ _ effect) <- step15RuntimeEvidence observed,
    (remote, reason) <- case effect of
      RejectPeerConnection binding disposition ->
        [(peerBindingRemoteHeraldEpoch binding, show disposition)]
      ClosePeerBinding binding ->
        [(peerBindingRemoteHeraldEpoch binding, "ClosePeerBinding")]
      _ -> [],
    remote `elem` survivorEpochs
  ]
  where
    survivorNames = [Step15H1, Step15H2, Step15H3]
    survivorEpochs = fmap step15HeraldEpoch survivorNames

namedEvidence :: [Step15WorkEvidence] -> [(Step15HeraldName, Step15WorkEvidence)]
namedEvidence = zip [Step15H1, Step15H2, Step15H3, Step15H4]

assertTerminalWorkBounds ::
  HeraldMembershipGeneration ->
  [Step15WorkEvidence] ->
  [Step15WorkEvidence] ->
  [Step15AttributableWork] ->
  Assertion
assertTerminalWorkBounds successor baseline evidence work = do
  let alignmentInputs =
        [ ( peerBindingRemoteHeraldEpoch binding,
            step15HeraldEpoch name,
            control
          )
        | (name, observed) <- namedEvidence evidence,
          KernelEvent _ (KernelTraceStepped _ _ input _) <- step15RuntimeEvidence observed,
          PeerInput (SemanticPeerControlReceived binding (SemanticPeerAlignmentControl control)) <- [inputBody input]
        ]
      alignmentOutputs =
        [ ( step15HeraldEpoch name,
            peerBindingRemoteHeraldEpoch binding,
            control
          )
        | (name, observed) <- namedEvidence evidence,
          ShellEvent
            _
            (ShellEffectRouted _ _ (SemanticSendPeerControl binding (SemanticPeerAlignmentControl control))) <-
            step15RuntimeEvidence observed
        ]
      duplicateAlignmentOutputs =
        nub
          [ output
          | output <- alignmentOutputs,
            length (filter (== output) alignmentOutputs) > 1
          ]
      unmatchedAlignmentOutputs =
        take 8 [output | output <- alignmentOutputs, output `notElem` alignmentInputs]
      unmatchedAlignmentInputs =
        take 8 [input | input <- alignmentInputs, input `notElem` alignmentOutputs]

  -- The successor cut deterministically releases one finite alignment
  -- catalogue. Require the semantic families and exact directed deliveries;
  -- receipt coalescing may reduce the physical wave below its former size.
  bound "non-alignment kernel inputs" 512 (nonAlignmentKernelInputCount evidence)
  bound "alignment inputs" expectedAlignmentInputTotal (length alignmentInputs)
  assertBool
    "H1 announces successor plans and every survivor receives its required alignment evidence"
    (requiredAlignmentEvidencePresent evidence)
  bound
    "alignment outputs"
    expectedAlignmentInputTotal
    (length alignmentOutputs)
  assertEqual
    "each exact directed successor alignment control is emitted at most once"
    []
    duplicateAlignmentOutputs
  assertEqual
    "every emitted successor alignment control reaches its exact destination"
    []
    unmatchedAlignmentOutputs
  assertEqual
    "every received successor alignment control has one exact directed emission"
    []
    unmatchedAlignmentInputs
  assertEqual
    "the post-cursor successor wave does not promote another peer connection"
    []
    (postCursorPeerPromotions evidence)
  assertEqual
    "the post-cursor successor wave does not admit a replacement peer binding"
    []
    (postCursorPeerAdmissions evidence)
  assertEqual
    "the post-cursor successor wave does not receive another peer Hello"
    []
    (postCursorPeerHellos evidence)
  assertEqual
    "the successor wave never closes a survivor-to-survivor binding"
    []
    (postCursorSurvivorClosures evidence)
  bound "physical peer dials" 384 physicalDials
  bound "complete timer lifecycle" 1_024 (sumField step15TimerLifecycleEvents)
  bound "complete peer-dial lifecycle" 768 (sumField step15PeerDialLifecycleEvents)
  bound "complete heartbeat lifecycle" 256 (sumField step15HeartbeatLifecycleEvents)
  bound "complete Oracle lifecycle" 512 (sumField step15OracleLifecycleEvents)
  bound "Oracle submissions" 64 (sumField step15OracleSubmissions)
  bound "failure-probe messages" 64 failureProbeMessages
  bound "terminal inventories" 12 (sumField step15TerminalSourceInventories)
  bound "terminal payload requests" 4 (sumField step15TerminalPayloadRequests)
  bound "terminal payload relays" 4 (sumField step15TerminalOccurrenceReplays)
  bound "terminal union messages" 12 (sumField step15TerminalUnionMessages)
  bound "structural-applied reports" 256 (sumField step15StructuralAppliedReports)
  where
    sumField project = sum (fmap project work)
    physicalDials =
      sumField (\item -> step15ConfiguredSeedDialAttempts item + step15PeerDialAttempts item)
    failureProbeMessages =
      sumField (\item -> step15FailureProbeRequests item + step15FailureProbeResponses item)
    bound description upper observed =
      assertBool
        ( "Step-15 terminal repair exceeded the attributable "
            <> description
            <> " bound "
            <> show upper
            <> "; observed="
            <> show observed
            <> "; per-Herald="
            <> show work
            <> "; semantic diagnostic="
            <> show (terminalEvidenceDiagnostic successor evidence)
            <> "; pre-cursor per-Herald work="
            <> show (fmap summarizeStep15WorkEvidence baseline)
        )
        (observed <= upper)

awaitStoreGrowth :: String -> Int -> IO [value] -> IO [value]
awaitStoreGrowth context predecessor action = do
  observed <- timeout progressTimeoutMicroseconds loop
  maybe (assertFailure (context <> " did not advance")) pure observed
  where
    loop = do
      current <- action
      if length current > predecessor then pure current else threadDelay 10_000 >> loop

scenarioTimeoutMicroseconds, progressTimeoutMicroseconds :: Int
scenarioTimeoutMicroseconds = 25_000_000
progressTimeoutMicroseconds = 10_000_000
