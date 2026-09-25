{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Live Oracle commands and watch projection drive both complete terminal
-- applicators. Remote absence claims come from independently serialized owners
-- and complete ordered marker cuts, never an injected Oracle report receipt.
module DisappearanceLiveProperties (tests, prepareLiveOpen, prepareLiveResolve, prepareLiveResolveNetwork, submitEnvelope, submitLocalRequest, step, LiveNetwork, prepareQuietNetwork, commitNetwork, commitNetworkWithEffects, drainNetwork, networkMessages, bindOracle, connectAllPeers) where

import ApplicationLabelProperties (locallyTakenNeutralDisappearanceFixture)
import Control.Monad (filterM, foldM, forM, forM_)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Disappearance qualified as Domain
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Publication (checkedPublicationSort)
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Evidence qualified as Evidence
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.EffectBatch (EffectBatch, HeraldEffect (..), effectBatchIsEmpty, effectBatchMembers, orderedEffectBatch)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (..),
    heraldInput,
  )
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction (..),
    OracleClientIngress (..),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.Request (oracleRequestDispatchEnvelope)
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Peer.RPC
  ( EstablishedPeerInbound (..),
    admitEstablishedPeerEnvelope,
    projectPeerControlWithProgress,
    projectPeerLogicalAttempt,
    projectPeerLogicalAttemptWithProgress,
  )
import Eclips.Herald.PeerPayload qualified as Payload
import Eclips.Herald.PeerStream qualified as Stream
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement (PlacementUpdate (FullPlacementSnapshot))
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.UseCase.Disappearance qualified as DisappearanceReplay
import Eclips.Herald.UseCase.DisappearanceLive qualified as Live
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.PeerDelivery qualified as PeerDelivery
import Eclips.Oracle.Canonical
  ( canonicalAppliedOracleEntryBytes,
    canonicalOracleEnvelopeValue,
    canonicalizeAppliedOracleEntry,
    decodeCanonicalAppliedOracleEntry,
  )
import Eclips.Oracle.Command (OracleEnvelope)
import Eclips.Oracle.Effect (OracleEffect (EmitAppliedOracleEntry), OracleStepOutcome (OracleCommitted), oracleEffects)
import Eclips.Oracle.Projection
  ( AppliedOracleEntry,
    OracleProjectionEventView (..),
    appliedEntryControlIndex,
    appliedEntryProjectionEvents,
    oracleProjectionEventView,
  )
import Eclips.Oracle.State (OracleState)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Protocol.Peer.Codec (decodePeerEnvelope, encodePeerEnvelope)
import Eclips.Protocol.Peer.Types (PeerEnvelope)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import GenesisFixtures
  ( currentPeerHelloReceived,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedInitialBootstrapsFor,
    fixtureCheckedOracleGenesisFor,
    fixtureDeploymentAt,
    fixtureGeneratorSeed,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
import PeerInputProperties (assertMarkerWorkReference)
import Step16RegularRetirementAcceptanceProperties (liveRegularDisappearanceFixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "live disappearance workflows"
    [ testCase "controlled NeutralVertex reaches live Oracle Resolve and terminal removal" caseControlled,
      testCase "regular definition reaches live Oracle Resolve and atomic retirement" caseRegular
    ]

caseControlled :: Assertion
caseControlled = do
  (seed, object, _) <- locallyTakenNeutralDisappearanceFixture
  let subjects =
        [ candidate.candidateSubject
        | candidate <- Disappearance.candidateWitnesses (startupDisappearanceState seed),
          Domain.ControlledPredefinedSubjectView candidateObject _ _ <-
            [Domain.disappearanceSubjectView (candidate.candidateSubject)],
          candidateObject == object
        ]
  subject <- exactlyOne "accepted LocalTake controlled candidate" subjects
  record <-
    require
      "controlled object before live disappearance"
      (Controlled.controlledLocalRecord object (startupControlledState seed))
  let sortId = checkedPublicationSort (Controlled.controlledRecordLatestPublication record)
  successor <- runLiveWorkflow seed subject
  assertBool
    "Resolve installs the controlled terminal deletion"
    (Controlled.controlledObjectTerminallyDeleted object (startupControlledState successor))
  assertEqual
    "the carrier sort remains effective"
    True
    ( case SortRegistry.lookupEffectiveSort sortId (startupSortRegistryState successor) of
        Just _ -> True
        Nothing -> False
    )

caseRegular :: Assertion
caseRegular = do
  let (seed, subject) = liveRegularDisappearanceFixture
  successor <- runLiveWorkflow seed subject
  case Domain.disappearanceSubjectView subject of
    Domain.RegularSortDefinitionSubjectView sortId _ _ -> do
      assertEqual
        "Resolve removes the exact effective regular occurrence"
        Nothing
        (SortRegistry.lookupEffectiveSort sortId (startupSortRegistryState successor))
      assertBool
        "Resolve retains the exact regular retirement authority"
        ( case SortRegistry.latestRegularSortRetirement sortId (startupSortRegistryState successor) of
            Just _ -> True
            Nothing -> False
        )
    _ -> assertFailure "regular fixture returned the controlled subject arm"

runLiveWorkflow :: HeraldState -> Domain.DisappearanceSubject -> IO HeraldState
runLiveWorkflow seed subject = do
  (oracle2, reportedNetwork, probe) <- prepareLiveResolveNetwork seed subject
  (coordinator, _) <- require "live Resolve coordinator" (Map.lookup local reportedNetwork)
  resolveEnvelope <- retainedEnvelope "one actual Resolve intention" coordinator
  (_, terminalNetwork, resolveEntry) <- commitNetwork "Resolve" oracle2 reportedNetwork resolveEnvelope
  assertBool
    "Resolve is emitted by the live Oracle state machine"
    (any isResolved (fmap oracleProjectionEventView (appliedEntryProjectionEvents resolveEntry)))
  forM_ (Map.toList terminalNetwork) $ \(_, (resolved, memberBinding)) -> do
    terminal <-
      require
        "resolved probe at each Herald"
        (Disappearance.probeWitness probe (startupDisappearanceState resolved))
    assertBool
      "the live leaf records the exact terminal outcome"
      (case terminal.witnessPhase of Disappearance.ProbeResolvedView {} -> True; _ -> False)
    assertEqual "terminal owner is whole-state valid" (Right ()) (validateHeraldState resolved)
    projected <-
      require
        "retained canonical resolution"
        (OracleProjection.oracleViewDisappearanceProbe probe (OracleProjection.oracleView (startupOracleProjectionState resolved)))
    assertBool
      "canonical history replay cannot replace a serving member's own disappearance protocol"
      (case DisappearanceReplay.replayProjectedDisappearanceResolution projected resolved of Left _ -> True; Right _ -> False)
    case Domain.disappearanceSubjectView subject of
      Domain.ControlledPredefinedSubjectView object _ _ ->
        assertBool
          "every captured Herald installs controlled terminal suppression"
          (Controlled.controlledObjectTerminallyDeleted object (startupControlledState resolved))
      Domain.RegularSortDefinitionSubjectView sortId _ occurrence -> do
        assertEqual
          "every captured Herald removes the effective regular occurrence"
          Nothing
          (SortRegistry.lookupEffectiveSort sortId (startupSortRegistryState resolved))
        assertBool
          "every captured Herald retains the exact regular retirement floor"
          (case SortRegistry.lookupRegularSortRetirement sortId occurrence (startupSortRegistryState resolved) of Just _ -> True; Nothing -> False)
    let client = startupOracleClientState resolved
        actions = OracleClient.oracleClientRequestActions client
    assertEqual
      "terminal cleanup releases every local disappearance request"
      []
      [dispatch | SubmitOracleRequest _ dispatch <- actions]
    assertEqual "the remaining action coalesces retirement on the current binding" [ScheduleOracleProgress memberBinding] actions
    assertBool
      "idle retirement covers newly settled receipts"
      (OracleClient.oracleClientReceiptRetirementReady client > OracleClient.oracleClientReceiptRetirementConfirmed client)
    (replayed, _) <-
      step
        "exact Resolve watch replay"
        (OracleInput (OracleEntriesReceived memberBinding (canonicalizeAppliedOracleEntry resolveEntry :| [])))
        resolved
    assertBool "exact live Resolve replay preserves the terminal owner state" (replayed == resolved)
  assertEqual
    "finite workflow commits Open, one report per member, and Resolve"
    (Identity.controlIndex (fromIntegral (length members + 2)))
    (appliedEntryControlIndex resolveEntry)
  fst <$> require "terminal local Herald" (Map.lookup local terminalNetwork)
  where
    genesis = startupGenesis seed
    local = Genesis.checkedLocalHeraldEpoch genesis
    members = Genesis.checkedActiveHeraldEpochs genesis
    isResolved DisappearanceProbeResolvedView {} = True
    isResolved _ = False

prepareLiveResolve :: HeraldState -> Domain.DisappearanceSubject -> IO (OracleState, HeraldState, OracleBinding)
prepareLiveResolve seed subject = do
  (oracle, network, _) <- prepareLiveResolveNetwork seed subject
  (state, binding) <- require "prepared live Resolve coordinator" (Map.lookup (Genesis.checkedLocalHeraldEpoch (startupGenesis seed)) network)
  pure (oracle, state, binding)

prepareLiveResolveNetwork :: HeraldState -> Domain.DisappearanceSubject -> IO (OracleState, Map.Map Identity.HeraldEpoch (HeraldState, OracleBinding), Domain.DisappearanceProbeId)
prepareLiveResolveNetwork seed subject = do
  settledNetwork <- prepareQuietNetwork seed
  (quietLocal, binding) <- require "settled live coordinator" (Map.lookup local settledNetwork)
  oracle0 <- checked "initialize actual Oracle for the settled network" (initialOracle (fixtureCheckedOracleGenesisFor (startupGenesis seed)))
  (oracle1, opened, openEntry) <- submitLocalRequest "Open" binding oracle0 quietLocal
  assertBool
    "Open is emitted by the live Oracle state machine"
    (any isOpened (fmap oracleProjectionEventView (appliedEntryProjectionEvents openEntry)))
  localWitness <- exactlyOne "one projected probe" (Disappearance.probeWitnesses (startupDisappearanceState opened))
  let probe = Protocol.projectedProbeId localWitness.witnessProjectedProbe
  assertEqual
    "the live probe keeps the accepted subject"
    subject
    (Protocol.projectedProbeSubject localWitness.witnessProjectedProbe)
  -- Each remote consumes the actual predecessor stream before Open. In the
  -- controlled case that includes the structural carrier at sequence one;
  -- completing only its later marker would not prove a contiguous cut.
  remoteStates <- forM remotes $ \remote -> do
    (connected, remoteBinding) <- require "settled captured remote" (Map.lookup remote settledNetwork)
    marker <-
      require
        "local marker addressed to remote"
        (lookup remote localWitness.witnessOutgoingPublicationMarkers)
    history <-
      checked
        "source retains its exact premarker work"
        (PeerStream.activeOutgoingItems (Protocol.disappearancePublicationMarkerDirection marker) (startupPeerStreamState opened))
    beforeOpen <-
      foldM
        (flip deliverItem)
        connected
        (filter ((< Protocol.disappearancePublicationMarkerSequence marker) . Stream.sequencedItemSequence) history)
    settled <-
      foldM
        (\state established -> deliverControlFrom local (PeerTopologyCutEstablished established) state)
        beforeOpen
        (GraphProgress.structuralInstalledEstablishedChain (startupStructuralProgressState seed))
    earlyMarker <-
      exactlyOne
        "source retains its exact assigned marker"
        (filter ((== Protocol.disappearancePublicationMarkerSequence marker) . Stream.sequencedItemSequence) history)
    early <- deliverItem earlyMarker settled
    assertMarkerWorkReference early
    (projectionOnly, _) <- checked "project actual Open before ordered marker release" (Live.applyDisappearanceOpen localWitness.witnessProjectedProbe early)
    assertMarkerWorkReference projectionOnly
    assertEqual
      "an early ordered marker cannot fabricate Open or absence evidence"
      []
      (Disappearance.probeWitnesses (startupDisappearanceState early))
    earlyWatermarks <-
      checked
        "early marker retained watermarks"
        (PeerStream.incomingWatermarks (Protocol.disappearancePublicationMarkerDirection marker) (startupPeerStreamState early))
    assertBool
      "an early ordered marker does not complete semantically"
      (Stream.streamCompletedPrefix earlyWatermarks < Stream.streamPrefixThrough (Protocol.disappearancePublicationMarkerSequence marker))
    (remoteOpened, _) <-
      step
        "remote applies actual Open watch"
        (OracleInput (OracleEntriesReceived remoteBinding (canonicalizeAppliedOracleEntry openEntry :| [])))
        early
    assertMarkerWorkReference remoteOpened
    openedWatermarks <-
      checked
        "Open redrives retained marker watermarks"
        (PeerStream.incomingWatermarks (Protocol.disappearancePublicationMarkerDirection marker) (startupPeerStreamState remoteOpened))
    assertEqual
      "Open alone releases the retained contiguous marker"
      (Stream.streamPrefixThrough (Protocol.disappearancePublicationMarkerSequence marker))
      (Stream.streamCompletedPrefix openedWatermarks)
    pure (remote, (remoteOpened, remoteBinding))
  let network0 = Map.insert local (opened, binding) (Map.fromList remoteStates)
  allMarkers <- fmap concat $ forM (Map.toList network0) $ \(source, (state, _)) -> do
    witness <-
      require
        "every live Herald projects the probe"
        (Disappearance.probeWitness probe (startupDisappearanceState state))
    pure [((source, destination), marker) | (destination, marker) <- witness.witnessOutgoingPublicationMarkers]
  assertEqual
    "each captured source assigns exactly one marker per other member"
    (length members * (length members - 1))
    (length allMarkers)
  network1 <-
    foldM
      ( \network ((source, destination), marker) -> do
          (sender, _) <- require "retained marker source" (Map.lookup source network)
          throughMarker <-
            checked
              "retained contiguous source prefix"
              (PeerStream.activeOutgoingItems (Protocol.disappearancePublicationMarkerDirection marker) (startupPeerStreamState sender))
          let prefix = filter ((<= Protocol.disappearancePublicationMarkerSequence marker) . Stream.sequencedItemSequence) throughMarker
          updateHerald destination (\receiver -> foldM (flip deliverItem) receiver prefix) network
      )
      network0
      allMarkers
  network2 <-
    foldM
      ( \network ((source, remote), marker) ->
          updateHerald
            source
            (acknowledgeMarker remote marker)
            network
      )
      network1
      allMarkers
  reportedOwners <- forM (Map.toList network2) $ \(member, (state, memberBinding)) -> do
    (advanced, _) <- checked "live owner composes its complete absence report" (Live.advanceDisappearanceWork state)
    snapshot <-
      checked
        "capture real eight-owner absence snapshot"
        (Evidence.disappearanceEvidenceSnapshotForHerald subject advanced)
    assertEqual
      "each actual Herald has no disappearance blockers"
      []
      (Evidence.disappearanceEvidenceSnapshotBlockers snapshot)
    witness <-
      require
        "live reporting owner retains its probe"
        (Disappearance.probeWitness probe (startupDisappearanceState advanced))
    assertBool
      "complete actual owner cuts produce a report"
      (case witness.witnessReport of Just _ -> True; Nothing -> False)
    assertQuiescent "retained Report intention" advanced
    pure (member, (advanced, memberBinding))
  let network3 = Map.fromList reportedOwners
  (oracle2, reportedNetwork) <-
    foldM
      ( \(oracle, network) reporter -> do
          (reporting, _) <- require "captured report sender" (Map.lookup reporter network)
          envelope <- retainedEnvelope "one actual Report intention" reporting
          (nextOracle, nextNetwork, _) <- commitNetwork "actual owner Report" oracle network envelope
          pure (nextOracle, nextNetwork)
      )
      (oracle1, network3)
      members
  pure (oracle2, reportedNetwork, probe)
  where
    genesis = startupGenesis seed
    local = Genesis.checkedLocalHeraldEpoch genesis
    members = Genesis.checkedActiveHeraldEpochs genesis
    remotes = filter (/= local) members
    isOpened DisappearanceProbeOpenedView {} = True
    isOpened _ = False

-- A finite lossless pre-Open schedule delivers only retained owner work. This
-- includes the placement vector and alignment generations needed to settle
-- the real structural carrier; no debt is erased to manufacture absence.
type LiveNetwork = Map.Map Identity.HeraldEpoch (HeraldState, OracleBinding)

data NetworkMessage
  = NetworkControl ReceiptRetirement PeerControl
  | NetworkPublication ReceiptRetirement (Stream.SequencedItem Payload.PeerLogicalPayload)

type NetworkEnvelope = (Identity.HeraldEpoch, Identity.HeraldEpoch, NetworkMessage)

prepareQuietNetwork :: HeraldState -> IO LiveNetwork
prepareQuietNetwork seed = do
  (bound, binding) <- bindOracle seed
  localState <- connectAllPeers bound
  remoteStates <- forM remotes $ \remote -> do
    initial <- initializeRemote seed remote
    (boundRemote, remoteBinding) <- bindOracle initial
    connected <- connectAllPeers boundRemote
    pure (remote, (connected, remoteBinding))
  let initialNetwork = Map.insert local (localState, binding) (Map.fromList remoteStates)
  seeded <- forM (Map.toList initialNetwork) $ \(source, (state, oracleBinding)) -> do
    prepared <-
      checked
        "retain exact local placement snapshot"
        (Placement.prepareRetainedLocalSnapshot (startupPlacementState state))
    let (placement, result) = Placement.commitLocalSnapshot prepared
        successor = replaceStartupPlacementState placement state
        bindings = DiscoveryState.currentPeerBindings (startupDiscoveryState successor)
        snapshotEffects = [SendPeerControl peer (PeerPlacementUpdate (FullPlacementSnapshot (Placement.localSnapshotMessage result))) | peer <- bindings]
    repairs <- fmap concat $ forM bindings $ \peer ->
      checked "reoffer exact retained peer control work" (PeerControl.reconnectRepairEffects peer successor)
    (normalized, effects) <- checked "normalize initial retained repair deliveries" (PeerDelivery.normalizeEffects successor (orderedEffectBatch (snapshotEffects <> repairs)))
    let seededState = foldl (flip PeerDelivery.markSeeded) normalized bindings
    pure (source, (seededState, oracleBinding), networkMessages source (effectBatchMembers effects))
  drainNetwork
    Set.empty
    (Map.fromList [(source, state) | (source, state, _) <- seeded])
    (concat [messages | (_, _, messages) <- seeded])
  where
    local = Genesis.checkedLocalHeraldEpoch (startupGenesis seed)
    remotes = filter (/= local) (Genesis.checkedActiveHeraldEpochs (startupGenesis seed))

networkMessages :: Identity.HeraldEpoch -> [HeraldEffect] -> [NetworkEnvelope]
networkMessages source = concatMap message
  where
    message effect = case effect of
      SendPeerControl binding control -> [(source, Discovery.peerBindingRemoteHeraldEpoch binding, NetworkControl mempty control)]
      SendPeerControlWithProgress binding progress control -> [(source, Discovery.peerBindingRemoteHeraldEpoch binding, NetworkControl progress control)]
      SendPeerItem binding attempt -> [(source, Discovery.peerBindingRemoteHeraldEpoch binding, NetworkPublication mempty (Stream.peerDispatchAttemptItem attempt))]
      SendPeerItemWithProgress binding progress attempt -> [(source, Discovery.peerBindingRemoteHeraldEpoch binding, NetworkPublication progress (Stream.peerDispatchAttemptItem attempt))]
      _ -> []

networkKey :: NetworkEnvelope -> IO (Identity.HeraldEpoch, Identity.HeraldEpoch, ByteString)
networkKey (source, destination, message) = do
  envelope <- projectNetworkMessage message
  pure (source, destination, encodePeerEnvelope envelope)

projectNetworkMessage :: NetworkMessage -> IO PeerEnvelope
projectNetworkMessage message = case message of
  NetworkControl progress control -> checked "encode retained peer control" (projectPeerControlWithProgress progress control)
  NetworkPublication progress item ->
    checked
      "encode retained ordered peer work"
      ( projectPeerLogicalAttemptWithProgress
          progress
          ( Stream.peerDispatchAttempt
              (must (Stream.mkPeerDispatchBindingGeneration 1))
              (Stream.peerDispatchAttemptGenerationForOwner 1)
              item
          )
      )

drainNetwork :: Set.Set (Identity.HeraldEpoch, Identity.HeraldEpoch, ByteString) -> LiveNetwork -> [NetworkEnvelope] -> IO LiveNetwork
drainNetwork seen network [] = do
  collected <- forM (Map.toList network) $ \(source, (state, oracleBinding)) -> do
    directions <- checked "inspect current outgoing directions" (PeerStream.peerStreamOutgoingDirections (startupPeerStreamState state))
    work <- fmap concat $ forM directions $ \direction -> do
      items <- checked "inspect retained ordered network work" (PeerStream.activeOutgoingItems direction (startupPeerStreamState state))
      case items of
        [] -> pure []
        _ -> do
          binding <- require "current outgoing repair binding" (DiscoveryState.currentPeerBinding (Stream.streamDirectionDestination direction) (startupDiscoveryState state))
          pure [SendPeerItem binding (Stream.peerDispatchAttempt (must (Stream.mkPeerDispatchBindingGeneration 1)) (Stream.peerDispatchAttemptGenerationForOwner 1) item) | item <- items]
    (normalized, effects) <- checked "attach current receipts to retained stream repairs" (PeerDelivery.normalizeEffects state (orderedEffectBatch work))
    pure ((source, (normalized, oracleBinding)), networkMessages source (effectBatchMembers effects))
  let normalizedNetwork = Map.fromList (map fst collected)
      pending = concatMap snd collected
  fresh <- filterM (fmap (`Set.notMember` seen) . networkKey) pending
  if null fresh then pure normalizedNetwork else drainNetwork seen normalizedNetwork fresh
drainNetwork seen network (message@(source, destination, body) : rest) = do
  key <- networkKey message
  if Set.member key seen
    then drainNetwork seen network rest
    else do
      (receiver, oracleBinding) <- require "network message destination" (Map.lookup destination network)
      binding <-
        require
          "authenticated network source"
          (DiscoveryState.currentPeerBinding source (startupDiscoveryState receiver))
      envelope <- projectNetworkMessage body
      admitted <- checked "admit actual retained peer envelope" (admitEstablishedPeerEnvelope envelope)
      (successor, effects) <- case admitted of
        EstablishedPeerControl progress control ->
          step "deliver actual peer control" (PeerInput (PeerControlReceivedWithProgress binding progress control)) receiver
        EstablishedPeerPublication progress item ->
          step "deliver actual retained peer publication" (PeerInput (PeerPublicationReceivedWithProgress binding progress item)) receiver
      assertEqual
        "valid retained network work preserves peer bindings"
        []
        [effect | effect <- effectBatchMembers effects, case effect of RejectPeerConnection {} -> True; ClosePeerBinding {} -> True; _ -> False]
      drainNetwork
        (Set.insert key seen)
        (Map.insert destination (successor, oracleBinding) network)
        (rest <> networkMessages destination (effectBatchMembers effects))

initializeRemote :: HeraldState -> Identity.HeraldEpoch -> IO HeraldState
initializeRemote source remote = do
  member <-
    exactlyOne
      "remote genesis member"
      [m | m <- Genesis.checkedActiveHeralds (startupGenesis source), Genesis.heraldMemberEpoch m == remote]
  let initial = fixtureDeploymentAt member
      members = Genesis.checkedActiveHeralds (startupGenesis source)
      manifest =
        initial
          { Genesis.deploymentActiveHeralds = members,
            Genesis.deploymentOracleGenesis =
              (Genesis.deploymentOracleGenesis initial)
                { Genesis.oracleGenesisActiveHeralds = members
                }
          }
  genesis <- checked "check exact remote genesis" (Genesis.checkHeraldGenesis manifest)
  fst
    <$> checked
      "initialize real remote Herald"
      ( initialHerald
          (monotonicInstant 0)
          genesis
          (fixtureCheckedInitialBootstrapsFor genesis)
          fixtureOracleContacts
          fixtureGeneratorSeed
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
      )

updateHerald :: Identity.HeraldEpoch -> (HeraldState -> IO HeraldState) -> Map.Map Identity.HeraldEpoch (HeraldState, OracleBinding) -> IO (Map.Map Identity.HeraldEpoch (HeraldState, OracleBinding))
updateHerald member action network = do
  (state, binding) <- require "captured network Herald" (Map.lookup member network)
  successor <- action state
  pure (Map.insert member (successor, binding) network)

retainedEnvelope :: String -> HeraldState -> IO OracleEnvelope
retainedEnvelope label state = do
  dispatch <-
    exactlyOne
      label
      [request | SubmitOracleRequest _ request <- OracleClient.oracleClientRequestActions (startupOracleClientState state)]
  pure (canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch))

commitNetwork :: String -> OracleState -> LiveNetwork -> OracleEnvelope -> IO (OracleState, LiveNetwork, AppliedOracleEntry)
commitNetwork label oracle network envelope = do
  (nextOracle, successor, entry, _) <- commitNetworkWithEffects label oracle network envelope
  pure (nextOracle, successor, entry)

commitNetworkWithEffects :: String -> OracleState -> LiveNetwork -> OracleEnvelope -> IO (OracleState, LiveNetwork, AppliedOracleEntry, [NetworkEnvelope])
commitNetworkWithEffects label oracle network envelope = do
  (nextOracle, outcome, effects) <- checked (label <> " live Oracle transition") (stepOracle envelope oracle)
  entry <- case outcome of
    OracleCommitted _ -> exactlyOne (label <> " committed entry") [entry | EmitAppliedOracleEntry entry <- oracleEffects effects]
    other -> assertFailure (label <> " did not commit: " <> show other)
  canonical <-
    checked
      (label <> " canonical applied-entry decoder")
      (decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry entry)))
  projected <-
    Map.traverseWithKey
      ( \member (state, binding) -> do
          (advanced, watchEffects) <-
            step
              (label <> " contiguous live Oracle watch at " <> show (appliedEntryControlIndex entry) <> " on " <> show (Genesis.checkedLocalHeraldEpoch (startupGenesis state)))
              (OracleInput (OracleEntriesReceived binding (canonical :| [])))
              state
          pure ((advanced, binding), networkMessages member (effectBatchMembers watchEffects))
      )
      network
  pure (nextOracle, fmap fst projected, entry, concatMap snd (Map.elems projected))

deliverItem :: Stream.SequencedItem Payload.PeerLogicalPayload -> HeraldState -> IO HeraldState
deliverItem item state = do
  binding <-
    require
      "current receiving peer binding"
      (DiscoveryState.currentPeerBinding (Stream.streamDirectionSource (Stream.sequencedItemDirection item)) (startupDiscoveryState state))
  let attempt =
        Stream.peerDispatchAttempt
          (must (Stream.mkPeerDispatchBindingGeneration 1))
          (Stream.peerDispatchAttemptGenerationForOwner 1)
          item
  envelope <- checked "project current peer envelope" (projectPeerLogicalAttempt attempt)
  decoded <- checked "decode current peer envelope" (decodePeerEnvelope (encodePeerEnvelope envelope))
  incoming <- checked "bridge current peer envelope" (admitEstablishedPeerEnvelope decoded)
  case incoming of
    EstablishedPeerPublication progress admitted ->
      fst
        <$> step
          "receive exact live peer work"
          (PeerInput (PeerPublicationReceivedWithProgress binding progress admitted))
          state
    _ -> assertFailure "ordered stream item did not project into the publication stream"

deliverControlFrom :: Identity.HeraldEpoch -> PeerControl -> HeraldState -> IO HeraldState
deliverControlFrom source control state = do
  binding <-
    require
      "current source control binding"
      (DiscoveryState.currentPeerBinding source (startupDiscoveryState state))
  fst
    <$> step
      "receive source topology settlement"
      (PeerInput (PeerControlReceived binding control))
      state

acknowledgeMarker :: Identity.HeraldEpoch -> Protocol.DisappearancePublicationMarker -> HeraldState -> IO HeraldState
acknowledgeMarker remote marker state = do
  binding <-
    require
      "current outgoing peer binding"
      (DiscoveryState.currentPeerBinding remote (startupDiscoveryState state))
  fst
    <$> step
      "remote acknowledges completed exact marker assignment"
      ( PeerInput
          ( PeerControlReceived
              binding
              ( PeerStreamCompleted
                  (Protocol.disappearancePublicationMarkerDirection marker)
                  (Stream.streamPrefixCompletion (Stream.streamPrefixThrough (Protocol.disappearancePublicationMarkerSequence marker)))
              )
          )
      )
      state

prepareLiveOpen :: HeraldState -> IO (OracleState, HeraldState, OracleBinding, AppliedOracleEntry)
prepareLiveOpen seed = do
  assertEqual "reachable seed is whole-state valid" (Right ()) (validateHeraldState seed)
  (bound, binding) <- bindOracle seed
  connected <- connectAllPeers bound
  (ready, _) <- checked "advance the accepted LocalTake candidate" (Live.advanceDisappearanceWork connected)
  assertQuiescent "retained Open intention" ready
  oracle0 <- checked "initialize actual Oracle state" (initialOracle (fixtureCheckedOracleGenesisFor (startupGenesis seed)))
  (oracle1, opened, openEntry) <- submitLocalRequest "Open" binding oracle0 ready
  pure (oracle1, opened, binding, openEntry)

connectAllPeers :: HeraldState -> IO HeraldState
connectAllPeers seed = foldM connectRemote seed remotes
  where
    genesis = startupGenesis seed
    local = Genesis.checkedLocalHeraldEpoch genesis
    remotes = filter (/= local) (Genesis.checkedActiveHeraldEpochs genesis)
    connectRemote state remote = do
      member <-
        exactlyOne
          "remote genesis member"
          [m | m <- Genesis.checkedActiveHeralds genesis, Genesis.heraldMemberEpoch m == remote]
      let nonce = Discovery.connectionNonce 600
          candidate = Discovery.peerCandidate (Genesis.heraldMemberId member) remote nonce
          hello =
            Discovery.peerHello
              (Genesis.checkedSystemId genesis)
              (Genesis.heraldMemberId member)
              remote
              nonce
              Set.empty
              (Identity.controlIndex 0)
              (Genesis.checkedCatalogueDigest genesis)
              (Genesis.checkedInitialProjectionDigest (fixtureCheckedInitialBootstrapsFor genesis))
              Nothing
      fst
        <$> step
          "admit real remote peer binding"
          (PeerInput (currentPeerHelloReceived state candidate Set.empty hello))
          state

bindOracle :: HeraldState -> IO (HeraldState, OracleBinding)
bindOracle state = case OracleClient.oracleClientCurrentBinding (startupOracleClientState state) of
  Just binding -> pure (state, binding)
  Nothing -> do
    attempt <-
      exactlyOne
        "initial live Oracle connection"
        [candidate | ConnectAndHelloOracle candidate _ <- OracleClient.oracleClientActions (startupOracleClientState state)]
    let node = oracleContactNode (oracleConnectAttemptContact attempt)
        acceptance = oracleHelloAcceptance node (oracleObservedTerm 1) (Identity.controlIndex 0) (Just node) True
    (successor, _) <- step "bind actual Oracle client" (OracleInput (OracleHelloReceived attempt acceptance)) state
    binding <- require "accepted Oracle binding" (OracleClient.oracleClientCurrentBinding (startupOracleClientState successor))
    pure (successor, binding)

submitLocalRequest :: String -> OracleBinding -> OracleState -> HeraldState -> IO (OracleState, HeraldState, AppliedOracleEntry)
submitLocalRequest label binding oracle state = do
  (advanced, _) <- checked (label <> " intention progression") (Live.advanceDisappearanceWork state)
  dispatch <-
    exactlyOne
      (label <> " stable live dispatch")
      [request | SubmitOracleRequest _ request <- OracleClient.oracleClientRequestActions (startupOracleClientState advanced)]
  submitEnvelope
    label
    binding
    oracle
    advanced
    (canonicalOracleEnvelopeValue (oracleRequestDispatchEnvelope dispatch))

submitEnvelope :: String -> OracleBinding -> OracleState -> HeraldState -> OracleEnvelope -> IO (OracleState, HeraldState, AppliedOracleEntry)
submitEnvelope label binding oracle state envelope = do
  (nextOracle, outcome, effects) <- checked (label <> " live Oracle transition") (stepOracle envelope oracle)
  entry <- case outcome of
    OracleCommitted _ -> exactlyOne (label <> " committed entry") [entry | EmitAppliedOracleEntry entry <- oracleEffects effects]
    other -> assertFailure (label <> " did not commit: " <> show other)
  canonical <-
    checked
      (label <> " canonical applied-entry decoder")
      (decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry entry)))
  (successor, _) <-
    step
      (label <> " contiguous live Oracle watch at " <> show (appliedEntryControlIndex entry) <> " on " <> show (Genesis.checkedLocalHeraldEpoch (startupGenesis state)))
      (OracleInput (OracleEntriesReceived binding (canonical :| [])))
      state
  pure (nextOracle, successor, entry)

step :: String -> HeraldInputBody -> HeraldState -> IO (HeraldState, EffectBatch)
step label input state = checked label (verifiedStepHerald (heraldInput (monotonicInstant 100) input) state)

exactlyOne :: String -> [value] -> IO value
exactlyOne _ [value] = pure value
exactlyOne label values = assertFailure (label <> ": expected one, observed " <> show (length values))

require :: String -> Maybe value -> IO value
require label = maybe (assertFailure (label <> ": missing")) pure

checked :: (Show problem) => String -> Either problem value -> IO value
checked label = either (assertFailure . ((label <> ": ") <>) . show) pure

must :: (Show problem) => Either problem value -> value
must = either (error . show) id

assertQuiescent :: String -> HeraldState -> Assertion
assertQuiescent label state = do
  (successor, effects) <- checked (label <> " unchanged scheduler step") (Live.advanceDisappearanceWork state)
  assertBool (label <> " keeps stable logical work") (successor == state)
  assertBool (label <> " emits no physical retry without a new fact") (effectBatchIsEmpty effects)
