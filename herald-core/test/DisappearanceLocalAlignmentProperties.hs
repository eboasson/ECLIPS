{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DisappearanceLocalAlignmentProperties (tests) where

import Control.Monad (foldM)
import DisappearanceLiveProperties (prepareLiveOpen)
import Eclips.Domain.Alignment (initialStoreRevision)
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Disappearance.Protocol qualified as DisappearanceProtocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery (peerBindingRemoteHeraldEpoch)
import Eclips.Herald.EffectBatch (HeraldEffect (SendPeerControl), effectBatchIsEmpty, effectBatchMembers)
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.DisappearanceLive qualified as Live
import Step16RegularRetirementAcceptanceProperties (liveRegularAlignmentGateFixture, liveRegularDisappearanceFixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Step16 local alignment marker corpus"
    [ testCase "seed-160120: local-base; Open; local-marker; repeat" (localMarker True),
      testCase "seed-160121: Open; local-marker; local-base; repeat" (localMarker False)
    ]

-- Both transcript endpoints are checked Transfer owners on the same Herald.
-- The empty base is actually consumed and acknowledged; it is not replaced by
-- an invented watermark or disappearance report. Remote publication markers
-- deliberately remain outstanding throughout this focused owner regression.
localMarker :: Bool -> Assertion
localMarker baseBeforeOpen = do
  let (seed, _) = liveRegularDisappearanceFixture
      (prototype, _, _, subscription, _) = liveRegularAlignmentGateFixture False
      local = checkedLocalHeraldEpoch (startupGenesis seed)
  original <- one "checked destination prototype" (Transfer.destinationSubscriptionEntries (transfer prototype))
  attempt <- maybe (assertFailure "prototype has no destination attempt") pure (Transfer.destinationSubscriptionAttempt (snd original))
  localAttempt <- checked (Protocol.alignmentAttempt (Protocol.alignmentAttemptObligation attempt) subscription local (Protocol.alignmentAttemptHistoricalCertificate attempt))
  destination <- checked (Transfer.prepareDestinationAttempt localAttempt Transfer.emptyState)
  let destinationOwner = fst (Transfer.commitDestinationAttempt destination)
  source <- checked (Transfer.prepareSourceSubscription (Transfer.preparedDestinationAttemptSubscribe destination) initialStoreRevision [] Transfer.SourceLiveImmediate destinationOwner)
  let sourceOwner = fst (Transfer.commitSourceSubscription source)
      controls = Transfer.preparedSourceSubscriptionControls source
  beforeOwner <- if baseBeforeOpen then foldM consume sourceOwner controls else pure sourceOwner
  assertEqual "both local transcript owners are valid" (Right ()) (Transfer.validateAlignmentTransferState beforeOwner)
  (_, canonical, _, _) <- prepareLiveOpen seed
  canonicalWitness <- one "actual canonical Open" (Disappearance.probeWitnesses (startupDisappearanceState canonical))
  let projected = canonicalWitness.witnessProjectedProbe
      probe = DisappearanceProtocol.projectedProbeId projected
  (opened, effects) <- checked (Live.applyDisappearanceOpen projected (install beforeOwner seed))
  witness <- maybe (assertFailure "missing local-alignment probe") pure (Disappearance.probeWitness probe (startupDisappearanceState opened))
  (_, cell) <- one "exactly one captured local subscription" witness.witnessIncomingAlignmentCuts
  assertEqual
    "the captured alignment source is this owner"
    local
    (DisappearanceProtocol.incomingAlignmentCutSource cell.alignmentCellCapture)
  assertBool "the local marker is received without a peer connection" (case cell.alignmentCellMarker of Just _ -> True; Nothing -> False)
  assertEqual
    "completion uses the actual destination applied-through watermark"
    (if baseBeforeOpen then Just initialStoreRevision else Nothing)
    cell.alignmentCellCompletedThrough
  assertEqual
    "no control dispatch targets a fabricated self peer"
    []
    [binding | SendPeerControl binding _ <- effectBatchMembers effects, peerBindingRemoteHeraldEpoch binding == local]
  afterOwner <- if baseBeforeOpen then pure (transfer opened) else foldM consume (transfer opened) controls
  (completed, _) <- checked (Live.advanceDisappearanceWork (install afterOwner opened))
  completedWitness <- maybe (assertFailure "missing completed local-alignment probe") pure (Disappearance.probeWitness probe (startupDisappearanceState completed))
  (_, completedCell) <- one "one retained local subscription" completedWitness.witnessIncomingAlignmentCuts
  assertEqual "the same frozen local marker completes after real base consumption" (Just initialStoreRevision) completedCell.alignmentCellCompletedThrough
  assertEqual "an alignment cut cannot manufacture the missing remote publication reports" Nothing completedWitness.witnessReportIntention
  let ledger = Disappearance.disappearanceWorkLedger (startupDisappearanceState completed)
  assertEqual
    "A=1 charges exactly assignment, receipt, and completion (3*A)"
    (1, 1, 1)
    (ledger.alignmentMarkerAssignments, ledger.alignmentMarkerReceipts, ledger.alignmentMarkerCompletions)
  (repeated, repeatEffects) <- checked (Live.advanceDisappearanceWork completed)
  assertBool "equal owner observations preserve state and all work counts" (repeated == completed)
  assertBool "equal owner observations emit no effects" (effectBatchIsEmpty repeatEffects)

consume :: Transfer.State -> Protocol.AlignmentControl -> IO Transfer.State
consume state control = do
  prepared <-
    checked =<< case control of
      Protocol.AlignmentSnapshotStarted start -> pure (Transfer.prepareDestinationSnapshotStart start state)
      Protocol.AlignmentSnapshotChunkTransferred chunk -> pure (Transfer.prepareDestinationSnapshotChunk chunk state)
      Protocol.AlignmentSnapshotEnded end -> pure (Transfer.prepareDestinationSnapshotEnd end state)
      Protocol.AlignmentLiveAdvertised live -> pure (Transfer.prepareDestinationLive live state)
      _ -> assertFailure "unexpected empty-base control"
  let successor = fst (Transfer.commitDestinationWork prepared)
      work = Transfer.preparedDestinationWork prepared
  assertEqual
    "the checked source base carries no fabricated publication facts"
    []
    (maybe [] snd (Transfer.destinationWorkSnapshot work))
  case Transfer.destinationWorkAcknowledgement work of
    Nothing -> pure successor
    Just acknowledgement -> fst . Transfer.commitSourceAcknowledgement <$> checked (Transfer.prepareSourceAcknowledgement acknowledgement successor)

transfer :: HeraldState -> Transfer.State
transfer = Alignment.alignmentTransferState . startupAlignmentState

install :: Transfer.State -> HeraldState -> HeraldState
install owner state = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState owner (startupAlignmentState state)) state

checked :: (Show problem) => Either problem value -> IO value
checked = either (assertFailure . show) pure

one :: String -> [value] -> IO value
one _ [value] = pure value
one label values = assertFailure (label <> ": expected one, got " <> show (length values))
