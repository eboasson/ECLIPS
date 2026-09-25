{-# LANGUAGE OverloadedRecordDot #-}

module DisappearanceAlignmentRaceProperties (tests) where

import Control.Monad (forM_)
import DisappearanceLiveProperties (prepareLiveOpen)
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.EffectBatch (effectBatchIsEmpty)
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.DisappearanceLive qualified as Live
import Step16RegularRetirementAcceptanceProperties (liveRegularAlignmentGateFixture, liveRegularDisappearanceFixture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

data Mutation = Created | Replaced | Cancelled | SourceLost
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "Step16 alignment mutation corpus"
    [ testCase "seed-160110: Open; subscription-created; repeat" (caseMutation Created),
      testCase "seed-160111: Open; subscription-replaced; repeat" (caseMutation Replaced),
      testCase "seed-160112: Open; subscription-cancelled; repeat" (caseMutation Cancelled),
      testCase "seed-160113: Open; subscription-source-lost; repeat" (caseMutation SourceLost)
    ]

-- Reuse a checked destination owner with an empty completed base. Mutations
-- prepare through that owner, then the live coordinator observes its subject
-- view. The canonical Open comes from the actual Oracle workflow; no leaf
-- mutation or manufactured absence evidence substitutes for owner observations.
caseMutation :: Mutation -> Assertion
caseMutation mutation = do
  let (seed, _) = liveRegularDisappearanceFixture
      (destination, _, _, subscription, _) = liveRegularAlignmentGateFixture False
      initialTransfer = transfer destination
      before = if mutation == Created then install Transfer.emptyState destination else destination
  (_, canonical, _, _) <- prepareLiveOpen seed
  projected <- case Disappearance.probeWitnesses (startupDisappearanceState canonical) of
    [witness] -> pure witness.witnessProjectedProbe
    _ -> assertFailure "expected one live canonical Open"
  let probe = Protocol.projectedProbeId projected
  (opened, _) <- checked (Live.applyDisappearanceOpen projected before)
  frozen <- witnessFor probe opened
  changedTransfer <- case mutation of
    Created -> pure initialTransfer
    _ -> do
      let reason = if mutation == SourceLost then AlignmentProtocol.AlignmentSourceIncarnationLost else AlignmentProtocol.AlignmentRelationRemoved
      prepared <- checked (Transfer.prepareAlignmentCancellation (AlignmentProtocol.alignmentCancel subscription reason) (transfer opened))
      let (cancelled, _) = Transfer.commitAlignmentCancellation prepared
      if mutation /= Replaced
        then pure cancelled
        else do
          attempt <- case Transfer.destinationSubscriptionEntries initialTransfer of
            [(_, entry)] -> maybe (assertFailure "missing checked destination attempt") pure (Transfer.destinationSubscriptionAttempt entry)
            _ -> assertFailure "expected one destination owner"
          let successor = AlignmentProtocol.alignmentSubscriptionId (AlignmentProtocol.alignmentSubscriptionIdDestinationHerald subscription) (AlignmentProtocol.nextAlignmentSubscriptionSequence (AlignmentProtocol.alignmentSubscriptionIdSequence subscription))
          replacement <- checked (AlignmentProtocol.alignmentAttempt (AlignmentProtocol.alignmentAttemptObligation attempt) successor (AlignmentProtocol.alignmentAttemptSourceHerald attempt) (AlignmentProtocol.alignmentAttemptHistoricalCertificate attempt))
          next <- checked (Transfer.prepareDestinationAttempt replacement cancelled)
          pure (fst (Transfer.commitDestinationAttempt next))
  assertEqual "each mutation preserves the transfer owner invariant" (Right ()) (Transfer.validateAlignmentTransferState changedTransfer)
  (observed, _) <- checked (Live.advanceDisappearanceWork (install changedTransfer opened))
  witness <- witnessFor probe observed
  invalidation <- maybe (assertFailure "changed actual subscription set must invalidate the collecting cut") pure witness.witnessInvalidationIntention
  assertEqual "one owner contradiction names the exact probe" probe (Protocol.disappearanceInvalidationProbe invalidation)
  assertEqual "alignment changes invalidate local evidence" Protocol.LocalEvidenceContradicted (Protocol.disappearanceInvalidationReason invalidation)
  assertEqual "the original captured subscriptions and marker cuts remain immutable" frozen.witnessIncomingAlignmentCuts witness.witnessIncomingAlignmentCuts
  assertEqual "mutation cannot create an absence report" Nothing witness.witnessReportIntention
  assertEqual "mutation cannot create a Resolve" Nothing witness.witnessResolveIntention
  let oldWork = Disappearance.disappearanceWorkLedger (startupDisappearanceState opened)
      newWork = Disappearance.disappearanceWorkLedger (startupDisappearanceState observed)
  -- Replacement removes one captured incoming identity and adds its successor;
  -- the empty replacement transcript adds no subject-relevant payload blocker.
  assertEqual
    "changed subscription evidence touches the exact owner facts"
    (if mutation == Replaced then 2 else 1)
    (newWork.blockerObservations - oldWork.blockerObservations)
  assertEqual "one stable invalidation per mutation" 1 (newWork.invalidationIntentionCreations - oldWork.invalidationIntentionCreations)
  assertEqual "mutation does not allocate another probe or marker" (oldWork.projectedOpenInstallations, oldWork.publicationMarkerAssignments, oldWork.alignmentMarkerAssignments) (newWork.projectedOpenInstallations, newWork.publicationMarkerAssignments, newWork.alignmentMarkerAssignments)
  forM_ [1 :: Int .. 3] $ \_ -> do
    (replayed, effects) <- checked (Live.advanceDisappearanceWork observed)
    assertBool "equal owner observations preserve state and the exact work ledger" (replayed == observed)
    assertBool "equal owner observations emit no work" (effectBatchIsEmpty effects)

transfer :: HeraldState -> Transfer.State
transfer = Alignment.alignmentTransferState . startupAlignmentState

install :: Transfer.State -> HeraldState -> HeraldState
install owner state = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState owner (startupAlignmentState state)) state

witnessFor :: DisappearanceProbeId -> HeraldState -> IO Disappearance.ProbeWitness
witnessFor probe state = maybe (assertFailure "missing live probe") pure (Disappearance.probeWitness probe (startupDisappearanceState state))

checked :: (Show problem) => Either problem value -> IO value
checked = either (assertFailure . show) pure
