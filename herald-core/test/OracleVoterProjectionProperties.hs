{-# LANGUAGE OverloadedStrings #-}

-- | Canonical Oracle watch entries are the sole source of changing voter
-- authority. These histories use the real sealed Raft configuration boundary.
module OracleVoterProjectionProperties (tests) where

import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust, isNothing)
import Data.Word (Word64)
import Eclips.Domain.Identity (controlIndex, controlIndexWord64)
import Eclips.Domain.Membership qualified as Membership
import Eclips.Herald.OracleProjection.State qualified as H
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command
import Eclips.Oracle.Effect
import Eclips.Oracle.Failure
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Eclips.Oracle.Voter
import Eclips.Raft.Effect qualified as RE
import Eclips.Raft.Genesis qualified as RG
import Eclips.Raft.Identity qualified as RI
import Eclips.Raft.Input qualified as R
import Eclips.Raft.Transition qualified as RT
import GenesisFixtures qualified as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

firstFixture :: [value] -> value
firstFixture (value : _) = value
firstFixture [] = error "expected nonempty voter property fixture"

lastFixture :: [value] -> value
lastFixture [] = error "expected nonempty voter property fixture"
lastFixture [value] = value
lastFixture (_ : remaining) = lastFixture remaining

tests :: TestTree
tests =
  testGroup
    "Oracle voter watch projection"
    [ testCase "accepted failure terminal survives native exclusion, retirement and later exact reobservation" automaticFailureProjection,
      testCase "captured failure configurations cannot be mixed across valid native histories" failureConfigurationMixing,
      QC.testProperty "automatic failure prefixes remain immutable under duplicate delivery" automaticDuplicatePrefixes,
      testCase "genesis bootstrap seeds checked roles without advancing the watch" bootstrap,
      testCase "registration enrichment and retained reobservation preserve their original coordinate" registrations,
      testCase "joint and final observations advance one cursor without inventing command receipts" configurationOrigins,
      testCase "demotion changes voter authority without retiring a Herald or its processes" demotion,
      testCase "cancelled intention remains retained across a fresh request retry" cancellation,
      testCase "gaps and different native terms cannot replace retained configuration evidence" ordering,
      QC.testProperty "every contiguous prefix agrees with Oracle under duplicate delivery" duplicatePrefixes
    ]

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

initialOracleState :: OracleState
initialOracleState = checked (initialOracle F.fixtureCheckedOracleGenesis)

initialProjection :: H.State
initialProjection = checked (H.initializeOracleVoters (oracleVoterConfiguration initialOracleState) (oracleReplicaRegistrations initialOracleState) F.fixtureOracleProjectionState)

bindings :: [RaftVoterBinding]
bindings = checkedOracleRaftVoterBindings F.fixtureCheckedOracleGenesis

contact :: OracleReplicaContact
contact = oracleReplicaContact endpoint endpoint endpoint
  where
    endpoint = checked (oracleReplicaEndpoint "127.0.0.1" 14001)

request :: Word64 -> OracleCommand -> OracleEnvelope
request ordinal = oracleEnvelope (oracleClientRequestId host ordinal) Nothing host
  where
    host = raftVoterBindingHeraldEpoch (firstFixture bindings)

register :: Word64 -> OracleEnvelope
register ordinal = request ordinal (registerOracleReplicaCommand (raftVoterBindingNode (firstFixture bindings)) (raftVoterBindingHeraldEpoch (firstFixture bindings)) contact)

run :: OracleEnvelope -> OracleState -> (OracleState, AppliedOracleEntry)
run envelope state = case checked (stepOracle envelope state) of
  (successor, OracleCommitted receipt, batch)
    | oracleReceiptResult receipt == OracleAccepted,
      [entry] <- [entry | EmitAppliedOracleEntry entry <- oracleEffects batch] ->
        (successor, entry)
  other -> error ("expected one accepted command observation: " <> show other)

application :: OracleEnvelope -> R.RaftEntry ByteString
application = R.Application . canonicalOracleEnvelopeBytes . canonicalizeOracleEnvelope

-- Only the native transition can produce the sealed committed entry.
sealedLast :: Word64 -> [R.RaftEntry ByteString] -> RE.RaftCommittedEntry ByteString
sealedLast term payloads =
  let native = checkedOracleRaftNativeConfiguration F.fixtureCheckedOracleGenesis
      nodes = map raftVoterBindingNode bindings
      leader = firstFixture nodes
      genesis = checked (RG.checkRaftGenesis (RG.raftGenesis (nodes !! 1) nodes (RG.raftNativeHeartbeatInterval native) (RG.raftNativeElectionTimeoutLower native) (RG.raftNativeElectionTimeoutUpper native)))
      entries = zipWith (\index payload -> R.raftLogEntry (RI.raftLogIndex index) (RI.raftTerm term) payload) [1 ..] payloads
      rpc = checked (R.appendEntries (RI.raftTerm term) leader (RI.raftLogIndex 0) (RI.raftTerm 0) entries (RI.raftLogIndex (fromIntegral (length entries))))
      (_, batch) = checked (RT.stepRaft (checked (R.observeRaftRequest leader rpc)) (RT.initialRaft genesis))
   in lastFixture [entry | RE.ExposeCommittedEntries committed <- RE.raftEffectBatchEffects batch, entry <- NE.toList committed]

transcript :: Word64 -> [(OracleState, AppliedOracleEntry)]
transcript = transcriptFor (take 2 bindings)

transcriptFor :: [RaftVoterBinding] -> Word64 -> [(OracleState, AppliedOracleEntry)]
transcriptFor desired term =
  let first@(registered, _) = run (register 1) initialOracleState
      second@(reobserved, _) = run (register 2) registered
      begin = request 3 (beginVoterChangeCommand (voterConfigurationId (oracleVoterConfiguration reobserved)) ExplicitDemotion (checked (oracleVoterBindings desired)))
      third@(preparing, _) = run begin reobserved
      ident = voterChangeId (fromJust (oraclePendingVoterChange preparing))
      (joint, jointMetadata) = checked (prepareOracleConfiguration ident JointConfigurationStage preparing)
      prefix = map application [register 1, register 2, begin] <> [R.Configuration joint jointMetadata]
      fourth@(jointState, _) = checked (applyCommittedOracleConfiguration (sealedLast term prefix) preparing)
      (final, finalMetadata) = checked (prepareOracleConfiguration ident FinalConfigurationStage jointState)
      fifth = checked (applyCommittedOracleConfiguration (sealedLast term (prefix <> [R.Configuration final finalMetadata])) jointState)
   in [first, second, third, fourth, fifth]

apply :: H.State -> AppliedOracleEntry -> H.State
apply state entry = H.commitAppliedEntry (checked (H.prepareAppliedEntry (canonicalizeAppliedOracleEntry entry) state))

projectedPrefixes :: [(OracleState, AppliedOracleEntry)] -> [H.State]
projectedPrefixes = drop 1 . scanl (\state (_, entry) -> apply state entry) initialProjection

bootstrap :: Assertion
bootstrap = do
  let view = H.oracleView initialProjection
  H.oracleViewControlIndex view @?= controlIndex 0
  H.oracleViewVoterConfiguration view @?= Just (oracleVoterConfiguration initialOracleState)
  H.oracleViewOracleReplicas view @?= oracleReplicaRegistrations initialOracleState
  H.oracleViewPendingVoterChange view @?= Nothing
  void (H.validateState initialProjection) @?= Right ()
  let after = firstFixture (projectedPrefixes (transcript 1))
  case H.initializeOracleVoters (oracleVoterConfiguration initialOracleState) (oracleReplicaRegistrations initialOracleState) after of
    Left (H.AppliedEntryVoterProjectionMismatch index) -> index @?= controlIndex 1
    _ -> assertFailure "bootstrap reset was admitted after a control entry"

registrations :: Assertion
registrations = do
  let prefixes = projectedPrefixes (transcript 1)
      first = firstFixture prefixes
      second = firstFixture (drop 1 prefixes)
      registered = H.oracleViewOracleReplicas (H.oracleView first)
  registered @?= H.oracleViewOracleReplicas (H.oracleView second)
  map replicaRegistrationControlIndex registered @?= [controlIndex 1, controlIndex 0, controlIndex 0]
  H.oracleViewControlIndex (H.oracleView second) @?= controlIndex 2
  H.oracleViewVoterConfiguration (H.oracleView second) @?= Just (oracleVoterConfiguration initialOracleState)

configurationOrigins :: Assertion
configurationOrigins = do
  let history = transcript 1
  forM_ (zip history (projectedPrefixes history)) $ \((oracle, entry), herald) -> do
    agrees oracle herald @?= True
    void (H.validateState herald) @?= Right ()
    H.projectedOracleStateDigest herald @?= oracleStateDigest oracle
    let isConfiguration = appliedEntryControlIndex entry >= controlIndex 4
    isNothing (appliedEntryCommand entry) @?= isConfiguration
    isNothing (appliedEntryConfiguration entry) @?= not isConfiguration
  let phases = map (fmap voterChangePhase . H.oracleViewPendingVoterChange . H.oracleView) (projectedPrefixes history)
  phases @?= [Nothing, Nothing, Just VoterChangePreparing, Just VoterChangeJointCommitted, Nothing]
  map voterChangePhase (H.oracleViewVoterChanges (H.oracleView (lastFixture (projectedPrefixes history)))) @?= [VoterChangeCompleted]

demotion :: Assertion
demotion = do
  let after = lastFixture (projectedPrefixes (transcript 1))
      beforeView = H.oracleView initialProjection
      afterView = H.oracleView after
  H.oracleViewActiveHeraldEpochs afterView @?= H.oracleViewActiveHeraldEpochs beforeView
  H.oracleViewCurrentHeraldMembership afterView @?= H.oracleViewCurrentHeraldMembership beforeView
  H.projectedProcessEpochs after @?= H.projectedProcessEpochs initialProjection
  H.projectedEndedProcesses after @?= []
  fmap voterConfigurationBindings (H.oracleViewVoterConfiguration afterView) @?= Just (take 2 bindings)

cancellation :: Assertion
cancellation = do
  let prefix = take 3 (transcript 1)
      preparing = fst (lastFixture prefix)
      ident = voterChangeId (fromJust (oraclePendingVoterChange preparing))
      fourth@(cancelled, _) = run (request 4 (cancelVoterChangeCommand ident)) preparing
      fifth@(reobserved, _) = run (request 5 (cancelVoterChangeCommand ident)) cancelled
      after = lastFixture (projectedPrefixes (prefix <> [fourth, fifth]))
      view = H.oracleView after
  H.oracleViewPendingVoterChange view @?= Nothing
  fmap voterChangePhase (H.oracleViewVoterChange ident view) @?= Just VoterChangeCancelled
  fmap voterChangeControlIndex (H.oracleViewVoterChange ident view) @?= Just (controlIndex 4)
  H.oracleViewControlIndex view @?= controlIndex 5
  H.oracleViewVoterConfiguration view @?= Just (oracleVoterConfiguration initialOracleState)
  void (H.validateState after) @?= Right ()
  let nextBegin = request 6 (beginVoterChangeCommand (voterConfigurationId (oracleVoterConfiguration reobserved)) ExplicitDemotion (checked (oracleVoterBindings (take 2 bindings))))
      sixth@(preparingAgain, _) = run nextBegin reobserved
      seventh@(finalOracle, _) = run (request 7 (cancelVoterChangeCommand ident)) preparingAgain
      finalHerald = lastFixture (projectedPrefixes (prefix <> [fourth, fifth, sixth, seventh]))
      pending = H.oracleViewPendingVoterChange (H.oracleView finalHerald)
  pending @?= oraclePendingVoterChange preparingAgain
  fmap voterChangeId pending @?= Just (checked (mkVoterChangeId (controlIndex 6)))
  assertBool "old terminal reobservation cannot clear a newer pending intention" (agrees finalOracle finalHerald)
  void (H.validateState finalHerald) @?= Right ()

ordering :: Assertion
ordering = do
  let history = transcript 1
      joint = snd (history !! 3)
      gap = checked (H.prepareAppliedEntry (canonicalizeAppliedOracleEntry joint) initialProjection)
  H.preparedAppliedEntryClassification gap @?= H.AppliedEntryGap (controlIndex 1) (controlIndex 4)
  assertBool "gap cannot advance authority" (H.commitAppliedEntry gap == initialProjection)
  let after = lastFixture (projectedPrefixes history)
      conflict = snd (transcript 2 !! 3)
  case H.prepareAppliedEntry (canonicalizeAppliedOracleEntry conflict) after of
    Left (H.AppliedEntryUnequalConflictFault index) -> index @?= controlIndex 4
    _ -> assertFailure "different native configuration evidence replaced a retained coordinate"
  assertBool "old exact joint replay cannot roll back final authority" (apply after joint == after)
  let preparing = projectedPrefixes history !! 2
      alternativeJoint = snd (transcriptFor [firstFixture bindings, bindings !! 2] 1 !! 3)
  case H.prepareAppliedEntry (canonicalizeAppliedOracleEntry alternativeJoint) preparing of
    Left (H.AppliedEntryVoterProjectionMismatch index) -> index @?= controlIndex 4
    _ -> assertFailure "joint observation replaced the previously retained desired bindings"

agrees :: OracleState -> H.State -> Bool
agrees oracle herald =
  let view = H.oracleView herald
   in H.oracleViewControlIndex view == oracleGreatestControlIndex oracle
        && H.oracleViewVoterConfiguration view == Just (oracleVoterConfiguration oracle)
        && H.oracleViewOracleReplicas view == oracleReplicaRegistrations oracle
        && H.oracleViewPendingVoterChange view == oraclePendingVoterChange oracle

duplicatePrefixes :: QC.NonNegative Int -> QC.Positive Word64 -> QC.Property
duplicatePrefixes (QC.NonNegative duplicateCount) (QC.Positive suppliedTerm) =
  let history = transcript (1 + suppliedTerm `mod` 20)
      step state (_, entry) = iterate (`apply` entry) state !! (1 + duplicateCount `mod` 4)
      prefixes = drop 1 (scanl step initialProjection history)
   in QC.conjoin
        [ QC.counterexample
            ("control prefix " <> show (appliedEntryControlIndex entry))
            (agrees oracle projected && void (H.validateState projected) == Right ())
        | ((oracle, entry), projected) <- zip history prefixes
        ]

failurePrelude :: OracleState -> ([OracleEnvelope], [(OracleState, AppliedOracleEntry)])
failurePrelude initial = (commands, commandStates initial commands)
  where
    configuration = oracleVoterConfiguration initial
    voters = voterConfigurationBindings configuration
    target = raftVoterBindingHeraldEpoch (lastFixture voters)
    firstIndex = controlIndexWord64 (oracleGreatestControlIndex initial) + 1
    probe = checked (Membership.deriveHeraldFailureProbeId (controlIndex firstIndex))
    reporters = take (length voters `div` 2 + 1) voters
    commands =
      request firstIndex (openHeraldFailureProbeCommand target (Membership.heraldMembershipGenerationId (oracleCurrentMembership initial)) (voterConfigurationId configuration))
        : [ oracleEnvelope (oracleClientRequestId reporter ordinal) Nothing reporter (reportHeraldFailureProbeCommand probe (voterConfigurationId configuration) ProbeUnreachable)
          | (ordinal, binding) <- zip [firstIndex + 1 ..] reporters,
            let reporter = raftVoterBindingHeraldEpoch binding
          ]
          <> [request (firstIndex + fromIntegral (length reporters) + 1) (acceptVoterHostFailureCommand (Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget))]
    commandStates _ [] = []
    commandStates state (envelope : remaining) = let result@(successor, _) = run envelope state in result : commandStates successor remaining

failureTranscript :: Word64 -> [(OracleState, AppliedOracleEntry)]
failureTranscript term =
  let (commands, prefix) = failurePrelude initialOracleState
      preparing = fst (lastFixture prefix)
      pending = fromJust (oraclePendingVoterChange preparing)
      ident = voterChangeId pending
      resolution = case voterChangeReason pending of AcceptedHostFailureReason value -> value; _ -> error "expected automatic voter intent"
      target = raftVoterBindingHeraldEpoch (lastFixture bindings)
      (joint, jointMetadata) = checked (prepareOracleConfiguration ident JointConfigurationStage preparing)
      nativePrefix = map application commands <> [R.Configuration joint jointMetadata]
      jointResult@(jointState, _) = checked (applyCommittedOracleConfiguration (sealedLast term nativePrefix) preparing)
      (final, finalMetadata) = checked (prepareOracleConfiguration ident FinalConfigurationStage jointState)
      finalResult@(excluded, _) = checked (applyCommittedOracleConfiguration (sealedLast term (nativePrefix <> [R.Configuration final finalMetadata])) jointState)
      retirement = retireHeraldEpochCommand resolution target
      retiredResult@(retired, _) = run (request 70 retirement) excluded
      beginAgain = request 71 (beginVoterChangeCommand (voterConfigurationId (oracleVoterConfiguration retired)) ExplicitDemotion (checked (oracleVoterBindings (take 1 bindings))))
      newIntent@(another, _) = run beginAgain retired
      replayedAcceptance@(sameIntent, _) = run (request 72 (acceptVoterHostFailureCommand resolution)) another
      replayedRetirement = run (request 73 retirement) sameIntent
   in prefix <> [jointResult, finalResult, retiredResult, newIntent, replayedAcceptance, replayedRetirement]

automaticFailureProjection :: Assertion
automaticFailureProjection = do
  let history = failureTranscript 1
      prefixes = projectedPrefixes history
      probe = checked (Membership.deriveHeraldFailureProbeId (controlIndex 1))
      accepted = prefixes !! 3
      excluded = prefixes !! 5
      retired = prefixes !! 6
      terminal projected = H.projectedFailureProbeTerminal =<< H.oracleViewFailureProbe probe (H.oracleView projected)
  forM_ (zip history prefixes) $ \((oracle, _), projected) -> do
    assertBool "automatic failure prefix agrees with Oracle" (agrees oracle projected)
    void (H.validateState projected) @?= Right ()
  case terminal accepted of
    Just (H.ProjectedVoterHostFailureAccepted certificate) -> do
      acceptedVoterHostFailureConfiguration certificate @?= oracleVoterConfiguration initialOracleState
      terminal excluded @?= Just (H.ProjectedVoterHostFailureAccepted certificate)
      terminal retired @?= Just (H.ProjectedVoterHostFailureAccepted certificate)
      terminal (lastFixture prefixes) @?= Just (H.ProjectedVoterHostFailureAccepted certificate)
    other -> assertFailure ("missing retained acceptance terminal: " <> show other)
  fmap voterChangePhase (H.oracleViewPendingVoterChange (H.oracleView excluded)) @?= Just VoterExcludedAwaitingHeraldRetirement
  H.oracleViewCurrentHeraldMembership (H.oracleView excluded) @?= H.oracleViewCurrentHeraldMembership (H.oracleView initialProjection)
  H.oracleViewPendingVoterChange (H.oracleView retired) @?= Nothing
  assertBool "old acceptance/retirement reobservation preserves a newer slot" (H.oracleViewPendingVoterChange (H.oracleView (lastFixture prefixes)) /= Nothing)

failureConfigurationMixing :: Assertion
failureConfigurationMixing = do
  let ownPrefix = transcript 1
      otherPrefix = transcript 2
      ownState = fst (lastFixture ownPrefix)
      otherState = fst (lastFixture otherPrefix)
      (_, ownFailure) = failurePrelude ownState
      (_, otherFailure) = failurePrelude otherState
      ownProjected = lastFixture (projectedPrefixes ownPrefix)
      foreignOpen = snd (firstFixture otherFailure)
  case H.prepareAppliedEntry (canonicalizeAppliedOracleEntry foreignOpen) ownProjected of
    Left (H.AppliedEntryVoterProjectionMismatch _) -> pure ()
    other -> case other of Left problem -> assertFailure (show problem); Right _ -> assertFailure "foreign captured voter configuration admitted at Open"
  let reported = lastFixture (projectedPrefixes (ownPrefix <> take (length ownFailure - 1) ownFailure))
      foreignAcceptance = snd (lastFixture otherFailure)
  case H.prepareAppliedEntry (canonicalizeAppliedOracleEntry foreignAcceptance) reported of
    Left (H.AppliedEntryVoterProjectionMismatch _) -> pure ()
    other -> case other of Left problem -> assertFailure (show problem); Right _ -> assertFailure "accepted certificate borrowed a different captured native configuration"

automaticDuplicatePrefixes :: QC.Positive Word64 -> QC.Property
automaticDuplicatePrefixes (QC.Positive supplied) =
  let history = failureTranscript (1 + supplied `mod` 20)
      step projected (_, entry) = apply (apply projected entry) entry
      prefixes = drop 1 (scanl step initialProjection history)
   in QC.conjoin [QC.counterexample ("automatic failure control " <> show (appliedEntryControlIndex entry)) (agrees oracle projected && void (H.validateState projected) == Right ()) | ((oracle, entry), projected) <- zip history prefixes]
