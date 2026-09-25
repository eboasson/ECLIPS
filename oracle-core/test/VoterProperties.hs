{-# LANGUAGE OverloadedStrings #-}

-- | Oracle composition independently replays a sealed native history. Native
-- quorum/election correctness remains covered by raft-core's model properties.
module VoterProperties (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Serialize qualified as S
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity (controlIndex, controlIndexWord64, heraldEpochBytes, mkTopologyCutId)
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.Topology qualified as Topology
import Eclips.Oracle.Admission
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
import OracleFixtures
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
    "retained Oracle voter administration"
    [ testCase "accepted voter failure survives joint, final exclusion and semantic retirement" acceptedFailureWorkflow,
      testCase "accepted certificates reject incomplete, duplicate and stale proof shapes" failureCertificateAdmission,
      testCase "failure thresholds keep the captured old denominator and cannot retire current voters" failureOldDenominator,
      QC.testProperty "accepted failure replay and checkpoint continuation agree at every phase" failureReplayPrefixes,
      testCase "singleton commissions two registered active learners through one joint transition" singletonCommission,
      testCase "admission, voter change and accepted retirement share the coordination slot" coordinationSlot,
      testCase "registration fills genesis hints once and preserves exact retry receipt" registration,
      testCase "bindings are injective and hosts must already be active" registrationAdmission,
      testCase "retained cancellation terminalizes an unstarted intention" cancellation,
      testCase "native metadata cannot invent, replace or revive an intention" metadataAdmission,
      testCase "native joint/final commits have control events without client receipts" orderedConfiguration,
      testCase "committed demotion preserves processes and changes failure eligibility" demotionRoles,
      QC.testProperty "every replay and checkpoint prefix agrees through no-op, command, retry, conflict, joint and final" replayPrefixes,
      QC.testProperty "configuration identity binds the actual native entry term" configurationIdentity,
      QC.testProperty "all typed voter transcripts round-trip canonically" canonicalRoundtrips
    ]

checkedValue :: (Show problem) => Either problem value -> value
checkedValue = checked "voter property"
contact :: OracleReplicaContact
contact = oracleReplicaContact (endpoint 14001) (endpoint 14002) (endpoint 14003)
  where
    endpoint = checkedValue . oracleReplicaEndpoint "127.0.0.1"
node :: Int -> RI.RaftNodeId
node index = fixtureRaftNodes !! index
bindings :: [Int] -> OracleVoterBindings
bindings indices = checkedValue (oracleVoterBindings [fixtureBindings !! index | index <- indices])
run :: OracleEnvelope -> OracleState -> (OracleState, OracleReceipt, [AppliedOracleEntry])
run envelope state = case checkedValue (stepOracle envelope state) of
  (successor, OracleCommitted receipt, effects) -> (successor, receipt, [entry | EmitAppliedOracleEntry entry <- oracleEffects effects])
  (successor, OracleDuplicate receipt, _) -> (successor, receipt, [])
  other -> error ("expected receipt: " <> show other)
request :: Word64 -> OracleCommand -> OracleEnvelope
request = envelopeFor
begin :: OracleEnvelope
begin = request 41 (beginVoterChangeCommand (voterConfigurationId (oracleVoterConfiguration fixtureInitialState)) ExplicitDemotion (bindings [0, 1]))
preparedState :: OracleState
preparedState = let (state, _, _) = run begin fixtureInitialState in state
changeId :: VoterChangeId
changeId = voterChangeId (fromJust (oraclePendingVoterChange preparedState))

-- Materialize sealed exposures using ordinary follower prefix application.
-- No test or public API constructs RaftCommittedEntry directly.
sealed :: Word64 -> [R.RaftEntry ByteString] -> [RE.RaftCommittedEntry ByteString]
sealed = sealedFor fixtureCheckedGenesis
sealedFor :: CheckedOracleGenesis -> Word64 -> [R.RaftEntry ByteString] -> [RE.RaftCommittedEntry ByteString]
sealedFor oracleGenesisValue term payloads =
  let native = checkedOracleRaftNativeConfiguration oracleGenesisValue
      voters = RG.raftNativeVoters native
      check = if node 1 `elem` voters then RG.checkRaftGenesis else RG.checkRaftLearnerGenesis
      genesis = checkedValue (check (RG.raftGenesis (node 1) voters (RG.raftNativeHeartbeatInterval native) (RG.raftNativeElectionTimeoutLower native) (RG.raftNativeElectionTimeoutUpper native)))
      entries = zipWith (\i payload -> R.raftLogEntry (RI.raftLogIndex i) (RI.raftTerm term) payload) [1 ..] payloads
      rpc = checkedValue (R.appendEntries (RI.raftTerm term) (node 0) (RI.raftLogIndex 0) (RI.raftTerm 0) entries (RI.raftLogIndex (fromIntegral (length entries))))
      (_, effects) = checkedValue (RT.stepRaft (checkedValue (R.observeRaftRequest (node 0) rpc)) (RT.initialRaft genesis))
   in [entry | RE.ExposeCommittedEntries batch <- RE.raftEffectBatchEffects effects, entry <- NE.toList batch]
application :: OracleEnvelope -> R.RaftEntry ByteString
application = R.Application . canonicalOracleEnvelopeBytes . canonicalizeOracleEnvelope
applyOne :: OracleState -> RE.RaftCommittedEntry ByteString -> OracleState
applyOne state entry = case RE.committedEntryPayload entry of
  R.LeaderNoOp -> state
  R.Application bytes -> let canonical = checkedValue (decodeCanonicalOracleEnvelope bytes); (successor, _, _) = checkedValue (stepOracle (canonicalOracleEnvelopeValue canonical) state) in successor
  R.Configuration {} -> fst (checkedValue (applyCommittedOracleConfiguration entry state))

history :: Word64 -> Int -> [RE.RaftCommittedEntry ByteString]
history term padding =
  let (joint, jointBytes) = checkedValue (prepareOracleConfiguration changeId JointConfigurationStage preparedState)
      first = replicate padding R.LeaderNoOp <> [application begin, application begin, application (request 41 validCommand), R.Configuration joint jointBytes]
      jointState = foldl applyOne fixtureInitialState (sealed term first)
      (final, finalBytes) = checkedValue (prepareOracleConfiguration changeId FinalConfigurationStage jointState)
   in sealed term (first <> [R.Configuration final finalBytes, application (request 42 validCommand)])

registration :: Assertion
registration = do
  let envelope = request 1 (registerOracleReplicaCommand (node 0) fixtureHeraldEpoch contact)
      (state, receipt, entries) = run envelope fixtureInitialState
      (again, duplicate, duplicateEntries) = run envelope state
      registration' = firstFixture (oracleReplicaRegistrations state)
  oracleReceiptResult receipt @?= OracleAccepted
  replicaRegistrationContact registration' @?= Just contact
  replicaRegistrationControlIndex registration' @?= controlIndex 1
  duplicate @?= receipt
  again @?= state
  duplicateEntries @?= []
  length entries @?= 1
  let (_, later, _) = run (request 2 (registerOracleReplicaCommand (node 0) fixtureHeraldEpoch contact)) state
  case oracleReceiptVoterResult later of
    Just (ReplicaAlreadyVoter same) -> same @?= registration'
    other -> assertFailure (show other)

registrationAdmission :: Assertion
registrationAdmission = do
  assertBool "duplicate hosts" (isLeft (oracleVoterBindings [firstFixture fixtureBindings, raftVoterBinding (raftNodeId 231) fixtureHeraldEpoch]))
  oracleVoterBindings [] @?= Left VoterWouldRemoveLastVoter
  let (_, inactive, _) = run (request 1 (registerOracleReplicaCommand (raftNodeId 231) (heraldEpoch 232) contact)) fixtureInitialState
  oracleReceiptResult inactive @?= OracleRejected (VoterCommandRejected VoterHostNotActive)
  let (_, conflicting, _) = run (request 2 (registerOracleReplicaCommand (raftNodeId 231) fixtureHeraldEpoch contact)) fixtureInitialState
  oracleReceiptResult conflicting @?= OracleRejected (VoterCommandRejected VoterBindingConflict)
  oracleReplicaEndpoint (BS.pack [255]) 99 @?= Left VoterInvalidEndpoint

cancellation :: Assertion
cancellation = do
  let (state, receipt, _) = run (request 42 (cancelVoterChangeCommand changeId)) preparedState
  oracleReceiptResult receipt @?= OracleAccepted
  oraclePendingVoterChange state @?= Nothing
  fmap voterChangePhase (oracleVoterChange changeId state) @?= Just VoterChangeCancelled
  assertBool "cancelled intention cannot prepare metadata" (isLeft (prepareOracleConfiguration changeId JointConfigurationStage state))
  oracleVoterConfiguration state @?= oracleVoterConfiguration fixtureInitialState

metadataAdmission :: Assertion
metadataAdmission = do
  let (joint, bytes) = checkedValue (prepareOracleConfiguration changeId JointConfigurationStage preparedState)
      native = firstFixture (sealed 1 [R.Configuration joint bytes])
      cancelled = let (state, _, _) = run (request 42 (cancelVoterChangeCommand changeId)) preparedState in state
      malformed = firstFixture (sealed 1 [R.Configuration joint (bytes <> "x")])
  assertBool "absent intention" (isLeft (applyCommittedOracleConfiguration native fixtureInitialState))
  assertBool "cancelled intention" (isLeft (applyCommittedOracleConfiguration native cancelled))
  assertBool "noncanonical metadata" (isLeft (applyCommittedOracleConfiguration malformed preparedState))

orderedConfiguration :: Assertion
orderedConfiguration = do
  let entries = history 3 2
      states = scanl applyOne fixtureInitialState entries
      lastState = lastFixture states
  fmap (controlIndexWord64 . oracleGreatestControlIndex) states @?= [0, 0, 0, 1, 1, 1, 2, 3, 4]
  oracleRequestCount lastState @?= 2
  oraclePendingVoterChange lastState @?= Nothing
  fmap voterChangePhase (oracleVoterChange changeId lastState) @?= Just VoterChangeCompleted
  let configs = [(state, entry) | (state, entry) <- zip states entries, R.Configuration {} <- [RE.committedEntryPayload entry]]
  mapM_
    ( \(state, native) -> do
        let (successor, entry) = checkedValue (applyCommittedOracleConfiguration native state)
        appliedEntryCommand entry @?= Nothing
        appliedEntryConfiguration entry @?= Just (oracleVoterConfiguration successor)
        length (appliedEntryProjectionEvents entry) @?= 2
        oracleRequestCount successor @?= oracleRequestCount state
        canonicalAppliedOracleEntryValue (checkedValue (decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry entry)))) @?= entry
    )
    configs
  let (_, receipt, _) = run begin lastState
      (_, original, _) = run begin fixtureInitialState
  receipt @?= original

demotionRoles :: Assertion
demotionRoles = do
  let final = foldl applyOne fixtureInitialState (history 1 0)
  oracleReplicaReplicationTargets final @?= fixtureRaftNodes
  oracleProcessRecord fixtureProcessEpoch final @?= oracleProcessRecord fixtureProcessEpoch fixtureInitialState
  oracleProcessRecord fixtureRemoteProcessEpoch final @?= oracleProcessRecord fixtureRemoteProcessEpoch fixtureInitialState
  let generation = oracleCurrentMembership final
      open = openHeraldFailureProbeCommand fixtureThirdHeraldEpoch (Membership.heraldMembershipGenerationId generation) (voterConfigurationId (oracleVoterConfiguration final))
      (_, receipt, _) = run (request 90 open) final
  oracleReceiptResult receipt @?= OracleAccepted
  let (_, blocked, _) = run (request 91 open) preparedState
  oracleReceiptResult blocked @?= OracleRejected (VoterCommandRejected VoterChangeInProgress)

replayPrefixes :: QC.NonNegative Int -> QC.Property
replayPrefixes (QC.NonNegative n) =
  let entries = history 1 (n `mod` 7)
      expected = scanl applyOne fixtureInitialState entries
      reconstructed = [foldl applyOne fixtureInitialState (take prefix entries) | prefix <- [0 .. length entries]]
   in QC.conjoin
        [ QC.counterexample "replay state/receipt/configuration differs" (expected QC.=== reconstructed),
          checkpointContinuations expected entries
        ]

-- The next native exposure runs against the restored owner, including failure
-- certificates, dynamic voter hosts and pending configuration intentions.
checkpointContinuations :: [OracleState] -> [RE.RaftCommittedEntry ByteString] -> QC.Property
checkpointContinuations states entries = QC.conjoin (roundtrips <> continuations)
  where
    restore state = decodeOracleCheckpoint (oracleStateGenesis state) (oracleCheckpointBytes state)
    roundtrips = [QC.counterexample ("checkpoint at control index " <> show (oracleGreatestControlIndex state)) (restore state QC.=== Right state) | state <- states]
    continuations = [fmap (\restored -> applyOne restored entry) (restore state) QC.=== Right (applyOne state entry) | (state, entry) <- zip states entries]
configurationIdentity :: QC.Positive Word64 -> QC.Property
configurationIdentity (QC.Positive supplied) =
  let term = 1 + supplied `mod` 1000
      a = oracleVoterConfiguration (foldl applyOne fixtureInitialState (history term 0))
      b = oracleVoterConfiguration (foldl applyOne fixtureInitialState (history (term + 1) 0))
   in QC.conjoin [voterConfigurationBindings a QC.=== voterConfigurationBindings b, QC.property (voterConfigurationId a /= voterConfigurationId b)]
canonicalRoundtrips :: QC.NonNegative Int -> QC.Property
canonicalRoundtrips (QC.NonNegative n) =
  let states = scanl applyOne fixtureInitialState (history 1 (n `mod` 5))
      configs = concatMap oracleVoterConfigurations states
      changes = [change | state <- states, Just change <- [oracleVoterChange changeId state]]
   in QC.conjoin ([decodeVoterConfiguration (encodeVoterConfiguration configuration) QC.=== Right configuration | configuration <- configs] <> [decodeVoterChange (encodeVoterChange change) QC.=== Right change | change <- changes])

singletonGenesis :: CheckedOracleGenesis
singletonGenesis = checkedValue (checkOracleGenesis value)
  where
    old = fixtureCheckedGenesis
    bindings' = take 1 fixtureBindings
    native = raftConfigurationForVoters [node 0] 100000 300000 500000
    value =
      oracleGenesis
        (checkedOracleSystemId old)
        (checkedOracleActiveHeralds old)
        (checkedOracleCatalogueDigest old)
        (checkedOraclePredefinedDescriptors old)
        (checkedOracleAppliedBootstraps old)
        (checkedOracleConfigurationDigest old)
        (checkedOracleInitialTopologyProjection old)
        (checkedOracleInitialProjectionDigest old)
        bindings'
        native
        (deriveRaftConfigurationDigest (checkedOracleSystemId old) bindings' native)

singletonCommission :: Assertion
singletonCommission = do
  let initial = checkedValue (initialOracle singletonGenesis)
      registerB = request 1 (registerOracleReplicaCommand (node 1) fixtureRemoteHeraldEpoch contact)
      registerC = request 2 (registerOracleReplicaCommand (node 2) fixtureThirdHeraldEpoch contact)
      (registeredB, _, _) = run registerB initial
      (registered, _, _) = run registerC registeredB
      command = request 3 (beginVoterChangeCommand (voterConfigurationId (oracleVoterConfiguration initial)) ExplicitCommission (bindings [0, 1, 2]))
      (preparing, receipt, _) = run command registered
      change = fromJust (oraclePendingVoterChange preparing)
      (joint, jointMetadata) = checkedValue (prepareOracleConfiguration (voterChangeId change) JointConfigurationStage preparing)
      prefix = fmap application [registerB, registerC, command] <> [R.Configuration joint jointMetadata]
      jointState = foldl applyOne initial (sealedFor singletonGenesis 1 prefix)
      (final, finalMetadata) = checkedValue (prepareOracleConfiguration (voterChangeId change) FinalConfigurationStage jointState)
      completed = foldl applyOne initial (sealedFor singletonGenesis 1 (prefix <> [R.Configuration final finalMetadata]))
  oracleReceiptResult receipt @?= OracleAccepted
  voterChangeLearners change @?= [node 1, node 2]
  voterConfigurationView (oracleVoterConfiguration preparing) @?= StableVoterConfigurationView (bindings [0])
  voterConfigurationView (oracleVoterConfiguration jointState) @?= JointVoterConfigurationView (bindings [0]) (bindings [0, 1, 2])
  voterConfigurationView (oracleVoterConfiguration completed) @?= StableVoterConfigurationView (bindings [0, 1, 2])
  oracleGreatestControlIndex completed @?= controlIndex 5
  oracleRequestCount completed @?= 3
  oraclePendingVoterChange completed @?= Nothing
  map replicaRegistrationNode (oracleReplicaRegistrations completed) @?= fixtureRaftNodes

coordinationSlot :: Assertion
coordinationSlot = do
  let initial = checkedValue (initialOracle singletonGenesis)
      generation = oracleCurrentMembership initial
      digest = checkedValue (Topology.mkTopologyOccurrenceDigest (identifierBytes 220))
      predecessor = Topology.sameGenerationPredecessor (checkedValue (mkTopologyCutId (identifierBytes 240)))
      anchor = checkedValue (Topology.topologyCut predecessor (Topology.topologyFrontier (Structural.emptyStructuralVersionVector generation) (controlIndex 0)) digest)
      admission = beginHeraldAdmissionCommand (heraldAdmissionManifest fixtureSystemId (heraldId 221) (heraldEpoch 222)) anchor
      (admitting, _, _) = run (request 1 admission) initial
      (registered, _, _) = run (request 2 (registerOracleReplicaCommand (node 1) fixtureRemoteHeraldEpoch contact)) admitting
      commission = beginVoterChangeCommand (voterConfigurationId (oracleVoterConfiguration initial)) ExplicitCommission (bindings [0, 1])
      (_, busy, _) = run (request 3 commission) registered
      (_, blockedAdmission, _) = run (request 50 admission) preparedState
  oracleReceiptResult busy @?= OracleRejected (VoterCommandRejected VoterMembershipCoordinationBusy)
  oracleReceiptResult blockedAdmission @?= OracleRejected (VoterCommandRejected VoterChangeInProgress)
  let (registeredOnly, _, _) = run (request 1 (registerOracleReplicaCommand (node 1) fixtureRemoteHeraldEpoch contact)) initial
      open = openHeraldFailureProbeCommand fixtureThirdHeraldEpoch (Membership.heraldMembershipGenerationId generation) (voterConfigurationId (oracleVoterConfiguration initial))
      (opened, _, _) = run (request 2 open) registeredOnly
      probe = fromJust (oracleActiveFailureProbeFor fixtureThirdHeraldEpoch (Membership.heraldMembershipGenerationId generation) opened)
      (acceptedFailure, _, _) = run (request 3 (reportHeraldFailureProbeCommand probe (voterConfigurationId (oracleVoterConfiguration initial)) ProbeUnreachable)) opened
      (_, cannotRebase, _) = run (request 4 commission) acceptedFailure
      (voterPreparing, _, events) = run (request 4 commission) opened
  oracleReceiptResult cannotRebase @?= OracleRejected (VoterCommandRejected VoterMembershipCoordinationBusy)
  let (_, cannotAdmit, _) = run (request 6 admission) acceptedFailure
  oracleReceiptResult cannotAdmit @?= OracleRejected (VoterCommandRejected VoterMembershipCoordinationBusy)
  assertBool "voter intention retained" (oraclePendingVoterChange voterPreparing /= Nothing)
  assertBool "unaccepted probe superseded by new voter intention" (any (any isSuperseded . appliedEntryProjectionEvents) events)
  let (_, lateReport, _) = run (request 5 (reportHeraldFailureProbeCommand probe (voterConfigurationId (oracleVoterConfiguration initial)) ProbeUnreachable)) voterPreparing
  assertBool "prior probe cannot acquire reports after supersession" (oracleReceiptResult lateReport /= OracleAccepted)
  where
    isSuperseded event = case oracleProjectionEventView event of OracleFailureProbeSupersededView {} -> True; _ -> False

failureCommands :: [OracleEnvelope]
failureCommands =
  [ request 51 (openHeraldFailureProbeCommand fixtureRemoteHeraldEpoch (Membership.heraldMembershipGenerationId (oracleCurrentMembership fixtureInitialState)) initialConfigurationId),
    request 52 (reportHeraldFailureProbeCommand failureProbeId initialConfigurationId ProbeUnreachable),
    oracleEnvelope (oracleClientRequestId fixtureThirdHeraldEpoch 53) Nothing fixtureThirdHeraldEpoch (reportHeraldFailureProbeCommand failureProbeId initialConfigurationId ProbeUnreachable),
    request 54 (acceptVoterHostFailureCommand failureResolution)
  ]
initialConfigurationId :: VoterConfigurationId
initialConfigurationId = voterConfigurationId (oracleVoterConfiguration fixtureInitialState)
failureProbeId :: Membership.HeraldFailureProbeId
failureProbeId = checkedValue (Membership.deriveHeraldFailureProbeId (controlIndex 1))
failureResolution :: Membership.FailureProbeResolutionId
failureResolution = Membership.deriveFailureProbeResolutionId failureProbeId Membership.RetireFailureProbeTarget
failurePreparing :: OracleState
failurePreparing = foldl (\state envelope -> let (next, _, _) = run envelope state in next) fixtureInitialState failureCommands
failureChange :: VoterChange
failureChange = fromJust (oraclePendingVoterChange failurePreparing)
failureCertificate :: AcceptedVoterHostFailure
failureCertificate = case oracleFailureProbe failureProbeId failurePreparing of
  Just (FailureProbeView _ _ _ _ _ _ (Just (ProbeVoterHostFailureAcceptedView certificate))) -> certificate
  other -> error ("expected accepted failure fixture: " <> show other)

failureHistory :: Word64 -> [RE.RaftCommittedEntry ByteString]
failureHistory term =
  let (joint, jointMetadata) = checkedValue (prepareOracleConfiguration (voterChangeId failureChange) JointConfigurationStage failurePreparing)
      throughJoint = fmap application failureCommands <> [R.Configuration joint jointMetadata]
      jointState = foldl applyOne fixtureInitialState (sealed term throughJoint)
      (final, finalMetadata) = checkedValue (prepareOracleConfiguration (voterChangeId failureChange) FinalConfigurationStage jointState)
   in sealed term (throughJoint <> [R.Configuration final finalMetadata, application (request 55 (retireHeraldEpochCommand failureResolution fixtureRemoteHeraldEpoch))])

acceptedFailureWorkflow :: Assertion
acceptedFailureWorkflow = do
  let states = scanl applyOne fixtureInitialState (failureHistory 1)
      joint = states !! 5
      excluded = states !! 6
      completed = states !! 7
      retire = retireHeraldEpochCommand failureResolution fixtureRemoteHeraldEpoch
      (_, originalEntries) = foldl (\(state, retained) envelope -> let (next, _, emitted) = run envelope state in (next, retained <> emitted)) (fixtureInitialState, []) failureCommands
      acceptance = acceptedVoterHostFailureControlIndex failureCertificate
  -- Live Heralds may reclaim these entries. The Oracle transition itself
  -- proves the unique original event and immutable later reoffers.
  assertEqual "acceptance is emitted exactly once at its original control index" [(acceptance, failureCertificate)] (acceptanceEvents originalEntries)
  voterChangeReason failureChange @?= AcceptedHostFailureReason failureResolution
  voterChangeOldBindings failureChange @?= bindings [0, 1, 2]
  voterChangeNewBindings failureChange @?= bindings [0, 2]
  acceptedVoterHostFailureConfiguration failureCertificate @?= oracleVoterConfiguration fixtureInitialState
  acceptedVoterHostFailureReports failureCertificate @?= [(fixtureHeraldEpoch, ProbeUnreachable), (fixtureThirdHeraldEpoch, ProbeUnreachable)]
  acceptedVoterHostFailureChangeId failureCertificate @?= voterChangeId failureChange
  oracleReplicaReplicationTargets failurePreparing @?= fixtureRaftNodes
  oracleReplicaReplicationTargets joint @?= fixtureRaftNodes
  oracleReplicaReplicationTargets excluded @?= [node 0, node 2]
  oracleReplicaReplicationTargets completed @?= [node 0, node 2]
  length (oracleReplicaRegistrations completed) @?= 3
  oracleCurrentMembership excluded @?= oracleCurrentMembership fixtureInitialState
  fmap voterChangePhase (oraclePendingVoterChange excluded) @?= Just VoterExcludedAwaitingHeraldRetirement
  assertBool "native final cannot be proposed twice" (isLeft (prepareOracleConfiguration (voterChangeId failureChange) FinalConfigurationStage excluded))
  mapM_
    ( \state -> do
        let (_, receipt, _) = run (request 60 retire) state
        oracleReceiptResult receipt @?= OracleRejected (FailureCommandRejected (FailureVoterExclusionNotApplied fixtureRemoteHeraldEpoch))
    )
    [failurePreparing, joint]
  let (_, cancelled, _) = run (request 61 (cancelVoterChangeCommand (voterChangeId failureChange))) failurePreparing
  oracleReceiptResult cancelled @?= OracleRejected (VoterCommandRejected VoterAcceptedFailureNotCancellable)
  oraclePendingVoterChange completed @?= Nothing
  fmap voterChangePhase (oracleVoterChange (voterChangeId failureChange) completed) @?= Just VoterChangeCompleted
  oracleRetiredHeralds completed @?= [fixtureRemoteHeraldEpoch]
  assertBool "resident process is terminally ended" (oracleProcessRecord fixtureRemoteProcessEpoch completed /= oracleProcessRecord fixtureRemoteProcessEpoch fixtureInitialState)
  oracleFailureProbe failureProbeId completed @?= oracleFailureProbe failureProbeId failurePreparing
  let (retriedRetirement, retirementReceipt, _) = run (request 62 retire) completed
  oracleReceiptFailureResult retirementReceipt @?= Just (HeraldRetiredResult failureResolution (Membership.heraldMembershipGenerationId (oracleCurrentMembership completed)))
  oracleMembershipHistory retriedRetirement @?= oracleMembershipHistory completed
  let (nextIntent, nextReceipt, _) = run (request 64 (beginVoterChangeCommand (voterConfigurationId (oracleVoterConfiguration completed)) ExplicitDemotion (bindings [0]))) completed
      (oldAcceptance, _, _) = run (request 65 (acceptVoterHostFailureCommand failureResolution)) nextIntent
      (oldRetirement, _, _) = run (request 66 retire) oldAcceptance
  oracleReceiptResult nextReceipt @?= OracleAccepted
  oraclePendingVoterChange oldAcceptance @?= oraclePendingVoterChange nextIntent
  oraclePendingVoterChange oldRetirement @?= oraclePendingVoterChange nextIntent
  mapM_
    ( \state -> do
        let (retried, receipt, entries) = run (request 63 (acceptVoterHostFailureCommand failureResolution)) state
        oracleReceiptFailureResult receipt @?= Just (VoterHostFailureAcceptedResult failureCertificate)
        oraclePendingVoterChange retried @?= oraclePendingVoterChange state
        oracleVoterConfiguration retried @?= oracleVoterConfiguration state
        assertEqual "a fresh acceptance reoffer emits the immutable original certificate" [(oracleReceiptControlIndex receipt, failureCertificate)] (acceptanceEvents entries)
        assertBool "acceptance reoffers follow the original event" (oracleReceiptControlIndex receipt > acceptance)
        assertEqual "reoffers cannot duplicate the original acceptance coordinate" [(acceptance, failureCertificate)] (filter ((== acceptance) . fst) (acceptanceEvents (originalEntries <> entries)))
        let (duplicate, duplicateReceipt, duplicateEntries) = run (request 63 (acceptVoterHostFailureCommand failureResolution)) retried
        assertBool "the exact request duplicate leaves Oracle state unchanged" (duplicate == retried)
        assertEqual "the exact request duplicate retains its original receipt" receipt duplicateReceipt
        assertEqual "the exact request duplicate emits no new acceptance event" [] duplicateEntries
        mapM_ assertEntryRoundTrip entries
    )
    [failurePreparing, joint, excluded, completed]
  where
    acceptanceEvents entries = [(appliedEntryControlIndex entry, certificate) | entry <- entries, event <- appliedEntryProjectionEvents entry, OracleVoterHostFailureAcceptedView certificate <- [oracleProjectionEventView event]]
    assertEntryRoundTrip entry = canonicalAppliedOracleEntryValue (checkedValue (decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry entry)))) @?= entry

failureOldDenominator :: Assertion
failureOldDenominator = do
  let opened = let (state, _, _) = run (firstFixture failureCommands) fixtureInitialState in state
      (oneReport, _, _) = run (failureCommands !! 1) opened
      (_, minority, _) = run (request 90 (acceptVoterHostFailureCommand failureResolution)) oneReport
      (_, directRetirement, _) = run (request 91 (retireHeraldEpochCommand failureResolution fixtureRemoteHeraldEpoch)) (foldl (\state envelope -> let (next, _, _) = run envelope state in next) fixtureInitialState (take 3 failureCommands))
      stale = checkedValue (mkVoterConfigurationId (identifierBytes 240))
      (_, staleReport, _) = run (request 92 (reportHeraldFailureProbeCommand failureProbeId stale ProbeUnreachable)) opened
      (_, staleOpen, _) = run (request 93 (openHeraldFailureProbeCommand fixtureRemoteHeraldEpoch (Membership.heraldMembershipGenerationId (oracleCurrentMembership opened)) stale)) opened
  oracleReceiptResult minority @?= OracleRejected (FailureCommandRejected (FailureProbeThresholdNotReached failureProbeId Membership.RetireFailureProbeTarget))
  oracleReceiptResult directRetirement @?= OracleRejected (FailureCommandRejected (FailureProbeTargetHostsVoter fixtureRemoteHeraldEpoch))
  oracleReceiptResult staleReport @?= OracleRejected (FailureCommandRejected (StaleFailureVoterConfiguration stale initialConfigurationId))
  oracleReceiptResult staleOpen @?= OracleRejected (FailureCommandRejected (StaleFailureVoterConfiguration stale initialConfigurationId))
  mapM_ checkSmall [1, 2]
  where
    checkSmall count = do
      let genesis = genesisWithVoters (take count fixtureBindings)
          initial = checkedValue (initialOracle genesis)
          configuration = voterConfigurationId (oracleVoterConfiguration initial)
          target = if count == 1 then fixtureHeraldEpoch else fixtureRemoteHeraldEpoch
          (opened, _, _) = run (request 1 (openHeraldFailureProbeCommand target (Membership.heraldMembershipGenerationId (oracleCurrentMembership initial)) configuration)) initial
          (reported, _, _) = run (request 2 (reportHeraldFailureProbeCommand failureProbeId configuration ProbeUnreachable)) opened
          (_, receipt, _) = run (request 3 (acceptVoterHostFailureCommand failureResolution)) reported
      if count == 1
        then oracleReceiptResult receipt @?= OracleRejected (VoterCommandRejected VoterWouldRemoveLastVoter)
        else oracleReceiptResult receipt @?= OracleRejected (FailureCommandRejected (FailureProbeThresholdNotReached failureProbeId Membership.RetireFailureProbeTarget))
      oraclePendingVoterChange reported @?= Nothing

genesisWithVoters :: [RaftVoterBinding] -> CheckedOracleGenesis
genesisWithVoters selected = checkedValue (checkOracleGenesis value)
  where
    old = fixtureCheckedGenesis
    native = raftConfigurationForVoters (fmap raftVoterBindingNode selected) 100000 300000 500000
    value = oracleGenesis (checkedOracleSystemId old) (checkedOracleActiveHeralds old) (checkedOracleCatalogueDigest old) (checkedOraclePredefinedDescriptors old) (checkedOracleAppliedBootstraps old) (checkedOracleConfigurationDigest old) (checkedOracleInitialTopologyProjection old) (checkedOracleInitialProjectionDigest old) selected native (deriveRaftConfigurationDigest (checkedOracleSystemId old) selected native)

failureReplayPrefixes :: QC.Positive Word64 -> QC.Property
failureReplayPrefixes (QC.Positive supplied) =
  let entries = failureHistory (1 + supplied `mod` 1000)
      states = scanl applyOne fixtureInitialState entries
      checks = [state QC.=== foldl applyOne fixtureInitialState (take prefix entries) | (prefix, state) <- zip [0 ..] states]
      changes = [change | state <- states, Just change <- [oracleVoterChange (voterChangeId failureChange) state]]
      canonical = [decodeVoterChange (encodeVoterChange change) QC.=== Right change | change <- changes]
      roundtrip state native = case RE.committedEntryPayload native of
        R.Configuration {} -> let (_, entry) = checkedValue (applyCommittedOracleConfiguration native state) in [entryCheck entry]
        R.Application bytes -> let envelope = canonicalOracleEnvelopeValue (checkedValue (decodeCanonicalOracleEnvelope bytes)); (_, _, projected) = run envelope state in map entryCheck projected
        R.LeaderNoOp -> []
      entryCheck entry = canonicalAppliedOracleEntryValue (checkedValue (decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry entry)))) QC.=== entry
   in QC.conjoin (checks <> canonical <> concat (zipWith roundtrip states entries) <> [checkpointContinuations states entries, decodeAcceptedVoterHostFailure (encodeAcceptedVoterHostFailure failureCertificate) QC.=== Right failureCertificate])

failureCertificateAdmission :: Assertion
failureCertificateAdmission = do
  let bytes = encodeAcceptedVoterHostFailure failureCertificate
      raw = checkedValue (S.decode bytes) :: (ByteString, ByteString, ByteString, ByteString, [(ByteString, Word8)], Word64)
      (resolution, target, generation, configuration, reports, index) = raw
      encode :: ByteString -> [(ByteString, Word8)] -> Word64 -> ByteString
      encode changedTarget changedReports changedIndex = S.encode (resolution, changedTarget, generation, configuration, changedReports, changedIndex)
      invalid =
        [ bytes <> "trailing",
          encode target (reverse reports) index,
          encode target (reports <> reports) index,
          encode target (take 1 reports) index,
          encode target (map (\(host, _) -> (host, 0)) reports) index,
          encode (heraldEpochBytes (heraldEpoch 239)) reports index,
          encode target reports 1
        ]
  decodeAcceptedVoterHostFailure bytes @?= Right failureCertificate
  mapM_ (\malformed -> assertBool "malformed accepted certificate admitted" (isLeft (decodeAcceptedVoterHostFailure malformed))) invalid
