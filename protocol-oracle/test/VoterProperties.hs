{-# LANGUAGE OverloadedStrings #-}

module VoterProperties (tests) where

import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.Membership qualified as Membership
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command
import Eclips.Oracle.Effect
import Eclips.Oracle.Failure
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection
import Eclips.Oracle.Receipt qualified as Receipt
import Eclips.Oracle.State qualified as State
import Eclips.Oracle.Transition
import Eclips.Oracle.Voter
import Eclips.Protocol.Oracle.Codec
import Eclips.Protocol.Oracle.Types qualified as Wire
import Eclips.Raft.Effect qualified as RE
import Eclips.Raft.Genesis qualified as RG
import Eclips.Raft.Identity qualified as RI
import Eclips.Raft.Input qualified as R
import Eclips.Raft.Transition qualified as RT
import OracleFixtures qualified as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

checked :: (Show error) => Either error value -> value
checked = F.checked "voter EORC fixture"

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
    "voter EORC carriers"
    [ testCase "accepted voter-host failure spans command and native origins through immutable replay" acceptedFailureCarriers,
      testCase "retained registration and Begin/Cancel commands use ordinary receipt carriers" commandCarriers,
      testCase "configuration observations share contiguous watches without command receipts" mixedWatch,
      testCase "native entry constructor has a distinct exact canonical domain" constructorGolden,
      testCase "trailing or unknown native observation bytes cannot enter a watch" rejectedObservation
    ]

contact :: OracleReplicaContact
contact = oracleReplicaContact endpoint endpoint endpoint
  where
    endpoint = checked (oracleReplicaEndpoint "127.0.0.1" 14001)
begin :: OracleEnvelope
begin = F.envelopeFor 41 (beginVoterChangeCommand (voterConfigurationId (oracleVoterConfiguration F.fixtureInitialState)) ExplicitDemotion (checked (oracleVoterBindings (take 2 F.fixtureBindings))))
entries :: [AppliedOracleEntry]
entries =
  let (state, _, effects) = checked (stepOracle begin F.fixtureInitialState)
      change = fromJust (oraclePendingVoterChange state)
      (joint, metadata) = checked (prepareOracleConfiguration (voterChangeId change) JointConfigurationStage state)
      native = checkedOracleRaftNativeConfiguration F.fixtureCheckedGenesis
      leader = firstFixture F.fixtureRaftNodes
      follower = F.fixtureRaftNodes !! 1
      genesis = checked (RG.checkRaftGenesis (RG.raftGenesis follower F.fixtureRaftNodes (RG.raftNativeHeartbeatInterval native) (RG.raftNativeElectionTimeoutLower native) (RG.raftNativeElectionTimeoutUpper native)))
      payloads = [R.Application (canonicalOracleEnvelopeBytes (canonicalizeOracleEnvelope begin)), R.Configuration joint metadata]
      logEntries = zipWith (\index payload -> R.raftLogEntry (RI.raftLogIndex index) (RI.raftTerm 1) payload) [1 ..] payloads
      rpc = checked (R.appendEntries (RI.raftTerm 1) leader (RI.raftLogIndex 0) (RI.raftTerm 0) logEntries (RI.raftLogIndex 2))
      (_, batch) = checked (RT.stepRaft (checked (R.observeRaftRequest leader rpc)) (RT.initialRaft genesis))
      configEntry = firstFixture [entry | RE.ExposeCommittedEntries sealed <- RE.raftEffectBatchEffects batch, entry <- NE.toList sealed, R.Configuration {} <- [RE.committedEntryPayload entry]]
      (_, applied) = checked (applyCommittedOracleConfiguration configEntry state)
   in [entry | EmitAppliedOracleEntry entry <- oracleEffects effects] <> [applied]

commandCarriers :: Assertion
commandCarriers = do
  let (prepared, _, _) = checked (stepOracle begin F.fixtureInitialState)
      ident = voterChangeId (fromJust (oraclePendingVoterChange prepared))
      register = F.envelopeFor 1 (registerOracleReplicaCommand (firstFixture F.fixtureRaftNodes) F.fixtureHeraldEpoch contact)
      cancel = F.envelopeFor 42 (cancelVoterChangeCommand ident)
  mapM_
    ( \(envelope, state) -> do
        let dto = canonicalOracleEnvelopeDtoFromValue envelope
        canonicalOracleEnvelopeDtoValue dto @?= Right envelope
        let message = Wire.OracleClientEnvelope (Wire.SubmitOracleCommand dto)
        decodeOracleEnvelope (encodeOracleEnvelope message) @?= Right message
        let (_, outcome, effects) = checked (stepOracle envelope state)
        case outcome of
          OracleCommitted receipt -> do
            Receipt.oracleReceiptResult receipt @?= Receipt.OracleAccepted
            canonicalOracleReceiptDtoValue (canonicalOracleReceiptDtoFromValue receipt) @?= Right receipt
            mapM_ (\entry -> canonicalAppliedOracleEntryDtoValue (canonicalAppliedOracleEntryDtoFromValue entry) @?= Right entry) [entry | EmitAppliedOracleEntry entry <- oracleEffects effects]
          other -> assertFailure (show other)
    )
    [(register, F.fixtureInitialState), (begin, F.fixtureInitialState), (cancel, prepared)]

mixedWatch :: Assertion
mixedWatch = do
  let batch = checked (committedOracleEntriesDtoFromValues (controlIndex 0) entries)
      message = Wire.OracleServerEnvelope (Wire.CommittedOracleEntries batch)
  decodeOracleEnvelope (encodeOracleEnvelope message) @?= Right message
  fmap NE.toList (committedOracleEntriesDtoValues batch) @?= Right entries
  appliedEntryCommand (lastFixture entries) @?= Nothing
  assertBool "configuration projection present" (appliedEntryConfiguration (lastFixture entries) /= Nothing)
  assertBool "control gap rejected across the two entry origins" (isLeft (committedOracleEntriesDtoFromValues (controlIndex 0) [lastFixture entries]))

constructorGolden :: Assertion
constructorGolden = do
  let bytes = Wire.canonicalAppliedOracleEntryDtoBytes (canonicalAppliedOracleEntryDtoFromValue (lastFixture entries))
      domain = "ECLIPS-APPLIED-ORACLE-CONFIGURATION"
      expected = BS.pack [0, 0, 0, 0, 0, 0, 0, 35] <> domain <> BS.pack [0, 0, 0, 0, 0, 0, 0, 2]
  BS.take (BS.length expected) bytes @?= expected

rejectedObservation :: Assertion
rejectedObservation = do
  let bytes = Wire.canonicalAppliedOracleEntryDtoBytes (canonicalAppliedOracleEntryDtoFromValue (lastFixture entries))
  assertBool "trailing native observation" (isLeft (canonicalAppliedOracleEntryDtoValue (Wire.canonicalAppliedOracleEntryDto (bytes <> "x"))))
  assertBool "unknown constructor domain" (isLeft (canonicalAppliedOracleEntryDtoValue (Wire.canonicalAppliedOracleEntryDto (BS.take 8 bytes <> "X" <> BS.drop 9 bytes))))

acceptedFailureCarriers :: Assertion
acceptedFailureCarriers = do
  let initial = F.fixtureInitialState
      configuration = voterConfigurationId (oracleVoterConfiguration initial)
      probe = checked (Membership.deriveHeraldFailureProbeId (controlIndex 1))
      resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget
      commands =
        [ F.envelopeFor 51 (openHeraldFailureProbeCommand F.fixtureRemoteHeraldEpoch (Membership.heraldMembershipGenerationId (State.oracleCurrentMembership initial)) configuration),
          F.envelopeFor 52 (reportHeraldFailureProbeCommand probe configuration ProbeUnreachable),
          oracleEnvelope (oracleClientRequestId F.fixtureThirdHeraldEpoch 53) Nothing F.fixtureThirdHeraldEpoch (reportHeraldFailureProbeCommand probe configuration ProbeUnreachable),
          F.envelopeFor 54 (acceptVoterHostFailureCommand resolution)
        ]
  (preparing, commandEntries) <- foldM applyCommand (initial, []) commands
  let change = fromJust (oraclePendingVoterChange preparing)
      ident = voterChangeId change
      (joint, jointMetadata) = checked (prepareOracleConfiguration ident JointConfigurationStage preparing)
      throughJoint = fmap (R.Application . canonicalOracleEnvelopeBytes . canonicalizeOracleEnvelope) commands <> [R.Configuration joint jointMetadata]
      (jointState, jointEntry) = checked (applyCommittedOracleConfiguration (lastFixture (sealedPayloads throughJoint)) preparing)
      (final, finalMetadata) = checked (prepareOracleConfiguration ident FinalConfigurationStage jointState)
      (excluded, finalEntry) = checked (applyCommittedOracleConfiguration (lastFixture (sealedPayloads (throughJoint <> [R.Configuration final finalMetadata]))) jointState)
  voterChangePhase (fromJust (oraclePendingVoterChange excluded)) @?= VoterExcludedAwaitingHeraldRetirement
  (completed, fullEntries) <- applyCommand (excluded, commandEntries <> [jointEntry, finalEntry]) (F.envelopeFor 55 (retireHeraldEpochCommand resolution F.fixtureRemoteHeraldEpoch))
  oraclePendingVoterChange completed @?= Nothing
  let batch = checked (committedOracleEntriesDtoFromValues (controlIndex 0) fullEntries)
      message = Wire.OracleServerEnvelope (Wire.CommittedOracleEntries batch)
  decodeOracleEnvelope (encodeOracleEnvelope message) @?= Right message
  fmap NE.toList (committedOracleEntriesDtoValues batch) @?= Right fullEntries
  let acceptedEntries = [certificate | entry <- fullEntries, event <- appliedEntryProjectionEvents entry, OracleVoterHostFailureAcceptedView certificate <- [oracleProjectionEventView event]]
  length acceptedEntries @?= 1
  mapM_ (\certificate -> decodeAcceptedVoterHostFailure (encodeAcceptedVoterHostFailure certificate) @?= Right certificate) acceptedEntries
  -- Terminal re-observation carries the original certificate after completion.
  _ <- applyCommand (completed, fullEntries) (F.envelopeFor 56 (acceptVoterHostFailureCommand resolution))
  pure ()
  where
    applyCommand (state, retained) envelope = do
      let commandDto = canonicalOracleEnvelopeDtoFromValue envelope
          message = Wire.OracleClientEnvelope (Wire.SubmitOracleCommand commandDto)
      decodeOracleEnvelope (encodeOracleEnvelope message) @?= Right message
      canonicalOracleEnvelopeDtoValue commandDto @?= Right envelope
      let (next, outcome, effects) = checked (stepOracle envelope state)
          projected = [entry | EmitAppliedOracleEntry entry <- oracleEffects effects]
      case outcome of
        OracleCommitted receipt -> do
          Receipt.oracleReceiptResult receipt @?= Receipt.OracleAccepted
          canonicalOracleReceiptDtoValue (canonicalOracleReceiptDtoFromValue receipt) @?= Right receipt
        other -> assertFailure (show other)
      mapM_ (\entry -> canonicalAppliedOracleEntryDtoValue (canonicalAppliedOracleEntryDtoFromValue entry) @?= Right entry) projected
      pure (next, retained <> projected)

sealedPayloads :: [R.RaftEntry BS.ByteString] -> [RE.RaftCommittedEntry BS.ByteString]
sealedPayloads payloads =
  let native = checkedOracleRaftNativeConfiguration F.fixtureCheckedGenesis
      leader = firstFixture F.fixtureRaftNodes
      follower = F.fixtureRaftNodes !! 1
      genesis = checked (RG.checkRaftGenesis (RG.raftGenesis follower F.fixtureRaftNodes (RG.raftNativeHeartbeatInterval native) (RG.raftNativeElectionTimeoutLower native) (RG.raftNativeElectionTimeoutUpper native)))
      logEntries = zipWith (\index payload -> R.raftLogEntry (RI.raftLogIndex index) (RI.raftTerm 1) payload) [1 ..] payloads
      rpc = checked (R.appendEntries (RI.raftTerm 1) leader (RI.raftLogIndex 0) (RI.raftTerm 0) logEntries (RI.raftLogIndex (fromIntegral (length payloads))))
      (_, batch) = checked (RT.stepRaft (checked (R.observeRaftRequest leader rpc)) (RT.initialRaft genesis))
   in [entry | RE.ExposeCommittedEntries sealed <- RE.raftEffectBatchEffects batch, entry <- NE.toList sealed]
