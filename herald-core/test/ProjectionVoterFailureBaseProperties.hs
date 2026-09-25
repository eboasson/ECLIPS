{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectionVoterFailureBaseProperties (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Serialize qualified as Codec
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity (HeraldEpoch, controlIndex, heraldEpochBytes, mkHeraldEpoch)
import Eclips.Domain.Membership qualified as Membership
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Command
import Eclips.Oracle.Effect
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures qualified as Fixtures
import NativeVoterFailureFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "portable Projection voter and failure admission"
    [ QC.testProperty "real native failure prefixes preserve pending phases and old captured configurations" (QC.withNumTests 20 propNativePrefixes),
      testCase "optional voter initialization remains supported" caseOptionalVoters,
      testCase "registration and cancelled intentions retain a newer pending slot" caseExplicitCancellation,
      testCase "dismissed and superseded probes preserve their original denominator" caseDismissedAndSuperseded,
      testCase "a cancelled voter intention leaves probes superseded under the unchanged configuration" caseCancelledSupersession,
      testCase "canonical voter and failure bytes reject duplicate, reordered and malformed rows" caseCanonical,
      testCase "independent admission checks current voter and failure closure" caseClosure,
      testCase "covered snapshots keep semantic frontiers at retained voter and failure facts" caseCoveredSemanticCoordinates
    ]

type RawProbe =
  (Word64, ByteString, ByteString, ByteString, [(ByteString, Word8)], Maybe (Word8, ByteString, Maybe ByteString))

type RawBase = (ByteString, Maybe ByteString, [ByteString], [ByteString], [RawProbe])

initialOracleState :: OracleState
initialOracleState = checked "initial Oracle" (initialOracle Fixtures.fixtureCheckedOracleGenesis)

initialProjection :: Projection.State
initialProjection = checked "initialize voter projection" (Projection.initializeOracleVoters (Voter.oracleVoterConfiguration initialOracleState) (Voter.oracleReplicaRegistrations initialOracleState) Fixtures.fixtureOracleProjectionState)

bindings :: [RaftVoterBinding]
bindings = checkedOracleRaftVoterBindings Fixtures.fixtureCheckedOracleGenesis

home, target :: HeraldEpoch
home = raftVoterBindingHeraldEpoch (fixtureFirst bindings)
target = raftVoterBindingHeraldEpoch (last bindings)

envelope :: Word64 -> HeraldEpoch -> OracleCommand -> OracleEnvelope
envelope ordinal reporter = oracleEnvelope (oracleClientRequestId reporter ordinal) Nothing reporter

registration :: Word64 -> OracleEnvelope
registration ordinal = envelope ordinal home (Voter.registerOracleReplicaCommand (raftVoterBindingNode (fixtureFirst bindings)) home contact)
  where
    endpoint = checked "replica endpoint" (Voter.oracleReplicaEndpoint "127.0.0.1" 14001)
    contact = Voter.oracleReplicaContact endpoint endpoint endpoint

command :: OracleEnvelope -> OracleState -> (OracleState, CanonicalAppliedOracleEntry)
command input predecessor = case checked "commit command" (stepOracle input predecessor) of
  (successor, OracleCommitted receipt, effects)
    | oracleReceiptResult receipt == OracleAccepted,
      [entry] <- [entry | EmitAppliedOracleEntry entry <- oracleEffects effects] ->
        (successor, canonicalizeAppliedOracleEntry entry)
  other -> error ("expected accepted canonical command: " <> show other)

project :: Projection.State -> CanonicalAppliedOracleEntry -> Projection.State
project state entry = Projection.commitAppliedEntry (checked "project canonical entry" (Projection.prepareAppliedEntry entry state))

failureHistory :: [OracleEnvelope] -> [(OracleState, CanonicalAppliedOracleEntry)]
failureHistory prefix =
  let fixture = nativeVoterFailureTranscript Fixtures.fixtureCheckedOracleGenesis prefix target
   in nativeFailurePrelude fixture <> nativeFailureEvidence fixture <> [nativeFailureJoint fixture, nativeFailureExcluded fixture, nativeFailureRetired fixture]

roundTrip :: Projection.State -> Assertion
roundTrip = roundTripWith initialProjection

roundTripWith :: Projection.State -> Projection.State -> Assertion
roundTripWith receiver state = do
  base <- either (error . show) pure (Projection.captureProjectionVoterFailureBase state)
  let bytes = Projection.encodeProjectionVoterFailureBase base
  assertEqual "nominal voter/failure facts roundtrip" (Right base) (Projection.decodeProjectionVoterFailureBase state bytes)
  let aggregate = checked "capture aggregate voter/failure base" (Projection.captureProjectionBase state)
  assertBool "full aggregate roundtrips against fresh receiver facts" (Projection.decodeProjectionBase receiver (Projection.encodeProjectionBase aggregate) == Right aggregate)
  assertEqual "decoder does not change source projection" (Right (Projection.stateWitness state)) (Projection.validateState state)

propNativePrefixes :: QC.NonNegative Int -> QC.Property
propNativePrefixes (QC.NonNegative count) = QC.ioProperty $ do
  let prefix = map registration [1001 .. 1000 + fromIntegral (count `mod` 4)]
      history = failureHistory prefix
      projected = scanl project initialProjection (map snd history)
  mapM_ roundTrip projected
  let (retiredOracle, _) = last history
      retiredProjection = last projected
      next = envelope 10000001 home (Voter.beginVoterChangeCommand (Voter.voterConfigurationId (Voter.oracleVoterConfiguration retiredOracle)) Voter.ExplicitDemotion (checked "one remaining voter" (Voter.oracleVoterBindings (take 1 bindings))))
      (preparing, began) = command next retiredOracle
      after = project retiredProjection began
      oldProbe = checked "native probe" (Membership.deriveHeraldFailureProbeId (controlIndex (fromIntegral (length prefix + 1))))
  roundTrip after
  assertEqual "new intent survives while old failure remains terminal" (Voter.oraclePendingVoterChange preparing) (Projection.oracleViewPendingVoterChange (Projection.oracleView after))
  let terminal = Projection.projectedFailureProbeTerminal =<< Projection.oracleViewFailureProbe oldProbe (Projection.oracleView after)
  assertBool "accepted old configuration remains retained after retirement" (case terminal of Just Projection.ProjectedVoterHostFailureAccepted {} -> True; _ -> False)
  pure True

caseOptionalVoters :: Assertion
caseOptionalVoters = do
  roundTripWith Fixtures.fixtureOracleProjectionState Fixtures.fixtureOracleProjectionState
  let (_, entry) = command (registration 1) initialOracleState
      observed = project Fixtures.fixtureOracleProjectionState entry
  assertEqual "registration does not initialize optional voter authority" Nothing (Projection.oracleViewVoterConfiguration (Projection.oracleView observed))
  roundTripWith Fixtures.fixtureOracleProjectionState observed

caseExplicitCancellation :: Assertion
caseExplicitCancellation = do
  let first = command (registration 1) initialOracleState
      second = command (registration 2) (fst first)
      begin ordinal state = envelope ordinal home (Voter.beginVoterChangeCommand (Voter.voterConfigurationId (Voter.oracleVoterConfiguration state)) Voter.ExplicitDemotion (checked "desired voters" (Voter.oracleVoterBindings (take 2 bindings))))
      third = command (begin 3 (fst second)) (fst second)
      identifier = Voter.voterChangeId (fromJust (Voter.oraclePendingVoterChange (fst third)))
      fourth = command (envelope 4 home (Voter.cancelVoterChangeCommand identifier)) (fst third)
      fifth = command (begin 5 (fst fourth)) (fst fourth)
      sixth = command (envelope 6 home (Voter.cancelVoterChangeCommand identifier)) (fst fifth)
      projected = scanl project initialProjection (map snd [first, second, third, fourth, fifth, sixth])
  mapM_ roundTrip projected
  assertEqual "old cancelled retry preserves current pending identity" (Voter.oraclePendingVoterChange (fst fifth)) (Projection.oracleViewPendingVoterChange (Projection.oracleView (last projected)))

caseDismissedAndSuperseded :: Assertion
caseDismissedAndSuperseded = do
  let configuration = Voter.oracleVoterConfiguration initialOracleState
      generation = Membership.heraldMembershipGenerationId (oracleCurrentMembership initialOracleState)
      firstProbe = checked "dismissed probe" (Membership.deriveHeraldFailureProbeId (controlIndex 1))
      reporters = take 2 (map raftVoterBindingHeraldEpoch bindings)
      prefix =
        [envelope 1 home (openHeraldFailureProbeCommand home generation (Voter.voterConfigurationId configuration))]
          <> [envelope ordinal reporter (reportHeraldFailureProbeCommand firstProbe (Voter.voterConfigurationId configuration) ProbeReachable) | (ordinal, reporter) <- zip [2 ..] reporters]
          <> [ envelope 4 home (dismissHeraldFailureProbeCommand (Membership.deriveFailureProbeResolutionId firstProbe Membership.DismissFailureProbe)),
               envelope 5 home (openHeraldFailureProbeCommand home generation (Voter.voterConfigurationId configuration))
             ]
      projected = scanl project initialProjection (map snd (failureHistory prefix))
      final = last projected
      secondProbe = checked "superseded probe" (Membership.deriveHeraldFailureProbeId (controlIndex 5))
      terminal identifier = Projection.projectedFailureProbeTerminal =<< Projection.oracleViewFailureProbe identifier (Projection.oracleView final)
  mapM_ roundTrip projected
  assertBool "dismissal is retained" (case terminal firstProbe of Just Projection.ProjectedFailureProbeDismissed {} -> True; _ -> False)
  assertBool "native voter change supersedes the other original probe" (case terminal secondProbe of Just Projection.ProjectedFailureProbeSuperseded {} -> True; _ -> False)

caseCancelledSupersession :: Assertion
caseCancelledSupersession = do
  let configuration = Voter.oracleVoterConfiguration initialOracleState
      generation = Membership.heraldMembershipGenerationId (oracleCurrentMembership initialOracleState)
      probe = checked "cancelled-intention probe" (Membership.deriveHeraldFailureProbeId (controlIndex 1))
      opened = command (envelope 1 home (openHeraldFailureProbeCommand target generation (Voter.voterConfigurationId configuration))) initialOracleState
      desired = checked "cancelled desired voters" (Voter.oracleVoterBindings (take 2 bindings))
      begun = command (envelope 2 home (Voter.beginVoterChangeCommand (Voter.voterConfigurationId configuration) Voter.ExplicitDemotion desired)) (fst opened)
      intention = Voter.voterChangeId (fromJust (Voter.oraclePendingVoterChange (fst begun)))
      cancelled = command (envelope 3 home (Voter.cancelVoterChangeCommand intention)) (fst begun)
      projected = scanl project initialProjection (map snd [opened, begun, cancelled])
      afterBegin = projected !! 2
      afterCancel = last projected
      terminal state = Projection.projectedFailureProbeTerminal =<< Projection.oracleViewFailureProbe probe (Projection.oracleView state)
  assertEqual "Begin supersedes before configuration changes" (Just (Projection.ProjectedFailureProbeSuperseded generation)) (terminal afterBegin)
  assertEqual "Cancel cannot reopen the probe" (terminal afterBegin) (terminal afterCancel)
  assertEqual "the captured configuration remains current" (Just configuration) (Projection.oracleViewVoterConfiguration (Projection.oracleView afterCancel))
  assertEqual "Cancel releases the pending slot" Nothing (Projection.oracleViewPendingVoterChange (Projection.oracleView afterCancel))
  mapM_ roundTrip projected
  let (domain, current, registrations, _, probes) = rawBase afterCancel
  assertBool
    "same-generation supersession keeps its retained initiation witness"
    (isLeft (Projection.decodeProjectionVoterFailureBase afterCancel (Codec.encode (domain, current, registrations, [] :: [ByteString], probes))))

source :: Projection.State
source = foldl' project initialProjection (map snd (failureHistory []))

rawBase :: Projection.State -> RawBase
rawBase = checked "read voter/failure transcript" . Codec.decode . Projection.encodeProjectionVoterFailureBase . checked "capture voter/failure" . Projection.captureProjectionVoterFailureBase

caseCanonical :: Assertion
caseCanonical = do
  let (domain, configuration, registrations, changes, probes) = rawBase source
      bytes = Codec.encode (domain, configuration, registrations, changes, probes)
      reject detail changed = assertBool detail (isLeft (Projection.decodeProjectionVoterFailureBase source (Codec.encode (changed :: RawBase))))
  assertBool "trailing bytes are rejected" (isLeft (Projection.decodeProjectionVoterFailureBase source (ByteString.snoc bytes 0)))
  reject "domain is exact" ("wrong", configuration, registrations, changes, probes)
  reject "registration order is canonical" (domain, configuration, reverse registrations, changes, probes)
  reject "registration duplicates are rejected" (domain, configuration, fixtureFirst registrations : registrations, changes, probes)
  reject "failure duplicates are rejected" (domain, configuration, registrations, changes, fixtureFirst probes : probes)
  let (opened, probeTarget, generation, captured, reports, terminal) = fixtureFirst probes
  reject "unknown report tags are rejected" (domain, configuration, registrations, changes, [(opened, probeTarget, generation, captured, [(fst (fixtureFirst reports), 2)], terminal)])

caseClosure :: Assertion
caseClosure = do
  let (domain, configuration, registrations, changes, probes) = rawBase source
      reject detail changed = assertBool detail (isLeft (Projection.decodeProjectionVoterFailureBase source (Codec.encode (changed :: RawBase))))
      (opened, probeTarget, generation, captured, reports, terminal) = fixtureFirst probes
      foreignHost = checked "foreign reporter" (mkHeraldEpoch (Fixtures.fixtureIdentifierBytes 0xf3))
  reject "accepted certificate requires its automatic voter intention" (domain, configuration, registrations, [], probes)
  reject "genesis replica cannot disappear" (domain, configuration, drop 1 registrations, changes, probes)
  reject "reports use captured voter hosts" (domain, configuration, registrations, changes, [(opened, probeTarget, generation, captured, sortOn fst ((heraldEpochBytes foreignHost, 1) : reports), terminal)])
  reject "retired probe cannot be reopened under old membership" (domain, configuration, registrations, changes, [(opened, probeTarget, generation, captured, reports, Nothing)])
  assertBool "semantic cut bounds remain independent of leaf bytes" (isLeft (Projection.decodeProjectionVoterFailureBase initialProjection (Codec.encode (rawBase source))))
  let fixture = nativeVoterFailureTranscript Fixtures.fixtureCheckedOracleGenesis [] target
      joint = foldl' project initialProjection (map snd (nativeFailureEvidence fixture <> [nativeFailureJoint fixture]))
      (jointDomain, _, jointRegistrations, jointChanges, jointProbes) = rawBase joint
      wrongCurrent = Just (Voter.encodeVoterConfiguration (Voter.oracleVoterConfiguration initialOracleState))
  assertBool "joint pending phase cannot claim the previous stable configuration" (isLeft (Projection.decodeProjectionVoterFailureBase joint (Codec.encode (jointDomain, wrongCurrent, jointRegistrations, jointChanges, jointProbes))))

type RawAggregate =
  ( ByteString,
    (ByteString, Maybe ByteString, [ByteString]),
    (ByteString, ByteString, ByteString),
    (Word64, Maybe ByteString),
    (Word64, [(Word64, ByteString)], [(Word64, ByteString, [(ByteString, Word64, ByteString)])])
  )

caseCoveredSemanticCoordinates :: Assertion
caseCoveredSemanticCoordinates = mapM_ check states
  where
    fixture = nativeVoterFailureTranscript Fixtures.fixtureCheckedOracleGenesis [] target
    accepted = foldl' project initialProjection (map snd (nativeFailureEvidence fixture))
    joint = project accepted (snd (nativeFailureJoint fixture))
    excluded = project joint (snd (nativeFailureExcluded fixture))
    registered = project initialProjection (snd (command (registration 1) initialOracleState))
    states = [("registration", registered), ("accepted certificate and intention", accepted), ("joint configuration", joint), ("final exclusion configuration", excluded)]
    check (description, state) = do
      let based = checked "advance voter semantic base" (Projection.advanceReplayBase state)
          reclaimed = checked "reclaim voter evidence prefix" (Projection.reclaimAppliedPrefixThroughBase Set.empty Nothing based)
          base = checked "capture sparse voter aggregate" (Projection.captureProjectionBase reclaimed)
          bytes = Projection.encodeProjectionBase base
          (domain, identity, leaves, (semantic, digest), archive@(_, entries, _)) = checked "read sparse aggregate" (Codec.decode bytes :: Either String RawAggregate)
          behind = (domain, identity, leaves, (semantic - 1, digest), archive)
      assertBool (description <> ": canonical prefix was actually reclaimed") (null entries && semantic > 0)
      assertBool (description <> ": sparse aggregate remains independently admissible") (Projection.decodeProjectionBase initialProjection bytes == Right base)
      assertBool (description <> ": semantic frontier cannot precede retained current facts") (isLeft (Projection.decodeProjectionBase initialProjection (Codec.encode behind)))

fixtureFirst :: [value] -> value
fixtureFirst (value : _) = value
fixtureFirst [] = error "empty voter/failure fixture"

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
