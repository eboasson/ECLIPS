{-# LANGUAGE OverloadedStrings #-}

module ProjectionBaseAdmissionProperties (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromJust)
import Data.Serialize qualified as Serialize
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd), processEndReasonCanonicalBytes)
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Domain.Startup (HeraldMember (heraldMemberEpoch))
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.Topology
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label qualified as Oracle
import Eclips.Oracle.Receipt (OracleReceiptResult (OracleAccepted), oracleReceiptResult)
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures qualified as Fixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "portable Projection identity admission"
    [ QC.testProperty "every generated mixed history prefix round-trips independently" propMixedPrefixes,
      testCase "retirement retains earlier ordinary Ends and closes exactly the surviving resident set" caseRetirementClosure,
      testCase "canonical identity payload rejects duplicate, reordered and malformed facts" caseCanonicalShapes,
      testCase "decoded identity facts bind receiver genesis and initial process selection" caseGenesisBinding,
      testCase "independent process admission rejects impossible lifetime coordinates" caseProcessLifetimes,
      testCase "admission facts and membership transitions must describe the same history" caseAdmissionHistory
    ]

type RawIdentityBase =
  ( ByteString,
    (ByteString, ByteString, ByteString, ByteString),
    Word64,
    ByteString,
    [ByteString],
    [(ByteString, ByteString, ByteString, Word64, Word64)],
    [(ByteString, Word64, ByteString)]
  )

type History = (Oracle.OracleState, [CanonicalAppliedOracleEntry])

local :: HeraldEpoch
local = heraldMemberEpoch Fixtures.fixtureLocalMember

remote :: HeraldEpoch
remote = heraldMemberEpoch Fixtures.fixtureRemoteMember

applicant :: HeraldEpoch
applicant = epoch 181

epoch :: Word8 -> HeraldEpoch
epoch = checked "Herald epoch" . mkHeraldEpoch . Fixtures.fixtureIdentifierBytes

processEpoch :: Word8 -> ProcessEpochId
processEpoch = checked "process epoch" . mkProcessEpochId . Fixtures.fixtureIdentifierBytes

processIdentity :: Word8 -> ProcessId
processIdentity = checked "process identity" . mkProcessId . Fixtures.fixtureIdentifierBytes

initialHistory :: History
initialHistory = (Oracle.initialOracleState Fixtures.fixtureCheckedOracleGenesis, [])

submit :: HeraldEpoch -> Oracle.OracleCommand -> History -> History
submit home command (oracle, entries) = case Oracle.oracleSubmissionOutcomeView outcome of
  Oracle.OracleSubmissionCommittedView receipt entry
    | oracleReceiptResult receipt == OracleAccepted -> (next, entries <> [canonicalizeAppliedOracleEntry entry])
  other -> error ("identity fixture command did not succeed: " <> show other)
  where
    request = oracleClientRequestId home (1 + controlIndexWord64 (Oracle.oracleGreatestControlIndex oracle))
    (next, outcome) = Oracle.submitOracleState (Oracle.oracleEnvelope request Nothing home command) oracle

start :: Word8 -> HeraldEpoch -> History -> History
start identifier home = submit home (Oracle.startProcessEpochCommand (processStart (processIdentity identifier) (processEpoch identifier) home))

end :: Word8 -> HeraldEpoch -> History -> History
end identifier home = submit home (checked "ordinary End" (Oracle.endProcessEpochCommand (processEpoch identifier) ExplicitAdministrativeEnd))

cutFor :: Membership.HeraldMembershipGeneration -> ControlIndex -> TopologyCut
cutFor membership at = checked "admission anchor" (topologyCut (sameGenerationPredecessor cutId) (topologyFrontier (emptyStructuralVersionVector membership) at) contribution)
  where
    cutId = checked "anchor identity" (mkTopologyCutId (Fixtures.fixtureIdentifierBytes 230))

contribution :: TopologyOccurrenceDigest
contribution = checked "contribution" (mkTopologyOccurrenceDigest (Fixtures.fixtureIdentifierBytes 231))

begin :: Word8 -> History -> History
begin identifier history@(oracle, _) = submit local (Oracle.beginHeraldAdmissionCommand manifest anchor) history
  where
    manifest = Admission.heraldAdmissionManifest (Genesis.checkedSystemId Fixtures.fixtureStep14CheckedGenesis) (checked "Herald ID" (mkHeraldId (Fixtures.fixtureIdentifierBytes identifier))) (epoch identifier)
    anchor = cutFor (Oracle.oracleCurrentMembership oracle) (Oracle.oracleGreatestControlIndex oracle)

pending :: History -> Admission.HeraldAdmissionRecord
pending = fromJust . Oracle.oraclePendingHeraldAdmission . fst

activate :: History -> History
activate history@(oracle, _) = submit local (Oracle.activateHeraldCommand ident) ready
  where
    record = pending history
    ident = Admission.admissionRecordId record
    members = NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs (Admission.admissionRecordPredecessor record))
    at = Oracle.oracleGreatestControlIndex oracle
    cut = cutFor (Admission.admissionRecordPredecessor record) at
    digest = checked "join digest" (Admission.mkHeraldJoinDigest (topologyOccurrenceDigestBytes contribution))
    recipe = checked "join recipe" (heraldJoinBaseRecipe ident (Admission.admissionManifestHeraldEpoch (Admission.admissionRecordManifest record)) (Admission.admissionRecordPredecessor record) cut contribution)
    recipeDigest = checked "recipe digest" (Admission.mkHeraldJoinDigest (heraldJoinBaseRecipeDigestBytes recipe))
    seal = checked "join seal" (Admission.heraldJoinSeal ident (Admission.admissionRecordAttempt record) cut at [(member, Admission.heraldJoinMemberCut 0 0 digest) | member <- members] digest recipeDigest)
    sealed = submit (NonEmpty.head (Membership.heraldMembershipGenerationActiveHeraldEpochs (Admission.admissionRecordPredecessor record))) (Oracle.sealHeraldAdmissionCommand seal) history
    accepted = foldl' (\state member -> submit member (Oracle.acceptHeraldJoinSealCommand ident (Admission.joinSealAttempt seal) (Admission.joinSealDigest seal)) state) sealed members
    report member = Admission.heraldJoinReadyReport ident (Admission.joinSealAttempt seal) member (Admission.joinSealDigest seal) at recipeDigest
    oldReady = foldl' (\state member -> submit member (Oracle.reportHeraldJoinBaseReadyCommand (report member)) state) accepted members
    ready = submit local (Oracle.reportHeraldJoinReadyCommand (report (Admission.admissionManifestHeraldEpoch (Admission.admissionRecordManifest record)))) oldReady

cancel :: History -> History
cancel history = submit local (Oracle.cancelHeraldAdmissionCommand (Admission.admissionRecordId (pending history))) history

-- A newcomer is a non-voter, so ordinary majority failure evidence retires it
-- without a separate Raft voter-removal fixture.
retireApplicant :: History -> History
retireApplicant history@(oracle, _) = submit local (Oracle.retireHeraldEpochCommand resolution applicant) reported
  where
    configuration = Voter.voterConfigurationId (Voter.oracleVoterConfiguration oracle)
    generation = Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership oracle)
    opened = submit local (Oracle.openHeraldFailureProbeCommand applicant generation configuration) history
    probe = checked "failure probe" (Membership.deriveHeraldFailureProbeId (Oracle.oracleGreatestControlIndex (fst opened)))
    reported = foldl' (\state home -> submit home (Oracle.reportHeraldFailureProbeCommand probe configuration Oracle.ProbeUnreachable) state) opened [local, remote]
    resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget

mixedHistory :: Int -> History
mixedHistory extra = begin 184 (retireApplicant waiting)
  where
    ordinary = foldl' (\state number -> end number local (start number local state)) initialHistory (take extra [200 ..])
    invalidated = end 190 local (start 191 local (begin 181 (start 190 local ordinary)))
    admitted = activate invalidated
    residents = end 192 applicant (start 193 applicant (start 192 applicant admitted))
    cancelled = cancel (begin 182 residents)
    waiting = begin 183 cancelled

projectionPrefixes :: History -> [Projection.State]
projectionPrefixes (_, entries) = scanl apply Fixtures.fixtureOracleProjectionState entries
  where
    apply state entry = Projection.commitAppliedEntry (checked "canonical Projection prefix" (Projection.prepareAppliedEntry entry state))

base :: Projection.State -> Projection.ProjectionIdentityBase
base = checked "capture identity facts" . Projection.captureProjectionIdentityBase

bytesFor :: Projection.State -> ByteString
bytesFor = Projection.encodeProjectionIdentityBase . base

decode :: ByteString -> Either Projection.ProjectionIdentityBaseProblem Projection.ProjectionIdentityBase
decode = Projection.decodeProjectionIdentityBase Fixtures.fixtureStep14CheckedGenesis Fixtures.fixtureCheckedInitialBootstraps

propMixedPrefixes :: QC.NonNegative Int -> QC.Property
propMixedPrefixes (QC.NonNegative seed) =
  QC.conjoin
    [ QC.counterexample ("prefix " <> show ordinal) (decode (bytesFor state) QC.=== Right (base state))
    | (ordinal, state) <- zip [(0 :: Int) ..] (projectionPrefixes (mixedHistory (seed `mod` 5)))
    ]

caseRetirementClosure :: Assertion
caseRetirementClosure = do
  let state = last (projectionPrefixes (mixedHistory 0))
      ends = Projection.projectedEndedProcesses state
      ended identifier = fromJust (lookup (processEpoch identifier) ends)
      ordinary = ended 192
      retired = ended 193
  assertEqual "the earlier explicit End is retained" ExplicitAdministrativeEnd (Projection.projectedEndedProcessReason ordinary)
  assertEqual "the surviving newcomer resident gets the retirement End" Fixtures.fixtureHeraldRetirementReason (Projection.projectedEndedProcessReason retired)
  assertBool "ordinary End precedes retirement" (Projection.projectedEndedProcessControlIndex ordinary < Projection.projectedEndedProcessControlIndex retired)
  assertEqual "independent admission keeps both End lifetimes" (Right (base state)) (decode (bytesFor state))
  let raw = rawFor state
      removeResident (tag, identity, at, history, admissions, starts, endings) = (tag, identity, at, history, admissions, starts, filter (\(who, _, _) -> who /= processEpochIdBytes (processEpoch 193)) endings)
  rejects "retirement must End every still-live resident" (removeResident raw)

rawFor :: Projection.State -> RawIdentityBase
rawFor = checked "decode nominal fixture tuple" . Serialize.decode . bytesFor

rejects :: String -> RawIdentityBase -> Assertion
rejects label = assertBool label . isLeft . decode . Serialize.encode

caseCanonicalShapes :: Assertion
caseCanonicalShapes = do
  let state = last (projectionPrefixes (mixedHistory 0))
      raw@(tag, identity, at, history, admissions, starts, endings) = rawFor state
  rejects "wrong domain" ("wrong-domain", identity, at, history, admissions, starts, endings)
  rejects "duplicate start" (tag, identity, at, history, admissions, starts <> take 1 starts, endings)
  rejects "reordered start map" (tag, identity, at, history, admissions, reverse starts, endings)
  rejects "duplicate End" (tag, identity, at, history, admissions, starts, endings <> take 1 endings)
  rejects "reordered admissions" (tag, identity, at, history, reverse admissions, starts, endings)
  rejects "malformed membership frame" (tag, identity, at, "bad-history", admissions, starts, endings)
  assertBool "trailing data rejected" (isLeft (decode (Serialize.encode raw <> "x")))
  assertBool "truncated data rejected" (isLeft (decode (Serialize.encode (tag, identity))))

caseGenesisBinding :: Assertion
caseGenesisBinding = do
  let state = last (projectionPrefixes (mixedHistory 0))
      bytes = bytesFor state
      (tag, (system, catalogue, configuration, initial), at, history, admissions, starts, endings) = rawFor state
      emptyBootstraps = checked "different initial process selection" (Genesis.checkInitialBootstraps Fixtures.fixtureStep14CheckedGenesis (Genesis.PrimordialProcessManifest []))
  rejects "foreign system" (tag, (Fixtures.fixtureIdentifierBytes 241, catalogue, configuration, initial), at, history, admissions, starts, endings)
  rejects "foreign configuration" (tag, (system, catalogue, Fixtures.fixtureIdentifierBytes 242, initial), at, history, admissions, starts, endings)
  assertBool "receiver with other genesis membership rejects" (isLeft (Projection.decodeProjectionIdentityBase Fixtures.fixtureCheckedGenesis (Fixtures.fixtureCheckedInitialBootstrapsFor Fixtures.fixtureCheckedGenesis) bytes))
  assertBool "receiver with other initial processes rejects" (isLeft (Projection.decodeProjectionIdentityBase Fixtures.fixtureStep14CheckedGenesis emptyBootstraps bytes))

caseProcessLifetimes :: Assertion
caseProcessLifetimes = do
  let state = last (projectionPrefixes (mixedHistory 0))
      (tag, identity, at, history, admissions, starts, endings) = rawFor state
      firstStart = case starts of
        first : _ -> first
        [] -> error "fixture has no dynamic Starts"
      (who, process, home, born, sequenceNumber) = firstStart
      retirement = controlIndexWord64 (Projection.projectedEndedProcessControlIndex (fromJust (lookup (processEpoch 193) (Projection.projectedEndedProcesses state))))
      updateStart identifier update = map (\row@(subject, _, _, _, _) -> if subject == processEpochIdBytes (processEpoch identifier) then update row else row) starts
      updateEnd identifier update = map (\row@(subject, _, _) -> if subject == processEpochIdBytes (processEpoch identifier) then update row else row) endings
      withStarts rows = (tag, identity, at, history, admissions, rows, endings)
      withEnds rows = (tag, identity, at, history, admissions, starts, rows)
      activation = case [controlIndexWord64 index | record <- Projection.projectedHeraldAdmissions state, Admission.AdmissionActivated index _ <- [Admission.admissionRecordPhase record]] of
        first : _ -> first
        [] -> error "fixture has no activation"
  rejects "Start beyond cut" (withStarts ((who, process, home, at + 1, sequenceNumber) : drop 1 starts))
  rejects "duplicate process identity" (withStarts (firstStart : [(other, process, resident, index, request) | (other, _, resident, index, request) <- drop 1 starts]))
  rejects "two Starts cannot share one control index" (withStarts (firstStart : [(other, identifier, resident, born, request) | (other, identifier, resident, _, request) <- drop 1 starts]))
  rejects "one Oracle request cannot start two processes" (withStarts (updateStart 191 (\(subject, identifier, _, index, _) -> (subject, identifier, home, index, sequenceNumber))))
  rejects "Start cannot share admission activation coordinate" (withStarts (updateStart 192 (\(subject, identifier, resident, _, request) -> (subject, identifier, resident, activation, request))))
  rejects "unknown End" (withEnds (endings <> [(Fixtures.fixtureIdentifierBytes 243, at, processEndReasonCanonicalBytes ExplicitAdministrativeEnd)]))
  rejects "End at Start coordinate" (withEnds ((who, born, processEndReasonCanonicalBytes ExplicitAdministrativeEnd) : filter (\(subject, _, _) -> subject /= who) endings))
  rejects "Start at home retirement" (withStarts (updateStart 193 (\(subject, identifier, resident, _, request) -> (subject, identifier, resident, retirement, request))))
  rejects "Start on an unactivated home" (withStarts ((who, process, heraldEpochBytes (epoch 184), born, sequenceNumber) : drop 1 starts))
  rejects "ordinary End at retirement coordinate" (withEnds (updateEnd 193 (\(subject, index, _) -> (subject, index, processEndReasonCanonicalBytes ExplicitAdministrativeEnd))))
  rejects "retirement End before retirement" (withEnds (updateEnd 193 (\(subject, index, reason) -> (subject, index - 1, reason))))

caseAdmissionHistory :: Assertion
caseAdmissionHistory = do
  let states = projectionPrefixes (mixedHistory 0)
      final = last states
      (tag, identity, at, history, admissions, starts, endings) = rawFor final
      currentRecords = Projection.projectedHeraldAdmissions final
      activated = [Admission.encodeAdmissionRecord record | record <- currentRecords, Admission.AdmissionActivated {} <- [Admission.admissionRecordPhase record]]
      oldPreparing = [record | state <- states, record <- Projection.projectedHeraldAdmissions state, Admission.admissionManifestHeraldEpoch (Admission.admissionRecordManifest record) == applicant, Admission.admissionRecordPhase record == Admission.AdmissionPreparing]
      replaceActivated record = [if encoded `elem` activated then Admission.encodeAdmissionRecord record else encoded | encoded <- admissions]
  assertEqual "fixture has an activated, cancelled, preempted and pending admission" 4 (length admissions)
  assertBool "fixture preserves a genuine invalidated attempt" (any ((> 1) . Admission.admissionRecordAttempt) currentRecords)
  rejects "activation history requires its admission record" (tag, identity, at, history, filter (`notElem` activated) admissions, starts, endings)
  forM_ (take 1 oldPreparing) $ \record ->
    rejects "old preparing record cannot authorize recorded activation" (tag, identity, at, history, replaceActivated record, starts, endings)
  rejects "admission changes cannot lie beyond base cut" (tag, identity, 0, history, admissions, starts, endings)

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
