{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectionDisappearanceBaseProperties (tests) where

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Serialize qualified as Serialize
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Application.Types.SortDescriptor qualified as Syntax
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Disappearance qualified as Domain
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Sort.Canonical (CanonicalDescriptor)
import Eclips.Domain.Sort.Profile (PredefinedSortRole (NeutralVertexRole))
import Eclips.Domain.SortOccurrence qualified as Occurrence
import Eclips.Domain.Startup (HeraldMember (heraldMemberEpoch))
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Herald.Application.SortDefinition (admitApplicationSortDefinition, admittedApplicationSortDescriptor)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Canonical (canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Disappearance qualified as Disappearance
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
    "portable Projection disappearance admission"
    [ QC.testProperty "canonical regular retirement histories round-trip at every generated prefix" propRegularPrefixes,
      testCase "all controlled terminals and membership abort survive nominal transfer" caseMixedTerminals,
      testCase "Begin aborts transfer before cancellation and bind their admission cause" caseAdmissionPreparation,
      testCase "decoded evidence does not depend on the context's old disappearance map" caseIndependentContext,
      testCase "nominal header admission derives its identity and coordinate" caseNominalHeader,
      testCase "canonical rows reject malformed shape, reordering, duplicate evidence and missing reports" caseMalformedRows,
      testCase "resolved regular outcomes bind their deterministic system-derived successor" caseOutcomeBinding,
      testCase "collecting probes agree with current labels, retirement occurrence and membership" caseCurrentClosure,
      testCase "sparse aggregate semantic frontier includes disappearance terminals" caseSparseTerminalFrontier
    ]

type RawProbe = (Word64, ByteString, ByteString, [ByteString], Maybe (Word8, Word64, [ByteString]))
type RawBase = (ByteString, [RawProbe])

type RawAggregate =
  ( ByteString,
    (ByteString, Maybe ByteString, [ByteString]),
    (ByteString, ByteString, ByteString),
    (Word64, Maybe ByteString),
    (Word64, [(Word64, ByteString)], [(Word64, ByteString, [(ByteString, Word64, ByteString)])])
  )

data Fixture = Fixture
  { oracle :: Oracle.OracleState,
    projection :: Projection.State,
    prefixes :: [Projection.State]
  }

initial :: Fixture
initial = Fixture (Oracle.initialOracleState Fixtures.fixtureCheckedOracleGenesis) Fixtures.fixtureOracleProjectionState [Fixtures.fixtureOracleProjectionState]

local :: Identity.HeraldEpoch
local = heraldMemberEpoch Fixtures.fixtureLocalMember

system :: Identity.SystemId
system = Genesis.checkedSystemId Fixtures.fixtureStep14CheckedGenesis

caller :: Identity.ProcessEpochId
caller = case [process | process <- Projection.projectedProcessEpochs Fixtures.fixtureOracleProjectionState, Projection.oracleViewProcessResidence process (Projection.oracleView Fixtures.fixtureOracleProjectionState) == Just local] of
  process : _ -> process
  [] -> error "disappearance fixture lacks local process"

object :: Identity.GlobalObjectId
object = checked "controlled object" (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 210))

controlled :: Maybe Label.LabelRevision -> Domain.DisappearanceSubject
controlled revision = checked "controlled subject" (Domain.controlledPredefinedDisappearanceSubject NeutralVertexRole object (Identity.structuralOccurrenceId local Identity.firstStructuralSequence) revision)

regularDescriptor :: CanonicalDescriptor
regularDescriptor = admittedApplicationSortDescriptor (checked "regular descriptor" (admitApplicationSortDefinition definition))
  where
    definition =
      Syntax.DeclaredSortDefinition
        Syntax.ApplicationSortDescriptor
          { Syntax.sortKind = Syntax.RegularSort,
            Syntax.valueSchema = Syntax.RecordSchema (Map.singleton "key" Syntax.BoolSchema),
            Syntax.keyProjections = [Syntax.ApplicationProjection ("key" NonEmpty.:| [])],
            Syntax.validityPredicate = Syntax.AlwaysPredicate,
            Syntax.obsolescencePredicate = Syntax.NeverPredicate,
            Syntax.rankTerms = Syntax.RankApplicationValue Syntax.Ascending NonEmpty.:| [],
            Syntax.minimumRetentionMicros = 0,
            Syntax.isImmutable = False,
            Syntax.labelField = Nothing
          }
        Nothing

regular :: Occurrence.SortOccurrenceBase -> Domain.DisappearanceSubject
regular occurrence = Domain.regularSortDefinitionDisappearanceSubject (Domain.deriveRegularSortOccurrenceClaim system regularDescriptor occurrence)

submit :: Identity.HeraldEpoch -> Oracle.OracleCommand -> Fixture -> Fixture
submit home command fixture = case Oracle.oracleSubmissionOutcomeView outcome of
  Oracle.OracleSubmissionCommittedView receipt entry
    | oracleReceiptResult receipt == OracleAccepted ->
        let successor = Projection.commitAppliedEntry (checked "project canonical disappearance control" (Projection.prepareAppliedEntry (canonicalizeAppliedOracleEntry entry) fixture.projection))
         in Fixture next successor (fixture.prefixes <> [successor])
  other -> error ("disappearance fixture command rejected: " <> show other)
  where
    request = oracleClientRequestId home (1 + Identity.controlIndexWord64 (Oracle.oracleGreatestControlIndex fixture.oracle))
    (next, outcome) = Oracle.submitOracleState (Oracle.oracleEnvelope request Nothing home command) fixture.oracle

open :: Domain.DisappearanceSubject -> Fixture -> (Fixture, Domain.DisappearanceProbeId)
open subject fixture = (opened, checked "opened probe" (Domain.deriveDisappearanceProbeId (Oracle.oracleGreatestControlIndex opened.oracle)))
  where
    generation = Oracle.oracleCurrentMembership fixture.oracle
    opened = submit local (Oracle.openDisappearanceProbeCommand subject (Domain.disappearanceSubjectMembershipCoordinate subject generation)) fixture

claimsFor :: Domain.DisappearanceSubject -> Domain.DisappearanceProbeId -> Fixture -> [Domain.DisappearanceEvidenceClaim]
claimsFor subject probe fixture =
  [ checked "captured report" (Domain.admitDisappearanceEvidenceClaim subject generation probe reporter (Domain.deriveDisappearanceEvidenceDigest "quiet Store witness"))
  | reporter <- NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs generation)
  ]
  where
    generation = Oracle.oracleCurrentMembership fixture.oracle

resolve :: Domain.DisappearanceSubject -> Domain.DisappearanceProbeId -> Fixture -> Fixture
resolve subject probe fixture = submit local (Oracle.resolveDisappearanceProbeCommand probe (Disappearance.completeDisappearanceEvidenceDigest reports)) reported
  where
    reports = claimsFor subject probe fixture
    reported = foldl' (\state report -> submit (Domain.disappearanceEvidenceClaimReporter report) (Oracle.reportPredefinedAbsenceCommand probe report) state) fixture reports

regularHistory :: Int -> Fixture
regularHistory rounds = fst (open (regular finalOccurrence) finished)
  where
    (finished, finalOccurrence) = foldl' step (initial, Occurrence.Genesis) [1 .. rounds]
    step (fixture, occurrence) _ =
      let subject = regular occurrence
          (opened, probe) = open subject fixture
          resolved = resolve subject probe opened
          nextOccurrence = checked "retired successor" (Occurrence.resolvedRetirementOccurrenceBase (Oracle.oracleGreatestControlIndex resolved.oracle))
       in (resolved, nextOccurrence)

labelControlled :: Fixture -> (Fixture, Identity.LabelDecisionId)
labelControlled fixture = (decided, decision)
  where
    request = oracleClientRequestId local (1 + Identity.controlIndexWord64 (Oracle.oracleGreatestControlIndex fixture.oracle))
    decision = Oracle.deriveLabelDecisionId system request
    acceptance = checked "label acceptance" (Label.mkLabelProcessAcceptancePosition caller 1)
    command = Oracle.decideLabelCommand decision caller object (ProcessLabel caller, 0) (Just (Label.initialBootstrapLabelEvidence object caller)) Nothing Nothing (Label.targetProcess caller) (Label.homeLabelAcceptanceCut acceptance EmptyHeraldPublicationPrefix)
    decided = submit local command fixture

completeLabel :: Identity.LabelDecisionId -> Fixture -> Fixture
completeLabel decision fixture = submit local command fixture
  where
    opened = fromJust (Oracle.oracleOpenDecision decision fixture.oracle)
    terminal = fromJust (Oracle.oracleTerminalOutcome decision fixture.oracle)
    command = Oracle.completeLabelDecisionCommand (Oracle.labelCompletionAttestation decision (Oracle.liveDecisionRequestControlIndex opened) (Oracle.deriveLabelOutcomeDigest terminal) local (Oracle.liveDecisionMembershipGeneration opened))

controlledStages :: (Fixture, Fixture, Fixture)
controlledStages = (decided, reopened, resolved)
  where
    subject = controlled Nothing
    (first, firstProbe) = open subject initial
    invalidated = submit local (Oracle.invalidateDisappearanceProbeCommand firstProbe local Disappearance.MatchingPublicationObserved (Domain.deriveDisappearanceEvidenceDigest "new value")) first
    (second, secondProbe) = open subject invalidated
    aborted = submit local (Oracle.abortDisappearanceProbeCommand secondProbe Disappearance.authorizedDisappearanceAbortReason) second
    (third, _) = open subject aborted
    (decided, decision) = labelControlled third
    completed = completeLabel decision decided
    overlay = fromJust (Projection.oracleProjectionReleasedLabelAt (Oracle.oracleGreatestControlIndex completed.oracle) object completed.projection)
    updated = controlled (Just (Oracle.liveRecordRevision overlay))
    (reopened, currentProbe) = open updated completed
    resolved = resolve updated currentProbe reopened

activateNewcomer :: Fixture -> Fixture
activateNewcomer = activatePreparedNewcomer . beginNewcomer

newcomer :: Identity.HeraldEpoch
newcomer = checked "new Herald epoch" (Identity.mkHeraldEpoch (Fixtures.fixtureIdentifierBytes 240))

joinContribution :: Topology.TopologyOccurrenceDigest
joinContribution = checked "contribution" (Topology.mkTopologyOccurrenceDigest (Fixtures.fixtureIdentifierBytes 237))

joinCut :: Membership.HeraldMembershipGeneration -> Identity.ControlIndex -> Topology.TopologyCut
joinCut generation index = checked "membership cut" (Topology.topologyCut (Topology.sameGenerationPredecessor cutId) (Topology.topologyFrontier (emptyStructuralVersionVector generation) index) joinContribution)
  where
    cutId = checked "cut ID" (Identity.mkTopologyCutId (Fixtures.fixtureIdentifierBytes 238))

beginNewcomer :: Fixture -> Fixture
beginNewcomer fixture = submit local (Oracle.beginHeraldAdmissionCommand manifest (joinCut generation at)) fixture
  where
    manifest = Admission.heraldAdmissionManifest system (checked "new Herald identity" (Identity.mkHeraldId (Fixtures.fixtureIdentifierBytes 239))) newcomer
    generation = Oracle.oracleCurrentMembership fixture.oracle
    at = Oracle.oracleGreatestControlIndex fixture.oracle

activatePreparedNewcomer :: Fixture -> Fixture
activatePreparedNewcomer begun = submit local (Oracle.activateHeraldCommand identifier) ready
  where
    generation = Oracle.oracleCurrentMembership begun.oracle
    cut = joinCut generation
    contribution = joinContribution
    record = fromJust (Oracle.oraclePendingHeraldAdmission begun.oracle)
    identifier = Admission.admissionRecordId record
    members = NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs generation)
    coordinator = NonEmpty.head (Membership.heraldMembershipGenerationActiveHeraldEpochs generation)
    prefix = Oracle.oracleGreatestControlIndex begun.oracle
    digest = checked "join digest" (Admission.mkHeraldJoinDigest (Topology.topologyOccurrenceDigestBytes contribution))
    recipe = checked "join recipe" (Topology.heraldJoinBaseRecipe identifier newcomer generation (cut prefix) contribution)
    recipeDigest = checked "recipe digest" (Admission.mkHeraldJoinDigest (Topology.heraldJoinBaseRecipeDigestBytes recipe))
    seal = checked "join seal" (Admission.heraldJoinSeal identifier (Admission.admissionRecordAttempt record) (cut prefix) prefix [(member, Admission.heraldJoinMemberCut 0 0 digest) | member <- members] digest recipeDigest)
    sealed = submit coordinator (Oracle.sealHeraldAdmissionCommand seal) begun
    accepted = foldl' (\state member -> submit member (Oracle.acceptHeraldJoinSealCommand identifier (Admission.joinSealAttempt seal) (Admission.joinSealDigest seal)) state) sealed members
    report member = Admission.heraldJoinReadyReport identifier (Admission.joinSealAttempt seal) member (Admission.joinSealDigest seal) prefix recipeDigest
    oldReady = foldl' (\state member -> submit member (Oracle.reportHeraldJoinBaseReadyCommand (report member)) state) accepted members
    ready = submit local (Oracle.reportHeraldJoinReadyCommand (report newcomer)) oldReady

-- The admitted newcomer is not a Raft voter, so its later retirement is a
-- genuine membership-superseded case independent of Begin-time preparation.
retireNewcomer :: Fixture -> Fixture
retireNewcomer fixture = submit local (Oracle.retireHeraldEpochCommand resolution newcomer) reported
  where
    generation = Oracle.oracleCurrentMembership fixture.oracle
    configuration = Voter.voterConfigurationId (Voter.oracleVoterConfiguration fixture.oracle)
    opened = submit local (Oracle.openHeraldFailureProbeCommand newcomer (Membership.heraldMembershipGenerationId generation) configuration) fixture
    probe = fromJust (Oracle.oracleActiveFailureProbeFor newcomer (Membership.heraldMembershipGenerationId generation) opened.oracle)
    resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget
    reported = foldl' (\state reporter -> submit reporter (Oracle.reportHeraldFailureProbeCommand probe configuration Oracle.ProbeUnreachable) state) opened [local, heraldMemberEpoch Fixtures.fixtureRemoteMember]

base :: Projection.State -> Projection.ProjectionDisappearanceBase
base = checked "capture disappearance base" . Projection.captureProjectionDisappearanceBase

bytesFor :: Projection.State -> ByteString
bytesFor = Projection.encodeProjectionDisappearanceBase . base

rawFor :: Projection.State -> RawBase
rawFor = checked "raw disappearance fixture" . Serialize.decode . bytesFor

aggregateCapture :: Projection.State -> Projection.ProjectionBaseCapture
aggregateCapture = checked "capture aggregate disappearance base" . Projection.captureProjectionBase

aggregateRoundTrip :: Projection.State -> Bool
aggregateRoundTrip state =
  Projection.decodeProjectionBase initial.projection (Projection.encodeProjectionBase captured) == Right captured
  where
    captured = aggregateCapture state

reclaimPrefix :: Projection.State -> Projection.State
reclaimPrefix state = checked "reclaim disappearance prefix" (Projection.advanceReplayBase state >>= Projection.reclaimAppliedPrefixThroughBase Set.empty Nothing)

propRegularPrefixes :: QC.NonNegative Int -> QC.Property
propRegularPrefixes (QC.NonNegative seed) =
  QC.conjoin
    [ QC.counterexample ("regular prefix " <> show index)
        $ QC.conjoin
          [ Projection.decodeProjectionDisappearanceBase state (bytesFor state) QC.=== Right (base state),
            QC.counterexample "complete aggregate differs after transfer to fresh receiver" (aggregateRoundTrip state),
            QC.counterexample "reclaimed aggregate differs after transfer to fresh receiver" (aggregateRoundTrip (reclaimPrefix state))
          ]
    | (index, state) <- zip [(0 :: Int) ..] (regularHistory (1 + seed `mod` 4)).prefixes
    ]

caseMixedTerminals :: Assertion
caseMixedTerminals = do
  let (_, _, controlledResolved) = controlledStages
      (collecting, _) = open (regular Occurrence.Genesis) controlledResolved
      active = activateNewcomer collecting
      (reopened, _) = open (regular Occurrence.Genesis) active
      changed = retireNewcomer reopened
      (domain, rows) = rawFor changed.projection
      terminalTags = [tag | (_, _, _, _, Just (tag, _, _)) <- rows]
  assertBool "every terminal arm is present" (all (`elem` terminalTags) [0, 1, 2, 3, 4, 5])
  assertEqual "mixed nominal transfer round-trips" (Right (base changed.projection)) (Projection.decodeProjectionDisappearanceBase changed.projection (Serialize.encode (domain, rows)))
  mapM_
    ( \state -> do
        assertEqual "each mixed canonical prefix admits" (Right (base state)) (Projection.decodeProjectionDisappearanceBase state (bytesFor state))
        assertBool "complete mixed aggregate admits at fresh receiver" (aggregateRoundTrip state)
        assertBool "reclaimed mixed aggregate admits at fresh receiver" (aggregateRoundTrip (reclaimPrefix state))
    )
    changed.prefixes

caseAdmissionPreparation :: Assertion
caseAdmissionPreparation = do
  let (withControlled, controlledProbe) = open (controlled Nothing) initial
      (collecting, regularProbe) = open (regular Occurrence.Genesis) withControlled
      begun = beginNewcomer collecting
      record = fromJust (Oracle.oraclePendingHeraldAdmission begun.oracle)
      admission = Admission.admissionRecordId record
      beginIndex = Identity.controlIndexWord64 (Oracle.oracleGreatestControlIndex begun.oracle)
      cancelled = submit local (Oracle.cancelHeraldAdmissionCommand admission) begun
      (reopenedControlled, freshControlled) = open (controlled Nothing) cancelled
      (reopened, freshRegular) = open (regular Occurrence.Genesis) reopenedControlled
      active = activatePreparedNewcomer begun
      (afterActivation, freshAfterActivation) = open (regular Occurrence.Genesis) active
      (domain, rows) = rawFor begun.projection
      phases = [terminal | (_, _, _, _, terminal) <- rows]
      expected = Just (5, beginIndex, [Membership.heraldAdmissionIdCanonicalBytes admission])
      unknownAdmission = checked "unknown admission" (Membership.deriveHeraldAdmissionId (Identity.controlIndex (beginIndex + 1)))
      mutateCause fields = (domain, [(at, subject, members, reports, Just (5, beginIndex, fields)) | (at, subject, members, reports, _) <- rows])
  assertEqual "both probes terminate in the Begin entry" [expected, expected] phases
  assertEqual "Begin retains its predecessor membership" (Oracle.oracleCurrentMembership collecting.oracle) (Oracle.oracleCurrentMembership begun.oracle)
  assertBool "cancellation permits fresh controlled and regular capture" (freshControlled /= controlledProbe && freshRegular /= regularProbe)
  assertBool "activation permits fresh capture" (freshAfterActivation /= regularProbe)
  mapM_
    ( \state -> do
        assertEqual "preparation/cancellation leaf survives transfer" (Right (base state)) (Projection.decodeProjectionDisappearanceBase state (bytesFor state))
        assertBool "preparation/cancellation aggregate survives transfer" (aggregateRoundTrip state)
        assertBool "reclaimed preparation/cancellation survives transfer" (aggregateRoundTrip (reclaimPrefix state))
    )
    (reopened.prefixes <> afterActivation.prefixes)
  rejects begun.projection "preparation cause requires exactly one admission" (mutateCause [])
  rejects begun.projection "preparation cause rejects duplicated admission" (mutateCause [Membership.heraldAdmissionIdCanonicalBytes admission, Membership.heraldAdmissionIdCanonicalBytes admission])
  rejects cancelled.projection "preparation cause binds an existing admission" (mutateCause [Membership.heraldAdmissionIdCanonicalBytes unknownAdmission])
  rejects cancelled.projection "preparation terminal binds the exact Begin coordinate" (domain, [(at, subject, members, reports, Just (5, beginIndex + 1, fields)) | (at, subject, members, reports, Just (_, _, fields)) <- rows])
  rejects begun.projection "pending admission cannot import a collecting predecessor probe" (domain, [(at, subject, members, reports, Nothing) | (at, subject, members, reports, _) <- rows])
  -- A canonical entry from an alternative history is nominally valid but
  -- cannot open a probe against this owner's pending admission.
  let skipRequest = oracleClientRequestId local beginIndex
      skipCommand = Voter.cancelVoterChangeCommand (checked "absent competing change" (Voter.mkVoterChangeId (Identity.controlIndex 99999)))
      (alternativeOracle, skipOutcome) = Oracle.submitOracleState (Oracle.oracleEnvelope skipRequest Nothing local skipCommand) collecting.oracle
      alternativeProjection = case Oracle.oracleSubmissionOutcomeView skipOutcome of
        Oracle.OracleSubmissionCommittedView _ entry -> Projection.commitAppliedEntry (checked "alternative inert control" (Projection.prepareAppliedEntry (canonicalizeAppliedOracleEntry entry) collecting.projection))
        other -> error ("alternative control was not committed: " <> show other)
      freshObject = checked "alternative controlled object" (Identity.mkGlobalObjectId (Fixtures.fixtureIdentifierBytes 212))
      freshSubject = checked "alternative controlled subject" (Domain.controlledPredefinedDisappearanceSubject NeutralVertexRole freshObject (Identity.structuralOccurrenceId local Identity.firstStructuralSequence) Nothing)
      freshRequest = oracleClientRequestId local (beginIndex + 1)
      freshCommand = Oracle.openDisappearanceProbeCommand freshSubject (Domain.disappearanceSubjectMembershipCoordinate freshSubject (Oracle.oracleCurrentMembership alternativeOracle))
      (_, freshOutcome) = Oracle.submitOracleState (Oracle.oracleEnvelope freshRequest Nothing local freshCommand) alternativeOracle
  case Oracle.oracleSubmissionOutcomeView freshOutcome of
    Oracle.OracleSubmissionCommittedView receipt entry -> do
      assertEqual "alternative history admits a genuinely fresh probe" OracleAccepted (oracleReceiptResult receipt)
      assertBool "accepted Open belongs to its actual alternative predecessor" (not (isLeft (Projection.prepareAppliedEntry (canonicalizeAppliedOracleEntry entry) alternativeProjection)))
      assertBool "pending admission rejects an otherwise valid accepted Open" (isLeft (Projection.prepareAppliedEntry (canonicalizeAppliedOracleEntry entry) begun.projection))
    other -> error ("alternative Open was not committed: " <> show other)

caseIndependentContext :: Assertion
caseIndependentContext = do
  let source = (regularHistory 3).projection
      at = Identity.controlIndexWord64 (Projection.oracleViewControlIndex (Projection.oracleView source))
      context = advanceEmpty at (initial.oracle, initial.projection)
  assertEqual "alternative context has no disappearance evidence" [] (Projection.projectedDisappearanceProbes context)
  assertEqual "decode uses supplied rows rather than context evidence" (Right (base source)) (Projection.decodeProjectionDisappearanceBase context (bytesFor source))
  where
    advanceEmpty count (oracleState, projected)
      | count == 0 = projected
      | otherwise = case Oracle.oracleSubmissionOutcomeView outcome of
          Oracle.OracleSubmissionCommittedView _ entry -> advanceEmpty (count - 1) (next, Projection.commitAppliedEntry (checked "empty semantic progress" (Projection.prepareAppliedEntry (canonicalizeAppliedOracleEntry entry) projected)))
          other -> error ("empty context control not committed: " <> show other)
      where
        sequenceNumber = 1 + Identity.controlIndexWord64 (Oracle.oracleGreatestControlIndex oracleState)
        request = oracleClientRequestId local sequenceNumber
        command = Voter.cancelVoterChangeCommand (checked "absent change" (Voter.mkVoterChangeId (Identity.controlIndex 99999)))
        (next, outcome) = Oracle.submitOracleState (Oracle.oracleEnvelope request Nothing local command) oracleState

caseNominalHeader :: Assertion
caseNominalHeader = do
  let membership = Oracle.oracleCurrentMembership initial.oracle
      subject = controlled Nothing
      opened = Identity.controlIndex 7
      header = checked "nominal header" (Disappearance.admitProjectedDisappearanceProbeHeader opened subject membership)
  assertEqual "derived probe" (Domain.deriveDisappearanceProbeId opened) (Right (Disappearance.projectedDisappearanceProbeHeaderId header))
  assertEqual "derived coordinate" (Domain.disappearanceSubjectMembershipCoordinate subject membership) (Disappearance.projectedDisappearanceProbeHeaderCoordinate header)
  assertBool "zero Open rejected" (isLeft (Disappearance.admitProjectedDisappearanceProbeHeader (Identity.controlIndex 0) subject membership))

rejects :: Projection.State -> String -> RawBase -> Assertion
rejects context label = assertBool label . isLeft . Projection.decodeProjectionDisappearanceBase context . Serialize.encode

caseMalformedRows :: Assertion
caseMalformedRows = do
  let context = (regularHistory 2).projection
      raw@(domain, rows) = rawFor context
      first = case rows of row : _ -> row; [] -> error "regular fixture has no probes"
      (opened, subject, generation, reports, terminal) = first
      replaceFirst value = (domain, value : drop 1 rows)
      beyond = 1 + Identity.controlIndexWord64 (Projection.oracleViewControlIndex (Projection.oracleView context))
  rejects context "wrong domain" ("wrong-domain", rows)
  rejects context "duplicate row" (domain, rows <> [first])
  rejects context "reordered rows" (domain, reverse rows)
  rejects context "malformed subject" (replaceFirst (opened, "bad-subject", generation, reports, terminal))
  rejects context "unknown membership" (replaceFirst (opened, subject, Fixtures.fixtureIdentifierBytes 234, reports, terminal))
  rejects context "Open beyond cut" (replaceFirst (beyond, subject, generation, reports, terminal))
  rejects context "missing resolved reports" (domain, [(at, value, members, if result == Nothing then evidence else [], result) | (at, value, members, evidence, result) <- rows])
  rejects context "duplicated reporter" (domain, [(at, value, members, evidence <> take 1 evidence, result) | (at, value, members, evidence, result) <- rows])
  rejects context "terminal at Open" (domain, [(at, value, members, evidence, fmap (\(tag, _, fields) -> (tag, at, fields)) result) | (at, value, members, evidence, result) <- rows])
  rejects context "unknown terminal tag" (replaceFirst (opened, subject, generation, reports, Just (255, beyond, [])))
  assertBool "trailing bytes rejected" (isLeft (Projection.decodeProjectionDisappearanceBase context (Serialize.encode raw <> "x")))

caseOutcomeBinding :: Assertion
caseOutcomeBinding = do
  let context = (regularHistory 1).projection
      (domain, rows) = rawFor context
      foreignSystem = checked "foreign system" (Identity.mkSystemId (Fixtures.fixtureIdentifierBytes 233))
      mutate row@(opened, subjectBytes, generation, reports, terminal) = case terminal of
        Just (2, at, [_]) ->
          let subject = checked "subject frame" (Domain.decodeDisappearanceSubjectCanonicalBytes subjectBytes)
              outcome = checked "foreign resolution" (Domain.resolveDisappearanceSubject foreignSystem (Identity.controlIndex at) subject)
           in (opened, subjectBytes, generation, reports, Just (2, at, [Domain.disappearanceResolutionOutcomeCanonicalBytes outcome]))
        _ -> row
  rejects context "foreign system cannot choose retirement successor" (domain, map mutate rows)

caseCurrentClosure :: Assertion
caseCurrentClosure = do
  let regularContext = (regularHistory 1).projection
      (domain, rows) = rawFor regularContext
      staleRegular row@(opened, _, generation, reports, terminal) = case terminal of
        Nothing -> (opened, Domain.disappearanceSubjectCanonicalBytes (regular Occurrence.Genesis), generation, reports, Nothing)
        _ -> row
  rejects regularContext "collecting regular probe uses current occurrence" (domain, map staleRegular rows)
  let (pending, liveControlled, _) = controlledStages
      (controlledDomain, controlledRows) = rawFor liveControlled.projection
      staleRevision row@(opened, _, generation, reports, terminal) = case terminal of
        Nothing -> (opened, Domain.disappearanceSubjectCanonicalBytes (controlled Nothing), generation, reports, Nothing)
        _ -> row
  rejects liveControlled.projection "collecting controlled probe uses current revision" (controlledDomain, map staleRevision controlledRows)
  let pendingAt = Identity.controlIndexWord64 (Oracle.oracleGreatestControlIndex pending.oracle)
      overlay = fromJust (Projection.oracleProjectionReleasedLabelAt (Identity.controlIndex pendingAt) object pending.projection)
      row = (pendingAt, Domain.disappearanceSubjectCanonicalBytes (controlled (Just (Oracle.liveRecordRevision overlay))), Membership.heraldMembershipGenerationIdBytes (Membership.heraldMembershipGenerationId (Oracle.oracleCurrentMembership pending.oracle)), [], Nothing)
      (pendingDomain, pendingRows) = rawFor pending.projection
  rejects pending.projection "collecting probe cannot coexist with pending label" (pendingDomain, row : pendingRows)
  let changed = activateNewcomer (regularHistory 1)
      (changedDomain, changedRows) = rawFor changed.projection
      revive rowValue@(opened, subject, generation, reports, terminal) = case terminal of
        Just (5, _, _) -> (opened, subject, generation, reports, Nothing)
        _ -> rowValue
  rejects changed.projection "collecting old-member probe cannot survive activation" (changedDomain, map revive changedRows)

caseSparseTerminalFrontier :: Assertion
caseSparseTerminalFrontier = do
  let (_, _, resolved) = controlledStages
      sparse = reclaimPrefix resolved.projection
      captured = aggregateCapture sparse
      bytes = Projection.encodeProjectionBase captured
      (domain, identity, leaves, (semantic, digest), archive@(_, entries, _)) = checked "raw sparse disappearance aggregate" (Serialize.decode bytes :: Either String RawAggregate)
      terminalIndices = [at | (_, _, _, _, Just (_, at, _)) <- snd (rawFor sparse)]
      openedIndices = [at | (at, _, _, _, _) <- snd (rawFor sparse)]
      earlier = semantic - 1
      mutation = (domain, identity, leaves, (earlier, digest), archive)
  assertBool "sparse source has no retained canonical entry to prove its frontier" (null entries)
  assertBool "final semantic fact is a disappearance terminal" (semantic `elem` terminalIndices)
  assertBool "mutation still covers every disappearance Open" (all (<= earlier) openedIndices)
  assertBool "sparse complete capture survives transfer" (Projection.decodeProjectionBase initial.projection bytes == Right captured)
  assertBool "terminal facts reject a frontier before resolution even without exact entries" (isLeft (Projection.decodeProjectionBase initial.projection (Serialize.encode mutation)))

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
