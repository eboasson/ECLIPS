{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module AdmissionProperties (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Serialize (Serialize)
import Data.Serialize qualified as S
import Data.Word (Word64)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Identity
import Eclips.Domain.Label qualified as DomainLabel
import Eclips.Domain.Membership
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Domain.Sort.Profile (PredefinedSortRole (EdgeRole, NeutralVertexRole), predefinedCatalogueDescriptor, profileCatalogueDigest, profileEntryFor)
import Eclips.Domain.SortOccurrence (SortOccurrenceBase (Genesis))
import Eclips.Domain.Startup (HeraldMember (heraldMemberEpoch, heraldMemberId), deriveInitialProjectionDigest)
import Eclips.Domain.Structural
import Eclips.Domain.Topology
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Oracle.Admission
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command
import Eclips.Oracle.Disappearance qualified as D
import Eclips.Oracle.Effect
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity
import Eclips.Oracle.Label qualified as Label
import Eclips.Oracle.Projection
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import Eclips.Oracle.Voter qualified as V
import GHC.Generics (Generic)
import OracleFixtures hiding (heraldEpoch)
import OracleFixtures qualified as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, chooseInt, conjoin, forAll, ioProperty, property, shuffle, testProperty)
import VoterFailureFixtures qualified as VF

tests :: TestTree
tests =
  testGroup
    "live Herald admission"
    [ testProperty "old-member evidence and newcomer readiness commute through checkpoints without circular barrier" propReadinessOrders,
      testProperty "Begin atomically aborts every collecting disappearance probe before cancellation or activation" propBeginAbortsDisappearance,
      testCase "early activation and foreign/stale evidence reject without granting membership" caseEarlyActivation,
      testCase "End reopens seal with a fresh attempt and stale readiness cannot activate" caseEndInvalidation,
      testCase "End invalidates already frozen capture identities before the first seal" caseUnsealedEndInvalidation,
      testCase "cancellation retains exact retry and permanently spends the epoch" caseCancel,
      testCase "failure threshold preempts admission and retirement remains possible" caseFailurePreemption,
      testCase "accepted voter failure preempts every pending join capture and base stage" caseAcceptedVoterFailurePreemption,
      testCase "fresh epoch re-admission after voter retirement cannot revive old epoch or voter authority" caseFreshEpochAfterVoterRetirement,
      testCase "seal must cover every stamped source prefix and exact old members" caseStampedCuts,
      testCase "pending applicants have no ordinary Oracle home; activated homes start processes" caseApplicantAuthority,
      testCase "newly admitted Herald cannot claim an older completed label" caseCompletedLabelMembership,
      testCase "all commands and activation projection snapshots round-trip canonically" caseCanonical,
      testCase "canonical records reject missing seals and contribution-unbound recipes" caseMalformedRecordClosure
    ]

applicant :: HeraldEpoch
applicant = F.heraldEpoch 201
manifest :: HeraldAdmissionManifest
manifest = heraldAdmissionManifest fixtureSystemId (F.heraldId 200) applicant
oldGeneration :: HeraldMembershipGeneration
oldGeneration = oracleCurrentMembership fixtureInitialState
oldMembers :: [HeraldEpoch]
oldMembers = NE.toList (heraldMembershipGenerationActiveHeraldEpochs oldGeneration)
anchor :: TopologyCut
anchor = cutFor oldGeneration (controlIndex 0)
cutFor :: HeraldMembershipGeneration -> ControlIndex -> TopologyCut
cutFor generation prefix = checked "join cut" (topologyCut (sameGenerationPredecessor (checked "cut predecessor" (mkTopologyCutId (identifierBytes 240)))) (topologyFrontier (emptyStructuralVersionVector generation) prefix) contribution)
contribution :: TopologyOccurrenceDigest
contribution = checked "contribution" (mkTopologyOccurrenceDigest (identifierBytes 220))
digest :: HeraldJoinDigest
digest = checked "join digest" (mkHeraldJoinDigest (topologyOccurrenceDigestBytes contribution))
ident :: HeraldAdmissionId
ident = checked "admission ID" (deriveHeraldAdmissionId (controlIndex 1))
sealFor :: HeraldAdmissionRecord -> ControlIndex -> HeraldJoinSeal
sealFor record prefix = checked "join seal" (heraldJoinSeal (admissionRecordId record) (admissionRecordAttempt record) cut prefix [(h, heraldJoinMemberCut 0 0 digest) | h <- NE.toList (heraldMembershipGenerationActiveHeraldEpochs generation)] digest recipeDigest)
  where
    generation = admissionRecordPredecessor record
    cut = cutFor generation prefix
    recipe = checked "join recipe" (heraldJoinBaseRecipe (admissionRecordId record) (admissionManifestHeraldEpoch (admissionRecordManifest record)) generation cut contribution)
    recipeDigest = checked "recipe digest" (mkHeraldJoinDigest (heraldJoinBaseRecipeDigestBytes recipe))
initialRecord :: HeraldAdmissionRecord
initialRecord = fromJust (oraclePendingHeraldAdmission begun)
begun :: OracleState
begun = accepted fixtureHeraldEpoch (beginHeraldAdmissionCommand manifest anchor) fixtureInitialState
seal :: HeraldJoinSeal
seal = sealFor initialRecord (controlIndex 1)
sealed :: OracleState
sealed = accepted (NE.head (heraldMembershipGenerationActiveHeraldEpochs oldGeneration)) (sealHeraldAdmissionCommand seal) begun
readyReport :: HeraldJoinSeal -> HeraldEpoch -> HeraldJoinReadyReport
readyReport s reporter = heraldJoinReadyReport (joinSealAdmissionId s) (joinSealAttempt s) reporter (joinSealDigest s) (joinSealControlPrefix s) (joinSealRecipeDigest s)
acceptSeal :: HeraldJoinSeal -> HeraldEpoch -> OracleState -> OracleState
acceptSeal s h = accepted h (acceptHeraldJoinSealCommand (joinSealAdmissionId s) (joinSealAttempt s) (joinSealDigest s))
oldReady :: HeraldJoinSeal -> HeraldEpoch -> OracleState -> OracleState
oldReady s h = accepted h (reportHeraldJoinBaseReadyCommand (readyReport s h))
newReady :: HeraldJoinSeal -> OracleState -> OracleState
newReady s = accepted fixtureHeraldEpoch (reportHeraldJoinReadyCommand (readyReport s applicant))
allAccepted :: OracleState
allAccepted = foldl' (flip (acceptSeal seal)) sealed oldMembers
allReady :: OracleState
allReady = newReady seal (foldl' (flip (oldReady seal)) allAccepted oldMembers)
activated :: OracleState
activated = accepted fixtureHeraldEpoch (activateHeraldCommand ident) allReady

submit :: HeraldEpoch -> OracleCommand -> OracleState -> (OracleState, OracleReceipt, AppliedOracleEntry)
submit home command state =
  let request = oracleClientRequestId home (1 + controlIndexWord64 (oracleGreatestControlIndex state))
      envelope = oracleEnvelope request Nothing home command
   in case checked "submit admission test" (stepOracle envelope state) of
        (next, OracleCommitted receipt, effects) -> case oracleEffects effects of
          [EmitAppliedOracleEntry entry] -> (next, receipt, entry)
          _ -> error "missing applied admission entry"
        _ -> error "fresh admission request did not commit"
accepted :: HeraldEpoch -> OracleCommand -> OracleState -> OracleState
accepted home command state = case submit home command state of
  (next, receipt, _) | oracleReceiptResult receipt == OracleAccepted -> next
  (_, receipt, _) -> error ("admission request unexpectedly rejected: " <> show (oracleReceiptResult receipt))
rejects :: OracleRejection -> HeraldEpoch -> OracleCommand -> OracleState -> Assertion
rejects problem home command state = let (_, receipt, _) = submit home command state in oracleReceiptResult receipt @?= OracleRejected problem

propReadinessOrders :: Property
propReadinessOrders = forAll (shuffle oldMembers) $ \acceptOrder -> forAll (shuffle (applicant : oldMembers)) $ \readyOrder ->
  let acceptedSeals = foldl' (flip (acceptSeal seal)) sealed acceptOrder
      states = scanl (\state h -> if h == applicant then newReady seal state else oldReady seal h state) acceptedSeals readyOrder
      ready = last states
      final = accepted fixtureHeraldEpoch (activateHeraldCommand ident) ready
      record = fromJust (oracleHeraldAdmission ident final)
   in conjoin
        [ property (all ((== oldGeneration) . oracleCurrentMembership) states),
          property (admissionRecordPhase (fromJust (oraclePendingHeraldAdmission ready)) == AdmissionReady),
          property (NE.toList (heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership final)) == oldMembers <> [applicant]),
          property (oracleStateGenesis final == fixtureCheckedGenesis),
          property (decodeAdmissionRecord (encodeAdmissionRecord record) == Right record),
          property (oraclePendingHeraldAdmission final == Nothing),
          conjoin [property (decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes state) == Right state) | state <- fixtureInitialState : begun : sealed : final : states],
          property (fmap (accepted fixtureHeraldEpoch (activateHeraldCommand ident)) (decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes ready)) == Right final)
        ]

-- Exercise independent controlled objects and regular sorts, including empty,
-- partial and complete evidence. Begin must terminalize them before any new
-- member exists; cancellation must not revive the captured predecessor proof.
admissionDisappearanceSubjects :: [Disappearance.DisappearanceSubject]
admissionDisappearanceSubjects =
  [ controlledSubject NeutralVertexRole 213,
    controlledSubject EdgeRole 214,
    regularSubject NeutralVertexRole,
    regularSubject EdgeRole
  ]
  where
    controlledSubject role seed = checked "admission controlled disappearance" (Disappearance.controlledPredefinedDisappearanceSubject role (globalObjectId seed) (structuralOccurrenceId fixtureHeraldEpoch firstStructuralSequence) Nothing)
    regularSubject role = Disappearance.regularSortDefinitionDisappearanceSubject (Disappearance.deriveRegularSortOccurrenceClaim fixtureSystemId (predefinedCatalogueDescriptor (profileEntryFor role)) Genesis)

openAdmissionDisappearance :: Disappearance.DisappearanceSubject -> OracleState -> (OracleState, Disappearance.DisappearanceProbeId, AppliedOracleEntry)
openAdmissionDisappearance subject state =
  let command = Label.openDisappearanceProbeCommand subject (Disappearance.disappearanceSubjectMembershipCoordinate subject (oracleCurrentMembership state))
      (next, receipt, entry) = submit fixtureHeraldEpoch command state
      probe = checked "admission disappearance probe" (Disappearance.deriveDisappearanceProbeId (oracleGreatestControlIndex next))
   in if oracleReceiptResult receipt == OracleAccepted then (next, probe, entry) else error ("admission disappearance Open rejected: " <> show receipt)

admissionDisappearanceClaim :: OracleState -> Disappearance.DisappearanceSubject -> Disappearance.DisappearanceProbeId -> HeraldEpoch -> Disappearance.DisappearanceEvidenceClaim
admissionDisappearanceClaim state subject probe reporter = checked "admission disappearance claim" (Disappearance.admitDisappearanceEvidenceClaim subject (oracleCurrentMembership state) probe reporter (Disappearance.deriveDisappearanceEvidenceDigest "admission absence witness"))

probePhase :: Disappearance.DisappearanceProbeId -> OracleState -> Maybe D.DisappearanceProbePhaseView
probePhase probe state = case Label.oracleDisappearanceProbe probe state of
  Just (D.DisappearanceProbeView _ _ _ _ _ _ _ phase) -> Just phase
  Nothing -> Nothing

readyAdmissionFrom :: OracleState -> (OracleState, [OracleState])
readyAdmissionFrom preparing = (newcomerReady, sealedState : acceptedStates <> readyStates <> [newcomerReady])
  where
    record = fromJust (oraclePendingHeraldAdmission preparing)
    captured = sealFor record (oracleGreatestControlIndex preparing)
    sealedState = accepted fixtureHeraldEpoch (sealHeraldAdmissionCommand captured) preparing
    acceptedStates = drop 1 (scanl (\state member -> acceptSeal captured member state) sealedState oldMembers)
    acknowledged = foldl' (flip (acceptSeal captured)) sealedState oldMembers
    readyStates = drop 1 (scanl (\state member -> oldReady captured member state) acknowledged oldMembers)
    based = foldl' (flip (oldReady captured)) acknowledged oldMembers
    newcomerReady = newReady captured based

propBeginAbortsDisappearance :: Property
propBeginAbortsDisappearance =
  forAll (shuffle admissionDisappearanceSubjects) $ \subjects ->
    forAll (shuffle oldMembers) $ \reporters ->
      forAll (chooseInt (0, length oldMembers)) $ \reportCount -> ioProperty $ do
        let historicalSubject = case subjects of value : _ -> value; [] -> error "empty disappearance subject fixture"
            (historicalOpen, historicalProbe, _) = openAdmissionDisappearance historicalSubject fixtureInitialState
            predecessor = accepted fixtureHeraldEpoch (Label.abortDisappearanceProbeCommand historicalProbe D.authorizedDisappearanceAbortReason) historicalOpen
            historicalTerminal = Label.oracleDisappearanceProbe historicalProbe predecessor
            addSubject (state, priorProbes, priorHistory) subject =
              let (opened, probe, openEntry) = openAdmissionDisappearance subject state
                  addReport (current, prior) reporter =
                    let claim = admissionDisappearanceClaim current subject probe reporter
                        (next, reportReceipt, reportEntry) = submit reporter (Label.reportPredefinedAbsenceCommand probe claim) current
                     in if oracleReceiptResult reportReceipt == OracleAccepted then (next, prior <> [(next, reportEntry)]) else error "admission fixture report rejected"
                  (reported, reports) = foldl' addReport (opened, []) (take reportCount reporters)
               in (reported, priorProbes <> [(subject, probe)], priorHistory <> [(opened, openEntry)] <> reports)
            (collecting, probes, history) = foldl' addSubject (predecessor, [], []) subjects
            (preparing, receipt, entry) = submit fixtureHeraldEpoch (beginHeraldAdmissionCommand manifest (cutFor oldGeneration (oracleGreatestControlIndex collecting))) collecting
            admission = admissionRecordId (fromJust (oraclePendingHeraldAdmission preparing))
            beginIndex = oracleGreatestControlIndex preparing
            views = fmap oracleProjectionEventView (appliedEntryProjectionEvents entry)
            aborts = [(probe, cause) | DisappearanceProbeAbortedView probe (D.AdmissionPreparingDisappearanceAbortView cause) <- views]
            (ready, preparationStates) = readyAdmissionFrom preparing
            cancelled = accepted fixtureHeraldEpoch (cancelHeraldAdmissionCommand admission) ready
            active = accepted fixtureHeraldEpoch (activateHeraldCommand admission) ready
            allStates = preparing : preparationStates <> [cancelled, active]
        oracleReceiptResult receipt @?= OracleAccepted
        oracleCurrentMembership preparing @?= oldGeneration
        sort aborts @?= sort [(probe, admission) | (_, probe) <- probes]
        length views @?= 1 + length probes
        forM_ allStates $ \state -> do
          Label.oracleDisappearanceProbe historicalProbe state @?= historicalTerminal
          forM_ probes $ \(_, probe) -> probePhase probe state @?= Just (D.DisappearanceAdmissionPreparingView admission beginIndex)
          decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes state) @?= Right state
        forM_ (history <> [(preparing, entry)]) $ \(state, applied) -> do
          let canonical = canonicalizeAppliedOracleEntry applied
              events = canonicalizeOracleProjectionEventVector (appliedEntryProjectionEvents applied)
          decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes canonical) @?= Right canonical
          decodeCanonicalOracleProjectionEventVector (canonicalOracleProjectionEventVectorBytes events) @?= Right events
          decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes state) @?= Right state
        forM_ (preparing : preparationStates) $ \state -> forM_ probes $ \(subject, probe) -> do
          let command = Label.openDisappearanceProbeCommand subject (Disappearance.disappearanceSubjectMembershipCoordinate subject (oracleCurrentMembership state))
              (blocked, blockedReceipt, blockedEntry) = submit fixtureHeraldEpoch command state
          oracleReceiptResult blockedReceipt @?= OracleRejected (DisappearanceCommandRejected (D.DisappearanceAdmissionPreparing admission))
          appliedEntryProjectionEvents blockedEntry @?= []
          Label.oracleDisappearanceProbes blocked @?= Label.oracleDisappearanceProbes state
          let canonicalRejected = canonicalizeOracleReceipt blockedReceipt
              canonicalBlocked = canonicalizeAppliedOracleEntry blockedEntry
          decodeCanonicalOracleReceipt (canonicalOracleReceiptBytes canonicalRejected) @?= Right canonicalRejected
          decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes canonicalBlocked) @?= Right canonicalBlocked
          let originalRequest = oracleClientRequestId fixtureHeraldEpoch (controlIndexWord64 (Disappearance.disappearanceProbeOpenControlIndex probe))
              originalEnvelope = oracleEnvelope originalRequest Nothing fixtureHeraldEpoch command
              (retried, retryOutcome, retryEffects) = checked "retry predecessor Open during admission" (stepOracle originalEnvelope state)
          retried @?= state
          assertBool "the original Open retry retains its exact receipt" (case retryOutcome of OracleDuplicate {} -> True; _ -> False)
          oracleEffects retryEffects @?= []
          let claim = admissionDisappearanceClaim state subject probe fixtureHeraldEpoch
              (_, lateReceipt, lateEntry) = submit fixtureHeraldEpoch (Label.reportPredefinedAbsenceCommand probe claim) state
          assertBool "late predecessor report cannot restart the probe" (oracleReceiptResult lateReceipt /= OracleAccepted)
          appliedEntryProjectionEvents lateEntry @?= []
        forM_ [cancelled, active] $ \state -> forM_ probes $ \(subject, oldProbe) -> do
          let (reopened, fresh, _) = openAdmissionDisappearance subject state
          assertBool "new capture has a new identity" (fresh /= oldProbe)
          probePhase fresh reopened @?= Just D.DisappearanceCollectingView
          probePhase oldProbe reopened @?= Just (D.DisappearanceAdmissionPreparingView admission beginIndex)
          decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes reopened) @?= Right reopened
        pure True

caseEarlyActivation :: Assertion
caseEarlyActivation = do
  rejects (HeraldAdmissionCommandRejected JoinReadinessIncomplete) fixtureHeraldEpoch (activateHeraldCommand ident) sealed
  rejects (HeraldAdmissionCommandRejected JoinSealNotCollected) fixtureHeraldEpoch (reportHeraldJoinReadyCommand (readyReport seal applicant)) sealed
  rejects (HeraldAdmissionCommandRejected JoinReporterMismatch) fixtureHeraldEpoch (reportHeraldJoinBaseReadyCommand (readyReport seal fixtureRemoteHeraldEpoch)) allAccepted
  let wrong = heraldJoinReadyReport ident 2 applicant (joinSealDigest seal) (controlIndex 1) (joinSealRecipeDigest seal)
  rejects (HeraldAdmissionCommandRejected JoinReadinessMismatch) fixtureHeraldEpoch (reportHeraldJoinReadyCommand wrong) allAccepted

caseUnsealedEndInvalidation :: Assertion
caseUnsealedEndInvalidation = do
  let end = checked "End" (endProcessEpochCommand fixtureProcessEpoch ExplicitAdministrativeEnd)
      ended = accepted fixtureHeraldEpoch end begun
      record = fromJust (oraclePendingHeraldAdmission ended)
  admissionRecordPhase record @?= AdmissionPreparing
  admissionRecordAttempt record @?= 2
  admissionRecordSemanticToken record @?= controlIndex 2
  admissionRecordSeal record @?= Nothing
  rejects (HeraldAdmissionCommandRejected JoinSealAttemptMismatch) fixtureHeraldEpoch (sealHeraldAdmissionCommand seal) ended

caseEndInvalidation :: Assertion
caseEndInvalidation = do
  let end = checked "End" (endProcessEpochCommand fixtureProcessEpoch ExplicitAdministrativeEnd)
      (ended, receipt, entry) = submit fixtureHeraldEpoch end allReady
      record = fromJust (oraclePendingHeraldAdmission ended)
  oracleReceiptResult receipt @?= OracleAccepted
  admissionRecordPhase record @?= AdmissionPreparing
  admissionRecordAttempt record @?= 2
  admissionRecordBaseReporters record @?= []
  admissionRecordNewcomerReport record @?= Nothing
  admissionRecordSemanticToken record @?= oracleGreatestControlIndex ended
  assertBool "End and invalidation in one projection" (length (appliedEntryProjectionEvents entry) == 2)
  rejects (HeraldAdmissionCommandRejected JoinReadinessIncomplete) fixtureHeraldEpoch (activateHeraldCommand ident) ended
  let freshSeal = sealFor record (oracleGreatestControlIndex ended)
      recaptured = accepted (NE.head (heraldMembershipGenerationActiveHeraldEpochs oldGeneration)) (sealHeraldAdmissionCommand freshSeal) ended
  rejects (HeraldAdmissionCommandRejected JoinSealAttemptMismatch) fixtureHeraldEpoch (acceptHeraldJoinSealCommand ident 1 (joinSealDigest seal)) recaptured
  let prepared = newReady freshSeal (foldl' (flip (oldReady freshSeal)) (foldl' (flip (acceptSeal freshSeal)) recaptured oldMembers) oldMembers)
  assertBool "new attempt activates" (oracleCurrentMembership (accepted fixtureHeraldEpoch (activateHeraldCommand ident) prepared) /= oldGeneration)

caseCancel :: Assertion
caseCancel = do
  let command = cancelHeraldAdmissionCommand ident
      (cancelled, receipt, _) = submit fixtureHeraldEpoch command allReady
      request = oracleReceiptRequestId receipt
      envelope = oracleEnvelope request Nothing fixtureHeraldEpoch command
      (retried, outcome, effects) = checked "cancel retry" (stepOracle envelope cancelled)
  retried @?= cancelled
  outcome @?= OracleDuplicate receipt
  oracleEffects effects @?= []
  oracleCurrentMembership cancelled @?= oldGeneration
  oraclePendingHeraldAdmission cancelled @?= Nothing
  rejects (HeraldAdmissionCommandRejected HeraldAdmissionTerminal) fixtureHeraldEpoch (activateHeraldCommand ident) cancelled
  rejects (HeraldAdmissionCommandRejected AdmissionEpochAlreadyKnown) fixtureHeraldEpoch (beginHeraldAdmissionCommand manifest anchor) cancelled
  let replacement = heraldAdmissionManifest fixtureSystemId (admissionManifestHeraldId manifest) (F.heraldEpoch 202)
  assertBool "fresh epoch may use cancelled Herald ID" (oraclePendingHeraldAdmission (accepted fixtureHeraldEpoch (beginHeraldAdmissionCommand replacement anchor) cancelled) /= Nothing)

caseFailurePreemption :: Assertion
caseFailurePreemption = do
  let native = raftConfigurationForVoters [fixtureRaftLocalNode] 100000 300000 500000
      bindings = [raftVoterBinding fixtureRaftLocalNode fixtureHeraldEpoch]
      genesis = checked "single voter" (checkOracleGenesis (oracleGenesis fixtureSystemId fixtureMembers profileCatalogueDigest fixtureDescriptors fixtureBootstraps fixtureConfigurationDigest fixtureTopology (deriveInitialProjectionDigest fixtureBootstraps fixtureTopology) bindings native (deriveRaftConfigurationDigest fixtureSystemId bindings native)))
      initial = checked "single voter state" (initialOracle genesis)
      preparing = accepted fixtureHeraldEpoch (beginHeraldAdmissionCommand manifest anchor) initial
      opened = accepted fixtureHeraldEpoch (openHeraldFailureProbeCommand fixtureThirdHeraldEpoch (heraldMembershipGenerationId oldGeneration) (V.voterConfigurationId (V.oracleVoterConfiguration initial))) preparing
      probe = fromJust (oracleActiveFailureProbeFor fixtureThirdHeraldEpoch (heraldMembershipGenerationId oldGeneration) opened)
      (preempted, receipt, entry) = submit fixtureHeraldEpoch (reportHeraldFailureProbeCommand probe (V.voterConfigurationId (V.oracleVoterConfiguration initial)) ProbeUnreachable) opened
      resolution = deriveFailureProbeResolutionId probe RetireFailureProbeTarget
      retired = accepted fixtureHeraldEpoch (retireHeraldEpochCommand resolution fixtureThirdHeraldEpoch) preempted
  oracleReceiptResult receipt @?= OracleAccepted
  oraclePendingHeraldAdmission preempted @?= Nothing
  assertBool "threshold projects cancellation" (any cancelledEvent (appliedEntryProjectionEvents entry))
  oracleCurrentMembership preempted @?= oldGeneration
  oracleRetiredHeralds preempted @?= []
  oracleRetiredHeralds retired @?= [fixtureThirdHeraldEpoch]
  where
    cancelledEvent event = case oracleProjectionEventView event of
      HeraldAdmissionChangedView record -> case admissionRecordPhase record of
        AdmissionCancelled {} -> True
        _ -> False
      _ -> False

caseStampedCuts :: Assertion
caseStampedCuts = do
  let badCuts = [(h, heraldJoinMemberCut (if h == NE.head (heraldMembershipGenerationActiveHeraldEpochs oldGeneration) then 1 else 0) 0 digest) | h <- oldMembers]
  heraldJoinSeal ident 1 anchor (controlIndex 1) badCuts digest digest @?= Left JoinStampedPrefixNotCovered
  assertBool "missing member cannot seal" (isLeft (heraldJoinSeal ident 1 anchor (controlIndex 1) [(NE.head (heraldMembershipGenerationActiveHeraldEpochs oldGeneration), heraldJoinMemberCut 0 0 digest)] digest digest))
  assertBool "newcomer cannot enter old cut" (isLeft (heraldJoinSeal ident 1 anchor (controlIndex 1) ((applicant, heraldJoinMemberCut 0 0 digest) : joinSealMemberCuts seal) digest digest))

caseApplicantAuthority :: Assertion
caseApplicantAuthority = do
  let start = startProcessEpochCommand (processStart (F.processId 203) (F.processEpochId 204) applicant)
  rejects (InactiveHomeHerald applicant) applicant start begun
  let running = accepted applicant start activated
  oracleLiveProcessResidence (F.processEpochId 204) running @?= Just applicant
  length (oracleHeraldCatalogue running) @?= 4

caseCompletedLabelMembership :: Assertion
caseCompletedLabelMembership = do
  let object = globalObjectId 211
      request = oracleClientRequestId fixtureHeraldEpoch 1
      decision = Label.deriveLabelDecisionId fixtureSystemId request
      cut = DomainLabel.homeLabelAcceptanceCut (DomainLabel.firstLabelProcessAcceptancePosition fixtureProcessEpoch) EmptyHeraldPublicationPrefix
      decide = Label.decideLabelCommand decision fixtureProcessEpoch object (ProcessLabel fixtureProcessEpoch, 0) (Just (DomainLabel.initialBootstrapLabelEvidence object fixtureProcessEpoch)) Nothing Nothing DomainLabel.targetVoid cut
      decided = accepted fixtureHeraldEpoch decide fixtureInitialState
      outcome = Label.deriveLabelOutcomeDigest (fromJust (oracleTerminalOutcome decision decided))
      completion reporter = Label.completeLabelDecisionCommand (Label.labelCompletionAttestation decision (controlIndex 1) outcome reporter (heraldMembershipGenerationId oldGeneration))
      completed = accepted fixtureHeraldEpoch (completion fixtureHeraldEpoch) decided
      preparing = accepted fixtureHeraldEpoch (beginHeraldAdmissionCommand manifest (cutFor oldGeneration (oracleGreatestControlIndex completed))) completed
      record = fromJust (oraclePendingHeraldAdmission preparing)
      captured = sealFor record (oracleGreatestControlIndex preparing)
      sealedState = accepted fixtureHeraldEpoch (sealHeraldAdmissionCommand captured) preparing
      acknowledged = foldl' (flip (acceptSeal captured)) sealedState oldMembers
      based = foldl' (flip (oldReady captured)) acknowledged oldMembers
      active = accepted fixtureHeraldEpoch (activateHeraldCommand (admissionRecordId record)) (newReady captured based)
      restored = checked "completed label and new membership" (decodeOracleCheckpoint fixtureCheckedGenesis (oracleCheckpointBytes active))
  restored @?= active
  assertBool "new reporter is active" (applicant `elem` heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership restored))
  rejects (ReporterNotCaptured applicant) applicant (completion applicant) restored
  let rediscovered = accepted fixtureRemoteHeraldEpoch (completion fixtureRemoteHeraldEpoch) restored
  oracleCompletedWorkflowCount rediscovered @?= 1
  oracleLabelRecords rediscovered @?= oracleLabelRecords restored

caseCanonical :: Assertion
caseCanonical = do
  let commands = [BeginHeraldAdmission manifest anchor, SealHeraldAdmission seal, AcceptHeraldJoinSeal ident 1 (joinSealDigest seal), HeraldJoinBaseReady (readyReport seal fixtureHeraldEpoch), HeraldJoinReady (readyReport seal applicant), ActivateHerald ident, CancelHeraldAdmission ident]
  mapM_
    ( \command -> do
        decodeAdmissionCommand (encodeAdmissionCommand command) @?= Right command
        let envelope = envelopeFor 999 (heraldAdmissionCommand command)
            canonical = canonicalizeOracleEnvelope envelope
        decodeCanonicalOracleEnvelope (canonicalOracleEnvelopeBytes canonical) @?= Right canonical
    )
    commands
  let (_, _, entry) = submit fixtureHeraldEpoch (activateHeraldCommand ident) allReady
      canonical = canonicalizeAppliedOracleEntry entry
      (_, rejected, _) = submit fixtureHeraldEpoch (activateHeraldCommand ident) sealed
      canonicalRejected = canonicalizeOracleReceipt rejected
  decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes canonical) @?= Right canonical
  decodeCanonicalOracleReceipt (canonicalOracleReceiptBytes canonicalRejected) @?= Right canonicalRejected

-- These wire-only mirrors let the test change one semantic field while
-- retaining a fully canonical encoding; no checked record constructor escapes.
data RecordManifestClaim = RecordManifestClaim ByteString ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RecordCutClaim = RecordCutClaim ByteString Word64 Word64 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RecordSealClaim = RecordSealClaim ByteString Word64 ByteString Word64 [RecordCutClaim] ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RecordReadyClaim = RecordReadyClaim ByteString Word64 ByteString ByteString Word64 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RecordPhaseClaim = PreparingClaim | SealedClaim | ReadyClaim | ActivatedClaim Word64 ByteString | CancelledClaim Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)
data RecordClaim = RecordClaim ByteString RecordManifestClaim ByteString Word64 Word64 Word64 Word64 RecordPhaseClaim (Maybe RecordSealClaim) [ByteString] [RecordReadyClaim] (Maybe RecordReadyClaim)
  deriving stock (Generic)
  deriving anyclass (Serialize)

caseMalformedRecordClosure :: Assertion
caseMalformedRecordClosure = do
  let cancelledFrom state = accepted fixtureHeraldEpoch (cancelHeraldAdmissionCommand ident) state
      records = map (fromJust . oracleHeraldAdmission ident) [begun, sealed, allAccepted, allReady, activated, cancelledFrom begun, cancelledFrom sealed, cancelledFrom allReady]
      readClaim record = checked "decode canonical record mutation fixture" (S.decode (encodeAdmissionRecord record) :: Either String (ByteString, RecordClaim))
      withoutSeal (RecordClaim i m g b c a t p _ acceptors reports newcomer) = RecordClaim i m g b c a t p Nothing acceptors reports newcomer
      withUnboundRecipe (RecordClaim i m g b c a t p supplied acceptors reports newcomer) =
        let replaceRecipe (RecordSealClaim sid attempt cut prefix cuts contributionBytes _) = RecordSealClaim sid attempt cut prefix cuts contributionBytes (heraldJoinDigestBytes digest)
         in RecordClaim i m g b c a t p (replaceRecipe <$> supplied) acceptors reports newcomer
      reject label mutation state = do
        let (tag, record) = readClaim (fromJust (oracleHeraldAdmission ident state))
        assertBool label (isLeft (decodeAdmissionRecord (S.encode (tag, mutation record))))
  mapM_
    ( \record -> do
        let claimBytes = S.encode (readClaim record)
        claimBytes @?= encodeAdmissionRecord record
        decodeAdmissionRecord claimBytes @?= Right record
    )
    records
  reject "activation cannot retain complete readiness without its exact seal" withoutSeal activated
  reject "cancelled readiness still requires its retained seal" withoutSeal (cancelledFrom allReady)
  reject "cancelled seal acceptance still requires its retained seal" withoutSeal (cancelledFrom allAccepted)
  reject "a sealed recipe must derive from its exact contribution and predecessor" withUnboundRecipe sealed
  reject "a cancelled sealed recipe retains the same checked binding" withUnboundRecipe (cancelledFrom sealed)

caseAcceptedVoterFailurePreemption :: Assertion
caseAcceptedVoterFailurePreemption = forM_ [begun, sealed, allAccepted, allReady] $ \predecessor -> do
  let (excluded, certificate, entries) = VF.excludeVoterHost fixtureThirdHeraldEpoch predecessor
      (retired, retirementEntry) = VF.retireExcludedHost certificate excluded
  oracleCurrentMembership excluded @?= oldGeneration
  oraclePendingHeraldAdmission excluded @?= Nothing
  fmap admissionRecordPhase (oracleHeraldAdmission ident excluded) @?= Just (AdmissionCancelled (controlIndex (controlIndexWord64 (oracleGreatestControlIndex predecessor) + 3)))
  fmap V.voterChangePhase (V.oraclePendingVoterChange excluded) @?= Just V.VoterExcludedAwaitingHeraldRetirement
  oraclePendingHeraldAdmission retired @?= Nothing
  oracleRetiredHeralds retired @?= [fixtureThirdHeraldEpoch]
  V.oraclePendingVoterChange retired @?= Nothing
  rejects (VoterCommandRejected V.VoterChangeInProgress) fixtureHeraldEpoch (activateHeraldCommand ident) excluded
  rejects (HeraldAdmissionCommandRejected HeraldAdmissionTerminal) fixtureHeraldEpoch (activateHeraldCommand ident) retired
  mapM_ (\entry -> canonicalAppliedOracleEntryValue (checked "failure admission entry" (decodeCanonicalAppliedOracleEntry (canonicalAppliedOracleEntryBytes (canonicalizeAppliedOracleEntry entry)))) @?= entry) (entries <> [retirementEntry])

caseFreshEpochAfterVoterRetirement :: Assertion
caseFreshEpochAfterVoterRetirement = do
  let retired = VF.retireVoterHost fixtureThirdHeraldEpoch allReady
      survivorGeneration = oracleCurrentMembership retired
      freshEpoch = F.heraldEpoch 202
      formerMember = case [member | member <- checkedOracleActiveHeralds fixtureCheckedGenesis, heraldMemberEpoch member == fixtureThirdHeraldEpoch] of
        [member] -> member
        _ -> error "fixture missing retired Herald identity"
      freshManifest = heraldAdmissionManifest fixtureSystemId (heraldMemberId formerMember) freshEpoch
      freshAnchor = cutFor survivorGeneration (oracleGreatestControlIndex retired)
      preparing = accepted fixtureHeraldEpoch (beginHeraldAdmissionCommand freshManifest freshAnchor) retired
      retained = fromJust (oraclePendingHeraldAdmission preparing)
      freshId = admissionRecordId retained
      freshSeal = sealFor retained (oracleGreatestControlIndex preparing)
      recaptured = accepted fixtureHeraldEpoch (sealHeraldAdmissionCommand freshSeal) preparing
      survivors = NE.toList (heraldMembershipGenerationActiveHeraldEpochs survivorGeneration)
      acknowledged = foldl' (flip (acceptSeal freshSeal)) recaptured survivors
      based = foldl' (flip (oldReady freshSeal)) acknowledged survivors
      newcomerReady = accepted fixtureHeraldEpoch (reportHeraldJoinReadyCommand (readyReport freshSeal freshEpoch)) based
      active = accepted fixtureHeraldEpoch (activateHeraldCommand freshId) newcomerReady
  assertBool "fresh epoch is active after its own sealed join" (freshEpoch `elem` heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership active))
  assertBool "retired epoch never returns" (not (fixtureThirdHeraldEpoch `elem` heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership active)))
  assertBool "fresh Herald admission grants no voter role" (all ((/= freshEpoch) . raftVoterBindingHeraldEpoch) (V.voterConfigurationBindings (V.oracleVoterConfiguration active)))
  fmap admissionRecordPhase (oracleHeraldAdmission ident active) @?= Just (AdmissionCancelled (controlIndex (controlIndexWord64 (oracleGreatestControlIndex allReady) + 3)))
  let (_, staleActivation, _) = submit fixtureHeraldEpoch (activateHeraldCommand ident) active
  oracleReceiptResult staleActivation @?= OracleRejected (HeraldAdmissionCommandRejected HeraldAdmissionTerminal)
