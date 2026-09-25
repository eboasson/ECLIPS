module OracleProjectionMembershipProperties
  ( tests,
  )
where

import Control.Exception (bracket, evaluate)
import Control.Monad (forM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    SystemId,
    controlIndex,
    controlIndexWord64,
    mkSystemId,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership
  ( FailureProbeResolution (RetireFailureProbeTarget),
    FailureProbeResolutionId,
    HeraldMembershipGeneration,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
    heraldMembershipLineageRetiredHeraldEpochs,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ExplicitAdministrativeEnd),
  )
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Domain.Startup (HeraldMember (heraldMemberEpoch))
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry, canonicalAppliedOracleEntryValue, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label qualified as Live
import Eclips.Oracle.Projection qualified as Entry
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GenesisFixtures
  ( fixtureCheckedOracleGenesis,
    fixtureHeraldRetirementReason,
    fixtureIdentifierBytes,
    fixtureLocalMember,
    fixtureOracleProjectionState,
    fixtureRemoteMember,
  )
import GenesisFixtures qualified as Fixtures
import System.Mem (performGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Oracle projection membership"
    [ testCase "contiguous watch evidence releases preceding projection owners before history audit" caseProjectionOwnerRetention,
      testCase "repeated checked bases release preceding projection owners" caseReplayBaseOwnerRetention,
      testCase "a checked zero base preserves subsequent voter initialization" caseReplayBaseVoterInitialization,
      QC.testProperty "checked bases and suffixes agree with full label-history replay at every generated cut" propReplayBases,
      testCase "checked bases retain mixed canonical and detached membership evidence" caseReplayBaseMixedMembership,
      testCase "checked bases retain old Start and End witnesses and historical process authority" caseReplayBaseProcessHistory,
      testCase "checked bases reject corrupt capture and detect later semantic corruption" caseReplayBaseTamper,
      QC.testProperty "transferred checked bases preserve receiver identity and agree with canonical prefix replay" propProjectionBaseTransfers,
      QC.testProperty "semantic bases reconstruct local label predecessors after raw history reclamation" propProjectedLabelPatchBase,
      testCase "transferred bases retain exact detached membership evidence" caseProjectionBaseMixedMembership,
      testCase "projection base installation requires fresh matching genesis" caseProjectionBaseInstallAdmission,
      testCase "transferred zero bases preserve subsequent voter initialization" caseProjectionBaseZero,
      testCase "projection base captures release donor and previous base owners" caseProjectionBaseCaptureRetention,
      QC.testProperty "covered canonical prefixes preserve facts, exact pins and transferred sparse evidence" propCoveredCanonicalPrefix,
      QC.testProperty "exclusive tail floors preserve contiguous bytes below semantic bases alongside sparse pins" propCoveredCanonicalTail,
      testCase "canonical tail retention rejects unavailable floors and preserves the post-base suffix" caseCoveredCanonicalTailAdmission,
      testCase "canonical tail retention releases preceding projection owners" caseCoveredCanonicalTailOwnerRetention,
      QC.testProperty "current prefix reclamation agrees with checked replay and same-cursor releases" propCurrentCanonicalReclamation,
      testCase "current prefix reclamation checks supplied pins and canonical tails" caseCurrentCanonicalReclamationAdmission,
      testCase "current prefix reclamation releases preceding owners before any history audit" caseCurrentCanonicalOwnerRetention,
      testCase "covered prefixes distinguish exact pins, conflicts and discarded evidence" caseCoveredCanonicalClassification,
      testCase "covered prefixes retain detached membership and independent semantic and digest coordinates" caseCoveredCanonicalScalars,
      testCase "covered canonical reclamation releases prior projection owners" caseCoveredCanonicalOwnerRetention,
      testCase "transferred covered prefixes retain only explicitly pinned canonical evidence" caseCoveredCanonicalEvidenceRetention,
      testCase "genesis generation and exact history are derived once" caseGenesis,
      testCase "the exact next retirement successor advances membership and resident Ends" caseValidSuccessor,
      testCase "successive contractions retain exact lineage and every resident End" caseRepeatedSuccessor,
      testCase "an exact retained membership batch is idempotent" caseDuplicate,
      testCase "stale and conflicting advances retain their exact rejection" caseRejectedAdvances,
      testCase "a successor cannot change the immutable Herald catalogue" caseCatalogueMismatch,
      testCase "retirement rejects an ordinary process-End reason" caseEndReasonMismatch,
      testCase "retiring the local Herald remains a valid self-fence input" caseLocalRetired,
      testCase "whole-state validation replays retained membership evidence" caseValidationTamper
    ]

-- A fresh rejected command advances the watch prefix without changing any
-- semantic projection facts. Force only the successor constructor before
-- attaching its weak witness: forcing the cold history map or full invariant
-- before collection would hide a deferred-map predecessor chain.
{-# NOINLINE projectRetentionEntry #-}
projectRetentionEntry :: CanonicalAppliedOracleEntry -> OracleProjection.State -> IO OracleProjection.State
projectRetentionEntry entry state =
  evaluate (OracleProjection.commitAppliedEntry (checked "project retention entry" (OracleProjection.prepareAppliedEntry entry state)))

{-# NOINLINE rejectedProjectionOwnerHistory #-}
rejectedProjectionOwnerHistory :: Bool -> Int -> IO (OracleProjection.State, [Weak OracleProjection.State])
rejectedProjectionOwnerHistory rebase count = loop 1 (Live.initialOracleState fixtureCheckedOracleGenesis) fixtureOracleProjectionState []
  where
    loop ordinal oracle projection witnesses
      | ordinal > count = pure (projection, reverse witnesses)
      | otherwise = do
          let command = Voter.cancelVoterChangeCommand (checked "unknown retention voter change" (Voter.mkVoterChangeId (controlIndex 999999)))
              envelope = Live.oracleEnvelope (oracleClientRequestId localEpoch (fromIntegral ordinal)) Nothing localEpoch command
              (nextOracle, outcome) = Live.submitOracleState envelope oracle
              entry = case Live.oracleSubmissionOutcomeView outcome of
                Live.OracleSubmissionCommittedView _ supplied -> canonicalizeAppliedOracleEntry supplied
                other -> error ("expected canonical rejection: " <> show other)
          applied <- projectRetentionEntry entry projection
          appliedWitness <- mkWeakPtr applied Nothing
          successor <-
            if rebase
              then evaluate (checked "advance retention base" (OracleProjection.advanceReplayBase applied))
              else pure applied
          witness <- mkWeakPtr successor Nothing
          let retainedWitnesses = if rebase then witness : appliedWitness : witnesses else witness : witnesses
          loop (ordinal + 1) nextOracle successor retainedWitnesses

caseProjectionOwnerRetention :: Assertion
caseProjectionOwnerRetention = do
  (projection, witnesses) <- rejectedProjectionOwnerHistory False 64
  bracket (newStablePtr projection) freeStablePtr $ \root -> do
    performGC
    alive <- forM witnesses (fmap (maybe False (const True)) . deRefWeak)
    assertEqual "only the current projection owner remains reachable" (replicate 63 False <> [True]) alive
    retained <- deRefStablePtr root
    assertEqual "the current prefix still covers every received entry" (controlIndex 64) (OracleProjection.oracleViewControlIndex (OracleProjection.oracleView retained))
    assertBool "the first exact watch entry remains available" (case OracleProjection.appliedEntryEvidence (controlIndex 1) retained of Just _ -> True; Nothing -> False)
    assertValid retained

caseReplayBaseOwnerRetention :: Assertion
caseReplayBaseOwnerRetention = do
  (projection, witnesses) <- rejectedProjectionOwnerHistory True 64
  bracket (newStablePtr projection) freeStablePtr $ \root -> do
    performGC
    alive <- forM witnesses (fmap (maybe False (const True)) . deRefWeak)
    assertEqual "only the current base owner remains reachable, including immediate pre-capture owners" (replicate 127 False <> [True]) alive
    retained <- deRefStablePtr root
    assertEqual "each capture moves the replay base to its checked tip" (controlIndex 64) (OracleProjection.replayBaseControlIndex retained)
    assertBool "rebasing does not erase old exact watch evidence" (case OracleProjection.appliedEntryEvidence (controlIndex 1) retained of Just _ -> True; Nothing -> False)
    assertEqual "base replay and independent genesis replay yield the same witness" (OracleProjection.validateStateFromGenesis retained) (OracleProjection.validateState retained)
    assertValid retained

caseReplayBaseVoterInitialization :: Assertion
caseReplayBaseVoterInitialization = do
  let oracle = Live.initialOracleState fixtureCheckedOracleGenesis
      initialize = OracleProjection.initializeOracleVoters (Voter.oracleVoterConfiguration oracle) (Voter.oracleReplicaRegistrations oracle)
      captured = checked "capture zero base before voters" (OracleProjection.advanceReplayBase fixtureOracleProjectionState)
      initialized = checked "initialize voters after zero base" (initialize captured)
      reference = checked "initialize voters without zero base" (initialize fixtureOracleProjectionState)
      next = applyCanonical (firstEntry (rejectedCanonicalHistory 1)) initialized
      nextReference = applyCanonical (firstEntry (rejectedCanonicalHistory 1)) reference
  assertEqual "zero-base voter initialization keeps the original cursor" (controlIndex 0) (projectionCursor initialized)
  assertEqual "zero-base voter initialization keeps the replay coordinate" (controlIndex 0) (OracleProjection.replayBaseControlIndex initialized)
  assertBool "zero-base voter initialization preserves the semantic view" (OracleProjection.oracleView initialized == OracleProjection.oracleView reference)
  assertEqual "initialized zero-base witness agrees with genesis replay" (OracleProjection.validateStateFromGenesis initialized) (OracleProjection.validateState initialized)
  assertEqual "the first canonical suffix has the same witness" (OracleProjection.validateState nextReference) (OracleProjection.validateState next)
  assertValid initialized
  assertValid next

-- Select both an unfinished/finished workflow cut and a later suffix. The
-- receiver has a different local identity but the same immutable genesis.
propProjectionBaseTransfers :: QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
propProjectionBaseTransfers (QC.NonNegative historySeed) (QC.NonNegative cutSeed) =
  QC.counterexample ("transferred control cut " <> show cut)
    $ QC.conjoin
      [ OracleProjection.projectionBaseControlIndex capture QC.=== controlIndex (fromIntegral cut),
        OracleProjection.encodeProjectionBase uncheckedCapture QC.=== OracleProjection.encodeProjectionBase capture,
        fmap OracleProjection.encodeProjectionBase (OracleProjection.decodeProjectionBase remoteProjection (OracleProjection.encodeProjectionBase uncheckedCapture)) QC.=== Right (OracleProjection.encodeProjectionBase capture),
        OracleProjection.replayBaseControlIndex installed QC.=== controlIndex (fromIntegral cut),
        OracleProjection.oracleViewLocalHeraldEpoch (OracleProjection.oracleView final) QC.=== remoteEpoch,
        QC.counterexample "receiver semantic facts differ from full canonical replay" (OracleProjection.oracleView final == OracleProjection.oracleView reference),
        OracleProjection.validateState final QC.=== OracleProjection.validateState reference,
        OracleProjection.validateStateFromGenesis final QC.=== OracleProjection.validateState final,
        QC.conjoin
          [ OracleProjection.appliedEntryEvidence (Entry.appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)) final QC.=== Just entry
          | entry <- history
          ],
        QC.conjoin
          [ OracleProjection.oracleProjectionReleasedLabelAt (controlIndex index) replayObject final
              QC.=== OracleProjection.oracleProjectionReleasedLabelAt (controlIndex index) replayObject reference
          | index <- [0 .. controlIndexWord64 (projectionCursor final)]
          ]
      ]
  where
    history = labelReplayHistory (fromIntegral (2 + historySeed `mod` 7))
    cut = cutSeed `mod` (length history + 1)
    (prefix, suffix) = splitAt cut history
    donor = foldl' (flip applyCanonical) fixtureOracleProjectionState prefix
    capture = checked "capture transferable projection base" (OracleProjection.captureProjectionBase donor)
    uncheckedCapture = checked "capture admitted projection without redundant diagnostics" (OracleProjection.captureProjectionBaseWithDiagnostics DiagnosticChecksDisabled donor)
    installed = checked "install transferable projection base" (OracleProjection.installProjectionBase capture remoteProjection)
    final = foldl' (flip applyCanonical) installed suffix
    reference = foldl' (flip applyCanonical) remoteProjection history

-- The reference executes the ordinary local patch prepare/release path for
-- every successful Oracle decision. The candidate adopts only the semantic
-- predecessor at an arbitrary cut after discarding all raw prefix entries.
propProjectedLabelPatchBase :: QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
propProjectedLabelPatchBase (QC.NonNegative historySeed) (QC.NonNegative cutSeed) =
  QC.counterexample ("label predecessor cut " <> show cut)
    $ QC.conjoin
      [ LabelPatch.captureCheckedLabelPatchBase donorPatches QC.=== Right base,
        LabelPatch.captureCheckedLabelPatchBase adoptedPatches QC.=== LabelPatch.captureCheckedLabelPatchBase referencePatches,
        LabelPatch.labelPatchBaseFromProjection finalProjection QC.=== LabelPatch.labelPatchBaseFromProjection referenceProjection,
        QC.counterexample "prefix bytes remain discarded" (all ((> controlIndex (fromIntegral cut)) . fst) (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness finalProjection))),
        Map.size (LabelPatch.retainedLabelPatches adoptedPatches) QC.=== length [() | entry <- suffix, releasedAt entry finalProjection /= Nothing],
        OracleProjection.validateState finalProjection QC.=== Right (OracleProjection.stateWitness finalProjection)
      ]
  where
    history = labelReplayHistory (fromIntegral (2 + historySeed `mod` 7))
    cut = cutSeed `mod` (length history + 1)
    (prefix, suffix) = splitAt cut history
    generator = checked "label predecessor generator" (IdGenerator.initialState Fixtures.fixtureGeneratorSeed)
    initial = (fixtureOracleProjectionState, LabelPatch.initialState, generator)
    (donorProjection, donorPatches, _) = foldl' replay initial prefix
    compacted = checked "reclaim label predecessor source" (OracleProjection.advanceReplayBase donorProjection >>= OracleProjection.reclaimAppliedPrefixThroughBase Set.empty Nothing)
    base = LabelPatch.labelPatchBaseFromProjection compacted
    imported = checked "install semantic label predecessor" (LabelPatch.installCheckedLabelPatchBase (projectionCursor compacted) base LabelPatch.initialState)
    (finalProjection, adoptedPatches, _) = foldl' replay (compacted, imported, generator) suffix
    (referenceProjection, referencePatches, _) = foldl' replay initial history
    releasedAt entry projection = case [ workflow
                                       | (_, workflow) <- OracleProjection.projectedLabelWorkflows projection,
                                         Just (OracleProjection.ProjectedLabelReleased index _ _) <- [OracleProjection.projectedLabelWorkflowTerminal workflow],
                                         index == Entry.appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)
                                       ] of
      [] -> Nothing
      [workflow] -> Just workflow
      _ -> error "one label decision per generated command"
    replay (projection, patches, currentGenerator) entry =
      let nextProjection = applyCanonical entry projection
       in case releasedAt entry nextProjection of
            Nothing -> (nextProjection, patches, currentGenerator)
            Just workflow ->
              let facts = maybe (error "released label has no prepared facts") id (OracleProjection.projectedLabelWorkflowPreparedFacts workflow)
                  digest = maybe (error "released label has no prepared digest") id (OracleProjection.projectedLabelWorkflowPreparedDigest workflow)
                  specification = LabelPatch.patchSpecification facts digest LabelPatch.ordinaryControlledPatchContext
                  prepared = checked "prepare reference local label" (LabelPatch.prepareLabelPatch specification currentGenerator patches)
                  (pending, _) = LabelPatch.commitLabelPatch prepared
                  released = checked "release reference local label" (LabelPatch.preparePatchRelease specification (Label.preparedLabelFactsResolveIndex facts) currentGenerator pending)
                  (nextPatches, nextGenerator) = LabelPatch.commitPatchRelease released
               in (nextProjection, nextPatches, nextGenerator)

remoteProjection :: OracleProjection.State
remoteProjection = OracleProjection.initialState genesis (Fixtures.fixtureCheckedInitialBootstrapsFor genesis)
  where
    initial = Fixtures.fixtureDeploymentAt fixtureRemoteMember
    members = Genesis.checkedActiveHeralds Fixtures.fixtureStep14CheckedGenesis
    genesis =
      checked "remote projection genesis"
        $ Genesis.checkHeraldGenesis
          initial
            { Genesis.deploymentActiveHeralds = members,
              Genesis.deploymentOracleGenesis =
                (Genesis.deploymentOracleGenesis initial) {Genesis.oracleGenesisActiveHeralds = members}
            }

caseProjectionBaseMixedMembership :: Assertion
caseProjectionBaseMixedMembership = do
  let entries = rejectedCanonicalHistory 3
      first = applyCanonical (firstEntry entries) fixtureOracleProjectionState
      (generation, ends, retired) = retireProjection (controlIndex 2) remoteEpoch first
      capture = checked "capture mixed transferable base" (OracleProjection.captureProjectionBase retired)
      installed = checked "install mixed transferable base" (OracleProjection.installProjectionBase capture remoteProjection)
      final = applyCanonical (entries !! 2) installed
      remoteFirst = applyCanonical (firstEntry entries) remoteProjection
      (_, _, remoteRetired) = retireProjection (controlIndex 2) remoteEpoch remoteFirst
      reference = applyCanonical (entries !! 2) remoteRetired
  duplicate <- checkedIO "transferred old detached membership" (OracleProjection.prepareMembershipAdvance (controlIndex 2) generation ends final)
  assertEqual "the detached exact evidence remains duplicate" OracleProjection.MembershipAdvanceExactDuplicate (OracleProjection.preparedMembershipAdvanceDisposition duplicate)
  assertEqual "the local identity remains the receiver even when canonically retired" remoteEpoch (OracleProjection.oracleViewLocalHeraldEpoch (OracleProjection.oracleView final))
  assertBool "mixed semantic facts equal receiver replay" (OracleProjection.oracleView final == OracleProjection.oracleView reference)
  assertEqual "mixed exact evidence equals receiver replay" (OracleProjection.validateState reference) (OracleProjection.validateState final)
  assertEqual "mixed transfer has independent genesis evidence" (OracleProjection.validateStateFromGenesis final) (OracleProjection.validateState final)

caseProjectionBaseInstallAdmission :: Assertion
caseProjectionBaseInstallAdmission = do
  let entry = firstEntry (rejectedCanonicalHistory 1)
      donor = applyCanonical entry fixtureOracleProjectionState
      capture = checked "capture installation admission fixture" (OracleProjection.captureProjectionBase donor)
      differentGenesis = OracleProjection.initialState Fixtures.fixtureCheckedGenesis (Fixtures.fixtureCheckedInitialBootstrapsFor Fixtures.fixtureCheckedGenesis)
      oracle = Live.initialOracleState fixtureCheckedOracleGenesis
      initializedVoters = checked "install admission voter fixture" (OracleProjection.initializeOracleVoters (Voter.oracleVoterConfiguration oracle) (Voter.oracleReplicaRegistrations oracle) remoteProjection)
      expect problem outcome = case outcome of
        Left actual -> assertEqual "installation rejects incompatible receiver" problem actual
        Right _ -> assertFailure "incompatible projection base was installed"
  expect (OracleProjection.ProjectionBaseReceiverNotFresh (controlIndex 1)) (OracleProjection.installProjectionBase capture (applyCanonical entry remoteProjection))
  expect OracleProjection.ProjectionBaseGenesisMismatch (OracleProjection.installProjectionBase capture differentGenesis)
  expect OracleProjection.ProjectionBaseGenesisMismatch (OracleProjection.installProjectionBase capture initializedVoters)
  let bad = OracleProjection.replaceCurrentHeraldMembershipForInvariantTest (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView fixtureOracleProjectionState)) (thirdOf3 (retireProjection (controlIndex 2) remoteEpoch donor))
  case OracleProjection.captureProjectionBase bad of
    Left OracleProjection.ProjectionCurrentHeraldMembershipMismatch -> pure ()
    Left other -> assertFailure ("unexpected corrupt capture rejection: " <> show other)
    Right _ -> assertFailure "corrupt projection was captured"

caseProjectionBaseZero :: Assertion
caseProjectionBaseZero = do
  let capture = checked "capture transferable zero base" (OracleProjection.captureProjectionBase fixtureOracleProjectionState)
      installed = checked "install transferable zero base" (OracleProjection.installProjectionBase capture remoteProjection)
      oracle = Live.initialOracleState fixtureCheckedOracleGenesis
      initialize = OracleProjection.initializeOracleVoters (Voter.oracleVoterConfiguration oracle) (Voter.oracleReplicaRegistrations oracle)
      initialized = checked "initialize voters after transfer" (initialize installed)
      reference = checked "initialize remote voters without transfer" (initialize remoteProjection)
  assertBool "zero transfer preserves the receiver" (installed == remoteProjection)
  assertBool "voter initialization agrees after zero transfer" (initialized == reference)
  assertEqual "zero transfer genesis audit agrees" (OracleProjection.validateStateFromGenesis initialized) (OracleProjection.validateState initialized)

{-# NOINLINE captureProjectionOwnerHistory #-}
captureProjectionOwnerHistory :: IO (OracleProjection.ProjectionBaseCapture, [Weak OracleProjection.State])
captureProjectionOwnerHistory = do
  (donor, witnesses) <- rejectedProjectionOwnerHistory True 32
  capture <- evaluate (checked "capture owner history" (OracleProjection.captureProjectionBase donor))
  pure (capture, witnesses)

caseProjectionBaseCaptureRetention :: Assertion
caseProjectionBaseCaptureRetention = do
  (capture, witnesses) <- captureProjectionOwnerHistory
  bracket (newStablePtr capture) freeStablePtr $ \root -> do
    performGC
    alive <- forM witnesses (fmap (maybe False (const True)) . deRefWeak)
    assertEqual "no donor or previous replay-base owner is reachable from the capture" (replicate 64 False) alive
    retained <- deRefStablePtr root
    let installed = checked "install retained capture" (OracleProjection.installProjectionBase retained remoteProjection)
    assertEqual "the independent capture retains its cut" (controlIndex 32) (projectionCursor installed)
    assertBool "separate exact archives remain retained" (case OracleProjection.appliedEntryEvidence (controlIndex 1) installed of Just _ -> True; Nothing -> False)
    assertEqual "the capture still installs independently checked semantics" (OracleProjection.validateStateFromGenesis installed) (OracleProjection.validateState installed)

thirdOf3 :: (a, b, c) -> c
thirdOf3 (_, _, value) = value

-- A checked base is an alternative replay origin, not permission to forget
-- exact command evidence or historical authority. The reference never rebases.
propReplayBases :: QC.NonNegative Int -> [Bool] -> QC.Property
propReplayBases (QC.NonNegative seed) suppliedCaptures =
  QC.conjoin (map checkPrefix prefixes)
  where
    history = labelReplayHistory (fromIntegral (2 + seed `mod` 7))
    initial = fixtureOracleProjectionState
    capturedInitial = checked "capture genesis base" (OracleProjection.advanceReplayBase initial)
    prefixes =
      scanl
        advance
        (initial, capturedInitial, controlIndex 0, [])
        (zip history (cycle (True : suppliedCaptures)))
    advance (reference, candidate, base, entries) (entry, capture) =
      let nextReference = applyCanonical entry reference
          nextCandidate = applyCanonical entry candidate
          nextBase = if capture then projectionCursor nextCandidate else base
          captured = if capture then checked "capture generated base" (OracleProjection.advanceReplayBase nextCandidate) else nextCandidate
       in (nextReference, captured, nextBase, entries <> [entry])
    checkPrefix (reference, candidate, expectedBase, entries) =
      QC.counterexample ("at control index " <> show (projectionCursor reference) <> ", replay base " <> show expectedBase)
        $ QC.conjoin
          [ QC.counterexample "the semantic view changed" (OracleProjection.oracleView reference == OracleProjection.oracleView candidate),
            OracleProjection.validateState candidate QC.=== OracleProjection.validateState reference,
            OracleProjection.validateStateFromGenesis candidate QC.=== OracleProjection.validateState candidate,
            OracleProjection.replayBaseControlIndex candidate QC.=== expectedBase,
            projectionCursor candidate QC.=== projectionCursor reference,
            QC.conjoin (map (checkRetained candidate reference) entries),
            QC.conjoin
              [ OracleProjection.oracleProjectionReleasedLabelAt (controlIndex index) replayObject candidate
                  QC.=== OracleProjection.oracleProjectionReleasedLabelAt (controlIndex index) replayObject reference
              | index <- [0 .. controlIndexWord64 (projectionCursor reference)]
              ],
            QC.counterexample "old conflicting canonical evidence changed classification"
              $ null entries
                || (rejectsConflict candidate && rejectsConflict reference)
          ]
    checkRetained candidate reference entry =
      let value = canonicalAppliedOracleEntryValue entry
          index = Entry.appliedEntryControlIndex value
          prepared = checked "old exact canonical replay" (OracleProjection.prepareAppliedEntry entry candidate)
       in QC.conjoin
            [ OracleProjection.appliedEntryEvidence index candidate QC.=== Just entry,
              OracleProjection.appliedEntryEvidence index candidate QC.=== OracleProjection.appliedEntryEvidence index reference,
              OracleProjection.preparedAppliedEntryClassification prepared QC.=== OracleProjection.AppliedEntryExactDuplicate index,
              QC.counterexample "exact replay changed the rebased owner" (OracleProjection.commitAppliedEntry prepared == candidate),
              case Entry.appliedEntryCommand value of
                Nothing -> QC.property True
                Just command ->
                  OracleProjection.appliedEntryForRequest (Entry.appliedEntryRequestId command) candidate
                    QC.=== OracleProjection.appliedEntryForRequest (Entry.appliedEntryRequestId command) reference
            ]
    rejectsConflict state = case OracleProjection.prepareAppliedEntry (firstEntry (rejectedCanonicalHistory 1)) state of
      Left (OracleProjection.AppliedEntryUnequalConflictFault index) -> index == controlIndex 1
      _ -> False

-- Completed and still-pending label workflows straddle arbitrary base cuts.
-- An unrelated rejected command between Decide and Complete ensures suffix
-- replay also preserves semantic facts across controls with no semantic event.
labelReplayHistory :: Word64 -> [CanonicalAppliedOracleEntry]
labelReplayHistory count = go 0 (Live.initialOracleState fixtureCheckedOracleGenesis)
  where
    go generation oracle
      | generation >= count = []
      | otherwise =
          let ordinal = 3 * generation + 1
              request = oracleClientRequestId localEpoch ordinal
              ident = Live.deriveLabelDecisionId (OracleProjection.oracleViewSystemId (OracleProjection.oracleView fixtureOracleProjectionState)) request
              initialEvidence = if generation == 0 then Just (Label.initialBootstrapLabelEvidence replayObject replayCaller) else Nothing
              acceptance = checked "label replay acceptance" (Label.mkLabelProcessAcceptancePosition replayCaller (generation + 1))
              command = Live.decideLabelCommand ident replayCaller replayObject (ProcessLabel replayCaller, generation) initialEvidence Nothing Nothing (Label.targetProcess replayCaller) (Label.homeLabelAcceptanceCut acceptance EmptyHeraldPublicationPrefix)
              (opened, decided) = commitHistoryCommand ordinal command oracle
              (interleaved, rejected) = commitHistoryCommand (ordinal + 1) rejectedCommand opened
              decision = maybe (error "replay history has no pending decision") id (Live.oracleOpenDecision ident interleaved)
              terminal = maybe (error "replay history has no prepared terminal") id (Live.oracleTerminalOutcome ident interleaved)
              completion = Live.completeLabelDecisionCommand (Live.labelCompletionAttestation ident (Live.liveDecisionRequestControlIndex decision) (Live.deriveLabelOutcomeDigest terminal) localEpoch (Live.liveDecisionMembershipGeneration decision))
              (completed, completedEntry) = commitHistoryCommand (ordinal + 2) completion interleaved
           in if generation + 1 == count
                then [decided, rejected]
                else decided : rejected : completedEntry : go (generation + 1) completed

replayObject :: Identity.GlobalObjectId
replayObject = checked "replay object" (Identity.mkGlobalObjectId (fixtureIdentifierBytes 0xfb))

replayCaller :: Identity.ProcessEpochId
replayCaller = case [process | process <- OracleProjection.projectedProcessEpochs fixtureOracleProjectionState, OracleProjection.oracleViewProcessResidence process (OracleProjection.oracleView fixtureOracleProjectionState) == Just localEpoch] of
  process : _ -> process
  [] -> error "replay fixture has no local process"

commitHistoryCommand :: Word64 -> Live.OracleCommand -> Live.OracleState -> (Live.OracleState, CanonicalAppliedOracleEntry)
commitHistoryCommand ordinal command oracle = case Live.oracleSubmissionOutcomeView outcome of
  Live.OracleSubmissionCommittedView _ entry -> (next, canonicalizeAppliedOracleEntry entry)
  other -> error ("expected committed replay history entry: " <> show other)
  where
    (next, outcome) = Live.submitOracleState (Live.oracleEnvelope (oracleClientRequestId localEpoch ordinal) Nothing localEpoch command) oracle

rejectedCommand :: Live.OracleCommand
rejectedCommand = Voter.cancelVoterChangeCommand (checked "unknown replay voter change" (Voter.mkVoterChangeId (controlIndex 999999)))

rejectedCanonicalHistory :: Word64 -> [CanonicalAppliedOracleEntry]
rejectedCanonicalHistory count = go 1 (Live.initialOracleState fixtureCheckedOracleGenesis)
  where
    go ordinal oracle
      | ordinal > count = []
      | otherwise =
          let (next, entry) = commitHistoryCommand ordinal rejectedCommand oracle
           in entry : go (ordinal + 1) next

applyCanonical :: CanonicalAppliedOracleEntry -> OracleProjection.State -> OracleProjection.State
applyCanonical entry state = OracleProjection.commitAppliedEntry (checked "project canonical history" (OracleProjection.prepareAppliedEntry entry state))

projectionCursor :: OracleProjection.State -> ControlIndex
projectionCursor = OracleProjection.oracleViewControlIndex . OracleProjection.oracleView

caseReplayBaseMixedMembership :: Assertion
caseReplayBaseMixedMembership = do
  let entries = rejectedCanonicalHistory 5
      first = applyCanonical (firstEntry entries) fixtureOracleProjectionState
      (remoteGeneration, remoteEnds, remoteRetired) = retireProjection (controlIndex 2) remoteEpoch first
      firstBase = checked "capture detached membership base" (OracleProjection.advanceReplayBase remoteRetired)
      canonicalSuffix = applyCanonical (entries !! 2) firstBase
      (localGeneration, localEnds, localRetired) = retireProjection (controlIndex 4) localEpoch canonicalSuffix
      secondBase = checked "capture mixed membership base" (OracleProjection.advanceReplayBase localRetired)
      final = applyCanonical (entries !! 4) secondBase
      referenceAfterCanonical = applyCanonical (entries !! 2) remoteRetired
      (_, _, referenceRetired) = retireProjection (controlIndex 4) localEpoch referenceAfterCanonical
      reference = applyCanonical (entries !! 4) referenceRetired
  assertEqual "the first base includes a detached retirement" (controlIndex 2) (OracleProjection.replayBaseControlIndex firstBase)
  assertEqual "the second base includes canonical and detached evidence" (controlIndex 4) (OracleProjection.replayBaseControlIndex final)
  assertEqual "a retained suffix advances the original watch cursor" (controlIndex 5) (projectionCursor final)
  assertBool "mixed replay produces the same semantic view" (OracleProjection.oracleView final == OracleProjection.oracleView reference)
  assertEqual "mixed replay produces the same witness" (OracleProjection.validateState reference) (OracleProjection.validateState final)
  assertEqual "genesis remains an independent mixed-evidence reference" (OracleProjection.validateStateFromGenesis final) (OracleProjection.validateState final)
  mapM_
    ( \(index, generation, ends) -> do
        duplicate <- checkedIO "old detached membership duplicate" (OracleProjection.prepareMembershipAdvance index generation ends final)
        assertEqual "old detached evidence remains exact" OracleProjection.MembershipAdvanceExactDuplicate (OracleProjection.preparedMembershipAdvanceDisposition duplicate)
        assertBool "old detached replay preserves the current base and authority" (OracleProjection.commitMembershipAdvance duplicate == final)
    )
    [(controlIndex 2, remoteGeneration, remoteEnds), (controlIndex 4, localGeneration, localEnds)]
  assertEqual "the first canonical entry is still available below both bases" (Just (firstEntry entries)) (OracleProjection.appliedEntryEvidence (controlIndex 1) final)
  assertEqual "detached evidence does not invent a canonical entry" Nothing (OracleProjection.appliedEntryEvidence (controlIndex 2) final)
  assertValid final

firstEntry :: [entry] -> entry
firstEntry (entry : _) = entry
firstEntry [] = error "expected nonempty canonical replay history"

propCoveredCanonicalPrefix :: QC.NonNegative Int -> QC.NonNegative Int -> [Bool] -> QC.Property
propCoveredCanonicalPrefix (QC.NonNegative historySeed) (QC.NonNegative cutSeed) suppliedPins =
  QC.counterexample ("covered control cut " <> show through <> ", pins " <> show pins)
    $ QC.conjoin
      [ QC.counterexample "covered-prefix semantic facts changed" (OracleProjection.oracleView final == OracleProjection.oracleView reference),
        OracleProjection.projectionCanonicalCoveredThrough final QC.=== through,
        OracleProjection.replayBaseControlIndex final QC.=== through,
        OracleProjection.projectionLastSemanticControlIndex final QC.=== OracleProjection.projectionLastSemanticControlIndex reference,
        OracleProjection.projectedOracleStateDigest final QC.=== OracleProjection.projectedOracleStateDigest reference,
        QC.counterexample "sparse projection failed its base/suffix audit" (valid final),
        OracleProjection.validateStateFromGenesis final QC.=== Left (OracleProjection.ProjectionGenesisEvidenceCovered through),
        QC.conjoin
          [ let index = Entry.appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)
                exact = index > through || Set.member index pins
                expected = if exact then OracleProjection.AppliedEntryExactDuplicate index else OracleProjection.AppliedEntryCoveredByBase index
                prepared = checked "classify covered history" (OracleProjection.prepareAppliedEntry entry final)
             in QC.conjoin
                  [ OracleProjection.appliedEntryEvidence index final QC.=== (if exact then Just entry else Nothing),
                    OracleProjection.preparedAppliedEntryClassification prepared QC.=== expected,
                    QC.counterexample "historical replay changed covered state" (OracleProjection.commitAppliedEntry prepared == final)
                  ]
          | entry <- history
          ],
        QC.conjoin
          [ OracleProjection.oracleProjectionReleasedLabelAt (controlIndex index) replayObject final QC.=== OracleProjection.oracleProjectionReleasedLabelAt (controlIndex index) replayObject reference
          | index <- [0 .. controlIndexWord64 (projectionCursor final)]
          ],
        OracleProjection.projectionBaseCanonicalCoveredThrough capture QC.=== through,
        OracleProjection.projectionCanonicalCoveredThrough imported QC.=== through,
        QC.counterexample "transferred sparse base changed receiver semantic facts" (OracleProjection.oracleView imported == OracleProjection.oracleView remoteReference),
        OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness imported) QC.=== OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness final),
        OracleProjection.projectionLastSemanticControlIndex imported QC.=== OracleProjection.projectionLastSemanticControlIndex final,
        OracleProjection.projectedOracleStateDigest imported QC.=== OracleProjection.projectedOracleStateDigest final,
        QC.counterexample "transferred sparse base failed audit" (valid imported)
      ]
  where
    history = labelReplayHistory (fromIntegral (2 + historySeed `mod` 6))
    cut = 1 + cutSeed `mod` length history
    through = controlIndex (fromIntegral cut)
    (prefix, suffix) = splitAt cut history
    pins = Set.fromList [Entry.appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) | (entry, pin) <- zip prefix (cycle (True : suppliedPins)), pin]
    based = checked "capture covered base" (OracleProjection.advanceReplayBase (foldl' (flip applyCanonical) fixtureOracleProjectionState prefix))
    reclaimed = checked "reclaim covered prefix" (OracleProjection.reclaimAppliedPrefixThroughBase pins Nothing based)
    final = foldl' (flip applyCanonical) reclaimed suffix
    reference = foldl' (flip applyCanonical) fixtureOracleProjectionState history
    capture = checked "capture sparse projection" (OracleProjection.captureProjectionBase final)
    imported = checked "install sparse projection" (OracleProjection.installProjectionBase capture remoteProjection)
    remoteReference = foldl' (flip applyCanonical) remoteProjection history
    valid state = case OracleProjection.validateState state of Left _ -> False; Right _ -> True

propCoveredCanonicalTail :: QC.NonNegative Int -> QC.NonNegative Int -> QC.NonNegative Int -> [Bool] -> QC.Property
propCoveredCanonicalTail (QC.NonNegative historySeed) (QC.NonNegative baseSeed) (QC.NonNegative floorSeed) suppliedPins =
  QC.counterexample ("base " <> show through <> ", exclusive tail floor " <> show floorIndex <> ", pins " <> show pins)
    $ QC.conjoin
      [ OracleProjection.projectionCanonicalCoveredThrough retained QC.=== through,
        OracleProjection.retainedControlSuffixAfter floorIndex retained QC.=== Just (drop floorPosition entries),
        OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness retained) QC.=== expected (\index -> index > floorIndex || Set.member index pins),
        OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness released) QC.=== expected (\index -> index > through || Set.member index pins),
        QC.counterexample "tail retention changes semantic facts" (OracleProjection.oracleView retained == OracleProjection.oracleView complete),
        QC.counterexample "tail release changes semantic facts" (OracleProjection.oracleView released == OracleProjection.oracleView complete),
        QC.counterexample "tail replay is owner-idempotent" (checked "repeat tail retention" (OracleProjection.reclaimAppliedPrefixThroughBase pins (Just floorIndex) retained) == retained),
        QC.counterexample "retained tail violates projection invariant" (valid retained),
        QC.counterexample "released tail violates projection invariant" (valid released),
        OracleProjection.retainedControlSuffixAfter floorIndex imported QC.=== Just (drop floorPosition entries),
        OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness imported) QC.=== OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness retained),
        QC.counterexample "tail archive fails its canonical round-trip" (OracleProjection.encodeProjectionBase decoded == bytes),
        QC.counterexample "transferred tail violates projection invariant" (valid imported)
      ]
  where
    count = 6 + historySeed `mod` 12
    entries = rejectedCanonicalHistory (fromIntegral count)
    basePosition = 2 + baseSeed `mod` (count - 2)
    floorPosition = floorSeed `mod` basePosition
    through = controlIndex (fromIntegral basePosition)
    floorIndex = controlIndex (fromIntegral floorPosition)
    -- Include an exact old pin and a pin overlapping the requested tail.
    pins = Set.fromList (controlIndex 1 : controlIndex (fromIntegral floorPosition + 1) : [entryIndex entry | (entry, pin) <- zip entries suppliedPins, pin])
    (prefix, suffix) = splitAt basePosition entries
    base = checked "capture tail-retention base" (OracleProjection.advanceReplayBase (foldl' (flip applyCanonical) fixtureOracleProjectionState prefix))
    complete = foldl' (flip applyCanonical) base suffix
    retained = checked "retain tail below semantic base" (OracleProjection.reclaimAppliedPrefixThroughBase pins (Just floorIndex) complete)
    released = checked "release tail, retain exact pins" (OracleProjection.reclaimAppliedPrefixThroughBase pins Nothing retained)
    capture = checked "capture contiguous canonical tail" (OracleProjection.captureProjectionBase retained)
    bytes = OracleProjection.encodeProjectionBase capture
    decoded = checked "decode contiguous canonical tail" (OracleProjection.decodeProjectionBase remoteProjection bytes)
    imported = checked "install contiguous canonical tail" (OracleProjection.installProjectionBase decoded remoteProjection)
    entryIndex = Entry.appliedEntryControlIndex . canonicalAppliedOracleEntryValue
    expected predicate = [(entryIndex entry, entry) | entry <- entries, predicate (entryIndex entry)]
    valid state = case OracleProjection.validateState state of Left _ -> False; Right _ -> True

caseCoveredCanonicalTailAdmission :: Assertion
caseCoveredCanonicalTailAdmission = do
  let entries = rejectedCanonicalHistory 5
      (prefix, suffix) = splitAt 3 entries
      base = checked "capture tail admission base" (OracleProjection.advanceReplayBase (foldl' (flip applyCanonical) fixtureOracleProjectionState prefix))
      complete = foldl' (flip applyCanonical) base suffix
      recentFloor = checked "tail above base retains ordinary suffix" (OracleProjection.reclaimAppliedPrefixThroughBase Set.empty (Just (controlIndex 4)) complete)
      tipFloor = checked "exclusive tail at cursor is empty" (OracleProjection.reclaimAppliedPrefixThroughBase Set.empty (Just (controlIndex 5)) complete)
      rebased = checked "advance tail admission base to tip" (OracleProjection.advanceReplayBase complete)
      sparse = checked "release intervening bytes" (OracleProjection.reclaimAppliedPrefixThroughBase (Set.fromList [controlIndex 1, controlIndex 4]) Nothing rebased)
  assertEqual "a floor above the semantic base cannot remove the ordinary suffix" [controlIndex 4, controlIndex 5] (map fst (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness recentFloor)))
  assertBool "a cursor floor likewise retains all post-base bytes" (recentFloor == tipFloor)
  assertEqual "exclusive cursor floor needs no bytes" (Just []) (OracleProjection.retainedControlSuffixAfter (controlIndex 5) tipFloor)
  case OracleProjection.reclaimAppliedPrefixThroughBase Set.empty (Just (controlIndex 6)) complete of
    Left problem -> assertEqual "a floor cannot precede unavailable future bytes" (OracleProjection.ProjectionCanonicalTailFloorBeyondCursor (controlIndex 6) (controlIndex 5)) problem
    Right _ -> assertFailure "tail floor ahead of current cursor accepted"
  case OracleProjection.reclaimAppliedPrefixThroughBase (Set.singleton (controlIndex 1)) (Just (controlIndex 2)) sparse of
    Left problem -> assertEqual "isolated pins cannot bridge a missing canonical tail" (OracleProjection.ProjectionCanonicalTailUnavailable (controlIndex 2) (controlIndex 5)) problem
    Right _ -> assertFailure "reclaimed tail silently accepted"
  let emptyTail = checked "empty tail at sparse cursor" (OracleProjection.reclaimAppliedPrefixThroughBase (Set.singleton (controlIndex 1)) (Just (controlIndex 5)) sparse)
  assertEqual "an empty requested tail preserves only the exact pin" [controlIndex 1] (map fst (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness emptyTail)))
  let first = applyCanonical (firstEntry entries) fixtureOracleProjectionState
      (_, _, detached) = retireProjection (controlIndex 2) remoteEpoch first
      mixed = checked "capture detached gap in canonical history" (OracleProjection.advanceReplayBase (applyCanonical (entries !! 2) detached))
  case OracleProjection.reclaimAppliedPrefixThroughBase Set.empty (Just (controlIndex 0)) mixed of
    Left problem -> assertEqual "detached membership evidence cannot fill a canonical tail" (OracleProjection.ProjectionCanonicalTailUnavailable (controlIndex 0) (controlIndex 3)) problem
    Right _ -> assertFailure "detached membership accepted as missing canonical bytes"
  mapM_ assertValid [recentFloor, tipFloor, emptyTail]

-- The operational transition receives an already admitted owner. Compare it
-- with the independent history audit across varying workflow/base cuts, then
-- advance and discharge retention obligations without advancing that cursor.
propCurrentCanonicalReclamation :: QC.NonNegative Int -> QC.NonNegative Int -> QC.NonNegative Int -> [Bool] -> QC.Property
propCurrentCanonicalReclamation (QC.NonNegative historySeed) (QC.NonNegative cutSeed) (QC.NonNegative floorSeed) suppliedPins =
  QC.counterexample ("current cut " <> show currentIndex <> ", old base " <> show basePosition <> ", tail " <> show tailFloor)
    $ QC.conjoin
      [ QC.conjoin
          [ QC.counterexample (context <> " differs from independent checked reclamation") (candidate == reference)
          | (context, candidate, reference) <- comparisons
          ],
        QC.conjoin [QC.counterexample (context <> " violates the owner invariant") (valid candidate) | (context, candidate, _) <- comparisons],
        OracleProjection.projectionCanonicalCoveredThrough retained QC.=== currentIndex,
        OracleProjection.replayBaseControlIndex retained QC.=== currentIndex,
        QC.counterexample "same-cursor retention changed semantic facts" (all ((== OracleProjection.oracleView complete) . OracleProjection.oracleView . secondOf3) comparisons),
        OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness discharged) QC.=== Map.toAscList survivingPins,
        OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness released) QC.=== [],
        QC.counterexample "repeating the same retention changed the owner" (checked "repeat current retention" (OracleProjection.reclaimCurrentAppliedPrefix pins tailFloor retained) == retained),
        QC.counterexample "later controls differ from full replay" (OracleProjection.oracleView continued == OracleProjection.oracleView fullReference),
        QC.counterexample "later controls violate the owner invariant" (valid continued)
      ]
  where
    history = labelReplayHistory (fromIntegral (1 + historySeed `mod` 6))
    currentPosition = cutSeed `mod` (length history + 1)
    currentIndex = controlIndex (fromIntegral currentPosition)
    basePosition = currentPosition `div` 2
    (prefix, suffix) = splitAt currentPosition history
    (beforeBase, afterBase) = splitAt basePosition prefix
    base = checked "capture earlier operational reference base" (OracleProjection.advanceReplayBase (foldl' (flip applyCanonical) fixtureOracleProjectionState beforeBase))
    complete = foldl' (flip applyCanonical) base afterBase
    pins = Map.fromList [(entryIndex entry, entry) | (entry, selected) <- zip prefix (cycle (True : suppliedPins)), selected]
    tailFloor = if even historySeed then Just (controlIndex (fromIntegral (floorSeed `mod` (currentPosition + 1)))) else Nothing
    advancedFloor = fmap (\floorIndex -> controlIndex ((controlIndexWord64 floorIndex + fromIntegral currentPosition) `div` 2)) tailFloor
    survivingPins = Map.filterWithKey (\index _ -> even (controlIndexWord64 index)) pins
    retained = checked "current prefix retention" (OracleProjection.reclaimCurrentAppliedPrefix pins tailFloor complete)
    advanced = checked "same-cursor floor and pin release" (OracleProjection.reclaimCurrentAppliedPrefix survivingPins advancedFloor retained)
    discharged = checked "same-cursor tail discharge" (OracleProjection.reclaimCurrentAppliedPrefix survivingPins Nothing advanced)
    released = checked "same-cursor final pin release" (OracleProjection.reclaimCurrentAppliedPrefix Map.empty Nothing discharged)
    comparisons =
      [ ("initial retention", retained, referenceReclamation pins tailFloor complete),
        ("floor and pin release", advanced, referenceReclamation survivingPins advancedFloor retained),
        ("tail discharge", discharged, referenceReclamation survivingPins Nothing advanced),
        ("final pin release", released, referenceReclamation Map.empty Nothing discharged)
      ]
    referenceReclamation selected floorIndex owner = checked "checked current reclamation reference" (OracleProjection.advanceReplayBase owner >>= OracleProjection.reclaimAppliedPrefixThroughBase (Map.keysSet selected) floorIndex)
    continued = foldl' (flip applyCanonical) released suffix
    fullReference = foldl' (flip applyCanonical) fixtureOracleProjectionState history
    entryIndex = Entry.appliedEntryControlIndex . canonicalAppliedOracleEntryValue
    secondOf3 (_, value, _) = value
    valid state = case OracleProjection.validateState state of Left _ -> False; Right _ -> True

caseCurrentCanonicalReclamationAdmission :: Assertion
caseCurrentCanonicalReclamationAdmission = do
  let entries = rejectedCanonicalHistory 5
      complete = foldl' (flip applyCanonical) fixtureOracleProjectionState entries
      pins = Map.fromList [(controlIndex 1, entries !! 0), (controlIndex 4, entries !! 3)]
      sparse = checked "current sparse retention" (OracleProjection.reclaimCurrentAppliedPrefix pins Nothing complete)
      emptyTail = checked "current empty tail" (OracleProjection.reclaimCurrentAppliedPrefix pins (Just (controlIndex 5)) sparse)
      zero = checked "reclaim zero owner" (OracleProjection.reclaimCurrentAppliedPrefix Map.empty (Just (controlIndex 0)) fixtureOracleProjectionState)
      released = checked "release current exact pins" (OracleProjection.reclaimCurrentAppliedPrefix Map.empty Nothing sparse)
      coordinates = Map.singleton (controlIndex 1) (entries !! 2)
      original = checked "retain original archive bytes" (OracleProjection.reclaimCurrentAppliedPrefix coordinates Nothing complete)
      rejected context expected selected floorIndex owner = do
        let observed = OracleProjection.reclaimCurrentAppliedPrefix selected floorIndex owner
            reference = OracleProjection.advanceReplayBase owner >>= OracleProjection.reclaimAppliedPrefixThroughBase (Map.keysSet selected) floorIndex
        assertEqual (context <> ": operational result") (Just expected) (either Just (const Nothing) observed)
        assertEqual (context <> ": independent checked result") (Just expected) (either Just (const Nothing) reference)
  assertBool "reclamation preserves the zero owner" (zero == fixtureOracleProjectionState)
  assertBool "a cursor floor retains precisely the selected pins" (emptyTail == sparse)
  assertEqual "supplied pin values cannot replace original archive bytes" (Just (entries !! 0)) (OracleProjection.appliedEntryEvidence (controlIndex 1) original)
  rejected "future floor" (OracleProjection.ProjectionCanonicalTailFloorBeyondCursor (controlIndex 6) (controlIndex 5)) Map.empty (Just (controlIndex 6)) complete
  rejected "reclaimed canonical gap" (OracleProjection.ProjectionCanonicalTailUnavailable (controlIndex 2) (controlIndex 5)) pins (Just (controlIndex 2)) sparse
  rejected "released pin" (OracleProjection.ProjectionCanonicalPinMissing (controlIndex 1)) (Map.singleton (controlIndex 1) (entries !! 0)) Nothing released
  rejected "future pin" (OracleProjection.ProjectionCanonicalPinMissing (controlIndex 6)) (Map.singleton (controlIndex 6) (entries !! 0)) Nothing complete
  let first = applyCanonical (firstEntry entries) fixtureOracleProjectionState
      (_, _, detached) = retireProjection (controlIndex 2) remoteEpoch first
      mixed = applyCanonical (entries !! 2) detached
      mixedTail = checked "current tail after detached membership" (OracleProjection.reclaimCurrentAppliedPrefix Map.empty (Just (controlIndex 2)) mixed)
      mixedReference = checked "checked tail after detached membership" (OracleProjection.advanceReplayBase mixed >>= OracleProjection.reclaimAppliedPrefixThroughBase Set.empty (Just (controlIndex 2)))
  rejected "detached membership is not canonical evidence" (OracleProjection.ProjectionCanonicalTailUnavailable (controlIndex 0) (controlIndex 3)) Map.empty (Just (controlIndex 0)) mixed
  assertBool "detached evidence and semantic facts survive operational reclamation" (mixedTail == mixedReference)
  mapM_ assertValid [sparse, emptyTail, zero, released, original, mixedTail]

-- Force only successive owner constructors. Reclamation must not capture a
-- previous owner through a lazy archive operation or the new semantic base.
{-# NOINLINE currentReclamationOwnerHistory #-}
currentReclamationOwnerHistory :: IO (OracleProjection.State, [Weak OracleProjection.State])
currentReclamationOwnerHistory = loop 1 (Live.initialOracleState fixtureCheckedOracleGenesis) fixtureOracleProjectionState Map.empty []
  where
    loop ordinal oracle projection pins witnesses
      | ordinal > 64 = pure (projection, reverse witnesses)
      | otherwise = do
          let (nextOracle, entry) = commitHistoryCommand ordinal rejectedCommand oracle
              !retainedPins = if ordinal == 1 then Map.singleton (controlIndex 1) entry else pins
              floorIndex = controlIndex (if ordinal > 8 then ordinal - 8 else 0)
          applied <- projectRetentionEntry entry projection
          appliedWitness <- mkWeakPtr applied Nothing
          successor <- evaluate (checked "operational retention owner" (OracleProjection.reclaimCurrentAppliedPrefix retainedPins (Just floorIndex) applied))
          witness <- mkWeakPtr successor Nothing
          loop (ordinal + 1) nextOracle successor retainedPins (witness : appliedWitness : witnesses)

caseCurrentCanonicalOwnerRetention :: Assertion
caseCurrentCanonicalOwnerRetention = do
  (projection, witnesses) <- currentReclamationOwnerHistory
  bracket (newStablePtr projection) freeStablePtr $ \root -> do
    performGC
    alive <- forM witnesses (fmap (maybe False (const True)) . deRefWeak)
    assertEqual "only the current operational owner survives before any history audit" (replicate 127 False <> [True]) alive
    retained <- deRefStablePtr root
    assertEqual "current semantic base covers every received entry" (controlIndex 64) (OracleProjection.replayBaseControlIndex retained)
    assertEqual "current coverage is independent of retained tail and pin" (controlIndex 64) (OracleProjection.projectionCanonicalCoveredThrough retained)
    assertEqual "only the old pin and current tail survive" (map controlIndex (1 : [57 .. 64])) (map fst (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness retained)))
    assertEqual "the outstanding tail remains readable" (Just 8) (length <$> OracleProjection.retainedControlSuffixAfter (controlIndex 56) retained)
    assertValid retained

caseCoveredCanonicalClassification :: Assertion
caseCoveredCanonicalClassification = do
  let entries = rejectedCanonicalHistory 3
      full = foldl' (flip applyCanonical) fixtureOracleProjectionState entries
      based = checked "base canonical classification" (OracleProjection.advanceReplayBase full)
      first = controlIndex 1
      covered = checked "retain one exact pin" (OracleProjection.reclaimAppliedPrefixThroughBase (Set.singleton first) Nothing based)
      conflicting = firstEntry (labelReplayHistory 1)
  assertEqual "rebasing alone does not cover or discard exact evidence" (controlIndex 0) (OracleProjection.projectionCanonicalCoveredThrough based)
  assertEqual "retained exact pin remains exact" (OracleProjection.AppliedEntryExactDuplicate first) (OracleProjection.classifyAppliedEntry (firstEntry entries) covered)
  case OracleProjection.prepareAppliedEntry conflicting covered of
    Left problem -> assertEqual "a conflicting retained pin still faults" (OracleProjection.AppliedEntryUnequalConflictFault first) problem
    Right _ -> assertFailure "conflicting retained pin accepted"
  let released = checked "release final exact pin" (OracleProjection.reclaimAppliedPrefixThroughBase Set.empty Nothing covered)
  assertEqual "released historical evidence is covered regardless of bytes" (OracleProjection.AppliedEntryCoveredByBase first) (OracleProjection.classifyAppliedEntry conflicting released)
  assertEqual "all canonical bytes can be released at this base" [] (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness released))
  case OracleProjection.reclaimAppliedPrefixThroughBase (Set.singleton first) Nothing released of
    Left problem -> assertEqual "released bytes cannot be fabricated as a new exact pin" (OracleProjection.ProjectionCanonicalPinMissing first) problem
    Right _ -> assertFailure "missing exact pin accepted"
  case OracleProjection.reclaimAppliedPrefixThroughBase (Set.singleton (controlIndex 4)) Nothing covered of
    Left problem -> assertEqual "future coordinates cannot be pinned" (OracleProjection.ProjectionCanonicalPinMissing (controlIndex 4)) problem
    Right _ -> assertFailure "future exact pin accepted"
  assertValid released

caseCoveredCanonicalScalars :: Assertion
caseCoveredCanonicalScalars = do
  let initialOracle = Live.initialOracleStateWithDigestMode Live.OracleStateDigestEnabled fixtureCheckedOracleGenesis
      (afterCommand, command) = commitHistoryCommand 1 rejectedCommand initialOracle
      commandProjection = applyCanonical command fixtureOracleProjectionState
      progressEnvelope = Live.oracleEnvelope (oracleClientRequestId localEpoch 1) Nothing localEpoch (Live.retireOracleReceiptProgressCommand (Lifetime.receiptRetirementPrefix (Just 1)))
      (_, progressOutcome) = Live.submitOracleState progressEnvelope afterCommand
      progress = case Live.oracleSubmissionOutcomeView progressOutcome of
        Live.OracleSubmissionProgressRetiredView _ _ (Just entry) -> canonicalizeAppliedOracleEntry entry
        other -> error ("expected canonical progress: " <> show other)
      progressed = applyCanonical progress commandProjection
      (generation, ends, retired) = retireProjection (controlIndex 3) remoteEpoch progressed
      based = checked "capture scalar base" (OracleProjection.advanceReplayBase retired)
      covered = checked "reclaim every canonical scalar witness" (OracleProjection.reclaimAppliedPrefixThroughBase Set.empty Nothing based)
      capture = checked "capture sparse mixed evidence" (OracleProjection.captureProjectionBase covered)
      imported = checked "install sparse mixed evidence" (OracleProjection.installProjectionBase capture remoteProjection)
      digest = Entry.appliedEntryPostStateDigest (canonicalAppliedOracleEntryValue progress)
  assertBool "diagnostic fixture includes a canonical digest" (digest /= Nothing)
  assertEqual "rejected command is semantic for progress" (controlIndex 1) (OracleProjection.projectionLastSemanticControlIndex commandProjection)
  assertEqual "receipt progress advances the watch cursor" (controlIndex 2) (projectionCursor progressed)
  assertEqual "receipt progress does not advance semantic work" (controlIndex 1) (OracleProjection.projectionLastSemanticControlIndex progressed)
  assertEqual "detached membership advances semantic work" (controlIndex 3) (OracleProjection.projectionLastSemanticControlIndex covered)
  assertEqual "detached membership retains the latest canonical digest" digest (OracleProjection.projectedOracleStateDigest covered)
  assertEqual "base transfer preserves the independent digest" digest (OracleProjection.projectedOracleStateDigest imported)
  assertEqual "base transfer preserves the independent semantic coordinate" (controlIndex 3) (OracleProjection.projectionLastSemanticControlIndex imported)
  duplicate <- checkedIO "retained detached duplicate after canonical reclamation" (OracleProjection.prepareMembershipAdvance (controlIndex 3) generation ends imported)
  assertEqual "detached membership remains exact" OracleProjection.MembershipAdvanceExactDuplicate (OracleProjection.preparedMembershipAdvanceDisposition duplicate)
  assertBool "detached duplicate does not alter sparse imported state" (OracleProjection.commitMembershipAdvance duplicate == imported)
  assertValid covered
  assertValid imported

{-# NOINLINE reclaimedProjectionOwnerHistory #-}
reclaimedProjectionOwnerHistory :: Maybe ControlIndex -> IO (OracleProjection.State, [Weak OracleProjection.State])
reclaimedProjectionOwnerHistory tailFloor = do
  (donor, witnesses) <- rejectedProjectionOwnerHistory True 64
  reclaimed <- evaluate (checked "reclaim owner history" (OracleProjection.reclaimAppliedPrefixThroughBase Set.empty tailFloor donor))
  pure (reclaimed, witnesses)

caseCoveredCanonicalOwnerRetention :: Assertion
caseCoveredCanonicalOwnerRetention = do
  (projection, witnesses) <- reclaimedProjectionOwnerHistory Nothing
  bracket (newStablePtr projection) freeStablePtr $ \root -> do
    performGC
    alive <- forM witnesses (fmap (maybe False (const True)) . deRefWeak)
    assertEqual "sparse evidence and scalar witnesses retain no prior owners" (replicate 128 False) alive
    retained <- deRefStablePtr root
    assertEqual "the covered floor survives collection" (controlIndex 64) (OracleProjection.projectionCanonicalCoveredThrough retained)
    assertEqual "the semantic scalar survives collection" (controlIndex 64) (OracleProjection.projectionLastSemanticControlIndex retained)
    assertEqual "reclaimed canonical archive remains empty" [] (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness retained))
    assertValid retained

caseCoveredCanonicalTailOwnerRetention :: Assertion
caseCoveredCanonicalTailOwnerRetention = do
  (projection, witnesses) <- reclaimedProjectionOwnerHistory (Just (controlIndex 32))
  bracket (newStablePtr projection) freeStablePtr $ \root -> do
    performGC
    alive <- forM witnesses (fmap (maybe False (const True)) . deRefWeak)
    assertEqual "a retained canonical tail does not retain any preceding projection owner" (replicate 128 False) alive
    retained <- deRefStablePtr root
    assertEqual "tail retention remains independent of semantic coverage" (controlIndex 64) (OracleProjection.projectionCanonicalCoveredThrough retained)
    assertEqual "only the requested exclusive tail survives" (map controlIndex [33 .. 64]) (map fst (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness retained)))
    assertEqual "the retained tail remains completely readable" (Just 32) (length <$> OracleProjection.retainedControlSuffixAfter (controlIndex 32) retained)
    assertValid retained

{-# NOINLINE reclaimedCanonicalEvidenceHistory #-}
reclaimedCanonicalEvidenceHistory :: IO (OracleProjection.State, [Weak CanonicalAppliedOracleEntry])
reclaimedCanonicalEvidenceHistory = loop 1 initialOracle fixtureOracleProjectionState []
  where
    initialOracle = Live.initialOracleStateWithDigestMode Live.OracleStateDigestEnabled fixtureCheckedOracleGenesis
    loop ordinal oracle projection witnesses
      | ordinal > 32 = do
          let based = checked "base exact-evidence retention history" (OracleProjection.advanceReplayBase projection)
              reclaimed = checked "reclaim exact-evidence retention history" (OracleProjection.reclaimAppliedPrefixThroughBase (Set.singleton (controlIndex 1)) Nothing based)
              capture = checked "capture exact-evidence retention history" (OracleProjection.captureProjectionBase reclaimed)
          imported <- evaluate (checked "import exact-evidence retention history" (OracleProjection.installProjectionBase capture remoteProjection))
          pure (imported, reverse witnesses)
      | otherwise = do
          let (nextOracle, supplied) = commitHistoryCommand ordinal rejectedCommand oracle
          entry <- evaluate supplied
          witness <- mkWeakPtr entry Nothing
          nextProjection <- projectRetentionEntry entry projection
          loop (ordinal + 1) nextOracle nextProjection (witness : witnesses)

caseCoveredCanonicalEvidenceRetention :: Assertion
caseCoveredCanonicalEvidenceRetention = do
  (projection, witnesses) <- reclaimedCanonicalEvidenceHistory
  bracket (newStablePtr projection) freeStablePtr $ \root -> do
    performGC
    alive <- forM witnesses (fmap (maybe False (const True)) . deRefWeak)
    assertEqual "only the requested canonical pin survives sparse capture and transfer" (True : replicate 31 False) alive
    retained <- deRefStablePtr root
    assertBool "the latest diagnostic digest survives without its canonical entry" (OracleProjection.projectedOracleStateDigest retained /= Nothing)
    assertEqual "the imported exact archive contains only the pin" [controlIndex 1] (map fst (OracleProjection.projectionWitnessAppliedEntries (OracleProjection.stateWitness retained)))
    assertValid retained

caseReplayBaseProcessHistory :: Assertion
caseReplayBaseProcessHistory = do
  let process = checked "fresh replay process epoch" (Identity.mkProcessEpochId (fixtureIdentifierBytes 0xf2))
      identity = checked "fresh replay process identity" (Identity.mkProcessId (fixtureIdentifierBytes 0xf1))
      start = processStart identity process localEpoch
      (started, startEntry) = commitHistoryCommand 1 (Live.startProcessEpochCommand start) (Live.initialOracleState fixtureCheckedOracleGenesis)
      (interleaved, middleEntry) = commitHistoryCommand 2 rejectedCommand started
      (ended, endEntry) = commitHistoryCommand 3 (checked "replay process End" (Live.endProcessEpochCommand process ExplicitAdministrativeEnd)) interleaved
      (_, tailEntry) = commitHistoryCommand 4 rejectedCommand ended
      beforeBase = foldl' (flip applyCanonical) fixtureOracleProjectionState [startEntry, middleEntry]
      firstBase = checked "capture live started process" (OracleProjection.advanceReplayBase beforeBase)
      suffix = foldl' (flip applyCanonical) firstBase [endEntry, tailEntry]
      final = checked "capture ended process" (OracleProjection.advanceReplayBase suffix)
      reference = foldl' (flip applyCanonical) fixtureOracleProjectionState [startEntry, middleEntry, endEntry, tailEntry]
  assertEqual "Start lookup survives both bases" (Just startEntry) (OracleProjection.appliedEntryForRequest (oracleClientRequestId localEpoch 1) final)
  assertEqual "End lookup survives both bases" (Just endEntry) (OracleProjection.appliedEntryForRequest (oracleClientRequestId localEpoch 3) final)
  assertEqual "started process remains bound to its original coordinate" [controlIndex 1] (map (OracleProjection.projectedStartedProcessControlIndex . snd) (OracleProjection.projectedStartedProcesses final))
  assertEqual "process residence was absent before Start" Nothing (OracleProjection.oracleProjectionProcessResidenceAt (controlIndex 0) process final)
  assertEqual "process residence is available at its old Start coordinate" (Just localEpoch) (OracleProjection.oracleProjectionProcessResidenceAt (controlIndex 1) process final)
  assertBool "old authority before End remains live" (OracleProjection.oracleProjectionProcessIsLiveAt (controlIndex 2) process final)
  assertBool "End is effective at its original coordinate" (not (OracleProjection.oracleProjectionProcessIsLiveAt (controlIndex 3) process final))
  assertBool "Start and End semantic facts match full replay" (OracleProjection.oracleView final == OracleProjection.oracleView reference)
  assertEqual "Start and End exact witnesses match full replay" (OracleProjection.validateState reference) (OracleProjection.validateState final)
  assertEqual "Start and End genesis validation is unchanged" (OracleProjection.validateStateFromGenesis final) (OracleProjection.validateState final)
  assertValid final

retireProjection :: ControlIndex -> HeraldEpoch -> OracleProjection.State -> (HeraldMembershipGeneration, [OracleProjection.MembershipAdvanceProcessEnd], OracleProjection.State)
retireProjection index target state =
  let current = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView state)
      generation = checked "mixed retirement generation" (retireHeraldMembershipGeneration index (retirementAt index) target current)
      ends = residentEnds index target state
      prepared = checked "mixed detached retirement" (OracleProjection.prepareMembershipAdvance index generation ends state)
   in (generation, ends, OracleProjection.commitMembershipAdvance prepared)

caseReplayBaseTamper :: Assertion
caseReplayBaseTamper = do
  let (initial, _, _, _, retired) = advancedRemote
      genesis = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView initial)
      corrupt = OracleProjection.replaceCurrentHeraldMembershipForInvariantTest genesis
      captured = checked "capture valid membership base" (OracleProjection.advanceReplayBase retired)
  case OracleProjection.advanceReplayBase (corrupt retired) of
    Left OracleProjection.ProjectionCurrentHeraldMembershipMismatch -> pure ()
    Left problem -> assertFailure ("unexpected invalid base rejection: " <> show problem)
    Right _ -> assertFailure "corrupt semantic facts were sealed into a checked base"
  assertEqual "post-capture corruption cannot change the checked origin" (Left OracleProjection.ProjectionCurrentHeraldMembershipMismatch) (OracleProjection.validateState (corrupt captured))
  assertEqual "independent genesis replay also detects post-capture corruption" (Left OracleProjection.ProjectionCurrentHeraldMembershipMismatch) (OracleProjection.validateStateFromGenesis (corrupt captured))

caseGenesis :: Assertion
caseGenesis = do
  let state = fixtureOracleProjectionState
      view = OracleProjection.oracleView state
      current = OracleProjection.oracleViewCurrentHeraldMembership view
  assertEqual
    "current generation is the sole genesis history entry"
    [current]
    (OracleProjection.oracleViewHeraldMembershipHistory view)
  assertEqual
    "the current ID is derived from that checked generation"
    (heraldMembershipGenerationId current)
    (OracleProjection.oracleViewCurrentHeraldMembershipId view)
  assertBool
    "the checked local epoch starts current"
    (OracleProjection.oracleViewLocalHeraldIsCurrent view)
  assertValid state

caseValidSuccessor :: Assertion
caseValidSuccessor = do
  let (initial, index, successor, ends, after) = advancedRemote
      before = OracleProjection.oracleView initial
      afterView = OracleProjection.oracleView after
  assertEqual "the retirement occupies the next control index" index (OracleProjection.oracleViewControlIndex afterView)
  assertEqual "the supplied checked successor becomes current" successor (OracleProjection.oracleViewCurrentHeraldMembership afterView)
  assertEqual
    "history retains the exact predecessor and successor"
    [OracleProjection.oracleViewCurrentHeraldMembership before, successor]
    (OracleProjection.oracleViewHeraldMembershipHistory afterView)
  assertEqual
    "frozen genesis work resolves its predecessor membership"
    (Just (OracleProjection.oracleViewCurrentHeraldMembership before))
    (OracleProjection.oracleViewHeraldMembershipAt (controlIndex 0) afterView)
  assertEqual
    "work at the retirement coordinate resolves the successor membership"
    (Just successor)
    (OracleProjection.oracleViewHeraldMembershipAt index afterView)
  assertEqual
    "an unapplied future coordinate has no membership authority"
    Nothing
    (OracleProjection.oracleViewHeraldMembershipAt (controlIndex 3) afterView)
  assertBool
    "the retired epoch is absent from current authority"
    (not (OracleProjection.oracleViewIsActiveHerald remoteEpoch afterView))
  assertEqual
    "the exact target-resident process set ends at the retirement coordinate"
    (fmap OracleProjection.membershipAdvanceProcessEndEpoch ends)
    (fmap fst (OracleProjection.projectedEndedProcesses after))
  assertValid after

caseDuplicate :: Assertion
caseDuplicate = do
  let (_, index, successor, ends, after) = advancedRemote
  duplicate <-
    checkedIO
      "exact duplicate membership batch"
      (OracleProjection.prepareMembershipAdvance index successor ends after)
  assertEqual
    "the retained batch is classified exactly"
    OracleProjection.MembershipAdvanceExactDuplicate
    (OracleProjection.preparedMembershipAdvanceDisposition duplicate)
  assertBool
    "committing an exact duplicate is a no-op"
    (OracleProjection.commitMembershipAdvance duplicate == after)

caseRepeatedSuccessor :: Assertion
caseRepeatedSuccessor = do
  let (initial, firstIndex, firstGeneration, firstEnds, firstState) = advancedRemote
      secondIndex = controlIndex 3
      secondGeneration = checked "second retirement generation" (retireHeraldMembershipGeneration secondIndex (retirementAt secondIndex) localEpoch firstGeneration)
      secondEnds = residentEnds secondIndex localEpoch firstState
      prepared = checked "second retirement projection" (OracleProjection.prepareMembershipAdvance secondIndex secondGeneration secondEnds firstState)
      after = OracleProjection.commitMembershipAdvance prepared
      view = OracleProjection.oracleView after
      origin = OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView initial)
  assertEqual "complete immutable history" [OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView initial), firstGeneration, secondGeneration] (OracleProjection.oracleViewHeraldMembershipHistory view)
  assertEqual "exact ancestor path retains both terminal epochs" (Just [remoteEpoch, localEpoch]) (heraldMembershipLineageRetiredHeraldEpochs <$> OracleProjection.oracleViewHeraldMembershipLineage origin (heraldMembershipGenerationId secondGeneration) view)
  assertEqual "a descendant cannot authorize its ancestor" Nothing (OracleProjection.oracleViewHeraldMembershipLineage (heraldMembershipGenerationId secondGeneration) origin view)
  assertBool "both removed epochs lose current authority" (all (not . (`OracleProjection.oracleViewIsActiveHerald` view)) [remoteEpoch, localEpoch])
  assertEqual "both resident End sets remain retained" (length firstEnds + length secondEnds) (length (OracleProjection.projectedEndedProcesses after))
  let replay = checked "earlier exact membership replay" (OracleProjection.prepareMembershipAdvance firstIndex firstGeneration firstEnds after)
  assertBool "replaying the first retirement cannot restore its authority" (OracleProjection.commitMembershipAdvance replay == after)
  assertValid after

caseRejectedAdvances :: Assertion
caseRejectedAdvances = do
  let (initial, index, successor, ends, after) = advancedRemote
      initialView = OracleProjection.oracleView initial
      genesis = OracleProjection.oracleViewCurrentHeraldMembership initialView
      active = OracleProjection.oracleViewActiveHeraldEpochs initialView
      foreignGenesis = checked "foreign genesis membership" (genesisHeraldMembershipGeneration foreignSystem (NonEmpty.fromList active))
      staleSuccessor = checked "foreign successor" (retireHeraldMembershipGeneration index (retirementAt index) remoteEpoch foreignGenesis)
      secondIndex = controlIndex 3
      sibling = checked "sibling successor" (retireHeraldMembershipGeneration secondIndex (retirementAt secondIndex) localEpoch genesis)
  case OracleProjection.prepareMembershipAdvance index staleSuccessor ends initial of
    Left OracleProjection.MembershipAdvanceStalePredecessor {} -> pure ()
    observed -> assertFailure ("expected stale predecessor rejection, got " <> showPreparationResult observed)
  case OracleProjection.prepareMembershipAdvance index successor [] after of
    Left (OracleProjection.MembershipAdvanceControlConflict observedIndex) ->
      assertEqual "the conflict retains its exact coordinate" index observedIndex
    observed -> assertFailure ("expected retained-coordinate conflict, got " <> showPreparationResult observed)
  case OracleProjection.prepareMembershipAdvance secondIndex sibling [] after of
    Left OracleProjection.MembershipAdvanceCatalogueMismatch {} -> pure ()
    observed -> assertFailure ("expected sibling history rejection, got " <> showPreparationResult observed)

caseCatalogueMismatch :: Assertion
caseCatalogueMismatch = do
  let state = projectionAfterProbePrefix
      view = OracleProjection.oracleView state
      index = controlIndex 2
      suppliedCatalogue = NonEmpty.fromList [localEpoch, remoteEpoch]
      smallerGenesis = checked "smaller genesis membership" (genesisHeraldMembershipGeneration (OracleProjection.oracleViewSystemId view) suppliedCatalogue)
      smallerSuccessor = checked "smaller successor" (retireHeraldMembershipGeneration index (retirementAt index) remoteEpoch smallerGenesis)
  case OracleProjection.prepareMembershipAdvance index smallerSuccessor (residentEnds index remoteEpoch state) state of
    Left OracleProjection.MembershipAdvanceCatalogueMismatch {} -> pure ()
    observed -> assertFailure ("expected catalogue mismatch, got " <> showPreparationResult observed)

caseEndReasonMismatch :: Assertion
caseEndReasonMismatch = do
  let state = projectionAfterProbePrefix
      index = controlIndex 2
      genesis = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView state)
      successor = checked "reason-mismatch successor" (retireHeraldMembershipGeneration index (retirementAt index) remoteEpoch genesis)
      ends =
        [ OracleProjection.membershipAdvanceProcessEnd
            (OracleProjection.membershipAdvanceProcessEndEpoch ended)
            index
            ExplicitAdministrativeEnd
        | ended <- residentEnds index remoteEpoch state
        ]
  case OracleProjection.prepareMembershipAdvance index successor ends state of
    Left (OracleProjection.MembershipAdvanceProcessEndReasonMismatch process) ->
      assertBool
        "the rejection identifies one of the exact retired-resident processes"
        (process `elem` fmap OracleProjection.membershipAdvanceProcessEndEpoch ends)
    observed -> assertFailure ("expected process-End reason mismatch, got " <> showPreparationResult observed)

caseLocalRetired :: Assertion
caseLocalRetired = do
  let state = projectionAfterProbePrefix
      index = controlIndex 2
      genesis = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView state)
      successor = checked "local-retired successor" (retireHeraldMembershipGeneration index (retirementAt index) localEpoch genesis)
      ends = residentEnds index localEpoch state
      prepared = checked "local-retired projection" (OracleProjection.prepareMembershipAdvance index successor ends state)
      after = OracleProjection.commitMembershipAdvance prepared
  assertBool
    "local retirement is represented rather than rejected as an invariant"
    (not (OracleProjection.oracleViewLocalHeraldIsCurrent (OracleProjection.oracleView after)))
  assertValid after

caseValidationTamper :: Assertion
caseValidationTamper = do
  let (initial, _, _, _, after) = advancedRemote
      genesis = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView initial)
      corrupted = OracleProjection.replaceCurrentHeraldMembershipForInvariantTest genesis after
  assertEqual
    "the mutable claim cannot disagree with replayed checked evidence"
    (Left OracleProjection.ProjectionCurrentHeraldMembershipMismatch)
    (OracleProjection.validateState corrupted)

advancedRemote ::
  ( OracleProjection.State,
    ControlIndex,
    HeraldMembershipGeneration,
    [OracleProjection.MembershipAdvanceProcessEnd],
    OracleProjection.State
  )
advancedRemote =
  (initial, index, successor, ends, OracleProjection.commitMembershipAdvance prepared)
  where
    initial = projectionAfterProbePrefix
    index = controlIndex 2
    genesis = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView initial)
    successor = checked "remote-retired successor" (retireHeraldMembershipGeneration index (retirementAt index) remoteEpoch genesis)
    ends = residentEnds index remoteEpoch initial
    prepared = checked "remote-retired projection" (OracleProjection.prepareMembershipAdvance index successor ends initial)

residentEnds ::
  ControlIndex ->
  HeraldEpoch ->
  OracleProjection.State ->
  [OracleProjection.MembershipAdvanceProcessEnd]
residentEnds index residence state =
  [ OracleProjection.membershipAdvanceProcessEnd process index fixtureHeraldRetirementReason
  | process <- OracleProjection.projectedProcessEpochs state,
    OracleProjection.oracleProjectionProcessResidenceAt cursor process state == Just residence,
    OracleProjection.oracleProjectionProcessIsLiveAt cursor process state
  ]
  where
    cursor = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView state)

-- The retirement resolution must refer to a strictly earlier control index.
-- A canonical rejected probe supplies that prefix without changing projection
-- facts; this detached membership seam does not admit the probe itself.
projectionAfterProbePrefix :: OracleProjection.State
projectionAfterProbePrefix =
  OracleProjection.commitAppliedEntry
    (checked "project preceding canonical entry" (OracleProjection.prepareAppliedEntry canonical fixtureOracleProjectionState))
  where
    initial = Live.initialOracleState fixtureCheckedOracleGenesis
    envelope = Live.oracleEnvelope (oracleClientRequestId localEpoch 1) Nothing localEpoch (Voter.cancelVoterChangeCommand (checked "unknown voter change" (Voter.mkVoterChangeId (controlIndex 999999))))
    (_, outcome) = Live.submitOracleState envelope initial
    canonical = case Live.oracleSubmissionOutcomeView outcome of
      Live.OracleSubmissionCommittedView _ entry -> canonicalizeAppliedOracleEntry entry
      other -> error ("expected canonical prefix: " <> show other)

retirementAt :: ControlIndex -> FailureProbeResolutionId
retirementAt index = deriveFailureProbeResolutionId probe RetireFailureProbeTarget
  where
    probe = checked "preceding failure probe coordinate" (deriveHeraldFailureProbeId (controlIndex (controlIndexWord64 index - 1)))

localEpoch :: HeraldEpoch
localEpoch = heraldMemberEpoch fixtureLocalMember

remoteEpoch :: HeraldEpoch
remoteEpoch = heraldMemberEpoch fixtureRemoteMember

foreignSystem :: SystemId
foreignSystem = checked "foreign system" (mkSystemId (fixtureIdentifierBytes 0xf1))

assertValid :: OracleProjection.State -> Assertion
assertValid state = case OracleProjection.validateState state of
  Left problem -> assertFailure ("projection validation failed: " <> show problem)
  Right _ -> pure ()

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

showPreparationResult ::
  Either OracleProjection.MembershipAdvanceProblem OracleProjection.PreparedMembershipAdvance ->
  String
showPreparationResult = either show (const "accepted")
