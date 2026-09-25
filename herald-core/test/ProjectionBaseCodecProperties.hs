{-# LANGUAGE OverloadedStrings #-}

module ProjectionBaseCodecProperties (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as Serialize
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Domain.Startup (heraldMemberEpoch)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry, canonicalAppliedOracleEntryValue, canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label qualified as Oracle
import Eclips.Oracle.Projection qualified as Entry
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures qualified as Fixtures
import ProjectionLabelBaseProperties qualified as LabelFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "portable complete Projection base"
    [ QC.testProperty "checked bases round-trip and resume a different receiver from every chosen cut" propBaseTransfers,
      QC.testProperty "covered prefixes preserve only requested exact pins after transfer and suffix" propSparseBaseTransfers,
      testCase "base decoding requires fresh matching receiver facts and canonical bytes" caseDecodeAdmission,
      testCase "base leaf bindings and sparse exact-evidence coordinates are checked" caseWireShape
    ]

local :: HeraldEpoch
local = heraldMemberEpoch Fixtures.fixtureLocalMember

remote :: HeraldEpoch
remote = heraldMemberEpoch Fixtures.fixtureRemoteMember

freshLocal, freshRemote :: Projection.State
freshLocal = Fixtures.fixtureOracleProjectionState
freshRemote = Projection.initialState genesis (Fixtures.fixtureCheckedInitialBootstrapsFor genesis)
  where
    deployment = Fixtures.fixtureDeploymentAt Fixtures.fixtureRemoteMember
    members = Genesis.checkedActiveHeralds Fixtures.fixtureStep14CheckedGenesis
    genesis =
      checked "portable receiver genesis"
        $ Genesis.checkHeraldGenesis
          deployment
            { Genesis.deploymentActiveHeralds = members,
              Genesis.deploymentOracleGenesis =
                (Genesis.deploymentOracleGenesis deployment) {Genesis.oracleGenesisActiveHeralds = members}
            }

withVoters :: Bool -> Projection.State -> Projection.State
withVoters False = id
withVoters True = checked "initialize portable voters" . Projection.initializeOracleVoters (Voter.oracleVoterConfiguration oracle) (Voter.oracleReplicaRegistrations oracle)
  where
    oracle = Oracle.initialOracleState Fixtures.fixtureCheckedOracleGenesis

-- Include accepted Start/End and an exact rejected duplicate End, so a suffix
-- cannot be reconstructed merely from the semantic label/process maps.
history :: Int -> [CanonicalAppliedOracleEntry]
history count = snd rejected
  where
    prior = LabelFixtures.projectionLabelTestHistory count
    process = checked "portable dynamic process" (mkProcessId (Fixtures.fixtureIdentifierBytes 246))
    epoch = checked "portable dynamic epoch" (mkProcessEpochId (Fixtures.fixtureIdentifierBytes 247))
    started = submit (Oracle.startProcessEpochCommand (processStart process epoch local)) prior
    command = checked "portable dynamic End" (Oracle.endProcessEpochCommand epoch ExplicitAdministrativeEnd)
    ended = submit command started
    rejected = submit command ended
    submit nextCommand (state, entries) = case Oracle.oracleSubmissionOutcomeView outcome of
      Oracle.OracleSubmissionCommittedView _ entry -> (next, entries <> [canonicalizeAppliedOracleEntry entry])
      other -> error ("portable history expected committed command: " <> show other)
      where
        request = oracleClientRequestId local (1 + controlIndexWord64 (Oracle.oracleGreatestControlIndex state))
        (next, outcome) = Oracle.submitOracleState (Oracle.oracleEnvelope request Nothing local nextCommand) state

apply :: Projection.State -> CanonicalAppliedOracleEntry -> Projection.State
apply state entry = Projection.commitAppliedEntry (checked "apply portable suffix" (Projection.prepareAppliedEntry entry state))

capture :: Projection.State -> Projection.ProjectionBaseCapture
capture = checked "capture portable base" . Projection.captureProjectionBase

encode :: Projection.State -> ByteString
encode = Projection.encodeProjectionBase . capture

cursor :: Projection.State -> ControlIndex
cursor = Projection.oracleViewControlIndex . Projection.oracleView

transfer :: Projection.State -> Projection.State -> (Projection.ProjectionBaseCapture, Projection.State)
transfer donor receiver =
  let decoded = checked "decode complete base" (Projection.decodeProjectionBase receiver (encode donor))
   in (decoded, checked "install decoded base" (Projection.installProjectionBase decoded receiver))

propBaseTransfers :: QC.NonNegative Int -> QC.NonNegative Int -> Bool -> QC.Property
propBaseTransfers (QC.NonNegative historySeed) (QC.NonNegative cutSeed) voters =
  QC.counterexample ("full portable cut " <> show cut)
    $ QC.conjoin
      [ QC.counterexample "complete capture changed on wire" (decoded == capture donor),
        Projection.oracleViewLocalHeraldEpoch (Projection.oracleView installed) QC.=== remote,
        QC.counterexample "receiver semantic view differs from canonical replay" (Projection.oracleView final == Projection.oracleView reference),
        Projection.validateState final QC.=== Projection.validateState reference,
        Projection.validateStateFromGenesis final QC.=== Projection.validateState final,
        Projection.projectionLastSemanticControlIndex final QC.=== Projection.projectionLastSemanticControlIndex reference,
        Projection.projectedOracleStateDigest final QC.=== Projection.projectedOracleStateDigest reference,
        QC.conjoin [Projection.appliedEntryEvidence (entryIndex entry) final QC.=== Just entry | entry <- entries],
        historicalLabels final reference
      ]
  where
    entries = history (1 + historySeed `mod` 5)
    cut = cutSeed `mod` (length entries + 1)
    (prefix, suffix) = splitAt cut entries
    donor = foldl' apply (withVoters voters freshLocal) prefix
    receiver = withVoters voters freshRemote
    (decoded, installed) = transfer donor receiver
    final = foldl' apply installed suffix
    reference = foldl' apply receiver entries

propSparseBaseTransfers :: QC.NonNegative Int -> QC.NonNegative Int -> [Bool] -> Bool -> QC.Property
propSparseBaseTransfers (QC.NonNegative historySeed) (QC.NonNegative cutSeed) suppliedPins voters =
  QC.counterexample ("sparse portable cut " <> show cut)
    $ QC.conjoin
      [ QC.counterexample "sparse capture changed on wire" (decoded == capture reclaimed),
        Projection.projectionBaseCanonicalCoveredThrough decoded QC.=== through,
        Map.keysSet (Projection.projectionBaseAppliedEntries decoded) QC.=== pins,
        Projection.projectionCanonicalCoveredThrough final QC.=== through,
        QC.counterexample "receiver semantic view differs from canonical replay" (Projection.oracleView final == Projection.oracleView reference),
        Projection.projectionLastSemanticControlIndex final QC.=== Projection.projectionLastSemanticControlIndex reference,
        Projection.projectedOracleStateDigest final QC.=== Projection.projectedOracleStateDigest reference,
        Projection.validateState final QC.=== Right (Projection.stateWitness final),
        Projection.validateStateFromGenesis final QC.=== Left (Projection.ProjectionGenesisEvidenceCovered through),
        historicalLabels final reference,
        QC.conjoin
          [ let index = entryIndex entry
                exact = index > through || Set.member index pins
                classification = if exact then Projection.AppliedEntryExactDuplicate index else Projection.AppliedEntryCoveredByBase index
                admitted = checked "covered duplicate after transfer" (Projection.prepareAppliedEntry entry final)
             in QC.conjoin
                  [ Projection.appliedEntryEvidence index final QC.=== (if exact then Just entry else Nothing),
                    Projection.preparedAppliedEntryClassification admitted QC.=== classification,
                    QC.counterexample "duplicate replay changed imported facts" (Projection.commitAppliedEntry admitted == final)
                  ]
          | entry <- entries
          ]
      ]
  where
    entries = history (1 + historySeed `mod` 5)
    cut = 1 + cutSeed `mod` length entries
    (prefix, suffix) = splitAt cut entries
    through = controlIndex (fromIntegral cut)
    pins = Set.fromList [entryIndex entry | (entry, keep) <- zip prefix (cycle (False : suppliedPins)), keep]
    donor = foldl' apply (withVoters voters freshLocal) prefix
    based = checked "advance imported replay base" (Projection.advanceReplayBase donor)
    reclaimed = checked "reclaim captured prefix" (Projection.reclaimAppliedPrefixThroughBase pins Nothing based)
    receiver = withVoters voters freshRemote
    (decoded, installed) = transfer reclaimed receiver
    final = foldl' apply installed suffix
    reference = foldl' apply receiver entries

historicalLabels :: Projection.State -> Projection.State -> QC.Property
historicalLabels candidate reference =
  QC.conjoin
    [ Projection.oracleProjectionReleasedLabelAt (controlIndex index) object candidate QC.=== Projection.oracleProjectionReleasedLabelAt (controlIndex index) object reference
    | index <- [0 .. controlIndexWord64 (cursor reference)],
      object <- objects
    ]
  where
    objects = Set.toAscList (Set.fromList [Oracle.liveDecisionObject (Projection.projectedLabelWorkflowDecision workflow) | (_, workflow) <- Projection.projectedLabelWorkflows reference])

entryIndex :: CanonicalAppliedOracleEntry -> ControlIndex
entryIndex = Entry.appliedEntryControlIndex . canonicalAppliedOracleEntryValue

caseDecodeAdmission :: Assertion
caseDecodeAdmission = do
  let donor = foldl' apply freshLocal (history 2)
      bytes = encode donor
      initialBase = capture freshLocal
      advanced = apply freshRemote (firstEntry (history 1))
      differentGenesis = Projection.initialState Fixtures.fixtureCheckedGenesis (Fixtures.fixtureCheckedInitialBootstrapsFor Fixtures.fixtureCheckedGenesis)
  assertBool "zero base round-trip" (Projection.decodeProjectionBase freshLocal (Projection.encodeProjectionBase initialBase) == Right initialBase)
  assertBool "nonfresh receiver rejected" (isLeft (Projection.decodeProjectionBase advanced bytes))
  assertBool "different immutable genesis rejected" (isLeft (Projection.decodeProjectionBase differentGenesis bytes))
  assertBool "initial voter seed must match donor" (isLeft (Projection.decodeProjectionBase (withVoters True freshRemote) bytes))
  assertBool "empty base rejected" (isLeft (Projection.decodeProjectionBase freshRemote ByteString.empty))
  assertBool "truncated base rejected" (isLeft (Projection.decodeProjectionBase freshRemote (ByteString.init bytes)))
  assertBool "trailing base bytes rejected" (isLeft (Projection.decodeProjectionBase freshRemote (bytes <> "x")))

type RawBase =
  ( ByteString,
    (ByteString, Maybe ByteString, [ByteString]),
    (ByteString, ByteString, ByteString),
    (Word64, Maybe ByteString),
    (Word64, [(Word64, ByteString)], [(Word64, ByteString, [(ByteString, Word64, ByteString)])])
  )

caseWireShape :: Assertion
caseWireShape = do
  let donor = foldl' apply freshLocal (history 2)
      raw@(domain, identity, (labels, disappearance, voters), (semantic, digest), (through, entries, advances)) = checked "decode base tuple" (Serialize.decode (encode donor) :: Either String RawBase)
      reject name claim = assertBool name (isLeft (Projection.decodeProjectionBase freshRemote (Serialize.encode claim)))
      mismatchedLabels = Projection.encodeProjectionLabelBase (checked "other-cut labels" (Projection.captureProjectionLabelBase freshLocal))
      wrongKeys = [(key + 1, entry) | (key, entry) <- entries]
      differentDigest = case digest of
        Nothing -> Just (ByteString.replicate 32 0)
        Just _ -> Nothing
      pins = Set.singleton (controlIndex 1)
      based = checked "wire-shape checked base" (Projection.advanceReplayBase donor)
      sparse = checked "wire-shape reclamation" (Projection.reclaimAppliedPrefixThroughBase pins Nothing based)
      (sparseDomain, sparseIdentity, sparseLeaves, sparseScalars, (covered, retained, detached)) = checked "decode sparse tuple" (Serialize.decode (encode sparse) :: Either String RawBase)
  reject "wrong aggregate domain" ("wrong-domain" :: ByteString, identity, (labels, disappearance, voters), (semantic, digest), (through, entries, advances))
  reject "label and identity frames bind the same cut" (domain, identity, (mismatchedLabels, disappearance, voters), (semantic, digest), (through, entries, advances))
  reject "semantic frontier covers current semantic facts" (domain, identity, (labels, disappearance, voters), (0 :: Word64, digest), (through, entries, advances))
  reject "a rejected command still advances the semantic frontier beyond the prior End" (domain, identity, (labels, disappearance, voters), (semantic - 1, digest), (through, entries, advances))
  reject "the final exact canonical entry fixes the diagnostic digest" (domain, identity, (labels, disappearance, voters), (semantic, differentDigest), (through, entries, advances))
  reject "covered prefix never exceeds source cut" (domain, identity, (labels, disappearance, voters), (semantic, digest), (1 + controlIndexWord64 (cursor donor), entries, advances))
  reject "uncovered suffix must be contiguous" (domain, identity, (labels, disappearance, voters), (semantic, digest), (through, drop 1 entries, advances))
  reject "canonical entry keys match their admitted indices" (domain, identity, (labels, disappearance, voters), (semantic, digest), (through, wrongKeys, advances))
  reject "duplicate exact entry keys rejected" (domain, identity, (labels, disappearance, voters), (semantic, digest), (through, entries <> take 1 entries, advances))
  reject "exact evidence map order is canonical" (domain, identity, (labels, disappearance, voters), (semantic, digest), (through, reverse entries, advances))
  reject "sparse pins cannot claim an uncovered gap" (sparseDomain, sparseIdentity, sparseLeaves, sparseScalars, (0 :: Word64, retained, detached))
  assertBool "arbitrary old pin subset is admitted below covered floor" (Projection.decodeProjectionBase freshRemote (Serialize.encode (sparseDomain, sparseIdentity, sparseLeaves, sparseScalars, (covered, retained, detached))) == Right (capture sparse))
  assertBool "raw fixture is the exact source encoding" (Serialize.encode raw == encode donor)

firstEntry :: [value] -> value
firstEntry (entry : _) = entry
firstEntry [] = error "portable fixture has no canonical entries"

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
