{-# LANGUAGE OverloadedStrings #-}

module LabelProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Serialize qualified as Serialize
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( HeraldPublicationPrefix (..),
    firstHeraldPublicationPosition,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    PublicationId,
    StructuralOccurrenceId,
    SystemId,
    TopologyCutId,
    controlIndex,
    genesisAuthorityEpoch,
    heraldEpochBytes,
    labelAuthorityEpoch,
    labelDecisionIdBytes,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkStructuralSequence,
    mkSystemId,
    mkTopologyCutId,
    nablaSequence,
    publicationId,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Label
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Sort.Profile
  ( CatalogueDigest,
    mkCatalogueDigest,
  )
import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    mkInitialProjectionDigest,
  )
import Eclips.Domain.Topology
  ( MemberSetDigest,
    deriveMemberSetDigest,
    memberSetDigestBytes,
  )
import Eclips.Domain.Value (Label, LabelOwner (..))
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "label domain"
    [ testProperty "label decision IDs require exactly 32 bytes" propDecisionIdWidth,
      testProperty "label evidence digests require exactly 32 bytes" propDigestWidths,
      testCase "checked nominal label bytes round trip exactly" caseCheckedByteRoundTrips,
      testCase "target and released-state views are total" caseViews,
      testProperty "bootstrap initial-label evidence fixes the raw owner and generation" propInitialBootstrapEvidence,
      testProperty "publication initial-label evidence preserves independent raw state and provenance" propInitialPublicationEvidence,
      testProperty "nonzero observed generations cannot establish initial-label evidence" propInitialEvidenceGeneration,
      testCase "the complete CAS/caller/target table is fixed" caseTransitionTable,
      testCase "an expected-value mismatch precedes transition rules" caseExpectedMismatch,
      testProperty "every admitted target advances exactly once" propGenerationSuccess,
      testProperty "same-owner success invalidates a competing expected pair" propSameOwnerRace,
      testProperty "owner ABA cannot revive an earlier expected pair" propGenerationABA,
      testProperty "generation mismatch and caller rejection cannot apply" propGenerationRejection,
      testProperty "prepared facts require the exact live or terminal successor" propPreparedGeneration,
      testCase "revisions and positions are positive" casePositivePositions,
      testCase "released records distinguish retained and applicable authority" caseLabelRecord,
      testCase "authority preparation handles same, changed, and deleted labels" caseAuthorityDisposition,
      testCase "compact readiness claims use contextual reporter and membership admission" caseReadyReport,
      testProperty "compact readiness decoding checks each nominal field width" propReadyFieldWidths,
      testCase "prepared facts enforce authority coherence and bind their digest" casePreparedFacts
    ]

propDecisionIdWidth :: Property
propDecisionIdWidth =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    counterexample ("byte count: " <> show byteCount)
      $ isLeft (mkLabelDecisionId (ByteString.replicate byteCount 0))
        === (byteCount /= 32)

propDigestWidths :: Property
propDigestWidths =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let rawBytes = ByteString.replicate byteCount 0
        malformed = byteCount /= 32
     in counterexample ("byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (mkPreparedLabelDigest rawBytes) === malformed,
              isLeft (mkLabelOutcomeDigest rawBytes) === malformed
            ]

caseCheckedByteRoundTrips :: Assertion
caseCheckedByteRoundTrips = do
  let rawBytes = ByteString.pack [0 .. 31]
  assertEqual
    "decision identity bytes"
    rawBytes
    (labelDecisionIdBytes (checked "decision identity" (mkLabelDecisionId rawBytes)))
  assertEqual
    "prepared digest bytes"
    rawBytes
    (preparedLabelDigestBytes (checked "prepared digest" (mkPreparedLabelDigest rawBytes)))
  assertEqual
    "outcome digest bytes"
    rawBytes
    (labelOutcomeDigestBytes (checked "outcome digest" (mkLabelOutcomeDigest rawBytes)))

caseViews :: Assertion
caseViews = do
  assertEqual
    "process target"
    (TargetProcessView processA)
    (labelTargetView (targetProcess processA))
  assertEqual "void target" TargetVoidView (labelTargetView targetVoid)
  assertEqual "delete target" TargetDeleteView (labelTargetView targetDelete)
  assertEqual
    "released label"
    (ReleasedLabelView ((ProcessLabel processA, 0)))
    (releasedLabelStateView (releasedLabel ((ProcessLabel processA, 0))))
  assertEqual
    "released delete"
    (ReleasedDeletedView 1)
    (releasedLabelStateView (releasedDeleted 1))

propInitialBootstrapEvidence :: Property
propInitialBootstrapEvidence =
  forAll (chooseInt (0, 255)) $ \seed ->
    forAll (elements [objectA, objectB]) $ \object ->
      let owner = process (fromIntegral seed)
          evidence = initialBootstrapLabelEvidence object owner
       in conjoin
            [ initialLabelEvidenceObject evidence === object,
              initialLabelEvidenceLabel evidence === (ProcessLabel owner, 0),
              initialLabelEvidenceSource evidence === InitialBootstrapLabelEvidenceView owner
            ]

propInitialPublicationEvidence :: Property
propInitialPublicationEvidence =
  forAll initialEvidencePublication $ \publication ->
    forAll (elements [objectA, objectB]) $ \object ->
      conjoin
        [ case initialPublicationLabelEvidence object raw publication of
            Nothing -> counterexample ("generation-zero observation rejected: " <> show raw) False
            Just evidence ->
              conjoin
                [ initialLabelEvidenceObject evidence === object,
                  initialLabelEvidenceLabel evidence === raw,
                  initialLabelEvidenceSource evidence === InitialPublicationLabelEvidenceView publication
                ]
        | owner <- [VoidLabel, ProcessLabel processA, ZombieLabel processB],
          let raw = (owner, 0)
        ]

propInitialEvidenceGeneration :: Property
propInitialEvidenceGeneration =
  forAll (chooseInt (1, 100000)) $ \observedGeneration ->
    forAll initialEvidencePublication $ \publication ->
      conjoin
        [ initialPublicationLabelEvidence object (owner, fromIntegral observedGeneration) publication === Nothing
        | object <- [objectA, objectB],
          owner <- [VoidLabel, ProcessLabel processA, ZombieLabel processB]
        ]

initialEvidencePublication :: Gen PublicationId
initialEvidencePublication =
  (\sequenceNumber source -> publicationId sourceNabla genesisAuthorityEpoch source (nablaSequence (fromIntegral sequenceNumber)))
    <$> chooseInt (1, 100000)
    <*> elements [heraldA, heraldB]
  where
    sourceNabla = checked "initial evidence source Nabla" (mkNablaId (bytes 62))

caseTransitionTable :: Assertion
caseTransitionTable =
  mapM_ checkTransition transitionCases
  where
    checkTransition (description, caller, expected, prior, target, outcome) =
      assertEqual description outcome (labelTransition caller expected prior target)

caseExpectedMismatch :: Assertion
caseExpectedMismatch = do
  let actual = releasedLabel ((ProcessLabel processA, 0))
  assertEqual
    "mismatch does not evaluate process ownership"
    (Right LabelExpectedMismatch)
    (labelTransition processB (VoidLabel, 0) actual targetVoid)
  assertEqual
    "mismatch does not evaluate the requested target"
    (Right LabelExpectedMismatch)
    (labelTransition processA (VoidLabel, 0) actual (targetProcess processB))

generation :: Gen Word64
generation = fromIntegral <$> chooseInt (0, 100000)

propGenerationSuccess :: Property
propGenerationSuccess =
  forAll generation $ \priorGeneration ->
    forAll (elements [ProcessLabel processA, VoidLabel, ZombieLabel processB]) $ \owner ->
      forAll (elements [targetProcess processA, targetVoid, targetDelete]) $ \target ->
        let prior = (owner, priorGeneration)
         in case labelTransition processA prior (releasedLabel prior) target of
              Right (LabelTransitionApplied outcome) ->
                releasedLabelStateGeneration outcome === priorGeneration + 1
              other -> counterexample (show other) False

propSameOwnerRace :: Property
propSameOwnerRace =
  forAll generation $ \priorGeneration ->
    let expected = (ProcessLabel processA, priorGeneration)
        first = checked "same-owner transition" (labelTransition processA expected (releasedLabel expected) (targetProcess processA))
     in case first of
          LabelTransitionApplied next ->
            conjoin
              [ next === releasedLabel (ProcessLabel processA, priorGeneration + 1),
                labelTransition processA expected next (targetProcess processB) === Right LabelExpectedMismatch,
                preparedAuthorityDisposition (Just genesisAuthorityEpoch) expected next === retainPriorAuthority genesisAuthorityEpoch
              ]
          LabelExpectedMismatch -> counterexample "the exact first compare must apply" False

propGenerationABA :: Property
propGenerationABA =
  forAll generation $ \priorGeneration ->
    let original = (ProcessLabel processA, priorGeneration)
        changed = (ProcessLabel processB, priorGeneration + 1)
        returned = releasedLabel (ProcessLabel processA, priorGeneration + 2)
     in conjoin
          [ labelTransition processA original (releasedLabel original) (targetProcess processB)
              === Right (LabelTransitionApplied (releasedLabel changed)),
            labelTransition processB changed (releasedLabel changed) (targetProcess processA)
              === Right (LabelTransitionApplied returned),
            labelTransition processA original returned targetVoid === Right LabelExpectedMismatch
          ]

propGenerationRejection :: Property
propGenerationRejection =
  forAll generation $ \priorGeneration ->
    let actual = (ProcessLabel processA, priorGeneration)
     in conjoin
          [ labelTransition processA (ProcessLabel processA, priorGeneration + 1) (releasedLabel actual) targetDelete
              === Right LabelExpectedMismatch,
            labelTransition processB actual (releasedLabel actual) targetVoid
              === Left (LabelTransitionCallerMismatch processA processB),
            labelTransition processA (VoidLabel, priorGeneration) (releasedDeleted priorGeneration) targetVoid
              === Left LabelTransitionFromDeleted
          ]

propPreparedGeneration :: Property
propPreparedGeneration =
  forAll generation $ \priorGeneration ->
    forAll (elements [False, True]) $ \deleted ->
      let outcome next = if deleted then releasedDeleted next else releasedLabel (VoidLabel, next)
          prepare next =
            mkPreparedLabelFacts
              decisionA
              (controlIndex 4)
              objectA
              (outcome next)
              (releasedLabel (VoidLabel, priorGeneration))
              Nothing
              catalogueDigest
              Nothing
              Nothing
              noTargetAuthority
              membersDigest
          admitted = checked "successor preparation" (prepare (priorGeneration + 1))
       in conjoin
            [ prepare priorGeneration === Left (PreparedLabelFactsGenerationMismatch (priorGeneration + 1) priorGeneration),
              prepare (priorGeneration + 2) === Left (PreparedLabelFactsGenerationMismatch (priorGeneration + 1) (priorGeneration + 2)),
              decodePreparedLabelFactsCanonicalBytes (preparedLabelFactsCanonicalBytes admitted) === Right admitted,
              ( derivePreparedLabelDigest admitted
                  /= derivePreparedLabelDigest
                    ( checked
                        "next generation preparation"
                        ( mkPreparedLabelFacts
                            decisionA
                            (controlIndex 4)
                            objectA
                            (outcome (priorGeneration + 2))
                            (releasedLabel (VoidLabel, priorGeneration + 1))
                            Nothing
                            catalogueDigest
                            Nothing
                            Nothing
                            noTargetAuthority
                            membersDigest
                        )
                    )
              )
                === True
            ]

transitionCases ::
  [ ( String,
      ProcessEpochId,
      Label,
      ReleasedLabelState,
      LabelTarget,
      Either LabelTransitionError LabelTransitionOutcome
    )
  ]
transitionCases =
  [ ( "process owner may hand off to a nameable process",
      processA,
      (ProcessLabel processA, 0),
      releasedLabel ((ProcessLabel processA, 0)),
      targetProcess processB,
      Right (LabelTransitionApplied (releasedLabel ((ProcessLabel processB, 1))))
    ),
    ( "process owner may choose void",
      processA,
      (ProcessLabel processA, 0),
      releasedLabel ((ProcessLabel processA, 0)),
      targetVoid,
      Right (LabelTransitionApplied (releasedLabel (VoidLabel, 1)))
    ),
    ( "process owner may delete",
      processA,
      (ProcessLabel processA, 0),
      releasedLabel ((ProcessLabel processA, 0)),
      targetDelete,
      Right (LabelTransitionApplied (releasedDeleted 1))
    ),
    ( "process owner may perform a same-label release",
      processA,
      (ProcessLabel processA, 0),
      releasedLabel ((ProcessLabel processA, 0)),
      targetProcess processA,
      Right (LabelTransitionApplied (releasedLabel ((ProcessLabel processA, 1))))
    ),
    ( "another caller cannot change a process label",
      processB,
      (ProcessLabel processA, 0),
      releasedLabel ((ProcessLabel processA, 0)),
      targetVoid,
      Left (LabelTransitionCallerMismatch processA processB)
    ),
    ( "void may name the caller",
      processA,
      (VoidLabel, 0),
      releasedLabel (VoidLabel, 0),
      targetProcess processA,
      Right (LabelTransitionApplied (releasedLabel ((ProcessLabel processA, 1))))
    ),
    ( "void may stay void",
      processA,
      (VoidLabel, 0),
      releasedLabel (VoidLabel, 0),
      targetVoid,
      Right (LabelTransitionApplied (releasedLabel (VoidLabel, 1)))
    ),
    ( "void may delete",
      processA,
      (VoidLabel, 0),
      releasedLabel (VoidLabel, 0),
      targetDelete,
      Right (LabelTransitionApplied (releasedDeleted 1))
    ),
    ( "void cannot name another process",
      processA,
      (VoidLabel, 0),
      releasedLabel (VoidLabel, 0),
      targetProcess processB,
      Left (LabelTransitionTargetMustBeCaller processA processB)
    ),
    ( "an expected zombie may name the caller",
      processA,
      (ZombieLabel processB, 0),
      releasedLabel ((ZombieLabel processB, 0)),
      targetProcess processA,
      Right (LabelTransitionApplied (releasedLabel ((ProcessLabel processA, 1))))
    ),
    ( "an expected zombie may choose void",
      processA,
      (ZombieLabel processB, 0),
      releasedLabel ((ZombieLabel processB, 0)),
      targetVoid,
      Right (LabelTransitionApplied (releasedLabel (VoidLabel, 1)))
    ),
    ( "an expected zombie may delete",
      processA,
      (ZombieLabel processB, 0),
      releasedLabel ((ZombieLabel processB, 0)),
      targetDelete,
      Right (LabelTransitionApplied (releasedDeleted 1))
    ),
    ( "an expected zombie cannot name another process",
      processA,
      (ZombieLabel processB, 0),
      releasedLabel ((ZombieLabel processB, 0)),
      targetProcess processB,
      Left (LabelTransitionTargetMustBeCaller processA processB)
    ),
    ( "deleted is terminal",
      processA,
      (VoidLabel, 0),
      (releasedDeleted 1),
      targetVoid,
      Left LabelTransitionFromDeleted
    )
  ]

casePositivePositions :: Assertion
casePositivePositions = do
  assertEqual
    "release index zero is not a revision"
    (Left LabelRevisionMustBePositive)
    (mkLabelRevision (controlIndex 0))
  let revision = checked "label revision" (mkLabelRevision (controlIndex 7))
  assertEqual "revision retains its exact control index" (controlIndex 7) (labelRevisionControlIndex revision)
  assertEqual
    "acceptance ordinal zero is rejected"
    (Left LabelProcessAcceptancePositionMustBePositive)
    (mkLabelProcessAcceptancePosition processA 0)
  let firstPosition = firstLabelProcessAcceptancePosition processA
      secondPosition = nextLabelProcessAcceptancePosition firstPosition
      cut =
        homeLabelAcceptanceCut
          secondPosition
          (HeraldPublicationPrefixThrough firstHeraldPublicationPosition)
  assertEqual "first acceptance ordinal" 1 (labelProcessAcceptancePositionOrdinal firstPosition)
  assertEqual "next acceptance ordinal" 2 (labelProcessAcceptancePositionOrdinal secondPosition)
  assertEqual "position retains process" processA (labelProcessAcceptancePositionProcess secondPosition)
  assertEqual "cut derives the same caller" processA (homeLabelAcceptanceCutCallerProcessEpoch cut)
  assertEqual "cut retains the position" secondPosition (homeLabelAcceptanceCutProcessPosition cut)
  assertEqual
    "cut retains an explicit publication prefix"
    (HeraldPublicationPrefixThrough firstHeraldPublicationPosition)
    (homeLabelAcceptanceCutPublicationPrefix cut)
  let cutBytes = homeLabelAcceptanceCutCanonicalBytes cut
  assertEqual
    "home acceptance cut canonical bytes decode through checked positions"
    (Right cut)
    (decodeHomeLabelAcceptanceCutCanonicalBytes cutBytes)
  assertBool
    "home acceptance cut rejects the wrong canonical domain"
    ( isLeft
        ( decodeHomeLabelAcceptanceCutCanonicalBytes
            (corruptDomain "ECLIPS-HOME-LABEL-ACCEPTANCE-CUT" cutBytes)
        )
    )
  assertBool
    "home acceptance cut rechecks the positive publication position"
    ( isLeft
        ( decodeHomeLabelAcceptanceCutCanonicalBytes
            (replaceFinalWord64WithZero cutBytes)
        )
    )

caseLabelRecord :: Assertion
caseLabelRecord = do
  let revision = checked "record revision" (mkLabelRevision (controlIndex 2))
      liveRecord =
        checked
          "live label record"
          ( mkLabelRecord
              objectA
              (releasedLabel ((ProcessLabel processA, 0)))
              revision
              (Just genesisAuthorityEpoch)
          )
      deletedRecord =
        checked
          "deleted label record"
          ( mkLabelRecord
              objectA
              (releasedDeleted 1)
              revision
              (Just genesisAuthorityEpoch)
          )
  assertEqual "record object" objectA (labelRecordObjectId liveRecord)
  assertEqual "record state" (releasedLabel ((ProcessLabel processA, 0))) (labelRecordReleasedState liveRecord)
  assertEqual "record revision" revision (labelRecordRevision liveRecord)
  assertEqual "live authority remains applicable" (Just genesisAuthorityEpoch) (labelRecordApplicableAuthority (const False) liveRecord)
  assertEqual "ended operator has no applicable authority" Nothing (labelRecordApplicableAuthority (== processA) liveRecord)
  assertEqual "delete retains historical authority" (Just genesisAuthorityEpoch) (labelRecordRetainedAuthority deletedRecord)
  assertEqual "delete retires applicability" Nothing (labelRecordApplicableAuthority (const False) deletedRecord)
  let voidRecord =
        checked
          "void label record"
          (mkLabelRecord objectA (releasedLabel (VoidLabel, 0)) revision (Just genesisAuthorityEpoch))
  assertEqual "void retains only historical authority" Nothing (labelRecordApplicableAuthority (const False) voidRecord)
  assertEqual
    "a projected zombie cannot be stored"
    (Left (LabelRecordStoredZombieNotPermitted processA))
    ( mkLabelRecord
        objectA
        (releasedLabel ((ZombieLabel processA, 0)))
        revision
        (Just genesisAuthorityEpoch)
    )

caseAuthorityDisposition :: Assertion
caseAuthorityDisposition = do
  let prior = (ProcessLabel processA, 0)
  assertEqual
    "ordinary target has no authority"
    NoTargetAuthorityView
    ( preparedAuthorityDispositionView
        (preparedAuthorityDisposition Nothing prior (releasedLabel (VoidLabel, 0)))
    )
  assertEqual
    "same effective owner retains authority while generation advances"
    (RetainPriorAuthorityView genesisAuthorityEpoch)
    ( preparedAuthorityDispositionView
        ( preparedAuthorityDisposition
            (Just genesisAuthorityEpoch)
            prior
            (releasedLabel (fst prior, snd prior + 1))
        )
    )
  assertEqual
    "void retains the last tenure only as history"
    (RetainPriorAuthorityView genesisAuthorityEpoch)
    ( preparedAuthorityDispositionView
        ( preparedAuthorityDisposition
            (Just genesisAuthorityEpoch)
            prior
            (releasedLabel (VoidLabel, 0))
        )
    )
  assertEqual
    "a different process operator derives at release"
    DeriveAuthorityAtReleaseView
    ( preparedAuthorityDispositionView
        ( preparedAuthorityDisposition
            (Just genesisAuthorityEpoch)
            prior
            (releasedLabel ((ProcessLabel processB, 0)))
        )
    )
  assertEqual
    "a forced zombie to live transition derives at release"
    DeriveAuthorityAtReleaseView
    ( preparedAuthorityDispositionView
        ( preparedAuthorityDisposition
            (Just genesisAuthorityEpoch)
            ((ZombieLabel processA, 0))
            (releasedLabel ((ProcessLabel processB, 0)))
        )
    )
  assertEqual
    "void to void retains the prior tenure only as history"
    (RetainPriorAuthorityView genesisAuthorityEpoch)
    ( preparedAuthorityDispositionView
        ( preparedAuthorityDisposition
            (Just genesisAuthorityEpoch)
            (VoidLabel, 0)
            (releasedLabel (VoidLabel, 0))
        )
    )
  assertEqual
    "delete retires the prior tenure"
    (RetirePriorAuthorityView genesisAuthorityEpoch)
    ( preparedAuthorityDispositionView
        ( preparedAuthorityDisposition
            (Just genesisAuthorityEpoch)
            prior
            (releasedDeleted 1)
        )
    )
  assertEqual
    "existing-record justification view"
    (ExistingReleasedAuthorityView revisionOne)
    (priorAuthorityJustificationView (existingReleasedAuthority revisionOne))
  assertEqual
    "checked-genesis justification view"
    (CheckedGenesisAuthorityView initialDigest)
    (priorAuthorityJustificationView (checkedGenesisAuthority initialDigest))
  assertEqual
    "established-structural justification view"
    (EstablishedStructuralAuthorityView occurrence topologyCut)
    ( priorAuthorityJustificationView
        (establishedStructuralAuthority occurrence topologyCut)
    )

caseReadyReport :: Assertion
caseReadyReport = do
  let valid = mkFenceReadyReport decisionA heraldA membersDigest
      canonicalReady = fenceReadyReportCanonicalBytes valid
  assertEqual "ready decision" decisionA (fenceReadyReportDecisionId valid)
  assertEqual "ready reporter" heraldA (fenceReadyReportReporter valid)
  assertEqual "ready member digest" membersDigest (fenceReadyReportMemberSetDigest valid)
  assertEqual "compact canonical Ready round trips" (Right valid) (decodeFenceReadyReportCanonicalBytes canonicalReady)
  let qualifiedReady = checked "generation-qualified ready report" (qualifyFenceReadyReport membership valid)
  assertEqual "ready evidence retains its exact generation" (heraldMembershipGenerationId membership) (generationQualifiedFenceReadyReportGeneration qualifiedReady)
  assertEqual "qualification retains the compact claim" valid (generationQualifiedFenceReadyReportValue qualifiedReady)
  assertBool "qualified transcript binds generation lineage" (generationQualifiedFenceReadyReportCanonicalBytes qualifiedReady /= canonicalReady)
  assertEqual
    "qualification rejects another captured member set"
    (Left (GenerationQualifiedReadyMemberSetMismatch otherMembersDigest membersDigest))
    (qualifyFenceReadyReport otherMembership valid)
  let foreignReporter = mkFenceReadyReport decisionA heraldD membersDigest
  assertEqual
    "canonical decoding does not invent workflow membership"
    (Right foreignReporter)
    (decodeFenceReadyReportCanonicalBytes (fenceReadyReportCanonicalBytes foreignReporter))
  assertEqual
    "contextual qualification rejects an uncaptured reporter"
    (Left (GenerationQualifiedReadyReporterNotMember heraldD))
    (qualifyFenceReadyReport membership foreignReporter)
  assertBool
    "ready report rejects a different canonical domain"
    (isLeft (decodeFenceReadyReportCanonicalBytes (corruptDomain "ECLIPS-LABEL-FENCE-READY" canonicalReady)))
  assertBool
    "ready report rejects truncated canonical fields"
    (isLeft (decodeFenceReadyReportCanonicalBytes (ByteString.init canonicalReady)))
  assertBool
    "ready report rejects trailing canonical bytes"
    (isLeft (decodeFenceReadyReportCanonicalBytes (canonicalReady <> ByteString.singleton 0)))
  assertEqual
    "report bytes contain no rows proportional to captured membership"
    (ByteString.length canonicalReady)
    (ByteString.length (fenceReadyReportCanonicalBytes (mkFenceReadyReport decisionA heraldA (deriveMemberSetDigest (heraldA :| [])))))

propReadyFieldWidths :: Property
propReadyFieldWidths =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let report = mkFenceReadyReport decisionA heraldA membersDigest
        canonical = fenceReadyReportCanonicalBytes report
        replace original = replaceCanonicalField original (ByteString.replicate byteCount 9) canonical
        malformed = byteCount /= 32
     in counterexample ("readiness field byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (decodeFenceReadyReportCanonicalBytes (replace (labelDecisionIdBytes decisionA))) === malformed,
              isLeft (decodeFenceReadyReportCanonicalBytes (replace (heraldEpochBytes heraldA))) === malformed,
              isLeft (decodeFenceReadyReportCanonicalBytes (replace (memberSetDigestBytes membersDigest))) === malformed
            ]

-- Replace one length-framed field without damaging its surrounding transcript,
-- so these cases exercise nominal field admission rather than truncation alone.
replaceCanonicalField :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString
replaceCanonicalField original replacement canonical =
  let encoded = Serialize.encode original
      (before, remaining) = ByteString.breakSubstring encoded canonical
   in if ByteString.null remaining
        then error "canonical Ready fixture omitted its expected field"
        else before <> Serialize.encode replacement <> ByteString.drop (ByteString.length encoded) remaining

casePreparedFacts :: Assertion
casePreparedFacts = do
  let facts description decisionValue resolve object proposed prior revision catalogue authority justification disposition memberDigest =
        checked
          description
          ( mkPreparedLabelFacts
              decisionValue
              resolve
              object
              proposed
              prior
              revision
              catalogue
              authority
              justification
              disposition
              memberDigest
          )
      ordinaryFacts =
        facts
          "ordinary prepared facts"
          decisionA
          (controlIndex 4)
          objectA
          (releasedLabel (VoidLabel, 1))
          (releasedLabel (VoidLabel, 0))
          Nothing
          catalogueDigest
          Nothing
          Nothing
          noTargetAuthority
          membersDigest
      retainedFacts =
        facts
          "retained-authority prepared facts"
          decisionA
          (controlIndex 4)
          objectA
          (releasedLabel ((ProcessLabel processA, 1)))
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          catalogueDigest
          (Just genesisAuthorityEpoch)
          (Just (checkedGenesisAuthority initialDigest))
          (retainPriorAuthority genesisAuthorityEpoch)
          membersDigest
      deletedFacts =
        facts
          "retired-authority prepared facts"
          decisionA
          (controlIndex 4)
          objectA
          (releasedDeleted 1)
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          catalogueDigest
          (Just structuralAuthority)
          (Just (establishedStructuralAuthority occurrence topologyCut))
          (retirePriorAuthority structuralAuthority)
          membersDigest
      derivedFacts =
        facts
          "derived-authority prepared facts"
          decisionA
          (controlIndex 4)
          objectA
          (releasedLabel ((ProcessLabel processB, 1)))
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          catalogueDigest
          (Just genesisAuthorityEpoch)
          (Just (checkedGenesisAuthority initialDigest))
          deriveAuthorityAtRelease
          membersDigest
      existingDerivedFacts authority =
        facts
          "existing-authority prepared facts"
          decisionA
          (controlIndex 4)
          objectA
          (releasedLabel ((ProcessLabel processB, 1)))
          (releasedLabel ((ProcessLabel processA, 0)))
          (Just revisionOne)
          catalogueDigest
          (Just authority)
          (Just (existingReleasedAuthority revisionOne))
          deriveAuthorityAtRelease
          membersDigest
      genesisFacts digest =
        facts
          "genesis-authority prepared facts"
          decisionA
          (controlIndex 4)
          objectA
          (releasedLabel ((ProcessLabel processA, 1)))
          (releasedLabel ((ProcessLabel processA, 0)))
          Nothing
          catalogueDigest
          (Just genesisAuthorityEpoch)
          (Just (checkedGenesisAuthority digest))
          (retainPriorAuthority genesisAuthorityEpoch)
          membersDigest
      zombieFacts =
        facts
          "zombie-prior prepared facts"
          decisionA
          (controlIndex 4)
          objectA
          (releasedLabel ((ProcessLabel processB, 1)))
          (releasedLabel ((ZombieLabel processA, 0)))
          Nothing
          catalogueDigest
          (Just genesisAuthorityEpoch)
          (Just (checkedGenesisAuthority initialDigest))
          deriveAuthorityAtRelease
          membersDigest
      labelAuthorityFacts =
        existingDerivedFacts (labelAuthorityEpoch (controlIndex 3))
  mapM_
    ( \expected ->
        assertEqual
          "prepared facts canonical bytes decode through nested admission"
          (Right expected)
          ( decodePreparedLabelFactsCanonicalBytes
              (preparedLabelFactsCanonicalBytes expected)
          )
    )
    [ ordinaryFacts,
      retainedFacts,
      deletedFacts,
      derivedFacts,
      labelAuthorityFacts,
      zombieFacts
    ]
  assertBool
    "prepared facts reject the wrong canonical domain"
    ( isLeft
        ( decodePreparedLabelFactsCanonicalBytes
            ( corruptDomain
                "ECLIPS-PREPARED-LABEL-FACTS"
                (preparedLabelFactsCanonicalBytes ordinaryFacts)
            )
        )
    )
  let ordinaryBytes = preparedLabelFactsCanonicalBytes ordinaryFacts
      zeroResolveIndex =
        replaceFirstSubsequence
          (bytes 50 <> word64Four)
          (bytes 50 <> ByteString.replicate 8 0)
          ordinaryBytes
      labelAuthorityBytes =
        preparedLabelFactsCanonicalBytes labelAuthorityFacts
      unknownAuthorityTag =
        replaceFirstSubsequence
          (ByteString.singleton 2 <> word64Three)
          (ByteString.singleton 3 <> word64Three)
          labelAuthorityBytes
  assertBool
    "prepared facts decoding rechecks the positive resolve index"
    (isLeft (decodePreparedLabelFactsCanonicalBytes zeroResolveIndex))
  assertBool
    "prepared facts decoding rejects an unknown nested authority tag"
    (isLeft (decodePreparedLabelFactsCanonicalBytes unknownAuthorityTag))
  assertEqual "facts retain decision" decisionA (preparedLabelFactsDecisionId ordinaryFacts)
  assertEqual "facts retain resolve index" (controlIndex 4) (preparedLabelFactsResolveIndex ordinaryFacts)
  assertEqual "facts retain object" objectA (preparedLabelFactsObjectId ordinaryFacts)
  assertEqual "facts retain outcome" (releasedLabel (VoidLabel, 1)) (preparedLabelFactsProposedOutcome ordinaryFacts)
  assertEqual "facts retain expected prior state" (releasedLabel (VoidLabel, 0)) (preparedLabelFactsExpectedPriorState ordinaryFacts)
  assertEqual "facts retain expected prior revision" Nothing (preparedLabelFactsExpectedPriorRevision ordinaryFacts)
  assertEqual "facts retain catalogue" catalogueDigest (preparedLabelFactsCatalogueDigest ordinaryFacts)
  assertEqual "facts retain member digest" membersDigest (preparedLabelFactsMemberSetDigest ordinaryFacts)
  let qualifiedFacts =
        checked
          "generation-qualified prepared facts"
          (qualifyPreparedLabelFacts membership ordinaryFacts)
  assertEqual
    "prepared evidence retains the exact membership generation"
    (heraldMembershipGenerationId membership)
    (generationQualifiedPreparedLabelFactsGeneration qualifiedFacts)
  assertEqual
    "prepared evidence retains the unchanged live facts"
    ordinaryFacts
    (generationQualifiedPreparedLabelFactsValue qualifiedFacts)
  assertBool
    "the prepared qualified transcript adds exact generation lineage"
    ( generationQualifiedPreparedLabelFactsCanonicalBytes qualifiedFacts
        /= preparedLabelFactsCanonicalBytes ordinaryFacts
    )
  assertEqual
    "prepared evidence rejects a generation with another member set"
    ( Left
        ( GenerationQualifiedPreparedMemberSetMismatch
            otherMembersDigest
            membersDigest
        )
    )
    (qualifyPreparedLabelFacts otherMembership ordinaryFacts)
  assertEqual
    "resolve index must be positive"
    (Left PreparedLabelFactsResolveIndexMustBePositive)
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 0)
        objectA
        (releasedLabel (VoidLabel, 1))
        (releasedLabel (VoidLabel, 0))
        Nothing
        catalogueDigest
        Nothing
        Nothing
        noTargetAuthority
        membersDigest
    )
  assertEqual
    "a deleted prior record cannot be prepared again"
    (Left PreparedLabelFactsExpectedPriorDeleted)
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel (VoidLabel, 0))
        (releasedDeleted 1)
        (Just revisionOne)
        catalogueDigest
        Nothing
        Nothing
        noTargetAuthority
        membersDigest
    )
  assertEqual
    "authority requires justification"
    (Left (PreparedLabelFactsMissingAuthorityJustification genesisAuthorityEpoch))
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel (VoidLabel, 1))
        (releasedLabel ((ProcessLabel processA, 0)))
        Nothing
        catalogueDigest
        (Just genesisAuthorityEpoch)
        Nothing
        (retainPriorAuthority genesisAuthorityEpoch)
        membersDigest
    )
  assertEqual
    "zombie is not a proposed caller target"
    (Left (PreparedLabelFactsZombieOutcomeNotPermitted processA))
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel ((ZombieLabel processA, 1)))
        (releasedLabel (VoidLabel, 0))
        Nothing
        catalogueDigest
        Nothing
        Nothing
        noTargetAuthority
        membersDigest
    )
  assertEqual
    "the authority disposition is derived from prior and proposed labels"
    ( Left
        ( PreparedLabelFactsAuthorityDispositionMismatch
            (retainPriorAuthority genesisAuthorityEpoch)
            deriveAuthorityAtRelease
        )
    )
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel (VoidLabel, 1))
        (releasedLabel ((ProcessLabel processA, 0)))
        Nothing
        catalogueDigest
        (Just genesisAuthorityEpoch)
        (Just (checkedGenesisAuthority initialDigest))
        deriveAuthorityAtRelease
        membersDigest
    )
  assertEqual
    "ordinary target rejects authority justification"
    (Left (PreparedLabelFactsUnexpectedAuthorityJustification (checkedGenesisAuthority initialDigest)))
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel (VoidLabel, 1))
        (releasedLabel (VoidLabel, 0))
        Nothing
        catalogueDigest
        Nothing
        (Just (checkedGenesisAuthority initialDigest))
        noTargetAuthority
        membersDigest
    )
  assertEqual
    "a checked-genesis justification supports only genesis authority"
    ( Left
        ( PreparedLabelFactsAuthorityJustificationMismatch
            structuralAuthority
            (checkedGenesisAuthority initialDigest)
        )
    )
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel ((ProcessLabel processA, 1)))
        (releasedLabel ((ProcessLabel processA, 0)))
        Nothing
        catalogueDigest
        (Just structuralAuthority)
        (Just (checkedGenesisAuthority initialDigest))
        (retainPriorAuthority structuralAuthority)
        membersDigest
    )
  assertEqual
    "a structural justification supports only its exact structural authority"
    ( Left
        ( PreparedLabelFactsAuthorityJustificationMismatch
            genesisAuthorityEpoch
            (establishedStructuralAuthority occurrence topologyCut)
        )
    )
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel ((ProcessLabel processA, 1)))
        (releasedLabel ((ProcessLabel processA, 0)))
        Nothing
        catalogueDigest
        (Just genesisAuthorityEpoch)
        (Just (establishedStructuralAuthority occurrence topologyCut))
        (retainPriorAuthority genesisAuthorityEpoch)
        membersDigest
    )
  assertEqual
    "a genesis justification cannot explain an existing released revision"
    ( Left
        ( PreparedLabelFactsRevisionJustificationMismatch
            (Just revisionOne)
            (checkedGenesisAuthority initialDigest)
        )
    )
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel ((ProcessLabel processA, 1)))
        (releasedLabel ((ProcessLabel processA, 0)))
        (Just revisionOne)
        catalogueDigest
        (Just genesisAuthorityEpoch)
        (Just (checkedGenesisAuthority initialDigest))
        (retainPriorAuthority genesisAuthorityEpoch)
        membersDigest
    )
  assertEqual
    "an existing-authority justification names the exact prior revision"
    ( Left
        ( PreparedLabelFactsRevisionJustificationMismatch
            (Just revisionTwo)
            (existingReleasedAuthority revisionOne)
        )
    )
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel ((ProcessLabel processA, 1)))
        (releasedLabel ((ProcessLabel processA, 0)))
        (Just revisionTwo)
        catalogueDigest
        (Just genesisAuthorityEpoch)
        (Just (existingReleasedAuthority revisionOne))
        (retainPriorAuthority genesisAuthorityEpoch)
        membersDigest
    )
  assertEqual
    "delete must retire rather than derive authority"
    ( Left
        ( PreparedLabelFactsAuthorityDispositionMismatch
            (retirePriorAuthority genesisAuthorityEpoch)
            deriveAuthorityAtRelease
        )
    )
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedDeleted 1)
        (releasedLabel ((ProcessLabel processA, 0)))
        Nothing
        catalogueDigest
        (Just genesisAuthorityEpoch)
        (Just (checkedGenesisAuthority initialDigest))
        deriveAuthorityAtRelease
        membersDigest
    )
  assertEqual
    "a non-delete outcome cannot retire authority"
    ( Left
        ( PreparedLabelFactsAuthorityDispositionMismatch
            (retainPriorAuthority genesisAuthorityEpoch)
            (retirePriorAuthority genesisAuthorityEpoch)
        )
    )
    ( mkPreparedLabelFacts
        decisionA
        (controlIndex 4)
        objectA
        (releasedLabel (VoidLabel, 1))
        (releasedLabel ((ProcessLabel processA, 0)))
        Nothing
        catalogueDigest
        (Just genesisAuthorityEpoch)
        (Just (checkedGenesisAuthority initialDigest))
        (retirePriorAuthority genesisAuthorityEpoch)
        membersDigest
    )
  let digests = fmap derivePreparedLabelDigest [ordinaryFacts, retainedFacts, deletedFacts, derivedFacts]
  assertEqual "distinct typed preparation facts have distinct digests" 4 (length (unique digests))
  let ordinaryMutations =
        [ ( "decision",
            facts "changed decision" decisionB (controlIndex 4) objectA (releasedLabel (VoidLabel, 1)) (releasedLabel (VoidLabel, 0)) Nothing catalogueDigest Nothing Nothing noTargetAuthority membersDigest
          ),
          ( "resolve index",
            facts "changed resolve index" decisionA (controlIndex 5) objectA (releasedLabel (VoidLabel, 1)) (releasedLabel (VoidLabel, 0)) Nothing catalogueDigest Nothing Nothing noTargetAuthority membersDigest
          ),
          ( "object",
            facts "changed object" decisionA (controlIndex 4) objectB (releasedLabel (VoidLabel, 1)) (releasedLabel (VoidLabel, 0)) Nothing catalogueDigest Nothing Nothing noTargetAuthority membersDigest
          ),
          ( "proposed outcome",
            facts "changed outcome" decisionA (controlIndex 4) objectA (releasedLabel ((ProcessLabel processA, 1))) (releasedLabel (VoidLabel, 0)) Nothing catalogueDigest Nothing Nothing noTargetAuthority membersDigest
          ),
          ( "expected prior state",
            facts "changed prior state" decisionA (controlIndex 4) objectA (releasedLabel (VoidLabel, 1)) (releasedLabel ((ProcessLabel processA, 0))) Nothing catalogueDigest Nothing Nothing noTargetAuthority membersDigest
          ),
          ( "expected prior revision",
            facts "changed prior revision" decisionA (controlIndex 4) objectA (releasedLabel (VoidLabel, 1)) (releasedLabel (VoidLabel, 0)) (Just revisionOne) catalogueDigest Nothing Nothing noTargetAuthority membersDigest
          ),
          ( "catalogue digest",
            facts "changed catalogue" decisionA (controlIndex 4) objectA (releasedLabel (VoidLabel, 1)) (releasedLabel (VoidLabel, 0)) Nothing otherCatalogueDigest Nothing Nothing noTargetAuthority membersDigest
          ),
          ( "member-set digest",
            facts "changed members" decisionA (controlIndex 4) objectA (releasedLabel (VoidLabel, 1)) (releasedLabel (VoidLabel, 0)) Nothing catalogueDigest Nothing Nothing noTargetAuthority otherMembersDigest
          )
        ]
      ordinaryDigest = derivePreparedLabelDigest ordinaryFacts
  mapM_
    ( \(field, changed) ->
        assertBool
          (field <> " is digest-significant")
          (derivePreparedLabelDigest changed /= ordinaryDigest)
    )
    ordinaryMutations
  assertBool
    "expected prior authority is digest-significant"
    ( derivePreparedLabelDigest (existingDerivedFacts structuralAuthority)
        /= derivePreparedLabelDigest (existingDerivedFacts genesisAuthorityEpoch)
    )
  assertBool
    "prior-authority justification is digest-significant"
    ( derivePreparedLabelDigest (genesisFacts otherInitialDigest)
        /= derivePreparedLabelDigest (genesisFacts initialDigest)
    )
  assertEqual
    "prepared digest golden"
    "cca1836ef44ca48ff45dd791515d9ecd5c05f680292206c3678f3484077c28f0"
    (hexBytes (preparedLabelDigestBytes ordinaryDigest))
  assertEqual
    "prepared digest transcript tags and optional fields have pinned vectors"
    [ "cca1836ef44ca48ff45dd791515d9ecd5c05f680292206c3678f3484077c28f0",
      "84fc9302324621c4c2db45c4843b2b523376b944fd0cc0b9c1520a382aa6568c",
      "64e5bbfc20c4e2bcc6a067b6e1190297fad1af0a5414f84cab81ac0805340b89",
      "56144ca3143a2e6950700a3b3fd7658fb1bbf5b3ffae5d6b891a66a49486053a",
      "e41c9df5477c69d6b28c2d0887a24ab4cc5d48ed446d3c074577f93732cbf570",
      "1b0827297afa29e925ba53dd1df3d1a743b4a0a95512e57c5aab4f99dd994ce9"
    ]
    ( fmap
        (hexBytes . preparedLabelDigestBytes . derivePreparedLabelDigest)
        [ ordinaryFacts,
          retainedFacts,
          deletedFacts,
          derivedFacts,
          existingDerivedFacts genesisAuthorityEpoch,
          zombieFacts
        ]
    )
  let report = preparedLabelReport heraldA ordinaryFacts
      expectedDigest = derivePreparedLabelDigest ordinaryFacts
      wrongDigest = checked "wrong prepared digest" (mkPreparedLabelDigest (ByteString.replicate 32 77))
  assertEqual "report retains reporter" heraldA (preparedLabelReportReporter report)
  assertEqual "report retains facts" ordinaryFacts (preparedLabelReportFacts report)
  assertEqual "report derives digest" expectedDigest (preparedLabelReportDigest report)
  assertEqual
    "matching claimed digest admits"
    (Right report)
    (admitPreparedLabelReport heraldA ordinaryFacts expectedDigest)
  assertEqual
    "unequal claimed digest is rejected"
    (Left (PreparedLabelReportDigestMismatch expectedDigest wrongDigest))
    (admitPreparedLabelReport heraldA ordinaryFacts wrongDigest)

unique :: (Eq value) => [value] -> [value]
unique = foldr (\value values -> if value `elem` values then values else value : values) []

processA, processB :: ProcessEpochId
processA = process 1
processB = process 2

process :: Word -> ProcessEpochId
process byte = checked "process" (mkProcessEpochId (bytes byte))

heraldA, heraldB, heraldC, heraldD :: HeraldEpoch
heraldA = herald 10
heraldB = herald 20
heraldC = herald 30
heraldD = herald 40

herald :: Word -> HeraldEpoch
herald byte = checked "Herald" (mkHeraldEpoch (bytes byte))

members :: NonEmpty HeraldEpoch
members = heraldA :| [heraldB, heraldC]

membersDigest :: MemberSetDigest
membersDigest = deriveMemberSetDigest members

otherMembersDigest :: MemberSetDigest
otherMembersDigest = deriveMemberSetDigest (heraldA :| [heraldC])

membership, otherMembership :: HeraldMembershipGeneration
membership =
  checked
    "label evidence membership"
    (genesisHeraldMembershipGeneration system members)
otherMembership =
  checked
    "other label evidence membership"
    (genesisHeraldMembershipGeneration system (heraldA :| [heraldC]))

system :: SystemId
system = checked "label evidence system" (mkSystemId (bytes 9))

decisionA, decisionB :: LabelDecisionId
decisionA = decision 50
decisionB = decision 51

decision :: Word -> LabelDecisionId
decision byte = checked "label decision" (mkLabelDecisionId (bytes byte))

objectA, objectB :: GlobalObjectId
objectA = checked "object" (mkGlobalObjectId (bytes 60))
objectB = checked "other object" (mkGlobalObjectId (bytes 61))

catalogueDigest, otherCatalogueDigest :: CatalogueDigest
catalogueDigest = checked "catalogue digest" (mkCatalogueDigest (bytes 70))
otherCatalogueDigest = checked "other catalogue digest" (mkCatalogueDigest (bytes 69))

initialDigest, otherInitialDigest :: InitialProjectionDigest
initialDigest = checked "initial projection digest" (mkInitialProjectionDigest (bytes 71))
otherInitialDigest = checked "other initial projection digest" (mkInitialProjectionDigest (bytes 72))

revisionOne, revisionTwo :: LabelRevision
revisionOne = checked "label revision" (mkLabelRevision (controlIndex 1))
revisionTwo = checked "second label revision" (mkLabelRevision (controlIndex 2))

occurrence :: StructuralOccurrenceId
occurrence = structuralOccurrenceId heraldB (checked "structural sequence" (mkStructuralSequence 3))

topologyCut :: TopologyCutId
topologyCut = checked "topology cut" (mkTopologyCutId (bytes 80))

structuralAuthority :: AuthorityEpoch
structuralAuthority = structuralAuthorityEpoch occurrence topologyCut

bytes :: Word -> ByteString.ByteString
bytes byte = ByteString.replicate 32 (fromIntegral byte)

hexBytes :: ByteString.ByteString -> String
hexBytes = concatMap hexByte . ByteString.unpack
  where
    hexByte byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (\problem -> error (label <> " fixture invariant: " <> show problem)) id

corruptDomain :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString
corruptDomain domain bytesValue =
  case ByteString.breakSubstring domain bytesValue of
    (prefix, suffix)
      | not (ByteString.null suffix) ->
          prefix <> ByteString.singleton 0 <> ByteString.drop 1 suffix
    _ -> error "canonical test fixture does not contain its domain separator"

replaceFinalWord64WithZero :: ByteString.ByteString -> ByteString.ByteString
replaceFinalWord64WithZero bytesValue
  | ByteString.length bytesValue < 8 =
      error "canonical test fixture has no final Word64"
  | otherwise =
      ByteString.take (ByteString.length bytesValue - 8) bytesValue
        <> ByteString.replicate 8 0

replaceFirstSubsequence ::
  ByteString.ByteString ->
  ByteString.ByteString ->
  ByteString.ByteString ->
  ByteString.ByteString
replaceFirstSubsequence expected replacement bytesValue =
  case ByteString.breakSubstring expected bytesValue of
    (prefix, suffix)
      | not (ByteString.null suffix) ->
          prefix <> replacement <> ByteString.drop (ByteString.length expected) suffix
    _ -> error "canonical test fixture does not contain the expected subsequence"

word64Three, word64Four :: ByteString.ByteString
word64Three = ByteString.pack [0, 0, 0, 0, 0, 0, 0, 3]
word64Four = ByteString.pack [0, 0, 0, 0, 0, 0, 0, 4]
