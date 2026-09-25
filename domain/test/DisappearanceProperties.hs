{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module DisappearanceProperties
  ( tests,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance
  ( CanonicalDescriptorDigest,
    ControlledDisappearanceSubjectProblem (..),
    ControlledDisappearanceSubjectRevisionProblem (..),
    DisappearanceDigestProblem (..),
    DisappearanceEvidenceClaim,
    DisappearanceEvidenceClaimProblem (..),
    DisappearanceEvidenceDigest,
    DisappearanceOpenResultView (..),
    DisappearanceProbeId,
    DisappearanceProbeIdentityProblem (..),
    DisappearanceResolutionOutcome,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceResolutionProblem (..),
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    RegularSortOccurrenceClaim,
    RegularSortOccurrenceClaimProblem (..),
    admitDisappearanceEvidenceClaim,
    admitDisappearanceProbeId,
    admitRegularSortOccurrenceClaim,
    aliasedDisappearanceProbe,
    canonicalDescriptorDigestBytes,
    controlledPredefinedDisappearanceSubject,
    deriveCanonicalDescriptorDigest,
    deriveDisappearanceEvidenceDigest,
    deriveDisappearanceOutcomeDigest,
    deriveDisappearanceProbeId,
    deriveDisappearanceSubjectDigest,
    deriveRegularSortOccurrenceClaim,
    disappearanceCoordinateMemberSetDigest,
    disappearanceCoordinateMembershipGenerationId,
    disappearanceCoordinateSubjectDigest,
    disappearanceEvidenceClaimCanonicalBytes,
    disappearanceEvidenceClaimCoordinate,
    disappearanceEvidenceClaimDigest,
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceEvidenceDigestBytes,
    disappearanceOpenResultView,
    disappearanceOutcomeDigestBytes,
    disappearanceProbeIdBytes,
    disappearanceProbeOpenControlIndex,
    disappearanceResolutionControlIndex,
    disappearanceResolutionOutcomeCanonicalBytes,
    disappearanceResolutionOutcomeView,
    disappearanceResolutionSubject,
    disappearanceSubjectCanonicalBytes,
    disappearanceSubjectDigestBytes,
    disappearanceSubjectView,
    mkCanonicalDescriptorDigest,
    mkDisappearanceEvidenceDigest,
    mkDisappearanceOutcomeDigest,
    mkDisappearanceSubjectDigest,
    openedDisappearanceProbe,
    regularSortDefinitionDisappearanceSubject,
    regularSortOccurrenceClaimDescriptorDigest,
    regularSortOccurrenceClaimOccurrenceId,
    regularSortOccurrenceClaimSortId,
    resolveDisappearanceSubject,
    reviseControlledDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( GlobalObjectId,
    HeraldEpoch,
    SortDefinitionOccurrenceId,
    StructuralOccurrenceId,
    SystemId,
    controlIndex,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkStructuralSequence,
    mkSystemId,
    sortIdBytes,
    structuralOccurrenceId,
  )
import Eclips.Domain.Label
  ( LabelRevision,
    mkLabelRevision,
  )
import Eclips.Domain.MemberSet (memberSetDigestBytes)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalDescriptorBytes,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedCatalogueDescriptor,
    profileEntryFor,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "private disappearance vocabulary"
    [ testCase "all nominal digests require exactly 32 bytes" caseDigestWidths,
      testCase "descriptor digests are canonical and content-sensitive" caseDescriptorDigest,
      testCase "genesis and resolved occurrence claims recompute their identity" caseOccurrenceClaims,
      testCase "controlled subjects admit only eligible predefined roles" caseControlledSubjectAdmission,
      testCase "controlled subject revision changes only the revision" caseControlledSubjectRevision,
      testCase "regular subjects reject controlled revision" caseRegularSubjectRevisionRejection,
      testCase "subjects expose exactly the controlled and regular shapes" caseSubjectShapes,
      testCase "controlled subjects sort before regular and bytewise within each arm" caseSubjectOrder,
      testCase "subject transcripts and digests are deterministic and content-sensitive" caseSubjectIdentity,
      testCase "probe IDs require a positive Open index and validate claimed bytes" caseProbeIdentity,
      testCase "evidence claims bind subject, membership, reporter, probe, and digest" caseEvidenceBinding,
      testCase "Open results distinguish creation from aliasing without changing the probe" caseOpenResult,
      testCase "resolution outcomes require a positive index and derive the next occurrence" caseResolutionOutcome,
      testCase "canonical identity surfaces are manual private transcripts, not wire codecs" caseManualTranscripts
    ]

caseDigestWidths :: Assertion
caseDigestWidths = do
  mapM_
    ( \byteCount -> do
        let bytes = ByteString.replicate byteCount 0
            malformed = byteCount /= 32
        assertDigestAdmission malformed (mkCanonicalDescriptorDigest bytes)
        assertDigestAdmission malformed (mkDisappearanceSubjectDigest bytes)
        assertDigestAdmission malformed (mkDisappearanceEvidenceDigest bytes)
        assertDigestAdmission malformed (mkDisappearanceOutcomeDigest bytes)
    )
    [0, 1, 31, 32, 33, 64]
  assertEqual "descriptor digest width" 32 (ByteString.length (canonicalDescriptorDigestBytes descriptorDigestA))
  assertEqual "subject digest width" 32 (ByteString.length (disappearanceSubjectDigestBytes (deriveDisappearanceSubjectDigest controlledSubjectA)))
  assertEqual "evidence digest width" 32 (ByteString.length (disappearanceEvidenceDigestBytes evidenceDigestA))
  assertEqual "outcome digest width" 32 (ByteString.length (disappearanceOutcomeDigestBytes (deriveDisappearanceOutcomeDigest controlledOutcome)))

caseDescriptorDigest :: Assertion
caseDescriptorDigest = do
  assertEqual
    "the descriptor digest hashes the exact canonical bytes"
    (SHA256.hash (canonicalDescriptorBytes descriptorA))
    (canonicalDescriptorDigestBytes descriptorDigestA)
  assertEqual
    "descriptor identity and descriptor digest retain equal content-address bits"
    (sortIdBytes (descriptorSortId descriptorA))
    (canonicalDescriptorDigestBytes descriptorDigestA)
  assertEqual "equal descriptors derive equal digests" descriptorDigestA (deriveCanonicalDescriptorDigest descriptorA)
  assertBool "different canonical descriptor bytes change the digest" (descriptorDigestA /= descriptorDigestB)

caseOccurrenceClaims :: Assertion
caseOccurrenceClaims = do
  let genesisOccurrence = occurrenceId systemA descriptorA Genesis
      resolvedBase = retirementBase 17
      resolvedOccurrence = occurrenceId systemA descriptorA resolvedBase
      expectedGenesisClaim = deriveRegularSortOccurrenceClaim systemA descriptorA Genesis
      expectedResolvedClaim = deriveRegularSortOccurrenceClaim systemA descriptorA resolvedBase
  assertEqual
    "genesis claim admits"
    (Right expectedGenesisClaim)
    (admitRegularSortOccurrenceClaim systemA descriptorA Genesis genesisOccurrence)
  assertEqual
    "resolved claim admits"
    (Right expectedResolvedClaim)
    (admitRegularSortOccurrenceClaim systemA descriptorA resolvedBase resolvedOccurrence)
  assertEqual "claim retains sort identity" (descriptorSortId descriptorA) (regularSortOccurrenceClaimSortId expectedGenesisClaim)
  assertEqual "claim retains descriptor digest" descriptorDigestA (regularSortOccurrenceClaimDescriptorDigest expectedGenesisClaim)
  assertEqual "claim retains occurrence identity" resolvedOccurrence (regularSortOccurrenceClaimOccurrenceId expectedResolvedClaim)
  assertEqual
    "genesis occurrence cannot pose as the resolved occurrence"
    (Left (RegularSortOccurrenceClaimMismatch genesisOccurrence resolvedOccurrence))
    (admitRegularSortOccurrenceClaim systemA descriptorA resolvedBase genesisOccurrence)
  assertEqual
    "resolved occurrence cannot pose as genesis"
    (Left (RegularSortOccurrenceClaimMismatch resolvedOccurrence genesisOccurrence))
    (admitRegularSortOccurrenceClaim systemA descriptorA Genesis resolvedOccurrence)
  let otherExpected = occurrenceId systemA descriptorB Genesis
  assertEqual
    "descriptor content participates in occurrence admission"
    (Left (RegularSortOccurrenceClaimMismatch genesisOccurrence otherExpected))
    (admitRegularSortOccurrenceClaim systemA descriptorB Genesis genesisOccurrence)

caseControlledSubjectAdmission :: Assertion
caseControlledSubjectAdmission = do
  mapM_
    ( \role ->
        assertEqual
          (show role)
          (Right controlledSubjectA)
          ( controlledPredefinedDisappearanceSubject
              role
              objectA
              structuralOccurrenceA
              (Just labelRevision)
          )
    )
    [NeutralVertexRole, EdgeRole, NablaRole, DeltaRole]
  mapM_
    ( \role ->
        assertEqual
          (show role)
          (Left (ControlledDisappearanceSubjectIneligibleRole role))
          ( controlledPredefinedDisappearanceSubject
              role
              objectA
              structuralOccurrenceA
              (Just labelRevision)
          )
    )
    [SortDefinitionRole, ProcessEpochRole]

caseControlledSubjectRevision :: Assertion
caseControlledSubjectRevision = do
  let revised =
        checked
          "revised controlled subject"
          (reviseControlledDisappearanceSubject revisedLabelRevision controlledSubjectA)
  assertEqual
    "revision preserves the controlled object and structural occurrence"
    ( ControlledPredefinedSubjectView
        objectA
        structuralOccurrenceA
        (Just revisedLabelRevision)
    )
    (disappearanceSubjectView revised)
  assertBool
    "revision replaces the prior label revision"
    (revised /= controlledSubjectA)

caseRegularSubjectRevisionRejection :: Assertion
caseRegularSubjectRevisionRejection =
  assertEqual
    "a regular occurrence cannot be rekeyed as a controlled subject"
    (Left ControlledDisappearanceSubjectRevisionRequiresControlled)
    (reviseControlledDisappearanceSubject revisedLabelRevision regularSubjectA)

caseSubjectShapes :: Assertion
caseSubjectShapes = do
  assertEqual
    "controlled subject has only object, occurrence, and optional label revision"
    (ControlledPredefinedSubjectView objectA structuralOccurrenceA (Just labelRevision))
    (disappearanceSubjectView controlledSubjectA)
  assertEqual
    "regular subject has only sort, descriptor digest, and occurrence"
    ( RegularSortDefinitionSubjectView
        (regularSortOccurrenceClaimSortId regularClaimA)
        (regularSortOccurrenceClaimDescriptorDigest regularClaimA)
        (regularSortOccurrenceClaimOccurrenceId regularClaimA)
    )
    (disappearanceSubjectView regularSubjectA)

caseSubjectOrder :: Assertion
caseSubjectOrder = do
  assertBool "controlled arm precedes regular even when its bytes are larger" (controlledSubjectZ < regularSubjectA)
  assertBool "regular arm follows controlled" (regularSubjectA > controlledSubjectZ)
  assertEqual
    "controlled subjects use bytewise transcript order within the arm"
    (compare (disappearanceSubjectCanonicalBytes controlledSubjectA) (disappearanceSubjectCanonicalBytes controlledSubjectZ))
    (compare controlledSubjectA controlledSubjectZ)
  assertEqual
    "regular subjects use bytewise transcript order within the arm"
    (compare (disappearanceSubjectCanonicalBytes regularSubjectA) (disappearanceSubjectCanonicalBytes regularSubjectB))
    (compare regularSubjectA regularSubjectB)
  mapM_
    ( \leftSubject ->
        mapM_
          ( \rightSubject ->
              assertEqual
                "subject comparison is equal exactly when subject equality holds"
                (leftSubject == rightSubject)
                (compare leftSubject rightSubject == EQ)
          )
          subjects
    )
    subjects
  where
    subjects =
      [ controlledSubjectA,
        controlledSubjectZ,
        controlledWithoutRevision,
        controlledOtherOccurrence,
        regularSubjectA,
        regularSubjectB,
        regularSubjectResolved
      ]

caseSubjectIdentity :: Assertion
caseSubjectIdentity = do
  let digest = deriveDisappearanceSubjectDigest controlledSubjectA
  assertEqual
    "controlled subject digest has a fixed golden"
    "99b2601f:54115eb0:6d7e92d5:6ed10848:b9625dd0:f38999f0:5bd333d3:986800a7"
    (show digest)
  assertEqual "equal subjects have equal transcripts" (disappearanceSubjectCanonicalBytes controlledSubjectA) (disappearanceSubjectCanonicalBytes controlledSubjectA)
  assertEqual "equal subjects have equal digests" digest (deriveDisappearanceSubjectDigest controlledSubjectA)
  mapM_
    (\subject -> assertBool (show (disappearanceSubjectView subject)) (digest /= deriveDisappearanceSubjectDigest subject))
    [controlledSubjectZ, controlledWithoutRevision, controlledOtherOccurrence, regularSubjectA]
  assertBool "descriptor content changes a regular subject digest" (deriveDisappearanceSubjectDigest regularSubjectA /= deriveDisappearanceSubjectDigest regularSubjectB)
  assertBool "occurrence base changes a regular subject digest" (deriveDisappearanceSubjectDigest regularSubjectA /= deriveDisappearanceSubjectDigest regularSubjectResolved)

caseProbeIdentity :: Assertion
caseProbeIdentity = do
  assertEqual
    "index zero cannot identify a committed Open"
    (Left DisappearanceProbeOpenControlIndexMustBePositive)
    (deriveDisappearanceProbeId (controlIndex 0))
  assertEqual "equal Open indices derive equal IDs" probeA (checked "equal probe" (deriveDisappearanceProbeId (controlIndex 23)))
  assertBool "different Open indices derive different IDs" (probeA /= probeB)
  assertEqual "the Open coordinate is retained" (controlIndex 23) (disappearanceProbeOpenControlIndex probeA)
  assertEqual "probe digest width" 32 (ByteString.length (disappearanceProbeIdBytes probeA))
  assertEqual
    "an equal claimed digest admits"
    (Right probeA)
    (admitDisappearanceProbeId (controlIndex 23) (disappearanceProbeIdBytes probeA))
  assertEqual
    "a malformed claimed digest rejects"
    ( Left
        ( DisappearanceProbeClaimedDigestInvalid
            DisappearanceDigestWrongByteCount
              { expectedDisappearanceDigestByteCount = 32,
                actualDisappearanceDigestByteCount = 31
              }
        )
    )
    (admitDisappearanceProbeId (controlIndex 23) (ByteString.replicate 31 0))
  let changed = changeFirstByte (disappearanceProbeIdBytes probeA)
  assertEqual
    "a different well-shaped claim rejects"
    (Left (DisappearanceProbeClaimedDigestMismatch changed (disappearanceProbeIdBytes probeA)))
    (admitDisappearanceProbeId (controlIndex 23) changed)
  assertEqual
    "a zero index rejects before accepting even a well-shaped claim"
    (Left DisappearanceProbeOpenControlIndexMustBePositive)
    (admitDisappearanceProbeId (controlIndex 0) (disappearanceProbeIdBytes probeA))

caseEvidenceBinding :: Assertion
caseEvidenceBinding = do
  let claim = evidenceClaim controlledSubjectA generationA probeA heraldA evidenceDigestA
      coordinate = disappearanceEvidenceClaimCoordinate claim
      otherSubjectClaim = evidenceClaim controlledSubjectZ generationA probeA heraldA evidenceDigestA
      otherReporterClaim = evidenceClaim controlledSubjectA generationA probeA heraldB evidenceDigestA
      otherMembershipClaim = evidenceClaim controlledSubjectA generationB probeA heraldA evidenceDigestA
      otherEvidenceClaim = evidenceClaim controlledSubjectA generationA probeA heraldA evidenceDigestB
  assertEqual "subject digest is bound" (deriveDisappearanceSubjectDigest controlledSubjectA) (disappearanceCoordinateSubjectDigest coordinate)
  assertEqual "membership generation is bound" (heraldMembershipGenerationId generationA) (disappearanceCoordinateMembershipGenerationId coordinate)
  assertEqual "member-set digest is bound" (heraldMembershipGenerationActiveMemberSetDigest generationA) (disappearanceCoordinateMemberSetDigest coordinate)
  assertEqual "probe is bound" probeA (disappearanceEvidenceClaimProbeId claim)
  assertEqual "reporter is bound" heraldA (disappearanceEvidenceClaimReporter claim)
  assertEqual "evidence digest is bound" evidenceDigestA (disappearanceEvidenceClaimDigest claim)
  assertEqual
    "a reporter outside the captured generation rejects"
    (Left (DisappearanceEvidenceReporterNotCaptured heraldZ))
    (admitDisappearanceEvidenceClaim controlledSubjectA generationA probeA heraldZ evidenceDigestA)
  assertEqual "equal evidence payloads derive equal digests" evidenceDigestA (deriveDisappearanceEvidenceDigest "evidence-a")
  assertBool "different evidence payloads change their digest" (evidenceDigestA /= evidenceDigestB)
  mapM_
    (\other -> assertBool "every bound coordinate participates in the claim transcript" (disappearanceEvidenceClaimCanonicalBytes claim /= disappearanceEvidenceClaimCanonicalBytes other))
    [otherSubjectClaim, otherReporterClaim, otherMembershipClaim, otherEvidenceClaim]
  assertBool
    "member-set bytes are the exact captured digest"
    ( not
        ( ByteString.null
            (memberSetDigestBytes (disappearanceCoordinateMemberSetDigest coordinate))
        )
    )

caseOpenResult :: Assertion
caseOpenResult = do
  assertEqual
    "first accepted Open is classified as opened"
    (OpenedDisappearanceProbe probeA)
    (disappearanceOpenResultView (openedDisappearanceProbe probeA))
  assertEqual
    "a racing Open aliases the existing probe"
    (AliasedDisappearanceProbe probeA)
    (disappearanceOpenResultView (aliasedDisappearanceProbe probeA))

caseResolutionOutcome :: Assertion
caseResolutionOutcome = do
  assertEqual
    "controlled Resolve requires a positive index"
    (Left DisappearanceResolutionControlIndexMustBePositive)
    (resolveDisappearanceSubject systemA (controlIndex 0) controlledSubjectA)
  assertEqual
    "regular Resolve requires a positive index"
    (Left DisappearanceResolutionControlIndexMustBePositive)
    (resolveDisappearanceSubject systemA (controlIndex 0) regularSubjectA)
  assertEqual "outcome retains its exact subject" controlledSubjectA (disappearanceResolutionSubject controlledOutcome)
  assertEqual "outcome retains its resolve index" (controlIndex 31) (disappearanceResolutionControlIndex controlledOutcome)
  assertEqual
    "controlled arm retains its occurrence and label claim"
    ( ControlledDisappearanceResolved
        objectA
        structuralOccurrenceA
        (Just labelRevision)
        (controlIndex 31)
    )
    (disappearanceResolutionOutcomeView controlledOutcome)
  let expectedNext = occurrenceId systemA descriptorA (retirementBase 31)
  assertEqual
    "regular arm retires the old occurrence and derives the next at Resolve"
    ( RegularSortDefinitionRetired
        (regularSortOccurrenceClaimSortId regularClaimA)
        descriptorDigestA
        (regularSortOccurrenceClaimOccurrenceId regularClaimA)
        (controlIndex 31)
        expectedNext
    )
    (disappearanceResolutionOutcomeView regularOutcome)
  assertBool "resolution arms have distinct canonical bytes" (disappearanceResolutionOutcomeCanonicalBytes controlledOutcome /= disappearanceResolutionOutcomeCanonicalBytes regularOutcome)
  assertBool "resolution arms have distinct outcome digests" (deriveDisappearanceOutcomeDigest controlledOutcome /= deriveDisappearanceOutcomeDigest regularOutcome)
  let later = checked "later controlled outcome" (resolveDisappearanceSubject systemA (controlIndex 32) controlledSubjectA)
  assertBool "resolve index participates in the outcome digest" (deriveDisappearanceOutcomeDigest controlledOutcome /= deriveDisappearanceOutcomeDigest later)

caseManualTranscripts :: Assertion
caseManualTranscripts = do
  assertManualDomain "ECLIPS-DISAPPEARANCE-SUBJECT" (disappearanceSubjectCanonicalBytes controlledSubjectA)
  assertManualDomain "ECLIPS-DISAPPEARANCE-EVIDENCE-CLAIM" (disappearanceEvidenceClaimCanonicalBytes claim)
  assertManualDomain "ECLIPS-DISAPPEARANCE-OUTCOME" (disappearanceResolutionOutcomeCanonicalBytes controlledOutcome)
  where
    claim = evidenceClaim controlledSubjectA generationA probeA heraldA evidenceDigestA

assertDigestAdmission ::
  Bool ->
  Either DisappearanceDigestProblem digest ->
  Assertion
assertDigestAdmission malformed actual = case actual of
  Left DisappearanceDigestWrongByteCount {} ->
    assertBool "only malformed widths reject" malformed
  Right _ -> assertBool "exactly 32 bytes admit" (not malformed)

assertManualDomain :: ByteString -> ByteString -> Assertion
assertManualDomain domain bytes = do
  assertBool "canonical transcript is not empty" (not (ByteString.null bytes))
  assertEqual
    "manual transcript starts with its explicitly framed domain"
    domain
    (ByteString.take (ByteString.length domain) (ByteString.drop 8 bytes))

evidenceClaim ::
  DisappearanceSubject ->
  HeraldMembershipGeneration ->
  DisappearanceProbeId ->
  HeraldEpoch ->
  DisappearanceEvidenceDigest ->
  DisappearanceEvidenceClaim
evidenceClaim subject generation probe reporter digest =
  checked
    "evidence claim"
    (admitDisappearanceEvidenceClaim subject generation probe reporter digest)

controlledSubjectA,
  controlledSubjectZ,
  controlledWithoutRevision,
  controlledOtherOccurrence ::
    DisappearanceSubject
controlledSubjectA =
  checked
    "controlled subject A"
    ( controlledPredefinedDisappearanceSubject
        NeutralVertexRole
        objectA
        structuralOccurrenceA
        (Just labelRevision)
    )
controlledSubjectZ =
  checked
    "controlled subject Z"
    ( controlledPredefinedDisappearanceSubject
        NeutralVertexRole
        objectZ
        structuralOccurrenceA
        (Just labelRevision)
    )
controlledWithoutRevision =
  checked
    "controlled subject without revision"
    ( controlledPredefinedDisappearanceSubject
        EdgeRole
        objectA
        structuralOccurrenceA
        Nothing
    )
controlledOtherOccurrence =
  checked
    "controlled subject with other occurrence"
    ( controlledPredefinedDisappearanceSubject
        NablaRole
        objectA
        structuralOccurrenceB
        (Just labelRevision)
    )

regularSubjectA, regularSubjectB, regularSubjectResolved :: DisappearanceSubject
regularSubjectA = regularSortDefinitionDisappearanceSubject regularClaimA
regularSubjectB = regularSortDefinitionDisappearanceSubject regularClaimB
regularSubjectResolved = regularSortDefinitionDisappearanceSubject regularClaimResolved

regularClaimA, regularClaimB, regularClaimResolved :: RegularSortOccurrenceClaim
regularClaimA = deriveRegularSortOccurrenceClaim systemA descriptorA Genesis
regularClaimB = deriveRegularSortOccurrenceClaim systemA descriptorB Genesis
regularClaimResolved = deriveRegularSortOccurrenceClaim systemA descriptorA (retirementBase 7)

descriptorA, descriptorB :: CanonicalDescriptor
descriptorA = predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole)
descriptorB = predefinedCatalogueDescriptor (profileEntryFor EdgeRole)

descriptorDigestA, descriptorDigestB :: CanonicalDescriptorDigest
descriptorDigestA = deriveCanonicalDescriptorDigest descriptorA
descriptorDigestB = deriveCanonicalDescriptorDigest descriptorB

systemA, systemB :: SystemId
systemA = fixedIdentity "system A" mkSystemId 0x11
systemB = fixedIdentity "system B" mkSystemId 0x12

objectA, objectZ :: GlobalObjectId
objectA = fixedIdentity "object A" mkGlobalObjectId 0x21
objectZ = fixedIdentity "object Z" mkGlobalObjectId 0xf1

heraldA, heraldB, heraldZ :: HeraldEpoch
heraldA = fixedIdentity "herald A" mkHeraldEpoch 0x31
heraldB = fixedIdentity "herald B" mkHeraldEpoch 0x32
heraldZ = fixedIdentity "herald Z" mkHeraldEpoch 0xf2

structuralOccurrenceA, structuralOccurrenceB :: StructuralOccurrenceId
structuralOccurrenceA = structuralOccurrenceId heraldA (checked "structural sequence 1" (mkStructuralSequence 1))
structuralOccurrenceB = structuralOccurrenceId heraldA (checked "structural sequence 2" (mkStructuralSequence 2))

labelRevision :: LabelRevision
labelRevision = checked "label revision" (mkLabelRevision (controlIndex 19))

revisedLabelRevision :: LabelRevision
revisedLabelRevision = checked "revised label revision" (mkLabelRevision (controlIndex 29))

generationA, generationB :: HeraldMembershipGeneration
generationA = checked "generation A" (genesisHeraldMembershipGeneration systemA fixedMembers)
generationB = checked "generation B" (genesisHeraldMembershipGeneration systemB fixedMembers)

fixedMembers :: NonEmpty HeraldEpoch
fixedMembers = heraldA :| [heraldB]

probeA, probeB :: DisappearanceProbeId
probeA = checked "probe A" (deriveDisappearanceProbeId (controlIndex 23))
probeB = checked "probe B" (deriveDisappearanceProbeId (controlIndex 24))

evidenceDigestA, evidenceDigestB :: DisappearanceEvidenceDigest
evidenceDigestA = deriveDisappearanceEvidenceDigest "evidence-a"
evidenceDigestB = deriveDisappearanceEvidenceDigest "evidence-b"

controlledOutcome, regularOutcome :: DisappearanceResolutionOutcome
controlledOutcome = checked "controlled outcome" (resolveDisappearanceSubject systemA (controlIndex 31) controlledSubjectA)
regularOutcome = checked "regular outcome" (resolveDisappearanceSubject systemA (controlIndex 31) regularSubjectA)

occurrenceId ::
  SystemId ->
  CanonicalDescriptor ->
  SortOccurrenceBase ->
  SortDefinitionOccurrenceId
occurrenceId system descriptor =
  deriveSortDefinitionOccurrenceId system (descriptorSortId descriptor)

retirementBase :: Word64 -> SortOccurrenceBase
retirementBase =
  checked "resolved retirement base"
    . resolvedRetirementOccurrenceBase
    . controlIndex

fixedIdentity ::
  (Show problem) =>
  String ->
  (ByteString -> Either problem identity) ->
  Word8 ->
  identity
fixedIdentity context constructor byte =
  checked context (constructor (ByteString.replicate 32 byte))

changeFirstByte :: ByteString -> ByteString
changeFirstByte bytes = case ByteString.uncons bytes of
  Nothing -> bytes
  Just (first, rest) -> ByteString.cons (if first == 0 then 1 else 0) rest

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
