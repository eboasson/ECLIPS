{-# LANGUAGE OverloadedStrings #-}

module MembershipProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    SystemId,
    controlIndex,
    controlIndexWord64,
    mkHeraldEpoch,
    mkSystemId,
  )
import Eclips.Domain.MemberSet (deriveMemberSetDigest)
import Eclips.Domain.Membership
  ( FailureProbeIdCanonicalProblem (..),
    FailureProbeIdentityProblem (..),
    FailureProbeResolution (..),
    FailureProbeResolutionCanonicalProblem (..),
    HeraldFailureProbeId,
    HeraldMembershipGeneration,
    MembershipGenerationCanonicalProblem (..),
    MembershipGenerationProblem (..),
    MembershipIdentityError (..),
    decodeFailureProbeResolutionIdCanonicalBytes,
    decodeHeraldFailureProbeIdCanonicalBytes,
    decodeHeraldMembershipGenerationCanonicalBytes,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    failureProbeResolutionDisposition,
    failureProbeResolutionIdBytes,
    failureProbeResolutionIdCanonicalBytes,
    failureProbeResolutionProbeId,
    genesisHeraldMembershipGeneration,
    heraldFailureProbeControlIndex,
    heraldFailureProbeIdBytes,
    heraldFailureProbeIdCanonicalBytes,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationCanonicalBytes,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipGenerationRetirementControlIndex,
    mkHeraldMembershipGenerationId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Membership qualified as Membership
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    shuffle,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "membership generation and failure identities"
    [ testProperty "membership generation IDs require exactly 32 bytes" propGenerationIdentityWidth,
      testCase "genesis normalizes one exact non-empty member set" caseGenesisShape,
      testCase "each retirement removes exactly its target" caseRetirementShape,
      testCase "invalid retirement coordinates and targets are rejected" caseRetirementRejections,
      testCase "admission identity is checked, canonical and domain-separated" caseAdmissionIdentity,
      testCase "admission inserts exactly one epoch and orders all membership changes" caseAdmissionShape,
      testCase "history retains admission identities and terminal epoch tombstones" caseAdmissionHistory,
      testProperty "mixed joins and retirements retain a canonical checked history" propMixedMembershipHistory,
      testProperty "generated retirement histories preserve exact origins and descendant intervals" propRetirementHistory,
      testCase "history rejects skipped, reversed, branched and reused retirement identities" caseInvalidHistory,
      testCase "history canonical decoder rejects malformed and non-chain claims" caseHistoryCanonical,
      testCase "membership generation canonical bytes round-trip" caseMembershipRoundTrip,
      testCase "membership decoder rejects wrong family, trailing bytes, and semantic mutation" caseMembershipNegativeControls,
      testCase "failure probe identity is derived from its exact control index" caseProbeIdentity,
      testCase "failure probe decoder rejects wrong family, trailing bytes, and digest mutation" caseProbeNegativeControls,
      testCase "terminal resolution identities are disposition-separated and round-trip" caseResolutionIdentity,
      testCase "resolution decoder rejects wrong family, trailing bytes, digest mutation, and unknown tag" caseResolutionNegativeControls,
      testGroup
        "canonical goldens"
        [ golden "genesis membership" genesisGolden (heraldMembershipGenerationCanonicalBytes (genesis fixedMembership)),
          golden "retired membership" retiredGolden (heraldMembershipGenerationCanonicalBytes (retired retirementIndex heraldB (genesis fixedMembership))),
          golden "probe" probeGolden (heraldFailureProbeIdCanonicalBytes probe),
          golden "dismiss resolution" dismissGolden (failureProbeResolutionIdCanonicalBytes (deriveFailureProbeResolutionId probe DismissFailureProbe)),
          golden "retire resolution" retireGolden (failureProbeResolutionIdCanonicalBytes (deriveFailureProbeResolutionId probe RetireFailureProbeTarget))
        ]
    ]

propGenerationIdentityWidth :: Property
propGenerationIdentityWidth =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let actual = mkHeraldMembershipGenerationId (ByteString.replicate byteCount 0)
        malformed = byteCount /= 32
     in counterexample ("byte count: " <> show byteCount)
          $ case actual of
            Left MembershipIdentityWrongByteCount {} -> malformed === True
            Right _ -> malformed === False

caseAdmissionIdentity :: Assertion
caseAdmissionIdentity = do
  let admission = checked "admission" (Membership.deriveHeraldAdmissionId (controlIndex 7))
      bytes = Membership.heraldAdmissionIdCanonicalBytes admission
      digestStart = 8 + ByteString.length "ECLIPS-HERALD-ADMISSION-VALUE" + 8
  assertEqual "genesis does not open an admission" (Left Membership.HeraldAdmissionControlIndexZero) (Membership.deriveHeraldAdmissionId (controlIndex 0))
  assertEqual "admission round-trip" (Right admission) (Membership.decodeHeraldAdmissionIdCanonicalBytes bytes)
  assertEqual "admission retains its opening coordinate" (controlIndex 7) (Membership.heraldAdmissionControlIndex admission)
  assertBool "a probe at the same coordinate has another identity" (Membership.heraldAdmissionIdBytes admission /= heraldFailureProbeIdBytes probe)
  assertDecodeFailure "admission wrong family rejects" (Membership.decodeHeraldAdmissionIdCanonicalBytes (replaceByte 8 0x58 bytes))
  assertDecodeFailure "admission trailing bytes reject" (Membership.decodeHeraldAdmissionIdCanonicalBytes (bytes <> "\NUL"))
  assertEqual "admission digest mutation rejects" (Left Membership.HeraldAdmissionIdCanonicalIdentifierMismatch) (Membership.decodeHeraldAdmissionIdCanonicalBytes (replaceByte digestStart 0xff bytes))

caseAdmissionShape :: Assertion
caseAdmissionShape = do
  let first = genesis (heraldA :| [heraldC])
      admission = checked "admission" (Membership.deriveHeraldAdmissionId (controlIndex 2))
      second = checked "admit" (Membership.admitHeraldMembershipGeneration (controlIndex 5) admission heraldB first)
  assertEqual "exact sorted insertion" fixedMembership (heraldMembershipGenerationActiveHeraldEpochs second)
  assertEqual "admission identity retained" (Just admission) (Membership.heraldMembershipGenerationAdmissionId second)
  assertEqual "admitted epoch retained" (Just heraldB) (Membership.heraldMembershipGenerationAdmittedHeraldEpoch second)
  assertEqual "activation coordinate retained" (Just (controlIndex 5)) (Membership.heraldMembershipGenerationChangeControlIndex second)
  assertEqual "activation is not a retirement" Nothing (heraldMembershipGenerationRetirementControlIndex second)
  assertEqual "admission generation canonical round-trip" (Right second) (decodeHeraldMembershipGenerationCanonicalBytes (heraldMembershipGenerationCanonicalBytes second))
  assertEqual "active epoch cannot join again" (Left (MembershipGenerationTargetAlreadyActive heraldA)) (Membership.admitHeraldMembershipGeneration (controlIndex 5) admission heraldA first)
  assertEqual "admission begins before activation" (Left (Membership.MembershipGenerationAdmissionNotBeforeActivation (controlIndex 2) (controlIndex 2))) (Membership.admitHeraldMembershipGeneration (controlIndex 2) admission heraldB first)
  assertEqual "retirement checks the latest admission coordinate" (Left (MembershipGenerationRetirementControlIndexNotIncreasing (controlIndex 5) (controlIndex 4))) (retireHeraldMembershipGeneration (controlIndex 4) (retirementResolution (controlIndex 4)) heraldB second)

caseAdmissionHistory :: Assertion
caseAdmissionHistory = do
  let first = genesis (heraldA :| [])
      admission = checked "admission" (Membership.deriveHeraldAdmissionId (controlIndex 2))
      second = checked "admit" (Membership.admitHeraldMembershipGeneration (controlIndex 4) admission heraldB first)
      third = retired (controlIndex 6) heraldB second
      history = checked "joined history" (Membership.heraldMembershipHistory (first :| [second, third]))
      reusedIdentity = checked "shape-valid reused admission" (Membership.admitHeraldMembershipGeneration (controlIndex 8) admission heraldC third)
      reusedEpoch = admitted (controlIndex 8) heraldB third
      lowerAdmission = checked "low-index admission" (Membership.deriveHeraldAdmissionId (controlIndex 3))
  assertEqual "terminal epochs are never readmitted" (Left (Membership.MembershipHistoryHeraldEpochReused heraldB)) (Membership.appendHeraldMembershipGeneration reusedEpoch history)
  assertEqual "an admission identity describes only one activation" (Left (Membership.MembershipHistoryAdmissionIdentityReused admission)) (Membership.appendHeraldMembershipGeneration reusedIdentity history)
  assertEqual "join checks the latest retirement coordinate" (Left (Membership.MembershipGenerationAdmissionControlIndexNotIncreasing (controlIndex 6) (controlIndex 5))) (Membership.admitHeraldMembershipGeneration (controlIndex 5) lowerAdmission heraldC third)

propMixedMembershipHistory :: Property
propMixedMembershipHistory =
  forAll (chooseInt (1, 8)) $ \count ->
    forAll (shuffle (fmap (herald . fromIntegral) [2 .. count + 1])) $ \newcomers ->
      let first = genesis (herald 1 :| [])
          changes = zip [4, 8 ..] newcomers
          extend priorGenerations (index, newcomer) =
            let joined = admitted (controlIndex index) newcomer (last priorGenerations)
                removed = retired (controlIndex (index + 2)) newcomer joined
             in priorGenerations <> [joined, removed]
          generations = foldl extend [first] changes
          history = checked "mixed history" (Membership.heraldMembershipHistory (NonEmpty.fromList generations))
          lineage = checked "mixed lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId first) (heraldMembershipGenerationId (last generations)) history)
       in conjoin
            [ Membership.heraldMembershipLineageAdmittedHeraldEpochs lineage === newcomers,
              Membership.heraldMembershipLineageRetiredHeraldEpochs lineage === newcomers,
              heraldMembershipGenerationActiveHeraldEpochs (last generations) === (herald 1 :| []),
              Membership.decodeHeraldMembershipHistoryCanonicalBytes (Membership.heraldMembershipHistoryCanonicalBytes history) === Right history,
              conjoin [decodeHeraldMembershipGenerationCanonicalBytes (heraldMembershipGenerationCanonicalBytes generation) === Right generation | generation <- generations],
              Membership.appendHeraldMembershipGeneration (last generations) history === Right history
            ]

admitted :: ControlIndex -> HeraldEpoch -> HeraldMembershipGeneration -> HeraldMembershipGeneration
admitted index newcomer = checked "admit fixture" . Membership.admitHeraldMembershipGeneration index (checked "admission identity fixture" (Membership.deriveHeraldAdmissionId (controlIndex (controlIndexWord64 index - 1)))) newcomer

caseGenesisShape :: Assertion
caseGenesisShape = do
  let generation = genesis (heraldC :| [heraldA, heraldB])
      expected = heraldA :| [heraldB, heraldC]
  assertEqual "active epochs are ascending" expected (heraldMembershipGenerationActiveHeraldEpochs generation)
  assertEqual "genesis has no predecessor" Nothing (heraldMembershipGenerationPredecessor generation)
  assertEqual "genesis has no retirement coordinate" Nothing (heraldMembershipGenerationRetirementControlIndex generation)
  assertEqual "genesis has no retired epoch" Nothing (heraldMembershipGenerationRetiredHeraldEpoch generation)
  assertEqual "member digest is derived from the normalized set" (deriveMemberSetDigest expected) (heraldMembershipGenerationActiveMemberSetDigest generation)
  assertEqual
    "duplicate genesis members reject"
    (Left (MembershipGenerationDuplicateHeraldEpoch heraldA))
    (genesisHeraldMembershipGeneration system (heraldA :| [heraldA]))

caseRetirementShape :: Assertion
caseRetirementShape = do
  let predecessor = genesis fixedMembership
      successor = retired retirementIndex heraldB predecessor
      expected = heraldA :| [heraldC]
  assertEqual "predecessor is exact" (Just (heraldMembershipGenerationId predecessor)) (heraldMembershipGenerationPredecessor successor)
  assertEqual "retirement index is exact" (Just retirementIndex) (heraldMembershipGenerationRetirementControlIndex successor)
  assertEqual "retired epoch is exact" (Just heraldB) (heraldMembershipGenerationRetiredHeraldEpoch successor)
  assertEqual "only the target is removed" expected (heraldMembershipGenerationActiveHeraldEpochs successor)
  assertEqual "successor digest is recomputed" (deriveMemberSetDigest expected) (heraldMembershipGenerationActiveMemberSetDigest successor)
  assertBool "successor identity differs" (heraldMembershipGenerationId predecessor /= heraldMembershipGenerationId successor)

caseRetirementRejections :: Assertion
caseRetirementRejections = do
  let predecessor = genesis fixedMembership
      successor = retired retirementIndex heraldB predecessor
      singleton = genesis (heraldA :| [])
  assertEqual "control index zero is not a retirement" (Left MembershipGenerationRetirementControlIndexZero) (retireHeraldMembershipGeneration (controlIndex 0) (retirementResolution retirementIndex) heraldB predecessor)
  assertEqual "foreign target rejects" (Left (MembershipGenerationTargetNotActive heraldD)) (retireHeraldMembershipGeneration retirementIndex (retirementResolution retirementIndex) heraldD predecessor)
  assertEqual "membership cannot become empty" (Left MembershipGenerationRetirementWouldEmpty) (retireHeraldMembershipGeneration retirementIndex (retirementResolution retirementIndex) heraldA singleton)
  assertEqual "an equal control index cannot contract again" (Left (MembershipGenerationRetirementControlIndexNotIncreasing retirementIndex retirementIndex)) (retireHeraldMembershipGeneration retirementIndex (retirementResolution retirementIndex) heraldA successor)
  assertEqual "the retirement resolution is retained" (Just (retirementResolution retirementIndex)) (Membership.heraldMembershipGenerationRetirementId successor)
  assertEqual "a dismissed probe cannot retire" (Left (MembershipGenerationResolutionNotRetirement dismissed)) (retireHeraldMembershipGeneration retirementIndex dismissed heraldB predecessor)
  assertEqual "the probe must precede its resolution" (Left (MembershipGenerationResolutionNotBeforeRetirement retirementIndex retirementIndex)) (retireHeraldMembershipGeneration retirementIndex sameIndexResolution heraldB predecessor)
  where
    dismissed = deriveFailureProbeResolutionId probe DismissFailureProbe
    sameIndexResolution = deriveFailureProbeResolutionId (checked "same-index probe" (deriveHeraldFailureProbeId retirementIndex)) RetireFailureProbeTarget

caseMembershipRoundTrip :: Assertion
caseMembershipRoundTrip = do
  let predecessor = genesis fixedMembership
      successor = retired retirementIndex heraldB predecessor
  mapM_
    (\generation -> assertEqual (show generation) (Right generation) (decodeHeraldMembershipGenerationCanonicalBytes (heraldMembershipGenerationCanonicalBytes generation)))
    [predecessor, successor, retired (controlIndex 11) heraldA successor]

caseMembershipNegativeControls :: Assertion
caseMembershipNegativeControls = do
  let canonical = heraldMembershipGenerationCanonicalBytes (genesis fixedMembership)
      wrongFamily = replaceByte 8 0x58 canonical
      idStart = 8 + ByteString.length membershipValueDomain + 8
      wrongId = replaceByte idStart 0xff canonical
      wrongDigest = replaceByte (ByteString.length canonical - 1) 0xff canonical
  assertWrongMembershipDomain wrongFamily
  assertDecodeFailure "membership trailing bytes" (decodeHeraldMembershipGenerationCanonicalBytes (canonical <> "\NUL"))
  assertEqual "changed stored ID rejects" (Left MembershipGenerationCanonicalIdentifierMismatch) (decodeHeraldMembershipGenerationCanonicalBytes wrongId)
  assertEqual "changed member digest rejects" (Left MembershipGenerationCanonicalMemberSetDigestMismatch) (decodeHeraldMembershipGenerationCanonicalBytes wrongDigest)

propRetirementHistory :: Property
propRetirementHistory =
  forAll (chooseInt (3, 8)) $ \count ->
    forAll (shuffle (fmap (herald . fromIntegral) [1 .. count])) $ \members ->
      forAll (chooseInt (0, count - 1)) $ \originPosition ->
        forAll (chooseInt (originPosition, count - 1)) $ \targetPosition ->
          let initial = genesis (NonEmpty.fromList members)
              changes = zip [2, 4 ..] (take (count - 1) members)
              generations = scanl (\generation (index, removed) -> retired (controlIndex index) removed generation) initial changes
              history = checked "generated history" (Membership.heraldMembershipHistory (NonEmpty.fromList generations))
              origin = generations !! originPosition
              target = generations !! targetPosition
              lineage = checked "generated lineage" (Membership.heraldMembershipLineage (heraldMembershipGenerationId origin) (heraldMembershipGenerationId target) history)
              appended = foldl (\retained generation -> checked "append generated membership" (Membership.appendHeraldMembershipGeneration generation retained)) (checked "genesis history" (Membership.heraldMembershipHistory (initial :| []))) (drop 1 generations)
           in conjoin
                [ appended === history,
                  Membership.heraldMembershipHistoryGenesis history === initial,
                  Membership.heraldMembershipHistoryCurrent history === last generations,
                  Membership.heraldMembershipLineageFrom (heraldMembershipGenerationId origin) (checked "full interval" (Membership.heraldMembershipLineage (heraldMembershipGenerationId initial) (heraldMembershipGenerationId target) history)) === Right lineage,
                  Membership.heraldMembershipLineageOrigin lineage === origin,
                  Membership.heraldMembershipLineageTarget lineage === target,
                  NonEmpty.toList (Membership.heraldMembershipLineageGenerations lineage) === take (targetPosition - originPosition + 1) (drop originPosition generations),
                  Membership.heraldMembershipLineageRetiredHeraldEpochs lineage === take (targetPosition - originPosition) (drop originPosition members),
                  Membership.decodeHeraldMembershipHistoryCanonicalBytes (Membership.heraldMembershipHistoryCanonicalBytes history) === Right history,
                  Membership.appendHeraldMembershipGeneration (last generations) history === Right history
                ]

caseInvalidHistory :: Assertion
caseInvalidHistory = do
  let first = genesis fixedMembership
      second = retired retirementIndex heraldB first
      third = retired (controlIndex 12) heraldC second
      branch = retired (controlIndex 12) heraldC first
      reused = checked "shape-valid repeated resolution" (retireHeraldMembershipGeneration (controlIndex 12) (retirementResolution retirementIndex) heraldC second)
      history = checked "three generations" (Membership.heraldMembershipHistory (first :| [second, third]))
  assertDecodeFailure "a history must begin at genesis" (Membership.heraldMembershipHistory (second :| [third]))
  assertDecodeFailure "a skipped predecessor rejects" (Membership.heraldMembershipHistory (first :| [third]))
  assertDecodeFailure "a branch cannot be appended" (Membership.appendHeraldMembershipGeneration branch history)
  assertDecodeFailure "a duplicate generation is not an ordered change" (Membership.heraldMembershipHistory (first :| [second, second]))
  assertEqual "one retirement identity cannot name two changes" (Left (Membership.MembershipHistoryRetirementIdentityReused (retirementResolution retirementIndex))) (Membership.heraldMembershipHistory (first :| [second, reused]))
  assertEqual "reverse ancestry is not authorized" (Left (Membership.MembershipHistoryNotDescendant (heraldMembershipGenerationId third) (heraldMembershipGenerationId first))) (Membership.heraldMembershipLineage (heraldMembershipGenerationId third) (heraldMembershipGenerationId first) history)
  assertEqual "a different branch is not an ancestor" (Left (Membership.MembershipHistoryGenerationUnknown (heraldMembershipGenerationId branch))) (Membership.heraldMembershipLineage (heraldMembershipGenerationId branch) (heraldMembershipGenerationId third) history)

caseHistoryCanonical :: Assertion
caseHistoryCanonical = do
  let first = genesis fixedMembership
      second = retired retirementIndex heraldB first
      third = retired (controlIndex 12) heraldC second
      history = checked "history codec fixture" (Membership.heraldMembershipHistory (first :| [second]))
      bytes = Membership.heraldMembershipHistoryCanonicalBytes history
  assertEqual "history canonical round-trip" (Right history) (Membership.decodeHeraldMembershipHistoryCanonicalBytes bytes)
  assertDecodeFailure "history wrong family" (Membership.decodeHeraldMembershipHistoryCanonicalBytes (replaceByte 8 0x58 bytes))
  assertDecodeFailure "history trailing bytes" (Membership.decodeHeraldMembershipHistoryCanonicalBytes (bytes <> "\NUL"))
  assertDecodeFailure "history changed nested payload" (Membership.decodeHeraldMembershipHistoryCanonicalBytes (replaceByte (ByteString.length bytes - 1) 0xff bytes))
  assertDecodeFailure "canonical generation payloads do not authorize a skipped change" (Membership.decodeHeraldMembershipHistoryCanonicalBytes (claim [first, third]))
  assertDecodeFailure "canonical generation payloads do not authorize duplicate changes" (Membership.decodeHeraldMembershipHistoryCanonicalBytes (claim [first, second, second]))
  assertDecodeFailure "an empty encoded history cannot grant ancestry" (Membership.decodeHeraldMembershipHistoryCanonicalBytes (claim []))
  where
    -- The current canonical envelope contains a family blob and an ordered list
    -- of generation blobs. Independent malformed envelopes keep each nested
    -- generation valid so the history decoder must reject the ancestry itself.
    claim generations = blob "ECLIPS-HERALD-MEMBERSHIP-HISTORY" <> count (length generations) <> foldMap (blob . heraldMembershipGenerationCanonicalBytes) generations
    blob value = count (ByteString.length value) <> value
    count value = ByteString.pack [fromIntegral (toInteger value `div` (256 ^ place)) | place <- [7, 6 .. 0 :: Int]]

caseProbeIdentity :: Assertion
caseProbeIdentity = do
  let first = checked "probe 17" (deriveHeraldFailureProbeId (controlIndex 17))
      equal = checked "probe 17 retry" (deriveHeraldFailureProbeId (controlIndex 17))
      other = checked "probe 18" (deriveHeraldFailureProbeId (controlIndex 18))
  assertEqual "control index zero cannot identify a probe" (Left FailureProbeControlIndexZero) (deriveHeraldFailureProbeId (controlIndex 0))
  assertEqual "equal control indices alias" first equal
  assertBool "different control indices do not alias" (first /= other)
  assertEqual "the derivation coordinate is retained" (controlIndex 17) (heraldFailureProbeControlIndex first)
  assertEqual "probe digest width" 32 (ByteString.length (heraldFailureProbeIdBytes first))
  assertEqual "probe canonical round-trip" (Right first) (decodeHeraldFailureProbeIdCanonicalBytes (heraldFailureProbeIdCanonicalBytes first))

caseProbeNegativeControls :: Assertion
caseProbeNegativeControls = do
  let canonical = heraldFailureProbeIdCanonicalBytes probe
      wrongFamily = replaceByte 8 0x58 canonical
      digestStart = 8 + ByteString.length probeValueDomain + 8
      wrongDigest = replaceByte digestStart 0xff canonical
      zeroIndex = ByteString.take (ByteString.length canonical - 8) canonical <> ByteString.replicate 8 0
  assertWrongProbeDomain wrongFamily
  assertDecodeFailure "probe trailing bytes" (decodeHeraldFailureProbeIdCanonicalBytes (canonical <> "\NUL"))
  assertEqual "changed probe digest rejects" (Left FailureProbeIdCanonicalIdentifierMismatch) (decodeHeraldFailureProbeIdCanonicalBytes wrongDigest)
  assertEqual "decoded control index zero rejects" (Left (FailureProbeIdCanonicalAdmissionFailed FailureProbeControlIndexZero)) (decodeHeraldFailureProbeIdCanonicalBytes zeroIndex)

caseResolutionIdentity :: Assertion
caseResolutionIdentity = do
  let dismiss = deriveFailureProbeResolutionId probe DismissFailureProbe
      retire = deriveFailureProbeResolutionId probe RetireFailureProbeTarget
  assertBool "terminal outcomes are domain-separated" (failureProbeResolutionIdBytes dismiss /= failureProbeResolutionIdBytes retire)
  assertEqual "probe identity is retained" probe (failureProbeResolutionProbeId dismiss)
  assertEqual "disposition is retained" DismissFailureProbe (failureProbeResolutionDisposition dismiss)
  mapM_
    (\resolution -> assertEqual (show resolution) (Right resolution) (decodeFailureProbeResolutionIdCanonicalBytes (failureProbeResolutionIdCanonicalBytes resolution)))
    [dismiss, retire]

caseResolutionNegativeControls :: Assertion
caseResolutionNegativeControls = do
  let resolution = deriveFailureProbeResolutionId probe DismissFailureProbe
      canonical = failureProbeResolutionIdCanonicalBytes resolution
      wrongFamily = replaceByte 8 0x58 canonical
      digestStart = 8 + ByteString.length resolutionValueDomain + 8
      wrongDigest = replaceByte digestStart 0xff canonical
      unknownTag = replaceByte (ByteString.length canonical - 1) 2 canonical
  assertWrongResolutionDomain wrongFamily
  assertDecodeFailure "resolution trailing bytes" (decodeFailureProbeResolutionIdCanonicalBytes (canonical <> "\NUL"))
  assertEqual "changed resolution digest rejects" (Left FailureProbeResolutionCanonicalIdentifierMismatch) (decodeFailureProbeResolutionIdCanonicalBytes wrongDigest)
  assertEqual "unknown resolution tag rejects" (Left (FailureProbeResolutionCanonicalUnknownTag 2)) (decodeFailureProbeResolutionIdCanonicalBytes unknownTag)

golden :: String -> String -> ByteString -> TestTree
golden label expected actual =
  testCase label (assertEqual "fixed canonical bytes" expected (renderHex actual))

assertWrongMembershipDomain :: ByteString -> Assertion
assertWrongMembershipDomain bytes = case decodeHeraldMembershipGenerationCanonicalBytes bytes of
  Left MembershipGenerationCanonicalWrongDomain {} -> pure ()
  other -> assertFailure ("expected wrong membership domain, got " <> show other)

assertWrongProbeDomain :: ByteString -> Assertion
assertWrongProbeDomain bytes = case decodeHeraldFailureProbeIdCanonicalBytes bytes of
  Left FailureProbeIdCanonicalWrongDomain {} -> pure ()
  other -> assertFailure ("expected wrong probe domain, got " <> show other)

assertWrongResolutionDomain :: ByteString -> Assertion
assertWrongResolutionDomain bytes = case decodeFailureProbeResolutionIdCanonicalBytes bytes of
  Left FailureProbeResolutionCanonicalWrongDomain {} -> pure ()
  other -> assertFailure ("expected wrong resolution domain, got " <> show other)

assertDecodeFailure :: String -> Either problem value -> Assertion
assertDecodeFailure label actual = assertEqual label True (isFailure actual)

isFailure :: Either problem value -> Bool
isFailure = either (const True) (const False)

replaceByte :: Int -> Word -> ByteString -> ByteString
replaceByte offset byte bytes =
  ByteString.take offset bytes
    <> ByteString.singleton (fromIntegral byte)
    <> ByteString.drop (offset + 1) bytes

renderHex :: ByteString -> String
renderHex = concatMap byteHex . ByteString.unpack
  where
    byteHex byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

genesis :: NonEmpty HeraldEpoch -> HeraldMembershipGeneration
genesis = checked "genesis membership" . genesisHeraldMembershipGeneration system

retired :: ControlIndex -> HeraldEpoch -> HeraldMembershipGeneration -> HeraldMembershipGeneration
retired index target = checked "retired membership" . retireHeraldMembershipGeneration index (retirementResolution index) target

retirementResolution :: ControlIndex -> Membership.FailureProbeResolutionId
retirementResolution index = deriveFailureProbeResolutionId (checked "retirement probe" (deriveHeraldFailureProbeId (controlIndex (controlIndexWord64 index - 1)))) RetireFailureProbeTarget

fixedMembership :: NonEmpty HeraldEpoch
fixedMembership = heraldA :| [heraldB, heraldC]

system :: SystemId
system = checked "system" (mkSystemId (ByteString.replicate 32 0x44))

heraldA, heraldB, heraldC, heraldD :: HeraldEpoch
heraldA = herald 0x11
heraldB = herald 0x22
heraldC = herald 0x33
heraldD = herald 0x55

herald :: Word -> HeraldEpoch
herald byte = checked "Herald epoch" (mkHeraldEpoch (ByteString.replicate 32 (fromIntegral byte)))

retirementIndex :: ControlIndex
retirementIndex = controlIndex 9

probe :: HeraldFailureProbeId
probe = checked "probe" (deriveHeraldFailureProbeId (controlIndex 7))

membershipValueDomain, probeValueDomain, resolutionValueDomain :: ByteString
membershipValueDomain = "ECLIPS-HERALD-MEMBERSHIP-GENERATION-VALUE"
probeValueDomain = "ECLIPS-HERALD-FAILURE-PROBE-VALUE"
resolutionValueDomain = "ECLIPS-HERALD-FAILURE-PROBE-RESOLUTION-VALUE"

genesisGolden, retiredGolden, probeGolden, dismissGolden, retireGolden :: String
genesisGolden =
  "000000000000002945434c4950532d484552414c442d4d454d424552534849502d47454e45524154494f4e2d56414c55450000000000000020a2b7d2813e852c2baef0dd768eb28ef2e69dfa6aff0a8139378904b41a8c34430000000000000000204444444444444444444444444444444444444444444444444444444444444444000000000000000000000300000000000000201111111111111111111111111111111111111111111111111111111111111111000000000000002022222222222222222222222222222222222222222222222222222222222222220000000000000020333333333333333333333333333333333333333333333333333333333333333300000000000000201cbbbd8651f60cf688cdbbc6775ba05f1fbd58b99052c75dc6d0d1206d12c8a2"
retiredGolden =
  "000000000000002945434c4950532d484552414c442d4d454d424552534849502d47454e45524154494f4e2d56414c5545000000000000002086296e8ac841e2c4845b0135156dcff067e7c1433418b9e035d4c26821947856010000000000000000010000000000000020a2b7d2813e852c2baef0dd768eb28ef2e69dfa6aff0a8139378904b41a8c3443010000000000000009010000000000000020222222222222222222222222222222222222222222222222222222222222222200000000000000be000000000000002c45434c4950532d484552414c442d4641494c5552452d50524f42452d5245534f4c5554494f4e2d56414c5545000000000000002007f0ed4ed069333d217b68250afaf6e4fa2271ae820761168a28375ffaa261cd0000000000000059000000000000002145434c4950532d484552414c442d4641494c5552452d50524f42452d56414c5545000000000000002057718f4965e30e2bfffecd4bf794d4ac002b6e7964e6ebe3f3b83641ee3ad849000000000000000801000000000000000200000000000000201111111111111111111111111111111111111111111111111111111111111111000000000000002033333333333333333333333333333333333333333333333333333333333333330000000000000020740cd64b8dd053df22971a6f0c46ae828ec4e631ccf6f35fe1025888bfd30843"
probeGolden =
  "000000000000002145434c4950532d484552414c442d4641494c5552452d50524f42452d56414c554500000000000000208dea4b314901592d18a2816033c48bb021b37eafa9b97ba257d9ac6f5d4db2720000000000000007"
dismissGolden =
  "000000000000002c45434c4950532d484552414c442d4641494c5552452d50524f42452d5245534f4c5554494f4e2d56414c554500000000000000208879b33bd8a5cbc9964e0982d0f5b146c060644ed90335f9c3954df20349ee530000000000000059000000000000002145434c4950532d484552414c442d4641494c5552452d50524f42452d56414c554500000000000000208dea4b314901592d18a2816033c48bb021b37eafa9b97ba257d9ac6f5d4db272000000000000000700"
retireGolden =
  "000000000000002c45434c4950532d484552414c442d4641494c5552452d50524f42452d5245534f4c5554494f4e2d56414c55450000000000000020fbb2e310dbfb26c4aa41af5e997c50e99cad6ba97211ddd1731b1ce83cbb14af0000000000000059000000000000002145434c4950532d484552414c442d4641494c5552452d50524f42452d56414c554500000000000000208dea4b314901592d18a2816033c48bb021b37eafa9b97ba257d9ac6f5d4db272000000000000000701"

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
