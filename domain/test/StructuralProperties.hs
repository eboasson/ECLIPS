module StructuralProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Eclips.Domain.Disappearance
  ( deriveDisappearanceProbeId,
    disappearanceProbeIdBytes,
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    StructuralSequence,
    StructuralSequenceError (..),
    SystemId,
    controlIndex,
    firstStructuralSequence,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkProcessEpochId,
    mkStructuralSequence,
    mkSystemId,
    nextStructuralSequence,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (..),
    structuralCarrierRoleTag,
  )
import Eclips.Domain.Structural
  ( StructuralPrefix,
    StructuralVectorProblem (..),
    StructuralVersionVector,
    emptyStructuralPrefix,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    nextAfterStructuralPrefix,
    structuralPredecessorPrefix,
    structuralPrefixSequence,
    structuralPrefixThrough,
    structuralVersionVectorComponent,
    structuralVersionVectorCovers,
    structuralVersionVectorEntries,
    structuralVersionVectorFromClaimedCoordinates,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCauseError
      ( StructuralConsequenceControlIndexMustBePositive
      ),
    StructuralConsequenceCauseView (..),
    labelReleaseCause,
    predefinedDisappearanceCause,
    processEndCause,
    structuralConsequenceCauseCanonicalBytes,
    structuralConsequenceCauseControlIndex,
    structuralConsequenceCauseStructuralOccurrence,
    structuralConsequenceCauseView,
    structuralOccurrenceCause,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "structural identity and vectors"
    [ testGroup
        "positive source sequence"
        [ testProperty "zero alone is rejected" propStructuralSequenceAdmission,
          testProperty "first and next retain positive numeric order" propStructuralSequenceNext,
          testProperty "occurrence accessors retain the exact pair" propOccurrenceAccessors,
          testProperty "occurrence ordering is the canonical pair ordering" propOccurrenceOrder
        ],
      testGroup
        "prefix"
        [ testCase "empty and through have explicit next sequences" casePrefixNext,
          testProperty "the allocation predecessor is exact" propPredecessorPrefix
        ],
      testGroup
        "exact generation-qualified vector"
        [ testCase "presentation order normalizes to ascending membership" caseVectorNormalization,
          testCase "a repeated component is rejected" caseVectorDuplicate,
          testCase "a missing component is rejected" caseVectorMissing,
          testCase "a foreign component is rejected" caseVectorForeign,
          testCase "claimed coordinates check shape while deferring generation resolution" caseClaimedVector,
          testCase "genesis contains one empty component per member" caseGenesisVector,
          testProperty "componentwise coverage is reflexive" propCoverageReflexive,
          testProperty "componentwise coverage is antisymmetric" propCoverageAntisymmetric,
          testProperty "componentwise coverage is transitive" propCoverageTransitive,
          testCase "crossed components are causally incomparable" caseCrossedComponents,
          testCase "unequal memberships never cover one another" caseUnequalMembership,
          testCase "equal member sets in distinct generations remain incomparable" caseUnequalGeneration
        ],
      testGroup
        "consequence cause"
        [ testCase "control-caused work requires a positive control index" caseCausePositiveControl,
          testCase "each cause retains its exact nominal provenance" caseCauseViews,
          testCase "canonical cause tags and field order are fixed" caseCauseCanonicalBytes
        ],
      testCase "structural carrier transcript tags are fixed" caseCarrierRoleTags
    ]

propStructuralSequenceAdmission :: Word64 -> Property
propStructuralSequenceAdmission value =
  case mkStructuralSequence value of
    Left problem ->
      conjoin
        [ value === 0,
          problem === StructuralSequenceMustBePositive
        ]
    Right sequenceNumber ->
      conjoin
        [ counterexample "an admitted structural sequence is positive" (value > 0),
          structuralSequenceWord64 sequenceNumber === value
        ]

-- Word32 keeps this property wholly inside the profile's assumed finite range;
-- counter-exhaustion behavior is intentionally not part of the model.
propStructuralSequenceNext :: Word32 -> Property
propStructuralSequenceNext offset =
  conjoin
    [ structuralSequenceWord64 firstStructuralSequence === 1,
      structuralSequenceWord64 (nextStructuralSequence predecessor)
        === structuralSequenceWord64 predecessor + 1
    ]
  where
    predecessor = positiveSequence (fromIntegral offset + 1)

propOccurrenceAccessors :: Word8 -> Word32 -> Property
propOccurrenceAccessors sourceByte offset =
  conjoin
    [ structuralOccurrenceSourceHeraldEpoch occurrence === source,
      structuralOccurrenceSourceSequence occurrence === sequenceNumber
    ]
  where
    source = herald sourceByte
    sequenceNumber = positiveSequence (fromIntegral offset + 1)
    occurrence = structuralOccurrenceId source sequenceNumber

propOccurrenceOrder :: Word8 -> Word16 -> Word8 -> Word16 -> Property
propOccurrenceOrder leftSource leftOffset rightSource rightOffset =
  compare left right === compare leftPair rightPair
  where
    leftPair =
      ( herald leftSource,
        positiveSequence (fromIntegral leftOffset + 1)
      )
    rightPair =
      ( herald rightSource,
        positiveSequence (fromIntegral rightOffset + 1)
      )
    left = uncurry structuralOccurrenceId leftPair
    right = uncurry structuralOccurrenceId rightPair

casePrefixNext :: Assertion
casePrefixNext = do
  let third = positiveSequence 3
  assertEqual
    "empty starts at one"
    firstStructuralSequence
    (nextAfterStructuralPrefix emptyStructuralPrefix)
  assertEqual
    "a non-empty prefix advances its sequence"
    (positiveSequence 4)
    (nextAfterStructuralPrefix (structuralPrefixThrough third))

propPredecessorPrefix :: Word32 -> Property
propPredecessorPrefix offset =
  structuralPrefixSequence (structuralPredecessorPrefix sequenceNumber)
    === expected
  where
    value = fromIntegral offset + 1
    sequenceNumber = positiveSequence value
    expected
      | value == 1 = Nothing
      | otherwise = Just (positiveSequence (value - 1))

caseVectorNormalization :: Assertion
caseVectorNormalization = do
  let entries =
        [ (herald 1, emptyStructuralPrefix),
          (herald 2, structuralPrefixThrough (positiveSequence 4)),
          (herald 3, structuralPrefixThrough firstStructuralSequence)
        ]
      forward = checkedVector fixtureGeneration entries
      backward = checkedVector fixtureGeneration (reverse entries)
  assertEqual "presentation does not affect the vector" forward backward
  assertEqual
    "entries are ascending"
    (sort entries)
    (structuralVersionVectorEntries forward)

caseVectorDuplicate :: Assertion
caseVectorDuplicate =
  assertEqual
    "even an equal repeated component is not an exact map presentation"
    (Left (StructuralVectorDuplicateComponent (herald 1)))
    ( mkStructuralVersionVector
        fixtureGeneration
        [ (herald 1, emptyStructuralPrefix),
          (herald 2, emptyStructuralPrefix),
          (herald 1, emptyStructuralPrefix),
          (herald 3, emptyStructuralPrefix)
        ]
    )

caseVectorMissing :: Assertion
caseVectorMissing =
  assertEqual
    "the first missing member is named"
    (Left (StructuralVectorMissingComponent (herald 2)))
    ( mkStructuralVersionVector
        fixtureGeneration
        [ (herald 1, emptyStructuralPrefix),
          (herald 3, emptyStructuralPrefix)
        ]
    )

caseVectorForeign :: Assertion
caseVectorForeign =
  assertEqual
    "the first foreign member is named before any missing member"
    (Left (StructuralVectorForeignComponent (herald 4)))
    ( mkStructuralVersionVector
        fixtureGeneration
        [ (herald 1, emptyStructuralPrefix),
          (herald 3, emptyStructuralPrefix),
          (herald 4, emptyStructuralPrefix)
        ]
    )

caseClaimedVector :: Assertion
caseClaimedVector = do
  let claimedMembers = herald 1 :| [herald 2]
      claimedMemberDigest =
        heraldMembershipGenerationActiveMemberSetDigest
          (generation 0x42 claimedMembers)
      claimedEntries =
        (herald 2, structuralPrefixThrough firstStructuralSequence)
          :| [(herald 1, emptyStructuralPrefix)]
      claimed =
        checked
          "claimed StructuralVersionVector"
          ( structuralVersionVectorFromClaimedCoordinates
              (heraldMembershipGenerationId fixtureGeneration)
              claimedMemberDigest
              claimedEntries
          )
  assertEqual
    "claimed entries normalize without resolving the generation"
    [ (herald 1, emptyStructuralPrefix),
      (herald 2, structuralPrefixThrough firstStructuralSequence)
    ]
    (structuralVersionVectorEntries claimed)
  assertEqual
    "the unresolved generation claim is retained"
    (heraldMembershipGenerationId fixtureGeneration)
    (structuralVersionVectorMembershipGenerationId claimed)
  assertEqual
    "a digest for different keys is rejected at the boundary"
    ( Left
        ( StructuralVectorClaimedMemberSetDigestMismatch
            claimedMemberDigest
            (heraldMembershipGenerationActiveMemberSetDigest fixtureGeneration)
        )
    )
    ( structuralVersionVectorFromClaimedCoordinates
        (heraldMembershipGenerationId fixtureGeneration)
        (heraldMembershipGenerationActiveMemberSetDigest fixtureGeneration)
        claimedEntries
    )
  assertEqual
    "a duplicate claimed component is rejected"
    (Left (StructuralVectorDuplicateComponent (herald 1)))
    ( structuralVersionVectorFromClaimedCoordinates
        (heraldMembershipGenerationId fixtureGeneration)
        claimedMemberDigest
        ( (herald 1, emptyStructuralPrefix)
            :| [(herald 1, emptyStructuralPrefix)]
        )
    )

caseGenesisVector :: Assertion
caseGenesisVector = do
  let genesis = emptyStructuralVersionVector fixtureGeneration
  assertEqual
    "one normalized empty component per fixed member"
    [ (herald 1, emptyStructuralPrefix),
      (herald 2, emptyStructuralPrefix),
      (herald 3, emptyStructuralPrefix)
    ]
    (structuralVersionVectorEntries genesis)
  assertEqual
    "component lookup retains empty explicitly"
    (Just emptyStructuralPrefix)
    (structuralVersionVectorComponent (herald 2) genesis)
  assertEqual
    "a foreign component is absent"
    Nothing
    (structuralVersionVectorComponent (herald 4) genesis)
  assertEqual
    "the exact generation is retained"
    (heraldMembershipGenerationId fixtureGeneration)
    (structuralVersionVectorMembershipGenerationId genesis)
  assertEqual
    "the checked generation's member-set digest is retained"
    (heraldMembershipGenerationActiveMemberSetDigest fixtureGeneration)
    (structuralVersionVectorMemberSetDigest genesis)

propCoverageReflexive :: Word16 -> Word16 -> Word16 -> Property
propCoverageReflexive first second third =
  structuralVersionVectorCovers vector vector === True
  where
    vector = vectorFromWords fixtureGeneration [first, second, third]

propCoverageAntisymmetric ::
  Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Property
propCoverageAntisymmetric firstA secondA thirdA firstB secondB thirdB =
  counterexample
    "mutual componentwise coverage implies exact equality"
    (not (left `structuralVersionVectorCovers` right && right `structuralVersionVectorCovers` left) || left == right)
  where
    left = vectorFromWords fixtureGeneration [firstA, secondA, thirdA]
    right = vectorFromWords fixtureGeneration [firstB, secondB, thirdB]

propCoverageTransitive ::
  Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Property
propCoverageTransitive firstA firstB firstC secondA secondB secondC thirdA thirdB thirdC =
  conjoin
    [ high `structuralVersionVectorCovers` middle === True,
      middle `structuralVersionVectorCovers` low === True,
      high `structuralVersionVectorCovers` low === True
    ]
  where
    (firstLow, firstMiddle, firstHigh) = orderedTriple firstA firstB firstC
    (secondLow, secondMiddle, secondHigh) = orderedTriple secondA secondB secondC
    (thirdLow, thirdMiddle, thirdHigh) = orderedTriple thirdA thirdB thirdC
    low = vectorFromWords fixtureGeneration [firstLow, secondLow, thirdLow]
    middle = vectorFromWords fixtureGeneration [firstMiddle, secondMiddle, thirdMiddle]
    high = vectorFromWords fixtureGeneration [firstHigh, secondHigh, thirdHigh]

caseCrossedComponents :: Assertion
caseCrossedComponents = do
  let left = vectorFromWords fixtureGeneration [2, 0, 0]
      right = vectorFromWords fixtureGeneration [0, 2, 0]
  assertBool
    "progress at the first member does not cover progress at the second"
    (not (left `structuralVersionVectorCovers` right))
  assertBool
    "progress at the second member does not cover progress at the first"
    (not (right `structuralVersionVectorCovers` left))

caseUnequalMembership :: Assertion
caseUnequalMembership = do
  let shorterMembers = herald 1 :| [herald 2]
      shorterGeneration = generation 0x42 shorterMembers
      full = emptyStructuralVersionVector fixtureGeneration
      shorter = emptyStructuralVersionVector shorterGeneration
  assertBool
    "the longer membership does not cover the shorter"
    (not (full `structuralVersionVectorCovers` shorter))
  assertBool
    "the shorter membership does not cover the longer"
    (not (shorter `structuralVersionVectorCovers` full))

caseUnequalGeneration :: Assertion
caseUnequalGeneration = do
  let otherGeneration = generation 0x43 fixtureMembers
      firstVector = emptyStructuralVersionVector fixtureGeneration
      otherVector = emptyStructuralVersionVector otherGeneration
  assertEqual
    "equal active sets retain equal member-set digests"
    (structuralVersionVectorMemberSetDigest firstVector)
    (structuralVersionVectorMemberSetDigest otherVector)
  assertBool
    "generation identities remain distinct"
    ( structuralVersionVectorMembershipGenerationId firstVector
        /= structuralVersionVectorMembershipGenerationId otherVector
    )
  assertBool
    "the first generation does not cover the second"
    (not (firstVector `structuralVersionVectorCovers` otherVector))
  assertBool
    "the second generation does not cover the first"
    (not (otherVector `structuralVersionVectorCovers` firstVector))

caseCausePositiveControl :: Assertion
caseCausePositiveControl = do
  assertEqual
    "a label release cannot claim control zero"
    (Left StructuralConsequenceControlIndexMustBePositive)
    (labelReleaseCause (labelDecision 11) (controlIndex 0))
  assertEqual
    "a process End cannot claim control zero"
    (Left StructuralConsequenceControlIndexMustBePositive)
    (processEndCause (processEpoch 12) (controlIndex 0))
  assertEqual
    "a predefined disappearance cannot claim control zero"
    (Left StructuralConsequenceControlIndexMustBePositive)
    ( predefinedDisappearanceCause
        (checked "disappearance probe" (deriveDisappearanceProbeId (controlIndex 13)))
        (controlIndex 0)
    )

caseCauseViews :: Assertion
caseCauseViews = do
  let occurrence = structuralOccurrenceId (herald 13) (positiveSequence 14)
      decision = labelDecision 15
      process = processEpoch 16
      probe = checked "disappearance probe" (deriveDisappearanceProbeId (controlIndex 17))
      labelCause = checked "label release cause" (labelReleaseCause decision (controlIndex 17))
      endCause = checked "process End cause" (processEndCause process (controlIndex 18))
      disappearanceCause =
        checked
          "predefined disappearance cause"
          (predefinedDisappearanceCause probe (controlIndex 19))
  assertEqual
    "structural provenance"
    (StructuralOccurrenceCauseView occurrence)
    (structuralConsequenceCauseView (structuralOccurrenceCause occurrence))
  assertEqual
    "label-release provenance"
    (LabelReleaseCauseView decision (controlIndex 17))
    (structuralConsequenceCauseView labelCause)
  assertEqual
    "process-End provenance"
    (ProcessEndCauseView process (controlIndex 18))
    (structuralConsequenceCauseView endCause)
  assertEqual
    "predefined-disappearance provenance"
    (PredefinedDisappearanceCauseView probe (controlIndex 19))
    (structuralConsequenceCauseView disappearanceCause)
  assertEqual
    "predefined disappearance is not a structural occurrence"
    Nothing
    (structuralConsequenceCauseStructuralOccurrence disappearanceCause)
  assertEqual
    "predefined disappearance retains its ordered control position"
    (Just (controlIndex 19))
    (structuralConsequenceCauseControlIndex disappearanceCause)

caseCauseCanonicalBytes :: Assertion
caseCauseCanonicalBytes = do
  let occurrence = structuralOccurrenceId (herald 21) (positiveSequence 22)
      decision = labelDecision 23
      process = processEpoch 24
      probe = checked "disappearance probe" (deriveDisappearanceProbeId (controlIndex 27))
      counter value = ByteString.replicate 7 0 <> ByteString.singleton value
  assertEqual
    "structural tag, source, then sequence"
    (ByteString.singleton 0 <> ByteString.replicate 32 21 <> counter 22)
    ( structuralConsequenceCauseCanonicalBytes
        (structuralOccurrenceCause occurrence)
    )
  assertEqual
    "label tag, decision, then control index"
    (ByteString.singleton 1 <> ByteString.replicate 32 23 <> counter 25)
    ( structuralConsequenceCauseCanonicalBytes
        (checked "label cause" (labelReleaseCause decision (controlIndex 25)))
    )
  assertEqual
    "End tag, process, then control index"
    (ByteString.singleton 2 <> ByteString.replicate 32 24 <> counter 26)
    ( structuralConsequenceCauseCanonicalBytes
        (checked "End cause" (processEndCause process (controlIndex 26)))
    )
  assertEqual
    "predefined-disappearance tag, probe, then control index"
    (ByteString.singleton 3 <> disappearanceProbeIdBytes probe <> counter 28)
    ( structuralConsequenceCauseCanonicalBytes
        ( checked
            "predefined disappearance cause"
            (predefinedDisappearanceCause probe (controlIndex 28))
        )
    )

caseCarrierRoleTags :: Assertion
caseCarrierRoleTags =
  assertEqual
    "the five closed role tags"
    [0, 1, 2, 3, 4]
    ( fmap
        structuralCarrierRoleTag
        [ NeutralVertexCarrier,
          EdgeCarrier,
          NablaCarrier,
          DeltaCarrier,
          ProcessEpochCarrier
        ]
    )

fixtureMembers :: NonEmpty HeraldEpoch
fixtureMembers = herald 1 :| [herald 2, herald 3]

fixtureGeneration :: HeraldMembershipGeneration
fixtureGeneration = generation 0x41 fixtureMembers

vectorFromWords ::
  HeraldMembershipGeneration -> [Word16] -> StructuralVersionVector
vectorFromWords generationValue wordsForPrefixes =
  checkedVector
    generationValue
    ( zip
        ( nonEmptyToList
            (heraldMembershipGenerationActiveHeraldEpochs generationValue)
        )
        (fmap prefixFromWord wordsForPrefixes)
    )

prefixFromWord :: Word16 -> StructuralPrefix
prefixFromWord 0 = emptyStructuralPrefix
prefixFromWord value =
  structuralPrefixThrough (positiveSequence (fromIntegral value))

orderedTriple :: (Ord value) => value -> value -> value -> (value, value, value)
orderedTriple first second third = case sort [first, second, third] of
  [low, middle, high] -> (low, middle, high)
  _ -> error "three values did not sort to three values"

herald :: Word8 -> HeraldEpoch
herald byte = checked "HeraldEpoch" (mkHeraldEpoch (ByteString.replicate 32 byte))

labelDecision :: Word8 -> LabelDecisionId
labelDecision byte =
  checked "LabelDecisionId" (mkLabelDecisionId (ByteString.replicate 32 byte))

processEpoch :: Word8 -> ProcessEpochId
processEpoch byte =
  checked "ProcessEpochId" (mkProcessEpochId (ByteString.replicate 32 byte))

positiveSequence :: Word64 -> StructuralSequence
positiveSequence value =
  checked "StructuralSequence" (mkStructuralSequence value)

checkedVector ::
  HeraldMembershipGeneration ->
  [(HeraldEpoch, StructuralPrefix)] ->
  StructuralVersionVector
checkedVector generationValue entries =
  checked
    "StructuralVersionVector"
    (mkStructuralVersionVector generationValue entries)

generation :: Word8 -> NonEmpty HeraldEpoch -> HeraldMembershipGeneration
generation systemByte members =
  checked
    "HeraldMembershipGeneration"
    (genesisHeraldMembershipGeneration (system systemByte) members)

system :: Word8 -> SystemId
system byte =
  checked "SystemId" (mkSystemId (ByteString.replicate 32 byte))

checked :: (Show problem) => String -> Either problem value -> value
checked _ (Right value) = value
checked label (Left problem) =
  error (label <> " fixture rejected: " <> show problem)

nonEmptyToList :: NonEmpty value -> [value]
nonEmptyToList (first :| remaining) = first : remaining
