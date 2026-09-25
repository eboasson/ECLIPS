{-# LANGUAGE OverloadedStrings #-}

module AlignmentProperties
  ( tests,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( AlignmentCut,
    AlignmentMember,
    AlignmentShapeError (..),
    ContextClassGenerationId,
    FreshMemberBaseEvidence,
    HeraldPublicationPrefix (..),
    PhysicalPlacementRevisionVector,
    alignmentCut,
    alignmentCutAtAttempt,
    alignmentCutCanonicalBytes,
    alignmentCutExactMembers,
    alignmentCutFreshMemberBaseEvidence,
    alignmentCutPredecessorGenerationIds,
    alignmentMember,
    alignmentMemberDelta,
    alignmentPlanAttemptMembershipChange,
    alignmentPlanAttemptWord64,
    alignmentSnapshotDigestBytes,
    bootstrapEvidenceDigestBytes,
    contextClassGenerationIdBytes,
    deriveAlignmentSnapshotDigest,
    deriveBootstrapEvidenceDigest,
    deriveContextClassGenerationId,
    deriveHistoricalCertificateDigest,
    deriveMemberReadyEvidenceDigest,
    deriveRouteCutoverEvidenceDigest,
    firstHeraldPublicationPosition,
    firstPlacementRevision,
    freshMemberBaseDelta,
    freshMemberBaseEvidence,
    heraldPublicationPositionPredecessor,
    heraldPublicationPrefixPosition,
    historicalCertificateDigestBytes,
    initialStoreRevision,
    memberReadyEvidenceDigestBytes,
    mkAlignmentPlanAttempt,
    mkAlignmentPlanAttemptAtMembership,
    mkAlignmentSnapshotDigest,
    mkBootstrapEvidenceDigest,
    mkContextClassGenerationId,
    mkHeraldPublicationPosition,
    mkHistoricalCertificateDigest,
    mkMemberReadyEvidenceDigest,
    mkPlacementRevision,
    mkRouteCutoverEvidenceDigest,
    nextAlignmentPlanAttempt,
    nextHeraldPublicationPosition,
    nextPlacementRevision,
    nextStoreRevision,
    physicalPlacementRevisionEntries,
    physicalPlacementRevisionMemberSetDigest,
    physicalPlacementRevisionMembershipGenerationId,
    physicalPlacementRevisionVector,
    physicalPlacementRevisionVectorFromClaimedCoordinates,
    placementRevisionWord64,
    routeCutoverEvidenceDigestBytes,
    storeRevisionWord64,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    HeraldEpoch,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    SystemId,
    TopologyCutId,
    controlIndex,
    mkDeltaId,
    mkHeraldEpoch,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkStoreIncarnationId,
    mkSystemId,
    mkTopologyCutId,
    topologyCutIdBytes,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    emptyStructuralVersionVector,
  )
import Eclips.Domain.Topology
  ( TopologyCut,
    deriveTopologyCutId,
    deriveTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyCutCanonicalBytes,
    topologyFrontier,
  )
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "alignment identity"
    [ testProperty "all nominal alignment digests require exactly 32 bytes" propDigestWidths,
      testCase "revision domains retain their distinct zero and positive origins" caseRevisions,
      testCase "physical placement is exact, complete, and ascending" casePlacementVector,
      testCase "claimed placement checks shape while deferring generation resolution" caseClaimedPlacement,
      testCase "alignment rejects a placement from another generation" casePlacementGeneration,
      testCase "alignment cuts normalize exact members, ancestry, and fresh bases" caseCutNormalization,
      testCase "fresh bases must name an exact member incarnation" caseFreshBaseMembership,
      testCase "generation identity covers every canonical cut component" caseGenerationSensitivity,
      testProperty "membership changes order alignment attempts before local retry ordinals" propAttemptMembershipOrder,
      testCase "topology and generation derivations hash their exposed canonical bytes" caseCanonicalBytes,
      testCase "alignment digest domains are bytewise separated" caseDigestDomains,
      testCase "alignment transcript tags and digests have pinned golden vectors" caseGoldens
    ]

propDigestWidths :: Property
propDigestWidths =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let bytes = ByteString.replicate byteCount 0
        malformed = byteCount /= 32
     in counterexample ("byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (mkContextClassGenerationId bytes) === malformed,
              isLeft (mkMemberReadyEvidenceDigest bytes) === malformed,
              isLeft (mkHistoricalCertificateDigest bytes) === malformed,
              isLeft (mkAlignmentSnapshotDigest bytes) === malformed,
              isLeft (mkBootstrapEvidenceDigest bytes) === malformed,
              isLeft (mkRouteCutoverEvidenceDigest bytes) === malformed
            ]

caseRevisions :: IO ()
caseRevisions = do
  assertEqual "fresh store is revision zero" 0 (storeRevisionWord64 initialStoreRevision)
  assertEqual "first retained transition advances to one" 1 (storeRevisionWord64 (nextStoreRevision initialStoreRevision))
  assertBool "placement revision zero is rejected" (isLeft (mkPlacementRevision 0))
  assertEqual "placement starts at one" 1 (placementRevisionWord64 firstPlacementRevision)
  assertEqual "placement advances monotonically" 2 (placementRevisionWord64 (nextPlacementRevision firstPlacementRevision))
  assertBool "publication position zero is rejected" (isLeft (mkHeraldPublicationPosition 0))
  assertEqual "the first position has no predecessor" Nothing (heraldPublicationPositionPredecessor firstHeraldPublicationPosition)
  assertEqual
    "the second position's predecessor is the first"
    (Just firstHeraldPublicationPosition)
    (heraldPublicationPositionPredecessor (nextHeraldPublicationPosition firstHeraldPublicationPosition))
  assertEqual "empty prefix is explicit" Nothing (heraldPublicationPrefixPosition EmptyHeraldPublicationPrefix)
  assertEqual
    "through prefix retains its positive position"
    (Just firstHeraldPublicationPosition)
    (heraldPublicationPrefixPosition (HeraldPublicationPrefixThrough firstHeraldPublicationPosition))

casePlacementVector :: IO ()
casePlacementVector = do
  assertEqual
    "input is normalized by HeraldEpoch"
    ((heraldA, firstPlacementRevision) :| [(heraldB, nextPlacementRevision firstPlacementRevision)])
    (physicalPlacementRevisionEntries placementVector)
  assertEqual
    "the exact checked membership generation is retained"
    (heraldMembershipGenerationId membershipGeneration)
    (physicalPlacementRevisionMembershipGenerationId placementVector)
  assertEqual
    "the checked member-set digest is retained"
    (heraldMembershipGenerationActiveMemberSetDigest membershipGeneration)
    (physicalPlacementRevisionMemberSetDigest placementVector)
  assertEqual
    "a missing fixed member is rejected"
    ( Left
        ( PhysicalPlacementRevisionMembershipMismatch
            [heraldA, heraldB]
            [heraldA]
        )
    )
    (physicalPlacementRevisionVector membershipGeneration [(heraldA, firstPlacementRevision)])
  assertEqual
    "a repeated supplied Herald is not one exact entry"
    (Left (PhysicalPlacementRevisionDuplicateHerald heraldA))
    ( physicalPlacementRevisionVector
        membershipGeneration
        [ (heraldA, firstPlacementRevision),
          (heraldA, firstPlacementRevision),
          (heraldB, firstPlacementRevision)
        ]
    )

caseClaimedPlacement :: IO ()
caseClaimedPlacement = do
  let oneMemberGeneration =
        checked
          "one-member membership generation"
          (genesisHeraldMembershipGeneration otherSystemId (heraldA :| []))
      claimedMemberDigest =
        heraldMembershipGenerationActiveMemberSetDigest oneMemberGeneration
      claimedEntries = (heraldA, firstPlacementRevision) :| []
      claimed =
        checked
          "claimed placement vector"
          ( physicalPlacementRevisionVectorFromClaimedCoordinates
              (heraldMembershipGenerationId membershipGeneration)
              claimedMemberDigest
              claimedEntries
          )
  assertEqual
    "the unresolved generation claim is retained"
    (heraldMembershipGenerationId membershipGeneration)
    (physicalPlacementRevisionMembershipGenerationId claimed)
  assertEqual
    "the claimed entry is normalized and retained"
    claimedEntries
    (physicalPlacementRevisionEntries claimed)
  assertEqual
    "a digest for different entry keys is rejected at the boundary"
    ( Left
        ( PhysicalPlacementRevisionClaimedMemberSetDigestMismatch
            claimedMemberDigest
            (heraldMembershipGenerationActiveMemberSetDigest membershipGeneration)
        )
    )
    ( physicalPlacementRevisionVectorFromClaimedCoordinates
        (heraldMembershipGenerationId membershipGeneration)
        (heraldMembershipGenerationActiveMemberSetDigest membershipGeneration)
        claimedEntries
    )
  assertEqual
    "a duplicate claimed Herald is rejected"
    (Left (PhysicalPlacementRevisionDuplicateHerald heraldA))
    ( physicalPlacementRevisionVectorFromClaimedCoordinates
        (heraldMembershipGenerationId membershipGeneration)
        claimedMemberDigest
        ( (heraldA, firstPlacementRevision)
            :| [(heraldA, firstPlacementRevision)]
        )
    )

casePlacementGeneration :: IO ()
casePlacementGeneration = do
  let otherGeneration =
        checked
          "other membership generation"
          (genesisHeraldMembershipGeneration otherSystemId fixedMembership)
      otherPlacement =
        checked
          "other generation placement"
          ( physicalPlacementRevisionVector
              otherGeneration
              [ (heraldA, firstPlacementRevision),
                (heraldB, nextPlacementRevision firstPlacementRevision)
              ]
          )
  assertEqual
    "equal active member sets do not erase generation lineage"
    ( Left
        ( AlignmentTopologyPlacementGenerationMismatch
            (heraldMembershipGenerationId membershipGeneration)
            (heraldMembershipGenerationId otherGeneration)
        )
    )
    ( alignmentCut
        sortA
        occurrenceA
        topologyA
        otherPlacement
        members
        predecessors
        freshBases
    )

caseCutNormalization :: IO ()
caseCutNormalization = do
  let reordered =
        checked
          "reordered alignment cut"
          ( alignmentCut
              sortA
              occurrenceA
              topologyA
              placementVector
              (memberB :| [memberA, memberA])
              [predecessorB, predecessorA, predecessorA]
              [freshB, freshA, freshA]
          )
  assertEqual "presentation does not affect the canonical cut" canonicalCut reordered
  assertEqual
    "members are sorted by exact DeltaId"
    [deltaA, deltaB]
    (fmap alignmentMemberDelta (NonEmpty.toList (alignmentCutExactMembers canonicalCut)))
  assertEqual
    "predecessor generation IDs are sorted and unique"
    [predecessorA, predecessorB]
    (alignmentCutPredecessorGenerationIds canonicalCut)
  assertEqual
    "fresh bases are sorted by their exact member DeltaId"
    [deltaA, deltaB]
    (fmap freshMemberBaseDelta (alignmentCutFreshMemberBaseEvidence canonicalCut))

caseFreshBaseMembership :: IO ()
caseFreshBaseMembership = do
  let wrongBase = freshMemberBaseEvidence deltaA storeB initialStoreRevision
  assertEqual
    "another member's store does not satisfy the Delta/store pair"
    (Left (FreshMemberBaseNotExactMember deltaA storeB))
    ( alignmentCut
        sortA
        occurrenceA
        topologyA
        placementVector
        (memberA :| [memberB])
        []
        [wrongBase]
    )
  assertBool
    "one store incarnation cannot back two exact members"
    ( isLeft
        ( alignmentCut
            sortA
            occurrenceA
            topologyA
            placementVector
            (memberA :| [alignmentMember deltaB storeA heraldB])
            []
            []
        )
    )

caseGenerationSensitivity :: IO ()
caseGenerationSensitivity = do
  let base = deriveContextClassGenerationId canonicalCut
      changed =
        fmap
          (deriveContextClassGenerationId . checked "changed alignment cut")
          [ alignmentCutAtAttempt (mkAlignmentPlanAttemptAtMembership (controlIndex 1) 0) sortA occurrenceA topologyA placementVector members predecessors freshBases,
            alignmentCutAtAttempt (mkAlignmentPlanAttempt 1) sortA occurrenceA topologyA placementVector members predecessors freshBases,
            alignmentCut sortB occurrenceA topologyA placementVector members predecessors freshBases,
            alignmentCut sortA occurrenceB topologyA placementVector members predecessors freshBases,
            alignmentCut sortA occurrenceA topologyB placementVector members predecessors freshBases,
            alignmentCut sortA occurrenceA topologyA placementVectorB members predecessors freshBases,
            alignmentCut sortA occurrenceA topologyA placementVector changedMembers predecessors freshBases,
            alignmentCut sortA occurrenceA topologyA placementVector members [predecessorA] freshBases,
            alignmentCut sortA occurrenceA topologyA placementVector members predecessors [freshB, changedFreshA]
          ]
  assertBool "every changed canonical component changes the generation" (all (/= base) changed)

propAttemptMembershipOrder :: Property
propAttemptMembershipOrder =
  forAll (chooseInt (0, 1000)) $ \previousMembership ->
    forAll (chooseInt (2, 10000)) $ \previousOrdinal ->
      let prior = mkAlignmentPlanAttemptAtMembership (controlIndex (fromIntegral previousMembership)) (fromIntegral previousOrdinal)
          successorMembership = controlIndex (fromIntegral previousMembership + 1)
          replacement = mkAlignmentPlanAttemptAtMembership successorMembership 1
          retry = nextAlignmentPlanAttempt replacement
          generation attempt = deriveContextClassGenerationId (checked "membership-scoped attempt cut" (alignmentCutAtAttempt attempt sortA occurrenceA topologyA placementVector members predecessors freshBases))
          priorEpochSameOrdinal = mkAlignmentPlanAttemptAtMembership (controlIndex (fromIntegral previousMembership)) 1
       in conjoin
            [ (prior < replacement) === True,
              (nextAlignmentPlanAttempt prior < replacement) === True,
              alignmentPlanAttemptMembershipChange retry === successorMembership,
              alignmentPlanAttemptWord64 retry === 2,
              (generation priorEpochSameOrdinal /= generation replacement) === True,
              mkAlignmentPlanAttempt (fromIntegral previousOrdinal) === mkAlignmentPlanAttemptAtMembership (controlIndex 0) (fromIntegral previousOrdinal)
            ]

caseCanonicalBytes :: IO ()
caseCanonicalBytes = do
  assertEqual
    "TopologyCutId hashes the exported topology transcript"
    (SHA256.hash (topologyCutCanonicalBytes topologyA))
    (topologyCutIdBytes (deriveTopologyCutId topologyA))
  assertEqual
    "ContextClassGenerationId is exactly the canonical AlignmentCut hash"
    (SHA256.hash (alignmentCutCanonicalBytes canonicalCut))
    (contextClassGenerationIdBytes (deriveContextClassGenerationId canonicalCut))

caseDigestDomains :: IO ()
caseDigestDomains = do
  let payload = "same canonical payload"
      digests =
        [ memberReadyEvidenceDigestBytes (deriveMemberReadyEvidenceDigest payload),
          historicalCertificateDigestBytes (deriveHistoricalCertificateDigest payload),
          alignmentSnapshotDigestBytes (deriveAlignmentSnapshotDigest payload),
          bootstrapEvidenceDigestBytes (deriveBootstrapEvidenceDigest payload),
          routeCutoverEvidenceDigestBytes (deriveRouteCutoverEvidenceDigest payload)
        ]
  assertEqual "five derivations produce five distinct byte strings" 5 (Set.size (Set.fromList digests))

caseGoldens :: IO ()
caseGoldens =
  assertEqual
    "changing a stable domain tag or canonical field layout changes a pinned vector"
    expectedGoldens
    actualGoldens

expectedGoldens :: [(String, String)]
expectedGoldens =
  [ ("generation", "b85404b336d5caa4e2ccd9f6e2559d79253ac1c6be3e802c285e0c225b5d8b88"),
    ("member-ready", "743d2ebda608ff95d640a005ff57b4c48c9c13d640f2fc9ce3e0085998508489"),
    ("historical-certificate", "31d5aa7a4be79a9a6abe3a759e1c7b991c29447597d53c52198dd97109890afc"),
    ("snapshot", "482157f9814cf4af00413318f3fcb0e471b2a8c53a653c034a9ab84e1346e99d"),
    ("bootstrap", "b1af331382d3c60934768d3903f16c7d048136bf68139c4ee0f3ec3be4c5e34f"),
    ("route-cutover", "cc59de195942d20717d087598b914a6ee92abbe40c8eda8e80f1fc5d5fc0eee4")
  ]

actualGoldens :: [(String, String)]
actualGoldens =
  [ ("generation", hex (contextClassGenerationIdBytes (deriveContextClassGenerationId canonicalCut))),
    ("member-ready", hex (memberReadyEvidenceDigestBytes (deriveMemberReadyEvidenceDigest goldenPayload))),
    ("historical-certificate", hex (historicalCertificateDigestBytes (deriveHistoricalCertificateDigest goldenPayload))),
    ("snapshot", hex (alignmentSnapshotDigestBytes (deriveAlignmentSnapshotDigest goldenPayload))),
    ("bootstrap", hex (bootstrapEvidenceDigestBytes (deriveBootstrapEvidenceDigest goldenPayload))),
    ("route-cutover", hex (routeCutoverEvidenceDigestBytes (deriveRouteCutoverEvidenceDigest goldenPayload)))
  ]

goldenPayload :: ByteString
goldenPayload = "canonical payload"

canonicalCut :: AlignmentCut
canonicalCut =
  checked
    "canonical alignment cut"
    ( alignmentCut
        sortA
        occurrenceA
        topologyA
        placementVector
        members
        predecessors
        freshBases
    )

members :: NonEmpty AlignmentMember
members = memberA :| [memberB]

changedMembers :: NonEmpty AlignmentMember
changedMembers = alignmentMember deltaA storeA heraldB :| [alignmentMember deltaB storeB heraldA]

predecessors :: [ContextClassGenerationId]
predecessors = [predecessorA, predecessorB]

freshBases :: [FreshMemberBaseEvidence]
freshBases = [freshA, freshB]

changedFreshA :: FreshMemberBaseEvidence
changedFreshA = freshMemberBaseEvidence deltaA storeA (nextStoreRevision initialStoreRevision)

memberA, memberB :: AlignmentMember
memberA = alignmentMember deltaA storeA heraldA
memberB = alignmentMember deltaB storeB heraldB

freshA, freshB :: FreshMemberBaseEvidence
freshA = freshMemberBaseEvidence deltaA storeA initialStoreRevision
freshB = freshMemberBaseEvidence deltaB storeB (nextStoreRevision initialStoreRevision)

placementVector, placementVectorB :: PhysicalPlacementRevisionVector
placementVector =
  checked
    "placement vector"
    ( physicalPlacementRevisionVector
        membershipGeneration
        [ (heraldB, nextPlacementRevision firstPlacementRevision),
          (heraldA, firstPlacementRevision)
        ]
    )
placementVectorB =
  checked
    "changed placement vector"
    ( physicalPlacementRevisionVector
        membershipGeneration
        [ (heraldA, firstPlacementRevision),
          (heraldB, nextPlacementRevision (nextPlacementRevision firstPlacementRevision))
        ]
    )

fixedMembership :: NonEmpty HeraldEpoch
fixedMembership = heraldA :| [heraldB]

membershipGeneration :: HeraldMembershipGeneration
membershipGeneration =
  checked
    "membership generation"
    (genesisHeraldMembershipGeneration systemId fixedMembership)

topologyA, topologyB :: TopologyCut
topologyA =
  checked
    "topology A"
    ( topologyCut
        (sameGenerationPredecessor topologyPredecessor)
        (topologyFrontier structuralVector controlPrerequisite)
        (deriveTopologyOccurrenceDigest "canonical topology occurrence")
    )
topologyB =
  checked
    "topology B"
    ( topologyCut
        (sameGenerationPredecessor topologyPredecessor)
        (topologyFrontier structuralVector controlPrerequisite)
        (deriveTopologyOccurrenceDigest "changed topology occurrence")
    )

structuralVector :: StructuralVersionVector
structuralVector = emptyStructuralVersionVector membershipGeneration

controlPrerequisite :: ControlIndex
controlPrerequisite = controlIndex 7

topologyPredecessor :: TopologyCutId
topologyPredecessor = identifier "topology predecessor" mkTopologyCutId 0x31

systemId, otherSystemId :: SystemId
systemId = identifier "system" mkSystemId 0x21
otherSystemId = identifier "other system" mkSystemId 0x22

sortA, sortB :: SortId
sortA = identifier "sort A" mkSortId 0x41
sortB = identifier "sort B" mkSortId 0x42

occurrenceA, occurrenceB :: SortDefinitionOccurrenceId
occurrenceA = identifier "occurrence A" mkSortDefinitionOccurrenceId 0x51
occurrenceB = identifier "occurrence B" mkSortDefinitionOccurrenceId 0x52

deltaA, deltaB :: DeltaId
deltaA = identifier "delta A" mkDeltaId 0x61
deltaB = identifier "delta B" mkDeltaId 0x62

storeA, storeB :: StoreIncarnationId
storeA = identifier "store A" mkStoreIncarnationId 0x71
storeB = identifier "store B" mkStoreIncarnationId 0x72

heraldA, heraldB :: HeraldEpoch
heraldA = identifier "herald A" mkHeraldEpoch 0x81
heraldB = identifier "herald B" mkHeraldEpoch 0x82

predecessorA, predecessorB :: ContextClassGenerationId
predecessorA = identifier "predecessor A" mkContextClassGenerationId 0x91
predecessorB = identifier "predecessor B" mkContextClassGenerationId 0x92

identifier ::
  (Show problem) =>
  String ->
  (ByteString -> Either problem value) ->
  Word ->
  value
identifier label constructor byte =
  checked
    label
    (constructor (ByteString.replicate 32 (fromIntegral byte)))

hex :: ByteString -> String
hex = foldMap byteHex . ByteString.unpack
  where
    byteHex byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
