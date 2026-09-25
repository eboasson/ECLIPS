module FoundationProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Word (Word16, Word8)
import Eclips.Domain.Graph
  ( EdgeStrength (..),
    VertexId (..),
    edgeDestination,
    edgePayload,
    edgeSource,
    edgeStrength,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    AuthorityEpochView (..),
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    IdentityError (..),
    NablaId,
    ProcessEpochId,
    PublicationId,
    StoreIncarnationId,
    TopologyCutId,
    authorityEpochCanonicalBytes,
    authorityEpochView,
    controlIndex,
    decodeAuthorityEpochCanonicalBytes,
    deltaIdBytes,
    deltaIdFromGlobalObjectId,
    genesisAuthorityEpoch,
    globalObjectIdBytes,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdBytes,
    globalUniqueIdFromGlobalObjectId,
    labelAuthorityEpoch,
    mkDeltaId,
    mkGlobalObjectId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    mkSortId,
    mkStoreIncarnationId,
    mkStructuralSequence,
    mkTopologyCutId,
    nablaIdBytes,
    nablaIdFromGlobalObjectId,
    nablaSequence,
    processEpochIdBytes,
    processEpochIdFromGlobalObjectId,
    publicationId,
    sortIdBytes,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Route
  ( ReplicaStrength (..),
    RouteDestination,
    RouteError (..),
    destinationDelta,
    freezeRoute,
    partitionFrozenRoute,
    routeDestination,
    routeDestinationCount,
    routeDestinations,
    routePartitionLocal,
    routePartitionRemote,
  )
import Eclips.Public.Types.SortId qualified as PublicSortId
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
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
    "domain foundations"
    [ testGroup
        "nominal identity"
        [ testProperty "only 32-byte controlled identities are admitted" propIdentityLength,
          testProperty "checked identity bytes round-trip" propIdentityRoundTrip,
          testProperty "only 32-byte global unique IDs are admitted" propGlobalUniqueIdLength,
          testProperty "checked global unique-ID bytes round-trip" propGlobalUniqueIdRoundTrip,
          testCase "fixed-width identities render as grouped lowercase hexadecimal" caseIdentityRendering,
          testProperty "domain SortId adapter preserves its checked API" propDomainSortIdAdapter,
          testCase "Domain re-exports the exact public SortId type" caseSharedSortIdType,
          testProperty "global unique/object conversion preserves exact bytes" propGlobalUniqueObjectBytes,
          testProperty "global unique/object conversion round-trips unique IDs" propGlobalUniqueObjectRoundTrip,
          testProperty "global object/unique conversion round-trips object IDs" propGlobalObjectUniqueRoundTrip,
          testProperty "process-epoch refinement preserves bytes and round-trips" propProcessEpochRefinement,
          testProperty "nabla refinement preserves bytes and round-trips" propNablaRefinement,
          testProperty "delta refinement preserves bytes and round-trips" propDeltaRefinement,
          testCase "identity refinements preserve non-uniform byte order" caseRefinementByteOrder,
          testCase "authority constructors have fixed canonical tags and payloads" caseAuthorityEncoding,
          testProperty "authority canonical bytes decode through all three arms" propAuthorityCanonicalRoundTrip,
          testCase "authority decoding rejects unknown tags and invalid structural sequence" caseAuthorityDecoderAdmission,
          testCase "authority constructor and label-tenure order is canonical" caseAuthorityOrder,
          testProperty "PublicationId uses the normative order" propPublicationIdOrder
        ],
      testGroup
        "universal graph edge"
        [ testCase "payload consists of its two endpoints and strength" caseEdgePayload
        ],
      testGroup
        "frozen route"
        [ testCase "the empty cut is valid" caseEmptyRoute,
          testCase "exact duplicate destinations collapse" caseDuplicateRoute,
          testCase "one delta cannot name conflicting destinations" caseConflictingRoute,
          testCase "partition is local-or-one-nonempty-peer-cut" caseRoutePartition,
          testProperty "input order does not change a valid cut" propRoutePermutation
        ]
    ]

propIdentityLength :: Property
propIdentityLength =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let result = mkGlobalObjectId (ByteString.replicate byteCount 0)
     in counterexample ("byte count: " <> show byteCount)
          $ isLeft result === (byteCount /= 32)

propIdentityRoundTrip :: Word8 -> Property
propIdentityRoundTrip byte =
  let bytes = ByteString.replicate 32 byte
   in globalObjectIdBytes (globalObject byte) === bytes

propGlobalUniqueIdLength :: Property
propGlobalUniqueIdLength =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let result = mkGlobalUniqueId (ByteString.replicate byteCount 0)
     in counterexample ("byte count: " <> show byteCount)
          $ isLeft result === (byteCount /= 32)

propGlobalUniqueIdRoundTrip :: Word8 -> Property
propGlobalUniqueIdRoundTrip byte =
  let bytes = ByteString.replicate 32 byte
   in globalUniqueIdBytes (globalUniqueId byte) === bytes

caseIdentityRendering :: IO ()
caseIdentityRendering =
  assertEqual
    "global object ID"
    "00010203:04050607:08090a0b:0c0d0e0f:10111213:14151617:18191a1b:1c1d1e1f"
    (show identifier)
  where
    identifier =
      checked
        "GlobalObjectId"
        (mkGlobalObjectId (ByteString.pack [0 .. 31]))

propDomainSortIdAdapter :: Property
propDomainSortIdAdapter =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let bytes = ByteString.pack (take byteCount (cycle [0 .. 31]))
     in if byteCount == 32
          then (sortIdBytes <$> mkSortId bytes) === Right bytes
          else
            mkSortId bytes
              === Left
                WrongIdentityByteCount
                  { expectedIdentityByteCount = 32,
                    actualIdentityByteCount = byteCount
                  }

caseSharedSortIdType :: IO ()
caseSharedSortIdType = do
  let bytes = ByteString.pack [0 .. 31]
  case (mkSortId bytes, PublicSortId.mkSortId bytes) of
    (Right domainIdentity, Right publicIdentity) ->
      assertEqual "shared SortId" publicIdentity domainIdentity
    (domainResult, publicResult) ->
      assertFailure
        ( "valid shared SortId rejected: "
            <> show domainResult
            <> ", "
            <> show publicResult
        )

propGlobalUniqueObjectBytes :: Word8 -> Property
propGlobalUniqueObjectBytes byte =
  globalObjectIdBytes (globalObjectIdFromGlobalUniqueId identifier)
    === globalUniqueIdBytes identifier
  where
    identifier = globalUniqueId byte

propGlobalUniqueObjectRoundTrip :: Word8 -> Property
propGlobalUniqueObjectRoundTrip byte =
  globalUniqueIdFromGlobalObjectId
    (globalObjectIdFromGlobalUniqueId identifier)
    === identifier
  where
    identifier = globalUniqueId byte

propGlobalObjectUniqueRoundTrip :: Word8 -> Property
propGlobalObjectUniqueRoundTrip byte =
  globalObjectIdFromGlobalUniqueId
    (globalUniqueIdFromGlobalObjectId identifier)
    === identifier
  where
    identifier = globalObject byte

propProcessEpochRefinement :: Word8 -> Property
propProcessEpochRefinement byte =
  conjoin
    [ processEpochIdBytes refined === globalObjectIdBytes object,
      globalObjectIdFromProcessEpochId refined === object,
      processEpochIdFromGlobalObjectId
        (globalObjectIdFromProcessEpochId process)
        === process
    ]
  where
    object = globalObject byte
    refined = processEpochIdFromGlobalObjectId object
    process = processEpoch byte

propNablaRefinement :: Word8 -> Property
propNablaRefinement byte =
  conjoin
    [ nablaIdBytes refined === globalObjectIdBytes object,
      globalObjectIdFromNablaId refined === object,
      nablaIdFromGlobalObjectId (globalObjectIdFromNablaId identityNabla)
        === identityNabla
    ]
  where
    object = globalObject byte
    refined = nablaIdFromGlobalObjectId object
    identityNabla = nabla byte

propDeltaRefinement :: Word8 -> Property
propDeltaRefinement byte =
  conjoin
    [ deltaIdBytes refined === globalObjectIdBytes object,
      globalObjectIdFromDeltaId refined === object,
      deltaIdFromGlobalObjectId (globalObjectIdFromDeltaId identityDelta)
        === identityDelta
    ]
  where
    object = globalObject byte
    refined = deltaIdFromGlobalObjectId object
    identityDelta = delta byte

caseRefinementByteOrder :: IO ()
caseRefinementByteOrder = do
  let bytes = ByteString.pack [0 .. 31]
      globalUnique = checked "GlobalUniqueId" (mkGlobalUniqueId bytes)
      objectIdentity = globalObjectIdFromGlobalUniqueId globalUnique
      process = processEpochIdFromGlobalObjectId objectIdentity
      identityNabla = nablaIdFromGlobalObjectId objectIdentity
      identityDelta = deltaIdFromGlobalObjectId objectIdentity
  assertEqual "global-object refinement" bytes (globalObjectIdBytes objectIdentity)
  assertEqual "process-epoch refinement" bytes (processEpochIdBytes process)
  assertEqual "nabla refinement" bytes (nablaIdBytes identityNabla)
  assertEqual "delta refinement" bytes (deltaIdBytes identityDelta)

propPublicationIdOrder ::
  Word8 ->
  Word16 ->
  Word8 ->
  Word16 ->
  Word8 ->
  Word16 ->
  Word8 ->
  Word16 ->
  Property
propPublicationIdOrder leftNabla leftAuthority leftHerald leftSequence rightNabla rightAuthority rightHerald rightSequence =
  compare left right === compare leftTuple rightTuple
  where
    left =
      publication
        leftNabla
        leftAuthority
        leftHerald
        leftSequence
    right =
      publication
        rightNabla
        rightAuthority
        rightHerald
        rightSequence
    leftTuple =
      ( nabla leftNabla,
        authority leftAuthority,
        nablaSequence (fromIntegral leftSequence),
        herald leftHerald
      )
    rightTuple =
      ( nabla rightNabla,
        authority rightAuthority,
        nablaSequence (fromIntegral rightSequence),
        herald rightHerald
      )

caseAuthorityEncoding :: IO ()
caseAuthorityEncoding = do
  let occurrence =
        structuralOccurrenceId
          (herald 7)
          (checked "StructuralSequence" (mkStructuralSequence 3))
      cut = topologyCut 9
      structural = structuralAuthorityEpoch occurrence cut
      label = labelAuthorityEpoch (controlIndex 4)
  assertEqual "genesis tag" (ByteString.singleton 0) (authorityEpochCanonicalBytes genesisAuthorityEpoch)
  assertEqual
    "structural tag/source/sequence/cut"
    ( ByteString.singleton 1
        <> identifierBytes 7
        <> ByteString.pack [0, 0, 0, 0, 0, 0, 0, 3]
        <> identifierBytes 9
    )
    (authorityEpochCanonicalBytes structural)
  assertEqual
    "label tag/control index"
    ( ByteString.singleton 2
        <> ByteString.pack [0, 0, 0, 0, 0, 0, 0, 4]
    )
    (authorityEpochCanonicalBytes label)
  assertEqual
    "structural view"
    (StructuralAuthorityEpochView occurrence cut)
    (authorityEpochView structural)
  assertEqual
    "label view"
    (LabelAuthorityEpochView (controlIndex 4))
    (authorityEpochView label)

propAuthorityCanonicalRoundTrip :: Word16 -> Property
propAuthorityCanonicalRoundTrip seed =
  let expected = authority seed
   in decodeAuthorityEpochCanonicalBytes (authorityEpochCanonicalBytes expected)
        === Right expected

caseAuthorityDecoderAdmission :: IO ()
caseAuthorityDecoderAdmission = do
  let occurrence =
        structuralOccurrenceId
          (herald 7)
          (checked "StructuralSequence" (mkStructuralSequence 3))
      canonical =
        authorityEpochCanonicalBytes
          (structuralAuthorityEpoch occurrence (topologyCut 9))
      zeroSequence =
        ByteString.take 33 canonical
          <> ByteString.replicate 8 0
          <> ByteString.drop 41 canonical
  assertBool
    "the closed tag grammar rejects an unknown arm"
    (isLeft (decodeAuthorityEpochCanonicalBytes (ByteString.singleton 3)))
  assertBool
    "structural sequence admission remains positive"
    (isLeft (decodeAuthorityEpochCanonicalBytes zeroSequence))

caseAuthorityOrder :: IO ()
caseAuthorityOrder = do
  let occurrence =
        structuralOccurrenceId
          (herald 1)
          (checked "StructuralSequence" (mkStructuralSequence 1))
      structural = structuralAuthorityEpoch occurrence (topologyCut 1)
      labelAt2 = labelAuthorityEpoch (controlIndex 2)
      labelAt3 = labelAuthorityEpoch (controlIndex 3)
  assertEqual "genesis view" GenesisAuthorityEpochView (authorityEpochView genesisAuthorityEpoch)
  assertEqual "genesis before structural" LT (compare genesisAuthorityEpoch structural)
  assertEqual "structural before label" LT (compare structural labelAt2)
  assertEqual "label tenures follow control order" LT (compare labelAt2 labelAt3)

caseEdgePayload :: IO ()
caseEdgePayload = do
  let source = NablaVertex (nabla 1)
      destination = DeltaVertex (delta 2)
      preserving = edgePayload source destination Preserve
      weakening = edgePayload source destination Weaken
  assertEqual "source" source (edgeSource preserving)
  assertEqual "destination" destination (edgeDestination preserving)
  assertEqual "preserving strength" Preserve (edgeStrength preserving)
  assertEqual "weakening strength" Weaken (edgeStrength weakening)

caseEmptyRoute :: IO ()
caseEmptyRoute = case freezeRoute [] of
  Left problem -> assertFailure ("empty route rejected: " <> show problem)
  Right route -> do
    assertEqual "destination count" 0 (routeDestinationCount route)
    assertEqual "destinations" [] (routeDestinations route)

caseDuplicateRoute :: IO ()
caseDuplicateRoute = case freezeRoute [destination, destination] of
  Left problem -> assertFailure ("duplicate rejected: " <> show problem)
  Right route -> do
    assertEqual "destination count" 1 (routeDestinationCount route)
    assertEqual "destination" [destination] (routeDestinations route)
  where
    destination = routeFixture 1 2 3 Normal

caseConflictingRoute :: IO ()
caseConflictingRoute =
  case freezeRoute [first, second] of
    Left (ConflictingRouteDestination deltaId _ _) ->
      assertEqual "conflicting delta" (destinationDelta first) deltaId
    Right _ -> assertFailure "conflicting destinations were accepted"
  where
    first = routeFixture 1 2 3 Normal
    second = routeFixture 1 2 4 Normal

caseRoutePartition :: IO ()
caseRoutePartition = case freezeRoute destinations of
  Left problem -> assertFailure ("route rejected: " <> show problem)
  Right route -> do
    let partition = partitionFrozenRoute (herald 7) route
    assertEqual
      "local destinations"
      [routeFixture 1 11 7 Normal]
      (routeDestinations (routePartitionLocal partition))
    assertEqual
      "peer cuts"
      [ ( herald 8,
          [routeFixture 2 12 8 Weak, routeFixture 3 13 8 Normal]
        ),
        (herald 9, [routeFixture 4 14 9 Normal])
      ]
      ( fmap
          (fmap NonEmpty.toList)
          (Map.toAscList (routePartitionRemote partition))
      )
  where
    destinations =
      [ routeFixture 4 14 9 Normal,
        routeFixture 2 12 8 Weak,
        routeFixture 1 11 7 Normal,
        routeFixture 3 13 8 Normal
      ]

propRoutePermutation :: Property
propRoutePermutation =
  forAll (shuffle destinations) $ \permutation ->
    freezeRoute permutation === freezeRoute destinations
  where
    destinations =
      [ routeFixture 3 13 23 Weak,
        routeFixture 1 11 21 Normal,
        routeFixture 2 12 22 Normal
      ]

publication :: Word8 -> Word16 -> Word8 -> Word16 -> PublicationId
publication nablaByte authorityNumber heraldByte sequenceNumber =
  publicationId
    (nabla nablaByte)
    (authority authorityNumber)
    (herald heraldByte)
    (nablaSequence (fromIntegral sequenceNumber))

authority :: Word16 -> AuthorityEpoch
authority value
  | value `mod` 3 == 0 = genesisAuthorityEpoch
  | value `mod` 3 == 1 =
      structuralAuthorityEpoch
        ( structuralOccurrenceId
            (herald (fromIntegral value))
            (checked "StructuralSequence" (mkStructuralSequence (1 + fromIntegral value)))
        )
        (topologyCut (fromIntegral value))
  | otherwise = labelAuthorityEpoch (controlIndex (fromIntegral value))

topologyCut :: Word8 -> TopologyCutId
topologyCut byte =
  checked "TopologyCutId" (mkTopologyCutId (identifierBytes byte))

routeFixture :: Word8 -> Word8 -> Word8 -> ReplicaStrength -> RouteDestination
routeFixture deltaByte incarnationByte heraldByte =
  routeDestination
    (delta deltaByte)
    (storeIncarnation incarnationByte)
    (herald heraldByte)

globalObject :: Word8 -> GlobalObjectId
globalObject byte = checked "GlobalObjectId" (mkGlobalObjectId (identifierBytes byte))

globalUniqueId :: Word8 -> GlobalUniqueId
globalUniqueId byte =
  checked "GlobalUniqueId" (mkGlobalUniqueId (identifierBytes byte))

processEpoch :: Word8 -> ProcessEpochId
processEpoch byte =
  checked "ProcessEpochId" (mkProcessEpochId (identifierBytes byte))

nabla :: Word8 -> NablaId
nabla byte = checked "NablaId" (mkNablaId (identifierBytes byte))

delta :: Word8 -> DeltaId
delta byte = checked "DeltaId" (mkDeltaId (identifierBytes byte))

herald :: Word8 -> HeraldEpoch
herald byte = checked "HeraldEpoch" (mkHeraldEpoch (identifierBytes byte))

storeIncarnation :: Word8 -> StoreIncarnationId
storeIncarnation byte =
  checked
    "StoreIncarnationId"
    (mkStoreIncarnationId (identifierBytes byte))

identifierBytes :: Word8 -> ByteString.ByteString
identifierBytes = ByteString.replicate 32

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
