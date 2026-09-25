{-# LANGUAGE OverloadedStrings #-}

module BinaryProperties
  ( tests,
  )
where

import Data.Binary (Binary, decodeOrFail, encode, put)
import Data.Binary.Put (putWord8, runPut)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (..),
    ApplicationStartupAccess,
    EnvironmentAccess,
    PredefinedAccess,
    allApplicationPredefinedSortRoles,
    applicationStartupAccess,
    environmentAccess,
    predefinedAccess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Forward (ForwardResult (..))
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    PrivateNablaId,
    PrivateObjectId,
    PrivateProcessId,
    PrivateUniqueId,
    SortId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    mkPrivateUniqueId,
    mkSortId,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (..),
    LabelResult (..),
  )
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryLiteral (..),
    ApplicationQueryPredicate (..),
  )
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Application.Types.Result
  ( OperationPendingReason (..),
    RegularCallResult (..),
    WaitResult (WaitReady),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (..),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (..),
    ApplicationScalarLiteral (..),
    ApplicationSortDefinition (..),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (..),
    ApplicationValueSchema (..),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabel,
    ApplicationLabelOwner (..),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    Positive (getPositive),
    Property,
    arbitrary,
    conjoin,
    elements,
    forAll,
    frequency,
    listOf,
    oneof,
    resize,
    shuffle,
    sized,
    testProperty,
    vectorOf,
    (===),
    (==>),
  )

tests :: TestTree
tests =
  testGroup
    "current-build Binary instances"
    [ testProperty "generated application values round-trip" (forAll genApplicationValue roundTrip),
      testProperty "generated descriptor drafts round-trip" (forAll genSortDefinition roundTrip),
      testProperty "an empty descriptor key list round-trips" (roundTrip emptyKeyDefinition),
      testProperty "generated application queries round-trip" (forAll genQuery roundTrip),
      testProperty "generated newid targets round-trip" (forAll genNewIdTarget roundTrip),
      testProperty "generated current write values round-trip" (forAll genWriteValue roundTrip),
      testProperty "generated operations round-trip" (forAll genOperation roundTrip),
      testCase "label-operation Binary preserves the expected-label CAS operand" caseLabelCasOperand,
      testProperty "generation-only differences change sample, query, and expected CAS bytes" propLabelGenerationBytes,
      testProperty "structural stabilization pending round-trips" (roundTrip StructuralStabilizationPending),
      testProperty "label settlement pending round-trips" (roundTrip LabelSettlementPending),
      testProperty "environment stabilization pending round-trips" (roundTrip EnvironmentStabilizationPending),
      testProperty "generated regular results round-trip" (forAll genRegularCallResult roundTrip),
      testProperty "generated semantic rejections round-trip" (forAll genRejection roundTrip),
      testProperty "generated canonical startup access round-trips" (forAll genStartupAccess roundTrip),
      testProperty "generated canonical environment access round-trips" (forAll genEnvironmentAccess roundTrip),
      testCase "new shared unions cover every current constructor" caseSharedUnionCoverage,
      testCase "the appended live constructors have fixed Binary tags" caseLiveConstructorTags,
      testCase "the new closed sums and nested identities decode through checked boundaries" caseLiveDecoderAdmission,
      testCase "private-ID decoders reject serialized zero in every nominal role" casePrivateIdsRejectZero,
      testCase "the selected Text decoder rejects invalid UTF-8" caseInvalidUtf8,
      testCase "checked access decoders reject every non-canonical catalogue shape" caseAccessRejectsNonCanonicalRoles
    ]

roundTrip :: (Binary value, Eq value, Show value) => value -> Property
roundTrip value = decodeBinary (encode value) === Right value

emptyKeyDefinition :: ApplicationSortDefinition
emptyKeyDefinition =
  DeclaredSortDefinition
    ApplicationSortDescriptor
      { sortKind = RegularSort,
        valueSchema = TextSchema,
        keyProjections = [],
        validityPredicate = AlwaysPredicate,
        obsolescencePredicate = NeverPredicate,
        rankTerms = RankApplicationValue Ascending :| [],
        minimumRetentionMicros = 0,
        isImmutable = False,
        labelField = Nothing
      }
    Nothing

decodeBinary :: (Binary value) => LazyByteString.ByteString -> Either String value
decodeBinary bytes =
  case decodeOrFail bytes of
    Left (_, _, problem) -> Left problem
    Right (remaining, _, value)
      | LazyByteString.null remaining -> Right value
      | otherwise -> Left "unexpected trailing bytes"

genPrivateUniqueId :: Gen PrivateUniqueId
genPrivateUniqueId = do
  value <- getPositive <$> (arbitrary :: Gen (Positive Word64))
  pure (checked "generated private ID" (mkPrivateUniqueId value))

genPrivateNablaId :: Gen PrivateNablaId
genPrivateNablaId = asPrivateNablaId <$> genPrivateUniqueId

genPrivateDeltaId :: Gen PrivateDeltaId
genPrivateDeltaId = asPrivateDeltaId <$> genPrivateUniqueId

genPrivateProcessId :: Gen PrivateProcessId
genPrivateProcessId = asPrivateProcessId <$> genPrivateUniqueId

genPrivateObjectId :: Gen PrivateObjectId
genPrivateObjectId = asPrivateObjectId <$> genPrivateUniqueId

genSortId :: Gen SortId
genSortId =
  checked "generated SortId" . mkSortId . ByteString.pack <$> vectorOf 32 arbitrary

genText :: Gen Text
genText = Text.pack <$> resize 8 (listOf (elements ['a' .. 'z']))

genProjection :: Gen ApplicationProjection
genProjection =
  ApplicationProjection <$> ((:|) <$> genText <*> resize 3 (listOf genText))

genSchema :: Gen ApplicationValueSchema
genSchema = sized generate
  where
    generate remaining
      | remaining <= 0 = scalar
      | otherwise =
          frequency
            [ (7, scalar),
              (1, RecordSchema . Map.fromList <$> resize 3 (listOf ((,) <$> genText <*> generate (remaining `div` 2))))
            ]
    scalar =
      oneof
        [ pure BoolSchema,
          pure Int64Schema,
          pure BytesSchema,
          pure TextSchema,
          pure UniqueIdSchema,
          pure LabelSchema,
          EnumSchema <$> ((:|) <$> genText <*> resize 3 (listOf genText)),
          pure OptionalUniqueIdSchema
        ]

genScalarLiteral :: Gen ApplicationScalarLiteral
genScalarLiteral =
  oneof
    [ LiteralBool <$> arbitrary,
      LiteralInt64 <$> (arbitrary :: Gen Int64),
      LiteralBytes . ByteString.pack <$> resize 8 (listOf arbitrary),
      LiteralText <$> genText,
      LiteralEnum <$> genText
    ]

genDescriptorPredicate :: Gen ApplicationPredicateExpression
genDescriptorPredicate = sized generate
  where
    generate remaining
      | remaining <= 0 = leaf
      | otherwise =
          frequency
            [ (6, leaf),
              (1, NotPredicate <$> generate (remaining - 1)),
              (1, AllPredicates <$> children (remaining `div` 2)),
              (1, AnyPredicates <$> children (remaining `div` 2))
            ]
    children remaining =
      (:|)
        <$> generate remaining
        <*> resize 3 (listOf (generate remaining))
    leaf =
      oneof
        [ pure AlwaysPredicate,
          pure NeverPredicate,
          CompareField
            <$> genProjection
            <*> elements [minBound .. maxBound]
            <*> genScalarLiteral
        ]

genRankTerm :: Gen ApplicationRankTerm
genRankTerm =
  oneof
    [ RankField <$> genProjection <*> elements [minBound .. maxBound],
      RankApplicationValue <$> elements [minBound .. maxBound]
    ]

genSortDescriptor :: Gen ApplicationSortDescriptor
genSortDescriptor =
  ApplicationSortDescriptor
    <$> elements [RegularSort, ControlledSort]
    <*> resize 4 genSchema
    <*> resize 3 (listOf genProjection)
    <*> resize 4 genDescriptorPredicate
    <*> resize 4 genDescriptorPredicate
    <*> ((:|) <$> genRankTerm <*> resize 3 (listOf genRankTerm))
    <*> arbitrary
    <*> arbitrary
    <*> oneof [pure Nothing, Just <$> genText]

genSortDefinition :: Gen ApplicationSortDefinition
genSortDefinition =
  oneof
    [ DeclaredSortDefinition
        <$> genSortDescriptor
        <*> oneof [pure Nothing, Just <$> genSortId],
      PredefinedSortDefinition
        <$> elements allApplicationPredefinedSortRoles
        <*> genSortId
    ]

genLabel :: Gen ApplicationLabel
genLabel =
  (,)
    <$> oneof
      [ pure VoidLabel,
        ProcessLabel <$> genPrivateProcessId,
        ZombieLabel <$> genPrivateProcessId
      ]
    <*> arbitrary

genApplicationValue :: Gen ApplicationValue
genApplicationValue = sized generate
  where
    generate remaining
      | remaining <= 0 = scalar
      | otherwise =
          frequency
            [ (7, scalar),
              (2, RecordValue . Map.fromList <$> resize 3 (listOf ((,) <$> genText <*> generate (remaining `div` 2))))
            ]
    scalar =
      oneof
        [ BoolValue <$> arbitrary,
          Int64Value <$> (arbitrary :: Gen Int64),
          BytesValue . ByteString.pack <$> resize 8 (listOf arbitrary),
          TextValue <$> genText,
          UniqueIdValue <$> genPrivateUniqueId,
          LabelValue <$> genLabel,
          SortDefinitionValue <$> resize 3 genSortDefinition,
          EnumValue <$> genText,
          OptionalUniqueIdValue
            <$> oneof [pure Nothing, Just <$> genPrivateUniqueId]
        ]

genQueryLiteral :: Gen ApplicationQueryLiteral
genQueryLiteral =
  oneof
    [ QueryBool <$> arbitrary,
      QueryInt64 <$> (arbitrary :: Gen Int64),
      QueryBytes . ByteString.pack <$> resize 8 (listOf arbitrary),
      QueryText <$> genText,
      QueryUniqueId <$> genPrivateUniqueId,
      QueryLabel <$> genLabel,
      QueryEnum <$> genText
    ]

genQueryPredicate :: Gen ApplicationQueryPredicate
genQueryPredicate = sized generate
  where
    generate remaining
      | remaining <= 0 = leaf
      | otherwise =
          frequency
            [ (6, leaf),
              (1, QueryNot <$> generate (remaining - 1)),
              (1, QueryAll <$> children (remaining `div` 2)),
              (1, QueryAny <$> children (remaining `div` 2))
            ]
    children remaining =
      (:|)
        <$> generate remaining
        <*> resize 3 (listOf (generate remaining))
    leaf =
      oneof
        [ pure QueryAlways,
          pure QueryNever,
          QueryCompare
            <$> genProjection
            <*> elements [minBound .. maxBound]
            <*> genQueryLiteral
        ]

genQuery :: Gen ApplicationQuery
genQuery =
  ApplicationQuery
    . Set.fromList
    <$> resize 4 (listOf genPrivateDeltaId)
    <*> resize 5 genQueryPredicate

genOperation :: Gen ApplicationOperation
genOperation =
  oneof
    [ NewIdApplication <$> genNewIdTarget,
      WriteApplication <$> genPrivateNablaId <*> genWriteValue,
      ForwardApplication <$> genPrivateNablaId <*> genPrivateObjectId,
      ReadApplication <$> genQuery,
      LocalTakeApplication <$> genQuery,
      WaitApplication <$> resize 4 (listOf genQuery),
      LabelApplication <$> genPrivateObjectId <*> genLabel <*> genLabelTarget,
      pure NewEnvironmentApplication
    ]

genLabelTarget :: Gen ApplicationLabelTarget
genLabelTarget =
  oneof
    [ LabelToProcess <$> genPrivateProcessId,
      pure LabelToVoid,
      pure LabelToDelete
    ]

genNewIdTarget :: Gen NewIdTarget
genNewIdTarget =
  oneof
    [ pure BareNewId,
      ControlledNewId <$> genPrivateNablaId
    ]

genWriteValue :: Gen ApplicationWriteValue
genWriteValue =
  oneof
    [ PublishValue <$> resize 4 genApplicationValue,
      DeleteReserved <$> genPrivateObjectId
    ]

genRegularCallResult :: Gen RegularCallResult
genRegularCallResult =
  oneof
    [ NewIdCompleted <$> genPrivateUniqueId,
      WriteCompleted . SortDefinitionWritten <$> genSortId,
      pure (WriteCompleted WriteAccepted),
      pure (ForwardCompleted ForwardAccepted),
      ReadCompleted <$> resize 4 (listOf genApplicationValue),
      LocalTakeCompleted <$> resize 4 (listOf genApplicationValue),
      pure (WaitCompleted WaitReady),
      LabelCompleted <$> elements [LabelApplied, LabelNotApplied],
      NewEnvironmentCompleted <$> genEnvironmentAccess
    ]

genRejection :: Gen ApplicationRejection
genRejection =
  oneof
    [ ApplicationUnknownPrivateIdentity <$> genPrivateUniqueId,
      ApplicationInvalidFieldName <$> genText,
      ApplicationInvalidQueryProjection <$> genProjection,
      ApplicationNablaRoleMismatch <$> genPrivateNablaId,
      ApplicationDeltaRoleMismatch <$> genPrivateDeltaId,
      ApplicationProcessRoleMismatch <$> genPrivateProcessId,
      ApplicationDeltaNotLocal <$> genPrivateDeltaId,
      ApplicationDeltaStoreUnavailable <$> genPrivateDeltaId,
      pure ApplicationOperateNotPermitted,
      pure ApplicationQueryPredicateMismatch,
      ApplicationSortDefinitionProjectionUnsupported <$> genProjection,
      pure ApplicationSortDescriptorRejected,
      ApplicationSortIdClaimMismatch <$> genSortId <*> genSortId,
      ApplicationNotCurrentSortDefinitionWriter <$> genPrivateNablaId,
      ApplicationNablaSortNotControlled <$> genPrivateNablaId,
      ApplicationReservationUnavailable <$> genPrivateNablaId <*> genPrivateObjectId,
      ApplicationObjectNotLabelable <$> genPrivateObjectId,
      ApplicationLabelTargetNotNameable <$> genPrivateProcessId,
      pure EnvironmentSourcesUnavailable,
      pure ApplicationLabelTransitionNotPermitted
    ]

genStartupAccess :: Gen ApplicationStartupAccess
genStartupAccess = do
  process <- genPrivateProcessId
  identities <- listOf genPrivateUniqueId
  writerSort <- genSortId
  readerSort <- genSortId
  let writer = Access.Writer (asPrivateNablaId (checked "startup writer" (mkPrivateUniqueId 1)))
      reader = Access.Reader (asPrivateDeltaId (checked "startup reader" (mkPrivateUniqueId 2)))
      entries =
        [(Text.pack (show index), Access.Identity identity) | (index, identity) <- zip [0 :: Int ..] identities]
          <> [("writer", writer), ("writer-alias", writer), ("reader", reader)]
      sorts = Map.fromList [("writer", writerSort), ("writer-alias", writerSort), ("reader", readerSort)]
      access = checked "generated selected access" (Access.primordialAccessWithEndpointSorts entries Set.empty Nothing sorts)
  pure (applicationStartupAccess process access)

genEnvironmentAccess :: Gen EnvironmentAccess
genEnvironmentAccess = do
  identities <- shuffle [100 .. 130]
  let private = checked "generated environment identity" . mkPrivateUniqueId
      pairs = zipWith (\role (writer, reader) -> predefinedAccess role (asPrivateNablaId (private writer)) (asPrivateDeltaId (private reader))) allApplicationPredefinedSortRoles (zip (take 6 identities) (take 6 (drop 6 identities)))
  pure (checked "generated environment access" (environmentAccess pairs (asPrivateObjectId (private (identities !! 12))) (fmap (asPrivateObjectId . private) (drop 13 identities))))

caseSharedUnionCoverage :: IO ()
caseSharedUnionCoverage = do
  let identifier = checked "private ID fixture" (mkPrivateUniqueId 7)
      process = asPrivateProcessId identifier
      nabla = asPrivateNablaId identifier
      object = asPrivateObjectId identifier
      delta = asPrivateDeltaId identifier
      sortId = checked "SortId fixture" (mkSortId (ByteString.pack [0 .. 31]))
      projection = ApplicationProjection ("field" :| [])
      query = ApplicationQuery Set.empty QueryAlways
      definition = PredefinedSortDefinition SortDefinitionRole sortId
      environment =
        checked
          "environment access fixture"
          ( environmentAccess
              [uniquePair role | role <- allApplicationPredefinedSortRoles]
              fixtureHub
              fixtureEdges
          )
      operations =
        [ NewIdApplication BareNewId,
          NewIdApplication (ControlledNewId nabla),
          WriteApplication nabla (PublishValue (SortDefinitionValue definition)),
          WriteApplication nabla (DeleteReserved object),
          ForwardApplication nabla object,
          ReadApplication query,
          LocalTakeApplication query,
          WaitApplication [query],
          LabelApplication object (VoidLabel, 0) (LabelToProcess process),
          LabelApplication object (ProcessLabel process, 0) LabelToVoid,
          LabelApplication object (ZombieLabel process, 0) LabelToDelete,
          NewEnvironmentApplication
        ]
      rejections =
        [ ApplicationUnknownPrivateIdentity identifier,
          ApplicationInvalidFieldName "field",
          ApplicationInvalidQueryProjection projection,
          ApplicationNablaRoleMismatch nabla,
          ApplicationDeltaRoleMismatch delta,
          ApplicationProcessRoleMismatch process,
          ApplicationDeltaNotLocal delta,
          ApplicationDeltaStoreUnavailable delta,
          ApplicationOperateNotPermitted,
          ApplicationQueryPredicateMismatch,
          ApplicationSortDefinitionProjectionUnsupported projection,
          ApplicationSortDescriptorRejected,
          ApplicationSortIdClaimMismatch sortId sortId,
          ApplicationNotCurrentSortDefinitionWriter nabla,
          ApplicationNablaSortNotControlled nabla,
          ApplicationReservationUnavailable nabla object,
          ApplicationObjectNotLabelable object,
          ApplicationLabelTargetNotNameable process,
          ApplicationLabelTransitionNotPermitted,
          EnvironmentSourcesUnavailable
        ]
      results =
        [ NewIdCompleted identifier,
          WriteCompleted (SortDefinitionWritten sortId),
          WriteCompleted WriteAccepted,
          ForwardCompleted ForwardAccepted,
          ReadCompleted [BoolValue True],
          LocalTakeCompleted [BoolValue False],
          WaitCompleted WaitReady,
          LabelCompleted LabelApplied,
          LabelCompleted LabelNotApplied,
          NewEnvironmentCompleted environment
        ]
  assertBool
    "operation fixture exhaustiveness"
    (fmap operationConstructorTag operations == [0, 0, 1, 1, 2, 3, 4, 5, 6, 6, 6, 7])
  assertBool
    "newid target fixture exhaustiveness"
    (fmap newIdTargetConstructorTag [BareNewId, ControlledNewId nabla] == [0, 1])
  assertBool
    "write payload fixture exhaustiveness"
    ( fmap
        applicationWriteValueConstructorTag
        [PublishValue (SortDefinitionValue definition), DeleteReserved object]
        == [0, 1]
    )
  assertBool
    "regular result fixture exhaustiveness"
    (fmap regularResultConstructorTag results == [0, 1, 1, 2, 3, 4, 5, 6, 6, 7])
  assertBool
    "write result fixture exhaustiveness"
    (fmap writeResultConstructorTag [SortDefinitionWritten sortId, WriteAccepted] == [0, 1])
  traverse_ assertRoundTrip operations
  traverse_ assertRoundTrip rejections
  traverse_ assertRoundTrip results

caseLiveConstructorTags :: IO ()
caseLiveConstructorTags = do
  let identifier = checked "private ID fixture" (mkPrivateUniqueId 7)
      process = asPrivateProcessId identifier
      nabla = asPrivateNablaId identifier
      object = asPrivateObjectId identifier
      query = ApplicationQuery Set.empty QueryAlways
      environment =
        checked
          "environment access fixture"
          ( environmentAccess
              [uniquePair role | role <- allApplicationPredefinedSortRoles]
              fixtureHub
              fixtureEdges
          )
  assertEqual
    "operation tags"
    [0 .. 7]
    ( fmap
        binaryTag
        [ NewIdApplication BareNewId,
          WriteApplication nabla (DeleteReserved object),
          ForwardApplication nabla object,
          ReadApplication query,
          LocalTakeApplication query,
          WaitApplication [],
          LabelApplication object (VoidLabel, 0) LabelToVoid,
          NewEnvironmentApplication
        ]
    )

  assertEqual
    "regular-result tags"
    [0 .. 7]
    ( fmap
        binaryTag
        [ NewIdCompleted identifier,
          WriteCompleted WriteAccepted,
          ForwardCompleted ForwardAccepted,
          ReadCompleted [],
          LocalTakeCompleted [],
          WaitCompleted WaitReady,
          LabelCompleted LabelApplied,
          NewEnvironmentCompleted environment
        ]
    )
  assertEqual
    "pending tags"
    [0, 1, 2]
    ( fmap
        binaryTag
        [ StructuralStabilizationPending,
          LabelSettlementPending,
          EnvironmentStabilizationPending
        ]
    )
  assertEqual
    "label-target tags"
    [0, 1, 2]
    (fmap binaryTag [LabelToProcess process, LabelToVoid, LabelToDelete])
  assertEqual
    "label-result tags"
    [0, 1]
    (fmap binaryTag [LabelApplied, LabelNotApplied])
  assertEqual
    "label-rejection tags"
    [16, 17, 18]
    ( fmap
        binaryTag
        [ ApplicationObjectNotLabelable object,
          ApplicationLabelTargetNotNameable process,
          ApplicationLabelTransitionNotPermitted
        ]
    )
  assertEqual "optional schema tag" 8 (binaryTag OptionalUniqueIdSchema)
  assertEqual
    "optional value tag"
    9
    (binaryTag (OptionalUniqueIdValue Nothing))

caseLabelCasOperand :: IO ()
caseLabelCasOperand = do
  let identifier = checked "private ID fixture" (mkPrivateUniqueId 7)
      process = asPrivateProcessId identifier
      object = asPrivateObjectId identifier
      operation = LabelApplication object (ZombieLabel process, 7) LabelToVoid
      otherExpected = LabelApplication object (ZombieLabel process, 8) LabelToVoid
  assertEqual
    "the expected label survives the operation Binary instance"
    (Right operation)
    (decodeBinary (encode operation))
  assertBool
    "changing only the expected generation changes the canonical operation bytes"
    (encode operation /= encode otherExpected)

-- All three boundaries carry the same scalar pair, so a stale or ABA-returned
-- owner is still a distinct operand when its generation differs.
propLabelGenerationBytes :: Word64 -> Property
propLabelGenerationBytes otherGeneration =
  forAll genLabel $ \(owner, generation) ->
    generation /= otherGeneration
      ==> let current = (owner, generation)
              other = (owner, otherGeneration)
              object = asPrivateObjectId (checked "private ID fixture" (mkPrivateUniqueId 7))
           in conjoin
                [ (encode (LabelValue current) /= encode (LabelValue other)) === True,
                  (encode (QueryLabel current) /= encode (QueryLabel other)) === True,
                  (encode (LabelApplication object current LabelToVoid) /= encode (LabelApplication object other LabelToVoid)) === True,
                  (LabelValue current /= LabelValue other) === True,
                  (QueryLabel current /= QueryLabel other) === True,
                  (LabelApplication object current LabelToVoid /= LabelApplication object other LabelToVoid) === True
                ]

caseLiveDecoderAdmission :: IO ()
caseLiveDecoderAdmission = do
  assertDecodeFails
    (decodeBinary (LazyByteString.singleton 3) :: Either String ApplicationLabelTarget)
  assertDecodeFails
    (decodeBinary (LazyByteString.singleton 2) :: Either String LabelResult)
  assertDecodeFails
    (decodeBinary (LazyByteString.singleton 3) :: Either String OperationPendingReason)
  assertDecodeFails
    (decodeBinary (LazyByteString.singleton 8) :: Either String ApplicationOperation)
  assertDecodeFails
    (decodeBinary (LazyByteString.singleton 8) :: Either String RegularCallResult)
  let zeroProcessTarget = runPut (putWord8 0 >> put (0 :: Word64))
      zeroOptionalIdentity =
        runPut
          ( putWord8 9
              >> put (Just (0 :: Word64))
          )
  assertDecodeFails
    (decodeBinary zeroProcessTarget :: Either String ApplicationLabelTarget)
  assertDecodeFails
    (decodeBinary zeroOptionalIdentity :: Either String ApplicationValue)

binaryTag :: (Binary value) => value -> Word8
binaryTag = LazyByteString.head . encode

newIdTargetConstructorTag :: NewIdTarget -> Int
newIdTargetConstructorTag BareNewId = 0
newIdTargetConstructorTag (ControlledNewId _) = 1

applicationWriteValueConstructorTag :: ApplicationWriteValue -> Int
applicationWriteValueConstructorTag (PublishValue _) = 0
applicationWriteValueConstructorTag (DeleteReserved _) = 1

operationConstructorTag :: ApplicationOperation -> Int
operationConstructorTag (NewIdApplication _) = 0
operationConstructorTag (WriteApplication _ _) = 1
operationConstructorTag (ForwardApplication _ _) = 2
operationConstructorTag (ReadApplication _) = 3
operationConstructorTag (LocalTakeApplication _) = 4
operationConstructorTag (WaitApplication _) = 5
operationConstructorTag LabelApplication {} = 6
operationConstructorTag NewEnvironmentApplication = 7

regularResultConstructorTag :: RegularCallResult -> Int
regularResultConstructorTag (NewIdCompleted _) = 0
regularResultConstructorTag (WriteCompleted _) = 1
regularResultConstructorTag (ForwardCompleted _) = 2
regularResultConstructorTag (ReadCompleted _) = 3
regularResultConstructorTag (LocalTakeCompleted _) = 4
regularResultConstructorTag (WaitCompleted _) = 5
regularResultConstructorTag (LabelCompleted _) = 6
regularResultConstructorTag (NewEnvironmentCompleted _) = 7

writeResultConstructorTag :: WriteResult -> Int
writeResultConstructorTag (SortDefinitionWritten _) = 0
writeResultConstructorTag WriteAccepted = 1

casePrivateIdsRejectZero :: IO ()
casePrivateIdsRejectZero = do
  let zeroBytes = encode (0 :: Word64)
  assertDecodeFails (decodeBinary zeroBytes :: Either String PrivateUniqueId)
  assertDecodeFails (decodeBinary zeroBytes :: Either String PrivateObjectId)
  assertDecodeFails (decodeBinary zeroBytes :: Either String PrivateProcessId)
  assertDecodeFails (decodeBinary zeroBytes :: Either String PrivateNablaId)
  assertDecodeFails (decodeBinary zeroBytes :: Either String PrivateDeltaId)

caseInvalidUtf8 :: IO ()
caseInvalidUtf8 =
  assertDecodeFails
    ( decodeBinary (encode (ByteString.singleton 0xff)) ::
        Either String Text
    )

caseAccessRejectsNonCanonicalRoles :: IO ()
caseAccessRejectsNonCanonicalRoles = do
  let identifier = checked "private ID fixture" (mkPrivateUniqueId 7)
      access role = predefinedAccess role (asPrivateNablaId identifier) (asPrivateDeltaId identifier)
      canonical = fmap access allApplicationPredefinedSortRoles
      reordered = swapFirstTwo canonical
      duplicated = duplicateFirst canonical
      incomplete = init canonical
      environmentBytes accesses = runPut (put accesses >> put fixtureHub >> put fixtureEdges)
  mapM_
    (assertDecodeFails . (decodeBinary :: LazyByteString.ByteString -> Either String EnvironmentAccess) . environmentBytes)
    [reordered, duplicated, incomplete]
  let pairs = fmap uniquePair allApplicationPredefinedSortRoles
      malformedWiring hub edges = runPut (put pairs >> put hub >> put edges)
  mapM_
    (assertDecodeFails . (decodeBinary :: LazyByteString.ByteString -> Either String EnvironmentAccess))
    [malformedWiring fixtureHub (init fixtureEdges), malformedWiring fixtureHub (fixtureHub : drop 1 fixtureEdges), malformedWiring fixtureHub (fixtureEdges <> [fixtureHub])]

assertRoundTrip :: (Binary value, Eq value, Show value) => value -> IO ()
assertRoundTrip value =
  case decodeBinary (encode value) of
    Left problem -> assertFailure problem
    Right decoded -> assertBool ("round-trip mismatch: " <> show value) (decoded == value)

assertDecodeFails :: Either String value -> IO ()
assertDecodeFails result =
  case result of
    Left _ -> pure ()
    Right _ -> assertFailure "malformed refined value decoded successfully"

swapFirstTwo :: [value] -> [value]
swapFirstTwo (first : second : remaining) = second : first : remaining
swapFirstTwo values = values

duplicateFirst :: [value] -> [value]
duplicateFirst (first : _ : remaining) = first : first : remaining
duplicateFirst values = values

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

uniquePair :: ApplicationPredefinedSortRole -> PredefinedAccess
uniquePair role =
  predefinedAccess
    role
    (asPrivateNablaId (checked "environment writer" (mkPrivateUniqueId (100 + fromIntegral (fromEnum role) * 2))))
    (asPrivateDeltaId (checked "environment reader" (mkPrivateUniqueId (101 + fromIntegral (fromEnum role) * 2))))

fixtureHub :: PrivateObjectId
fixtureHub = asPrivateObjectId (checked "environment hub" (mkPrivateUniqueId 1000))
fixtureEdges :: [PrivateObjectId]
fixtureEdges = [asPrivateObjectId (checked "environment edge" (mkPrivateUniqueId value)) | value <- [1001 .. 1018]]
