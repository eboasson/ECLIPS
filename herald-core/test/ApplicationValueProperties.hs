{-# LANGUAGE OverloadedStrings #-}

module ApplicationValueProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Identity
  ( PrivateUniqueId,
    asPrivateProcessId,
    mkPrivateUniqueId,
    privateProcessUniqueId,
    privateUniqueIdWord64,
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (BoolSchema),
  )
import Eclips.Application.Types.Value qualified as Application
import Eclips.Domain.Identity
  ( GlobalUniqueId,
    ProcessEpochId,
    mkBootstrapManifestId,
    mkGlobalUniqueId,
    mkProcessEpochId,
    processEpochIdBytes,
  )
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Application.PrivateIdentity
  ( PrivateIdentityError (ProcessEpochIsUnknown),
    lookupPrivateUniqueId,
  )
import Eclips.Herald.Application.Session.Internal (applicationAttachmentForBootstrap)
import Eclips.Herald.Application.State
  ( ApplicationValueGlobalizationError (..),
    ApplicationValueInvariant (..),
    ApplicationValueRejection (..),
    BootstrapAccess,
    State,
    applicationPrivateIdentity,
    bootstrapAccessProcess,
    commitApplicationBootstrap,
    commitOrdinaryApplicationValueLocalization,
    emptyState,
    globalizeOrdinaryApplicationValue,
    prepareApplicationProcessRegistration,
    prepareOrdinaryApplicationValueLocalization,
    prepareOrdinaryApplicationValueLocalizationWithZombieOverlay,
    preparedLocalizedOrdinaryApplicationValue,
    processRoleView,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    counterexample,
    elements,
    forAll,
    frequency,
    listOf,
    oneof,
    resize,
    sized,
    sublistOf,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "application value translation"
    [ testProperty "generated coherent ordinary trees round-trip" propGeneratedApplicationRoundTrip,
      testProperty "nested localization and globalization round-trip" propNestedRoundTrip,
      testCase "invalid nested field text rejects at the Herald boundary" caseInvalidFieldName,
      testCase "an unknown nested private ID rejects without state" caseUnknownPrivateId,
      testCase "a nested sort definition is an ordinary rejection" caseMisplacedSortDefinition,
      testCase "localization allocates in ascending field order and reuses aliases" caseLocalizationOrder,
      testCase "record presentation order cannot affect localization" casePresentationOrder,
      testCase "failed localization discards every tentative alias" caseLocalizationAtomicity,
      testCase "process, zombie, and void labels retain their roles" caseLabels,
      testProperty "zombie overlays preserve the complete observed generation" propZombieOverlayGeneration,
      testCase "a mapped non-process identity cannot become a process label" caseProcessRoleMismatch,
      testCase "a locally installed process missing from the role view is an invariant" caseLocalProcessRoleContradiction,
      testCase "checked-state contradictions remain invariant faults" caseInvariantClassification,
      testCase "equal private bits remain isolated by process epoch" caseProcessIsolation
    ]

propGeneratedApplicationRoundTrip :: Property
propGeneratedApplicationRoundTrip =
  forAll (genCoherentApplicationValue privateIdentityA) $ \applicationValue ->
    case globalizeOrdinaryApplicationValue roles processA applicationValue predecessor of
      Left problem -> counterexample ("globalization failed: " <> show problem) False
      Right internal ->
        case prepareOrdinaryApplicationValueLocalization roles processA internal predecessor of
          Left problem -> counterexample ("localization failed: " <> show problem) False
          Right prepared ->
            let (successor, roundTrip) =
                  commitOrdinaryApplicationValueLocalization prepared
             in counterexample
                  ("round-trip value: " <> show roundTrip)
                  (roundTrip == applicationValue && successor == predecessor)
  where
    (predecessor, accesses) = bootstrapped [processA]
    privateIdentityA =
      privateProcessUniqueId
        (bootstrapAccessProcess (accesses Map.! processA))
    roles = processRoleView [processA]

genCoherentApplicationValue :: PrivateUniqueId -> Gen Application.ApplicationValue
genCoherentApplicationValue privateIdentityA = sized generate
  where
    generate remaining
      | remaining <= 0 = scalar
      | otherwise =
          frequency
            [ (5, scalar),
              (2, record (remaining `div` 2))
            ]
    scalar =
      oneof
        [ Application.BoolValue <$> arbitrary,
          Application.Int64Value <$> arbitrary,
          Application.BytesValue . ByteString.pack <$> resize 8 (listOf arbitrary),
          Application.TextValue . Text.pack <$> resize 8 (listOf (elements ['a' .. 'z'])),
          pure (Application.UniqueIdValue privateIdentityA),
          Application.OptionalUniqueIdValue
            <$> elements [Nothing, Just privateIdentityA],
          Application.LabelValue
            <$> ((,) <$> elements [Application.VoidLabel, Application.ProcessLabel privateProcessA, Application.ZombieLabel privateProcessA] <*> arbitrary),
          Application.EnumValue . Text.pack <$> resize 8 (listOf (elements ['a' .. 'z']))
        ]
    record remaining = do
      names <- sublistOf ["alpha", "beta", "gamma", "nested"]
      values <- traverse (const (generate remaining)) names
      pure (Application.RecordValue (Map.fromList (zip names values)))
    privateProcessA = asPrivateProcessId privateIdentityA

propNestedRoundTrip :: Word8 -> Int64 -> Bool -> Property
propNestedRoundTrip identityByte integer boolean =
  case prepareOrdinaryApplicationValueLocalization roles processA internal predecessor of
    Left problem -> counterexample ("localization failed: " <> show problem) False
    Right prepared ->
      let (successor, applicationValue) =
            commitOrdinaryApplicationValueLocalization prepared
          repeated =
            prepareOrdinaryApplicationValueLocalization
              roles
              processA
              internal
              successor
       in case repeated of
            Left problem -> counterexample ("repeat failed: " <> show problem) False
            Right repeatedPrepared ->
              let (afterRepeat, repeatedValue) =
                    commitOrdinaryApplicationValueLocalization repeatedPrepared
               in counterexample
                    ("localized value: " <> show applicationValue)
                    ( globalizeOrdinaryApplicationValue roles processA applicationValue successor
                        == Right internal
                        && repeatedValue == applicationValue
                        && afterRepeat == successor
                    )
  where
    (predecessor, _) = bootstrapped [processA]
    roles = processRoleView [processA]
    internal =
      domainRecord
        [ ("bool", Domain.boolValue boolean),
          ("bytes", Domain.bytesValue (ByteString.pack [identityByte, 0, identityByte])),
          ("enum", Domain.enumValue (Domain.enumSymbol "middle")),
          ("nested", domainRecord [("identity", Domain.globalUniqueIdValue (globalIdentity identityByte))]),
          ("number", Domain.int64Value integer),
          ("optional_none", Domain.optionalGlobalUniqueIdValue Nothing),
          ("optional_some", Domain.optionalGlobalUniqueIdValue (Just (globalIdentity identityByte))),
          ("process", Domain.labelValue (Domain.ProcessLabel processA, 7)),
          ("text", Domain.textValue "round trip"),
          ("zombie", Domain.labelValue (Domain.ZombieLabel processA, 7))
        ]

caseInvalidFieldName :: IO ()
caseInvalidFieldName = do
  let (state, _) = bootstrapped [processA]
      value =
        Application.RecordValue
          (Map.singleton "outer" (Application.RecordValue (Map.singleton "not-valid" (Application.BoolValue True))))
  assertEqual
    "the domain field-name error is retained"
    ( Left
        ( ApplicationValueRejected
            ( ApplicationValueInvalidFieldName
                "not-valid"
                (Domain.InvalidFieldNameCharacter '-')
            )
        )
    )
    (globalizeOrdinaryApplicationValue (processRoleView [processA]) processA value state)

caseUnknownPrivateId :: IO ()
caseUnknownPrivateId = do
  let (state, _) = bootstrapped [processA]
      unknown = privateIdentity 99
      value =
        Application.RecordValue
          ( Map.singleton
              "outer"
              (Application.RecordValue (Map.singleton "identity" (Application.UniqueIdValue unknown)))
          )
  assertEqual
    "the complete value rejects"
    ( Left
        ( ApplicationValueRejected
            (ApplicationValueUnknownPrivateId unknown)
        )
    )
    (globalizeOrdinaryApplicationValue (processRoleView [processA]) processA value state)
  assertEqual
    "no binding appeared"
    (Right Nothing)
    (lookupPrivateUniqueId processA (globalIdentity 99) (applicationPrivateIdentity state))

caseMisplacedSortDefinition :: IO ()
caseMisplacedSortDefinition = do
  let (state, _) = bootstrapped [processA]
      descriptor =
        ApplicationSortDescriptor
          { sortKind = RegularSort,
            valueSchema = BoolSchema,
            keyProjections =
              [ApplicationProjection ("key" :| [])],
            validityPredicate = AlwaysPredicate,
            obsolescencePredicate = AlwaysPredicate,
            rankTerms = RankApplicationValue Ascending :| [],
            minimumRetentionMicros = 0,
            isImmutable = False,
            labelField = Nothing
          }
      misplaced =
        Application.RecordValue
          ( Map.singleton
              "nested"
              ( Application.SortDefinitionValue
                  (DeclaredSortDefinition descriptor Nothing)
              )
          )
  assertEqual
    "misplaced structured definition is malformed application input"
    ( Left
        ( ApplicationValueRejected
            ApplicationValueMisplacedSortDefinition
        )
    )
    ( globalizeOrdinaryApplicationValue
        (processRoleView [processA])
        processA
        misplaced
        state
    )

caseLocalizationOrder :: IO ()
caseLocalizationOrder = do
  let (predecessor, _) = bootstrapped [processA]
      firstGlobal = globalIdentity 0x11
      secondGlobal = globalIdentity 0x22
      internal =
        domainRecord
          [ ("z", Domain.globalUniqueIdValue secondGlobal),
            ("m", Domain.globalUniqueIdValue firstGlobal),
            ("a", Domain.globalUniqueIdValue firstGlobal)
          ]
  prepared <-
    checked
      "prepare localization"
      ( prepareOrdinaryApplicationValueLocalization
          (processRoleView [processA])
          processA
          internal
          predecessor
      )
  let preparedValue = preparedLocalizedOrdinaryApplicationValue prepared
      (successor, committedValue) = commitOrdinaryApplicationValueLocalization prepared
      fields = applicationRecordFields committedValue
      firstPrivate = applicationUniqueId (fields Map.! "a")
      repeatedPrivate = applicationUniqueId (fields Map.! "m")
      secondPrivate = applicationUniqueId (fields Map.! "z")
  assertEqual "prepared and committed output" preparedValue committedValue
  assertEqual
    "preparing leaves the predecessor map unchanged"
    (Right Nothing)
    (lookupPrivateUniqueId processA firstGlobal (applicationPrivateIdentity predecessor))
  assertEqual "ascending first alias" 2 (privateUniqueIdWord64 firstPrivate)
  assertEqual "repeated alias" firstPrivate repeatedPrivate
  assertEqual "ascending second alias" 3 (privateUniqueIdWord64 secondPrivate)
  assertEqual
    "first alias commits"
    (Right (Just firstPrivate))
    (lookupPrivateUniqueId processA firstGlobal (applicationPrivateIdentity successor))
  assertEqual
    "second alias commits"
    (Right (Just secondPrivate))
    (lookupPrivateUniqueId processA secondGlobal (applicationPrivateIdentity successor))

casePresentationOrder :: IO ()
casePresentationOrder = do
  let (predecessor, _) = bootstrapped [processA]
      roles = processRoleView [processA]
      fields =
        [ ("z", Domain.globalUniqueIdValue (globalIdentity 0x22)),
          ("m", Domain.globalUniqueIdValue (globalIdentity 0x11)),
          ("a", Domain.globalUniqueIdValue (globalIdentity 0x11))
        ]
      forward = domainRecord fields
      backward = domainRecord (reverse fields)
  assertEqual "the internal records are equal" forward backward
  forwardPrepared <-
    checked
      "localize forward presentation"
      (prepareOrdinaryApplicationValueLocalization roles processA forward predecessor)
  backwardPrepared <-
    checked
      "localize backward presentation"
      (prepareOrdinaryApplicationValueLocalization roles processA backward predecessor)
  let (forwardState, forwardValue) =
        commitOrdinaryApplicationValueLocalization forwardPrepared
      (backwardState, backwardValue) =
        commitOrdinaryApplicationValueLocalization backwardPrepared
  assertEqual "localized output" forwardValue backwardValue
  assertBool "localized successor" (forwardState == backwardState)

caseLocalizationAtomicity :: IO ()
caseLocalizationAtomicity = do
  let (predecessor, _) = bootstrapped [processA]
      freshGlobal = globalIdentity 0x33
      incompleteRoles = processRoleView [processA]
      internal =
        domainRecord
          [ ("a", Domain.globalUniqueIdValue freshGlobal),
            ("z", Domain.labelValue (Domain.ProcessLabel processB, 7))
          ]
  assertLocalizationError
    "the late missing role rejects the preparation"
    (ApplicationValueInternalProcessRoleMismatch processB)
    ( prepareOrdinaryApplicationValueLocalization
        incompleteRoles
        processA
        internal
        predecessor
    )
  assertEqual
    "the earlier tentative alias was not installed"
    (Right Nothing)
    (lookupPrivateUniqueId processA freshGlobal (applicationPrivateIdentity predecessor))
  prepared <-
    checked
      "localize after failed preparation"
      ( prepareOrdinaryApplicationValueLocalization
          incompleteRoles
          processA
          (Domain.globalUniqueIdValue freshGlobal)
          predecessor
      )
  let (_, localized) = commitOrdinaryApplicationValueLocalization prepared
  assertEqual
    "the failed preparation did not consume the next alias"
    2
    (privateUniqueIdWord64 (applicationUniqueId localized))

caseLabels :: IO ()
caseLabels = do
  let (predecessor, accesses) = bootstrapped [processA]
      accessA = accesses Map.! processA
      roles = processRoleView [processA, processB]
      privateA = bootstrapAccessProcess accessA
      applicationLabels =
        [ ((Application.VoidLabel, 3), (Domain.VoidLabel, 3)),
          ((Application.ProcessLabel privateA, 7), (Domain.ProcessLabel processA, 7)),
          ((Application.ZombieLabel privateA, 9), (Domain.ZombieLabel processA, 9))
        ]
  mapM_
    ( \(applicationLabel, domainLabel) ->
        assertEqual
          "label globalization"
          (Right (Domain.labelValue domainLabel))
          ( globalizeOrdinaryApplicationValue
              roles
              processA
              (Application.LabelValue applicationLabel)
              predecessor
          )
    )
    applicationLabels
  prepared <-
    checked
      "localize another process label"
      ( prepareOrdinaryApplicationValueLocalization
          roles
          processA
          (Domain.labelValue (Domain.ZombieLabel processB, 7))
          predecessor
      )
  let (successor, localized) = commitOrdinaryApplicationValueLocalization prepared
  case localized of
    Application.LabelValue (Application.ZombieLabel privateB, generation) -> do
      assertEqual "localized zombie preserves generation" 7 generation
      assertEqual
        "a remote process role receives an alias in the caller's local map"
        2
        (privateUniqueIdWord64 (privateProcessUniqueId privateB))
      assertEqual
        "localized zombie round-trip"
        (Right (Domain.labelValue (Domain.ZombieLabel processB, 7)))
        (globalizeOrdinaryApplicationValue roles processA localized successor)
    _ -> assertFailure ("unexpected localized label: " <> show localized)

propZombieOverlayGeneration :: Word64 -> Property
propZombieOverlayGeneration generation =
  case prepareOrdinaryApplicationValueLocalizationWithZombieOverlay
    (Set.singleton processA)
    (processRoleView [processA])
    processA
    (Domain.labelValue (Domain.ProcessLabel processA, generation))
    predecessor of
    Left problem -> counterexample ("overlay localization failed: " <> show problem) False
    Right prepared ->
      let (successor, localized) = commitOrdinaryApplicationValueLocalization prepared
       in counterexample
            ("overlay output: " <> show localized)
            ( localized == Application.LabelValue (Application.ZombieLabel privateProcess, generation)
                && successor == predecessor
            )
  where
    (predecessor, accesses) = bootstrapped [processA]
    privateProcess = bootstrapAccessProcess (accesses Map.! processA)

caseProcessRoleMismatch :: IO ()
caseProcessRoleMismatch = do
  let (predecessor, _) = bootstrapped [processA]
      roles = processRoleView [processA]
      notAProcess = globalIdentity 0x44
  prepared <-
    checked
      "localize ordinary global identity"
      ( prepareOrdinaryApplicationValueLocalization
          roles
          processA
          (Domain.globalUniqueIdValue notAProcess)
          predecessor
      )
  let (successor, localized) = commitOrdinaryApplicationValueLocalization prepared
      privateProcess = asPrivateProcessId (applicationUniqueId localized)
  assertEqual
    "the alias alone does not confer a process role"
    ( Left
        ( ApplicationValueRejected
            (ApplicationValueProcessRoleMismatch privateProcess)
        )
    )
    ( globalizeOrdinaryApplicationValue
        roles
        processA
        (Application.LabelValue (Application.ProcessLabel privateProcess, 7))
        successor
    )

caseLocalProcessRoleContradiction :: IO ()
caseLocalProcessRoleContradiction = do
  let (state, accesses) = bootstrapped [processA]
      privateProcess = bootstrapAccessProcess (accesses Map.! processA)
      invariant = ApplicationValueInternalProcessRoleMismatch processA
  assertEqual
    "local application installation makes the missing Oracle role contradictory"
    (Left (ApplicationValueGlobalizationInvariant invariant))
    ( globalizeOrdinaryApplicationValue
        (processRoleView [])
        processA
        (Application.LabelValue (Application.ProcessLabel privateProcess, 7))
        state
    )

caseInvariantClassification :: IO ()
caseInvariantClassification = do
  let global = globalIdentity 0x77
      private = privateIdentity 1
      invariant = ApplicationValuePrivateIdentityInvariant ProcessEpochIsUnknown
  assertEqual
    "globalization classifies an absent process map as an invariant"
    (Left (ApplicationValueGlobalizationInvariant invariant))
    ( globalizeOrdinaryApplicationValue
        (processRoleView [processA])
        processA
        (Application.UniqueIdValue private)
        emptyState
    )
  assertLocalizationError
    "localization classifies an absent process map as an invariant"
    invariant
    ( prepareOrdinaryApplicationValueLocalization
        (processRoleView [processA])
        processA
        (Domain.globalUniqueIdValue global)
        emptyState
    )

caseProcessIsolation :: IO ()
caseProcessIsolation = do
  let (predecessor, _) = bootstrapped [processA, processB]
      roles = processRoleView [processA, processB]
      globalA = globalIdentity 0x55
      globalB = globalIdentity 0x66
  preparedA <-
    checked
      "localize for process A"
      (prepareOrdinaryApplicationValueLocalization roles processA (Domain.globalUniqueIdValue globalA) predecessor)
  let (afterA, valueA) = commitOrdinaryApplicationValueLocalization preparedA
  preparedB <-
    checked
      "localize for process B"
      (prepareOrdinaryApplicationValueLocalization roles processB (Domain.globalUniqueIdValue globalB) afterA)
  let (successor, valueB) = commitOrdinaryApplicationValueLocalization preparedB
      privateA = applicationUniqueId valueA
      privateB = applicationUniqueId valueB
  assertBool "the global meanings differ" (globalA /= globalB)
  assertEqual "both fresh namespaces begin at the same local bits" privateA privateB
  assertEqual
    "process A resolves in its own namespace"
    (Right (Domain.globalUniqueIdValue globalA))
    (globalizeOrdinaryApplicationValue roles processA valueA successor)
  assertEqual
    "process B resolves in its own namespace"
    (Right (Domain.globalUniqueIdValue globalB))
    (globalizeOrdinaryApplicationValue roles processB valueB successor)

bootstrapped :: [ProcessEpochId] -> (State, Map ProcessEpochId BootstrapAccess)
bootstrapped = foldl install (emptyState, Map.empty)
  where
    install (state, accesses) process =
      case prepareApplicationProcessRegistration attachment process state of
        Left problem -> error ("application bootstrap failed: " <> show problem)
        Right prepared ->
          let (successor, access) = commitApplicationBootstrap prepared
           in (successor, Map.insert process access accesses)
      where
        attachment =
          applicationAttachmentForBootstrap
            ( checkedPure
                "bootstrap manifest ID"
                (mkBootstrapManifestId (processEpochIdBytes process))
            )

domainRecord :: [(Text, Domain.Value)] -> Domain.Value
domainRecord fields =
  Domain.recordValueMap
    ( Map.fromList
        [ ( checkedPure
              ("field " <> Text.unpack name)
              (Domain.mkFieldName name),
            value
          )
        | (name, value) <- fields
        ]
    )

applicationRecordFields :: Application.ApplicationValue -> Map Text Application.ApplicationValue
applicationRecordFields = \case
  Application.RecordValue fields -> fields
  value -> error ("expected application record, got " <> show value)

applicationUniqueId :: Application.ApplicationValue -> PrivateUniqueId
applicationUniqueId = \case
  Application.UniqueIdValue identifier -> identifier
  value -> error ("expected application unique ID, got " <> show value)

globalIdentity :: Word8 -> GlobalUniqueId
globalIdentity byte =
  checkedPure
    "global unique ID"
    (mkGlobalUniqueId (ByteString.replicate 32 byte))

processA, processB :: ProcessEpochId
processA =
  checkedPure
    "process A"
    (mkProcessEpochId (ByteString.replicate 32 0xa1))
processB =
  checkedPure
    "process B"
    (mkProcessEpochId (ByteString.replicate 32 0xb2))

privateIdentity :: Word8 -> PrivateUniqueId
privateIdentity value =
  checkedPure "private unique ID" (mkPrivateUniqueId (fromIntegral value))

checked :: (Show problem) => String -> Either problem value -> IO value
checked description = either (fail . ((description <> ": ") <>) . show) pure

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure description =
  either (error . ((description <> ": ") <>) . show) id

assertLocalizationError ::
  String ->
  ApplicationValueInvariant ->
  Either ApplicationValueInvariant value ->
  IO ()
assertLocalizationError description expected result = case result of
  Left actual -> assertEqual description expected actual
  Right _ -> assertFailure (description <> ": preparation succeeded")
