{-# LANGUAGE OverloadedStrings #-}

module EnvironmentControlledProperties (tests) where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Eclips.Domain.Environment
  ( EnvironmentRootClaimView (..),
    EnvironmentRootSlot,
    environmentManifestShapeRoots,
    environmentRootClaimView,
    environmentRootSlotClaim,
    environmentRootSlotStructuralCarrierRole,
    profileEnvironmentManifestShape,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    NablaId,
    NablaSequencing (..),
    ProcessEpochId,
    ProcessId,
    SortDefinitionOccurrenceId,
    controlIndex,
    deltaIdFromGlobalObjectId,
    genesisAuthorityEpoch,
    globalObjectIdFromGlobalUniqueId,
    globalUniqueIdFromGlobalObjectId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkSortDefinitionOccurrenceId,
    nablaIdFromGlobalObjectId,
    nablaSequence,
    publicationId,
    sortIdBytes,
  )
import Eclips.Domain.Publication
  ( checkedPublicationId,
    mkCheckedPublication,
  )
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole)
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profileSortFor,
  )
import Eclips.Domain.Value
  ( FieldName,
    LabelOwner (ProcessLabel),
    Value,
    bytesValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( NonNegative (..),
    Property,
    counterexample,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "controlled private-environment roots"
    [ testCase
        "the canonical range installs twelve dynamic records without bootstrap roles or reservations"
        caseCanonicalRange,
      testCase
        "the complete exact range replays state-identically"
        caseExactReplay,
      testCase
        "exact replay survives later record evolution but still pins first use"
        caseExactReplayAfterEvolution,
      testCase
        "manifest order and a retained prefix reject atomically"
        caseManifestAndPartialReplay,
      testCase
        "root admission pins carried sort and unsequenced writer shape"
        caseRootShape,
      testCase
        "End rejects a fresh range but preserves exact accepted history"
        caseProcessEnd,
      testProperty
        "a conflicting record at any ordinal cannot install a suffix"
        propConflictAtAnyOrdinal
    ]

caseCanonicalRange :: Assertion
caseCanonicalRange = do
  let (successor, classification) = commitRange controlledBase environmentRoots
  assertEqual
    "first range classification"
    Controlled.ControlledEnvironmentRootsEstablished
    classification
  assertEqual
    "exactly twelve local records are added"
    12
    (length (Controlled.controlledLocalRecords successor))
  assertEqual
    "the two canonical source RootFacts are unchanged"
    (Controlled.controlledRootFacts controlledBase)
    (Controlled.controlledRootFacts successor)
  assertEqual
    "environment targets create no newid reservations"
    []
    (Controlled.controlledReservationWitnesses successor)
  mapM_ (assertInstalledRoot successor) (NonEmpty.toList environmentRoots)

caseExactReplay :: Assertion
caseExactReplay = do
  let (installed, _) = commitRange controlledBase environmentRoots
  replay <-
    checkedIO
      "prepare exact environment replay"
      (Controlled.prepareControlledEnvironmentRoots process environmentRoots installed)
  assertEqual
    "prepared replay classification"
    Controlled.ControlledEnvironmentRootsExactReplay
    (Controlled.preparedControlledEnvironmentRootsClassification replay)
  let (replayed, classification) = Controlled.commitControlledEnvironmentRoots replay
  assertEqual
    "committed replay classification"
    Controlled.ControlledEnvironmentRootsExactReplay
    classification
  assertBool "exact replay is state-identical" (replayed == installed)

caseExactReplayAfterEvolution :: Assertion
caseExactReplayAfterEvolution = do
  let (installed, _) = commitRange controlledBase environmentRoots
      firstRoot = NonEmpty.head environmentRoots
      firstChecked =
        Controlled.checkedControlledObservationPublication
          (Controlled.controlledEnvironmentRootObservation firstRoot)
      evolvedRoot =
        checked
          "later environment-root observation"
          ( checkedEnvironmentRoot
              (Controlled.controlledEnvironmentRootSlot firstRoot)
              (Controlled.controlledEnvironmentRootGeneratedId firstRoot)
              100
              Nothing
              Nothing
          )
      evolvedObservation =
        Controlled.controlledEnvironmentRootObservation evolvedRoot
      evolvedChecked =
        Controlled.checkedControlledObservationPublication evolvedObservation
  preparedEvolution <-
    checkedIO
      "prepare later environment-root observation"
      (Controlled.prepareControlledPeerObservation evolvedObservation installed)
  let (evolved, evolutionResult) =
        Controlled.commitControlledPeerObservation preparedEvolution
  assertEqual
    "later publication advances the mutable winner"
    Controlled.ControlledPeerWinnerAdvanced
    evolutionResult
  record <-
    maybe
      (assertFailure "evolved environment record is missing")
      pure
      (Controlled.controlledLocalRecord (Controlled.controlledEnvironmentRootTargetObject firstRoot) evolved)
  assertEqual
    "the original first-use publication remains immutable"
    firstChecked
    (Controlled.controlledRecordFirstPublication record)
  assertEqual
    "the later publication is now the mutable winner"
    evolvedChecked
    (Controlled.controlledRecordLatestPublication record)
  replay <-
    checkedIO
      "prepare environment replay after winner evolution"
      (Controlled.prepareControlledEnvironmentRoots process environmentRoots evolved)
  let (replayed, classification) =
        Controlled.commitControlledEnvironmentRoots replay
  assertEqual
    "the original manifest remains an exact replay"
    Controlled.ControlledEnvironmentRootsExactReplay
    classification
  assertBool "evolved state is retained unchanged" (replayed == evolved)
  let evolvedAsFirstUse =
        NonEmpty.fromList (evolvedRoot : NonEmpty.tail environmentRoots)
  case Controlled.prepareControlledEnvironmentRoots process evolvedAsFirstUse evolved of
    Left (Controlled.ControlledEnvironmentRootsReplayConflict target identifier) -> do
      assertEqual
        "the conflict names the evolved target"
        (Controlled.controlledEnvironmentRootTargetObject firstRoot)
        target
      assertEqual
        "the conflict names the substituted publication"
        (checkedPublicationId evolvedChecked)
        identifier
    other ->
      assertFailure
        ("expected immutable first-use conflict, got " <> resultShape other)

caseManifestAndPartialReplay :: Assertion
caseManifestAndPartialReplay = do
  let reversed = NonEmpty.fromList (reverse (NonEmpty.toList environmentRoots))
  case Controlled.prepareControlledEnvironmentRoots process reversed controlledBase of
    Left (Controlled.ControlledEnvironmentRootsManifestMismatch _) -> pure ()
    other -> assertFailure ("expected manifest mismatch, got " <> resultShape other)
  let first = NonEmpty.head environmentRoots
  withPrefix <-
    checkedIO
      "install exact retained prefix through ordinary checked first use"
      (installOrdinaryRoot first controlledBase)
  case Controlled.prepareControlledEnvironmentRoots process environmentRoots withPrefix of
    Left Controlled.ControlledEnvironmentRootsPartialReplay -> pure ()
    other -> assertFailure ("expected partial-replay rejection, got " <> resultShape other)
  assertEqual
    "only the pre-existing prefix remains"
    1
    (length (Controlled.controlledLocalRecords withPrefix))

caseRootShape :: Assertion
caseRootShape = do
  let writerSlot = NonEmpty.head environmentSlots
      target = generatedId 0
      wrongSort = checkedEnvironmentRoot writerSlot target 1 (Just EdgeRole) Nothing
  case wrongSort of
    Left (Controlled.ControlledEnvironmentRootCarriedSortMismatch {}) -> pure ()
    other -> assertFailure ("expected carried-sort mismatch, got " <> either show (const "accepted") other)
  let sequenced =
        checkedEnvironmentRoot
          writerSlot
          target
          1
          Nothing
          (Just (globalObjectIdFromGlobalUniqueId (generatedId 31)))
  case sequenced of
    Left (Controlled.ControlledEnvironmentRootSequencingShapeMismatch _) -> pure ()
    other -> assertFailure ("expected sequencing mismatch, got " <> either show (const "accepted") other)

caseProcessEnd :: Assertion
caseProcessEnd = do
  let endedBefore =
        Controlled.commitControlledProcessRetirement
          (Controlled.prepareControlledProcessRetirement process controlledBase)
  case Controlled.prepareControlledEnvironmentRoots process environmentRoots endedBefore of
    Left (Controlled.ControlledEnvironmentRootsProcessEnded observed) ->
      assertEqual "ended process" process observed
    other -> assertFailure ("expected ended-process rejection, got " <> resultShape other)
  let (installed, _) = commitRange controlledBase environmentRoots
      endedAfter =
        Controlled.commitControlledProcessRetirement
          (Controlled.prepareControlledProcessRetirement process installed)
  replay <-
    checkedIO
      "prepare accepted range after End"
      (Controlled.prepareControlledEnvironmentRoots process environmentRoots endedAfter)
  let (replayed, classification) = Controlled.commitControlledEnvironmentRoots replay
  assertEqual
    "accepted history remains an exact replay"
    Controlled.ControlledEnvironmentRootsExactReplay
    classification
  assertBool "post-End replay is state-identical" (replayed == endedAfter)
  assertEqual
    "all twelve immutable records remain"
    12
    (length (Controlled.controlledLocalRecords replayed))
  assertBool
    "End removes every target possession"
    ( all
        ( \root ->
            not
              ( Controlled.controlledHasNormalPossession
                  process
                  (Controlled.controlledEnvironmentRootTargetObject root)
                  replayed
              )
        )
        (NonEmpty.toList environmentRoots)
    )
  assertBool
    "End removes every retained direct-possession entry"
    ( all
        ( \root ->
            not
              ( Controlled.controlledHasDirectPossession
                  process
                  (Controlled.controlledEnvironmentRootTargetObject root)
                  endedAfter
              )
        )
        (NonEmpty.toList environmentRoots)
    )

propConflictAtAnyOrdinal :: NonNegative Int -> Property
propConflictAtAnyOrdinal (NonNegative choice) =
  counterexample ("ordinal=" <> show ordinal) $ case conflictingState of
    Left problem -> counterexample problem False
    Right state ->
      case Controlled.prepareControlledEnvironmentRoots process environmentRoots state of
        Left Controlled.ControlledEnvironmentRootsReplayConflict {} ->
          counterexample
            "conflict preparation exposed a partial successor"
            (length (Controlled.controlledLocalRecords state) == 1)
        other -> counterexample ("unexpected result: " <> resultShape other) False
  where
    ordinal = choice `mod` 12
    root = NonEmpty.toList environmentRoots !! ordinal
    slot = Controlled.controlledEnvironmentRootSlot root
    target = Controlled.controlledEnvironmentRootGeneratedId root
    conflictingState = do
      competingRoot <-
        either
          (Left . show)
          Right
          (checkedEnvironmentRoot slot target (100 + fromIntegral ordinal) Nothing Nothing)
      either (Left . show) Right (installOrdinaryRoot competingRoot controlledBase)

assertInstalledRoot :: Controlled.State -> Controlled.ControlledEnvironmentRoot -> Assertion
assertInstalledRoot state root = do
  let target = Controlled.controlledEnvironmentRootTargetObject root
      slot = Controlled.controlledEnvironmentRootSlot root
      observation = Controlled.controlledEnvironmentRootObservation root
      expectedCarrier = environmentRootSlotStructuralCarrierRole slot
  record <-
    maybe
      (assertFailure "environment target record is missing")
      pure
      (Controlled.controlledLocalRecord target state)
  assertEqual "target object" target (Controlled.controlledRecordObjectId record)
  assertEqual
    "carrier occurrence"
    (Controlled.checkedControlledObservationOccurrence observation)
    (Controlled.controlledRecordOccurrenceId record)
  assertEqual
    "carrier role"
    (Just expectedCarrier)
    (Controlled.controlledRecordStructuralRole record)
  assertEqual
    "initial process label"
    (Just ((ProcessLabel process, 0)))
    (Controlled.controlledRecordObservedLabel record)
  assertEqual
    "immutable publisher source"
    (Set.singleton process)
    (Controlled.controlledRecordPossessionSources record)
  assertBool
    "the live caller effectively possesses the target"
    (Controlled.controlledHasNormalPossession process target state)
  assertBool
    "the target is not a bootstrap RootFact"
    (not (Controlled.controlledHasAnyRole target state))
  assertEqual
    "the target has no reservation"
    Nothing
    ( Controlled.controlledReservationWitness
        (Controlled.controlledEnvironmentRootGeneratedId root)
        state
    )
  case environmentRootClaimView (environmentRootSlotClaim slot) of
    EnvironmentWriterRootClaimView {} ->
      assertEqual
        "writer target is absent from bootstrap writer facts"
        Nothing
        ( Controlled.controlledWriterFact
            (nablaIdFromGlobalObjectId target)
            state
        )
    EnvironmentReaderRootClaimView {} ->
      assertEqual
        "reader target is absent from bootstrap reader facts"
        Nothing
        ( Controlled.controlledReaderFact
            (deltaIdFromGlobalObjectId target)
            state
        )
    EnvironmentHubRootClaimView -> assertFailure "endpoint fixture received a hub slot"
    EnvironmentEdgeRootClaimView {} -> assertFailure "endpoint fixture received an edge slot"

commitRange ::
  Controlled.State ->
  NonEmpty Controlled.ControlledEnvironmentRoot ->
  (Controlled.State, Controlled.ControlledEnvironmentRootsClassification)
commitRange state roots =
  Controlled.commitControlledEnvironmentRoots
    ( checked
        "prepare controlled environment range"
        (Controlled.prepareControlledEnvironmentRoots process roots state)
    )

environmentRoots :: NonEmpty Controlled.ControlledEnvironmentRoot
environmentRoots =
  NonEmpty.fromList
    [ checked
        ("checked environment root " <> show ordinal)
        (checkedEnvironmentRoot slot (generatedId ordinal) (fromIntegral ordinal + 1) Nothing Nothing)
    | (ordinal, slot) <- zip [0 ..] (NonEmpty.toList environmentSlots)
    ]

environmentSlots :: NonEmpty EnvironmentRootSlot
environmentSlots = environmentManifestShapeRoots profileEnvironmentManifestShape

checkedEnvironmentRoot ::
  EnvironmentRootSlot ->
  GlobalUniqueId ->
  Word64 ->
  Maybe PredefinedSortRole ->
  Maybe GlobalObjectId ->
  Either Controlled.ControlledEnvironmentRootError Controlled.ControlledEnvironmentRoot
checkedEnvironmentRoot slot generated sequenceNumber carriedOverride sequencingOverride = do
  let (carrierRole, carrierCatalogueRole, dataRole, source, occurrence) = rootFacts slot
      descriptor = predefinedCatalogueDescriptor (profileEntryFor carrierCatalogueRole)
      carriedRole = maybe dataRole id carriedOverride
      binding =
        Controlled.controlledWriterBinding
          process
          source
          (profileSortFor carrierCatalogueRole)
          occurrence
          authority
          prerequisite
      identifier = publicationId source authority herald (nablaSequence sequenceNumber)
      value = rootValue slot generated carriedRole sequencingOverride
      publication = checked "environment root publication" (mkCheckedPublication descriptor identifier value)
      observation =
        checked
          "controlled environment observation"
          (Controlled.checkControlledObservation descriptor occurrence publication)
  if environmentRootSlotStructuralCarrierRole slot /= carrierRole
    then error "closed environment carrier role changed"
    else
      Controlled.checkControlledEnvironmentRoot
        process
        slot
        generated
        binding
        observation

rootValue ::
  EnvironmentRootSlot ->
  GlobalUniqueId ->
  PredefinedSortRole ->
  Maybe GlobalObjectId ->
  Value
rootValue slot generated carriedRole sequencingOverride =
  checked "closed environment root value" (recordValue fields)
  where
    fields =
      [ (field "object_id", globalUniqueIdValue generated),
        (field "label", labelValue ((ProcessLabel process, 0))),
        (field "sort_id", bytesValue (sortIdBytes (profileSortFor carriedRole)))
      ]
        <> case environmentRootClaimView (environmentRootSlotClaim slot) of
          EnvironmentWriterRootClaimView {} ->
            [ ( field "sequencing_object",
                optionalGlobalUniqueIdValue
                  ( globalUniqueIdFromGlobalObjectId
                      <$> sequencingOverride
                  )
              )
            ]
          EnvironmentReaderRootClaimView {} -> []
          EnvironmentHubRootClaimView -> error "endpoint fixture received a hub slot"
          EnvironmentEdgeRootClaimView {} -> error "endpoint fixture received an edge slot"

rootFacts ::
  EnvironmentRootSlot ->
  ( StructuralCarrierRole,
    PredefinedSortRole,
    PredefinedSortRole,
    NablaId,
    SortDefinitionOccurrenceId
  )
rootFacts slot =
  case environmentRootClaimView (environmentRootSlotClaim slot) of
    EnvironmentWriterRootClaimView role _ ->
      ( environmentRootSlotStructuralCarrierRole slot,
        NablaRole,
        role,
        nablaSource,
        nablaOccurrence
      )
    EnvironmentReaderRootClaimView role ->
      ( environmentRootSlotStructuralCarrierRole slot,
        DeltaRole,
        role,
        deltaSource,
        deltaOccurrence
      )
    EnvironmentHubRootClaimView -> error "endpoint fixture received a hub slot"
    EnvironmentEdgeRootClaimView {} -> error "endpoint fixture received an edge slot"

controlledBase :: Controlled.State
controlledBase =
  Controlled.commitControlledBootstrap
    ( checked
        "controlled source bootstrap"
        ( Controlled.prepareControlledBootstrap
            (Controlled.processFact processIdentity process herald authority)
            [ sourceFact NablaRole nablaSource nablaOccurrence,
              sourceFact DeltaRole deltaSource deltaOccurrence
            ]
            Controlled.emptyState
        )
    )

sourceFact ::
  PredefinedSortRole ->
  NablaId ->
  SortDefinitionOccurrenceId ->
  Controlled.RootFact
sourceFact role source occurrence =
  Controlled.writerRootFact
    process
    role
    (profileSortFor role)
    occurrence
    authority
    prerequisite
    source
    UnsequencedNabla

process :: ProcessEpochId
process = checked "process epoch" (mkProcessEpochId (identityBytes 0x11))

processIdentity :: ProcessId
processIdentity = checked "process id" (mkProcessId (identityBytes 0x12))

herald :: HeraldEpoch
herald = checked "herald epoch" (mkHeraldEpoch (identityBytes 0x13))

authority :: AuthorityEpoch
authority = genesisAuthorityEpoch

prerequisite :: ControlIndex
prerequisite = controlIndex 1

nablaSource, deltaSource :: NablaId
nablaSource = checked "nabla source" (mkNablaId (identityBytes 0xe1))
deltaSource = checked "delta source" (mkNablaId (identityBytes 0xe2))

nablaOccurrence, deltaOccurrence :: SortDefinitionOccurrenceId
nablaOccurrence =
  checked "nabla occurrence" (mkSortDefinitionOccurrenceId (identityBytes 0xd1))
deltaOccurrence =
  checked "delta occurrence" (mkSortDefinitionOccurrenceId (identityBytes 0xd2))

generatedId :: Int -> GlobalUniqueId
generatedId ordinal =
  checked
    "generated environment identity"
    (mkGlobalUniqueId (identityBytes (fromIntegral ordinal + 0x20)))

identityBytes :: Word8 -> ByteString.ByteString
identityBytes byte = ByteString.replicate 31 0 <> ByteString.singleton byte

field :: Text -> FieldName
field name = checked ("field " <> show name) (mkFieldName name)

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO context = either (assertFailure . ((context <> ": ") <>) . show) pure

resultShape :: Either problem value -> String
resultShape = either (const "Left") (const "Right")

-- | Establish one ordinary source-owned object to exercise atomic environment
-- conflict/replay classification without reviving the removed configured-root arm.
installOrdinaryRoot :: Controlled.ControlledEnvironmentRoot -> Controlled.State -> Either Controlled.ControlledFirstUseError Controlled.State
installOrdinaryRoot root state = fst . Controlled.commitControlledFirstUse <$> Controlled.prepareControlledFirstUseWithBinding binding generated observation reserved
  where
    binding = Controlled.controlledEnvironmentRootSourceBinding root
    generated = Controlled.controlledEnvironmentRootGeneratedId root
    observation = Controlled.controlledEnvironmentRootObservation root
    reserved =
      Controlled.commitControlledReservation
        ( Controlled.prepareControlledReservation
            (Controlled.controlledWriterBindingProcess binding)
            (Controlled.controlledWriterBindingWriter binding)
            (Controlled.controlledWriterBindingSortId binding)
            (Controlled.controlledWriterBindingAuthority binding)
            generated
            state
        )
