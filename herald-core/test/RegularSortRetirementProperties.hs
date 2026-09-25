{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module RegularSortRetirementProperties (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteStringInternal
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as Serialize
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (ApplicationSortDescriptor),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (BoolSchema, RecordSchema),
  )
import Eclips.Application.Types.SortDescriptor qualified as SortSyntax
import Eclips.Domain.Disappearance (canonicalDescriptorDigestBytes, deriveCanonicalDescriptorDigest)
import Eclips.Domain.Identity
  ( ControlIndex,
    SortDefinitionOccurrenceId,
    SortId,
    SystemId,
    controlIndex,
    nablaSequence,
    sortDefinitionOccurrenceIdBytes,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Publication
  ( CheckedPublication,
    mkCheckedPublication,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalDescriptorBytes,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Profile (sortDefinitionValue)
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Domain.Startup (PredefinedSortRole (SortDefinitionRole))
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortDescriptor,
  )
import Eclips.Herald.Genesis.Internal
  ( DeploymentManifest (..),
    PrimordialDefinitionManifest (..),
    PrimordialDefinitionReplica,
    checkHeraldGenesis,
    checkedPrimordialReplicas,
    checkedSystemId,
    primordialReplicaDescriptor,
    primordialReplicaPublicationId,
    primordialReplicaRole,
  )
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (ptrToIntPtr)
import GenesisFixtures (fixtureCheckedGenesis, fixtureDeploymentManifest, fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    choose,
    conjoin,
    counterexample,
    forAll,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "regular sort retirement registry"
    [ testCase
        "fresh exact retirement removes only the regular effective entry"
        caseFreshExactRetirement,
      testCase
        "retirement history is immutable and an exact replay is a no-op"
        caseRetirementHistoryAndReplay,
      testCase
        "missing, predefined, zero-index, and malformed retirements reject"
        caseInitialRetirementRejections,
      testCase
        "conflicting history and non-increasing retirement indices reject"
        caseHistoryAndOrderingRejections,
      testProperty
        "every admitted resolve index determines the successor coordinate and prerequisite"
        propResolveIndexDeterminesSuccessor,
      testProperty
        "unseen retirements preserve history and require the latest successor on first induction"
        propUnseenRetirementHistory,
      testProperty
        "checked registry base preserves effective and retired definitions without inventing unseen descriptors"
        propRegistryBasePreservesCurrentFacts,
      testCase
        "checked registry base requires a fresh receiver with the same primordial provenance"
        caseRegistryBaseAdmission,
      testProperty
        "canonical registry bases preserve immutable multi-round known and unseen retirement chains"
        propRegistryBaseCodecChains,
      testCase
        "registry base decoding rejects noncanonical transcripts and inconsistent nominal facts"
        caseRegistryBaseCodecRejections,
      testCase
        "decoded registry facts own buffers independent of the enclosing transcript"
        caseRegistryBaseBufferOwnership,
      testCase
        "genesis and post-retirement induction plans use different epochs"
        caseInductionPlans,
      testCase
        "a stale retired occurrence cannot restore the old definition"
        caseStaleOccurrenceRejected,
      testCase
        "the equal successor definition induces once and then repeats idempotently"
        caseEqualSuccessorInduction,
      testCase
        "a supplied successor that disagrees with derivation is detected"
        caseWrongSuccessorDetected,
      testCase
        "two retirement and equal-redefinition rounds form an ordered chain"
        caseSecondRetirementChain,
      testCase
        "a different descriptor keeps its distinct content-addressed identity"
        caseDistinctDescriptorIdentity
    ]

caseFreshExactRetirement :: Assertion
caseFreshExactRetirement = do
  let before = SortRegistry.clearAlignmentPlanChange registryWithDefinition
      predefinedBefore = SortRegistry.predefinedRegistryEntries before
      prepared =
        checkedPure
          "fresh retirement"
          ( SortRegistry.prepareExactRegularSortRetirement
              systemId
              effectiveEntry
              firstResolveIndex
              firstSuccessorOccurrence
              before
          )
      after = SortRegistry.commitRegularSortRetirement prepared
  assertEqual "removing an effective definition wakes plans" True (fst (SortRegistry.takeAlignmentPlanChange after))
  let clean = snd (SortRegistry.takeAlignmentPlanChange after)
      repeated = SortRegistry.commitRegularSortRetirement (checkedPure "drained retirement replay" (prepareFirstRetirement clean))
  assertEqual "retirement replay does not wake plans" False (fst (SortRegistry.takeAlignmentPlanChange repeated))
  assertEqual "first induction wakes plans" True (fst (SortRegistry.takeAlignmentPlanChange registryWithDefinition))
  assertEqual "equal induction after draining is silent" False (fst (SortRegistry.takeAlignmentPlanChange (induce regularDescriptor genesisOccurrence before)))
  assertEqual
    "the exact regular occurrence is no longer effective"
    Nothing
    (SortRegistry.lookupEffectiveSort regularSortId after)
  assertEqual
    "the closed predefined catalogue is untouched"
    predefinedBefore
    (SortRegistry.predefinedRegistryEntries after)
  assertEqual
    "exactly the added regular definition left the effective registry"
    (length (SortRegistry.registryEntries before) - 1)
    (length (SortRegistry.registryEntries after))
  assertEqual
    "the fresh transition reports application"
    SortRegistry.RegularSortRetirementApplied
    ( SortRegistry.regularSortRetirementSummaryDisposition
        (SortRegistry.preparedRegularSortRetirementSummary prepared)
    )

caseRetirementHistoryAndReplay :: Assertion
caseRetirementHistoryAndReplay = do
  let firstPrepared =
        checkedPure
          "first retirement"
          (prepareFirstRetirement registryWithDefinition)
      firstSummary =
        SortRegistry.preparedRegularSortRetirementSummary firstPrepared
      retired = SortRegistry.commitRegularSortRetirement firstPrepared
      replayPrepared =
        checkedPure
          "exact retirement replay"
          (prepareFirstRetirement retired)
      replaySummary =
        SortRegistry.preparedRegularSortRetirementSummary replayPrepared
      replayed = SortRegistry.commitRegularSortRetirement replayPrepared
      retained =
        required
          "retained first retirement"
          ( SortRegistry.lookupRegularSortRetirement
              regularSortId
              genesisOccurrence
              retired
          )
  assertEqual
    "history retains the exact retired entry"
    (Just effectiveEntry)
    (SortRegistry.regularSortRetirementEntry retained)
  assertEqual
    "history retains the applied resolve index"
    firstResolveIndex
    (SortRegistry.regularSortRetirementResolveIndex retained)
  assertEqual
    "history retains the successor occurrence"
    firstSuccessorOccurrence
    (SortRegistry.regularSortRetirementSuccessorOccurrenceId retained)
  assertEqual
    "the applied summary carries the retired entry"
    (Just effectiveEntry)
    (SortRegistry.regularSortRetirementSummaryEntry firstSummary)
  assertEqual
    "the applied summary carries the resolve index"
    firstResolveIndex
    (SortRegistry.regularSortRetirementSummaryResolveIndex firstSummary)
  assertEqual
    "the applied summary carries the successor"
    firstSuccessorOccurrence
    (SortRegistry.regularSortRetirementSummarySuccessorOccurrenceId firstSummary)
  assertEqual
    "the replay is classified explicitly"
    SortRegistry.RegularSortRetirementExactReplay
    (SortRegistry.regularSortRetirementSummaryDisposition replaySummary)
  assertBool "an exact replay preserves the complete registry state" (replayed == retired)
  assertEqual
    "an exact replay does not append duplicate history"
    1
    (length (SortRegistry.regularSortRetirements replayed))

caseInitialRetirementRejections :: Assertion
caseInitialRetirementRejections = do
  let before = registryWithDefinition
      zeroIndex = controlIndex 0
      predefinedEntry =
        required
          "sort-definition predefined entry"
          (SortRegistry.lookupPredefinedRole SortDefinitionRole before)
      predefinedSortId = SortRegistry.registryEntrySortId predefinedEntry
      predefinedOccurrence =
        SortRegistry.registryEntryOccurrenceId predefinedEntry
      regularSnapshot = SortRegistry.lookupEffectiveSort regularSortId before
      predefinedSnapshot =
        SortRegistry.lookupEffectiveSort predefinedSortId before
  assertLeft
    "an entry absent from the effective state"
    ( SortRegistry.RegularSortRetirementEffectiveEntryMismatch
        (Just effectiveEntry)
        Nothing
    )
    (prepareFirstRetirement (SortRegistry.initialState fixtureCheckedGenesis))
  assertLeft
    "a predefined sort"
    ( SortRegistry.RegularSortRetirementPredefinedSort
        predefinedSortId
        SortDefinitionRole
    )
    ( SortRegistry.prepareExactRegularSortRetirement
        systemId
        predefinedEntry
        firstResolveIndex
        (successorOccurrence predefinedSortId firstResolveIndex)
        before
    )
  assertLeft
    "resolve index zero"
    ( SortRegistry.RegularSortRetirementResolveIndexMustBePositive
        regularSortId
        zeroIndex
    )
    ( SortRegistry.prepareExactRegularSortRetirement
        systemId
        effectiveEntry
        zeroIndex
        firstSuccessorOccurrence
        before
    )
  assertLeft
    "a successor equal to the retired occurrence"
    ( SortRegistry.RegularSortRetirementSuccessorEqualsRetired
        regularSortId
        genesisOccurrence
    )
    ( SortRegistry.prepareExactRegularSortRetirement
        systemId
        effectiveEntry
        firstResolveIndex
        genesisOccurrence
        before
    )
  assertEqual
    "rejected preparation leaves the regular effective entry available"
    regularSnapshot
    (SortRegistry.lookupEffectiveSort regularSortId before)
  assertEqual
    "rejected preparation leaves the predefined effective entry available"
    predefinedSnapshot
    (SortRegistry.lookupEffectiveSort predefinedSortId before)
  assertEqual
    "rejected preparation leaves retirement history empty"
    []
    (SortRegistry.regularSortRetirements before)
  assertEqual
    "the predefined occurrence itself was not altered"
    predefinedOccurrence
    ( SortRegistry.registryEntryOccurrenceId
        ( required
            "retained predefined entry"
            (SortRegistry.lookupEffectiveSort predefinedSortId before)
        )
    )

caseHistoryAndOrderingRejections :: Assertion
caseHistoryAndOrderingRejections = do
  let firstRetired = retireFirst registryWithDefinition
      changedIndex = controlIndex 8
      changedSuccessor = successorOccurrence regularSortId changedIndex
      firstRedefined = induce regularDescriptor firstSuccessorOccurrence firstRetired
      successorEntry =
        required
          "first successor entry"
          (SortRegistry.lookupEffectiveSort regularSortId firstRedefined)
      secondSuccessor = successorOccurrence regularSortId (controlIndex 9)
  assertLeft
    "the same occurrence with different history"
    ( SortRegistry.RegularSortRetirementHistoryMismatch
        regularSortId
        genesisOccurrence
    )
    ( SortRegistry.prepareExactRegularSortRetirement
        systemId
        effectiveEntry
        changedIndex
        changedSuccessor
        firstRetired
    )
  assertLeft
    "a second retirement at the same resolve index"
    ( SortRegistry.RegularSortRetirementResolveIndexNotIncreasing
        regularSortId
        firstResolveIndex
        firstResolveIndex
    )
    ( SortRegistry.prepareExactRegularSortRetirement
        systemId
        successorEntry
        firstResolveIndex
        secondSuccessor
        firstRedefined
    )
  assertLeft
    "a retired occurrence cannot be nominated as a later successor"
    ( SortRegistry.RegularSortRetirementSuccessorAlreadyRetired
        regularSortId
        genesisOccurrence
    )
    ( SortRegistry.prepareExactRegularSortRetirement
        systemId
        successorEntry
        (controlIndex 9)
        genesisOccurrence
        firstRedefined
    )
  assertBool
    "all rejected preparations preserve the established first-successor state"
    ( firstRedefined
        == induce regularDescriptor firstSuccessorOccurrence firstRetired
    )

propResolveIndexDeterminesSuccessor :: Property
propResolveIndexDeterminesSuccessor =
  forAll (choose (1, 1_000_000) :: Gen Word64) $ \rawIndex ->
    let resolveIndex = controlIndex rawIndex
        expectedOccurrence = successorOccurrence regularSortId resolveIndex
        retired =
          retire
            effectiveEntry
            resolveIndex
            expectedOccurrence
            registryWithDefinition
     in case SortRegistry.planSortInduction systemId regularDescriptor retired of
          Left problem -> counterexample ("planning rejected: " <> show problem) False
          Right plan ->
            conjoin
              [ SortRegistry.sortInductionPlanSortId plan === regularSortId,
                SortRegistry.sortInductionPlanOccurrenceId plan
                  === expectedOccurrence,
                SortRegistry.sortInductionPlanControlPrerequisite plan
                  === resolveIndex
              ]

propUnseenRetirementHistory :: Property
propUnseenRetirementHistory =
  forAll (choose (1, 1_000_000) :: Gen Word64) $ \firstIndex ->
    forAll (choose (1, 1_000_000) :: Gen Word64) $ \distance ->
      let first = controlIndex firstIndex
          second = controlIndex (firstIndex + distance)
          firstSuccessor = successorOccurrence regularSortId first
          secondSuccessor = successorOccurrence regularSortId second
          digest = deriveCanonicalDescriptorDigest regularDescriptor
          unseen occurrence index =
            SortRegistry.commitRegularSortRetirement
              . checkedPure "unseen retirement"
              . SortRegistry.prepareUnseenRegularSortRetirement
                systemId
                regularSortId
                digest
                occurrence
                index
                (successorOccurrence regularSortId index)
          retired =
            unseen
              firstSuccessor
              second
              (unseen genesisOccurrence first (SortRegistry.initialState fixtureCheckedGenesis))
          replayed = unseen genesisOccurrence first retired
          plan = checkedPure "unseen successor plan" (SortRegistry.planSortInduction systemId regularDescriptor retired)
          induced = induce regularDescriptor secondSuccessor retired
          expectedHistory =
            [ (genesisOccurrence, first, firstSuccessor),
              (firstSuccessor, second, secondSuccessor)
            ]
          historyLaw (occurrence, index, successor) =
            let retained = required "unseen retained occurrence" (SortRegistry.lookupRegularSortRetirement regularSortId occurrence retired)
             in conjoin
                  [ SortRegistry.regularSortRetirementEntry retained === Nothing,
                    SortRegistry.regularSortRetirementDescriptorDigest retained === digest,
                    SortRegistry.regularSortRetirementResolveIndex retained === index,
                    SortRegistry.regularSortRetirementSuccessorOccurrenceId retained === successor
                  ]
          staleLaw occurrence =
            case SortRegistry.prepareSortInduction regularDescriptor occurrence definitionPublication retired of
              Left problem -> problem === SortRegistry.SortInductionRetiredOccurrence regularSortId occurrence
              Right _ -> counterexample "retired unseen occurrence was admitted" False
       in conjoin
            [ counterexample "unseen history replay changed the registry" (replayed == retired),
              SortRegistry.lookupEffectiveSort regularSortId retired === Nothing,
              SortRegistry.predefinedRegistryEntries retired === SortRegistry.predefinedRegistryEntries registryWithDefinition,
              conjoin (map historyLaw expectedHistory),
              conjoin (map staleLaw [genesisOccurrence, firstSuccessor]),
              SortRegistry.sortInductionPlanOccurrenceId plan === secondSuccessor,
              SortRegistry.sortInductionPlanControlPrerequisite plan === second,
              (SortRegistry.registryEntryOccurrenceId <$> SortRegistry.lookupEffectiveSort regularSortId induced) === Just secondSuccessor,
              counterexample
                "first induction rewrote unseen retirement history"
                (SortRegistry.regularSortRetirements induced == SortRegistry.regularSortRetirements retired)
            ]

propRegistryBasePreservesCurrentFacts :: Property
propRegistryBasePreservesCurrentFacts =
  forAll (choose (1, 1_000_000) :: Gen Word64) $ \rawIndex ->
    conjoin [checkBase rawIndex reinduce | reinduce <- [False, True]]
  where
    checkBase rawIndex reinduce =
      let fresh = SortRegistry.initialState fixtureCheckedGenesis
          knownIndex = controlIndex rawIndex
          knownSuccessor = successorOccurrence regularSortId knownIndex
          knownRetired = retire effectiveEntry knownIndex knownSuccessor registryWithDefinition
          unseenOccurrence = deriveSortDefinitionOccurrenceId systemId alternateSortId Genesis
          unseenIndex = controlIndex (rawIndex + 1)
          bothRetired =
            SortRegistry.commitRegularSortRetirement
              ( checkedPure
                  "unseen base retirement"
                  (SortRegistry.prepareUnseenRegularSortRetirement systemId alternateSortId (deriveCanonicalDescriptorDigest alternateDescriptor) unseenOccurrence unseenIndex (successorOccurrence alternateSortId unseenIndex) knownRetired)
              )
          source = SortRegistry.clearAlignmentPlanChange (if reinduce then induce regularDescriptor knownSuccessor bothRetired else bothRetired)
          captured = SortRegistry.captureSortRegistryBase source
          decoded = roundtripRegistryBase source
          installed = importRegistryBase source fresh
          knownHistory = required "known imported retirement" (SortRegistry.lookupRegularSortRetirement regularSortId genesisOccurrence installed)
          unseenHistory = required "unseen imported retirement" (SortRegistry.lookupRegularSortRetirement alternateSortId unseenOccurrence installed)
          stale descriptor occurrence = case SortRegistry.prepareSortInduction descriptor occurrence (definitionPublicationFor descriptor) installed of
            Left problem -> problem === SortRegistry.SortInductionRetiredOccurrence (descriptorSortId descriptor) occurrence
            Right _ -> counterexample "base installation allowed a retired occurrence to become effective" False
       in conjoin
            [ counterexample "the imported current facts differ from the captured source" (SortRegistry.clearAlignmentPlanChange installed == source),
              counterexample "canonical registry base changed captured facts" (decoded == captured),
              SortRegistry.encodeSortRegistryBase decoded === SortRegistry.encodeSortRegistryBase captured,
              SortRegistry.regularSortRetirementEntry knownHistory === Just effectiveEntry,
              SortRegistry.regularSortRetirementEntry unseenHistory === Nothing,
              SortRegistry.lookupEffectiveSort alternateSortId installed === Nothing,
              (SortRegistry.registryEntryOccurrenceId <$> SortRegistry.lookupEffectiveSort regularSortId installed) === (if reinduce then Just knownSuccessor else Nothing),
              SortRegistry.regularSortReferenceControlPrerequisite regularSortId installed === knownIndex,
              SortRegistry.regularSortReferenceControlPrerequisite alternateSortId installed === unseenIndex,
              SortRegistry.planSortInduction systemId regularDescriptor installed === SortRegistry.planSortInduction systemId regularDescriptor source,
              SortRegistry.planSortInduction systemId alternateDescriptor installed === SortRegistry.planSortInduction systemId alternateDescriptor source,
              stale regularDescriptor genesisOccurrence,
              stale alternateDescriptor unseenOccurrence,
              fst (SortRegistry.takeAlignmentPlanChange installed) === True
            ]

caseRegistryBaseAdmission :: Assertion
caseRegistryBaseAdmission = do
  let fresh = SortRegistry.initialState fixtureCheckedGenesis
      unchanged = importRegistryBase fresh fresh
      retired = retireFirst registryWithDefinition
      source = roundtripRegistryBase retired
  assertBool "equal primordial-only import is a complete no-op" (unchanged == fresh)
  assertEqual "equal primordial import does not wake alignment" False (fst (SortRegistry.takeAlignmentPlanChange unchanged))
  assertLeft "a receiver's effective definitions cannot be replaced" SortRegistry.SortRegistryBaseReceiverNotFresh (SortRegistry.prepareSortRegistryBaseImport source registryWithDefinition)
  assertLeft "a receiver's retirement history cannot be replaced" SortRegistry.SortRegistryBaseReceiverNotFresh (SortRegistry.prepareSortRegistryBaseImport source retired)
  let definitions = deploymentPrimordialDefinitions fixtureDeploymentManifest
  changedDefinitions <- case definitions of
    first : rest -> pure (first {primordialManifestPublicationSequence = nablaSequence 1000} : rest)
    [] -> assertFailure "primordial fixture catalogue is empty"
  otherGenesis <- checkedIO "different checked primordial provenance" (checkHeraldGenesis fixtureDeploymentManifest {deploymentPrimordialDefinitions = changedDefinitions})
  assertLeft "a base from another primordial publication anchor cannot be installed" SortRegistry.SortRegistryBasePrimordialMismatch (SortRegistry.prepareSortRegistryBaseImport source (SortRegistry.initialState otherGenesis))
  assertLeft "canonical decoding binds the same immutable primordial provenance" SortRegistry.SortRegistryBaseGenesisMismatch (SortRegistry.decodeSortRegistryBase otherGenesis (SortRegistry.encodeSortRegistryBase source))
  mapM_
    (\state -> assertBool "primordial-only and active definitions roundtrip exactly" (roundtripRegistryBase state == SortRegistry.captureSortRegistryBase state))
    [fresh, registryWithDefinition]

importRegistryBase :: SortRegistry.State -> SortRegistry.State -> SortRegistry.State
importRegistryBase source receiver =
  SortRegistry.commitSortRegistryBaseImport
    (checkedPure "checked sort registry base import" (SortRegistry.prepareSortRegistryBaseImport (roundtripRegistryBase source) receiver))

roundtripRegistryBase :: SortRegistry.State -> SortRegistry.SortRegistryBase
roundtripRegistryBase source =
  checkedPure "canonical sort registry base decode" (SortRegistry.decodeSortRegistryBase fixtureCheckedGenesis (SortRegistry.encodeSortRegistryBase (SortRegistry.captureSortRegistryBase source)))

propRegistryBaseCodecChains :: Property
propRegistryBaseCodecChains =
  forAll (choose (1, 8) :: Gen Word64) $ \rounds ->
    forAll (choose (1, 10_000) :: Gen Word64) $ \firstIndex ->
      let retired = foldl' (retireRound firstIndex) (SortRegistry.initialState fixtureCheckedGenesis) [1 .. rounds]
          finalOccurrence = successorOccurrence regularSortId (controlIndex (firstIndex + rounds * 3))
          variants = [retired, induce regularDescriptor finalOccurrence retired]
       in conjoin
            [ let imported = importRegistryBase source (SortRegistry.initialState fixtureCheckedGenesis)
                  captured = SortRegistry.captureSortRegistryBase source
                  decoded = roundtripRegistryBase source
               in conjoin
                    [ counterexample "multi-round canonical base changed immutable facts" (captured == decoded),
                      counterexample "multi-round import changed effective state" (SortRegistry.clearAlignmentPlanChange imported == SortRegistry.clearAlignmentPlanChange source),
                      SortRegistry.regularSortRetirements imported === SortRegistry.regularSortRetirements source,
                      map (fmap SortRegistry.registryEntryDescriptor . SortRegistry.regularSortRetirementEntry) (sortOnRetirement (SortRegistry.regularSortRetirements imported)) === [if odd ordinal then Just regularDescriptor else Nothing | ordinal <- [1 .. rounds]],
                      SortRegistry.encodeSortRegistryBase decoded === SortRegistry.encodeSortRegistryBase captured
                    ]
            | source <- variants
            ]
  where
    sortOnRetirement = Map.elems . Map.fromList . map (\retirement -> (SortRegistry.regularSortRetirementResolveIndex retirement, retirement))
    retireRound firstIndex state ordinal =
      let occurrence = maybe genesisOccurrence SortRegistry.regularSortRetirementSuccessorOccurrenceId (SortRegistry.latestRegularSortRetirement regularSortId state)
          index = controlIndex (firstIndex + ordinal * 3)
          successor = successorOccurrence regularSortId index
       in if odd ordinal
            then
              let induced = induce regularDescriptor occurrence state
                  effective = required "current chain entry" (SortRegistry.lookupEffectiveSort regularSortId induced)
               in retire effective index successor induced
            else
              SortRegistry.commitRegularSortRetirement
                (checkedPure "unseen chain link" (SortRegistry.prepareUnseenRegularSortRetirement systemId regularSortId (deriveCanonicalDescriptorDigest regularDescriptor) occurrence index successor state))

-- Nominal wire rows are deliberately exercised independently of opaque owner
-- construction: malformed transport must not create an admitted base.
type RawRegistryEntryFixture = (Maybe Word8, ByteString, ByteString, (ByteString, ByteString, ByteString, Word64))
type RawRegistryRetirementFixture = (ByteString, ByteString, ByteString, Maybe RawRegistryEntryFixture, Word64, ByteString)
type RawRegistryBaseFixture = (ByteString, [RawRegistryEntryFixture], [RawRegistryRetirementFixture])

caseRegistryBaseCodecRejections :: Assertion
caseRegistryBaseCodecRejections = do
  let retired = retireFirst registryWithDefinition
      bytes = SortRegistry.encodeSortRegistryBase (SortRegistry.captureSortRegistryBase retired)
      raw@(domain, entries, retirements) = checkedPure "decode nominal registry rows" (Serialize.decode bytes :: Either String RawRegistryBaseFixture)
      decode = SortRegistry.decodeSortRegistryBase fixtureCheckedGenesis
      reject context supplied = case decode supplied of
        Left _ -> pure ()
        Right _ -> assertFailure (context <> " accepted")
      mutateRetirement :: (RawRegistryRetirementFixture -> RawRegistryRetirementFixture) -> ByteString
      mutateRetirement change = case retirements of
        [retirement] -> Serialize.encode (domain, entries, [change retirement])
        _ -> error "registry codec fixture must have one retirement"
  reject "truncated canonical registry" (ByteString.take (ByteString.length bytes - 1) bytes)
  reject "trailing registry bytes" (bytes <> "trailing")
  reject "wrong registry domain" (Serialize.encode (("wrong-domain" :: ByteString), entries, retirements))
  assertLeft "unordered definitions are not canonical" SortRegistry.SortRegistryBaseNotCanonical (decode (Serialize.encode (domain, reverse entries, retirements)))
  case entries of
    first : _ -> assertLeft "duplicate definitions are not canonical" SortRegistry.SortRegistryBaseNotCanonical (decode (Serialize.encode (domain, first : entries, retirements)))
    [] -> assertFailure "registry codec fixture omitted primordial definitions"
  assertLeft "duplicate retirement coordinates are not canonical" SortRegistry.SortRegistryBaseNotCanonical (decode (Serialize.encode (domain, entries, retirements <> retirements)))
  assertLeft "retirement release must be positive" SortRegistry.SortRegistryBaseInvalidFacts (decode (mutateRetirement (\(sortId, digest, occurrence, original, _, successor) -> (sortId, digest, occurrence, original, 0, successor))))
  assertLeft "retirement digest must name the immutable descriptor" SortRegistry.SortRegistryBaseInvalidFacts (decode (mutateRetirement (\(sortId, _, occurrence, original, index, successor) -> (sortId, canonicalDescriptorDigestBytes (deriveCanonicalDescriptorDigest alternateDescriptor), occurrence, original, index, successor))))
  assertLeft "successor occurrence must derive from its release" SortRegistry.SortRegistryBaseInvalidFacts (decode (mutateRetirement (\(sortId, digest, occurrence, original, index, _) -> (sortId, digest, occurrence, original, index, sortDefinitionOccurrenceIdBytes genesisOccurrence))))
  let (_, activeEntries, _) = checkedPure "decode active definition rows" (Serialize.decode (SortRegistry.encodeSortRegistryBase (SortRegistry.captureSortRegistryBase registryWithDefinition)) :: Either String RawRegistryBaseFixture)
  mapM_
    ( \role -> do
        let claimPrimordial row@(tag, descriptor, occurrence, publication) = case tag of
              Nothing -> (Just role, descriptor, occurrence, publication)
              Just _ -> row
        reject "an additional definition claiming an existing primordial role" (Serialize.encode (domain, map claimPrimordial activeEntries, [] :: [RawRegistryRetirementFixture]))
    )
    [role | (Just role, _, _, _) <- entries]
  assertLeft "a retired definition cannot be restored as effective" SortRegistry.SortRegistryBaseInvalidFacts (decode (Serialize.encode (domain, activeEntries, retirements)))
  assertEqual "the canonical transcript is stable after rejected mutations" bytes (Serialize.encode raw)

caseRegistryBaseBufferOwnership :: Assertion
caseRegistryBaseBufferOwnership = do
  let fresh = SortRegistry.initialState fixtureCheckedGenesis
      originalPublication = primordialReplicaPublicationId sortDefinitionCarrier
      publisher = Identity.publicationSourceHeraldEpoch originalPublication
      structuralAuthority = Identity.structuralAuthorityEpoch (Identity.structuralOccurrenceId publisher Identity.firstStructuralSequence) (checkedPure "buffer fixture topology cut" (Identity.mkTopologyCutId (fixtureIdentifierBytes 0xe1)))
      identifier = Identity.publicationId (Identity.publicationNabla originalPublication) structuralAuthority publisher (nablaSequence 9)
      publication = checkedPure "buffer fixture definition publication" (mkCheckedPublication (primordialReplicaDescriptor sortDefinitionCarrier) identifier (sortDefinitionValue regularDescriptor))
      define occurrence state = SortRegistry.commitSortInduction (checkedPure "buffer fixture induction" (SortRegistry.prepareSortInduction regularDescriptor occurrence publication state))
      known = define genesisOccurrence fresh
      retired = retire (required "buffer fixture known definition" (SortRegistry.lookupEffectiveSort regularSortId known)) firstResolveIndex firstSuccessorOccurrence known
      unseenOccurrence = deriveSortDefinitionOccurrenceId systemId alternateSortId Genesis
      unseen = SortRegistry.commitRegularSortRetirement (checkedPure "buffer fixture unseen retirement" (SortRegistry.prepareUnseenRegularSortRetirement systemId alternateSortId (deriveCanonicalDescriptorDigest alternateDescriptor) unseenOccurrence (controlIndex 11) (successorOccurrence alternateSortId (controlIndex 11)) retired))
      source = define firstSuccessorOccurrence unseen
      -- Give the outer transcript one explicit allocation whose complete range
      -- is held alive throughout inspection. A weak ByteString wrapper would
      -- not detect another slice retaining this same ForeignPtr.
      bytes = ByteString.copy (SortRegistry.encodeSortRegistryBase (SortRegistry.captureSortRegistryBase source))
      decoded = checkedPure "buffer ownership decode" (SortRegistry.decodeSortRegistryBase fixtureCheckedGenesis bytes)
      imported = SortRegistry.commitSortRegistryBaseImport (checkedPure "buffer ownership import" (SortRegistry.prepareSortRegistryBaseImport decoded fresh))
      retirements = SortRegistry.regularSortRetirements imported
      entries = SortRegistry.registryEntries imported <> [retained | retirement <- retirements, Just retained <- [SortRegistry.regularSortRetirementEntry retirement]]
      retainedBuffers = concatMap entryBuffers entries <> concatMap retirementBuffers retirements
      (inputBuffer, inputOffset, inputLength) = ByteStringInternal.toForeignPtr bytes
  assertBool "fixture exercises structural publication-authority buffers" (any ((> 0) . length . authorityBuffers . Identity.publicationAuthorityEpoch . SortRegistry.registryEntryPublicationId) entries)
  withForeignPtr inputBuffer $ \inputPointer -> do
    let inputStart = ptrToIntPtr inputPointer + fromIntegral inputOffset
        inputEnd = inputStart + fromIntegral inputLength
    mapM_
      ( \(name, retained) -> do
          let (buffer, offset, count) = ByteStringInternal.toForeignPtr retained
          withForeignPtr buffer $ \pointer -> do
            let start = ptrToIntPtr pointer + fromIntegral offset
                end = start + fromIntegral count
            assertBool (name <> " still shares the outer registry transcript allocation") (end <= inputStart || start >= inputEnd)
      )
      retainedBuffers
  assertBool "buffer detachment preserves exact registry facts" (SortRegistry.clearAlignmentPlanChange imported == SortRegistry.clearAlignmentPlanChange source)
  where
    entryBuffers retained =
      let identifier = SortRegistry.registryEntryPublicationId retained
       in [ ("descriptor", canonicalDescriptorBytes (SortRegistry.registryEntryDescriptor retained)),
            ("descriptor identity", Identity.sortIdBytes (SortRegistry.registryEntrySortId retained)),
            ("definition occurrence", sortDefinitionOccurrenceIdBytes (SortRegistry.registryEntryOccurrenceId retained)),
            ("publication writer", Identity.nablaIdBytes (Identity.publicationNabla identifier)),
            ("publication source", Identity.heraldEpochBytes (Identity.publicationSourceHeraldEpoch identifier))
          ]
            <> authorityBuffers (Identity.publicationAuthorityEpoch identifier)
    authorityBuffers :: Identity.AuthorityEpoch -> [(String, ByteString)]
    authorityBuffers authority = case Identity.authorityEpochView authority of
      Identity.StructuralAuthorityEpochView occurrence cut ->
        [ ("authority occurrence source", Identity.heraldEpochBytes (Identity.structuralOccurrenceSourceHeraldEpoch occurrence)),
          ("authority topology cut", Identity.topologyCutIdBytes cut)
        ]
      _ -> []
    retirementBuffers retirement =
      [ ("retired sort identity", Identity.sortIdBytes (SortRegistry.regularSortRetirementSortId retirement)),
        ("retired descriptor digest", canonicalDescriptorDigestBytes (SortRegistry.regularSortRetirementDescriptorDigest retirement)),
        ("retired occurrence", sortDefinitionOccurrenceIdBytes (SortRegistry.regularSortRetirementOccurrenceId retirement)),
        ("retirement successor", sortDefinitionOccurrenceIdBytes (SortRegistry.regularSortRetirementSuccessorOccurrenceId retirement))
      ]

caseInductionPlans :: Assertion
caseInductionPlans = do
  genesisPlan <-
    checkedIO
      "genesis induction plan"
      ( SortRegistry.planSortInduction
          systemId
          regularDescriptor
          (SortRegistry.initialState fixtureCheckedGenesis)
      )
  let retired = retireFirst registryWithDefinition
  successorPlan <-
    checkedIO
      "successor induction plan"
      (SortRegistry.planSortInduction systemId regularDescriptor retired)
  assertEqual
    "genesis derives the genesis occurrence"
    genesisOccurrence
    (SortRegistry.sortInductionPlanOccurrenceId genesisPlan)
  assertEqual
    "genesis has no Oracle prerequisite"
    (controlIndex 0)
    (SortRegistry.sortInductionPlanControlPrerequisite genesisPlan)
  assertEqual
    "the retired epoch derives a new occurrence"
    firstSuccessorOccurrence
    (SortRegistry.sortInductionPlanOccurrenceId successorPlan)
  assertBool
    "retirement changes the occurrence coordinate"
    ( firstSuccessorOccurrence
        /= SortRegistry.sortInductionPlanOccurrenceId genesisPlan
    )
  assertEqual
    "the resolving prefix releases the successor"
    firstResolveIndex
    (SortRegistry.sortInductionPlanControlPrerequisite successorPlan)

caseStaleOccurrenceRejected :: Assertion
caseStaleOccurrenceRejected = do
  let retired = retireFirst registryWithDefinition
      redefined = induce regularDescriptor firstSuccessorOccurrence retired
      expected =
        SortRegistry.SortInductionRetiredOccurrence
          regularSortId
          genesisOccurrence
  assertLeft
    "stale induction immediately after retirement"
    expected
    ( SortRegistry.prepareSortInduction
        regularDescriptor
        genesisOccurrence
        definitionPublication
        retired
    )
  assertLeft
    "stale induction after the equal successor is effective"
    expected
    ( SortRegistry.prepareSortInduction
        regularDescriptor
        genesisOccurrence
        definitionPublication
        redefined
    )
  assertEqual
    "the successor remains effective"
    (Just firstSuccessorOccurrence)
    ( SortRegistry.registryEntryOccurrenceId
        <$> SortRegistry.lookupEffectiveSort regularSortId redefined
    )

caseEqualSuccessorInduction :: Assertion
caseEqualSuccessorInduction = do
  let retired = retireFirst registryWithDefinition
      once = induce regularDescriptor firstSuccessorOccurrence retired
      twice = induce regularDescriptor firstSuccessorOccurrence once
      entry =
        required
          "effective successor"
          (SortRegistry.lookupEffectiveSort regularSortId once)
  assertEqual
    "the equal descriptor is restored"
    regularDescriptor
    (SortRegistry.registryEntryDescriptor entry)
  assertEqual
    "the new effective entry uses the planned successor occurrence"
    firstSuccessorOccurrence
    (SortRegistry.registryEntryOccurrenceId entry)
  assertBool "same-occurrence repetition is a complete no-op" (twice == once)
  assertEqual
    "redefinition does not consume immutable retirement history"
    1
    (length (SortRegistry.regularSortRetirements twice))

caseWrongSuccessorDetected :: Assertion
caseWrongSuccessorDetected = do
  let wrongSuccessor = successorOccurrence regularSortId (controlIndex 8)
  assertLeft
    "the authority coordinate must agree with deployment-and-Resolve derivation"
    ( SortRegistry.RegularSortRetirementSuccessorOccurrenceMismatch
        regularSortId
        firstSuccessorOccurrence
        wrongSuccessor
    )
    ( SortRegistry.prepareExactRegularSortRetirement
        systemId
        effectiveEntry
        firstResolveIndex
        wrongSuccessor
        registryWithDefinition
    )
  assertEqual
    "rejection leaves retirement history empty"
    []
    (SortRegistry.regularSortRetirements registryWithDefinition)

caseSecondRetirementChain :: Assertion
caseSecondRetirementChain = do
  let afterFirstRetirement = retireFirst registryWithDefinition
      afterFirstRedefinition =
        induce
          regularDescriptor
          firstSuccessorOccurrence
          afterFirstRetirement
      firstSuccessorEntry =
        required
          "first successor entry"
          ( SortRegistry.lookupEffectiveSort
              regularSortId
              afterFirstRedefinition
          )
      secondIndex = controlIndex 13
      secondOccurrence = successorOccurrence regularSortId secondIndex
      afterSecondRetirement =
        retire
          firstSuccessorEntry
          secondIndex
          secondOccurrence
          afterFirstRedefinition
      afterSecondRedefinition =
        induce regularDescriptor secondOccurrence afterSecondRetirement
      latest =
        required
          "latest regular retirement"
          ( SortRegistry.latestRegularSortRetirement
              regularSortId
              afterSecondRedefinition
          )
  assertEqual
    "both retired occurrence identities remain suppressed"
    [Just firstResolveIndex, Just secondIndex]
    [ SortRegistry.regularSortRetirementResolveIndex
        <$> SortRegistry.lookupRegularSortRetirement
          regularSortId
          occurrence
          afterSecondRedefinition
    | occurrence <- [genesisOccurrence, firstSuccessorOccurrence]
    ]
  assertEqual
    "the later retirement is selected as latest"
    secondIndex
    (SortRegistry.regularSortRetirementResolveIndex latest)
  assertEqual
    "the second equal redefinition uses the second successor occurrence"
    (Just secondOccurrence)
    ( SortRegistry.registryEntryOccurrenceId
        <$> SortRegistry.lookupEffectiveSort
          regularSortId
          afterSecondRedefinition
    )
  assertLeft
    "the immediately preceding occurrence also remains stale"
    ( SortRegistry.SortInductionRetiredOccurrence
        regularSortId
        firstSuccessorOccurrence
    )
    ( SortRegistry.prepareSortInduction
        regularDescriptor
        firstSuccessorOccurrence
        definitionPublication
        afterSecondRedefinition
    )

-- A canonical descriptor carries its SHA-256 content address, so well-formed
-- code cannot manufacture the unequal-descriptor/same-SortId collision needed
-- to reach 'SortInductionIdentityContradiction'.  This is the corresponding
-- positive law: an unequal descriptor owns a distinct identity and neither its
-- plan nor its induction rewrites the retirement chain for the first identity.
caseDistinctDescriptorIdentity :: Assertion
caseDistinctDescriptorIdentity = do
  let firstRetired = retireFirst registryWithDefinition
      alternatePlan =
        checkedPure
          "alternate genesis plan"
          (SortRegistry.planSortInduction systemId alternateDescriptor firstRetired)
      withAlternate =
        induce
          alternateDescriptor
          (SortRegistry.sortInductionPlanOccurrenceId alternatePlan)
          firstRetired
  assertBool
    "unequal canonical descriptors have distinct content addresses"
    (alternateSortId /= regularSortId)
  assertEqual
    "the unrelated definition starts in its genesis epoch"
    (controlIndex 0)
    (SortRegistry.sortInductionPlanControlPrerequisite alternatePlan)
  assertEqual
    "the retired identity stays absent"
    Nothing
    (SortRegistry.lookupEffectiveSort regularSortId withAlternate)
  assertEqual
    "the unrelated identity becomes effective"
    (Just alternateDescriptor)
    ( SortRegistry.registryEntryDescriptor
        <$> SortRegistry.lookupEffectiveSort alternateSortId withAlternate
    )
  assertEqual
    "the first identity's retirement history is unchanged"
    1
    (length (SortRegistry.regularSortRetirements withAlternate))

prepareFirstRetirement ::
  SortRegistry.State ->
  Either
    SortRegistry.RegularSortRetirementError
    SortRegistry.PreparedRegularSortRetirement
prepareFirstRetirement =
  SortRegistry.prepareExactRegularSortRetirement
    systemId
    effectiveEntry
    firstResolveIndex
    firstSuccessorOccurrence

retireFirst :: SortRegistry.State -> SortRegistry.State
retireFirst =
  retire effectiveEntry firstResolveIndex firstSuccessorOccurrence

retire ::
  SortRegistry.RegistryEntry ->
  ControlIndex ->
  SortDefinitionOccurrenceId ->
  SortRegistry.State ->
  SortRegistry.State
retire entry index successor state =
  SortRegistry.commitRegularSortRetirement
    ( checkedPure
        "regular retirement"
        ( SortRegistry.prepareExactRegularSortRetirement
            systemId
            entry
            index
            successor
            state
        )
    )

induce ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  SortRegistry.State ->
  SortRegistry.State
induce descriptor occurrence state =
  SortRegistry.commitSortInduction
    ( checkedPure
        "sort induction"
        ( SortRegistry.prepareSortInduction
            descriptor
            occurrence
            (definitionPublicationFor descriptor)
            state
        )
    )

registryWithDefinition :: SortRegistry.State
registryWithDefinition =
  induce
    regularDescriptor
    genesisOccurrence
    (SortRegistry.initialState fixtureCheckedGenesis)

effectiveEntry :: SortRegistry.RegistryEntry
effectiveEntry =
  required
    "effective regular definition"
    (SortRegistry.lookupEffectiveSort regularSortId registryWithDefinition)

systemId :: SystemId
systemId = checkedSystemId fixtureCheckedGenesis

regularSortId, alternateSortId :: SortId
regularSortId = descriptorSortId regularDescriptor
alternateSortId = descriptorSortId alternateDescriptor

genesisOccurrence :: SortDefinitionOccurrenceId
genesisOccurrence =
  deriveSortDefinitionOccurrenceId systemId regularSortId Genesis

firstResolveIndex :: ControlIndex
firstResolveIndex = controlIndex 7

firstSuccessorOccurrence :: SortDefinitionOccurrenceId
firstSuccessorOccurrence = successorOccurrence regularSortId firstResolveIndex

successorOccurrence ::
  SortId ->
  ControlIndex ->
  SortDefinitionOccurrenceId
successorOccurrence sortId resolveIndex =
  deriveSortDefinitionOccurrenceId
    systemId
    sortId
    ( checkedPure
        "resolved-retirement occurrence base"
        (resolvedRetirementOccurrenceBase resolveIndex)
    )

regularDescriptor, alternateDescriptor :: CanonicalDescriptor
regularDescriptor = admittedDescriptor "key"
alternateDescriptor = admittedDescriptor "alternate_key"

admittedDescriptor :: Text -> CanonicalDescriptor
admittedDescriptor field =
  admittedApplicationSortDescriptor
    ( checkedPure
        "regular application descriptor"
        ( admitApplicationSortDefinition
            ( DeclaredSortDefinition
                ApplicationSortDescriptor
                  { SortSyntax.sortKind = RegularSort,
                    SortSyntax.valueSchema =
                      RecordSchema (Map.singleton field BoolSchema),
                    SortSyntax.keyProjections =
                      [ApplicationProjection (field :| [])],
                    SortSyntax.validityPredicate = AlwaysPredicate,
                    SortSyntax.obsolescencePredicate = NeverPredicate,
                    SortSyntax.rankTerms =
                      RankApplicationValue Ascending :| [],
                    SortSyntax.minimumRetentionMicros = 0,
                    SortSyntax.isImmutable = False,
                    SortSyntax.labelField = Nothing
                  }
                Nothing
            )
        )
    )

definitionPublication :: CheckedPublication
definitionPublication = definitionPublicationFor regularDescriptor

definitionPublicationFor :: CanonicalDescriptor -> CheckedPublication
definitionPublicationFor descriptor =
  checkedPure
    "checked regular sort-definition publication"
    ( mkCheckedPublication
        (primordialReplicaDescriptor sortDefinitionCarrier)
        (primordialReplicaPublicationId sortDefinitionCarrier)
        (sortDefinitionValue descriptor)
    )

sortDefinitionCarrier :: PrimordialDefinitionReplica
sortDefinitionCarrier =
  required
    "primordial sort-definition carrier"
    ( find
        ((== SortDefinitionRole) . primordialReplicaRole)
        (checkedPrimordialReplicas fixtureCheckedGenesis)
    )

assertLeft ::
  (Eq problem, Show problem) =>
  String ->
  problem ->
  Either problem value ->
  Assertion
assertLeft description expected result =
  case result of
    Left actual -> assertEqual description expected actual
    Right _ -> assertFailure (description <> ": expected rejection")

required :: String -> Maybe value -> value
required description =
  maybe (error (description <> ": missing fixture value")) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO description =
  either (assertFailure . ((description <> ": ") <>) . show) pure

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure description =
  either (error . ((description <> ": ") <>) . show) id
