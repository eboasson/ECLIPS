{-# LANGUAGE OverloadedStrings #-}

module StoreProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Foldable (foldlM)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word8)
import DescriptorProperties
  ( emptyKeyDescriptor,
    regularDescriptor,
    regularDescriptorSpec,
    regularValue,
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    NablaId,
    PublicationId,
    SortId,
    genesisAuthorityEpoch,
    mkHeraldEpoch,
    mkNablaId,
    nablaSequence,
    publicationId,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationWinnerKey,
    mkCheckedPublication,
    sameLogicalState,
  )
import Eclips.Domain.Route (ReplicaStrength (..))
import Eclips.Domain.Sort.Canonical
  ( canonicalizeDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( DescriptorAdmission (..),
    DescriptorSpec (..),
  )
import Eclips.Domain.Store
  ( DeltaStore,
    RetainedApplyResult (..),
    RetainedTransition,
    StoreError (..),
    applyPublication,
    applyPublicationDetailed,
    applyRetainedSnapshot,
    emptyDeltaStore,
    localTake,
    lookupRetained,
    lookupVisible,
    mkRetainedSnapshot,
    purgeObjectKey,
    retainedInstances,
    retainedSnapshot,
    retainedSnapshotLogicalStrengthFacts,
    retainedSnapshotWinnerFacts,
    retainedTransitionLogicalFactAfter,
    retainedTransitionLogicalFactBefore,
    retainedTransitionPublication,
    retainedTransitionWinnerAfter,
    retainedTransitionWinnerBefore,
    storedPublication,
    storedStrength,
    visibleInstances,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    counterexample,
    forAll,
    listOf,
    resize,
    shuffle,
    testProperty,
    (.&&.),
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "publication winner and delta store"
    [ testGroup
        "winner order"
        [ testCase "ordinary publications have no carrier intrinsic" caseOrdinaryPublication,
          testProperty "winner comparison is antisymmetric" propWinnerAntisymmetric,
          testProperty "winner comparison is transitive" propWinnerTransitive,
          testCase "logical state excludes publication provenance" caseLogicalState
        ],
      testGroup
        "convergence"
        [ testCase "duplicate application equals one application" caseDuplicate,
          testCase "an empty key list retains one object" caseEmptyKeySingleObject,
          testProperty "every permutation retains the same state" propPermutation,
          testProperty "obsolete defeats current in either order" propObsoleteDominance,
          testCase "same-publication strength observations converge" caseStrengthObservationOrder
        ],
      testGroup
        "visibility and strength"
        [ testCase "local take removes visible but preserves retained" caseLocalTake,
          testCase "equal and older replay remain hidden" caseReplaySuppression,
          testCase "genuinely newer publication reappears" caseNewerReappears,
          testCase "weak input never creates normal possession" caseWeakStaysWeak,
          testCase "equal logical state upgrades weak to normal" caseStrengthUpgrade,
          testCase "fresh weak provenance cannot downgrade visible normal" caseNoDowngrade,
          testCase "strength-only upgrade does not re-expose a take" caseHiddenUpgrade,
          testCase "newer weak provenance re-exposes weak, not hidden normal" caseWeakReappearance,
          testCase "terminal purge removes every projection and is idempotent" caseTerminalPurge
        ],
      testGroup
        "retained transitions"
        [ testCase "fresh and replacement winners report exact transitions" caseWinnerTransitions,
          testCase "obsolete suppression reports a winner transition" caseSuppressionTransition,
          testCase "strength-only upgrade reports both retained projections" caseStrengthTransition,
          testCase "duplicate reports no retained change" caseDuplicateTransition,
          testCase "lower losing observation reports no retained change" caseLosingNoOp,
          testCase "greater losing witness advances the logical projection" caseLosingWitnessAdvance
        ],
      testGroup
        "retained snapshots"
        [ testProperty "snapshot reconstruction is fact-order independent" propSnapshotOrderIndependent,
          testProperty "snapshot capture and checked reconstruction round trip" propSnapshotRoundtrip,
          testCase "snapshot merge replays through the ordinary winner law" caseSnapshotMerge,
          testProperty "newer live data commutes with an older snapshot" propNewerLiveSnapshotOrder
        ],
      testGroup
        "contradictions"
        [testCase "one store accepts only its fixed sort" caseSortMismatch]
    ]

caseOrdinaryPublication :: IO ()
caseOrdinaryPublication =
  assertBool
    "ordinary checked publication succeeds"
    ( either
        (const False)
        (const True)
        ( mkCheckedPublication
            regularDescriptor
            (fixturePublicationId 1)
            (regularValue 1 1 False "ordinary")
        )
    )

propWinnerAntisymmetric :: Word16 -> Bool -> Word16 -> Word16 -> Bool -> Word16 -> Property
propWinnerAntisymmetric leftRevision leftObsolete leftSequence rightRevision rightObsolete rightSequence =
  compare leftKey rightKey === invertOrdering (compare rightKey leftKey)
  where
    leftKey = checkedPublicationWinnerKey (publicationFixture leftSequence (fromIntegral leftRevision) leftObsolete "left")
    rightKey = checkedPublicationWinnerKey (publicationFixture rightSequence (fromIntegral rightRevision) rightObsolete "right")

propWinnerTransitive ::
  Word16 ->
  Bool ->
  Word16 ->
  Word16 ->
  Bool ->
  Word16 ->
  Word16 ->
  Bool ->
  Word16 ->
  Property
propWinnerTransitive firstRevision firstObsolete firstSequence secondRevision secondObsolete secondSequence thirdRevision thirdObsolete thirdSequence =
  counterexample (show (firstKey, secondKey, thirdKey))
    $ not (firstKey <= secondKey && secondKey <= thirdKey) || firstKey <= thirdKey
  where
    firstKey = checkedPublicationWinnerKey (publicationFixture firstSequence (fromIntegral firstRevision) firstObsolete "first")
    secondKey = checkedPublicationWinnerKey (publicationFixture secondSequence (fromIntegral secondRevision) secondObsolete "second")
    thirdKey = checkedPublicationWinnerKey (publicationFixture thirdSequence (fromIntegral thirdRevision) thirdObsolete "third")

caseLogicalState :: IO ()
caseLogicalState = do
  let first = publicationFixture 1 7 False "same"
      second = publicationFixture 2 7 False "same"
  assertBool "different provenance" (checkedPublicationId first /= checkedPublicationId second)
  assertBool "same logical state" (sameLogicalState first second)

caseDuplicate :: IO ()
caseDuplicate = do
  let publication = publicationFixture 1 1 False "value"
      once = applyChecked Normal publication emptyStore
      twice = applyChecked Normal publication once
  assertEqual "duplicate" once twice

caseEmptyKeySingleObject :: IO ()
caseEmptyKeySingleObject = do
  let first =
        checked
          "first empty-key publication"
          ( mkCheckedPublication
              emptyKeyDescriptor
              (fixturePublicationId 1)
              (regularValue 1 1 False "first")
          )
      second =
        checked
          "second empty-key publication"
          ( mkCheckedPublication
              emptyKeyDescriptor
              (fixturePublicationId 2)
              (regularValue 2 2 False "second")
          )
      initial = emptyDeltaStore (descriptorSortId emptyKeyDescriptor)
      withFirst = checked "first empty-key application" (applyPublication Normal first initial)
      withSecond = checked "second empty-key application" (applyPublication Normal second withFirst)
  assertEqual "unit key" (checkedPublicationKey first) (checkedPublicationKey second)
  assertEqual "one visible object" 1 (length (visibleInstances withSecond))
  assertEqual
    "higher-ranked value wins that object"
    (Just second)
    (storedPublication <$> lookupVisible (checkedPublicationKey first) withSecond)

propPermutation :: Property
propPermutation =
  forAll generatedObservations $ \observations ->
    forAll (shuffle observations) $ \permutation ->
      counterexample (show observations)
        $ applyAll permutation === applyAll observations

generatedObservations :: Gen [(ReplicaStrength, CheckedPublication)]
generatedObservations = do
  raw <- resize 24 (listOf arbitrary)
  pure (zipWith observation [1 ..] raw)
  where
    observation sequenceNumber (revision, obsolete, payload, strong) =
      ( if strong then Normal else Weak,
        publicationFixture
          sequenceNumber
          (fromIntegral (revision :: Word8))
          obsolete
          (Text.pack (show (payload :: Word8)))
      )

propObsoleteDominance :: Word16 -> Word16 -> Property
propObsoleteDominance currentRevision obsoleteRevision =
  let current = publicationFixture 1 (fromIntegral currentRevision) False "current"
      obsolete = publicationFixture 2 (fromIntegral obsoleteRevision) True "obsolete"
      forward = applyAll [(Normal, current), (Weak, obsolete)]
      reverseOrder = applyAll [(Weak, obsolete), (Normal, current)]
      retainedPublication store = storedPublication <$> lookupRetained (checkedPublicationKey current) store
   in counterexample (show (forward, reverseOrder))
        $ (forward === reverseOrder)
          .&&. (retainedPublication forward === Just obsolete)

caseStrengthObservationOrder :: IO ()
caseStrengthObservationOrder = do
  let publication = publicationFixture 1 1 False "value"
      weakThenNormal = applyAll [(Weak, publication), (Normal, publication)]
      normalThenWeak = applyAll [(Normal, publication), (Weak, publication)]
  assertEqual "state" weakThenNormal normalThenWeak

caseLocalTake :: IO ()
caseLocalTake = do
  let publication = publicationFixture 1 1 False "value"
      key = checkedPublicationKey publication
      initial = applyChecked Normal publication emptyStore
      beforeSnapshot = retainedSnapshot initial
      (taken, afterTake) = localTake key initial
  assertEqual "returned publication" (Just publication) (storedPublication <$> taken)
  assertEqual "visible removed" Nothing (lookupVisible key afterTake)
  assertEqual "retained unchanged" (lookupRetained key initial) (lookupRetained key afterTake)
  assertEqual "retained collection" (retainedInstances initial) (retainedInstances afterTake)
  assertEqual "hidden snapshot" beforeSnapshot (retainedSnapshot afterTake)

caseReplaySuppression :: IO ()
caseReplaySuppression = do
  let older = publicationFixture 1 1 False "value"
      winner = publicationFixture 2 1 False "value"
      key = checkedPublicationKey winner
      beforeTake = applyAll [(Normal, older), (Normal, winner)]
      (_, hidden) = localTake key beforeTake
      exactReplay = applyChecked Normal winner hidden
      olderReplay = applyChecked Normal older exactReplay
  assertEqual "exact replay hidden" Nothing (lookupVisible key exactReplay)
  assertEqual "older replay hidden" Nothing (lookupVisible key olderReplay)

caseNewerReappears :: IO ()
caseNewerReappears = do
  let older = publicationFixture 1 1 False "value"
      newer = publicationFixture 2 1 False "value"
      key = checkedPublicationKey older
      (_, hidden) = localTake key (applyChecked Normal older emptyStore)
      reappeared = applyChecked Weak newer hidden
  assertEqual "new winner" (Just newer) (storedPublication <$> lookupVisible key reappeared)

caseWeakStaysWeak :: IO ()
caseWeakStaysWeak = do
  let publication = publicationFixture 1 1 False "value"
      stored = lookupVisible (checkedPublicationKey publication) (applyChecked Weak publication emptyStore)
  assertEqual "weak" (Just Weak) (storedStrength <$> stored)

caseStrengthUpgrade :: IO ()
caseStrengthUpgrade = do
  let publication = publicationFixture 1 1 False "value"
      key = checkedPublicationKey publication
      upgraded = applyAll [(Weak, publication), (Normal, publication)]
  assertEqual "visible" (Just Normal) (storedStrength <$> lookupVisible key upgraded)
  assertEqual "retained" (Just Normal) (storedStrength <$> lookupRetained key upgraded)

caseNoDowngrade :: IO ()
caseNoDowngrade = do
  let older = publicationFixture 1 1 False "value"
      newer = publicationFixture 2 1 False "value"
      key = checkedPublicationKey older
      store = applyAll [(Normal, older), (Weak, newer)]
  assertEqual "normal remains" (Just Normal) (storedStrength <$> lookupVisible key store)
  assertEqual "new provenance wins" (Just newer) (storedPublication <$> lookupVisible key store)

caseHiddenUpgrade :: IO ()
caseHiddenUpgrade = do
  let publication = publicationFixture 1 1 False "value"
      key = checkedPublicationKey publication
      (_, hidden) = localTake key (applyChecked Weak publication emptyStore)
      upgraded = applyChecked Normal publication hidden
  assertEqual "still hidden" Nothing (lookupVisible key upgraded)
  assertEqual "retained upgraded" (Just Normal) (storedStrength <$> lookupRetained key upgraded)

caseWeakReappearance :: IO ()
caseWeakReappearance = do
  let older = publicationFixture 1 1 False "value"
      newer = publicationFixture 2 1 False "value"
      key = checkedPublicationKey older
      (_, hidden) = localTake key (applyChecked Normal older emptyStore)
      reappeared = applyChecked Weak newer hidden
  assertEqual "visible incoming strength" (Just Weak) (storedStrength <$> lookupVisible key reappeared)
  assertEqual "retained historical strength" (Just Normal) (storedStrength <$> lookupRetained key reappeared)

caseTerminalPurge :: IO ()
caseTerminalPurge = do
  let older = publicationFixture 1 1 False "older"
      newer = publicationFixture 2 2 False "newer"
      key = checkedPublicationKey older
      predecessor = applyAll [(Normal, older), (Weak, newer)]
      (purged, removed) = purgeObjectKey key predecessor
      (duplicate, duplicateRemoved) = purgeObjectKey key purged
  assertEqual "one visible, one retained, and two logical facts removed" 4 removed
  assertEqual "visible winner removed" Nothing (lookupVisible key purged)
  assertEqual "retained winner removed" Nothing (lookupRetained key purged)
  assertEqual
    "logical-strength witnesses removed"
    []
    (retainedSnapshotLogicalStrengthFacts (retainedSnapshot purged))
  assertEqual "duplicate purge state" purged duplicate
  assertEqual "duplicate purge count" 0 duplicateRemoved

caseWinnerTransitions :: IO ()
caseWinnerTransitions = do
  let older = publicationFixture 1 1 False "older"
      newer = publicationFixture 2 2 False "newer"
      (withOlder, firstResult) = applyDetailedChecked Weak older emptyStore
      (withNewer, secondResult) = applyDetailedChecked Normal newer withOlder
      first = requireTransition "fresh winner" firstResult
      second = requireTransition "replacement winner" secondResult
  assertEqual "fresh observation" older (retainedTransitionPublication first)
  assertEqual "fresh before" Nothing (retainedTransitionWinnerBefore first)
  assertEqual "fresh after" (Just older) (storedPublication <$> retainedTransitionWinnerAfter first)
  assertEqual "replacement before" (Just older) (storedPublication <$> retainedTransitionWinnerBefore second)
  assertEqual "replacement after" (Just newer) (storedPublication <$> retainedTransitionWinnerAfter second)
  assertEqual "installed replacement" (Just newer) (storedPublication <$> lookupRetained (checkedPublicationKey newer) withNewer)

caseSuppressionTransition :: IO ()
caseSuppressionTransition = do
  let current = publicationFixture 1 100 False "current"
      obsolete = publicationFixture 2 1 True "obsolete"
      withCurrent = applyChecked Normal current emptyStore
      (suppressed, result) = applyDetailedChecked Weak obsolete withCurrent
      transition = requireTransition "obsolete suppression" result
  assertEqual "suppressed incumbent" (Just current) (storedPublication <$> retainedTransitionWinnerBefore transition)
  assertEqual "obsolete winner" (Just obsolete) (storedPublication <$> retainedTransitionWinnerAfter transition)
  assertEqual "retained obsolete" (Just obsolete) (storedPublication <$> lookupRetained (checkedPublicationKey obsolete) suppressed)

caseStrengthTransition :: IO ()
caseStrengthTransition = do
  let publication = publicationFixture 1 1 False "value"
      weak = applyChecked Weak publication emptyStore
      (_, result) = applyDetailedChecked Normal publication weak
      transition = requireTransition "strength upgrade" result
  assertEqual "winner before weak" (Just Weak) (storedStrength <$> retainedTransitionWinnerBefore transition)
  assertEqual "winner after normal" (Just Normal) (storedStrength <$> retainedTransitionWinnerAfter transition)
  assertEqual "logical before weak" (Just Weak) (storedStrength <$> retainedTransitionLogicalFactBefore transition)
  assertEqual "logical after normal" Normal (storedStrength (retainedTransitionLogicalFactAfter transition))

caseDuplicateTransition :: IO ()
caseDuplicateTransition = do
  let publication = publicationFixture 1 1 False "value"
      once = applyChecked Normal publication emptyStore
      (twice, result) = applyDetailedChecked Normal publication once
  assertEqual "store" once twice
  assertEqual "result" NoRetainedChange result

caseLosingNoOp :: IO ()
caseLosingNoOp = do
  let retainedLosingState = publicationFixture 2 1 False "losing-state"
      winner = publicationFixture 3 10 False "winner"
      lowerReplay = publicationFixture 1 1 False "losing-state"
      before = applyAll [(Normal, retainedLosingState), (Normal, winner)]
      (after, result) = applyDetailedChecked Weak lowerReplay before
  assertEqual "store" before after
  assertEqual "result" NoRetainedChange result

caseLosingWitnessAdvance :: IO ()
caseLosingWitnessAdvance = do
  let firstWitness = publicationFixture 1 1 False "losing-state"
      winner = publicationFixture 2 10 False "winner"
      greaterWitness = publicationFixture 3 1 False "losing-state"
      before = applyAll [(Normal, firstWitness), (Normal, winner)]
      (after, result) = applyDetailedChecked Weak greaterWitness before
      transition = requireTransition "logical witness advance" result
  assertEqual "winner unchanged" (retainedTransitionWinnerBefore transition) (retainedTransitionWinnerAfter transition)
  assertEqual "logical witness before" (Just firstWitness) (storedPublication <$> retainedTransitionLogicalFactBefore transition)
  assertEqual "logical witness after" greaterWitness (storedPublication (retainedTransitionLogicalFactAfter transition))
  assertBool "hidden snapshot changed" (retainedSnapshot before /= retainedSnapshot after)

propSnapshotOrderIndependent :: Property
propSnapshotOrderIndependent =
  forAll generatedObservations $ \observations ->
    let snapshot = retainedSnapshot (applyAll observations)
        facts = retainedSnapshotLogicalStrengthFacts snapshot
     in forAll (shuffle facts) $ \permutation ->
          checked "snapshot facts" (mkRetainedSnapshot regularSort permutation)
            === snapshot

propSnapshotRoundtrip :: Property
propSnapshotRoundtrip =
  forAll generatedObservations $ \observations ->
    let snapshot = retainedSnapshot (applyAll observations)
        rebuilt =
          checked
            "snapshot reconstruction"
            ( mkRetainedSnapshot
                regularSort
                (retainedSnapshotLogicalStrengthFacts snapshot)
            )
        (replayed, _) = checked "snapshot replay" (applyRetainedSnapshot rebuilt emptyStore)
     in counterexample (show snapshot)
          $ (rebuilt === snapshot)
            .&&. (retainedSnapshot replayed === snapshot)

caseSnapshotMerge :: IO ()
caseSnapshotMerge = do
  let sourceOlder = publicationFixture 1 1 False "source-older"
      sourceWinner = publicationFixture 3 3 False "source-winner"
      targetState = publicationFixture 2 2 False "target-state"
      source = applyAll [(Normal, sourceOlder), (Weak, sourceWinner)]
      target = applyChecked Normal targetState emptyStore
      snapshot = retainedSnapshot source
      (merged, transitions) = checked "snapshot merge" (applyRetainedSnapshot snapshot target)
      ordinary = applyAll [(Normal, targetState), (Normal, sourceOlder), (Weak, sourceWinner)]
  assertEqual "hidden merge" (retainedSnapshot ordinary) (retainedSnapshot merged)
  assertEqual "winner facts" (retainedSnapshotWinnerFacts (retainedSnapshot ordinary)) (retainedSnapshotWinnerFacts (retainedSnapshot merged))
  assertBool "reported retained work" (not (null transitions))

propNewerLiveSnapshotOrder :: Word16 -> Bool -> Bool -> Property
propNewerLiveSnapshotOrder revision oldStrong liveStrong =
  let older = publicationFixture 1 (fromIntegral revision) False "older"
      newer = publicationFixture 2 (fromIntegral revision + 1) False "newer"
      oldStrength = if oldStrong then Normal else Weak
      liveStrength = if liveStrong then Normal else Weak
      snapshot = retainedSnapshot (applyChecked oldStrength older emptyStore)
      (oldFirst, _) = checked "old snapshot first" (applyRetainedSnapshot snapshot emptyStore)
      oldThenLive = applyChecked liveStrength newer oldFirst
      liveFirst = applyChecked liveStrength newer emptyStore
      (liveThenOld, _) = checked "new live first" (applyRetainedSnapshot snapshot liveFirst)
   in counterexample
        (show (oldThenLive, liveThenOld))
        (oldThenLive === liveThenOld)

caseSortMismatch :: IO ()
caseSortMismatch = do
  let otherDescriptor =
        checked
          "other descriptor"
          ( canonicalizeDescriptor
              ApplicationDescriptor
              regularDescriptorSpec {descriptorSpecMinimumRetentionMicros = 1001}
          )
      otherPublication =
        checked
          "other publication"
          ( mkCheckedPublication
              otherDescriptor
              (fixturePublicationId 1)
              (regularValue 1 1 False "value")
          )
  assertEqual
    "sort mismatch"
    (Left (StoreSortMismatch (descriptorSortId regularDescriptor) (descriptorSortId otherDescriptor)))
    (applyPublication Normal otherPublication emptyStore)

emptyStore :: DeltaStore
emptyStore = emptyDeltaStore (descriptorSortId regularDescriptor)

regularSort :: SortId
regularSort = descriptorSortId regularDescriptor

applyAll :: [(ReplicaStrength, CheckedPublication)] -> DeltaStore
applyAll =
  checked "publication trace"
    . foldlM
      (\store (strength, publication) -> applyPublication strength publication store)
      emptyStore

applyChecked :: ReplicaStrength -> CheckedPublication -> DeltaStore -> DeltaStore
applyChecked strength publication store =
  checked "publication application" (applyPublication strength publication store)

applyDetailedChecked ::
  ReplicaStrength ->
  CheckedPublication ->
  DeltaStore ->
  (DeltaStore, RetainedApplyResult)
applyDetailedChecked strength publication store =
  checked
    "detailed publication application"
    (applyPublicationDetailed strength publication store)

requireTransition :: String -> RetainedApplyResult -> RetainedTransition
requireTransition label result = case result of
  NoRetainedChange -> error (label <> ": expected retained transition")
  RetainedChanged transition -> transition

publicationFixture :: Word16 -> Int64 -> Bool -> Text -> CheckedPublication
publicationFixture sequenceNumber revision obsolete payload =
  publicationWithId (fixturePublicationId sequenceNumber) revision obsolete payload

publicationWithId :: PublicationId -> Int64 -> Bool -> Text -> CheckedPublication
publicationWithId identifier revision obsolete payload =
  checked
    "publication"
    ( mkCheckedPublication
        regularDescriptor
        identifier
        (regularValue 1 revision obsolete payload)
    )

fixturePublicationId :: Word16 -> PublicationId
fixturePublicationId sequenceNumber =
  publicationId
    (fixtureNabla 1)
    genesisAuthorityEpoch
    (fixtureHerald 1)
    (nablaSequence (fromIntegral sequenceNumber))

fixtureNabla :: Word8 -> NablaId
fixtureNabla byte = checked "NablaId" (mkNablaId (ByteString.replicate 32 byte))

fixtureHerald :: Word8 -> HeraldEpoch
fixtureHerald byte = checked "HeraldEpoch" (mkHeraldEpoch (ByteString.replicate 32 byte))

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
