module VisibilityProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( GlobalObjectId,
    LabelDecisionId,
    genesisAuthorityEpoch,
    mkGlobalObjectId,
    mkLabelDecisionId,
  )
import Eclips.Herald.Visibility.State
  ( HoldClassification (..),
    State,
    VisibilityDependency (..),
    VisibilityInvariantViolation (..),
    VisibilityProblem (..),
    classifyOwnerHold,
    commitBatchInstall,
    commitBatchRelease,
    initialState,
    prepareBatchInstall,
    prepareBatchRelease,
    preparedBatchNewlyHeldOwnerKeys,
    preparedBatchReleasedOwnerKeys,
    validateVisibilityState,
    visibilityDependenciesFor,
    visibilityDependencyRelations,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    forAll,
    shuffle,
    sublistOf,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "detached visibility owner"
    [ testCase
        "batch install retains exact canonical dependency relations and no payload"
        caseBatchInstall,
      testCase
        "release makes only owners without another blocker eligible"
        caseBatchRelease,
      testCase
        "a hold whose stable owner escaped is an invariant fault"
        caseMissingOwner,
      testProperty
        "dependency order and duplicates do not change an installed hold"
        propCanonicalInstall,
      testProperty
        "an owner is released exactly when its final dependency is released"
        propReleaseEligibility
    ]

caseBatchInstall :: Assertion
caseBatchInstall = do
  let owners = Set.fromList [ownerA, ownerB, ownerC]
      predecessor = initialState :: State FixtureOwnerKey
      requests =
        Map.fromList
          [ ( ownerA,
              fenceA :| [authorityA, targetA, fenceA]
            ),
            (ownerB, targetA :| [])
          ]
  assertEqual
    "a retained owner without a relation is unheld"
    OwnerUnheld
    (classifyOwnerHold owners ownerA predecessor)
  assertEqual
    "an unknown owner is distinct from unheld retained work"
    OwnerMissing
    (classifyOwnerHold owners missingOwner predecessor)
  prepared <-
    checked
      "install exact visibility batch"
      (prepareBatchInstall requests owners predecessor)
  assertEqual
    "both previously unheld owner keys become held"
    (Set.fromList [ownerA, ownerB])
    (preparedBatchNewlyHeldOwnerKeys prepared)
  let (installed, newlyHeld) = commitBatchInstall prepared
  assertEqual
    "commit exposes the exact prepared newly-held keys"
    (preparedBatchNewlyHeldOwnerKeys prepared)
    newlyHeld
  assertEqual
    "duplicate dependency facts normalize to one exact relation"
    (Set.fromList [fenceA, authorityA, targetA])
    (visibilityDependenciesFor ownerA installed)
  assertEqual
    "the witness contains only stable owner keys and semantic dependencies"
    ( Map.fromList
        [ (ownerA, canonicalDependencies [fenceA, authorityA, targetA]),
          (ownerB, targetA :| [])
        ]
    )
    (visibilityDependencyRelations installed)
  assertEqual
    "the exact blockers classify the owner as held"
    (OwnerHeld (canonicalDependencies [fenceA, authorityA, targetA]))
    (classifyOwnerHold owners ownerA installed)
  repeated <-
    checked
      "repeat equal visibility batch"
      (prepareBatchInstall requests owners installed)
  assertEqual
    "an equal reinstall creates no second hold transition"
    Set.empty
    (preparedBatchNewlyHeldOwnerKeys repeated)
  assertEqual
    "an equal reinstall is state-identical"
    installed
    (fst (commitBatchInstall repeated))
  assertEqual
    "installed state resolves every hold to one retained owner key"
    (Right ())
    (validateVisibilityState owners installed)

caseBatchRelease :: Assertion
caseBatchRelease = do
  let owners = Set.fromList [ownerA, ownerB, ownerC]
      requests =
        Map.fromList
          [ (ownerA, fenceA :| [authorityA, targetA]),
            (ownerB, targetA :| [])
          ]
  installed <-
    fst . commitBatchInstall
      <$> checked
        "install release fixture"
        (prepareBatchInstall requests owners initialState)
  partial <-
    checked
      "release fence and target dependencies"
      (prepareBatchRelease (Set.fromList [fenceA, targetA]) owners installed)
  assertEqual
    "only the owner whose final blocker disappeared is released"
    (Set.singleton ownerB)
    (preparedBatchReleasedOwnerKeys partial)
  let (partiallyReleased, newlyReleased) = commitBatchRelease partial
  assertEqual
    "commit exposes the exact prepared release set"
    (preparedBatchReleasedOwnerKeys partial)
    newlyReleased
  assertEqual
    "an unrelated authority dependency keeps its owner held"
    (OwnerHeld (authorityA :| []))
    (classifyOwnerHold owners ownerA partiallyReleased)
  assertEqual
    "the target-only owner is now eligible"
    OwnerUnheld
    (classifyOwnerHold owners ownerB partiallyReleased)
  final <-
    checked
      "release remaining authority dependency"
      (prepareBatchRelease (Set.singleton authorityA) owners partiallyReleased)
  assertEqual
    "the final blocker releases its exact owner"
    (Set.singleton ownerA)
    (preparedBatchReleasedOwnerKeys final)
  let (released, _) = commitBatchRelease final
  assertEqual
    "release consumes every completed relation"
    (initialState :: State FixtureOwnerKey)
    released
  assertEqual
    "an unrelated retained owner remains unheld throughout"
    OwnerUnheld
    (classifyOwnerHold owners ownerC released)

caseMissingOwner :: Assertion
caseMissingOwner = do
  let onlyA = Set.singleton ownerA
      predecessor = initialState :: State FixtureOwnerKey
  assertProblem
    "install cannot create an unresolved hold"
    ( VisibilityInvariantProblem
        (VisibilityHoldsMissingOwners (Set.singleton ownerB))
    )
    ( prepareBatchInstall
        (Map.singleton ownerB (targetA :| []))
        onlyA
        predecessor
    )
  prepared <-
    checked
      "install retained owner hold"
      ( prepareBatchInstall
          (Map.singleton ownerA (targetA :| []))
          onlyA
          predecessor
      )
  let held = fst (commitBatchInstall prepared)
  assertEqual
    "removing held work from its semantic owner is detected"
    ( Left
        (VisibilityHoldsMissingOwners (Set.singleton ownerA))
    )
    (validateVisibilityState Set.empty held)
  assertEqual
    "a vanished owner cannot be mistaken for eligible work"
    OwnerMissing
    (classifyOwnerHold Set.empty ownerA held)

propCanonicalInstall :: Property
propCanonicalInstall =
  forAll (shuffle exactDependencies) $ \permutation ->
    let owners = Set.singleton ownerA
        duplicated = permutation <> permutation
        requested =
          Map.singleton ownerA (NonEmpty.fromList duplicated)
     in case prepareBatchInstall requested owners initialState of
          Left problem -> counterexample (show problem) False
          Right prepared ->
            let (installed, _) = commitBatchInstall prepared
             in visibilityDependenciesFor ownerA installed
                  === Set.fromList exactDependencies

propReleaseEligibility :: Property
propReleaseEligibility =
  forAll (sublistOf exactDependencies) $ \releasedList ->
    let owners = Set.singleton ownerA
        requested =
          Map.singleton ownerA (NonEmpty.fromList exactDependencies)
        releasedDependencies = Set.fromList releasedList
        remaining =
          Set.fromList exactDependencies
            `Set.difference` releasedDependencies
     in case prepareBatchInstall requested owners initialState of
          Left problem -> counterexample (show problem) False
          Right install ->
            let (installed, _) = commitBatchInstall install
             in case prepareBatchRelease releasedDependencies owners installed of
                  Left problem -> counterexample (show problem) False
                  Right release ->
                    let (successor, newlyReleased) = commitBatchRelease release
                        expectedClassification =
                          case NonEmpty.nonEmpty (Set.toAscList remaining) of
                            Nothing -> OwnerUnheld
                            Just dependencies -> OwnerHeld dependencies
                        expectedReleased
                          | Set.null remaining = Set.singleton ownerA
                          | otherwise = Set.empty
                     in ( classifyOwnerHold owners ownerA successor,
                          newlyReleased
                        )
                          === (expectedClassification, expectedReleased)

newtype FixtureOwnerKey = FixtureOwnerKey Int
  deriving stock (Eq, Ord, Show)

ownerA, ownerB, ownerC, missingOwner :: FixtureOwnerKey
ownerA = FixtureOwnerKey 1
ownerB = FixtureOwnerKey 2
ownerC = FixtureOwnerKey 3
missingOwner = FixtureOwnerKey 99

fenceA, authorityA, targetA :: VisibilityDependency
fenceA = LabelFenceDependency decisionA
authorityA = AuthorityDependency objectA genesisAuthorityEpoch
targetA = TargetDependency decisionA objectA

exactDependencies :: [VisibilityDependency]
exactDependencies = [fenceA, authorityA, targetA]

decisionA :: LabelDecisionId
decisionA =
  checkedValue
    "label decision"
    (mkLabelDecisionId (fixtureBytes 0x31))

objectA :: GlobalObjectId
objectA =
  checkedValue
    "global object"
    (mkGlobalObjectId (fixtureBytes 0x71))

canonicalDependencies :: [VisibilityDependency] -> NonEmpty VisibilityDependency
canonicalDependencies = NonEmpty.fromList . Set.toAscList . Set.fromList

fixtureBytes :: Word -> ByteString.ByteString
fixtureBytes seed =
  ByteString.pack
    [ fromIntegral (seed + index)
    | index <- [(0 :: Word) .. 31]
    ]

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (assertFailure . ((context <> ": ") <>) . show) pure

checkedValue :: (Show problem) => String -> Either problem value -> value
checkedValue context = either (error . ((context <> ": ") <>) . show) id

assertProblem ::
  (Eq problem, Show problem) =>
  String ->
  problem ->
  Either problem value ->
  Assertion
assertProblem context expected result = case result of
  Left actual -> assertEqual context expected actual
  Right _ -> assertFailure (context <> ": preparation unexpectedly succeeded")
