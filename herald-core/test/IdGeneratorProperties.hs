module IdGeneratorProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (digitToInt)
import Data.List (unfoldr)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Domain.Identity (globalUniqueIdBytes)
import Eclips.Herald.IdGenerator
  ( StartupSeedError (WrongGeneratorSeedByteCount),
    mkGeneratorSeed,
  )
import Eclips.Herald.IdGenerator.State
  ( IdGeneratorStateWitness (..),
    PositiveCountProblem (PositiveCountMustBePositive),
    State,
    commitGeneratedId,
    commitGeneratedIdRange,
    idGeneratorForbiddenPrefix,
    idGeneratorInitialPrefix,
    idGeneratorStateWitness,
    initialState,
    mkPositiveCount,
    positiveCountWord64,
    prepareGeneratedId,
    prepareGeneratedIdRange,
    preparedGlobalUniqueId,
    preparedGlobalUniqueIds,
  )
import GenesisFixtures
  ( fixtureGeneratorSeed,
    fixtureGeneratorSeedH1,
    fixtureGeneratorSeedH2,
    fixtureGeneratorSeedH3,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Positive (Positive),
    Property,
    arbitrary,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    property,
    testProperty,
    vectorOf,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "id generator"
    [ testProperty "seed admits exactly 32 bytes" propSeedWidth,
      testProperty "every admitted seed initializes at counter zero" propInitialStateTotal,
      testCase "fixed AES vectors and derived prefixes" fixedVectors,
      testCase "preparation is repeatable and counter starts at zero" repeatablePreparation,
      testCase "range count rejects zero" zeroRangeCount,
      testProperty "positive range counts round-trip" propPositiveRangeCount,
      testProperty "range equals repeated single-step preparation" propRangeEquivalence,
      testProperty "range is distinct under the non-wrapping premise" propRangeDistinct,
      testProperty "range exposes no partial state and has one exact commit point" propRangeCommit,
      testCase "finite fixed-key sequence is injective and excludes zero" finiteInjectivity,
      testCase "fixture keys produce distinct sampled sequences" distinctFixtureSequences,
      testCase "public seed rendering is opaque" seedRenderingIsOpaque
    ]

propSeedWidth :: Property
propSeedWidth =
  forAll (chooseInt (0, 64)) $ \width ->
    let result = mkGeneratorSeed (ByteString.replicate width 0x5a)
     in if width == 32
          then property (either (const False) (const True) result)
          else case result of
            Left (WrongGeneratorSeedByteCount expected actual) ->
              (expected, actual) === (32, width)
            Right _ -> property False

propInitialStateTotal :: Property
propInitialStateTotal =
  forAll (ByteString.pack <$> vectorOf 32 arbitrary) $ \bytes ->
    case mkGeneratorSeed bytes of
      Left problem -> counterexample ("exact-width seed rejected: " <> show problem) False
      Right seed -> case initialState seed of
        Left problem -> counterexample ("admitted seed failed to initialize: " <> show problem) False
        Right state ->
          witnessedNextGeneratorCounter (idGeneratorStateWitness state) === 0

fixedVectors :: Assertion
fixedVectors = do
  state0 <- checked "initialize default generator" (initialState fixtureGeneratorSeed)
  forbiddenExpected <- expectedHex "forbidden prefix fixture" "b48984cff1ab9a096d9f08eb2a2e277ab48984cff1ab9a09"
  prefixExpected <- expectedHex "admissible prefix fixture" "348984cff1ab9a096d9f08eb2a2e277ab48984cff1ab9a09"
  assertEqual
    "forbidden prefix"
    forbiddenExpected
    =<< checked "derive forbidden prefix" (idGeneratorForbiddenPrefix fixtureGeneratorSeed)
  assertEqual
    "admissible prefix"
    prefixExpected
    (idGeneratorInitialPrefix state0)
  (defaultOutputs, _) <- checked "default sequence" (generate 2 state0)
  defaultZeroExpected <- expectedHex "default counter-zero fixture" "92492786aba3cfbe5f98385f66e1bde5e5462c56615aa9631ae0a2321e5d4c1b"
  defaultOneExpected <- expectedHex "default counter-one fixture" "bc1397f6d70b952dd21e87596bad9a0ac45fb0120411fffb30acfb1a2c0ba3c8"
  case defaultOutputs of
    [defaultZero, defaultOne] -> do
      assertEqual "default counter zero" defaultZeroExpected defaultZero
      assertEqual "default counter one" defaultOneExpected defaultOne
    outputs -> assertFailure ("expected two default outputs, got " <> show (length outputs))
  a5Seed <- checked "admit a5 seed" (mkGeneratorSeed (ByteString.replicate 32 0xa5))
  a5State <- checked "initialize a5 generator" (initialState a5Seed)
  a5PrefixExpected <- expectedHex "a5 prefix fixture" "1873cd6aa6c05e289fb4a7a1ef1385629873cd6aa6c05e28"
  assertEqual
    "a5 admissible prefix"
    a5PrefixExpected
    (idGeneratorInitialPrefix a5State)
  (a5Outputs, _) <- checked "a5 sequence" (generate 1 a5State)
  a5Expected <- expectedHex "a5 counter-zero fixture" "7cbbbf3be0c2a47cbe40011299edee13db5724c69c7301cec1000005db32cad5"
  case a5Outputs of
    [a5Output] -> assertEqual "a5 counter zero" a5Expected a5Output
    outputs -> assertFailure ("expected one a5 output, got " <> show (length outputs))

repeatablePreparation :: Assertion
repeatablePreparation = do
  state <- checked "initialize generator" (initialState fixtureGeneratorSeed)
  assertEqual
    "initial counter"
    0
    (witnessedNextGeneratorCounter (idGeneratorStateWitness state))
  first <- checked "first preparation" (prepareGeneratedId state)
  repeated <- checked "repeated preparation" (prepareGeneratedId state)
  assertEqual
    "same predecessor produces same output"
    (preparedGlobalUniqueId first)
    (preparedGlobalUniqueId repeated)
  assertBool
    "same predecessor produces same successor"
    (commitGeneratedId first == commitGeneratedId repeated)
  assertEqual
    "predecessor remains at zero"
    0
    (witnessedNextGeneratorCounter (idGeneratorStateWitness state))
  assertEqual
    "committed successor advances once"
    1
    ( witnessedNextGeneratorCounter
        (idGeneratorStateWitness (commitGeneratedId first))
    )

zeroRangeCount :: Assertion
zeroRangeCount =
  assertEqual
    "zero is not a non-empty range"
    (Left PositiveCountMustBePositive)
    (mkPositiveCount 0)

propPositiveRangeCount :: Positive Word8 -> Property
propPositiveRangeCount (Positive supplied) =
  case mkPositiveCount (fromIntegral supplied) of
    Left problem -> counterexample ("positive count rejected: " <> show problem) False
    Right count -> positiveCountWord64 count === fromIntegral supplied

propRangeEquivalence :: Positive Word8 -> Property
propRangeEquivalence (Positive supplied) =
  let countWord = fromIntegral supplied
      countInt = fromIntegral supplied
   in case initialState fixtureGeneratorSeed of
        Left problem -> counterexample ("generator initialization failed: " <> show problem) False
        Right predecessor -> case mkPositiveCount countWord of
          Left problem -> counterexample ("positive count rejected: " <> show problem) False
          Right count -> case prepareGeneratedIdRange count predecessor of
            Left problem -> counterexample ("range preparation failed: " <> show problem) False
            Right preparedRange -> case generate countInt predecessor of
              Left problem -> counterexample ("single-step preparation failed: " <> problem) False
              Right (expectedBytes, expectedSuccessor) ->
                let generated = NonEmpty.toList (preparedGlobalUniqueIds preparedRange)
                    generatedBytes = fmap globalUniqueIdBytes generated
                 in conjoin
                      [ counterexample "range length differs from requested count"
                          $ length generated === countInt,
                        counterexample "range order or identities differ from repeated preparation"
                          $ generatedBytes === expectedBytes,
                        counterexample "range successor differs from repeated commits"
                          $ property (commitGeneratedIdRange preparedRange == expectedSuccessor)
                      ]

propRangeDistinct :: Positive Word8 -> Property
propRangeDistinct (Positive supplied) =
  let countWord = fromIntegral supplied
      countInt = fromIntegral supplied
   in case initialState fixtureGeneratorSeed of
        Left problem -> counterexample ("generator initialization failed: " <> show problem) False
        Right predecessor -> case mkPositiveCount countWord of
          Left problem -> counterexample ("positive count rejected: " <> show problem) False
          Right count -> case prepareGeneratedIdRange count predecessor of
            Left problem -> counterexample ("range preparation failed: " <> show problem) False
            Right preparedRange ->
              let generatedBytes =
                    fmap globalUniqueIdBytes
                      . NonEmpty.toList
                      $ preparedGlobalUniqueIds preparedRange
               in counterexample "generated range contains a duplicate"
                    $ Set.size (Set.fromList generatedBytes) === countInt

propRangeCommit :: Word8 -> Positive Word8 -> Property
propRangeCommit rawOffset (Positive supplied) =
  let offset = fromIntegral rawOffset
      countWord = fromIntegral supplied
      expectedCounter = fromIntegral rawOffset + countWord
   in case initialState fixtureGeneratorSeed of
        Left problem -> counterexample ("generator initialization failed: " <> show problem) False
        Right initial -> case generate offset initial of
          Left problem -> counterexample ("predecessor advance failed: " <> problem) False
          Right (_, predecessor) -> case mkPositiveCount countWord of
            Left problem -> counterexample ("positive count rejected: " <> show problem) False
            Right count ->
              case prepareGeneratedIdRange count predecessor of
                Left problem -> counterexample ("first range preparation failed: " <> show problem) False
                Right preparedRange ->
                  conjoin
                    [ counterexample "preparation changed the predecessor"
                        $ witnessedNextGeneratorCounter (idGeneratorStateWitness predecessor)
                          === fromIntegral rawOffset,
                      counterexample "the sole commit did not advance by the whole count"
                        $ witnessedNextGeneratorCounter
                          (idGeneratorStateWitness (commitGeneratedIdRange preparedRange))
                          === expectedCounter
                    ]

finiteInjectivity :: Assertion
finiteInjectivity = do
  state <- checked "initialize generator" (initialState fixtureGeneratorSeed)
  (outputs, _) <- checked "generate finite sequence" (generate 2048 state)
  assertEqual "all outputs are exact-width" (replicate 2048 32) (fmap ByteString.length outputs)
  assertEqual "all outputs are distinct" 2048 (Set.size (Set.fromList outputs))
  assertBool
    "all-zero identity is absent"
    (all (/= ByteString.replicate 32 0) outputs)

distinctFixtureSequences :: Assertion
distinctFixtureSequences = do
  sequences <- traverse sample [fixtureGeneratorSeedH1, fixtureGeneratorSeedH2, fixtureGeneratorSeedH3]
  assertEqual "three fixture sequences differ" 3 (Set.size (Set.fromList sequences))
  where
    sample seed = do
      state <- checked "initialize fixture generator" (initialState seed)
      fst <$> checked "sample fixture generator" (generate 16 state)

seedRenderingIsOpaque :: Assertion
seedRenderingIsOpaque =
  assertEqual "opaque rendering" "<generator-seed>" (show fixtureGeneratorSeed)

generate :: Int -> State -> Either String ([ByteString], State)
generate count initial = go count initial []
  where
    go remaining state outputs
      | remaining <= 0 = Right (reverse outputs, state)
      | otherwise = case prepareGeneratedId state of
          Left problem -> Left (show problem)
          Right prepared ->
            go
              (remaining - 1)
              (commitGeneratedId prepared)
              (globalUniqueIdBytes (preparedGlobalUniqueId prepared) : outputs)

hexBytes :: String -> Either String ByteString
hexBytes input
  | odd (length input) = Left "odd hexadecimal fixture"
  | otherwise = Right (ByteString.pack (unfoldr next input))
  where
    next [] = Nothing
    next (high : low : rest) =
      Just (fromIntegral (digitToInt high * 16 + digitToInt low), rest)
    next _ = Nothing

expectedHex :: String -> String -> IO ByteString
expectedHex label = checked label . hexBytes

checked :: (Show error) => String -> Either error value -> IO value
checked label = either (\problem -> assertFailure (label <> ": " <> show problem)) pure
