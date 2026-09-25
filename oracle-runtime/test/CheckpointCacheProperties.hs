module CheckpointCacheProperties (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (foldM, unless)
import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity (controlIndex)
import Eclips.Oracle.Command
  ( OracleEnvelope,
    oracleEnvelope,
    oracleEnvelopeWithReceiptRetirementProgress,
    retireOracleReceiptProgressCommand,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Runtime.Internal.CheckpointCache
  ( OracleCheckpointCache,
    OracleCheckpointCaptureWork (..),
    advanceOracleCheckpointCache,
    captureOracleCheckpoint,
    capturedOracleCheckpointBytes,
    capturedOracleCheckpointIndex,
    emptyOracleCheckpointCache,
  )
import Eclips.Oracle.State
  ( OracleStateDigestMode (..),
    oracleCheckpointBytes,
    oracleGreatestControlIndex,
  )
import Eclips.Oracle.Transition (initialOracleWithStateDigestMode, stepOracle)
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Property,
    choose,
    chooseInt,
    conjoin,
    counterexample,
    frequency,
    property,
    shrinkList,
    sublistOf,
    testProperty,
    vectorOf,
  )
import TestFixtures
  ( alternateOracleCommand,
    checked,
    fixtureCheckedGenesis,
    fixtureHeraldEpoch,
    validOracleCommand,
  )

tests :: TestTree
tests =
  testGroup
    "Oracle checkpoint cache"
    [ testProperty "capture, prefix advances and replacement installs agree with uncached snapshots" propCacheHistory,
      testProperty "real Oracle transitions with unchanged prefixes preserve checkpoint bytes in both digest modes" propOraclePrefixes,
      testCase "a repeated capture does not evaluate its encoder" caseHitSkipsEncoder,
      testCase "advancing the prefix immediately forgets the old capture" caseAdvanceDropsCapture,
      testCase "a cache miss materializes bytes before retaining the capture" caseMissForcesBytes
    ]

data CacheAction
  = Capture
  | Advance Word8
  | UnchangedPrefix
  | InstallSameIndex Word8
  deriving stock (Show)

newtype CacheHistory = CacheHistory [CacheAction]
  deriving stock (Show)

instance Arbitrary CacheHistory where
  arbitrary = do
    count <- chooseInt (0, 60)
    CacheHistory <$> vectorOf count (frequency [(4, pure Capture), (2, Advance <$> arbitrary), (1, pure UnchangedPrefix), (1, InstallSameIndex <$> arbitrary)])
  shrink (CacheHistory actions) = CacheHistory <$> shrinkList (const []) actions

-- The reference snapshot is replaced even when installation preserves its
-- numeric prefix. It deliberately knows nothing about the cache representation.
propCacheHistory :: CacheHistory -> Property
propCacheHistory (CacheHistory generated) =
  counterexample (show actions) $ case foldM apply initial actions of
    Left problem -> counterexample problem False
    Right _ -> property True
  where
    actions = [Capture, Capture, UnchangedPrefix, Capture, Advance 1, Capture, InstallSameIndex 2, Capture, Capture] <> generated
    initial = (0 :: Word64, ByteString.singleton 0, False, emptyOracleCheckpointCache)
    apply (index, payload, available, cache) action = case action of
      Capture -> do
        let (successor, captured, work) = captureOracleCheckpoint (controlIndex index) payload cache
        require "captured the current snapshot" (capturedOracleCheckpointBytes captured == payload)
        require "captured the current control prefix" (capturedOracleCheckpointIndex captured == controlIndex index)
        require "only an available current snapshot is reused" (work == if available then OracleCheckpointReused else OracleCheckpointEncoded)
        pure (index, payload, True, successor)
      Advance value ->
        let next = index + 1
         in pure (next, ByteString.snoc payload value, False, advanceOracleCheckpointCache (controlIndex next) cache)
      UnchangedPrefix -> pure (index, payload, available, advanceOracleCheckpointCache (controlIndex index) cache)
      -- Installation is a replacement boundary, even when its prefix matches.
      InstallSameIndex value -> pure (index, ByteString.snoc payload value, False, emptyOracleCheckpointCache)

caseHitSkipsEncoder :: Assertion
caseHitSkipsEncoder = do
  let index = controlIndex 7
      payload = ByteString.pack [1, 2, 3]
      (cache, _, _) = captureOracleCheckpoint index payload emptyOracleCheckpointCache
      (_, captured, work) = captureOracleCheckpoint index (error "an unchanged checkpoint was serialized again") (advanceOracleCheckpointCache index cache)
  assertEqual "reuse was reported" OracleCheckpointReused work
  assertEqual "the retained payload is returned" payload (capturedOracleCheckpointBytes captured)
  assertEqual "the retained prefix is returned" index (capturedOracleCheckpointIndex captured)

caseAdvanceDropsCapture :: Assertion
caseAdvanceDropsCapture = do
  let oldIndex = controlIndex 7
      (cache, _, _) = captureOracleCheckpoint oldIndex (ByteString.singleton 1) emptyOracleCheckpointCache
      invalidated = advanceOracleCheckpointCache (controlIndex 8) cache
      -- Asking about the former key distinguishes eager invalidation from
      -- keeping an obsolete payload until the next current-prefix capture.
      (_, captured, work) = captureOracleCheckpoint oldIndex (ByteString.singleton 2) invalidated
  assertEqual "the obsolete entry was removed at advance" OracleCheckpointEncoded work
  assertEqual "the obsolete bytes cannot be returned" (ByteString.singleton 2) (capturedOracleCheckpointBytes captured)

caseMissForcesBytes :: Assertion
caseMissForcesBytes = do
  let (cache, _, _) = captureOracleCheckpoint (controlIndex 1) (error "encoding failed before cache installation") emptyOracleCheckpointCache
  result <- try (evaluate cache) :: IO (Either ErrorCall OracleCheckpointCache)
  case result of
    Left _ -> pure ()
    Right _ -> assertFailure "the cache retained an unevaluated encoder instead of materialized bytes"

data OracleAction
  = Request Word64 Bool
  | Retire Word64 [Word64]
  | Piggyback Word64 Bool Word64 [Word64]
  deriving stock (Show)

newtype OracleHistory = OracleHistory [OracleAction]
  deriving stock (Show)

instance Arbitrary OracleHistory where
  arbitrary = do
    count <- chooseInt (0, 50)
    let retirement = do
          through <- choose (0, 24)
          Retire through <$> sublistOf [0 .. through]
        piggyback = do
          sequenceNumber <- choose (1, 24)
          through <- choose (0, sequenceNumber - 1)
          Piggyback sequenceNumber <$> arbitrary <*> pure through <*> sublistOf [0 .. through]
    OracleHistory <$> vectorOf count (frequency [(4, Request <$> choose (0, 24) <*> arbitrary), (2, retirement), (2, piggyback)])
  shrink (OracleHistory actions) = OracleHistory <$> shrinkList (const []) actions

propOraclePrefixes :: OracleHistory -> Property
propOraclePrefixes (OracleHistory generated) =
  conjoin [checkMode mode | mode <- [OracleStateDigestDisabled, OracleStateDigestEnabled]]
  where
    -- Establish exact and conflicting retries, a fresh semantic rejection,
    -- sparse maintenance, a duplicate/conflict carrying fresh retirement,
    -- repeated maintenance, retired requests and one fresh piggyback command.
    actions =
      [ Request 1 False,
        Request 1 False,
        Request 1 True,
        Request 2 False,
        Request 3 False,
        Piggyback 3 False 2 [1],
        Piggyback 3 False 2 [1],
        Piggyback 3 True 2 [],
        Piggyback 3 True 2 [],
        Request 1 False,
        Retire 3 [3],
        Retire 3 [3],
        Retire 3 [],
        Request 4 False,
        Piggyback 5 False 4 [],
        Piggyback 5 False 4 []
      ]
        <> generated
    checkMode mode =
      let state = checked "initial checkpoint-cache Oracle" (initialOracleWithStateDigestMode mode fixtureCheckedGenesis)
          bytes = oracleCheckpointBytes state
          (cache, _, _) = captureOracleCheckpoint (oracleGreatestControlIndex state) bytes emptyOracleCheckpointCache
       in counterexample ("mode=" <> show mode <> "; actions=" <> show actions) $ case foldM apply (state, bytes, cache) actions of
            Left problem -> counterexample problem False
            Right _ -> property True
    apply (state, oldBytes, cache) action = do
      (successor, _, _) <- either (Left . show) Right (stepOracle (envelope action) state)
      let index = oracleGreatestControlIndex successor
          samePrefix = index == oracleGreatestControlIndex state
          freshBytes = oracleCheckpointBytes successor
          (nextCache, captured, work) = captureOracleCheckpoint index freshBytes (advanceOracleCheckpointCache index cache)
      require (show action <> ": unchanged prefix changed checkpoint bytes") (not samePrefix || freshBytes == oldBytes)
      require (show action <> ": cache disagrees with fresh serialization") (capturedOracleCheckpointBytes captured == freshBytes)
      require (show action <> ": cache reports the wrong prefix") (capturedOracleCheckpointIndex captured == index)
      require (show action <> ": cache reuse does not follow state advancement") (work == if samePrefix then OracleCheckpointReused else OracleCheckpointEncoded)
      pure (successor, freshBytes, nextCache)

envelope :: OracleAction -> OracleEnvelope
envelope action = case action of
  Request sequenceNumber alternate ->
    make sequenceNumber (if alternate then alternateOracleCommand else validOracleCommand)
  Retire through exceptions ->
    make through (retireOracleReceiptProgressCommand (progress through exceptions))
  Piggyback sequenceNumber alternate through exceptions ->
    oracleEnvelopeWithReceiptRetirementProgress (progress through exceptions) (envelope (Request sequenceNumber alternate))
  where
    make sequenceNumber = oracleEnvelope (oracleClientRequestId fixtureHeraldEpoch sequenceNumber) Nothing fixtureHeraldEpoch
    progress through exceptions = checked "generated sparse receipt progress" (Lifetime.receiptRetirement (Just through) (Set.fromList exceptions))

require :: String -> Bool -> Either String ()
require message condition = unless condition (Left message)
