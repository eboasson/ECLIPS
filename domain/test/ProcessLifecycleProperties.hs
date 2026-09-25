{-# LANGUAGE OverloadedStrings #-}

module ProcessLifecycleProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.Word (Word8)
import Eclips.Domain.Identity
  ( ProcessEpochId,
    mkProcessEpochId,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (..),
    ProcessEndReasonCanonicalProblem (..),
    decodeProcessEndReasonCanonicalBytes,
    effectiveProcessLabel,
    normalizeLabelForProcessEnd,
    processEndReasonCanonicalBytes,
    processEndReasonIsHeraldRetirement,
    processEndReasonMayBeHeraldSubmitted,
  )
import Eclips.Domain.Value (LabelOwner (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck (chooseInt, forAll, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "process lifecycle vocabulary"
    [ testCase "the process-end reason vocabulary is closed" caseClosedReason,
      testCase "the process-end reason has pinned canonical bytes" caseCanonicalReasonBytes,
      testCase "the process-end reason decoder is checked and canonical" caseCanonicalReasonDecoder,
      testCase "only Herald-originated reasons enter the ordinary End command" caseHeraldSubmissionReasons,
      testCase "exactly one reason denotes Oracle-derived Herald retirement" caseHeraldRetirementReason,
      testCase "ended processes project process labels to zombie" caseEffectiveLabel,
      testCase "one exact End normalizes only its named process" caseExactEnd,
      testCase "process-label normalization is idempotent" caseIdempotent,
      testProperty "End and retirement preserve the complete generation"
        $ forAll (chooseInt (0, 100000))
        $ \generation ->
          let ended = process 1
           in effectiveProcessLabel (== ended) (ProcessLabel ended, fromIntegral generation)
                === (ZombieLabel ended, fromIntegral generation)
    ]

caseClosedReason :: IO ()
caseClosedReason = do
  assertEqual
    "the closed reasons retain their fixed enum positions"
    [ExplicitAdministrativeEnd, ApplicationPermanentlyLost, HeraldRetired]
    ([minBound .. maxBound] :: [ProcessEndReason])

caseCanonicalReasonBytes :: IO ()
caseCanonicalReasonBytes = do
  assertEqual
    "explicit administrative End remains tag 0"
    (canonicalReasonBytes 0)
    (processEndReasonCanonicalBytes ExplicitAdministrativeEnd)
  assertEqual
    "application permanently lost remains tag 1"
    (canonicalReasonBytes 1)
    (processEndReasonCanonicalBytes ApplicationPermanentlyLost)
  assertEqual
    "Herald retirement is tag 2"
    (canonicalReasonBytes 2)
    (processEndReasonCanonicalBytes HeraldRetired)

canonicalReasonBytes :: Word8 -> ByteString.ByteString
canonicalReasonBytes tag =
  ByteString.replicate 7 0
    <> ByteString.singleton 25
    <> "ECLIPS-PROCESS-END-REASON"
    <> ByteString.singleton tag

caseCanonicalReasonDecoder :: IO ()
caseCanonicalReasonDecoder = do
  let canonical = processEndReasonCanonicalBytes ExplicitAdministrativeEnd
      applicationLoss = processEndReasonCanonicalBytes ApplicationPermanentlyLost
      heraldRetirement = processEndReasonCanonicalBytes HeraldRetired
      unknownTag = ByteString.init canonical <> ByteString.singleton 3
      wrongDomain =
        ByteString.take 8 canonical
          <> "X"
          <> ByteString.drop 9 canonical
  assertEqual
    "decode inverts the explicit canonical reason"
    (Right ExplicitAdministrativeEnd)
    (decodeProcessEndReasonCanonicalBytes canonical)
  assertEqual
    "application-loss reason round-trips"
    (Right ApplicationPermanentlyLost)
    (decodeProcessEndReasonCanonicalBytes applicationLoss)
  assertEqual
    "Herald-retirement reason round-trips"
    (Right HeraldRetired)
    (decodeProcessEndReasonCanonicalBytes heraldRetirement)
  assertEqual
    "an unknown closed-sum tag is rejected explicitly"
    (Left (ProcessEndReasonCanonicalUnknownTag 3))
    (decodeProcessEndReasonCanonicalBytes unknownTag)
  assertEqual
    "the checked decoder rejects a foreign domain"
    ( Left
        ( ProcessEndReasonCanonicalWrongDomain
            "XCLIPS-PROCESS-END-REASON"
        )
    )
    (decodeProcessEndReasonCanonicalBytes wrongDomain)
  assertEqual
    "the checked decoder rejects trailing bytes"
    True
    (isLeft (decodeProcessEndReasonCanonicalBytes (canonical <> "\NUL")))

caseHeraldSubmissionReasons :: IO ()
caseHeraldSubmissionReasons = do
  assertEqual
    "administrative End is Herald-submittable"
    True
    (processEndReasonMayBeHeraldSubmitted ExplicitAdministrativeEnd)
  assertEqual
    "application loss is Herald-submittable"
    True
    (processEndReasonMayBeHeraldSubmitted ApplicationPermanentlyLost)
  assertEqual
    "retirement is derived by Oracle membership authority"
    False
    (processEndReasonMayBeHeraldSubmitted HeraldRetired)

caseHeraldRetirementReason :: IO ()
caseHeraldRetirementReason = do
  let retirementReasons =
        filter
          processEndReasonIsHeraldRetirement
          ([minBound .. maxBound] :: [ProcessEndReason])
  assertEqual
    "the closed vocabulary has one retirement-derived reason"
    [HeraldRetired]
    retirementReasons
  assertEqual
    "the retirement-derived reason cannot enter an ordinary Herald End command"
    [False]
    (fmap processEndReasonMayBeHeraldSubmitted retirementReasons)

caseEffectiveLabel :: IO ()
caseEffectiveLabel = do
  let ended = process 1
      live = process 2
      isEnded candidate = candidate == ended
  assertEqual
    "matching process becomes zombie"
    ((ZombieLabel ended, 0))
    (effectiveProcessLabel isEnded ((ProcessLabel ended, 0)))
  assertEqual
    "other process stays live"
    ((ProcessLabel live, 0))
    (effectiveProcessLabel isEnded ((ProcessLabel live, 0)))
  assertEqual
    "void stays void"
    (VoidLabel, 0)
    (effectiveProcessLabel isEnded (VoidLabel, 0))
  assertEqual
    "an existing zombie is not reinterpreted"
    ((ZombieLabel live, 0))
    (effectiveProcessLabel isEnded ((ZombieLabel live, 0)))

caseExactEnd :: IO ()
caseExactEnd = do
  let ended = process 3
      other = process 4
  assertEqual
    "the exact process label is normalized"
    ((ZombieLabel ended, 0))
    (normalizeLabelForProcessEnd ended ((ProcessLabel ended, 0)))
  assertEqual
    "an unrelated process label is unchanged"
    ((ProcessLabel other, 0))
    (normalizeLabelForProcessEnd ended ((ProcessLabel other, 0)))

caseIdempotent :: IO ()
caseIdempotent = do
  let ended = process 5
      normalize = normalizeLabelForProcessEnd ended
      labels = [(VoidLabel, 0), (ProcessLabel ended, 0), (ZombieLabel ended, 0)]
  mapM_
    (\label -> assertEqual (show label) (normalize label) (normalize (normalize label)))
    labels

process :: Word -> ProcessEpochId
process byte =
  checked
    "process epoch"
    (mkProcessEpochId (ByteString.replicate 32 (fromIntegral byte)))

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (invariantFailure label) id

invariantFailure :: (Show problem) => String -> problem -> value
invariantFailure label problem =
  error (label <> " fixture invariant: " <> show problem)
