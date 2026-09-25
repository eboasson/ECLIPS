module TypesProperties
  ( tests,
  )
where

import Data.Binary qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft)
import Data.Word (Word64)
import Eclips.Protocol.Admin.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    testProperty,
    (===),
  )
import TestFixtures
  ( attachmentClaim,
    deploymentClaim,
    heraldClaim,
    processClaim,
    processEpochClaim,
  )

tests :: TestTree
tests =
  testGroup
    "administration claims"
    [ testProperty "every nominal claim admits exactly 32 bytes" propClaimWidths,
      testCase "nominal claims render as grouped lowercase hexadecimal" caseClaimRendering,
      testCase "every checked claim Binary-round-trips" caseClaimRoundTrips,
      testProperty "every Binary decoder rechecks nominal width" propEncodedWidths,
      testProperty "correlations preserve every Word64 including zero" propCorrelationRoundTrip,
      testProperty "control indexes preserve every Word64 including zero" propControlIndexRoundTrip,
      testCase "the current role union has exactly the administration role" caseRole,
      testCase "the EADM process-End reason is an explicit-administration singleton" caseEndReason,
      testCase "voter change identities require a nonzero applied control index" caseVoterChangeIdentity,
      testCase "retirement metadata cannot wrap handshake or nested maintenance" caseRetirementShape
    ]

caseRetirementShape :: IO ()
caseRetirementShape =
  mapM_
    check
    [AdminHello deploymentClaim ProcessAdministrator, RetireAdminReceipts mempty, AdminWithRetirement mempty (GetHeraldStatus (adminCorrelationIdClaim 0))]
  where
    check work = assertEqual "invalid retirement work" True (isLeft (decodeClaim (Binary.encode (AdminWithRetirement mempty work)) :: Either String AdminClientDto))

propClaimWidths :: Property
propClaimWidths =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let bytes = ByteString.replicate byteCount 0
        malformed = byteCount /= 32
     in counterexample ("claim bytes: " <> show byteCount)
          $ conjoin
            [ isLeft (adminDeploymentIdClaim bytes) === malformed,
              isLeft (adminProcessEpochIdClaim bytes) === malformed,
              isLeft (adminProcessIdClaim bytes) === malformed,
              isLeft (adminHeraldEpochClaim bytes) === malformed,
              isLeft (adminApplicationAttachmentClaim bytes) === malformed,
              isLeft (adminVoterConfigurationClaim bytes) === malformed
            ]

caseClaimRendering :: IO ()
caseClaimRendering =
  assertEqual "deployment claim" (Right groupedHex) (show <$> adminDeploymentIdClaim bytes)
  where
    bytes = ByteString.pack [0 .. 31]
    groupedHex =
      "00010203:04050607:08090a0b:0c0d0e0f:10111213:14151617:18191a1b:1c1d1e1f"

caseClaimRoundTrips :: IO ()
caseClaimRoundTrips = do
  assertEqual "deployment" deploymentClaim (Binary.decode (Binary.encode deploymentClaim))
  assertEqual "process epoch" processEpochClaim (Binary.decode (Binary.encode processEpochClaim))
  assertEqual "process" processClaim (Binary.decode (Binary.encode processClaim))
  assertEqual "Herald epoch" heraldClaim (Binary.decode (Binary.encode heraldClaim))
  assertEqual "attachment" attachmentClaim (Binary.decode (Binary.encode attachmentClaim))

propEncodedWidths :: Property
propEncodedWidths =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let bytes = Binary.encode (ByteString.replicate byteCount 0)
        malformed = byteCount /= 32
     in conjoin
          [ isLeft (decodeClaim bytes :: Either String AdminDeploymentIdClaim) === malformed,
            isLeft (decodeClaim bytes :: Either String AdminProcessEpochIdClaim) === malformed,
            isLeft (decodeClaim bytes :: Either String AdminProcessIdClaim) === malformed,
            isLeft (decodeClaim bytes :: Either String AdminHeraldEpochClaim) === malformed,
            isLeft (decodeClaim bytes :: Either String AdminApplicationAttachmentClaim) === malformed,
            isLeft (decodeClaim bytes :: Either String AdminVoterConfigurationClaim) === malformed
          ]

propCorrelationRoundTrip :: Word64 -> Property
propCorrelationRoundTrip value =
  let claim = adminCorrelationIdClaim value
   in conjoin
        [ adminCorrelationIdClaimWord64 claim === value,
          Binary.decode (Binary.encode claim) === claim
        ]

propControlIndexRoundTrip :: Word64 -> Property
propControlIndexRoundTrip value =
  let dto = adminControlIndexDto value
   in conjoin
        [ adminControlIndexDtoWord64 dto === value,
          Binary.decode (Binary.encode dto) === dto
        ]

caseRole :: IO ()
caseRole =
  assertEqual
    "role round-trip"
    ProcessAdministrator
    (Binary.decode (Binary.encode ProcessAdministrator))

caseVoterChangeIdentity :: IO ()
caseVoterChangeIdentity = do
  assertEqual "zero is not a committed change identity" (Left AdminVoterChangeIdZero) (adminVoterChangeIdClaim 0)
  assertEqual "encoded zero is rejected" True (isLeft (decodeClaim (Binary.encode (0 :: Word64)) :: Either String AdminVoterChangeIdClaim))
  case adminVoterChangeIdClaim 1 of
    Left problem -> error (show problem)
    Right change -> assertEqual "a valid change identity roundtrips" change (Binary.decode (Binary.encode change))

caseEndReason :: IO ()
caseEndReason =
  assertEqual
    "no inferred-loss reason is constructible in EADM"
    [AdminExplicitAdministrativeEnd]
    ([minBound .. maxBound] :: [AdminProcessEndReason])

decodeClaim :: (Binary.Binary value) => LazyByteString.ByteString -> Either String value
decodeClaim bytes =
  case Binary.decodeOrFail bytes of
    Left (_, _, problem) -> Left problem
    Right (remaining, _, value)
      | LazyByteString.null remaining -> Right value
      | otherwise -> Left "trailing bytes"
