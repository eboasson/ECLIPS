module TypesProperties
  ( tests,
  )
where

import Data.Binary qualified as Binary
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft)
import Data.Word (Word64)
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    ApplicationClaimError (..),
    ApplicationReplyCursorClaim,
    ApplicationRequestSummaryDto,
    ApplicationRequestSummaryEntryDto (..),
    ApplicationRequestSummaryStatusDto (..),
    ApplicationResumeTokenClaim,
    ApplicationSessionClaim,
    ApplicationWaitClaim,
    applicationAttachmentClaim,
    applicationClientNonce,
    applicationClientNonceWord64,
    applicationHeartbeatNonce,
    applicationHeartbeatNonceWord64,
    applicationReplyCursorClaim,
    applicationReplyCursorClaimWord64,
    applicationRequestIdClaim,
    applicationRequestIdClaimWord64,
    applicationRequestSummaryDto,
    applicationResumeTokenClaim,
    applicationSessionClaim,
    applicationWaitClaim,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Positive (..),
    Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    testProperty,
    (===),
  )
import TestFixtures (attachmentClaim)

tests :: TestTree
tests =
  testGroup
    "protocol claims"
    [ testProperty "all nominal scopes admit exactly 32 bytes" propNominalScopeWidths,
      testCase "nominal scopes render as grouped lowercase hexadecimal" caseNominalScopeRendering,
      testProperty "valid attachment claims Binary-round-trip" propAttachmentBinaryRoundTrip,
      testProperty "structured claims Binary-round-trip with generated ordinals" propStructuredClaimsBinaryRoundTrip,
      testProperty "all nominal Binary decoders recheck scope width" propEncodedNominalScopeWidths,
      testCase "reply cursor zero is rejected" caseReplyCursorZero,
      testProperty "positive reply cursors preserve their representation" propReplyCursorRoundTrip,
      testCase "request IDs, client nonces, and heartbeat nonces admit zero" caseZeroClientClaims,
      testCase "summaries require strict ascending request order" caseSummaryOrdering,
      testCase "the summary Binary decoder rechecks ordering" caseEncodedSummaryOrdering
    ]

propNominalScopeWidths :: Property
propNominalScopeWidths =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let scope = ByteString.replicate byteCount 0
        malformed = byteCount /= 32
        request = applicationRequestIdClaim 11
     in counterexample ("byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (applicationAttachmentClaim scope) === malformed,
              isLeft (applicationSessionClaim scope 7) === malformed,
              isLeft (applicationResumeTokenClaim scope 7) === malformed,
              isLeft (applicationWaitClaim scope 7 request) === malformed
            ]

caseNominalScopeRendering :: IO ()
caseNominalScopeRendering = do
  let scope = ByteString.pack [0 .. 31]
      expected =
        "00010203:04050607:08090a0b:0c0d0e0f:10111213:14151617:18191a1b:1c1d1e1f"
      request = applicationRequestIdClaim 11
  assertEqual "attachment claim" (Right expected) (show <$> applicationAttachmentClaim scope)
  assertEqual
    "session claim"
    (Right ("ApplicationSessionClaim " <> expected <> " 7"))
    (show <$> applicationSessionClaim scope 7)
  assertEqual
    "resume-token claim"
    (Right ("ApplicationResumeTokenClaim " <> expected <> " 7"))
    (show <$> applicationResumeTokenClaim scope 7)
  assertEqual
    "wait claim"
    ( Right
        ( "ApplicationWaitClaim "
            <> expected
            <> " 7 "
            <> show request
        )
    )
    (show <$> applicationWaitClaim scope 7 request)

propAttachmentBinaryRoundTrip :: Property
propAttachmentBinaryRoundTrip =
  decodeAttachment (Binary.encode attachmentClaim) === Right attachmentClaim

propStructuredClaimsBinaryRoundTrip :: Word64 -> Word64 -> Word64 -> Positive Word64 -> Property
propStructuredClaimsBinaryRoundTrip ordinal requestValue nonceValue (Positive cursorValue) =
  case ( applicationSessionClaim scope ordinal,
         applicationResumeTokenClaim scope ordinal,
         applicationWaitClaim scope ordinal request,
         applicationReplyCursorClaim cursorValue
       ) of
    (Right session, Right token, Right wait, Right cursor) ->
      conjoin
        [ binaryRoundTrip session,
          binaryRoundTrip token,
          binaryRoundTrip request,
          binaryRoundTrip wait,
          binaryRoundTrip cursor,
          binaryRoundTrip (applicationClientNonce nonceValue),
          binaryRoundTrip (applicationHeartbeatNonce nonceValue)
        ]
    invalidClaims -> counterexample (show invalidClaims) False
  where
    scope = ByteString.pack [0 .. 31]
    request = applicationRequestIdClaim requestValue

propEncodedNominalScopeWidths :: Property
propEncodedNominalScopeWidths =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let scope = ByteString.replicate byteCount 0
        malformed = byteCount /= 32
        ordinal = 7 :: Word64
        request = applicationRequestIdClaim 11
     in counterexample ("encoded byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (decodeAttachment (Binary.encode scope)) === malformed,
              isLeft (decodeSession (Binary.encode scope <> Binary.encode ordinal)) === malformed,
              isLeft (decodeResumeToken (Binary.encode scope <> Binary.encode ordinal)) === malformed,
              isLeft (decodeWait (Binary.encode scope <> Binary.encode ordinal <> Binary.encode request)) === malformed
            ]

caseReplyCursorZero :: IO ()
caseReplyCursorZero = do
  assertEqual "checked constructor" (Left ApplicationReplyCursorIsZero) (applicationReplyCursorClaim 0)
  assertBool "validating decoder" (isLeft (decodeCursor (Binary.encode (0 :: Word64))))

propReplyCursorRoundTrip :: Property
propReplyCursorRoundTrip =
  forAll (chooseInt (1, 100000)) $ \value ->
    case applicationReplyCursorClaim (fromIntegral value) of
      Left claimError -> counterexample (show claimError) False
      Right claim -> applicationReplyCursorClaimWord64 claim === fromIntegral value

caseZeroClientClaims :: IO ()
caseZeroClientClaims = do
  assertEqual "request ID zero" 0 (applicationRequestIdClaimWord64 (applicationRequestIdClaim 0))
  assertEqual "client nonce zero" 0 (applicationClientNonceWord64 (applicationClientNonce 0))
  assertEqual "heartbeat nonce zero" 0 (applicationHeartbeatNonceWord64 (applicationHeartbeatNonce 0))

caseSummaryOrdering :: IO ()
caseSummaryOrdering = do
  assertBool "duplicate request IDs" (isLeft (applicationRequestSummaryDto [entry 1, entry 1]))
  assertBool "descending request IDs" (isLeft (applicationRequestSummaryDto [entry 2, entry 1]))
  assertBool "ascending request IDs" (not (isLeft (applicationRequestSummaryDto [entry 1, entry 2])))

caseEncodedSummaryOrdering :: IO ()
caseEncodedSummaryOrdering =
  assertBool
    "the validating summary decoder rejects raw descending entries"
    (isLeft (decodeSummary (Binary.encode [entry 2, entry 1])))

entry :: Word64 -> ApplicationRequestSummaryEntryDto
entry value =
  ApplicationRequestSummaryEntryDto
    (applicationRequestIdClaim value)
    CancelledDto

decodeAttachment :: LazyByteString.ByteString -> Either String ApplicationAttachmentClaim
decodeAttachment bytes =
  decodedValue <$> decodeBinary bytes

decodeSession :: LazyByteString.ByteString -> Either String ApplicationSessionClaim
decodeSession bytes =
  decodedValue <$> decodeBinary bytes

decodeResumeToken :: LazyByteString.ByteString -> Either String ApplicationResumeTokenClaim
decodeResumeToken bytes =
  decodedValue <$> decodeBinary bytes

decodeWait :: LazyByteString.ByteString -> Either String ApplicationWaitClaim
decodeWait bytes =
  decodedValue <$> decodeBinary bytes

decodeCursor :: LazyByteString.ByteString -> Either String ApplicationReplyCursorClaim
decodeCursor bytes =
  decodedValue <$> decodeBinary bytes

decodeSummary :: LazyByteString.ByteString -> Either String ApplicationRequestSummaryDto
decodeSummary bytes =
  decodedValue <$> decodeBinary bytes

decodeBinary :: (Binary.Binary value) => LazyByteString.ByteString -> Either String (LazyByteString.ByteString, ByteOffset, value)
decodeBinary bytes =
  case Binary.decodeOrFail bytes of
    Left (_, _, message) -> Left message
    Right decoded -> Right decoded

decodedValue :: (LazyByteString.ByteString, ByteOffset, value) -> value
decodedValue (_, _, value) = value

binaryRoundTrip :: (Binary.Binary value, Eq value, Show value) => value -> Property
binaryRoundTrip value =
  counterexample (show value) (Binary.decode (Binary.encode value) === value)
