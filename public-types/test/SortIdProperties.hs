module SortIdProperties
  ( tests,
  )
where

import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft)
import Data.Word (Word8)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Public.Types.SortId
  ( SortId,
    SortIdError
      ( WrongSortIdByteCount,
        actualSortIdByteCount,
        expectedSortIdByteCount
      ),
    mkSortId,
    sortIdBytes,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    counterexample,
    forAll,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "public SortId"
    [ testProperty "only exactly 32 bytes are admitted" propCheckedLength,
      testProperty "checked arbitrary bytes round-trip exactly" propByteRoundTrip,
      testProperty "checked identities round-trip through Binary" propBinaryRoundTrip,
      testCase "Binary decoding repeats exact-width admission" caseBinaryRejectsWrongLength,
      testCase "error reports expected and actual lengths" caseLengthError,
      testCase "non-uniform byte order is preserved" caseNonUniformByteOrder,
      testCase "diagnostic rendering is grouped lowercase hexadecimal" caseDiagnosticRendering
    ]

propCheckedLength :: Property
propCheckedLength =
  forAll (chooseInt (0, 64)) $ \byteCount ->
    let result = mkSortId (ByteString.replicate byteCount 0)
     in counterexample ("byte count: " <> show byteCount)
          $ isLeft result === (byteCount /= 32)

propByteRoundTrip :: [Word8] -> Property
propByteRoundTrip generated =
  let bytes = exactly32Bytes generated
   in (sortIdBytes <$> mkSortId bytes) === Right bytes

propBinaryRoundTrip :: [Word8] -> Property
propBinaryRoundTrip generated =
  let bytes = exactly32Bytes generated
   in case mkSortId bytes of
        Left problem -> counterexample (show problem) False
        Right sortId -> decodeBinary (encode sortId) === Right sortId

caseBinaryRejectsWrongLength :: IO ()
caseBinaryRejectsWrongLength =
  isLeft
    ( decodeBinary (encode (ByteString.replicate 31 0)) ::
        Either String SortId
    )
    @?= True

caseLengthError :: IO ()
caseLengthError =
  mkSortId (ByteString.replicate 31 0)
    @?= Left
      WrongSortIdByteCount
        { expectedSortIdByteCount = 32,
          actualSortIdByteCount = 31
        }

caseNonUniformByteOrder :: IO ()
caseNonUniformByteOrder = do
  let bytes = ByteString.pack [0 .. 31]
  (sortIdBytes <$> mkSortId bytes) @?= Right bytes

caseDiagnosticRendering :: IO ()
caseDiagnosticRendering = do
  let bytes = ByteString.pack [0 .. 31]
      expected =
        "00010203:04050607:08090a0b:0c0d0e0f:10111213:14151617:18191a1b:1c1d1e1f"
  renderGroupedHex bytes @?= expected
  show <$> mkSortId bytes @?= Right expected

exactly32Bytes :: [Word8] -> ByteString
exactly32Bytes generated =
  ByteString.pack (take 32 (generated <> [0 .. 31]))

decodeBinary :: (Binary value) => LazyByteString.ByteString -> Either String value
decodeBinary bytes =
  case decodeOrFail bytes of
    Left (_, _, problem) -> Left problem
    Right (remaining, _, value)
      | LazyByteString.null remaining -> Right value
      | otherwise -> Left "unexpected trailing bytes"
