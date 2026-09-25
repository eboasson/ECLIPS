{-# LANGUAGE OverloadedStrings #-}

module ConnectionProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.Text qualified as Text
import Data.Word (Word16, Word8)
import Eclips.Application.Connection
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Public.Types.Timing qualified as Timing
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (Positive (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "connection descriptor text"
    [ testProperty "generated descriptors round-trip through canonical portable text" propRoundTrip,
      testCase "malformed hex and incomplete or trailing bodies are rejected" caseMalformed
    ]

propRoundTrip :: Word8 -> Positive Int -> Bool
propRoundTrip byte (Positive sample) =
  decodeConnectionDescriptor encoded == Right descriptor
    && decodeConnectionDescriptor (" \n" <> encoded <> "\n") == Right descriptor
  where
    descriptor = sampleDescriptor byte (fromIntegral (1 + sample `mod` 65535))
    encoded = encodeConnectionDescriptor descriptor

caseMalformed :: IO ()
caseMalformed = do
  let encoded = encodeConnectionDescriptor (sampleDescriptor 9 7100)
  assertEqual "nonhex text" (Left ConnectionDescriptorInvalidHex) (decodeConnectionDescriptor "xy")
  assertEqual "incomplete nibble" (Left ConnectionDescriptorInvalidHex) (decodeConnectionDescriptor "a")
  assertEqual "empty binary body" (Left ConnectionDescriptorInvalidBody) (decodeConnectionDescriptor "")
  assertEqual "incomplete body" (Left ConnectionDescriptorInvalidBody) (decodeConnectionDescriptor (Text.dropEnd 2 encoded))
  assertEqual "decoder residual" (Left ConnectionDescriptorInvalidBody) (decodeConnectionDescriptor (encoded <> "00"))

sampleDescriptor :: Word8 -> Word16 -> Lifecycle.ConnectionDescriptor
sampleDescriptor byte port = checked (Lifecycle.connectionDescriptorWithTiming target locator (Bytes.replicate 32 byte) (Bytes.replicate 32 2) (Bytes.replicate 32 3))
  where
    target = checked (Timing.takeoverTarget (500 + fromIntegral port * 1000))
    locator = checked (Lifecycle.heraldLocator "127.0.0.1" port)
    checked result = either (error . show) id result
