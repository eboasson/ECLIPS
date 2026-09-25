{-# LANGUAGE OverloadedStrings #-}

module JoinSourceBundleProperties (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.Serialize qualified as Codec
import Data.Word (Word8)
import Eclips.Herald.Join.SourceBundle
import Eclips.Oracle.Admission (deriveHeraldJoinDigest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (Property, conjoin, testProperty, (=/=), (===))

tests :: TestTree
tests =
  testGroup
    "Herald frozen source bundle framing"
    [ testProperty "all five opaque frames roundtrip in their original roles" propRoundtrip,
      testProperty "canonical bytes preserve the aggregate transcript contract" propCanonicalTranscript,
      testProperty "every owner frame contributes to the external source digest" propDigestFrames,
      testCase "wrong domain, incomplete and trailing frames are rejected" caseMalformed
    ]

type FrameBytes = ([Word8], [Word8], [Word8], [Word8], [Word8])

bundleFromBytes :: FrameBytes -> JoinSourceBundle
bundleFromBytes (projection, registry, progress, carriers, history) =
  joinSourceBundle
    (ByteString.pack projection)
    (ByteString.pack registry)
    (ByteString.pack progress)
    (ByteString.pack carriers)
    (ByteString.pack history)

propRoundtrip :: FrameBytes -> Property
propRoundtrip supplied@(projection, registry, progress, carriers, history) =
  let bundle = bundleFromBytes supplied
   in conjoin
        [ decodeJoinSourceBundle (encodeJoinSourceBundle bundle) === Right bundle,
          joinSourceBundleProjectionFrame bundle === ByteString.pack projection,
          joinSourceBundleRegistryFrame bundle === ByteString.pack registry,
          joinSourceBundleProgressFrame bundle === ByteString.pack progress,
          joinSourceBundleCarrierFrame bundle === ByteString.pack carriers,
          joinSourceBundleHistoryFrame bundle === ByteString.pack history
        ]

propCanonicalTranscript :: FrameBytes -> Property
propCanonicalTranscript supplied@(projection, registry, progress, carriers, history) =
  encodeJoinSourceBundle (bundleFromBytes supplied)
    === Codec.encode
      ( "ECLIPS-HERALD-JOINING-BASE" :: ByteString,
        ByteString.pack projection,
        ByteString.pack registry,
        ByteString.pack progress,
        ByteString.pack carriers,
        ByteString.pack history
      )

propDigestFrames :: FrameBytes -> Property
propDigestFrames supplied@(projection, registry, progress, carriers, history) =
  let digest = deriveHeraldJoinDigest . encodeJoinSourceBundle . bundleFromBytes
      original = digest supplied
      changed =
        [ (0 : projection, registry, progress, carriers, history),
          (projection, 0 : registry, progress, carriers, history),
          (projection, registry, 0 : progress, carriers, history),
          (projection, registry, progress, 0 : carriers, history),
          (projection, registry, progress, carriers, 0 : history)
        ]
   in conjoin [digest frames =/= original | frames <- changed]

caseMalformed :: IO ()
caseMalformed = do
  let bytes = encodeJoinSourceBundle (joinSourceBundle "projection" "registry" "progress" "carriers" "history")
      badDomain = Codec.encode ("ECLIPS-HERALD-HISTORY" :: ByteString, "projection" :: ByteString, "registry" :: ByteString, "progress" :: ByteString, "carriers" :: ByteString, "history" :: ByteString)
  assertBool "another domain is not a source bundle" (isLeft (decodeJoinSourceBundle badDomain))
  assertBool "extra bytes cannot extend the certified frame" (isLeft (decodeJoinSourceBundle (bytes <> "extra")))
  assertBool "every incomplete frame is rejected" (all (isLeft . decodeJoinSourceBundle . (`ByteString.take` bytes)) [0 .. ByteString.length bytes - 1])
