module CodecProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Data.Word (Word64)
import Eclips.Protocol.Raft.Codec
  ( RaftPayloadError (..),
    decodeRaftEnvelope,
    encodeRaftEnvelope,
  )
import Eclips.Protocol.Raft.Types (RaftProtocolEnvelope)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )
import TestFixtures (allEnvelopes, generatedEnvelopes)

tests :: TestTree
tests =
  testGroup
    "Raft envelope codec"
    [ testCase "every current constructor fixture round-trips" caseAllConstructorsRoundTrip,
      testProperty "generated fields round-trip every current constructor" propGeneratedRoundTrip,
      testCase "trailing bytes inside one declared body reject" caseTrailingBody,
      testCase "unknown envelope and RPC tags reject" caseUnknownTags,
      testCase "every strict prefix of a valid envelope rejects" caseTruncation
    ]

caseAllConstructorsRoundTrip :: IO ()
caseAllConstructorsRoundTrip =
  mapM_
    (\envelope -> assertEqual (show envelope) (Right envelope) (roundTrip envelope))
    allEnvelopes

propGeneratedRoundTrip :: Word64 -> Bool -> Property
propGeneratedRoundTrip counter positiveResult =
  conjoin
    [ counterexample (show envelope) (roundTrip envelope === Right envelope)
    | envelope <- generatedEnvelopes counter positiveResult
    ]

caseTrailingBody :: IO ()
caseTrailingBody =
  assertEqual
    "trailing byte"
    (Left RaftPayloadMalformed)
    (decodeRaftEnvelope (encodeRaftEnvelope firstEnvelope <> ByteString.singleton 0))

caseUnknownTags :: IO ()
caseUnknownTags = do
  assertEqual
    "unknown envelope"
    (Left RaftPayloadMalformed)
    (decodeRaftEnvelope (ByteString.singleton 0xff))
  assertEqual
    "unknown nested RPC"
    (Left RaftPayloadMalformed)
    (decodeRaftEnvelope (ByteString.pack [1, 0xff]))

caseTruncation :: IO ()
caseTruncation =
  assertBool
    "every strict prefix fails closed"
    (all (isLeft . decodeRaftEnvelope . (`ByteString.take` body)) [0 .. ByteString.length body - 1])
  where
    body = encodeRaftEnvelope largestEnvelope

roundTrip :: RaftProtocolEnvelope -> Either RaftPayloadError RaftProtocolEnvelope
roundTrip = decodeRaftEnvelope . encodeRaftEnvelope

firstEnvelope :: RaftProtocolEnvelope
firstEnvelope = case allEnvelopes of
  envelope : _ -> envelope
  [] -> error "the closed Raft-envelope fixture catalogue is empty"

largestEnvelope :: RaftProtocolEnvelope
largestEnvelope = case reverse allEnvelopes of
  envelope : _ -> envelope
  [] -> error "the closed Raft-envelope fixture catalogue is empty"
