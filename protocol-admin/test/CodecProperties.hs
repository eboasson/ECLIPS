module CodecProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Either (isLeft)
import Eclips.Protocol.Admin.Codec
  ( AdminPayloadError (AdminPayloadMalformed),
    decodeAdminEnvelope,
    encodeAdminEnvelope,
  )
import Eclips.Protocol.Admin.Types
  ( AdminClientDto (..),
    AdminEnvelope (AdminClientEnvelope),
    AdminVoterBindingDto (..),
    AdminVoterChangeReasonDto (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    elements,
    forAll,
    testProperty,
    (===),
  )
import TestFixtures (allEnvelopes, correlationClaim, heraldClaim, oracleNode, voterChangeClaim, voterConfigurationClaim)

tests :: TestTree
tests =
  testGroup
    "administration codec"
    [ testCase "every lifecycle and operator snapshot constructor fixture round-trips" caseAllRoundTrip,
      testProperty "generated fixture envelopes round-trip" propRoundTrip,
      testCase "operator command tags are fixed" caseOperatorTags,
      testCase "voter administration has closed command tags" caseVoterTags,
      testCase "trailing bytes reject" caseTrailing,
      testCase "unknown generic constructor rejects" caseUnknownConstructor,
      testCase "application-loss End reason rejects at EADM decoding" caseApplicationLossReason,
      testCase "Herald-retirement End reason rejects at EADM decoding" caseHeraldRetirementReason,
      testCase "every strict prefix rejects" caseTruncation
    ]

caseAllRoundTrip :: IO ()
caseAllRoundTrip =
  mapM_
    (\envelope -> assertEqual (show envelope) (Right envelope) (roundTrip envelope))
    allEnvelopes

propRoundTrip :: Property
propRoundTrip =
  forAll (elements allEnvelopes) $ \envelope ->
    counterexample (show envelope) (roundTrip envelope === Right envelope)

caseOperatorTags :: IO ()
caseOperatorTags = mapM_ check [(5, GetHeraldStatus), (6, ListChildPreparations), (7, DrainHerald)]
  where
    check (tag, operation) =
      assertEqual
        "closed EADM operator tag"
        (ByteString.pack [0, tag])
        (ByteString.take 2 (encodeAdminEnvelope (AdminClientEnvelope (operation correlationClaim))))

caseTrailing :: IO ()
caseTrailing =
  assertEqual
    "trailing byte"
    (Left AdminPayloadMalformed)
    (decodeAdminEnvelope (encodeAdminEnvelope firstEnvelope <> ByteString.singleton 0))

caseVoterTags :: IO ()
caseVoterTags =
  mapM_
    check
    [ (8, PrepareOracleReplica correlationClaim),
      (9, BeginVoterChange correlationClaim voterConfigurationClaim AdminCommissionVotersDto [AdminVoterBindingDto oracleNode heraldClaim]),
      (10, CancelVoterChange correlationClaim voterChangeClaim),
      (11, GetOracleConfiguration correlationClaim),
      (12, GetVoterChangeStatus correlationClaim voterChangeClaim)
    ]
  where
    check (tag, command) =
      assertEqual
        "closed voter command tag"
        (ByteString.pack [0, tag])
        (ByteString.take 2 (encodeAdminEnvelope (AdminClientEnvelope command)))

caseUnknownConstructor :: IO ()
caseUnknownConstructor =
  assertEqual
    "unknown outer discriminator"
    (Left AdminPayloadMalformed)
    (decodeAdminEnvelope (ByteString.singleton 0xff))

caseApplicationLossReason :: IO ()
caseApplicationLossReason =
  assertEqual
    "Domain reason tag 1 is reserved for resident Herald inference"
    (Left AdminPayloadMalformed)
    (decodeAdminEnvelope applicationLossPayload)
  where
    applicationLossPayload =
      ByteString.init canonicalAdministrativeEndPayload
        <> ByteString.singleton 1
    canonicalAdministrativeEndPayload =
      encodeAdminEnvelope administrationEndEnvelope

caseHeraldRetirementReason :: IO ()
caseHeraldRetirementReason =
  assertEqual
    "Domain reason tag 2 is reserved for Oracle membership projection"
    (Left AdminPayloadMalformed)
    (decodeAdminEnvelope heraldRetirementPayload)
  where
    heraldRetirementPayload =
      ByteString.init canonicalAdministrativeEndPayload
        <> ByteString.singleton 2
    canonicalAdministrativeEndPayload =
      encodeAdminEnvelope administrationEndEnvelope

administrationEndEnvelope :: AdminEnvelope
administrationEndEnvelope = case allEnvelopes of
  envelope@(AdminClientEnvelope EndProcessEpoch {}) : _ -> envelope
  _ : remaining -> findEnd remaining
  [] -> error "the administration fixture catalogue has no End command"
  where
    findEnd (envelope@(AdminClientEnvelope EndProcessEpoch {}) : _) = envelope
    findEnd (_ : remaining) = findEnd remaining
    findEnd [] = error "the administration fixture catalogue has no End command"

caseTruncation :: IO ()
caseTruncation =
  mapM_ check allEnvelopes
  where
    check envelope =
      let body = encodeAdminEnvelope envelope
       in assertBool
            ("all strict prefixes fail closed: " <> show envelope)
            (all (isLeft . decodeAdminEnvelope . (`ByteString.take` body)) [0 .. ByteString.length body - 1])

roundTrip :: AdminEnvelope -> Either AdminPayloadError AdminEnvelope
roundTrip = decodeAdminEnvelope . encodeAdminEnvelope

firstEnvelope :: AdminEnvelope
firstEnvelope = case allEnvelopes of
  envelope : _ -> envelope
  [] -> error "the administration fixture catalogue is empty"
