module CodecProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Protocol.Application.Codec
  ( ApplicationPayloadError (..),
    decodeApplicationEnvelope,
    encodeApplicationEnvelope,
  )
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (..),
    ApplicationEnvelope (..),
    ApplicationIsolationOverlayDto (..),
    ApplicationServerDto (..),
    ApplicationSessionUnavailableReasonDto (..),
    applicationClientNonce,
    applicationHeartbeatNonce,
    applicationReplyCursorClaim,
    applicationRequestIdClaim,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    elements,
    forAll,
    testProperty,
    (===),
  )
import TestFixtures
  ( allEnvelopes,
    attachmentClaim,
    labelCasOperation,
    sessionClaim,
  )

tests :: TestTree
tests =
  testGroup
    "generic envelope codec"
    [ testCase "every current constructor fixture round-trips" caseAllConstructorsRoundTrip,
      testProperty "generated current envelopes round-trip" propGeneratedRoundTrip,
      testProperty "generated request and nonce claims survive their DTO positions" propGeneratedClaimFields,
      testCase "heartbeat arms have stable current-build golden bytes" caseHeartbeatGolden,
      testCase "isolation-begin notice has stable current-build golden bytes" caseIsolationBeginGolden,
      testCase "terminal session disposition has stable current-build golden bytes" caseSessionUnavailableGolden,
      testCase "new-environment Call has stable current-build golden bytes" caseNewEnvironmentCallGolden,
      testCase "label calls preserve the explicit expected-label CAS operand" caseLabelCasOperand,
      testCase "trailing bytes inside every declared payload reject" caseTrailingBody,
      testCase "an unknown generic constructor discriminator rejects" caseUnknownConstructor,
      testCase "every strict prefix of a valid envelope rejects" caseTruncation
    ]

caseAllConstructorsRoundTrip :: IO ()
caseAllConstructorsRoundTrip =
  mapM_
    (\envelope -> assertEqual (show envelope) (Right envelope) (roundTrip envelope))
    allEnvelopes

propGeneratedRoundTrip :: Property
propGeneratedRoundTrip =
  forAll (elements allEnvelopes) $ \envelope ->
    counterexample (show envelope) (roundTrip envelope === Right envelope)

propGeneratedClaimFields :: Word64 -> Word64 -> Property
propGeneratedClaimFields requestValue nonceValue =
  conjoin
    ( fmap
        (\envelope -> counterexample (show envelope) (roundTrip envelope === Right envelope))
        [ ApplicationClientEnvelope (OpenSession attachmentClaim (applicationClientNonce nonceValue)),
          ApplicationClientEnvelope (Ping (applicationHeartbeatNonce nonceValue)),
          ApplicationServerEnvelope (Pong (applicationHeartbeatNonce nonceValue)),
          ApplicationClientEnvelope
            (Call sessionClaim (applicationRequestIdClaim requestValue) (WaitApplication []))
        ]
    )

caseHeartbeatGolden :: IO ()
caseHeartbeatGolden = do
  assertEqual
    "Ping envelope bytes"
    heartbeatPingGolden
    (encodeApplicationEnvelope (ApplicationClientEnvelope (Ping heartbeat)))
  assertEqual
    "Pong envelope bytes"
    heartbeatPongGolden
    (encodeApplicationEnvelope (ApplicationServerEnvelope (Pong heartbeat)))
  assertEqual
    "Ping golden decodes"
    (Right (ApplicationClientEnvelope (Ping heartbeat)))
    (decodeApplicationEnvelope heartbeatPingGolden)
  assertEqual
    "Pong golden decodes"
    (Right (ApplicationServerEnvelope (Pong heartbeat)))
    (decodeApplicationEnvelope heartbeatPongGolden)
  where
    heartbeat = applicationHeartbeatNonce 0x0102030405060708

heartbeatPingGolden :: ByteString.ByteString
heartbeatPingGolden = ByteString.pack [0x00, 0x06, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]

heartbeatPongGolden :: ByteString.ByteString
heartbeatPongGolden = ByteString.pack [0x01, 0x07, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]

caseIsolationBeginGolden :: IO ()
caseIsolationBeginGolden = do
  let envelope =
        ApplicationServerEnvelope
          ( HeraldIsolationBegun
              replyCursor
              sessionClaim
              ResidentProcessesZombie
              HeraldIsolatedDto
          )
  assertEqual
    "isolation-begin envelope bytes"
    isolationBeginGolden
    (encodeApplicationEnvelope envelope)
  assertEqual
    "isolation-begin golden decodes"
    (Right envelope)
    (decodeApplicationEnvelope isolationBeginGolden)
  where
    replyCursor = case applicationReplyCursorClaim 7 of
      Left problem -> error (show problem)
      Right checked -> checked

isolationBeginGolden :: ByteString.ByteString
isolationBeginGolden =
  ByteString.pack
    ( [0x01, 0x08]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
        <> [0x00 .. 0x1f]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]
        <> [0x01]
    )

caseSessionUnavailableGolden :: IO ()
caseSessionUnavailableGolden = do
  assertEqual
    "terminal session disposition bytes"
    sessionUnavailableGolden
    (encodeApplicationEnvelope terminalEnvelope)
  assertEqual
    "terminal session disposition golden decodes"
    (Right terminalEnvelope)
    (decodeApplicationEnvelope sessionUnavailableGolden)
  where
    terminalEnvelope =
      ApplicationServerEnvelope
        ( HeraldPermanentlyUnavailable
            sessionClaim
            ApplicationSessionNoLongerLiveDto
        )

sessionUnavailableGolden :: ByteString.ByteString
sessionUnavailableGolden =
  ByteString.pack
    ( [0x01, 0x0e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
        <> [0x00 .. 0x1f]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]
        <> [0x00]
    )

caseNewEnvironmentCallGolden :: IO ()
caseNewEnvironmentCallGolden = do
  assertEqual
    "new-environment Call envelope bytes"
    newEnvironmentCallGolden
    (encodeApplicationEnvelope newEnvironmentCallEnvelope)
  assertEqual
    "new-environment Call golden decodes"
    (Right newEnvironmentCallEnvelope)
    (decodeApplicationEnvelope newEnvironmentCallGolden)
  where
    newEnvironmentCallEnvelope =
      ApplicationClientEnvelope
        (Call sessionClaim (applicationRequestIdClaim 0) NewEnvironmentApplication)

newEnvironmentCallGolden :: ByteString.ByteString
newEnvironmentCallGolden =
  ByteString.pack
    ( [0x00, 0x03]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
        <> [0x00 .. 0x1f]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        <> [0x07]
    )

caseLabelCasOperand :: IO ()
caseLabelCasOperand =
  case labelCasOperation of
    LabelApplication object expected target ->
      case roundTrip
        ( ApplicationClientEnvelope
            (Call sessionClaim (applicationRequestIdClaim 17) labelCasOperation)
        ) of
        Right
          ( ApplicationClientEnvelope
              (Call _ _ (LabelApplication decodedObject decodedExpected decodedTarget))
            ) -> do
            assertEqual "the labelled object survives" object decodedObject
            assertEqual "the expected-label CAS operand survives" expected decodedExpected
            assertEqual "the replacement target survives" target decodedTarget
            let (owner, generation) = expected
                staleExpected = (owner, generation - 1)
                currentEnvelope = ApplicationClientEnvelope (Call sessionClaim (applicationRequestIdClaim 17) labelCasOperation)
                staleEnvelope = ApplicationClientEnvelope (Call sessionClaim (applicationRequestIdClaim 17) (LabelApplication object staleExpected target))
            assertBool
              "a reused request identity with a changed generation has different bytes"
              (encodeApplicationEnvelope currentEnvelope /= encodeApplicationEnvelope staleEnvelope)
            assertEqual "the complete label-pair call has stable current-build bytes" labelCasCallGolden (encodeApplicationEnvelope currentEnvelope)
            assertEqual "the label-pair golden decodes" (Right currentEnvelope) (decodeApplicationEnvelope labelCasCallGolden)
        decoded -> error ("unexpected decoded label call: " <> show decoded)
    _ -> error "labelCasOperation is not a label operation"

labelCasCallGolden :: ByteString.ByteString
labelCasCallGolden =
  ByteString.pack
    ( [0x00, 0x03]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
        <> [0x00 .. 0x1f]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x11]
        <> [0x06]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02]
        <> [0x02]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03]
        <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]
        <> [0x02]
    )

caseTrailingBody :: IO ()
caseTrailingBody =
  mapM_
    ( \envelope ->
        assertEqual
          (show envelope <> ", trailing byte")
          (Left ApplicationPayloadMalformed)
          (decodeApplicationEnvelope (encodeApplicationEnvelope envelope <> ByteString.singleton 0))
    )
    allEnvelopes

caseUnknownConstructor :: IO ()
caseUnknownConstructor =
  assertEqual
    "unknown outer constructor"
    (Left ApplicationPayloadMalformed)
    (decodeApplicationEnvelope (ByteString.singleton 0xff))

caseTruncation :: IO ()
caseTruncation =
  mapM_ assertEveryStrictPrefixRejects allEnvelopes
  where
    assertEveryStrictPrefixRejects envelope =
      let body = encodeApplicationEnvelope envelope
       in mapM_
            ( \splitPoint ->
                assertEqual
                  (show envelope <> ", split point " <> show splitPoint)
                  (Left ApplicationPayloadMalformed)
                  (decodeApplicationEnvelope (ByteString.take splitPoint body))
            )
            [0 .. ByteString.length body - 1]

roundTrip :: ApplicationEnvelope -> Either ApplicationPayloadError ApplicationEnvelope
roundTrip = decodeApplicationEnvelope . encodeApplicationEnvelope
