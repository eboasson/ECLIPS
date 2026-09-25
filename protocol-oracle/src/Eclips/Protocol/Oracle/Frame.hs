-- | Fixed EORC framing and its pure direction-aware incremental decoder.
module Eclips.Protocol.Oracle.Frame
  ( oracleFrameMagic,
    OracleFrameDirection (..),
    OracleFrameError (..),
    OracleFrameDecoder,
    initialOracleFrameDecoder,
    OracleFrameFeedResult (..),
    OracleFrameContinuation (..),
    encodeOracleFrame,
    feedOracleFrame,
    finishOracleFrameDecoder,
    OracleIngressContext,
    oracleServerIngressContext,
    oracleClientIngressContext,
    OracleIngressError (..),
    OracleIngressDecoder,
    initialOracleIngressDecoder,
    OracleIngressAuthorityUpdateError (..),
    OracleIngressAuthorityUpdateResult (..),
    updateOracleClientIngressAuthority,
    invalidateOracleClientIngressAuthority,
    OracleIngressFeedResult (..),
    OracleIngressContinuation (..),
    feedOracleIngress,
    finishOracleIngressDecoder,
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import Eclips.Protocol.Frame qualified as Frame
import Eclips.Protocol.Oracle.Codec
  ( OraclePayloadError,
    decodeOracleEnvelope,
    encodeOracleEnvelope,
  )
import Eclips.Protocol.Oracle.Types
  ( OracleAuthorityError,
    OracleClientMessage (OracleHealthQuery, OracleHello),
    OracleHelloAcceptedDto,
    OracleHelloContext,
    OracleLanePhase (AwaitingOracleHello),
    OracleLaneRole (..),
    OracleProtocolEnvelope (..),
    OracleRedirectDto,
    OracleRequestAbsenceAuthority,
    OracleRoleError,
    OracleServerMessage (OracleHelloAccepted, OracleRedirect),
    OracleServiceReadyLeaderContext,
    OracleShapeError,
    admitOracleEnvelopeAuthority,
    admitOracleEnvelopeRole,
    admitOracleHelloAuthority,
    noOracleRequestAbsenceAuthority,
    validateOracleHelloContext,
  )

-- | Fixed ASCII @EORC@ Oracle-client-family discriminator.
oracleFrameMagic :: Word32
oracleFrameMagic = 0x454f5243

-- | The envelope arm admitted by one endpoint decoder.
data OracleFrameDirection
  = OracleClientFrames
  | OracleServerFrames
  deriving stock (Eq, Show, Enum, Bounded)

data OracleFrameError
  = OracleFrameInvalidLength
  | OracleFrameWrongFamily
  | OracleFrameWrongEnvelopeDirection
  | OracleFramePayloadError OraclePayloadError
  | OracleFrameTruncated
  deriving stock (Eq, Show)

data OracleFrameDecoder
  = OracleFrameDecoder OracleFrameDirection Frame.FrameDecoder
  deriving stock (Eq, Show)

initialOracleFrameDecoder :: OracleFrameDirection -> OracleFrameDecoder
initialOracleFrameDecoder direction =
  OracleFrameDecoder direction (Frame.initialFrameDecoder oracleFrameMagic)

data OracleFrameFeedResult
  = OracleFrameFeedResult [OracleProtocolEnvelope] OracleFrameContinuation
  deriving stock (Eq, Show)

data OracleFrameContinuation
  = NeedOracleFrameBytes OracleFrameDecoder
  | OracleFrameFailed OracleFrameError
  deriving stock (Eq, Show)

encodeOracleFrame :: OracleProtocolEnvelope -> ByteString
encodeOracleFrame = Frame.encodeFrame oracleFrameMagic . encodeOracleEnvelope

-- | Feed a TCP-shaped byte chunk while preserving every valid prefix envelope.
feedOracleFrame :: OracleFrameDecoder -> ByteString -> OracleFrameFeedResult
feedOracleFrame (OracleFrameDecoder direction decoder) chunk =
  decodeBodies direction (Frame.feedFrame decoder chunk) []

finishOracleFrameDecoder :: OracleFrameDecoder -> Either OracleFrameError ()
finishOracleFrameDecoder (OracleFrameDecoder _ decoder) =
  either (Left . oracleFrameError) Right (Frame.finishFrameDecoder decoder)

-- | Static semantic context attached to one physical EORC lane. A server
-- ingress validates the remote Herald's deployment Hello. A client ingress
-- starts without absence authority and retains the exact accepted server Hello
-- against which later out-of-band authority updates are checked.
data OracleIngressContext
  = OracleServerIngressContext OracleHelloContext
  | OracleClientIngressContext
  deriving stock (Eq, Show)

oracleServerIngressContext :: OracleHelloContext -> OracleIngressContext
oracleServerIngressContext = OracleServerIngressContext

oracleClientIngressContext :: OracleIngressContext
oracleClientIngressContext = OracleClientIngressContext

data OracleIngressError
  = OracleIngressFrameError OracleFrameError
  | OracleIngressRoleError OracleRoleError
  | OracleIngressShapeError OracleShapeError
  | OracleIngressAuthorityError OracleAuthorityError
  deriving stock (Eq, Show)

data OracleIngressDecoder
  = OracleIngressDecoder
      OracleIngressContext
      OracleLanePhase
      (Maybe OracleHelloAcceptedDto)
      OracleRequestAbsenceAuthority
      OracleFrameDecoder
  deriving stock (Eq, Show)

initialOracleIngressDecoder :: OracleIngressContext -> OracleIngressDecoder
initialOracleIngressDecoder context =
  OracleIngressDecoder
    context
    AwaitingOracleHello
    Nothing
    noOracleRequestAbsenceAuthority
    (initialOracleFrameDecoder (ingressDirection context))

-- | Why one authority update could not be attached to this decoder. Every
-- rejected result carries the successor with any earlier authority cleared.
data OracleIngressAuthorityUpdateError
  = OracleIngressAuthorityRequiresClientLane
  | OracleIngressAuthorityRequiresAcceptedHello
  | OracleIngressAuthorityContextError OracleAuthorityError
  deriving stock (Eq, Show)

data OracleIngressAuthorityUpdateResult
  = OracleIngressAuthorityUpdated OracleIngressDecoder
  | OracleIngressAuthorityUpdateRejected
      OracleIngressAuthorityUpdateError
      OracleIngressDecoder
  deriving stock (Eq, Show)

-- | Install or clear current runtime-owned absence authority. Positive
-- authority must exactly match the node, term, service-ready fact, and complete
-- prefix of the Hello accepted on this connection. A mismatch invalidates any
-- earlier authority before it is reported.
updateOracleClientIngressAuthority ::
  OracleServiceReadyLeaderContext ->
  OracleIngressDecoder ->
  OracleIngressAuthorityUpdateResult
updateOracleClientIngressAuthority candidate (OracleIngressDecoder context phase acceptedHello _ frameDecoder) =
  case context of
    OracleServerIngressContext _ ->
      OracleIngressAuthorityUpdateRejected
        OracleIngressAuthorityRequiresClientLane
        cleared
    OracleClientIngressContext -> case acceptedHello of
      Nothing ->
        OracleIngressAuthorityUpdateRejected
          OracleIngressAuthorityRequiresAcceptedHello
          cleared
      Just hello -> case admitOracleHelloAuthority hello candidate of
        Left authorityError ->
          OracleIngressAuthorityUpdateRejected
            (OracleIngressAuthorityContextError authorityError)
            cleared
        Right authority ->
          OracleIngressAuthorityUpdated
            ( OracleIngressDecoder
                context
                phase
                acceptedHello
                authority
                frameDecoder
            )
  where
    cleared =
      OracleIngressDecoder
        context
        phase
        acceptedHello
        noOracleRequestAbsenceAuthority
        frameDecoder

-- | Explicitly clear authority when runtime-owned leadership, readiness, term,
-- or prefix facts change before another exact connection context is available.
invalidateOracleClientIngressAuthority ::
  OracleIngressDecoder ->
  OracleIngressDecoder
invalidateOracleClientIngressAuthority (OracleIngressDecoder context phase acceptedHello _ frameDecoder) =
  OracleIngressDecoder
    context
    phase
    acceptedHello
    noOracleRequestAbsenceAuthority
    frameDecoder

data OracleIngressFeedResult
  = OracleIngressFeedResult
      [OracleProtocolEnvelope]
      OracleIngressContinuation
  deriving stock (Eq, Show)

data OracleIngressContinuation
  = NeedOracleIngressBytes OracleIngressDecoder
  | OracleIngressRedirected OracleRedirectDto
  | OracleIngressFailed OracleIngressError
  deriving stock (Eq, Show)

-- | Production ingress composition: frame family and direction, exact body
-- decoding, Hello phase, deployment comparison, and terminal-absence authority
-- are admitted in that order. A valid prefix is retained before any later
-- failure in the same TCP chunk.
feedOracleIngress :: OracleIngressDecoder -> ByteString -> OracleIngressFeedResult
feedOracleIngress (OracleIngressDecoder context phase acceptedHello authority decoder) chunk =
  case feedOracleFrame decoder chunk of
    OracleFrameFeedResult envelopes continuation ->
      admitIngressEnvelopes
        context
        phase
        acceptedHello
        authority
        envelopes
        continuation
        []

finishOracleIngressDecoder :: OracleIngressDecoder -> Either OracleIngressError ()
finishOracleIngressDecoder (OracleIngressDecoder _ _ _ _ decoder) =
  either (Left . OracleIngressFrameError) Right (finishOracleFrameDecoder decoder)

decodeBodies :: OracleFrameDirection -> Frame.FrameFeedResult -> [OracleProtocolEnvelope] -> OracleFrameFeedResult
decodeBodies direction (Frame.FrameFeedResult [] continuation) reversedEnvelopes =
  OracleFrameFeedResult
    (reverse reversedEnvelopes)
    (oracleFrameContinuation direction continuation)
decodeBodies direction (Frame.FrameFeedResult (body : bodies) continuation) reversedEnvelopes =
  case decodeOracleEnvelope body of
    Left payloadError ->
      OracleFrameFeedResult
        (reverse reversedEnvelopes)
        (OracleFrameFailed (OracleFramePayloadError payloadError))
    Right envelope
      | directionAccepts direction envelope ->
          decodeBodies
            direction
            (Frame.FrameFeedResult bodies continuation)
            (envelope : reversedEnvelopes)
      | otherwise ->
          OracleFrameFeedResult
            (reverse reversedEnvelopes)
            (OracleFrameFailed OracleFrameWrongEnvelopeDirection)

admitIngressEnvelopes ::
  OracleIngressContext ->
  OracleLanePhase ->
  Maybe OracleHelloAcceptedDto ->
  OracleRequestAbsenceAuthority ->
  [OracleProtocolEnvelope] ->
  OracleFrameContinuation ->
  [OracleProtocolEnvelope] ->
  OracleIngressFeedResult
admitIngressEnvelopes context phase acceptedHello authority [] continuation reversedEnvelopes =
  OracleIngressFeedResult
    (reverse reversedEnvelopes)
    (oracleIngressContinuation context phase acceptedHello authority continuation)
admitIngressEnvelopes context phase acceptedHello authority (envelope : envelopes) continuation reversedEnvelopes =
  case admitIngressEnvelope context phase acceptedHello authority envelope of
    Left ingressError ->
      OracleIngressFeedResult
        (reverse reversedEnvelopes)
        (OracleIngressFailed ingressError)
    Right (OracleIngressRedirect redirect) ->
      OracleIngressFeedResult
        (reverse (envelope : reversedEnvelopes))
        (OracleIngressRedirected redirect)
    Right (OracleIngressContinue nextPhase nextHello nextAuthority) ->
      admitIngressEnvelopes
        context
        nextPhase
        nextHello
        nextAuthority
        envelopes
        continuation
        (envelope : reversedEnvelopes)

data OracleIngressAdmission
  = OracleIngressContinue
      OracleLanePhase
      (Maybe OracleHelloAcceptedDto)
      OracleRequestAbsenceAuthority
  | OracleIngressRedirect OracleRedirectDto

admitIngressEnvelope ::
  OracleIngressContext ->
  OracleLanePhase ->
  Maybe OracleHelloAcceptedDto ->
  OracleRequestAbsenceAuthority ->
  OracleProtocolEnvelope ->
  Either OracleIngressError OracleIngressAdmission
admitIngressEnvelope context phase acceptedHello authority envelope = do
  nextPhase <-
    mapLeft
      OracleIngressRoleError
      (admitOracleEnvelopeRole (ingressRole context) phase envelope)
  admitIngressDeployment context envelope
  case envelope of
    OracleServerEnvelope (OracleRedirect redirect) ->
      Right (OracleIngressRedirect redirect)
    OracleServerEnvelope (OracleHelloAccepted hello) ->
      Right
        ( OracleIngressContinue
            nextPhase
            (Just hello)
            noOracleRequestAbsenceAuthority
        )
    _ -> do
      mapLeft
        OracleIngressAuthorityError
        (admitOracleEnvelopeAuthority authority envelope)
      Right
        ( OracleIngressContinue
            nextPhase
            acceptedHello
            authority
        )

admitIngressDeployment :: OracleIngressContext -> OracleProtocolEnvelope -> Either OracleIngressError ()
admitIngressDeployment (OracleServerIngressContext expected) (OracleClientEnvelope (OracleHello actual)) =
  mapLeft OracleIngressShapeError (validateOracleHelloContext expected actual)
admitIngressDeployment (OracleServerIngressContext expected) (OracleClientEnvelope (OracleHealthQuery _ actual)) =
  mapLeft OracleIngressShapeError (validateOracleHelloContext expected actual)
admitIngressDeployment _ _ = Right ()

oracleIngressContinuation ::
  OracleIngressContext ->
  OracleLanePhase ->
  Maybe OracleHelloAcceptedDto ->
  OracleRequestAbsenceAuthority ->
  OracleFrameContinuation ->
  OracleIngressContinuation
oracleIngressContinuation context phase acceptedHello authority (NeedOracleFrameBytes decoder) =
  NeedOracleIngressBytes
    ( OracleIngressDecoder
        context
        phase
        acceptedHello
        authority
        decoder
    )
oracleIngressContinuation _ _ _ _ (OracleFrameFailed frameError) =
  OracleIngressFailed (OracleIngressFrameError frameError)

ingressRole :: OracleIngressContext -> OracleLaneRole
ingressRole (OracleServerIngressContext _) = OracleServerLane
ingressRole OracleClientIngressContext = OracleClientLane

ingressDirection :: OracleIngressContext -> OracleFrameDirection
ingressDirection (OracleServerIngressContext _) = OracleClientFrames
ingressDirection OracleClientIngressContext = OracleServerFrames

oracleFrameContinuation :: OracleFrameDirection -> Frame.FrameContinuation -> OracleFrameContinuation
oracleFrameContinuation direction (Frame.NeedFrameBytes decoder) =
  NeedOracleFrameBytes (OracleFrameDecoder direction decoder)
oracleFrameContinuation _ (Frame.FrameFailed frameError) =
  OracleFrameFailed (oracleFrameError frameError)

oracleFrameError :: Frame.FrameError -> OracleFrameError
oracleFrameError Frame.FrameInvalidLength = OracleFrameInvalidLength
oracleFrameError Frame.FrameWrongFamily = OracleFrameWrongFamily
oracleFrameError Frame.FrameTruncated = OracleFrameTruncated

directionAccepts :: OracleFrameDirection -> OracleProtocolEnvelope -> Bool
directionAccepts OracleClientFrames (OracleClientEnvelope _) = True
directionAccepts OracleServerFrames (OracleServerEnvelope _) = True
directionAccepts _ _ = False

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft f = \case
  Left problem -> Left (f problem)
  Right value -> Right value
