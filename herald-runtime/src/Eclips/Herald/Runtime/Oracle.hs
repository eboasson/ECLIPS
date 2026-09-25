-- | Pure EORC-to-Herald adapter for the outbound watch/configured-request lane.
--
-- Socket ownership and incremental frame admission remain in the TCP shell;
-- this module performs the exact nominal DTO/core conversions before an
-- observation can enter the Herald owner.
module Eclips.Herald.Runtime.Oracle
  ( OracleRuntimeAdapterError (..),
    oracleConnectHelloEnvelope,
    oracleHealthQueryEnvelope,
    oracleHealthReplyObservation,
    oracleRequestEnvelope,
    oracleProgressEnvelope,
    oracleProgressConfirmedIngress,
    oracleProgressNotReadyIngress,
    oracleRequestRetiredIngress,
    oracleWatchEnvelope,
    oracleHelloIngress,
    oracleRedirectIngress,
    oracleSubmissionNotReadyIngress,
    oracleSubmissionDeferredIngress,
    oracleEntriesIngress,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word64)
import Eclips.Domain.Identity (ControlIndex, controlIndex)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientIngress (..),
    OracleConnectAttempt,
    OracleContactProblem,
    OracleHelloAcceptance,
    OracleHelloClaims,
    OracleLane,
    OracleNodeClaim,
    OracleRedirect,
    OracleRequestDispatch,
    oracleConnectAttemptFromExclusive,
    oracleHelloAcceptance,
    oracleHelloCatalogueDigest,
    oracleHelloConfigurationDigest,
    oracleHelloHeraldEpoch,
    oracleHelloHeraldId,
    oracleHelloInitialProjectionDigest,
    oracleHelloMembershipGeneration,
    oracleHelloSystemId,
    oracleNodeClaim,
    oracleObservedTerm,
    oracleRedirect,
    oracleRequestDispatchEnvelope,
  )
import Eclips.Herald.OracleHealth (OracleHealthObservation, oracleHealthObservation)
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    CanonicalOracleEnvelope,
  )
import Eclips.Protocol.Oracle.Codec
  ( OracleAdapterError,
    canonicalOracleEnvelopeDtoFromCore,
    catalogueDigestClaimFromDomain,
    configurationDigestClaimFromDomain,
    controlIndexDtoFromDomain,
    controlIndexDtoToDomain,
    heraldEpochClaimFromDomain,
    heraldIdClaimFromDomain,
    heraldMembershipGenerationClaimFromDomain,
    initialProjectionDigestClaimFromDomain,
    labelDecisionIdClaimToDomain,
    oracleClientRequestIdDtoToCore,
    oracleProgressDtoToCore,
    oracleRequestRetirementDtoToCore,
    raftNodeIdClaimToCore,
    raftTermDtoToCore,
    systemIdClaimFromDomain,
  )
import Eclips.Protocol.Oracle.Types
  ( ControlIndexDto,
    LabelDecisionIdClaim,
    OracleClientRequestIdDto,
    OracleHelloAcceptedDto,
    OracleProtocolEnvelope (..),
    OracleRedirectDto,
    RaftNodeIdClaim,
    oracleHelloAcceptedCurrentTerm,
    oracleHelloAcceptedLeaderHint,
    oracleHelloAcceptedLocalAppliedControlIndex,
    oracleHelloAcceptedRaftNodeId,
    oracleHelloAcceptedServiceReady,
    oracleHelloDto,
    oracleRedirectCurrentTerm,
    oracleRedirectLeaderHint,
    raftNodeIdClaimBytes,
    raftTermDtoWord64,
  )
import Eclips.Protocol.Oracle.Types qualified as Protocol

-- | A checked server claim could not enter the Herald's nominal vocabulary.
data OracleRuntimeAdapterError
  = -- | A server-supplied Raft node claim was not exactly 32 bytes.
    OracleRuntimeNodeClaimRejected OracleContactProblem
  | OracleRuntimeSemanticClaimRejected OracleAdapterError
  deriving stock (Eq, Show)

-- | Encode the Hello for an owner-minted physical connect attempt.
oracleConnectHelloEnvelope ::
  OracleConnectAttempt ->
  HeraldMembershipGenerationId ->
  OracleHelloClaims ->
  OracleProtocolEnvelope
oracleConnectHelloEnvelope attempt membershipGeneration claims =
  OracleClientEnvelope
    ( Protocol.OracleHello
        (helloClaimsDto (oracleConnectAttemptFromExclusive attempt) membershipGeneration claims)
    )

-- | Health is a fresh one-shot owner query; its Hello carries immutable run
-- identity without claiming an ordinary watch or command-admission binding.
oracleHealthQueryEnvelope :: Word64 -> OracleHelloClaims -> OracleProtocolEnvelope
oracleHealthQueryEnvelope roundNumber claims =
  OracleClientEnvelope
    (Protocol.OracleHealthQuery roundNumber (helloClaimsDto (controlIndex 0) (oracleHelloMembershipGeneration claims) claims))

oracleHealthReplyObservation :: Protocol.OracleHealthReplyDto -> Either OracleRuntimeAdapterError OracleHealthObservation
oracleHealthReplyObservation reply = do
  node <- either (Left . OracleRuntimeSemanticClaimRejected) Right (raftNodeIdClaimToCore (Protocol.oracleHealthReplyNode reply))
  let (reference, configuration) = Protocol.oracleHealthReplyConfiguration reply
  pure
    ( oracleHealthObservation
        node
        (raftTermDtoToCore (Protocol.oracleHealthReplyTerm reply))
        (Protocol.oracleHealthReplyGeneration reply)
        reference
        configuration
    )

helloClaimsDto :: ControlIndex -> HeraldMembershipGenerationId -> OracleHelloClaims -> Protocol.OracleHelloDto
helloClaimsDto cursor membership claims =
  oracleHelloDto
    (systemIdClaimFromDomain (oracleHelloSystemId claims))
    (catalogueDigestClaimFromDomain (oracleHelloCatalogueDigest claims))
    (configurationDigestClaimFromDomain (oracleHelloConfigurationDigest claims))
    (initialProjectionDigestClaimFromDomain (oracleHelloInitialProjectionDigest claims))
    (heraldIdClaimFromDomain (oracleHelloHeraldId claims))
    (heraldEpochClaimFromDomain (oracleHelloHeraldEpoch claims))
    (controlIndexDtoFromDomain cursor)
    (heraldMembershipGenerationClaimFromDomain membership)

-- | Encode a watch request from the greatest contiguously applied cursor.
oracleWatchEnvelope :: ControlIndex -> OracleProtocolEnvelope
oracleWatchEnvelope cursor =
  OracleClientEnvelope (Protocol.WatchOracle (controlIndexDtoFromDomain cursor))

-- | Encode one owner-retained request for its exact established
-- Oracle binding. The dispatch remains level-triggered until projection.
oracleRequestEnvelope ::
  OracleRequestDispatch ->
  OracleProtocolEnvelope
oracleRequestEnvelope dispatch =
  OracleClientEnvelope
    ( Protocol.SubmitOracleCommand
        ( canonicalOracleEnvelopeDtoFromCore
            (oracleRequestDispatchEnvelope dispatch)
        )
    )

-- Maintenance carries both home-owned progress promises, without minting a
-- semantic request ID or retaining another ordinary receipt.
oracleProgressEnvelope :: CanonicalOracleEnvelope -> OracleProtocolEnvelope
oracleProgressEnvelope = OracleClientEnvelope . Protocol.SubmitOracleCommand . canonicalOracleEnvelopeDtoFromCore

oracleProgressConfirmedIngress :: OracleBinding -> OracleClientRequestIdDto -> Protocol.OracleProgressDto -> Either OracleRuntimeAdapterError OracleClientIngress
oracleProgressConfirmedIngress binding request through = do
  checkedRequest <- mapLeft OracleRuntimeSemanticClaimRejected (oracleClientRequestIdDtoToCore request)
  pure (OracleProgressConfirmed binding checkedRequest (oracleProgressDtoToCore through))

oracleProgressNotReadyIngress :: OracleBinding -> OracleClientRequestIdDto -> Protocol.OracleProgressDto -> Protocol.RaftTermDto -> Either OracleRuntimeAdapterError OracleClientIngress
oracleProgressNotReadyIngress binding request offered term = do
  checkedRequest <- mapLeft OracleRuntimeSemanticClaimRejected (oracleClientRequestIdDtoToCore request)
  pure (OracleProgressNotReadyReceived binding checkedRequest (oracleProgressDtoToCore offered) (oracleObservedTerm (raftTermDtoWord64 term)))

oracleRequestRetiredIngress :: OracleBinding -> OracleClientRequestIdDto -> Protocol.OracleRequestRetirementDto -> Either OracleRuntimeAdapterError OracleClientIngress
oracleRequestRetiredIngress binding request reason = do
  checkedRequest <- mapLeft OracleRuntimeSemanticClaimRejected (oracleClientRequestIdDtoToCore request)
  pure (OracleRequestRetiredReceived binding checkedRequest (oracleRequestRetirementDtoToCore reason))

-- | Convert a checked accepted-Hello DTO for its exact candidate attempt.
oracleHelloIngress ::
  OracleConnectAttempt ->
  OracleHelloAcceptedDto ->
  Either OracleRuntimeAdapterError OracleClientIngress
oracleHelloIngress attempt accepted =
  OracleHelloReceived attempt <$> decodeAcceptance accepted

-- | Convert a checked redirect DTO for its candidate or established lane.
oracleRedirectIngress ::
  OracleLane ->
  OracleRedirectDto ->
  Either OracleRuntimeAdapterError OracleClientIngress
oracleRedirectIngress lane redirected =
  OracleRedirectReceived lane <$> decodeRedirect redirected

-- | Preserve an established lane while correlating transient proposal
-- unavailability with the exact owner-retained request that may be reoffered later.
oracleSubmissionNotReadyIngress ::
  OracleBinding ->
  OracleClientRequestIdDto ->
  Protocol.RaftTermDto ->
  Either OracleRuntimeAdapterError OracleClientIngress
oracleSubmissionNotReadyIngress binding request term = do
  checkedRequest <-
    mapLeft
      OracleRuntimeSemanticClaimRejected
      (oracleClientRequestIdDtoToCore request)
  pure
    ( OracleSubmissionNotReadyReceived
        binding
        checkedRequest
        (oracleObservedTerm (raftTermDtoWord64 term))
    )

oracleSubmissionDeferredIngress ::
  OracleBinding ->
  OracleClientRequestIdDto ->
  LabelDecisionIdClaim ->
  ControlIndexDto ->
  Either OracleRuntimeAdapterError OracleClientIngress
oracleSubmissionDeferredIngress binding request decision observedIndex = do
  checkedRequest <-
    mapLeft
      OracleRuntimeSemanticClaimRejected
      (oracleClientRequestIdDtoToCore request)
  checkedDecision <-
    mapLeft
      OracleRuntimeSemanticClaimRejected
      (labelDecisionIdClaimToDomain decision)
  pure
    ( OracleSubmissionDeferredReceived
        binding
        checkedRequest
        checkedDecision
        (controlIndexDtoToDomain observedIndex)
    )

-- | Attach decoded canonical entries to their exact established binding.
oracleEntriesIngress ::
  OracleBinding ->
  NonEmpty CanonicalAppliedOracleEntry ->
  OracleClientIngress
oracleEntriesIngress binding entries =
  OracleEntriesReceived binding entries

decodeAcceptance ::
  OracleHelloAcceptedDto ->
  Either OracleRuntimeAdapterError OracleHelloAcceptance
decodeAcceptance accepted = do
  node <- claimNode (oracleHelloAcceptedRaftNodeId accepted)
  hint <- traverse claimNode (oracleHelloAcceptedLeaderHint accepted)
  pure
    ( oracleHelloAcceptance
        node
        (oracleObservedTerm (raftTermDtoWord64 (oracleHelloAcceptedCurrentTerm accepted)))
        (controlIndexDtoToDomain (oracleHelloAcceptedLocalAppliedControlIndex accepted))
        hint
        (oracleHelloAcceptedServiceReady accepted)
    )

decodeRedirect ::
  OracleRedirectDto ->
  Either OracleRuntimeAdapterError OracleRedirect
decodeRedirect redirected = do
  hint <- traverse claimNode (oracleRedirectLeaderHint redirected)
  pure
    ( oracleRedirect
        hint
        (oracleObservedTerm (raftTermDtoWord64 (oracleRedirectCurrentTerm redirected)))
    )

claimNode ::
  RaftNodeIdClaim ->
  Either OracleRuntimeAdapterError OracleNodeClaim
claimNode =
  mapLeft OracleRuntimeNodeClaimRejected
    . oracleNodeClaim
    . raftNodeIdClaimBytes

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft transform = \case
  Left problem -> Left (transform problem)
  Right value -> Right value
