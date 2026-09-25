{-# LANGUAGE OverloadedRecordDot #-}

-- | Pure typed vocabulary for the Oracle watch and semantic-command client.
--
-- Contacts are checked routing hints, independent of committed voter authority. Attempts, retries, bindings, and
-- semantic dispatches are opaque logical correlations minted by the owner;
-- they are not sockets or Raft identities.
module Eclips.Herald.OracleClient
  ( OracleNodeClaim,
    oracleNodeClaim,
    oracleNodeClaimBytes,
    OracleContact,
    oracleContact,
    oracleContactNode,
    oracleContactHost,
    oracleContactPort,
    OracleContactSet,
    oracleContactSet,
    checkOracleContactSet,
    oracleContacts,
    oracleContactForNode,
    OracleContactProblem (..),
    OracleHelloClaims,
    oracleHelloClaims,
    oracleHelloSystemId,
    oracleHelloCatalogueDigest,
    oracleHelloConfigurationDigest,
    oracleHelloInitialProjectionDigest,
    oracleHelloHeraldId,
    oracleHelloHeraldEpoch,
    oracleHelloMembershipGeneration,
    OracleObservedTerm,
    oracleObservedTerm,
    oracleObservedTermWord64,
    OracleConnectAttempt,
    oracleConnectAttemptOrdinal,
    oracleConnectAttemptContact,
    oracleConnectAttemptFromExclusive,
    OracleRetry,
    oracleRetryOrdinal,
    OracleRetryPurpose (..),
    OracleBindingGeneration,
    oracleBindingGenerationWord64,
    OracleBinding,
    oracleBindingGeneration,
    oracleBindingContact,
    OracleLane,
    oracleCandidateLane,
    oracleEstablishedLane,
    OracleHelloAcceptance,
    oracleHelloAcceptance,
    oracleHelloAcceptedNode,
    oracleHelloAcceptedTerm,
    oracleHelloAcceptedLocalApplied,
    oracleHelloAcceptedLeaderHint,
    oracleHelloAcceptedServiceReady,
    OracleRedirect,
    oracleRedirect,
    oracleRedirectLeaderHint,
    oracleRedirectTerm,
    OracleRequestDispatch,
    oracleRequestDispatchRef,
    oracleRequestDispatchEnvelope,
    OracleClientIngress (..),
    OracleClientAction (..),
    OracleClientDiagnostic (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word64)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    SystemId,
  )
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Startup
  ( CatalogueDigest,
    ConfigurationDigest,
    InitialProjectionDigest,
  )
import Eclips.Herald.OracleClient.Internal
  ( OracleBinding (..),
    OracleBindingGeneration (..),
    OracleClientAction (..),
    OracleClientDiagnostic (..),
    OracleClientIngress (..),
    OracleConnectAttempt (..),
    OracleContact (..),
    OracleContactProblem (..),
    OracleContactSet (..),
    OracleHelloAcceptance (..),
    OracleHelloClaims (..),
    OracleLane (..),
    OracleNodeClaim (..),
    OracleObservedTerm (..),
    OracleRedirect (..),
    OracleRetry (..),
    OracleRetryPurpose (..),
  )
import Eclips.Herald.OracleClient.Request
  ( OracleRequestDispatch,
    oracleRequestDispatchEnvelope,
    oracleRequestDispatchRef,
  )

nodeClaimByteCount :: Int
nodeClaimByteCount = 32

oracleNodeClaim :: ByteString -> Either OracleContactProblem OracleNodeClaim
oracleNodeClaim bytes
  | ByteString.length bytes == nodeClaimByteCount = Right (OracleNodeClaim bytes)
  | otherwise = Left (OracleNodeClaimWrongByteCount (ByteString.length bytes))

oracleNodeClaimBytes :: OracleNodeClaim -> ByteString
oracleNodeClaimBytes (OracleNodeClaim bytes) = bytes

oracleContact ::
  OracleNodeClaim ->
  Text ->
  Word16 ->
  Either OracleContactProblem OracleContact
oracleContact node host port
  | Text.null host = Left OracleContactHostEmpty
  | port == 0 = Left OracleContactPortZero
  | otherwise = Right OracleContact {node, host, port}

oracleContactNode :: OracleContact -> OracleNodeClaim
oracleContactNode = (.node)

oracleContactHost :: OracleContact -> Text
oracleContactHost = (.host)

oracleContactPort :: OracleContact -> Word16
oracleContactPort = (.port)

oracleContactSet ::
  NonEmpty OracleContact ->
  Either OracleContactProblem OracleContactSet
oracleContactSet = checkOracleContactSet . NonEmpty.toList

checkOracleContactSet ::
  [OracleContact] ->
  Either OracleContactProblem OracleContactSet
checkOracleContactSet supplied = do
  contacts <- maybe (Left OracleContactSetEmpty) Right (NonEmpty.nonEmpty supplied)
  rejectDuplicate oracleContactNode OracleContactNodeDuplicate supplied
  rejectDuplicate
    (\contact -> (oracleContactHost contact, oracleContactPort contact))
    (uncurry OracleContactEndpointDuplicate)
    supplied
  Right (OracleContactSet (NonEmpty.sortWith oracleContactNode contacts))

oracleContacts :: OracleContactSet -> NonEmpty OracleContact
oracleContacts (OracleContactSet contacts) = contacts

oracleContactForNode :: OracleNodeClaim -> OracleContactSet -> Maybe OracleContact
oracleContactForNode node =
  find ((== node) . oracleContactNode) . NonEmpty.toList . oracleContacts

rejectDuplicate ::
  (Ord key) =>
  (value -> key) ->
  (key -> problem) ->
  [value] ->
  Either problem ()
rejectDuplicate key problem = go Set.empty
  where
    go _ [] = Right ()
    go seen (value : remaining)
      | current `Set.member` seen = Left (problem current)
      | otherwise = go (Set.insert current seen) remaining
      where
        current = key value

oracleHelloClaims ::
  SystemId ->
  CatalogueDigest ->
  ConfigurationDigest ->
  InitialProjectionDigest ->
  HeraldId ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  OracleHelloClaims
oracleHelloClaims systemId catalogueDigest configurationDigest initialProjectionDigest heraldId heraldEpoch membershipGeneration =
  OracleHelloClaims {systemId, catalogueDigest, configurationDigest, initialProjectionDigest, heraldId, heraldEpoch, membershipGeneration}

oracleHelloSystemId :: OracleHelloClaims -> SystemId
oracleHelloSystemId = (.systemId)

oracleHelloCatalogueDigest :: OracleHelloClaims -> CatalogueDigest
oracleHelloCatalogueDigest = (.catalogueDigest)

oracleHelloConfigurationDigest :: OracleHelloClaims -> ConfigurationDigest
oracleHelloConfigurationDigest = (.configurationDigest)

oracleHelloInitialProjectionDigest :: OracleHelloClaims -> InitialProjectionDigest
oracleHelloInitialProjectionDigest = (.initialProjectionDigest)

oracleHelloHeraldId :: OracleHelloClaims -> HeraldId
oracleHelloHeraldId = (.heraldId)

oracleHelloHeraldEpoch :: OracleHelloClaims -> HeraldEpoch
oracleHelloHeraldEpoch = (.heraldEpoch)

oracleHelloMembershipGeneration ::
  OracleHelloClaims -> HeraldMembershipGenerationId
oracleHelloMembershipGeneration = (.membershipGeneration)

oracleObservedTerm :: Word64 -> OracleObservedTerm
oracleObservedTerm = OracleObservedTerm

oracleObservedTermWord64 :: OracleObservedTerm -> Word64
oracleObservedTermWord64 (OracleObservedTerm term) = term

oracleConnectAttemptOrdinal :: OracleConnectAttempt -> Word64
oracleConnectAttemptOrdinal = (.ordinal)

oracleConnectAttemptContact :: OracleConnectAttempt -> OracleContact
oracleConnectAttemptContact = (.contact)

oracleConnectAttemptFromExclusive :: OracleConnectAttempt -> ControlIndex
oracleConnectAttemptFromExclusive = (.fromExclusive)

oracleRetryOrdinal :: OracleRetry -> Word64
oracleRetryOrdinal (OracleRetry ordinal) = ordinal

oracleBindingGenerationWord64 :: OracleBindingGeneration -> Word64
oracleBindingGenerationWord64 (OracleBindingGeneration generation) = generation

oracleBindingGeneration :: OracleBinding -> OracleBindingGeneration
oracleBindingGeneration = (.generation)

oracleBindingContact :: OracleBinding -> OracleContact
oracleBindingContact = (.contact)

oracleCandidateLane :: OracleConnectAttempt -> OracleLane
oracleCandidateLane = OracleCandidateLane

oracleEstablishedLane :: OracleBinding -> OracleLane
oracleEstablishedLane = OracleEstablishedLane

oracleHelloAcceptance ::
  OracleNodeClaim ->
  OracleObservedTerm ->
  ControlIndex ->
  Maybe OracleNodeClaim ->
  Bool ->
  OracleHelloAcceptance
oracleHelloAcceptance node term localApplied leaderHint serviceReady =
  OracleHelloAcceptance {node, term, localApplied, leaderHint, serviceReady}

oracleHelloAcceptedNode :: OracleHelloAcceptance -> OracleNodeClaim
oracleHelloAcceptedNode = (.node)

oracleHelloAcceptedTerm :: OracleHelloAcceptance -> OracleObservedTerm
oracleHelloAcceptedTerm = (.term)

oracleHelloAcceptedLocalApplied :: OracleHelloAcceptance -> ControlIndex
oracleHelloAcceptedLocalApplied = (.localApplied)

oracleHelloAcceptedLeaderHint :: OracleHelloAcceptance -> Maybe OracleNodeClaim
oracleHelloAcceptedLeaderHint = (.leaderHint)

oracleHelloAcceptedServiceReady :: OracleHelloAcceptance -> Bool
oracleHelloAcceptedServiceReady = (.serviceReady)

oracleRedirect :: Maybe OracleNodeClaim -> OracleObservedTerm -> OracleRedirect
oracleRedirect leaderHint term = OracleRedirect {leaderHint, term}

oracleRedirectLeaderHint :: OracleRedirect -> Maybe OracleNodeClaim
oracleRedirectLeaderHint = (.leaderHint)

oracleRedirectTerm :: OracleRedirect -> OracleObservedTerm
oracleRedirectTerm = (.term)
