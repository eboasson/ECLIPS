{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Immutable explicit routing plans and generation birth evidence crossing a
-- sealed admission boundary. No obligations, subscriptions or Store contents.
module Eclips.Herald.Alignment.History
  ( AlignmentHistory,
    captureAlignmentHistory,
    alignmentHistoryPlans,
    alignmentHistoryFrontier,
    alignmentHistoryReadiness,
    alignmentHistoryCertificates,
    alignmentHistoryCoverage,
    alignmentHistoryPlanAcceptances,
    withAlignmentHistoryPlanAcceptances,
    encodeAlignmentHistory,
    decodeAlignmentHistory,
  ) where

import Control.Monad (foldM, unless, (>=>))
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Serialize qualified as S
import Data.Set qualified as Set
import Eclips.Domain.Alignment
import Eclips.Domain.Identity
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Peer.RPC.Internal qualified as Bridge
import Eclips.Herald.Structural.Debt
import Eclips.Protocol.Peer.Types qualified as Wire
import GHC.Generics (Generic)

data AlignmentHistory
  = AlignmentHistory
      [Protocol.AlignmentPlanAnnounce]
      [(SortOccurrence, Plan.AlignmentPlanId)]
      [Protocol.ClassMemberReady]
      [Protocol.HistoricalCertificate]
      [(Alignment.AlignmentPromotionKey, [ContextClassGenerationId])]
      [Protocol.AlignmentPlanAccepted]
  deriving stock (Eq, Show)

alignmentHistoryPlans :: AlignmentHistory -> [Protocol.AlignmentPlanAnnounce]
alignmentHistoryPlans (AlignmentHistory plans _ _ _ _ _) = plans
alignmentHistoryFrontier :: AlignmentHistory -> [(SortOccurrence, Plan.AlignmentPlanId)]
alignmentHistoryFrontier (AlignmentHistory _ frontier _ _ _ _) = frontier
alignmentHistoryReadiness :: AlignmentHistory -> [Protocol.ClassMemberReady]
alignmentHistoryReadiness (AlignmentHistory _ _ readiness _ _ _) = readiness
alignmentHistoryCertificates :: AlignmentHistory -> [Protocol.HistoricalCertificate]
alignmentHistoryCertificates (AlignmentHistory _ _ _ certificates _ _) = certificates
alignmentHistoryCoverage :: AlignmentHistory -> [(Alignment.AlignmentPromotionKey, [ContextClassGenerationId])]
alignmentHistoryCoverage (AlignmentHistory _ _ _ _ coverage _) = coverage
alignmentHistoryPlanAcceptances :: AlignmentHistory -> [Protocol.AlignmentPlanAccepted]
alignmentHistoryPlanAcceptances (AlignmentHistory _ _ _ _ _ acceptances) = acceptances

-- Inherited acceptances remain passive facts, never the observer's local votes.
withAlignmentHistoryPlanAcceptances :: [Protocol.AlignmentPlanAccepted] -> AlignmentHistory -> Either String AlignmentHistory
withAlignmentHistoryPlanAcceptances supplied (AlignmentHistory plans frontier readiness certificates coverage incumbent) = do
  accepted <- foldM insert (Map.fromList [(acceptanceKey value, value) | value <- incumbent]) supplied
  pure (AlignmentHistory plans frontier readiness certificates coverage (Map.elems accepted))
  where
    retainedPlans = Map.fromList [(Protocol.alignmentPlanAnnounceId plan, plan) | plan <- plans]
    insert retained value
      | Map.notMember (Protocol.alignmentPlanAcceptedId value) retainedPlans = Right retained
      | otherwise = case Map.lookup (acceptanceKey value) retained of
          Just previous | previous /= value -> Left "conflicting historical plan acceptance"
          _ -> Right (Map.insert (acceptanceKey value) value retained)

acceptanceKey :: Protocol.AlignmentPlanAccepted -> (Plan.AlignmentPlanId, HeraldEpoch)
acceptanceKey value = (Protocol.alignmentPlanAcceptedId value, Protocol.alignmentPlanAcceptedHerald value)

-- Parent plans retain all generation birth cuts needed by carried bindings.
-- Empty plans survive because closure follows explicit IDs rather than classes.
captureAlignmentHistory :: Alignment.State -> AlignmentHistory
captureAlignmentHistory state = AlignmentHistory plans frontier readiness certificates coverage acceptances
  where
    retained = Map.fromList (Alignment.alignmentPlanEntries state)
    frontier = Alignment.historicalAlignmentPlanFrontier state
    coverage = Alignment.alignmentCompletedCauseCoverage state
    required = close (Set.fromList (map snd frontier <> map (coveragePlanId . fst) coverage))
    close identifiers =
      let extended = Set.union identifiers (Set.fromList [prior | identifier <- Set.toAscList identifiers, Just prior <- [Plan.alignmentPlanPredecessor (requiredPlan identifier)]])
       in if extended == identifiers then identifiers else close extended
    requiredPlan identifier = maybe (error "alignment history requires an absent checked plan") id (Map.lookup identifier retained)
    plans = [Plan.alignmentPlanAnnouncement (requiredPlan identifier) | identifier <- Set.toAscList required]
    generationIds = Set.fromList [Protocol.alignmentCutAnnounceGeneration cut | plan <- plans, cut <- Protocol.alignmentPlanAnnounceCreatedCuts plan]
    readiness = [ready | ((identifier, _), ready) <- Alignment.alignmentMemberReadinessEntries state, Set.member identifier generationIds]
    certificates = [certificate | ((identifier, _), certificate) <- Alignment.alignmentHistoricalCertificateEntries state, Set.member identifier generationIds]
    acceptances = [accepted | ((identifier, _), accepted) <- Alignment.alignmentPlanAcceptanceEntries state, Set.member identifier required]

data RawCoverage = RawCoverage ByteString ByteString [ByteString]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawHistory = RawHistory [ByteString] [(ByteString, ByteString, ByteString)] [ByteString] [ByteString] [RawCoverage] [ByteString]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

encodeAlignmentHistory :: AlignmentHistory -> ByteString
encodeAlignmentHistory (AlignmentHistory plans frontier readiness certificates coverage acceptances) =
  S.encode
    ( "ECLIPS-ALIGNMENT-HISTORY" :: ByteString,
      RawHistory
        (map (encodeControl . Protocol.AlignmentPlanAnnounced) plans)
        [(sortIdBytes (sortOccurrenceSortId occurrence), sortDefinitionOccurrenceIdBytes (sortOccurrenceDefinition occurrence), encodeChecked Bridge.alignmentPlanIdToDto identifier) | (occurrence, identifier) <- frontier]
        (map (encodeEvidence . Protocol.AlignmentMemberReadyAdvertised) readiness)
        (map (encodeControl . Protocol.AlignmentHistoricalCertificateAdvertised) certificates)
        (map encodeCoverage coverage)
        (map (encodeEvidence . Protocol.AlignmentPlanAcceptanceAdvertised) acceptances)
    )

decodeAlignmentHistory :: ByteString -> Either String AlignmentHistory
decodeAlignmentHistory bytes = do
  (domain, RawHistory rawPlans rawFrontier rawReadiness rawCertificates rawCoverage rawAcceptances) <- S.decode bytes
  unless (domain == ("ECLIPS-ALIGNMENT-HISTORY" :: ByteString)) (Left "wrong alignment history domain")
  plans <- traverse (decodeControl >=> planAnnounce) rawPlans
  frontier <- traverse decodeFrontier rawFrontier
  readiness <- traverse (decodeEvidence >=> ready) rawReadiness
  certificates <- traverse (decodeControl >=> certificate) rawCertificates
  coverage <- traverse decodeCoverage rawCoverage
  acceptances <- traverse (decodeEvidence >=> acceptance) rawAcceptances
  let history = AlignmentHistory plans frontier readiness certificates coverage acceptances
      planIds = map Protocol.alignmentPlanAnnounceId plans
      planMap = Map.fromList (zip planIds plans)
      createdCuts = concatMap Protocol.alignmentPlanAnnounceCreatedCuts plans
      generationIds = map Protocol.alignmentCutAnnounceGeneration createdCuts
      generations = Map.fromList [(Protocol.alignmentCutAnnounceGeneration announce, Protocol.alignmentCutAnnounceCut announce) | announce <- createdCuts]
      sortedUnique values = values == Set.toAscList (Set.fromList values)
  unless (sortedUnique planIds && sortedUnique (map fst frontier)) (Left "noncanonical alignment plan catalogue")
  unless (length generationIds == Set.size (Set.fromList generationIds)) (Left "generation birth appears in multiple plans")
  mapM_ (validatePlanClosure planMap generations) plans
  validateAcyclic planMap Set.empty
  mapM_ (\(occurrence, identifier) -> unless (Plan.alignmentPlanIdSort identifier == occurrence && Map.member identifier planMap) (Left "alignment frontier lacks an exact retained plan")) frontier
  unless (sortedUnique (map readyKey readiness) && sortedUnique (map certificateKey certificates)) (Left "noncanonical alignment history evidence")
  let memberPresent (identifier, store) = maybe False (any ((== store) . alignmentMemberStoreIncarnation) . NE.toList . alignmentCutExactMembers) (Map.lookup identifier generations)
  unless (all (memberPresent . readyKey) readiness && all (memberPresent . certificateKey) certificates) (Left "historical evidence names an absent generation member")
  let readyKeys = Set.fromList (map readyKey readiness)
      certified = Set.fromList (map (fst . certificateKey) certificates)
      predecessors = concatMap alignmentCutPredecessorGenerationIds (Map.elems generations)
      completeReadiness certificateValue = case Map.lookup (Protocol.historicalCertificateClassGeneration certificateValue) generations of
        Nothing -> False
        Just cut -> all (\member -> Set.member (Protocol.historicalCertificateClassGeneration certificateValue, alignmentMemberStoreIncarnation member) readyKeys) (NE.toList (alignmentCutExactMembers cut))
  unless (all (\identifier -> Map.member identifier generations && Set.member identifier certified) predecessors) (Left "historical predecessor closure is incomplete")
  unless (all completeReadiness certificates) (Left "historical certificate lacks its complete readiness set")
  unless (sortedUnique (map fst coverage)) (Left "noncanonical historical cause coverage")
  mapM_
    ( \(key, identifiers) -> case Map.lookup (coveragePlanId key) planMap of
        Just plan | sortedUnique identifiers && identifiers == sort (map Protocol.alignmentPlanBindingClaimGeneration (Protocol.alignmentPlanAnnounceBindings plan)) -> Right ()
        _ -> Left "historical cause coverage lacks its complete explicit plan"
    )
    coverage
  unless (sortedUnique (map acceptanceKey acceptances) && all ((`Map.member` planMap) . Protocol.alignmentPlanAcceptedId) acceptances) (Left "historical acceptance lacks an exact retained plan")
  unless (encodeAlignmentHistory history == bytes) (Left "noncanonical alignment history bytes")
  pure history
  where
    decodeFrontier (sortBytes, occurrenceBytes, identifierBytes) = (,) <$> (sortOccurrence <$> shape (mkSortId sortBytes) <*> shape (mkSortDefinitionOccurrenceId occurrenceBytes)) <*> (decodeDto identifierBytes >>= shape . Bridge.alignmentPlanIdFromDto)
    planAnnounce (Protocol.AlignmentPlanAnnounced value) = Right value
    planAnnounce _ = Left "expected historical plan"
    ready (Protocol.AlignmentMemberReadyAdvertised value) = Right value
    ready _ = Left "expected historical readiness"
    certificate (Protocol.AlignmentHistoricalCertificateAdvertised value) = Right value
    certificate _ = Left "expected historical certificate"
    acceptance (Protocol.AlignmentPlanAcceptanceAdvertised value) = Right value
    acceptance _ = Left "expected historical plan acceptance"
    readyKey value = (Protocol.classMemberReadyGeneration value, Protocol.classMemberReadyStoreIncarnation value)
    certificateKey value = (Protocol.historicalCertificateClassGeneration value, Protocol.historicalCertificateSourceStoreIncarnation value)

validatePlanClosure :: Map.Map Plan.AlignmentPlanId Protocol.AlignmentPlanAnnounce -> Map.Map ContextClassGenerationId AlignmentCut -> Protocol.AlignmentPlanAnnounce -> Either String ()
validatePlanClosure plans generations plan = do
  prior <- traverse (\identifier -> maybe (Left "historical predecessor plan is absent") Right (Map.lookup identifier plans)) (Protocol.alignmentPlanAnnouncePredecessor plan)
  let priorBindings = maybe Map.empty (Map.fromList . map (\binding -> (Protocol.alignmentPlanBindingClaimMembers binding, Protocol.alignmentPlanBindingClaimGeneration binding)) . Protocol.alignmentPlanAnnounceBindings) prior
  mapM_
    ( \binding -> do
        let identifier = Protocol.alignmentPlanBindingClaimGeneration binding
        cut <- maybe (Left "historical binding lacks its generation birth") Right (Map.lookup identifier generations)
        unless (fmap alignmentMemberDelta (alignmentCutExactMembers cut) == Protocol.alignmentPlanBindingClaimMembers binding) (Left "historical binding names different class members")
        case Protocol.alignmentPlanBindingClaimDisposition binding of
          Protocol.AlignmentCreated -> Right ()
          Protocol.AlignmentCarried -> unless (Map.lookup (Protocol.alignmentPlanBindingClaimMembers binding) priorBindings == Just identifier) (Left "historical carry lacks its exact predecessor binding")
    )
    (Protocol.alignmentPlanAnnounceBindings plan)

validateAcyclic :: Map.Map Plan.AlignmentPlanId Protocol.AlignmentPlanAnnounce -> Set.Set Plan.AlignmentPlanId -> Either String ()
validateAcyclic remaining admitted
  | Map.null remaining = Right ()
  | Map.null ready = Left "cyclic historical predecessor plans"
  | otherwise = validateAcyclic (remaining `Map.difference` ready) (Set.union admitted (Map.keysSet ready))
  where
    ready = Map.filter (maybe True (`Set.member` admitted) . Protocol.alignmentPlanAnnouncePredecessor) remaining

coveragePlanId :: Alignment.AlignmentPromotionKey -> Plan.AlignmentPlanId
coveragePlanId = Alignment.alignmentPromotionKeyPlanId

encodeCoverage :: (Alignment.AlignmentPromotionKey, [ContextClassGenerationId]) -> RawCoverage
encodeCoverage (key, identifiers) =
  RawCoverage
    (encodeChecked Bridge.structuralConsequenceCauseToDto (Alignment.alignmentPromotionKeyCause key))
    (encodeChecked Bridge.alignmentPlanIdToDto (coveragePlanId key))
    (map contextClassGenerationIdBytes identifiers)

decodeCoverage :: RawCoverage -> Either String (Alignment.AlignmentPromotionKey, [ContextClassGenerationId])
decodeCoverage (RawCoverage rawCause rawPlan rawIdentifiers) = do
  cause <- decodeDto rawCause >>= shape . Bridge.structuralConsequenceCauseFromDto
  identifier <- decodeDto rawPlan >>= shape . Bridge.alignmentPlanIdFromDto
  identifiers <- traverse (shape . mkContextClassGenerationId) rawIdentifiers
  pure (Alignment.alignmentPromotionKeyForPlan cause identifier, identifiers)

encodeChecked :: (Show problem, Binary.Binary wire) => (semantic -> Either problem wire) -> semantic -> ByteString
encodeChecked encode value = case encode value of
  Left problem -> error ("checked alignment history encoding invariant: " <> show problem)
  Right dto -> BL.toStrict (Binary.encode dto)

decodeDto :: (Binary.Binary value) => ByteString -> Either String value
decodeDto bytes = case Binary.decodeOrFail (BL.fromStrict bytes) of
  Left (_, _, problem) -> Left problem
  Right (rest, _, value) | BL.null rest -> Right value
  _ -> Left "trailing historical evidence bytes"

encodeControl :: Protocol.AlignmentControl -> ByteString
encodeControl control = case Bridge.alignmentControlToDto control of
  Left problem -> error ("checked alignment history encoding invariant: " <> show problem)
  Right dto -> BL.toStrict (Binary.encode dto)

-- Historical facts are semantic payloads, independent of a live peer's
-- delivery sequence and cumulative receipt. Use their narrow checked DTO
-- directly rather than introducing transport bookkeeping into the catalogue.
encodeEvidence :: Protocol.AlignmentControl -> ByteString
encodeEvidence = encodeChecked Bridge.alignmentRetainedEvidenceToDto

decodeEvidence :: ByteString -> Either String Protocol.AlignmentControl
decodeEvidence bytes = do
  dto <- decodeDto bytes
  evidence <- shape (Bridge.alignmentRetainedEvidenceFromDto dto)
  unless (encodeEvidence evidence == bytes) (Left "noncanonical alignment evidence bytes")
  pure evidence

decodeControl :: ByteString -> Either String Protocol.AlignmentControl
decodeControl bytes = do
  dto <- case Binary.decodeOrFail (BL.fromStrict bytes) of
    Left (_, _, problem) -> Left problem
    Right (rest, _, value) | BL.null rest -> Right (value :: Wire.AlignmentControlDto)
    _ -> Left "trailing alignment control bytes"
  control <- shape (Bridge.alignmentControlFromDto dto)
  unless (encodeControl control == bytes) (Left "noncanonical alignment control bytes")
  pure control

shape :: (Show problem) => Either problem value -> Either String value
shape = either (Left . show) Right
