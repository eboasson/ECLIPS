{-# LANGUAGE ImportQualifiedPost #-}

module Eclips.Oracle.Internal.DisappearanceCanonical
  ( encodeCommand,
    decodeCommand,
    encodeAccepted,
    decodeAccepted,
    encodeEvent,
    decodeEvent,
    encodeRejection,
    decodeRejection,
  ) where

import Control.Monad (replicateM, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Serialize.Get qualified as G
import Data.Serialize.Put qualified as P
import Eclips.Domain.Disappearance
import Eclips.Domain.Identity
import Eclips.Domain.Label (LabelRevision, labelRevisionControlIndex, mkLabelRevision)
import Eclips.Domain.MemberSet
import Eclips.Domain.Membership
import Eclips.Oracle.Internal.Disappearance

putCommand :: DisappearanceCommand -> P.Put
putCommand value = case value of
  DisappearanceOpen x0 x1 -> do
    P.putWord8 0
    putSubject x0
    putCoordinate x1
  DisappearanceReport x0 x1 -> do
    P.putWord8 1
    putProbe x0
    putClaim x1
  DisappearanceInvalidate x0 x1 x2 x3 -> do
    P.putWord8 2
    putProbe x0
    putHerald x1
    putInvalidationReason x2
    putEvidenceDigest x3
  DisappearanceResolve x0 x1 -> do
    P.putWord8 3
    putProbe x0
    putCompleteDigest x1
  DisappearanceAbort x0 x1 -> do
    P.putWord8 4
    putProbe x0
    putAbortReason x1

getCommand :: G.Get DisappearanceCommand
getCommand = do
  tag <- G.getWord8
  case tag of
    0 -> DisappearanceOpen <$> getSubject <*> getCoordinate
    1 -> DisappearanceReport <$> getProbe <*> getClaim
    2 -> DisappearanceInvalidate <$> getProbe <*> getHerald <*> getInvalidationReason <*> getEvidenceDigest
    3 -> DisappearanceResolve <$> getProbe <*> getCompleteDigest
    4 -> DisappearanceAbort <$> getProbe <*> getAbortReason
    _ -> fail "unknown disappearance command tag"

encodeCommand :: DisappearanceCommand -> ByteString
encodeCommand = P.runPut . putCommand
decodeCommand :: ByteString -> Either String DisappearanceCommand
decodeCommand = exact encodeCommand getCommand

putAccepted :: DisappearanceCommandResult -> P.Put
putAccepted value = case value of
  DisappearanceOpenAccepted x0 -> do
    P.putWord8 0
    putOpenResult x0
  DisappearanceReportAccepted x0 x1 -> do
    P.putWord8 1
    putProbe x0
    putHerald x1
  DisappearanceInvalidateAccepted x0 -> do
    P.putWord8 2
    putProbe x0
  DisappearanceResolveAccepted x0 -> do
    P.putWord8 3
    putOutcome x0
  DisappearanceAbortAccepted x0 -> do
    P.putWord8 4
    putProbe x0

getAccepted :: G.Get DisappearanceCommandResult
getAccepted = do
  tag <- G.getWord8
  case tag of
    0 -> DisappearanceOpenAccepted <$> getOpenResult
    1 -> DisappearanceReportAccepted <$> getProbe <*> getHerald
    2 -> DisappearanceInvalidateAccepted <$> getProbe
    3 -> DisappearanceResolveAccepted <$> getOutcome
    4 -> DisappearanceAbortAccepted <$> getProbe
    _ -> fail "unknown disappearance accepted tag"

encodeAccepted :: DisappearanceCommandResult -> ByteString
encodeAccepted = P.runPut . putAccepted
decodeAccepted :: ByteString -> Either String DisappearanceCommandResult
decodeAccepted = exact encodeAccepted getAccepted

putInvalidation :: DisappearanceProbeInvalidationView -> P.Put
putInvalidation value = case value of
  CommandDisappearanceInvalidation x0 x1 x2 -> do
    P.putWord8 0
    putHerald x0
    putInvalidationReason x1
    putEvidenceDigest x2
  LabelOpenDisappearanceInvalidation x0 -> do
    P.putWord8 1
    putDecision x0

getInvalidation :: G.Get DisappearanceProbeInvalidationView
getInvalidation = do
  tag <- G.getWord8
  case tag of
    0 -> CommandDisappearanceInvalidation <$> getHerald <*> getInvalidationReason <*> getEvidenceDigest
    1 -> LabelOpenDisappearanceInvalidation <$> getDecision
    _ -> fail "unknown disappearance invalidation tag"

putAbortView :: DisappearanceProbeAbortReasonView -> P.Put
putAbortView value = case value of
  ExplicitDisappearanceAbortReasonView x0 -> do
    P.putWord8 0
    putAbortReason x0
  MembershipSupersededDisappearanceAbortView x0 -> do
    P.putWord8 1
    putGeneration x0
  AdmissionPreparingDisappearanceAbortView admission -> do
    P.putWord8 2
    putAdmission admission

getAbortView :: G.Get DisappearanceProbeAbortReasonView
getAbortView = do
  tag <- G.getWord8
  case tag of
    0 -> ExplicitDisappearanceAbortReasonView <$> getAbortReason
    1 -> MembershipSupersededDisappearanceAbortView <$> getGeneration
    2 -> AdmissionPreparingDisappearanceAbortView <$> getAdmission
    _ -> fail "unknown disappearance abortview tag"

putPhase :: DisappearanceProbePhaseView -> P.Put
putPhase value = case value of
  DisappearanceCollectingView -> do
    P.putWord8 0
  DisappearanceInvalidatedView x0 x1 -> do
    P.putWord8 1
    putInvalidation x0
    putIndex x1
  DisappearanceResolvedView x0 x1 -> do
    P.putWord8 2
    putOutcome x0
    putIndex x1
  DisappearanceAbortedView x0 x1 -> do
    P.putWord8 3
    putAbortReason x0
    putIndex x1
  DisappearanceMembershipSupersededView x0 x1 -> do
    P.putWord8 4
    putGeneration x0
    putIndex x1
  DisappearanceAdmissionPreparingView admission index -> do
    P.putWord8 5
    putAdmission admission
    putIndex index

getPhase :: G.Get DisappearanceProbePhaseView
getPhase = do
  tag <- G.getWord8
  case tag of
    0 -> pure DisappearanceCollectingView
    1 -> DisappearanceInvalidatedView <$> getInvalidation <*> getPositiveIndex
    2 -> do
      outcome <- getOutcome
      index <- getPositiveIndex
      unless (disappearanceResolutionControlIndex outcome == index) (fail "disappearance terminal phase Resolve index mismatch")
      pure (DisappearanceResolvedView outcome index)
    3 -> DisappearanceAbortedView <$> getAbortReason <*> getPositiveIndex
    4 -> DisappearanceMembershipSupersededView <$> getGeneration <*> getPositiveIndex
    5 -> do
      admission <- getAdmission
      index <- getPositiveIndex
      unless (heraldAdmissionControlIndex admission == index) (fail "disappearance terminal phase admission index mismatch")
      pure (DisappearanceAdmissionPreparingView admission index)
    _ -> fail "unknown disappearance phase tag"

putEvent :: DisappearanceProjectionEvent -> P.Put
putEvent value = case value of
  DisappearanceOpenProjected x0 x1 -> do
    P.putWord8 0
    putHeader x0
    putOpenResult x1
  DisappearanceReportProjected x0 -> do
    P.putWord8 1
    putClaim x0
  DisappearanceInvalidatedProjected x0 x1 -> do
    P.putWord8 2
    putProbe x0
    putInvalidation x1
  DisappearanceResolvedProjected x0 x1 -> do
    P.putWord8 3
    putProbe x0
    putOutcome x1
  DisappearanceAbortedProjected x0 x1 -> do
    P.putWord8 4
    putProbe x0
    putAbortView x1

getEvent :: G.Get DisappearanceProjectionEvent
getEvent = do
  tag <- G.getWord8
  case tag of
    0 -> DisappearanceOpenProjected <$> getHeader <*> getOpenResult
    1 -> DisappearanceReportProjected <$> getClaim
    2 -> DisappearanceInvalidatedProjected <$> getProbe <*> getInvalidation
    3 -> DisappearanceResolvedProjected <$> getProbe <*> getOutcome
    4 -> DisappearanceAbortedProjected <$> getProbe <*> getAbortView
    _ -> fail "unknown disappearance event tag"

encodeEvent :: DisappearanceProjectionEvent -> ByteString
encodeEvent = P.runPut . putEvent
decodeEvent :: ByteString -> Either String DisappearanceProjectionEvent
decodeEvent bytes = do
  event <- exact encodeEvent getEvent bytes
  case event of
    DisappearanceOpenProjected header result ->
      let resultProbe = case disappearanceOpenResultView result of
            OpenedDisappearanceProbe probe -> probe
            AliasedDisappearanceProbe probe -> probe
       in if projectedDisappearanceProbeHeaderId header == resultProbe
            then Right event
            else Left "disappearance Open header/result probe mismatch"
    DisappearanceResolvedProjected probe outcome
      | disappearanceProbeOpenControlIndex probe >= disappearanceResolutionControlIndex outcome ->
          Left "disappearance Resolve must follow its Open"
    DisappearanceAbortedProjected probe (AdmissionPreparingDisappearanceAbortView admission)
      | disappearanceProbeOpenControlIndex probe >= heraldAdmissionControlIndex admission ->
          Left "disappearance admission preparation must follow its Open"
    _ -> Right event

putRejection :: DisappearanceRejection -> P.Put
putRejection value = case value of
  DisappearanceAdmissionPreparing admission -> do
    P.putWord8 14
    putAdmission admission
  DisappearanceSubjectMembershipCoordinateMismatch x0 x1 -> do
    P.putWord8 0
    putCoordinate x0
    putCoordinate x1
  DisappearanceControlledRevisionMismatch x0 x1 x2 -> do
    P.putWord8 1
    putObject x0
    putMaybeRevision x1
    putMaybeRevision x2
  DisappearanceControlledSubjectDeleted x0 -> do
    P.putWord8 2
    putObject x0
  DisappearanceLabelWorkflowInProgress x0 x1 -> do
    P.putWord8 3
    putObject x0
    putDecision x1
  DisappearanceStaleRegularOccurrence x0 x1 x2 -> do
    P.putWord8 4
    putSort x0
    putOccurrence x1
    putOccurrence x2
  DisappearanceUnknownProbe x0 -> do
    P.putWord8 5
    putProbe x0
  DisappearanceProbeAlreadyTerminal x0 x1 -> do
    P.putWord8 6
    putProbe x0
    putPhase x1
  DisappearanceReporterHomeMismatch x0 x1 -> do
    P.putWord8 7
    putHerald x0
    putHerald x1
  DisappearanceReporterNotCaptured x0 x1 -> do
    P.putWord8 8
    putProbe x0
    putHerald x1
  DisappearanceEvidenceProbeMismatch x0 x1 -> do
    P.putWord8 9
    putProbe x0
    putProbe x1
  DisappearanceEvidenceCoordinateMismatch x0 x1 -> do
    P.putWord8 10
    putCoordinate x0
    putCoordinate x1
  DisappearanceConflictingReport x0 x1 x2 x3 -> do
    P.putWord8 11
    putProbe x0
    putHerald x1
    putClaim x2
    putClaim x3
  DisappearanceIncompleteEvidence x0 x1 -> do
    P.putWord8 12
    putProbe x0
    putHeralds x1
  DisappearanceCompleteEvidenceMismatch x0 x1 x2 -> do
    P.putWord8 13
    putProbe x0
    putCompleteDigest x1
    putCompleteDigest x2

getRejection :: G.Get DisappearanceRejection
getRejection = do
  tag <- G.getWord8
  case tag of
    0 -> DisappearanceSubjectMembershipCoordinateMismatch <$> getCoordinate <*> getCoordinate
    1 -> DisappearanceControlledRevisionMismatch <$> getObject <*> getMaybeRevision <*> getMaybeRevision
    2 -> DisappearanceControlledSubjectDeleted <$> getObject
    3 -> DisappearanceLabelWorkflowInProgress <$> getObject <*> getDecision
    4 -> DisappearanceStaleRegularOccurrence <$> getSort <*> getOccurrence <*> getOccurrence
    5 -> DisappearanceUnknownProbe <$> getProbe
    6 -> DisappearanceProbeAlreadyTerminal <$> getProbe <*> getPhase
    7 -> DisappearanceReporterHomeMismatch <$> getHerald <*> getHerald
    8 -> DisappearanceReporterNotCaptured <$> getProbe <*> getHerald
    9 -> DisappearanceEvidenceProbeMismatch <$> getProbe <*> getProbe
    10 -> DisappearanceEvidenceCoordinateMismatch <$> getCoordinate <*> getCoordinate
    11 -> DisappearanceConflictingReport <$> getProbe <*> getHerald <*> getClaim <*> getClaim
    12 -> DisappearanceIncompleteEvidence <$> getProbe <*> getHeralds
    13 -> DisappearanceCompleteEvidenceMismatch <$> getProbe <*> getCompleteDigest <*> getCompleteDigest
    14 -> DisappearanceAdmissionPreparing <$> getAdmission
    _ -> fail "unknown disappearance rejection tag"

encodeRejection :: DisappearanceRejection -> ByteString
encodeRejection = P.runPut . putRejection
decodeRejection :: ByteString -> Either String DisappearanceRejection
decodeRejection bytes = do
  rejection <- exact encodeRejection getRejection bytes
  case rejection of
    DisappearanceProbeAlreadyTerminal probe phase -> do
      terminalIndex <- case phase of
        DisappearanceCollectingView -> Left "already-terminal rejection carries collecting phase"
        DisappearanceInvalidatedView _ index -> Right index
        DisappearanceResolvedView _ index -> Right index
        DisappearanceAbortedView _ index -> Right index
        DisappearanceMembershipSupersededView _ index -> Right index
        DisappearanceAdmissionPreparingView _ index -> Right index
      if terminalIndex > disappearanceProbeOpenControlIndex probe
        then Right rejection
        else Left "disappearance terminal phase must follow Open"
    _ -> Right rejection

exact :: (a -> ByteString) -> G.Get a -> ByteString -> Either String a
exact encode parse bytes = do
  value <- G.runGet (parse <* (G.isEmpty >>= \done -> unless done (fail "trailing disappearance bytes"))) bytes
  if encode value == bytes then Right value else Left "noncanonical disappearance payload"
admit :: (Show problem) => Either problem a -> G.Get a
admit = either (fail . show) pure
putFrame :: ByteString -> P.Put
putFrame bytes = P.putWord64be (fromIntegral (ByteString.length bytes)) >> P.putByteString bytes
getFrame :: G.Get ByteString
getFrame = do
  count <- G.getWord64be
  remaining <- G.remaining
  unless (count <= fromIntegral remaining) (fail "truncated disappearance frame")
  G.getBytes (fromIntegral count)
putIndex :: ControlIndex -> P.Put
putIndex = P.putWord64be . controlIndexWord64
getIndex :: G.Get ControlIndex
getIndex = controlIndex <$> G.getWord64be
getPositiveIndex :: G.Get ControlIndex
getPositiveIndex = do
  index <- getIndex
  unless (controlIndexWord64 index > 0) (fail "disappearance index must be positive")
  pure index
putProbe :: DisappearanceProbeId -> P.Put
putProbe probe = putIndex (disappearanceProbeOpenControlIndex probe) >> P.putByteString (disappearanceProbeIdBytes probe)
getProbe :: G.Get DisappearanceProbeId
getProbe = do
  index <- getPositiveIndex
  G.getBytes 32 >>= admit . admitDisappearanceProbeId index
putSubject :: DisappearanceSubject -> P.Put
putSubject = putFrame . disappearanceSubjectCanonicalBytes
getSubject :: G.Get DisappearanceSubject
getSubject = getFrame >>= either fail pure . decodeDisappearanceSubjectCanonicalBytes
putCoordinate :: DisappearanceSubjectMembershipCoordinate -> P.Put
putCoordinate coordinate = do
  P.putByteString (disappearanceSubjectDigestBytes (disappearanceCoordinateSubjectDigest coordinate))
  putGeneration (disappearanceCoordinateMembershipGenerationId coordinate)
  P.putByteString (memberSetDigestBytes (disappearanceCoordinateMemberSetDigest coordinate))
getCoordinate :: G.Get DisappearanceSubjectMembershipCoordinate
getCoordinate =
  admitDisappearanceSubjectMembershipCoordinate
    <$> (G.getBytes 32 >>= admit . mkDisappearanceSubjectDigest)
    <*> getGeneration
    <*> (G.getBytes 32 >>= admit . mkMemberSetDigest)
putClaim :: DisappearanceEvidenceClaim -> P.Put
putClaim claim = putIndex (disappearanceProbeOpenControlIndex (disappearanceEvidenceClaimProbeId claim)) >> putFrame (disappearanceEvidenceClaimCanonicalBytes claim)
getClaim :: G.Get DisappearanceEvidenceClaim
getClaim = do
  index <- getPositiveIndex
  getFrame >>= either fail pure . decodeDisappearanceEvidenceClaimCanonicalBytes index
putOutcome :: DisappearanceResolutionOutcome -> P.Put
putOutcome = putFrame . disappearanceResolutionOutcomeCanonicalBytes
getOutcome :: G.Get DisappearanceResolutionOutcome
getOutcome = getFrame >>= either fail pure . decodeDisappearanceResolutionOutcomeCanonicalBytes
putHeader :: ProjectedDisappearanceProbeHeader -> P.Put
putHeader header = do
  putProbe (projectedDisappearanceProbeHeaderId header)
  putSubject (projectedDisappearanceProbeHeaderSubject header)
  putCoordinate (projectedDisappearanceProbeHeaderCoordinate header)
  putFrame (heraldMembershipGenerationCanonicalBytes (projectedDisappearanceProbeHeaderMembership header))
getHeader :: G.Get ProjectedDisappearanceProbeHeader
getHeader = do
  probe <- getProbe
  subject <- getSubject
  coordinate <- getCoordinate
  membership <- getFrame >>= admit . decodeHeraldMembershipGenerationCanonicalBytes
  unless (coordinate == disappearanceSubjectMembershipCoordinate subject membership) (fail "disappearance header coordinate mismatch")
  pure (ProjectedDisappearanceProbeHeader probe subject coordinate membership)
putOpenResult :: DisappearanceOpenResult -> P.Put
putOpenResult result = case disappearanceOpenResultView result of
  OpenedDisappearanceProbe probe -> P.putWord8 0 >> putProbe probe
  AliasedDisappearanceProbe probe -> P.putWord8 1 >> putProbe probe
getOpenResult :: G.Get DisappearanceOpenResult
getOpenResult = do
  tag <- G.getWord8
  case tag of
    0 -> openedDisappearanceProbe <$> getProbe
    1 -> aliasedDisappearanceProbe <$> getProbe
    _ -> fail "unknown disappearance Open result tag"
putInvalidationReason :: DisappearanceInvalidationReason -> P.Put
putInvalidationReason = P.putWord8 . fromIntegral . fromEnum
getInvalidationReason :: G.Get DisappearanceInvalidationReason
getInvalidationReason = do
  tag <- G.getWord8
  case tag of
    0 -> pure MatchingPublicationObserved
    1 -> pure EvidenceContradicted
    _ -> fail "unknown disappearance invalidation reason"
putAbortReason :: DisappearanceAbortReason -> P.Put
putAbortReason AuthorizedDisappearanceAbort = P.putWord8 0
getAbortReason :: G.Get DisappearanceAbortReason
getAbortReason = G.getWord8 >>= \tag -> if tag == 0 then pure authorizedDisappearanceAbortReason else fail "unknown authorized Abort reason"
putCompleteDigest :: CompleteDisappearanceEvidenceDigest -> P.Put
putCompleteDigest = P.putByteString . completeDisappearanceEvidenceDigestBytes
getCompleteDigest :: G.Get CompleteDisappearanceEvidenceDigest
getCompleteDigest = CompleteDisappearanceEvidenceDigest <$> G.getBytes 32
putEvidenceDigest :: DisappearanceEvidenceDigest -> P.Put
putEvidenceDigest = P.putByteString . disappearanceEvidenceDigestBytes
getEvidenceDigest :: G.Get DisappearanceEvidenceDigest
getEvidenceDigest = G.getBytes 32 >>= admit . mkDisappearanceEvidenceDigest
putMaybeRevision :: Maybe LabelRevision -> P.Put
putMaybeRevision = putMaybe (putIndex . labelRevisionControlIndex)
getMaybeRevision :: G.Get (Maybe LabelRevision)
getMaybeRevision = getMaybe (getPositiveIndex >>= admit . mkLabelRevision)
putMaybe :: (a -> P.Put) -> Maybe a -> P.Put
putMaybe _ Nothing = P.putWord8 0
putMaybe put (Just value) = P.putWord8 1 >> put value
getMaybe :: G.Get a -> G.Get (Maybe a)
getMaybe get =
  G.getWord8 >>= \tag -> case tag of
    0 -> pure Nothing
    1 -> Just <$> get
    _ -> fail "unknown optional disappearance field tag"
putHeralds :: [HeraldEpoch] -> P.Put
putHeralds values = P.putWord64be (fromIntegral (length values)) >> mapM_ putHerald values
getHeralds :: G.Get [HeraldEpoch]
getHeralds = do
  count <- G.getWord64be
  remaining <- G.remaining
  unless (count <= fromIntegral remaining `div` 32) (fail "truncated disappearance reporter list")
  replicateM (fromIntegral count) getHerald

putHerald :: HeraldEpoch -> P.Put
putHerald = P.putByteString . heraldEpochBytes
getHerald :: G.Get HeraldEpoch
getHerald = G.getBytes 32 >>= admit . mkHeraldEpoch

putObject :: GlobalObjectId -> P.Put
putObject = P.putByteString . globalObjectIdBytes
getObject :: G.Get GlobalObjectId
getObject = G.getBytes 32 >>= admit . mkGlobalObjectId

putDecision :: LabelDecisionId -> P.Put
putDecision = P.putByteString . labelDecisionIdBytes
getDecision :: G.Get LabelDecisionId
getDecision = G.getBytes 32 >>= admit . mkLabelDecisionId

putGeneration :: HeraldMembershipGenerationId -> P.Put
putGeneration = P.putByteString . heraldMembershipGenerationIdBytes
getGeneration :: G.Get HeraldMembershipGenerationId
getGeneration = G.getBytes 32 >>= admit . mkHeraldMembershipGenerationId

putAdmission :: HeraldAdmissionId -> P.Put
putAdmission = putFrame . heraldAdmissionIdCanonicalBytes
getAdmission :: G.Get HeraldAdmissionId
getAdmission = getFrame >>= admit . decodeHeraldAdmissionIdCanonicalBytes . ByteString.copy

putSort :: SortId -> P.Put
putSort = P.putByteString . sortIdBytes
getSort :: G.Get SortId
getSort = G.getBytes 32 >>= admit . mkSortId

putOccurrence :: SortDefinitionOccurrenceId -> P.Put
putOccurrence = P.putByteString . sortDefinitionOccurrenceIdBytes
getOccurrence :: G.Get SortDefinitionOccurrenceId
getOccurrence = G.getBytes 32 >>= admit . mkSortDefinitionOccurrenceId
