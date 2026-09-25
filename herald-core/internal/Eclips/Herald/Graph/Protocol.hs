{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure semantic messages for structural progress and topology-cut repair.
--
-- These values are independent of peer-stream publication assignment.  Their
-- constructors normalize intrinsic evidence; the Graph owner additionally
-- checks fixed membership, binding identity, monotonicity, causal coverage,
-- and predecessor-chain state when admitting them.
module Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    structuralAppliedReport,
    structuralAppliedReportReporter,
    structuralAppliedReportMembershipGenerationId,
    structuralAppliedReportMemberSetDigest,
    structuralAppliedReportVersionVector,
    structuralAppliedReportControlPrefix,
    TopologyCutAnnounce,
    TopologyCutAnnounceProblem (..),
    topologyCutAnnounce,
    checkTopologyCutAnnounce,
    topologyCutAnnounceAnnouncer,
    topologyCutAnnounceId,
    topologyCutAnnounceCut,
    TopologyCutAcceptance,
    topologyCutAcceptance,
    topologyCutAcceptanceId,
    topologyCutAcceptanceReport,
    topologyCutAcceptanceReporter,
    TopologyCutEstablished,
    TopologyCutEstablishedProblem (..),
    topologyCutEstablished,
    topologyCutEstablishedId,
    topologyCutEstablishedCut,
    topologyCutEstablishedAcceptances,
    topologyCutEstablishedCanonicalBytes,
    TopologyCutEstablishedCanonicalProblem (..),
    decodeTopologyCutEstablishedCanonicalBytes,
    TopologyCutEstablishedAck,
    topologyCutEstablishedAck,
    topologyCutEstablishedAckId,
    topologyCutEstablishedAckReporter,
    topologyCutEstablishedAckMembershipGenerationId,
  )
where

import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize qualified as Serialize
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    TopologyCutId,
    controlIndex,
    controlIndexWord64,
    heraldEpochBytes,
    mkHeraldEpoch,
    mkStructuralSequence,
    mkTopologyCutId,
    structuralSequenceWord64,
    topologyCutIdBytes,
  )
import Eclips.Domain.MemberSet (MemberSetDigest, memberSetDigestBytes, mkMemberSetDigest)
import Eclips.Domain.Membership (HeraldMembershipGenerationId, heraldMembershipGenerationIdBytes, mkHeraldMembershipGenerationId)
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    emptyStructuralPrefix,
    structuralPrefixSequence,
    structuralPrefixThrough,
    structuralVersionVectorEntries,
    structuralVersionVectorFromClaimedCoordinates,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.Topology
  ( TopologyCut,
    decodeTopologyCutCanonicalBytes,
    deriveTopologyCutId,
    topologyCutCanonicalBytes,
  )

data StructuralAppliedReport = StructuralAppliedReport
  { reporter :: HeraldEpoch,
    versionVector :: StructuralVersionVector,
    controlPrefix :: ControlIndex
  }
  deriving stock (Eq, Show)

structuralAppliedReport ::
  HeraldEpoch ->
  StructuralVersionVector ->
  ControlIndex ->
  StructuralAppliedReport
structuralAppliedReport = StructuralAppliedReport

structuralAppliedReportReporter :: StructuralAppliedReport -> HeraldEpoch
structuralAppliedReportReporter report = report.reporter

structuralAppliedReportMembershipGenerationId ::
  StructuralAppliedReport -> HeraldMembershipGenerationId
structuralAppliedReportMembershipGenerationId =
  structuralVersionVectorMembershipGenerationId . (.versionVector)

structuralAppliedReportMemberSetDigest ::
  StructuralAppliedReport -> MemberSetDigest
structuralAppliedReportMemberSetDigest =
  structuralVersionVectorMemberSetDigest . (.versionVector)

structuralAppliedReportVersionVector ::
  StructuralAppliedReport -> StructuralVersionVector
structuralAppliedReportVersionVector report = report.versionVector

structuralAppliedReportControlPrefix :: StructuralAppliedReport -> ControlIndex
structuralAppliedReportControlPrefix report = report.controlPrefix

data TopologyCutAnnounce = TopologyCutAnnounce
  { announcer :: HeraldEpoch,
    cutId :: TopologyCutId,
    cut :: TopologyCut
  }
  deriving stock (Eq, Show)

data TopologyCutAnnounceProblem
  = TopologyCutAnnounceIdMismatch TopologyCutId TopologyCutId
  deriving stock (Eq, Show)

topologyCutAnnounce :: HeraldEpoch -> TopologyCut -> TopologyCutAnnounce
topologyCutAnnounce announcer cut =
  TopologyCutAnnounce announcer (deriveTopologyCutId cut) cut

checkTopologyCutAnnounce ::
  HeraldEpoch ->
  TopologyCutId ->
  TopologyCut ->
  Either TopologyCutAnnounceProblem TopologyCutAnnounce
checkTopologyCutAnnounce announcer claimed cut
  | claimed == derived = Right (TopologyCutAnnounce announcer claimed cut)
  | otherwise = Left (TopologyCutAnnounceIdMismatch derived claimed)
  where
    derived = deriveTopologyCutId cut

topologyCutAnnounceAnnouncer :: TopologyCutAnnounce -> HeraldEpoch
topologyCutAnnounceAnnouncer announce = announce.announcer

topologyCutAnnounceId :: TopologyCutAnnounce -> TopologyCutId
topologyCutAnnounceId announce = announce.cutId

topologyCutAnnounceCut :: TopologyCutAnnounce -> TopologyCut
topologyCutAnnounceCut announce = announce.cut

data TopologyCutAcceptance = TopologyCutAcceptance
  { cutId :: TopologyCutId,
    report :: StructuralAppliedReport
  }
  deriving stock (Eq, Show)

topologyCutAcceptance ::
  TopologyCutId -> StructuralAppliedReport -> TopologyCutAcceptance
topologyCutAcceptance = TopologyCutAcceptance

topologyCutAcceptanceId :: TopologyCutAcceptance -> TopologyCutId
topologyCutAcceptanceId acceptance = acceptance.cutId

topologyCutAcceptanceReport ::
  TopologyCutAcceptance -> StructuralAppliedReport
topologyCutAcceptanceReport acceptance = acceptance.report

topologyCutAcceptanceReporter :: TopologyCutAcceptance -> HeraldEpoch
topologyCutAcceptanceReporter =
  structuralAppliedReportReporter . topologyCutAcceptanceReport

data TopologyCutEstablished = TopologyCutEstablished
  { cutId :: TopologyCutId,
    cut :: TopologyCut,
    acceptances :: [TopologyCutAcceptance]
  }
  deriving stock (Eq, Show)

data TopologyCutEstablishedProblem
  = TopologyCutEstablishedIdMismatch TopologyCutId TopologyCutId
  | TopologyCutEstablishedAcceptanceCutMismatch
      HeraldEpoch
      TopologyCutId
      TopologyCutId
  | TopologyCutEstablishedDuplicateReporter HeraldEpoch
  deriving stock (Eq, Show)

-- | Check intrinsic cut identity and normalize the one-per-reporter evidence.
-- Exact membership and frontier coverage remain Graph-owner checks.
topologyCutEstablished ::
  TopologyCutId ->
  TopologyCut ->
  [TopologyCutAcceptance] ->
  Either TopologyCutEstablishedProblem TopologyCutEstablished
topologyCutEstablished claimed cut supplied = do
  let derived = deriveTopologyCutId cut
  if claimed == derived
    then Right ()
    else Left (TopologyCutEstablishedIdMismatch derived claimed)
  mapM_ requireCut supplied
  case firstDuplicate (fmap topologyCutAcceptanceReporter supplied) of
    Just duplicate ->
      Left (TopologyCutEstablishedDuplicateReporter duplicate)
    Nothing ->
      Right
        TopologyCutEstablished
          { cutId = claimed,
            cut,
            acceptances = sortOn topologyCutAcceptanceReporter supplied
          }
  where
    requireCut acceptance
      | topologyCutAcceptanceId acceptance == claimed = Right ()
      | otherwise =
          Left
            ( TopologyCutEstablishedAcceptanceCutMismatch
                (topologyCutAcceptanceReporter acceptance)
                claimed
                (topologyCutAcceptanceId acceptance)
            )

topologyCutEstablishedId :: TopologyCutEstablished -> TopologyCutId
topologyCutEstablishedId established = established.cutId

topologyCutEstablishedCut :: TopologyCutEstablished -> TopologyCut
topologyCutEstablishedCut established = established.cut

topologyCutEstablishedAcceptances ::
  TopologyCutEstablished -> [TopologyCutAcceptance]
topologyCutEstablishedAcceptances established = established.acceptances

-- | Canonical retained certificate material for membership-base inventories.
-- This preserves the complete reports, so a cut digest alone cannot stand in
-- for the established evidence required by the receiving Graph owner.
topologyCutEstablishedCanonicalBytes :: TopologyCutEstablished -> ByteString
topologyCutEstablishedCanonicalBytes established =
  Serialize.encode
    ( establishedCertificateDomain,
      topologyCutIdBytes established.cutId,
      topologyCutCanonicalBytes established.cut,
      fmap (reportTranscript . topologyCutAcceptanceReport) established.acceptances
    )

type VectorTranscript = (ByteString, ByteString, [(ByteString, Maybe Word64)])

type ReportTranscript = (ByteString, VectorTranscript, Word64)

type EstablishedTranscript = (ByteString, ByteString, ByteString, [ReportTranscript])

establishedCertificateDomain :: ByteString
establishedCertificateDomain = "eclips/topology-established"

reportTranscript :: StructuralAppliedReport -> ReportTranscript
reportTranscript report =
  ( heraldEpochBytes report.reporter,
    vectorTranscript report.versionVector,
    controlIndexWord64 report.controlPrefix
  )

vectorTranscript :: StructuralVersionVector -> VectorTranscript
vectorTranscript vector =
  ( heraldMembershipGenerationIdBytes (structuralVersionVectorMembershipGenerationId vector),
    memberSetDigestBytes (structuralVersionVectorMemberSetDigest vector),
    [ (heraldEpochBytes herald, structuralSequenceWord64 <$> structuralPrefixSequence prefix)
    | (herald, prefix) <- structuralVersionVectorEntries vector
    ]
  )

data TopologyCutEstablishedCanonicalProblem
  = TopologyCutEstablishedCanonicalMalformed
  | TopologyCutEstablishedCanonicalInvalidShape
  | TopologyCutEstablishedCanonicalNotCanonical
  deriving stock (Eq, Show)

-- | Decode intrinsic certificate shape. Authority, complete reporter membership,
-- causal coverage and ancestry remain checks of the single Graph owner.
decodeTopologyCutEstablishedCanonicalBytes ::
  ByteString -> Either TopologyCutEstablishedCanonicalProblem TopologyCutEstablished
decodeTopologyCutEstablishedCanonicalBytes bytes = do
  (domain, identifier, cutBytes, reports) <-
    either
      (const (Left TopologyCutEstablishedCanonicalMalformed))
      Right
      (Serialize.decode bytes :: Either String EstablishedTranscript)
  if domain == establishedCertificateDomain
    then Right ()
    else Left TopologyCutEstablishedCanonicalInvalidShape
  cutId <- certificateShape (mkTopologyCutId identifier)
  cut <- certificateShape (decodeTopologyCutCanonicalBytes cutBytes)
  acceptances <- traverse (fmap (topologyCutAcceptance cutId) . decodeReport) reports
  established <- certificateShape (topologyCutEstablished cutId cut acceptances)
  if topologyCutEstablishedCanonicalBytes established == bytes
    then Right established
    else Left TopologyCutEstablishedCanonicalNotCanonical
  where
    decodeReport (reporter, vector, control) =
      structuralAppliedReport
        <$> certificateShape (mkHeraldEpoch reporter)
        <*> decodeVector vector
        <*> pure (controlIndex control)

    decodeVector (generationBytes, digestBytes, supplied) = do
      generation <- certificateShape (mkHeraldMembershipGenerationId generationBytes)
      digest <- certificateShape (mkMemberSetDigest digestBytes)
      entries <- traverse decodeEntry supplied
      nonempty <- maybe (Left TopologyCutEstablishedCanonicalInvalidShape) Right (NonEmpty.nonEmpty entries)
      certificateShape (structuralVersionVectorFromClaimedCoordinates generation digest nonempty)

    decodeEntry (herald, sequenceNumber) =
      (,)
        <$> certificateShape (mkHeraldEpoch herald)
        <*> maybe (Right emptyStructuralPrefix) (fmap structuralPrefixThrough . certificateShape . mkStructuralSequence) sequenceNumber

certificateShape :: Either problem value -> Either TopologyCutEstablishedCanonicalProblem value
certificateShape = either (const (Left TopologyCutEstablishedCanonicalInvalidShape)) Right

data TopologyCutEstablishedAck = TopologyCutEstablishedAck
  { cutId :: TopologyCutId,
    reporter :: HeraldEpoch,
    membershipGenerationId :: HeraldMembershipGenerationId
  }
  deriving stock (Eq, Show)

topologyCutEstablishedAck ::
  TopologyCutId ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  TopologyCutEstablishedAck
topologyCutEstablishedAck = TopologyCutEstablishedAck

topologyCutEstablishedAckId :: TopologyCutEstablishedAck -> TopologyCutId
topologyCutEstablishedAckId acknowledgement = acknowledgement.cutId

topologyCutEstablishedAckReporter ::
  TopologyCutEstablishedAck -> HeraldEpoch
topologyCutEstablishedAckReporter acknowledgement = acknowledgement.reporter

topologyCutEstablishedAckMembershipGenerationId ::
  TopologyCutEstablishedAck -> HeraldMembershipGenerationId
topologyCutEstablishedAckMembershipGenerationId acknowledgement =
  acknowledgement.membershipGenerationId

firstDuplicate :: (Ord value) => [value] -> Maybe value
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (value : remaining)
      | Set.member value seen = Just value
      | otherwise = go (Set.insert value seen) remaining
