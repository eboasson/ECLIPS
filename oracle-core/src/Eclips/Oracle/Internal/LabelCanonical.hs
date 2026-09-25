{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Package-private parameterized canonical transcripts for label decisions.
--
-- Keeping the normalized facts parameterized lets the label owner supply the
-- concrete authority and disposition encodings while this module fixes their
-- shared transcript shape.
module Eclips.Oracle.Internal.LabelCanonical
  ( NormalizedPreparedFacts (..),
    canonicalPreparedFactsBytes,
    NormalizedTerminalOutcome (..),
    canonicalTerminalOutcomeBytes,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    LabelDecisionId,
    controlIndexWord64,
    globalObjectIdBytes,
    heraldEpochBytes,
    labelDecisionIdBytes,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
    topologyCutIdBytes,
  )
import Eclips.Domain.Label
  ( LabelRevision,
    PreparedLabelDigest,
    PriorAuthorityJustification,
    PriorAuthorityJustificationView (..),
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    labelRevisionControlIndex,
    preparedLabelDigestBytes,
    priorAuthorityJustificationView,
    releasedLabelStateView,
  )
import Eclips.Domain.Sort.Profile
  ( CatalogueDigest,
    catalogueDigestBytes,
  )
import Eclips.Domain.Startup (initialProjectionDigestBytes)
import Eclips.Domain.Topology
  ( MemberSetDigest,
    memberSetDigestBytes,
  )
import Eclips.Domain.Value
  ( Label,
    canonicalValueByteString,
    canonicalValueBytes,
    labelValue,
  )
import GHC.Generics (Generic)

-- | Complete normalized preparation facts, parameterized only by the authority
-- representation and its already-validated release disposition.
data NormalizedPreparedFacts authority disposition
  = NormalizedPreparedFacts
      LabelDecisionId
      ControlIndex
      GlobalObjectId
      ReleasedLabelState
      ReleasedLabelState
      (Maybe LabelRevision)
      CatalogueDigest
      (Maybe authority)
      (Maybe PriorAuthorityJustification)
      disposition
      MemberSetDigest
  deriving stock (Eq, Show)

-- | Encode the exact shared prepared-facts transcript. The caller owns the
-- concrete canonical encoding of authority and disposition arms.
canonicalPreparedFactsBytes ::
  (authority -> ByteString) ->
  (disposition -> (Word8, ByteString)) ->
  NormalizedPreparedFacts authority disposition ->
  ByteString
canonicalPreparedFactsBytes encodeAuthority encodeDisposition facts =
  Serialize.encode (preparedFactsTranscript encodeAuthority encodeDisposition facts)

data PreparedFactsTranscript
  = PreparedFactsTranscript
      ByteString
      ByteString
      Word64
      ByteString
      ReleasedStateTranscript
      ReleasedStateTranscript
      (Maybe Word64)
      ByteString
      (Maybe ByteString)
      (Maybe PriorAuthorityJustificationTranscript)
      PreparedAuthorityDispositionTranscript
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReleasedStateTranscript
  = ReleasedStateTranscript Word8 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PriorAuthorityJustificationTranscript
  = PriorAuthorityJustificationTranscript
      Word8
      Word64
      ByteString
      Word64
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PreparedAuthorityDispositionTranscript
  = PreparedAuthorityDispositionTranscript Word8 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

preparedFactsTranscript ::
  (authority -> ByteString) ->
  (disposition -> (Word8, ByteString)) ->
  NormalizedPreparedFacts authority disposition ->
  PreparedFactsTranscript
preparedFactsTranscript
  encodeAuthority
  encodeDisposition
  ( NormalizedPreparedFacts
      decision
      resolveIndex
      object
      proposed
      expectedPrior
      expectedRevision
      catalogue
      expectedAuthority
      justification
      disposition
      members
    ) =
    PreparedFactsTranscript
      "ECLIPS-PREPARED-LABEL-FACTS"
      (labelDecisionIdBytes decision)
      (controlIndexWord64 resolveIndex)
      (globalObjectIdBytes object)
      (releasedStateTranscript proposed)
      (releasedStateTranscript expectedPrior)
      (controlIndexWord64 . labelRevisionControlIndex <$> expectedRevision)
      (catalogueDigestBytes catalogue)
      (encodeAuthority <$> expectedAuthority)
      (justificationTranscript <$> justification)
      (uncurry PreparedAuthorityDispositionTranscript (encodeDisposition disposition))
      (memberSetDigestBytes members)

releasedStateTranscript :: ReleasedLabelState -> ReleasedStateTranscript
releasedStateTranscript state = case releasedLabelStateView state of
  ReleasedLabelView label -> ReleasedStateTranscript 0 (canonicalLabelBytes label)
  ReleasedDeletedView generation -> ReleasedStateTranscript 1 (Serialize.encode generation)

canonicalLabelBytes :: Label -> ByteString
canonicalLabelBytes = canonicalValueByteString . canonicalValueBytes . labelValue

justificationTranscript ::
  PriorAuthorityJustification ->
  PriorAuthorityJustificationTranscript
justificationTranscript justification = case priorAuthorityJustificationView justification of
  ExistingReleasedAuthorityView revision ->
    PriorAuthorityJustificationTranscript
      0
      (controlIndexWord64 (labelRevisionControlIndex revision))
      ByteString.empty
      0
      ByteString.empty
  CheckedGenesisAuthorityView digest ->
    PriorAuthorityJustificationTranscript
      1
      0
      (initialProjectionDigestBytes digest)
      0
      ByteString.empty
  EstablishedStructuralAuthorityView occurrence cut ->
    PriorAuthorityJustificationTranscript
      2
      0
      (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence))
      (structuralSequenceWord64 (structuralOccurrenceSourceSequence occurrence))
      (topologyCutIdBytes cut)

-- | Exact terminal outcome.  The released arm embeds the complete normalized
-- preparation facts as typed data, rather than accepting an unstructured digest
-- preimage.
data NormalizedTerminalOutcome authority disposition reason
  = NormalizedNotApplied
      LabelDecisionId
      ControlIndex
      GlobalObjectId
      reason
      CatalogueDigest
      MemberSetDigest
  | NormalizedReleased
      (NormalizedPreparedFacts authority disposition)
      PreparedLabelDigest
      ControlIndex
      ReleasedLabelState
      LabelRevision
      (Maybe authority)
  deriving stock (Eq, Show)

-- | Canonical terminal-outcome transcript.  The variant and reason encoders
-- are explicit so source constructor reordering cannot renumber either sum.
canonicalTerminalOutcomeBytes ::
  (authority -> ByteString) ->
  (disposition -> (Word8, ByteString)) ->
  (reason -> (Word8, ByteString)) ->
  NormalizedTerminalOutcome authority disposition reason ->
  ByteString
canonicalTerminalOutcomeBytes encodeAuthority encodeDisposition encodeReason outcome =
  Serialize.encode
    ( TerminalOutcomeTranscript
        "ECLIPS-LABEL-TERMINAL-OUTCOME"
        variantTag
        payload
    )
  where
    (variantTag, payload) = case outcome of
      NormalizedNotApplied decision terminalIndex object reason catalogue members ->
        ( 0,
          Serialize.encode
            ( NotAppliedOutcomeTranscript
                (labelDecisionIdBytes decision)
                (controlIndexWord64 terminalIndex)
                (globalObjectIdBytes object)
                reasonTag
                reasonPayload
                (catalogueDigestBytes catalogue)
                (memberSetDigestBytes members)
            )
        )
        where
          (reasonTag, reasonPayload) = encodeReason reason
      NormalizedReleased facts preparedDigest releaseIndex state revision authority ->
        ( 1,
          Serialize.encode
            ( ReleasedOutcomeTranscript
                (canonicalPreparedFactsBytes encodeAuthority encodeDisposition facts)
                (preparedLabelDigestBytes preparedDigest)
                (controlIndexWord64 releaseIndex)
                (releasedStateTranscript state)
                (controlIndexWord64 (labelRevisionControlIndex revision))
                (encodeAuthority <$> authority)
            )
        )

data TerminalOutcomeTranscript
  = TerminalOutcomeTranscript ByteString Word8 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data NotAppliedOutcomeTranscript
  = NotAppliedOutcomeTranscript
      ByteString
      Word64
      ByteString
      Word8
      ByteString
      ByteString
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReleasedOutcomeTranscript
  = ReleasedOutcomeTranscript
      ByteString
      ByteString
      Word64
      ReleasedStateTranscript
      Word64
      (Maybe ByteString)
  deriving stock (Generic)
  deriving anyclass (Serialize)
