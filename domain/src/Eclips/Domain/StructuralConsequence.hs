{-# LANGUAGE ImportQualifiedPost #-}

-- | Honest provenance for work caused by structural or ordered control change.
--
-- A control-stream position is carried only by causes which actually arise
-- from Oracle control.  In particular it is never reinterpreted as a structural
-- occurrence.
module Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    StructuralConsequenceCauseView (..),
    StructuralConsequenceCauseError (..),
    structuralOccurrenceCause,
    labelReleaseCause,
    processEndCause,
    predefinedDisappearanceCause,
    structuralConsequenceCauseView,
    structuralConsequenceCauseStructuralOccurrence,
    structuralConsequenceCauseControlIndex,
    structuralConsequenceCauseCanonicalBytes,
  )
where

import Data.ByteString (ByteString)
import Data.Serialize.Put qualified as Serialize
import Eclips.Domain.Disappearance
  ( DisappearanceProbeId,
    disappearanceProbeIdBytes,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    LabelDecisionId,
    ProcessEpochId,
    StructuralOccurrenceId,
    controlIndexWord64,
    heraldEpochBytes,
    labelDecisionIdBytes,
    processEpochIdBytes,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
  )

-- | The complete Herald-owner provenance vocabulary for a structural
-- consequence.
data StructuralConsequenceCause
  = StructuralOccurrenceCause StructuralOccurrenceId
  | LabelReleaseCause LabelDecisionId ControlIndex
  | ProcessEndCause ProcessEpochId ControlIndex
  | PredefinedDisappearanceCause DisappearanceProbeId ControlIndex
  deriving stock (Eq, Ord, Show)

-- | Read-only elimination vocabulary for the opaque cause sum.
data StructuralConsequenceCauseView
  = StructuralOccurrenceCauseView StructuralOccurrenceId
  | LabelReleaseCauseView LabelDecisionId ControlIndex
  | ProcessEndCauseView ProcessEpochId ControlIndex
  | PredefinedDisappearanceCauseView DisappearanceProbeId ControlIndex
  deriving stock (Eq, Ord, Show)

data StructuralConsequenceCauseError
  = StructuralConsequenceControlIndexMustBePositive
  deriving stock (Eq, Ord, Show)

structuralOccurrenceCause :: StructuralOccurrenceId -> StructuralConsequenceCause
structuralOccurrenceCause = StructuralOccurrenceCause

labelReleaseCause ::
  LabelDecisionId ->
  ControlIndex ->
  Either StructuralConsequenceCauseError StructuralConsequenceCause
labelReleaseCause decision index
  | controlIndexWord64 index == 0 =
      Left StructuralConsequenceControlIndexMustBePositive
  | otherwise = Right (LabelReleaseCause decision index)

processEndCause ::
  ProcessEpochId ->
  ControlIndex ->
  Either StructuralConsequenceCauseError StructuralConsequenceCause
processEndCause process index
  | controlIndexWord64 index == 0 =
      Left StructuralConsequenceControlIndexMustBePositive
  | otherwise = Right (ProcessEndCause process index)

predefinedDisappearanceCause ::
  DisappearanceProbeId ->
  ControlIndex ->
  Either StructuralConsequenceCauseError StructuralConsequenceCause
predefinedDisappearanceCause probe index
  | controlIndexWord64 index == 0 =
      Left StructuralConsequenceControlIndexMustBePositive
  | otherwise = Right (PredefinedDisappearanceCause probe index)

structuralConsequenceCauseView ::
  StructuralConsequenceCause -> StructuralConsequenceCauseView
structuralConsequenceCauseView cause = case cause of
  StructuralOccurrenceCause occurrence ->
    StructuralOccurrenceCauseView occurrence
  LabelReleaseCause decision index -> LabelReleaseCauseView decision index
  ProcessEndCause process index -> ProcessEndCauseView process index
  PredefinedDisappearanceCause probe index ->
    PredefinedDisappearanceCauseView probe index

structuralConsequenceCauseStructuralOccurrence ::
  StructuralConsequenceCause -> Maybe StructuralOccurrenceId
structuralConsequenceCauseStructuralOccurrence cause =
  case structuralConsequenceCauseView cause of
    StructuralOccurrenceCauseView occurrence -> Just occurrence
    LabelReleaseCauseView {} -> Nothing
    ProcessEndCauseView {} -> Nothing
    PredefinedDisappearanceCauseView {} -> Nothing

structuralConsequenceCauseControlIndex ::
  StructuralConsequenceCause -> Maybe ControlIndex
structuralConsequenceCauseControlIndex cause =
  case structuralConsequenceCauseView cause of
    StructuralOccurrenceCauseView {} -> Nothing
    LabelReleaseCauseView _ index -> Just index
    ProcessEndCauseView _ index -> Just index
    PredefinedDisappearanceCauseView _ index -> Just index

-- | Wire-independent canonical leaf bytes for consequence/evidence digests.
--
-- Tags are fixed at structural occurrence = 0, label release = 1, process End =
-- 2, and predefined disappearance = 3.  Multi-byte counters use network byte
-- order.
structuralConsequenceCauseCanonicalBytes ::
  StructuralConsequenceCause -> ByteString
structuralConsequenceCauseCanonicalBytes cause =
  Serialize.runPut $ case structuralConsequenceCauseView cause of
    StructuralOccurrenceCauseView occurrence -> do
      Serialize.putWord8 0
      Serialize.putByteString
        (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence))
      Serialize.putWord64be
        (structuralSequenceWord64 (structuralOccurrenceSourceSequence occurrence))
    LabelReleaseCauseView decision index -> do
      Serialize.putWord8 1
      Serialize.putByteString (labelDecisionIdBytes decision)
      Serialize.putWord64be (controlIndexWord64 index)
    ProcessEndCauseView process index -> do
      Serialize.putWord8 2
      Serialize.putByteString (processEpochIdBytes process)
      Serialize.putWord64be (controlIndexWord64 index)
    PredefinedDisappearanceCauseView probe index -> do
      Serialize.putWord8 3
      Serialize.putByteString (disappearanceProbeIdBytes probe)
      Serialize.putWord64be (controlIndexWord64 index)
