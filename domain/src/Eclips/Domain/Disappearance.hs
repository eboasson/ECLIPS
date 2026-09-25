{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Checked vocabulary shared by the live disappearance owners.
--
-- This module defines pure values and canonical transcripts with checked
-- decoding. Oracle commands and state, Herald state, and peer frame codecs
-- remain in their owning packages.
module Eclips.Domain.Disappearance
  ( admitDisappearanceSubjectMembershipCoordinate,
    decodeDisappearanceSubjectCanonicalBytes,
    decodeDisappearanceEvidenceClaimCanonicalBytes,
    decodeDisappearanceResolutionOutcomeCanonicalBytes,
    DisappearanceDigestProblem (..),
    CanonicalDescriptorDigest,
    mkCanonicalDescriptorDigest,
    canonicalDescriptorDigestBytes,
    deriveCanonicalDescriptorDigest,
    RegularSortOccurrenceClaim,
    RegularSortOccurrenceClaimProblem (..),
    deriveRegularSortOccurrenceClaim,
    admitRegularSortOccurrenceClaim,
    regularSortOccurrenceClaimSortId,
    regularSortOccurrenceClaimDescriptorDigest,
    regularSortOccurrenceClaimOccurrenceId,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    ControlledDisappearanceSubjectProblem (..),
    ControlledDisappearanceSubjectRevisionProblem (..),
    controlledPredefinedDisappearanceSubject,
    reviseControlledDisappearanceSubject,
    regularSortDefinitionDisappearanceSubject,
    disappearanceSubjectView,
    disappearanceSubjectCanonicalBytes,
    DisappearanceSubjectDigest,
    mkDisappearanceSubjectDigest,
    disappearanceSubjectDigestBytes,
    deriveDisappearanceSubjectDigest,
    DisappearanceProbeId,
    DisappearanceProbeIdentityProblem (..),
    deriveDisappearanceProbeId,
    admitDisappearanceProbeId,
    disappearanceProbeIdBytes,
    disappearanceProbeOpenControlIndex,
    DisappearanceSubjectMembershipCoordinate,
    disappearanceSubjectMembershipCoordinate,
    disappearanceCoordinateSubjectDigest,
    disappearanceCoordinateMembershipGenerationId,
    disappearanceCoordinateMemberSetDigest,
    DisappearanceEvidenceDigest,
    mkDisappearanceEvidenceDigest,
    disappearanceEvidenceDigestBytes,
    deriveDisappearanceEvidenceDigest,
    DisappearanceEvidenceClaim,
    DisappearanceEvidenceClaimProblem (..),
    admitDisappearanceEvidenceClaim,
    disappearanceEvidenceClaimCoordinate,
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceEvidenceClaimDigest,
    disappearanceEvidenceClaimCanonicalBytes,
    DisappearanceOpenResult,
    DisappearanceOpenResultView (..),
    openedDisappearanceProbe,
    aliasedDisappearanceProbe,
    disappearanceOpenResultView,
    DisappearanceResolutionOutcome,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceResolutionProblem (..),
    resolveDisappearanceSubject,
    disappearanceResolutionSubject,
    disappearanceResolutionControlIndex,
    disappearanceResolutionOutcomeView,
    disappearanceResolutionOutcomeCanonicalBytes,
    DisappearanceOutcomeDigest,
    mkDisappearanceOutcomeDigest,
    disappearanceOutcomeDigestBytes,
    deriveDisappearanceOutcomeDigest,
  )
where

import Control.Monad (unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize.Get qualified as Get
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    SortDefinitionOccurrenceId,
    SortId,
    StructuralOccurrenceId,
    SystemId,
    controlIndexWord64,
    globalObjectIdBytes,
    heraldEpochBytes,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label
  ( LabelRevision,
    labelRevisionControlIndex,
  )
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.MemberSet
  ( MemberSetDigest,
    memberSetDigestBytes,
  )
import Eclips.Domain.MemberSet qualified as MemberSet
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalDescriptorBytes,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase,
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)

digestByteCount :: Int
digestByteCount = 32

-- | Structural rejection shared by the four nominal SHA-256 digest wrappers.
data DisappearanceDigestProblem = DisappearanceDigestWrongByteCount
  { expectedDisappearanceDigestByteCount :: Int,
    actualDisappearanceDigestByteCount :: Int
  }
  deriving stock (Eq, Show)

checkDigestBytes :: ByteString -> Either DisappearanceDigestProblem ByteString
checkDigestBytes bytes
  | ByteString.length bytes == digestByteCount = Right bytes
  | otherwise =
      Left
        DisappearanceDigestWrongByteCount
          { expectedDisappearanceDigestByteCount = digestByteCount,
            actualDisappearanceDigestByteCount = ByteString.length bytes
          }

-- | Nominal digest of the exact canonical descriptor bytes.
--
-- The digest has the same bits as the descriptor's content-addressed 'SortId',
-- but its nominal type records the distinct claim made by the regular subject.
newtype CanonicalDescriptorDigest
  = CanonicalDescriptorDigest ByteString
  deriving stock (Eq, Ord)

instance Show CanonicalDescriptorDigest where
  show = renderGroupedHex . canonicalDescriptorDigestBytes

mkCanonicalDescriptorDigest ::
  ByteString -> Either DisappearanceDigestProblem CanonicalDescriptorDigest
mkCanonicalDescriptorDigest = fmap CanonicalDescriptorDigest . checkDigestBytes

canonicalDescriptorDigestBytes :: CanonicalDescriptorDigest -> ByteString
canonicalDescriptorDigestBytes (CanonicalDescriptorDigest bytes) = bytes

deriveCanonicalDescriptorDigest ::
  CanonicalDescriptor -> CanonicalDescriptorDigest
deriveCanonicalDescriptorDigest =
  digestInvariant "canonical descriptor"
    . mkCanonicalDescriptorDigest
    . SHA256.hash
    . canonicalDescriptorBytes

-- | Checked identity claim for one regular sort-definition occurrence.
--
-- The checked base is consumed while deriving or admitting the claim but is not
-- retained: the normative subject consists of exactly sort, descriptor digest,
-- and expected occurrence, and equal canonical subjects must be equal values.
data RegularSortOccurrenceClaim
  = RegularSortOccurrenceClaim
      SortId
      CanonicalDescriptorDigest
      SortDefinitionOccurrenceId
  deriving stock (Eq, Show)

data RegularSortOccurrenceClaimProblem
  = RegularSortOccurrenceClaimMismatch
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  deriving stock (Eq, Show)

deriveRegularSortOccurrenceClaim ::
  SystemId ->
  CanonicalDescriptor ->
  SortOccurrenceBase ->
  RegularSortOccurrenceClaim
deriveRegularSortOccurrenceClaim systemId descriptor base =
  RegularSortOccurrenceClaim
    sortId
    (deriveCanonicalDescriptorDigest descriptor)
    (deriveSortDefinitionOccurrenceId systemId sortId base)
  where
    sortId = descriptorSortId descriptor

-- | Admit an occurrence only after recomputing it from the exact checked inputs.
admitRegularSortOccurrenceClaim ::
  SystemId ->
  CanonicalDescriptor ->
  SortOccurrenceBase ->
  SortDefinitionOccurrenceId ->
  Either RegularSortOccurrenceClaimProblem RegularSortOccurrenceClaim
admitRegularSortOccurrenceClaim systemId descriptor base claimedOccurrence
  | claimedOccurrence == expectedOccurrence =
      Right (deriveRegularSortOccurrenceClaim systemId descriptor base)
  | otherwise =
      Left
        ( RegularSortOccurrenceClaimMismatch
            claimedOccurrence
            expectedOccurrence
        )
  where
    expectedOccurrence =
      deriveSortDefinitionOccurrenceId
        systemId
        (descriptorSortId descriptor)
        base

regularSortOccurrenceClaimSortId :: RegularSortOccurrenceClaim -> SortId
regularSortOccurrenceClaimSortId
  (RegularSortOccurrenceClaim sortId _ _) = sortId

regularSortOccurrenceClaimDescriptorDigest ::
  RegularSortOccurrenceClaim -> CanonicalDescriptorDigest
regularSortOccurrenceClaimDescriptorDigest
  (RegularSortOccurrenceClaim _ digest _) = digest

regularSortOccurrenceClaimOccurrenceId ::
  RegularSortOccurrenceClaim -> SortDefinitionOccurrenceId
regularSortOccurrenceClaimOccurrenceId
  (RegularSortOccurrenceClaim _ _ occurrence) = occurrence

-- | The two and only two profile-0.1 disappearance subject shapes.
--
-- Eligibility of a controlled object is checked against the home projection by
-- its owning transition; this closed value prevents it from being confused with
-- a regular-definition claim and prevents descriptor/occurrence drift in the
-- regular arm.
data DisappearanceSubject
  = ControlledPredefinedSubject
      GlobalObjectId
      StructuralOccurrenceId
      (Maybe LabelRevision)
  | RegularSortDefinitionSubject RegularSortOccurrenceClaim
  deriving stock (Eq, Show)

-- | Read-only elimination vocabulary for the opaque subject sum.
data DisappearanceSubjectView
  = ControlledPredefinedSubjectView
      GlobalObjectId
      StructuralOccurrenceId
      (Maybe LabelRevision)
  | RegularSortDefinitionSubjectView
      SortId
      CanonicalDescriptorDigest
      SortDefinitionOccurrenceId
  deriving stock (Eq, Show)

-- | A predefined role which cannot enter the controlled-disappearance path.
--
-- The eligible role is an admission witness only.  It is deliberately discarded
-- so the retained and canonical controlled subject remains exactly the three
-- fields named by the protocol design.
data ControlledDisappearanceSubjectProblem
  = ControlledDisappearanceSubjectIneligibleRole PredefinedSortRole
  deriving stock (Eq, Show)

-- | A revision can be changed only on a controlled subject which has already
-- passed role-qualified admission.  The helper deliberately takes the opaque
-- subject rather than another role witness: a later label terminal changes only
-- the retained label coordinate and must preserve object and occurrence
-- identity exactly.
data ControlledDisappearanceSubjectRevisionProblem
  = ControlledDisappearanceSubjectRevisionRequiresControlled
  deriving stock (Eq, Show)

controlledPredefinedDisappearanceSubject ::
  PredefinedSortRole ->
  GlobalObjectId ->
  StructuralOccurrenceId ->
  Maybe LabelRevision ->
  Either ControlledDisappearanceSubjectProblem DisappearanceSubject
controlledPredefinedDisappearanceSubject role object occurrence revision =
  case role of
    SortDefinitionRole ->
      Left (ControlledDisappearanceSubjectIneligibleRole role)
    ProcessEpochRole ->
      Left (ControlledDisappearanceSubjectIneligibleRole role)
    NeutralVertexRole -> admitted
    EdgeRole -> admitted
    NablaRole -> admitted
    DeltaRole -> admitted
  where
    admitted = Right (ControlledPredefinedSubject object occurrence revision)

reviseControlledDisappearanceSubject ::
  LabelRevision ->
  DisappearanceSubject ->
  Either ControlledDisappearanceSubjectRevisionProblem DisappearanceSubject
reviseControlledDisappearanceSubject revision subject = case subject of
  ControlledPredefinedSubject object occurrence _ ->
    Right (ControlledPredefinedSubject object occurrence (Just revision))
  RegularSortDefinitionSubject {} ->
    Left ControlledDisappearanceSubjectRevisionRequiresControlled

regularSortDefinitionDisappearanceSubject ::
  RegularSortOccurrenceClaim -> DisappearanceSubject
regularSortDefinitionDisappearanceSubject = RegularSortDefinitionSubject

disappearanceSubjectView ::
  DisappearanceSubject -> DisappearanceSubjectView
disappearanceSubjectView subject = case subject of
  ControlledPredefinedSubject object occurrence revision ->
    ControlledPredefinedSubjectView object occurrence revision
  RegularSortDefinitionSubject claim ->
    RegularSortDefinitionSubjectView
      (regularSortOccurrenceClaimSortId claim)
      (regularSortOccurrenceClaimDescriptorDigest claim)
      (regularSortOccurrenceClaimOccurrenceId claim)

-- | Manual canonical transcript used for ordering, subject identity, and the
-- current wire payload. Controlled is tag 0 and regular is tag 1. All fields are
-- fixed-width except the explicit optional-label tag.
disappearanceSubjectCanonicalBytes :: DisappearanceSubject -> ByteString
disappearanceSubjectCanonicalBytes subject =
  buildCanonical
    $ framedBytes disappearanceSubjectDomain
      <> Builder.word8 (disappearanceSubjectArmTag subject)
      <> case subject of
        ControlledPredefinedSubject object occurrence revision ->
          Builder.byteString (globalObjectIdBytes object)
            <> structuralOccurrenceBuilder occurrence
            <> maybe
              (Builder.word8 0)
              ( \labelRevision ->
                  Builder.word8 1
                    <> Builder.word64BE
                      (controlIndexWord64 (labelRevisionControlIndex labelRevision))
              )
              revision
        RegularSortDefinitionSubject claim ->
          Builder.byteString
            (sortIdBytes (regularSortOccurrenceClaimSortId claim))
            <> Builder.byteString
              ( canonicalDescriptorDigestBytes
                  (regularSortOccurrenceClaimDescriptorDigest claim)
              )
            <> Builder.byteString
              ( sortDefinitionOccurrenceIdBytes
                  (regularSortOccurrenceClaimOccurrenceId claim)
              )

disappearanceSubjectDomain :: ByteString
disappearanceSubjectDomain = "ECLIPS-DISAPPEARANCE-SUBJECT"

disappearanceSubjectArmTag :: DisappearanceSubject -> Word8
disappearanceSubjectArmTag subject = case subject of
  ControlledPredefinedSubject {} -> 0
  RegularSortDefinitionSubject {} -> 1

-- | Normative order: controlled subjects first, then regular subjects, with an
-- unsigned bytewise comparison of the manual transcript inside each arm.
instance Ord DisappearanceSubject where
  compare left right =
    compare
      (disappearanceSubjectArmTag left)
      (disappearanceSubjectArmTag right)
      <> compare
        (disappearanceSubjectCanonicalBytes left)
        (disappearanceSubjectCanonicalBytes right)

newtype DisappearanceSubjectDigest
  = DisappearanceSubjectDigest ByteString
  deriving stock (Eq, Ord)

instance Show DisappearanceSubjectDigest where
  show = renderGroupedHex . disappearanceSubjectDigestBytes

mkDisappearanceSubjectDigest ::
  ByteString -> Either DisappearanceDigestProblem DisappearanceSubjectDigest
mkDisappearanceSubjectDigest = fmap DisappearanceSubjectDigest . checkDigestBytes

disappearanceSubjectDigestBytes :: DisappearanceSubjectDigest -> ByteString
disappearanceSubjectDigestBytes (DisappearanceSubjectDigest bytes) = bytes

deriveDisappearanceSubjectDigest ::
  DisappearanceSubject -> DisappearanceSubjectDigest
deriveDisappearanceSubjectDigest =
  digestInvariant "disappearance subject"
    . mkDisappearanceSubjectDigest
    . SHA256.hash
    . disappearanceSubjectCanonicalBytes

-- | Canonical ID of the first committed Open entry for one probe.
--
-- The retained index is part of the opaque invariant and is useful to later
-- Oracle records; neither a bare digest nor index zero can construct this type.
data DisappearanceProbeId
  = DisappearanceProbeId ByteString ControlIndex
  deriving stock (Eq, Ord)

instance Show DisappearanceProbeId where
  show = renderGroupedHex . disappearanceProbeIdBytes

data DisappearanceProbeIdentityProblem
  = DisappearanceProbeOpenControlIndexMustBePositive
  | DisappearanceProbeClaimedDigestInvalid DisappearanceDigestProblem
  | DisappearanceProbeClaimedDigestMismatch ByteString ByteString
  deriving stock (Eq, Show)

deriveDisappearanceProbeId ::
  ControlIndex ->
  Either DisappearanceProbeIdentityProblem DisappearanceProbeId
deriveDisappearanceProbeId openIndex
  | controlIndexWord64 openIndex == 0 =
      Left DisappearanceProbeOpenControlIndexMustBePositive
  | otherwise =
      Right
        ( DisappearanceProbeId
            ( SHA256.hash
                ( buildCanonical
                    ( framedBytes disappearanceProbeDomain
                        <> Builder.word64BE (controlIndexWord64 openIndex)
                    )
                )
            )
            openIndex
        )

-- | Recompute and validate a claimed probe digest at its committed Open index.
admitDisappearanceProbeId ::
  ControlIndex ->
  ByteString ->
  Either DisappearanceProbeIdentityProblem DisappearanceProbeId
admitDisappearanceProbeId openIndex claimedBytes = do
  _ <-
    either
      (Left . DisappearanceProbeClaimedDigestInvalid)
      Right
      (checkDigestBytes claimedBytes)
  expected <- deriveDisappearanceProbeId openIndex
  if claimedBytes == disappearanceProbeIdBytes expected
    then Right expected
    else
      Left
        ( DisappearanceProbeClaimedDigestMismatch
            claimedBytes
            (disappearanceProbeIdBytes expected)
        )

disappearanceProbeIdBytes :: DisappearanceProbeId -> ByteString
disappearanceProbeIdBytes (DisappearanceProbeId bytes _) = bytes

disappearanceProbeOpenControlIndex :: DisappearanceProbeId -> ControlIndex
disappearanceProbeOpenControlIndex (DisappearanceProbeId _ index) = index

disappearanceProbeDomain :: ByteString
disappearanceProbeDomain = "ECLIPS-DISAPPEARANCE-PROBE"

-- | Exact subject and membership coordinate captured by an Open.
data DisappearanceSubjectMembershipCoordinate
  = DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectDigest
      HeraldMembershipGenerationId
      MemberSetDigest
  deriving stock (Eq, Ord, Show)

disappearanceSubjectMembershipCoordinate ::
  DisappearanceSubject ->
  HeraldMembershipGeneration ->
  DisappearanceSubjectMembershipCoordinate
disappearanceSubjectMembershipCoordinate subject generation =
  DisappearanceSubjectMembershipCoordinate
    (deriveDisappearanceSubjectDigest subject)
    (heraldMembershipGenerationId generation)
    (heraldMembershipGenerationActiveMemberSetDigest generation)

disappearanceCoordinateSubjectDigest ::
  DisappearanceSubjectMembershipCoordinate -> DisappearanceSubjectDigest
disappearanceCoordinateSubjectDigest
  (DisappearanceSubjectMembershipCoordinate digest _ _) = digest

disappearanceCoordinateMembershipGenerationId ::
  DisappearanceSubjectMembershipCoordinate -> HeraldMembershipGenerationId
disappearanceCoordinateMembershipGenerationId
  (DisappearanceSubjectMembershipCoordinate _ generation _) = generation

disappearanceCoordinateMemberSetDigest ::
  DisappearanceSubjectMembershipCoordinate -> MemberSetDigest
disappearanceCoordinateMemberSetDigest
  (DisappearanceSubjectMembershipCoordinate _ _ members) = members

-- | Nominal digest of one owner's canonical local absence evidence payload.
newtype DisappearanceEvidenceDigest
  = DisappearanceEvidenceDigest ByteString
  deriving stock (Eq, Ord)

instance Show DisappearanceEvidenceDigest where
  show = renderGroupedHex . disappearanceEvidenceDigestBytes

mkDisappearanceEvidenceDigest ::
  ByteString -> Either DisappearanceDigestProblem DisappearanceEvidenceDigest
mkDisappearanceEvidenceDigest = fmap DisappearanceEvidenceDigest . checkDigestBytes

disappearanceEvidenceDigestBytes :: DisappearanceEvidenceDigest -> ByteString
disappearanceEvidenceDigestBytes (DisappearanceEvidenceDigest bytes) = bytes

-- | Domain-separate a later owner's already canonical evidence payload.
--
-- This is only a nominal content claim: it does not establish that arbitrary
-- bytes constitute complete or semantically valid absence evidence.  A later
-- evidence owner must first normalize and check its typed report, then pass that
-- owner's canonical payload here.  This function supplies the shared digest law
-- and deliberately has no inverse wire decoder.
deriveDisappearanceEvidenceDigest :: ByteString -> DisappearanceEvidenceDigest
deriveDisappearanceEvidenceDigest payload =
  digestInvariant "disappearance evidence"
    . mkDisappearanceEvidenceDigest
    . SHA256.hash
    . buildCanonical
    $ framedBytes disappearanceEvidenceDomain <> framedBytes payload

disappearanceEvidenceDomain :: ByteString
disappearanceEvidenceDomain = "ECLIPS-DISAPPEARANCE-EVIDENCE"

-- | One reporter's evidence bound to the exact probe, subject, and captured
-- membership coordinate.  Construction also proves that the reporter belongs to
-- the captured generation.
data DisappearanceEvidenceClaim
  = DisappearanceEvidenceClaim
      DisappearanceSubjectMembershipCoordinate
      DisappearanceProbeId
      HeraldEpoch
      DisappearanceEvidenceDigest
  deriving stock (Eq, Show)

data DisappearanceEvidenceClaimProblem
  = DisappearanceEvidenceReporterNotCaptured HeraldEpoch
  deriving stock (Eq, Show)

admitDisappearanceEvidenceClaim ::
  DisappearanceSubject ->
  HeraldMembershipGeneration ->
  DisappearanceProbeId ->
  HeraldEpoch ->
  DisappearanceEvidenceDigest ->
  Either DisappearanceEvidenceClaimProblem DisappearanceEvidenceClaim
admitDisappearanceEvidenceClaim subject generation probe reporter evidence
  | reporter
      `elem` NonEmpty.toList
        (heraldMembershipGenerationActiveHeraldEpochs generation) =
      Right
        ( DisappearanceEvidenceClaim
            (disappearanceSubjectMembershipCoordinate subject generation)
            probe
            reporter
            evidence
        )
  | otherwise = Left (DisappearanceEvidenceReporterNotCaptured reporter)

disappearanceEvidenceClaimCoordinate ::
  DisappearanceEvidenceClaim -> DisappearanceSubjectMembershipCoordinate
disappearanceEvidenceClaimCoordinate
  (DisappearanceEvidenceClaim coordinate _ _ _) = coordinate

disappearanceEvidenceClaimProbeId ::
  DisappearanceEvidenceClaim -> DisappearanceProbeId
disappearanceEvidenceClaimProbeId
  (DisappearanceEvidenceClaim _ probe _ _) = probe

disappearanceEvidenceClaimReporter ::
  DisappearanceEvidenceClaim -> HeraldEpoch
disappearanceEvidenceClaimReporter
  (DisappearanceEvidenceClaim _ _ reporter _) = reporter

disappearanceEvidenceClaimDigest ::
  DisappearanceEvidenceClaim -> DisappearanceEvidenceDigest
disappearanceEvidenceClaimDigest
  (DisappearanceEvidenceClaim _ _ _ digest) = digest

-- | Manual identity transcript for later evidence-command hashing.  It is not a
-- protocol serialization and intentionally has no decoder or serialization
-- type-class instance.
disappearanceEvidenceClaimCanonicalBytes ::
  DisappearanceEvidenceClaim -> ByteString
disappearanceEvidenceClaimCanonicalBytes claim =
  buildCanonical
    $ framedBytes disappearanceEvidenceClaimDomain
      <> Builder.byteString
        ( disappearanceSubjectDigestBytes
            ( disappearanceCoordinateSubjectDigest
                (disappearanceEvidenceClaimCoordinate claim)
            )
        )
      <> Builder.byteString
        ( heraldMembershipGenerationIdBytes
            ( disappearanceCoordinateMembershipGenerationId
                (disappearanceEvidenceClaimCoordinate claim)
            )
        )
      <> Builder.byteString
        ( memberSetDigestBytes
            ( disappearanceCoordinateMemberSetDigest
                (disappearanceEvidenceClaimCoordinate claim)
            )
        )
      <> Builder.byteString
        (disappearanceProbeIdBytes (disappearanceEvidenceClaimProbeId claim))
      <> Builder.byteString
        (heraldEpochBytes (disappearanceEvidenceClaimReporter claim))
      <> Builder.byteString
        ( disappearanceEvidenceDigestBytes
            (disappearanceEvidenceClaimDigest claim)
        )

disappearanceEvidenceClaimDomain :: ByteString
disappearanceEvidenceClaimDomain = "ECLIPS-DISAPPEARANCE-EVIDENCE-CLAIM"

-- | Immutable result of one applied Open command.
data DisappearanceOpenResult
  = DisappearanceProbeOpened DisappearanceProbeId
  | DisappearanceProbeAliased DisappearanceProbeId
  deriving stock (Eq, Show)

data DisappearanceOpenResultView
  = OpenedDisappearanceProbe DisappearanceProbeId
  | AliasedDisappearanceProbe DisappearanceProbeId
  deriving stock (Eq, Show)

openedDisappearanceProbe :: DisappearanceProbeId -> DisappearanceOpenResult
openedDisappearanceProbe = DisappearanceProbeOpened

aliasedDisappearanceProbe :: DisappearanceProbeId -> DisappearanceOpenResult
aliasedDisappearanceProbe = DisappearanceProbeAliased

disappearanceOpenResultView ::
  DisappearanceOpenResult -> DisappearanceOpenResultView
disappearanceOpenResultView result = case result of
  DisappearanceProbeOpened probe -> OpenedDisappearanceProbe probe
  DisappearanceProbeAliased probe -> AliasedDisappearanceProbe probe

-- | Checked terminal lifecycle result of a successful Resolve.
data DisappearanceResolutionOutcome
  = ControlledDisappearanceResolution
      DisappearanceSubject
      ControlIndex
  | RegularDefinitionRetirementResolution
      DisappearanceSubject
      ControlIndex
      SortDefinitionOccurrenceId
  deriving stock (Eq, Show)

data DisappearanceResolutionOutcomeView
  = ControlledDisappearanceResolved
      GlobalObjectId
      StructuralOccurrenceId
      (Maybe LabelRevision)
      ControlIndex
  | RegularSortDefinitionRetired
      SortId
      CanonicalDescriptorDigest
      SortDefinitionOccurrenceId
      ControlIndex
      SortDefinitionOccurrenceId
  deriving stock (Eq, Show)

data DisappearanceResolutionProblem
  = DisappearanceResolutionControlIndexMustBePositive
  deriving stock (Eq, Show)

-- | Resolve one subject at a positive applied Oracle index.
--
-- For the regular arm that exact index is admitted as a retirement base and is
-- then fed back through the sole occurrence derivation law to name the next
-- possible occurrence of the same content-addressed sort.
resolveDisappearanceSubject ::
  SystemId ->
  ControlIndex ->
  DisappearanceSubject ->
  Either DisappearanceResolutionProblem DisappearanceResolutionOutcome
resolveDisappearanceSubject systemId resolveIndex subject
  | controlIndexWord64 resolveIndex == 0 =
      Left DisappearanceResolutionControlIndexMustBePositive
  | otherwise = Right $ case subject of
      ControlledPredefinedSubject {} ->
        ControlledDisappearanceResolution subject resolveIndex
      RegularSortDefinitionSubject claim ->
        RegularDefinitionRetirementResolution
          subject
          resolveIndex
          ( deriveSortDefinitionOccurrenceId
              systemId
              (regularSortOccurrenceClaimSortId claim)
              nextBase
          )
  where
    nextBase =
      either
        (error . ("positive disappearance resolution index rejected: " <>) . show)
        id
        (resolvedRetirementOccurrenceBase resolveIndex)

disappearanceResolutionSubject ::
  DisappearanceResolutionOutcome -> DisappearanceSubject
disappearanceResolutionSubject outcome = case outcome of
  ControlledDisappearanceResolution subject _ -> subject
  RegularDefinitionRetirementResolution subject _ _ -> subject

disappearanceResolutionControlIndex ::
  DisappearanceResolutionOutcome -> ControlIndex
disappearanceResolutionControlIndex outcome = case outcome of
  ControlledDisappearanceResolution _ index -> index
  RegularDefinitionRetirementResolution _ index _ -> index

disappearanceResolutionOutcomeView ::
  DisappearanceResolutionOutcome -> DisappearanceResolutionOutcomeView
disappearanceResolutionOutcomeView outcome = case outcome of
  ControlledDisappearanceResolution subject index ->
    case disappearanceSubjectView subject of
      ControlledPredefinedSubjectView object occurrence revision ->
        ControlledDisappearanceResolved object occurrence revision index
      RegularSortDefinitionSubjectView {} ->
        error "controlled disappearance outcome retained a regular subject"
  RegularDefinitionRetirementResolution subject index nextOccurrence ->
    case disappearanceSubjectView subject of
      RegularSortDefinitionSubjectView sortId descriptorDigest occurrence ->
        RegularSortDefinitionRetired
          sortId
          descriptorDigest
          occurrence
          index
          nextOccurrence
      ControlledPredefinedSubjectView {} ->
        error "regular retirement outcome retained a controlled subject"

-- | Manual transcript of the checked outcome.  The subject transcript is framed
-- so future subject additions cannot collide with the resolution suffix.
disappearanceResolutionOutcomeCanonicalBytes ::
  DisappearanceResolutionOutcome -> ByteString
disappearanceResolutionOutcomeCanonicalBytes outcome =
  buildCanonical
    $ framedBytes disappearanceOutcomeDomain
      <> Builder.word8 armTag
      <> framedBytes
        (disappearanceSubjectCanonicalBytes (disappearanceResolutionSubject outcome))
      <> Builder.word64BE
        (controlIndexWord64 (disappearanceResolutionControlIndex outcome))
      <> nextOccurrenceBuilder
  where
    (armTag, nextOccurrenceBuilder) = case outcome of
      ControlledDisappearanceResolution {} -> (0, mempty)
      RegularDefinitionRetirementResolution _ _ nextOccurrence ->
        ( 1,
          Builder.byteString
            (sortDefinitionOccurrenceIdBytes nextOccurrence)
        )

newtype DisappearanceOutcomeDigest
  = DisappearanceOutcomeDigest ByteString
  deriving stock (Eq, Ord)

instance Show DisappearanceOutcomeDigest where
  show = renderGroupedHex . disappearanceOutcomeDigestBytes

mkDisappearanceOutcomeDigest ::
  ByteString -> Either DisappearanceDigestProblem DisappearanceOutcomeDigest
mkDisappearanceOutcomeDigest = fmap DisappearanceOutcomeDigest . checkDigestBytes

disappearanceOutcomeDigestBytes :: DisappearanceOutcomeDigest -> ByteString
disappearanceOutcomeDigestBytes (DisappearanceOutcomeDigest bytes) = bytes

deriveDisappearanceOutcomeDigest ::
  DisappearanceResolutionOutcome -> DisappearanceOutcomeDigest
deriveDisappearanceOutcomeDigest =
  digestInvariant "disappearance outcome"
    . mkDisappearanceOutcomeDigest
    . SHA256.hash
    . disappearanceResolutionOutcomeCanonicalBytes

disappearanceOutcomeDomain :: ByteString
disappearanceOutcomeDomain = "ECLIPS-DISAPPEARANCE-OUTCOME"

structuralOccurrenceBuilder :: StructuralOccurrenceId -> Builder.Builder
structuralOccurrenceBuilder occurrence =
  Builder.byteString
    ( heraldEpochBytes
        (structuralOccurrenceSourceHeraldEpoch occurrence)
    )
    <> Builder.word64BE
      ( structuralSequenceWord64
          (structuralOccurrenceSourceSequence occurrence)
      )

framedBytes :: ByteString -> Builder.Builder
framedBytes bytes =
  Builder.word64BE (fromIntegral (ByteString.length bytes) :: Word64)
    <> Builder.byteString bytes

buildCanonical :: Builder.Builder -> ByteString
buildCanonical = LazyByteString.toStrict . Builder.toLazyByteString

digestInvariant ::
  String ->
  Either DisappearanceDigestProblem digest ->
  digest
digestInvariant context =
  either
    (error . (("invalid closed " <> context <> " digest: ") <>) . show)
    id

-- | Admit a structurally complete wire coordinate. The owner compares it with
-- the exact projected subject and membership before using any semantic claim.
admitDisappearanceSubjectMembershipCoordinate ::
  DisappearanceSubjectDigest -> HeraldMembershipGenerationId -> MemberSetDigest -> DisappearanceSubjectMembershipCoordinate
admitDisappearanceSubjectMembershipCoordinate = DisappearanceSubjectMembershipCoordinate

-- | Decode the sole-current canonical subject shape. Role eligibility, current
-- label revision, and effective regular occurrence are checked by the owner.
decodeDisappearanceSubjectCanonicalBytes :: ByteString -> Either String DisappearanceSubject
decodeDisappearanceSubjectCanonicalBytes = decodeCanonical disappearanceSubjectCanonicalBytes $ do
  expectDomain disappearanceSubjectDomain
  tag <- Get.getWord8
  case tag of
    0 -> do
      object <- decodeFixed Identity.mkGlobalObjectId
      source <- decodeFixed Identity.mkHeraldEpoch
      sequenceNumber <- Get.getWord64be >>= admitDecoded . Identity.mkStructuralSequence
      optionalRevision <- Get.getWord8
      revision <- case optionalRevision of
        0 -> pure Nothing
        1 -> Just <$> (Get.getWord64be >>= admitDecoded . Label.mkLabelRevision . Identity.controlIndex)
        _ -> fail "unknown controlled disappearance revision tag"
      pure (ControlledPredefinedSubject object (Identity.structuralOccurrenceId source sequenceNumber) revision)
    1 -> do
      sort <- decodeFixed Identity.mkSortId
      descriptorDigest <- decodeFixed mkCanonicalDescriptorDigest
      unless
        (Identity.sortIdBytes sort == canonicalDescriptorDigestBytes descriptorDigest)
        (fail "regular disappearance descriptor digest differs from its sort identity")
      occurrence <- decodeFixed Identity.mkSortDefinitionOccurrenceId
      pure (RegularSortDefinitionSubject (RegularSortOccurrenceClaim sort descriptorDigest occurrence))
    _ -> fail "unknown disappearance subject tag"

-- | The Open index is carried beside this identity transcript on the live wire;
-- it authenticates the otherwise one-way probe digest without adding a second ID.
decodeDisappearanceEvidenceClaimCanonicalBytes :: ControlIndex -> ByteString -> Either String DisappearanceEvidenceClaim
decodeDisappearanceEvidenceClaimCanonicalBytes openIndex = decodeCanonical disappearanceEvidenceClaimCanonicalBytes $ do
  expectDomain disappearanceEvidenceClaimDomain
  subjectDigest <- decodeFixed mkDisappearanceSubjectDigest
  generation <- decodeFixed Membership.mkHeraldMembershipGenerationId
  members <- decodeFixed MemberSet.mkMemberSetDigest
  probe <- Get.getBytes 32 >>= admitDecoded . admitDisappearanceProbeId openIndex
  reporter <- decodeFixed Identity.mkHeraldEpoch
  digest <- decodeFixed mkDisappearanceEvidenceDigest
  pure (DisappearanceEvidenceClaim (DisappearanceSubjectMembershipCoordinate subjectDigest generation members) probe reporter digest)

-- | Shape admission for an immutable projected outcome. The consuming live
-- owner recomputes its exact local-System resolution before installing authority.
decodeDisappearanceResolutionOutcomeCanonicalBytes :: ByteString -> Either String DisappearanceResolutionOutcome
decodeDisappearanceResolutionOutcomeCanonicalBytes = decodeCanonical disappearanceResolutionOutcomeCanonicalBytes $ do
  expectDomain disappearanceOutcomeDomain
  tag <- Get.getWord8
  subject <- getCanonicalFrame >>= either fail pure . decodeDisappearanceSubjectCanonicalBytes
  index <- Identity.controlIndex <$> Get.getWord64be
  unless (Identity.controlIndexWord64 index > 0) (fail "disappearance Resolve index must be positive")
  case (tag, subject) of
    (0, ControlledPredefinedSubject {}) -> pure (ControlledDisappearanceResolution subject index)
    (1, RegularSortDefinitionSubject {}) -> RegularDefinitionRetirementResolution subject index <$> decodeFixed Identity.mkSortDefinitionOccurrenceId
    _ -> fail "disappearance outcome arm does not match its subject"

decodeCanonical :: (a -> ByteString) -> Get.Get a -> ByteString -> Either String a
decodeCanonical encode parser bytes = do
  value <- Get.runGet (parser <* ensureEnd) bytes
  if encode value == bytes then Right value else Left "noncanonical disappearance bytes"
  where
    ensureEnd = Get.isEmpty >>= \empty -> unless empty (fail "trailing disappearance bytes")

admitDecoded :: (Show problem) => Either problem a -> Get.Get a
admitDecoded = either (fail . show) pure

decodeFixed :: (Show problem) => (ByteString -> Either problem a) -> Get.Get a
decodeFixed admit = Get.getBytes 32 >>= admitDecoded . admit

getCanonicalFrame :: Get.Get ByteString
getCanonicalFrame = do
  count <- Get.getWord64be
  remaining <- Get.remaining
  unless (count <= fromIntegral remaining) (fail "truncated disappearance frame")
  Get.getBytes (fromIntegral count)

expectDomain :: ByteString -> Get.Get ()
expectDomain expected = do
  supplied <- getCanonicalFrame
  unless (supplied == expected) (fail "incorrect disappearance domain")
