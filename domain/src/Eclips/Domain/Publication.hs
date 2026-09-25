{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Checked immutable publication observations.
--
-- A caller supplies only a canonical descriptor, value, and stable provenance.
-- Key, lifecycle, application rank, and canonical value bytes
-- are always recomputed by the descriptor evaluator; no sender-supplied cache of
-- those facts can enter the checked type.
module Eclips.Domain.Publication
  ( CheckedPublication,
    PublicationError (..),
    PublicationLogicalState,
    PublicationWinnerKey,
    mkCheckedPublication,
    checkedPublicationId,
    checkedPublicationSort,
    checkedPublicationKey,
    checkedPublicationValue,
    checkedPublicationLifecycle,
    checkedPublicationApplicationRank,
    checkedPublicationCanonicalValue,
    checkedPublicationLogicalState,
    checkedPublicationWinnerKey,
    sameLogicalState,
  )
where

import Eclips.Domain.Identity
  ( PublicationId,
    SortId,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationRank,
    Lifecycle,
    ObjectKey,
    ValueEvaluationError,
    checkedInstanceCanonicalValue,
    checkedInstanceKey,
    checkedInstanceLifecycle,
    checkedInstanceRank,
    evaluateValue,
  )
import Eclips.Domain.Sort.Profile
  ( SortDefinitionIntrinsicError,
    validateProfilePublicationIntrinsic,
  )
import Eclips.Domain.Value
  ( CanonicalValueBytes,
    Value,
  )

-- | Facts whose equality defines one semantic state at a delta store.
--
-- Publication provenance and route strength are intentionally absent.  Equal
-- logical states can therefore join strength independently while retaining the
-- greatest provenance.
data PublicationLogicalState
  = PublicationLogicalState
      SortId
      ObjectKey
      Lifecycle
      CanonicalValueBytes
  deriving stock (Eq, Ord, Show)

-- | The normative total winner order: lifecycle first, then the descriptor's
-- total application rank, then stable publication identity.
data PublicationWinnerKey
  = PublicationWinnerKey
      Lifecycle
      ApplicationRank
      PublicationId
  deriving stock (Eq, Ord, Show)

-- | A checked publication carries both the original checked value and all
-- descriptor-derived facts required by winner/store transitions.
data CheckedPublication = CheckedPublication
  { provenance :: PublicationId,
    sortId :: SortId,
    key :: ObjectKey,
    value :: Value,
    lifecycle :: Lifecycle,
    rank :: ApplicationRank,
    canonicalValue :: CanonicalValueBytes
  }
  deriving stock (Eq, Show)

-- | Generic descriptor evaluation or the closed sort-definition carrier
-- intrinsic rejected the supplied value.
data PublicationError
  = PublicationValueRejected ValueEvaluationError
  | PublicationIntrinsicRejected SortDefinitionIntrinsicError
  deriving stock (Eq, Show)

mkCheckedPublication ::
  CanonicalDescriptor ->
  PublicationId ->
  Value ->
  Either PublicationError CheckedPublication
mkCheckedPublication descriptor publicationId value =
  do
    either
      (Left . PublicationIntrinsicRejected)
      Right
      (validateProfilePublicationIntrinsic descriptor value)
    checked <-
      either
        (Left . PublicationValueRejected)
        Right
        (evaluateValue (canonicalCheckedDescriptor descriptor) value)
    Right
      CheckedPublication
        { provenance = publicationId,
          sortId = descriptorSortId descriptor,
          key = checkedInstanceKey checked,
          value = value,
          lifecycle = checkedInstanceLifecycle checked,
          rank = checkedInstanceRank checked,
          canonicalValue = checkedInstanceCanonicalValue checked
        }

checkedPublicationId :: CheckedPublication -> PublicationId
checkedPublicationId publication = publication.provenance

checkedPublicationSort :: CheckedPublication -> SortId
checkedPublicationSort publication = publication.sortId

checkedPublicationKey :: CheckedPublication -> ObjectKey
checkedPublicationKey publication = publication.key

checkedPublicationValue :: CheckedPublication -> Value
checkedPublicationValue publication = publication.value

checkedPublicationLifecycle :: CheckedPublication -> Lifecycle
checkedPublicationLifecycle publication = publication.lifecycle

checkedPublicationApplicationRank :: CheckedPublication -> ApplicationRank
checkedPublicationApplicationRank publication = publication.rank

checkedPublicationCanonicalValue ::
  CheckedPublication ->
  CanonicalValueBytes
checkedPublicationCanonicalValue publication = publication.canonicalValue

checkedPublicationLogicalState ::
  CheckedPublication ->
  PublicationLogicalState
checkedPublicationLogicalState publication =
  PublicationLogicalState
    (checkedPublicationSort publication)
    (checkedPublicationKey publication)
    (checkedPublicationLifecycle publication)
    (checkedPublicationCanonicalValue publication)

checkedPublicationWinnerKey :: CheckedPublication -> PublicationWinnerKey
checkedPublicationWinnerKey publication =
  PublicationWinnerKey
    (checkedPublicationLifecycle publication)
    (checkedPublicationApplicationRank publication)
    (checkedPublicationId publication)

sameLogicalState :: CheckedPublication -> CheckedPublication -> Bool
sameLogicalState left right =
  checkedPublicationLogicalState left == checkedPublicationLogicalState right
