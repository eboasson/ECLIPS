-- | Immutable application-publication evidence shared by the application and
-- publication owners.
--
-- Keeping this vocabulary outside either owner prevents their private states
-- from depending on each other merely to retain already admitted work.
module Eclips.Herald.Application.PublicationEvidence
  ( AdmittedApplicationPublicationValue,
    admittedOrdinaryApplicationPublicationValue,
    admittedSortDefinitionApplicationPublicationValue,
    admittedApplicationPublicationValue,
    admittedApplicationPublicationSortDefinition,
    ApplicationPublicationOrigin (..),
  )
where

import Eclips.Domain.Identity (GlobalObjectId, GlobalUniqueId)
import Eclips.Domain.Route (ReplicaStrength)
import Eclips.Domain.Value (Value)
import Eclips.Herald.Application.SortDefinition
  ( AdmittedApplicationSortDefinition,
    admittedApplicationSortValue,
  )

-- | The exact value-admission evidence retained with the application request.
-- Keeping the structured arm here lets an exact retry return before repeating
-- globalization or sort-definition admission, and gives same-cut SortRegistry
-- coordination the original checked descriptor evidence.
data AdmittedApplicationPublicationValue
  = AdmittedOrdinaryApplicationPublicationValue Value
  | AdmittedSortDefinitionApplicationPublicationValue
      AdmittedApplicationSortDefinition
  deriving stock (Eq, Show)

-- | Retain an already globalized ordinary value. The constructor remains
-- private so structured sort evidence cannot be paired with unrelated bytes.
admittedOrdinaryApplicationPublicationValue ::
  Value -> AdmittedApplicationPublicationValue
admittedOrdinaryApplicationPublicationValue =
  AdmittedOrdinaryApplicationPublicationValue

-- | Retain one admitted sort definition. Its semantic value is derived from
-- the same opaque admission evidence, making materialized-value drift
-- unrepresentable before same-cut SortRegistry induction.
admittedSortDefinitionApplicationPublicationValue ::
  AdmittedApplicationSortDefinition ->
  AdmittedApplicationPublicationValue
admittedSortDefinitionApplicationPublicationValue =
  AdmittedSortDefinitionApplicationPublicationValue

admittedApplicationPublicationValue ::
  AdmittedApplicationPublicationValue -> Value
admittedApplicationPublicationValue = \case
  AdmittedOrdinaryApplicationPublicationValue value -> value
  AdmittedSortDefinitionApplicationPublicationValue admitted ->
    admittedApplicationSortValue admitted

admittedApplicationPublicationSortDefinition ::
  AdmittedApplicationPublicationValue ->
  Maybe AdmittedApplicationSortDefinition
admittedApplicationPublicationSortDefinition = \case
  AdmittedOrdinaryApplicationPublicationValue _ -> Nothing
  AdmittedSortDefinitionApplicationPublicationValue admitted -> Just admitted

-- | Immutable admission origin. In particular a published pristine
-- reservation can never later be reinterpreted as an existing-object update.
data ApplicationPublicationOrigin
  = RegularApplicationPublicationOrigin
  | ControlledFirstUseApplicationPublicationOrigin GlobalUniqueId
  | ControlledUpdateApplicationPublicationOrigin GlobalObjectId
  | ForwardApplicationPublicationOrigin GlobalObjectId ReplicaStrength
  deriving stock (Eq, Show)
