{-# LANGUAGE OverloadedStrings #-}

-- | The sole descriptor definitions for the closed predefined catalogue.
--
-- Both application bindings and the semantic domain derive their identities
-- from these definitions. The domain separately admits them through its closed
-- primordial path; exporting their syntax grants no mutation authority. The four
-- topology sorts have immutable definitions; labels remain separately mutable.
module Eclips.Public.Types.SortCatalogue
  ( PredefinedSortRole (..),
    allPredefinedSortRoles,
    predefinedSortRoleTag,
    predefinedRawDescriptor,
    predefinedSortId,
  )
where

import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Word (Word8)
import Eclips.Public.Types.SortCanonical
import Eclips.Public.Types.SortId (SortId)

data PredefinedSortRole
  = SortDefinitionRole
  | NeutralVertexRole
  | EdgeRole
  | NablaRole
  | DeltaRole
  | ProcessEpochRole
  deriving stock (Eq, Ord, Show, Enum, Bounded)

allPredefinedSortRoles :: [PredefinedSortRole]
allPredefinedSortRoles = [minBound .. maxBound]

-- | Stable canonical tag, independent of the derived 'Enum' representation.
predefinedSortRoleTag :: PredefinedSortRole -> Word8
predefinedSortRoleTag = \case
  SortDefinitionRole -> 0
  NeutralVertexRole -> 1
  EdgeRole -> 2
  NablaRole -> 3
  DeltaRole -> 4
  ProcessEpochRole -> 5

predefinedSortId :: PredefinedSortRole -> SortId
predefinedSortId = rawDescriptorSortId . predefinedRawDescriptor

predefinedRawDescriptor :: PredefinedSortRole -> RawDescriptor
predefinedRawDescriptor SortDefinitionRole =
  regularSpec
    [("sort_id", RawBytesSchema), ("canonical_descriptor", RawBytesSchema)]
    "sort_id"
predefinedRawDescriptor NeutralVertexRole =
  controlledSpec
    RawNeutralVertexCarrier
    RawOrdinaryApplicationMutation
    [("object_id", RawGlobalUniqueIdSchema), ("label", RawLabelSchema)]
predefinedRawDescriptor EdgeRole =
  controlledSpec
    RawEdgeCarrier
    RawOrdinaryApplicationMutation
    [ ("object_id", RawGlobalUniqueIdSchema),
      ("label", RawLabelSchema),
      ("source_vertex", RawGlobalUniqueIdSchema),
      ("destination_vertex", RawGlobalUniqueIdSchema),
      ("strength", RawEnumSchema ["preserve", "weaken"])
    ]
predefinedRawDescriptor NablaRole =
  controlledSpec
    RawNablaCarrier
    RawOrdinaryApplicationMutation
    [ ("object_id", RawGlobalUniqueIdSchema),
      ("label", RawLabelSchema),
      ("sort_id", RawBytesSchema),
      ("sequencing_object", RawOptionalGlobalUniqueIdSchema)
    ]
predefinedRawDescriptor DeltaRole =
  controlledSpec
    RawDeltaCarrier
    RawOrdinaryApplicationMutation
    [ ("object_id", RawGlobalUniqueIdSchema),
      ("label", RawLabelSchema),
      ("sort_id", RawBytesSchema)
    ]
predefinedRawDescriptor ProcessEpochRole =
  controlledSpec
    RawProcessEpochCarrier
    RawHeraldManagedMutation
    [ ("object_id", RawGlobalUniqueIdSchema),
      ("label", RawLabelSchema),
      ("process_id", RawBytesSchema),
      ("residence", RawBytesSchema),
      ("live", RawBoolSchema)
    ]

regularSpec :: [(ByteString, RawSchema)] -> ByteString -> RawDescriptor
regularSpec fields key =
  RawDescriptor
    { domain = descriptorDomainTag,
      kind = RawRegularSort,
      schema = recordSchema fields,
      keyProjections = [RawProjection [key]],
      validity = RawAlwaysPredicate,
      obsolescence = RawNeverPredicate,
      rankTerms = [RawRankApplicationValue RawAscending],
      minimumRetentionMicros = 0,
      immutable = False,
      labelField = Nothing,
      applicationMutation = RawOrdinaryApplicationMutation,
      structuralCarrierRole = Nothing
    }

controlledSpec ::
  RawStructuralCarrierRole ->
  RawApplicationMutation ->
  [(ByteString, RawSchema)] ->
  RawDescriptor
controlledSpec role mutation fields =
  RawDescriptor
    { domain = descriptorDomainTag,
      kind = RawControlledSort,
      schema = recordSchema fields,
      keyProjections = [RawProjection ["object_id"]],
      validity = RawAlwaysPredicate,
      obsolescence = RawNeverPredicate,
      rankTerms = [RawRankApplicationValue RawAscending],
      minimumRetentionMicros = 0,
      immutable = case role of
        RawNeutralVertexCarrier -> True
        RawEdgeCarrier -> True
        RawNablaCarrier -> True
        RawDeltaCarrier -> True
        RawProcessEpochCarrier -> False,
      labelField = Just "label",
      applicationMutation = mutation,
      structuralCarrierRole = Just role
    }

recordSchema :: [(ByteString, RawSchema)] -> RawSchema
recordSchema = RawRecordSchema . sortOn fst
