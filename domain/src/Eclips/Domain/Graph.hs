{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Universal graph-edge data for profile 0.1.
module Eclips.Domain.Graph
  ( VertexId (..),
    EdgeStrength (..),
    edgeStrengthSymbol,
    edgeStrengthFromSymbol,
    edgeStrengthTranscriptTag,
    EdgePayload,
    edgePayload,
    edgeSource,
    edgeDestination,
    edgeStrength,
  )
where

import Data.Int (Int64)
import Eclips.Domain.Identity
  ( DeltaId,
    GlobalObjectId,
    NablaId,
  )
import Eclips.Domain.Value (EnumSymbol, enumSymbol)

-- | A graph vertex. Nablas and deltas remain typed elsewhere; a neutral vertex
-- only routes.
data VertexId
  = NablaVertex NablaId
  | DeltaVertex DeltaId
  | NeutralVertex GlobalObjectId
  deriving stock (Eq, Ord, Show)

-- | Effect of traversing an edge on publication strength.
data EdgeStrength
  = Preserve
  | Weaken
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Readable closed symbols used by the predefined edge carrier.
edgeStrengthSymbol :: EdgeStrength -> EnumSymbol
edgeStrengthSymbol Preserve = enumSymbol "preserve"
edgeStrengthSymbol Weaken = enumSymbol "weaken"

edgeStrengthFromSymbol :: EnumSymbol -> Maybe EdgeStrength
edgeStrengthFromSymbol symbol
  | symbol == edgeStrengthSymbol Preserve = Just Preserve
  | symbol == edgeStrengthSymbol Weaken = Just Weaken
  | otherwise = Nothing

-- | Stable code retained by startup and structural-reconciliation evidence
-- transcripts.
--
-- This is deliberately independent of both declaration order and the readable
-- carrier symbol, so changing application presentation does not silently alter
-- those separate evidence formats.
edgeStrengthTranscriptTag :: EdgeStrength -> Int64
edgeStrengthTranscriptTag Preserve = 0
edgeStrengthTranscriptTag Weaken = 1

-- | The complete edge-specific payload.
--
-- There is intentionally no sort selector, predicate, allow-list, or dormant
-- extension field.
data EdgePayload = EdgePayload
  { source :: VertexId,
    destination :: VertexId,
    strength :: EdgeStrength
  }
  deriving stock (Eq, Ord, Show)

edgePayload :: VertexId -> VertexId -> EdgeStrength -> EdgePayload
edgePayload = EdgePayload

edgeSource :: EdgePayload -> VertexId
edgeSource payload = payload.source

edgeDestination :: EdgePayload -> VertexId
edgeDestination payload = payload.destination

edgeStrength :: EdgePayload -> EdgeStrength
edgeStrength payload = payload.strength
