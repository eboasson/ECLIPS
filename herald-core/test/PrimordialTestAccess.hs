-- | Assertions for tests whose fixture explicitly selects a whole environment.
module PrimordialTestAccess (conventionalStartupPairs, conventionalStartupEnvironment, fixtureEnvironmentHub, fixtureEnvironmentEdges) where

import Data.ByteString qualified as ByteString

import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Domain.Graph (EdgePayload, EdgeStrength (Preserve), VertexId (..), edgePayload)
import Eclips.Domain.Identity (GlobalObjectId, mkGlobalObjectId)
import Eclips.Domain.Sort.Profile (allPredefinedSortRoles)
import Eclips.Herald.Application.State qualified as Application

conventionalStartupPairs :: Access.ApplicationStartupAccess -> [Access.PredefinedAccess]
conventionalStartupPairs startup = map pair Access.allApplicationPredefinedSortRoles
  where
    entries = Access.accessEntries (Access.startupAccessPrimordial startup)
    pair role = case (Map.lookup (Access.environmentWriterKey role) entries, Map.lookup (Access.environmentReaderKey role) entries) of
      (Just (Access.Writer writer), Just (Access.Reader reader)) -> Access.predefinedAccess role writer reader
      _ -> error "test fixture did not select the conventional environment interface"

-- | Reconstruct only a fixture that explicitly selected the full environment.
conventionalStartupEnvironment :: Access.ApplicationStartupAccess -> Access.EnvironmentAccess
conventionalStartupEnvironment startup = checked (Access.environmentAccess (conventionalStartupPairs startup) (object Access.environmentHubKey) edges)
  where
    entries = Access.accessEntries (Access.startupAccessPrimordial startup)
    object key = case Map.lookup key entries of
      Just (Access.Object value) -> value
      _ -> error "test fixture did not select the complete environment wiring"
    edges = [object (Access.environmentEdgeKey role direction) | role <- Access.allApplicationPredefinedSortRoles, direction <- Access.allEnvironmentEdgeRoles]

-- | These synthetic application-owner fixtures have no semantic graph owner.
-- Give their complete interface consistent spokes rather than invented aliases.
fixtureEnvironmentHub :: GlobalObjectId
fixtureEnvironmentHub = fixtureObject 0

fixtureEnvironmentEdges :: [Application.ApplicationRoot] -> [(GlobalObjectId, EdgePayload)]
fixtureEnvironmentEdges roots = zipWith (\ordinal payload -> (fixtureObject ordinal, payload)) [1 .. 18] (concatMap spokes allPredefinedSortRoles)
  where
    hub = NeutralVertex fixtureEnvironmentHub
    spokes role = case ([writer | Application.ApplicationWriterRoot candidate writer _ <- roots, candidate == role], [reader | Application.ApplicationReaderRoot candidate reader <- roots, candidate == role]) of
      ([writer], [reader]) -> [edgePayload (NablaVertex writer) hub Preserve, edgePayload hub (DeltaVertex reader) Preserve, edgePayload (DeltaVertex reader) hub Preserve]
      _ -> error "test environment needs one pair per role"

fixtureObject :: Word8 -> GlobalObjectId
fixtureObject ordinal = checked (mkGlobalObjectId (ByteString.pack ([0xfe, ordinal] <> replicate 30 0xfd)))
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
