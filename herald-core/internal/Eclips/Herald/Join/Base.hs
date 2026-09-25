-- | Derive one checked admission contribution and its Oracle-sealed recipe.
module Eclips.Herald.Join.Base (checkedJoinBase) where

import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Graph (VertexId (..))
import Eclips.Domain.Identity
import Eclips.Domain.Topology
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Join.Bootstrap
import Eclips.Herald.Placement qualified as Placement
import Eclips.Herald.Placement.State qualified as PlacementState
import Eclips.Herald.Startup.State
import Eclips.Herald.Store.State qualified as Store
import Eclips.Oracle.Admission

checkedJoinBase :: HeraldAdmissionRecord -> HeraldState -> Either String (HeraldBootstrapContribution, HeraldJoinBaseRecipe)
checkedJoinBase record state = do
  seal <- maybe (Left "admission has no seal") Right (admissionRecordSeal record)
  contribution <- checked (heraldBootstrapContribution system (admissionRecordId record) applicant objects incarnations)
  recipe <- checked (heraldJoinBaseRecipe (admissionRecordId record) applicant (admissionRecordPredecessor record) (joinSealTopologyCut seal) (contributionDigest contribution))
  unless
    ( heraldJoinDigestBytes (joinSealContributionDigest seal) == topologyOccurrenceDigestBytes (contributionDigest contribution)
        && heraldJoinDigestBytes (joinSealRecipeDigest seal) == heraldJoinBaseRecipeDigestBytes recipe
    )
    (Left "admission recipe differs from sealed contribution")
  pure (contribution, recipe)
  where
    system = Genesis.checkedSystemId (startupGenesis state)
    applicant = admissionManifestHeraldEpoch (admissionRecordManifest record)
    objects =
      Set.fromList
        ( map vertexObject (Graph.graphVertices (startupGraphState state))
            <> Map.keys (Graph.graphStructuralVertexProjections (startupGraphState state))
            <> Map.keys (Graph.graphStructuralEdgeProjections (startupGraphState state))
            <> map Controlled.controlledRecordObjectId (Controlled.controlledLocalRecords (startupControlledState state))
            <> [globalObjectIdFromDeltaId (Store.storeSlotDelta slot) | slot <- Store.retainedStoreSlots (startupStoreState state), not (pendingLocalSlot slot)]
        )
    incarnations =
      Set.fromList
        ( [Store.storeSlotIncarnation slot | slot <- Store.retainedStoreSlots (startupStoreState state), not (pendingLocalSlot slot)]
            <> [Placement.deltaRouteStoreIncarnation route | owner <- PlacementState.remotePlacementOwners placement, route <- PlacementState.remotePlacementRoutes owner placement]
        )
    placement = startupPlacementState state
    pendingLocalSlot slot = case Store.storeSlotProvenance slot of
      Store.HeraldSystemView owner _ -> owner == applicant && Genesis.checkedLocalHeraldEpoch (startupGenesis state) == applicant && Genesis.checkedStartupAdmission (startupGenesis state) /= Nothing
      _ -> False
    vertexObject (NablaVertex ident) = globalObjectIdFromNablaId ident
    vertexObject (DeltaVertex ident) = globalObjectIdFromDeltaId ident
    vertexObject (NeutralVertex ident) = ident
    checked :: (Show problem) => Either problem value -> Either String value
    checked = either (Left . show) Right
