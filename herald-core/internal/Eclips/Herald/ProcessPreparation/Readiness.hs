-- | Current physical installation and alignment evidence for a required reader.
-- Preparation itself never waits for this predicate: the parent first needs the
-- child identity with which to transfer the endpoint's ordinary label.
module Eclips.Herald.ProcessPreparation.Readiness
  ( requiredDeltaReady,
    PreparedReadiness,
    prepareReadiness,
    preparedRequiredWriterReady,
    preparedRequiredDeltaReady,
    preparationReadinessDependencies,
    preparationInstallationDependencies,
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( alignmentCutExactMembers,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutSortId,
    alignmentMemberDelta,
    alignmentMemberHerald,
    alignmentMemberStoreIncarnation,
  )
import Eclips.Domain.Identity (DeltaId, HeraldEpoch, NablaId, ProcessEpochId, StoreIncarnationId, globalObjectIdFromGlobalUniqueId)
import Eclips.Domain.Label (ReleasedLabelStateView (ReleasedLabelView), releasedLabelStateView)
import Eclips.Domain.ProcessStart (processStartProcessEpochId)
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Herald.Alignment.Generation (alignmentGenerationCut, alignmentGenerationId)
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Controlled.Operate qualified as Operate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.State
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    qualifiedStoreDelta,
    qualifiedStoreHerald,
    sortOccurrence,
    structuralConsequenceDebtKey,
    structuralDebtKeyDestination,
    structuralDebtKeySort,
  )

-- | Prepared once for all incoming preparations. These derived indexes are
-- deliberately lazy: an empty or bootstrap-only preparation does not build
-- graph, debt, or member facts which it never queries. The value is local to
-- the batch and is never installed in HeraldState.
data PreparedReadiness
  = PreparedReadiness
      Operate.PreparedControlledQueries
      Placement.State
      Store.State
      HeraldEpoch
      (Set SortOccurrence, Set (SortOccurrence, DeltaId))
      (Set (SortOccurrence, DeltaId, StoreIncarnationId))

prepareReadiness :: HeraldState -> PreparedReadiness
prepareReadiness state =
  PreparedReadiness
    (Operate.prepareControlledQueries (startupControlledState state) (startupGraphState state) (startupStructuralProgressState state))
    (startupPlacementState state)
    (startupStoreState state)
    local
    debtIndex
    readyMembers
  where
    local = checkedLocalHeraldEpoch (startupGenesis state)
    alignment = startupAlignmentState state
    debtIndex = foldr retainDebt (Set.empty, Set.empty) (Alignment.liveAlignmentStructuralDebtEntries alignment)
    retainDebt debt (sorts, destinations) =
      let key = structuralConsequenceDebtKey debt
          sort = structuralDebtKeySort key
       in case structuralDebtKeyDestination key of
            Nothing -> (Set.insert sort sorts, destinations)
            Just destination
              | qualifiedStoreHerald destination == local ->
                  (sorts, Set.insert (sort, qualifiedStoreDelta destination) destinations)
              | otherwise -> (sorts, destinations)
    readyMembers =
      Set.fromList
        [ (sort, alignmentMemberDelta member, incarnation)
        | (_, generation) <- Alignment.latestAlignmentGenerationEntries alignment,
          let cut = alignmentGenerationCut generation
              sort = sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut),
          member <- NonEmpty.toList (alignmentCutExactMembers cut),
          alignmentMemberHerald member == local,
          let incarnation = alignmentMemberStoreIncarnation member,
          Alignment.currentAlignmentMemberReady local (alignmentGenerationId generation) incarnation alignment
        ]

preparedRequiredWriterReady :: ProcessEpochId -> NablaId -> PreparedReadiness -> Bool
preparedRequiredWriterReady process writer (PreparedReadiness queries _ _ _ _ _) =
  case Operate.checkPreparedControlledOperate process writer queries of
    Right _ -> True
    Left _ -> False

preparedRequiredDeltaReady :: ProcessEpochId -> DeltaId -> PreparedReadiness -> Bool
preparedRequiredDeltaReady process delta (PreparedReadiness queries placements stores local debts readyMembers) =
  case ( Operate.checkPreparedControlledRead process delta queries,
         Placement.lookupLocalPlacement delta placements,
         Store.lookupStoreSlot delta stores
       ) of
    (Right readAuthority, Just placement, Just slot) ->
      let sort = sortOccurrence (Operate.controlledReadSortId readAuthority) (Operate.controlledReadOccurrenceId readAuthority)
          incarnation = Placement.localPlacementStoreIncarnation placement
          currentPlacement =
            Placement.localPlacementProcessEpoch placement == process
              && Placement.localPlacementHeraldEpoch placement == local
              && Operate.controlledReadHeraldEpoch readAuthority == local
              && Placement.localPlacementSortId placement == Operate.controlledReadSortId readAuthority
              && Placement.localPlacementOccurrenceId placement == Operate.controlledReadOccurrenceId readAuthority
              && Store.storeSlotDelta slot == delta
              && Store.storeSlotIncarnation slot == incarnation
              && Store.storeSlotSortId slot == Operate.controlledReadSortId readAuthority
              && Store.storeSlotOccurrenceId slot == Operate.controlledReadOccurrenceId readAuthority
       in currentPlacement
            && Set.notMember sort (fst debts)
            && Set.notMember (sort, delta) (snd debts)
            && Set.member (sort, delta, incarnation) readyMembers
    _ -> False

requiredDeltaReady :: ProcessEpochId -> DeltaId -> HeraldState -> Bool
requiredDeltaReady process delta state =
  case ( Operate.checkControlledRead process delta (startupControlledState state) (startupStructuralProgressState state),
         Placement.lookupLocalPlacement delta (startupPlacementState state),
         Store.lookupStoreSlot delta (startupStoreState state)
       ) of
    (Right readAuthority, Just placement, Just slot) ->
      let sort = sortOccurrence (Operate.controlledReadSortId readAuthority) (Operate.controlledReadOccurrenceId readAuthority)
          incarnation = Placement.localPlacementStoreIncarnation placement
          currentPlacement =
            Placement.localPlacementProcessEpoch placement == process
              && Placement.localPlacementHeraldEpoch placement == local
              && Operate.controlledReadHeraldEpoch readAuthority == local
              && Placement.localPlacementSortId placement == Operate.controlledReadSortId readAuthority
              && Placement.localPlacementOccurrenceId placement == Operate.controlledReadOccurrenceId readAuthority
              && Store.storeSlotDelta slot == delta
              && Store.storeSlotIncarnation slot == incarnation
              && Store.storeSlotSortId slot == Operate.controlledReadSortId readAuthority
              && Store.storeSlotOccurrenceId slot == Operate.controlledReadOccurrenceId readAuthority
          relevantDebt debt =
            let key = structuralConsequenceDebtKey debt
             in structuralDebtKeySort key == sort
                  && maybe True (\destination -> qualifiedStoreDelta destination == delta && qualifiedStoreHerald destination == local) (structuralDebtKeyDestination key)
          memberReady generation =
            any
              ( \member ->
                  alignmentMemberDelta member == delta
                    && alignmentMemberHerald member == local
                    && alignmentMemberStoreIncarnation member == incarnation
              )
              (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))
              && Alignment.currentAlignmentMemberReady local (alignmentGenerationId generation) incarnation alignment
       in currentPlacement
            && not (any relevantDebt (Alignment.liveAlignmentStructuralDebtEntries alignment))
            && any memberReady (Alignment.latestAlignmentGenerationsForSort sort alignment)
    _ -> False
  where
    local = checkedLocalHeraldEpoch (startupGenesis state)
    alignment = startupAlignmentState state

-- | Capture identifiers only when a compact cached answer is evaluated. The
-- current reader placement names the sort whenever physical readiness succeeds;
-- absent/mismatched placement remains subscribed to that Delta and structural
-- progress. No endpoint operation predicate is demanded to construct watches.
preparationReadinessDependencies :: Preparation.Preparation -> HeraldState -> Set Preparation.PreparationDependency
preparationReadinessDependencies preparation state
  | Preparation.preparationPhase preparation == Preparation.Terminal || Preparation.preparationCancelled preparation || Preparation.preparationFailure preparation /= Nothing = Set.empty
  | Preparation.preparationPhase preparation == Preparation.Preparing = base
  | otherwise = base <> Set.singleton Preparation.GateChanged <> Set.unions (map endpoint entries)
  where
    reference = Preparation.preparationReference preparation
    process = processStartProcessEpochId (Preparation.preparationStart preparation)
    grant = Preparation.preparationGrant preparation
    controlled = startupControlledState state
    base = Set.fromList [Preparation.PreparationChanged reference, Preparation.ProcessChanged process, Preparation.ActivityChanged]
    entries = [entry | key <- Set.toAscList (Primordial.primordialGrantRequiredEndpoints grant), Just entry <- [Map.lookup key (Primordial.primordialGrantEntries grant)]]
    endpoint entry =
      let object = globalObjectIdFromGlobalUniqueId (Primordial.globalPrimordialIdentity entry)
          objectKeys = Set.fromList [Preparation.ObjectChanged object, Preparation.PossessionChanged process object, Preparation.StructuralReadinessChanged]
          labelOwners = case Controlled.controlledEffectiveLabelState object controlled of
            Just released -> case releasedLabelStateView released of
              ReleasedLabelView (ProcessLabel owner, _) -> Set.singleton (Preparation.ProcessChanged owner)
              _ -> Set.empty
            Nothing -> Set.empty
          rootKeys root = Set.singleton (Preparation.ProcessChanged (Controlled.rootFactProcessEpoch root))
          specific = case entry of
            Primordial.GlobalWriter writer _ -> maybe Set.empty rootKeys (Controlled.controlledWriterFact writer controlled)
            Primordial.GlobalReader delta _ ->
              Set.singleton (Preparation.DeltaChanged delta)
                <> maybe Set.empty (\root -> rootKeys root <> Set.singleton (Preparation.SortReadinessChanged (sortOccurrence (Controlled.rootFactSortId root) (Controlled.rootFactOccurrenceId root)))) (Controlled.controlledReaderFact delta controlled)
                <> maybe Set.empty (\placement -> Set.singleton (Preparation.SortReadinessChanged (sortOccurrence (Placement.localPlacementSortId placement) (Placement.localPlacementOccurrenceId placement)))) (Placement.lookupLocalPlacement delta (startupPlacementState state))
            _ -> Set.empty
       in objectKeys <> labelOwners <> specific

-- Installation consumes every selected capability, not just required endpoints.
preparationInstallationDependencies :: Preparation.Preparation -> Set Preparation.PreparationDependency
preparationInstallationDependencies preparation =
  Set.fromList
    [ Preparation.ObjectChanged (globalObjectIdFromGlobalUniqueId (Primordial.globalPrimordialIdentity entry))
    | entry <- Map.elems (Primordial.primordialGrantEntries (Preparation.preparationGrant preparation)),
      case entry of Primordial.GlobalIdentity _ -> False; _ -> True
    ]
