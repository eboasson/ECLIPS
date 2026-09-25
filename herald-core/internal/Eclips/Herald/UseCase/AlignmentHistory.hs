-- | Checked replay of explicit historical plans and immutable class evidence.
-- An applicant is a passive observer; old members keep their ordinary admission.
module Eclips.Herald.UseCase.AlignmentHistory
  ( advanceAlignmentHistory,
    adoptAlignmentHistoryCoverage,
    alignmentHistoryInstalled,
    alignmentHistoryFingerprint,
  ) where

import Control.Monad (foldM, unless)
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
import Eclips.Domain.Identity
import Eclips.Herald.Alignment.Generation qualified as Generation
import Eclips.Herald.Alignment.History
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Startup.State
import Eclips.Herald.Structural.Debt
import Eclips.Herald.UseCase.Alignment qualified as Coordinator

-- Plan dependency ordering is explicit. A mixed-age or empty plan can advance
-- the catalogue without creating a generation, so its count enters the cursor.
advanceAlignmentHistory :: AlignmentHistory -> HeraldState -> Either String (HeraldState, EffectBatch)
advanceAlignmentHistory history initial = loop initial mempty
  where
    loop state emitted = do
      (planned, planEffects) <- foldM installPlan (state, mempty) (alignmentHistoryPlans history)
      ready <- foldM installReady planned (alignmentHistoryReadiness history)
      certified <- foldM installCertificate ready (alignmentHistoryCertificates history)
      let effects = emitted <> planEffects
      if alignmentHistoryFingerprint certified == alignmentHistoryFingerprint state then pure (certified, effects) else loop certified effects
    installPlan (state, effects) announce = case Alignment.lookupAlignmentPlan identifier (startupAlignmentState state) of
      Just retained -> do
        _ <- shape (Plan.checkAlignmentPlanAnnouncement retained announce)
        pure (state, effects)
      Nothing
        | not (planDependenciesReady state announce) -> pure (state, effects)
        | local state `elem` map fst (NE.toList (physicalPlacementRevisionEntries placement)) -> do
            let remote = minimum (map fst (NE.toList (physicalPlacementRevisionEntries placement)))
            (admitted, emitted) <- shape (Coordinator.applyHistoricalAlignmentPlan remote announce state)
            pure (admitted, effects <> emitted)
        | otherwise -> do
            plan <- shape (Coordinator.replayAnnouncedAlignmentPlan state announce)
            alignment <- shape (Alignment.retainHistoricalAlignmentRoutingPlan (local state) plan (startupAlignmentState state))
            pure (replaceStartupAlignmentState alignment state, effects)
      where
        identifier = Protocol.alignmentPlanAnnounceId announce
        placement = Plan.alignmentPlanIdPlacement identifier
    installReady state ready
      | isJust (Alignment.lookupAlignmentGeneration (Protocol.classMemberReadyGeneration ready) (startupAlignmentState state)) = do
          prepared <- shape (Alignment.prepareClassMemberReady ready (startupAlignmentState state))
          pure (replaceStartupAlignmentState (fst (Alignment.commitClassMemberReady prepared)) state)
      | otherwise = pure state
    installCertificate state certificate = case Alignment.lookupAlignmentGeneration (Protocol.historicalCertificateClassGeneration certificate) alignment of
      Just generation
        | all (\member -> isJust (Alignment.lookupAlignmentMemberReadiness (Generation.alignmentGenerationId generation) (alignmentMemberStoreIncarnation member) alignment)) (NE.toList (alignmentCutExactMembers (Generation.alignmentGenerationCut generation))) -> do
            prepared <- shape (Alignment.prepareHistoricalCertificate certificate alignment)
            pure (replaceStartupAlignmentState (fst (Alignment.commitHistoricalCertificate prepared)) state)
      _ -> pure state
      where
        alignment = startupAlignmentState state

alignmentHistoryInstalled :: AlignmentHistory -> HeraldState -> Bool
alignmentHistoryInstalled history state =
  all (completePlanInstalled state) requiredPlans
    && all (\(key, _) -> local state `elem` map fst (NE.toList (physicalPlacementRevisionEntries (Alignment.alignmentPromotionKeyPlacementVector key))) || coverageInputsPresent state key) (alignmentHistoryCoverage history)
    && all (\ready -> not (required (Protocol.classMemberReadyGeneration ready)) || Alignment.lookupAlignmentMemberReadiness (Protocol.classMemberReadyGeneration ready) (Protocol.classMemberReadyStoreIncarnation ready) alignment == Just ready) (alignmentHistoryReadiness history)
    && all (\certificate -> not (required (Protocol.historicalCertificateClassGeneration certificate)) || Alignment.lookupAlignmentHistoricalCertificate (Protocol.historicalCertificateClassGeneration certificate) (Protocol.historicalCertificateSourceStoreIncarnation certificate) alignment == Just certificate) (alignmentHistoryCertificates history)
  where
    alignment = startupAlignmentState state
    -- Old owners may already have superseded an unreceived historical plan.
    -- Importing a bundle does not manufacture their obsolete local obligations.
    requiredPlans = [plan | plan <- alignmentHistoryPlans history, local state `notElem` fmap fst (physicalPlacementRevisionEntries (Plan.alignmentPlanIdPlacement (Protocol.alignmentPlanAnnounceId plan)))]
    requiredIds = Set.fromList [Protocol.alignmentPlanBindingClaimGeneration binding | plan <- requiredPlans, binding <- Protocol.alignmentPlanAnnounceBindings plan]
    required identifier = Set.member identifier requiredIds

-- Only canonical sealed-donor activation adopts coverage; passive catalogue
-- arrival neither chooses a frontier nor suppresses current work.
adoptAlignmentHistoryCoverage :: AlignmentHistory -> HeraldState -> Either String HeraldState
adoptAlignmentHistoryCoverage history initial = foldM adopt initial (alignmentHistoryCoverage history)
  where
    adopt state (key, identifiers) = do
      unless (coverageInputsPresent state key) (Left "historical cause coverage lacks its exact installed inputs")
      plan <- maybe (Left "historical coverage plan is absent") Right (Alignment.lookupAlignmentPlan (coveragePlanId key) (startupAlignmentState state))
      unless (sort (map (Generation.alignmentGenerationId . Plan.alignmentPlanBindingGeneration . snd) (Plan.alignmentPlanBindings plan)) == identifiers) (Left "historical cause coverage has a different complete plan")
      alignment <- shape (Alignment.retainHistoricalAlignmentPlanCoverage (local state) key plan (startupAlignmentState state))
      pure (replaceStartupAlignmentState alignment state)

coveragePlanId :: Alignment.AlignmentPromotionKey -> Plan.AlignmentPlanId
coveragePlanId = Alignment.alignmentPromotionKeyPlanId

coverageInputsPresent :: HeraldState -> Alignment.AlignmentPromotionKey -> Bool
coverageInputsPresent state key =
  Progress.installedCutCoversCause (Alignment.alignmentPromotionKeyTopologyCut key) (Alignment.alignmentPromotionKeyCause key) (startupStructuralProgressState state)
    && either (const False) (const True) (Placement.placementRoutesAtVector (Alignment.alignmentPromotionKeyPlacementVector key) (startupPlacementState state))

alignmentHistoryFingerprint :: HeraldState -> (Int, Int, Int, Int, [(SortOccurrence, Plan.AlignmentPlanId)])
alignmentHistoryFingerprint state =
  ( length (Alignment.alignmentPlanEntries alignment),
    length (Alignment.alignmentGenerationEntries alignment),
    length (Alignment.alignmentMemberReadinessEntries alignment),
    length (Alignment.alignmentHistoricalCertificateEntries alignment),
    Alignment.historicalAlignmentPlanFrontier alignment
  )
  where
    alignment = startupAlignmentState state

completePlanInstalled :: HeraldState -> Protocol.AlignmentPlanAnnounce -> Bool
completePlanInstalled state announce = case Alignment.lookupAlignmentPlan (Protocol.alignmentPlanAnnounceId announce) (startupAlignmentState state) of
  Just retained -> Plan.alignmentPlanAnnouncement retained == announce
  Nothing -> False

planDependenciesReady :: HeraldState -> Protocol.AlignmentPlanAnnounce -> Bool
planDependenciesReady state announce =
  isJust (Progress.lookupInstalledTopologyCut (Plan.alignmentPlanIdTopology identifier) (startupStructuralProgressState state))
    && either (const False) (const True) (Placement.placementRoutesAtVector (Plan.alignmentPlanIdPlacement identifier) (startupPlacementState state))
    && maybe True (isJust . (`Alignment.lookupAlignmentPlan` alignment)) (Protocol.alignmentPlanAnnouncePredecessor announce)
    && all (\generation -> isJust (Alignment.lookupAlignmentGeneration generation alignment) && generation `Set.member` certificates) predecessors
  where
    identifier = Protocol.alignmentPlanAnnounceId announce
    alignment = startupAlignmentState state
    predecessors = concatMap (alignmentCutPredecessorGenerationIds . Protocol.alignmentCutAnnounceCut) (Protocol.alignmentPlanAnnounceCreatedCuts announce)
    certificates = Set.fromList [generation | ((generation, _), _) <- Alignment.alignmentHistoricalCertificateEntries alignment]

local :: HeraldState -> HeraldEpoch
local = Genesis.checkedLocalHeraldEpoch . startupGenesis

shape :: (Show problem) => Either problem value -> Either String value
shape = either (Left . show) Right
