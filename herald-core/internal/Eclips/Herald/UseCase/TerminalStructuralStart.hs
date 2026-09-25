{-# LANGUAGE OverloadedRecordDot #-}

-- | Start or extend the one retained closure after an authoritative retirement.
-- Its anchor is an actually installed established cut, even when several newer
-- membership generations have not yet established a structural base.
module Eclips.Herald.UseCase.TerminalStructuralStart
  ( TerminalStructuralStartProblem (..),
    beginAfterAppliedMembershipAdvance,
  )
where

import Control.Monad (unless)
import Eclips.Domain.Identity (HeraldEpoch, TopologyCutId)
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipHistoryGenesis,
    heraldMembershipLineageOrigin,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.TerminalSource
  ( MembershipBaseClosure (..),
    TerminalSourceProblem,
    TerminalStructuralOccurrence,
    beginTerminalStructuralCoordinator,
    extendTerminalStructuralCoordinator,
    successorStructuralBaseEstablishedUnion,
    terminalSourceGenesisPredecessorBase,
    terminalSourceInstalledPredecessorBase,
    terminalSourcePayloadEntries,
    terminalSourcePredecessorBaseCutId,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Startup.State
  ( HeraldState,
    startupOracleProjectionState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
  )

data TerminalStructuralStartProblem
  = TerminalStructuralStartSuccessorPredecessorMismatch HeraldMembershipGenerationId (Maybe HeraldMembershipGenerationId)
  | TerminalStructuralStartSuccessorHasNoRetiredSource
  | TerminalStructuralStartRetiredLocal HeraldEpoch
  | TerminalStructuralStartPredecessorCutMissing TopologyCutId
  | TerminalStructuralStartLineageMissing HeraldMembershipGenerationId HeraldMembershipGenerationId
  | TerminalStructuralStartTerminalProblem TerminalSourceProblem
  deriving stock (Eq, Show)

beginAfterAppliedMembershipAdvance :: [TerminalStructuralOccurrence] -> HeraldState -> HeraldState -> Either TerminalStructuralStartProblem MembershipBaseClosure
beginAfterAppliedMembershipAdvance retained predecessor successor = do
  let before = OracleProjection.oracleView (startupOracleProjectionState predecessor)
      after = OracleProjection.oracleView (startupOracleProjectionState successor)
      previousMembership = OracleProjection.oracleViewCurrentHeraldMembership before
      target = OracleProjection.oracleViewCurrentHeraldMembership after
      local = OracleProjection.oracleViewLocalHeraldEpoch before
      previousId = heraldMembershipGenerationId previousMembership
      targetId = heraldMembershipGenerationId target
      progress = startupStructuralProgressState predecessor
      anchorId = GraphProgress.structuralProgressMembershipGenerationId progress
      cutId = GraphProgress.structuralLastInstalledCutId progress
      chain = GraphProgress.structuralInstalledEstablishedChain progress
      bases = fmap successorStructuralBaseEstablishedUnion (GraphProgress.structuralInstalledSuccessorBases progress)
  unless
    (heraldMembershipGenerationPredecessor target == Just previousId)
    (Left (TerminalStructuralStartSuccessorPredecessorMismatch previousId (heraldMembershipGenerationPredecessor target)))
  retired <- maybe (Left TerminalStructuralStartSuccessorHasNoRetiredSource) Right (heraldMembershipGenerationRetiredHeraldEpoch target)
  unless (retired /= local) (Left (TerminalStructuralStartRetiredLocal retired))
  lineage <- maybe (Left (TerminalStructuralStartLineageMissing anchorId targetId)) Right (OracleProjection.oracleViewHeraldMembershipLineage anchorId targetId after)
  let genesisId = heraldMembershipGenerationId (heraldMembershipHistoryGenesis (OracleProjection.oracleViewHeraldMembershipHistoryChecked after))
  historyLineage <- maybe (Left (TerminalStructuralStartLineageMissing genesisId targetId)) Right (OracleProjection.oracleViewHeraldMembershipLineage genesisId targetId after)
  let anchor = heraldMembershipLineageOrigin lineage
  base <-
    if cutId == GraphProgress.structuralGenesisCutId progress
      then mapTerminalProblem (terminalSourceGenesisPredecessorBase anchor cutId)
      else case GraphProgress.lookupInstalledTopologyCut cutId progress of
        Nothing -> Left (TerminalStructuralStartPredecessorCutMissing cutId)
        Just installed -> mapTerminalProblem (terminalSourceInstalledPredecessorBase anchor (GraphProgress.installedTopologyCutCut installed))
  case startupStructuralBaseCoordinator predecessor of
    Just incumbent
      | terminalSourcePredecessorBaseCutId incumbent.predecessorBase == cutId ->
          mapTerminalProblem (extendTerminalStructuralCoordinator historyLineage lineage chain bases retained incumbent)
    incumbent -> do
      let oldPayloads = maybe [] (fmap snd . terminalSourcePayloadEntries . (.payloads)) incumbent
      mapTerminalProblem (beginTerminalStructuralCoordinator historyLineage lineage local base chain bases (oldPayloads <> retained))

mapTerminalProblem :: Either TerminalSourceProblem value -> Either TerminalStructuralStartProblem value
mapTerminalProblem = either (Left . TerminalStructuralStartTerminalProblem) Right
