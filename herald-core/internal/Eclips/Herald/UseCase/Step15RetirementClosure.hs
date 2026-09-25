{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Package-private completion of the deterministic Step-15 survivor path.
--
-- The live Oracle and peer protocol sums deliberately cannot reach this seam
-- yet.  It composes an already checked membership contraction with an
-- established terminal-source transcript, installs the successor structural
-- base, consumes the transition's exact retired-placement loss receipts, and
-- drains the label, publication, structural, and alignment work which that
-- base enables.
module Eclips.Herald.UseCase.Step15RetirementClosure
  ( RetirementClosureProblem (..),
    RetirementClosureDisposition (..),
    RetirementClosureReceipt,
    retirementClosureDisposition,
    retirementClosureRetiredHerald,
    retirementClosureLostStores,
    retirementClosureSettledAssignments,
    retirementClosureAlignmentLossPlans,
    retirementClosureReleasedPublications,
    retirementClosureSuccessorBase,
    PreparedRetirementClosure,
    prepareRetirementClosure,
    preparedRetirementClosureReceipt,
    preparedRetirementClosureEffects,
    commitRetirementClosure,
  )
where

import Control.Monad (unless)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( HeraldEpoch,
    PublicationId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipLineageGenerations,
  )
import Eclips.Herald.Alignment.Loss qualified as AlignmentLoss
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    emptyEffectBatch,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.TerminalSource
  ( SuccessorStructuralBase,
    successorStructuralBaseCutId,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerStream
  ( AssignmentReceipt,
  )
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault,
    validateHeraldState,
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupStructuralBaseCoordinator,
    replaceStartupStructuralProgressState,
    startupGenesis,
    startupOracleProjectionState,
    startupStructuralProgressState,
  )
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ApplicationCall qualified as ApplicationCall
import Eclips.Herald.UseCase.OracleAdvance qualified as OracleAdvance
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator

data RetirementClosureProblem
  = RetirementClosureLocalHeraldRetired HeraldEpoch
  | RetirementClosureRetiredPlacementMissing HeraldEpoch
  | RetirementClosureRetiredPlacementOwnerMismatch HeraldEpoch HeraldEpoch
  | RetirementClosurePredecessorMembershipMismatch
  | RetirementClosureSuccessorMembershipMismatch
  | RetirementClosureRetiredSourceNotInClosure HeraldEpoch
  | RetirementClosureLocalHeraldMismatch HeraldEpoch HeraldEpoch
  | RetirementClosureStructuralBaseProblem StructuralBase.StructuralBaseProblem
  | RetirementClosureStructuralBaseEvidenceMissing
  | RetirementClosureAlignmentTransferProblem
      AlignmentTransfer.AlignmentTransferCoordinatorProblem
  | RetirementClosureApplicationLabelProblem HeraldInvariantFault
  | RetirementClosureStructuralCoordinationProblem
      StructuralCoordinator.StructuralCoordinationProblem
  | RetirementClosureInvariantFault HeraldInvariantFault
  deriving stock (Eq, Show)

data RetirementClosureDisposition
  = RetirementClosureApplied
  | RetirementClosureExactReplay
  deriving stock (Eq, Ord, Show)

data RetirementClosureReceipt = RetirementClosureReceipt
  { disposition :: RetirementClosureDisposition,
    retiredHerald :: HeraldEpoch,
    lostStores :: [AlignmentLoss.QualifiedStoreCoordinate],
    settledAssignments :: [AssignmentReceipt],
    alignmentLossPlans :: [AlignmentLoss.AlignmentLossPlan],
    releasedPublications :: [PublicationId],
    successorBase :: SuccessorStructuralBase
  }
  deriving stock (Eq, Show)

retirementClosureDisposition ::
  RetirementClosureReceipt -> RetirementClosureDisposition
retirementClosureDisposition receipt = receipt.disposition

retirementClosureRetiredHerald :: RetirementClosureReceipt -> HeraldEpoch
retirementClosureRetiredHerald receipt = receipt.retiredHerald

retirementClosureLostStores ::
  RetirementClosureReceipt -> [AlignmentLoss.QualifiedStoreCoordinate]
retirementClosureLostStores receipt = receipt.lostStores

retirementClosureSettledAssignments ::
  RetirementClosureReceipt -> [AssignmentReceipt]
retirementClosureSettledAssignments receipt = receipt.settledAssignments

retirementClosureAlignmentLossPlans ::
  RetirementClosureReceipt -> [AlignmentLoss.AlignmentLossPlan]
retirementClosureAlignmentLossPlans receipt = receipt.alignmentLossPlans

retirementClosureReleasedPublications ::
  RetirementClosureReceipt -> [PublicationId]
retirementClosureReleasedPublications receipt = receipt.releasedPublications

retirementClosureSuccessorBase ::
  RetirementClosureReceipt -> SuccessorStructuralBase
retirementClosureSuccessorBase receipt = receipt.successorBase

data PreparedRetirementClosure = PreparedRetirementClosure
  { successor :: HeraldState,
    receipt :: RetirementClosureReceipt,
    effects :: EffectBatch
  }

-- | Complete one fresh surviving membership advance, or recognize an exact
-- replay after its successor base and all closure work have already committed.
--
-- The established structural coordinator is opaque evidence tied to the exact
-- predecessor/successor membership pair.  The exact retired placement and
-- peer-stream settlement receipts likewise come only from the prepared
-- membership transition; callers cannot nominate loss coordinates.
prepareRetirementClosure ::
  HeraldState ->
  HeraldState ->
  MembershipAdvance.PreparedMembershipAdvance ->
  StructuralBase.MembershipBaseClosure ->
  Either RetirementClosureProblem PreparedRetirementClosure
prepareRetirementClosure membershipPredecessor current prepared structuralBase =
  case MembershipAdvance.preparedMembershipAdvanceDisposition prepared of
    MembershipAdvance.MembershipAdvanceApplied retired True ->
      Left (RetirementClosureLocalHeraldRetired retired)
    MembershipAdvance.MembershipAdvanceApplied retired False ->
      prepareFresh retired
    MembershipAdvance.MembershipAdvanceExactDuplicate ->
      prepareReplay
  where
    prepareFresh retired = do
      validateFreshCoordinates
        retired
        membershipPredecessor
        preparedSuccessor
        current
        structuralBase
      retiredPlacement <-
        maybe
          (Left (RetirementClosureRetiredPlacementMissing retired))
          Right
          (MembershipAdvance.preparedMembershipAdvanceRetiredPlacement prepared)
      let placementOwner = Placement.retiredRemotePlacementOwner retiredPlacement
      unless
        (placementOwner == retired)
        (Left (RetirementClosureRetiredPlacementOwnerMismatch retired placementOwner))
      let lostStores =
            MembershipAdvance.preparedMembershipAdvanceLostStores prepared
          lossPlans =
            MembershipAdvance.preparedMembershipAdvanceAlignmentLossPlans prepared
      tailResult <- prepareClosureTail structuralBase current
      let successor = closureTailSuccessor tailResult
          base = closureTailSuccessorBase tailResult
      mapLeft RetirementClosureInvariantFault (validateHeraldState successor)
      let receipt =
            RetirementClosureReceipt
              { disposition = RetirementClosureApplied,
                retiredHerald = retired,
                lostStores,
                settledAssignments =
                  MembershipAdvance.preparedMembershipAdvanceSettledAssignments prepared,
                alignmentLossPlans = lossPlans,
                releasedPublications =
                  closureTailReleasedPublications tailResult,
                successorBase = base
              }
      Right
        PreparedRetirementClosure
          { successor,
            receipt,
            effects = closureTailEffects tailResult
          }
      where
        preparedSuccessor = MembershipAdvance.commitMembershipAdvance prepared

    prepareReplay = do
      retired <-
        maybe
          (Left RetirementClosureSuccessorMembershipMismatch)
          Right
          (heraldMembershipGenerationRetiredHeraldEpoch (StructuralBase.structuralBaseSuccessorMembership structuralBase))
      validateReplayCoordinates
        retired
        membershipPredecessor
        current
        structuralBase
      tailResult <- prepareClosureTail structuralBase current
      let successorState = closureTailSuccessor tailResult
          exactReplay = successorState == current
          disposition =
            if exactReplay
              then RetirementClosureExactReplay
              else RetirementClosureApplied
          effects =
            if exactReplay
              then emptyEffectBatch
              else closureTailEffects tailResult
      mapLeft RetirementClosureInvariantFault (validateHeraldState successorState)
      Right
        PreparedRetirementClosure
          { successor = successorState,
            receipt =
              RetirementClosureReceipt
                { disposition,
                  retiredHerald = retired,
                  lostStores = [],
                  settledAssignments = [],
                  alignmentLossPlans = [],
                  releasedPublications =
                    closureTailReleasedPublications tailResult,
                  successorBase = closureTailSuccessorBase tailResult
                },
            effects
          }

preparedRetirementClosureReceipt ::
  PreparedRetirementClosure -> RetirementClosureReceipt
preparedRetirementClosureReceipt prepared = prepared.receipt

preparedRetirementClosureEffects :: PreparedRetirementClosure -> EffectBatch
preparedRetirementClosureEffects prepared = prepared.effects

commitRetirementClosure :: PreparedRetirementClosure -> HeraldState
commitRetirementClosure prepared = prepared.successor

validateFreshCoordinates ::
  HeraldEpoch ->
  HeraldState ->
  HeraldState ->
  HeraldState ->
  StructuralBase.MembershipBaseClosure ->
  Either RetirementClosureProblem ()
validateFreshCoordinates
  retired
  predecessor
  preparedSuccessor
  current
  structuralBase = do
    let predecessorMembership = currentMembership predecessor
        successorMembership = currentMembership preparedSuccessor
    unless
      (predecessorMembership `elem` NonEmpty.toList (heraldMembershipLineageGenerations (StructuralBase.structuralBaseLineage structuralBase)))
      (Left RetirementClosurePredecessorMembershipMismatch)
    unless
      ( StructuralBase.structuralBaseSuccessorMembership structuralBase
          == successorMembership
      )
      (Left RetirementClosureSuccessorMembershipMismatch)
    unless
      ( StructuralBase.structuralBaseSuccessorMembership structuralBase
          == currentMembership current
      )
      (Left RetirementClosureSuccessorMembershipMismatch)
    validateCommonCoordinates retired predecessor structuralBase
    validateCommonCoordinates retired current structuralBase

validateReplayCoordinates ::
  HeraldEpoch ->
  HeraldState ->
  HeraldState ->
  StructuralBase.MembershipBaseClosure ->
  Either RetirementClosureProblem ()
validateReplayCoordinates retired predecessor successor structuralBase = do
  let successorView =
        OracleProjection.oracleView (startupOracleProjectionState successor)
      successorMembership =
        OracleProjection.oracleViewCurrentHeraldMembership successorView
      predecessorMembership =
        StructuralBase.structuralBasePredecessorMembership structuralBase
  unless
    (predecessorMembership == currentMembership predecessor)
    (Left RetirementClosurePredecessorMembershipMismatch)
  unless
    ( StructuralBase.structuralBaseSuccessorMembership structuralBase
        == successorMembership
    )
    (Left RetirementClosureSuccessorMembershipMismatch)
  unless
    (predecessorMembership `elem` OracleProjection.oracleViewHeraldMembershipHistory successorView)
    (Left RetirementClosurePredecessorMembershipMismatch)
  validateCommonCoordinates retired predecessor structuralBase
  validateCommonCoordinates retired successor structuralBase

validateCommonCoordinates ::
  HeraldEpoch ->
  HeraldState ->
  StructuralBase.MembershipBaseClosure ->
  Either RetirementClosureProblem ()
validateCommonCoordinates retired state structuralBase = do
  let local = checkedLocalHeraldEpoch (startupGenesis state)
      structuralLocal = StructuralBase.structuralBaseLocalHerald structuralBase
      structuralRetired = StructuralBase.structuralBaseRetiredSources structuralBase
  unless
    (Set.member retired structuralRetired)
    (Left (RetirementClosureRetiredSourceNotInClosure retired))
  unless
    (structuralLocal == local)
    (Left (RetirementClosureLocalHeraldMismatch local structuralLocal))

currentMembership :: HeraldState -> HeraldMembershipGeneration
currentMembership =
  OracleProjection.oracleViewCurrentHeraldMembership
    . OracleProjection.oracleView
    . startupOracleProjectionState

data ClosureTail = ClosureTail
  { successor :: HeraldState,
    successorBase :: SuccessorStructuralBase,
    releasedPublications :: [PublicationId],
    effects :: EffectBatch
  }

closureTailSuccessor :: ClosureTail -> HeraldState
closureTailSuccessor result = result.successor

closureTailSuccessorBase :: ClosureTail -> SuccessorStructuralBase
closureTailSuccessorBase result = result.successorBase

closureTailReleasedPublications :: ClosureTail -> [PublicationId]
closureTailReleasedPublications result = result.releasedPublications

closureTailEffects :: ClosureTail -> EffectBatch
closureTailEffects result = result.effects

-- | Idempotent recovery tail after the membership owner has committed. It may
-- start before the base exists, after base-only installation, or after the
-- entire closure. The caller suppresses level-triggered reoffers only when the
-- speculative successor proves that every durable owner was already inert.
prepareClosureTail ::
  StructuralBase.MembershipBaseClosure ->
  HeraldState ->
  Either RetirementClosureProblem ClosureTail
prepareClosureTail structuralBase current = do
  expectedCut <-
    mapLeft
      RetirementClosureStructuralBaseProblem
      (StructuralBase.expectedSuccessorStructuralCut structuralBase)
  projectedCoordinator <-
    mapLeft
      RetirementClosureStructuralBaseProblem
      ( StructuralBase.recordInstalledSuccessorStructuralCut
          expectedCut
          structuralBase
      )
  base <-
    maybe
      (Left RetirementClosureStructuralBaseEvidenceMissing)
      Right
      (StructuralBase.structuralBaseEvidence projectedCoordinator)
  installedProgress <-
    let progress = startupStructuralProgressState current
     in if GraphProgress.structuralSuccessorBaseInstalled base progress
          then Right progress
          else
            fst
              <$> mapLeft
                RetirementClosureStructuralBaseProblem
                ( StructuralBase.installSuccessorStructuralBase
                    progress
                    structuralBase
                )
  let withBase =
        replaceStartupStructuralBaseCoordinator (Just projectedCoordinator)
          . replaceStartupStructuralProgressState installedProgress
          $ current
  (withSettledBase, baseSettlementEffects) <-
    mapLeft
      RetirementClosureStructuralCoordinationProblem
      ( StructuralCoordinator.settleInstalledStructuralCuts
          [successorStructuralBaseCutId base]
          withBase
      )
  (withBaseWork, releasedPublications, baseWorkEffects) <-
    mapLeft
      RetirementClosureStructuralCoordinationProblem
      ( StructuralCoordinator.advanceSuccessorStructuralBaseWork
          base
          withSettledBase
      )
  (successor, fixedPointEffects) <- advanceRetirementClosureWork withBaseWork
  Right
    ClosureTail
      { successor,
        successorBase = base,
        releasedPublications,
        effects =
          baseSettlementEffects
            <> baseWorkEffects
            <> fixedPointEffects
      }

advanceRetirementClosureWork ::
  HeraldState -> Either RetirementClosureProblem (HeraldState, EffectBatch)
advanceRetirementClosureWork initial = fixedPoint initial emptyEffectBatch
  where
    fixedPoint predecessor effects = do
      (afterLabels, labelEffects, _) <-
        mapLeft
          RetirementClosureApplicationLabelProblem
          (ApplicationCall.advanceApplicationLabelWork predecessor)
      (afterStructural, structuralEffects) <-
        mapLeft
          RetirementClosureStructuralCoordinationProblem
          (StructuralCoordinator.advanceLocalStructuralWork afterLabels)
      (afterAlignment, alignmentEffects) <-
        mapLeft
          RetirementClosureAlignmentTransferProblem
          (AlignmentTransfer.advanceAlignmentTransfers afterStructural)
      (afterRoutes, routeCutoverEffects) <-
        mapLeft
          RetirementClosureStructuralCoordinationProblem
          ( StructuralCoordinator.releasePendingAlignmentRouteCutovers
              afterAlignment
          )
      (successor, sourceLabelEffects) <-
        mapLeft
          RetirementClosureApplicationLabelProblem
          (OracleAdvance.advanceLocalLabelWork afterRoutes)
      let accumulated =
            effects
              <> labelEffects
              <> structuralEffects
              <> alignmentEffects
              <> routeCutoverEffects
              <> sourceLabelEffects
      if AlignmentTransfer.sameAlignmentTransferWorkState successor predecessor
        then Right (successor, accumulated)
        else fixedPoint successor accumulated

mapLeft :: (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert = either (Left . convert) Right
