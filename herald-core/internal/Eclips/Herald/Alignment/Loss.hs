{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Live coordination after an exact Store placement route disappears.
--
-- Destination and fresh-base loss use the live whole-plan invalidation, which
-- retires ordinary obligations, bootstrap imports, their attempts, and their
-- transfer work together. Selected ordinary and predecessor-bootstrap source
-- loss use the live attempt invalidations. Ordinary Store replacement may
-- immediately ask the same owner to select again with every durably lost source
-- route excluded. Membership retirement instead defers that selection until the
-- successor structural base has been installed and the ordinary alignment fixed
-- point runs. The coordinator is a transaction boundary: all preparations are
-- evaluated from one immutable predecessor and only the final checked successor
-- can be committed.
--
-- Structural causes remain in the composite receipt, while the low-level
-- receipts deliberately retain their protocol reasons.
module Eclips.Herald.Alignment.Loss
  ( QualifiedStoreCoordinate,
    qualifiedStoreCoordinate,
    qualifiedStoreCoordinateHerald,
    qualifiedStoreCoordinateDelta,
    qualifiedStoreCoordinateIncarnation,
    alignmentLossRelevantCauses,
    AlignmentLossCoordinatorState,
    alignmentLossCoordinatorState,
    alignmentLossCoordinatorOwner,
    alignmentLossCoordinatorLostStores,
    alignmentLossCoordinatorRedriveRequired,
    LossAwareSelectionProblem (..),
    prepareLossAwareAlignmentAttempt,
    prepareLossAwareBootstrapImportAttempt,
    AlignmentLossReceipt,
    alignmentLossReceiptPlan,
    alignmentLossReceiptEntries,
    AlignmentLossDisposition (..),
    AlignmentLossPlan,
    alignmentLossPlanCause,
    alignmentLossPlanLostStore,
    alignmentLossPlanInvalidationCauses,
    PlanInvalidationEvent,
    planInvalidationEventCauses,
    planInvalidationEventObligations,
    planInvalidationEventBootstrapImports,
    alignmentLossPlanInvalidationEvents,
    alignmentLossPlanOrdinaryRetirements,
    alignmentLossPlanBootstrapRetirements,
    alignmentLossPlanCancellations,
    AlignmentLossProblem (..),
    PreparedAlignmentLoss,
    prepareAlignmentLoss,
    prepareAlignmentLossAfterMembershipRetirement,
    preparedAlignmentLossPlan,
    preparedAlignmentLossDisposition,
    commitAlignmentLoss,
    AlignmentLossInvariantProblem (..),
    validateAlignmentLossCoordinatorState,
  )
where

import Control.Monad (foldM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( AlignmentMember,
    ContextClassGenerationId,
    FreshMemberBaseEvidence,
    alignmentCutExactMembers,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutSortId,
    alignmentMemberDelta,
    alignmentMemberHerald,
    alignmentMemberStoreIncarnation,
    freshMemberBaseDelta,
    freshMemberBaseStoreIncarnation,
  )
import Eclips.Domain.Identity
  ( DeltaId,
    HeraldEpoch,
    StoreIncarnationId,
  )
import Eclips.Domain.StructuralConsequence (StructuralConsequenceCause)
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    alignmentGenerationCut,
    alignmentGenerationId,
  )
import Eclips.Herald.Alignment.Protocol
  ( AlignmentAttempt,
    AlignmentCancel,
    AlignmentCancelReason
      ( AlignmentDestinationIncarnationLost,
        AlignmentSourceIncarnationLost
      ),
    AlignmentObligation,
    AlignmentObligationId,
    DestinationStore,
    alignmentAttemptObligation,
    alignmentAttemptObligationId,
    alignmentCancel,
    alignmentCancelSubscriptionId,
    alignmentObligationCause,
    alignmentObligationDestinationGeneration,
    alignmentObligationDestinationStores,
    alignmentObligationIdDestinationHerald,
    alignmentObligationIdValue,
    alignmentObligationSourceGeneration,
    alignmentSubscribeObligation,
    alignmentSubscribeObligationId,
    alignmentSubscribeSourceStoreIncarnation,
    destinationStore,
    destinationStoreDelta,
    destinationStoreIncarnation,
  )
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Structural.Debt (SortOccurrence, sortOccurrence)

data QualifiedStoreCoordinate = QualifiedStoreCoordinate
  { herald :: HeraldEpoch,
    delta :: DeltaId,
    incarnation :: StoreIncarnationId
  }
  deriving stock (Eq, Ord, Show)

qualifiedStoreCoordinate ::
  HeraldEpoch -> DeltaId -> StoreIncarnationId -> QualifiedStoreCoordinate
qualifiedStoreCoordinate = QualifiedStoreCoordinate

qualifiedStoreCoordinateHerald :: QualifiedStoreCoordinate -> HeraldEpoch
qualifiedStoreCoordinateHerald coordinate = coordinate.herald

qualifiedStoreCoordinateDelta :: QualifiedStoreCoordinate -> DeltaId
qualifiedStoreCoordinateDelta coordinate = coordinate.delta

qualifiedStoreCoordinateIncarnation ::
  QualifiedStoreCoordinate -> StoreIncarnationId
qualifiedStoreCoordinateIncarnation coordinate = coordinate.incarnation

-- | Every retained structural consequence whose executable or current
-- generation material names one exact physical Store coordinate.  A remote
-- placement update carries no structural cause of its own, so the live peer
-- boundary uses these already checked owner links rather than fabricating one
-- from a placement sequence or control prerequisite.
--
-- An empty result is legitimate when loss is observed before any alignment
-- work exists.  The unavailable-source coordinate is still retained directly
-- in 'Alignment.State' and later selection will honor it.
alignmentLossRelevantCauses ::
  QualifiedStoreCoordinate ->
  Alignment.State ->
  Set StructuralConsequenceCause
alignmentLossRelevantCauses lost owner =
  Set.fromList
    ( fmap alignmentObligationCause relevantObligations
        <> [ Alignment.alignmentPromotionKeyCause key
           | (key, receipt) <- Alignment.alignmentPromotionEntries owner,
             any
               (generationIdentifierUsesLostStore lost owner)
               (Alignment.alignmentPromotionReceiptGenerationIds receipt)
           ]
        -- A joiner retains checked semantic completion without the old
        -- members' promotion receipts or executable work. Its covered causes
        -- must still reach whole-plan invalidation when a member disappears.
        <> [ Alignment.alignmentPromotionKeyCause key
           | (key, identifiers) <- Alignment.alignmentCompletedCauseCoverage owner,
             any (generationIdentifierUsesLostStore lost owner) identifiers
           ]
    )
  where
    relevantObligations =
      filter (obligationUsesLostStore lost owner) retainedObligations
        <> filter
          (obligationUsesLostStore lost owner)
          ( Alignment.bootstrapImportSubscribeEnvelope . snd
              <$> retainedBootstrapImports
          )
        <> [ obligation
           | (_, source) <-
               Transfer.sourceSubscriptionEntries
                 (Alignment.alignmentTransferState owner),
             let obligation =
                   alignmentSubscribeObligation
                     (Transfer.sourceSubscriptionSubscribe source),
             sourceSubscriptionUsesLostDestination lost source
           ]

    retainedObligations =
      fmap snd (Alignment.activeAlignmentObligationEntries owner)
        <> [ obligation
           | (_, receipt) <- Alignment.alignmentPlanInvalidationEntries owner,
             (_, obligation) <-
               Alignment.alignmentPlanInvalidationReceiptObligations receipt
           ]

    retainedBootstrapImports =
      Alignment.bootstrapImportEntries owner
        <> [ retained
           | (_, receipt) <- Alignment.alignmentPlanInvalidationEntries owner,
             retained <-
               Alignment.alignmentPlanInvalidationReceiptBootstrapImports receipt
           ]

obligationUsesLostStore ::
  QualifiedStoreCoordinate ->
  Alignment.State ->
  AlignmentObligation ->
  Bool
obligationUsesLostStore lost owner obligation =
  destinationUsesLostStore || sourceGenerationUsesLostStore
  where
    destinationUsesLostStore =
      alignmentObligationIdDestinationHerald
        (alignmentObligationIdValue obligation)
        == lost.herald
        && any
          (importUsesDestination lost)
          (NonEmpty.toList (alignmentObligationDestinationStores obligation))
    sourceGenerationUsesLostStore =
      generationIdentifierUsesLostStore
        lost
        owner
        (alignmentObligationSourceGeneration obligation)

generationIdentifierUsesLostStore ::
  QualifiedStoreCoordinate ->
  Alignment.State ->
  ContextClassGenerationId ->
  Bool
generationIdentifierUsesLostStore lost owner identifier =
  maybe False (generationUsesLostStore lost) (Alignment.lookupAlignmentGeneration identifier owner)

generationUsesLostStore :: QualifiedStoreCoordinate -> AlignmentGeneration -> Bool
generationUsesLostStore lost generation =
  any memberMatches (generationMembers generation)
  where
    memberMatches member =
      alignmentMemberHerald member == lost.herald
        && alignmentMemberDelta member == lost.delta
        && alignmentMemberStoreIncarnation member == lost.incarnation

data AlignmentLossCoordinatorState = AlignmentLossCoordinatorState
  { localHerald :: HeraldEpoch,
    owner :: Alignment.State,
    lostStores :: Set QualifiedStoreCoordinate,
    receipts ::
      Map
        (StructuralConsequenceCause, QualifiedStoreCoordinate)
        AlignmentLossReceipt,
    redriveRequired ::
      Map Alignment.AlignmentPlanCoordinate (Set StructuralConsequenceCause)
  }
  deriving stock (Eq, Show)

alignmentLossCoordinatorState ::
  HeraldEpoch ->
  Alignment.State ->
  Either AlignmentLossInvariantProblem AlignmentLossCoordinatorState
alignmentLossCoordinatorState localHerald owner = do
  let state =
        AlignmentLossCoordinatorState
          { localHerald,
            owner,
            lostStores = Set.empty,
            receipts = Map.empty,
            redriveRequired = Map.empty
          }
  validateAlignmentLossCoordinatorRelationships state
  Right state

alignmentLossCoordinatorOwner :: AlignmentLossCoordinatorState -> Alignment.State
alignmentLossCoordinatorOwner state = state.owner

alignmentLossCoordinatorLostStores ::
  AlignmentLossCoordinatorState -> Set QualifiedStoreCoordinate
alignmentLossCoordinatorLostStores state = state.lostStores

alignmentLossCoordinatorRedriveRequired ::
  AlignmentLossCoordinatorState ->
  [(Alignment.AlignmentPlanCoordinate, Set StructuralConsequenceCause)]
alignmentLossCoordinatorRedriveRequired state =
  Map.toAscList state.redriveRequired

data LossAwareSelectionProblem
  = LossAwareOrdinarySelectionProblem Alignment.AlignmentAttemptProblem
  | LossAwareOrdinaryTransferProblem Transfer.AlignmentTransferProblem
  | LossAwareBootstrapSelectionProblem Alignment.BootstrapImportProblem
  | LossAwareSelectionInvariant AlignmentLossInvariantProblem
  deriving stock (Eq, Show)

-- | Allocate ordinary work through the coordinator so every observed source
-- loss, including a loss observed before any attempt existed, participates in
-- the live owner's canonical @(Herald, StoreIncarnation)@ selection order.
prepareLossAwareAlignmentAttempt ::
  AlignmentObligationId ->
  AlignmentLossCoordinatorState ->
  Either
    LossAwareSelectionProblem
    (AlignmentLossCoordinatorState, AlignmentAttempt)
prepareLossAwareAlignmentAttempt identifier state = do
  prepared <-
    either
      (Left . LossAwareOrdinarySelectionProblem)
      Right
      ( Alignment.prepareAlignmentAttemptExcluding
          Set.empty
          identifier
          state.owner
      )
  let (withAttempt, _) = Alignment.commitAlignmentAttempt prepared
      attempt = Alignment.preparedAlignmentAttempt prepared
  preparedTransfer <-
    either
      (Left . LossAwareOrdinaryTransferProblem)
      Right
      ( Transfer.prepareDestinationAttempt
          attempt
          (Alignment.alignmentTransferState withAttempt)
      )
  let (successorTransfer, _) = Transfer.commitDestinationAttempt preparedTransfer
      successorOwner =
        Alignment.replaceAlignmentTransferState successorTransfer withAttempt
      successor = state {owner = successorOwner}
  either
    (Left . LossAwareSelectionInvariant)
    (const (Right (successor, attempt)))
    (validateAlignmentLossCoordinatorRelationships successor)

-- | Bootstrap counterpart of 'prepareLossAwareAlignmentAttempt'.
prepareLossAwareBootstrapImportAttempt ::
  Alignment.BootstrapImportKey ->
  AlignmentLossCoordinatorState ->
  Either
    LossAwareSelectionProblem
    (AlignmentLossCoordinatorState, Alignment.BootstrapImportAttempt)
prepareLossAwareBootstrapImportAttempt key state = do
  prepared <-
    either
      (Left . LossAwareBootstrapSelectionProblem)
      Right
      ( Alignment.prepareBootstrapImportAttemptExcluding
          Set.empty
          key
          state.owner
      )
  let (successorOwner, _) = Alignment.commitBootstrapImportAttempt prepared
      successor = state {owner = successorOwner}
  either
    (Left . LossAwareSelectionInvariant)
    (const (Right (successor, Alignment.preparedBootstrapImportAttempt prepared)))
    (validateAlignmentLossCoordinatorRelationships successor)

data OrdinarySourceRetirement = OrdinarySourceRetirement
  { retired :: AlignmentAttempt,
    receipt :: Alignment.AlignmentAttemptInvalidationReceipt,
    replacement :: Maybe AlignmentAttempt
  }
  deriving stock (Eq, Show)

data BootstrapSourceRetirement = BootstrapSourceRetirement
  { retired :: Alignment.BootstrapImportAttempt,
    receipt :: Alignment.BootstrapImportAttemptInvalidationReceipt,
    replacement :: Maybe Alignment.BootstrapImportAttempt
  }
  deriving stock (Eq, Show)

data AlignmentLossPlan = AlignmentLossPlan
  { cause :: StructuralConsequenceCause,
    lostStore :: QualifiedStoreCoordinate,
    planCauses :: Set Alignment.AlignmentPlanInvalidationCause,
    planEvents :: Map Alignment.AlignmentPlanCoordinate PlanInvalidationEvent,
    ordinaryRetirements :: [OrdinarySourceRetirement],
    bootstrapRetirements :: [BootstrapSourceRetirement],
    cancellations :: [AlignmentCancel]
  }
  deriving stock (Eq, Show)

-- | Order-independent projection of this one physical loss into a live plan
-- tombstone. The live owner keeps the accumulated low-level receipt; a
-- structural receipt snapshots only this event's causes plus immutable owner
-- identities, so distinct destination losses commute.
data PlanInvalidationEvent = PlanInvalidationEvent
  { causes :: Set Alignment.AlignmentPlanInvalidationCause,
    obligations :: [(AlignmentObligationId, AlignmentObligation)],
    bootstrapImports ::
      [(Alignment.BootstrapImportKey, Alignment.BootstrapImport)]
  }
  deriving stock (Eq, Show)

planInvalidationEventCauses ::
  PlanInvalidationEvent -> Set Alignment.AlignmentPlanInvalidationCause
planInvalidationEventCauses event = event.causes

planInvalidationEventObligations ::
  PlanInvalidationEvent -> [(AlignmentObligationId, AlignmentObligation)]
planInvalidationEventObligations event = event.obligations

planInvalidationEventBootstrapImports ::
  PlanInvalidationEvent ->
  [(Alignment.BootstrapImportKey, Alignment.BootstrapImport)]
planInvalidationEventBootstrapImports event = event.bootstrapImports

alignmentLossPlanCause :: AlignmentLossPlan -> StructuralConsequenceCause
alignmentLossPlanCause plan = plan.cause

alignmentLossPlanLostStore :: AlignmentLossPlan -> QualifiedStoreCoordinate
alignmentLossPlanLostStore plan = plan.lostStore

alignmentLossPlanInvalidationCauses ::
  AlignmentLossPlan -> Set Alignment.AlignmentPlanInvalidationCause
alignmentLossPlanInvalidationCauses plan = plan.planCauses

alignmentLossPlanInvalidationEvents ::
  AlignmentLossPlan ->
  [(Alignment.AlignmentPlanCoordinate, PlanInvalidationEvent)]
alignmentLossPlanInvalidationEvents plan = Map.toAscList plan.planEvents

alignmentLossPlanOrdinaryRetirements ::
  AlignmentLossPlan ->
  [ ( AlignmentAttempt,
      Alignment.AlignmentAttemptInvalidationReceipt,
      Maybe AlignmentAttempt
    )
  ]
alignmentLossPlanOrdinaryRetirements plan =
  [ (retirement.retired, retirement.receipt, retirement.replacement)
  | retirement <- plan.ordinaryRetirements
  ]

alignmentLossPlanBootstrapRetirements ::
  AlignmentLossPlan ->
  [ ( Alignment.BootstrapImportAttempt,
      Alignment.BootstrapImportAttemptInvalidationReceipt,
      Maybe Alignment.BootstrapImportAttempt
    )
  ]
alignmentLossPlanBootstrapRetirements plan =
  [ (retirement.retired, retirement.receipt, retirement.replacement)
  | retirement <- plan.bootstrapRetirements
  ]

alignmentLossPlanCancellations :: AlignmentLossPlan -> [AlignmentCancel]
alignmentLossPlanCancellations plan = plan.cancellations

newtype AlignmentLossReceipt = AlignmentLossReceipt AlignmentLossPlan
  deriving stock (Eq, Show)

alignmentLossReceiptPlan :: AlignmentLossReceipt -> AlignmentLossPlan
alignmentLossReceiptPlan (AlignmentLossReceipt plan) = plan

alignmentLossReceiptEntries ::
  AlignmentLossCoordinatorState ->
  [ ( (StructuralConsequenceCause, QualifiedStoreCoordinate),
      AlignmentLossReceipt
    )
  ]
alignmentLossReceiptEntries state = Map.toAscList state.receipts

data AlignmentLossDisposition
  = AlignmentLossApplied
  | AlignmentLossUnchanged
  deriving stock (Eq, Show)

data AlignmentLossProblem
  = AlignmentLossPlanInvalidationProblem Alignment.AlignmentPlanInvalidationProblem
  | AlignmentLossBarrierCleanupProblem Transfer.AlignmentTransferProblem
  | AlignmentLossSourceCancellationProblem Transfer.AlignmentTransferProblem
  | AlignmentLossDestinationCancellationProblem Transfer.AlignmentTransferProblem
  | AlignmentLossOrdinaryInvalidationProblem
      Alignment.AlignmentAttemptInvalidationProblem
  | AlignmentLossOrdinaryReselectionProblem Alignment.AlignmentAttemptProblem
  | AlignmentLossOrdinaryReselectionTransferProblem
      Transfer.AlignmentTransferProblem
  | AlignmentLossBootstrapInvalidationProblem
      Alignment.BootstrapImportAttemptInvalidationProblem
  | AlignmentLossBootstrapReselectionProblem Alignment.BootstrapImportProblem
  | AlignmentLossSuccessorInvariant AlignmentLossInvariantProblem
  deriving stock (Eq, Show)

data PreparedAlignmentLoss = PreparedAlignmentLoss
  { successor :: AlignmentLossCoordinatorState,
    plan :: AlignmentLossPlan,
    disposition :: AlignmentLossDisposition
  }
  deriving stock (Eq, Show)

data SourceReselectionPolicy
  = ReselectImmediately
  | DeferUntilSuccessorStructuralBase

-- | Apply an ordinary Store loss. A surviving same-generation source may be
-- selected immediately; this is the path used by local Store replacement.
prepareAlignmentLoss ::
  StructuralConsequenceCause ->
  QualifiedStoreCoordinate ->
  AlignmentLossCoordinatorState ->
  Either AlignmentLossProblem PreparedAlignmentLoss
prepareAlignmentLoss = prepareAlignmentLossWithPolicy ReselectImmediately

-- | Apply Store loss derived from membership retirement. The stable logical
-- obligation or bootstrap import survives, but its exact source-qualified
-- attempt is cancelled without selecting from predecessor evidence. The Step
-- 15 retirement closure installs the successor structural base before running
-- the ordinary alignment generation/transfer fixed point, which then performs
-- any replacement selection from successor-qualified evidence.
prepareAlignmentLossAfterMembershipRetirement ::
  StructuralConsequenceCause ->
  QualifiedStoreCoordinate ->
  AlignmentLossCoordinatorState ->
  Either AlignmentLossProblem PreparedAlignmentLoss
prepareAlignmentLossAfterMembershipRetirement =
  prepareAlignmentLossWithPolicy DeferUntilSuccessorStructuralBase

prepareAlignmentLossWithPolicy ::
  SourceReselectionPolicy ->
  StructuralConsequenceCause ->
  QualifiedStoreCoordinate ->
  AlignmentLossCoordinatorState ->
  Either AlignmentLossProblem PreparedAlignmentLoss
prepareAlignmentLossWithPolicy reselectionPolicy cause lostStore state =
  case Map.lookup receiptKey state.receipts of
    Just retained ->
      Right
        PreparedAlignmentLoss
          { successor = state,
            plan = alignmentLossReceiptPlan retained,
            disposition = AlignmentLossUnchanged
          }
    Nothing -> prepareFirstLoss
  where
    receiptKey = (cause, lostStore)
    allLostStores = Set.insert lostStore state.lostStores
    unavailableSource =
      Alignment.unavailableAlignmentSource
        lostStore.herald
        lostStore.delta
        lostStore.incarnation

    prepareFirstLoss = do
      let (ownerWithUnavailableSource, _) =
            Alignment.commitUnavailableAlignmentSourceRetention
              ( Alignment.prepareUnavailableAlignmentSourceRetention
                  unavailableSource
                  state.owner
              )
          sourceSubscriptions =
            [ (identifier, source)
            | lostStore.herald == state.localHerald,
              (identifier, source) <-
                Transfer.sourceSubscriptionEntries
                  (Alignment.alignmentTransferState ownerWithUnavailableSource),
              alignmentSubscribeSourceStoreIncarnation
                (Transfer.sourceSubscriptionSubscribe source)
                == lostStore.incarnation
            ]
          destinationSubscriptions =
            [ (identifier, source)
            | lostStore.herald /= state.localHerald,
              (identifier, source) <-
                Transfer.sourceSubscriptionEntries
                  (Alignment.alignmentTransferState ownerWithUnavailableSource),
              sourceSubscriptionUsesLostDestination lostStore source
            ]
      (ownerWithSourceLoss, sourceCancellations) <-
        foldM
          applyExactSourceLoss
          (ownerWithUnavailableSource, [])
          sourceSubscriptions
      (ownerWithDestinationLoss, destinationCancellations) <-
        foldM
          applyExactDestinationLoss
          (ownerWithSourceLoss, [])
          destinationSubscriptions
      let planCauses =
            lossPlanCauses state.localHerald lostStore ownerWithDestinationLoss
          frozenFreshBootstrapAttempts =
            [ attempt
            | (_, attempt) <-
                Alignment.bootstrapImportAttemptEntries ownerWithDestinationLoss,
              Alignment.bootstrapImportAttemptUsesUnavailableSource
                unavailableSource
                attempt
                ownerWithDestinationLoss,
              Alignment.BootstrapFromFreshBase _ <-
                [ Alignment.bootstrapImportKeySource
                    ( Alignment.bootstrapImportKeyValue
                        (Alignment.bootstrapImportAttemptImport attempt)
                    )
                ]
            ]
      (afterPlans, accumulatedPlanReceipts, planCancellations) <-
        foldM
          applyPlanInvalidation
          (ownerWithDestinationLoss, Map.empty, [])
          (Set.toAscList planCauses)
      let ordinaryAttempts =
            [ attempt
            | (_, attempt) <- Alignment.alignmentAttemptEntries afterPlans,
              Alignment.alignmentAttemptUsesUnavailableSource
                unavailableSource
                attempt
                afterPlans
            ]
      (afterOrdinary, ordinaryRetirements, ordinaryCancellations) <-
        foldM
          (applyOrdinarySourceLoss reselectionPolicy)
          (afterPlans, [], [])
          ordinaryAttempts
      let bootstrapAttempts =
            frozenFreshBootstrapAttempts
              <> [ attempt
                 | (_, attempt) <- Alignment.bootstrapImportAttemptEntries afterOrdinary,
                   Alignment.bootstrapImportAttemptUsesUnavailableSource
                     unavailableSource
                     attempt
                     afterOrdinary
                 ]
      (afterBootstrap, bootstrapRetirements, bootstrapCancellations) <-
        foldM
          (applyBootstrapSourceLoss reselectionPolicy)
          (afterOrdinary, [], [])
          bootstrapAttempts
      let planEvents =
            Map.map
              ( \receipt ->
                  PlanInvalidationEvent
                    { causes =
                        Set.intersection
                          planCauses
                          (Alignment.alignmentPlanInvalidationReceiptCauses receipt),
                      obligations =
                        Alignment.alignmentPlanInvalidationReceiptObligations receipt,
                      bootstrapImports =
                        Alignment.alignmentPlanInvalidationReceiptBootstrapImports receipt
                    }
              )
              accumulatedPlanReceipts
      let plan =
            AlignmentLossPlan
              { cause,
                lostStore,
                planCauses,
                planEvents,
                ordinaryRetirements,
                bootstrapRetirements,
                cancellations =
                  normalizeCancellations
                    ( sourceCancellations
                        <> destinationCancellations
                        <> planCancellations
                        <> ordinaryCancellations
                        <> bootstrapCancellations
                    )
              }
          successor =
            state
              { owner = afterBootstrap,
                lostStores = allLostStores,
                receipts =
                  Map.insert receiptKey (AlignmentLossReceipt plan) state.receipts,
                redriveRequired =
                  foldr
                    ( \coordinate ->
                        Map.insertWith Set.union coordinate (Set.singleton cause)
                    )
                    state.redriveRequired
                    (Map.keys planEvents)
              }
      either
        (Left . AlignmentLossSuccessorInvariant)
        ( const
            ( Right
                PreparedAlignmentLoss
                  { successor,
                    plan,
                    disposition = AlignmentLossApplied
                  }
            )
        )
        (validateAlignmentLossCoordinatorRelationships successor)

    applyExactSourceLoss (owner, cancellations) (identifier, _) = do
      let cancellation = alignmentCancel identifier AlignmentSourceIncarnationLost
      prepared <-
        either
          (Left . AlignmentLossSourceCancellationProblem)
          Right
          ( Transfer.prepareAlignmentCancellation
              cancellation
              (Alignment.alignmentTransferState owner)
          )
      let (successorTransfer, disposition) =
            Transfer.commitAlignmentCancellation prepared
          successorOwner =
            Alignment.replaceAlignmentTransferState successorTransfer owner
          newlyRetained = case disposition of
            Transfer.AlignmentTransferUnchanged -> []
            _ -> [cancellation]
      Right (successorOwner, cancellations <> newlyRetained)

    applyExactDestinationLoss (owner, cancellations) (identifier, _) = do
      let cancellation =
            alignmentCancel identifier AlignmentDestinationIncarnationLost
      prepared <-
        either
          (Left . AlignmentLossDestinationCancellationProblem)
          Right
          ( Transfer.prepareAlignmentCancellation
              cancellation
              (Alignment.alignmentTransferState owner)
          )
      let (cancelledTransfer, disposition) =
            Transfer.commitAlignmentCancellation prepared
          successorOwner =
            Alignment.replaceAlignmentTransferState cancelledTransfer owner
          newlyRetained = case disposition of
            Transfer.AlignmentTransferUnchanged -> []
            _ -> [cancellation]
      Right (successorOwner, cancellations <> newlyRetained)

    applyPlanInvalidation (owner, retainedReceipts, cancellations) planCause = do
      prepared <-
        either
          (Left . AlignmentLossPlanInvalidationProblem)
          Right
          (Alignment.prepareAlignmentPlanInvalidation planCause owner)
      let receipt = Alignment.preparedAlignmentPlanInvalidationReceipt prepared
          coordinate = Alignment.alignmentPlanInvalidationReceiptCoordinate receipt
          retiredOwners =
            Set.fromList
              (fst <$> Alignment.alignmentPlanInvalidationReceiptObligations receipt)
          planCancellations =
            Alignment.preparedAlignmentPlanInvalidationCancellations prepared
          (afterInvalidation, _) =
            Alignment.commitAlignmentPlanInvalidation prepared
      cleanup <-
        either
          (Left . AlignmentLossBarrierCleanupProblem)
          Right
          ( Transfer.prepareIncompletePredecessorInputClosureRemoval
              retiredOwners
              (Alignment.alignmentTransferState afterInvalidation)
          )
      let (cleanedTransfer, _) =
            Transfer.commitIncompletePredecessorInputClosureRemoval cleanup
          cleanedOwner =
            Alignment.replaceAlignmentTransferState cleanedTransfer afterInvalidation
      Right
        ( cleanedOwner,
          Map.insert coordinate receipt retainedReceipts,
          cancellations <> planCancellations
        )

preparedAlignmentLossPlan :: PreparedAlignmentLoss -> AlignmentLossPlan
preparedAlignmentLossPlan prepared = prepared.plan

preparedAlignmentLossDisposition ::
  PreparedAlignmentLoss -> AlignmentLossDisposition
preparedAlignmentLossDisposition prepared = prepared.disposition

commitAlignmentLoss :: PreparedAlignmentLoss -> AlignmentLossCoordinatorState
commitAlignmentLoss prepared = prepared.successor

applyOrdinarySourceLoss ::
  SourceReselectionPolicy ->
  (Alignment.State, [OrdinarySourceRetirement], [AlignmentCancel]) ->
  AlignmentAttempt ->
  Either
    AlignmentLossProblem
    (Alignment.State, [OrdinarySourceRetirement], [AlignmentCancel])
applyOrdinarySourceLoss reselectionPolicy (owner, retained, cancellations) attempt = do
  prepared <-
    either
      (Left . AlignmentLossOrdinaryInvalidationProblem)
      Right
      ( Alignment.prepareAlignmentAttemptInvalidation
          attempt
          AlignmentSourceIncarnationLost
          owner
      )
  let receipt = Alignment.preparedAlignmentAttemptInvalidationReceipt prepared
      cancellation = Alignment.preparedAlignmentAttemptInvalidationCancellation prepared
      (afterInvalidation, _) = Alignment.commitAlignmentAttemptInvalidation prepared
      identifier = alignmentAttemptObligationId attempt
  (afterReselection, replacement) <-
    case reselectionPolicy of
      DeferUntilSuccessorStructuralBase -> Right (afterInvalidation, Nothing)
      ReselectImmediately ->
        case Alignment.prepareAlignmentAttemptExcluding Set.empty identifier afterInvalidation of
          Right selection -> do
            let (withAttempt, _) = Alignment.commitAlignmentAttempt selection
                replacementAttempt = Alignment.preparedAlignmentAttempt selection
            preparedTransfer <-
              either
                (Left . AlignmentLossOrdinaryReselectionTransferProblem)
                Right
                ( Transfer.prepareDestinationAttempt
                    replacementAttempt
                    (Alignment.alignmentTransferState withAttempt)
                )
            let (successorTransfer, _) =
                  Transfer.commitDestinationAttempt preparedTransfer
                successor =
                  Alignment.replaceAlignmentTransferState successorTransfer withAttempt
            Right (successor, Just replacementAttempt)
          Left (Alignment.AlignmentAttemptSourceEvidenceUnavailable _) ->
            Right (afterInvalidation, Nothing)
          Left problem -> Left (AlignmentLossOrdinaryReselectionProblem problem)
  Right
    ( afterReselection,
      retained <> [OrdinarySourceRetirement attempt receipt replacement],
      cancellations <> [cancellation]
    )

applyBootstrapSourceLoss ::
  SourceReselectionPolicy ->
  (Alignment.State, [BootstrapSourceRetirement], [AlignmentCancel]) ->
  Alignment.BootstrapImportAttempt ->
  Either
    AlignmentLossProblem
    (Alignment.State, [BootstrapSourceRetirement], [AlignmentCancel])
applyBootstrapSourceLoss reselectionPolicy (owner, retained, cancellations) attempt = do
  prepared <-
    either
      (Left . AlignmentLossBootstrapInvalidationProblem)
      Right
      ( Alignment.prepareBootstrapImportAttemptInvalidation
          attempt
          AlignmentSourceIncarnationLost
          owner
      )
  let receipt =
        Alignment.preparedBootstrapImportAttemptInvalidationReceipt prepared
      cancellation =
        Alignment.preparedBootstrapImportAttemptInvalidationCancellation prepared
      (afterInvalidation, _) =
        Alignment.commitBootstrapImportAttemptInvalidation prepared
      key =
        Alignment.bootstrapImportKeyValue
          (Alignment.bootstrapImportAttemptImport attempt)
  (afterReselection, replacement) <-
    case reselectionPolicy of
      DeferUntilSuccessorStructuralBase -> Right (afterInvalidation, Nothing)
      ReselectImmediately ->
        case Alignment.prepareBootstrapImportAttemptExcluding
          Set.empty
          key
          afterInvalidation of
          Right selection ->
            let (successor, _) = Alignment.commitBootstrapImportAttempt selection
             in Right
                  ( successor,
                    Just (Alignment.preparedBootstrapImportAttempt selection)
                  )
          Left (Alignment.BootstrapImportSourceEvidenceUnavailable _) ->
            Right (afterInvalidation, Nothing)
          Left (Alignment.BootstrapImportMissing _) ->
            Right (afterInvalidation, Nothing)
          Left problem -> Left (AlignmentLossBootstrapReselectionProblem problem)
  Right
    ( afterReselection,
      retained <> [BootstrapSourceRetirement attempt receipt replacement],
      cancellations <> [cancellation]
    )

lossPlanCauses ::
  HeraldEpoch ->
  QualifiedStoreCoordinate ->
  Alignment.State ->
  Set Alignment.AlignmentPlanInvalidationCause
lossPlanCauses localHerald lost owner =
  Set.fromList (destinationCauses <> freshBaseCauses)
  where
    destinationCauses =
      [ Alignment.AlignmentPlanDestinationStoreLost
          generation
          (destinationStore lost.delta lost.incarnation)
      | generation <- Set.toAscList (localDestinationGenerations <> currentMemberGenerations)
      ]

    localDestinationGenerations
      | lost.herald == localHerald = activeDestinationGenerations
      | otherwise = Set.empty

    activeDestinationGenerations =
      Set.fromList
        ( [ alignmentObligationDestinationGeneration obligation
          | (_, obligation) <- Alignment.activeAlignmentObligationEntries owner,
            any
              (importUsesDestination lost)
              (NonEmpty.toList (alignmentObligationDestinationStores obligation))
          ]
            <> [ Alignment.bootstrapImportKeyGeneration key
               | (key, _) <- Alignment.bootstrapImportEntries owner,
                 importUsesDestination
                   lost
                   (Alignment.bootstrapImportKeyDestination key)
               ]
            <> [ alignmentObligationDestinationGeneration obligation
               | (_, receipt) <- Alignment.alignmentPlanInvalidationEntries owner,
                 (_, obligation) <-
                   Alignment.alignmentPlanInvalidationReceiptObligations receipt,
                 any
                   (importUsesDestination lost)
                   ( NonEmpty.toList
                       (alignmentObligationDestinationStores obligation)
                   )
               ]
            <> [ Alignment.bootstrapImportKeyGeneration key
               | (_, receipt) <- Alignment.alignmentPlanInvalidationEntries owner,
                 (key, _) <-
                   Alignment.alignmentPlanInvalidationReceiptBootstrapImports receipt,
                 importUsesDestination
                   lost
                   (Alignment.bootstrapImportKeyDestination key)
               ]
        )

    -- Every replica retains the complete plan, including foreign members whose
    -- one-time imports may already have finished. Exact checked placement loss
    -- invalidates that plan everywhere even when no live transfer can carry a
    -- cancellation. Superseded generations remain historical predecessors.
    currentMemberGenerations =
      Set.fromList
        [ alignmentGenerationId generation
        | (_, generation) <- Alignment.alignmentGenerationEntries owner,
          generationIsCurrent generation,
          member <- generationMembers generation,
          alignmentMemberHerald member == lost.herald,
          alignmentMemberDelta member == lost.delta,
          alignmentMemberStoreIncarnation member == lost.incarnation
        ]

    freshBaseCauses =
      [ Alignment.AlignmentPlanFreshBaseStoreLost
          generationId
          fresh
          (sourceCoordinate lost)
      | (key, _) <- activeAndRetainedBootstrapImports,
        Alignment.BootstrapFromFreshBase fresh <-
          [Alignment.bootstrapImportKeySource key],
        let generationId = Alignment.bootstrapImportKeyGeneration key,
        Just generation <- [Alignment.lookupAlignmentGeneration generationId owner],
        member <- generationMembers generation,
        alignmentMemberHerald member == lost.herald,
        alignmentMemberStoreIncarnation member == lost.incarnation,
        freshMatchesMember fresh member
      ]

    activeAndRetainedBootstrapImports =
      Alignment.bootstrapImportEntries owner
        <> concatMap
          (Alignment.alignmentPlanInvalidationReceiptBootstrapImports . snd)
          (Alignment.alignmentPlanInvalidationEntries owner)

    generationIsCurrent generation =
      any
        ((== alignmentGenerationId generation) . alignmentGenerationId)
        ( Alignment.latestAlignmentGenerationsForSort
            (generationSort generation)
            owner
        )

generationMembers :: AlignmentGeneration -> [AlignmentMember]
generationMembers generation =
  NonEmpty.toList
    (alignmentCutExactMembers (alignmentGenerationCut generation))

freshMatchesMember :: FreshMemberBaseEvidence -> AlignmentMember -> Bool
freshMatchesMember fresh member =
  freshMemberBaseDelta fresh == alignmentMemberDelta member
    && freshMemberBaseStoreIncarnation fresh
      == alignmentMemberStoreIncarnation member

generationSort :: AlignmentGeneration -> SortOccurrence
generationSort generation =
  let cut = alignmentGenerationCut generation
   in sortOccurrence
        (alignmentCutSortId cut)
        (alignmentCutSortDefinitionOccurrenceId cut)

sourceCoordinate ::
  QualifiedStoreCoordinate -> Alignment.AlignmentSourceCoordinate
sourceCoordinate lost =
  Alignment.alignmentSourceCoordinate lost.herald lost.incarnation

data AlignmentLossInvariantProblem
  = AlignmentLossOwnerInvariant Alignment.AlignmentStateInvariantProblem
  | AlignmentLossReceiptKeyMismatch
  | AlignmentLossLostStoreMismatch
  | AlignmentLossRedriveMarkerMismatch
  | AlignmentLossActiveDestinationUsesLostStore QualifiedStoreCoordinate
  | AlignmentLossOwnerHeraldMismatch HeraldEpoch HeraldEpoch
  deriving stock (Eq, Show)

-- | Exhaustive diagnostic of the retained Alignment owner and the loss
-- coordinator's relationships. Ordinary admission checks those relationships
-- directly; property and whole-state audits also replay the owner's history.
validateAlignmentLossCoordinatorState ::
  AlignmentLossCoordinatorState ->
  Either AlignmentLossInvariantProblem ()
validateAlignmentLossCoordinatorState state = do
  either
    (Left . AlignmentLossOwnerInvariant)
    Right
    (Alignment.validateAlignmentState state.owner)
  validateAlignmentLossCoordinatorRelationships state

-- The Alignment and Transfer owners admit their own opaque successors. A loss
-- transaction checks the relationships it introduces without re-auditing every
-- retained generation, promotion, certificate, and transfer transcript. Keep
-- that exhaustive diagnostic in 'validateAlignmentLossCoordinatorState' for
-- explicit whole-state audits and transition properties.
--
-- In particular, the supplied local Herald is a new claim at coordinator
-- construction, so destination ownership must still be checked here.
validateAlignmentLossCoordinatorRelationships ::
  AlignmentLossCoordinatorState ->
  Either AlignmentLossInvariantProblem ()
validateAlignmentLossCoordinatorRelationships state = do
  case firstForeignDestinationOwner state.localHerald state.owner of
    Nothing -> Right ()
    Just observed ->
      Left (AlignmentLossOwnerHeraldMismatch state.localHerald observed)
  if all receiptMatchesKey (Map.toAscList state.receipts)
    then Right ()
    else Left AlignmentLossReceiptKeyMismatch
  let receiptStores =
        Set.fromList
          [ store
          | ((_, store), _) <- Map.toAscList state.receipts
          ]
  if receiptStores == state.lostStores
    then Right ()
    else Left AlignmentLossLostStoreMismatch
  if expectedRedrive == state.redriveRequired
    then Right ()
    else Left AlignmentLossRedriveMarkerMismatch
  case firstActiveLostDestination state of
    Nothing -> Right ()
    Just lost -> Left (AlignmentLossActiveDestinationUsesLostStore lost)
  where
    receiptMatchesKey ((cause, store), AlignmentLossReceipt plan) =
      plan.cause == cause && plan.lostStore == store

    expectedRedrive =
      foldr retainReceiptRedrive Map.empty (Map.elems state.receipts)

    retainReceiptRedrive (AlignmentLossReceipt plan) retained =
      foldr
        ( \coordinate ->
            Map.insertWith Set.union coordinate (Set.singleton plan.cause)
        )
        retained
        (Map.keys plan.planEvents)

firstActiveLostDestination ::
  AlignmentLossCoordinatorState -> Maybe QualifiedStoreCoordinate
firstActiveLostDestination state =
  Set.lookupMin
    ( Set.filter
        (activeDestinationUses state.localHerald state.owner)
        state.lostStores
    )

activeDestinationUses ::
  HeraldEpoch ->
  Alignment.State ->
  QualifiedStoreCoordinate ->
  Bool
activeDestinationUses localHerald owner lost =
  lost.herald == localHerald
    && ( any (obligationUsesDestination lost . snd) (Alignment.activeAlignmentObligationEntries owner)
           || any
             ( importUsesDestination lost
                 . Alignment.bootstrapImportKeyDestination
                 . fst
             )
             (Alignment.bootstrapImportEntries owner)
       )

obligationUsesDestination :: QualifiedStoreCoordinate -> AlignmentObligation -> Bool
obligationUsesDestination lost obligation =
  alignmentObligationIdDestinationHerald
    (alignmentObligationIdValue obligation)
    == lost.herald
    && any
      (importUsesDestination lost)
      (NonEmpty.toList (alignmentObligationDestinationStores obligation))

importUsesDestination :: QualifiedStoreCoordinate -> DestinationStore -> Bool
importUsesDestination lost destination =
  destinationStoreDelta destination == lost.delta
    && destinationStoreIncarnation destination == lost.incarnation

sourceSubscriptionUsesLostDestination ::
  QualifiedStoreCoordinate ->
  Transfer.SourceSubscription ->
  Bool
sourceSubscriptionUsesLostDestination lost source =
  alignmentObligationIdDestinationHerald
    (alignmentSubscribeObligationId subscribe)
    == lost.herald
    && any
      (importUsesDestination lost)
      (NonEmpty.toList (alignmentObligationDestinationStores obligation))
  where
    subscribe = Transfer.sourceSubscriptionSubscribe source
    obligation = alignmentSubscribeObligation subscribe

normalizeCancellations :: [AlignmentCancel] -> [AlignmentCancel]
normalizeCancellations cancellations =
  Map.elems
    ( Map.fromList
        [ (alignmentCancelSubscriptionId cancellation, cancellation)
        | cancellation <- cancellations
        ]
    )

firstForeignDestinationOwner ::
  HeraldEpoch -> Alignment.State -> Maybe HeraldEpoch
firstForeignDestinationOwner local owner =
  case filter
    (/= local)
    ( fmap
        ( alignmentObligationIdDestinationHerald
            . alignmentObligationIdValue
        )
        retainedObligations
    ) of
    [] -> Nothing
    observed : _ -> Just observed
  where
    retainedObligations =
      fmap snd (Alignment.alignmentObligationEntries owner)
        <> fmap snd (Alignment.alignmentClosedObligationEntries owner)
        <> fmap
          ( alignmentAttemptObligation
              . Alignment.alignmentAttemptInvalidationAttempt
              . snd
          )
          (Alignment.alignmentAttemptInvalidationEntries owner)
        <> fmap
          ( Alignment.bootstrapImportSubscribeEnvelope
              . snd
          )
          (Alignment.bootstrapImportEntries owner)
        <> concatMap
          ( fmap snd
              . Alignment.alignmentPlanInvalidationReceiptObligations
              . snd
          )
          (Alignment.alignmentPlanInvalidationEntries owner)
        <> fmap
          ( Alignment.bootstrapImportSubscribeEnvelope
              . Alignment.bootstrapImportAttemptImport
              . Alignment.bootstrapImportAttemptInvalidationAttempt
              . snd
          )
          (Alignment.bootstrapImportAttemptInvalidationEntries owner)
        <> fmap
          ( Alignment.bootstrapImportSubscribeEnvelope
              . Alignment.bootstrapImportAttemptImport
              . Alignment.bootstrapImportFulfillmentAttempt
              . snd
          )
          (Alignment.bootstrapImportFulfillmentEntries owner)
        <> concatMap
          ( fmap
              ( Alignment.bootstrapImportSubscribeEnvelope
                  . snd
              )
              . Alignment.alignmentPlanInvalidationReceiptBootstrapImports
              . snd
          )
          (Alignment.alignmentPlanInvalidationEntries owner)
