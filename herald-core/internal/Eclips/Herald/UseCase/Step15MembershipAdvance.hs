{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Package-private composition of one already-authoritative Step-15
-- membership contraction with its immediately executable Herald consequences.
--
-- The live Oracle-watch path reaches this seam only after the canonical
-- retirement entry has made the successor membership authoritative. TCP and
-- runtime code still have no authority to infer retirement from transport loss.
module Eclips.Herald.UseCase.Step15MembershipAdvance
  ( MembershipAdvanceProblem (..),
    MembershipAdvanceDisposition (..),
    PreparedMembershipAdvance,
    prepareMembershipAdvance,
    preparedMembershipAdvanceDisposition,
    preparedMembershipAdvanceEffects,
    preparedMembershipAdvanceRetiredPlacement,
    preparedMembershipAdvanceSettledAssignments,
    preparedMembershipAdvanceLostStores,
    preparedMembershipAdvanceAlignmentLossPlans,
    preparedMembershipAdvanceRetiredGenerationEvidence,
    commitMembershipAdvance,
  )
where

import Control.Monad (foldM, unless)
import Data.Set qualified as Set
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationId,
    heraldMembershipGenerationRetiredHeraldEpoch,
  )
import Eclips.Herald.Alignment.Loss qualified as AlignmentLoss
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Discovery
  ( DiscoveryInvariantFault,
    peerDialIntentHeraldEpoch,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleClient (OracleClientAction (CancelOracleRetry))
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.PeerStream
  ( AssignmentReceipt,
    streamDirectionDestination,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement
  ( deltaRouteDelta,
    deltaRouteStoreIncarnation,
  )
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.Invariant (HeraldInvariantFault)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentState,
    replaceStartupDiscoveryState,
    replaceStartupOracleClientState,
    replaceStartupOracleProjectionState,
    replaceStartupPeerLivenessState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupPublicationState,
    replaceStartupStructuralProgressState,
    startupAlignmentState,
    startupDiagnosticChecks,
    startupDiscoveryState,
    startupGenesis,
    startupJoinState,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerLivenessState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupStructuralProgressState,
  )
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.JoinControlTails qualified as JoinControlTails
import Eclips.Herald.UseCase.OracleAdvance qualified as OracleAdvance

data MembershipAdvanceProblem
  = MembershipProjectionProblem OracleProjection.MembershipAdvanceProblem
  | MembershipSuccessorHasNoRetiredEpoch
  | MembershipDiscoveryProblem DiscoveryInvariantFault
  | MembershipOracleClientProblem OracleClient.OracleClientProblem
  | MembershipOracleClientEvidenceMismatch ControlIndex
  | MembershipAdmissionTailRequiresCanonicalEntry ControlIndex
  | MembershipPeerStreamProblem PeerStream.PeerStreamProblem
  | MembershipPlacementProblem Placement.PlacementProblem
  | MembershipAlignmentLossInvariant
      AlignmentLoss.AlignmentLossInvariantProblem
  | MembershipAlignmentLossProblem AlignmentLoss.AlignmentLossProblem
  | MembershipAlignmentTransferProblem
      AlignmentTransfer.AlignmentTransferCoordinatorProblem
  | MembershipResidentEndProblem HeraldInvariantFault
  | MembershipStructuralProgressProblem GraphProgress.StructuralProgressProblem
  deriving stock (Eq, Show)

data MembershipAdvanceDisposition
  = MembershipAdvanceApplied HeraldEpoch Bool
  | MembershipAdvanceExactDuplicate
  deriving stock (Eq, Show)

data PreparedMembershipAdvance = PreparedMembershipAdvance
  { successor :: HeraldState,
    disposition :: MembershipAdvanceDisposition,
    effects :: EffectBatch,
    retiredPlacement :: Maybe Placement.RetiredRemotePlacement,
    settledAssignments :: [AssignmentReceipt],
    lostStores :: [AlignmentLoss.QualifiedStoreCoordinate],
    alignmentLossPlans :: [AlignmentLoss.AlignmentLossPlan],
    retiredGenerationEvidence :: [Alignment.PendingGenerationEvidence]
  }

prepareMembershipAdvance ::
  ControlIndex ->
  HeraldMembershipGeneration ->
  [OracleProjection.MembershipAdvanceProcessEnd] ->
  HeraldState ->
  Either MembershipAdvanceProblem PreparedMembershipAdvance
prepareMembershipAdvance index membership processEnds predecessor = do
  preparedProjection <-
    either
      (Left . MembershipProjectionProblem)
      Right
      ( OracleProjection.prepareMembershipAdvance
          index
          membership
          processEnds
          (startupOracleProjectionState predecessor)
      )
  case OracleProjection.preparedMembershipAdvanceDisposition preparedProjection of
    OracleProjection.MembershipAdvanceExactDuplicate ->
      if OracleClient.oracleClientMembershipAdvance
        index
        (startupOracleClientState predecessor)
        == Just (heraldMembershipGenerationId membership)
        then
          Right
            PreparedMembershipAdvance
              { successor = predecessor,
                disposition = MembershipAdvanceExactDuplicate,
                effects = orderedEffectBatch [],
                retiredPlacement = Nothing,
                settledAssignments = [],
                lostStores = [],
                alignmentLossPlans = [],
                retiredGenerationEvidence = []
              }
        else Left (MembershipOracleClientEvidenceMismatch index)
    OracleProjection.MembershipAdvanceApplied -> do
      retired <-
        maybe
          (Left MembershipSuccessorHasNoRetiredEpoch)
          Right
          (heraldMembershipGenerationRetiredHeraldEpoch membership)
      let tailReleased = JoinControlTails.releaseRetired retired predecessor
      -- This reference seam has detached membership evidence, not the exact
      -- canonical entry required to extend an outstanding admission tail.
      unless
        (null (Join.admissionControlTails (startupJoinState tailReleased)))
        (Left (MembershipAdmissionTailRequiresCanonicalEntry index))
      applyFresh retired preparedProjection tailReleased
  where
    applyFresh retired preparedProjection tailReleased = do
      preparedOracleClient <-
        either
          (Left . MembershipOracleClientProblem)
          Right
          ( OracleClient.prepareMembershipCursorAdvance
              index
              (heraldMembershipGenerationId membership)
              (startupOracleClientState predecessor)
          )
      let discoveryBefore = startupDiscoveryState predecessor
          pendingDials = Discovery.peerDialIntents discoveryBefore
      preparedDiscovery <-
        either
          (Left . MembershipDiscoveryProblem)
          Right
          (Discovery.prepareMembershipAdvance membership discoveryBefore)
      let retiredBindings =
            Discovery.preparedMembershipRetiredBindings preparedDiscovery
          localRetired =
            Discovery.preparedMembershipLocalRetired preparedDiscovery
          dials =
            if localRetired
              then pendingDials
              else filter ((== retired) . peerDialIntentHeraldEpoch) pendingDials
          (discovery, _) =
            Discovery.commitMembershipAdvance preparedDiscovery
          advancedOracleClient =
            OracleClient.commitMembershipCursorAdvance preparedOracleClient
          (peerLiveness, recoveryTimers) =
            if localRetired
              then PeerLiveness.cutForDrain (startupPeerLivenessState predecessor)
              else
                let (successor, cancellation) =
                      PeerLiveness.retirePeer retired (startupPeerLivenessState predecessor)
                 in (successor, maybe [] pure cancellation)
      preparedOracleProgress <-
        either
          (Left . MembershipOracleClientProblem)
          Right
          ( OracleClient.prepareCommittedPrefixProgress
              (OracleClient.oracleClientAppliedCursor (startupOracleClientState predecessor))
              advancedOracleClient
          )
      let progressedOracleClient =
            OracleClient.commitClientIngress preparedOracleProgress
          oracleProgressEffects =
            fmap
              RunOracleClientAction
              (OracleClient.preparedClientIngressActions preparedOracleProgress)
      oracleClient <-
        if localRetired
          then
            either
              (Left . MembershipOracleClientProblem)
              Right
              (OracleClient.prepareDrain progressedOracleClient)
          else Right progressedOracleClient
      let oracleContinuationEffects =
            if localRetired
              then []
              else fmap RunOracleClientAction (OracleClient.oracleClientActions oracleClient)
      (peerStream, placement, cancelledDestinations, retiredPlacement, settledAssignments) <-
        if localRetired
          then
            Right
              ( startupPeerStreamState predecessor,
                startupPlacementState predecessor,
                [],
                Nothing,
                []
              )
          else do
            preparedPeerStream <-
              either
                (Left . MembershipPeerStreamProblem)
                Right
                ( PeerStream.preparePeerRetirement
                    retired
                    (startupPeerStreamState predecessor)
                )
            preparedPlacement <-
              either
                (Left . MembershipPlacementProblem)
                Right
                ( Placement.prepareRemotePlacementRetirementWithDiagnostics
                    (startupDiagnosticChecks predecessor)
                    retired
                    (startupPlacementState predecessor)
                )
            let settledAssignments =
                  PeerStream.preparedPeerRetirementSettledAssignments
                    preparedPeerStream
                (nextPeerStream, _) =
                  PeerStream.commitPeerRetirement preparedPeerStream
                (nextPlacement, retiredPlacement) =
                  Placement.commitRemotePlacementRetirement preparedPlacement
            Right
              ( nextPeerStream,
                nextPlacement,
                [retired],
                Just retiredPlacement,
                settledAssignments
              )
      let withMembership =
            replaceStartupPublicationState
              (Publication.retirePublicationGroupPeer retired (startupPublicationState predecessor))
              . replaceStartupPlacementState placement
              . replaceStartupStructuralProgressState
                (GraphProgress.notifyAlignmentMembershipChange (startupStructuralProgressState predecessor))
              . replaceStartupPeerStreamState peerStream
              . replaceStartupPeerLivenessState peerLiveness
              . replaceStartupDiscoveryState discovery
              . replaceStartupOracleClientState oracleClient
              . replaceStartupOracleProjectionState
                (OracleProjection.commitMembershipAdvance preparedProjection)
              $ tailReleased
      (withEnds, endEffects) <-
        foldM
          applyProjectedEnd
          (withMembership, [])
          (OracleProjection.preparedMembershipAdvanceProcessEnds preparedProjection)
      (withFinalPeerCut, finalCancelledDestinations) <-
        if localRetired
          then do
            preparedPeerStream <-
              either
                (Left . MembershipPeerStreamProblem)
                Right
                (PeerStream.prepareLocalRetirementCut (startupPeerStreamState withEnds))
            let (nextPeerStream, directions) =
                  PeerStream.commitLocalRetirementCut preparedPeerStream
            Right
              ( replaceStartupPublicationState
                  (foldl' (flip Publication.retirePublicationGroupPeer) (startupPublicationState withEnds) (fmap streamDirectionDestination directions))
                  (replaceStartupPeerStreamState nextPeerStream withEnds),
                fmap streamDirectionDestination directions
              )
          else Right (withEnds, cancelledDestinations)
      preparedControlProgress <-
        either
          (Left . MembershipStructuralProgressProblem)
          Right
          ( GraphProgress.prepareStructuralControlProgress
              index
              (startupStructuralProgressState withFinalPeerCut)
          )
      let (structuralProgress, _) =
            GraphProgress.commitStructuralControlProgress preparedControlProgress
          withControlProgress =
            replaceStartupStructuralProgressState structuralProgress withFinalPeerCut
      (withReleasedDependencies, dependencyEffects) <-
        if localRetired
          then Right (withControlProgress, orderedEffectBatch [])
          else
            either
              (Left . MembershipResidentEndProblem)
              Right
              (OracleAdvance.releaseControlIndexDependencies withControlProgress)
      let preparedEvidenceRetirement =
            Alignment.preparePendingGenerationEvidenceRetirement
              retired
              (startupAlignmentState withReleasedDependencies)
          (alignmentAfterEvidenceRetirement, retiredGenerationEvidence) =
            Alignment.commitPendingGenerationEvidenceRetirement
              preparedEvidenceRetirement
          withRetiredGenerationEvidence =
            replaceStartupAlignmentState
              alignmentAfterEvidenceRetirement
              withReleasedDependencies
      (afterLoss, lostStores, alignmentLossPlans) <-
        case retiredPlacement of
          Nothing -> Right (withRetiredGenerationEvidence, [], [])
          Just retiredProjection -> do
            let lostStores = retiredPlacementCoordinates retiredProjection
                alignmentBeforeLoss =
                  startupAlignmentState withRetiredGenerationEvidence
            (alignment, plans) <-
              applyRetiredPlacementLosses
                (checkedLocalHeraldEpoch (startupGenesis predecessor))
                lostStores
                alignmentBeforeLoss
            Right
              ( replaceStartupAlignmentState alignment withRetiredGenerationEvidence,
                lostStores,
                plans
              )
      (successor, alignmentLossEffects) <-
        either
          (Left . MembershipAlignmentTransferProblem)
          Right
          ( AlignmentTransfer.deliverAlignmentLossTransferDelta
              withRetiredGenerationEvidence
              afterLoss
          )
      let retirementEffects =
            ( if localRetired
                then filter localCleanupEffect (oracleProgressEffects <> endEffects)
                else oracleProgressEffects <> oracleContinuationEffects <> endEffects
            )
              <> fmap CancelTimer recoveryTimers
              <> fmap CancelPeerDial dials
              <> fmap ClosePeerBinding retiredBindings
              <> fmap CancelPeerDestination finalCancelledDestinations
              <> effectBatchMembers dependencyEffects
              <> effectBatchMembers alignmentLossEffects
      Right
        PreparedMembershipAdvance
          { successor,
            disposition = MembershipAdvanceApplied retired localRetired,
            effects = orderedEffectBatch retirementEffects,
            retiredPlacement,
            settledAssignments,
            lostStores,
            alignmentLossPlans,
            retiredGenerationEvidence
          }

    applyProjectedEnd (state, retainedEffects) ended = do
      (next, emitted) <-
        either
          (Left . MembershipResidentEndProblem)
          Right
          ( OracleAdvance.applyResidentEnd
              (OracleProjection.projectedEndedProcessEpoch ended)
              (OracleProjection.projectedEndedProcessControlIndex ended)
              (OracleProjection.projectedEndedProcessReason ended)
              state
          )
      Right (next, retainedEffects <> emitted)

    localCleanupEffect = \case
      SendApplicationIsolationBegun {} -> True
      DisposeApplicationSession {} -> True
      CancelTimer {} -> True
      CancelPeerDial {} -> True
      ClosePeerBinding {} -> True
      CancelPeerDestination {} -> True
      RunOracleClientAction (CancelOracleRetry _) -> True
      _ -> False

preparedMembershipAdvanceDisposition ::
  PreparedMembershipAdvance -> MembershipAdvanceDisposition
preparedMembershipAdvanceDisposition prepared = prepared.disposition

preparedMembershipAdvanceEffects :: PreparedMembershipAdvance -> EffectBatch
preparedMembershipAdvanceEffects prepared = prepared.effects

preparedMembershipAdvanceRetiredPlacement ::
  PreparedMembershipAdvance ->
  Maybe Placement.RetiredRemotePlacement
preparedMembershipAdvanceRetiredPlacement prepared = prepared.retiredPlacement

preparedMembershipAdvanceSettledAssignments ::
  PreparedMembershipAdvance ->
  [AssignmentReceipt]
preparedMembershipAdvanceSettledAssignments prepared = prepared.settledAssignments

preparedMembershipAdvanceLostStores ::
  PreparedMembershipAdvance -> [AlignmentLoss.QualifiedStoreCoordinate]
preparedMembershipAdvanceLostStores prepared = prepared.lostStores

preparedMembershipAdvanceAlignmentLossPlans ::
  PreparedMembershipAdvance -> [AlignmentLoss.AlignmentLossPlan]
preparedMembershipAdvanceAlignmentLossPlans prepared = prepared.alignmentLossPlans

preparedMembershipAdvanceRetiredGenerationEvidence ::
  PreparedMembershipAdvance -> [Alignment.PendingGenerationEvidence]
preparedMembershipAdvanceRetiredGenerationEvidence prepared =
  prepared.retiredGenerationEvidence

commitMembershipAdvance :: PreparedMembershipAdvance -> HeraldState
commitMembershipAdvance prepared = prepared.successor

retiredPlacementCoordinates ::
  Placement.RetiredRemotePlacement ->
  [AlignmentLoss.QualifiedStoreCoordinate]
retiredPlacementCoordinates retiredPlacement =
  Set.toAscList
    ( Set.fromList
        [ AlignmentLoss.qualifiedStoreCoordinate
            owner
            (deltaRouteDelta route)
            (deltaRouteStoreIncarnation route)
        | route <- Placement.retiredRemotePlacementRoutes retiredPlacement
        ]
    )
  where
    owner = Placement.retiredRemotePlacementOwner retiredPlacement

applyRetiredPlacementLosses ::
  HeraldEpoch ->
  [AlignmentLoss.QualifiedStoreCoordinate] ->
  Alignment.State ->
  Either
    MembershipAdvanceProblem
    (Alignment.State, [AlignmentLoss.AlignmentLossPlan])
applyRetiredPlacementLosses local lostStores predecessor =
  foldM applyCoordinate (predecessor, []) lostStores
  where
    applyCoordinate (owner, retainedPlans) lost =
      case Set.toAscList (AlignmentLoss.alignmentLossRelevantCauses lost owner) of
        [] ->
          let unavailable =
                Alignment.unavailableAlignmentSource
                  (AlignmentLoss.qualifiedStoreCoordinateHerald lost)
                  (AlignmentLoss.qualifiedStoreCoordinateDelta lost)
                  (AlignmentLoss.qualifiedStoreCoordinateIncarnation lost)
              (successor, _) =
                Alignment.commitUnavailableAlignmentSourceRetention
                  (Alignment.prepareUnavailableAlignmentSourceRetention unavailable owner)
           in Right (successor, retainedPlans)
        causes -> do
          coordinator <-
            either
              (Left . MembershipAlignmentLossInvariant)
              Right
              (AlignmentLoss.alignmentLossCoordinatorState local owner)
          (successorCoordinator, plans) <-
            foldM (applyCause lost) (coordinator, []) causes
          Right
            ( AlignmentLoss.alignmentLossCoordinatorOwner successorCoordinator,
              retainedPlans <> plans
            )

    applyCause lost (coordinator, plans) cause = do
      prepared <-
        either
          (Left . MembershipAlignmentLossProblem)
          Right
          ( AlignmentLoss.prepareAlignmentLossAfterMembershipRetirement
              cause
              lost
              coordinator
          )
      Right
        ( AlignmentLoss.commitAlignmentLoss prepared,
          plans <> [AlignmentLoss.preparedAlignmentLossPlan prepared]
        )
