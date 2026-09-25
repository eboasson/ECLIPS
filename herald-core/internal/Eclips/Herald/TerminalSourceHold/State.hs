{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Independently retained authenticated controls awaiting membership or local
-- evidence. A later target may arrive before intermediate Oracle entries. Each
-- occurrence keeps its target and phase; obsolete targets become inert without
-- rejecting the still-active supplying peer.
module Eclips.Herald.TerminalSourceHold.State
  ( State,
    initialState,
    TerminalSourceHoldDisposition (..),
    retainAheadControl,
    retainReadinessControl,
    retainSuccessorStructuralReport,
    TerminalSourceHoldResolution (..),
    resolveForMembershipAdvance,
    TerminalSourceReadinessResolution (..),
    resolveForReadinessAdvance,
    consumeReadinessControl,
    heldTerminalSourceControls,
    terminalSourceControlTarget,
    validateState,
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipHistory,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipHistoryCurrent,
    heraldMembershipLineage,
    heraldMembershipLineageRetiredHeraldEpochs,
    lookupHeraldMembershipGeneration,
  )
import Eclips.Domain.Structural (structuralVersionVectorCovers, structuralVersionVectorEntries)
import Eclips.Herald.Discovery (PeerBinding, peerBindingRemoteHeraldEpoch)
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    structuralAppliedReportControlPrefix,
    structuralAppliedReportMemberSetDigest,
    structuralAppliedReportMembershipGenerationId,
    structuralAppliedReportReporter,
    structuralAppliedReportVersionVector,
  )
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.Input (PeerControl (..))

data TerminalSourceCoordinates = TerminalSourceCoordinates
  { predecessor :: HeraldMembershipGenerationId,
    successor :: HeraldMembershipGenerationId,
    retired :: Set.Set HeraldEpoch
  }
  deriving stock (Eq, Show)

data TerminalSourceHoldPhase = AwaitingOracleProjection | AwaitingLocalReadiness
  deriving stock (Eq, Show)

data HeldTerminalSourceControl = HeldTerminalSourceControl
  {phase :: TerminalSourceHoldPhase, binding :: PeerBinding, control :: PeerControl}
  deriving stock (Eq, Show)

newtype State = State [HeldTerminalSourceControl]
  deriving stock (Eq, Show)

initialState :: State
initialState = State []

data TerminalSourceHoldDisposition
  = TerminalSourceControlHeld
  | TerminalSourceControlAdvanced
  | TerminalSourceControlDuplicate
  | TerminalSourceControlRejected
  deriving stock (Eq, Show)

retainAheadControl :: HeraldMembershipHistory -> PeerBinding -> PeerControl -> State -> (State, TerminalSourceHoldDisposition)
retainAheadControl history binding control state
  | historicalTarget history control = (state, TerminalSourceControlDuplicate)
  | admissible AwaitingOracleProjection history binding control = retain AwaitingOracleProjection binding control state
  | otherwise = (state, TerminalSourceControlRejected)

retainReadinessControl :: HeraldMembershipHistory -> PeerBinding -> PeerControl -> State -> (State, TerminalSourceHoldDisposition)
retainReadinessControl history binding control state
  | historicalTarget history control = (state, TerminalSourceControlDuplicate)
  | admissible AwaitingLocalReadiness history binding control = retain AwaitingLocalReadiness binding control state
  | otherwise = (state, TerminalSourceControlRejected)

retain :: TerminalSourceHoldPhase -> PeerBinding -> PeerControl -> State -> (State, TerminalSourceHoldDisposition)
retain phase binding control (State controls) =
  case break (sameOccurrence binding control) controls of
    (_, []) -> (State (controls <> [HeldTerminalSourceControl phase binding control]), TerminalSourceControlHeld)
    (before, _ : after) -> (State (before <> [HeldTerminalSourceControl phase binding control] <> after), TerminalSourceControlDuplicate)

-- Reports coalesce only within the same target and authenticated reporter.
-- Monotonic growth preserves a single maximum; incomparable reports reject.
retainSuccessorStructuralReport :: HeraldMembershipHistory -> PeerBinding -> StructuralAppliedReport -> State -> (State, TerminalSourceHoldDisposition)
retainSuccessorStructuralReport history binding report state@(State controls)
  | historicalTarget history control = (state, TerminalSourceControlDuplicate)
  | not (admissible phase history binding control) = (state, TerminalSourceControlRejected)
  | otherwise = case break sameReporterTarget controls of
      (_, []) -> retain phase binding control state
      (before, incumbent : after) -> case incumbent.control of
        PeerStructuralAppliedReported previous
          | reportCovers report previous -> (State (before <> [HeldTerminalSourceControl phase binding control] <> after), if report == previous then TerminalSourceControlDuplicate else TerminalSourceControlAdvanced)
          | reportCovers previous report -> (State (before <> [incumbent {phase, binding}] <> after), TerminalSourceControlDuplicate)
        _ -> (state, TerminalSourceControlRejected)
  where
    control = PeerStructuralAppliedReported report
    target = structuralAppliedReportMembershipGenerationId report
    phase = if lookupHeraldMembershipGeneration target history == Nothing then AwaitingOracleProjection else AwaitingLocalReadiness
    sameReporterTarget held = case held.control of
      PeerStructuralAppliedReported previous -> structuralAppliedReportReporter previous == structuralAppliedReportReporter report && structuralAppliedReportMembershipGenerationId previous == target
      _ -> False

reportCovers :: StructuralAppliedReport -> StructuralAppliedReport -> Bool
reportCovers covering required = structuralVersionVectorCovers (structuralAppliedReportVersionVector covering) (structuralAppliedReportVersionVector required) && structuralAppliedReportControlPrefix covering >= structuralAppliedReportControlPrefix required

data TerminalSourceHoldResolution
  = TerminalSourceHoldUnchanged State
  | TerminalSourceHoldMatched State [(PeerBinding, PeerControl)]
  deriving stock (Eq, Show)

resolveForMembershipAdvance :: HeraldMembershipHistory -> State -> TerminalSourceHoldResolution
resolveForMembershipAdvance history state = case release AwaitingOracleProjection history state of
  (retained, controls) | retained == state && null controls -> TerminalSourceHoldUnchanged state
  (retained, controls) -> TerminalSourceHoldMatched retained controls

data TerminalSourceReadinessResolution
  = TerminalSourceReadinessUnchanged State
  | TerminalSourceReadinessMatched State [(PeerBinding, PeerControl)]
  deriving stock (Eq, Show)

resolveForReadinessAdvance :: HeraldMembershipHistory -> State -> TerminalSourceReadinessResolution
resolveForReadinessAdvance history state = case release AwaitingLocalReadiness history state of
  (retained, controls) | retained == state && null controls -> TerminalSourceReadinessUnchanged state
  (retained, controls) -> TerminalSourceReadinessMatched retained controls

release :: TerminalSourceHoldPhase -> HeraldMembershipHistory -> State -> (State, [(PeerBinding, PeerControl)])
release requested history (State controls) =
  let (retained, ready) = foldr one ([], []) controls
   in (State retained, ready)
  where
    currentId = heraldMembershipGenerationId (heraldMembershipHistoryCurrent history)
    one held (retained, ready)
      | historicalTarget history held.control = (retained, ready)
      | not (bindingActive history held.binding) = (retained, ready)
      | held.phase == requested,
        terminalSourceControlTarget held.control == Just currentId =
          if admissible AwaitingLocalReadiness history held.binding held.control || currentTerminalAdmissible history held.binding held.control
            then (retained, (held.binding, held.control) : ready)
            else (retained, ready)
      | otherwise = (held : retained, ready)

consumeReadinessControl :: PeerBinding -> PeerControl -> State -> State
consumeReadinessControl binding control (State controls) = State (filter (not . sameOccurrence binding control) controls)

sameOccurrence :: PeerBinding -> PeerControl -> HeldTerminalSourceControl -> Bool
sameOccurrence binding control held = peerBindingRemoteHeraldEpoch held.binding == peerBindingRemoteHeraldEpoch binding && held.control == control

heldTerminalSourceControls :: State -> [(PeerBinding, PeerControl)]
heldTerminalSourceControls (State controls) = [(held.binding, held.control) | held <- controls]

validateState :: HeraldMembershipHistory -> State -> Either String ()
validateState history (State controls)
  | all (\held -> admissible held.phase history held.binding held.control) controls
      && uniqueOccurrences controls =
      Right ()
  | otherwise = Left "terminal-source hold contains an unauthenticated or obsolete membership coordinate"
  where
    uniqueOccurrences [] = True
    uniqueOccurrences (held : remaining) = not (any (sameOccurrence held.binding held.control) remaining) && uniqueOccurrences remaining

admissible :: TerminalSourceHoldPhase -> HeraldMembershipHistory -> PeerBinding -> PeerControl -> Bool
admissible phase history binding control =
  bindingActive history binding && case terminalSourceControlTarget control of
    Nothing -> False
    Just target -> case phase of
      AwaitingOracleProjection -> lookupHeraldMembershipGeneration target history == Nothing && futureShape control && authenticated control
      AwaitingLocalReadiness ->
        target == currentId && controlMayAwaitReadiness control && case control of
          PeerStructuralAppliedReported report -> structuralReportMatches current binding report
          _ -> currentTerminalAdmissible history binding control
  where
    current = heraldMembershipHistoryCurrent history
    currentId = heraldMembershipGenerationId current
    remote = peerBindingRemoteHeraldEpoch binding
    authenticated (PeerStructuralAppliedReported report) = remote == structuralAppliedReportReporter report
    authenticated supplied = controlAuthenticatedBy remote supplied
    futureShape (PeerStructuralAppliedReported _) = True
    futureShape supplied = case terminalSourceControlCoordinates supplied of
      Just coordinates -> coordinates.predecessor /= coordinates.successor && not (Set.null coordinates.retired) && Set.notMember remote coordinates.retired
      Nothing -> False

currentTerminalAdmissible :: HeraldMembershipHistory -> PeerBinding -> PeerControl -> Bool
currentTerminalAdmissible history binding control =
  bindingActive history binding && controlAuthenticatedBy remote control && case terminalSourceControlCoordinates control of
    Just supplied | supplied.successor == currentId -> case heraldMembershipLineage supplied.predecessor supplied.successor history of
      Right lineage ->
        let retired = Set.fromList (heraldMembershipLineageRetiredHeraldEpochs lineage)
         in not (Set.null supplied.retired) && Set.notMember remote supplied.retired && if isPerSource control then supplied.retired `Set.isSubsetOf` retired else supplied.retired == retired
      Left _ -> False
    _ -> False
  where
    currentId = heraldMembershipGenerationId (heraldMembershipHistoryCurrent history)
    remote = peerBindingRemoteHeraldEpoch binding
    isPerSource PeerTerminalSourceInventoryAdvertised {} = True
    isPerSource PeerTerminalSourcePayloadRequested {} = True
    isPerSource PeerTerminalSourcePayloadRelayed {} = True
    isPerSource _ = False

historicalTarget :: HeraldMembershipHistory -> PeerControl -> Bool
historicalTarget history control = case terminalSourceControlTarget control of
  Just target -> target /= heraldMembershipGenerationId (heraldMembershipHistoryCurrent history) && lookupHeraldMembershipGeneration target history /= Nothing
  Nothing -> False

bindingActive :: HeraldMembershipHistory -> PeerBinding -> Bool
bindingActive history binding = peerBindingRemoteHeraldEpoch binding `Set.member` membershipMembers (heraldMembershipHistoryCurrent history)

controlMayAwaitReadiness :: PeerControl -> Bool
controlMayAwaitReadiness control = case control of
  PeerTerminalSourceInventoryAdvertised {} -> True
  PeerTerminalSourceUnionAnnounced {} -> True
  PeerTerminalSourceUnionEstablished {} -> True
  PeerStructuralAppliedReported {} -> True
  _ -> False

structuralReportMatches :: HeraldMembershipGeneration -> PeerBinding -> StructuralAppliedReport -> Bool
structuralReportMatches membership binding report = peerBindingRemoteHeraldEpoch binding == structuralAppliedReportReporter report && structuralAppliedReportMembershipGenerationId report == heraldMembershipGenerationId membership && structuralAppliedReportMemberSetDigest report == heraldMembershipGenerationActiveMemberSetDigest membership && Set.fromList (fmap fst (structuralVersionVectorEntries (structuralAppliedReportVersionVector report))) == membershipMembers membership

terminalSourceControlTarget :: PeerControl -> Maybe HeraldMembershipGenerationId
terminalSourceControlTarget (PeerStructuralAppliedReported report) = Just (structuralAppliedReportMembershipGenerationId report)
terminalSourceControlTarget control = (.successor) <$> terminalSourceControlCoordinates control

membershipMembers :: HeraldMembershipGeneration -> Set.Set HeraldEpoch
membershipMembers = Set.fromList . NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs

controlAuthenticatedBy :: HeraldEpoch -> PeerControl -> Bool
controlAuthenticatedBy remote control = case control of
  PeerTerminalSourceInventoryAdvertised inventory ->
    TerminalSource.terminalSourceInventoryReporter inventory == remote
  PeerTerminalSourcePayloadRequested {} -> True
  PeerTerminalSourcePayloadRelayed relay ->
    TerminalSource.terminalSourcePayloadRelayHerald relay == remote
  PeerTerminalSourceUnionAnnounced announce ->
    TerminalSource.terminalSourceUnionAnnounceAnnouncer announce == remote
  PeerTerminalSourceUnionAccepted acceptance ->
    TerminalSource.terminalSourceUnionAcceptanceReporter acceptance == remote
  PeerTerminalSourceUnionEstablished established ->
    terminalSourceEstablishedAnnouncer established == remote
  _ -> False

terminalSourceControlCoordinates ::
  PeerControl -> Maybe TerminalSourceCoordinates
terminalSourceControlCoordinates control = case control of
  PeerTerminalSourceInventoryAdvertised inventory ->
    Just
      ( TerminalSourceCoordinates
          (TerminalSource.terminalSourceInventoryPredecessorGenerationId inventory)
          (TerminalSource.terminalSourceInventorySuccessorGenerationId inventory)
          (Set.singleton (TerminalSource.terminalSourceInventoryRetiredSource inventory))
      )
  PeerTerminalSourcePayloadRequested request ->
    Just
      ( TerminalSourceCoordinates
          (TerminalSource.terminalSourcePayloadRequestPredecessorGenerationId request)
          (TerminalSource.terminalSourcePayloadRequestSuccessorGenerationId request)
          (Set.singleton (TerminalSource.terminalSourcePayloadRequestRetiredSource request))
      )
  PeerTerminalSourcePayloadRelayed relay ->
    Just
      ( TerminalSourceCoordinates
          (TerminalSource.terminalSourcePayloadRelayPredecessorGenerationId relay)
          (TerminalSource.terminalSourcePayloadRelaySuccessorGenerationId relay)
          (Set.singleton (TerminalSource.terminalSourcePayloadRelayRetiredSource relay))
      )
  PeerTerminalSourceUnionAnnounced announce ->
    Just (terminalSourceUnionCoordinates (TerminalSource.terminalSourceUnionAnnounceUnion announce))
  PeerTerminalSourceUnionAccepted acceptance ->
    Just
      ( TerminalSourceCoordinates
          (TerminalSource.terminalSourceUnionAcceptancePredecessorGenerationId acceptance)
          (TerminalSource.terminalSourceUnionAcceptanceSuccessorGenerationId acceptance)
          (TerminalSource.terminalSourceUnionAcceptanceRetiredSources acceptance)
      )
  PeerTerminalSourceUnionEstablished established ->
    Just
      ( terminalSourceUnionCoordinates
          (TerminalSource.terminalSourceUnionEstablishedUnion established)
      )
  _ -> Nothing

terminalSourceUnionCoordinates ::
  TerminalSource.TerminalSourceUnion -> TerminalSourceCoordinates
terminalSourceUnionCoordinates union =
  TerminalSourceCoordinates
    (TerminalSource.terminalSourceUnionPredecessorGenerationId union)
    (TerminalSource.terminalSourceUnionSuccessorGenerationId union)
    (TerminalSource.terminalSourceUnionRetiredSources union)

terminalSourceEstablishedAnnouncer ::
  TerminalSource.TerminalSourceUnionEstablished -> HeraldEpoch
terminalSourceEstablishedAnnouncer =
  minimum
    . fmap fst
    . structuralVersionVectorEntries
    . TerminalSource.terminalSourceUnionSuccessorInitialVector
    . TerminalSource.terminalSourceUnionEstablishedUnion
