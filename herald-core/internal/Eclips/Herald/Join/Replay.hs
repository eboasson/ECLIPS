-- | Replay one retained structural occurrence through the shared canonical
-- historical authority and structural-owner transaction. Onboarding supplies
-- control and topology prerequisites before retrying a held occurrence.
module Eclips.Herald.Join.Replay
  ( JoinReplayProblem (..),
    replayJoinStructuralOccurrence,
  )
where

import Eclips.Herald.Graph.TerminalSource (TerminalStructuralOccurrence)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentState,
    replaceStartupControlledState,
    replaceStartupGraphState,
    replaceStartupPlacementState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDisappearanceState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupLabelBarrierState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
    startupWaitState,
  )
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput

newtype JoinReplayProblem = JoinReplayPublicationProblem PeerInput.PeerInputProblem
  deriving stock (Eq, Show)

replayJoinStructuralOccurrence :: TerminalStructuralOccurrence -> HeraldState -> Either JoinReplayProblem (Maybe HeraldState)
replayJoinStructuralOccurrence occurrence state = do
  let context =
        PeerInput.peerInputContext
          (startupGenesis state)
          (startupOracleProjectionState state)
          (startupDiscoveryState state)
          (startupPlacementState state)
      owners =
        PeerInput.peerInputState
          (startupPublicationState state)
          (startupPeerStreamState state)
          (startupStoreState state)
          (startupGraphState state)
          (startupIdGeneratorState state)
          (startupStructuralProgressState state)
          (startupAlignmentState state)
          (startupPlacementState state)
          (startupControlledState state)
          (startupSortRegistryState state)
          (startupApplicationState state)
          (startupWaitState state)
          (startupLabelBarrierState state)
          (startupDisappearanceState state)
  replayed <-
    either
      (Left . JoinReplayPublicationProblem)
      Right
      (PeerInput.replayHistoricalStructuralOccurrence context occurrence owners)
  pure
    $ fmap
      ( \applied ->
          replaceStartupStoreState (PeerInput.peerInputStoreState applied)
            . replaceStartupGraphState (PeerInput.peerInputGraphState applied)
            . replaceStartupStructuralProgressState (PeerInput.peerInputStructuralProgressState applied)
            . replaceStartupAlignmentState (PeerInput.peerInputAlignmentState applied)
            . replaceStartupPlacementState (PeerInput.peerInputPlacementState applied)
            . replaceStartupControlledState (PeerInput.peerInputControlledState applied)
            $ state
      )
      replayed
