{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Delivery bookkeeping is independent of retained alignment semantics.
module Eclips.Herald.PeerDelivery
  ( State,
    PeerState (..),
    EvidenceKey,
    evidenceKey,
    emptyState,
    peerState,
    replacePeer,
    retirePeer,
    peers,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Alignment
  ( ContextClassGenerationId,
  )
import Eclips.Domain.Identity (HeraldEpoch, StoreIncarnationId)
import Eclips.Herald.Alignment.Plan.Identity (AlignmentPlanId)
import Eclips.Herald.Alignment.Protocol
  ( AlignmentControl (..),
    alignmentCutAcceptedGeneration,
    alignmentCutAcceptedHerald,
    alignmentPlanAcceptedHerald,
    alignmentPlanAcceptedId,
    alignmentPlanObsoleteId,
    alignmentPlanObsoleteReporter,
    classMemberReadyGeneration,
    classMemberReadyStoreIncarnation,
  )
import Eclips.Herald.PeerDelivery.State qualified as Delivery
import Eclips.Herald.Timer.Internal
  ( TimerAttempt,
    TimerAttemptGeneration,
    TimerSpec,
    firstTimerAttemptGeneration,
  )

data EvidenceKey
  = PlanAcceptance AlignmentPlanId HeraldEpoch
  | Acceptance ContextClassGenerationId HeraldEpoch
  | Readiness ContextClassGenerationId StoreIncarnationId
  | PlanObsolete AlignmentPlanId HeraldEpoch
  deriving stock (Eq, Ord, Show)

evidenceKey :: AlignmentControl -> Maybe EvidenceKey
evidenceKey (AlignmentPlanAcceptanceAdvertised accepted) =
  Just (PlanAcceptance (alignmentPlanAcceptedId accepted) (alignmentPlanAcceptedHerald accepted))
evidenceKey (AlignmentPlanObsoleteAdvertised report) =
  Just (PlanObsolete (alignmentPlanObsoleteId report) (alignmentPlanObsoleteReporter report))
evidenceKey (AlignmentCutAcceptanceAdvertised accepted) =
  Just (Acceptance (alignmentCutAcceptedGeneration accepted) (alignmentCutAcceptedHerald accepted))
evidenceKey (AlignmentMemberReadyAdvertised ready) =
  Just (Readiness (classMemberReadyGeneration ready) (classMemberReadyStoreIncarnation ready))
evidenceKey _ = Nothing

data PeerState = PeerState
  { delivery :: !(Delivery.State EvidenceKey AlignmentControl),
    seeded :: !Bool,
    dirty :: !Bool,
    timer :: !(Maybe (TimerAttempt, TimerSpec)),
    nextAttempt :: !TimerAttemptGeneration
  }
  deriving stock (Eq, Show)

newtype State = State (Map HeraldEpoch PeerState)
  deriving stock (Eq, Show)

emptyState :: State
emptyState = State Map.empty

peerState :: HeraldEpoch -> State -> PeerState
peerState peer (State states) = Map.findWithDefault initial peer states
  where
    initial = PeerState Delivery.emptyState False False Nothing firstTimerAttemptGeneration

replacePeer :: HeraldEpoch -> PeerState -> State -> State
replacePeer peer value (State states) = State (Map.insert peer value states)

retirePeer :: HeraldEpoch -> State -> State
retirePeer peer (State states) = State (Map.delete peer states)

peers :: State -> [(HeraldEpoch, PeerState)]
peers (State states) = Map.toAscList states
