-- | A complete ordered batch of effects emitted by one Herald transition.
module Eclips.Herald.EffectBatch
  ( ApplicationDispositionTarget (..),
    AdministrationDispositionTarget (..),
    PeerProtocolDisposition (..),
    HeraldEffect (..),
    EffectBatch,
    emptyEffectBatch,
    singletonEffectBatch,
    orderedEffectBatch,
    effectBatchMembers,
    effectBatchIsEmpty,
    effectBatchMemberCount,
  )
where

import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (InitialClaimId, LifecycleReply, StartupError)
import Eclips.Application.Types.Result (WaitResult)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Administration
  ( AdminReply,
    AdministrationBinding,
    DrainId,
    FinalAdminReply,
  )
import Eclips.Herald.Alignment.Protocol (AlignmentControl)
import Eclips.Herald.Application.Request
  ( ApplicationReplyCursor,
    ApplicationRequestReply,
    RequestId,
    WaitId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionRejection,
    ApplicationSessionUnavailableReason,
  )
import Eclips.Herald.Discovery
  ( ConnectionNonce,
    PeerBinding,
    PeerCandidate,
    PeerDialIntent,
    PeerHello,
    PeerHelloDisposition,
  )
import Eclips.Herald.Input
  ( CandidateAdministrationLane,
    CandidateApplicationLane,
    PeerControl,
  )
import Eclips.Herald.Isolation (IsolationDrainId)
import Eclips.Herald.OracleClient (OracleClientAction, OracleContact, OracleHelloClaims)
import Eclips.Herald.PeerDispatch
  ( PeerDispatchTicket,
    PeerLogicalAttempt,
  )
import Eclips.Herald.Timer (TimerAttempt, TimerSpec)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)

-- | Logical target for a final application connection rejection.
data ApplicationDispositionTarget
  = CandidateApplicationDisposition CandidateApplicationLane
  | EstablishedApplicationDisposition ApplicationSessionBinding
  deriving stock (Eq, Show)

-- | Logical target for a Start-administration connection rejection.
data AdministrationDispositionTarget
  = CandidateAdministrationDisposition CandidateAdministrationLane
  | EstablishedAdministrationDisposition AdministrationBinding
  deriving stock (Eq, Show)

-- | Stateful peer-protocol failures close only the current logical binding;
-- checked owner contradictions still terminate as invariant faults.
data PeerProtocolDisposition
  = ClosePeerControlProtocol
  | ClosePeerPlacementProtocol
  | ClosePeerStreamProtocol
  | ClosePeerPublicationProtocol
  deriving stock (Eq, Ord, Show)

-- | The complete declarative effect vocabulary executable by the current kernel.
data HeraldEffect
  = RunOracleHealthRound Word64 Word64 OracleHelloClaims [OracleContact]
  | SetApplicationConnectionDisposition
      CandidateApplicationLane
      ApplicationSessionAcceptance
  | DeferApplicationSession CandidateApplicationLane
  | RejectApplicationConnection
      ApplicationDispositionTarget
      ApplicationSessionRejection
  | SendApplicationReply
      ApplicationSessionBinding
      ApplicationRequestReply
  | SendApplicationLifecycleReply
      ApplicationDispositionTarget
      LifecycleReply
  | SendInitialClaimPending
      CandidateApplicationLane
      InitialClaimId
      [Text]
  | RejectInitialClaim
      CandidateApplicationLane
      InitialClaimId
      StartupError
  | SendApplicationWaitWake
      ApplicationSessionBinding
      ApplicationReplyCursor
      RequestId
      WaitId
      WaitResult
  | SendApplicationIsolationBegun
      ApplicationSessionBinding
      ApplicationReplyCursor
      ApplicationSessionId
      ApplicationSessionUnavailableReason
  | DisposeApplicationSession
      ApplicationSessionId
      ApplicationSessionUnavailableReason
  | SetAdministrationConnectionDisposition
      CandidateAdministrationLane
      AdministrationBinding
  | RejectAdministrationConnection AdministrationDispositionTarget
  | SendAdministrationReply AdministrationBinding AdminReply
  | SendPeerCandidate
      PeerCandidate
      HeraldMembershipGenerationId
      MemberSetDigest
      PeerHello
  | RejectPeerCandidateOpened ConnectionNonce
  | SetPeerCandidateDisposition
      PeerCandidate
      PeerHelloDisposition
  | QueuePeerAlignmentEvidence HeraldEpoch AlignmentControl
  | SendPeerControl
      PeerBinding
      PeerControl
  | SendPeerControlWithProgress
      PeerBinding
      ReceiptRetirement
      PeerControl
  | DialPeer PeerDialIntent
  | ClosePeerBinding PeerBinding
  | CancelPeerDial PeerDialIntent
  | CancelPeerDestination HeraldEpoch
  | SchedulePeerDispatch PeerDispatchTicket
  | SendPeerItem
      PeerBinding
      PeerLogicalAttempt
  | SendPeerItemWithProgress
      PeerBinding
      ReceiptRetirement
      PeerLogicalAttempt
  | RejectPeerConnection
      PeerBinding
      PeerProtocolDisposition
  | RunOracleClientAction OracleClientAction
  | SendJoinReply Word64 ByteString
  | ArmTimer TimerAttempt TimerSpec
  | CancelTimer TimerAttempt
  | BeginDrain DrainId
  | FinishDrain DrainId (Maybe FinalAdminReply)
  | FinishIsolation IsolationDrainId
  deriving stock (Eq, Show)

-- | The complete, already ordered output of one kernel transition.
newtype EffectBatch = EffectBatch [HeraldEffect]
  deriving stock (Eq, Show)

instance Semigroup EffectBatch where
  EffectBatch left <> EffectBatch right =
    EffectBatch (suppressPeerSendsAfterRejection (left <> right))

instance Monoid EffectBatch where
  mempty = emptyEffectBatch

emptyEffectBatch :: EffectBatch
emptyEffectBatch = EffectBatch []

singletonEffectBatch :: HeraldEffect -> EffectBatch
singletonEffectBatch effect = EffectBatch [effect]

-- | Retain the caller's semantic order while enforcing the exact-binding
-- rejection cut described below.
orderedEffectBatch :: [HeraldEffect] -> EffectBatch
orderedEffectBatch = EffectBatch . suppressPeerSendsAfterRejection

-- | Read-only observation for interpreters and deterministic properties.
effectBatchMembers :: EffectBatch -> [HeraldEffect]
effectBatchMembers (EffectBatch effects) = effects

effectBatchIsEmpty :: EffectBatch -> Bool
effectBatchIsEmpty (EffectBatch effects) = null effects

effectBatchMemberCount :: EffectBatch -> Int
effectBatchMemberCount (EffectBatch effects) = length effects

-- | Once one atomic transition rejects a logical peer binding, retain the
-- rejection at its protocol-ordering slot but discard later direct sends on
-- that exact binding.  Logical dispatch and redial work remains durable for a
-- replacement binding; only the lane made immediately ineligible is filtered.
suppressPeerSendsAfterRejection :: [HeraldEffect] -> [HeraldEffect]
suppressPeerSendsAfterRejection = go Set.empty
  where
    go _ [] = []
    go rejected (effect : remaining) = case effect of
      RejectPeerConnection binding _ ->
        effect
          : go
            (Set.insert binding rejected)
            remaining
      SendPeerControl binding _
        | binding `Set.member` rejected ->
            go rejected remaining
      SendPeerControlWithProgress binding _ _
        | binding `Set.member` rejected ->
            go rejected remaining
      SendPeerItemWithProgress binding _ _
        | binding `Set.member` rejected ->
            go rejected remaining
      SendPeerItem binding _
        | binding `Set.member` rejected ->
            go rejected remaining
      _ -> effect : go rejected remaining
