-- | Closed current input vocabulary for the pure Herald transition.
--
-- These values are typed kernel inputs, not wire messages. Runtime-owned
-- connection facts are represented only by opaque logical correlations.
module Eclips.Herald.Input
  ( HeraldInput,
    heraldInput,
    inputObservedAt,
    inputBody,
    HeraldInputBody (..),
    CandidateApplicationLane,
    candidateApplicationLane,
    candidateApplicationLaneWord64,
    CandidateAdministrationLane,
    candidateAdministrationLane,
    candidateAdministrationLaneWord64,
    ApplicationSessionIngress (..),
    ApplicationRequestIngress (..),
    ApplicationLifecycleIngress (..),
    ApplicationReceiptRetirementIngress (..),
    ApplicationRetirementWork (..),
    AdministrationIngress (..),
    administrationIngressCorrelation,
    DisappearanceIngress (..),
    PeerIngress (..),
    peerPublicationReceived,
    PeerControl (..),
    AlignmentControl,
    RuntimeObservation (..),
  )
where

import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle
  ( ChildPreparation,
    ConnectionDescriptor,
    HeraldLocator,
    InitialClaimId,
    LifecycleCommand,
    LifecycleRequestId,
  )
import Eclips.Application.Types.Lifetime (ApplicationReceiptRetirement)
import Eclips.Application.Types.Operation (ApplicationOperation)
import Eclips.Domain.Alignment (AlignmentDeliverySequence)
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Identity (HeraldEpoch, SystemId)
import Eclips.Domain.Label (LabelInstallationReport)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Administration
  ( AdminCorrelationId,
    AdministrationBinding,
    DrainId,
    EndProcessEpochRequest,
    StartProcessRequest,
    endProcessEpochCorrelation,
    startProcessCorrelation,
  )
import Eclips.Herald.Alignment.Protocol (AlignmentControl)
import Eclips.Herald.Application.Request
  ( ApplicationReplyCursor,
    RequestId,
    WaitId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ClientNonce,
  )
import Eclips.Herald.Discovery
  ( KnownHerald,
    PeerAddress,
    PeerBinding,
    PeerCandidate,
    PeerCandidateOpened,
    PeerHello,
  )
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    TopologyCutAcceptance,
    TopologyCutAnnounce,
    TopologyCutEstablished,
    TopologyCutEstablishedAck,
  )
import Eclips.Herald.Graph.TerminalSource
  ( TerminalSourceInventory,
    TerminalSourcePayloadRelay,
    TerminalSourcePayloadRequest,
    TerminalSourceUnionAcceptance,
    TerminalSourceUnionAnnounce,
    TerminalSourceUnionEstablished,
  )
import Eclips.Herald.OracleClient (OracleClientIngress)
import Eclips.Herald.OracleHealth (OracleHealthObservation)
import Eclips.Herald.Peer.Step15
  ( DirectFailureProbeRequest,
    DirectFailureProbeResponse,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome,
    PeerDispatchTicket,
    PeerLogicalAttempt,
    PeerLogicalItem,
  )
import Eclips.Herald.PeerStream
  ( ResumeOffer,
    ResumeResponse,
    StreamDirection,
    StreamPrefix,
    StreamSequence,
  )
import Eclips.Herald.Placement
  ( PlacementAcknowledgement,
    PlacementUpdate,
  )
import Eclips.Herald.ProcessPreparation.Protocol (PreparationControl)
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Herald.Timer (TimerAttempt, TimerOutcome)
import Eclips.Oracle.Voter (ExplicitVoterChangeReason, OracleVoterBindings, VoterChangeId, VoterConfigurationId)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)

-- | One runtime-minted candidate application delivery lane.
--
-- The Herald passes this target through to a disposition effect. It neither
-- interprets the bits nor stores the candidate as session authority.
newtype CandidateApplicationLane = CandidateApplicationLane Word64
  deriving stock (Eq, Ord, Show)

candidateApplicationLane :: Word64 -> CandidateApplicationLane
candidateApplicationLane = CandidateApplicationLane

candidateApplicationLaneWord64 :: CandidateApplicationLane -> Word64
candidateApplicationLaneWord64 (CandidateApplicationLane candidate) = candidate

-- | One runtime-minted candidate Start-administration delivery lane.
--
-- It is distinct from the package-private generation-zero orderly-shutdown
-- lane and becomes authority only after a checked 'AdminHello'.
newtype CandidateAdministrationLane = CandidateAdministrationLane Word64
  deriving stock (Eq, Ord, Show)

candidateAdministrationLane :: Word64 -> CandidateAdministrationLane
candidateAdministrationLane = CandidateAdministrationLane

candidateAdministrationLaneWord64 :: CandidateAdministrationLane -> Word64
candidateAdministrationLaneWord64
  (CandidateAdministrationLane candidate) = candidate

-- | Session-control application ingress introduced in Step 4.
data ApplicationSessionIngress
  = OpenApplicationSession
      CandidateApplicationLane
      ApplicationAttachment
      ClientNonce
  | ResumeApplicationSession
      CandidateApplicationLane
      ApplicationSessionId
      ApplicationResumeToken
      ApplicationReplyCursor
  | EndApplicationSession
      ApplicationSessionBinding
      ApplicationSessionId
  | ClaimInitialApplicationSession
      CandidateApplicationLane
      ConnectionDescriptor
      InitialClaimId
      (Maybe HeraldLocator)
  deriving stock (Eq, Show)

-- | Request bookkeeping and the complete regular application-call family.
-- The separately decoded session ID is checked against the established binding.
data ApplicationRequestIngress
  = CallApplicationRequest
      ApplicationSessionBinding
      ApplicationSessionId
      RequestId
      ApplicationOperation
  | GetApplicationRequestResult
      ApplicationSessionBinding
      ApplicationSessionId
      RequestId
  | CancelApplicationPendingWait
      ApplicationSessionBinding
      ApplicationSessionId
      RequestId
      WaitId
  deriving stock (Eq, Show)

-- | Session-scoped lifecycle work has its own retained request namespace.
-- The local locator is an explicit runtime observation of the serving listener.
data ApplicationLifecycleIngress
  = CallApplicationLifecycle
      ApplicationSessionBinding
      ApplicationSessionId
      LifecycleRequestId
      HeraldLocator
      LifecycleCommand
  | GetApplicationLifecycleResult
      ApplicationSessionBinding
      ApplicationSessionId
      LifecycleRequestId
  | RecoverApplicationLifecycleResult
      CandidateApplicationLane
      ApplicationAttachment
      LifecycleRequestId
  deriving stock (Eq, Show)

-- | One established-lane retirement announcement, optionally carried by new
-- semantic work. Admission composes both receipt owners before that work runs.
data ApplicationReceiptRetirementIngress
  = RetireApplicationReceipts
      ApplicationSessionBinding
      ApplicationSessionId
      ApplicationReceiptRetirement
      (Maybe ApplicationRetirementWork)
  deriving stock (Eq, Show)

data ApplicationRetirementWork
  = RetiringApplicationRequest RequestId ApplicationOperation
  | RetiringApplicationLifecycle LifecycleRequestId HeraldLocator LifecycleCommand
  deriving stock (Eq, Show)

-- | Closed Start/End administration ingress plus the private orderly-shutdown
-- command. The shutdown constructor is never admitted by the public admin RPC
-- adapter.
data AdministrationIngress
  = OpenAdministrationConnection CandidateAdministrationLane SystemId
  | RetireAdministrationReceipts AdministrationBinding ReceiptRetirement (Maybe AdministrationIngress)
  | StartProcessEpoch
      AdministrationBinding
      StartProcessRequest
  | EndProcessEpoch
      AdministrationBinding
      EndProcessEpochRequest
  | CancelChildPreparation
      AdministrationBinding
      AdminCorrelationId
      ChildPreparation
  | GetAdministrationResult
      AdministrationBinding
      AdminCorrelationId
  | PrepareOracleReplica AdministrationBinding AdminCorrelationId
  | BeginVoterChange AdministrationBinding AdminCorrelationId VoterConfigurationId ExplicitVoterChangeReason OracleVoterBindings
  | CancelVoterChange AdministrationBinding AdminCorrelationId VoterChangeId
  | GetOracleConfiguration AdministrationBinding AdminCorrelationId
  | GetVoterChangeStatus AdministrationBinding AdminCorrelationId VoterChangeId
  | GetHeraldStatus AdministrationBinding AdminCorrelationId
  | ListChildPreparations AdministrationBinding AdminCorrelationId
  | DrainConfiguredHerald AdministrationBinding AdminCorrelationId
  | OrderlyHeraldShutdown
      AdministrationBinding
      AdminCorrelationId
  deriving stock (Eq, Show)

administrationIngressCorrelation :: AdministrationIngress -> Maybe (AdministrationBinding, AdminCorrelationId)
administrationIngressCorrelation = \case
  OpenAdministrationConnection {} -> Nothing
  RetireAdministrationReceipts {} -> Nothing
  StartProcessEpoch binding request -> Just (binding, startProcessCorrelation request)
  EndProcessEpoch binding request -> Just (binding, endProcessEpochCorrelation request)
  CancelChildPreparation binding correlation _ -> Just (binding, correlation)
  GetAdministrationResult binding correlation -> Just (binding, correlation)
  PrepareOracleReplica binding correlation -> Just (binding, correlation)
  BeginVoterChange binding correlation _ _ _ -> Just (binding, correlation)
  CancelVoterChange binding correlation _ -> Just (binding, correlation)
  GetOracleConfiguration binding correlation -> Just (binding, correlation)
  GetVoterChangeStatus binding correlation _ -> Just (binding, correlation)
  GetHeraldStatus binding correlation -> Just (binding, correlation)
  ListChildPreparations binding correlation -> Just (binding, correlation)
  DrainConfiguredHerald binding correlation -> Just (binding, correlation)
  OrderlyHeraldShutdown {} -> Nothing

-- | Explicit package-internal management authority, never produced by a
-- timeout, connection observer, or public application/admin decoder.
data DisappearanceIngress
  = AbortDisappearanceProbe DisappearanceProbeId
  deriving stock (Eq, Show)

-- | Established-binding control messages. These are typed kernel values, not
-- their later Step-10 wire representation.
data PeerControl
  = PeerKnownHeralds [KnownHerald]
  | PeerPlacementUpdate PlacementUpdate
  | PeerPlacementAcknowledged PlacementAcknowledgement
  | PeerStreamReceived StreamDirection StreamPrefix
  | PeerStreamCompleted StreamDirection ReceiptRetirement
  | PeerStreamFrontierAdvanced StreamDirection StreamSequence
  | PeerStreamResumeOffered ResumeOffer
  | PeerStreamResumeAccepted ResumeResponse
  | PeerStructuralAppliedReported StructuralAppliedReport
  | PeerTopologyCutAnnounced TopologyCutAnnounce
  | PeerTopologyCutAccepted TopologyCutAcceptance
  | PeerTopologyCutEstablished TopologyCutEstablished
  | PeerTopologyCutEstablishedAcknowledged TopologyCutEstablishedAck
  | PeerAlignmentControl AlignmentControl
  | PeerAlignmentEvidenceDelivered AlignmentDeliverySequence AlignmentControl
  | PeerAlignmentDeliveryProgress
  | PeerPreparationControl PreparationControl
  | PeerDirectFailureProbeRequested DirectFailureProbeRequest
  | PeerDirectFailureProbeResponded DirectFailureProbeResponse
  | PeerTerminalSourceInventoryAdvertised TerminalSourceInventory
  | PeerTerminalSourcePayloadRequested TerminalSourcePayloadRequest
  | PeerTerminalSourcePayloadRelayed TerminalSourcePayloadRelay
  | PeerTerminalSourceUnionAnnounced TerminalSourceUnionAnnounce
  | PeerTerminalSourceUnionAccepted TerminalSourceUnionAcceptance
  | PeerTerminalSourceUnionEstablished TerminalSourceUnionEstablished
  | PeerLabelInstalled LabelInstallationReport
  deriving stock (Eq, Show)

-- | Candidate, established-control, stream-item, and dispatch ingress for the
-- Step-7 peer vertical.
data PeerIngress
  = PeerCandidateOpened
      PeerCandidateOpened
  | PeerHelloReceived
      PeerCandidate
      (Set PeerAddress)
      PeerHello
      HeraldMembershipGenerationId
      MemberSetDigest
      (Maybe HeraldLocator)
  | PeerControlReceived
      PeerBinding
      PeerControl
  | PeerControlReceivedWithProgress
      PeerBinding
      ReceiptRetirement
      PeerControl
  | PeerPublicationReceivedWithProgress
      PeerBinding
      ReceiptRetirement
      PeerLogicalItem
  | PeerPublicationReceived
      PeerBinding
      PeerLogicalItem
  | PeerDispatchSelected
      PeerDispatchTicket
      PeerBinding
  deriving stock (Eq)

instance Show PeerIngress where
  showsPrec precedence ingress = showParen (precedence > 10) $ case ingress of
    PeerCandidateOpened opened ->
      showString "PeerCandidateOpened " . showsPrec 11 opened
    PeerHelloReceived candidate addresses hello generation members locator ->
      showString "PeerHelloReceived "
        . showsPrec 11 candidate
        . showChar ' '
        . showsPrec 11 addresses
        . showChar ' '
        . showsPrec 11 hello
        . showChar ' '
        . showsPrec 11 generation
        . showChar ' '
        . showsPrec 11 members
        . showChar ' '
        . showsPrec 11 locator
    PeerControlReceived binding control ->
      showString "PeerControlReceived "
        . showsPrec 11 binding
        . showChar ' '
        . showsPrec 11 control
    PeerControlReceivedWithProgress binding progress control ->
      showString "PeerControlReceivedWithProgress "
        . showsPrec 11 binding
        . showChar ' '
        . showsPrec 11 progress
        . showChar ' '
        . showsPrec 11 control
    PeerPublicationReceivedWithProgress binding progress _ ->
      showString "PeerPublicationReceivedWithProgress "
        . showsPrec 11 binding
        . showChar ' '
        . showsPrec 11 progress
        . showString " PeerLogicalItem"
    PeerPublicationReceived binding _ ->
      showString "PeerPublicationReceived "
        . showsPrec 11 binding
        . showString " PeerLogicalItem"
    PeerDispatchSelected ticket binding ->
      showString "PeerDispatchSelected "
        . showsPrec 11 ticket
        . showChar ' '
        . showsPrec 11 binding

-- | Relay one exact opaque in-memory publication item into an established peer
-- binding.  The item cannot be inspected or constructed by a public caller.
peerPublicationReceived :: PeerBinding -> PeerLogicalItem -> PeerIngress
peerPublicationReceived = PeerPublicationReceived

-- | Runtime facts correlated with already established logical state.
data RuntimeObservation
  = OracleHealthRoundObserved Word64 [OracleHealthObservation]
  | ApplicationBindingLost ApplicationSessionBinding
  | ApplicationCandidateLost CandidateApplicationLane
  | AdministrationBindingLost AdministrationBinding
  | DrainBarrierObserved DrainId
  | PeerBindingLost PeerBinding
  | RetiredLocalHeraldEpochRejected HeraldEpoch HeraldMembershipGenerationId
  | PeerDispatchObserved
      PeerLogicalAttempt
      PeerDispatchOutcome
  | TimerObserved TimerAttempt TimerOutcome
  deriving stock (Eq, Show)

-- | Source-separated input body for the current slice.
data HeraldInputBody
  = ApplicationSessionInput ApplicationSessionIngress
  | ApplicationRequestInput ApplicationRequestIngress
  | ApplicationLifecycleInput ApplicationLifecycleIngress
  | ApplicationReceiptRetirementInput ApplicationReceiptRetirementIngress
  | AdministrationInput AdministrationIngress
  | DisappearanceInput DisappearanceIngress
  | PeerInput PeerIngress
  | OracleInput OracleClientIngress
  | JoinInput Word64 ByteString
  | RuntimeObserved RuntimeObservation
  deriving stock (Eq, Show)

-- | One selected input stamped immediately before the exclusive pure step.
data HeraldInput = HeraldInput MonotonicInstant HeraldInputBody
  deriving stock (Eq, Show)

heraldInput :: MonotonicInstant -> HeraldInputBody -> HeraldInput
heraldInput = HeraldInput

inputObservedAt :: HeraldInput -> MonotonicInstant
inputObservedAt (HeraldInput observedAt _) = observedAt

inputBody :: HeraldInput -> HeraldInputBody
inputBody (HeraldInput _ body) = body
