module PublicRuntimeFacade
  ( EffectArm (..),
    InputArm (..),
    classifyEveryEffect,
    classifyEveryInput,
    registerEveryPlane,
    submitRegisteredApplication,
    openRegisteredPeer,
    routeScheduledTicket,
    sameScheduledDestination,
    submitSelectedPublication,
    observePeerOutcome,
    configurationWithExplicitGeneratorSeedSource,
  )
where

import Data.ByteString (ByteString)
import Data.Set (Set)
import Eclips.Herald.Discovery
  ( PeerAddress,
    PeerBinding,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (..),
    PeerIngress (PeerDispatchSelected),
    RuntimeObservation (PeerDispatchObserved),
  )
import Eclips.Herald.OracleClient (OracleContactSet)
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome (PeerDispatchWritten),
    peerDispatchAttemptItem,
    peerDispatchTicketDestinationHeraldEpoch,
  )
import Eclips.Herald.Runtime
  ( ApplicationRecoveryConfiguration,
    HeraldRuntime,
    HeraldRuntimeConfiguration,
    PeerRecoveryConfiguration,
    RuntimeMonotonicClock,
    RuntimePeerWorkDelay,
    heraldRuntimeConfiguration,
    runtimeGeneratorSeedSource,
  )
import Eclips.Herald.Runtime.Connection
  ( ApplicationPlane,
    ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler
  ( HeraldRuntimeHandlers,
    RuntimeLaneOffer (LaneOffered),
    runtimeAdministrationConnectionHandlers,
    runtimeApplicationConnectionHandlers,
    runtimePeerConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeAdministrationRegistration,
    RuntimeRegistration (..),
    RuntimeSubmission (..),
    openPeerCandidate,
    registerAdministrationConnection,
    registerApplicationConnection,
    registerPeerConnection,
    submitApplicationDto,
    submitPeerPublication,
  )
import Eclips.Protocol.Application.Types (ApplicationClientDto)

-- These two closed views deliberately carry no owner-private payload.  Their
-- exhaustive matches are a downstream compile proof that a public runtime can
-- account for the complete current kernel boundary without importing the
-- private Herald component.
data EffectArm
  = ApplicationDispositionEffect
  | ApplicationAdmissionDeferredEffect
  | ApplicationRejectionEffect
  | ApplicationReplyEffect
  | ApplicationWakeEffect
  | ApplicationIsolationBegunEffect
  | ApplicationLifecycleReplyEffect
  | InitialClaimPendingEffect
  | InitialClaimRejectionEffect
  | ApplicationSessionDisposalEffect
  | AdministrationDispositionEffect
  | AdministrationRejectionEffect
  | AdministrationReplyEffect
  | PeerCandidateEffect
  | PeerCandidateRejectedEffect
  | PeerDispositionEffect
  | PeerControlEffect
  | PeerScheduleEffect
  | PeerItemEffect
  | PeerRejectionEffect
  | PeerDialEffect
  | PeerBindingCloseEffect
  | PeerDialCancellationEffect
  | PeerDestinationCancellationEffect
  | OracleClientEffect
  | OracleHealthEffect
  | JoinReplyEffect
  | TimerArmEffect
  | TimerCancelEffect
  | BeginDrainEffect
  | FinishDrainEffect
  | FinishIsolationEffect

data InputArm
  = ApplicationSessionInputArm
  | ApplicationLifecycleInputArm
  | ApplicationRequestInputArm
  | ApplicationReceiptRetirementInputArm
  | AdministrationInputArm
  | DisappearanceInputArm
  | JoinInputArm
  | PeerInputArm
  | OracleClientInputArm
  | RuntimeObservationInputArm

classifyEveryEffect :: HeraldEffect -> EffectArm
classifyEveryEffect = \case
  SetApplicationConnectionDisposition {} -> ApplicationDispositionEffect
  DeferApplicationSession {} -> ApplicationAdmissionDeferredEffect
  RejectApplicationConnection {} -> ApplicationRejectionEffect
  SendApplicationReply {} -> ApplicationReplyEffect
  SendApplicationWaitWake {} -> ApplicationWakeEffect
  SendApplicationIsolationBegun {} -> ApplicationIsolationBegunEffect
  SendApplicationLifecycleReply {} -> ApplicationLifecycleReplyEffect
  SendInitialClaimPending {} -> InitialClaimPendingEffect
  RejectInitialClaim {} -> InitialClaimRejectionEffect
  DisposeApplicationSession {} -> ApplicationSessionDisposalEffect
  SetAdministrationConnectionDisposition {} -> AdministrationDispositionEffect
  RejectAdministrationConnection {} -> AdministrationRejectionEffect
  SendAdministrationReply {} -> AdministrationReplyEffect
  SendPeerCandidate {} -> PeerCandidateEffect
  RejectPeerCandidateOpened {} -> PeerCandidateRejectedEffect
  SetPeerCandidateDisposition {} -> PeerDispositionEffect
  QueuePeerAlignmentEvidence {} -> PeerControlEffect
  SendPeerControl {} -> PeerControlEffect
  SendPeerControlWithProgress {} -> PeerControlEffect
  SchedulePeerDispatch {} -> PeerScheduleEffect
  SendPeerItem {} -> PeerItemEffect
  SendPeerItemWithProgress {} -> PeerItemEffect
  RejectPeerConnection {} -> PeerRejectionEffect
  DialPeer {} -> PeerDialEffect
  ClosePeerBinding {} -> PeerBindingCloseEffect
  CancelPeerDial {} -> PeerDialCancellationEffect
  CancelPeerDestination {} -> PeerDestinationCancellationEffect
  RunOracleClientAction {} -> OracleClientEffect
  RunOracleHealthRound {} -> OracleHealthEffect
  SendJoinReply {} -> JoinReplyEffect
  ArmTimer {} -> TimerArmEffect
  CancelTimer {} -> TimerCancelEffect
  BeginDrain {} -> BeginDrainEffect
  FinishDrain {} -> FinishDrainEffect
  FinishIsolation {} -> FinishIsolationEffect

classifyEveryInput :: HeraldInputBody -> InputArm
classifyEveryInput = \case
  ApplicationSessionInput {} -> ApplicationSessionInputArm
  ApplicationLifecycleInput {} -> ApplicationLifecycleInputArm
  ApplicationRequestInput {} -> ApplicationRequestInputArm
  ApplicationReceiptRetirementInput {} -> ApplicationReceiptRetirementInputArm
  AdministrationInput {} -> AdministrationInputArm
  DisappearanceInput {} -> DisappearanceInputArm
  JoinInput {} -> JoinInputArm
  PeerInput {} -> PeerInputArm
  OracleInput {} -> OracleClientInputArm
  RuntimeObserved {} -> RuntimeObservationInputArm

registerEveryPlane ::
  HeraldRuntime ->
  IO
    ( RuntimeRegistration ApplicationPlane,
      RuntimeAdministrationRegistration,
      RuntimeRegistration PeerPlane
    )
registerEveryPlane runtime = do
  application <-
    registerApplicationConnection
      runtime
      (runtimeApplicationConnectionHandlers (const (pure LaneOffered)) (pure ()))
  administration <-
    registerAdministrationConnection
      runtime
      (runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
  peer <-
    registerPeerConnection
      runtime
      ( runtimePeerConnectionHandlers
          (\_ _ -> pure LaneOffered)
          (\_ _ -> pure LaneOffered)
          (\_ _ -> pure LaneOffered)
          (\_ _ -> pure LaneOffered)
          (\_ _ _ -> pure PeerDispatchWritten)
          (pure ())
      )
  pure (application, administration, peer)

submitRegisteredApplication ::
  HeraldRuntime ->
  RuntimeRegistration ApplicationPlane ->
  ApplicationClientDto ->
  IO RuntimeSubmission
submitRegisteredApplication runtime registration dto = case registration of
  ConnectionRegistered reference -> submitApplicationDto runtime reference dto
  RegistrationStopped -> pure RuntimeStopped

openRegisteredPeer ::
  HeraldRuntime ->
  RuntimeRegistration PeerPlane ->
  Set PeerAddress ->
  IO RuntimeSubmission
openRegisteredPeer runtime registration addresses = case registration of
  ConnectionRegistered reference -> openPeerCandidate runtime reference addresses Nothing
  RegistrationStopped -> pure RuntimeStopped

routeScheduledTicket :: PeerBinding -> HeraldEffect -> Maybe HeraldInputBody
routeScheduledTicket binding effect = case effect of
  SchedulePeerDispatch ticket ->
    Just (PeerInput (PeerDispatchSelected ticket binding))
  _ -> Nothing

sameScheduledDestination :: HeraldEffect -> HeraldEffect -> Bool
sameScheduledDestination left right = case (left, right) of
  (SchedulePeerDispatch leftTicket, SchedulePeerDispatch rightTicket) ->
    peerDispatchTicketDestinationHeraldEpoch leftTicket
      == peerDispatchTicketDestinationHeraldEpoch rightTicket
  _ -> False

submitSelectedPublication ::
  HeraldRuntime ->
  ConnectionRef PeerPlane ->
  PeerBinding ->
  HeraldEffect ->
  IO RuntimeSubmission
submitSelectedPublication runtime reference binding effect = case effect of
  SendPeerItem _ attempt ->
    submitPeerPublication runtime reference binding (peerDispatchAttemptItem attempt)
  _ -> pure RuntimeStopped

observePeerOutcome :: HeraldEffect -> Maybe HeraldInputBody
observePeerOutcome effect = case effect of
  SendPeerItem _ attempt ->
    Just
      ( RuntimeObserved
          (PeerDispatchObserved attempt PeerDispatchWritten)
      )
  _ -> Nothing

configurationWithExplicitGeneratorSeedSource ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  ByteString ->
  RuntimeMonotonicClock ->
  RuntimePeerWorkDelay ->
  HeraldRuntimeHandlers ->
  ApplicationRecoveryConfiguration ->
  PeerRecoveryConfiguration ->
  HeraldRuntimeConfiguration
configurationWithExplicitGeneratorSeedSource genesis bootstraps contacts seed clock delay handlers applicationRecovery peerRecovery =
  heraldRuntimeConfiguration
    genesis
    bootstraps
    contacts
    (runtimeGeneratorSeedSource (pure seed))
    clock
    delay
    handlers
    applicationRecovery
    peerRecovery
