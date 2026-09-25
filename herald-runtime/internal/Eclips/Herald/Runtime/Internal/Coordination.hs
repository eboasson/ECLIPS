-- | Small typed coordination values shared by the owner and dispatcher.
module Eclips.Herald.Runtime.Internal.Coordination
  ( StepOrdinal (..),
    BatchOrdinal (..),
    EffectMemberOrdinal (..),
    CompletionOrdinal (..),
    BatchEnvelope (..),
    RuntimePhase (..),
    IngressEnvelope (..),
    CompletionCorrelation (..),
    DispositionExpectation (..),
    DispositionRoute (..),
    TransferBinding (..),
    dispositionExpectationFor,
    effectMatchesDisposition,
    validateDispositionBatch,
    ObligationOrigin (..),
    ObligationState (..),
    DrainLedger,
    emptyDrainLedger,
    registerObligation,
    settleObligationByOffer,
    awaitPhysicalOutcome,
    queueObligationObservation,
    markObligationKernelStepped,
    advanceRoutedThrough,
    sealDrainLedger,
    outstandingPhysicalOutcomes,
    drainBarrierReady,
    drainLedgerObligations,
    OutcomeCellState (..),
    claimOutcomeCell,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Administration
  ( AdministrationBinding,
    DrainId,
  )
import Eclips.Herald.Application.Session (ApplicationSessionBinding)
import Eclips.Herald.Discovery
  ( ConnectionNonce,
    PeerAddress,
    PeerBinding,
    PeerCandidate,
    PeerHello,
    peerCandidateOpenedConnectionNonce,
    peerHelloConnectionNonce,
  )
import Eclips.Herald.EffectBatch
  ( AdministrationDispositionTarget (..),
    ApplicationDispositionTarget (..),
    EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( AdministrationIngress (..),
    ApplicationLifecycleIngress (..),
    ApplicationSessionIngress (..),
    CandidateAdministrationLane,
    CandidateApplicationLane,
    HeraldInputBody (..),
    PeerControl,
    PeerIngress (..),
    RuntimeObservation,
  )
import Eclips.Herald.OracleClient (OracleClientIngress)
import Eclips.Herald.PeerDispatch (PeerDispatchOutcome (..), PeerLogicalItem)
import Eclips.Herald.Runtime.Internal.Arbiter (SourceId)
import Eclips.Herald.Runtime.Internal.Types
  ( ApplicationPlane,
    ConfiguredAdministrationPlane,
    ConnectionRef,
    HeraldRuntimeExit,
    PeerPlane,
  )
import Eclips.Protocol.Admin.Types (AdminClientDto)
import Eclips.Protocol.Application.Types (ApplicationClientDto)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)

newtype StepOrdinal = StepOrdinal Word64
  deriving stock (Eq, Ord, Show)

newtype BatchOrdinal = BatchOrdinal Word64
  deriving stock (Eq, Ord, Show)

newtype EffectMemberOrdinal = EffectMemberOrdinal Word64
  deriving stock (Eq, Ord, Show)

newtype CompletionOrdinal = CompletionOrdinal Word64
  deriving stock (Eq, Ord, Show)

data BatchEnvelope = BatchEnvelope
  { batchEnvelopeOrdinal :: BatchOrdinal,
    batchEnvelopeEffects :: EffectBatch,
    batchEnvelopeDisposition :: Maybe DispositionExpectation,
    batchEnvelopeRoute :: Maybe DispositionRoute
  }
  deriving stock (Eq, Show)

data RuntimePhase
  = RuntimeStarting
  | RuntimeRunning
  | RuntimeDraining DrainId
  | RuntimeStoppedPhase HeraldRuntimeExit
  deriving stock (Eq, Show)

-- | Immutable work accepted by one runtime source FIFO.
data IngressEnvelope
  = ApplicationEnvelope
      (ConnectionRef ApplicationPlane)
      ApplicationClientDto
  | ConfiguredAdministrationEnvelope
      (ConnectionRef ConfiguredAdministrationPlane)
      AdminClientDto
  | PeerOpenEnvelope
      (ConnectionRef PeerPlane)
      (Set PeerAddress)
      (Maybe HeraldLocator)
  | PeerHelloEnvelope
      (ConnectionRef PeerPlane)
      PeerCandidate
      (Set PeerAddress)
      PeerHello
      HeraldMembershipGenerationId
      MemberSetDigest
      (Maybe HeraldLocator)
  | PeerControlEnvelope
      (ConnectionRef PeerPlane)
      PeerBinding
      PeerControl
  | PeerPublicationEnvelope
      (ConnectionRef PeerPlane)
      PeerBinding
      PeerLogicalItem
  | PeerControlWithProgressEnvelope
      (ConnectionRef PeerPlane)
      PeerBinding
      ReceiptRetirement
      PeerControl
  | PeerPublicationWithProgressEnvelope
      (ConnectionRef PeerPlane)
      PeerBinding
      ReceiptRetirement
      PeerLogicalItem
  | OracleEnvelope OracleClientIngress
  | DisappearanceAbortEnvelope DisappearanceProbeId
  | CompletionEnvelope
      CompletionOrdinal
      CompletionCorrelation
      RuntimeObservation
  | AdmittedEnvelope HeraldInputBody
  deriving stock (Eq, Show)

data CompletionCorrelation
  = UncorrelatedCompletion
  | OriginCompletion ObligationOrigin
  | BindingLossCompletion TransferBinding
  deriving stock (Eq, Show)

-- | The exact routing action owed by a source paused for semantic disposition.
data DispositionExpectation
  = ExpectApplicationDisposition CandidateApplicationLane
  | ExpectConfiguredAdministrationDisposition CandidateAdministrationLane
  | ExpectPeerCandidateRoute ConnectionNonce
  | ExpectPeerDisposition PeerCandidate
  deriving stock (Eq, Show)

-- | Exact physical source selected for an input which pauses until its
-- disposition is routed.  Retaining the lease in the batch prevents a delayed
-- dispatcher from rediscovering an ambiguous candidate in the registry.
data DispositionRoute
  = ApplicationDispositionRoute
      (ConnectionRef ApplicationPlane)
      SourceId
      CandidateApplicationLane
  | ConfiguredAdministrationDispositionRoute
      (ConnectionRef ConfiguredAdministrationPlane)
      SourceId
      CandidateAdministrationLane
  | LocalPeerCandidateRoute
      (ConnectionRef PeerPlane)
      SourceId
      ConnectionNonce
  | PeerDispositionRoute
      (ConnectionRef PeerPlane)
      SourceId
      PeerCandidate
  deriving stock (Eq, Show)

-- | Logical keys whose physical binding-loss observation may be fenced by a
-- pending same-binding promotion.
data TransferBinding
  = TransferApplicationBinding ApplicationSessionBinding
  | TransferAdministrationBinding AdministrationBinding
  | TransferPeerBinding PeerBinding
  deriving stock (Eq, Ord, Show)

dispositionExpectationFor :: HeraldInputBody -> Maybe DispositionExpectation
dispositionExpectationFor = \case
  ApplicationSessionInput ingress -> case ingress of
    ClaimInitialApplicationSession candidate _ _ _ -> Just (ExpectApplicationDisposition candidate)
    OpenApplicationSession candidate _ _ -> Just (ExpectApplicationDisposition candidate)
    ResumeApplicationSession candidate _ _ _ -> Just (ExpectApplicationDisposition candidate)
    EndApplicationSession {} -> Nothing
  ApplicationReceiptRetirementInput _ -> Nothing
  ApplicationLifecycleInput ingress -> case ingress of
    RecoverApplicationLifecycleResult candidate _ _ -> Just (ExpectApplicationDisposition candidate)
    _ -> Nothing
  AdministrationInput ingress -> case ingress of
    OpenAdministrationConnection candidate _ ->
      Just (ExpectConfiguredAdministrationDisposition candidate)
    CancelChildPreparation {} -> Nothing
    StartProcessEpoch {} -> Nothing
    EndProcessEpoch {} -> Nothing
    RetireAdministrationReceipts {} -> Nothing
    GetAdministrationResult {} -> Nothing
    GetHeraldStatus {} -> Nothing
    PrepareOracleReplica {} -> Nothing
    BeginVoterChange {} -> Nothing
    CancelVoterChange {} -> Nothing
    GetOracleConfiguration {} -> Nothing
    GetVoterChangeStatus {} -> Nothing
    ListChildPreparations {} -> Nothing
    DrainConfiguredHerald {} -> Nothing
    OrderlyHeraldShutdown {} -> Nothing
  PeerInput ingress -> case ingress of
    PeerCandidateOpened opened -> Just (ExpectPeerCandidateRoute (peerCandidateOpenedConnectionNonce opened))
    PeerHelloReceived candidate _ _ _ _ _ -> Just (ExpectPeerDisposition candidate)
    PeerControlReceived {} -> Nothing
    PeerControlReceivedWithProgress {} -> Nothing
    PeerPublicationReceivedWithProgress {} -> Nothing
    PeerPublicationReceived {} -> Nothing
    PeerDispatchSelected {} -> Nothing
  JoinInput {} -> Nothing
  OracleInput {} -> Nothing
  DisappearanceInput {} -> Nothing
  ApplicationRequestInput {} -> Nothing
  RuntimeObserved {} -> Nothing

effectMatchesDisposition :: DispositionExpectation -> HeraldEffect -> Bool
effectMatchesDisposition expectation = case expectation of
  ExpectApplicationDisposition candidate -> \case
    DeferApplicationSession current -> current == candidate
    SendInitialClaimPending current _ _ -> current == candidate
    RejectInitialClaim current _ _ -> current == candidate
    SendApplicationLifecycleReply (CandidateApplicationDisposition current) _ -> current == candidate
    SetApplicationConnectionDisposition current _ -> current == candidate
    RejectApplicationConnection (CandidateApplicationDisposition current) _ -> current == candidate
    _ -> False
  ExpectConfiguredAdministrationDisposition candidate -> \case
    SetAdministrationConnectionDisposition current _ -> current == candidate
    RejectAdministrationConnection (CandidateAdministrationDisposition current) ->
      current == candidate
    _ -> False
  ExpectPeerDisposition candidate -> \case
    SetPeerCandidateDisposition current _ -> current == candidate
    _ -> False
  ExpectPeerCandidateRoute nonce -> \case
    RejectPeerCandidateOpened current -> current == nonce
    SendPeerCandidate _ _ _ hello -> peerHelloConnectionNonce hello == nonce
    _ -> False

-- | Exactly one routing decision must identify a paused source. Explicit
-- deferral retains its pause; a later disposition uses the same candidate.
validateDispositionBatch :: DispositionExpectation -> EffectBatch -> Bool
validateDispositionBatch expectation =
  (== 1) . length . filter (effectMatchesDisposition expectation) . effectBatchMembers

data ObligationOrigin = ObligationOrigin BatchOrdinal EffectMemberOrdinal
  deriving stock (Eq, Ord, Show)

data ObligationState
  = ObligationRegistered
  | SettledByOffer
  | AwaitingPhysicalOutcome
  | ObservationQueued CompletionOrdinal
  | KernelStepped
  deriving stock (Eq, Ord, Show)

data DrainLedger = DrainLedger
  { ledgerObligations :: Map ObligationOrigin ObligationState,
    ledgerRegisteredThrough :: Maybe ObligationOrigin,
    ledgerRoutedThrough :: Maybe BatchOrdinal,
    ledgerSealedCut :: Maybe BatchOrdinal,
    ledgerCompletionCut :: Maybe CompletionOrdinal
  }
  deriving stock (Eq, Show)

emptyDrainLedger :: DrainLedger
emptyDrainLedger = DrainLedger Map.empty Nothing Nothing Nothing Nothing

-- | The single dispatcher registers effect members in strict batch/member
-- order before handing them to a consumer. The high water prevents duplicate
-- registration from reopening a released obligation; the map is its unresolved
-- exception set and contains no terminal history.
registerObligation :: ObligationOrigin -> DrainLedger -> DrainLedger
registerObligation origin ledger
  | maybe False (origin <=) (ledgerRegisteredThrough ledger) = ledger
  | otherwise =
      ledger
        { ledgerObligations = Map.insert origin ObligationRegistered (ledgerObligations ledger),
          ledgerRegisteredThrough = Just origin
        }

settleObligationByOffer :: ObligationOrigin -> DrainLedger -> DrainLedger
settleObligationByOffer = setObligation SettledByOffer

awaitPhysicalOutcome :: ObligationOrigin -> DrainLedger -> DrainLedger
awaitPhysicalOutcome = setObligation AwaitingPhysicalOutcome

queueObligationObservation :: ObligationOrigin -> CompletionOrdinal -> DrainLedger -> DrainLedger
queueObligationObservation origin completion = setObligation (ObservationQueued completion) origin

markObligationKernelStepped :: ObligationOrigin -> DrainLedger -> DrainLedger
markObligationKernelStepped = setObligation KernelStepped

advanceRoutedThrough :: BatchOrdinal -> DrainLedger -> DrainLedger
advanceRoutedThrough batch ledger = ledger {ledgerRoutedThrough = Just (maybe batch (max batch) (ledgerRoutedThrough ledger))}

sealDrainLedger :: BatchOrdinal -> CompletionOrdinal -> DrainLedger -> DrainLedger
sealDrainLedger cut completionCut ledger =
  ledger
    { ledgerSealedCut = Just cut,
      ledgerCompletionCut = Just completionCut
    }

outstandingPhysicalOutcomes :: DrainLedger -> [ObligationOrigin]
outstandingPhysicalOutcomes =
  Map.keys
    . Map.filter (== AwaitingPhysicalOutcome)
    . ledgerObligations

drainBarrierReady :: CompletionOrdinal -> DrainLedger -> Bool
drainBarrierReady steppedThrough ledger =
  case (ledgerSealedCut ledger, ledgerRoutedThrough ledger, ledgerCompletionCut ledger) of
    (Just cut, Just routed, Just completionCut) ->
      routed >= cut
        && steppedThrough >= completionCut
        && Map.null (ledgerObligations ledger)
    _ -> False

drainLedgerObligations :: DrainLedger -> Map ObligationOrigin ObligationState
drainLedgerObligations = ledgerObligations

-- An origin is admitted only by registerObligation, synchronously before its
-- offer/outcome can arrive. Terminal settlement releases it immediately. Late
-- physical, transfer, or duplicate settlement observations cannot recreate it.
setObligation :: ObligationState -> ObligationOrigin -> DrainLedger -> DrainLedger
setObligation state origin ledger =
  ledger
    { ledgerObligations = case state of
        SettledByOffer -> Map.delete origin (ledgerObligations ledger)
        KernelStepped -> Map.delete origin (ledgerObligations ledger)
        _ -> Map.adjust (const state) origin (ledgerObligations ledger)
    }

data OutcomeCellState
  = OutcomeCellEmpty
  | OutcomeCellClaimed PeerDispatchOutcome
  deriving stock (Eq, Show)

claimOutcomeCell :: PeerDispatchOutcome -> OutcomeCellState -> (Bool, OutcomeCellState)
claimOutcomeCell outcome = \case
  OutcomeCellEmpty -> (True, OutcomeCellClaimed outcome)
  retained@(OutcomeCellClaimed _) -> (False, retained)
