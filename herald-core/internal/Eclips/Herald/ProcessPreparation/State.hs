{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Lifecycle receipts and independently owned preparation state. Application
-- acknowledgements release terminal replies without discarding child work or
-- its accepted origin. Recovery remains available only while its receipt lives.
module Eclips.Herald.ProcessPreparation.State
  ( State,
    emptyState,
    WorkStage (..),
    preparationContext,
    lookupPendingClaim,
    PreparationDependency (..),
    PreparationContext (..),
    workIds,
    pendingWork,
    nextWork,
    consumeWork,
    replaceWorkDependencies,
    notifyPreparationDependencies,
    seedPreparationContext,
    synchronizePreparationContext,
    lookupReadiness,
    retainReadiness,
    PreparationResultConsumer (..),
    unobservedPreparationResults,
    unobservedOwnEndResult,
    notifyOracleResults,
    markResultObserved,
    clearPreparationScheduling,
    takeChangedRequestKeys,
    takeLifecycleRetirementSessions,
    requestEntriesForSession,
    ownEndRecovery,
    preparationReferencesForProcess,
    retainIncomingOffer,
    sealIncomingCancellation,
    failIncoming,
    requestIncomingReply,
    recordIncomingReply,
    receiveOutgoingStatus,
    retainOutgoingResult,
    OutgoingCancellationMode (..),
    cancelOutgoing,
    recordOutgoingDelivery,
    ProcessPreparationInvariant (..),
    validateState,
    LifecycleKey,
    lifecycleKey,
    lifecycleKeyProcess,
    lifecycleKeyRequest,
    RequestRecord,
    requestCommand,
    requestStatus,
    requestSession,
    requestBinding,
    requestAttachment,
    requestEndReference,
    requestEntries,
    lookupRequest,
    retainRequest,
    replaceRequestBinding,
    settleRequest,
    retireLifecycleReceipts,
    retireClosedSessionLifecycleReceipts,
    retainOwnEndReference,
    deferredBeginEntries,
    retainDeferredBegin,
    removeDeferredBegin,
    OutgoingPreparation (..),
    IncomingPreparation (..),
    outgoingEntries,
    lookupOutgoing,
    retainOutgoing,
    replaceOutgoing,
    incomingEntries,
    lookupIncoming,
    retainIncoming,
    replaceIncoming,
    retainRemotePreparation,
    preparationSource,
    Preparation,
    PreparationPhase (..),
    preparationReference,
    preparationParent,
    preparationGrant,
    preparationStart,
    preparationStartReference,
    preparationEndReference,
    preparationPhase,
    preparationResult,
    preparationLocator,
    preparationStartControl,
    preparationCancelled,
    preparationFailure,
    preparationWasInstalled,
    markPreparationFailed,
    markPreparationInstallationFailed,
    deferredSessionEntries,
    retainDeferredSession,
    removeDeferredSession,
    observeCandidateLoss,
    pendingClaimEntries,
    retainPendingClaim,
    removePendingClaim,
    preparationEntries,
    lookupPreparation,
    preparationForAttachment,
    retainPreparation,
    markPreparationStarted,
    markPreparationStartReference,
    markPreparationInstalled,
    markPreparationCancelling,
    markPreparationEndReference,
    markPreparationTerminal,
    markPreparationAttached,
  ) where

import Control.Monad (foldM, unless)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Types.Lifecycle
import Eclips.Domain.Identity (ControlIndex, DeltaId, GlobalObjectId, HeraldEpoch, ProcessEpochId, globalObjectIdFromProcessEpochId, heraldEpochBytes)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.ProcessStart (ProcessStart, processStartProcessEpochId)
import Eclips.Herald.Application.Primordial (CheckedPrimordialGrant)
import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationSessionBinding,
    ApplicationSessionId,
    applicationAttachmentForProcess,
    applicationSessionIdHeraldEpoch,
    applicationSessionIdOrdinal,
  )
import Eclips.Herald.Application.State (ApplicationGatePhase)
import Eclips.Herald.Discovery (PeerBinding)
import Eclips.Herald.Input (ApplicationSessionIngress (..), CandidateApplicationLane)
import Eclips.Herald.Internal.WorkIndex qualified as Work
import Eclips.Herald.OracleClient (OracleBinding)
import Eclips.Herald.OracleClient.State (OracleRequestRef)
import Eclips.Herald.ProcessPreparation.Protocol (RemoteChild (..), RemotePreparationStatus (..))
import Eclips.Herald.Structural.Debt (SortOccurrence)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement, receiptIsRetired)

-- Session-scoped correlations remain associated with their logical process.
data LifecycleKey = LifecycleKey !ProcessEpochId !LifecycleRequestId deriving stock (Eq, Ord, Show)
lifecycleKey :: ProcessEpochId -> LifecycleRequestId -> LifecycleKey
lifecycleKey = LifecycleKey
lifecycleKeyProcess :: LifecycleKey -> ProcessEpochId
lifecycleKeyProcess (LifecycleKey process _) = process
lifecycleKeyRequest :: LifecycleKey -> LifecycleRequestId
lifecycleKeyRequest (LifecycleKey _ request) = request

data RequestRecord = RequestRecord
  { command :: LifecycleCommand,
    status :: LifecycleStatus,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    attachment :: ApplicationAttachment,
    ownEndReference :: Maybe OracleRequestRef
  }
  deriving stock (Eq, Show)
requestCommand :: RequestRecord -> LifecycleCommand
requestCommand = (.command)
requestStatus :: RequestRecord -> LifecycleStatus
requestStatus = (.status)
requestSession :: RequestRecord -> ApplicationSessionId
requestSession = (.session)
requestBinding :: RequestRecord -> ApplicationSessionBinding
requestBinding = (.binding)
requestAttachment :: RequestRecord -> ApplicationAttachment
requestAttachment = (.attachment)
requestEndReference :: RequestRecord -> Maybe OracleRequestRef
requestEndReference = (.ownEndReference)

data PreparationPhase = Preparing | Prepared | Attached | Cancelling | Terminal
  deriving stock (Eq, Ord, Show)

-- This fact can be admitted only while the exact accepted Begin receipt is
-- still available. It lives with the semantic preparation that consumes it,
-- rather than retaining another per-request receipt or tombstone collection.
data BeginReceiptProvenance = BeginReceiptRetained | BeginReceiptRetired
  deriving stock (Eq, Show)

data Preparation = Preparation
  { reference :: ChildPreparation,
    parent :: ProcessEpochId,
    grant :: CheckedPrimordialGrant,
    start :: ProcessStart,
    startReference :: Maybe OracleRequestRef,
    endReference :: Maybe OracleRequestRef,
    phase :: PreparationPhase,
    result :: Maybe PreparedChild,
    locator :: HeraldLocator,
    startControl :: Maybe ControlIndex,
    cancelled :: Bool,
    failure :: Maybe StartupError,
    installed :: Bool,
    source :: Maybe HeraldEpoch,
    beginReceipt :: BeginReceiptProvenance
  }
  deriving stock (Eq, Show)
preparationReference :: Preparation -> ChildPreparation
preparationReference = (.reference)
preparationParent :: Preparation -> ProcessEpochId
preparationParent = (.parent)
preparationGrant :: Preparation -> CheckedPrimordialGrant
preparationGrant = (.grant)
preparationStart :: Preparation -> ProcessStart
preparationStart = (.start)
preparationStartReference :: Preparation -> Maybe OracleRequestRef
preparationStartReference = (.startReference)
preparationEndReference :: Preparation -> Maybe OracleRequestRef
preparationEndReference = (.endReference)
preparationPhase :: Preparation -> PreparationPhase
preparationPhase = (.phase)
preparationResult :: Preparation -> Maybe PreparedChild
preparationResult = (.result)
preparationLocator :: Preparation -> HeraldLocator
preparationLocator = (.locator)
preparationStartControl :: Preparation -> Maybe ControlIndex
preparationStartControl = (.startControl)
preparationCancelled :: Preparation -> Bool
preparationCancelled = (.cancelled)
preparationFailure :: Preparation -> Maybe StartupError
preparationFailure = (.failure)
preparationWasInstalled :: Preparation -> Bool
preparationWasInstalled = (.installed)
preparationSource :: Preparation -> Maybe HeraldEpoch
preparationSource = (.source)

-- Source-side receipts retain the accepted global selection independently of
-- the parent's live aliases. Binding stamps make reconnect reoffers exact.
data OutgoingPreparation = OutgoingPreparation
  { outgoingParent :: ProcessEpochId,
    outgoingTarget :: HeraldEpoch,
    outgoingLocator :: HeraldLocator,
    outgoingTransfer :: ByteString,
    outgoingStatus :: RemotePreparationStatus,
    outgoingChild :: Maybe RemoteChild,
    outgoingResult :: Maybe PreparedChild,
    outgoingCancelled :: Bool,
    outgoingOfferBinding :: Maybe PeerBinding,
    outgoingCancelBinding :: Maybe PeerBinding
  }
  deriving stock (Eq, Show)

-- Cancellation can be admitted before its offer; that retained seal prevents
-- a delayed offer from allocating a child after cancellation completed.
data IncomingPreparation = IncomingPreparation
  { incomingSource :: HeraldEpoch,
    incomingOffer :: Maybe (HeraldLocator, ByteString),
    incomingCancelled :: Bool,
    incomingFailure :: Maybe StartupError,
    incomingLastReply :: Maybe (PeerBinding, RemotePreparationStatus)
  }
  deriving stock (Eq, Show)

-- OutgoingPreparation remains constructible for the handoff coordinator;
-- only this private wrapper may carry admitted receipt-retirement evidence.
data RetainedOutgoingPreparation = RetainedOutgoingPreparation OutgoingPreparation BeginReceiptProvenance
  deriving stock (Eq, Show)

data State = State
  { requests :: Map LifecycleKey RequestRecord,
    preparations :: Map ChildPreparation Preparation,
    outgoing :: Map ChildPreparation RetainedOutgoingPreparation,
    incoming :: Map ChildPreparation IncomingPreparation,
    pendingClaims :: Map (ApplicationAttachment, InitialClaimId) CandidateApplicationLane,
    deferredBegins :: Map LifecycleKey HeraldLocator,
    deferredSessions :: Map CandidateApplicationLane ApplicationSessionIngress,
    localWork :: !(Work.WorkIndex ChildPreparation PreparationDependency),
    remoteWork :: !(Work.WorkIndex ChildPreparation PreparationDependency),
    requestWork :: !(Work.WorkIndex LifecycleKey PreparationDependency),
    claimWork :: !(Work.WorkIndex (ApplicationAttachment, InitialClaimId) PreparationDependency),
    outgoingDeliveryWork :: !(Work.WorkIndex ChildPreparation PreparationDependency),
    incomingDeliveryWork :: !(Work.WorkIndex ChildPreparation PreparationDependency),
    readinessWork :: !(Work.WorkIndex ChildPreparation PreparationDependency),
    readinessOutcomes :: !(Map ChildPreparation (Either StartupError (Maybe [Text]))),
    context :: !(Maybe PreparationContext),
    preparationsByProcess :: !(Map ProcessEpochId (Set ChildPreparation)),
    preparationsByAttachment :: !(Map ApplicationAttachment (Set ChildPreparation)),
    requestsByPreparation :: !(Map ChildPreparation (Set LifecycleKey)),
    requestsBySession :: !(Map ApplicationSessionId (Set LifecycleKey)),
    ownEndRecoveries :: !(Map (ApplicationAttachment, LifecycleRequestId) (Set LifecycleKey)),
    claimsByAttachment :: !(Map ApplicationAttachment (Set (ApplicationAttachment, InitialClaimId))),
    claimsByLane :: !(Map CandidateApplicationLane (ApplicationAttachment, InitialClaimId)),
    resultReferences :: !(Map PreparationResultConsumer OracleRequestRef),
    resultConsumers :: !(Map OracleRequestRef (Set PreparationResultConsumer)),
    observedResults :: !(Set PreparationResultConsumer),
    changedRequestKeys :: !(Set LifecycleKey),
    lifecycleRetirementSessions :: !(Set ApplicationSessionId)
  }
  deriving stock (Eq, Show)
emptyState :: State
emptyState =
  State
    { requests = Map.empty,
      preparations = Map.empty,
      outgoing = Map.empty,
      incoming = Map.empty,
      pendingClaims = Map.empty,
      deferredBegins = Map.empty,
      deferredSessions = Map.empty,
      localWork = Work.empty,
      remoteWork = Work.empty,
      requestWork = Work.empty,
      claimWork = Work.empty,
      outgoingDeliveryWork = Work.empty,
      incomingDeliveryWork = Work.empty,
      readinessWork = Work.empty,
      readinessOutcomes = Map.empty,
      context = Nothing,
      preparationsByProcess = Map.empty,
      preparationsByAttachment = Map.empty,
      requestsByPreparation = Map.empty,
      requestsBySession = Map.empty,
      ownEndRecoveries = Map.empty,
      claimsByAttachment = Map.empty,
      claimsByLane = Map.empty,
      resultReferences = Map.empty,
      resultConsumers = Map.empty,
      observedResults = Set.empty,
      changedRequestKeys = Set.empty,
      lifecycleRetirementSessions = Set.empty
    }

-- | A candidate lane is retained only as a reply correlation, never as a
-- session grant. Replaying its admitted request uses the ordinary owner check.
deferredSessionEntries :: State -> [(CandidateApplicationLane, ApplicationSessionIngress)]
deferredSessionEntries = Map.toAscList . (.deferredSessions)

retainDeferredSession :: ApplicationSessionIngress -> State -> Either ProcessPreparationInvariant State
retainDeferredSession ingress state = case ingress of
  OpenApplicationSession lane _ _ -> retain lane
  ResumeApplicationSession lane _ _ _ -> retain lane
  _ -> Left PreparationPhaseContradiction
  where
    retain lane = case Map.lookup lane state.deferredSessions of
      Just previous | previous /= ingress -> Left PreparationRequestConflict
      _ -> Right state {deferredSessions = Map.insert lane ingress state.deferredSessions}
removeDeferredSession :: CandidateApplicationLane -> State -> State
removeDeferredSession lane state = state {deferredSessions = Map.delete lane state.deferredSessions}

-- | A dead candidate may no longer acquire an attachment when a gate opens.
-- Request/result ownership is retained independently of these reply lanes.
observeCandidateLoss :: CandidateApplicationLane -> State -> State
observeCandidateLoss lane state =
  ( case Map.lookup lane state.claimsByLane of
      Nothing -> state
      Just (attachment, claim) -> removePendingClaim attachment claim state
  )
    { deferredSessions = Map.delete lane state.deferredSessions
    }

deferredBeginEntries :: State -> [(LifecycleKey, HeraldLocator)]
deferredBeginEntries = Map.toAscList . (.deferredBegins)

-- | Retain only the exact call and its local return locator while membership
-- admission is closed. No child identity, grant, or Oracle request exists yet.
retainDeferredBegin :: LifecycleKey -> HeraldLocator -> State -> Either ProcessPreparationInvariant State
retainDeferredBegin key locator state = case Map.lookup key state.requests of
  Just record
    | BeginChild {} <- record.command,
      LifecyclePending _ <- record.status ->
        case Map.lookup key state.deferredBegins of
          Just incumbent | incumbent /= locator -> Left PreparationRequestConflict
          _ -> Right state {deferredBegins = Map.insert key locator state.deferredBegins}
  _ -> Left PreparationPhaseContradiction

removeDeferredBegin :: LifecycleKey -> State -> State
removeDeferredBegin key state = state {deferredBegins = Map.delete key state.deferredBegins}
outgoingEntries :: State -> [(ChildPreparation, OutgoingPreparation)]
outgoingEntries state = [(reference, entry) | (reference, RetainedOutgoingPreparation entry _) <- Map.toAscList state.outgoing]
lookupOutgoing :: ChildPreparation -> State -> Maybe OutgoingPreparation
lookupOutgoing reference state = do
  RetainedOutgoingPreparation entry _ <- Map.lookup reference state.outgoing
  pure entry
retainOutgoing :: ChildPreparation -> OutgoingPreparation -> State -> Either ProcessPreparationInvariant State
retainOutgoing reference entry state = do
  unless (Map.notMember reference state.outgoing && Map.notMember reference state.preparations) (Left PreparationIdentityContradiction)
  pure
    ( refreshOutgoingWork
        reference
        entry
        state
          { outgoing = Map.insert reference (RetainedOutgoingPreparation entry BeginReceiptRetained) state.outgoing
          }
    )
replaceOutgoing :: ChildPreparation -> OutgoingPreparation -> State -> Either ProcessPreparationInvariant State
replaceOutgoing reference entry state = do
  RetainedOutgoingPreparation previous provenance <- maybe (Left PreparationMissing) Right (Map.lookup reference state.outgoing)
  unless (entry.outgoingParent == previous.outgoingParent) (Left PreparationIdentityContradiction)
  let retained = state {outgoing = Map.insert reference (RetainedOutgoingPreparation entry provenance) state.outgoing}
  pure
    ( if outgoingSemanticFacts previous == outgoingSemanticFacts entry
        then retained
        else notifyOutgoingMutation reference entry retained
    )
incomingEntries :: State -> [(ChildPreparation, IncomingPreparation)]
incomingEntries = Map.toAscList . (.incoming)
lookupIncoming :: ChildPreparation -> State -> Maybe IncomingPreparation
lookupIncoming reference state = Map.lookup reference state.incoming
retainIncoming :: ChildPreparation -> IncomingPreparation -> State -> Either ProcessPreparationInvariant State
retainIncoming reference entry state = do
  unless (Map.notMember reference state.incoming) (Left PreparationIdentityContradiction)
  pure
    state
      { incoming = Map.insert reference entry state.incoming,
        incomingDeliveryWork = Work.registerWork reference (Set.singleton (PeerBindingChanged entry.incomingSource)) state.incomingDeliveryWork
      }
replaceIncoming :: ChildPreparation -> IncomingPreparation -> State -> Either ProcessPreparationInvariant State
replaceIncoming reference entry state = do
  previous <- maybe (Left PreparationMissing) Right (Map.lookup reference state.incoming)
  let retained = state {incoming = Map.insert reference entry state.incoming}
      semanticChanged = (previous.incomingCancelled, previous.incomingFailure) /= (entry.incomingCancelled, entry.incomingFailure)
      requested = entry.incomingLastReply == Nothing
  pure (if semanticChanged || requested then dirtyIncomingDelivery reference retained else retained)

requestEntries :: State -> [(LifecycleKey, RequestRecord)]
requestEntries = Map.toAscList . (.requests)
lookupRequest :: LifecycleKey -> State -> Maybe RequestRecord
lookupRequest key state = Map.lookup key state.requests
pendingClaimEntries :: State -> [((ApplicationAttachment, InitialClaimId), CandidateApplicationLane)]
pendingClaimEntries = Map.toAscList . (.pendingClaims)
retainPendingClaim :: ApplicationAttachment -> InitialClaimId -> CandidateApplicationLane -> State -> State
retainPendingClaim attachment claim lane state
  | Map.lookup (attachment, claim) state.pendingClaims == Just lane = state
  | otherwise =
      let key = claimWorkKey attachment claim
          withoutLane = case Map.lookup lane state.claimsByLane of
            Nothing -> state
            Just (oldAttachment, oldClaim) -> removePendingClaim oldAttachment oldClaim state
          withoutKey = removePendingClaim attachment claim withoutLane
       in withoutKey
            { pendingClaims = Map.insert key lane withoutKey.pendingClaims,
              claimsByLane = Map.insert lane key withoutKey.claimsByLane,
              claimsByAttachment = insertBucket attachment key withoutKey.claimsByAttachment,
              claimWork = Work.registerWork key (Set.fromList [AttachmentChanged attachment, GateChanged, ActivityChanged]) withoutKey.claimWork
            }
removePendingClaim :: ApplicationAttachment -> InitialClaimId -> State -> State
removePendingClaim attachment claim state =
  let key = claimWorkKey attachment claim
   in state
        { pendingClaims = Map.delete key state.pendingClaims,
          claimsByLane = maybe state.claimsByLane (`Map.delete` state.claimsByLane) (Map.lookup key state.pendingClaims),
          claimsByAttachment = deleteBucket attachment key state.claimsByAttachment,
          claimWork = Work.removeWork key state.claimWork
        }

preparationEntries :: State -> [(ChildPreparation, Preparation)]
preparationEntries = Map.toAscList . (.preparations)
lookupPreparation :: ChildPreparation -> State -> Maybe Preparation
lookupPreparation ref state = Map.lookup ref state.preparations
preparationForAttachment :: ApplicationAttachment -> State -> Maybe Preparation
preparationForAttachment attachment state = case Set.toList (Map.findWithDefault Set.empty attachment state.preparationsByAttachment) of
  [reference] -> Map.lookup reference state.preparations
  _ -> Nothing

data ProcessPreparationInvariant
  = PreparationRequestConflict
  | PreparationMissing
  | PreparationPhaseContradiction
  | PreparationIdentityContradiction
  | PreparationResultContradiction
  | PreparationReceiptRetirementNotReady
  | PreparationWorkIndexContradiction
  deriving stock (Eq, Show)

retainRequest :: LifecycleKey -> LifecycleCommand -> LifecycleStatus -> ApplicationSessionId -> ApplicationSessionBinding -> ApplicationAttachment -> State -> Either ProcessPreparationInvariant State
retainRequest key command status session binding attachment state = do
  unless (Map.notMember key state.requests) (Left PreparationRequestConflict)
  let record = RequestRecord command status session binding attachment Nothing
  pure (indexNewRequest key record state {requests = Map.insert key record state.requests})
replaceRequestBinding :: LifecycleKey -> ApplicationSessionBinding -> State -> Either ProcessPreparationInvariant State
replaceRequestBinding key binding = updateRequest key (\record -> Right record {binding})
settleRequest :: LifecycleKey -> LifecycleStatus -> State -> Either ProcessPreparationInvariant State
settleRequest key status = updateRequest key $ \record -> case record.status of
  LifecyclePending _ -> Right record {status}
  previous | previous == status -> Right record
  _ -> Left PreparationResultContradiction

-- | Release only consumed lifecycle receipts for one exact session. Unresolved
-- exceptions keep their own correlations without pinning unrelated receipts.
retireLifecycleReceipts :: ApplicationSessionId -> ReceiptRetirement -> State -> Either ProcessPreparationInvariant State
retireLifecycleReceipts session progress state = do
  let entries = requestEntriesForSession session state
      covered key = receiptIsRetired (lifecycleRequestIdWord64 (lifecycleKeyRequest key)) progress
  unless
    (all (\(key, record) -> not (covered key) || terminalStatus record.status) entries)
    (Left PreparationReceiptRetirementNotReady)
  retireReceiptKeys (Set.fromList [key | (key, _) <- entries, covered key]) state

-- | Session death ends lifecycle retry entitlement, including own-End recovery.
-- Observation-only waits have lost their consumer and can be discarded, while
-- pending Begin/Cancel/End work continues under its semantic owner. Capture any
-- final own-End reply effect before invoking this helper, then invoke it again
-- after pending mutations settle to release their now-terminal records.
retireClosedSessionLifecycleReceipts :: ApplicationSessionId -> State -> Either ProcessPreparationInvariant State
retireClosedSessionLifecycleReceipts session state =
  retireReceiptKeys
    ( Set.fromList
        [key | (key, record) <- requestEntriesForSession session state, terminalStatus record.status || observationCommand record.command]
    )
    state {lifecycleRetirementSessions = Set.delete session state.lifecycleRetirementSessions}

observationCommand :: LifecycleCommand -> Bool
observationCommand AwaitPreparedChild {} = True
observationCommand AwaitChildReady {} = True
observationCommand _ = False

retireReceiptKeys :: Set LifecycleKey -> State -> Either ProcessPreparationInvariant State
retireReceiptKeys keys state = foldM retireOne state (Set.toAscList keys)
  where
    retireOne retained key = do
      record <- maybe (Left PreparationMissing) Right (Map.lookup key retained.requests)
      withProvenance <- retainBeginProvenance retained (key, record)
      pure (removeRequestIndexes key record withProvenance {requests = Map.delete key withProvenance.requests})

terminalStatus :: LifecycleStatus -> Bool
terminalStatus LifecyclePending {} = False
terminalStatus _ = True

retainBeginProvenance :: State -> (LifecycleKey, RequestRecord) -> Either ProcessPreparationInvariant State
retainBeginProvenance state (key, record) = case (record.command, record.status) of
  (BeginChild {}, LifecycleCompleted (ChildPreparationAccepted reference)) -> do
    let request = lifecycleKeyRequest key
    unless
      ( childPreparationScopeBytes reference == lifecycleRequestIdScopeBytes request
          && childPreparationSessionOrdinal reference == lifecycleRequestIdSessionOrdinal request
          && childPreparationOrdinal reference == lifecycleRequestIdWord64 request
      )
      (Left PreparationIdentityContradiction)
    case (Map.lookup reference state.preparations, Map.lookup reference state.outgoing) of
      (Just preparation, Nothing) -> do
        unless (preparation.parent == lifecycleKeyProcess key && preparation.source == Nothing) (Left PreparationIdentityContradiction)
        pure state {preparations = Map.insert reference preparation {beginReceipt = BeginReceiptRetired} state.preparations}
      (Nothing, Just (RetainedOutgoingPreparation outgoing _)) -> do
        unless (outgoing.outgoingParent == lifecycleKeyProcess key) (Left PreparationIdentityContradiction)
        pure state {outgoing = Map.insert reference (RetainedOutgoingPreparation outgoing BeginReceiptRetired) state.outgoing}
      (Nothing, Nothing) -> Right state
      _ -> Left PreparationIdentityContradiction
  _ -> Right state
retainOwnEndReference :: LifecycleKey -> OracleRequestRef -> State -> Either ProcessPreparationInvariant State
retainOwnEndReference key reference state = do
  previous <- maybe (Left PreparationMissing) Right (Map.lookup key state.requests)
  retained <-
    updateRequest
      key
      ( \record -> case (record.command, record.ownEndReference) of
          (EndOwnProcess, Nothing) -> Right record {ownEndReference = Just reference}
          (EndOwnProcess, Just existing) | existing == reference -> Right record
          _ -> Left PreparationPhaseContradiction
      )
      state
  pure (if previous.ownEndReference == Nothing then registerResult (OwnEndResult key) reference retained else retained)

updateRequest :: LifecycleKey -> (RequestRecord -> Either ProcessPreparationInvariant RequestRecord) -> State -> Either ProcessPreparationInvariant State
updateRequest key update state = do
  record <- maybe (Left PreparationMissing) Right (Map.lookup key state.requests)
  next <- update record
  let retained = state {requests = Map.insert key next state.requests}
  pure
    ( if record.status == next.status
        then retained
        else
          refreshRequestWork
            key
            next
            (removePendingRequestIndex key record retained)
              { changedRequestKeys = insertSet key retained.changedRequestKeys,
                lifecycleRetirementSessions = insertSet next.session retained.lifecycleRetirementSessions
              }
    )

retainPreparation :: ChildPreparation -> ProcessEpochId -> HeraldLocator -> CheckedPrimordialGrant -> ProcessStart -> State -> Either ProcessPreparationInvariant State
retainPreparation reference parent locator grant start state = do
  unless (Map.notMember reference state.preparations) (Left PreparationIdentityContradiction)
  let entry = Preparation reference parent grant start Nothing Nothing Preparing Nothing locator Nothing False Nothing False Nothing BeginReceiptRetained
  let process = processStartProcessEpochId start
      attachment = applicationAttachmentForProcess process
  pure
    ( notifyPreparationMutation
        reference
        entry
        state
          { preparations = Map.insert reference entry state.preparations,
            preparationsByProcess = insertBucket process reference state.preparationsByProcess,
            preparationsByAttachment = insertBucket attachment reference state.preparationsByAttachment
          }
    )
retainRemotePreparation :: HeraldEpoch -> ChildPreparation -> ProcessEpochId -> HeraldLocator -> CheckedPrimordialGrant -> ProcessStart -> State -> Either ProcessPreparationInvariant State
retainRemotePreparation source reference parent locator grant start state = do
  retained <- retainPreparation reference parent locator grant start state
  updatePreparation reference (\entry -> Right entry {source = Just source}) retained

markPreparationStartReference :: ChildPreparation -> OracleRequestRef -> State -> Either ProcessPreparationInvariant State
markPreparationStartReference reference oracle state = do
  previous <- maybe (Left PreparationMissing) Right (lookupPreparation reference state)
  retained <-
    updatePreparation
      reference
      ( \entry -> case (entry.phase, entry.startReference) of
          (Preparing, Nothing) -> Right entry {startReference = Just oracle}
          (Preparing, Just prior) | prior == oracle -> Right entry
          _ -> Left PreparationPhaseContradiction
      )
      state
  pure (if previous.startReference == Nothing then registerResult (PreparationStartResult reference) oracle retained else retained)

markPreparationStarted :: ChildPreparation -> ControlIndex -> State -> Either ProcessPreparationInvariant State
markPreparationStarted reference control = updatePreparation reference $ \entry -> case entry.startControl of
  Nothing -> Right entry {startControl = Just control}
  Just prior | prior == control -> Right entry
  _ -> Left PreparationPhaseContradiction
markPreparationInstalled :: ChildPreparation -> Maybe PreparedChild -> State -> Either ProcessPreparationInvariant State
markPreparationInstalled reference result = updatePreparation reference $ \entry -> case entry.phase of
  Preparing | entry.startControl /= Nothing -> Right entry {phase = Prepared, result, installed = True}
  Prepared | entry.result == result -> Right entry
  _ -> Left PreparationPhaseContradiction
markPreparationCancelling :: ChildPreparation -> State -> Either ProcessPreparationInvariant State
markPreparationCancelling reference = updatePreparation reference $ \entry -> case entry.phase of
  Preparing -> Right entry {phase = Cancelling, cancelled = True}
  Prepared -> Right entry {phase = Cancelling, cancelled = True}
  Cancelling -> Right entry
  _ -> Left PreparationPhaseContradiction
markPreparationEndReference :: ChildPreparation -> OracleRequestRef -> State -> Either ProcessPreparationInvariant State
markPreparationEndReference reference oracle state = do
  previous <- maybe (Left PreparationMissing) Right (lookupPreparation reference state)
  retained <-
    updatePreparation
      reference
      ( \entry -> case (entry.phase, entry.endReference) of
          (Cancelling, Nothing) -> Right entry {endReference = Just oracle}
          (Cancelling, Just prior) | prior == oracle -> Right entry
          _ -> Left PreparationPhaseContradiction
      )
      state
  pure (if previous.endReference == Nothing then registerResult (PreparationEndResult reference) oracle retained else retained)

markPreparationInstallationFailed :: ChildPreparation -> StartupError -> State -> Either ProcessPreparationInvariant State
markPreparationInstallationFailed reference failure = updatePreparation reference (\entry -> Right entry {phase = Cancelling, failure = Just failure})
markPreparationFailed :: ChildPreparation -> StartupError -> State -> Either ProcessPreparationInvariant State
markPreparationFailed reference failure = updatePreparation reference (\entry -> Right entry {phase = Terminal, failure = Just failure})
markPreparationTerminal :: ChildPreparation -> State -> Either ProcessPreparationInvariant State
markPreparationTerminal reference = updatePreparation reference (\entry -> Right entry {phase = Terminal})
markPreparationAttached :: ChildPreparation -> State -> Either ProcessPreparationInvariant State
markPreparationAttached reference = updatePreparation reference $ \entry -> case entry.phase of
  Prepared -> Right entry {phase = Attached}
  Attached -> Right entry
  _ -> Left PreparationPhaseContradiction
updatePreparation :: ChildPreparation -> (Preparation -> Either ProcessPreparationInvariant Preparation) -> State -> Either ProcessPreparationInvariant State
updatePreparation reference update state = do
  entry <- maybe (Left PreparationMissing) Right (lookupPreparation reference state)
  next <- update entry
  let retained = state {preparations = Map.insert reference next state.preparations}
  pure
    ( if preparationSemanticFacts entry == preparationSemanticFacts next
        then retained
        else notifyPreparationMutation reference next retained
    )

validateState :: State -> Either ProcessPreparationInvariant ()
validateState state = do
  validateScheduling state
  mapM_ validateDeferredBegin (Map.keys state.deferredBegins)
  mapM_ validPreparation (preparationEntries state)
  mapM_ validRequest (requestEntries state)
  mapM_ validOutgoing (outgoingEntries state)
  mapM_ validIncoming (incomingEntries state)
  where
    validateDeferredBegin key = case Map.lookup key state.requests of
      Just record | BeginChild {} <- record.command, LifecyclePending _ <- record.status -> Right ()
      _ -> Left PreparationPhaseContradiction
    validPreparation (reference, entry) = do
      unless
        (reference == entry.reference)
        (Left PreparationIdentityContradiction)
      request <-
        either
          (const (Left PreparationIdentityContradiction))
          Right
          (lifecycleRequestId (childPreparationScopeBytes reference) (childPreparationSessionOrdinal reference) (childPreparationOrdinal reference))
      case entry.source of
        Nothing -> validateBeginProvenance entry.beginReceipt entry.parent request reference
        Just source -> case lookupIncoming reference state of
          Just incoming | incoming.incomingSource == source && incoming.incomingOffer /= Nothing -> pure ()
          _ -> Left PreparationIdentityContradiction
      unless (not entry.installed || entry.startControl /= Nothing) (Left PreparationPhaseContradiction)
      unless (entry.startControl == Nothing || entry.startReference /= Nothing) (Left PreparationPhaseContradiction)
      unless
        (entry.phase `notElem` [Prepared, Attached] || entry.startControl /= Nothing)
        (Left PreparationPhaseContradiction)
      case entry.result of
        Just result -> unless (preparedChildPreparation result == reference) (Left PreparationResultContradiction)
        Nothing -> pure ()
    validIncoming (reference, incoming) = do
      unless (childPreparationScopeBytes reference == heraldEpochBytes incoming.incomingSource) (Left PreparationIdentityContradiction)
      unless (incoming.incomingOffer /= Nothing || incoming.incomingCancelled) (Left PreparationPhaseContradiction)
    validOutgoing (reference, outgoing) = do
      request <-
        either
          (const (Left PreparationIdentityContradiction))
          Right
          (lifecycleRequestId (childPreparationScopeBytes reference) (childPreparationSessionOrdinal reference) (childPreparationOrdinal reference))
      RetainedOutgoingPreparation _ provenance <- maybe (Left PreparationMissing) Right (Map.lookup reference state.outgoing)
      validateBeginProvenance provenance outgoing.outgoingParent request reference
      unless (Map.notMember reference state.preparations && Map.notMember reference state.incoming) (Left PreparationIdentityContradiction)
      case outgoing.outgoingResult of
        Nothing -> pure ()
        Just result -> unless (preparedChildPreparation result == reference && outgoing.outgoingChild /= Nothing) (Left PreparationResultContradiction)
    validateBeginProvenance provenance parent request reference =
      case (provenance, lookupRequest (lifecycleKey parent request) state) of
        (BeginReceiptRetained, Just record)
          | record.status == LifecycleCompleted (ChildPreparationAccepted reference) -> pure ()
        (BeginReceiptRetired, Nothing) -> pure ()
        _ -> Left PreparationIdentityContradiction
    validRequest (LifecycleKey _ request, record) = do
      unless
        ( lifecycleRequestIdScopeBytes request == heraldEpochBytes (applicationSessionIdHeraldEpoch record.session)
            && lifecycleRequestIdSessionOrdinal request == applicationSessionIdOrdinal record.session
        )
        (Left PreparationIdentityContradiction)
      case record.status of
        LifecycleCompleted result -> unless (lifecycleResultMatches record.command result) (Left PreparationResultContradiction)
        _ -> pure ()

-- | Stage witnesses preserve the original independent traversal boundaries.
data WorkStage key where
  PreparationStage :: WorkStage ChildPreparation
  OutgoingStage :: WorkStage ChildPreparation
  RequestStage :: WorkStage LifecycleKey
  ClaimStage :: WorkStage (ApplicationAttachment, InitialClaimId)
  OutgoingDeliveryStage :: WorkStage ChildPreparation
  IncomingDeliveryStage :: WorkStage ChildPreparation

data PreparationDependency
  = ObjectChanged !GlobalObjectId
  | ProcessChanged !ProcessEpochId
  | PossessionChanged !ProcessEpochId !GlobalObjectId
  | DeltaChanged !DeltaId
  | SortReadinessChanged !SortOccurrence
  | StructuralReadinessChanged
  | GateChanged
  | ActivityChanged
  | MembershipChanged
  | OracleBindingChanged
  | AttachmentChanged !ApplicationAttachment
  | SessionChanged !ApplicationSessionId
  | PreparationChanged !ChildPreparation
  | ReadinessChanged !ChildPreparation
  | PeerBindingChanged !HeraldEpoch
  deriving stock (Eq, Ord, Show)

data PreparationContext
  = PreparationContext
      !HeraldMembershipGenerationId
      !Bool
      !ApplicationGatePhase
      !Bool
      !(Maybe OracleBinding)
  deriving stock (Eq, Show)

workIndex :: WorkStage key -> State -> Work.WorkIndex key PreparationDependency
workIndex stage state = case stage of
  PreparationStage -> state.localWork
  OutgoingStage -> state.remoteWork
  RequestStage -> state.requestWork
  ClaimStage -> state.claimWork
  OutgoingDeliveryStage -> state.outgoingDeliveryWork
  IncomingDeliveryStage -> state.incomingDeliveryWork

putWorkIndex :: WorkStage key -> Work.WorkIndex key PreparationDependency -> State -> State
putWorkIndex stage index state = case stage of
  PreparationStage -> state {localWork = index}
  OutgoingStage -> state {remoteWork = index}
  RequestStage -> state {requestWork = index}
  ClaimStage -> state {claimWork = index}
  OutgoingDeliveryStage -> state {outgoingDeliveryWork = index}
  IncomingDeliveryStage -> state {incomingDeliveryWork = index}

workIds :: WorkStage key -> State -> Set key
workIds stage = Work.registeredKeys . workIndex stage

pendingWork :: WorkStage key -> State -> Set key
pendingWork stage = Work.pendingKeys . workIndex stage

nextWork :: (Ord key) => WorkStage key -> Set key -> Maybe key -> State -> Maybe key
nextWork stage inventory cursor = Work.nextWork inventory cursor . workIndex stage

consumeWork :: (Ord key) => WorkStage key -> key -> State -> State
consumeWork stage key state = putWorkIndex stage (Work.consumeWork key (workIndex stage state)) state

replaceWorkDependencies :: (Ord key) => WorkStage key -> key -> Set PreparationDependency -> State -> Either ProcessPreparationInvariant State
replaceWorkDependencies stage key dependencies state = do
  unless (Set.member key (workIds stage state)) (Left PreparationWorkIndexContradiction)
  pure (putWorkIndex stage (Work.replaceDependencies key dependencies (workIndex stage state)) state)

notifyPreparationDependencies :: Set PreparationDependency -> State -> State
notifyPreparationDependencies dependencies state
  | Set.null dependencies = state
  | otherwise = Set.foldl' (flip wakeReadinessConsumers) notified affected
  where
    (affected, readinessIndex) = Work.notifyDependencies dependencies state.readinessWork
    notified =
      state
        { localWork = snd (Work.notifyDependencies dependencies state.localWork),
          remoteWork = snd (Work.notifyDependencies dependencies state.remoteWork),
          requestWork = snd (Work.notifyDependencies dependencies state.requestWork),
          claimWork = snd (Work.notifyDependencies dependencies state.claimWork),
          outgoingDeliveryWork = snd (Work.notifyDependencies dependencies state.outgoingDeliveryWork),
          incomingDeliveryWork = snd (Work.notifyDependencies dependencies state.incomingDeliveryWork),
          readinessWork = readinessIndex,
          readinessOutcomes = Map.withoutKeys state.readinessOutcomes affected,
          lifecycleRetirementSessions = Set.foldl' retireSession state.lifecycleRetirementSessions dependencies
        }

    retireSession sessions (SessionChanged session)
      | Map.member session state.requestsBySession = insertSet session sessions
    retireSession sessions _ = sessions

seedPreparationContext :: PreparationContext -> State -> State
seedPreparationContext observed state = forceContext observed `seq` state {context = Just observed}

synchronizePreparationContext :: PreparationContext -> State -> State
synchronizePreparationContext observed state
  | state.context == Just observed = state
  | otherwise = forceContext observed `seq` (notifyPreparationDependencies changes state) {context = Just observed}
  where
    PreparationContext membership active gate membershipGate binding = observed
    changes = case state.context of
      Nothing -> Set.fromList [MembershipChanged, ActivityChanged, GateChanged, OracleBindingChanged]
      Just (PreparationContext oldMembership oldActive oldGate oldMembershipGate oldBinding) ->
        Set.fromList
          ( [MembershipChanged | membership /= oldMembership]
              <> [ActivityChanged | active /= oldActive]
              <> [GateChanged | gate /= oldGate || membershipGate /= oldMembershipGate]
              <> [OracleBindingChanged | binding /= oldBinding]
          )

forceContext :: PreparationContext -> ()
forceContext (PreparationContext _ _ _ _ binding) = maybe () (`seq` ()) binding

lookupReadiness :: ChildPreparation -> State -> Maybe (Either StartupError (Maybe [Text]))
lookupReadiness reference state = Map.lookup reference state.readinessOutcomes

-- | Materialize only the compact answer before installing it. Neither the
-- dependency set nor the result retains a prepared view of another owner.
retainReadiness :: ChildPreparation -> Set PreparationDependency -> Either StartupError (Maybe [Text]) -> State -> Either ProcessPreparationInvariant State
retainReadiness reference dependencies outcome state = do
  unless (Map.member reference state.preparations) (Left PreparationMissing)
  forceOutcome outcome
    `seq` pure
      state
        { readinessWork = Work.consumeWork reference (Work.registerWork reference dependencies state.readinessWork),
          readinessOutcomes = Map.insert reference outcome state.readinessOutcomes
        }
  where
    forceOutcome (Left (StartupRequiredObjectUnavailable key)) = key `seq` ()
    forceOutcome (Left failure) = failure `seq` ()
    forceOutcome (Right Nothing) = ()
    forceOutcome (Right (Just keys)) = foldr (\key rest -> key `seq` rest) () keys

invalidateReadiness :: ChildPreparation -> State -> State
invalidateReadiness reference state =
  wakeReadinessConsumers
    reference
    state
      { readinessOutcomes = Map.delete reference state.readinessOutcomes
      }

wakeReadinessConsumers :: ChildPreparation -> State -> State
wakeReadinessConsumers reference state =
  let dependencies = Set.singleton (ReadinessChanged reference)
      claims =
        maybe
          Set.empty
          (\preparation -> Map.findWithDefault Set.empty (preparationAttachment preparation) state.claimsByAttachment)
          (Map.lookup reference state.preparations)
   in state
        { requestWork =
            Work.markDirty
              (Map.findWithDefault Set.empty reference state.requestsByPreparation)
              (snd (Work.notifyDependencies dependencies state.requestWork)),
          claimWork = Work.markDirty claims (snd (Work.notifyDependencies dependencies state.claimWork)),
          incomingDeliveryWork =
            Work.markDirty
              (Set.singleton reference)
              (snd (Work.notifyDependencies dependencies state.incomingDeliveryWork))
        }

-- | Explicit comparison copies erase only scheduler observations. Registrations,
-- result observations and authoritative joins retain their ordinary equality.
clearPreparationScheduling :: State -> State
clearPreparationScheduling state =
  state
    { localWork = Work.clearPendingWork state.localWork,
      remoteWork = Work.clearPendingWork state.remoteWork,
      requestWork = Work.clearPendingWork state.requestWork,
      claimWork = Work.clearPendingWork state.claimWork,
      outgoingDeliveryWork = Work.clearPendingWork state.outgoingDeliveryWork,
      incomingDeliveryWork = Work.clearPendingWork state.incomingDeliveryWork,
      readinessWork = Work.clearPendingWork state.readinessWork,
      readinessOutcomes = Map.empty,
      context = Nothing,
      changedRequestKeys = Set.empty,
      lifecycleRetirementSessions = Set.empty
    }

takeChangedRequestKeys :: State -> (Set LifecycleKey, State)
takeChangedRequestKeys state =
  let keys = state.changedRequestKeys
   in keys `seq` (keys, state {changedRequestKeys = Set.empty})

-- | Only sessions with newly retained/changed receipts or an explicit session
-- change need closed-session retirement after the reply effects are assembled.
takeLifecycleRetirementSessions :: State -> (Set ApplicationSessionId, State)
takeLifecycleRetirementSessions state =
  let sessions = state.lifecycleRetirementSessions
   in sessions `seq` (sessions, state {lifecycleRetirementSessions = Set.empty})

preparationReferencesForProcess :: ProcessEpochId -> State -> Set ChildPreparation
preparationReferencesForProcess process state = Map.findWithDefault Set.empty process state.preparationsByProcess

requestEntriesForSession :: ApplicationSessionId -> State -> [(LifecycleKey, RequestRecord)]
requestEntriesForSession session state =
  [(key, record) | key <- Set.toAscList (Map.findWithDefault Set.empty session state.requestsBySession), Just record <- [Map.lookup key state.requests]]

ownEndRecovery :: ApplicationAttachment -> LifecycleRequestId -> State -> Maybe LifecycleStatus
ownEndRecovery attachment request state = case Set.toList (Map.findWithDefault Set.empty (ownEndRecoveryKey attachment request) state.ownEndRecoveries) of
  [key] -> (.status) <$> Map.lookup key state.requests
  _ -> Nothing

commandPreparation :: LifecycleCommand -> Maybe ChildPreparation
commandPreparation = \case
  AwaitPreparedChild reference -> Just reference
  AwaitChildReady reference -> Just reference
  CancelChild reference -> Just reference
  _ -> Nothing

indexNewRequest :: LifecycleKey -> RequestRecord -> State -> State
indexNewRequest key record state =
  refreshRequestWork
    key
    record
    state
      { requestsBySession = insertBucket record.session key state.requestsBySession,
        lifecycleRetirementSessions = insertSet record.session state.lifecycleRetirementSessions,
        ownEndRecoveries = case record.command of
          EndOwnProcess -> insertBucket (ownEndRecoveryKey record.attachment (lifecycleKeyRequest key)) key state.ownEndRecoveries
          _ -> state.ownEndRecoveries
      }

requestNeedsWork :: RequestRecord -> Bool
requestNeedsWork record =
  not (terminalStatus record.status) && case record.command of
    BeginChild {} -> False
    _ -> True

refreshRequestWork :: LifecycleKey -> RequestRecord -> State -> State
refreshRequestWork key record state
  | not (requestNeedsWork record) = state {requestWork = Work.removeWork key state.requestWork}
  | otherwise =
      state
        { requestWork =
            if Set.member key (Work.registeredKeys state.requestWork)
              then state.requestWork
              else Work.registerWork key dependencies state.requestWork,
          requestsByPreparation =
            maybe
              state.requestsByPreparation
              (\reference -> insertBucket reference key state.requestsByPreparation)
              (commandPreparation record.command)
        }
  where
    dependencies =
      Set.fromList
        ( SessionChanged record.session : case commandPreparation record.command of
            Nothing -> []
            Just reference -> [PreparationChanged reference, ReadinessChanged reference]
        )

removePendingRequestIndex :: LifecycleKey -> RequestRecord -> State -> State
removePendingRequestIndex key record state =
  state
    { requestsByPreparation =
        maybe
          state.requestsByPreparation
          (\reference -> deleteBucket reference key state.requestsByPreparation)
          (commandPreparation record.command)
    }

removeRequestIndexes :: LifecycleKey -> RequestRecord -> State -> State
removeRequestIndexes key record state =
  (removePendingRequestIndex key record (removeResultConsumer (OwnEndResult key) state))
    { requestsBySession = deleteBucket record.session key state.requestsBySession,
      ownEndRecoveries = deleteBucket (ownEndRecoveryKey record.attachment (lifecycleKeyRequest key)) key state.ownEndRecoveries,
      requestWork = Work.removeWork key state.requestWork,
      deferredBegins = Map.delete key state.deferredBegins,
      changedRequestKeys = Set.delete key state.changedRequestKeys,
      lifecycleRetirementSessions =
        if Map.findWithDefault Set.empty record.session state.requestsBySession == Set.singleton key
          then Set.delete record.session state.lifecycleRetirementSessions
          else state.lifecycleRetirementSessions
    }

preparationAttachment :: Preparation -> ApplicationAttachment
preparationAttachment = applicationAttachmentForProcess . processStartProcessEpochId . (.start)

-- Only small mutable phase/result facts participate in change detection.
preparationSemanticFacts :: Preparation -> (Maybe OracleRequestRef, Maybe OracleRequestRef, PreparationPhase, Maybe PreparedChild, Maybe ControlIndex, Bool, Maybe StartupError, Bool, Maybe HeraldEpoch)
preparationSemanticFacts entry = (entry.startReference, entry.endReference, entry.phase, entry.result, entry.startControl, entry.cancelled, entry.failure, entry.installed, entry.source)

preparationNeedsWork :: ChildPreparation -> Preparation -> State -> Bool
preparationNeedsWork reference entry state =
  entry.phase /= Terminal
    || any
      (`Map.member` state.resultReferences)
      [PreparationStartResult reference, PreparationEndResult reference]

refreshPreparationWork :: ChildPreparation -> Preparation -> State -> State
refreshPreparationWork reference entry state
  | not (preparationNeedsWork reference entry state) = state {localWork = Work.removeWork reference state.localWork}
  | otherwise =
      state
        { localWork =
            if Set.member reference (Work.registeredKeys state.localWork)
              then Work.markDirty (Set.singleton reference) state.localWork
              else Work.registerWork reference dependencies state.localWork
        }
  where
    process = processStartProcessEpochId entry.start
    dependencies = Set.fromList [ActivityChanged, OracleBindingChanged, ProcessChanged process, ObjectChanged (globalObjectIdFromProcessEpochId process)]

notifyPreparationMutation :: ChildPreparation -> Preparation -> State -> State
notifyPreparationMutation reference entry state =
  let changed =
        notifyPreparationDependencies
          (Set.fromList [PreparationChanged reference, AttachmentChanged (preparationAttachment entry)])
          (refreshPreparationWork reference entry (invalidateReadiness reference state))
   in if entry.phase == Terminal
        then changed {readinessWork = Work.removeWork reference changed.readinessWork}
        else changed

data PreparationResultConsumer
  = PreparationStartResult !ChildPreparation
  | PreparationEndResult !ChildPreparation
  | OwnEndResult !LifecycleKey
  deriving stock (Eq, Ord, Show)

unobservedPreparationResults :: ChildPreparation -> State -> [(PreparationResultConsumer, OracleRequestRef)]
unobservedPreparationResults reference state =
  [(consumer, oracle) | consumer <- [PreparationStartResult reference, PreparationEndResult reference], Just oracle <- [Map.lookup consumer state.resultReferences]]

unobservedOwnEndResult :: LifecycleKey -> State -> Maybe OracleRequestRef
unobservedOwnEndResult key state = Map.lookup (OwnEndResult key) state.resultReferences

registerResult :: PreparationResultConsumer -> OracleRequestRef -> State -> State
registerResult consumer reference state =
  dirtyResultConsumer
    consumer
    state
      { resultReferences = Map.insert consumer reference state.resultReferences,
        resultConsumers = insertBucket reference consumer state.resultConsumers
      }

notifyOracleResults :: Set OracleRequestRef -> State -> State
notifyOracleResults references state = Set.foldl' (flip dirtyResultConsumer) state consumers
  where
    consumers = Set.foldl' (\found reference -> found `Set.union` Map.findWithDefault Set.empty reference state.resultConsumers) Set.empty references

dirtyResultConsumer :: PreparationResultConsumer -> State -> State
dirtyResultConsumer consumer state = case consumer of
  OwnEndResult key -> state {requestWork = Work.markDirty (Set.singleton key) state.requestWork}
  PreparationStartResult reference -> dirtyPreparation reference
  PreparationEndResult reference -> dirtyPreparation reference
  where
    dirtyPreparation reference = case Map.lookup reference state.preparations of
      Nothing -> state
      Just entry -> refreshPreparationWork reference entry state

markResultObserved :: PreparationResultConsumer -> State -> Either ProcessPreparationInvariant State
markResultObserved consumer state = case Map.lookup consumer state.resultReferences of
  Nothing | Set.member consumer state.observedResults -> Right state
  Nothing -> Left PreparationWorkIndexContradiction
  Just reference ->
    let observed =
          state
            { resultReferences = Map.delete consumer state.resultReferences,
              resultConsumers = deleteBucket reference consumer state.resultConsumers,
              observedResults = Set.insert consumer state.observedResults
            }
     in Right
          ( case consumer of
              OwnEndResult _ -> observed
              PreparationStartResult preparation -> finish preparation observed
              PreparationEndResult preparation -> finish preparation observed
          )
  where
    finish reference observed = case Map.lookup reference observed.preparations of
      Just entry | not (preparationNeedsWork reference entry observed) -> observed {localWork = Work.removeWork reference observed.localWork}
      _ -> observed

removeResultConsumer :: PreparationResultConsumer -> State -> State
removeResultConsumer consumer state =
  state
    { resultReferences = Map.delete consumer state.resultReferences,
      resultConsumers = maybe state.resultConsumers (\reference -> deleteBucket reference consumer state.resultConsumers) (Map.lookup consumer state.resultReferences),
      observedResults = Set.delete consumer state.observedResults
    }

insertSet :: (Ord value) => value -> Set value -> Set value
insertSet value values = value `seq` Set.insert value values

insertBucket :: (Ord key, Ord value) => key -> value -> Map key (Set value) -> Map key (Set value)
insertBucket key value retained = key `seq` value `seq` Map.insertWith Set.union key (Set.singleton value) retained

deleteBucket :: (Ord key, Ord value) => key -> value -> Map key (Set value) -> Map key (Set value)
deleteBucket key value = Map.update (\values -> let remaining = Set.delete value values in if Set.null remaining then Nothing else Just remaining) key

outgoingSemanticFacts :: OutgoingPreparation -> (RemotePreparationStatus, Maybe RemoteChild, Maybe PreparedChild, Bool)
outgoingSemanticFacts entry = (entry.outgoingStatus, entry.outgoingChild, entry.outgoingResult, entry.outgoingCancelled)

remoteTerminal :: RemotePreparationStatus -> Bool
remoteTerminal = \case
  RemotePreparationCancelled -> True
  RemotePreparationFailed _ -> True
  _ -> False

remoteStatusChild :: RemotePreparationStatus -> Maybe RemoteChild
remoteStatusChild = \case
  RemotePreparationAvailable child _ -> Just child
  RemotePreparationClaimed child -> Just child
  _ -> Nothing

refreshOutgoingWork :: ChildPreparation -> OutgoingPreparation -> State -> State
refreshOutgoingWork reference entry state
  | remoteTerminal entry.outgoingStatus =
      state
        { remoteWork = Work.removeWork reference state.remoteWork,
          outgoingDeliveryWork = Work.removeWork reference state.outgoingDeliveryWork
        }
  | otherwise =
      state
        { remoteWork =
            if Set.member reference (Work.registeredKeys state.remoteWork)
              then Work.markDirty (Set.singleton reference) state.remoteWork
              else Work.registerWork reference semanticDependencies state.remoteWork,
          outgoingDeliveryWork =
            if Set.member reference (Work.registeredKeys state.outgoingDeliveryWork)
              then Work.markDirty (Set.singleton reference) state.outgoingDeliveryWork
              else Work.registerWork reference deliveryDependencies state.outgoingDeliveryWork
        }
  where
    semanticDependencies =
      Set.fromList
        ( [ActivityChanged, MembershipChanged, ProcessChanged entry.outgoingParent, AttachmentChanged (applicationAttachmentForProcess entry.outgoingParent)]
            <> case entry.outgoingChild of Nothing -> []; Just (RemoteChild process _ _) -> [ProcessChanged process, ObjectChanged (globalObjectIdFromProcessEpochId process)]
        )
    deliveryDependencies = Set.fromList [ActivityChanged, PeerBindingChanged entry.outgoingTarget]

notifyOutgoingMutation :: ChildPreparation -> OutgoingPreparation -> State -> State
notifyOutgoingMutation reference entry state =
  let changed = refreshOutgoingWork reference entry (notifyPreparationDependencies (Set.singleton (PreparationChanged reference)) state)
   in changed {requestWork = Work.markDirty (Map.findWithDefault Set.empty reference changed.requestsByPreparation) changed.requestWork}

dirtyIncomingDelivery :: ChildPreparation -> State -> State
dirtyIncomingDelivery reference state = state {incomingDeliveryWork = Work.markDirty (Set.singleton reference) state.incomingDeliveryWork}

retainIncomingOffer :: ChildPreparation -> HeraldEpoch -> HeraldLocator -> ByteString -> State -> Either ProcessPreparationInvariant State
retainIncomingOffer reference source locator bytes state = do
  retained <- case Map.lookup reference state.incoming of
    Nothing -> retainIncoming reference (IncomingPreparation source (Just (locator, bytes)) False Nothing Nothing) state
    Just previous -> do
      unless (previous.incomingSource == source && maybe True (== (locator, bytes)) previous.incomingOffer) (Left PreparationIdentityContradiction)
      pure state {incoming = Map.insert reference previous {incomingOffer = Just (locator, bytes), incomingLastReply = Nothing} state.incoming}
  pure (dirtyIncomingDelivery reference retained)

sealIncomingCancellation :: ChildPreparation -> HeraldEpoch -> State -> Either ProcessPreparationInvariant State
sealIncomingCancellation reference source state = do
  retained <- case Map.lookup reference state.incoming of
    Nothing -> retainIncoming reference (IncomingPreparation source Nothing True Nothing Nothing) state
    Just previous -> do
      unless (previous.incomingSource == source) (Left PreparationIdentityContradiction)
      replaceIncoming reference previous {incomingCancelled = True, incomingLastReply = Nothing} state
  pure (dirtyIncomingDelivery reference retained)

failIncoming :: ChildPreparation -> StartupError -> State -> Either ProcessPreparationInvariant State
failIncoming reference failure state = do
  entry <- maybe (Left PreparationMissing) Right (Map.lookup reference state.incoming)
  replaceIncoming reference entry {incomingFailure = Just failure} state

requestIncomingReply :: ChildPreparation -> State -> Either ProcessPreparationInvariant State
requestIncomingReply reference state = do
  entry <- maybe (Left PreparationMissing) Right (Map.lookup reference state.incoming)
  pure (dirtyIncomingDelivery reference state {incoming = Map.insert reference entry {incomingLastReply = Nothing} state.incoming})

recordIncomingReply :: ChildPreparation -> PeerBinding -> RemotePreparationStatus -> State -> Either ProcessPreparationInvariant State
recordIncomingReply reference binding status state = do
  entry <- maybe (Left PreparationMissing) Right (Map.lookup reference state.incoming)
  pure state {incoming = Map.insert reference entry {incomingLastReply = Just (binding, status)} state.incoming}

receiveOutgoingStatus :: ChildPreparation -> RemotePreparationStatus -> State -> Either ProcessPreparationInvariant State
receiveOutgoingStatus reference status state = do
  entry <- maybe (Left PreparationMissing) Right (lookupOutgoing reference state)
  replaceOutgoing
    reference
    entry
      { outgoingStatus = status,
        outgoingChild = case remoteStatusChild status of Just child -> Just child; Nothing -> entry.outgoingChild
      }
    state

retainOutgoingResult :: ChildPreparation -> PreparedChild -> State -> Either ProcessPreparationInvariant State
retainOutgoingResult reference result state = do
  entry <- maybe (Left PreparationMissing) Right (lookupOutgoing reference state)
  replaceOutgoing reference entry {outgoingResult = Just result} state

data OutgoingCancellationMode = OrdinaryCancellation | RetryRemoteCleanup
  deriving stock (Eq, Show)

cancelOutgoing :: ChildPreparation -> OutgoingCancellationMode -> State -> Either ProcessPreparationInvariant State
cancelOutgoing reference mode state = do
  entry <- maybe (Left PreparationMissing) Right (lookupOutgoing reference state)
  replaceOutgoing
    reference
    entry
      { outgoingCancelled = True,
        outgoingStatus = if mode == RetryRemoteCleanup then RemotePreparationPending [] else entry.outgoingStatus,
        outgoingCancelBinding = if mode == RetryRemoteCleanup then Nothing else entry.outgoingCancelBinding
      }
    state

recordOutgoingDelivery :: ChildPreparation -> PeerBinding -> Bool -> State -> Either ProcessPreparationInvariant State
recordOutgoingDelivery reference binding cancellationSent state = do
  RetainedOutgoingPreparation entry provenance <- maybe (Left PreparationMissing) Right (Map.lookup reference state.outgoing)
  let next = entry {outgoingOfferBinding = Just binding, outgoingCancelBinding = if cancellationSent then Just binding else entry.outgoingCancelBinding}
  pure state {outgoing = Map.insert reference (RetainedOutgoingPreparation next provenance) state.outgoing}

preparationContext :: State -> Maybe PreparationContext
preparationContext state = state.context

lookupPendingClaim :: ApplicationAttachment -> InitialClaimId -> State -> Maybe CandidateApplicationLane
lookupPendingClaim attachment claim state = Map.lookup (attachment, claim) state.pendingClaims

claimWorkKey :: ApplicationAttachment -> InitialClaimId -> (ApplicationAttachment, InitialClaimId)
claimWorkKey attachment claim = attachment `seq` claim `seq` (attachment, claim)

ownEndRecoveryKey :: ApplicationAttachment -> LifecycleRequestId -> (ApplicationAttachment, LifecycleRequestId)
ownEndRecoveryKey attachment request = attachment `seq` request `seq` (attachment, request)

-- | Reconstruct owner joins from authoritative transcripts, independently of
-- incremental registration and notification. Dynamic declarations additionally
-- satisfy the small WorkIndex inverse-membership law.
validateScheduling :: State -> Either ProcessPreparationInvariant ()
validateScheduling state = unless validIndexes (Left PreparationWorkIndexContradiction)
  where
    grouped pairs = Map.fromListWith Set.union [(key, Set.singleton value) | (key, value) <- pairs]
    localKeys = Set.fromList [reference | (reference, entry) <- Map.toAscList state.preparations, preparationNeedsWork reference entry state]
    outgoingKeys = Set.fromList [reference | (reference, RetainedOutgoingPreparation entry _) <- Map.toAscList state.outgoing, not (remoteTerminal entry.outgoingStatus)]
    requestKeys = Set.fromList [key | (key, record) <- Map.toAscList state.requests, requestNeedsWork record]
    allReferences =
      Map.fromList
        ( [(PreparationStartResult reference, oracle) | (reference, entry) <- Map.toAscList state.preparations, Just oracle <- [entry.startReference]]
            <> [(PreparationEndResult reference, oracle) | (reference, entry) <- Map.toAscList state.preparations, Just oracle <- [entry.endReference]]
            <> [(OwnEndResult key, oracle) | (key, record) <- Map.toAscList state.requests, Just oracle <- [record.ownEndReference]]
        )
    expectedReferences = Map.withoutKeys allReferences state.observedResults
    validIndexes =
      and
        [ Work.valid state.localWork,
          Work.valid state.remoteWork,
          Work.valid state.requestWork,
          Work.valid state.claimWork,
          Work.valid state.outgoingDeliveryWork,
          Work.valid state.incomingDeliveryWork,
          Work.valid state.readinessWork,
          Work.registeredKeys state.localWork == localKeys,
          Work.registeredKeys state.remoteWork == outgoingKeys,
          Work.registeredKeys state.outgoingDeliveryWork == outgoingKeys,
          Work.registeredKeys state.incomingDeliveryWork == Map.keysSet state.incoming,
          Work.registeredKeys state.requestWork == requestKeys,
          Work.registeredKeys state.claimWork == Map.keysSet state.pendingClaims,
          Work.registeredKeys state.readinessWork `Set.isSubsetOf` Map.keysSet state.preparations,
          Map.keysSet state.readinessOutcomes `Set.isSubsetOf` Work.registeredKeys state.readinessWork,
          state.preparationsByProcess == grouped [(processStartProcessEpochId entry.start, reference) | (reference, entry) <- Map.toAscList state.preparations],
          state.preparationsByAttachment == grouped [(preparationAttachment entry, reference) | (reference, entry) <- Map.toAscList state.preparations],
          state.requestsByPreparation == grouped [(reference, key) | (key, record) <- Map.toAscList state.requests, requestNeedsWork record, Just reference <- [commandPreparation record.command]],
          state.requestsBySession == grouped [(record.session, key) | (key, record) <- Map.toAscList state.requests],
          state.ownEndRecoveries == grouped [((record.attachment, lifecycleKeyRequest key), key) | (key, record) <- Map.toAscList state.requests, record.command == EndOwnProcess],
          state.claimsByAttachment == grouped [(attachment, key) | key@(attachment, _) <- Map.keys state.pendingClaims],
          state.claimsByLane == Map.fromList [(lane, key) | (key, lane) <- Map.toAscList state.pendingClaims],
          Map.size state.claimsByLane == Map.size state.pendingClaims,
          state.resultReferences == expectedReferences,
          state.resultConsumers == grouped [(oracle, consumer) | (consumer, oracle) <- Map.toAscList expectedReferences],
          state.observedResults `Set.isSubsetOf` Map.keysSet allReferences,
          state.changedRequestKeys `Set.isSubsetOf` Map.keysSet state.requests,
          state.lifecycleRetirementSessions `Set.isSubsetOf` Map.keysSet state.requestsBySession
        ]
