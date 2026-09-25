-- | Generation-qualified physical registry and writer commands.
module Eclips.Herald.Runtime.Internal.Connection
  ( ApplicationPhase (..),
    ConfiguredAdministrationPhase (..),
    PeerPhase (..),
    ConnectionSlot (..),
    WriterCommand (..),
    Registry (..),
    newRegistry,
    allocateApplication,
    allocateAdministration,
    allocateConfiguredAdministration,
    allocatePeer,
    lookupCurrentSlot,
    replaceSlot,
    removeCurrentSlot,
    registrySlots,
    slotSource,
    slotWriterQueue,
    slotRetirementAction,
    slotCloseAction,
    findApplicationCandidate,
    findApplicationBinding,
    findApplicationSession,
    claimApplicationSessionDisposal,
    findConfiguredAdministrationCandidate,
    findConfiguredAdministrationBinding,
    findPeerCandidate,
    findPeerBinding,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    TQueue,
    TVar,
    newTQueue,
    newTVar,
    readTVar,
    stateTVar,
    writeTVar,
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Herald.Administration
  ( AdministrationBinding,
    FinalAdminReply,
  )
import Eclips.Herald.Administration.RPC (AdministrationOutbound)
import Eclips.Herald.Application.RPC (ApplicationOutbound)
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    ApplicationSessionId,
    sessionBindingSessionId,
  )
import Eclips.Herald.Discovery
  ( ConnectionNonce,
    PeerBinding,
    PeerCandidate,
    PeerHelloDisposition,
  )
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.EffectBatch (PeerProtocolDisposition)
import Eclips.Herald.Input
  ( CandidateAdministrationLane,
    CandidateApplicationLane,
  )
import Eclips.Herald.Input qualified as Input
import Eclips.Herald.PeerDispatch (PeerLogicalAttempt)
import Eclips.Herald.Runtime.Internal.Arbiter
  ( SourceFamily (..),
    SourceId (..),
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( ObligationOrigin,
    OutcomeCellState,
  )
import Eclips.Herald.Runtime.Internal.Types
  ( AdministrationPlane,
    ApplicationPlane,
    ConfiguredAdministrationPlane,
    ConnectionRef (..),
    HeraldRuntimeHandlers,
    PeerPlane,
    RuntimeAdministrationConnectionHandlers (..),
    RuntimeApplicationConnectionHandlers (..),
    RuntimeConfiguredAdministrationConnectionHandlers (..),
    RuntimePeerConnectionHandlers (..),
    RuntimeToken,
  )
import Eclips.Protocol.Peer.Types (PeerEnvelope)

data ApplicationPhase
  = ApplicationCandidate CandidateApplicationLane
  | ApplicationDispositionPending CandidateApplicationLane
  | ApplicationClaimPending CandidateApplicationLane
  | ApplicationAdmissionDeferred CandidateApplicationLane
  | ApplicationEstablished ApplicationSessionBinding
  | ApplicationClosing (Maybe ApplicationSessionBinding)
  deriving stock (Eq, Show)

data ConfiguredAdministrationPhase
  = ConfiguredAdministrationCandidate CandidateAdministrationLane
  | ConfiguredAdministrationDispositionPending CandidateAdministrationLane
  | ConfiguredAdministrationEstablished AdministrationBinding
  | ConfiguredAdministrationClosing (Maybe AdministrationBinding)
  deriving stock (Eq, Show)

data PeerPhase
  = PeerFresh ConnectionNonce
  | PeerCandidateRoutePending ConnectionNonce
  | PeerCandidateCurrent PeerCandidate
  | PeerDispositionPending PeerCandidate
  | PeerEstablished PeerBinding
  | PeerClosing (Maybe PeerBinding)
  deriving stock (Eq, Show)

data WriterCommand
  = WriteApplication ApplicationOutbound
  | WriteConfiguredAdministration AdministrationOutbound
  | WriteAdministration (Maybe FinalAdminReply)
  | WriteFinalAdministration (Maybe FinalAdminReply) (TMVar ())
  | WritePeerCandidate PeerCandidate PeerEnvelope
  | WritePeerDisposition PeerCandidate PeerHelloDisposition
  | WritePeerControl PeerBinding PeerEnvelope
  | WritePeerRejection PeerBinding PeerProtocolDisposition
  | WritePeerItem
      PeerBinding
      PeerLogicalAttempt
      PeerEnvelope
      ObligationOrigin
      (TVar OutcomeCellState)
  | CloseWriter (Maybe (TMVar ()))
  deriving stock (Eq)

instance Show WriterCommand where
  show = \case
    WriteApplication outbound -> "WriteApplication " <> show outbound
    WriteConfiguredAdministration outbound ->
      "WriteConfiguredAdministration " <> show outbound
    WriteAdministration reply -> "WriteAdministration " <> show reply
    WriteFinalAdministration reply _ -> "WriteFinalAdministration " <> show reply
    WritePeerCandidate candidate hello -> "WritePeerCandidate " <> show candidate <> " " <> show hello
    WritePeerDisposition candidate disposition -> "WritePeerDisposition " <> show candidate <> " " <> show disposition
    WritePeerControl binding control -> "WritePeerControl " <> show binding <> " " <> show control
    WritePeerRejection binding disposition -> "WritePeerRejection " <> show binding <> " " <> show disposition
    WritePeerItem binding _ _ origin _ -> "WritePeerItem " <> show binding <> " " <> show origin
    CloseWriter _ -> "CloseWriter"

data ConnectionSlot
  = ApplicationSlot
      (ConnectionRef ApplicationPlane)
      ApplicationPhase
      RuntimeApplicationConnectionHandlers
      (TQueue WriterCommand)
  | AdministrationSlot
      (ConnectionRef AdministrationPlane)
      RuntimeAdministrationConnectionHandlers
      (TQueue WriterCommand)
  | ConfiguredAdministrationSlot
      (ConnectionRef ConfiguredAdministrationPlane)
      ConfiguredAdministrationPhase
      RuntimeConfiguredAdministrationConnectionHandlers
      (TQueue WriterCommand)
  | PeerSlot
      (ConnectionRef PeerPlane)
      PeerPhase
      RuntimePeerConnectionHandlers
      (TQueue WriterCommand)

instance Show ConnectionSlot where
  show = \case
    ApplicationSlot reference phase _ _ -> "ApplicationSlot " <> show reference <> " " <> show phase
    AdministrationSlot reference _ _ -> "AdministrationSlot " <> show reference
    ConfiguredAdministrationSlot reference phase _ _ ->
      "ConfiguredAdministrationSlot " <> show reference <> " " <> show phase
    PeerSlot reference phase _ _ -> "PeerSlot " <> show reference <> " " <> show phase

data Registry = Registry
  { registryToken :: RuntimeToken,
    registryNextOrdinal :: TVar Word64,
    registrySlotMap :: TVar (Map Word64 ConnectionSlot),
    registryAdministrationUsed :: TVar Bool,
    registryRuntimeHandlers :: HeraldRuntimeHandlers
  }

newRegistry :: RuntimeToken -> HeraldRuntimeHandlers -> STM Registry
newRegistry token handlers =
  Registry token <$> newTVar 0 <*> newTVar Map.empty <*> newTVar False <*> pure handlers

allocateApplication ::
  Registry ->
  RuntimeApplicationConnectionHandlers ->
  STM (ConnectionRef ApplicationPlane, ConnectionSlot)
allocateApplication registry handlers = do
  (reference, queue) <- allocate registry
  let slot =
        ApplicationSlot
          reference
          (ApplicationCandidate (candidateFor reference))
          handlers
          queue
  insertSlot registry reference slot
  pure (reference, slot)

allocateAdministration ::
  Registry ->
  RuntimeAdministrationConnectionHandlers ->
  STM (Maybe (ConnectionRef AdministrationPlane, ConnectionSlot))
allocateAdministration registry handlers = do
  used <- readTVar (registryAdministrationUsed registry)
  if used
    then pure Nothing
    else do
      writeTVar (registryAdministrationUsed registry) True
      (reference, queue) <- allocate registry
      let slot = AdministrationSlot reference handlers queue
      insertSlot registry reference slot
      pure (Just (reference, slot))

allocateConfiguredAdministration ::
  Registry ->
  RuntimeConfiguredAdministrationConnectionHandlers ->
  STM (ConnectionRef ConfiguredAdministrationPlane, ConnectionSlot)
allocateConfiguredAdministration registry handlers = do
  (reference, queue) <- allocate registry
  let slot =
        ConfiguredAdministrationSlot
          reference
          (ConfiguredAdministrationCandidate (configuredAdministrationCandidateFor reference))
          handlers
          queue
  insertSlot registry reference slot
  pure (reference, slot)

allocatePeer ::
  Registry ->
  RuntimePeerConnectionHandlers ->
  STM (ConnectionRef PeerPlane, ConnectionSlot)
allocatePeer registry handlers = do
  (reference, queue) <- allocate registry
  let slot = PeerSlot reference (PeerFresh (nonceFor reference)) handlers queue
  insertSlot registry reference slot
  pure (reference, slot)

lookupCurrentSlot :: Registry -> ConnectionRef plane -> STM (Maybe ConnectionSlot)
lookupCurrentSlot registry reference
  | connectionToken reference /= registryToken registry = pure Nothing
  | otherwise = Map.lookup (connectionOrdinal reference) <$> readTVar (registrySlotMap registry)

replaceSlot :: Registry -> ConnectionSlot -> STM ()
replaceSlot registry slot = do
  slots <- readTVar (registrySlotMap registry)
  writeTVar (registrySlotMap registry) (Map.insert (slotOrdinal slot) slot slots)

removeCurrentSlot :: Registry -> ConnectionRef plane -> STM (Maybe ConnectionSlot)
removeCurrentSlot registry reference = do
  current <- lookupCurrentSlot registry reference
  case current of
    Nothing -> pure Nothing
    Just slot -> do
      slots <- readTVar (registrySlotMap registry)
      writeTVar (registrySlotMap registry) (Map.delete (connectionOrdinal reference) slots)
      pure (Just slot)

registrySlots :: Registry -> STM [ConnectionSlot]
registrySlots registry = Map.elems <$> readTVar (registrySlotMap registry)

slotSource :: ConnectionSlot -> SourceId
slotSource = \case
  ApplicationSlot reference _ _ _ -> SourceId ApplicationSources (connectionOrdinal reference)
  AdministrationSlot reference _ _ -> SourceId AdministrationSources (connectionOrdinal reference)
  ConfiguredAdministrationSlot reference _ _ _ ->
    SourceId ConfiguredAdministrationSources (connectionOrdinal reference)
  PeerSlot reference _ _ _ -> SourceId PeerSources (connectionOrdinal reference)

slotWriterQueue :: ConnectionSlot -> TQueue WriterCommand
slotWriterQueue = \case
  ApplicationSlot _ _ _ queue -> queue
  AdministrationSlot _ _ queue -> queue
  ConfiguredAdministrationSlot _ _ _ queue -> queue
  PeerSlot _ _ _ queue -> queue

-- | Atomically invalidate a transport-local projection of a registry lease.
-- Most handlers have no such projection; peer TCP uses this to prevent a
-- dequeued old-route disposition from resurrecting a stale physical row after
-- an owner handoff.
slotRetirementAction :: ConnectionSlot -> STM ()
slotRetirementAction = \case
  PeerSlot _ _ (RuntimePeerConnectionHandlers _ _ _ _ _ retire _) _ -> retire
  _ -> pure ()

slotCloseAction :: ConnectionSlot -> IO ()
slotCloseAction = \case
  ApplicationSlot _ _ (RuntimeApplicationConnectionHandlers _ _ close) _ -> close
  AdministrationSlot _ (RuntimeAdministrationConnectionHandlers _ close) _ -> close
  ConfiguredAdministrationSlot _ _ (RuntimeConfiguredAdministrationConnectionHandlers _ close) _ -> close
  PeerSlot _ _ (RuntimePeerConnectionHandlers _ _ _ _ _ _ close) _ -> close

findApplicationCandidate :: Registry -> CandidateApplicationLane -> STM (Maybe ConnectionSlot)
findApplicationCandidate registry candidate =
  findSlot matches <$> registrySlots registry
  where
    matches (ApplicationSlot _ phase _ _) = case phase of
      ApplicationCandidate current -> current == candidate
      ApplicationDispositionPending current -> current == candidate
      ApplicationClaimPending current -> current == candidate
      ApplicationAdmissionDeferred current -> current == candidate
      ApplicationEstablished _ -> False
      ApplicationClosing _ -> False
    matches _ = False

findApplicationBinding :: Registry -> ApplicationSessionBinding -> STM (Maybe ConnectionSlot)
findApplicationBinding registry binding =
  findSlot matches <$> registrySlots registry
  where
    matches (ApplicationSlot _ (ApplicationEstablished current) _ _) = current == binding
    matches _ = False

-- | Find the current established physical projection for one logical session,
-- irrespective of its binding generation. Terminal session dispositions are
-- session facts and must therefore follow a successful concurrent resume to
-- the replacement route rather than target the stale binding that happened to
-- observe retirement.
findApplicationSession :: Registry -> ApplicationSessionId -> STM (Maybe ConnectionSlot)
findApplicationSession registry session =
  findSlot matches <$> registrySlots registry
  where
    matches (ApplicationSlot _ (ApplicationEstablished binding) _ _) =
      sessionBindingSessionId binding == session
    matches _ = False

-- | Atomically claim the current established route for one terminal session
-- disposition. The returned slot still carries its writer queue, while the
-- registry has already become ingress-inert. A repeated claim observes no
-- established route. 'ApplicationClosing Nothing' is intentional: removal of
-- this terminal route must not generate a synthetic binding-loss observation.
claimApplicationSessionDisposal ::
  Registry ->
  ApplicationSessionId ->
  STM (Maybe ConnectionSlot)
claimApplicationSessionDisposal registry session = do
  target <- findApplicationSession registry session
  case target of
    Just slot@(ApplicationSlot reference _ handlers queue) -> do
      replaceSlot
        registry
        (ApplicationSlot reference (ApplicationClosing Nothing) handlers queue)
      pure (Just slot)
    _ -> pure Nothing

findConfiguredAdministrationCandidate ::
  Registry ->
  CandidateAdministrationLane ->
  STM (Maybe ConnectionSlot)
findConfiguredAdministrationCandidate registry candidate =
  findSlot matches <$> registrySlots registry
  where
    matches (ConfiguredAdministrationSlot _ phase _ _) = case phase of
      ConfiguredAdministrationCandidate current -> current == candidate
      ConfiguredAdministrationDispositionPending current -> current == candidate
      ConfiguredAdministrationEstablished _ -> False
      ConfiguredAdministrationClosing _ -> False
    matches _ = False

findConfiguredAdministrationBinding ::
  Registry ->
  AdministrationBinding ->
  STM (Maybe ConnectionSlot)
findConfiguredAdministrationBinding registry binding =
  findSlot matches <$> registrySlots registry
  where
    matches (ConfiguredAdministrationSlot _ (ConfiguredAdministrationEstablished current) _ _) =
      current == binding
    matches _ = False

findPeerCandidate :: Registry -> PeerCandidate -> STM (Maybe ConnectionSlot)
findPeerCandidate registry candidate =
  findSlot matches <$> registrySlots registry
  where
    matches (PeerSlot _ phase _ _) = case phase of
      PeerCandidateCurrent current -> current == candidate
      PeerDispositionPending current -> current == candidate
      PeerFresh _ -> False
      PeerCandidateRoutePending _ -> False
      PeerEstablished _ -> False
      PeerClosing _ -> False
    matches _ = False

findPeerBinding :: Registry -> PeerBinding -> STM (Maybe ConnectionSlot)
findPeerBinding registry binding =
  findSlot matches <$> registrySlots registry
  where
    matches (PeerSlot _ (PeerEstablished current) _ _) = current == binding
    matches _ = False

allocate :: Registry -> STM (ConnectionRef plane, TQueue WriterCommand)
allocate registry = do
  ordinal <- stateTVar (registryNextOrdinal registry) (\value -> (value, value + 1))
  queue <- newTQueue
  pure (ConnectionRef (registryToken registry) ordinal, queue)

insertSlot :: Registry -> ConnectionRef plane -> ConnectionSlot -> STM ()
insertSlot registry reference slot = do
  slots <- readTVar (registrySlotMap registry)
  writeTVar (registrySlotMap registry) (Map.insert (connectionOrdinal reference) slot slots)

candidateFor :: ConnectionRef plane -> CandidateApplicationLane
candidateFor = Input.candidateApplicationLane . connectionOrdinal

configuredAdministrationCandidateFor ::
  ConnectionRef plane -> CandidateAdministrationLane
configuredAdministrationCandidateFor =
  Input.candidateAdministrationLane . connectionOrdinal

nonceFor :: ConnectionRef plane -> ConnectionNonce
nonceFor = Discovery.connectionNonce . connectionOrdinal

connectionToken :: ConnectionRef plane -> RuntimeToken
connectionToken (ConnectionRef token _) = token

connectionOrdinal :: ConnectionRef plane -> Word64
connectionOrdinal (ConnectionRef _ ordinal) = ordinal

slotOrdinal :: ConnectionSlot -> Word64
slotOrdinal = \case
  ApplicationSlot reference _ _ _ -> connectionOrdinal reference
  AdministrationSlot reference _ _ -> connectionOrdinal reference
  ConfiguredAdministrationSlot reference _ _ _ -> connectionOrdinal reference
  PeerSlot reference _ _ _ -> connectionOrdinal reference

findSlot :: (ConnectionSlot -> Bool) -> [ConnectionSlot] -> Maybe ConnectionSlot
findSlot predicate = foldr (\slot found -> if predicate slot then Just slot else found) Nothing
