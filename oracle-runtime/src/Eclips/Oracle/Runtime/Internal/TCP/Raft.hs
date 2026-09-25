-- | Current ERFT transport with one deterministic duplex binding per registered replica
-- pair. The smaller node initiates; both peers may send requests on that
-- physical connection.
module Eclips.Oracle.Runtime.Internal.TCP.Raft
  ( RaftTcpManager,
    newRaftTcpManager,
    startRaftTcpManager,
    stopRaftTcpManager,
    raftTcpTransport,
    updateRaftTcpRegistrations,
    handleRaftTcpConnection,
  )
where

import Control.Concurrent
  ( forkIOWithUnmask,
    killThread,
    threadDelay,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
  )
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    orElse,
    readTVar,
    readTVarIO,
    retry,
    stateTVar,
    writeTVar,
  )
import Control.Exception
  ( IOException,
    bracket,
    finally,
    mask,
    mask_,
    try,
  )
import Control.Monad
  ( forM,
    forM_,
    guard,
    void,
    when,
  )
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkedOracleRaftConfigurationDigest,
  )
import Eclips.Oracle.Identity (raftConfigurationDigestBytes)
import Eclips.Oracle.Runtime.Internal.Connection (offerRaftRequestInput)
import Eclips.Oracle.Runtime.Internal.LeaseQueue
  ( LeaseClaim,
    LeaseQueue,
    claimCurrentLeaseItem,
    completeLeaseClaim,
    leaseClaimItem,
    newLeaseQueueIO,
    retireLeaseClaims,
    writeLeaseQueue,
  )
import Eclips.Oracle.Runtime.Internal.ReplicaRegistration
  ( ReplicaRegistrationAdmission (..),
    compareReplicaRegistration,
  )
import Eclips.Oracle.Runtime.Internal.RetainedRequest
  ( RetainedRequestClaim,
    RetainedRequestQueue,
    acknowledgeRetainedRequest,
    claimRetainedRequest,
    completeRetainedRequestSend,
    newRetainedRequestQueueIO,
    offerRetainedRequest,
    retainedRequestClaimGeneration,
    retainedRequestClaimPayload,
    retireRetainedRequestLease,
  )
import Eclips.Oracle.Runtime.Internal.Scope (signalRuntimeFailure)
import Eclips.Oracle.Runtime.Internal.TCP.ManagedWorkers
  ( ManagedWorkers,
    managedWorkerOwners,
    newManagedWorkersIO,
    retireManagedWorkers,
    startManagedWorker,
  )
import Eclips.Oracle.Runtime.Internal.TCP.Retirement
  ( retireSocketQuietly,
  )
import Eclips.Oracle.Runtime.Internal.TCP.Retry
import Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( ResolvedTcpEndpoint (..),
    closeSocketQuietly,
    connectTcpEndpoint,
    rawFrameBody,
    receiveRawFrame,
    sendSocketBytes,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( OracleRuntime (..),
    OracleRuntimeFailure (RuntimeEssentialChildFailed),
    RuntimeRaftTransport (..),
  )
import Eclips.Oracle.Voter
import Eclips.Protocol.Raft.Codec (decodeRaftEnvelope)
import Eclips.Protocol.Raft.Frame
  ( RaftFrameContinuation (..),
    RaftFrameDecoder,
    RaftFrameFeedResult (..),
    encodeRaftFrame,
    feedRaftFrame,
    initialRaftFrameDecoder,
    raftFrameMagic,
  )
import Eclips.Protocol.Raft.Types
  ( RaftGenesisDigestDto,
    RaftNodeIdDto,
    RaftProtocolEnvelope (..),
    RaftRpcDto,
    RaftRpcKind (..),
    raftConnectionContext,
    raftGenesisDigestDto,
    raftHelloClaim,
    raftHelloSource,
    raftHelloTarget,
    raftNodeIdDtoFromCore,
    raftNodeIdDtoToCore,
    raftRequestDtoToCoreInput,
    raftRpcDtoFromCore,
    raftRpcDtoToCore,
    raftRpcKind,
  )
import Eclips.Raft.Genesis
  ( CheckedRaftGenesis,
    checkedRaftLocalNode,
    checkedRaftNativeConfiguration,
    raftNativeVoters,
  )
import Eclips.Raft.Identity
  ( RaftDispatchGeneration,
    RaftNodeId,
  )
import Eclips.Raft.Input (RaftRpc)
import Network.Socket (Socket)

data PeerLane = PeerLane
  { peerLaneEndpoint :: ResolvedTcpEndpoint,
    peerLaneRequests ::
      RetainedRequestQueue RaftDispatchGeneration Word64 RetainedRaftRequest,
    peerLaneResponses :: LeaseQueue Word64 OutgoingRaftResponse
  }

data RetainedRaftRequest
  = RetainedRaftRequest RaftRpcDto (RaftRpc ByteString -> IO ())

data OutgoingRaftResponse = OutgoingRaftResponse Word64 RaftRpcDto

data RaftWriterClaim
  = RaftWriterRequest
      ( RetainedRequestClaim
          RaftDispatchGeneration
          Word64
          RetainedRaftRequest
      )
  | RaftWriterResponse (LeaseClaim Word64 OutgoingRaftResponse)

data ResponseCorrelation
  = ResponseCorrelation
      RaftDispatchGeneration
      RaftRpcKind
      (RaftRpc ByteString -> IO ())

data RaftBinding = RaftBinding
  { bindingGeneration :: Word64,
    bindingSocket :: Socket,
    bindingLane :: PeerLane,
    bindingCorrelations :: TVar [ResponseCorrelation]
  }

data RaftTcpManager = RaftTcpManager
  { managerLocalNode :: RaftNodeId,
    managerRetryPolicy :: RaftRetryPolicy,
    managerLocalNodeDto :: RaftNodeIdDto,
    managerDigestDto :: RaftGenesisDigestDto,
    managerPeers :: TVar (Map RaftNodeId PeerLane),
    managerReplicaResolver :: RaftNodeId -> IO (Maybe OracleReplicaRegistration),
    managerProvisionalRegistrations :: TVar (Map RaftNodeId OracleReplicaRegistration),
    managerCommittedRegistrations :: TVar (Map RaftNodeId OracleReplicaRegistration),
    managerRetiredPeers :: TVar (Set.Set RaftNodeId),
    managerAccepting :: TVar Bool,
    managerStarted :: TVar Bool,
    managerNextBinding :: TVar Word64,
    managerBindings :: TVar (Map RaftNodeId RaftBinding),
    managerInitiators :: ManagedWorkers (Maybe RaftNodeId),
    managerStopDone :: MVar ()
  }

newRaftTcpManager ::
  RaftRetryPolicy ->
  CheckedRaftGenesis ->
  CheckedOracleGenesis ->
  Map RaftNodeId ResolvedTcpEndpoint ->
  (RaftNodeId -> IO (Maybe OracleReplicaRegistration)) ->
  Maybe OracleReplicaRegistration ->
  IO (Either String RaftTcpManager)
newRaftTcpManager retryPolicy raftGenesis oracleGenesis endpoints resolve bootstrap = do
  let local = checkedRaftLocalNode raftGenesis
      voters = raftNativeVoters (checkedRaftNativeConfiguration raftGenesis)
  if not (all (`Map.member` endpoints) (local : voters))
    then pure (Left "Raft replica registry omitted a checked genesis voter or the local node")
    else case checkedWireFacts local oracleGenesis of
      Left problem -> pure (Left problem)
      Right (localDto, digestDto) -> do
        peers <-
          fmap
            Map.fromList
            ( forM
                (filter ((/= local) . fst) (Map.toAscList endpoints))
                ( \(node, endpoint) -> do
                    requests <- newRetainedRequestQueueIO
                    responses <- newLeaseQueueIO
                    pure (node, PeerLane endpoint requests responses)
                )
            )
        provisional <- newTVarIO (maybe Map.empty (\registration -> Map.singleton (replicaRegistrationNode registration) registration) bootstrap)
        committed <- newTVarIO Map.empty
        retired <- newTVarIO Set.empty
        peerRegistry <- newTVarIO peers
        accepting <- newTVarIO True
        started <- newTVarIO False
        nextBinding <- newTVarIO 1
        bindings <- newTVarIO Map.empty
        initiators <- newManagedWorkersIO
        stopDone <- newEmptyMVar
        pure
          ( Right
              RaftTcpManager
                { managerLocalNode = local,
                  managerRetryPolicy = retryPolicy,
                  managerLocalNodeDto = localDto,
                  managerDigestDto = digestDto,
                  managerPeers = peerRegistry,
                  managerReplicaResolver = resolve,
                  managerProvisionalRegistrations = provisional,
                  managerCommittedRegistrations = committed,
                  managerRetiredPeers = retired,
                  managerAccepting = accepting,
                  managerStarted = started,
                  managerNextBinding = nextBinding,
                  managerBindings = bindings,
                  managerInitiators = initiators,
                  managerStopDone = stopDone
                }
          )

startRaftTcpManager :: RaftTcpManager -> OracleRuntime -> IO ()
startRaftTcpManager manager runtime = mask_ $ do
  shouldStart <- atomically $ do
    active <- readTVar manager.managerAccepting
    started <- readTVar manager.managerStarted
    if active && not started
      then writeTVar manager.managerStarted True >> pure True
      else pure False
  when shouldStart (startManaged manager Nothing watchRegistry)
  where
    watchRegistry = do
      added <- atomically $ do
        active <- readTVar manager.managerAccepting
        retired <- readTVar manager.managerRetiredPeers
        peers <- readTVar manager.managerPeers
        workers <- managedWorkerOwners manager.managerInitiators
        let started = Set.fromList [peer | Just peer <- Set.toList workers]
            fresh = Map.withoutKeys peers (Set.union retired started)
        if not active || Set.member manager.managerLocalNode retired then pure Nothing else if Map.null fresh then retry else pure (Just fresh)
      case added of
        Nothing -> pure ()
        Just fresh -> do
          forM_ (Map.toAscList fresh) $ \(peer, lane) ->
            startManaged manager (Just peer) (runInitiator manager runtime peer lane)
          watchRegistry

startManaged :: RaftTcpManager -> Maybe RaftNodeId -> IO () -> IO ()
startManaged manager peer = startManagedWorker manager.managerInitiators peer $ do
  active <- readTVar manager.managerAccepting
  retired <- readTVar manager.managerRetiredPeers
  peers <- readTVar manager.managerPeers
  pure
    ( active
        && Set.notMember manager.managerLocalNode retired
        && maybe True (\node -> Set.notMember node retired && Map.member node peers) peer
    )

-- Contact additions wake the registry supervisor once. Existing lanes and
-- retained request queues survive unrelated registration updates.
updateRaftTcpRegistrations :: RaftTcpManager -> [OracleReplicaRegistration] -> [RaftNodeId] -> IO ()
updateRaftTcpRegistrations manager registrations retired = mask_ $ do
  newlyRetired <- atomically $ do
    let committed = Map.fromList [(replicaRegistrationNode registration, registration) | registration <- registrations]
    provisional <- readTVar manager.managerProvisionalRegistrations
    let reached expected = case compareReplicaRegistration expected (Map.lookup (replicaRegistrationNode expected) committed) of
          ReplicaRegistrationMatched -> True
          ReplicaRegistrationPending -> False
          ReplicaRegistrationContradiction -> error "committed replica registration contradicts its provisional binding"
    writeTVar manager.managerCommittedRegistrations committed
    oldRetired <- readTVar manager.managerRetiredPeers
    let nextRetired = Set.union oldRetired (Set.fromList retired)
        pending
          | Set.member manager.managerLocalNode nextRetired = Map.empty
          | otherwise = Map.withoutKeys (Map.filter (not . reached) provisional) nextRetired
    when (nextRetired /= oldRetired) (writeTVar manager.managerRetiredPeers nextRetired)
    when (pending /= provisional) (writeTVar manager.managerProvisionalRegistrations pending)
    pure (Set.difference nextRetired oldRetired)
  installReplicaContacts manager registrations
  obsolete <- atomically $ do
    bindings <- readTVar manager.managerBindings
    retiredPeers <- readTVar manager.managerRetiredPeers
    let selected = if Set.member manager.managerLocalNode retiredPeers then bindings else Map.restrictKeys bindings retiredPeers
    forM_ selected $ \binding -> retireBindingLane binding.bindingGeneration binding.bindingLane
    writeTVar manager.managerBindings (Map.withoutKeys bindings (Map.keysSet selected))
    pure selected
  forM_ (Map.elems obsolete) (retireSocketQuietly . bindingSocket)
  -- An initial Hello or connect has no installed binding yet. Cancel the same
  -- manager-owned worker so its acquisition bracket closes that physical attempt
  -- too; retirement does not leave a parked native lane behind.
  when (not (Set.null newlyRetired)) $ do
    let localRetired = Set.member manager.managerLocalNode newlyRetired
    retireManagedWorkers
      manager.managerInitiators
      (\peer -> localRetired || maybe False (`Set.member` newlyRetired) peer)
    -- Membership fenced contact/worker admission before the join. Outstanding
    -- replies also require their exact installed binding, which was detached
    -- above, so no late callback can repopulate these released queue owners.
    atomically (modifyTVar' manager.managerPeers (if localRetired then const Map.empty else (`Map.withoutKeys` newlyRetired)))

installReplicaContacts :: RaftTcpManager -> [OracleReplicaRegistration] -> IO ()
installReplicaContacts manager registrations = forM_ registrations $ \registration ->
  case replicaRegistrationContact registration of
    Nothing -> pure ()
    Just contact -> do
      let node = replicaRegistrationNode registration
          endpoint = replicaContactRaft contact
      when (node /= manager.managerLocalNode) $ do
        peers <- readTVarIO manager.managerPeers
        when (Map.notMember node peers) $ do
          requests <- newRetainedRequestQueueIO
          responses <- newLeaseQueueIO
          let lane = PeerLane (ResolvedTcpEndpoint (Text.unpack (Text.decodeUtf8 (replicaEndpointHost endpoint))) (replicaEndpointPort endpoint)) requests responses
          atomically $ do
            active <- readTVar manager.managerAccepting
            retired <- readTVar manager.managerRetiredPeers
            when
              (active && Set.notMember manager.managerLocalNode retired && Set.notMember node retired)
              (modifyTVar' manager.managerPeers (Map.insertWith (\_ existing -> existing) node lane))

stopRaftTcpManager :: RaftTcpManager -> IO ()
stopRaftTcpManager manager = mask_ $ do
  owner <- atomically $ do
    active <- readTVar manager.managerAccepting
    writeTVar manager.managerAccepting False
    pure active
  if not owner
    then void (readMVar manager.managerStopDone)
    else
      ( do
          bindings <- atomically $ do
            current <- readTVar manager.managerBindings
            forM_ current $ \binding -> retireBindingLane binding.bindingGeneration binding.bindingLane
            writeTVar manager.managerBindings Map.empty
            pure current
          forM_ (Map.elems bindings) (retireSocketQuietly . bindingSocket)
          retireManagedWorkers manager.managerInitiators (const True)
          atomically (writeTVar manager.managerPeers Map.empty)
      )
        `finally` putMVar manager.managerStopDone ()

raftTcpTransport :: RaftTcpManager -> RuntimeRaftTransport
raftTcpTransport manager = RuntimeRaftTransport $ \target generation rpc callback -> do
  peers <- readTVarIO manager.managerPeers
  case (Map.lookup target peers, raftRpcDtoFromCore rpc) of
    (Just lane, Right dto)
      | requestKind (raftRpcKind dto) ->
          atomically $ do
            active <- readTVar manager.managerAccepting
            retired <- readTVar manager.managerRetiredPeers
            when
              (active && Set.notMember manager.managerLocalNode retired && Set.notMember target retired)
              ( void
                  ( offerRetainedRequest
                      generation
                      (RetainedRaftRequest dto callback)
                      lane.peerLaneRequests
                  )
              )
    _ -> pure ()

handleRaftTcpConnection :: RaftTcpManager -> OracleRuntime -> Socket -> IO ()
handleRaftTcpConnection manager runtime connection = do
  first <- receiveRawFrame connection
  case bootstrapRemote manager first of
    Nothing -> pure ()
    Just (peer, remoteDto, decoder) -> do
      known <- readTVarIO manager.managerPeers
      retired <- readTVarIO manager.managerRetiredPeers
      admitted <-
        if Set.member manager.managerLocalNode retired || Set.member peer retired
          then pure False
          else
            if Map.member peer known
              then pure True
              else do
                resolved <- manager.managerReplicaResolver peer
                case resolved of
                  Just registration | replicaRegistrationNode registration == peer -> do
                    admission <- atomically $ do
                      active <- readTVar manager.managerAccepting
                      committed <- readTVar manager.managerCommittedRegistrations
                      retiredNow <- readTVar manager.managerRetiredPeers
                      if not active || Set.member manager.managerLocalNode retiredNow || Set.member peer retiredNow
                        then pure Nothing
                        else case compareReplicaRegistration registration (Map.lookup peer committed) of
                          ReplicaRegistrationPending -> modifyTVar' manager.managerProvisionalRegistrations (Map.insert peer registration) >> pure (Just ReplicaRegistrationPending)
                          other -> pure (Just other)
                    case admission of
                      Nothing -> pure False
                      Just ReplicaRegistrationContradiction -> do
                        signalRuntimeFailure runtime.runtimeScope (RuntimeEssentialChildFailed "raft-registration" "discovered replica registration contradicts the applied binding")
                        pure False
                      Just _ -> installReplicaContacts manager [registration] >> pure True
                  _ -> pure False
      -- A higher-node probe can teach a lagging smaller node its registration;
      -- only the smaller node's ordinary connection becomes the duplex lane.
      when admitted $ do
        peers <- readTVarIO manager.managerPeers
        case Map.lookup peer peers of
          Just lane | peer < manager.managerLocalNode -> do
            sent <- sendHello manager remoteDto connection
            when sent (runEstablished manager runtime peer lane connection decoder)
          _ -> pure ()

runInitiator :: RaftTcpManager -> OracleRuntime -> RaftNodeId -> PeerLane -> IO ()
runInitiator manager runtime peer lane = loop (initialRaftRetryState manager.managerRetryPolicy)
  where
    loop retryState = do
      active <- atomically $ do
        accepting <- readTVar manager.managerAccepting
        retired <- readTVar manager.managerRetiredPeers
        bindings <- readTVar manager.managerBindings
        let active = accepting && Set.notMember manager.managerLocalNode retired && Set.notMember peer retired
        if active && manager.managerLocalNode > peer && Map.member peer bindings
          then retry
          else pure active
      when active $ do
        attempted <-
          try @IOException
            ( bracket
                (connectTcpEndpoint lane.peerLaneEndpoint)
                closeSocketQuietly
                (runInitiated manager runtime peer lane)
            )
        let outcome = either (const RaftAttemptUnestablished) id attempted
        -- A successful TCP connect followed by a refused Hello must back off
        -- too. Only an admitted native lane can reset repeated failure pacing.
        let (delay, nextRetry) = nextRaftRetry manager.managerRetryPolicy (manager.managerLocalNode > peer) outcome retryState
        threadDelay delay
        loop nextRetry

runInitiated ::
  RaftTcpManager ->
  OracleRuntime ->
  RaftNodeId ->
  PeerLane ->
  Socket ->
  IO RaftRetryOutcome
runInitiated manager runtime peer lane connection =
  case raftNodeIdDtoFromCore peer of
    Left _ -> pure RaftAttemptUnestablished
    Right remoteDto -> do
      sent <- sendHello manager remoteDto connection
      if not sent
        then pure RaftAttemptUnestablished
        else do
          first <- receiveRawFrame connection
          case initialDecoder manager remoteDto >>= \decoder -> admitOne decoder first of
            Just (RaftHello _, established) -> do
              _ <- try @IOException (runEstablished manager runtime peer lane connection established)
              pure RaftEstablishedLaneEnded
            _ -> pure RaftAttemptUnestablished

runEstablished ::
  RaftTcpManager ->
  OracleRuntime ->
  RaftNodeId ->
  PeerLane ->
  Socket ->
  RaftFrameDecoder ->
  IO ()
runEstablished manager runtime peer lane connection decoder = mask $ \restore -> do
  admitted <- installBinding manager peer lane connection
  forM_ admitted $ \binding -> do
    writerDone <- newEmptyMVar
    writer <-
      forkIOWithUnmask $ \unmask ->
        void (try @IOException (unmask (runWriter manager peer binding)))
          `finally` (retireSocketQuietly connection >> putMVar writerDone ())
    restore (runReader manager runtime peer binding decoder)
      `finally` do
        clearBinding manager peer binding.bindingGeneration
        killThread writer
        void (readMVar writerDone)

runWriter :: RaftTcpManager -> RaftNodeId -> RaftBinding -> IO ()
runWriter manager peer binding = loop
  where
    loop = do
      claimed <- atomically claimCurrentWork
      case claimed of
        Nothing -> pure ()
        Just claim -> do
          sent <- try @IOException (handle claim)
          atomically $ case sent of
            Right () -> complete claim
            Left _ -> retireBindingWork
          case sent of
            Right () -> loop
            Left _ -> pure ()

    claimCurrentWork = do
      current <- bindingIsCurrent
      if not current
        then pure Nothing
        else
          Just
            <$> (claimResponse `orElse` claimRequest)

    claimResponse = do
      claimed <-
        claimCurrentLeaseItem
          binding.bindingGeneration
          (pure True)
          binding.bindingLane.peerLaneResponses
      case claimed of
        Just claim -> pure (RaftWriterResponse claim)
        Nothing -> retry

    claimRequest =
      RaftWriterRequest
        <$> claimRetainedRequest
          binding.bindingGeneration
          binding.bindingLane.peerLaneRequests

    handle = \case
      RaftWriterResponse claim -> case leaseClaimItem claim of
        OutgoingRaftResponse generation dto
          | generation == binding.bindingGeneration ->
              send dto
          | otherwise -> pure ()
      RaftWriterRequest claim ->
        case retainedRequestClaimPayload claim of
          RetainedRaftRequest dto callback ->
            case responseKindFor (raftRpcKind dto) of
              Nothing -> pure ()
              Just expected -> do
                atomically
                  ( modifyTVar'
                      binding.bindingCorrelations
                      ( <>
                          [ ResponseCorrelation
                              (retainedRequestClaimGeneration claim)
                              expected
                              callback
                          ]
                      )
                  )
                send dto

    complete = \case
      RaftWriterResponse claim ->
        void
          ( completeLeaseClaim
              claim
              binding.bindingLane.peerLaneResponses
          )
      RaftWriterRequest claim ->
        void
          ( completeRetainedRequestSend
              claim
              binding.bindingLane.peerLaneRequests
          )

    retireBindingWork = do
      retireLeaseClaims
        outgoingRaftResponseBindingLease
        binding.bindingGeneration
        binding.bindingLane.peerLaneResponses
      void
        ( retireRetainedRequestLease
            binding.bindingGeneration
            binding.bindingLane.peerLaneRequests
        )

    bindingIsCurrent = do
      bindings <- readTVar manager.managerBindings
      pure $ case Map.lookup peer bindings of
        Just current ->
          current.bindingGeneration == binding.bindingGeneration
        Nothing -> False

    send dto =
      sendSocketBytes
        binding.bindingSocket
        (encodeRaftFrame (RaftRpc dto))

runReader ::
  RaftTcpManager ->
  OracleRuntime ->
  RaftNodeId ->
  RaftBinding ->
  RaftFrameDecoder ->
  IO ()
runReader manager runtime peer binding = loop
  where
    loop decoder = do
      raw <- receiveRawFrame binding.bindingSocket
      case admitOne decoder raw of
        Just (RaftRpc dto, successor)
          | requestKind (raftRpcKind dto) -> do
              accepted <- handleRequest dto
              when accepted (loop successor)
          | responseKind (raftRpcKind dto) -> do
              matched <- handleResponse dto
              if matched then loop successor else pure ()
        _ -> pure ()

    handleRequest dto =
      case raftRequestDtoToCoreInput (remoteNodeDto peer) dto of
        Left _ -> pure False
        Right input -> do
          let reply rpc = case raftRpcDtoFromCore rpc of
                Right responseDto
                  | responseKind (raftRpcKind responseDto) ->
                      atomically $ do
                        current <- readTVar manager.managerBindings
                        case Map.lookup peer current of
                          Just installed
                            | installed.bindingGeneration == binding.bindingGeneration ->
                                writeLeaseQueue
                                  binding.bindingLane.peerLaneResponses
                                  (OutgoingRaftResponse binding.bindingGeneration responseDto)
                          _ -> pure ()
                _ -> pure ()
          offerRaftRequestInput runtime input reply

    handleResponse dto = do
      correlation <- atomically $ do
        queued <- readTVar binding.bindingCorrelations
        case queued of
          [] -> pure Nothing
          first : rest -> do
            writeTVar binding.bindingCorrelations rest
            pure (Just first)
      case correlation of
        Just (ResponseCorrelation generation expectedKind callback)
          | expectedKind == raftRpcKind dto ->
              case raftRpcDtoToCore dto of
                Right rpc -> do
                  acknowledged <-
                    atomically
                      ( acknowledgeRetainedRequest
                          generation
                          binding.bindingLane.peerLaneRequests
                      )
                  when acknowledged (callback rpc)
                  pure True
                Left _ -> pure False
        _ -> pure False

installBinding ::
  RaftTcpManager ->
  RaftNodeId ->
  PeerLane ->
  Socket ->
  IO (Maybe RaftBinding)
installBinding manager peer lane connection = do
  correlations <- newTVarIO []
  installed <- atomically $ do
    accepting <- readTVar manager.managerAccepting
    retired <- readTVar manager.managerRetiredPeers
    if not accepting || Set.member manager.managerLocalNode retired || Set.member peer retired
      then pure Nothing
      else do
        generation <-
          stateTVar manager.managerNextBinding (\current -> (current, current + 1))
        bindings <- readTVar manager.managerBindings
        let binding = RaftBinding generation connection lane correlations
        forM_ (Map.lookup peer bindings) $ \previousBinding ->
          retireBindingLane previousBinding.bindingGeneration lane
        writeTVar manager.managerBindings (Map.insert peer binding bindings)
        pure (Just (binding, Map.lookup peer bindings))
  forM_ installed $ \(_, replaced) ->
    forM_ replaced (retireSocketQuietly . bindingSocket)
  pure (fst <$> installed)

clearBinding :: RaftTcpManager -> RaftNodeId -> Word64 -> IO ()
clearBinding manager peer generation = atomically $ do
  bindings <- readTVar manager.managerBindings
  case Map.lookup peer bindings of
    Just binding
      | binding.bindingGeneration == generation -> do
          retireBindingLane generation binding.bindingLane
          writeTVar manager.managerBindings (Map.delete peer bindings)
    _ -> pure ()

-- Each response belongs to the physical binding on which its remote request
-- arrived. The retained local request is separate: binding retirement rearms
-- it exactly once for the replacement connection.
retireBindingLane :: Word64 -> PeerLane -> STM ()
retireBindingLane generation lane = do
  retireLeaseClaims
    outgoingRaftResponseBindingLease
    generation
    lane.peerLaneResponses
  void (retireRetainedRequestLease generation lane.peerLaneRequests)

outgoingRaftResponseBindingLease :: OutgoingRaftResponse -> Maybe Word64
outgoingRaftResponseBindingLease (OutgoingRaftResponse generation _) =
  Just generation

bootstrapRemote ::
  RaftTcpManager ->
  ByteString ->
  Maybe (RaftNodeId, RaftNodeIdDto, RaftFrameDecoder)
bootstrapRemote manager raw = do
  body <- either (const Nothing) Just (rawFrameBody raftFrameMagic raw)
  envelope <- either (const Nothing) Just (decodeRaftEnvelope body)
  hello <- case envelope of
    RaftHello value -> Just value
    RaftRpc _ -> Nothing
  guard (raftHelloTarget hello == manager.managerLocalNodeDto)
  let remoteDto = raftHelloSource hello
  peer <- either (const Nothing) Just (raftNodeIdDtoToCore remoteDto)
  decoder <- initialDecoder manager remoteDto
  (_, established) <- admitOne decoder raw
  pure (peer, remoteDto, established)

initialDecoder :: RaftTcpManager -> RaftNodeIdDto -> Maybe RaftFrameDecoder
initialDecoder manager remote =
  initialRaftFrameDecoder
    <$> either
      (const Nothing)
      Just
      (raftConnectionContext manager.managerDigestDto manager.managerLocalNodeDto remote)

admitOne ::
  RaftFrameDecoder ->
  ByteString ->
  Maybe (RaftProtocolEnvelope, RaftFrameDecoder)
admitOne decoder raw =
  case feedRaftFrame decoder raw of
    RaftFrameFeedResult [envelope] (NeedRaftFrameBytes successor) ->
      Just (envelope, successor)
    _ -> Nothing

sendHello :: RaftTcpManager -> RaftNodeIdDto -> Socket -> IO Bool
sendHello manager remote connection =
  case raftHelloClaim manager.managerDigestDto manager.managerLocalNodeDto remote of
    Left _ -> pure False
    Right hello -> do
      sent <-
        try @IOException
          (sendSocketBytes connection (encodeRaftFrame (RaftHello hello)))
      pure (either (const False) (const True) sent)

checkedWireFacts ::
  RaftNodeId ->
  CheckedOracleGenesis ->
  Either String (RaftNodeIdDto, RaftGenesisDigestDto)
-- This checked genesis digest includes the SystemId and immutable initial
-- native node/host bindings. Effective voter configurations never gate a lane.
checkedWireFacts local oracleGenesis = do
  localDto <-
    either
      (Left . show)
      Right
      (raftNodeIdDtoFromCore local)
  digestDto <-
    either
      (Left . show)
      Right
      ( raftGenesisDigestDto
          (raftConfigurationDigestBytes (checkedOracleRaftConfigurationDigest oracleGenesis))
      )
  pure (localDto, digestDto)

remoteNodeDto :: RaftNodeId -> RaftNodeIdDto
remoteNodeDto node = case raftNodeIdDtoFromCore node of
  Left problem -> error ("checked Raft node failed ERFT adaptation: " <> show problem)
  Right dto -> dto

requestKind :: RaftRpcKind -> Bool
requestKind RequestVoteKind = True
requestKind AppendEntriesKind = True
requestKind InstallSnapshotKind = True
requestKind _ = False

responseKind :: RaftRpcKind -> Bool
responseKind RequestVoteResponseKind = True
responseKind AppendEntriesResponseKind = True
responseKind InstallSnapshotResponseKind = True
responseKind _ = False

responseKindFor :: RaftRpcKind -> Maybe RaftRpcKind
responseKindFor RequestVoteKind = Just RequestVoteResponseKind
responseKindFor AppendEntriesKind = Just AppendEntriesResponseKind
responseKindFor InstallSnapshotKind = Just InstallSnapshotResponseKind
responseKindFor _ = Nothing
