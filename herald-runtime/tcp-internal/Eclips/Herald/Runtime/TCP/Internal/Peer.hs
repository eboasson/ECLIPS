{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Symmetric EPRP connections plus configured and learned physical dialing.
module Eclips.Herald.Runtime.TCP.Internal.Peer
  ( closeAcceptedPeerBinding,
    cancelPeerDial,
    dialCancelled,
    closePeerOwnerOnceWithProbe,
    settlePeerSocketClose,
    setConfiguredSeedProbeForTest,
    setPeerDialProbeForTest,
    setPeerDialSchedulerForTest,
    setPeerPublicationProbeForTest,
    setPeerEnvelopeProbeForTest,
    fallbackPeerDialDelayMicroseconds,
    handoffConfiguredSeedForTest,
    peerHelloDispositionEstablishesIdentity,
    peerAdvertisedAddress,
    parsePeerAddress,
    runConfiguredSeedManager,
    runLearnedDialRegistry,
    runPeerListener,
  )
where

import Control.Concurrent
  ( MVar,
    ThreadId,
    readMVar,
  )
import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    check,
    modifyTVar',
    newEmptyTMVarIO,
    newTVar,
    newTVarIO,
    orElse,
    readTMVar,
    readTQueue,
    readTVar,
    retry,
    tryPutTMVar,
    tryReadTMVar,
    writeTVar,
  )
import Control.Exception
  ( IOException,
    finally,
    mask,
    mask_,
    onException,
    try,
  )
import Control.Monad
  ( foldM,
    forM_,
    guard,
    unless,
    void,
    when,
  )

import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch, HeraldId)
import Eclips.Herald.Discovery
  ( HelloRejection (HelloCandidateNotSelected, HelloRetiredHerald, HelloSelfConnection),
    PeerAddress,
    PeerBinding,
    PeerCandidate,
    PeerDialClass (..),
    PeerDialIntent,
    PeerHello,
    PeerHelloDisposition (..),
    peerAddress,
    peerAddressText,
    peerBindingGeneration,
    peerBindingGenerationWord64,
    peerBindingRemoteHeraldEpoch,
    peerBindingRemoteHeraldId,
    peerDialIntentAddresses,
    peerDialIntentClass,
    peerDialIntentGenerationWord64,
    peerDialIntentHeraldEpoch,
    peerDialIntentHeraldId,
    peerHelloAdvertisedAddresses,
    peerHelloHeraldEpoch,
    peerHelloHeraldId,
  )
import Eclips.Herald.Peer.RPC
  ( CandidatePeerContext (..),
    CandidatePeerInbound (..),
    EstablishedPeerInbound (..),
    admitCandidatePeerEnvelope,
    admitEstablishedPeerEnvelope,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome (..),
    PeerLogicalAttempt,
  )
import Eclips.Herald.Runtime
  ( HeraldRuntime,
  )
import Eclips.Herald.Runtime.Connection
  ( ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (..),
    RuntimePeerConnectionHandlers,
    runtimePeerConnectionHandlersWithRetirement,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    openPeerCandidate,
    registerPeerConnection,
    submitPeerControlWithProgress,
    submitPeerHello,
    submitPeerPublicationWithProgress,
  )
import Eclips.Herald.Runtime.TCP.Internal.Discovery
  ( isDiscoveryPrefix,
    receiveFamilyPrefix,
    serveDiscoveryExchange,
  )
import Eclips.Herald.Runtime.TCP.Internal.Heartbeat
  ( HeartbeatPhase (..),
    HeartbeatState,
    discardHeartbeatAttribution,
    newHeartbeatState,
    noteHeartbeatActivity,
    recordHeartbeatPong,
    runActiveHeartbeat,
    runEstablishmentDeadline,
  )
import Eclips.Herald.Runtime.TCP.Internal.Scope
  ( claimPeerTcpSocketIf,
    claimTcpSocket,
    newTcpSocketKey,
    releaseTcpSocket,
  )
import Eclips.Herald.Runtime.TCP.Internal.Socket
  ( TcpSocketWriter,
    closeSocketOnce,
    connectTcpEndpoint,
    newTcpSocketWriter,
    prepareConnectedTcpSocket,
    receiveTcpSocketBytes,
    retireSocketQuietly,
    sendTcpSocketBytes,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( ConfiguredPeerSeed (..),
    ConfiguredPeerTarget (..),
    ConfiguredSeedProbeEvent (..),
    HeartbeatConfiguration,
    HeartbeatLane (PeerHeartbeatLane),
    HeraldTcp (..),
    PeerDialProbeEvent (..),
    PeerDialProbePhase (..),
    PeerDialRetryDelay (..),
    PeerDialScheduler (..),
    PeerEnvelopeProbePhase (..),
    PeerTransportGate (..),
    PeerTransportLease (..),
    ResolvedTcpEndpoint (..),
    TcpContext (..),
    TcpTrackedThread (..),
  )
import Eclips.Protocol.Peer.Frame
  ( PeerFrameContinuation (..),
    PeerFrameFeedResult (..),
    encodePeerFrame,
    feedPeerFrame,
    initialPeerFrameDecoder,
  )
import Eclips.Protocol.Peer.Types
  ( PeerEnvelope (..),
    PeerHeartbeatDto (..),
    PeerHeartbeatNonce,
    peerHeartbeatNonce,
    peerHeartbeatNonceWord64,
  )
import Network.Socket
  ( Socket,
    accept,
    close,
  )
import System.Timeout (timeout)

cancelPeerDial :: TcpContext -> PeerDialIntent -> STM ()
cancelPeerDial context intent = do
  satisfied <- peerDialGenerationIsSatisfied context intent
  unless satisfied
    $ modifyTVar'
      context.tcpCancelledDialGenerations
      ( retainPeerGeneration
          (peerDialIntentHeraldId intent)
          (peerDialIntentHeraldEpoch intent)
          (peerDialIntentGenerationWord64 intent)
      )

-- A newer permission supersedes every earlier permission for the same target.
-- Keep the generation floor, not each cancelled intent and its address set.
retainPeerGeneration :: HeraldId -> HeraldEpoch -> Word64 -> [(HeraldId, HeraldEpoch, Word64)] -> [(HeraldId, HeraldEpoch, Word64)]
retainPeerGeneration identifier epoch generation retained =
  let sameTarget (other, otherEpoch, _) = other == identifier && otherEpoch == epoch
      latest = maybe generation (\(_, _, previous) -> max previous generation) (find sameTarget retained)
      remaining = filter (not . sameTarget) retained
   in length remaining `seq` (identifier, epoch, latest) : remaining

data PeerDialOrigin
  = InboundPeer
  | ConfiguredSeedOrigin
  | LearnedContactOrigin PeerDialIntent
  deriving stock (Eq, Show)

data PeerSocketPhase
  = AwaitingRemoteHello
  | AwaitingLocalCandidate PeerDialOrigin
  | CandidateSent PeerCandidate PeerDialOrigin
  | DispositionPending PeerCandidate PeerHello PeerDialOrigin
  | EstablishedPeer PeerBinding PeerDialOrigin
  | ClosingPeer
  deriving stock (Eq, Show)

data PeerSocketHandle = PeerSocketHandle
  { peerSocketAccepted :: TMVar PeerBinding,
    peerSocketIdentifiedHello :: TMVar (Maybe PeerHello),
    peerSocketDone :: TMVar (),
    peerSocketClose :: IO ()
  }

-- | Socket-local workers owned by one peer transport lease.  Setup seals the
-- tracker after its final spawn decision; a reversible peer cut can then wait
-- for every admitted reader, heartbeat, and establishment worker to finish.
data PeerSocketWorkers
  = PeerSocketWorkers
      (TVar Int)
      (TVar [MVar ()])
      (TVar Bool)

data LearnedDialJob = LearnedDialJob
  { learnedTarget :: PeerDialIntent,
    learnedAddresses :: TVar (Set PeerAddress)
  }

data PeerDialPermission
  = PeerDialReady
  | PeerDialTargetBound
  | PeerDialGenerationStale
  | PeerDialScopeStopped

data PeerDialSocketOutcome
  = PeerDialSocketAccepted PeerBinding
  | PeerDialSocketDone
  | PeerDialSocketPermissionRevoked PeerDialPermission

data ConfiguredSeedAdmission
  = ConfiguredSeedDial
  | ConfiguredSeedHandedOff
  | ConfiguredSeedStop

-- | Close the exact currently retained physical lease for a previously
-- observed accepted binding. This package-private seam is used by the process
-- fixture to exercise ordinary reconnect without exposing references publicly.
closeAcceptedPeerBinding :: HeraldTcp -> PeerBinding -> IO (Maybe RuntimeSubmission)
closeAcceptedPeerBinding (HeraldTcp runtime _ _ context) binding = do
  reference <-
    atomically $ do
      current <- readTVar context.tcpCurrentPeerConnections
      pure (snd <$> find ((== binding) . fst) current)
  traverse (closeRuntimeConnection runtime) reference

-- | Install or remove the package-private configured-seed progress observer.
-- Step-14 transport tests use it as a latch; production startup leaves it
-- absent and follows the same physical path without callbacks.
setConfiguredSeedProbeForTest ::
  HeraldTcp ->
  Maybe (ConfiguredSeedProbeEvent -> IO ()) ->
  IO ()
setConfiguredSeedProbeForTest (HeraldTcp _ _ _ context) probe =
  atomically (writeTVar context.tcpConfiguredSeedProbe probe)

-- | Install or remove the package-private physical reconnect observer.
setPeerDialProbeForTest ::
  HeraldTcp ->
  Maybe (PeerDialProbeEvent -> IO ()) ->
  IO ()
setPeerDialProbeForTest (HeraldTcp _ _ _ context) probe =
  atomically (writeTVar context.tcpPeerDialProbe probe)

-- | Replace the peer-delay interpreter for deterministic transport tests.
-- Production installs an STM timer when the TCP context is created.
setPeerDialSchedulerForTest :: HeraldTcp -> PeerDialScheduler -> IO ()
setPeerDialSchedulerForTest (HeraldTcp _ _ _ context) scheduler =
  atomically (writeTVar context.tcpPeerDialScheduler scheduler)

-- | Install or remove the package-private publication handoff observer.  It
-- runs on the physical writer after the item has been dequeued and immediately
-- before the TCP phase check; production startup leaves it absent.
setPeerPublicationProbeForTest ::
  HeraldTcp ->
  Maybe (PeerBinding -> PeerLogicalAttempt -> IO ()) ->
  IO ()
setPeerPublicationProbeForTest (HeraldTcp _ _ _ context) probe =
  atomically (writeTVar context.tcpPeerPublicationProbe probe)

-- | Observe an established envelope immediately before receive admission or
-- after a successful physical write. Tests can latch these transport boundaries;
-- ordinary startup leaves the observer absent.
setPeerEnvelopeProbeForTest ::
  HeraldTcp ->
  Maybe (PeerEnvelopeProbePhase -> PeerBinding -> PeerEnvelope -> IO ()) ->
  IO ()
setPeerEnvelopeProbeForTest (HeraldTcp _ _ _ context) probe =
  atomically (writeTVar context.tcpPeerEnvelopeProbe probe)

-- | Give the non-preferred endpoint enough time for an ordinary preferred
-- handshake under contention, while preserving an explicitly slower retry
-- configuration.  The one-second value is the same bounded physical retry
-- ceiling used by Oracle connection pacing.
fallbackPeerDialDelayMicroseconds :: PeerDialRetryDelay -> Word64
fallbackPeerDialDelayMicroseconds (PeerDialRetryDelay configured) =
  max configured 1_000_000

-- | Exercise the configured-seed handoff after another socket has already
-- identified the exact target, either in the durable route map or in the
-- current accepted-contact projection. Production reaches the same branch
-- inside 'runConfiguredSeedManager'; this seam avoids a wall-clock startup
-- race in the transport property.
handoffConfiguredSeedForTest ::
  HeraldTcp ->
  ResolvedTcpEndpoint ->
  IO Bool
handoffConfiguredSeedForTest (HeraldTcp _ _ _ context) endpoint =
  atomically $ do
    admission <-
      waitForConfiguredSeedTargetUnbound
        context
        (peerAdvertisedAddress endpoint)
    case admission of
      ConfiguredSeedHandedOff -> pure True
      _ -> pure False

-- | Render the one canonical address form advertised by this prototype.
peerAdvertisedAddress :: ResolvedTcpEndpoint -> PeerAddress
peerAdvertisedAddress (ResolvedTcpEndpoint host port) =
  peerAddress
    ( "tcp://"
        <> renderHost host
        <> ":"
        <> Text.pack (show port)
    )
  where
    renderHost value
      | Text.any (== ':') value = "[" <> value <> "]"
      | otherwise = value

-- | Recognize only the canonical numeric TCP forms emitted above. Numeric
-- address validity is checked by the socket connector without invoking DNS.
parsePeerAddress :: PeerAddress -> Maybe ResolvedTcpEndpoint
parsePeerAddress address = do
  remainder <- Text.stripPrefix "tcp://" (peerAddressText address)
  (host, portText) <-
    if Text.isPrefixOf "[" remainder
      then parseBracketed remainder
      else parseUnbracketed remainder
  port <- parsePort portText
  pure (ResolvedTcpEndpoint host port)
  where
    parseBracketed value = do
      withoutOpen <- Text.stripPrefix "[" value
      let (host, suffix) = Text.breakOn "]" withoutOpen
      guard (not (Text.null host))
      portText <- Text.stripPrefix "]:" suffix
      pure (host, portText)
    parseUnbracketed value = do
      let (hostAndColon, portText) = Text.breakOnEnd ":" value
      host <- Text.stripSuffix ":" hostAndColon
      guard (not (Text.null host) && not (Text.any (== ':') host))
      pure (host, portText)
    parsePort value = case reads (Text.unpack value) of
      [(number, "")]
        | not (Text.null value),
          Text.all (\character -> character >= '0' && character <= '9') value,
          show (number :: Integer) == Text.unpack value,
          number > 0,
          number <= maximumTcpPort ->
            Just (fromIntegral number)
      _ -> Nothing
    maximumTcpPort = 65535

-- | Accept peer sockets until the listener closes.
runPeerListener ::
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  TcpContext ->
  HeraldRuntime ->
  Set PeerAddress ->
  HeartbeatConfiguration ->
  Socket ->
  IO ()
runPeerListener spawnSocket spawnFinite context runtime localAddresses heartbeatConfiguration listener = loop
  where
    loop = do
      accepting <- atomically (waitForPeerTransportAdmission context)
      if not accepting
        then pure ()
        else do
          (connection, _) <- accept listener
          case context.tcpDiscoveryHandler of
            Nothing -> installOrdinary connection ByteString.empty
            Just handler -> do
              socketKey <- newTcpSocketKey
              claimed <- claimTcpSocket context socketKey (retireSocketQuietly connection)
              if not claimed
                then close connection
                else do
                  installed <- spawnSocket $ mask $ \restore -> do
                    handedOff <- newTVarIO False
                    let classify = do
                          received <-
                            timeout
                              5_000_000
                              (prepareConnectedTcpSocket connection >> receiveFamilyPrefix connection)
                          case received of
                            Just prefix
                              | ByteString.length prefix == 8 ->
                                  if isDiscoveryPrefix prefix
                                    then void (timeout 5_000_000 (serveDiscoveryExchange (handler runtime) connection prefix))
                                    else do
                                      installOrdinary connection prefix
                                      atomically (writeTVar handedOff True)
                            _ -> pure ()
                        finish = do
                          ownedByPeer <- atomically (readTVar handedOff)
                          unless ownedByPeer (close connection)
                          releaseTcpSocket context socketKey
                    restore classify `finally` finish
                  case installed of
                    Nothing -> close connection >> releaseTcpSocket context socketKey
                    Just _ -> pure ()
          loop

    installOrdinary connection prefix = do
      installed <-
        try
          ( do
              prepareConnectedTcpSocket connection
              installPeerSocketWithPrefix
                prefix
                spawnSocket
                spawnFinite
                context
                runtime
                localAddresses
                heartbeatConfiguration
                AwaitingRemoteHello
                (pure True)
                connection
          ) ::
          IO (Either IOException (Maybe PeerSocketHandle))
      case installed of { Left _ -> close connection; Right _ -> pure () }

-- | Retry one identity-free configured bootstrap endpoint while admission is
-- open.  Once an admitted Hello identifies the exact target, retain the
-- configured route in the shared known-peer registry and retire this
-- independent manager. A checked self Hello likewise resolves the seed, but
-- Discovery never grants that target reconnect permission.
runConfiguredSeedManager ::
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  TcpContext ->
  HeraldRuntime ->
  Set PeerAddress ->
  PeerDialRetryDelay ->
  HeartbeatConfiguration ->
  ConfiguredPeerSeed ->
  IO ()
runConfiguredSeedManager spawnSocket spawnFinite context runtime localAddresses delay heartbeatConfiguration (ConfiguredPeerSeed endpoint) = loop
  where
    loop = do
      notifyConfiguredSeedProbe context ConfiguredSeedAwaitingAdmission
      admission <-
        atomically
          ( waitForConfiguredSeedTargetUnbound
              context
              (peerAdvertisedAddress endpoint)
          )
      case admission of
        ConfiguredSeedStop -> pure ()
        ConfiguredSeedHandedOff -> pure ()
        ConfiguredSeedDial -> do
          notifyConfiguredSeedProbe context ConfiguredSeedDialAttempted
          connected <- try (connectTcpEndpoint endpoint) :: IO (Either IOException Socket)
          case connected of
            Left _ -> waitAndRepeat
            Right socketHandle -> do
              installed <-
                installPeerSocket
                  spawnSocket
                  spawnFinite
                  context
                  runtime
                  localAddresses
                  heartbeatConfiguration
                  (AwaitingLocalCandidate ConfiguredSeedOrigin)
                  (pure True)
                  socketHandle
              case installed of
                -- The peer cut may close after this manager passed its
                -- admission wait and connected, but before the atomic socket
                -- claim. Retain the manager by returning to its normal loop;
                -- a still-closed gate holds it at the next admission wait and
                -- reopening resumes this same configured seed.
                Nothing -> waitAndRepeat
                Just handle -> do
                  outcome <-
                    atomically
                      ( (Left <$> readTMVar handle.peerSocketIdentifiedHello)
                          `orElse` (readTMVar handle.peerSocketDone >> pure (Right ()))
                      )
                  case outcome of
                    Left (Just hello) ->
                      atomically (retainConfiguredTarget context endpoint hello)
                    Left Nothing -> do
                      atomically (readTMVar handle.peerSocketDone)
                      waitAndRepeat
                    Right () -> waitAndRepeat
    waitAndRepeat = do
      repeatAllowed <- awaitConfiguredSeedDelay context delay
      if repeatAllowed then loop else pure ()

retainConfiguredTarget ::
  TcpContext ->
  ResolvedTcpEndpoint ->
  PeerHello ->
  STM ()
retainConfiguredTarget context endpoint hello = do
  retainConfiguredTargetAddress
    context
    (peerHelloHeraldId hello)
    (peerHelloHeraldEpoch hello)
    (peerAdvertisedAddress endpoint)

retainConfiguredTargetAddress ::
  TcpContext ->
  HeraldId ->
  HeraldEpoch ->
  PeerAddress ->
  STM ()
retainConfiguredTargetAddress context identifier epoch address = do
  let target = ConfiguredPeerTarget identifier epoch address
  modifyTVar' context.tcpConfiguredPeerTargets $ \retained ->
    if target `elem` retained then retained else target : retained

-- | Coalesce owner-authored level intentions by exact target epoch and run one
-- physical manager for each target.
runLearnedDialRegistry ::
  (IO () -> IO (Maybe ThreadId)) ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  TcpContext ->
  HeraldRuntime ->
  Set PeerAddress ->
  PeerDialRetryDelay ->
  HeartbeatConfiguration ->
  IO ()
runLearnedDialRegistry spawnJob spawnSocket spawnFinite context runtime localAddresses delay heartbeatConfiguration = do
  jobs <- newTVarIO []
  loop jobs
  where
    loop jobs = do
      next <- atomically (readDialIntentOrStop context)
      case next of
        Nothing -> pure ()
        Just intent -> do
          created <- atomically (installOrRefresh jobs intent)
          case created of
            Nothing -> pure ()
            Just job ->
              void
                ( spawnJob
                    (runLearnedJob spawnSocket spawnFinite context runtime localAddresses delay heartbeatConfiguration jobs job)
                )
          loop jobs

installOrRefresh ::
  TVar [LearnedDialJob] ->
  PeerDialIntent ->
  STM (Maybe LearnedDialJob)
installOrRefresh jobs intent = do
  current <- readTVar jobs
  let addresses = peerDialIntentAddresses intent
      sameTarget job =
        peerDialIntentHeraldId job.learnedTarget == peerDialIntentHeraldId intent
          && peerDialIntentHeraldEpoch job.learnedTarget == peerDialIntentHeraldEpoch intent
  case find sameTarget current of
    Just job ->
      case compare
        (peerDialIntentGenerationWord64 intent)
        (peerDialIntentGenerationWord64 job.learnedTarget) of
        LT -> pure Nothing
        EQ -> do
          modifyTVar' job.learnedAddresses (`Set.union` addresses)
          pure Nothing
        GT -> install (filter (not . sameTarget) current)
    Nothing -> install current
  where
    install retainedJobs = do
      retained <- newTVar (peerDialIntentAddresses intent)
      let job = LearnedDialJob intent retained
      writeTVar jobs (job : retainedJobs)
      pure (Just job)

readDialIntentOrStop :: TcpContext -> STM (Maybe PeerDialIntent)
readDialIntentOrStop context = do
  accepting <- readTVar context.tcpAccepting
  if accepting
    then Just <$> readTQueue context.tcpDialIntents
    else pure Nothing

runLearnedJob ::
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  TcpContext ->
  HeraldRuntime ->
  Set PeerAddress ->
  PeerDialRetryDelay ->
  HeartbeatConfiguration ->
  TVar [LearnedDialJob] ->
  LearnedDialJob ->
  IO ()
runLearnedJob spawnSocket spawnFinite context runtime localAddresses delay heartbeatConfiguration jobs job = do
  initial <- case peerDialIntentClass job.learnedTarget of
    ImmediatePeerDial -> atomically (peerDialPermission context jobs job)
    FallbackPeerDial -> do
      notifyPeerDialProbe context job.learnedTarget PeerDialFallbackScheduled
      awaitPeerDialDelay
        context
        jobs
        job
        (fallbackPeerDialDelayMicroseconds delay)
  continue initial
  where
    continue PeerDialReady = do
      addresses <- atomically (jobDialAddresses context job)
      afterAttempt <- connectSupported (Set.toAscList addresses)
      case afterAttempt of
        PeerDialReady -> do
          afterDelay <-
            awaitPeerDialDelay
              context
              jobs
              job
              (effectivePeerDialRetryDelay delay)
          continue afterDelay
        terminal -> finish terminal
    continue terminal = finish terminal

    finish PeerDialTargetBound =
      notifyPeerDialProbe context job.learnedTarget PeerDialSatisfied
    finish PeerDialGenerationStale =
      notifyPeerDialProbe context job.learnedTarget PeerDialSuppressed
    finish PeerDialScopeStopped =
      notifyPeerDialProbe context job.learnedTarget PeerDialCancelled
    finish PeerDialReady = pure ()

    connectSupported [] = atomically (peerDialPermission context jobs job)
    connectSupported (address : remaining) = do
      permission <- atomically (peerDialPermission context jobs job)
      case permission of
        PeerDialReady -> case parsePeerAddress address of
          Nothing -> connectSupported remaining
          Just endpoint -> do
            notifyPeerDialProbe context job.learnedTarget PeerDialAttempted
            connected <- try (connectTcpEndpoint endpoint) :: IO (Either IOException Socket)
            case connected of
              Left _ -> connectSupported remaining
              Right socketHandle -> do
                installed <-
                  installPeerSocket
                    spawnSocket
                    spawnFinite
                    context
                    runtime
                    localAddresses
                    heartbeatConfiguration
                    (AwaitingLocalCandidate (LearnedContactOrigin job.learnedTarget))
                    (peerDialClaimIsCurrent context jobs job)
                    socketHandle
                case installed of
                  Nothing -> connectSupported remaining
                  Just handle -> do
                    outcome <-
                      atomically
                        ( (PeerDialSocketAccepted <$> readTMVar handle.peerSocketAccepted)
                            `orElse` (readTMVar handle.peerSocketDone >> pure PeerDialSocketDone)
                            `orElse` revokedSocketPermission context jobs job
                        )
                    case outcome of
                      PeerDialSocketAccepted binding
                        | bindingMatchesIntent job.learnedTarget binding ->
                            pure PeerDialTargetBound
                      PeerDialSocketAccepted _ -> connectSupported remaining
                      PeerDialSocketDone -> connectSupported remaining
                      PeerDialSocketPermissionRevoked terminal -> do
                        handle.peerSocketClose
                        pure terminal
        terminal -> pure terminal

effectivePeerDialRetryDelay :: PeerDialRetryDelay -> Word64
effectivePeerDialRetryDelay (PeerDialRetryDelay configured) = configured

awaitConfiguredSeedDelay ::
  TcpContext ->
  PeerDialRetryDelay ->
  IO Bool
awaitConfiguredSeedDelay context (PeerDialRetryDelay microseconds) = do
  PeerDialScheduler schedule <- atomically (readTVar context.tcpPeerDialScheduler)
  elapsed <- schedule microseconds
  atomically $ do
    accepting <- readTVar context.tcpAccepting
    if not accepting
      then pure False
      else do
        completed <- readTVar elapsed
        check completed
        pure True

awaitPeerDialDelay ::
  TcpContext ->
  TVar [LearnedDialJob] ->
  LearnedDialJob ->
  Word64 ->
  IO PeerDialPermission
awaitPeerDialDelay context jobs job microseconds = do
  PeerDialScheduler schedule <- atomically (readTVar context.tcpPeerDialScheduler)
  elapsed <- schedule microseconds
  atomically $ do
    permission <- peerDialPermission context jobs job
    case permission of
      PeerDialReady -> do
        completed <- readTVar elapsed
        check completed
        pure PeerDialReady
      terminal -> pure terminal

peerDialPermission ::
  TcpContext ->
  TVar [LearnedDialJob] ->
  LearnedDialJob ->
  STM PeerDialPermission
peerDialPermission context jobs job = do
  accepting <- readTVar context.tcpAccepting
  if not accepting
    then pure PeerDialScopeStopped
    else do
      cancelled <- dialCancelled context job.learnedTarget
      current <- peerDialJobIsCurrent jobs job
      if cancelled
        then pure PeerDialScopeStopped
        else
          if not current
            then pure PeerDialGenerationStale
            else do
              satisfied <- peerDialGenerationIsSatisfied context job.learnedTarget
              if satisfied
                then pure PeerDialTargetBound
                else case context.tcpPeerTransportGate of
                  PeerTransportGate enabled _ -> do
                    open <- readTVar enabled
                    check open
                    bound <- intentTargetIsBound context job.learnedTarget
                    check (not bound)
                    pure PeerDialReady

peerDialClaimIsCurrent ::
  TcpContext ->
  TVar [LearnedDialJob] ->
  LearnedDialJob ->
  STM Bool
peerDialClaimIsCurrent context jobs job = do
  cancelled <- dialCancelled context job.learnedTarget
  current <- peerDialJobIsCurrent jobs job
  bound <- intentTargetIsBound context job.learnedTarget
  satisfied <- peerDialGenerationIsSatisfied context job.learnedTarget
  pure (not cancelled && current && not bound && not satisfied)

peerDialGenerationIsSatisfied :: TcpContext -> PeerDialIntent -> STM Bool
peerDialGenerationIsSatisfied context intent = do
  satisfied <- readTVar context.tcpSatisfiedPeerDialGenerations
  pure
    ( any
        ( \(identifier, epoch, generation) ->
            identifier == peerDialIntentHeraldId intent
              && epoch == peerDialIntentHeraldEpoch intent
              && generation >= peerDialIntentGenerationWord64 intent
        )
        satisfied
    )

peerDialJobIsCurrent :: TVar [LearnedDialJob] -> LearnedDialJob -> STM Bool
peerDialJobIsCurrent jobs job = do
  current <- readTVar jobs
  pure $ case find (sameDialTarget job.learnedTarget . (.learnedTarget)) current of
    Nothing -> False
    Just retained ->
      peerDialIntentGenerationWord64 retained.learnedTarget
        == peerDialIntentGenerationWord64 job.learnedTarget

jobDialAddresses :: TcpContext -> LearnedDialJob -> STM (Set PeerAddress)
jobDialAddresses context job = do
  learned <- readTVar job.learnedAddresses
  configured <- readTVar context.tcpConfiguredPeerTargets
  pure
    ( learned
        <> Set.fromList
          [ address
          | ConfiguredPeerTarget identifier epoch address <- configured,
            identifier == peerDialIntentHeraldId job.learnedTarget,
            epoch == peerDialIntentHeraldEpoch job.learnedTarget
          ]
    )

sameDialTarget :: PeerDialIntent -> PeerDialIntent -> Bool
sameDialTarget left right =
  peerDialIntentHeraldId left == peerDialIntentHeraldId right
    && peerDialIntentHeraldEpoch left == peerDialIntentHeraldEpoch right

dialCancelled :: TcpContext -> PeerDialIntent -> STM Bool
dialCancelled context intent = do
  cancelled <- readTVar context.tcpCancelledDialGenerations
  pure
    ( any
        ( \(identifier, epoch, generation) ->
            identifier == peerDialIntentHeraldId intent
              && epoch == peerDialIntentHeraldEpoch intent
              && generation >= peerDialIntentGenerationWord64 intent
        )
        cancelled
    )

notifyPeerDialProbe :: TcpContext -> PeerDialIntent -> PeerDialProbePhase -> IO ()
notifyPeerDialProbe context intent phase = do
  probe <- atomically (readTVar context.tcpPeerDialProbe)
  forM_ probe (\observe -> observe (PeerDialProbeEvent intent phase))

revokedSocketPermission ::
  TcpContext ->
  TVar [LearnedDialJob] ->
  LearnedDialJob ->
  STM PeerDialSocketOutcome
revokedSocketPermission context jobs job = do
  permission <- peerDialPermission context jobs job
  case permission of
    PeerDialReady -> retry
    terminal -> pure (PeerDialSocketPermissionRevoked terminal)

bindingMatchesIntent :: PeerDialIntent -> PeerBinding -> Bool
bindingMatchesIntent intent binding =
  peerBindingRemoteHeraldId binding == peerDialIntentHeraldId intent
    && peerBindingRemoteHeraldEpoch binding == peerDialIntentHeraldEpoch intent

intentTargetIsBound :: TcpContext -> PeerDialIntent -> STM Bool
intentTargetIsBound context intent = do
  current <- readTVar context.tcpCurrentPeerConnections
  pure (any (bindingMatchesIntent intent . fst) current)

-- | Keep retained dial managers dormant for the duration of a reversible
-- peer-only cut. Composite drain still wakes them so their structured scope can
-- stop, and the socket claim remains the final linearization point for a cut
-- racing an already-started connect.
waitForPeerTransportAdmission :: TcpContext -> STM Bool
waitForPeerTransportAdmission context = do
  accepting <- readTVar context.tcpAccepting
  if not accepting
    then pure False
    else case context.tcpPeerTransportGate of
      PeerTransportGate enabled _ -> do
        open <- readTVar enabled
        check open
        pure True

waitForConfiguredSeedTargetUnbound ::
  TcpContext ->
  PeerAddress ->
  STM ConfiguredSeedAdmission
waitForConfiguredSeedTargetUnbound context targetAddress = do
  accepting <- waitForPeerTransportAdmission context
  if not accepting
    then pure ConfiguredSeedStop
    else do
      retainedTargets <- readTVar context.tcpConfiguredPeerTargets
      let durableTargets =
            Set.fromList
              [ (identifier, epoch)
              | ConfiguredPeerTarget identifier epoch address <- retainedTargets,
                address == targetAddress
              ]
      case Set.toList durableTargets of
        [_] -> pure ConfiguredSeedHandedOff
        _ : _ : _ -> retry
        [] -> handoffCurrent
  where
    handoffCurrent = do
      current <- readTVar context.tcpCurrentPeerContacts
      let matching =
            [ hello
            | (hello, _) <- current,
              Set.member targetAddress (peerHelloAdvertisedAddresses hello)
            ]
          targets =
            Set.fromList
              [ (peerHelloHeraldId hello, peerHelloHeraldEpoch hello)
              | hello <- matching
              ]
      case Set.toList targets of
        [] -> pure ConfiguredSeedDial
        [target@(identifier, epoch)] ->
          case find
            (\hello -> (peerHelloHeraldId hello, peerHelloHeraldEpoch hello) == target)
            matching of
            Just _ -> do
              retainConfiguredTargetAddress context identifier epoch targetAddress
              pure ConfiguredSeedHandedOff
            Nothing -> retry
        _ -> retry

installPeerSocket ::
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  TcpContext ->
  HeraldRuntime ->
  Set PeerAddress ->
  HeartbeatConfiguration ->
  PeerSocketPhase ->
  STM Bool ->
  Socket ->
  IO (Maybe PeerSocketHandle)
installPeerSocket = installPeerSocketWithPrefix ByteString.empty

installPeerSocketWithPrefix ::
  ByteString.ByteString ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  TcpContext ->
  HeraldRuntime ->
  Set PeerAddress ->
  HeartbeatConfiguration ->
  PeerSocketPhase ->
  STM Bool ->
  Socket ->
  IO (Maybe PeerSocketHandle)
installPeerSocketWithPrefix initialBytes spawnSocket spawnFinite context runtime localAddresses heartbeatConfiguration initialPhase permitted socketHandle = do
  descriptorClosed <- newTVarIO False
  socketKey <- newTcpSocketKey
  closeRequested <- newTVarIO False
  phase <- newTVarIO initialPhase
  accepted <- newEmptyTMVarIO
  identifiedHello <- newEmptyTMVarIO
  referenceCell <- newEmptyTMVarIO
  ownerClosed <- newTVarIO False
  done <- newEmptyTMVarIO
  runtimeWriterDone <- newEmptyTMVarIO
  runtimeWriterInstalled <- newTVarIO False
  workers <- newPeerSocketWorkers
  heartbeat <- newHeartbeatState
  writer <- newTcpSocketWriter
  let requestClose =
        requestPeerSocketRetirement
          context
          runtime
          referenceCell
          ownerClosed
          closeRequested
          phase
          socketHandle
      ownerClose = mask_ (closeSocketOnce descriptorClosed socketHandle >> releaseTcpSocket context socketKey)
      writerFinished =
        requestClose
          `finally` atomically (void (tryPutTMVar runtimeWriterDone ()))
      handlers =
        peerHandlers
          context
          referenceCell
          phase
          accepted
          identifiedHello
          writer
          requestClose
          writerFinished
          socketHandle
  notifyConfiguredSeedClaim context initialPhase ConfiguredSeedBeforeSocketClaim
  mask $ \restore -> do
    owner <-
      spawnFinite
        ( ( do
              atomically $ readTVar closeRequested >>= check
              awaitPeerSocketWorkers workers
              atomically (readTMVar runtimeWriterDone)
          )
            -- A composite stop can admit this owner immediately before the
            -- socket claim loses its race with the stop cut. In that unclaimed
            -- case no retained lease exists, so cancellation of the tracked
            -- installer must still leave the raw descriptor with one closer.
            `finally` settlePeerSocketClose done ownerClose
        )
    case owner of
      Nothing -> do
        atomically (void (tryPutTMVar runtimeWriterDone ()))
        sealPeerSocketWorkers workers
        requestClose
        settlePeerSocketClose done ownerClose
        restore
          ( notifyConfiguredSeedClaim
              context
              initialPhase
              (ConfiguredSeedSocketClaimResult False)
          )
        pure Nothing
      Just (TcpTrackedThread _ ownerDone) -> do
        let awaitOwner = void (readMVar ownerDone)
            transportLease = PeerTransportLease requestClose awaitOwner
            abortInstall = do
              installed <- atomically (readTVar runtimeWriterInstalled)
              unless
                installed
                (atomically (void (tryPutTMVar runtimeWriterDone ())))
              requestClose
        claimed <- claimPeerTcpSocketIf context socketKey permitted transportLease
        let observeClaim =
              notifyConfiguredSeedClaim context initialPhase (ConfiguredSeedSocketClaimResult claimed)
        installed <-
          ( if not claimed
              then do
                restore observeClaim
                atomically (void (tryPutTMVar runtimeWriterDone ()))
                requestClose
                pure Nothing
              else do
                restore observeClaim
                installClaimed
                  restore
                  referenceCell
                  runtimeWriterDone
                  runtimeWriterInstalled
                  closeRequested
                  phase
                  heartbeat
                  writer
                  accepted
                  identifiedHello
                  done
                  handlers
                  requestClose
                  awaitOwner
                  workers
          )
            `onException` abortInstall
            `finally` sealPeerSocketWorkers workers
        case installed of
          Nothing -> restore awaitOwner >> pure Nothing
          Just handle -> pure (Just handle)
  where
    installClaimed restore referenceCell runtimeWriterDone runtimeWriterInstalled closeRequested phase heartbeat writer accepted identifiedHello done handlers requestClose awaitOwner workers = do
      registration <- restore (registerPeerConnection runtime handlers)
      case registration of
        RegistrationStopped -> do
          atomically (void (tryPutTMVar runtimeWriterDone ()))
          requestClose
          pure Nothing
        ConnectionRegistered connectionRef -> do
          closeWasRequested <- atomically $ do
            void (tryPutTMVar referenceCell connectionRef)
            writeTVar runtimeWriterInstalled True
            readTVar closeRequested
          if closeWasRequested
            then requestClose >> pure Nothing
            else do
              reader <-
                spawnPeerSocketWorker
                  workers
                  spawnSocket
                  ( runPeerReader
                      initialBytes
                      context
                      runtime
                      connectionRef
                      localAddresses
                      phase
                      heartbeat
                      writer
                      requestClose
                      socketHandle
                      `finally` requestClose
                  )
              case reader of
                Nothing -> requestClose >> pure Nothing
                Just _ -> do
                  heartbeatWorker <-
                    spawnPeerSocketWorker
                      workers
                      spawnFinite
                      ( runActiveHeartbeat
                          context
                          heartbeatConfiguration
                          PeerHeartbeatLane
                          (peerHeartbeatPhase phase)
                          heartbeat
                          (sendPeerPing writer socketHandle)
                          requestClose
                          `finally` requestClose
                      )
                  case heartbeatWorker of
                    Nothing -> requestClose >> pure Nothing
                    Just _ -> do
                      establishmentWorker <-
                        spawnPeerSocketWorker
                          workers
                          spawnFinite
                          ( runEstablishmentDeadline
                              context
                              heartbeatConfiguration
                              PeerHeartbeatLane
                              (peerHeartbeatPhase phase)
                              requestClose
                          )
                      case establishmentWorker of
                        Nothing -> requestClose >> pure Nothing
                        Just _ -> do
                          opened <- case initialPhase of
                            AwaitingLocalCandidate _ -> openPeerCandidate runtime connectionRef localAddresses context.tcpApplicationLocator
                            _ -> pure Queued
                          case opened of
                            Queued ->
                              pure
                                ( Just
                                    (PeerSocketHandle accepted identifiedHello done (requestClose >> awaitOwner))
                                )
                            _ -> requestClose >> pure Nothing

newPeerSocketWorkers :: IO PeerSocketWorkers
newPeerSocketWorkers =
  PeerSocketWorkers <$> newTVarIO 0 <*> newTVarIO [] <*> newTVarIO False

-- | Reserve one worker before invoking the injected spawn boundary. The owner
-- cannot snapshot exact completion witnesses until every reserved spawn has
-- either returned its tracked child or reported rejection.
reservePeerSocketWorker :: PeerSocketWorkers -> IO ()
reservePeerSocketWorker (PeerSocketWorkers pending _ _) =
  atomically (modifyTVar' pending (+ 1))

settlePeerSocketWorkerSpawn ::
  PeerSocketWorkers ->
  Maybe TcpTrackedThread ->
  IO ()
settlePeerSocketWorkerSpawn (PeerSocketWorkers pending completions _) result =
  atomically $ do
    modifyTVar' pending (subtract 1)
    forM_ result $ \(TcpTrackedThread _ done) ->
      modifyTVar' completions (done :)

spawnPeerSocketWorker ::
  PeerSocketWorkers ->
  (IO () -> IO (Maybe TcpTrackedThread)) ->
  IO () ->
  IO (Maybe TcpTrackedThread)
spawnPeerSocketWorker workers spawn action = mask_ $ do
  reservePeerSocketWorker workers
  result <- spawn action `onException` settlePeerSocketWorkerSpawn workers Nothing
  settlePeerSocketWorkerSpawn workers result
  pure result

sealPeerSocketWorkers :: PeerSocketWorkers -> IO ()
sealPeerSocketWorkers (PeerSocketWorkers _ _ sealed) =
  atomically (writeTVar sealed True)

awaitPeerSocketWorkers :: PeerSocketWorkers -> IO ()
awaitPeerSocketWorkers (PeerSocketWorkers pending completions sealed) = do
  retained <- atomically $ do
    isSealed <- readTVar sealed
    remaining <- readTVar pending
    check (isSealed && remaining == 0)
    readTVar completions
  forM_ retained (void . readMVar)

notifyConfiguredSeedClaim ::
  TcpContext ->
  PeerSocketPhase ->
  ConfiguredSeedProbeEvent ->
  IO ()
notifyConfiguredSeedClaim context phase event = case phase of
  AwaitingLocalCandidate ConfiguredSeedOrigin ->
    notifyConfiguredSeedProbe context event
  _ -> pure ()

notifyConfiguredSeedProbe :: TcpContext -> ConfiguredSeedProbeEvent -> IO ()
notifyConfiguredSeedProbe context event = do
  probe <- atomically (readTVar context.tcpConfiguredSeedProbe)
  forM_ probe ($ event)

peerHandlers ::
  TcpContext ->
  TMVar (ConnectionRef PeerPlane) ->
  TVar PeerSocketPhase ->
  TMVar PeerBinding ->
  TMVar (Maybe PeerHello) ->
  TcpSocketWriter ->
  IO () ->
  IO () ->
  Socket ->
  RuntimePeerConnectionHandlers
peerHandlers context referenceCell phase accepted identifiedHello writer requestClose writerFinished socketHandle =
  runtimePeerConnectionHandlersWithRetirement
    (\candidate envelope -> offerCandidate context referenceCell phase identifiedHello writer socketHandle candidate envelope)
    (\candidate disposition -> offerDisposition context referenceCell phase accepted identifiedHello candidate disposition)
    ( \binding envelope -> do
        notifyPeerEnvelopeProbe context PeerEnvelopeWriting binding envelope
        offered <- offerEstablished phase writer socketHandle binding envelope
        when (offered == LaneOffered) (notifyPeerEnvelopeProbe context PeerEnvelopeWritten binding envelope)
        pure offered
    )
    (\binding _ -> offerClosing phase binding)
    ( \binding attempt envelope -> do
        notifyPeerPublicationProbe context binding attempt
        outcome <- offerPublication phase writer requestClose socketHandle binding attempt envelope
        when (outcome == PeerDispatchWritten) (notifyPeerEnvelopeProbe context PeerEnvelopeWritten binding envelope)
        pure outcome
    )
    (retirePeerSocket context referenceCell phase)
    writerFinished

notifyPeerPublicationProbe ::
  TcpContext ->
  PeerBinding ->
  PeerLogicalAttempt ->
  IO ()
notifyPeerPublicationProbe context binding attempt = do
  probe <- atomically (readTVar context.tcpPeerPublicationProbe)
  forM_ probe (\observe -> observe binding attempt)

notifyPeerEnvelopeProbe :: TcpContext -> PeerEnvelopeProbePhase -> PeerBinding -> PeerEnvelope -> IO ()
notifyPeerEnvelopeProbe context phase binding envelope = do
  probe <- atomically (readTVar context.tcpPeerEnvelopeProbe)
  forM_ probe (\observe -> observe phase binding envelope)

offerCandidate ::
  TcpContext ->
  TMVar (ConnectionRef PeerPlane) ->
  TVar PeerSocketPhase ->
  TMVar (Maybe PeerHello) ->
  TcpSocketWriter ->
  Socket ->
  PeerCandidate ->
  PeerEnvelope ->
  IO RuntimeLaneOffer
offerCandidate context referenceCell phase identifiedHello writer socketHandle candidate envelope = do
  current <-
    atomically $ do
      observed <- readTVar phase
      case observed of
        AwaitingLocalCandidate origin -> do
          case origin of
            ConfiguredSeedOrigin -> do
              reference <- readTMVar referenceCell
              modifyTVar' context.tcpConfiguredPeerCandidates ((reference, candidate, identifiedHello) :)
            _ -> pure ()
          writeTVar phase (CandidateSent candidate origin)
          pure True
        CandidateSent retained _ -> pure (retained == candidate)
        DispositionPending retained _ _ -> pure (retained == candidate)
        _ -> pure False
  if current then sendPeerEnvelope writer socketHandle envelope else pure LaneLost

offerDisposition ::
  TcpContext ->
  TMVar (ConnectionRef PeerPlane) ->
  TVar PeerSocketPhase ->
  TMVar PeerBinding ->
  TMVar (Maybe PeerHello) ->
  PeerCandidate ->
  PeerHelloDisposition ->
  IO RuntimeLaneOffer
offerDisposition context referenceCell phase accepted identifiedHello candidate disposition =
  atomically $ do
    observed <- readTVar phase
    case observed of
      DispositionPending retained hello origin
        | retained == candidate -> case disposition of
            PeerHelloAccepted _ binding -> do
              reference <- readTMVar referenceCell
              forgetConfiguredPeerCandidate context reference
              writeTVar phase (EstablishedPeer binding origin)
              void (tryPutTMVar accepted binding)
              void (tryPutTMVar identifiedHello (Just hello))
              retainConfiguredTargetsForHello context hello
              retainSatisfiedPeerDialGeneration context binding
              modifyTVar'
                context.tcpCurrentPeerConnections
                (((binding, reference) :) . filter ((/= reference) . snd))
              modifyTVar'
                context.tcpCurrentPeerContacts
                (((hello, reference) :) . filter ((/= reference) . snd))
              pure LaneOffered
            PeerHelloRejected _ -> do
              -- An inbound self connection is rejected before sending a
              -- response Hello. Resolve its locally initiated seed through
              -- the exact candidate, including aliases absent from Hello's
              -- advertised addresses. Socket retirement removes this row.
              case disposition of
                PeerHelloRejected HelloSelfConnection -> do
                  configured <- readTVar context.tcpConfiguredPeerCandidates
                  forM_ configured $ \(_, pending, identified) ->
                    when (pending == candidate) (void (tryPutTMVar identified (Just hello)))
                _ -> pure ()
              if peerHelloDispositionEstablishesIdentity disposition
                then retainConfiguredTargetsForHello context hello
                else pure ()
              void
                ( tryPutTMVar
                    identifiedHello
                    ( if peerHelloDispositionEstablishesIdentity disposition
                        then Just hello
                        else Nothing
                    )
                )
              writeTVar phase ClosingPeer
              pure LaneOffered
      _ -> pure LaneLost

peerHelloDispositionEstablishesIdentity :: PeerHelloDisposition -> Bool
peerHelloDispositionEstablishesIdentity = \case
  PeerHelloAccepted {} -> True
  PeerHelloRejected HelloRetiredHerald -> True
  PeerHelloRejected HelloSelfConnection -> True
  PeerHelloRejected (HelloCandidateNotSelected _) -> True
  PeerHelloRejected _ -> False

forgetConfiguredPeerCandidate :: TcpContext -> ConnectionRef PeerPlane -> STM ()
forgetConfiguredPeerCandidate context reference =
  modifyTVar' context.tcpConfiguredPeerCandidates $ \entries ->
    let retained = filter (\(owner, _, _) -> owner /= reference) entries
     in length retained `seq` retained

retainConfiguredTargetsForHello :: TcpContext -> PeerHello -> STM ()
retainConfiguredTargetsForHello context hello =
  forM_
    ( Set.toAscList
        ( Set.intersection
            context.tcpConfiguredPeerAddresses
            (peerHelloAdvertisedAddresses hello)
        )
    )
    ( retainConfiguredTargetAddress
        context
        (peerHelloHeraldId hello)
        (peerHelloHeraldEpoch hello)
    )

retainSatisfiedPeerDialGeneration :: TcpContext -> PeerBinding -> STM ()
retainSatisfiedPeerDialGeneration context binding = do
  let identifier = peerBindingRemoteHeraldId binding
      epoch = peerBindingRemoteHeraldEpoch binding
      generation = peerBindingGenerationWord64 (peerBindingGeneration binding)
  modifyTVar' context.tcpSatisfiedPeerDialGenerations (retainPeerGeneration identifier epoch generation)
  -- The shared satisfied floor now also excludes these delayed cancellations.
  -- Exact job checks continue to consult it before claiming any socket.
  modifyTVar' context.tcpCancelledDialGenerations $ \retained ->
    let remaining = filter (\(other, otherEpoch, cancelled) -> other /= identifier || otherEpoch /= epoch || cancelled > generation) retained
     in length remaining `seq` remaining

offerEstablished ::
  TVar PeerSocketPhase ->
  TcpSocketWriter ->
  Socket ->
  PeerBinding ->
  PeerEnvelope ->
  IO RuntimeLaneOffer
offerEstablished phase writer socketHandle binding envelope = do
  current <- atomically (isEstablished phase binding)
  if current
    then sendPeerEnvelope writer socketHandle envelope
    else pure LaneLost

offerClosing :: TVar PeerSocketPhase -> PeerBinding -> IO RuntimeLaneOffer
offerClosing phase binding =
  atomically $ do
    current <- isEstablished phase binding
    if current
      then writeTVar phase ClosingPeer >> pure LaneOffered
      else pure LaneLost

offerPublication ::
  TVar PeerSocketPhase ->
  TcpSocketWriter ->
  IO () ->
  Socket ->
  PeerBinding ->
  PeerLogicalAttempt ->
  PeerEnvelope ->
  IO PeerDispatchOutcome
offerPublication phase writer closeAction socketHandle binding _ envelope = do
  current <- atomically (isEstablished phase binding)
  if not current
    then do
      -- Returning Deferred would leave the runtime slot established, so the
      -- pure owner would immediately select the same binding again while this
      -- physical lease is already closing.  Terminating the writer lets its
      -- close path atomically claim Deferred for this attempt and queue the
      -- binding loss which retains the item for a replacement lease.
      closeAction
      ioError (userError "peer publication lane is no longer established")
    else do
      result <- try (sendTcpSocketBytes writer socketHandle (encodePeerFrame envelope)) :: IO (Either IOException ())
      case result of
        Right () -> pure PeerDispatchWritten
        Left failure -> closeAction >> ioError failure

sendPeerEnvelope :: TcpSocketWriter -> Socket -> PeerEnvelope -> IO RuntimeLaneOffer
sendPeerEnvelope writer socketHandle envelope = do
  result <- try (sendTcpSocketBytes writer socketHandle (encodePeerFrame envelope)) :: IO (Either IOException ())
  pure $ case result of
    Right () -> LaneOffered
    Left _ -> LaneLost

sendPeerPing :: TcpSocketWriter -> Socket -> Word64 -> IO Bool
sendPeerPing writer socketHandle nonce = do
  offered <-
    sendPeerEnvelope
      writer
      socketHandle
      (PeerHeartbeatEnvelope (Ping (peerHeartbeatNonce nonce)))
  pure (offered == LaneOffered)

sendPeerPong ::
  TcpSocketWriter ->
  IO () ->
  Socket ->
  PeerHeartbeatNonce ->
  IO Bool
sendPeerPong writer closeAction socketHandle nonce = do
  offered <-
    sendPeerEnvelope
      writer
      socketHandle
      (PeerHeartbeatEnvelope (Pong nonce))
  case offered of
    LaneOffered -> pure True
    LaneLost -> closeAction >> pure False

peerHeartbeatPhase :: TVar PeerSocketPhase -> STM HeartbeatPhase
peerHeartbeatPhase phase = do
  observed <- readTVar phase
  pure $ case observed of
    EstablishedPeer {} -> HeartbeatEstablished
    ClosingPeer -> HeartbeatClosed
    _ -> HeartbeatNotEstablished

isEstablished :: TVar PeerSocketPhase -> PeerBinding -> STM Bool
isEstablished phase binding = do
  observed <- readTVar phase
  pure $ case observed of
    EstablishedPeer retained _ -> retained == binding
    _ -> False

-- | Invalidate both peer projections and retire the stream without releasing
-- its descriptor. This is safe from reader, heartbeat, and runtime-writer
-- threads: the dedicated socket owner joins every completion witness before it
-- performs the eventual physical close.
requestPeerSocketRetirement ::
  TcpContext ->
  HeraldRuntime ->
  TMVar (ConnectionRef PeerPlane) ->
  TVar Bool ->
  TVar Bool ->
  TVar PeerSocketPhase ->
  Socket ->
  IO ()
requestPeerSocketRetirement context runtime referenceCell ownerClosed closeRequested phase socketHandle =
  mask_ $ do
    -- The requested fact is distinct from completed descriptor close: a
    -- registration racing this path must not install a reader while the owner
    -- is still joining an already admitted worker.
    atomically $ do
      writeTVar closeRequested True
      retirePeerSocket context referenceCell phase
    retireSocketQuietly socketHandle
    closePeerOwnerOnceWithProbe (pure ()) runtime referenceCell ownerClosed

-- | Publish completion of the dedicated physical owner even when descriptor
-- close raises. The tracked owner wrapper still reports that exception through
-- the ordinary TCP-scope failure path before its exact completion witness.
settlePeerSocketClose :: TMVar () -> IO () -> IO ()
settlePeerSocketClose done action =
  action `finally` atomically (void (tryPutTMVar done ()))

-- | Claim and invalidate one peer owner lease without an asynchronous-exception
-- gap between those operations. 'closeRuntimeConnection' is one finite STM
-- transaction, so keeping it masked cannot strand a thread on blocking I/O.
-- The probe is a package-private deterministic interruption seam.
closePeerOwnerOnceWithProbe ::
  IO () ->
  HeraldRuntime ->
  TMVar (ConnectionRef PeerPlane) ->
  TVar Bool ->
  IO ()
closePeerOwnerOnceWithProbe beforeOwnerClose runtime referenceCell ownerClosed =
  mask_ $ do
    maybeReference <- atomically $ do
      reference <- tryReadTMVar referenceCell
      alreadyClosed <- readTVar ownerClosed
      case reference of
        Just current | not alreadyClosed -> do
          writeTVar ownerClosed True
          pure (Just current)
        _ -> pure Nothing
    forM_ maybeReference $ \reference -> do
      beforeOwnerClose
      void (closeRuntimeConnection runtime reference)

-- | Retire the TCP projection in the same STM transaction that invalidates
-- the owner's registry lease.  A disposition already dequeued by the writer
-- either commits first and has its row removed here, or observes ClosingPeer
-- and cannot resurrect that row afterward.
retirePeerSocket ::
  TcpContext ->
  TMVar (ConnectionRef PeerPlane) ->
  TVar PeerSocketPhase ->
  STM ()
retirePeerSocket context referenceCell phase = do
  writeTVar phase ClosingPeer
  maybeReference <- tryReadTMVar referenceCell
  forM_ maybeReference $ \reference -> do
    forgetConfiguredPeerCandidate context reference
    modifyTVar'
      context.tcpCurrentPeerConnections
      (filter ((/= reference) . snd))
    modifyTVar'
      context.tcpCurrentPeerContacts
      (filter ((/= reference) . snd))

runPeerReader ::
  ByteString.ByteString ->
  TcpContext ->
  HeraldRuntime ->
  ConnectionRef PeerPlane ->
  Set PeerAddress ->
  TVar PeerSocketPhase ->
  HeartbeatState ->
  TcpSocketWriter ->
  IO () ->
  Socket ->
  IO ()
runPeerReader initialBytes context runtime connectionRef localAddresses phase heartbeat writer closeAction socketHandle =
  if ByteString.null initialBytes then loop initialPeerFrameDecoder else feed initialPeerFrameDecoder initialBytes
  where
    loop decoder = do
      chunk <- receiveTcpSocketBytes socketHandle 32768
      if ByteString.null chunk
        then pure ()
        else feed decoder chunk
    feed decoder chunk = do
      atomically (notePeerChunkActivity phase heartbeat)
      case feedPeerFrame decoder chunk of
        PeerFrameFeedResult envelopes continuation -> do
          continue <- foldM admitEnvelope True envelopes
          if continue
            then case continuation of
              NeedPeerFrameBytes successor -> loop successor
              PeerFrameFailed _ -> pure ()
            else pure ()
    admitEnvelope False _ = pure False
    admitEnvelope True envelope = do
      observed <- atomically (awaitReadablePhase phase)
      case observed of
        AwaitingRemoteHello -> admitCandidateHeartbeatClosed RemoteInitiatedCandidate envelope
        CandidateSent candidate _ -> admitCandidateHeartbeatClosed (LocallyInitiatedCandidate candidate) envelope
        EstablishedPeer binding _ -> do
          notifyPeerEnvelopeProbe context PeerEnvelopeReceiving binding envelope
          admitEstablished binding envelope
        ClosingPeer -> pure False
        AwaitingLocalCandidate _ -> pure False
        DispositionPending _ _ _ -> pure False
    admitCandidateHeartbeatClosed candidateContext envelope = case envelope of
      PeerHeartbeatEnvelope _ -> pure False
      _ -> admitCandidate candidateContext envelope
    admitCandidate candidateContext envelope =
      case admitCandidatePeerEnvelope candidateContext envelope of
        Left _ -> pure False
        Right (CandidatePeerHello candidate hello generation members) -> do
          claimed <-
            atomically $ do
              current <- readTVar phase
              let retainedOrigin = case (candidateContext, current) of
                    (RemoteInitiatedCandidate, AwaitingRemoteHello) -> Just InboundPeer
                    (LocallyInitiatedCandidate expected, CandidateSent retained origin)
                      | expected == retained && retained == candidate -> Just origin
                    _ -> Nothing
              case retainedOrigin of
                Nothing -> pure False
                Just origin -> do
                  writeTVar phase (DispositionPending candidate hello origin)
                  pure True
          if not claimed
            then pure False
            else do
              submitted <- submitPeerHello runtime connectionRef candidate localAddresses hello generation members context.tcpApplicationLocator
              pure (submitted == Queued)
    admitEstablished binding envelope = case envelope of
      PeerHeartbeatEnvelope heartbeatDto -> consumeHeartbeat heartbeatDto
      _ -> do
        atomically (discardHeartbeatAttribution heartbeat)
        case admitEstablishedPeerEnvelope envelope of
          Left _ -> pure False
          Right (EstablishedPeerControl progress control) -> do
            submitted <- submitPeerControlWithProgress runtime connectionRef binding progress control
            pure (submitted == Queued)
          Right (EstablishedPeerPublication progress item) -> do
            submitted <- submitPeerPublicationWithProgress runtime connectionRef binding progress item
            pure (submitted == Queued)
    consumeHeartbeat heartbeatDto = case heartbeatDto of
      Ping nonce -> do
        atomically (discardHeartbeatAttribution heartbeat)
        sendPeerPong writer closeAction socketHandle nonce
      Pong nonce -> do
        _ <-
          recordHeartbeatPong
            context
            PeerHeartbeatLane
            heartbeat
            (peerHeartbeatNonceWord64 nonce)
        pure True

awaitReadablePhase :: TVar PeerSocketPhase -> STM PeerSocketPhase
awaitReadablePhase phase = do
  observed <- readTVar phase
  case observed of
    AwaitingLocalCandidate _ -> retry
    DispositionPending _ _ _ -> retry
    _ -> pure observed

notePeerChunkActivity ::
  TVar PeerSocketPhase ->
  HeartbeatState ->
  STM ()
notePeerChunkActivity phase heartbeat = do
  observed <- readTVar phase
  case observed of
    EstablishedPeer {} -> noteHeartbeatActivity heartbeat
    _ -> pure ()
