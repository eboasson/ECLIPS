module RaftRetryProperties (tests) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket, finally)
import Control.Monad (forM_, replicateM, void)
import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.Membership qualified as M
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Oracle.Canonical
import Eclips.Oracle.Command
import Eclips.Oracle.Failure
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Receipt
import Eclips.Oracle.Runtime
import Eclips.Oracle.Runtime.Internal.TCP.ManagedWorkers
import Eclips.Oracle.Runtime.Internal.TCP.Retry
import Eclips.Oracle.Runtime.Internal.TCP.Socket
import Eclips.Oracle.Runtime.TCP
import Eclips.Oracle.Voter
import Eclips.Protocol.Oracle.Codec qualified as C
import Eclips.Protocol.Oracle.Frame qualified as P
import Eclips.Protocol.Oracle.Types qualified as P
import Eclips.Public.Types.Timing
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import TestFixtures (awaitReadyLeader, checked, fixtureCheckedGenesis, fixtureFirstRuntimeConfiguration, fixtureInitialLeaderNode, fixtureMembers, fixtureRaftNodes, fixtureRuntimeConfigurations, validOracleCommand)
import VoterRuntimeProperties (poll, withCluster, within)

tests :: TestTree
tests =
  testGroup
    "native TCP retry pacing"
    [ testCase "configured timing survives listener/contact setup and paces real rejected Hellos" rejectedHelloPacing,
      testCase "retirement cancels and joins native workers parked before Hello acceptance" parkedHelloRetirement,
      QC.testProperty "retired worker witnesses stay bounded by live peer owners" (QC.withNumTests 30 boundedManagedWorkerRetention),
      testCase "concurrent admission and retirement preserve joinable worker witnesses" concurrentManagedWorkerAdmission,
      testCase "five-second target caps reconnects at 250ms" $ delays defaultTakeoverTarget False 10 @?= [10000, 20000, 40000, 80000, 160000, 250000, 250000, 250000, 250000, 250000],
      testCase "reverse discovery probes derive a 100ms floor at the default target" $ delays defaultTakeoverTarget True 7 @?= [100000, 100000, 100000, 100000, 160000, 250000, 250000],
      QC.testProperty "only a valid established lane resets repeated failed attempts" resetsAfterEstablished,
      QC.testProperty "repeated unavailable peers remain paced after the cap" cappedRetries
    ]

delays :: TakeoverTarget -> Bool -> Int -> [Int]
delays target reverseProbe count = take count (go (initialRaftRetryState policy))
  where
    policy = raftRetryPolicy target
    go state = let (delay, next) = nextRaftRetry policy reverseProbe RaftAttemptUnestablished state in delay : go next

resetsAfterEstablished :: Bool -> QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
resetsAfterEstablished reverseProbe suppliedTarget (QC.NonNegative supplied) =
  let failures = supplied `mod` 30
      policy = raftRetryPolicy (generatedTarget suppliedTarget)
      initialState = initialRaftRetryState policy
      state = iterate (snd . nextRaftRetry policy reverseProbe RaftAttemptUnestablished) initialState !! failures
      reset = nextRaftRetry policy reverseProbe RaftEstablishedLaneEnded state
      initial = nextRaftRetry policy reverseProbe RaftAttemptUnestablished initialState
   in reset QC.=== initial

cappedRetries :: Bool -> QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
cappedRetries reverseProbe suppliedTarget (QC.NonNegative supplied) =
  let count = 8 + supplied `mod` 40
      target = generatedTarget suppliedTarget
      timing = deriveTimingPolicy target
      cap = fromIntegral (timingRetryCapMicroseconds timing)
      floorDelay = fromIntegral (if reverseProbe then takeoverTargetMicroseconds target `div` 50 else timingReconnectInitialMicroseconds timing)
      observed = delays target reverseProbe count
   in QC.conjoin
        [ QC.property (all (\delay -> delay >= floorDelay && delay <= cap) observed),
          QC.property (and (zipWith (<=) observed (drop 1 observed))),
          drop 6 observed QC.=== replicate (count - 6) cap
        ]

generatedTarget :: QC.NonNegative Int -> TakeoverTarget
generatedTarget (QC.NonNegative supplied) =
  checked "generated takeover target" (takeoverTarget (500 + fromIntegral (supplied `mod` 100_000_000)))

-- One live peer remains blocked while arbitrarily many later peer owners finish
-- their lifetime. A retired admission stays inert without retaining its thread,
-- completion MVar, or the action's resource closure in the managed collection.
boundedManagedWorkerRetention :: QC.Positive Int -> QC.Property
boundedManagedWorkerRetention (QC.Positive supplied) = QC.ioProperty $ do
  workers <- newManagedWorkersIO
  retiredThrough <- newTVarIO (0 :: Int)
  finalized <- newTVarIO (0 :: Int)
  let count = 1 + supplied `mod` 50
      permitted owner = do
        retired <- readTVar retiredThrough
        pure (owner == 0 || owner > retired)
      startBlocked owner = do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        startManagedWorker
          workers
          owner
          (permitted owner)
          ((putMVar entered () >> takeMVar blocked) `finally` atomically (modifyTVar' finalized (+ 1)))
        within "managed worker entered its resource bracket" (readMVar entered)
      owners = atomically (managedWorkerOwners workers)
  ( do
      startBlocked 0
      forM_ [1 .. count] $ \owner -> do
        startBlocked owner
        owners >>= (@?= Set.fromList [0, owner])
        atomically (writeTVar retiredThrough owner)
        within "retired resource worker joined" (retireManagedWorkers workers (== owner))
        owners >>= (@?= Set.singleton 0)
        startManagedWorker
          workers
          owner
          (permitted owner)
          (atomically (modifyTVar' finalized (+ 1000)))
        startManagedWorker
          workers
          0
          (pure True)
          (atomically (modifyTVar' finalized (+ 1000)))
        owners >>= (@?= Set.singleton 0)
      atomically (readTVar finalized) >>= (@?= count)
    )
    `finally` retireManagedWorkers workers (const True)
  owners >>= (@?= Set.empty)
  atomically (readTVar finalized) >>= (@?= count + 1)
  pure True

-- The retiring caller races the parent's start-gate release. Scheduling may
-- choose either ordering; every visible witness must remain joinable.
concurrentManagedWorkerAdmission :: Assertion
concurrentManagedWorkerAdmission = do
  workers <- newManagedWorkersIO
  forM_ [1 .. 50 :: Int] $ \owner -> do
    accepting <- newTVarIO True
    parentDone <- newEmptyMVar
    blocked <- newEmptyMVar
    _ <-
      forkFinally
        (startManagedWorker workers owner (readTVar accepting) (takeMVar blocked))
        (putMVar parentDone)
    within "managed child registration became visible" $ atomically $ do
      current <- managedWorkerOwners workers
      check (Set.member owner current)
      writeTVar accepting False
    within "start-gate retirement joined" (retireManagedWorkers workers (== owner))
    result <- within "managed worker parent released its gate" (takeMVar parentDone)
    either (assertFailure . show) pure result
    atomically (managedWorkerOwners workers) >>= (@?= Set.empty)

-- A TCP connection succeeds and receives a real ERFT Hello, then closes without
-- admitting a lane. A twenty-second target makes four such attempts exercise
-- 40+80+160ms of pacing, proving the explicit target reaches the native manager
-- after both public listener and contact configuration transformations.
rejectedHelloPacing :: Assertion
rejectedHelloPacing = withListener $ \rejected -> withListener $ \silent -> do
  times <- newEmptyMVar
  done <- newEmptyMVar
  let endpoint listener = let address = boundTcpEndpoint listener in checked "retry listener" (oracleTcpListenEndpoint address.resolvedTcpHost address.resolvedTcpPort)
      target = checked "custom retry target" (takeoverTarget 20_000_000)
      base = configureOracleTcpTiming target (checked "single native owner" (oracleTcpClusterConfiguration [fixtureFirstRuntimeConfiguration]))
      loopback = checked "retry local listener" (oracleTcpListenEndpoint "127.0.0.1" 0)
      listeners = checked "retry listener configuration" (configureOracleTcpListeners [(fixtureInitialLeaderNode, loopback, loopback)] base)
      configured = checked "retry peers" (configureOracleTcpReplicaContacts (zip (drop 1 fixtureRaftNodes) [endpoint rejected, endpoint silent]) listeners)
      acceptOne = bracket (fst <$> Socket.accept (boundTcpSocket rejected)) closeSocketQuietly $ \connection -> do
        started <- getMonotonicTimeNSec
        _ <- receiveRawFrame connection
        pure started
      serve = replicateM 4 acceptOne >>= putMVar times
      cleanup worker = killThread worker >> takeMVar done >> pure ()
  bracket (forkFinally serve (putMVar done)) cleanup $ \_ -> do
    result <- withOracleTcpCluster configured $ \_ -> do
      stamps <- timeout 2000000 (takeMVar times) >>= maybe (assertFailure "four rejected-Hello attempts did not complete") pure
      case stamps of
        [first, _, _, fourth] -> assertBool "configured target or successful-connect/EOF retry pacing was bypassed" (fourth - first >= 250_000_000)
        _ -> assertFailure "retry server did not record four attempts"
    either (assertFailure . show) pure result
  where
    withListener = bracket bindLoopbackListener (closeSocketQuietly . boundTcpSocket)

-- Two live old voters retain quorum while both initiators are parked waiting
-- for the failed third node's Hello. Exclusion must close those attempts before
-- cluster shutdown, and the surviving dispatchers must continue past the joins.
parkedHelloRetirement :: Assertion
parkedHelloRetirement = bracket bindLoopbackListener (closeSocketQuietly . boundTcpSocket) $ \listener -> do
  parked <- newEmptyMVar
  closed <- newEmptyMVar
  done <- newEmptyMVar
  let address = boundTcpEndpoint listener
      endpoint = checked "parked Hello endpoint" (oracleTcpListenEndpoint address.resolvedTcpHost address.resolvedTcpPort)
      (survivorConfigurations, targetNode, targetHost) = case (splitAt 2 fixtureRuntimeConfigurations, drop 2 fixtureRaftNodes, drop 2 fixtureMembers) of
        ((live, [_]), [node], [member]) -> (live, node, heraldMemberEpoch member)
        _ -> error "parked Hello fixture requires exactly three native voters"
      base = checked "two surviving native owners" (oracleTcpClusterConfiguration survivorConfigurations)
      configured = checked "parked failed native peer" (configureOracleTcpReplicaContacts [(targetNode, endpoint)] base)
      acceptOne use = bracket (fst <$> Socket.accept (boundTcpSocket listener)) closeSocketQuietly $ \connection -> receiveRawFrame connection >> use connection
      serve = acceptOne $ \first -> acceptOne $ \second -> do
        putMVar parked ()
        endings <- mapM (\connection -> SocketBytes.recv connection 1) [first, second]
        putMVar closed endings
      cleanup worker = killThread worker >> void (readMVar done)
  bracket (forkFinally serve (putMVar done)) cleanup $ \_ -> withCluster configured $ \cluster -> do
    leader <- within "surviving old majority elects" (awaitReadyLeader cluster)
    within "both failed-peer initiators wait for Hello" (readMVar parked)
    statuses <- oracleTcpNodeStatuses cluster
    status <- maybe (assertFailure "survivor leader status missing") pure (lookup leader statuses)
    member <- case fixtureMembers of first : _ -> pure first; [] -> assertFailure "survivor identity missing"
    let submit = submitDirect cluster
    let captured = oracleRuntimeVoterConfiguration status
    opened <- submit member 1 (openHeraldFailureProbeCommand targetHost (M.heraldMembershipGenerationId (oracleRuntimeMembershipGeneration status)) (voterConfigurationId captured))
    probe <- case oracleReceiptFailureResult opened of Just (FailureProbeOpened value) -> pure value; other -> assertFailure ("parked-peer Open rejected: " <> show other)
    forM_ (take 2 fixtureMembers) $ \reporter -> do
      receipt <- submit reporter 2 (reportHeraldFailureProbeCommand probe (voterConfigurationId captured) ProbeUnreachable)
      oracleReceiptResult receipt @?= OracleAccepted
    let resolution = M.deriveFailureProbeResolutionId probe M.RetireFailureProbeTarget
    accepted <- submit member 3 (acceptVoterHostFailureCommand resolution)
    certificate <- case oracleReceiptFailureResult accepted of Just (VoterHostFailureAcceptedResult value) -> pure value; other -> assertFailure ("parked-peer acceptance rejected: " <> show other)
    within "survivors apply final native exclusion" $ poll $ do
      current <- oracleTcpNodeStatuses cluster
      pure $ if all (\(_, value) -> fmap voterChangePhase (oracleRuntimePendingVoterChange value) == Just VoterExcludedAwaitingHeraldRetirement) current then Just () else Nothing
    endings <- timeout 2_000_000 (readMVar closed) >>= maybe (assertFailure "retired peer kept an initial-Hello connection open") pure
    endings @?= [ByteString.empty, ByteString.empty]
    retirement <- submit member 4 (retireHeraldEpochCommand (acceptedVoterHostFailureResolution certificate) targetHost)
    oracleReceiptResult retirement @?= OracleAccepted
    ordinary <- submit member 5 validOracleCommand
    oracleReceiptResult ordinary @?= OracleAccepted

-- This fixture deliberately runs only two of the three native owners. Address
-- the observed live leader directly; full wire/run identity and old voting
-- membership stay unchanged even though the conformance cluster is incomplete.
submitDirect :: OracleTcpCluster -> HeraldMember -> Word64 -> OracleCommand -> IO OracleReceipt
submitDirect cluster member ordinal command = within "parked-Hello fixture canonical submission" $ do
  leader <- awaitReadyLeader cluster
  endpoint <- maybe (assertFailure "live leader contact missing") pure (lookup leader (oracleTcpOracleContacts cluster))
  statuses <- oracleTcpNodeStatuses cluster
  status <- maybe (assertFailure "live leader status missing") pure (lookup leader statuses)
  let genesis = fixtureCheckedGenesis
      hello =
        P.oracleHelloDto
          (C.systemIdClaimFromDomain (checkedOracleSystemId genesis))
          (C.catalogueDigestClaimFromDomain (checkedOracleCatalogueDigest genesis))
          (C.configurationDigestClaimFromDomain (checkedOracleConfigurationDigest genesis))
          (C.initialProjectionDigestClaimFromDomain (checkedOracleInitialProjectionDigest genesis))
          (C.heraldIdClaimFromDomain (heraldMemberId member))
          (C.heraldEpochClaimFromDomain (heraldMemberEpoch member))
          (C.controlIndexDtoFromDomain (controlIndex 0))
          (C.heraldMembershipGenerationClaimFromDomain (M.heraldMembershipGenerationId (oracleRuntimeMembershipGeneration status)))
      envelope = canonicalizeOracleEnvelope (oracleEnvelope (oracleClientRequestId (heraldMemberEpoch member) ordinal) Nothing (heraldMemberEpoch member) command)
      receive connection decoder = do
        bytes <- receiveRawFrame connection
        case P.feedOracleIngress decoder bytes of
          P.OracleIngressFeedResult [P.OracleServerEnvelope message] (P.NeedOracleIngressBytes next) -> pure (message, next)
          other -> assertFailure ("unexpected direct fixture frame: " <> show other)
  bracket (connectTcpEndpoint (ResolvedTcpEndpoint (oracleTcpEndpointHost endpoint) (oracleTcpEndpointPort endpoint))) closeSocketQuietly $ \connection -> do
    sendSocketBytes connection (P.encodeOracleFrame (P.OracleClientEnvelope (P.OracleHello hello)))
    (accepted, decoder) <- receive connection (P.initialOracleIngressDecoder P.oracleClientIngressContext)
    case accepted of { P.OracleHelloAccepted _ -> pure (); other -> assertFailure ("direct fixture Hello refused: " <> show other) }
    sendSocketBytes connection (P.encodeOracleFrame (P.OracleClientEnvelope (P.SubmitOracleCommand (C.canonicalOracleEnvelopeDtoFromCore envelope))))
    (response, _) <- receive connection decoder
    case response of
      P.OracleReceipt receipt -> either (assertFailure . show) pure (C.canonicalOracleReceiptDtoValue receipt)
      other -> assertFailure ("direct fixture command refused: " <> show other)
