module ConformanceClientProperties (tests) where

import Control.Concurrent
  ( forkIO,
    threadDelay,
  )
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
  )
import Control.Exception
  ( IOException,
    SomeException,
    bracket,
    onException,
    try,
  )
import Control.Monad
  ( forM,
    forM_,
    replicateM_,
  )
import Data.Bits
  ( shiftL,
    (.|.),
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word32)
import Eclips.Domain.Identity
  ( ControlIndex,
    controlIndex,
    mkProcessEpochId,
    mkProcessId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Oracle.Canonical (canonicalizeOracleEnvelope)
import Eclips.Oracle.Command
  ( oracleEnvelope,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkedOracleCatalogueDigest,
    checkedOracleConfigurationDigest,
    checkedOracleInitialProjectionDigest,
    checkedOracleSystemId,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestId,
  )
import Eclips.Oracle.Projection (OracleProjectionEventView (ProcessStartedView), appliedEntryControlIndex, appliedEntryProjectionEvents, oracleProjectionEventView, processRecordProcessEpoch, processRecordResidence)
import Eclips.Oracle.Receipt
  ( OracleReceiptResult (OracleAccepted),
    oracleReceiptControlIndex,
    oracleReceiptRequestId,
    oracleReceiptResult,
  )
import Eclips.Oracle.Runtime
  ( OracleRuntimeStatus,
    configureOracleRuntimeSubmissionCheckpoint,
    oracleRuntimeLeaderHint,
    oracleRuntimeServiceReady,
  )
import Eclips.Oracle.Runtime.ConformanceClient
  ( ConformanceRequestResult (ConformanceRequestAbsent),
    conformanceRequestId,
    newConformanceClient,
    prepareConformanceRequest,
    queryConformanceRequest,
    submitConformanceRequest,
    submitConformanceRequestVia,
    watchConformanceEntries,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    OracleTcpEndpoint,
    oracleTcpClusterConfiguration,
    oracleTcpEndpointHost,
    oracleTcpEndpointPort,
    oracleTcpNodeStatuses,
    oracleTcpOracleContacts,
    stopOracleTcpNode,
    withOracleTcpCluster,
  )
import Eclips.Protocol.Oracle.Codec
  ( canonicalOracleEnvelopeDtoFromCore,
    canonicalOracleReceiptDtoValue,
    catalogueDigestClaimFromDomain,
    committedOracleEntriesDtoValues,
    configurationDigestClaimFromDomain,
    controlIndexDtoFromDomain,
    heraldEpochClaimFromDomain,
    heraldIdClaimFromDomain,
    heraldMembershipGenerationClaimFromDomain,
    initialProjectionDigestClaimFromDomain,
    raftNodeIdClaimFromCore,
    systemIdClaimFromDomain,
  )
import Eclips.Protocol.Oracle.Frame
  ( OracleIngressContinuation (NeedOracleIngressBytes),
    OracleIngressDecoder,
    OracleIngressFeedResult (..),
    encodeOracleFrame,
    feedOracleIngress,
    initialOracleIngressDecoder,
    oracleClientIngressContext,
  )
import Eclips.Protocol.Oracle.Types
  ( OracleClientMessage (OracleHello, SubmitOracleCommand, WatchOracle),
    OracleHelloAcceptedDto,
    OracleHelloDto,
    OracleProtocolEnvelope (OracleClientEnvelope, OracleServerEnvelope),
    OracleServerMessage (..),
    oracleHelloAcceptedLeaderHint,
    oracleHelloAcceptedLocalAppliedControlIndex,
    oracleHelloAcceptedServiceReady,
    oracleHelloDto,
  )
import Eclips.Raft.Identity (RaftNodeId)
import Network.Socket
  ( AddrInfo (..),
    AddrInfoFlag (AI_NUMERICHOST, AI_NUMERICSERV),
    SocketType (Stream),
    defaultHints,
    getAddrInfo,
  )
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import TestFixtures
  ( awaitReadyLeader,
    checked,
    fixtureCheckedGenesis,
    fixtureFollowerNode,
    fixtureHeraldEpoch,
    fixtureHeraldId,
    fixtureMembers,
    fixtureRaftNodes,
    fixtureRuntimeConfigurations,
    fixtureSystemId,
    fixtureTcpConfiguration,
    identifierBytes,
    validOracleCommand,
  )

tests :: TestTree
tests =
  testGroup
    "conformance-client"
    [ testCase "fresh minimal process facts cross EORC at the same and different residents" caseDynamicProcessStarts,
      testCase
        "private sequence, terminal absence, and same-lane watch then submit are live"
        caseSequenceAndAbsence,
      testCase "a selected follower redirects the exact opaque Start request over EORC" caseSelectedFollowerRedirect,
      testCase "parked Hello observes readiness, routing, EOF, and shutdown exactly once" caseParkedHelloLifecycle,
      testCase "one EORC watch lease rejects overlap and permits sequential reuse" caseWatchWorkerLaneProtocol,
      testCase "Hello rejects a structurally valid stale membership generation" caseStaleMembershipGeneration
    ]

caseSequenceAndAbsence :: IO ()
caseSequenceAndAbsence = do
  composed <-
    withOracleTcpCluster fixtureTcpConfiguration $ \cluster -> do
      forM_ (oracleTcpOracleContacts cluster) $ \(_, endpoint) -> do
        let hints =
              defaultHints
                { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
                  addrSocketType = Stream
                }
        resolved <-
          ( getAddrInfo
              (Just hints)
              (Just (oracleTcpEndpointHost endpoint))
              (Just (show (oracleTcpEndpointPort endpoint))) ::
              IO [AddrInfo]
          )
        assertBool "public Oracle contacts are resolved numeric stream endpoints" (not (null resolved))
      leader <- awaitReadyLeader cluster
      client <-
        newConformanceClient
          fixtureCheckedGenesis
          fixtureHeraldId
          fixtureHeraldEpoch
          cluster
          >>= either (assertFailure . show) pure
      first <- prepareConformanceRequest client Nothing validOracleCommand
      second <- prepareConformanceRequest client Nothing validOracleCommand
      assertBool "request identities are distinct" (conformanceRequestId first /= conformanceRequestId second)
      result <- queryConformanceRequest client first
      case result of
        ConformanceRequestAbsent prefix ->
          assertEqual "service-ready leader authorizes absence at genesis prefix" (controlIndex 0) prefix
        other -> assertFailure ("expected terminal absence, got " <> show other)
      assertSameLaneWatchThenSubmit cluster leader
  either (assertFailure . show) pure composed

assertSameLaneWatchThenSubmit :: OracleTcpCluster -> RaftNodeId -> IO ()
assertSameLaneWatchThenSubmit cluster leader = do
  endpoint <-
    maybe
      (assertFailure "the ready leader has no Oracle TCP endpoint")
      pure
      (lookup leader (oracleTcpOracleContacts cluster))
  bracket (connectEndpoint endpoint) Socket.close $ \connection -> do
    SocketByteString.sendAll
      connection
      (encodeOracleFrame (OracleClientEnvelope (OracleHello fixtureHello)))
    (helloReply, established) <-
      receiveServerFrame
        (initialOracleIngressDecoder oracleClientIngressContext)
        connection
    case helloReply of
      OracleHelloAccepted {} -> pure ()
      other -> assertFailure ("same-lane test expected HelloAccepted, got " <> show other)
    let request = oracleClientRequestId fixtureHeraldEpoch 100
        canonical =
          canonicalizeOracleEnvelope
            ( oracleEnvelope
                request
                (Just (controlIndex 0))
                fixtureHeraldEpoch
                validOracleCommand
            )
        watch =
          encodeOracleFrame
            (OracleClientEnvelope (WatchOracle (controlIndexDtoFromDomain (controlIndex 0))))
        submit =
          encodeOracleFrame
            ( OracleClientEnvelope
                (SubmitOracleCommand (canonicalOracleEnvelopeDtoFromCore canonical))
            )
    -- One write fixes the request order independently of client scheduling: the
    -- server must keep reading after parking the first frame's long poll.
    SocketByteString.sendAll connection (watch <> submit)
    observed <-
      timeout
        5000000
        (awaitReceiptAndProjection request established connection Nothing Nothing)
    case observed of
      Nothing ->
        assertFailure
          "a parked same-connection Watch prevented the following Submit from completing"
      Just (receiptIndex, projectedIndex) -> do
        assertEqual "same-lane receipt control" (controlIndex 1) receiptIndex
        assertEqual "same-lane projected control" (controlIndex 1) projectedIndex

caseWatchWorkerLaneProtocol :: IO ()
caseWatchWorkerLaneProtocol = do
  composed <-
    withOracleTcpCluster fixtureTcpConfiguration $ \cluster -> do
      leader <- awaitReadyLeader cluster
      client <-
        newConformanceClient
          fixtureCheckedGenesis
          fixtureHeraldId
          fixtureHeraldEpoch
          cluster
          >>= either (assertFailure . show) pure
      request <- prepareConformanceRequest client Nothing validOracleCommand
      _receipt <- submitConformanceRequest client request
      endpoint <- lookupEndpoint cluster leader
      assertSequentialWatchReuse endpoint
      assertDuplicateWatchCloses endpoint
  either (assertFailure . show) pure composed

assertSequentialWatchReuse :: OracleTcpEndpoint -> IO ()
assertSequentialWatchReuse endpoint =
  bracket (connectEndpoint endpoint) Socket.close $ \connection -> do
    (_accepted, established) <- establishFixtureLane connection
    let watch =
          encodeOracleFrame
            (OracleClientEnvelope (WatchOracle (controlIndexDtoFromDomain (controlIndex 0))))
    SocketByteString.sendAll connection watch
    first <- awaitServerFrame "the first sequential Watch did not complete" established connection
    firstSuccessor <- requireCommittedWatch "first sequential Watch" first

    -- Send immediately after receiving the reply. The server must not confuse
    -- this with an overlapping Watch merely because the first worker's thread
    -- has not yet run its final instruction.
    SocketByteString.sendAll connection watch
    second <- awaitServerFrame "the reused sequential Watch did not complete" firstSuccessor connection
    _ <- requireCommittedWatch "reused sequential Watch" second
    pure ()

assertDuplicateWatchCloses :: OracleTcpEndpoint -> IO ()
assertDuplicateWatchCloses endpoint =
  bracket (connectEndpoint endpoint) Socket.close $ \connection -> do
    (accepted, _established) <- establishFixtureLane connection
    let parked =
          encodeOracleFrame
            ( OracleClientEnvelope
                (WatchOracle (oracleHelloAcceptedLocalAppliedControlIndex accepted))
            )
    -- Both frames are present before the server can publish a reply. The first
    -- parks at the exact current cursor; the second is an uncorrelated overlap
    -- and therefore retires the physical lane.
    SocketByteString.sendAll connection (parked <> parked)
    assertSocketClosed "a second outstanding Watch did not close the EORC lane" connection

establishFixtureLane :: Socket.Socket -> IO (OracleHelloAcceptedDto, OracleIngressDecoder)
establishFixtureLane connection = do
  sendFixtureHello connection
  (reply, established) <-
    receiveServerFrame
      (initialOracleIngressDecoder oracleClientIngressContext)
      connection
  case reply of
    OracleHelloAccepted accepted -> pure (accepted, established)
    other -> assertFailure ("watch-worker test expected HelloAccepted, got " <> show other)

awaitServerFrame ::
  String ->
  OracleIngressDecoder ->
  Socket.Socket ->
  IO (OracleServerMessage, OracleIngressDecoder)
awaitServerFrame failure decoder connection = do
  observed <- timeout 5000000 (receiveServerFrame decoder connection)
  maybe (assertFailure failure) pure observed

requireCommittedWatch ::
  String ->
  (OracleServerMessage, OracleIngressDecoder) ->
  IO OracleIngressDecoder
requireCommittedWatch context = \case
  (CommittedOracleEntries entries, successor) ->
    case committedOracleEntriesDtoValues entries of
      Right _ -> pure successor
      Left problem -> assertFailure (context <> " returned invalid entries: " <> show problem)
  (other, _) -> assertFailure (context <> " returned " <> show other)

awaitReceiptAndProjection ::
  OracleClientRequestId ->
  OracleIngressDecoder ->
  Socket.Socket ->
  Maybe ControlIndex ->
  Maybe ControlIndex ->
  IO (ControlIndex, ControlIndex)
awaitReceiptAndProjection request decoder connection receiptIndex projectedIndex =
  case (receiptIndex, projectedIndex) of
    (Just receipt, Just projected) -> pure (receipt, projected)
    _ -> do
      (message, successor) <- receiveServerFrame decoder connection
      case message of
        OracleReceipt dto ->
          case canonicalOracleReceiptDtoValue dto of
            Right receipt
              | oracleReceiptRequestId receipt == request ->
                  awaitReceiptAndProjection
                    request
                    successor
                    connection
                    (Just (oracleReceiptControlIndex receipt))
                    projectedIndex
            _ -> assertFailure "same-lane Submit returned the wrong Oracle receipt"
        CommittedOracleEntries dto ->
          case committedOracleEntriesDtoValues dto of
            Right entries ->
              awaitReceiptAndProjection
                request
                successor
                connection
                receiptIndex
                (Just (appliedEntryControlIndex (NonEmpty.last entries)))
            Left problem ->
              assertFailure ("same-lane Watch returned invalid entries: " <> show problem)
        other ->
          assertFailure ("same-lane Watch/Submit returned an unexpected frame: " <> show other)

receiveServerFrame ::
  OracleIngressDecoder ->
  Socket.Socket ->
  IO (OracleServerMessage, OracleIngressDecoder)
receiveServerFrame decoder connection = do
  raw <- receiveRawFrame connection
  case feedOracleIngress decoder raw of
    OracleIngressFeedResult
      [OracleServerEnvelope message]
      (NeedOracleIngressBytes successor) ->
        pure (message, successor)
    result -> assertFailure ("invalid Oracle server frame: " <> show result)

receiveRawFrame :: Socket.Socket -> IO ByteString
receiveRawFrame connection = do
  prefix <- receiveExact connection 4
  let declared = fromIntegral (decodeWord32 prefix)
  if declared < 4
    then assertFailure "Oracle server returned an invalid common frame length"
    else (prefix <>) <$> receiveExact connection declared

receiveExact :: Socket.Socket -> Int -> IO ByteString
receiveExact connection = go []
  where
    go reversed remaining
      | remaining == 0 = pure (ByteString.concat (reverse reversed))
      | otherwise = do
          chunk <- SocketByteString.recv connection remaining
          if ByteString.null chunk
            then assertFailure "Oracle server closed inside a frame"
            else go (chunk : reversed) (remaining - ByteString.length chunk)

decodeWord32 :: ByteString -> Word32
decodeWord32 =
  ByteString.foldl'
    (\accumulator byte -> (accumulator `shiftL` 8) .|. fromIntegral byte)
    0

caseSelectedFollowerRedirect :: IO ()
caseSelectedFollowerRedirect = do
  composed <-
    withOracleTcpCluster fixtureTcpConfiguration $ \cluster -> do
      _ <- awaitReadyLeader cluster
      client <-
        newConformanceClient
          fixtureCheckedGenesis
          fixtureHeraldId
          fixtureHeraldEpoch
          cluster
          >>= either (assertFailure . show) pure
      request <- prepareConformanceRequest client (Just (controlIndex 0)) validOracleCommand
      receipt <- submitConformanceRequestVia client fixtureFollowerNode request
      assertBool "the Start request through the selected follower was accepted" (maybe False (const True) receipt)
  either (assertFailure . show) pure composed

caseParkedHelloLifecycle :: IO ()
caseParkedHelloLifecycle = do
  checkpointEntered <- newEmptyTMVarIO
  releaseCheckpoint <- newEmptyTMVarIO
  let checkpoint _ _ _ = do
        atomically (putTMVar checkpointEntered ())
        atomically (readTMVar releaseCheckpoint)
      configured =
        checked
          "parked-Hello TCP cluster configuration"
          ( oracleTcpClusterConfiguration
              (fmap (configureOracleRuntimeSubmissionCheckpoint checkpoint) fixtureRuntimeConfigurations)
          )
  composed <-
    withOracleTcpCluster configured $ \cluster -> do
      leader <- awaitReadyLeader cluster
      let follower = fixtureFollowerNode
      assertBool "the routing-Hello contact is distinct from the leader" (follower /= leader)
      assertDistinctHintResponse cluster follower leader

      client <-
        newConformanceClient
          fixtureCheckedGenesis
          fixtureHeraldId
          fixtureHeraldEpoch
          cluster
          >>= either (assertFailure . show) pure
      request <- prepareConformanceRequest client (Just (controlIndex 0)) validOracleCommand
      submissionDone <- newEmptyTMVarIO
      _ <-
        forkIO $ do
          outcome <- try @SomeException (submitConformanceRequest client request)
          atomically (putTMVar submissionDone outcome)
      awaitTMVar "the controlled submission did not reach its pre-Raft checkpoint" checkpointEntered

      leaderStatus <- lookupNodeStatus cluster leader
      assertEqual "the unready leader still hints itself" (Just leader) (oracleRuntimeLeaderHint leaderStatus)
      assertBool "the reserved submission makes semantic service unready" (not (oracleRuntimeServiceReady leaderStatus))

      leaderEndpoint <- lookupEndpoint cluster leader
      bracket (connectEndpoint leaderEndpoint) Socket.close $ \connection -> do
        sendFixtureHello connection
        assertSocketSilent "a self-hinted, semantically unready leader answered Hello" connection
        atomically (putTMVar releaseCheckpoint ())
        (reply, _established) <-
          receiveServerFrame
            (initialOracleIngressDecoder oracleClientIngressContext)
            connection
        case reply of
          OracleHelloAccepted accepted -> do
            assertBool "the released leader answers with service-ready Hello" (oracleHelloAcceptedServiceReady accepted)
            assertEqual
              "the ready Hello retains the local leader hint"
              (Just (raftNodeIdClaimFromCore leader))
              (oracleHelloAcceptedLeaderHint accepted)
          other -> assertFailure ("ready parked Hello returned " <> show other)
        assertSocketSilent "readiness produced more than one Hello response" connection

      submission <- awaitTMVar "the released controlled submission did not finish" submissionDone
      case submission of
        Left exception -> assertFailure ("controlled submission failed: " <> show exception)
        Right _receipt -> pure ()

      keepEndpoint <- lookupEndpoint cluster follower
      forM_ (filter (/= follower) fixtureRaftNodes) $ \node -> do
        stopped <- stopOracleTcpNode cluster node
        assertBool "a designated peer stopped before the leaderless check" stopped
      awaitLeaderless cluster follower

      bracket (connectEndpoint keepEndpoint) Socket.close $ \connection -> do
        sendFixtureHello connection
        assertSocketSilent "a leaderless replica answered Hello" connection
        replicateM_ 8 $ do
          transient <- connectEndpoint keepEndpoint
          sendFixtureHello transient
          Socket.close transient
        stopped <- stopOracleTcpNode cluster follower
        assertBool "the parked-Hello replica stopped" stopped
        assertSocketClosed "runtime shutdown did not close the parked Hello" connection
  either (assertFailure . show) pure composed

assertDistinctHintResponse :: OracleTcpCluster -> RaftNodeId -> RaftNodeId -> IO ()
assertDistinctHintResponse cluster contacted leader = do
  endpoint <- lookupEndpoint cluster contacted
  bracket (connectEndpoint endpoint) Socket.close $ \connection -> do
    sendFixtureHello connection
    (reply, _established) <-
      receiveServerFrame
        (initialOracleIngressDecoder oracleClientIngressContext)
        connection
    case reply of
      OracleHelloAccepted accepted -> do
        assertBool "the routing Hello is not service-ready" (not (oracleHelloAcceptedServiceReady accepted))
        assertEqual
          "the routing Hello carries the distinct leader hint"
          (Just (raftNodeIdClaimFromCore leader))
          (oracleHelloAcceptedLeaderHint accepted)
      other -> assertFailure ("distinct-hint Hello returned " <> show other)
    assertSocketSilent "a distinct leader hint produced more than one Hello response" connection

sendFixtureHello :: Socket.Socket -> IO ()
sendFixtureHello connection =
  SocketByteString.sendAll
    connection
    (encodeOracleFrame (OracleClientEnvelope (OracleHello fixtureHello)))

assertSocketSilent :: String -> Socket.Socket -> IO ()
assertSocketSilent failure connection = do
  observed <- timeout 50000 (SocketByteString.recv connection 1)
  assertEqual failure Nothing observed

assertSocketClosed :: String -> Socket.Socket -> IO ()
assertSocketClosed failure connection = do
  observed <- timeout 1000000 (try @IOException (SocketByteString.recv connection 1))
  case observed of
    Nothing -> assertFailure failure
    Just (Left _) -> pure ()
    Just (Right bytes) -> assertEqual failure ByteString.empty bytes

lookupEndpoint :: OracleTcpCluster -> RaftNodeId -> IO OracleTcpEndpoint
lookupEndpoint cluster node =
  maybe
    (assertFailure "the Oracle node has no TCP endpoint")
    pure
    (lookup node (oracleTcpOracleContacts cluster))

lookupNodeStatus :: OracleTcpCluster -> RaftNodeId -> IO OracleRuntimeStatus
lookupNodeStatus cluster node = do
  statuses <- oracleTcpNodeStatuses cluster
  maybe (assertFailure "the Oracle node has no runtime status") pure (lookup node statuses)

awaitLeaderless :: OracleTcpCluster -> RaftNodeId -> IO ()
awaitLeaderless cluster node = do
  observed <- timeout 5000000 loop
  maybe (assertFailure "the surviving replica did not become leaderless") pure observed
  where
    loop = do
      status <- lookupNodeStatus cluster node
      if not (oracleRuntimeServiceReady status) && oracleRuntimeLeaderHint status == Nothing
        then pure ()
        else threadDelay 10000 >> loop

awaitTMVar :: String -> TMVar value -> IO value
awaitTMVar failure cell = do
  observed <- timeout 5000000 (atomically (readTMVar cell))
  maybe (assertFailure failure) pure observed

caseStaleMembershipGeneration :: IO ()
caseStaleMembershipGeneration = do
  composed <-
    withOracleTcpCluster fixtureTcpConfiguration $ \cluster ->
      case oracleTcpOracleContacts cluster of
        [] -> assertFailure "the cluster unexpectedly has no Oracle contact"
        (_, endpoint) : _ ->
          bracket (connectEndpoint endpoint) Socket.close $ \connection -> do
            SocketByteString.sendAll
              connection
              ( encodeOracleFrame
                  ( OracleClientEnvelope
                      (OracleHello (helloFor fixtureCheckedGenesis staleMembershipGeneration))
                  )
              )
            closed <- timeout 1000000 (SocketByteString.recv connection 1)
            assertEqual
              "the server closes a lane whose exact-width generation is stale"
              (Just ByteString.empty)
              closed
  either (assertFailure . show) pure composed

fixtureHello :: OracleHelloDto
fixtureHello = helloFor fixtureCheckedGenesis (heraldMembershipGenerationId fixtureMembershipGeneration)

helloFor :: CheckedOracleGenesis -> HeraldMembershipGenerationId -> OracleHelloDto
helloFor genesis membershipGeneration =
  oracleHelloDto
    (systemIdClaimFromDomain (checkedOracleSystemId genesis))
    (catalogueDigestClaimFromDomain (checkedOracleCatalogueDigest genesis))
    (configurationDigestClaimFromDomain (checkedOracleConfigurationDigest genesis))
    (initialProjectionDigestClaimFromDomain (checkedOracleInitialProjectionDigest genesis))
    (heraldIdClaimFromDomain fixtureHeraldId)
    (heraldEpochClaimFromDomain fixtureHeraldEpoch)
    (controlIndexDtoFromDomain (controlIndex 0))
    (heraldMembershipGenerationClaimFromDomain membershipGeneration)

fixtureMembershipGeneration :: HeraldMembershipGeneration
fixtureMembershipGeneration =
  checked
    "Herald membership generation"
    ( genesisHeraldMembershipGeneration
        fixtureSystemId
        (NonEmpty.fromList (fmap heraldMemberEpoch fixtureMembers))
    )

staleMembershipGeneration :: HeraldMembershipGenerationId
staleMembershipGeneration =
  checked
    "stale Herald membership generation"
    (mkHeraldMembershipGenerationId (ByteString.replicate 32 0xd1))

connectEndpoint :: OracleTcpEndpoint -> IO Socket.Socket
connectEndpoint endpoint = do
  let hints =
        defaultHints
          { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
            addrSocketType = Stream
          }
  resolved <-
    getAddrInfo
      (Just hints)
      (Just (oracleTcpEndpointHost endpoint))
      (Just (show (oracleTcpEndpointPort endpoint)))
  case resolved of
    [] -> fail "the Oracle endpoint did not resolve"
    address : _ -> do
      connection <- Socket.socket address.addrFamily Stream Socket.defaultProtocol
      (Socket.connect connection address.addrAddress >> pure connection)
        `onException` Socket.close connection

caseDynamicProcessStarts :: IO ()
caseDynamicProcessStarts = do
  composed <- withOracleTcpCluster fixtureTcpConfiguration $ \cluster -> do
    _ <- awaitReadyLeader cluster
    residents <- forM (take 2 fixtureMembers) $ \member -> do
      client <-
        newConformanceClient fixtureCheckedGenesis (heraldMemberId member) (heraldMemberEpoch member) cluster
          >>= either (assertFailure . show) pure
      pure (member, client)
    forM_ (zip [200 ..] (take 1 residents <> residents)) $ \(seed, (member, client)) -> do
      let epoch = checked "dynamic epoch" (mkProcessEpochId (identifierBytes (seed + 20)))
          start = processStart (checked "dynamic process" (mkProcessId (identifierBytes seed))) epoch (heraldMemberEpoch member)
      request <- prepareConformanceRequest client Nothing (startProcessEpochCommand start)
      receipt <- submitConformanceRequest client request
      assertEqual "fresh identity is admitted without a future manifest" OracleAccepted (oracleReceiptResult receipt)
      duplicate <- submitConformanceRequest client request
      assertEqual "exact retry returns the same receipt" receipt duplicate
      entries <- watchConformanceEntries client (controlIndex (fromIntegral seed - 200))
      case fmap oracleProjectionEventView (appliedEntryProjectionEvents (NonEmpty.head entries)) of
        [ProcessStartedView record] -> do
          assertEqual "projected process epoch" epoch (processRecordProcessEpoch record)
          assertEqual "projected residence" (heraldMemberEpoch member) (processRecordResidence record)
        other -> assertFailure ("unexpected dynamic Start projection " <> show other)
  either (assertFailure . show) pure composed
