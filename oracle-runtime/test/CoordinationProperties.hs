module CoordinationProperties (tests) where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    retry,
  )
import Control.Exception
  ( Exception,
    bracket,
    evaluate,
    finally,
    onException,
    throwIO,
  )
import Control.Monad (forM_, void)
import Data.Bits
  ( shiftL,
    (.|.),
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    mkWeakIORef,
    newIORef,
    readIORef,
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Word (Word32, Word64)
import Eclips.Domain.Alignment
  ( HeraldPublicationPrefix (EmptyHeraldPublicationPrefix),
  )
import Eclips.Domain.Identity
  ( controlIndex,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkHeraldId,
  )
import Eclips.Domain.Label
  ( firstLabelProcessAcceptancePosition,
    homeLabelAcceptanceCut,
    initialBootstrapLabelEvidence,
    targetVoid,
  )
import Eclips.Domain.Membership
  ( FailureProbeResolution (RetireFailureProbeTarget),
    FailureProbeResolutionId,
    HeraldMembershipGeneration,
    admitHeraldMembershipGeneration,
    deriveFailureProbeResolutionId,
    deriveHeraldAdmissionId,
    deriveHeraldFailureProbeId,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    heraldMembershipHistory,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Startup
  ( HeraldMember (..),
  )
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Oracle.Admission (admissionManifestHeraldEpoch, admissionManifestHeraldId, heraldAdmissionManifest)
import Eclips.Oracle.Canonical
  ( CanonicalOracleEnvelope,
    canonicalOracleEnvelopeBytes,
    canonicalizeOracleEnvelope,
  )
import Eclips.Oracle.Command
  ( completeLabelDecisionCommand,
    decideLabelCommand,
    oracleEnvelope,
    retireOracleProgressCommand,
  )
import Eclips.Oracle.Genesis
  ( checkedOracleCatalogueDigest,
    checkedOracleConfigurationDigest,
    checkedOracleInitialProjectionDigest,
    checkedOracleRaftConfigurationDigest,
    checkedOracleRaftNativeConfiguration,
    checkedOracleSystemId,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestId,
    raftConfigurationDigestBytes,
  )
import Eclips.Oracle.Label
  ( deriveLabelDecisionId,
    labelCompletionAttestation,
    liveDecisionId,
  )
import Eclips.Oracle.Progress (oracleProgress)
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (LabelDecidedView),
    appliedEntryControlIndex,
    appliedEntryOracleProgress,
    appliedEntryProjectionEvents,
    oracleProjectionEventView,
  )
import Eclips.Oracle.Receipt (oracleReceiptControlIndex)
import Eclips.Oracle.Runtime
  ( OracleRuntimeEvent (RuntimeProposalObserved, RuntimeRaftInputInstalled),
    OracleRuntimeFailure (RuntimeSubmissionPreflightInvariantFault),
    OracleRuntimeRecordingMode (OracleCaptureHistory),
    configureOracleRuntimeRecording,
    configureOracleRuntimeSubmissionCheckpoint,
    oracleRuntimeAppliedControlIndex,
    oracleRuntimeConfiguration,
    oracleRuntimeRole,
    oracleRuntimeTerm,
    runtimeElectionTimeoutSource,
  )
import Eclips.Oracle.Runtime.ConformanceClient
  ( ConformanceRequestResult (ConformanceRequestAbsent, ConformanceRequestFound),
    ConformanceSubmissionDisposition (..),
    conformanceRequestId,
    newConformanceClient,
    prepareConformanceRequest,
    queryConformanceRequest,
    retireConformanceProgress,
    submitConformanceRequest,
    submitConformanceRequestDispositionVia,
    watchConformanceEntries,
  )
import Eclips.Oracle.Runtime.Internal.Hello
  ( OracleHelloAvailability (..),
    activeHeraldIdentities,
    classifyOracleHelloAvailability,
    heraldLaneGenerationAuthorized,
    heraldLaneIdentityAuthorized,
  )
import Eclips.Oracle.Runtime.Internal.StatusProjection
  ( RaftStatusProjection (..),
    membershipStatusProjectionChanged,
    raftStatusProjection,
    raftStatusProjectionChanged,
  )
import Eclips.Oracle.Runtime.Internal.Submission
  ( SubmissionRegistrationDecision (FailSubmissionRegistration, RetryStaleSubmissionReservation),
    decideSubmissionRegistration,
    submissionRegistrationProposalCount,
    submissionRegistrationRecursivePreflightCount,
  )
import Eclips.Oracle.Runtime.Internal.WatchAvailability
  ( OracleWatchAvailability (..),
    awaitWatchAuthorityGrace,
    classifyOracleWatchAvailability,
    runtimeDelay,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    OracleTcpEndpoint,
    oracleTcpClusterConfiguration,
    oracleTcpEndpointHost,
    oracleTcpEndpointPort,
    oracleTcpNodeRecordings,
    oracleTcpNodeStatuses,
    oracleTcpOracleContacts,
    oracleTcpRaftEndpoints,
    stopOracleTcpNode,
    withOracleTcpCluster,
  )
import Eclips.Protocol.Oracle.Codec
  ( canonicalOracleEnvelopeDtoFromCore,
    catalogueDigestClaimFromDomain,
    configurationDigestClaimFromDomain,
    controlIndexDtoFromDomain,
    heraldEpochClaimFromDomain,
    heraldIdClaimFromDomain,
    heraldMembershipGenerationClaimFromDomain,
    initialProjectionDigestClaimFromDomain,
    oracleClientRequestIdDtoFromCore,
    oracleProgressDtoToCore,
    raftTermDtoToCore,
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
  ( OracleClientMessage (OracleHello, SubmitOracleCommand),
    OracleHelloDto,
    OracleProtocolEnvelope (OracleClientEnvelope, OracleServerEnvelope),
    OracleServerMessage (..),
    oracleHelloDto,
  )
import Eclips.Protocol.Raft.Frame qualified as RaftFrame
import Eclips.Protocol.Raft.Types qualified as RaftProtocol
import Eclips.Raft.Configuration (RaftConfigurationRef, RaftVotingConfiguration, raftConfigurationEntryRef)
import Eclips.Raft.Effect (RaftProposalStatus (RaftProposalAppended))
import Eclips.Raft.Genesis
  ( checkRaftGenesis,
    raftGenesis,
    raftNativeElectionTimeoutLower,
    raftNativeElectionTimeoutUpper,
    raftNativeHeartbeatInterval,
  )
import Eclips.Raft.Identity (RaftNodeId, RaftTerm, raftLogIndex, raftTerm, raftTermWord64)
import Eclips.Raft.Input qualified as RaftInput
import Eclips.Raft.State
  ( RaftRole (RaftLeader),
    raftStateEffectiveConfiguration,
    raftStateLeaderHint,
    raftStateRole,
    raftStateServiceReady,
    raftStateTerm,
  )
import Eclips.Raft.Transition (initialRaft)
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import Network.Socket
  ( AddrInfo (..),
    AddrInfoFlag (AI_NUMERICHOST, AI_NUMERICSERV),
    SocketType (Stream),
    defaultHints,
    getAddrInfo,
  )
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import System.Mem.Weak (Weak, deRefWeak)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( NonNegative (NonNegative),
    Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    shuffle,
    testProperty,
    (.&&.),
    (===),
  )
import TestFixtures
  ( alternateOracleCommand,
    awaitNodesAppliedPrefix,
    awaitReadyLeader,
    checked,
    fixtureCheckedGenesis,
    fixtureFirstRaftGenesis,
    fixtureFollowerNode,
    fixtureHeraldEpoch,
    fixtureHeraldId,
    fixtureMembers,
    fixtureOracleGenesis,
    fixtureProcessEpoch,
    fixtureRaftNodes,
    fixtureRecordedTcpConfiguration,
    fixtureReplacementNode,
    fixtureRuntimeConfigurations,
    fixtureSystemId,
    fixtureTcpConfiguration,
    identifierBytes,
    validOracleCommand,
  )

tests :: TestTree
tests =
  testGroup
    "coordination"
    [ testCase "Start result publication precedes admission of the next semantic proposal" caseSerializedProposals,
      testCase "a live incomplete label installation defers the exact next decision" caseLiveDecisionDeferral,
      testCase "a reserved leader prefix remains stable across an exact concurrent retry" caseReservedLeaderPrefixStable,
      testCase "a reservation retries after leadership loss, intervening commit and regain" caseReservationAcrossLeadership,
      testCase "a throwing submission checkpoint releases the pre-Raft reservation" caseThrowingCheckpointReleasesReservation,
      testCase "Hello waits for exact service or redirect evidence" caseHelloAvailability,
      testCase "stable Raft routing status is not republished" caseStableRaftStatusProjection,
      testCase "published Raft status releases its deferred predecessor without reading generation" caseRaftStatusReleasesPredecessor,
      testCase "stable Oracle membership status is not republished" caseStableMembershipStatusProjection,
      testCase "a predecessor-generation survivor can catch up and keep using its lane, but unrelated generations and retired epochs cannot" caseMembershipCatchUpAuthorization,
      testCase "EORC identity authorization includes activated nonvoters and excludes pending identities" caseAdmissionHelloAuthorization,
      testProperty "current health identities agree with checked lineage after repeated fresh-epoch admission and retirement" propActiveHealthIdentities,
      testCase "watch authority distinguishes local, unresolved, and distinct evidence" caseWatchAvailability,
      testCase "watch authority grace uses the exact typed delay and left-biased evidence" caseWatchAuthorityGrace,
      testProperty "a same-term changed preflight token faults without proposal or recursive preflight" propChangedPreflightTokenFaults,
      testProperty "a stale reservation term retries before changed-prefix validation" propStaleReservationRetries
    ]

caseHelloAvailability :: Assertion
caseHelloAvailability = do
  let classify accepting ready hint =
        classifyOracleHelloAvailability accepting ready (1 :: Int) hint
  assertEqual "a stopped Oracle closes an awaiting Hello" OracleHelloStopped (classify False True Nothing)
  assertEqual "a leaderless node parks the existing connection" OracleHelloWaiting (classify True False Nothing)
  assertEqual "an unready self hint cannot create a reconnect" OracleHelloWaiting (classify True False (Just 1))
  assertEqual "a distinct leader hint makes one redirect actionable" OracleHelloActionable (classify True False (Just 2))
  assertEqual "a service-ready local node makes one binding actionable" OracleHelloActionable (classify True True Nothing)

caseStableRaftStatusProjection :: Assertion
caseStableRaftStatusProjection = do
  let state =
        initialRaft
          (checked "Raft status projection genesis" (checkRaftGenesis fixtureFirstRaftGenesis))
      changed role term leader ready =
        raftStatusProjectionChanged state role term leader ready
  assertBool
    "the exact Raft projection suppresses a same-value status write"
    ( not
        ( changed
            (raftStateRole state)
            (raftStateTerm state)
            (raftStateLeaderHint state)
            (raftStateServiceReady state)
        )
    )
  assertBool
    "a changed readiness fact remains publishable"
    ( changed
        (raftStateRole state)
        (raftStateTerm state)
        (raftStateLeaderHint state)
        (not (raftStateServiceReady state))
    )
  let configuration = raftStateEffectiveConfiguration state
      changedReference = checked "different status configuration reference" (raftConfigurationEntryRef (raftLogIndex 1) (raftTerm 1))
      generation (RaftStatusProjection _ _ _ _ _ _ _ _ value) = value
  assertEqual
    "unchanged configuration preserves the generation"
    41
    (generation (raftStatusProjection state 41 configuration))
  assertEqual
    "changed configuration advances the generation once"
    42
    (generation (raftStatusProjection state 41 (changedReference, snd configuration)))

type StatusPredecessor = (Word64, (RaftConfigurationRef, RaftVotingConfiguration))

-- This immutable test cell witnesses a deferred owner query's reachability.
-- No production unsafe IO is involved. Do not force the published generation
-- before GC: that would hide the lazy chain this regression must detect.
{-# NOINLINE deferredStatusPredecessor #-}
deferredStatusPredecessor :: IORef StatusPredecessor -> StatusPredecessor
deferredStatusPredecessor reference = unsafePerformIO (readIORef reference)

{-# NOINLINE statusProjectionWithPredecessor #-}
statusProjectionWithPredecessor :: IO (RaftStatusProjection, Weak (IORef StatusPredecessor))
statusProjectionWithPredecessor = do
  let state = initialRaft (checked "status lifetime genesis" (checkRaftGenesis fixtureFirstRaftGenesis))
  predecessor <- newIORef (41, raftStateEffectiveConfiguration state)
  witness <- mkWeakIORef predecessor (pure ())
  let (generation, configuration) = deferredStatusPredecessor predecessor
  projection <- evaluate (raftStatusProjection state generation configuration)
  pure (projection, witness)

caseRaftStatusReleasesPredecessor :: Assertion
caseRaftStatusReleasesPredecessor = do
  (projection, witness) <- statusProjectionWithPredecessor
  bracket (newStablePtr projection) freeStablePtr $ \root -> do
    performGC
    retained <- deRefWeak witness
    case retained of
      Nothing -> pure ()
      Just _ -> assertFailure "published native status still retains its deferred predecessor query"
    RaftStatusProjection _ _ _ _ _ _ _ _ generation <- deRefStablePtr root
    assertEqual "rooted projection remains usable after predecessor collection" 41 generation

caseStableMembershipStatusProjection :: Assertion
caseStableMembershipStatusProjection = do
  let predecessor = fixtureMembershipGeneration
      members = fmap heraldMemberEpoch fixtureMembers
  retired <- case members of
    _ : _ : suppliedRetired : _ -> pure suppliedRetired
    observed -> assertFailure ("expected at least three Heralds, got " <> show observed)
  successor <-
    either
      (assertFailure . (("derive membership status successor: " <>) . show))
      pure
      (retireHeraldMembershipGeneration (controlIndex 2) (fixtureRetirementResolution 1) retired predecessor)
  assertBool
    "the exact membership projection suppresses a same-value status write"
    (not (membershipStatusProjectionChanged predecessor predecessor))
  assertBool
    "a successor membership generation remains publishable"
    (membershipStatusProjectionChanged successor predecessor)

caseAdmissionHelloAuthorization :: Assertion
caseAdmissionHelloAuthorization = do
  let applicantId = checked "applicant Herald ID" (mkHeraldId (ByteString.replicate 32 0xef))
      applicant = checked "applicant epoch" (mkHeraldEpoch (ByteString.replicate 32 0xf0))
      admission = checked "admission identity" (deriveHeraldAdmissionId (controlIndex 1))
      predecessor = fixtureMembershipGeneration
      successor = checked "admitted nonvoter membership" (admitHeraldMembershipGeneration (controlIndex 2) admission applicant predecessor)
      prior = checked "prior membership history" (heraldMembershipHistory (NonEmpty.singleton predecessor))
      activated = checked "activated membership history" (heraldMembershipHistory (predecessor NonEmpty.:| [successor]))
      catalogue = [heraldAdmissionManifest fixtureSystemId (heraldMemberId member) (heraldMemberEpoch member) | member <- fixtureMembers]
      withPending = catalogue <> [heraldAdmissionManifest fixtureSystemId applicantId applicant]
      authorize ident history = heraldLaneIdentityAuthorized ident applicant (heraldMembershipGenerationId predecessor) withPending history
  assertBool "known pending identity has no ordinary lane" (not (authorize applicantId prior))
  assertBool "active newcomer can watch from its retained genesis coordinate" (authorize applicantId activated)
  assertBool "another known Herald ID cannot claim the newcomer epoch" (not (authorize fixtureHeraldId activated))
  assertBool "immutable bootstrap catalogue alone is insufficient" (not (heraldLaneIdentityAuthorized applicantId applicant (heraldMembershipGenerationId successor) catalogue activated))

caseMembershipCatchUpAuthorization :: Assertion
caseMembershipCatchUpAuthorization = do
  let predecessor = fixtureMembershipGeneration
      predecessorId = heraldMembershipGenerationId predecessor
      members = fmap heraldMemberEpoch fixtureMembers
  (survivor, secondRetired, retired) <- case members of
    suppliedSurvivor : suppliedSecond : suppliedRetired : _ ->
      pure (suppliedSurvivor, suppliedSecond, suppliedRetired)
    observed -> assertFailure ("expected at least three Heralds, got " <> show observed)
  let successor =
        checked
          "first membership retirement"
          (retireHeraldMembershipGeneration (controlIndex 2) (fixtureRetirementResolution 1) retired predecessor)
      final =
        checked
          "second membership retirement"
          (retireHeraldMembershipGeneration (controlIndex 4) (fixtureRetirementResolution 3) secondRetired successor)
      successorId = heraldMembershipGenerationId successor
      finalId = heraldMembershipGenerationId final
      originalHistory = checked "genesis membership history" (heraldMembershipHistory (NonEmpty.singleton predecessor))
      firstHistory = checked "first membership history" (heraldMembershipHistory (predecessor NonEmpty.:| [successor]))
      history = checked "repeated membership history" (heraldMembershipHistory (predecessor NonEmpty.:| [successor, final]))
      unrelatedId =
        heraldMembershipGenerationId
          ( checked
              "unrelated Herald membership generation"
              ( genesisHeraldMembershipGeneration
                  fixtureSystemId
                  (NonEmpty.singleton survivor)
              )
          )
  forM_ [predecessorId, successorId, finalId] $ \claimed -> do
    assertBool
      "an active survivor may catch up from every retained generation"
      (heraldLaneGenerationAuthorized survivor claimed history)
    forM_ [retired, secondRetired] $ \removed ->
      assertBool
        "neither retired epoch regains authority by claiming any retained generation"
        (not (heraldLaneGenerationAuthorized removed claimed history))
  assertBool
    "the second target remains active after only the first retirement"
    (heraldLaneGenerationAuthorized secondRetired predecessorId firstHistory)
  assertBool
    "an unrelated generation cannot use the survivor catch-up allowance"
    (not (heraldLaneGenerationAuthorized survivor unrelatedId history))
  assertBool
    "a future generation is not authorized against its predecessor"
    (not (heraldLaneGenerationAuthorized survivor successorId originalHistory))

propActiveHealthIdentities :: Property
propActiveHealthIdentities = forAll (shuffle fixtureMembers) $ \members -> forAll (chooseInt (1, 6)) $ \rounds ->
  case members of
    first : second : _ ->
      let genesis = fixtureMembershipGeneration
          retiredFirst = checked "health identity first retirement" (retireHeraldMembershipGeneration (controlIndex 2) (fixtureRetirementResolution 1) (heraldMemberEpoch first) genesis)
          retiredSecond = checked "health identity second retirement" (retireHeraldMembershipGeneration (controlIndex 4) (fixtureRetirementResolution 3) (heraldMemberEpoch second) retiredFirst)
          freshEpoch ordinal = checked "health identity fresh epoch" (mkHeraldEpoch (ByteString.replicate 32 (fromIntegral (100 + ordinal))))
          grow previous ordinal =
            let index = 5 + 4 * fromIntegral ordinal
                admission = checked "health identity fresh admission" (deriveHeraldAdmissionId (controlIndex index))
                admitted = checked "health identity admission successor" (admitHeraldMembershipGeneration (controlIndex (index + 1)) admission (freshEpoch ordinal) previous)
                retired = checked "health identity retirement successor" (retireHeraldMembershipGeneration (controlIndex (index + 3)) (fixtureRetirementResolution (index + 2)) (freshEpoch ordinal) admitted)
             in admitted : retired : if ordinal == rounds then [] else grow retired (ordinal + 1)
          generations = genesis : retiredFirst : retiredSecond : grow retiredSecond 1
          catalogue =
            [heraldAdmissionManifest fixtureSystemId (heraldMemberId member) (heraldMemberEpoch member) | member <- fixtureMembers]
              <> [heraldAdmissionManifest fixtureSystemId (heraldMemberId first) (freshEpoch ordinal) | ordinal <- [1 .. rounds + 1]]
          prefixes = scanl (\prior generation -> prior <> [generation]) [genesis] (drop 1 generations)
       in forAll (shuffle catalogue) $ \ordered ->
            conjoin
              [ let history = checked "health identity checked history" (heraldMembershipHistory (NonEmpty.fromList prefix))
                    actual = activeHeraldIdentities ordered history
                    current = finalGeneration prefix
                 in counterexample
                      ("health identity prefix " <> show (heraldMembershipGenerationId current))
                      ( conjoin
                          ( [ (Map.lookup epoch actual == Just ident) === heraldLaneIdentityAuthorized ident epoch (heraldMembershipGenerationId current) ordered history
                            | candidate <- ordered,
                              let epoch = admissionManifestHeraldEpoch candidate,
                              ident <- fmap admissionManifestHeraldId ordered
                            ]
                              <> [Map.keysSet actual === Map.keysSet (Map.fromList [(epoch, ()) | epoch <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs current)])]
                          )
                      )
              | prefix <- prefixes
              ]
    _ -> error "health identity property requires three genesis members"
  where
    finalGeneration [generation] = generation
    finalGeneration (_ : remaining) = finalGeneration remaining
    finalGeneration [] = error "empty health identity history"

fixtureRetirementResolution :: Word64 -> FailureProbeResolutionId
fixtureRetirementResolution probeIndex =
  deriveFailureProbeResolutionId
    (checked "retirement probe" (deriveHeraldFailureProbeId (controlIndex probeIndex)))
    RetireFailureProbeTarget

caseWatchAvailability :: Assertion
caseWatchAvailability = do
  let classify accepting localLeader hint =
        classifyOracleWatchAvailability accepting localLeader (1 :: Int) hint
  assertEqual "a stopped Oracle terminates the watch" OracleWatchAuthorityStopped (classify False True Nothing)
  assertEqual "a local leader retains its watch across term movement" OracleWatchAuthorityLocal (classify True True Nothing)
  assertEqual "a local leader ignores a stale distinct hint" OracleWatchAuthorityLocal (classify True True (Just 2))
  assertEqual "leaderless authority is unresolved" OracleWatchAuthorityUnresolved (classify True False Nothing)
  assertEqual "a self hint leaves authority unresolved" OracleWatchAuthorityUnresolved (classify True False (Just 1))
  assertEqual "a distinct known leader makes one redirect actionable" OracleWatchAuthorityDistinct (classify True False (Just 2))

data WatchGraceOutcome
  = WatchGraceObserved
  | WatchGraceExpired
  deriving stock (Eq, Show)

caseWatchAuthorityGrace :: Assertion
caseWatchAuthorityGrace = do
  let expectedDuration =
        raftNativeElectionTimeoutUpper
          (checkedOracleRaftNativeConfiguration fixtureCheckedGenesis)
  observedDuration <- newEmptyMVar
  progress <- newEmptyTMVarIO
  elapsed <- newEmptyTMVarIO
  completed <- newEmptyMVar
  let delay =
        runtimeDelay $ \duration -> do
          putMVar observedDuration duration
          pure (readTMVar elapsed)
  _ <-
    forkFinally
      ( awaitWatchAuthorityGrace
          delay
          expectedDuration
          (readTMVar progress >> pure WatchGraceObserved)
          (pure WatchGraceExpired)
      )
      (putMVar completed)
  assertEqual "the grace registrar receives the checked election upper" expectedDuration
    =<< takeWithin "the watch grace was not armed" observedDuration
  pending <- tryReadMVar completed
  assertBool "the authority wait remains parked before either gate opens" (case pending of Nothing -> True; Just _ -> False)
  atomically $ do
    putTMVar elapsed ()
    putTMVar progress ()
  simultaneous <- takeWithin "the simultaneous grace schedule did not complete" completed >>= either throwIO pure
  assertEqual "watch evidence wins a simultaneous expiry" WatchGraceObserved simultaneous

  expiryObservedDuration <- newEmptyMVar
  expiryElapsed <- newEmptyTMVarIO
  expiryCompleted <- newEmptyMVar
  let expiryDelay =
        runtimeDelay $ \duration -> do
          putMVar expiryObservedDuration duration
          pure (readTMVar expiryElapsed)
  _ <-
    forkFinally
      ( awaitWatchAuthorityGrace
          expiryDelay
          expectedDuration
          retry
          (pure WatchGraceExpired)
      )
      (putMVar expiryCompleted)
  assertEqual "a later grace episode is armed independently" expectedDuration
    =<< takeWithin "the later watch grace was not armed" expiryObservedDuration
  atomically (putTMVar expiryElapsed ())
  expired <- takeWithin "the expired grace schedule did not complete" expiryCompleted >>= either throwIO pure
  assertEqual "unresolved authority expires when its gate opens" WatchGraceExpired expired

data InjectedSubmissionCheckpointFailure
  = InjectedSubmissionCheckpointFailure
  deriving stock (Show)

instance Exception InjectedSubmissionCheckpointFailure

caseSerializedProposals :: IO ()
caseSerializedProposals = do
  composed <-
    withOracleTcpCluster fixtureTcpConfiguration $ \cluster -> do
      _ <- awaitReadyLeader cluster
      client <-
        newConformanceClient fixtureCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster
          >>= either (assertFailure . show) pure
      request <- prepareConformanceRequest client (Just (controlIndex 0)) validOracleCommand
      first <- submitConformanceRequest client request
      duplicate <- submitConformanceRequest client request
      assertEqual "published duplicate is the retained receipt" first duplicate
      assertEqual "receipt was published at control one" (controlIndex 1) (oracleReceiptControlIndex first)
      statuses <- fmap (fmap snd) (oracleTcpNodeStatuses cluster)
      assertBool
        "no duplicate control entry became visible"
        (all ((<= controlIndex 1) . oracleRuntimeAppliedControlIndex) statuses)
  either (assertFailure . show) pure composed

caseLiveDecisionDeferral :: IO ()
caseLiveDecisionDeferral = do
  composed <-
    withOracleTcpCluster fixtureRecordedTcpConfiguration $ \cluster -> do
      leader <- awaitReadyLeader cluster
      homeClient <-
        newConformanceClient fixtureCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster
          >>= either (assertFailure . show) pure
      let firstRequestId = oracleClientRequestId fixtureHeraldEpoch 1
          secondRequestId = oracleClientRequestId fixtureHeraldEpoch 2
          decision = deriveLabelDecisionId fixtureSystemId firstRequestId
          command request objectByte =
            let object = checked "live decision object" (mkGlobalObjectId (identifierBytes objectByte))
             in decideLabelCommand
                  (deriveLabelDecisionId fixtureSystemId request)
                  fixtureProcessEpoch
                  object
                  (ProcessLabel fixtureProcessEpoch, 0)
                  (Just (initialBootstrapLabelEvidence object fixtureProcessEpoch))
                  Nothing
                  Nothing
                  targetVoid
                  (homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition fixtureProcessEpoch) EmptyHeraldPublicationPrefix)
      firstRequest <- prepareConformanceRequest homeClient Nothing (command firstRequestId 240)
      assertEqual "the first request identity is exact" firstRequestId (conformanceRequestId firstRequest)
      firstReceipt <- submitConformanceRequest homeClient firstRequest
      assertEqual "the first atomic decision orders at control one" (controlIndex 1) (oracleReceiptControlIndex firstReceipt)
      decidedEntries <- watchConformanceEntries homeClient (controlIndex 0)
      outcomeDigest <-
        case [ digest
             | entry <- NonEmpty.toList decidedEntries,
               event <- appliedEntryProjectionEvents entry,
               LabelDecidedView observed _ digest <- [oracleProjectionEventView event],
               liveDecisionId observed == decision
             ] of
          [digest] -> pure digest
          evidence -> assertFailure ("expected one atomic decision event, got " <> show evidence)
      _ <- awaitNodesAppliedPrefix cluster fixtureRaftNodes 1
      proposalCountBeforeDeferral <- appendedProposalCount <$> oracleTcpNodeRecordings cluster
      secondRequest <- prepareConformanceRequest homeClient Nothing (command secondRequestId 240)
      assertEqual "the second request identity is exact" secondRequestId (conformanceRequestId secondRequest)
      deferred <- submitConformanceRequestDispositionVia homeClient leader secondRequest
      assertEqual
        "the live EORC response identifies the blocking decision and observed prefix"
        (Just (ConformanceSubmissionDeferred decision (controlIndex 1)))
        deferred
      absent <- queryConformanceRequest homeClient secondRequest
      assertEqual "deferral creates no retained request receipt" (ConformanceRequestAbsent (controlIndex 1)) absent
      proposalCountAfterDeferral <- appendedProposalCount <$> oracleTcpNodeRecordings cluster
      assertEqual "preflight deferral registers no Raft proposal" proposalCountBeforeDeferral proposalCountAfterDeferral
      statusesAfterDeferral <- fmap (fmap snd) (oracleTcpNodeStatuses cluster)
      assertBool "deferral creates no Oracle entry" (all ((== controlIndex 1) . oracleRuntimeAppliedControlIndex) statusesAfterDeferral)
      completionRequest <-
        prepareConformanceRequest
          homeClient
          Nothing
          (completeLabelDecisionCommand (labelCompletionAttestation decision (controlIndex 1) outcomeDigest fixtureHeraldEpoch (heraldMembershipGenerationId fixtureMembershipGeneration)))
      completionReceipt <- submitConformanceRequest homeClient completionRequest
      assertEqual "one collected completion releases the slot" (controlIndex 2) (oracleReceiptControlIndex completionReceipt)
      retryLeader <- awaitReadyLeader cluster
      admitted <- submitConformanceRequestDispositionVia homeClient retryLeader secondRequest
      secondReceipt <- case admitted of
        Just (ConformanceSubmissionReceipt receipt) -> pure receipt
        other -> assertFailure ("expected the exact deferred decision to become admissible, got " <> show other)
      assertEqual "the exact deferred decision orders after completion" (controlIndex 3) (oracleReceiptControlIndex secondReceipt)
      duplicate <- submitConformanceRequest homeClient secondRequest
      assertEqual "the accepted decision retains exact retry" secondReceipt duplicate
      let olderProgress = oracleProgress mempty (controlIndex 1)
          newerProgress = oracleProgress mempty (controlIndex 3)
      assertEqual "label-only progress uses the production maintenance reply" olderProgress =<< retireConformanceProgress homeClient olderProgress
      assertEqual "the same receipt high water can advance its label frontier" newerProgress =<< retireConformanceProgress homeClient newerProgress
      -- Each conformance maintenance call opens a fresh TCP lane. A delayed
      -- snapshot after reconnect must acknowledge the componentwise join, and
      -- neither confirmation nor replay may create another maintenance entry.
      assertEqual "a stale same-high-water offer after reconnect cannot regress progress" newerProgress =<< retireConformanceProgress homeClient olderProgress
      assertEqual "a repeated current offer is idempotent" newerProgress =<< retireConformanceProgress homeClient newerProgress
      assertEqual "label-only retirement preserves the first exact receipt" (ConformanceRequestFound firstReceipt) =<< queryConformanceRequest homeClient firstRequest
      assertEqual "label-only retirement preserves the second exact receipt" (ConformanceRequestFound secondReceipt) =<< queryConformanceRequest homeClient secondRequest
      suffix <- watchConformanceEntries homeClient (controlIndex 3)
      assertEqual "only two advancing progress announcements consume entries" [controlIndex 4, controlIndex 5] (map appliedEntryControlIndex (NonEmpty.toList suffix))
      assertEqual "maintenance watch entries confirm the full progress" [Just (fixtureHeraldEpoch, olderProgress), Just (fixtureHeraldEpoch, newerProgress)] (map appliedEntryOracleProgress (NonEmpty.toList suffix))
      assertEqual "progress creates no semantic acknowledgement events" [[], []] (map appliedEntryProjectionEvents (NonEmpty.toList suffix))
      _ <- awaitNodesAppliedPrefix cluster fixtureRaftNodes 5
      finalStatuses <- fmap (fmap snd) (oracleTcpNodeStatuses cluster)
      assertBool "confirmations and stale offers do not cause an acknowledgement loop" (all ((== controlIndex 5) . oracleRuntimeAppliedControlIndex) finalStatuses)
  either (assertFailure . show) pure composed

caseReservedLeaderPrefixStable :: IO ()
caseReservedLeaderPrefixStable = do
  checkpointReached <- newEmptyMVar
  releaseCheckpoint <- newEmptyMVar
  let checkpoint request command index = do
        putMVar checkpointReached (request, command, index)
        readMVar releaseCheckpoint
      releaseCheckpointOnce = void (tryPutMVar releaseCheckpoint ())
      configuration =
        checked
          "checkpoint Oracle TCP configuration"
          ( oracleTcpClusterConfiguration
              ( fmap
                  (configureOracleRuntimeRecording OracleCaptureHistory . configureOracleRuntimeSubmissionCheckpoint checkpoint)
                  fixtureRuntimeConfigurations
              )
          )
  bounded <-
    ( timeout
        30000000
        ( withOracleTcpCluster configuration $ \cluster ->
            ( do
                leader <- awaitReadyLeader cluster
                client <-
                  newConformanceClient fixtureCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster
                    >>= either (assertFailure . show) pure
                request <- prepareConformanceRequest client (Just (controlIndex 0)) validOracleCommand
                withAcceptedRetryLane cluster leader $ \retryConnection retryDecoder -> do
                  completed <- newEmptyMVar
                  _ <-
                    forkFinally
                      (submitConformanceRequest client request)
                      (putMVar completed)

                  (checkpointRequest, checkpointCommand, checkpointIndex) <-
                    takeWithin "the first submission did not reach semantic preflight" checkpointReached
                  assertEqual "the checkpoint names the first-seen request" (conformanceRequestId request) checkpointRequest
                  assertEqual "the checkpoint names the exact reserved command" validOracleCommand checkpointCommand
                  assertEqual "preflight classified the genesis Oracle prefix" (controlIndex 0) checkpointIndex

                  beforeRetry <- oracleTcpNodeRecordings cluster
                  assertEqual
                    "preflight has not registered a Raft proposal"
                    0
                    (appendedProposalCount beforeRetry)

                  let canonical =
                        canonicalizeOracleEnvelope
                          ( oracleEnvelope
                              (conformanceRequestId request)
                              (Just (controlIndex 0))
                              fixtureHeraldEpoch
                              validOracleCommand
                          )
                  progressDecoder <- assertExactRetryNotReady retryConnection retryDecoder (conformanceRequestId request) canonical
                  assertProgressOffersNotReady retryConnection progressDecoder

                  afterRetry <- oracleTcpNodeRecordings cluster
                  assertEqual
                    "the exact retry cannot register behind the semantic reservation"
                    0
                    (appendedProposalCount afterRetry)

                  releaseCheckpointOnce
                  firstOutcome <- takeWithin "the released first submission did not finish" completed
                  first <- either (assertFailure . show) pure firstOutcome
                  assertEqual
                    "the stable preflight token admits control one"
                    (controlIndex 1)
                    (oracleReceiptControlIndex first)
                  _ <- awaitNodesAppliedPrefix cluster fixtureRaftNodes 1
                  finalRecordings <- oracleTcpNodeRecordings cluster
                  assertEqual
                    "only the first-seen request reaches Raft"
                    1
                    (appendedProposalCount finalRecordings)
            )
              `finally` releaseCheckpointOnce
        )
    )
      `finally` releaseCheckpointOnce
  composed <-
    maybe
      (assertFailure "the reserved-prefix coordination schedule exceeded its outer timeout")
      pure
      bounded
  either (assertFailure . show) pure composed

-- This schedule leaves the original EORC call at its public preflight
-- checkpoint. A delayed, checked ERFT prefix from another configured replica
-- represents a higher-term leader's no-op and committed ordinary command. The
-- sender then remains stopped, while the original two surviving native owners
-- elect the original serving node again. No private runtime state is changed.
caseReservationAcrossLeadership :: IO ()
caseReservationAcrossLeadership = do
  checkpointReached <- newEmptyMVar
  releaseCheckpoint <- newEmptyMVar
  let reservedRequest = oracleClientRequestId fixtureHeraldEpoch 401
      interveningRequest = oracleClientRequestId fixtureHeraldEpoch 402
      reservedCanonical = canonicalizeOracleEnvelope (oracleEnvelope reservedRequest Nothing fixtureHeraldEpoch validOracleCommand)
      interveningCanonical = canonicalizeOracleEnvelope (oracleEnvelope interveningRequest (Just (controlIndex 0)) fixtureHeraldEpoch alternateOracleCommand)
      checkpoint request _ index =
        if request == reservedRequest
          then putMVar checkpointReached index >> readMVar releaseCheckpoint
          else pure ()
      release = void (tryPutMVar releaseCheckpoint ())
      native = checkedOracleRaftNativeConfiguration fixtureCheckedGenesis
      -- The greatest node accepts the ordinary initiation side from the
      -- delayed sender. Its shorter timeout makes re-election deterministic.
      leader = fixtureFollowerNode
      sender = fixtureReplacementNode
      configurations =
        [ configureOracleRuntimeRecording OracleCaptureHistory
            $ configureOracleRuntimeSubmissionCheckpoint
              checkpoint
              ( checked
                  "term-scoped reservation runtime"
                  ( oracleRuntimeConfiguration
                      (raftGenesis node fixtureRaftNodes (raftNativeHeartbeatInterval native) (raftNativeElectionTimeoutLower native) (raftNativeElectionTimeoutUpper native))
                      fixtureOracleGenesis
                      member.heraldMemberEpoch
                      (runtimeElectionTimeoutSource (pure (if node == leader then 0 else 60000)))
                  )
              )
        | (node, member) <- zip fixtureRaftNodes fixtureMembers
        ]
      configuration = checked "term-scoped reservation cluster" (oracleTcpClusterConfiguration configurations)
  bounded <-
    timeout
      30000000
      ( withOracleTcpCluster configuration $ \cluster ->
          ( do
              elected <- awaitReadyLeader cluster
              assertEqual "the scheduled original leader is the greatest node" leader elected
              original <- nodeStatus cluster leader
              let originalTerm = oracleRuntimeTerm original
                  interveningTerm = raftTerm (raftTermWord64 originalTerm + 1)
              withAcceptedRetryLane cluster leader $ \connection decoder -> do
                sendCanonical connection reservedCanonical
                observedPrefix <- takeWithin "the first attempt did not reserve its old-term prefix" checkpointReached
                assertEqual "the original reservation observed control zero" (controlIndex 0) observedPrefix
                before <- oracleTcpNodeRecordings cluster
                assertEqual "the reserved command has no native proposal" 0 (appendedProposalCount before)
                stopped <- stopOracleTcpNode cluster sender
                assertBool "the delayed sender is stopped before its queued history arrives" stopped
                deliverInterveningPrefix cluster sender leader originalTerm interveningTerm interveningCanonical
                awaitRuntimeCondition "the original node did not apply the intervening command and regain native leadership" $ do
                  current <- nodeStatus cluster leader
                  recordings <- oracleTcpNodeRecordings cluster
                  let acknowledged =
                        any
                          ( \case
                              RuntimeRaftInputInstalled input -> case RaftInput.raftInputView input of
                                RaftInput.AcknowledgeCommittedEntriesView index -> index == raftLogIndex 4
                                _ -> False
                              _ -> False
                          )
                          (maybe [] id (lookup leader recordings))
                  pure (oracleRuntimeRole current == RaftLeader && oracleRuntimeTerm current > interveningTerm && oracleRuntimeAppliedControlIndex current == controlIndex 1 && acknowledged)
                release
                (reply, successor) <- receiveServerFrame decoder connection
                case reply of
                  OracleSubmissionNotReady request term -> do
                    assertEqual "the stale reservation is retriable for its exact request" (oracleClientRequestIdDtoFromCore reservedRequest) request
                    assertBool "the retry names the new leadership term" (raftTermDtoToCore term > interveningTerm)
                  other -> assertFailure ("stale reservation returned " <> show other)
                after <- oracleTcpNodeRecordings cluster
                assertEqual "the stale attempt registered no proposal after re-election" 0 (appendedProposalCount after)
                ready <- awaitReadyLeader cluster
                assertEqual "the rejection releases semantic readiness on the regained leader" leader ready
                -- The unchanged request now gets a fresh term and owner preflight.
                sendCanonical connection reservedCanonical
                secondPrefix <- takeWithin "the retry did not obtain a fresh owner preflight" checkpointReached
                assertEqual "fresh preflight observes the genuine intervening control entry" (controlIndex 1) secondPrefix
                (retryReply, _) <- receiveServerFrame successor connection
                case retryReply of
                  OracleReceipt _ -> pure ()
                  other -> assertFailure ("fresh reservation returned " <> show other)
                final <- nodeStatus cluster leader
                assertEqual "only the two ordinary commands create control entries" (controlIndex 2) (oracleRuntimeAppliedControlIndex final)
                finalRecordings <- oracleTcpNodeRecordings cluster
                assertEqual "only the fresh retry is proposed locally" 1 (appendedProposalCount finalRecordings)
          )
            `finally` release
      )
      `finally` release
  case bounded of
    Just (Right ()) -> pure ()
    other -> assertFailure ("term-scoped reservation schedule failed: " <> show other)
  where
    sendCanonical connection canonical = SocketByteString.sendAll connection (encodeOracleFrame (OracleClientEnvelope (SubmitOracleCommand (canonicalOracleEnvelopeDtoFromCore canonical))))
    nodeStatus cluster node = do
      statuses <- oracleTcpNodeStatuses cluster
      maybe (assertFailure "reservation fixture lost its original node") pure (lookup node statuses)

awaitRuntimeCondition :: String -> IO Bool -> IO ()
awaitRuntimeCondition failure predicate = do
  completed <- timeout 5000000 loop
  maybe (assertFailure failure) pure completed
  where
    loop = predicate >>= \ready -> if ready then pure () else threadDelay 1000 >> loop

deliverInterveningPrefix :: OracleTcpCluster -> RaftNodeId -> RaftNodeId -> RaftTerm -> RaftTerm -> CanonicalOracleEnvelope -> IO ()
deliverInterveningPrefix cluster sender recipient originalTerm nextTerm canonical = do
  endpoint <- maybe (assertFailure "the reserved node lacks its ERFT listener") pure (lookup recipient (oracleTcpRaftEndpoints cluster))
  let source = checked "delayed source" (RaftProtocol.raftNodeIdDtoFromCore sender)
      target = checked "reserved target" (RaftProtocol.raftNodeIdDtoFromCore recipient)
      digest = checked "immutable native run" (RaftProtocol.raftGenesisDigestDto (raftConfigurationDigestBytes (checkedOracleRaftConfigurationDigest fixtureCheckedGenesis)))
      hello = checked "delayed peer Hello" (RaftProtocol.raftHelloClaim digest source target)
      context = checked "delayed peer context" (RaftProtocol.raftConnectionContext digest source target)
      logEntry index term payload = checked "delayed checked log entry" (RaftProtocol.raftLogEntryDto (RaftProtocol.raftLogIndexDto index) (RaftProtocol.raftTermDtoFromCore term) payload)
      -- The original service-ready node has applied position 1 and may have
      -- compacted it. Continue from that known common position, as a real
      -- sender does after discovering a receiver's compacted base.
      entries = [logEntry 2 nextTerm RaftProtocol.LeaderNoOpDto, logEntry 3 nextTerm (RaftProtocol.ApplicationBytesDto (canonicalOracleEnvelopeBytes canonical))]
      append = checked "delayed suffix after the common base" (RaftProtocol.appendEntriesDto (RaftProtocol.raftTermDtoFromCore nextTerm) source (RaftProtocol.raftLogIndexDto 1) (RaftProtocol.raftTermDtoFromCore originalTerm) entries (RaftProtocol.raftLogIndexDto 3))
  bracket (connectEndpoint endpoint) Socket.close $ \connection -> do
    SocketByteString.sendAll connection (RaftFrame.encodeRaftFrame (RaftProtocol.RaftHello hello))
    helloBytes <- receiveRawFrame connection
    decoder <- case RaftFrame.feedRaftFrame (RaftFrame.initialRaftFrameDecoder context) helloBytes of
      RaftFrame.RaftFrameFeedResult [RaftProtocol.RaftHello admitted] (RaftFrame.NeedRaftFrameBytes successor) -> do
        assertEqual "the delayed lane preserves its native run" digest (RaftProtocol.raftHelloGenesisDigest admitted)
        pure successor
      other -> assertFailure ("delayed peer admission failed: " <> show other)
    SocketByteString.sendAll connection (RaftFrame.encodeRaftFrame (RaftProtocol.RaftRpc append))
    awaitAcknowledgement decoder connection
  where
    awaitAcknowledgement decoder connection = do
      bytes <- receiveRawFrame connection
      case RaftFrame.feedRaftFrame decoder bytes of
        RaftFrame.RaftFrameFeedResult [RaftProtocol.RaftRpc rpc] (RaftFrame.NeedRaftFrameBytes successor) -> case RaftProtocol.raftAppendEntriesResponseFields rpc of
          Just (term, source, True, index, _, Nothing) -> do
            assertEqual "the response comes from the reserved replica" (Right recipient) (RaftProtocol.raftNodeIdDtoToCore source)
            assertEqual "the intervening term reached the native owner" nextTerm (RaftProtocol.raftTermDtoToCore term)
            assertEqual "the complete checked prefix was accepted" (RaftProtocol.raftLogIndexDto 3) index
          _ -> awaitAcknowledgement successor connection
        other -> assertFailure ("delayed prefix response failed: " <> show other)

caseThrowingCheckpointReleasesReservation :: IO ()
caseThrowingCheckpointReleasesReservation = do
  checkpointCalls <- newIORef (0 :: Int)
  let checkpoint _ _ _ = do
        invocation <-
          atomicModifyIORef' checkpointCalls $ \current ->
            let successor = current + 1
             in (successor, successor)
        case invocation of
          1 -> throwIO InjectedSubmissionCheckpointFailure
          _ -> pure ()
      configuration =
        checked
          "throwing-checkpoint Oracle TCP configuration"
          ( oracleTcpClusterConfiguration
              ( fmap
                  (configureOracleRuntimeRecording OracleCaptureHistory . configureOracleRuntimeSubmissionCheckpoint checkpoint)
                  fixtureRuntimeConfigurations
              )
          )
  bounded <-
    timeout 30000000 $ withOracleTcpCluster configuration $ \cluster -> do
      leaderBefore <- awaitReadyLeader cluster
      client <-
        newConformanceClient fixtureCheckedGenesis fixtureHeraldId fixtureHeraldEpoch cluster
          >>= either (assertFailure . show) pure
      request <- prepareConformanceRequest client (Just (controlIndex 0)) validOracleCommand
      receipt <- submitConformanceRequest client request
      assertEqual
        "the retry after the throwing checkpoint orders control one"
        (controlIndex 1)
        (oracleReceiptControlIndex receipt)
      leaderAfter <- awaitReadyLeader cluster
      assertEqual
        "the reservation is retried at the same leader"
        leaderBefore
        leaderAfter
      invocations <- readIORef checkpointCalls
      assertBool
        "the injected failure was followed by a fresh successful preflight"
        (invocations >= 2)
      recordings <- oracleTcpNodeRecordings cluster
      assertEqual
        "the throwing checkpoint registered no proposal before the sole retry"
        1
        (appendedProposalCount recordings)
  composed <-
    maybe
      (assertFailure "the throwing-checkpoint retry remained stranded")
      pure
      bounded
  either (assertFailure . show) pure composed

propChangedPreflightTokenFaults :: NonNegative Word32 -> Property
propChangedPreflightTokenFaults (NonNegative seed) =
  let observedIndex = controlIndex (fromIntegral seed)
      currentIndex = controlIndex (fromIntegral seed + 1)
      expectedFailure =
        RuntimeSubmissionPreflightInvariantFault observedIndex currentIndex
      decision =
        decideSubmissionRegistration
          RuntimeSubmissionPreflightInvariantFault
          (raftTerm 1)
          (raftTerm 1)
          observedIndex
          currentIndex
   in counterexample ("registration decision: " <> show decision)
        $ (decision === FailSubmissionRegistration expectedFailure)
          .&&. (submissionRegistrationProposalCount decision === 0)
          .&&. (submissionRegistrationRecursivePreflightCount decision === 0)

propStaleReservationRetries :: NonNegative Word32 -> Bool -> Property
propStaleReservationRetries (NonNegative seed) changedPrefix =
  let observedIndex = controlIndex (fromIntegral seed)
      currentIndex = controlIndex (fromIntegral seed + if changedPrefix then 1 else 0)
      observedTerm = raftTerm (fromIntegral seed + 1)
      currentTerm = raftTerm (fromIntegral seed + 2)
      decision = decideSubmissionRegistration RuntimeSubmissionPreflightInvariantFault observedTerm currentTerm observedIndex currentIndex
   in counterexample ("stale reservation decision: " <> show decision)
        $ (decision === RetryStaleSubmissionReservation)
          .&&. (submissionRegistrationProposalCount decision === 0)
          .&&. (submissionRegistrationRecursivePreflightCount decision === 0)

appendedProposalCount :: [(RaftNodeId, [OracleRuntimeEvent])] -> Int
appendedProposalCount recordings =
  length
    [ ()
    | (_, events) <- recordings,
      RuntimeProposalObserved _ RaftProposalAppended {} <- events
    ]

withAcceptedRetryLane ::
  OracleTcpCluster ->
  RaftNodeId ->
  (Socket.Socket -> OracleIngressDecoder -> IO result) ->
  IO result
withAcceptedRetryLane cluster leader use = do
  endpoint <-
    maybe
      (assertFailure "the ready leader has no Oracle TCP endpoint")
      pure
      (lookup leader (oracleTcpOracleContacts cluster))
  bracket (connectEndpoint endpoint) Socket.close $ \connection -> do
    SocketByteString.sendAll
      connection
      (encodeOracleFrame (OracleClientEnvelope (OracleHello fixtureHello)))
    (hello, established) <-
      receiveServerFrame
        (initialOracleIngressDecoder oracleClientIngressContext)
        connection
    case hello of
      OracleHelloAccepted {} -> use connection established
      other -> assertFailure ("the exact-retry lane was not accepted: " <> show other)

assertExactRetryNotReady ::
  Socket.Socket ->
  OracleIngressDecoder ->
  OracleClientRequestId ->
  CanonicalOracleEnvelope ->
  IO OracleIngressDecoder
assertExactRetryNotReady connection decoder request canonical = do
  SocketByteString.sendAll
    connection
    ( encodeOracleFrame
        ( OracleClientEnvelope
            (SubmitOracleCommand (canonicalOracleEnvelopeDtoFromCore canonical))
        )
    )
  (response, successor) <- receiveServerFrame decoder connection
  case response of
    OracleSubmissionNotReady observed _ -> do
      assertEqual
        "the reservation rejects the exact concurrent request as not ready"
        (oracleClientRequestIdDtoFromCore request)
        observed
      pure successor
    other -> assertFailure ("the exact concurrent retry returned " <> show other)

-- The held reservation prevents semantic admission, allowing the same physical
-- lane to receive distinct full offers at one routing request sequence. Even a
-- reordered old snapshot must be echoed exactly, never reconstructed from H.
assertProgressOffersNotReady :: Socket.Socket -> OracleIngressDecoder -> IO ()
assertProgressOffersNotReady connection = go [controlIndex 20, controlIndex 30, controlIndex 20]
  where
    request = oracleClientRequestId fixtureHeraldEpoch 0
    go [] _ = pure ()
    go (labelsThrough : rest) decoder = do
      let progress = oracleProgress mempty labelsThrough
          canonical = canonicalizeOracleEnvelope (oracleEnvelope request Nothing fixtureHeraldEpoch (retireOracleProgressCommand progress))
      SocketByteString.sendAll connection (encodeOracleFrame (OracleClientEnvelope (SubmitOracleCommand (canonicalOracleEnvelopeDtoFromCore canonical))))
      (response, successor) <- receiveServerFrame decoder connection
      case response of
        OracleProgressNotReady observed offered _ -> do
          assertEqual "maintenance retains its routing request" (oracleClientRequestIdDtoFromCore request) observed
          assertEqual "NotReady echoes the complete exact offer" progress (oracleProgressDtoToCore offered)
          go rest successor
        other -> assertFailure ("the reserved maintenance offer returned " <> show other)

takeWithin :: String -> MVar value -> IO value
takeWithin context value = do
  observed <- timeout 5000000 (takeMVar value)
  maybe (assertFailure context) pure observed

fixtureHello :: OracleHelloDto
fixtureHello =
  oracleHelloDto
    (systemIdClaimFromDomain (checkedOracleSystemId fixtureCheckedGenesis))
    (catalogueDigestClaimFromDomain (checkedOracleCatalogueDigest fixtureCheckedGenesis))
    (configurationDigestClaimFromDomain (checkedOracleConfigurationDigest fixtureCheckedGenesis))
    (initialProjectionDigestClaimFromDomain (checkedOracleInitialProjectionDigest fixtureCheckedGenesis))
    (heraldIdClaimFromDomain fixtureHeraldId)
    (heraldEpochClaimFromDomain fixtureHeraldEpoch)
    (controlIndexDtoFromDomain (controlIndex 0))
    ( heraldMembershipGenerationClaimFromDomain
        (heraldMembershipGenerationId fixtureMembershipGeneration)
    )

fixtureMembershipGeneration :: HeraldMembershipGeneration
fixtureMembershipGeneration =
  checked
    "Herald membership generation"
    ( genesisHeraldMembershipGeneration
        fixtureSystemId
        (NonEmpty.fromList (fmap heraldMemberEpoch fixtureMembers))
    )

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
