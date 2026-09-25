{-# LANGUAGE OverloadedStrings #-}

-- | Real native role loss at causal boundaries of automatic exclusion. Five
-- voters leave an old majority after both the target and leader have stopped.
module FailureRuntimeProperties (tests) where

import Control.Concurrent.STM (TVar, atomically, check, newTVarIO, readTVar, writeTVar)
import Control.Monad (forM, forM_, void)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.List.NonEmpty qualified as NE
import Data.Word (Word8)
import Eclips.Domain.Identity (controlIndex, mkHeraldEpoch, mkHeraldId)
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Startup (HeraldMember (..), InitialTopologyManifest (..), checkInitialTopologyProjection, deriveInitialProjectionDigest)
import Eclips.Oracle.Command
import Eclips.Oracle.Failure
import Eclips.Oracle.Genesis
import Eclips.Oracle.Projection
import Eclips.Oracle.Receipt
import Eclips.Oracle.Runtime
import Eclips.Oracle.Runtime.ConformanceClient
import Eclips.Oracle.Runtime.Internal.Submission (mayCancelVoterPreparation)
import Eclips.Oracle.Runtime.TCP
import Eclips.Oracle.Voter
import Eclips.Protocol.Oracle.Types (oracleHealthReplyConfiguration)
import Eclips.Raft.Configuration (RaftVotingConfigurationView (..), raftVotingConfigurationView)
import Eclips.Raft.Genesis qualified as RG
import Eclips.Raft.Identity (RaftNodeId, mkRaftDurationMicros, mkRaftNodeId)
import HealthRuntimeProperties (queryHealth, queryHealthAs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestFixtures
import VoterRuntimeProperties (poll, send, withCluster, within)

data FailurePhase = Preparing | JointAppended | JointCommitted | FinalAppended | FinalCommitted | SemanticCommitted | SemanticApplied
  deriving stock (Eq, Enum, Bounded, Show)

tests :: TestTree
tests =
  testGroup
    "accepted voter-host failure TCP"
    ( testCase "p09-failure-native-target-still-running: fresh target health cannot restore excluded authority" (caseFailureWithTarget False 3 Nothing)
        : testCase "p09-failure-three-to-two: captured majority, native exclusion, semantic retirement and immutable replay" (caseFailure 3 Nothing)
        : [testCase ("p09-failure-leader-loss-" <> phaseName phase) (caseFailure 5 (Just phase)) | phase <- [minBound .. maxBound]]
    )

phaseName :: FailurePhase -> String
phaseName phase = case phase of
  Preparing -> "preparing"
  JointAppended -> "joint-appended"
  JointCommitted -> "joint-committed"
  FinalAppended -> "final-appended"
  FinalCommitted -> "final-committed"
  SemanticCommitted -> "semantic-committed"
  SemanticApplied -> "semantic-applied"

matches :: FailurePhase -> OracleCommand -> OracleVoterCheckpoint -> Bool
matches phase retirement event = case (phase, event) of
  (Preparing, OracleVoterPreparationCaptured _) -> True
  (JointAppended, OracleVoterConfigurationAppended configuration _) -> joint configuration
  (JointCommitted, OracleVoterConfigurationCommitted configuration _) -> joint configuration
  (FinalAppended, OracleVoterConfigurationAppended configuration _) -> not (joint configuration)
  (FinalCommitted, OracleVoterConfigurationCommitted configuration _) -> not (joint configuration)
  (SemanticCommitted, OracleApplicationCommitted command _) -> command == retirement
  (SemanticApplied, OracleApplicationApplied entry _) -> any retired (appliedEntryProjectionEvents entry)
  _ -> False
  where
    joint configuration = case raftVotingConfigurationView configuration of JointRaftConfigurationView {} -> True; StableRaftConfigurationView {} -> False
    retired eventValue = case oracleProjectionEventView eventValue of HeraldMembershipAdvancedView _ -> True; _ -> False

caseFailure :: Int -> Maybe FailurePhase -> Assertion
caseFailure = caseFailureWithTarget True

caseFailureWithTarget :: Bool -> Int -> Maybe FailurePhase -> Assertion
caseFailureWithTarget stopTarget count failurePhase = do
  armed <- newTVarIO Nothing
  interrupted <- newTVarIO Nothing
  let (genesis, nativeGenesis, members, nodes) = failureFixture count
      target = lastFixture members
      targetNode = lastFixture nodes
      hook node event = do
        trigger <- atomically $ do
          pending <- readTVar armed
          case (failurePhase, pending) of
            (Just phase, Just (leader, retirement)) | leader == node && matches phase retirement event -> do
              writeTVar armed Nothing
              writeTVar interrupted (Just node)
              pure True
            _ -> pure False
        if trigger then ioError (userError ("injected failure workflow owner loss at " <> show failurePhase)) else pure ()
      configurations =
        [ configureOracleRuntimeVoterCheckpoint
            (hook node)
            (checked "failure runtime" (oracleRuntimeConfiguration raft genesis (heraldMemberEpoch member) (runtimeElectionTimeoutSource (pure (fromIntegral ordinal * 50000)))))
        | (ordinal, (node, raft, member)) <- zip [0 :: Int ..] (zip3 nodes nativeGenesis members)
        ]
  withCluster (checked "failure TCP cluster" (oracleTcpClusterConfiguration configurations)) $ \cluster -> do
    _ <- awaitReadyLeader cluster
    clients <- forM members $ \member -> newConformanceClient (checked "failure genesis" (checkOracleGenesis genesis)) (heraldMemberId member) (heraldMemberEpoch member) cluster >>= either (assertFailure . show) pure
    let client = firstFixture clients
    initial <- leaderStatus cluster
    let captured = oracleRuntimeVoterConfiguration initial
        membership = oracleRuntimeMembershipGeneration initial
    (_, openReceipt) <- send client (openHeraldFailureProbeCommand (heraldMemberEpoch target) (Membership.heraldMembershipGenerationId membership) (voterConfigurationId captured))
    probe <- case oracleReceiptFailureResult openReceipt of Just (FailureProbeOpened identifier) -> pure identifier; other -> assertFailure ("Open rejected: " <> show other)
    -- The target runtime is physically absent before fresh reporter results.
    if stopTarget
      then do
        stopped <- stopOracleTcpNode cluster targetNode
        assertBool "target role stopped" stopped
      else pure ()
    forM_ (take (count `div` 2 + 1) clients) $ \reporter -> do
      (_, receipt) <- send reporter (reportHeraldFailureProbeCommand probe (voterConfigurationId captured) ProbeUnreachable)
      oracleReceiptResult receipt @?= OracleAccepted
    let resolution = Membership.deriveFailureProbeResolutionId probe Membership.RetireFailureProbeTarget
        retirement = retireHeraldEpochCommand resolution (heraldMemberEpoch target)
        semanticPhase = failurePhase `elem` [Just SemanticCommitted, Just SemanticApplied]
        arm = do
          leader <- awaitReadyLeader cluster
          atomically (writeTVar armed (Just (leader, retirement)))
    if semanticPhase || failurePhase == Nothing then pure () else arm
    (acceptRequest, acceptanceReceipt) <- send client (acceptVoterHostFailureCommand resolution)
    certificate <- case oracleReceiptFailureResult acceptanceReceipt of Just (VoterHostFailureAcceptedResult value) -> pure value; other -> assertFailure ("failure acceptance rejected: " <> show other)
    acceptedVoterHostFailureConfiguration certificate @?= captured
    length (acceptedVoterHostFailureReports certificate) @?= count `div` 2 + 1
    let identifier = acceptedVoterHostFailureChangeId certificate
    if semanticPhase || failurePhase == Nothing then pure () else within "native failure checkpoint" (atomically (readTVar interrupted >>= check . (/= Nothing)))
    excluded <- awaitPhase cluster interrupted targetNode identifier VoterExcludedAwaitingHeraldRetirement
    oracleRuntimeMembershipGeneration excluded @?= membership
    let expected = filter (/= targetNode) nodes
    map raftVoterBindingNode (voterConfigurationBindings (oracleRuntimeVoterConfiguration excluded)) @?= expected
    if stopTarget
      then pure ()
      else do
        contact <- maybe (assertFailure "target health endpoint missing") pure (lookup targetNode (oracleTcpOracleContacts cluster))
        health <- within "excluded target fresh native health" (queryHealth (checked "failure checked genesis" (checkOracleGenesis genesis)) contact 909)
        -- The old target may miss final commitment. Its local health is only a
        -- liveness observation, and never replaces the survivors' configuration.
        let observed = snd (oracleHealthReplyConfiguration health)
            oldNodes = fmap raftVoterBindingNode (voterConfigurationBindings captured)
        case raftVotingConfigurationView observed of
          StableRaftConfigurationView local -> assertBool "target native view belongs to this history" (local == oldNodes || local == expected)
          JointRaftConfigurationView old new -> (old, new) @?= (oldNodes, expected)
    if semanticPhase then arm else pure ()
    (retireRequest, retirementReceipt) <- send client retirement
    case oracleReceiptFailureResult retirementReceipt of
      Just (HeraldRetiredResult same _) -> same @?= resolution
      other -> assertFailure ("retirement rejected: " <> show other)
    if semanticPhase then within "semantic failure checkpoint" (atomically (readTVar interrupted >>= check . (/= Nothing))) else pure ()
    completed <- awaitCompleted cluster interrupted targetNode
    survivingContact <- maybe (assertFailure "surviving health endpoint missing") pure (lookup (oracleRuntimeNode completed) (oracleTcpOracleContacts cluster))
    denied <- within "retired source cannot renew health after missing its watch" (queryHealthAs (checked "failure checked genesis" (checkOracleGenesis genesis)) (heraldMemberId target) (heraldMemberEpoch target) survivingContact 910)
    denied @?= Nothing
    assertBool "semantic membership changed only after retirement" (oracleRuntimeMembershipGeneration completed /= membership)
    replayedAcceptance <- within "original acceptance receipt after role loss" (submitConformanceRequest client acceptRequest)
    replayedAcceptance @?= acceptanceReceipt
    replayedRetirement <- within "original retirement receipt after role loss" (submitConformanceRequest client retireRequest)
    replayedRetirement @?= retirementReceipt
    (_, semanticRetry) <- send client (acceptVoterHostFailureCommand resolution)
    oracleReceiptFailureResult semanticRetry @?= Just (VoterHostFailureAcceptedResult certificate)
    history <- NE.toList <$> within "completed failure watch" (watchConformanceEntries client (controlIndex 0))
    let phases = [voterChangePhase change | entry <- history, event <- appliedEntryProjectionEvents entry, OracleVoterChangeChangedView change <- [oracleProjectionEventView event], voterChangeId change == identifier]
        observations = [configuration | entry <- history, Just configuration <- [appliedEntryConfiguration entry]]
        acceptances = [retained | entry <- history, event <- appliedEntryProjectionEvents entry, OracleVoterHostFailureAcceptedView retained <- [oracleProjectionEventView event]]
    let preparingChanges = [change | entry <- history, event <- appliedEntryProjectionEvents entry, OracleVoterChangeChangedView change <- [oracleProjectionEventView event], voterChangeId change == identifier, voterChangePhase change == VoterChangePreparing]
    case preparingChanges of
      [change] -> mayCancelVoterPreparation identifier (Just change) @?= False
      _ -> assertFailure "expected one retained automatic Preparing record"
    phases @?= [VoterChangePreparing, VoterChangeJointCommitted, VoterExcludedAwaitingHeraldRetirement, VoterChangeCompleted]
    length observations @?= 2
    acceptances @?= [certificate, certificate]
    void (send client validOracleCommand)
    serving <- leaderStatus cluster
    map raftVoterBindingNode (voterConfigurationBindings (oracleRuntimeVoterConfiguration serving)) @?= expected

leaderStatus :: OracleTcpCluster -> IO OracleRuntimeStatus
leaderStatus cluster = do
  leader <- awaitReadyLeader cluster
  statuses <- oracleTcpNodeStatuses cluster
  maybe (assertFailure "ready leader disappeared") pure (lookup leader statuses)

awaitPhase :: OracleTcpCluster -> TVar (Maybe RaftNodeId) -> RaftNodeId -> VoterChangeId -> VoterChangePhase -> IO OracleRuntimeStatus
awaitPhase cluster interrupted target identifier phase = within ("failure workflow phase " <> show phase) $ poll $ do
  failed <- atomically (readTVar interrupted)
  statuses <- oracleTcpNodeStatuses cluster
  let active = [status | (node, status) <- statuses, node /= target, Just node /= failed]
      correct status = case oracleRuntimePendingVoterChange status of Just change -> voterChangeId change == identifier && voterChangePhase change == phase; _ -> False
  pure (if all correct active then find oracleRuntimeServiceReady active else Nothing)

awaitCompleted :: OracleTcpCluster -> TVar (Maybe RaftNodeId) -> RaftNodeId -> IO OracleRuntimeStatus
awaitCompleted cluster interrupted target = within "failure workflow completion on every surviving adapter" $ poll $ do
  failed <- atomically (readTVar interrupted)
  statuses <- oracleTcpNodeStatuses cluster
  let active = [status | (node, status) <- statuses, node /= target, Just node /= failed]
      complete status = oracleRuntimePendingVoterChange status == Nothing && not (target `elem` fmap raftVoterBindingNode (voterConfigurationBindings (oracleRuntimeVoterConfiguration status)))
  pure (if all complete active then find oracleRuntimeServiceReady active else Nothing)

failureFixture :: Int -> (OracleGenesis, [RG.RaftGenesis], [HeraldMember], [RaftNodeId])
failureFixture count = (genesis, nativeGenesis, members, nodes)
  where
    existing = fixtureCheckedGenesis
    members = take count (fixtureMembers <> [HeraldMember (identity mkHeraldId 16) (identity mkHeraldEpoch 17), HeraldMember (identity mkHeraldId 18) (identity mkHeraldEpoch 19)])
    nodes = take count (fixtureRaftNodes <> [identity mkRaftNodeId 5, identity mkRaftNodeId 6])
    nativeGenesis = [RG.raftGenesis node nodes (duration 20000) (duration 120000) (duration 420000) | node <- nodes]
    native = RG.checkedRaftNativeConfiguration (checked "failure native genesis" (RG.checkRaftGenesis (firstFixture nativeGenesis)))
    bindings = zipWith raftVoterBinding nodes (fmap heraldMemberEpoch members)
    bootstraps = checkedOracleAppliedBootstraps existing
    topology = checked "failure initial topology" (checkInitialTopologyProjection (checkedOracleSystemId existing) members bootstraps (InitialTopologyManifest []))
    genesis = oracleGenesis (checkedOracleSystemId existing) members (checkedOracleCatalogueDigest existing) (checkedOraclePredefinedDescriptors existing) bootstraps (checkedOracleConfigurationDigest existing) topology (deriveInitialProjectionDigest bootstraps topology) bindings native (deriveRaftConfigurationDigest (checkedOracleSystemId existing) bindings native)
    duration = checked "failure duration" . mkRaftDurationMicros

identity :: (Show problem) => (ByteString -> Either problem value) -> Word8 -> value
identity constructor = checked "failure fixture identity" . constructor . identifierBytes
firstFixture :: [value] -> value
firstFixture (value : _) = value
firstFixture [] = error "empty failure runtime fixture"
lastFixture :: [value] -> value
lastFixture [value] = value
lastFixture (_ : remaining) = lastFixture remaining
lastFixture [] = error "empty failure runtime fixture"
