{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, orElse, putTMVar, readTMVar)
import Control.Exception (IOException, SomeException, bracket, onException, throwIO, try)
import Control.Monad (forM_, forever, void)
import Data.ByteString qualified as Bytes
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as Text
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (ConnectionDescriptor, connectionDescriptorAttachmentBytes)
import Eclips.Deployment.Admin
import Eclips.Deployment.Configuration
import Eclips.Deployment.Discovery
import Eclips.Deployment.Manifest
import Eclips.Deployment.Runtime (withDeploymentResident)
import Eclips.Deployment.Timing (takeoverTargetMicroseconds)
import Eclips.Domain.Identity
import Eclips.Domain.Membership (HeraldMembershipGenerationId, heraldMembershipGenerationActiveHeraldEpochs, heraldMembershipGenerationId, heraldMembershipGenerationPredecessor, heraldMembershipGenerationRetiredHeraldEpoch, heraldMembershipGenerationRetirementControlIndex, heraldMembershipGenerationRetirementId)
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Herald.Join
import Eclips.Herald.OracleHealth (oracleHealthObservationConfiguration, oracleHealthObservationReference)
import Eclips.Oracle.Admission
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryValue)
import Eclips.Oracle.Failure qualified as Failure
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch, raftVoterBindingNode)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Configuration (RaftVotingConfiguration, genesisRaftConfigurationRef, jointRaftConfiguration, raftConfigurationEntryRef, raftVoterSet, raftVotingConfigurationNodes, stableRaftConfiguration)
import Eclips.Raft.Identity (RaftNodeId, mkRaftNodeId, raftLogIndex, raftNodeIdBytes, raftTerm)
import NativeHealth (awaitNativeConfiguration, readNativeHealthAt)
import Network.Socket
import ParentExit (runChildAfterParentExit, runParentBeforeClaim, runParentBeforeClaims)
import Partition (casePartitionConnectionScope, withPartitionProxies)
import Proxy (withReplyLossProxy)
import System.Directory hiding (executable)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO
import System.Process hiding (system)
import System.Timeout (timeout)
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))
import Text.Read (readMaybe)
import Timing (runTimingApplications)
import VoterApplication (runApplicationAcrossFence, runParentAcrossDemotion, runParentOnTarget)
import VoterCheckpoint

main :: IO ()
main =
  getArgs >>= \case
    ["--p04-parent-before-claim", parent, child] -> runParentBeforeClaim parent child
    ["--p04-child-after-parent-exit", child] -> runChildAfterParentExit child
    ["--p06-interrupted-applicant", seed, contact] -> runInterruptedApplicant seed contact
    ["--p06-prepare-launchers", parent, child1, child2] -> runParentBeforeClaims parent [child1, child2]
    ["--p10-prepare-launchers", parent, local, remote, demotion, replacement] -> runParentBeforeClaims parent [local, remote, demotion, replacement]
    ["--p10-control-history", endpointText, output] -> checked (parseEndpoint (Text.pack endpointText)) >>= \target -> writeRecentControlHistory target output
    ["--p08-parent-across-demotion", parent, resume, child] -> runParentAcrossDemotion parent resume child
    ["--p08-parent-on-promoted-herald", parent, target, child] -> runParentOnTarget parent target child
    ["--p09-application-across-fence", descriptor, fenced, healed] -> runApplicationAcrossFence descriptor fenced healed
    ["--p08-checkpoint-herald", manifest, ordinal, directory] -> runCheckpointHerald manifest ordinal directory
    ["--timing-applications", descriptor] -> runTimingApplications descriptor
    _ ->
      defaultMain
        -- Each case starts a complete OS deployment and temporarily releases its
        -- reserved ports before the children bind them. Keep these allocations
        -- and the final listener-rebinding checks in one serial ownership scope.
        $ localOption (NumThreads 1)
        $ testGroup
          "independent deployment and ordinary applications"
          ( [ testCase (order <> " / " <> residence) (tour OrdinaryTour readerFirst remote)
            | (order, readerFirst) <- [("publisher first", False), ("reader first", True)],
              (residence, remote) <- [("same Herald", False), ("different Heralds", True)]
            ]
              <> [ testCase "disconnect before the first child reply ends the child [p04-child-reply-loss]" (tour ChildReplyLossTour True True),
                   testCase "p04-failed-spawn: failed first OS spawn cancels both prepared children" (tour FailedSpawnTour False True),
                   testCase "p04-parent-end-before-claim: child operates after its parent process exits" caseParentExitBeforeClaim,
                   testCase "exceptional deployment scope releases Herald and Oracle listeners" caseScopeCleanup,
                   testCase "f01a-timing-policy: CLI target reaches startup, discovery and ordinary parent/child applications" caseTakeoverTiming,
                   testCase "partition proxy scope releases completed bridges" casePartitionConnectionScope,
                   testCase "p06-seed-chain: delayed founder and unknown Heralds activate before serving children" caseSeedOnlyTour,
                   testCase "p06-retirement-retry: interrupted applicant is cancelled by old-member retirement before a fresh epoch joins" caseRetirementJoinRetry,
                   testCase "fixture checkpoint coordination: completion suppresses late phase callbacks" caseCheckpointCompletionFirst,
                   testCase "fixture checkpoint coordination: owned firing survives final completion" caseCheckpointCallbackFirst,
                   testCase "fixture checkpoint coordination: prior growth cannot claim a demotion" caseCheckpointBinding,
                   testCase "p08-voter-growth-demotion: singleton grows and demoted founder serves existing and new applications" caseVoterGrowthDemotion,
                   testCase "p08-last-voter: last-voter removal is refused and an unknown cancellation retains its receipt" caseLastVoterRefusal,
                   testCase "p08-cancel-unavailable-learner: captured preparation cancels before entering joint configuration" caseVoterCancellation,
                   testCase "p08-seed-joined-voter: a dynamically admitted Herald becomes a voter and serves a new OS application" caseSeedJoinedVoter,
                   testCase "p10-combined-tour: applications, seed joins, voter replacement, failure exclusion and fresh readmission" caseCombinedTour,
                   testCase "p09-live-partition: fencing retains native participation and healing never reactivates the old epoch" caseLiveVoterPartition,
                   testCase "p09-lost-quorum: the last connected voter fences without shrinking native or semantic membership" caseLostVoterQuorum
                 ]
              <> [testCase ("p08-oracle-role-loss-" <> voterPhaseName phase <> ": surviving voters finish while the old cohost stays active") (caseVoterLeaderLoss phase) | phase <- [minBound .. maxBound]]
          )

-- Actual TCP proxies cut both directions of EPRP, EORC and ERFT. The target
-- process remains alive; direct EADM and native-owner observations distinguish
-- irreversible semantic fencing from a stopped cohost runtime.
caseLiveVoterPartition :: Assertion
caseLiveVoterPartition = withWorkspace $ \directory -> do
  completed <- timeout 240_000_000 $ do
    herald <- executable "eclips-herald"
    administrator <- executable "eclips-admin"
    application <- getExecutablePath
    (plan, residents) <- partitionVoterDeployment 3
    let manifest = directory </> "run.manifest"
        descriptor = directory </> "launcher.connection"
        system = deploymentSystemBytes plan
        members = map residentMember (NonEmpty.toList (deploymentResidents plan))
        hosts = map heraldMemberEpoch members
    Bytes.writeFile manifest (encodeDeployment plan)
    withPartitionProxies (zip hosts residents) $ \partitionHost awaitReconnection ->
      withFixedResidents herald directory manifest descriptor residents $ \processes ->
        ( case (residents, members, processes) of
            (first : otherPlanes@(observer : _), founderMember : otherMembers@(observerMember : _), founderProcess : _) -> do
              let founder = heraldMemberEpoch founderMember
                  otherHosts = map heraldMemberEpoch otherMembers
                  applicationLog = directory </> "partition-application.log"
                  fenced = directory </> "partition.fenced"
                  healed = directory </> "partition.healed"
              configuration <- commissionFixtureVoters directory administrator residents system hosts
              registrations <- awaitRegistrations first hosts
              awaitNativeConfiguration plan founderMember registrations configuration
              before <- awaitJoinStatus (peerListen first)
              node <- voterNodeFor configuration founder
              registration <- case find ((== node) . Voter.replicaRegistrationNode) registrations of
                Just value -> pure value
                Nothing -> assertFailure "partition target has a retained replica registration"
              withFile applicationLog WriteMode $ \output ->
                withScopedProcess (proc application ["--p09-application-across-fence", descriptor, fenced, healed]) {std_out = UseHandle output, std_err = UseHandle output} $ \attached -> do
                  awaitReady attached applicationLog
                  partitionHost founder node True
                  _ <- awaitIsolatedStatus first system
                  writeFile fenced "owner reported irreversible semantic fence\n"
                  awaitLogLine attached applicationLog "fenced"
                  awaitDirectNativeConfiguration plan founderMember first registration configuration
                  getProcessExitCode founderProcess >>= assertEqual "fenced cohost process remains alive" Nothing
                  excluded <- awaitStableVoters otherPlanes otherHosts
                  awaitNativeConfiguration plan observerMember registrations excluded
                  forM_ otherPlanes $ \planes -> void $ awaitJoinObservation (peerListen planes) $ \status ->
                    if sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (joinStatusMembership status))) == sort otherHosts then Just status else Nothing
                  assertVoterFailureOrder (peerListen observer) configuration excluded (heraldMembershipGenerationId (joinStatusMembership before)) founder
                  -- The fenced native owner cannot learn the new configuration
                  -- through a closed network; it still answers on its real port.
                  awaitDirectNativeConfiguration plan founderMember first registration configuration
                  partitionHost founder node False
                  -- The target can permanently miss final exclusion: surviving
                  -- replicas cease sending to it, and new EORC Hello admission
                  -- rejects its retired epoch. Observe a real reopened outbound
                  -- handshake, without demanding a newer target control view.
                  awaitReconnection founder node
                  _ <- awaitStableVoters otherPlanes otherHosts
                  _ <- awaitIsolatedStatus first system
                  writeFile healed "target handshake crossed healed TCP proxy\n"
                  waitForProcess attached >>= assertEqual "application observes irreversible unavailability across healing" ExitSuccess
                  _ <- awaitIsolatedStatus first system
                  getProcessExitCode founderProcess >>= assertEqual "healed fenced cohost process remains alive" Nothing
                  drainResidents system residents processes
            _ -> assertFailure "partition fixture has three resident members"
        )
          `onException` preserveVoterEvidence directory system residents
  assertBool "live all-protocol partition schedule finished" (completed == Just ())

-- Loss of two OS processes establishes missing native quorum independently of
-- the semantic peer mesh. Once the remaining owner's absolute grace expires,
-- no committed prefix or membership may be reduced to manufacture progress.
caseLostVoterQuorum :: Assertion
caseLostVoterQuorum = withWorkspace $ \directory -> do
  completed <- timeout 240_000_000 $ do
    herald <- executable "eclips-herald"
    administrator <- executable "eclips-admin"
    (plan, residents) <- voterDeployment 3
    let manifest = directory </> "run.manifest"
        descriptor = directory </> "launcher.connection"
        system = deploymentSystemBytes plan
        members = map residentMember (NonEmpty.toList (deploymentResidents plan))
        hosts = map heraldMemberEpoch members
    Bytes.writeFile manifest (encodeDeployment plan)
    withFixedResidents herald directory manifest descriptor residents $ \processes ->
      ( case (residents, processes, members) of
          (first : _, survivor : failed, member : _) -> do
            configuration <- commissionFixtureVoters directory administrator residents system hosts
            registrations <- awaitRegistrations first hosts
            awaitNativeConfiguration plan member registrations configuration
            before <- awaitOracleObservation (peerListen first) Just
            membership <- joinStatusMembership <$> awaitJoinStatus (peerListen first)
            node <- voterNodeFor configuration (heraldMemberEpoch member)
            registration <- case find ((== node) . Voter.replicaRegistrationNode) registrations of
              Just value -> pure value
              Nothing -> assertFailure "remaining voter has a retained replica registration"
            forM_ failed terminateProcess
            forM_ failed (void . waitForProcess)
            _ <- awaitIsolatedStatus first system
            after <- awaitOracleObservation (peerListen first) Just
            assertEqual "lost quorum preserves the complete committed Oracle status" before after
            currentMembership <- joinStatusMembership <$> awaitJoinStatus (peerListen first)
            assertEqual "lost quorum cannot invent semantic retirement" membership currentMembership
            awaitDirectNativeConfiguration plan member first registration configuration
            getProcessExitCode survivor >>= assertEqual "self-fenced native voter process remains alive without quorum" Nothing
            drainResidents system [first] [survivor]
          _ -> assertFailure "lost-quorum fixture has three resident processes"
      )
        `onException` preserveVoterEvidence directory system residents
  assertBool "lost native quorum schedule finished" (completed == Just ())

commissionFixtureVoters :: FilePath -> FilePath -> [DeploymentEndpoints] -> Bytes.ByteString -> [HeraldEpoch] -> IO Voter.VoterConfiguration
commissionFixtureVoters directory administrator residents system hosts = case (residents, hosts) of
  (first : _, founder : promoted) -> do
    initial <- awaitStableVoters residents [founder]
    forM_ (zip [9201 ..] residents) $ \(correlation, planes) -> voterMutation directory administrator planes system correlation ["prepare-oracle"] >>= assertAcceptedVoterReceipt
    registrations <- awaitRegistrations first promoted
    receipt <- voterMutation directory administrator first system 9210 (["commission-voters", configurationArgument initial] <> map (nodeArgument . Voter.replicaRegistrationNode) registrations)
    assertAcceptedVoterReceipt receipt
    configuration <- awaitStableVoters residents hosts
    assertCompletedChange residents receipt
    pure configuration
  _ -> assertFailure "voter fixture requires one founder"

awaitIsolatedStatus :: DeploymentEndpoints -> Bytes.ByteString -> IO Text.Text
awaitIsolatedStatus planes system = do
  rendered <- Text.concat <$> runAdministration (administrationListen planes) system 9290 InspectStatus
  if "phase AdminIsolated" `Text.isInfixOf` rendered then pure rendered else threadDelay 10_000 >> awaitIsolatedStatus planes system

awaitDirectNativeConfiguration :: CheckedDeployment -> HeraldMember -> DeploymentEndpoints -> Voter.OracleReplicaRegistration -> Voter.VoterConfiguration -> IO ()
awaitDirectNativeConfiguration plan member planes registration configuration = do
  actual <- checked (Voter.oracleReplicaEndpoint (TextEncoding.encodeUtf8 (endpointHost (oracleListen planes))) (endpointPort (oracleListen planes)))
  let await roundNumber = do
        observation <- readNativeHealthAt plan member roundNumber actual registration
        case observation of
          Just value
            | oracleHealthObservationConfiguration value == Voter.voterConfigurationNative configuration,
              oracleHealthObservationReference value == Voter.voterConfigurationNativeRef configuration ->
                pure ()
          _ -> threadDelay 10_000 >> await (roundNumber + 1)
  await 1

awaitLogLine :: ProcessHandle -> FilePath -> Text.Text -> IO ()
awaitLogLine process path line = do
  logged <- Text.lines <$> Text.readFile path
  if line `elem` logged
    then pure ()
    else
      getProcessExitCode process >>= \case
        Nothing -> threadDelay 10_000 >> awaitLogLine process path line
        Just code -> assertFailure ("fixture exited before " <> Text.unpack line <> ": " <> show code <> "\n" <> Text.unpack (Text.unlines logged))

drainResidents :: Bytes.ByteString -> [DeploymentEndpoints] -> [ProcessHandle] -> IO ()
drainResidents system residents processes = forM_ (zip residents processes) $ \(planes, process) -> do
  _ <- runAdministration (administrationListen planes) system 9299 Drain
  waitForProcess process >>= assertEqual "explicit drain joins the retained control shell" ExitSuccess

partitionVoterDeployment :: Int -> IO (CheckedDeployment, [DeploymentEndpoints])
partitionVoterDeployment count = do
  allocated <- freshEndpoints (8 * count)
  let planes [] = pure []
      planes (p : a : d : o : r : proxyP : proxyO : proxyR : rest) =
        (:) <$> checked (deploymentEndpointsFromPlanes p a d o r Nothing (Just proxyP) Nothing (Just proxyO) (Just proxyR)) <*> planes rest
      planes _ = fail "partition endpoint fixture shape"
  residents <- planes allocated
  case residents of
    first : rest -> (,residents) <$> checked (checkDeployment (Bytes.pack [11 .. 42]) (first :| rest))
    [] -> fail "partition deployment requires a founder"

-- Section 14's complete tour uses one fresh system and separate ordinary OS
-- applications throughout. Every fault follows a committed owner observation.
caseCombinedTour :: Assertion
caseCombinedTour = withWorkspace $ \directory -> do
  completed <- timeout 900_000_000 $ do
    herald <- executable "eclips-herald"
    administrator <- executable "eclips-admin"
    helper <- getExecutablePath
    launcher <- executable "eclips-hello-launcher"
    publisher <- executable "eclips-hello-publisher"
    reader <- executable "eclips-hello-reader"
    (_, allocated) <- voterDeployment 5
    let rootDescriptor = directory </> "launcher.connection"
        localDescriptor = directory </> "local-launcher.connection"
        remoteDescriptor = directory </> "remote-launcher.connection"
        demotionDescriptor = directory </> "demotion-parent.connection"
        replacementDescriptor = directory </> "replacement-parent.connection"
        runHello system residents descriptor pubTarget readTarget = do
          (code, output, diagnostic) <-
            runLauncherObserved
              (preserveLiveEvidence directory system residents)
              directory
              launcher
              ["--connection-file", descriptor, "--publisher-herald", Text.unpack (renderEndpoint (applicationListen pubTarget)), "--reader-herald", Text.unpack (renderEndpoint (applicationListen readTarget)), "--publisher-executable", publisher, "--reader-executable", reader]
          assertEqual ("combined tour application failed: " <> diagnostic) ExitSuccess code
          assertEqual "publisher and reader exchange all data through ECLIPS" "Hello, world!\n" output
        checkpoint message = putStrLn ("P10: " <> message) >> hFlush stdout
        joinMode seed = ["--peer", Text.unpack (renderEndpoint (peerListen seed))]
        hostAt = fmap (heraldMemberEpoch . joinStatusMember) . awaitJoinStatus . peerListen
    case allocated of
      [first, second, third, fourth, fifth] ->
        withSeedHerald herald directory 1 first ["--bootstrap", "--launcher-output", rootDescriptor] $ \process1 -> do
          awaitReady process1 (directory </> "herald-1.log")
          founderStatus <- awaitJoinStatus (peerListen first)
          let founderMember = joinStatusMember founderStatus
              founder = heraldMemberEpoch founderMember
              system = systemIdBytes (joinStatusSystem founderStatus)
          bootstrap <-
            exchangeDiscoveryRequest (peerListen first) (DiscoverSystem 9100 []) >>= \case
              Right (SystemDiscovered 9100 snapshot) -> pure (discoveryBootstrap snapshot)
              other -> assertFailure ("combined tour bootstrap discovery: " <> show other)
          (preparedCode, _, preparedDiagnostic) <- runLauncher directory helper ["--p10-prepare-launchers", rootDescriptor, localDescriptor, remoteDescriptor, demotionDescriptor, replacementDescriptor]
          assertEqual ("combined tour launcher preparation: " <> preparedDiagnostic) ExitSuccess preparedCode
          runHello system [first] localDescriptor first first
          checkpoint "founder application completed"
          withSeedHerald herald directory 2 second (joinMode first) $ \process2 -> do
            awaitReadyWithResidents [(process1, directory </> "herald-1.log")] process2 (directory </> "herald-2.log")
            withSeedHerald herald directory 3 third (joinMode second) $ \process3 -> do
              awaitReadyWithResidents [(process1, directory </> "herald-1.log"), (process2, directory </> "herald-2.log")] process3 (directory </> "herald-3.log")
              let firstThree = [first, second, third]
                  processes3 = [process1, process2, process3]
                  required processes = [(process, directory </> ("herald-" <> show ordinal <> ".log")) | (ordinal, process) <- zip [1 :: Int ..] processes]
              (hosts3, three, growth) <- withRequiredResidents (required processes3) $ do
                hosts3 <- traverse hostAt firstThree
                runHello system firstThree remoteDescriptor second third
                checkpoint "application across two seed joins completed"
                initial <- awaitStableVoters firstThree [founder]
                forM_ (zip [9102, 9103] [second, third]) $ \(correlation, planes) ->
                  voterMutation directory administrator planes system correlation ["prepare-oracle"] >>= assertAcceptedVoterReceipt
                promoted <- awaitRegistrations first (drop 1 hosts3)
                growth <- voterMutation directory administrator first system 9110 (["commission-voters", configurationArgument initial] <> map (nodeArgument . Voter.replicaRegistrationNode) promoted)
                assertAcceptedVoterReceipt growth
                three <- awaitStableVoters firstThree hosts3
                assertCompletedChange firstThree growth
                checkpoint "three-voter commission completed"
                pure (hosts3, three, growth)
              withSeedHerald herald directory 4 fourth (joinMode third) $ \process4 -> do
                awaitReadyWithResidents [(process1, directory </> "herald-1.log"), (process2, directory </> "herald-2.log"), (process3, directory </> "herald-3.log")] process4 (directory </> "herald-4.log")
                let residents4 = firstThree <> [fourth]
                    processes4 = [process1, process2, process3, process4]
                    parentLog = directory </> "combined-demotion-parent.log"
                    resume = directory </> "combined-demotion.applied"
                    child = directory </> "combined-demotion-child.connection"
                (hosts4, voters3, beforeFailure, failedHost, failedProcess, demotion, replacement) <- withRequiredResidents (required processes4) $ flip onException (preserveLiveEvidence directory system residents4) $ do
                  hosts4 <- traverse hostAt residents4
                  withFile parentLog WriteMode $ \output ->
                    withScopedProcess (proc helper ["--p08-parent-across-demotion", demotionDescriptor, resume, child]) {std_out = UseHandle output, std_err = UseHandle output} $ \parent -> do
                      awaitReadyWithResidents [(process, directory </> ("herald-" <> show ordinal <> ".log")) | (ordinal, process) <- zip [1 :: Int ..] processes4] parent parentLog
                      founderNode <- voterNodeFor three founder
                      demotion <- voterMutation directory administrator first system 9111 ["decommission-voter", configurationArgument three, nodeArgument founderNode]
                      assertAcceptedVoterReceipt demotion
                      two <- awaitStableVoters residents4 (drop 1 hosts3)
                      assertCompletedChange residents4 demotion
                      writeFile resume "all observers applied founder demotion\n"
                      parentExit <- waitForProcess parent
                      diagnostic <- Text.readFile parentLog
                      assertEqual ("attached application failed across demotion: " <> Text.unpack diagnostic) ExitSuccess parentExit
                      assertOrdinaryChild directory helper child
                      voterMutation directory administrator fourth system 9104 ["prepare-oracle"] >>= assertAcceptedVoterReceipt
                      fourthHost <- hostAt fourth
                      fourthRegistration <- awaitRegistrations first [fourthHost]
                      replacement <- voterMutation directory administrator first system 9112 (["commission-voters", configurationArgument two] <> map (nodeArgument . Voter.replicaRegistrationNode) fourthRegistration)
                      assertAcceptedVoterReceipt replacement
                      voters3 <- awaitStableVoters residents4 (drop 1 hosts4)
                      assertCompletedChange residents4 replacement
                      checkpoint "application across demotion and voter replacement completed"
                      beforeFailure <- awaitJoinStatus (peerListen first)
                      leader <- awaitVoterLeader residents4 system voters3
                      (failedHost, failedProcess) <- case [(host, process) | (host, process) <- zip hosts4 processes4, any (\binding -> raftVoterBindingNode binding == leader && raftVoterBindingHeraldEpoch binding == host) (Voter.voterConfigurationBindings voters3)] of
                        [value] -> pure value
                        _ -> assertFailure "current voter leader has one live host"
                      pure (hosts4, voters3, beforeFailure, failedHost, failedProcess, demotion, replacement)
                terminateProcess failedProcess
                _ <- waitForProcess failedProcess
                let survivors = [(planes, host, process) | (planes, host, process) <- zip3 residents4 hosts4 processes4, host /= failedHost]
                    survivorPlanes = [planes | (planes, _, _) <- survivors]
                    survivorHosts = [host | (_, host, _) <- survivors]
                    remainingVoters = filter (/= failedHost) (drop 1 hosts4)
                    requiredSurvivors = [(process, directory </> ("herald-" <> show ordinal <> ".log")) | (ordinal, (host, process)) <- zip [1 :: Int ..] (zip hosts4 processes4), host /= failedHost]
                excluded <- withRequiredResidents requiredSurvivors $ do
                  excluded <- awaitStableVoters survivorPlanes remainingVoters
                  registrations <- heraldOracleStatusReplicas <$> awaitOracleObservation (peerListen first) Just
                  awaitNativeConfiguration bootstrap founderMember registrations excluded
                  forM_ survivorPlanes $ \planes -> void $ awaitJoinObservation (peerListen planes) $ \status ->
                    if sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (joinStatusMembership status))) == sort survivorHosts then Just status else Nothing
                  assertVoterFailureOrder (peerListen first) voters3 excluded (heraldMembershipGenerationId (joinStatusMembership beforeFailure)) failedHost
                  checkpoint "failed voter leader excluded and retired"
                  pure excluded
                withSeedHerald herald directory 5 fifth (joinMode first) $ \process5 -> do
                  awaitReadyWithResidents requiredSurvivors process5 (directory </> "herald-5.log")
                    `onException` preserveLiveEvidence directory system (residents4 <> [fifth])
                  withRequiredResidents (requiredSurvivors <> [(process5, directory </> "herald-5.log")]) $ do
                    fresh <- hostAt fifth
                    assertBool "replacement joins with a fresh epoch" (fresh `notElem` hosts4)
                    let live = survivorPlanes <> [fifth]
                    voterMutation directory administrator fifth system 9105 ["prepare-oracle"] >>= assertAcceptedVoterReceipt
                    freshRegistration <- awaitRegistrations first [fresh]
                    readmission <- voterMutation directory administrator first system 9113 (["commission-voters", configurationArgument excluded] <> map (nodeArgument . Voter.replicaRegistrationNode) freshRegistration)
                    assertAcceptedVoterReceipt readmission
                    final <- awaitStableVoters live (remainingVoters <> [fresh])
                    assertCompletedChange live readmission
                    finalRegistrations <- heraldOracleStatusReplicas <$> awaitOracleObservation (peerListen first) Just
                    awaitNativeConfiguration bootstrap founderMember finalRegistrations final
                    let freshChild = directory </> "fresh-replacement-child.connection"
                    (parentCode, _, freshDiagnostic) <- runLauncher directory helper ["--p08-parent-on-promoted-herald", replacementDescriptor, Text.unpack (renderEndpoint (applicationListen fifth)), freshChild]
                    assertEqual ("fresh replacement child preparation: " <> freshDiagnostic) ExitSuccess parentCode
                    assertOrdinaryChild directory helper freshChild
                    assertClosedVoterReceiptLifetime administrator first system 9110 growth
                    assertClosedVoterReceiptLifetime administrator first system 9111 demotion
                    assertClosedVoterReceiptLifetime administrator first system 9112 replacement
                    checkpoint "fresh-epoch readmission and application completed"
                  forM_ ([(planes, process) | (planes, _, process) <- survivors] <> [(fifth, process5)]) $ \(planes, process) -> do
                    _ <- runAdministration (administrationListen planes) system 9199 Drain
                    waitForProcess process >>= assertEqual "combined tour explicitly drains and joins each surviving Herald" ExitSuccess
      _ -> fail "combined tour requires five endpoint bundles"
  assertBool "complete profile-0.2 process tour finished" (completed == Just ())

awaitRegistrations :: DeploymentEndpoints -> [HeraldEpoch] -> IO [Voter.OracleReplicaRegistration]
awaitRegistrations observer hosts = awaitOracleObservation (peerListen observer) $ \status ->
  traverse (\host -> find ((== host) . Voter.replicaRegistrationHost) (heraldOracleStatusReplicas status)) hosts

voterNodeFor :: Voter.VoterConfiguration -> HeraldEpoch -> IO RaftNodeId
voterNodeFor configuration host = case [raftVoterBindingNode binding | binding <- Voter.voterConfigurationBindings configuration, raftVoterBindingHeraldEpoch binding == host] of
  [node] -> pure node
  _ -> assertFailure "one exact current voter binding required"

assertOrdinaryChild :: FilePath -> FilePath -> FilePath -> Assertion
assertOrdinaryChild directory helper child = do
  (code, output, diagnostic) <- runLauncher directory helper ["--p04-child-after-parent-exit", child]
  assertEqual ("ordinary child failed: " <> diagnostic) ExitSuccess code
  assertEqual "prepared child claims, operates and ends" "child claimed, operated and ended after parent exit\n" output

assertVoterFailureOrder :: Endpoint -> Voter.VoterConfiguration -> Voter.VoterConfiguration -> HeraldMembershipGenerationId -> HeraldEpoch -> Assertion
assertVoterFailureOrder observer previous excluded membership target = do
  status <- awaitJoinStatus observer
  let generation = joinStatusMembership status
  assertEqual "semantic retirement names the failed Herald" (Just target) (heraldMembershipGenerationRetiredHeraldEpoch generation)
  assertEqual "semantic retirement follows the captured membership generation" (Just membership) (heraldMembershipGenerationPredecessor generation)
  (retirement, resolution) <- case (heraldMembershipGenerationRetirementControlIndex generation, heraldMembershipGenerationRetirementId generation) of
    (Just index, Just identifier) -> pure (index, identifier)
    other -> assertFailure ("expected retained semantic retirement evidence: " <> show other)
  certificate <-
    restrictedJoin observer (ReadOracleVoterHostFailure resolution) >>= \case
      OracleVoterHostFailureReply (Just accepted) -> pure accepted
      other -> assertFailure ("retained failure certificate query: " <> show other)
  let acceptance = Failure.acceptedVoterHostFailureControlIndex certificate
  assertEqual "the retained failure certificate names the failed Herald" target (Failure.acceptedVoterHostFailureTarget certificate)
  assertEqual "semantic retirement refers to the original failure resolution" resolution (Failure.acceptedVoterHostFailureResolution certificate)
  assertEqual "failure retains the exact pre-failure voter denominator" previous (Failure.acceptedVoterHostFailureConfiguration certificate)
  assertEqual "failure retains the exact semantic membership generation" membership (Failure.acceptedVoterHostFailureMembership certificate)
  assertBool "the failed voter remains in the captured denominator" (target `elem` map raftVoterBindingHeraldEpoch (Voter.voterConfigurationBindings previous))
  assertBool "native exclusion follows accepted evidence and precedes semantic retirement" (acceptance < Voter.voterConfigurationControlIndex excluded && Voter.voterConfigurationControlIndex excluded < retirement)
  reply <- restrictedJoin observer (ReadOracleVoterChange (Failure.acceptedVoterHostFailureChangeId certificate))
  case reply of
    OracleVoterChangeReply (Just change) -> do
      assertEqual "accepted failure links to its original retained voter intent" (Failure.acceptedVoterHostFailureChangeId certificate) (Voter.voterChangeId change)
      assertEqual "the voter intent retains its accepted failure resolution" (Voter.AcceptedHostFailureReason resolution) (Voter.voterChangeReason change)
      assertEqual "the voter intent retains its expected configuration" (Voter.voterConfigurationId previous) (Voter.voterChangeExpectedConfiguration change)
      assertEqual "the voter intent retains the original bindings" (Voter.voterConfigurationBindings previous) (Voter.oracleVoterBindingList (Voter.voterChangeOldBindings change))
      assertEqual "the voter intent excludes exactly the failed host" (filter ((/= target) . raftVoterBindingHeraldEpoch) (Voter.voterConfigurationBindings previous)) (Voter.oracleVoterBindingList (Voter.voterChangeNewBindings change))
      assertEqual "accepted failure links to a completed retained voter change" Voter.VoterChangeCompleted (Voter.voterChangePhase change)
      assertEqual "semantic retirement completes the original voter intent" retirement (Voter.voterChangeControlIndex change)
    other -> assertFailure ("accepted failure voter change: " <> show other)

-- Start H2 before its sole useful seed exists, then grow H3 through H2.
-- Every process receives only endpoint hints and generates its own identity.
-- Two complete application workflows with transferred launcher roots take about
-- eleven minutes; retain a finite fifteen-minute bound for this larger tour.
caseSeedOnlyTour :: Assertion
caseSeedOnlyTour = withWorkspace $ \directory -> do
  completed <- timeout 900_000_000 $ do
    herald <- executable "eclips-herald"
    helper <- getExecutablePath
    launcher <- executable "eclips-hello-launcher"
    publisher <- executable "eclips-hello-publisher"
    reader <- executable "eclips-hello-reader"
    allocated <- freshEndpoints 16
    (first, second, third, unavailable) <- case allocated of
      [p1, a1, d1, o1, r1, p2, a2, d2, o2, r2, p3, a3, d3, o3, r3, dead] -> do
        make <- pure (\p a d o r -> checked (deploymentEndpointsFromPlanes p a d o r Nothing Nothing Nothing Nothing Nothing))
        (,,,) <$> make p1 a1 d1 o1 r1 <*> make p2 a2 d2 o2 r2 <*> make p3 a3 d3 o3 r3 <*> pure dead
      _ -> fail "seed tour endpoint fixture shape"
    let descriptor = directory </> "launcher.connection"
        seed value = ["--peer", Text.unpack (renderEndpoint value)]
        firstMode = ["--bootstrap", "--launcher-output", descriptor]
        secondMode = seed (peerListen first) <> seed (peerListen first) <> seed unavailable
        thirdMode = seed (peerListen second)
    withSeedHerald herald directory 2 second secondMode $ \secondProcess -> do
      threadDelay 150_000
      early <- Text.readFile (directory </> "herald-2.log")
      assertBool "unreachable seed does not create a second genesis or report readiness" (not ("ready\n" `Text.isInfixOf` early))
      assertEqual "newcomer remains alive to retry its seed" Nothing =<< getProcessExitCode secondProcess
      withSeedHerald herald directory 1 first firstMode $ \firstProcess -> do
        awaitReady firstProcess (directory </> "herald-1.log")
        awaitReady secondProcess (directory </> "herald-2.log")
          `onException` preserveJoinEvidence directory (peerListen first)
        withSeedHerald herald directory 3 third thirdMode $ \thirdProcess -> do
          awaitReady thirdProcess (directory </> "herald-3.log")
            `onException` preserveJoinEvidence directory (peerListen first)
          logs <- traverse (Text.readFile . (directory </>)) ["herald-1.log", "herald-2.log", "herald-3.log"]
          systems <- traverse (checked . systemFromLog) logs
          system <- case systems of
            [a, b, c] | a == b && b == c -> pure a
            _ -> assertFailure "seed-only residents disagreed on system identity"
          snapshots <-
            traverse
              ( \planes -> do
                  response <- exchangeDiscoveryRequest (peerListen planes) (DiscoverSystem 731 [])
                  case response of
                    Right (SystemDiscovered 731 snapshot) -> pure snapshot
                    other -> assertFailure ("activated Herald discovery failed: " <> show other)
              )
              [first, second, third]
          case snapshots of
            a : b : c : [] -> do
              assertEqual "H2 retains the exact founder bootstrap" (encodeDeployment (discoveryBootstrap a)) (encodeDeployment (discoveryBootstrap b))
              assertEqual "H3 retains the exact founder bootstrap" (encodeDeployment (discoveryBootstrap a)) (encodeDeployment (discoveryBootstrap c))
            _ -> assertFailure "seed tour snapshot fixture shape"
          let firstLauncher = directory </> "first-launcher.connection"
              secondLauncher = directory </> "second-launcher.connection"
          (parentCode, _, parentDiagnostic) <-
            runLauncher directory helper ["--p06-prepare-launchers", descriptor, firstLauncher, secondLauncher]
          assertEqual ("preparing distinct launcher processes failed: " <> parentDiagnostic) ExitSuccess parentCode
          mapM_
            ( \(launcherDescriptor, readerFirst, readerTarget) -> do
                let arguments = ["--connection-file", launcherDescriptor, "--publisher-herald", Text.unpack (renderEndpoint (applicationListen second)), "--reader-herald", Text.unpack (renderEndpoint readerTarget), "--publisher-executable", publisher, "--reader-executable", reader] <> ["--reader-first" | readerFirst]
                (code, output, diagnostic) <-
                  runLauncherObserved
                    (preserveLiveEvidence directory system [first, second, third])
                    directory
                    launcher
                    arguments
                assertEqual ("children on joined Heralds failed (reader first=" <> show readerFirst <> ", reader=" <> Text.unpack (renderEndpoint readerTarget) <> "): " <> diagnostic) ExitSuccess code
                assertEqual "joined members exchange data through ECLIPS" "Hello, world!\n" output
            )
            [(firstLauncher, False, applicationListen second), (secondLauncher, True, applicationListen third)]
          _ <- runAdministration (administrationListen third) system 181 Drain
          assertHeraldExit directory 3 "H3 drained" thirdProcess
          _ <- runAdministration (administrationListen second) system 182 Drain
          assertHeraldExit directory 2 "H2 drained" secondProcess
          _ <- runAdministration (administrationListen first) system 183 Drain
          assertHeraldExit directory 1 "founder and Oracle drained" firstProcess
  case completed of
    Just () -> pure ()
    Nothing -> do
      logs <-
        traverse
          ( \name -> do
              let path = directory </> name
              exists <- doesFileExist path
              if exists then Text.readFile path else pure (Text.pack name <> ": not started")
          )
          ["herald-1.log", "herald-2.log", "herald-3.log"]
      assertFailure ("seed-only OS tour timed out\n" <> Text.unpack (Text.intercalate "\n" logs))
  where
    systemFromLog output = case [value | line <- Text.lines output, Just value <- [Text.stripPrefix "system " line]] of
      [value] -> decodeIdentity value
      _ -> Left "expected one system identity after activated startup"

withSeedHerald :: FilePath -> FilePath -> Int -> DeploymentEndpoints -> [String] -> (ProcessHandle -> IO value) -> IO value
withSeedHerald path directory ordinal planes mode use =
  withFile (directory </> ("herald-" <> show ordinal <> ".log")) WriteMode $ \logHandle ->
    let explicit = concat [["--" <> name <> "-listen", Text.unpack (renderEndpoint (projection planes))] | (name, projection) <- [("peer", peerListen), ("application", applicationListen), ("administration", administrationListen), ("oracle", oracleListen), ("raft", raftListen)]]
     in withScopedProcess (proc path (mode <> ["--bind", "127.0.0.1", "--base-port", "0"] <> explicit)) {std_out = UseHandle logHandle, std_err = UseHandle logHandle} use

-- A separate OS applicant stops after its exact admission offer commits. The
-- test kills that process, then retires an old member through real EADM/EORC.
-- A later CLI process must replay both that admission and the retirement.
caseRetirementJoinRetry :: Assertion
caseRetirementJoinRetry = withWorkspace $ \directory -> do
  completed <- timeout 240_000_000 $ do
    herald <- executable "eclips-herald"
    helper <- getExecutablePath
    launcher <- executable "eclips-hello-launcher"
    publisher <- executable "eclips-hello-publisher"
    reader <- executable "eclips-hello-reader"
    allocated <- freshEndpoints 15
    (first, second, third) <- case allocated of
      [p1, a1, d1, o1, r1, p2, a2, d2, o2, r2, p3, a3, d3, o3, r3] -> do
        let make p a d o r = checked (deploymentEndpointsFromPlanes p a d o r Nothing Nothing Nothing Nothing Nothing)
        (,,) <$> make p1 a1 d1 o1 r1 <*> make p2 a2 d2 o2 r2 <*> make p3 a3 d3 o3 r3
      _ -> fail "retirement join endpoint fixture shape"
    let descriptor = directory </> "launcher.connection"
        seed target = ["--peer", Text.unpack (renderEndpoint target)]
    withSeedHerald herald directory 1 first ["--bootstrap", "--launcher-output", descriptor] $ \firstProcess -> do
      awaitReady firstProcess (directory </> "herald-1.log")
      founder <- awaitJoinStatus (peerListen first)
      withSeedHerald herald directory 2 second (seed (peerListen first)) $ \secondProcess -> do
        awaitReady secondProcess (directory </> "herald-2.log")
          `onException` preserveJoinEvidence directory (peerListen first)
        oldMember <- heraldMemberEpoch . joinStatusMember <$> awaitJoinStatus (peerListen second)
        let applicantLog = directory </> "interrupted-applicant.log"
            applicantArguments = ["--p06-interrupted-applicant", Text.unpack (renderEndpoint (peerListen first)), Text.unpack (renderEndpoint (peerListen third))]
        withFile applicantLog WriteMode $ \logHandle ->
          withScopedProcess (proc helper applicantArguments) {std_out = UseHandle logHandle, std_err = UseHandle logHandle} $ \applicant -> do
            awaitReady applicant applicantLog
            pending <- awaitAdmissionPhase (peerListen first) interruptedEpoch (\case AdmissionPreparing -> True; _ -> False)
            assertBool "pending applicant has no active role" (interruptedEpoch `notElem` heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor pending))
            terminateProcess applicant
            _ <- waitForProcess applicant
            _ <- runAdministration (administrationListen second) (systemIdBytes (joinStatusSystem founder)) 281 Drain
            assertHeraldExit directory 2 "old nonvoter drained while join pending" secondProcess
            -- Failure-threshold acceptance cancels the applicant before the
            -- separate retirement command changes membership. Observe both
            -- committed facts together before starting the fresh admission.
            let retiredAdmission status = do
                  record <- find ((== interruptedEpoch) . admissionManifestHeraldEpoch . admissionRecordManifest) (joinStatusAdmissions status)
                  case admissionRecordPhase record of
                    AdmissionCancelled {}
                      | oldMember `notElem` heraldMembershipGenerationActiveHeraldEpochs (joinStatusMembership status) -> Just status
                    _ -> Nothing
            survivor <-
              awaitJoinObservation (peerListen first) retiredAdmission
                `onException` preserveJoinEvidence directory (peerListen first)
            assertBool "retired member is absent from current membership" (oldMember `notElem` heraldMembershipGenerationActiveHeraldEpochs (joinStatusMembership survivor))
        withSeedHerald herald directory 3 third (seed (peerListen first)) $ \thirdProcess -> do
          awaitReady thirdProcess (directory </> "herald-3.log")
            `onException` preserveJoinEvidence directory (peerListen first)
          fresh <- awaitJoinStatus (peerListen third)
          assertBool "fresh process has a different epoch from the cancelled applicant" (heraldMemberEpoch (joinStatusMember fresh) /= interruptedEpoch)
          assertEqual "fresh process joined the existing system" (joinStatusSystem founder) (joinStatusSystem fresh)
          let arguments = ["--connection-file", descriptor, "--publisher-herald", Text.unpack (renderEndpoint (applicationListen third)), "--reader-herald", Text.unpack (renderEndpoint (applicationListen first)), "--publisher-executable", publisher, "--reader-executable", reader]
          (code, output, diagnostic) <-
            runLauncherObserved
              (preserveLiveEvidence directory (systemIdBytes (joinStatusSystem founder)) [first, third])
              directory
              launcher
              arguments
          assertEqual ("children after cancelled join and retirement failed: " <> diagnostic) ExitSuccess code
          assertEqual "fresh admission serves ordinary cross-Herald data" "Hello, world!\n" output
          _ <- runAdministration (administrationListen third) (systemIdBytes (joinStatusSystem founder)) 282 Drain
          assertHeraldExit directory 3 "fresh admitted Herald drained" thirdProcess
      _ <- runAdministration (administrationListen first) (systemIdBytes (joinStatusSystem founder)) 283 Drain
      assertHeraldExit directory 1 "surviving founder drained" firstProcess
  case completed of
    Just () -> pure ()
    Nothing -> do
      names <- listDirectory directory
      logs <- traverse (Text.readFile . (directory </>)) (filter (\name -> ".log" `Text.isSuffixOf` Text.pack name) names)
      assertFailure ("retirement join retry timed out\n" <> Text.unpack (Text.intercalate "\n" logs))

interruptedEpoch :: HeraldEpoch
interruptedEpoch = either (error . show) id (mkHeraldEpoch (Bytes.replicate 32 197))

runInterruptedApplicant :: String -> String -> IO ()
runInterruptedApplicant seedText contactText = do
  hSetBuffering stdout LineBuffering
  seed <- checked (parseEndpoint (Text.pack seedText))
  locator <- checked (parseEndpoint (Text.pack contactText))
  ident <- checked (mkHeraldId (Bytes.replicate 32 196))
  contact <- checked (discoveryContact (HeraldMember ident interruptedEpoch) (locator :| []))
  _ <- discoverSystem 91 [contact] (seed :| [])
  let offer = do
        status <- awaitJoinStatus seed
        case joinStatusCut status of
          Nothing -> threadDelay 10_000 >> offer
          Just cut -> do
            let manifest = heraldAdmissionManifest (joinStatusSystem status) ident interruptedEpoch
                token = joinRequestId interruptedEpoch 0
                command = SubmitJoinCommand token (BeginHeraldAdmission manifest cut)
                await request =
                  restrictedJoin seed request >>= \case
                    JoinCommandPending -> threadDelay 10_000 >> await (ReadJoinCommand token)
                    JoinCommandComplete [_] -> pure ()
                    JoinRetry -> threadDelay 10_000 >> await command
                    other -> fail ("interrupted applicant offer failed: " <> show other)
            await command
  offer
  putStrLn "ready"
  let waitForInterruption = threadDelay 1_000_000 >> waitForInterruption
  waitForInterruption

restrictedJoin :: Endpoint -> JoinRequest -> IO JoinReply
restrictedJoin target request = do
  response <- exchangeDiscoveryRequest target (Onboard 92 (encodeJoinRequest request))
  case response of
    Right (OnboardReply 92 bytes) -> checked (decodeJoinReply bytes)
    _ -> threadDelay 10_000 >> restrictedJoin target request

awaitJoinStatus :: Endpoint -> IO JoinStatus
awaitJoinStatus target =
  restrictedJoin target ReadJoinStatus >>= \case
    JoinStatusReply status -> pure status
    _ -> threadDelay 10_000 >> awaitJoinStatus target

awaitAdmissionPhase :: Endpoint -> HeraldEpoch -> (HeraldAdmissionPhase -> Bool) -> IO HeraldAdmissionRecord
awaitAdmissionPhase target epoch accepted =
  awaitJoinObservation target $ \status -> do
    record <- find ((== epoch) . admissionManifestHeraldEpoch . admissionRecordManifest) (joinStatusAdmissions status)
    if accepted (admissionRecordPhase record) then Just record else Nothing

awaitJoinObservation :: Endpoint -> (JoinStatus -> Maybe value) -> IO value
awaitJoinObservation target observe = do
  status <- awaitJoinStatus target
  case observe status of
    Just value -> pure value
    Nothing -> threadDelay 10_000 >> awaitJoinObservation target observe

-- Keep the exact public inputs needed to reproduce a failed observer replay.
-- Successful tours leave no artifacts; diagnostic IO never prolongs a failure
-- beyond one physical discovery attempt per retained admission.
preserveJoinEvidence :: FilePath -> Endpoint -> IO ()
preserveJoinEvidence directory target = do
  discovery <- exchangeDiscoveryRequest target (DiscoverSystem 94 [])
  case discovery of
    Right (SystemDiscovered 94 snapshot) -> Bytes.writeFile (directory </> "join-bootstrap.bin") (encodeDeployment (discoveryBootstrap snapshot))
    _ -> pure ()
  reply <- exchangeDiscoveryRequest target (Onboard 95 (encodeJoinRequest ReadJoinStatus))
  case reply of
    Right (OnboardReply 95 bytes) -> do
      Bytes.writeFile (directory </> "join-status.bin") bytes
      case decodeJoinReply bytes of
        Right (JoinStatusReply status) -> mapM_ (captureEvidence status) (zip [1 :: Int ..] (joinStatusAdmissions status))
        _ -> pure ()
    _ -> pure ()
  where
    captureEvidence _ (ordinal, record) = do
      Bytes.writeFile (directory </> ("join-admission-" <> show ordinal <> ".bin")) (encodeAdmissionRecord record)
      response <- exchangeDiscoveryRequest target (Onboard 96 (encodeJoinRequest (CaptureJoinHistory (admissionRecordId record))))
      case response of
        Right (OnboardReply 96 bytes) -> case decodeJoinReply bytes of
          Right (JoinHistoryReply history) -> Bytes.writeFile (directory </> ("join-history-" <> show ordinal <> ".bin")) history
          _ -> pure ()
        _ -> pure ()

preserveLiveEvidence :: FilePath -> Bytes.ByteString -> [DeploymentEndpoints] -> IO ()
preserveLiveEvidence directory system residents = forM_ (zip [1 :: Int ..] residents) $ \(ordinal, planes) -> do
  let stem = directory </> ("herald-" <> show ordinal)
  writeEvidence (stem <> ".status") (runAdministration (administrationListen planes) system 771 InspectStatus)
  writeEvidence (stem <> ".preparations") (runAdministration (administrationListen planes) system 772 InspectPreparations)
  writeEvidence (stem <> ".join-status") (restrictedJoin (peerListen planes) ReadJoinStatus)
  writeRecentControlHistory (peerListen planes) (stem <> ".control-history")

-- Read a bounded recent suffix when the full prefix has been reclaimed. Both
-- attempts share one deadline; unavailable history is normal diagnostic output.
-- Artifacts contain immutable receipts/events, never owner-state dumps.
writeRecentControlHistory :: Endpoint -> FilePath -> IO ()
writeRecentControlHistory target path = do
  outcome <- timeout 2_000_000 (try @IOException readHistory)
  withFile path WriteMode $ \output -> case outcome of
    Nothing -> hPutStrLn output "control history query timed out after 2 seconds"
    Just (Left problem) -> hPutStrLn output ("control history query failed: " <> show problem)
    Just (Right (note, JoinControlHistoryReply entries)) -> do
      let count = length entries
          recent = drop (max 0 (count - 30)) entries
      hPutStrLn output note
      hPutStrLn output ("retained entries: " <> show count <> "; showing latest " <> show (length recent))
      forM_ recent (hPrint output . canonicalAppliedOracleEntryValue)
    Just (Right (note, JoinRejected)) -> hPutStrLn output (note <> "; requested control history is unavailable after reclamation")
    Just (Right (note, other)) -> hPutStrLn output (note <> "; unexpected control history reply: " <> take 512 (show other))
  where
    readHistory =
      restrictedJoin target (ReadJoinControlHistory (controlIndex 0)) >>= \case
        JoinRejected ->
          restrictedJoin target ReadJoinStatus >>= \case
            JoinStatusReply status -> do
              let applied = controlIndexWord64 (joinStatusAppliedControl status)
                  after = controlIndex (if applied > 30 then applied - 30 else 0)
                  note = "full prefix unavailable; observed cursor " <> show (joinStatusControl status) <> "; applied cursor " <> show (joinStatusAppliedControl status) <> "; suffix after " <> show after
              reply <- restrictedJoin target (ReadJoinControlHistory after)
              pure (note, reply)
            other -> pure ("full prefix unavailable; status query returned " <> take 512 (show other), JoinRejected)
        reply -> pure ("full retained prefix", reply)

writeEvidence :: (Show value) => FilePath -> IO value -> IO ()
writeEvidence path action = timeout 1_000_000 (try @IOException action) >>= writeFile path . show

-- Every replica endpoint is concrete before prepare-oracle. Herald membership
-- is fixed for this schedule; only the committed Oracle voter role changes.
caseVoterGrowthDemotion :: Assertion
caseVoterGrowthDemotion = withWorkspace $ \directory -> do
  completed <- timeout 240_000_000 $ do
    herald <- executable "eclips-herald"
    administrator <- executable "eclips-admin"
    application <- getExecutablePath
    (plan, residents) <- voterDeployment 4
    let manifest = directory </> "run.manifest"
        descriptor = directory </> "launcher.connection"
        system = deploymentSystemBytes plan
    Bytes.writeFile manifest (encodeDeployment plan)
    withFixedResidents herald directory manifest descriptor residents $ \_ ->
      ( case residents of
          [first, second, third, fourth] -> do
            hosts <- traverse (fmap (heraldMemberEpoch . joinStatusMember) . awaitJoinStatus . peerListen) residents
            (founder, promoted, replacement) <- case hosts of
              a : b : c : d : [] -> pure (a, [b, c], d)
              _ -> fail "voter host fixture shape"
            initial <- awaitStableVoters residents [founder]
            let parentLog = directory </> "voter-parent.log"
                resume = directory </> "demotion.applied"
                child = directory </> "child.connection"
            withFile parentLog WriteMode $ \output ->
              withScopedProcess (proc application ["--p08-parent-across-demotion", descriptor, resume, child]) {std_out = UseHandle output, std_err = UseHandle output} $ \parent -> do
                awaitReady parent parentLog
                forM_ (zip [402, 403] [second, third]) $ \(correlation, planes) -> do
                  receipt <- voterMutation directory administrator planes system correlation ["prepare-oracle"]
                  assertAcceptedVoterReceipt receipt
                registrations <- awaitOracleObservation (peerListen first) $ \status ->
                  traverse (\host -> find ((== host) . Voter.replicaRegistrationHost) (heraldOracleStatusReplicas status)) promoted
                let promotedNodes = map Voter.replicaRegistrationNode registrations
                growthReceipt <-
                  voterMutation
                    directory
                    administrator
                    first
                    system
                    410
                    (["commission-voters", configurationArgument initial] <> map nodeArgument promotedNodes)
                assertAcceptedVoterReceipt growthReceipt
                three <- awaitStableVoters residents (founder : promoted)
                assertCompletedChange residents growthReceipt
                assertClosedVoterReceiptLifetime administrator first system 410 growthReceipt
                prepared <- voterMutation directory administrator fourth system 404 ["prepare-oracle"]
                assertAcceptedVoterReceipt prepared
                replacementRegistration <- awaitOracleObservation (peerListen first) $ \status ->
                  find ((== replacement) . Voter.replicaRegistrationHost) (heraldOracleStatusReplicas status)
                replacementReceipt <-
                  voterMutation
                    directory
                    administrator
                    first
                    system
                    411
                    ["commission-voters", configurationArgument three, nodeArgument (Voter.replicaRegistrationNode replacementRegistration)]
                assertAcceptedVoterReceipt replacementReceipt
                four <- awaitStableVoters residents hosts
                assertCompletedChange residents replacementReceipt
                founderNode <- case [raftVoterBindingNode binding | binding <- Voter.voterConfigurationBindings four, raftVoterBindingHeraldEpoch binding == founder] of
                  [node] -> pure node
                  _ -> assertFailure "founder has exactly one voter binding"
                demotionReceipt <-
                  voterMutation
                    directory
                    administrator
                    first
                    system
                    412
                    ["decommission-voter", configurationArgument four, nodeArgument founderNode]
                assertAcceptedVoterReceipt demotionReceipt
                _ <- awaitStableVoters residents (promoted <> [replacement])
                assertCompletedChange residents demotionReceipt
                assertClosedVoterReceiptLifetime administrator first system 412 demotionReceipt
                finalMembership <- awaitJoinStatus (peerListen first)
                assertEqual "demotion preserves the founder Herald epoch" founder (heraldMemberEpoch (joinStatusMember finalMembership))
                assertEqual
                  "voter demotion preserves every active Herald"
                  (sort hosts)
                  (sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (joinStatusMembership finalMembership))))
                writeFile resume "committed stable configuration applied at every Herald\n"
                parentCode <- waitForProcess parent
                parentDiagnostic <- Text.readFile parentLog
                assertEqual ("existing application failed after demotion:\n" <> Text.unpack parentDiagnostic) ExitSuccess parentCode
                assertBool
                  "the existing attachment completed post-demotion work"
                  ("existing application operated and ended after demotion" `Text.isInfixOf` parentDiagnostic)
                (childCode, childOutput, childDiagnostic) <- runLauncher directory application ["--p04-child-after-parent-exit", child]
                assertEqual ("new application could not attach to demoted Herald: " <> childDiagnostic) ExitSuccess childCode
                assertEqual
                  "the newly prepared OS child performs ordinary calls and ordered End"
                  "child claimed, operated and ended after parent exit\n"
                  childOutput
                assertClosedVoterReceiptLifetime administrator first system 410 growthReceipt
                assertClosedVoterReceiptLifetime administrator first system 411 replacementReceipt
          _ -> fail "four-resident voter fixture shape"
      )
        `onException` preserveVoterEvidence directory system residents
  assertBool "voter growth and application service schedule completed" (completed == Just ())

caseLastVoterRefusal :: Assertion
caseLastVoterRefusal = withWorkspace $ \directory -> do
  completed <- timeout 120_000_000 $ do
    herald <- executable "eclips-herald"
    administrator <- executable "eclips-admin"
    (plan, residents) <- voterDeployment 1
    let manifest = directory </> "run.manifest"
        descriptor = directory </> "launcher.connection"
        system = deploymentSystemBytes plan
    Bytes.writeFile manifest (encodeDeployment plan)
    withFixedResidents herald directory manifest descriptor residents $ \_ -> case residents of
      [first] -> do
        founder <- heraldMemberEpoch . joinStatusMember <$> awaitJoinStatus (peerListen first)
        initial <- awaitStableVoters residents [founder]
        node <- case Voter.voterConfigurationBindings initial of
          [binding] -> pure (raftVoterBindingNode binding)
          _ -> assertFailure "singleton initial voter configuration"
        before <- awaitOracleObservation (peerListen first) Just
        (refusalCode, _, refusalDiagnostic) <-
          readCreateProcessWithExitCode
            (proc administrator ["--endpoint", Text.unpack (renderEndpoint (administrationListen first)), "--system", Text.unpack (encodeIdentity system), "--correlation", "420", "decommission-voter", configurationArgument initial, nodeArgument node])
            ""
        assertBool "last-voter removal returns an operator failure" (refusalCode /= ExitSuccess)
        assertBool "the last voter has an explicit diagnostic" ("cannot remove the last Oracle voter" `Text.isInfixOf` Text.pack refusalDiagnostic)
        assertEqual "last-voter preflight leaves the applied control prefix and configuration unchanged" before =<< awaitOracleObservation (peerListen first) Just
        unknown <- voterMutation directory administrator first system 421 ["cancel-voter-change", "999999"]
        assertBool "an unknown cancellation is canonically refused" ("VoterUnknownChange" `Text.isInfixOf` unknown)
        assertEqual "refusals leave the stable configuration unchanged" initial =<< awaitStableVoters residents [founder]
        assertClosedVoterReceiptLifetime administrator first system 421 unknown
      _ -> fail "singleton voter fixture shape"
  assertBool "last-voter refusal schedule completed" (completed == Just ())

caseSeedJoinedVoter :: Assertion
caseSeedJoinedVoter = withWorkspace $ \directory -> do
  completed <- timeout 120_000_000 $ do
    herald <- executable "eclips-herald"
    administrator <- executable "eclips-admin"
    application <- getExecutablePath
    (_, residents) <- voterDeployment 2
    let descriptor = directory </> "launcher.connection"
        child = directory </> "child.connection"
    case residents of
      [first, second] ->
        withSeedHerald herald directory 1 first ["--bootstrap", "--launcher-output", descriptor] $ \founderProcess -> do
          awaitReady founderProcess (directory </> "herald-1.log")
          founderStatus <- awaitJoinStatus (peerListen first)
          let system = systemIdBytes (joinStatusSystem founderStatus)
              founder = heraldMemberEpoch (joinStatusMember founderStatus)
          withSeedHerald herald directory 2 second ["--peer", Text.unpack (renderEndpoint (peerListen first))] $ \joinedProcess ->
            ( do
                awaitReady joinedProcess (directory </> "herald-2.log")
                joinedStatus <- awaitJoinStatus (peerListen second)
                let joined = heraldMemberEpoch (joinStatusMember joinedStatus)
                assertEqual "seed admission preserves the original system" (joinStatusSystem founderStatus) (joinStatusSystem joinedStatus)
                assertBool "the joined Herald owns a fresh epoch" (joined /= founder)
                initial <- awaitStableVoters residents [founder]
                prepared <- voterMutation directory administrator second system 802 ["prepare-oracle"]
                assertAcceptedVoterReceipt prepared
                registration <- awaitOracleObservation (peerListen first) $ \status ->
                  find ((== joined) . Voter.replicaRegistrationHost) (heraldOracleStatusReplicas status)
                growth <-
                  voterMutation
                    directory
                    administrator
                    first
                    system
                    810
                    ["commission-voters", configurationArgument initial, nodeArgument (Voter.replicaRegistrationNode registration)]
                assertAcceptedVoterReceipt growth
                _ <- awaitStableVoters residents [founder, joined]
                assertCompletedChange residents growth
                assertClosedVoterReceiptLifetime administrator second system 802 prepared
                assertClosedVoterReceiptLifetime administrator first system 810 growth
                forM_ residents $ \planes -> do
                  status <- awaitJoinStatus (peerListen planes)
                  assertEqual
                    "promotion retains the dynamically admitted membership"
                    (sort [founder, joined])
                    (sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (joinStatusMembership status))))
                (parentCode, _, parentDiagnostic) <-
                  runLauncher
                    directory
                    application
                    ["--p08-parent-on-promoted-herald", descriptor, Text.unpack (renderEndpoint (applicationListen second)), child]
                assertEqual ("could not prepare child on seed-joined voter: " <> parentDiagnostic) ExitSuccess parentCode
                (childCode, childOutput, childDiagnostic) <- runLauncher directory application ["--p04-child-after-parent-exit", child]
                assertEqual ("seed-joined voter could not serve its new application: " <> childDiagnostic) ExitSuccess childCode
                assertEqual
                  "new application operates and ends on the promoted dynamic epoch"
                  "child claimed, operated and ended after parent exit\n"
                  childOutput
                assertEqual "promoted seed-joined Herald remains alive" Nothing =<< getProcessExitCode joinedProcess
            )
              `onException` preserveVoterEvidence directory system residents
      _ -> fail "seed-joined voter fixture shape"
  assertBool "seed-joined voter and application schedule completed" (completed == Just ())

caseCheckpointCompletionFirst :: Assertion
caseCheckpointCompletionFirst = do
  let (binding, preparation, _, final) = checkpointRegressionFixture
      step = advanceVoterCheckpoint VoterFinalAppended (Just binding)
      prepared = step preparation initialVoterCheckpointProgress
      missed = step (VoterCommitObserved final) prepared
  assertEqual "owned preparation followed by commit without local append is a miss" (Just VoterCheckpointMissed) (voterCheckpointOutcome missed)
  assertEqual "a terminal miss cannot later inject" missed (step (VoterAppendObserved final) missed)
  let unowned = advanceVoterCheckpoint VoterFinalCommitted (Just binding) (VoterCommitObserved final) initialVoterCheckpointProgress
  assertEqual "final commit without this leader's preparation is a miss" (Just VoterCheckpointMissed) (voterCheckpointOutcome unowned)

caseCheckpointCallbackFirst :: Assertion
caseCheckpointCallbackFirst = do
  let (binding@(VoterCheckpointBinding leader _ _), preparation, joint, final) = checkpointRegressionFixture
      schedule = [preparation, VoterAppendObserved joint, VoterCommitObserved joint, VoterAppendObserved final, VoterCommitObserved final]
  forM_ [minBound .. maxBound] $ \phase -> do
    let progress = foldl (flip (advanceVoterCheckpoint phase (Just binding))) initialVoterCheckpointProgress schedule
    assertEqual ("the owned phase wins and remains fired through final commit: " <> voterPhaseName phase) (Just (VoterCheckpointFired leader)) (voterCheckpointOutcome progress)

caseCheckpointBinding :: Assertion
caseCheckpointBinding = do
  let (binding@(VoterCheckpointBinding leader _ _), preparation, _, final) = checkpointRegressionFixture
      oldNodes = sort (leader : raftVotingConfigurationNodes final)
      growth = stableRaftConfiguration (either (error . show) id (raftVoterSet oldNodes))
      step = advanceVoterCheckpoint VoterFinalCommitted (Just binding)
      stale = step (VoterPreparationObserved leader genesisRaftConfigurationRef) initialVoterCheckpointProgress
      unrelated = step (VoterCommitObserved growth) stale
  assertEqual "a prior growth commit cannot fire or finish a demotion attempt" Nothing (voterCheckpointOutcome unrelated)
  assertEqual "a stale predecessor cannot confer ownership" (Just VoterCheckpointMissed) (voterCheckpointOutcome (step (VoterCommitObserved final) unrelated))
  assertEqual "the exact preparation admits the requested final-commit phase" (Just (VoterCheckpointFired leader)) (voterCheckpointOutcome (step (VoterCommitObserved final) (step preparation unrelated)))

checkpointRegressionFixture :: (VoterCheckpointBinding, VoterCheckpointObservation, RaftVotingConfiguration, RaftVotingConfiguration)
checkpointRegressionFixture =
  let checkedValue :: (Show problem) => Either problem value -> value
      checkedValue = either (error . show) id
      node byte = checkedValue (mkRaftNodeId (Bytes.replicate 32 byte))
      leader = node 1
      previous = checkedValue (raftConfigurationEntryRef (raftLogIndex 5) (raftTerm 1))
      old = checkedValue (raftVoterSet [leader, node 2, node 3])
      new = checkedValue (raftVoterSet [node 2, node 3])
      final = stableRaftConfiguration new
   in (VoterCheckpointBinding leader previous final, VoterPreparationObserved leader previous, jointRaftConfiguration old new, final)

caseVoterLeaderLoss :: VoterFailurePhase -> Assertion
caseVoterLeaderLoss phase = withWorkspace $ \directory -> do
  -- Fresh attempts share the original deadline. An uninjected schedule never
  -- passes: only the selected resident's causal miss witness permits a retry.
  completed <- timeout 180_000_000 (attempt directory (1 :: Int))
  assertBool ("OS role-loss schedule completed at " <> voterPhaseName phase) (completed == Just ())
  where
    attempt directory ordinal = do
      let current = directory </> ("attempt-" <> show ordinal)
      createDirectory current
      injected <- runVoterLeaderLossAttempt phase current
      if injected
        then pure ()
        else do
          -- withWorkspace removes successful runs. Move each missed attempt to
          -- its own retained sibling after every resident has been reaped.
          let retained = directory <> "-missed-" <> show ordinal
          renameDirectory current retained
          putStrLn ("completed voter schedule missed its checkpoint; artifacts retained at " <> retained)
          if ordinal < 3
            then attempt directory (ordinal + 1)
            else assertFailure "three completed voter schedules missed the selected owned checkpoint"

runVoterLeaderLossAttempt :: VoterFailurePhase -> FilePath -> IO Bool
runVoterLeaderLossAttempt phase directory = do
  helper <- getExecutablePath
  administrator <- executable "eclips-admin"
  (plan, residents) <- voterDeployment 3
  let manifest = directory </> "run.manifest"
      system = deploymentSystemBytes plan
  Bytes.writeFile manifest (encodeDeployment plan)
  withCheckpointResidents helper directory manifest residents $ \processes ->
    ( case residents of
        [first, second, third] -> do
          hosts <- traverse (fmap (heraldMemberEpoch . joinStatusMember) . awaitJoinStatus . peerListen) residents
          founder <- case hosts of
            value : _ -> pure value
            [] -> fail "role-loss founder missing"
          initial <- awaitStableVoters residents [founder]
          forM_ (zip [502, 503] [second, third]) $ \(correlation, planes) ->
            voterMutation directory administrator planes system correlation ["prepare-oracle"] >>= assertAcceptedVoterReceipt
          registrations <- awaitOracleObservation (peerListen first) $ \status ->
            traverse (\host -> find ((== host) . Voter.replicaRegistrationHost) (heraldOracleStatusReplicas status)) (drop 1 hosts)
          growth <-
            voterMutation
              directory
              administrator
              first
              system
              510
              (["commission-voters", configurationArgument initial] <> map (nodeArgument . Voter.replicaRegistrationNode) registrations)
          assertAcceptedVoterReceipt growth
          three <- awaitStableVoters residents hosts
          allRegistrations <- awaitRegistrations first hosts
          let member = residentMember (NonEmpty.head (deploymentResidents plan))
          -- The fixed-manifest founder needs no advertised contact. Query each
          -- resident's actual listener using its matching committed identity.
          forM_ (zip residents allRegistrations) $ \(planes, registration) ->
            awaitDirectNativeConfiguration plan member planes registration three
          leader <- awaitVoterLeader residents system three
          (leaderOrdinal, leaderPlanes, leaderHost) <- case [ (ordinal, planes, host)
                                                            | (ordinal, (planes, host)) <- zip [1 ..] (zip residents hosts),
                                                              any (\binding -> raftVoterBindingNode binding == leader && raftVoterBindingHeraldEpoch binding == host) (Voter.voterConfigurationBindings three)
                                                            ] of
            [selected] -> pure selected
            _ -> assertFailure "the observed leader has exactly one resident"
          final <- stableRaftConfiguration <$> checked (raftVoterSet [raftVoterBindingNode binding | binding <- Voter.voterConfigurationBindings three, raftVoterBindingNode binding /= leader])
          armVoterCheckpoint directory leaderOrdinal phase (VoterCheckpointBinding leader (Voter.voterConfigurationNativeRef three) final)
          demotion <-
            voterMutation
              directory
              administrator
              leaderPlanes
              system
              511
              ["decommission-voter", configurationArgument three, nodeArgument leader]
          assertAcceptedVoterReceipt demotion
          witness <- awaitVoterCheckpointWitness directory leaderOrdinal
          case witness of
            MissedCheckpoint evidence -> do
              -- A local exact final-commit callback proves this attempt missed
              -- its owned phase. Retry only after the original change is also
              -- observably Completed; all other failures propagate unchanged.
              _ <- awaitStableVoters residents (filter (/= leaderHost) hosts)
              assertCompletedChange residents demotion
              Text.writeFile (directory </> "missed-checkpoint.log") evidence
              preserveVoterEvidence directory system residents
              pure False
            FiredCheckpoint fired -> do
              assertBool
                "failure occurred at the selected causal owner checkpoint"
                (Text.pack (voterPhaseName phase) `elem` Text.lines fired)
              assertBool
                "the demoted resident owned the captured leader preparation"
                (("leader " <> encodeIdentity (raftNodeIdBytes leader)) `elem` Text.lines fired)
              _ <- awaitStableVoters residents (filter (/= leaderHost) hosts)
              assertCompletedChange residents demotion
              assertClosedVoterReceiptLifetime administrator leaderPlanes system 511 demotion
              assertClosedVoterReceiptLifetime administrator first system 510 growth
              -- A later canonical command proves the surviving quorum can still
              -- serve work; the failed Oracle's Herald must apply that same prefix.
              survivor <- case [planes | (planes, host) <- zip residents hosts, host /= leaderHost] of
                value : _ -> pure value
                [] -> assertFailure "two voters survive the role loss"
              later <- voterMutation directory administrator survivor system 512 ["prepare-oracle"]
              assertAcceptedVoterReceipt later
              laterIndex <- receiptControlIndex later
              _ <- awaitOracleObservation (peerListen leaderPlanes) $ \status ->
                if heraldOracleStatusControlIndex status >= laterIndex then Just () else Nothing
              forM_ (zip residents processes) $ \(planes, process) -> do
                assertEqual "Oracle role loss leaves each Herald OS process running" Nothing =<< getProcessExitCode process
                status <- awaitJoinStatus (peerListen planes)
                assertEqual
                  "Oracle role loss and demotion preserve Herald membership"
                  (sort hosts)
                  (sort (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (joinStatusMembership status))))
              pure True
        _ -> fail "three-resident role-loss fixture shape"
    )
      `onException` preserveVoterEvidence directory system residents

caseVoterCancellation :: Assertion
caseVoterCancellation = withWorkspace $ \directory -> do
  completed <- timeout 120_000_000 $ do
    helper <- getExecutablePath
    administrator <- executable "eclips-admin"
    (plan, residents) <- voterDeployment 2
    let manifest = directory </> "run.manifest"
        system = deploymentSystemBytes plan
    Bytes.writeFile manifest (encodeDeployment plan)
    withCheckpointResidents helper directory manifest residents $ \processes -> case (residents, processes) of
      ([first, second], [_, learnerProcess]) -> do
        founder <- heraldMemberEpoch . joinStatusMember <$> awaitJoinStatus (peerListen first)
        learnerHost <- heraldMemberEpoch . joinStatusMember <$> awaitJoinStatus (peerListen second)
        initial <- awaitStableVoters residents [founder]
        voterMutation directory administrator second system 602 ["prepare-oracle"] >>= assertAcceptedVoterReceipt
        learner <- awaitOracleObservation (peerListen first) $ \status ->
          find ((== learnerHost) . Voter.replicaRegistrationHost) (heraldOracleStatusReplicas status)
        -- No transport of the new Begin entry can reach this learner. Its
        -- preparation frontier therefore cannot become jointly eligible.
        terminateProcess learnerProcess
        _ <- waitForProcess learnerProcess
        let arm = voterArmPath directory 1
        Text.writeFile (arm <> ".pending") "observe-preparing"
        renameFile (arm <> ".pending") arm
        begin <-
          voterMutation
            directory
            administrator
            first
            system
            610
            ["commission-voters", configurationArgument initial, nodeArgument (Voter.replicaRegistrationNode learner)]
        assertAcceptedVoterReceipt begin
        _ <- awaitFixtureFile (voterFiredPath directory 1)
        ident <- receiptControlIndex begin >>= checked . Voter.mkVoterChangeId
        preparing <- awaitOracleObservation (peerListen first) $ \status -> do
          pending <- heraldOracleStatusPendingChange status
          if Voter.voterChangeId pending == ident && Voter.voterChangePhase pending == Voter.VoterChangePreparing
            then Just status
            else Nothing
        assertEqual
          "unavailable learner has not changed the committed voter configuration"
          (Just initial)
          (heraldOracleStatusConfiguration preparing)
        let changeArgument = show (controlIndexWord64 (Voter.voterChangeIdControlIndex ident))
        cancelled <- voterMutation directory administrator first system 611 ["cancel-voter-change", changeArgument]
        assertAcceptedVoterReceipt cancelled
        assertBool
          "cancellation's immutable result records its terminal phase"
          ("VoterChangeCancelled" `Text.isInfixOf` cancelled)
        assertEqual
          "cancelled preparation leaves the original stable voter configuration"
          initial
          =<< awaitStableVoters [first] [founder]
        retained <- restrictedJoin (peerListen first) (ReadOracleVoterChange ident)
        case retained of
          OracleVoterChangeReply (Just change) -> assertEqual "the cancelled change remains queryable" Voter.VoterChangeCancelled (Voter.voterChangePhase change)
          _ -> assertFailure ("cancelled change missing: " <> show retained)
        rendered <- runVoterCLI administrator first system 612 ["voter-change", changeArgument]
        assertBool "the public change query reports cancellation" ("VoterChangeCancelled" `Text.isInfixOf` rendered)
        assertClosedVoterReceiptLifetime administrator first system 610 begin
        assertClosedVoterReceiptLifetime administrator first system 611 cancelled
      _ -> fail "unavailable learner fixture shape"
  assertBool "unavailable learner cancellation schedule completed" (completed == Just ())

withCheckpointResidents :: FilePath -> FilePath -> FilePath -> [DeploymentEndpoints] -> ([ProcessHandle] -> IO value) -> IO value
withCheckpointResidents helper directory manifest residents use = start (1 :: Int) residents []
  where
    start _ [] reversed = use (reverse reversed)
    start ordinal (_ : rest) reversed =
      withFile (directory </> ("herald-" <> show ordinal <> ".log")) WriteMode $ \output ->
        withScopedProcess (proc helper ["--p08-checkpoint-herald", manifest, show ordinal, directory]) {std_out = UseHandle output, std_err = UseHandle output} $ \process -> do
          awaitReady process (directory </> ("herald-" <> show ordinal <> ".log"))
          start (ordinal + 1) rest (process : reversed)

-- Hints select which process to arm. The fired native preparation frontier
-- subsequently proves that process actually owned the interrupted leadership.
awaitVoterLeader :: [DeploymentEndpoints] -> Bytes.ByteString -> Voter.VoterConfiguration -> IO RaftNodeId
awaitVoterLeader residents system configuration = do
  statuses <- traverse (\planes -> Text.concat <$> runAdministration (administrationListen planes) system 790 InspectStatus) residents
  let hints = [raw | status <- statuses, line <- Text.lines status, Just raw <- [Text.stripPrefix "oracle-leader-hint " line]]
      nodes =
        [ raftVoterBindingNode binding
        | binding <- Voter.voterConfigurationBindings configuration,
          let encoded = encodeIdentity (raftNodeIdBytes (raftVoterBindingNode binding)),
          length hints == length residents && all (== encoded) hints
        ]
  case nodes of
    [node] -> pure node
    _ -> threadDelay 10_000 >> awaitVoterLeader residents system configuration

data VoterCheckpointWitness = FiredCheckpoint Text.Text | MissedCheckpoint Text.Text

awaitVoterCheckpointWitness :: FilePath -> Int -> IO VoterCheckpointWitness
awaitVoterCheckpointWitness directory ordinal = do
  fired <- doesFileExist (voterFiredPath directory ordinal)
  if fired
    then FiredCheckpoint <$> Text.readFile (voterFiredPath directory ordinal)
    else do
      missed <- doesFileExist (voterMissedPath directory ordinal)
      if missed
        then MissedCheckpoint <$> Text.readFile (voterMissedPath directory ordinal)
        else threadDelay 10_000 >> awaitVoterCheckpointWitness directory ordinal

awaitFixtureFile :: FilePath -> IO Text.Text
awaitFixtureFile path =
  doesFileExist path >>= \exists ->
    if exists then Text.readFile path else threadDelay 10_000 >> awaitFixtureFile path

voterDeployment :: Int -> IO (CheckedDeployment, [DeploymentEndpoints])
voterDeployment count = do
  allocated <- freshEndpoints (5 * count)
  let planes [] = pure []
      planes (p : a : d : o : r : rest) =
        (:) <$> checked (deploymentEndpointsFromPlanes p a d o r Nothing Nothing Nothing Nothing Nothing) <*> planes rest
      planes _ = fail "voter endpoint fixture shape"
  residents <- planes allocated
  case residents of
    first : rest -> (,residents) <$> checked (checkDeployment (Bytes.pack [11 .. 42]) (first :| rest))
    [] -> fail "voter deployment requires a founder"

withFixedResidents :: FilePath -> FilePath -> FilePath -> FilePath -> [DeploymentEndpoints] -> ([ProcessHandle] -> IO value) -> IO value
withFixedResidents herald directory manifest descriptor residents use = start 1 residents []
  where
    start _ [] reversed = use (reverse reversed)
    start ordinal (_ : rest) reversed = withHerald herald directory manifest descriptor ordinal $ \process -> do
      awaitReady process (directory </> ("herald-" <> show ordinal <> ".log"))
      start (ordinal + 1) rest (process : reversed)

configurationArgument :: Voter.VoterConfiguration -> String
configurationArgument = Text.unpack . encodeIdentity . Voter.voterConfigurationIdBytes . Voter.voterConfigurationId

nodeArgument :: RaftNodeId -> String
nodeArgument = Text.unpack . encodeIdentity . raftNodeIdBytes

runVoterCLI :: FilePath -> DeploymentEndpoints -> Bytes.ByteString -> Word64 -> [String] -> IO Text.Text
runVoterCLI administrator planes system correlation arguments = do
  (code, output, diagnostic) <-
    readCreateProcessWithExitCode
      (proc administrator (["--endpoint", Text.unpack (renderEndpoint (administrationListen planes)), "--system", Text.unpack (encodeIdentity system), "--correlation", show correlation] <> arguments))
      ""
  assertEqual ("voter administration failed: " <> unwords arguments <> "\n" <> diagnostic) ExitSuccess code
  pure (Text.pack output)

-- One invocation waits on its own connection for the original canonical
-- command receipt, then retires that delivery receipt before closing.
voterMutation :: FilePath -> FilePath -> DeploymentEndpoints -> Bytes.ByteString -> Word64 -> [String] -> IO Text.Text
voterMutation directory administrator planes system correlation arguments = do
  initial <- runVoterCLI administrator planes system correlation arguments
  Text.appendFile (directory </> "voter-operations.log") (Text.pack (show correlation <> " " <> unwords arguments) <> "\n" <> initial)
  assertBool
    ("voter operation did not return its terminal receipt: " <> Text.unpack initial)
    ("receipt-control-index " `Text.isInfixOf` initial)
  pure initial

assertAcceptedVoterReceipt :: Text.Text -> Assertion
assertAcceptedVoterReceipt receipt =
  assertBool
    ("expected accepted canonical voter receipt: " <> Text.unpack receipt)
    ("receipt-result OracleAccepted" `Text.isInfixOf` receipt)

assertClosedVoterReceiptLifetime :: FilePath -> DeploymentEndpoints -> Bytes.ByteString -> Word64 -> Text.Text -> Assertion
assertClosedVoterReceiptLifetime _administrator planes system correlation receipt = do
  result <- Text.concat <$> runAdministration (administrationListen planes) system correlation InspectResult
  assertBool "the caller owns its original canonical result" ("receipt-control-index " `Text.isInfixOf` receipt)
  assertBool "a fresh connection cannot recover another lifetime's receipt" ("AdminAbsent" `Text.isInfixOf` result)

awaitOracleObservation :: Endpoint -> (HeraldOracleStatus -> Maybe value) -> IO value
awaitOracleObservation target observe =
  restrictedJoin target ReadOracleStatus >>= \case
    OracleStatusReply status | Just value <- observe status -> pure value
    _ -> threadDelay 10_000 >> awaitOracleObservation target observe

awaitStableVoters :: [DeploymentEndpoints] -> [HeraldEpoch] -> IO Voter.VoterConfiguration
awaitStableVoters residents expected = do
  configurations <-
    traverse
      ( \planes -> awaitOracleObservation (peerListen planes) $ \status -> do
          configuration <- heraldOracleStatusConfiguration status
          case Voter.voterConfigurationView configuration of
            Voter.StableVoterConfigurationView _
              | sort (map raftVoterBindingHeraldEpoch (Voter.voterConfigurationBindings configuration)) == sort expected,
                heraldOracleStatusPendingChange status == Nothing ->
                  Just configuration
            _ -> Nothing
      )
      residents
  case configurations of
    first : rest -> do
      assertBool "every Herald applies the identical stable voter configuration" (all (== first) rest)
      pure first
    [] -> fail "stable voter observer requires a resident"

assertCompletedChange :: [DeploymentEndpoints] -> Text.Text -> Assertion
assertCompletedChange residents receipt = do
  ident <- receiptControlIndex receipt >>= checked . Voter.mkVoterChangeId
  forM_ residents $ \planes -> do
    reply <- restrictedJoin (peerListen planes) (ReadOracleVoterChange ident)
    case reply of
      OracleVoterChangeReply (Just change) -> assertEqual "retained change completed" Voter.VoterChangeCompleted (Voter.voterChangePhase change)
      _ -> assertFailure ("completed change is retained: " <> show reply)

receiptControlIndex :: Text.Text -> IO ControlIndex
receiptControlIndex receipt = case [value | line <- Text.lines receipt, Just raw <- [Text.stripPrefix "receipt-control-index " line], Just value <- [readMaybe (Text.unpack raw)]] of
  [value] -> pure (controlIndex value)
  _ -> assertFailure "canonical voter command receipt has one control index"

preserveVoterEvidence :: FilePath -> Bytes.ByteString -> [DeploymentEndpoints] -> IO ()
preserveVoterEvidence directory system residents = forM_ (zip [1 :: Int ..] residents) $ \(ordinal, planes) -> do
  let stem = directory </> ("herald-" <> show ordinal)
  writeEvidence (stem <> ".oracle-configuration") (runAdministration (administrationListen planes) system 799 InspectOracleConfiguration)
  writeEvidence (stem <> ".status") (runAdministration (administrationListen planes) system 798 InspectStatus)

caseParentExitBeforeClaim :: Assertion
caseParentExitBeforeClaim = withWorkspace $ \directory -> do
  completed <- timeout 120_000_000 $ do
    herald <- executable "eclips-herald"
    application <- getExecutablePath
    allocated <- freshEndpoints 5
    planes <- case allocated of
      [peer, client, administration, oracle, raft] -> checked (deploymentEndpointsFromPlanes peer client administration oracle raft Nothing Nothing Nothing Nothing Nothing)
      _ -> fail "parent exit endpoint fixture shape"
    plan <- checked (checkDeployment (Bytes.pack [2 .. 33]) (planes :| []))
    let manifest = directory </> "run.manifest"
        parentDescriptor = directory </> "launcher.connection"
        childDescriptor = directory </> "child.connection"
    Bytes.writeFile manifest (encodeDeployment plan)
    withHerald herald directory manifest parentDescriptor 1 $ \resident -> do
      awaitReady resident (directory </> "herald-1.log")
      (parentExit, parentOutput, parentDiagnostic) <- runLauncher directory application ["--p04-parent-before-claim", parentDescriptor, childDescriptor]
      assertEqual ("parent application failed: " <> parentDiagnostic) ExitSuccess parentExit
      assertEqual "ordered parent End completed before its OS process exited" "parent ended before child claim\n" parentOutput
      -- runLauncher waits and joins the parent before the child process is
      -- created. The child can receive only the retained opaque descriptor.
      (childExit, childOutput, childDiagnostic) <- runLauncher directory application ["--p04-child-after-parent-exit", childDescriptor]
      assertEqual ("child application failed after parent exit: " <> childDiagnostic) ExitSuccess childExit
      assertEqual "the orphaned preparation remains attachable and usable" "child claimed, operated and ended after parent exit\n" childOutput
      inspections <- runAdministration (administrationListen planes) (deploymentSystemBytes plan) 91 InspectPreparations
      assertEqual "one child preparation survives its parent" 1 (length (Text.lines (Text.concat inspections)))
      assertBool "the child reaches ordered End" (all (Text.isInfixOf "AdminPreparationTerminalDto") (Text.lines (Text.concat inspections)))
      _ <- runAdministration (administrationListen planes) (deploymentSystemBytes plan) 92 Drain
      assertHeraldExit directory 1 "parent exit deployment drained and joined" resident
  case completed of
    Just () -> pure ()
    Nothing -> do
      logs <- traverse (Text.readFile . (directory </>)) ["herald-1.log", "launcher.err"]
      assertFailure ("parent exit OS tour timed out\n" <> Text.unpack (Text.intercalate "\n" logs))

-- Exercise CLI admission, inherited discovery configuration and real application
-- startup together; these are configuration checks, not takeover-latency claims.
caseTakeoverTiming :: Assertion
caseTakeoverTiming = withWorkspace $ \directory -> do
  completed <- timeout 120_000_000 $ do
    herald <- executable "eclips-herald"
    helper <- getExecutablePath
    allocated <- freshEndpoints 5
    planes <- case allocated of
      [peer, application, administration, oracle, raft] -> checked (deploymentEndpointsFromPlanes peer application administration oracle raft Nothing Nothing Nothing Nothing Nothing)
      _ -> fail "timing endpoint fixture shape"
    let descriptor = directory </> "launcher.connection"
        logPath = directory </> "herald-1.log"
    withSeedHerald herald directory 1 planes ["--bootstrap", "--takeover-target", "8s", "--launcher-output", descriptor] $ \resident -> do
      awaitReady resident logPath
      output <- Text.lines <$> Text.readFile logPath
      assertEqual
        "ready reports every resolved nondefault duration"
        [ "timing takeover-target 8000000us",
          "timing heartbeat-idle 400000us",
          "timing heartbeat-reply 800000us",
          "timing peer-recovery 3200000us",
          "timing failure-probe 800000us",
          "timing raft-heartbeat 40000us",
          "timing raft-election-lower 400000us",
          "timing raft-election-upper 800000us",
          "timing health-round 400000us",
          "timing health-query 160000us",
          "timing isolation-grace 4800000us",
          "timing application-recovery 3200000us",
          "timing reconnect-initial 16000us",
          "timing submission-initial 160000us",
          "timing retry-cap 400000us",
          "timing application-reply 10000000us",
          "timing read-only-drain 500000us"
        ]
        (filter (Text.isPrefixOf "timing ") output)
      discovery <- exchangeDiscoveryRequest (peerListen planes) (DiscoverSystem 801 [])
      bootstrap <- case discovery of
        Right (SystemDiscovered 801 snapshot) -> pure (discoveryBootstrap snapshot)
        other -> assertFailure ("timing deployment discovery failed: " <> show other)
      assertEqual "discovery carries the founder target to future joiners" 8_000_000 (takeoverTargetMicroseconds (deploymentTakeoverTarget bootstrap))
      (code, applicationOutput, diagnostic) <- runLauncher directory helper ["--timing-applications", descriptor]
      assertEqual ("ordinary nondefault application startup failed: " <> diagnostic) ExitSuccess code
      assertEqual
        "ordinary parent and prepared child completed using inherited defaults"
        "nondefault timing reached launcher and prepared child\n"
        applicationOutput
      _ <- runAdministration (administrationListen planes) (deploymentSystemBytes bootstrap) 802 Drain
      assertHeraldExit directory 1 "timing deployment drained and joined" resident
  case completed of
    Just () -> pure ()
    Nothing -> assertFailure ("nondefault takeover timing OS case timed out; artifacts: " <> directory)

caseScopeCleanup :: Assertion
caseScopeCleanup = withWorkspace $ \directory -> do
  herald <- executable "eclips-herald"
  let manifest = directory </> "must-not-be-written.manifest"
      invalidArguments =
        [ (["--bootstrap", "--fixed-manifest", manifest, "--base-port", "0"], "choose exactly one startup mode"),
          (["--write-fixed-manifest", manifest, "--fixed-manifest", manifest, "--member", "127.0.0.1:42000"], "choose exactly one startup mode"),
          (["--fixed-manifest", manifest, "--member", "1", "--application-advertise", "localhost:42001"], "argument is not supported"),
          (["--write-fixed-manifest", manifest, "--member", "127.0.0.1:42000", "--bind", "localhost"], "argument is not supported"),
          (["--bootstrap", "--base-port", "0", "--member", "1"], "argument is not supported"),
          (["--bootstrap", "--base-port", "0", "--takeover-target", "0s"], "TakeoverTargetBelowMicrosecondResolution"),
          (["--bootstrap", "--base-port", "0", "--takeover-target", "5"], "takeover target requires s, ms or us"),
          (["--peer", "127.0.0.1:42000", "--base-port", "0", "--takeover-target", "8s"], "argument is not supported"),
          (["--fixed-manifest", manifest, "--member", "1", "--takeover-target", "8s"], "argument is not supported")
        ]
  mapM_
    ( \(arguments, expected) -> do
        outcome <- timeout 5_000_000 (readCreateProcessWithExitCode (proc herald arguments) "")
        (code, _, diagnostic) <- maybe (assertFailure "invalid CLI arguments started a long-running resident") pure outcome
        assertBool "invalid CLI mode combinations fail before startup" (code /= ExitSuccess)
        assertBool ("unexpected CLI rejection: " <> diagnostic) (Text.pack expected `Text.isInfixOf` Text.pack diagnostic)
    )
    invalidArguments
  assertBool "invalid CLI modes cannot create a manifest" . not =<< doesFileExist manifest
  allocated <- freshEndpoints 5
  case allocated of
    [peer, application, administration, oracle, raft] -> do
      planes <- checked (deploymentEndpointsFromPlanes peer application administration oracle raft Nothing Nothing Nothing Nothing Nothing)
      plan <- checked (checkDeployment (Bytes.pack [1 .. 32]) (planes :| []))
      result <- timeout 10_000_000 (try @IOException (withDeploymentResident plan 1 (\_ -> ioError (userError "scope cleanup sentinel"))))
      case result of
        Just (Left failure) -> assertBool "callback failure is propagated" ("scope cleanup sentinel" `Text.isInfixOf` Text.pack (show failure))
        _ -> assertFailure "deployment scope did not propagate its callback failure"
      mapM_ rebind allocated
    _ -> fail "scope cleanup endpoint shape"
  where
    rebind value = bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
      setSocketOption listener ReuseAddr 1
      bind listener (SockAddrInet (fromIntegral (endpointPort value)) (tupleToHostAddress (127, 0, 0, 1)))

data TourMode = OrdinaryTour | FailedSpawnTour | ChildReplyLossTour
  deriving stock (Eq)

tour :: TourMode -> Bool -> Bool -> Assertion
tour mode readerFirst remote = withWorkspace $ \directory -> do
  completed <- timeout 180_000_000 $ do
    herald <- executable "eclips-herald"
    administrator <- executable "eclips-admin"
    launcher <- executable "eclips-hello-launcher"
    publisher <- executable "eclips-hello-publisher"
    reader <- executable "eclips-hello-reader"
    endpoints <- freshEndpoints 11
    (first, second, proxy) <- case endpoints of
      [peer1, application1, administration1, oracle1, raft1, peer2, application2, administration2, oracle2, raft2, relay] -> do
        first <- checked (deploymentEndpointsFromPlanes peer1 application1 administration1 oracle1 raft1 Nothing Nothing Nothing Nothing Nothing)
        second <- checked (deploymentEndpointsFromPlanes peer2 application2 administration2 oracle2 raft2 Nothing Nothing Nothing Nothing Nothing)
        pure (first, second, relay)
      _ -> fail "endpoint fixture shape"
    firstWithProxy <- checked (deploymentEndpointsFromPlanes (peerListen first) (applicationListen first) (administrationListen first) (oracleListen first) (raftListen first) (Just proxy) Nothing Nothing Nothing Nothing)
    let loseReply = mode == ChildReplyLossTour
        failedSpawn = mode == FailedSpawnTour
        founder = if loseReply then firstWithProxy else first
    plan <- checked (checkDeployment (Bytes.pack [0 .. 31]) (founder :| [second]))
    let manifest = directory </> "run.manifest"
        descriptor = directory </> "launcher.connection"
        system = deploymentSystemBytes plan
        run dropObserved = withHerald herald directory manifest descriptor 1 $ \founderProcess ->
          withHerald herald directory manifest descriptor 2 $ \remoteProcess -> do
            awaitReady founderProcess (directory </> "herald-1.log")
            awaitReady remoteProcess (directory </> "herald-2.log")
            (statusExit, statusOutput, statusError) <- readCreateProcessWithExitCode (proc administrator ["--endpoint", Text.unpack (renderEndpoint (administrationListen first)), "--system", Text.unpack (encodeIdentity system), "status"]) ""
            assertEqual ("operator status failed: " <> statusError) ExitSuccess statusExit
            assertBool "operator status reports this system" (encodeIdentity system `Text.isInfixOf` Text.pack statusOutput)
            exists <- doesFileExist descriptor
            assertBool "only the founder wrote the requested launcher artifact" exists
            let publisherTarget = if loseReply then proxy else applicationListen first
                readerTarget = if remote then applicationListen second else publisherTarget
                args = ["--connection-file", descriptor, "--publisher-herald", Text.unpack (renderEndpoint publisherTarget), "--reader-herald", Text.unpack (renderEndpoint readerTarget), "--publisher-executable", if failedSpawn then directory </> "missing-publisher" else publisher, "--reader-executable", reader] <> ["--reader-first" | readerFirst]
            (exitCode, output, diagnostic) <- runLauncher directory launcher args
            case mode of
              FailedSpawnTour -> assertBool "the first OS spawn failed" (exitCode /= ExitSuccess)
              ChildReplyLossTour -> do
                assertBool "the disconnected prepared child cannot finish its startup" (exitCode /= ExitSuccess)
                assertBool
                  ("the child retry must report terminal startup: " <> diagnostic)
                  ("ConnectStartupRejected StartupNotLive" `Text.isInfixOf` Text.pack diagnostic)
                assertEqual "a disconnected child cannot complete the hello workflow" "" output
              OrdinaryTour -> do
                assertEqual ("ordinary launcher failed: " <> diagnostic) ExitSuccess exitCode
                assertEqual "all data and acknowledgements crossed ECLIPS" "Hello, world!\n" output
            assertEqual "the exact child's terminal preparation followed its lost session-open reply" loseReply =<< dropObserved
            inspections <- awaitTerminalPreparations (administrationListen first) system
            assertEqual "parent retains both child preparation receipts" 2 (length (Text.lines (Text.concat inspections)))
            assertBool "both children reached ordered End" (all (Text.isInfixOf "AdminPreparationTerminalDto") (Text.lines (Text.concat inspections)))
            _ <- runAdministration (administrationListen second) system 81 Drain
            assertHeraldExit directory 2 "remote Herald drained and joined" remoteProcess
            _ <- runAdministration (administrationListen first) system 82 Drain
            assertHeraldExit directory 1 "founder Herald and co-hosted Oracle drained and joined" founderProcess
    Bytes.writeFile manifest (encodeDeployment plan)
    if loseReply
      then withReplyLossProxy proxy (applicationListen first) (awaitChildTermination directory (administrationListen first) system) run
      else run (pure False)
  case completed of
    Just () -> pure ()
    Nothing -> do
      logs <- traverse (Text.readFile . (directory </>)) ["herald-1.log", "herald-2.log", "launcher.err"]
      assertFailure ("OS deployment tour timed out\n" <> Text.unpack (Text.intercalate "\n" logs))

-- Prepared-child attachments contain the process epoch bytes. Match the exact
-- public administration field, not whichever preparation happens to end first.
-- Each fresh response is the synchronization event; no elapsed delay establishes
-- ordering between the upstream loss and the client's permitted retry.
awaitChildTermination :: FilePath -> Endpoint -> Bytes.ByteString -> ConnectionDescriptor -> IO ()
awaitChildTermination directory endpointValue system descriptor = do
  completed <- timeout 10_000_000 observe
  case completed of
    Just () -> pure ()
    Nothing -> assertFailure "the exact child did not become terminal after its upstream connection closed"
  where
    processField = "process=" <> encodeIdentity (connectionDescriptorAttachmentBytes descriptor)
    observe = do
      inspections <- runAdministration endpointValue system 70 InspectPreparations
      case [line | line <- Text.lines (Text.concat inspections), processField `elem` Text.words line] of
        [line]
          | "AdminPreparationTerminalDto" `elem` Text.words line ->
              Text.writeFile (directory </> "child-reply-loss.preparation") (line <> "\n")
          | otherwise -> observe
        matches -> assertFailure ("expected one preparation for the dropped child's " <> Text.unpack processField <> "; got " <> show matches)

-- A failed child makes the launcher's scope kill and join its sibling. That
-- physical cleanup precedes the sibling's projected End, so await both receipts.
awaitTerminalPreparations :: Endpoint -> Bytes.ByteString -> IO [Text.Text]
awaitTerminalPreparations endpointValue system = do
  completed <- timeout 10_000_000 observe
  maybe (assertFailure "both child preparations did not become terminal after launcher cleanup") pure completed
  where
    observe = do
      inspections <- runAdministration endpointValue system 71 InspectPreparations
      let entries = Text.lines (Text.concat inspections)
      assertEqual "parent retains both child preparation receipts while awaiting End" 2 (length entries)
      if all (elem "AdminPreparationTerminalDto" . Text.words) entries
        then pure inspections
        else observe

assertHeraldExit :: FilePath -> Int -> String -> ProcessHandle -> Assertion
assertHeraldExit directory ordinal message process = do
  outcome <- waitForProcess process
  diagnostic <- Text.readFile (directory </> ("herald-" <> show ordinal <> ".log"))
  assertEqual (message <> "\n" <> Text.unpack diagnostic) ExitSuccess outcome

withHerald :: FilePath -> FilePath -> FilePath -> FilePath -> Int -> (ProcessHandle -> IO value) -> IO value
withHerald executablePath directory manifest descriptor ordinal use =
  withFile (directory </> ("herald-" <> show ordinal <> ".log")) WriteMode $ \logHandle ->
    withScopedProcess
      (proc executablePath (["--fixed-manifest", manifest, "--member", show ordinal] <> if ordinal == 1 then ["--launcher-output", descriptor] else [])) {std_out = UseHandle logHandle, std_err = UseHandle logHandle}
      use

runLauncher :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runLauncher = runLauncherObserved (pure ())

runLauncherObserved :: IO () -> FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runLauncherObserved beforeCleanup directory path arguments = do
  let outputPath = directory </> "launcher.out"
      diagnosticPath = directory </> "launcher.err"
  writeFile (directory </> "launcher.arguments") (unlines (path : arguments))
  exitCode <- withFile outputPath WriteMode $ \output -> withFile diagnosticPath WriteMode $ \diagnostic ->
    withScopedProcess (proc path arguments) {std_out = UseHandle output, std_err = UseHandle diagnostic} $ \process -> do
      code <- waitForProcess process `onException` beforeCleanup
      case code of
        ExitSuccess -> pure ()
        _ -> beforeCleanup
      pure code
  output <- readFile' outputPath
  diagnostic <- readFile' diagnosticPath
  pure (exitCode, output, diagnostic)

-- SIGINT reaches the whole ordinary application process group on exceptional
-- test exit; every launcher worker and child is joined before the scope closes.
withScopedProcess :: CreateProcess -> (ProcessHandle -> IO value) -> IO value
withScopedProcess command use =
  bracket (createProcess command {create_group = True}) stop (\(_, _, _, process) -> use process)
  where
    stop (_, _, _, process) = do
      exit <- getProcessExitCode process
      case exit of
        Just _ -> pure ()
        Nothing -> do
          interruptProcessGroupOf process
          stopped <- timeout 5_000_000 (waitForProcess process)
          case stopped of { Just _ -> pure (); Nothing -> terminateProcess process >> void (waitForProcess process) }

awaitReady :: ProcessHandle -> FilePath -> IO ()
awaitReady process path = do
  output <- Text.readFile path
  if "ready\n" `Text.isInfixOf` output
    then pure ()
    else do
      exit <- getProcessExitCode process
      case exit of
        Just code -> assertFailure ("Herald exited before ready: " <> show code <> "\n" <> Text.unpack output)
        Nothing -> threadDelay 10_000 >> awaitReady process path

-- A joining process can remain alive after the seed/old member that it needs
-- has failed. Poll those existing handles with the readiness log so the fixture
-- preserves the first failure and enters scoped cleanup immediately.
awaitReadyWithResidents :: [(ProcessHandle, FilePath)] -> ProcessHandle -> FilePath -> IO ()
awaitReadyWithResidents required process path = do
  forM_ ((process, path) : required) $ \(resident, logPath) ->
    getProcessExitCode resident >>= \case
      Nothing -> pure ()
      Just code -> do
        diagnostic <- Text.readFile logPath
        assertFailure ("required process exited while waiting for " <> path <> ": " <> show code <> "\n" <> logPath <> "\n" <> Text.unpack diagnostic)
  output <- Text.readFile path
  if "ready\n" `Text.isInfixOf` output
    then pure ()
    else threadDelay 10_000 >> awaitReadyWithResidents required process path

-- Supervise each stable resident set through the complete application/control
-- workflow, including blocking child waits. The caller leaves this scope before
-- a deliberate kill or drain, so no watcher can mistake those exits for faults.
withRequiredResidents :: [(ProcessHandle, FilePath)] -> IO value -> IO value
withRequiredResidents required action =
  bracket (start (forever (checkResidents >> threadDelay 10_000))) stop $ \(_, watcherDone) ->
    bracket (start (checkResidents >> action <* checkResidents)) stop $ \(_, actionDone) ->
      atomically (readTMVar watcherDone `orElse` readTMVar actionDone) >>= either throwIO pure
  where
    checkResidents = forM_ required $ \(resident, logPath) ->
      getProcessExitCode resident >>= \case
        Nothing -> pure ()
        Just code -> do
          diagnostic <- Text.readFile logPath
          assertFailure ("required resident exited during the combined tour: " <> show code <> "\n" <> logPath <> "\n" <> Text.unpack diagnostic)
    start :: IO result -> IO (ThreadId, TMVar (Either SomeException result))
    start work = do
      done <- newEmptyTMVarIO
      -- Bracket masks acquisition; explicitly unmask each worker so a failed
      -- resident cancels blocking fixture work and enters its child cleanup.
      worker <- forkIOWithUnmask $ \unmask -> try (unmask work) >>= atomically . putTMVar done
      pure (worker, done)
    stop :: (ThreadId, TMVar (Either SomeException result)) -> IO ()
    stop (worker, done) = killThread worker >> atomically (void (readTMVar done))

-- Reserve the entire fixture at once so distinct protocol planes cannot
-- accidentally receive a port recycled by an earlier fixture allocation.
freshEndpoints :: Int -> IO [Endpoint]
freshEndpoints 0 = pure []
freshEndpoints remaining = bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  address <- getSocketName listener
  current <- case address of
    SockAddrInet port _ -> checked (endpoint "127.0.0.1" (fromIntegral port))
    _ -> fail "unexpected loopback address"
  others <- freshEndpoints (remaining - 1)
  pure (current : others)

withWorkspace :: (FilePath -> IO value) -> IO value
withWorkspace use = do
  createDirectoryIfMissing True ".cabal"
  (path, handle) <- openTempFile ".cabal" "p04-deployment-tour"
  hClose handle
  removeFile path
  createDirectory path
  value <- use path `onException` putStrLn ("deployment test artifacts retained at " <> path)
  removeDirectoryRecursive path
  pure value
executable :: String -> IO FilePath
executable name = findExecutable name >>= maybe (fail ("missing Cabal build tool " <> name)) pure
checked :: (Show problem) => Either problem value -> IO value
checked = either (fail . show) pure
