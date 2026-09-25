{-# LANGUAGE OverloadedStrings #-}

-- | OS fixture interpretation of the public causal Oracle checkpoints. Only
-- the Oracle role fails; the independent ordinary Herald scope stays alive.
module VoterCheckpoint
  ( VoterFailurePhase (..),
    voterPhaseName,
    voterArmPath,
    voterFiredPath,
    voterMissedPath,
    VoterCheckpointBinding (..),
    armVoterCheckpoint,
    VoterCheckpointObservation (..),
    VoterCheckpointOutcome (..),
    VoterCheckpointProgress,
    initialVoterCheckpointProgress,
    advanceVoterCheckpoint,
    voterCheckpointOutcome,
    runCheckpointHerald,
  ) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Monad (when)
import Data.ByteString qualified as Bytes
import Data.List (find)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Eclips.Deployment.Admin (decodeIdentity, encodeIdentity)
import Eclips.Deployment.Manifest (decodeDeployment, deploymentSystemBytes)
import Eclips.Deployment.Runtime
import Eclips.Oracle.Runtime
import Eclips.Raft.Configuration
import Eclips.Raft.Identity
import System.Directory (doesFileExist, renameFile)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)

data VoterFailurePhase = VoterPreparing | VoterJointAppended | VoterJointCommitted | VoterFinalAppended | VoterFinalCommitted
  deriving stock (Eq, Enum, Bounded, Show)

voterPhaseName :: VoterFailurePhase -> String
voterPhaseName = \case
  VoterPreparing -> "preparing"
  VoterJointAppended -> "joint-appended"
  VoterJointCommitted -> "joint-committed"
  VoterFinalAppended -> "final-appended"
  VoterFinalCommitted -> "final-committed"

voterArmPath :: FilePath -> Int -> FilePath
voterArmPath directory ordinal = directory </> ("voter-arm-" <> show ordinal)

voterFiredPath :: FilePath -> Int -> FilePath
voterFiredPath directory ordinal = directory </> ("voter-fired-" <> show ordinal)

voterMissedPath :: FilePath -> Int -> FilePath
voterMissedPath directory ordinal = directory </> ("voter-missed-" <> show ordinal)

-- Bind a failure to the selected leader's next preparation and the exact
-- demotion configuration. A delayed growth callback cannot claim this attempt.
data VoterCheckpointBinding = VoterCheckpointBinding RaftNodeId RaftConfigurationRef RaftVotingConfiguration
  deriving stock (Eq, Show)

data VoterCheckpointObservation
  = VoterPreparationObserved RaftNodeId RaftConfigurationRef
  | VoterAppendObserved RaftVotingConfiguration
  | VoterCommitObserved RaftVotingConfiguration
  deriving stock (Eq, Show)

data VoterCheckpointOutcome = VoterCheckpointFired RaftNodeId | VoterCheckpointMissed
  deriving stock (Eq, Show)

data VoterCheckpointProgress = VoterCheckpointProgress (Maybe RaftNodeId) (Maybe VoterCheckpointOutcome)
  deriving stock (Eq, Show)

initialVoterCheckpointProgress :: VoterCheckpointProgress
initialVoterCheckpointProgress = VoterCheckpointProgress Nothing Nothing

voterCheckpointOutcome :: VoterCheckpointProgress -> Maybe VoterCheckpointOutcome
voterCheckpointOutcome (VoterCheckpointProgress _ outcome) = outcome

-- The selected resident's final-commit adapter callback follows its own
-- preparation, appends and joint commit. If that exact final configuration is
-- reached without the owned phase firing, this attempt cannot exercise it later.
-- For FinalCommitted, the owned requested phase takes priority over the miss.
advanceVoterCheckpoint :: VoterFailurePhase -> Maybe VoterCheckpointBinding -> VoterCheckpointObservation -> VoterCheckpointProgress -> VoterCheckpointProgress
advanceVoterCheckpoint _ _ _ completed@(VoterCheckpointProgress _ (Just _)) = completed
advanceVoterCheckpoint phase binding observation (VoterCheckpointProgress priorLeader Nothing) =
  let eligible = observationMatchesBinding binding observation
      leader = case observation of
        VoterPreparationObserved candidate _ | eligible -> Just candidate
        _ -> priorLeader
      outcome
        | eligible, matchesPhase phase observation, Just owner <- leader = Just (VoterCheckpointFired owner)
        | Just (VoterCheckpointBinding _ _ final) <- binding,
          VoterCommitObserved configuration <- observation,
          configuration == final =
            Just VoterCheckpointMissed
        | otherwise = Nothing
   in VoterCheckpointProgress leader outcome

observationMatchesBinding :: Maybe VoterCheckpointBinding -> VoterCheckpointObservation -> Bool
observationMatchesBinding Nothing = const True
observationMatchesBinding (Just (VoterCheckpointBinding expectedLeader predecessor final)) = \case
  VoterPreparationObserved leader reference -> leader == expectedLeader && reference == predecessor
  VoterAppendObserved configuration -> targetMatches configuration
  VoterCommitObserved configuration -> targetMatches configuration
  where
    targetMatches configuration = case raftVotingConfigurationView configuration of
      JointRaftConfigurationView _ new -> new == raftVotingConfigurationNodes final
      StableRaftConfigurationView _ -> configuration == final

armVoterCheckpoint :: FilePath -> Int -> VoterFailurePhase -> VoterCheckpointBinding -> IO ()
armVoterCheckpoint directory ordinal phase (VoterCheckpointBinding leader predecessor final) =
  writeWitness
    (voterArmPath directory ordinal)
    ( Text.unlines
        [ Text.pack (voterPhaseName phase),
          encodeIdentity (raftNodeIdBytes leader),
          case raftConfigurationRefView predecessor of
            GenesisRaftConfigurationRefView -> "genesis"
            RaftConfigurationEntryRefView index term -> Text.pack (show (raftLogIndexWord64 index) <> " " <> show (raftTermWord64 term)),
          Text.unwords (map (encodeIdentity . raftNodeIdBytes) (raftVotingConfigurationNodes final))
        ]
    )

readArm :: Text.Text -> IO (Bool, VoterFailurePhase, Maybe VoterCheckpointBinding)
readArm contents = case Text.lines (Text.strip contents) of
  name : fields -> do
    let (interrupt, phaseText) = case Text.stripPrefix "observe-" name of
          Just observedPhase -> (False, observedPhase)
          Nothing -> (True, name)
    phase <- maybe (fail "invalid armed voter phase") pure (find ((== phaseText) . Text.pack . voterPhaseName) [minBound .. maxBound])
    binding <- case fields of
      [] -> pure Nothing
      [leaderText, predecessorText, finalText] -> do
        leader <- node leaderText
        predecessor <- case Text.words predecessorText of
          ["genesis"] -> pure genesisRaftConfigurationRef
          [indexText, termText] -> do
            index <- number indexText
            term <- number termText
            checked (raftConfigurationEntryRef (raftLogIndex index) (raftTerm term))
          _ -> fail "invalid checkpoint predecessor"
        final <- stableRaftConfiguration <$> (traverse node (Text.words finalText) >>= checked . raftVoterSet)
        pure (Just (VoterCheckpointBinding leader predecessor final))
      _ -> fail "invalid checkpoint binding"
    pure (interrupt, phase, binding)
  [] -> fail "empty checkpoint arm"
  where
    node text = checked (decodeIdentity text) >>= checked . mkRaftNodeId
    number text = maybe (fail "invalid checkpoint native position") pure (readMaybe (Text.unpack text))

runCheckpointHerald :: FilePath -> String -> FilePath -> IO ()
runCheckpointHerald manifest ordinalText directory = do
  hSetBuffering stdout LineBuffering
  ordinal <- maybe (fail "invalid checkpoint Herald ordinal") pure (readMaybe ordinalText)
  plan <- Bytes.readFile manifest >>= checked . decodeDeployment
  progress <- newMVar initialVoterCheckpointProgress
  let checkpoint event = do
        armed <- doesFileExist (voterArmPath directory ordinal)
        when armed $ do
          (interrupt, phase, binding) <- Text.readFile (voterArmPath directory ordinal) >>= readArm
          case observeCheckpoint event of
            Nothing -> pure ()
            Just observation -> do
              inject <- modifyMVar progress $ \before -> do
                let after = advanceVoterCheckpoint phase binding observation before
                    newlyDecided = voterCheckpointOutcome before == Nothing
                    evidence = Text.unlines [Text.pack (show event), Text.pack (show after)]
                Text.appendFile (directory </> ("voter-observations-" <> show ordinal <> ".log")) evidence
                case voterCheckpointOutcome after of
                  Just (VoterCheckpointFired actualLeader) | newlyDecided -> do
                    writeWitness
                      (voterFiredPath directory ordinal)
                      (Text.unlines [Text.pack (voterPhaseName phase), "leader " <> encodeIdentity (raftNodeIdBytes actualLeader), Text.pack (show event)])
                    pure (after, interrupt)
                  Just VoterCheckpointMissed | newlyDecided -> do
                    writeWitness (voterMissedPath directory ordinal) evidence
                    pure (after, False)
                  _ -> pure (after, False)
              when inject (ioError (userError ("injected Oracle role loss at " <> voterPhaseName phase)))
  withDeploymentResidentOracle (configureOracleRuntimeVoterCheckpoint checkpoint) plan (fromIntegral ordinal) $ \resident -> do
    Text.putStrLn ("system " <> encodeIdentity (deploymentSystemBytes plan))
    mapM_ Text.putStrLn (deploymentEndpointLines resident)
    case deploymentLauncherArtifact resident of
      Nothing -> pure ()
      Just descriptor -> Text.writeFile (directory </> "launcher.connection") (descriptor <> "\n")
    putStrLn "ready"
    awaitDeploymentExit resident

observeCheckpoint :: OracleVoterCheckpoint -> Maybe VoterCheckpointObservation
observeCheckpoint = \case
  OracleVoterPreparationCaptured frontier -> Just (VoterPreparationObserved (raftCatchUpFrontierLeader frontier) (raftCatchUpFrontierConfiguration frontier))
  OracleVoterConfigurationAppended configuration _ -> Just (VoterAppendObserved configuration)
  OracleVoterConfigurationCommitted configuration _ -> Just (VoterCommitObserved configuration)
  _ -> Nothing

matchesPhase :: VoterFailurePhase -> VoterCheckpointObservation -> Bool
matchesPhase phase event = case (phase, event) of
  (VoterPreparing, VoterPreparationObserved {}) -> True
  (VoterJointAppended, VoterAppendObserved configuration) -> joint configuration
  (VoterJointCommitted, VoterCommitObserved configuration) -> joint configuration
  (VoterFinalAppended, VoterAppendObserved configuration) -> not (joint configuration)
  (VoterFinalCommitted, VoterCommitObserved configuration) -> not (joint configuration)
  _ -> False
  where
    joint configuration = case raftVotingConfigurationView configuration of
      JointRaftConfigurationView {} -> True
      StableRaftConfigurationView {} -> False

writeWitness :: FilePath -> Text.Text -> IO ()
writeWitness path evidence = do
  Text.writeFile (path <> ".pending") evidence
  renameFile (path <> ".pending") path

checked :: (Show problem) => Either problem value -> IO value
checked = either (fail . show) pure
