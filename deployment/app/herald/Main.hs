{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString qualified as Bytes
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Eclips.Deployment.Admin (encodeIdentity)
import Eclips.Deployment.Configuration
import Eclips.Deployment.Joining
import Eclips.Deployment.Manifest
import Eclips.Deployment.Runtime
import Eclips.Deployment.Timing
import System.Entropy (getEntropy)
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  arguments <- getArgs
  if arguments == ["--help"] then putStrLn usage else validateArguments arguments >> run arguments

run :: [String] -> IO ()
run arguments
  | Just path <- option "--write-fixed-manifest" arguments = do
      endpoints <- traverse parseMember (options "--member" arguments)
      members <- maybe (fail "at least one --member HOST:BASE is required") pure (NonEmpty.nonEmpty endpoints)
      seed <- getEntropy 32
      target <- selectedTiming arguments
      manifest <- checked (checkDeploymentWithTiming target seed members)
      Bytes.writeFile path (encodeDeployment manifest)
      Text.putStrLn ("system " <> encodeIdentity (deploymentSystemBytes manifest))
  | not (null (options "--peer" arguments)) = do
      endpoints <- localEndpoints arguments
      seeds <- traverse (checked . parseEndpoint . Text.pack) (options "--peer" arguments)
      nonempty <- maybe (fail "a --peer seed is required") pure (NonEmpty.nonEmpty seeds)
      entropy <- getEntropy 32
      withJoiningDeployment entropy endpoints nonempty $ \resident -> do
        Text.putStrLn ("system " <> encodeIdentity (deploymentSystemIdentity resident))
        mapM_ Text.putStrLn (deploymentEndpointLines resident)
        putStrLn "ready"
        awaitDeploymentExit resident
  | otherwise = do
      (manifest, member) <- case option "--fixed-manifest" arguments of
        Just path -> do
          plan <- Bytes.readFile path >>= checked . decodeDeployment
          ordinal <- maybe (fail "--member ORDINAL is required with --fixed-manifest") readNumber (option "--member" arguments)
          pure (plan, ordinal)
        Nothing
          | "--bootstrap" `elem` arguments -> do
              endpoints <- localEndpoints arguments
              seed <- getEntropy 32
              target <- selectedTiming arguments
              plan <- checked (checkDeploymentWithTiming target seed (NonEmpty.singleton endpoints))
              pure (plan, 1)
          | otherwise -> fail usage
      withDeploymentResident manifest member $ \resident -> do
        Text.putStrLn ("system " <> encodeIdentity (deploymentSystemBytes manifest))
        mapM_ Text.putStrLn (deploymentEndpointLines resident)
        case option "--launcher-output" arguments of
          Nothing -> pure ()
          Just path -> case deploymentLauncherArtifact resident of
            Nothing -> fail "only the founder resident has an initial launcher"
            Just descriptor -> Text.writeFile path (descriptor <> "\n")
        putStrLn "ready"
        awaitDeploymentExit resident
  where
    parseMember value = do
      endpointValue <- checked (parseEndpoint (Text.pack value))
      checked (deploymentEndpoints (endpointHost endpointValue) (endpointPort endpointValue) Nothing)

localEndpoints :: [String] -> IO DeploymentEndpoints
localEndpoints arguments = do
  let host = Text.pack (fromMaybe "127.0.0.1" (option "--bind" arguments))
  base <- maybe (fail "--base-port PORT is required") (fmap endpointPort . checked . parseEndpoint . ("host:" <>) . Text.pack) (option "--base-port" arguments)
  shorthand <- checked (deploymentEndpoints host base (Text.pack <$> option "--advertise" arguments))
  overridePlanes shorthand arguments

overridePlanes :: DeploymentEndpoints -> [String] -> IO DeploymentEndpoints
overridePlanes base arguments = do
  peer <- listen "peer" (peerListen base)
  application <- listen "application" (applicationListen base)
  administration <- listen "administration" (administrationListen base)
  oracle <- listen "oracle" (oracleListen base)
  raft <- listen "raft" (raftListen base)
  applicationContact <- advertise "application" (applicationAdvertised base)
  peerContact <- advertise "peer" (peerAdvertised base)
  administrationContact <- advertise "administration" (administrationAdvertised base)
  oracleContact <- advertise "oracle" (oracleAdvertised base)
  raftContact <- advertise "raft" (raftAdvertised base)
  checked (deploymentEndpointsFromPlanes peer application administration oracle raft applicationContact peerContact administrationContact oracleContact raftContact)
  where
    listen plane fallback = maybe (pure fallback) (checked . parseEndpoint . Text.pack) (option ("--" <> plane <> "-listen") arguments)
    advertise plane fallback = maybe (pure fallback) (fmap Just . checked . parseEndpoint . Text.pack) (option ("--" <> plane <> "-advertise") arguments)

validateArguments :: [String] -> IO ()
validateArguments supplied = do
  seen <- go [] supplied
  mode <- case filter (`elem` seen) ["--bootstrap", "--peer", "--fixed-manifest", "--write-fixed-manifest"] of
    [selected] -> pure selected
    _ -> fail "choose exactly one startup mode"
  let allowed = case mode of
        "--bootstrap" -> [mode, "--bind", "--base-port", "--advertise", "--launcher-output", "--takeover-target"] <> planeOptions
        "--peer" -> [mode, "--bind", "--base-port", "--advertise"] <> planeOptions
        "--fixed-manifest" -> [mode, "--member", "--launcher-output"]
        _ -> [mode, "--member", "--takeover-target"]
  if all (`elem` allowed) seen then pure () else fail "argument is not supported by the selected startup mode"
  where
    planeOptions = ["--" <> plane <> suffix | plane <- ["peer", "application", "administration", "oracle", "raft"], suffix <- ["-listen", "-advertise"]]
    valueOptions = ["--bind", "--base-port", "--advertise", "--launcher-output", "--fixed-manifest", "--write-fixed-manifest", "--member", "--peer", "--takeover-target"] <> planeOptions
    go seen [] = pure seen
    go seen ("--bootstrap" : rest) | "--bootstrap" `notElem` seen = go ("--bootstrap" : seen) rest
    go seen (key : value : rest)
      | key `elem` valueOptions,
        key `notElem` seen || key == "--peer" || key == "--member" && "--write-fixed-manifest" `elem` supplied,
        take 2 value /= "--" =
          go (key : seen) rest
    go _ _ = fail "unknown, repeated, or incomplete deployment argument; use --help"

selectedTiming :: [String] -> IO TakeoverTarget
selectedTiming arguments = maybe (pure defaultTakeoverTarget) (checked . parseTakeoverTarget . Text.pack) (option "--takeover-target" arguments)

option :: String -> [String] -> Maybe String
option key arguments = case options key arguments of [] -> Nothing; value : _ -> Just value
options :: String -> [String] -> [String]
options key = \case
  name : value : rest | name == key -> value : options key rest
  _ : rest -> options key rest
  [] -> []
readNumber :: (Read number) => String -> IO number
readNumber = maybe (fail "invalid numeric argument") pure . readMaybe
checked :: (Show problem) => Either problem value -> IO value
checked = either (fail . show) pure
usage :: String
usage =
  unlines
    [ "eclips-herald --bootstrap --bind HOST --base-port PORT [--advertise HOST] [--launcher-output PATH] [--takeover-target 5s]",
      "eclips-herald --peer HOST:PORT [--peer HOST:PORT ...] --bind HOST --base-port PORT [--advertise HOST]",
      "  Joiners and fixed-manifest residents inherit the run takeover target.",
      "  Explicit planes: --{peer,application,administration,oracle,raft}-listen HOST:PORT",
      "  Advertisements: --{peer,application,administration,oracle,raft}-advertise HOST:PORT",
      "eclips-herald --write-fixed-manifest PATH --member HOST:BASE [--member HOST:BASE ...] [--takeover-target 5s]",
      "eclips-herald --fixed-manifest PATH --member ORDINAL [--launcher-output PATH]"
    ]
