{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.STM
  ( atomically,
    readTVar,
    retry,
  )
import Control.Monad (unless)
import Data.Text qualified as Text
import Data.Word (Word16)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Discovery
  ( peerBindingRemoteHeraldEpoch,
    peerBindingSelectedCandidate,
    peerCandidateInitiatorHeraldEpoch,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Runtime
  ( HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    heraldRuntimeConfiguration,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission (..))
import Eclips.Herald.Runtime.TCP
  ( HeraldTcpFailure,
    ResolvedTcpEndpoint,
    awaitHeraldTcpExit,
    configuredPeerSeed,
    heartbeatConfigurationMicroseconds,
    heraldTcpApplicationEndpoint,
    heraldTcpConfiguration,
    heraldTcpEndpoints,
    heraldTcpPeerEndpoint,
    peerDialRetryDelayMicroseconds,
    requestHeraldTcpDrain,
    resolvedTcpHost,
    resolvedTcpPort,
    tcpListenEndpoint,
    withHeraldTcpRuntime,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeraldTcp (..),
    TcpContext (..),
  )
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (..))
import System.Environment (getArgs)
import System.Exit (die)
import System.IO
  ( BufferMode (LineBuffering),
    hFlush,
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
  )
import Text.Read (readMaybe)
import ThreeHeraldFixtures
  ( threeH1Bootstraps,
    threeH1GeneratorSeedSource,
    threeH1Genesis,
    threeH1HeraldEpoch,
    threeH2Bootstraps,
    threeH2GeneratorSeedSource,
    threeH2Genesis,
    threeH2HeraldEpoch,
    threeH3Bootstraps,
    threeH3GeneratorSeedSource,
    threeH3Genesis,
    threeH3HeraldEpoch,
    threeOracleContacts,
  )

data NodeRole
  = H1
  | H2
  | H3
  deriving stock (Eq, Show)

data NodeInvocation = NodeInvocation NodeRole (Maybe (Text.Text, Word16))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  invocation <- either die pure . parseInvocation =<< getArgs
  runNode invocation

runNode :: NodeInvocation -> IO ()
runNode (NodeInvocation role seedEndpoint) = do
  let (genesis, bootstraps, generatorSeedSource) = fixtureFor role
      listener = checked "listener" (tcpListenEndpoint "127.0.0.1" 0)
      seeds = case seedEndpoint of
        Nothing -> []
        Just (host, port) -> [checked "configured seed" (configuredPeerSeed host port)]
      configuration =
        heraldTcpConfiguration
          (runtimeConfiguration genesis bootstraps generatorSeedSource)
          listener
          listener
          listener
          seeds
          (checked "peer retry delay" (peerDialRetryDelayMicroseconds 1000))
          (checked "heartbeat" (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))
  result <- withHeraldTcpRuntime configuration (runControl role)
  case result of
    Left failure -> die (renderTcpFailure failure)
    Right ((), HeraldRuntimeDrained) -> pure ()
    Right ((), exit) -> die ("node scope ended without semantic drain: " <> show exit)

runControl :: NodeRole -> HeraldTcp -> IO ()
runControl role tcp = do
  reportReady role tcp
  loop
  where
    loop = do
      command <- words <$> getLine
      case command of
        ["await-peer", targetText, initiatorText] -> do
          target <- either (ioError . userError) pure (parseRole targetText)
          initiator <- either (ioError . userError) pure (parseRole initiatorText)
          awaitCurrentPeer tcp (roleEpoch target) (roleEpoch initiator)
          putProtocol ("PEER " <> show role <> " " <> show target <> " " <> show initiator)
          loop
        ["drain"] -> do
          submission <- requestHeraldTcpDrain tcp (adminCorrelationId 1)
          unless (submission == Queued)
            $ ioError
            $ userError
            $ "drain was not admitted: " <> show submission
          exit <- awaitHeraldTcpExit tcp
          case exit of
            Left failure -> ioError (userError (renderTcpFailure failure))
            Right HeraldRuntimeDrained -> putProtocol ("SUMMARY " <> show role <> " DRAINED")
            Right other -> ioError (userError ("unexpected runtime exit: " <> show other))
        _ -> ioError (userError ("unknown control command: " <> unwords command))

reportReady :: NodeRole -> HeraldTcp -> IO ()
reportReady role tcp =
  putProtocol
    ( unwords
        [ "READY",
          show role,
          renderEndpoint applicationEndpoint,
          renderEndpoint peerEndpoint
        ]
    )
  where
    endpoints = heraldTcpEndpoints tcp
    applicationEndpoint = heraldTcpApplicationEndpoint endpoints
    peerEndpoint = heraldTcpPeerEndpoint endpoints

renderEndpoint :: ResolvedTcpEndpoint -> String
renderEndpoint endpoint =
  Text.unpack (resolvedTcpHost endpoint) <> ":" <> show (resolvedTcpPort endpoint)

putProtocol :: String -> IO ()
putProtocol message = putStrLn message >> hFlush stdout

awaitCurrentPeer :: HeraldTcp -> HeraldEpoch -> HeraldEpoch -> IO ()
awaitCurrentPeer (HeraldTcp _ _ _ context) remoteEpoch initiatorEpoch =
  atomically $ do
    current <- readTVar context.tcpCurrentPeerConnections
    unless
      (any (matches . fst) current)
      retry
  where
    matches binding =
      peerBindingRemoteHeraldEpoch binding == remoteEpoch
        && peerCandidateInitiatorHeraldEpoch (peerBindingSelectedCandidate binding) == initiatorEpoch

-- This executable is the fixed three-Herald transport fixture. Its deliberately
-- long heartbeat windows and explicit recovery values isolate transport tests;
-- ordinary deployment uses configureHeraldTcpTiming and the takeover target.
runtimeConfiguration ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  HeraldRuntimeConfiguration
runtimeConfiguration genesis bootstraps seedSource =
  heraldRuntimeConfiguration
    genesis
    bootstraps
    threeOracleContacts
    seedSource
    systemRuntimeMonotonicClock
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (hPutStrLn stderr . ("runtime: " <>) . show))
    (checked "application recovery" (checkApplicationRecoveryConfiguration 30_000_000))
    (checked "peer recovery" (checkPeerRecoveryConfiguration 30_000_000))

fixtureFor :: NodeRole -> (CheckedHeraldGenesis, CheckedInitialBootstraps, RuntimeGeneratorSeedSource)
fixtureFor role = case role of
  H1 -> (threeH1Genesis, threeH1Bootstraps, threeH1GeneratorSeedSource)
  H2 -> (threeH2Genesis, threeH2Bootstraps, threeH2GeneratorSeedSource)
  H3 -> (threeH3Genesis, threeH3Bootstraps, threeH3GeneratorSeedSource)

roleEpoch :: NodeRole -> HeraldEpoch
roleEpoch role = case role of
  H1 -> threeH1HeraldEpoch
  H2 -> threeH2HeraldEpoch
  H3 -> threeH3HeraldEpoch

parseInvocation :: [String] -> Either String NodeInvocation
parseInvocation arguments = case arguments of
  ["H3"] -> Right (NodeInvocation H3 Nothing)
  [roleText, "--seed", host, portText] -> do
    role <- parseRole roleText
    if role == H3
      then Left usage
      else case readMaybe portText of
        Nothing -> Left usage
        Just port -> Right (NodeInvocation role (Just (Text.pack host, port)))
  _ -> Left usage

parseRole :: String -> Either String NodeRole
parseRole value = case value of
  "H1" -> Right H1
  "H2" -> Right H2
  "H3" -> Right H3
  _ -> Left ("unknown fixture role: " <> value)

renderTcpFailure :: HeraldTcpFailure -> String
renderTcpFailure failure = "Herald TCP failed: " <> show failure

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

usage :: String
usage =
  "usage: eclips-herald-slice1-node H3 | "
    <> "eclips-herald-slice1-node (H1|H2) --seed HOST PORT"
