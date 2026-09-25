{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Exception
  ( IOException,
    bracket,
    catch,
    onException,
  )
import Control.Monad (forM_, unless, void)
import Data.Word (Word16)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    hClose,
    hFlush,
    hGetLine,
    hPutStrLn,
    hSetBuffering,
  )
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe, Inherit),
    createProcess,
    getProcessExitCode,
    proc,
    terminateProcess,
    waitForProcess,
  )
import System.Timeout (timeout)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )
import Text.Read (readMaybe)

data NodeRole = H1 | H2 | H3
  deriving stock (Eq, Show)

data PeerEndpoint = PeerEndpoint String Word16

data ChildNode = ChildNode
  { childRole :: NodeRole,
    childControl :: Handle,
    childEvents :: Handle,
    childProcess :: ProcessHandle,
    childPeerEndpoint :: PeerEndpoint
  }

main :: IO ()
main =
  defaultMain
    ( testGroup
        "eclips-herald-runtime process gate"
        [testCase "partial seeds establish a direct learned H1/H3 binding and drain cleanly" caseThreeProcesses]
    )

caseThreeProcesses :: Assertion
caseThreeProcesses = do
  executable <- findExecutable "eclips-herald-slice1-node"
  case executable of
    Nothing -> assertFailure "Cabal did not place eclips-herald-slice1-node on the test PATH"
    Just nodeExecutable -> do
      observed <- timeout 20000000 (runThreeProcesses nodeExecutable)
      case observed of
        Nothing -> assertFailure "the three-process direct-binding gate timed out"
        Just () -> pure ()

runThreeProcesses :: FilePath -> IO ()
runThreeProcesses executable =
  bracket
    (startNode executable H3 Nothing)
    cleanupNode
    ( \h3 ->
        bracket
          (startNode executable H2 (Just h3.childPeerEndpoint))
          cleanupNode
          ( \h2 -> do
              awaitPeer h2 H3 H2
              awaitPeer h3 H2 H2
              bracket
                (startNode executable H1 (Just h2.childPeerEndpoint))
                cleanupNode
                ( \h1 -> do
                    awaitPeer h1 H2 H1
                    awaitPeer h1 H3 H1
                    awaitPeer h3 H1 H1
                    forM_ [h1, h2, h3] requestDrain
                    forM_ [h1, h2, h3] awaitDrained
                    forM_ [h1, h2, h3] awaitCleanExit
                )
          )
    )

startNode :: FilePath -> NodeRole -> Maybe PeerEndpoint -> IO ChildNode
startNode executable role seed = do
  let arguments = show role : maybe [] renderSeedArguments seed
      specification =
        (proc executable arguments)
          { std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = Inherit
          }
  created <- createProcess specification
  case created of
    (Just control, Just events, Nothing, process) -> do
      hSetBuffering control LineBuffering
      hSetBuffering events LineBuffering
      let partial = ChildNode role control events process (PeerEndpoint "" 0)
      ready <- readReady partial `onException` cleanupNode partial
      pure partial {childPeerEndpoint = ready}
    _ -> assertFailure "the child process did not supply its requested control pipes"

readReady :: ChildNode -> IO PeerEndpoint
readReady child = do
  message <- hGetLine child.childEvents
  case words message of
    ["READY", roleText, applicationText, peerText]
      | roleText == show child.childRole -> do
          _ <- parseEndpoint "application" applicationText
          parseEndpoint "peer" peerText
    _ -> assertFailure ("unexpected readiness message from " <> show child.childRole <> ": " <> message)

renderSeedArguments :: PeerEndpoint -> [String]
renderSeedArguments (PeerEndpoint host port) = ["--seed", host, show port]

parseEndpoint :: String -> String -> IO PeerEndpoint
parseEndpoint label encoded = case break (== ':') encoded of
  (host, ':' : portText)
    | not (null host),
      Just port <- readMaybe portText,
      port /= 0 ->
        pure (PeerEndpoint host port)
  _ -> assertFailure ("invalid " <> label <> " readiness endpoint: " <> encoded)

awaitPeer :: ChildNode -> NodeRole -> NodeRole -> IO ()
awaitPeer child remote initiator = do
  sendCommand child ("await-peer " <> show remote <> " " <> show initiator)
  expectEvent child ("PEER " <> show child.childRole <> " " <> show remote <> " " <> show initiator)

requestDrain :: ChildNode -> IO ()
requestDrain child = sendCommand child "drain"

awaitDrained :: ChildNode -> IO ()
awaitDrained child = expectEvent child ("SUMMARY " <> show child.childRole <> " DRAINED")

sendCommand :: ChildNode -> String -> IO ()
sendCommand child command = do
  hPutStrLn child.childControl command
  hFlush child.childControl

expectEvent :: ChildNode -> String -> IO ()
expectEvent child expected = do
  actual <- hGetLine child.childEvents
  assertEqual ("event from " <> show child.childRole) expected actual

awaitCleanExit :: ChildNode -> IO ()
awaitCleanExit child = do
  exit <- waitForProcess child.childProcess
  assertEqual (show child.childRole <> " process exit") ExitSuccess exit

cleanupNode :: ChildNode -> IO ()
cleanupNode child = do
  ignoreIOException (hClose child.childControl)
  status <- getProcessExitCode child.childProcess
  unless (status /= Nothing) (terminateProcess child.childProcess)
  void (waitForProcess child.childProcess)
  ignoreIOException (hClose child.childEvents)

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `catch` ignore
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()
