{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, onException)
import Control.Monad (forM_, void)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Network.Socket
import System.Directory hiding (executable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO
import System.Process hiding (system)
import System.Timeout (timeout)
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))

main :: IO ()
main =
  defaultMain
    $
    -- Endpoint reservations are released immediately before starting residents;
    -- keep complete deployments serial to avoid recycling a sibling's ports.
    localOption (NumThreads 1)
    $ testGroup
      "independent greeting context applications"
      [ testCase "publisher exits before the singleton reader starts" caseSingleton,
        testCase "a fresh reader obtains context after one of three voters crashes" caseVoterCrash
      ]

caseSingleton :: Assertion
caseSingleton = withWorkspace $ \directory -> within directory $ do
  herald <- executable "eclips-herald"
  application <- executable "eclips-hello-context"
  residents <- freshResidents 1
  withResidents herald directory residents $ \_ descriptor _system -> do
    let logPath = directory </> "hello-context.log"
    withLoggedProcess logPath (proc application ["--connection-file", descriptor, "--context-herald", applicationAddress (headResident residents), "--once"]) $ \_ process -> do
      waitForProcess process >>= assertEqual "one-shot tutorial exits successfully" ExitSuccess
      output <- Text.readFile logPath
      assertGreetingOrder residents 1 output

caseVoterCrash :: Assertion
caseVoterCrash = withWorkspace $ \directory -> within directory $ do
  herald <- executable "eclips-herald"
  administrator <- executable "eclips-admin"
  application <- executable "eclips-hello-context"
  residents <- freshResidents 3
  withResidents herald directory residents $ \processes descriptor system ->
    case (residents, processes) of
      ([first, second, third], [firstProcess, secondProcess, thirdProcess]) -> do
        let admin planes = runAdmin directory administrator system planes
        initial <- awaitStableConfiguration admin [first, second, third] 1
        forM_ residents $ \planes -> do
          receipt <- admin planes ["prepare-oracle"]
          assertBool ("replica preparation rejected:\n" <> Text.unpack receipt) ("receipt-result OracleAccepted" `Text.isInfixOf` receipt)
        registrations <- awaitRegistrations admin first 3
        receipt <- admin first (["commission-voters", Text.unpack initial] <> map Text.unpack registrations)
        assertBool ("voter commissioning rejected:\n" <> Text.unpack receipt) ("receipt-result OracleAccepted" `Text.isInfixOf` receipt)
        _ <- awaitStableConfiguration admin residents 3
        let logPath = directory </> "hello-context.log"
            arguments = ["--connection-file", descriptor] <> concatMap (\planes -> ["--context-herald", applicationAddress planes]) residents
        withLoggedProcess logPath (proc application arguments) {std_in = CreatePipe} $ \input process -> do
          command <- maybe (assertFailure "tutorial stdin pipe was not created") pure input
          awaitLine process logPath "context ready; publisher exited"
          before <- Text.readFile logPath
          assertBool "no reader has printed the greeting before the publisher exits and the crash is injected" ("Hello, world!" `notElem` Text.lines before)
          -- The readiness marker follows observations at every holder, so this
          -- crash cannot race an unobserved initial context transfer.
          terminateProcess thirdProcess
          void (waitForProcess thirdProcess)
          _ <- awaitStableConfiguration admin [first, second] 2
          forM_ [firstProcess, secondProcess] $ \survivor -> getProcessExitCode survivor >>= assertEqual "surviving Herald remains running" Nothing
          hPutStrLn command ("read " <> applicationAddress second)
          hFlush command
          awaitLine process logPath "reader completed"
          -- Completion is printed only after the first reader has explicitly
          -- ended its logical process and its OS process has exited. A second
          -- reader must still obtain the greeting from the retained context.
          hPutStrLn command ("read " <> applicationAddress first)
          hFlush command
          awaitLineCount process logPath "reader completed" 2
          hPutStrLn command "quit"
          hFlush command
          waitForProcess process >>= assertEqual "tutorial cleans up after the host crash" ExitSuccess
          output <- Text.readFile logPath
          assertGreetingOrder residents 2 output
      _ -> assertFailure "three-voter fixture has exactly three residents"

assertGreetingOrder :: [ResidentEndpoints] -> Int -> Text -> Assertion
assertGreetingOrder residents expectedReaders output = do
  let linesSeen = Text.lines output
      positions line = [index | (index, value) <- zip [0 :: Int ..] linesSeen, value == line]
  case (positions "publisher exited", positions "context ready; publisher exited", positions "Hello, world!", positions "reader completed") of
    ([publisherExit], [ready], greetings, completions) -> do
      assertBool "the readiness marker follows publisher process exit" (publisherExit < ready)
      forM_ residents $ \planes ->
        case positions ("context stored greeting on " <> Text.pack (applicationAddress planes)) of
          [stored] -> assertBool "every holder confirms retained context before crash readiness" (stored < ready)
          _ -> assertFailure ("missing holder observation for " <> applicationAddress planes <> "\n" <> Text.unpack output)
      assertEqual "each fresh reader prints one greeting" expectedReaders (length greetings)
      assertEqual "each fresh reader ends and exits once" expectedReaders (length completions)
      forM_ (zip greetings completions) $ \(greeting, completed) -> do
        assertBool "the publisher exits before each fresh reader obtains the greeting" (ready < greeting)
        assertBool "each reader ends and exits after printing its greeting" (greeting < completed)
      forM_ (zip completions (drop 1 greetings)) $ \(completed, nextGreeting) ->
        assertBool "the previous reader has ended and exited before the next reader obtains context" (completed < nextGreeting)
    _ -> assertFailure ("expected exactly one publication-exit and one readiness marker:\n" <> Text.unpack output)

-- Exercise the same public administration commands shown in the tutorial.
-- Snapshot text is inspected only for the stable configuration and replica
-- identities that those commands publish to an operator.
runAdmin :: FilePath -> FilePath -> String -> ResidentEndpoints -> [String] -> IO Text
runAdmin directory administrator system planes arguments = do
  (exitCode, output, diagnostic) <-
    readCreateProcessWithExitCode
      (proc administrator (["--endpoint", administrationAddress planes, "--system", system] <> arguments))
      ""
  Text.appendFile (directory </> "administration.log") (Text.pack (unwords arguments <> "\n" <> output <> diagnostic))
  assertEqual ("administration command failed: " <> unwords arguments <> "\n" <> diagnostic) ExitSuccess exitCode
  pure (Text.pack output)

awaitStableConfiguration :: (ResidentEndpoints -> [String] -> IO Text) -> [ResidentEndpoints] -> Int -> IO Text
awaitStableConfiguration admin residents expected = do
  snapshots <- traverse (\planes -> admin planes ["oracle-configuration"]) residents
  case traverse stable snapshots of
    Just ((ident, voters) : others)
      | all (== (ident, voters)) others -> pure ident
    _ -> threadDelay 50_000 >> awaitStableConfiguration admin residents expected
  where
    stable snapshot = do
      ident <- field "configuration-id" snapshot
      voters <- Text.words <$> field "voters" snapshot
      pending <- field "pending-change" snapshot
      if length voters == expected && pending == "none" then Just (ident, sort voters) else Nothing

awaitRegistrations :: (ResidentEndpoints -> [String] -> IO Text) -> ResidentEndpoints -> Int -> IO [Text]
awaitRegistrations admin planes expected = do
  snapshot <- admin planes ["oracle-configuration"]
  let registrations = [node | line <- Text.lines snapshot, ["replica", node, _] <- [Text.words line]]
  if length registrations == expected then pure registrations else threadDelay 50_000 >> awaitRegistrations admin planes expected

field :: Text -> Text -> Maybe Text
field name snapshot = case [value | line <- Text.lines snapshot, Just value <- [Text.stripPrefix (name <> " ") line]] of
  [value] -> Just value
  _ -> Nothing

-- The fixture uses exactly the public founder/seed startup path documented for
-- readers, with separate ephemeral ports for all five protocol planes.
data ResidentEndpoints = ResidentEndpoints
  { peerAddress :: String,
    applicationAddress :: String,
    administrationAddress :: String,
    oracleAddress :: String,
    raftAddress :: String
  }

freshResidents :: Int -> IO [ResidentEndpoints]
freshResidents count = freshEndpoints (5 * count) >>= assemble
  where
    assemble [] = pure []
    assemble (peer : application : administration : oracle : raft : rest) =
      (ResidentEndpoints peer application administration oracle raft :) <$> assemble rest
    assemble _ = assertFailure "incomplete five-plane endpoint allocation"

withResidents :: FilePath -> FilePath -> [ResidentEndpoints] -> ([ProcessHandle] -> FilePath -> String -> IO value) -> IO value
withResidents herald directory residents use =
  start 1 residents []
  where
    descriptor = directory </> "launcher.connection"
    first = headResident residents
    start _ [] reversed = do
      output <- Text.readFile (directory </> "herald-1.log")
      system <- maybe (assertFailure "founder did not print its system identity") (pure . Text.unpack) (field "system" output)
      use (reverse reversed) descriptor system
    start ordinal (planes : remaining) reversed = do
      let logPath = directory </> ("herald-" <> show ordinal <> ".log")
          startup = if ordinal == (1 :: Int) then ["--bootstrap", "--launcher-output", descriptor] else ["--peer", peerAddress first]
          arguments =
            startup
              <> ["--base-port", "0"]
              <> concatMap
                (\(name, value) -> ["--" <> name <> "-listen", value])
                [ ("peer", peerAddress planes),
                  ("application", applicationAddress planes),
                  ("administration", administrationAddress planes),
                  ("oracle", oracleAddress planes),
                  ("raft", raftAddress planes)
                ]
      withLoggedProcess logPath (proc herald arguments) $ \_ process -> do
        awaitLine process logPath "ready"
        start (ordinal + 1) remaining (process : reversed)

headResident :: [ResidentEndpoints] -> ResidentEndpoints
headResident (first : _) = first
headResident [] = error "the test requested an empty resident list"

-- Keep reservations alive together until every protocol plane has a distinct
-- OS-assigned port. Child process scopes own all listeners after that point.
freshEndpoints :: Int -> IO [String]
freshEndpoints 0 = pure []
freshEndpoints count = bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  current <-
    getSocketName listener >>= \case
      SockAddrInet port _ -> pure ("127.0.0.1:" <> show (fromIntegral port :: Int))
      _ -> assertFailure "expected an IPv4 loopback listener"
  remaining <- freshEndpoints (count - 1)
  pure (current : remaining)

withLoggedProcess :: FilePath -> CreateProcess -> (Maybe Handle -> ProcessHandle -> IO value) -> IO value
withLoggedProcess path command use = withFile path WriteMode $ \output ->
  bracket
    (createProcess command {std_out = UseHandle output, std_err = UseHandle output, create_group = True})
    cleanup
    (\(input, _, _, process) -> use input process)
  where
    cleanup (input, _, _, process) = do
      getProcessExitCode process >>= \case
        Just _ -> pure ()
        Nothing -> do
          interruptProcessGroupOf process
          stopped <- timeout 5_000_000 (waitForProcess process)
          case stopped of
            Just _ -> pure ()
            Nothing -> terminateProcess process >> void (waitForProcess process)
      forM_ input hClose

awaitLine :: ProcessHandle -> FilePath -> Text -> IO ()
awaitLine process path expected = awaitLineCount process path expected 1

awaitLineCount :: ProcessHandle -> FilePath -> Text -> Int -> IO ()
awaitLineCount process path expected count = do
  output <- Text.readFile path
  if length (filter (== expected) (Text.lines output)) >= count
    then pure ()
    else
      getProcessExitCode process >>= \case
        Just code -> assertFailure ("process exited while awaiting " <> Text.unpack expected <> ": " <> show code <> "\n" <> Text.unpack output)
        Nothing -> threadDelay 20_000 >> awaitLineCount process path expected count

within :: FilePath -> IO () -> Assertion
within directory action = do
  completed <- timeout 180_000_000 action
  assertBool ("context tour timed out; inspect " <> directory) (completed == Just ())

withWorkspace :: (FilePath -> IO value) -> IO value
withWorkspace use = do
  createDirectoryIfMissing True ".cabal"
  (path, temporary) <- openTempFile ".cabal" "hello-context-os-"
  hClose temporary
  removeFile path
  createDirectory path
  value <- use path `onException` putStrLn ("context tutorial artifacts retained at " <> path)
  removeDirectoryRecursive path
  pure value

executable :: String -> IO FilePath
executable name = findExecutable name >>= maybe (fail ("missing Cabal build tool " <> name)) pure
