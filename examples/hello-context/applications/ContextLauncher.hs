{-# LANGUAGE OverloadedStrings #-}

-- | Parent-side graph construction and ordinary child-process supervision.
-- The publisher and reader themselves live in ContextApplication.
module ContextLauncher (runLauncher) where

import ContextApplication (expect)
import ContextArguments (locatorText, readLocator)
import ContextVocabulary
import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, readMVar)
import Control.Exception (IOException, SomeException, mask, onException, throwIO, try)
import Control.Monad (forM_, unless, void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text qualified as Text
import Eclips.Application.Advanced qualified as Advanced
import Eclips.Application.Typed qualified as App
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToProcess), LabelResult (LabelApplied))
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.Value (ApplicationLabelOwner (ProcessLabel))
import System.Exit (ExitCode (ExitSuccess))
import System.IO (Handle, hFlush, hGetLine, hIsEOF, hPutStrLn, stdin)
import System.Process
  ( CreateProcess (std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe),
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
    withCreateProcess,
  )

data Holder = Holder HeraldLocator Handle Handle ProcessHandle

runLauncher :: FilePath -> App.ConnectionDescriptor -> NonEmpty HeraldLocator -> Bool -> IO ()
runLauncher executable descriptor locators once = App.withHerald descriptor workflow >>= expect
  where
    workflow herald = do
      environment <- expect (App.startupEnvironment herald)
      sort <- expect (App.compileSort @Message)
      App.declareSort environment sort >>= expect
      deltas <- traverse (const (App.createDelta environment sort >>= expect)) locators
      let edge source destination = void (App.createEdge environment App.Preserve source destination >>= expect)
      forM_ (contextEdges (fmap App.deltaVertex deltas)) (uncurry edge)
      let holderInputs = zip (NonEmpty.toList locators) (NonEmpty.toList deltas)
      withHolders executable herald holderInputs $ \holders -> do
        writer <- App.createNabla environment sort Nothing >>= expect
        edge (App.nablaVertex writer) (App.deltaVertex (NonEmpty.head deltas))
        selection <- endpointSelection (Access.Writer (App.nablaId writer))
        withPrepared herald (connectionDescriptorLocator descriptor) selection (App.nablaVertex writer) $ \publisher -> do
          runChild executable "publisher" (preparedChildConnection publisher) "published\n"
          putStrLn "publisher exited"
        -- Observe the exact greeting in every independent holder before offering
        -- the crash experiment. Successful publication alone is not this barrier.
        forM_ holders $ \(Holder locator _ output _) -> do
          line <- hGetLine output
          unless (line == "stored") (fail ("context holder: " <> line))
          putStrLn ("context stored greeting on " <> locatorText locator)
        putStrLn "context ready; publisher exited"
        let readAt locator = do
              -- This delta does not exist until after publisher process exit.
              -- Its store can obtain the greeting only through context alignment.
              reader <- App.createDelta environment sort >>= expect
              edge (App.deltaVertex (NonEmpty.head deltas)) (App.deltaVertex reader)
              readerSelection <- endpointSelection (Access.Reader (App.deltaId reader))
              withPrepared herald locator readerSelection (App.deltaVertex reader) $ \child ->
                runChild executable "reader" (preparedChildConnection child) "Hello, world!\n"
              putStrLn "Hello, world!"
              putStrLn "reader completed"
        if once then readAt (NonEmpty.head locators) else commandLoop readAt
        -- Send every stop before waiting for any End. The context remains in its
        -- holders until the user quits; fresh reader exit never stops a holder.
        forM_ holders $ \(Holder _ input _ _) ->
          ignoreClosedPipe (hPutStrLn input "stop" >> hFlush input)
        forM_ holders $ \(Holder locator _ _ process) -> do
          exitCode <- waitForProcess process
          unless (exitCode == ExitSuccess) (fail ("context holder on " <> locatorText locator <> " failed: " <> show exitCode))
        App.endProcess herald >>= expect
        putStrLn "context stopped"

endpointSelection :: Access.PrimordialEntry -> IO Access.PrimordialSelection
endpointSelection entry = expect (Access.primordialSelection [(key, entry)] (Set.singleton key) Nothing)
  where
    key = case entry of Access.Writer _ -> "messages.writer"; _ -> "messages.reader"

-- Keep the accepted preparation reference even if the awaiting thread is
-- interrupted. A failed OS launch cancels that exact preparation, never a retry
-- under a fresh identity. Successfully attached children own their lifetimes.
withPrepared :: App.Herald -> HeraldLocator -> Access.PrimordialSelection -> App.Vertex -> (PreparedChild -> IO value) -> IO value
withPrepared herald locator selection endpoint use = mask $ \restore -> do
  submitted <- newEmptyMVar
  _ <- forkFinally (Advanced.submitLifecycle herald (Advanced.BeginChild locator selection)) (putMVar submitted)
  let reference = readMVar submitted >>= either throwIO pure >>= Advanced.awaitLifecycle >>= expect
      cancel = void (try @SomeException (reference >>= App.cancelChild herald >>= expect))
  restore
    ( do
        preparation <- reference
        child <- Advanced.submitLifecycle herald (Advanced.AwaitPreparedChild preparation) >>= Advanced.awaitLifecycle >>= expect
        let parent = Access.startupAccessProcess (App.startup herald)
        result <- App.labelVertex endpoint (ProcessLabel parent, 0) (LabelToProcess (preparedChildProcess child)) >>= expect
        unless (result == LabelApplied) (fail ("endpoint transfer: " <> show result))
        App.awaitChildReady herald child >>= expect
        use child
    )
    `onException` cancel

withHolders :: FilePath -> App.Herald -> [(HeraldLocator, App.Delta Message)] -> ([Holder] -> IO value) -> IO value
withHolders _ _ [] use = use []
withHolders executable herald ((locator, delta) : rest) use = do
  selection <- endpointSelection (Access.Reader (App.deltaId delta))
  withPrepared herald locator selection (App.deltaVertex delta) $ \child ->
    withCreateProcess
      (childProcess executable "holder" (preparedChildConnection child)) {std_in = CreatePipe, std_out = CreatePipe}
      $ \input output _ process -> case (input, output) of
        (Just commands, Just messages) ->
          withHolders executable herald rest $ \holders -> use (Holder locator commands messages process : holders)
        _ -> fail "context holder pipes were not created"

runChild :: FilePath -> String -> App.ConnectionDescriptor -> String -> IO ()
runChild executable role descriptor expected = do
  (exitCode, output, diagnostic) <- readCreateProcessWithExitCode (childProcess executable role descriptor) ""
  unless (exitCode == ExitSuccess && output == expected) (fail (role <> " failed: " <> show exitCode <> " " <> output <> diagnostic))

childProcess :: FilePath -> String -> App.ConnectionDescriptor -> CreateProcess
childProcess executable role descriptor = proc executable [role, "--connection", Text.unpack (App.encodeConnectionDescriptor descriptor)]

commandLoop :: (HeraldLocator -> IO ()) -> IO ()
commandLoop readAt = do
  putStrLn "commands: read HOST:PORT | quit"
  loop
  where
    loop = do
      eof <- hIsEOF stdin
      unless eof $ do
        command <- words <$> getLine
        case command of
          ["quit"] -> pure ()
          ["read", target] -> readLocator target >>= readAt >> loop
          [] -> loop
          _ -> putStrLn "expected read HOST:PORT or quit" >> loop

ignoreClosedPipe :: IO () -> IO ()
ignoreClosedPipe action = void (try @IOException action)
