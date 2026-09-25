{-# LANGUAGE OverloadedStrings #-}

-- | An ordinary launcher prepares and labels children before spawning OS work.
module HelloLauncher
  ( ChildExecutables (..),
    ChildOrder (..),
    runHelloLauncher,
  ) where

import Control.Concurrent (MVar, forkFinally, killThread, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (SomeException, bracket, evaluate, finally, mask, onException, throwIO, try)
import Control.Monad (forM_, unless, void)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Eclips.Application qualified as App
import Eclips.Application.Advanced qualified as Advanced
import Eclips.Application.Typed qualified as Typed
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToProcess), LabelResult (LabelApplied))
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.Value (ApplicationLabelOwner (ProcessLabel))
import HelloCommon
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hGetContents)
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (CreatePipe, NoStream), cleanupProcess, createProcess, proc, terminateProcess, waitForProcess)

data ChildExecutables = ChildExecutables FilePath FilePath
  deriving stock (Eq, Show)
data ChildOrder = PublisherFirst | ReaderFirst
  deriving stock (Eq, Show)

runHelloLauncher :: ChildExecutables -> ChildOrder -> App.ConnectionDescriptor -> HeraldLocator -> HeraldLocator -> IO Text
runHelloLauncher executables order descriptor publisherLocator readerLocator =
  App.withHerald descriptor workflow >>= expect
  where
    workflow herald = do
      environment <- expect (Typed.startupEnvironment herald)
      publisherEnvironment <- Typed.newEnvironment herald >>= expect
      readerEnvironment <- Typed.newEnvironment herald >>= expect
      sort <- expect messageSort
      Typed.declareSort environment sort >>= expect
      let parent = Access.startupAccessProcess (App.startup herald)
      messages <- Typed.createNabla environment sort Nothing >>= expect
      messageReader <- Typed.createDelta environment sort >>= expect
      acknowledgements <- Typed.createNabla environment sort Nothing >>= expect
      acknowledgementReader <- Typed.createDelta environment sort >>= expect
      commands <- Typed.createNabla environment sort Nothing >>= expect
      commandReader <- Typed.createDelta environment sort >>= expect
      firstEdge <- Typed.createEdge environment Typed.Preserve (Typed.nablaVertex messages) (Typed.deltaVertex messageReader) >>= expect
      _ <- Typed.createEdge environment Typed.Preserve (Typed.nablaVertex acknowledgements) (Typed.deltaVertex acknowledgementReader) >>= expect
      _ <- Typed.createEdge environment Typed.Preserve (Typed.nablaVertex commands) (Typed.deltaVertex commandReader) >>= expect
      forwarded <- Typed.forwardEdge firstEdge >>= expect
      unless (forwarded == ForwardAccepted) (failure "edge forwarding was not accepted")
      publisherSelection <-
        selectedEnvironment
          publisherEnvironment
          [("messages.writer", Access.Writer (Typed.nablaId messages)), ("acks.reader", Access.Reader (Typed.deltaId acknowledgementReader))]
      readerSelection <-
        selectedEnvironment
          readerEnvironment
          [("messages.reader", Access.Reader (Typed.deltaId messageReader)), ("acks.writer", Access.Writer (Typed.nablaId acknowledgements)), ("commands.reader", Access.Reader (Typed.deltaId commandReader))]
      withPreparationReferences herald publisherLocator publisherSelection readerLocator readerSelection $ \publisherReference readerReference -> do
        publisher <- Advanced.submitLifecycle herald (Advanced.AwaitPreparedChild publisherReference) >>= Advanced.awaitLifecycle >>= expect
        reader <- Advanced.submitLifecycle herald (Advanced.AwaitPreparedChild readerReference) >>= Advanced.awaitLifecycle >>= expect
        transfer herald parent publisher publisherSelection
        transfer herald parent reader readerSelection
        App.awaitChildReady herald publisher >>= expect
        App.awaitChildReady herald reader >>= expect
        runChildren executables order (preparedChildConnection publisher) (preparedChildConnection reader) (publishMessage commands "exit")
        App.endProcess herald >>= expect
        pure "Hello, world!"

selectedEnvironment :: Typed.Environment -> [(Text, Access.PrimordialEntry)] -> IO Access.PrimordialSelection
selectedEnvironment environment extra = expect (Access.primordialSelection entries required (Access.selectionEnvironmentSources base))
  where
    base = Typed.environmentSelection environment
    entries = Map.toList (Access.selectionEntries base) <> extra
    required = Access.selectionRequiredEndpoints base <> Set.fromList (map fst extra)

transfer :: App.Herald -> PrivateProcessId -> PreparedChild -> Access.PrimordialSelection -> IO ()
transfer herald parent child selection = forM_ (Map.elems (Access.selectionEntries selection)) $ \entry -> do
  result <-
    App.label
      herald
      (asPrivateObjectId (Access.primordialEntryIdentity entry))
      (ProcessLabel parent, 0)
      (LabelToProcess (preparedChildProcess child))
      >>= expect
  unless (result == LabelApplied) (failure "selected object label transfer was not applied")

data PreparationAcquisition
  = SubmittedPreparation (MVar (Either SomeException (Advanced.LifecycleCall ChildPreparation)))
  | AcceptedPreparation ChildPreparation

-- Register the exact submission before restoring an interruptible acceptance
-- wait. Its short worker waits only for the client owner to assign the request;
-- cleanup observes that same worker and call even if acceptance is still pending.
submitPreparation :: App.Herald -> HeraldLocator -> Access.PrimordialSelection -> IO PreparationAcquisition
submitPreparation herald locator selection =
  SubmittedPreparation <$> submitOwned (Advanced.submitLifecycle herald (Advanced.BeginChild locator selection))

submitOwned :: IO value -> IO (MVar (Either SomeException value))
submitOwned action = do
  submitted <- newEmptyMVar
  _ <- forkFinally action (putMVar submitted)
  pure submitted

awaitPreparationReference :: PreparationAcquisition -> IO ChildPreparation
awaitPreparationReference (AcceptedPreparation reference) = pure reference
awaitPreparationReference (SubmittedPreparation submitted) =
  readMVar submitted >>= either throwIO pure >>= Advanced.awaitLifecycle >>= expect

withPreparationReferences :: App.Herald -> HeraldLocator -> Access.PrimordialSelection -> HeraldLocator -> Access.PrimordialSelection -> (ChildPreparation -> ChildPreparation -> IO value) -> IO value
withPreparationReferences herald publisherLocator publisherSelection readerLocator readerSelection use = mask $ \restore -> do
  publisher <- submitPreparation herald publisherLocator publisherSelection
  publisherReference <-
    restore (awaitPreparationReference publisher)
      `onPreparationException` cancelAcquisitions herald [publisher]
  reader <-
    submitPreparation herald readerLocator readerSelection
      `onPreparationException` cancelAcquisitions herald [AcceptedPreparation publisherReference]
  readerReference <-
    restore (awaitPreparationReference reader)
      `onPreparationException` cancelAcquisitions herald [AcceptedPreparation publisherReference, reader]
  restore (use publisherReference readerReference)
    `onPreparationException` cancelAcquisitions herald [AcceptedPreparation publisherReference, AcceptedPreparation readerReference]

-- Cleanup still observes every acquired request; an error while unwinding must
-- not replace the original setup, child-process or caller exception.
onPreparationException :: IO value -> IO () -> IO value
onPreparationException action cleanup = action `onException` void (try cleanup :: IO (Either SomeException ()))

-- Known targets receive cancellation before awaiting a still-pending Begin
-- result. All accepted targets receive cancellation before any End is awaited.
-- Every completion is joined even if another acquisition or cancellation failed.
cancelAcquisitions :: App.Herald -> [PreparationAcquisition] -> IO ()
cancelAcquisitions herald acquisitions = mask $ \restore -> do
  let submitCancel reference = submitOwned (Advanced.submitLifecycle herald (Advanced.CancelChild reference))
      awaitCancellation call = void (Advanced.awaitLifecycle call >>= expect)
      collectSubmission (Left problem) = pure (Left problem)
      collectSubmission (Right submitted) = either Left id <$> try (restore (readMVar submitted))
      submitAll references = traverse (try . submitCancel) references >>= traverse collectSubmission
  knownResults <- submitAll [reference | AcceptedPreparation reference <- acquisitions]
  pendingResults <- traverse (try . restore . awaitPreparationReference) [pending | pending@SubmittedPreparation {} <- acquisitions]
  acceptedResults <- submitAll [reference | Right reference <- pendingResults]
  completions <- traverse (try . restore . awaitCancellation) [call | Right call <- knownResults <> acceptedResults]
  let failures = [problem | Left problem <- knownResults] <> [problem | Left problem <- pendingResults] <> [problem | Left problem <- acceptedResults] <> [problem | Left problem <- completions]
  case failures of
    (problem :: SomeException) : _ -> throwIO problem
    [] -> pure ()

-- Process acquisition is synchronous and nested, so the requested OS startup
-- order is real and failure to start the first child never starts the second.
-- Output readers and each process waiter remain supervised by the same scope.
runChildren :: ChildExecutables -> ChildOrder -> App.ConnectionDescriptor -> App.ConnectionDescriptor -> IO () -> IO ()
runChildren (ChildExecutables publisherExecutable readerExecutable) order publisher reader afterPublisher = do
  completed <- newChan
  let publisherRun = (True, publisherExecutable, publisher, "publisher completed\n")
      readerRun = (False, readerExecutable, reader, "Hello, world!\n")
      runs = case order of PublisherFirst -> [publisherRun, readerRun]; ReaderFirst -> [readerRun, publisherRun]
      start (isPublisher, executable, connection, expected) = do
        process@(_, stdoutHandle, stderrHandle, handle) <-
          createProcess
            (proc executable ["--connection", Text.unpack (App.encodeConnectionDescriptor connection)])
              { std_in = NoStream,
                std_out = CreatePipe,
                std_err = CreatePipe
              }
        let run = case (stdoutHandle, stderrHandle) of
              (Just outputHandle, Just errorHandle) ->
                withOutput outputHandle $ \outputResult ->
                  withOutput errorHandle $ \errorResult -> do
                    output <- readMVar outputResult >>= either throwIO pure
                    diagnostic <- readMVar errorResult >>= either throwIO pure
                    exitCode <- waitForProcess handle
                    unless
                      (exitCode == ExitSuccess && output == expected)
                      (failure ("child application failed: " <> show exitCode <> " " <> output <> diagnostic))
              _ -> failure "requested child output pipes were not created"
        stopped <- newEmptyMVar
        worker <-
          ( forkFinally run $ \result -> do
              writeChan completed (isPublisher, result :: Either SomeException ())
                `finally` putMVar stopped ()
          )
            `onException` cleanupProcess process
        pure (process, worker, stopped)
      stop (process@(_, _, _, handle), worker, stopped) = do
        terminateProcess handle
        killThread worker
        _ <- readMVar stopped
        cleanupProcess process
      withRuns [] action = action
      withRuns (item : remaining) action = bracket (start item) stop (\_ -> withRuns remaining action)
      collect = do
        (isPublisher, result) <- readChan completed
        either throwIO pure result
        if isPublisher then afterPublisher else pure ()
  withRuns runs (collect >> collect)
  where
    withOutput handle use = bracket start stop (use . snd)
      where
        start = do
          result <- newEmptyMVar
          worker <-
            forkFinally
              ( do
                  output <- hGetContents handle
                  _ <- evaluate (length output)
                  pure output
              )
              (putMVar result)
          pure (worker, result)
        stop (worker, result) = killThread worker >> void (readMVar result)
