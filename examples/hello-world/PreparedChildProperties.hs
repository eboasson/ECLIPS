{-# LANGUAGE OverloadedStrings #-}

-- | A real OS child claims an environment prepared and labelled by its parent.
module PreparedChildProperties (runPreparedChildTour, runPreparedChildProcess) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.Binary (decodeOrFail, encode)
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Lazy
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Application.Runtime qualified as App
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToProcess), LabelResult (LabelApplied))
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Query (ApplicationQuery (..), ApplicationQueryPredicate (QueryAlways))
import Eclips.Application.Types.Result (RegularCallResult (..))
import Eclips.Application.Types.Value (ApplicationLabelOwner (ProcessLabel))
import Eclips.Application.Types.Write (ApplicationWriteValue (DeleteReserved), WriteResult (WriteAccepted))
import Eclips.Domain.Identity (controlIndex)
import FounderDeployment (withFounderDeploymentAtControl)
import System.Directory (removeFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, openBinaryTempFile)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

runPreparedChildTour :: IO ()
runPreparedChildTour = forM_ [False, True] $ \readerFirst -> do
  putStrLn (if readerFirst then "transferring readers before writers" else "transferring writers before readers")
  -- Each of the thirty-one object labels commits two singleton control entries;
  -- the two child lifecycles and the parent's End contribute five more.
  observed <- timeout 120_000_000 $ withFounderDeploymentAtControl (controlIndex 67) $ \descriptor -> do
    claim <- checked (initialClaimId (Bytes.replicate 32 0x72))
    let config = App.preparedApplicationConfiguration descriptor claim liveness
    result <- App.withApplication config $ \parent startup -> do
      environment <- expectEnvironment =<< App.newenv parent
      let locator = connectionDescriptorLocator descriptor
      preparation <- expectPreparation =<< App.beginChild parent locator (Access.selectEnvironment environment)
      prepared <- expectPrepared =<< App.awaitPreparedChild parent preparation
      unless (preparedChildPreparation prepared == preparation) (failure "preparation identity changed")
      -- Preparation must complete before labels can name the child.
      forM_ (Access.environmentAccessPredefined environment) $ \pair -> do
        let writer = privateNablaUniqueId (Access.predefinedWriter pair)
            reader = privateDeltaUniqueId (Access.predefinedReader pair)
        forM_ (if readerFirst then [reader, writer] else [writer, reader]) $ \object -> do
          reply <- App.awaitApplicationCall =<< App.label parent (asPrivateObjectId object) (ProcessLabel (Access.startupAccessProcess startup), 0) (LabelToProcess (preparedChildProcess prepared))
          unless (reply == App.ApplicationCallSucceeded (LabelCompleted LabelApplied)) (failure ("root label transfer: " <> show reply))
      forM_ (Access.environmentAccessHub environment : Access.environmentAccessEdges environment) $ \object -> do
        reply <- App.awaitApplicationCall =<< App.label parent object (ProcessLabel (Access.startupAccessProcess startup), 0) (LabelToProcess (preparedChildProcess prepared))
        unless (reply == App.ApplicationCallSucceeded (LabelCompleted LabelApplied)) (failure ("wiring label transfer: " <> show reply))
      expectLifecycle ChildReady =<< App.awaitChildReady parent preparation
      withDescriptor (preparedChildConnection prepared) $ \path -> do
        executable <- getExecutablePath
        (exitCode, output, diagnostic) <- readCreateProcessWithExitCode (proc executable ["--prepared-child", path]) ""
        unless
          (exitCode == ExitSuccess && output == "prepared child operated and ended\n")
          (failure ("independent child failed: " <> show exitCode <> " " <> output <> diagnostic))
      -- Failed spawning is explicit cancellation, not implicit parent ownership.
      empty <- checked (Access.primordialSelection [] Set.empty Nothing)
      abandoned <- expectPreparation =<< App.beginChild parent locator empty
      _ <- expectPrepared =<< App.awaitPreparedChild parent abandoned
      expectLifecycle ChildCancelled =<< App.cancelChild parent abandoned
      end <- App.endProcess parent
      expectCompletedEndLifetime config end
    either (failure . show) pure result
  case observed of
    Nothing -> failure "prepared-child OS tour exceeded its deadlock watchdog"
    Just () -> pure ()

runPreparedChildProcess :: FilePath -> IO ()
runPreparedChildProcess path = do
  bytes <- Lazy.readFile path
  descriptor <- case decodeOrFail bytes of
    Right (remaining, _, value) | Lazy.null remaining -> pure value
    _ -> failure "invalid test child descriptor"
  claim <- checked (initialClaimId (Bytes.replicate 32 0x73))
  let config = App.preparedApplicationConfiguration descriptor claim liveness
  result <- App.withApplication config $ \child startup -> do
    let access = Access.startupAccessPrimordial startup
    unless
      (Map.size (Access.accessEntries access) == 31 && Set.size (Access.accessRequiredEndpoints access) == 12 && Map.size (Access.accessObjectSorts access) == 19)
      (failure "child did not receive the exact selected environment")
    -- Required deltas have their own installed placement/incarnation and are usable.
    forM_ [reader | Access.Reader reader <- Map.elems (Access.accessEntries access)] $ \reader -> do
      response <- App.awaitApplicationCall =<< App.read child (ApplicationQuery (Set.singleton reader) QueryAlways)
      case response of
        App.ApplicationCallSucceeded (ReadCompleted _) -> pure ()
        _ -> failure ("child required delta was not operational: " <> show response)
    writer <- case Map.lookup (Access.environmentWriterKey Access.EdgeRole) (Access.accessEntries access) of
      Just (Access.Writer value) -> pure value
      _ -> failure "missing selected edge writer"
    reserved <- App.awaitApplicationCall =<< App.newid child (ControlledNewId writer)
    object <- case reserved of
      App.ApplicationCallSucceeded (NewIdCompleted value) -> pure value
      _ -> failure ("child selected writer reservation: " <> show reserved)
    removed <- App.awaitApplicationCall =<< App.write child writer (DeleteReserved (asPrivateObjectId object))
    unless (removed == App.ApplicationCallSucceeded (WriteCompleted WriteAccepted)) (failure ("child selected writer update: " <> show removed))
    environment <- expectEnvironment =<< App.newenv child
    unless
      (length (Access.environmentAccessPredefined environment) == 6 && length (Access.environmentAccessEdges environment) == 18)
      (failure "child newenv result was incomplete")
    end <- App.endProcess child
    expectCompletedEndLifetime config end
  either (failure . show) pure result
  putStrLn "prepared child operated and ended"

liveness :: App.ApplicationLivenessConfiguration
liveness = either (error . show) id (App.applicationLivenessConfigurationMicroseconds 10_000 5_000_000 500_000 500_000)

expectEnvironment :: App.ApplicationCall -> IO Access.EnvironmentAccess
expectEnvironment call = do
  reply <- App.awaitApplicationCall call
  case reply of
    App.ApplicationCallSucceeded (NewEnvironmentCompleted access) -> pure access
    _ -> failure ("newenv: " <> show reply)

expectPreparation :: App.ApplicationLifecycleCall -> IO ChildPreparation
expectPreparation call = do
  reply <- App.awaitApplicationLifecycleCall call
  case reply of
    App.ApplicationLifecycleSucceeded (ChildPreparationAccepted preparation) -> pure preparation
    _ -> failure ("begin child: " <> show reply)

expectPrepared :: App.ApplicationLifecycleCall -> IO PreparedChild
expectPrepared call = do
  reply <- App.awaitApplicationLifecycleCall call
  case reply of
    App.ApplicationLifecycleSucceeded (ChildPrepared prepared) -> pure prepared
    _ -> failure ("await prepared: " <> show reply)

expectLifecycle :: LifecycleResult -> App.ApplicationLifecycleCall -> IO ()
expectLifecycle expected call = do
  reply <- App.awaitApplicationLifecycleCall call
  unless (reply == App.ApplicationLifecycleSucceeded expected) (failure ("lifecycle result: " <> show reply))

expectCompletedEndLifetime :: App.ApplicationConfiguration -> App.ApplicationLifecycleCall -> IO ()
expectCompletedEndLifetime configuration call = do
  expectLifecycle ProcessEnded call
  -- The caller owns the completed handle after session receipt cleanup.
  expectLifecycle ProcessEnded call
  request <- maybe (failure "End was not assigned a lifecycle identity") pure (App.applicationLifecycleRequestId call)
  queried <- App.recoverEndProcess configuration request
  unless
    (queried == Right (App.ApplicationLifecycleUnknown App.LifecycleResultNotRetained))
    (failure ("new End query after receipt retirement: " <> show queried))
  -- A fresh restricted query cannot alter or recreate the completed call.
  expectLifecycle ProcessEnded call

withDescriptor :: ConnectionDescriptor -> (FilePath -> IO value) -> IO value
withDescriptor descriptor action = bracket create removeFile action
  where
    create = do
      (path, handle) <- openBinaryTempFile "." "p03-child-connection-"
      Lazy.hPut handle (encode descriptor)
      hClose handle
      pure path

checked :: (Show problem) => Either problem value -> IO value
checked = either (failure . show) pure
failure :: String -> IO value
failure = ioError . userError
