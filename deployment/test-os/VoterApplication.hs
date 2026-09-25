{-# LANGUAGE OverloadedStrings #-}

-- | An ordinary OS application keeps its attachment across its Herald's
-- demotion. A file is only the fixture's permission to start post-demotion work.
module VoterApplication (runParentAcrossDemotion, runParentOnTarget, runApplicationAcrossFence) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Eclips.Application qualified as App
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity (asPrivateObjectId)
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToProcess), LabelResult (LabelApplied))
import Eclips.Application.Types.Lifecycle (HeraldLocator, connectionDescriptorLocator, heraldLocator, preparedChildConnection, preparedChildProcess)
import Eclips.Application.Types.Value (ApplicationLabelOwner (ProcessLabel))
import Eclips.Deployment.Configuration (endpointHost, endpointPort, parseEndpoint)
import System.Directory (doesFileExist)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

runApplicationAcrossFence :: FilePath -> FilePath -> FilePath -> IO ()
runApplicationAcrossFence descriptorPath fencedPath healedPath = do
  hSetBuffering stdout LineBuffering
  descriptor <- Text.readFile descriptorPath >>= checked . App.decodeConnectionDescriptor
  App.withHerald descriptor (operate descriptor) >>= checked
  putStrLn "fenced attachment remained unavailable after transport healing"
  where
    operate descriptor application = do
      _ <- App.newenv application >>= checked
      putStrLn "ready"
      awaitMarker fencedPath
      requireUnavailable application
      putStrLn "fenced"
      awaitMarker healedPath
      requireUnavailable application
      App.withHerald descriptor (const (pure ())) >>= \case
        Left _ -> pure ()
        Right () -> fail "fenced epoch accepted a new application attachment after healing"
    requireUnavailable application =
      App.newenv application >>= \case
        Left (App.CallUnavailable _) -> pure ()
        -- Terminal loss settles an admitted call as unavailable. Once the
        -- client has consumed that disposition, fresh calls are closed locally.
        Left App.CallClosed -> pure ()
        other -> fail ("fenced application did not report terminal unavailability: " <> either show (const "successful newenv") other)
    awaitMarker path = doesFileExist path >>= \exists -> unless exists (threadDelay 10_000 >> awaitMarker path)

runParentAcrossDemotion :: FilePath -> FilePath -> FilePath -> IO ()
runParentAcrossDemotion parentPath resumePath childPath = do
  hSetBuffering stdout LineBuffering
  descriptor <- Text.readFile parentPath >>= checked . App.decodeConnectionDescriptor
  App.withHerald descriptor (operate descriptor) >>= checked
  putStrLn "existing application operated and ended after demotion"
  where
    operate descriptor parent = do
      _ <- App.newenv parent >>= checked
      putStrLn "ready"
      awaitResume
      prepareAndEnd (connectionDescriptorLocator descriptor) childPath parent
    awaitResume = do
      resumed <- doesFileExist resumePath
      unless resumed (threadDelay 10_000 >> awaitResume)

runParentOnTarget :: FilePath -> String -> FilePath -> IO ()
runParentOnTarget parentPath targetText childPath = do
  descriptor <- Text.readFile parentPath >>= checked . App.decodeConnectionDescriptor
  endpoint <- checked (parseEndpoint (Text.pack targetText))
  target <- checked (heraldLocator (endpointHost endpoint) (endpointPort endpoint))
  App.withHerald descriptor (prepareAndEnd target childPath) >>= checked
  putStrLn "parent prepared child on promoted Herald"

prepareAndEnd :: HeraldLocator -> FilePath -> App.Herald -> IO ()
prepareAndEnd target childPath parent = do
  environment <- App.newenv parent >>= checked
  let selection = Access.selectEnvironment environment
      parentProcess = Access.startupAccessProcess (App.startup parent)
  child <- App.prepareChild parent target selection >>= checked
  unless
    (connectionDescriptorLocator (preparedChildConnection child) == target)
    (fail "prepared child does not name the requested Herald endpoint")
  forM_ (Access.selectionEntries selection) $ \entry -> do
    result <-
      App.label
        parent
        (asPrivateObjectId (Access.primordialEntryIdentity entry))
        (ProcessLabel parentProcess, 0)
        (LabelToProcess (preparedChildProcess child))
        >>= checked
    unless (result == LabelApplied) (fail "Herald did not apply selected child label")
  App.awaitChildReady parent child >>= checked
  Text.writeFile childPath (App.encodeConnectionDescriptor (preparedChildConnection child))
  App.endProcess parent >>= checked

checked :: (Show problem) => Either problem value -> IO value
checked = either (fail . show) pure
