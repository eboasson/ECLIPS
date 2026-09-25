{-# LANGUAGE OverloadedStrings #-}

-- | Ordinary application processes for the parent-exit startup regression.
-- Their only shared semantic artifact is the child's opaque descriptor.
module ParentExit (runParentBeforeClaim, runParentBeforeClaims, runChildAfterParentExit) where

import Control.Monad (forM_, unless)
import Data.Text.IO qualified as Text
import Eclips.Application qualified as App
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity (asPrivateObjectId)
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToProcess), LabelResult (LabelApplied))
import Eclips.Application.Types.Lifecycle (connectionDescriptorLocator, preparedChildConnection, preparedChildProcess)
import Eclips.Application.Types.Value (ApplicationLabelOwner (ProcessLabel))

runParentBeforeClaim :: FilePath -> FilePath -> IO ()
runParentBeforeClaim parentPath childPath = runParentBeforeClaims parentPath [childPath]

-- Each exported descriptor has one distinct prepared process identity. This
-- also lets deployment tours launch several ordinary parents without reusing
-- the founder's one-shot launcher descriptor after that launcher has ended.
runParentBeforeClaims :: FilePath -> [FilePath] -> IO ()
runParentBeforeClaims parentPath childPaths = do
  descriptor <- readDescriptor parentPath
  App.withHerald descriptor (prepareAndEnd descriptor) >>= expect
  putStrLn "parent ended before child claim"
  where
    prepareAndEnd descriptor parent = do
      children <- traverse (const (prepareChild descriptor parent)) childPaths
      App.endProcess parent >>= expect
      forM_ (zip childPaths children) $ \(childPath, child) ->
        Text.writeFile childPath (App.encodeConnectionDescriptor (preparedChildConnection child))
    prepareChild descriptor parent = do
      environment <- App.newenv parent >>= expect
      let selection = Access.selectEnvironment environment
          parentProcess = Access.startupAccessProcess (App.startup parent)
      child <- App.prepareChild parent (connectionDescriptorLocator descriptor) selection >>= expect
      forM_ (Access.selectionEntries selection) $ \entry -> do
        result <-
          App.label
            parent
            (asPrivateObjectId (Access.primordialEntryIdentity entry))
            (ProcessLabel parentProcess, 0)
            (LabelToProcess (preparedChildProcess child))
            >>= expect
        unless (result == LabelApplied) (fail "selected child label was not applied")
      App.awaitChildReady parent child >>= expect
      pure child

runChildAfterParentExit :: FilePath -> IO ()
runChildAfterParentExit childPath = do
  descriptor <- readDescriptor childPath
  App.withHerald descriptor operateAndEnd >>= expect
  putStrLn "child claimed, operated and ended after parent exit"
  where
    operateAndEnd child = do
      let access = Access.startupAccessPrimordial (App.startup child)
      unless
        (length (Access.accessEntries access) == 31 && length (Access.accessRequiredEndpoints access) == 12 && length (Access.accessObjectSorts access) == 19)
        (fail "child did not receive its complete selected environment")
      environment <- App.newenv child >>= expect
      unless
        (length (Access.environmentAccessPredefined environment) == 6 && length (Access.environmentAccessEdges environment) == 18)
        (fail "child could not create a complete environment")
      App.endProcess child >>= expect

readDescriptor :: FilePath -> IO App.ConnectionDescriptor
readDescriptor path = Text.readFile path >>= expect . App.decodeConnectionDescriptor

expect :: (Show problem) => Either problem value -> IO value
expect = either (fail . show) pure
