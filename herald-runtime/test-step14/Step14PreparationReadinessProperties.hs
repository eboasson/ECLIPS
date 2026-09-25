{-# LANGUAGE OverloadedStrings #-}

module Step14PreparationReadinessProperties (tests) where

import Control.Monad (forM, forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity (asPrivateObjectId)
import Eclips.Application.Types.Label (ApplicationLabelTarget (LabelToProcess), LabelResult (LabelApplied))
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.Result (RegularCallResult (LabelCompleted, NewEnvironmentCompleted))
import Eclips.Application.Types.Value (ApplicationLabelOwner (ProcessLabel))
import Eclips.Herald.Runtime.TCP (heraldTcpApplicationEndpoint, heraldTcpEndpoints, resolvedTcpHost, resolvedTcpPort)
import Eclips.Protocol.Application.Types (applicationClientNonce)
import Step14Fixtures
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "prepared child alignment readiness" [testCase "two same-Herald children reach reader readiness with multiple Heralds" caseEnvironmentReadiness]

caseEnvironmentReadiness :: Assertion
caseEnvironmentReadiness = do
  currentStage <- newIORef "deployment startup"
  let stage = writeIORef currentStage
  -- The outer bound also covers connection establishment and supervised scope
  -- teardown. Inner waits report their exact semantic stage before this bound.
  completed <- timeout (10 * causalWaitTimeoutMicroseconds)
    $ withStep14Deployment
    $ \deployment -> do
      bounded stage "peer mesh" (awaitStep14PeerMesh deployment)
      stage "parent application connection"
      result <- EAPP.withApplication
        (EAPP.applicationConfiguration (step14PApplicationEndpoint deployment) step14PApplicationAttachment (applicationClientNonce 14_501) step14ApplicationLivenessConfiguration)
        $ \application startup -> do
          environments <- forM [1 :: Int, 2] $ \ordinal ->
            bounded stage ("newenv " <> show ordinal) (EAPP.newenv application >>= EAPP.awaitApplicationCall) >>= \case
              EAPP.ApplicationCallSucceeded (NewEnvironmentCompleted value) -> pure value
              other -> assertFailure ("newenv failed: " <> show other)
          let endpoint = heraldTcpApplicationEndpoint (heraldTcpEndpoints (step14HeraldTcp (step14H1 deployment)))
          locator <- checked (heraldLocator (resolvedTcpHost endpoint) (resolvedTcpPort endpoint))
          children <- forM (zip [1 :: Int ..] environments) $ \(ordinal, environment) -> do
            let selection = Access.selectEnvironment environment
            reference <-
              bounded stage ("BeginChild " <> show ordinal) (EAPP.beginChild application locator selection >>= EAPP.awaitApplicationLifecycleCall) >>= \case
                EAPP.ApplicationLifecycleSucceeded (ChildPreparationAccepted value) -> pure value
                other -> assertFailure ("BeginChild failed: " <> show other)
            child <-
              bounded stage ("AwaitPreparedChild " <> show ordinal) (EAPP.awaitPreparedChild application reference >>= EAPP.awaitApplicationLifecycleCall) >>= \case
                EAPP.ApplicationLifecycleSucceeded (ChildPrepared value) -> pure value
                other -> assertFailure ("AwaitPreparedChild failed: " <> show other)
            pure (selection, child)
          -- Canonical key order moves each reader before its writer. Both
          -- environments move before either child's readiness is awaited.
          forM_ (zip [1 :: Int ..] children) $ \(ordinal, (selection, child)) ->
            forM_ (Map.toAscList (Access.selectionEntries selection)) $ \(key, entry) -> do
              labelled <- bounded stage ("child " <> show ordinal <> " label " <> show key) (EAPP.label application (asPrivateObjectId (Access.primordialEntryIdentity entry)) ((ProcessLabel (Access.startupAccessProcess startup), 0)) (LabelToProcess (preparedChildProcess child)) >>= EAPP.awaitApplicationCall)
              assertEqual ("transferred " <> show key) (EAPP.ApplicationCallSucceeded (LabelCompleted LabelApplied)) labelled
          ready <- bounded stage "submit both readiness requests" (traverse (EAPP.awaitChildReady application . preparedChildPreparation . snd) children)
          stage "await both child readiness results"
          outcome <- timeout causalWaitTimeoutMicroseconds (traverse EAPP.awaitApplicationLifecycleCall ready)
          statuses <- bounded stage "inspect retained readiness results" (traverse EAPP.applicationLifecycleStatus ready)
          assertEqual
            ("both children become ready after all label transfers; retained status=" <> show statuses)
            (Just (replicate 2 (EAPP.ApplicationLifecycleSucceeded ChildReady)))
            outcome
          forM_ (zip [1 :: Int ..] children) $ \(ordinal, (_, child)) -> do
            cancelled <- bounded stage ("cancel unclaimed child " <> show ordinal) (EAPP.cancelChild application (preparedChildPreparation child) >>= EAPP.awaitApplicationLifecycleCall)
            assertEqual "unclaimed child ends before deployment drain" (EAPP.ApplicationLifecycleSucceeded ChildCancelled) cancelled
          endCall <- bounded stage "submit parent own-End" (EAPP.endProcess application)
          ended <- bounded stage "parent own-End terminal precedes session disposal" (EAPP.awaitApplicationLifecycleCall endCall)
          assertEqual "own-End terminal crosses real TCP" (EAPP.ApplicationLifecycleSucceeded ProcessEnded) ended
          repeated <- bounded stage "caller retains own-End completion after session cleanup" (EAPP.awaitApplicationLifecycleCall endCall)
          assertEqual "call handle owns the completion after routing retirement" ended repeated
          stage "parent application scope teardown"
      _ <- checked result
      stage "deployment drain"
  case completed of
    Just () -> pure ()
    Nothing -> readIORef currentStage >>= assertFailure . ("prepared-child scenario timed out during " <>)

checked :: (Show problem) => Either problem value -> IO value
checked = either (assertFailure . show) pure

bounded :: (String -> IO ()) -> String -> IO value -> IO value
bounded stage description action = do
  stage description
  timeout causalWaitTimeoutMicroseconds action >>= maybe (assertFailure ("timed out during " <> description)) pure
