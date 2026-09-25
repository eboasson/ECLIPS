{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Real EAPP clients used by the Step-15 crash and terminal-repair tours.
module Step15Applications
  ( withStep15Applications,
    withStep15Application,
    step15PredefinedAccess,
    publishStep15NeutralCarrier,
    readStep15PredefinedStore,
    awaitStep15ApplicationCallWithin,
  )
where

import Control.Concurrent
  ( MVar,
    forkFinally,
    newEmptyMVar,
    putMVar,
    readMVar,
  )
import Control.Exception
  ( SomeException,
    throwIO,
  )
import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole,
    ApplicationStartupAccess,
    PredefinedAccess,
    predefinedReader,
    predefinedWriter,
    startupAccessProcess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity (PrivateUniqueId)
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Result (RegularCallResult (NewIdCompleted, ReadCompleted))
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (ProcessLabel),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write (ApplicationWriteValue (PublishValue))
import Eclips.Domain.Identity (bootstrapManifestIdBytes)
import Eclips.Herald.Runtime.TCP
  ( heraldTcpApplicationEndpoint,
    heraldTcpEndpoints,
    resolvedTcpHost,
    resolvedTcpPort,
  )
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    applicationAttachmentClaim,
    applicationClientNonce,
  )
import Step15Configuration (Step15HeraldName (..))
import Step15Fixtures
  ( Step15Deployment,
    Step15Herald,
    step15H1,
    step15H2,
    step15H4,
    step15ManagedHeraldTcp,
  )
import Step15Genesis
  ( step15H1ProcessBootstrapId,
    step15H2ProcessBootstrapId,
    step15H4ProcessBootstrapId,
  )
import System.Timeout (timeout)
import Test.Tasty.HUnit (assertFailure)

withStep15Applications ::
  Step15Deployment ->
  ( EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    EAPP.Application ->
    ApplicationStartupAccess ->
    IO result
  ) ->
  IO result
withStep15Applications deployment use = do
  h1Result <-
    EAPP.withApplication
      (configuration Step15H1 (step15H1 deployment) 15_501)
      $ \h1Application h1Startup -> do
        h2Result <-
          EAPP.withApplication
            (configuration Step15H2 (step15H2 deployment) 15_502)
            $ \h2Application h2Startup -> do
              h4Result <-
                EAPP.withApplication
                  (configuration Step15H4 (step15H4 deployment) 15_504)
                  (use h1Application h1Startup h2Application h2Startup)
              requireApplicationResult "H4" h4Result
        requireApplicationResult "H2" h2Result
  requireApplicationResult "H1" h1Result

withStep15Application ::
  Step15HeraldName ->
  Step15Herald ->
  Word64 ->
  (EAPP.Application -> ApplicationStartupAccess -> IO result) ->
  IO result
withStep15Application name herald nonce use =
  requireApplicationResult (show name)
    =<< EAPP.withApplication (configuration name herald nonce) use

configuration ::
  Step15HeraldName ->
  Step15Herald ->
  Word64 ->
  EAPP.ApplicationConfiguration
configuration name herald nonce =
  EAPP.applicationConfiguration
    endpoint
    (attachment name)
    (applicationClientNonce nonce)
    liveness
  where
    resolved =
      heraldTcpApplicationEndpoint
        (heraldTcpEndpoints (step15ManagedHeraldTcp herald))
    endpoint =
      checked
        (show name <> " application endpoint")
        (EAPP.applicationEndpoint (resolvedTcpHost resolved) (resolvedTcpPort resolved))

attachment :: Step15HeraldName -> ApplicationAttachmentClaim
attachment name =
  checked
    (show name <> " application attachment")
    (applicationAttachmentClaim (bootstrapManifestIdBytes bootstrap))
  where
    bootstrap = case name of
      Step15H1 -> step15H1ProcessBootstrapId
      Step15H2 -> step15H2ProcessBootstrapId
      Step15H4 -> step15H4ProcessBootstrapId
      Step15H3 -> error "Step-15 H3 deliberately has no configured process"

liveness :: EAPP.ApplicationLivenessConfiguration
liveness =
  checked
    "Step-15 application liveness"
    (EAPP.applicationLivenessConfigurationMicroseconds 1_000 30_000_000 60_000_000 5_000_000)

step15PredefinedAccess ::
  String ->
  ApplicationPredefinedSortRole ->
  ApplicationStartupAccess ->
  IO PredefinedAccess
step15PredefinedAccess context role startup =
  case (Map.lookup (Access.environmentWriterKey role) (Access.accessEntries (Access.startupAccessPrimordial startup)), Map.lookup (Access.environmentReaderKey role) (Access.accessEntries (Access.startupAccessPrimordial startup))) of
    (Just (Access.Writer writer), Just (Access.Reader reader)) -> pure (Access.predefinedAccess role writer reader)
    _ -> assertFailure (context <> " startup access is missing " <> show role)

-- | Publish one genuine structural occurrence from a configured process.
-- The returned call is deliberately not awaited by the terminal-repair tour:
-- H4 crashes while two physical destination handoffs remain paused.
publishStep15NeutralCarrier ::
  String ->
  EAPP.Application ->
  ApplicationStartupAccess ->
  PredefinedAccess ->
  IO (PrivateUniqueId, EAPP.ApplicationCall)
publishStep15NeutralCarrier context application startup access = do
  objectCall <- EAPP.newid application (ControlledNewId (predefinedWriter access))
  object <-
    awaitStep15ApplicationCallWithin (context <> " object reservation") objectCall >>= \case
      EAPP.ApplicationCallSucceeded (NewIdCompleted identifier) -> pure identifier
      other -> unexpected (context <> " object reservation") other
  publication <-
    EAPP.write
      application
      (predefinedWriter access)
      (PublishValue (neutralCarrier startup object))
  pure (object, publication)

neutralCarrier :: ApplicationStartupAccess -> PrivateUniqueId -> ApplicationValue
neutralCarrier startup object =
  RecordValue
    ( Map.fromList
        [ ("label", LabelValue ((ProcessLabel (startupAccessProcess startup), 0))),
          ("object_id", UniqueIdValue object)
        ]
    )

readStep15PredefinedStore ::
  String ->
  EAPP.Application ->
  PredefinedAccess ->
  IO [ApplicationValue]
readStep15PredefinedStore context application access = do
  call <-
    EAPP.read
      application
      ApplicationQuery
        { applicationQueryDeltas = Set.singleton (predefinedReader access),
          applicationQueryPredicate = QueryAlways
        }
  awaitStep15ApplicationCallWithin context call >>= \case
    EAPP.ApplicationCallSucceeded (ReadCompleted values) -> pure values
    other -> unexpected context other

-- See the matching Step-14 helper: 'awaitApplicationCall' has a stable
-- cancellation handler, so applying 'timeout' directly does not bound a test.
awaitStep15ApplicationCallWithin ::
  String ->
  EAPP.ApplicationCall ->
  IO EAPP.ApplicationCallCompletion
awaitStep15ApplicationCallWithin context call = do
  observed <-
    newEmptyMVar ::
      IO (MVar (Either SomeException EAPP.ApplicationCallCompletion))
  void (forkFinally (EAPP.awaitApplicationCall call) (putMVar observed))
  timeout applicationCallTimeoutMicroseconds (readMVar observed) >>= \case
    Nothing -> do
      EAPP.cancelApplicationCall call
      assertFailure (context <> ": application call exceeded its failure bound")
    Just (Left exception) -> throwIO exception
    Just (Right completion) -> pure completion

requireApplicationResult ::
  String ->
  Either EAPP.ApplicationRuntimeFailure result ->
  IO result
requireApplicationResult name = \case
  Left failure -> assertFailure (name <> " application runtime failed: " <> show failure)
  Right result -> pure result

unexpected :: (Show actual) => String -> actual -> IO value
unexpected context actual =
  assertFailure (context <> ": unexpected application result " <> show actual)

applicationCallTimeoutMicroseconds :: Int
applicationCallTimeoutMicroseconds = 5_000_000

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
