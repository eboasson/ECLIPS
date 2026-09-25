{-# LANGUAGE OverloadedStrings #-}

-- | The ordinary eight-operation workflow used by the singleton founder tour.
module HelloApplication
  ( runFounderApplication,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Typed qualified as App
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    EnvironmentAccess,
    PredefinedAccess,
    allApplicationPredefinedSortRoles,
    environmentAccessPredefined,
    predefinedAccessRole,
    predefinedReader,
    predefinedWriter,
    startupAccessProcess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateUniqueId,
    privateDeltaUniqueId,
    privateNablaUniqueId,
    privateObjectUniqueId,
    privateProcessUniqueId,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToVoid),
    LabelResult (LabelApplied),
  )
import Eclips.Application.Types.Result (WaitResult (WaitReady))
import Eclips.Application.Types.Value (ApplicationLabelOwner (ProcessLabel))
import Eclips.Application.Types.Write (WriteResult (WriteAccepted))
import HelloCommon (Message (..), messageQuery, messageSort)

runFounderApplication :: App.ConnectionDescriptor -> IO Text
runFounderApplication descriptor = do
  result <- App.withHerald descriptor $ \application -> do
    greeting <- runHelloWorkflow application application
    expect "founder launcher End" =<< App.endProcess application
    pure greeting
  either (applicationFailure "founder launcher") pure result

runHelloWorkflow :: App.Herald -> App.Herald -> IO Text
runHelloWorkflow pApplication qApplication = do
  privateEnvironment <- expect "P private environment" =<< App.newEnvironment pApplication
  checkEnvironmentAccess "P private environment" (App.startup pApplication) (App.environmentAccess privateEnvironment)
  exerciseEnvironmentRoot privateEnvironment

  pEnvironment <- expect "P startup environment" (App.startupEnvironment pApplication)
  qEnvironment <- expect "Q startup environment" (App.startupEnvironment qApplication)
  definition <- expect "message sort" messageSort
  _ <- expect "P message sort publication" =<< App.declareSort pEnvironment definition
  awaitSortDefinition qEnvironment definition
  writer <- expect "P Nabla carrier" =<< App.createNabla pEnvironment definition Nothing
  reader <- expect "Q Delta carrier" =<< App.createDelta qEnvironment definition
  localizedReader <- awaitLocalizedDelta pEnvironment definition
  edge <- expect "P preserving Edge carrier" =<< App.createEdge pEnvironment App.Preserve (App.nablaVertex writer) (App.deltaVertex localizedReader)
  expectForwardAccepted "P preserving Edge forwarding" =<< App.forwardEdge edge
  expectWriteAccepted "P greeting publication" =<< App.write writer (Message helloText)
  greeting <- awaitGreeting reader
  expectLocalTake "Q greeting local take" [Message helloText] =<< App.localTake (messageQuery reader helloText)
  expectLabelApplied "P Nabla label-to-void"
    =<< App.labelVertex
      (App.nablaVertex writer)
      (ProcessLabel (startupAccessProcess (App.startup pApplication)), 0)
      LabelToVoid
  pure greeting

checkEnvironmentAccess :: String -> ApplicationStartupAccess -> EnvironmentAccess -> IO ()
checkEnvironmentAccess context startup environment = do
  unless (fmap predefinedAccessRole accesses == allApplicationPredefinedSortRoles)
    $ unexpected (context <> " role order") (fmap predefinedAccessRole accesses)
  unless (Set.size environmentIdentifiers == 31)
    $ unexpected (context <> " duplicate private environment objects") accesses
  unless (Set.disjoint environmentIdentifiers startupIdentifiers)
    $ unexpected (context <> " aliases a startup object") accesses
  where
    accesses = environmentAccessPredefined environment
    environmentIdentifiers = Set.fromList (accessIdentifiers accesses <> fmap privateObjectUniqueId (Access.environmentAccessHub environment : Access.environmentAccessEdges environment))
    startupIdentifiers =
      Set.fromList
        ( privateProcessUniqueId (startupAccessProcess startup)
            : map Access.primordialEntryIdentity (Map.elems (Access.accessEntries (Access.startupAccessPrimordial startup)))
        )

exerciseEnvironmentRoot :: App.Environment -> IO ()
exerciseEnvironmentRoot environment = do
  reserved <- expect "P private environment reservation" =<< App.reserveEdge environment
  expectWriteAccepted "P private environment reservation deletion" =<< App.cancelReservedEdge reserved

accessIdentifiers :: [PredefinedAccess] -> [PrivateUniqueId]
accessIdentifiers =
  concatMap
    ( \access ->
        [ privateNablaUniqueId (predefinedWriter access),
          privateDeltaUniqueId (predefinedReader access)
        ]
    )

helloText :: Text
helloText = "Hello, world!"

awaitSortDefinition :: App.Environment -> App.Sort Message -> IO ()
awaitSortDefinition environment definition = do
  visible <- expect "Q sort-definition observation" =<< App.sortVisible environment definition
  -- Sort definitions have a structured carrier without field projections.
  -- Poll until this exact definition is visible, without blocking on another sort.
  unless visible (threadDelay 10_000 >> awaitSortDefinition environment definition)

awaitLocalizedDelta :: App.Environment -> App.Sort Message -> IO (App.Delta Message)
awaitLocalizedDelta environment definition = do
  readers <- expect "P localized Q Delta-carrier observation" =<< App.discoverDeltas environment definition
  case readers of
    [] -> threadDelay 10_000 >> awaitLocalizedDelta environment definition
    [reader] -> pure reader
    _ -> unexpected "P localized Q Delta carrier count" (length readers)

awaitGreeting :: App.Delta Message -> IO Text
awaitGreeting reader = do
  completion <- App.wait (App.SomeQuery query :| [])
  case completion of
    Right WaitReady -> pure ()
    other -> unexpected "Q greeting observation" other
  values <- expect "Q greeting observation" =<< App.read query
  case values of
    [Message value] | value == helloText -> pure value
    _ -> unexpected "Q greeting value set" values
  where
    query = messageQuery reader helloText

expectWriteAccepted :: String -> Either App.TypedError WriteResult -> IO ()
expectWriteAccepted _ (Right WriteAccepted) = pure ()
expectWriteAccepted context other = unexpected context other

expectForwardAccepted :: String -> Either App.TypedError ForwardResult -> IO ()
expectForwardAccepted _ (Right ForwardAccepted) = pure ()
expectForwardAccepted context other = unexpected context other

expectLabelApplied :: String -> Either App.TypedError LabelResult -> IO ()
expectLabelApplied _ (Right LabelApplied) = pure ()
expectLabelApplied context other = unexpected context other

expectLocalTake :: String -> [Message] -> Either App.TypedError [Message] -> IO ()
expectLocalTake _ expected (Right actual) | expected == actual = pure ()
expectLocalTake context _ other = unexpected context other

expect :: (Show problem) => String -> Either problem value -> IO value
expect _ (Right value) = pure value
expect context (Left problem) = unexpected context problem

applicationFailure :: String -> App.ConnectError -> IO value
applicationFailure process failure =
  ioError (userError (process <> " application failed: " <> show failure))

unexpected :: (Show actual) => String -> actual -> IO value
unexpected context actual =
  ioError (userError (context <> ": unexpected application result " <> show actual))
