{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Real EAPP operations and an explicit transport latch for the Slice-6 tour.
-- These helpers inspect calls, frames and trace evidence, never a running kernel.
module Step16TourFixtures
  ( withHeldH1EnvironmentDestination,
    awaitHeldH1EnvironmentDestination,
    releaseHeldH1EnvironmentDestination,
    awaitTourEnvironment,
    tourEnvironmentRole,
    publishTourDefinition,
    connectTourPrivatePair,
    tourDefinition,
    readTourAccess,
    awaitTourDefinition,
    readTourDefinition,
    takeTourDefinition,
    takeTourObject,
    withinTourProgress,
  ) where

import Control.Concurrent (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole,
    EnvironmentAccess,
    PredefinedAccess,
    environmentAccessPredefined,
    predefinedAccessRole,
    predefinedReader,
    predefinedWriter,
  )
import Eclips.Application.Types.Identity (PrivateProcessId, PrivateUniqueId, SortId, privateDeltaUniqueId, privateNablaUniqueId)
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryLiteral (QueryUniqueId),
    ApplicationQueryPredicate (QueryAlways, QueryCompare),
  )
import Eclips.Application.Types.Result (RegularCallResult (..))
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationScalarComparison (ScalarEqual),
    ApplicationSortDefinition (DeclaredSortDefinition, PredefinedSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value (ApplicationLabelOwner (ProcessLabel), ApplicationValue (..))
import Eclips.Application.Types.Write (ApplicationWriteValue (PublishValue), WriteResult (SortDefinitionWritten, WriteAccepted))
import Eclips.Herald.Discovery (peerBindingRemoteHeraldEpoch)
import Eclips.Herald.Peer.RPC (projectPeerLogicalAttempt)
import Eclips.Herald.Runtime.TCP.Internal.Peer (setPeerPublicationProbeForTest)
import Eclips.Protocol.Peer.Types qualified as Protocol
import Step15Applications (awaitStep15ApplicationCallWithin)
import Step15Configuration (Step15HeraldName (Step15H4), step15HeraldEpoch)
import Step15Fixtures (Step15Deployment, step15H1, step15ManagedHeraldTcp)
import System.Timeout (timeout)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)

data HeldEnvironmentDestination = HeldEnvironmentDestination (MVar Protocol.StructuralOccurrenceStampDto) (MVar ())

withHeldH1EnvironmentDestination :: Step15Deployment -> (HeldEnvironmentDestination -> IO a) -> IO a
withHeldH1EnvironmentDestination deployment = bracket install cleanup
  where
    tcp = step15ManagedHeraldTcp (step15H1 deployment)
    install = do
      observed <- newEmptyMVar
      released <- newEmptyMVar
      setPeerPublicationProbeForTest tcp . Just $ \binding attempt ->
        when (peerBindingRemoteHeraldEpoch binding == step15HeraldEpoch Step15H4)
          $ case projectPeerLogicalAttempt attempt of
            Right (Protocol.PeerPublicationEnvelope _ (Protocol.PeerPublicationDto _ _ _ (Protocol.StructuralPublicationDto stamp@(Protocol.StructuralOccurrenceStampDto _ _ _ _ role) _)))
              | role `elem` [Protocol.NablaCarrierDto, Protocol.DeltaCarrierDto] -> do
                  void (tryPutMVar observed stamp)
                  readMVar released
            _ -> pure ()
      pure (HeldEnvironmentDestination observed released)
    cleanup gate = do
      releaseHeldH1EnvironmentDestination gate
      setPeerPublicationProbeForTest tcp Nothing

awaitHeldH1EnvironmentDestination :: HeldEnvironmentDestination -> IO Protocol.StructuralOccurrenceStampDto
awaitHeldH1EnvironmentDestination (HeldEnvironmentDestination observed _) =
  withinTourProgress "accepted newenv did not reach its H4 structural handoff" (readMVar observed)

releaseHeldH1EnvironmentDestination :: HeldEnvironmentDestination -> IO ()
releaseHeldH1EnvironmentDestination (HeldEnvironmentDestination _ released) = void (tryPutMVar released ())

awaitTourEnvironment :: String -> EAPP.ApplicationCall -> IO EnvironmentAccess
awaitTourEnvironment label call =
  awaitStep15ApplicationCallWithin label call >>= \case
    EAPP.ApplicationCallSucceeded (NewEnvironmentCompleted access) -> pure access
    other -> assertFailure (label <> ": " <> show other)

tourEnvironmentRole :: ApplicationPredefinedSortRole -> EnvironmentAccess -> IO PredefinedAccess
tourEnvironmentRole role environment =
  maybe
    (assertFailure ("environment missing " <> show role))
    pure
    (find ((== role) . predefinedAccessRole) (environmentAccessPredefined environment))

tourDefinition :: Text -> ApplicationSortDefinition
tourDefinition field =
  DeclaredSortDefinition
    ApplicationSortDescriptor
      { sortKind = RegularSort,
        valueSchema = RecordSchema (Map.singleton field TextSchema),
        keyProjections = [ApplicationProjection (field :| [])],
        validityPredicate = AlwaysPredicate,
        obsolescencePredicate = NeverPredicate,
        rankTerms = RankApplicationValue Ascending :| [],
        minimumRetentionMicros = 0,
        isImmutable = False,
        labelField = Nothing
      }
    Nothing

publishTourDefinition :: String -> EAPP.Application -> PredefinedAccess -> ApplicationSortDefinition -> IO SortId
publishTourDefinition label app access definition = do
  call <- EAPP.write app (predefinedWriter access) (PublishValue (SortDefinitionValue definition))
  awaitStep15ApplicationCallWithin label call >>= \case
    EAPP.ApplicationCallSucceeded (WriteCompleted (SortDefinitionWritten sortId)) -> pure sortId
    other -> assertFailure (label <> ": " <> show other)

-- New environments supply roots and capabilities, with no implicit edges.
-- Connect just the owner's chosen private pair through an ordinary controlled
-- Edge publication; this discloses no path to either original environment.
connectTourPrivatePair :: EAPP.Application -> PrivateProcessId -> PredefinedAccess -> PredefinedAccess -> IO ()
connectTourPrivatePair app process edgeAccess pair = do
  objectCall <- EAPP.newid app (ControlledNewId (predefinedWriter edgeAccess))
  object <-
    awaitStep15ApplicationCallWithin "private edge reservation" objectCall >>= \case
      EAPP.ApplicationCallSucceeded (NewIdCompleted identifier) -> pure identifier
      other -> assertFailure ("private edge reservation: " <> show other)
  let carrier =
        RecordValue
          ( Map.fromList
              [ ("destination_vertex", UniqueIdValue (privateDeltaUniqueId (predefinedReader pair))),
                ("label", LabelValue ((ProcessLabel process, 0))),
                ("object_id", UniqueIdValue object),
                ("source_vertex", UniqueIdValue (privateNablaUniqueId (predefinedWriter pair))),
                ("strength", EnumValue "preserve")
              ]
          )
  call <- EAPP.write app (predefinedWriter edgeAccess) (PublishValue carrier)
  awaitStep15ApplicationCallWithin "private edge publication" call >>= \case
    EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted) -> pure ()
    other -> assertFailure ("private edge publication: " <> show other)

readTourAccess :: String -> EAPP.Application -> PredefinedAccess -> IO [ApplicationValue]
readTourAccess label app access = readTourQuery label app (query access QueryAlways)

readTourQuery :: String -> EAPP.Application -> ApplicationQuery -> IO [ApplicationValue]
readTourQuery label app selected = do
  call <- EAPP.read app selected
  awaitStep15ApplicationCallWithin label call >>= \case
    EAPP.ApplicationCallSucceeded (ReadCompleted values) -> pure values
    other -> assertFailure (label <> ": " <> show other)

-- The caller first closes the exact finite network wave. Sort-definition
-- readers intentionally support no field projections, so this single read
-- compares the checked SortId carried by the public returned definition.
awaitTourDefinition :: String -> EAPP.Application -> PredefinedAccess -> SortId -> IO ()
awaitTourDefinition label app access sortId = do
  values <- readTourDefinition label app access sortId
  assertEqual (label <> " has exactly the requested definition") 1 (length values)

readTourDefinition :: String -> EAPP.Application -> PredefinedAccess -> SortId -> IO [ApplicationValue]
readTourDefinition label app access sortId =
  filter (matchesDefinition sortId) <$> readTourAccess label app access

takeTourDefinition :: String -> EAPP.Application -> PredefinedAccess -> SortId -> IO [ApplicationValue]
takeTourDefinition label app access sortId = do
  values <- readTourAccess (label <> " declared-definition precondition") app access
  let declared = filter (matchesDefinition sortId) values
  assertEqual (label <> " reader has one intended declared definition") 1 (length declared)
  assertBool
    (label <> " all other visible definitions belong to the closed predefined catalogue")
    (all (\value -> matchesDefinition sortId value || predefinedDefinition value) values)
  -- The public sort-sort query deliberately has no descriptor projections.
  -- Taking this reader also removes its local predefined catalogue copies;
  -- those closed definitions never create regular retirement candidates.
  taken <- takeTourQuery label app (query access QueryAlways)
  assertEqual (label <> " removes exactly the observed local definition copies") values taken
  pure (filter (matchesDefinition sortId) taken)
  where
    predefinedDefinition (SortDefinitionValue PredefinedSortDefinition {}) = True
    predefinedDefinition _ = False

matchesDefinition :: SortId -> ApplicationValue -> Bool
matchesDefinition expected (SortDefinitionValue (DeclaredSortDefinition _ (Just actual))) = actual == expected
matchesDefinition _ _ = False

takeTourObject :: String -> EAPP.Application -> PredefinedAccess -> PrivateUniqueId -> IO [ApplicationValue]
takeTourObject label app access object =
  takeTourQuery
    label
    app
    (query access (QueryCompare (ApplicationProjection ("object_id" :| [])) ScalarEqual (QueryUniqueId object)))

takeTourQuery :: String -> EAPP.Application -> ApplicationQuery -> IO [ApplicationValue]
takeTourQuery label app selected = do
  call <- EAPP.localTake app selected
  awaitStep15ApplicationCallWithin label call >>= \case
    EAPP.ApplicationCallSucceeded (LocalTakeCompleted values) -> pure values
    other -> assertFailure (label <> ": " <> show other)

query :: PredefinedAccess -> ApplicationQueryPredicate -> ApplicationQuery
query access predicate = ApplicationQuery (Set.singleton (predefinedReader access)) predicate

withinTourProgress :: String -> IO a -> IO a
withinTourProgress label action = timeout 10_000_000 action >>= maybe (assertFailure label) pure
