{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reusable real-EAPP setup for the Step-14 release scenarios.
--
-- P owns a dynamic Nabla, Q owns the matching dynamic Delta, and P publishes a
-- preserving Edge from the former to P's localization of the latter. The
-- helpers deliberately retain the application calls for operations that a test
-- may need to observe while a structural or label fence is open.
module Step14ApplicationWorkflow
  ( Step14ApplicationTopology (..),
    establishStep14ApplicationTopology,
    publishStep14Message,
    step14MessageValue,
    readStep14Messages,
    awaitStep14Message,
    readStep14SequencingCarrier,
    readStep14NablaCarrier,
    readStep14EdgeCarrier,
    expectStep14ApplicationLabel,
    expectStep14LabelResult,
    expectStep14WriteAccepted,
    expectStep14SortDefinition,
    expectStep14Read,
  )
where

import Control.Concurrent (threadDelay)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Runtime qualified as EAPP
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (..),
    ApplicationStartupAccess,
    PredefinedAccess,
    predefinedReader,
    predefinedWriter,
    startupAccessProcess,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    PrivateNablaId,
    PrivateProcessId,
    PrivateUniqueId,
    SortId,
    asPrivateDeltaId,
    asPrivateNablaId,
    sortIdBytes,
  )
import Eclips.Application.Types.Label (LabelResult)
import Eclips.Application.Types.NewId (NewIdTarget (ControlledNewId))
import Eclips.Application.Types.Query
  ( ApplicationProjection (..),
    ApplicationQuery (..),
    ApplicationQueryLiteral (QueryBytes, QueryText, QueryUniqueId),
    ApplicationQueryPredicate (QueryAlways, QueryCompare),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (..),
    WaitResult (WaitReady),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationScalarComparison (ScalarEqual),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value
  ( ApplicationLabel,
    ApplicationLabelOwner (ProcessLabel),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten, WriteAccepted),
  )
import Test.Tasty.HUnit (assertEqual, assertFailure)

-- | Process-private handles established by the common P/Q topology.
data Step14ApplicationTopology = Step14ApplicationTopology
  { messageSort :: SortId,
    pSequencingObject :: PrivateUniqueId,
    pNablaObject :: PrivateUniqueId,
    qDeltaObject :: PrivateUniqueId,
    pEdgeObject :: PrivateUniqueId,
    pNablaWriter :: PrivateNablaId,
    qMessageReader :: PrivateDeltaId,
    pSequencingCarrierReader :: PrivateDeltaId,
    pNablaCarrierReader :: PrivateDeltaId,
    pEdgeCarrierReader :: PrivateDeltaId,
    pSortReader :: PrivateDeltaId,
    qSortWriter :: PrivateNablaId,
    localizedQDelta :: PrivateUniqueId,
    localizedQProcess :: PrivateProcessId
  }
  deriving stock (Eq, Show)

-- | Establish the hello-world topology without publishing an ordinary value.
-- Tests can therefore place ordinary writes exactly before or after a fence.
establishStep14ApplicationTopology ::
  EAPP.Application ->
  ApplicationStartupAccess ->
  EAPP.Application ->
  ApplicationStartupAccess ->
  IO Step14ApplicationTopology
establishStep14ApplicationTopology pApplication pStartup qApplication qStartup = do
  pSortAccess <- accessFor "P" SortDefinitionRole pStartup
  qSortAccess <- accessFor "Q" SortDefinitionRole qStartup
  pNeutralAccess <- accessFor "P" NeutralVertexRole pStartup
  pNablaAccess <- accessFor "P" NablaRole pStartup
  qDeltaAccess <- accessFor "Q" DeltaRole qStartup
  pDeltaAccess <- accessFor "P" DeltaRole pStartup
  pEdgeAccess <- accessFor "P" EdgeRole pStartup

  publishedMessageSort <-
    expectStep14SortDefinition "P message sort publication"
      =<< EAPP.write
        pApplication
        (predefinedWriter pSortAccess)
        ( PublishValue
            ( SortDefinitionValue
                (DeclaredSortDefinition messageDescriptor Nothing)
            )
        )
  awaitSortDefinition
    qApplication
    (predefinedReader qSortAccess)
    publishedMessageSort
  publishedSequencingObject <-
    reserveAndPublishCarrier
      "P sequencing vertex"
      pApplication
      (predefinedWriter pNeutralAccess)
      (neutralCarrier (startupAccessProcess pStartup))
  publishedPNablaObject <-
    reserveAndPublishCarrier
      "P Nabla carrier"
      pApplication
      (predefinedWriter pNablaAccess)
      ( nablaCarrier
          (startupAccessProcess pStartup)
          publishedMessageSort
          publishedSequencingObject
      )
  publishedQDeltaObject <-
    reserveAndPublishCarrier
      "Q Delta carrier"
      qApplication
      (predefinedWriter qDeltaAccess)
      (deltaCarrier (startupAccessProcess qStartup) publishedMessageSort)
  (publishedLocalizedQDelta, publishedLocalizedQProcess) <-
    awaitLocalizedDeltaCarrier
      pApplication
      (predefinedReader pDeltaAccess)
      publishedMessageSort
  publishedPEdgeObject <-
    reserveAndPublishCarrier
      "P preserving Edge carrier"
      pApplication
      (predefinedWriter pEdgeAccess)
      ( preservingEdgeCarrier
          (startupAccessProcess pStartup)
          publishedPNablaObject
          publishedLocalizedQDelta
      )
  pure
    Step14ApplicationTopology
      { messageSort = publishedMessageSort,
        pSequencingObject = publishedSequencingObject,
        pNablaObject = publishedPNablaObject,
        qDeltaObject = publishedQDeltaObject,
        pEdgeObject = publishedPEdgeObject,
        pNablaWriter = asPrivateNablaId publishedPNablaObject,
        qMessageReader = asPrivateDeltaId publishedQDeltaObject,
        pSequencingCarrierReader = predefinedReader pNeutralAccess,
        pNablaCarrierReader = predefinedReader pNablaAccess,
        pEdgeCarrierReader = predefinedReader pEdgeAccess,
        pSortReader = predefinedReader pSortAccess,
        qSortWriter = predefinedWriter qSortAccess,
        localizedQDelta = publishedLocalizedQDelta,
        localizedQProcess = publishedLocalizedQProcess
      }

-- | Submit an ordinary publication and retain its terminal application call.
publishStep14Message ::
  EAPP.Application ->
  Step14ApplicationTopology ->
  Text ->
  IO EAPP.ApplicationCall
publishStep14Message application topology message =
  EAPP.write
    application
    (pNablaWriter topology)
    (PublishValue (step14MessageValue message))

-- | The single-field value admitted by the common message descriptor.
step14MessageValue :: Text -> ApplicationValue
step14MessageValue message =
  RecordValue (Map.singleton "message" (TextValue message))

-- | Read Q's current cut of the common message sort.
readStep14Messages ::
  EAPP.Application ->
  Step14ApplicationTopology ->
  IO [ApplicationValue]
readStep14Messages application topology =
  expectStep14Read "Q message observation"
    =<< EAPP.read application (queryAlways (qMessageReader topology))

-- | Wait until Q's current cut contains the expected ordinary publication.
awaitStep14Message ::
  EAPP.Application ->
  Step14ApplicationTopology ->
  Text ->
  IO ()
awaitStep14Message application topology message = do
  completion <-
    EAPP.awaitApplicationCall
      =<< EAPP.wait application [query]
  case completion of
    EAPP.ApplicationCallSucceeded (WaitCompleted WaitReady) -> pure ()
    other -> unexpected "Q message wait" other
  where
    query =
      ApplicationQuery
        { applicationQueryDeltas = Set.singleton (qMessageReader topology),
          applicationQueryPredicate =
            QueryCompare
              (ApplicationProjection ("message" :| []))
              ScalarEqual
              (QueryText message)
        }

-- | Read the independent structural object named by P's sequencing relation.
readStep14SequencingCarrier ::
  EAPP.Application ->
  Step14ApplicationTopology ->
  IO ApplicationValue
readStep14SequencingCarrier application topology = do
  observed <-
    readStep14Carrier
      "P sequencing-vertex observation"
      application
      (pSequencingCarrierReader topology)
      (pSequencingObject topology)
  maybe (assertFailure "P sequencing vertex is absent") pure observed

-- | Read P's own Nabla structural carrier by its process-private object ID.
readStep14NablaCarrier ::
  EAPP.Application ->
  Step14ApplicationTopology ->
  IO ApplicationValue
readStep14NablaCarrier application topology = do
  observed <-
    readStep14Carrier
      "P Nabla-carrier observation"
      application
      (pNablaCarrierReader topology)
      (pNablaObject topology)
  maybe (assertFailure "P Nabla carrier is absent") pure observed

-- | Read P's preserving Edge carrier, returning 'Nothing' once label deletion
-- removes it from the application-visible predefined Edge store.
readStep14EdgeCarrier ::
  EAPP.Application ->
  Step14ApplicationTopology ->
  IO (Maybe ApplicationValue)
readStep14EdgeCarrier application topology =
  readStep14Carrier
    "P Edge-carrier observation"
    application
    (pEdgeCarrierReader topology)
    (pEdgeObject topology)

readStep14Carrier ::
  String ->
  EAPP.Application ->
  PrivateDeltaId ->
  PrivateUniqueId ->
  IO (Maybe ApplicationValue)
readStep14Carrier context application reader object = do
  values <-
    expectStep14Read context
      =<< EAPP.read
        application
        ApplicationQuery
          { applicationQueryDeltas = Set.singleton reader,
            applicationQueryPredicate =
              QueryCompare
                (ApplicationProjection ("object_id" :| []))
                ScalarEqual
                (QueryUniqueId object)
          }
  case values of
    [] -> pure Nothing
    [value] -> pure (Just value)
    _ -> unexpected (context <> " set") values

-- | Assert the canonical @label@ field of an application record value.
expectStep14ApplicationLabel ::
  String ->
  ApplicationLabel ->
  ApplicationValue ->
  IO ()
expectStep14ApplicationLabel context expected = \case
  RecordValue fields ->
    case Map.lookup "label" fields of
      Just (LabelValue actual) -> assertEqual context expected actual
      actual -> unexpected (context <> " label field") actual
  actual -> unexpected context actual

-- | Await and assert either terminal label outcome.
expectStep14LabelResult ::
  String ->
  LabelResult ->
  EAPP.ApplicationCall ->
  IO ()
expectStep14LabelResult context expected call = do
  completion <- EAPP.awaitApplicationCall call
  case completion of
    EAPP.ApplicationCallSucceeded (LabelCompleted actual) ->
      assertEqual context expected actual
    other -> unexpected context other

-- | Await an ordinary or structural publication's accepted result.
expectStep14WriteAccepted :: String -> EAPP.ApplicationCall -> IO ()
expectStep14WriteAccepted context call = do
  completion <- EAPP.awaitApplicationCall call
  case completion of
    EAPP.ApplicationCallSucceeded (WriteCompleted WriteAccepted) -> pure ()
    other -> unexpected context other

-- | Await a sort-definition publication and return its public content ID.
expectStep14SortDefinition :: String -> EAPP.ApplicationCall -> IO SortId
expectStep14SortDefinition context call = do
  completion <- EAPP.awaitApplicationCall call
  case completion of
    EAPP.ApplicationCallSucceeded
      (WriteCompleted (SortDefinitionWritten identifier)) -> pure identifier
    other -> unexpected context other

-- | Await a read and unwrap its terminal values.
expectStep14Read :: String -> EAPP.ApplicationCall -> IO [ApplicationValue]
expectStep14Read context call = do
  completion <- EAPP.awaitApplicationCall call
  case completion of
    EAPP.ApplicationCallSucceeded (ReadCompleted values) -> pure values
    other -> unexpected context other

messageDescriptor :: ApplicationSortDescriptor
messageDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "message" TextSchema),
      keyProjections = [ApplicationProjection ("message" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

nablaCarrier ::
  PrivateProcessId ->
  SortId ->
  PrivateUniqueId ->
  PrivateUniqueId ->
  ApplicationValue
nablaCarrier process sortId sequencingObject object =
  -- The application explicitly supplies the object whose label fence orders
  -- this Nabla. Herald does not synthesize a catalogue or identity fallback.
  RecordValue
    ( Map.fromList
        [ ("label", LabelValue ((ProcessLabel process, 0))),
          ("object_id", UniqueIdValue object),
          ("sequencing_object", OptionalUniqueIdValue (Just sequencingObject)),
          ("sort_id", BytesValue (sortIdBytes sortId))
        ]
    )

neutralCarrier ::
  PrivateProcessId ->
  PrivateUniqueId ->
  ApplicationValue
neutralCarrier process object =
  RecordValue
    ( Map.fromList
        [ ("label", LabelValue ((ProcessLabel process, 0))),
          ("object_id", UniqueIdValue object)
        ]
    )

deltaCarrier ::
  PrivateProcessId ->
  SortId ->
  PrivateUniqueId ->
  ApplicationValue
deltaCarrier process sortId object =
  RecordValue
    ( Map.fromList
        [ ("label", LabelValue ((ProcessLabel process, 0))),
          ("object_id", UniqueIdValue object),
          ("sort_id", BytesValue (sortIdBytes sortId))
        ]
    )

preservingEdgeCarrier ::
  PrivateProcessId ->
  PrivateUniqueId ->
  PrivateUniqueId ->
  PrivateUniqueId ->
  ApplicationValue
preservingEdgeCarrier process source destination object =
  RecordValue
    ( Map.fromList
        [ ("destination_vertex", UniqueIdValue destination),
          ("label", LabelValue ((ProcessLabel process, 0))),
          ("object_id", UniqueIdValue object),
          ("source_vertex", UniqueIdValue source),
          ("strength", EnumValue "preserve")
        ]
    )

reserveAndPublishCarrier ::
  String ->
  EAPP.Application ->
  PrivateNablaId ->
  (PrivateUniqueId -> ApplicationValue) ->
  IO PrivateUniqueId
reserveAndPublishCarrier context application writer carrier = do
  object <-
    expectNewId (context <> " reservation")
      =<< EAPP.newid application (ControlledNewId writer)
  expectStep14WriteAccepted (context <> " publication")
    =<< EAPP.write application writer (PublishValue (carrier object))
  pure object

awaitSortDefinition ::
  EAPP.Application ->
  PrivateDeltaId ->
  SortId ->
  IO ()
awaitSortDefinition application reader sortId = poll
  where
    expected =
      SortDefinitionValue
        (DeclaredSortDefinition messageDescriptor (Just sortId))
    poll = do
      values <-
        expectStep14Read "Q sort-definition observation"
          =<< EAPP.read application (queryAlways reader)
      if expected `elem` values
        then pure ()
        else causalRetry poll

awaitLocalizedDeltaCarrier ::
  EAPP.Application ->
  PrivateDeltaId ->
  SortId ->
  IO (PrivateUniqueId, PrivateProcessId)
awaitLocalizedDeltaCarrier application reader sortId = poll
  where
    query =
      ApplicationQuery
        { applicationQueryDeltas = Set.singleton reader,
          applicationQueryPredicate =
            QueryCompare
              (ApplicationProjection ("sort_id" :| []))
              ScalarEqual
              (QueryBytes (sortIdBytes sortId))
        }
    poll = do
      values <-
        expectStep14Read "P localized Q Delta-carrier observation"
          =<< EAPP.read application query
      case values of
        [] -> causalRetry poll
        [value] -> case localizedDeltaIdentity sortId value of
          Just identity -> pure identity
          Nothing -> unexpected "P localized Q Delta carrier" value
        _ -> unexpected "P localized Q Delta carrier set" values

localizedDeltaIdentity ::
  SortId ->
  ApplicationValue ->
  Maybe (PrivateUniqueId, PrivateProcessId)
localizedDeltaIdentity sortId (RecordValue fields)
  | Map.size fields == 3,
    Just (UniqueIdValue object) <- Map.lookup "object_id" fields,
    Just (LabelValue ((ProcessLabel process, _))) <- Map.lookup "label" fields,
    Just (BytesValue observedSort) <- Map.lookup "sort_id" fields,
    observedSort == sortIdBytes sortId =
      Just (object, process)
localizedDeltaIdentity _ _ = Nothing

queryAlways :: PrivateDeltaId -> ApplicationQuery
queryAlways reader =
  ApplicationQuery
    { applicationQueryDeltas = Set.singleton reader,
      applicationQueryPredicate = QueryAlways
    }

accessFor ::
  String ->
  ApplicationPredefinedSortRole ->
  ApplicationStartupAccess ->
  IO PredefinedAccess
accessFor process role startup =
  case (Map.lookup (Access.environmentWriterKey role) (Access.accessEntries (Access.startupAccessPrimordial startup)), Map.lookup (Access.environmentReaderKey role) (Access.accessEntries (Access.startupAccessPrimordial startup))) of
    (Just (Access.Writer writer), Just (Access.Reader reader)) -> pure (Access.predefinedAccess role writer reader)
    _ ->
      assertFailure
        (process <> " startup access is missing " <> show role)

expectNewId :: String -> EAPP.ApplicationCall -> IO PrivateUniqueId
expectNewId context call = do
  completion <- EAPP.awaitApplicationCall call
  case completion of
    EAPP.ApplicationCallSucceeded (NewIdCompleted identifier) -> pure identifier
    other -> unexpected context other

causalRetry :: IO value -> IO value
causalRetry retry = threadDelay 10000 >> retry

unexpected :: (Show actual) => String -> actual -> IO value
unexpected context actual =
  assertFailure (context <> ": unexpected application result " <> show actual)
