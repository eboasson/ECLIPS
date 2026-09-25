{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- | Checked application types bound to their originating connection. Constructors
-- stay private: a private ID and a caller-provided SortId are not binding evidence.
module Eclips.Application.Typed.Internal
  ( TypedError (..),
    Nabla,
    Delta,
    Query,
    SomeQuery (..),
    Environment,
    Vertex,
    Edge,
    EdgeStrength (..),
    Sequencer,
    sequenceOn,
    sequenceOnVertex,
    sequenceOnEdge,
    EdgeReservation,
    Creation,
    Reservation,
    Object,
    nablaId,
    deltaId,
    nablaVertex,
    deltaVertex,
    vertexId,
    edgeId,
    reservationId,
    objectId,
    creationId,
    bindNabla,
    bindDelta,
    startupEnvironment,
    environmentSelection,
    environmentAccess,
    environmentHub,
    environmentEdges,
    newEnvironment,
    query,
    queryMany,
    Operation (..),
    Call,
    CallStatus (..),
    submit,
    await,
    cancel,
    status,
    write,
    read,
    readObjects,
    localTake,
    wait,
    newid,
    forward,
    label,
    reserve,
    cancelReservation,
    publishReserved,
    update,
    declareSort,
    sortVisible,
    createNabla,
    createDelta,
    createVertex,
    createEdge,
    reserveNabla,
    reserveDelta,
    reserveVertex,
    reserveEdgeFor,
    publishCreation,
    cancelCreation,
    forwardEdge,
    labelVertex,
    labelEdge,
    reserveEdge,
    cancelReservedEdge,
    discoverDeltas,
    discoverNablas,
  ) where

import Control.Monad (unless)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Internal qualified as Raw
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Forward (ForwardResult)
import Eclips.Application.Types.Identity
import Eclips.Application.Types.Label (ApplicationLabelTarget, LabelResult)
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Query qualified as Q
import Eclips.Application.Types.Result (WaitResult)
import Eclips.Application.Types.SortDescriptor qualified as D
import Eclips.Application.Types.Typed
import Eclips.Application.Types.Value (ApplicationLabel, ApplicationLabelOwner (..), ApplicationValue (..))
import Eclips.Application.Types.Write (ApplicationWriteValue (..), WriteResult (..))
import Eclips.Public.Types.SortCatalogue qualified as Catalogue
import Prelude hiding (read)

data TypedError
  = UnderlyingCall Raw.CallError
  | InvalidSort SortError
  | InvalidValue ValueError
  | MissingEndpoint Text
  | EndpointRoleMismatch Text
  | EndpointSortMismatch SortId SortId
  | MissingEndpointSort Text
  | MissingObject Text
  | ObjectRoleMismatch Text
  | ObjectSortMismatch SortId SortId
  | MissingObjectSort Text
  | DifferentSessions
  | UnexpectedValue
  | ReservationIdentityMismatch
  deriving stock (Eq, Show)

type role Nabla nominal
data Nabla a = Nabla Raw.Herald PrivateNablaId (Sort a)
type role Delta nominal
data Delta a = Delta Raw.Herald PrivateDeltaId (Sort a)
type role Query nominal
data Query a = Query Raw.Herald (Sort a) Q.ApplicationQuery

data SomeQuery where
  SomeQuery :: Query a -> SomeQuery

-- | Complete predefined interface whose roles were verified by its owner.
data Environment = Environment Raw.Herald Access.EnvironmentAccess

data Vertex = Vertex Raw.Herald PrivateUniqueId
data Sequencer = Sequencer Raw.Herald PrivateUniqueId

-- | An edge retains its writer so forwarding cannot switch namespaces.
data Edge = Edge Raw.Herald PrivateNablaId PrivateObjectId

data EdgeReservation = EdgeReservation Raw.Herald PrivateNablaId PrivateUniqueId

-- | A reserved structural identity, fixed carrier and result that is released
-- only after its publication succeeds. Retain this across a rejected publish.
type role Creation nominal

data Creation result = Creation Raw.Herald PrivateNablaId PrivateUniqueId ApplicationValue result

data EdgeStrength = Preserve | Weaken deriving stock (Eq, Ord, Show)

type role Reservation nominal
data Reservation a = Reservation (Nabla a) PrivateUniqueId
type role Object nominal
data Object a = Object Raw.Herald PrivateObjectId (Sort a)

nablaId :: Nabla a -> PrivateNablaId
nablaId (Nabla _ identifier _) = identifier
deltaId :: Delta a -> PrivateDeltaId
deltaId (Delta _ identifier _) = identifier
nablaVertex :: Nabla a -> Vertex
nablaVertex (Nabla herald identifier _) = Vertex herald (privateNablaUniqueId identifier)
deltaVertex :: Delta a -> Vertex
deltaVertex (Delta herald identifier _) = Vertex herald (privateDeltaUniqueId identifier)
vertexId :: Vertex -> PrivateUniqueId
vertexId (Vertex _ identifier) = identifier
edgeId :: Edge -> PrivateObjectId
edgeId (Edge _ _ identifier) = identifier
reservationId :: Reservation a -> PrivateUniqueId
reservationId (Reservation _ identifier) = identifier
objectId :: Object a -> PrivateObjectId
objectId (Object _ identifier _) = identifier
creationId :: Creation result -> PrivateUniqueId
creationId (Creation _ _ identifier _ _) = identifier
sequenceOn :: Object a -> Sequencer
sequenceOn (Object herald identifier _) = Sequencer herald (privateObjectUniqueId identifier)
sequenceOnVertex :: Vertex -> Sequencer
sequenceOnVertex (Vertex herald identifier) = Sequencer herald identifier
sequenceOnEdge :: Edge -> Sequencer
sequenceOnEdge (Edge herald _ identifier) = Sequencer herald (privateObjectUniqueId identifier)

-- | Only the startup access retained by the connected Herald is authoritative.
bindNabla :: forall a. (ApplicationSort a) => Raw.Herald -> Text -> Either TypedError (Nabla a)
bindNabla herald name = do
  definition <- either (Left . InvalidSort) Right (compileSort @a)
  case Map.lookup name (Access.accessEntries access) of
    Nothing -> Left (MissingEndpoint name)
    Just (Access.Writer identifier) -> do
      verifyStartupSort access name (sortId definition)
      pure (Nabla herald identifier definition)
    Just _ -> Left (EndpointRoleMismatch name)
  where
    access = Access.startupAccessPrimordial (Raw.startup herald)

bindDelta :: forall a. (ApplicationSort a) => Raw.Herald -> Text -> Either TypedError (Delta a)
bindDelta herald name = do
  definition <- either (Left . InvalidSort) Right (compileSort @a)
  case Map.lookup name (Access.accessEntries access) of
    Nothing -> Left (MissingEndpoint name)
    Just (Access.Reader identifier) -> do
      verifyStartupSort access name (sortId definition)
      pure (Delta herald identifier definition)
    Just _ -> Left (EndpointRoleMismatch name)
  where
    access = Access.startupAccessPrimordial (Raw.startup herald)

verifyStartupSort :: Access.PrimordialAccess -> Text -> SortId -> Either TypedError ()
verifyStartupSort access name expected = case Map.lookup name (Access.accessEndpointSorts access) of
  Nothing -> Left (MissingEndpointSort name)
  Just actual -> verifySort expected actual

verifySort :: SortId -> SortId -> Either TypedError ()
verifySort expected actual = unless (expected == actual) (Left (EndpointSortMismatch expected actual))

startupEnvironment :: Raw.Herald -> Either TypedError Environment
startupEnvironment herald = do
  pairs <- traverse pair Access.allApplicationPredefinedSortRoles
  hub <- object Access.environmentHubKey Access.NeutralVertexRole
  edges <- traverse (\(role, direction) -> object (Access.environmentEdgeKey role direction) Access.EdgeRole) [(role, direction) | role <- Access.allApplicationPredefinedSortRoles, direction <- Access.allEnvironmentEdgeRoles]
  case Access.environmentAccess pairs hub edges of
    Left _ -> Left UnexpectedValue
    Right environment -> Right (Environment herald environment)
  where
    access = Access.startupAccessPrimordial (Raw.startup herald)
    object name role = case Map.lookup name (Access.accessEntries access) of
      Nothing -> Left (MissingObject name)
      Just (Access.Object identifier) -> do
        actual <- maybe (Left (MissingObjectSort name)) Right (Map.lookup name (Access.accessObjectSorts access))
        let expected = predefinedSortId role
        unless (actual == expected) (Left (ObjectSortMismatch expected actual))
        pure identifier
      Just _ -> Left (ObjectRoleMismatch name)
    pair role = do
      let writerName = Access.environmentWriterKey role
          readerName = Access.environmentReaderKey role
          expected = predefinedSortId role
      verifyStartupSort access writerName expected
      verifyStartupSort access readerName expected
      case (Map.lookup writerName (Access.accessEntries access), Map.lookup readerName (Access.accessEntries access)) of
        (Just (Access.Writer writer), Just (Access.Reader reader)) -> Right (Access.predefinedAccess role writer reader)
        _ -> Left (EndpointRoleMismatch writerName)

predefinedSortId :: Access.ApplicationPredefinedSortRole -> SortId
predefinedSortId =
  Catalogue.predefinedSortId . \case
    Access.SortDefinitionRole -> Catalogue.SortDefinitionRole
    Access.NeutralVertexRole -> Catalogue.NeutralVertexRole
    Access.EdgeRole -> Catalogue.EdgeRole
    Access.NablaRole -> Catalogue.NablaRole
    Access.DeltaRole -> Catalogue.DeltaRole
    Access.ProcessEpochRole -> Catalogue.ProcessEpochRole

-- | Selection contains private names, never proof supplied by the caller.
environmentSelection :: Environment -> Access.PrimordialSelection
environmentSelection (Environment _ environment) = Access.selectEnvironment environment

environmentAccess :: Environment -> Access.EnvironmentAccess
environmentAccess (Environment _ environment) = environment

-- | The environment's checked neutral vertex can be used as an ordinary edge
-- endpoint, sequencing object or label target.
environmentHub :: Environment -> Vertex
environmentHub (Environment herald environment) = Vertex herald (privateObjectUniqueId (Access.environmentAccessHub environment))

-- | Supporting edges in canonical role/direction order. Their ordinary handles
-- support relabelling, deletion and forwarding through this environment.
environmentEdges :: Environment -> [Edge]
environmentEdges environment@(Environment herald access) = fmap (Edge herald writer) (Access.environmentAccessEdges access)
  where
    writer = Access.predefinedWriter (rootPair environment Access.EdgeRole)

newEnvironment :: Raw.Herald -> IO (Either TypedError Environment)
newEnvironment herald = submit (NewEnvironment herald) >>= await

rootPair :: Environment -> Access.ApplicationPredefinedSortRole -> Access.PredefinedAccess
rootPair (Environment _ environment) role =
  case find ((== role) . Access.predefinedAccessRole) (Access.environmentAccessPredefined environment) of
    Just pair -> pair
    Nothing -> error "checked environment has every predefined role"

query :: Delta a -> QueryPredicate a -> Query a
query (Delta herald identifier definition) predicate = Query herald definition (Q.ApplicationQuery (Set.singleton identifier) (lowerQueryPredicate predicate))

-- | A multi-delta query is checked once for connection ownership.
queryMany :: NonEmpty (Delta a) -> QueryPredicate a -> Either TypedError (Query a)
queryMany deltas@(Delta herald _ definition :| _) predicate = do
  mapM_ (\(Delta owner _ _) -> unless (Raw.sameHerald herald owner) (Left DifferentSessions)) deltas
  pure (Query herald definition (Q.ApplicationQuery (Set.fromList (fmap deltaId (NonEmpty.toList deltas))) (lowerQueryPredicate predicate)))

-- | The operation determines both its payload and result. Calls retain the
-- original runtime invocation; decoding never resubmits an operation.
data Operation result where
  Write :: (ValueType a) => Nabla a -> a -> Operation WriteResult
  Read :: (ValueType a) => Query a -> Operation [a]
  ReadObjects :: (ValueType a, SortKindOf a ~ 'D.ControlledSort) => Query a -> Operation [(Object a, a)]
  LocalTake :: (ValueType a) => Query a -> Operation [a]
  Wait :: NonEmpty SomeQuery -> Operation WaitResult
  NewId :: Raw.Herald -> Operation PrivateUniqueId
  NewEnvironment :: Raw.Herald -> Operation Environment
  DeclareSort :: Environment -> Sort a -> Operation ()
  ReserveNabla :: Environment -> Sort a -> Maybe Sequencer -> Operation (Creation (Nabla a))
  ReserveDelta :: Environment -> Sort a -> Operation (Creation (Delta a))
  ReserveVertex :: Environment -> Operation (Creation Vertex)
  ReserveEdgeFor :: Environment -> EdgeStrength -> Vertex -> Vertex -> Operation (Creation Edge)
  PublishCreation :: Creation result -> Operation result
  CancelCreation :: Creation result -> Operation WriteResult
  Reserve :: (SortKindOf a ~ 'D.ControlledSort) => Nabla a -> Operation (Reservation a)
  CancelReservation :: Reservation a -> Operation WriteResult
  PublishReserved :: (ValueType a) => Reservation a -> a -> Operation (Object a)
  Update :: (ValueType a) => Nabla a -> Object a -> a -> Operation WriteResult
  Forward :: Nabla a -> Object a -> Operation ForwardResult
  Label :: Object a -> ApplicationLabel -> ApplicationLabelTarget -> Operation LabelResult

-- Existential raw result is tied to its decoder and exact completion cell.
type role Call nominal
data Call result where
  Call :: Raw.Call raw -> (raw -> Either TypedError result) -> Call result
  FailedCall :: TypedError -> Call result

data CallStatus result = Pending | Complete (Either TypedError result)
  deriving stock (Eq, Show)

submit :: Operation result -> IO (Call result)
submit = \case
  Write (Nabla herald identifier _) value -> attach herald (Raw.Write identifier (PublishValue (encodeValue value))) Right
  Read (Query herald _ syntax) -> attach herald (Raw.Read syntax) decodeValues
  LocalTake (Query herald _ syntax) -> attach herald (Raw.LocalTake syntax) decodeValues
  Wait queries@(SomeQuery (Query herald _ _) :| _) ->
    case traverse (ownedQuery herald) queries of
      Left problem -> pure (FailedCall problem)
      Right syntax -> attach herald (Raw.Wait (NonEmpty.toList syntax)) Right
  NewId herald -> attach herald (Raw.NewId BareNewId) Right
  NewEnvironment herald -> attach herald Raw.NewEnvironment (Right . Environment herald)
  DeclareSort environment@(Environment herald _) definition ->
    attach
      herald
      ( Raw.Write
          (Access.predefinedWriter (rootPair environment Access.SortDefinitionRole))
          (PublishValue (SortDefinitionValue (D.DeclaredSortDefinition (sortDescriptor definition) (Just (sortId definition)))))
      )
      (\case SortDefinitionWritten actual -> verifySort (sortId definition) actual; _ -> Left UnexpectedValue)
  ReserveNabla environment@(Environment herald _) definition sequencing ->
    case traverse (ownedSequencer herald) sequencing of
      Left problem -> pure (FailedCall problem)
      Right identifier ->
        reserveCreation
          environment
          Access.NablaRole
          [("sort_id", BytesValue (sortIdBytes (sortId definition))), ("sequencing_object", OptionalUniqueIdValue identifier)]
          (\_ object -> Nabla herald (asPrivateNablaId object) definition)
  ReserveDelta environment@(Environment herald _) definition ->
    reserveCreation
      environment
      Access.DeltaRole
      [("sort_id", BytesValue (sortIdBytes (sortId definition)))]
      (\_ object -> Delta herald (asPrivateDeltaId object) definition)
  ReserveVertex environment@(Environment herald _) -> reserveCreation environment Access.NeutralVertexRole [] (\_ -> Vertex herald)
  ReserveEdgeFor environment@(Environment herald _) strength (Vertex sourceOwner source) (Vertex destinationOwner destination)
    | Raw.sameHerald herald sourceOwner && Raw.sameHerald herald destinationOwner ->
        reserveCreation
          environment
          Access.EdgeRole
          [("source_vertex", UniqueIdValue source), ("destination_vertex", UniqueIdValue destination), ("strength", EnumValue (case strength of Preserve -> "preserve"; Weaken -> "weaken"))]
          (\writer object -> Edge herald writer (asPrivateObjectId object))
    | otherwise -> pure (FailedCall DifferentSessions)
  PublishCreation (Creation herald writer _ value result) ->
    attach
      herald
      (Raw.Write writer (PublishValue value))
      (\case WriteAccepted -> Right result; _ -> Left UnexpectedValue)
  CancelCreation (Creation herald writer identifier _ _) -> attach herald (Raw.Write writer (DeleteReserved (asPrivateObjectId identifier))) Right
  Reserve writer@(Nabla herald identifier _) -> attach herald (Raw.NewId (ControlledNewId identifier)) (Right . Reservation writer)
  CancelReservation (Reservation (Nabla herald writer _) identifier) -> attach herald (Raw.Write writer (DeleteReserved (asPrivateObjectId identifier))) Right
  PublishReserved (Reservation (Nabla herald writer definition) identifier) value ->
    case controlledIdentity definition (encodeValue value) of
      Right actual
        | actual == identifier ->
            attach
              herald
              (Raw.Write writer (PublishValue (encodeValue value)))
              (\case WriteAccepted -> Right (Object herald (asPrivateObjectId identifier) definition); _ -> Left UnexpectedValue)
      _ -> pure (FailedCall ReservationIdentityMismatch)
  Update (Nabla herald writer _) (Object owner identifier definition) value
    | not (Raw.sameHerald herald owner) -> pure (FailedCall DifferentSessions)
    | otherwise -> case controlledIdentity definition (encodeValue value) of
        Right actual | actual == privateObjectUniqueId identifier -> attach herald (Raw.Write writer (PublishValue (encodeValue value))) Right
        _ -> pure (FailedCall ReservationIdentityMismatch)
  ReadObjects (Query herald definition syntax) -> attach herald (Raw.Read syntax) (traverse (decodeObject herald definition))
  Forward (Nabla herald writer _) (Object owner identifier _)
    | Raw.sameHerald herald owner -> attach herald (Raw.Forward writer identifier) Right
    | otherwise -> pure (FailedCall DifferentSessions)
  Label (Object herald identifier _) expected target -> attach herald (Raw.Label identifier expected target) Right
  where
    ownedSequencer herald (Sequencer owner identifier)
      | Raw.sameHerald herald owner = Right identifier
      | otherwise = Left DifferentSessions
    ownedQuery herald (SomeQuery (Query owner _ syntax))
      | Raw.sameHerald herald owner = Right syntax
      | otherwise = Left DifferentSessions
    decodeValues :: (ValueType a) => [ApplicationValue] -> Either TypedError [a]
    decodeValues = traverse (either (Left . InvalidValue) Right . decodeValue)

attach :: Raw.Herald -> Raw.Operation raw -> (raw -> Either TypedError result) -> IO (Call result)
attach herald operation decode = (\call -> Call call decode) <$> Raw.submit herald operation

await :: Call result -> IO (Either TypedError result)
await (FailedCall problem) = pure (Left problem)
await (Call call decode) = (either (Left . UnderlyingCall) decode) <$> Raw.await call

cancel :: Call result -> IO ()
cancel (FailedCall _) = pure ()
cancel (Call call _) = Raw.cancel call

status :: Call result -> IO (CallStatus result)
status (FailedCall problem) = pure (Complete (Left problem))
status (Call call decode) =
  Raw.status call >>= \case
    Raw.CallPending -> pure Pending
    Raw.CallComplete result -> pure (Complete (either (Left . UnderlyingCall) decode result))

write :: (ValueType a) => Nabla a -> a -> IO (Either TypedError WriteResult)
write writer value = submit (Write writer value) >>= await
read :: (ValueType a) => Query a -> IO (Either TypedError [a])
read value = submit (Read value) >>= await
localTake :: (ValueType a) => Query a -> IO (Either TypedError [a])
localTake value = submit (LocalTake value) >>= await
wait :: NonEmpty SomeQuery -> IO (Either TypedError WaitResult)
wait values = submit (Wait values) >>= await
newid :: Raw.Herald -> IO (Either TypedError PrivateUniqueId)
newid herald = submit (NewId herald) >>= await
forward :: Nabla a -> Object a -> IO (Either TypedError ForwardResult)
forward writer value = submit (Forward writer value) >>= await
label :: Object a -> ApplicationLabel -> ApplicationLabelTarget -> IO (Either TypedError LabelResult)
label value expected target = submit (Label value expected target) >>= await

reserve :: (SortKindOf a ~ 'D.ControlledSort) => Nabla a -> IO (Either TypedError (Reservation a))
reserve writer = submit (Reserve writer) >>= await

cancelReservation :: Reservation a -> IO (Either TypedError WriteResult)
cancelReservation value = submit (CancelReservation value) >>= await

publishReserved :: (ValueType a) => Reservation a -> a -> IO (Either TypedError (Object a))
publishReserved reservation value = submit (PublishReserved reservation value) >>= await

-- | Controlled references from a read retain the read's session and sort.
readObjects :: (ValueType a, SortKindOf a ~ 'D.ControlledSort) => Query a -> IO (Either TypedError [(Object a, a)])
readObjects value = submit (ReadObjects value) >>= await

decodeObject :: (ValueType a) => Raw.Herald -> Sort a -> ApplicationValue -> Either TypedError (Object a, a)
decodeObject herald definition value = do
  decoded <- either (Left . InvalidValue) Right (decodeValue value)
  identifier <- controlledIdentity definition value
  pure (Object herald (asPrivateObjectId identifier) definition, decoded)

controlledIdentity :: Sort a -> ApplicationValue -> Either TypedError PrivateUniqueId
controlledIdentity definition value = case D.keyProjections (sortDescriptor definition) of
  [D.ApplicationProjection (keyName :| [])]
    | RecordValue fields <- value,
      Just (UniqueIdValue identifier) <- Map.lookup keyName fields ->
        Right identifier
  _ -> Left UnexpectedValue

-- | Updating a controlled value cannot silently change its object identity.
update :: (ValueType a) => Nabla a -> Object a -> a -> IO (Either TypedError WriteResult)
update writer object value = submit (Update writer object value) >>= await

rawCall :: Raw.Herald -> Raw.Operation value -> IO (Either TypedError value)
rawCall herald operation = either (Left . UnderlyingCall) Right <$> (Raw.submit herald operation >>= Raw.await)

declareSort :: Environment -> Sort a -> IO (Either TypedError ())
declareSort environment definition = submit (DeclareSort environment definition) >>= await

sortVisible :: Environment -> Sort a -> IO (Either TypedError Bool)
sortVisible environment@(Environment herald _) definition =
  fmap (expected `elem`) <$> rawCall herald (Raw.Read (Q.ApplicationQuery (Set.singleton reader) Q.QueryAlways))
  where
    reader = Access.predefinedReader (rootPair environment Access.SortDefinitionRole)
    expected = SortDefinitionValue (D.DeclaredSortDefinition (sortDescriptor definition) (Just (sortId definition)))

createNabla :: Environment -> Sort a -> Maybe Sequencer -> IO (Either TypedError (Nabla a))
createNabla environment definition sequencing = completeCreation (reserveNabla environment definition sequencing)

createDelta :: Environment -> Sort a -> IO (Either TypedError (Delta a))
createDelta environment definition = completeCreation (reserveDelta environment definition)

createVertex :: Environment -> IO (Either TypedError Vertex)
createVertex environment = completeCreation (reserveVertex environment)

createEdge :: Environment -> EdgeStrength -> Vertex -> Vertex -> IO (Either TypedError Edge)
createEdge environment strength source destination = completeCreation (reserveEdgeFor environment strength source destination)

reserveNabla :: Environment -> Sort a -> Maybe Sequencer -> IO (Either TypedError (Creation (Nabla a)))
reserveNabla environment definition sequencing = submit (ReserveNabla environment definition sequencing) >>= await
reserveDelta :: Environment -> Sort a -> IO (Either TypedError (Creation (Delta a)))
reserveDelta environment definition = submit (ReserveDelta environment definition) >>= await
reserveVertex :: Environment -> IO (Either TypedError (Creation Vertex))
reserveVertex environment = submit (ReserveVertex environment) >>= await
reserveEdgeFor :: Environment -> EdgeStrength -> Vertex -> Vertex -> IO (Either TypedError (Creation Edge))
reserveEdgeFor environment strength source destination = submit (ReserveEdgeFor environment strength source destination) >>= await
publishCreation :: Creation result -> IO (Either TypedError result)
publishCreation creation = submit (PublishCreation creation) >>= await
cancelCreation :: Creation result -> IO (Either TypedError WriteResult)
cancelCreation creation = submit (CancelCreation creation) >>= await

-- These conveniences do not cancel potentially accepted work after an unknown
-- outcome. Use staged creation and retain its Calls when recovery is needed.
completeCreation :: IO (Either TypedError (Creation result)) -> IO (Either TypedError result)
completeCreation reservation = reservation >>= either (pure . Left) publishCreation

forwardEdge :: Edge -> IO (Either TypedError ForwardResult)
forwardEdge (Edge herald writer identifier) = rawCall herald (Raw.Forward writer identifier)

labelVertex :: Vertex -> ApplicationLabel -> ApplicationLabelTarget -> IO (Either TypedError LabelResult)
labelVertex (Vertex herald identifier) expected target = rawCall herald (Raw.Label (asPrivateObjectId identifier) expected target)

labelEdge :: Edge -> ApplicationLabel -> ApplicationLabelTarget -> IO (Either TypedError LabelResult)
labelEdge (Edge herald _ identifier) expected target = rawCall herald (Raw.Label identifier expected target)

-- | Reserve and cancel a structural object without constructing a carrier.
-- Useful when abandoning graph construction before its first publication.
reserveEdge :: Environment -> IO (Either TypedError EdgeReservation)
reserveEdge environment@(Environment herald _) = fmap (EdgeReservation herald writer) <$> rawCall herald (Raw.NewId (ControlledNewId writer))
  where
    writer = Access.predefinedWriter (rootPair environment Access.EdgeRole)

cancelReservedEdge :: EdgeReservation -> IO (Either TypedError WriteResult)
cancelReservedEdge (EdgeReservation herald writer identifier) = rawCall herald (Raw.Write writer (DeleteReserved (asPrivateObjectId identifier)))

carrier :: Raw.Herald -> PrivateUniqueId -> [(Text, ApplicationValue)] -> ApplicationValue
carrier herald identifier members =
  RecordValue
    ( Map.fromList
        ([("object_id", UniqueIdValue identifier), ("label", LabelValue (ProcessLabel (Access.startupAccessProcess (Raw.startup herald)), 0))] <> members)
    )

reserveCreation :: Environment -> Access.ApplicationPredefinedSortRole -> [(Text, ApplicationValue)] -> (PrivateNablaId -> PrivateUniqueId -> result) -> IO (Call (Creation result))
reserveCreation environment@(Environment herald _) role fields result = do
  let writer = Access.predefinedWriter (rootPair environment role)
  attach
    herald
    (Raw.NewId (ControlledNewId writer))
    (\identifier -> Right (Creation herald writer identifier (carrier herald identifier fields) (result writer identifier)))

-- | Binding discovered endpoints is deliberately fused with a library-owned
-- read. No public record or caller-asserted ID pair can mint a typed handle.
discoverDeltas :: Environment -> Sort a -> IO (Either TypedError [Delta a])
discoverDeltas environment@(Environment herald _) definition =
  fmap (map (\identifier -> Delta herald (asPrivateDeltaId identifier) definition)) <$> discoverEndpoints environment Access.DeltaRole definition

discoverNablas :: Environment -> Sort a -> IO (Either TypedError [Nabla a])
discoverNablas environment@(Environment herald _) definition =
  fmap (map (\identifier -> Nabla herald (asPrivateNablaId identifier) definition)) <$> discoverEndpoints environment Access.NablaRole definition

discoverEndpoints :: Environment -> Access.ApplicationPredefinedSortRole -> Sort a -> IO (Either TypedError [PrivateUniqueId])
discoverEndpoints environment@(Environment herald _) role definition = do
  values <- rawCall herald (Raw.Read (Q.ApplicationQuery (Set.singleton reader) predicate))
  pure (values >>= traverse decode)
  where
    reader = Access.predefinedReader (rootPair environment role)
    predicate = Q.QueryCompare (D.ApplicationProjection ("sort_id" :| [])) D.ScalarEqual (Q.QueryBytes (sortIdBytes (sortId definition)))
    decode (RecordValue fields)
      | correctShape fields,
        Just (UniqueIdValue identifier) <- Map.lookup "object_id" fields,
        Just (BytesValue carried) <- Map.lookup "sort_id" fields,
        Just (LabelValue _) <- Map.lookup "label" fields = do
          actual <- either (const (Left UnexpectedValue)) Right (mkSortId carried)
          verifySort (sortId definition) actual
          pure identifier
    decode _ = Left UnexpectedValue
    correctShape fields = case role of
      Access.DeltaRole -> Map.keysSet fields == Set.fromList ["object_id", "label", "sort_id"]
      Access.NablaRole ->
        Map.keysSet fields == Set.fromList ["object_id", "label", "sort_id", "sequencing_object"]
          && case Map.lookup "sequencing_object" fields of Just (OptionalUniqueIdValue _) -> True; _ -> False
      _ -> False
