{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private names selected at startup, and complete newly created environments.
-- Structural admission here grants no authority: Herald checks possession and roles.
module Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (..),
    allApplicationPredefinedSortRoles,
    PredefinedAccess,
    predefinedAccess,
    predefinedAccessRole,
    predefinedWriter,
    predefinedReader,
    StartupAccessError (..),
    EnvironmentAccess,
    environmentAccess,
    environmentAccessPredefined,
    environmentAccessHub,
    environmentAccessEdges,
    EnvironmentEdgeRole (..),
    allEnvironmentEdgeRoles,
    PrimordialEntry (..),
    primordialEntryIdentity,
    PrimordialSelection,
    primordialSelection,
    selectionEntries,
    selectionRequiredEndpoints,
    selectionEnvironmentSources,
    PrimordialAccess,
    primordialAccess,
    primordialAccessWithEndpointSorts,
    primordialAccessWithSorts,
    accessEntries,
    accessEndpointSorts,
    accessObjectSorts,
    accessRequiredEndpoints,
    accessEnvironmentSources,
    selectEnvironment,
    environmentWriterKey,
    environmentReaderKey,
    environmentHubKey,
    environmentEdgeKey,
    primordialAccessFromSelection,
    ApplicationStartupAccess,
    applicationStartupAccess,
    startupAccessProcess,
    startupAccessPrimordial,
  ) where

import Control.Monad (unless)
import Data.Binary (Binary (get, put))
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Types.Identity
import GHC.Generics (Generic)

data ApplicationPredefinedSortRole
  = SortDefinitionRole
  | NeutralVertexRole
  | EdgeRole
  | NablaRole
  | DeltaRole
  | ProcessEpochRole
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Binary)

allApplicationPredefinedSortRoles :: [ApplicationPredefinedSortRole]
allApplicationPredefinedSortRoles = [minBound .. maxBound]

data PredefinedAccess = PredefinedAccess ApplicationPredefinedSortRole PrivateNablaId PrivateDeltaId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

predefinedAccess :: ApplicationPredefinedSortRole -> PrivateNablaId -> PrivateDeltaId -> PredefinedAccess
predefinedAccess = PredefinedAccess
predefinedAccessRole :: PredefinedAccess -> ApplicationPredefinedSortRole
predefinedAccessRole (PredefinedAccess role _ _) = role
predefinedWriter :: PredefinedAccess -> PrivateNablaId
predefinedWriter (PredefinedAccess _ writer _) = writer
predefinedReader :: PredefinedAccess -> PrivateDeltaId
predefinedReader (PredefinedAccess _ _ reader) = reader

data StartupAccessError
  = StartupAccessRolesNotCanonical [ApplicationPredefinedSortRole]
  | EnvironmentAccessRepeatedIdentity PrivateUniqueId
  | EnvironmentAccessEdgeCount Int
  | PrimordialDuplicateKey Text
  | PrimordialInconsistentRoles PrivateUniqueId
  | PrimordialRequiredEndpointNotSelected Text
  | PrimordialEnvironmentSourceNotWriter Text
  | PrimordialEnvironmentSourcesCoincide
  | PrimordialEndpointSortKeysMismatch (Set Text) (Set Text)
  | PrimordialEndpointSortAliasMismatch PrivateUniqueId
  | PrimordialObjectSortKeyNotSelected Text
  | PrimordialObjectSortAliasMismatch PrivateUniqueId
  deriving stock (Eq, Show)

-- | Internal wiring is ordered by predefined role, then by edge role.
data EnvironmentEdgeRole = WriterToHub | HubToReader | ReaderToHub
  deriving stock (Eq, Ord, Show, Enum, Bounded)

allEnvironmentEdgeRoles :: [EnvironmentEdgeRole]
allEnvironmentEdgeRoles = [minBound .. maxBound]

-- | Six endpoint pairs, their neutral hub and eighteen ordinary edge objects.
-- The checked representation cannot omit wiring or alias distinct graph objects.
data EnvironmentAccess = EnvironmentAccess [PredefinedAccess] PrivateObjectId [PrivateObjectId]
  deriving stock (Eq, Show)

environmentAccess :: [PredefinedAccess] -> PrivateObjectId -> [PrivateObjectId] -> Either StartupAccessError EnvironmentAccess
environmentAccess accesses hub edges
  | fmap predefinedAccessRole accesses /= allApplicationPredefinedSortRoles = Left (StartupAccessRolesNotCanonical (fmap predefinedAccessRole accesses))
  | length edges /= length allApplicationPredefinedSortRoles * length allEnvironmentEdgeRoles = Left (EnvironmentAccessEdgeCount (length edges))
  | repeated : _ <- [identity | (identity, count) <- Map.toAscList counts, count > 1] = Left (EnvironmentAccessRepeatedIdentity repeated)
  | otherwise = Right (EnvironmentAccess accesses hub edges)
  where
    identities = [identity | PredefinedAccess _ writer reader <- accesses, identity <- [privateNablaUniqueId writer, privateDeltaUniqueId reader]] <> fmap privateObjectUniqueId (hub : edges)
    counts = Map.fromListWith (+) [(identity, 1 :: Int) | identity <- identities]
environmentAccessPredefined :: EnvironmentAccess -> [PredefinedAccess]
environmentAccessPredefined (EnvironmentAccess accesses _ _) = accesses
environmentAccessHub :: EnvironmentAccess -> PrivateObjectId
environmentAccessHub (EnvironmentAccess _ hub _) = hub
environmentAccessEdges :: EnvironmentAccess -> [PrivateObjectId]
environmentAccessEdges (EnvironmentAccess _ _ edges) = edges

instance Binary EnvironmentAccess where
  put (EnvironmentAccess accesses hub edges) = put accesses >> put hub >> put edges
  get = do
    accesses <- get
    hub <- get
    edges <- get
    either (fail . show) pure (environmentAccess accesses hub edges)

-- | Names are exact text keys; identity-only aliases do not carry possession.
data PrimordialEntry
  = Identity PrivateUniqueId
  | Object PrivateObjectId
  | Writer PrivateNablaId
  | Reader PrivateDeltaId
  | Process PrivateProcessId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

primordialEntryIdentity :: PrimordialEntry -> PrivateUniqueId
primordialEntryIdentity = \case
  Identity value -> value
  Object value -> privateObjectUniqueId value
  Writer value -> privateNablaUniqueId value
  Reader value -> privateDeltaUniqueId value
  Process value -> privateProcessUniqueId value

data Selected = Selected (Map Text PrimordialEntry) (Set Text) (Maybe (Text, Text))
  deriving stock (Eq, Show)
newtype PrimordialSelection = PrimordialSelection Selected deriving stock (Eq, Show)
data PrimordialAccess = PrimordialAccess Selected (Map Text SortId) (Map Text SortId) deriving stock (Eq, Show)

primordialSelection :: [(Text, PrimordialEntry)] -> Set Text -> Maybe (Text, Text) -> Either StartupAccessError PrimordialSelection
primordialSelection entries required sources = PrimordialSelection <$> checkedSelected entries required sources
primordialAccess :: [(Text, PrimordialEntry)] -> Set Text -> Maybe (Text, Text) -> Either StartupAccessError PrimordialAccess
primordialAccess entries required sources = (\selected -> PrimordialAccess selected Map.empty Map.empty) <$> checkedSelected entries required sources

-- | Preserve the carried sorts verified by the owner admitting the startup
-- grant. Every selected endpoint must have evidence, including passive aliases.
-- This constructor only checks shape; the DTO itself grants no authority.
primordialAccessWithEndpointSorts :: [(Text, PrimordialEntry)] -> Set Text -> Maybe (Text, Text) -> Map Text SortId -> Either StartupAccessError PrimordialAccess
primordialAccessWithEndpointSorts entries required sources sorts = primordialAccessWithSorts entries required sources sorts Map.empty

-- | Object sorts are retained admission evidence for selected normal objects.
-- Their presence permits the typed API to bind a hub or edge without trusting
-- its caller-chosen key. Missing object metadata remains valid for untyped use.
primordialAccessWithSorts :: [(Text, PrimordialEntry)] -> Set Text -> Maybe (Text, Text) -> Map Text SortId -> Map Text SortId -> Either StartupAccessError PrimordialAccess
primordialAccessWithSorts entries required sources endpointSorts objectSorts = do
  selected <- checkedSelected entries required sources
  checkedEndpointSorts selected endpointSorts
  checkedObjectSorts selected objectSorts
  pure (PrimordialAccess selected endpointSorts objectSorts)

selectionEntries :: PrimordialSelection -> Map Text PrimordialEntry
selectionEntries (PrimordialSelection (Selected entries _ _)) = entries
selectionRequiredEndpoints :: PrimordialSelection -> Set Text
selectionRequiredEndpoints (PrimordialSelection (Selected _ required _)) = required
selectionEnvironmentSources :: PrimordialSelection -> Maybe (Text, Text)
selectionEnvironmentSources (PrimordialSelection (Selected _ _ sources)) = sources
accessEntries :: PrimordialAccess -> Map Text PrimordialEntry
accessEntries (PrimordialAccess (Selected entries _ _) _ _) = entries
accessEndpointSorts :: PrimordialAccess -> Map Text SortId
accessEndpointSorts (PrimordialAccess _ sorts _) = sorts
accessObjectSorts :: PrimordialAccess -> Map Text SortId
accessObjectSorts (PrimordialAccess _ _ sorts) = sorts
accessRequiredEndpoints :: PrimordialAccess -> Set Text
accessRequiredEndpoints (PrimordialAccess (Selected _ required _) _ _) = required
accessEnvironmentSources :: PrimordialAccess -> Maybe (Text, Text)
accessEnvironmentSources (PrimordialAccess (Selected _ _ sources) _ _) = sources

-- | Structural conversion, used after names have been localized by their owner.
-- Neither this nor any public constructor admits a semantic possession grant.
primordialAccessFromSelection :: PrimordialSelection -> PrimordialAccess
primordialAccessFromSelection (PrimordialSelection selected) = PrimordialAccess selected Map.empty Map.empty

checkedEndpointSorts :: Selected -> Map Text SortId -> Either StartupAccessError ()
checkedEndpointSorts (Selected entries _ _) sorts = do
  unless
    (Map.keysSet endpoints == Map.keysSet sorts)
    (Left (PrimordialEndpointSortKeysMismatch (Map.keysSet endpoints) (Map.keysSet sorts)))
  mapM_ checkAlias (Map.toAscList aliases)
  where
    endpoints = Map.filter isEndpoint entries
    isEndpoint = \case Writer _ -> True; Reader _ -> True; _ -> False
    aliases =
      Map.fromListWith
        Set.union
        [(primordialEntryIdentity entry, Set.singleton sortId) | (key, entry) <- Map.toAscList endpoints, Just sortId <- [Map.lookup key sorts]]
    checkAlias (identity, carriedSorts) = unless (Set.size carriedSorts == 1) (Left (PrimordialEndpointSortAliasMismatch identity))

checkedObjectSorts :: Selected -> Map Text SortId -> Either StartupAccessError ()
checkedObjectSorts (Selected entries _ _) sorts = do
  mapM_ checkKey (Map.keys sorts)
  mapM_ checkAlias (Map.toAscList aliases)
  where
    checkKey key = case Map.lookup key entries of
      Just (Object _) -> pure ()
      _ -> Left (PrimordialObjectSortKeyNotSelected key)
    aliases = Map.fromListWith Set.union [(primordialEntryIdentity entry, Set.singleton sortId) | (key, entry) <- Map.toAscList entries, Just sortId <- [Map.lookup key sorts]]
    checkAlias (identity, objectSorts) = unless (Set.size objectSorts == 1) (Left (PrimordialObjectSortAliasMismatch identity))

checkedSelected :: [(Text, PrimordialEntry)] -> Set Text -> Maybe (Text, Text) -> Either StartupAccessError Selected
checkedSelected entries required sources = do
  mapM_ checkDuplicate (Map.toAscList (Map.fromListWith (+) [(key, 1 :: Int) | (key, _) <- entries]))
  mapM_ checkRoles (Map.toAscList (Map.fromListWith Set.union [(primordialEntryIdentity entry, roleClaim entry) | (_, entry) <- entries]))
  mapM_ checkRequired (Set.toAscList required)
  case sources of
    Nothing -> pure ()
    Just (nabla, delta) -> do
      mapM_ checkSource [nabla, delta]
      unless (nabla /= delta && fmap primordialEntryIdentity (Map.lookup nabla names) /= fmap primordialEntryIdentity (Map.lookup delta names)) (Left PrimordialEnvironmentSourcesCoincide)
  pure (Selected names required sources)
  where
    names = Map.fromList entries
    checkDuplicate (key, count) = unless (count == 1) (Left (PrimordialDuplicateKey key))
    checkRoles (identity, claims) = unless (Set.size claims <= 1) (Left (PrimordialInconsistentRoles identity))
    roleClaim = \case
      Writer _ -> Set.singleton (0 :: Int)
      Reader _ -> Set.singleton 1
      Process _ -> Set.singleton 2
      _ -> Set.empty
    checkRequired key = case Map.lookup key names of
      Just (Writer _) -> pure ()
      Just (Reader _) -> pure ()
      _ -> Left (PrimordialRequiredEndpointNotSelected key)
    checkSource key = case Map.lookup key names of
      Just (Writer _) -> pure ()
      _ -> Left (PrimordialEnvironmentSourceNotWriter key)

instance Binary Selected where
  put (Selected entries required sources) = put (Map.toAscList entries) >> put (Set.toAscList required) >> put sources
  get = do
    entries <- get
    required <- get
    sources <- get
    unless (map fst entries == sort (map fst entries) && required == Set.toAscList (Set.fromList required)) (fail "primordial keys are not canonical")
    either (fail . show) pure (checkedSelected entries (Set.fromList required) sources)
instance Binary PrimordialSelection where
  put (PrimordialSelection value) = put value
  get = PrimordialSelection <$> get
instance Binary PrimordialAccess where
  put (PrimordialAccess value endpointSorts objectSorts) = put value >> put (Map.toAscList endpointSorts) >> put (Map.toAscList objectSorts)
  get = do
    selected <- get
    endpointSorts <- get
    objectSorts <- get
    let canonicalEndpoints = Map.fromList endpointSorts
        canonicalObjects = Map.fromList objectSorts
    unless (endpointSorts == Map.toAscList canonicalEndpoints && objectSorts == Map.toAscList canonicalObjects) (fail "startup sort keys are not canonical")
    -- An empty map is the explicit structural-only DTO. Operational startup
    -- producers always retain the complete verified endpoint map.
    unless (Map.null canonicalEndpoints) (either (fail . show) pure (checkedEndpointSorts selected canonicalEndpoints))
    either (fail . show) pure (checkedObjectSorts selected canonicalObjects)
    pure (PrimordialAccess selected canonicalEndpoints canonicalObjects)

-- | Conventional application keys for the complete environment interface.
environmentWriterKey, environmentReaderKey :: ApplicationPredefinedSortRole -> Text
environmentWriterKey role = roleKey role <> ".writer"
environmentReaderKey role = roleKey role <> ".reader"

environmentHubKey :: Text
environmentHubKey = "environment.hub"
environmentEdgeKey :: ApplicationPredefinedSortRole -> EnvironmentEdgeRole -> Text
environmentEdgeKey role direction =
  "environment." <> roleKey role <> "." <> case direction of
    WriterToHub -> "writer-to-hub"
    HubToReader -> "hub-to-reader"
    ReaderToHub -> "reader-to-hub"

roleKey :: ApplicationPredefinedSortRole -> Text
roleKey = \case
  SortDefinitionRole -> "sort-definition"
  NeutralVertexRole -> "neutral-vertex"
  EdgeRole -> "edge"
  NablaRole -> "nabla"
  DeltaRole -> "delta"
  ProcessEpochRole -> "process-epoch"

selectEnvironment :: EnvironmentAccess -> PrimordialSelection
selectEnvironment (EnvironmentAccess pairs hub edges) = PrimordialSelection (Selected entries (Map.keysSet endpoints) sources)
  where
    endpoints = Map.fromList (concatMap selectedPair pairs)
    edgeKeys = [environmentEdgeKey role direction | role <- allApplicationPredefinedSortRoles, direction <- allEnvironmentEdgeRoles]
    entries = endpoints <> Map.fromList ((environmentHubKey, Object hub) : zip edgeKeys (fmap Object edges))
    selectedPair (PredefinedAccess role writer reader) = [(environmentWriterKey role, Writer writer), (environmentReaderKey role, Reader reader)]
    sources = Just (environmentWriterKey NablaRole, environmentWriterKey DeltaRole)

data ApplicationStartupAccess = ApplicationStartupAccess PrivateProcessId PrimordialAccess
  deriving stock (Eq, Show)
instance Binary ApplicationStartupAccess where
  put (ApplicationStartupAccess process access) = put process >> put access
  get = ApplicationStartupAccess <$> get <*> get
applicationStartupAccess :: PrivateProcessId -> PrimordialAccess -> ApplicationStartupAccess
applicationStartupAccess = ApplicationStartupAccess
startupAccessProcess :: ApplicationStartupAccess -> PrivateProcessId
startupAccessProcess (ApplicationStartupAccess process _) = process
startupAccessPrimordial :: ApplicationStartupAccess -> PrimordialAccess
startupAccessPrimordial (ApplicationStartupAccess _ access) = access
