-- | Checked source admission for selected startup names. Accepted normal grants
-- remain distinct from identity-only aliases throughout localization.
module Eclips.Herald.Application.Primordial
  ( CheckedPrimordialGrant,
    PrimordialGrantError (..),
    checkPrimordialSelection,
    checkTransferredPrimordialShape,
    admitTransferredPrimordialGrant,
    primordialGrantEntries,
    primordialGrantRequiredEndpoints,
    primordialGrantEnvironmentSources,
    primordialGrantPossessions,
    primordialGrantSequencing,
    GlobalPrimordialEntry (..),
    globalPrimordialIdentity,
  ) where

import Control.Monad (unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity qualified as Private
import Eclips.Domain.Identity
import Eclips.Domain.Publication (checkedPublicationId)
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (..))
import Eclips.Domain.Sort.Profile (PredefinedSortRole (..), profileSortFor)
import Eclips.Herald.Application.PrivateIdentity qualified as Identity
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Structural.Debt (sortOccurrenceSortId)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation

data GlobalPrimordialEntry
  = GlobalIdentity GlobalUniqueId
  | GlobalObject GlobalObjectId SortId
  | GlobalWriter NablaId SortId
  | GlobalReader DeltaId SortId
  | GlobalProcess ProcessEpochId
  deriving stock (Eq, Show)

globalPrimordialIdentity :: GlobalPrimordialEntry -> GlobalUniqueId
globalPrimordialIdentity = \case
  GlobalIdentity identity -> identity
  GlobalObject object _ -> globalUniqueIdFromGlobalObjectId object
  GlobalWriter writer _ -> globalUniqueIdFromGlobalObjectId (globalObjectIdFromNablaId writer)
  GlobalReader reader _ -> globalUniqueIdFromGlobalObjectId (globalObjectIdFromDeltaId reader)
  GlobalProcess process -> globalUniqueIdFromGlobalObjectId (globalObjectIdFromProcessEpochId process)

data CheckedPrimordialGrant = CheckedPrimordialGrant (Map Text GlobalPrimordialEntry) (Set Text) (Maybe (Text, Text)) [Controlled.CheckedControlledGrant] (Map NablaId GlobalObjectId)
  deriving stock (Eq, Show)

data PrimordialGrantError
  = PrimordialGrantUnknownAlias Private.PrivateUniqueId
  | PrimordialGrantPrivateNamespace Identity.PrivateIdentityError
  | PrimordialGrantPossession Controlled.ControlledGrantError
  | PrimordialGrantRoleMismatch Private.PrivateUniqueId
  | PrimordialGrantEnvironmentSourceSortMismatch Text
  deriving stock (Eq, Show)

primordialGrantEntries :: CheckedPrimordialGrant -> Map Text GlobalPrimordialEntry
primordialGrantEntries (CheckedPrimordialGrant entries _ _ _ _) = entries
primordialGrantRequiredEndpoints :: CheckedPrimordialGrant -> Set Text
primordialGrantRequiredEndpoints (CheckedPrimordialGrant _ required _ _ _) = required
primordialGrantEnvironmentSources :: CheckedPrimordialGrant -> Maybe (Text, Text)
primordialGrantEnvironmentSources (CheckedPrimordialGrant _ _ sources _ _) = sources
primordialGrantPossessions :: CheckedPrimordialGrant -> [Controlled.CheckedControlledGrant]
primordialGrantPossessions (CheckedPrimordialGrant _ _ _ grants _) = grants

-- | Bootstrap sequencing is immutable object meaning, carried with the grant.
-- Dynamic writer sequencing remains interpreted from current structural records.
primordialGrantSequencing :: CheckedPrimordialGrant -> Map NablaId GlobalObjectId
primordialGrantSequencing (CheckedPrimordialGrant _ _ _ _ sequencing) = sequencing

-- | Roles require current definitions, not label ownership or operation.
checkPrimordialSelection :: ProcessEpochId -> Access.PrimordialSelection -> Identity.PrivateIdentity -> Controlled.State -> Progress.StructuralProgressState -> Either PrimordialGrantError CheckedPrimordialGrant
checkPrimordialSelection parent selection identities controlled progress = do
  admitted <- traverse admit (Access.selectionEntries selection)
  let entries = fmap fst admitted
      grants = [grant | (_, Just grant) <- Map.elems admitted]
  case Access.selectionEnvironmentSources selection of
    Nothing -> pure ()
    Just (nabla, delta) -> do
      checkSource entries nabla (profileSortFor NablaRole)
      checkSource entries delta (profileSortFor DeltaRole)
  let sequencing =
        Map.fromList
          [(writer, target) | possession <- grants, let writer = nablaIdFromGlobalObjectId (Controlled.controlledGrantObject possession), Just root <- [Controlled.controlledWriterFact writer controlled], Just (NablaSequencedBy target) <- [Controlled.rootFactWriterSequencing root]]
  pure (CheckedPrimordialGrant entries (Access.selectionRequiredEndpoints selection) (Access.selectionEnvironmentSources selection) grants sequencing)
  where
    admit entry = do
      let private = Access.primordialEntryIdentity entry
      resolved <- either (Left . PrimordialGrantPrivateNamespace) Right (Identity.resolvePrivateUniqueId parent private identities)
      unique <- maybe (Left (PrimordialGrantUnknownAlias private)) Right resolved
      let object = globalObjectIdFromGlobalUniqueId unique
          mismatch :: Either PrimordialGrantError value
          mismatch = Left (PrimordialGrantRoleMismatch private)
          requireRole role = unless (Controlled.controlledCurrentStructuralRole object controlled == Just role) mismatch
      grant <- case entry of
        Access.Identity _ -> pure Nothing
        _ -> Just <$> either (Left . PrimordialGrantPossession) Right (Controlled.checkControlledGrant parent object controlled)
      selected <- case entry of
        Access.Identity _ -> pure (GlobalIdentity unique)
        Access.Object _ -> GlobalObject object <$> maybe mismatch Right (currentObjectSort object controlled)
        Access.Writer _ -> do
          requireRole NablaCarrier
          let writer = nablaIdFromGlobalObjectId object
          sort <-
            maybe
              mismatch
              Right
              ( case Controlled.controlledWriterFact writer controlled of
                  Just root -> Just (Controlled.rootFactSortId root)
                  Nothing -> currentWriterSort writer object
              )
          pure (GlobalWriter writer sort)
        Access.Reader _ -> do
          requireRole DeltaCarrier
          let reader = deltaIdFromGlobalObjectId object
          sort <-
            maybe
              mismatch
              Right
              ( case Controlled.controlledReaderFact reader controlled of
                  Just root -> Just (Controlled.rootFactSortId root)
                  Nothing -> currentReaderSort reader object
              )
          pure (GlobalReader reader sort)
        Access.Process _ -> do
          requireRole ProcessEpochCarrier
          pure (GlobalProcess (processEpochIdFromGlobalObjectId object))
      pure (selected, grant)
    checkSource entries key sort = case Map.lookup key entries of
      Just (GlobalWriter _ actual) | actual == sort -> pure ()
      _ -> Left (PrimordialGrantEnvironmentSourceSortMismatch key)

    currentWriterSort writer object =
      listToMaybe
        [ sortOccurrenceSortId typedSort
        | projection@(Reconciliation.NablaVertexProjection _ candidate typedSort _ _) <- projections,
          candidate == writer,
          projectionCurrent object projection
        ]
    currentReaderSort reader object =
      listToMaybe
        [ sortOccurrenceSortId typedSort
        | projection@(Reconciliation.DeltaVertexProjection _ candidate typedSort _ _) <- projections,
          candidate == reader,
          projectionCurrent object projection
        ]
    projections = Map.elems (Reconciliation.structuralAppliedVertexProjections (Progress.structuralProgressReconciliation progress))
    projectionCurrent object projection =
      Reconciliation.structuralVertexProjectionPublication projection
        == (checkedPublicationId . Controlled.controlledRecordLatestPublication <$> Controlled.controlledLocalRecord object controlled)

-- | Validate the same selection shape after exact global-name translation.
-- This creates no possession and is safe at the canonical transfer decoder.
checkTransferredPrimordialShape :: Map Text GlobalPrimordialEntry -> Set Text -> Maybe (Text, Text) -> Map NablaId GlobalObjectId -> Either () ()
checkTransferredPrimordialShape entries required sources sequencing = do
  mapM_
    (\claims -> unless (Set.size claims <= 1) (Left ()))
    (Map.elems (Map.fromListWith Set.union [(globalPrimordialIdentity entry, role entry) | entry <- Map.elems entries]))
  mapM_ (\sorts -> unless (Set.size sorts <= 1) (Left ())) (Map.elems (Map.fromListWith Set.union [(object, Set.singleton sort) | GlobalObject object sort <- Map.elems entries]))
  mapM_ (\key -> case Map.lookup key entries of Just (GlobalWriter _ _) -> pure (); Just (GlobalReader _ _) -> pure (); _ -> Left ()) (Set.toAscList required)
  case sources of
    Nothing -> pure ()
    Just (nabla, delta) -> case (Map.lookup nabla entries, Map.lookup delta entries) of
      (Just (GlobalWriter first firstSort), Just (GlobalWriter second secondSort)) ->
        unless (first /= second && firstSort == profileSortFor NablaRole && secondSort == profileSortFor DeltaRole) (Left ())
      _ -> Left ()
  mapM_ (\writer -> unless (any (\entry -> globalPrimordialIdentity entry == globalUniqueIdFromGlobalObjectId (globalObjectIdFromNablaId writer)) (Map.elems entries)) (Left ())) (Map.keys sequencing)
  where
    role = \case
      GlobalWriter _ sort -> Set.singleton (0 :: Int, Just sort)
      GlobalReader _ sort -> Set.singleton (1, Just sort)
      GlobalProcess _ -> Set.singleton (2, Nothing)
      _ -> Set.empty

-- | Source admission is retained by the peer preparation owner. Local semantic
-- roles and currentness must still agree before these exact grants can install.
admitTransferredPrimordialGrant :: Map Text GlobalPrimordialEntry -> Set Text -> Maybe (Text, Text) -> Map NablaId GlobalObjectId -> Controlled.State -> Progress.StructuralProgressState -> Either () CheckedPrimordialGrant
admitTransferredPrimordialGrant entries required sources sequencing controlled progress = do
  checkTransferredPrimordialShape entries required sources sequencing
  grants <- traverse admit (Map.elems entries)
  mapM_ checkSequencing (Map.toAscList sequencing)
  pure (CheckedPrimordialGrant entries required sources [grant | Just grant <- grants] sequencing)
  where
    admit (GlobalIdentity _) = Right Nothing
    admit entry = do
      let object = globalObjectIdFromGlobalUniqueId (globalPrimordialIdentity entry)
          requireRole role = unless (Controlled.controlledCurrentStructuralRole object controlled == Just role) (Left ())
      case entry of
        GlobalObject _ sort -> unless (currentObjectSort object controlled == Just sort) (Left ())
        GlobalWriter writer sort -> do
          requireRole NablaCarrier
          unless (currentWriterSort writer object == Just sort) (Left ())
        GlobalReader reader sort -> do
          requireRole DeltaCarrier
          unless (currentReaderSort reader object == Just sort) (Left ())
        GlobalProcess _ -> requireRole ProcessEpochCarrier
      Just <$> either (const (Left ())) Right (Controlled.checkTransferredControlledGrant object controlled)
    checkSequencing (writer, target) = case Controlled.controlledWriterFact writer controlled of
      Just root -> unless (Controlled.rootFactWriterSequencing root == Just (NablaSequencedBy target)) (Left ())
      Nothing -> Left ()
    currentWriterSort writer object = case Controlled.controlledWriterFact writer controlled of
      Just root -> Just (Controlled.rootFactSortId root)
      Nothing -> listToMaybe [sortOccurrenceSortId sort | projection@(Reconciliation.NablaVertexProjection _ candidate sort _ _) <- projections, candidate == writer, current object projection]
    currentReaderSort reader object = case Controlled.controlledReaderFact reader controlled of
      Just root -> Just (Controlled.rootFactSortId root)
      Nothing -> listToMaybe [sortOccurrenceSortId sort | projection@(Reconciliation.DeltaVertexProjection _ candidate sort _ _) <- projections, candidate == reader, current object projection]
    projections = Map.elems (Reconciliation.structuralAppliedVertexProjections (Progress.structuralProgressReconciliation progress))
    current object projection =
      Reconciliation.structuralVertexProjectionPublication projection
        == (checkedPublicationId . Controlled.controlledRecordLatestPublication <$> Controlled.controlledLocalRecord object controlled)

-- | Record sorts remain authoritative for ordinary objects; the initial process
-- and any bootstrap endpoints can also have retained primitive role facts.
currentObjectSort :: GlobalObjectId -> Controlled.State -> Maybe SortId
currentObjectSort object controlled = case Controlled.controlledLocalRecord object controlled of
  Just record -> Just (Controlled.controlledRecordSortId record)
  Nothing -> fmap (profileSortFor . structuralSort) (Controlled.controlledCurrentStructuralRole object controlled)
  where
    structuralSort = \case
      NeutralVertexCarrier -> NeutralVertexRole
      EdgeCarrier -> EdgeRole
      NablaCarrier -> NablaRole
      DeltaCarrier -> DeltaRole
      ProcessEpochCarrier -> ProcessEpochRole
