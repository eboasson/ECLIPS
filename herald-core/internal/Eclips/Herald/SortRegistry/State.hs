{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Effective primordial sort definitions and immutable regular-retirement
-- history retained by the Herald.
module Eclips.Herald.SortRegistry.State
  ( State,
    View,
    RegistryEntry,
    registryEntryPredefinedRole,
    registryEntrySortId,
    registryEntryDescriptor,
    registryEntryOccurrenceId,
    registryEntryPublicationId,
    initialState,
    SortRegistryBase,
    captureSortRegistryBase,
    SortRegistryBaseCodecError (..),
    encodeSortRegistryBase,
    decodeSortRegistryBase,
    SortRegistryBaseImportError (..),
    PreparedSortRegistryBaseImport,
    prepareSortRegistryBaseImport,
    commitSortRegistryBaseImport,
    takeAlignmentPlanChange,
    clearAlignmentPlanChange,
    lookupEffectiveSort,
    lookupPredefinedRole,
    registryEntries,
    predefinedRegistryEntries,
    RegularSortRetirementView,
    regularSortRetirementEntry,
    regularSortRetirementSortId,
    regularSortRetirementDescriptorDigest,
    regularSortRetirementOccurrenceId,
    regularSortRetirementResolveIndex,
    regularSortRetirementSuccessorOccurrenceId,
    lookupRegularSortRetirement,
    latestRegularSortRetirement,
    regularSortReferenceControlPrerequisite,
    regularSortRetirements,
    RegularSortRetirementDisposition (..),
    RegularSortRetirementSummary,
    regularSortRetirementSummaryEntry,
    regularSortRetirementSummaryResolveIndex,
    regularSortRetirementSummarySuccessorOccurrenceId,
    regularSortRetirementSummaryDisposition,
    RegularSortRetirementError (..),
    PreparedRegularSortRetirement,
    prepareExactRegularSortRetirement,
    prepareUnseenRegularSortRetirement,
    preparedRegularSortRetirementSummary,
    commitRegularSortRetirement,
    SortInductionPlan,
    sortInductionPlanSortId,
    sortInductionPlanOccurrenceId,
    sortInductionPlanControlPrerequisite,
    planSortInduction,
    SortInductionError (..),
    PreparedSortInduction,
    prepareSortInduction,
    commitSortInduction,
    sortView,
    sortViewPredefinedRole,
  )
where

import Control.Monad (foldM, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as Serialize
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance (CanonicalDescriptorDigest, canonicalDescriptorDigestBytes, deriveCanonicalDescriptorDigest, mkCanonicalDescriptorDigest)
import Eclips.Domain.Identity
  ( ControlIndex,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    SystemId,
    authorityEpochCanonicalBytes,
    controlIndex,
    controlIndexWord64,
    decodeAuthorityEpochCanonicalBytes,
    heraldEpochBytes,
    mkHeraldEpoch,
    mkNablaId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    nablaIdBytes,
    nablaSequence,
    nablaSequenceWord64,
    publicationAuthorityEpoch,
    publicationId,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    CanonicalDescriptorError,
    canonicalDescriptorBytes,
    decodeCanonicalDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor (DescriptorAdmission (ApplicationDescriptor, PrimordialDescriptor))
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Domain.Startup (PredefinedSortRole, allPredefinedSortRoles, predefinedSortRoleTag)
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    PrimordialDefinitionReplica,
    checkedPrimordialReplicas,
    checkedSystemId,
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaPublicationId,
    primordialReplicaRole,
    primordialReplicaSortId,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
  )

-- | One effective descriptor occurrence and its ordinary primordial provenance.
data RegistryEntry = RegistryEntry
  { role :: Maybe PredefinedSortRole,
    descriptor :: CanonicalDescriptor,
    occurrenceId :: SortDefinitionOccurrenceId,
    publicationId :: PublicationId
  }
  deriving stock (Eq, Show)

registryEntryPredefinedRole :: RegistryEntry -> Maybe PredefinedSortRole
registryEntryPredefinedRole registry = registry.role

registryEntrySortId :: RegistryEntry -> SortId
registryEntrySortId registry = descriptorSortId registry.descriptor

registryEntryDescriptor :: RegistryEntry -> CanonicalDescriptor
registryEntryDescriptor registry = registry.descriptor

registryEntryOccurrenceId :: RegistryEntry -> SortDefinitionOccurrenceId
registryEntryOccurrenceId registry = registry.occurrenceId

registryEntryPublicationId :: RegistryEntry -> PublicationId
registryEntryPublicationId registry = registry.publicationId

data State = State
  { bySort :: (Map SortId RegistryEntry),
    byRole :: (Map PredefinedSortRole RegistryEntry),
    regularRetirementHistory ::
      Map
        SortId
        (Map SortDefinitionOccurrenceId RegularSortRetirementView),
    alignmentPlanChanged :: !Bool
  }
  deriving stock (Eq)

-- | Immutable descriptor/occurrence projection supplied to coordinators.
newtype View = View (Map PredefinedSortRole RegistryEntry)
  deriving stock (Eq)

initialState ::
  CheckedHeraldGenesis ->
  State
initialState genesis =
  State
    { bySort = Map.fromList [(primordialReplicaSortId replica, entry replica) | replica <- replicas],
      byRole = Map.fromList [(primordialReplicaRole replica, entry replica) | replica <- replicas],
      regularRetirementHistory = Map.empty,
      alignmentPlanChanged = False
    }
  where
    replicas = checkedPrimordialReplicas genesis

-- | Portable current definitions and exact retirement facts. The opaque
-- source owner already admitted each entry; a whole-Herald capture checks
-- their control/carrier context before publishing the paired base.
data SortRegistryBase
  = SortRegistryBase
      !(Map SortId RegistryEntry)
      !(Map PredefinedSortRole RegistryEntry)
      !(Map SortId (Map SortDefinitionOccurrenceId RegularSortRetirementView))
  deriving stock (Eq)

captureSortRegistryBase :: State -> SortRegistryBase
captureSortRegistryBase state =
  SortRegistryBase state.bySort state.byRole state.regularRetirementHistory

-- Explicit nominal facts, not the private owner representation. Effective
-- entries contain the primordial catalogue, so its role index is reconstructed
-- and checked against the receiver's genesis rather than serialized twice.
type RawRegistryPublication = (ByteString, ByteString, ByteString, Word64)
type RawRegistryEntry = (Maybe Word8, ByteString, ByteString, RawRegistryPublication)
type RawRegistryRetirement = (ByteString, ByteString, ByteString, Maybe RawRegistryEntry, Word64, ByteString)

data SortRegistryBaseCodecError
  = SortRegistryBaseMalformed
  | SortRegistryBaseNotCanonical
  | SortRegistryBaseGenesisMismatch
  | SortRegistryBaseDescriptorRejected CanonicalDescriptorError
  | SortRegistryBaseInvalidFacts
  deriving stock (Eq, Show)

-- | Canonical current definitions and immutable retirement facts. Optional
-- historical descriptors remain optional; no publication payload is invented
-- for a definition which the capturing Herald never observed.
encodeSortRegistryBase :: SortRegistryBase -> ByteString
encodeSortRegistryBase (SortRegistryBase effective _ retirements) =
  Serialize.encode
    ( "ECLIPS-SORT-REGISTRY-BASE" :: ByteString,
      map rawRegistryEntry (Map.elems effective),
      map rawRetirement (concatMap Map.elems (Map.elems retirements))
    )
  where
    rawRetirement retirement =
      ( sortIdBytes retirement.regularSortId,
        canonicalDescriptorDigestBytes retirement.regularDescriptorDigest,
        sortDefinitionOccurrenceIdBytes retirement.regularOccurrence,
        rawRegistryEntry <$> retirement.regularRetiredEntry,
        controlIndexWord64 retirement.regularRetirementIndex,
        sortDefinitionOccurrenceIdBytes retirement.regularSuccessorOccurrence
      )

rawRegistryEntry :: RegistryEntry -> RawRegistryEntry
rawRegistryEntry retained =
  ( predefinedSortRoleTag <$> retained.role,
    canonicalDescriptorBytes retained.descriptor,
    sortDefinitionOccurrenceIdBytes retained.occurrenceId,
    ( nablaIdBytes (publicationNabla retained.publicationId),
      authorityEpochCanonicalBytes (publicationAuthorityEpoch retained.publicationId),
      heraldEpochBytes (publicationSourceHeraldEpoch retained.publicationId),
      nablaSequenceWord64 (publicationNablaSequence retained.publicationId)
    )
  )

-- | Re-admit portable facts under the receiver's checked immutable genesis.
-- Descriptor syntax, nominal identities, exact primordial provenance and every
-- retirement-chain link are checked here. The enclosing control/carrier base
-- separately admits their shared control cut and publication context.
decodeSortRegistryBase :: CheckedHeraldGenesis -> ByteString -> Either SortRegistryBaseCodecError SortRegistryBase
decodeSortRegistryBase genesis bytes = do
  (domain, rawEffective, rawRetirements) <- registryBaseShape (Serialize.decode bytes :: Either String (ByteString, [RawRegistryEntry], [RawRegistryRetirement]))
  unless (domain == "ECLIPS-SORT-REGISTRY-BASE") (Left SortRegistryBaseMalformed)
  entries <- traverse decodeRegistryEntry rawEffective
  history <- traverse decodeRetirement rawRetirements
  let effective = Map.fromList [(registryEntrySortId retained, retained) | retained <- entries]
      primordialRows = [(role, retained) | retained <- entries, Just role <- [retained.role]]
      primordial = Map.fromList primordialRows
      expectedPrimordial = Map.fromList [(primordialReplicaRole replica, entry replica) | replica <- checkedPrimordialReplicas genesis]
      retirements = Map.fromListWith Map.union [(retirement.regularSortId, Map.singleton retirement.regularOccurrence retirement) | retirement <- history]
      base = SortRegistryBase effective primordial retirements
  -- A different sort can repeat an existing role without changing the role
  -- map after insertion. Check every row, not only the derived index.
  unless
    (primordial == expectedPrimordial && all (\(role, retained) -> Map.lookup role expectedPrimordial == Just retained) primordialRows)
    (Left SortRegistryBaseGenesisMismatch)
  -- Sorting/deduplication is never an admission policy: the original transcript
  -- must already use the unique ascending-key representation.
  unless (encodeSortRegistryBase base == bytes) (Left SortRegistryBaseNotCanonical)
  mapM_ (validateHistory primordial) (Map.toAscList retirements)
  mapM_ (validateEffective retirements) (Map.elems effective)
  pure base
  where
    system = checkedSystemId genesis
    invalidUnless condition = unless condition (Left SortRegistryBaseInvalidFacts)
    decodeRetirement (sortBytes, digestBytes, occurrenceBytes, retired, index, successorBytes) =
      RegularSortRetirementView
        <$> registryBaseShape (mkSortId (ByteString.copy sortBytes))
        <*> registryBaseShape (mkCanonicalDescriptorDigest (ByteString.copy digestBytes))
        <*> registryBaseShape (mkSortDefinitionOccurrenceId (ByteString.copy occurrenceBytes))
        <*> traverse decodeRegistryEntry retired
        <*> pure (controlIndex index)
        <*> registryBaseShape (mkSortDefinitionOccurrenceId (ByteString.copy successorBytes))
    validateHistory primordial (sortId, history) = do
      invalidUnless (all ((/= sortId) . registryEntrySortId) (Map.elems primordial))
      _ <- foldM (validateRetirement sortId) (controlIndex 0, deriveSortDefinitionOccurrenceId system sortId Genesis) (sortOn regularSortRetirementResolveIndex (Map.elems history))
      pure ()
    validateRetirement sortId (previousIndex, expectedOccurrence) retirement = do
      invalidUnless
        ( retirement.regularSortId == sortId
            && canonicalDescriptorDigestBytes retirement.regularDescriptorDigest == sortIdBytes sortId
            && retirement.regularRetirementIndex > previousIndex
            && retirement.regularOccurrence == expectedOccurrence
        )
      successorBase <- registryBaseShape (resolvedRetirementOccurrenceBase retirement.regularRetirementIndex)
      invalidUnless (retirement.regularSuccessorOccurrence == deriveSortDefinitionOccurrenceId system sortId successorBase)
      case retirement.regularRetiredEntry of
        Nothing -> pure ()
        Just retained ->
          invalidUnless
            ( retained.role == Nothing
                && registryEntrySortId retained == sortId
                && retained.occurrenceId == retirement.regularOccurrence
                && deriveCanonicalDescriptorDigest retained.descriptor == retirement.regularDescriptorDigest
            )
      pure (retirement.regularRetirementIndex, retirement.regularSuccessorOccurrence)
    validateEffective retirements retained = case retained.role of
      Just _ -> pure ()
      Nothing -> do
        let sortId = registryEntrySortId retained
            history = Map.findWithDefault Map.empty sortId retirements
            latest = case sortOn regularSortRetirementResolveIndex (Map.elems history) of
              [] -> Nothing
              chain -> Just (last chain)
            expected = maybe (deriveSortDefinitionOccurrenceId system sortId Genesis) regularSortRetirementSuccessorOccurrenceId latest
        invalidUnless (retained.occurrenceId == expected)
        case latest of
          Nothing -> pure ()
          Just retirement -> invalidUnless (deriveCanonicalDescriptorDigest retained.descriptor == retirement.regularDescriptorDigest)

decodeRegistryEntry :: RawRegistryEntry -> Either SortRegistryBaseCodecError RegistryEntry
decodeRegistryEntry (tag, descriptorBytes, occurrenceBytes, (writerBytes, authorityBytes, sourceBytes, sequenceNumber)) = do
  role <- traverse (\value -> maybe (Left SortRegistryBaseMalformed) Right (find ((== value) . predefinedSortRoleTag) allPredefinedSortRoles)) tag
  -- Cereal fields can be slices of the enclosing transcript. Detach nominal
  -- bytes before admission, including frames whose nested decoders retain their
  -- own slices, so one surviving definition cannot pin the complete base.
  descriptor <- either (Left . SortRegistryBaseDescriptorRejected) Right (decodeCanonicalDescriptor (maybe ApplicationDescriptor (const PrimordialDescriptor) role) (ByteString.copy descriptorBytes))
  occurrence <- registryBaseShape (mkSortDefinitionOccurrenceId (ByteString.copy occurrenceBytes))
  publication <- publicationId <$> registryBaseShape (mkNablaId (ByteString.copy writerBytes)) <*> registryBaseShape (decodeAuthorityEpochCanonicalBytes (ByteString.copy authorityBytes)) <*> registryBaseShape (mkHeraldEpoch (ByteString.copy sourceBytes)) <*> pure (nablaSequence sequenceNumber)
  pure (RegistryEntry role descriptor occurrence publication)

registryBaseShape :: Either problem value -> Either SortRegistryBaseCodecError value
registryBaseShape = either (const (Left SortRegistryBaseMalformed)) Right

data SortRegistryBaseImportError
  = SortRegistryBaseReceiverNotFresh
  | SortRegistryBasePrimordialMismatch
  deriving stock (Eq, Show)

newtype PreparedSortRegistryBaseImport = PreparedSortRegistryBaseImport (Prepared State ())

-- | Adopt a checked registry base into the primordial-only leaf of a fresh
-- joining receiver. The composer owns Joining and control-cut admission.
-- Optional historical descriptors remain historical: neither a retired
-- definition nor an unseen retirement is promoted into the effective map.
prepareSortRegistryBaseImport ::
  SortRegistryBase ->
  State ->
  Either SortRegistryBaseImportError PreparedSortRegistryBaseImport
prepareSortRegistryBaseImport (SortRegistryBase effective primordial retirements) state = do
  if state.bySort == Map.fromList [(registryEntrySortId retained, retained) | retained <- Map.elems state.byRole]
    && Map.null state.regularRetirementHistory
    then pure ()
    else Left SortRegistryBaseReceiverNotFresh
  if primordial == state.byRole
    then pure ()
    else Left SortRegistryBasePrimordialMismatch
  let changed = effective /= state.bySort || retirements /= state.regularRetirementHistory
      successor =
        state
          { bySort = effective,
            regularRetirementHistory = retirements,
            alignmentPlanChanged = state.alignmentPlanChanged || changed
          }
  PreparedSortRegistryBaseImport <$> prepareTransition (\_ -> Right (successor, ())) state

commitSortRegistryBaseImport :: PreparedSortRegistryBaseImport -> State
commitSortRegistryBaseImport (PreparedSortRegistryBaseImport prepared) = fst (commitPrepared prepared)

-- | Coalesced facts read by alignment plan selection. Delivery/report
-- bookkeeping is intentionally absent from this independent consumer journal.
takeAlignmentPlanChange :: State -> (Bool, State)
takeAlignmentPlanChange state =
  let changed = state.alignmentPlanChanged
   in changed `seq` (changed, clearAlignmentPlanChange state)

clearAlignmentPlanChange :: State -> State
clearAlignmentPlanChange state = state {alignmentPlanChanged = False}

lookupEffectiveSort :: SortId -> State -> Maybe RegistryEntry
lookupEffectiveSort sortId state = Map.lookup sortId state.bySort

lookupPredefinedRole :: PredefinedSortRole -> State -> Maybe RegistryEntry
lookupPredefinedRole role state = Map.lookup role state.byRole

registryEntries :: State -> [RegistryEntry]
registryEntries state = Map.elems state.bySort

predefinedRegistryEntries :: State -> [RegistryEntry]
predefinedRegistryEntries state = Map.elems state.byRole

-- | One immutable retirement in the chain for a regular sort occurrence.
--
-- Every participant retains the descriptor digest and exact occurrence. A
-- participant which learned the definition also preserves its descriptor and
-- publication provenance; an unseen definition never acquires invented data.  The resolve index is both the release prerequisite and
-- the base of the successor occurrence.  Its expected derived identity is
-- retained so a coordinator can cross-check the authority projection without
-- granting this leaf owner access to Disappearance state.
data RegularSortRetirementView = RegularSortRetirementView
  { regularSortId :: SortId,
    regularDescriptorDigest :: CanonicalDescriptorDigest,
    regularOccurrence :: SortDefinitionOccurrenceId,
    regularRetiredEntry :: Maybe RegistryEntry,
    regularRetirementIndex :: ControlIndex,
    regularSuccessorOccurrence :: SortDefinitionOccurrenceId
  }
  deriving stock (Eq, Show)

regularSortRetirementEntry :: RegularSortRetirementView -> Maybe RegistryEntry
regularSortRetirementEntry retirement = retirement.regularRetiredEntry

regularSortRetirementSortId :: RegularSortRetirementView -> SortId
regularSortRetirementSortId retirement = retirement.regularSortId

regularSortRetirementDescriptorDigest :: RegularSortRetirementView -> CanonicalDescriptorDigest
regularSortRetirementDescriptorDigest retirement = retirement.regularDescriptorDigest

regularSortRetirementOccurrenceId :: RegularSortRetirementView -> SortDefinitionOccurrenceId
regularSortRetirementOccurrenceId retirement = retirement.regularOccurrence

regularSortRetirementResolveIndex :: RegularSortRetirementView -> ControlIndex
regularSortRetirementResolveIndex retirement = retirement.regularRetirementIndex

regularSortRetirementSuccessorOccurrenceId ::
  RegularSortRetirementView -> SortDefinitionOccurrenceId
regularSortRetirementSuccessorOccurrenceId retirement =
  retirement.regularSuccessorOccurrence

lookupRegularSortRetirement ::
  SortId ->
  SortDefinitionOccurrenceId ->
  State ->
  Maybe RegularSortRetirementView
lookupRegularSortRetirement sortId occurrence state =
  Map.lookup sortId state.regularRetirementHistory
    >>= Map.lookup occurrence

latestRegularSortRetirement ::
  SortId -> State -> Maybe RegularSortRetirementView
latestRegularSortRetirement sortId state =
  Map.lookup sortId state.regularRetirementHistory
    >>= Map.foldl' laterRetirement Nothing
  where
    laterRetirement Nothing candidate = Just candidate
    laterRetirement (Just incumbent) candidate
      | regularSortRetirementResolveIndex candidate
          > regularSortRetirementResolveIndex incumbent =
          Just candidate
      | otherwise = Just incumbent

-- | Minimum Oracle prefix at which a structural carrier may bind this public
-- SortId to the registry's current retirement epoch. A never-retired or
-- predefined sort has the Genesis floor; every retirement advances the floor
-- to its Resolve index even while the successor definition is not effective.
regularSortReferenceControlPrerequisite :: SortId -> State -> ControlIndex
regularSortReferenceControlPrerequisite sortId state =
  maybe
    (controlIndex 0)
    regularSortRetirementResolveIndex
    (latestRegularSortRetirement sortId state)

regularSortRetirements :: State -> [RegularSortRetirementView]
regularSortRetirements state =
  concatMap Map.elems (Map.elems state.regularRetirementHistory)

data RegularSortRetirementDisposition
  = RegularSortRetirementApplied
  | RegularSortRetirementExactReplay
  deriving stock (Eq, Ord, Show)

data RegularSortRetirementSummary = RegularSortRetirementSummary
  { regularSummaryEntry :: Maybe RegistryEntry,
    regularSummaryResolveIndex :: ControlIndex,
    regularSummarySuccessorOccurrence :: SortDefinitionOccurrenceId,
    regularSummaryDisposition :: RegularSortRetirementDisposition
  }
  deriving stock (Eq, Show)

regularSortRetirementSummaryEntry ::
  RegularSortRetirementSummary -> Maybe RegistryEntry
regularSortRetirementSummaryEntry summary = summary.regularSummaryEntry

regularSortRetirementSummaryResolveIndex ::
  RegularSortRetirementSummary -> ControlIndex
regularSortRetirementSummaryResolveIndex summary =
  summary.regularSummaryResolveIndex

regularSortRetirementSummarySuccessorOccurrenceId ::
  RegularSortRetirementSummary -> SortDefinitionOccurrenceId
regularSortRetirementSummarySuccessorOccurrenceId summary =
  summary.regularSummarySuccessorOccurrence

regularSortRetirementSummaryDisposition ::
  RegularSortRetirementSummary -> RegularSortRetirementDisposition
regularSortRetirementSummaryDisposition summary = summary.regularSummaryDisposition

data RegularSortRetirementError
  = RegularSortRetirementPredefinedSort
      SortId
      PredefinedSortRole
  | RegularSortRetirementEffectiveEntryMismatch
      (Maybe RegistryEntry)
      (Maybe RegistryEntry)
  | RegularSortRetirementHistoryMismatch
      SortId
      SortDefinitionOccurrenceId
  | RegularSortRetirementResolveIndexMustBePositive
      SortId
      ControlIndex
  | RegularSortRetirementResolveIndexNotIncreasing
      SortId
      ControlIndex
      ControlIndex
  | RegularSortRetirementPredecessorOccurrenceMismatch
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | RegularSortRetirementSuccessorAlreadyRetired
      SortId
      SortDefinitionOccurrenceId
  | RegularSortRetirementSuccessorEqualsRetired
      SortId
      SortDefinitionOccurrenceId
  | RegularSortRetirementSuccessorOccurrenceMismatch
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  deriving stock (Eq, Show)

newtype PreparedRegularSortRetirement
  = PreparedRegularSortRetirement
      (Prepared State RegularSortRetirementSummary)

-- | Prepare removal of one exact effective regular occurrence.
--
-- The coordinator supplies the checked deployment identity, the authenticated
-- Resolve index, and the successor occurrence carried by its authority.  This
-- leaf rederives that successor before admitting it, checks exact registry
-- ownership, refuses predefined entries, appends rather than overwrites
-- history, and makes an exact replay a no-op even after a later occurrence has
-- become effective.
prepareExactRegularSortRetirement ::
  SystemId ->
  RegistryEntry ->
  ControlIndex ->
  SortDefinitionOccurrenceId ->
  State ->
  Either RegularSortRetirementError PreparedRegularSortRetirement
prepareExactRegularSortRetirement systemId retired =
  prepareRegularSortRetirement
    systemId
    (registryEntrySortId retired)
    (deriveCanonicalDescriptorDigest (registryEntryDescriptor retired))
    (registryEntryOccurrenceId retired)
    (Just retired)

-- | An all-member Resolve also reaches Heralds which never learned the
-- descriptor. Preserve only its authenticated digest and occurrence floor;
-- the first later definition must match them before it becomes effective.
prepareUnseenRegularSortRetirement ::
  SystemId ->
  SortId ->
  CanonicalDescriptorDigest ->
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  SortDefinitionOccurrenceId ->
  State ->
  Either RegularSortRetirementError PreparedRegularSortRetirement
prepareUnseenRegularSortRetirement systemId sortId digest occurrence =
  prepareRegularSortRetirement systemId sortId digest occurrence Nothing

prepareRegularSortRetirement ::
  SystemId ->
  SortId ->
  CanonicalDescriptorDigest ->
  SortDefinitionOccurrenceId ->
  Maybe RegistryEntry ->
  ControlIndex ->
  SortDefinitionOccurrenceId ->
  State ->
  Either RegularSortRetirementError PreparedRegularSortRetirement
prepareRegularSortRetirement systemId sortId digest occurrence retired resolveIndex successorOccurrence state =
  PreparedRegularSortRetirement
    <$> prepareTransition install state
  where
    retirement =
      RegularSortRetirementView
        { regularSortId = sortId,
          regularDescriptorDigest = digest,
          regularOccurrence = occurrence,
          regularRetiredEntry = retired,
          regularRetirementIndex = resolveIndex,
          regularSuccessorOccurrence = successorOccurrence
        }

    install predecessor = do
      case retired >>= registryEntryPredefinedRole of
        Just role -> Left (RegularSortRetirementPredefinedSort sortId role)
        Nothing -> Right ()
      if controlIndexWord64 resolveIndex == 0
        then
          Left
            (RegularSortRetirementResolveIndexMustBePositive sortId resolveIndex)
        else Right ()
      case lookupRegularSortRetirement sortId occurrence predecessor of
        Just retained
          | retained == retirement -> do
              validateSuccessorOccurrence
              Right
                ( predecessor,
                  retirementSummary RegularSortRetirementExactReplay
                )
          | otherwise ->
              Left (RegularSortRetirementHistoryMismatch sortId occurrence)
        Nothing -> do
          case Map.lookup sortId predecessor.bySort of
            actual | actual == retired -> Right ()
            actual ->
              Left
                ( RegularSortRetirementEffectiveEntryMismatch
                    retired
                    actual
                )
          case latestRegularSortRetirement sortId predecessor of
            Nothing ->
              let expected = deriveSortDefinitionOccurrenceId systemId sortId Genesis
               in if occurrence == expected
                    then Right ()
                    else Left (RegularSortRetirementPredecessorOccurrenceMismatch sortId expected occurrence)
            Just latest -> do
              if regularSortRetirementDescriptorDigest latest == digest
                then Right ()
                else Left (RegularSortRetirementHistoryMismatch sortId occurrence)
              let previousIndex = regularSortRetirementResolveIndex latest
                  expectedOccurrence =
                    regularSortRetirementSuccessorOccurrenceId latest
              if resolveIndex > previousIndex
                then Right ()
                else
                  Left
                    ( RegularSortRetirementResolveIndexNotIncreasing
                        sortId
                        previousIndex
                        resolveIndex
                    )
              if occurrence == expectedOccurrence
                then Right ()
                else
                  Left
                    ( RegularSortRetirementPredecessorOccurrenceMismatch
                        sortId
                        expectedOccurrence
                        occurrence
                    )
          if successorOccurrence == occurrence
            then
              Left
                ( RegularSortRetirementSuccessorEqualsRetired
                    sortId
                    occurrence
                )
            else Right ()
          case lookupRegularSortRetirement sortId successorOccurrence predecessor of
            Nothing -> Right ()
            Just _ ->
              Left
                ( RegularSortRetirementSuccessorAlreadyRetired
                    sortId
                    successorOccurrence
                )
          validateSuccessorOccurrence
          let sortHistory =
                Map.findWithDefault Map.empty sortId predecessor.regularRetirementHistory
              successor =
                predecessor
                  { bySort = Map.delete sortId predecessor.bySort,
                    alignmentPlanChanged = predecessor.alignmentPlanChanged || Map.member sortId predecessor.bySort,
                    regularRetirementHistory =
                      Map.insert
                        sortId
                        (Map.insert occurrence retirement sortHistory)
                        predecessor.regularRetirementHistory
                  }
          Right
            ( successor,
              retirementSummary RegularSortRetirementApplied
            )

    validateSuccessorOccurrence = do
      successorBase <-
        case resolvedRetirementOccurrenceBase resolveIndex of
          Left _ ->
            Left
              (RegularSortRetirementResolveIndexMustBePositive sortId resolveIndex)
          Right base -> Right base
      let expectedSuccessor =
            deriveSortDefinitionOccurrenceId systemId sortId successorBase
      if successorOccurrence == expectedSuccessor
        then Right ()
        else
          Left
            ( RegularSortRetirementSuccessorOccurrenceMismatch
                sortId
                expectedSuccessor
                successorOccurrence
            )

    retirementSummary disposition =
      RegularSortRetirementSummary
        { regularSummaryEntry = retired,
          regularSummaryResolveIndex = resolveIndex,
          regularSummarySuccessorOccurrence = successorOccurrence,
          regularSummaryDisposition = disposition
        }

preparedRegularSortRetirementSummary ::
  PreparedRegularSortRetirement -> RegularSortRetirementSummary
preparedRegularSortRetirementSummary
  (PreparedRegularSortRetirement prepared) = snd (commitPrepared prepared)

commitRegularSortRetirement :: PreparedRegularSortRetirement -> State
commitRegularSortRetirement (PreparedRegularSortRetirement prepared) =
  fst (commitPrepared prepared)

-- | The occurrence coordinate and Oracle prefix required for a write that
-- defines this public sort in the registry's current retirement epoch.
data SortInductionPlan = SortInductionPlan
  { plannedSortId :: SortId,
    plannedOccurrenceId :: SortDefinitionOccurrenceId,
    plannedControlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

sortInductionPlanSortId :: SortInductionPlan -> SortId
sortInductionPlanSortId plan = plan.plannedSortId

sortInductionPlanOccurrenceId ::
  SortInductionPlan -> SortDefinitionOccurrenceId
sortInductionPlanOccurrenceId plan = plan.plannedOccurrenceId

sortInductionPlanControlPrerequisite :: SortInductionPlan -> ControlIndex
sortInductionPlanControlPrerequisite plan = plan.plannedControlPrerequisite

-- | Derive the only occurrence coordinate currently admitted for a descriptor.
-- Genesis has prerequisite zero.  Once an occurrence has been retired, the
-- latest resolve index supplies both the successor occurrence base and the
-- minimum control prefix for any equal redefinition.
planSortInduction ::
  SystemId ->
  CanonicalDescriptor ->
  State ->
  Either SortInductionError SortInductionPlan
planSortInduction systemId descriptor state = do
  let sortId = descriptorSortId descriptor
      latest = latestRegularSortRetirement sortId state
      (base, prerequisite, retainedExpected) =
        case latest of
          Nothing -> (Genesis, controlIndex 0, Nothing)
          Just retirement ->
            let retirementIndex = regularSortRetirementResolveIndex retirement
             in ( retirementBase retirementIndex,
                  retirementIndex,
                  Just (regularSortRetirementSuccessorOccurrenceId retirement)
                )
      occurrence = deriveSortDefinitionOccurrenceId systemId sortId base
  case latest of
    Just retirement
      | regularSortRetirementDescriptorDigest retirement /= deriveCanonicalDescriptorDigest descriptor
          || maybe False ((/= descriptor) . registryEntryDescriptor) (regularSortRetirementEntry retirement) ->
          Left (SortInductionIdentityContradiction sortId)
    _ -> Right ()
  case retainedExpected of
    Just expected
      | expected /= occurrence ->
          Left
            ( SortInductionRetirementSuccessorMismatch
                sortId
                expected
                occurrence
            )
    _ -> Right ()
  case Map.lookup sortId state.bySort of
    Just effective
      | registryEntryDescriptor effective /= descriptor ->
          Left (SortInductionIdentityContradiction sortId)
      | registryEntryOccurrenceId effective /= occurrence ->
          Left
            ( SortInductionOccurrenceMismatch
                sortId
                (registryEntryOccurrenceId effective)
                occurrence
            )
    _ -> Right ()
  Right
    SortInductionPlan
      { plannedSortId = sortId,
        plannedOccurrenceId = occurrence,
        plannedControlPrerequisite = prerequisite
      }
  where
    retirementBase retirementIndex =
      either
        (error . ("invalid retained regular sort retirement: " <>) . show)
        id
        (resolvedRetirementOccurrenceBase retirementIndex)

sortView :: State -> View
sortView state = View state.byRole

sortViewPredefinedRole :: PredefinedSortRole -> View -> Maybe RegistryEntry
sortViewPredefinedRole role (View entries) = Map.lookup role entries

entry :: PrimordialDefinitionReplica -> RegistryEntry
entry replica =
  RegistryEntry
    { role = Just (primordialReplicaRole replica),
      descriptor = primordialReplicaDescriptor replica,
      occurrenceId = primordialReplicaOccurrenceId replica,
      publicationId = primordialReplicaPublicationId replica
    }

data SortInductionError
  = SortInductionIdentityContradiction SortId
  | SortInductionOccurrenceMismatch
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | SortInductionRetiredOccurrence
      SortId
      SortDefinitionOccurrenceId
  | SortInductionRetirementSuccessorMismatch
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  deriving stock (Eq, Show)

newtype PreparedSortInduction
  = PreparedSortInduction (Prepared State ())

-- | Make one checked ordinary sort definition effective. Re-observing the
-- exact descriptor /and occurrence/ is idempotent; a different occurrence is
-- never silently folded into the incumbent.  After retirement, only the latest
-- recorded successor occurrence can re-establish the equal descriptor, and a
-- retired occurrence remains suppressed permanently.
prepareSortInduction ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  State ->
  Either SortInductionError PreparedSortInduction
prepareSortInduction descriptor occurrence publication state =
  PreparedSortInduction
    <$> prepareTransition install state
  where
    sortId = descriptorSortId descriptor
    install predecessor = case Map.lookup sortId predecessor.bySort of
      Nothing -> do
        case lookupRegularSortRetirement sortId occurrence predecessor of
          Just _ -> Left (SortInductionRetiredOccurrence sortId occurrence)
          Nothing -> Right ()
        case latestRegularSortRetirement sortId predecessor of
          Nothing -> Right ()
          Just latest -> do
            let expectedOccurrence =
                  regularSortRetirementSuccessorOccurrenceId latest
            if deriveCanonicalDescriptorDigest descriptor == regularSortRetirementDescriptorDigest latest
              && maybe True ((== descriptor) . registryEntryDescriptor) (regularSortRetirementEntry latest)
              then Right ()
              else Left (SortInductionIdentityContradiction sortId)
            if occurrence == expectedOccurrence
              then Right ()
              else
                Left
                  ( SortInductionOccurrenceMismatch
                      sortId
                      expectedOccurrence
                      occurrence
                  )
        Right
          ( predecessor
              { bySort =
                  Map.insert
                    sortId
                    RegistryEntry
                      { role = Nothing,
                        descriptor = descriptor,
                        occurrenceId = occurrence,
                        publicationId = checkedPublicationId publication
                      }
                    predecessor.bySort,
                alignmentPlanChanged = True
              },
            ()
          )
      Just incumbent
        | registryEntryDescriptor incumbent /= descriptor ->
            Left (SortInductionIdentityContradiction sortId)
        | registryEntryOccurrenceId incumbent == occurrence ->
            Right (predecessor, ())
        | otherwise ->
            case lookupRegularSortRetirement sortId occurrence predecessor of
              Just _ -> Left (SortInductionRetiredOccurrence sortId occurrence)
              Nothing ->
                Left
                  ( SortInductionOccurrenceMismatch
                      sortId
                      (registryEntryOccurrenceId incumbent)
                      occurrence
                  )

commitSortInduction :: PreparedSortInduction -> State
commitSortInduction (PreparedSortInduction prepared) =
  fst (commitPrepared prepared)
