{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Package-private composition of the Step-15 membership contraction and the
-- terminal structural-source algebra.
--
-- The live peer/TCP path reaches this coordinator only through checked EPRP
-- controls. It freezes the local inventory at the already-authoritative
-- membership advance, retains late survivor inventories and repaired payloads,
-- and exposes a successor-base witness only after the exact
-- membership-successor cut is reported installed. None of these decisions move
-- into the runtime.
module Eclips.Herald.UseCase.Step15StructuralBase
  ( MembershipBaseClosure,
    StructuralBaseProblem (..),
    TopologyDigestReconstructor,
    beginStructuralBaseAfterAppliedMembershipAdvance,
    beginMembershipBaseClosure,
    structuralBasePredecessorMembership,
    structuralBaseSuccessorMembership,
    structuralBaseRetiredSources,
    structuralBaseLineage,
    structuralBaseAnchorCutId,
    retainStructuralBaseInventoryEvidence,
    structuralBasePayloadRequestsForInventory,
    prepareStructuralBaseEvidencePayloadRelay,
    receiveStructuralBaseEvidencePayloadRelay,
    structuralBaseEstablishedChain,
    rebaseStructuralBaseAnchor,
    structuralBaseLocalHerald,
    structuralBaseLocalInventories,
    structuralBaseInventoryEntries,
    structuralBaseRetainedInventoryEntries,
    structuralBasePayloadEntries,
    structuralBasePayloadRequests,
    receiveStructuralBaseInventory,
    prepareStructuralBasePayloadRelay,
    receiveStructuralBasePayloadRelay,
    prepareStructuralBaseUnionAnnouncement,
    acceptStructuralBaseUnionAnnouncement,
    receiveStructuralBaseUnionAcceptance,
    establishStructuralBaseUnion,
    receiveStructuralBaseUnionEstablished,
    structuralBaseAgreedUnion,
    structuralBaseAcceptanceEntries,
    structuralBaseEstablishedUnion,
    expectedSuccessorStructuralCut,
    installSuccessorStructuralBase,
    recordInstalledSuccessorStructuralCut,
    structuralBaseEvidence,
    structuralBaseReleases,
  )
where

import Control.Monad (unless)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    StructuralOccurrenceId,
    TopologyCutId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipLineage,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageRetiredHeraldEpochs,
    heraldMembershipLineageTarget,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
  )
import Eclips.Domain.Topology
  ( TerminalSourceUnionDigest,
    TopologyCut,
    TopologyOccurrenceDigest,
    TopologyShapeProblem,
    deriveTopologyCutId,
    topologyCut,
    topologyFrontier,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol (TopologyCutEstablished, topologyCutEstablishedId)
import Eclips.Herald.Graph.TerminalSource
  ( MembershipBaseClosure (..),
    SuccessorStructuralBase,
    TerminalSourceInventory,
    TerminalSourceInventoryDigest,
    TerminalSourcePayloadRelay,
    TerminalSourcePayloadRequest,
    TerminalSourcePredecessorBase,
    TerminalSourceProblem,
    TerminalSourceUnion,
    TerminalSourceUnionAcceptance,
    TerminalSourceUnionAnnounce,
    TerminalSourceUnionEstablished,
    TerminalStructuralOccurrence,
    acceptTerminalSourceEvidencePayloadRelay,
    acceptTerminalSourcePayloadRelay,
    beginTerminalStructuralCoordinator,
    compareTerminalSourceUnions,
    deriveTerminalSourceEvidencePayloadRequests,
    deriveTerminalSourcePayloadRequests,
    deriveTerminalSourceUnionChecked,
    establishedSuccessorStructuralBase,
    lookupTerminalSourcePayload,
    retainTerminalSourceInventoryEvidence,
    successorStructuralBaseReleases,
    terminalSourceEvidencePayloadRelay,
    terminalSourceInventoryDigest,
    terminalSourceInventoryEstablishedChain,
    terminalSourceInventoryReporter,
    terminalSourceInventoryRetiredSource,
    terminalSourcePayloadEntries,
    terminalSourcePayloadRelay,
    terminalSourcePayloadRequestOccurrence,
    terminalSourcePayloadRequestSupportingInventory,
    terminalSourcePredecessorBaseCutId,
    terminalSourceUnionAcceptance,
    terminalSourceUnionAcceptanceReporter,
    terminalSourceUnionAnnounce,
    terminalSourceUnionAnnounceAnnouncer,
    terminalSourceUnionAnnounceUnion,
    terminalSourceUnionDigest,
    terminalSourceUnionEstablished,
    terminalSourceUnionEstablishedAcceptances,
    terminalSourceUnionEstablishedUnion,
    terminalSourceUnionReady,
    terminalSourceUnionSuccessorInitialVector,
    terminalSourceUnionTerminalControlPrefix,
    terminalSourceUnionTerminalTopologyOccurrenceDigest,
    terminalSourceUnionTopologyPredecessor,
    validateTerminalSourceUnionAcceptance,
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
  )
import Eclips.Herald.UseCase.TerminalStructuralStart qualified as TerminalStart

-- | Graph supplies the canonical topology projection at the exact vector and
-- control coordinate chosen by the terminal-source algebra.
type TopologyDigestReconstructor =
  StructuralVersionVector ->
  ControlIndex ->
  Either TerminalSourceProblem TopologyOccurrenceDigest

data StructuralBaseProblem
  = StructuralBaseStartProblem TerminalStart.TerminalStructuralStartProblem
  | StructuralBaseEstablishedLineageConflict
  | StructuralBaseAnchorNotInstalled TopologyCutId
  | StructuralBaseMembershipAdvanceRetiredLocal HeraldEpoch
  | StructuralBaseAppliedSuccessorPredecessorMismatch
      HeraldMembershipGenerationId
      (Maybe HeraldMembershipGenerationId)
  | StructuralBaseSuccessorHasNoRetiredSource
  | StructuralBaseRetiredSourceMismatch HeraldEpoch HeraldEpoch
  | StructuralBasePredecessorCutMissing TopologyCutId
  | StructuralBasePredecessorVectorGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | StructuralBasePredecessorVectorMemberSetMismatch
  | StructuralBaseTerminalSourceProblem TerminalSourceProblem
  | StructuralBaseInventoryReporterConflict HeraldEpoch
  | StructuralBaseInventorySetAlreadySealed HeraldEpoch
  | StructuralBaseSupportingInventoryMissing TerminalSourceInventoryDigest
  | StructuralBasePayloadUnavailable StructuralOccurrenceId
  | StructuralBaseUnionUnavailable
  | StructuralBaseUnionMismatch
      TerminalSourceUnionDigest
      TerminalSourceUnionDigest
  | StructuralBaseUnionAnnouncementMismatch
  | StructuralBaseAcceptanceMismatch HeraldEpoch
  | StructuralBaseAcceptanceReporterConflict HeraldEpoch
  | StructuralBaseEstablishedMismatch
  | StructuralBaseTopologyShapeProblem TopologyShapeProblem
  | StructuralBaseGraphProgressProblem GraphProgress.StructuralProgressProblem
  | StructuralBaseSuccessorCutMismatch TopologyCutId TopologyCutId
  deriving stock (Eq, Show)

-- | Begin from the production Oracle-watch cut, where the canonical Oracle
-- entry has already advanced the projection before the ordinary membership
-- owners are composed.  The predecessor and successor states are both owner
-- evidence: callers cannot supply either membership generation separately.
beginStructuralBaseAfterAppliedMembershipAdvance ::
  [TerminalStructuralOccurrence] ->
  HeraldState ->
  HeraldState ->
  Either StructuralBaseProblem MembershipBaseClosure
beginStructuralBaseAfterAppliedMembershipAdvance =
  beginStructuralBaseFromAppliedStates

beginStructuralBaseFromAppliedStates :: [TerminalStructuralOccurrence] -> HeraldState -> HeraldState -> Either StructuralBaseProblem MembershipBaseClosure
beginStructuralBaseFromAppliedStates retained predecessor successor =
  either (Left . StructuralBaseStartProblem) Right (TerminalStart.beginAfterAppliedMembershipAdvance retained predecessor successor)

beginMembershipBaseClosure ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  HeraldEpoch ->
  TerminalSourcePredecessorBase ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  [TerminalStructuralOccurrence] ->
  Either StructuralBaseProblem MembershipBaseClosure
beginMembershipBaseClosure historyLineage lineage local base chain bases retained = mapTerminalProblem (beginTerminalStructuralCoordinator historyLineage lineage local base chain bases retained)

rebaseStructuralBaseAnchor ::
  HeraldMembershipLineage ->
  HeraldMembershipLineage ->
  TerminalSourcePredecessorBase ->
  [TopologyCutEstablished] ->
  [TerminalSourceUnionEstablished] ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem MembershipBaseClosure
rebaseStructuralBaseAnchor historyLineage lineage base chain bases incumbent = do
  replacement <- beginMembershipBaseClosure historyLineage lineage incumbent.localHerald base chain bases (fmap snd (terminalSourcePayloadEntries incumbent.payloads))
  localInventories <- mapTerminalProblem (traverse (retainTerminalSourceInventoryEvidence (Map.elems incumbent.retainedInventories) (fmap snd (terminalSourcePayloadEntries incumbent.payloads))) replacement.localInventories)
  let inventories = Map.fromList [((origin, replacement.localHerald), inventory) | (origin, inventory) <- Map.toAscList localInventories]
  pure replacement {localInventories, inventories, retainedInventories = Map.union incumbent.retainedInventories (Map.fromList [(terminalSourceInventoryDigest inventory, inventory) | inventory <- Map.elems inventories])}

structuralBaseAnchorCutId :: MembershipBaseClosure -> TopologyCutId
structuralBaseAnchorCutId state = terminalSourcePredecessorBaseCutId state.predecessorBase

structuralBaseLineage :: MembershipBaseClosure -> HeraldMembershipLineage
structuralBaseLineage state = state.lineage

-- | Compare proved chains by exact predecessor order, never by vector maximum.
-- A longer chain can be repaired into the Graph owner before inventories are
-- sealed at its anchor. Unestablished announcements never enter this carrier.
structuralBaseEstablishedChain :: MembershipBaseClosure -> Either StructuralBaseProblem [TopologyCutEstablished]
structuralBaseEstablishedChain state = foldl choose (Right state.establishedChain) (fmap terminalSourceInventoryEstablishedChain (Map.elems state.inventories))
  where
    choose retained candidate = do
      incumbent <- retained
      let common = min (length incumbent) (length candidate)
      unless (take common incumbent == take common candidate) (Left StructuralBaseEstablishedLineageConflict)
      pure (if length candidate > length incumbent then candidate else incumbent)

structuralBasePredecessorMembership ::
  MembershipBaseClosure -> HeraldMembershipGeneration
structuralBasePredecessorMembership state = (heraldMembershipLineageOrigin state.lineage)

structuralBaseSuccessorMembership ::
  MembershipBaseClosure -> HeraldMembershipGeneration
structuralBaseSuccessorMembership state = (heraldMembershipLineageTarget state.lineage)

structuralBaseRetiredSources :: MembershipBaseClosure -> Set HeraldEpoch
structuralBaseRetiredSources state = Set.fromList (heraldMembershipLineageRetiredHeraldEpochs state.lineage)

structuralBaseLocalHerald :: MembershipBaseClosure -> HeraldEpoch
structuralBaseLocalHerald state = state.localHerald

structuralBaseLocalInventories ::
  MembershipBaseClosure -> [TerminalSourceInventory]
structuralBaseLocalInventories state = Map.elems state.localInventories

structuralBaseInventoryEntries ::
  MembershipBaseClosure -> [((HeraldEpoch, HeraldEpoch), TerminalSourceInventory)]
structuralBaseInventoryEntries state = Map.toAscList state.inventories

structuralBaseRetainedInventoryEntries :: MembershipBaseClosure -> [(TerminalSourceInventoryDigest, TerminalSourceInventory)]
structuralBaseRetainedInventoryEntries state = Map.toAscList state.retainedInventories

structuralBasePayloadEntries ::
  MembershipBaseClosure ->
  [(StructuralOccurrenceId, TerminalStructuralOccurrence)]
structuralBasePayloadEntries state = terminalSourcePayloadEntries state.payloads

structuralBasePayloadRequests ::
  MembershipBaseClosure ->
  Either StructuralBaseProblem [TerminalSourcePayloadRequest]
structuralBasePayloadRequests state = concat <$> traverse forOrigin (Set.toAscList (structuralBaseRetiredSources state))
  where
    forOrigin origin = mapTerminalProblem (deriveTerminalSourcePayloadRequests state.historyLineage state.lineage origin (filter ((== origin) . terminalSourceInventoryRetiredSource) (Map.elems state.inventories)) state.payloads)

receiveStructuralBaseInventory :: TerminalSourceInventory -> MembershipBaseClosure -> Either StructuralBaseProblem MembershipBaseClosure
receiveStructuralBaseInventory inventory state = do
  _ <- mapTerminalProblem (deriveTerminalSourcePayloadRequests state.historyLineage state.lineage (terminalSourceInventoryRetiredSource inventory) [inventory] state.payloads)
  case Map.lookup key state.inventories of
    Just incumbent
      | incumbent == inventory -> Right state
      | otherwise -> Left (StructuralBaseInventoryReporterConflict reporter)
    Nothing -> do
      unless (state.agreedUnion == Nothing) (Left (StructuralBaseInventorySetAlreadySealed reporter))
      let replacement =
            state
              { inventories = Map.insert key inventory state.inventories,
                retainedInventories = Map.insert (terminalSourceInventoryDigest inventory) inventory state.retainedInventories
              }
      _ <- structuralBasePayloadRequests replacement
      _ <- structuralBaseEstablishedChain replacement
      pure replacement
  where
    reporter = terminalSourceInventoryReporter inventory
    key = (terminalSourceInventoryRetiredSource inventory, reporter)

structuralBasePayloadRequestsForInventory :: HeraldMembershipLineage -> TerminalSourceInventory -> MembershipBaseClosure -> Either StructuralBaseProblem [TerminalSourcePayloadRequest]
structuralBasePayloadRequestsForInventory history inventory state = mapTerminalProblem (deriveTerminalSourceEvidencePayloadRequests history inventory state.payloads)

-- | Retain a checked supporter before starting repair. Its lifetime is separate
-- from a pending inventory hold or the current anchor's agreement matrix.
retainStructuralBaseInventoryEvidence :: HeraldMembershipLineage -> TerminalSourceInventory -> MembershipBaseClosure -> Either StructuralBaseProblem MembershipBaseClosure
retainStructuralBaseInventoryEvidence history inventory state = do
  _ <- structuralBasePayloadRequestsForInventory history inventory state
  pure state {retainedInventories = Map.insert (terminalSourceInventoryDigest inventory) inventory state.retainedInventories}

prepareStructuralBaseEvidencePayloadRelay :: HeraldMembershipLineage -> TerminalSourceInventory -> TerminalSourcePayloadRequest -> MembershipBaseClosure -> Either StructuralBaseProblem TerminalSourcePayloadRelay
prepareStructuralBaseEvidencePayloadRelay history inventory request state = do
  occurrence <- maybe (Left (StructuralBasePayloadUnavailable (terminalSourcePayloadRequestOccurrence request))) Right (lookupTerminalSourcePayload (terminalSourcePayloadRequestOccurrence request) state.payloads)
  mapTerminalProblem (terminalSourceEvidencePayloadRelay history state.localHerald request inventory occurrence)

receiveStructuralBaseEvidencePayloadRelay :: HeraldMembershipLineage -> TerminalSourceInventory -> TerminalSourcePayloadRequest -> TerminalSourcePayloadRelay -> MembershipBaseClosure -> Either StructuralBaseProblem MembershipBaseClosure
receiveStructuralBaseEvidencePayloadRelay history inventory request relay state = do
  payloads <- mapTerminalProblem (acceptTerminalSourceEvidencePayloadRelay history request inventory relay state.payloads)
  pure state {payloads, retainedInventories = Map.insert (terminalSourceInventoryDigest inventory) inventory state.retainedInventories}

prepareStructuralBasePayloadRelay ::
  TerminalSourcePayloadRequest ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem TerminalSourcePayloadRelay
prepareStructuralBasePayloadRelay request state = do
  inventory <- supportingInventory request state
  occurrence <-
    maybe
      (Left (StructuralBasePayloadUnavailable occurrenceId))
      Right
      (lookupTerminalSourcePayload occurrenceId state.payloads)
  mapTerminalProblem
    ( terminalSourcePayloadRelay
        state.historyLineage
        state.lineage
        state.localHerald
        request
        inventory
        occurrence
    )
  where
    occurrenceId = terminalSourcePayloadRequestOccurrence request

receiveStructuralBasePayloadRelay ::
  TerminalSourcePayloadRequest ->
  TerminalSourcePayloadRelay ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem MembershipBaseClosure
receiveStructuralBasePayloadRelay request relay state = do
  inventory <- supportingInventory request state
  payloads <-
    mapTerminalProblem
      ( acceptTerminalSourcePayloadRelay
          state.historyLineage
          state.lineage
          request
          inventory
          relay
          state.payloads
      )
  pure state {payloads}

prepareStructuralBaseUnionAnnouncement ::
  TopologyDigestReconstructor ->
  MembershipBaseClosure ->
  Either
    StructuralBaseProblem
    (MembershipBaseClosure, TerminalSourceUnionAnnounce)
prepareStructuralBaseUnionAnnouncement reconstruct state = do
  union <- deriveCoordinatorUnion reconstruct state
  announce <-
    mapTerminalProblem
      ( terminalSourceUnionAnnounce
          (heraldMembershipLineageTarget state.lineage)
          state.localHerald
          union
      )
  next <- retainAgreedUnion union state
  pure (next, announce)

acceptStructuralBaseUnionAnnouncement ::
  TopologyDigestReconstructor ->
  StructuralVersionVector ->
  ControlIndex ->
  TerminalSourceUnionAnnounce ->
  MembershipBaseClosure ->
  Either
    StructuralBaseProblem
    (MembershipBaseClosure, TerminalSourceUnionAcceptance)
acceptStructuralBaseUnionAnnouncement
  reconstruct
  appliedVector
  appliedControl
  announce
  state = do
    let suppliedUnion = terminalSourceUnionAnnounceUnion announce
    expectedUnion <- deriveCoordinatorUnion reconstruct state
    requireEqualUnion expectedUnion suppliedUnion
    expectedAnnounce <-
      mapTerminalProblem
        ( terminalSourceUnionAnnounce
            (heraldMembershipLineageTarget state.lineage)
            (terminalSourceUnionAnnounceAnnouncer announce)
            suppliedUnion
        )
    unless
      (expectedAnnounce == announce)
      (Left StructuralBaseUnionAnnouncementMismatch)
    ready <-
      mapTerminalProblem
        (terminalSourceUnionReady appliedVector appliedControl suppliedUnion)
    acceptance <-
      mapTerminalProblem
        ( terminalSourceUnionAcceptance
            (heraldMembershipLineageTarget state.lineage)
            state.localHerald
            ready
        )
    withUnion <- retainAgreedUnion suppliedUnion state
    withAcceptance <- retainAcceptance acceptance withUnion
    pure (withAcceptance, acceptance)

receiveStructuralBaseUnionAcceptance ::
  TerminalSourceUnionAcceptance ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem MembershipBaseClosure
receiveStructuralBaseUnionAcceptance acceptance state = do
  union <- maybe (Left StructuralBaseUnionUnavailable) Right state.agreedUnion
  case validateTerminalSourceUnionAcceptance
    (heraldMembershipLineageTarget state.lineage)
    union
    acceptance of
    Left _ ->
      Left
        ( StructuralBaseAcceptanceMismatch
            (terminalSourceUnionAcceptanceReporter acceptance)
        )
    Right () -> pure ()
  retainAcceptance acceptance state

establishStructuralBaseUnion ::
  MembershipBaseClosure ->
  Either
    StructuralBaseProblem
    (MembershipBaseClosure, TerminalSourceUnionEstablished)
establishStructuralBaseUnion state = do
  union <- maybe (Left StructuralBaseUnionUnavailable) Right state.agreedUnion
  -- Reusing the checked announce constructor here ensures that only the
  -- deterministic successor announcer can establish the transcript.
  _ <-
    mapTerminalProblem
      ( terminalSourceUnionAnnounce
          (heraldMembershipLineageTarget state.lineage)
          state.localHerald
          union
      )
  established <-
    mapTerminalProblem
      ( terminalSourceUnionEstablished
          (heraldMembershipLineageTarget state.lineage)
          union
          (Map.elems state.acceptances)
      )
  pure (state {established = Just established}, established)

receiveStructuralBaseUnionEstablished ::
  TopologyDigestReconstructor ->
  StructuralVersionVector ->
  ControlIndex ->
  TerminalSourceUnionEstablished ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem MembershipBaseClosure
receiveStructuralBaseUnionEstablished
  reconstruct
  appliedVector
  appliedControl
  supplied
  state = do
    expectedUnion <- deriveCoordinatorUnion reconstruct state
    let suppliedUnion = terminalSourceUnionEstablishedUnion supplied
    requireEqualUnion expectedUnion suppliedUnion
    _ <-
      mapTerminalProblem
        (terminalSourceUnionReady appliedVector appliedControl suppliedUnion)
    checked <-
      mapTerminalProblem
        ( terminalSourceUnionEstablished
            (heraldMembershipLineageTarget state.lineage)
            suppliedUnion
            (terminalSourceUnionEstablishedAcceptances supplied)
        )
    unless (checked == supplied) (Left StructuralBaseEstablishedMismatch)
    pure
      state
        { agreedUnion = Just suppliedUnion,
          acceptances =
            Map.fromList
              [ (terminalSourceUnionAcceptanceReporter acceptance, acceptance)
              | acceptance <- terminalSourceUnionEstablishedAcceptances supplied
              ],
          established = Just supplied
        }

structuralBaseAgreedUnion ::
  MembershipBaseClosure -> Maybe TerminalSourceUnion
structuralBaseAgreedUnion state = state.agreedUnion

structuralBaseAcceptanceEntries ::
  MembershipBaseClosure -> [(HeraldEpoch, TerminalSourceUnionAcceptance)]
structuralBaseAcceptanceEntries state = Map.toAscList state.acceptances

structuralBaseEstablishedUnion ::
  MembershipBaseClosure -> Maybe TerminalSourceUnionEstablished
structuralBaseEstablishedUnion state = state.established

-- | The sole cut which may install this successor base.  It has the distinct
-- membership-successor predecessor and the exact successor projection/control
-- frontier authenticated by the established terminal union.
expectedSuccessorStructuralCut ::
  MembershipBaseClosure -> Either StructuralBaseProblem TopologyCut
expectedSuccessorStructuralCut state = do
  established <- maybe (Left StructuralBaseUnionUnavailable) Right state.established
  let union = terminalSourceUnionEstablishedUnion established
  predecessor <-
    mapTerminalProblem
      ( terminalSourceUnionTopologyPredecessor
          state.lineage
          union
      )
  either
    (Left . StructuralBaseTopologyShapeProblem)
    Right
    ( topologyCut
        predecessor
        ( topologyFrontier
            (terminalSourceUnionSuccessorInitialVector union)
            (terminalSourceUnionTerminalControlPrefix union)
        )
        (terminalSourceUnionTerminalTopologyOccurrenceDigest union)
    )

-- | Install the exact membership-successor base through the Graph owner and
-- retain the resulting release witness in one package-private composition.
-- The Graph transition independently rechecks terminal history/control
-- readiness and the complete cut lineage before changing generations.
installSuccessorStructuralBase ::
  GraphProgress.StructuralProgressState ->
  MembershipBaseClosure ->
  Either
    StructuralBaseProblem
    (GraphProgress.StructuralProgressState, MembershipBaseClosure)
installSuccessorStructuralBase progress state = do
  cut <- expectedSuccessorStructuralCut state
  withBase <- recordInstalledSuccessorStructuralCut cut state
  base <- maybe (Left StructuralBaseUnionUnavailable) Right withBase.installedBase
  prepared <-
    either
      (Left . StructuralBaseGraphProgressProblem)
      Right
      ( GraphProgress.prepareMembershipSuccessorBaseInstallation
          (heraldMembershipLineageTarget state.lineage)
          base
          cut
          progress
      )
  pure
    ( GraphProgress.commitMembershipSuccessorBaseInstallation prepared,
      withBase
    )

-- | Lower-level checked observation used by focused schedules. Production
-- composition should prefer 'installSuccessorStructuralBase', which performs
-- the owner transition before exposing the witness. Downstream release paths
-- independently require this exact cut in the Graph owner's installed chain.
recordInstalledSuccessorStructuralCut ::
  TopologyCut ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem MembershipBaseClosure
recordInstalledSuccessorStructuralCut installed state = do
  expected <- expectedSuccessorStructuralCut state
  unless
    (installed == expected)
    ( Left
        ( StructuralBaseSuccessorCutMismatch
            (deriveTopologyCutId expected)
            (deriveTopologyCutId installed)
        )
    )
  established <- maybe (Left StructuralBaseUnionUnavailable) Right state.established
  base <-
    mapTerminalProblem
      ( establishedSuccessorStructuralBase
          state.lineage
          established
          installed
      )
  pure state {installedBase = Just base}

structuralBaseEvidence ::
  MembershipBaseClosure -> Maybe SuccessorStructuralBase
structuralBaseEvidence state = state.installedBase

structuralBaseReleases ::
  HeraldMembershipGenerationId -> MembershipBaseClosure -> Bool
structuralBaseReleases dependency state =
  maybe False (successorStructuralBaseReleases dependency) state.installedBase

supportingInventory ::
  TerminalSourcePayloadRequest ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem TerminalSourceInventory
supportingInventory request state =
  maybe
    (Left (StructuralBaseSupportingInventoryMissing wanted))
    Right
    ( find
        ((== wanted) . terminalSourceInventoryDigest)
        (Map.elems state.inventories)
    )
  where
    wanted = terminalSourcePayloadRequestSupportingInventory request

deriveCoordinatorUnion ::
  TopologyDigestReconstructor ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem TerminalSourceUnion
deriveCoordinatorUnion reconstruct state = do
  chain <- structuralBaseEstablishedChain state
  case reverse chain of
    latest : _ -> unless (topologyCutEstablishedId latest == terminalSourcePredecessorBaseCutId state.predecessorBase) (Left (StructuralBaseAnchorNotInstalled (topologyCutEstablishedId latest)))
    [] -> pure ()
  mapTerminalProblem
    ( deriveTerminalSourceUnionChecked
        state.historyLineage
        state.lineage
        state.predecessorBase
        (Map.elems state.inventories)
        state.payloads
        reconstruct
    )

retainAgreedUnion ::
  TerminalSourceUnion ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem MembershipBaseClosure
retainAgreedUnion supplied state = case state.agreedUnion of
  Nothing -> Right state {agreedUnion = Just supplied}
  Just incumbent -> do
    requireEqualUnion incumbent supplied
    Right state

requireEqualUnion ::
  TerminalSourceUnion ->
  TerminalSourceUnion ->
  Either StructuralBaseProblem ()
requireEqualUnion expected supplied = do
  equal <- mapTerminalProblem (compareTerminalSourceUnions expected supplied)
  unless
    equal
    ( Left
        ( StructuralBaseUnionMismatch
            (terminalSourceUnionDigest expected)
            (terminalSourceUnionDigest supplied)
        )
    )

retainAcceptance ::
  TerminalSourceUnionAcceptance ->
  MembershipBaseClosure ->
  Either StructuralBaseProblem MembershipBaseClosure
retainAcceptance supplied state =
  case Map.lookup reporter state.acceptances of
    Nothing ->
      Right
        state
          { acceptances = Map.insert reporter supplied state.acceptances
          }
    Just incumbent
      | incumbent == supplied -> Right state
      | otherwise -> Left (StructuralBaseAcceptanceReporterConflict reporter)
  where
    reporter = terminalSourceUnionAcceptanceReporter supplied

mapTerminalProblem ::
  Either TerminalSourceProblem value -> Either StructuralBaseProblem value
mapTerminalProblem = either (Left . StructuralBaseTerminalSourceProblem) Right
