{-# LANGUAGE ImportQualifiedPost #-}

-- | Authority-qualified, atomic retirement of one regular sort occurrence.
--
-- A successful disappearance proof is negative: every occurrence-qualified
-- dependant must already be absent.  This applicator therefore rechecks all
-- eight evidence owners and changes only the effective SortRegistry entry, the
-- cut-qualified Store projection of matching @sort-sort@ definition copies,
-- and the applied structural control prefix.
module Eclips.Herald.UseCase.RegularRetirement
  ( RegularRetirementProblem (..),
    RegularRetirementDisposition (..),
    RegularRetirementSummary,
    regularRetirementSummarySubject,
    regularRetirementSummarySortId,
    regularRetirementSummaryRetiredOccurrence,
    regularRetirementSummarySuccessorOccurrence,
    regularRetirementSummaryResolveIndex,
    regularRetirementSummaryDisposition,
    regularRetirementSummaryMatchingWork,
    regularRetirementSummaryPurgedStoreFacts,
    regularRetirementSummaryFalloutCount,
    regularRetirementSummaryLogicalWork,
    PreparedRegularRetirement,
    prepareDisappearanceRegularRetirement,
    prepareImportedRegularRetirement,
    commitRegularRetirement,
  )
where

import Control.Monad (unless)
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    HeraldPublicationPrefix (..),
  )
import Eclips.Domain.Disappearance
  ( CanonicalDescriptorDigest,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    deriveCanonicalDescriptorDigest,
    disappearanceResolutionOutcomeView,
    disappearanceResolutionSubject,
    disappearanceSubjectMembershipCoordinate,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    SortDefinitionOccurrenceId,
    SortId,
  )
import Eclips.Herald.Disappearance.Evidence qualified as Evidence
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceBlockerWitness,
    matchingPublicationPosition,
  )
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Genesis.Internal (checkedSystemId)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupGenesis,
    startupOracleProjectionState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Oracle.Disappearance (projectedDisappearanceProbeHeaderId)

data RegularRetirementProblem
  = RegularRetirementCapturedMembershipMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | RegularRetirementPredefinedSort SortId
  | RegularRetirementDescriptorMismatch SortId
  | RegularRetirementOccurrenceMismatch
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | RegularRetirementEvidenceProblem Evidence.DisappearanceEvidenceProblem
  | RegularRetirementEvidenceBlocked [DisappearanceBlockerWitness]
  | RegularRetirementPostCutMatchingWrite HeraldPublicationPosition
  | RegularRetirementSortRegistryProblem
      SortRegistry.RegularSortRetirementError
  | RegularRetirementStoreProblem Store.RegularDefinitionRetirementError
  | RegularRetirementProgressProblem GraphProgress.StructuralProgressProblem
  | RegularRetirementDispositionMismatch
  | RegularRetirementImportedResolutionInvalid
  deriving stock (Eq, Show)

data RegularRetirementDisposition
  = RegularRetirementApplied
  | RegularRetirementExactReplay
  deriving stock (Eq, Ord, Show)

data RegularRetirementSummary
  = RegularRetirementSummary
      DisappearanceSubject
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
      ControlIndex
      RegularRetirementDisposition
      Word64
      Word64
  deriving stock (Eq, Show)

regularRetirementSummarySubject ::
  RegularRetirementSummary -> DisappearanceSubject
regularRetirementSummarySubject
  (RegularRetirementSummary subject _ _ _ _ _ _ _) = subject

regularRetirementSummarySortId :: RegularRetirementSummary -> SortId
regularRetirementSummarySortId
  (RegularRetirementSummary _ sortId _ _ _ _ _ _) = sortId

regularRetirementSummaryRetiredOccurrence ::
  RegularRetirementSummary -> SortDefinitionOccurrenceId
regularRetirementSummaryRetiredOccurrence
  (RegularRetirementSummary _ _ occurrence _ _ _ _ _) = occurrence

regularRetirementSummarySuccessorOccurrence ::
  RegularRetirementSummary -> SortDefinitionOccurrenceId
regularRetirementSummarySuccessorOccurrence
  (RegularRetirementSummary _ _ _ occurrence _ _ _ _) = occurrence

regularRetirementSummaryResolveIndex ::
  RegularRetirementSummary -> ControlIndex
regularRetirementSummaryResolveIndex
  (RegularRetirementSummary _ _ _ _ index _ _ _) = index

regularRetirementSummaryDisposition ::
  RegularRetirementSummary -> RegularRetirementDisposition
regularRetirementSummaryDisposition
  (RegularRetirementSummary _ _ _ _ _ disposition _ _) = disposition

regularRetirementSummaryMatchingWork :: RegularRetirementSummary -> Word64
regularRetirementSummaryMatchingWork
  (RegularRetirementSummary _ _ _ _ _ _ matching _) = matching

regularRetirementSummaryPurgedStoreFacts :: RegularRetirementSummary -> Word64
regularRetirementSummaryPurgedStoreFacts
  (RegularRetirementSummary _ _ _ _ _ _ _ purged) = purged

-- | Exact variable consequence dimension.  Registry removal is a fixed
-- action; the only variable consequences are matched cut work and effective
-- Store facts removed from its projection.
regularRetirementSummaryFalloutCount :: RegularRetirementSummary -> Word64
regularRetirementSummaryFalloutCount summary =
  case regularRetirementSummaryDisposition summary of
    RegularRetirementExactReplay -> 0
    RegularRetirementApplied ->
      regularRetirementSummaryMatchingWork summary
        + regularRetirementSummaryPurgedStoreFacts summary

-- | Increment-8 work contract: four fixed checked owner actions, plus one unit
-- for every unique matching-work observation and effective Store fact.
regularRetirementSummaryLogicalWork :: RegularRetirementSummary -> Word64
regularRetirementSummaryLogicalWork summary =
  case regularRetirementSummaryDisposition summary of
    RegularRetirementExactReplay -> 0
    RegularRetirementApplied ->
      4 + regularRetirementSummaryFalloutCount summary

data PreparedRegularRetirement
  = PreparedRegularRetirement HeraldState RegularRetirementSummary

-- | Import a canonical retirement on an observer which has no local source
-- publication history. The Oracle projection already checked the complete
-- captured report set; no local absence proof or all-member barrier is begun.
prepareImportedRegularRetirement ::
  OracleProjection.ProjectedDisappearanceProbe ->
  HeraldState ->
  Either RegularRetirementProblem PreparedRegularRetirement
prepareImportedRegularRetirement projected state = do
  let probe = projectedDisappearanceProbeHeaderId (OracleProjection.projectedDisappearanceHeader projected)
  unless
    ( GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState state)
        && OracleProjection.oracleViewDisappearanceProbe probe (OracleProjection.oracleView (startupOracleProjectionState state)) == Just projected
    )
    (Left RegularRetirementImportedResolutionInvalid)
  case OracleProjection.projectedDisappearanceTerminal projected of
    Just (OracleProjection.ProjectedDisappearanceResolved outcome resolveIndex) ->
      case disappearanceResolutionOutcomeView outcome of
        RegularSortDefinitionRetired sortId digest retiredOccurrence _ successorOccurrence -> do
          entry <- authenticateRegistryEntryValues sortId digest retiredOccurrence state
          preparedRegistry <-
            either
              (Left . RegularRetirementSortRegistryProblem)
              Right
              ( case entry of
                  Just retained ->
                    SortRegistry.prepareExactRegularSortRetirement
                      (checkedSystemId (startupGenesis state))
                      retained
                      resolveIndex
                      successorOccurrence
                      (startupSortRegistryState state)
                  Nothing ->
                    SortRegistry.prepareUnseenRegularSortRetirement
                      (checkedSystemId (startupGenesis state))
                      sortId
                      digest
                      retiredOccurrence
                      resolveIndex
                      successorOccurrence
                      (startupSortRegistryState state)
              )
          let subject = disappearanceResolutionSubject outcome
          preparedStore <-
            either
              (Left . RegularRetirementStoreProblem)
              Right
              (Store.prepareRegularDefinitionRetirement subject EmptyHeraldPublicationPrefix resolveIndex (startupStoreState state))
          let registrySummary = SortRegistry.preparedRegularSortRetirementSummary preparedRegistry
              storeSummary = Store.preparedRegularDefinitionRetirementSummary preparedStore
          disposition <- case ( SortRegistry.regularSortRetirementSummaryDisposition registrySummary,
                                Store.regularDefinitionRetirementSummaryDisposition storeSummary
                              ) of
            (SortRegistry.RegularSortRetirementApplied, Store.RegularDefinitionRetirementApplied) -> Right RegularRetirementApplied
            (SortRegistry.RegularSortRetirementExactReplay, Store.RegularDefinitionRetirementExactReplay) -> Right RegularRetirementExactReplay
            _ -> Left RegularRetirementDispositionMismatch
          let registry = SortRegistry.commitRegularSortRetirement preparedRegistry
              (store, _) = Store.commitRegularDefinitionRetirement preparedStore
              withOwners = replaceStartupStoreState store . replaceStartupSortRegistryState registry $ state
          successor <- advanceStructuralControlProgress resolveIndex withOwners
          Right
            ( PreparedRegularRetirement
                successor
                ( RegularRetirementSummary
                    subject
                    sortId
                    retiredOccurrence
                    successorOccurrence
                    resolveIndex
                    disposition
                    0
                    (Store.regularDefinitionRetirementSummaryPurgedFactCount storeSummary)
                )
            )
        ControlledDisappearanceResolved {} -> Left RegularRetirementImportedResolutionInvalid
    _ -> Left RegularRetirementImportedResolutionInvalid

prepareDisappearanceRegularRetirement ::
  Disappearance.RegularRetirementAuthority ->
  HeraldState ->
  Either RegularRetirementProblem PreparedRegularRetirement
prepareDisappearanceRegularRetirement authority state = do
  entry <- authenticateRegistryEntry authority state
  preparedRegistry <-
    either
      (Left . RegularRetirementSortRegistryProblem)
      Right
      ( case entry of
          Just retained ->
            SortRegistry.prepareExactRegularSortRetirement
              localSystem
              retained
              resolveIndex
              successorOccurrence
              (startupSortRegistryState state)
          Nothing ->
            SortRegistry.prepareUnseenRegularSortRetirement
              localSystem
              sortId
              (Disappearance.regularRetirementAuthorityDescriptorDigest authority)
              retiredOccurrence
              resolveIndex
              successorOccurrence
              (startupSortRegistryState state)
      )
  preparedStore <-
    either
      (Left . RegularRetirementStoreProblem)
      Right
      ( Store.prepareRegularDefinitionRetirement
          subject
          localCut
          resolveIndex
          (startupStoreState state)
      )
  let registrySummary =
        SortRegistry.preparedRegularSortRetirementSummary preparedRegistry
      storeSummary =
        Store.preparedRegularDefinitionRetirementSummary preparedStore
      registryDisposition =
        SortRegistry.regularSortRetirementSummaryDisposition registrySummary
      storeDisposition =
        Store.regularDefinitionRetirementSummaryDisposition storeSummary
  case (registryDisposition, storeDisposition) of
    ( SortRegistry.RegularSortRetirementExactReplay,
      Store.RegularDefinitionRetirementExactReplay
      ) ->
        Right
          ( PreparedRegularRetirement
              state
              (retirementSummary RegularRetirementExactReplay 0 0)
          )
    disposition -> do
      -- A complete owner replay is state-identical even after membership has
      -- advanced. Every other attempt, including an impossible partial owner
      -- replay, remains bound to the exact membership captured by its
      -- authority and cannot cross that boundary.
      authenticateCapturedMembership authority state
      case disposition of
        ( SortRegistry.RegularSortRetirementApplied,
          Store.RegularDefinitionRetirementApplied
          ) -> do
            currentEvidence <-
              either
                (Left . RegularRetirementEvidenceProblem)
                Right
                ( Evidence.regularRetirementEvidenceSnapshotForHerald
                    subject
                    resolveIndex
                    state
                )
            let blockers =
                  Evidence.disappearanceEvidenceSnapshotBlockers currentEvidence
            unless (null blockers) (Left (RegularRetirementEvidenceBlocked blockers))
            case filter
              (not . positionCoveredBy localCut . matchingPublicationPosition)
              (Evidence.disappearanceEvidenceSnapshotMatchingPublications currentEvidence) of
              [] -> Right ()
              observation : _ ->
                Left
                  ( RegularRetirementPostCutMatchingWrite
                      (matchingPublicationPosition observation)
                  )
            let registry = SortRegistry.commitRegularSortRetirement preparedRegistry
                (store, _) = Store.commitRegularDefinitionRetirement preparedStore
                withOwners =
                  replaceStartupStoreState store
                    . replaceStartupSortRegistryState registry
                    $ state
            successor <- advanceStructuralControlProgress resolveIndex withOwners
            Right
              ( PreparedRegularRetirement
                  successor
                  ( retirementSummary
                      RegularRetirementApplied
                      ( fromIntegral
                          ( length
                              ( Evidence.disappearanceEvidenceSnapshotMatchingPublications
                                  currentEvidence
                              )
                          )
                      )
                      ( Store.regularDefinitionRetirementSummaryPurgedFactCount
                          storeSummary
                      )
                  )
              )
        _ -> Left RegularRetirementDispositionMismatch
  where
    subject = Disappearance.regularRetirementAuthoritySubject authority
    sortId = Disappearance.regularRetirementAuthoritySortId authority
    retiredOccurrence =
      Disappearance.regularRetirementAuthorityOccurrence authority
    resolveIndex =
      Disappearance.regularRetirementAuthorityResolveIndex authority
    successorOccurrence =
      Disappearance.regularRetirementAuthoritySuccessorOccurrence authority
    localCut =
      Disappearance.regularRetirementAuthorityLocalPublicationCut authority
    localSystem = checkedSystemId (startupGenesis state)
    retirementSummary disposition matching purged =
      RegularRetirementSummary
        subject
        sortId
        retiredOccurrence
        successorOccurrence
        resolveIndex
        disposition
        matching
        purged

authenticateCapturedMembership ::
  Disappearance.RegularRetirementAuthority ->
  HeraldState ->
  Either RegularRetirementProblem ()
authenticateCapturedMembership authority state =
  unless
    (actual == expected)
    (Left (RegularRetirementCapturedMembershipMismatch expected actual))
  where
    actual = Disappearance.regularRetirementAuthorityCoordinate authority
    oracleView = OracleProjection.oracleView (startupOracleProjectionState state)
    expected =
      disappearanceSubjectMembershipCoordinate
        (Disappearance.regularRetirementAuthoritySubject authority)
        (OracleProjection.oracleViewCurrentHeraldMembership oracleView)

authenticateRegistryEntry ::
  Disappearance.RegularRetirementAuthority ->
  HeraldState ->
  Either RegularRetirementProblem (Maybe SortRegistry.RegistryEntry)
authenticateRegistryEntry authority =
  authenticateRegistryEntryValues
    (Disappearance.regularRetirementAuthoritySortId authority)
    (Disappearance.regularRetirementAuthorityDescriptorDigest authority)
    (Disappearance.regularRetirementAuthorityOccurrence authority)

authenticateRegistryEntryValues ::
  SortId -> CanonicalDescriptorDigest -> SortDefinitionOccurrenceId -> HeraldState -> Either RegularRetirementProblem (Maybe SortRegistry.RegistryEntry)
authenticateRegistryEntryValues sortId expectedDigest expectedOccurrence state = do
  let entry = case SortRegistry.lookupRegularSortRetirement sortId expectedOccurrence registry of
        Just retired -> SortRegistry.regularSortRetirementEntry retired
        Nothing -> SortRegistry.lookupEffectiveSort sortId registry
  case entry of
    Nothing -> Right Nothing
    Just retained -> do
      case SortRegistry.registryEntryPredefinedRole retained of
        Nothing -> Right ()
        Just _ -> Left (RegularRetirementPredefinedSort sortId)
      unless
        ( deriveCanonicalDescriptorDigest (SortRegistry.registryEntryDescriptor retained)
            == expectedDigest
        )
        (Left (RegularRetirementDescriptorMismatch sortId))
      unless
        (SortRegistry.registryEntryOccurrenceId retained == expectedOccurrence)
        (Left (RegularRetirementOccurrenceMismatch sortId expectedOccurrence (SortRegistry.registryEntryOccurrenceId retained)))
      Right (Just retained)
  where
    registry = startupSortRegistryState state

advanceStructuralControlProgress ::
  ControlIndex ->
  HeraldState ->
  Either RegularRetirementProblem HeraldState
advanceStructuralControlProgress required state
  | current >= required = Right state
  | otherwise = do
      prepared <-
        either
          (Left . RegularRetirementProgressProblem)
          Right
          ( GraphProgress.prepareStructuralControlProgress
              required
              (startupStructuralProgressState state)
          )
      let (progress, _) = GraphProgress.commitStructuralControlProgress prepared
      Right (replaceStartupStructuralProgressState progress state)
  where
    current =
      GraphProgress.structuralAppliedControlPrefix
        (startupStructuralProgressState state)

positionCoveredBy ::
  HeraldPublicationPrefix -> HeraldPublicationPosition -> Bool
positionCoveredBy EmptyHeraldPublicationPrefix _ = False
positionCoveredBy (HeraldPublicationPrefixThrough through) position =
  position <= through

commitRegularRetirement ::
  PreparedRegularRetirement -> (HeraldState, RegularRetirementSummary)
commitRegularRetirement (PreparedRegularRetirement state summary) =
  (state, summary)
