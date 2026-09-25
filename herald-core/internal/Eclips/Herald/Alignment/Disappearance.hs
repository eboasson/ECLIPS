{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Subject-bound negative-evidence adapter for Alignment and Transfer.
--
-- The adapter scans the actual retained owner state, including Transfer's
-- private snapshot/change transcripts.  Its constructor and fact vocabulary
-- remain hidden so callers cannot substitute a caller-authored list for an
-- owner observation.
module Eclips.Herald.Alignment.Disappearance
  ( AlignmentDisappearanceView,
    alignmentDisappearanceView,
    alignmentRegularRetirementDisappearanceView,
    alignmentDisappearanceSubject,
    alignmentDisappearanceBlockers,
    alignmentDisappearanceFactCount,
    alignmentDisappearanceAbsenceDigest,
    alignmentDisappearanceIncomingCuts,
    alignmentDisappearanceOutgoingCuts,
  )
where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Serialize.Put qualified as Serialize
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( AlignmentCut,
    ContextClassGenerationId,
    HeraldPublicationPrefix (..),
    PhysicalPlacementRevisionVector,
    StoreRevision,
    alignmentCutCanonicalBytes,
    alignmentCutExactMembers,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutSortId,
    alignmentMemberDelta,
    contextClassGenerationIdBytes,
    heraldPublicationPositionWord64,
    physicalPlacementRevisionEntries,
    physicalPlacementRevisionMemberSetDigest,
    physicalPlacementRevisionMembershipGenerationId,
    placementRevisionWord64,
  )
import Eclips.Domain.Context (contextClassMembers)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    disappearanceSubjectView,
  )
import Eclips.Domain.Graph (VertexId (..))
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    SortDefinitionOccurrenceId,
    SortId,
    deltaIdBytes,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    heraldEpochBytes,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    storeIncarnationIdBytes,
    topologyCutIdBytes,
  )
import Eclips.Domain.MemberSet (memberSetDigestBytes)
import Eclips.Domain.Membership (heraldMembershipGenerationIdBytes)
import Eclips.Domain.Route (ReplicaStrength (..))
import Eclips.Domain.Topology (topologyCutCanonicalBytes)
import Eclips.Domain.Value (CanonicalValueBytes)
import Eclips.Herald.Alignment.Generation qualified as Generation
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Plan.Identity (alignmentPlanIdCanonicalBytes)
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Disappearance.OwnerEvidence
  ( OwnerEvidenceFact,
    OwnerEvidenceSnapshot,
    canonicalPublicationIsRegularRetirementResidual,
    canonicalPublicationIsSubjectRelevant,
    ownerEvidenceFact,
    ownerEvidenceSnapshot,
    ownerEvidenceSnapshotAbsenceDigest,
    ownerEvidenceSnapshotBlockers,
    ownerEvidenceSnapshotFactCount,
    ownerEvidenceSnapshotSubject,
  )
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
    IncomingAlignmentCut,
    OutgoingAlignmentCut,
    incomingAlignmentCut,
    outgoingAlignmentCut,
  )
import Eclips.Herald.Structural.Debt
  ( StructuralConsequenceDebt,
    StructuralIdentity (..),
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
    structuralConsequenceDebtEvidence,
    structuralConsequenceDebtKey,
    structuralDebtAffectedIdentities,
    structuralDebtKeySort,
  )

data AlignmentDisappearanceView
  = AlignmentDisappearanceView
      OwnerEvidenceSnapshot
      [IncomingAlignmentCut]
      [OutgoingAlignmentCut]
  deriving stock (Eq, Show)

type PublicationRelevance =
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  Maybe ControlIndex ->
  Bool

alignmentDisappearanceView ::
  DisappearanceSubject ->
  Alignment.State ->
  AlignmentDisappearanceView
alignmentDisappearanceView subject =
  alignmentDisappearanceViewWith
    False
    subject
    ( \sortId occurrence canonical _ ->
        canonicalPublicationIsSubjectRelevant
          subject
          sortId
          occurrence
          canonical
    )

-- | Alignment work which still depends on an already retired regular-sort
-- epoch. Exact occurrence coordinates remain blockers, while snapshot and
-- change evidence for an equal definition or structural sort reference is
-- successor work once its retained prerequisite reaches Resolve.  A stale
-- carrier transcript remains a blocker until its exact destination applications
-- are retained as terminally ignored and the source has acknowledged that
-- revision; the immutable settled transcript is then audit history, not live
-- dependency work.
alignmentRegularRetirementDisappearanceView ::
  DisappearanceSubject ->
  ControlIndex ->
  Alignment.State ->
  AlignmentDisappearanceView
alignmentRegularRetirementDisappearanceView subject resolveIndex =
  alignmentDisappearanceViewWith
    True
    subject
    (canonicalPublicationIsRegularRetirementResidual subject resolveIndex)

alignmentDisappearanceViewWith ::
  Bool ->
  DisappearanceSubject ->
  PublicationRelevance ->
  Alignment.State ->
  AlignmentDisappearanceView
alignmentDisappearanceViewWith ignoreTerminallySettled subject publicationIsRelevant owner =
  let snapshot =
        ownerEvidenceSnapshot
          alignmentEvidenceDomain
          subject
          (alignmentFacts ignoreTerminallySettled publicationIsRelevant subject owner)
      transfer = Alignment.alignmentTransferState owner
      incoming = incomingCuts transfer
      outgoing = outgoingCuts transfer
   in AlignmentDisappearanceView snapshot incoming outgoing

alignmentDisappearanceSubject ::
  AlignmentDisappearanceView -> DisappearanceSubject
alignmentDisappearanceSubject (AlignmentDisappearanceView snapshot _ _) =
  ownerEvidenceSnapshotSubject snapshot

alignmentDisappearanceBlockers ::
  AlignmentDisappearanceView -> [DisappearanceBlockerWitness]
alignmentDisappearanceBlockers
  (AlignmentDisappearanceView snapshot _ _) =
    ownerEvidenceSnapshotBlockers snapshot

alignmentDisappearanceFactCount :: AlignmentDisappearanceView -> Word64
alignmentDisappearanceFactCount (AlignmentDisappearanceView snapshot _ _) =
  ownerEvidenceSnapshotFactCount snapshot

alignmentDisappearanceAbsenceDigest ::
  AlignmentDisappearanceView -> Maybe DisappearanceEvidenceDigest
alignmentDisappearanceAbsenceDigest
  (AlignmentDisappearanceView snapshot _ _) =
    ownerEvidenceSnapshotAbsenceDigest snapshot

alignmentDisappearanceIncomingCuts ::
  AlignmentDisappearanceView -> [IncomingAlignmentCut]
alignmentDisappearanceIncomingCuts
  (AlignmentDisappearanceView _ cuts _) = cuts

alignmentDisappearanceOutgoingCuts ::
  AlignmentDisappearanceView -> [OutgoingAlignmentCut]
alignmentDisappearanceOutgoingCuts
  (AlignmentDisappearanceView _ _ cuts) = cuts

incomingCuts :: Transfer.State -> [IncomingAlignmentCut]
incomingCuts transfer =
  sort
    ( mapMaybe
        capture
        (Transfer.destinationSubscriptionEntries transfer)
    )
  where
    capture (identifier, retained)
      | Transfer.destinationSubscriptionCancellation retained /= Nothing = Nothing
      | otherwise = incomingAlignmentCut identifier <$> sourceHerald retained
    sourceHerald retained =
      ( Protocol.alignmentAttemptSourceHerald
          <$> Transfer.destinationSubscriptionAttempt retained
      )
        <|> Transfer.destinationSubscriptionBootstrapSourceHerald retained

outgoingCuts :: Transfer.State -> [OutgoingAlignmentCut]
outgoingCuts transfer =
  sort
    [ outgoingAlignmentCut
        identifier
        (Transfer.sourceSubscriptionSentThrough retained)
    | (identifier, retained) <- Transfer.sourceSubscriptionEntries transfer,
      Transfer.sourceSubscriptionCancellation retained == Nothing
    ]

alignmentFacts ::
  Bool ->
  PublicationRelevance ->
  DisappearanceSubject ->
  Alignment.State ->
  [OwnerEvidenceFact]
alignmentFacts ignoreTerminallySettled publicationIsRelevant subject owner =
  debtFacts
    <> unsettledPlanFacts
    <> obligationFacts
    <> attemptFacts
    <> bootstrapImportFacts
    <> bootstrapAttemptFacts
    <> pendingGenerationEvidenceFacts
    <> sourceTransferFacts
    <> destinationTransferFacts
    <> certificateFacts
  where
    debtFacts =
      indexedFacts
        AlignmentDebtBlocker
        [ ()
        | debt <-
            Alignment.liveAlignmentStructuralDebtEntries owner,
          debtMatches subject debt
        ]
    unsettledPlanFacts =
      [ ownerEvidenceFact
          AlignmentObligationBlocker
          (Serialize.runPut (putSizedBytes "ECLIPS-UNSETTLED-ALIGNMENT-PLAN" >> putSizedBytes (alignmentPlanIdCanonicalBytes (Plan.alignmentPlanId plan))))
      | plan <- Alignment.unsettledAlignmentPlans owner,
        planMatches subject plan
      ]
    obligationFacts =
      [ ownerEvidenceFact AlignmentObligationBlocker (obligationBytes obligation)
      | (_, obligation) <- Alignment.activeAlignmentObligationEntries owner,
        obligationMatches subject owner obligation
      ]
    attemptFacts =
      [ ownerEvidenceFact AlignmentAttemptBlocker (attemptBytes attempt)
      | (_, attempt) <- Alignment.alignmentAttemptEntries owner,
        obligationMatches
          subject
          owner
          (Protocol.alignmentAttemptObligation attempt)
      ]
    bootstrapImportFacts =
      [ ownerEvidenceFact
          AlignmentObligationBlocker
          (bootstrapImportBytes bootstrapImport)
      | (_, bootstrapImport) <- Alignment.bootstrapImportEntries owner,
        obligationMatches
          subject
          owner
          (Alignment.bootstrapImportSubscribeEnvelope bootstrapImport)
      ]
    bootstrapAttemptFacts =
      [ ownerEvidenceFact
          AlignmentAttemptBlocker
          (bootstrapAttemptBytes attempt)
      | (_, attempt) <- Alignment.bootstrapImportAttemptEntries owner,
        obligationMatches
          subject
          owner
          ( Protocol.alignmentSubscribeObligation
              (Alignment.bootstrapImportAttemptSubscribe attempt)
          )
      ]
    pendingGenerationEvidenceFacts =
      concatMap
        (pendingGenerationEvidenceFactsFor subject)
        (Alignment.pendingGenerationEvidenceEntries owner)
    transfer = Alignment.alignmentTransferState owner
    sourceTransferFacts =
      concatMap
        ( sourceSubscriptionFacts
            ignoreTerminallySettled
            publicationIsRelevant
            subject
            owner
        )
        (Transfer.sourceSubscriptionEntries transfer)
    destinationTransferFacts =
      concatMap
        ( destinationSubscriptionFacts
            ignoreTerminallySettled
            publicationIsRelevant
            subject
            owner
            transfer
        )
        (Transfer.destinationSubscriptionEntries transfer)
    certificateFacts =
      [ ownerEvidenceFact
          AlignmentCertificateBlocker
          (Protocol.historicalCertificateCanonicalBytes certificate)
      | ((generation, _), certificate) <-
          Alignment.liveAlignmentHistoricalCertificateEntries owner,
        generationMatches subject owner generation
      ]

pendingGenerationEvidenceFactsFor ::
  DisappearanceSubject ->
  Alignment.PendingGenerationEvidence ->
  [OwnerEvidenceFact]
pendingGenerationEvidenceFactsFor subject pending =
  case Alignment.pendingGenerationEvidenceControl pending of
    Protocol.AlignmentPlanAnnounced announce ->
      [ ownerEvidenceFact
          AlignmentObligationBlocker
          (pendingGenerationEvidenceBytes pending)
      | planAnnounceMatches subject announce
      ]
    Protocol.AlignmentPlanAcceptanceAdvertised _ -> uncertain
    Protocol.AlignmentPlanObsoleteAdvertised _ -> uncertain
    Protocol.AlignmentCutAnnounced announce ->
      [ ownerEvidenceFact
          AlignmentObligationBlocker
          (pendingGenerationEvidenceBytes pending)
      | cutAnnounceMatches subject announce
      ]
    Protocol.AlignmentCutAcceptanceAdvertised _ -> uncertain
    Protocol.AlignmentMemberReadyAdvertised _ -> uncertain
    Protocol.AlignmentHistoricalCertificateAdvertised _ -> uncertain
    _ ->
      error
        "Alignment pending-generation evidence contains an unsupported control"
  where
    -- Without the announced cut, a generation ID cannot exclude either a
    -- regular sort occurrence or a controlled object that the cut may later
    -- reveal as a Delta member.  Retain the uncertainty until normal admission
    -- drains the entry or source retirement removes it.
    uncertain =
      [ ownerEvidenceFact
          UncertainSemanticPayloadBlocker
          (pendingGenerationEvidenceBytes pending)
      ]

planMatches :: DisappearanceSubject -> Plan.AlignmentPlan -> Bool
planMatches subject plan =
  case disappearanceSubjectView subject of
    RegularSortDefinitionSubjectView sortId _ occurrence ->
      let retained = Plan.alignmentPlanIdSort (Plan.alignmentPlanId plan)
       in sortOccurrenceSortId retained == sortId
            && sortOccurrenceDefinition retained == occurrence
    ControlledPredefinedSubjectView object _ _ ->
      any
        ( any ((== object) . globalObjectIdFromDeltaId)
            . contextClassMembers
            . fst
        )
        (Plan.alignmentPlanBindings plan)

planAnnounceMatches :: DisappearanceSubject -> Protocol.AlignmentPlanAnnounce -> Bool
planAnnounceMatches subject announce =
  case disappearanceSubjectView subject of
    RegularSortDefinitionSubjectView sortId _ occurrence ->
      let retained = Plan.alignmentPlanIdSort (Protocol.alignmentPlanAnnounceId announce)
       in sortOccurrenceSortId retained == sortId
            && sortOccurrenceDefinition retained == occurrence
    ControlledPredefinedSubjectView object _ _ ->
      any
        (any ((== object) . globalObjectIdFromDeltaId) . Protocol.alignmentPlanBindingClaimMembers)
        (Protocol.alignmentPlanAnnounceBindings announce)

cutAnnounceMatches ::
  DisappearanceSubject ->
  Protocol.AlignmentCutAnnounce ->
  Bool
cutAnnounceMatches subject announce =
  case disappearanceSubjectView subject of
    RegularSortDefinitionSubjectView sortId _ occurrence ->
      cutMatches sortId occurrence cut
    ControlledPredefinedSubjectView object _ _ ->
      any
        ((== object) . globalObjectIdFromDeltaId . alignmentMemberDelta)
        (NonEmpty.toList (alignmentCutExactMembers cut))
  where
    cut = Protocol.alignmentCutAnnounceCut announce

indexedFacts :: DisappearanceBlockerClass -> [()] -> [OwnerEvidenceFact]
indexedFacts blockerClass facts =
  [ ownerEvidenceFact
      blockerClass
      (Serialize.runPut (Serialize.putWord64be index))
  | (index, ()) <- zip [0 ..] facts
  ]

debtMatches ::
  DisappearanceSubject ->
  StructuralConsequenceDebt ->
  Bool
debtMatches subject debt = case disappearanceSubjectView subject of
  RegularSortDefinitionSubjectView sortId _ occurrence ->
    sortOccurrenceSortId retainedSort == sortId
      && sortOccurrenceDefinition retainedSort == occurrence
  ControlledPredefinedSubjectView object _ _ ->
    any
      (identityMatchesObject object)
      ( Set.toAscList
          ( structuralDebtAffectedIdentities
              (structuralConsequenceDebtEvidence debt)
          )
      )
  where
    retainedSort = structuralDebtKeySort (structuralConsequenceDebtKey debt)

identityMatchesObject :: GlobalObjectId -> StructuralIdentity -> Bool
identityMatchesObject object identity = case identity of
  StructuralEdgeIdentity retained -> retained == object
  StructuralVertexIdentity vertex -> case vertex of
    NeutralVertex retained -> retained == object
    NablaVertex retained -> globalObjectIdFromNablaId retained == object
    DeltaVertex retained -> globalObjectIdFromDeltaId retained == object

obligationMatches ::
  DisappearanceSubject ->
  Alignment.State ->
  Protocol.AlignmentObligation ->
  Bool
obligationMatches subject owner obligation =
  case disappearanceSubjectView subject of
    RegularSortDefinitionSubjectView sortId _ occurrence ->
      Protocol.alignmentObligationSortId obligation == sortId
        && Protocol.alignmentObligationSortDefinitionOccurrenceId obligation
          == occurrence
    ControlledPredefinedSubjectView object _ _ ->
      any
        ( \store ->
            globalObjectIdFromDeltaId (Protocol.destinationStoreDelta store)
              == object
        )
        (NonEmpty.toList (Protocol.alignmentObligationDestinationStores obligation))
        || generationMatches
          subject
          owner
          (Protocol.alignmentObligationSourceGeneration obligation)
        || generationMatches
          subject
          owner
          (Protocol.alignmentObligationDestinationGeneration obligation)

generationMatches ::
  DisappearanceSubject ->
  Alignment.State ->
  ContextClassGenerationId ->
  Bool
generationMatches subject owner identifier =
  case Alignment.lookupAlignmentGeneration identifier owner of
    Nothing -> False
    Just generation -> case disappearanceSubjectView subject of
      RegularSortDefinitionSubjectView sortId _ occurrence ->
        cutMatches sortId occurrence (Generation.alignmentGenerationCut generation)
      ControlledPredefinedSubjectView object _ _ ->
        any
          ((== object) . globalObjectIdFromDeltaId)
          ( NonEmpty.toList
              ( contextClassMembers
                  (Generation.alignmentGenerationContextClass generation)
              )
          )

cutMatches :: SortId -> SortDefinitionOccurrenceId -> AlignmentCut -> Bool
cutMatches sortId occurrence cut =
  alignmentCutSortId cut == sortId
    && alignmentCutSortDefinitionOccurrenceId cut == occurrence

sourceSubscriptionFacts ::
  Bool ->
  PublicationRelevance ->
  DisappearanceSubject ->
  Alignment.State ->
  (Protocol.AlignmentSubscriptionId, Transfer.SourceSubscription) ->
  [OwnerEvidenceFact]
sourceSubscriptionFacts
  ignoreTerminallySettled
  publicationIsRelevant
  subject
  owner
  (identifier, retained)
    | Transfer.sourceSubscriptionCancellation retained /= Nothing = []
    | otherwise = subscriptionFact <> snapshotFact <> changeFact
    where
      subscribe = Transfer.sourceSubscriptionSubscribe retained
      obligation = Protocol.alignmentSubscribeObligation subscribe
      snapshotEvidence = Transfer.sourceSubscriptionSnapshotFacts retained
      changes = Transfer.sourceSubscriptionSentChanges retained
      matchingSnapshotAll =
        filter (stateEvidenceMatches publicationIsRelevant) snapshotEvidence
      matchingChangesAll =
        filter
          ( publicationMatches publicationIsRelevant
              . Protocol.alignmentChangeRetainedTransition
          )
          changes
      matchingSnapshot
        | ignoreTerminallySettled && sourceSnapshotIsAcknowledged retained = []
        | otherwise = matchingSnapshotAll
      matchingChanges =
        filter
          ( \change ->
              not
                ( ignoreTerminallySettled
                    && sourceChangeIsAcknowledged retained change
                )
          )
          matchingChangesAll
      coordinateMatches = obligationMatches subject owner obligation
      relevant =
        coordinateMatches
          || not (null matchingSnapshot)
          || not (null matchingChanges)
      subscriptionFact =
        [ ownerEvidenceFact AlignmentSubscriptionBlocker (subscriptionBytes identifier)
        | relevant,
          Transfer.sourceSubscriptionCancellation retained == Nothing
        ]
      snapshotFact =
        [ ownerEvidenceFact
            AlignmentSnapshotBlocker
            ( evidenceBytes
                identifier
                (if coordinateMatches then snapshotEvidence else matchingSnapshot)
            )
        | coordinateMatches || not (null matchingSnapshot)
        ]
      changeFact =
        [ ownerEvidenceFact
            AlignmentChangeLogBlocker
            ( changeBytes
                identifier
                (if coordinateMatches then changes else matchingChanges)
            )
        | not (null changes),
          coordinateMatches || not (null matchingChanges)
        ]

sourceSnapshotIsAcknowledged :: Transfer.SourceSubscription -> Bool
sourceSnapshotIsAcknowledged retained =
  maybe
    False
    (>= Transfer.sourceSubscriptionBaseRevision retained)
    (Transfer.sourceSubscriptionAcknowledgedThrough retained)

sourceChangeIsAcknowledged ::
  Transfer.SourceSubscription -> Protocol.AlignmentChange -> Bool
sourceChangeIsAcknowledged retained change =
  maybe
    False
    (>= Protocol.alignmentChangeStoreRevision change)
    (Transfer.sourceSubscriptionAcknowledgedThrough retained)

destinationSubscriptionFacts ::
  Bool ->
  PublicationRelevance ->
  DisappearanceSubject ->
  Alignment.State ->
  Transfer.State ->
  (Protocol.AlignmentSubscriptionId, Transfer.DestinationSubscription) ->
  [OwnerEvidenceFact]
destinationSubscriptionFacts
  ignoreTerminallySettled
  publicationIsRelevant
  subject
  owner
  transfer
  (identifier, retained)
    | Transfer.destinationSubscriptionCancellation retained /= Nothing = []
    | otherwise = subscriptionFact <> snapshotFact <> changeFact
    where
      subscribe = Transfer.destinationSubscriptionSubscribe retained
      obligation = Protocol.alignmentSubscribeObligation subscribe
      snapshotEvidence = Transfer.destinationSubscriptionSnapshotFacts retained
      changes = Transfer.destinationSubscriptionChanges retained
      matchingSnapshot =
        filter
          ( stateEvidenceHasUnsettledRelevantPublication
              ignoreTerminallySettled
              publicationIsRelevant
              transfer
              identifier
              obligation
              (Transfer.destinationSubscriptionAppliedSnapshotBaseRevision retained)
          )
          snapshotEvidence
      matchingChanges =
        filter
          ( \change ->
              let publication = Protocol.alignmentChangeRetainedTransition change
               in publicationMatches publicationIsRelevant publication
                    && not
                      ( ignoreTerminallySettled
                          && destinationEvidenceTerminallyIgnored
                            transfer
                            identifier
                            obligation
                            (Protocol.alignmentChangeStoreRevision change)
                            Transfer.AppliedContinuingChange
                            publication
                      )
          )
          changes
      coordinateMatches = obligationMatches subject owner obligation
      relevant =
        coordinateMatches
          || not (null matchingSnapshot)
          || not (null matchingChanges)
      subscriptionFact =
        [ ownerEvidenceFact AlignmentSubscriptionBlocker (subscriptionBytes identifier)
        | relevant,
          Transfer.destinationSubscriptionCancellation retained == Nothing
        ]
      snapshotFact =
        [ ownerEvidenceFact
            AlignmentSnapshotBlocker
            ( evidenceBytes
                identifier
                (if coordinateMatches then snapshotEvidence else matchingSnapshot)
            )
        | Transfer.destinationSubscriptionHasSnapshotTranscript retained,
          coordinateMatches || not (null matchingSnapshot)
        ]
      changeFact =
        [ ownerEvidenceFact
            AlignmentChangeLogBlocker
            ( changeBytes
                identifier
                (if coordinateMatches then changes else matchingChanges)
            )
        | not (null changes),
          coordinateMatches || not (null matchingChanges)
        ]

stateEvidenceHasUnsettledRelevantPublication ::
  Bool ->
  PublicationRelevance ->
  Transfer.State ->
  Protocol.AlignmentSubscriptionId ->
  Protocol.AlignmentObligation ->
  Maybe StoreRevision ->
  Protocol.RetainedStateEvidence ->
  Bool
stateEvidenceHasUnsettledRelevantPublication
  ignoreTerminallySettled
  publicationIsRelevant
  transfer
  identifier
  obligation
  appliedBase
  evidence =
    unsettled
      Transfer.AppliedSnapshotRepresentative
      (Protocol.retainedStateEvidenceRepresentative evidence)
      || unsettled
        Transfer.AppliedSnapshotStrengthWitness
        (Protocol.retainedStateEvidenceStrengthWitness evidence)
    where
      unsettled kind publication =
        publicationMatches publicationIsRelevant publication
          && not
            ( ignoreTerminallySettled
                && maybe
                  False
                  ( \revision ->
                      destinationEvidenceTerminallyIgnored
                        transfer
                        identifier
                        obligation
                        revision
                        kind
                        publication
                  )
                  appliedBase
            )

destinationEvidenceTerminallyIgnored ::
  Transfer.State ->
  Protocol.AlignmentSubscriptionId ->
  Protocol.AlignmentObligation ->
  StoreRevision ->
  Transfer.AppliedDestinationEvidenceKind ->
  Protocol.RetainedPublicationEvidence ->
  Bool
destinationEvidenceTerminallyIgnored
  transfer
  identifier
  obligation
  expectedRevision
  expectedKind
  publication =
    all
      destinationSettled
      (NonEmpty.toList (Protocol.alignmentObligationDestinationStores obligation))
    where
      destinationSettled destination =
        any
          ( \applied ->
              Transfer.appliedDestinationEvidenceSubscription applied == identifier
                && Transfer.appliedDestinationEvidenceDestination applied == destination
                && Transfer.appliedDestinationEvidencePublication applied == publication
                && Transfer.appliedDestinationEvidenceKind applied == expectedKind
                && Transfer.appliedDestinationEvidenceSourceRevision applied
                  == expectedRevision
                && Transfer.appliedDestinationEvidenceDisposition applied
                  `elem` [ Transfer.StoreTerminallyIgnored,
                           Transfer.ControlledSuppressed
                         ]
          )
          (Transfer.appliedDestinationEvidenceEntries transfer)

stateEvidenceMatches ::
  PublicationRelevance -> Protocol.RetainedStateEvidence -> Bool
stateEvidenceMatches publicationIsRelevant evidence =
  publicationMatches
    publicationIsRelevant
    (Protocol.retainedStateEvidenceRepresentative evidence)
    || publicationMatches
      publicationIsRelevant
      (Protocol.retainedStateEvidenceStrengthWitness evidence)

publicationMatches ::
  PublicationRelevance -> Protocol.RetainedPublicationEvidence -> Bool
publicationMatches publicationIsRelevant evidence =
  publicationIsRelevant
    (Protocol.retainedPublicationEvidenceSortId evidence)
    (Protocol.retainedPublicationEvidenceSortDefinitionOccurrenceId evidence)
    (Protocol.retainedPublicationEvidenceCanonicalValue evidence)
    (Protocol.retainedPublicationEvidenceControlPrerequisite evidence)

pendingGenerationEvidenceBytes ::
  Alignment.PendingGenerationEvidence -> ByteString
pendingGenerationEvidenceBytes pending =
  Serialize.runPut $ do
    putSizedBytes "ECLIPS-STEP16-PENDING-GENERATION-EVIDENCE-V1"
    Serialize.putByteString
      (heraldEpochBytes (Alignment.pendingGenerationEvidenceRemoteHerald pending))
    case Alignment.pendingGenerationEvidenceControl pending of
      Protocol.AlignmentPlanAnnounced announce -> do
        Serialize.putWord8 4
        putPlanAnnounce announce
      Protocol.AlignmentPlanAcceptanceAdvertised accepted -> do
        Serialize.putWord8 5
        putSizedBytes (alignmentPlanIdCanonicalBytes (Protocol.alignmentPlanAcceptedId accepted))
        Serialize.putByteString (heraldEpochBytes (Protocol.alignmentPlanAcceptedHerald accepted))
        putPublicationPrefix (Protocol.alignmentPlanAcceptedPublicationPrefix accepted)
      Protocol.AlignmentPlanObsoleteAdvertised report -> do
        Serialize.putWord8 6
        putSizedBytes (Protocol.alignmentPlanObsoleteCanonicalBytes report)
      Protocol.AlignmentCutAnnounced announce -> do
        Serialize.putWord8 0
        putSizedBytes
          (alignmentCutCanonicalBytes (Protocol.alignmentCutAnnounceCut announce))
      Protocol.AlignmentCutAcceptanceAdvertised accepted -> do
        Serialize.putWord8 1
        putCutAcceptance accepted
      Protocol.AlignmentMemberReadyAdvertised ready -> do
        Serialize.putWord8 2
        putSizedBytes (Protocol.classMemberReadyCanonicalBytes ready)
      Protocol.AlignmentHistoricalCertificateAdvertised certificate -> do
        Serialize.putWord8 3
        putSizedBytes (Protocol.historicalCertificateCanonicalBytes certificate)
      _ ->
        error
          "Alignment pending-generation evidence contains an unsupported control"

putPlanAnnounce :: Protocol.AlignmentPlanAnnounce -> Serialize.Put
putPlanAnnounce announce = do
  putSizedBytes (alignmentPlanIdCanonicalBytes (Protocol.alignmentPlanAnnounceId announce))
  putSizedBytes (topologyCutCanonicalBytes (Protocol.alignmentPlanAnnounceTopology announce))
  case Protocol.alignmentPlanAnnouncePredecessor announce of
    Nothing -> Serialize.putWord8 0
    Just predecessor -> Serialize.putWord8 1 >> putSizedBytes (alignmentPlanIdCanonicalBytes predecessor)
  Serialize.putWord8 (case Protocol.alignmentPlanAnnouncePredecessorStatus announce of Plan.AlignmentPredecessorUsable -> 0; Plan.AlignmentPredecessorInvalidated -> 1; Plan.AlignmentPredecessorReset -> 2)
  let bindings = Protocol.alignmentPlanAnnounceBindings announce
      relations = Protocol.alignmentPlanAnnounceRelations announce
      created = Protocol.alignmentPlanAnnounceCreatedCuts announce
  Serialize.putWord64be (fromIntegral (length bindings))
  mapM_ putBinding bindings
  Serialize.putWord64be (fromIntegral (length relations))
  mapM_ putRelation relations
  Serialize.putWord64be (fromIntegral (length created))
  mapM_ (putSizedBytes . alignmentCutCanonicalBytes . Protocol.alignmentCutAnnounceCut) created
  where
    putBinding binding = do
      let members = Protocol.alignmentPlanBindingClaimMembers binding
      Serialize.putWord64be (fromIntegral (length members))
      mapM_ (Serialize.putByteString . deltaIdBytes) members
      Serialize.putWord8 (case Protocol.alignmentPlanBindingClaimDisposition binding of Plan.AlignmentCreated -> 0; Plan.AlignmentCarried -> 1)
      Serialize.putByteString (contextClassGenerationIdBytes (Protocol.alignmentPlanBindingClaimGeneration binding))
    putRelation relation = do
      Serialize.putByteString (contextClassGenerationIdBytes (Protocol.alignmentPlanRelationClaimSource relation))
      Serialize.putByteString (contextClassGenerationIdBytes (Protocol.alignmentPlanRelationClaimDestination relation))
      Serialize.putWord8 (case Protocol.alignmentPlanRelationClaimStrength relation of Weak -> 0; Normal -> 1)

putCutAcceptance :: Protocol.AlignmentCutAccepted -> Serialize.Put
putCutAcceptance accepted = do
  Serialize.putByteString
    (contextClassGenerationIdBytes (Protocol.alignmentCutAcceptedGeneration accepted))
  Serialize.putByteString
    (heraldEpochBytes (Protocol.alignmentCutAcceptedHerald accepted))
  Serialize.putByteString
    (topologyCutIdBytes (Protocol.alignmentCutAcceptedTopologyCut accepted))
  putPhysicalPlacementVector
    (Protocol.alignmentCutAcceptedPlacementVector accepted)
  putPublicationPrefix (Protocol.alignmentCutAcceptedPublicationPrefix accepted)

putPublicationPrefix :: HeraldPublicationPrefix -> Serialize.Put
putPublicationPrefix prefix =
  case prefix of
    EmptyHeraldPublicationPrefix -> Serialize.putWord8 0
    HeraldPublicationPrefixThrough position -> do
      Serialize.putWord8 1
      Serialize.putWord64be (heraldPublicationPositionWord64 position)

putPhysicalPlacementVector ::
  PhysicalPlacementRevisionVector -> Serialize.Put
putPhysicalPlacementVector vector = do
  Serialize.putByteString
    ( heraldMembershipGenerationIdBytes
        (physicalPlacementRevisionMembershipGenerationId vector)
    )
  Serialize.putByteString
    (memberSetDigestBytes (physicalPlacementRevisionMemberSetDigest vector))
  let entries = NonEmpty.toList (physicalPlacementRevisionEntries vector)
  Serialize.putWord64be (fromIntegral (length entries))
  mapM_
    ( \(herald, revision) -> do
        Serialize.putByteString (heraldEpochBytes herald)
        Serialize.putWord64be (placementRevisionWord64 revision)
    )
    entries

obligationBytes :: Protocol.AlignmentObligation -> ByteString
obligationBytes obligation =
  Serialize.runPut $ do
    let identifier = Protocol.alignmentObligationIdValue obligation
    Serialize.putByteString
      (heraldEpochBytes (Protocol.alignmentObligationIdDestinationHerald identifier))
    Serialize.putWord64be
      ( Protocol.alignmentObligationSequenceWord64
          (Protocol.alignmentObligationIdSequence identifier)
      )
    Serialize.putByteString
      (sortIdBytes (Protocol.alignmentObligationSortId obligation))
    Serialize.putByteString
      ( sortDefinitionOccurrenceIdBytes
          (Protocol.alignmentObligationSortDefinitionOccurrenceId obligation)
      )

attemptBytes :: Protocol.AlignmentAttempt -> ByteString
attemptBytes attempt =
  obligationBytes (Protocol.alignmentAttemptObligation attempt)
    <> subscriptionBytes (Protocol.alignmentAttemptSubscriptionId attempt)
    <> heraldEpochBytes (Protocol.alignmentAttemptSourceHerald attempt)
    <> storeIncarnationIdBytes
      (Protocol.alignmentAttemptSourceStoreIncarnation attempt)

bootstrapImportBytes :: Alignment.BootstrapImport -> ByteString
bootstrapImportBytes =
  obligationBytes . Alignment.bootstrapImportSubscribeEnvelope

bootstrapAttemptBytes :: Alignment.BootstrapImportAttempt -> ByteString
bootstrapAttemptBytes attempt =
  obligationBytes (Protocol.alignmentSubscribeObligation subscribe)
    <> heraldEpochBytes (Alignment.bootstrapImportAttemptSourceHerald attempt)
    <> subscriptionBytes (Protocol.alignmentSubscribeSubscriptionId subscribe)
    <> storeIncarnationIdBytes
      (Protocol.alignmentSubscribeSourceStoreIncarnation subscribe)
  where
    subscribe = Alignment.bootstrapImportAttemptSubscribe attempt

subscriptionBytes :: Protocol.AlignmentSubscriptionId -> ByteString
subscriptionBytes identifier =
  Serialize.runPut $ do
    Serialize.putByteString
      ( heraldEpochBytes
          (Protocol.alignmentSubscriptionIdDestinationHerald identifier)
      )
    Serialize.putWord64be
      ( Protocol.alignmentSubscriptionSequenceWord64
          (Protocol.alignmentSubscriptionIdSequence identifier)
      )

evidenceBytes ::
  Protocol.AlignmentSubscriptionId ->
  [Protocol.RetainedStateEvidence] ->
  ByteString
evidenceBytes identifier evidence =
  Serialize.runPut $ do
    putSizedBytes (subscriptionBytes identifier)
    let canonical = sort (fmap Protocol.retainedStateEvidenceCanonicalBytes evidence)
    Serialize.putWord64be (fromIntegral (length canonical))
    mapM_ putSizedBytes canonical

changeBytes ::
  Protocol.AlignmentSubscriptionId ->
  [Protocol.AlignmentChange] ->
  ByteString
changeBytes identifier changes =
  Serialize.runPut $ do
    putSizedBytes (subscriptionBytes identifier)
    let canonical =
          sort
            ( fmap
                ( Protocol.retainedPublicationEvidenceCanonicalBytes
                    . Protocol.alignmentChangeRetainedTransition
                )
                changes
            )
    Serialize.putWord64be (fromIntegral (length canonical))
    mapM_ putSizedBytes canonical

putSizedBytes :: ByteString -> Serialize.Put
putSizedBytes bytes = do
  Serialize.putWord64be (fromIntegral (ByteString.length bytes))
  Serialize.putByteString bytes

alignmentEvidenceDomain :: ByteString
alignmentEvidenceDomain = "ECLIPS-DISAPPEARANCE-ALIGNMENT-WORK"
