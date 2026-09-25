{-# LANGUAGE OverloadedStrings #-}

module TestFixtures
  ( system,
    heraldIdA,
    heraldIdB,
    heraldEpochA,
    heraldEpochB,
    heraldEpochC,
    globalObject,
    processEpoch,
    nabla,
    deltaA,
    deltaB,
    occurrence,
    incarnationA,
    incarnationB,
    catalogueDigest,
    projectionDigest,
    itemDigest,
    membershipGeneration,
    successorMembershipGeneration,
    memberSetDigest,
    failureProbeDigest,
    structuralPublicationDigest,
    topologyOccurrenceDigest,
    topologyCut,
    terminalSourceUnionDigest,
    terminalSourceInventoryDigest,
    terminalSourceAcceptanceDigest,
    placementSequence1,
    streamSequence1,
    streamSequence2,
    streamSequence3,
    structuralSequence1,
    structuralVector,
    authorityEpochDtos,
    addressA,
    addressB,
    hello,
    heartbeatNonce,
    heartbeatDtos,
    knownA,
    knownB,
    knownCollection,
    applicationRoute,
    privateRoute,
    snapshot,
    directionAB,
    directionBA,
    gap,
    resumeOffer,
    resumeResponse,
    destinations,
    publication,
    structuralPublication,
    disappearanceProbeId,
    disappearanceProbeMarker,
    alignmentProbeMarker,
    alignmentCut,
    alignmentPlanId,
    failureProbeId,
    directFailureProbeRequest,
    directFailureProbeResponse,
    terminalSourceUnionAcceptanceA,
    terminalSourceUnionAcceptanceC,
    labelInstallation,
    admissionTopologyPredecessor,
    controlDtos,
    allEnvelopes,
    mustAdmit,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Protocol.Peer.Types
import Eclips.Public.Types.ReceiptRetirement (receiptRetirementPrefix)
import Eclips.Public.Types.SortId (SortId, mkSortId)

system :: SystemClaim
system = mustAdmit (systemClaim (claimBytes 1))

heraldIdA :: HeraldIdClaim
heraldIdA = mustAdmit (heraldIdClaim (claimBytes 2))

heraldIdB :: HeraldIdClaim
heraldIdB = mustAdmit (heraldIdClaim (claimBytes 3))

heraldEpochA :: HeraldEpochClaim
heraldEpochA = mustAdmit (heraldEpochClaim (claimBytes 4))

heraldEpochB :: HeraldEpochClaim
heraldEpochB = mustAdmit (heraldEpochClaim (claimBytes 5))

heraldEpochC :: HeraldEpochClaim
heraldEpochC = mustAdmit (heraldEpochClaim (claimBytes 36))

globalObject :: GlobalObjectClaim
globalObject = mustAdmit (globalObjectClaim (claimBytes 6))

processEpoch :: ProcessEpochClaim
processEpoch = mustAdmit (processEpochClaim (claimBytes 7))

nabla :: NablaClaim
nabla = mustAdmit (nablaClaim (claimBytes 8))

deltaA :: DeltaClaim
deltaA = mustAdmit (deltaClaim (claimBytes 9))

deltaB :: DeltaClaim
deltaB = mustAdmit (deltaClaim (claimBytes 10))

occurrence :: SortOccurrenceClaim
occurrence = mustAdmit (sortOccurrenceClaim (claimBytes 11))

incarnationA :: StoreIncarnationClaim
incarnationA = mustAdmit (storeIncarnationClaim (claimBytes 12))

incarnationB :: StoreIncarnationClaim
incarnationB = mustAdmit (storeIncarnationClaim (claimBytes 13))

catalogueDigest :: CatalogueDigestClaim
catalogueDigest = mustAdmit (catalogueDigestClaim (claimBytes 14))

projectionDigest :: InitialProjectionDigestClaim
projectionDigest = mustAdmit (initialProjectionDigestClaim (claimBytes 15))

itemDigest :: PeerItemDigestClaim
itemDigest = mustAdmit (peerItemDigestClaim (claimBytes 16))

membershipGeneration :: HeraldMembershipGenerationClaim
membershipGeneration =
  mustAdmit (heraldMembershipGenerationClaim (claimBytes 30))

successorMembershipGeneration :: HeraldMembershipGenerationClaim
successorMembershipGeneration =
  mustAdmit (heraldMembershipGenerationClaim (claimBytes 32))

memberSetDigest :: MemberSetDigestClaim
memberSetDigest = mustAdmit (memberSetDigestClaim (claimBytes 18))

failureProbeDigest :: HeraldFailureProbeDigestClaim
failureProbeDigest =
  mustAdmit (heraldFailureProbeDigestClaim (claimBytes 33))

structuralPublicationDigest :: StructuralPublicationDigestClaim
structuralPublicationDigest =
  mustAdmit (structuralPublicationDigestClaim (claimBytes 19))

topologyOccurrenceDigest :: TopologyOccurrenceDigestClaim
topologyOccurrenceDigest =
  mustAdmit (topologyOccurrenceDigestClaim (claimBytes 20))

topologyCut :: TopologyCutClaim
topologyCut = mustAdmit (topologyCutClaim (claimBytes 21))

terminalSourceUnionDigest :: TerminalSourceUnionDigestClaim
terminalSourceUnionDigest =
  mustAdmit (terminalSourceUnionDigestClaim (claimBytes 31))

terminalSourceInventoryDigest :: TerminalSourceInventoryDigestClaim
terminalSourceInventoryDigest =
  mustAdmit (terminalSourceInventoryDigestClaim (claimBytes 34))

terminalSourceAcceptanceDigest :: TerminalSourceAcceptanceDigestClaim
terminalSourceAcceptanceDigest =
  mustAdmit (terminalSourceAcceptanceDigestClaim (claimBytes 35))

terminalSourceAcceptanceDigestC :: TerminalSourceAcceptanceDigestClaim
terminalSourceAcceptanceDigestC =
  mustAdmit (terminalSourceAcceptanceDigestClaim (claimBytes 37))

classGenerationA, classGenerationB :: ContextClassGenerationClaim
classGenerationA = mustAdmit (contextClassGenerationClaim (claimBytes 22))
classGenerationB = mustAdmit (contextClassGenerationClaim (claimBytes 23))

memberReadyDigest :: MemberReadyEvidenceDigestClaim
memberReadyDigest = mustAdmit (memberReadyEvidenceDigestClaim (claimBytes 24))

historicalDigest :: HistoricalCertificateDigestClaim
historicalDigest = mustAdmit (historicalCertificateDigestClaim (claimBytes 25))

alignmentSnapshotDigest :: AlignmentSnapshotDigestClaim
alignmentSnapshotDigest = mustAdmit (alignmentSnapshotDigestClaim (claimBytes 26))

bootstrapDigest :: BootstrapEvidenceDigestClaim
bootstrapDigest = mustAdmit (bootstrapEvidenceDigestClaim (claimBytes 27))

placementSequence1 :: PositivePlacementSequenceDto
placementSequence1 = mustAdmit (positivePlacementSequenceDto 1)

streamSequence1 :: PositiveStreamSequenceDto
streamSequence1 = mustAdmit (positiveStreamSequenceDto 1)

streamSequence2 :: PositiveStreamSequenceDto
streamSequence2 = mustAdmit (positiveStreamSequenceDto 2)

streamSequence3 :: PositiveStreamSequenceDto
streamSequence3 = mustAdmit (positiveStreamSequenceDto 3)

structuralSequence1 :: PositiveStructuralSequenceDto
structuralSequence1 = mustAdmit (positiveStructuralSequenceDto 1)

obligationSequence1 :: PositiveAlignmentObligationSequenceDto
obligationSequence1 = mustAdmit (positiveAlignmentObligationSequenceDto 1)

subscriptionSequence1 :: PositiveAlignmentSubscriptionSequenceDto
subscriptionSequence1 = mustAdmit (positiveAlignmentSubscriptionSequenceDto 1)

evidenceSequence1 :: PositiveAlignmentEvidenceSequenceDto
evidenceSequence1 = mustAdmit (positiveAlignmentEvidenceSequenceDto 1)

publicationPosition1 :: PositiveHeraldPublicationPositionDto
publicationPosition1 = mustAdmit (positiveHeraldPublicationPositionDto 1)

structuralVector :: StructuralVersionVectorDto
structuralVector =
  mustAdmit
    ( structuralVersionVectorDto
        membershipGeneration
        memberSetDigest
        [ StructuralVectorEntryDto heraldEpochB EmptyStructuralPrefixDto,
          StructuralVectorEntryDto
            heraldEpochA
            (StructuralPrefixThroughDto structuralSequence1)
        ]
    )

authorityEpochDtos :: [AuthorityEpochDto]
authorityEpochDtos =
  [ GenesisAuthorityEpochDto,
    StructuralAuthorityEpochDto
      (StructuralOccurrenceIdDto heraldEpochA structuralSequence1)
      topologyCut,
    LabelAuthorityEpochDto (controlIndexDto 7)
  ]

addressA :: PeerAddressDto
addressA = peerAddressDto "tcp://127.0.0.1:41001"

addressB :: PeerAddressDto
addressB = peerAddressDto "tcp://[::1]:41002"

hello :: PeerHelloDto
hello =
  peerHelloDto
    system
    heraldIdA
    heraldEpochA
    (connectionNonceDto 0)
    [addressB, addressA, addressA]
    (controlIndexDto 0)
    membershipGeneration
    memberSetDigest
    catalogueDigest
    projectionDigest
    (Just (mustAdmit (Lifecycle.heraldLocator "localhost" 41003)))

heartbeatNonce :: PeerHeartbeatNonce
heartbeatNonce = peerHeartbeatNonce 0x0102030405060708

heartbeatDtos :: [PeerHeartbeatDto]
heartbeatDtos = [Ping heartbeatNonce, Pong heartbeatNonce]

knownA :: KnownHeraldDto
knownA = knownHeraldDto heraldIdA heraldEpochA [addressB, addressA]

knownB :: KnownHeraldDto
knownB = knownHeraldDto heraldIdB heraldEpochB []

knownCollection :: KnownHeraldCollectionDto
knownCollection = mustAdmit (knownHeraldCollectionDto [knownB, knownA])

sortId :: SortId
sortId = mustAdmit (mkSortId (claimBytes 17))

applicationRoute :: DeltaRouteDto
applicationRoute =
  ApplicationDeltaRouteDto
    deltaA
    sortId
    occurrence
    globalObject
    processEpoch
    incarnationA
    (controlIndexDto 7)

privateRoute :: DeltaRouteDto
privateRoute =
  PrivateSystemViewDeltaRouteDto
    DeltaRoleDto
    deltaB
    sortId
    occurrence
    incarnationB

snapshot :: PlacementSnapshotDto
snapshot =
  mustAdmit
    (placementSnapshotDto heraldEpochA placementSequence1 [privateRoute, applicationRoute])

directionAB :: StreamDirectionDto
directionAB = mustAdmit (streamDirectionDto heraldEpochA heraldEpochB)

directionBA :: StreamDirectionDto
directionBA = mustAdmit (streamDirectionDto heraldEpochB heraldEpochA)

gap :: GapSummaryDto
gap =
  mustAdmit
    ( gapSummaryDto
        directionBA
        (StreamPrefixThroughDto streamSequence1)
        [(streamSequence3, itemDigest)]
    )

resumeOffer :: ResumeOfferDto
resumeOffer =
  mustAdmit
    ( resumeOfferDto
        heraldEpochA
        heraldEpochB
        streamSequence3
        (StreamPrefixThroughDto streamSequence1)
        EmptyStreamPrefixDto
        gap
    )

resumeResponse :: ResumeResponseDto
resumeResponse =
  mustAdmit
    ( resumeResponseDto
        (StreamPrefixThroughDto streamSequence1)
        EmptyStreamPrefixDto
        streamSequence2
    )

destinations :: PublicationDestinationsDto
destinations =
  mustAdmit
    ( publicationDestinationsDto
        [ PublicationDestinationDto deltaB incarnationB WeakReplicaDto,
          PublicationDestinationDto deltaA incarnationA NormalReplicaDto
        ]
    )

publication :: PeerStreamItemDto
publication =
  PeerPublicationDto
    directionAB
    streamSequence1
    itemDigest
    (OrdinaryPublicationDto publicationBatch)

structuralPublication :: PeerStreamItemDto
structuralPublication =
  PeerPublicationDto
    directionAB
    streamSequence2
    itemDigest
    ( StructuralPublicationDto
        ( StructuralOccurrenceStampDto
            (StructuralOccurrenceIdDto heraldEpochA structuralSequence1)
            structuralVector
            publicationIdentifier
            structuralPublicationDigest
            EdgeCarrierDto
        )
        publicationBatch
    )

disappearanceProbeId :: DisappearanceProbeIdDto
disappearanceProbeId =
  mustAdmit
    ( disappearanceProbeIdDto
        (mustAdmit (disappearanceProbeDigestClaim (claimBytes 37)))
        (controlIndexDto 7)
    )

disappearanceProbeMarker :: PeerStreamItemDto
disappearanceProbeMarker =
  PeerDisappearanceProbeMarkerDto
    directionAB
    streamSequence3
    itemDigest
    ( DisappearanceProbeMarkerDto
        disappearanceProbeId
        ( DisappearanceCoordinateDto
            (mustAdmit (disappearanceSubjectDigestClaim (claimBytes 38)))
            membershipGeneration
            memberSetDigest
        )
        directionAB
        streamSequence3
    )

alignmentProbeMarker :: AlignmentControlDto
alignmentProbeMarker = AlignmentProbeMarkerDto disappearanceProbeId subscriptionId (storeRevisionDto 10)

publicationIdentifier :: PublicationIdDto
publicationIdentifier =
  PublicationIdDto
    nabla
    GenesisAuthorityEpochDto
    (nablaSequenceDto 0)
    heraldEpochA

publicationBatch :: PublicationBatchDto
publicationBatch =
  PublicationBatchDto
    publicationIdentifier
    processEpoch
    sortId
    occurrence
    (ByteString.pack [0, 1, 2, 3])
    NormalReplicaDto
    topologyCut
    (controlIndexDto 0)
    destinations

topologyFrontier :: TopologyFrontierDto
topologyFrontier =
  TopologyFrontierDto
    structuralVector
    (controlIndexDto 0)

canonicalTopologyCut :: TopologyCutDto
canonicalTopologyCut =
  TopologyCutDto
    (SameGenerationPredecessorDto topologyCut)
    topologyFrontier
    topologyOccurrenceDigest

-- The protocol preserves these opaque admission and recipe claims; the Herald
-- bridge subsequently checks their canonical domain identities and coordinates.
admissionTopologyPredecessor :: TopologyPredecessorDto
admissionTopologyPredecessor =
  AdmissionTopologyPredecessorDto
    topologyCut
    membershipGeneration
    successorMembershipGeneration
    structuralVector
    admissionStructuralVector
    "opaque canonical admission claim"
    (claimBytes 36)

admissionStructuralVector :: StructuralVersionVectorDto
admissionStructuralVector =
  mustAdmit
    ( structuralVersionVectorDto
        successorMembershipGeneration
        (mustAdmit (memberSetDigestClaim (claimBytes 38)))
        [ StructuralVectorEntryDto heraldEpochA (StructuralPrefixThroughDto structuralSequence1),
          StructuralVectorEntryDto heraldEpochB EmptyStructuralPrefixDto,
          StructuralVectorEntryDto heraldEpochC EmptyStructuralPrefixDto
        ]
    )

admissionTopologyCut :: TopologyCutDto
admissionTopologyCut =
  TopologyCutDto
    admissionTopologyPredecessor
    (TopologyFrontierDto admissionStructuralVector (controlIndexDto 8))
    topologyOccurrenceDigest

structuralReportA :: StructuralAppliedReportDto
structuralReportA =
  StructuralAppliedReportDto
    heraldEpochA
    structuralVector
    (controlIndexDto 0)
cutAcceptanceA, cutAcceptanceB :: TopologyCutAcceptanceDto
cutAcceptanceA =
  TopologyCutAcceptanceDto
    topologyCut
    heraldEpochA
    structuralVector
    (controlIndexDto 0)
cutAcceptanceB =
  TopologyCutAcceptanceDto
    topologyCut
    heraldEpochB
    structuralVector
    (controlIndexDto 0)

cutAcceptances :: TopologyCutAcceptancesDto
cutAcceptances =
  mustAdmit (topologyCutAcceptancesDto [cutAcceptanceB, cutAcceptanceA])

placementRevisionVector :: PhysicalPlacementRevisionVectorDto
placementRevisionVector =
  mustAdmit
    ( physicalPlacementRevisionVectorDto
        membershipGeneration
        memberSetDigest
        [ PhysicalPlacementRevisionEntryDto heraldEpochB placementSequence1,
          PhysicalPlacementRevisionEntryDto heraldEpochA placementSequence1
        ]
    )

alignmentPlanId :: AlignmentPlanIdDto
alignmentPlanId = AlignmentPlanIdDto 11 7 sortId occurrence topologyCut placementRevisionVector

alignmentCut :: AlignmentCutDto
alignmentCut =
  AlignmentCutDto
    11
    7
    sortId
    occurrence
    canonicalTopologyCut
    placementRevisionVector
    ( AlignmentMemberDto deltaA incarnationA heraldEpochA
        :| [AlignmentMemberDto deltaB incarnationB heraldEpochB]
    )
    [classGenerationB]
    [FreshMemberBaseEvidenceDto deltaA incarnationA (storeRevisionDto 0)]

obligationId :: AlignmentObligationIdDto
obligationId = AlignmentObligationIdDto heraldEpochA obligationSequence1

subscriptionId :: AlignmentSubscriptionIdDto
subscriptionId = AlignmentSubscriptionIdDto heraldEpochA subscriptionSequence1

alignmentObligation :: AlignmentObligationDto
alignmentObligation =
  AlignmentObligationDto
    obligationId
    (StructuralOccurrenceCauseDto (StructuralOccurrenceIdDto heraldEpochA structuralSequence1))
    sortId
    occurrence
    classGenerationA
    ( DestinationStoreDto deltaA incarnationA
        :| [DestinationStoreDto deltaB incarnationB]
    )
    classGenerationB
    NormalReplicaDto

memberReady :: ClassMemberReadyDto
memberReady =
  ClassMemberReadyDto
    evidenceSequence1
    classGenerationA
    incarnationA
    memberReadyDigest
    (storeRevisionDto 7)

historicalCertificate :: HistoricalCertificateDto
historicalCertificate =
  HistoricalCertificateDto
    classGenerationB
    incarnationB
    memberReadyDigest
    bootstrapDigest
    (storeRevisionDto 9)

primordialEvidence :: RetainedPublicationEvidenceDto
primordialEvidence =
  RetainedPublicationEvidenceDto
    publicationIdentifier
    sortId
    occurrence
    (ByteString.pack [4, 5, 6])
    WeakReplicaDto
    PrimordialRetainedObservationDto

routedEvidence :: RetainedPublicationEvidenceDto
routedEvidence =
  RetainedPublicationEvidenceDto
    ( PublicationIdDto
        nabla
        GenesisAuthorityEpochDto
        (nablaSequenceDto 1)
        heraldEpochA
    )
    sortId
    occurrence
    (ByteString.pack [4, 5, 6])
    NormalReplicaDto
    ( RoutedRetainedObservationDto
        processEpoch
        topologyCut
        (controlIndexDto 7)
        Nothing
    )

retainedStateEvidence :: RetainedStateEvidenceDto
retainedStateEvidence =
  RetainedStateEvidenceDto primordialEvidence routedEvidence NormalReplicaDto

alignmentControlDtos :: [AlignmentControlDto]
alignmentControlDtos =
  [ AlignmentPlanAnnouncedDto (AlignmentPlanAnnounceDto alignmentPlanId canonicalTopologyCut Nothing AlignmentPredecessorUsableDto [AlignmentPlanBindingClaimDto (deltaA :| [deltaB]) AlignmentCreatedDto classGenerationA] [] [AlignmentCutAnnounceDto classGenerationA alignmentCut]),
    AlignmentCutAnnouncedDto
      (AlignmentCutAnnounceDto classGenerationA alignmentCut),
    AlignmentHistoricalCertificateAdvertisedDto historicalCertificate,
    AlignmentSubscribeRequestedDto
      ( AlignmentSubscribeDto
          alignmentObligation
          subscriptionId
          incarnationB
          (FinalCertificateDto classGenerationB historicalDigest)
      ),
    AlignmentSnapshotStartedDto
      ( AlignmentSnapshotStartDto
          subscriptionId
          (storeRevisionDto 9)
          alignmentSnapshotDigest
          1
      ),
    AlignmentSnapshotChunkTransferredDto
      (AlignmentSnapshotChunkDto subscriptionId 0 [retainedStateEvidence]),
    AlignmentSnapshotEndedDto
      ( AlignmentSnapshotEndDto
          subscriptionId
          (storeRevisionDto 9)
          alignmentSnapshotDigest
      ),
    AlignmentChangeTransferredDto
      (AlignmentChangeDto subscriptionId (storeRevisionDto 10) routedEvidence),
    AlignmentLiveAdvertisedDto
      (AlignmentLiveDto subscriptionId (storeRevisionDto 10)),
    AlignmentAcknowledgedDto
      (AlignmentAckDto subscriptionId (storeRevisionDto 10)),
    AlignmentCancelledDto
      (AlignmentCancelDto subscriptionId AlignmentRelationRemovedDto),
    alignmentProbeMarker,
    AlignmentSubscribeRequestedDto
      ( AlignmentSubscribeDto
          ( AlignmentObligationDto
              obligationId
              (PredefinedDisappearanceCauseDto disappearanceProbeId (controlIndexDto 8))
              sortId
              occurrence
              classGenerationA
              (DestinationStoreDto deltaA incarnationA :| [DestinationStoreDto deltaB incarnationB])
              classGenerationB
              NormalReplicaDto
          )
          subscriptionId
          incarnationB
          (FinalCertificateDto classGenerationB historicalDigest)
      )
  ]

routeCutover :: PeerStreamItemDto
routeCutover =
  PeerRouteCutoverDto
    directionAB
    streamSequence3
    itemDigest
    (AlignmentRouteCutoverMarkerDto alignmentPlanId classGenerationA heraldEpochA heraldEpochB)

failureProbeId :: HeraldFailureProbeIdDto
failureProbeId =
  mustAdmit (heraldFailureProbeIdDto failureProbeDigest (controlIndexDto 7))

directFailureProbeRequest :: DirectFailureProbeRequestDto
directFailureProbeRequest =
  DirectFailureProbeRequestDto
    failureProbeId
    heraldEpochB
    membershipGeneration
    (mustAdmit (oracleVoterConfigurationClaim (claimBytes 50)))

directFailureProbeResponse :: DirectFailureProbeResponseDto
directFailureProbeResponse =
  DirectFailureProbeResponseDto
    directFailureProbeRequest
    DirectFailureProbeUnreachableDto

terminalSourceOccurrence :: StructuralOccurrenceIdDto
terminalSourceOccurrence =
  StructuralOccurrenceIdDto heraldEpochB structuralSequence1

terminalSourceInventory :: TerminalSourceInventoryDto
terminalSourceInventory =
  TerminalSourceInventoryDto
    membershipGeneration
    successorMembershipGeneration
    heraldEpochB
    heraldEpochA
    (StructuralPrefixThroughDto structuralSequence1)
    terminalSourceInventoryDigest
    (ByteString.pack [40, 41, 42])

terminalSourcePayloadRequest :: TerminalSourcePayloadRequestDto
terminalSourcePayloadRequest =
  TerminalSourcePayloadRequestDto
    membershipGeneration
    successorMembershipGeneration
    heraldEpochB
    terminalSourceOccurrence
    terminalSourceInventoryDigest

terminalSourcePayloadRelay :: TerminalSourcePayloadRelayDto
terminalSourcePayloadRelay =
  TerminalSourcePayloadRelayDto
    membershipGeneration
    successorMembershipGeneration
    heraldEpochB
    heraldEpochA
    terminalSourceInventoryDigest
    terminalSourceOccurrence
    structuralPublicationDigest
    (ByteString.pack [43, 44, 45])

terminalSourceUnion :: TerminalSourceUnionDto
terminalSourceUnion =
  TerminalSourceUnionDto
    membershipGeneration
    successorMembershipGeneration
    (mustAdmit (retiredHeraldsDto [heraldEpochB]))
    terminalSourceUnionDigest
    (ByteString.pack [46, 47, 48])

terminalSourceUnionAnnounce :: TerminalSourceUnionAnnounceDto
terminalSourceUnionAnnounce =
  TerminalSourceUnionAnnounceDto heraldEpochA terminalSourceUnion

terminalSourceUnionAcceptanceA,
  terminalSourceUnionAcceptanceC ::
    TerminalSourceUnionAcceptanceDto
terminalSourceUnionAcceptanceA =
  TerminalSourceUnionAcceptanceDto
    membershipGeneration
    successorMembershipGeneration
    (mustAdmit (retiredHeraldsDto [heraldEpochB]))
    terminalSourceUnionDigest
    heraldEpochA
    terminalSourceAcceptanceDigest
terminalSourceUnionAcceptanceC =
  TerminalSourceUnionAcceptanceDto
    membershipGeneration
    successorMembershipGeneration
    (mustAdmit (retiredHeraldsDto [heraldEpochB]))
    terminalSourceUnionDigest
    heraldEpochC
    terminalSourceAcceptanceDigestC

terminalSourceUnionAcceptances :: TerminalSourceUnionAcceptancesDto
terminalSourceUnionAcceptances =
  mustAdmit
    ( terminalSourceUnionAcceptancesDto
        [terminalSourceUnionAcceptanceC, terminalSourceUnionAcceptanceA]
    )

terminalSourceUnionEstablished :: TerminalSourceUnionEstablishedDto
terminalSourceUnionEstablished =
  TerminalSourceUnionEstablishedDto
    terminalSourceUnion
    terminalSourceUnionAcceptances

labelInstallation :: LabelInstallationReportDto
labelInstallation =
  mustAdmit
    ( labelInstallationReportDto
        (mustAdmit (labelDecisionClaim (claimBytes 51)))
        heraldEpochA
        (controlIndexDto 9)
        (mustAdmit (labelOutcomeDigestClaim (claimBytes 52)))
    )

controlDtos :: [PeerControlDto]
controlDtos =
  [ KnownHeraldsDto knownCollection,
    PlacementUpdateDto (FullPlacementSnapshotDto snapshot),
    PlacementAcknowledgedDto (PlacementAcknowledgementDto heraldEpochA placementSequence1),
    StreamReceivedDto directionAB EmptyStreamPrefixDto,
    StreamCompletedDto directionAB (receiptRetirementPrefix (Just (positiveStreamSequenceDtoWord64 streamSequence1))),
    StreamFrontierAdvancedDto directionAB streamSequence3,
    StreamResumeOfferedDto resumeOffer,
    StreamResumeAcceptedDto resumeResponse,
    StructuralAppliedReportedDto structuralReportA,
    TopologyCutAnnouncedDto
      (TopologyCutAnnounceDto heraldEpochA topologyCut canonicalTopologyCut),
    TopologyCutAnnouncedDto
      (TopologyCutAnnounceDto heraldEpochA topologyCut admissionTopologyCut),
    TopologyCutAcceptedDto cutAcceptanceB,
    TopologyCutEstablishedControlDto
      (TopologyCutEstablishedDto topologyCut canonicalTopologyCut cutAcceptances),
    TopologyCutEstablishedAcknowledgedDto
      (TopologyCutEstablishedAckDto topologyCut heraldEpochB membershipGeneration),
    AlignmentEvidenceDeliveredDto
      (mustAdmit (positiveAlignmentDeliverySequenceDto 3))
      (AlignmentPlanAcceptanceEvidenceDto (AlignmentPlanAcceptedDto alignmentPlanId heraldEpochA (HeraldPublicationPrefixThroughDto publicationPosition1))),
    AlignmentEvidenceDeliveredDto
      (mustAdmit (positiveAlignmentDeliverySequenceDto 1))
      (AlignmentCutAcceptanceEvidenceDto (AlignmentCutAcceptedDto classGenerationA heraldEpochA topologyCut placementRevisionVector (HeraldPublicationPrefixThroughDto publicationPosition1))),
    AlignmentEvidenceDeliveredDto
      (mustAdmit (positiveAlignmentDeliverySequenceDto 2))
      (AlignmentMemberReadyEvidenceDto memberReady),
    AlignmentEvidenceDeliveredDto
      (mustAdmit (positiveAlignmentDeliverySequenceDto 4))
      (AlignmentPlanObsoleteEvidenceDto (AlignmentPlanObsoleteDto alignmentPlanId heraldEpochA (AlignmentLostStoreDto heraldEpochA deltaA incarnationA placementSequence1 :| []))),
    AlignmentDeliveryProgressDto,
    DirectFailureProbeRequestedDto directFailureProbeRequest,
    DirectFailureProbeRespondedDto directFailureProbeResponse,
    TerminalSourceInventoryAdvertisedDto terminalSourceInventory,
    TerminalSourcePayloadRequestedDto terminalSourcePayloadRequest,
    TerminalSourcePayloadRelayedDto terminalSourcePayloadRelay,
    TerminalSourceUnionAnnouncedDto terminalSourceUnionAnnounce,
    TerminalSourceUnionAcceptedDto terminalSourceUnionAcceptanceA,
    TerminalSourceUnionEstablishedControlDto terminalSourceUnionEstablished,
    LabelInstalledDto labelInstallation
  ]
    <> fmap AlignmentControlDto alignmentControlDtos
    <> fmap PreparationControlDto preparationControlDtos

preparationControlDtos :: [PreparationControlDto]
preparationControlDtos =
  [ PreparationOfferedDto preparation locator "canonical startup transfer bytes",
    PreparationCancelledDto preparation,
    PreparationQueriedDto preparation
  ]
    <> [PreparationRepliedDto preparation status | status <- statuses]
  where
    preparation = mustAdmit (Lifecycle.childPreparation (claimBytes 40) 3 7)
    locator = mustAdmit (Lifecycle.heraldLocator "localhost" 41003)
    descriptor = mustAdmit (Lifecycle.connectionDescriptor locator (claimBytes 1) (claimBytes 4) (claimBytes 8))
    child = RemoteChildDto processEpoch (adminControl 9) descriptor
    adminControl = controlIndexDto
    statuses =
      [ RemotePreparationPendingDto [],
        RemotePreparationPendingDto ["reader"],
        RemotePreparationAvailableDto child Nothing,
        RemotePreparationAvailableDto child (Just ["reader"]),
        RemotePreparationClaimedDto child,
        RemotePreparationCancelledDto,
        RemotePreparationFailedDto Lifecycle.StartupNotLive,
        RemotePreparationFailedDto (Lifecycle.StartupRequiredObjectUnavailable "writer")
      ]

allEnvelopes :: [PeerEnvelope]
allEnvelopes =
  PeerHelloEnvelope hello
    : fmap PeerHeartbeatEnvelope heartbeatDtos
      <> fmap (PeerControlEnvelope mempty) controlDtos
      <> [ PeerPublicationEnvelope mempty publication,
           PeerPublicationEnvelope mempty structuralPublication,
           PeerPublicationEnvelope mempty routeCutover,
           PeerPublicationEnvelope mempty disappearanceProbeMarker
         ]

claimBytes :: Word -> ByteString.ByteString
claimBytes value = ByteString.replicate 32 (fromIntegral value)

mustAdmit :: (Show error) => Either error value -> value
mustAdmit = either (error . ("invalid peer-protocol fixture: " <>) . show) id
