-- | Exhaustive structural conversion between the peer wire DTOs and the pure
-- Herald owners.
--
-- This module is the only bridge allowed to inspect an opaque inbound
-- publication item.  It establishes representation and immutable-payload
-- integrity only; stateful membership, placement, stream-frontier, and binding
-- admission remains in the Herald transition.
module Eclips.Herald.Peer.RPC.Internal
  ( PeerRpcBridgeError (..),
    helloFromDto,
    helloToDto,
    controlFromDto,
    controlToDto,
    publicationFromDto,
    logicalAttemptToDto,
    structuralAppliedReportFromDto,
    structuralAppliedReportToDto,
    topologyCutAnnounceFromDto,
    topologyCutAnnounceToDto,
    topologyCutAcceptanceFromDto,
    topologyCutAcceptanceToDto,
    topologyCutEstablishedFromDto,
    topologyCutEstablishedToDto,
    topologyCutEstablishedAckFromDto,
    topologyCutEstablishedAckToDto,
    retainedPublicationEvidenceFromDto,
    retainedPublicationEvidenceToDto,
    placementSnapshotFromDto,
    placementSnapshotToDto,
    alignmentPlanIdFromDto,
    alignmentPlanIdToDto,
    alignmentControlFromDto,
    alignmentControlToDto,
    alignmentRetainedEvidenceFromDto,
    alignmentRetainedEvidenceToDto,
    structuralConsequenceCauseFromDto,
    structuralConsequenceCauseToDto,
    physicalPlacementRevisionVectorFromDto,
    physicalPlacementRevisionVectorToDto,
    terminalSourceUnionContainsOccurrenceClaimFromDto,
  )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Disappearance qualified as DomainDisappearance
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Sort.Descriptor qualified as Descriptor
import Eclips.Domain.Startup qualified as Startup
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence qualified as StructuralConsequence
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.Alignment.Plan.Identity qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as Alignment
import Eclips.Herald.Disappearance.Protocol qualified as Disappearance
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Graph.Protocol qualified as GraphProtocol
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.Input qualified as Input
import Eclips.Herald.Peer.Step15 qualified as PeerStep15
import Eclips.Herald.PeerDispatch
  ( PeerLogicalAttempt,
    PeerLogicalItem,
  )
import Eclips.Herald.PeerDispatch.Internal qualified as PeerDispatch
import Eclips.Herald.PeerPayload qualified as Payload
import Eclips.Herald.PeerPublication qualified as Publication
import Eclips.Herald.PeerStream qualified as Stream
import Eclips.Herald.Placement qualified as Placement
import Eclips.Herald.ProcessPreparation.Protocol qualified as Preparation
import Eclips.Herald.Structural.Debt qualified as Debt
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Peer.Types qualified as Protocol

-- | Failure categories shared by inbound admission and checked-owner
-- projection.  The public facade maps them to the direction-appropriate error
-- vocabulary.
data PeerRpcBridgeError
  = PeerRpcBridgeClaimContradiction
  | PeerRpcBridgeCanonicalValueContradiction
  | PeerRpcBridgePublicationDigestMismatch
  | PeerRpcBridgeDisappearanceMarkerCoordinateMismatch
  | PeerRpcBridgeOwnerPublicationContradiction
  deriving stock (Eq, Show)

helloFromDto ::
  Protocol.PeerHelloDto ->
  Either
    PeerRpcBridgeError
    ( Discovery.PeerHello,
      Membership.HeraldMembershipGenerationId,
      Topology.MemberSetDigest
    )
helloFromDto dto = do
  system <- systemFromClaim (Protocol.peerHelloSystem dto)
  heraldId <- heraldIdFromClaim (Protocol.peerHelloHeraldId dto)
  epoch <- heraldEpochFromClaim (Protocol.peerHelloHeraldEpoch dto)
  catalogue <- catalogueDigestFromClaim (Protocol.peerHelloCatalogueDigest dto)
  projection <-
    initialProjectionDigestFromClaim
      (Protocol.peerHelloInitialProjectionDigest dto)
  generation <-
    membershipGenerationFromClaim
      (Protocol.peerHelloMembershipGeneration dto)
  members <-
    memberSetDigestFromClaim
      (Protocol.peerHelloActiveMemberSetDigest dto)
  pure
    ( Discovery.peerHello
        system
        heraldId
        epoch
        (connectionNonceFromDto (Protocol.peerHelloConnectionNonce dto))
        ( Set.fromList
            (fmap addressFromDto (Protocol.peerHelloAdvertisedAddresses dto))
        )
        (controlIndexFromDto (Protocol.peerHelloAppliedControlIndex dto))
        catalogue
        projection
        (Protocol.peerHelloApplicationLocator dto),
      generation,
      members
    )

helloToDto ::
  Membership.HeraldMembershipGenerationId ->
  Topology.MemberSetDigest ->
  Discovery.PeerHello ->
  Either PeerRpcBridgeError Protocol.PeerHelloDto
helloToDto generation members hello = do
  system <- systemToClaim (Discovery.peerHelloSystemId hello)
  heraldId <- heraldIdToClaim (Discovery.peerHelloHeraldId hello)
  epoch <- heraldEpochToClaim (Discovery.peerHelloHeraldEpoch hello)
  catalogue <- catalogueDigestToClaim (Discovery.peerHelloCatalogueDigest hello)
  projection <-
    initialProjectionDigestToClaim
      (Discovery.peerHelloInitialProjectionDigest hello)
  generationClaim <- membershipGenerationToClaim generation
  memberSetClaim <- memberSetDigestToClaim members
  pure
    ( Protocol.peerHelloDto
        system
        heraldId
        epoch
        (connectionNonceToDto (Discovery.peerHelloConnectionNonce hello))
        ( fmap
            addressToDto
            (Set.toAscList (Discovery.peerHelloAdvertisedAddresses hello))
        )
        (controlIndexToDto (Discovery.peerHelloAppliedControlIndex hello))
        generationClaim
        memberSetClaim
        catalogue
        projection
        (Discovery.peerHelloApplicationLocator hello)
    )

controlFromDto ::
  Protocol.PeerControlDto ->
  Either PeerRpcBridgeError Input.PeerControl
controlFromDto = \case
  Protocol.KnownHeraldsDto collection ->
    Input.PeerKnownHeralds
      <$> traverse
        knownHeraldFromDto
        (Protocol.knownHeraldCollectionEntries collection)
  Protocol.PlacementUpdateDto update ->
    Input.PeerPlacementUpdate <$> placementUpdateFromDto update
  Protocol.PlacementAcknowledgedDto acknowledgement ->
    Input.PeerPlacementAcknowledged <$> placementAcknowledgementFromDto acknowledgement
  Protocol.StreamReceivedDto direction prefix ->
    Input.PeerStreamReceived
      <$> streamDirectionFromDto direction
      <*> streamPrefixFromDto prefix
  Protocol.StreamCompletedDto direction progress ->
    Input.PeerStreamCompleted
      <$> streamDirectionFromDto direction
      <*> bridgeClaim (Stream.checkStreamCompletion progress)
  Protocol.StreamFrontierAdvancedDto direction nextSequence ->
    Input.PeerStreamFrontierAdvanced
      <$> streamDirectionFromDto direction
      <*> streamSequenceFromDto nextSequence
  Protocol.StreamResumeOfferedDto offer ->
    Input.PeerStreamResumeOffered <$> resumeOfferFromDto offer
  Protocol.StreamResumeAcceptedDto response ->
    Input.PeerStreamResumeAccepted <$> resumeResponseFromDto response
  Protocol.StructuralAppliedReportedDto report ->
    Input.PeerStructuralAppliedReported <$> structuralAppliedReportFromDto report
  Protocol.TopologyCutAnnouncedDto announce ->
    Input.PeerTopologyCutAnnounced <$> topologyCutAnnounceFromDto announce
  Protocol.TopologyCutAcceptedDto acceptance ->
    Input.PeerTopologyCutAccepted <$> topologyCutAcceptanceFromDto acceptance
  Protocol.TopologyCutEstablishedControlDto established ->
    Input.PeerTopologyCutEstablished <$> topologyCutEstablishedFromDto established
  Protocol.TopologyCutEstablishedAcknowledgedDto acknowledgement ->
    Input.PeerTopologyCutEstablishedAcknowledged
      <$> topologyCutEstablishedAckFromDto acknowledgement
  Protocol.PreparationControlDto preparation ->
    Input.PeerPreparationControl <$> preparationFromDto preparation
  Protocol.AlignmentControlDto alignment ->
    Input.PeerAlignmentControl <$> alignmentControlFromDto alignment
  Protocol.AlignmentEvidenceDeliveredDto sequenceDto evidence ->
    Input.PeerAlignmentEvidenceDelivered
      <$> bridgeClaim (DomainAlignment.alignmentDeliverySequence (Protocol.positiveAlignmentDeliverySequenceDtoWord64 sequenceDto))
      <*> alignmentRetainedEvidenceFromDto evidence
  Protocol.AlignmentDeliveryProgressDto -> pure Input.PeerAlignmentDeliveryProgress
  Protocol.DirectFailureProbeRequestedDto request ->
    Input.PeerDirectFailureProbeRequested <$> directFailureProbeRequestFromDto request
  Protocol.DirectFailureProbeRespondedDto response ->
    Input.PeerDirectFailureProbeResponded <$> directFailureProbeResponseFromDto response
  Protocol.TerminalSourceInventoryAdvertisedDto inventory ->
    Input.PeerTerminalSourceInventoryAdvertised <$> terminalSourceInventoryFromDto inventory
  Protocol.TerminalSourcePayloadRequestedDto request ->
    Input.PeerTerminalSourcePayloadRequested <$> terminalSourcePayloadRequestFromDto request
  Protocol.TerminalSourcePayloadRelayedDto relay ->
    Input.PeerTerminalSourcePayloadRelayed <$> terminalSourcePayloadRelayFromDto relay
  Protocol.TerminalSourceUnionAnnouncedDto announce ->
    Input.PeerTerminalSourceUnionAnnounced <$> terminalSourceUnionAnnounceFromDto announce
  Protocol.TerminalSourceUnionAcceptedDto acceptance ->
    Input.PeerTerminalSourceUnionAccepted <$> terminalSourceUnionAcceptanceFromDto acceptance
  Protocol.TerminalSourceUnionEstablishedControlDto established ->
    Input.PeerTerminalSourceUnionEstablished <$> terminalSourceUnionEstablishedFromDto established
  Protocol.LabelInstalledDto report ->
    Input.PeerLabelInstalled <$> labelInstallationReportFromDto report

controlToDto ::
  Input.PeerControl ->
  Either PeerRpcBridgeError Protocol.PeerControlDto
controlToDto = \case
  Input.PeerKnownHeralds known -> do
    entries <- traverse knownHeraldToDto known
    collection <- bridgeClaim (Protocol.knownHeraldCollectionDto entries)
    pure (Protocol.KnownHeraldsDto collection)
  Input.PeerPlacementUpdate update ->
    Protocol.PlacementUpdateDto <$> placementUpdateToDto update
  Input.PeerPlacementAcknowledged acknowledgement ->
    Protocol.PlacementAcknowledgedDto <$> placementAcknowledgementToDto acknowledgement
  Input.PeerStreamReceived direction prefix ->
    Protocol.StreamReceivedDto
      <$> streamDirectionToDto direction
      <*> streamPrefixToDto prefix
  Input.PeerStreamCompleted direction progress ->
    Protocol.StreamCompletedDto
      <$> streamDirectionToDto direction
      <*> bridgeClaim (Stream.checkStreamCompletion progress)
  Input.PeerStreamFrontierAdvanced direction nextSequence ->
    Protocol.StreamFrontierAdvancedDto
      <$> streamDirectionToDto direction
      <*> streamSequenceToDto nextSequence
  Input.PeerStreamResumeOffered offer ->
    Protocol.StreamResumeOfferedDto <$> resumeOfferToDto offer
  Input.PeerStreamResumeAccepted response ->
    Protocol.StreamResumeAcceptedDto <$> resumeResponseToDto response
  Input.PeerStructuralAppliedReported report ->
    Protocol.StructuralAppliedReportedDto <$> structuralAppliedReportToDto report
  Input.PeerTopologyCutAnnounced announce ->
    Protocol.TopologyCutAnnouncedDto <$> topologyCutAnnounceToDto announce
  Input.PeerTopologyCutAccepted acceptance ->
    Protocol.TopologyCutAcceptedDto <$> topologyCutAcceptanceToDto acceptance
  Input.PeerTopologyCutEstablished established ->
    Protocol.TopologyCutEstablishedControlDto
      <$> topologyCutEstablishedToDto established
  Input.PeerTopologyCutEstablishedAcknowledged acknowledgement ->
    Protocol.TopologyCutEstablishedAcknowledgedDto
      <$> topologyCutEstablishedAckToDto acknowledgement
  Input.PeerPreparationControl preparation ->
    Protocol.PreparationControlDto <$> preparationToDto preparation
  Input.PeerAlignmentControl alignment ->
    Protocol.AlignmentControlDto <$> alignmentControlToDto alignment
  Input.PeerAlignmentEvidenceDelivered sequenceNumber evidence ->
    Protocol.AlignmentEvidenceDeliveredDto
      <$> bridgeClaim (Protocol.positiveAlignmentDeliverySequenceDto (DomainAlignment.alignmentDeliverySequenceWord64 sequenceNumber))
      <*> alignmentRetainedEvidenceToDto evidence
  Input.PeerAlignmentDeliveryProgress -> pure Protocol.AlignmentDeliveryProgressDto
  Input.PeerDirectFailureProbeRequested request ->
    Protocol.DirectFailureProbeRequestedDto <$> directFailureProbeRequestToDto request
  Input.PeerDirectFailureProbeResponded response ->
    Protocol.DirectFailureProbeRespondedDto <$> directFailureProbeResponseToDto response
  Input.PeerTerminalSourceInventoryAdvertised inventory ->
    Protocol.TerminalSourceInventoryAdvertisedDto <$> terminalSourceInventoryToDto inventory
  Input.PeerTerminalSourcePayloadRequested request ->
    Protocol.TerminalSourcePayloadRequestedDto <$> terminalSourcePayloadRequestToDto request
  Input.PeerTerminalSourcePayloadRelayed relay ->
    Protocol.TerminalSourcePayloadRelayedDto <$> terminalSourcePayloadRelayToDto relay
  Input.PeerTerminalSourceUnionAnnounced announce ->
    Protocol.TerminalSourceUnionAnnouncedDto <$> terminalSourceUnionAnnounceToDto announce
  Input.PeerTerminalSourceUnionAccepted acceptance ->
    Protocol.TerminalSourceUnionAcceptedDto <$> terminalSourceUnionAcceptanceToDto acceptance
  Input.PeerTerminalSourceUnionEstablished established ->
    Protocol.TerminalSourceUnionEstablishedControlDto <$> terminalSourceUnionEstablishedToDto established
  Input.PeerLabelInstalled report ->
    Protocol.LabelInstalledDto <$> labelInstallationReportToDto report

labelInstallationReportFromDto ::
  Protocol.LabelInstallationReportDto ->
  Either PeerRpcBridgeError Label.LabelInstallationReport
labelInstallationReportFromDto report =
  Label.labelInstallationReport
    <$> bridgeClaim
      ( Identity.mkLabelDecisionId
          (Protocol.labelDecisionClaimBytes (Protocol.labelInstallationDecisionId report))
      )
    <*> heraldEpochFromClaim (Protocol.labelInstallationReporter report)
    <*> pure (controlIndexFromDto (Protocol.labelInstallationControlIndex report))
    <*> bridgeClaim
      ( Label.mkLabelOutcomeDigest
          (Protocol.labelOutcomeDigestClaimBytes (Protocol.labelInstallationOutcomeDigest report))
      )

labelInstallationReportToDto ::
  Label.LabelInstallationReport ->
  Either PeerRpcBridgeError Protocol.LabelInstallationReportDto
labelInstallationReportToDto report = do
  decision <-
    bridgeClaim
      (Protocol.labelDecisionClaim (Identity.labelDecisionIdBytes (Label.labelInstallationDecisionId report)))
  reporter <- heraldEpochToClaim (Label.labelInstallationReporter report)
  digest <-
    bridgeClaim
      (Protocol.labelOutcomeDigestClaim (Label.labelOutcomeDigestBytes (Label.labelInstallationOutcomeDigest report)))
  bridgeClaim
    ( Protocol.labelInstallationReportDto
        decision
        reporter
        (controlIndexToDto (Label.labelInstallationControlIndex report))
        digest
    )

directFailureProbeRequestFromDto ::
  Protocol.DirectFailureProbeRequestDto ->
  Either PeerRpcBridgeError PeerStep15.DirectFailureProbeRequest
directFailureProbeRequestFromDto
  (Protocol.DirectFailureProbeRequestDto probeDto targetClaim generationClaim configurationClaim) = do
    probe <- failureProbeFromDto probeDto
    target <- heraldEpochFromClaim targetClaim
    generation <- membershipGenerationFromClaim generationClaim
    configuration <- bridgeClaim (Voter.mkVoterConfigurationId (Protocol.oracleVoterConfigurationClaimBytes configurationClaim))
    pure (PeerStep15.directFailureProbeRequest probe target generation configuration)

directFailureProbeRequestToDto ::
  PeerStep15.DirectFailureProbeRequest ->
  Either PeerRpcBridgeError Protocol.DirectFailureProbeRequestDto
directFailureProbeRequestToDto request =
  Protocol.DirectFailureProbeRequestDto
    <$> failureProbeToDto (PeerStep15.directFailureProbeRequestProbe request)
    <*> heraldEpochToClaim (PeerStep15.directFailureProbeRequestTarget request)
    <*> membershipGenerationToClaim
      (PeerStep15.directFailureProbeRequestGeneration request)
    <*> bridgeClaim
      (Protocol.oracleVoterConfigurationClaim (Voter.voterConfigurationIdBytes (PeerStep15.directFailureProbeRequestVoterConfiguration request)))

directFailureProbeResponseFromDto ::
  Protocol.DirectFailureProbeResponseDto ->
  Either PeerRpcBridgeError PeerStep15.DirectFailureProbeResponse
directFailureProbeResponseFromDto
  (Protocol.DirectFailureProbeResponseDto requestDto resultDto) =
    PeerStep15.directFailureProbeResponse
      <$> directFailureProbeRequestFromDto requestDto
      <*> pure (directFailureProbeResultFromDto resultDto)

directFailureProbeResponseToDto ::
  PeerStep15.DirectFailureProbeResponse ->
  Either PeerRpcBridgeError Protocol.DirectFailureProbeResponseDto
directFailureProbeResponseToDto response =
  Protocol.DirectFailureProbeResponseDto
    <$> directFailureProbeRequestToDto
      (PeerStep15.directFailureProbeResponseRequest response)
    <*> pure
      ( directFailureProbeResultToDto
          (PeerStep15.directFailureProbeResponseResult response)
      )

directFailureProbeResultFromDto ::
  Protocol.DirectFailureProbeResultDto ->
  PeerStep15.DirectFailureProbeResult
directFailureProbeResultFromDto = \case
  Protocol.DirectFailureProbeReachableDto -> PeerStep15.DirectFailureProbeReachable
  Protocol.DirectFailureProbeUnreachableDto -> PeerStep15.DirectFailureProbeUnreachable

directFailureProbeResultToDto ::
  PeerStep15.DirectFailureProbeResult ->
  Protocol.DirectFailureProbeResultDto
directFailureProbeResultToDto = \case
  PeerStep15.DirectFailureProbeReachable -> Protocol.DirectFailureProbeReachableDto
  PeerStep15.DirectFailureProbeUnreachable -> Protocol.DirectFailureProbeUnreachableDto

failureProbeFromDto ::
  Protocol.HeraldFailureProbeIdDto ->
  Either PeerRpcBridgeError Membership.HeraldFailureProbeId
failureProbeFromDto dto = do
  let index = controlIndexFromDto (Protocol.heraldFailureProbeIdControlIndex dto)
  probe <- bridgeClaim (Membership.deriveHeraldFailureProbeId index)
  if Membership.heraldFailureProbeIdBytes probe
    == Protocol.heraldFailureProbeDigestClaimBytes
      (Protocol.heraldFailureProbeIdDigest dto)
    then Right probe
    else Left PeerRpcBridgeClaimContradiction

failureProbeToDto ::
  Membership.HeraldFailureProbeId ->
  Either PeerRpcBridgeError Protocol.HeraldFailureProbeIdDto
failureProbeToDto probe = do
  digest <-
    bridgeClaim
      ( Protocol.heraldFailureProbeDigestClaim
          (Membership.heraldFailureProbeIdBytes probe)
      )
  bridgeOwnerPublication
    ( Protocol.heraldFailureProbeIdDto
        digest
        (controlIndexToDto (Membership.heraldFailureProbeControlIndex probe))
    )

terminalSourceInventoryFromDto ::
  Protocol.TerminalSourceInventoryDto ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourceInventory
terminalSourceInventoryFromDto
  ( Protocol.TerminalSourceInventoryDto
      predecessorClaim
      successorClaim
      retiredClaim
      reporterClaim
      prefixDto
      digestClaim
      canonicalBytes
    ) = do
    predecessor <- membershipGenerationFromClaim predecessorClaim
    successor <- membershipGenerationFromClaim successorClaim
    retired <- heraldEpochFromClaim retiredClaim
    reporter <- heraldEpochFromClaim reporterClaim
    prefix <- structuralPrefixFromDto prefixDto
    digest <- terminalSourceInventoryDigestFromClaim digestClaim
    inventory <-
      bridgeCanonicalValue
        (TerminalSource.decodeTerminalSourceInventoryCanonicalBytes canonicalBytes)
    requireBridgeClaim
      ( predecessor
          == TerminalSource.terminalSourceInventoryPredecessorGenerationId inventory
          && successor
            == TerminalSource.terminalSourceInventorySuccessorGenerationId inventory
          && retired == TerminalSource.terminalSourceInventoryRetiredSource inventory
          && reporter == TerminalSource.terminalSourceInventoryReporter inventory
          && prefix
            == TerminalSource.terminalSourceInventoryContiguousPrefix inventory
          && digest == TerminalSource.terminalSourceInventoryDigest inventory
      )
    pure inventory

terminalSourceInventoryToDto ::
  TerminalSource.TerminalSourceInventory ->
  Either PeerRpcBridgeError Protocol.TerminalSourceInventoryDto
terminalSourceInventoryToDto inventory =
  Protocol.TerminalSourceInventoryDto
    <$> membershipGenerationToClaim
      (TerminalSource.terminalSourceInventoryPredecessorGenerationId inventory)
    <*> membershipGenerationToClaim
      (TerminalSource.terminalSourceInventorySuccessorGenerationId inventory)
    <*> heraldEpochToClaim
      (TerminalSource.terminalSourceInventoryRetiredSource inventory)
    <*> heraldEpochToClaim
      (TerminalSource.terminalSourceInventoryReporter inventory)
    <*> structuralPrefixToDto
      (TerminalSource.terminalSourceInventoryContiguousPrefix inventory)
    <*> terminalSourceInventoryDigestToClaim
      (TerminalSource.terminalSourceInventoryDigest inventory)
    <*> pure (TerminalSource.terminalSourceInventoryCanonicalBytes inventory)

terminalSourcePayloadRequestFromDto ::
  Protocol.TerminalSourcePayloadRequestDto ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourcePayloadRequest
terminalSourcePayloadRequestFromDto
  ( Protocol.TerminalSourcePayloadRequestDto
      predecessorClaim
      successorClaim
      retiredClaim
      occurrenceDto
      inventoryClaim
    ) = do
    predecessor <- membershipGenerationFromClaim predecessorClaim
    successor <- membershipGenerationFromClaim successorClaim
    retired <- heraldEpochFromClaim retiredClaim
    occurrence <- structuralOccurrenceIdFromDto occurrenceDto
    inventory <- terminalSourceInventoryDigestFromClaim inventoryClaim
    bridgeClaim
      ( TerminalSource.terminalSourcePayloadRequestFromClaimedCoordinates
          predecessor
          successor
          retired
          occurrence
          inventory
      )

terminalSourcePayloadRequestToDto ::
  TerminalSource.TerminalSourcePayloadRequest ->
  Either PeerRpcBridgeError Protocol.TerminalSourcePayloadRequestDto
terminalSourcePayloadRequestToDto request =
  Protocol.TerminalSourcePayloadRequestDto
    <$> membershipGenerationToClaim
      (TerminalSource.terminalSourcePayloadRequestPredecessorGenerationId request)
    <*> membershipGenerationToClaim
      (TerminalSource.terminalSourcePayloadRequestSuccessorGenerationId request)
    <*> heraldEpochToClaim
      (TerminalSource.terminalSourcePayloadRequestRetiredSource request)
    <*> structuralOccurrenceIdToDto
      (TerminalSource.terminalSourcePayloadRequestOccurrence request)
    <*> terminalSourceInventoryDigestToClaim
      (TerminalSource.terminalSourcePayloadRequestSupportingInventory request)

terminalSourcePayloadRelayFromDto ::
  Protocol.TerminalSourcePayloadRelayDto ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourcePayloadRelay
terminalSourcePayloadRelayFromDto
  ( Protocol.TerminalSourcePayloadRelayDto
      predecessorClaim
      successorClaim
      retiredClaim
      relayClaim
      inventoryClaim
      occurrenceDto
      publicationDigestClaim
      canonicalBytes
    ) = do
    predecessor <- membershipGenerationFromClaim predecessorClaim
    successor <- membershipGenerationFromClaim successorClaim
    retired <- heraldEpochFromClaim retiredClaim
    relayHerald <- heraldEpochFromClaim relayClaim
    inventory <- terminalSourceInventoryDigestFromClaim inventoryClaim
    occurrence <- structuralOccurrenceIdFromDto occurrenceDto
    publicationDigest <-
      structuralPublicationDigestFromClaim publicationDigestClaim
    relay <-
      bridgeCanonicalValue
        (TerminalSource.decodeTerminalSourcePayloadRelayCanonicalBytes canonicalBytes)
    let relayedOccurrence = TerminalSource.terminalSourcePayloadRelayOccurrence relay
    requireBridgeClaim
      ( predecessor
          == TerminalSource.terminalSourcePayloadRelayPredecessorGenerationId relay
          && successor
            == TerminalSource.terminalSourcePayloadRelaySuccessorGenerationId relay
          && retired == TerminalSource.terminalSourcePayloadRelayRetiredSource relay
          && relayHerald == TerminalSource.terminalSourcePayloadRelayHerald relay
          && inventory
            == TerminalSource.terminalSourcePayloadRelaySupportingInventory relay
          && occurrence
            == TerminalSource.terminalStructuralOccurrenceId relayedOccurrence
          && publicationDigest
            == TerminalSource.terminalStructuralOccurrencePublicationDigest relayedOccurrence
      )
    pure relay

terminalSourcePayloadRelayToDto ::
  TerminalSource.TerminalSourcePayloadRelay ->
  Either PeerRpcBridgeError Protocol.TerminalSourcePayloadRelayDto
terminalSourcePayloadRelayToDto relay =
  let occurrence = TerminalSource.terminalSourcePayloadRelayOccurrence relay
   in Protocol.TerminalSourcePayloadRelayDto
        <$> membershipGenerationToClaim
          (TerminalSource.terminalSourcePayloadRelayPredecessorGenerationId relay)
        <*> membershipGenerationToClaim
          (TerminalSource.terminalSourcePayloadRelaySuccessorGenerationId relay)
        <*> heraldEpochToClaim
          (TerminalSource.terminalSourcePayloadRelayRetiredSource relay)
        <*> heraldEpochToClaim
          (TerminalSource.terminalSourcePayloadRelayHerald relay)
        <*> terminalSourceInventoryDigestToClaim
          (TerminalSource.terminalSourcePayloadRelaySupportingInventory relay)
        <*> structuralOccurrenceIdToDto
          (TerminalSource.terminalStructuralOccurrenceId occurrence)
        <*> structuralPublicationDigestToClaim
          (TerminalSource.terminalStructuralOccurrencePublicationDigest occurrence)
        <*> pure (TerminalSource.terminalSourcePayloadRelayCanonicalBytes relay)

terminalSourceUnionFromDto ::
  Protocol.TerminalSourceUnionDto ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourceUnion
terminalSourceUnionFromDto
  ( Protocol.TerminalSourceUnionDto
      predecessorClaim
      successorClaim
      retiredClaim
      digestClaim
      canonicalBytes
    ) = do
    predecessor <- membershipGenerationFromClaim predecessorClaim
    successor <- membershipGenerationFromClaim successorClaim
    retired <- retiredHeraldsFromDto retiredClaim
    digest <- terminalSourceUnionDigestFromClaim digestClaim
    union <-
      bridgeCanonicalValue
        (TerminalSource.decodeTerminalSourceUnionCanonicalBytes canonicalBytes)
    requireBridgeClaim
      ( predecessor
          == TerminalSource.terminalSourceUnionPredecessorGenerationId union
          && successor
            == TerminalSource.terminalSourceUnionSuccessorGenerationId union
          && retired == TerminalSource.terminalSourceUnionRetiredSources union
          && digest == TerminalSource.terminalSourceUnionDigest union
      )
    pure union

terminalSourceUnionContainsOccurrenceClaimFromDto ::
  Protocol.TerminalSourceUnionDto ->
  Protocol.StructuralOccurrenceIdDto ->
  Protocol.StructuralPublicationDigestClaim ->
  Either PeerRpcBridgeError Bool
terminalSourceUnionContainsOccurrenceClaimFromDto
  unionDto
  occurrenceDto
  publicationDigestClaim = do
    union <- terminalSourceUnionFromDto unionDto
    occurrence <- structuralOccurrenceIdFromDto occurrenceDto
    publicationDigest <-
      structuralPublicationDigestFromClaim publicationDigestClaim
    let claim =
          TerminalSource.terminalSourceOccurrenceClaim
            occurrence
            publicationDigest
        retainedClaims =
          TerminalSource.terminalSourceUnionIncludedClaims union
            <> TerminalSource.terminalSourceUnionAheadClaims union
    pure (claim `elem` retainedClaims)

terminalSourceUnionToDto ::
  TerminalSource.TerminalSourceUnion ->
  Either PeerRpcBridgeError Protocol.TerminalSourceUnionDto
terminalSourceUnionToDto union =
  Protocol.TerminalSourceUnionDto
    <$> membershipGenerationToClaim
      (TerminalSource.terminalSourceUnionPredecessorGenerationId union)
    <*> membershipGenerationToClaim
      (TerminalSource.terminalSourceUnionSuccessorGenerationId union)
    <*> retiredHeraldsToDto (TerminalSource.terminalSourceUnionRetiredSources union)
    <*> terminalSourceUnionDigestToClaim
      (TerminalSource.terminalSourceUnionDigest union)
    <*> pure (TerminalSource.terminalSourceUnionCanonicalBytes union)

terminalSourceUnionAnnounceFromDto ::
  Protocol.TerminalSourceUnionAnnounceDto ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourceUnionAnnounce
terminalSourceUnionAnnounceFromDto
  (Protocol.TerminalSourceUnionAnnounceDto announcerClaim unionDto) = do
    announcer <- heraldEpochFromClaim announcerClaim
    union <- terminalSourceUnionFromDto unionDto
    bridgeClaim
      (TerminalSource.terminalSourceUnionAnnounceFromClaimedCoordinates announcer union)

terminalSourceUnionAnnounceToDto ::
  TerminalSource.TerminalSourceUnionAnnounce ->
  Either PeerRpcBridgeError Protocol.TerminalSourceUnionAnnounceDto
terminalSourceUnionAnnounceToDto announce =
  Protocol.TerminalSourceUnionAnnounceDto
    <$> heraldEpochToClaim
      (TerminalSource.terminalSourceUnionAnnounceAnnouncer announce)
    <*> terminalSourceUnionToDto
      (TerminalSource.terminalSourceUnionAnnounceUnion announce)

terminalSourceUnionAcceptanceFromDto ::
  Protocol.TerminalSourceUnionAcceptanceDto ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourceUnionAcceptance
terminalSourceUnionAcceptanceFromDto
  ( Protocol.TerminalSourceUnionAcceptanceDto
      predecessorClaim
      successorClaim
      retiredClaim
      unionDigestClaim
      reporterClaim
      acceptanceDigestClaim
    ) = do
    predecessor <- membershipGenerationFromClaim predecessorClaim
    successor <- membershipGenerationFromClaim successorClaim
    retired <- retiredHeraldsFromDto retiredClaim
    unionDigest <- terminalSourceUnionDigestFromClaim unionDigestClaim
    reporter <- heraldEpochFromClaim reporterClaim
    acceptanceDigest <-
      terminalSourceAcceptanceDigestFromClaim acceptanceDigestClaim
    bridgeClaim
      ( TerminalSource.terminalSourceUnionAcceptanceFromClaimedCoordinates
          predecessor
          successor
          retired
          unionDigest
          reporter
          acceptanceDigest
      )

terminalSourceUnionAcceptanceToDto ::
  TerminalSource.TerminalSourceUnionAcceptance ->
  Either PeerRpcBridgeError Protocol.TerminalSourceUnionAcceptanceDto
terminalSourceUnionAcceptanceToDto acceptance =
  Protocol.TerminalSourceUnionAcceptanceDto
    <$> membershipGenerationToClaim
      (TerminalSource.terminalSourceUnionAcceptancePredecessorGenerationId acceptance)
    <*> membershipGenerationToClaim
      (TerminalSource.terminalSourceUnionAcceptanceSuccessorGenerationId acceptance)
    <*> retiredHeraldsToDto
      (TerminalSource.terminalSourceUnionAcceptanceRetiredSources acceptance)
    <*> terminalSourceUnionDigestToClaim
      (TerminalSource.terminalSourceUnionAcceptanceUnionDigest acceptance)
    <*> heraldEpochToClaim
      (TerminalSource.terminalSourceUnionAcceptanceReporter acceptance)
    <*> terminalSourceAcceptanceDigestToClaim
      (TerminalSource.terminalSourceUnionAcceptanceDigest acceptance)

terminalSourceUnionEstablishedFromDto ::
  Protocol.TerminalSourceUnionEstablishedDto ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourceUnionEstablished
terminalSourceUnionEstablishedFromDto
  (Protocol.TerminalSourceUnionEstablishedDto unionDto acceptancesDto) = do
    union <- terminalSourceUnionFromDto unionDto
    acceptances <-
      traverse
        terminalSourceUnionAcceptanceFromDto
        ( NonEmpty.toList
            (Protocol.terminalSourceUnionAcceptanceEntries acceptancesDto)
        )
    bridgeClaim
      ( TerminalSource.terminalSourceUnionEstablishedFromClaimedCoordinates
          union
          acceptances
      )

terminalSourceUnionEstablishedToDto ::
  TerminalSource.TerminalSourceUnionEstablished ->
  Either PeerRpcBridgeError Protocol.TerminalSourceUnionEstablishedDto
terminalSourceUnionEstablishedToDto established = do
  unionDto <-
    terminalSourceUnionToDto
      (TerminalSource.terminalSourceUnionEstablishedUnion established)
  acceptances <-
    traverse
      terminalSourceUnionAcceptanceToDto
      (TerminalSource.terminalSourceUnionEstablishedAcceptances established)
  checkedAcceptances <-
    bridgeOwnerPublication
      (Protocol.terminalSourceUnionAcceptancesDto acceptances)
  pure (Protocol.TerminalSourceUnionEstablishedDto unionDto checkedAcceptances)

retiredHeraldsFromDto :: Protocol.RetiredHeraldsDto -> Either PeerRpcBridgeError (Set.Set Identity.HeraldEpoch)
retiredHeraldsFromDto = fmap Set.fromList . traverse heraldEpochFromClaim . NonEmpty.toList . Protocol.retiredHeraldEntries

retiredHeraldsToDto :: Set.Set Identity.HeraldEpoch -> Either PeerRpcBridgeError Protocol.RetiredHeraldsDto
retiredHeraldsToDto retired = do
  claims <- traverse heraldEpochToClaim (Set.toAscList retired)
  bridgeOwnerPublication (Protocol.retiredHeraldsDto claims)

requireBridgeClaim :: Bool -> Either PeerRpcBridgeError ()
requireBridgeClaim condition =
  unless condition (Left PeerRpcBridgeClaimContradiction)

bridgeCanonicalValue ::
  Either error value -> Either PeerRpcBridgeError value
bridgeCanonicalValue =
  either (const (Left PeerRpcBridgeCanonicalValueContradiction)) Right

topologyFrontierFromDto ::
  Protocol.TopologyFrontierDto ->
  Either PeerRpcBridgeError Topology.TopologyFrontier
topologyFrontierFromDto
  (Protocol.TopologyFrontierDto vectorDto controlDto) =
    Topology.topologyFrontier
      <$> structuralVersionVectorFromDto vectorDto
      <*> pure (controlIndexFromDto controlDto)

topologyFrontierToDto ::
  Topology.TopologyFrontier ->
  Either PeerRpcBridgeError Protocol.TopologyFrontierDto
topologyFrontierToDto frontier =
  Protocol.TopologyFrontierDto
    <$> structuralVersionVectorToDto
      (Topology.topologyFrontierStructuralVersionVector frontier)
    <*> pure
      ( controlIndexToDto
          (Topology.topologyFrontierAppliedControlPrefix frontier)
      )

topologyCutFromDto ::
  Protocol.TopologyCutDto ->
  Either PeerRpcBridgeError Topology.TopologyCut
topologyCutFromDto
  (Protocol.TopologyCutDto predecessorDto frontierDto occurrenceClaim) = do
    predecessor <- topologyPredecessorFromDto predecessorDto
    frontier <- topologyFrontierFromDto frontierDto
    occurrence <- topologyOccurrenceDigestFromClaim occurrenceClaim
    bridgeClaim (Topology.topologyCut predecessor frontier occurrence)

topologyCutToDto ::
  Topology.TopologyCut ->
  Either PeerRpcBridgeError Protocol.TopologyCutDto
topologyCutToDto cut =
  Protocol.TopologyCutDto
    <$> topologyPredecessorToDto (Topology.topologyCutPredecessor cut)
    <*> topologyFrontierToDto (Topology.topologyCutFrontier cut)
    <*> topologyOccurrenceDigestToClaim
      (Topology.topologyCutOccurrenceDigest cut)

topologyPredecessorFromDto ::
  Protocol.TopologyPredecessorDto ->
  Either PeerRpcBridgeError Topology.TopologyPredecessor
topologyPredecessorFromDto = \case
  Protocol.SameGenerationPredecessorDto cutClaim ->
    Topology.sameGenerationPredecessor <$> topologyCutFromClaim cutClaim
  Protocol.MembershipSuccessorPredecessorDto
    cutClaim
    predecessorGenerationClaim
    successorGenerationClaim
    terminalVectorDto
    successorInitialVectorDto
    unionDigestClaim -> do
      cut <- topologyCutFromClaim cutClaim
      predecessorGeneration <-
        membershipGenerationFromClaim predecessorGenerationClaim
      successorGeneration <-
        membershipGenerationFromClaim successorGenerationClaim
      terminalVector <- structuralVersionVectorFromDto terminalVectorDto
      successorInitialVector <-
        structuralVersionVectorFromDto successorInitialVectorDto
      unionDigest <- terminalSourceUnionDigestFromClaim unionDigestClaim
      bridgeClaim
        ( Topology.topologyPredecessorFromClaimedCoordinates
            cut
            predecessorGeneration
            successorGeneration
            terminalVector
            successorInitialVector
            unionDigest
        )
  Protocol.AdmissionTopologyPredecessorDto cut old new terminal initial admission recipe -> do
    cutId <- topologyCutFromClaim cut
    oldId <- membershipGenerationFromClaim old
    newId <- membershipGenerationFromClaim new
    terminalVector <- structuralVersionVectorFromDto terminal
    initialVector <- structuralVersionVectorFromDto initial
    admissionId <- bridgeClaim (Membership.decodeHeraldAdmissionIdCanonicalBytes admission)
    recipeDigest <- bridgeClaim (Topology.mkHeraldJoinBaseRecipeDigest recipe)
    bridgeClaim (Topology.admissionTopologyPredecessorFromClaimedCoordinates cutId oldId newId terminalVector initialVector admissionId recipeDigest)

topologyPredecessorToDto ::
  Topology.TopologyPredecessor ->
  Either PeerRpcBridgeError Protocol.TopologyPredecessorDto
topologyPredecessorToDto predecessor =
  case Topology.topologyPredecessorView predecessor of
    Topology.SameGenerationPredecessorView cut ->
      Protocol.SameGenerationPredecessorDto <$> topologyCutToClaim cut
    Topology.MembershipSuccessorPredecessorView
      cut
      predecessorGeneration
      successorGeneration
      terminalVector
      successorInitialVector
      unionDigest ->
        Protocol.MembershipSuccessorPredecessorDto
          <$> topologyCutToClaim cut
          <*> membershipGenerationToClaim predecessorGeneration
          <*> membershipGenerationToClaim successorGeneration
          <*> structuralVersionVectorToDto terminalVector
          <*> structuralVersionVectorToDto successorInitialVector
          <*> terminalSourceUnionDigestToClaim unionDigest
    Topology.AdmissionTopologyPredecessorView cut old new terminal initial admission recipe ->
      Protocol.AdmissionTopologyPredecessorDto
        <$> topologyCutToClaim cut
        <*> membershipGenerationToClaim old
        <*> membershipGenerationToClaim new
        <*> structuralVersionVectorToDto terminal
        <*> structuralVersionVectorToDto initial
        <*> pure (Membership.heraldAdmissionIdCanonicalBytes admission)
        <*> pure (Topology.heraldJoinBaseRecipeDigestValueBytes recipe)

structuralAppliedReportFromDto ::
  Protocol.StructuralAppliedReportDto ->
  Either PeerRpcBridgeError GraphProtocol.StructuralAppliedReport
structuralAppliedReportFromDto
  (Protocol.StructuralAppliedReportDto reporterClaim vectorDto controlDto) =
    GraphProtocol.structuralAppliedReport
      <$> heraldEpochFromClaim reporterClaim
      <*> structuralVersionVectorFromDto vectorDto
      <*> pure (controlIndexFromDto controlDto)

structuralAppliedReportToDto ::
  GraphProtocol.StructuralAppliedReport ->
  Either PeerRpcBridgeError Protocol.StructuralAppliedReportDto
structuralAppliedReportToDto report =
  Protocol.StructuralAppliedReportDto
    <$> heraldEpochToClaim
      (GraphProtocol.structuralAppliedReportReporter report)
    <*> structuralVersionVectorToDto
      (GraphProtocol.structuralAppliedReportVersionVector report)
    <*> pure
      ( controlIndexToDto
          (GraphProtocol.structuralAppliedReportControlPrefix report)
      )

topologyCutAnnounceFromDto ::
  Protocol.TopologyCutAnnounceDto ->
  Either PeerRpcBridgeError GraphProtocol.TopologyCutAnnounce
topologyCutAnnounceFromDto
  (Protocol.TopologyCutAnnounceDto announcerClaim cutClaim cutDto) = do
    announcer <- heraldEpochFromClaim announcerClaim
    claimed <- topologyCutFromClaim cutClaim
    cut <- topologyCutFromDto cutDto
    bridgeClaim (GraphProtocol.checkTopologyCutAnnounce announcer claimed cut)

topologyCutAnnounceToDto ::
  GraphProtocol.TopologyCutAnnounce ->
  Either PeerRpcBridgeError Protocol.TopologyCutAnnounceDto
topologyCutAnnounceToDto announce =
  Protocol.TopologyCutAnnounceDto
    <$> heraldEpochToClaim (GraphProtocol.topologyCutAnnounceAnnouncer announce)
    <*> topologyCutToClaim (GraphProtocol.topologyCutAnnounceId announce)
    <*> topologyCutToDto (GraphProtocol.topologyCutAnnounceCut announce)

topologyCutAcceptanceFromDto ::
  Protocol.TopologyCutAcceptanceDto ->
  Either PeerRpcBridgeError GraphProtocol.TopologyCutAcceptance
topologyCutAcceptanceFromDto
  ( Protocol.TopologyCutAcceptanceDto
      cutClaim
      reporterClaim
      vectorDto
      controlDto
    ) =
    GraphProtocol.topologyCutAcceptance
      <$> topologyCutFromClaim cutClaim
      <*> ( GraphProtocol.structuralAppliedReport
              <$> heraldEpochFromClaim reporterClaim
              <*> structuralVersionVectorFromDto vectorDto
              <*> pure (controlIndexFromDto controlDto)
          )

topologyCutAcceptanceToDto ::
  GraphProtocol.TopologyCutAcceptance ->
  Either PeerRpcBridgeError Protocol.TopologyCutAcceptanceDto
topologyCutAcceptanceToDto acceptance =
  let report = GraphProtocol.topologyCutAcceptanceReport acceptance
   in Protocol.TopologyCutAcceptanceDto
        <$> topologyCutToClaim (GraphProtocol.topologyCutAcceptanceId acceptance)
        <*> heraldEpochToClaim
          (GraphProtocol.structuralAppliedReportReporter report)
        <*> structuralVersionVectorToDto
          (GraphProtocol.structuralAppliedReportVersionVector report)
        <*> pure
          ( controlIndexToDto
              (GraphProtocol.structuralAppliedReportControlPrefix report)
          )

topologyCutEstablishedFromDto ::
  Protocol.TopologyCutEstablishedDto ->
  Either PeerRpcBridgeError GraphProtocol.TopologyCutEstablished
topologyCutEstablishedFromDto
  (Protocol.TopologyCutEstablishedDto cutClaim cutDto acceptancesDto) = do
    claimed <- topologyCutFromClaim cutClaim
    cut <- topologyCutFromDto cutDto
    acceptances <-
      traverse
        topologyCutAcceptanceFromDto
        (Protocol.topologyCutAcceptanceEntries acceptancesDto)
    bridgeClaim
      ( GraphProtocol.topologyCutEstablished
          claimed
          cut
          (NonEmpty.toList acceptances)
      )

topologyCutEstablishedToDto ::
  GraphProtocol.TopologyCutEstablished ->
  Either PeerRpcBridgeError Protocol.TopologyCutEstablishedDto
topologyCutEstablishedToDto established = do
  cutClaim <- topologyCutToClaim (GraphProtocol.topologyCutEstablishedId established)
  cutDto <- topologyCutToDto (GraphProtocol.topologyCutEstablishedCut established)
  acceptances <-
    traverse
      topologyCutAcceptanceToDto
      (GraphProtocol.topologyCutEstablishedAcceptances established)
  checkedAcceptances <-
    bridgeOwnerPublication (Protocol.topologyCutAcceptancesDto acceptances)
  pure (Protocol.TopologyCutEstablishedDto cutClaim cutDto checkedAcceptances)

topologyCutEstablishedAckFromDto ::
  Protocol.TopologyCutEstablishedAckDto ->
  Either PeerRpcBridgeError GraphProtocol.TopologyCutEstablishedAck
topologyCutEstablishedAckFromDto
  (Protocol.TopologyCutEstablishedAckDto cutClaim reporterClaim generationClaim) =
    GraphProtocol.topologyCutEstablishedAck
      <$> topologyCutFromClaim cutClaim
      <*> heraldEpochFromClaim reporterClaim
      <*> membershipGenerationFromClaim generationClaim

topologyCutEstablishedAckToDto ::
  GraphProtocol.TopologyCutEstablishedAck ->
  Either PeerRpcBridgeError Protocol.TopologyCutEstablishedAckDto
topologyCutEstablishedAckToDto acknowledgement =
  Protocol.TopologyCutEstablishedAckDto
    <$> topologyCutToClaim
      (GraphProtocol.topologyCutEstablishedAckId acknowledgement)
    <*> heraldEpochToClaim
      (GraphProtocol.topologyCutEstablishedAckReporter acknowledgement)
    <*> membershipGenerationToClaim
      (GraphProtocol.topologyCutEstablishedAckMembershipGenerationId acknowledgement)

alignmentRetainedEvidenceFromDto :: Protocol.AlignmentRetainedEvidenceDto -> Either PeerRpcBridgeError Alignment.AlignmentControl
alignmentRetainedEvidenceFromDto = \case
  Protocol.AlignmentPlanAcceptanceEvidenceDto accepted -> Alignment.AlignmentPlanAcceptanceAdvertised <$> alignmentPlanAcceptedFromDto accepted
  Protocol.AlignmentCutAcceptanceEvidenceDto accepted -> Alignment.AlignmentCutAcceptanceAdvertised <$> alignmentCutAcceptedFromDto accepted
  Protocol.AlignmentMemberReadyEvidenceDto ready -> Alignment.AlignmentMemberReadyAdvertised <$> classMemberReadyFromDto ready
  Protocol.AlignmentPlanObsoleteEvidenceDto obsolete -> Alignment.AlignmentPlanObsoleteAdvertised <$> alignmentPlanObsoleteFromDto obsolete

alignmentRetainedEvidenceToDto :: Alignment.AlignmentControl -> Either PeerRpcBridgeError Protocol.AlignmentRetainedEvidenceDto
alignmentRetainedEvidenceToDto = \case
  Alignment.AlignmentPlanAcceptanceAdvertised accepted -> Protocol.AlignmentPlanAcceptanceEvidenceDto <$> alignmentPlanAcceptedToDto accepted
  Alignment.AlignmentCutAcceptanceAdvertised accepted -> Protocol.AlignmentCutAcceptanceEvidenceDto <$> alignmentCutAcceptedToDto accepted
  Alignment.AlignmentMemberReadyAdvertised ready -> Protocol.AlignmentMemberReadyEvidenceDto <$> classMemberReadyToDto ready
  Alignment.AlignmentPlanObsoleteAdvertised obsolete -> Protocol.AlignmentPlanObsoleteEvidenceDto <$> alignmentPlanObsoleteToDto obsolete
  _ -> Left PeerRpcBridgeOwnerPublicationContradiction

alignmentControlFromDto ::
  Protocol.AlignmentControlDto ->
  Either PeerRpcBridgeError Alignment.AlignmentControl
alignmentControlFromDto = \case
  Protocol.AlignmentPlanAnnouncedDto announce -> Alignment.AlignmentPlanAnnounced <$> alignmentPlanAnnounceFromDto announce
  Protocol.AlignmentCutAnnouncedDto announce ->
    Alignment.AlignmentCutAnnounced <$> alignmentCutAnnounceFromDto announce
  Protocol.AlignmentHistoricalCertificateAdvertisedDto certificate ->
    Alignment.AlignmentHistoricalCertificateAdvertised
      <$> historicalCertificateFromDto certificate
  Protocol.AlignmentSubscribeRequestedDto subscribe ->
    Alignment.AlignmentSubscribeRequested <$> alignmentSubscribeFromDto subscribe
  Protocol.AlignmentSnapshotStartedDto start ->
    Alignment.AlignmentSnapshotStarted <$> alignmentSnapshotStartFromDto start
  Protocol.AlignmentSnapshotChunkTransferredDto chunk ->
    Alignment.AlignmentSnapshotChunkTransferred
      <$> alignmentSnapshotChunkFromDto chunk
  Protocol.AlignmentSnapshotEndedDto end ->
    Alignment.AlignmentSnapshotEnded <$> alignmentSnapshotEndFromDto end
  Protocol.AlignmentChangeTransferredDto change ->
    Alignment.AlignmentChangeTransferred <$> alignmentChangeFromDto change
  Protocol.AlignmentLiveAdvertisedDto live ->
    Alignment.AlignmentLiveAdvertised <$> alignmentLiveFromDto live
  Protocol.AlignmentAcknowledgedDto acknowledgement ->
    Alignment.AlignmentAcknowledged <$> alignmentAckFromDto acknowledgement
  Protocol.AlignmentCancelledDto cancellation ->
    Alignment.AlignmentCancelled <$> alignmentCancelFromDto cancellation
  Protocol.AlignmentProbeMarkerDto probe subscription revision ->
    Alignment.AlignmentProbeMarker
      <$> disappearanceProbeFromDto probe
      <*> alignmentSubscriptionIdFromDto subscription
      <*> pure (storeRevisionFromDto revision)

alignmentControlToDto ::
  Alignment.AlignmentControl ->
  Either PeerRpcBridgeError Protocol.AlignmentControlDto
alignmentControlToDto = \case
  Alignment.AlignmentPlanAnnounced announce -> Protocol.AlignmentPlanAnnouncedDto <$> alignmentPlanAnnounceToDto announce
  Alignment.AlignmentPlanAcceptanceAdvertised _ -> Left PeerRpcBridgeOwnerPublicationContradiction
  Alignment.AlignmentPlanObsoleteAdvertised _ -> Left PeerRpcBridgeOwnerPublicationContradiction
  Alignment.AlignmentCutAnnounced announce ->
    Protocol.AlignmentCutAnnouncedDto <$> alignmentCutAnnounceToDto announce
  Alignment.AlignmentCutAcceptanceAdvertised _ -> Left PeerRpcBridgeOwnerPublicationContradiction
  Alignment.AlignmentMemberReadyAdvertised _ -> Left PeerRpcBridgeOwnerPublicationContradiction
  Alignment.AlignmentHistoricalCertificateAdvertised certificate ->
    Protocol.AlignmentHistoricalCertificateAdvertisedDto
      <$> historicalCertificateToDto certificate
  Alignment.AlignmentSubscribeRequested subscribe ->
    Protocol.AlignmentSubscribeRequestedDto <$> alignmentSubscribeToDto subscribe
  Alignment.AlignmentSnapshotStarted start ->
    Protocol.AlignmentSnapshotStartedDto <$> alignmentSnapshotStartToDto start
  Alignment.AlignmentSnapshotChunkTransferred chunk ->
    Protocol.AlignmentSnapshotChunkTransferredDto
      <$> alignmentSnapshotChunkToDto chunk
  Alignment.AlignmentSnapshotEnded end ->
    Protocol.AlignmentSnapshotEndedDto <$> alignmentSnapshotEndToDto end
  Alignment.AlignmentChangeTransferred change ->
    Protocol.AlignmentChangeTransferredDto <$> alignmentChangeToDto change
  Alignment.AlignmentLiveAdvertised live ->
    Protocol.AlignmentLiveAdvertisedDto <$> alignmentLiveToDto live
  Alignment.AlignmentAcknowledged acknowledgement ->
    Protocol.AlignmentAcknowledgedDto <$> alignmentAckToDto acknowledgement
  Alignment.AlignmentCancelled cancellation ->
    Protocol.AlignmentCancelledDto <$> alignmentCancelToDto cancellation
  Alignment.AlignmentProbeMarker probe subscription revision ->
    Protocol.AlignmentProbeMarkerDto
      <$> disappearanceProbeToDto probe
      <*> alignmentSubscriptionIdToDto subscription
      <*> pure (storeRevisionToDto revision)

alignmentPlanIdFromDto :: Protocol.AlignmentPlanIdDto -> Either PeerRpcBridgeError Plan.AlignmentPlanId
alignmentPlanIdFromDto (Protocol.AlignmentPlanIdDto membershipChange attempt sortId occurrenceClaim topologyClaim placementDto) =
  Plan.alignmentPlanIdFromClaimedCoordinatesAtAttempt (DomainAlignment.mkAlignmentPlanAttemptAtMembership (Identity.controlIndex membershipChange) attempt)
    <$> (Debt.sortOccurrence sortId <$> sortOccurrenceFromClaim occurrenceClaim)
    <*> topologyCutFromClaim topologyClaim
    <*> physicalPlacementRevisionVectorFromDto placementDto

alignmentPlanIdToDto :: Plan.AlignmentPlanId -> Either PeerRpcBridgeError Protocol.AlignmentPlanIdDto
alignmentPlanIdToDto identifier =
  Protocol.AlignmentPlanIdDto (Identity.controlIndexWord64 (DomainAlignment.alignmentPlanAttemptMembershipChange (Plan.alignmentPlanIdAttempt identifier))) (DomainAlignment.alignmentPlanAttemptWord64 (Plan.alignmentPlanIdAttempt identifier)) (Debt.sortOccurrenceSortId occurrence)
    <$> sortOccurrenceToClaim (Debt.sortOccurrenceDefinition occurrence)
    <*> topologyCutToClaim (Plan.alignmentPlanIdTopology identifier)
    <*> physicalPlacementRevisionVectorToDto (Plan.alignmentPlanIdPlacement identifier)
  where
    occurrence = Plan.alignmentPlanIdSort identifier

alignmentPlanAnnounceFromDto :: Protocol.AlignmentPlanAnnounceDto -> Either PeerRpcBridgeError Alignment.AlignmentPlanAnnounce
alignmentPlanAnnounceFromDto (Protocol.AlignmentPlanAnnounceDto identifierDto topologyDto predecessorDto statusDto bindingDtos relationDtos cutDtos) = do
  identifier <- alignmentPlanIdFromDto identifierDto
  topology <- topologyCutFromDto topologyDto
  predecessor <- traverse alignmentPlanIdFromDto predecessorDto
  bindings <- traverse bindingFromDto bindingDtos
  relations <- traverse relationFromDto relationDtos
  cuts <- traverse alignmentCutAnnounceFromDto cutDtos
  bridgeClaim (Alignment.alignmentPlanAnnounce identifier topology predecessor (case statusDto of Protocol.AlignmentPredecessorUsableDto -> Alignment.AlignmentPredecessorUsable; Protocol.AlignmentPredecessorInvalidatedDto -> Alignment.AlignmentPredecessorInvalidated; Protocol.AlignmentPredecessorResetDto -> Alignment.AlignmentPredecessorReset) bindings relations cuts)
  where
    bindingFromDto (Protocol.AlignmentPlanBindingClaimDto memberClaims dispositionDto generationClaim) = do
      members <- traverse deltaFromClaim memberClaims
      generation <- contextClassGenerationFromClaim generationClaim
      let disposition = case dispositionDto of
            Protocol.AlignmentCreatedDto -> Alignment.AlignmentCreated
            Protocol.AlignmentCarriedDto -> Alignment.AlignmentCarried
      bridgeClaim (Alignment.alignmentPlanBindingClaim members disposition generation)
    relationFromDto (Protocol.AlignmentPlanRelationClaimDto sourceClaim destinationClaim strengthDto) =
      Alignment.alignmentPlanRelationClaim <$> contextClassGenerationFromClaim sourceClaim <*> contextClassGenerationFromClaim destinationClaim <*> pure (replicaStrengthFromDto strengthDto)

alignmentPlanAnnounceToDto :: Alignment.AlignmentPlanAnnounce -> Either PeerRpcBridgeError Protocol.AlignmentPlanAnnounceDto
alignmentPlanAnnounceToDto announce =
  Protocol.AlignmentPlanAnnounceDto
    <$> alignmentPlanIdToDto (Alignment.alignmentPlanAnnounceId announce)
    <*> topologyCutToDto (Alignment.alignmentPlanAnnounceTopology announce)
    <*> traverse alignmentPlanIdToDto (Alignment.alignmentPlanAnnouncePredecessor announce)
    <*> pure (case Alignment.alignmentPlanAnnouncePredecessorStatus announce of Alignment.AlignmentPredecessorUsable -> Protocol.AlignmentPredecessorUsableDto; Alignment.AlignmentPredecessorInvalidated -> Protocol.AlignmentPredecessorInvalidatedDto; Alignment.AlignmentPredecessorReset -> Protocol.AlignmentPredecessorResetDto)
    <*> traverse bindingToDto (Alignment.alignmentPlanAnnounceBindings announce)
    <*> traverse relationToDto (Alignment.alignmentPlanAnnounceRelations announce)
    <*> traverse alignmentCutAnnounceToDto (Alignment.alignmentPlanAnnounceCreatedCuts announce)
  where
    bindingToDto binding =
      Protocol.AlignmentPlanBindingClaimDto
        <$> traverse deltaToClaim (Alignment.alignmentPlanBindingClaimMembers binding)
        <*> pure
          ( case Alignment.alignmentPlanBindingClaimDisposition binding of
              Alignment.AlignmentCreated -> Protocol.AlignmentCreatedDto
              Alignment.AlignmentCarried -> Protocol.AlignmentCarriedDto
          )
        <*> contextClassGenerationToClaim (Alignment.alignmentPlanBindingClaimGeneration binding)
    relationToDto relation =
      Protocol.AlignmentPlanRelationClaimDto
        <$> contextClassGenerationToClaim (Alignment.alignmentPlanRelationClaimSource relation)
        <*> contextClassGenerationToClaim (Alignment.alignmentPlanRelationClaimDestination relation)
        <*> pure (replicaStrengthToDto (Alignment.alignmentPlanRelationClaimStrength relation))

alignmentPlanAcceptedFromDto :: Protocol.AlignmentPlanAcceptedDto -> Either PeerRpcBridgeError Alignment.AlignmentPlanAccepted
alignmentPlanAcceptedFromDto (Protocol.AlignmentPlanAcceptedDto identifierDto heraldClaim prefixDto) = do
  identifier <- alignmentPlanIdFromDto identifierDto
  herald <- heraldEpochFromClaim heraldClaim
  prefix <- heraldPublicationPrefixFromDto prefixDto
  bridgeClaim (Alignment.alignmentPlanAccepted identifier herald prefix)

alignmentPlanAcceptedToDto :: Alignment.AlignmentPlanAccepted -> Either PeerRpcBridgeError Protocol.AlignmentPlanAcceptedDto
alignmentPlanAcceptedToDto accepted =
  Protocol.AlignmentPlanAcceptedDto
    <$> alignmentPlanIdToDto (Alignment.alignmentPlanAcceptedId accepted)
    <*> heraldEpochToClaim (Alignment.alignmentPlanAcceptedHerald accepted)
    <*> heraldPublicationPrefixToDto (Alignment.alignmentPlanAcceptedPublicationPrefix accepted)

alignmentPlanObsoleteFromDto :: Protocol.AlignmentPlanObsoleteDto -> Either PeerRpcBridgeError Alignment.AlignmentPlanObsolete
alignmentPlanObsoleteFromDto (Protocol.AlignmentPlanObsoleteDto identifierDto reporterClaim factsDto) = do
  identifier <- alignmentPlanIdFromDto identifierDto
  reporter <- heraldEpochFromClaim reporterClaim
  facts <- traverse lostFromDto factsDto
  bridgeClaim (Alignment.alignmentPlanObsolete identifier reporter facts)
  where
    lostFromDto (Protocol.AlignmentLostStoreDto homeClaim deltaClaim incarnationClaim revisionDto) =
      Alignment.alignmentLostStore
        <$> heraldEpochFromClaim homeClaim
        <*> deltaFromClaim deltaClaim
        <*> storeIncarnationFromClaim incarnationClaim
        <*> placementSequenceFromDto revisionDto

alignmentPlanObsoleteToDto :: Alignment.AlignmentPlanObsolete -> Either PeerRpcBridgeError Protocol.AlignmentPlanObsoleteDto
alignmentPlanObsoleteToDto report =
  Protocol.AlignmentPlanObsoleteDto
    <$> alignmentPlanIdToDto (Alignment.alignmentPlanObsoleteId report)
    <*> heraldEpochToClaim (Alignment.alignmentPlanObsoleteReporter report)
    <*> traverse lostToDto (Alignment.alignmentPlanObsoleteLostStores report)
  where
    lostToDto fact =
      Protocol.AlignmentLostStoreDto
        <$> heraldEpochToClaim (Alignment.alignmentLostStoreHome fact)
        <*> deltaToClaim (Alignment.alignmentLostStoreDelta fact)
        <*> storeIncarnationToClaim (Alignment.alignmentLostStoreIncarnation fact)
        <*> placementSequenceToDto (Alignment.alignmentLostStorePlacementRevision fact)

alignmentCutAnnounceFromDto ::
  Protocol.AlignmentCutAnnounceDto ->
  Either PeerRpcBridgeError Alignment.AlignmentCutAnnounce
alignmentCutAnnounceFromDto
  (Protocol.AlignmentCutAnnounceDto generationClaim cutDto) = do
    generation <- contextClassGenerationFromClaim generationClaim
    cut <- alignmentCutFromDto cutDto
    bridgeClaim (Alignment.checkAlignmentCutAnnounce generation cut)

alignmentCutAnnounceToDto ::
  Alignment.AlignmentCutAnnounce ->
  Either PeerRpcBridgeError Protocol.AlignmentCutAnnounceDto
alignmentCutAnnounceToDto announce =
  Protocol.AlignmentCutAnnounceDto
    <$> contextClassGenerationToClaim
      (Alignment.alignmentCutAnnounceGeneration announce)
    <*> alignmentCutToDto (Alignment.alignmentCutAnnounceCut announce)

alignmentCutFromDto ::
  Protocol.AlignmentCutDto ->
  Either PeerRpcBridgeError DomainAlignment.AlignmentCut
alignmentCutFromDto
  ( Protocol.AlignmentCutDto
      membershipChange
      attempt
      sortId
      occurrenceClaim
      topologyDto
      placementDto
      membersDto
      predecessorsClaims
      freshDto
    ) = do
    occurrence <- sortOccurrenceFromClaim occurrenceClaim
    topology <- topologyCutFromDto topologyDto
    placement <- physicalPlacementRevisionVectorFromDto placementDto
    members <- traverse alignmentMemberFromDto membersDto
    predecessors <- traverse contextClassGenerationFromClaim predecessorsClaims
    fresh <- traverse freshMemberBaseEvidenceFromDto freshDto
    bridgeClaim
      ( DomainAlignment.alignmentCutAtAttempt
          (DomainAlignment.mkAlignmentPlanAttemptAtMembership (Identity.controlIndex membershipChange) attempt)
          sortId
          occurrence
          topology
          placement
          members
          predecessors
          fresh
      )

alignmentCutToDto ::
  DomainAlignment.AlignmentCut ->
  Either PeerRpcBridgeError Protocol.AlignmentCutDto
alignmentCutToDto cut =
  Protocol.AlignmentCutDto
    (Identity.controlIndexWord64 (DomainAlignment.alignmentPlanAttemptMembershipChange (DomainAlignment.alignmentCutAttempt cut)))
    (DomainAlignment.alignmentPlanAttemptWord64 (DomainAlignment.alignmentCutAttempt cut))
    (DomainAlignment.alignmentCutSortId cut)
    <$> sortOccurrenceToClaim
      (DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut)
    <*> topologyCutToDto (DomainAlignment.alignmentCutTopologyCut cut)
    <*> physicalPlacementRevisionVectorToDto
      (DomainAlignment.alignmentCutPhysicalPlacementRevisionVector cut)
    <*> traverse
      alignmentMemberToDto
      (DomainAlignment.alignmentCutExactMembers cut)
    <*> traverse
      contextClassGenerationToClaim
      (DomainAlignment.alignmentCutPredecessorGenerationIds cut)
    <*> traverse
      freshMemberBaseEvidenceToDto
      (DomainAlignment.alignmentCutFreshMemberBaseEvidence cut)

physicalPlacementRevisionVectorFromDto ::
  Protocol.PhysicalPlacementRevisionVectorDto ->
  Either PeerRpcBridgeError DomainAlignment.PhysicalPlacementRevisionVector
physicalPlacementRevisionVectorFromDto dto = do
  generation <-
    membershipGenerationFromClaim
      (Protocol.physicalPlacementRevisionVectorMembershipGeneration dto)
  members <-
    memberSetDigestFromClaim
      (Protocol.physicalPlacementRevisionVectorMemberSetDigest dto)
  entries <-
    traverse
      physicalPlacementRevisionEntryFromDto
      (Protocol.physicalPlacementRevisionVectorEntries dto)
  bridgeClaim
    ( DomainAlignment.physicalPlacementRevisionVectorFromClaimedCoordinates
        generation
        members
        entries
    )

physicalPlacementRevisionVectorToDto ::
  DomainAlignment.PhysicalPlacementRevisionVector ->
  Either PeerRpcBridgeError Protocol.PhysicalPlacementRevisionVectorDto
physicalPlacementRevisionVectorToDto vector = do
  generation <-
    membershipGenerationToClaim
      (DomainAlignment.physicalPlacementRevisionMembershipGenerationId vector)
  members <-
    memberSetDigestToClaim
      (DomainAlignment.physicalPlacementRevisionMemberSetDigest vector)
  entries <-
    traverse
      physicalPlacementRevisionEntryToDto
      (DomainAlignment.physicalPlacementRevisionEntries vector)
  bridgeOwnerPublication
    ( Protocol.physicalPlacementRevisionVectorDto
        generation
        members
        (NonEmpty.toList entries)
    )

physicalPlacementRevisionEntryFromDto ::
  Protocol.PhysicalPlacementRevisionEntryDto ->
  Either
    PeerRpcBridgeError
    (Identity.HeraldEpoch, DomainAlignment.PlacementRevision)
physicalPlacementRevisionEntryFromDto
  (Protocol.PhysicalPlacementRevisionEntryDto heraldClaim revisionDto) =
    (,)
      <$> heraldEpochFromClaim heraldClaim
      <*> bridgeClaim
        ( DomainAlignment.mkPlacementRevision
            (Protocol.positivePlacementSequenceDtoWord64 revisionDto)
        )

physicalPlacementRevisionEntryToDto ::
  (Identity.HeraldEpoch, DomainAlignment.PlacementRevision) ->
  Either PeerRpcBridgeError Protocol.PhysicalPlacementRevisionEntryDto
physicalPlacementRevisionEntryToDto (herald, revision) =
  Protocol.PhysicalPlacementRevisionEntryDto
    <$> heraldEpochToClaim herald
    <*> bridgeClaim
      ( Protocol.positivePlacementSequenceDto
          (DomainAlignment.placementRevisionWord64 revision)
      )

alignmentMemberFromDto ::
  Protocol.AlignmentMemberDto ->
  Either PeerRpcBridgeError DomainAlignment.AlignmentMember
alignmentMemberFromDto
  (Protocol.AlignmentMemberDto deltaClaim storeClaim heraldClaim) =
    DomainAlignment.alignmentMember
      <$> deltaFromClaim deltaClaim
      <*> storeIncarnationFromClaim storeClaim
      <*> heraldEpochFromClaim heraldClaim

alignmentMemberToDto ::
  DomainAlignment.AlignmentMember ->
  Either PeerRpcBridgeError Protocol.AlignmentMemberDto
alignmentMemberToDto member =
  Protocol.AlignmentMemberDto
    <$> deltaToClaim (DomainAlignment.alignmentMemberDelta member)
    <*> storeIncarnationToClaim
      (DomainAlignment.alignmentMemberStoreIncarnation member)
    <*> heraldEpochToClaim (DomainAlignment.alignmentMemberHerald member)

freshMemberBaseEvidenceFromDto ::
  Protocol.FreshMemberBaseEvidenceDto ->
  Either PeerRpcBridgeError DomainAlignment.FreshMemberBaseEvidence
freshMemberBaseEvidenceFromDto
  (Protocol.FreshMemberBaseEvidenceDto deltaClaim storeClaim revisionDto) =
    DomainAlignment.freshMemberBaseEvidence
      <$> deltaFromClaim deltaClaim
      <*> storeIncarnationFromClaim storeClaim
      <*> pure (storeRevisionFromDto revisionDto)

freshMemberBaseEvidenceToDto ::
  DomainAlignment.FreshMemberBaseEvidence ->
  Either PeerRpcBridgeError Protocol.FreshMemberBaseEvidenceDto
freshMemberBaseEvidenceToDto evidence =
  Protocol.FreshMemberBaseEvidenceDto
    <$> deltaToClaim (DomainAlignment.freshMemberBaseDelta evidence)
    <*> storeIncarnationToClaim
      (DomainAlignment.freshMemberBaseStoreIncarnation evidence)
    <*> pure
      (storeRevisionToDto (DomainAlignment.freshMemberBaseRevision evidence))

alignmentCutAcceptedFromDto ::
  Protocol.AlignmentCutAcceptedDto ->
  Either PeerRpcBridgeError Alignment.AlignmentCutAccepted
alignmentCutAcceptedFromDto
  ( Protocol.AlignmentCutAcceptedDto
      generationClaim
      heraldClaim
      topologyClaim
      placementDto
      publicationPrefixDto
    ) =
    Alignment.alignmentCutAccepted
      <$> contextClassGenerationFromClaim generationClaim
      <*> heraldEpochFromClaim heraldClaim
      <*> topologyCutFromClaim topologyClaim
      <*> physicalPlacementRevisionVectorFromDto placementDto
      <*> heraldPublicationPrefixFromDto publicationPrefixDto

alignmentCutAcceptedToDto ::
  Alignment.AlignmentCutAccepted ->
  Either PeerRpcBridgeError Protocol.AlignmentCutAcceptedDto
alignmentCutAcceptedToDto accepted =
  Protocol.AlignmentCutAcceptedDto
    <$> contextClassGenerationToClaim
      (Alignment.alignmentCutAcceptedGeneration accepted)
    <*> heraldEpochToClaim (Alignment.alignmentCutAcceptedHerald accepted)
    <*> topologyCutToClaim (Alignment.alignmentCutAcceptedTopologyCut accepted)
    <*> physicalPlacementRevisionVectorToDto
      (Alignment.alignmentCutAcceptedPlacementVector accepted)
    <*> heraldPublicationPrefixToDto
      (Alignment.alignmentCutAcceptedPublicationPrefix accepted)

classMemberReadyFromDto ::
  Protocol.ClassMemberReadyDto ->
  Either PeerRpcBridgeError Alignment.ClassMemberReady
classMemberReadyFromDto
  ( Protocol.ClassMemberReadyDto
      sequenceDto
      generationClaim
      storeClaim
      prefixDigestClaim
      revisionDto
    ) =
    Alignment.classMemberReady
      <$> alignmentEvidenceSequenceFromDto sequenceDto
      <*> contextClassGenerationFromClaim generationClaim
      <*> storeIncarnationFromClaim storeClaim
      <*> memberReadyEvidenceDigestFromClaim prefixDigestClaim
      <*> pure (storeRevisionFromDto revisionDto)

classMemberReadyToDto ::
  Alignment.ClassMemberReady ->
  Either PeerRpcBridgeError Protocol.ClassMemberReadyDto
classMemberReadyToDto ready =
  Protocol.ClassMemberReadyDto
    <$> alignmentEvidenceSequenceToDto
      (Alignment.classMemberReadyEvidenceSequence ready)
    <*> contextClassGenerationToClaim
      (Alignment.classMemberReadyGeneration ready)
    <*> storeIncarnationToClaim
      (Alignment.classMemberReadyStoreIncarnation ready)
    <*> memberReadyEvidenceDigestToClaim
      (Alignment.classMemberReadyPredecessorAndBasePrefixDigest ready)
    <*> pure (storeRevisionToDto (Alignment.classMemberReadyStoreRevision ready))

historicalCertificateFromDto ::
  Protocol.HistoricalCertificateDto ->
  Either PeerRpcBridgeError Alignment.HistoricalCertificate
historicalCertificateFromDto
  ( Protocol.HistoricalCertificateDto
      generationClaim
      storeClaim
      readyDigestClaim
      predecessorDigestClaim
      revisionDto
    ) =
    Alignment.historicalCertificate
      <$> contextClassGenerationFromClaim generationClaim
      <*> storeIncarnationFromClaim storeClaim
      <*> memberReadyEvidenceDigestFromClaim readyDigestClaim
      <*> bootstrapEvidenceDigestFromClaim predecessorDigestClaim
      <*> pure (storeRevisionFromDto revisionDto)

historicalCertificateToDto ::
  Alignment.HistoricalCertificate ->
  Either PeerRpcBridgeError Protocol.HistoricalCertificateDto
historicalCertificateToDto certificate =
  Protocol.HistoricalCertificateDto
    <$> contextClassGenerationToClaim
      (Alignment.historicalCertificateClassGeneration certificate)
    <*> storeIncarnationToClaim
      (Alignment.historicalCertificateSourceStoreIncarnation certificate)
    <*> memberReadyEvidenceDigestToClaim
      (Alignment.historicalCertificateExactMemberReadyDigest certificate)
    <*> bootstrapEvidenceDigestToClaim
      (Alignment.historicalCertificateCompletedPredecessorPrefixDigest certificate)
    <*> pure
      ( storeRevisionToDto
          (Alignment.historicalCertificateCertifiedStoreRevision certificate)
      )

alignmentSubscribeFromDto ::
  Protocol.AlignmentSubscribeDto ->
  Either PeerRpcBridgeError Alignment.AlignmentSubscribe
alignmentSubscribeFromDto
  ( Protocol.AlignmentSubscribeDto
      obligationDto
      subscriptionDto
      sourceStoreClaim
      proofDto
    ) = do
    obligation <- alignmentObligationFromDto obligationDto
    subscription <- alignmentSubscriptionIdFromDto subscriptionDto
    sourceStore <- storeIncarnationFromClaim sourceStoreClaim
    proof <- alignmentSourceProofFromDto proofDto
    bridgeClaim
      ( Alignment.alignmentSubscribe
          obligation
          subscription
          sourceStore
          proof
      )

alignmentSubscribeToDto ::
  Alignment.AlignmentSubscribe ->
  Either PeerRpcBridgeError Protocol.AlignmentSubscribeDto
alignmentSubscribeToDto subscribe =
  Protocol.AlignmentSubscribeDto
    <$> alignmentObligationToDto
      (Alignment.alignmentSubscribeObligation subscribe)
    <*> alignmentSubscriptionIdToDto
      (Alignment.alignmentSubscribeSubscriptionId subscribe)
    <*> storeIncarnationToClaim
      (Alignment.alignmentSubscribeSourceStoreIncarnation subscribe)
    <*> alignmentSourceProofToDto
      (Alignment.alignmentSubscribeSourceProof subscribe)

alignmentObligationFromDto ::
  Protocol.AlignmentObligationDto ->
  Either PeerRpcBridgeError Alignment.AlignmentObligation
alignmentObligationFromDto
  ( Protocol.AlignmentObligationDto
      identifierDto
      causeDto
      sortId
      occurrenceClaim
      destinationGenerationClaim
      destinationDtos
      sourceGenerationClaim
      strengthDto
    ) = do
    identifier <- alignmentObligationIdFromDto identifierDto
    cause <- structuralConsequenceCauseFromDto causeDto
    occurrence <- sortOccurrenceFromClaim occurrenceClaim
    destinationGeneration <-
      contextClassGenerationFromClaim destinationGenerationClaim
    destinations <- traverse destinationStoreFromDto destinationDtos
    sourceGeneration <- contextClassGenerationFromClaim sourceGenerationClaim
    bridgeClaim
      ( Alignment.alignmentObligation
          identifier
          cause
          sortId
          occurrence
          destinationGeneration
          destinations
          sourceGeneration
          (replicaStrengthFromDto strengthDto)
      )

alignmentObligationToDto ::
  Alignment.AlignmentObligation ->
  Either PeerRpcBridgeError Protocol.AlignmentObligationDto
alignmentObligationToDto obligation =
  Protocol.AlignmentObligationDto
    <$> alignmentObligationIdToDto
      (Alignment.alignmentObligationIdValue obligation)
    <*> structuralConsequenceCauseToDto
      (Alignment.alignmentObligationCause obligation)
    <*> pure (Alignment.alignmentObligationSortId obligation)
    <*> sortOccurrenceToClaim
      (Alignment.alignmentObligationSortDefinitionOccurrenceId obligation)
    <*> contextClassGenerationToClaim
      (Alignment.alignmentObligationDestinationGeneration obligation)
    <*> traverse
      destinationStoreToDto
      (Alignment.alignmentObligationDestinationStores obligation)
    <*> contextClassGenerationToClaim
      (Alignment.alignmentObligationSourceGeneration obligation)
    <*> pure
      ( replicaStrengthToDto
          (Alignment.alignmentObligationFrozenContextPathStrength obligation)
      )

structuralConsequenceCauseFromDto ::
  Protocol.StructuralConsequenceCauseDto ->
  Either PeerRpcBridgeError StructuralConsequence.StructuralConsequenceCause
structuralConsequenceCauseFromDto = \case
  Protocol.StructuralOccurrenceCauseDto occurrence ->
    StructuralConsequence.structuralOccurrenceCause
      <$> structuralOccurrenceIdFromDto occurrence
  Protocol.LabelReleaseCauseDto decisionClaim indexDto -> do
    decision <-
      bridgeClaim
        (Identity.mkLabelDecisionId (Protocol.labelDecisionClaimBytes decisionClaim))
    bridgeClaim
      ( StructuralConsequence.labelReleaseCause
          decision
          (controlIndexFromDto indexDto)
      )
  Protocol.ProcessEndCauseDto processClaim indexDto -> do
    process <- processEpochFromClaim processClaim
    bridgeClaim
      ( StructuralConsequence.processEndCause
          process
          (controlIndexFromDto indexDto)
      )
  Protocol.PredefinedDisappearanceCauseDto probeDto indexDto -> do
    probe <- disappearanceProbeFromDto probeDto
    bridgeClaim
      (StructuralConsequence.predefinedDisappearanceCause probe (controlIndexFromDto indexDto))

structuralConsequenceCauseToDto ::
  StructuralConsequence.StructuralConsequenceCause ->
  Either PeerRpcBridgeError Protocol.StructuralConsequenceCauseDto
structuralConsequenceCauseToDto cause =
  case StructuralConsequence.structuralConsequenceCauseView cause of
    StructuralConsequence.StructuralOccurrenceCauseView occurrence ->
      Protocol.StructuralOccurrenceCauseDto
        <$> structuralOccurrenceIdToDto occurrence
    StructuralConsequence.LabelReleaseCauseView decision index ->
      Protocol.LabelReleaseCauseDto
        <$> bridgeClaim
          (Protocol.labelDecisionClaim (Identity.labelDecisionIdBytes decision))
        <*> pure (controlIndexToDto index)
    StructuralConsequence.ProcessEndCauseView process index ->
      Protocol.ProcessEndCauseDto
        <$> processEpochToClaim process
        <*> pure (controlIndexToDto index)
    StructuralConsequence.PredefinedDisappearanceCauseView probe index ->
      Protocol.PredefinedDisappearanceCauseDto
        <$> disappearanceProbeToDto probe
        <*> pure (controlIndexToDto index)

alignmentSourceProofFromDto ::
  Protocol.AlignmentSourceProofDto ->
  Either PeerRpcBridgeError Alignment.AlignmentSourceProof
alignmentSourceProofFromDto = \case
  Protocol.FinalCertificateDto generationClaim digestClaim ->
    Alignment.FinalCertificate
      <$> contextClassGenerationFromClaim generationClaim
      <*> historicalCertificateDigestFromClaim digestClaim
  Protocol.BootstrapProofDto generationClaim digestClaim ->
    Alignment.BootstrapProof
      <$> contextClassGenerationFromClaim generationClaim
      <*> bootstrapEvidenceDigestFromClaim digestClaim

alignmentSourceProofToDto ::
  Alignment.AlignmentSourceProof ->
  Either PeerRpcBridgeError Protocol.AlignmentSourceProofDto
alignmentSourceProofToDto = \case
  Alignment.FinalCertificate generation digest ->
    Protocol.FinalCertificateDto
      <$> contextClassGenerationToClaim generation
      <*> historicalCertificateDigestToClaim digest
  Alignment.BootstrapProof generation digest ->
    Protocol.BootstrapProofDto
      <$> contextClassGenerationToClaim generation
      <*> bootstrapEvidenceDigestToClaim digest

destinationStoreFromDto ::
  Protocol.DestinationStoreDto ->
  Either PeerRpcBridgeError Alignment.DestinationStore
destinationStoreFromDto
  (Protocol.DestinationStoreDto deltaClaim storeClaim) =
    Alignment.destinationStore
      <$> deltaFromClaim deltaClaim
      <*> storeIncarnationFromClaim storeClaim

destinationStoreToDto ::
  Alignment.DestinationStore ->
  Either PeerRpcBridgeError Protocol.DestinationStoreDto
destinationStoreToDto destination =
  Protocol.DestinationStoreDto
    <$> deltaToClaim (Alignment.destinationStoreDelta destination)
    <*> storeIncarnationToClaim
      (Alignment.destinationStoreIncarnation destination)

alignmentSnapshotStartFromDto ::
  Protocol.AlignmentSnapshotStartDto ->
  Either PeerRpcBridgeError Alignment.AlignmentSnapshotStart
alignmentSnapshotStartFromDto
  (Protocol.AlignmentSnapshotStartDto subscriptionDto revisionDto digestClaim chunks) =
    Alignment.alignmentSnapshotStart
      <$> alignmentSubscriptionIdFromDto subscriptionDto
      <*> pure (storeRevisionFromDto revisionDto)
      <*> alignmentSnapshotDigestFromClaim digestClaim
      <*> pure chunks

alignmentSnapshotStartToDto ::
  Alignment.AlignmentSnapshotStart ->
  Either PeerRpcBridgeError Protocol.AlignmentSnapshotStartDto
alignmentSnapshotStartToDto start =
  Protocol.AlignmentSnapshotStartDto
    <$> alignmentSubscriptionIdToDto
      (Alignment.alignmentSnapshotStartSubscriptionId start)
    <*> pure
      (storeRevisionToDto (Alignment.alignmentSnapshotStartBaseRevision start))
    <*> alignmentSnapshotDigestToClaim
      (Alignment.alignmentSnapshotStartSemanticDigest start)
    <*> pure (Alignment.alignmentSnapshotStartChunkCount start)

alignmentSnapshotChunkFromDto ::
  Protocol.AlignmentSnapshotChunkDto ->
  Either PeerRpcBridgeError Alignment.AlignmentSnapshotChunk
alignmentSnapshotChunkFromDto
  (Protocol.AlignmentSnapshotChunkDto subscriptionDto chunkNumber retainedDtos) =
    Alignment.alignmentSnapshotChunk
      <$> alignmentSubscriptionIdFromDto subscriptionDto
      <*> pure chunkNumber
      <*> traverse retainedStateEvidenceFromDto retainedDtos

alignmentSnapshotChunkToDto ::
  Alignment.AlignmentSnapshotChunk ->
  Either PeerRpcBridgeError Protocol.AlignmentSnapshotChunkDto
alignmentSnapshotChunkToDto chunk =
  Protocol.AlignmentSnapshotChunkDto
    <$> alignmentSubscriptionIdToDto
      (Alignment.alignmentSnapshotChunkSubscriptionId chunk)
    <*> pure (Alignment.alignmentSnapshotChunkNumber chunk)
    <*> traverse
      retainedStateEvidenceToDto
      (Alignment.alignmentSnapshotChunkRetainedStates chunk)

alignmentSnapshotEndFromDto ::
  Protocol.AlignmentSnapshotEndDto ->
  Either PeerRpcBridgeError Alignment.AlignmentSnapshotEnd
alignmentSnapshotEndFromDto
  (Protocol.AlignmentSnapshotEndDto subscriptionDto revisionDto digestClaim) =
    Alignment.alignmentSnapshotEnd
      <$> alignmentSubscriptionIdFromDto subscriptionDto
      <*> pure (storeRevisionFromDto revisionDto)
      <*> alignmentSnapshotDigestFromClaim digestClaim

alignmentSnapshotEndToDto ::
  Alignment.AlignmentSnapshotEnd ->
  Either PeerRpcBridgeError Protocol.AlignmentSnapshotEndDto
alignmentSnapshotEndToDto end =
  Protocol.AlignmentSnapshotEndDto
    <$> alignmentSubscriptionIdToDto
      (Alignment.alignmentSnapshotEndSubscriptionId end)
    <*> pure
      (storeRevisionToDto (Alignment.alignmentSnapshotEndBaseRevision end))
    <*> alignmentSnapshotDigestToClaim
      (Alignment.alignmentSnapshotEndSemanticDigest end)

alignmentChangeFromDto ::
  Protocol.AlignmentChangeDto ->
  Either PeerRpcBridgeError Alignment.AlignmentChange
alignmentChangeFromDto
  (Protocol.AlignmentChangeDto subscriptionDto revisionDto retainedDto) =
    Alignment.alignmentChange
      <$> alignmentSubscriptionIdFromDto subscriptionDto
      <*> pure (storeRevisionFromDto revisionDto)
      <*> retainedPublicationEvidenceFromDto retainedDto

alignmentChangeToDto ::
  Alignment.AlignmentChange ->
  Either PeerRpcBridgeError Protocol.AlignmentChangeDto
alignmentChangeToDto change =
  Protocol.AlignmentChangeDto
    <$> alignmentSubscriptionIdToDto
      (Alignment.alignmentChangeSubscriptionId change)
    <*> pure (storeRevisionToDto (Alignment.alignmentChangeStoreRevision change))
    <*> retainedPublicationEvidenceToDto
      (Alignment.alignmentChangeRetainedTransition change)

alignmentLiveFromDto ::
  Protocol.AlignmentLiveDto ->
  Either PeerRpcBridgeError Alignment.AlignmentLive
alignmentLiveFromDto (Protocol.AlignmentLiveDto subscriptionDto revisionDto) =
  Alignment.alignmentLive
    <$> alignmentSubscriptionIdFromDto subscriptionDto
    <*> pure (storeRevisionFromDto revisionDto)

alignmentLiveToDto ::
  Alignment.AlignmentLive ->
  Either PeerRpcBridgeError Protocol.AlignmentLiveDto
alignmentLiveToDto live =
  Protocol.AlignmentLiveDto
    <$> alignmentSubscriptionIdToDto (Alignment.alignmentLiveSubscriptionId live)
    <*> pure
      ( storeRevisionToDto
          (Alignment.alignmentLiveThroughSourceStoreRevision live)
      )

alignmentAckFromDto ::
  Protocol.AlignmentAckDto ->
  Either PeerRpcBridgeError Alignment.AlignmentAck
alignmentAckFromDto (Protocol.AlignmentAckDto subscriptionDto revisionDto) =
  Alignment.alignmentAck
    <$> alignmentSubscriptionIdFromDto subscriptionDto
    <*> pure (storeRevisionFromDto revisionDto)

alignmentAckToDto ::
  Alignment.AlignmentAck ->
  Either PeerRpcBridgeError Protocol.AlignmentAckDto
alignmentAckToDto acknowledgement =
  Protocol.AlignmentAckDto
    <$> alignmentSubscriptionIdToDto
      (Alignment.alignmentAckSubscriptionId acknowledgement)
    <*> pure
      ( storeRevisionToDto
          (Alignment.alignmentAckAppliedSourceStoreRevision acknowledgement)
      )

alignmentCancelFromDto ::
  Protocol.AlignmentCancelDto ->
  Either PeerRpcBridgeError Alignment.AlignmentCancel
alignmentCancelFromDto
  (Protocol.AlignmentCancelDto subscriptionDto reasonDto) =
    Alignment.alignmentCancel
      <$> alignmentSubscriptionIdFromDto subscriptionDto
      <*> pure (alignmentCancelReasonFromDto reasonDto)

alignmentCancelToDto ::
  Alignment.AlignmentCancel ->
  Either PeerRpcBridgeError Protocol.AlignmentCancelDto
alignmentCancelToDto cancellation =
  Protocol.AlignmentCancelDto
    <$> alignmentSubscriptionIdToDto
      (Alignment.alignmentCancelSubscriptionId cancellation)
    <*> pure
      (alignmentCancelReasonToDto (Alignment.alignmentCancelReason cancellation))

alignmentCancelReasonFromDto ::
  Protocol.AlignmentCancelReasonDto -> Alignment.AlignmentCancelReason
alignmentCancelReasonFromDto = \case
  Protocol.AlignmentRelationRemovedDto -> Alignment.AlignmentRelationRemoved
  Protocol.AlignmentSourceIncarnationLostDto ->
    Alignment.AlignmentSourceIncarnationLost
  Protocol.AlignmentDestinationIncarnationLostDto ->
    Alignment.AlignmentDestinationIncarnationLost

alignmentCancelReasonToDto ::
  Alignment.AlignmentCancelReason -> Protocol.AlignmentCancelReasonDto
alignmentCancelReasonToDto = \case
  Alignment.AlignmentRelationRemoved -> Protocol.AlignmentRelationRemovedDto
  Alignment.AlignmentSourceIncarnationLost ->
    Protocol.AlignmentSourceIncarnationLostDto
  Alignment.AlignmentDestinationIncarnationLost ->
    Protocol.AlignmentDestinationIncarnationLostDto

retainedPublicationEvidenceFromDto ::
  Protocol.RetainedPublicationEvidenceDto ->
  Either PeerRpcBridgeError Alignment.RetainedPublicationEvidence
retainedPublicationEvidenceFromDto
  ( Protocol.RetainedPublicationEvidenceDto
      publicationDto
      sortId
      occurrenceClaim
      canonicalBytes
      strengthDto
      originDto
    ) = do
    publication <- publicationIdFromDto publicationDto
    occurrence <- sortOccurrenceFromClaim occurrenceClaim
    canonical <- canonicalValueFromBytes canonicalBytes
    let strength = replicaStrengthFromDto strengthDto
    case originDto of
      Protocol.PrimordialRetainedObservationDto ->
        Right
          ( Alignment.primordialRetainedPublicationEvidence
              publication
              sortId
              occurrence
              canonical
              strength
          )
      Protocol.RoutedRetainedObservationDto
        sourceClaim
        topologyClaim
        controlDto
        stampDto -> do
          source <- processEpochFromClaim sourceClaim
          topology <- topologyCutFromClaim topologyClaim
          stamp <- traverse structuralOccurrenceStampFromDto stampDto
          bridgeClaim
            ( Alignment.retainedPublicationEvidence
                publication
                source
                sortId
                occurrence
                canonical
                strength
                topology
                (controlIndexFromDto controlDto)
                stamp
            )

retainedPublicationEvidenceToDto ::
  Alignment.RetainedPublicationEvidence ->
  Either PeerRpcBridgeError Protocol.RetainedPublicationEvidenceDto
retainedPublicationEvidenceToDto evidence = do
  let canonicalBytes =
        Value.canonicalValueByteString
          (Alignment.retainedPublicationEvidenceCanonicalValue evidence)
  _ <- canonicalValueFromOwnerBytes canonicalBytes
  publication <-
    publicationIdToDto
      (Alignment.retainedPublicationEvidencePublicationId evidence)
  occurrence <-
    sortOccurrenceToClaim
      (Alignment.retainedPublicationEvidenceSortDefinitionOccurrenceId evidence)
  origin <- case Alignment.retainedPublicationEvidenceOrigin evidence of
    Alignment.PrimordialRetainedObservation ->
      Right Protocol.PrimordialRetainedObservationDto
    Alignment.RoutedRetainedObservation source topology control stamp ->
      Protocol.RoutedRetainedObservationDto
        <$> processEpochToClaim source
        <*> topologyCutToClaim topology
        <*> pure (controlIndexToDto control)
        <*> traverse structuralOccurrenceStampToDto stamp
  Right
    ( Protocol.RetainedPublicationEvidenceDto
        publication
        (Alignment.retainedPublicationEvidenceSortId evidence)
        occurrence
        canonicalBytes
        ( replicaStrengthToDto
            (Alignment.retainedPublicationEvidenceSourceStrength evidence)
        )
        origin
    )

retainedStateEvidenceFromDto ::
  Protocol.RetainedStateEvidenceDto ->
  Either PeerRpcBridgeError Alignment.RetainedStateEvidence
retainedStateEvidenceFromDto
  (Protocol.RetainedStateEvidenceDto representativeDto witnessDto strengthDto) = do
    representative <- retainedPublicationEvidenceFromDto representativeDto
    witness <- retainedPublicationEvidenceFromDto witnessDto
    bridgeClaim
      ( Alignment.retainedStateEvidence
          representative
          witness
          (replicaStrengthFromDto strengthDto)
      )

retainedStateEvidenceToDto ::
  Alignment.RetainedStateEvidence ->
  Either PeerRpcBridgeError Protocol.RetainedStateEvidenceDto
retainedStateEvidenceToDto fact =
  Protocol.RetainedStateEvidenceDto
    <$> retainedPublicationEvidenceToDto
      (Alignment.retainedStateEvidenceRepresentative fact)
    <*> retainedPublicationEvidenceToDto
      (Alignment.retainedStateEvidenceStrengthWitness fact)
    <*> pure
      (replicaStrengthToDto (Alignment.retainedStateEvidenceJoinedStrength fact))

alignmentObligationIdFromDto ::
  Protocol.AlignmentObligationIdDto ->
  Either PeerRpcBridgeError Alignment.AlignmentObligationId
alignmentObligationIdFromDto
  (Protocol.AlignmentObligationIdDto heraldClaim sequenceDto) =
    Alignment.alignmentObligationId
      <$> heraldEpochFromClaim heraldClaim
      <*> alignmentObligationSequenceFromDto sequenceDto

alignmentObligationIdToDto ::
  Alignment.AlignmentObligationId ->
  Either PeerRpcBridgeError Protocol.AlignmentObligationIdDto
alignmentObligationIdToDto identifier =
  Protocol.AlignmentObligationIdDto
    <$> heraldEpochToClaim
      (Alignment.alignmentObligationIdDestinationHerald identifier)
    <*> alignmentObligationSequenceToDto
      (Alignment.alignmentObligationIdSequence identifier)

alignmentSubscriptionIdFromDto ::
  Protocol.AlignmentSubscriptionIdDto ->
  Either PeerRpcBridgeError Alignment.AlignmentSubscriptionId
alignmentSubscriptionIdFromDto
  (Protocol.AlignmentSubscriptionIdDto heraldClaim sequenceDto) =
    Alignment.alignmentSubscriptionId
      <$> heraldEpochFromClaim heraldClaim
      <*> alignmentSubscriptionSequenceFromDto sequenceDto

alignmentSubscriptionIdToDto ::
  Alignment.AlignmentSubscriptionId ->
  Either PeerRpcBridgeError Protocol.AlignmentSubscriptionIdDto
alignmentSubscriptionIdToDto identifier =
  Protocol.AlignmentSubscriptionIdDto
    <$> heraldEpochToClaim
      (Alignment.alignmentSubscriptionIdDestinationHerald identifier)
    <*> alignmentSubscriptionSequenceToDto
      (Alignment.alignmentSubscriptionIdSequence identifier)

alignmentObligationSequenceFromDto ::
  Protocol.PositiveAlignmentObligationSequenceDto ->
  Either PeerRpcBridgeError Alignment.AlignmentObligationSequence
alignmentObligationSequenceFromDto =
  bridgeClaim
    . Alignment.mkAlignmentObligationSequence
    . Protocol.positiveAlignmentObligationSequenceDtoWord64

alignmentObligationSequenceToDto ::
  Alignment.AlignmentObligationSequence ->
  Either PeerRpcBridgeError Protocol.PositiveAlignmentObligationSequenceDto
alignmentObligationSequenceToDto =
  bridgeClaim
    . Protocol.positiveAlignmentObligationSequenceDto
    . Alignment.alignmentObligationSequenceWord64

alignmentSubscriptionSequenceFromDto ::
  Protocol.PositiveAlignmentSubscriptionSequenceDto ->
  Either PeerRpcBridgeError Alignment.AlignmentSubscriptionSequence
alignmentSubscriptionSequenceFromDto =
  bridgeClaim
    . Alignment.mkAlignmentSubscriptionSequence
    . Protocol.positiveAlignmentSubscriptionSequenceDtoWord64

alignmentSubscriptionSequenceToDto ::
  Alignment.AlignmentSubscriptionSequence ->
  Either PeerRpcBridgeError Protocol.PositiveAlignmentSubscriptionSequenceDto
alignmentSubscriptionSequenceToDto =
  bridgeClaim
    . Protocol.positiveAlignmentSubscriptionSequenceDto
    . Alignment.alignmentSubscriptionSequenceWord64

alignmentEvidenceSequenceFromDto ::
  Protocol.PositiveAlignmentEvidenceSequenceDto ->
  Either PeerRpcBridgeError Alignment.AlignmentEvidenceSequence
alignmentEvidenceSequenceFromDto =
  bridgeClaim
    . Alignment.mkAlignmentEvidenceSequence
    . Protocol.positiveAlignmentEvidenceSequenceDtoWord64

alignmentEvidenceSequenceToDto ::
  Alignment.AlignmentEvidenceSequence ->
  Either PeerRpcBridgeError Protocol.PositiveAlignmentEvidenceSequenceDto
alignmentEvidenceSequenceToDto =
  bridgeClaim
    . Protocol.positiveAlignmentEvidenceSequenceDto
    . Alignment.alignmentEvidenceSequenceWord64

storeRevisionFromDto :: Protocol.StoreRevisionDto -> DomainAlignment.StoreRevision
storeRevisionFromDto =
  DomainAlignment.storeRevisionFromWord64 . Protocol.storeRevisionDtoWord64

storeRevisionToDto :: DomainAlignment.StoreRevision -> Protocol.StoreRevisionDto
storeRevisionToDto =
  Protocol.storeRevisionDto . DomainAlignment.storeRevisionWord64

heraldPublicationPrefixFromDto ::
  Protocol.HeraldPublicationPrefixDto ->
  Either PeerRpcBridgeError DomainAlignment.HeraldPublicationPrefix
heraldPublicationPrefixFromDto = \case
  Protocol.EmptyHeraldPublicationPrefixDto ->
    Right DomainAlignment.EmptyHeraldPublicationPrefix
  Protocol.HeraldPublicationPrefixThroughDto positionDto ->
    DomainAlignment.HeraldPublicationPrefixThrough
      <$> bridgeClaim
        ( DomainAlignment.mkHeraldPublicationPosition
            (Protocol.positiveHeraldPublicationPositionDtoWord64 positionDto)
        )

heraldPublicationPrefixToDto ::
  DomainAlignment.HeraldPublicationPrefix ->
  Either PeerRpcBridgeError Protocol.HeraldPublicationPrefixDto
heraldPublicationPrefixToDto = \case
  DomainAlignment.EmptyHeraldPublicationPrefix ->
    Right Protocol.EmptyHeraldPublicationPrefixDto
  DomainAlignment.HeraldPublicationPrefixThrough position ->
    Protocol.HeraldPublicationPrefixThroughDto
      <$> bridgeClaim
        ( Protocol.positiveHeraldPublicationPositionDto
            (DomainAlignment.heraldPublicationPositionWord64 position)
        )

publicationFromDto ::
  Protocol.PeerStreamItemDto ->
  Either PeerRpcBridgeError PeerLogicalItem
publicationFromDto dto = do
  (directionDto, sequenceDto, digestClaim, logicalPayload) <- case dto of
    Protocol.PeerPublicationDto direction sequenceNumber digest itemDto ->
      (direction,sequenceNumber,digest,)
        . Payload.PeerLogicalPublication
        <$> peerPublicationFromDto itemDto
    Protocol.PeerRouteCutoverDto direction sequenceNumber digest markerDto ->
      (direction,sequenceNumber,digest,)
        . Payload.PeerLogicalRouteCutover
        <$> alignmentRouteCutoverMarkerFromDto markerDto
    Protocol.PeerDisappearanceProbeMarkerDto direction sequenceNumber digest markerDto ->
      (direction,sequenceNumber,digest,)
        . Payload.PeerLogicalDisappearanceProbeMarker
        <$> disappearanceProbeMarkerFromDto markerDto
  direction <- streamDirectionFromDto directionDto
  sequenceNumber <- streamSequenceFromDto sequenceDto
  carriedDigest <- peerItemDigestFromClaim digestClaim
  if not (logicalPayloadCoordinateMatches direction sequenceNumber logicalPayload)
    then Left PeerRpcBridgeDisappearanceMarkerCoordinateMismatch
    else
      if carriedDigest /= Payload.peerLogicalPayloadDigest logicalPayload
        then Left PeerRpcBridgePublicationDigestMismatch
        else
          pure
            ( PeerDispatch.peerLogicalItem
                ( Stream.sequencedItem
                    direction
                    sequenceNumber
                    carriedDigest
                    logicalPayload
                )
            )

disappearanceProbeFromDto ::
  Protocol.DisappearanceProbeIdDto ->
  Either PeerRpcBridgeError DomainDisappearance.DisappearanceProbeId
disappearanceProbeFromDto probe =
  bridgeClaim
    ( DomainDisappearance.admitDisappearanceProbeId
        (controlIndexFromDto (Protocol.disappearanceProbeIdControlIndex probe))
        (Protocol.disappearanceProbeDigestClaimBytes (Protocol.disappearanceProbeIdDigest probe))
    )

disappearanceProbeToDto ::
  DomainDisappearance.DisappearanceProbeId ->
  Either PeerRpcBridgeError Protocol.DisappearanceProbeIdDto
disappearanceProbeToDto probe = do
  digest <-
    bridgeClaim
      (Protocol.disappearanceProbeDigestClaim (DomainDisappearance.disappearanceProbeIdBytes probe))
  bridgeClaim
    ( Protocol.disappearanceProbeIdDto
        digest
        (controlIndexToDto (DomainDisappearance.disappearanceProbeOpenControlIndex probe))
    )

disappearanceProbeMarkerFromDto ::
  Protocol.DisappearanceProbeMarkerDto ->
  Either PeerRpcBridgeError Disappearance.DisappearancePublicationMarker
disappearanceProbeMarkerFromDto
  ( Protocol.DisappearanceProbeMarkerDto
      probeDto
      (Protocol.DisappearanceCoordinateDto subjectClaim generationClaim memberClaim)
      directionDto
      sequenceDto
    ) = do
    probe <- disappearanceProbeFromDto probeDto
    subject <-
      bridgeClaim
        ( DomainDisappearance.mkDisappearanceSubjectDigest
            (Protocol.disappearanceSubjectDigestClaimBytes subjectClaim)
        )
    generation <- membershipGenerationFromClaim generationClaim
    members <- memberSetDigestFromClaim memberClaim
    direction <- streamDirectionFromDto directionDto
    sequenceNumber <- streamSequenceFromDto sequenceDto
    pure
      ( Disappearance.disappearancePublicationMarker
          probe
          (DomainDisappearance.admitDisappearanceSubjectMembershipCoordinate subject generation members)
          direction
          sequenceNumber
      )

disappearanceProbeMarkerToDto ::
  Disappearance.DisappearancePublicationMarker ->
  Either PeerRpcBridgeError Protocol.DisappearanceProbeMarkerDto
disappearanceProbeMarkerToDto marker = do
  let coordinate = Disappearance.disappearancePublicationMarkerCoordinate marker
  subject <-
    bridgeClaim
      ( Protocol.disappearanceSubjectDigestClaim
          ( DomainDisappearance.disappearanceSubjectDigestBytes
              (DomainDisappearance.disappearanceCoordinateSubjectDigest coordinate)
          )
      )
  generation <-
    membershipGenerationToClaim
      (DomainDisappearance.disappearanceCoordinateMembershipGenerationId coordinate)
  members <-
    memberSetDigestToClaim
      (DomainDisappearance.disappearanceCoordinateMemberSetDigest coordinate)
  Protocol.DisappearanceProbeMarkerDto
    <$> disappearanceProbeToDto (Disappearance.disappearancePublicationMarkerProbe marker)
    <*> pure (Protocol.DisappearanceCoordinateDto subject generation members)
    <*> streamDirectionToDto (Disappearance.disappearancePublicationMarkerDirection marker)
    <*> streamSequenceToDto (Disappearance.disappearancePublicationMarkerSequence marker)

alignmentRouteCutoverMarkerFromDto ::
  Protocol.AlignmentRouteCutoverMarkerDto ->
  Either PeerRpcBridgeError Alignment.AlignmentRouteCutoverMarker
alignmentRouteCutoverMarkerFromDto
  ( Protocol.AlignmentRouteCutoverMarkerDto
      identifierDto
      generationClaim
      sourceClaim
      predecessorClaim
    ) = do
    identifier <- alignmentPlanIdFromDto identifierDto
    generation <- contextClassGenerationFromClaim generationClaim
    source <- heraldEpochFromClaim sourceClaim
    predecessor <- heraldEpochFromClaim predecessorClaim
    bridgeClaim
      (Alignment.alignmentRouteCutoverMarker identifier generation source predecessor)

alignmentRouteCutoverMarkerToDto ::
  Alignment.AlignmentRouteCutoverMarker ->
  Either PeerRpcBridgeError Protocol.AlignmentRouteCutoverMarkerDto
alignmentRouteCutoverMarkerToDto marker =
  Protocol.AlignmentRouteCutoverMarkerDto
    <$> alignmentPlanIdToDto (Alignment.alignmentRouteCutoverMarkerPlan marker)
    <*> contextClassGenerationToClaim
      (Alignment.alignmentRouteCutoverMarkerGeneration marker)
    <*> heraldEpochToClaim
      (Alignment.alignmentRouteCutoverMarkerSourceHerald marker)
    <*> heraldEpochToClaim
      (Alignment.alignmentRouteCutoverMarkerPredecessorHerald marker)

logicalAttemptToDto ::
  PeerLogicalAttempt ->
  Either PeerRpcBridgeError Protocol.PeerStreamItemDto
logicalAttemptToDto attempt = do
  let item = Stream.peerDispatchAttemptItem attempt
      payload = Stream.sequencedItemPayload item
      itemDigest = Stream.sequencedItemDigest item
  if itemDigest /= Payload.peerLogicalPayloadDigest payload
    then Left PeerRpcBridgeOwnerPublicationContradiction
    else do
      direction <- streamDirectionToDto (Stream.sequencedItemDirection item)
      sequenceNumber <- streamSequenceToDto (Stream.sequencedItemSequence item)
      digest <- peerItemDigestToClaim itemDigest
      case payload of
        Payload.PeerLogicalPublication publication ->
          Protocol.PeerPublicationDto direction sequenceNumber digest
            <$> peerPublicationToDto publication
        Payload.PeerLogicalRouteCutover marker ->
          Protocol.PeerRouteCutoverDto direction sequenceNumber digest
            <$> alignmentRouteCutoverMarkerToDto marker
        Payload.PeerLogicalDisappearanceProbeMarker marker
          | logicalPayloadCoordinateMatches
              (Stream.sequencedItemDirection item)
              (Stream.sequencedItemSequence item)
              payload ->
              Protocol.PeerDisappearanceProbeMarkerDto direction sequenceNumber digest
                <$> disappearanceProbeMarkerToDto marker
          | otherwise -> Left PeerRpcBridgeOwnerPublicationContradiction

logicalPayloadCoordinateMatches ::
  Stream.StreamDirection ->
  Stream.StreamSequence ->
  Payload.PeerLogicalPayload ->
  Bool
logicalPayloadCoordinateMatches direction sequenceNumber = \case
  Payload.PeerLogicalDisappearanceProbeMarker marker ->
    Disappearance.disappearancePublicationMarkerDirection marker == direction
      && Disappearance.disappearancePublicationMarkerSequence marker == sequenceNumber
  Payload.PeerLogicalPublication _ -> True
  Payload.PeerLogicalRouteCutover _ -> True

knownHeraldFromDto ::
  Protocol.KnownHeraldDto ->
  Either PeerRpcBridgeError Discovery.KnownHerald
knownHeraldFromDto dto =
  Discovery.knownHerald
    <$> heraldIdFromClaim (Protocol.knownHeraldId dto)
    <*> heraldEpochFromClaim (Protocol.knownHeraldEpoch dto)
    <*> pure
      ( Set.fromList
          (fmap addressFromDto (Protocol.knownHeraldAddresses dto))
      )

knownHeraldToDto ::
  Discovery.KnownHerald ->
  Either PeerRpcBridgeError Protocol.KnownHeraldDto
knownHeraldToDto known =
  Protocol.knownHeraldDto
    <$> heraldIdToClaim (Discovery.knownHeraldId known)
    <*> heraldEpochToClaim (Discovery.knownHeraldObservedEpoch known)
    <*> pure
      ( fmap
          addressToDto
          (Set.toAscList (Discovery.knownHeraldAddresses known))
      )

placementUpdateFromDto ::
  Protocol.PlacementUpdateDto ->
  Either PeerRpcBridgeError Placement.PlacementUpdate
placementUpdateFromDto = \case
  Protocol.FullPlacementSnapshotDto snapshot ->
    Placement.FullPlacementSnapshot <$> placementSnapshotFromDto snapshot

placementUpdateToDto ::
  Placement.PlacementUpdate ->
  Either PeerRpcBridgeError Protocol.PlacementUpdateDto
placementUpdateToDto = \case
  Placement.FullPlacementSnapshot snapshot ->
    Protocol.FullPlacementSnapshotDto <$> placementSnapshotToDto snapshot

placementSnapshotFromDto ::
  Protocol.PlacementSnapshotDto ->
  Either PeerRpcBridgeError Placement.PlacementSnapshot
placementSnapshotFromDto dto = do
  owner <- heraldEpochFromClaim (Protocol.placementSnapshotOwner dto)
  sequenceNumber <- placementSequenceFromDto (Protocol.placementSnapshotSequence dto)
  routes <- traverse deltaRouteFromDto (Protocol.placementSnapshotRoutes dto)
  bridgeClaim (Placement.placementSnapshot owner sequenceNumber routes)

placementSnapshotToDto ::
  Placement.PlacementSnapshot ->
  Either PeerRpcBridgeError Protocol.PlacementSnapshotDto
placementSnapshotToDto snapshot = do
  owner <- heraldEpochToClaim (Placement.placementSnapshotOwner snapshot)
  sequenceNumber <- placementSequenceToDto (Placement.placementSnapshotSequence snapshot)
  routes <- traverse deltaRouteToDto (Placement.placementSnapshotRoutes snapshot)
  bridgeClaim (Protocol.placementSnapshotDto owner sequenceNumber routes)

placementAcknowledgementFromDto ::
  Protocol.PlacementAcknowledgementDto ->
  Either PeerRpcBridgeError Placement.PlacementAcknowledgement
placementAcknowledgementFromDto
  (Protocol.PlacementAcknowledgementDto ownerClaim sequenceDto) =
    Placement.placementAcknowledgement
      <$> heraldEpochFromClaim ownerClaim
      <*> placementSequenceFromDto sequenceDto

placementAcknowledgementToDto ::
  Placement.PlacementAcknowledgement ->
  Either PeerRpcBridgeError Protocol.PlacementAcknowledgementDto
placementAcknowledgementToDto acknowledgement =
  Protocol.PlacementAcknowledgementDto
    <$> heraldEpochToClaim (Placement.placementAcknowledgementOwner acknowledgement)
    <*> placementSequenceToDto
      (Placement.placementAcknowledgementSequence acknowledgement)

deltaRouteFromDto ::
  Protocol.DeltaRouteDto ->
  Either PeerRpcBridgeError Placement.DeltaRoute
deltaRouteFromDto = \case
  Protocol.ApplicationDeltaRouteDto
    deltaClaim
    sortId
    occurrenceClaim
    controllerClaim
    processClaim
    incarnationClaim
    controlDto ->
      Placement.applicationDeltaRoute
        <$> deltaFromClaim deltaClaim
        <*> pure sortId
        <*> sortOccurrenceFromClaim occurrenceClaim
        <*> globalObjectFromClaim controllerClaim
        <*> processEpochFromClaim processClaim
        <*> storeIncarnationFromClaim incarnationClaim
        <*> pure (controlIndexFromDto controlDto)
  Protocol.PrivateSystemViewDeltaRouteDto
    role
    deltaClaim
    sortId
    occurrenceClaim
    incarnationClaim ->
      Placement.privateSystemViewDeltaRoute
        (predefinedSortRoleFromDto role)
        <$> deltaFromClaim deltaClaim
        <*> pure sortId
        <*> sortOccurrenceFromClaim occurrenceClaim
        <*> storeIncarnationFromClaim incarnationClaim

deltaRouteToDto ::
  Placement.DeltaRoute ->
  Either PeerRpcBridgeError Protocol.DeltaRouteDto
deltaRouteToDto route = case Placement.viewDeltaRoute route of
  Placement.ApplicationDeltaRouteView
    delta
    sortId
    occurrence
    controller
    process
    incarnation
    control ->
      Protocol.ApplicationDeltaRouteDto
        <$> deltaToClaim delta
        <*> pure sortId
        <*> sortOccurrenceToClaim occurrence
        <*> globalObjectToClaim controller
        <*> processEpochToClaim process
        <*> storeIncarnationToClaim incarnation
        <*> pure (controlIndexToDto control)
  Placement.PrivateSystemViewDeltaRouteView role delta sortId occurrence incarnation ->
    Protocol.PrivateSystemViewDeltaRouteDto
      (predefinedSortRoleToDto role)
      <$> deltaToClaim delta
      <*> pure sortId
      <*> sortOccurrenceToClaim occurrence
      <*> storeIncarnationToClaim incarnation

streamDirectionFromDto ::
  Protocol.StreamDirectionDto ->
  Either PeerRpcBridgeError Stream.StreamDirection
streamDirectionFromDto dto = do
  source <- heraldEpochFromClaim (Protocol.streamDirectionSource dto)
  destination <- heraldEpochFromClaim (Protocol.streamDirectionDestination dto)
  bridgeClaim (Stream.mkStreamDirection source destination)

streamDirectionToDto ::
  Stream.StreamDirection ->
  Either PeerRpcBridgeError Protocol.StreamDirectionDto
streamDirectionToDto direction = do
  source <- heraldEpochToClaim (Stream.streamDirectionSource direction)
  destination <- heraldEpochToClaim (Stream.streamDirectionDestination direction)
  bridgeClaim (Protocol.streamDirectionDto source destination)

streamPrefixFromDto ::
  Protocol.StreamPrefixDto ->
  Either PeerRpcBridgeError Stream.StreamPrefix
streamPrefixFromDto = \case
  Protocol.EmptyStreamPrefixDto -> pure Stream.emptyStreamPrefix
  Protocol.StreamPrefixThroughDto sequenceDto ->
    Stream.streamPrefixThrough <$> streamSequenceFromDto sequenceDto

streamPrefixToDto ::
  Stream.StreamPrefix ->
  Either PeerRpcBridgeError Protocol.StreamPrefixDto
streamPrefixToDto prefix = case Stream.streamPrefixSequence prefix of
  Nothing -> pure Protocol.EmptyStreamPrefixDto
  Just sequenceNumber ->
    Protocol.StreamPrefixThroughDto <$> streamSequenceToDto sequenceNumber

gapSummaryFromDto ::
  Protocol.GapSummaryDto ->
  Either PeerRpcBridgeError Stream.GapSummary
gapSummaryFromDto dto = do
  direction <- streamDirectionFromDto (Protocol.gapSummaryDirection dto)
  received <- streamPrefixFromDto (Protocol.gapSummaryReceivedPrefix dto)
  entries <- traverse gapEntryFromDto (Protocol.gapSummaryEntries dto)
  bridgeClaim (Stream.mkGapSummary direction received entries)

gapSummaryToDto ::
  Stream.GapSummary ->
  Either PeerRpcBridgeError Protocol.GapSummaryDto
gapSummaryToDto gap = do
  direction <- streamDirectionToDto (Stream.gapSummaryDirection gap)
  received <- streamPrefixToDto (Stream.gapSummaryReceivedPrefix gap)
  entries <- traverse gapEntryToDto (Stream.gapSummaryEntries gap)
  bridgeClaim (Protocol.gapSummaryDto direction received entries)

gapEntryFromDto ::
  (Protocol.PositiveStreamSequenceDto, Protocol.PeerItemDigestClaim) ->
  Either PeerRpcBridgeError (Stream.StreamSequence, Stream.PeerItemDigest)
gapEntryFromDto (sequenceDto, digestClaim) =
  (,)
    <$> streamSequenceFromDto sequenceDto
    <*> peerItemDigestFromClaim digestClaim

gapEntryToDto ::
  (Stream.StreamSequence, Stream.PeerItemDigest) ->
  Either
    PeerRpcBridgeError
    (Protocol.PositiveStreamSequenceDto, Protocol.PeerItemDigestClaim)
gapEntryToDto (sequenceNumber, digest) =
  (,)
    <$> streamSequenceToDto sequenceNumber
    <*> peerItemDigestToClaim digest

resumeOfferFromDto ::
  Protocol.ResumeOfferDto ->
  Either PeerRpcBridgeError Stream.ResumeOffer
resumeOfferFromDto dto = do
  source <- heraldEpochFromClaim (Protocol.resumeOfferSource dto)
  destination <- heraldEpochFromClaim (Protocol.resumeOfferDestination dto)
  nextSource <- streamSequenceFromDto (Protocol.resumeOfferNextSourceSequence dto)
  received <- streamPrefixFromDto (Protocol.resumeOfferReceivedPrefix dto)
  let completed = Protocol.resumeOfferCompletion dto
  gap <- gapSummaryFromDto (Protocol.resumeOfferGapSummary dto)
  bridgeClaim
    (Stream.mkResumeOfferWithCompletion source destination nextSource received completed gap)

resumeOfferToDto ::
  Stream.ResumeOffer ->
  Either PeerRpcBridgeError Protocol.ResumeOfferDto
resumeOfferToDto offer = do
  source <- heraldEpochToClaim (Stream.resumeOfferSource offer)
  destination <- heraldEpochToClaim (Stream.resumeOfferDestination offer)
  nextSource <- streamSequenceToDto (Stream.resumeOfferNextSourceSequence offer)
  received <- streamPrefixToDto (Stream.resumeOfferReceivedPrefix offer)
  let completed = Stream.resumeOfferCompletion offer
  gap <- gapSummaryToDto (Stream.resumeOfferGapSummary offer)
  bridgeClaim
    (Protocol.resumeOfferWithCompletionDto source destination nextSource received completed gap)

resumeResponseFromDto ::
  Protocol.ResumeResponseDto ->
  Either PeerRpcBridgeError Stream.ResumeResponse
resumeResponseFromDto dto =
  Stream.resumeResponseWithCompletion
    <$> streamPrefixFromDto (Protocol.resumeResponseReceivedPrefix dto)
    <*> pure (Protocol.resumeResponseCompletion dto)
    <*> streamSequenceFromDto (Protocol.resumeResponseRetransmitFrom dto)

resumeResponseToDto ::
  Stream.ResumeResponse ->
  Either PeerRpcBridgeError Protocol.ResumeResponseDto
resumeResponseToDto response = do
  received <- streamPrefixToDto (Stream.resumeResponseReceivedPrefix response)
  let completed = Stream.resumeResponseCompletion response
  retransmitFrom <- streamSequenceToDto (Stream.resumeResponseRetransmitFrom response)
  bridgeClaim (Protocol.resumeResponseWithCompletionDto received completed retransmitFrom)

peerPublicationFromDto ::
  Protocol.PeerPublicationItemDto ->
  Either PeerRpcBridgeError Publication.PeerPublication
peerPublicationFromDto = \case
  Protocol.OrdinaryPublicationDto batchDto ->
    Publication.presentedOrdinaryPeerPublication
      <$> publicationBatchFromDto batchDto
  Protocol.StructuralPublicationDto stampDto batchDto -> do
    stamp <- structuralOccurrenceStampFromDto stampDto
    batch <- publicationBatchFromDto batchDto
    bridgeClaim (Publication.presentedStructuralPeerPublication stamp batch)

peerPublicationToDto ::
  Publication.PeerPublication ->
  Either PeerRpcBridgeError Protocol.PeerPublicationItemDto
peerPublicationToDto publication =
  case Publication.peerPublicationStructuralStamp publication of
    Nothing ->
      Protocol.OrdinaryPublicationDto
        <$> publicationBatchToDto (Publication.peerPublicationBatch publication)
    Just stamp ->
      Protocol.StructuralPublicationDto
        <$> structuralOccurrenceStampToDto stamp
        <*> publicationBatchToDto (Publication.peerPublicationBatch publication)

publicationBatchFromDto ::
  Protocol.PublicationBatchDto ->
  Either PeerRpcBridgeError Publication.PublicationBatch
publicationBatchFromDto
  ( Protocol.PublicationBatchDto
      identifierDto
      sourceClaim
      sortId
      occurrenceClaim
      canonicalBytes
      sourceStrengthDto
      topologyClaim
      controlDto
      destinationsDto
    ) = do
    identifier <- publicationIdFromDto identifierDto
    source <- processEpochFromClaim sourceClaim
    occurrence <- sortOccurrenceFromClaim occurrenceClaim
    sourceTopology <- topologyCutFromClaim topologyClaim
    canonicalValue <- canonicalValueFromBytes canonicalBytes
    destinations <-
      traverse
        publicationDestinationFromDto
        (Protocol.publicationDestinationEntries destinationsDto)
    bridgeClaim
      ( Publication.mkPublicationBatch
          identifier
          source
          sortId
          occurrence
          canonicalValue
          (replicaStrengthFromDto sourceStrengthDto)
          sourceTopology
          (controlIndexFromDto controlDto)
          destinations
      )

publicationBatchToDto ::
  Publication.PublicationBatch ->
  Either PeerRpcBridgeError Protocol.PublicationBatchDto
publicationBatchToDto batch = do
  identifier <- publicationIdToDto (Publication.publicationBatchId batch)
  source <- processEpochToClaim (Publication.publicationBatchSourceProcess batch)
  occurrence <- sortOccurrenceToClaim (Publication.publicationBatchOccurrenceId batch)
  sourceTopology <-
    topologyCutToClaim
      (Publication.publicationBatchSourceTopologyPrerequisite batch)
  destinations <-
    traverse
      publicationDestinationToDto
      (Publication.publicationBatchDestinations batch)
  checkedDestinations <-
    bridgeOwnerPublication
      (Protocol.publicationDestinationsDto (NonEmpty.toList destinations))
  let canonicalBytes =
        Value.canonicalValueByteString
          (Publication.publicationBatchCanonicalValue batch)
  _ <- canonicalValueFromOwnerBytes canonicalBytes
  pure
    ( Protocol.PublicationBatchDto
        identifier
        source
        (Publication.publicationBatchSortId batch)
        occurrence
        canonicalBytes
        (replicaStrengthToDto (Publication.publicationBatchSourceStrength batch))
        sourceTopology
        (controlIndexToDto (Publication.publicationBatchControlPrerequisite batch))
        checkedDestinations
    )

structuralOccurrenceStampFromDto ::
  Protocol.StructuralOccurrenceStampDto ->
  Either PeerRpcBridgeError Publication.StructuralOccurrenceStamp
structuralOccurrenceStampFromDto
  ( Protocol.StructuralOccurrenceStampDto
      occurrenceDto
      predecessorDto
      publicationDto
      digestClaim
      carrierRoleDto
    ) = do
    occurrence <- structuralOccurrenceIdFromDto occurrenceDto
    predecessor <- structuralVersionVectorFromDto predecessorDto
    publication <- publicationIdFromDto publicationDto
    publicationDigest <- structuralPublicationDigestFromClaim digestClaim
    bridgeClaim
      ( Publication.mkStructuralOccurrenceStamp
          occurrence
          predecessor
          publication
          publicationDigest
          (structuralCarrierRoleFromDto carrierRoleDto)
      )

structuralOccurrenceStampToDto ::
  Publication.StructuralOccurrenceStamp ->
  Either PeerRpcBridgeError Protocol.StructuralOccurrenceStampDto
structuralOccurrenceStampToDto stamp =
  Protocol.StructuralOccurrenceStampDto
    <$> structuralOccurrenceIdToDto
      (Publication.structuralOccurrenceStampOccurrence stamp)
    <*> structuralVersionVectorToDto
      (Publication.structuralOccurrenceStampPredecessor stamp)
    <*> publicationIdToDto
      (Publication.structuralOccurrenceStampPublication stamp)
    <*> structuralPublicationDigestToClaim
      (Publication.structuralOccurrenceStampPublicationDigest stamp)
    <*> pure
      ( structuralCarrierRoleToDto
          (Publication.structuralOccurrenceStampCarrierRole stamp)
      )

publicationIdFromDto ::
  Protocol.PublicationIdDto ->
  Either PeerRpcBridgeError Identity.PublicationId
publicationIdFromDto
  (Protocol.PublicationIdDto nablaClaim authorityDto sequenceDto sourceClaim) = do
    nabla <- nablaFromClaim nablaClaim
    authority <- authorityEpochFromDto authorityDto
    source <- heraldEpochFromClaim sourceClaim
    pure
      ( Identity.publicationId
          nabla
          authority
          source
          (nablaSequenceFromDto sequenceDto)
      )

publicationIdToDto ::
  Identity.PublicationId ->
  Either PeerRpcBridgeError Protocol.PublicationIdDto
publicationIdToDto identifier =
  Protocol.PublicationIdDto
    <$> nablaToClaim (Identity.publicationNabla identifier)
    <*> authorityEpochToDto (Identity.publicationAuthorityEpoch identifier)
    <*> pure (nablaSequenceToDto (Identity.publicationNablaSequence identifier))
    <*> heraldEpochToClaim (Identity.publicationSourceHeraldEpoch identifier)

publicationDestinationFromDto ::
  Protocol.PublicationDestinationDto ->
  Either PeerRpcBridgeError Publication.PublicationDestination
publicationDestinationFromDto
  (Protocol.PublicationDestinationDto deltaClaim incarnationClaim strengthDto) =
    Publication.publicationDestination
      <$> deltaFromClaim deltaClaim
      <*> storeIncarnationFromClaim incarnationClaim
      <*> pure (replicaStrengthFromDto strengthDto)

publicationDestinationToDto ::
  Publication.PublicationDestination ->
  Either PeerRpcBridgeError Protocol.PublicationDestinationDto
publicationDestinationToDto destination =
  Protocol.PublicationDestinationDto
    <$> deltaToClaim (Publication.publicationDestinationDelta destination)
    <*> storeIncarnationToClaim
      (Publication.publicationDestinationStoreIncarnation destination)
    <*> pure
      (replicaStrengthToDto (Publication.publicationDestinationStrength destination))

canonicalValueFromBytes ::
  ByteString ->
  Either PeerRpcBridgeError Value.CanonicalValueBytes
canonicalValueFromBytes bytes =
  case Value.decodeCanonicalValue bytes of
    Left _ -> Left PeerRpcBridgeCanonicalValueContradiction
    Right value ->
      let canonical = Value.canonicalValueBytes value
       in if Value.canonicalValueByteString canonical == bytes
            then Right canonical
            else Left PeerRpcBridgeCanonicalValueContradiction

canonicalValueFromOwnerBytes ::
  ByteString ->
  Either PeerRpcBridgeError Value.CanonicalValueBytes
canonicalValueFromOwnerBytes bytes =
  case canonicalValueFromBytes bytes of
    Left _ -> Left PeerRpcBridgeOwnerPublicationContradiction
    Right canonical -> Right canonical

predefinedSortRoleFromDto :: Protocol.PredefinedSortRoleDto -> Startup.PredefinedSortRole
predefinedSortRoleFromDto = \case
  Protocol.SortDefinitionRoleDto -> Startup.SortDefinitionRole
  Protocol.NeutralVertexRoleDto -> Startup.NeutralVertexRole
  Protocol.EdgeRoleDto -> Startup.EdgeRole
  Protocol.NablaRoleDto -> Startup.NablaRole
  Protocol.DeltaRoleDto -> Startup.DeltaRole
  Protocol.ProcessEpochRoleDto -> Startup.ProcessEpochRole

predefinedSortRoleToDto :: Startup.PredefinedSortRole -> Protocol.PredefinedSortRoleDto
predefinedSortRoleToDto = \case
  Startup.SortDefinitionRole -> Protocol.SortDefinitionRoleDto
  Startup.NeutralVertexRole -> Protocol.NeutralVertexRoleDto
  Startup.EdgeRole -> Protocol.EdgeRoleDto
  Startup.NablaRole -> Protocol.NablaRoleDto
  Startup.DeltaRole -> Protocol.DeltaRoleDto
  Startup.ProcessEpochRole -> Protocol.ProcessEpochRoleDto

structuralCarrierRoleFromDto ::
  Protocol.StructuralCarrierRoleDto -> Descriptor.StructuralCarrierRole
structuralCarrierRoleFromDto = \case
  Protocol.NeutralVertexCarrierDto -> Descriptor.NeutralVertexCarrier
  Protocol.EdgeCarrierDto -> Descriptor.EdgeCarrier
  Protocol.NablaCarrierDto -> Descriptor.NablaCarrier
  Protocol.DeltaCarrierDto -> Descriptor.DeltaCarrier

structuralCarrierRoleToDto ::
  Descriptor.StructuralCarrierRole -> Protocol.StructuralCarrierRoleDto
structuralCarrierRoleToDto = \case
  Descriptor.NeutralVertexCarrier -> Protocol.NeutralVertexCarrierDto
  Descriptor.EdgeCarrier -> Protocol.EdgeCarrierDto
  Descriptor.NablaCarrier -> Protocol.NablaCarrierDto
  Descriptor.DeltaCarrier -> Protocol.DeltaCarrierDto
  Descriptor.ProcessEpochCarrier ->
    error "process epochs have no structural peer carrier tag"

replicaStrengthFromDto :: Protocol.ReplicaStrengthDto -> Route.ReplicaStrength
replicaStrengthFromDto = \case
  Protocol.WeakReplicaDto -> Route.Weak
  Protocol.NormalReplicaDto -> Route.Normal

replicaStrengthToDto :: Route.ReplicaStrength -> Protocol.ReplicaStrengthDto
replicaStrengthToDto = \case
  Route.Weak -> Protocol.WeakReplicaDto
  Route.Normal -> Protocol.NormalReplicaDto

addressFromDto :: Protocol.PeerAddressDto -> Discovery.PeerAddress
addressFromDto = Discovery.peerAddress . Protocol.peerAddressDtoText

addressToDto :: Discovery.PeerAddress -> Protocol.PeerAddressDto
addressToDto = Protocol.peerAddressDto . Discovery.peerAddressText

connectionNonceFromDto :: Protocol.ConnectionNonceDto -> Discovery.ConnectionNonce
connectionNonceFromDto =
  Discovery.connectionNonce . Protocol.connectionNonceDtoWord64

connectionNonceToDto :: Discovery.ConnectionNonce -> Protocol.ConnectionNonceDto
connectionNonceToDto =
  Protocol.connectionNonceDto . Discovery.connectionNonceWord64

controlIndexFromDto :: Protocol.ControlIndexDto -> Identity.ControlIndex
controlIndexFromDto = Identity.controlIndex . Protocol.controlIndexDtoWord64

controlIndexToDto :: Identity.ControlIndex -> Protocol.ControlIndexDto
controlIndexToDto = Protocol.controlIndexDto . Identity.controlIndexWord64

authorityEpochFromDto ::
  Protocol.AuthorityEpochDto ->
  Either PeerRpcBridgeError Identity.AuthorityEpoch
authorityEpochFromDto = \case
  Protocol.GenesisAuthorityEpochDto -> Right Identity.genesisAuthorityEpoch
  Protocol.StructuralAuthorityEpochDto occurrenceDto cutClaim ->
    Identity.structuralAuthorityEpoch
      <$> structuralOccurrenceIdFromDto occurrenceDto
      <*> topologyCutFromClaim cutClaim
  Protocol.LabelAuthorityEpochDto indexDto ->
    Right (Identity.labelAuthorityEpoch (controlIndexFromDto indexDto))

authorityEpochToDto ::
  Identity.AuthorityEpoch ->
  Either PeerRpcBridgeError Protocol.AuthorityEpochDto
authorityEpochToDto authority = case Identity.authorityEpochView authority of
  Identity.GenesisAuthorityEpochView ->
    Right Protocol.GenesisAuthorityEpochDto
  Identity.StructuralAuthorityEpochView occurrence cut ->
    Protocol.StructuralAuthorityEpochDto
      <$> structuralOccurrenceIdToDto occurrence
      <*> topologyCutToClaim cut
  Identity.LabelAuthorityEpochView index ->
    Right (Protocol.LabelAuthorityEpochDto (controlIndexToDto index))

structuralOccurrenceIdFromDto ::
  Protocol.StructuralOccurrenceIdDto ->
  Either PeerRpcBridgeError Identity.StructuralOccurrenceId
structuralOccurrenceIdFromDto
  (Protocol.StructuralOccurrenceIdDto sourceClaim sequenceDto) =
    Identity.structuralOccurrenceId
      <$> heraldEpochFromClaim sourceClaim
      <*> structuralSequenceFromDto sequenceDto

structuralOccurrenceIdToDto ::
  Identity.StructuralOccurrenceId ->
  Either PeerRpcBridgeError Protocol.StructuralOccurrenceIdDto
structuralOccurrenceIdToDto occurrence =
  Protocol.StructuralOccurrenceIdDto
    <$> heraldEpochToClaim
      (Identity.structuralOccurrenceSourceHeraldEpoch occurrence)
    <*> structuralSequenceToDto
      (Identity.structuralOccurrenceSourceSequence occurrence)

structuralSequenceFromDto ::
  Protocol.PositiveStructuralSequenceDto ->
  Either PeerRpcBridgeError Identity.StructuralSequence
structuralSequenceFromDto =
  bridgeClaim
    . Identity.mkStructuralSequence
    . Protocol.positiveStructuralSequenceDtoWord64

structuralSequenceToDto ::
  Identity.StructuralSequence ->
  Either PeerRpcBridgeError Protocol.PositiveStructuralSequenceDto
structuralSequenceToDto =
  bridgeClaim
    . Protocol.positiveStructuralSequenceDto
    . Identity.structuralSequenceWord64

structuralPrefixFromDto ::
  Protocol.StructuralPrefixDto ->
  Either PeerRpcBridgeError Structural.StructuralPrefix
structuralPrefixFromDto = \case
  Protocol.EmptyStructuralPrefixDto -> Right Structural.emptyStructuralPrefix
  Protocol.StructuralPrefixThroughDto sequenceDto ->
    Structural.structuralPrefixThrough <$> structuralSequenceFromDto sequenceDto

structuralPrefixToDto ::
  Structural.StructuralPrefix ->
  Either PeerRpcBridgeError Protocol.StructuralPrefixDto
structuralPrefixToDto prefix =
  case Structural.structuralPrefixSequence prefix of
    Nothing -> Right Protocol.EmptyStructuralPrefixDto
    Just sequenceNumber ->
      Protocol.StructuralPrefixThroughDto
        <$> structuralSequenceToDto sequenceNumber

structuralVersionVectorFromDto ::
  Protocol.StructuralVersionVectorDto ->
  Either PeerRpcBridgeError Structural.StructuralVersionVector
structuralVersionVectorFromDto dto = do
  generation <-
    membershipGenerationFromClaim
      (Protocol.structuralVersionVectorMembershipGeneration dto)
  members <-
    memberSetDigestFromClaim
      (Protocol.structuralVersionVectorMemberSetDigest dto)
  entries <-
    traverse
      structuralVectorEntryFromDto
      (Protocol.structuralVersionVectorEntries dto)
  bridgeClaim
    ( Structural.structuralVersionVectorFromClaimedCoordinates
        generation
        members
        entries
    )

structuralVectorEntryFromDto ::
  Protocol.StructuralVectorEntryDto ->
  Either PeerRpcBridgeError (Identity.HeraldEpoch, Structural.StructuralPrefix)
structuralVectorEntryFromDto
  (Protocol.StructuralVectorEntryDto heraldClaim prefixDto) =
    (,)
      <$> heraldEpochFromClaim heraldClaim
      <*> structuralPrefixFromDto prefixDto

structuralVersionVectorToDto ::
  Structural.StructuralVersionVector ->
  Either PeerRpcBridgeError Protocol.StructuralVersionVectorDto
structuralVersionVectorToDto vector = do
  generation <-
    membershipGenerationToClaim
      (Structural.structuralVersionVectorMembershipGenerationId vector)
  members <-
    memberSetDigestToClaim
      (Structural.structuralVersionVectorMemberSetDigest vector)
  entries <-
    traverse
      structuralVectorEntryToDto
      (Structural.structuralVersionVectorEntries vector)
  bridgeOwnerPublication
    ( Protocol.structuralVersionVectorDto
        generation
        members
        entries
    )

structuralVectorEntryToDto ::
  (Identity.HeraldEpoch, Structural.StructuralPrefix) ->
  Either PeerRpcBridgeError Protocol.StructuralVectorEntryDto
structuralVectorEntryToDto (herald, prefix) =
  Protocol.StructuralVectorEntryDto
    <$> heraldEpochToClaim herald
    <*> structuralPrefixToDto prefix

nablaSequenceFromDto :: Protocol.NablaSequenceDto -> Identity.NablaSequence
nablaSequenceFromDto = Identity.nablaSequence . Protocol.nablaSequenceDtoWord64

nablaSequenceToDto :: Identity.NablaSequence -> Protocol.NablaSequenceDto
nablaSequenceToDto = Protocol.nablaSequenceDto . Identity.nablaSequenceWord64

placementSequenceFromDto ::
  Protocol.PositivePlacementSequenceDto ->
  Either PeerRpcBridgeError Placement.PlacementSequence
placementSequenceFromDto =
  bridgeClaim
    . Placement.mkPlacementSequence
    . Protocol.positivePlacementSequenceDtoWord64

placementSequenceToDto ::
  Placement.PlacementSequence ->
  Either PeerRpcBridgeError Protocol.PositivePlacementSequenceDto
placementSequenceToDto =
  bridgeClaim
    . Protocol.positivePlacementSequenceDto
    . Placement.placementSequenceWord64

streamSequenceFromDto ::
  Protocol.PositiveStreamSequenceDto ->
  Either PeerRpcBridgeError Stream.StreamSequence
streamSequenceFromDto =
  bridgeClaim
    . Stream.mkStreamSequence
    . Protocol.positiveStreamSequenceDtoWord64

streamSequenceToDto ::
  Stream.StreamSequence ->
  Either PeerRpcBridgeError Protocol.PositiveStreamSequenceDto
streamSequenceToDto =
  bridgeClaim
    . Protocol.positiveStreamSequenceDto
    . Stream.streamSequenceWord64

systemFromClaim :: Protocol.SystemClaim -> Either PeerRpcBridgeError Identity.SystemId
systemFromClaim = bridgeClaim . Identity.mkSystemId . Protocol.systemClaimBytes

systemToClaim :: Identity.SystemId -> Either PeerRpcBridgeError Protocol.SystemClaim
systemToClaim = bridgeClaim . Protocol.systemClaim . Identity.systemIdBytes

heraldIdFromClaim :: Protocol.HeraldIdClaim -> Either PeerRpcBridgeError Identity.HeraldId
heraldIdFromClaim = bridgeClaim . Identity.mkHeraldId . Protocol.heraldIdClaimBytes

heraldIdToClaim :: Identity.HeraldId -> Either PeerRpcBridgeError Protocol.HeraldIdClaim
heraldIdToClaim = bridgeClaim . Protocol.heraldIdClaim . Identity.heraldIdBytes

heraldEpochFromClaim ::
  Protocol.HeraldEpochClaim ->
  Either PeerRpcBridgeError Identity.HeraldEpoch
heraldEpochFromClaim =
  bridgeClaim . Identity.mkHeraldEpoch . Protocol.heraldEpochClaimBytes

heraldEpochToClaim ::
  Identity.HeraldEpoch ->
  Either PeerRpcBridgeError Protocol.HeraldEpochClaim
heraldEpochToClaim =
  bridgeClaim . Protocol.heraldEpochClaim . Identity.heraldEpochBytes

globalObjectFromClaim ::
  Protocol.GlobalObjectClaim ->
  Either PeerRpcBridgeError Identity.GlobalObjectId
globalObjectFromClaim =
  bridgeClaim . Identity.mkGlobalObjectId . Protocol.globalObjectClaimBytes

globalObjectToClaim ::
  Identity.GlobalObjectId ->
  Either PeerRpcBridgeError Protocol.GlobalObjectClaim
globalObjectToClaim =
  bridgeClaim . Protocol.globalObjectClaim . Identity.globalObjectIdBytes

processEpochFromClaim ::
  Protocol.ProcessEpochClaim ->
  Either PeerRpcBridgeError Identity.ProcessEpochId
processEpochFromClaim =
  bridgeClaim . Identity.mkProcessEpochId . Protocol.processEpochClaimBytes

processEpochToClaim ::
  Identity.ProcessEpochId ->
  Either PeerRpcBridgeError Protocol.ProcessEpochClaim
processEpochToClaim =
  bridgeClaim . Protocol.processEpochClaim . Identity.processEpochIdBytes

nablaFromClaim :: Protocol.NablaClaim -> Either PeerRpcBridgeError Identity.NablaId
nablaFromClaim = bridgeClaim . Identity.mkNablaId . Protocol.nablaClaimBytes

nablaToClaim :: Identity.NablaId -> Either PeerRpcBridgeError Protocol.NablaClaim
nablaToClaim = bridgeClaim . Protocol.nablaClaim . Identity.nablaIdBytes

deltaFromClaim :: Protocol.DeltaClaim -> Either PeerRpcBridgeError Identity.DeltaId
deltaFromClaim = bridgeClaim . Identity.mkDeltaId . Protocol.deltaClaimBytes

deltaToClaim :: Identity.DeltaId -> Either PeerRpcBridgeError Protocol.DeltaClaim
deltaToClaim = bridgeClaim . Protocol.deltaClaim . Identity.deltaIdBytes

sortOccurrenceFromClaim ::
  Protocol.SortOccurrenceClaim ->
  Either PeerRpcBridgeError Identity.SortDefinitionOccurrenceId
sortOccurrenceFromClaim =
  bridgeClaim
    . Identity.mkSortDefinitionOccurrenceId
    . Protocol.sortOccurrenceClaimBytes

sortOccurrenceToClaim ::
  Identity.SortDefinitionOccurrenceId ->
  Either PeerRpcBridgeError Protocol.SortOccurrenceClaim
sortOccurrenceToClaim =
  bridgeClaim
    . Protocol.sortOccurrenceClaim
    . Identity.sortDefinitionOccurrenceIdBytes

storeIncarnationFromClaim ::
  Protocol.StoreIncarnationClaim ->
  Either PeerRpcBridgeError Identity.StoreIncarnationId
storeIncarnationFromClaim =
  bridgeClaim
    . Identity.mkStoreIncarnationId
    . Protocol.storeIncarnationClaimBytes

storeIncarnationToClaim ::
  Identity.StoreIncarnationId ->
  Either PeerRpcBridgeError Protocol.StoreIncarnationClaim
storeIncarnationToClaim =
  bridgeClaim
    . Protocol.storeIncarnationClaim
    . Identity.storeIncarnationIdBytes

catalogueDigestFromClaim ::
  Protocol.CatalogueDigestClaim ->
  Either PeerRpcBridgeError Startup.CatalogueDigest
catalogueDigestFromClaim =
  bridgeClaim
    . Startup.mkCatalogueDigest
    . Protocol.catalogueDigestClaimBytes

catalogueDigestToClaim ::
  Startup.CatalogueDigest ->
  Either PeerRpcBridgeError Protocol.CatalogueDigestClaim
catalogueDigestToClaim =
  bridgeClaim
    . Protocol.catalogueDigestClaim
    . Startup.catalogueDigestBytes

initialProjectionDigestFromClaim ::
  Protocol.InitialProjectionDigestClaim ->
  Either PeerRpcBridgeError Startup.InitialProjectionDigest
initialProjectionDigestFromClaim =
  bridgeClaim
    . Startup.mkInitialProjectionDigest
    . Protocol.initialProjectionDigestClaimBytes

initialProjectionDigestToClaim ::
  Startup.InitialProjectionDigest ->
  Either PeerRpcBridgeError Protocol.InitialProjectionDigestClaim
initialProjectionDigestToClaim =
  bridgeClaim
    . Protocol.initialProjectionDigestClaim
    . Startup.initialProjectionDigestBytes

membershipGenerationFromClaim ::
  Protocol.HeraldMembershipGenerationClaim ->
  Either PeerRpcBridgeError Membership.HeraldMembershipGenerationId
membershipGenerationFromClaim =
  bridgeClaim
    . Membership.mkHeraldMembershipGenerationId
    . Protocol.heraldMembershipGenerationClaimBytes

membershipGenerationToClaim ::
  Membership.HeraldMembershipGenerationId ->
  Either PeerRpcBridgeError Protocol.HeraldMembershipGenerationClaim
membershipGenerationToClaim =
  bridgeClaim
    . Protocol.heraldMembershipGenerationClaim
    . Membership.heraldMembershipGenerationIdBytes

memberSetDigestFromClaim ::
  Protocol.MemberSetDigestClaim ->
  Either PeerRpcBridgeError Topology.MemberSetDigest
memberSetDigestFromClaim =
  bridgeClaim
    . Topology.mkMemberSetDigest
    . Protocol.memberSetDigestClaimBytes

memberSetDigestToClaim ::
  Topology.MemberSetDigest ->
  Either PeerRpcBridgeError Protocol.MemberSetDigestClaim
memberSetDigestToClaim =
  bridgeClaim
    . Protocol.memberSetDigestClaim
    . Topology.memberSetDigestBytes

structuralPublicationDigestFromClaim ::
  Protocol.StructuralPublicationDigestClaim ->
  Either PeerRpcBridgeError Publication.StructuralPublicationDigest
structuralPublicationDigestFromClaim =
  bridgeClaim
    . Publication.mkStructuralPublicationDigest
    . Protocol.structuralPublicationDigestClaimBytes

structuralPublicationDigestToClaim ::
  Publication.StructuralPublicationDigest ->
  Either PeerRpcBridgeError Protocol.StructuralPublicationDigestClaim
structuralPublicationDigestToClaim =
  bridgeClaim
    . Protocol.structuralPublicationDigestClaim
    . Publication.structuralPublicationDigestBytes

topologyOccurrenceDigestFromClaim ::
  Protocol.TopologyOccurrenceDigestClaim ->
  Either PeerRpcBridgeError Topology.TopologyOccurrenceDigest
topologyOccurrenceDigestFromClaim =
  bridgeClaim
    . Topology.mkTopologyOccurrenceDigest
    . Protocol.topologyOccurrenceDigestClaimBytes

topologyOccurrenceDigestToClaim ::
  Topology.TopologyOccurrenceDigest ->
  Either PeerRpcBridgeError Protocol.TopologyOccurrenceDigestClaim
topologyOccurrenceDigestToClaim =
  bridgeClaim
    . Protocol.topologyOccurrenceDigestClaim
    . Topology.topologyOccurrenceDigestBytes

terminalSourceUnionDigestFromClaim ::
  Protocol.TerminalSourceUnionDigestClaim ->
  Either PeerRpcBridgeError Topology.TerminalSourceUnionDigest
terminalSourceUnionDigestFromClaim =
  bridgeClaim
    . Topology.mkTerminalSourceUnionDigest
    . Protocol.terminalSourceUnionDigestClaimBytes

terminalSourceUnionDigestToClaim ::
  Topology.TerminalSourceUnionDigest ->
  Either PeerRpcBridgeError Protocol.TerminalSourceUnionDigestClaim
terminalSourceUnionDigestToClaim =
  bridgeClaim
    . Protocol.terminalSourceUnionDigestClaim
    . Topology.terminalSourceUnionDigestBytes

terminalSourceInventoryDigestFromClaim ::
  Protocol.TerminalSourceInventoryDigestClaim ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourceInventoryDigest
terminalSourceInventoryDigestFromClaim =
  bridgeClaim
    . TerminalSource.mkTerminalSourceInventoryDigest
    . Protocol.terminalSourceInventoryDigestClaimBytes

terminalSourceInventoryDigestToClaim ::
  TerminalSource.TerminalSourceInventoryDigest ->
  Either PeerRpcBridgeError Protocol.TerminalSourceInventoryDigestClaim
terminalSourceInventoryDigestToClaim =
  bridgeClaim
    . Protocol.terminalSourceInventoryDigestClaim
    . TerminalSource.terminalSourceInventoryDigestBytes

terminalSourceAcceptanceDigestFromClaim ::
  Protocol.TerminalSourceAcceptanceDigestClaim ->
  Either PeerRpcBridgeError TerminalSource.TerminalSourceAcceptanceDigest
terminalSourceAcceptanceDigestFromClaim =
  bridgeClaim
    . TerminalSource.mkTerminalSourceAcceptanceDigest
    . Protocol.terminalSourceAcceptanceDigestClaimBytes

terminalSourceAcceptanceDigestToClaim ::
  TerminalSource.TerminalSourceAcceptanceDigest ->
  Either PeerRpcBridgeError Protocol.TerminalSourceAcceptanceDigestClaim
terminalSourceAcceptanceDigestToClaim =
  bridgeClaim
    . Protocol.terminalSourceAcceptanceDigestClaim
    . TerminalSource.terminalSourceAcceptanceDigestBytes

contextClassGenerationFromClaim ::
  Protocol.ContextClassGenerationClaim ->
  Either PeerRpcBridgeError DomainAlignment.ContextClassGenerationId
contextClassGenerationFromClaim =
  bridgeClaim
    . DomainAlignment.mkContextClassGenerationId
    . Protocol.contextClassGenerationClaimBytes

contextClassGenerationToClaim ::
  DomainAlignment.ContextClassGenerationId ->
  Either PeerRpcBridgeError Protocol.ContextClassGenerationClaim
contextClassGenerationToClaim =
  bridgeClaim
    . Protocol.contextClassGenerationClaim
    . DomainAlignment.contextClassGenerationIdBytes

memberReadyEvidenceDigestFromClaim ::
  Protocol.MemberReadyEvidenceDigestClaim ->
  Either PeerRpcBridgeError DomainAlignment.MemberReadyEvidenceDigest
memberReadyEvidenceDigestFromClaim =
  bridgeClaim
    . DomainAlignment.mkMemberReadyEvidenceDigest
    . Protocol.memberReadyEvidenceDigestClaimBytes

memberReadyEvidenceDigestToClaim ::
  DomainAlignment.MemberReadyEvidenceDigest ->
  Either PeerRpcBridgeError Protocol.MemberReadyEvidenceDigestClaim
memberReadyEvidenceDigestToClaim =
  bridgeClaim
    . Protocol.memberReadyEvidenceDigestClaim
    . DomainAlignment.memberReadyEvidenceDigestBytes

historicalCertificateDigestFromClaim ::
  Protocol.HistoricalCertificateDigestClaim ->
  Either PeerRpcBridgeError DomainAlignment.HistoricalCertificateDigest
historicalCertificateDigestFromClaim =
  bridgeClaim
    . DomainAlignment.mkHistoricalCertificateDigest
    . Protocol.historicalCertificateDigestClaimBytes

historicalCertificateDigestToClaim ::
  DomainAlignment.HistoricalCertificateDigest ->
  Either PeerRpcBridgeError Protocol.HistoricalCertificateDigestClaim
historicalCertificateDigestToClaim =
  bridgeClaim
    . Protocol.historicalCertificateDigestClaim
    . DomainAlignment.historicalCertificateDigestBytes

alignmentSnapshotDigestFromClaim ::
  Protocol.AlignmentSnapshotDigestClaim ->
  Either PeerRpcBridgeError DomainAlignment.AlignmentSnapshotDigest
alignmentSnapshotDigestFromClaim =
  bridgeClaim
    . DomainAlignment.mkAlignmentSnapshotDigest
    . Protocol.alignmentSnapshotDigestClaimBytes

alignmentSnapshotDigestToClaim ::
  DomainAlignment.AlignmentSnapshotDigest ->
  Either PeerRpcBridgeError Protocol.AlignmentSnapshotDigestClaim
alignmentSnapshotDigestToClaim =
  bridgeClaim
    . Protocol.alignmentSnapshotDigestClaim
    . DomainAlignment.alignmentSnapshotDigestBytes

bootstrapEvidenceDigestFromClaim ::
  Protocol.BootstrapEvidenceDigestClaim ->
  Either PeerRpcBridgeError DomainAlignment.BootstrapEvidenceDigest
bootstrapEvidenceDigestFromClaim =
  bridgeClaim
    . DomainAlignment.mkBootstrapEvidenceDigest
    . Protocol.bootstrapEvidenceDigestClaimBytes

bootstrapEvidenceDigestToClaim ::
  DomainAlignment.BootstrapEvidenceDigest ->
  Either PeerRpcBridgeError Protocol.BootstrapEvidenceDigestClaim
bootstrapEvidenceDigestToClaim =
  bridgeClaim
    . Protocol.bootstrapEvidenceDigestClaim
    . DomainAlignment.bootstrapEvidenceDigestBytes

topologyCutFromClaim ::
  Protocol.TopologyCutClaim ->
  Either PeerRpcBridgeError Identity.TopologyCutId
topologyCutFromClaim =
  bridgeClaim . Identity.mkTopologyCutId . Protocol.topologyCutClaimBytes

topologyCutToClaim ::
  Identity.TopologyCutId ->
  Either PeerRpcBridgeError Protocol.TopologyCutClaim
topologyCutToClaim =
  bridgeClaim . Protocol.topologyCutClaim . Identity.topologyCutIdBytes

peerItemDigestFromClaim ::
  Protocol.PeerItemDigestClaim ->
  Either PeerRpcBridgeError Stream.PeerItemDigest
peerItemDigestFromClaim =
  bridgeClaim . Stream.mkPeerItemDigest . Protocol.peerItemDigestClaimBytes

peerItemDigestToClaim ::
  Stream.PeerItemDigest ->
  Either PeerRpcBridgeError Protocol.PeerItemDigestClaim
peerItemDigestToClaim =
  bridgeClaim . Protocol.peerItemDigestClaim . Stream.peerItemDigestBytes

bridgeClaim :: Either error value -> Either PeerRpcBridgeError value
bridgeClaim = either (const (Left PeerRpcBridgeClaimContradiction)) Right

bridgeOwnerPublication ::
  Either error value ->
  Either PeerRpcBridgeError value
bridgeOwnerPublication =
  either (const (Left PeerRpcBridgeOwnerPublicationContradiction)) Right

preparationFromDto :: Protocol.PreparationControlDto -> Either PeerRpcBridgeError Preparation.PreparationControl
preparationFromDto = \case
  Protocol.PreparationOfferedDto reference locator bytes -> pure (Preparation.PreparationOffered reference locator bytes)
  Protocol.PreparationCancelledDto reference -> pure (Preparation.PreparationCancelled reference)
  Protocol.PreparationQueriedDto reference -> pure (Preparation.PreparationQueried reference)
  Protocol.PreparationRepliedDto reference status -> Preparation.PreparationReplied reference <$> statusFromDto status
  where
    childFromDto (Protocol.RemoteChildDto process control descriptor) = Preparation.RemoteChild <$> processEpochFromClaim process <*> pure (controlIndexFromDto control) <*> pure descriptor
    statusFromDto = \case
      Protocol.RemotePreparationPendingDto keys -> pure (Preparation.RemotePreparationPending keys)
      Protocol.RemotePreparationAvailableDto child keys -> Preparation.RemotePreparationAvailable <$> childFromDto child <*> pure keys
      Protocol.RemotePreparationClaimedDto child -> Preparation.RemotePreparationClaimed <$> childFromDto child
      Protocol.RemotePreparationCancelledDto -> pure Preparation.RemotePreparationCancelled
      Protocol.RemotePreparationFailedDto failure -> pure (Preparation.RemotePreparationFailed failure)
preparationToDto :: Preparation.PreparationControl -> Either PeerRpcBridgeError Protocol.PreparationControlDto
preparationToDto = \case
  Preparation.PreparationOffered reference locator bytes -> pure (Protocol.PreparationOfferedDto reference locator bytes)
  Preparation.PreparationCancelled reference -> pure (Protocol.PreparationCancelledDto reference)
  Preparation.PreparationQueried reference -> pure (Protocol.PreparationQueriedDto reference)
  Preparation.PreparationReplied reference status -> Protocol.PreparationRepliedDto reference <$> statusToDto status
  where
    childToDto (Preparation.RemoteChild process control descriptor) = Protocol.RemoteChildDto <$> processEpochToClaim process <*> pure (controlIndexToDto control) <*> pure descriptor
    statusToDto = \case
      Preparation.RemotePreparationPending keys -> pure (Protocol.RemotePreparationPendingDto keys)
      Preparation.RemotePreparationAvailable child keys -> Protocol.RemotePreparationAvailableDto <$> childToDto child <*> pure keys
      Preparation.RemotePreparationClaimed child -> Protocol.RemotePreparationClaimedDto <$> childToDto child
      Preparation.RemotePreparationCancelled -> pure Protocol.RemotePreparationCancelledDto
      Preparation.RemotePreparationFailed failure -> pure (Protocol.RemotePreparationFailedDto failure)
