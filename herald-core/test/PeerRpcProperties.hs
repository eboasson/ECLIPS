{-# LANGUAGE OverloadedStrings #-}

module PeerRpcProperties
  ( tests,
  )
where

import Data.Bits (complement)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Disappearance qualified as Disappearance
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
import Eclips.Herald.Disappearance.Protocol qualified as DisappearanceProtocol
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Graph.Protocol qualified as GraphProtocol
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.Input qualified as Input
import Eclips.Herald.Peer.RPC
import Eclips.Herald.Peer.Step15 qualified as PeerStep15
import Eclips.Herald.PeerDispatch
  ( PeerLogicalAttempt,
  )
import Eclips.Herald.PeerDispatch.Internal qualified as PeerDispatch
import Eclips.Herald.PeerPayload qualified as Payload
import Eclips.Herald.PeerPublication qualified as Publication
import Eclips.Herald.PeerStream qualified as Stream
import Eclips.Herald.PeerStream.State qualified as StreamState
import Eclips.Herald.Placement qualified as Placement
import Eclips.Herald.ProcessPreparation.Protocol qualified as Preparation
import Eclips.Herald.Structural.Debt qualified as Debt
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Peer.Frame
  ( PeerFrameContinuation (..),
    PeerFrameFeedResult (..),
    encodePeerFrame,
    feedPeerFrame,
    finishPeerFrameDecoder,
    initialPeerFrameDecoder,
  )
import Eclips.Protocol.Peer.Types qualified as Protocol
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "peer RPC adapter"
    [ testCase "remote candidate context derives the exact Hello tuple" caseRemoteCandidate,
      testCase "local candidate context retains the owner candidate and checks nonce" caseLocalCandidate,
      testCase "candidate and established phase matrices reject every wrong envelope arm" casePhaseMatrix,
      testCase "every current control arm projects and admits losslessly" caseControlRoundTrips,
      testCase "alignment evidence delivery and sparse receipts round-trip atomically" caseAlignmentDelivery,
      testCase "label installation reports retain terminal identity and reject zero control coordinates" caseLabelInstallation,
      testCase "membership-successor topology cuts project and admit losslessly" caseMembershipSuccessorRoundTrip,
      testCase "membership-admission topology cuts preserve their recipe through EPRP" caseMembershipAdmissionRoundTrip,
      testCase "membership-admission topology claims require canonical identity and one empty extension" caseMembershipAdmissionClaims,
      testCase "failure probes and terminal-source coordination round-trip as semantic controls" caseStep15ControlRoundTrips,
      testCase "direct failure-probe identities are authenticated" caseDirectFailureProbeAuthentication,
      testCase "terminal-source canonical bytes and redundant coordinates are authenticated" caseTerminalSourceAuthentication,
      testCase "terminal-source union occurrence claims authenticate through the public RPC boundary" caseTerminalSourceUnionOccurrenceClaim,
      testCase "vector member-set contradictions fail at the RPC boundary" caseVectorMemberSetContradiction,
      testCase "all control causes round-trip through the current protocol" caseControlCauseRoundTrip,
      testCase "all predefined roles project and admit exhaustively" casePredefinedRoles,
      testCase "both replica strengths project and admit exhaustively" caseReplicaStrengths,
      testCase "every authority arm survives publication DTO, frame, and opaque admission" casePublicationRoundTrip,
      testCase "route-cutover markers share the publication stream and authenticate every field" caseRouteCutoverRoundTrip,
      testCase "disappearance markers bind probe, coordinate, direction, sequence, and digest" caseDisappearanceMarkerRoundTrip,
      testCase "publication and all markers retain order across dispatch reconnect" caseLogicalOrderingReconnect,
      testCase "malformed canonical value bytes fail before digest admission" caseMalformedCanonicalValue,
      testCase "the carried item digest is checked" caseCarriedDigestMismatch,
      testCase "mutating every semantic batch field invalidates the digest" caseEveryBatchFieldInDigest,
      testCase "inconsistent owner publication items fail projection" caseOwnerPublicationFault,
      testCase "incoherent owner resume responses fail projection" caseOwnerControlFault
    ]

caseRemoteCandidate :: IO ()
caseRemoteCandidate = do
  envelope <- projectHelloOrFail peerHello
  assertEqual
    "candidate is derived from the remote Hello"
    ( Right
        ( CandidatePeerHello
            ( Discovery.peerCandidate
                heraldIdA
                heraldEpochA
                connectionNonce
            )
            peerHello
            genesisMembershipId
            genesisMemberSetDigest
        )
    )
    (admitCandidatePeerEnvelope RemoteInitiatedCandidate envelope)

caseLocalCandidate :: IO ()
caseLocalCandidate = do
  envelope <- projectHelloOrFail peerHello
  let ownerCandidate = Discovery.peerCandidate heraldIdB heraldEpochB connectionNonce
      wrongCandidate =
        Discovery.peerCandidate
          heraldIdB
          heraldEpochB
          (Discovery.connectionNonce 99)
  assertEqual
    "same owner-minted candidate"
    ( Right
        ( CandidatePeerHello
            ownerCandidate
            peerHello
            genesisMembershipId
            genesisMemberSetDigest
        )
    )
    ( admitCandidatePeerEnvelope
        (LocallyInitiatedCandidate ownerCandidate)
        envelope
    )
  assertEqual
    "reciprocal nonce mismatch"
    (Left PeerRpcClaimContradiction)
    ( admitCandidatePeerEnvelope
        (LocallyInitiatedCandidate wrongCandidate)
        envelope
    )

casePhaseMatrix :: IO ()
casePhaseMatrix = do
  helloEnvelope <- projectHelloOrFail peerHello
  controlEnvelope <- projectOrFail (projectPeerControl firstControl)
  publicationEnvelope <- projectOrFail (projectPeerLogicalAttempt publicationAttempt)
  assertEqual
    "Hello after establishment"
    (Left PeerRpcWrongIngressPhase)
    (admitEstablishedPeerEnvelope helloEnvelope)
  assertEqual
    "control during candidate phase"
    (Left PeerRpcWrongIngressPhase)
    (admitCandidatePeerEnvelope RemoteInitiatedCandidate controlEnvelope)
  assertEqual
    "publication during candidate phase"
    (Left PeerRpcWrongIngressPhase)
    (admitCandidatePeerEnvelope RemoteInitiatedCandidate publicationEnvelope)

caseControlRoundTrips :: IO ()
caseControlRoundTrips =
  mapM_
    ( \control -> do
        envelope <- projectOrFail (projectPeerControl control)
        framed <- frameRoundTrip envelope
        assertEqual
          (show control)
          (Right (EstablishedPeerControl mempty control))
          (admitEstablishedPeerEnvelope framed)
    )
    allControls

caseLabelInstallation :: IO ()
caseLabelInstallation = do
  let decision = identity Identity.mkLabelDecisionId 0x74
      digest = mustAdmit (Label.mkLabelOutcomeDigest (fixtureBytes 0x75))
      progress = Receipt.receiptRetirementPrefix (Just 3)
      report = Label.labelInstallationReport decision heraldEpochA (Identity.controlIndex 7) digest
      control = Input.PeerLabelInstalled report
  envelope <- projectOrFail (projectPeerControlWithProgress progress control)
  framed <- frameRoundTrip envelope
  assertEqual
    "the direct report preserves its terminal coordinate, digest, reporter and receipt progress"
    (Right (EstablishedPeerControl progress control))
    (admitEstablishedPeerEnvelope framed)
  assertEqual
    "owner reports with no terminal coordinate cannot reach the wire"
    (Left PeerRpcOwnerClaimContradiction)
    ( projectPeerControl
        ( Input.PeerLabelInstalled
            (Label.labelInstallationReport decision heraldEpochA (Identity.controlIndex 0) digest)
        )
    )

caseAlignmentDelivery :: IO ()
caseAlignmentDelivery = do
  let progress = mustAdmit (Receipt.receiptRetirement (Just 4) (Set.singleton 2))
      sequenceNumber = DomainAlignment.firstAlignmentDeliverySequence
      placementVector = mustAdmit (DomainAlignment.physicalPlacementRevisionVector genesisMembership [(heraldEpochA, DomainAlignment.firstPlacementRevision), (heraldEpochB, DomainAlignment.firstPlacementRevision)])
      acceptance = Alignment.AlignmentCutAcceptanceAdvertised (Alignment.alignmentCutAccepted generationA heraldEpochA canonicalTopologyCutId placementVector DomainAlignment.EmptyHeraldPublicationPrefix)
      readiness = Alignment.AlignmentMemberReadyAdvertised (Alignment.classMemberReady (mustAdmit (Alignment.mkAlignmentEvidenceSequence 1)) generationA incarnationA (DomainAlignment.deriveMemberReadyEvidenceDigest "delivery test") DomainAlignment.initialStoreRevision)
      controls = [Input.PeerAlignmentEvidenceDelivered sequenceNumber acceptance, Input.PeerAlignmentEvidenceDelivered (DomainAlignment.nextAlignmentDeliverySequence sequenceNumber) readiness, Input.PeerAlignmentDeliveryProgress]
  assertEqual "delivery sequence zero rejects" (Left DomainAlignment.AlignmentDeliverySequenceMustBePositive) (DomainAlignment.alignmentDeliverySequence 0)
  assertEqual "delivery sequence increments independently" 2 (DomainAlignment.alignmentDeliverySequenceWord64 (DomainAlignment.nextAlignmentDeliverySequence sequenceNumber))
  mapM_
    ( \control -> do
        envelope <- projectOrFail (projectPeerControlWithProgress progress control)
        framed <- frameRoundTrip envelope
        assertEqual "same delivery and sparse receipt" (Right (EstablishedPeerControl progress control)) (admitEstablishedPeerEnvelope framed)
        assertEqual "established evidence cannot enter candidate phase" (Left PeerRpcWrongIngressPhase) (admitCandidatePeerEnvelope RemoteInitiatedCandidate framed)
    )
    controls
  envelope <- projectOrFail (projectPeerLogicalAttemptWithProgress progress publicationAttempt)
  case admitEstablishedPeerEnvelope envelope of
    Right (EstablishedPeerPublication retainedProgress admitted) -> do
      assertEqual "publication carries sparse progress" progress retainedProgress
      assertEqual "header does not alter publication identity" (Stream.peerDispatchAttemptItem publicationAttempt) (PeerDispatch.peerLogicalSequencedItem admitted)
    other -> assertFailure ("unexpected progress publication admission: " <> show other)
  mapM_ (\evidence -> assertEqual "bare delivery evidence cannot bypass sequence" (Left PeerRpcOwnerPublicationContradiction) (projectPeerControl (Input.PeerAlignmentControl evidence))) [acceptance, readiness]
  assertEqual
    "unrelated alignment control cannot enter evidence delivery lane"
    (Left PeerRpcOwnerPublicationContradiction)
    (projectPeerControl (Input.PeerAlignmentEvidenceDelivered sequenceNumber (Alignment.AlignmentProbeMarker disappearanceProbe (Alignment.alignmentSubscriptionId heraldEpochA (mustAdmit (Alignment.mkAlignmentSubscriptionSequence 1))) DomainAlignment.initialStoreRevision)))

caseMembershipSuccessorRoundTrip :: IO ()
caseMembershipSuccessorRoundTrip =
  assertControlRoundTrip
    ( Input.PeerTopologyCutAnnounced
        (GraphProtocol.topologyCutAnnounce heraldEpochA successorTopologyCut)
    )

caseMembershipAdmissionRoundTrip :: IO ()
caseMembershipAdmissionRoundTrip = do
  let control = Input.PeerTopologyCutAnnounced (GraphProtocol.topologyCutAnnounce heraldEpochA admissionTopologyCut)
  envelope <- projectOrFail (projectPeerControl control)
  framed <- frameRoundTrip envelope
  assertEqual
    "the checked activation recipe and every predecessor coordinate survive framing"
    (Right (EstablishedPeerControl mempty control))
    (admitEstablishedPeerEnvelope framed)
  assertEqual "projection is canonical after decoding" envelope framed

caseMembershipAdmissionClaims :: IO ()
caseMembershipAdmissionClaims = do
  projected <-
    projectedControlDto
      (Input.PeerTopologyCutAnnounced (GraphProtocol.topologyCutAnnounce heraldEpochA admissionTopologyCut))
  case projected of
    Protocol.TopologyCutAnnouncedDto
      ( Protocol.TopologyCutAnnounceDto
          reporter
          cutId
          ( Protocol.TopologyCutDto
              (Protocol.AdmissionTopologyPredecessorDto oldCut old new terminal initial admission recipe)
              frontier
              occurrenceClaim
            )
        ) -> do
        let predecessorWith replacementVector admissionBytes recipeBytes =
              Protocol.TopologyCutAnnouncedDto
                ( Protocol.TopologyCutAnnounceDto
                    reporter
                    cutId
                    ( Protocol.TopologyCutDto
                        (Protocol.AdmissionTopologyPredecessorDto oldCut old new terminal replacementVector admissionBytes recipeBytes)
                        frontier
                        occurrenceClaim
                    )
                )
            requireRejected label = assertControlAdmissionError label PeerRpcClaimContradiction
            changePrefix target =
              mustAdmit
                ( Protocol.structuralVersionVectorDto
                    (Protocol.structuralVersionVectorMembershipGeneration initial)
                    (Protocol.structuralVersionVectorMemberSetDigest initial)
                    [ Protocol.StructuralVectorEntryDto
                        epoch
                        (if epoch == target then Protocol.StructuralPrefixThroughDto (mustAdmit (Protocol.positiveStructuralSequenceDto 1)) else prefix)
                    | Protocol.StructuralVectorEntryDto epoch prefix <- NonEmpty.toList (Protocol.structuralVersionVectorEntries initial)
                    ]
                )
        assertEqual
          "the producer uses the canonical admission identity"
          (Membership.heraldAdmissionIdCanonicalBytes (Topology.heraldJoinBaseRecipeAdmissionId admissionRecipe))
          admission
        assertEqual
          "the producer binds the exact activation recipe digest"
          (Topology.heraldJoinBaseRecipeDigestBytes admissionRecipe)
          recipe
        requireRejected "trailing bytes cannot create another canonical admission identity" (predecessorWith initial (admission <> "\NUL") recipe)
        requireRejected "a shortened recipe digest is not a checked digest" (predecessorWith initial admission (ByteString.take 31 recipe))
        requireRejected "the applicant must start at the empty structural prefix" (predecessorWith (changePrefix (identity Protocol.heraldEpochClaim 0x6f)) admission recipe)
        requireRejected "every old member retains its exact prefix" (predecessorWith (changePrefix (identity Protocol.heraldEpochClaim 4)) admission recipe)
        requireRejected "the predecessor vector cannot serve as a zero-dimensional admission" (predecessorWith terminal admission recipe)
    other -> assertFailure ("unexpected admission topology projection: " <> show other)

caseStep15ControlRoundTrips :: IO ()
caseStep15ControlRoundTrips = mapM_ assertControlRoundTrip step15Controls

caseDirectFailureProbeAuthentication :: IO ()
caseDirectFailureProbeAuthentication = do
  projected <-
    projectedControlDto
      (Input.PeerDirectFailureProbeRequested directProbeRequest)
  case projected of
    Protocol.DirectFailureProbeRequestedDto
      (Protocol.DirectFailureProbeRequestDto probe target generation configuration) -> do
        assertEqual
          "the request carries its captured voter configuration exactly"
          (Voter.voterConfigurationIdBytes (PeerStep15.directFailureProbeRequestVoterConfiguration directProbeRequest))
          (Protocol.oracleVoterConfigurationClaimBytes configuration)
        let wrongDigest =
              mustAdmit
                (Protocol.heraldFailureProbeDigestClaim (fixtureBytes 0xd7))
            wrongProbe =
              mustAdmit
                ( Protocol.heraldFailureProbeIdDto
                    wrongDigest
                    (Protocol.heraldFailureProbeIdControlIndex probe)
                )
        assertControlAdmissionError
          "the probe digest must rederive from its control index"
          PeerRpcClaimContradiction
          ( Protocol.DirectFailureProbeRequestedDto
              (Protocol.DirectFailureProbeRequestDto wrongProbe target generation configuration)
          )
        -- A different well-shaped configuration remains a distinct claim; the
        -- failure owner, not the stateless bridge, checks captured authority.
        let changed =
              PeerStep15.directFailureProbeRequest
                (PeerStep15.directFailureProbeRequestProbe directProbeRequest)
                (PeerStep15.directFailureProbeRequestTarget directProbeRequest)
                (PeerStep15.directFailureProbeRequestGeneration directProbeRequest)
                (mustAdmit (Voter.mkVoterConfigurationId (fixtureBytes 0xdb)))
        assertControlRoundTrip (Input.PeerDirectFailureProbeRequested changed)
        assertControlRoundTrip
          ( Input.PeerDirectFailureProbeResponded
              (PeerStep15.directFailureProbeResponse changed PeerStep15.DirectFailureProbeUnreachable)
          )
    other -> assertFailure ("unexpected direct failure-probe projection: " <> show other)

caseTerminalSourceAuthentication :: IO ()
caseTerminalSourceAuthentication = do
  assertTerminalInventoryAuthentication
  assertTerminalPayloadRelayAuthentication
  assertTerminalUnionAnnounceAuthentication
  assertTerminalUnionAcceptanceAuthentication
  assertTerminalUnionEstablishedAuthentication

assertTerminalInventoryAuthentication :: IO ()
assertTerminalInventoryAuthentication = do
  projected <-
    projectedControlDto
      (Input.PeerTerminalSourceInventoryAdvertised terminalInventory)
  case projected of
    Protocol.TerminalSourceInventoryAdvertisedDto
      ( Protocol.TerminalSourceInventoryDto
          predecessor
          successor
          retired
          reporter
          prefix
          digest
          canonicalBytes
        ) -> do
        assertEqual
          "trailing bytes cannot alias a canonical inventory"
          (Left PeerRpcCanonicalValueContradiction)
          ( admitEstablishedPeerEnvelope
              ( Protocol.PeerControlEnvelope
                  mempty
                  ( Protocol.TerminalSourceInventoryAdvertisedDto
                      ( Protocol.TerminalSourceInventoryDto
                          predecessor
                          successor
                          retired
                          reporter
                          prefix
                          digest
                          (canonicalBytes <> "x")
                      )
                  )
              )
          )
        assertControlAdmissionError
          "a redundant coordinate must name the parsed inventory"
          PeerRpcClaimContradiction
          ( Protocol.TerminalSourceInventoryAdvertisedDto
              ( Protocol.TerminalSourceInventoryDto
                  predecessor
                  wrongMembershipGenerationClaim
                  retired
                  reporter
                  prefix
                  digest
                  canonicalBytes
              )
          )
    other -> assertFailure ("unexpected terminal inventory projection: " <> show other)

assertTerminalPayloadRelayAuthentication :: IO ()
assertTerminalPayloadRelayAuthentication = do
  projected <-
    projectedControlDto
      (Input.PeerTerminalSourcePayloadRelayed terminalPayloadRelay)
  case projected of
    Protocol.TerminalSourcePayloadRelayedDto
      ( Protocol.TerminalSourcePayloadRelayDto
          predecessor
          successor
          retired
          relay
          inventory
          terminalOccurrenceDto@(Protocol.StructuralOccurrenceIdDto _ sequenceNumber)
          publicationDigest
          canonicalBytes
        ) -> do
        assertControlAdmissionError
          "trailing bytes cannot alias a canonical terminal payload relay"
          PeerRpcCanonicalValueContradiction
          ( Protocol.TerminalSourcePayloadRelayedDto
              ( Protocol.TerminalSourcePayloadRelayDto
                  predecessor
                  successor
                  retired
                  relay
                  inventory
                  terminalOccurrenceDto
                  publicationDigest
                  (canonicalBytes <> "x")
              )
          )
        assertControlAdmissionError
          "the occurrence coordinate must name the canonical relayed occurrence"
          PeerRpcClaimContradiction
          ( Protocol.TerminalSourcePayloadRelayedDto
              ( Protocol.TerminalSourcePayloadRelayDto
                  predecessor
                  successor
                  retired
                  relay
                  inventory
                  ( Protocol.StructuralOccurrenceIdDto
                      (protocolHeraldEpochClaim heraldEpochA)
                      sequenceNumber
                  )
                  publicationDigest
                  canonicalBytes
              )
          )
    other -> assertFailure ("unexpected terminal payload-relay projection: " <> show other)

assertTerminalUnionAnnounceAuthentication :: IO ()
assertTerminalUnionAnnounceAuthentication = do
  projected <-
    projectedControlDto
      (Input.PeerTerminalSourceUnionAnnounced terminalUnionAnnounce)
  case projected of
    Protocol.TerminalSourceUnionAnnouncedDto
      ( Protocol.TerminalSourceUnionAnnounceDto
          _
          ( Protocol.TerminalSourceUnionDto
              predecessor
              successor
              retired
              digest
              canonicalBytes
            )
        ) -> do
        let unionDto =
              Protocol.TerminalSourceUnionDto
                predecessor
                successor
                retired
                digest
                canonicalBytes
        assertControlAdmissionError
          "only the canonical successor announcer is admitted"
          PeerRpcClaimContradiction
          ( Protocol.TerminalSourceUnionAnnouncedDto
              ( Protocol.TerminalSourceUnionAnnounceDto
                  wrongHeraldEpochClaim
                  unionDto
              )
          )
        assertControlAdmissionError
          "the union generation coordinate must name the canonical union"
          PeerRpcClaimContradiction
          ( Protocol.TerminalSourceUnionAnnouncedDto
              ( Protocol.TerminalSourceUnionAnnounceDto
                  (protocolHeraldEpochClaim heraldEpochA)
                  ( Protocol.TerminalSourceUnionDto
                      predecessor
                      wrongMembershipGenerationClaim
                      retired
                      digest
                      canonicalBytes
                  )
              )
          )
    other -> assertFailure ("unexpected terminal union-announcement projection: " <> show other)

assertTerminalUnionAcceptanceAuthentication :: IO ()
assertTerminalUnionAcceptanceAuthentication = do
  projected <-
    projectedControlDto
      (Input.PeerTerminalSourceUnionAccepted terminalUnionAcceptance)
  case projected of
    Protocol.TerminalSourceUnionAcceptedDto
      ( Protocol.TerminalSourceUnionAcceptanceDto
          predecessor
          successor
          retired
          unionDigest
          _
          acceptanceDigest
        ) ->
        assertControlAdmissionError
          "the acceptance digest authenticates its reporter"
          PeerRpcClaimContradiction
          ( Protocol.TerminalSourceUnionAcceptedDto
              ( Protocol.TerminalSourceUnionAcceptanceDto
                  predecessor
                  successor
                  retired
                  unionDigest
                  wrongHeraldEpochClaim
                  acceptanceDigest
              )
          )
    other -> assertFailure ("unexpected terminal union-acceptance projection: " <> show other)

assertTerminalUnionEstablishedAuthentication :: IO ()
assertTerminalUnionEstablishedAuthentication = do
  projected <-
    projectedControlDto
      (Input.PeerTerminalSourceUnionEstablished terminalUnionEstablished)
  case projected of
    Protocol.TerminalSourceUnionEstablishedControlDto
      (Protocol.TerminalSourceUnionEstablishedDto unionDto acceptancesDto) ->
        case NonEmpty.toList (Protocol.terminalSourceUnionAcceptanceEntries acceptancesDto) of
          Protocol.TerminalSourceUnionAcceptanceDto
            _
            successor
            retired
            unionDigest
            reporter
            acceptanceDigest
            : remaining -> do
              let mutatedAcceptance =
                    Protocol.TerminalSourceUnionAcceptanceDto
                      wrongMembershipGenerationClaim
                      successor
                      retired
                      unionDigest
                      reporter
                      acceptanceDigest
                  mutatedAcceptances =
                    mustAdmit
                      ( Protocol.terminalSourceUnionAcceptancesDto
                          (mutatedAcceptance : remaining)
                      )
              assertControlAdmissionError
                "an established acceptance must name the enclosing union coordinates"
                PeerRpcClaimContradiction
                ( Protocol.TerminalSourceUnionEstablishedControlDto
                    ( Protocol.TerminalSourceUnionEstablishedDto
                        unionDto
                        mutatedAcceptances
                    )
                )
          [] -> assertFailure "projected terminal union establishment has no acceptances"
    other -> assertFailure ("unexpected terminal union-establishment projection: " <> show other)

caseTerminalSourceUnionOccurrenceClaim :: IO ()
caseTerminalSourceUnionOccurrenceClaim = do
  unionControl <-
    projectedControlDto
      (Input.PeerTerminalSourceUnionAnnounced terminalUnionAnnounce)
  relayControl <-
    projectedControlDto
      (Input.PeerTerminalSourcePayloadRelayed terminalPayloadRelay)
  case (unionControl, relayControl) of
    ( Protocol.TerminalSourceUnionAnnouncedDto
        (Protocol.TerminalSourceUnionAnnounceDto _ unionDto),
      Protocol.TerminalSourcePayloadRelayedDto
        (Protocol.TerminalSourcePayloadRelayDto _ _ _ _ _ occurrenceDto publicationDigest _)
      ) -> do
        assertEqual
          "the exact retained occurrence and digest are present"
          (Right True)
          (admitTerminalSourceUnionOccurrenceClaim unionDto occurrenceDto publicationDigest)
        assertEqual
          "the same occurrence with another publication digest is absent"
          (Right False)
          ( admitTerminalSourceUnionOccurrenceClaim
              unionDto
              occurrenceDto
              wrongStructuralPublicationDigestClaim
          )
    other -> assertFailure ("unexpected terminal union/relay projections: " <> show other)

caseVectorMemberSetContradiction :: IO ()
caseVectorMemberSetContradiction = do
  envelope <-
    projectOrFail
      ( projectPeerControl
          (Input.PeerStructuralAppliedReported structuralReportA)
      )
  case envelope of
    Protocol.PeerControlEnvelope
      _
      ( Protocol.StructuralAppliedReportedDto
          (Protocol.StructuralAppliedReportDto reporter vector control)
        ) -> do
        contradictoryMembers <-
          projectOrFail
            (Protocol.memberSetDigestClaim (fixtureBytes 0x72))
        contradictoryVector <-
          projectOrFail
            ( Protocol.structuralVersionVectorDto
                (Protocol.structuralVersionVectorMembershipGeneration vector)
                contradictoryMembers
                ( NonEmpty.toList
                    (Protocol.structuralVersionVectorEntries vector)
                )
            )
        assertEqual
          "member-set claim does not name the vector keys"
          (Left PeerRpcClaimContradiction)
          ( admitEstablishedPeerEnvelope
              ( Protocol.PeerControlEnvelope
                  mempty
                  ( Protocol.StructuralAppliedReportedDto
                      ( Protocol.StructuralAppliedReportDto
                          reporter
                          contradictoryVector
                          control
                      )
                  )
              )
          )
    other -> assertFailure ("unexpected structural report projection: " <> show other)

caseControlCauseRoundTrip :: IO ()
caseControlCauseRoundTrip = do
  decision <-
    projectOrFail
      (Identity.mkLabelDecisionId (ByteString.replicate 32 77))
  labelCause <-
    projectOrFail
      ( StructuralConsequence.labelReleaseCause
          decision
          (Identity.controlIndex 8)
      )
  endCause <-
    projectOrFail
      ( StructuralConsequence.processEndCause
          processEpochA
          (Identity.controlIndex 9)
      )
  mapM_ assertCause [labelCause, endCause]
  probe <-
    projectOrFail
      (Disappearance.deriveDisappearanceProbeId (Identity.controlIndex 10))
  disappearanceCause <-
    projectOrFail
      ( StructuralConsequence.predefinedDisappearanceCause
          probe
          (Identity.controlIndex 11)
      )
  assertCause disappearanceCause
  where
    assertCause cause = do
      control <- controlForCause cause
      envelope <- projectOrFail (projectPeerControl control)
      framed <- frameRoundTrip envelope
      assertEqual
        (show cause)
        (Right (EstablishedPeerControl mempty control))
        (admitEstablishedPeerEnvelope framed)

    controlForCause cause = do
      obligation <-
        projectOrFail
          ( Alignment.alignmentObligation
              ( Alignment.alignmentObligationId
                  heraldEpochA
                  Alignment.firstAlignmentObligationSequence
              )
              cause
              sortId
              occurrence
              generationA
              (Alignment.destinationStore deltaA incarnationA :| [])
              generationB
              Route.Normal
          )
      subscribe <-
        projectOrFail
          ( Alignment.alignmentSubscribe
              obligation
              ( Alignment.alignmentSubscriptionId
                  heraldEpochA
                  Alignment.firstAlignmentSubscriptionSequence
              )
              incarnationB
              ( Alignment.BootstrapProof
                  generationA
                  (DomainAlignment.deriveBootstrapEvidenceDigest "control-cause")
              )
          )
      pure
        ( Input.PeerAlignmentControl
            (Alignment.AlignmentSubscribeRequested subscribe)
        )

casePredefinedRoles :: IO ()
casePredefinedRoles =
  mapM_
    ( \(offset, role) -> do
        let route =
              Placement.privateSystemViewDeltaRoute
                role
                (deltaAt (40 + offset))
                sortId
                occurrence
                (incarnationAt (60 + offset))
            roleSnapshot =
              mustAdmit
                ( Placement.placementSnapshot
                    heraldEpochA
                    placementSequence
                    [route]
                )
            control =
              Input.PeerPlacementUpdate
                (Placement.FullPlacementSnapshot roleSnapshot)
        assertControlRoundTrip control
    )
    (zip [0 ..] ([minBound .. maxBound] :: [Startup.PredefinedSortRole]))

caseReplicaStrengths :: IO ()
caseReplicaStrengths =
  mapM_
    ( \strength -> do
        let attempt = publicationAttemptFor (publicationBatchFor strength)
            expected = Stream.peerDispatchAttemptItem attempt
        envelope <- projectOrFail (projectPeerLogicalAttempt attempt)
        case admitEstablishedPeerEnvelope envelope of
          Right (EstablishedPeerPublication _ admitted) ->
            assertEqual
              (show strength)
              expected
              (PeerDispatch.peerLogicalSequencedItem admitted)
          other -> assertFailure ("unexpected publication admission: " <> show other)
    )
    ([minBound .. maxBound] :: [Route.ReplicaStrength])

casePublicationRoundTrip :: IO ()
casePublicationRoundTrip =
  mapM_ assertAuthorityRoundTrip authorityEpochs
  where
    assertAuthorityRoundTrip authority = do
      let attempt =
            publicationAttemptFor
              (publicationBatchForAuthority authority Route.Normal)
      envelope <- projectOrFail (projectPeerLogicalAttempt attempt)
      framed <- frameRoundTrip envelope
      case admitEstablishedPeerEnvelope framed of
        Right (EstablishedPeerPublication _ admitted) ->
          assertEqual
            (show (Identity.authorityEpochView authority))
            (Stream.peerDispatchAttemptItem attempt)
            (PeerDispatch.peerLogicalSequencedItem admitted)
        other -> assertFailure ("unexpected publication admission: " <> show other)

caseRouteCutoverRoundTrip :: IO ()
caseRouteCutoverRoundTrip = do
  envelope <- projectOrFail (projectPeerLogicalAttempt routeCutoverAttempt)
  framed <- frameRoundTrip envelope
  case admitEstablishedPeerEnvelope framed of
    Right (EstablishedPeerPublication _ admitted) ->
      assertEqual
        "same direction, position, digest, and cutover marker"
        (Stream.peerDispatchAttemptItem routeCutoverAttempt)
        (PeerDispatch.peerLogicalSequencedItem admitted)
    other -> assertFailure ("unexpected route-cutover admission: " <> show other)
  case envelope of
    Protocol.PeerPublicationEnvelope
      _
      (Protocol.PeerRouteCutoverDto direction sequenceNumber digest markerDto) -> do
        let Protocol.AlignmentRouteCutoverMarkerDto plan _ source predecessor = markerDto
            mutated =
              Protocol.PeerRouteCutoverDto
                direction
                sequenceNumber
                digest
                ( Protocol.AlignmentRouteCutoverMarkerDto
                    plan
                    protocolGenerationB
                    source
                    predecessor
                )
        assertEqual
          "the carried digest binds the generation"
          (Left PeerRpcPublicationDigestMismatch)
          ( admitEstablishedPeerEnvelope
              (Protocol.PeerPublicationEnvelope mempty mutated)
          )
    other -> assertFailure ("unexpected route-cutover envelope: " <> show other)

caseDisappearanceMarkerRoundTrip :: IO ()
caseDisappearanceMarkerRoundTrip = do
  let payload = disappearanceMarkerPayloadAt directionAB streamSequence1
      attempt =
        peerAttempt
          ( Stream.sequencedItem
              directionAB
              streamSequence1
              (Payload.peerLogicalPayloadDigest payload)
              payload
          )
      subscription =
        Alignment.alignmentSubscriptionId
          heraldEpochB
          (mustAdmit (Alignment.mkAlignmentSubscriptionSequence 1))
      control =
        Input.PeerAlignmentControl
          ( Alignment.AlignmentProbeMarker
              disappearanceProbe
              subscription
              (DomainAlignment.storeRevisionFromWord64 7)
          )
  envelope <- projectOrFail (projectPeerLogicalAttempt attempt)
  framed <- frameRoundTrip envelope
  case admitEstablishedPeerEnvelope framed of
    Right (EstablishedPeerPublication _ admitted) ->
      assertEqual
        "same immutable publication marker"
        (Stream.peerDispatchAttemptItem attempt)
        (PeerDispatch.peerLogicalSequencedItem admitted)
    other -> assertFailure ("disappearance marker admission: " <> show other)
  controlEnvelope <- projectOrFail (projectPeerControl control)
  assertEqual
    "alignment marker preserves probe, subscription, and watermark"
    (Right (EstablishedPeerControl mempty control))
    (admitEstablishedPeerEnvelope controlEnvelope)
  case envelope of
    Protocol.PeerPublicationEnvelope
      _
      ( Protocol.PeerDisappearanceProbeMarkerDto
          direction
          sequenceNumber
          digest
          (Protocol.DisappearanceProbeMarkerDto probe coordinate innerDirection innerSequence)
        ) -> do
        let mutate outerDirection outerSequence claimedDigest markerProbe changedCoordinate =
              Protocol.PeerPublicationEnvelope
                mempty
                ( Protocol.PeerDisappearanceProbeMarkerDto
                    outerDirection
                    outerSequence
                    claimedDigest
                    (Protocol.DisappearanceProbeMarkerDto markerProbe changedCoordinate innerDirection innerSequence)
                )
            Protocol.DisappearanceCoordinateDto subject generation members = coordinate
            changedSubject = mustAdmit (Protocol.disappearanceSubjectDigestClaim (fixtureBytes 0x79))
            changedProbe =
              mustAdmit
                ( Protocol.disappearanceProbeIdDto
                    (Protocol.disappearanceProbeIdDigest probe)
                    (Protocol.controlIndexDto 8)
                )
        assertEqual
          "outer stream sequence must equal immutable marker sequence"
          (Left PeerRpcDisappearanceMarkerCoordinateMismatch)
          ( admitEstablishedPeerEnvelope
              (mutate direction (mustAdmit (Protocol.positiveStreamSequenceDto 2)) digest probe coordinate)
          )
        assertEqual
          "changed subject requires a new marker digest"
          (Left PeerRpcPublicationDigestMismatch)
          ( admitEstablishedPeerEnvelope
              ( mutate
                  direction
                  sequenceNumber
                  digest
                  probe
                  (Protocol.DisappearanceCoordinateDto changedSubject generation members)
              )
          )
        assertEqual
          "probe digest authenticates its committed Open coordinate"
          (Left PeerRpcClaimContradiction)
          ( admitEstablishedPeerEnvelope
              ( mutate
                  direction
                  sequenceNumber
                  digest
                  changedProbe
                  (Protocol.DisappearanceCoordinateDto subject generation members)
              )
          )
    other -> assertFailure ("disappearance envelope arm: " <> show other)

caseLogicalOrderingReconnect :: IO ()
caseLogicalOrderingReconnect = do
  let binding1 = mustAdmit (Stream.mkPeerDispatchBindingGeneration 1)
      binding2 = mustAdmit (Stream.mkPeerDispatchBindingGeneration 2)
      publicationPayload =
        Payload.PeerLogicalPublication
          (Publication.presentedOrdinaryPeerPublication (publicationBatchFor Route.Normal))
      constantPayload payload =
        StreamState.sequenceBoundPeerItem
          (\_ _ -> Payload.peerLogicalPayloadItem payload)
      requests =
        Map.singleton
          heraldEpochB
          ( constantPayload publicationPayload
              :| [ constantPayload routeCutoverPayload,
                   Payload.peerLogicalDisappearanceProbeMarkerItem
                     disappearanceProbe
                     disappearanceCoordinate
                 ]
          )
      pristine = StreamState.initialState heraldEpochA
  bindingPlan <-
    projectOrFail
      (StreamState.prepareDispatchBinding directionAB binding1 pristine)
  let bound = fst (StreamState.commitDispatchBinding bindingPlan)
  enqueuePlan <-
    projectOrFail (StreamState.prepareSequenceBoundEnqueue requests bound)
  assigned <- case Map.lookup heraldEpochB (StreamState.preparedEnqueueAssignments enqueuePlan) of
    Just values -> pure values
    Nothing -> assertFailure "logical enqueue omitted destination assignment"
  assertEqual
    "publication, cutover, and disappearance marker receive one exact logical order"
    [ publicationPayload,
      routeCutoverPayload,
      disappearanceMarkerPayloadAt directionAB streamSequence3
    ]
    (fmap Stream.sequencedItemPayload (toList assigned))
  firstTicket <- case StreamState.preparedEnqueueDispatchTickets enqueuePlan of
    [ticket] -> pure ticket
    tickets -> assertFailure ("logical enqueue tickets: " <> show (length tickets))
  let ready = fst (StreamState.commitEnqueue enqueuePlan)
  firstSelection <-
    projectOrFail
      (StreamState.prepareDispatchSelection firstTicket binding1 ready)
  firstAttempt <- case StreamState.preparedDispatchSelectionAttempt firstSelection of
    Just attempt -> pure attempt
    Nothing -> assertFailure "publication dispatch selection omitted attempt"
  let attempting = fst (StreamState.commitDispatchSelection firstSelection)
  assertEqual
    "the marker cannot overtake the publication"
    publicationPayload
    (Stream.sequencedItemPayload (Stream.peerDispatchAttemptItem firstAttempt))
  loss <-
    projectOrFail
      (StreamState.prepareDispatchBindingLoss directionAB binding1 attempting)
  let unbound = fst (StreamState.commitDispatchBindingLoss loss)
  replacement <-
    projectOrFail
      (StreamState.prepareDispatchBinding directionAB binding2 unbound)
  replacementTicket <- case StreamState.preparedDispatchBindingTicket replacement of
    Just ticket -> pure ticket
    Nothing -> assertFailure "reconnect omitted retained publication ticket"
  let rebound = fst (StreamState.commitDispatchBinding replacement)
  reselection <-
    projectOrFail
      (StreamState.prepareDispatchSelection replacementTicket binding2 rebound)
  retransmission <- case StreamState.preparedDispatchSelectionAttempt reselection of
    Just attempt -> pure attempt
    Nothing -> assertFailure "reconnect omitted retained publication attempt"
  assertEqual
    "reconnect retransmits the exact publication assignment"
    (Stream.peerDispatchAttemptItem firstAttempt)
    (Stream.peerDispatchAttemptItem retransmission)
  let reattempting = fst (StreamState.commitDispatchSelection reselection)
  completion <-
    projectOrFail
      ( StreamState.prepareCompletedAck
          directionAB
          (Stream.streamPrefixThrough streamSequence1)
          reattempting
      )
  markerTicket <- case StreamState.preparedCompletedAckDispatchTicket completion of
    Just ticket -> pure ticket
    Nothing -> assertFailure "publication completion omitted cutover ticket"
  let afterPublication = fst (StreamState.commitCompletedAck completion)
  markerSelection <-
    projectOrFail
      (StreamState.prepareDispatchSelection markerTicket binding2 afterPublication)
  markerAttempt <- case StreamState.preparedDispatchSelectionAttempt markerSelection of
    Just attempt -> pure attempt
    Nothing -> assertFailure "cutover dispatch selection omitted attempt"
  assertEqual
    "the cutover follows the completed publication"
    routeCutoverPayload
    (Stream.sequencedItemPayload (Stream.peerDispatchAttemptItem markerAttempt))
  markerEnvelope <- projectOrFail (projectPeerLogicalAttempt markerAttempt)
  case markerEnvelope of
    Protocol.PeerPublicationEnvelope _ Protocol.PeerRouteCutoverDto {} -> pure ()
    other -> assertFailure ("logical cutover projection: " <> show other)
  let afterCutover = fst (StreamState.commitDispatchSelection markerSelection)
  cutoverCompletion <-
    projectOrFail
      ( StreamState.prepareCompletedAck
          directionAB
          (Stream.streamPrefixThrough streamSequence2)
          afterCutover
      )
  disappearanceTicket <- case StreamState.preparedCompletedAckDispatchTicket cutoverCompletion of
    Just ticket -> pure ticket
    Nothing -> assertFailure "cutover completion omitted disappearance marker ticket"
  let afterCutoverCompletion = fst (StreamState.commitCompletedAck cutoverCompletion)
  disappearanceSelection <-
    projectOrFail
      (StreamState.prepareDispatchSelection disappearanceTicket binding2 afterCutoverCompletion)
  disappearanceAttempt <- case StreamState.preparedDispatchSelectionAttempt disappearanceSelection of
    Just attempt -> pure attempt
    Nothing -> assertFailure "disappearance selection omitted retained marker"
  assertEqual
    "the disappearance marker follows the completed cutover at its assigned coordinate"
    (disappearanceMarkerPayloadAt directionAB streamSequence3)
    (Stream.sequencedItemPayload (Stream.peerDispatchAttemptItem disappearanceAttempt))
  let markerInFlight = fst (StreamState.commitDispatchSelection disappearanceSelection)
  markerLoss <-
    projectOrFail
      (StreamState.prepareDispatchBindingLoss directionAB binding2 markerInFlight)
  let markerUnbound = fst (StreamState.commitDispatchBindingLoss markerLoss)
      binding3 = mustAdmit (Stream.mkPeerDispatchBindingGeneration 3)
  markerReboundPlan <-
    projectOrFail
      (StreamState.prepareDispatchBinding directionAB binding3 markerUnbound)
  markerRepairTicket <- case StreamState.preparedDispatchBindingTicket markerReboundPlan of
    Just ticket -> pure ticket
    Nothing -> assertFailure "one-loss repair omitted disappearance marker"
  markerRepair <-
    projectOrFail
      ( StreamState.prepareDispatchSelection
          markerRepairTicket
          binding3
          (fst (StreamState.commitDispatchBinding markerReboundPlan))
      )
  repairedAttempt <- case StreamState.preparedDispatchSelectionAttempt markerRepair of
    Just attempt -> pure attempt
    Nothing -> assertFailure "repair selection omitted disappearance marker"
  assertEqual
    "one loss repairs the exact allocated disappearance marker"
    (Stream.peerDispatchAttemptItem disappearanceAttempt)
    (Stream.peerDispatchAttemptItem repairedAttempt)
  repairedEnvelope <- projectOrFail (projectPeerLogicalAttempt repairedAttempt)
  framed <- frameRoundTrip repairedEnvelope
  case admitEstablishedPeerEnvelope framed of
    Right (EstablishedPeerPublication _ admitted) ->
      assertEqual
        "repaired marker survives current EPRP framing"
        (Stream.peerDispatchAttemptItem disappearanceAttempt)
        (PeerDispatch.peerLogicalSequencedItem admitted)
    other -> assertFailure ("repaired disappearance marker admission: " <> show other)
  finalCompletion <-
    projectOrFail
      ( StreamState.prepareCompletedAck
          directionAB
          (Stream.streamPrefixThrough streamSequence3)
          (fst (StreamState.commitDispatchSelection markerRepair))
      )
  assertEqual
    "disappearance marker is the final retained logical item"
    Nothing
    (StreamState.preparedCompletedAckDispatchTicket finalCompletion)
  where
    toList (first :| remaining) = first : remaining

caseMalformedCanonicalValue :: IO ()
caseMalformedCanonicalValue = do
  dto <- projectedPublicationDto publicationAttempt
  let malformed = mapPublicationBatch (replaceCanonicalBytes (ByteString.singleton 0xff)) dto
  assertEqual
    "malformed payload admits neither publication nor attached progress"
    (Left PeerRpcCanonicalValueContradiction)
    (admitEstablishedPeerEnvelope (Protocol.PeerPublicationEnvelope (Receipt.receiptRetirementPrefix (Just 3)) malformed))

caseCarriedDigestMismatch :: IO ()
caseCarriedDigestMismatch = do
  dto <- projectedPublicationDto publicationAttempt
  case dto of
    Protocol.PeerPublicationDto direction sequenceNumber digest batch -> do
      let mutatedDigest =
            mustAdmit
              ( Protocol.peerItemDigestClaim
                  (ByteString.map complement (Protocol.peerItemDigestClaimBytes digest))
              )
          mutated =
            Protocol.PeerPublicationDto
              direction
              sequenceNumber
              mutatedDigest
              batch
      assertEqual
        "carried digest mismatch"
        (Left PeerRpcPublicationDigestMismatch)
        (admitEstablishedPeerEnvelope (Protocol.PeerPublicationEnvelope mempty mutated))
    Protocol.PeerRouteCutoverDto {} ->
      assertFailure "publication projection produced a route-cutover arm"
    Protocol.PeerDisappearanceProbeMarkerDto {} ->
      assertFailure "publication projection produced a disappearance-marker arm"

caseEveryBatchFieldInDigest :: IO ()
caseEveryBatchFieldInDigest = do
  dto <- projectedPublicationDto publicationAttempt
  let mutations =
        [ replacePublicationNabla (protocolNablaClaim nablaB),
          replacePublicationAuthority protocolStructuralAuthority,
          replacePublicationAuthority protocolLabelAuthority,
          replacePublicationSequence (Protocol.nablaSequenceDto 3),
          replacePublicationSource (protocolHeraldEpochClaim heraldEpochB),
          replaceSourceProcess (protocolProcessEpochClaim processEpochB),
          replaceSortId sortIdB,
          replaceOccurrence (protocolOccurrenceClaim occurrenceB),
          replaceCanonicalBytes canonicalFalseBytes,
          replaceSourceStrength Protocol.WeakReplicaDto,
          replaceSourceTopology protocolTopologyCutB,
          replaceControlIndex (Protocol.controlIndexDto 8),
          replaceDestinations
            ( protocolDestinations
                (Protocol.PublicationDestinationDto protocolDeltaB protocolIncarnationA Protocol.NormalReplicaDto)
            ),
          replaceDestinations
            ( protocolDestinations
                (Protocol.PublicationDestinationDto protocolDeltaA protocolIncarnationB Protocol.NormalReplicaDto)
            ),
          replaceDestinations
            ( protocolDestinations
                (Protocol.PublicationDestinationDto protocolDeltaA protocolIncarnationA Protocol.WeakReplicaDto)
            )
        ]
  mapM_
    ( \(index, mutation) ->
        assertEqual
          ("batch mutation " <> show index)
          (Left PeerRpcPublicationDigestMismatch)
          ( admitEstablishedPeerEnvelope
              ( Protocol.PeerPublicationEnvelope
                  mempty
                  (mapPublicationBatch mutation dto)
              )
          )
    )
    (zip [(1 :: Int) ..] mutations)

caseOwnerPublicationFault :: IO ()
caseOwnerPublicationFault = do
  let goodItem = Stream.peerDispatchAttemptItem publicationAttempt
      wrongDigest =
        mustAdmit
          ( Stream.mkPeerItemDigest
              ( ByteString.map
                  complement
                  (Stream.peerItemDigestBytes (Stream.sequencedItemDigest goodItem))
              )
          )
      inconsistentItem =
        Stream.sequencedItem
          (Stream.sequencedItemDirection goodItem)
          (Stream.sequencedItemSequence goodItem)
          wrongDigest
          (Stream.sequencedItemPayload goodItem)
      inconsistentAttempt = peerAttempt inconsistentItem
  assertEqual
    "owner item digest"
    (Left PeerRpcOwnerPublicationContradiction)
    (projectPeerLogicalAttempt inconsistentAttempt)

caseOwnerControlFault :: IO ()
caseOwnerControlFault = do
  let incoherent =
        Stream.resumeResponse
          Stream.emptyStreamPrefix
          (Stream.streamPrefixThrough streamSequence1)
          streamSequence1
  assertEqual
    "completed prefix beyond received"
    (Left PeerRpcOwnerClaimContradiction)
    (projectPeerControl (Input.PeerStreamResumeAccepted incoherent))

assertControlRoundTrip :: Input.PeerControl -> IO ()
assertControlRoundTrip control = do
  envelope <- projectOrFail (projectPeerControl control)
  assertEqual
    (show control)
    (Right (EstablishedPeerControl mempty control))
    (admitEstablishedPeerEnvelope envelope)

assertControlAdmissionError ::
  String ->
  PeerRpcAdmissionError ->
  Protocol.PeerControlDto ->
  IO ()
assertControlAdmissionError label expected dto =
  assertEqual
    label
    (Left expected)
    (admitEstablishedPeerEnvelope (Protocol.PeerControlEnvelope mempty dto))

frameRoundTrip :: Protocol.PeerEnvelope -> IO Protocol.PeerEnvelope
frameRoundTrip envelope =
  case feedPeerFrame initialPeerFrameDecoder (encodePeerFrame envelope) of
    PeerFrameFeedResult [decoded] (NeedPeerFrameBytes decoder) -> do
      assertEqual "complete frame continuation" (Right ()) (finishPeerFrameDecoder decoder)
      pure decoded
    other -> fail ("unexpected EPRP frame result: " <> show other)

projectedPublicationDto :: PeerLogicalAttempt -> IO Protocol.PeerStreamItemDto
projectedPublicationDto attempt = do
  envelope <- projectOrFail (projectPeerLogicalAttempt attempt)
  case envelope of
    Protocol.PeerPublicationEnvelope _ dto -> pure dto
    other -> fail ("unexpected projected envelope: " <> show other)

projectedControlDto :: Input.PeerControl -> IO Protocol.PeerControlDto
projectedControlDto control = do
  envelope <- projectOrFail (projectPeerControl control)
  case envelope of
    Protocol.PeerControlEnvelope _ dto -> pure dto
    other -> fail ("unexpected projected control envelope: " <> show other)

projectOrFail :: (Show error) => Either error value -> IO value
projectOrFail = either (fail . show) pure

projectHelloOrFail :: Discovery.PeerHello -> IO Protocol.PeerEnvelope
projectHelloOrFail =
  projectOrFail
    . projectPeerHello genesisMembershipId genesisMemberSetDigest

firstControl :: Input.PeerControl
firstControl = case allControls of
  control : _ -> control
  [] -> error "peer control fixture catalogue is empty"

allControls :: [Input.PeerControl]
allControls =
  [ Input.PeerKnownHeralds [knownHerald],
    Input.PeerPlacementUpdate (Placement.FullPlacementSnapshot placementSnapshot),
    Input.PeerPlacementAcknowledged placementAcknowledgement,
    Input.PeerStreamReceived directionAB Stream.emptyStreamPrefix,
    Input.PeerStreamCompleted directionAB (Stream.streamPrefixCompletion (Stream.streamPrefixThrough streamSequence1)),
    Input.PeerStreamFrontierAdvanced directionAB streamSequence3,
    Input.PeerStreamResumeOffered resumeOffer,
    Input.PeerStreamResumeAccepted resumeResponse,
    Input.PeerStructuralAppliedReported structuralReportA,
    Input.PeerTopologyCutAnnounced topologyAnnounce,
    Input.PeerTopologyCutAccepted topologyAcceptanceB,
    Input.PeerTopologyCutEstablished topologyEstablished,
    Input.PeerTopologyCutEstablishedAcknowledged topologyEstablishedAck,
    Input.PeerAlignmentDeliveryProgress,
    Input.PeerLabelInstalled
      ( Label.labelInstallationReport
          (identity Identity.mkLabelDecisionId 0x74)
          heraldEpochA
          (Identity.controlIndex 7)
          (mustAdmit (Label.mkLabelOutcomeDigest (fixtureBytes 0x75)))
      )
  ]
    <> step15Controls
    <> preparationControls

preparationControls :: [Input.PeerControl]
preparationControls =
  map
    Input.PeerPreparationControl
    ( [ Preparation.PreparationOffered reference locator "retained canonical bytes",
        Preparation.PreparationCancelled reference,
        Preparation.PreparationQueried reference
      ]
        <> [Preparation.PreparationReplied reference status | status <- statuses]
    )
  where
    reference = mustAdmit (Lifecycle.childPreparation (ByteString.replicate 32 41) 3 7)
    locator = mustAdmit (Lifecycle.heraldLocator "localhost" 41003)
    descriptor = mustAdmit (Lifecycle.connectionDescriptor locator (ByteString.replicate 32 1) (Identity.heraldEpochBytes heraldEpochA) (Identity.processEpochIdBytes processEpochA))
    child = Preparation.RemoteChild processEpochA (Identity.controlIndex 9) descriptor
    statuses =
      [ Preparation.RemotePreparationPending [],
        Preparation.RemotePreparationPending ["reader"],
        Preparation.RemotePreparationAvailable child Nothing,
        Preparation.RemotePreparationAvailable child (Just ["reader"]),
        Preparation.RemotePreparationClaimed child,
        Preparation.RemotePreparationCancelled,
        Preparation.RemotePreparationFailed Lifecycle.StartupNotLive
      ]

step15Controls :: [Input.PeerControl]
step15Controls =
  [ Input.PeerDirectFailureProbeRequested directProbeRequest,
    Input.PeerDirectFailureProbeResponded directProbeReachable,
    Input.PeerDirectFailureProbeResponded directProbeUnreachable,
    Input.PeerTerminalSourceInventoryAdvertised terminalInventory,
    Input.PeerTerminalSourcePayloadRequested terminalPayloadRequest,
    Input.PeerTerminalSourcePayloadRelayed terminalPayloadRelay,
    Input.PeerTerminalSourceUnionAnnounced terminalUnionAnnounce,
    Input.PeerTerminalSourceUnionAccepted terminalUnionAcceptance,
    Input.PeerTerminalSourceUnionEstablished terminalUnionEstablished
  ]

structuralReportA, structuralReportB :: GraphProtocol.StructuralAppliedReport
structuralReportA =
  GraphProtocol.structuralAppliedReport
    heraldEpochA
    genesisStructuralVector
    (Identity.controlIndex 0)
structuralReportB =
  GraphProtocol.structuralAppliedReport
    heraldEpochB
    genesisStructuralVector
    (Identity.controlIndex 0)

topologyAnnounce :: GraphProtocol.TopologyCutAnnounce
topologyAnnounce = GraphProtocol.topologyCutAnnounce heraldEpochA canonicalTopologyCut

topologyAcceptanceA, topologyAcceptanceB :: GraphProtocol.TopologyCutAcceptance
topologyAcceptanceA =
  GraphProtocol.topologyCutAcceptance canonicalTopologyCutId structuralReportA
topologyAcceptanceB =
  GraphProtocol.topologyCutAcceptance canonicalTopologyCutId structuralReportB

topologyEstablished :: GraphProtocol.TopologyCutEstablished
topologyEstablished =
  mustAdmit
    ( GraphProtocol.topologyCutEstablished
        canonicalTopologyCutId
        canonicalTopologyCut
        [topologyAcceptanceB, topologyAcceptanceA]
    )

topologyEstablishedAck :: GraphProtocol.TopologyCutEstablishedAck
topologyEstablishedAck =
  GraphProtocol.topologyCutEstablishedAck
    canonicalTopologyCutId
    heraldEpochB
    (Membership.heraldMembershipGenerationId genesisMembership)

canonicalTopologyCut :: Topology.TopologyCut
canonicalTopologyCut =
  mustAdmit
    ( Topology.topologyCut
        (Topology.sameGenerationPredecessor topologyCutA)
        ( Topology.topologyFrontier
            genesisStructuralVector
            (Identity.controlIndex 0)
        )
        (Topology.deriveTopologyOccurrenceDigest "peer-rpc-topology")
    )

canonicalTopologyCutId :: Identity.TopologyCutId
canonicalTopologyCutId = Topology.deriveTopologyCutId canonicalTopologyCut

admissionRecipe :: Topology.HeraldJoinBaseRecipe
admissionRecipe =
  mustAdmit
    ( Topology.heraldJoinBaseRecipe
        (mustAdmit (Membership.deriveHeraldAdmissionId (Identity.controlIndex 3)))
        (identity Identity.mkHeraldEpoch 0x6f)
        genesisMembership
        ( mustAdmit
            ( Topology.topologyCut
                (Topology.sameGenerationPredecessor canonicalTopologyCutId)
                (Topology.topologyFrontier genesisStructuralVector (Identity.controlIndex 3))
                (Topology.deriveTopologyOccurrenceDigest "peer-rpc-join-anchor")
            )
        )
        (Topology.deriveTopologyOccurrenceDigest "peer-rpc-join-contribution")
    )

admissionTopologyCut :: Topology.TopologyCut
admissionTopologyCut =
  snd
    ( mustAdmit
        ( Topology.activateHeraldJoinBase
            (Identity.controlIndex 4)
            (Topology.deriveTopologyOccurrenceDigest "peer-rpc-admitted-topology")
            admissionRecipe
        )
    )

successorTopologyCut :: Topology.TopologyCut
successorTopologyCut =
  mustAdmit
    ( Topology.topologyCut
        successorPredecessor
        ( Topology.topologyFrontier
            successorStructuralVector
            (Identity.controlIndex 1)
        )
        (Topology.deriveTopologyOccurrenceDigest "peer-rpc-successor-topology")
    )

successorPredecessor :: Topology.TopologyPredecessor
successorPredecessor =
  mustAdmit
    ( Topology.membershipSuccessorPredecessor
        terminalLineage
        canonicalTopologyCutId
        genesisStructuralVector
        successorStructuralVector
        terminalUnionDigest
    )

terminalUnionDigest :: Topology.TerminalSourceUnionDigest
terminalUnionDigest =
  mustAdmit (Topology.mkTerminalSourceUnionDigest (fixtureBytes 0x73))

genesisStructuralVector, successorStructuralVector :: Structural.StructuralVersionVector
genesisStructuralVector = Structural.emptyStructuralVersionVector genesisMembership
successorStructuralVector = Structural.emptyStructuralVersionVector successorMembership

genesisMembership, successorMembership :: Membership.HeraldMembershipGeneration
genesisMembership =
  mustAdmit
    ( Membership.genesisHeraldMembershipGeneration
        systemId
        (heraldEpochA :| [heraldEpochB])
    )
successorMembership =
  mustAdmit
    ( Membership.retireHeraldMembershipGeneration
        (Identity.controlIndex 2)
        (Membership.deriveFailureProbeResolutionId (mustAdmit (Membership.deriveHeraldFailureProbeId (Identity.controlIndex 1))) Membership.RetireFailureProbeTarget)
        heraldEpochB
        genesisMembership
    )

directProbeRequest :: PeerStep15.DirectFailureProbeRequest
directProbeRequest =
  PeerStep15.directFailureProbeRequest
    (mustAdmit (Membership.deriveHeraldFailureProbeId (Identity.controlIndex 4)))
    heraldEpochB
    genesisMembershipId
    (mustAdmit (Voter.mkVoterConfigurationId (fixtureBytes 0xda)))

directProbeReachable,
  directProbeUnreachable ::
    PeerStep15.DirectFailureProbeResponse
directProbeReachable =
  PeerStep15.directFailureProbeResponse
    directProbeRequest
    PeerStep15.DirectFailureProbeReachable
directProbeUnreachable =
  PeerStep15.directFailureProbeResponse
    directProbeRequest
    PeerStep15.DirectFailureProbeUnreachable

terminalOccurrence :: TerminalSource.TerminalStructuralOccurrence
terminalOccurrence =
  let identifier =
        Identity.structuralOccurrenceId
          heraldEpochB
          (mustAdmit (Identity.mkStructuralSequence 1))
      payload = "peer-rpc-terminal-payload"
      stamp =
        mustAdmit
          ( Publication.mkStructuralOccurrenceStamp
              identifier
              genesisStructuralVector
              ( Identity.publicationId
                  nablaA
                  Identity.genesisAuthorityEpoch
                  heraldEpochB
                  (Identity.nablaSequence 1)
              )
              (TerminalSource.deriveTerminalStructuralPayloadDigest payload)
              Descriptor.NeutralVertexCarrier
          )
   in mustAdmit (TerminalSource.terminalStructuralOccurrence stamp payload)

terminalLineage :: Membership.HeraldMembershipLineage
terminalLineage = mustAdmit (Membership.heraldMembershipLineage (Membership.heraldMembershipGenerationId genesisMembership) (Membership.heraldMembershipGenerationId successorMembership) (mustAdmit (Membership.heraldMembershipHistory (genesisMembership :| [successorMembership]))))

terminalInventory :: TerminalSource.TerminalSourceInventory
terminalPayloadArchive :: TerminalSource.TerminalSourcePayloadArchive
(terminalInventory, terminalPayloadArchive) =
  mustAdmit
    ( TerminalSource.sealTerminalSourceInventory
        terminalLineage
        terminalLineage
        heraldEpochB
        heraldEpochA
        []
        []
        [terminalOccurrence]
    )

terminalPayloadRequest :: TerminalSource.TerminalSourcePayloadRequest
terminalPayloadRequest =
  case mustAdmit
    ( TerminalSource.deriveTerminalSourcePayloadRequests
        terminalLineage
        terminalLineage
        heraldEpochB
        [terminalInventory]
        TerminalSource.emptyTerminalSourcePayloadArchive
    ) of
    [request] -> request
    requests -> error ("expected one terminal payload request, got " <> show requests)

terminalPayloadRelay :: TerminalSource.TerminalSourcePayloadRelay
terminalPayloadRelay =
  mustAdmit
    ( TerminalSource.terminalSourcePayloadRelay
        terminalLineage
        terminalLineage
        heraldEpochA
        terminalPayloadRequest
        terminalInventory
        terminalOccurrence
    )

terminalSourceUnion :: TerminalSource.TerminalSourceUnion
terminalSourceUnion =
  mustAdmit
    ( TerminalSource.deriveTerminalSourceUnion
        terminalLineage
        terminalLineage
        ( mustAdmit
            ( TerminalSource.terminalSourceGenesisPredecessorBase
                genesisMembership
                canonicalTopologyCutId
            )
        )
        [terminalInventory]
        terminalPayloadArchive
        (\_ _ -> Topology.deriveTopologyOccurrenceDigest "peer-rpc-terminal-topology")
    )

terminalUnionAnnounce :: TerminalSource.TerminalSourceUnionAnnounce
terminalUnionAnnounce =
  mustAdmit
    ( TerminalSource.terminalSourceUnionAnnounce
        successorMembership
        heraldEpochA
        terminalSourceUnion
    )

terminalUnionAcceptance :: TerminalSource.TerminalSourceUnionAcceptance
terminalUnionAcceptance =
  mustAdmit
    ( TerminalSource.terminalSourceUnionAcceptance
        successorMembership
        heraldEpochA
        ( mustAdmit
            ( TerminalSource.terminalSourceUnionReady
                ( TerminalSource.terminalSourceUnionTerminalPredecessorVector
                    terminalSourceUnion
                )
                ( TerminalSource.terminalSourceUnionTerminalControlPrefix
                    terminalSourceUnion
                )
                terminalSourceUnion
            )
        )
    )

terminalUnionEstablished :: TerminalSource.TerminalSourceUnionEstablished
terminalUnionEstablished =
  mustAdmit
    ( TerminalSource.terminalSourceUnionEstablished
        successorMembership
        terminalSourceUnion
        [terminalUnionAcceptance]
    )

genesisMembershipId :: Membership.HeraldMembershipGenerationId
genesisMembershipId = Membership.heraldMembershipGenerationId genesisMembership

genesisMemberSetDigest :: Topology.MemberSetDigest
genesisMemberSetDigest =
  Membership.heraldMembershipGenerationActiveMemberSetDigest genesisMembership

peerHello :: Discovery.PeerHello
peerHello =
  Discovery.peerHello
    systemId
    heraldIdA
    heraldEpochA
    connectionNonce
    (Set.fromList [addressB, addressA])
    (Identity.controlIndex 7)
    catalogueDigest
    projectionDigest
    Nothing

knownHerald :: Discovery.KnownHerald
knownHerald =
  Discovery.knownHerald
    heraldIdB
    heraldEpochB
    (Set.fromList [addressB, addressA])

applicationRoute :: Placement.DeltaRoute
applicationRoute =
  Placement.applicationDeltaRoute
    deltaA
    sortId
    occurrence
    globalObject
    processEpochA
    incarnationA
    (Identity.controlIndex 7)

privateRoute :: Placement.DeltaRoute
privateRoute =
  Placement.privateSystemViewDeltaRoute
    Startup.DeltaRole
    deltaB
    sortId
    occurrenceB
    incarnationB

placementSnapshot :: Placement.PlacementSnapshot
placementSnapshot =
  mustAdmit
    ( Placement.placementSnapshot
        heraldEpochA
        placementSequence
        [privateRoute, applicationRoute]
    )

placementAcknowledgement :: Placement.PlacementAcknowledgement
placementAcknowledgement =
  Placement.placementAcknowledgement heraldEpochA placementSequence

resumeOffer :: Stream.ResumeOffer
resumeOffer =
  mustAdmit
    ( Stream.mkResumeOffer
        heraldEpochA
        heraldEpochB
        streamSequence2
        Stream.emptyStreamPrefix
        Stream.emptyStreamPrefix
        emptyReverseGap
    )

resumeResponse :: Stream.ResumeResponse
resumeResponse =
  Stream.resumeResponse
    Stream.emptyStreamPrefix
    Stream.emptyStreamPrefix
    streamSequence1

emptyReverseGap :: Stream.GapSummary
emptyReverseGap =
  mustAdmit
    (Stream.mkGapSummary directionBA Stream.emptyStreamPrefix [])

publicationAttempt :: PeerLogicalAttempt
publicationAttempt = publicationAttemptFor (publicationBatchFor Route.Normal)

publicationAttemptFor :: Publication.PublicationBatch -> PeerLogicalAttempt
publicationAttemptFor = publicationAttemptForItem . publicationItem

publicationItem :: Publication.PublicationBatch -> Stream.SequencedItem Payload.PeerLogicalPayload
publicationItem batch =
  let item = Publication.presentedOrdinaryPeerPublication batch
      payload = Payload.PeerLogicalPublication item
   in Stream.sequencedItem
        directionAB
        streamSequence1
        (Payload.peerLogicalPayloadDigest payload)
        payload

publicationAttemptForItem ::
  Stream.SequencedItem Payload.PeerLogicalPayload ->
  PeerLogicalAttempt
publicationAttemptForItem = peerAttempt

peerAttempt ::
  Stream.SequencedItem Payload.PeerLogicalPayload ->
  PeerLogicalAttempt
peerAttempt item =
  Stream.peerDispatchAttempt
    (mustAdmit (Stream.mkPeerDispatchBindingGeneration 1))
    (Stream.peerDispatchAttemptGenerationForOwner 1)
    item

routeCutoverAttempt :: PeerLogicalAttempt
routeCutoverAttempt =
  Stream.peerDispatchAttempt
    (mustAdmit (Stream.mkPeerDispatchBindingGeneration 1))
    (Stream.peerDispatchAttemptGenerationForOwner 1)
    ( Stream.sequencedItem
        directionAB
        streamSequence1
        (Payload.peerLogicalPayloadDigest routeCutoverPayload)
        routeCutoverPayload
    )

routeCutoverPayload :: Payload.PeerLogicalPayload
routeCutoverPayload =
  Payload.PeerLogicalRouteCutover
    ( mustAdmit
        ( Alignment.alignmentRouteCutoverMarker
            (Plan.alignmentPlanIdFromClaimedCoordinates (Debt.sortOccurrence sortId occurrence) topologyCutA (mustAdmit (DomainAlignment.physicalPlacementRevisionVector genesisMembership [(heraldEpochA, DomainAlignment.firstPlacementRevision), (heraldEpochB, DomainAlignment.firstPlacementRevision)])))
            generationA
            heraldEpochA
            heraldEpochB
        )
    )

disappearanceProbe :: Disappearance.DisappearanceProbeId
disappearanceProbe =
  mustAdmit
    (Disappearance.deriveDisappearanceProbeId (Identity.controlIndex 7))

disappearanceCoordinate :: Disappearance.DisappearanceSubjectMembershipCoordinate
disappearanceCoordinate =
  Disappearance.admitDisappearanceSubjectMembershipCoordinate
    (mustAdmit (Disappearance.mkDisappearanceSubjectDigest (fixtureBytes 0x76)))
    genesisMembershipId
    genesisMemberSetDigest

disappearanceMarkerPayloadAt ::
  Stream.StreamDirection -> Stream.StreamSequence -> Payload.PeerLogicalPayload
disappearanceMarkerPayloadAt direction sequenceNumber =
  Payload.PeerLogicalDisappearanceProbeMarker
    ( DisappearanceProtocol.disappearancePublicationMarker
        disappearanceProbe
        disappearanceCoordinate
        direction
        sequenceNumber
    )

generationA :: DomainAlignment.ContextClassGenerationId
generationA = mustAdmit (DomainAlignment.mkContextClassGenerationId (fixtureBytes 0x61))

generationB :: DomainAlignment.ContextClassGenerationId
generationB = mustAdmit (DomainAlignment.mkContextClassGenerationId (fixtureBytes 0x62))

protocolGenerationB :: Protocol.ContextClassGenerationClaim
protocolGenerationB =
  mustAdmit
    ( Protocol.contextClassGenerationClaim
        (DomainAlignment.contextClassGenerationIdBytes generationB)
    )

fixtureBytes :: Word8 -> ByteString
fixtureBytes = ByteString.replicate 32

wrongMembershipGenerationClaim :: Protocol.HeraldMembershipGenerationClaim
wrongMembershipGenerationClaim =
  mustAdmit (Protocol.heraldMembershipGenerationClaim (fixtureBytes 0xd8))

wrongHeraldEpochClaim :: Protocol.HeraldEpochClaim
wrongHeraldEpochClaim =
  mustAdmit (Protocol.heraldEpochClaim (fixtureBytes 0xd9))

wrongStructuralPublicationDigestClaim :: Protocol.StructuralPublicationDigestClaim
wrongStructuralPublicationDigestClaim =
  mustAdmit (Protocol.structuralPublicationDigestClaim (fixtureBytes 0xda))

publicationBatchFor :: Route.ReplicaStrength -> Publication.PublicationBatch
publicationBatchFor strength =
  publicationBatchForAuthority Identity.genesisAuthorityEpoch strength

publicationBatchForAuthority ::
  Identity.AuthorityEpoch ->
  Route.ReplicaStrength ->
  Publication.PublicationBatch
publicationBatchForAuthority authority strength =
  mustAdmit
    ( Publication.mkPublicationBatch
        (publicationIdentifierForAuthority authority)
        processEpochA
        sortId
        occurrence
        canonicalTrue
        Route.Normal
        topologyCutA
        (Identity.controlIndex 7)
        (Publication.publicationDestination deltaA incarnationA strength :| [])
    )

publicationIdentifierForAuthority :: Identity.AuthorityEpoch -> Identity.PublicationId
publicationIdentifierForAuthority authority =
  Identity.publicationId
    nablaA
    authority
    heraldEpochA
    (Identity.nablaSequence 2)

authorityEpochs :: [Identity.AuthorityEpoch]
authorityEpochs =
  [ Identity.genesisAuthorityEpoch,
    Identity.structuralAuthorityEpoch
      ( Identity.structuralOccurrenceId
          heraldEpochB
          (mustAdmit (Identity.mkStructuralSequence 1))
      )
      topologyCutB,
    Identity.labelAuthorityEpoch (Identity.controlIndex 9)
  ]

canonicalTrue :: Value.CanonicalValueBytes
canonicalTrue = Value.canonicalValueBytes (Value.boolValue True)

canonicalFalseBytes :: ByteString
canonicalFalseBytes =
  Value.canonicalValueByteString (Value.canonicalValueBytes (Value.boolValue False))

placementSequence :: Placement.PlacementSequence
placementSequence = mustAdmit (Placement.mkPlacementSequence 1)

streamSequence1 :: Stream.StreamSequence
streamSequence1 = mustAdmit (Stream.mkStreamSequence 1)

streamSequence2 :: Stream.StreamSequence
streamSequence2 = mustAdmit (Stream.mkStreamSequence 2)

streamSequence3 :: Stream.StreamSequence
streamSequence3 = mustAdmit (Stream.mkStreamSequence 3)

directionAB :: Stream.StreamDirection
directionAB = mustAdmit (Stream.mkStreamDirection heraldEpochA heraldEpochB)

directionBA :: Stream.StreamDirection
directionBA = mustAdmit (Stream.mkStreamDirection heraldEpochB heraldEpochA)

connectionNonce :: Discovery.ConnectionNonce
connectionNonce = Discovery.connectionNonce 11

addressA :: Discovery.PeerAddress
addressA = Discovery.peerAddress "tcp://127.0.0.1:41001"

addressB :: Discovery.PeerAddress
addressB = Discovery.peerAddress "tcp://[::1]:41002"

systemId :: Identity.SystemId
systemId = identity Identity.mkSystemId 1

heraldIdA :: Identity.HeraldId
heraldIdA = identity Identity.mkHeraldId 2

heraldIdB :: Identity.HeraldId
heraldIdB = identity Identity.mkHeraldId 3

heraldEpochA :: Identity.HeraldEpoch
heraldEpochA = identity Identity.mkHeraldEpoch 4

heraldEpochB :: Identity.HeraldEpoch
heraldEpochB = identity Identity.mkHeraldEpoch 5

globalObject :: Identity.GlobalObjectId
globalObject = identity Identity.mkGlobalObjectId 6

processEpochA :: Identity.ProcessEpochId
processEpochA = identity Identity.mkProcessEpochId 7

processEpochB :: Identity.ProcessEpochId
processEpochB = identity Identity.mkProcessEpochId 8

nablaA :: Identity.NablaId
nablaA = identity Identity.mkNablaId 9

nablaB :: Identity.NablaId
nablaB = identity Identity.mkNablaId 10

deltaA :: Identity.DeltaId
deltaA = deltaAt 11

deltaB :: Identity.DeltaId
deltaB = deltaAt 12

occurrence :: Identity.SortDefinitionOccurrenceId
occurrence = identity Identity.mkSortDefinitionOccurrenceId 13

occurrenceB :: Identity.SortDefinitionOccurrenceId
occurrenceB = identity Identity.mkSortDefinitionOccurrenceId 14

incarnationA :: Identity.StoreIncarnationId
incarnationA = incarnationAt 15

incarnationB :: Identity.StoreIncarnationId
incarnationB = incarnationAt 16

sortId :: Identity.SortId
sortId = identity Identity.mkSortId 17

sortIdB :: Identity.SortId
sortIdB = identity Identity.mkSortId 18

catalogueDigest :: Startup.CatalogueDigest
catalogueDigest = mustAdmit (Startup.mkCatalogueDigest (identityBytes 19))

projectionDigest :: Startup.InitialProjectionDigest
projectionDigest = mustAdmit (Startup.mkInitialProjectionDigest (identityBytes 20))

topologyCutA :: Identity.TopologyCutId
topologyCutA = identity Identity.mkTopologyCutId 21

topologyCutB :: Identity.TopologyCutId
topologyCutB = identity Identity.mkTopologyCutId 22

deltaAt :: Word8 -> Identity.DeltaId
deltaAt = identity Identity.mkDeltaId

incarnationAt :: Word8 -> Identity.StoreIncarnationId
incarnationAt = identity Identity.mkStoreIncarnationId

identity :: (Show error) => (ByteString -> Either error value) -> Word8 -> value
identity constructor = mustAdmit . constructor . identityBytes

identityBytes :: Word8 -> ByteString
identityBytes = ByteString.replicate 32

protocolNablaClaim :: Identity.NablaId -> Protocol.NablaClaim
protocolNablaClaim =
  mustAdmit . Protocol.nablaClaim . Identity.nablaIdBytes

protocolHeraldEpochClaim :: Identity.HeraldEpoch -> Protocol.HeraldEpochClaim
protocolHeraldEpochClaim =
  mustAdmit . Protocol.heraldEpochClaim . Identity.heraldEpochBytes

protocolProcessEpochClaim :: Identity.ProcessEpochId -> Protocol.ProcessEpochClaim
protocolProcessEpochClaim =
  mustAdmit . Protocol.processEpochClaim . Identity.processEpochIdBytes

protocolOccurrenceClaim ::
  Identity.SortDefinitionOccurrenceId ->
  Protocol.SortOccurrenceClaim
protocolOccurrenceClaim =
  mustAdmit
    . Protocol.sortOccurrenceClaim
    . Identity.sortDefinitionOccurrenceIdBytes

protocolTopologyCutB :: Protocol.TopologyCutClaim
protocolTopologyCutB =
  mustAdmit
    (Protocol.topologyCutClaim (Identity.topologyCutIdBytes topologyCutB))

protocolStructuralAuthority :: Protocol.AuthorityEpochDto
protocolStructuralAuthority =
  Protocol.StructuralAuthorityEpochDto
    ( Protocol.StructuralOccurrenceIdDto
        (protocolHeraldEpochClaim heraldEpochB)
        (mustAdmit (Protocol.positiveStructuralSequenceDto 1))
    )
    protocolTopologyCutB

protocolLabelAuthority :: Protocol.AuthorityEpochDto
protocolLabelAuthority = Protocol.LabelAuthorityEpochDto (Protocol.controlIndexDto 9)

protocolDeltaA :: Protocol.DeltaClaim
protocolDeltaA = mustAdmit (Protocol.deltaClaim (Identity.deltaIdBytes deltaA))

protocolDeltaB :: Protocol.DeltaClaim
protocolDeltaB = mustAdmit (Protocol.deltaClaim (Identity.deltaIdBytes deltaB))

protocolIncarnationA :: Protocol.StoreIncarnationClaim
protocolIncarnationA =
  mustAdmit (Protocol.storeIncarnationClaim (Identity.storeIncarnationIdBytes incarnationA))

protocolIncarnationB :: Protocol.StoreIncarnationClaim
protocolIncarnationB =
  mustAdmit (Protocol.storeIncarnationClaim (Identity.storeIncarnationIdBytes incarnationB))

protocolDestinations ::
  Protocol.PublicationDestinationDto ->
  Protocol.PublicationDestinationsDto
protocolDestinations = mustAdmit . Protocol.publicationDestinationsDto . pure

type BatchMutation = Protocol.PublicationBatchDto -> Protocol.PublicationBatchDto

mapPublicationBatch ::
  BatchMutation ->
  Protocol.PeerStreamItemDto ->
  Protocol.PeerStreamItemDto
mapPublicationBatch
  mutation
  (Protocol.PeerPublicationDto direction sequenceNumber digest item) =
    Protocol.PeerPublicationDto direction sequenceNumber digest (mapItem item)
    where
      mapItem (Protocol.OrdinaryPublicationDto batch) =
        Protocol.OrdinaryPublicationDto (mutation batch)
      mapItem (Protocol.StructuralPublicationDto stamp batch) =
        Protocol.StructuralPublicationDto stamp (mutation batch)
mapPublicationBatch _ marker@Protocol.PeerRouteCutoverDto {} = marker
mapPublicationBatch _ marker@Protocol.PeerDisappearanceProbeMarkerDto {} = marker

replacePublicationNabla :: Protocol.NablaClaim -> BatchMutation
replacePublicationNabla
  replacement
  ( Protocol.PublicationBatchDto
      (Protocol.PublicationIdDto _ authority sequenceNumber source)
      process
      batchSort
      batchOccurrence
      bytes
      sourceStrength
      topology
      control
      destinations
    ) =
    Protocol.PublicationBatchDto
      (Protocol.PublicationIdDto replacement authority sequenceNumber source)
      process
      batchSort
      batchOccurrence
      bytes
      sourceStrength
      topology
      control
      destinations

replacePublicationAuthority :: Protocol.AuthorityEpochDto -> BatchMutation
replacePublicationAuthority replacement =
  mapPublicationIdentifier
    ( \(Protocol.PublicationIdDto nabla _ sequenceNumber source) ->
        Protocol.PublicationIdDto nabla replacement sequenceNumber source
    )

replacePublicationSequence :: Protocol.NablaSequenceDto -> BatchMutation
replacePublicationSequence replacement =
  mapPublicationIdentifier
    ( \(Protocol.PublicationIdDto nabla authority _ source) ->
        Protocol.PublicationIdDto nabla authority replacement source
    )

replacePublicationSource :: Protocol.HeraldEpochClaim -> BatchMutation
replacePublicationSource replacement =
  mapPublicationIdentifier
    ( \(Protocol.PublicationIdDto nabla authority sequenceNumber _) ->
        Protocol.PublicationIdDto nabla authority sequenceNumber replacement
    )

mapPublicationIdentifier ::
  (Protocol.PublicationIdDto -> Protocol.PublicationIdDto) ->
  BatchMutation
mapPublicationIdentifier
  mutation
  (Protocol.PublicationBatchDto identifier process batchSort batchOccurrence bytes sourceStrength topology control destinations) =
    Protocol.PublicationBatchDto
      (mutation identifier)
      process
      batchSort
      batchOccurrence
      bytes
      sourceStrength
      topology
      control
      destinations

replaceSourceProcess :: Protocol.ProcessEpochClaim -> BatchMutation
replaceSourceProcess
  replacement
  (Protocol.PublicationBatchDto identifier _ batchSort batchOccurrence bytes sourceStrength topology control destinations) =
    Protocol.PublicationBatchDto identifier replacement batchSort batchOccurrence bytes sourceStrength topology control destinations

replaceSortId :: Identity.SortId -> BatchMutation
replaceSortId
  replacement
  (Protocol.PublicationBatchDto identifier process _ batchOccurrence bytes sourceStrength topology control destinations) =
    Protocol.PublicationBatchDto identifier process replacement batchOccurrence bytes sourceStrength topology control destinations

replaceOccurrence :: Protocol.SortOccurrenceClaim -> BatchMutation
replaceOccurrence
  replacement
  (Protocol.PublicationBatchDto identifier process batchSort _ bytes sourceStrength topology control destinations) =
    Protocol.PublicationBatchDto identifier process batchSort replacement bytes sourceStrength topology control destinations

replaceCanonicalBytes :: ByteString -> BatchMutation
replaceCanonicalBytes
  replacement
  (Protocol.PublicationBatchDto identifier process batchSort batchOccurrence _ sourceStrength topology control destinations) =
    Protocol.PublicationBatchDto identifier process batchSort batchOccurrence replacement sourceStrength topology control destinations

replaceSourceStrength :: Protocol.ReplicaStrengthDto -> BatchMutation
replaceSourceStrength
  replacement
  (Protocol.PublicationBatchDto identifier process batchSort batchOccurrence bytes _ topology control destinations) =
    Protocol.PublicationBatchDto
      identifier
      process
      batchSort
      batchOccurrence
      bytes
      replacement
      topology
      control
      ( case replacement of
          Protocol.WeakReplicaDto ->
            mustAdmit
              ( Protocol.publicationDestinationsDto
                  (fmap weakenDestination (toList (Protocol.publicationDestinationEntries destinations)))
              )
          Protocol.NormalReplicaDto -> destinations
      )
    where
      weakenDestination (Protocol.PublicationDestinationDto delta incarnation _) =
        Protocol.PublicationDestinationDto delta incarnation Protocol.WeakReplicaDto
      toList (first :| remaining) = first : remaining

replaceSourceTopology :: Protocol.TopologyCutClaim -> BatchMutation
replaceSourceTopology
  replacement
  (Protocol.PublicationBatchDto identifier process batchSort batchOccurrence bytes sourceStrength _ control destinations) =
    Protocol.PublicationBatchDto identifier process batchSort batchOccurrence bytes sourceStrength replacement control destinations

replaceControlIndex :: Protocol.ControlIndexDto -> BatchMutation
replaceControlIndex
  replacement
  (Protocol.PublicationBatchDto identifier process batchSort batchOccurrence bytes sourceStrength topology _ destinations) =
    Protocol.PublicationBatchDto identifier process batchSort batchOccurrence bytes sourceStrength topology replacement destinations

replaceDestinations :: Protocol.PublicationDestinationsDto -> BatchMutation
replaceDestinations
  replacement
  (Protocol.PublicationBatchDto identifier process batchSort batchOccurrence bytes sourceStrength topology control _) =
    Protocol.PublicationBatchDto identifier process batchSort batchOccurrence bytes sourceStrength topology control replacement

mustAdmit :: (Show error) => Either error value -> value
mustAdmit = either (error . ("invalid peer RPC fixture: " <>) . show) id
