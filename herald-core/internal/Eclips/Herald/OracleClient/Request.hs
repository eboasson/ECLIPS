{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Package-internal Oracle request evidence owned by 'OracleClient.State'.
--
-- Administration and LabelBarrier may retain an opaque 'OracleRequestRef', but
-- they cannot mint a request, choose its sequence, construct its envelope, or
-- drive its retry. Those facts remain in the Oracle-client owner and are
-- exposed here only as read-only evidence for atomic coordinators and
-- whole-state validation.
module Eclips.Herald.OracleClient.Request
  ( OracleRequestRef (..),
    oracleRequestRefRequestId,
    OracleRequestStatus (..),
    OracleSemanticIntent (..),
    OracleVoterOperation (..),
    OracleRequestWitness (..),
    oracleRequestWitnessRef,
    oracleRequestWitnessSemanticKey,
    oracleRequestWitnessIntent,
    oracleRequestWitnessBootstrap,
    oracleRequestWitnessEnvelope,
    oracleRequestWitnessStatus,
    OracleRequestDispatch (..),
    oracleRequestDispatchRef,
    oracleRequestDispatchEnvelope,
  )
where

import Data.ByteString (ByteString)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceEvidenceDigest,
    DisappearanceProbeId,
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
  )
import Eclips.Domain.Label
  ( HomeLabelAcceptanceCut,
    InitialLabelEvidence,
    LabelTarget,
    PriorAuthorityJustification,
  )
import Eclips.Domain.Membership
  ( FailureProbeResolutionId,
    HeraldFailureProbeId,
    HeraldMembershipGenerationId,
  )
import Eclips.Domain.ProcessLifecycle (ProcessEndReason)
import Eclips.Domain.ProcessStart (ProcessStart)
import Eclips.Domain.Value (Label)
import Eclips.Herald.PeerLiveness.Internal (PeerRecoveryGeneration)
import Eclips.Oracle.Admission (HeraldAdmissionCommand)
import Eclips.Oracle.Canonical (CanonicalOracleEnvelope)
import Eclips.Oracle.Command (ProbeResult)
import Eclips.Oracle.Disappearance
  ( CompleteDisappearanceEvidenceDigest,
    DisappearanceAbortReason,
    DisappearanceInvalidationReason,
  )
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Label
  ( LabelCompletionAttestation,
  )

import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Identity (RaftNodeId)

-- | Opaque outside the Oracle-client implementation.  The constructor is
-- package-internal so the owner can materialize it without widening the public
-- Oracle-client or application/admin protocol surfaces.
newtype OracleRequestRef = OracleRequestRef OracleClientRequestId
  deriving stock (Eq, Ord, Show)

oracleRequestRefRequestId :: OracleRequestRef -> OracleClientRequestId
oracleRequestRefRequestId (OracleRequestRef request) = request

data OracleRequestStatus
  = OracleRequestAwaitingSourceDrain
  | OracleRequestAwaitingProjection
  | OracleRequestDeferred LabelDecisionId ControlIndex
  | OracleRequestProjected ControlIndex
  | OracleRequestEndedBeforeSubmission ControlIndex
  deriving stock (Eq, Ord, Show)

-- | Closed semantic intention retained independently of connection state.
-- Thin command-specific coordinators construct these values; the runtime sees
-- only the canonical envelope owned alongside them.
data OracleVoterOperation
  = RegisterOracleReplica RaftNodeId HeraldEpoch Voter.OracleReplicaContact
  | BeginOracleVoterChange Voter.VoterConfigurationId Voter.ExplicitVoterChangeReason Voter.OracleVoterBindings
  | CancelOracleVoterChange Voter.VoterChangeId
  deriving stock (Eq, Show)

data OracleSemanticIntent
  = HeraldAdmissionIntent HeraldAdmissionCommand
  | VoterAdministrationIntent OracleVoterOperation
  | FailureVoterCancellationIntent Voter.VoterChangeId
  | DynamicStartIntent ProcessStart
  | ProcessEndIntent ProcessEpochId ProcessEndReason
  | DecideLabelIntent
      LabelDecisionId
      ProcessEpochId
      GlobalObjectId
      Label
      (Maybe InitialLabelEvidence)
      (Maybe AuthorityEpoch)
      (Maybe PriorAuthorityJustification)
      LabelTarget
      HomeLabelAcceptanceCut
  | CompleteLabelDecisionIntent LabelCompletionAttestation
  | OpenHeraldFailureProbeIntent
      HeraldEpoch
      HeraldMembershipGenerationId
      PeerRecoveryGeneration
      Voter.VoterConfigurationId
  | ReportHeraldFailureProbeIntent HeraldFailureProbeId Voter.VoterConfigurationId ProbeResult
  | DismissHeraldFailureProbeIntent FailureProbeResolutionId
  | AcceptVoterHostFailureIntent FailureProbeResolutionId
  | RetireHeraldEpochIntent FailureProbeResolutionId HeraldEpoch
  | OpenDisappearanceProbeIntent DisappearanceSubject DisappearanceSubjectMembershipCoordinate
  | ReportPredefinedAbsenceIntent DisappearanceEvidenceClaim
  | InvalidateDisappearanceProbeIntent DisappearanceProbeId HeraldEpoch DisappearanceInvalidationReason DisappearanceEvidenceDigest
  | ResolveDisappearanceProbeIntent DisappearanceProbeId CompleteDisappearanceEvidenceDigest
  | AbortDisappearanceProbeIntent DisappearanceProbeId DisappearanceAbortReason
  deriving stock (Eq, Show)

data OracleRequestWitness = OracleRequestWitness
  { reference :: OracleRequestRef,
    semanticKey :: ByteString,
    intent :: OracleSemanticIntent,
    envelope :: CanonicalOracleEnvelope,
    status :: OracleRequestStatus
  }
  deriving stock (Eq, Show)

oracleRequestWitnessRef ::
  OracleRequestWitness ->
  OracleRequestRef
oracleRequestWitnessRef witness = witness.reference

oracleRequestWitnessSemanticKey ::
  OracleRequestWitness ->
  ByteString
oracleRequestWitnessSemanticKey witness = witness.semanticKey

oracleRequestWitnessBootstrap ::
  OracleRequestWitness ->
  Maybe ProcessStart
oracleRequestWitnessBootstrap witness = case witness.intent of
  HeraldAdmissionIntent {} -> Nothing
  VoterAdministrationIntent {} -> Nothing
  FailureVoterCancellationIntent {} -> Nothing
  DynamicStartIntent bootstrap -> Just bootstrap
  ProcessEndIntent {} -> Nothing
  DecideLabelIntent {} -> Nothing
  CompleteLabelDecisionIntent {} -> Nothing
  OpenHeraldFailureProbeIntent {} -> Nothing
  ReportHeraldFailureProbeIntent {} -> Nothing
  DismissHeraldFailureProbeIntent {} -> Nothing
  AcceptVoterHostFailureIntent {} -> Nothing
  RetireHeraldEpochIntent {} -> Nothing
  OpenDisappearanceProbeIntent {} -> Nothing
  ReportPredefinedAbsenceIntent {} -> Nothing
  InvalidateDisappearanceProbeIntent {} -> Nothing
  ResolveDisappearanceProbeIntent {} -> Nothing
  AbortDisappearanceProbeIntent {} -> Nothing

oracleRequestWitnessIntent ::
  OracleRequestWitness ->
  OracleSemanticIntent
oracleRequestWitnessIntent witness = witness.intent

oracleRequestWitnessEnvelope ::
  OracleRequestWitness ->
  CanonicalOracleEnvelope
oracleRequestWitnessEnvelope witness = witness.envelope

oracleRequestWitnessStatus ::
  OracleRequestWitness ->
  OracleRequestStatus
oracleRequestWitnessStatus witness = witness.status

-- | Level-triggered package-internal dispatch intent.  A pending request is
-- returned with the same canonical envelope on every observation until its
-- exact locally projected result is recorded or the client is retired.
data OracleRequestDispatch = OracleRequestDispatch
  { reference :: OracleRequestRef,
    envelope :: CanonicalOracleEnvelope
  }
  deriving stock (Eq, Show)

oracleRequestDispatchRef :: OracleRequestDispatch -> OracleRequestRef
oracleRequestDispatchRef dispatch = dispatch.reference

oracleRequestDispatchEnvelope ::
  OracleRequestDispatch ->
  CanonicalOracleEnvelope
oracleRequestDispatchEnvelope dispatch = dispatch.envelope
