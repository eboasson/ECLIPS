-- | Pure runtime-safe admission and projection for Start/End administration.
--
-- Protocol decoding establishes claim shape.  Candidate 'AdminHello' binds
-- the sole current role to a checked deployment claim; all subsequent Start/End,
-- query, retry, and reconnect semantics remain in the Herald transition.
module Eclips.Herald.Administration.RPC
  ( AdministrationIngressContext (..),
    AdministrationRpcAdmissionError (..),
    admitAdministrationClientDto,
    AdministrationOutbound (..),
    AdministrationRpcProjectionFault (..),
    projectAdministrationEffect,
    projectFinalAdministrationReply,
  )
where

import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.ProcessLifecycle qualified as Domain
import Eclips.Herald.Administration qualified as Administration
import Eclips.Herald.Administration.RPC.Internal qualified as Internal
import Eclips.Herald.EffectBatch
  ( AdministrationDispositionTarget (..),
    HeraldEffect (..),
  )
import Eclips.Herald.Input
  ( AdministrationIngress (..),
    CandidateAdministrationLane,
    HeraldInputBody (AdministrationInput),
  )
import Eclips.Oracle.Genesis (raftVoterBinding)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Admin.Types qualified as Protocol
import Eclips.Raft.Identity (mkRaftNodeId)

-- | Runtime-owned logical phase in which a decoded administration DTO arrived.
data AdministrationIngressContext
  = CandidateAdministrationIngress CandidateAdministrationLane
  | EstablishedAdministrationIngress Administration.AdministrationBinding
  deriving stock (Eq, Show)

data AdministrationRpcAdmissionError
  = AdministrationRpcWrongIngressPhase
  | AdministrationRpcClaimContradiction
  deriving stock (Eq, Show)

-- | Translate the executable operator vocabulary. An established operator may
-- request the shared orderly drain without acquiring the private runtime lease.
admitAdministrationClientDto ::
  AdministrationIngressContext ->
  Protocol.AdminClientDto ->
  Either AdministrationRpcAdmissionError HeraldInputBody
admitAdministrationClientDto context dto = case (context, dto) of
  (EstablishedAdministrationIngress binding, Protocol.RetireAdminReceipts progress) ->
    pure (AdministrationInput (RetireAdministrationReceipts binding progress Nothing))
  (EstablishedAdministrationIngress binding, Protocol.AdminWithRetirement progress work)
    | Protocol.adminReceiptRetirementWork work -> do
        admitted <- admitAdministrationClientDto context work
        case admitted of
          AdministrationInput input -> pure (AdministrationInput (RetireAdministrationReceipts binding progress (Just input)))
          _ -> Left AdministrationRpcWrongIngressPhase
  (EstablishedAdministrationIngress binding, Protocol.PrepareOracleReplica correlation) ->
    pure (AdministrationInput (PrepareOracleReplica binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation))))
  (EstablishedAdministrationIngress binding, Protocol.BeginVoterChange correlation expected reason proposed) -> do
    configuration <- checked (Voter.mkVoterConfigurationId (Protocol.adminVoterConfigurationClaimBytes expected))
    bindings <-
      traverse
        ( \(Protocol.AdminVoterBindingDto node host) -> do
            checkedNode <- checked (mkRaftNodeId (Protocol.adminOracleNodeClaimBytes node))
            checkedHost <- checked (Identity.mkHeraldEpoch (Protocol.adminHeraldEpochClaimBytes host))
            pure (raftVoterBinding checkedNode checkedHost)
        )
        proposed
        >>= checked . Voter.oracleVoterBindings
    pure (AdministrationInput (BeginVoterChange binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation)) configuration (case reason of Protocol.AdminCommissionVotersDto -> Voter.ExplicitCommission; Protocol.AdminDecommissionVotersDto -> Voter.ExplicitDemotion) bindings))
  (EstablishedAdministrationIngress binding, Protocol.CancelVoterChange correlation change) -> do
    identifier <- checked (Voter.mkVoterChangeId (Identity.controlIndex (Protocol.adminVoterChangeIdClaimWord64 change)))
    pure (AdministrationInput (CancelVoterChange binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation)) identifier))
  (EstablishedAdministrationIngress binding, Protocol.GetOracleConfiguration correlation) ->
    pure (AdministrationInput (GetOracleConfiguration binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation))))
  (EstablishedAdministrationIngress binding, Protocol.GetVoterChangeStatus correlation change) -> do
    identifier <- checked (Voter.mkVoterChangeId (Identity.controlIndex (Protocol.adminVoterChangeIdClaimWord64 change)))
    pure (AdministrationInput (GetVoterChangeStatus binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation)) identifier))
  (EstablishedAdministrationIngress binding, Protocol.GetHeraldStatus correlation) ->
    pure (AdministrationInput (GetHeraldStatus binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation))))
  (EstablishedAdministrationIngress binding, Protocol.ListChildPreparations correlation) ->
    pure (AdministrationInput (ListChildPreparations binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation))))
  (EstablishedAdministrationIngress binding, Protocol.DrainHerald correlation) ->
    pure (AdministrationInput (DrainConfiguredHerald binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation))))
  (EstablishedAdministrationIngress binding, Protocol.CancelChildPreparation correlation preparation) ->
    pure (AdministrationInput (CancelChildPreparation binding (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation)) preparation))
  ( CandidateAdministrationIngress lane,
    Protocol.AdminHello deployment Protocol.ProcessAdministrator
    ) -> do
      system <- convertClaim (Internal.deploymentFromClaim deployment)
      pure (AdministrationInput (OpenAdministrationConnection lane system))
  ( EstablishedAdministrationIngress binding,
    Protocol.StartProcessEpoch correlation
    ) -> do
      pure
        ( AdministrationInput
            ( StartProcessEpoch
                binding
                ( Administration.startProcessRequest
                    (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation))
                )
            )
        )
  ( EstablishedAdministrationIngress binding,
    Protocol.EndProcessEpoch
      correlation
      process
      Protocol.AdminExplicitAdministrativeEnd
    ) -> do
      checkedProcess <- convertClaim (Internal.processEpochFromClaim process)
      pure
        ( AdministrationInput
            ( EndProcessEpoch
                binding
                ( Administration.endProcessEpochRequest
                    (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation))
                    checkedProcess
                    Domain.ExplicitAdministrativeEnd
                )
            )
        )
  ( EstablishedAdministrationIngress binding,
    Protocol.GetAdminResult correlation
    ) ->
      pure
        ( AdministrationInput
            ( GetAdministrationResult
                binding
                (Administration.adminCorrelationIdForBinding binding (Protocol.adminCorrelationIdClaimWord64 correlation))
            )
        )
  _ -> Left AdministrationRpcWrongIngressPhase

-- | Complete administration output plus runtime-only candidate/binding
-- metadata.  Connection acceptance is transport metadata, not a server DTO.
data AdministrationOutbound
  = AcceptAdministrationCandidate
      CandidateAdministrationLane
      Administration.AdministrationBinding
  | RejectAdministrationCandidate CandidateAdministrationLane
  | RejectEstablishedAdministration Administration.AdministrationBinding
  | SendEstablishedAdministration
      Administration.AdministrationBinding
      Protocol.AdminServerDto
  deriving stock (Eq, Show)

data AdministrationRpcProjectionFault
  = AdministrationRpcOwnerClaimContradiction
  deriving stock (Eq, Show)

-- | Project one current administration effect.  Non-administration effects
-- are outside this adapter and return 'Right Nothing'.
projectAdministrationEffect ::
  HeraldEffect ->
  Either AdministrationRpcProjectionFault (Maybe AdministrationOutbound)
projectAdministrationEffect = \case
  SetAdministrationConnectionDisposition lane binding ->
    pure (Just (AcceptAdministrationCandidate lane binding))
  RejectAdministrationConnection target ->
    pure
      ( Just $ case target of
          CandidateAdministrationDisposition lane ->
            RejectAdministrationCandidate lane
          EstablishedAdministrationDisposition binding ->
            RejectEstablishedAdministration binding
      )
  SendAdministrationReply binding reply -> do
    dto <- projectClaim (Internal.adminReplyToDto reply)
    pure (Just (SendEstablishedAdministration binding dto))
  _ -> Right Nothing

-- | The final public drain receipt uses the same established writer route.
-- The private runtime administration lease has no EADM representation.
projectFinalAdministrationReply ::
  Administration.FinalAdminReply -> Maybe AdministrationOutbound
projectFinalAdministrationReply = \case
  Administration.OrderlyHeraldShutdownCompleted {} -> Nothing
  Administration.ConfiguredHeraldShutdownCompleted binding correlation ->
    Just (SendEstablishedAdministration binding (Protocol.HeraldDrained (Internal.correlationToClaim correlation)))

convertClaim ::
  Either Internal.AdministrationRpcClaimError value ->
  Either AdministrationRpcAdmissionError value
convertClaim =
  either
    (const (Left AdministrationRpcClaimContradiction))
    Right

projectClaim ::
  Either Internal.AdministrationRpcClaimError value ->
  Either AdministrationRpcProjectionFault value
projectClaim =
  either
    (const (Left AdministrationRpcOwnerClaimContradiction))
    Right

checked :: Either problem value -> Either AdministrationRpcAdmissionError value
checked = either (const (Left AdministrationRpcClaimContradiction)) Right
