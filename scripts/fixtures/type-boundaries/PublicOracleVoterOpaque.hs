{-# LANGUAGE CPP #-}

module PublicOracleVoterOpaque where

#if defined(ORACLE_ACCEPTED_FAILURE_CONSTRUCTOR)
import Eclips.Oracle.Failure (AcceptedVoterHostFailure (..))
forged :: AcceptedVoterHostFailure
forged = AcceptedVoterHostFailure undefined undefined undefined undefined undefined undefined
#elif defined(ORACLE_ACCEPTED_FAILURE_UPDATE)
import Eclips.Oracle.Failure (AcceptedVoterHostFailure, acceptedVoterHostFailureConfiguration)
forged :: AcceptedVoterHostFailure -> AcceptedVoterHostFailure
forged certificate = certificate {acceptedVoterHostFailureConfiguration = undefined}
#elif defined(ORACLE_ACCEPTED_FAILURE_INTERNAL_IMPORT)
import Eclips.Oracle.Internal.Failure (AcceptedVoterHostFailure)
forged :: Maybe AcceptedVoterHostFailure
forged = Nothing
#elif defined(ORACLE_RUNTIME_HEALTH_CONSTRUCTOR)
import Eclips.Oracle.Runtime (OracleRuntimeHealth (..))
forged :: OracleRuntimeHealth
forged = OracleRuntimeHealth undefined undefined undefined undefined undefined
#elif defined(ORACLE_HEALTH_DTO_CONSTRUCTOR)
import Eclips.Protocol.Oracle.Types (OracleHealthReplyDto (..))
forged :: OracleHealthReplyDto
forged = OracleHealthReplyDto undefined undefined undefined undefined undefined undefined
#elif defined(ORACLE_REPLICA_ENDPOINT_CONSTRUCTOR)
import Eclips.Oracle.Voter (OracleReplicaEndpoint (..))
forged :: OracleReplicaEndpoint
forged = OracleReplicaEndpoint undefined undefined
#elif defined(ORACLE_REPLICA_REGISTRATION_CONSTRUCTOR)
import Eclips.Oracle.Voter (OracleReplicaRegistration (..))
forged :: OracleReplicaRegistration
forged = OracleReplicaRegistration undefined undefined undefined undefined
#elif defined(ORACLE_REPLICA_REGISTRATION_UPDATE)
import Eclips.Oracle.Voter (OracleReplicaRegistration, replicaRegistrationHost)
forged :: OracleReplicaRegistration -> OracleReplicaRegistration
forged registration = registration {replicaRegistrationHost = undefined}
#elif defined(ORACLE_VOTER_BINDINGS_CONSTRUCTOR)
import Eclips.Oracle.Voter (OracleVoterBindings (..))
forged :: OracleVoterBindings
forged = OracleVoterBindings []
#elif defined(ORACLE_VOTER_BINDINGS_COERCION)
import Data.Coerce (coerce)
import Eclips.Oracle.Genesis (RaftVoterBinding)
import Eclips.Oracle.Voter (OracleVoterBindings)
forged :: [RaftVoterBinding] -> OracleVoterBindings
forged = coerce
#elif defined(ORACLE_VOTER_CONFIGURATION_CONSTRUCTOR)
import Eclips.Oracle.Voter (VoterConfiguration (..))
forged :: VoterConfiguration
forged = VoterConfiguration undefined undefined undefined undefined undefined
#elif defined(ORACLE_VOTER_CONFIGURATION_UPDATE)
import Eclips.Oracle.Voter (VoterConfiguration, voterConfigurationNativeRef)
forged :: VoterConfiguration -> VoterConfiguration
forged configuration = configuration {voterConfigurationNativeRef = undefined}
#elif defined(ORACLE_VOTER_CHANGE_CONSTRUCTOR)
import Eclips.Oracle.Voter (VoterChange (..))
forged :: VoterChange
forged = VoterChange undefined undefined undefined undefined undefined undefined undefined
#elif defined(ORACLE_VOTER_CHANGE_UPDATE)
import Eclips.Oracle.Voter (VoterChange, voterChangePhase)
forged :: VoterChange -> VoterChange
forged change = change {voterChangePhase = undefined}
#elif defined(ORACLE_VOTER_INTERNAL_IMPORT)
import Eclips.Oracle.Internal.Voter (VoterChange)
forged :: Maybe VoterChange
forged = Nothing
#elif defined(ORACLE_VOTER_PRIVATE_APPLY)
import Eclips.Oracle.Label (applyCommittedVoterState)
#elif defined(ORACLE_CONFIGURATION_IS_CLIENT_COMMAND)
import Eclips.Oracle.Projection (AppliedOracleEntry, appliedEntryRequestId)
import Eclips.Oracle.Identity (OracleClientRequestId)
forged :: AppliedOracleEntry -> OracleClientRequestId
forged = appliedEntryRequestId
#else
#error "select one Oracle voter opacity fixture"
#endif
