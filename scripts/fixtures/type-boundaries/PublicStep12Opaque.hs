{-# LANGUAGE CPP #-}

module PublicStep12Opaque where

#if defined(ORACLE_CLIENT_INTERNAL)
import Eclips.Herald.OracleClient.Internal ()
#elif defined(ORACLE_CLIENT_STATE)
import Eclips.Herald.OracleClient.State ()
#elif defined(ORACLE_PROJECTION_STATE)
import Eclips.Herald.OracleProjection.State ()
#elif defined(ORACLE_CONNECT_ATTEMPT_CONSTRUCTOR)
import Eclips.Herald.OracleClient (OracleConnectAttempt (..))

forgedOracleConnectAttempt :: OracleConnectAttempt
forgedOracleConnectAttempt = OracleConnectAttempt undefined undefined undefined
#elif defined(ORACLE_RETRY_CONSTRUCTOR)
import Eclips.Herald.OracleClient (OracleRetry (..))

forgedOracleRetry :: OracleRetry
forgedOracleRetry = OracleRetry 1
#elif defined(ORACLE_BINDING_GENERATION_CONSTRUCTOR)
import Eclips.Herald.OracleClient (OracleBindingGeneration (..))

forgedOracleBindingGeneration :: OracleBindingGeneration
forgedOracleBindingGeneration = OracleBindingGeneration 1
#elif defined(ORACLE_BINDING_CONSTRUCTOR)
import Eclips.Herald.OracleClient (OracleBinding (..))

forgedOracleBinding :: OracleBinding
forgedOracleBinding = OracleBinding undefined undefined
#elif defined(ORACLE_CONTACT_SET_CONSTRUCTOR)
import Eclips.Herald.OracleClient (OracleContactSet (..))

forgedOracleContactSet :: OracleContactSet
forgedOracleContactSet = OracleContactSet undefined
#elif defined(DTO_NODE_CLAIM_IS_CORE_CLAIM)
import Eclips.Herald.OracleClient (OracleNodeClaim)
import Eclips.Protocol.Oracle.Types (RaftNodeIdClaim)

miswiredNodeClaim :: RaftNodeIdClaim -> OracleNodeClaim
miswiredNodeClaim = id
#elif defined(DTO_ENTRY_IS_CANONICAL_ENTRY)
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry)
import Eclips.Protocol.Oracle.Types (CanonicalAppliedOracleEntryDto)

miswiredCanonicalEntry :: CanonicalAppliedOracleEntryDto -> CanonicalAppliedOracleEntry
miswiredCanonicalEntry = id
#else
#error "select one Step-12 opacity fixture"
#endif
