{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Owner-private representation of the Oracle-client vocabulary.
module Eclips.Herald.OracleClient.Internal
  ( OracleNodeClaim (..),
    OracleContact (..),
    OracleContactSet (..),
    OracleContactProblem (..),
    OracleHelloClaims (..),
    OracleObservedTerm (..),
    OracleConnectAttempt (..),
    OracleRetry (..),
    OracleRetryPurpose (..),
    OracleBindingGeneration (..),
    OracleBinding (..),
    OracleLane (..),
    OracleHelloAcceptance (..),
    OracleRedirect (..),
    OracleClientIngress (..),
    OracleClientAction (..),
    OracleClientDiagnostic (..),
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    LabelDecisionId,
    SystemId,
  )
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Startup
  ( CatalogueDigest,
    ConfigurationDigest,
    InitialProjectionDigest,
  )
import Eclips.Herald.OracleClient.Request (OracleRequestDispatch)
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry, CanonicalOracleEnvelope)
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Progress (OracleProgress)
import Eclips.Oracle.Receipt (OracleRequestRetirement)
import Eclips.Oracle.Voter (OracleReplicaContact)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Raft.Identity (RaftNodeId)

newtype OracleNodeClaim = OracleNodeClaim ByteString
  deriving stock (Eq, Ord)

instance Show OracleNodeClaim where
  show (OracleNodeClaim bytes) = renderGroupedHex bytes

data OracleContact = OracleContact
  { node :: OracleNodeClaim,
    host :: Text,
    port :: Word16
  }
  deriving stock (Eq, Ord, Show)

newtype OracleContactSet = OracleContactSet (NonEmpty OracleContact)
  deriving stock (Eq, Show)

data OracleContactProblem
  = OracleNodeClaimWrongByteCount Int
  | OracleContactHostEmpty
  | OracleContactPortZero
  | OracleContactSetEmpty
  | OracleContactNodeDuplicate OracleNodeClaim
  | OracleContactEndpointDuplicate Text Word16
  deriving stock (Eq, Show)

data OracleHelloClaims = OracleHelloClaims
  { systemId :: SystemId,
    catalogueDigest :: CatalogueDigest,
    configurationDigest :: ConfigurationDigest,
    initialProjectionDigest :: InitialProjectionDigest,
    heraldId :: HeraldId,
    heraldEpoch :: HeraldEpoch,
    membershipGeneration :: HeraldMembershipGenerationId
  }
  deriving stock (Eq, Show)

newtype OracleObservedTerm = OracleObservedTerm Word64
  deriving stock (Eq, Ord, Show)

data OracleConnectAttempt = OracleConnectAttempt
  { ordinal :: Word64,
    contact :: OracleContact,
    fromExclusive :: ControlIndex
  }
  deriving stock (Eq, Ord, Show)

newtype OracleRetry = OracleRetry Word64
  deriving stock (Eq, Ord, Show)

-- | Logical reason for one owner-minted retry. The pure owner retains this
-- classification; a physical runtime interprets it but never infers it from
-- socket state.
data OracleRetryPurpose
  = OracleConnectionRetry
  | OracleSubmissionRetry
  deriving stock (Eq, Ord, Show)

newtype OracleBindingGeneration = OracleBindingGeneration Word64
  deriving stock (Eq, Ord, Show)

data OracleBinding = OracleBinding
  { generation :: OracleBindingGeneration,
    contact :: OracleContact
  }
  deriving stock (Eq, Ord, Show)

data OracleLane
  = OracleCandidateLane OracleConnectAttempt
  | OracleEstablishedLane OracleBinding
  deriving stock (Eq, Ord, Show)

data OracleHelloAcceptance = OracleHelloAcceptance
  { node :: OracleNodeClaim,
    term :: OracleObservedTerm,
    localApplied :: ControlIndex,
    leaderHint :: Maybe OracleNodeClaim,
    serviceReady :: Bool
  }
  deriving stock (Eq, Show)

data OracleRedirect = OracleRedirect
  { leaderHint :: Maybe OracleNodeClaim,
    term :: OracleObservedTerm
  }
  deriving stock (Eq, Show)

-- | Runtime observations accepted by the watch/configured-request owner. The canonical
-- entries remain opaque validated Oracle-core values until the projection
-- coordinator processes them.
data OracleClientIngress
  = OracleContactsDiscovered OracleContactSet
  | OracleLocalReplicaConfigured RaftNodeId OracleReplicaContact
  | OracleConnectFailed OracleConnectAttempt
  | OracleHelloReceived OracleConnectAttempt OracleHelloAcceptance
  | OracleRedirectReceived OracleLane OracleRedirect
  | OracleBindingLost OracleBinding
  | OracleRetryElapsed OracleRetry
  | OracleSubmissionNotReadyReceived
      OracleBinding
      OracleClientRequestId
      OracleObservedTerm
  | OracleSubmissionDeferredReceived
      OracleBinding
      OracleClientRequestId
      LabelDecisionId
      ControlIndex
  | OracleProgressConfirmed OracleBinding OracleClientRequestId OracleProgress
  | OracleProgressFlush OracleBinding
  | OracleProgressNotReadyReceived OracleBinding OracleClientRequestId OracleProgress OracleObservedTerm
  | OracleRequestRetiredReceived OracleBinding OracleClientRequestId OracleRequestRetirement
  | OracleEntriesReceived OracleBinding (NonEmpty CanonicalAppliedOracleEntry)
  deriving stock (Eq, Show)

-- | Declarative actions at the runtime boundary.  Configured Start submission
-- carries the exact owner-retained canonical dispatch; the runtime cannot mint
-- or alter a request sequence or command.
data OracleClientAction
  = ConnectAndHelloOracle OracleConnectAttempt OracleHelloClaims
  | BindOracleConnection OracleConnectAttempt OracleBinding
  | RejectOracleConnection OracleLane
  | WatchOracle OracleBinding ControlIndex
  | SubmitOracleRequest OracleBinding OracleRequestDispatch
  | SubmitOracleProgress OracleBinding CanonicalOracleEnvelope
  | ScheduleOracleProgress OracleBinding
  | ReleaseOracleProgress OracleBinding OracleProgress
  | ReleaseDeferredOracleSubmission OracleBinding OracleClientRequestId
  | AssociateOracleSubmissionRetry
      OracleBinding
      OracleClientRequestId
      OracleRetry
  | ScheduleOracleRetry OracleRetryPurpose OracleRetry
  | CancelOracleRetry OracleRetry
  | ReportOracleClientDiagnostic OracleClientDiagnostic
  deriving stock (Eq, Show)

data OracleClientDiagnostic
  = StaleOracleClientIngress
  | UnknownOracleLeaderHint OracleNodeClaim
  | OracleReplicaBehind ControlIndex ControlIndex
  deriving stock (Eq, Show)
