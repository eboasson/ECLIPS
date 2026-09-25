-- | Pure deterministic Oracle transition.
module Eclips.Oracle.Transition
  ( OracleInvariantFault (..),
    applyCommittedOracleConfiguration,
    initialOracle,
    initialOracleWithStateDigestMode,
    oracleSubmissionPreflightBlocker,
    stepOracle,
    stepOracleInput,
  )
where

import Data.ByteString (ByteString)
import Eclips.Domain.Identity (LabelDecisionId)
import Eclips.Oracle.Command (OracleEnvelope)
import Eclips.Oracle.Effect
  ( OracleEffectBatch,
    OracleStepOutcome (..),
    emitAppliedOracleEntry,
    emptyOracleEffectBatch,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    OracleGenesisFault,
  )
import Eclips.Oracle.Input (OracleInput (..))
import Eclips.Oracle.Internal.Label
  ( OracleState,
    OracleStateDigestMode,
    OracleSubmissionOutcomeView (..),
    initialOracleState,
    initialOracleStateWithDigestMode,
    oracleSubmissionAppliedEntry,
    oracleSubmissionOutcomeView,
    submitOracleState,
  )
import Eclips.Oracle.Internal.Label qualified as Internal
import Eclips.Oracle.Internal.Voter (VoterChangeProblem)
import Eclips.Oracle.Projection (AppliedOracleEntry)
import Eclips.Raft.Effect (RaftCommittedEntry)

-- | A contradiction between a sealed native configuration commit and the
-- retained Oracle intention which authorized it.
data OracleInvariantFault = OracleCommittedConfigurationInvariant VoterChangeProblem
  deriving stock (Eq, Show)

initialOracle :: CheckedOracleGenesis -> Either OracleGenesisFault OracleState
initialOracle = Right . initialOracleState

initialOracleWithStateDigestMode :: OracleStateDigestMode -> CheckedOracleGenesis -> Either OracleGenesisFault OracleState
initialOracleWithStateDigestMode mode = Right . initialOracleStateWithDigestMode mode

oracleSubmissionPreflightBlocker ::
  OracleEnvelope -> OracleState -> Maybe LabelDecisionId
oracleSubmissionPreflightBlocker = Internal.oracleSubmissionPreflightBlocker

stepOracle ::
  OracleEnvelope ->
  OracleState ->
  Either OracleInvariantFault (OracleState, OracleStepOutcome, OracleEffectBatch)
stepOracle envelope state =
  Right $ case oracleSubmissionOutcomeView outcome of
    OracleSubmissionCommittedView receipt entry ->
      (successor, OracleCommitted receipt, emitAppliedOracleEntry entry)
    OracleSubmissionDeferredView request blocker index ->
      (successor, OracleDeferred request blocker index, emptyOracleEffectBatch)
    OracleSubmissionDuplicateView receipt ->
      (successor, OracleDuplicate receipt, maybe emptyOracleEffectBatch emitAppliedOracleEntry (oracleSubmissionAppliedEntry outcome))
    OracleSubmissionProtocolRejectedView problem ->
      (successor, OracleProtocolRejected problem, maybe emptyOracleEffectBatch emitAppliedOracleEntry (oracleSubmissionAppliedEntry outcome))
    OracleSubmissionRetiredView retirement ->
      (successor, OracleRequestRetired retirement, emptyOracleEffectBatch)
    OracleSubmissionProgressRetiredView home through entry ->
      (successor, OracleProgressRetired home through, maybe emptyOracleEffectBatch emitAppliedOracleEntry entry)
  where
    (successor, outcome) = submitOracleState envelope state

stepOracleInput ::
  OracleInput ->
  OracleState ->
  Either OracleInvariantFault (OracleState, OracleStepOutcome, OracleEffectBatch)
stepOracleInput (ApplyOracleEnvelope envelope) = stepOracle envelope

-- Native observations enter only through the sealed committed-entry boundary.
applyCommittedOracleConfiguration :: RaftCommittedEntry ByteString -> OracleState -> Either OracleInvariantFault (OracleState, AppliedOracleEntry)
applyCommittedOracleConfiguration entry state =
  either (Left . OracleCommittedConfigurationInvariant) Right (Internal.applyCommittedVoterState entry state)
