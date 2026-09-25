-- | Configuration-qualified failure evidence and the immutable certificate that
-- authorizes normal native exclusion followed by semantic Herald retirement.
module Eclips.Oracle.Failure
  ( AcceptedVoterHostFailure,
    acceptedVoterHostFailureResolution,
    acceptedVoterHostFailureTarget,
    acceptedVoterHostFailureMembership,
    acceptedVoterHostFailureConfiguration,
    acceptedVoterHostFailureReports,
    acceptedVoterHostFailureControlIndex,
    acceptedVoterHostFailureChangeId,
    encodeAcceptedVoterHostFailure,
    decodeAcceptedVoterHostFailure,
    acceptVoterHostFailureCommand,
  ) where

import Eclips.Oracle.Internal.Failure
import Eclips.Oracle.Internal.Label (acceptVoterHostFailureCommand)
