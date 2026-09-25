-- | Pure planning at the semantic-preflight/proposal-registration cut.
module Eclips.Oracle.Runtime.Internal.Submission
  ( SubmissionRegistrationDecision (..),
    decideSubmissionRegistration,
    submissionRegistrationProposalCount,
    submissionRegistrationRecursivePreflightCount,
    mayCancelVoterPreparation,
  )
where

import Eclips.Domain.Identity (ControlIndex)
import Eclips.Oracle.Voter
import Eclips.Raft.Identity (RaftTerm)

-- | The complete next action after one successful owner preflight. A changed
-- token in the same leadership term is an invariant fault. A reservation from
-- an earlier term is a normal retry, even when another leader advanced control.
data SubmissionRegistrationDecision failure
  = RegisterSubmissionProposal
  | RetryStaleSubmissionReservation
  | FailSubmissionRegistration failure
  deriving stock (Eq, Show)

decideSubmissionRegistration ::
  (ControlIndex -> ControlIndex -> failure) ->
  RaftTerm ->
  RaftTerm ->
  ControlIndex ->
  ControlIndex ->
  SubmissionRegistrationDecision failure
decideSubmissionRegistration invariantFault observedTerm currentTerm observedIndex currentIndex
  | currentTerm /= observedTerm = RetryStaleSubmissionReservation
  | currentIndex == observedIndex = RegisterSubmissionProposal
  | otherwise =
      FailSubmissionRegistration
        (invariantFault observedIndex currentIndex)

-- | Executable witnesses used by the internal property. They make the closed
-- plan's absence of proposal and recursive-preflight work explicit.
submissionRegistrationProposalCount :: SubmissionRegistrationDecision failure -> Int
submissionRegistrationProposalCount = \case
  RegisterSubmissionProposal -> 1
  RetryStaleSubmissionReservation -> 0
  FailSubmissionRegistration _ -> 0

submissionRegistrationRecursivePreflightCount :: SubmissionRegistrationDecision failure -> Int
submissionRegistrationRecursivePreflightCount _ = 0

-- | Only the exact still-preparing explicit intention may open the native
-- cancellation path. Other requests cannot disturb a different frontier or
-- roll back preparation of an already accepted failure certificate.
mayCancelVoterPreparation :: VoterChangeId -> Maybe VoterChange -> Bool
mayCancelVoterPreparation requested (Just change) =
  requested == voterChangeId change
    && voterChangePhase change == VoterChangePreparing
    && case voterChangeReason change of
      ExplicitVoterReason _ -> True
      AcceptedHostFailureReason _ -> False
mayCancelVoterPreparation _ Nothing = False
