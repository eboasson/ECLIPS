-- | Compare a provisional transport claim with the owner's current immutable
-- registration. Only a contact-less genesis record can still be enriched.
module Eclips.Oracle.Runtime.Internal.ReplicaRegistration
  ( ReplicaRegistrationAdmission (..),
    compareReplicaRegistration,
  ) where

import Eclips.Domain.Identity (controlIndex)
import Eclips.Oracle.Voter

data ReplicaRegistrationAdmission
  = ReplicaRegistrationPending
  | ReplicaRegistrationMatched
  | ReplicaRegistrationContradiction
  deriving stock (Eq, Show)

compareReplicaRegistration :: OracleReplicaRegistration -> Maybe OracleReplicaRegistration -> ReplicaRegistrationAdmission
compareReplicaRegistration _ Nothing = ReplicaRegistrationPending
compareReplicaRegistration expected (Just actual)
  | actual == expected = ReplicaRegistrationMatched
  | replicaRegistrationNode actual == replicaRegistrationNode expected,
    replicaRegistrationHost actual == replicaRegistrationHost expected,
    replicaRegistrationControlIndex actual == controlIndex 0,
    replicaRegistrationContact actual == Nothing,
    replicaRegistrationContact expected /= Nothing =
      ReplicaRegistrationPending
  | otherwise = ReplicaRegistrationContradiction
