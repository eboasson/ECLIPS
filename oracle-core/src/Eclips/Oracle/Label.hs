-- | Live Slice-4 Oracle process/label state-machine vocabulary.
--
-- The implementation remains package-private so canonical construction and
-- state ownership stay centralized. This module is the supported surface used
-- by the Oracle runtime and Herald projection during the atomic cutover.
module Eclips.Oracle.Label
  ( module Eclips.Oracle.Internal.Label,
  )
where

import Eclips.Oracle.Internal.Label hiding (applyCommittedVoterState)
