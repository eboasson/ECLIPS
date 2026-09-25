-- | Explicit input admitted by the pure Oracle owner.
module Eclips.Oracle.Input
  ( OracleInput (..),
  )
where

import Eclips.Oracle.Command (OracleEnvelope)

newtype OracleInput = ApplyOracleEnvelope OracleEnvelope
  deriving stock (Eq, Show)
