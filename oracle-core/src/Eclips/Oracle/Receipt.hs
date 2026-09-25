-- | Immutable Oracle results retained for exact retry.
module Eclips.Oracle.Receipt
  ( oracleReceiptDisappearanceResult,
    oracleReceiptVoterResult,
    FailureProbeTerminalView (..),
    FailureResolutionReady (..),
    FailureCommandResult (..),
    FailureRejection (..),
    OracleRejection (..),
    OracleReceiptResult (..),
    OracleRequestRetirement (..),
    OracleReceipt,
    oracleReceiptRequestId,
    oracleReceiptCommandDigest,
    oracleReceiptControlIndex,
    oracleReceiptResult,
    oracleReceiptFailureResult,
  )
where

import Eclips.Oracle.Internal.Label
  ( FailureCommandResult (..),
    FailureProbeTerminalView (..),
    FailureRejection (..),
    FailureResolutionReady (..),
    OracleReceipt,
    OracleReceiptResult (..),
    OracleRejection (..),
    OracleRequestRetirement (..),
    oracleReceiptCommandDigest,
    oracleReceiptControlIndex,
    oracleReceiptDisappearanceResult,
    oracleReceiptFailureResult,
    oracleReceiptRequestId,
    oracleReceiptResult,
    oracleReceiptVoterResult,
  )
