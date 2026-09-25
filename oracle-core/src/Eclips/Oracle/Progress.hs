-- | Independent, monotone Oracle receipt and label-completion promises.
module Eclips.Oracle.Progress
  ( OracleProgress,
    oracleProgress,
    oracleProgressReceipts,
    oracleProgressLabelsThrough,
    oracleProgressCovers,
  )
where

import Eclips.Domain.Identity (ControlIndex, controlIndex)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)

-- The constructor is private: callers compose checked receipt progress and a
-- typed canonical index; the Oracle admits the promised index against its high water.
data OracleProgress = OracleProgress !ReceiptRetirement !ControlIndex
  deriving stock (Eq, Show)

oracleProgress :: ReceiptRetirement -> ControlIndex -> OracleProgress
oracleProgress = OracleProgress

oracleProgressReceipts :: OracleProgress -> ReceiptRetirement
oracleProgressReceipts (OracleProgress receipts _) = receipts

oracleProgressLabelsThrough :: OracleProgress -> ControlIndex
oracleProgressLabelsThrough (OracleProgress _ through) = through

instance Semigroup OracleProgress where
  OracleProgress receipts left <> OracleProgress other right = OracleProgress (receipts <> other) (max left right)

instance Monoid OracleProgress where
  mempty = OracleProgress mempty (controlIndex 0)

oracleProgressCovers :: OracleProgress -> OracleProgress -> Bool
oracleProgressCovers promised offered = promised <> offered == promised
