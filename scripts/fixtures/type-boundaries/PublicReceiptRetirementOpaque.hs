{-# LANGUAGE CPP #-}

module PublicReceiptRetirementOpaque where

#if defined(RECEIPT_RETIREMENT_CONSTRUCTOR)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement (..))
forged :: ReceiptRetirement
forged = ReceiptRetirement Nothing undefined
#elif defined(RECEIPT_RETIREMENT_HIGH_WATER_UPDATE)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement, receiptRetirementHighWater)
forged :: ReceiptRetirement -> ReceiptRetirement
forged progress = progress {receiptRetirementHighWater = Nothing}
#elif defined(RECEIPT_RETIREMENT_EXCEPTIONS_UPDATE)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement, receiptRetirementExceptions)
forged :: ReceiptRetirement -> ReceiptRetirement
forged progress = progress {receiptRetirementExceptions = undefined}
#elif defined(RECEIPT_RETIREMENT_COERCION)
import Data.Coerce (coerce)
import Data.Set (Set)
import Data.Word (Word64)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
forged :: (Maybe Word64, Set Word64) -> ReceiptRetirement
forged = coerce
#else
#error "select one receipt retirement opacity fixture"
#endif
