{-# LANGUAGE DeriveAnyClass #-}

-- | The closed current application write boundary.
module Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
where

import Data.Binary (Binary)
import Eclips.Application.Types.Identity
  ( PrivateObjectId,
    SortId,
  )
import Eclips.Application.Types.Value (ApplicationValue)
import GHC.Generics (Generic)

-- | Input accepted by the one application @write@ operation.
--
-- Every semantic publication uses the same recursively typed value arm.
-- Reservation cancellation remains a separate boundary action: it cannot be
-- nested in an application value and has no semantic value/label conversion.
data ApplicationWriteValue
  = PublishValue ApplicationValue
  | DeleteReserved PrivateObjectId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Currently executable successful write results.
--
-- 'WriteAccepted' deliberately carries no identity or stored value.  The
-- retained typed request identifies the successful branch and its operands.
data WriteResult
  = SortDefinitionWritten SortId
  | WriteAccepted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
