{-# LANGUAGE DeriveAnyClass #-}

-- | Results of the currently executable regular application calls.
module Eclips.Application.Types.Result
  ( OperationPendingReason (..),
    WaitResult (..),
    RegularCallResult (..),
  )
where

import Data.Binary (Binary)
import Eclips.Application.Types.Access (EnvironmentAccess)
import Eclips.Application.Types.Forward (ForwardResult)
import Eclips.Application.Types.Identity (PrivateUniqueId)
import Eclips.Application.Types.Label (LabelResult)
import Eclips.Application.Types.Value (ApplicationValue)
import Eclips.Application.Types.Write (WriteResult)
import GHC.Generics (Generic)

-- | A nonterminal semantic-operation status.
--
-- It is retained under the existing request identity and is never exposed as a
-- separate application operation or cancellation token.
data OperationPendingReason
  = StructuralStabilizationPending
  | LabelSettlementPending
  | EnvironmentStabilizationPending
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | A level-triggered wait observed readiness.
--
-- Cancellation is RPC bookkeeping and is deliberately not a successful wait
-- result.
data WaitResult
  = WaitReady
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Semantic results of the eight regular calls executable in the current build.
data RegularCallResult
  = NewIdCompleted PrivateUniqueId
  | WriteCompleted WriteResult
  | ForwardCompleted ForwardResult
  | ReadCompleted [ApplicationValue]
  | LocalTakeCompleted [ApplicationValue]
  | WaitCompleted WaitResult
  | LabelCompleted LabelResult
  | NewEnvironmentCompleted EnvironmentAccess
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
