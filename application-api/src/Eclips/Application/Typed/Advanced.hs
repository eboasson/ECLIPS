-- | Exact invocation ownership for typed application payloads. Status, await
-- and cancellation all refer to the same underlying retained call.
module Eclips.Application.Typed.Advanced
  ( Operation (..),
    Call,
    CallStatus (..),
    submit,
    await,
    cancel,
    status,
  ) where

import Eclips.Application.Typed.Internal
