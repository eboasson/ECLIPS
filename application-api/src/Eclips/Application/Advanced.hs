-- | Typed concurrent calls over the same supervised owner as the direct facade.
-- Cancelling a mutation waiter does not revoke accepted semantic work. Keep its
-- Call to inspect or await the exact result again; no new request is submitted.
module Eclips.Application.Advanced
  ( Operation (..),
    Call,
    CallStatus (..),
    submit,
    await,
    cancel,
    status,
    LifecycleOperation (..),
    LifecycleCall,
    submitLifecycle,
    awaitLifecycle,
    lifecycleStatus,
    lifecycleRequestId,
    recoverEndProcess,
    connectWith,
  )
where

import Eclips.Application.Internal
