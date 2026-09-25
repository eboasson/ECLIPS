-- | Safe facade for the deterministic application-side session owner.
module Eclips.Application.Client
  ( ApplicationClientState,
    initialApplicationClient,
    initialPreparedApplicationClient,
    initialLifecycleRecoveryClient,
    ApplicationClientPhase (..),
    applicationClientPhase,
    applicationClientCurrentTransportAttempt,
    applicationClientRetentionCounts,
    applicationClientReceiptRetirement,
    ApplicationClientCommandError (..),
    ApplicationClientInvariantFault (..),
    ApplicationClientTransition (..),
    stepApplicationClient,
    validateApplicationClientState,
    submitNewId,
    submitWrite,
    submitForward,
    submitRead,
    submitLocalTake,
    submitWait,
    submitLabel,
    submitNewEnvironment,
    module Eclips.Application.Client.Effect,
    module Eclips.Application.Client.Input,
    module Eclips.Application.Client.Recovery,
  )
where

import Eclips.Application.Client.Effect
import Eclips.Application.Client.Input
import Eclips.Application.Client.Internal
  ( ApplicationClientCommandError (..),
    ApplicationClientInvariantFault (..),
    ApplicationClientPhase (..),
    ApplicationClientState,
    ApplicationClientTransition (..),
    applicationClientCurrentTransportAttempt,
    applicationClientPhase,
    applicationClientReceiptRetirement,
    applicationClientRetentionCounts,
    initialApplicationClient,
    initialLifecycleRecoveryClient,
    initialPreparedApplicationClient,
    stepApplicationClient,
    validateApplicationClientState,
  )
import Eclips.Application.Client.Recovery
import Eclips.Application.Types.Identity (PrivateNablaId, PrivateObjectId)
import Eclips.Application.Types.Label (ApplicationLabelTarget)
import Eclips.Application.Types.NewId (NewIdTarget)
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Query (ApplicationQuery)
import Eclips.Application.Types.Value (ApplicationLabel)
import Eclips.Application.Types.Write (ApplicationWriteValue)

-- | Submit one bare or controlled generated-identity operation.
submitNewId ::
  ApplicationInvocationId ->
  NewIdTarget ->
  ApplicationClientInput
submitNewId invocation target =
  Submit invocation (NewIdApplication target)

-- | Submit one of the current executable write payloads.
submitWrite ::
  ApplicationInvocationId ->
  PrivateNablaId ->
  ApplicationWriteValue ->
  ApplicationClientInput
submitWrite invocation writer value =
  Submit invocation (WriteApplication writer value)

-- | Submit one ordinary or structural forward through the shared publication
-- path.
submitForward ::
  ApplicationInvocationId ->
  PrivateNablaId ->
  PrivateObjectId ->
  ApplicationClientInput
submitForward invocation writer object =
  Submit invocation (ForwardApplication writer object)

-- | Submit the current local read operation.
submitRead :: ApplicationInvocationId -> ApplicationQuery -> ApplicationClientInput
submitRead invocation query = Submit invocation (ReadApplication query)

-- | Submit the current local destructive-take operation.
submitLocalTake ::
  ApplicationInvocationId ->
  ApplicationQuery ->
  ApplicationClientInput
submitLocalTake invocation query = Submit invocation (LocalTakeApplication query)

-- | Submit the current level-triggered wait operation.
submitWait ::
  ApplicationInvocationId ->
  [ApplicationQuery] ->
  ApplicationClientInput
submitWait invocation queries = Submit invocation (WaitApplication queries)

-- | Submit one compare-and-swap using the exact sampled owner/generation pair.
submitLabel ::
  ApplicationInvocationId ->
  PrivateObjectId ->
  ApplicationLabel ->
  ApplicationLabelTarget ->
  ApplicationClientInput
submitLabel invocation object expected target =
  Submit invocation (LabelApplication object expected target)

-- | Submit creation of one complete private environment.
submitNewEnvironment :: ApplicationInvocationId -> ApplicationClientInput
submitNewEnvironment invocation = Submit invocation NewEnvironmentApplication
