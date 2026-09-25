-- | Direct typed operations through one scoped Herald session.
module Eclips.Application
  ( Herald,
    ConnectError (..),
    CallError (..),
    ApplicationUnavailableReason (..),
    LifecycleCallError (..),
    CancelResult (..),
    module Eclips.Application.Connection,
    connect,
    disconnect,
    withHerald,
    startup,
    newid,
    newenv,
    write,
    forward,
    read,
    localTake,
    wait,
    label,
    prepareChild,
    awaitChildReady,
    cancelChild,
    endProcess,
  )
where

import Eclips.Application.Connection
import Eclips.Application.Internal
  ( CallError (..),
    CancelResult (..),
    ConnectError (..),
    Herald,
    LifecycleCallError (..),
    connect,
    disconnect,
    startup,
    withHerald,
  )
import Eclips.Application.Internal qualified as Internal
import Eclips.Application.Runtime (ApplicationUnavailableReason (..))
import Eclips.Application.Types.Access (EnvironmentAccess, PrimordialSelection)
import Eclips.Application.Types.Forward (ForwardResult)
import Eclips.Application.Types.Identity (PrivateNablaId, PrivateObjectId, PrivateUniqueId)
import Eclips.Application.Types.Label (ApplicationLabelTarget, LabelResult)
import Eclips.Application.Types.Lifecycle (ChildPreparation, HeraldLocator, PreparedChild, preparedChildPreparation)
import Eclips.Application.Types.NewId (NewIdTarget)
import Eclips.Application.Types.Query (ApplicationQuery)
import Eclips.Application.Types.Result (WaitResult)
import Eclips.Application.Types.Value (ApplicationLabel, ApplicationValue)
import Eclips.Application.Types.Write (ApplicationWriteValue, WriteResult)
import Prelude hiding (read)

newid :: Herald -> NewIdTarget -> IO (Either CallError PrivateUniqueId)
newid herald target = call herald (Internal.NewId target)

newenv :: Herald -> IO (Either CallError EnvironmentAccess)
newenv herald = call herald Internal.NewEnvironment

write :: Herald -> PrivateNablaId -> ApplicationWriteValue -> IO (Either CallError WriteResult)
write herald writer value = call herald (Internal.Write writer value)

forward :: Herald -> PrivateNablaId -> PrivateObjectId -> IO (Either CallError ForwardResult)
forward herald writer object = call herald (Internal.Forward writer object)

read :: Herald -> ApplicationQuery -> IO (Either CallError [ApplicationValue])
read herald query = call herald (Internal.Read query)

localTake :: Herald -> ApplicationQuery -> IO (Either CallError [ApplicationValue])
localTake herald query = call herald (Internal.LocalTake query)

wait :: Herald -> [ApplicationQuery] -> IO (Either CallError WaitResult)
wait herald queries = call herald (Internal.Wait queries)

-- | Compare the complete sampled owner/generation pair. A successful call
-- advances the generation once, including a transfer to the current owner.
label :: Herald -> PrivateObjectId -> ApplicationLabel -> ApplicationLabelTarget -> IO (Either CallError LabelResult)
label herald object expected target = call herald (Internal.Label object expected target)

call :: Herald -> Internal.Operation result -> IO (Either CallError result)
call herald operation = Internal.submit herald operation >>= Internal.await

-- | Preparation returns before readiness so ordinary labels can be transferred
-- to the returned process. Cancelling the caller's wait never cancels the child;
-- use Advanced.BeginChild to retain a preparation handle before it is ready.
prepareChild :: Herald -> HeraldLocator -> PrimordialSelection -> IO (Either LifecycleCallError PreparedChild)
prepareChild herald locator selection = do
  accepted <- lifecycle herald (Internal.BeginChild locator selection)
  case accepted of
    Left problem -> pure (Left problem)
    Right preparation -> lifecycle herald (Internal.AwaitPreparedChild preparation)

awaitChildReady :: Herald -> PreparedChild -> IO (Either LifecycleCallError ())
awaitChildReady herald child = lifecycle herald (Internal.AwaitChildReady (preparedChildPreparation child))

cancelChild :: Herald -> ChildPreparation -> IO (Either LifecycleCallError CancelResult)
cancelChild herald preparation = lifecycle herald (Internal.CancelChild preparation)

endProcess :: Herald -> IO (Either LifecycleCallError ())
endProcess herald = lifecycle herald Internal.EndProcess

lifecycle :: Herald -> Internal.LifecycleOperation result -> IO (Either LifecycleCallError result)
lifecycle herald operation = Internal.submitLifecycle herald operation >>= Internal.awaitLifecycle
