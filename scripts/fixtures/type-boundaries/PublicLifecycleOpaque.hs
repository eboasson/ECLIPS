{-# LANGUAGE CPP #-}

module PublicLifecycleOpaque where

#if defined(LIFECYCLE_DESCRIPTOR_CONSTRUCTOR)
import Eclips.Application.Types.Lifecycle (ConnectionDescriptor (..))
forged :: ConnectionDescriptor
forged = ConnectionDescriptor undefined undefined undefined undefined
#elif defined(LIFECYCLE_CLAIM_CONSTRUCTOR)
import Eclips.Application.Types.Lifecycle (InitialClaimId (..))
forged :: InitialClaimId
forged = InitialClaimId undefined
#elif defined(LIFECYCLE_PREPARATION_CONSTRUCTOR)
import Eclips.Application.Types.Lifecycle (ChildPreparation (..))
forged :: ChildPreparation
forged = ChildPreparation undefined undefined undefined
#elif defined(LIFECYCLE_REQUEST_CONSTRUCTOR)
import Eclips.Application.Types.Lifecycle (LifecycleRequestId (..))
forged :: LifecycleRequestId
forged = LifecycleRequestId undefined undefined undefined
#elif defined(LIFECYCLE_CHILD_CONSTRUCTOR)
import Eclips.Application.Types.Lifecycle (PreparedChild (..))
forged :: PreparedChild
forged = PreparedChild undefined undefined undefined
#elif defined(LIFECYCLE_DESCRIPTOR_RECORD)
import Data.ByteString qualified as Bytes
import Eclips.Application.Types.Lifecycle (ConnectionDescriptor, connectionDescriptorAttachmentBytes)
forged :: ConnectionDescriptor -> ConnectionDescriptor
forged value = value {connectionDescriptorAttachmentBytes = Bytes.empty}
#elif defined(LIFECYCLE_CORRELATION_COERCION)
import Data.Coerce (coerce)
import Eclips.Application.Types.Lifecycle (ChildPreparation, LifecycleRequestId)
forged :: ChildPreparation -> LifecycleRequestId
forged = coerce
#elif defined(LIFECYCLE_OWNER_IMPORT)
import Eclips.Herald.ProcessPreparation.State (State)
privateOwner :: Maybe State
privateOwner = Nothing
#else
#error "select one lifecycle opacity probe"
#endif
