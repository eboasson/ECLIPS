{-# LANGUAGE CPP #-}

module PublicTypedApplicationOpaque where

#if defined(TYPED_HERALD_CONSTRUCTOR)
import Eclips.Application (Herald(..))
forged :: Herald
forged = Herald undefined undefined undefined undefined undefined
#elif defined(TYPED_CALL_CONSTRUCTOR)
import Eclips.Application.Advanced (Call(..))
forged :: Call ()
forged = Call undefined undefined
#elif defined(TYPED_LIFECYCLE_CONSTRUCTOR)
import Eclips.Application.Advanced (LifecycleCall(..))
forged :: LifecycleCall ()
forged = LifecycleCall undefined undefined
#elif defined(TYPED_HERALD_RECORD)
import Eclips.Application (Herald, startup)
forged :: Herald -> Herald
forged value = value {startup = undefined}
#elif defined(TYPED_CALL_RECORD)
import Eclips.Application.Advanced (Call, status)
forged :: Call () -> Call ()
forged value = value {status = undefined}
#elif defined(TYPED_LIFECYCLE_RECORD)
import Eclips.Application.Advanced (LifecycleCall, lifecycleRequestId)
forged :: LifecycleCall () -> LifecycleCall ()
forged value = value {lifecycleRequestId = undefined}
#elif defined(TYPED_RESULT_MISMATCH)
import Eclips.Application (Herald)
import Eclips.Application.Advanced (Call, Operation(NewEnvironment), submit)
import Eclips.Application.Types.Identity (PrivateUniqueId)
forged :: Herald -> IO (Call PrivateUniqueId)
forged herald = submit herald NewEnvironment
#elif defined(TYPED_RESULT_COERCION)
import Data.Coerce (coerce)
import Eclips.Application.Advanced (Call)
import Eclips.Application.Types.Access (EnvironmentAccess)
import Eclips.Application.Types.Identity (PrivateUniqueId)
forged :: Call EnvironmentAccess -> Call PrivateUniqueId
forged = coerce
#elif defined(TYPED_INTERNAL_IMPORT)
import Eclips.Application.Internal (Herald)
forged :: Maybe Herald
forged = Nothing
#else
#error "select one typed application opacity probe"
#endif
