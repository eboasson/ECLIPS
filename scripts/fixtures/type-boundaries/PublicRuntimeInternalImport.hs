{-# LANGUAGE CPP #-}

module PublicRuntimeInternalImport where

#if defined(RUNTIME_INTERNAL_TYPES)
import Eclips.Herald.Runtime.Internal.Types
#elif defined(RUNTIME_EXACT_TRACE)
import Eclips.Herald.Runtime.Internal.Trace
#elif defined(RUNTIME_OWNER)
import Eclips.Herald.Runtime.Internal.Owner
#elif defined(RUNTIME_RECORDING)
import Eclips.Herald.Runtime.Internal.Recording
#elif defined(RUNTIME_CONNECTION)
import Eclips.Herald.Runtime.Internal.Connection
#elif defined(RUNTIME_TIMER)
import Eclips.Herald.Runtime.Internal.Timer
#elif defined(RUNTIME_PRIVATE_DEPENDENCY)
import Eclips.Herald.Runtime.Internal.Types
#else
#error "select one private runtime module"
#endif

unreachable :: ()
unreachable = ()
