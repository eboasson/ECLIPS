{-# LANGUAGE CPP #-}

module PublicStep8InternalImport where

#if defined(APPLICATION_CLIENT_INTERNAL)
import Eclips.Application.Client.Internal
#elif defined(APPLICATION_RECOVERY_INTERNAL)
import Eclips.Application.Client.Recovery.Internal
#elif defined(APPLICATION_RPC_INTERNAL)
import Eclips.Herald.Application.RPC.Internal
#else
#error "select one Step-8 private-module fixture"
#endif

unreachable :: ()
unreachable = ()
