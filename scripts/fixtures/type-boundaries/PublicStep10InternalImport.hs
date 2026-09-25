{-# LANGUAGE CPP #-}

module PublicStep10InternalImport where

#ifdef APPLICATION_API_INTERNAL
import Eclips.Application.Runtime.Internal.Owner ()
#endif

#ifdef PEER_RPC_INTERNAL
import Eclips.Herald.Peer.RPC.Internal ()
#endif

#ifdef TCP_INTERNAL_MODULE
import Eclips.Herald.Runtime.TCP.Internal.Facade ()
#endif

#ifdef TCP_HEARTBEAT
import Eclips.Herald.Runtime.TCP.Internal.Heartbeat ()
#endif

#ifdef TCP_PRIVATE_DEPENDENCY
import Eclips.Herald.Runtime.TCP.Internal.Types ()
#endif

witness :: ()
witness = ()
