{-# LANGUAGE CPP #-}

module PublicStep10Opaque where

#ifdef PEER_DIAL_INTENT_CONSTRUCTOR
import Eclips.Herald.Discovery (PeerDialIntent)
#else
import Eclips.Herald.Discovery (PeerDialIntent)
#endif

witness :: Maybe PeerDialIntent
witness = Nothing

#ifdef PEER_DIAL_INTENT_CONSTRUCTOR
forgedPeerDialIntent = PeerDialIntent
#endif
