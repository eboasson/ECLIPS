{-# LANGUAGE CPP #-}

module PublicStep7InternalImport where

#if defined(DISCOVERY_INTERNAL)
import Eclips.Herald.Discovery.Internal
#elif defined(DISCOVERY_STATE)
import Eclips.Herald.Discovery.State
#elif defined(PEER_LIVENESS_INTERNAL)
import Eclips.Herald.PeerLiveness.Internal ()
#elif defined(PEER_LIVENESS_STATE)
import Eclips.Herald.PeerLiveness.State ()
#elif defined(TIMER_INTERNAL)
import Eclips.Herald.Timer.Internal ()
#elif defined(PLACEMENT_STATE)
import Eclips.Herald.Placement.State
#elif defined(PUBLICATION_STATE)
import Eclips.Herald.Publication.State
#elif defined(PEER_PUBLICATION)
import Eclips.Herald.PeerPublication (PublicationBatch)

privateTypeMustRemainHidden :: Maybe PublicationBatch
privateTypeMustRemainHidden = Nothing
#elif defined(PEER_STREAM)
import Eclips.Herald.PeerStream (StreamSequence)

privateTypeMustRemainHidden :: Maybe StreamSequence
privateTypeMustRemainHidden = Nothing
#elif defined(PEER_STREAM_STATE)
import Eclips.Herald.PeerStream.State (State)

privateTypeMustRemainHidden :: Maybe (State ())
privateTypeMustRemainHidden = Nothing
#elif defined(PEER_CONTROL)
import Eclips.Herald.UseCase.PeerControl (applyPeerControl)

privateFunctionMustRemainHidden = applyPeerControl
#elif defined(PEER_INPUT)
import Eclips.Herald.UseCase.PeerInput (PeerInputState)

privateTypeMustRemainHidden :: Maybe PeerInputState
privateTypeMustRemainHidden = Nothing
#elif defined(PEER_PLACEMENT)
import Eclips.Herald.UseCase.PeerPlacement (remotePlacementRouteIsValid)

privateFunctionMustRemainHidden = remotePlacementRouteIsValid
#else
#error "select one Step-7 private-module fixture"
#endif

unreachable :: ()
unreachable = ()
