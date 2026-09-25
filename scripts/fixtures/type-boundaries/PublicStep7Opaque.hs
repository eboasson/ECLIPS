{-# LANGUAGE CPP #-}

module PublicStep7Opaque where

#if defined(DISCOVERY_BINDING_GENERATION)
import Eclips.Herald.Discovery (PeerBindingGeneration (..))

ownerOnlyConstructor = PeerBindingGeneration
#elif defined(DISCOVERY_BINDING)
import Eclips.Herald.Discovery (PeerBinding (..))

ownerOnlyConstructor = PeerBinding
#elif defined(PLACEMENT_SEQUENCE)
import Eclips.Herald.Placement (PlacementSequence (..))

ownerOnlyConstructor = PlacementSequence
#elif defined(PLACEMENT_ROUTE)
import Eclips.Herald.Placement (DeltaRoute (..))

ownerOnlyConstructor = ApplicationDeltaRoute
#elif defined(PLACEMENT_SNAPSHOT)
import Eclips.Herald.Placement (PlacementSnapshot (..))

ownerOnlyConstructor = PlacementSnapshot
#else
#error "select one Step-7 opaque-constructor fixture"
#endif
