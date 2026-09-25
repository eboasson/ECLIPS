{-# LANGUAGE CPP #-}

module PeerClaimRoleCoercion where

import Data.Coerce (coerce)
import Eclips.Protocol.Peer.Types

#if defined(SYSTEM_HERALD_ID)
invalidCoercion :: SystemClaim -> HeraldIdClaim
#elif defined(HERALD_ID_EPOCH)
invalidCoercion :: HeraldIdClaim -> HeraldEpochClaim
#elif defined(GLOBAL_PROCESS)
invalidCoercion :: GlobalObjectClaim -> ProcessEpochClaim
#elif defined(NABLA_DELTA)
invalidCoercion :: NablaClaim -> DeltaClaim
#elif defined(CATALOGUE_PROJECTION)
invalidCoercion :: CatalogueDigestClaim -> InitialProjectionDigestClaim
#elif defined(PROJECTION_ITEM)
invalidCoercion :: InitialProjectionDigestClaim -> PeerItemDigestClaim
#else
#error "select one peer-claim role coercion fixture"
#endif
invalidCoercion = coerce
