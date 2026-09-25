{-# LANGUAGE CPP #-}

module PublicRuntimeReference where

import Data.Coerce (coerce)
import Eclips.Herald.Runtime.Connection
  ( AdministrationPlane,
    ApplicationPlane,
    ConnectionRef,
    PeerPlane,
  )

#if defined(RUNTIME_REF_CONSTRUCTOR)
import Eclips.Herald.Runtime.Connection (ConnectionRef (..))

ownerOnlyConstructor = ConnectionRef
#elif defined(APPLICATION_PEER_REF)
invalidReferenceCoercion :: ConnectionRef ApplicationPlane -> ConnectionRef PeerPlane
#elif defined(APPLICATION_ADMINISTRATION_REF)
invalidReferenceCoercion :: ConnectionRef ApplicationPlane -> ConnectionRef AdministrationPlane
#elif defined(ADMINISTRATION_PEER_REF)
invalidReferenceCoercion :: ConnectionRef AdministrationPlane -> ConnectionRef PeerPlane
#else
#error "select one runtime-reference boundary"
#endif

#if !defined(RUNTIME_REF_CONSTRUCTOR)
invalidReferenceCoercion = coerce
#endif
