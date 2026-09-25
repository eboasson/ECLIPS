{-# LANGUAGE CPP #-}

module ProtocolClaimSeparation where

#if defined(CLAIM_IS_BINDING)
import Eclips.Herald.Application.Session (ApplicationSessionBinding)
import Eclips.Protocol.Application.Types (ApplicationSessionClaim)

claimIsNotBinding :: ApplicationSessionClaim -> ApplicationSessionBinding
claimIsNotBinding = id
#elif defined(GLOBAL_ID_IS_SCOPE)
import Eclips.Domain.Identity (GlobalUniqueId)
import Eclips.Protocol.Application.Types
  ( ApplicationClaimError,
    ApplicationSessionClaim,
    applicationSessionClaim,
  )

globalIdIsNotProtocolScope ::
  GlobalUniqueId ->
  Either ApplicationClaimError ApplicationSessionClaim
globalIdIsNotProtocolScope global = applicationSessionClaim global 0
#elif defined(DELETE_RESERVED_IS_OPERATION)
import Eclips.Application.Types.Identity (PrivateObjectId)
import Eclips.Application.Types.Write (ApplicationWriteValue (DeleteReserved))
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (Call),
    ApplicationRequestIdClaim,
    ApplicationSessionClaim,
  )

deleteReservedIsNotOperation ::
  ApplicationSessionClaim ->
  ApplicationRequestIdClaim ->
  PrivateObjectId ->
  ApplicationClientDto
deleteReservedIsNotOperation session request object =
  Call session request (DeleteReserved object)
#else
#error "select one Step-8 nominal-separation fixture"
#endif
