module ApplicationQueryGlobalIdentity where

import Eclips.Application.Types.Query
  ( ApplicationQueryLiteral (QueryUniqueId),
  )
import Eclips.Domain.Identity (GlobalUniqueId)

-- This must remain ill-typed even for a client that depends on both packages:
-- application query syntax accepts only a process-private identity.
invalidGlobalQueryLiteral :: GlobalUniqueId -> ApplicationQueryLiteral
invalidGlobalQueryLiteral = QueryUniqueId
