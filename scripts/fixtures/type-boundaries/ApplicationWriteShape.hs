module ApplicationWriteShape where

import Eclips.Application.Types.Identity (PrivateObjectId)
import Eclips.Application.Types.Value (ApplicationValue)
import Eclips.Application.Types.Write (ApplicationWriteValue (DeleteReserved))

-- This must remain ill-typed: reservation cancellation is an outer write-input
-- arm, not an application value that could be nested or published.
invalidDeleteValue :: PrivateObjectId -> ApplicationValue
invalidDeleteValue = DeleteReserved
