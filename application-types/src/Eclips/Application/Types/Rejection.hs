{-# LANGUAGE DeriveAnyClass #-}

-- | Redacted semantic failures returned at the application boundary.
--
-- Detailed domain errors remain inside their owning coordinators. These
-- categories contain only private application operands and the deliberately
-- public sort identity.
module Eclips.Application.Types.Rejection
  ( ApplicationRejection (..),
  )
where

import Data.Binary (Binary)
import Data.Text (Text)
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    PrivateNablaId,
    PrivateObjectId,
    PrivateProcessId,
    PrivateUniqueId,
    SortId,
  )
import Eclips.Application.Types.SortDescriptor (ApplicationProjection)
import GHC.Generics (Generic)

-- | Ordinary stateful-admission or semantic-validation failure.
data ApplicationRejection
  = ApplicationUnknownPrivateIdentity PrivateUniqueId
  | ApplicationInvalidFieldName Text
  | ApplicationInvalidQueryProjection ApplicationProjection
  | ApplicationNablaRoleMismatch PrivateNablaId
  | ApplicationDeltaRoleMismatch PrivateDeltaId
  | ApplicationProcessRoleMismatch PrivateProcessId
  | ApplicationDeltaNotLocal PrivateDeltaId
  | ApplicationDeltaStoreUnavailable PrivateDeltaId
  | ApplicationOperateNotPermitted
  | ApplicationQueryPredicateMismatch
  | ApplicationSortDefinitionProjectionUnsupported ApplicationProjection
  | ApplicationSortDescriptorRejected
  | ApplicationSortIdClaimMismatch SortId SortId
  | ApplicationNotCurrentSortDefinitionWriter PrivateNablaId
  | ApplicationNablaSortNotControlled PrivateNablaId
  | ApplicationReservationUnavailable PrivateNablaId PrivateObjectId
  | ApplicationObjectNotLabelable PrivateObjectId
  | ApplicationLabelTargetNotNameable PrivateProcessId
  | ApplicationLabelTransitionNotPermitted
  | EnvironmentSourcesUnavailable
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
