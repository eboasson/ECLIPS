{-# LANGUAGE DeriveAnyClass #-}

-- | The complete application operation union executable by Herald.
--
-- This type is shared by the client, protocol, and Herald owners so retained
-- typed equality is the sole retry identity. Unsupported future operations are
-- absent rather than represented by inert constructors.
module Eclips.Application.Types.Operation
  ( ApplicationOperation (..),
  )
where

import Data.Binary (Binary)
import Eclips.Application.Types.Identity (PrivateNablaId, PrivateObjectId)
import Eclips.Application.Types.Label (ApplicationLabelTarget)
import Eclips.Application.Types.NewId (NewIdTarget)
import Eclips.Application.Types.Query (ApplicationQuery)
import Eclips.Application.Types.Value (ApplicationLabel)
import Eclips.Application.Types.Write (ApplicationWriteValue)
import GHC.Generics (Generic)

-- | The eight regular calls implemented by the application owner.  Label
-- carries its expected effective owner/generation pair before the target.
data ApplicationOperation
  = NewIdApplication NewIdTarget
  | WriteApplication PrivateNablaId ApplicationWriteValue
  | ForwardApplication PrivateNablaId PrivateObjectId
  | ReadApplication ApplicationQuery
  | LocalTakeApplication ApplicationQuery
  | WaitApplication [ApplicationQuery]
  | LabelApplication PrivateObjectId ApplicationLabel ApplicationLabelTarget
  | NewEnvironmentApplication
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
