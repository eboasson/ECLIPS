{-# LANGUAGE DeriveAnyClass #-}

-- | The closed current target vocabulary for generated application identities.
module Eclips.Application.Types.NewId
  ( NewIdTarget (..),
  )
where

import Data.Binary (Binary)
import Eclips.Application.Types.Identity (PrivateNablaId)
import GHC.Generics (Generic)

-- | Select a bare generated identity or reserve one under a controlled writer.
--
-- The result remains process-private in both cases.  The controlled target is
-- an existing private writer handle; its role, sort, and authority are admitted
-- by the Herald that owns the calling process.
data NewIdTarget
  = BareNewId
  | ControlledNewId PrivateNablaId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
