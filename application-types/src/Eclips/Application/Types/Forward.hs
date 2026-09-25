{-# LANGUAGE DeriveAnyClass #-}

-- | Successful completion of the application @forward@ operation.
module Eclips.Application.Types.Forward
  ( ForwardResult (..),
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)

-- | A forward has been accepted through the ordinary or structural publication
-- path. Structural acceptance becomes terminal only after its covering topology
-- cut is established at the home Herald.
data ForwardResult
  = ForwardAccepted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
