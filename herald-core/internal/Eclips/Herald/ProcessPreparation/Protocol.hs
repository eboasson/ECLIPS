-- | Global-only preparation handoffs over an admitted peer binding. Transfer
-- bytes carry canonical observations; only the transfer admission owner turns
-- them into reusable checked grants.
module Eclips.Herald.ProcessPreparation.Protocol
  ( PreparationControl (..),
    RemotePreparationStatus (..),
    RemoteChild (..),
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Eclips.Application.Types.Lifecycle
import Eclips.Domain.Identity (ControlIndex, ProcessEpochId)

data RemoteChild = RemoteChild ProcessEpochId ControlIndex ConnectionDescriptor
  deriving stock (Eq, Show)

data RemotePreparationStatus
  = RemotePreparationPending [Text]
  | RemotePreparationAvailable RemoteChild (Maybe [Text])
  | RemotePreparationClaimed RemoteChild
  | RemotePreparationCancelled
  | RemotePreparationFailed StartupError
  deriving stock (Eq, Show)

data PreparationControl
  = PreparationOffered ChildPreparation HeraldLocator ByteString
  | PreparationCancelled ChildPreparation
  | PreparationQueried ChildPreparation
  | PreparationReplied ChildPreparation RemotePreparationStatus
  deriving stock (Eq, Show)
