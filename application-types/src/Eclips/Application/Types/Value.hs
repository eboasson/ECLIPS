{-# LANGUAGE DeriveAnyClass #-}

-- | Values at the application boundary.
--
-- Unique identities are process-private names.  Record field names remain
-- ordinary text here: the Herald admits their semantic shape when it translates
-- a value into the checked domain.  Consequently this package needs no semantic-
-- domain dependency and cannot expose a global identity accidentally.
module Eclips.Application.Types.Value
  ( ApplicationLabel,
    ApplicationLabelOwner (..),
    ApplicationValue (..),
  )
where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Types.Identity
  ( PrivateProcessId,
    PrivateUniqueId,
  )
import Eclips.Application.Types.SortDescriptor (ApplicationSortDefinition)
import GHC.Generics (Generic)

-- | The owner component of an application-private label.
data ApplicationLabelOwner
  = VoidLabel
  | ProcessLabel PrivateProcessId
  | ZombieLabel PrivateProcessId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | The owner and per-object generation used both by samples and label CAS.
-- Successful label operations increment the generation, even for the same owner.
-- Private/global translation changes only the process identity component.
type ApplicationLabel = (ApplicationLabelOwner, Word64)

-- | The recursively constructible application value tree.
--
-- A record uses an ordered map so recursive translation has a canonical field
-- order independent of insertion history.  Textual field-name admission belongs
-- to the Herald/domain boundary rather than this data carrier.  The structured
-- sort-definition arm is interpreted only by its dedicated semantic adapter; it
-- cannot be mistaken for an ordinary record carrying canonical descriptor bytes.
-- An enum value carries its exact declared symbol; the selected descriptor
-- supplies and checks the closed member set.
data ApplicationValue
  = BoolValue Bool
  | Int64Value Int64
  | BytesValue ByteString
  | TextValue Text
  | UniqueIdValue PrivateUniqueId
  | LabelValue ApplicationLabel
  | RecordValue (Map Text ApplicationValue)
  | SortDefinitionValue ApplicationSortDefinition
  | EnumValue Text
  | OptionalUniqueIdValue (Maybe PrivateUniqueId)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)
