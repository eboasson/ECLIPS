-- | Identity types exposed at the application boundary.
--
-- 'SortId' is the deliberate public exception: it is a shared content address and
-- crosses the boundary unchanged. Constructors for process-private IDs remain
-- hidden so API roles are nominal. A private ID has meaning only together with the
-- calling process epoch; no process or global identity is embedded in it.
module Eclips.Application.Types.Identity
  ( SortIdError (..),
    SortId,
    mkSortId,
    sortIdBytes,
    PrivateIdError (..),
    PrivateUniqueId,
    mkPrivateUniqueId,
    privateUniqueIdWord64,
    PrivateUniqueIdSupply,
    initialPrivateUniqueIdSupply,
    takePrivateUniqueId,
    PrivateObjectId,
    asPrivateObjectId,
    privateObjectUniqueId,
    PrivateProcessId,
    asPrivateProcessId,
    privateProcessUniqueId,
    PrivateNablaId,
    asPrivateNablaId,
    privateNablaUniqueId,
    PrivateDeltaId,
    asPrivateDeltaId,
    privateDeltaUniqueId,
  )
where

import Data.Binary (Binary (get, put))
import Data.Word (Word64)
import Eclips.Public.Types.SortId
  ( SortId,
    SortIdError (..),
    mkSortId,
    sortIdBytes,
  )

-- | A structurally invalid private-ID representation.
data PrivateIdError
  = PrivateIdIsZero
  deriving stock (Eq, Show)

-- | An opaque process-epoch-local unique ID.
--
-- Zero is reserved as a structural sentinel. Positive values carry no meaning
-- outside the process epoch whose Herald-private registry assigned them.
newtype PrivateUniqueId = PrivateUniqueId Word64
  deriving stock (Eq, Ord, Show)

-- | Current-build wire representation of a checked private identifier.
--
-- The decoder deliberately goes through 'mkPrivateUniqueId' so serialized zero
-- cannot construct the reserved representation.
instance Binary PrivateUniqueId where
  put (PrivateUniqueId value) = put value
  get = do
    value <- get
    case mkPrivateUniqueId value of
      Left _ -> fail "PrivateUniqueId must be positive"
      Right identifier -> pure identifier

-- | Check the prototype's positive-@Word64@ representation.
mkPrivateUniqueId :: Word64 -> Either PrivateIdError PrivateUniqueId
mkPrivateUniqueId value
  | value == 0 = Left PrivateIdIsZero
  | otherwise = Right (PrivateUniqueId value)

-- | Opaque state for one Herald-owned process-local allocation sequence.
--
-- Callers cannot reposition a supply. Obtaining a candidate does not install a
-- binding or grant the candidate meaning.
newtype PrivateUniqueIdSupply = PrivateUniqueIdSupply Word64

-- | Begin a process-local allocation sequence at one.
initialPrivateUniqueIdSupply :: PrivateUniqueIdSupply
initialPrivateUniqueIdSupply = PrivateUniqueIdSupply 1

-- | Take the current candidate and advance its opaque supply.
--
-- Profile 0.1 assumes every run remains far below @Word64@ exhaustion, so this
-- operation deliberately has no overflow branch or rollover contract.
takePrivateUniqueId ::
  PrivateUniqueIdSupply ->
  (PrivateUniqueId, PrivateUniqueIdSupply)
takePrivateUniqueId (PrivateUniqueIdSupply value) =
  (PrivateUniqueId value, PrivateUniqueIdSupply (value + 1))

-- | Obtain the current prototype wire representation.
--
-- Observing these bits grants no authority and does not make the ID meaningful in
-- another process epoch.
privateUniqueIdWord64 :: PrivateUniqueId -> Word64
privateUniqueIdWord64 (PrivateUniqueId value) = value

-- | A private ID refined for an object-taking application operation.
newtype PrivateObjectId = PrivateObjectId PrivateUniqueId
  deriving stock (Eq, Ord, Show)

instance Binary PrivateObjectId where
  put (PrivateObjectId identifier) = put identifier
  get = PrivateObjectId <$> get

-- | Refine a local unique ID for an object-taking API position.
asPrivateObjectId :: PrivateUniqueId -> PrivateObjectId
asPrivateObjectId = PrivateObjectId

-- | Forget the object API role while retaining the same local name.
privateObjectUniqueId :: PrivateObjectId -> PrivateUniqueId
privateObjectUniqueId (PrivateObjectId identifier) = identifier

-- | A private ID refined for a process-taking application operation.
newtype PrivateProcessId = PrivateProcessId PrivateUniqueId
  deriving stock (Eq, Ord, Show)

instance Binary PrivateProcessId where
  put (PrivateProcessId identifier) = put identifier
  get = PrivateProcessId <$> get

-- | Refine a local unique ID for a process-taking API position.
asPrivateProcessId :: PrivateUniqueId -> PrivateProcessId
asPrivateProcessId = PrivateProcessId

-- | Forget the process API role while retaining the same local name.
privateProcessUniqueId :: PrivateProcessId -> PrivateUniqueId
privateProcessUniqueId (PrivateProcessId identifier) = identifier

-- | A private ID refined for a nabla-taking application operation.
newtype PrivateNablaId = PrivateNablaId PrivateUniqueId
  deriving stock (Eq, Ord, Show)

instance Binary PrivateNablaId where
  put (PrivateNablaId identifier) = put identifier
  get = PrivateNablaId <$> get

-- | Refine a local unique ID for a nabla-taking API position.
asPrivateNablaId :: PrivateUniqueId -> PrivateNablaId
asPrivateNablaId = PrivateNablaId

-- | Forget the nabla API role while retaining the same local name.
privateNablaUniqueId :: PrivateNablaId -> PrivateUniqueId
privateNablaUniqueId (PrivateNablaId identifier) = identifier

-- | A private ID refined for a delta-taking application operation.
newtype PrivateDeltaId = PrivateDeltaId PrivateUniqueId
  deriving stock (Eq, Ord, Show)

instance Binary PrivateDeltaId where
  put (PrivateDeltaId identifier) = put identifier
  get = PrivateDeltaId <$> get

-- | Refine a local unique ID for a delta-taking API position.
asPrivateDeltaId :: PrivateUniqueId -> PrivateDeltaId
asPrivateDeltaId = PrivateDeltaId

-- | Forget the delta API role while retaining the same local name.
privateDeltaUniqueId :: PrivateDeltaId -> PrivateUniqueId
privateDeltaUniqueId (PrivateDeltaId identifier) = identifier
