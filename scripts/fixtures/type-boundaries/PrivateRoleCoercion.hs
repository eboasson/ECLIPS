{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}

module PrivateRoleCoercion where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Word (Word64)
import Eclips.Application.Types.Identity
import Eclips.Public.Types.SortId qualified as PublicSortId

#if defined(UNIQUE_WORD64)
invalidCoercion :: PrivateUniqueId -> Word64
#elif defined(UNIQUE_OBJECT)
invalidCoercion :: PrivateUniqueId -> PrivateObjectId
#elif defined(UNIQUE_PROCESS)
invalidCoercion :: PrivateUniqueId -> PrivateProcessId
#elif defined(UNIQUE_NABLA)
invalidCoercion :: PrivateUniqueId -> PrivateNablaId
#elif defined(UNIQUE_DELTA)
invalidCoercion :: PrivateUniqueId -> PrivateDeltaId
#elif defined(SORT_BYTES)
invalidCoercion :: PublicSortId.SortId -> ByteString
#elif defined(OBJECT_PROCESS)
invalidCoercion :: PrivateObjectId -> PrivateProcessId
#elif defined(OBJECT_NABLA)
invalidCoercion :: PrivateObjectId -> PrivateNablaId
#elif defined(OBJECT_DELTA)
invalidCoercion :: PrivateObjectId -> PrivateDeltaId
#elif defined(PROCESS_NABLA)
invalidCoercion :: PrivateProcessId -> PrivateNablaId
#elif defined(PROCESS_DELTA)
invalidCoercion :: PrivateProcessId -> PrivateDeltaId
#elif defined(NABLA_DELTA)
invalidCoercion :: PrivateNablaId -> PrivateDeltaId
#else
#error "select one private-role coercion fixture"
#endif
invalidCoercion = coerce
