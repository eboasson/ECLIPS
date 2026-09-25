{-# LANGUAGE CPP #-}

module PublicPrimordialOpaque where

#if defined(PRIMORDIAL_SELECTION_CONSTRUCTOR)
import Eclips.Application.Types.Access (PrimordialSelection (..))

forgedSelection :: PrimordialSelection
forgedSelection = PrimordialSelection undefined
#elif defined(PRIMORDIAL_ACCESS_CONSTRUCTOR)
import Eclips.Application.Types.Access (PrimordialAccess (..))

forgedAccess :: PrimordialAccess
forgedAccess = PrimordialAccess undefined
#elif defined(PRIMORDIAL_SELECTION_RECORD)
import Data.Map.Strict qualified as Map
import Eclips.Application.Types.Access (PrimordialSelection, selectionEntries)

forgedSelection :: PrimordialSelection -> PrimordialSelection
forgedSelection selection = selection {selectionEntries = Map.empty}
#elif defined(PRIMORDIAL_ACCESS_RECORD)
import Data.Map.Strict qualified as Map
import Eclips.Application.Types.Access (PrimordialAccess, accessEntries)

forgedAccess :: PrimordialAccess -> PrimordialAccess
forgedAccess access = access {accessEntries = Map.empty}
#elif defined(PRIMORDIAL_SELECTION_ACCESS_COERCION)
import Data.Coerce (coerce)
import Eclips.Application.Types.Access (PrimordialAccess, PrimordialSelection)

forgedAccess :: PrimordialSelection -> PrimordialAccess
forgedAccess = coerce
#elif defined(PRIMORDIAL_GRANT_INTERNAL_IMPORT)
import Eclips.Herald.Application.Primordial (CheckedPrimordialGrant)

privateGrant :: Maybe CheckedPrimordialGrant
privateGrant = Nothing
#else
#error "select one primordial opacity fixture"
#endif
