-- | Package-private, non-authoritative derived data. Owner transitions retain
-- or replace these values explicitly when their semantic inputs change.
module Eclips.Herald.Internal.Derived
  ( Derived,
    derive,
    derivedValue,
  )
where

-- | The payload is deliberately lazy. Equality and display concern the
-- semantic owner, never whether a derived calculation has been demanded.
-- Owners must construct this value from the exact required inputs, without
-- closing over an earlier owner and its previous derived value.
data Derived value = Derived value

derive :: value -> Derived value
derive = Derived

derivedValue :: Derived value -> value
derivedValue (Derived value) = value

instance Eq (Derived value) where
  _ == _ = True

instance Show (Derived value) where
  showsPrec _ _ = showString "<derived>"
