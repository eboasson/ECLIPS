-- | Checked startup material for the Herald-owned identity generator.
--
-- The seed is intentionally opaque at the public kernel boundary.  Its exact
-- bytes remain available only to the package-private generator and runtime
-- recording boundary.
module Eclips.Herald.IdGenerator
  ( GeneratorSeed,
    StartupSeedError (..),
    mkGeneratorSeed,
  )
where

import Eclips.Herald.IdGenerator.Internal
  ( GeneratorSeed,
    StartupSeedError (..),
    mkGeneratorSeed,
  )
