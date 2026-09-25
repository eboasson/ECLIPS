-- | The sole host-entropy boundary for one Herald generator key.
module Eclips.Herald.Runtime.Internal.GeneratorSeed
  ( runtimeGeneratorSeedSource,
    systemRuntimeGeneratorSeedSource,
  )
where

import Data.ByteString (ByteString)
import Eclips.Herald.Runtime.Internal.Types
  ( RuntimeGeneratorSeedSource (..),
  )
import System.Entropy (getEntropy)

-- | Supply the exact bytes used to initialize one runtime's pure generator.
-- The action is invoked once by each attempted runtime start.
runtimeGeneratorSeedSource :: IO ByteString -> RuntimeGeneratorSeedSource
runtimeGeneratorSeedSource = RuntimeGeneratorSeedSource

-- | Obtain one 32-byte generator key from the host entropy source.
systemRuntimeGeneratorSeedSource :: RuntimeGeneratorSeedSource
systemRuntimeGeneratorSeedSource = RuntimeGeneratorSeedSource (getEntropy 32)
