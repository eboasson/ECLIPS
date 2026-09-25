-- | Exact key representation shared by the private generator and trace owner.
module Eclips.Herald.IdGenerator.Internal
  ( GeneratorSeed (..),
    StartupSeedError (..),
    mkGeneratorSeed,
    generatorSeedBytes,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

generatorSeedByteCount :: Int
generatorSeedByteCount = 32

-- | Exactly one AES-256 key.  Equality is retained for deterministic state and
-- replay comparison; ordinary rendering never reveals its bytes.
newtype GeneratorSeed = GeneratorSeed ByteString
  deriving stock (Eq)

instance Show GeneratorSeed where
  show _ = "<generator-seed>"

-- | The sole structural seed-admission error.
data StartupSeedError
  = WrongGeneratorSeedByteCount
  { expectedGeneratorSeedByteCount :: Int,
    actualGeneratorSeedByteCount :: Int
  }
  deriving stock (Eq, Show)

-- | Admit exactly 32 bytes without applying an entropy-quality policy.
mkGeneratorSeed :: ByteString -> Either StartupSeedError GeneratorSeed
mkGeneratorSeed bytes
  | ByteString.length bytes == generatorSeedByteCount =
      Right (GeneratorSeed bytes)
  | otherwise =
      Left
        WrongGeneratorSeedByteCount
          { expectedGeneratorSeedByteCount = generatorSeedByteCount,
            actualGeneratorSeedByteCount = ByteString.length bytes
          }

generatorSeedBytes :: GeneratorSeed -> ByteString
generatorSeedBytes (GeneratorSeed bytes) = bytes
