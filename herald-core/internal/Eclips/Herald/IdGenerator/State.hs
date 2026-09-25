-- | Pure owner for the Herald's deterministic generated-identity sequence.
--
-- The mapping is the exact two-block AES-256/CBC permutation documented for
-- Step 11.  AES is used only as a block permutation; this is not an encryption
-- protocol and supplies no authentication claim.
module Eclips.Herald.IdGenerator.State
  ( State,
    IdGeneratorInvariant (..),
    initialState,
    PreparedGeneratedId,
    prepareGeneratedId,
    preparedGlobalUniqueId,
    commitGeneratedId,
    PositiveCount,
    PositiveCountProblem (..),
    mkPositiveCount,
    positiveCountWord64,
    PreparedGeneratedIdRange,
    prepareGeneratedIdRange,
    preparedGlobalUniqueIds,
    commitGeneratedIdRange,
    IdGeneratorStateWitness (..),
    idGeneratorStateWitness,
    idGeneratorInitialPrefix,
    idGeneratorForbiddenPrefix,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (ecbDecrypt, ecbEncrypt), Cipher (cipherInit))
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Data.Bits (shiftR, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity (GlobalUniqueId, mkGlobalUniqueId)
import Eclips.Herald.IdGenerator.Internal
  ( GeneratorSeed,
    generatorSeedBytes,
  )

blockByteCount :: Int
blockByteCount = 16

prefixByteCount :: Int
prefixByteCount = 24

zeroBlock :: ByteString
zeroBlock = ByteString.replicate blockByteCount 0

-- | Redacted contradictions possible only at the admitted primitive boundary.
data IdGeneratorInvariant
  = IdGeneratorCipherInitializationInvariant
  | IdGeneratorPrefixDerivationInvariant
  | IdGeneratorIdentityRefinementInvariant
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | One independently keyed sequence.  Equality deliberately compares the
-- exact startup key, derived prefix, and next counter, but never the cached AES
-- context directly.
data State = State GeneratorSeed AES256 ByteString Word64

instance Eq State where
  State leftSeed _ leftPrefix leftCounter
    == State rightSeed _ rightPrefix rightCounter =
      leftSeed == rightSeed
        && leftPrefix == rightPrefix
        && leftCounter == rightCounter

-- | Derive the sole forbidden prefix, toggle its leading high bit, and start at
-- counter zero.
initialState :: GeneratorSeed -> Either IdGeneratorInvariant State
initialState seed = do
  cipher <- initializeCipher seed
  forbidden <- deriveForbiddenPrefix cipher
  prefix <- toggleInitialPrefix forbidden
  pure (State seed cipher prefix 0)

initializeCipher :: GeneratorSeed -> Either IdGeneratorInvariant AES256
initializeCipher seed = case cipherInit (generatorSeedBytes seed) of
  CryptoPassed cipher -> Right cipher
  CryptoFailed _ -> Left IdGeneratorCipherInitializationInvariant

deriveForbiddenPrefix :: AES256 -> Either IdGeneratorInvariant ByteString
deriveForbiddenPrefix cipher =
  let zeroPreimage = ecbDecrypt cipher zeroBlock
      prefix = ByteString.drop 8 zeroPreimage <> zeroPreimage
   in if ByteString.length zeroPreimage == blockByteCount
        && ByteString.length prefix == prefixByteCount
        then Right prefix
        else Left IdGeneratorPrefixDerivationInvariant

toggleInitialPrefix :: ByteString -> Either IdGeneratorInvariant ByteString
toggleInitialPrefix forbidden = case ByteString.uncons forbidden of
  Just (first, remainder)
    | ByteString.length remainder == prefixByteCount - 1 ->
        Right (ByteString.cons (first `xor` 0x80) remainder)
  _ -> Left IdGeneratorPrefixDerivationInvariant

-- | A repeatable prepared output and its already determined successor.
data PreparedGeneratedId = PreparedGeneratedId GlobalUniqueId State

prepareGeneratedId :: State -> Either IdGeneratorInvariant PreparedGeneratedId
prepareGeneratedId (State seed cipher prefix counter) = do
  let package = encodeWord64BE counter <> prefix
      (blockA, blockB) = ByteString.splitAt blockByteCount package
      blockU = ecbEncrypt cipher blockA
      blockV = ecbEncrypt cipher (xorBytes blockB blockU)
      generatedBytes = blockU <> blockV
  if ByteString.length package /= 2 * blockByteCount
    || ByteString.length blockA /= blockByteCount
    || ByteString.length blockB /= blockByteCount
    || ByteString.length blockU /= blockByteCount
    || ByteString.length blockV /= blockByteCount
    then Left IdGeneratorIdentityRefinementInvariant
    else case mkGlobalUniqueId generatedBytes of
      Left _ -> Left IdGeneratorIdentityRefinementInvariant
      Right generated ->
        Right
          ( PreparedGeneratedId
              generated
              (State seed cipher prefix (counter + 1))
          )

preparedGlobalUniqueId :: PreparedGeneratedId -> GlobalUniqueId
preparedGlobalUniqueId (PreparedGeneratedId generated _) = generated

commitGeneratedId :: PreparedGeneratedId -> State
commitGeneratedId (PreparedGeneratedId _ successor) = successor

-- | A checked non-empty range size.  The profile-0.1 resource and counter
-- premises make exhaustion and materialization limits deliberately out of
-- scope.
newtype PositiveCount = PositiveCount Word64
  deriving stock (Eq, Ord, Show)

data PositiveCountProblem = PositiveCountMustBePositive
  deriving stock (Eq, Ord, Show, Enum, Bounded)

mkPositiveCount :: Word64 -> Either PositiveCountProblem PositiveCount
mkPositiveCount 0 = Left PositiveCountMustBePositive
mkPositiveCount count = Right (PositiveCount count)

positiveCountWord64 :: PositiveCount -> Word64
positiveCountWord64 (PositiveCount count) = count

-- | One atomic preparation for a non-empty counter-ordered range.  Only the
-- complete range's successor is exposed, so callers cannot commit a prefix.
data PreparedGeneratedIdRange
  = PreparedGeneratedIdRange (NonEmpty GlobalUniqueId) State

prepareGeneratedIdRange ::
  PositiveCount ->
  State ->
  Either IdGeneratorInvariant PreparedGeneratedIdRange
prepareGeneratedIdRange (PositiveCount count) predecessor = do
  first <- prepareGeneratedId predecessor
  go
    (count - 1)
    (commitGeneratedId first)
    (preparedGlobalUniqueId first :| [])
  where
    go remaining state reversed
      | remaining == 0 =
          Right
            ( PreparedGeneratedIdRange
                (NonEmpty.reverse reversed)
                state
            )
      | otherwise = do
          prepared <- prepareGeneratedId state
          go
            (remaining - 1)
            (commitGeneratedId prepared)
            (preparedGlobalUniqueId prepared NonEmpty.<| reversed)

preparedGlobalUniqueIds ::
  PreparedGeneratedIdRange ->
  NonEmpty GlobalUniqueId
preparedGlobalUniqueIds (PreparedGeneratedIdRange generated _) = generated

commitGeneratedIdRange :: PreparedGeneratedIdRange -> State
commitGeneratedIdRange (PreparedGeneratedIdRange _ successor) = successor

-- | Safe counter-only evidence for composed validation and properties.
data IdGeneratorStateWitness = IdGeneratorStateWitness
  { witnessedNextGeneratorCounter :: Word64
  }
  deriving stock (Eq, Show)

idGeneratorStateWitness :: State -> IdGeneratorStateWitness
idGeneratorStateWitness (State _ _ _ counter) =
  IdGeneratorStateWitness counter

-- | Package-private exact prefix view used by conformance properties.
idGeneratorInitialPrefix :: State -> ByteString
idGeneratorInitialPrefix (State _ _ prefix _) = prefix

-- | Package-private derivation view used by conformance properties.
idGeneratorForbiddenPrefix :: GeneratorSeed -> Either IdGeneratorInvariant ByteString
idGeneratorForbiddenPrefix seed =
  initializeCipher seed >>= deriveForbiddenPrefix

xorBytes :: ByteString -> ByteString -> ByteString
xorBytes left right =
  ByteString.pack (ByteString.zipWith xor left right)

encodeWord64BE :: Word64 -> ByteString
encodeWord64BE value =
  ByteString.pack
    [ byte 56,
      byte 48,
      byte 40,
      byte 32,
      byte 24,
      byte 16,
      byte 8,
      byte 0
    ]
  where
    byte :: Int -> Word8
    byte bits = fromIntegral (value `shiftR` bits)
