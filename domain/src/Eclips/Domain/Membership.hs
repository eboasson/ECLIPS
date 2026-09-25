{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Canonical membership generations and failure-workflow identities.
--
-- The finite history starts at checked genesis and retains every exact ordered
-- admission and retirement. Canonical generation decoding proves shape; checked history and
-- lineage separately authorize a descendant without changing immutable origins.
module Eclips.Domain.Membership
  ( MembershipIdentityError (..),
    HeraldMembershipGenerationId,
    mkHeraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    HeraldAdmissionId,
    HeraldAdmissionIdentityProblem (..),
    deriveHeraldAdmissionId,
    heraldAdmissionIdBytes,
    heraldAdmissionControlIndex,
    heraldAdmissionIdCanonicalBytes,
    HeraldAdmissionIdCanonicalProblem (..),
    decodeHeraldAdmissionIdCanonicalBytes,
    HeraldFailureProbeId,
    FailureProbeIdentityProblem (..),
    heraldFailureProbeIdBytes,
    heraldFailureProbeControlIndex,
    deriveHeraldFailureProbeId,
    FailureProbeResolution (..),
    FailureProbeResolutionId,
    failureProbeResolutionIdBytes,
    failureProbeResolutionProbeId,
    failureProbeResolutionDisposition,
    deriveFailureProbeResolutionId,
    HeraldMembershipGenerationBody,
    HeraldMembershipGeneration,
    MembershipGenerationProblem (..),
    genesisHeraldMembershipGeneration,
    admitHeraldMembershipGeneration,
    retireHeraldMembershipGeneration,
    heraldMembershipGenerationId,
    heraldMembershipGenerationBody,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationChangeControlIndex,
    heraldMembershipGenerationAdmissionId,
    heraldMembershipGenerationAdmittedHeraldEpoch,
    heraldMembershipGenerationRetirementControlIndex,
    heraldMembershipGenerationRetirementId,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationCanonicalBytes,
    MembershipGenerationCanonicalProblem (..),
    decodeHeraldMembershipGenerationCanonicalBytes,
    HeraldMembershipHistory,
    HeraldMembershipLineage,
    MembershipHistoryProblem (..),
    heraldMembershipHistory,
    appendHeraldMembershipGeneration,
    heraldMembershipHistoryGenerations,
    heraldMembershipHistoryGenesis,
    heraldMembershipHistoryCurrent,
    lookupHeraldMembershipGeneration,
    heraldMembershipLineage,
    heraldMembershipLineageFrom,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageTarget,
    heraldMembershipLineageGenerations,
    heraldMembershipLineageRetiredHeraldEpochs,
    heraldMembershipLineageAdmittedHeraldEpochs,
    heraldMembershipHistoryCanonicalBytes,
    MembershipHistoryCanonicalProblem (..),
    decodeHeraldMembershipHistoryCanonicalBytes,
    heraldFailureProbeIdCanonicalBytes,
    FailureProbeIdCanonicalProblem (..),
    decodeHeraldFailureProbeIdCanonicalBytes,
    failureProbeResolutionIdCanonicalBytes,
    FailureProbeResolutionCanonicalProblem (..),
    decodeFailureProbeResolutionIdCanonicalBytes,
  )
where

import Control.Monad (foldM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    IdentityError,
    SystemId,
    controlIndex,
    controlIndexWord64,
    heraldEpochBytes,
    mkHeraldEpoch,
    mkSystemId,
    systemIdBytes,
  )
import Eclips.Domain.MemberSet
  ( MemberSetDigest,
    TopologyDigestError,
    deriveMemberSetDigest,
    memberSetDigestBytes,
    mkMemberSetDigest,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import GHC.Generics (Generic)

digestByteCount :: Int
digestByteCount = 32

data MembershipIdentityError = MembershipIdentityWrongByteCount
  { expectedMembershipIdentityByteCount :: Int,
    actualMembershipIdentityByteCount :: Int
  }
  deriving stock (Eq, Show)

newtype HeraldMembershipGenerationId
  = HeraldMembershipGenerationId ByteString
  deriving stock (Eq, Ord)

instance Show HeraldMembershipGenerationId where
  show = renderGroupedHex . heraldMembershipGenerationIdBytes

mkHeraldMembershipGenerationId ::
  ByteString -> Either MembershipIdentityError HeraldMembershipGenerationId
mkHeraldMembershipGenerationId bytes
  | ByteString.length bytes == digestByteCount =
      Right (HeraldMembershipGenerationId bytes)
  | otherwise =
      Left
        MembershipIdentityWrongByteCount
          { expectedMembershipIdentityByteCount = digestByteCount,
            actualMembershipIdentityByteCount = ByteString.length bytes
          }

heraldMembershipGenerationIdBytes :: HeraldMembershipGenerationId -> ByteString
heraldMembershipGenerationIdBytes (HeraldMembershipGenerationId bytes) = bytes

-- | Stable identity of the control entry opening an admission. A later
-- activation coordinate is deliberately not part of this identity.
data HeraldAdmissionId = HeraldAdmissionId ByteString ControlIndex
  deriving stock (Eq, Ord)

instance Show HeraldAdmissionId where
  show = renderGroupedHex . heraldAdmissionIdBytes

data HeraldAdmissionIdentityProblem = HeraldAdmissionControlIndexZero
  deriving stock (Eq, Show)

deriveHeraldAdmissionId :: ControlIndex -> Either HeraldAdmissionIdentityProblem HeraldAdmissionId
deriveHeraldAdmissionId index
  | controlIndexWord64 index == 0 = Left HeraldAdmissionControlIndexZero
  | otherwise = Right (HeraldAdmissionId (SHA256.hash (Serialize.encode (HeraldAdmissionDerivationTranscript "ECLIPS-HERALD-ADMISSION" (controlIndexWord64 index)))) index)

heraldAdmissionIdBytes :: HeraldAdmissionId -> ByteString
heraldAdmissionIdBytes (HeraldAdmissionId bytes _) = bytes

heraldAdmissionControlIndex :: HeraldAdmissionId -> ControlIndex
heraldAdmissionControlIndex (HeraldAdmissionId _ index) = index

heraldAdmissionIdCanonicalBytes :: HeraldAdmissionId -> ByteString
heraldAdmissionIdCanonicalBytes admission = Serialize.encode (HeraldAdmissionValueTranscript "ECLIPS-HERALD-ADMISSION-VALUE" (heraldAdmissionIdBytes admission) (controlIndexWord64 (heraldAdmissionControlIndex admission)))

data HeraldAdmissionIdCanonicalProblem
  = HeraldAdmissionIdCanonicalDecodeFailed String
  | HeraldAdmissionIdCanonicalWrongDomain ByteString
  | HeraldAdmissionIdCanonicalWrongDigestWidth Int
  | HeraldAdmissionIdCanonicalAdmissionFailed HeraldAdmissionIdentityProblem
  | HeraldAdmissionIdCanonicalIdentifierMismatch
  | HeraldAdmissionIdCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeHeraldAdmissionIdCanonicalBytes :: ByteString -> Either HeraldAdmissionIdCanonicalProblem HeraldAdmissionId
decodeHeraldAdmissionIdCanonicalBytes bytes = do
  HeraldAdmissionValueTranscript domain suppliedDigest rawIndex <- decodeCanonical HeraldAdmissionIdCanonicalDecodeFailed bytes
  if domain == "ECLIPS-HERALD-ADMISSION-VALUE" then Right () else Left (HeraldAdmissionIdCanonicalWrongDomain domain)
  if ByteString.length suppliedDigest == digestByteCount then Right () else Left (HeraldAdmissionIdCanonicalWrongDigestWidth (ByteString.length suppliedDigest))
  derived <- either (Left . HeraldAdmissionIdCanonicalAdmissionFailed) Right (deriveHeraldAdmissionId (controlIndex rawIndex))
  if heraldAdmissionIdBytes derived == suppliedDigest then Right () else Left HeraldAdmissionIdCanonicalIdentifierMismatch
  if heraldAdmissionIdCanonicalBytes derived == bytes then Right derived else Left HeraldAdmissionIdCanonicalNonCanonical

-- | Probe identity plus the canonical control coordinate from which it was
-- derived. The digest alone is exposed for embedding in surrounding protocols.
data HeraldFailureProbeId
  = HeraldFailureProbeId ByteString ControlIndex
  deriving stock (Eq, Ord)

instance Show HeraldFailureProbeId where
  show = renderGroupedHex . heraldFailureProbeIdBytes

heraldFailureProbeIdBytes :: HeraldFailureProbeId -> ByteString
heraldFailureProbeIdBytes (HeraldFailureProbeId bytes _) = bytes

heraldFailureProbeControlIndex :: HeraldFailureProbeId -> ControlIndex
heraldFailureProbeControlIndex (HeraldFailureProbeId _ index) = index

data FailureProbeIdentityProblem
  = FailureProbeControlIndexZero
  deriving stock (Eq, Show)

deriveHeraldFailureProbeId ::
  ControlIndex -> Either FailureProbeIdentityProblem HeraldFailureProbeId
deriveHeraldFailureProbeId index
  | controlIndexWord64 index == 0 = Left FailureProbeControlIndexZero
  | otherwise =
      Right
        ( HeraldFailureProbeId
            ( SHA256.hash
                ( Serialize.encode
                    (HeraldFailureProbeDerivationTranscript failureProbeDerivationDomain (controlIndexWord64 index))
                )
            )
            index
        )

data FailureProbeResolution
  = DismissFailureProbe
  | RetireFailureProbeTarget
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Deterministic identity of one mutually exclusive terminal probe outcome.
data FailureProbeResolutionId
  = FailureProbeResolutionId
      ByteString
      HeraldFailureProbeId
      FailureProbeResolution
  deriving stock (Eq, Ord)

instance Show FailureProbeResolutionId where
  show = renderGroupedHex . failureProbeResolutionIdBytes

failureProbeResolutionIdBytes :: FailureProbeResolutionId -> ByteString
failureProbeResolutionIdBytes (FailureProbeResolutionId bytes _ _) = bytes

failureProbeResolutionProbeId :: FailureProbeResolutionId -> HeraldFailureProbeId
failureProbeResolutionProbeId (FailureProbeResolutionId _ probe _) = probe

failureProbeResolutionDisposition ::
  FailureProbeResolutionId -> FailureProbeResolution
failureProbeResolutionDisposition (FailureProbeResolutionId _ _ disposition) = disposition

deriveFailureProbeResolutionId ::
  HeraldFailureProbeId -> FailureProbeResolution -> FailureProbeResolutionId
deriveFailureProbeResolutionId probe disposition =
  FailureProbeResolutionId
    ( SHA256.hash
        ( Serialize.encode
            ( FailureProbeResolutionDerivationTranscript
                failureProbeResolutionDerivationDomain
                (heraldFailureProbeIdBytes probe)
                (failureProbeResolutionTag disposition)
            )
        )
    )
    probe
    disposition

data HeraldMembershipGenerationBody
  = GenesisHeraldMembership
      SystemId
      (NonEmpty HeraldEpoch)
      MemberSetDigest
  | RetiredHeraldMembership
      HeraldMembershipGenerationId
      ControlIndex
      FailureProbeResolutionId
      HeraldEpoch
      (NonEmpty HeraldEpoch)
      MemberSetDigest
  | AdmittedHeraldMembership
      HeraldMembershipGenerationId
      ControlIndex
      HeraldAdmissionId
      HeraldEpoch
      (NonEmpty HeraldEpoch)
      MemberSetDigest
  deriving stock (Eq, Show)

data HeraldMembershipGeneration
  = HeraldMembershipGeneration
      HeraldMembershipGenerationId
      HeraldMembershipGenerationBody
  deriving stock (Eq, Show)

data MembershipGenerationProblem
  = MembershipGenerationDuplicateHeraldEpoch HeraldEpoch
  | MembershipGenerationRetirementControlIndexZero
  | MembershipGenerationTargetNotActive HeraldEpoch
  | MembershipGenerationRetirementWouldEmpty
  | MembershipGenerationRetirementControlIndexNotIncreasing ControlIndex ControlIndex
  | MembershipGenerationResolutionNotRetirement FailureProbeResolutionId
  | MembershipGenerationResolutionNotBeforeRetirement ControlIndex ControlIndex
  | MembershipGenerationAdmissionControlIndexZero
  | MembershipGenerationAdmissionControlIndexNotIncreasing ControlIndex ControlIndex
  | MembershipGenerationAdmissionNotBeforeActivation ControlIndex ControlIndex
  | MembershipGenerationTargetAlreadyActive HeraldEpoch
  deriving stock (Eq, Show)

-- | Construct generation zero, normalizing presentation order while rejecting
-- duplicate epochs.
genesisHeraldMembershipGeneration ::
  SystemId ->
  NonEmpty HeraldEpoch ->
  Either MembershipGenerationProblem HeraldMembershipGeneration
genesisHeraldMembershipGeneration system supplied = do
  active <- checkedActiveMembers supplied
  pure (generationFromBody (GenesisHeraldMembership system active (deriveMemberSetDigest active)))

-- | Insert one fresh epoch. Full history admission also excludes every retired
-- epoch, because a generation alone cannot establish historical freshness.
admitHeraldMembershipGeneration :: ControlIndex -> HeraldAdmissionId -> HeraldEpoch -> HeraldMembershipGeneration -> Either MembershipGenerationProblem HeraldMembershipGeneration
admitHeraldMembershipGeneration activationIndex admission target predecessor = do
  if controlIndexWord64 activationIndex == 0 then Left MembershipGenerationAdmissionControlIndexZero else Right ()
  case heraldMembershipGenerationChangeControlIndex predecessor of
    Just previous | activationIndex <= previous -> Left (MembershipGenerationAdmissionControlIndexNotIncreasing previous activationIndex)
    _ -> Right ()
  requireAdmissionBeforeActivation activationIndex admission
  if target `elem` heraldMembershipGenerationActiveHeraldEpochs predecessor
    then Left (MembershipGenerationTargetAlreadyActive target)
    else Right ()
  active <- checkedActiveMembers (target NonEmpty.:| NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs predecessor))
  Right (generationFromBody (AdmittedHeraldMembership (heraldMembershipGenerationId predecessor) activationIndex admission target active (deriveMemberSetDigest active)))

requireAdmissionBeforeActivation :: ControlIndex -> HeraldAdmissionId -> Either MembershipGenerationProblem ()
requireAdmissionBeforeActivation index admission
  | heraldAdmissionControlIndex admission >= index = Left (MembershipGenerationAdmissionNotBeforeActivation (heraldAdmissionControlIndex admission) index)
  | otherwise = Right ()

-- | Remove exactly one active epoch at a strictly later control coordinate,
-- retaining the exact retirement resolution rather than just its effect.
retireHeraldMembershipGeneration ::
  ControlIndex ->
  FailureProbeResolutionId ->
  HeraldEpoch ->
  HeraldMembershipGeneration ->
  Either MembershipGenerationProblem HeraldMembershipGeneration
retireHeraldMembershipGeneration retirementIndex resolution target predecessor = do
  if controlIndexWord64 retirementIndex == 0
    then Left MembershipGenerationRetirementControlIndexZero
    else Right ()
  case heraldMembershipGenerationChangeControlIndex predecessor of
    Just previous
      | retirementIndex <= previous ->
          Left (MembershipGenerationRetirementControlIndexNotIncreasing previous retirementIndex)
    _ -> Right ()
  requireRetirementResolution retirementIndex resolution
  let active = heraldMembershipGenerationActiveHeraldEpochs predecessor
  if target `elem` active
    then Right ()
    else Left (MembershipGenerationTargetNotActive target)
  successorMembers <-
    maybe
      (Left MembershipGenerationRetirementWouldEmpty)
      Right
      (NonEmpty.nonEmpty (filter (/= target) (NonEmpty.toList active)))
  pure
    ( generationFromBody
        ( RetiredHeraldMembership
            (heraldMembershipGenerationId predecessor)
            retirementIndex
            resolution
            target
            successorMembers
            (deriveMemberSetDigest successorMembers)
        )
    )

heraldMembershipGenerationId ::
  HeraldMembershipGeneration -> HeraldMembershipGenerationId
heraldMembershipGenerationId (HeraldMembershipGeneration generationId _) = generationId

heraldMembershipGenerationBody ::
  HeraldMembershipGeneration -> HeraldMembershipGenerationBody
heraldMembershipGenerationBody (HeraldMembershipGeneration _ body) = body

heraldMembershipGenerationPredecessor ::
  HeraldMembershipGeneration -> Maybe HeraldMembershipGenerationId
heraldMembershipGenerationPredecessor generation = case heraldMembershipGenerationBody generation of
  GenesisHeraldMembership {} -> Nothing
  RetiredHeraldMembership predecessor _ _ _ _ _ -> Just predecessor
  AdmittedHeraldMembership predecessor _ _ _ _ _ -> Just predecessor

heraldMembershipGenerationChangeControlIndex :: HeraldMembershipGeneration -> Maybe ControlIndex
heraldMembershipGenerationChangeControlIndex generation = case heraldMembershipGenerationBody generation of
  GenesisHeraldMembership {} -> Nothing
  RetiredHeraldMembership _ index _ _ _ _ -> Just index
  AdmittedHeraldMembership _ index _ _ _ _ -> Just index

heraldMembershipGenerationAdmissionId :: HeraldMembershipGeneration -> Maybe HeraldAdmissionId
heraldMembershipGenerationAdmissionId generation = case heraldMembershipGenerationBody generation of
  AdmittedHeraldMembership _ _ admission _ _ _ -> Just admission
  _ -> Nothing

heraldMembershipGenerationAdmittedHeraldEpoch :: HeraldMembershipGeneration -> Maybe HeraldEpoch
heraldMembershipGenerationAdmittedHeraldEpoch generation = case heraldMembershipGenerationBody generation of
  AdmittedHeraldMembership _ _ _ admitted _ _ -> Just admitted
  _ -> Nothing

heraldMembershipGenerationRetirementControlIndex ::
  HeraldMembershipGeneration -> Maybe ControlIndex
heraldMembershipGenerationRetirementControlIndex generation = case heraldMembershipGenerationBody generation of
  GenesisHeraldMembership {} -> Nothing
  AdmittedHeraldMembership {} -> Nothing
  RetiredHeraldMembership _ index _ _ _ _ -> Just index

heraldMembershipGenerationRetirementId ::
  HeraldMembershipGeneration -> Maybe FailureProbeResolutionId
heraldMembershipGenerationRetirementId generation = case heraldMembershipGenerationBody generation of
  GenesisHeraldMembership {} -> Nothing
  AdmittedHeraldMembership {} -> Nothing
  RetiredHeraldMembership _ _ resolution _ _ _ -> Just resolution

heraldMembershipGenerationRetiredHeraldEpoch ::
  HeraldMembershipGeneration -> Maybe HeraldEpoch
heraldMembershipGenerationRetiredHeraldEpoch generation = case heraldMembershipGenerationBody generation of
  GenesisHeraldMembership {} -> Nothing
  AdmittedHeraldMembership {} -> Nothing
  RetiredHeraldMembership _ _ _ retired _ _ -> Just retired

heraldMembershipGenerationActiveHeraldEpochs ::
  HeraldMembershipGeneration -> NonEmpty HeraldEpoch
heraldMembershipGenerationActiveHeraldEpochs generation = case heraldMembershipGenerationBody generation of
  GenesisHeraldMembership _ active _ -> active
  RetiredHeraldMembership _ _ _ _ active _ -> active
  AdmittedHeraldMembership _ _ _ _ active _ -> active

heraldMembershipGenerationActiveMemberSetDigest ::
  HeraldMembershipGeneration -> MemberSetDigest
heraldMembershipGenerationActiveMemberSetDigest generation = case heraldMembershipGenerationBody generation of
  GenesisHeraldMembership _ _ digest -> digest
  RetiredHeraldMembership _ _ _ _ _ digest -> digest
  AdmittedHeraldMembership _ _ _ _ _ digest -> digest

requireRetirementResolution :: ControlIndex -> FailureProbeResolutionId -> Either MembershipGenerationProblem ()
requireRetirementResolution index resolution
  | failureProbeResolutionDisposition resolution /= RetireFailureProbeTarget =
      Left (MembershipGenerationResolutionNotRetirement resolution)
  | probeIndex >= index =
      Left (MembershipGenerationResolutionNotBeforeRetirement probeIndex index)
  | otherwise = Right ()
  where
    probeIndex = heraldFailureProbeControlIndex (failureProbeResolutionProbeId resolution)

-- | A complete, genesis-rooted chain. Decoded generations become authority only
-- after every exact predecessor, change identity, coordinate and set is checked.
data HeraldMembershipHistory = HeraldMembershipHistory (NonEmpty HeraldMembershipGeneration)
  deriving stock (Eq, Show)

-- | An ordered interval selected from one checked history. The first generation
-- is the immutable origin; the last is the authority at the selected descendant.
data HeraldMembershipLineage = HeraldMembershipLineage (NonEmpty HeraldMembershipGeneration)
  deriving stock (Eq, Show)

data MembershipHistoryProblem
  = MembershipHistoryMustStartAtGenesis HeraldMembershipGenerationId
  | MembershipHistoryPredecessorMismatch HeraldMembershipGenerationId (Maybe HeraldMembershipGenerationId)
  | MembershipHistorySuccessorMismatch HeraldMembershipGenerationId HeraldMembershipGenerationId
  | MembershipHistoryInvalidSuccessor MembershipGenerationProblem
  | MembershipHistoryRetirementIdentityReused FailureProbeResolutionId
  | MembershipHistoryAdmissionIdentityReused HeraldAdmissionId
  | MembershipHistoryHeraldEpochReused HeraldEpoch
  | MembershipHistoryGenerationUnknown HeraldMembershipGenerationId
  | MembershipHistoryNotDescendant HeraldMembershipGenerationId HeraldMembershipGenerationId
  deriving stock (Eq, Show)

heraldMembershipHistory :: NonEmpty HeraldMembershipGeneration -> Either MembershipHistoryProblem HeraldMembershipHistory
heraldMembershipHistory supplied = do
  let genesis = NonEmpty.head supplied
  case heraldMembershipGenerationPredecessor genesis of
    Nothing -> Right ()
    Just _ -> Left (MembershipHistoryMustStartAtGenesis (heraldMembershipGenerationId genesis))
  foldM appendChecked (HeraldMembershipHistory (genesis NonEmpty.:| [])) (NonEmpty.tail supplied)

-- | Exact replay of the current generation is idempotent. A new generation must
-- extend the current tail; an old branch or a skipped change is never merged.
appendHeraldMembershipGeneration :: HeraldMembershipGeneration -> HeraldMembershipHistory -> Either MembershipHistoryProblem HeraldMembershipHistory
appendHeraldMembershipGeneration generation history
  | generation == heraldMembershipHistoryCurrent history = Right history
  | otherwise = appendChecked history generation

appendChecked :: HeraldMembershipHistory -> HeraldMembershipGeneration -> Either MembershipHistoryProblem HeraldMembershipHistory
appendChecked history supplied = do
  let previous = heraldMembershipHistoryCurrent history
      previousId = heraldMembershipGenerationId previous
  case heraldMembershipGenerationBody supplied of
    AdmittedHeraldMembership predecessor index admission admitted _ _ -> do
      if predecessor == previousId then Right () else Left (MembershipHistoryPredecessorMismatch previousId (Just predecessor))
      let generations = NonEmpty.toList (heraldMembershipHistoryGenerations history)
      if Just admission `elem` fmap heraldMembershipGenerationAdmissionId generations
        then Left (MembershipHistoryAdmissionIdentityReused admission)
        else Right ()
      if any (elem admitted . heraldMembershipGenerationActiveHeraldEpochs) generations
        then Left (MembershipHistoryHeraldEpochReused admitted)
        else Right ()
      expected <- either (Left . MembershipHistoryInvalidSuccessor) Right (admitHeraldMembershipGeneration index admission admitted previous)
      if supplied == expected then Right () else Left (MembershipHistorySuccessorMismatch (heraldMembershipGenerationId expected) (heraldMembershipGenerationId supplied))
      Right (HeraldMembershipHistory (heraldMembershipHistoryGenerations history <> (supplied NonEmpty.:| [])))
    RetiredHeraldMembership predecessor index resolution retired _ _ -> do
      if predecessor == previousId
        then Right ()
        else Left (MembershipHistoryPredecessorMismatch previousId (Just predecessor))
      if Just resolution `elem` fmap heraldMembershipGenerationRetirementId (NonEmpty.toList (heraldMembershipHistoryGenerations history))
        then Left (MembershipHistoryRetirementIdentityReused resolution)
        else Right ()
      expected <- either (Left . MembershipHistoryInvalidSuccessor) Right (retireHeraldMembershipGeneration index resolution retired previous)
      if supplied == expected
        then Right ()
        else Left (MembershipHistorySuccessorMismatch (heraldMembershipGenerationId expected) (heraldMembershipGenerationId supplied))
      Right (HeraldMembershipHistory (heraldMembershipHistoryGenerations history <> (supplied NonEmpty.:| [])))
    GenesisHeraldMembership {} -> Left (MembershipHistoryPredecessorMismatch previousId Nothing)

heraldMembershipHistoryGenerations :: HeraldMembershipHistory -> NonEmpty HeraldMembershipGeneration
heraldMembershipHistoryGenerations (HeraldMembershipHistory generations) = generations

heraldMembershipHistoryGenesis :: HeraldMembershipHistory -> HeraldMembershipGeneration
heraldMembershipHistoryGenesis = NonEmpty.head . heraldMembershipHistoryGenerations

heraldMembershipHistoryCurrent :: HeraldMembershipHistory -> HeraldMembershipGeneration
heraldMembershipHistoryCurrent = NonEmpty.last . heraldMembershipHistoryGenerations

lookupHeraldMembershipGeneration :: HeraldMembershipGenerationId -> HeraldMembershipHistory -> Maybe HeraldMembershipGeneration
lookupHeraldMembershipGeneration identifier = find ((== identifier) . heraldMembershipGenerationId) . heraldMembershipHistoryGenerations

heraldMembershipLineage :: HeraldMembershipGenerationId -> HeraldMembershipGenerationId -> HeraldMembershipHistory -> Either MembershipHistoryProblem HeraldMembershipLineage
heraldMembershipLineage origin target history = do
  _ <- maybe (Left (MembershipHistoryGenerationUnknown origin)) Right (lookupHeraldMembershipGeneration origin history)
  _ <- maybe (Left (MembershipHistoryGenerationUnknown target)) Right (lookupHeraldMembershipGeneration target history)
  let suffix = dropWhile ((/= origin) . heraldMembershipGenerationId) (NonEmpty.toList (heraldMembershipHistoryGenerations history))
      (before, remaining) = break ((== target) . heraldMembershipGenerationId) suffix
  case remaining of
    found : _ -> case NonEmpty.nonEmpty (before <> [found]) of
      Just generations -> Right (HeraldMembershipLineage generations)
      Nothing -> error "membership lineage interval is empty"
    [] -> Left (MembershipHistoryNotDescendant origin target)

heraldMembershipLineageGenerations :: HeraldMembershipLineage -> NonEmpty HeraldMembershipGeneration
heraldMembershipLineageGenerations (HeraldMembershipLineage generations) = generations

-- | Narrow an admitted interval to a later retained origin without rebuilding
-- genesis history or changing its terminal authority.
heraldMembershipLineageFrom :: HeraldMembershipGenerationId -> HeraldMembershipLineage -> Either MembershipHistoryProblem HeraldMembershipLineage
heraldMembershipLineageFrom origin lineage =
  maybe
    (Left (MembershipHistoryGenerationUnknown origin))
    (Right . HeraldMembershipLineage)
    (NonEmpty.nonEmpty (dropWhile ((/= origin) . heraldMembershipGenerationId) (NonEmpty.toList (heraldMembershipLineageGenerations lineage))))

heraldMembershipLineageOrigin :: HeraldMembershipLineage -> HeraldMembershipGeneration
heraldMembershipLineageOrigin = NonEmpty.head . heraldMembershipLineageGenerations

heraldMembershipLineageTarget :: HeraldMembershipLineage -> HeraldMembershipGeneration
heraldMembershipLineageTarget = NonEmpty.last . heraldMembershipLineageGenerations

heraldMembershipLineageRetiredHeraldEpochs :: HeraldMembershipLineage -> [HeraldEpoch]
heraldMembershipLineageRetiredHeraldEpochs lineage =
  [ retired
  | generation <- NonEmpty.tail (heraldMembershipLineageGenerations lineage),
    Just retired <- [heraldMembershipGenerationRetiredHeraldEpoch generation]
  ]

heraldMembershipLineageAdmittedHeraldEpochs :: HeraldMembershipLineage -> [HeraldEpoch]
heraldMembershipLineageAdmittedHeraldEpochs lineage =
  [ admitted
  | generation <- NonEmpty.tail (heraldMembershipLineageGenerations lineage),
    Just admitted <- [heraldMembershipGenerationAdmittedHeraldEpoch generation]
  ]

heraldMembershipHistoryCanonicalBytes :: HeraldMembershipHistory -> ByteString
heraldMembershipHistoryCanonicalBytes history =
  Serialize.encode (MembershipHistoryTranscript "ECLIPS-HERALD-MEMBERSHIP-HISTORY" (fmap heraldMembershipGenerationCanonicalBytes (NonEmpty.toList (heraldMembershipHistoryGenerations history))))

data MembershipHistoryCanonicalProblem
  = MembershipHistoryCanonicalDecodeFailed String
  | MembershipHistoryCanonicalWrongDomain ByteString
  | MembershipHistoryCanonicalEmpty
  | MembershipHistoryCanonicalGenerationProblem MembershipGenerationCanonicalProblem
  | MembershipHistoryCanonicalAdmissionFailed MembershipHistoryProblem
  | MembershipHistoryCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeHeraldMembershipHistoryCanonicalBytes :: ByteString -> Either MembershipHistoryCanonicalProblem HeraldMembershipHistory
decodeHeraldMembershipHistoryCanonicalBytes bytes = do
  MembershipHistoryTranscript domain encoded <- decodeCanonical MembershipHistoryCanonicalDecodeFailed bytes
  if domain == "ECLIPS-HERALD-MEMBERSHIP-HISTORY" then Right () else Left (MembershipHistoryCanonicalWrongDomain domain)
  decoded <- traverse (either (Left . MembershipHistoryCanonicalGenerationProblem) Right . decodeHeraldMembershipGenerationCanonicalBytes) encoded
  generations <- maybe (Left MembershipHistoryCanonicalEmpty) Right (NonEmpty.nonEmpty decoded)
  history <- either (Left . MembershipHistoryCanonicalAdmissionFailed) Right (heraldMembershipHistory generations)
  if heraldMembershipHistoryCanonicalBytes history == bytes then Right history else Left MembershipHistoryCanonicalNonCanonical

data MembershipHistoryTranscript = MembershipHistoryTranscript ByteString [ByteString]
  deriving stock (Generic)
  deriving anyclass (Serialize)

generationFromBody :: HeraldMembershipGenerationBody -> HeraldMembershipGeneration
generationFromBody body =
  HeraldMembershipGeneration
    (digestGenerationBody (generationBodyTranscript body))
    body

digestGenerationBody :: MembershipBodyTranscript -> HeraldMembershipGenerationId
digestGenerationBody transcript =
  generationIdentityInvariant
    ( SHA256.hash
        ( Serialize.encode
            (MembershipGenerationDerivationTranscript membershipGenerationDerivationDomain transcript)
        )
    )

generationIdentityInvariant :: ByteString -> HeraldMembershipGenerationId
generationIdentityInvariant =
  either (error . ("membership generation identity invariant: " <>) . show) id
    . mkHeraldMembershipGenerationId

checkedActiveMembers ::
  NonEmpty HeraldEpoch -> Either MembershipGenerationProblem (NonEmpty HeraldEpoch)
checkedActiveMembers supplied = do
  let members = NonEmpty.toList supplied
      unique = Set.fromList members
  case firstDuplicate members Set.empty of
    Just duplicate -> Left (MembershipGenerationDuplicateHeraldEpoch duplicate)
    Nothing ->
      case NonEmpty.nonEmpty (Set.toAscList unique) of
        Just normalized -> Right normalized
        Nothing -> error "non-empty membership normalized to empty"

firstDuplicate :: (Ord value) => [value] -> Set.Set value -> Maybe value
firstDuplicate [] _ = Nothing
firstDuplicate (value : rest) seen
  | Set.member value seen = Just value
  | otherwise = firstDuplicate rest (Set.insert value seen)

-- | Complete canonical membership value, including the externally stored ID.
heraldMembershipGenerationCanonicalBytes :: HeraldMembershipGeneration -> ByteString
heraldMembershipGenerationCanonicalBytes generation =
  Serialize.encode
    ( MembershipGenerationValueTranscript
        membershipGenerationValueDomain
        (heraldMembershipGenerationIdBytes (heraldMembershipGenerationId generation))
        (generationBodyTranscript (heraldMembershipGenerationBody generation))
    )

data MembershipGenerationCanonicalProblem
  = MembershipGenerationCanonicalDecodeFailed String
  | MembershipGenerationCanonicalWrongDomain ByteString
  | MembershipGenerationCanonicalUnknownBodyTag Word8
  | MembershipGenerationCanonicalInvalidIdentity MembershipIdentityError
  | MembershipGenerationCanonicalInvalidDomainIdentity IdentityError
  | MembershipGenerationCanonicalInvalidMemberSetDigest TopologyDigestError
  | MembershipGenerationCanonicalInvalidRetirementId FailureProbeResolutionCanonicalProblem
  | MembershipGenerationCanonicalInvalidAdmissionId HeraldAdmissionIdCanonicalProblem
  | MembershipGenerationCanonicalEmptyActiveMembers
  | MembershipGenerationCanonicalAdmissionFailed MembershipGenerationProblem
  | MembershipGenerationCanonicalIdentifierMismatch
  | MembershipGenerationCanonicalMemberSetDigestMismatch
  | MembershipGenerationCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeHeraldMembershipGenerationCanonicalBytes ::
  ByteString ->
  Either MembershipGenerationCanonicalProblem HeraldMembershipGeneration
-- A successor carries only its predecessor identity. This checked inverse
-- therefore admits the canonical membership claim; the Oracle must still
-- validate that claim against the retained predecessor generation.
decodeHeraldMembershipGenerationCanonicalBytes bytes = do
  MembershipGenerationValueTranscript domain suppliedId transcript <-
    decodeCanonical MembershipGenerationCanonicalDecodeFailed bytes
  if domain == membershipGenerationValueDomain
    then Right ()
    else Left (MembershipGenerationCanonicalWrongDomain domain)
  generationId <-
    either
      (Left . MembershipGenerationCanonicalInvalidIdentity)
      Right
      (mkHeraldMembershipGenerationId suppliedId)
  decoded <- decodeGenerationBody transcript
  if heraldMembershipGenerationId decoded == generationId
    then Right ()
    else Left MembershipGenerationCanonicalIdentifierMismatch
  if heraldMembershipGenerationCanonicalBytes decoded == bytes
    then Right decoded
    else Left MembershipGenerationCanonicalNonCanonical

decodeGenerationBody ::
  MembershipBodyTranscript ->
  Either MembershipGenerationCanonicalProblem HeraldMembershipGeneration
decodeGenerationBody transcript = case transcript of
  MembershipBodyTranscript tag systemBytes predecessorBytes index retiredBytes activeBytes digestBytes -> do
    active <- decodeActiveMembers activeBytes
    suppliedDigest <- decodeMemberDigest digestBytes
    let expectedDigest = deriveMemberSetDigest active
    if suppliedDigest == expectedDigest
      then Right ()
      else Left MembershipGenerationCanonicalMemberSetDigestMismatch
    case tag of
      0 -> do
        system <- decodeIdentity mkSystemId systemBytes
        if predecessorBytes == Nothing && index == Nothing && retiredBytes == Nothing
          then Right ()
          else Left MembershipGenerationCanonicalNonCanonical
        either
          (Left . MembershipGenerationCanonicalAdmissionFailed)
          Right
          (genesisHeraldMembershipGeneration system active)
      1 -> do
        predecessorRaw <- maybe (Left MembershipGenerationCanonicalNonCanonical) Right predecessorBytes
        predecessor <-
          either
            (Left . MembershipGenerationCanonicalInvalidIdentity)
            Right
            (mkHeraldMembershipGenerationId predecessorRaw)
        retirementIndex <- maybe (Left MembershipGenerationCanonicalNonCanonical) (Right . controlIndex) index
        (retiredRaw, resolutionRaw) <- maybe (Left MembershipGenerationCanonicalNonCanonical) Right retiredBytes
        retired <- decodeIdentity mkHeraldEpoch retiredRaw
        resolution <- either (Left . MembershipGenerationCanonicalInvalidRetirementId) Right (decodeFailureProbeResolutionIdCanonicalBytes resolutionRaw)
        if systemBytes == ByteString.empty
          then Right ()
          else Left MembershipGenerationCanonicalNonCanonical
        if controlIndexWord64 retirementIndex == 0
          then Left (MembershipGenerationCanonicalAdmissionFailed MembershipGenerationRetirementControlIndexZero)
          else Right ()
        if retired `elem` active
          then Left MembershipGenerationCanonicalNonCanonical
          else Right ()
        either (Left . MembershipGenerationCanonicalAdmissionFailed) Right (requireRetirementResolution retirementIndex resolution)
        Right
          ( generationFromBody
              (RetiredHeraldMembership predecessor retirementIndex resolution retired active suppliedDigest)
          )
      2 -> do
        predecessorRaw <- maybe (Left MembershipGenerationCanonicalNonCanonical) Right predecessorBytes
        predecessor <- either (Left . MembershipGenerationCanonicalInvalidIdentity) Right (mkHeraldMembershipGenerationId predecessorRaw)
        activationIndex <- maybe (Left MembershipGenerationCanonicalNonCanonical) (Right . controlIndex) index
        (admittedRaw, admissionRaw) <- maybe (Left MembershipGenerationCanonicalNonCanonical) Right retiredBytes
        admitted <- decodeIdentity mkHeraldEpoch admittedRaw
        admission <- either (Left . MembershipGenerationCanonicalInvalidAdmissionId) Right (decodeHeraldAdmissionIdCanonicalBytes admissionRaw)
        if ByteString.null systemBytes && admitted `elem` active && NonEmpty.length active > 1 then Right () else Left MembershipGenerationCanonicalNonCanonical
        either (Left . MembershipGenerationCanonicalAdmissionFailed) Right (requireAdmissionBeforeActivation activationIndex admission)
        Right (generationFromBody (AdmittedHeraldMembership predecessor activationIndex admission admitted active suppliedDigest))
      other -> Left (MembershipGenerationCanonicalUnknownBodyTag other)

decodeActiveMembers ::
  [ByteString] -> Either MembershipGenerationCanonicalProblem (NonEmpty HeraldEpoch)
decodeActiveMembers bytes = do
  decoded <- traverse (decodeIdentity mkHeraldEpoch) bytes
  active <-
    maybe
      (Left MembershipGenerationCanonicalEmptyActiveMembers)
      Right
      (NonEmpty.nonEmpty decoded)
  either
    (Left . MembershipGenerationCanonicalAdmissionFailed)
    Right
    (checkedActiveMembers active)

decodeIdentity ::
  (ByteString -> Either IdentityError value) ->
  ByteString ->
  Either MembershipGenerationCanonicalProblem value
decodeIdentity constructor bytes =
  either
    (Left . MembershipGenerationCanonicalInvalidDomainIdentity)
    Right
    (constructor bytes)

decodeMemberDigest ::
  ByteString -> Either MembershipGenerationCanonicalProblem MemberSetDigest
decodeMemberDigest =
  either
    (Left . MembershipGenerationCanonicalInvalidMemberSetDigest)
    Right
    . mkMemberSetDigest

heraldFailureProbeIdCanonicalBytes :: HeraldFailureProbeId -> ByteString
heraldFailureProbeIdCanonicalBytes probe =
  Serialize.encode
    ( HeraldFailureProbeValueTranscript
        failureProbeValueDomain
        (heraldFailureProbeIdBytes probe)
        (controlIndexWord64 (heraldFailureProbeControlIndex probe))
    )

data FailureProbeIdCanonicalProblem
  = FailureProbeIdCanonicalDecodeFailed String
  | FailureProbeIdCanonicalWrongDomain ByteString
  | FailureProbeIdCanonicalWrongDigestWidth Int
  | FailureProbeIdCanonicalAdmissionFailed FailureProbeIdentityProblem
  | FailureProbeIdCanonicalIdentifierMismatch
  | FailureProbeIdCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeHeraldFailureProbeIdCanonicalBytes ::
  ByteString -> Either FailureProbeIdCanonicalProblem HeraldFailureProbeId
decodeHeraldFailureProbeIdCanonicalBytes bytes = do
  HeraldFailureProbeValueTranscript domain suppliedDigest rawIndex <-
    decodeCanonical FailureProbeIdCanonicalDecodeFailed bytes
  if domain == failureProbeValueDomain
    then Right ()
    else Left (FailureProbeIdCanonicalWrongDomain domain)
  if ByteString.length suppliedDigest == digestByteCount
    then Right ()
    else Left (FailureProbeIdCanonicalWrongDigestWidth (ByteString.length suppliedDigest))
  derived <-
    either
      (Left . FailureProbeIdCanonicalAdmissionFailed)
      Right
      (deriveHeraldFailureProbeId (controlIndex rawIndex))
  if heraldFailureProbeIdBytes derived == suppliedDigest
    then Right ()
    else Left FailureProbeIdCanonicalIdentifierMismatch
  if heraldFailureProbeIdCanonicalBytes derived == bytes
    then Right derived
    else Left FailureProbeIdCanonicalNonCanonical

failureProbeResolutionIdCanonicalBytes :: FailureProbeResolutionId -> ByteString
failureProbeResolutionIdCanonicalBytes resolution =
  Serialize.encode
    ( FailureProbeResolutionValueTranscript
        failureProbeResolutionValueDomain
        (failureProbeResolutionIdBytes resolution)
        (heraldFailureProbeIdCanonicalBytes (failureProbeResolutionProbeId resolution))
        (failureProbeResolutionTag (failureProbeResolutionDisposition resolution))
    )

data FailureProbeResolutionCanonicalProblem
  = FailureProbeResolutionCanonicalDecodeFailed String
  | FailureProbeResolutionCanonicalWrongDomain ByteString
  | FailureProbeResolutionCanonicalWrongDigestWidth Int
  | FailureProbeResolutionCanonicalInvalidProbe FailureProbeIdCanonicalProblem
  | FailureProbeResolutionCanonicalUnknownTag Word8
  | FailureProbeResolutionCanonicalIdentifierMismatch
  | FailureProbeResolutionCanonicalNonCanonical
  deriving stock (Eq, Show)

decodeFailureProbeResolutionIdCanonicalBytes ::
  ByteString ->
  Either FailureProbeResolutionCanonicalProblem FailureProbeResolutionId
decodeFailureProbeResolutionIdCanonicalBytes bytes = do
  FailureProbeResolutionValueTranscript domain suppliedDigest probeBytes tag <-
    decodeCanonical FailureProbeResolutionCanonicalDecodeFailed bytes
  if domain == failureProbeResolutionValueDomain
    then Right ()
    else Left (FailureProbeResolutionCanonicalWrongDomain domain)
  if ByteString.length suppliedDigest == digestByteCount
    then Right ()
    else Left (FailureProbeResolutionCanonicalWrongDigestWidth (ByteString.length suppliedDigest))
  probe <-
    either
      (Left . FailureProbeResolutionCanonicalInvalidProbe)
      Right
      (decodeHeraldFailureProbeIdCanonicalBytes probeBytes)
  disposition <- case tag of
    0 -> Right DismissFailureProbe
    1 -> Right RetireFailureProbeTarget
    other -> Left (FailureProbeResolutionCanonicalUnknownTag other)
  let derived = deriveFailureProbeResolutionId probe disposition
  if failureProbeResolutionIdBytes derived == suppliedDigest
    then Right ()
    else Left FailureProbeResolutionCanonicalIdentifierMismatch
  if failureProbeResolutionIdCanonicalBytes derived == bytes
    then Right derived
    else Left FailureProbeResolutionCanonicalNonCanonical

decodeCanonical ::
  (Serialize value) =>
  (String -> problem) -> ByteString -> Either problem value
decodeCanonical failure bytes = either (Left . failure) Right (Serialize.decode bytes)

generationBodyTranscript :: HeraldMembershipGenerationBody -> MembershipBodyTranscript
generationBodyTranscript body = case body of
  GenesisHeraldMembership system active digest ->
    MembershipBodyTranscript
      0
      (systemIdBytes system)
      Nothing
      Nothing
      Nothing
      (fmap heraldEpochBytes (NonEmpty.toList active))
      (memberSetDigestBytes digest)
  RetiredHeraldMembership predecessor index resolution retired active digest ->
    MembershipBodyTranscript
      1
      ByteString.empty
      (Just (heraldMembershipGenerationIdBytes predecessor))
      (Just (controlIndexWord64 index))
      (Just (heraldEpochBytes retired, failureProbeResolutionIdCanonicalBytes resolution))
      (fmap heraldEpochBytes (NonEmpty.toList active))
      (memberSetDigestBytes digest)
  AdmittedHeraldMembership predecessor index admission admitted active digest ->
    MembershipBodyTranscript
      2
      ByteString.empty
      (Just (heraldMembershipGenerationIdBytes predecessor))
      (Just (controlIndexWord64 index))
      (Just (heraldEpochBytes admitted, heraldAdmissionIdCanonicalBytes admission))
      (fmap heraldEpochBytes (NonEmpty.toList active))
      (memberSetDigestBytes digest)

failureProbeResolutionTag :: FailureProbeResolution -> Word8
failureProbeResolutionTag DismissFailureProbe = 0
failureProbeResolutionTag RetireFailureProbeTarget = 1

membershipGenerationDerivationDomain,
  membershipGenerationValueDomain,
  failureProbeDerivationDomain,
  failureProbeValueDomain,
  failureProbeResolutionDerivationDomain,
  failureProbeResolutionValueDomain ::
    ByteString
membershipGenerationDerivationDomain = "ECLIPS-HERALD-MEMBERSHIP-GENERATION"
membershipGenerationValueDomain = "ECLIPS-HERALD-MEMBERSHIP-GENERATION-VALUE"
failureProbeDerivationDomain = "ECLIPS-HERALD-FAILURE-PROBE"
failureProbeValueDomain = "ECLIPS-HERALD-FAILURE-PROBE-VALUE"
failureProbeResolutionDerivationDomain = "ECLIPS-HERALD-FAILURE-PROBE-RESOLUTION"
failureProbeResolutionValueDomain = "ECLIPS-HERALD-FAILURE-PROBE-RESOLUTION-VALUE"

data MembershipBodyTranscript
  = MembershipBodyTranscript
      Word8
      ByteString
      (Maybe ByteString)
      (Maybe Word64)
      (Maybe (ByteString, ByteString))
      [ByteString]
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data MembershipGenerationDerivationTranscript
  = MembershipGenerationDerivationTranscript ByteString MembershipBodyTranscript
  deriving stock (Generic)
  deriving anyclass (Serialize)

data MembershipGenerationValueTranscript
  = MembershipGenerationValueTranscript ByteString ByteString MembershipBodyTranscript
  deriving stock (Generic)
  deriving anyclass (Serialize)

data HeraldFailureProbeDerivationTranscript
  = HeraldFailureProbeDerivationTranscript ByteString Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data HeraldAdmissionDerivationTranscript = HeraldAdmissionDerivationTranscript ByteString Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data HeraldAdmissionValueTranscript = HeraldAdmissionValueTranscript ByteString ByteString Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data HeraldFailureProbeValueTranscript
  = HeraldFailureProbeValueTranscript ByteString ByteString Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data FailureProbeResolutionDerivationTranscript
  = FailureProbeResolutionDerivationTranscript ByteString ByteString Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)

data FailureProbeResolutionValueTranscript
  = FailureProbeResolutionValueTranscript ByteString ByteString ByteString Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)
