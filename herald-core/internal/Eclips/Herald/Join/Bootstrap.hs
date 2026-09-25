{-# LANGUAGE OverloadedStrings #-}

-- | The exact private system-view contribution of a post-genesis Herald.
-- There are no application roots, bootstrap writers, or exposing edges here.
module Eclips.Herald.Join.Bootstrap
  ( HeraldBootstrapContribution,
    MembershipAdmissionOrigin,
    membershipAdmissionOrigin,
    HeraldBootstrapProblem (..),
    heraldBootstrapContribution,
    contributionSystem,
    contributionAdmission,
    contributionHerald,
    contributionViews,
    contributionOrigin,
    contributionCanonicalBytes,
    contributionDigest,
    decodeContribution,
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Serialize.Get qualified as Get
import Data.Serialize.Put qualified as Put
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Sort.Profile (PredefinedSortRole, allPredefinedSortRoles)
import Eclips.Domain.Startup (deriveSystemViewDeltaId, deriveSystemViewStoreIncarnationId)
import Eclips.Domain.Topology (TopologyOccurrenceDigest, deriveTopologyOccurrenceDigest)

-- | A topology origin. It supplies no publication or process authority.
newtype MembershipAdmissionOrigin = MembershipAdmissionOrigin HeraldAdmissionId
  deriving stock (Eq, Ord, Show)

membershipAdmissionOrigin :: MembershipAdmissionOrigin -> HeraldAdmissionId
membershipAdmissionOrigin (MembershipAdmissionOrigin admission) = admission

data HeraldBootstrapContribution = HeraldBootstrapContribution SystemId HeraldAdmissionId HeraldEpoch
  deriving stock (Eq, Ord, Show)

data HeraldBootstrapProblem
  = HeraldBootstrapIdentityCollision
  | HeraldBootstrapNonCanonical
  deriving stock (Eq, Show)

-- | Check disjointness against the owner's complete retained catalogues. Shape
-- and derivation are fixed by the closed predefined catalogue.
heraldBootstrapContribution :: SystemId -> HeraldAdmissionId -> HeraldEpoch -> Set GlobalObjectId -> Set StoreIncarnationId -> Either HeraldBootstrapProblem HeraldBootstrapContribution
heraldBootstrapContribution system admission herald objects incarnations = do
  let contribution = HeraldBootstrapContribution system admission herald
      views = contributionViews contribution
      deltas = Set.fromList [globalObjectIdFromDeltaId delta | (_, delta, _) <- views]
      stores = Set.fromList [incarnation | (_, _, incarnation) <- views]
  unless
    ( Set.size deltas == length allPredefinedSortRoles
        && Set.size stores == length allPredefinedSortRoles
        && Set.null (deltas `Set.intersection` objects)
        && Set.null (stores `Set.intersection` incarnations)
        && Set.null (Set.map globalObjectIdBytes deltas `Set.intersection` Set.map storeIncarnationIdBytes stores)
    )
    (Left HeraldBootstrapIdentityCollision)
  pure contribution

contributionSystem :: HeraldBootstrapContribution -> SystemId
contributionSystem (HeraldBootstrapContribution system _ _) = system
contributionAdmission :: HeraldBootstrapContribution -> HeraldAdmissionId
contributionAdmission (HeraldBootstrapContribution _ admission _) = admission
contributionHerald :: HeraldBootstrapContribution -> HeraldEpoch
contributionHerald (HeraldBootstrapContribution _ _ herald) = herald
contributionOrigin :: HeraldBootstrapContribution -> MembershipAdmissionOrigin
contributionOrigin = MembershipAdmissionOrigin . contributionAdmission

contributionViews :: HeraldBootstrapContribution -> [(PredefinedSortRole, DeltaId, StoreIncarnationId)]
contributionViews (HeraldBootstrapContribution system _ herald) =
  [ (role, deriveSystemViewDeltaId system herald role, deriveSystemViewStoreIncarnationId system herald role)
  | role <- allPredefinedSortRoles
  ]

-- The complete derived manifest is committed; decoding does not accept an
-- arbitrary role list or arbitrary delta/incarnation claims.
contributionCanonicalBytes :: HeraldBootstrapContribution -> ByteString
contributionCanonicalBytes contribution = Put.runPut $ do
  Put.putByteString "ECLIPS-HERALD-BOOTSTRAP"
  Put.putByteString (systemIdBytes (contributionSystem contribution))
  Put.putWord64be (controlIndexWord64 (heraldAdmissionControlIndex (contributionAdmission contribution)))
  Put.putByteString (heraldAdmissionIdBytes (contributionAdmission contribution))
  Put.putByteString (heraldEpochBytes (contributionHerald contribution))
  mapM_ (\(_, delta, incarnation) -> Put.putByteString (deltaIdBytes delta) >> Put.putByteString (storeIncarnationIdBytes incarnation)) (contributionViews contribution)

contributionDigest :: HeraldBootstrapContribution -> TopologyOccurrenceDigest
contributionDigest = deriveTopologyOccurrenceDigest . contributionCanonicalBytes

decodeContribution :: ByteString -> Either HeraldBootstrapProblem HeraldBootstrapContribution
decodeContribution bytes = do
  (system, admission, herald) <- either (const (Left HeraldBootstrapNonCanonical)) Right $ Get.runGet parser bytes
  let contribution = HeraldBootstrapContribution system admission herald
  unless (contributionCanonicalBytes contribution == bytes) (Left HeraldBootstrapNonCanonical)
  pure contribution
  where
    parser = do
      tag <- Get.getByteString 21
      unless (tag == "ECLIPS-HERALD-BOOTSTRAP") (fail "bootstrap tag")
      system <- checked mkSystemId =<< Get.getByteString 32
      admission <- checked (deriveHeraldAdmissionId . controlIndex) =<< Get.getWord64be
      _ <- Get.getByteString 32
      herald <- checked mkHeraldEpoch =<< Get.getByteString 32
      _ <- Get.getByteString (length allPredefinedSortRoles * 64)
      pure (system, admission, herald)
    checked constructor = either (fail . show) pure . constructor
