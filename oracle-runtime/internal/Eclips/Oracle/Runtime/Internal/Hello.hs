-- | Pure classification at the EORC Hello/readiness boundary.
module Eclips.Oracle.Runtime.Internal.Hello
  ( OracleHelloAvailability (..),
    classifyOracleHelloAvailability,
    heraldLaneGenerationAuthorized,
    heraldLaneIdentityAuthorized,
    activeHeraldIdentities,
  )
where

import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity (HeraldEpoch, HeraldId)
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    HeraldMembershipHistory,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    heraldMembershipHistoryCurrent,
    heraldMembershipLineage,
  )
import Eclips.Oracle.Admission (HeraldAdmissionManifest, admissionManifestHeraldEpoch, admissionManifestHeraldId)

-- | Rebuilt only when the owner publishes changed semantic membership. Health
-- queries then look up one active identity without scanning retained lineage.
activeHeraldIdentities :: [HeraldAdmissionManifest] -> HeraldMembershipHistory -> Map.Map HeraldEpoch HeraldId
activeHeraldIdentities catalogue history =
  Map.fromList
    [ (admissionManifestHeraldEpoch member, admissionManifestHeraldId member)
    | member <- catalogue,
      Set.member (admissionManifestHeraldEpoch member) active
    ]
  where
    active = Set.fromList (toList (heraldMembershipGenerationActiveHeraldEpochs (heraldMembershipHistoryCurrent history)))

-- | Catalogue identity and current membership are published together by the
-- Oracle owner. A pending identity remains known without acquiring a lane.
heraldLaneIdentityAuthorized :: HeraldId -> HeraldEpoch -> HeraldMembershipGenerationId -> [HeraldAdmissionManifest] -> HeraldMembershipHistory -> Bool
heraldLaneIdentityAuthorized ident epoch generation catalogue history =
  heraldLaneGenerationAuthorized epoch generation history
    && any (\member -> admissionManifestHeraldId member == ident && admissionManifestHeraldEpoch member == epoch) catalogue

-- | Whether an authorized physical Hello has enough current routing evidence
-- to receive its one response.  Waiting keeps the already-open connection and
-- lets the owning STM facts wake it; it does not poll or infer liveness.
data OracleHelloAvailability
  = OracleHelloStopped
  | OracleHelloWaiting
  | OracleHelloActionable
  deriving stock (Eq, Show)

classifyOracleHelloAvailability ::
  (Eq node) =>
  Bool ->
  Bool ->
  node ->
  Maybe node ->
  OracleHelloAvailability
classifyOracleHelloAvailability accepting serviceReady local leaderHint
  | not accepting = OracleHelloStopped
  | serviceReady = OracleHelloActionable
  | maybe False (/= local) leaderHint = OracleHelloActionable
  | otherwise = OracleHelloWaiting

-- | An active survivor may catch up from any exact ancestor retained in the
-- Oracle owner's checked history. Historical membership is evidence of origin;
-- only the current active set grants lane authority. A retired epoch, unrelated
-- generation, or future generation never gains authority through this path.
heraldLaneGenerationAuthorized ::
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  HeraldMembershipHistory ->
  Bool
heraldLaneGenerationAuthorized epoch claimedGeneration history =
  epoch `elem` heraldMembershipGenerationActiveHeraldEpochs current
    && isRight
      (heraldMembershipLineage claimedGeneration (heraldMembershipGenerationId current) history)
  where
    current = heraldMembershipHistoryCurrent history
