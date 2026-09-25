-- | Build the deterministic seal claim from complete source-owned histories.
-- Owners still validate local catalogue disjointness and installed material.
module Eclips.Herald.Join.Seal (prepareJoinSeal) where

import Control.Monad (unless)
import Data.List (find, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity (TopologyCutId)
import Eclips.Domain.Membership
import Eclips.Domain.Topology
import Eclips.Herald.Graph.Protocol qualified as Graph
import Eclips.Herald.Join.Bootstrap qualified as Bootstrap
import Eclips.Herald.Join.History
import Eclips.Oracle.Admission

prepareJoinSeal :: HeraldAdmissionRecord -> [JoinSourceHistory] -> Either String HeraldJoinSeal
prepareJoinSeal record supplied = do
  let sources = sortOn (joinHistorySource . sourceJoinHistory) supplied
      histories = map sourceJoinHistory sources
      manifest = admissionRecordManifest record
      admission = admissionRecordId record
      attempt = admissionRecordAttempt record
      members = NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record))
  unless (map joinHistorySource histories == members) (Left "join histories do not cover the exact predecessor membership")
  let prefix = maximum (map joinHistoryControlPrefix histories)
  unless
    ( all
        ( \history ->
            joinHistorySystem history == admissionManifestSystem manifest
              && joinHistoryAdmission history == admission
              && joinHistoryAttempt history == attempt
              && topologyFrontierMembershipGenerationId (topologyCutFrontier (joinHistoryTopologyCut history)) == heraldMembershipGenerationId (admissionRecordPredecessor record)
        )
        histories
    )
    (Left "join histories disagree on the system, admission, attempt, or predecessor membership")
  dominant <-
    maybe
      (Left "join histories have no common certified descendant cut")
      Right
      (find (\candidate -> all (certifiedAncestor candidate . joinHistoryTopologyCut) histories) histories)
  let cut = joinHistoryTopologyCut dominant
  unless (prefix >= admissionRecordSemanticToken record) (Left "join histories precede the admission semantic prefix")
  contribution <-
    checked
      ( Bootstrap.heraldBootstrapContribution
          (admissionManifestSystem manifest)
          admission
          (admissionManifestHeraldEpoch manifest)
          Set.empty
          Set.empty
      )
  let contributionDigest = Bootstrap.contributionDigest contribution
  recipe <-
    checked
      ( heraldJoinBaseRecipe
          admission
          (admissionManifestHeraldEpoch manifest)
          (admissionRecordPredecessor record)
          cut
          contributionDigest
      )
  contributionClaim <- checked (mkHeraldJoinDigest (topologyOccurrenceDigestBytes contributionDigest))
  recipeClaim <- checked (mkHeraldJoinDigest (heraldJoinBaseRecipeDigestBytes recipe))
  checked
    ( heraldJoinSeal
        admission
        attempt
        cut
        prefix
        [(joinHistorySource (sourceJoinHistory source), joinSourceMemberCut source) | source <- sources]
        contributionClaim
        recipeClaim
    )
  where
    checked :: (Show problem) => Either problem value -> Either String value
    checked = either (Left . show) Right

-- Captures are immutable. A source which froze first may name K1 while a
-- later donor already holds its real descendant K2. Choose that retained K2
-- certificate only when its ordinary predecessor chain proves every source
-- cut; never synthesize a frontier or mutate either source's captured bytes.
certifiedAncestor :: JoinHistory -> TopologyCut -> Bool
certifiedAncestor descendant ancestor = walk Set.empty (joinHistoryTopologyCut descendant)
  where
    certificates = Map.fromList [(Graph.topologyCutEstablishedId proof, Graph.topologyCutEstablishedCut proof) | proof <- joinHistoryTopologyCertificates descendant]
    generation = topologyFrontierMembershipGenerationId (topologyCutFrontier ancestor)
    walk :: Set.Set TopologyCutId -> TopologyCut -> Bool
    walk visited cut
      | cut == ancestor = True
      | Set.member (deriveTopologyCutId cut) visited = False
      | topologyFrontierMembershipGenerationId (topologyCutFrontier cut) /= generation = False
      | otherwise = case topologyPredecessorView (topologyCutPredecessor cut) of
          SameGenerationPredecessorView previous -> maybe False (walk (Set.insert (deriveTopologyCutId cut) visited)) (Map.lookup previous certificates)
          _ -> False
