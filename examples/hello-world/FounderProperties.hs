module FounderProperties (propFounderConfiguration) where

import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
import Eclips.Domain.Identity (controlIndex)
import Eclips.Domain.Startup
  ( HeraldMember (..),
    allPredefinedSortRoles,
    appliedBootstrapManifestId,
    appliedProcessEnvironmentEdges,
    appliedProcessEnvironmentObjects,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootCatalogueRole,
    appliedRootControlPrerequisite,
    appliedRootObjectId,
  )
import Eclips.Herald.Genesis
  ( checkedConfigurationDigest,
    checkedInitialProjectionDigest,
  )
import Eclips.Oracle.Genesis
  ( checkedOracleActiveHeralds,
    checkedOracleAppliedBootstraps,
    checkedOracleConfigurationDigest,
    checkedOracleControlIndex,
    checkedOracleInitialProjectionDigest,
    checkedOracleRaftNativeConfiguration,
    checkedOracleRaftVoterBindings,
    checkedOracleSystemId,
    raftVoterBindingHeraldEpoch,
    raftVoterBindingNode,
  )
import Eclips.Raft.Genesis (raftNativeVoters)
import FounderConfiguration
  ( founderBootstraps,
    founderConfiguration,
    founderHeraldGenesis,
    founderLauncherBootstrap,
    founderOracleGenesis,
  )
import Test.QuickCheck
  ( Property,
    arbitrary,
    conjoin,
    counterexample,
    forAll,
    vectorOf,
    (===),
  )

-- | Generated founder inputs must produce coherent checked compositions,
-- with one member, voter, launcher, twelve endpoint roots and named wiring.
propFounderConfiguration :: Property
propFounderConfiguration = forAll (vectorOf 32 arbitrary) $ \bytes ->
  let seed = ByteString.pack bytes
      nextSeed = ByteString.cons (ByteString.head seed + 1) (ByteString.tail seed)
   in case (founderConfiguration seed, founderConfiguration nextSeed) of
        (Right configuration, Right next) ->
          let oracle = founderOracleGenesis configuration
           in case ( checkedOracleActiveHeralds oracle,
                     checkedOracleRaftVoterBindings oracle,
                     checkedOracleAppliedBootstraps oracle
                   ) of
                ([member], [binding], [launcher]) ->
                  let roots = appliedProcessRoots launcher
                      objects = appliedProcessEnvironmentObjects launcher
                   in conjoin
                        [ raftNativeVoters (checkedOracleRaftNativeConfiguration oracle) === [raftVoterBindingNode binding],
                          raftVoterBindingHeraldEpoch binding === heraldMemberEpoch member,
                          appliedProcessResidence launcher === heraldMemberEpoch member,
                          appliedBootstrapManifestId launcher === founderLauncherBootstrap configuration,
                          length roots === 12,
                          Set.size (Set.fromList (fmap appliedRootObjectId roots)) === 12,
                          length objects === 31,
                          Set.size (Set.fromList objects) === 31,
                          length (appliedProcessEnvironmentEdges launcher) === 18,
                          Set.fromList (fmap appliedRootCatalogueRole roots) === Set.fromList allPredefinedSortRoles,
                          fmap appliedRootControlPrerequisite roots === replicate 12 (controlIndex 0),
                          checkedOracleControlIndex oracle === controlIndex 0,
                          checkedOracleConfigurationDigest oracle === checkedConfigurationDigest (founderHeraldGenesis configuration),
                          checkedOracleInitialProjectionDigest oracle === checkedInitialProjectionDigest (founderBootstraps configuration),
                          counterexample
                            "distinct entropy reused a run identity"
                            (checkedOracleSystemId oracle /= checkedOracleSystemId (founderOracleGenesis next)),
                          counterexample
                            "distinct entropy reused a Herald epoch"
                            (checkedOracleActiveHeralds oracle /= checkedOracleActiveHeralds (founderOracleGenesis next)),
                          counterexample
                            "distinct entropy reused a launcher attachment"
                            (founderLauncherBootstrap configuration /= founderLauncherBootstrap next)
                        ]
                _ -> counterexample "founder did not admit exactly one member, voter and launcher" False
        (Left problem, _) -> counterexample problem False
        (_, Left problem) -> counterexample problem False
