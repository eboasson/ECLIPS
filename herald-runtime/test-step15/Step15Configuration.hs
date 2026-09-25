{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fixed four-Herald configuration for the Step-15 failure acceptance tour.
--
-- H1, H2, and H3 cohost the three Oracle/Raft voters. H4 is an active Herald
-- member, but deliberately has no Oracle/Raft voter binding.
module Step15Configuration
  ( Step15HeraldName (..),
    Step15HeraldRole (..),
    Step15RuntimeProfile (..),
    step15HeraldNames,
    step15HeraldEpoch,
    step15HeraldRole,
    step15ActiveHeraldEpochs,
    step15OracleVoterBindings,
    step15OracleVoterHostEpochs,
    step15OracleVoterNodes,
    step15OracleTcpConfiguration,
    step15OracleTcpConfigurationFor,
    step15OracleContacts,
    awaitStep15OracleReady,
    step15TcpConfiguration,
    step15TcpConfigurationFor,
  )
where

import Data.List (sort)
import Data.Set qualified as Set
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Startup (heraldMemberEpoch)
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.OracleClient (OracleContactSet)
import Eclips.Herald.Runtime
  ( HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    configureHeraldRuntimeIsolation,
    configureHeraldRuntimeOracleVoters,
    heraldRuntimeConfiguration,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.TCP
  ( ConfiguredPeerSeed,
    HeraldTcpConfiguration,
    TcpListenEndpoint,
    heartbeatConfigurationMicroseconds,
    heraldTcpConfiguration,
    peerDialRetryDelayMicroseconds,
  )
import Eclips.Oracle.Genesis
  ( checkedOracleActiveHeralds,
    checkedOracleRaftVoterBindings,
    raftVoterBindingHeraldEpoch,
    raftVoterBindingNode,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    OracleTcpClusterConfiguration,
  )
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Identity (RaftNodeId)
import Step12Fixtures
  ( awaitStep12ReadyLeader,
    step12OracleContacts,
  )
import Step15Genesis
  ( repeatedRetirementCheckedOracleGenesis,
    repeatedRetirementOracleTcpConfiguration,
    step15CheckedOracleGenesis,
    step15H1GeneratorSeedSource,
    step15H1Genesis,
    step15H1HeraldEpoch,
    step15H1RoutedBootstraps,
    step15H2GeneratorSeedSource,
    step15H2Genesis,
    step15H2HeraldEpoch,
    step15H2RoutedBootstraps,
    step15H3GeneratorSeedSource,
    step15H3Genesis,
    step15H3HeraldEpoch,
    step15H3RoutedBootstraps,
    step15H4GeneratorSeedSource,
    step15H4Genesis,
    step15H4HeraldEpoch,
    step15H4RoutedBootstraps,
    step15RoutedOracleTcpConfiguration,
  )

data Step15HeraldName
  = Step15H1
  | Step15H2
  | Step15H3
  | Step15H4
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Step15HeraldRole
  = Step15OracleVoterHost
  | Step15NonVoter
  deriving stock (Eq, Show)

data Step15RuntimeProfile
  = Step15StandardProfile
  | Step15CrashAcceptanceProfile
  | Step15ApplicationLossProfile
  | RepeatedRetirementProfile
  | RepeatedRetirementSelfFenceProfile
  deriving stock (Eq, Show)

step15HeraldNames :: [Step15HeraldName]
step15HeraldNames = [minBound .. maxBound]

step15HeraldEpoch :: Step15HeraldName -> HeraldEpoch
step15HeraldEpoch = \case
  Step15H1 -> step15H1HeraldEpoch
  Step15H2 -> step15H2HeraldEpoch
  Step15H3 -> step15H3HeraldEpoch
  Step15H4 -> step15H4HeraldEpoch

step15HeraldRole :: Step15HeraldName -> Step15HeraldRole
step15HeraldRole = \case
  Step15H1 -> Step15OracleVoterHost
  Step15H2 -> Step15OracleVoterHost
  Step15H3 -> Step15OracleVoterHost
  Step15H4 -> Step15NonVoter

-- | Active membership read back from the checked Oracle genesis rather than
-- restating the intended topology in the acceptance property.
step15ActiveHeraldEpochs :: [HeraldEpoch]
step15ActiveHeraldEpochs =
  sort
    (fmap heraldMemberEpoch (checkedOracleActiveHeralds step15CheckedOracleGenesis))

-- | Voter-host membership read back from the checked Raft/Oracle bindings.
step15OracleVoterBindings :: [(Step15HeraldName, RaftNodeId)]
step15OracleVoterBindings =
  sort
    [ (step15HeraldNameForEpoch (raftVoterBindingHeraldEpoch binding), raftVoterBindingNode binding)
    | binding <- checkedOracleRaftVoterBindings step15CheckedOracleGenesis
    ]

step15OracleVoterHostEpochs :: [HeraldEpoch]
step15OracleVoterHostEpochs =
  fmap (step15HeraldEpoch . fst) step15OracleVoterBindings

step15OracleVoterNodes :: [RaftNodeId]
step15OracleVoterNodes = sort (fmap snd step15OracleVoterBindings)

step15HeraldNameForEpoch :: HeraldEpoch -> Step15HeraldName
step15HeraldNameForEpoch epoch =
  case filter ((== epoch) . step15HeraldEpoch) step15HeraldNames of
    [name] -> name
    _ -> error ("checked Step-15 voter binding names an unknown Herald epoch: " <> show epoch)

step15OracleTcpConfiguration :: OracleTcpClusterConfiguration
step15OracleTcpConfiguration = step15RoutedOracleTcpConfiguration

step15OracleTcpConfigurationFor :: Step15RuntimeProfile -> OracleTcpClusterConfiguration
step15OracleTcpConfigurationFor RepeatedRetirementProfile = repeatedRetirementOracleTcpConfiguration
step15OracleTcpConfigurationFor RepeatedRetirementSelfFenceProfile = repeatedRetirementOracleTcpConfiguration
step15OracleTcpConfigurationFor _ = step15OracleTcpConfiguration

step15OracleContacts :: OracleTcpCluster -> OracleContactSet
step15OracleContacts = step12OracleContacts

awaitStep15OracleReady :: OracleTcpCluster -> IO RaftNodeId
awaitStep15OracleReady = awaitStep12ReadyLeader

step15TcpConfiguration ::
  TcpListenEndpoint ->
  OracleContactSet ->
  Step15HeraldName ->
  [ConfiguredPeerSeed] ->
  HeraldTcpConfiguration
step15TcpConfiguration listener contacts name seeds =
  step15TcpConfigurationFor Step15StandardProfile listener contacts name seeds

step15TcpConfigurationFor ::
  Step15RuntimeProfile ->
  TcpListenEndpoint ->
  OracleContactSet ->
  Step15HeraldName ->
  [ConfiguredPeerSeed] ->
  HeraldTcpConfiguration
step15TcpConfigurationFor profile listener contacts name seeds =
  heraldTcpConfiguration
    (step15RuntimeConfiguration profile contacts name)
    listener
    listener
    listener
    seeds
    (checked "Step-15 peer retry delay" (peerDialRetryDelayMicroseconds 10_000))
    (checked "Step-15 heartbeat" (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))

step15RuntimeConfiguration ::
  Step15RuntimeProfile ->
  OracleContactSet ->
  Step15HeraldName ->
  HeraldRuntimeConfiguration
step15RuntimeConfiguration profile contacts name =
  configureHeraldRuntimeOracleVoters (Voter.oracleVoterConfiguration initial) (Voter.oracleReplicaRegistrations initial)
    $ configureHeraldRuntimeIsolation
      (Set.fromList voterHosts)
      isolation
      ( heraldRuntimeConfiguration
          genesis
          bootstraps
          contacts
          generatorSeedSource
          systemRuntimeMonotonicClock
          (runtimePeerWorkDelayMicroseconds 0)
          (heraldRuntimeHandlers (const (pure ())))
          applicationRecovery
          peerRecovery
      )
  where
    -- Use the same checked index-zero authority as the real Oracle cluster.
    -- Native health and captured failure evidence need its complete voter
    -- configuration and registrations, including the P05 singleton variant.
    oracleGenesis = case profile of
      RepeatedRetirementProfile -> repeatedRetirementCheckedOracleGenesis
      RepeatedRetirementSelfFenceProfile -> repeatedRetirementCheckedOracleGenesis
      _ -> step15CheckedOracleGenesis
    initial = checked "Step-15 initial Oracle authority" (initialOracle oracleGenesis)
    voterHosts = fmap raftVoterBindingHeraldEpoch (checkedOracleRaftVoterBindings oracleGenesis)
    (genesis, bootstraps, generatorSeedSource) = step15HeraldInputs name
    (applicationRecovery, isolation, peerRecovery) = case profile of
      Step15StandardProfile ->
        ( checked "Step-15 application recovery" (checkApplicationRecoveryConfiguration 30_000_000),
          checked "Step-15 isolation" (checkIsolationConfiguration 2_000_000 1_000_000),
          checked "Step-15 peer recovery" (checkPeerRecoveryConfiguration 30_000_000)
        )
      -- The real crash executable owns this deliberately short detector
      -- profile. Ordinary runtimes and the transient-cut foundation retain
      -- the 30-second recovery grace above.
      RepeatedRetirementProfile ->
        ( checked "P05 application recovery" (checkApplicationRecoveryConfiguration 30_000_000),
          checked "P05 isolation" (checkIsolationConfiguration 30_000_000 1_000_000),
          checked "P05 peer recovery" (checkPeerRecoveryConfiguration 250_000)
        )
      -- A removed executing epoch may lose its next EORC watch at the exact
      -- retirement boundary. This case also observes the lawful quorum-loss
      -- fence, with an explicit short grace inside the existing case deadline.
      RepeatedRetirementSelfFenceProfile ->
        ( checked "P05 self-fence application recovery" (checkApplicationRecoveryConfiguration 30_000_000),
          checked "P05 self-fence isolation" (checkIsolationConfiguration 2_000_000 1_000_000),
          checked "P05 self-fence peer recovery" (checkPeerRecoveryConfiguration 250_000)
        )
      Step15CrashAcceptanceProfile ->
        ( checked "Step-15 crash application recovery" (checkApplicationRecoveryConfiguration 30_000_000),
          checked "Step-15 crash isolation" (checkIsolationConfiguration 30_000_000 1_000_000),
          checked "Step-15 crash peer recovery" (checkPeerRecoveryConfiguration 250_000)
        )
      -- The application-loss tour needs to observe expiry without changing
      -- the ordinary peer and isolation deadlines exercised by the same real
      -- four-Herald fixture.
      Step15ApplicationLossProfile ->
        ( checked "Step-15 loss application recovery" (checkApplicationRecoveryConfiguration 250_000),
          checked "Step-15 loss isolation" (checkIsolationConfiguration 2_000_000 1_000_000),
          checked "Step-15 loss peer recovery" (checkPeerRecoveryConfiguration 30_000_000)
        )

step15HeraldInputs ::
  Step15HeraldName ->
  (CheckedHeraldGenesis, CheckedInitialBootstraps, RuntimeGeneratorSeedSource)
step15HeraldInputs = \case
  Step15H1 ->
    (step15H1Genesis, step15H1RoutedBootstraps, step15H1GeneratorSeedSource)
  Step15H2 ->
    (step15H2Genesis, step15H2RoutedBootstraps, step15H2GeneratorSeedSource)
  Step15H3 ->
    (step15H3Genesis, step15H3RoutedBootstraps, step15H3GeneratorSeedSource)
  Step15H4 ->
    (step15H4Genesis, step15H4RoutedBootstraps, step15H4GeneratorSeedSource)

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
