-- | Pure checked construction of one complete serving Herald kernel.
module Eclips.Herald.Initialization
  ( HeraldState,
    ApplicationAttachment,
    primordialApplicationAttachment,
    HeraldInvariantFault (..),
    BootstrapInvariantCategory (..),
    StartupInvariantSubject (..),
    StartupInvariantViolation (..),
    HeraldTransitionInvariantViolation (..),
    DiagnosticChecks (..),
    configureHeraldDiagnosticChecks,
    initialHerald,
    initialHeraldWithIsolation,
    initialHeraldWithOracleVoters,
    initialHeraldWithTiming,
    initialHeraldWithTimingAndIsolation,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Application.Recovery (ApplicationRecoveryConfiguration, checkApplicationRecoveryConfiguration)
import Eclips.Herald.Application.Session
  ( ApplicationAttachment,
    primordialApplicationAttachment,
  )
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.Discovery.Internal (discoveryStatic)
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (ArmTimer, RunOracleClientAction, RunOracleHealthRound),
    orderedEffectBatch,
  )
import Eclips.Herald.FailureDetection.State qualified as FailureDetection
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    checkedActiveHeraldEpochs,
    checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedLocalHeraldId,
    checkedOracleControlIndex,
    checkedStartupAdmission,
    checkedSystemId,
  )
import Eclips.Herald.IdGenerator (GeneratorSeed)
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.Isolation (IsolationConfiguration, checkIsolationConfiguration)
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient
  ( OracleContactSet,
    oracleContacts,
    oracleHelloClaims,
    oracleNodeClaim,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerLiveness (PeerRecoveryConfiguration, checkPeerRecoveryConfiguration)
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Startup.Invariant
  ( BootstrapInvariantCategory (..),
    HeraldInvariantFault (..),
    HeraldTransitionInvariantViolation (..),
    StartupInvariantSubject (..),
    StartupInvariantViolation (..),
    validateHeraldState,
  )
import Eclips.Herald.Startup.Semantic qualified as Semantic
import Eclips.Herald.Startup.State
  ( HeraldState,
    configureHeraldDiagnosticChecks,
    heraldStatic,
    heraldStaticWithTiming,
    replaceStartupPlacementState,
    servingHeraldState,
  )
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch, raftVoterBindingNode)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.Timing
import Eclips.Raft.Identity (raftNodeIdBytes)

-- | Construct one complete serving Herald from checked startup data.
--
-- The Oracle projection retains the complete locally checked index-zero process
-- set and its peer-comparable digest. Only bootstraps resident at this Herald
-- are folded into local owner states.
-- No partial state or effect batch escapes if checked projections contradict.
initialHerald ::
  MonotonicInstant ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  GeneratorSeed ->
  ApplicationRecoveryConfiguration ->
  PeerRecoveryConfiguration ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
initialHerald observedTime genesis checkedBootstraps contacts seed applicationRecoveryConfiguration recoveryConfiguration = do
  let compatibilityConfiguration =
        case checkIsolationConfiguration 5000000 100000 of
          Right checked -> checked
          Left _ -> error "positive compatibility isolation configuration rejected"
  initialHeraldWithIsolation
    observedTime
    genesis
    checkedBootstraps
    contacts
    seed
    applicationRecoveryConfiguration
    recoveryConfiguration
    (Set.fromList (checkedActiveHeraldEpochs genesis))
    compatibilityConfiguration

-- | Construct a serving Herald with the checked local failure-detector policy.
-- The voter-host epochs are immutable Raft placement facts supplied by the
-- deployment composition, rather than inferred from mutable membership. Pure
-- startup requires them to be a non-empty subset of checked genesis membership.
initialHeraldWithIsolation ::
  MonotonicInstant ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  GeneratorSeed ->
  ApplicationRecoveryConfiguration ->
  PeerRecoveryConfiguration ->
  Set.Set HeraldEpoch ->
  IsolationConfiguration ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
initialHeraldWithIsolation observedTime genesis checkedBootstraps contacts seed applicationRecoveryConfiguration recoveryConfiguration voterHosts isolationConfiguration =
  initialHeraldWithOptions Nothing Nothing observedTime genesis checkedBootstraps contacts seed applicationRecoveryConfiguration recoveryConfiguration voterHosts isolationConfiguration

-- | Construct the complete index-zero Oracle projection from the same checked
-- immutable Oracle genesis used by its replicas. Committed voter bindings choose
-- the initial Herald roles and contact preference before any effect is emitted.
initialHeraldWithOracleVoters ::
  Voter.VoterConfiguration ->
  [Voter.OracleReplicaRegistration] ->
  MonotonicInstant ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  GeneratorSeed ->
  ApplicationRecoveryConfiguration ->
  PeerRecoveryConfiguration ->
  Maybe IsolationConfiguration ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
initialHeraldWithOracleVoters configuration registrations observedTime genesis checkedBootstraps contacts seed applicationRecoveryConfiguration recoveryConfiguration isolation = do
  policy <- case isolation of
    Just supplied -> Right supplied
    Nothing -> either (const (Left isolationInitializationFault)) Right (checkIsolationConfiguration 5000000 100000)
  initialHeraldWithOptions
    Nothing
    (Just (configuration, registrations))
    observedTime
    genesis
    checkedBootstraps
    contacts
    seed
    applicationRecoveryConfiguration
    recoveryConfiguration
    (Set.fromList (map raftVoterBindingHeraldEpoch (Voter.voterConfigurationBindings configuration)))
    policy

-- | Start with one checked deployment target. The target changes operational
-- timing and client connection descriptors, never committed membership facts.
initialHeraldWithTiming ::
  TakeoverTarget ->
  Maybe (Voter.VoterConfiguration, [Voter.OracleReplicaRegistration]) ->
  MonotonicInstant ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  GeneratorSeed ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
initialHeraldWithTiming = initialHeraldWithTimingAndIsolation Nothing

-- | Apply a checked explicit isolation override after deriving timing defaults.
-- Native registration determines voter hosts when supplied; the override's grace
-- and read-only drain remain independent operational configuration.
initialHeraldWithTimingAndIsolation ::
  Maybe (Set.Set HeraldEpoch, IsolationConfiguration) ->
  TakeoverTarget ->
  Maybe (Voter.VoterConfiguration, [Voter.OracleReplicaRegistration]) ->
  MonotonicInstant ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  GeneratorSeed ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
initialHeraldWithTimingAndIsolation configuredIsolation target oracleVoters observedTime genesis bootstraps contacts seed = do
  let timing = deriveTimingPolicy target
  applicationRecovery <- checkedTiming (checkApplicationRecoveryConfiguration (timingApplicationRecoveryGraceMicroseconds timing))
  recovery <- checkedTiming (checkPeerRecoveryConfiguration (timingPeerRecoveryGraceMicroseconds timing))
  isolation <- case configuredIsolation of
    Just (_, supplied) -> Right supplied
    Nothing -> checkedTiming (checkIsolationConfiguration (timingIsolationGraceMicroseconds timing) defaultReadOnlyDrainMicroseconds)
  let voters =
        maybe
          (maybe (Set.fromList (checkedActiveHeraldEpochs genesis)) fst configuredIsolation)
          (Set.fromList . map raftVoterBindingHeraldEpoch . Voter.voterConfigurationBindings . fst)
          oracleVoters
  initialHeraldWithOptions (Just target) oracleVoters observedTime genesis bootstraps contacts seed applicationRecovery recovery voters isolation
  where
    checkedTiming = either (const (Left isolationInitializationFault)) Right

initialHeraldWithOptions ::
  Maybe TakeoverTarget ->
  Maybe (Voter.VoterConfiguration, [Voter.OracleReplicaRegistration]) ->
  MonotonicInstant ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  GeneratorSeed ->
  ApplicationRecoveryConfiguration ->
  PeerRecoveryConfiguration ->
  Set.Set HeraldEpoch ->
  IsolationConfiguration ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
initialHeraldWithOptions timingTarget oracleVoters observedTime genesis checkedBootstraps contacts seed applicationRecoveryConfiguration recoveryConfiguration voterHosts isolationConfiguration = do
  if Set.null voterHosts || not (voterHosts `Set.isSubsetOf` genesisHeralds)
    then Left isolationInitializationFault
    else Right ()
  configuredOracleState <- case oracleVoters of
    Nothing -> Right oracleState
    Just (configuration, registrations) ->
      either
        (const (Left oracleInitializationFault))
        Right
        (OracleProjection.initializeOracleVoters configuration registrations oracleState)
  configuredOracleClient <- case oracleVoters of
    Nothing -> Right oracleClientState
    Just (configuration, _) -> do
      nodes <-
        traverse
          (either (const (Left oracleInitializationFault)) Right . oracleNodeClaim . raftNodeIdBytes . raftVoterBindingNode)
          (Voter.voterConfigurationBindings configuration)
      Right (OracleClient.preferOracleVoters (Set.fromList nodes) oracleClientState)
  semantic <- Semantic.prepareInitialSemanticOwners genesis checkedBootstraps
  generatorState <-
    either
      (const (Left generatorInitializationFault))
      Right
      (IdGenerator.initialState seed)
  (isolationState, isolationDisposition) <-
    either
      (const (Left isolationInitializationFault))
      Right
      ( Isolation.initialState
          observedTime
          localEpoch
          voterHosts
          (OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView oracleState))
          isolationConfiguration
      )
  let initialMembership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView oracleState)
  let discoveryConfiguration =
        discoveryStatic
          (checkedSystemId genesis)
          (checkedLocalHeraldId genesis)
          localEpoch
          (checkedActiveHeralds genesis)
          (checkedCatalogueDigest genesis)
          (checkedInitialProjectionDigest checkedBootstraps)
          initialMembership
  discoveryState <- case checkedStartupAdmission genesis of
    Nothing -> Right (Discovery.initialState discoveryConfiguration)
    Just admission ->
      either
        (const (Left structuralProgressInitializationFault))
        Right
        (Discovery.initialJoiningState discoveryConfiguration admission)
  let configuredIsolation = case oracleVoters of
        Nothing -> isolationState
        Just (configuration, _) ->
          let configure = case timingTarget of
                Nothing -> Isolation.configureOracleHealth
                Just target -> Isolation.configureOracleHealthWithCadence (timingHealthRoundMicroseconds (deriveTimingPolicy target))
           in configure (Voter.voterConfigurationNativeRef configuration) (Voter.voterConfigurationNative configuration) isolationState
  let state =
        replaceStartupPlacementState (Placement.clearAlignmentLossChange (Placement.clearAlignmentPlanChange (Placement.clearPreparationPlacementChanges (Semantic.initialSemanticPlacement semantic))))
          $ servingHeraldState
            (maybe heraldStatic heraldStaticWithTiming timingTarget genesis checkedBootstraps)
            applicationRecoveryConfiguration
            observedTime
            generatorState
            configuredOracleClient
            configuredOracleState
            (Semantic.initialSemanticRegistry semantic)
            (Semantic.initialSemanticGraph semantic)
            (Semantic.initialSemanticProgress semantic)
            discoveryState
            (PeerLiveness.initialState recoveryConfiguration)
            ( case timingTarget of
                Nothing -> FailureDetection.initialState localEpoch voterHosts (Voter.voterConfigurationId . fst <$> oracleVoters) recoveryConfiguration
                Just target -> FailureDetection.initialStateWithProbeDuration (timingFailureProbeMicroseconds (deriveTimingPolicy target)) localEpoch voterHosts (Voter.voterConfigurationId . fst <$> oracleVoters)
            )
            configuredIsolation
            (PeerStream.initialState localEpoch)
            (Semantic.initialSemanticBootstrapOwners semantic)
  validateHeraldState state
  pure
    ( state,
      orderedEffectBatch
        ( if isJust (checkedStartupAdmission genesis)
            then []
            else
              fmap RunOracleClientAction (OracleClient.oracleClientActions configuredOracleClient)
                <> [RunOracleHealthRound roundNumber cadence (OracleClient.oracleClientHelloClaims configuredOracleClient) (NonEmpty.toList (oracleContacts contacts)) | (roundNumber, cadence) <- maybe [] pure (Isolation.oracleHealthRound configuredIsolation)]
                <> case isolationDisposition of
                  Isolation.InitialIsolationGraceArmed attempt specification ->
                    [ArmTimer attempt specification]
        )
    )
  where
    oracleState = OracleProjection.initialState genesis checkedBootstraps
    oracleClientState =
      (if isJust (checkedStartupAdmission genesis) then OracleClient.initialJoiningState else OracleClient.initialState)
        ( oracleHelloClaims
            (checkedSystemId genesis)
            (checkedCatalogueDigest genesis)
            (checkedConfigurationDigest genesis)
            (checkedInitialProjectionDigest checkedBootstraps)
            (checkedLocalHeraldId genesis)
            (checkedLocalHeraldEpoch genesis)
            ( OracleProjection.oracleViewCurrentHeraldMembershipId
                (OracleProjection.oracleView oracleState)
            )
        )
        contacts
        (checkedOracleControlIndex genesis)
    localEpoch = checkedLocalHeraldEpoch genesis
    genesisHeralds = Set.fromList (checkedActiveHeraldEpochs genesis)

generatorInitializationFault :: HeraldInvariantFault
generatorInitializationFault =
  HeraldStartupInvariant StartupStatic IdGeneratorInitializationInvariant

structuralProgressInitializationFault :: HeraldInvariantFault
structuralProgressInitializationFault =
  HeraldStartupInvariant StartupStatic StructuralProgressInvariant

isolationInitializationFault :: HeraldInvariantFault
isolationInitializationFault =
  HeraldStartupInvariant StartupStatic IsolationOwnerInvariant

oracleInitializationFault :: HeraldInvariantFault
oracleInitializationFault =
  HeraldStartupInvariant StartupStatic OracleProjectionOwnerInvariant
