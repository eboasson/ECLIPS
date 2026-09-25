{-# LANGUAGE CPP #-}

module OpaqueStartupMutation where

import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    PublicationId,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRoot,
    ConfiguredProcessBootstrap,
    PrimordialDefinitionReplica,
    appliedProcessResidence,
    appliedRootControlPrerequisite,
    configuredProcessBootstrapResidence,
    primordialReplicaPublicationId,
  )
import Eclips.Herald.Genesis (CheckedHeraldGenesis)
import GHC.Generics (Generic)

#if defined(CHECKED_GENESIS_RECORD)
invalidMutation ::
  CheckedHeraldGenesis -> HeraldId -> CheckedHeraldGenesis
invalidMutation genesis herald =
  genesis {checkedLocalHeraldId = herald}
#elif defined(APPLIED_BOOTSTRAP_RECORD)
invalidMutation ::
  AppliedProcessBootstrap -> HeraldEpoch -> AppliedProcessBootstrap
invalidMutation bootstrap residence =
  bootstrap {appliedProcessResidence = residence}
#elif defined(CONFIGURED_PROCESS_BOOTSTRAP_RECORD)
invalidMutation ::
  ConfiguredProcessBootstrap -> HeraldEpoch -> ConfiguredProcessBootstrap
invalidMutation bootstrap residence =
  bootstrap {configuredProcessBootstrapResidence = residence}
#elif defined(PRIMORDIAL_DEFINITION_REPLICA_RECORD)
invalidMutation ::
  PrimordialDefinitionReplica -> PublicationId -> PrimordialDefinitionReplica
invalidMutation replica publication =
  replica {primordialReplicaPublicationId = publication}
#elif defined(APPLIED_ROOT_RECORD)
invalidMutation :: AppliedRoot -> ControlIndex -> AppliedRoot
invalidMutation root prerequisite =
  root {appliedRootControlPrerequisite = prerequisite}
#elif defined(CHECKED_GENESIS_GENERIC)
invalidMutation :: CheckedHeraldGenesis -> ()
invalidMutation = requiresGeneric
#elif defined(APPLIED_BOOTSTRAP_GENERIC)
invalidMutation :: AppliedProcessBootstrap -> ()
invalidMutation = requiresGeneric
#else
#error "select one opaque-startup fixture"
#endif

requiresGeneric :: (Generic value) => value -> ()
requiresGeneric _ = ()
