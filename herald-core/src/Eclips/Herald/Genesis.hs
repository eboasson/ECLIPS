-- | Checked pure startup boundary for one Herald.
--
-- The ordinary API exposes constructible deployment vocabulary, typed checking
-- failures, and opaque checked products.  Semantic startup records and read
-- ports used by kernel leaves remain in the package-private implementation;
-- future Oracle code imports their shared vocabulary from
-- "Eclips.Domain.Startup" rather than depending on this package.
module Eclips.Herald.Genesis
  ( PredefinedSortRole (..),
    HeraldMember (..),
    PredefinedCatalogueManifest (..),
    PrimordialSortWriterAuthority (..),
    PrimordialDefinitionManifest (..),
    ConfiguredRootManifest (..),
    ConfiguredProcessManifest (..),
    OracleGenesisManifest (..),
    DeploymentManifest (..),
    profilePredefinedCatalogueManifest,
    GenesisError (..),
    CheckedHeraldGenesis,
    checkHeraldGenesis,
    checkJoiningHeraldGenesis,
    checkedConfigurationDigest,
    PrimordialProcessManifest (..),
    InitialTopologyEdgeManifest (..),
    InitialTopologyManifest (..),
    InitialTopologyError (..),
    BootstrapFault (..),
    InitialProjectionDigest,
    initialProjectionDigestBytes,
    CheckedInitialBootstraps,
    checkInitialBootstraps,
    checkInitialBootstrapsWithTopology,
    checkedInitialProjectionDigest,
  )
where

import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    InitialTopologyEdgeManifest (..),
    InitialTopologyError (..),
    InitialTopologyManifest (..),
    PredefinedSortRole (..),
    initialProjectionDigestBytes,
  )
import Eclips.Herald.Genesis.Internal
  ( BootstrapFault (..),
    CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (..),
    ConfiguredRootManifest (..),
    DeploymentManifest (..),
    GenesisError (..),
    HeraldMember (..),
    OracleGenesisManifest (..),
    PredefinedCatalogueManifest (..),
    PrimordialDefinitionManifest (..),
    PrimordialProcessManifest (..),
    PrimordialSortWriterAuthority (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
    checkInitialBootstrapsWithTopology,
    checkJoiningHeraldGenesis,
    checkedConfigurationDigest,
    checkedInitialProjectionDigest,
    profilePredefinedCatalogueManifest,
  )
