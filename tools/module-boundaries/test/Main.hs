module Main (main) where

import Control.Exception (bracket, finally, onException)
import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as ByteString
import Data.List (isInfixOf, sort)
import Eclips.ModuleBoundaries (checkProject, renderViolations)
import System.Directory
  ( copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getDirectoryContents,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
    renameFile,
  )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO (hClose, openTempFile, readFile')

data RejectionCase = RejectionCase
  { description :: String,
    mutateFixture :: FilePath -> IO FixtureRestoration,
    expectedViolationFragment :: Maybe String
  }

newtype FixtureRestoration = FixtureRestoration
  { runFixtureRestoration :: IO ()
  }

data FixtureEntry
  = FixtureDirectory FilePath
  | FixtureFile FilePath ByteString.ByteString
  deriving stock (Eq)

main :: IO ()
main = do
  arguments <- getArgs
  projectRoot <- case arguments of
    [root] -> pure root
    _ -> die "usage: module-boundaries-negative PROJECT_ROOT"
  positive <- checkProject projectRoot
  unless (null positive) (die ("positive boundary fixture failed:\n" <> renderViolations positive))
  withFixture projectRoot $ \fixtureRoot -> do
    baseline <- fixtureManifest fixtureRoot
    assertPristineFixture "before negative cases" fixtureRoot
    cases <- rejectionCasesFor fixtureRoot
    forM_ cases $ \RejectionCase {description, mutateFixture, expectedViolationFragment} ->
      bracket (mutateFixture fixtureRoot) runFixtureRestoration $ \_ -> do
        violations <- checkProject fixtureRoot
        when (null violations) (die ("boundary checker accepted forbidden fixture: " <> description))
        forM_ expectedViolationFragment $ \fragment ->
          unless
            (fragment `isInfixOf` renderViolations violations)
            (die ("boundary checker rejected " <> description <> " for an unexpected reason:\n" <> renderViolations violations))
    assertPristineFixture "after negative cases" fixtureRoot
    restored <- fixtureManifest fixtureRoot
    unless
      (restored == baseline)
      (die "boundary fixture did not exactly match its baseline after negative cases")
  putStrLn "module-boundary negative fixtures passed"

assertPristineFixture :: String -> FilePath -> IO ()
assertPristineFixture phase fixtureRoot = do
  violations <- checkProject fixtureRoot
  unless
    (null violations)
    (die ("boundary fixture was not pristine " <> phase <> ":\n" <> renderViolations violations))

rejectionCases :: [RejectionCase]
rejectionCases =
  [ rejectionContaining "native health observer imports the Raft transition kernel" "Oracle semantic imports in Herald core must remain" $ \root ->
      insertImport (root </> "herald-core/internal/Eclips/Herald/OracleHealth/State.hs") "import Eclips.Raft.Transition qualified as ForbiddenHealthKernel",
    rejectionContaining "unrelated leaf imports native health owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport (root </> "herald-core/internal/Eclips/Herald/Wait/State.hs") "import Eclips.Herald.OracleHealth.State qualified as ForbiddenHealthOwner",
    rejectionContaining "health TCP worker imports the native runtime owner" "production Herald runtime must not depend on a Raft kernel or Oracle runtime owner" $ \root ->
      insertImport (root </> "herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/OracleHealth.hs") "import Eclips.Oracle.Runtime qualified as ForbiddenHealthRuntime",
    rejectionContaining "EADM frame imports Oracle voter semantics" "canonical Oracle voter values in EADM belong only to the reviewed DTO owner" $ \root ->
      insertImport (root </> "protocol-admin/src/Eclips/Protocol/Admin/Frame.hs") "import Eclips.Oracle.Voter qualified as ForbiddenFrameVoters",
    rejectionContaining "voter status DTO imports private Oracle owner" "administration protocol may import only" $ \root ->
      insertImport (root </> "protocol-admin/src/Eclips/Protocol/Admin/Types.hs") "import Eclips.Oracle.Internal.Voter qualified as ForbiddenVoterOwner",
    rejectionContaining "replica discovery imports private Oracle state" "deployment runtime may import only" $ \root ->
      insertImport (root </> "deployment/src/Eclips/Deployment/Discovery.hs") "import Eclips.Oracle.Internal.Label qualified as ForbiddenReplicaState",
    rejectionContaining "Herald registration observer imports Raft state" "production Herald runtime must not depend on a Raft kernel or Oracle runtime owner" $ \root ->
      insertImport (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Owner.hs") "import Eclips.Raft.State qualified as ForbiddenReplicaKernel",
    rejectionContaining "unrelated Herald leaf imports voter authority" "Oracle semantic imports in Herald core must remain" $ \root ->
      insertImport (root </> "herald-core/internal/Eclips/Herald/Wait/State.hs") "import Eclips.Oracle.Voter qualified as ForbiddenVoterAuthority",
    rejectionContaining "unrelated Herald runtime worker imports replica identity" "production Herald runtime must not depend on a Raft kernel or Oracle runtime owner" $ \root ->
      insertImport (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Connection.hs") "import Eclips.Raft.Identity qualified as ForbiddenReplicaIdentity",
    rejectionContaining "frozen source envelope imports Registry owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Join/SourceBundle.hs")
        "import Eclips.Herald.SortRegistry.State qualified as ForbiddenSourceOwner",
    rejectionContaining "admission history composer imports runtime" "Herald core must not import runtime" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Join/History.hs")
        "import Control.Concurrent.STM qualified as ForbiddenJoinRuntime",
    rejectionContaining "deployment imports private admission owner" "deployment runtime may import only" $ \root ->
      insertImport
        (root </> "deployment/src/Eclips/Deployment/Joining.hs")
        "import Eclips.Oracle.Internal.Admission qualified as ForbiddenAdmissionOwner",
    rejectionContaining "private history-transfer helper imports Herald owner" "deployment runtime may import only" $ \root ->
      insertImport
        (root </> "deployment/internal/Eclips/Deployment/Joining/HistoryTransfer.hs")
        "import Eclips.Herald.Startup.State qualified as ForbiddenHistoryState",
    rejectionContaining "private history-transfer helper becomes public" "exposed modules differ" $ \root ->
      replaceIn
        (root </> "deployment/eclips-deployment.cabal")
        "      Eclips.Deployment.Admin\n  other-modules:\n      Eclips.Deployment.Admin.Exchange\n      Eclips.Deployment.Joining.HistoryTransfer\n"
        "      Eclips.Deployment.Admin\n      Eclips.Deployment.Joining.HistoryTransfer\n  other-modules:\n      Eclips.Deployment.Admin.Exchange\n",
    rejectionContaining "admission Oracle owner imports clock" "pure Oracle core must not import runtime" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Internal/Admission.hs")
        "import Data.Time.Clock qualified as ForbiddenAdmissionClock",
    rejectionContaining "ordinary reader imports Domain" "ordinary application executables may import only" $ \root ->
      insertImport
        (root </> "examples/hello-world/applications/HelloReader.hs")
        "import Eclips.Domain.Identity qualified as ForbiddenDomain",
    rejectionContaining "ordinary launcher imports low-level runtime" "ordinary application executables may import only" $ \root ->
      insertImport
        (root </> "examples/hello-world/applications/HelloLauncher.hs")
        "import Eclips.Application.Runtime qualified as ForbiddenRuntime",
    rejectionContaining "context application imports Domain" "hello-context example may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "examples/hello-context/applications/ContextApplication.hs")
        "import Eclips.Domain.Identity qualified as ForbiddenDomain",
    rejectionContaining "context launcher imports low-level application runtime" "hello-context example may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "examples/hello-context/applications/ContextLauncher.hs")
        "import Eclips.Application.Runtime qualified as ForbiddenRuntime",
    rejectionContaining "deployment imports private Herald owner" "deployment runtime may import only" $ \root ->
      insertImport
        (root </> "deployment/src/Eclips/Deployment/Runtime.hs")
        "import Eclips.Herald.Startup.State qualified as ForbiddenState",
    rejectionContaining "deployment public surface widened" "exposed modules differ" $ \root ->
      replaceIn
        (root </> "deployment/eclips-deployment.cabal")
        "      Eclips.Deployment.Admin\n"
        "      Eclips.Deployment.Admin\n      Eclips.Deployment.Unreviewed\n",
    rejectionContaining "unledgered Step-15 membership-sensitive site" "unreviewed Step-15 membership-sensitive token: checkedActiveHeraldEpochs" $ \root ->
      appendTo
        (root </> "herald-core/internal/Eclips/Herald/Authority.hs")
        "\nstep15UnreviewedMembershipRead = checkedActiveHeraldEpochs\n",
    rejectionContaining "unreviewed checked membership history read" "unreviewed Step-15 membership-sensitive token: oracleViewHeraldMembershipHistoryChecked" $ \root ->
      appendTo
        (root </> "herald-core/internal/Eclips/Herald/Authority.hs")
        "\np05UnreviewedHistory = oracleViewHeraldMembershipHistoryChecked\n",
    rejectionContaining "unreviewed descendant projection authority" "unreviewed Step-15 membership-sensitive token: projectStructuralVersionVector" $ \root ->
      appendTo
        (root </> "herald-core/internal/Eclips/Herald/Authority.hs")
        "\np05UnreviewedProjection = projectStructuralVersionVector\n",
    rejectionContaining "unreviewed Oracle runtime history read" "unreviewed Step-15 membership-sensitive token: statusMembershipHistory" $ \root ->
      appendTo
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Hello.hs")
        "\np05UnreviewedRuntimeHistory = statusMembershipHistory\n",
    rejectionContaining "missing Step-15 membership-ledger anchor" "Step-15 membership ledger anchor MEM-GEN-001/oracleGenesisActiveHeralds" $ \root ->
      replaceIn
        (root </> "domain/src/Eclips/Domain/Startup.hs")
        "oracleGenesisActiveHeralds"
        "oracleGenesisMembers",
    rejection "unreviewed package in the default plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./examples/hello-context/eclips-hello-context.cabal"
        "    ./examples/hello-context/eclips-hello-context.cabal\n    ./unexpected/unexpected.cabal",
    rejection "inline unreviewed package in the default plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "packages:"
        "packages: ./unexpected/unexpected.cabal",
    rejection "optional unreviewed package in the default plan" $ \root ->
      appendTo (root </> "cabal.project") "optional-packages: ./unexpected/unexpected.cabal\n",
    rejection "Herald runtime omitted from the default plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./herald-runtime/eclips-herald-runtime.cabal"
        "    -- ./herald-runtime/eclips-herald-runtime.cabal",
    rejection "protocol-frame omitted from the default plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./protocol-frame/eclips-protocol-frame.cabal"
        "    -- ./protocol-frame/eclips-protocol-frame.cabal",
    rejectionContaining "administration protocol omitted from the default plan" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./protocol-admin/eclips-protocol-admin.cabal"
        "    -- ./protocol-admin/eclips-protocol-admin.cabal",
    rejectionContaining "administration protocol moved after the peer protocol" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./protocol-admin/eclips-protocol-admin.cabal\n    ./protocol-peer/eclips-protocol-peer.cabal"
        "    ./protocol-peer/eclips-protocol-peer.cabal\n    ./protocol-admin/eclips-protocol-admin.cabal",
    rejection "protocol-peer omitted from the default plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./protocol-peer/eclips-protocol-peer.cabal"
        "    -- ./protocol-peer/eclips-protocol-peer.cabal",
    rejectionContaining "Raft core omitted from the default plan" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./raft-core/eclips-raft-core.cabal"
        "    -- ./raft-core/eclips-raft-core.cabal",
    rejectionContaining "Raft core moved before the peer protocol" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./protocol-peer/eclips-protocol-peer.cabal\n    ./raft-core/eclips-raft-core.cabal"
        "    ./raft-core/eclips-raft-core.cabal\n    ./protocol-peer/eclips-protocol-peer.cabal",
    rejectionContaining "Oracle core omitted from the default plan" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./oracle-core/eclips-oracle-core.cabal"
        "    -- ./oracle-core/eclips-oracle-core.cabal",
    rejectionContaining "Oracle core moved before Raft core" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./raft-core/eclips-raft-core.cabal\n    ./oracle-core/eclips-oracle-core.cabal"
        "    ./oracle-core/eclips-oracle-core.cabal\n    ./raft-core/eclips-raft-core.cabal",
    rejectionContaining "Raft protocol omitted from the default plan" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./protocol-raft/eclips-protocol-raft.cabal"
        "    -- ./protocol-raft/eclips-protocol-raft.cabal",
    rejectionContaining "Raft protocol moved before Oracle core" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./oracle-core/eclips-oracle-core.cabal\n    ./protocol-raft/eclips-protocol-raft.cabal"
        "    ./protocol-raft/eclips-protocol-raft.cabal\n    ./oracle-core/eclips-oracle-core.cabal",
    rejectionContaining "Oracle protocol omitted from the default plan" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./protocol-oracle/eclips-protocol-oracle.cabal"
        "    -- ./protocol-oracle/eclips-protocol-oracle.cabal",
    rejectionContaining "Oracle protocol moved before Raft protocol" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./protocol-raft/eclips-protocol-raft.cabal\n    ./protocol-oracle/eclips-protocol-oracle.cabal"
        "    ./protocol-oracle/eclips-protocol-oracle.cabal\n    ./protocol-raft/eclips-protocol-raft.cabal",
    rejection "application-api omitted from the default plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./application-api/eclips-application-api.cabal"
        "    -- ./application-api/eclips-application-api.cabal",
    rejectionContaining "Oracle runtime omitted from the default plan" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./oracle-runtime/eclips-oracle-runtime.cabal"
        "    -- ./oracle-runtime/eclips-oracle-runtime.cabal",
    rejectionContaining "Oracle runtime moved before the application facade" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./application-api/eclips-application-api.cabal\n    ./oracle-runtime/eclips-oracle-runtime.cabal"
        "    ./oracle-runtime/eclips-oracle-runtime.cabal\n    ./application-api/eclips-application-api.cabal",
    rejectionContaining "hello-world example omitted from the default plan" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./examples/hello-world/eclips-hello-world.cabal"
        "    -- ./examples/hello-world/eclips-hello-world.cabal",
    rejectionContaining "hello-context example omitted from the default plan" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./examples/hello-context/eclips-hello-context.cabal"
        "    -- ./examples/hello-context/eclips-hello-context.cabal",
    rejectionContaining "reviewed project packages reordered" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./public-types/eclips-public-types.cabal\n    ./application-types/eclips-application-types.cabal"
        "    ./application-types/eclips-application-types.cabal\n    ./public-types/eclips-public-types.cabal",
    rejectionContaining "reviewed project package duplicated" "default project must contain exactly the reviewed architecture and auxiliary package plan" $ \root ->
      replaceIn
        (root </> "cabal.project")
        "    ./examples/hello-world/eclips-hello-world.cabal"
        "    ./examples/hello-world/eclips-hello-world.cabal\n    ./examples/hello-world/eclips-hello-world.cabal",
    rejectionContaining "hello-world package renamed" "hello-world example auxiliary example package name differs" $ \root ->
      replaceIn
        (root </> "examples/hello-world/eclips-hello-world.cabal")
        "name:               eclips-hello-world"
        "name:               eclips-renamed-hello-world",
    rejectionContaining "unreviewed hello-world dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "examples/hello-world/eclips-hello-world.cabal")
        "base >=4.22 && <4.23,"
        "base >=4.22 && <4.23,\n      vector,",
    rejectionContaining "hello-world Oracle runtime dependency removed" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "examples/hello-world/eclips-hello-world.cabal")
        "      eclips-oracle-runtime >=0.1 && <0.2,"
        "      -- eclips-oracle-runtime >=0.1 && <0.2,",
    rejectionContaining "hello-world import of a private runtime owner" "hello-world example may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "examples/hello-world/Main.hs")
        "import Eclips.Herald.Runtime.Internal.Owner qualified as ForbiddenRuntimeOwner",
    rejectionContaining "Raft core package renamed" "pure Raft core architecture package name differs" $ \root ->
      replaceIn
        (root </> "raft-core/eclips-raft-core.cabal")
        "name:               eclips-raft-core"
        "name:               eclips-renamed-raft-core",
    rejectionContaining "unreviewed Raft core dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "raft-core/eclips-raft-core.cabal")
        "      bytestring >=0.12.2 && <0.13,"
        "      binary,\n      bytestring >=0.12.2 && <0.13,",
    rejectionContaining "unreviewed public Raft module" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "raft-core/eclips-raft-core.cabal")
        "      Eclips.Raft.Transition\n  other-modules:"
        "      Eclips.Raft.Transition\n      Eclips.Raft.Unreviewed\n  other-modules:",
    rejectionContaining "unreviewed private Raft module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "raft-core/eclips-raft-core.cabal")
        "      Eclips.Raft.Internal.State\n  hs-source-dirs:"
        "      Eclips.Raft.Internal.State\n      Eclips.Raft.Internal.Unreviewed\n  hs-source-dirs:",
    rejectionContaining "Oracle core package renamed" "pure Oracle core architecture package name differs" $ \root ->
      replaceIn
        (root </> "oracle-core/eclips-oracle-core.cabal")
        "name:               eclips-oracle-core"
        "name:               eclips-renamed-oracle-core",
    rejectionContaining "removed prospective Oracle library restored" "components differ from the reviewed package wall" $ \root ->
      appendTo
        (root </> "oracle-core/eclips-oracle-core.cabal")
        "\nlibrary step16-prospective\n  visibility: public\n  hs-source-dirs: test\n  exposed-modules: Eclips.Oracle.Step16.DisappearanceReference\n  build-depends: base\n  default-language: GHC2024\n",
    rejectionContaining "prospective Oracle dependency promoted into the live library" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-core/eclips-oracle-core.cabal")
        "      eclips-public-types >=0.1 && <0.2,\n      eclips-raft-core >=0.1 && <0.2"
        "      eclips-oracle-core:step16-prospective,\n      eclips-public-types >=0.1 && <0.2,\n      eclips-raft-core >=0.1 && <0.2",
    rejectionContaining "unreviewed Oracle core dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-core/eclips-oracle-core.cabal")
        "      cryptohash-sha256 >=0.11.102 && <0.12,"
        "      cryptohash-sha256 >=0.11.102 && <0.12,\n      network,",
    rejectionContaining "unreviewed public Oracle module" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-core/eclips-oracle-core.cabal")
        "      Eclips.Oracle.Transition\n  other-modules:"
        "      Eclips.Oracle.Transition\n      Eclips.Oracle.Unreviewed\n  other-modules:",
    rejectionContaining "unreviewed private Oracle module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-core/eclips-oracle-core.cabal")
        "      Eclips.Oracle.Internal.LabelCanonical\n  hs-source-dirs:"
        "      Eclips.Oracle.Internal.LabelCanonical\n      Eclips.Oracle.Internal.Unreviewed\n  hs-source-dirs:",
    rejectionContaining "unreviewed Oracle property module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-core/eclips-oracle-core.cabal")
        "      TransitionProperties\n"
        "      TransitionProperties\n      UnreviewedProperties\n",
    rejectionContaining "ordinary Domain payload imported by Oracle core" "pure Oracle core may import only its reviewed ECLIPS namespace roots" $ \root ->
      writeFixtureSource
        (root </> "oracle-core/src/Eclips/Oracle/DomainPayloadLeak.hs")
        "module Eclips.Oracle.DomainPayloadLeak where\nimport Eclips.Domain.Publication\n",
    rejectionContaining "test reference imported by the live Oracle library" "test-only disappearance reference must not enter a production unit" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Identity.hs")
        "import Eclips.Oracle.Step16.DisappearanceReference qualified as ForbiddenReference",
    rejectionContaining "test reference imported by live Herald composition" "test-only disappearance reference must not enter a production unit" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Startup/State.hs")
        "import Eclips.Oracle.Step16.DisappearanceReference qualified as ForbiddenReference",
    rejectionContaining "failure observer constructs disappearance Abort" "owner-only constructor function used outside its reviewed allowlist: AbortDisappearanceProbe" $ \root ->
      appendTo
        (root </> "herald-core/internal/Eclips/Herald/UseCase/FailureDetection.hs")
        "\nunauthorizedDisappearanceAbort = AbortDisappearanceProbe\n",
    rejectionContaining "runtime retry worker synthesizes disappearance Abort" "owner-only constructor function used outside its reviewed allowlist: DisappearanceAbortEnvelope" $ \root ->
      appendTo
        (root </> "herald-runtime/src/Eclips/Herald/Runtime/Ingress.hs")
        "\nunauthorizedDisappearanceAbort = DisappearanceAbortEnvelope\n",
    rejectionContaining "generic binary codec imported by Oracle core" "pure Oracle core must not import generic wire codecs, persistence, or higher-level components" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Canonical.hs")
        "import Data.Binary qualified as ForbiddenBinary",
    rejectionContaining "Herald owner imported by Oracle core" "pure Oracle core must not import generic wire codecs, persistence, or higher-level components" $ \root ->
      writeFixtureSource
        (root </> "oracle-core/src/Eclips/Oracle/HeraldLeak.hs")
        "module Eclips.Oracle.HeraldLeak where\nimport Eclips.Herald.Transition\n",
    rejectionContaining "Raft state imported by Oracle core" "pure Oracle core may import only its reviewed ECLIPS namespace roots" $ \root ->
      writeFixtureSource
        (root </> "oracle-core/src/Eclips/Oracle/RaftStateLeak.hs")
        "module Eclips.Oracle.RaftStateLeak where\nimport Eclips.Raft.State\n",
    rejectionContaining "Raft identity imported outside Oracle genesis" "only reviewed Oracle genesis and committed-configuration owners may import the checked Raft vocabulary" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Identity.hs")
        "import Eclips.Raft.Identity qualified as ForbiddenRaftIdentity",
    rejectionContaining "runtime import from the pure Oracle core" "pure Oracle core must not import runtime" $ \root ->
      writeFixtureSource
        (root </> "oracle-core/src/Eclips/Oracle/RuntimeLeak.hs")
        "module Eclips.Oracle.RuntimeLeak where\nimport Control.Concurrent.STM\n",
    rejectionContaining "future Oracle runtime imported by Oracle core" "pure Oracle core must not import runtime" $ \root ->
      writeFixtureSource
        (root </> "oracle-core/src/Eclips/Oracle/SelfRuntimeLeak.hs")
        "module Eclips.Oracle.SelfRuntimeLeak where\nimport Eclips.Oracle.Runtime\n",
    rejectionContaining "SHA-256 imported outside the Oracle digest owners" "Oracle-core cryptographic imports must remain in the reviewed digest owners" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Identity.hs")
        "import Crypto.Hash.SHA256 qualified as ForbiddenSHA256",
    rejectionContaining "unreviewed hash imported by an Oracle digest owner" "Oracle-core cryptographic imports must remain in the reviewed digest owners" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Internal/Digest.hs")
        "import Crypto.Hash.SHA512 qualified as ForbiddenSHA512",
    rejectionContaining "cereal imported outside Oracle canonical owners" "Oracle-core cereal imports must remain in the reviewed canonical and normalized-transcript owners" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Identity.hs")
        "import Data.Serialize qualified as ForbiddenSerialize",
    rejectionContaining "unreviewed cereal submodule imported by an Oracle canonical owner" "Oracle-core cereal imports must remain in the reviewed canonical and normalized-transcript owners" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Canonical.hs")
        "import Data.Serialize.Put qualified as ForbiddenSerializePut",
    rejectionContaining "Raft protocol package renamed" "Raft protocol architecture package name differs" $ \root ->
      replaceIn
        (root </> "protocol-raft/eclips-protocol-raft.cabal")
        "name:               eclips-protocol-raft"
        "name:               eclips-renamed-protocol-raft",
    rejectionContaining "unreviewed Raft protocol dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-raft/eclips-protocol-raft.cabal")
        "      bytestring >=0.12.2 && <0.13,"
        "      bytestring >=0.12.2 && <0.13,\n      network,",
    rejectionContaining "unreviewed public Raft protocol module" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-raft/eclips-protocol-raft.cabal")
        "      Eclips.Protocol.Raft.Frame\n  hs-source-dirs:"
        "      Eclips.Protocol.Raft.Frame\n      Eclips.Protocol.Raft.Unreviewed\n  hs-source-dirs:",
    rejectionContaining "unreviewed private Raft protocol module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-raft/eclips-protocol-raft.cabal")
        "      Eclips.Protocol.Raft.Frame\n  hs-source-dirs:"
        "      Eclips.Protocol.Raft.Frame\n  other-modules:\n      Eclips.Protocol.Raft.Internal.Unreviewed\n  hs-source-dirs:",
    rejectionContaining "unreviewed Raft protocol property module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-raft/eclips-protocol-raft.cabal")
        "      TypesProperties\n  hs-source-dirs:      test"
        "      TypesProperties\n      UnreviewedProperties\n  hs-source-dirs:      test",
    rejectionContaining "runtime import from the pure Raft protocol" "Raft protocol must not import runtime" $ \root ->
      writeFixtureSource
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/RuntimeLeak.hs")
        "module Eclips.Protocol.Raft.RuntimeLeak where\nimport Control.Concurrent.STM\n",
    rejectionContaining "Domain import from the Raft protocol" "pure Raft protocol must not import canonical codecs, cryptography, persistence, or higher-level components" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Types.hs")
        "import Eclips.Domain.Identity qualified as ForbiddenDomain",
    rejectionContaining "Oracle import from the Raft protocol" "pure Raft protocol must not import canonical codecs, cryptography, persistence, or higher-level components" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Types.hs")
        "import Eclips.Oracle.Identity qualified as ForbiddenOracle",
    rejectionContaining "higher protocol family imported by the Raft protocol" "pure Raft protocol must not import canonical codecs, cryptography, persistence, or higher-level components" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Types.hs")
        "import Eclips.Protocol.Application.Types qualified as ForbiddenApplicationProtocol",
    rejectionContaining "Raft state imported by the Raft protocol" "Raft protocol may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Types.hs")
        "import Eclips.Raft.State qualified as ForbiddenRaftState",
    rejectionContaining "Raft identity imported outside the DTO adapter" "only Raft-protocol DTO adapters may import the reviewed Raft-core configuration, identity and input vocabulary" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Codec.hs")
        "import Eclips.Raft.Identity qualified as ForbiddenRaftIdentity",
    rejectionContaining "Raft configuration imported outside the DTO adapter" "only Raft-protocol DTO adapters may import the reviewed Raft-core configuration, identity and input vocabulary" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Codec.hs")
        "import Eclips.Raft.Configuration qualified as ForbiddenRaftConfiguration",
    rejectionContaining "common framing imported outside the Raft frame owner" "only the Raft-protocol frame owner may import the common frame protocol" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Codec.hs")
        "import Eclips.Protocol.Frame qualified as ForbiddenCommonFrame",
    rejectionContaining "Binary imported outside Raft DTO and codec owners" "Raft-protocol Binary imports must remain in the reviewed DTO and codec owners" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Frame.hs")
        "import Data.Binary qualified as ForbiddenBinary",
    rejectionContaining "unreviewed Binary submodule imported by the Raft codec owner" "Raft-protocol Binary imports must remain in the reviewed DTO and codec owners" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Codec.hs")
        "import Data.Binary.Get qualified as ForbiddenBinaryGet",
    rejectionContaining "reverse Raft-protocol import from the pure Raft core" "pure Raft core may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "raft-core/src/Eclips/Raft/Identity.hs")
        "import Eclips.Protocol.Raft.Types qualified as ForbiddenRaftProtocol",
    rejectionContaining "reverse Raft-protocol import from the family-neutral framer" "family-neutral frame protocol may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "protocol-frame/src/Eclips/Protocol/Frame.hs")
        "import Eclips.Protocol.Raft.Types qualified as ForbiddenRaftProtocol",
    rejectionContaining "Oracle protocol package renamed" "Oracle protocol architecture package name differs" $ \root ->
      replaceIn
        (root </> "protocol-oracle/eclips-protocol-oracle.cabal")
        "name:               eclips-protocol-oracle"
        "name:               eclips-renamed-protocol-oracle",
    rejectionContaining "unreviewed Oracle protocol dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-oracle/eclips-protocol-oracle.cabal")
        "      bytestring >=0.12.2 && <0.13,"
        "      bytestring >=0.12.2 && <0.13,\n      network,",
    rejectionContaining "Raft protocol promoted from test-only into the Oracle protocol library" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-oracle/eclips-protocol-oracle.cabal")
        "      eclips-protocol-frame >=0.1 && <0.2,"
        "      eclips-protocol-frame >=0.1 && <0.2,\n      eclips-protocol-raft >=0.1 && <0.2,",
    rejectionContaining "unreviewed public Oracle protocol module" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-oracle/eclips-protocol-oracle.cabal")
        "      Eclips.Protocol.Oracle.Frame\n  hs-source-dirs:"
        "      Eclips.Protocol.Oracle.Frame\n      Eclips.Protocol.Oracle.Unreviewed\n  hs-source-dirs:",
    rejectionContaining "unreviewed private Oracle protocol module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-oracle/eclips-protocol-oracle.cabal")
        "      Eclips.Protocol.Oracle.Frame\n  hs-source-dirs:"
        "      Eclips.Protocol.Oracle.Frame\n  other-modules:\n      Eclips.Protocol.Oracle.Internal.Unreviewed\n  hs-source-dirs:",
    rejectionContaining "unreviewed Oracle protocol property module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "protocol-oracle/eclips-protocol-oracle.cabal")
        "      TypesProperties\n"
        "      TypesProperties\n      UnreviewedProperties\n",
    rejectionContaining "runtime import from the pure Oracle protocol" "Oracle protocol must not import runtime" $ \root ->
      writeFixtureSource
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/RuntimeLeak.hs")
        "module Eclips.Protocol.Oracle.RuntimeLeak where\nimport Control.Concurrent.STM\n",
    rejectionContaining "ordinary Domain payload imported by the Oracle protocol" "Oracle-protocol semantic/core imports must remain in the reviewed DTO and adapter owners" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs")
        "import Eclips.Domain.Publication qualified as ForbiddenPublication",
    rejectionContaining "Herald owner imported by the Oracle protocol" "pure Oracle protocol must not import cryptography, persistence, runtime owners, or unrelated protocol families" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs")
        "import Eclips.Herald.Transition qualified as ForbiddenHerald",
    rejectionContaining "Oracle state imported by the Oracle protocol" "Oracle-protocol semantic/core imports must remain in the reviewed DTO and adapter owners" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs")
        "import Eclips.Oracle.State qualified as ForbiddenOracleState",
    rejectionContaining "Raft state imported by the Oracle protocol" "Oracle-protocol semantic/core imports must remain in the reviewed DTO and adapter owners" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs")
        "import Eclips.Raft.State qualified as ForbiddenRaftState",
    rejectionContaining "unrelated application protocol imported by the Oracle protocol" "pure Oracle protocol must not import cryptography, persistence, runtime owners, or unrelated protocol families" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs")
        "import Eclips.Protocol.Application.Types qualified as ForbiddenApplicationProtocol",
    rejectionContaining "Raft wire protocol imported by the Oracle protocol" "pure Oracle protocol must not import cryptography, persistence, runtime owners, or unrelated protocol families" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs")
        "import Eclips.Protocol.Raft.Types qualified as ForbiddenRaftProtocol",
    rejectionContaining "semantic identity imported outside Oracle DTO and adapter owners" "Oracle-protocol semantic/core imports must remain in the reviewed DTO and adapter owners" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Frame.hs")
        "import Eclips.Domain.Identity qualified as ForbiddenDomainIdentity",
    rejectionContaining "canonical Oracle semantics imported outside Oracle DTO and adapter owners" "Oracle-protocol canonical semantic bytes must use Oracle.Canonical only in the reviewed DTO and adapter owners" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Frame.hs")
        "import Eclips.Oracle.Canonical qualified as ForbiddenCanonical",
    rejectionContaining "Binary imported outside Oracle DTO and codec owners" "Oracle-protocol Binary imports must remain in the reviewed DTO and codec owners" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Frame.hs")
        "import Data.Binary qualified as ForbiddenBinary",
    rejectionContaining "unreviewed Binary submodule imported by the Oracle codec owner" "Oracle-protocol Binary imports must remain in the reviewed DTO and codec owners" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Codec.hs")
        "import Data.Binary.Get qualified as ForbiddenBinaryGet",
    rejectionContaining "alternate cereal canonical encoding in the Oracle protocol" "Oracle-protocol wire DTOs must not define an alternate cereal canonical encoding" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Codec.hs")
        "import Data.Serialize qualified as ForbiddenSerialize",
    rejectionContaining "common framing imported outside the Oracle frame owner" "only the Oracle-protocol frame owner may import the common frame protocol" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Codec.hs")
        "import Eclips.Protocol.Frame qualified as ForbiddenCommonFrame",
    rejectionContaining "reverse internal Oracle protocol dependency" "Oracle-protocol internal imports may flow only from Frame to Codec/Types and from Codec to Types" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs")
        "import Eclips.Protocol.Oracle.Codec qualified as ForbiddenCodec",
    rejectionContaining "request-absence authority used outside Oracle Types and Frame" "Oracle request-absence authority vocabulary used outside its Types/Frame owners" $ \root ->
      writeFixtureSource
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/AuthorityLeak.hs")
        "module Eclips.Protocol.Oracle.AuthorityLeak where\nimport Eclips.Protocol.Oracle.Types\nforbiddenAuthority = noOracleRequestAbsenceAuthority\n",
    rejectionContaining "reverse Oracle-protocol import from Oracle core" "pure Oracle core may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "oracle-core/src/Eclips/Oracle/Identity.hs")
        "import Eclips.Protocol.Oracle.Types qualified as ForbiddenOracleProtocol",
    rejectionContaining "reverse Oracle-protocol import from the family-neutral framer" "family-neutral frame protocol may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "protocol-frame/src/Eclips/Protocol/Frame.hs")
        "import Eclips.Protocol.Oracle.Types qualified as ForbiddenOracleProtocol",
    rejectionContaining "reverse Oracle-protocol import from the Raft protocol" "pure Raft protocol must not import canonical codecs, cryptography, persistence, or higher-level components" $ \root ->
      insertImport
        (root </> "protocol-raft/src/Eclips/Protocol/Raft/Types.hs")
        "import Eclips.Protocol.Oracle.Types qualified as ForbiddenOracleProtocol",
    rejectionContaining "reverse Oracle import from the pure Raft core" "pure Raft core may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "raft-core/src/Eclips/Raft/Identity.hs")
        "import Eclips.Oracle.Identity qualified as ForbiddenOracle",
    rejectionContaining "Oracle runtime package renamed" "Oracle/Raft runtime architecture package name differs" $ \root ->
      replaceIn
        (root </> "oracle-runtime/eclips-oracle-runtime.cabal")
        "name:               eclips-oracle-runtime"
        "name:               eclips-renamed-oracle-runtime",
    rejectionContaining "unreviewed Oracle runtime dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-runtime/eclips-oracle-runtime.cabal")
        "      entropy >=0.4.1.11 && <0.5,"
        "      entropy >=0.4.1.11 && <0.5,\n      vector,",
    rejectionContaining "entropy promoted into deterministic Oracle runtime tests" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-runtime/eclips-oracle-runtime.cabal")
        "      eclips-raft-core >=0.1 && <0.2,\n      network >=3.2.8 && <3.3,"
        "      eclips-raft-core >=0.1 && <0.2,\n      entropy >=0.4.1.11 && <0.5,\n      network >=3.2.8 && <3.3,",
    rejectionContaining "unreviewed public Oracle runtime module" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-runtime/eclips-oracle-runtime.cabal")
        "      Eclips.Oracle.Runtime.ConformanceClient\n  other-modules:"
        "      Eclips.Oracle.Runtime.ConformanceClient\n      Eclips.Oracle.Runtime.Unreviewed\n  other-modules:",
    rejectionContaining "unreviewed private Oracle runtime module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-runtime/eclips-oracle-runtime.cabal")
        "      Eclips.Oracle.Runtime.Internal.TCP.Server\n  hs-source-dirs:      src internal"
        "      Eclips.Oracle.Runtime.Internal.TCP.Server\n      Eclips.Oracle.Runtime.Internal.Unreviewed\n  hs-source-dirs:      src internal",
    rejectionContaining "public Oracle socket implementation sublibrary" "visibility differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-runtime/eclips-oracle-runtime.cabal")
        "library socket-internal\n  import:             warnings\n  visibility:         private"
        "library socket-internal\n  import:             warnings\n  visibility:         public",
    rejectionContaining "unreviewed private Oracle socket module" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-runtime/eclips-oracle-runtime.cabal")
        "      Eclips.Oracle.Runtime.Internal.TCP.Socket\n  hs-source-dirs:      socket-internal"
        "      Eclips.Oracle.Runtime.Internal.TCP.Socket\n      Eclips.Oracle.Runtime.Internal.TCP.Unreviewed\n  hs-source-dirs:      socket-internal",
    rejectionContaining "unreviewed Oracle runtime property module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "oracle-runtime/eclips-oracle-runtime.cabal")
        "      WatchWorkerProperties\n"
        "      WatchWorkerProperties\n      UnreviewedProperties\n",
    rejectionContaining "reverse Oracle-runtime import from the Oracle protocol" "Oracle protocol must not import runtime" $ \root ->
      insertImport
        (root </> "protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs")
        "import Eclips.Oracle.Runtime qualified as ForbiddenRuntime",
    rejectionContaining "Herald imported by the Oracle runtime" "Oracle/Raft runtime must not import application, Herald, effect-library, persistence, compatibility, or unreviewed ambient-effect modules" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Herald.Transition qualified as ForbiddenHerald",
    rejectionContaining "ordinary Domain payload imported by the Oracle runtime" "Oracle/Raft runtime may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Domain.Publication qualified as ForbiddenPublication",
    rejectionContaining "effect library imported by the Oracle runtime" "Oracle/Raft runtime must not import application, Herald, effect-library, persistence, compatibility, or unreviewed ambient-effect modules" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Effectful qualified as ForbiddenEffectful",
    rejectionContaining "diagnostic clock imported outside the Oracle owner" "Oracle/Raft runtime must not import application, Herald, effect-library, persistence, compatibility, or unreviewed ambient-effect modules" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import GHC.Clock qualified as ForbiddenDiagnosticClock",
    rejectionContaining "diagnostic environment imported by pure Oracle counters" "Oracle/Raft runtime must not import application, Herald, effect-library, persistence, compatibility, or unreviewed ambient-effect modules" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/WorkCounts.hs")
        "import System.Environment qualified as ForbiddenDiagnosticEnvironment",
    rejectionContaining "unreviewed ambient module imported by the Oracle owner" "Oracle/Raft runtime must not import application, Herald, effect-library, persistence, compatibility, or unreviewed ambient-effect modules" $ \root ->
      insertImport
        (root </> "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs")
        "import System.Process qualified as ForbiddenDiagnosticProcess",
    rejectionContaining "entropy acquisition outside the Oracle runtime entropy owner" "Oracle/Raft runtime entropy acquisition must remain in the private election-timeout source" $ \root ->
      insertImport
        (root </> "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/Dispatcher.hs")
        "import System.Entropy (getEntropy)",
    rejectionContaining "socket import outside the Oracle runtime socket owners" "Oracle/Raft socket imports must remain in the reviewed socket owners" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Network.Socket qualified as ForbiddenSocket",
    rejectionContaining "status-projection seam imported outside the serialized owners" "the Oracle/Raft runtime status projection may be imported only by its serialized owners" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Oracle.Runtime.Internal.StatusProjection qualified as ForbiddenStatusProjection",
    rejectionContaining "provisional replica-registration seam imported outside its transport owner" "the provisional replica-registration comparison may be imported only by the Raft transport binding owner" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Oracle.Runtime.Internal.ReplicaRegistration qualified as ForbiddenReplicaRegistration",
    rejectionContaining "socket-retirement seam imported outside its stream owners" "the Oracle/Raft socket-retirement seam may be imported only by its reviewed stream owners" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Oracle.Runtime.Internal.TCP.Retirement qualified as ForbiddenRetirement",
    rejectionContaining "private socket sublibrary imported outside its TCP consumers" "the private Oracle/Raft socket sublibrary may be imported only by its reviewed TCP consumers" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Oracle.Runtime.Internal.TCP.Socket qualified as ForbiddenSocketSublibrary",
    rejectionContaining "Oracle codec imported outside the Oracle runtime TCP boundary" "Oracle wire protocol modules may be imported only by the Oracle TCP worker and authorized conformance client" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Protocol.Oracle.Codec qualified as ForbiddenCodec",
    rejectionContaining "Raft framing imported outside the Oracle runtime TCP boundary" "Raft wire protocol modules may be imported only by the Raft TCP worker" $ \root ->
      insertImport
        (root </> "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/Dispatcher.hs")
        "import Eclips.Protocol.Raft.Frame qualified as ForbiddenFrame",
    rejectionContaining "second Oracle transition caller in the Oracle runtime" "only the serialized Oracle owner and shared fault vocabulary may import the Oracle transition kernel" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Oracle.Transition (stepOracle)",
    rejectionContaining "second Raft transition caller in the Oracle runtime" "only the serialized Raft owner and shared fault vocabulary may import the Raft transition kernel" $ \root ->
      insertImport
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Adapter.hs")
        "import Eclips.Raft.Transition (stepRaft)",
    rejectionContaining "Oracle transition authority used through shared fault vocabulary" "kernel state/transition authority used outside its serialized runtime owner" $ \root ->
      appendTo
        (root </> "oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Types.hs")
        "\nforbiddenOwnerCall = stepOracle\n",
    rejectionContaining "raw Raft state used by an Oracle runtime TCP worker" "kernel state/transition authority used outside its serialized runtime owner" $ \root -> do
      let path = root </> "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Raft.hs"
      importRestoration <-
        insertImport
          path
          "import Eclips.Raft.State (RaftState)"
      appendRestoration <-
        appendTo
          path
          "\nforbiddenRawState :: Maybe (RaftState ByteString)\nforbiddenRawState = Nothing\n"
          `onException` runFixtureRestoration importRestoration
      pure
        $ FixtureRestoration
        $ runFixtureRestoration appendRestoration
          `finally` runFixtureRestoration importRestoration,
    rejectionContaining "capacity policy identifier in the Oracle runtime" "deferred runtime policy/scaffolding identifier is not part of profile 0.1" $ \root ->
      writeFixtureSource
        (root </> "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/CapacityLeak.hs")
        "module Eclips.Oracle.Runtime.Internal.CapacityLeak where\nimport Control.Concurrent.STM (TBQueue)\n",
    rejectionContaining "protocol version identifier in the Oracle runtime" "deferred runtime policy/scaffolding identifier is not part of profile 0.1" $ \root ->
      writeFixtureSource
        (root </> "oracle-runtime/src/Eclips/Oracle/Runtime/Internal/VersionLeak.hs")
        "module Eclips.Oracle.Runtime.Internal.VersionLeak where\ndata ProtocolVersion = ProtocolVersion\n",
    rejection "unreviewed production source root" $ \root ->
      replaceIn
        (root </> "public-types/eclips-public-types.cabal")
        "hs-source-dirs:      src"
        "hs-source-dirs:      src, internal",
    rejection "multiline unreviewed production source root" $ \root ->
      replaceIn
        (root </> "public-types/eclips-public-types.cabal")
        "hs-source-dirs:      src"
        "hs-source-dirs:      src\n      ../unexpected/src",
    rejection "unreviewed application dependency" $ \root ->
      replaceIn
        (root </> "application-types/eclips-application-types.cabal")
        "base >=4.22 && <4.23,"
        "base >=4.22 && <4.23,\n      network,",
    rejection "unreviewed application-facing module" $ \root ->
      replaceIn
        (root </> "application-types/eclips-application-types.cabal")
        "      Eclips.Application.Types.Write\n  hs-source-dirs:      src"
        "      Eclips.Application.Types.Write\n      Eclips.Application.Types.Unreviewed\n  hs-source-dirs:      src",
    rejection "unreviewed application-protocol dependency" $ \root ->
      replaceIn
        (root </> "protocol-application/eclips-protocol-application.cabal")
        "base >=4.22 && <4.23,"
        "base >=4.22 && <4.23,\n      network,",
    rejection "unreviewed application-protocol module" $ \root ->
      replaceIn
        (root </> "protocol-application/eclips-protocol-application.cabal")
        "      Eclips.Protocol.Application.Types\n  hs-source-dirs:      src"
        "      Eclips.Protocol.Application.Types\n      Eclips.Protocol.Application.Unreviewed\n  hs-source-dirs:      src",
    rejection "unreviewed common-frame dependency" $ \root ->
      replaceIn
        (root </> "protocol-frame/eclips-protocol-frame.cabal")
        "base >=4.22 && <4.23,"
        "base >=4.22 && <4.23,\n      network,",
    rejection "unreviewed common-frame module" $ \root ->
      replaceIn
        (root </> "protocol-frame/eclips-protocol-frame.cabal")
        "exposed-modules:    Eclips.Protocol.Frame"
        "exposed-modules:    Eclips.Protocol.Frame, Eclips.Protocol.Frame.Unreviewed",
    rejection "unreviewed peer-protocol dependency" $ \root ->
      replaceIn
        (root </> "protocol-peer/eclips-protocol-peer.cabal")
        "base >=4.22 && <4.23,"
        "base >=4.22 && <4.23,\n      eclips-domain,",
    rejection "unreviewed peer-protocol module" $ \root ->
      replaceIn
        (root </> "protocol-peer/eclips-protocol-peer.cabal")
        "      Eclips.Protocol.Peer.Types\n  hs-source-dirs:      src"
        "      Eclips.Protocol.Peer.Types\n      Eclips.Protocol.Peer.Unreviewed\n  hs-source-dirs:      src",
    rejection "unreviewed application-client dependency" $ \root ->
      replaceIn
        (root </> "application-client/eclips-application-client.cabal")
        "base >=4.22 && <4.23,"
        "base >=4.22 && <4.23,\n      network,",
    rejection "application-client representation exposed" $ \root ->
      replaceIn
        (root </> "application-client/eclips-application-client.cabal")
        "      Eclips.Application.Client.Recovery\n  other-modules:"
        "      Eclips.Application.Client.Recovery\n      Eclips.Application.Client.Internal\n  other-modules:",
    rejectionContaining "application recovery representation exposed" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "application-client/eclips-application-client.cabal")
        "      Eclips.Application.Client.Internal\n      Eclips.Application.Client.Recovery.Internal"
        "      Eclips.Application.Client.Internal",
    rejectionContaining "application recovery constructors imported outside their owner" "application recovery constructors may be imported only by their public facade and state owner" $ \root ->
      insertImport
        (root </> "application-client/src/Eclips/Application/Client/Effect/Internal.hs")
        "import Eclips.Application.Client.Recovery.Internal qualified as ForbiddenRecoveryConstruction",
    rejection "unreviewed application-api dependency" $ \root ->
      replaceIn
        (root </> "application-api/eclips-application-api.cabal")
        "      bytestring >=0.12.2 && <0.13,\n      containers >=0.8 && <0.9,"
        "      bytestring >=0.12.2 && <0.13,\n      containers >=0.8 && <0.9,\n      eclips-herald-core,",
    rejectionContaining "public application physical sublibrary" "visibility differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "application-api/eclips-application-api.cabal")
        "library physical-internal\n  import:             warnings\n  visibility:         private"
        "library physical-internal\n  import:             warnings\n  visibility:         public",
    rejectionContaining "unreviewed application physical dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "application-api/eclips-application-api.cabal")
        "library physical-internal\n  import:             warnings\n  visibility:         private\n  exposed-modules:\n      Eclips.Application.Runtime.Internal.Physical\n  hs-source-dirs:      internal\n  build-depends:\n      base >=4.22 && <4.23"
        "library physical-internal\n  import:             warnings\n  visibility:         private\n  exposed-modules:\n      Eclips.Application.Runtime.Internal.Physical\n  hs-source-dirs:      internal\n  build-depends:\n      base >=4.22 && <4.23,\n      network",
    rejectionContaining "unreviewed application physical module" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "application-api/eclips-application-api.cabal")
        "      Eclips.Application.Runtime.Internal.Physical\n  hs-source-dirs:      internal"
        "      Eclips.Application.Runtime.Internal.Physical\n      Eclips.Application.Runtime.Internal.UnreviewedPhysical\n  hs-source-dirs:      internal",
    rejectionContaining "runtime import in the application physical state machine" "the private application physical state machine must remain pure and runtime-free" $ \root ->
      insertImport
        (root </> "application-api/internal/Eclips/Application/Runtime/Internal/Physical.hs")
        "import Control.Concurrent.STM qualified as ForbiddenRuntime",
    rejection "application-api implementation exposed" $ \root ->
      replaceIn
        (root </> "application-api/eclips-application-api.cabal")
        "      Eclips.Application.Typed.Advanced\n  other-modules:"
        "      Eclips.Application.Typed.Advanced\n      Eclips.Application.Runtime.Internal.Owner\n  other-modules:",
    rejectionContaining "unreviewed private application runtime module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "application-api/eclips-application-api.cabal")
        "      Eclips.Application.Runtime.Internal.Types\n  hs-source-dirs:      src"
        "      Eclips.Application.Runtime.Internal.Types\n      Eclips.Application.Runtime.Internal.Unreviewed\n  hs-source-dirs:      src",
    rejection "unreviewed semantic-domain module" $ \root ->
      replaceIn
        (root </> "domain/eclips-domain.cabal")
        "      Eclips.Domain.Value\n  hs-source-dirs:      src"
        "      Eclips.Domain.Value\n      Eclips.Domain.Unreviewed\n  hs-source-dirs:      src",
    rejection "dependency inherited from a common stanza" $ \root ->
      replaceIn
        (root </> "public-types/eclips-public-types.cabal")
        "common warnings\n"
        "common warnings\n  build-depends: vector\n",
    rejection "unreviewed executable build tool" $ \root ->
      replaceIn
        (root </> "public-types/eclips-public-types.cabal")
        "  default-language:    GHC2024"
        "  build-tool-depends:  alex:alex\n  default-language:    GHC2024",
    rejection "process gate missing its Cabal-built node" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "  build-tool-depends:\n      eclips-herald-runtime:eclips-herald-slice1-node\n"
        "",
    rejection "process gate uses an unreviewed executable" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "eclips-herald-runtime:eclips-herald-slice1-node"
        "alex:alex",
    rejection "legacy component language edition" $ \root ->
      replaceIn
        (root </> "public-types/eclips-public-types.cabal")
        "default-language:    GHC2024"
        "default-language:    GHC2021",
    rejection "public Herald implementation sublibrary" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "visibility:         private"
        "visibility:         public",
    rejection "unreviewed private Herald exposed module" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "      Eclips.Herald.Wait.State\n  hs-source-dirs:      src internal"
        "      Eclips.Herald.Wait.State\n      Eclips.Herald.Unreviewed\n  hs-source-dirs:      src internal",
    rejectionContaining "Oracle core dependency removed from Herald kernel" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "      eclips-oracle-core >=0.1 && <0.2,"
        "      -- eclips-oracle-core >=0.1 && <0.2,",
    rejectionContaining "Oracle runtime promoted into the production Herald kernel" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "      eclips-oracle-core >=0.1 && <0.2,"
        "      eclips-oracle-core >=0.1 && <0.2,\n      eclips-oracle-runtime >=0.1 && <0.2,",
    rejection "unreviewed public Herald re-export" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "      Eclips.Herald.Timer\n  build-depends:\n      eclips-herald-core:kernel-internal"
        "      Eclips.Herald.Timer,\n      Eclips.Herald.Bootstrap\n  build-depends:\n      eclips-herald-core:kernel-internal",
    rejectionContaining "Oracle-client facade removed from public Herald core" "library re-exported modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "      Eclips.Herald.OracleClient,"
        "      -- Eclips.Herald.OracleClient,",
    rejection "public PeerStream re-export" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "      Eclips.Herald.Timer\n  build-depends:\n      eclips-herald-core:kernel-internal"
        "      Eclips.Herald.Timer,\n      Eclips.Herald.PeerStream\n  build-depends:\n      eclips-herald-core:kernel-internal",
    rejectionContaining "peer-liveness owner removed from the Herald kernel" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "      Eclips.Herald.PeerLiveness.State\n"
        "      -- Eclips.Herald.PeerLiveness.State\n",
    rejectionContaining "timer facade removed from public Herald core" "library re-exported modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-core/eclips-herald-core.cabal")
        "      Eclips.Herald.Timer\n  build-depends:"
        "      -- Eclips.Herald.Timer\n  build-depends:",
    rejection "unreviewed Herald runtime dependency" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "base >=4.22 && <4.23,"
        "base >=4.22 && <4.23,\n      network,",
    rejectionContaining "Oracle core dependency removed from the typed Herald runtime" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      eclips-oracle-core >=0.1 && <0.2,"
        "      -- eclips-oracle-core >=0.1 && <0.2,",
    rejectionContaining "Oracle protocol dependency removed from the typed Herald runtime" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      eclips-protocol-oracle >=0.1 && <0.2,"
        "      -- eclips-protocol-oracle >=0.1 && <0.2,",
    rejectionContaining "Oracle runtime promoted into the production Herald runtime" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      eclips-protocol-oracle >=0.1 && <0.2,"
        "      eclips-oracle-runtime >=0.1 && <0.2,\n      eclips-protocol-oracle >=0.1 && <0.2,",
    rejectionContaining "named Step-12 Herald control component omitted" "components differ from the reviewed package wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "test-suite herald-step12-control-properties"
        "test-suite herald-step12-control-properties-renamed",
    rejectionContaining "Step-12 Herald control source root omitted" "source directories differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "  hs-source-dirs:      test-step12 test"
        "  hs-source-dirs:      test",
    rejectionContaining "unreviewed Step-12 Herald control dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      eclips-oracle-runtime >=0.1 && <0.2,"
        "      eclips-oracle-runtime >=0.1 && <0.2,\n      text,",
    rejectionContaining "unreviewed Step-12 Herald control property module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      Step12Fixtures\n  hs-source-dirs:      test-step12 test"
        "      Step12Fixtures\n      UnreviewedStep12Properties\n  hs-source-dirs:      test-step12 test",
    rejectionContaining "Step-12 Oracle submission tracker omitted" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      OracleSubmissionTrackerProperties\n"
        "",
    rejectionContaining "Step-12 QuickCheck dependency omitted" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      tasty-hunit >=0.10 && <0.11,\n      tasty-quickcheck >=0.11 && <0.12\n  default-language:    GHC2024\n\ntest-suite herald-step14-label-properties"
        "      tasty-hunit >=0.10 && <0.11\n  default-language:    GHC2024\n\ntest-suite herald-step14-label-properties",
    rejectionContaining "named Step-14 Herald label component omitted" "components differ from the reviewed package wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "test-suite herald-step14-label-properties"
        "test-suite herald-step14-label-properties-renamed",
    rejectionContaining "Step-14 Herald label source root omitted" "source directories differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "  hs-source-dirs:      test-step14 test-step12"
        "  hs-source-dirs:      test-step14",
    rejectionContaining "unreviewed Step-14 Herald label dependency" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      eclips-protocol-admin >=0.1 && <0.2,\n      eclips-protocol-application >=0.1 && <0.2,"
        "      eclips-protocol-admin >=0.1 && <0.2,\n      eclips-protocol-application >=0.1 && <0.2,\n      vector,",
    rejectionContaining "Step-14 peer protocol dependency omitted" "dependencies differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      eclips-protocol-application >=0.1 && <0.2,\n      eclips-protocol-peer >=0.1 && <0.2,\n      eclips-raft-core >=0.1 && <0.2,"
        "      eclips-protocol-application >=0.1 && <0.2,\n      eclips-raft-core >=0.1 && <0.2,",
    rejectionContaining "unreviewed Step-14 Herald label property module" "other modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      Step14TargetEndProperties\n  hs-source-dirs:      test-step14 test-step12"
        "      Step14TargetEndProperties\n      UnreviewedStep14Properties\n  hs-source-dirs:      test-step14 test-step12",
    rejectionContaining "named Step-15 Herald crash component omitted" "components differ from the reviewed package wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "test-suite herald-step15-crash-properties"
        "test-suite herald-step15-crash-properties-renamed",
    rejection "public Herald runtime implementation sublibrary" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "visibility:         private"
        "visibility:         public",
    rejection "public Herald TCP implementation sublibrary" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "library tcp-internal\n  import:             warnings\n  visibility:         private"
        "library tcp-internal\n  import:             warnings\n  visibility:         public",
    rejection "unreviewed private Herald runtime module" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      Eclips.Herald.Runtime.Internal.Types\n  hs-source-dirs:      src internal"
        "      Eclips.Herald.Runtime.Internal.Types\n      Eclips.Herald.Runtime.Internal.Unreviewed\n  hs-source-dirs:      src internal",
    rejectionContaining "runtime timer scheduler removed from the reviewed wall" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      Eclips.Herald.Runtime.Internal.Timer\n"
        "      -- Eclips.Herald.Runtime.Internal.Timer\n",
    rejectionContaining "Oracle adapter removed from the public Herald runtime facade" "library re-exported modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      Eclips.Herald.Runtime.Ingress,\n      Eclips.Herald.Runtime.Oracle,\n      Eclips.Herald.Runtime.Trace\n  build-depends:"
        "      Eclips.Herald.Runtime.Ingress,\n      Eclips.Herald.Runtime.Trace\n  build-depends:",
    rejectionContaining "Oracle worker removed from private Herald TCP" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      Eclips.Herald.Runtime.TCP.Internal.Oracle\n"
        "      -- Eclips.Herald.Runtime.TCP.Internal.Oracle\n",
    rejectionContaining "heartbeat supervisor removed from private Herald TCP" "exposed modules differ from the reviewed wall" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      Eclips.Herald.Runtime.TCP.Internal.Heartbeat\n"
        "      -- Eclips.Herald.Runtime.TCP.Internal.Heartbeat\n",
    rejection "exact runtime trace publicly re-exported" $ \root ->
      replaceIn
        (root </> "herald-runtime/eclips-herald-runtime.cabal")
        "      Eclips.Herald.Runtime.Trace\n  build-depends:\n      eclips-herald-runtime:runtime-internal"
        "      Eclips.Herald.Runtime.Trace,\n      Eclips.Herald.Runtime.Internal.Trace\n  build-depends:\n      eclips-herald-runtime:runtime-internal",
    rejection "second Herald transition caller in the runtime" $ \root ->
      insertImport
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Connection.hs")
        "import Eclips.Herald.Transition (stepHerald)",
    rejectionContaining "runtime timer scheduler imported outside the owner" "the runtime timer scheduler may be imported only by the serialized runtime owner" $ \root ->
      insertImport
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Connection.hs")
        "import Eclips.Herald.Runtime.Internal.Timer qualified as ForbiddenRuntimeTimer",
    rejectionContaining "peer-recovery vocabulary imported by an unrelated runtime seam" "peer-recovery vocabulary may be imported only by the reviewed runtime configuration, owner, conformance, recording, and timer seams" $ \root ->
      insertImport
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Connection.hs")
        "import Eclips.Herald.PeerLiveness qualified as ForbiddenPeerLiveness",
    rejectionContaining "logical timer vocabulary imported by an unrelated runtime seam" "logical timer vocabulary may be imported only by the reviewed runtime owner, scheduler, and trace seams" $ \root ->
      insertImport
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Connection.hs")
        "import Eclips.Herald.Timer qualified as ForbiddenTimer",
    rejectionContaining "heartbeat supervisor imported outside a connection owner" "the TCP heartbeat supervisor may be imported only by the application and peer connection owners" $ \root ->
      insertImport
        (root </> "herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs")
        "import Eclips.Herald.Runtime.TCP.Internal.Heartbeat qualified as ForbiddenHeartbeat",
    rejection "AES primitive imported outside the private ID-generator state" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Crypto.Cipher.AES (AES256)",
    rejection "unreviewed crypto module imported by the private ID-generator state" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/IdGenerator/State.hs")
        "import Crypto.Cipher.ChaCha qualified as ForbiddenCipher",
    rejection "exact generator key bytes imported outside the reviewed generator modules" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Startup/State.hs")
        "import Eclips.Herald.IdGenerator.Internal (generatorSeedBytes)",
    rejectionContaining "peer-recovery constructors imported outside their owner" "owner-only Herald correlation constructors may be imported only by their facade, owner, bootstrap, or invariant" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.PeerLiveness.Internal qualified as ForbiddenPeerRecoveryConstruction",
    rejectionContaining "timer constructors imported outside their owner" "owner-only Herald correlation constructors may be imported only by their facade, owner, bootstrap, or invariant" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.Timer.Internal qualified as ForbiddenTimerConstruction",
    rejectionContaining "peer-liveness seam imported by an unrelated owner" "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.PeerLiveness qualified as ForbiddenPeerLiveness",
    rejectionContaining "timer seam imported by an unrelated owner" "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.Timer qualified as ForbiddenTimer",
    rejection "entropy acquisition outside the runtime generator-seed boundary" $ \root ->
      insertImport
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Owner.hs")
        "import System.Entropy (getEntropy)",
    rejection "generator-seed implementation imported by an unrelated public runtime facade" $ \root ->
      insertImport
        (root </> "herald-runtime/src/Eclips/Herald/Runtime/Handler.hs")
        "import Eclips.Herald.Runtime.Internal.GeneratorSeed qualified as ForbiddenGeneratorSeed",
    rejection "leaf-to-leaf Herald state import" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.Graph.State qualified as ForbiddenGraph",
    rejectionContaining "peer-delivery owner imported by unrelated leaf" "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.PeerDelivery qualified as ForbiddenPeerDelivery",
    rejectionContaining "peer-delivery bookkeeping imported by unrelated leaf" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.PeerDelivery.State qualified as ForbiddenPeerDeliveryState",
    rejectionContaining "peer-delivery coordinator imported outside reviewed consumers" "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Bootstrap.hs")
        "import Eclips.Herald.UseCase.PeerDelivery qualified as ForbiddenPeerDeliveryCoordinator",
    rejectionContaining "peer-delivery owner imports unrelated store owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/PeerDelivery.hs")
        "import Eclips.Herald.Store.State qualified as ForbiddenStore",
    rejectionContaining "peer-delivery coordinator imports unrelated graph owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/PeerDelivery.hs")
        "import Eclips.Herald.Graph.State qualified as ForbiddenGraph",
    rejectionContaining "peer-liveness owner imported by another Herald leaf" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.PeerLiveness.State qualified as ForbiddenPeerLivenessState",
    rejectionContaining "Graph progress imported outside its reviewed Transition seam" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/src/Eclips/Herald/OracleProjection.hs")
        "import Eclips.Herald.Graph.Progress qualified as ForbiddenGraphProgress",
    rejection "Application.State import of an unrelated Herald leaf" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Application/State.hs")
        "import Eclips.Herald.Graph.State qualified as ForbiddenGraph",
    rejection "alignment-loss owner import of the Store owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Alignment/Loss.hs")
        "import Eclips.Herald.Store.State qualified as ForbiddenStore",
    rejection "label-barrier leaf import of the Application owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/LabelBarrier/State.hs")
        "import Eclips.Herald.Application.State qualified as ForbiddenApplication",
    rejectionContaining "label collector imported by an unrelated leaf" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.Label.Collection qualified as ForbiddenLabelCollection",
    rejectionContaining "label collector leaf imports Oracle client state" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Label/Collection.hs")
        "import Eclips.Herald.OracleClient.State qualified as ForbiddenOracleClient",
    rejectionContaining "label collection coordinator imported outside reviewed consumers" "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Bootstrap.hs")
        "import Eclips.Herald.UseCase.LabelCollection qualified as ForbiddenLabelCollectionCoordinator",
    rejectionContaining "label collection coordinator imports unrelated Store owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/LabelCollection.hs")
        "import Eclips.Herald.Store.State qualified as ForbiddenStore",
    rejection "visibility leaf import of the PeerStream owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Visibility/State.hs")
        "import Eclips.Herald.PeerStream.State qualified as ForbiddenPeerStream",
    rejection "effective Store observation import of the Graph owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Store/Observation.hs")
        "import Eclips.Herald.Graph.State qualified as ForbiddenGraph",
    rejectionContaining "authority resolver imported by an unrelated owner" "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Store/State.hs")
        "import Eclips.Herald.Authority qualified as ForbiddenAuthority",
    rejectionContaining "authority resolver import of an unrelated owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Authority.hs")
        "import Eclips.Herald.Application.State qualified as ForbiddenApplication",
    rejectionContaining "label-collection coordinator import of an unrelated owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/LabelCollection.hs")
        "import Eclips.Herald.Store.State qualified as ForbiddenStore",
    rejection "unrelated live Store owner imports the effective-publication coordinator seam" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Store/State.hs")
        "import Eclips.Herald.EffectivePublication qualified as ForbiddenEffectivePublication",
    rejection "Application.State import of the administration leaf" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Application/State.hs")
        "import Eclips.Herald.Administration.State qualified as ForbiddenAdministration",
    rejection "Administration.State import of another Herald leaf" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Administration/State.hs")
        "import Eclips.Herald.Graph.State qualified as ForbiddenGraph",
    rejection "Application.State import of administration constructors" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Application/State.hs")
        "import Eclips.Herald.Administration.Internal qualified as ForbiddenAdministration",
    rejection "Administration.State import of session constructors" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Administration/State.hs")
        "import Eclips.Herald.Application.Session.Internal qualified as ForbiddenSession",
    rejection "Transition import of an unrelated Herald leaf" $ \root ->
      insertImport
        (root </> "herald-core/src/Eclips/Herald/Transition.hs")
        "import Eclips.Herald.Graph.State qualified as ForbiddenGraph",
    rejection "Application.State import of the publication owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Application/State.hs")
        "import Eclips.Herald.Publication.State qualified as ForbiddenPublication",
    rejection "ApplicationCall import of an unrelated Herald leaf" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/ApplicationCall.hs")
        "import Eclips.Herald.Administration.State qualified as ForbiddenAdministration",
    rejection "Transition import of the newenv coordinator" $ \root ->
      insertImport
        (root </> "herald-core/src/Eclips/Herald/Transition.hs")
        "import Eclips.Herald.UseCase.NewEnvironment qualified as ForbiddenNewEnvironment",
    rejection "PeerStream.State import of the publication owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/PeerStream/State.hs")
        "import Eclips.Herald.Publication.State qualified as ForbiddenPublication",
    rejection "PeerStream.State import of the composed effect vocabulary" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/PeerStream/State.hs")
        "import Eclips.Herald.EffectBatch qualified as ForbiddenEffects",
    rejection "PeerStream.State import of Discovery" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/PeerStream/State.hs")
        "import Eclips.Herald.Discovery.State qualified as ForbiddenDiscovery",
    rejection "PeerStream.State import of Placement" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/PeerStream/State.hs")
        "import Eclips.Herald.Placement.State qualified as ForbiddenPlacement",
    rejection "PeerStream.State import of Store" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/PeerStream/State.hs")
        "import Eclips.Herald.Store.State qualified as ForbiddenStore",
    rejection "PeerStream.State import of a generic codec" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/PeerStream/State.hs")
        "import Data.Serialize qualified as ForbiddenCodec",
    rejection "premature PeerStream vocabulary import by the application coordinator" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/ApplicationCall.hs")
        "import Eclips.Herald.PeerStream qualified as ForbiddenPeerStream",
    rejection "Discovery owner import of Placement" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Discovery/State.hs")
        "import Eclips.Herald.Placement.State qualified as ForbiddenPlacement",
    rejection "Placement owner import of Discovery" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Placement/State.hs")
        "import Eclips.Herald.Discovery.State qualified as ForbiddenDiscovery",
    rejection "Publication owner import of Placement" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Publication/State.hs")
        "import Eclips.Herald.Placement.State qualified as ForbiddenPlacement",
    rejection "PeerPublication adapter import of a state owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/PeerPublication.hs")
        "import Eclips.Herald.Publication.State qualified as ForbiddenPublication",
    rejection "unreviewed leaf import of PeerPublication" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Wait/State.hs")
        "import Eclips.Herald.PeerPublication qualified as ForbiddenPeerPublication",
    rejection "unreviewed leaf import of structural debt preparation" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Wait/State.hs")
        "import Eclips.Herald.Structural.Debt qualified as ForbiddenStructuralDebt",
    rejection "unreviewed leaf import of structural reconciliation" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Wait/State.hs")
        "import Eclips.Herald.Structural.Reconciliation qualified as ForbiddenStructuralReconciliation",
    rejection "unreviewed leaf import of the public PeerDispatch facade" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Store/State.hs")
        "import Eclips.Herald.PeerDispatch qualified as ForbiddenPeerDispatch",
    rejection "unreviewed coordinator import of Discovery.Internal" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/ApplicationCall.hs")
        "import Eclips.Herald.Discovery.Internal qualified as ForbiddenDiscoveryMinting",
    rejection "Herald leaf import of the application protocol" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Application/State.hs")
        "import Eclips.Protocol.Application.Types qualified as ForbiddenProtocol",
    rejection "Herald RPC facade import of application framing" $ \root ->
      insertImport
        (root </> "herald-core/src/Eclips/Herald/Application/RPC.hs")
        "import Eclips.Protocol.Application.Frame qualified as ForbiddenFrame",
    rejection "application claim reconstruction outside the RPC adapter" $ \root ->
      appendTo
        (root </> "herald-core/internal/Eclips/Herald/Application/State.hs")
        "\nforbiddenClaimReconstruction = Request.applicationReplyCursorFromClaimWord64\n",
    rejection "RPC adapter trying to allocate a session" $ \root ->
      appendTo
        (root </> "herald-core/internal/Eclips/Herald/Application/RPC/Internal.hs")
        "\nforbiddenSessionAllocation = Session.initialSessionAllocation\n",
    rejection "authorised Discovery facade trying to mint a generation" $ \root ->
      insertImport
        (root </> "herald-core/src/Eclips/Herald/Discovery.hs")
        "import Eclips.Herald.Discovery.Internal (firstPeerBindingGeneration)",
    rejection "authorised peer coordinator trying to mint a dispatch ticket" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/PeerControl.hs")
        "import Eclips.Herald.PeerStream (peerDispatchTicket)",
    rejection "application coordinator import of PeerInput" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/ApplicationCall.hs")
        "import Eclips.Herald.UseCase.PeerInput qualified as ForbiddenPeerInput",
    rejection "bootstrap import of PeerControl" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Bootstrap.hs")
        "import Eclips.Herald.UseCase.PeerControl qualified as ForbiddenPeerControl",
    rejection "leaf import of PeerInput coordinator" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Store/State.hs")
        "import Eclips.Herald.UseCase.PeerInput qualified as ForbiddenPeerInput",
    rejection "PeerControl direct import of Store owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/PeerControl.hs")
        "import Eclips.Herald.Store.State qualified as ForbiddenStore",
    rejection "Wait.State import of another Herald leaf" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Wait/State.hs")
        "import Eclips.Herald.Application.State qualified as ForbiddenApplication",
    rejection "Bootstrap import of the wait owner" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Bootstrap.hs")
        "import Eclips.Herald.Wait.State qualified as ForbiddenWait",
    rejection "unreviewed request-owner constructor import" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Application/SortDefinition.hs")
        "import Eclips.Herald.Application.Request.Internal qualified as ForbiddenRequest",
    rejection "unreviewed resolved-query vocabulary import" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Administration/State.hs")
        "import Eclips.Herald.Query qualified as ForbiddenQuery",
    rejection "unreviewed application-call coordinator import" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Bootstrap.hs")
        "import Eclips.Herald.UseCase.ApplicationCall qualified as ForbiddenApplicationCall",
    rejection "package-qualified global-domain import" $ \root ->
      writeFixtureSource
        (root </> "application-types/src/Eclips/Application/Types/Leak.hs")
        "{-# LANGUAGE PackageImports #-}\nmodule Eclips.Application.Types.Leak where\nimport \"eclips-domain\" Eclips.Domain.Identity\n",
    rejection "global-domain import from the application protocol" $ \root ->
      writeFixtureSource
        (root </> "protocol-application/src/Eclips/Protocol/Application/DomainLeak.hs")
        "module Eclips.Protocol.Application.DomainLeak where\nimport Eclips.Domain.Identity\n",
    rejection "Herald import from the pure application client" $ \root ->
      writeFixtureSource
        (root </> "application-client/src/Eclips/Application/Client/HeraldLeak.hs")
        "module Eclips.Application.Client.HeraldLeak where\nimport Eclips.Herald.Input\n",
    rejection "Herald runtime import from the pure Herald kernel" $ \root ->
      writeFixtureSource
        (root </> "herald-core/src/Eclips/Herald/RuntimeLeak.hs")
        "module Eclips.Herald.RuntimeLeak where\nimport Eclips.Herald.Runtime\n",
    rejectionContaining "unreviewed aggregate Oracle namespace imported by Herald core" "Herald core may import only its reviewed ECLIPS namespace roots" $ \root ->
      writeFixtureSource
        (root </> "herald-core/src/Eclips/Herald/OracleLeak.hs")
        "module Eclips.Herald.OracleLeak where\nimport Eclips.Oracle\n",
    rejectionContaining "Oracle canonical semantics imported by an unrelated Herald leaf" "Oracle semantic imports in Herald core must remain in the reviewed client, projection, invariant, and coordinator seams" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Application/State.hs")
        "import Eclips.Oracle.Canonical qualified as ForbiddenOracleCanonical",
    rejectionContaining "Oracle-client private representation imported by an unrelated Herald coordinator" "owner-only Herald correlation constructors may be imported only by their facade, owner, bootstrap, or invariant" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/ApplicationCall.hs")
        "import Eclips.Herald.OracleClient.Internal qualified as ForbiddenOracleClientInternal",
    rejectionContaining "Oracle-client owner imported by another Herald leaf" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.OracleClient.State qualified as ForbiddenOracleClientState",
    rejectionContaining "checked Oracle history owner imported outside its reviewed semantic-base readers" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/SortRegistry/State.hs")
        "import Eclips.Herald.OracleProjection.State qualified as ForbiddenOracleProjectionState",
    rejectionContaining "Oracle-client public seam imported by an unrelated Herald leaf" "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Controlled/State.hs")
        "import Eclips.Herald.OracleClient qualified as ForbiddenOracleClient",
    rejectionContaining "Oracle-advance coordinator imported outside Transition" "private Herald vocabulary and use-case seams may be imported only by their reviewed consumers" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/Bootstrap.hs")
        "import Eclips.Herald.UseCase.OracleAdvance qualified as ForbiddenOracleAdvance",
    rejectionContaining "Oracle-advance coordinator importing an unrelated owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs")
        "import Eclips.Herald.IdGenerator.State qualified as ForbiddenIdGenerator",
    rejectionContaining "paired control base importing the receiver's local allocation owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/ControlBase.hs")
        "import Eclips.Herald.IdGenerator.State qualified as ForbiddenIdGenerator",
    rejectionContaining "Peer-input coordinator importing the unrelated administration owner" "cross-leaf imports must use the reviewed owner/coordinator allowlist" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/UseCase/PeerInput.hs")
        "import Eclips.Herald.Administration.State qualified as ForbiddenAdministration",
    rejectionContaining "Raft kernel imported by the pure Herald core" "Herald core may import only its reviewed ECLIPS namespace roots" $ \root ->
      insertImport
        (root </> "herald-core/internal/Eclips/Herald/OracleProjection/State.hs")
        "import Eclips.Raft.State qualified as ForbiddenRaftState",
    rejectionContaining "Domain import from the pure Raft core" "pure Raft core may import only its reviewed ECLIPS namespace roots" $ \root ->
      writeFixtureSource
        (root </> "raft-core/src/Eclips/Raft/DomainLeak.hs")
        "module Eclips.Raft.DomainLeak where\nimport Eclips.Domain.Identity\n",
    rejectionContaining "runtime import from the pure Raft core" "pure Raft core must not import runtime" $ \root ->
      writeFixtureSource
        (root </> "raft-core/src/Eclips/Raft/RuntimeLeak.hs")
        "module Eclips.Raft.RuntimeLeak where\nimport Control.Concurrent.STM\n",
    rejectionContaining "codec import from the pure Raft core" "pure Raft core must not import codecs, cryptography, persistence, or higher-level components" $ \root ->
      writeFixtureSource
        (root </> "raft-core/src/Eclips/Raft/CodecLeak.hs")
        "module Eclips.Raft.CodecLeak where\nimport Data.Binary\n",
    rejectionContaining "Raft import below its dependency layer" "semantic Domain may import only its reviewed ECLIPS namespace roots" $ \root ->
      writeFixtureSource
        (root </> "domain/src/Eclips/Domain/RaftLeak.hs")
        "module Eclips.Domain.RaftLeak where\nimport Eclips.Raft.Identity\n",
    rejectionContaining "premature Raft import from the peer protocol" "peer protocol may import only its reviewed ECLIPS namespace roots" $ \root ->
      writeFixtureSource
        (root </> "protocol-peer/src/Eclips/Protocol/Peer/RaftLeak.hs")
        "module Eclips.Protocol.Peer.RaftLeak where\nimport Eclips.Raft.Identity\n",
    rejection "socket import from the typed Herald runtime" $ \root ->
      writeFixtureSource
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/SocketLeak.hs")
        "module Eclips.Herald.Runtime.Internal.SocketLeak where\nimport Network.Socket\n",
    rejection "capacity policy identifier in the typed Herald runtime" $ \root ->
      writeFixtureSource
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/CapacityLeak.hs")
        "module Eclips.Herald.Runtime.Internal.CapacityLeak where\nimport Control.Concurrent.STM (TBQueue)\n",
    rejection "counter-exhaustion branch in the typed Herald runtime" $ \root ->
      writeFixtureSource
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/ExhaustionLeak.hs")
        "module Eclips.Herald.Runtime.Internal.ExhaustionLeak where\nforbiddenCounterLimit :: Word\nforbiddenCounterLimit = maxBound\n",
    rejection "application frame import from the typed Herald runtime" $ \root ->
      writeFixtureSource
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/FrameLeak.hs")
        "module Eclips.Herald.Runtime.Internal.FrameLeak where\nimport Eclips.Protocol.Application.Frame\n",
    rejection "peer frame import from the typed Herald runtime" $ \root ->
      writeFixtureSource
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/PeerFrameLeak.hs")
        "module Eclips.Herald.Runtime.Internal.PeerFrameLeak where\nimport Eclips.Protocol.Peer.Frame\n",
    rejectionContaining "Oracle codec imported outside the reviewed Herald adapter" "Herald Oracle-client imports must remain in the reviewed typed adapter, owner, trace, and private TCP seams" $ \root ->
      insertImport
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Owner.hs")
        "import Eclips.Protocol.Oracle.Codec qualified as ForbiddenOracleCodec",
    rejectionContaining "Oracle core imported outside the reviewed Herald adapter" "Herald Oracle-client imports must remain in the reviewed typed adapter, owner, trace, and private TCP seams" $ \root ->
      insertImport
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Connection.hs")
        "import Eclips.Oracle.Canonical qualified as ForbiddenOracleCanonical",
    rejectionContaining "Oracle framing imported by the Herald peer TCP worker" "Herald Oracle-client imports must remain in the reviewed typed adapter, owner, trace, and private TCP seams" $ \root ->
      insertImport
        (root </> "herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Peer.hs")
        "import Eclips.Protocol.Oracle.Frame qualified as ForbiddenOracleFrame",
    rejectionContaining "Oracle command imported by the Herald peer TCP worker" "Herald Oracle-client imports must remain in the reviewed typed adapter, owner, trace, and private TCP seams" $ \root ->
      insertImport
        (root </> "herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Peer.hs")
        "import Eclips.Oracle.Command qualified as ForbiddenOracleCommand",
    rejectionContaining "Oracle runtime owner imported by production Herald runtime" "production Herald runtime must not depend on a Raft kernel or Oracle runtime owner" $ \root ->
      insertImport
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/Owner.hs")
        "import Eclips.Oracle.Runtime qualified as ForbiddenOracleRuntime",
    rejectionContaining "Raft kernel imported by the Herald Oracle TCP worker" "production Herald runtime must not depend on a Raft kernel or Oracle runtime owner" $ \root ->
      insertImport
        (root </> "herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs")
        "import Eclips.Raft.State qualified as ForbiddenRaftState",
    rejection "runtime-internal import from Herald TCP" $ \root ->
      writeFixtureSource
        (root </> "herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/OwnerLeak.hs")
        "module Eclips.Herald.Runtime.TCP.Internal.OwnerLeak where\nimport Eclips.Herald.Runtime.Internal.Owner\n",
    rejection "private Herald kernel import from the runtime" $ \root ->
      writeFixtureSource
        (root </> "herald-runtime/internal/Eclips/Herald/Runtime/Internal/KernelLeak.hs")
        "module Eclips.Herald.Runtime.Internal.KernelLeak where\nimport Eclips.Herald.Genesis.Internal\n",
    rejection "exact trace imported by a public runtime facade module" $ \root ->
      insertImport
        (root </> "herald-runtime/src/Eclips/Herald/Runtime/Handler.hs")
        "import Eclips.Herald.Runtime.Internal.Trace qualified as ForbiddenTrace",
    rejection "runtime import from the pure application protocol" $ \root ->
      writeFixtureSource
        (root </> "protocol-application/src/Eclips/Protocol/Application/RuntimeLeak.hs")
        "module Eclips.Protocol.Application.RuntimeLeak where\nimport System.IO\n",
    rejection "runtime import from the pure application client" $ \root ->
      writeFixtureSource
        (root </> "application-client/src/Eclips/Application/Client/RuntimeLeak.hs")
        "module Eclips.Application.Client.RuntimeLeak where\nimport Control.Concurrent.STM\n",
    rejection "runtime import from the family-neutral framer" $ \root ->
      writeFixtureSource
        (root </> "protocol-frame/src/Eclips/Protocol/Frame/RuntimeLeak.hs")
        "module Eclips.Protocol.Frame.RuntimeLeak where\nimport Network.Socket\n",
    rejection "Herald import from the pure peer protocol" $ \root ->
      writeFixtureSource
        (root </> "protocol-peer/src/Eclips/Protocol/Peer/HeraldLeak.hs")
        "module Eclips.Protocol.Peer.HeraldLeak where\nimport Eclips.Herald.Discovery\n",
    rejection "Domain import from the pure peer protocol" $ \root ->
      writeFixtureSource
        (root </> "protocol-peer/src/Eclips/Protocol/Peer/DomainLeak.hs")
        "module Eclips.Protocol.Peer.DomainLeak where\nimport Eclips.Domain.Identity\n",
    rejection "Herald import from the application TCP facade" $ \root ->
      writeFixtureSource
        (root </> "application-api/src/Eclips/Application/Runtime/HeraldLeak.hs")
        "module Eclips.Application.Runtime.HeraldLeak where\nimport Eclips.Herald.Discovery\n",
    rejection "socket import from the public application facade" $ \root ->
      insertImport
        (root </> "application-api/src/Eclips/Application/Runtime.hs")
        "import Network.Socket qualified as ForbiddenSocket",
    rejection "safe-qualified runtime import" $ \root ->
      writeFixtureSource
        (root </> "public-types/src/Eclips/Public/Types/Leak.hs")
        "{-# LANGUAGE Safe #-}\nmodule Eclips.Public.Types.Leak where\nimport\n  safe\n  System.IO\n",
    rejection "runtime import from a preprocessed Haskell source" $ \root ->
      writeFixtureSource
        (root </> "public-types/src/Eclips/Public/Types/PreprocessedLeak.hsc")
        "module Eclips.Public.Types.PreprocessedLeak where\nimport System.IO\n"
  ]

rejectionCasesFor :: FilePath -> IO [RejectionCase]
rejectionCasesFor fixtureRoot = do
  referenceExists <-
    doesFileExist
      (fixtureRoot </> "oracle-core/src/Eclips/Oracle/Step15/Reference.hs")
  pure
    ( rejectionCases
        <> [ rejectionContaining
               "private Step-15 Oracle reference imported by live OracleAdvance"
               "the private Step-15 Oracle reference may be imported only by its two retained Herald reference-model consumers"
               ( \root ->
                   insertImport
                     (root </> "herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs")
                     "import Eclips.Oracle.Step15.Reference qualified as ForbiddenStep15Reference"
               )
           | referenceExists
           ]
    )

rejection :: String -> (FilePath -> IO FixtureRestoration) -> RejectionCase
rejection description mutateFixture =
  RejectionCase description mutateFixture Nothing

rejectionContaining :: String -> String -> (FilePath -> IO FixtureRestoration) -> RejectionCase
rejectionContaining description expectedViolationFragment mutateFixture =
  RejectionCase description mutateFixture (Just expectedViolationFragment)

withFixture :: FilePath -> (FilePath -> IO value) -> IO value
withFixture projectRoot action = bracket makeScratch removeDirectoryRecursive $ \scratch -> do
  copyFile (projectRoot </> "cabal.project") (scratch </> "cabal.project")
  forM_
    [ "public-types",
      "application-types",
      "domain",
      "protocol-frame",
      "protocol-application",
      "protocol-admin",
      "protocol-peer",
      "raft-core",
      "oracle-core",
      "protocol-raft",
      "protocol-oracle",
      "herald-core",
      "application-client",
      "application-api",
      "oracle-runtime",
      "herald-runtime",
      "deployment",
      "examples/hello-world",
      "examples/hello-context"
    ]
    $ \directory ->
      copyTree (projectRoot </> directory) (scratch </> directory)
  action scratch

makeScratch :: IO FilePath
makeScratch = do
  temporaryRoot <- getTemporaryDirectory
  (path, handle) <- openTempFile temporaryRoot "eclips-module-boundaries"
  hClose handle
  removeFile path
  createDirectory path
  pure path

copyTree :: FilePath -> FilePath -> IO ()
copyTree source destination = do
  isDirectory <- doesDirectoryExist source
  if isDirectory
    then do
      createDirectoryIfMissing True destination
      -- Package-local build and test artifacts are not checker inputs. In
      -- particular, retained OS-tour logs must not enter every fixture copy.
      entries <- filter (`notElem` [".", "..", ".cabal", "dist-newstyle"]) <$> getDirectoryContents source
      forM_ entries $ \entry -> copyTree (source </> entry) (destination </> entry)
    else do
      createDirectoryIfMissing True (takeDirectory destination)
      copyFile source destination

replaceIn :: FilePath -> String -> String -> IO FixtureRestoration
replaceIn path needle replacement = do
  contents <- readFile' path
  case replaceOnce needle replacement contents of
    Nothing -> die ("negative fixture mutation did not match: " <> path <> ": " <> show needle)
    Just changed -> replaceContents path changed

replaceOnce :: String -> String -> String -> Maybe String
replaceOnce needle replacement = go
  where
    go []
      | null needle = Just replacement
      | otherwise = Nothing
    go remaining@(character : rest)
      | needle `prefixOf` remaining = Just (replacement <> drop (length needle) remaining)
      | otherwise = (character :) <$> go rest

prefixOf :: String -> String -> Bool
prefixOf prefix value = take (length prefix) value == prefix

appendTo :: FilePath -> String -> IO FixtureRestoration
appendTo path suffix = do
  contents <- readFile' path
  replaceContents path (contents <> suffix)

insertImport :: FilePath -> String -> IO FixtureRestoration
insertImport path imported = replaceIn path "where\n" ("where\n\n" <> imported <> "\n")

writeFixtureSource :: FilePath -> String -> IO FixtureRestoration
writeFixtureSource path contents = do
  existed <- doesFileExist path
  createdDirectoryRoot <- findCreatedDirectoryRoot (takeDirectory path)
  restoration <-
    if existed
      then captureExistingFile path
      else pure
        $ FixtureRestoration
        $ do
          created <- doesFileExist path
          when created (removeFile path)
          forM_ createdDirectoryRoot $ \createdRoot -> do
            rootExists <- doesDirectoryExist createdRoot
            when rootExists (removeDirectoryRecursive createdRoot)
  ( when existed (removeFile path)
      >> createDirectoryIfMissing True (takeDirectory path)
      >> writeFile path contents
    )
    `onException` runFixtureRestoration restoration
  pure restoration

replaceContents :: FilePath -> String -> IO FixtureRestoration
replaceContents path changed = do
  restoration <- captureExistingFile path
  (removeFile path >> writeFile path changed)
    `onException` runFixtureRestoration restoration
  pure restoration

captureExistingFile :: FilePath -> IO FixtureRestoration
captureExistingFile path = do
  (backup, handle) <- openTempFile (takeDirectory path) ".eclips-module-boundary-backup"
  hClose handle
  let removeBackup = do
        exists <- doesFileExist backup
        when exists (removeFile backup)
  (removeFile backup >> copyFile path backup) `onException` removeBackup
  pure
    $ FixtureRestoration
    $ do
      exists <- doesFileExist path
      when exists (removeFile path)
      renameFile backup path

findCreatedDirectoryRoot :: FilePath -> IO (Maybe FilePath)
findCreatedDirectoryRoot path = do
  exists <- doesDirectoryExist path
  if exists
    then pure Nothing
    else do
      createdParent <- findCreatedDirectoryRoot (takeDirectory path)
      pure (Just (maybe path id createdParent))

fixtureManifest :: FilePath -> IO [FixtureEntry]
fixtureManifest fixtureRoot = walk fixtureRoot
  where
    walk directory = do
      entries <- sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents directory
      concat <$> traverse entryManifest entries
      where
        entryManifest entry = do
          let path = directory </> entry
              relativePath = makeRelative fixtureRoot path
          directoryEntry <- doesDirectoryExist path
          if directoryEntry
            then do
              children <- walk path
              pure (FixtureDirectory relativePath : children)
            else do
              -- Preserve exact bytes compactly: both baseline and restored
              -- manifests coexist for the final equality check.
              contents <- ByteString.readFile path
              pure [FixtureFile relativePath contents]
