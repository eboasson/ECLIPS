#!/bin/sh

# Current profile-0.2 aggregate gate. Current obligations are enforced by the
# schema audit; docs/verification explains the evidence and its limits.
set -eu

# Verification always exercises exhaustive internal audits, even after a local
# performance experiment exported the disabled mode.
ECLIPS_DIAGNOSTIC_CHECKS=on
export ECLIPS_DIAGNOSTIC_CHECKS

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_inventory="$project_root/scripts/profile-0.2-test-components.txt"
cd "$project_root"

# Native runtimes retain their 768MiB heap cap throughout the complete gate.
# macOS additionally watches RSS and every descendant, including child groups.
# Compilers receive a distinct budget below the user's 3GiB process ceiling.
if [ "$(uname -s)" = Darwin ] && [ "${ECLIPS_ACCEPTANCE_GUARDED:-0}" != 1 ]
then
  ECLIPS_ACCEPTANCE_GUARDED=1
  export ECLIPS_ACCEPTANCE_GUARDED
  exec python3 scripts/run-memory-guard.py \
    --output "${ECLIPS_ACCEPTANCE_OUTPUT_DIR:-$project_root/.cabal/profile-0.2/f01}/acceptance-memory.json" \
    --seconds "${ECLIPS_ACCEPTANCE_SECONDS:-7200}" \
    --heap 768m --process-mib 900 --compiler-mib 2816 \
    --tree-mib "${ECLIPS_ACCEPTANCE_TREE_MIB:-4096}" -- sh "$0" "$@"
fi

GHCRTS="${GHCRTS:-} -M768m"
export GHCRTS

# Keep the compiler-only override out of GHCRTS, which every test/application
# inherits. Capturing the original paths before installing these wrappers also
# covers the nested gates without changing their historical invocation text.
compiler_bin="${ECLIPS_ACCEPTANCE_OUTPUT_DIR:-$project_root/.cabal/profile-0.2/f01}/acceptance-compiler-bin"
mkdir -p "$compiler_bin"
ECLIPS_ACCEPTANCE_GHC=$(command -v ghc)
ECLIPS_ACCEPTANCE_RUN_GHC=$(command -v runghc)
ECLIPS_ACCEPTANCE_HADDOCK=$(command -v haddock)
ECLIPS_ACCEPTANCE_HSC2HS=$(command -v hsc2hs)
export ECLIPS_ACCEPTANCE_GHC ECLIPS_ACCEPTANCE_RUN_GHC
export ECLIPS_ACCEPTANCE_HADDOCK ECLIPS_ACCEPTANCE_HSC2HS
cat > "$compiler_bin/ghc" <<'EOF'
#!/bin/sh
exec "$ECLIPS_ACCEPTANCE_GHC" "$@" +RTS -M2304m -RTS
EOF
cat > "$compiler_bin/runghc" <<'EOF'
#!/bin/sh
exec "$ECLIPS_ACCEPTANCE_RUN_GHC" --ghc-arg=+RTS --ghc-arg=-M2304m --ghc-arg=-RTS "$@"
EOF
cat > "$compiler_bin/haddock" <<'EOF'
#!/bin/sh
exec "$ECLIPS_ACCEPTANCE_HADDOCK" "$@" +RTS -M2304m -RTS
EOF
cat > "$compiler_bin/hsc2hs" <<'EOF'
#!/bin/sh
exec "$ECLIPS_ACCEPTANCE_HSC2HS" "$@" +RTS -M2304m -RTS
EOF
chmod +x "$compiler_bin/ghc" "$compiler_bin/runghc" \
  "$compiler_bin/haddock" "$compiler_bin/hsc2hs"
PATH="$compiler_bin:$PATH"
export PATH

# Boundary probes, archive extraction and compiler scratch stay in the project.
TMPDIR="$project_root/.cabal/tmp"
export TMPDIR
mkdir -p "$TMPDIR"

gate_phase=initialization
trap 'gate_status=$?; if [ "$gate_status" -ne 0 ]; then printf "Profile-0.2 acceptance failed during: %s\n" "$gate_phase" >&2; fi' EXIT

phase()
{
  gate_phase=$1
  printf 'Profile-0.2 acceptance: %s\n' "$gate_phase"
}

require_component()
{
  if ! awk -v class="$1" -v component="$2" \
    '$1 == class && $2 == component { found++ } END { exit found != 1 }' \
    "$test_inventory"
  then
    printf 'Profile-0.2 acceptance requires exactly one %s entry: %s\n' "$1" "$2" >&2
    exit 1
  fi
}

phase 'diff, current schema and checked component inventory'
git diff --no-ext-diff --check HEAD
python3 scripts/check-documentation.py
python3 scripts/test-documentation.py
python3 scripts/test-local-toolchain.py
sh scripts/check-profile-0.2-schema.sh

# Name the release evidence explicitly without selecting any component twice.
# The schema audit also pins the all-eight RPC matrix and owner work laws.
require_component parallel-safe eclips-raft-core:test:raft-core-properties
require_component parallel-safe eclips-oracle-core:test:oracle-core-properties
require_component timing-sensitive eclips-oracle-runtime:test:oracle-runtime-properties
require_component parallel-safe eclips-herald-core:test:herald-core-properties
require_component parallel-safe eclips-herald-core:test:herald-step16-prospective-properties
require_component timing-sensitive eclips-application-api:test:application-api-properties
require_component timing-sensitive eclips-herald-runtime:test:herald-step15-crash-properties
require_component timing-sensitive eclips-herald-runtime:test:herald-step16-failure-properties
require_component timing-sensitive eclips-hello-world:test:hello-world-integration
require_component parallel-safe eclips-deployment:test:deployment-properties
require_component timing-sensitive eclips-deployment:test:deployment-os-integration

phase 'positive module and semantic/public boundaries'
sh scripts/check-module-boundaries.sh
sh scripts/check-type-boundaries.sh

phase 'complete optimized negative module-boundary matrix'
sh scripts/test-module-boundaries.sh

phase 'formatting and all-component warning-free build'
rg --files -0 -g '*.hs' | xargs -0 fourmolu --mode check
cabal --config-file="$project_root/.cabal/config" \
  build all -j4 --ghc-options=-Werror

phase 'independent profile-0.2 proof contracts'
cabal --config-file="$project_root/.cabal/config" \
  --project-dir=tools/profile-0.2-proofs \
  test eclips-profile02-proofs:test:proof-contracts -j4 \
  --ghc-options=-Werror --test-show-details=direct

phase 'property and unit components, one top-level workload at a time'
parallel_targets=$(awk '$1 == "parallel-safe" { print $2 }' "$test_inventory")
for component in $parallel_targets
do
  phase "property and unit component $component"
  cabal --config-file="$project_root/.cabal/config" \
    test "$component" -j4 --ghc-options=-Werror --test-show-details=direct
done

# Each timing-sensitive component owns its internal Tasty concurrency. Never
# overlap these top-level Cabal commands or impose a global test thread option.
timing_targets=$(awk '$1 == "timing-sensitive" { print $2 }' "$test_inventory")
for component in $timing_targets
do
  phase "timing-sensitive component $component"
  cabal --config-file="$project_root/.cabal/config" \
    test "$component" -j4 --ghc-options=-Werror --test-show-details=direct
done

phase 'three consecutive fresh processes per named failure-sensitive TCP case'
sh scripts/check-step-16-failure-tours.sh

phase 'three consecutive fresh processes per named P04 OS fault case'
sh scripts/check-profile-0.2-failure-tours.sh

phase 'three consecutive fresh processes per named P05 retirement case'
sh scripts/check-profile-0.2-retirement-tours.sh

phase 'three consecutive fresh processes per named P06 seed and admission case'
sh scripts/check-profile-0.2-join-tours.sh

phase 'three consecutive fresh processes per named P07 native ERFT case'
sh scripts/check-profile-0.2-raft-tours.sh

phase 'three consecutive fresh processes per named P08 voter TCP and OS case'
sh scripts/check-profile-0.2-voter-tours.sh

phase 'three consecutive fresh processes per named P09 failure and P10 combined case'
sh scripts/check-profile-0.2-completion-tours.sh

phase 'explicit one-Herald founder executable smoke test'
cabal --config-file="$project_root/.cabal/config" \
  run -j4 eclips-hello-world:exe:eclips-hello-world --ghc-options=-Werror -- --bootstrap

phase 'all-package Haddock in its separate compilation tree'
cabal --config-file="$project_root/.cabal/config" \
  --builddir="$project_root/.cabal/profile-0.2-haddock-dist" \
  haddock all -j4 --ghc-options=-Werror --haddock-executables

phase 'package checks, exact source archives and isolated archive build'
sh scripts/check-step-16-source-distributions.sh

phase 'complete'
printf 'Profile-0.2 aggregate acceptance passed with F01 label generation.\n'
