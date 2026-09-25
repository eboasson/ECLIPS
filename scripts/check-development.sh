#!/bin/sh

set -eu

# Verification always exercises exhaustive internal audits, even after a local
# performance experiment exported the disabled mode.
ECLIPS_DIAGNOSTIC_CHECKS=on
export ECLIPS_DIAGNOSTIC_CHECKS

# Fast feedback for ordinary implementation work. The exhaustive acceptance
# gate additionally exercises the boundary checkers' negative fixtures, public
# compile-fail fixtures, Haddock, package metadata, and source distributions.
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_inventory="$project_root/scripts/profile-0.2-test-components.txt"

cd "$project_root"
TMPDIR="$project_root/.cabal/tmp"
export TMPDIR
mkdir -p "$TMPDIR"
git diff --no-ext-diff --check HEAD
python3 scripts/check-documentation.py
python3 scripts/test-documentation.py
python3 scripts/test-local-toolchain.py
sh scripts/check-profile-0.2-schema.sh
sh scripts/check-module-boundaries.sh
rg --files -0 -g '*.hs' | xargs -0 fourmolu --mode check
cabal --config-file="$project_root/.cabal/config" build all -j4 --ghc-options=-Werror

# Independent prospective contracts run alongside production conformance tests.
cabal --config-file="$project_root/.cabal/config" \
  --project-dir=tools/profile-0.2-proofs \
  test eclips-profile02-proofs:test:proof-contracts -j4 \
  --ghc-options=-Werror --test-show-details=direct

# Pure/property components are safe to schedule as one explicit Cabal union.
# Keep the target list checked against cabal.project before expanding it here.
parallel_targets=$(awk '$1 == "parallel-safe" { print $2 }' "$test_inventory")
# Component names cannot contain shell whitespace; word splitting is intentional.
# shellcheck disable=SC2086
cabal --config-file="$project_root/.cabal/config" \
  test $parallel_targets -j4 \
  --ghc-options=-Werror \
  --test-show-details=direct

# Runtime/TCP/process suites execute one top-level component at a time.  Do not
# add a global --test-options value: their own Main modules own any Tasty cap.
timing_sensitive_targets=$(awk '$1 == "timing-sensitive" { print $2 }' "$test_inventory")
for component in $timing_sensitive_targets
do
  cabal --config-file="$project_root/.cabal/config" \
    test "$component" -j4 \
    --ghc-options=-Werror \
    --test-show-details=direct
done

cabal --config-file="$project_root/.cabal/config" run -j4 eclips-hello-world:exe:eclips-hello-world --ghc-options=-Werror -- --bootstrap
