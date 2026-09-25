#!/bin/sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cd "$project_root"
exec cabal --config-file="$project_root/.cabal/config" \
  --project-dir=tools/module-boundaries \
  test module-boundaries-negative -j4 \
  --ghc-options=-Werror \
  --test-show-details=direct \
  --test-options="$project_root"
