#!/bin/sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
checked_root=${ECLIPS_BOUNDARY_PROJECT_ROOT:-$project_root}

cd "$project_root"
exec cabal --config-file="$project_root/.cabal/config" \
  --project-dir=tools/module-boundaries \
  run -j4 --ghc-options=-Werror eclips-module-boundaries -- "$checked_root"
