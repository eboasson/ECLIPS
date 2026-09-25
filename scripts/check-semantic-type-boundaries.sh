#!/bin/sh

set -eu

# Build the architecture once, then reuse its interfaces for every semantic
# probe. Only the two assertions about Cabal-private sublibraries still ask
# Cabal to reject a client component.
case $# in
  0)
    write_surface=false
    ;;
  2)
    if [ "$1" != "--write-surface" ]
    then
      echo "usage: $0 [--write-surface CANDIDATE]" >&2
      exit 2
    fi
    write_surface=true
    ;;
  *)
    echo "usage: $0 [--write-surface CANDIDATE]" >&2
    exit 2
    ;;
esac

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
architecture_builddir=${ECLIPS_BOUNDARY_BUILDDIR:-"$project_root/dist-newstyle"}
fixture_project_dir="$project_root/scripts/fixtures/type-boundaries"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/eclips-semantic-type-boundaries.XXXXXX")
trap 'rm -rf "$scratch"' EXIT HUP INT TERM

cd "$project_root"

cabal --config-file="$project_root/.cabal/config" \
  --builddir="$architecture_builddir" \
  build all -j4 --ghc-options=-Werror

cabal_paths=$(cabal --config-file="$project_root/.cabal/config" path)
compiler_path=$(printf '%s\n' "$cabal_paths" | sed -n 's/^compiler-path: //p')
compiler_store_path=$(printf '%s\n' "$cabal_paths" | sed -n 's/^compiler-store-path: //p')

if [ -z "$compiler_path" ] || [ ! -x "$compiler_path" ]
then
  echo "cannot locate Cabal's configured GHC executable" >&2
  exit 1
fi

compiler_version=$("$compiler_path" --numeric-version)
compiler_libdir=$("$compiler_path" --print-libdir)

if [ -z "$compiler_store_path" ] || [ ! -d "$compiler_store_path/package.db" ]
then
  echo "cannot locate Cabal's package database for GHC $compiler_version" >&2
  exit 1
fi

cabal --config-file="$project_root/.cabal/config" \
  --project-dir="$project_root/tools/type-boundaries" \
  --with-compiler="$compiler_path" \
  test all -j4 --ghc-options=-Werror --test-show-details=direct

cabal --config-file="$project_root/.cabal/config" \
  --project-dir="$project_root/tools/type-boundaries" \
  --with-compiler="$compiler_path" \
  run -j4 eclips-type-boundaries --ghc-options=-Werror -- \
  --project-root "$project_root" \
  --ghc-libdir "$compiler_libdir" \
  --dependency-package-db "$compiler_store_path/package.db" \
  --package-db "$architecture_builddir/packagedb/ghc-$compiler_version" \
  --scratch "$scratch/ghc" \
  --surface "$project_root/tools/type-boundaries/api-surface.txt" \
  "$@"

if [ "$write_surface" = true ]
then
  exit 0
fi

expect_private_dependency_failure()
{
  case_id=$1
  fixture_component=$2
  private_component=$3
  fixture_flag=$4
  log="$scratch/$case_id.log"

  if cabal --config-file="$project_root/.cabal/config" \
    --project-dir="$fixture_project_dir" \
    --builddir="$scratch/private-client-dist" \
    build "eclips-public-boundary-fixtures:lib:$fixture_component" -j4 \
    "$fixture_flag" >"$log" 2>&1
  then
    echo "ordinary client unexpectedly depended on private component: $private_component" >&2
    exit 1
  fi

  if ! grep -Eqi 'hidden module|member of the hidden package|private librar|private sublibrary|component is private' "$log" ||
    ! grep -q "$private_component" "$log"
  then
    echo "Cabal-private dependency probe failed for an unexpected reason: $case_id" >&2
    cat "$log" >&2
    exit 1
  fi
}

expect_private_dependency_failure \
  runtime-private-dependency \
  public-runtime-internal-import \
  runtime-internal \
  -fruntime-private-dependency

expect_private_dependency_failure \
  tcp-private-dependency \
  public-step10-internal-import \
  tcp-internal \
  -ftcp-private-dependency

echo "semantic type-boundary gate passed, including 2 Cabal-private dependency probes"
