#!/bin/sh

# Every automatic failure and combined deployment schedule runs in three fresh test processes. Top-level
# workloads stay serial so election timing and CPU evidence remain meaningful.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"

run_corpus()
{
  component=$1
  selector=$2
  expected=$3
  cabal --config-file="$project_root/.cabal/config" build "$component" -j4 --ghc-options=-Werror
  test_binary=$(cabal --config-file="$project_root/.cabal/config" list-bin "$component")
  named_tests=$("$test_binary" --list-tests)
  case_count=$(printf '%s\n' "$named_tests" | awk -v selector="$selector" 'index($0, selector) { count++ } END { print count+0 }')
  if [ "$case_count" -ne "$expected" ]
  then
    echo "P09/P10 failure corpus: expected $expected $selector schedules, found $case_count" >&2
    exit 1
  fi
  for repetition in 1 2 3
  do
    echo "P09/P10 failure corpus $selector: fresh process $repetition/3"
    cabal --config-file="$project_root/.cabal/config" test "$component" -j4 --ghc-options=-Werror \
      --test-show-details=direct --test-options="-p /$selector/ -j1"
  done
}

run_corpus eclips-oracle-runtime:test:oracle-runtime-properties p09-failure- 9
run_corpus eclips-oracle-runtime:test:oracle-runtime-properties p09-health- 2
run_corpus eclips-deployment:test:deployment-os-integration p09- 2
run_corpus eclips-deployment:test:deployment-os-integration p10- 1
echo 'P09/P10 tours: every named TCP and OS schedule passed three fresh processes.'
