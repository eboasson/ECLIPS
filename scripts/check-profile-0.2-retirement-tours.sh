#!/bin/sh

# Every P05 retirement schedule runs in each of three fresh test processes.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"
component=eclips-herald-runtime:test:herald-step15-crash-properties
cabal --config-file="$project_root/.cabal/config" build "$component" -j4 --ghc-options=-Werror
test_binary=$(cabal --config-file="$project_root/.cabal/config" list-bin "$component")
named_tests=$("$test_binary" --list-tests)
case_count=$(printf '%s\n' "$named_tests" | awk '/P05 repeated non-voter retirement/ { count++ } END { print count+0 }')
if [ "$case_count" -ne 6 ]
then
  echo "P05 retirement corpus: expected exactly six schedules, found $case_count" >&2
  exit 1
fi
for repetition in 1 2 3
do
  echo "P05 retirement corpus: fresh process $repetition/3"
  cabal --config-file="$project_root/.cabal/config" test "$component" -j4 --ghc-options=-Werror \
    --test-show-details=direct --test-options='-p /P05/'
done
echo 'P05 retirement tours: every named schedule passed three fresh processes.'
