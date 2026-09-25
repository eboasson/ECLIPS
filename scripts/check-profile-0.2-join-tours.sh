#!/bin/sh

# Every P06 seed/admission schedule runs in each of three fresh test processes.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"
component=eclips-deployment:test:deployment-os-integration
cabal --config-file="$project_root/.cabal/config" build "$component" -j4 --ghc-options=-Werror
test_binary=$(cabal --config-file="$project_root/.cabal/config" list-bin "$component")
named_tests=$("$test_binary" --list-tests)
case_count=$(printf '%s\n' "$named_tests" | awk '/p06-/ { count++ } END { print count+0 }')
if [ "$case_count" -ne 2 ]
then
  echo "P06 join corpus: expected exactly two schedules, found $case_count" >&2
  exit 1
fi
for repetition in 1 2 3
do
  echo "P06 join corpus: fresh process $repetition/3"
  cabal --config-file="$project_root/.cabal/config" test "$component" -j4 --ghc-options=-Werror \
    --test-show-details=direct --test-options='-p /p06-/'
done
echo 'P06 join tours: every named schedule passed three fresh processes.'
