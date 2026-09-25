#!/bin/sh

# Exercise each native configuration transport schedule in three fresh processes.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"
component=eclips-protocol-raft:test:protocol-raft-properties
cabal --config-file="$project_root/.cabal/config" build "$component" -j4 --ghc-options=-Werror
test_binary=$(cabal --config-file="$project_root/.cabal/config" list-bin "$component")
named_tests=$("$test_binary" --list-tests)
case_count=$(printf '%s\n' "$named_tests" | awk '/p07-erft/ { count++ } END { print count+0 }')
if [ "$case_count" -ne 5 ]
then
  echo "P07 ERFT corpus: expected exactly five schedules, found $case_count" >&2
  exit 1
fi
for repetition in 1 2 3
do
  echo "P07 ERFT corpus: fresh process $repetition/3"
  cabal --config-file="$project_root/.cabal/config" test "$component" -j4 --ghc-options=-Werror \
    --test-show-details=direct --test-options='-p /p07-erft/'
done
echo 'P07 ERFT tours: every named schedule passed three fresh processes.'
