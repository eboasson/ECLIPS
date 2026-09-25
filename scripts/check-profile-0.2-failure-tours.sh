#!/bin/sh

# P04 certification: each named OS fault schedule must pass three
# consecutive fresh processes, with its deployment scope created anew.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"
component=eclips-deployment:test:deployment-os-integration

cabal --config-file="$project_root/.cabal/config" \
  build "$component" -j4 --ghc-options=-Werror
test_binary=$(cabal --config-file="$project_root/.cabal/config" list-bin "$component")
named_tests=$("$test_binary" --list-tests)

for token in p04-child-reply-loss p04-failed-spawn
do
  matching_tests=$(printf '%s\n' "$named_tests" | awk -v token="$token" 'index($0, token) { count++ } END { print count+0 }')
  if [ "$matching_tests" -ne 1 ]
  then
    echo "P04 fault corpus: expected exactly one test for $token, found $matching_tests" >&2
    exit 1
  fi
  for repetition in 1 2 3
  do
    echo "P04 $token: fresh process $repetition/3"
    cabal --config-file="$project_root/.cabal/config" \
      test "$component" -j4 --ghc-options=-Werror \
      --test-show-details=direct --test-options="-p /$token/"
  done
done

echo 'P04 failure tours: both named cases passed three consecutive fresh processes.'
