#!/bin/sh

# Retained failure corpus: each named fault schedule must pass three
# consecutive fresh processes. Ordinary randomized suites retain their own seeds.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"
component=eclips-herald-runtime:test:herald-step16-failure-properties

cabal --config-file="$project_root/.cabal/config" \
  build "$component" -j4 --ghc-options=-Werror
test_binary=$(cabal --config-file="$project_root/.cabal/config" list-bin "$component")
named_tests=$("$test_binary" --list-tests)

for seed in 162001 162002
do
  matching_tests=$(printf '%s\n' "$named_tests" | awk -v seed="seed-$seed:" 'index($0, seed) { count++ } END { print count+0 }')
  if [ "$matching_tests" -ne 1 ]
  then
    echo "Step-16 fault corpus: expected exactly one test for seed-$seed, found $matching_tests" >&2
    exit 1
  fi
  case "$seed" in
    162001) fault='hold both publication directions and H1-H2 alignment before admission/ack; close their duplex connection once; reconnect' ;;
    162002) fault='hold accepted H1 newenv root to H4; crash and retire H4; complete successor lineage' ;;
  esac
  for repetition in 1 2 3
  do
    echo "Step-16 seed-$seed: fresh process $repetition/3; $fault"
    cabal --config-file="$project_root/.cabal/config" \
      test "$component" -j4 --ghc-options=-Werror \
      --test-show-details=direct --test-options="-p /seed-$seed:/"
  done
done

echo 'Step-16 failure tours: both named cases passed three consecutive fresh processes.'
