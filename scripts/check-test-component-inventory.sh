#!/bin/sh

# Fast, read-only proof that the selected test classification is a partition of
# the test suites in the root Cabal project.  This deliberately reads Cabal
# source rather than asking the solver to rebuild a plan.

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
if [ "$#" -gt 1 ]
then
  echo "usage: $0 [INVENTORY]" >&2
  exit 2
fi
# Current gates and standalone use share the maintained component inventory.
inventory=${1:-"$project_root/scripts/profile-0.2-test-components.txt"}
case "$inventory" in
  /*) ;;
  *) inventory="$project_root/$inventory" ;;
esac
inventory_name=$(basename "$inventory" .txt)
project_file="$project_root/cabal.project"

cd "$project_root"

fail()
{
  echo "$inventory_name inventory: $1" >&2
  exit 1
}

[ -f "$inventory" ] || fail "missing classification file: $inventory"
[ -f "$project_file" ] || fail "missing root project file: $project_file"

invalid_lines=$(
  awk '
    /^[[:space:]]*(#|$)/ { next }
    NF != 2 || ($1 != "parallel-safe" && $1 != "timing-sensitive") {
      print NR ":" $0
    }
  ' "$inventory"
)
if [ -n "$invalid_lines" ]
then
  fail "invalid classification lines:\n$invalid_lines"
fi

classified_targets=$(
  awk '
    $1 == "parallel-safe" || $1 == "timing-sensitive" { print $2 }
  ' "$inventory" | LC_ALL=C sort
)
[ -n "$classified_targets" ] || fail "classification is empty"

duplicate_targets=$(
  printf '%s\n' "$classified_targets" | uniq -d
)
if [ -n "$duplicate_targets" ]
then
  fail "components classified more than once:\n$duplicate_targets"
fi

for class in parallel-safe timing-sensitive
do
  if ! awk -v wanted="$class" '$1 == wanted { found = 1 } END { exit !found }' "$inventory"
  then
    fail "classification has no $class component"
  fi
done

# The root project currently uses one simple packages: block.  Stop at the
# next unindented field and ignore commented package entries.
package_files=$(
  awk '
    /^packages:[[:space:]]*$/ { in_packages = 1; next }
    in_packages && /^[^[:space:]]/ { in_packages = 0 }
    in_packages {
      line = $0
      sub(/[[:space:]]*--.*/, "", line)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", line)
      if (line ~ /[.]cabal$/) print line
    }
  ' "$project_file"
)
[ -n "$package_files" ] || fail "root project contains no package files"

actual_targets=
for package_file in $package_files
do
  [ -f "$package_file" ] || fail "project package file does not exist: $package_file"
  package_name=$(awk '$1 == "name:" { print $2; exit }' "$package_file")
  [ -n "$package_name" ] || fail "package has no name field: $package_file"

  suites=$(awk '$1 == "test-suite" { print $2 }' "$package_file")
  for suite in $suites
  do
    target="$package_name:test:$suite"
    if [ -n "$actual_targets" ]
    then
      actual_targets="$actual_targets
$target"
    else
      actual_targets=$target
    fi
  done
done
actual_targets=$(printf '%s\n' "$actual_targets" | LC_ALL=C sort)

if [ "$actual_targets" != "$classified_targets" ]
then
  echo "$inventory_name inventory does not match the root Cabal project." >&2
  echo "Classified targets:" >&2
  printf '%s\n' "$classified_targets" >&2
  echo "Actual targets:" >&2
  printf '%s\n' "$actual_targets" >&2
  exit 1
fi

component_count=$(printf '%s\n' "$actual_targets" | awk 'NF { count += 1 } END { print count + 0 }')
parallel_count=$(awk '$1 == "parallel-safe" { count += 1 } END { print count + 0 }' "$inventory")
timing_count=$(awk '$1 == "timing-sensitive" { count += 1 } END { print count + 0 }' "$inventory")
echo "$inventory_name inventory passed: $component_count total ($parallel_count parallel-safe, $timing_count timing-sensitive)"
