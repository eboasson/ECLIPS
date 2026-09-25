#!/bin/sh

# Certify exactly the root project's source packages, then build only their
# audited archive contents. --no-build performs the focused metadata/archive
# checks without claiming the isolated-build gate has passed.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"
export PYTHONDONTWRITEBYTECODE=1
exec python3 scripts/source-distributions/check.py "$@"
