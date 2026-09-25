# Building and testing

Development, verification and performance measurements use the retained local
**GHC 9.14.2-rc1**, which reports **`9.14.1.20260728`**. Use that compiler until an
explicit toolchain change is agreed. This release candidate includes fixes for
concurrent black-hole handling that are relevant to parallel tests; see the
[official release announcement](https://www.haskell.org/ghc/blog/20260730-ghc-9.14.2-rc1-released.html).
The packages require GHC2024 and `base >= 4.22 && < 4.23`.

## Provision a fresh checkout

The compiler, wrappers, package cache and build products are ignored local assets.
An existing configured checkout can proceed to [toolchain selection](#select-the-local-toolchain).
Provision a fresh checkout explicitly; the build never substitutes a system GHC.

The exercised host is Apple Silicon macOS with command-line developer tools,
Cabal **3.16.1.0**, Python 3, `make`, `curl`, `tar` and `xz`. Development and
acceptance gates also require `rg` (ripgrep) and **fourmolu 0.20.0.0** on `PATH`.
Fourmolu is not required for a direct Cabal build or focused test. Install these
host tools separately before configuring the checkout. The shell wrappers work
on Unix hosts, but the aggregate RSS watchdog uses macOS APIs; other platforms
need separate verification and a suitable process-memory guard.

For Apple Silicon macOS, run the following from the repository root. The archive
name and SHA-256 identify the official RC bindist used by this project. The
[official release directory](https://downloads.haskell.org/~ghc/9.14.2-rc1/)
also supplies platform-specific archives and `SHA256SUMS`; another platform must
select and verify its matching archive while keeping the same compiler version.

```sh
eclips_root="$PWD"
eclips_rc="$eclips_root/.cabal/profile-0.2/p07/ghc-rc"
eclips_archive=ghc-9.14.1.20260728-aarch64-apple-darwin.tar.xz
mkdir -p "$eclips_rc"
(
  set -eu
  cd "$eclips_rc"
  test ! -e install
  curl --fail --location --output "$eclips_archive" \
    "https://downloads.haskell.org/~ghc/9.14.2-rc1/$eclips_archive"
  printf '%s  %s\n' \
    dc7df39e63a23ff5d551ee6dbc0d3bf361ce7c77a61dc9d2fed03040dc492ac9 \
    "$eclips_archive" | shasum -a 256 --check
  tar -xf "$eclips_archive"
  cd ghc-9.14.1.20260728-aarch64-apple-darwin
  ./configure --prefix="$eclips_rc/install"
  make install
)
```

Create the project-local environment, compiler wrappers and Cabal configuration:

```sh
python3 scripts/configure-local-toolchain.py --cabal "$(command -v cabal)"
```

Pass the real `cabal-install` executable, before sourcing any project Cabal
wrapper. This script verifies the compiler version, performs no downloads and
refuses to replace an existing local setup. It records the machine's Cabal path
in the ignored `.cabal/cabal-install` symlink. Generated wrappers derive the
checkout root from their own locations; their contents and the generated Cabal
configuration contain no checkout-specific absolute paths. The GHC installation
itself and Cabal build plans may contain installation paths, so moving a checkout
still requires toolchain verification and may require reinstalling the bindist.

After the checks in the next section pass, populate the local dependency cache:

```sh
. "$PWD/.cabal/profile-0.2/f01/env.sh"
cabal --config-file="$PWD/.cabal/config" update
cabal --config-file="$PWD/.cabal/config" build all -j4 --ghc-options=-Werror
```

This first build can download package dependencies. Subsequent builds can use
`--offline`. Retain the installed compiler and package store; the downloaded
archive and extracted bindist are installation inputs that can be reclaimed
once installation and verification have completed and no active work uses them.

## Select the local toolchain

Run from the repository root. Source the environment in **every new shell** that
builds, tests, profiles, runs a Cabal-based check, or resolves an executable with
`cabal list-bin`. For agent tool calls, source it in the same invocation as the
command; a previous shell's environment does not carry over.

```sh
. "$PWD/.cabal/profile-0.2/f01/env.sh"
ghc --numeric-version
ghc-pkg --version
cabal --config-file="$PWD/.cabal/config" path
```

Before building, verify:

- GHC and `ghc-pkg` report `9.14.1.20260728`.
- Cabal reports `compiler-id: ghc-9.14.1.20260728` and a `compiler-path` ending in
  `.cabal/profile-0.2/f01/compiler-bin/ghc` in this checkout.
- Cabal's `store-dir` is this checkout's `.cabal/p07-rc-store`, and its
  `config-file` is this checkout's `.cabal/config`.

The compiler wrapper invokes the installation under
`.cabal/profile-0.2/p07/ghc-rc/install`. The environment also selects the matching
package manager, the Cabal wrapper that selects the local package store, macOS
command-line tools and project-local temporary storage. **Using `.cabal/config`
alone does not select RC1.**

If the environment or compiler is missing, or verification selects another
toolchain, stop and report it. Do not substitute a system GHC or install a different
release implicitly. These `.cabal` files are local, ignored assets; a fresh clone
needs the provisioning procedure above before following this workflow. Older
local wrappers may contain checkout-specific paths; verify the selected compiler,
store and configuration again after moving or copying any checkout.

## Build and test

Keep Cabal's global `--config-file` option before the subcommand. Use `-j4` and
`-Werror`, and reuse the root `dist-newstyle` build rather than creating a new build
tree for each increment. Ordinary Cabal builds use `-O1`; profiling or optimization
changes must be explicit when comparing measurements.

Build all components using the cached dependencies:

```sh
. "$PWD/.cabal/profile-0.2/f01/env.sh"
cabal --config-file="$PWD/.cabal/config" build all --offline -j4 --ghc-options=-Werror
```

Run the tests relevant to the change, for example:

```sh
. "$PWD/.cabal/profile-0.2/f01/env.sh"
cabal --config-file="$PWD/.cabal/config" test eclips-herald-core:test:herald-core-properties \
  --offline -j4 --ghc-options=-Werror --test-show-details=direct \
  --test-options='+RTS -N4 -M768m -RTS'
```

`--offline` reuses the local dependency cache. If dependencies are missing, fetch
them deliberately using the same compiler, configuration and store. TCP/process
tests need local socket access. The [verification guide](docs/verification/README.md)
describes the development and aggregate gates; a focused test run is not an
aggregate pass. Source the same environment before invoking those scripts.

## Internal diagnostic audits

Herald runtimes enable redundant whole-state and historical-frontier audits by
default. The development and Profile-0.2 acceptance gates explicitly export
`ECLIPS_DIAGNOSTIC_CHECKS=on`, including for spawned applications and Heralds.
Pure checked entry points also enable audits by default.

For an ordinary-build performance comparison, set
`ECLIPS_DIAGNOSTIC_CHECKS=off` on the direct workload command; use `on` for its
comparison run. Both modes use the same executable. TCP startup reads the option
once and rejects values other than `on` or `off`; absence preserves the typed
runtime configuration. Embedders without TCP can call
`configureHeraldRuntimeDiagnosticChecks`. Exact conformance replay records the
selected policy.

This option gates selected redundant owner audits and repeated coverage/closure
proofs for already admitted frontiers. Wire shape and canonical checks, incoming
admission, missing-data holds, and pending-work/readiness guards remain enabled.
It is independent of tracing and work-count output. The
[label architecture](docs/architecture/labels.md) describes the current contract
and diagnostic scope. Performance measurements must record the selected policy.

## Heap limits and process memory

| Work | Heap limit | How it is selected |
| --- | ---: | --- |
| Ordinary tests and application/runtime processes | 768 MiB | The local environment sets `GHCRTS=-M768m`; focused test commands also specify `+RTS -M768m -RTS`. |
| GHC compilation | 2,304 MiB | The compiler wrapper passes `+RTS -M2304m -RTS` to GHC only. The aggregate gate also wraps its compiler helper tools. |
| Label prepared-child diagnostic measurement | 1,024 MiB | The agreed exception is passed explicitly as `+RTS -M1024m -RTS` for that case. It does not change the ordinary suite limit. |

Keep the compiler allowance out of global `GHCRTS`: test processes and spawned
applications inherit that variable. Do not disable or silently increase a limit
to make a failing test pass. Record heap exhaustion as a failed run; a diagnostic
override must be explicit, justified and recorded separately from normal-limit
verification. Profiling does not implicitly authorize a larger heap. Record the
effective runtime options, including executable defaults and explicit overrides.

A heap limit is not an RSS limit. Native allocations, stacks, mapped code and
multiple child processes add to resident memory. On macOS the
[aggregate acceptance gate](scripts/check-profile-0.2-acceptance.sh) uses
[run-memory-guard.py](scripts/run-memory-guard.py) with these additional limits:

| Guarded resource | Default threshold |
| --- | ---: |
| Each non-compiler process RSS | 900 MiB |
| Each recognized compiler/linker process RSS | 2,816 MiB |
| Simultaneous process-tree RSS | 4,096 MiB |

The guard samples RSS every 50 ms by default; these are watchdog thresholds,
not hard operating-system memory limits.

Sourcing the local environment alone does **not** start this watchdog. For an
expensive standalone build or test, use the same guard and pass
`--heap 768m --process-mib 900 --compiler-mib 2816 --tree-mib 4096`, a named output
file and an appropriate explicit `--seconds` timeout. The aggregate gate already
does this. A diagnostic heap exception also needs an appropriate recorded RSS
threshold; the ordinary 900 MiB threshold is not sufficient for a 1 GiB heap.
Keep failed-run logs and guard reports, and check that no descendants remain
before starting the next run. These are build/experiment controls, not protocol
resource limits.

## Build and artifact retention

Reuse the current ordinary build in `dist-newstyle`. Keep one reusable profiling
build only while profiling is needed. Do not create a new build tree or freeze a
copy of every executable for each run or increment. Separate trees required by
tool projects, Haddock or source-archive certification are legitimate; reuse them
where the check permits it and identify them in the cleanup report. A branch
switch by itself does not require a separate build tree.

| Artifact | Retention policy |
| --- | --- |
| Local RC installation, environment/compiler/Cabal wrappers, `.cabal/config`, and `.cabal/p07-rc-store` | Keep as shared toolchain and dependency inputs. |
| Current ordinary build and a currently used profiling build | Keep for incremental work; reclaim when idle if space is needed, accepting the rebuild cost. |
| Older build trees, extracted source builds and duplicate executable copies | Make eligible for cleanup when no active build, comparison or reproduction depends on them. Do not retain one per completed increment indefinitely. |
| Frozen baseline/candidate binaries and source snapshots | Retain the minimum set needed for the active comparison. Afterward retain source identity, options, hashes and results; list binaries as cleanup candidates unless an exact-binary rerun is still needed. A rebuild need not be bit-identical. |
| Compact measurement summaries, commands, manifests, counters and relevant failure logs | Retain the evidence supporting recorded results. Include compiler, optimization, profiling/tracing, heap and guard settings. |
| Large eventlogs, heap profiles and detailed traces | Retain while they support active analysis or an unresolved issue. Once superseded, identify them for explicit archival or deletion; summaries alone cannot answer new questions about the raw trace. |

In particular, preserve `.cabal/profile-0.2/f01/env.sh`, its `compiler-bin`
wrappers, `.cabal/profile-0.2/p07/verify-bin`, `.cabal/cabal-install` when present,
`.cabal/profile-0.2/p07/ghc-rc/install`, `.cabal/p07-rc-store` and `.cabal/config`.
Also preserve any source directory, fixture executable or wrapper referenced by
an active Cabal plan or measurement helper. A directory under an old experiment
name may still be a shared dependency. Do not remove `.cabal` or `.cabal/tmp`
wholesale. When discarding source-distribution extraction/build trees, preserve
the exact source archives and manifests needed by the recorded certification.

After substantial build or measurement work, report exact cleanup paths and
freshly measured sizes, distinguishing rebuildable outputs from retained evidence
and shared inputs. State what is lost by deletion and whether it has actually
happened. Before deleting, confirm builds/tests are idle, inspect Git status and
active `cache/plan.json` files, and check source paths and symlink targets. Preview
the exact deletion set first; refresh an old cleanup manifest when the checkout,
plans or dependencies have changed. Do not use broad experiment-directory globs.

The root project builds the networked implementation and examples described in
[the networked specification](docs/networked/README.md).
