# Verification

Verification combines pure property tests, independent reference schedules,
compiled ownership boundaries, source/schema audits, and real TCP and operating
system process tests. The executable entry points below define the checks for
the current tree. Passing an earlier development milestone does not certify a
later checkout, and a source audit alone does not establish runtime behavior.

## Prerequisites

Follow [BUILDING.md](../../BUILDING.md) for the retained GHC version, local Cabal
configuration, formatter and memory limits. Every shell that uses the toolchain
must source the local environment and verify its compiler before running checks:

```sh
. "$PWD/.cabal/profile-0.2/f01/env.sh"
ghc --numeric-version
cabal --config-file="$PWD/.cabal/config" path
```

The required reported compiler version is `9.14.1.20260728`. Stop if the compiler
is missing or resolves differently. Run commands from the repository root; do
not overlap builds or top-level runtime suites. The runtime tests need loopback
TCP, child-process execution and enough resources for the configured heaps. A
sandbox refusal is an environment limit to diagnose, not evidence of a protocol
regression.

## Maintained gates

| Command | Evidence collected |
| --- | --- |
| `python3 scripts/check-documentation.py` | Local Markdown targets and heading anchors outside private development records. |
| `python3 scripts/test-documentation.py` | Link-checker regression tests, including extracted source trees without Git metadata. |
| `python3 scripts/test-local-toolchain.py` | Setup refusal, configuration preservation, portable wrappers and heap separation, using stand-in tools without downloads or compilation. |
| `sh scripts/check-test-component-inventory.sh` | Each root Cabal test component appears exactly once in the maintained classification. Reads source without invoking a compiler. |
| `sh scripts/check-profile-0.2-schema.sh` | Source/schema obligations, owner consumers, property registrations and current gate composition. Includes the checked component inventory. |
| `sh scripts/check-module-boundaries.sh` | Compiled positive package/import policy, pure/runtime separation and membership source obligations. |
| `sh scripts/check-type-boundaries.sh` | Compiled public API and semantic boundary checks, including accepted clients and rejected construction/coercion/import clients. |
| `sh scripts/test-module-boundaries.sh` | Complete negative mutation matrix for the module-boundary checker. |
| `sh scripts/check-development.sh` | Source/schema audit, positive module boundaries, formatting, warning-free build, independent proof contracts, all classified root test suites and founder example. |
| `sh scripts/check-profile-0.2-acceptance.sh` | Complete aggregate: both boundary systems, negative matrix, formatting, warning-free build, proof contracts, all root suites, repeated failure tours, founder example, Haddock, package checks and isolated source-archive build. |

All Cabal build and test commands use the project-local config and `-j4`.
`scripts/profile-0.2-test-components.txt` is the authoritative root suite
classification. The inventory check derives the live suites from `cabal.project`
and the package descriptions, so counts cannot silently drift. The independent
boundary tools and `tools/profile-0.2-proofs` have separate Cabal projects.

The development gate groups parallel-safe components in one Cabal invocation;
timing-sensitive components run as separate top-level invocations. The aggregate
runs every component as a separate top-level workload. Each runtime suite owns
its own internal Tasty concurrency policy. Do not add a blanket test-thread
option or run a second test command alongside an aggregate.

Both gates run the documentation and toolchain-helper checks and enable exhaustive
internal diagnostic checks. The aggregate enforces
the 768 MiB runtime heap envelope and a separate 2304 MiB compiler heap allowance.
On macOS its watchdog also monitors descendant RSS, elapsed time and total tree
memory. See [BUILDING.md](../../BUILDING.md) for the exact operational budgets and
the explicit 1024 MiB prepared-child diagnostic override; that override is not
an aggregate certification default.

## Fault schedules and independent models

[Independent proof contracts](../../tools/profile-0.2-proofs/README.md) use finite
reference models and execute named counterexamples against faulty policies.
They import no production package. Their model boundaries and omitted behavior
are explicit; passing them does not substitute for live conformance tests.

The [deterministic failure corpus](failure-corpus.md) documents named disappearance,
marker-repair, alignment, membership and private-environment schedules. Its
retained helper `scripts/check-step-16-failure-tours.sh` selects each named TCP
case exactly once and runs it in three consecutive fresh processes. The
`step-16` name also appears in stable test component names; those names do not
refer to a separate current release.

The aggregate additionally repeats the following current tour families through
`scripts/check-profile-0.2-*-tours.sh`:

- process, attachment, cancellation and End faults (`failure`);
- successive Herald retirements (`retirement`);
- seed discovery, admission and history transfer (`join`);
- native Raft/ERFT reconfiguration (`raft`);
- retained voter administration over TCP and OS processes (`voter`);
- failed voter hosts, health observations and combined deployment (`completion`).

QuickCheck remains randomized and prints replay parameters on failure. Named
seed IDs select explicit schedules; deadlines detect hangs rather than choose
race winners. Diagnose a failed schedule before rerunning the complete gate.

The [owner contracts](owner-contracts.md) retain the lifecycle, voter and failure
obligations associated with the source audit. The
[membership ledger](membership-ledger.md) explains the stable source IDs enforced
by the module checker. Neither document is a test-result record.

## Source archives

`scripts/check-step-16-source-distributions.sh` checks package metadata, derives
the declared source manifest, creates exactly the root project's package archives,
checks archive content and builds the extracted packages in isolation. Its
historical name is retained because the current aggregate invokes it directly.

The focused `--no-build` option performs metadata and archive checks only:

```sh
sh scripts/check-step-16-source-distributions.sh --no-build
```

A successful focused run does not establish the isolated-build part of the gate.
Source archives and their build trees are verification artifacts, not the public
repository export. Follow the build guide's reuse and retention policy.

## Reporting results

A verification report identifies the source commit (and any uncommitted changes),
compiler, platform, exact gate or selected commands, results, and unresolved
failures or environment limits. Report a complete aggregate only after all its
phases pass for that source state. Focused passes, earlier records and inferred
source behavior must retain their narrower meaning.

This guide specifies the verification procedure. It makes no claim that the
current snapshot has passed the complete aggregate.
