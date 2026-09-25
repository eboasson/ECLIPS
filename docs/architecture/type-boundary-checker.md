# Semantic type-boundary checker

The development-only checker loads built project interfaces in one GHC session,
audits the complete public name surface, and compiles positive and negative client
probes. Cabal separately checks private-sublibrary dependency rejection.
The source/module checker remains the pre-build authority for package visibility,
production imports and dependency walls.

## Why not only parse source import/export lists?

Parsing source with the GHC API would remove compiler process startup, but a
source-only list comparison would not be equally thorough. It would have to
reimplement name resolution for module re-exports and subordinate exports,
account for CPP and Cabal-selected components, and still could not establish
constructor opacity, record-update availability, `Generic` instances, nominal
roles, claim/DTO type separation, or exhaustive downstream pattern matching.

The implemented split uses each representation where it is authoritative:

- Cabal's plan and the existing module checker cover component visibility,
  declared dependencies, and production imports before compilation;
- GHC's compiled `UnitInfo` and `ModInfo` cover the resolved public module/name
  surface after Cabal configuration and preprocessing;
- small generated client modules ask GHC's renamer, type checker, role solver,
  instance solver, and exhaustiveness checker about semantic capabilities;
- Cabal itself retains the two tests whose subject is private-sublibrary
  dependency rejection.

This gets one-session interface/probe evaluation without
weakening the checks to syntax alone.

This is not a general API-compatibility checker: the manifest deliberately
tracks modules and exported names, not every exported type signature or ABI
fingerprint. Boundary-sensitive type relationships remain explicit client
probes, while ordinary compilation, property tests, and downstream package tests
cover behavior and signatures outside the boundary policy.

## Required guarantees

The gate enforces the following assertions:

- hidden modules and private components are unavailable to ordinary clients;
- checked and owner-held constructors, fields, and accessors remain unavailable;
- checked startup carriers cannot be updated as public records or acquired
  through an unintended `Generic` instance;
- private identities, protocol claims, and physical-plane references remain
  nominally distinct and non-coercible;
- application and Oracle DTO shapes remain distinct from global and kernel
  representations;
- the public application, Herald, runtime, TCP, administration, recovery, and
  timer facades remain usable;
- reviewed public sums remain exhaustively consumable with incomplete-pattern
  warnings promoted to errors;
- any addition or removal in a public module's semantic export set requires an
  explicit review.

The checker must not infer expected policy from the implementation under test.
It may generate a candidate surface file for review, but the normal check compares
against an independently committed expectation and never updates it.

## Architecture

```text
production source ------> module-boundary checker
                              |
                              +-- exact Cabal plan and component metadata
                              +-- production import and dependency walls
                              +-- declared public/private component visibility

normal Cabal build ------> inplace package database and .hi interfaces
                                              |
                                              v
                                semantic type-boundary checker
                                +-- public unit/module audit
                                +-- exact semantic export audit
                                +-- in-process negative GHC probes
                                +-- positive facade/exhaustiveness probes

two small Cabal builds --> actual private-sublibrary dependency rejection
```

The checker is a standalone package under `tools/type-boundaries`. It depends on
the `ghc` library but has no build dependency on an ECLIPS package: project units
are loaded dynamically from the root build's inplace package database. This keeps
the root architecture plan and every production dependency graph unchanged.

## Package context

The acceptance wrapper performs one normal root build, obtains Cabal's configured
compiler and store from `cabal path`, and asks that exact compiler for its libdir.
The runner accepts the project root, scratch directory, GHC libdir, project
package database, and
zero or more dependency package databases explicitly. It initializes GHC in two
steps:

1. read the root database's `package.cache`, identify the exact installed unit ID
   for every reviewed logical unit, and reject missing or ambiguous matches;
2. start one GHC session with the dependency and root databases, hide all
   packages, then expose only those exact project unit IDs and the ordinary
   non-project packages needed by client fixtures.

Project units are addressed in policy by stable logical references:

```text
package name + main library
package name + named public sublibrary
```

Build-specific unit IDs are therefore resolved at runtime and never enter the
reviewed policy or manifest. The wrapper derives the root database name from the
same GHC version used to run the checker, so an incompatible or incomplete build
cannot be mistaken for a boundary success.

All public main libraries are exposed. The sole current public named sublibrary is
`eclips-herald-runtime:tcp`. These implementation components remain hidden:

- `eclips-herald-core:kernel-internal`;
- `eclips-application-api:physical-internal`;
- `eclips-herald-runtime:runtime-internal`;
- `eclips-herald-runtime:tcp-internal`.

Public facades may re-export names defined by a private component. That is an
intentional public capability and is distinguished from exposing the private unit
itself.

## Semantic export surface

For each module exposed or re-exported by a reviewed public unit, the checker
loads the compiled interface and obtains its semantic exports. The normalized
surface records:

- public logical unit;
- public module name;
- one marker per exposed module, including empty modules and re-exports;
- occurrence namespace;
- exported occurrence name;
- GHC's export parent for bundled types, classes, constructors, methods, and
  fields;
- defining logical unit and defining module when available.

Normalization removes unit hashes, inplace IDs, compiler uniques, build paths,
and enumeration order. Namespace is retained so a type, constructor, field, and
value with related occurrence text are not conflated.

The normalized surface is compared as an exact multiset with
`tools/type-boundaries/api-surface.txt`. The wrapper's explicit `--write-surface`
mode writes a deterministically sorted candidate to a caller-selected path and
skips the unrelated Cabal-private probes. It is a review workflow, not an
acceptance mode.

The exact surface detects general leakage. Boundary-sensitive properties are also
kept as direct client probes so that the GHC renamer, type checker, instance
solver, and role solver remain the final authority.

## Probe model

Each probe has a stable ID, an existing fixture source, an optional CPP selector,
an expected success or failure, and diagnostic evidence required for a failure.
The runner gives every probe a unique module name and source path in its scratch
directory. CPP is enabled once for the session and a selector is defined in the
generated source itself, avoiding a per-probe session reconfiguration. The
runner extracts the selectors present in each fixture and requires an exact match
with the 94-case ledger plus the two Cabal-only selectors. It also requires every
Haskell source in the fixture directory to belong to that ledger, so adding,
dropping, or replacing a fixture or branch cannot be hidden by preserving only
the total count.

Negative probes must fail, and one error-severity diagnostic must both mention
all reviewed names and match the probe's rejection class. Positive probes use `GHC2024`, `-Wall`,
`-Werror=incomplete-patterns`, and `-Werror=missing-fields` and must load without
errors. Infrastructure failures, missing sources, preprocessing failures, and
unexpected diagnostics are violations rather than successful rejection tests.

The session clears its target graph between cases, uses unique module names, and
continues after `Failed` or `SourceError`. A log hook captures error diagnostics
per case with source-line carets disabled, so evidence must come from GHC's
message rather than echoed fixture text and cannot be assembled across unrelated
errors. All violations are accumulated and reported together.

The committed probe ledger and fixture-source/selector coverage checks determine
the exact current inventory. Private-sublibrary dependency resolution remains a
Cabal-level check rather than a simulated GHC import rejection. Counts belong to
the source-specific verification output, not an assumed historical total.

## Diagnostics

Diagnostics identify the stable case and distinguish three failures:

- a forbidden program unexpectedly typechecked;
- an allowed program unexpectedly failed;
- a forbidden program failed for an unrelated reason.

Surface drift is rendered as sorted additions and removals. Configuration errors
such as a missing package database or ambiguous unit are reported separately and
cannot satisfy a negative probe.

## Testing the checker

The checker package has unit and property tests for:

- deterministic and idempotent surface normalization;
- stable ordering independent of GHC enumeration order;
- surface rendering and parsing round trips;
- fixture-module rewriting and CPP selector insertion;
- completeness of the positive and negative probe ledgers.

The end-to-end gate proves the same session continues after expected failures,
accepts the positive clients, matches the checked-in export surface and preserves
actual Cabal private-dependency rejection. Infrastructure failures cannot count as
successful negative probes.

Transition validation is reproducible from the committed client fixtures, which:

- attempt to construct values through hidden constructors and fields;
- attempt to import hidden modules and depend on private sublibraries;
- request unavailable `Generic` dictionaries for opaque startup carriers;
- attempt forbidden coercions across nominal roles;
- conflate claims and DTOs that must remain distinct;
- exhaustively consume reviewed public sums, so a new constructor fails the
  positive client until it is reviewed.

Run `scripts/check-type-boundaries.sh` after the configured build. Its semantic
runner is `scripts/check-semantic-type-boundaries.sh`. Candidate surface generation
is an explicit review action; ordinary checking never rewrites expected policy.
