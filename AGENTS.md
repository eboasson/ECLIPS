Instructions to Codex:

    - follow `BUILDING.md` and use the retained project-local GHC 9.14.2-rc1
      (reported version `9.14.1.20260728`) for builds, tests, profiling and
      Cabal-based checks. From the project root, source
      `. "$PWD/.cabal/profile-0.2/f01/env.sh"` in every shell invocation that
      uses the toolchain; environment changes do not persist between tool calls.
      Verify `ghc --numeric-version` and Cabal's compiler path before building.
      If the local environment/compiler is missing or resolves differently,
      stop and report it; do not silently fall back to the system GHC;

    - invoke Cabal from the project root with
      `cabal --config-file="$PWD/.cabal/config" ...` so it uses the
      project-local configuration; `--config-file` is a global option and
      therefore must precede the Cabal subcommand;

    - for building and running tests, pass `-j4` to cabal

    - follow `BUILDING.md` for heap limits and build/artifact retention: use
      768 MiB runtime heaps and the 2304 MiB compiler-only allowance; keep the
      agreed 1024 MiB prepared-child diagnostic override explicit. Reuse build
      trees, retain only the builds/comparison inputs still needed, and after
      substantial work report exact paths and measured sizes of artifacts that
      can safely be deleted. Preserve the local RC toolchain, package store and
      active build inputs; do not accumulate a build or binary copy per run;

    - the repository implements the modular networked specification in
      `docs/networked`; keep maintained architecture and build instructions
      independent of development-history records;

    - keep the Semantic, Herald, Oracle, and Raft transition kernels pure and
      deterministic, with TCP, clocks, entropy, storage, and concurrency interpreted
      at explicit runtime boundaries;

    - keep effects-library dependencies in runtime shells only; `effectful` is the
      default candidate for the first implementation, while kernel inputs, outputs,
      state, and effect descriptions remain concrete pure data;

    - when runtime work is concurrent, use STM for queues and small
      coordination facts; one supervised worker owns each kernel state privately,
      and no other thread reads or mutates that state;

    - use GHC2024 and appropriate typed abstractions to make invariants, ownership,
      and composition legible; prefer lawful reusable abstractions with property
      tests over repeated hand-written plumbing;

    - prefer concise semantic record labels for private and package-internal data;
      use `DuplicateRecordFields`, `NoFieldSelectors`, and `OverloadedRecordDot`
      selectively where they improve clarity. Keep public constructible fields
      qualified when repeated labels would make ordinary updates ambiguous, and
      keep checked opaque representations positional or hide their labels behind
      explicit read accessors so record update cannot bypass admission;

    - keep top-level state transitions explicit, but do not interpret that as a ban
      on higher-order functions, type classes, optics, generic derivation, or other
      type-directed internal composition;

    - add networking only through the TCP/protocol/runtime boundaries specified in
      `docs/networked`; durable recovery, DDS compatibility, and performance-oriented
      incremental graph algorithms remain deferred;

    - profile 0.1 assumes a benign, non-Byzantine deployment and non-malicious
      applications. Applications may still contain bugs, so malformed wire data,
      invalid calls, duplicates, reconnects, and reachable ordering or runtime
      failures must be handled at their specified boundaries. Do not add Byzantine
      defenses, adversarial-client isolation, recovery for forged checked internal
      state, or defensive branches for states and races made impossible by pure
      admission and single-owner kernel serialization; prove those invariants with
      types and property tests, and treat an actually observed contradiction as an
      invariant fault rather than designing a production recovery protocol;

    - profile-0.1 runs are finite and assumed to remain far below exhaustion of
      every unsigned 64-bit counter. Counter exhaustion and wrap-around are outside
      the prototype model: do not add capacity outcomes, rollover behavior, or
      boundary/property tests for them;

    - profile-0.1 inputs and retained work are assumed to fit the available
      machine resources. Do not add byte, text, collection, nesting, node-count,
      message, batch, queue-capacity, or similar resource ceilings, rejection
      outcomes, or boundary/property tests. Keep semantic shape checks such as
      exact identifier widths, required record fields, closed catalogue cardinality,
      and canonical encoding rules;

    - prototype edges are universal: do not add a sort-filter field, encode a
      sort filter on the wire, or add a filtering branch; nabla/delta sort matching
      remains required, and sort-restricted edges are deferred;

    - the experimental prototype carries no API or protocol versioning or
      compatibility guarantee: do not add API/protocol schema-version fields,
      supported-version lists, negotiation, compatibility decoders/adapters,
      migrations, or mixed-revision operation; change all producers, consumers,
      codecs, golden vectors, and tests together, and start a fresh experimental
      run after an incompatible change;

    - property tests are part of the deliverable.
