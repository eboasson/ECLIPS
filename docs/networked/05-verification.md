# Verification contract

Property tests are part of the implementation. This document defines obligations;
it does not certify that an arbitrary source revision passed them. The
[verification guide](../verification/README.md) identifies current entry points,
owner contracts and evidence rules. Build and resource policy is in
[BUILDING.md](../../BUILDING.md).

## 1. Verification model

The implementation exposes concrete pure transitions:

```text
stepHerald(input, state) -> state + effects
stepOracle(envelope, state) -> state + classified outcome + effects
stepRaft(input, machine state) -> machine state + effects
```

A deterministic test network may hold several private machine states, simulated
TCP buffers, explicit clocks and a schedule. It may delay, duplicate, fragment,
reconnect and reorder work where the protocol permits. It MUST NOT inspect one
machine's state to decide a transition in another, copy another projection, or
synthesize readiness by scanning all queues. Cross-machine facts use specified
messages. Independent reference models compare observable contracts without
becoming production owners.

A randomized failure reports seed, minimized typed inputs, connection/time
observations, relevant identities/indices and the differing invariant or result.
Tests distinguish semantic failures from fixture injection failures, missing
prerequisites and host resource failures. Passing reruns do not establish the cause
of an unreproduced failure or convert a failed original run into a pass.

## 2. Abstraction and kernel laws

Reusable semantic, ordering, validation, identity and protocol abstractions state
their laws. Every current instance runs the shared generated suite and an owning
component property. Generic derivation does not replace owner transitions,
admission tests or canonical digest vectors.

Pure client, Herald, Oracle and Raft packages import no socket, clock, entropy,
STM, concurrency, storage or effects-library implementation. Equal checked state
and typed input yield equal successor/effects. Opaque checked constructors and
nominal types prevent bypassing admission through record update, `Generic` or
coercion. The compiled public surface and downstream probes complement the
source/module dependency checker.

Tests exercise benign malformed inputs and reachable failures. They do not invent
recovery for forged checked internal state, Byzantine peers, resource ceilings or
counter exhaustion outside the finite-run profile. A contradiction of admitted
single-owner invariants is a fault rather than a normal recovery branch.

## 3. Semantic domain and Store properties

Canonical descriptors, values, identifiers and digest transcripts have round-trip,
normalization, validity and fixed-vector tests. Equal sort descriptors yield equal
public `SortId`s across application/domain compilation. Generation boundaries and
closed catalogue shape are checked without arbitrary data-size limits.

Required generated contracts include:

- deterministic total winner order, exact duplicate behavior, hidden retained
  winner/suppression and visible-state-only `local-take`;
- immutable endpoint sorts and structural meanings, sort occurrence retirement
  and correct control floors for equal redefinition;
- universal directed edges with preserving/weakening strength, sort matching at
  nablas/deltas, frozen publication destinations and no relay through the Oracle;
- capability derivation from checked Normal possession, absence of Normal grants
  from Weak copies, and correct removal/End effects;
- complete context snapshot plus revision changes, arbitrary local takes and
  retained suppression, continuing weak/normal context and path changes; and
- terminal deletion/disappearance with no stale resurrection or cross-occurrence
  data, authority or certificate reuse.

Graphs and context have independent finite reference derivations. Optimized or
cached results must agree with full recomputation on generated reachable states.

## 4. Application identity, startup and calls

The public surface has exactly `newid`, `newenv`, `write`, `forward`, `read`,
`local-take`, `wait` and `label`. Lifecycle and request bookkeeping are separate
from that union. Public typed handles bind the admitted sort and session namespace;
DTO construction cannot mint checked endpoint evidence.

Tests cover stable private/global bijection, recursive translation, unknown local
identity rejection, no cross-process alias meaning and public `SortId` stability.
Both `newid` forms allocate once; retry never regenerates. Controlled reservation,
first publication and `DeleteReserved` have one admitted winner. Delete input
cannot become a stored label, query value, peer item or Oracle command.

Startup properties cover arbitrary selected shape, duplicate-key rejection,
coherent aliases, checked copied possession, process nameability, fixed authorized
environment source pairs, local/remote preparation, exact initial claim,
claim/cancel/End races and current required-endpoint readiness. Parent End does not
implicitly end a child. Prepared logical processes may become operational before
an OS child exists. A moved delta gets a fresh Store; grants do not copy its data.

`newenv` properties cover its complete connected 31-object shape, phase-qualified
source authority, system-view-only construction routing, retry-stable identities,
local description seeding and completion through descendant membership bases.
Normal End waits for accepted construction; forced End cannot authorize an
unpositioned suffix. The prototype's universal edges do not establish the paper's
sort-filter confinement claim.

RPC/client properties cover exact duplicate/conflict classification, retained
pending/terminal status, result lookup, session/reply retirement, disconnect and
unknown outcomes. Typed result decoding belongs to the original call. Await,
status and cancellation never resubmit a mutation under a fresh identity. Wait
cancellation retains its one wake/cancel winner. Accepted semantic work survives
loss of its reply route or waiting caller.

## 5. Publication and logical-stream properties

Publication identity, frozen route and semantic digest bind immutable typed facts.
Structural staging holds accepted work without inventing a sequence; prerequisites
permit stamping, and structural completion requires a covering installed cut.
Ordinary publication does not wait for remote acknowledgements.

Logical-stream properties cover positive sequence allocation, explicit empty
prefixes, sparse receipt/completion sets, exact gap summaries, bidirectional resume,
received versus completed acknowledgement, and every retained retransmission.
Received acknowledgement alone does not release an active payload. Completion
releases only downstream-certified work and preserves required assignment facts.
Physical write success never means semantic delivery or Store application.

Peer work has one current logical attempt per stream, generation-qualified
completion and no lost wakeup across enqueue/outcome/ack schedules. Reconnect does
not reassign stable semantic identity or recompute frozen routes. Membership
retirement disposes only obligations whose exact incarnation/lifetime has ended.

## 6. Structural cuts, alignment and membership

Applied vectors, canonical cut identity and certified predecessor chains have
independent derivation and conflict/replay properties. A structural applied report
proves complete finite local consequences; it does not prove remote context is
ready. Topology and placement evidence use the captured membership and complete
member-qualified vector.

Alignment tests cover debt before its future cut exists, unsourced obligations,
exact supplier acceptance and certificate admission, immutable generation birth
facts, deterministic predecessor/fresh-base selection and retry identity. Plans
carry only the greatest dependency-closed unchanged set. Current-plan acceptance
is distinct from birth acceptance. Relation carry preserves owners and revision
progress; removed/re-added relations receive fresh owners. Invalidated history
cannot recreate work or block valid replacement indefinitely.

Input closure captures exact owner sets and revision lower bounds. Later data may
advance continuing streams without rewriting sealed receipts. Delayed old control,
Live, ack and route-marker traffic cannot reopen terminal owners. Exact
source-incarnation loss permits deterministic reselection; TCP churn alone does
not. Passive imported history creates no executable old-owner work.

Membership tests distinguish contacts, semantic members and voters. Discovery
cannot confer authority. Joining requires complete predecessor/newcomer evidence,
all-operation gate ownership, no early identity/possession grants, coherent
five-frame source admission, correct certified-cut/control-suffix selection,
receiver-owned system Stores and atomic activation. Repeated retirement may cross
unfinished successor bases. Old epochs never regain authority.

Reclamation properties cover exact pins, covered duplicate classification,
contiguous admission tails and their separate lifetimes. Source Activate
observation alone cannot release catch-up bytes; accepted applicant Hello must
cover activation. Replacement keeps the source import floor before local suffix
replay. Missing suffixes reject instead of guessing. Portable and in-memory
admission agree on successor/effects and reject incompatible valid nested frames.

## 7. Label and disappearance properties

The [label design](../architecture/labels.md) specifies the required full-pair
comparison, object generation, authority tenure and immutable outcome laws.
Generated schedules cover ABA, same-owner success, zero-generation first evidence,
canonical End normalization, intermediate historical observations, stale local
projections, cross-Herald comparisons and exact replay.

Accepted label settlement covers precisely the selected preceding publications;
qualifying later work remains held until local installation. Independent work
continues. Same-object pending success defers new decisions without receipt or
control allocation. Current-state installation preserves unrelated progress,
handles absent local payload and never installs a saved whole-owner copy.

Direct installation collection checks exact reporter/membership/outcome evidence,
collector reassignment and vacuous completion. Retirement does not change a
canonical success and later joins do not enlarge the captured set. Completion
frontiers account for all local consumers and prepared requests, including former
collectors. Request receipt release and semantic witness release are independent.

Predefined disappearance and regular-definition retirement require exact captured
publication/alignment cuts and all applicable negative evidence. Locally empty
Store state alone proves no global absence. Dependency-held pre-cut work blocks
reports, matching later work stays retained, stale reports cannot cross attempts,
and repeated terminal application is inert. Regular retirement preserves public
sort identity while closing only its exact occurrence.

## 8. Oracle and Raft properties

Oracle tests cover ordered canonical application, immutable receipts, exact retry,
control/log index distinction, process liveness, dynamic membership and voter
projection, label decisions and evidence-based disappearance. Ordinary publication
payloads and structural graph state do not enter the Oracle/Raft command path.

Raft properties cover one vote per term, log matching, current-term commitment,
leader no-op, ordered committed entries, explicit timers, stale reply correlation,
learners, log-effective stable/joint quorums and truncation restoration. Singleton
progress requires no fictitious remote reply. Two voters need both; one survivor
of three cannot commit or shrink itself. Native entries keep application bytes
opaque and configuration metadata sealed for ordered adapter admission.

Voter administration covers finite applied-prefix catch-up, accepted preparation,
joint/final commitment, cancellation only before surviving joint append, interrupted
owners and role changes independent of Herald lifetime. Failure-qualified exclusion
uses a captured old majority, fresh native health and retained accepted evidence.
Self-fencing does not invent a smaller quorum or prematurely kill a cohosted native
role. In-memory checkpoint/log reclamation is tested separately from deferred
durable restart.

## 9. Codec and runtime conformance

All five protocol families exercise generated DTO round trips, validating refined
carriers, exact complete-body decoding, wrong family/role rejection, split/coalesced
frames, truncated/malformed inputs and trailing bytes. Canonical semantic hashes
have their own golden vectors. A contract change updates every producer, consumer,
codec and vector together; no compatibility decoder or version negotiation exists.

Runtime tests prove exclusive kernel ownership, successor-before-effect order,
whole-batch handoff, independent fair lanes, retained intent across lane failure,
generation-filtered completions and scoped worker cleanup. STM contains queues and
small coordination facts, with I/O and substantial computation outside transactions.

Exact typed replay retains startup, seed, timing, diagnostic policy and normalized
input observations. Production and gated schedules compare the same causally
serialized stimulus; independent replay checks the pure trace. Wall-clock
watchdogs report harness failures, not semantic evidence. Optional redundant local
audits require mode-equivalence checks; disabling them never disables wire,
canonicality or semantic admission.

## 10. Independent-process coverage and review

Real TCP/OS workflows complement pure properties: founder startup, prepared children
on one or several Heralds, transitive seed joining, interrupted admission and fresh
epoch retry, native learner/voter changes, failed-voter retirement, isolation/drain,
continued context after writer/host loss and composed label/publication/alignment
traffic. The [failure corpus](../verification/failure-corpus.md) names deterministic
schedules; [hello-context](../../examples/hello-context/README.md) and
[hello-world](../../examples/hello-world/README.md) exercise ordinary public APIs.

Before accepting a change, review its concrete invariant, affected owners and
protocol planes, accompanying properties, reachable error classes and evidence.
New state and work must have one owner and a terminal/release path. Public export
changes require explicit surface review. Deferred features are not added merely
to generalize a helper. Verification output records the exact source, toolchain,
commands, resource envelope and failures; focused checks do not imply aggregate
release acceptance or a performance bound.
