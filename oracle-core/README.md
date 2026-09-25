# eclips-oracle-core

`eclips-oracle-core` is the pure, deterministic Oracle kernel. Its closed command
vocabulary owns dynamic Start and explicit End; atomic label decisions and
collected completion; Herald admission; replica registration and voter changes;
failure-probe Open, Report, accepted voter-host failure, Dismiss and Retire;
disappearance Open, Report, Invalidate, Resolve and authorized Abort; and
receipt/label-completion progress. Ordinary admitted client requests record an
immutable receipt and an ordered vector of zero or more projection events.
Maintenance progress and sealed native configuration entries have their own
ordered application rules without ordinary client receipts.

Oracle genesis retains immutable system/catalogue/configuration commitments,
checked index-zero applied bootstraps, initial topology, active Herald membership,
and Oracle/Raft co-host bindings. No future process catalogue is retained or hashed.
A Start carries checked fresh process and epoch identities plus an active residence;
Oracle rejects identity reuse, including after End. Its projection binds the Start
control index and request identity. Native voter sets may have any nonempty
cardinality; co-host bindings remain injective and exactly match the configuration.
Failure probes capture the current checked stable voter configuration and use
a strict majority of its voter hosts, including the failed target in the captured
denominator.

An accepted Start does **not** create a root publication, value, label, authority
epoch, Graph fact, Placement fact, or Store fact. Explicit Herald application operations create those structural effects after
the process is live. In particular,
the dynamic Start projection deliberately does not reuse the authority-bearing
`AppliedProcessBootstrap` shape reserved for checked genesis.

The package performs no networking, consensus, storage, clock, entropy, or
concurrency effects. Raft commits canonical envelope bytes; a runtime adapter
applies them in log order and interprets the returned effect batch. Every
first-seen ordinary request admitted to the replicated transition consumes one
control index and watch entry, including semantic rejections. Exact duplicates
return their retained receipt; conflicting request-ID reuse produces a
deterministic protocol disposition. An advancing combined progress offer can
still create a maintenance entry without re-executing the semantic command.
Same-object label deferral is a pure preflight observation and remains outside
the replicated transition; End remains independently orderable.

Canonical state hashing is factored through a package-private,
state-parameterized seam. Each state owner supplies its complete canonical
transcript bytes and a digest installer; the seam hashes those bytes without
another encoding layer. Canonical properties pin the closed command families,
the outer semantic rejection arms (including the complete typed disappearance rejection payload),
projection-event arms, and representative state/workflow transcripts.

Labels compare the full expected owner/generation pair at one Oracle decision.
A successful decision fixes its result and captured membership atomically;
Heralds install it and collect installation reports before one checked completion
attestation produces `LabelWorkflowCompleted`. A NotApplied decision needs no
installation collection. Conflicting live work for the same object can be deferred
outside replication without allocating a control index. See the
[label architecture](../docs/architecture/labels.md) for the complete contract.

Completed decisions retain compact identity, decision-index, membership-generation
and outcome-digest witnesses. Receipt and label-completion progress have separate
retirement frontiers. Advancing either frontier cannot discard evidence still
needed by another semantic event in the same canonical entry. Retired requests
cannot execute again, and retirement ends the old exact-result/conflict promise.
The [control-history design](../docs/architecture/control-history.md) describes
these retention boundaries.

A policy-private executable reference model specifies focused failure-probe
schedules. Its opaque Open, Report, Dismiss, and Retire
values have independent canonical command/envelope/state codecs; checked state
decoding replays retained requests from checked genesis rather than trusting
serialized derived state. The model uses the immutable Raft voter-host set for a
strict reporting majority, permits one non-voter retirement, advances one
canonical Herald-membership generation, supersedes competing predecessor probes,
and emits sorted resident-process End events. It is an independent cross-package
property oracle for this restricted schedule,
not the live owner or a model of every admission, repeated-retirement and native
voter-change path.

The separate `WorkflowReference` model applies one
checked membership retirement to captured label work: pre-release work becomes
NotApplied, while a released value completes over the successor reporter set.
This is a restricted reference schedule. The live label command family is the
atomic decision/completion family described above; reference phase names do not
define additional live commands.

The live disappearance family belongs to the sole Oracle kernel. Open,
Report, Invalidate, Resolve, and authorized Abort cover controlled predefined
objects and regular sort-definition occurrences. The private disappearance leaf
owns probe history, its collecting index, terminal controlled-deletion facts,
and regular retirement indexes; the enclosing Oracle retains the sole request
ledger, control index, label workflows, and membership authority. Equal racing
Opens return the original probe in receipts and complete projected headers.

A successful atomic label decision precedes its ordered disappearance
invalidations in the same applied entry. Herald retirement preserves the existing
membership/failure prefix, then aborts collecting predecessor probes before
resident-process and label-workflow consequences. A controlled Resolve makes a
later label decision record the corresponding NotApplied outcome.

The independent disappearance reference lives only in property-test sources.
Generated traces compare live receipts, projection claims, probe history, and
retirement indexes at every prefix, while the live canonical envelopes, receipts,
event vectors, and applied entries are decoded and re-encoded through their
checked boundaries. EORC carries the complete current language in canonical
byte DTOs.

This is an experimental prototype package. Its current-build API and canonical
schema change in lockstep with all consumers; it provides no compatibility
aliases or version negotiation.

Replica registrations and explicit voter-change intent are retained in the same
Oracle state. Checked complete bindings share the membership-change slot with
admission and retirement. Sealed native joint/final entries each consume one
ControlIndex, publish a configuration/change watch entry and preserve all client
receipts. Configuration application validates its predecessor, native reference,
bindings and canonical change metadata. The final entry completes an explicit
retained change; the earlier accepted command receipt remains immutable. Current stable
voter bindings determine failure reporters, while joint consensus gates new
failure probes. The failure workflow captures the exact old configuration and
direct-probe results in an opaque accepted-failure certificate. Acceptance atomically owns the shared
change slot. Native exclusion enters `VoterExcludedAwaitingHeraldRetirement`;
semantic retirement consumes that original certificate and completes the intent.
It never requires replacement evidence from the smaller successor configuration.
