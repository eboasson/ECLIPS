# Herald kernel and runtime composition

The Herald is a deterministic transition system whose state is privately owned by
one supervised runtime worker. [Ownership boundaries](../networked/02-boundaries.md)
are normative; this guide explains how a transition spans several pure owners
without creating shared mutable kernel state.

## Inputs, successors and effects

A normalized input carries an admitted application, peer, Oracle, administration,
clock or effect-completion observation. `stepHerald` returns either a typed fault
or one complete successor with an explicit effect batch. It performs no socket,
clock, entropy, storage, STM or thread operation.

The runtime installs the successor before handing its complete batch to dispatch.
It does not process another kernel input until that handoff succeeds. Independent
outbound lanes may then progress independently. An emitted, queued or TCP-written
effect is not a semantic acknowledgement. A correlated completion re-enters the
pure transition; wrong binding, attempt or timer generations cannot complete work.

Ordinary admission failures are typed public outcomes. A contradiction of checked
internal ownership is an invariant fault, not an invented recovery protocol.
Malformed wire input is rejected at its declared decoding/admission boundary.
The finite-run model has no resource-quota or counter-exhaustion result.

## Owner composition

| Owner | Exclusive responsibility |
| --- | --- |
| Application | Session/request lifecycle, process-private identity map, accepted call order, retained replies and startup claims |
| IdGenerator | Checked initialization seed, private generator prefix and next counter; no registry scan or runtime entropy inside a transition |
| Controlled | Object lifecycle, observations, possession, reservations, suppression and current operation eligibility |
| SortRegistry | Admitted definitions, occurrence identity and retirement floors |
| Store | Visible and retained state, exact incarnation receipts, revisions and continuing context history |
| Graph and structural Progress | Effective graph, admitted carrier meanings, applied vectors, cut certificates and membership-successor bases |
| Placement | Complete member-qualified placement snapshots and local resource correspondence |
| Publication | Stable accepted publication identity, process/Herald positions, frozen route and structural staging |
| PeerStream | Per-direction sequence, received/completed evidence, exact retransmission and physical-attempt correlation |
| PeerDelivery | Receipt-backed repair of alignment/cut/readiness evidence independently of publication-stream ownership |
| Alignment | Debt, checked plans, immutable generations, unsourced obligations, supplier attempts and subscriptions |
| LabelBarrier | Accepted selected-publication settlement, current-state installation, direct collection and completion |
| OracleClient and Projection | Exact intentions/results, canonical ordered facts, checked replay base, sparse evidence and applied progress |
| Discovery and Join | Contacts, peer bindings, restricted admission, frozen source material and admission tails |
| ProcessPreparation | Selected grants, retained child preparation, remote transfer and initial readiness |

A use-case coordinator composes narrow prepared transitions from the affected
owners. A failed preparation exposes no partial successor or effect. Inputs and
read ports are immutable concrete facts; they are not references to another
worker's private state. Checked representations are opaque or expose read-only
accessors, so record update cannot bypass admission.

Top-level transitions remain explicit while reusable internal abstractions may
factor repeated traversal, validation or state access. Such abstractions retain
owner and identity distinctions and have property tests for their laws.

## Application operations

Globalization/localization prepares a process-private/global bijection update
with the result which exposes it. An unknown private input rejects before any
semantic mutation. A private identity is stable and never rebound for the process
epoch. `SortId` is public content identity and does not pass through that map.

Bare `newid` atomically advances local and generator counters and records its
reply. Controlled `newid` additionally creates a pristine nabla/authority-bound
reservation. First publication and `DeleteReserved` have one local winner;
publication retains the object and frozen route, while deletion of an unused
reservation creates no value, peer item or Oracle command. Exact request retry
returns the original branch without another allocation.

Ordinary write/forward accepts a stable publication and routes directly to frozen
destinations. Structural publication may first be unsequenced while local
prerequisites are missing; only applied prerequisites allow a structural position.
Terminal structural completion waits for a covering installed topology cut, not
for recursively completed context alignment. Request receipt lifetime is separate
from already accepted publication work.

`newenv` retains one phase-qualified 31-object construction. Endpoint publication
establishes the authority needed for its hub/edge suffix; current authority is
resolved at that boundary. Aliases are exposed only after the complete cut and
local description seeding. [Environment construction](environment-hub.md) defines
normal and forced End and membership-retirement behavior.

`label` captures only its selected sequencing scope, settles it, submits one
canonical decision, installs against current owners and collects historical
installation evidence. [Labels](labels.md) separates generation, revision,
authority, canonical outcome and application completion. Unrelated work proceeds
while the retained label workflow waits.

## Structural progress and context

A checked structural occurrence retains its original semantic meaning and source
authority. Ordinary activity refresh may change local Store/activity consequences,
but cannot reinterpret that occurrence against a different current descriptor.
Ordered label/End/deletion controls are separate semantic transitions.

Applied vectors and canonical certificates establish exact graph cuts. Retirement
seals old sources through survivor evidence; admitted successor recipes permit
progress through repeated membership changes. Logical-stream completion means
finite local reconciliation, not completion of every remote context obligation.

[Alignment plans](alignment.md) distinguish current topology/placement from
immutable class birth cuts. Carry preserves unchanged incoming dependencies and
existing subscription owners. New generations select checked predecessor history
and fresh bases; a delayed certificate cannot change the generation identity.
Exact source-incarnation loss permits reselection; TCP churn does not.

## Retained work and scheduling

Accepted intent remains in its semantic owner. A runtime lane is never the sole
copy of a publication, Oracle proposal or application result. Pure work tickets
identify retained owner work and generation; physical scheduling changes eligibility
but does not manufacture semantic state. Stale tickets are inert.

The runtime uses STM for immutable envelopes and small coordination facts. It
performs decoding, hashing, substantial pure work, logging and blocking I/O outside
`atomically`. One reader and writer own each physical connection. Fair arbitration
makes ready ingress progress without depending on left-biased STM choice or
incidental thread scheduling. Per-connection order is preserved; cross-connection
races become explicit input order.

Logical timers carry generation and identity. Monotonic time is an explicit
runtime observation. Physical delay of a work ticket does not consume the kernel
clock stream or decide a semantic outcome. Runtime children have scoped lifetimes;
shutdown joins sockets, timers, dispatchers and workers. An unexpected owner fault
fails that epoch rather than restarting it with forgotten state.

## Replay and diagnostics

Exact typed replay retains checked startup, generator seed, contacts, timing and
selected diagnostic policy along with normalized inputs. Equal initial facts and
inputs produce equal state/effect traces. Independent concurrent events need not
have the same total order across runs; conformance compares causally serialized
schedules or explicitly normalized traces. Durable trace storage and old-epoch
restart require separate contracts.

Optional redundant local audits do not remove wire or semantic admission checks;
see [diagnostic audit policy](labels.md#diagnostic-audit-policy). Independent full
owner audits, reference derivations and real runtime tests complement the normal
transition path. [Control history](control-history.md) distinguishes semantic
bases, exact result pins and joining tails when raw archive bytes are reclaimed.
