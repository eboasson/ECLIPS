# ECLIPS Oracle client protocol

This package owns the pure current-build EORC wire boundary. It contains exact-
width structural claims, protocol-owned canonical-byte DTOs, the current client
and server messages used by the control plane, generic `Binary` body
encoding, and a stateful ingress decoder that composes incremental EORC framing
with direction, Hello phase, deployment, and terminal-absence authority admission.
For an ordinary command/watch lane, the first Hello binds immutable system,
predefined catalogue, configuration, and
initial projection claims before establishing a lane. Future process identities
are excluded from those commitments; there is no live configured-process
catalogue claim. Minimal dynamic Starts travel through the canonical Oracle
adapter and retain exact request identity across retries.

The generic submission DTO carries the complete Oracle command vocabulary:
process Start and End; atomic label decision and collected completion;
failure-probe Open, Report, Dismiss, and Retire; disappearance Open, Report,
Invalidate, Resolve, and authorized Abort for both controlled objects and regular
sort-definition occurrences; Herald admission and voter administration; and
combined receipt and label-completion progress. Canonical commands, receipts,
event vectors, and applied entries remain owned by
`eclips-oracle-core`; EORC does not define a second semantic representation.
`OracleSubmissionDeferred` reports an effect-free label-decision deferral while
an earlier decision for the same object awaits installation completion. It
carries the exact request, blocking label decision, and observed applied prefix,
but is neither a receipt nor a watch entry and consumes no control index.
`OracleSubmissionNotReady` is the correlated transient leader-busy disposition:
it leaves the established lane usable and names the exact retained request whose
submission may be reoffered after a client-owned delay. It is deliberately not a
redirect and carries no leader hint.

An ordinary canonical envelope can carry its home's combined progress: the
receipt released set (high water with unresolved exceptions) and the original
label-decision index through which no further completion work can be offered.
The components join independently. This metadata is excluded from the command
digest, so a newer promise preserves exact semantic retry identity. A fresh
command applies progress and its ordinary result in one control entry, whose
canonical watch representation confirms progress. A duplicate or conflict can
still advance progress through one maintenance watch entry while keeping its
original semantic result. An ordinary receipt alone does not confirm progress.

A standalone canonical maintenance command covers the remaining idle tail; its
request sequence names the receipt high water of its home Herald epoch. It
allocates no ordinary request sequence or receipt. `OracleProgressRetired`
correlates that identity with the full effective committed progress snapshot.
`OracleProgressNotReady` echoes the exact full offered snapshot with its term,
so an older offer cannot release a newer one sharing the same receipt high
water. These dispositions remain distinct from ordinary requests sharing that
identity. Duplicate or stale progress allocates no further control entry.

`OracleRequestRetired` answers obsolete submissions and result queries with
either a retired receipt prefix or an irrevocably retired home. It is distinct
from absence: the command must not execute again, and the old exact result and
conflict guarantees have ended. Committed retirement facts need no terminal
absence authority. Standalone advancing progress also travels through the
normal ordered watch as a canonical entry with no ordinary receipt or semantic
events.

Runtime readers use `OracleIngressDecoder`. A client lane starts without absence
authority, retains its exact accepted Hello tuple, and admits terminal
`OracleRequestAbsent` only after an out-of-band service-ready-leader update matches
that node, term, and complete applied prefix. Mismatched updates clear earlier
authority, while a redirect terminates the lane so the caller follows it with a
fresh connection; the wire `serviceReady` field is not itself authority. Outbound
committed-entry batches are constructible only relative to the requested exclusive
cursor, while wire decoding independently repeats non-empty and internal-
contiguity checks.

Canonical Oracle envelopes, receipts, and applied entries remain owned by
`eclips-oracle-core`. Named adapters validate their byte DTOs through that core;
this package defines no `Binary` instance for a core or domain value. Likewise,
Raft and domain identities are converted only at explicit checked adapters.

Native-health queries use a separate one-shot lane. They carry checked run and
source claims plus a fresh round correlation; the response describes the native
owner's term and effective voting configuration. Health observations neither
establish a command lane nor replace committed configuration or failure evidence.

The package performs no I/O or concurrency and imports no Herald or runtime
module. Process/operator administration uses EADM; EORC carries the canonical
Oracle commands resulting from those workflows. There is no workflow-status RPC,
ordinary-publication, version, negotiation, compatibility, queue, timer, or
physical-binding wire field. See the [label architecture](../docs/architecture/labels.md)
and [control-history design](../docs/architecture/control-history.md) for the
semantics carried by the canonical values.

The schema is an internal current-build contract, not a stable public protocol.
Every producer, consumer, codec, fixture, and test changes together. This package
is available under the MIT License; see this package's `LICENSE` file.
