# ECLIPS administration protocol

`eclips-protocol-admin` is the pure wire boundary for the externally
callable Herald-administration family. A client can identify its deployment
role, request a fresh dynamic process with an exact correlation, explicitly end one process
epoch, explicitly cancel an unclaimed child preparation, and query the retained
correlation result. It also exposes operator status, preparation inspection, and
ordered Herald drain.

Voter administration provides `PrepareOracleReplica`, `BeginVoterChange`, `CancelVoterChange`,
`GetOracleConfiguration`, and `GetVoterChangeStatus`. Preparation names the local
Herald only; its runtime supplies concrete native identity and endpoint facts.
Missing endpoint configuration returns an explicit local administration refusal.
Begin carries the expected configuration, commission/demotion reason, and full
desired node/host binding set. The original canonical Oracle receipt remains the
mutation result; separately queried canonical configuration/change projections
carry the local applied control prefix and current workflow progress.

The closed server vocabulary reports `AdminAccepted`, one of the three terminal
`StartProcessReady`, `StartProcessEndedBeforeAttachment`, or `ProcessEpochEnded`
results, or a command-specific Start/End Oracle
rejection. Unknown correlations are nonbinding and conflicting typed reuse is
explicit. A connection ending has no process-lifecycle meaning.
`GetHeraldStatus` reports lineage, local epoch, current membership/generation,
control index, voter hosts, service phase, connected Oracle and leader hint.
`ListChildPreparations` reports stable references, global process epochs when
allocated, and current preparation phases. Both remain available for terminal
status inspection without admitting application or mutation work.
`DrainHerald` retains its correlation and uses the existing semantic drain owner;
`HeraldDrainAccepted` precedes `HeraldDrained`, and the final live-lane reply is
flushed before listener teardown.

`CancelChildPreparation` uses the checked opaque preparation handle and retains
its exact administration correlation. `AdminPreparationCancellation` carries the
shared lifecycle pending keys, cancellation or AlreadyAttached result, or typed
failure. The kernel observes the same cancellation owner used by the parent;
administration does not create an independent cleanup End. Exact retry and
queries on the same connection cannot change the retained preparation or terminal result.

Nominal deployment, process, Herald-epoch, and attachment claims are
checked as exactly 32 bytes during construction and decoding. These checks prove
wire shape only; authorization, residence, correlation ownership, Oracle
workflow, selected access installation, and attachment admission belong to the
Herald kernel adapter.

The package owns current-build serialization and fixed `EADM` framing over
`eclips-protocol-frame`. The End command carries an administration-specific
singleton reason, encoded as the Domain-owned canonical
`ExplicitAdministrativeEnd` value. Its checked decoder rejects every other
Domain reason, so resident application-loss inference cannot be constructed over
EADM. The package reuses the pure Oracle canonical codecs for voter replies and
has no runtime, socket, clock, concurrency, or Herald dependency. `eclips-herald-runtime`
interprets this exported boundary over a dedicated authorized administration
lane. Each accepted `AdminHello` allocates a fresh binding generation and ends
the previous external delivery lifetime. A correlation belongs to that binding;
a new connection cannot query, conflict with, or retire an older binding's work.
The private orderly-drain lane remains separate.

`AdminWithRetirement progress request` carries high-water plus unresolved
exceptions from `Eclips.Public.Types.ReceiptRetirement`. `RetireAdminReceipts`
flushes the same progress while idle. Only a single established operation may
be wrapped; handshake and nested maintenance forms are rejected by decoding.
The Herald joins release sets, checks every selected result is terminal, and
removes receipts before admitting attached work. An unresolved selected result
returns `AdminRetirementNotReady` without changing state or executing the work.
`AdminReceiptsRetired` acknowledges the exact merged progress. `AdminResultRetired`
answers stale calls and result queries without reexecution.

A lost or superseded connection relinquishes all its result delivery interests.
Pending operations finish under their existing semantic owners, then their RPC
records are discarded. Completed Start results leave one compact process-origin
and attachment record so receipt retirement cannot erase process provenance or
live application access. End and preparation semantics remain independently
owned. The executable operator waits for its terminal reply, flushes retirement,
and closes its connection.
