# ECLIPS application protocol

This package contains the pure application wire boundary:

- opaque, structurally checked application claims;
- the one current closed client/server DTO envelope;
- lane-local `Ping`/`Pong` heartbeat correlations consumed by physical owners;
- session-targeted terminal dispositions for session loss, Herald isolation, and
  retirement;
- generic current-build `Binary` serialization with complete-body admission; and
- fixed `EAPP` framing, built on the family-neutral `eclips-protocol-frame`
  envelope, with a pure prefix-preserving incremental decoder.

The package grants no session or identity authority and has no Domain, Herald,
runtime, networking, concurrency, clock, entropy, or persistence dependency.
Stateful admission belongs to the Herald RPC adapter; TCP interpretation belongs
to the separate application and Herald runtime shells.

The payload representation is deliberately not a compatibility contract. All
participants in one experimental run use the same build, and schema changes
replace producers, consumers, and tests together.

The current `Call` payload carries exactly eight operations: newid, write,
forward, read, local take, wait, label, and private-environment creation. Write
carries the sole generic `PublishValue` arm or reservation deletion. A structural
write or forward may retain `OperationAccepted StructuralStabilizationPending`, a
label call may retain `OperationAccepted LabelSettlementPending`, and a `newenv`
call may retain `OperationAccepted EnvironmentStabilizationPending`, before its
operation-specific terminal result. The environment terminal carries only its
opaque, canonically checked `EnvironmentAccess`: six endpoint pairs, their hub
and eighteen supporting edge handles, all distinct private identities. It is
distinct from the
process-bearing startup bundle, which also retains Herald-verified carried
sorts for endpoint entries and object sorts for normal object entries. These phases carry no publication or global
identity. Generated identities and label targets remain represented at this
boundary only by process-private operands and results.


A separate `LifecycleCall` arm carries preparation, readiness, cancellation, and
own-process End under full session-qualified request identities. Live result
queries stay session-scoped. `RecoverLifecycleResult` uses the original attachment
and exact request identity only to query a still-retained own-End result after session
and private-map retirement; it cannot submit work or open a session.

Prepared connection uses `ClaimInitial` with an opaque target/lineage/epoch-bound
descriptor and a stable, independent initial-claim identity. Pending replies name
the required keys; rejection is typed. Successful and exact retry admission reuse
`SessionOpened`; an empty `RetireReceipts` on the same connection confirms its
receipt. Once established, physical connection loss ends the session. The
existing genesis Open arm remains distinct. Codec admission checks descriptor and
identity representation only; it does not establish semantic readiness or grants.

`CallWithRetirement` and `LifecycleCallWithRetirement` carry independent inclusive
ordinary and lifecycle high-water marks with unresolved exceptions beside the
semantic request body.
`RetireReceipts` flushes the same progress while idle; `ReceiptsRetired` confirms
it. These announcements join by union of released identities and never change request
identity. `RequestRetired` and `LifecycleRetired` distinguish reclaimed receipt
history from a request that was never admitted. Request zero is valid, so absent
high-water marks are represented explicitly. Exception IDs must be at or below
their high-water mark and encode in strictly ascending order; malformed or
duplicate exception lists are rejected by the shared checked Binary decoder.
