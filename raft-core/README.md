# ECLIPS Raft core

`eclips-raft-core` is the pure, domain-independent Raft kernel used by the Oracle
runtime. It replicates opaque application bytes, an internal leader no-op, and
stable/joint voting configurations with opaque adapter metadata. It imports no ECLIPS semantic, Oracle, protocol,
runtime, socket, clock, entropy, STM, or storage module.

The public boundary contains:

- exact-width opaque voter identities and nominal terms, log positions,
  proposal correlations, durations, and logical generations;
- checked native genesis containing the local node, immutable initial voters,
  and heartbeat/election timing configuration, with separate voter and learner
  admission;
- checked canonical nonempty voter sets, stable/joint configurations and exact
  genesis or log-entry configuration references;
- structurally checked RequestVote, AppendEntries and InstallSnapshot RPCs, explicit timer and
  proposal inputs, and local response-generation correlation that never goes on
  the wire;
- an opaque state value changed only by `stepRaft`; and
- one sealed effect batch containing RPC, timer, proposal-status, committed-entry,
  and diagnostic effects.

`stepRaft` requires equality only for the otherwise opaque application value so
an equal-index/equal-term entry must also have the same tag and payload. RPC
admission rejects term histories that exceed the sender term or decrease across
the supplied prefix. Exact retransmissions of one outstanding request keep its
runtime-local dispatch generation; a changed log expectation receives a fresh
generation, so heartbeat retries cannot starve a slower benign round trip.

The opaque log validates contiguity, term order and configuration transitions
when entries are appended or a conflicting suffix is replaced. It retains a
configuration-entry index for effective and committed lookups. Timers and adapter
acknowledgements reuse that admitted evidence; they do not scan application
history. `auditRaftState` provides an explicit exhaustive diagnostic and property
audit, including agreement between the index and the complete retained log.

Every nonempty voter set is supported. Election and commitment use the latest
configuration in the local log: stable sets require a strict majority, while
joint sets require a majority of each constituent set. Committed configuration
is a separate projection, and suffix replacement restores the preceding
effective configuration. A self-excluding leader replicates final commitment
before relinquishing leadership. Losing peers never reduces the required quorum.

Registered learners replicate the retained suffix or install a native checkpoint without contributing votes
or acknowledgements to a quorum. Preparing a change captures an exact settled
frontier and gates application proposals until promoted learners both match it
and acknowledge its complete adapter prefix. Readiness is invalidated by a
leadership change. After the joint entry commits and is applied, its final stable
entry can be proposed. Native cancellation applies only before joint append;
retained administration and cancellation outcomes belong to the Oracle owner.

The ordinary committed stream includes every retained no-op, application and configuration position
with its index and term. Adapter acknowledgements advance one position at a time;
service readiness requires the current-term no-op and the entire committed prefix
to be applied. A snapshot installation is the explicit exception to incremental
acknowledgements: the application first installs the checkpoint state, then
acknowledges its complete native prefix.

`checkpointRaftApplication` pairs actual application checkpoint bytes with an
already acknowledged log index. Raft derives its term and effective configuration
reference from its own log, replaces the prefix with one base position, and keeps
the suffix and one current checkpoint. A pending configuration-preparation
frontier remains available until that work completes. Checkpoint capture is an
explicit application-owner handoff, without a timer or history-size threshold.

Every successful local offer emits `ReportRaftCheckpointOffer offered retained`.
`retained` is the actual successor checkpoint, including its original immutable
payload; an equal or older offer acknowledges the existing image, even when the
offered bytes differ. An offer at the initial zero base reports `Nothing` until
an image exists. An offer beyond the acknowledged application prefix fails
without producing a batch. Configuration preparation captures the fully applied
tip and prevents ordinary proposal growth; a newer offer during that preparation
therefore remains unapplied and is rejected.

The report shares the retained payload without copying its bytes. Interpreting
whole effect batches in FIFO order makes the report a fence after previously
emitted snapshot recipes have been interpreted. It is not acknowledgement of
remote snapshot installation or network consumption. Runtime owners must also
account for any materialized payloads and outstanding transfers before releasing
their corresponding resources. An adapter waiting inside a dispatched apply
batch must finish that batch rather than wait for its checkpoint-offer report,
which is queued behind it.

A follower behind the base receives InstallSnapshot. A matching suffix remains;
an incompatible uncommitted suffix is discarded. The native owner marks an
application installation pending and emits `InstallRaftCheckpoint` before its
successful RPC response. The runtime must install actual application state and
acknowledge the checkpoint before sending that success. A metadata-only
observation does not satisfy this contract. Newer snapshots wait while one is
being installed; stale or duplicate snapshots never roll back applied state.
Stable and joint configurations retain their exact reference across compaction,
and elections continue to compare the base/suffix index and term.

Recent-leader protection expires through a generation-qualified timer. Followers
renew it on valid leader contact; leaders require fresh effective-quorum
acknowledgements. Sending heartbeats alone cannot renew a multi-voter guard.
A singleton's explicit local acknowledgement is itself a complete fresh quorum.
Replies dispatched before the previous renewal cannot renew the guard again.
An incomplete quorum observed without protection has its own minimum-election
timeout; insufficient replies cannot extend that window or retain evidence
indefinitely for a later quorum.

Applications cannot construct a `RaftEffectBatch`. The runtime owner protocol is
still essential: adopt the complete successor first, then hand the returned
sealed batch to the dispatcher as one value, and only then select another input.
Batch opacity prevents fabrication but does not itself enforce that scheduling
order.

The implementation is deliberately in-memory. A failed owner is not restarted in
the experimental run, and there is no persistence bit, stable store, durable
restart, capacity outcome, counter rollover behavior,
protocol version, or compatibility surface.

This is an internal prototype interface, not a stabilized API. Incompatible
changes update the core, adapters, codecs, fixtures, and tests together for a
fresh experimental run.

From the repository root, after the package is present in `cabal.project`, run:

```sh
cabal --config-file="$PWD/.cabal/config" test -j4 eclips-raft-core:raft-core-properties
```
