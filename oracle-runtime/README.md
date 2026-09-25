# eclips-oracle-runtime

This package is the runtime boundary for the pure Oracle and Raft kernels.
Its Oracle composition supports registered learners, retained explicit voter
changes and native joint consensus. One worker privately owns each kernel state, a third
worker privately owns the contiguous watch ledger, and immutable batches cross
between them through STM queues. TCP, physical timers, entropy, request
correlation, recording, and scoped concurrency are interpreted here.

Runtime diagnostics are streamed without retaining their lifetime history by
default. Conformance scenarios that inspect exact event history explicitly use
`configureOracleRuntimeRecording OracleCaptureHistory`; ordinary replicas keep
`OracleDiagnosticsOnly`. Batch counts are evaluated before queueing, and completed
TCP workers leave the live shutdown registry after releasing their sockets.

Full Oracle state digests are disabled by default. To retain this diagnostic
integrity witness, apply
`configureOracleRuntimeStateDigest OracleStateDigestEnabled` to the runtime
configuration before startup. Use the same mode for every replica, including
learners started later. The mode remains fixed for the run, and checkpoint
installation rejects a different mode. Enabled mode hashes the complete
canonical state after each advancing transition and verifies checkpoint digests;
disabled mode avoids that hashing while preserving canonical admission and all
operational command and workflow hashes. Checkpoint serialization still occurs
in either mode.

The Oracle owner caches one strict local checkpoint payload at its current
control index. Captures at the same index reuse those bytes, including their
local framing; advancing state or installing a remote checkpoint invalidates
the cache. Even a same-index installation invalidates it. The adapter still
publishes checkpoints at the required native Raft positions, so duplicate
commands and leader no-ops can compact their log positions without re-encoding
unchanged Oracle state. The existing optional work census reports encoded and
reused captures separately.

The profile-0.2 singleton composition uses these same owners, log, and adapter.
The sole voter elects itself and commits its current-term no-op before serving
ordinary Oracle commands. Ordinary submissions and conflicting reuse traverse Raft, while the watch ledger
publishes each first-seen control entry once. During captured learner preparation,
already-published receipts answer exact retries without disturbing the frontier. The
singleton runtime property exercises EORC submit, query, and watch without any
incoming ERFT request or response.

Receipt retirement follows the same replicated application path. After the
Oracle owner installs a committed step, it hands the adapter a concrete compact
summary of retired request prefixes and closed home epochs. The adapter removes
the corresponding published receipts at the same serialized boundary used for
query classification. Obsolete requests therefore receive a typed retired result
instead of an absence result, and delayed submissions cannot recreate receipt
history. Ordinary envelopes can carry their home's latest release frontier:
fresh command application and reclamation share one control entry, while a
duplicate or conflict that advances the prefix publishes a maintenance entry
without changing its semantic result. The watch entry confirms the frontier;
the ordinary receipt does not. In particular, retained-result service during
learner preparation may answer a retry without applying its optional frontier.
Standalone maintenance covers the idle tail. Its replay publishes no entry and
allocates no acknowledgement receipt. Maintenance has its own
correlated transient response because its envelope identity names an existing
request sequence. The Herald controls when its contiguous settled prefix can be
released; an Oracle reply alone does not make the receipt eligible.

This bounds Oracle receipt bookkeeping only when outstanding requests remain
bounded and their acknowledgement frontier advances. Herald semantic retry
records, the Raft log, watch history, and retired epoch catalogues have separate
lifetimes and are not reclaimed by this increment.

Every committed native entry arrives through a sealed ordered stream. The
adapter acknowledges no-ops as well as application entries, so its applied cursor
is an exact log prefix; no-ops consume no Oracle control index. Each configuration entry advances ControlIndex once after checked ordered Oracle
application, updates the retained configuration/change projection and publishes
one watch entry. It has no client request identity or synthetic receipt. Exact
metadata and native configuration references are checked together. A separate supervised, generation-qualified recent-leader timer
interprets the native election-disruption guard without sharing the Raft state.

`Eclips.Oracle.Runtime.TCP` launches the current ERFT/EORC composition.
`configureOracleTcpListeners` checks an exact per-node listener assignment;
explicit host/port binding and per-node exit observation support the supervised
deployment package. The loopback helper remains available for tests.
Its EORC server context is sourced from immutable checked Oracle genesis:
system, catalogue, configuration, and initial projection. The conformance client
sends these exact claims on each fresh or redirected Hello. Dynamic process
admissions require no recomputed deployment or future-process catalogue digest.
An authorized Hello for a leaderless/not-ready replica with no distinct leader
hint remains parked on that same connection. Its STM wait races an actionable
coordination change against peer readability: EOF or pipelined input retires the
lane, and shutdown closes it. This keeps election-time waiting from becoming a
client reconnect loop.

On POSIX, ERFT/EORC readers and writers use progress-accounted direct nonblocking
system calls. Only `EAGAIN`/`EWOULDBLOCK` waits on an IO-manager readiness token;
a one-second watchdog cancels and refreshes a token which has not fired, then
retries the system call. The leaderless pre-Hello wait uses the same periodic
registration while continuing to race the Oracle state transition in STM; after
a missed wake it probes with a non-consuming nonblocking `MSG_PEEK`, so queued
input or EOF is observed without stealing protocol bytes. A timeout never
interrupts a transfer, and each positive partial write advances the exact
retained suffix under masking. This is a pragmatic bounded-latency guard for a
rare lost readiness wake observed under the macOS/GHC test load. Windows retains
`network`'s blocking backend and does not claim this watchdog behavior.

An established watch worker either sends the owner-produced contiguous suffix or
closes its physical EORC lane. `OracleWatchUnavailable` is an ordinary unavailable
termination; failure to encode an owner-produced suffix is reported as a runtime
invariant fault before the same close. The runtime never leaves an established
lane with a terminated watch worker.

Each ERFT peer has one outbound FIFO shared across physical binding generations,
with at most one lease-owned writer claim. Retiring a lease returns an in-flight
locally originated Raft request to the FIFO head, discards an in-flight response
whose meaning was local to that binding, and prevents an obsolete blocked writer
from taking the replacement lease's item. Late completion cannot clear a newer
claim. Same-generation offers coalesce while a request is queued or sending. Once
that physical send completes, the next Raft heartbeat reoffer queues one
retransmission so a delayed response cannot suppress periodic keepalives; further
queued or sending reoffers coalesce again. The first matching response installs an
acknowledgement tombstone and invokes logical completion exactly once. Established
ERFT sockets enable `NoDelay`, because their small vote and heartbeat frames are
latency-sensitive relative to the checked election interval.

`Eclips.Oracle.Runtime.ConformanceClient` is the authorized test/runtime client
for the current closed command union: process lifecycle, label workflow,
failure probes, disappearance, Herald admission and explicit voter administration. It is not a Herald, administration, or application API. It exercises the
real submit, deferral, query, receipt retirement,
duplicate/conflict, failover, and ordered-watch paths without replacing the
Herald-owned semantic producers. Controlled publication and obsolescence never
enter this runtime or its Oracle log.

An active survivor may present any exact ancestor retained in the Oracle
owner's checked membership history while catching up through repeated non-voter
retirements. Hello and established-lane authorization require that the epoch
remain active in the current generation; retired epochs, unrelated generations,
and future generations are rejected. An already admitted outstanding watch may
deliver the local retirement, but a later watch or reconnect is denied after
retirement. A still-running removed Herald then self-fences through its existing
quorum-loss isolation grace if it did not receive that retirement entry.

Replica registration adds dynamic ERFT contacts independently of native voting
authority. A newly registered role replays the full retained log from immutable
genesis, including prior configuration entries. A current-term correlated
AppendEntries response must acknowledge both matched and applied positions before
a learner satisfies the captured frontier. The native owner drives joint and
final proposals from the Oracle owner's current retained intent; inherited
uncommitted or unapplied suffixes must settle first. Demoted voters remain
learners, and role failure does not retire the surrounding Herald. Preparation
can be cancelled before a joint entry is appended; later status remains distinct
from the immutable receipt which accepted the change.

Fresh one-shot EORC health queries reach the native owner even when ordinary
Oracle service lacks quorum. Current committed membership still authorizes the
source identity: a retired source receives no reply, while an active demoted
Herald remains admitted. Replies bind a round to the exact effective native
configuration and its local generation; they do not advance the control watch.
Accepted host failure drives ordinary joint/final reconfiguration before semantic
retirement. Retired native targets leave the replication set. Their transport
bindings are detached, parked connection attempts are cancelled and joined, and
the manager then releases their request/response queues and worker witnesses.
The registry supervisor derives its work from current owners; it retains no
second history of previously started workers. Membership retirement still fences
late discovery and worker admission. Explicit demotion retains learner service.
Failed connects and
rejected handshakes use progressive retry pacing. `configureOracleTcpTiming`
derives reconnect pacing from the shared takeover target: the five-second
default starts at 10 ms and caps exponential retries at 250 ms. Reverse
registration probes have a 100 ms floor derived from the same target. Initial
and later discovered peers share the manager's configured policy; only an
admitted native lane resets a failed-attempt progression. Raft heartbeat and
election durations remain explicit checked-genesis inputs.

The package deliberately provides no persistence, restart, authentication,
resource ceilings, compatibility protocol, or
production hardening. A stopped replica remains stopped for its finite experimental run.
The interfaces are current-build prototype interfaces and may change
incompatibly with the owning networked specification.
