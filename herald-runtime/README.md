# Herald runtime

`eclips-herald-runtime` is the concurrent shell and scoped TCP composition root
around the pure Herald kernel. It owns one kernel state in one task, uses STM for
typed lanes and small coordination facts, and keeps clocks, workers, callbacks,
framing, and sockets at explicit runtime boundaries.

The ordinary library remains socket-free. Its private `runtime-internal`
component owns the typed dispatcher and the exact nominal Oracle DTO/core
adapter before invoking callbacks. A separate private `tcp-internal` component
interprets EAPP/EADM/EPRP framing plus the outbound EORC lane and owns socket
lifecycle, while the named public `tcp` library exposes only
`Eclips.Herald.Runtime.TCP`.

The shell has one exclusive `HeraldState` owner, a generated-model-checked fair
STM arbiter, a role-indexed physical connection registry, one writer per live
typed lane, and an independent effect-batch dispatcher. Physical callback races
are settled through generation checks and single-assignment outcome cells;
the peer writer preserves a frontier-control-before-item order on each binding.
Logical acknowledgement remains kernel-owned. The package-private exact trace is
used for deterministic replay and conformance only. Its startup header retains
the exact checked Oracle contact set, opaque generator seed, and checked peer
recovery configuration so replay initializes the same watch client, generator,
and liveness owner; public diagnostics and rendering remain redacted and cannot
expose seed, application, peer, or Oracle payload bytes.

An explicit work-count sink (or `ECLIPS_WORK_COUNTS_DIR` in the TCP deployment)
enables finite aggregate diagnostic counters. The sole kernel owner classifies
emitted structural reports against its predecessor's local report, and computes
one retained-generation inventory at shutdown. It fully evaluates the summaries
outside STM; only numeric counts and fixed category names enter the ledger.
Disabled diagnostics do not inspect those states or effects. The inventory counts
retained generations per owner, including imported history, rather than planning
attempts or unique global work. A usable alignment census requires a successful
workload, zero kernel faults, and one inventory from each expected Herald.

The private runtime timer manager interprets generation-qualified absolute
deadlines from the pure owner. Early physical wakeups rearm the same attempt;
current firings preserve the runtime timer clock's observed instant; cancellation
and stale results cannot reach semantic owners as current work. Required
timer-allocation failure is an essential-worker failure, and trace replay records
the exact timer specification and outcome rather than reconstructing elapsed time.

Peer-slot replacement or closure atomically invalidates the runtime registry and
TCP projection for that physical lease; late writer, close, and outcome callbacks
are fenced by the physical generation and single-assignment outcome cell. If a
`SendPeerItem` effect names a logical binding for which the registry has no
physical route, the shell does not fabricate `PeerDispatchDeferred` and feed an
immediate select/defer loop. It queues one coalesced loss observation for that
exact binding. The pure owner then clears its dispatch ownership while retaining
the item and stream progress for a replacement binding.

The runtime interprets explicit `ClosePeerBinding`, `CancelPeerDial`, and
`CancelPeerDestination` effects. The shell acts only on those pure-owner
decisions: it never infers retirement from transport loss. Dial cancellation
matches the exact owner key independently of address growth, suppresses queued or
already-handed generations, and is rechecked by the TCP learned-job owner before
any socket attempt.

Loss of an admitted peer binding—or the first owner-authorized attempt to contact
a known but unbound peer—starts one pure fixed-deadline recovery episode. Address
growth, repeated dials, heartbeat loss, and half-handshakes cannot renew it; an
accepted current Hello cancels it. Expiry yields local suspicion only and cannot
construct Oracle retirement.

For a current stable voter host, terminal peer-recovery grace may retain one
stable failure-probe Open request. Applying the Oracle's Open entry starts one
fresh direct EPRP attempt per voter reporter with an absolute timer; current
bindings send immediately and later accepted bindings reoffer the same attempt.
Reachable responses and fresh timeouts become retained Oracle reports, whose
committed Dismiss, accepted failure, or Retire result clears the episode without reusing pre-probe
reachability. Direct probes and the terminal-source inventory/repair/union family
use the ordinary typed peer-control lane. The shell neither votes nor interprets
their semantic evidence.

Every Herald obtains fresh native Oracle-health rounds through independent EORC
queries, including its cohosted endpoint. A round requires the exact stable or
joint quorum predicate; cached contacts and leader hints cannot establish health.
The owner schedules the next round, and endpoint workers are joined and removed
from supervision after each round. If health cannot be restored before the pure
isolation deadline, the owner irreversibly fences its semantic role and performs
a bounded read-only application drain. The control/status shell and cohosted Raft
owner remain live until native exclusion or explicit physical shutdown ends their
respective duties. Transport recovery cannot reactivate the old semantic epoch.
OS process state, EOF, and socket timeout alone never construct retirement.

Each attempted runtime start invokes one explicit `RuntimeGeneratorSeedSource`
exactly once and checks its 32-byte result before allocating the Herald runtime
context, trace, kernel state, or worker tree. Ordinary source failures and wrong
widths therefore remain typed initialization failures with no partially usable
handle. `systemRuntimeGeneratorSeedSource` is the sole host-entropy boundary;
controlled embeddings and tests supply fixed sources explicitly. The checked
seed is passed into the pure Herald initializer and is never requested by a
kernel effect.

The TCP component provides scoped ephemeral-capable application,
process-administration, and peer listeners, one reader and writer per live
connection, configured-seed retries, and coalesced learned-contact dialing only
after the Discovery owner emits an opaque `PeerDialIntent`. It handles ordinary
EOF, malformed frames, reconnect, and generation fencing. Checked positive idle
and reply intervals drive physical EAPP/EPRP Ping/Pong after logical
establishment; every nonempty established-lane receive chunk rearms idle, while
complete-envelope boundaries keep exact Pong attribution honest across
fragmentation. One outstanding nonce is probed at a time, and a missed reply
closes only that socket. Semantic and
heartbeat frames share one per-socket serialized writer. Configured and learned
peer retries use the same injected paced scheduler. A connected EAPP/EPRP
candidate that does not establish within the checked reply interval is closed so
an outbound owner can resume paced retry. Failure to allocate a heartbeat or
candidate delay terminates the supervised TCP scope instead of becoming false
liveness evidence. The shell provides no TLS,
cryptographic authentication, compatibility negotiation, resource ceilings, or
daemon policy. The EAPP lane carries the eight current operations:
`newid`, `newenv`, `write`, `forward`, `read`, `localTake`, `wait`, and `label`; generic
ordinary/structural publication and the Oracle-backed label workflow use that
same scoped application path. The TCP modules remain unaware of generator keys,
prefixes, counters, global-ID construction, structural reconciliation, or label
semantics; they only frame shared DTOs and feed typed ingress.

The runtime also routes the separate EAPP local-child lifecycle and stable initial-claim
envelopes. The TCP adapter supplies the actual local application locator for
descriptor/target admission; the pure preparation owner chooses every semantic
outcome. A matched `InitialClaimPending` changes only the candidate's physical
phase: heartbeat continues while readiness is pending, with no logical session or
application-loss timer. Claim success promotes that candidate through the existing
checked binding disposition. The client then closes it and completes an ordinary
Resume on a fresh candidate before returning startup access. That receipt handoff
lets the server distinguish an idle attached client from a client still missing
its initial result, without renewing an unknown-result recovery deadline.

Own-process End's retained terminal reply precedes session disposal in the effect
batch. A lost result uses a restricted candidate query containing the original
attachment and lifecycle request identity. The adapter sends its current result
once and closes; it creates no session, binding, private map, or End request.
Pending queries are retried by the pure client's paced connection path. Scoped
client shutdown never waits for an Oracle quorum or implicitly cancels a child.

On POSIX, EAPP, EADM, EPRP, and EORC transfers try a direct nonblocking system
call before consulting the GHC IO manager. `EAGAIN`/`EWOULDBLOCK` installs one
cancellable readiness registration; if it has not fired after one second, the
runtime cancels it, retries the system call, and registers afresh only if the
socket still would block. No timeout surrounds a transfer that may have moved
bytes, and a partial write advances the exact retained suffix under masking. This
is a bounded-latency workaround for the rare test observation that a complete
frame or writable socket can remain in the kernel while its Haskell waiter does
not wake. Windows retains `network`'s blocking backend and does not claim this
watchdog behavior.

The reversible peer-only transport gate owns a two-phase lease for every socket
admitted before its cut. It requests every shutdown first, then joins the
socket-local reader, heartbeat, establishment worker, and runtime connection
writer before the lease owner performs the sole descriptor close. Reopening can
therefore admit no replacement generation while an old decoder or writer still
owns the predecessor socket, including across a close-between-claim-and-spawn
race.

The EADM listener is a third required endpoint. A fresh physical lease accepts
only the process-administrator Hello until the pure owner authorizes
it, then carries Start, explicit process-epoch End, unclaimed-child cancellation,
and retained-result queries. Cancellation names the same opaque preparation
handle returned to the parent and shares the pure owner's claim/cancel winner.
Replacement/loss retires only the binding: accepted and terminal lifecycle state
remains in the pure Administration owner and is reoffered after reconnect. This
plane is separate from the private one-shot runtime administration slot used
solely for orderly drain. EADM does not infer process death from connection loss,
and orderly Herald shutdown is not an EADM command.

The fixed-contact, outbound-only EORC manager remains the Herald's Oracle
boundary. The pure owner mints every logical connect, retry, binding, watch, and
semantic Start/End/label/failure/disappearance command action; the TCP shell owns only physical
connection attempts, incremental framing, retry delays, and scoped cancellation.
A redirect carrying the first admitted Oracle term, or a term strictly greater
than the retained term, and naming another member of the checked startup contact
set selects that fixed contact immediately. Repeated, stale, absent, unknown, and
self-referential hints remain on the ordinary paced retry path. Loss reconnects
from the exact greatest applied control index while retaining request and result
evidence. Controlled publication never submits an Oracle command.
After membership changes, an active survivor may reconnect with a Hello naming
any exact ancestor retained in the Oracle owner's checked membership history.
Admission still requires the epoch to be active in the current generation;
historical origin evidence cannot authorize a retired epoch. The same lane stays usable for later survivor
submissions, whose canonical commands retain their own generation checks.
Oracle unavailability does not block genesis-only or ordinary local application
operations, and drain retires the Oracle lane before the composite scope closes
and joins every worker.

An initially contacted nonleader handling an empty watch at its applied tip
returns a redirect immediately under the client policy above; a behind-tip watch
can still be served from the retained suffix. An empty committed-entry watch
already accepted by the local leader at its applied tip instead receives one
bounded authority-grace interval per authority-loss episode, using the configured
upper bound of the Raft election timeout. A completed watch result, local
applied-prefix progress, or recovered local leadership wins during that grace; a
distinct known leader redirects immediately. Authority still unresolved at
expiry redirects into paced fixed-contact rotation, so absent and self hints
cannot retain the watch indefinitely.

The EORC shell coalesces a missing physical lane to one loss report per logical
binding and paces connection, Hello, and binding-loss retries separately from
semantic submission retry. Connection retry starts at
`max(configuredDelay, 1 microsecond)`, doubles after each unsuccessful attempt up
to `max(configuredDelay, 1 second)`, and resets when a binding is established. A
correlated `OracleSubmissionNotReady` retry uses the fixed effective configured
delay and does not advance or reset that connection streak.

The named control gate keeps `H1` live when only the co-hosted `R1` voter/Oracle
scope stops, recovers the retained Start receipt through `R2`, verifies exact
duplicate/conflict handling, and proves that four Herald watch clients converge on
the byte-identical process-projection entry even when `H4` is deliberately held at
the preceding cursor until after failover. Peer publication does not require an
Oracle-created controlled target record.

The `herald-step14-label-properties` component composes four real Herald TCP scopes and exercises
the EAPP label call, Start/End EADM, direct label installation and collection, laggard and
handoff schedules, lifecycle consequences, retained replies, and alignment-loss
tails. Deterministic peer and Oracle checkpoints expose the relevant transport
linearization points without making socket scheduling part of a kernel proof.

The `herald-step15-crash-properties` component uses voter-hosts H1/H2/H3 and non-voter H4. Its crash
tour exercises fresh direct probes, Oracle-committed retirement, exact
survivor-held terminal-source repair, continued H1 application service, and
successor-generation structural reports from H1/H2/H3; its partition tour
exercises H4's irreversible local isolation and bounded read-only drain. Pure
properties separately prove that a still-running retired epoch is fenced and
that the terminal-union/closure algebra reaches an inert replay. The live tours
bound every recorded timer, dial, heartbeat, Oracle, terminal-control, and
structural-report phase so outer retry amplification cannot hide behind a widened
wall-clock deadline.


The `herald-step16-failure-properties` component has two failure-sensitive serial cases,
each run in three consecutive fresh processes under the aggregate release gate. One captures
the exact publication and live-alignment markers on both directions of a single
connection before loss, then proves each of the three affected markers is repaired
once: `P=12` and `A_remote=1` produce 13 loss-free versus 16 one-loss physical
writes. The other holds an accepted `newenv` root destined for H4, retires H4,
and proves completion of the unchanged twelve-root manifest in the H1/H2/H3
successor. It then resolves both disappearance subjects, verifies the canonical
equal-redefinition occurrence, and demonstrates private successor visibility
through an explicitly published owner-private Edge.

Those phase boundaries use coherent trace evidence for exact finite peer
dispatch, receipt, and completion facts, not an elapsed quiet period or private
kernel-state reads. Scope close and structured join precede final interval
accounting: the tour has exactly 64 structural occurrences (two 31-object
environments, one Neutral and one private Edge), frozen destination bound
`E<=141`, and no physical Oracle submission during the fresh `newenv` call.
The cases run serially with explicit deadlines.

The shell uses direct, narrow `IO` capabilities with scoped threads. It has no
`effectful` dependency: a second effect vocabulary would only wrap the same concrete clock, delay, writer,
and diagnostic callbacks while obscuring their STM linearization points. This is
an implementation choice at the runtime boundary. Pure kernels remain free
of `IO`, threads, STM, and runtime imports; changing a capability handler does not
change their transitions.

The live Oracle-attempt property runs two disappearance Opens through a
real Oracle TCP cluster and applies them in a real Herald owner before admitting
two authorized Abort intentions. It counts unique retained request identities
separately from actual TCP-manager connect, Submit, and Redirect interpretations.
For the checked three-voter contact set, the explicit finite-schedule bound is
`0 + O + O*V + 4*X`, where `O=2`, `V=3`, and `X` is zero or one deliberate
connection loss (`X <= O`). Both runs include a follower redirect through the fresh Hello leader hint; its
follower and leader connects are counted even when no `OracleRedirect` frame is
needed. The loss run
closes the first Abort lane immediately before its physical write, retains the
same two request identities, and adds exactly one Submit attempt and at most
four total physical attempts. Counts are read only after drain and scope join.

Bound listener addresses are separate from advertised peer/application contacts.
`configureHeraldTcpAdvertisedEndpoints` takes independently optional checked
application and peer endpoints. Each omitted contact uses its actual bound
listener, including an OS-assigned ephemeral port. The runtime passes its
immutable application locator into Hello and initial-claim
admission. The pure Discovery owner retains only admitted peer contacts. Remote
preparation controls use the existing peer dispatch and reconnect boundaries.
The production deployment package composes these scopes and exposes EADM status,
preparation inspection and correlated semantic drain.

The [label architecture](../docs/architecture/labels.md) defines the semantic
label decision and completion contract. The runtime interprets those owner-issued
actions without adding a marker/barrier protocol of its own. The
[verification guide](../docs/verification/README.md) describes the current gates
and distinguishes named tour assertions from aggregate results.
