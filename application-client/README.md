# ECLIPS application client

`eclips-application-client` is the pure application-side owner for the current
application protocol. It owns pending invocations and exact semantic request
bodies, handles wait cancellation races, and reports terminal connection loss
without performing I/O. An established TCP connection is the application's
lifetime boundary. Applications recover by restarting; established loss never
initiates an old-session Resume.

Initial attachment uses generation-qualified, paced retries before a session is
known. Pending own-End lifecycle requests retain their full correlations and use
only the restricted attachment-scoped result query after loss. This query cannot
open a session or submit another End; an absent or retired result remains
explicitly unknown.

Every new ordinary or lifecycle request carries the latest independent receipt
retirement high-water marks and unresolved exceptions. Metadata is separate from the immutable semantic request.
Terminal completion transfers result ownership to the ordered effect batch and
then to the caller's repeatable result handle. The pure owner retains only pending
request correlations, their pending cursor observations, and compact monotonic
sequence facts. Fresh caller invocation identities must strictly increase across
both request families; reuse or a lower identity is rejected even after old
records have been reclaimed. Cancellation of a retired invocation has an explicit
local `ApplicationInvocationRetired` outcome.

Each namespace announces its allocated high-water mark H and the set E of
unresolved requests at or below H. Every identity at or below H outside E is
released. A pending wait at request 0 therefore retains only its own receipt
while later completed reads are reclaimed. Joining announcements takes the
union of released identities: stale metadata cannot restore an old exception.
Ordinary and lifecycle progress remain independent.
There is one coalesced idle timer per physical attempt, using the existing retry
delay. It reads the latest progress when it fires and sends a standalone
`RetireReceipts` only if ordinary traffic has not confirmed that progress. No clock
or timer is read or created by the pure kernel.

The submission facade remains the closed eight-operation surface:
`submitNewId`, `submitWrite`, `submitForward`, `submitRead`, `submitLocalTake`,
`submitWait`, `submitLabel`, and `submitNewEnvironment`. Structural, label, and
environment acceptance stay pending until their distinct operation terminal
arrives. `SubmitLifecycle` independently carries preparation, readiness,
cancellation, and own-process End with checked session-qualified identities and
terminal result kinds.

`initialPreparedApplicationClient` takes one descriptor and stable initial-claim
identity. Pre-session retries preserve both. After `SessionOpened`, it sends an
empty receipt acknowledgement on that same connection and exposes startup after
`ReceiptsRetired` confirms it. The successful connection is never deliberately
closed as part of startup. `initialLifecycleRecoveryClient` queries an own-End
terminal without a session.

Encoding, framing, sockets, clocks, queues, and the Herald kernel remain at their
separate boundaries. The runtime interprets complete FIFO effect batches, filling
caller-owned result cells before sending receipt acknowledgements.
