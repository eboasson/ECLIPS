# Configurable takeover timing


## Requirements

Expose one deployment parameter, `takeoverTarget`, defaulting to five seconds.
Measure it from sustained failure to committed authority allowing takeover:
process End, or Herald retirement after any required native voter exclusion.
Application reconstruction, replacement startup and subsequent graph convergence
are separate work. A responsive surviving quorum and supported runtime load are
necessary for this operating target. Missing quorum, concurrent membership work,
blocked socket writes or long owner/scheduler pauses can make it exceed the target.
There is no global timeout that authorizes takeover or discards required evidence.

The policy supplies experimental defaults for a modest LAN. Individual scheduling,
GC, socket and owner delays must be measured before treating the target as
supported performance. A fast election does not by itself settle the separate
failure-report and semantic-retirement obligations.

## High-level design

Choose timing once when creating a fresh deployment. Its canonical manifest is
also the bootstrap transferred by restricted discovery. Joining Heralds therefore
inherit the founder's target; native Raft timing comes from the same checked run.
Connection descriptors carry the target to ordinary application clients, including
prepared children. Timing is configuration, not authority or protocol negotiation.
This is a coordinated current-build encoding change: old descriptors/manifests
are not adapted, and a new experimental run is required.

`Eclips.Public.Types.Timing` owns the shared opaque target and pure derivation.
The shared package admits no semantic identity or mutable state through this type.
Runtime shells interpret the resulting monotonic microsecond durations. Kernel
initialization receives concrete policy data, and replay retains the selected
policy. Existing explicit fixture constructors can still describe deliberately
short or long schedules; they do not silently become deployment defaults.

Normal CLI use supplies `--takeover-target 5s` with `--bootstrap` or
`--write-fixed-manifest`. Exact decimal seconds, milliseconds and microseconds are
accepted, for example `2.5s`, `2500ms` and `2500000us`. A joiner or a resident using
an existing manifest cannot override its run policy. Startup output lists every
resolved duration as `timing NAME NUMBERus`. The checked low-level configuration
interfaces remain available for explicit test schedules. Apply
`configureHeraldRuntimeTiming` before an explicit isolation/drain override and then
construct TCP with the chosen physical values, since `configureHeraldTcpTiming`
reapplies the complete derived defaults.

## Detailed timing model

For target T, derive these defaults, rounding down to whole microseconds:

| Parameter | Formula | T = 5 seconds |
| --- | --- | --- |
| TCP heartbeat idle | T / 20 | 250 ms |
| Peer heartbeat reply and candidate establishment | T / 10 | 500 ms |
| Application establishment, heartbeat reply and final session-close write | independent default | 10 s |
| Peer recovery grace | 2 T / 5 | 2 s |
| Fresh direct failure probe | T / 10 | 500 ms |
| Raft heartbeat | T / 200 | 25 ms |
| Randomized Raft election interval | T / 20 through T / 10 | 250–500 ms |
| Recent-leader guard / partial acknowledgement window | election lower | 250 ms |
| Accepted leader-watch authority-loss grace | election upper | 500 ms |
| Quorum-health round | T / 20 | 250 ms |
| Health query per endpoint | T / 50 | 100 ms |
| Quorum-loss isolation grace | 3 T / 5 | 3 s |
| Application-session recovery | 2 T / 5 | 2 s |
| Initial reconnect backoff | T / 500 | 10 ms |
| Initial Oracle submission backoff | T / 50 | 100 ms |
| Maximum retry backoff | T / 20 | 250 ms |
| Reverse Raft dial floor | T / 50 | 100 ms |
| Read-only drain after fencing | independent default | 500 ms |

The application and Herald use the same ten-second application transport response
allowance. It covers initial establishment, established heartbeat replies and the
client's final `EndSession` write. It does not impose a deadline on semantic RPC
completion: `wait`, `label` and other operations may remain pending while the
connection stays live. Physical application connection loss still ends its
session. A pending initial claim establishes physical heartbeat handling and
ends the candidate deadline without starting a session. Explicit application
liveness configurations retain their selected interval. Herald application timing
can be overridden after resolving defaults with
`configureHeraldTcpApplicationHeartbeat`, independently of the peer policy.

The minimum representable target is 500 microseconds, because its smallest derived
wait is T/500. This is a timer-resolution check, not a claim that such a target is
operationally achievable. No arbitrary maximum, rollover policy, resource quota
or capacity outcome is introduced. Derivation uses exact integer arithmetic.

The nominal failed-Herald decision budget is:

| Part | Share | At 5 seconds |
| --- | --- | --- |
| Physical observation | T / 5 | 1 s |
| Reconnection grace | 2 T / 5 | 2 s |
| Fresh post-Open probe | T / 10 | 0.5 s |
| Consensus, dispatch and scheduling reserve | remaining 3 T / 10 | 1.5 s |

Physical observation allows two idle windows plus the reply allowance. The current
heartbeat worker samples activity when an idle window finishes, so idle plus reply
alone would understate this part. The reply timer begins after the Ping write;
write and scheduling delays additionally consume the reserve. Observed EOF can
start recovery sooner. Ordinary bytes count as physical liveness.

The fresh probe starts only after committed Open is observed by each reporter.
The reserve covers Open, report and resolution commitment, projection/application,
and any required native exclusion and semantic retirement. It is an allocation
for measurement, not an extra timer. Raft election can proceed alongside recovery;
it is not an unconditional extra sequential wait. Its minimum of ten heartbeat
intervals gives more scheduling margin than the former three.

Reconnection receives most deliberate waiting time because it tolerates transient
link loss. The fresh probe has a distinct, shorter purpose: collect new evidence
under the captured membership/voter configuration. Required majorities do not
change, unavailable reporters remain in the denominator, and committed failure
acceptance is not inferred from heartbeat or health results.

Isolation is independent. Three seconds permits multiple health rounds and Raft
elections before irreversible semantic fencing. Correctness does not assume that
local self-fencing precedes survivor retirement: committed authority, epochs and
stale-traffic rules decide that. Failed observations do not renew absolute grace;
a sufficient fresh healthy quorum can cancel it before expiry. Fencing does not
unilaterally stop a cohosted Raft voter. The subsequent read-only drain does not
hold the takeover decision, so its default stays independent of T.

Applications derive the same heartbeat/recovery defaults from their descriptor.
Explicit `connectWith` configuration remains available. Peer recovery, fresh
probes and application-session recovery remain different semantic mechanisms even
when some defaults happen to be equal. Retry caps scale with T so a hidden fixed
one-second delay cannot consume most of the remaining decision budget.

## Verification obligations and performance limits

Properties check pure timing relationships, canonical serialization, propagation
through deployment discovery and application descriptors, independent fresh-probe
and health-round timers, and retry policy. Runtime checks exercise representative
TCP lifecycle and failure paths under the configured policy.

A passing configuration test does not establish a measured five-second recovery
bound. Measurements must distinguish heartbeat gaps, scheduled versus consumed
timer delays, probe collection, native reconfiguration, semantic retirement,
application reconstruction and graph convergence. Missing quorum remains
unavailable regardless of elapsed time. See the [verification guide](../verification/README.md)
for how to record source-specific results.
