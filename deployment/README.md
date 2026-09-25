# ECLIPS deployment

The deployment runtime supports founder bootstrap, live Herald admission,
Oracle voter changes, automatic failed-host retirement, and irreversible
semantic fencing. See the [architecture](../docs/architecture/architecture.md)
and [verification guide](../docs/verification/README.md) for the supported scope
and the checks required to assess a source snapshot.

Executables and tests default to a 768 MiB GHC heap budget and accept RTS options.
For guarded macOS runs, use `python3 scripts/run-memory-guard.py --output
.cabal/run-memory.json --seconds 180 -- COMMAND ...` from the project root. The
watchdog stops at 900 MiB resident memory per runtime or 4096 MiB for the process
tree. Certification uses `--heap 768m --process-mib 900 --compiler-mib 2816
--tree-mib 4096`; compiler-only wrappers apply a 2304 MiB heap budget while the
watchdog permits 2816 MiB RSS per compiler/linker. Runtime processes retain their
768 MiB heap and 900 MiB RSS thresholds during builds and tests. These sampled
limits leave headroom below the 1 GB runtime and 3 GB compiler process targets;
GHC's heap limit alone is not a hard RSS ceiling. See the
[build and memory instructions](../BUILDING.md#heap-limits-and-process-memory).

This package composes the public Herald and Oracle TCP runtimes. Each process
owns one Herald; the founder also starts the singleton Oracle voter. An active
Herald can prepare a local Oracle learner and participate in explicit voter
changes. Oracle role failure or demotion leaves the ordinary Herald running.
Kernels are never restarted after losing volatile state.

Start a fresh founder and explicitly request its one launcher descriptor:

```
eclips-herald --bootstrap --bind 127.0.0.1 --base-port 7100 --launcher-output launcher.connection
```

The five ports are peer 7100, application 7101, administration 7102, Oracle 7103,
and Raft 7104. `--advertise HOST` separates an advertised host from a listen
interface. Each plane also accepts explicit `--PLANE-listen HOST:PORT` and
`--PLANE-advertise HOST:PORT` arguments. Base port zero requests independent
OS-assigned listeners for a single local founder. Actual bound application,
administration, and peer endpoints are printed after readiness.
Application and peer advertisements are independent: an omitted advertisement
uses that listener's actual bound endpoint. Endpoint flags belong to bootstrap and seed startup; fixed residents obtain
their endpoints from the checked manifest.

The launcher file is canonical opaque connection material. Applications use
`Eclips.Application.connect`; they receive no runtime bootstrap records. No
launcher file is written unless requested.

Join the running system from a peer endpoint, then extend the seed chain:

```
eclips-herald --bind 127.0.0.1 --base-port 7200 --peer 127.0.0.1:7100
eclips-herald --bind 127.0.0.1 --base-port 7300 --peer 127.0.0.1:7200
```

Repeat `--peer` for multiple seeds. Duplicate hints are harmless. Seed startup
needs a nonzero peer listener or explicit peer advertisement so the admission
offer can publish its contact before the pending runtime starts. Application
and administration listeners may use port zero.
The newcomer has no configured remote identity or voter roster: discovery retrieves the exact
immutable founder bootstrap and current Oracle contacts, then follows transitive
contact hints. Successful seed replies must agree on the bootstrap. Unreachable
seeds are retried with pacing and never cause an implicit bootstrap.

Discovery uses the separate EDSC family on the peer listener. Its contact cache
does not grant an ordinary peer binding, application service, membership, or a
Raft vote. Onboarding requests retain their exact correlation and payload across
physical retries. Readiness is declared only after activation; only the founder
creates and exports a genesis launcher. New application admissions pause while
existing members seal the finite join cut and resume on the successor base.

The fixed-manifest fixture remains available. Create one fresh manifest and give
the same file to independent resident processes:

```
eclips-herald --write-fixed-manifest run.manifest --member 127.0.0.1:7100 --member 127.0.0.1:7200
eclips-herald --fixed-manifest run.manifest --member 1 --launcher-output launcher.connection
eclips-herald --fixed-manifest run.manifest --member 2
```

Only member 1 receives the genesis launcher and full root set. Every subsequent
application is prepared by an ordinary parent. The static manifest is an
explicit experimental configuration; a new run requires a fresh manifest.

Use the system identity printed at startup for operator admission:

```
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX status
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX preparations
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX --correlation 10 cancel-child PREPARATION_HEX
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX --correlation 11 drain
```

Status is a pure owner snapshot: current identity, membership and control
position, current committed voter hosts (the union while joint), service phase, current Oracle contact and its
leader hint. A leader hint is not a fresh native Raft read. Preparations include
local targets, remote targets waiting for prerequisites, and outgoing parent
receipts. Each invocation keeps its connection until the terminal result and then
retires its RPC receipt. Correlations are scoped to that connection. Drain waits for the
final EADM receipt before the runtime closes its connections and joins workers.

A newly admitted Herald is a non-voter. Prepare its replica through that Herald's
administration endpoint, then submit a voter change from any active Herald:

```
eclips-admin --endpoint 127.0.0.1:7202 --system SYSTEM_HEX --correlation 20 prepare-oracle
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX oracle-configuration
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX --correlation 21 commission-voters CONFIGURATION_HEX REPLICA_NODE_HEX
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX voter-change CHANGE_INTEGER
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX --correlation 22 decommission-voter CONFIGURATION_HEX REPLICA_NODE_HEX
eclips-admin --endpoint 127.0.0.1:7102 --system SYSTEM_HEX --correlation 23 cancel-voter-change CHANGE_INTEGER
```

Preparation requires concrete Oracle and Raft advertised/listen ports. It retains
one local replica identity and commits its registration before starting the
empty-history learner. The normal Herald may use zero role ports when no Oracle
preparation is wanted. `oracle-configuration` lists current registrations,
configuration identity and pending change. Commission accepts one or more
registered node identities and adds them to the current voters. Decommission
removes one current voter; the last voter cannot be removed. Both require the
expected configuration identity and follow committed joint then final stages.
Cancellation is available before joint commitment. A demoted replica continues
as a learner and its Herald keeps serving applications.

An accepted mutation receipt records the original request outcome and is returned
by the mutation invocation. Query `voter-change` for the later workflow phase.
Closing the operator connection retires its result-delivery lifetime while the
ordered semantic operation continues. A local snapshot or leader hint is not a quorum
health assertion. A missing old or new majority leaves the transition pending.
An accepted old-majority failure certificate drives automatic voter exclusion,
then semantic retirement. A three-voter system can retire one failed host; a
remaining minority cannot shrink its own quorum. All Heralds self-fence after
sustained loss of fresh native quorum health. That ends application authority
while leaving cohosted Raft duties and operator status/drain available. A fenced
or retired epoch cannot rejoin on reconnect; replacement uses a fresh epoch.
Durable restart remains deferred.

The `deployment-properties` suite checks fixed-manifest coherence, endpoint
admission, canonical discovery payloads, transitive unknown contacts, exact
retry retention, contact-set union, and conflicting seed replies. `deployment-os-integration` starts independent resident executables
and the ordinary launcher/publisher/reader executables in both startup orders,
with same-resident and cross-resident children. It also drops one prepared child's initial
session reply through an advertised TCP relay, verifies cancellation after a
failed first OS spawn, inspects retained child results, and drains every runtime.
Its seed tour starts H2 before its founder seed is reachable, includes duplicate
and unavailable hints, adds H3 through H2, checks byte-identical bootstrap
retrieval, and launches children on the activated newcomers. Its original
parent prepares distinct launcher children before ending, so each application
run claims its own process identity and connection descriptor.
The retirement tour kills a separate applicant after Begin commits, retires
an old non-voter, checks cancellation, and starts a fresh epoch that replays the
membership history before exchanging ordinary application data.
The partition and lost-quorum scenarios check live self-fencing, retained
operator access and the inability of a minority to shrink its voter set. The
combined tour starts a founder and independent applications, joins Heralds
through partial seed hints, transfers children to remote Heralds, commissions
voters, demotes a still-serving Herald, replaces a failed voter host and admits a
fresh replacement epoch. Each stage checks canonical outcomes and cleanup; an
unexpected resident exit aborts the active workflow with its log.

From the project root, `sh scripts/check-profile-0.2-completion-tours.sh` repeats
the two OS scenarios and combined tour in three fresh processes per
corpus, alongside nine failure TCP and two native-health TCP schedules. The full
`sh scripts/check-profile-0.2-acceptance.sh` gate also runs every inventoried
component, earlier repeated tours, executable smoke and source-archive checks
under the documented runtime and compiler memory budgets.
It requires permission to bind local TCP listeners.

## Takeover timing

New runs default to a five-second takeover-decision target. Set
`--takeover-target 5s` on `--bootstrap` or `--write-fixed-manifest` to choose it;
exact decimal seconds and integer/fractional `ms` and `us` are accepted when
representable. Joiners and fixed-manifest residents inherit the chosen target.
Startup prints every resolved `timing NAME NUMBERus` value. Application descriptors
carry the same policy to clients and prepared children. The [timing model](../docs/architecture/takeover-timing.md)
explains the ratios, nominal budget and distinction between committed takeover
and replacement startup. This is an operating target requiring quorum and
responsive owners, not a timer that grants authority. Create a fresh run after
changing it; there is no current-build compatibility decoder for old manifests.


## Join receipt lifetime

An applicant epoch allocates sequential request ordinals independently for each
Herald it contacts. Submission and polling carry consumed-receipt progress;
every terminal command result triggers an idle retirement exchange before the
sequential driver continues. If that destination disappears after delivering the
result, ready progress remains available for the next piggyback without blocking
semantic progress. A lost command reply repeats the same request identity.
Activation or cancellation ends the applicant's request lifetime, releasing
completed routing receipts while any pending Oracle command still runs to its
semantic result. A retired result or transfer causes the driver to refresh
current admission status. Restart after cancellation requires a fresh epoch.

History captures and installation envelopes are retained only for their current
admission attempt. Old attempt envelopes are released after invalidation,
cancellation, or completed activation. Imported semantic observations,
structural occurrences, and alignment acceptances are retained independently;
they remain inputs to later Join history until semantic checkpointing defines a
replacement baseline.
