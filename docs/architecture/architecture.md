# Architecture and deployment


## 1. Design contract and document ownership

The modular implementation retains
the paper's eight data-space operations, parent-labelled `newenv`, private
application names, pure deterministic kernels, and explicit runtime boundaries.
It supports deployment, prepared-child startup, live semantic Herald
membership, and dynamic Oracle voting membership.

The design is divided by responsibility:

| Document | Owns |
| --- | --- |
| [Application startup](application-startup.md) | Primordial access, process preparation, grants, root transfer, attachment, application facade, and launcher bootstrap |
| [Herald membership](herald-membership.md) | Discovery versus admission, joining, semantic cuts, repeated retirement, and data-plane fencing |
| [Oracle and Raft](oracle-raft.md) | Small voter sets, learners, configuration-log semantics, voter administration, and failure-qualified voter removal |
| This document | Shared commitments, executable/configuration surface, package boundaries, and observability |
| [Verification](../verification/README.md) | Properties, process tours, boundary checks, and interpretation of verification evidence |

The documents under [docs/networked](../networked/README.md) describe the currently
implemented contract. Each implementation increment updates its owning normative
sections and every affected producer, consumer, codec, fixture, and audit in one
change. The specification must change first if implementation uncovers a
different required contract. No mixed-build run, compatibility decoder, schema
version, migration adapter, or parallel legacy implementation is introduced.

## 2. Scope and fixed decisions

1. Heralds are independently launched OS processes. An explicit founder starts a
   fresh system; subsequent Heralds join through a possibly incomplete seed list.
   Inability to reach a seed never silently creates a competing system.
2. Gossip advertises contacts and discovery progress. Only checked Oracle
   membership decisions grant semantic authority; only Raft configuration rules
   grant votes. These are separate state transitions.
3. `newenv` creates the existing complete environment with roots labelled to its
   caller. A parent can select arbitrary startup material, prepare the child's
   logical process at a chosen Herald, relabel the desired roots to its nameable
   identity, and launch the OS child with an opaque connection descriptor.
4. Applications use `connect`/scoped connection management and typed terminal
   results for the eight operations. Session and workflow correlation remain
   inside the existing pure client; lifecycle convenience does not become a ninth
   data-space operation.
5. The first founder hosts a one-voter Oracle. A second Herald does not silently
   become a voter. Commissioning is explicit; three voters are the normal first
   resilient deployment, not a membership cardinality ceiling.
6. A newcomer catches up without contributing to active-member evidence. Join
   activation uses an explicit finite cut and gates all new application operations,
   including reads that could grant possession. Outstanding work needing future
   application input is retained beyond the cut, not awaited by it. Profile 0.2
   does not promise uninterrupted application service during joins.
7. Herald membership may grow and contract repeatedly, including a further
   failure before a previous successor's structural base has established.
8. Voter promotion first catches up a non-voting replica. Reconfiguration uses
   the same domain-independent Raft kernel at every supported size; configuration
   changes affect elections and commitment as well as the Oracle's projection.
9. Automatic voter-host retirement requires a strict majority of the captured
   old voter configuration and the normal reconfiguration path. Reachability
   evidence and replication quorum are distinct proofs. A required lost quorum
   remains unavailable; the runtime never lowers its denominator on a timeout.
10. A removed voter may remain an active Herald. Retiring the Herald ends its
    resident processes and settles its data-plane obligations as a separate,
    explicitly coordinated transition. No retired epoch returns to authority.

Continue the benign, non-Byzantine, finite-run, in-memory model. Inputs and retained
work fit available resources; counters do not exhaust. Do not add resource
ceilings, rollover behavior, sort filters, authentication/TLS, durable restart,
durable snapshot recovery, DDS compatibility, or performance-oriented graph algorithms.
In-memory checkpoints and retained-suffix replay for a new replica do not
authorize restarting an old identity after state loss.

## 3. Identities, facts, and owners

Keep immutable run identity and bootstrap facts separate from evolving authority.
The immutable description binds `SystemId`, the closed predefined catalogue,
initial genesis, and native timing/bootstrap configuration. It does not claim
that a live process, Herald, or voter catalogue is still the initial one.
Canonical current-state digests remain semantic commitments, never versions.
The full Oracle state digest is an optional diagnostic witness, disabled by
default and fixed for the run. Its absence is explicit evidence; operational
command and workflow hashes remain mandatory. Enabled state witnesses retain
their complete canonical meaning, with the admission and equality rules in the
[Oracle contract](../networked/04-oracle-and-raft.md#5-command-submission-and-retry).

| Fact | Sole semantic owner | Other components receive |
| --- | --- | --- |
| A generated identity or entropy choice | Relevant pure kernel after explicit runtime entropy input | Retained result/effect; exact retries do not regenerate |
| Application private/global identity map | Resident Herald application owner | Process-private aliases; never another process's map |
| Prepared child, selected startup grant, first claim/cancel | Target Herald startup owner within the Herald kernel | Immutable preparation and attachment receipts |
| Process liveness and residence | Oracle | Contiguous committed process projection |
| Discovered contacts and selected peer bindings | Herald Discovery | Typed connection intentions interpreted by runtime |
| Current semantic membership and retirement tombstones | Oracle | Checked history and generation-qualified projection |
| Structural base, publication/store/alignment settlement | Composed Herald owners | Exact semantic evidence; no foreign private state reads |
| Log-effective voting configuration and elections | Raft | Sealed committed-entry/configuration effects |
| Committed native-to-Herald voter bindings and administrative intent | Oracle via ordered application adapter | Current binding/contact projection and workflow status |
| TCP sockets, physical timers, OS children, queues | Runtime or launcher | Concrete observations admitted to the owning kernel |

One supervised worker privately owns each kernel state. STM carries queues and
small coordination facts such as dispatch availability; it is not a second
membership, process, graph, or Raft state owner. Timers and asynchronous completion
events remain generation-qualified. Install a successor before dispatching its
whole effect batch, and preserve contiguous Oracle/Raft application across
reconfiguration. A joining process reconstructs through checked public protocol
records, not by copying another worker's Haskell state.

Mutable membership coordinates must not become equality-only handshake passwords.
An active but lagging peer needs to learn missing history. Admit the exact role
and history needed for catch-up while withholding ordinary semantic authority
until the owning state says it is active. The detailed membership/Raft documents
define these distinct admission rules; runtime code must not invent exceptions.

## 4. Runtime and package composition

The deployment composition package, `deployment/eclips-deployment.cabal`, contains
the user-facing `eclips-herald` and `eclips-admin` executables and narrowly shared
configuration parsing/composition code. It may depend on both Herald and Oracle
runtime public APIs. Do not make `herald-core` depend on Oracle runtime, or make
Raft depend on ECLIPS domain types merely to host both roles in one OS process.

The deployment supervisor owns the Herald runtime and, when commissioned, its
co-hosted Oracle/Raft runtime. A learner may be started on an already running
Herald. Demotion/removal changes that role through committed intent and completion;
it does not kill the Herald worker. An unexpected kernel-owner failure follows
the existing fail-stop contract: no supervisor restarts it with forgotten state.
Role shutdown must join its sockets, timers, and dispatch workers.

The hello-world package contains publisher, reader, and launcher executable
components. Publisher and reader depend only on application-facing
packages. The launcher uses the same application API to prepare and label their
environments; its OS spawning is ordinary `System.Process` work. Deployment
configuration and integration-test orchestration may depend on runtime packages,
but must not leak those imports into the example application modules.

| Plane | Profile-0.2 extension | Boundary retained |
| --- | --- | --- |
| EAPP | Typed lifecycle preparation/claim operations alongside the eight-operation union; typed facade results | Application to its chosen Herald only |
| EADM | Deployment status, explicit membership/voter administration, and orderly shutdown | Operator actions; applications do not acquire generic admin privilege |
| EPRP | Discovery/admission and semantic catch-up, repeated retirement controls, cross-Herald prepared-child grants | Herald-owned normalized inputs/effects |
| EORC | New lifecycle/membership commands and committed configuration projections | Oracle control/evidence, not ordinary publication payloads |
| ERFT | Learner admission and native configuration log entries | Opaque application values and domain-independent Raft roles |

Endpoint role magic continues to distinguish protocol families. It is not a
negotiation or revision scheme. A field/variant is added to the sole-current
protocol only when its executable owner and terminal path land in that increment.

## 5. Executable and CLI contract

The deployment provides `eclips-herald`, `eclips-admin` and the three ordinary
application executables. The [deployment package](../../deployment/README.md) documents the
current founder, seed-only joining, fixed-manifest, endpoint and operator
commands, including explicit voter commissioning. Keep defaults adequate for a
local demonstration, while allowing explicit
advertised endpoints for multiple hosts. Resolve DNS and bind sockets in runtime;
pure kernels receive admitted identities and endpoint descriptions.

`eclips-herald` accepts exactly one startup mode, `--bootstrap` or one or more
`--peer HOST:PORT` seeds for joining. `--bind HOST --base-port PORT` expands to
five endpoints: peer at the base, application at base+1, administration at base+2,
Oracle at base+3, and Raft at base+4. This is CLI shorthand for explicit per-plane
listen/advertise settings, not a wire assumption. Validate actual TCP endpoint
shape and reject conflicting local bindings before declaring readiness. A role
that has not started does not accept that role's semantic requests.

New runs also accept `--takeover-target 5s` on `--bootstrap` or
`--write-fixed-manifest`. The canonical bootstrap carries this selected target;
joining and fixed-manifest residents inherit it. Startup reports every resolved
duration, and application connection descriptors carry the matching client policy.
See the [takeover timing model](takeover-timing.md) for requirements, derivation,
nominal budget and focused verification. Timing does not confer authority.

The founder creates the first logical launcher process and its full genesis root
bundle once. `--launcher-output PATH` explicitly writes its opaque attachment
descriptor after readiness; ordinary `newenv` does not print identifiers. This
is a launch artifact, not a durable state file. An incompatible run or a lost
founder epoch cannot resume from it. The user chooses the output path.

Founder startup:

```sh
cabal --config-file="$PWD/.cabal/config" run -j4 eclips-herald -- \
  --bootstrap --bind 127.0.0.1 --base-port 7100 \
  --launcher-output .cabal/profile-0.2/launcher.eclips
```

Seed-only startup uses the ordinary peer endpoint. A shared checked fixed
manifest and resident ordinal remain available as an explicit fixture:

```sh
cabal --config-file="$PWD/.cabal/config" run -j4 eclips-herald -- \
  --bind 127.0.0.1 --base-port 7200 --peer 127.0.0.1:7100

cabal --config-file="$PWD/.cabal/config" run -j4 eclips-herald -- \
  --bind 127.0.0.1 --base-port 7300 --peer 127.0.0.1:7200
```

The third Herald reaches the first through learned contacts. A disjoint set of
unreachable seeds does not imply discovery success. Conflicting system identities
are a configuration mismatch, not a request to merge experimental runs.

The example launcher consumes the founder descriptor and chooses each
child's residence independently:

```sh
cabal --config-file="$PWD/.cabal/config" run -j4 eclips-hello-launcher -- \
  --connection-file .cabal/profile-0.2/launcher.eclips \
  --publisher-herald 127.0.0.1:7101 --reader-herald 127.0.0.1:7201
```

The launcher prepares separate primordial sets and children, transfers their
roots, and starts `eclips-hello-publisher` and `eclips-hello-reader` with their
individual descriptors. Both child executables must also be runnable manually
with `--connection DESCRIPTOR`. Setting both residence arguments to the same
application endpoint exercises same-Herald startup. An attachment descriptor's
target is checked; changing its locator does not move the process.

`eclips-admin` commands over a chosen administration endpoint:

| Command | Result |
| --- | --- |
| `status` | System identity, this Herald's admitted role, current membership, voter configuration, leader hint, and pending workflow phase |
| `prepare-oracle` | Retain and start a local registered learner on the selected Herald |
| `commission-voters CONFIGURATION NODE...` | Add registered nodes through retained catch-up and joint consensus |
| `decommission-voter CONFIGURATION NODE` | Retained voter-role removal; the Herald remains active |
| `oracle-configuration` / `voter-change CHANGE` | Current committed configuration and later retained workflow phase |
| `cancel-voter-change CHANGE` | Cancel explicit preparation before a native joint append |
| `preparations` | Inspect pending children and return stable administration references without exposing private application handles |
| `cancel-child REFERENCE` | Explicitly cancel an unclaimed preparation, reporting an attachment that already won the race |
| `drain` | Existing explicit application/runtime shutdown behavior; never silently removes a voter |

Administrative retries query/reoffer the same workflow. `status` must distinguish
pending, completed, rejected, and unavailable outcomes; observing a request
accepted locally is not proof of a committed configuration. Automatic failure
retirement reuses the native reconfiguration path used by explicit voter changes;
semantic Herald retirement additionally requires its accepted failure evidence.
The deployment README gives the complete argument syntax and retained-result
commands.

## 6. Observable behavior and failure contract

A startup process reports its bound endpoints before semantic readiness only with
an explicit starting/joining status. Report ready only when the corresponding
Herald/Oracle/startup contract is satisfied. Keep application output separate from
diagnostics: the reader prints the greeting, while startup/role/failure details
go to stderr or a requested status response. Machine-consumed launch descriptors
are returned or explicitly written, never mixed with routine logs.

Retain immutable trace events for phase changes, causal coordinates, request
correlation, Oracle control position, Raft log/configuration position, and terminal
outcomes. Tests observe these public/runtime events; they do not read a foreign
worker's kernel state through STM. Record enough evidence to distinguish pending
network retry, missing quorum, pending semantic cut, and terminal retirement.

While a required quorum is absent, new Oracle decisions and dependent operations
remain pending/unavailable according to their existing call contracts. Never
report success from a local role timeout. Already permitted data-plane activity
continues only within the Herald's current authority/isolation contract.

Shutting down an OS process is not semantic decommissioning. A user who wants a
graceful role change waits for that administrative terminal first. Retired epochs
remain retired if transports reopen; joining again requires a fresh identity.
Losing all in-memory authority requires a fresh experimental run, not an implicit
resurrection or new quorum assembled from reachable contacts.

## 7. Verification and implementation detail

The [networked specification](../networked/README.md) owns observable semantics
and protocol rules. The [Herald kernel guide](herald-kernel.md) explains owner
composition; the [verification guide](../verification/README.md) identifies the
current checks and the distinction between an obligation and a recorded result.
