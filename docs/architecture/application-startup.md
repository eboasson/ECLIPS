# Application startup and the callable API


## 1. Decisions and application model

An application receives a primordial set chosen by its environment. Connecting
does not silently create a replacement environment. A parent may supply the full
result of `newenv`, an interface assembled from several environments, or a smaller
selection of objects. This implements the startup principle in the
[paper](../eclips-20100525.md#72-confining-subsystems-and-privacy), while retaining
universal edges and making no claim to its deferred sort-filter confinement.

Keep `newenv` parent-labelled. The target Herald creates the child's live logical
ECLIPS process before the parent starts an OS process. It retains the supplied
startup grant and returns a parent-private, nameable child-process handle plus a
portable connection descriptor. The parent uses ordinary `label` calls to assign
selected endpoints to that identity, then launches the OS child. The child claims
the prepared attachment with `connect`. No new label category, void-labelled
`newenv` variant, general global-ID application API, or primordial-set registry is
needed.

The logical process and OS process have separate lifetimes. A prepared child may
activate endpoints and align its deltas before it has an application session.
Preparing a child never starts an executable. Ending a parent does not end its
children. Process migration, transparent Herald restart, and inherited OS process
supervision are outside this profile.

## 2. Public startup values

Opaque `PrimordialSelection` and `PrimordialAccess` are separate from
`EnvironmentAccess`. The former uses the parent's private
namespace; the latter contains the same selected entries localized into the
child's namespace. `ApplicationStartupAccess` contains `self` plus that access.
`EnvironmentAccess` continues to contain exactly six canonically ordered
writer/reader pairs; arbitrary startup shape does not relax that law.

A selection is a map from distinct exact text keys to typed entries:

| Entry | What is retained and returned | Admission at the parent's Herald |
| --- | --- | --- |
| `Identity PrivateUniqueId` | The same unique value, localized for the child; no possession grant. | The private alias is known to the parent. Bare IDs are allowed. |
| `Object PrivateObjectId` | A normal possession grant and localized controlled-object handle. | The parent normally possesses the current controlled object. |
| `Writer PrivateNablaId` | The same grant plus a checked nabla role and sort. | Normal possession and an established current nabla definition. |
| `Reader PrivateDeltaId` | The same grant plus a checked delta role and sort. | Normal possession and an established current delta definition. |
| `Process PrivateProcessId` | The same grant plus a checked live process role. | Normal possession of that live process object. |

Entry names belong to the application interface, not to a global catalogue. Empty
and partial selections are valid. Multiple entries may refer to one object; they
receive the same child-private identity. Reject duplicate keys, inconsistent role
claims for one identity, unknown aliases, and unsupported possession claims before
accepting preparation. There are no collection or text ceilings. Regular values
and other OS arguments may be supplied separately; they need no controlled-object
grant. This API does not export delta stores.

Possession is copied, not moved. It establishes the child's ordinary derived name,
label, and update capabilities where the object's semantics permit them. An
`Identity` entry alone confers none of those capabilities. A normally possessed
endpoint labelled to someone else remains passive for the child. No separately
forgeable `operate = True` bit is accepted: operation still requires current
object, normal possession, live resident process, and matching current label.

A selection also contains:

- `requiredEndpoints`: selected writer/reader keys that must be operational when
  initial connection succeeds. Omission permits deliberate passive/name-only
  startup. This readiness condition grants no additional authority.
- Optional `environmentSources`: exactly two selected writer keys, one carrying
  the predefined Nabla sort and one carrying the predefined Delta sort. They are
  the child's fixed canonical sources for subsequent `newenv` calls.

`selectEnvironment` converts a complete `EnvironmentAccess` into a conventional
31-entry selection (twelve endpoints, the hub and eighteen edges), marks all twelve endpoints required, and selects its Nabla-role and
Delta-role writers as `environmentSources`. Arbitrary selections may nominate an
equivalent pair from different environments or omit the pair entirely. Admission
checks roles and membership in selected normal grants, without requiring parent
ownership of their labels; that ownership may be transferred later.

`newenv` requires the retained source pair and rechecks ordinary current writer
operation for both sources at call acceptance. Missing sources yield a typed
`EnvironmentSourcesUnavailable`; lost label/possession yields ordinary operation
rejection. Sources are fixed at startup, do not change when a later `newenv`
completes, and are not silently replaced by other possessed nablas. All 31 fresh
objects retain parent labels and Herald-generated identities. The twelve
endpoints are published through the two source nablas. After their installed
cut, the new environment's own vertex and edge writers publish its hub and
eighteen preserving edges. Completion waits for the complete environment's cut
and seeds its own descriptions into its structural readers.

The old requirement for six startup pairs before selecting these two actual
sources is removed. This gives a partial-startup application `newenv` exactly when
it was supplied the two needed writer sources. It supplies no hidden system writer
privilege and preserves the complete connected environment result shape.

## 3. Data owners and typed boundaries

| Owner | Retained responsibility |
| --- | --- |
| `Herald.Application` at parent | Request identity, private/global resolution, exact normalized selection, requester result, and eventual private child-process alias. |
| Proposed `Herald.ProcessPreparation` at target | Preparation identity, requester, exact transfer material, generated child identity, pending Oracle intention, grant/private installation status, readiness requirements, initial claim winner, and cancellation state. |
| `Herald.Controlled` at target | Checked direct possession sources for the child and current process/object facts; grants derive from accepted preparation, never from alias insertion. |
| `Herald.Application` at target | Child-private bijection and counter, startup access, canonical environment-source pair, sessions, request outcomes, and resume state. |
| Graph/Structural/Placement/Alignment owners | Current endpoint authority, active placement, new delta incarnation, and alignment induced by process and label facts. |
| `Herald.OracleClient` | Exact retained Start/End command bytes and correlation, submission/retry/watch effects. |
| Oracle | Canonical process identity, residence, liveness, and process-object authority; no primordial selection, root manifest, private map, session, or OS launch state. |
| Runtime/client shell | TCP, entropy, clocks, descriptor text, CLI arguments, OS process spawning, and supervised workers. |

These are logical pure owners composed by the Herald transition, not independently
mutable concurrent records. One supervised worker privately owns each kernel
state. STM carries input/output queues and small coordination facts. A compound
local transition prepares every relevant owner successor and commits them
together; sockets or other threads never inspect kernel state.

Public selection values contain application-private types only. Proposed
`CheckedStartupTransfer` contains internal global identities and evidence, has a
hidden constructor, and is produced only after source admission. Peer DTO decoding
checks shape first; semantic admission resolves its proofs against local applied
state. Prepared startup and child-name grants are opaque successor facts, not
public constructors or record-update paths. Decoding a checked
wire shape is not proof of current authority.

## 4. Dynamic process Start

`StartProcessEpoch` carries a checked dynamic process-start request: fresh `ProcessId`/`ProcessEpochId`, target
`HeraldEpoch`, and exact stable lifecycle request identity. The target's Herald
generator supplies fresh identity values after local preparation admission. The
Oracle checks that the residence is active and applies normal exact-request
deduplication. It atomically creates the live process/residence/process-object
fact; it does not mint root authority or require roots to be named in the log.

The resident target retains its immutable grant and Start intention before
submission, and installs the applied process fact before advertising preparation
success. A lost submission response uses the same request. A duplicate Start
cannot create another process. An End ordered before further preparation progress
terminalizes the workflow; the target cannot expose startup access for a dead
epoch. Generated IDs follow the existing uniqueness and finite-run assumptions;
contradictory checked internal identity reuse is an invariant fault.

The immutable predefined sort catalogue, system lineage and checked index-zero
projection identify the run. The founder's initial launcher and complete genesis
root set are a closed genesis assignment. Dynamic children use ordinary process
Start; there is no configured catalogue of future application processes.

Use current ordinary structural facts for supplied objects. Do not copy a parent's
`RootFact` unchanged to the child: it binds an old process/source tenure. A direct
possession source keyed by preparation and child belongs to the normal
controlled-object representation; derive reader/writer operation through current
dynamic `Controlled.Operate` resolution. Relabelling establishes the child's
current tenure, placement, and delta incarnation by existing rules.

The child's own process object receives normal self-possession from its applied
Start. The parent also receives a checked direct normal grant for that same live
process object, grounded in target preparation and the exact applied Start result.
Its Herald installs that grant and a private process alias before `prepareChild`
returns. This is the name capability needed for `label(..., child.process)`, not
an alias pretending to be a capability. Neither grant makes the parent a second
residence or operator of the child process.

## 5. Preparation, remote transfer, and readiness

Use one retained workflow for local and remote targets:

1. Parent admission resolves every reference, checks current roles and possession,
   canonicalizes the selection by key, and retains one exact transfer commitment
   under the preparation request. This is the grant acceptance point; later loss
   of the parent's possession does not retract that accepted grant.
2. The target admits the transfer only in its active epoch, retains it, allocates
   one child identity, and submits the retained Start. Calls addressed to a joining,
   retired, or different-lineage target receive an explicit disposition rather than
   implicitly joining or starting another system.
3. The target applies Start, retains child normal grants, reconciles selected object
   facts, and allocates stable child-private aliases in key order, with `self`
   first. Object lifecycle/label knowledge covers the transfer's stated control
   and structural prerequisites before installation completes.
4. The target returns its exact process-name grant and opaque connection descriptor.
   The parent installs the name grant/alias and returns `PreparedChild`. This is
   preparation completion; it does **not** wait for child labels or delta alignment.
5. The parent arranges topology and completes ordinary per-object label calls.
   `awaitChildReady` optionally waits for the target's current readiness predicate.
   The OS child is then started with the descriptor and performs its initial claim.

For a remote target, the source retains `OfferChildPreparation`; the target retains
its receipt and replies with `ChildPreparationAccepted`, `PreparedChild`, or the
exact terminal rejection. Retransmission reuses the same source epoch/request
correlation and normalized transfer. Status/reoffer messages return retained
outcomes. Same-target retries never allocate new child identities or private names.
Unequal bodies under the same identity are ordinary invalid-request outcomes.

The public target locator denotes the application's chosen Herald address, using
the application endpoint in the CLI examples. The parent's Herald resolves it to
an admitted epoch and that epoch's advertised peer endpoint through its checked
catalogue and learned contact map. An unresolved contact waits for discovery or
returns the typed unknown-target outcome; it does not admit a new Herald. Send
EPRP transfer messages to the resolved peer endpoint, never to the application
port. The returned child descriptor retains the target's application endpoint and
exact epoch binding.

The transfer contains selected global identities, role/sort claims, source
process/residence, source-admitted normal-grant evidence, readiness/source-key
selection, and control/structural prerequisites needed to interpret them. It
includes retained object/definition publications or names ordinary repair work for
already disseminated facts. The target uses existing structural, sort-definition,
lifecycle, and authority admission; bytes in a descriptor cache alone do not
establish sort visibility. Control/peer delivery may arrive in either order and is
retained until prerequisites are applied.

This is a specific startup possession transfer between admitted benign Heralds,
not ordinary graph delivery or a claim that possession is globally visible. The
source attests its serialized local admission; every receiver need not reconstruct
the parent's private stores. Labels and authority still come from current semantic
state. No signatures or adversarial proof system are added. Initial copied normal
possession survives later parent store loss until child End or ordinary
controlled-object removal revokes it.

After transfer acceptance, source/target owners retain enough exact material to
finish without consulting the parent's private map again. If the parent ends,
accepted transfer work may finish with no reachable parent reply. If the source
Herald is retired before any survivor/target retains its offer, there is no claim
that lost in-memory work can be recovered. The target retains any accepted
preparation across source retirement; target retirement ends the child and seals
the record.

The [membership](herald-membership.md) join barrier queues new preparation acceptance with
other new mutations. It drains already-ready finite transfer/Start/name-grant
effects and required repair. Work still awaiting an external definition or other
unavailable prerequisite is retained beyond the seal and revalidated afterward;
the barrier does not require every accepted preparation to return. An unclaimed
`Prepared` record, future parent label calls, readiness waiters, and an absent OS
child do not block that drain. They are retained facts, not accepted unfinished
mutations. After activation, readiness is recalculated against the new generation's
established topology and alignment. Retirement uses immutable transfer origin plus
surviving settlement-generation evidence, not a special startup quorum or a frozen
assumption that there are three Heralds.

Readiness is a derived local predicate: child live here, target active, exact
selection installed, no relevant observation gate, and each required endpoint
current, normally possessed, labelled to the child, and operational. A required
delta also has its current placement/incarnation and completed alignment for the
applicable established topology cut. The target never waits for the child to issue
an application call to obtain this evidence. Control/peer progress continues while
preparation/readiness/claim waits are pending.

`PreparedChild` is available before readiness: otherwise the parent could not learn
the child identity needed for the label calls that make it ready. `awaitChildReady`
is an observation, not a lock or permanent capability. Initial claim rechecks
readiness atomically with session creation. A prior ready report can become stale;
later operation admission always checks current facts. Removed or ended required
objects yield terminal startup failure. Temporarily missing prerequisites,
untransferred labels, or alignment yield pending status listing the relevant keys,
not fabricated successful access.

## 6. Initial claim, retry, and cancellation

A connection descriptor encodes the target locator, lineage binding, and an opaque
attachment reference; it is never an application object/process handle. It may be
passed as `--connection DESCRIPTOR` or read from `--connection-file PATH`. The
library returns descriptors as values and prints nothing. No wire/API version or
compatibility discriminator is introduced.

Target lifecycle states are `Preparing`, `Prepared`, `Attached`, `Cancelling`, and
`Terminal`; readiness is derived within `Prepared`. Retain immutable preparation
outcomes separately from current lifecycle state. `ClaimInitial` carries a
client-generated stable initial-claim identity retained across transport retries.
Before readiness it remains pending and creates no session. At readiness, one
serialized transition consumes the initial grant, installs one session and logical
connection binding, and retains the exact `SessionOpened` result and startup access.

An exact retry reoffers that result if its first reply was lost. A distinct claim
cannot consume the same grant again. There is no interval in which consumption is
visible but its result/session is absent. After the client learns session and
resume material, normal `ResumeSession` owns reconnects. Initial-claim correlation,
process identity, session identity, request IDs, and TCP connections stay distinct.
Terminal disposition takes precedence over reactivating an old result: an ended
process/expired session cannot be recreated by replaying the initial claim.

A session whose first response was lost is already attached; unexpected disconnect
starts its ordinary absolute recovery grace. Exact claim retry recovers the
retained session within that grace without renewing the deadline. A never-claimed
prepared child has no application-session-loss deadline.

`cancelChild` is a retained lifecycle request for an unclaimed preparation,
available to its preparing parent and through an explicit administration handle.
The advanced `beginChild` call returns a `ChildPreparation` handle once source
acceptance is retained; `awaitPreparedChild` obtains its eventual `PreparedChild`.
The convenience `prepareChild` composes these calls, and its result retains the
same preparation handle. Cancellation/status can therefore name an accepted
preparation before its child identity or final descriptor is available.
Source-local cancellation can seal an offer that was never sent. If a forwarded
cancellation overtakes its offer, the target retains a sealed receipt under that
same source request key; a later offer cannot start a child.
The target serializes cancellation against initial claim. Cancellation winning
seals all claims immediately, retains one End intention, and reports success only
after ordered End is applied. Claim winning returns `AlreadyAttached`; cancellation
does not kill that child. During `Preparing`, cancellation prevents unsent Start,
or retains End-after-Start if Start may already be accepted. It must resolve the
retained Start result before choosing that outcome.

Cancellation does not roll back labels, topology, accepted publications, or store
handoffs. Transferred labels follow ordinary zombie rules after End. Relabelling
the 31 environment objects is 31 ordinary operations, not an atomic transfer. Expose per-root
results; partial failure requires explicit retry/cleanup and cannot be reported as
successful initialization. A moved delta starts a fresh incarnation: its old store
is not copied. Arrange retained context sources and alignment, or populate its new
store through the data plane.

Failed OS spawn is handled by the parent calling `cancelChild`. Parent termination
alone leaves the preparation intact. No automatic preparation expiry is required;
explicit cancellation, target retirement, or the end of the in-memory run reclaims
it. Stale/wrong-residence/cancelled descriptors have closed boundary outcomes.

## 7. Ergonomic application facade

The public handle is `Herald`: one scoped client session and its startup access.
It does not grant access to another process's namespace or all server state.
The [typed application API](typed-application-api.md) binds payload types and
session-qualified endpoint handles to checked startup evidence. Its convenience
calls and advanced `Operation result`/`Call result` interface share one pure client
owner. The [application package guide](../../application-api/README.md) documents
the exported facade. The heterogeneous wire union is checked against the retained
operation result type; a mismatch is an invariant or protocol fault.

Session-scoped lifecycle requests have a distinct EAPP envelope alongside the
closed eight-operation union. Application preparation does not confer EADM
privileges. Convenience calls submit once; await, status and cancellation retain
the original identity and never resubmit a mutation under a fresh one.

`connect` succeeds after initial claim/readiness and returns the session-owning
handle. `disconnect` is idempotent, sends orderly `EndSession`, closes and joins
supervised transport workers, and remains session-scoped. `withHerald` brackets
these steps even when its callback throws. Logical process End is explicit through
`endProcess` or administration; clean session closure retains existing semantics.
Calls after close return a typed closed result. Unexpected last-session loss
immediately retains the automatic End intention.

`endProcess` has an explicit lifecycle completion record outside the retiring
process's request/private-map state. Accept it only for the caller's own live
process, retain its correlation and exact End intention, and expose a correlated
`ProcessEnded` terminal only after ordered End applies. A restricted lifecycle
result query using the same attachment/preparation reference and End correlation
can observe a still-retained result without opening a session or recreating the
dead process. Construct the final reply effect before reclaiming terminal receipts
for that session; a query afterward returns absent. Include the
genesis launcher in the same lifecycle-result representation. Ordinary session
unavailability alone is not a successful acknowledgement of the End request.

Cancelling a blocked `wait` submits existing cancellation and resolves its retained
wake/cancel winner. Cancelling a caller awaiting an accepted mutation stops that
caller's wait, not the semantic obligation: the supervised owner retains/drives
the exact outcome while its scope exists. Advanced callers retain `Call a` to
inspect it. Scope cleanup closes/joins client workers without waiting indefinitely
for Oracle quorum; Herald-owned accepted work continues. An operation may therefore
take effect after local cancellation. Document this at the API; never resubmit a
cancelled mutation under a fresh identity as an implicit retry. Cancelling local
`awaitChildReady` does not cancel the child.

## 8. First launcher, protocol inventory, and example

The founder's checked genesis supplies exactly one launcher logical process and
complete root environment at the founder Herald. It is the sole initial parent;
every later application is created by preparation. The implemented command
`eclips-herald --bootstrap --bind 127.0.0.1 --base-port 7100 --launcher-output
.cabal/profile-0.2/launcher.eclips` explicitly writes its opaque descriptor after
launcher startup installation is ready. Without that option there is no automatic
descriptor output. Joining Heralds receive no private launcher environment, and
administration cannot mint one on each request. Other Heralds join through seed
discovery and checked admission; fixed manifests are also available for explicit
fixtures.

`eclips-hello-launcher --connection-file .cabal/profile-0.2/launcher.eclips
--publisher-herald 127.0.0.1:7101 --reader-herald 127.0.0.1:7201` connects as that ordinary process, creates two
environments with `newenv`, prepares two children at chosen Herald addresses,
establishes intended context paths, labels required endpoints, waits for readiness,
and launches two independent executables. `eclips-hello-publisher --connection
DESCRIPTOR` and `eclips-hello-reader --connection DESCRIPTOR` use the direct-result
facade. Test same-Herald and different-Herald children. The descriptor supplies only
initial access; subsequent rendezvous/object references travel through ECLIPS.

| Boundary | Responsibility |
| --- | --- |
| EAPP lifecycle | Session-scoped prepare/status/readiness/cancel requests and dispositions; exact initial claim; selected startup-access encoding; explicit own-process End. |
| EPRP Herald peer | Offer/accept/status/reoffer for retained child preparation and reverse child-name grant; checked transfer prerequisites. |
| EADM | Inspect/cancel a retained preparation and existing explicit End; no application-private handles in admin DTOs. |
| Oracle | Dynamic Start and applied process fact, separate from immutable genesis. |
| Client runtime | Descriptor parsing, stable claim identity, typed result dispatch, handle lifecycle, cancellation, CLI adapters. |

Change producers, consumers, canonical encodings, golden vectors, and tests together
and start a fresh experimental run. Reuse existing TCP/protocol framing and owners;
no shared-file side channel between Heralds or raw global object IDs on the child's
command line. Deployment entry points live in the `deployment` package;
launcher/publisher/reader remain ordinary example applications.

## 9. Implementation map and proof gates

Startup values live in `application-types`, the callable facade in
`application-api`, and retry/session ownership in `application-client`.
`Herald.ProcessPreparation` and its use-case composition own retained preparation;
`Herald.Application` owns private namespaces and claims. Domain startup admission,
Oracle process facts, and the application/peer/administration codecs meet at
checked boundaries. Runtime execution stays in `herald-runtime`, with independent
launcher/publisher/reader executables in the examples.

Required properties and integration gates:

1. Arbitrary selected shape, canonical key order, duplicate alias coherence,
   connected six-pair `newenv` result, and no capability gained from an ID alone.
2. Parent can name the live child; child-private names denote exactly the selected
   objects. Equal token bits across processes never imply shared private meaning.
3. Copied possession survives parent store loss/End; normal strength is never
   fabricated from weak possession. Object removal and child End revoke grants.
4. Dynamic source-pair admission permits eligible partial startup, rejects absent
   or unusable sources, and preserves noncircular root publication provenance.
5. Transfer/control arrival orders produce one child, one namespace, and one exact
   outcome. Same-Herald and remote preparation satisfy the same lifecycle model.
6. Preparation completes before labels; claim succeeds only at current readiness;
   activation/alignment need no child session. Exercise partial relabel, invalidated
   ready observations, join barriers, and ordinary store-incarnation handoff.
7. Startup attempts, duplicate claims, claim/cancel races, End/Start ordering,
   and target retirement cannot create a second or resurrected process/session.
   Established connection loss ends its session immediately. An own-process End
   terminal effect precedes disposal; lost delivery remains unknown once reclaimed.
8. Typed convenience calls refine client semantics. Every result belongs to its
   submitted operation; cancellation retains accepted work; cleanup joins workers
   without quorum. Pure owners remain deterministic.
9. Real separate processes exchange the greeting through supplied primordial sets
   on one and two Heralds, including failed spawn, parent exit before claim,
   explicit cleanup, and reconnect after an injected lost initial reply.

Direct grant admission, transfer retention through membership changes, and
readiness/label/alignment composition are proof obligations before the startup
contract is considered verified. Generated schedules use an independent
lifecycle model alongside the real-process examples. Recorded verification results
remain scoped to the source and commands actually tested.
