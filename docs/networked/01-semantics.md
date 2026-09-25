# Observable semantics and application API

This document owns the observable data-space model and the application-facing
contract for one source-coherent prototype build. Protocol and implementation
details may refine its completion points but MUST NOT add an application operation
or change these outcomes within that build. This is a semantic constraint, not a
promise of compatibility with a later build.

## 1. Data-space model

ECLIPS is an asynchronously replicated typed data space. It is not a message
queue. An application writes a state description at a nabla and reads the current
winning descriptions retained by one or more deltas.

### 1.1 Sorts, objects, and instances

A checked sort descriptor contains at least:

```text
SortDescriptor = {
    kind: regular | controlled,
    valueType,
    keyProjections,
    validityPredicate,
    obsolescencePredicate,
    totalApplicationRank,
    minimumRetention,
    immutable,
    optionalLabelField
}
```

The descriptor has one canonical encoding, including a domain-separation tag. The
prototype defines `SortId = SHA-256(canonicalDescriptorBytes)`;
the hash algorithm is a protocol constant rather than an Oracle allocation or a
system-local namespace. A receiver MUST validate a descriptor and recompute all
derived facts from the decoded value. Sender-supplied keys, ranks, lifecycle
classifications, or definition interpretations are never authoritative.

Descriptor fields are data in one closed, portable descriptor AST;
they are not Haskell functions, callbacks, dynamically loaded code, or names whose
meaning depends on one process. The first AST provides checked value-schema
constructors, field projections, a closed set of total scalar comparisons, a
lexicographic rank tuple, and a closed set of total validity/obsolescence
expressions. Canonicalization rejects ambiguous field names, ill-typed expressions,
partial comparisons, and any rank that is not total for the
declared value schema. Every Herald and Oracle replica uses the same pure evaluator
and canonical encoding. The cereal-derived raw representation has canonical
golden vectors. Its domain separation makes that
one encoding unambiguous; it is not a compatibility version. A development
change may replace the grammar or encoding and thereby change `SortId`s, but old and
new forms never coexist in one run: all roles change together and start a fresh
genesis.

The scalar grammar includes closed symbolic enums. An enum schema is a non-empty
ordered list of pairwise-distinct exact text symbols. Symbols are neither
case-folded nor otherwise normalized, and declaration order is semantic: it
defines enum comparison order and the ordinals used when an enum projection is an
object-key or explicit rank term. Reordering members therefore changes the
descriptor and its `SortId`. `EnumValue`, descriptor `LiteralEnum`, and query
`QueryEnum` carry the exact symbol text; admission requires that symbol to be a
declared member. The complete-value rank fallback remains the canonical encoded
value, as it does for every other value kind.

Two valid instances of one sort describe the same object when their key projections
are equal. A regular object is named by `(SortId, Key)`. A controlled object is
named globally by one `GlobalObjectId`; that identifier names at most one object
across all controlled sorts. Its `SortId` is immutable type metadata, not a second
identity namespace.

A regular sort's key-projection list MAY be empty. The resulting empty tuple is
the one key of that sort, so all of its valid values are instances of the same
object. A controlled sort still requires exactly one direct unique-ID projection.

`GlobalUniqueId` is the global unique-ID value carried in a value field.
`GlobalObjectId` is a nominal controlled-object ID with exactly the same 256 value
bits when converted from that global unique value. `ProcessEpochId`, `NablaId`, and
`DeltaId` have corresponding exact-bit nominal conversions to/from
`GlobalObjectId`/`GlobalUniqueId`. Constructing or decoding any of these types proves
only byte shape and nominal domain; it does not prove that an object exists or has
the claimed role. A Herald-local admitted controlled publication establishes that
Herald's object-to-sort/lifecycle knowledge, checked predefined reconciliation
establishes each role, and every operation performs the relevant stateful lookup.
No Oracle allocation or globally canonical object record is required. These domain
conversions are absent from the
application dependency cone, so an application cannot use them to cross the
private/global type wall.

`HeraldEpoch`, `StoreIncarnationId`, and other operational identities are unrelated
domains and cannot be induced from global unique-ID bits.

For a controlled sort:

- the key is exactly one field of the unique-ID type;
- the sort may designate one label field;
- the label field is not part of the key and is not inspected by validity,
  obsolescence, or application rank;
- an identifier is instantiated at most once in the system lineage;
- current-to-obsolete and current/obsolete-to-deleted transitions are monotone;
- an obsolete or deleted object never becomes current again; and
- an immutable object may be instantiated once, forwarded, relabelled, or deleted,
  but not updated with another application state.

For an application-origin object created through controlled `newid(p)`, the
at-most-once property comes from the unique-ID premise, the one home-Herald
reservation, and the reservation's single winning first use. It is not an Oracle
approval. Closed system origins have their own single-winner proof: checked genesis
uses pairwise-disjoint assigned identities and one genesis-only installer;
the Herald-managed process object uses the generated identity and exact retained
`StartProcessEpoch` result, which creates no root bundle; and `newenv` atomically
retains one generated manifest/root-stage set before retry can observe it. None of
those origins fabricates or consumes an application `newid(p)` reservation, and no
general application call can select their IDs or source path. Each Herald retains
enough hidden object-to-sort, lifecycle, label, and suppression state to reject a
contradictory reuse and to prevent a current value from reviving an object already
known obsolete or deleted. Profile 0.1 retains that state for the finite run.

A regular sort MUST NOT declare itself immutable: independent processes may publish
different values for the same regular key, and resolving that conflict necessarily
changes some observer's value. Content addressing makes `sort-sort` effectively
single-valued without misclassifying it as immutable.

### 1.2 Total winner order

The paper's ordering recipe is replaced by one total, deterministic winner key:

```text
winnerKey(instance) =
    (lifecycleRank(instance),
     totalApplicationRank(instance.value),
     instance.publicationId)

lifecycleRank(current)  = 0
lifecycleRank(obsolete) = 1
```

`totalApplicationRank` MUST be a total order over valid values of the sort. The
stable `PublicationId` breaks the remaining ties. A sort descriptor that cannot
provide a checked total rank is invalid.

Consequences:

- application of the same set of publications is independent of network order;
- applying a duplicate is idempotent;
- obsolete state always defeats current state;
- current state cannot resurrect an obsolete object; and
- different obsolete payloads need not converge byte-for-byte, although every
  Herald MUST agree that the object is terminal.

Publication identity is defined in [03-protocols.md](03-protocols.md). It includes
an authority epoch so a nabla handed to a new process never reuses the previous
writer's identity range.

### 1.3 Graph and reachability

The data-space graph contains:

- neutral vertices, which only route;
- nablas, each accepting writes of exactly one sort;
- deltas, each exposing a local store for exactly one sort; and
- directed edges, each traversable by every sort, with a preserving or weakening
  strength.

Within the common controlled-instance value that supplies the edge's
`GlobalObjectId` key and optional label, the checked prototype edge-specific
payload is exactly:

```text
EdgePayload = {
    sourceVertex,
    destinationVertex,
    strength: preserve | weaken
}
```

The carrier field has the closed enum schema `preserve :| [weaken]`. Those exact
lower-case symbols are the only admitted values, in that semantic order; another
symbol or another scalar kind is invalid at checked-publication admission. The
numeric `0 = preserve` and `1 = weaken` codes remain only in normalized internal
startup/reconciliation evidence transcripts. They are not application values,
descriptor literals, or an alternative edge-carrier encoding.

It has no predicate, allow/deny set, sort reference, or dormant selector field.
Every admitted edge is available to application sorts, predefined topology-carrier
sorts, and `sort-sort`. A selective-edge value is invalid rather than silently
treated as universal. Introducing one later means replacing the semantic type,
canonical catalogue, codecs, vectors, and tests in lockstep for a fresh run; it is
not a compatible extension and adds no version discriminator.

An instance written at nabla `p` is eligible for delta `d` exactly when:

1. `p` and `d` have the instance's sort;
2. `d` is active;
3. a directed path from `p` to `d` exists; and
4. the accepting Herald has a current physical placement record for `d`.

Sort selection occurs at the typed nabla and delta, not at an edge. Vertices typed
for another sort may still lie on a routing path; they do not change which edges
are traversable or make a differently typed delta an eligible destination.

This is **graph reachability**. It does not require a TCP connection to be open at
that instant. Temporarily disconnected destinations remain in the frozen cut and
the source queues their publications for retry.

For a normal source instance, the destination strength is normal when at least one
eligible path contains no weakening edge; otherwise it is weak. A weak source
instance remains weak on every path. Weak state MUST NOT become normal through
delivery, forwarding, alignment, duplication, or path merging.

### 1.4 Active and passive entities

Nabla and delta entities are induced by controlled predefined objects. Such an
entity is active for process epoch `P` only when all of the following are true:

- its defining controlled object is canonically current;
- its canonical label owner is `process(P)`;
- `P` is canonically live and resident at one Herald;
- its carried sort has an effective definition at that resident Herald; and
- `P` normally possesses the defining object.

Matching a label alone is insufficient. When the conjunction does not hold, the
nabla or delta is passive and behaves as a neutral vertex. A passive delta has no
application-visible store. Activation creates a fresh `StoreIncarnationId` at the
resident Herald and starts context alignment. Passivation ends that store
incarnation and cancels its waits; an old publication MUST NOT be redirected to a
later incarnation with the same `DeltaId`.

The carried-sort condition is checked at the owner's Herald. Another Herald's
missing definition does not establish that a remote entity is passive; its
owner's placement evidence remains authoritative. A nabla cannot admit data
publication without its effective carried sort. A passive root retains its
defining object and neutral graph role, so it may still lie on a path and may
later activate when all conditions hold. The structural stamping prerequisites
in section 2.3 still apply before a newly accepted description induces that root.

The isolated `SystemBootstrapWriter` identities in the checked index-zero
projection are not application entities. They have no process label, possession,
private alias, store, or live publication path. Dynamic Start creates no roots,
and those genesis identities cannot justify a runtime publication or supply
`newenv` authority.

### 1.5 Frozen routing

The Herald that accepts a `write` or `forward` computes its destination deltas and
path strengths once from its current reconciled graph and placement directory.
That immutable set is the publication's routing cut.

- Deleting an edge later does not recall or cancel the publication.
- Adding an edge later does not add a destination to the publication.
- A destination missed because the accepting Herald had not yet learned its route
  receives later live data or context alignment, not a retargeted old publication.
- Revocation stops future routing; it cannot erase data already routed.

All destinations at one peer Herald are grouped into one direct peer message. The
peer applies the publication only to the exact named delta store incarnations. It
does not recompute a wider route and does not forward the publication.

## 2. Delta stores and context

### 2.1 Visible and retained state

Each active delta store has two related views:

```text
DeltaStore = {
    visible:  Map ObjectKey StoredInstance,
    retained: Map ObjectKey RetainedWinnerAndSuppression
}
```

Each `StoreIncarnationId` starts at `StoreRevision` zero. Revision zero denotes
the empty retained-change prefix. The revision advances exactly once when the
hidden retained winner, suppression, or joined strength changes; it does not
advance for receipt alone, an equal/older duplicate, readiness bookkeeping, or a
visible-only `local-take`. The retained log is therefore indexed by actual retained
state transitions, not by delivery attempts.

Every retained-history entry carries a canonical destination-free semantic source
and its exact `(SortId, SortDefinitionOccurrenceId)`, source-topology prerequisite,
and Oracle-control prerequisite. Where the source is structural, it retains the
exact structural occurrence/stamp as part of that source evidence. It contains no
destination delta/store set, frozen-route batch, peer stream sequence, or
receiver-specific digest. Alignment can consequently replay the same evidence to
a new destination without retargeting an old `PublicationBatch`.

`read` observes only `visible`. Delivery evaluates the publication against
`retained` first. A newer accepted winner normally becomes both retained and
visible. An older or duplicate publication changes neither.

Strength is ordered `weak < normal` and is joined separately from publication
provenance for one equal logical state. Two publications have equal logical state
when their sort, object key, lifecycle, and canonical value bytes are equal after
canonical label rewriting; `PublicationId` is deliberately excluded. A later weak
`forward` of an unchanged value may become the winning provenance, but it cannot
downgrade a currently visible normal copy of that logical state. A normal route for
the same logical state may upgrade weak to normal.

The retained and visible views track this join separately. A strength upgrade alone
does not re-expose a locally taken value, and hidden retained normal state does not
silently restore a taken capability at the same delta. A genuinely newer
publication may re-expose the value with its incoming strength, after which new
same-logical-state routes may upgrade it. A different logical state installs its
own routed strength. Alignment attenuates the source's retained historical strength
by the frozen context path in the same way as live routing, so a weak source never
becomes normal.

`local-take` removes matching entries from `visible` but not from `retained`.
Therefore:

- the operation is local and never becomes global deletion;
- replay of the same or an older publication does not undo the take;
- a genuinely newer current publication may make the object visible again;
- obsolete suppression remains after its payload is taken; and
- a delta remains a complete historical source after taking visible data.

The prototype MAY retain this hidden state for the life of the store incarnation.
Production reclamation is deferred.

### 2.2 Context and alignment

The directed edge relation used for SCC calculation is identical for every sort.
For one sort, contract that graph's strongly connected components and treat only
active deltas of the subject sort as state-bearing cut points; a passive delta
behaves as a neutral vertex as specified above. The context of a delta class is the
set of immediate upstream same-sort active-delta classes: upstream classes from
which it is reachable without crossing another active delta class of that sort.
Contexts remain per-sort because the typed active-delta cut points differ, not
because an edge filters traffic. Adding or removing an edge may therefore affect
context generations for every effective sort with typed endpoints on an affected
path. An ordinary non-delete relabel that assigns a different live process
creates a new authority tenure but does not change
the induced edge or its reachability. A same-owner result advances the object label generation and creates a new
release revision/fence outcome without minting another authority tenure. Relabelling to
void retains the preceding tenure only as inapplicable history; assigning a live
process from void creates a new tenure.

A new or strengthened context relation creates an identified continuing alignment
subscription. The destination Herald selects one certified historically ready
representative of each source class, requests a retained-state snapshot at one
source-store revision, and then receives every later retained transition in
contiguous revision order for as long as the relation exists. A snapshot contains
retained winners and suppression facts, including values no longer visible at the
source. Continuing changes close the race in which a publication reaches the old
source just after snapshot capture while its publisher has not yet learned the new
destination placement.

Every merge, split, context change, or physical-member change creates a generation.
Its `ContextClassGenerationId` is the sole canonical digest of its
`AlignmentCut`; no independently carried alignment-cut digest exists. The cut
contains the exact established topology cut, exact member incarnations, an
alignment-plan attempt, and a physical-placement vector with exactly one positive
sequence for every active
`HeraldEpoch` in the named membership generation, in ascending epoch order.
Predecessor certificates gate construction and bootstrap admission, but certificate
bytes/digests are not hash inputs: predecessor evidence contributes only its
sorted generation IDs and exact fresh-member bases.

Predecessors are selected deterministically from the immediately preceding
established alignment cut. They are the sorted unique prior latest generations of
the same `SortDefinitionOccurrenceId` whose member `DeltaId` set intersects the
new class, plus each prior latest source-class generation whose context relation to
this destination class is replaced by the transition. Each exact new member whose
`DeltaId` occurs in none of those predecessors contributes a fresh base. Missing
required predecessor certificates block construction; their arrival cannot change
the selected set or its order. This rule applies within an ordinary plan sequence.
A checked Store-loss report permits the fixed announcer to start a fresh reset
attempt once the selected placement incorporates the loss, or checked membership
has retired its home. A reset creates every class from its current members' fresh
bases, with no predecessor plan or generation ancestry; current context relations
still apply. It abandons obsolete executable alignment work while preserving
Store contents and immutable historical evidence.

An alignment-plan attempt is the lexicographically ordered pair
`(membershipChangeControlIndex, retryOrdinal)`. Genesis uses control index zero;
ordinary successor plans inherit their attempt. A reset uses the selected
membership generation's Oracle change index and an ordinal greater than every
observed attempt in that epoch. A later membership epoch therefore supersedes
old-announcer attempts even when their retry ordinals were never observed.
The pair is part of the complete plan identity and each newly created cut digest.

The generation is not historically ready until every member has imported a
complete prefix from every predecessor and fresh base and the continuing
subscriptions are live. A predecessor-bootstrap prefix counts only after the
source Store's old-cut input closure has been recorded as described below. Hosts exchange
per-member readiness evidence before issuing a complete source certificate.
Structural SCC membership alone never certifies equivalence.

Alignment uses the same winner transition as live publication. A newer live value
may safely overtake an older snapshot or change. Structurally equal obligations
created by different topology changes have distinct occurrence identifiers;
completion of an old occurrence MUST NOT discharge a new one.

Alignment transfers already-admitted Store state; it does not exercise the
original publisher's authority again. In the benign Herald model, the admitted
source and exact obligation/cut authorize that transfer. The receiver checks the
immutable sort occurrence, canonical value and publication identity, transfer
binding, and applicable lifecycle suppression, but does not reconstruct the
original writer's authority from historical labels or topology. Original source
coordinates remain immutable provenance. Ordinary publication admission still
checks writer authority before admitting new data into Store state.

A retained value can be arbitrarily older than a new alignment transfer carrying
it. The bounded-latency reclamation contract applies to captured work: that
transfer must be applied or permanently abandoned within its permitted lifetime.
Replaying an expired captured transfer does not create a fresh lifetime. The age
of a still-valid Store value therefore does not by itself pin its original writer
authority history. Profile 0.1 retains its finite-run suppression state; the later
timed reclamation workflow remains deferred.

Promotion creates an unsourced `AlignmentObligation`: it binds the causal
structural occurrence, destination class generation and exact destination store
incarnations, semantic source class, and the frozen `Normal | Weak` context-path
strength. It deliberately names no representative Herald, source store, or
certificate digest. Only later deterministic representative selection against an
exact `HistoricalCertificate` creates the supplier- and certificate-qualified
`AlignmentAttempt`. Applying its snapshot or changes joins the retained source
strength and attenuates it by that frozen path strength; neither source selection
nor a later graph may strengthen or reinterpret the obligation.

An obligation is stable across loss of a selected source incarnation; an attempt
is disposable. A checked `AlignmentSourceIncarnationLost` for the exact active
attempt retires that attempt, globally excludes that exact
`(HeraldEpoch, StoreIncarnationId)` from later selection, and deterministically
selects the least eligible non-excluded representative with a fresh subscription
identity. If no representative remains, the obligation stays explicitly pending
without an attempt or readiness claim. A terminal receipt for the retired
subscription makes every late old-attempt frame idempotently terminal and prevents
it from contributing to the replacement attempt. TCP binding loss, reconnect,
delay, or a retry deadline is not source-incarnation loss and MUST NOT change the
obligation, attempt, subscription, exclusion set, or revision transcript.

Checked `AlignmentDestinationIncarnationLost` for an exact destination store has
different semantics. It tombstones the whole alignment-plan coordinate
`(alignmentPlanAttempt, sortId, sortDefinitionOccurrenceId, topologyCutId,
physicalPlacementRevisionVector)` containing that store, discards all of that
coordinate's active obligations, attempts, and incomplete imports, and rejects
late frames against the tombstone idempotently. The Herald immediately re-drives
the retained structural consequence debt against the latest admissible physical-
placement vector; it does not leave a terminal active obligation waiting and does
not retarget work into an inactive retained store. If no vector with a replacement
store is admitted yet, the debt remains unpromoted rather than becoming active
terminal work. A successor coordinate receives
fresh generation, obligation, attempt, and subscription identities. A
predecessor-bootstrap obligation whose selected source is lost may reselect another
certified representative of that predecessor generation. A fresh-member base has
no alternate representative under the same plan coordinate: loss of its exact
store invalidates that coordinate and requires replanning. A prefix already
fulfilled locally remains fulfilled after later source loss; source recovery does
not reopen one-time imports.

A receiver that cannot use an announced plan because its exact predecessor or
carried Store incarnation was lost retains one immutable `AlignmentPlanObsolete`
report per plan and reporter. It names the lost home, Delta, Store incarnation and
observed withdrawal placement revision. Reliable delivery is independent of the
publication stream. The fixed announcer holds the report until its current
placement reaches that revision and excludes the incarnation, or current checked
membership excludes the home, then authors the fresh reset described above. A
missing message or TCP disconnection alone does not authorize reset. A reset may
bypass unreceived ordinary descendants of the failed plan; delayed announcements
and acceptances from older attempts cannot restore their executable work. Old
readiness, certificates and captured historical recipients remain valid evidence.

Historical readiness is level-triggered internal state. An active delta can be read
while its initial snapshot/change prefix remains incomplete, so an early `read` may
be empty or partial. Readiness may become true once that prefix is applied even
though the anti-entropy subscription remains live for later changes. A delta that
is not historically ready MUST NOT serve as a complete alignment source.
Historical readiness does not add a ninth application operation.

A destination may request context from a peer only after the peer has installed the
matching established structural vector/cut, the requester has durably received that
peer's `AlignmentCutAccepted` for the exact combined topology/placement cut, and the
peer has supplied the matching context-class
generation, exact source store incarnation, and `HistoricalCertificate`.
Componentwise vector coverage proves
that the source has applied the required structural occurrences, but a certificate
is still required because a later cut may have removed the relation and because
structural application alone does not prove imported history. The requester MUST
wait for all of that exact source evidence and MUST NOT intentionally send an early
context request.

Snapshot identity is semantic rather than packet-shaped. Its digest covers the
canonical ordered retained state and its class-generation, source-incarnation, and
base-revision binding; chunk count, chunk boundaries, and transport segmentation
are excluded. A receiver validates the reconstructed complete semantic snapshot
against that digest before committing its prefix.

Cutover records `HeraldPublicationPrefix = Empty | Through
HeraldPublicationPosition`; `Empty` is the first-class no-prior-publication case,
not a numeric sentinel. Every publication source in the captured generation settles
that old-cut prefix and, when it is remote from a predecessor host, then appends an ordered
`AlignmentRouteCutoverMarker` toward that host. The predecessor records its own
source's settlement locally rather than sending a marker to itself. It records an
input-closure receipt at local store revision `q` only after the exact complete
generation-qualified source cutover set—one local settlement plus one marker from
every other active-generation Herald—and after every exact
incoming alignment closure owner has applied a revision-qualified `AlignmentLive`
prefix and returned its ordinary `AlignmentAck`. An input-closure-only subscription
may outlive removal of its context relation until this local receipt exists, then
ends independently of any child acknowledgement.

The recorded `q` is a lower bound, not a frozen transfer snapshot: the Store and its
continuing subscriptions may advance beyond it. A predecessor-bootstrap transfer
may stream snapshot and changes early, but withholds its first readiness-counting
`AlignmentLive` until the exact source-store input-closure receipt exists and that
transfer has sent through at least `q`. There is no distinct
`AlignmentHandoffMarker`, captured-destination retention, child-cancellation gate,
revision-marker, or handoff-ack message. If the selected source is lost before
closure or before bootstrap fulfilment, a replacement attempt must establish its
own exact source-store closure; with no eligible source, readiness remains pending.

Under fair peer delivery, fair local work scheduling, a quiescent topology, and at
least one eligible source for every required relation and input closure, every
alignment obligation eventually completes and all relevant delta stores converge
according to the winner rule. If every eligible source incarnation has been
explicitly lost, stable pending work is the truthful result; the protocol does not
invent completeness.

Profile 0.1 specifies the pure transitions after an exact source- or destination-
incarnation loss has been checked. Membership retirement derives that fact from a committed
non-voting-Herald retirement after recovery grace and fresh voter-host probes.
Durable restart/restoration and production failure-detector calibration remain
deferred; transport churn alone is not an incarnation-loss fact.

### 2.3 Structural progress and common cuts

Structural carriers are ordinary controlled publications, not Oracle commands.
Every Herald assigns a contiguous local `StructuralSequence` to the
topology-affecting occurrences it originates, but only when every structural
identity/role, effective sort, control, and authority fact referenced by that
occurrence is already applied locally. Application acceptance may retain an
unsequenced structural-admission stage with a stable publication identity; it does
not yet create a stamped occurrence, peer outbox item, or vector advance. When the
prerequisites become true, sequence allocation, stamping from the complete current
applied vector, finite local reconciliation, local-vector advance, and stamped
outbox insertion occur atomically. A same-source occurrence therefore cannot
depend on a provider at a later local sequence. Every Herald maintains an applied
version vector with one component per Herald epoch in its named membership
generation; a component advances only after the corresponding occurrence and all
of its finite immediate local effects have been reconciled. Receipt alone does not
advance it. Retirement seals the old generation and its retired-source union; the
successor generation has its own vector and never renumbers old components.

Admission likewise starts a successor generation from a checked old-member cut.
Its activation preserves every old component and introduces the new source at
zero. Once an old source captures its finite join input, it assigns no further
structural sequence in that admission attempt. A previously unresolved stage
that becomes ready remains retained until the attempt is invalidated, cancelled
or activated. Receipt and replay of already-stamped occurrences continue so the
old members can establish the common seal cut without future application input.

Predefined system-view delivery covers the installed active membership, including
sort-definition observations that have no structural carrier. Extending this
mandatory delivery to an admitted Herald creates no graph edge or application
writer. Ordinary application destinations remain governed by their frozen route
and capability-qualified alignment.

This rule covers every new topology-affecting origin: `write`, structural
`forward`, `newenv`, and internal creation of a new
semantic structural publication. Repair or catch-up of an existing occurrence
reuses its exact stamp. A multi-root call reserves consecutive
`HeraldPublicationPosition`s in canonical manifest order before retaining its
individual stages, so readiness and sequence allocation have one total source
order with no equal-position roots.

Heralds advertise their cumulative applied vectors directly to the deterministic
topology-cut announcer for the named membership generation. The announcer retains
its own local report and the latest report from every other active member; other
Heralds do not need the full report matrix. The componentwise minimum of one
current report from every member is the exact stable frontier used for the next
proposal. Reconnecting or reoffering the announcer binding sends the latest report
again. Installing a successor structural membership base immediately offers that
generation's report to its announcer, even if no further local work occurs. A
joining observer cannot report before its admission base is installed.
It is causally closed because
every included occurrence was applied by every reporter only after its predecessor
vector and control prerequisite. An arbitrary lower vector is not a legal
candidate merely because it is bounded by that minimum. An exact common topology
cut contains its predecessor, membership-generation/member-set coordinates, that
vector, the stable applied-control prefix, and a canonical
digest covering the winning induced topology and its effective sort/label
interpretation. Equal vectors at the same frontier MUST produce equal topology digests
regardless of the order in which concurrent origins were observed.
The digest describes the effective projection at that frontier. An absent
object contributes no deletion-provenance row. Ordered deletion and retirement
facts separately prevent stale input from restoring it; equality of the topology
digest does not replace those admission checks.

Only one system-wide topology-cut proposal is open in profile 0.1. A deterministic
announcer proposes the exact current all-member stable structural vector and stable
control prefix from the shared predecessor cut. The candidate cannot regress from
that predecessor and must cover every included occurrence's predecessor vector and
control prerequisite. Every active Herald in the named generation returns immutable
acceptance only after it can reproduce the exact causally closed vector and digest;
the announcer then distributes the complete all-member
`TopologyCutEstablished` evidence. This prevents two opposite arrival orders from
creating incompatible public predecessor-generation lineages while preserving
asynchronous local routing between established common cuts. Per-sort placement and
alignment cuts remain separate.

Checked startup installs one identical distinguished genesis topology-cut ID at
every Herald from the `SystemId`, member-set digest, initial projection digest,
all-empty structural vector, and control zero. The first dynamic cut names this
non-recursive root; each later cut names the preceding established cut.

Structural stream completion means local reconciliation and durable creation of
all resulting alignment/cutover/input-closure work. It MUST NOT wait recursively for
that asynchronous work to finish. The application-visible structural stabilization
result waits until the exact common cut is established, not merely for cumulative
stream completion.

A compact Oracle-ordered structural activation index is a planned fallback if the
version-vector/canonical-cut model cannot be verified with acceptable complexity.
It is not part of the initial profile. Even that fallback would order only a
structural occurrence digest and would not carry or approve the publication,
compute graph/placement, select context sources, or prove historical readiness.

## 3. Controlled objects and capabilities

Application values contain process-epoch-local `PrivateUniqueId` values. Operation
arguments use nominal private wrappers such as `PrivateObjectId`,
`PrivateProcessId`, `PrivateNablaId`, and `PrivateDeltaId` so identifiers from
different API roles cannot be interchanged accidentally. These wrappers are
sort-free names: resolving one first produces its
`GlobalUniqueId`/`GlobalObjectId`, after which the Herald
checks the object's canonical sort, role, lifecycle, and capability. The private
identifier alone conveys none of those facts.

For each live `ProcessEpoch` record, the resident Herald owns one stable
bidirectional map keyed by that record's `ProcessEpochId`:

```text
PrivateUniqueId <-> GlobalUniqueId
```

The map and its unsigned 64-bit never-reused next-local counter belong to the
process epoch, not to an application session. All sessions attached to that epoch
observe the same private identifier for the same global identifier. Equal local
bits in another process epoch are unrelated. Copying a private ID between process
epochs therefore either fails to resolve or denotes a different binding; it never
imports the source process's name or authority. “Private” is a namespace and
encapsulation guarantee in profile 0.1, not a claim that benign-profile counter
values are cryptographically unguessable.

Bindings never change during a process epoch. The two-level representation retains
the paper's encapsulation boundary, but profile 0.1 does not implement federation,
system merge, or live global-ID renaming. Any profile that permits rebinding must
redesign the atomic rewrite of stored values, keys, retained history, topology,
in-flight work, and durable process maps.

Application-to-Herald translation recursively replaces every declared
`PrivateUniqueId` field, at any nesting depth, with its mapped `GlobalUniqueId`
before semantic validation, storage, peer delivery, or Oracle use. An unmapped
private ID is rejected before semantic acceptance. Herald-to-application
translation recursively replaces every declared global unique-ID field with the
existing reverse mapping or atomically allocates one fresh private ID and installs
both directions. Repeated occurrences and later results reuse the same binding.
This applies to bare unique IDs as well as controlled-object references; it is not
limited to top-level keys. Translation follows the checked value schema, so bytes
in a non-unique-ID field are never interpreted as an identifier.

When one result contains several values or identity occurrences, localization
visits values in the result's specified canonical order, then visits each value in
schema preorder; record fields are visited in ascending checked field-name order,
and a label is visited at its schema position. The first occurrence of an unmapped
global ID allocates its private name and every later occurrence reuses it. No
`Map`/`Set` traversal or runtime arrival order may choose observable local counters.

Process/zombie labels are the explicit companion case: their application form
carries `PrivateProcessId`, while the internal form carries `ProcessEpochId`.
Globalization resolves the underlying private token through the same map and then
statefully validates its process role; localization reuses or allocates the
process-qualified alias for the established internal process ID. Void labels are
unchanged. The same rule applies to typed label-schema query literals, and no
application value, query, result, or effect exposes `ProcessEpochId`.

The profile-0.1 `PrivateUniqueId` representation is a positive unsigned 64-bit
local counter value; zero is invalid. Its meaning is qualified by the
`ProcessEpochId`, not by a session or connection. Exhaustion is excluded by the profile-wide
no-overflow assumption. Its global meaning—not its current numeric
representation—is opaque at `Application.API`: applications may retain, compare,
encode/decode, inspect, and structurally construct any positive token. Construction
does not install a process mapping, assert a global role, or confer authority; an
unmapped token still rejects at Herald admission.

`SortId` is the deliberate exception: it is a public content address, is not a
unique-ID field or controlled identity, and crosses the application boundary
unchanged. Global unique IDs and `GlobalObjectId`s otherwise remain inside Herald,
peer, store, and Oracle representations and MUST NOT appear in `Application.API`
types or application RPC DTOs.

`SortId` and `SortDefinition` are deliberately different: they are shared regular
data, not controlled identities or handles. Knowing, reading, writing, or taking a
sort definition confers no `name`, `forward`, `label`, `update`, or possession
capability. A process that knows the structured `ApplicationSortDescriptor`
republishes that form with ordinary `write`; its Herald re-admits and canonicalizes
the draft before deriving the public `SortId`. Capability checks concern the
controlled nabla through which it writes, not the content-addressed definition
value. The prototype descriptor AST has no controlled-identity literal constructor.
If a later direct redesign adds one, those individual references still require
ordinary `name` authorization before publication; this would be a lockstep breaking
change, not a negotiated extension.

A process's effective capability is derived at its Herald from its live epoch,
active entities, publisher-retained state, and replicas in its operated deltas.
Normal possession grants more authority than weak possession:

| Capability | Minimum conditions |
| --- | --- |
| `forward` | The process possesses a current or retained obsolete controlled object, weak or normal, and the retention interval has not expired where applicable. |
| `name` | The process normally possesses a current controlled object. |
| `label` | The process normally possesses the controlled object and its current label permits the requested transition. |
| `update` | The process normally possesses a current object labelled void or to itself. |
| `operate` | The defining object of the nabla/delta is current, labelled to the process, and normally possessed by it. |

Successful controlled instantiation or update gives the publisher normal
possession even when no destination delta is local. Possession derived from a delta
is removed when `local-take` removes the last visible normal replica available to
the process. Hidden retention is Herald protocol state and does not itself confer a
new application capability.

When `read` materializes a value, the Herald performs the recursive localization
above against the caller process's map. Reading a controlled object also records
the normal/weak possession conveyed by the still-visible replica. `local-take`
performs the same localization before removing the visible replica. The private ID
remains resolvable for the rest of the process epoch, but store-derived possession
is then recomputed and may disappear. A referenced ID in an ordinary value can
therefore be represented privately without inventing normal possession.

Heralds enforce these rules even in the benign profile. They are semantic rules,
not a claim that the prototype resists a malicious client or compromised Herald.

## 4. Predefined sorts and inside-out induction

The immutable primordial catalogue assigns roles to the predefined sorts needed to
describe:

- sort definitions;
- neutral vertices;
- edges;
- nablas;
- deltas;
- process epochs.

`sort-sort` is the one regular predefined sort. Its primordial descriptor is known
before inside-out induction and has this semantic value shape:

```text
SortDefinition = {
    sortId: cryptoHash(canonicalDescriptorBytes),
    canonicalDescriptorBytes
}
```

Its key is `sortId`; validity re-parses and validates the canonical descriptor and
recomputes the hash. That rule is a closed intrinsic of publication admission for
the exact primordial `sort-sort` carrier, not a portable predicate-expression AST
node: the descriptor's portable validity predicate is `Always`, and the sole
checked-publication constructor additionally enforces this intrinsic before it can
produce a publication. Embedded bytes are admitted as an application descriptor;
the only structural/Herald-managed exceptions are byte-identical members of the
fixed primordial catalogue, admitted under that closed profile. Thus an application
cannot use `sort-sort` to invent a seventh structural carrier. Its obsolescence
predicate is `Never`. Like every regular sort,
`sort-sort` is not marked immutable, but—absent a cryptographic collision—its
validity and content-addressed key leave only one valid value for an object.
Independent processes defining the same canonical descriptor therefore publish the
same regular object. Repetition is expected and is precisely how processes agree on
the sort without a naming service, unique-ID allocation, capability transfer, or
Oracle decision.

Profile 0.1 relies on SHA-256 collision resistance: two descriptors with the same
`SortId` are treated as the same canonical descriptor. It defines no forced-hash
collision test, quarantine state, dependent containment, or recovery protocol.
Canonical decoding and recomputation of a claimed hash remain mandatory admission
checks.

The content address is stable across the system lineage, but retirement gives its
effective presence an occurrence epoch:

```text
SortDefinitionOccurrenceId =
    digest(SystemId, SortId, lastResolvedRetirementControlIndex | genesis)
```

All equal publications before a retirement belong to one occurrence regardless of
publisher or `PublicationId`. A successful retirement changes only the occurrence
epoch, not `SortId`; the first equal post-retirement write establishes the next
occurrence. Protocol routes, publications, and alignment certificates name the
occurrence so pre-retirement evidence cannot be reused after redefinition.

Neutral-vertex, edge, nabla, and delta definitions are controlled, immutable,
labelled, and never application-obsolete. Every non-label field is fixed at
instantiation: an edge retains its endpoints and strength, a nabla retains its
sort and sequencing object, and a delta retains its sort. A different definition
requires a fresh object identity; replacing an edge means creating a new edge
and deleting the old one. The operations remain separate, with their ordinary
completion and visibility rules.

Immutability does not prevent forwarding, relabelling, deletion, or ordered
predefined disappearance. Label changes may transfer ownership or change a
nabla or delta between active and passive, and delta stores still evolve through
ordinary data operations. Their carrier layout may use one controlled sort
per role or a checked tagged union, but it cannot share the regular `sort-sort`
carrier. The catalogue mapping and codecs are fixed for one system lineage.

The predefined edge codec combines the ordinary controlled-instance fields with
the closed `EdgePayload` shape above.
Inside-out induction creates the universal edge only after its endpoint and control
prerequisites resolve; it never waits for a referenced allowed-sort definition,
because no such reference exists. Updating an edge cannot selectively cut over one
sort. The paper's type-restricted boundary-edge confinement construction is
therefore unavailable in the prototype.

Process description values are Herald-authored observations of Oracle membership
and process state. Seeing such a value MUST NOT itself admit a Herald, create a
process epoch, move a process, or establish canonical liveness.

The prototype has no separate predefined host carrier or `HostId`. The fixed
`HeraldEpoch` is the process-residence identity for one run. A later deployment
model may add physical hosts without changing Herald identity or treating an
application-published description as membership authority.

The predefined process-epoch carrier is Herald-managed. An application may receive,
name, read, or forward a process object according to ordinary capability rules, but
cannot instantiate, update, obsolete, relabel, delete, or cause disappearance of
one through `write` or `label`. Only `StartProcessEpoch` and `EndProcessEpoch` may
change its canonical lifecycle/label facts. This prevents application deletion of
an identity while its Oracle process epoch remains live.

Every Herald maintains private system-view deltas for the predefined carriers.
At genesis it creates exactly one real store per role. The private `DeltaId` and
its unrelated startup `StoreIncarnationId` are separately domain-derived from
`(SystemId, local HeraldEpoch, predefined role)`; these operational stores do not
invent controlled objects, application possession, or process placement. The
regular-data vertical makes the already-present private destinations routable when
it installs the corresponding topology/placement projection.

The immutable genesis topology retains isolated system bootstrap-writer identities
for its checked initial projection. They have no application-private aliases,
ordinary possession, or live publication path. Dynamic Start publishes no roots,
and `write`, `forward`, `newenv`, and `label` cannot acquire hidden writer privileges.
The first launcher receives its checked genesis environment. Later processes
receive selected names and normal grants; creating an environment uses explicit
Nabla- and Delta-carrier source writers through the ordinary operation checks.

The checked index-zero topology is one common Graph projection at every Herald,
even though application, Controlled, Store, and Placement ownership remains
filtered by residence. Its derived base contains every selected bootstrap root
Nabla and Delta, each bootstrap environment's neutral hub and eighteen ordinary
controlled preserving edges (writer-to-hub, hub-to-reader and reader-to-hub for
all six roles), all six private system-view
Deltas and the isolated system bootstrap-writer nablas for every active Herald,
and a preserving edge from every selected application writer
to its resident Herald's same-role private view. A startup-only symbolic manifest
may add a selected writer-to-reader or selected writer-to-active-system-view edge.
It names only bootstrap manifests, active Herald epochs, and predefined roles;
the checker resolves all endpoint identities and admits only universal preserving
same-role edges. It accepts no raw vertex, placement, incarnation, controller, or
sort-filter facts. This trusted finite-run fixture is not a second dynamic
topology authority and is replaced by ordinary topology induction in later
verticals.

Application publications of predefined values reach those deltas through ordinary
frozen routing. A valid `sort-sort` publication may be reconciled immediately once
its own regular publication prerequisites are satisfied; it has no controlled
lifecycle prerequisite. Other predefined publications wait only for their ordinary
data-plane structural dependencies, exact source-topology frontier, and any genuine
label/source-authority fence they name. Each Herald then decodes the value and
deterministically reconciles its induced entities. Missing dependency objects are
retained as unresolved definitions rather than guessed or discarded. When the
dependencies arrive, reconciliation is retried. Conflicting definitions for one
controlled identity or unequal descriptor bytes for one `SortId` are invariant
faults.

A predefined publication that depends on a genuine Oracle-ordered label fact may
be received early but MUST remain invisible and MUST NOT induce an entity until the
receiving Herald has applied its stated `ControlIndex` prerequisite. A successful
`label` reply waits for qualifying predefined publications to be both stored and
reconciled.

## 5. The exact application API

The application-facing library exports exactly these operation names:

```text
newid() | newid(PrivateNablaId)      -> PrivateUniqueId
newenv()                             -> EnvironmentAccess
write(PrivateNablaId, ApplicationWriteValue) -> WriteResult
forward(PrivateNablaId, PrivateObjectId) -> ForwardResult
read(ApplicationQuery)               -> [ApplicationValue]
local-take(ApplicationQuery)         -> [ApplicationValue]
wait([ApplicationQuery])             -> WaitResult
label(PrivateObjectId, ApplicationLabel, ApplicationLabelTarget) -> LabelResult
```

`ForwardResult` has the sole payload-free success `ForwardAccepted`. Ordinary
forward returns it at local publication acceptance. Structural forward first uses
the shared `OperationAccepted(StructuralStabilizationPending)` request status and
advances to terminal `ForwardAccepted` only after its occurrence is covered by an
established cut installed at home.

The wire surface has exactly these eight operations. `newid` selects bare or
controlled allocation through `NewIdTarget = BareNewId | ControlledNewId(PrivateNablaId)`.
The typed facade binds this semantic vocabulary to application payload types and
checked endpoint handles; see [typed application API](../architecture/typed-application-api.md).

`ApplicationValue` has the same checked schema-directed shape as the internal
semantic value, except that every unique-ID leaf is a `PrivateUniqueId` rather than
a `GlobalUniqueId`, and every label leaf names its process through
`PrivateProcessId` rather than `ProcessEpochId`. `ApplicationQuery` likewise uses
`PrivateDeltaId` operands, `PrivateUniqueId` in unique-ID literals, and
`PrivateProcessId` in process/zombie label literals. Neither type can contain a
global identity. The application forms `EnumSchema(NonEmpty Text)`,
`EnumValue(Text)`, `LiteralEnum(Text)`, and `QueryEnum(Text)` retain the same exact
symbol text and declaration order as their checked domain forms.

The profile-0.1 query grammar is a closed Boolean expression over scalar field
projections: `Always`, `Never`, one of the six scalar comparisons, `Not`, non-empty
`All`, and non-empty `Any`. Literals are bool, signed 64-bit integer, bytes, text,
an exact enum symbol, private unique ID, or private process/void/zombie label.
Every projection is a non-empty textual field path and is admitted independently
against every named delta's effective descriptor. An empty delta set is valid and
reads/takes as an empty result. Matches are ordered by internal
`(DeltaId,ObjectKey)`; equal values from distinct stores remain distinct result
members. This order, followed by schema preorder with ascending record fields,
fixes private-ID localization.

Comparison uses the scalar's semantic order: `False < True`; signed integers use
numeric order; bytes, text, and unique IDs use lexicographic order; and labels use
`Void < Process(id) < Zombie(id)`, with identity-byte order within the latter two
arms. Enum values use their owning schema's zero-based declaration order; the
exact symbol must be a member before comparison, key extraction, or explicit
field ranking. Admission requires equal projected and literal scalar types, so no
cross-type order exists. The structured `sort-sort` application value is not the
internal carrier record: queries over that adapted carrier therefore admit only
`Always`, `Never`, `Not`, `All`, and `Any` in profile 0.1. A projected comparison
is rejected rather than exposing the internal sort-ID or canonical-descriptor
bytes through a query.

`ApplicationWriteValue` is the closed input-only sum
`PublishValue(ApplicationValue) | DeleteReserved(PrivateObjectId)`.

`DeleteReserved` is the boundary representation of an initial controlled label
of `delete`: it names only an uninstantiated object returned by `newid(p)` for the
same nabla. It is admitted and consumed before construction of an internal
semantic `Value`, so `delete` is not an `ApplicationValue`/`Value` label, cannot
occur in a regular write or update, and can never appear in a query, stored value,
read/take result, or peer publication. Both arms remain payload variants of the one `write` operation rather than additional API
operations.

`RegularCallResult` includes
`NewIdCompleted(PrivateUniqueId)` and retains write completion as
`WriteCompleted(WriteResult)`. `WriteResult` has the successful branches
`SortDefinitionWritten(SortId)` and `WriteAccepted`. A successful
sort-definition publication returns the Herald-derived public `SortId` through
the first branch; successful `DeleteReserved` returns the identity-free
`WriteAccepted`. An exact request retry returns the same retained branch and, when
present, ID. Other write-result branches do not make applications derive a sort
identity, and `WriteAccepted` does not assert publication, storage, or remote
receipt.

A structural write does not emit that terminal
`WriteAccepted` at local first-use acceptance. EAPP retains and may reoffer the
nonterminal `OperationAccepted(StructuralStabilizationPending)` status for the same
`RequestId`, including while the work is still an unsequenced structural stage.
After the home Herald installs an established topology cut whose vector covers the
occurrence, that request advances once to
`Completed(WriteCompleted(WriteAccepted))`. Exact retry and `GetRequestResult`
reoffer the current retained status while the logical session lives and never
reveal the internal `PublicationId`. Session retirement removes that EAPP state,
but the publication/topology owners continue stabilization by internal identity.

The one boundary adapter is `sort-sort`: its application value is a structured
`ApplicationSortDefinition`, never caller-supplied canonical descriptor bytes. An
application declaration contains an `ApplicationSortDescriptor` AST and an
optional claimed public `SortId`. A second closed form names one of the six
predefined catalogue roles plus its public `SortId`; the Herald re-materializes
and verifies that exact fixed descriptor. This makes primordial definitions
faithfully readable and safely repeatable without exposing structural-carrier or
Herald-managed descriptor fields, and cannot express a seventh carrier.
The Herald admits that AST through the semantic domain, produces the sole canonical
encoding, derives the `SortId`, and, when a claim is present, requires equality.
Results materialize the computed `SortId`; a fresh declaration may omit it. Thus
applications can submit and compare definitions without importing or duplicating
the canonical semantic codec. This adapter is still ordinary data passed through
`write`, `read`, and `local-take`, not a ninth operation.

The label call's expected `ApplicationLabel` is `(ApplicationLabelOwner, Word64)`:
the owner is process, zombie, or void in the caller's private process namespace.
`ApplicationLabelTarget` remains process, void, or delete and carries no generation;
neither type can carry a global process identity, and zombie is never a target.

Language bindings may express arguments and results idiomatically, but MUST NOT
expose handshake, session, request-status, wait-registration, cancellation,
historical-readiness, delivery, topology, Oracle, or Raft calls as additional
ECLIPS application operations.

### 5.1 Common call rules

- Calls are attributed to one live process epoch and one logical application
  session at one Herald.
- Private unique IDs and their typed handle wrappers resolve only in that process
  epoch's private bidirectional map. All of the epoch's sessions share that map;
  request IDs, pending waits, cached results, and resume state remain isolated per
  session.
- A call either returns a typed semantic result, a typed semantic rejection, or a
  transport-level unknown-outcome error. Retry behavior is specified in
  [03-protocols.md](03-protocols.md).
- Calls from all sessions attached to one process epoch acquire positions in one
  monotonically increasing process-scoped Herald acceptance order. That order, not
  TCP arrival fragments or session identity, separates pre- and post-fence work.
- A `label` request remains pre-acceptance while any already accepted publication
  in its sequencing scope is an unsequenced structural-admission stage. It installs
  no fence, process acceptance position, or Oracle intention in that phase, so
  later provider calls from any attached session may acquire positions and make the
  structural stage ready. `Application` reserves the session/request identity and
  exact typed call only for retry/conflict classification; it creates no retained
  reply status, and exact retry/result lookup simply reattaches and waits. Session
  or process end drops this pre-acceptance candidate. Once no such stage exists,
  accepting `label` captures the
  then-current process/publication cut and installs a local pending fence in that
  same transition, before any Oracle round trip. Every later qualifying call is
  held behind it. The eventual `DecideLabel` names this exact cut, so qualifying
  work remains ordered from local acceptance through decision installation.
- Rejection before acceptance has no semantic effect.
- An accepted asynchronous publication remains valid if its RPC reply is lost.
- Before accepting a genuinely Oracle-backed call such as `label`, the Herald
  installs the complete self-contained workflow and exact Oracle-client intention
  needed to finish it without the session or private-ID map. Controlled
  publication, controlled obsolescence, and structural publication are not in
  this category: their durable `Publication`, `Controlled`, peer-stream, and
  topology-progress owners survive reply loss without an Oracle intention.
  Session/process end and authority change cannot discard already accepted direct
  publication work. `wait` remains the explicit cancellable call.

### 5.2 `newid`

`newid` has one operation name and two forms:

- `newid()` creates a fresh `GlobalUniqueId`, binds it to the process epoch's next
  `PrivateUniqueId`, and returns only the private ID for the application to use as
  it sees fit; and
- `newid(p)` for a controlled-sort `PrivateNablaId` creates the same stable mapping
  and also reserves the global ID for one first controlled use through `p` by the
  calling process.

Bare `newid()` retains exactly the bidirectional alias required for future
globalization/localization. It creates no controlled reservation, possession,
capability, controlled-object identity, or Oracle fact. The session's general RPC
request-result cache also retains the completed private-ID reply under its
`RequestId` so an exact retry returns the same value. Neither record upgrades the ID
into a controlled object. The application may use it as data wherever the
unique-ID type is allowed, and every later occurrence is globalized through the
same process mapping.

`newid(p)` requires `operate(p)`. Its reservation is Herald-local and belongs to
the process epoch and nabla authority epoch, not to the particular session that
made the call. The Herald consumes the reservation when the first controlled
action commits. Cancelling or consuming the reservation does not remove or rebind
the process's private/global alias.

The application boundary uses typed semantic rejections already owned by the application
boundary for an unknown private operand (`ApplicationUnknownPrivateIdentity`), a
resolved value that is not a writer nabla (`ApplicationNablaRoleMismatch`), or a
caller without `operate` (`ApplicationOperateNotPermitted`). A resolved current
writer whose effective sort is regular is rejected as
`ApplicationNablaSortNotControlled(privateNabla)`. These decisions are retained
before the generator or process-local counter advances.

Passing a nabla whose data sort is regular is a sort-kind rejection. In particular,
`newid(sortWriter)` cannot allocate or reserve a `SortId`; an application submits a
structured `ApplicationSortDefinition` through `write`, and the Herald/domain
canonicalizes it, derives the public `SortId`, and returns/checks that ID.

`GlobalUniqueId` is an opaque value of exactly 256 bits. It has no observable
decomposition into a system, Herald, epoch, generator, or counter, and internal
codecs MUST NOT infer or expose such fields. A 64-bit counter may be sufficient
generator state; that does not make the counter itself, or a tuple containing it,
the global identifier. The application-facing `PrivateUniqueId` is instead a
checked local-counter value whose meaning is qualified by `ProcessEpochId`.

Runtime `newid`, `newenv`, and dynamic Start identities are produced by the
resident Herald generator. Identities selected for the index-zero genesis
projection are trusted checked deployment assignments validated pairwise and
disjointly. Both sources
inhabit the same opaque global identity domain; provenance is not encoded in the
256 bits.

There is no singleton system-wide generator. Profile 0.1 provisions exactly one
independent generator at each Herald. It owns an independently sampled, exactly
32-byte `GeneratorSeed`, used as an AES-256 key `K`; one private 24-byte prefix;
and an internal unsigned 64-bit next-counter initialized to zero. Different
Heralds do not coordinate their counters and may use the same counter values.

Let `AES-K(X)` and `AES-K^-1(X)` mean AES-256 encryption and decryption of one
exactly 16-byte block, and let `Z = AES-K^-1(0^128)`. With zero-based byte
indices, the sole 24-byte prefix that permits any counter to produce the all-zero
ID is `P0 = Z[8..15] || Z`. The initial prefix `P` is `P0` with the high bit of
its first byte toggled and remains fixed in this profile.

For current counter `C`, the exact current-build mapping is

```text
M = BE64(C) || P
M = A || B
U = AES-K(A)
V = AES-K(B xor U)
GlobalUniqueId = U || V
```

Here `M`, `U || V`, and the opaque `GlobalUniqueId` are exactly 32 bytes; `A`,
`B`, `U`, and `V` are exactly 16 bytes; `BE64` is the exactly eight-byte unsigned
big-endian encoding; `xor` is byte-for-byte exclusive-or; and `||` is raw
concatenation. This is the algebraic definition of unpadded two-block CBC with an
all-zero IV, used only as an identity-package permutation. It is not a
confidentiality or authentication protocol and carries no API/protocol version.

For fixed `K`, the inverse is `A = AES-K^-1(U)` and
`B = AES-K^-1(V) xor U`; distinct prefix/counter packages therefore have
distinct outputs. The unique package producing `0^256` is `Z || Z`, so the
chosen `P`, which differs from `P0`, makes the all-zero `GlobalUniqueId`
unreachable for every counter. On first acceptance the generator uses the
current counter and commits its successor exactly once; the first generated
output therefore uses counter zero and leaves counter one. Fixed keys, prefixes,
and counters have fixed conformance vectors for this build.

The fixed-key permutation guarantees that one generator does not repeat an output
while its prefix/counter package does not repeat. The profile separately assumes
that outputs under independently supplied keys are distinct from each other and
from checked deployment-assigned identities during the finite run. It adds no
emitted-ID registry, cross-owner conflict scan, Oracle prior-use recovery path,
distributed allocation, rename, or rollback protocol. Knowledge of an output is
not possession, a capability, or an authorization credential.

A future never-repeated prefix under the same key could preserve the exact
permutation guarantee across prefix epochs. Rekeying selects a different
permutation and is not equivalent because its outputs may overlap previous
outputs. Profile 0.1 performs neither prefix rotation nor rekeying.

Entropy is acquired only when a generator instance is provisioned, not once per
`newid` call. The runtime invokes its configured seed source exactly once before
constructing the Herald runtime's `RuntimeContext`, trace ledger, child, listener,
or Herald state, checks that the result is exactly 32 bytes, and supplies the opaque
`GeneratorSeed` as a mandatory argument to pure `initialHerald`. An ordinary seed
source failure, or a wrong-width result, yields runtime initialization failure and
no usable state or listener. The TCP facade may allocate an inert local context to
wire immutable configuration first, but starts no socket, child, callback, or sink
delivery on failure. There is no optional seed, implicit pure default,
entropy input, or entropy effect. The already provisioned key, derived prefix,
and counter belong
to the pure `Herald.IdGenerator` state; tests inject fixed seeds for deterministic
examples.
Neither seed, prefix, nor counter is part of an application value, peer protocol, or
Oracle/Raft command. Exact RPC retry returns the recorded result without
advancing the generator or the process-local counter. `Herald.IdGenerator` MUST NOT
retain a separate set or map of emitted values. Completed `newid` outputs are
retained only in the owning process mappings and, for `newid(p)`, the relevant
controlled workflow. A pending `newenv` instead retains its complete generated
manifest and direct structural publication/stabilization state in its Herald
owners until settlement and localization; it creates no `OracleClient` command.
Durable restart, including persistence of generator and process-map state, requires
a separate production design. The profile-wide no-overflow assumption applies to
both internal counters; overflow and wrap-around are not represented or tested.

A reservation is not a current object and confers no capability. Its single-winner
state begins `Reserved`. Across the complete profile, the first accepted operation
that uses it atomically takes exactly one of two branches:

- `write(p, DeleteReserved(o))` moves it to terminal `ConsumedByDelete` without an
  internal value, object, publication, or Oracle command; or
- a controlled `PublishValue` instantiation moves it to
  `Published(publicationId)` as the immutable first-use winner. The live
  application session, not this controlled record, owns any request correlation
  and pending/terminal reply state.

`DeleteReserved` is admitted only from pristine `Reserved`. A competing first
write/cancellation rejects; exact request retry follows the already recorded
branch. `Published` does not denote an invisible Oracle stage: the same accepted
Herald transition fixes the publication identity, local controlled record,
possession, and self-contained semantic work. For ordinary data, or a structural carrier
whose prerequisites are already applied, it also commits the applicable local
effects and peer outbox items. Otherwise it retains the private unsequenced
structural-admission stage; the later readiness transition atomically reconciles
the local structural effects, stamps and advances the occurrence, and inserts its
peer outbox items. Reservation cancellation is not a ninth operation.

Process death, nabla passivation, or handoff cancels a pristine `Reserved` entry
but MUST NOT drop a publication already accepted from it. Direct publication and
retry continue from their semantic owners even when reply/map reachability is
removed. No branch deletes the stable private/global alias or rolls back/reuses
the local counter; the alias itself remains until process death.

Completion point for bare `newid()`: the global value, fresh private ID,
bidirectional alias, session result, and both generator/local-counter advances are
atomically fixed, with no controlled semantic state. Completion point for
`newid(p)`: those same facts and the matching global-ID/nabla-bound reservation are
atomically fixed.

### 5.3 `newenv`

`newenv()` creates a fresh process-private environment with one private
writer/reader pair and the required initial capabilities for every predefined
carrier. The objects that control those private nabla/delta endpoints are unique
controlled objects. The endpoint pair for `sort-sort` nevertheless
carries regular `SortDefinition` values: creating the environment allocates no
`SortId`, creates no controlled record for a definition, and does not give an
environment-private identity to an otherwise equal descriptor.

The result type is a distinct opaque `EnvironmentAccess` containing exactly the
six canonically role-ordered writer/reader pairs, one neutral hub, and eighteen
preserving edges: 31 distinct private object names.
It does not repeat the caller's process handle and cannot serve as process-start
or attachment evidence. `ApplicationStartupAccess` instead pairs the private self
handle with an arbitrary checked `PrimordialAccess` map, which may be empty or
partial. `selectEnvironment` explicitly converts a complete environment into the
conventional 31-entry selection (twelve required endpoints and nineteen object
grants); a new environment does not enlarge the
process's retained startup access.

Both bootstrap and `newenv` supply the same connected environment: each writer
has a preserving edge to the hub, and every reader has preserving edges to and
from the hub. Endpoint sort matching selects which descriptions each reader
stores. The neutral hub and eighteen edges are ordinary controlled objects with
identities, labels and possession; they are not anonymous permanent graph facts.
The initial labels name the founder for bootstrap and the caller for `newenv`,
with generation zero. End has its ordinary zombie-label meaning. Hub and edge
activation does not require a live label owner; continued retention still requires
live application stores. The environment needs no permanent constructor process.

The new environment belongs to the existing `SystemId`. Its controlled endpoint,
hub and edge identities are generated and first published by the caller's Herald; the
Oracle neither allocates nor approves them. The catalogue's primordial sort
identities, including `sort-sort` itself, are shared protocol facts rather than
per-environment objects. No application-visible edge, identifier, or description
connects the new roots to another environment. The caller may later disclose or
connect them through ordinary ECLIPS operations. Once such a path is connected,
its edges admit every sort, so it is not a one-sort protected interface; privacy
continues to depend on the absence of a graph path and on ordinary endpoint typing
and capabilities.

The immutable catalogue fixes a canonical environment-manifest order and
the exact number and roles of global identities it requires. On first semantic
acceptance, the Herald generates that many identities from consecutive states of
its own `IdGenerator`, retains all 31 identities and the first twelve structural
publications, and commits the generator advance before exposing any result.
The endpoint prefix is followed by the hub and eighteen edges in canonical
predefined-role order, with writer-to-hub, hub-to-reader and reader-to-hub for
each role. Exact retry reuses the retained identities and positioned publications.
The construction advances only by appending the checked nineteen-object suffix;
other accepted publications may intervene between these two consecutive ranges.

The caller's startup access may retain `environmentSources`: two selected writer
keys carrying the predefined Nabla and Delta sorts respectively. `newenv` uses
that exact pair for the matching root carrier roles. Missing sources yield
`EnvironmentSourcesUnavailable`. Acceptance rechecks ordinary current writer
operation for both sources; lost label or normal possession yields ordinary
operation rejection, with no substitution from another possessed writer.
Acceptance freezes each source's current `AuthorityEpoch` and uses its positive
nabla publication sequence to construct root `PublicationId`s in manifest order.
The new root is the target, never its own source. Thus `newenv` needs no explicit
nabla operand or hidden privilege, and a partial startup needs only the selected
source pair. Later environments do not replace that retained pair.

An installed established cut covering the endpoints gives the new environment's
NeutralVertex-role and Edge-role writers their ordinary publication authority.
Construction then uses those writers to publish the hub and edges, retaining
their actual source authority, topology, control prerequisites and frozen routes.
It never guesses their future authority at initial acceptance or uses hidden
bootstrap writers for dynamic work. The endpoint descriptions are seeded into
the environment's exact own structural stores at the first cut; the complete
31-object descriptions are seeded at final settlement. Both steps retain the
original checked publications and structural provenance, use normal monotone
store observations and are idempotent. The new SortDefinition reader also gets
the same six immutable catalogue descriptions as bootstrap. These seeds do not
retarget frozen routes or make the descriptions defaults for unrelated future
readers.

The resident Herald disseminates the retained structural occurrences to every
active-generation Herald system view and reconciles their finite local structural effects while the
application bootstrap/result remains pending. Only after the resulting established
cut reaches the completion point below does it apply the process-visible bootstrap
and localize the roots into the caller process map in manifest order. A later
publication failure does not roll back or reuse the generated range.
The membership coordinate at acceptance remains immutable provenance. If the
accepting Herald survives successive non-voter retirements, the
checked installed membership lineage carries its accepted work forward: already stamped roots
keep their stamps, unsequenced roots stamp only after the successor base, and frozen
routes project by dropping exactly the retired destinations. The pending environment
may then settle from an installed descendant established cut covering the same
positioned objects; accepted identities, source provenance, routes and acceptance
positions are not retargeted. A not-yet-positioned wiring phase resolves its
ordinary sources and current routes only once its endpoint cut is available. If the accepting Herald itself is retired, its process receives no
result and only occurrences already retained by survivors participate in the
terminal source union. Recovering an untransmitted in-memory manifest would require
the deferred durable-recovery design.

A normal application own-End waits for accepted environment construction before
submitting Oracle End. Forced End or publisher-session loss can end the process
while only the endpoint phase exists. In that case construction stops: it settles
the already positioned prefix without aliases or a result, and never publishes the
unpositioned suffix through an ended process. All reserved IDs remain consumed;
published objects follow ordinary zombie-label and disappearance rules. If the
whole suffix was positioned before End, those accepted publications still settle,
without rebuilding retired stores or returning an environment to the dead process.

The generator's distinct-output and no-overflow assumptions mean there is no
duplicate-ID, counter-exhaustion, or wrap-around branch in this transition.

Completion point: all 31 global IDs have stable bindings in the caller process's
shared map, the descriptions are present in its own structural readers, and the
exact structural cut containing the complete environment has complete acceptance evidence in the captured membership or
a checked installed descendant, and its `TopologyCutEstablished` is installed
at the surviving home Herald. The returned typed private handles are then usable
from all sessions of that process epoch. This
conservative all-member stabilization closes the transient private-bootstrap chain
without putting payload approval in the Oracle.

### 5.4 `write`

`write(p, PublishValue(x))` validates and publishes one normal-strength instance
of the sort of nabla `p`.

`write(p, DeleteReserved(o))` is the narrow cancellation arm. The Herald resolves
both private handles in the caller's process map, requires the exact live
pristine `Reserved` state from `newid(p)` for `o` at the current
nabla/authority epoch, with no staged instantiation or Oracle intent, and the same
ordinary caller/capability admission as first instantiation. It then atomically
moves that reservation to `ConsumedByDelete` and records the accepted write
result/position as `WriteCompleted(WriteAccepted)`.
It constructs no internal `Value`, performs no descriptor/value publication
validation, creates no object/publication/store/outbox fact, and submits no Oracle
command. A missing, consumed, wrong-nabla, regular-sort, or already-instantiated
reservation rejects before acceptance; exact request retry reuses the recorded
result. After both private operands resolve, a missing, consumed, wrong-process,
wrong-nabla, or otherwise non-pristine reservation uses the one redacted rejection
`ApplicationReservationUnavailable(privateNabla, privateObject)`; it exposes no
global identity or foreign reservation fact. `DeleteReserved` in any other
syntactic position is unrepresentable.

A retained reservation whose captured authority differs from its writer's checked
current authority contradicts valid state and is an invariant fault. Any later
authority-change transition must cancel a pristine reservation before committing
the new authority, after which a caller observes only the ordinary missing
reservation rejection.

For the `PublishValue` arm, the Herald MUST:

1. resolve `p` in the caller process's private map and check `operate(p)`;
2. recursively globalize every unique-ID field of `x` through that same map,
   rejecting an unmapped local ID before semantic acceptance;
3. decode and validate the globalized value, recomputing its key, lifecycle, and
   total rank;
4. for a controlled object, validate an update capability or a matching pristine
   `Reserved` entry with no winning first operation;
5. for a structural definition, check `name` capability for every
   capability-bearing controlled reference, including edge endpoints and
   sequencing objects;
6. allocate one stable `PublicationId`, retain the canonical prepared value, freeze
   the route, and allocate the `HeraldPublicationPosition`. Ordinary data freezes
   its exact direct destination/path-strength cut; a structural carrier unions
   that ordinary frozen cut with one mandatory private-system-view destination at
   every active-generation Herald. Exact duplicate destinations collapse, so a
   configured application reader can observe the carrier without weakening
   all-member structural dissemination. If the structural carrier's referenced
   structural/data/control prerequisites are not all applied
   locally, retain an unsequenced structural-admission stage; that stage has no
   occurrence ID, stamp, peer item, stream assignment, or applied-vector effect;
7. for first controlled instantiation, atomically consume the reservation into
   `Published(publicationId)`, establish the local object-to-sort/lifecycle/label
   record, and retain normal publisher possession;
8. for obsolete state, install monotone hidden suppression before the publication
   can become visible or leave the Herald;
9. atomically record the retained request state. Ordinary data applies local
   destinations and inserts peer batches into logical outboxes immediately. A
   ready structural carrier instead allocates its next local structural sequence,
   stamps the complete current applied predecessor vector, reconciles its finite
   local effects, advances the local applied component, and inserts stamped peer
   items in one transition; an unready carrier performs that transition later,
   chosen with other ready stages by `HeraldPublicationPosition`; and
10. return ordinary-data acceptance without waiting for remote acknowledgements.
    A predefined structural publication instead returns/reoffers
    `OperationAccepted(StructuralStabilizationPending)` and retains that request
    state until every active-generation Herald has accepted an exact topology cut
    containing it and the resulting `TopologyCutEstablished` is installed at the
    home Herald;
    it then advances once to terminal `WriteAccepted`.

For controlled instantiation through `PublishValue`, the supplied persistent
initial label must be `(caller, 0)` or `(void, 0)`; nonzero initial generations are
rejected at first-use admission. Initial delete is represented only by the
separate `DeleteReserved` write arm above and consumes the reservation without
creating an object or publication. For a controlled update, the supplied label
field is ignored and the latest locally applied complete label pair is used. Its
owner must be void or the caller; `DeleteReserved` cannot name an existing object. The
paper-permitted race remains: a locally accepted update may arrive after another
process's later label unless the object's sequencing fence orders it.

Instantiation, current update, and current-to-obsolete publication create no Oracle
command. Every receiver retains local controlled identity/sort/lifecycle knowledge;
once it records obsolete or deleted suppression, delayed current traffic cannot
revive the object. A publication carries a control prerequisite only when a genuine
ordered label/source-authority fence or regular-definition retirement cut requires
one. Profile 0.1 retains suppression for the finite run. A later Oracle-coordinated
reclamation workflow may remove it only after every captured Herald reports both
observation and cut-qualified retirement readiness under the bounded-latency and
clock-skew contract; that workflow is deferred.

Writing an application `ApplicationSortDefinition` follows the regular path. The
Herald admits its structured descriptor AST, emits the semantic domain's sole
canonical descriptor bytes, computes `SHA-256(canonicalDescriptorBytes)`, and
requires any claimed `SortId` to equal the result. It then constructs and publishes
the internal ordinary `SortDefinition` value with the frozen routing cut and no
sort-definition lifecycle command. An equal declaration at any Herald denotes the
same regular object under the profile's SHA-256 collision-resistance premise.
Successful completion is
`SortDefinitionWritten(computedSortId)` in the operation's `WriteResult`; the
request-result cache retains that exact public ID for retry.

### 5.5 `forward`

`forward(p, x)` republishes the Herald's retained canonical state of controlled
object `x` through controlled-sort nabla `p`. A regular `SortDefinition` is not a
valid operand; an application repeats it with ordinary `write`.

It requires `operate(p)` and `forward(x)`, and the object's sort must equal the
nabla's sort. The value is selected when the Herald accepts the operation, so it
may be newer than the state that prompted the application call. The publication's
source strength is the caller's effective possession strength and may weaken on
the route.

An obsolete object may be forwarded only while its minimum-retention rule permits.
Each Herald records a local monotonic observation time when the object becomes
obsolete and rejects `forward` at or after that observation plus the descriptor's
minimum retention. Hidden payload and suppression metadata may still be retained
indefinitely; conservative storage does not extend application authority. Immutable
objects may be forwarded.

Completion point: identical to `write` after no new lifecycle command is needed.
When the retained object is a structural carrier, `forward` is itself a new
topology-affecting publication: it uses the unsequenced-stage/structural-stamp path,
returns `OperationAccepted(StructuralStabilizationPending)`, and becomes terminal
`ForwardCompleted(ForwardAccepted)` only after a covering established cut is
installed at home.

### 5.6 `read`

`read(q)` evaluates a pure query over the visible stores of its named deltas at the
serving Herald. The caller must have `operate` capability for every referenced
delta. The operation returns copies of all matching visible winners, or an empty
list when none currently match.

An empty result asserts only local visible absence at that instant. It says nothing
about peer stores, in-flight publications, or historical alignment readiness.

Completion point: one atomic local-store snapshot.

### 5.7 `local-take`

`local-take(q)` performs the same checked snapshot as `read`, returns the same
values, and atomically removes the matching visible entries from the named local
delta stores. It retains hidden winner/suppression state as described in section
2.1.

It does not publish, relabel, mark obsolete, delete globally, or immediately remove
a predefined entity. Removing a predefined definition from every application
store merely makes it eligible for the separate global-disappearance protocol.

Completion point: one atomic local-store transition.

### 5.8 `wait`

`wait([q1, ..., qn])` blocks while every corresponding `read` would be empty. It may
return when any query becomes non-empty, and it may return spuriously. A matching
value may disappear before the caller resumes, so applications MUST re-run `read`
or `local-take` in a loop.

The caller needs `operate` capability for every referenced delta. When an exact
delta/Store incarnation named by a retained query is passivated, the Herald removes
the internal registration and completes the retained request with the permitted
spurious `WaitReady` result; a connected session receives the corresponding wake,
while a disconnected session recovers the retained completion on resume. Process
termination, explicit RPC cancellation, and session termination also remove their
registrations. Registration, wake notification, and cancellation are RPC
bookkeeping, not application-facing ECLIPS operations.

Completion point: a local level-triggered observation or a permitted spurious wake.

### 5.9 `label`

`label(x, expected, target)` attempts an atomic value compare-and-swap on the
canonical effective label of a controlled object with a label field. `expected` is
an `(owner, generation)` pair in the caller's private namespace: the owner is
`process(P)`, `zombie(P)`, or void, and the generation is a `Word64` scoped to this
object. Samples and expected arguments carry the same pair; labels remain one
built-in scalar in values and queries. All comparisons include both components.
`target` is one of:

- a live process for which the caller has `name` capability;
- void; or
- delete.

Zombie is never a caller-selected target. Process End or Herald retirement
projects `(process(P), g)` to `(zombie(P), g)` without advancing generation.

A `SortDefinition` is regular and has no label field, so it can never be the target
of `label` or `label(..., expected, delete)`. A publication of one may still be included
in a label visibility fence when the controlled nabla used to publish it names the
labelled object as its sequencer; that orders the publication, not the definition's
lifecycle.

The caller must have normal label capability for `x`. If the current effective
label at the canonical decision differs from `expected`, the call completes
`LabelNotApplied` without changing the label. The Oracle judges comparison and
process lifecycle from its ordered state; a lagging local label or End projection
cannot decide the result. When the pair matches, the transition rules are:

- `process(P)` may be changed only by `P`, to any nameable live process, void, or
  delete;
- void or `zombie(P)` may be changed by a capable caller only to
  `process(caller)`, void, or delete.

Explicitly supplying `(zombie(P), g)` as the expected value makes adoption
intentional while detecting concurrent changes. Every successful operation proposes
exactly `g + 1` from its canonical prior and makes that successor authoritative in
one immutable decision. This includes same-owner success, void-to-void and zombie
adoption; each observes the selected publication fence. The sequence
`(P, 7) -> (Q, 8) -> (P, 9)` cannot make an old expected `(P, 7)` match again.
Rejection or `NotApplied` leaves generation unchanged. Exact request retry and
decision replay retain the recorded result and never take another successor.
A later End or retirement preserves the successful decision and its generation.

Fresh controlled publication, checked genesis and parent-labelled `newenv` roots
start at zero. Fresh controlled publication rejects a supplied nonzero generation
at its actual admission boundary; arbitrary label-valued application data may carry
nonzero generations. Existing-object updates and forwards use the whole canonical
pair; history and snapshot reconstruction preserve the recorded generation.
An already accepted publication may first arrive after another caller's label
operation has advanced the object. Its embedded pair is historical evidence;
delivery and replay MUST NOT restore that pair as current authority. The current
released overlay governs effective reads and endpoint activity. Ordinary value
winner and Store delivery rules still apply to the delayed publication. An unseen
older embedded generation therefore does not by itself invalidate accepted work;
an unexplained future generation or conflicting owner at the current generation
remains invalid. Known End projects process to zombie without changing generation
when comparing the current pair.
Successful delete retains `g + 1` in its terminal outcome and record, with no
readable deleted sample. Automatic disappearance preserves `g`. Shared removal
paths distinguish a successful label deletion from automatic removal.

An obsolete object may be relabelled or deleted. Delete removes the controlled
object globally and irreversibly; it is a label target, not a ninth operation.
For an authority-bearing nabla, assigning a different live process creates the
next authority tenure. A same-owner outcome retains the existing tenure.
Relabelling to void also retains that tenure as inapplicable history because void
is not a live operator assignment; assigning a live process from void creates a
new tenure. Delete ends applicability without minting an authority for a
nonexistent object. The preceding authority may remain only as terminal historical
evidence.

`label` supplies a visibility fence around publications issued by its caller
through a nabla sequenced by `x`, or publishing `x` itself. The latter includes
controlled first publication, updates and forwards. Capture issuer, immutable
writer sequencing object and published object at publication acceptance; a
publication matching both arms belongs to the scope once. Publications by another
caller do not enter this scope merely because they use the same sequencing object
or publish the same object. Generated `newenv` publications follow the same rule.

Qualifying publications accepted before the fence become visible before the new
label. Qualifying publications accepted after the fence opens cannot become
visible before the new label. The fence applies even when the old and new owners
are equal. For predefined publications, visibility includes deterministic entity
reconciliation.

A label cannot open while an in-scope accepted structural publication remains
unsequenced. The request waits before semantic acceptance and opens no local or
Oracle fence; later calls may therefore publish the missing structural providers.
When those stages have been sequenced and assigned to their frozen peer routes, the
label captures a later cut that includes them. This prevents a provider held behind
the label from being required to make a pre-label publication assignable.

The home settles the selected preceding publication group before submitting the
conditional request. The Oracle atomically compares the expected pair, judges the
canonical caller/target transition and records either `Applied` or `NotApplied`.
A busy installation slot defers the original request; it is not a failed CAS.

Each captured Herald installs an `Applied` decision against its current local
state, including the label/authority overlay, direct Store and placement effects,
and ownership of required structural/context work. Installation preserves normal
possession requirements and delayed-publication suppression even when the target
is locally absent. Unrelated application calls continue. A lagging receiver may
still expose its earlier state until it installs the decision; the protocol does
not impose a universal observation gate.

Success returns after every captured survivor has installed the immutable decision
and the collector's compact completion has committed. Required predefined work is
reconciled before installation is reported. `NotApplied` returns when the home
applies that immutable failure; it needs no installation reports or completion
command. No label state changes because of that failed attempt.

A later ordered End can make the effective owner zombie while installation
collection continues. Retirement removes only the retired participant's impossible
report and reassigns a retired collector deterministically. Neither event changes
a recorded `Applied` to `NotApplied`. A partition can therefore delay success
until peer recovery or canonical retirement. Detailed ordering is in
[03-protocols.md](03-protocols.md).

## 6. Process termination and zombie labels

A TCP close, failed heartbeat, or peer suspicion is not process termination. An
explicit administrative End may propose termination immediately. Separately, the
resident Herald may propose the same exact `EndProcessEpoch` only after the last
unexpectedly detached recovery-capable session of a previously attached process
has exhausted one uninterrupted absolute recovery grace and the Herald has
atomically fenced later resume. Orderly `EndSession` remains session-scoped.
Oracle retirement of a Herald also ends every still-live process resident at that
exact epoch. Once the resulting End is committed:

- that epoch never becomes live again;
- its application sessions and waits end;
- its session results become unreachable and its private/global unique-ID map and
  next-local counter are retired as part of the same terminal application; no
  binding or meaning from the dead epoch survives, while equal token bits allocated
  later denote only the later epoch's distinct binding;
- its pristine `Reserved` entries are forgotten, but every already accepted direct
  publication, structural stabilization, peer delivery, or context obligation
  remains retained by its semantic owner;
- every genuinely Oracle-backed workflow such as an accepted `label` remains
  retained without a deliverable reply until its ordered terminal settlement;
  selected publication settlement, installation and completion continue;
- its active entities passivate; and
- canonical labels `(process(P), g)` become `(zombie(P), g)`.

This rule also governs accepted descriptions that have not yet induced an entity,
for example because their carried sort is missing. End does not need that sort
to determine the label. When the description's structural prerequisites arrive,
materialization applies the ordered label/End state and creates no active endpoint
or Store for the ended process. An unknown process may remain a dependency; a
canonically ended process never waits to become live again. Void, already-zombie,
and other-process labels are unaffected by `End(P)`. Immutable publication evidence
and pre-End historical projections retain their original meaning.

A non-voting Herald that loses an Oracle-voter majority may not yet have observed
its canonical retirement. After its own absolute isolation grace it may instead
self-fence irreversibly. This installs a local abandoned-branch overlay: objects
labelled to processes resident at that Herald are observed as zombie during one
bounded read-only drain, while new/resumed sessions and mutating calls reject. The
overlay is not a surviving-system Oracle fact and does not affect objects controlled
by processes resident elsewhere. After the drain the Herald is terminally
unavailable and can never merge or reactivate that epoch. This initial profile does
not apply the rule to a Herald that hosts a Raft voter.

A later operating-system process receives a new process epoch even when it reuses a
human-readable name. Label, process, Herald, TCP, and Raft lifetimes are distinct.

## 7. Predefined disappearance

Local removal of a predefined definition cannot establish global absence. The
disappearance protocol therefore has two explicitly different subjects.

A **controlled structural object** has `Never` application-obsolescence policy. It
is removed from the active graph only by successful
`label(object, expectedLabel, delete)` or
the ordered predefined-disappearance workflow. Both install payload-free
suppression so stale publication cannot resurrect the globally unique identity.
The Store owner retains this suppression by sort and object key even if no Store
of that sort exists at deletion time; a later first Store incarnation inherits it.
Profile 0.1 retains that suppression and does not claim the object has disappeared
from protocol history. A later Oracle-coordinated reclamation decision may permit
every Herald in one captured membership generation to forget it only after the
bounded-latency, all-member observation/readiness contract is satisfied.

A **regular `SortDefinition` occurrence** is never labelled or terminally deleted.
An Oracle-ordered retirement probe may establish that one exact content-addressed
definition has no application-visible copies and no live semantic dependant. A
dependant includes a current/obsolete Herald-local controlled record of that sort, an active or
passive nabla/delta typed by it, a visible or retained value of it, an unresolved
structural definition referring to it, or captured publication, outbox, alignment,
snapshot, or change-log work requiring it. The prototype may conservatively refuse to
retire a definition whose retained history still contains such a
dependant.

Success purges the captured occurrence, including hidden retained definition
copies, from private system-view deltas and removes it from the effective
`Herald.SortRegistry`. It may leave equal canonical bytes in a decode-only cache,
but those bytes are not an effective sort definition and confer no visibility. The
retirement installs only cut-qualified suppression for publications covered by the
proof; it creates no controlled-object lifecycle record, permanent tombstone, or
terminal lifecycle for the `SortId`. A later ordinary `write` of the same canonical
descriptor after the captured cut can establish a new occurrence of the same sort
identity.

For both subject kinds, every Herald in the captured membership generation accounts
for relevant peer stream prefixes, held visibility/dependency work, and same-sort
alignment before reporting absence. Matching new traffic invalidates the probe. The
probe is qualified by that immutable captured generation. Herald retirement aborts
it rather than reinterpreting the evidence against a smaller set; a probe in the
successor generation may open later. Details belong to
[04-oracle-and-raft.md](04-oracle-and-raft.md).

## 8. Consistency and progress

The system provides no global total order for ordinary data and no causal ordering
between unrelated sorts. Heralds may temporarily observe topology changes and
ordinary publications in different orders.

Subject to valid descriptors, fair processing, eventual peer connectivity, and
quiescent input:

- every accepted ordinary publication is eventually applied or terminally ignored
  at each frozen destination;
- duplicate and reordered delivery converges to the same retained winner;
- graph and active-delta placement advertisements converge among connected
  Heralds;
- structural applied-version vectors converge within each membership generation;
  an established common cut has one canonical winning topology digest at every
  active-generation Herald;
- context obligations eventually make late delta stores historically complete;
- every active-generation Herald retains monotone local controlled lifecycle/
  suppression knowledge, so delayed current data cannot revive a locally known
  obsolete/deleted object.

Subject additionally to an available Oracle quorum for the workflows that actually
use it:

- every active-generation Herald applying the same Oracle label/process prefix has
  the same ordered label/process projection; and
- a successful `label` has the all-or-none and pre/post visibility properties
  above.
