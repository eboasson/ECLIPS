# Dynamic Herald membership


This document owns semantic
Herald admission, catch-up, membership-base construction, and terminal exclusion.
The [Oracle design](oracle-raft.md) owns Raft configurations and failure votes;
the [application design](application-startup.md) owns process preparation and claim.

## 1. Separate discovery, membership, and voting

A contact is an address hint, an admitted Herald is a semantic participant, and a
Raft voter is a consensus participant. Discovery grants neither membership nor
votes, and Herald admission does not grant a vote. A Herald
can serve applications without voting; demoting its voter role leaves that service
intact. Pending Herald admission permits only discovery and onboarding traffic.

`SystemBootstrap` is the immutable checked system identity: `SystemId`, the closed
predefined catalogue, exact initial members/processes/topology, and initial Oracle
composition. Its digest does not change as members or processes join. Retain the
original checked genesis topology and its control-zero interpretation. Do not
rebuild genesis, change the system identity, or distribute an enlarged genesis
manifest whenever an application starts.

Oracle state separately owns append-only checked Herald and process catalogues,
current membership, pending admission records, and terminal epoch tombstones.
Genesis entries start those catalogues. Later entries carry their admission
`ControlIndex`, exact canonical manifests, identity relationships, and outcomes.
`StartProcessEpoch` admits fresh process/residence facts into this live catalogue
rather than requiring an immutable list of every process that could ever run.
Selected primordial objects remain existing data-plane objects, not process-root
manifests minted by Oracle. Herald projections derive the same catalogue
from the contiguous control history. A decoded manifest remains a claim until
its owning pure transition checks it.

One `HeraldId` has at most one active or pending epoch. A replacement may reuse
that ID after retirement, with a fresh `HeraldEpoch`; old streams, stores, writers,
resident processes, or attachment descriptors never acquire the replacement's
meaning. Exact request retries recover the retained original admission outcome.

The checked membership body is:

```text
Genesis(systemId, ascendingNonemptyMembers, memberSetDigest)
Successor(predecessorId, changeControlIndex, change,
          ascendingNonemptyMembers, memberSetDigest)
change = Admit(heraldEpoch, admissionId) | Retire(heraldEpoch, retirementId)
MembershipGenerationId = digest(canonical tagged membership body)
```

Admission verifies an exact fresh pending catalogue entry and inserts one epoch;
retirement removes one active epoch and cannot leave an empty semantic system.
Both check the immediate predecessor and strictly later control coordinate.
Constructors are opaque and positional; canonical decoding checks shape/digest,
while Oracle admission checks history, freshness, and the exact set difference.
The history retains every generation and tombstone for the finite run. Arbitrary
joins and retirements are permitted; there is no second-retirement rejection.

## 2. Seed discovery and restricted onboarding

The Herald executable explicitly selects creation (`--bootstrap`) or joining
(repeatable `--peer` locators). Creation builds one
fresh checked bootstrap and the initial Oracle arrangement. Joining receives an
advertised locator and repeatable seed locators, obtains the bootstrap through a
seed, verifies agreement between successful seed replies, and follows current
Oracle contacts. Unreachable seeds cause retry, never a second genesis.

A restricted discovery exchange does not require the applicant to be in
the receiver's active membership:

```text
DiscoverSystem(requestNonce, advertisedLocators)
SystemDiscovered(requestNonce, canonicalSystemBootstrap,
                 membershipGenerationId, controlPrefix, contacts, oracleContacts)
OfferHeraldAdmission(requestId, systemId, heraldId, freshHeraldEpoch, locators)
AdmissionOffered(requestId, admissionId, admissionControlIndex, disposition)
```

The receiving active Herald forwards the exact offer through its Oracle client;
an applicant is not given a fictitious active-Herald Oracle envelope home. It
retries the same request through another seed if needed. A contact list identifies
each observed Herald epoch and advertised locator; it cannot establish admission.
Separate the contact cache from the checked active/pending catalogue lookup.
Unknown contacts can be learned and retried for discovery. Ordinary `PeerHello`,
publication, placement, alignment, and evidence admission still require a current
active binding. An admission binding accepts only the named onboarding exchange.

Existing nonce/binding replacement and paced dial ownership remain in Discovery.
Gossip merge is set union over observed contacts, not a membership transition.
Retired contacts may remain diagnostic history but produce no ordinary dial
intent. Under eventual connectivity through the seeds, transitive exchange finds
all admitted Heralds without preconfiguring their identities at every machine.

## 3. Join policy and availability

Use one pending semantic admission and one explicit join barrier. This first
implementation deliberately pauses **all new application-operation admissions** while
existing members capture and transfer a closed data-plane cut. It is an
administrative pause whose duration depends on reachable members and the
data to transfer, not an instantaneous gossip operation.

After applying `BeginHeraldAdmission`, old members gate all eight operations:
`newid`, `newenv`, `write`, `forward`, `read`, `local-take`, `wait`, and `label`,
as well as new process preparation, attachment grants, and pending-wait acquisition.
The session/call owner retains their normal correlated pending status; cancellation
and exact retries preserve the same call. Gates do not allocate identities or
claim semantic acceptance early. Already completed result lookup, cancellation,
session close, and diagnostics remain available. Reads are included because normal
possession can activate an endpoint and change placement; a wait cannot bypass
the barrier by returning a fresh possession. Private maps are never transferred.

Already admitted finite effects, peer deliveries, structural readiness, Oracle workflow
reports/resolution/release, and alignment progress continue through the barrier.
Once a member captures its immutable history for the current admission attempt,
it stops authoring new alignment plans until activation, cancellation, or an
invalidated attempt. Already authored anchors, old local obligations, readiness,
and historical certificates continue. Capture waits for any accepted label
workflow to close, so this handoff cannot stop the work required to finish that
label. The least old member's sealed frontier is therefore its final authored
frontier for that attempt; the seal does not certify closure of old alignment
inputs. Before capturing a nonempty frontier, that coordinator waits for exact
cut acceptance by its still-current original fixed members. This proves plan
admission, not input closure. An inherited frontier carries the same checked
admission evidence through subsequent joins without creating local acceptance
votes on its passive holders. Empty and invalidated plans are not admission
waits. Late old anchors retain the authority of their recorded placement
membership even if a newcomer sorts before their announcer.
`EndProcessEpoch`, failure decisions, and terminal fencing remain possible. An
unclaimed child descriptor, a pending OS launch, an unanswered wait, or an unmet
application dependency is not itself a barrier obligation. Capture completed
contiguous prefixes and ready finite effects that can precede the join cut. Retain
unresolved accepted stages and waits behind that cut, including a publication
awaiting a definition which an application has not supplied. A pending `newenv`
call keeps its exact roots; an unclaimed child need not attach or obtain a later
parent relabel. Recheck deferred acquisitions against the resulting current state.

Opening admission aborts open disappearance probes as membership preparation,
retains existing local candidates, and gates new Opens. Each abort carries the
exact admission identity and Begin control index, preserving the probe's captured
predecessor membership. Cancellation retains this terminal evidence and permits a
fresh Open after reevaluation; activation requires the established successor
structural base before reopening. Preparation is distinct from an abort caused by
an actual membership successor. Existing label workflows finish
normally; a pending join is not permission to abort an accepted label on timeout.
New admission starts only with an established current membership base. Retirement
may interrupt preparation or closure and has the rules below. A failed joining
process can be cancelled without changing active membership.

Admission occupies the shared membership-coordination slot in a preemptible
preparation state. It excludes ordinary voter changes but does not suppress fresh
failure probes against existing members under a stable voter configuration.
Accepting sufficient failure evidence atomically cancels pending admission before
claiming the retirement intent. An unfinished voter-change intent cannot be
preempted this way. A semantic retirement releases its coordination slot after
its committed projection; subsequent data-plane base closure is not a slot lock,
so another authorized retirement can extend an unfinished closure.

## 4. Join protocol and activation

The Oracle-owned states are `Preparing`, `Sealed`, `Ready`, `Activated`, and
`Cancelled`. A seal has its own attempt identity within the stable admission ID.
Commands carry only checked manifests, cut identities/digests, and normalized
reports; they never carry graph, store, or publication payloads.

1. **Begin.** `BeginHeraldAdmission` checks the applicant manifest and captures
   old generation `G` and exact old members `M`. Its watch entry installs the
   barrier at each old Herald. A pending catalogue entry is not an active member
   and cannot host a process, publish, contribute all-member evidence, or vote.
2. **Drain and seal.** Each old Herald completes its ready finite predecessor
   effects and reports an exact completed publication/input cut and applied
   control prefix, retaining unresolved suffix work behind that cut. The
   ordinary old-member cut protocol establishes the common structural cut `K`.
   Each source's captured high-water mark includes every already-stamped
   structural occurrence. `K` must cover all of these marks: stamping already
   requires complete admitted causal prerequisites, so this is finite retained
   transport/replay work while all old members survive, not a wait for future
   application input. Only unresolved unsequenced structural stages may remain
   outside `K`. A donor failure takes the cancellation/retirement path instead.
   The least old epoch coordinates a `JoinSeal` naming `G`, `K`, the control
   prerequisite, every old-member input/store cut, and the bootstrap contribution
   described below. Every old member accepts the same seal only after reproducing
   those facts. No part of this old-member evidence requires a newcomer report.
   Each source capture also fixes its alignment authoring frontier for the exact
   admission ID and attempt. Captures from older attempts cannot hold the current
   attempt's coordinator; continuing old evidence never rewrites that sealed
   frontier.
3. **Transfer.** The applicant obtains the immutable bootstrap and contiguous
   canonical Oracle control history, then checked structural occurrence history,
   topology certificates, sort-definition occurrence/suppression facts, and the
   mandatory private-system-view data cut. It constructs its own owned state
   through pure admission transitions. It does not clone another Herald.
4. **Ready.** After applying the seal's complete input cut and constructing the
   exact candidate successor-base recipe, the applicant reports `HeraldJoinReady` with
   the admission ID, seal attempt/digest, control prefix, and candidate base
   digest. Old members report `HeraldJoinBaseReady` for the same candidate. Each
   report means the full required bytes are installed in that reporter's own
   retained history, not that a peer promised to send them later.
5. **Activate.** `ActivateHerald` requires all `M` readiness reports and the
   applicant report for the same current seal. It checks no intervening semantic
   control change invalidated that seal, commits the successor membership, and
   publishes its exact activation certificate in the control watch. Applying
   this entry installs the already checked candidate base at every member,
   enables the newcomer's ordinary bindings, and releases each local barrier.

The old-member reports and newcomer readiness are deliberately separate phases.
The newcomer needs old `K` to prepare, and never participates in establishing
old `K`. The activation certificate supplies the first successor base; it does
not wait for a second all-member barrier which presupposes that activation.
Subsequent structural cuts use all active members, including the newcomer.

Readiness hashes a canonical base recipe, not a guessed future control index.
Applying `ActivateHerald` substitutes its actual `ControlIndex` to derive the
successor generation and final base identity. The certificate binds that index,
the sealed recipe, and all exact readiness reports. Oracle checks evidence equality;
each Herald has already checked the recipe's graph meaning locally.

Oracle retains a semantic-cut change token alongside the seal. Process End,
label release/delete, or another relevant ordered change after sealing makes its
readiness stale. It reopens seal collection with a new attempt rather than
accepting an obsolete base. Report commands themselves do not invalidate it.
The old members retain and reoffer the already checked transfer prefix. With
finite work and eventual quiescence, a seal can complete; ongoing failures can
delay administrative admission without creating false readiness.

Cancellation commits `CancelHeraldAdmission`, releases old barriers, and permanently
ends that pending epoch. All late report/transfer inputs name the cancelled attempt
and have no activation effect. A subsequent launch uses a fresh epoch. Retirement
commits cancellation of an unfinished admission in the same ordered projection;
an operator can offer a fresh applicant after the new base is established.

## 5. What is transferred and retained

`Herald.Join` is an opaque pure owner of admission cuts, transfer progress,
and evidence references. The restricted EDSC lane on the peer listener carries
the current canonical onboarding vocabulary before ordinary EPRP admission:

```text
ReadJoinStatus
SubmitJoinCommand / ReadJoinCommand
CaptureJoinHistory / InstallJoinHistory
ReadJoinControlHistory(afterAppliedControl) / InstallJoinControlHistory
ReadJoinReady
```

Control synchronization requests the exclusive semantic cursor reported as
`joinStatusAppliedControl`. The source returns exactly the contiguous canonical
entries after that cursor through its own applied prefix; a cursor beyond that
prefix is rejected. Repeating synchronization at the current prefix returns an
empty list. `joinStatusControl` continues to report the observed control
projection, which may lead semantic application while work is frozen. The two
coordinates are encoded separately, so synchronization cannot skip unapplied
work by using the observed cursor. `InstallJoinControlHistory` retains the same
ordered admission and activation boundary as complete-prefix replay.

Each immutable history bundle names system, admission, seal attempt, source and
prerequisite cut. It retains canonical semantic IDs, source revision coordinates,
original payloads and complete-prefix claims. Its digest binds the source's seal
entry. The finite prototype transfers complete bundles; it has no separate
chunk-inventory protocol. Requests/responses are idempotent and independently
retryable. A replacement connection retries the retained logical request and never
changes its admission ID. Malformed and stale claims reject at the owning
protocol or semantic boundary. Contradictory checked retained bytes are an
invariant fault, not a new recovery protocol.

Structural transfer reuses exact original occurrence identities, source authority,
publication identities, and generation-stamped predecessor vectors. Replay
reconstructs the common graph from the immutable initial topology, actual retained
occurrences, control history, and established membership-base certificates.
Already superseded values remain available when needed to reproduce an old cut.
Retired-source relays require the established evidence described in section 7.

Alignment transfer carries the source's effective latest frontier for each sort,
including an explicit empty frontier, and its finite predecessor closure. Each
referenced coordinate includes the complete sibling cut catalogue, exact
historical placement revisions, and original member-readiness and
historical-certificate evidence. Replay derives relations and checks each plan
against the installed topology, exact placement, named certified predecessors,
and captured fresh bases. It never orders plans by their identifiers or receipt-map order.
Placement history installation corroborates local-owner coordinates and adds
remote historical coordinates without changing current routes or reviving a
retired owner.

Transfer also records completed structural cause/sort coverage at its exact
topology and placement coordinate, including a completed plan with no classes.
The receiver derives its own matching debt keys from that immutable witness;
it does not copy the sender's local obligations, attempts or sequence counters.
Coverage is adopted with the sealed frontier at activation. Merely receiving
historical generations does not prove that a cause was completed, and merely
receiving an unactivated history bundle does not suppress current work. Coverage
shares the ordinary plan-invalidation and recovery rules, so a tombstoned
coordinate cannot continue to hide debt or become executable again.
For an inherited current generation, a live remote snapshot at or beyond its
frozen owner revision proves withdrawal of any missing exact Store incarnation.
This also applies when that snapshot was received before frontier adoption;
an absent or older snapshot does not prove a loss.
Loss-cause discovery includes passive completed coverage. Placement updates and
installed structural cuts reconcile known losses before authoring successor
generations, so a replaced frontier cannot hide an inherited invalidation.

The applicant retains those facts passively: it creates no old application Store,
promotion receipt, obligation, attempt, subscription, cut acceptance, or local
readiness vote. Immediately before applying its exact activation entry, it adopts
the sealed least-old member's frontier. The admission attempt and that source's
digest-bound seal entry must still match. Preparing and invalidated attempts never
choose executable ancestry. Other donors corroborate immutable facts; they do not
select or overwrite that frontier. An actual old member continues ordinary
historical-anchor admission and its own obligations. A superseded plan which it
never received does not become a new join-readiness obligation merely because
another donor retained it.

Original owners continue their existing transfers after capture. Readiness and
certificates completed later reach current peers, including the newcomer, for its
already imported old cuts. Each source retains the applicant's exact catalogue
IDs for the current admission attempt; a new attempt replaces that interest and
stale attempts cannot expand it. Unrelated old evidence is not sent to an
applicant which lacks its generation. Receipt acknowledgements retire advertisements and do
not make the newcomer an old cut participant. The captured frontier is a handoff
of generation ancestry; it is not proof that old input is closed, and predecessor
bootstrap still obeys the ordinary input-closure and certified-prefix rules.

Private-system-view transfer includes explicit semantic snapshots and source
revisions in each history bundle, using the Store and Alignment evidence laws
already used for context transfer. It carries
normalized retained values, strength, sort-definition occurrence, and terminal
suppression at the seal cut. It creates fresh locally owned store incarnations.
It carries no other Herald's maps, subscribers, waits, session results, physical
dispatch attempts, mutable graph indexes, or private kernel representation.
Historical transfer from several sources unions equal semantic facts using the
existing Store rules; it does not pick one arbitrary donor as the whole system.

The onboarding source freezes a canonical aggregate containing Projection,
Registry, Progress, carrier facts and semantic History. Its member-cut digest
binds every frame. History retains normalized structural placement/alignment
material and an explicit covered control anchor plus contiguous suffix. Portable
owner admission and complete-source reconstruction preserve the exact attempt,
seal and certified structural cut; see [joining bases](control-history.md).
Canonical decoding/re-encoding rejects trailing or alternate bytes, duplicate
coordinates, incomplete sibling catalogues and absent-member evidence.

The newcomer has no application delta stores to populate. Later child possession
and endpoint activation create its own stores and ordinary alignment obligations.
Ordinary application data is copied only through those capability-qualified paths,
not broadcast to a joiner merely because it became a Herald. Control-log replay
alone is insufficient to supply the mandatory structural/system-view data plane.

Admitted structural history, membership-base certificates and normalized system
material retain their semantic consumers. Checked replay bases may replace raw
control prefixes while exact request pins and admission tails preserve required
bytes. Transport acknowledgement alone does not release semantic history. A lost
sole in-memory copy cannot be reconstructed from its digest. These transfers do
not authorize old-epoch restart or arbitrary semantic-history deletion.

## 6. System views of a new Herald

Each admitted Herald needs exactly the same predefined private system-view roles
as a genesis Herald. Derive their identities
from `(SystemId, HeraldEpoch, predefined role, identity purpose)`; check them
against the complete admitted catalogue for the required disjoint relationships.
Private store incarnation and delta identity remain distinct domains.

A pending admission's `HeraldBootstrapContribution` is an exact closed manifest
containing these system views. It becomes effective
only in the admission successor base. No application handle, ordinary label,
process possession, or exposing graph edge is created by it. Its provenance is a
new nominal `MembershipAdmissionOrigin(admissionId)`, enabled at the activation
index, rather than a pretended control-zero origin. This is topology provenance,
not a new application publication authority.

The founder launcher receives the initial full primordial root set from checked
genesis. Every later child receives a parent-selected set of already existing
objects, so a newcomer does not require a hidden writer to create first-process
roots. Keep the existing genesis-only installer narrowly genesis-owned; remove
the configured-future-process root-publication path when minimal process admission
replaces it. Ordinary `newenv` retains its caller-labelled publication semantics
and does not acquire system-bootstrap privilege. A partial primordial set grants
only the operations for which it supplies the required ordinary capabilities.

The candidate join base extends `K` with exactly that contribution and an initially
empty newcomer structural component. Survivor vector components retain their
coordinates. Applying activation installs the contribution once and derives the
new local/remote private routes; only cut-qualified placement snapshots advertise
them. Its first child uses transferred selected possession at these established
control/topology coordinates; no new child root is created as a side effect.

## 7. Repeated retirement and unfinished bases

Committing a retirement immediately removes the epoch from authority, ends its
resident live/prepared processes, closes its bindings, and records exact affected
store/stream consequences. Establishing the successor structural base is subsequent
Herald work. Another failure can therefore commit another retirement before this
base finishes; administrative serialization cannot prevent that physical event.

A `MembershipBaseClosure` coordinates repeated retirement:

```text
MembershipBaseClosure {
  establishedAnchorCut,
  orderedMembershipChanges,
  targetGeneration,
  retiredEpochs,
  survivorInventories,
  retainedEvidenceReferences,
  terminalSourcePrefixes,
  replayableOccurrences,
  retainedAheadOccurrences,
  terminalControlPrefix,
  successorInitialVector,
  terminalTopologyDigest
}
```

The anchor is the latest established certificate jointly available to the current
survivors on their unique retained cut lineage, not an arbitrary local live graph.
Preserve the existing Graph.Progress rule of one open cut and exact installed
predecessor, which makes established cuts a chain even when transient vectors are
incomparable. Genesis is the fallback anchor. Never choose an arbitrary maximum
from incomparable unestablished candidates.
Inventory exchange includes established certificates; select the greatest proved
comparable anchor and retransmit it before sealing the inventory set. If an older
unestablished attempt is abandoned, retain its occurrences and evidence. A lost
announcer does not make its advertised but unproved candidate an established cut.

Every change from anchor generation to target generation is present in order.
No admission activates inside an unfinished closure. Consequently the pending
chain contains retirements only, and the terminal source set is the exact union
of epochs removed by those changes. Seal every still-live source's allocator for
the previous generation before preparing the next transition. Unsequenced accepted
work waits; already stamped work keeps its immutable origin.

For each retired source, freeze what each current survivor had semantically retained
when that source was excluded. Include admitted relays and previously retained
inventories/certificates with their original provenance. A new inventory can cite
an old inventory, but an old digest or acceptance cannot stand in for bytes that
no surviving owner holds. Relay transport must come from an active survivor which
actually retains the canonical occurrence; the embedded source remains retired.
Do not accept a new direct contribution from the retired source.

Union equal claims by occurrence ID. Unequal checked payload digests fault. Derive
the unique greatest contiguous causally replayable prefix for **each** terminal
source, jointly: a prefix cannot include an occurrence whose predecessor needs an
unavailable occurrence from another terminal source. Normalize the mutually
dependent prefix calculation to its fixed point; do not select an arbitrary lower
frontier. Current survivor sources repair the dependencies they still own through
their ordinary retained structural history. Keep every nonreplayable ahead claim
and payload separately in the closure digest; a gap does not erase the tail.

Starting from the anchor, the terminal predecessor frontier is the least closure
covering those terminal prefixes and their complete dependencies. Interpret every
vector through its recorded generation ancestry; never compare vectors from
different member sets componentwise. The final initial vector projects that exact
closure onto current survivors, without renumbering their source sequences.
Reconstruct the topology at the last retirement's exact control prefix, including
all intervening process/label consequences and no later control state.

The least current survivor announces the canonical closure; one acceptance from
every current survivor establishes it. Acceptances from an abandoned attempt do
not satisfy the new target generation's reporter set, although their source facts
and payloads remain reusable. A further retirement cancels outstanding physical
attempts, extends the same retained closure lineage, and recollects current-survivor
reports. All earlier established cuts remain predecessors; no branch is merged.

After installation, project already applied survivor-stamped work above the base
into its descendant frontier without replaying effects. An old occurrence can
cross several retired generations only when each removed-source dependency is
covered by the compound terminal evidence. Unsequenced work first stamps under
the established generation. This supports more than a direct-successor-only
completion checks with an opaque, checked descendant-settlement witness.

## 8. Concurrent semantic obligations

| Owner/work | Admission and retirement rule |
| --- | --- |
| Label | Join waits for existing successful-decision installation/application completion. A canonical decision is immutable: retirement removes impossible installation reporters without turning success into failure. Collect over the captured set intersected with current survivors; a newcomer never joins an old report set. Predecision comparison and process liveness are decided by the Oracle. See [labels](labels.md). |
| Disappearance | Preparation or membership change aborts the exact old probe and retains its candidate. Reopen only after a current established base, capturing a new complete membership/input/alignment cut; never reinterpret missing reports as absence. |
| Publication | Acceptance freezes identity, authority, source, and application destinations. Retirement removes only exact retired destinations. Already retained retired-origin work may apply to its frozen surviving destination; no application route is added because of join. Later alignment covers newly relevant routes. Mandatory structural-history replication separately covers every active system view, including newcomer delivery of retained stages first stamped after activation. |
| `newenv` / selected startup grants | Manifest, generated identities, source authority, accepted positions, and already assigned stamps remain fixed. Completion may use any proved descendant base covering those exact roots. A surviving home completes once; a retired home yields no application result. Never recover a lost untransmitted manifest from Oracle metadata; minimal child admission creates no replacement roots. |
| Placement / Store | Applying retirement withdraws only the exact owner's routes/incarnations and derives actual physical losses. A membership-generation change alone does not recreate surviving stores or withdraw their routes. |
| Alignment | Replan the retained obligation after a genuine selected source-incarnation loss. New generations use current cut membership; old immutable certificates retain their original coordinates. A stale attempt receipt cannot satisfy its replacement. |

No operation reports application success merely because membership committed.
Store reconciliation, publication settlement, prepared-label release, and their
existing terminal conditions remain necessary. A join transfers only released
label outcomes and already ordered suppression, not another Herald's prepared
private patches. Retirement carries accepted outstanding work forward, including
work whose application has ended, until its semantic owner settles it.

## 9. Voter retirement and self-fencing

The [Oracle design](oracle-raft.md) commits `AcceptVoterHostFailure` with an exact
old-configuration failure certificate, performs joint then stable voter removal,
and only then
commits semantic retirement using that retained authorization. Automatic retirement
uses this same path. Voter demotion alone does not end Herald membership. The
target's acknowledgement is not a prerequisite; failure is permanent exclusion of
an epoch, not proof that its operating-system process stopped.

Remove Isolation's current blanket `VoterHostIsolationDisabled` rule for semantic
service. Every active Herald tracks Oracle quorum reachability against exact
native-owner effective-configuration observations obtained through its Oracle
client; a co-hosted replica supplies the same typed observation locally when one
exists. An ordinary non-voting Herald need not host a replica. Joint configurations
require both majorities from first effective append, even before the committed
Oracle projection observes the change; an older stable watch projection cannot
relax that requirement. The local hosted voter counts only as its actual binding.
A configuration
change replaces the qualified observation set, not the meaning of old observations.
Failure timers retain an absolute uninterrupted isolation deadline; changing
configuration or reconnecting without a sufficient fresh quorum does not restart
it. Single-voter healthy startup recognizes its available local voter.

After grace, the Herald owner atomically seals semantic origins and new/resumed
sessions, installs the existing local zombie overlay for its resident processes,
and performs the existing bounded read-only drain to terminal unavailability.
Committed local Herald retirement or a checked accepted-host-failure notification
reaches the same
irreversible path without waiting for grace. Regaining connectivity cannot reopen
the semantic epoch. Exclusion evidence is admitted through the checked Oracle
failure-intent/membership history, not any unqualified peer statement. Native voter
exclusion for ordinary demotion never triggers semantic isolation or retirement.

Self-fencing the semantic Herald does **not** unilaterally alter, shrink, or stop
its cohosted Raft owner. That separate owner continues its configured replication
and voting duties until protocol-directed removal or actual process shutdown.
This distinction lets a temporarily partitioned voter help commit its own later
exclusion without reviving the abandoned semantic branch. The runtime supervisor
must support separate semantic-role terminalization and Raft-role lifetime.

Every survivor rejects old-epoch handshakes, direct publications, placement,
alignment, application claims, and fresh reports. The only retired-origin replay
exception is exact retained source material relayed by a surviving active owner
with checked closure evidence. A fresh replacement has fresh stores and processes;
it enters through admission and receives no old private identities or sessions.

## 10. Owners and verification

Discovery owns contacts and restricted onboarding. Oracle owns checked membership
and canonical admission. The composed Herald transition installs membership,
structural-base closure, process/label consequences and runtime intentions together.
The [owner contracts](../verification/owner-contracts.md) and
[membership ledger](../verification/membership-ledger.md) track the checks at these
boundaries.

Required pure properties include canonical round trips and invalid-set admission;
exact replay versus incremental catalogue projection; gossip without admission;
join activation iff complete matching cut evidence; no circular newcomer barrier;
gate/cancel/End/accepted-label/newenv interleavings; bootstrap identity and authority
laws; replay equivalence at exact historical cuts; and no copied private identity.
An unresolved definition/provider dependency and an unlaunched prepared child
must not prevent sealing, and no read/wait acquisition may bypass a closed seal.
Generate arbitrary join/retire chains, including two failures before closure,
different retained terminal tails, cross-source dependencies, lost announcers,
stale acceptance sets, and duplicate relays. Prove unique normalized closure,
retained evidence monotonicity, no fabricated payload/loss, at-most-once settlement,
and projection independence from the order of admissible concurrent deliveries.

Representative real-TCP tests start a newcomer from one seed in a nontrivial gossip
graph, interrupt transfer and resume it, start its first application after admission,
join while publisher/reader processes run, cancel admission, fail a donor, and
retire two members while the first base remains unestablished. Verify placement
and alignment continue on surviving stores. Partition a still-running voter-host
Herald through isolation and later removal, observe terminal application behavior,
and prove that its old epoch cannot resume while its Raft role follows its own
configuration. Run same-Herald and different-Herald prepared-child startup during
join gates, with delayed OS launch and lost claim replies.

All suites retain the benign, finite, in-memory model. This design adds no resource
ceilings, counter exhaustion outcomes, schema versions, sort-restricted edges,
durable restart, private-state snapshots, or authority inferred from transport loss.
