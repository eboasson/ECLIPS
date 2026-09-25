# Conceptual Oracle and Raft replication

This document owns the ECLIPS facts that actually require a global order and their
replication. The Oracle is conceptual: callers see one command history and one
result per request. Physically, an explicitly administered subset of active Herald
processes hosts replicas of that state machine using Raft. Profile 0.2 retains
replica registrations and voter-change intentions in Oracle state; committed
native stable/joint entries alone advance its voter configuration projection.

The committed watch is a tagged control history. Command entries carry immutable
request receipts and optional receipt/label progress metadata; native configuration
observations carry their exact Raft entry reference and checked binding
projection, without a fabricated client request. Both consume one ControlIndex
on first application. Standalone progress maintenance consumes one ControlIndex
only when either component advances, without creating a receipt. Leader no-ops and
request retries/conflicts without new retirement progress advance only the
applied Raft cursor. Learners replay every
committed native position in order. Configuration metadata is prepared from a
retained intention and accepted only through the sealed committed-entry adapter.
See [the profile-0.2 contract](../architecture/oracle-raft.md) for the proposal gate,
catch-up observations, cancellation, and cross-plane coordination rules.

## 1. Oracle responsibility

The Oracle exists because several paper promises cannot be derived from one
Herald's asynchronous view:

- process death creates one ordered zombie-label transition;
- `label` has one authoritative conditional decision and distributed installation completion;
- Herald retirement selects one monotone membership generation from
  fresh voter-host probe evidence;
- predefined-disappearance decisions require evidence from every Herald in the
  captured membership generation; later reclamation remains a separate deferred
  evidence workflow; and
- regular-definition retirement requires one common occurrence boundary.

Controlled instantiation, current update, and obsolescence are deliberately absent
from this list. The publishing Herald validates the value, its local
process/nabla/possession authority, and the single-winner reservation transition,
then publishes directly. Each receiver admits the same value against its local
descriptor and controlled history. The Oracle neither approves nor rejects those
publications and does not establish their current/obsolete state.

The Oracle MUST NOT contain:

- regular sort-definition objects or regular or controlled application payloads;
- delta visible or retained stores;
- process-private application ID maps/local counters or pending `newid`
  reservations;
- Herald-local possession indexes;
- the reconciled graph or reachability result;
- delta placement advertisements;
- publication counters, outboxes, inboxes, or acknowledgements;
- context obligations or alignment snapshots;
- application wait registrations; or
- semantic peer discovery contacts. Registered Oracle replica endpoint hints are
  retained only to reconstruct replication and client dialing; they grant neither
  semantic membership nor voting authority.

Neither regular nor controlled publication volume therefore causes Raft traffic.
Process lifecycle, label operations, evidence-based disappearance/retirement, and
the future reclamation permission are expected to be much rarer.

## 2. Oracle state

At the complete-profile boundary the deterministic state machine contains:

```text
OracleState = {
    genesis,
    controlIndex,
    requestResults,
    activeHeraldProgress: HeraldEpoch -> (receiptRetirement, labelsConsumedThrough),
    labelRetiredThrough,
    latestLabelDecision,
    oracleReplicaRegistrations,
    voterConfigurationHistory,
    voterChanges,
    pendingVoterChange?,
    heraldMembershipHistory,
    heraldCatalogue,
    heraldAdmissions,
    pendingHeraldAdmission?,
    openHeraldFailureProbes,
    heraldFailureProbeHistory,
    processEpochs,
    labelRecords,
    pendingLabelInstallations: DecisionId -> installation,
    completedLabelWitnesses: DecisionId -> (decisionIndex, membershipGeneration, outcomeDigest),
    disappearanceProbes,
    regularDefinitionRetirementIndex
}

OracleGenesis = {
    systemId,
    predefinedCatalogueDigest,
    canonicalPredefinedDescriptors,
    activeHeraldMembership: ascending [(HeraldId, HeraldEpoch)],
    configurationDigest: ConfigurationDigest,
    initialAppliedProcesses: ascending [AppliedProcessBootstrap],
    initialTopologyProjection: CheckedInitialTopologyProjection,
    initialProjectionDigest: InitialProjectionDigest,
    raftVoterBindings: ascending [(RaftNodeId, HeraldEpoch)],
    raftNativeConfiguration,
    raftConfigurationDigest
}

LabelRecord = {
    objectId,
    canonicalLabelOrDeleted,
    labelRevision,
    authorityEpoch?
}

InitialLabelEvidence = checked {
    objectId,
    rawInitialLabel: (LabelOwner, 0),
    source: BootstrapRoot(processEpoch) | ObservedPublication(PublicationId)
}

PriorAuthorityJustification =
    ExistingReleasedAuthority(canonicalLabelRevision)
  | CheckedGenesisAuthority(initialProjectionDigest)
  | EstablishedStructuralAuthority(StructuralOccurrenceId, TopologyCutId)

DisappearanceSubject =
    ControlledPredefined {
        objectId,
        expectedStructuralOccurrence,
        expectedLabelRevision?
    }
  | RegularSortDefinition {
        sortId,
        canonicalDescriptorDigest,
        expectedOccurrenceId
}
```

The state includes checked Herald and process catalogues, admission records,
label and disappearance workflows, failure evidence, and native voter projections.
Admission retains one pending applicant with sealed attempts, readiness and
terminal outcomes.
`heraldMembershipHistory` contains the genesis generation, current generation,
predecessor chain, and retired-epoch tombstones. Failure-probe state contains no
peer structural inventory or data-plane state.

`configurationDigest` binds the immutable system, initial Herald membership,
closed predefined catalogue and primordial publications. The separately checked
initial projection binds the exact genesis processes, roots and topology. Neither
commitment includes future process starts or a live process/Herald catalogue.
Oracle retains the already-derived configuration commitment because it does not
own the primordial publication payloads. EORC compares these immutable claims;
current residence and membership authority come from the applied Oracle state.
There is no configured future-process catalogue or parity digest.

`requestResults` maps a stable Oracle client request identity to the canonical
command digest and immutable receipt. It supplies exact retry after a reply is
lost. This is a receipt for that one command. A long-running label or disappearance
workflow has a separate monotone status keyed by `DecisionId`/probe ID; later
commands advance that status without mutating any earlier command receipt.
The current prototype can retire Oracle receipt copies at an explicit replicated
home-epoch request frontier. The exact-result guarantee applies until that
frontier reaches the request sequence or its home is semantically retired; a
retired request has a typed obsolete outcome. The frontier releases the Oracle
request-map and runtime published-result copies. Copies embedded in the retained
control watch remain part of that history.
The [receipt-lifetime contract](../architecture/control-history.md#oracle-request-receipts)
specifies the retained owners and the remaining unbounded histories.

`labelRecords` contains successful canonical decision outcomes (plus checked
genesis label/authority facts). Absence of a record says nothing about whether a
controlled object exists: a newly published object may remain entirely data-plane
until its first label operation. A record supplies the canonical overlay that every
Herald applies to any local or later-arriving copy; it is not a catalogue of
controlled IDs, sorts, values, descriptor policies, or current/obsolete states.
Process-label normalization does not require enumerating every controlled object in
the Oracle: applying `EndProcessEpoch(P)` makes each Herald interpret both retained
and later-arriving `process(P)` labels as `zombie(P)`. Released label records that
name `P` are subject to the same projection. For a first overlay without a
record, `InitialLabelEvidence` supplies the independent raw generation-zero
label. The Oracle applies its canonical process lifecycle to that label before
comparing the caller's expected pair. The expected pair alone never supplies
missing object state. Once a `LabelRecord` exists, its released state and revision
remain authoritative even when a command also carries older initial evidence.

`authorityEpoch` is present only for an authority-bearing controlled role such as a
nabla. A non-authority-bearing target has no synthetic authority merely because it
was labelled. For a first Oracle overlay on a nabla, the decision carries the exact
pre-existing genesis or structural authority and its checked provenance; the
Oracle does not invent one from target existence. A present local target validates
that provenance at installation. Once a canonical record exists, its current
authority and revision supply `ExistingReleasedAuthority`; stale caller-local
revision or authority expectations are not additional CAS conditions. `CheckedGenesisAuthority` justifies only `GenesisAuthorityEpoch` under
the common initial projection. `EstablishedStructuralAuthority` justifies only the
equal `StructuralAuthorityEpoch` after the named occurrence and cut are installed.

Oracle genesis is exact rather than digest-only. Herald IDs and epochs are both
unique in `activeHeraldMembership`. The applied process/root records and checked
topology are the complete normalized control-index-zero projection, and the
supplied `InitialProjectionDigest` MUST equal the canonical digest derived from
those exact two values. The Oracle uses the immutable topology only to check and
bind common genesis; it does not own or update a reconciled Graph.
Each `AppliedProcessBootstrap` includes the process identity/epoch/residence and
the complete process plus writer/reader root set for every closed predefined
catalogue role, all materialized at control zero. Oracle genesis derives its live
process records and the checked genesis label/authority overlay from those exact
values; each Herald independently installs the matching controlled root values and
structural graph projection from the checked bootstrap. An alternate root policy
or digest-only surrogate is not accepted.

`raftNativeConfiguration` is the normalized fixed `RaftNodeId` voter set plus
heartbeat and election-timeout bounds projected from checked Raft genesis.
`raftVoterBindings` has exactly those voter keys, maps them injectively to active
Herald epochs, and records co-hosting without equating Raft and Herald identities.
`RaftConfigurationDigest` is owned and derived by Oracle/composition startup as:

```text
SHA-256(cereal("ECLIPS-RAFT-CONFIGURATION",
              systemId,
              ascending[(raftNodeId, cohostHeraldEpoch)],
              heartbeatIntervalMicros,
              electionTimeoutLowerMicros,
              electionTimeoutUpperMicros))
```

Contact endpoints are runtime facts and do not enter either digest. Control index
zero is constructor semantics for this checked genesis, not a Raft log position.
The voter list is retained here to bind the exact co-hosting projection; it is not
a second mutable membership authority. Composition requires it to equal the
domain-independent checked Raft voter set before either owner starts.

`canonicalPredefinedDescriptors` is a closed genesis/bootstrap fact,
not a dynamic Oracle sort registry. Herald startup uses it to seed the primordial
regular definition replicas required to interpret the catalogue. Every
application-defined sort becomes effective only through a regular `sort-sort`
publication and peer/alignment delivery.

The prototype catalogue fixes the predefined edge-specific payload schema to
endpoints plus preserving/weakening strength, with no sort selector. Strength is
the closed enum `preserve :| [weaken]`; the exact symbols and their declaration
order are part of the descriptor. The common controlled-object key and label
fields remain outside that payload. The canonical predefined edge-carrier
descriptor encodes this payload shape, and its bytes contribute to
`predefinedCatalogueDigest`. Changing that shape means editing the sole schema,
catalogue, Herald codecs, and properties in lockstep and starting a fresh
`SystemId`/genesis; the current decoder accepts no alternative shape and negotiates
nothing.

The Oracle stores no current application value, controlled-object descriptor,
`SortId` binding, current/obsolete lifecycle, or reconciled structural definition.
The full checked value, including any topology definition, travels peer to peer.
Heralds validate its canonical descriptor and policy locally against an effective
regular `sort-sort` definition. Oracle label records and temporary label-decision
metadata are not authority for existence or visibility of that regular definition;
no second authoritative sort registry exists in the Oracle.

`regularDefinitionRetirementIndex` contains only the last successfully resolved
global cut for a `SortId`. It neither stores nor validates a definition and does not
say that the sort is currently effective. Heralds combine it with `SystemId` and
`SortId` to distinguish a post-retirement definition occurrence from stale
pre-retirement publication/alignment evidence.

## 3. Control index and log index

`ControlIndex` and `RaftLogIndex` are different types:

- `RaftLogIndex` counts every entry needed by Raft, including a leader's no-op
  entry; and
- `ControlIndex` counts application of canonical Oracle commands.

The checked genesis projection is control index zero. A publication using
primordial authority may therefore name a concrete source-authority prerequisite.
A dynamically published nabla's initial authority is instead established by its
exact structural occurrence and the receiver's data-plane structural frontier; a
later label assignment to a different live process uses the atomic decision
`ControlIndex`. A transition from process or zombie to void retains the prior
tenure instead. Target-object instantiation, currentness, and obsolescence never
become `ControlIndex` prerequisites.

A fresh delta, neutral vertex, edge or ordinary controlled object has no initial
target operation tenure. The authority of the nabla that published it remains
source-publication provenance; it is not an earlier label assignment to the new
object. This distinction also applies when a prepared child uses a handed-off
writer to create a new environment. Once the target has a released label record,
each later decision derives its revision and authority disposition from that
canonical record; a stale caller-local projection adds no extra CAS condition.

A first-seen, unretired ordinary Oracle request already occupies one
`RaftLogIndex` and consumes exactly one next `ControlIndex`, whether its receipt
accepts or rejects the requested semantic change. Any piggybacked retirement
shares that control entry. A later committed Raft entry containing the exact
same request ID and semantic command digest returns the recorded receipt;
conflicting reuse returns its deterministic protocol disposition. Neither
executes another semantic command. If its retirement metadata advances a
frontier, it emits one standalone maintenance entry at the next control index;
otherwise it consumes no control index and emits no watch entry.

A standalone `RetireOracleProgress` submission likewise creates one control
entry only when its frontier advances, without a receipt of its own. A
repeated/lower frontier, an obsolete ordinary request and a Raft no-op consume no
control index. Every committed position still occupies a `RaftLogIndex`. The
adapter acknowledges all classifications to Raft and keeps a separate applied-log
cursor; “no index” at the Oracle boundary means no `ControlIndex` only.

A Herald applies Oracle entries only as a contiguous control prefix. It exposes
that prefix as its `OracleProjection`. A peer publication may name a prerequisite
index in this same space.

The finite-run projection also retains every canonical applied
topology-affecting control entry—including process, released label/delete,
predefined-disappearance, and regular-definition-retirement progress—or equivalent
exact checkpoints sufficient to reproduce any older stable control prefix still
named by a topology-cut proposal or established cut. An ahead Herald never
validates an older cut from its current projection, and a later control transition
cannot erase that replay evidence. Durable compaction/restart remains deferred.

That prerequisite is narrow. It may cover the source nabla's released authority,
process liveness, a label fence/rewrite, or a regular-definition retirement
boundary. If peer data overtakes one of those facts, the receiver retains it until
the prefix arrives and then revalidates the source/fence fact. It MUST NOT require
an Oracle record for the publication's target object, use an Oracle rejection or
absence result to decide that the target was never instantiated, or use the prefix
to decide current versus obsolete. Descriptor/structural availability and local
obsolete/deleted suppression decide those data-plane states.

### 3.1 Topology order is initially peer-to-peer

Dynamic graph changes do not consume `ControlIndex`. Each source Herald locally
sequences the structural occurrences whose prerequisites it can apply locally,
while accepted-but-unready work remains unsequenced; each Herald advertises the
vector of per-origin structural sequences whose finite local effects it has
applied. An exact graph cut is named by that applied version vector together with
the canonical digest of its winning structural occurrences. Componentwise coverage
establishes graph progress; matching alignment-cut and historical-readiness
evidence is still needed before one Herald asks another to supply context.

The planned fallback, if property/model checking shows the peer-to-peer generation
lineage too complex, is a compact `TopologyActivationIndex`. It would order only
`(structuralOccurrenceId, publicationDigest)` after the payload was independently
published. It is a positive nominal counter distinct from `ControlIndex`, advances
only for accepted activation tokens, and is unaffected by interleaved label,
process, or evidence entries. A Herald would still advertise how far it had locally applied that
order, and the index would prove neither payload receipt, graph reconciliation,
placement, nor historical readiness. This fallback is not part of the initial
model, cannot approve or reject the publication, and requires a deliberate
specification change rather than an optional wire arm.

## 4. Closed Oracle command vocabulary

The prototype has the following command families. Adding one requires changing the
single closed command type, codecs, and properties in lockstep and starting a fresh
run; generic arbitrary commands are forbidden, and no compatibility command union
is retained.

### 4.1 Monotone Herald membership

`OracleGenesis` contains the exact finite initial set of active
`(HeraldId, HeraldEpoch)` pairs. Deployment starts those Heralds from the same
checked manifest. Later Heralds obtain that unchanged bootstrap through seed
discovery and enter through the Oracle admission workflow in section 11. Peer
gossip discovers addresses but cannot add, remove, or reactivate a member.

The canonical membership shape is:

```text
HeraldMembershipGenerationBody =
    GenesisHeraldMembership {
        systemId,
        activeHeraldEpochs,
        activeMemberSetDigest
    }
  | RetiredHeraldMembership {
        predecessorGenerationId,
        retirementControlIndex,
        failureProbeResolutionId,
        retiredHeraldEpoch,
        activeHeraldEpochs,
        activeMemberSetDigest
    }
  | AdmittedHeraldMembership {
        predecessorGenerationId,
        activationControlIndex,
        admissionId,
        admittedHeraldEpoch,
        activeHeraldEpochs,
        activeMemberSetDigest
    }

HeraldMembershipGenerationId = digest(
    "ECLIPS-HERALD-MEMBERSHIP-GENERATION",
    canonical(HeraldMembershipGenerationBody))
```

The ID is outside the body it hashes. Each successor names its exact immediate
predecessor and a strictly later control coordinate. Retirement removes exactly
one eligible current member and retains every survivor in ascending order.
Admission inserts exactly one fresh pending epoch and binds its Begin identity
and actual activation index. Checked history retains every generation; the
append-only Oracle catalogue retains terminal epoch tombstones. Repeated
retirements may commit while an earlier successor base is unfinished; Herald
owners extend their compound closure rather
than inventing an intermediate established cut. Admission starts only from an
established current membership base.

Failed-voter-host exclusion extends the monotone membership generation with a configuration-qualified
failure family:

| Command | Purpose |
| --- | --- |
| `OpenHeraldFailureProbe` | Capture one active target, the exact semantic generation, and the current stable voter configuration after local recovery grace expires. |
| `ReportHeraldFailureProbe` | Record one fresh `Reachable | Unreachable` result from a distinct host in that captured configuration, naming its exact configuration ID. |
| `DismissHeraldFailureProbe` | Close a probe after a strict captured-reporter majority finds the target reachable. |
| `AcceptVoterHostFailure` | Retain the old-configuration unreachable certificate and exclusively reserve normal native voter exclusion followed by semantic retirement. |
| `RetireHeraldEpoch` | Remove a non-voter after a strict captured-reporter majority, or consume a retained accepted voter-host certificate after final native exclusion. End the target's resident live processes atomically. |

An eligible report follows application of the Open entry and a fresh direct probe.
EOF, prior connection history and local timeouts alone are not quorum evidence.
The checked envelope home identifies the reporter. The Open record retains the
complete stable voter configuration, including its identity and exact node/host
bindings. Every report and terminal threshold uses that immutable reporter set,
including the suspected host in the denominator. One reporter has one immutable
result, so reachable and unreachable strict majorities cannot both exist. A
singleton requires one report, two voters require both, and three require two;
there is no special vote or reduced denominator for suspected voters.

There is at most one open probe for an exact
`(targetHeraldEpoch, membershipGenerationId, voterConfigurationId)`. Its first
Open derives the probe ID from its control index; simultaneous equal Opens alias.
An Open with stale semantic or voter configuration rejects. New probes cannot
open while a voter change is pending or the current configuration is joint. A
voter-change intention supersedes other nonterminal probes; their reports cannot
transfer to a successor configuration. Reports name the captured configuration
ID and reject a mismatch, ineligible reporter, conflicting repeat or terminal
probe. Equal terminal resolution retries preserve their original result.

A strict unreachable majority reserves the membership coordination slot before
terminal acceptance, so a competing admission or explicit voter change cannot
replace its semantic predecessor. For a captured non-voter, ordinary retirement
consumes this evidence directly. For a captured voter, `AcceptVoterHostFailure`
retains an immutable certificate containing the target, probe resolution,
semantic predecessor, captured stable configuration, reports, acceptance control
index and derived voter-change ID. Its authorizing probe becomes terminally
`VoterHostFailureAccepted`; later configuration supersession leaves that
certificate unchanged. This accepted intent cannot be cancelled.

The voter owner drives the accepted failure through ordinary old-to-old-minus-target
joint consensus and final configuration commitment. The final native entry
records `VoterExcludedAwaitingHeraldRetirement` and keeps the exclusive intent.
It does not silently retire the Herald. `RetireHeraldEpoch` subsequently checks
that exact accepted certificate, final exclusion and semantic predecessor, then
advances semantic membership and records `Completed`, releasing the slot in the
same entry. Surviving failure owners resume this operation from committed watch
history after leader loss or reconnect. Neither joint/final configuration changes
nor local demotion recomputes the certificate's old reporter denominator.
Unrelated semantic and voter changes remain serialized throughout; ordinary
non-membership commands may continue. Missing any native old or new majority
leaves the transition unavailable without local shrinking or fabricated votes.

Retirement supersedes other open predecessor-generation probes, ends resident
processes, and settles or aborts captured label/admission/disappearance work.
It never waits for a failed target to supply impossible label or structural-base
acknowledgments. Repeated retirements may extend the existing compound structural
closure. An already accepted voter-host probe remains terminally accepted after
retirement; the retained membership resolution and completed voter intent prove
its later semantic consequence. Exact retirement retries return their original
receipt while that request's retry entitlement remains live; receipt retirement
does not delete the separate membership resolution. Other authorizing probes
become `Retired`.

The target may still be executing. Final native exclusion removes its voter
authority; semantic retirement permanently rejects that Herald epoch. A future
Herald with the same logical identity requires a fresh epoch and ordinary
admission. Oracle owns the checked histories and certificates; survivor graph
closure, store loss, transport recovery and self-fencing remain Herald owners.
Graceful handoff, old-epoch restart and merging abandoned partitions remain
deferred.

### 4.2 Process lifecycle

| Command | Purpose |
| --- | --- |
| `StartProcessEpoch` | Create one live process epoch at one active Herald and commit its canonical process/residence fact. |
| `EndProcessEpoch` | Monotonically mark one exact epoch dead, passivate its authorities, and turn every canonical `process(P)` label into `zombie(P)`. |

`StartProcessEpoch` carries only fresh checked `ProcessId`, `ProcessEpochId` and
active residence `HeraldEpoch`. Stable lifecycle request identity is the Oracle
envelope identity. The target Herald generates and retains the identity once
before submission. Oracle checks active residence, records the live process and
its residence atomically, and preserves exact-request retry/conflict
semantics. It neither prescribes nor creates any root set.

Only checked index-zero genesis installs the initial launcher/root bundle. Every
later Start uses the ordinary dynamic process record; its root values, normal
possession grants, private aliases, selected startup access, labels and topology
remain Herald-owned. A process's own applied Start grounds its normal self grant.
Name-only aliases confer no possession or label/update capability. End remains
monotone and defeats a later attempted startup installation for the ended epoch.

`newenv` is not a process-lifecycle command. Its resident Herald generates the
canonical root manifest and maps every root role to the caller process's
explicitly selected, retained Nabla/Delta source writers, rechecking current operation. It freezes
those source authorities, directly admits/publishes the controlled structural
values through their positive publication sequences, and completes only after
every active-generation Herald has accepted the exact version-vector cut and the
resulting `TopologyCutEstablished` is installed at the home Herald. Initial root
labels are carried by those checked publications;
any later relabel uses the ordinary Oracle label workflow. The Oracle neither sees
nor authorizes the environment manifest, and exact application retry reuses the
Herald-retained manifest/publications.

The resident Herald serializes `newenv` acceptance against its applied process
projection. If `EndProcessEpoch` is already applied, admission rejects before
generating or publishing a manifest. If `newenv` was accepted first, its retained
direct publications and topology-stabilization work remain valid; later process
death can remove reply/map reachability but cannot roll them back. No Oracle
environment command, receipt, retry, or status query exists.

The endpoint controller remains controlled even when its carried data sort is the
regular `sort-sort`. Neither process-lifecycle command creates a `SortDefinition`,
reserves or allocates a `SortId`, or adds a label record for the regular definition.
Primordial regular definitions are genesis-seeded data-plane facts; application
definitions arrive only through ordinary `write`. Sharing their content-addressed
identities creates no graph path or capability between otherwise private environments.

`EndProcessEpoch` is the sole independently ordered process-End command. An
operator may request it only through the separately authorized
Herald-administration boundary. The resident Herald may also retain it internally
after its pure Application owner seals the final unexpectedly detached session at
the end of one absolute recovery grace. A connection loss or timeout observation
alone cannot propose it, and this automatic path is not an EAPP operation.
`RetireHeraldEpoch` separately derives End for every live resident process at the
retirement index rather than submitting one command per process. A dead process
epoch is never restarted; a later process uses a new epoch. Applying
the terminal entry at the resident Herald ends all of the epoch's sessions and
waits, cancels its pristine `Reserved` entries, and retires its private/global map
and next-local counter. It does not drop an already accepted direct publication or
its local/outbox history, including an accepted `newenv` manifest and structural
stabilization, nor an open label workflow, exact Oracle intent, or post-decision
barrier state. Exact
dispatch/barrier work continues until contiguous Oracle order reaches an Oracle
workflow's terminal settlement. The label fence separates accepted publications
that precede a process/authority change from work rejected against the later
projection; process end does not retroactively cancel the accepted prefix. Owner
records release only at their respective publication-acknowledgement or Oracle
workflow terminal points.
Process end does not
remove global objects/data or another process's mapping to the same global unique
IDs.

### 4.3 Controlled publication is not an Oracle command

The closed Oracle vocabulary has no command that instantiates, updates, approves,
rejects, or marks obsolete an application controlled object. Defining a sort is
likewise an ordinary peer-to-peer `write` of the regular, content-addressed
`sort-sort` object.

Reservation cancellation by the input-only application
`write(p, DeleteReserved(o))` arm consumes only the exact Herald-local pristine
`Reserved` entry. It is admitted before any internal semantic `Value` or persistent
`Label` is constructed: no object exists, no Oracle command is needed, and neither
the Oracle nor Raft protocol can encode this boundary-only form. Once generic
controlled `PublishValue` is executable, first publication is the competing local
single winner. One serialized home-Herald transition:

1. resolves and globalizes the process-private operands;
2. validates the pristine reservation, effective controlled descriptor and policy,
   initial label, process/nabla authority, capability, and value;
3. consumes the reservation, establishes the immutable local ID-to-sort binding,
   records publisher-retained controlled history/possession, and fixes the stable
   publication identity and retained request state; and
4. for ordinary data, or a structural carrier whose prerequisites are already
   applied, commits the applicable local effects and complete direct peer outbox
   work. Otherwise it retains a private unsequenced structural-admission stage;
   the later readiness transition allocates/stamps the occurrence, reconciles its
   finite local effects, advances the source vector, and inserts stamped peer items
   atomically.

No invisible Oracle-waiting stage occurs between those steps. Exact application
retry follows the retained local winner. A different first publication or
`DeleteReserved` observes the consumed branch and rejects locally. A lost reply,
session end, process end, or later authority change does not roll back accepted
publication/outbox work.

Every receiving Herald independently canonical-decodes the value with its effective
`SortDefinition`, checks the claimed `SortId` and policy, and establishes or checks
the same immutable object-ID binding in `Herald.Controlled`. Missing descriptors or
structural definitions are ordinary data-plane dependencies. Under the benign
profile, a conflicting sort binding for one generated controlled ID is an invariant
or protocol fault, not a conflict the Oracle resolves.

A current-to-current update follows the same direct path after local capability and
current-state admission. An obsolete value does too: applying it monotonically
records obsolete suppression, and no delayed or duplicate current publication can
make that object current again while the suppression is retained. A structural
carrier whose checked descriptor says application obsolescence is `Never` is
rejected by the publishing Herald before acceptance. Such an object leaves current
state only through the specified label-delete or evidence-based disappearance path.

The predefined process-epoch policy remains `heraldManaged`. Generic application
publication, update, obsolescence, label/delete, and disappearance are locally
rejected for that role; `StartProcessEpoch`, `EndProcessEpoch`, and the resident
process-End projection of `RetireHeraldEpoch` remain the only Oracle transitions
that establish or terminate process records. Application descriptors cannot assign
`heraldManaged`.

### 4.4 Label decisions

| Command | Purpose |
| --- | --- |
| `DecideLabel` | Compare the expected full label against canonical state, deriving a first overlay from independent initial evidence where needed; atomically commit an immutable `Applied` or `NotApplied` decision. Success installs the canonical label/delete and its authority disposition at this same control index. |
| `CompleteLabelDecision` | Admit one compact collector attestation that every captured survivor installed a successful decision, qualified by the current membership generation. |

The Oracle retains one successful decision awaiting installation completion per
object. A fresh competing decision for that object is deferred without a receipt
or control entry, and its unchanged request is retried after the slot clears.
Labels of distinct objects can decide and complete independently, including when
they share a home Herald. Both submission preflight and serialized application
enforce per-object exclusion. Failure completes immediately and owns no installation
collector or pending slot. Retirement preserves a committed success and removes
only retired members' impossible installation requirements. Pending decisions are
indexed by object, decision, participant and collector; disappearance consults the
same object index, while join sealing waits for every pending decision.

### 4.5 Predefined disappearance

| Command | Purpose |
| --- | --- |
| `OpenDisappearanceProbe` | Capture one allowed disappearance subject and the exact active Herald set. |
| `ReportPredefinedAbsence` | Record one Herald's stream-qualified negative evidence. |
| `InvalidateDisappearanceProbe` | Invalidate evidence when matching publication or alignment work appears, or when a controlled subject is relabelled. |
| `ResolveDisappearanceProbe` | Terminally delete a controlled subject or retire the captured regular-definition cut only when all required evidence remains valid. |
| `AbortDisappearanceProbe` | End a probe without a lifecycle change. |

The Oracle does not calculate local absence. It only orders and validates evidence
reported through the protocol in [03-protocols.md](03-protocols.md).

Production initiation is Herald-owned and event-driven. When `local-take` removes
an eligible disappearance subject—a controlled predefined object or regular
`SortDefinition` occurrence—from an application-visible Store, the local
`Herald.Disappearance` owner may retain that exact subject as a cleanup candidate
and submit `OpenDisappearanceProbe` through `Herald.OracleClient`. Bare local
emptiness, first observation of a structural object, a socket event, or elapsed
time creates no candidate and is never absence evidence. Once a candidate exists,
later relevant Store, publication, dependency, alignment, label, and membership
events re-evaluate only that subject. Any active Herald retaining the candidate may
offer Open; equivalent racing Opens alias one canonical probe.

At most one collecting probe exists for an exact
`(subject occurrence/revision, membership generation)`. Different subjects may
collect concurrently, so one legitimately retained remote copy cannot starve
unrelated cleanup. Oracle/Raft command admission remains serial at the leader.
The subject/generation index names only the active collecting probe; terminal
history is retained by probe ID. Equivalent Opens alias while that active entry
exists. Invalidation or authorized Abort removes it, but cannot itself cause an
immediate automatic reopen: a later meaningful eligibility transition is required.
Membership supersession is the explicit exception, preserving or reconstructing
the candidate for reoffer after the successor membership and structural base are
established. Only an initiating Herald needs a local candidate; every captured
Herald processes the projected Open, cuts its streams, and reports even if it never
performed the initiating `local-take`.

The first accepted Open derives the canonical probe ID from its control index.
Every later equivalent Open receipt explicitly maps that distinct request to the
original probe ID and classifies it as an alias. In projection, the existing outer
`AppliedOracleEntry` request ID paired with the event's canonical probe ID and
`Opened | Aliased` classification supplies the same mapping; the event does not
duplicate the request ID. A racing caller does not derive a second ID from the
later command's own index.

The subject/generation active index is only a secondary index. Every probe-ID-keyed
record permanently retains its full subject, Open control index, captured
membership generation/member digest/member set, accepted reports, and terminal
phase. Invalidation, Resolve, or Abort removes the active index but not that header
or evidence, so projection replay, stale rejection, and successor-candidate
reconstruction never depend on reverse lookup through a deleted active key.

Oracle log order also closes the controlled label/disappearance race. A
successful `DecideLabel` atomically invalidates any collecting disappearance probe
for that target and projects the coupled consequence. A disappearance Open is
rejected while an earlier matching successful label awaits installation
completion. If disappearance Resolve commits first, the later label decision is
immutable `NotApplied(TransitionNoLongerPermitted)`: it cannot resurrect the
terminal target and needs no installation collection. After label-caused
invalidation, delete consumes the retained cleanup candidate; a non-delete outcome
may re-arm it against the resulting label revision.

The applied-entry event vector pins compound ordering. A successful label event
`LabelDecided` precedes its disappearance invalidations in ascending probe-ID
order, after any admission-change prefix. A Herald-retirement entry retains its
membership/failure prefix, then predecessor disappearance Aborts in ascending
probe-ID order before its remaining resident-process and label-workflow
consequences. Thus the successor membership exists before survivors reconstruct
candidates.

Open, Report, Invalidate, and Resolve intentions for disappearance are derived by
Herald owners, not exposed through EAPP or EADM. `AbortDisappearanceProbe` is
available only to an explicit package-internal authorized deployment-management or
deterministic-test seam; no runtime failure policy synthesizes it. There is no
label Abort command or runtime ingress.

## 5. Command submission and retry

Every replicated client submission has this canonical envelope:

```text
OracleEnvelope = {
    oracleClientRequestId,
    expectedControlIndex?,
    homeHeraldEpoch,
    canonicalCommandPayload,
    progress? = (receiptRetirement, labelsConsumedThrough)
}
```

The client request ID remains stable. Before explicit retirement, exact retry
returns the original immutable command receipt. Reusing it with different
semantic command bytes is a state-machine protocol fault. Workflow callers query
the separate decision/probe status rather than expecting an `Open...` receipt to
mutate from pending to terminal.

The optional pair is progress metadata, excluded from the
semantic request digest. A retry may therefore carry newer progress without
conflicting with the original request. A fresh request and its advancing frontier
share one control entry and, when enabled, one post-state digest. A duplicate or
conflicting request can still advance retirement through a maintenance entry;
its ordinary outcome remains unchanged. Receipt progress must exclude the
submitted request and every unresolved request; its sparse exceptions retain
older unresolved identities. Label progress cannot exceed the latest canonical
label decision. Its distinct lifetime is defined below.

After projecting its results, the home may submit `RetireOracleProgress progress`
with its own epoch and the receipt high water as its routing request sequence,
without allocating a new ordinary request. The Oracle
replicates and canonically retains the frontier, then releases the covered receipt
records; its runtime releases the corresponding published-result copies. An
ordinary submit or query at or below the frontier returns `OracleRequestPrefixRetired`.
Committed Herald retirement releases that home's receipts and makes every later
request from the same epoch `OracleRequestHomeRetired`. Neither outcome admits
the request again or creates a rejection receipt. The Herald retains its local
semantic keys/results and control evidence for application and workflow retries.
Normal dispatch piggybacks the latest ready frontier. One supervised idle-flush
timer per binding coalesces standalone maintenance using the configured base
Oracle retry delay. Its owner input checks the latest confirmed frontier, so a successful
piggyback can make the flush unnecessary. The two components join independently:
receipt released-set union and maximum label index. Maintenance acknowledgements
carry full committed progress; `NotReady` echoes the full offered progress.
An older rejection cannot release physical suppression for a newer offer at the
same receipt high water. Reconnect preserves ready/confirmed progress and reoffers
the unconfirmed difference. Maintenance creates no ordinary receipt, and neither
its acknowledgement nor its control entry advances the label-decision high water.

A home's voluntary receipt frontier does not change Herald membership or certify
another Herald's control, structural or store progress. Committed semantic
retirement closes the target epoch regardless of whether that target has
acknowledged its own receipts. Any policy for retiring a lagging Herald must use
the separate membership/fencing contract; this receipt increment adds no lag
threshold or progress-based retirement trigger. Combined label progress reclaims
only completed Oracle witnesses as specified below. Neither component reclaims
Raft logs, control watches, local Herald evidence or epoch catalogues. Label-only
advances do not trigger receipt-map cleanup; receipt-only advances do not scan
completed label history.

`expectedControlIndex` prevents a home Herald from applying caller-derived changes
to a canonical state it knows has already changed. The leader may reject a stale
expectation before appending only when that rejection is explicitly defined as an
effect-free transport preflight. Any result presented as an Oracle semantic receipt
is itself a committed deterministic command result.

The prototype serializes semantic proposals at the leader: at most one uncommitted
Oracle command is admitted at a time. This is intentionally low throughput and
avoids validating a later command against a speculative prefix. It has no effect on
ordinary peer publications.

The pure Oracle generates no entropy. Herald/process/object/decision/probe IDs in a
command are supplied as checked external inputs or deterministically derived from
genesis plus already committed identities. Required uniqueness for decision/probe
identifiers is validated against Oracle state. Distinctness of a locally generated
`GlobalUniqueId` is a generator premise; neither bare IDs nor controlled first-
publication IDs are registered with the Oracle, so it has no collision/prior-use
admission branch for them. Their required stable private aliases
remain only in the owning process map at its resident Herald.

Every replica applies the exact canonical command bytes at the same control index.
Unequal receipts or applied-entry evidence for an equal committed prefix are
invariant faults, not conflicts to settle by another vote.

The whole-state `OracleStateDigest` is an optional diagnostic witness, disabled
by default. Its mode is selected at initialization and remains immutable for the
run; all replicas in an experimental deployment use the same mode. Enabled mode
hashes the complete canonical semantic state after each advancing transition and
retains the resulting digest in applied entries and checkpoints. Disabled mode
retains explicit absence and does not construct or hash that diagnostic
transcript. Command digests, workflow identities, prepared/outcome digests and
canonical command admission remain mandatory in either mode.

Digest absence is part of exact evidence equality, never a wildcard: absence,
presence and unequal present digests are distinct claims. A checkpoint retains
the mode through its optional witness. Admission still validates all nested
state and canonical re-encoding; a present digest must equal the independently
recomputed canonical state digest. Runtime installation rejects a checkpoint
whose mode differs from the receiving Oracle's initialization mode. Enabling
this witness requires a fresh experimental run; it is not a live mode change or
a replica agreement protocol. Optional hashing does not remove checkpoint
serialization or alter semantic state retention.

## 6. Direct controlled-publication workflow

### 6.1 First publication

Once generic controlled `PublishValue` is executable, the first `write` using a
controlled reservation proceeds as follows:

1. The home Herald resolves the process-private nabla/object IDs, recursively
   globalizes every unique-ID field in the value, and validates the pristine
   reservation, capability, initial label, request identity, effective controlled
   descriptor/policy, and source nabla authority. For a predefined edge this also
   rejects every representation other than the closed endpoints-plus-strength
   schema.
2. It derives one stable `PublicationId`, freezes the route, allocates the
   `HeraldPublicationPosition`, and prepares publisher-retained history and request
   state. Ordinary data freezes the complete local/remote destination cut and
   prepares local store and peer-batch changes; a structural carrier unions that
   ordinary frozen cut with one mandatory private-system-view destination at every
   active-generation Herald. A structural carrier
   whose referenced structural/data/control prerequisites are not all locally
   applied instead prepares an unsequenced structural-admission stage with no
   stamped peer item.
3. One serialized transition consumes the reservation and establishes the local
   controlled ID-to-sort binding and monotone lifecycle state. Ordinary data, or a
   ready structural carrier, also commits its applicable local effects and peer
   outbox work. An unready structural carrier retains no stamped occurrence or
   peer item; when it becomes ready, one later transition allocates its structural
   sequence, stamps the current applied predecessor vector, reconciles finite local
   structural effects, advances the vector, and inserts stamped peer items.
4. An ordinary-data application receives the locally accepted result. A structural
   publication instead retains its result until its occurrence is covered by
   complete active-generation `TopologyCutAccepted` evidence and the resulting
   `TopologyCutEstablished` is installed at the home Herald. This mandatory wait is
   peer evidence, not an Oracle decision, and it never waits recursively for context
   transfer completion.

The initial persistent label is `(void, 0)` or `(caller, 0)` as specified by the
application semantics; controlled first-use admission rejects a nonzero supplied
generation. A first label operation later establishes the Oracle's
canonical label overlay after its captured-generation fence; until then, the checked label in
the data-plane history is authoritative locally. An Oracle label release always
overrides an older embedded label on a delayed value.

Exact RPC retry while the logical session lives reuses the retained publication
and current request status; session loss removes only the latter, while semantic
stabilization continues. A different first
`PublishValue` or `DeleteReserved` observes the consumed local branch and rejects.
Profile 0.1 has no generated-identity prior-use fault or recovery protocol:
distinct generated outputs are a premise, and application DTOs cannot supply a
global identity directly.

Publishing the regular `sort-sort` definition is ordinary direct data under the
same no-Oracle rule. Every Herald locally validates controlled descriptors and the
edge carrier's closed `preserve :| [weaken]` strength enum. The Oracle receives
neither descriptor/value bytes nor edge endpoints, strength, or a sort filter.
The separate stable numeric strength codes used by normalized Herald evidence
transcripts never become Oracle commands or application/peer value alternatives.

### 6.2 Current update

A current-to-current controlled update is accepted against the home Herald's local
controlled history and released-label/process projection. It creates no Oracle
entry. The publication's prerequisites name only facts genuinely needed to validate
the publisher: the source nabla's structural occurrence/authority, any released
label or process transition, an applicable sequencing fence, and a
regular-definition retirement boundary. They do not name an Oracle decision that
created or kept the target object current.

This preserves the paper's asynchronous race: an update accepted while the local
label is void may arrive after the object's later label release. On receipt, its
embedded label is rewritten to the latest canonical label. A locally retained
obsolete or label-deleted suppression filters the stale current update. A label
sequencing fence supplies stronger ordering when the target is the designated
sequencer.

### 6.3 Obsolescence

A write whose checked value is obsolete is admitted and published directly after
the same local authority/capability checks. Applying it advances the receiving
Herald's controlled object to a monotone obsolete state and retains enough hidden
suppression that no current duplicate, update, snapshot, or reconnect delivery can
make it current again. An equal later obsolete observation is idempotent. If a
Herald already knows a label-deleted outcome, the obsolete item is terminally
suppressed.

The right to issue another current publication disappears at the publisher through
this same local transition. A different process cannot manufacture that right:
operator handoff is a label workflow whose fence separates the old accepted prefix
from the new authority. Temporary remote disagreement is expected until direct
publication/alignment converges.

Profile 0.1 retains obsolete suppression for the complete finite run. Section 8
specifies the shape of a later evidence-based reclamation workflow; the Oracle's
eventual role is to order permission after every Herald in the captured membership
generation has recorded/drained the obsolescence, never to approve the obsolete
write.

### 6.4 Forward

`forward` adds no Oracle entry. It carries the source-authority/label prerequisites
of the retained controlled object. The receiver validates those source facts,
applies its newest released-label overlay, and merges the value with local monotone
obsolete/deleted suppression.

Forwarding a structural carrier creates a new direct structural publication rather
than merely replaying the old occurrence. It receives a fresh publication identity
and Herald publication position, follows the same unsequenced-stage and stamped
version-vector path as structural `write`, and remains
`StructuralStabilizationPending` until a covering established cut is installed at
home. Protocol repair of the old occurrence, by contrast, reuses its exact stamp.

## 7. Label state machine

The canonical label is `(LabelOwner, Word64)`. Its object-scoped generation counts
successful operations; `LabelRevision` is the successful decision's
`ControlIndex`, while `AuthorityEpoch` describes operator tenure. CAS compares the
full label pair. Each success advances generation exactly once, including
same-owner success, which retains authority. Rejection and `NotApplied` preserve
generation. Exact replay installs the recorded successor without incrementing
again. End and retirement project process to zombie without changing generation.
Label deletion records `g + 1`; automatic disappearance records `g`. Neither
terminal deletion can be reversed.

### 7.1 Home acceptance and selected settlement

The home checks the live session, private handles, normal possession, descriptor
label support, permitted role and policy, and target nameability. The expected
pair is the caller's CAS operand. A lagging local label or lifecycle projection
cannot decide that comparison or reject a process target as canonically ended.

A label remains pre-acceptance while an already accepted publication in its exact
sequencing scope has no structural-admission position, or while the membership
successor lacks its established structural base. It owns only the exact typed
request for retry/conflict classification: no process position, decision ID,
fence, or Oracle intention. Later provider work can unblock it. Session or process
End discards this unaccepted candidate.

Acceptance captures one process-scoped cut and retains a complete workflow before
returning `LabelSettlementPending`. The selected preceding publications are those
accepted by this caller that publish or forward the labelled object or use a
labelled nabla as sequencing authority. They settle at their frozen destinations
before the home submits `DecideLabel`. Qualifying later publication remains held
until the decision is locally installed. Independent application operations,
Store observations, peer work, and Oracle traffic continue; no all-application
label gate or Ready phase exists.

The exact decision command contains the fresh decision ID, caller, object,
expected pair, optional initial evidence, optional initial authority and
justification, requested target and home acceptance cut. It carries no expected
released revision, private handle, or force bit. Accepted workflow ownership is
separate from reply reachability: caller/session End does not discard the exact
command, selected settlement, installation work, or completion collection.

### 7.2 Canonical comparison and immutable outcome

`DecideLabel` performs the comparison and canonical state change in one Oracle
transition. If another success for the same object still awaits installation,
submission is deferred, both at preflight and at serialized application. The
response identifies the blocking decision and observed control index; it creates
no command receipt, control entry, or semantic effect. The client keeps the exact
request identity and bytes for retry after relevant projection progress. End and
unrelated Oracle commands and labels of other objects remain independently orderable.

For the first overlay, checked `InitialLabelEvidence` binds the object, raw
observed generation-zero label, and either a bootstrap root or admitted
publication provenance. Expected cannot authenticate itself as missing state.
Nonzero observed generation cannot establish initial evidence. Oracle canonical
End normalizes raw `(process(P), 0)` to `(zombie(P), 0)` before comparison; a raw
zombie assertion requires matching canonical End. This evidence contains no
publication payload, descriptor, topology, or payload hash. It records the home's
checked observation in the benign deployment, not Oracle reconstruction of the
data plane. An existing `LabelRecord`, including terminal deletion, takes
precedence over supplied initial evidence.

Initial authority evidence is independent of the raw label evidence. A first
same-owner nabla overlay retains its exact genesis or established structural
tenure; source-publication authority is not target authority. Existing canonical
records supply their current authority and revision directly, without additional
CAS checks against a stale local projection. Missing or malformed first-overlay
provenance cannot invent authority.

Terminal-object admission takes precedence for already deleted state. For an
admitted nonterminal prior, the committed decision checks:

1. caller process liveness;
2. requested process-target liveness;
3. the expected full pair against canonical effective label; and
4. the permitted canonical transition.

The first failed test selects one immutable reason:

| Tag | Internal reason | Payload |
| ---: | --- | --- |
| 0 | `CallerProcessEnded` | Exact process epoch |
| 1 | `TargetProcessEnded` | Exact process epoch |
| 2 | `PriorLabelChanged` | Empty |
| 5 | `TransitionNoLongerPermitted` | Empty |

The old reason tags are not compatibility alternatives. Malformed command facts
remain typed command rejections; an admissible failed comparison produces
`LabelDecided(NotApplied)` without changing the label. Failure immediately enters
completed history, frees no collector because it never created one, and can
return to the home application after local projection. It requires no peer
installation report or `CompleteLabelDecision`.

Success installs the canonical successor and emits `LabelDecided(Applied)` at the
same index. That index is the operation's linearization point, canonical revision,
and new `LabelAuthorityEpoch` only when assigning a different live effective
operator. Same-owner success retains tenure. Void retains prior tenure as
inapplicable history; deletion ends applicability and retains only terminal
history. The outcome is immutable immediately: later End, retirement, a lost
reply, or loss of the originating application cannot turn it into failure.

The retained decision binds common facts, configuration/catalogue commitments,
the captured membership generation ID and fixed member-set digest. It does not
copy the member roster; the checked membership history supplies that roster.
There are no Open, Ready, Resolve, Prepared or Release wire phases, no per-member
Oracle readiness/preparation maps, and no label Abort command.

### 7.3 Current-state installation and direct collection

Every captured Herald applies a successful decision atomically against its
current checked owner state. It derives target-specific effects at that moment,
allocates any Store identities once, preserves unrelated progress, and retains
historical installation evidence. No precomputed whole-substate copy or executable
prepared recipe survives across Oracle entries. The internal name
`PreparedLabelFacts` denotes the normalized immutable installation facts; it does
not introduce a protocol phase or a report command.

A Herald without the target publication installs the canonical overlay or
terminal suppression without fabricating payload or possession. A later arriving
publication is projected through it. Present target provenance must agree with
the checked canonical authority facts. Missing local endpoint sort knowledge does
not delay the decision: an owned nabla cannot sequence without its admitted sort,
and an owned delta remains passive until the sort is known. Selected post-cut
work is released and revalidated against the installed outcome. A delete applies
all affected graph, routing, context and alignment consequences locally, without
requiring equality between legitimate Herald-local graphs or Stores.

Participants send `PeerLabelInstalled` directly to the assigned collector. Its
historical fact binds the decision, reporting epoch, decision index and immutable
outcome digest. Future-index reports await local projection; completed reports do
not recreate work. Peer disconnection retains the fact for direct replay. An
Oracle connection is no alternative reporting route. Later End or structural
change can supersede effective state without invalidating the historical fact;
transport acknowledgements do not release it.

`LabelOutcomeDigest` is SHA-256 over the typed canonical `cereal` transcript:

```text
TerminalOutcomeTranscript(
    "ECLIPS-LABEL-TERMINAL-OUTCOME",
    variantTag,
    canonicalVariantPayload)
```

Tag `0` (`NotApplied`) serializes decision ID, terminal control index, object ID,
fixed reason tag and payload, catalogue digest and member-set digest. Tag `1`
(`Applied`, named `Released` internally) serializes complete canonical installation
facts, their `PreparedLabelDigest`, decision control index, canonical released
state, `LabelRevision` control-index word and optional retained authority. A
released label is tag `0` plus canonical label bytes; terminal deletion is tag `1`
plus its successor generation word. Authority uses fixed genesis/structural/label
tag order; label authority is tag `2` followed by its decision control index.
Revision and decision index are both encoded despite being equal. No untyped
caller-supplied byte string can substitute for this transcript.

Successful application completion waits until all required captured survivors
installed the outcome and one `CompleteLabelDecision` commits. The attestation
contains decision ID, original decision control index, outcome digest, collector
epoch and current membership-generation ID: four 32-byte identifiers and one
8-byte index. Its command tag is `8`; there is no
per-participant terminal Oracle event or map. Completion emits
`LabelWorkflowCompleted`, reclaims installation evidence and frees that object's
slot. The normal successful path is two semantic Oracle commands; failure is one.
Progress maintenance is coalesced separately.

The active decision home is collector. Retirement reassigns to the least active
captured epoch and survivors resend their retained facts to it. A retired member's
impossible requirement is removed without changing success. With no captured
survivor, retirement itself emits `LabelWorkflowCompleted` without a completion
attestation. Later joins do not enlarge the captured
set. A fresh completion checks the current membership generation and assigned
collector, not a whole-control-index freshness precondition. Uncertain submission
reuses identical identity and bytes; a definite stale-generation rejection permits
one refreshed request under normal receipt retirement. Semantic replay by
successful decision/outcome rediscovers existing completion for a surviving
captured sender even after assignment or generation changes, while the
completion witness remains within its supported lifetime.

The Oracle retains a compact completion witness keyed by decision ID: original
decision control index, captured membership-generation ID and immutable outcome digest. Checked membership history
recovers the original participant set. A retry still checks the submitting home,
captured reporter and digest; it does not need the full decision or terminal
installation recipe. Applied entries and pending installation retain their own
evidence. Failed comparisons retain the same compact witness for rediscovery.

Each completed entry occupies 104 bytes in both canonical state and checkpoints,
including its key. Checkpoint admission checks canonical framing, fixed-width
identifiers, the referenced checked membership generation and disjoint pending
and completed decision sets. It no longer reconstructs an old terminal transcript
to rederive that witness's digest. The optional whole-state digest still checks
the complete checkpoint state when enabled. Private movable storage for completed
keys and digests avoids retaining their small pinned SHA allocation blocks.

Each active Herald announces the original label-decision index through which it
has no remaining completion work. The local promise follows installation,
application/disappearance consumption and every prepared completion request;
a former collector's unprojected request still pins its decision after another
collector completes it. An ordered pin index finds the earliest outstanding
decision without scanning historical requests. `NotApplied` releases its local
consumer after application settlement. Progress never advances between projection
and those consumers within a composed Herald transition.

The Oracle retains a promise for every active epoch and reclaims completed
witnesses through their minimum using an index by original decision position.
Pending workflows, current labels and exact request receipts have independent
lifetimes. Canonical retirement removes a member's promise; disconnect does not.
Activation initializes a new member at the latest label index, which its checked
observer replay consumes before activation. The irreversible global floor and
latest decision watermark survive checkpointing even when no completed witness
remains. Checkpoint admission requires exact active-member coverage and
`retiredThrough = min(active promises) <= latestLabelDecision <= controlIndex`.
Every member's promise lies between the floor and latest decision. Completed
witnesses lie strictly above the floor and no later than that latest decision;
original indices are distinct across pending and completed decisions. Admission
rebuilds the derived promise-count and completed-by-index maps.

A known decision must match the attested original index before completion checks.
For a fresh request naming an absent decision at or below the global floor,
`LabelCompletionObsolete` is an ordinary rejection with no completion event.
An absent decision above it remains unknown. Existing exact-request receipts and
request-retired classifications run first. This prefix policy may retain later
witnesses behind one old pin; it does not bound arbitrary history behind a
permanently lagging active member. The broader watch/Herald/authority retirement
contract remains in the [control-history design](../architecture/control-history.md).

Canonical commands, events, checkpoints, codecs and fixtures cut over together in
a fresh experimental deployment; no version, old-phase adapter or compatibility
decoder is retained.

## 8. Predefined disappearance

The disappearance state machine exists because empty local stores are not global
negative knowledge. Its subject is either a controlled predefined structural
object or one regular content-addressed `SortDefinition`; the two resolutions have
different semantics.

1. `OpenDisappearanceProbe` validates the subject. For
   `ControlledPredefined`, it checks the subject shape, any existing released
   label/deleted record, and captures the expected structural occurrence and label
   revision. The Herald reports, rather than Oracle state, establish whether that
   occurrence is locally present or retained. For
   `RegularSortDefinition`, it verifies the `SortId`/descriptor digest shape and the
   occurrence derived from that sort's last resolved retirement index, but creates
   no label or controlled-object record and claims no regular-object lifecycle
   authority. Both paths capture every active Herald epoch.
2. Peer markers establish the exact inbound publication cut and an
   `AlignmentProbeMarker` revision cut on every live incoming alignment subscription
   at each captured Herald. A subscription whose source is the same Herald still
   contributes its exact checked revision marker; its single owner assigns,
   receives and completes that marker locally without TCP.
3. `ReportPredefinedAbsence` is admitted only for the exact probe, member-set
   digest, Herald epoch, publication-stream cut, complete alignment-subscription
   watermark set, and subject. A controlled report additionally names the captured
   structural occurrence and label revision; a regular report names the exact
   descriptor digest, occurrence ID, dependency-absence attestation, and cut. Every
   regular report also attests that the reporting Herald has no locally known
   current/obsolete controlled object using that sort; the Oracle derives the global
   negative fact only from the complete captured report set.
4. Matching new publication, held visibility/dependency work, relabel of a
   controlled subject, a new semantic dependant of a regular subject, or same-sort
   alignment causes
   `InvalidateDisappearanceProbe`. Creation, replacement, or cancellation of a
   captured live incoming subscription also invalidates. Conservative whole-sort
   invalidation is permitted when a snapshot's object contents are not known yet.
5. Resolution branches by subject:
   - `ControlledPredefined` orders one cut-qualified disappearance/deleted outcome;
     it does not rewrite an Oracle-owned current/obsolete record. Heralds
     purge visible and publisher-retained payload/capability from application and
     private system-view stores, retain payload-free terminal suppression, and
     reconcile the induced graph. If the subject is an edge, that reconciliation
     covers every affected effective sort; the Oracle orders only the common
     evidence result and does not evaluate graph reachability.
   - `RegularSortDefinition` revalidates that the complete still-current Herald
     evidence set reports no current/obsolete controlled use, then orders retirement
     of only the occurrence and publication/alignment cut named by the reports. It
     does not mutate controlled-object history or create a terminal `SortId`
     lifecycle.
     Heralds purge matching captured visible and retained definition copies, remove
     the effective registry entry, retain cut-qualified stale-publication
     suppression, and reconcile dependent definitions. Decode-only descriptor
     bytes may remain cached. The resolve index becomes the occurrence base for a
     later equal redefinition, preventing old alignment generations from matching.

On applying an open regular-definition probe, each Herald gates matching new
`sort-sort` writes after its captured local publication position. Work accepted at
or before that position must be inserted before its probe marker or terminally
rejected. A matching gated write first invalidates the probe; if resolution wins
instead, the write is released afterward as a new occurrence carrying the resolve
index. It may therefore re-establish the same content-addressed definition without
allowing a stale pre-cut duplicate to defeat retirement.

For a controlled-subject probe, publisher-retained capability for that subject is
not a delta-store replica and does not by itself block an absence report; successful
resolution purges it. For a regular-definition probe, however, every current or
obsolete locally retained controlled object **using** the defined sort and every
Herald-reported semantic dependant blocks retirement. A publisher may race the
probe by republishing or relabelling. A matching accepted data-plane publication
invalidates the captured evidence or remains held beyond the cut; a label command is
ordered normally. Republish is never converted into an Oracle command.

A universal edge is not a semantic dependant of a regular sort definition merely
because that sort can traverse it; the edge contains no sort reference. Typed
nabla/delta or carrier definitions and retained values continue to count normally.

Herald retirement aborts an open disappearance probe rather than reinterpreting
its evidence set against a smaller generation. A replacement probe may open after
the successor membership and structural union are established. Disconnect or
timeout is never predefined-absence evidence.

### 8.1 Deferred obsolete-suppression reclamation

The same evidence pattern is the intended later mechanism for discarding hidden
obsolete-object state. It is intentionally not a precondition for publishing the
obsolete value:

1. each Herald in the captured membership generation first records the exact
   obsolescence occurrence and reports a
   cut showing that all earlier current publications, held work, and alignment
   prefixes for that object have been applied or terminally suppressed;
2. the reports name the exact object/sort binding and member-set digest, so a later
   observation or changed cut invalidates old evidence;
3. the runtime's bounded-latency contract supplies a conservative horizon derived
   from the declared delivery bound `T` and the declared clock-synchronization bound
   `D`; traffic exceeding the bound is treated by that contract as not deliverable,
   not silently admitted after the horizon; and
4. only after every report and the horizon may an Oracle command order common
   reclamation permission, after which each Herald may discard the named hidden
   obsolete suppression and payload history.

The intended later bounded-latency layer can use standard Ethernet despite
occasional overruns: captured work carries an origin time or expiry in a physical
clock domain synchronized within `D`, and a receiver admits it only while the
worst-skew calculation proves that its conservative delivery deadline has not
passed. Anything later is permanently discarded, turning an overrun into the “not
at all” branch of the `within T or not at all` guarantee. The later profile must
specify the exact equation, scheduling/resolution slack, retransmission expiry, and
wire fields; profile 0.1 adds none of them speculatively.

Raft commitment alone proves none of steps 1--3. Until this workflow, its clock
contract, and its properties are implemented, profile 0.1 retains the state for the
entire finite run. The Oracle's eventual resolution certifies completed observation
and a safe deletion cut; it never decides that the object became obsolete.

## 9. Raft replication profile

### 9.1 Membership

A deployment configures an initial nonempty set of opaque `RaftNodeId` voters,
each co-hosted with a selected Herald process. The same kernel supports one, two, three, and
larger nonempty sets through the same checked admission and quorum predicate;
strict ascending order, unique co-host bindings, and exact configuration agreement
remain required. Checked Raft genesis owns the immutable initial native voter set
and timer configuration. It consumes only local
node ID, initial voter IDs, and native configuration; it imports no `SystemId`,
`HeraldId`, `HeraldEpoch`, Oracle type, or hash dependency. Oracle genesis retains
the exact `(RaftNodeId, HeraldEpoch)` binding and Oracle-owned digest above, and
composition requires its voter keys/native configuration to equal checked Raft
genesis before startup.

- Not every Herald is a voter.
- Initial Herald membership and initial Raft membership are separate manifest
  fields; equality is not required. Non-voting Herald retirement does not change
  the Raft voter set.
- Stopping a co-hosted Herald role does not silently reconfigure its voter role.
- Native learners and joint consensus use replica registration
  and voter-change intentions in Oracle state; subsequent voter
  bindings derive only from committed native configuration entries. A lone survivor of a
  three-voter configuration cannot elect itself, commit a command, or shrink its
  configuration.

All active Heralds act as Oracle clients. Their control-watch projection selects
the union of old and new voters during a joint configuration and the new set after
final commitment. Followers return leader hints; clients tolerate stale or absent
hints and retain discovered or historical contacts as reconnect candidates. A
contact is a dialing hint and never grants voting authority.

An applicant discovers the immutable bootstrap and contacts through the restricted
peer-listener lane. An old member submits its admission to the ordinary Oracle
owner. Before activation the applicant imports canonical control as a joining
observer and cannot establish an ordinary Oracle client binding. Oracle runtime
Hello admission reads the serialized owner's current activated Herald catalogue;
the immutable initial roster is not a permanent whitelist. Activating a Herald
does not change the native voter set or create an Oracle replica. A subsequent
explicit preparation registers a fresh node on that active epoch, including an
epoch admitted after genesis, before starting its local learner runtime. That
runtime replays the complete retained log from an empty log under the same
immutable genesis. Live demotion retains its learner role and ordinary Herald
services; failure of the Oracle role alone does not retire or stop the Herald.

A fresh applicant exposes no application state before activation. Its joining
state must supply one coherent activation base, subsequent changes and the
outstanding obligations it takes over. It need not recreate states predating
that base solely because a donor once served them. Internal staging may be
replaced while the applicant remains an observer, provided the replacement
preserves the exact current admission/attempt binding and any readiness promise.
An invalidated attempt's readiness cannot authorize activation of its replacement.
Existing members remain responsible for their own outstanding historical work.

The deployment supplies concrete Oracle/Raft endpoints before accepting local
preparation. An owner-published registration notification starts the role only
after the binding commits; ordinary non-voting Heralds require no periodic owner
status polling. Restricted discovery can resolve a canonical registration for a
lagging Raft transport peer, but cannot apply that registration to Oracle state
or alter a native voter set. Local replay checks the provisional binding against
the committed registration. A lost-log restart requires a fresh Herald epoch and
native node identity; the same failed role is not restarted within this run.

### 9.2 Required Raft behavior

The pure Raft core implements at least:

- follower, candidate, and leader roles;
- monotonically increasing terms and at most one vote per term;
- randomized election timeouts supplied as explicit runtime inputs;
- last-log checks for votes;
- `AppendEntries` prefix matching and conflict repair;
- stable majority and joint dual-majority replication;
- advancement of commit only through an entry from the leader's current term;
- a current-term no-op before a new leader serves Oracle commands;
- idempotent handling of duplicate and delayed RPCs; and
- ordered exposure of every committed entry, including no-ops, opaque
  application bytes and native configuration entries with opaque adapter metadata.

Election and commit use the latest configuration in the local log. A stable
configuration requires a strict majority of its distinct voters; a joint
configuration requires a separate strict majority of both old and new sets. The
local self-vote is checked immediately, and every local leader append is checked
for commitment before any remote response. A singleton therefore elects and
commits its current-term no-op without incoming peer RPC; ordinary application
entries subsequently commit through the same log path. Two voters require both
votes. Remote or repeated identities outside the checked voter set never count.

The Oracle adapter applies committed entries in physical log order and
acknowledges every native log position before the leader becomes application-ready.
No-op log indices do not consume Oracle control indices. Unretired exact
application retries retain the recorded receipt. They add control history only
if newer piggybacked retirement requires a standalone maintenance entry.
Each committed native configuration consumes one control index and supplies a
tagged configuration observation without a client request identity or receipt.

Raft transports one canonical Oracle envelope whose encoded fields include the
stable client request ID and closed-vocabulary command. It neither decodes nor
prevalidates ECLIPS meaning.

#### Native configuration and learner boundaries

`Eclips.Raft.Configuration` owns opaque checked voter sets, stable/joint
configurations and exact genesis or `(log index, term)` configuration references.
The effective configuration is the latest logged configuration; the committed
configuration is the latest one in the committed prefix. Conflict truncation
restores the preceding effective configuration. Native identities and adapter
metadata carry no Herald, System or Oracle meaning inside Raft.

A learner starts from the same immutable initial configuration and an empty log.
It receives the full retained log through ordinary prefix matching and applies
every committed position, including historical configurations. A replication
target outside the effective voter sets does not campaign, vote or contribute
to commitment. Preparation captures a settled leader log/applied frontier and
gates new proposals. Promoted learners must independently attest to that exact
retained and applied prefix. The receiving owner reports its acknowledged applied
index in successful AppendEntries responses. The leader checks the exact matched
attempt and current term before sealing readiness for its captured frontier.
Leadership change invalidates those observations.

Append `Joint old new`, commit it with both majorities, then append `Stable new`
and commit it with the new majority. Each entry takes effect at append, including
on its leader. An inherited unfinished suffix is replicated under its effective
configuration; a new leader first establishes its current-term no-op and ordered
adapter progress. Only one configuration change can be unfinished. Final adapter
metadata is supplied explicitly after joint commitment; Raft never synthesizes
or decodes it.

A leader excluded by its uncommitted final entry may finish replication and
commitment without counting itself. A previous voter in that position may also
campaign without a self-vote. Committed exclusion ends its election eligibility;
a removed leader exposes the committed prefix and steps down. Receiver-side
voter membership never serves as a transport whitelist for candidates or leaders
because the receiver may lag their configuration.

Recent valid leader contact arms a generation-qualified guard for the minimum
election timeout. While that guard holds, RequestVote does not advance the term
or receive a vote. The runtime supplies explicit expiration. A leader renews
only with fresh quorum acknowledgements; local heartbeat sends are insufficient
when remote acknowledgements are required. A singleton's explicit local
acknowledgement suffices for its singleton quorum. After protection expires, a
first partial voter acknowledgement starts a collection window of one minimum
election timeout without enabling protection. Expiry discards that partial
cohort; further insufficient replies cannot extend its window. Normal applicable
append and response traffic retains higher-term handling.

The Oracle coordinator prepares native metadata from the retained accepted voter
intention, with exact `(changeId, Joint | Final)` identity. Only the sealed native
committed-entry stream can apply a configuration observation. Its metadata must
match the retained intention and native configuration; a committed contradiction
is a composition invariant fault. Replaying commands, configuration observations,
and no-ops reconstructs the same Oracle state, immutable receipts and control
watch at each corresponding applied prefix.

Preparation briefly gates ordinary proposals after preceding work settles. A
current-term leader lane remains available for progress and cancellation, and a
Herald's retained cancellation can bypass an ordinary request waiting for that
gate. Explicit cancellation is an ordered command and is allowed only before the joint
entry survives in the log. An inherited joint/final suffix must settle before a
new leader decides whether cancellation is possible. Lost required quorum leaves
the operation unfinished; neither a timeout nor a smaller reachable set is a
successful voter change. An accepted voter-host failure is never cancellable; it
retains its original certificate and exclusion/retirement slot.

The original explicit Begin receipt records accepted intention in `VoterChangePreparing`
and never changes. Owner status advances separately through
`VoterChangeJointCommitted` to `VoterChangeCompleted`, or to
`VoterChangeCancelled` before joint consensus begins. Committed final observation
and `Completed` share one control entry; there is no additional final-completion
command for explicit changes. Automatic failure exclusion instead records
`VoterExcludedAwaitingHeraldRetirement` at final commitment and `Completed` in its
later semantic retirement entry. Operator output distinguishes this immutable receipt from the current
workflow phase, and rejects an empty intended voter set before preparation.

#### Explicit founder bootstrap

`eclips-hello-world --bootstrap` explicitly starts one fresh system with one Herald,
one Raft voter, and one genesis launcher/root bundle. Runtime entropy supplies
fresh run and epoch identities for pure checked configuration admission. The
Herald's existing runtime source separately supplies its generator seed before
pure kernel initialization. The founder uses the ordinary Oracle runtime, Raft log,
ordered adapter, and Oracle watch; it exercises all eight application operations
and a label barrier capturing the sole Herald. Each invocation creates a fresh
finite run. This example entrypoint always creates its own system. The deployment
executable's `eclips-herald --peer` mode joins an existing system through the
restricted admission workflow without creating another genesis.

#### Fresh native health and semantic lifetime

Every health reply comes from a new request serialized by the native Raft owner.
It names that owner, its current term, the exact log-effective configuration
reference and voter sets, and a local generation that changes on configuration
append or truncation. Heartbeats that leave those facts unchanged do not advance
the generation. Stopped owners return no cached observation. The one-shot EORC
lane does not wait for leader or ordinary command readiness, but checks that its
source identity remains an active semantic Herald in Oracle-owner-published
membership. A retired source cannot renew isolation grace after missing its final
watch entry; explicit voter demotion preserves health admission.

Heralds assess one owner-issued round at a time. Only fresh matching observations
count toward the effective stable majority or both joint majorities. The latest
committed configuration is a floor; a learned uncommitted joint cannot be relaxed
by an older same-term stable reply. A newer term may supply fresh evidence of a
native truncation above that floor. Neither health nor a local timer changes the
Oracle configuration or grants failure-report authority.

A semantic self-fence ends application authority without stopping a cohosted Raft
owner. Its application drain is bounded, while the control/status shell and native
role remain separately supervised. Final automatic exclusion removes the native
node from replication targets; explicit demotion retains it as a learner. Native
transport retires excluded lanes and stops retrying them. Failed connects and
rejected handshakes share progressive pacing, so a still-running removed node
that missed final commitment cannot spin on refused connections.

### 9.3 Install-before-effect prototype contract

One serialized Raft runner exclusively owns the pure `RaftState` and is the only
caller of `stepRaft`. For every input it adopts the pure successor and effect batch
in its private owner loop, and only then hands
the whole batch to the dispatcher. It processes no later Raft input until that
handoff completes. Dispatcher scheduling can therefore delay output but cannot lose
a message or committed-entry notification or expose one before its in-memory
predecessor transition has been installed.

The Oracle application adapter likewise exposes a receipt or domain effect only
after the corresponding committed entry has been applied in order. A voter process
that fails remains stopped for that run; a surviving majority may continue. There
is no `StableStore`, prepare/persist/confirm phase, stable receipt, conditional
mutation digest, ambiguous-write retry, or restart claim in profile 0.1.

A future durable/restarting profile must redesign this boundary so term, vote, and
log mutation become stable before their derived messages or committed application
effects are released. Filesystem persistence, durable snapshots, restart recovery, and the required
failure semantics belong to that later design and MUST NOT be simulated by copying
another replica's internal state.

### 9.4 Two-stage application

For each committed Raft entry carrying an Oracle command:

1. Raft commits and exposes a sealed entry with its log index, term and opaque
   payload in contiguous log order.
2. The typed adapter decodes the canonical envelope and passes every syntactically
   valid committed envelope, including retries, to `stepOracle` in log order.
   `Oracle.Semantics` alone classifies the committed request against home closure,
   receipt frontiers and `requestResults`. A first-seen unretired ordinary request
   advances `ControlIndex` and derives its deterministic receipt/effects, including
   any piggybacked retirement in that same entry. An exact duplicate returns the
   retained receipt; conflicting reuse returns its deterministic protocol
   disposition. Either can advance a receipt frontier through one standalone
   maintenance entry. Obsolete requests create neither receipt nor control entry.
   The adapter mirrors the resulting published receipt availability and retains
   emitted watch entries; it does not choose the semantic classification.
3. After that deterministic classification, the adapter advances its separate
   contiguous applied-`RaftLogIndex` cursor and acknowledges the log position to
   Raft even for an exact duplicate or conflict. The existing retained-receipt
   fast path during native configuration preparation can return an already
   published receipt or conflict without a new proposal; it does not commit newer
   piggyback metadata. Unconfirmed retirement remains eligible for retry.

Committed no-ops traverse the same adapter cursor and acknowledgement boundary
without changing Oracle state or consuming a ControlIndex. Thus native applied
progress names an exact prefix even when it contains no application command.

For a native configuration entry, the adapter instead validates and applies its
checked observation against the retained voter intention. It advances one control
index, updates the committed configuration and change phase, and appends one
configuration-origin watch entry. It then acknowledges the same ordered native
cursor. It creates no command envelope, request result, or command receipt.

A crash between these stages in a durable future implementation is repaired by
re-exposing the committed Raft prefix. Stable Oracle client request IDs make
application idempotent. In the prototype both stages are in memory but retain the same
interface.

### 9.5 In-memory checkpoints and native log reclamation

The application owner may capture its actual state after acknowledging an exact
applied Raft position. `checkpointRaftApplication` carries that position and
opaque checkpoint bytes; Raft derives its term, configuration reference and
stable/joint voter sets from the native log. It replaces the covered prefix with
one base position while retaining the suffix. A checkpoint is application state,
not the old command stream or an observation of a peer's progress. No elapsed-time
or log-size threshold changes whether an entry is safe to reclaim.

A peer whose next required entry predates the base receives InstallSnapshot with
the checkpoint's index, term, configuration and application bytes. The receiving
native owner preserves a suffix only when its entry at that index has the same
term, installs the new native base, and marks application installation pending.
Its sealed effect batch requires the application adapter to install the actual
checkpoint and acknowledge the new applied position before the successful
snapshot response may leave the runtime. Until then native service readiness
remains false. Ordinary suffix application follows that installation in the
same dispatcher order.

Duplicate or older snapshots cannot roll back a committed/applied prefix; a newer
snapshot arriving during installation waits for a subsequent replication attempt.
Stale AppendEntries below the base cannot recreate retired history. Configuration
references and both joint-consensus voter sets survive the base change, so log
reclamation changes neither election freshness nor the quorum required for
commitment. This in-memory protocol supports continued operation and lagging-peer
catch-up; durable owner restart remains outside the prototype.

## 10. Oracle safety invariants

At every applied control prefix:

1. one process epoch has at most one residence and dead epochs never become live;
2. Herald membership begins at genesis and advances through one checked history
   of admissions and retirements; a retired or cancelled epoch never becomes
   active, a current voter-host epoch cannot be retired before certified native
   exclusion, and each successor names its
   exact predecessor and strictly later control coordinate. Retirement binds its
   unique resolution; admission binds its sealed base recipe and activation;
3. at most one failure probe is active for a target/membership/voter-configuration tuple; it
   binds one eligible reporter set and at most one equal result per reporter, only
   a strict reporter majority authorizes its unique terminal result, retirement
   supersedes every other predecessor-generation probe, and terminal probes never
   reopen;
4. one exact Oracle client request has at most one semantic command digest and
   receipt; retirement frontiers never decrease, and covered or closed-home
   requests cannot create a replacement receipt;
5. at most one successful label decision per object awaits installation;
   unrelated objects, including those at the same home Herald, progress independently;
6. one object has at most one released label record at a control prefix; its label
   revision/authority never decreases, and a terminal label-deleted outcome is not
   replaced by a live label;
7. a label decision follows the home's selected publication settlement; successful
   completion requires installation by every captured survivor;
8. a successful label has one immutable target, decision index, and result;
9. a predefined disappearance or regular-definition retirement is applied only from
   a still-valid complete captured
   evidence set;
10. future obsolete-state reclamation is ordered only from complete all-member cut
   evidence plus the specified bounded-latency horizon;
11. peer discovery, placement, structural-frontier, and alignment messages cannot
   change Oracle state except when their normalized evidence is explicitly submitted
   to an open Oracle workflow;
12. controlled instantiation, current update, obsolescence, and their publication
    payload/descriptor cannot be represented as Oracle commands;
13. Oracle state contains no authoritative controlled ID-to-sort or current/obsolete
    projection and no live/reconciled graph. Admission retains checked structural
    cut identities and vectors as claims; Herald owners reproduce their graph
    meaning before reporting readiness; and
14. equal committed Oracle prefixes produce equal canonical states, receipts, and
    effect digests at every replica.

## 11. Non-voter Herald admission

Only one admission is pending. Begin captures the exact predecessor generation
and closes each old member's membership gate when applied. The gate covers all
eight new application calls, child preparation, administrative Start, first
attachment and new wait acquisition. Requests retain their correlation without
allocating identities or possession. Existing result lookup, cancellation,
termination, and finite already-admitted work remain available. Membership and
selected label-publication holds have independent owners.

Each old source drains ready work, freezes its source cutoff, and retains a
canonical semantic history. The old membership establishes the structural cut;
the pending applicant supplies no old-member report. The seal binds the exact
source set, each source's completed input and stamped high-water marks, a common
established cut, the control prerequisite and six-view bootstrap contribution.
Every old member reproduces the complete source union before accepting it.

The applicant imports contiguous canonical control, original structural bytes and
stamps, established cut and terminal-base certificates, and the six mandatory
system-view observations into fresh receiver-owned stores. Historical label and
disappearance controls apply their semantic consequences without creating local
participant reports. No application delta store, possession map, private owner
representation or physical connection is transferred.

Old-member base readiness and newcomer readiness name the same current seal and
candidate recipe. Activate requires every exact report and supplies its actual
control coordinate to install the checked successor base. This adds the six
admission-origin system-view vertices, adds no edges or writers, zero-extends the
new source component, starts ordinary participation and releases the membership
gate in one serialized transition. The onboarding control replay authority ends
at the newcomer's activation; only exact retained retries remain valid there.

Process End and label release/deletion invalidate the semantic attempt. Retirement
cancels an unfinished admission, releases the old gates, and leaves the cancelled
epoch inactive. An operator can launch a fresh epoch after the surviving
predecessor base is established. The full contract and source ownership are in
[Herald membership](../architecture/herald-membership.md) and the
[owner contracts](../verification/owner-contracts.md).
