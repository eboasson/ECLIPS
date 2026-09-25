# Oracle replication and dynamic voters


This document extends [the networked Oracle design](../networked/04-oracle-and-raft.md).
[Herald membership](herald-membership.md) owns semantic admission, state transfer,
retirement application, and isolation. The two membership planes share ordered
coordination facts, but retain different members and different responsibilities.

## 1. Decisions and baseline

Use one Raft-backed Oracle for every deployment size. Use native learners and
joint consensus and checked in-memory checkpoints plus retained-suffix replay.
Voter changes include explicit administration and automatic exclusion of an
accepted failed voter host. Discovering
another Herald never grants a vote or initiates an automatic promotion.

The Oracle integration owners are:

- `raft-core/Eclips.Raft.Genesis` retains immutable initial voters and timer
  policy, with separate checked voter and learner initialization.
- `Eclips.Raft.Configuration` owns checked stable/joint sets and exact native
  references. `Internal.Prepared` uses log-effective quorums for elections and
  current-term commitment, correlates RPC replies and owns the leader no-op,
  learner preparation and recent-leader guard.
- `Eclips.Raft.Input` carries `LeaderNoOp`, opaque `Application` and native
  `Configuration` entries. `Internal.Log` exposes a sealed ordered stream of all
  committed positions, including opaque configuration metadata.
- `oracle-core/Eclips.Oracle.Genesis` binds native voters injectively to active
  Herald epochs initially. `Eclips.Oracle.Voter` owns retained registrations,
  change intentions and committed configuration projections. `Internal.Failure`
  derives its reporter set from current committed stable voter bindings,
  qualifies voter-host exclusion and repeated non-voter retirement through
  checked membership history. Pending/joint voter changes gate new probes;
  accepted voter intentions supersede existing open probes before bindings change.
- `protocol-raft` binds connections to immutable run/genesis identity and exact
  source/target identities. A transport catalogue can include learners and
  retained historical peers without granting native votes. `oracle-runtime`
  keeps private Raft and Oracle owners and acknowledges contiguous native
  positions. Its adapter interprets committed configuration metadata against the
  retained intention and publishes a configuration-origin control-watch entry.

Native and Oracle authority remain separate in their respective owners. Existing
semantic commands and application payloads remain opaque to Raft; structural
graphs, application values, process private handles, and semantic peer discovery
contacts remain outside Oracle state. Registered Oracle replica endpoint hints
are retained to reconstruct replication and client routes, without granting votes.

## 2. Small-system policy

| Active Heralds | Initial/default voters | Consequence |
| --- | --- | --- |
| One | The bootstrap Herald alone | Its own vote and log replica form a quorum. |
| Two | The same single voter; second may be a learner | Losing the sole voter stops Oracle progress. |
| Three or more | Explicitly commission a chosen voter set | Three voters tolerate one unavailable voter. |

The administration API accepts every nonempty voter set, including even sets.
Two voters require both; four require three. The launcher recommends one or three,
but voter-set parity does not constrain admission. A batch commission of two
caught-up learners can change one voter directly to three through joint consensus.

Only explicit `--bootstrap` startup constructs a new `SystemId` and singleton
genesis. A node started to join an existing run waits for that run if seeds are
unreachable. It never creates a competing Oracle, reuses an existing run identity
with new genesis, or guesses a smaller quorum from reachable contacts. Existing
multi-voter checked genesis remains useful for fixtures and explicit deployments.

A singleton election checks the self-vote immediately, becomes leader, appends
its current-term no-op, and runs commit advancement after that local append.
The same local-append path commits an Oracle proposal without waiting for a
nonexistent peer response. Service readiness still requires no-op commitment and
ordered adapter application. Self is counted only when present in the relevant
voter set, once per set. An empty proposed voter set rejects before preparation.

No lost-quorum downgrade exists. After three voters lose two, the survivor cannot
remove them; after two lose one, the survivor cannot become a singleton. Restoring
connectivity can restore progress for still-running replicas; restarting a failed
replica with forgotten term, vote, or log is outside this profile.

## 3. Native configuration representation

The native representation uses checked, domain-independent data with hidden
construction; the following shows its conceptual shape:

```haskell
data VotingConfiguration
  = Stable VoterSet
  | Joint VoterSet VoterSet

data RaftEntry bytes
  = LeaderNoOp
  | Application bytes
  | Configuration VotingConfiguration bytes
```

Both sets are nonempty and canonically ordered. The final `bytes` are opaque
adapter metadata; Raft can compare and replicate them but never decode them.
`RaftConfigurationRef` identifies either genesis or the exact `(log index, term)`
of a configuration entry. It is neither a schema version nor a protocol version.
Timer policy remains native immutable genesis configuration in this profile.

The owner retains derived views of the last configuration in its current log,
the last committed configuration, replication targets, and an unfinished change.
Log-effective configuration and committed configuration are deliberately distinct.
Conflict truncation recalculates the effective view from the surviving log. No
external worker mutates these facts, and no transport contact grants voting status.

A learner is a registered replication target outside the effective voter set. It
receives ordinary log replication and applies Oracle history without contributing
to elections or commit quorums. Learner registration is retained as an Oracle
control fact so the next leader can recreate replication intentions. The native
owner receives a concrete replication-target update from the adapter, containing
only `RaftNodeId`s; it receives no Herald identity or Oracle state.

## 4. Exact joint-consensus rules

For a set `V`, define `majority(V, A)` by `2 * size(V intersect A) > size(V)`.
The effective configuration's quorum predicate is:

```text
Stable V:       majority(V, acknowledgers)
Joint old new:  majority(old, acknowledgers) AND majority(new, acknowledgers)
```

A configuration takes effect locally when appended, including at the leader.
Append `Joint old new`, commit it using both majorities, then append `Stable new`
and commit it using the new majority. Each replica uses its latest logged
configuration for election and commitment decisions; conflicting uncommitted
suffix replacement restores the preceding configuration. Only one change is
unfinished at a time. Joint replication targets include both voter sets. These
mechanics follow [Raft section 6](https://raft.github.io/raft.pdf).

The existing last-log election check and current-term commit restriction continue
to apply to configuration entries. The leader chooses the greatest current-term
index acknowledged by its effective quorum and advances the whole committed
prefix; it never directly commits an earlier-term configuration by counting
replicas. A new leader first establishes its current-term no-op and applies the
committed prefix, then resumes the recorded change. If it inherits uncommitted
joint/final entries, it replicates that suffix under its effective configuration;
it does not append a second competing configuration.

Votes are counted from the effective sets, not the replication-target set or the
TCP connection map. Count one physical node in each set containing it. Learner
acks may advance learner progress but cannot satisfy a quorum until that node is
in the log-effective configuration. Refresh candidate vote accounting and dispatch
expectations whenever log repair changes that configuration.

A replica newly entering a logged joint configuration may vote immediately;
waiting for its Oracle projection would deadlock committing that configuration.
An ordinary learner outside the effective configuration does not campaign. A
previous voter excluded by an uncommitted final entry remains eligible to help
finish that transition, including campaigning without counting its own vote;
once committed exclusion is known, it stops campaigning. Do not encode all
these cases as the rule “local node absent means stop immediately.”

## 5. Removing leaders and suppressing stale election disruption

Allow the current leader to finish a change excluding itself. After appending the
final entry it replicates using the new set and does not count itself. On learning
that entry is committed, it exposes the committed prefix, relinquishes leadership,
and rejects new proposals. A surviving voter then runs an ordinary election. No
leadership-transfer RPC is required for this profile.

A removed, still-running node may not yet know its exclusion. The standard
recent-leader guard: a `RequestVote` arriving within the minimum election timeout
after valid current-leader contact neither advances term nor receives a vote.
This guard is checked before the ordinary higher-term rule, and expires through
an explicit generation-qualified timer input. Leaders renew their own guard from
fresh quorum acknowledgements, not merely from sending local heartbeats or still
having the leader role; without that evidence their guard also expires. An
explicit fresh local acknowledgement suffices when the effective configuration
is a singleton containing the leader; it cannot substitute for remote evidence
in a larger or joint configuration. Partial quorum evidence also expires: the
first fresh reply after protection has expired starts a collection window of
one minimum election timeout, without itself enabling the guard. Replies from
an expired window cannot combine with later replies to renew protection.
This protects an active cluster
from repeated elections by disconnected former members. See
[Ongaro's dissertation, sections 4.2.2–4.2.3](https://web.stanford.edu/~ouster/cgi-bin/papers/OngaroPhD.pdf).

Keep normal higher-term handling for applicable append traffic and responses.
This is an availability mechanism, not permission to disregard a legitimately
new leader. Existing term/dispatch correlations suppress stale replies; retired
connection generations cannot resurrect obsolete expectations. The property
suite must include asymmetric partitions and already-inflated old terms, rather
than testing only a clean removal whose recipient immediately learns the result.

Do not reject a candidate or leader merely because it is absent from the
receiver's latest voter set: receivers can lag configuration entries, and a leader
can temporarily finish a configuration excluding itself. Connection identity
admission, native log freshness, local voting eligibility, and counted quorum
membership are separate checks. This is a mandatory protocol migration from the
fixed-voter implementation, not an optional optimization.

## 6. Replica registration and finite catch-up

`prepareOracleReplica(activeHerald)` commits a unique native node-to-host binding
and retained learner intent. A pending semantic join cannot host a voter or
produce failure evidence; first complete its activation under the Herald design.
A fresh Raft node starts with an empty log plus the same checked immutable
genesis, in learner mode. It does not copy any other owner's `RaftState` or
`OracleState`. Live demotion may retain the same node as learner; a process that
actually crashed can only join as a fresh epoch and fresh node identity.

Preparation also works on Herald epochs admitted after genesis through the seed
join workflow. The deployment configures concrete Oracle/Raft endpoints before
accepting local preparation, then starts the learner only after an owner-issued
notification of its committed registration. Ordinary Heralds waiting for this
operation do not poll their kernel or a full status snapshot. A failed Oracle
role remains stopped for the run while its ordinary Herald scope can continue.

The leader replicates the retained suffix through prefix matching and append
acknowledgements. A learner needing an earlier prefix installs the checked
in-memory application checkpoint before receiving the suffix. The learner independently executes the
canonical Oracle commands and committed configuration metadata in order, deriving
the same control index, command receipts and membership history. Replicas use
the same immutable diagnostic-digest mode: disabled by default, or enabled for
the complete run. Enabled mode additionally derives the same full-state digest;
disabled mode retains explicit absence in applied-entry evidence. Exact entry
equality always includes this optional witness.
Native checkpoint installation preserves configuration and applied order, as
specified in [Raft checkpoints](../networked/04-oracle-and-raft.md#95-in-memory-checkpoints-and-native-log-reclamation).
There is no disk durability or old-epoch restart promise.
Semantic Herald catch-up remains an additional requirement; Oracle replay contains
no graph, publication inventory, or application data.

Commissioning captures a concrete leader log frontier after preceding proposals
settle. Briefly gate new Oracle proposals while checking that every promoted
learner both matches that frontier and has applied its adapter prefix. The native
owner reports progress tokens; the learner's Oracle owner reports an applied
frontier through its own boundary. The coordinator compares these observations,
not mutable private state. Once ready, it appends the joint entry and leaves
learner preparation; ordinary service still obeys native and adapter readiness.
Failed catch-up remains in `VoterChangePreparing`, representing the
`WaitingForLearner` condition; administration may cancel before the joint entry
enters the surviving log. Cancellation is an ordered state-machine outcome, not
deletion of a queue entry.

Leadership change invalidates local frontier/readiness observations. The next
leader establishes its own no-op, reconstructs the learner targets and intent,
and samples a new frontier before an unstarted change. After a joint entry exists,
catch-up failure can block progress; it cannot authorize rollback outside Raft.
Finite-log retention and finite test runs are assumed to fit available resources.

## 7. Oracle composition and control-index accounting

Oracle retains Oracle-owned configuration history and a change record with this
conceptual shape:

```text
VoterConfiguration = {
  nativeConfigurationRef, stableOrJointBindings, predecessor?, controlIndex
}
VoterChange = {
  changeId, expectedStableConfiguration, oldBindings, newBindings,
  reason, controlIndex,
  phase: Preparing | JointCommitted | VoterExcludedAwaitingHeraldRetirement
         | Completed | Cancelled
}
reason = ExplicitVoterReason (ExplicitCommission | ExplicitDemotion)
       | AcceptedHostFailureReason FailureProbeResolutionId
```

The public phase constructors are `VoterChangePreparing`,
`VoterChangeJointCommitted`, `VoterChangeCompleted`, and `VoterChangeCancelled`.
The promoted-learner requirement is derived from the new bindings minus the old
bindings. Applying the final stable configuration also marks the explicit change
`Completed` in that same control entry. Automatic failed-host exclusion instead
enters `VoterExcludedAwaitingHeraldRetirement`; its retained certificate permits
semantic retirement to complete the intent (§9). Explicit demotion preserves
semantic membership.

Bindings are injective from native node IDs to admitted Herald epochs. A joint
projection carries old and new maps separately, with equal bindings on shared
keys. `VoterConfigurationId` binds the native entry reference and exact canonical
binding projection. The Oracle keeps no independent mutable voter-list setter.
Genesis supplies the initial configuration; subsequent truth comes exclusively
from committed native configuration entries.

`BeginVoterChange` is an ordinary canonical Oracle command. It checks the expected
stable configuration, intended active hosts, complete new binding set, no empty
voter set, and the membership-coordination slot. Its receipt fixes one change ID
and complete intention. Exact request retry retains the same receipt. A later
status query reports workflow progress without mutating that original receipt.

The adapter alone prepares configuration-entry metadata from this accepted
intention. It supplies the exact native configuration and internal Oracle
configuration observation together. No application or administrator can directly
submit an observation claiming that a configuration committed. Configuration
metadata has stable identity `(changeId, Joint | Final)`, so leader retry cannot
create a second logical transition.

The sealed, ordered committed-entry stream has these adapter actions:

| Committed entry | Adapter action | Control-index effect |
| --- | --- | --- |
| Leader no-op | Advance applied-log cursor | None |
| First-seen Oracle command | Execute existing `stepOracle` command path | One |
| Exact command retry/conflicting reuse | Existing deterministic classification | None |
| Configuration | Apply its checked internal configuration observation | One |

A control index counts first application of Oracle commands **and**
committed configuration observations. Each configuration observation has its own
contiguous control-watch event, enabling clients to reconstruct current bindings.
A configuration is never exposed speculatively when merely appended to Raft.
The adapter acknowledges every processed log position in order; service readiness
waits for configuration observations as well as ordinary commands.

Oracle application validates that metadata names the retained intention and the
native configuration just exposed by Raft. An actually committed contradiction is
an invariant fault, not a semantic rejection followed by two different membership
authorities. Checked proposal construction, sole-owner serialization, and model
properties must establish that admitted histories cannot produce this fault.

A leader may disappear after committing a native entry but before its local
adapter sees it. Other replicas reconstruct the same event from the retained log.
No “configuration changed” callback outside the ordered stream can supply state
needed for reconstruction. Ordered intent and observation records survive leader
changes within the running cluster; this is not disk durability or restart support.

## 8. Explicit operations and contact discovery

Administration exposes these administration operations with typed receipts and status:

- `prepareOracleReplica(host)` registers/reuses a live learner on an active host.
- `commissionVoters(expectedConfig, learners)` adds a chosen set atomically through
  one joint transition; each must satisfy catch-up readiness.
- `decommissionVoter(expectedConfig, node)` removes its voting role and normally
  retains a live learner and all of that Herald's semantic services.
- `cancelVoterChange(changeId)` commits only while preparation can still be
  cancelled; its decision is serialized with the configuration proposal gate.
- `voterChangeStatus(changeId)` and `oracleConfiguration` return owner projections.

Planned voter demotion does not retire the Herald, end its applications, discard
its graph, or imply a graceful host shutdown. Full semantic shutdown requires the
separate membership workflow. Automatic accepted-host-failure processing, below,
excludes its native node entirely after configuration completion.

Mutation acceptance reports a retained command identity or immutable receipt; it
does not report a completed voter workflow. In particular, an accepted Begin
receipt continues to contain its original `Preparing` phase after later progress.
The configuration and change-status queries return the selected owner's applied
projection, including its control index. They neither mutate that receipt nor
certify that a currently unreachable quorum can make further progress.

The proposal gate settles an existing native proposal before accepting cancellation.
An inherited uncommitted joint/final entry must first settle under the new leader;
the coordinator cannot cancel using a stale `Preparing` Oracle projection. If the
entry was overwritten through normal log repair, cancellation may instead commit
before any replacement joint proposal. Once cancellation applies, no metadata for
that intention can pass the proposal gate. A command timeout leaves the result
unknown until its retained receipt or change status is observed.

The current-term leader's EORC lane stays available during learner preparation so
the operator can reach cancellation and retained results. Ordinary commands receive
paced `NotReady` while the proposal gate holds. A retained Herald cancellation
can bypass an ordinary request waiting at that gate without replacing its stable
request identity or discarding the ordinary request.

Client routing uses control-watch configuration projections plus runtime locators.
During a joint transition clients try the union of old/new voters; after final
commitment they prefer the new set. Existing
leader hints remain hints. Retain recently known contacts as reconnect candidates
until a fresh authoritative projection is obtained; a locator never changes votes.

ERFT and EORC Hello bind the immutable run/genesis identity and canonical catalogue
facts. Mutable voter configuration is not an equality gate for these identities.
A connection can be used for learner catch-up or configuration repair when its
ends know different configurations. Preserve exact source/target binding and
wire-shape checks while replacing “both endpoints are genesis voters.”

Replica registration supplies canonical node/host bindings and dynamic contact
projections. For a receiver behind that registration, the runtime resolves the
same-run binding through its bootstrap/control catch-up lane before retrying the
Raft lane; it must not require an all-voter acknowledgement to register a learner.
The exported registration fact names its canonical binding and admission control
index. Before local Raft replay reaches that fact, it supplies only a provisional
transport binding: it does not apply a command to the receiver's Oracle owner,
change voter sets, grant a local vote, or admit a semantic Herald. The restricted
discovery listener serves this lookup independently of ERFT, so resolution does
not require the very Raft connection it enables. Local replay subsequently checks
the same binding against authoritative committed history.
Learners, current transition participants, and retained historical bindings are
admissible lane peers, while native rules decide whether their messages can
contribute a vote. The implementation gate includes new-leader contact to a
receiver that has not yet installed that leader's commissioning entries. It must
make progress without circular dependence on receiving Raft over a refused lane.

## 9. Automatically excluding a failed voter host

Failure admission derives reporters from the captured stable configuration, qualifies direct
probes with its identity, and retains the checked old-majority certificate across
native exclusion and semantic retirement. Pending voter changes and joint
configurations gate new probes. Fresh health observations use the native owner's
log-effective configuration independently of the committed Oracle projection.

Keep suspicion, failure acceptance, native reconfiguration, and semantic retirement
as separate transitions. The Oracle records the accepted evidence and pending
intention before attempting either membership change. No local timer edits a voter
set, and no failure detector is treated as proof that an OS process has stopped.

A failure probe captures `(target epoch, semantic generation, stable voter
configuration ID, exact node/host bindings, probe ID)`. Reporters are the distinct
Herald hosts of that **old stable voter configuration**. Fresh direct probes begin
after committed Open, with exactly one immutable result per reporter. The target
is included in the old denominator; unreachable reporters are not subtracted.
Reachable and unreachable outcomes use separate strict-majority tests over that
same denominator, so contradictory accepted outcomes cannot both exist.

Do not open new failure probes during a pending voter change or a joint
configuration. Existing probes are superseded when a different change claims the
coordination slot. After final completion, open fresh probes under the successor
stable configuration. During joint consensus, Oracle-health/isolation observations
require both majorities; a majority of the union is insufficient. Do not reuse
health observations as failure-probe reports.

One-shot health admission requires the source Herald to remain active in the
Oracle owner's committed membership, independently of leader readiness and the
caller's history. A retired source cannot renew its grace if it missed the final
watch. Explicit voter demotion preserves health admission.

Health replies also carry the native owner's effective configuration reference,
which can precede the committed Oracle projection. Once a Herald learns that a
joint configuration is effective, a cached stable-configuration contact set cannot
establish health for it. Configuration append or truncation invalidates the relevant
health observation generation; require fresh matching observations under the
effective predicate. These observations guide runtime liveness only and never
install an uncommitted configuration into the Oracle control watch.

A failed voter suspected during cancellable learner preparation first causes an
ordered cancellation, then a fresh failure probe. Once joint consensus has begun,
complete that transition before opening another probe. If its required quorum is
unavailable, status remains blocked on quorum: automatic removal cannot repair an
arbitrary incomplete reconfiguration by bypassing its own consensus requirements.

The workflow for a voter-host failure is:

1. `AcceptVoterHostFailure` commits the exact old-configuration majority certificate
   and an exclusive intent. Preserve its original denominator and semantic
   generation. This terminalizes the probe as accepted, independently of progress.
2. Drive `Joint old (old minus target)` and its final stable configuration using
   normal Raft quorums. No acknowledgement from the target is required.
3. After committed final exclusion has been applied by the Oracle adapter, apply
   `RetireHeraldEpoch` against the retained certificate and intended semantic
   predecessor. End target-resident live process epochs and advance semantic
   membership through the existing pure projection machinery.
4. Mark the shared intent completed, expose its receipt/status, and release the
   membership-coordination slot. Future leaders can derive every unfinished step
   from committed history and retry it without collecting replacement evidence.

Unrelated semantic membership changes and voter changes are serialized while this
intent is active. Ordinary application and non-membership Oracle operations may
continue where their own semantic preconditions allow. A pre-existing label or
disappearance workflow cannot hold the membership slot waiting for the failed
Herald: retirement invalidates/replans its captured-generation evidence as specified
in the Herald membership design. Otherwise the very workflow retirement must
unblock would prevent retirement from starting.

The final semantic retirement accepts the **retained old certificate**, even
though its reporter set differs from the now-current voter bindings. It does not
open a new failure probe, wait for the removed voter, or re-evaluate the majority
against the smaller set. Applying final native exclusion alone never silently
changes the semantic member set. Intermediate status explicitly says
`VoterExcludedAwaitingHeraldRetirement`.

With old voters `{A,B,C}`, reports from `A,B` can exclude `C`; joint commitment
requires two old votes and both new votes, then final commitment requires `A,B`.
Afterwards both voters are needed until explicit expansion. If a replacement
learner exists, an administrator may commission it first; automatic replacement
selection is outside this profile. `{A,B}` cannot automatically lose `B`: one
survivor lacks the old majority. A singleton cannot remove its sole voter.

## 10. Still-running targets and isolation

Every Herald, including a voter host, uses fresh native quorum health to assess
isolation. Failure of one cohosted Oracle role is not itself a retirement or fence:
other current voters can establish health. Accepted failure or expiry of the
quorum-loss grace ends the semantic epoch irreversibly.

Acceptance is permanent exclusion of the named epoch, even if a partition later
heals. Survivor Heralds apply the committed retirement tombstone, reject that
epoch's new semantic work, close/reclassify peer sessions, and reconcile retained
origins according to the membership design. Reconnection cannot resurrect its
applications, prepare new children there, or regrant its former voting role.

There is no voter-host exemption from semantic isolation. A Herald that cannot establish current Oracle quorum health self-fences its
semantic role and seals publication origins through its own kernel. The cohosted
Raft owner remains separate and continues replication/voting under native rules;
semantic self-fencing never changes the quorum or unilaterally stops Raft. The
Herald control/status shell remains live after the bounded application drain, and
only explicit physical shutdown tears down the cohosted runtime scope. Once
committed exclusion reaches that owner, its native role ceases as specified above.

An isolated target can execute locally before detecting isolation, but cannot
obtain conflicting Oracle commitments. The surviving semantic membership must
not incorporate its post-fence traffic after retirement. These are finite-run
partition semantics, not proof that every process has stopped, Byzantine defence,
authentication, or recovery of an abandoned partition.

## 11. Required outcomes and verification gates

The admission/status vocabulary must distinguish `AlreadyVoter`, `AlreadyLearner`,
`UnknownReplica`, `HostNotActive`, `StaleConfiguration`, `ChangeInProgress`,
`WouldRemoveLastVoter`, `WaitingForLearner`, `JointInProgress`,
`Completed`, and `Cancelled`. Names follow the typed conventions above;
`Preparing` represents unfinished preparation and `JointCommitted` represents
committed joint progress. Automatic failure exclusion adds `VoterExcludedAwaitingHeraldRetirement` for its
separate cross-plane workflow. Transport loss is an uncertain/pending operation
with stable retry identity, not evidence that the semantic action did not happen.
No fixed resource limit, rollover outcome, compatibility decoder, or version field
belongs in these types.

The full profile requires properties at three levels. The native and composition
obligations cover native voting, accepted voter-host failure and semantic
retirement/fencing:

1. **Native:** singleton election/commit; set-majority laws; learners never count;
   joint dual-majority election/commit; current-term rule; config append/truncation;
   removal of current leader; no double leader or conflicting committed prefix;
   reordered/duplicate RPCs, correlation supersession, and stale timer generations.
2. **Composition:** replay equals an original replica at every applied-log prefix;
   no-op/retry/configuration control-index classification; one ordered observation
   per configuration; failed leader at every intent/joint/final/adapter boundary;
   replica/contact projection reconstruction; cancellation versus joint-append race.
3. **Membership:** exact old reporter denominator; no evidence rebasing or reuse;
   accepted intent resumes under a new leader; target acknowledgement unnecessary;
   no label/retirement deadlock; no empty voter set or lost-quorum fallback;
   planned demotion preserves Herald service; accepted host retirement fences it.

Real TCP/process verification across the profile must cover singleton; two
Heralds with one voter; one-to-three commission; learner catch-up during Oracle
activity; current-leader demotion; later expansion and replacement; a seed-joined
active Herald becoming a voter; a removed partition returning with stale
configuration and elevated term; leader failure at every unfinished transition;
and quorum loss that remains pending without manufacturing success. Explicit role changes include interrupted and pending outcomes. Automatic failure exclusion adds
three-to-two failed-host retirement, successive failures under successive
configurations, and fencing of the still-running excluded target. Run existing application,
label, disappearance, retirement, and wire golden suites against the changed
protocols. No sleeping test should stand in for observing an owner-issued frontier.

The hard proof obligations are the effective-versus-committed configuration
boundary, leader-removal election cases, ordered Oracle metadata projection,
lagging binding admission, and the cross-plane retirement interlock. Close each
with model/property evidence before enabling its corresponding TCP workflow.
A detailed plan is not evidence that these obligations are already satisfied.

## Configurable deployment timing

Deployment has one [takeover target and derived policy](takeover-timing.md), selected at
fresh deployment creation and inherited by joiners and application clients. At
the default five seconds, native heartbeat is 25 ms, elections are randomized
250–500 ms, peer recovery is two seconds and the fresh post-Open probe is
500 ms. These change waiting policy only: native quorum, accepted evidence,
ordered exclusion/retirement, stale epochs and independent semantic/native
lifetimes keep the rules above. Recent-leader protection and accepted-watch
authority grace continue to derive from the native lower/upper election bounds.
