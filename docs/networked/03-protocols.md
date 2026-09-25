# TCP and protocol specification

This document owns all communication between operating-system processes. It
defines logical behavior rather than a language-specific codec API.

## 1. Protocol planes

The prototype has five independent TCP protocol families:

| Plane | Endpoints | Purpose |
| --- | --- | --- |
| Application RPC | application library to one Herald | Sessions, eight-operation calls, local child preparation/claim/cancellation, own-process End, replies, wakeups, retry bookkeeping. |
| Herald administration | trusted process supervisor/operator to one Herald | Explicit process start/end, unclaimed-child cancellation and retained results; never application data. |
| Herald peer | Herald to Herald | Discovery gossip, delta placement, direct publications, markers, and alignment. |
| Oracle client | any Herald to Oracle leader/replica | Submit commands, follow redirects, watch committed Oracle entries, query exact results. |
| Raft | bound native replicas, including learners and retained transition peers | Elections and replicated opaque application/configuration log; transport binding grants no vote. |

Deployments SHOULD use separate listeners. A shared listener MAY demultiplex by a
fixed preface, but an established connection has exactly one role. Successful
decoding in the wrong family is still a role violation.

This is one internal prototype protocol/API, not a public compatibility contract.
“Profile 0.1” is a specification-scope label and never appears on the wire. Frames
and handshakes carry no API/protocol version, supported-version set, compatibility
range, or negotiated feature set. All endpoints in a run are built against the same
current API/schema definitions, and every Herald/Oracle role uses the same
deployment catalogue. A schema or API change replaces the sole current definition,
codec, and tests in lockstep; the old run is discarded and a fresh genesis is
started. Mixed builds, rolling upgrades, compatibility decoding, and migration are
unsupported.

Administrative verbs needed to open or end a session, report barrier progress, or
look up a request result belong to these protocols. They do not enlarge the
eight-operation data-space API.

## 2. Framing and decoding

TCP does not preserve messages. Every family uses incremental framing:

```text
uint32be frameLength
uint32be familyMagic
familyBody
```

`familyBody` is one closed current-build message or envelope value encoded with
`Data.Binary`. It owns its typed correlations and payload. There is no mandatory
generic correlation field: a session, request, wait, peer stream, Oracle request,
and Raft RPC have different nominal correlations, and a duplicate untyped header
value would create two sources of truth. A family defines a correlation in its own
message type only when that family has one real meaning for it.

The first application family fixes:

```text
uint32be frameLength
uint32be familyMagic = 0x45415050  -- ASCII "EAPP"
Binary   ApplicationEnvelope
```

The closed envelope is the ordinary sum:

```text
ApplicationEnvelope
  = ClientEnvelope ApplicationClientMessage
  | ServerEnvelope ApplicationServerMessage
```

Its constructor supplies direction; the nested closed message sum supplies the
current message choice. Neither has a separately assigned wire-tag table. An
endpoint rejects the wrong envelope arm before client or Herald admission.

Its pure codec and incremental decoder arrive before the TCP interpreter.
Candidate-lane and established-binding correlations are runtime metadata and are
never encoded.

The first peer family fixes:

```text
uint32be frameLength
uint32be familyMagic = 0x45505250  -- ASCII "EPRP"
Binary   PeerEnvelope
```

`PeerEnvelope` is the closed current Hello/control/publication sum. Peer
connections are symmetric, so it has no client/server direction arm. Candidate
versus established phase, logical binding, physical generation, dispatch ticket,
and dispatch attempt remain runtime/owner facts and are never encoded. The magic
is a family discriminator, not a protocol-version field.

Ordinary wire-carried closed sums and records derive `Binary` in their owning
modules, including application values and operation/message DTOs. There is no raw
mirror of the application AST and no handwritten primitive or constructor-tag
schema. The library's `NonEmpty` instance carries non-empty shapes directly, and
the selected `text` package's existing `Binary Text` instance performs checked
UTF-8 decoding. Application maps and sets use the stock instances under the
same-build typed-producer assumption. Refined carriers use
small handwritten validating `Binary` instances: `get` decodes the underlying
representation and invokes the same checked constructor used by ordinary code.
This applies at least to positive private IDs, exact-32-byte public IDs and opaque
claim scopes, and positive reply cursors. Invalid representations fail decoding
rather than constructing an unchecked value.

A DTO whose invariant spans several fields or a collection instead hides its
constructor and gives `Binary get` through its ordinary smart constructor. The
current peer direction/gap/resume, normalized placement/contact, and non-empty
normalized publication-destination aggregates use this rule. Generic derivation
is retained for the closed envelope and records whose complete invariant is only
the product of already-checked fields.

After decoding a complete `familyBody`, the protocol codec requires the `Binary`
decoder's residual to be empty before admission. Stock map/set decoding and any
normalization it performs are accepted as-is under the same-build benign-producer
assumption.
Generic DTO bytes deliberately follow the
current `Binary` instances and Haskell declarations. They have no golden
compatibility promise: changing a declaration changes all current producers,
consumers, and tests together and starts a fresh run. This wire policy does not
replace or alter the semantic domain's existing canonical `cereal` encodings used
to derive content identities and semantic digests, and it is unrelated to replay
journals.

Requirements:

- `frameLength` excludes its own four bytes;
- readers handle partial headers, partial payloads, and several frames in one TCP
  read;
- an incremental feed returns every complete valid frame before the first partial
  or malformed frame, followed by either a need-more decoder state or a terminal
  error; therefore `[valid][malformed]` admits the valid frame exactly once under
  every TCP chunking and admits nothing from the malformed frame;
- invalid generic constructor discriminators, malformed refined values, trailing
  body bytes, and the wrong envelope arm fail closed; the one current schema has
  no compatibility-extension mode;
- a malformed frame changes no semantic state and has no resynchronization mode;
  already completed frames before it retain their ordinary effects; and
- alignment snapshots may be sent as identified chunks for incremental transfer;
  partial transfer never becomes partial semantic admission.

Connection handshakes establish only role and the plane-specific identities or
session facts defined below; they do not negotiate a version or features. Every
semantic ID has a distinct nominal carrier in its closed DTO position; equal byte
widths do not permit typed interchange. The generic representation need not add a
redundant per-newtype byte tag: the enclosing constructor and field fix the role,
the validating decoder establishes shape, and stateful admission establishes
meaning.

On peer and Oracle planes, `GlobalUniqueId` is encoded as exactly 32 opaque octets.
Its encoding contains no observable issuer, generator, epoch, or counter fields.
For a `GlobalObjectId`, `ProcessEpochId`, `NablaId`, or `DeltaId`, decoding checks
the 32-byte shape and constructs only the nominal checked claim. Stateful
admission/instantiation/reconciliation proves existence and role; the nominal type
alone does not. Exact-bit domain conversions preserve the octets but likewise
confer no semantic fact. `HeraldEpoch` and `StoreIncarnationId` are unrelated
domains.

The application plane never encodes any of those global IDs. It encodes the
nominal `PrivateUniqueId`/private-handle forms as a positive unsigned 64-bit local
counter value whose process qualification comes from the established session;
zero is invalid. The closed message field's nominal private-handle type
distinguishes every role even when two representations contain the same local
value. `SortId` remains a public 32-octet content address on every plane that needs
it and is never translated as a private unique ID.

Decoding or explicitly constructing a positive private token establishes only its
shape; resolving it in the established process map determines its meaning, and an
unmapped token confers no authority.

Application attachment, session, resume-token, and wait claims use separate
nominal opaque checked representations. The attachment and session scope contain
exactly 32 uninterpreted bytes, but the application type cannot name or convert
them to a `BootstrapManifestId` or `HeraldEpoch`; only the reviewed Herald adapter
can do so.
Session and token claims append one unsigned 64-bit ordinal. A wait claim contains
the session scope/ordinal plus its request ID. Reply cursors are positive unsigned
64-bit values. Request IDs and client nonces are client-selected unsigned 64-bit
values and may be zero; they do not borrow the positivity rule of private IDs.

## 3. Application RPC

### 3.1 Logical identities

The protocol distinguishes:

```text
ProcessEpochId        nominal key of one server-side canonical application lifetime and private-ID namespace
PrivateUniqueId       one never-reused local name within that process epoch
ApplicationSessionId  one live request/result/wait namespace
ConnectionId          one ephemeral TCP connection
RequestId             one idempotent call within a session
LifecycleRequestId    opaque session scope, session ordinal, and lifecycle request ordinal
ChildPreparation      one retained BeginChild acceptance, distinct from its eventual process handle
InitialClaimId        one stable client-generated initial attachment attempt
WaitId                 internal registration for one accepted wait call
DecisionId             Oracle identity used internally by label
ProcessAcceptancePosition  one serialized call position across a process's sessions
HeraldPublicationPosition  one serialized accepted-publication cut across processes
```

An application receives its Herald address and process attachment material out of
band. Under the benign profile, attachment material need not be cryptographic, but
it MUST still be an opaque checked type and MUST NOT be inferred from a claimed
`ProcessId` in an untrusted call.

### 3.2 Handshake and session lifecycle

The bookkeeping messages are:

```text
OpenSession(attachment, clientNonce)
ClaimInitial(connectionDescriptor, initialClaimId)
InitialClaimPending(initialClaimId, requiredEndpointKeys)
InitialClaimRejected(initialClaimId, startupError)
SessionOpened(replyCursor, sessionId, resumeToken, startupAccess)
ResumeSession(sessionId, resumeToken, lastObservedReply)
SessionResumed(replyCursor, sessionId, pendingAndCompletedRequestSummary)
SessionRejected(sessionError)
EndSession(sessionId)
RetireReceipts(sessionId, receiptRetirement)
ReceiptsRetired(sessionId, receiptRetirement)
Ping(heartbeatNonce)
Pong(heartbeatNonce)

-- Isolation and permanent-loss dispositions
HeraldIsolationBegun(replyCursor, sessionId, ResidentProcessesZombie)
HeraldPermanentlyUnavailable(sessionId, HeraldIsolated | HeraldRetired)
```

`startupAccess` contains the private self-process handle and an opaque exact
`PrimordialAccess` map. Keys are application-defined text; values distinguish
identity-only, object, writer, reader, and process names. Empty and partial sets
are valid. Required endpoint keys and the optional Nabla/Delta source-writer keys
are retained with the map. Structural decoding validates exact key/role shape;
the Herald separately admits normal grants and localizes names. An alias alone
confers no possession, and no startup decoder synthesizes an environment.
`selectEnvironment` explicitly selects all 31 objects from a checked complete
`EnvironmentAccess`: twelve endpoints, one hub and eighteen edges. Object grants
retain their structural sort evidence, and the twelve endpoints are required.
Parent-facing preparation admits these grants before Start; required-endpoint
readiness is checked again when initial claim creates a session.

`heartbeatNonce` is a lane-local unsigned 64-bit correlation with no semantic
identity. EAPP and EPRP use `Ping`/`Pong` for physical liveness: the physical connection owner consumes routine heartbeat and
ordinary-byte progress and reports a generation-qualified unresponsive/lost transition
rather than enqueueing every heartbeat through the Herald semantic kernel. The
same names in the two family sums are distinct constructors and cannot cross a
family boundary.
A connected EAPP candidate and established EAPP heartbeat replies use the same
ten-second transport response allowance on both the application and Herald sides.
The final client `EndSession` write uses that interval too. This application
policy is independent of peer failure detection and the takeover target; explicit
runtime configurations may select another interval. EPRP candidates use their
checked heartbeat-reply interval. A candidate that
does not complete its handshake within its interval is closed; an outbound owner
then returns to its ordinary paced retry. Failure to allocate that deadline or an
established heartbeat deadline is a runtime resource failure, not a fabricated
connection-loss or unresponsive-peer observation.
Ordinary application RPC completion has no fixed response deadline: operations
such as `wait` and `label` may remain pending while transport liveness is maintained.
`HeraldIsolationBegun` is emitted only by a non-voting Herald's irreversible local
self-fence. It advances the ordered reply cursor and begins the bounded read-only
drain; it is not a list of objects or a canonical membership decision.
`HeraldPermanentlyUnavailable` is the final best-effort server disposition before
lane close. Physical loss of an established application connection makes the pure
client terminal immediately, even when that final frame was not received.

`OpenSession` retains the existing genesis/administrative attachment path for an
already installed live process epoch at this Herald. Prepared children use
`ClaimInitial` instead. The latter checks the descriptor's target locator, lineage,
Herald epoch, and opaque attachment, then rechecks the child's current readiness.
Pending readiness creates no session and starts no application-session-loss
deadline. The matched `InitialClaimPending` disposition instead establishes only
physical heartbeat handling and ends the candidate-handshake deadline.

At readiness, one serialized transition consumes the initial grant, installs one
session and logical binding, and retains the exact `SessionOpened`. A distinct
claim cannot consume the grant again. The client preserves its stable
`InitialClaimId` across startup retries; an exact retry may recover an unobserved
result only while its admitted session remains live. End, cancellation, target
retirement, or established binding loss takes precedence over replaying an old
success. Pending readiness and an unclaimed child have no application-session-loss
deadline.

Before exposing prepared startup access, the client confirms the learned session
on the same TCP connection with `RetireReceipts(sessionId, emptyProgress)` and
waits for `ReceiptsRetired`. An idle application therefore confirms the initial
result without a transport handoff. No startup retry can revive a session after
its established binding has been lost.

Neither attachment handshake creates a process or a private map. Missing, dead,
wrong-target, or cancelled attachment material has a closed typed rejection.
Process creation is an explicit retained lifecycle or administration request. A
successful attachment allocates an isolated request namespace and wait set.
Several open sessions for one process epoch share
its already installed private/global ID map and receive equal typed startup-access
handles. They do not share request IDs, results, waits, resume tokens, or logical
connection bindings. The global `ProcessEpoch` is resolved from attachment
material and retained server-side; it is not returned in an application DTO.

Loss of an established TCP binding ends its logical application session
immediately. The Herald removes its reply reachability and waits, while accepted
semantic work continues under its owning state. `ResumeSession` remains a
checked handoff of a still-live session; it cannot recover an ended one and
returns `SessionNotLive` after loss has been ordered. A stale close from a
superseded physical generation does not end the replacement binding. The
prototype stores session state in memory; a Herald restart cannot recover the old
session, process map, or Herald epoch.

`lastObservedReply` is a session-scoped opaque reply cursor. Conceptually no reply
has initially been observed; every wire cursor is positive. A client cannot resume
before `SessionOpened` has supplied the session, token, and first positive cursor,
so no zero cursor is ever encoded. Every newly retained session acknowledgement,
request reply, or wait wake receives the next positive sequence; retransmission
reuses it. The Herald retains the sequence
that established the current logical binding. A resume cursor below that sequence
reoffers the exact current binding and summary because the establishing reply was
lost. A cursor at or beyond it, but not beyond the greatest issued reply, permits
exactly one binding advance. A newly advanced binding retains the exact
`RequestId`-ordered summary carried by that disposition. Reoffer after a lost
disposition repeats that snapshot even if a wait completed meanwhile;
`GetRequestResult` returns the newer individual request state. The summary reports
only this session's unretired pending (including `WaitId`), completed, rejected,
and cancelled requests. Receipt retirement also prunes retained summary copies.
Runtime code never infers this decision from a physical lane.

Client cursor identity is the normalized owner observation, not merely the outer
message constructor. In particular, an asynchronous `WaitWake(WaitReady)` and a
later `RequestRetained(Completed(WaitCompleted(WaitReady)))` at the same cursor are
equal observations of one completed request. While the corresponding observation
is retained, an exact duplicate is idempotent; the same cursor with a
non-equivalent session disposition or request status is a protocol contradiction.

A newly emitted asynchronous wake on the current ordered live lane advances to
the next cursor. A retained reply returned by exact call retry or
`GetRequestResult` may instead reoffer that request's older issued cursor. The
client admits the old cursor only when its normalized request status is coherent
with the state already learned for that request and updates its observed cursor by
`max`; a legal reoffer never regresses resume progress.

The pure client retains normalized cursor observations only while their request
is pending, together with compact session/cursor high-water facts. Before a
terminal receipt is retired, an old individual reply can be reconciled with its
retained request state without regressing the greatest observed cursor. Once the
terminal result has been transferred to its caller-owned handle, the shared
request and cursor entries can be removed. Late covered replies are recognized
from compact retirement progress and cannot recreate a request or deliver its
result twice.

Client admission is phase-exact: ordinary opening accepts only its session-opened
or session-rejected disposition; prepared opening additionally handles matched
claim-pending/rejected dispositions; an outstanding resume accepts only its
session-resumed or session-rejected disposition; active operation accepts request
replies, wakes, receipt confirmations, or one ordered isolation-begin disposition.
Isolation drain accepts only replies to local `read`/existing result lookup and the terminal-unavailable
disposition; ending/terminal state accepts no server DTO. Every
session, request, lifecycle, initial-claim, and wait claim must match the client's retained owner facts. A
wrong-phase constructor or foreign/inconsistent correlation is a client protocol
contradiction with no successor, not a second semantic result. Exact session
dispositions are duplicate-idempotent only while the corresponding open or resume
attempt remains outstanding; stale physical-lane input is filtered by the runtime
boundary before client admission.

`EndSession` closes only that request/result/wait namespace and cancels its
internal waits. It neither removes the process's private/global mappings nor proves
that the canonical process died; another existing or later session for the same
live epoch observes the same private IDs. For unexpected established loss, the
pure Application owner immediately terminalizes that session. If no live sibling
remains, the same transition seals later resume/attachment and retains one
idempotent internal `EndProcessEpoch(ApplicationPermanentlyLost)` intention for
`OracleClient` dispatch. Duplicate or stale loss observations cannot retain another
End intention. A live sibling prevents the seal. Before accepting the
Oracle-backed `label` call, the Herald has already installed its complete
self-contained owner state and exact `OracleClient` intent. Session end removes
only reply reachability and cannot cancel its semantic stage, dispatch intention,
or post-decision label barrier. `newenv` and controlled
instantiation/update/obsolescence are retained direct-publication workflows and
create no Oracle intention. `wait` is the cancellable exception.

`EndProcessEpoch` ends every session for the epoch, cancels pristine `Reserved`
entries, and discards its private map and next-local counter. Every already
accepted Oracle-backed workflow continues exact dispatch/status query and any
label visibility-barrier work until contiguous Oracle order reaches its
owner-defined terminal settlement. Work ordered before process end applies first;
work ordered after it is rejected or otherwise terminally settled against the dead
process. A controlled publication or `newenv` already accepted before process end
continues through its retained direct-publication and structural-stabilization
work; it has no Oracle half to settle. No application reply is recreated, and all
records for one Oracle workflow release together only after settlement. Ordered
nabla passivation/handoff or another authority change uses the same rule. Global
identity/history, retained data subject to ordinary process-death lifecycle rules,
and other processes' mappings are not erased by that map retirement.

The application library exposes connection construction, ordinary resource cleanup,
and a separate lifecycle envelope. These do not enlarge the eight ECLIPS
data-space operations.

#### Prepared-child lifecycle

EAPP contains the following bookkeeping vocabulary, independently of
`ApplicationOperation`:

```text
LifecycleCall(sessionId, lifecycleRequestId, command)
GetLifecycleResult(sessionId, lifecycleRequestId)
RecoverLifecycleResult(attachment, lifecycleRequestId)
LifecycleReply(lifecycleRequestId, status)
LifecycleAbsent(lifecycleRequestId)
LifecycleConflict(lifecycleRequestId)
LifecycleRetired(lifecycleRequestId, retiredThrough)

command = BeginChild(targetLocator, primordialSelection)
        | AwaitPreparedChild(childPreparation)
        | AwaitChildReady(childPreparation)
        | CancelChild(childPreparation)
        | EndOwnProcess
status  = LifecyclePending(requiredEndpointKeys)
        | LifecycleCompleted(result)
        | LifecycleRejected(error)
result  = ChildPreparationAccepted(childPreparation)
        | ChildPrepared(preparedChild)
        | ChildReady | ChildCancelled | ChildAlreadyAttached | ProcessEnded
```

The request identity contains the admitting session's opaque scope and ordinal
plus its lifecycle request ordinal. The Herald validates that full identity
against the established session, retains the exact command before effects, and
rejects different-body reuse with `LifecycleConflict`. Several sessions for one
process have disjoint lifecycle identities. Live-session queries inspect pending
or unretired terminal lifecycle IDs; an absent live query permits reoffering the
same retained command. A query at or below its retired frontier returns
`LifecycleRetired`, so it cannot be mistaken for new work. Lifecycle status has its
own correlation and retirement frontier, separate from ordinary request summaries
and reply cursors. Dead-session cleanup removes terminal receipts and pending
observation-only waits; accepted Begin/Cancel/End mutation work continues until its
semantic owner settles it.

`BeginChild` checks the exact selection and retains a `ChildPreparation` before
the target submits one stable generated Oracle Start. The target can be local or
an admitted remote Herald resolved through its advertised application contact.
Applying Start installs the child process, normal self-possession,
selected grants and aliases, and a checked normal process-name grant for the
parent. `AwaitPreparedChild` can then return the parent-private child process
handle and portable descriptor before readiness, so ordinary parent `label` calls
can make the selected endpoints operational. Preparation never launches an OS
process. Parent End leaves accepted preparation and child lifetimes independent.

Readiness requires the child live at the active target, its selection installed,
and every required endpoint current, normally possessed and operational under its
current child label. A required reader also needs current placement/incarnation
and completed alignment. Control and peer progress drive readiness without a child
call. Missing labels or alignment yield pending keys; a removed/ended required
object yields terminal startup failure. `ChildReady` is an observation, and initial
claim rechecks it atomically rather than treating it as a permanent capability.

The preparing parent or an authorized administration cancellation handle can seal
an unclaimed preparation. The target serializes this with initial claim. A claim
winner yields `ChildAlreadyAttached`; a cancellation winner seals subsequent claims
and returns `ChildCancelled` only after any required ordered End is applied.
Cancellation before an unsent Start prevents that Start; if Start may have been
accepted, its exact outcome is resolved before End-after-Start. Cancellation does
not roll back labels or publications. Cancelling a local waiter alone does not
cancel the lifecycle request or child.

`EndOwnProcess` retains the exact Oracle End intention and reaches `ProcessEnded`
only after ordered application. Its terminal effect is constructed before dead-session
receipt reclamation and offered before the session lane is disposed. A best-effort
`RecoverLifecycleResult` query on a fresh candidate lane uses the original
attachment and full lifecycle identity. This restricted query can return only a
still-retained own-End result: session termination reclaims terminal receipts.
The query cannot open/resume a session, allocate private IDs,
retrieve other lifecycle results, or issue another End. An unknown query returns
`LifecycleAbsent`, which is an unknown outcome, not proof that End succeeded. Each
candidate query sends one current result and closes; a pending outcome is polled
through the client's paced retry path.

The callable facade returns direct typed results through opaque `Herald` handles.
Its advanced submit/await/status form uses result-indexed calls and supports
explicit stable claim configuration and `recoverEndProcess`. Both forms share
the existing pure client and the same wire request identities.

For remote preparation, EPRP carries one retained offer, query, cancellation and
status protocol. The source freezes selected global identities, role and source
keys, historical source residence, current control/structural prerequisites,
checked sort definitions and retained first/latest object observations. No
parent-private handle crosses EPRP. The target waits for prerequisites, admits
the selected facts through SortRegistry and Controlled, and retains one generated
child and exact Start/End intentions. Selected canonical genesis metadata can be
observed without copying its historical owner's possession. Current labels,
deletion and End still govern the target's grant and readiness.

The source accepts the returned child name only after its own canonical Start
projection matches the target receipt. It then creates one parent-private alias.
Reoffers on a new peer binding retain the original identity and body; duplicate
status on the same binding does not create a retry loop. Parent End does not
erase accepted work, and target End or retirement settles retained waiters.

### 3.3 Calls and replies

The current application message union is:

```text
Call(sessionId, requestId, ApplicationOperation)
CallWithRetirement(sessionId, requestId, ApplicationOperation, receiptRetirement)
LifecycleCallWithRetirement(sessionId, lifecycleRequestId, command, receiptRetirement)
RequestRetained(sessionId, replyCursor, requestId,
                WaitAccepted(waitId) |
                OperationAccepted(OperationPendingReason) |
                Completed(semanticResult) |
                Rejected(semanticError) |
                Cancelled(waitId))
RequestAbsent(sessionId, requestId)
RequestConflict(sessionId, requestId)
RequestRetired(sessionId, requestId, retiredThrough)
GetRequestResult(sessionId, requestId)
CancelPendingWait(sessionId, requestId, waitId)
WaitWake(sessionId, replyCursor, requestId, waitId, WaitReady)
```

These calls join the handshake, lifecycle, heartbeat and receipt-retirement
messages described above in one closed EAPP sum. The nested operation and result
sums each have eight constructors:

```text
ApplicationOperation
  = NewIdApplication(NewIdTarget)
  | WriteApplication(PrivateNablaId, ApplicationWriteValue)
  | ForwardApplication(PrivateNablaId, PrivateObjectId)
  | ReadApplication(ApplicationQuery)
  | LocalTakeApplication(ApplicationQuery)
  | WaitApplication([ApplicationQuery])
  | LabelApplication(PrivateObjectId, ApplicationLabel, ApplicationLabelTarget)
  | NewEnvironmentApplication

NewIdTarget
  = BareNewId
  | ControlledNewId(PrivateNablaId)

ApplicationWriteValue
  = PublishValue(ApplicationValue)
  | DeleteReserved(PrivateObjectId)

RegularCallResult
  = NewIdCompleted(PrivateUniqueId)
  | WriteCompleted(WriteResult)
  | ForwardCompleted(ForwardResult)
  | ReadCompleted([ApplicationValue])
  | LocalTakeCompleted([ApplicationValue])
  | WaitCompleted(WaitResult)
  | LabelCompleted(LabelResult)
  | NewEnvironmentCompleted(EnvironmentAccess)

WriteResult
  = SortDefinitionWritten(SortId)
  | WriteAccepted
```

Both write payload arms are executable; `WriteAccepted` is deliberately
payload-free. Generic `Binary` derivation owns the current-build constructor
discriminators and field encodings; no numeric values or primitive layout are
specified independently. The same EAPP family, frame, envelope, session owner, and
TCP connection carry this sole schema. Earlier five- and seven-call schemas remain
historical checkpoints and have no compatibility decoder.

A message decoder consumes its complete declared family body. Bytes after one
complete frame begin the next coalesced frame. A streaming end-of-input operation
rejects any retained partial length, header, or body as truncated; before
end-of-input, such a prefix merely waits for more bytes. The current sums contain
no future or unsupported constructor.

`receiptRetirement` contains independent optional inclusive ordinary-request and
lifecycle-request frontiers. `Nothing` means no prefix is released; zero is a
valid frontier. The client promises that it has consumed terminal results and
will never retry, query or cancel an ID in the advertised prefix. Its frontiers
stop before the earliest pending request. They use request sequence numbers,
not reply cursors, so a late low request remains protected even when later
replies have already arrived.

A later ordinary or lifecycle call can carry both frontiers; `RetireReceipts`
flushes the same progress when the client is idle. The Herald checks the live
session and both prefixes atomically. A prefix covering pending work returns
`ApplicationReceiptRetirementNotReady` with no retirement or attached call
admitted. Otherwise the Herald monotonically merges progress, removes covered
terminal receipt records and copied resume entries, and then admits any attached
call. `ReceiptsRetired` confirms merged progress when it advances or when the
request is standalone. Exact or regressed metadata does not change the retained
call identity or repeat semantic work.

Covered ordinary calls, result queries and cancellations return `RequestRetired`;
covered lifecycle calls and queries return `LifecycleRetired`. This distinction
also covers previously unused IDs in a released prefix: an absent receipt cannot
reopen old work. Retirement releases reply payloads, not accepted publication,
preparation, label or Oracle work. A child preparation retains checked Begin
provenance independently of its retired acceptance receipt.

The client transfers terminal results into caller-owned handles before offering
retirement. Shared client/runtime correlation maps retain pending work only;
strictly increasing local invocation IDs and compact high-water/frontier facts
reject reused identities without a lifetime-sized tombstone map. Caller-retained
handles can still be inspected after protocol receipts are reclaimed.

Application request identity is the retained typed operation itself. Exact retry
uses its `Eq` instance; no serialized-byte comparison, application-call hash, or
second digest is retained or recomputed.

A label call blocked by an earlier unsequenced structural stage has a narrower
pre-acceptance request record.
`Application` reserves the exact `(SessionId, RequestId)` and retains the complete
typed label call solely for duplicate/conflict classification, but allocates no
process acceptance position, reply cursor, semantic request status, fence, decision
ID, or Oracle intention. Exact call retry or `GetRequestResult` while the logical
session remains live reattaches reply interest and otherwise waits for the same
candidate to leave pre-acceptance; it does not return `RequestAbsent` or emit
`OperationAccepted`. Different typed reuse returns `RequestConflict`. Once its
structural precondition clears, the ordinary validation transition either rejects
the call or atomically promotes that same request identity into the accepted
label/fence/Oracle workflow and emits its first retained status. `EndSession` or
applied process end discards the candidate with the session; a later retry receives
the ordinary non-live-session result and cannot resurrect it.

A qualifying publication accepted after the home label cut owns its process
position and complete selected work item while it waits for local decision
installation. The protocol adds no generic visibility-fence pending reason: if
the call has not reached one of its ordinary retained statuses, exact retry/result
lookup waits and the resume summary contains no invented status. Session/process
retirement removes request/reply reachability while accepted work continues.
Independent calls, Store observations, wait evaluation and Oracle traffic remain
executable. There is no all-application label gate or gated semantic-ingress queue.
Decision installation re-drives selected positioned work against the canonical
outcome before later qualifying publication advances.

The current operation payload union has exactly eight constructors.
Controlled/regular, definition/application, and reservation-cancellation variants
are decoded behind the single `write` constructor and its closed
`PublishValue(ApplicationValue) | DeleteReserved(PrivateObjectId)` sum. `DeleteReserved` is accepted only for the
exact pristine `Reserved` state from `newid(p)` named by the same private nabla,
with no staged instantiation or Oracle intent; it is not a generic label/value
constructor and cannot occur in a regular write, update, query, result, or peer
DTO. Every application-value unique-ID field recursively
contains a `PrivateUniqueId`, and
all process/object/nabla/delta operands use their nominal private handle type. The
Herald globalizes the complete admitted `PublishValue` through the process map
before semantic validation; the cancellation arm resolves only its typed operands
and never constructs an internal `Value`. Results from `read`, `local-take`, `newid`, `newenv`, and any
later value-bearing result are recursively localized before the response and any
new bindings commit atomically with that result. No application DTO admits a raw
`GlobalUniqueId`, `GlobalObjectId`, `ProcessEpochId`, `NablaId`, or `DeltaId`.
No application or peer DTO admits a `GeneratorSeed` or generator counter.
Application reply/wake effects already contain the fully localized DTO; the
runtime must not perform identity translation. Application-visible errors and
diagnostics may carry a private ID or redacted digest, never a global ID.

The same application vocabulary includes `EnumSchema(NonEmpty Text)`,
`EnumValue(Text)`, descriptor `LiteralEnum(Text)`, and query `QueryEnum(Text)`.
For an admitted schema the members are pairwise distinct, exact, and
declaration-ordered; no codec case-folds or normalizes their text. Schema-directed
descriptor, publication, and query admission prove membership at each use, and the
owning schema's member ordinal defines enum comparisons, object-key order, and
explicit enum rank fields. Reordering an enum schema is therefore a descriptor and
`SortId` change.

The current-build application schema has exactly three semantic-operation
pending reasons:

```text
OperationAccepted(StructuralStabilizationPending)
OperationAccepted(LabelSettlementPending)
OperationAccepted(EnvironmentStabilizationPending)
```

They are inner `RequestRetained` statuses alongside `WaitAccepted`, `Completed`,
`Rejected`, and `Cancelled`; none is an application operation, and they carry
no `PublicationId`, global ID, Oracle request ID, or cancel token. The existing
`RequestId` is the sole application correlation. A structural `PublishValue`
enters `StructuralStabilizationPending` once its local first-use/publication
identity is accepted, including while it remains an unsequenced structural stage.
`GetRequestResult`, exact call retry, resume summary, and the pure client reoffer
that current retained status until the same request advances once to its terminal
`Completed(WriteCompleted(WriteAccepted))`. A terminal state never returns to
pending; an older issued
pending cursor reoffered after terminal observation is accepted only as the
already-known predecessor state. The host-language library exposes one blocking or
future-valued call and does not expose these protocol phases as extra operations.
Terminal request receipts can be released by their consumed prefix while the
session is live. `EndSession` or established binding loss removes all session
request/reply reachability, while the self-contained
publication/structural-stabilization owner continues by `PublicationId` and
occurrence identity; it does not recreate a reply in another session. An
accepted `label` enters `LabelSettlementPending` while its selected preceding
publications settle and its retained Oracle workflow and visibility barrier
progress, then reaches exactly one
`Completed(LabelCompleted LabelApplied|LabelNotApplied)` terminal. An accepted
`newenv` enters `EnvironmentStabilizationPending` and advances only to
`Completed(NewEnvironmentCompleted EnvironmentAccess)` after one established cut
covers the complete retained manifest. No dormant or compatibility pending arm is
encoded.

The current `forward` call has the one payload-free terminal result
`Completed(ForwardCompleted(ForwardAccepted))`. Ordinary forward may reach it at
local acceptance. Structural forward uses the already executable
`OperationAccepted(StructuralStabilizationPending)` phase and advances to that
terminal result only after a covering established cut is installed at home. No
parallel forward operation/result constructor exists.

Application process/zombie label arms, including typed label-schema query
literals, carry `PrivateProcessId`; void carries no ID. The Herald resolves the
private ID and statefully validates the process role before constructing an
internal `ProcessEpochId`. Results localize an established internal process label
through the same process map. `ProcessEpochId` never appears in an application DTO.

The application `sort-sort` DTO carries a structured
`ApplicationSortDescriptor` and an optional claimed public `SortId`; it never
carries caller-declared canonical descriptor bytes. Stateful Herald admission
converts the structure to the semantic descriptor AST, validates it, obtains the
sole canonical bytes from the semantic domain, derives the `SortId`, and checks an
optional claim. A successful write completes as
`RequestRetained(sessionId, replyCursor, requestId,
Completed(WriteCompleted(SortDefinitionWritten(computedSortId))))`; that branch of
`WriteResult` carries the public `SortId` derived by the Herald and is retained
unchanged for exact request retry. Read and take results likewise materialize the
computed identity in their structured definition values. This is a closed
constructor of ordinary application value data, not another RPC operation or a
second canonical codec.

The `DeleteReserved` write DTO is the boundary encoding of an initial controlled
label of delete. Successful admission makes the delete branch the single winner,
consumes only the matching pristine Herald-local reservation, retains its
private/global alias, records `WriteCompleted(WriteAccepted)`, and emits no
semantic value, publication, Oracle command, or peer work. First controlled
`PublishValue` instead consumes the pristine reservation into one admitted,
retained direct publication. The two branches have one serialized winner; exact
retry follows its retained result. Publication adds no Oracle intention or pending
Oracle phase. A decoder never turns
`DeleteReserved` into the domain's persistent `Label`, whose closed value grammar
is an owner/generation pair whose owner remains process/zombie/void.

The Herald keeps the original admitted typed call and monotone request state for
each `RequestId` for the life of the session:

- exact retransmission returns or continues the current retained pending/terminal
  state;
- reuse with a different typed call returns `RequestConflict` without semantic
  admission;
- a call rejected before semantic acceptance has an immutable rejection result;
- a lost `Completed` frame does not roll back an accepted publication or label;
  and
- `GetRequestResult` is bookkeeping for reconnect and is not a ninth operation.

Bare and controlled `newid`, `read`, `local-take`, and ordinary rejections
produce their own terminal result. `DeleteReserved` completes at local acceptance.
Regular and controlled writes and `forward` enter their retained
direct-publication workflow at their local acceptance point. Ordinary data writes
and forwards may complete there. A structural-carrier write or forward completes only after the
all-member structural stabilization in the captured generation defined in section
7.4; it does not wait for
context alignment to finish. `newenv`, structural publication, and `label` first
send their exact `OperationAccepted` status and finish later. A pending `wait`
specifically sends
`RequestRetained(sessionId, replyCursor, requestId, WaitAccepted(waitId))`, so the
client can name the opaque occurrence in `CancelPendingWait`; a client cannot mint
a `WaitId`. The application-facing
library presents one blocking or future-valued function and hides this distinction.

Cancelling a client thread waiting on `wait` sends `CancelPendingWait`. Cancellation
only removes the internal registration; it does not undo another semantic effect.
`wait` may race cancellation and return a spurious wake, which the application must
already tolerate.

Cancellation and result lookup use the separately decoded `sessionId` as well as
the established logical binding. A binding/session mismatch rejects that
connection and changes no owner. A covered retired request returns `RequestRetired`;
an unknown request above the frontier returns `RequestAbsent`; an
already terminal request reoffers its retained terminal reply; a pending wait
with the wrong `waitId` returns `RequestConflict`; and only the exact pending
tuple transitions to the retained `Cancelled` reply. Reusing a retained
`requestId` with a different typed call likewise returns `RequestConflict` and
changes no owner. In profile 0.1 these two explicit bookkeeping conflicts are
application protocol outcomes rather than automatic connection-close events.

### 3.4 Unknown outcomes

An established connection failure ends that application session. Any request
without an observed terminal result has a transport-level unknown outcome; the
library does not resume the dead session or silently repeat the operation under a
new request identity. Accepted semantic work may still complete at the Herald.
The application can restart with a new process lifetime and reconstruct its work
from semantic state.

`UnknownOutcome` is a client/library result, distinct from semantic `NotApplied`.
An explicit terminal Herald disposition or authoritative `SessionNotLive` can
produce the same outcome. A delayed or unobserved reply on a still-live binding
can be recovered by exact request retry or result query while its receipt is
unretired. Startup connection-attempt retries before a session is learned remain
paced and keep their stable attachment/claim identity. Restricted own-End lookup
is the best-effort exception described above; it does not restore a session or
promise retention after its lifetime ends.

### 3.5 Administrative separation

The application-facing lifecycle envelope can prepare a local or remote child, cancel its
own unclaimed preparation, and end its own process. Arbitrary process-epoch End
and supervisor Start remain on a distinct Herald-administration connection,
authorized by deployment configuration in the benign profile. Its current family
carries:

```text
AdminHello(deploymentId, role)
StartProcessEpoch(correlationId)
EndProcessEpoch(correlationId, processEpoch, explicitReason)
CancelChildPreparation(correlationId, childPreparation)
GetAdminResult(correlationId)
GetHeraldStatus(correlationId)
ListChildPreparations(correlationId)
DrainHerald(correlationId)
PrepareOracleReplica(correlationId)
BeginVoterChange(correlationId, expectedConfigurationId, explicitReason, desiredBindings)
CancelVoterChange(correlationId, changeId)
GetOracleConfiguration(correlationId)
GetVoterChangeStatus(correlationId, changeId)
AdminResult(correlationId,
            AdminAccepted |
            AdminCompleted(result) |
            AdminRejected(error) |
            AdminPreparationCancellation(lifecycleStatus))
AdminResult(correlationId, AdminOracleVoterResult(canonicalOriginalOracleReceipt))
AdminAbsent(correlationId)
AdminConflict(correlationId)
HeraldStatus(correlationId, snapshot)
ChildPreparations(correlationId, preparations)
HeraldDrainAccepted(correlationId)
HeraldDrained(correlationId)
OracleConfiguration(correlationId, appliedControlIndex, configuration?, registrations, pendingChange?)
VoterChangeStatus(correlationId, appliedControlIndex, retainedChange?)
```

Voter administration is a closed operator vocabulary. Preparing a replica
names only the selected local active Herald: its native node identity and concrete
advertised contacts are configured by the runtime and admitted through its owner,
never supplied as authority by the caller. Begin names the expected configuration,
explicit commission/demotion reason, and complete desired node/host binding set;
ordinary Oracle admission checks those claims. Mutation correlations retain the
original canonical Oracle receipt unchanged. Later configuration/change queries
return separately projected workflow progress and the local applied control
prefix. Canonical Oracle codecs carry these checked reply values directly rather
than defining an independent voter schema in EADM. Neither a local status reply
nor an advertised registration grants voting rights or establishes quorum health.
An ordinary Herald may use zero listen ports, but replica preparation requires
concrete Oracle and Raft advertised/listen ports. Missing local endpoint facts
return the explicit `OracleReplicaEndpointsNotConfigured` administration refusal
before creating an Oracle operation.

The terminal administration payload is a closed per-command sum:

```text
AdminCompletedResult =
    StartProcessReady(processEpoch, attachmentMaterial) |
    StartProcessEndedBeforeAttachment(processEpoch) |
    ProcessEpochEnded(processEpoch)

AdminRejectedError =
    StartProcessNotApplied(oracleReason) |
    EndProcessNotApplied(oracleReason)
```

Administration includes status, preparation inspection and drain. A drain request
returns acceptance followed by a final correlated receipt; the runtime flushes it
before joining its scopes. Drain does not order voter removal.

Status reports current lineage, local epoch, membership generation and members,
control position, voter hosts, service phase and the Oracle contact/leader hint.
Preparation inspection returns stable administration references, optional global
child epochs and current phases/failures; it exposes no application-private
handles. These reads are owner snapshots rather than fresh Raft leadership reads.

A pre-acceptance administration error has no Oracle effect. An accepted Start/End
command whose exact Oracle workflow settles
`NotApplied` advances monotonically from `AdminAccepted` to its command-specific
`AdminRejected`; exact retry and lookup reoffer that terminal result. A successful
End advances to `AdminCompleted(ProcessEpochEnded(processEpoch))`.

`CancelChildPreparation` names the same opaque source-acceptance handle available
to the parent before `PreparedChild`. Its retained
`AdminPreparationCancellation(LifecyclePending | LifecycleCompleted |
LifecycleRejected)` status uses the same pure cancellation owner and claim winner
as EAPP. Administration connection loss cannot revoke the seal or accepted End,
and exact retry/`GetAdminResult` returns the retained outcome. Cancellation is
available even when the parent session or parent process has ended.

The administrative `EndProcessEpoch` request proposes the matching Oracle command.
EAPP own-process End and cancellation of an unclaimed child use that same ordered
process-death path. The Application owner's automatic-loss transition may retain
the command internally, and committed Herald retirement derives resident-process
End at its retirement index. The administrative family
has a different preface and codec union from application RPC, and the application
client package does not depend on it.

The current administration frame discriminator is `0x4541444d` (ASCII `EADM`).
Like the other family magics it identifies the closed message family rather than
an API or protocol version.

`StartProcessEpoch` accepts a stable correlation at the selected active Herald.
The pure target owner generates and retains one process/epoch identity before
submitting the minimal Oracle Start. Exact retry reuses that retained identity.
After the applied Start, the target installs the process scaffold, its normal self
possession and selected startup access. It does not publish a new full root bundle.
The initial launcher remains an explicit checked genesis assignment.

`correlationId` is a caller-supplied opaque administration correlation scoped to
the Herald runtime. The Administration owner retains the complete typed request,
its canonical digest, and its monotone accepted/terminal status before emitting
`AdminAccepted`. Exact retry or `GetAdminResult` on the same or a later authorized
administration connection reoffers the current status; reuse with a different
typed request returns `AdminConflict`. Connection loss removes only reply
reachability and cannot cancel an accepted process-lifecycle workflow.
`GetAdminResult` for an unknown correlation returns nonbinding
`AdminAbsent(correlationId)`: it creates no request record, consumes no acceptance
position, and cannot prevent a later first use of that correlation.
`StartProcessEpoch` advances from `AdminAccepted` to
`AdminCompleted(StartProcessReady(processEpoch, attachmentMaterial))` only after
its applied Oracle result and local startup installation. An applied End wins over
subsequent attachment installation and yields the retained ended result. Accepted
Oracle work survives connection loss; it cannot allocate another identity on retry.
Application possession and source-writer selection are admitted separately by the
Herald. `newenv` creates twelve caller-labelled endpoints using the selected
operational Nabla/Delta writers, then its own fresh writers create a hub and
eighteen caller-labelled edges, without an Oracle submission.

## 4. Peer discovery and connection establishment

### 4.1 Seed dialing

Each Herald configuration contains:

- its `SystemId`, `HeraldId`, fresh `HeraldEpoch`, and advertised peer endpoint;
- zero or more seed peer addresses;
- Oracle client seed addresses; and
- for a voter, its separate Raft configuration.

The Herald attempts every seed with retry and backoff. The seed list is a
set of possible contacts, not a complete roster and not proof that an endpoint is
alive.

The runtime uses these configured endpoints to make the first contacts. After a
handshake, admitted known-Herald contacts may also cause a direct dial, but only
through the explicit owner-authored connection intentions described in Section
4.3. The runtime MUST NOT inspect an unadmitted contact DTO or private Discovery
state to invent a dial. Seed and learned-contact dials establish the same peer
protocol connection; neither introduces publication forwarding.

### 4.2 Peer handshake

Before peer messages are admitted, both sides exchange:

```text
PeerHello {
    systemId,
    heraldId,
    heraldEpoch,
    connectionNonce,
    advertisedPeerAddresses,
    advertisedApplicationContact,
    appliedControlIndex,
    membershipGenerationId,
    activeMemberSetDigest,
    predefinedCatalogueDigest,
    initialProjectionDigest
}
```

The optional application contact is admitted only with the active peer identity.
Discovery retains it across binding loss and removes it from resolution on
retirement. Runtime configuration supplies the local advertised contact; socket
binding and advertised endpoints may differ. Preparation resolution uses this
contact to choose an admitted peer and sends EPRP over the peer connection.

The connection is rejected when the system lineage or catalogue digest differs.
`initialProjectionDigest` canonically binds both the complete applied index-zero
process-bootstrap records and the normalized checked initial Graph projection,
not merely their manifest references or presentation order. The Graph contribution
contains resolved typed vertices and universal edges; it does not encode raw
startup-manifest references, placement advertisements, addresses, connection
state, or placement sequence. A mismatch means two benign Heralds started from
incompatible fixtures; the observing Herald terminates the run before admitting
peer data rather than attempting mixed-projection operation or recovery.
An equal `SystemId` does not itself grant active membership. Data-plane messages
are admitted only after the local Oracle projection contains the exact active
`(HeraldId, HeraldEpoch)`.

The Hello's membership generation is a synchronization claim, not authority to
change membership. A peer absent or retired in the receiver's current projection
is rejected. If both endpoints are active but one claims a known predecessor
generation, or the remote claims a higher `appliedControlIndex` whose generation
the local endpoint has not yet replayed, the binding remains recovery-only while
the lagging endpoint catches up through EORC. Semantic peer traffic begins only
once both claims name the same active generation/member digest. After replay, a
surviving peer is admitted and a retired one is rejected. An unknown generation at
an equal/lower applied control index, or a generation that is not in the replayed
predecessor chain, is divergent and rejects rather than falling back to genesis.

Simultaneous dialing may create two connections. Both sides compare the fully
exchanged tuple `(initiatorHeraldId, initiatorHeraldEpoch, connectionNonce)` and
retain the lexicographically smaller live connection for that peer epoch. Closing
the other connection changes no logical stream or semantic work.

An accepted Hello establishes the logical binding and emits exactly the
establishment roots in canonical order: the complete known-Herald set, the latest
retained cut-qualified full placement snapshot, and the bidirectional
`PeerStreamResume` offer. It does not eagerly dump every structural or alignment
reply retained by the owner. After admitting the first valid remote resume offer
on that accepted binding, the receiver emits `PeerStreamResumeAccepted` before
scheduling any dispatch ticket newly enabled by that claim and before emitting
the structural and alignment repair catalogues. Later offers for the same binding
still receive `PeerStreamResumeAccepted`, but an unchanged canonical raw gap claim
does not re-enable work already written on that association. Changed gap evidence
may enable only the exact retained items newly requested relative to the preceding
raw claim. Later offers do not replay the reconnect
catalogues. A later accepted replacement or reoffered Hello resets this binding-
owned first-offer fact. Thus the initial stream claim is the causal boundary for
the later repair controls on that binding.

### 4.3 Complete known-peer exchange

Immediately after `PeerHello`, and whenever its known set grows, each Herald sends:

```text
KnownHeralds {
    contacts: [{ heraldId, observedHeraldEpoch, peerAddresses }]
}
```

The receiver admits this control only on its exact current source binding and
unions new addresses into its discovery state. An equal reoffer is effect-free. If
the union grows, the same transition sends the enlarged complete set to every
other current peer binding, deliberately excluding the binding that supplied the
update, and prepares any newly eligible dial intentions. Its canonical effect
batch sends those peer controls first and orders the later dial effects by peer
identity. The source already supplied the new facts and receives this Herald's
complete set through its own accepted-Hello exchange or an earlier local growth
propagation; echoing the just-received union back to that binding would add no
repair information. A later accepted or reoffered Hello reoffers the complete
current set, so recovery requires no periodic timer.

For each non-local contact whose exact `(heraldId, observedHeraldEpoch)` agrees
with the applied active-membership projection, whose address set is non-empty, and
for which no current binding exists, Discovery emits an explicit owner-authored
intention to maintain a direct peer connection.

The runtime coalesces repeated intentions for one exact peer epoch, replaces that
physical job's endpoint set with the complete latest retained union, suppresses
dialing while a selected binding for that epoch is live, and resumes after its
loss. The job ends on drain. The first owner-authorized intention for an exact
active peer that is known and unbound starts that peer's fixed absolute recovery
episode. Individual physical failures only advance its paced retry schedule: they
do not renew the deadline or directly change membership, placement, or publication
state. A successful current-generation handshake cancels the episode. Selection
among the supported numeric TCP endpoints remains prototype runtime policy.
DNS discovery, multi-address racing, health scoring, and production backoff
remain outside this profile.

Discovery records are hints only:

- learning an address does not admit a Herald to the Oracle;
- a successful TCP connection does not make a process or delta active;
- a disconnect does not prove Herald failure or remove a route;
- the sender of a contact record is not trusted to change another Herald's epoch;
  and
- a contact becomes data-plane-authoritative only when its exact epoch appears in
  the applied Oracle membership projection.

Under fair dialing and eventual connectivity, the union of all initially known
addresses converges at every connected Herald.

## 5. Active delta placement

Peer discovery says where a Herald may be contacted. Placement says which physical
delta stores it currently hosts. These are separate streams.

A hosting Herald publishes sequenced complete snapshots. The typed semantic
vocabulary is:

```text
DeltaPlacementSnapshot(heraldEpoch, placementSequence, [DeltaRoute])
DeltaPlacementAck(heraldEpoch, placementSequence)

DeltaRoute =
    ApplicationDeltaRoute {
        deltaId,
        sortId,
        sortDefinitionOccurrenceId,
        controllerObjectId,
        processEpochId,
        storeIncarnationId,
        controlPrerequisite
    }
  | PrivateSystemViewDeltaRoute {
        predefinedSortRole,
        deltaId,
        sortId,
        sortDefinitionOccurrenceId,
        storeIncarnationId
    }
```

`placementSequence` is positive. The first full snapshot is sequence one, even
when its route set is empty. At most one route in a snapshot may name a given
`DeltaId`; presentation is normalized by ascending `DeltaId` before equality or
admission.

The host advertises only active stores it owns. For one `HeraldEpoch`, a full
snapshot at sequence `v` atomically replaces every older cached route from that
owner. `placementSequence` advances only when that authoritative **physical route
set** changes. Retransmission of the same logical set reuses the same sequence. The
owner sends its latest retained full snapshot on every accepted Hello/binding
establishment and sends a new one when an installed cut changes that set. Because
every placement message is complete, a newer snapshot may skip unseen sequence
values and immediately replace the older owner projection atomically. An
acknowledgement confirms the latest owner sequence admitted by that receiver and
cannot change placement.

Structural application may advance the host's live local Store/placement projection
while its covering topology cut is still open. That live-ahead projection is not an
advertisement boundary. The owner advances `placementSequence` only after installing
the all-member topology cut, from the exact route set reconstructed at that cut:
genesis reader roots and private system views plus the cut's active dynamic
Delta occurrences and their occurrence-specific Store resources. It MUST NOT derive
that snapshot by filtering the current live projection. Accepted Hello/binding
establishment reoffers the last retained cut-qualified snapshot without consulting
live-ahead routes. When a newly installed cut advances the snapshot, its peer-send
effects precede terminal settlement notifications for structural operations covered
by that cut.

Topology and placement cuts remain independent: no topology-cut field is added to
the placement stream. Installing the qualifying topology cut proves that every
member of its named membership generation has already applied each structural
occurrence needed to validate the snapshot. A receiver therefore authenticates a
dynamic application route against retained applied occurrence history even if its
current graph winner has advanced; genesis reader roots continue to validate
against their checked startup coordinates. This retained-history precondition,
rather than cross-connection TCP ordering, makes the independent placement
sequence admissible.

Certificate readiness is not a second placement stream. The generation-qualified
`ClassMemberReady` and immutable `HistoricalCertificate` evidence defined in
[section 9.1](#91-store-revisions-and-class-generations) may change source
eligibility, but never `placementSequence`, `AlignmentCut`, or an existing class
generation identity. Such evidence is admitted only for the exact store incarnation
and generation it names.

A receiver validates an application route's sort, exact sort-definition
occurrence, controller, process residence, and control prerequisite against its
own checked startup coordinate or retained applied structural occurrence. The
hosting Herald remains authoritative for a fresh physical store incarnation of
that checked reader root. A private
system-view route is a distinct closed arm: its delta, sort, genesis occurrence,
and store incarnation are derived from `(SystemId, owner HeraldEpoch,
PredefinedSortRole)`. It carries no fabricated controller, process, application
capability, or control-prerequisite field.
Definition-publication provenance remains owned by the sort registry; copying its
current winner into placement would add no route meaning and would couple an
unchanged physical route to equal-definition winner changes. Normal possession and
the existence of the physical store are host-local facts: under the benign
trusted-Herald profile, the exact hosting Herald is authoritative for those facts
and for `storeIncarnationId`. A remote Herald MUST NOT pretend to re-derive
host-local possession.

A route is withdrawn by its omission from a later complete snapshot from the same
owner; that replacement retires the exact previously advertised store incarnation.
Connection loss alone does not withdraw a placement. A later activation of the same
`DeltaId` appears in a later snapshot with a fresh store incarnation.

The accepting Herald's routable destination set is the intersection of:

1. active deltas of the publication's sort reachable through its local graph's
   universal directed edges;
2. current placement records admitted at its current local cut; and
3. the corresponding current reconciled process/label/authority projection. A
   later label assignment to a different live process is Oracle-ordered and
   derives its authority from the release index; a dynamically first-published
   nabla's initial authority instead comes from its established structural cut.

Every edge admits every sort in the prototype. Nablas, deltas, placements, and stores
remain single-sort, and preserving/weakening edges still determine routed strength.
Routing evaluates no edge predicate. None of the placement, publication, or
alignment DTOs contains an edge filter.

Temporary disagreement is allowed. New routes missed by a live publication are
made whole by later context alignment.

## 6. Peer logical streams

For each non-self `(sourceHeraldEpoch, destinationHeraldEpoch)` direction, the
source owns one ordered logical stream. The key contains no physical connection
identity, so it survives replacement TCP connections while both Herald epochs
remain alive. A new Herald epoch denotes a different stream and profile 0.1 has no
restart-recovery protocol for the old one.

`StreamSequence` is positive: allocation begins at one and advances contiguously.
Absence of acknowledged work is never encoded as sequence zero. It has an explicit
prefix arm:

```text
StreamPrefix = EmptyStreamPrefix | StreamPrefixThrough(StreamSequence)
```

`EmptyStreamPrefix < StreamPrefixThrough(1) < ...`; the successor of empty is
sequence one. Publication batches, alignment route cutovers and disappearance
markers share this ordering. Placement gossip and connection heartbeats do not.

The logical gap claim is an exact typed transcript:

```text
GapSummary {
    direction,
    receivedPrefix,
    entries: ascending [(StreamSequence, PeerItemDigest)]
}
```

`PeerItemDigest` is an exact 32-byte nominal item identity. The entries are unique,
strictly above the first missing sequence, and name every retained out-of-order
item and no contiguous item. Direction, the tagged prefix, each positive sequence,
and each complete digest participate in equality. The digest derives from the
complete immutable concrete peer item. EPRP directly encodes this exact transcript. It adds no redundant wire `gapDigest`; framing bytes, map
presentation order, and payload `Show` output are not semantic identity.

Resume is a bidirectional peer-pair exchange. An offer sent by `S` to `D` is:

```text
PeerStreamResume(S, D,
    nextSourceSequence,              -- S's outgoing S -> D frontier
    receivedPrefix,
    completion: ReceiptRetirement,
    gapSummary)                      -- S's incoming D -> S state

PeerStreamResumeAccepted(
    receivedPrefix, completion,      -- D's incoming S -> D state
    retransmitFrom)

PeerStreamFrontierAdvanced(
    direction, nextSourceSequence)   -- one-way S -> D allocation frontier
```

The frontier and receive claim deliberately describe opposite directions. Both
peers send an offer, so both directions are reconciled symmetrically. On receipt,
`D` first checks that every item it has retained for incoming `S -> D` is strictly
below `S`'s offered allocation frontier. It retains the greatest frontier ever
advertised by that live source epoch; a later lower frontier is a protocol fault
even if the missing tail never arrived. Once established, an incoming item at or
beyond that frontier is likewise a protocol fault until a valid equal-or-greater
frontier advances it.

A successful source enqueue sends `PeerStreamFrontierAdvanced` before the
corresponding dispatch ticket. This one-way control checks the live binding and
monotonically raises only `D`'s incoming `S -> D` frontier. It emits no response
and neither inspects nor reactivates `D`'s reverse outgoing stream. The binding's
single writer preserves the control-before-item order. If no binding exists, or
that writer fails, the next accepted Hello sends the full current resume offer
before enabling retained dispatch work. Thus post-handshake allocation advances
the contract without turning an ordinary enqueue into bidirectional
reconciliation.

Full resume remains the establishment/reconnect and exact gap-repair operation.
For it, `D` reconciles the offered reverse `D -> S` receive state against its own
outgoing history. The gap direction and prefix must equal the offer; every entry
must be unique, strictly above the first missing sequence (the successor of the
received prefix), below `D`'s next sequence, and carry the digest of the retained
active item at that sequence.

Completion is a downstream-certified sparse release set: `ReceiptRetirement(H,
E)` means every positive sequence through `H`, except the pending sequences in
`E`, is semantically complete. The receiver sets `H` to its contiguous received
frontier. Thus even a completely pending interval certifies receipt, and every
exception is backed by retained pending work. The cumulative `completedPrefix`
used by causal barriers is derived from the first exception, or `H` when none
remain. There is no sequence-zero item or exception.

The source merges completion by released-set union. An older completion snapshot
cannot reopen a released position, including when its exception set is stale.
Its separately recorded received watermark is also monotone. A raw receive/gap
claim may still request repair of an active item; it cannot reconstruct an already
completed item. Subject to the same-association exact-repeat rule below, a valid
resume selects active items above the offered received prefix and absent from its
exact gap summary, in ascending sequence order. It reuses retained typed payloads.

| Claim about `D -> S` | Result |
| --- | --- |
| completion high water is above received | protocol violation |
| either receive frontier is beyond `D`'s `lastAllocated` | protocol violation |
| completion is older or contains stale pending exceptions | merge release sets; never recreate released payloads |
| newly certified completion is locally possible | release those active payloads and their dispatch/retry correlations |
| received is below the recorded received prefix | accept it as a request to repair still-active work without lowering receipt or completion |
| gap direction or prefix differs from the offer | protocol violation |
| a gap entry is structurally invalid or an active item's digest disagrees | protocol violation |
| a gap entry names an already released position | stale evidence; no payload or receipt is recreated |
| a valid active gap entry | retain its receipt suppression and exclude it from routine retransmission |

The first admitted resume offer on an accepted binding reactivates every exact
unconfirmed retained item requested by the peer claim, including items suppressed
after a successful write on the previous association. A later offer on that same
binding still validates and reconciles prefix progress, completion, and exact gap
entries, but repeating the same canonical raw gap evidence does not clear
successful-write suppression or mint another dispatch attempt. A changed raw gap
claim reactivates only `currentRequested \\ previousRequested`: prefix advance or
added gap confirmations do not replay the still-missing suffix, while prefix
regression or removing a gap confirmation reactivates exactly the newly requested
items. Binding replacement or a Hello reoffer restores first-offer treatment for
the new association. Its first offer reactivates work retained before that
accepted/reoffered Hello. An attempt selected or successfully written after the
Hello is already repair work within the new association: the peer may have
produced its Resume snapshot before receiving that attempt, so the first offer
MUST preserve it instead of allocating another write on the same connection.
The owner captures the existing dispatch-attempt ordinal at Hello admission to
distinguish these cases, including a reoffer on the same binding. Later changed
gap evidence retains its ordinary repair semantics.

The accepted `retransmitFrom` is the successor of `D`'s received prefix for forward
direction `S -> D`. When no allocated forward item is missing it equals the
offered `nextSourceSequence`; it is a cursor, not proof that such an item exists.
Admitting the peer claim supersedes any selected physical dispatch attempt whose
sequence is confirmed through the prefix or as an exact gap. The owner retains
that receipt suppression even when normalization removes a raw gap beneath or
adjacent to its monotone received cursor. Superseding a callback cannot make
already-confirmed work eligible again; later raw-prefix regression or gap removal
still requests its exact repair. Missing work requiring repair becomes eligible
under a fresh attempt; the first-offer rule
above preserves attempts already selected or written since Hello. A later
written/deferred/failed callback for the superseded attempt is stale and changes
neither the peer-confirmed gap nor retry eligibility.
An exact repeat against the same predecessor produces the same state and response.
PeerStream owns the association phase and therefore preserves successful-write
suppression when other stream transitions occur between physical resume attempts.

Ordinary acknowledgements name one exact outgoing direction. `lastAllocated` is
computed from the independent next-sequence allocator, even when no payload or
completed receipt remains.

| Input | Condition | Result |
| --- | --- | --- |
| `StreamReceivedAck(p)` | `p > lastAllocated` | protocol violation; no successor |
| `StreamReceivedAck(p)` | `p <= receivedPrefix` | stale or exact duplicate; no-op |
| `StreamReceivedAck(p)` | later locally possible receipt | advance received; retain payloads and suppress routine sends through `p` |
| `StreamCompletedAck(H,E)` | malformed positive sequence set or `H > lastAllocated` | protocol violation; no successor |
| `StreamCompletedAck(H,E)` | locally possible | union the certified release set; advance receipt through `H`; discard completed payloads and obsolete retry/gap associations |

Completion controls carry the sparse retirement metadata directly. A semantic
completion emits the latest coalesced per-direction certificate even when there
is no next outgoing publication, so idle directions also release completed work.
Resume offers and responses piggyback the same certificate during repair and
binding establishment. Receipt/completion controls do not themselves solicit an
acknowledgement: there is no acknowledgement loop. A lost certificate is repaired
by an exact item retry or the next Resume exchange.

An output batch carrying `StreamCompletedAck(H,E)` omits a separate
`StreamReceivedAck(H)`: the certificate's `H` already acknowledges complete typed
receipt, including the still-unresolved items in `E`. Later completion of a held
item emits the updated `(H,E)` even when `H` is unchanged; removing an exception
is new release evidence. Standalone receipt controls remain available where no
completion certificate is emitted.

Incoming receipt first retains the complete typed item. A future sequence remains
an exact gap item and requests repair from the first missing sequence; an exact
same-digest duplicate is idempotent, while the same sequence with a different
digest is a protocol violation. Closing a gap exposes the entire newly contiguous
suffix in sequence order. The destination advances its received prefix only after
the concrete coordinator has installed each newly contiguous item together with
all dependency or held-work state required to finish it. Each item is then either
received-pending or completed. Independent later items may complete before an
earlier held item, but the completed prefix advances only over the contiguous
completed cut. A barrier marker cannot complete until every preceding item does.

Active outgoing items retain their exact payload and digest until downstream
semantic completion. Completed assignments leave only compact release progress;
there is no per-assignment completed-receipt map. Incoming completed payloads and
Publication's corresponding assignment links are reclaimed atomically after the
semantic owner has installed their destination outcomes. Only gaps and pending
assignments remain in the stream inbox. An old released sequence is a duplicate
with no new semantic work; its retired digest need not be retained for rechecking.

A publication's immutable source evidence retains a checked dispatch fact minted
from the exact prepared stream assignment. That fact lives with the semantic
publication or structural stage; it is not a new per-completed-assignment map.
Alignment route-cutover markers likewise retain their checked semantic admission
independently of the released stream payload. Terminal peer-epoch retirement
releases remaining outgoing assignments while preserving compact completion
versus retirement classification. Pending incoming semantic work survives until
its owning workflow can settle it.

With a fixed pending head and arbitrarily many later completed assignments,
retained stream payloads, raw Resume gap receipts and Publication assignment links
are bounded by the remaining pending/gap work. This receipt contract does not
by itself retire immutable publication, graph, Store, alignment or membership
history: those semantic consumers need their own checkpoint/release contracts.

The payload-parametric owner is composed through checked Herald transitions. The
prototype provides at-least-once transport with idempotent semantic application.
Production credits, eviction,
spill, durable admission, and reclamation remain deferred protocol/state-machine
work; runtime scheduling must not drop accepted logical work.

## 7. Direct publication protocol

### 7.1 Publication identity

Every application nabla is a locally induced controlled object. Its current
operator assignment is checked from the Herald's reconciled label/process view.
Genesis assignments use their checked genesis authority. A dynamically
first-published nabla derives its initial `AuthorityEpoch` from the exact stamped
structural occurrence and the established version-vector cut that makes that nabla
active. A later `label` outcome that assigns a different live process derives its
authority epoch from that label outcome's `ControlIndex`; a same-owner result
retains the existing authority epoch. Relabelling to void retains the preceding
tenure as inapplicable history, while assigning a live process from void derives a
new tenure. Label-delete ends the operator tenure: it retains the preceding
authority only as inapplicable terminal history and does not mint a post-delete
tenure. Thus `AuthorityEpoch` is a nominal
tagged domain rather than an assumption that every authority is an Oracle index.
Publishing a target controlled object does not create an authority entry for that
target in the Oracle.

The complete-profile representation is:

```text
AuthorityEpoch =
    GenesisAuthorityEpoch
  | StructuralAuthorityEpoch(StructuralOccurrenceId, TopologyCutId)
  | LabelAuthorityEpoch(ControlIndex)
```

The implemented authority type, DTOs, transcripts, digests, and every producer and
consumer use this three-arm sum. Its canonical constructor-tag order is exactly
`Genesis < Structural < Label`. There is no compatibility authority arm.
Within `Structural`, compare the canonical occurrence identity and then the cut ID;
within `Label`, compare `ControlIndex`. A given nabla has exactly one initial tenure:
`GenesisAuthorityEpoch` only for a checked startup nabla already present in the
index-zero initial topology projection, or one `StructuralAuthorityEpoch` for any
later data-plane-created nabla, including `newenv` roots. A process Start alone
establishes no root authority. Every later
live tenure is a `LabelAuthorityEpoch`, so it orders after that initial tenure and later
label tenures follow control order. Seeing both initial constructors, two unequal
structural initial authorities, or a label authority not justified by a released
label projection that assigns a different live process to one nabla is an
invariant/protocol fault. A projection from process or zombie to void retains the
prior tenure as inapplicable history and therefore cannot justify a fresh label
authority.

`StructuralAuthorityEpoch` is materialized only after the named
`TopologyCutEstablished` is installed. The carrier occurrence and canonical cut are
computed first; the resulting `TopologyCutId` then completes the initial authority.
The cut whose ID it contains never hashes that derived authority back into itself.

Root creation has no authority cycle. `newenv` publishes through the two explicitly
selected, retained startup source-writer keys. They must resolve to established
Nabla- and Delta-carrier writers with current normal possession and operation
permission. Missing sources yield `EnvironmentSourcesUnavailable`; loss of
permission yields the ordinary operation rejection. Acceptance freezes current
source authority and allocates through that writer's positive nabla sequence.
The new root is never its own source and creating another environment does not
replace the selected mapping. The target nabla receives structural authority only
after its covering topology cut is installed.

Dynamic `StartProcessEpoch` carries no roots or hidden structural publication.
`PublicationBatch.sourceProcessEpoch` is the actual caller process; for `newenv`
the publication source is its selected existing writer. Receivers validate that
writer's frozen authority at the batch's exact source topology/control
prerequisites, including residence, liveness at that prefix, tenure and sort
occurrence. Later End or writer deletion prevents new application operations but
does not invalidate already accepted traffic, including its first peer delivery.
Current destination Store incarnation and suppression checks still apply.
Immutable genesis system-writer coordinates retain no live publication privilege.

This tagged value is encoded canonically on EPRP and in every publication/reservation
digest. It is not compressed into a shared `Word64`: a structural occurrence/cut and
an Oracle control index are distinct nominal sources even if some component bits
coincide.

The accepting Herald owns a counter per `(NablaId, AuthorityEpoch)` and allocates:

```text
PublicationId = {
    nablaId,
    authorityEpoch,
    sourceHeraldEpoch,
    nablaSequence
}
```

The canonical total ordering of two publication identities first compares
`nablaId`, then the tagged `authorityEpoch` under the order above, then
`nablaSequence`, then `sourceHeraldEpoch`. For one nabla this preserves local
sequence and places a later authority tenure after every earlier tenure. Across
different nablas the ordering is an arbitrary but stable tie-break, not a causal
claim. One authority epoch has
exactly one source Herald epoch. Changing physical publisher requires a fresh
authority epoch established either by the established topology cut covering the
structural occurrence that first activates the nabla or, when assigning a
different live process, by that later Oracle-ordered label release; it does not
inherently require a controlled-publication Oracle command.

For ordinary data, allocation, exact request-result caching, local destination
application, stream-sequence assignment, and insertion of all remote batches into
logical outboxes are one serialized Herald transition. A structural carrier first
allocates the stable `PublicationId`, unions its ordinary frozen reader cut with
one mandatory system view per active-generation Herald, allocates its
`HeraldPublicationPosition`, and retains its
request state; if its local structural prerequisites are missing, the same
transition retains an unsequenced structural-admission stage but no stamped peer
item. Its later readiness transition allocates
the `StructuralSequence`, stamps the occurrence from the current applied vector,
reconciles local structural effects, advances that vector component, assigns peer
stream work, and inserts stamped outbox items atomically. A retry returns the same
application request status and reuses the same internal publication ID and
whichever retained ordinary or structural-stage work already exists; the
publication ID never crosses EAPP. A new operator or non-recovering Herald epoch
uses a new authority epoch or source epoch and may begin its
`PublicationId.nablaSequence` at zero without collision. The independent
`StructuralSequence` defined below remains strictly positive.

Each receiver-specific structural batch contains that receiver's exact mandatory
private system view at the retained source strength. Additional destinations are
the frozen application-reader cut for the same receiver: none may strengthen the
source, and each exact Delta/incarnation/sort-occurrence tuple MUST appear in a
cut-qualified placement revision retained from that receiver. This admits a
legitimate stale frozen route after local replacement while rejecting fabricated
readers and another Herald's private system view.

Every accepted `write`/`forward` that may publish also receives one monotone
`HeraldPublicationPosition` after its frozen route is known, including work held
behind a label fence or a data-plane dependency. This Herald-wide order is
distinct from the process-scoped order used by `label`; topology cutover uses it
to account for every retained publication from all local processes. A controlled
target's first publication, current update, or obsolete publication is never
staged for Oracle approval.

Every topology-affecting publication origin uses this position/sequence path,
including `write`, structural `forward`, `newenv`, and
an internal origin that creates a new semantic publication. A multi-root origin
atomically reserves consecutive `HeraldPublicationPosition`s in canonical manifest
order and retains one independently ready/unsequenced stage per root. Repair or
catch-up of an existing structural occurrence reuses its full stamp and creates
neither a new publication position nor a new structural sequence.

### 7.2 Typed publication item and semantic digest

One immutable semantic item groups every destination at one peer. It is
deliberately unsequenced:

```text
PublicationBatch {
    publicationId,
    sourceProcessEpoch,
    sortId,
    sortDefinitionOccurrenceId,
    canonicalValueBytes,
    sourceStrength: Weak | Normal,
    sourceTopologyPrerequisite,
    controlPrerequisite,
    destinations: [{ deltaId, storeIncarnationId, routedStrength }]
}

PeerPublicationItem =
    OrdinaryPublication(PublicationBatch)
  | StructuralPublication(StructuralOccurrenceStamp, PublicationBatch)
```

`canonicalValueBytes` is one opaque canonical domain-value encoding on the peer
wire. An enum adds no peer DTO constructor, ordinal field, or edge-specific wire
tag: its exact symbol is inside those canonical bytes and is interpreted only
after the receiver obtains the effective descriptor and re-runs semantic
admission. The application protocol's `EnumValue` constructor is likewise not a
peer-publication representation.

The sum is closed. `OrdinaryPublication` rejects a canonical value whose role is
one of neutral vertex, edge, nabla, or delta; every such carrier MUST use
`StructuralPublication`. In the structural arm, `stamp.publicationId` equals the
enclosed batch's `publicationId`, `stamp.sourceHeraldEpoch` equals that
publication's source Herald, and `stamp.carrierRole` equals the closed role decoded
from the canonical value. The receiver recomputes
`stamp.structuralPublicationDigest` from the normalized underlying structural
publication, sort occurrence, source prerequisites, and carrier role and requires
exact equality before admitting the occurrence. The complete stamp is therefore
wire-visible and bound by semantic item identity rather than being an out-of-band
progress claim.

The surrounding peer-stream assignment supplies the exact direction, positive
`streamSequence`, and item digest. Those assignment facts are not fields of the
item. The batch's `sourceStrength` is the retained effective possession strength
of the originating operation: ordinary write and `newenv` origins use
`Normal`, while `forward` may carry `Weak | Normal`. It is authenticated semantic
input, not a receiver-selected capability. Every destination retains its
independently routed `Weak | Normal` strength and MUST be no stronger than
`sourceStrength`; source preparation attenuates it to
`min(sourceStrength, routedStrength)`. The fence dependency remains the closed
fact `NoFenceDependency`, not a caller-selectable field. Destinations are
non-empty, unique by `DeltaId`, and normalized in ascending `DeltaId` order. The
batch does not contain a route to a third Herald.
A receiver MUST NOT add destinations based on its graph. Forwarding is a new
application `forward` operation with a new publication identity, not peer
relaying.

`sourceTopologyPrerequisite` is either the checked genesis topology or the exact
all-member established `TopologyCutId` that establishes a dynamically published
nabla and its initial structural authority epoch. It is a data-plane prerequisite,
not a `ControlIndex`. A receiver retains a batch until it has installed that
established cut; later label-established authority is checked through
`controlPrerequisite` as usual. The two fields are distinct nominal types and
neither can stand in for the other.

Publication stamping nevertheless keeps their evidence coherent. Let
`authorityMinimum` be the greatest control index required by the publishing
writer's current authority. The checked genesis topology contributes control
floor zero; any other `sourceTopologyPrerequisite` contributes the
`appliedControlPrefix` retained in that exact installed cut. The publication MUST
use:

```text
controlPrerequisite = max(authorityMinimum,
                          sourceTopologyPrerequisite.appliedControlPrefix)
```

It MUST use the prefix authenticated by the selected installed cut, not the
Herald's possibly newer live applied-control prefix. Later unrelated control work
therefore cannot over-constrain a publication stamped against an older current
cut, while the destination still receives every control fact on which that cut
depends.

`PeerItemDigest` is SHA-256 over one of two distinct cereal-encoded private
normalized semantic transcripts. The ordinary arm is:

```text
ECLIPS-PEER-PUBLICATION-BATCH
publicationId(nablaId, authorityEpoch, nablaSequence, sourceHeraldEpoch)
sourceProcessEpoch
sortId
sortDefinitionOccurrenceId
canonicalValueBytes
sourceStrength
sourceTopologyPrerequisite
controlPrerequisite
NoFenceDependency
ascending destinations(deltaId, storeIncarnationId, routedStrength)
```

The structural arm is:

```text
ECLIPS-PEER-STRUCTURAL-PUBLICATION-BATCH
StructuralOccurrenceStamp(
  sourceHeraldEpoch,
  sourceSequence,
  complete normalized predecessorVector,
  publicationId,
  structuralPublicationDigest,
  carrierRole)
publicationId(nablaId, authorityEpoch, nablaSequence, sourceHeraldEpoch)
sourceProcessEpoch
sortId
sortDefinitionOccurrenceId
canonicalValueBytes
sourceStrength
sourceTopologyPrerequisite
controlPrerequisite
NoFenceDependency
ascending destinations(deltaId, storeIncarnationId, routedStrength)
```

The duplicated publication identity is intentional and MUST compare equal before
hashing/admission. Distinct domain separators make an ordinary item and a
structural item unequal even if their underlying batch bytes are otherwise equal.

The closed strength tags are `Weak = 0` and `Normal = 1`; the explicit absent
fence tag is `0`. The transcript contains neither stream direction nor sequence,
because the digest exists before assignment. Direction still qualifies every
active assignment and checked semantic dispatch fact, so owner admission cannot
move an equal semantic digest between directions. Application session/request identifiers,
source-language field names, map presentation, `Show` output, DTOs, framing, and
TCP bytes are also absent. EPRP carries this already-fixed semantic digest and
separately defines the wire encoding; it does not redefine item identity from
frame bytes.

Application `SessionId` and `RequestId` never cross this boundary. `PublicationId`
is the globally meaningful semantic and diagnostic correlation for peer delivery.

### 7.3 Receive states

Incoming data moves through explicit states:

```text
decoded
  -> accepted/dependency-held
  -> admitted
  -> control-held (when needed)
  -> applied | terminally-ignored
```

- **decoded** means the framing and current schema were decoded successfully;
- **accepted/dependency-held** means the source epoch, stream identity, publication
  identity, and destination shapes are coherent, and the complete item plus its
  dependency-tracking state are installed, but an effective sort definition,
  source topology cut, or another data-plane definition may still be missing;
- **admitted** means every required definition is effective and the receiver has
  reconstructed and validated the value, key, rank, lifecycle class, and exact
  destination meaning;
- **control-held** means semantic admission succeeded but a genuinely
  Oracle-ordered source-authority, process, label, regular-definition-retirement,
  or label-fence prerequisite is not yet applied locally;
- **applied** means the normal store transition and any required predefined
  reconciliation are complete; and
- **terminally ignored** means every exact destination is stale, passive, removed,
  or canonically unable to accept the object after all required authoritative
  prefixes are available. A stale store incarnation, an applied label-delete, or
  locally retained monotone obsolete suppression may establish this disposition.
  Absence of an Oracle record for the target object cannot do so, because ordinary
  controlled publication is what establishes and advances that object.

Only applied data is visible. Receipt, decoding, or deduplication does not expose a
value or induce an entity. The stream completion acknowledgement advances only
after applied or terminally ignored, so a held item remains accounted for by
barriers even if its received acknowledgement has advanced.

Missing Oracle progress alone never establishes terminal ignore. An item for a
live destination whose genuine prerequisite is above the local applied cursor
remains retained, invisible, and incomplete. A controlled target publication is
otherwise admitted against the locally retained identity-to-sort binding, winner,
label, and obsolete-suppression state. First publication may establish that local
binding; current and obsolete publications advance it through the ordinary winner
transition without an Oracle target-lifecycle record.

The receiver reconstructs the checked publication from its locally effective
descriptor, recomputes key/rank/lifecycle/definition meaning, and applies it only
to the named live store incarnations. An invalid value is a peer protocol fault; a
stale store incarnation is a normal terminal ignore.

For a predefined edge publication, reconstruction uses the ordinary controlled
instance envelope plus the closed prototype edge-specific payload codec containing
only source endpoint, destination endpoint, and preserving/weakening strength. The
strength field has the exact closed enum schema `preserve :| [weaken]`; an integer
or an undeclared symbol is invalid. The home Herald validates that shape and its
local publication authority before semantic acceptance; no target-lifecycle
proposal follows. Each peer validates the shape again during admission. A payload
containing a sort selector or any additional edge-specific field is invalid; it is
not represented by a new `PublicationBatch` field or treated as an ignorable
extension.

After decoding, normalized internal startup and structural-reconciliation evidence
transcripts retain stable private codes `0 = preserve` and `1 = weaken`. Those
codes bind semantic evidence only. They are not carried as the edge's canonical
value, exposed through EAPP, or accepted as a second peer encoding.

The named `SortDefinitionOccurrenceId` must equal the receiver's effective
occurrence for `sortId`. An older occurrence is terminally stale even when its
canonical descriptor bytes equal the current definition; equality of `SortId` does
not let pre-retirement data or certificates cross a retirement boundary.

If a matching sort definition or another structural definition has not yet arrived
through the data plane, the receiver keeps the complete publication and its
explicit dependency keys in dependency-held state and schedules definition catch-up.
Delivery order is not grounds to call it invalid. `StreamReceivedAck` may advance
only after that item and dependency state are installed; acknowledgement records
the ownership handoff rather than reserving a resource. For a controlled value,
the effective descriptor and sort must equal the locally retained first admitted
binding for that `GlobalObjectId`. A conflicting binding from a trusted Herald is
an invariant/protocol fault; the Oracle is neither the descriptor registry nor the
admission authority for the publication.

### 7.4 Structural version vectors and stabilization

Neutral-vertex, edge, nabla, and delta carrier publications affect the graph or
the set of typed active cut points. Profile 0.1 does **not** send them through the
Oracle merely to obtain an order. Within one Herald-membership generation it uses
a version vector whose components are owned by the Heralds that accept structural
publications. Membership retirement seals a retired source from the union of survivor-retained
work and establishes a successor generation; it never rewrites an old vector as
though the retired member had not existed.

```text
StructuralSequence = positive unsigned 64-bit source-local sequence

StructuralPrefix =
    EmptyStructuralPrefix
  | StructuralPrefixThrough(StructuralSequence)

StructuralVersionComponents =
    ascending exact map HeraldEpoch -> StructuralPrefix

StructuralVersionVector = {
    membershipGenerationId,
    memberSetDigest,
    components: StructuralVersionComponents
}

StructuralOccurrenceId = (sourceHeraldEpoch, sourceSequence)

StructuralOccurrenceStamp = {
    sourceHeraldEpoch,
    sourceSequence,
    predecessorVector,
    publicationId,
    structuralPublicationDigest,
    carrierRole
}

TopologyFrontier = {
    structuralVersionVector,
    appliedControlPrefix
}

TopologyPredecessor =
    SameGenerationPredecessor(TopologyCutId)
  | MembershipSuccessorPredecessor {
        predecessorTopologyCutId,
        predecessorMembershipGenerationId,
        successorMembershipGenerationId,
        terminalPredecessorVector,
        successorInitialVector,
        terminalSourceUnionDigest
    }

TopologyCut = {
    predecessor: TopologyPredecessor,
    frontier: TopologyFrontier,
    topologyOccurrenceDigest
}

TopologyCutId = digest(canonical TopologyCut)

GenesisTopologyCutId = digest(
    "ECLIPS genesis topology cut",
    systemId,
    genesisMembershipGenerationId,
    memberSetDigest,
    initialProjectionDigest,
    all-empty StructuralVersionComponents,
    ControlIndex 0)
```

The vector contains exactly one component for every active `HeraldEpoch` in its
named membership generation/member-set digest; it is never sparse and never
acquires a component from discovery. Componentwise comparison is defined only when
both generation IDs and member-set digests are equal. At genesis every component
is empty. A source allocates its next
sequence only when the retained structural publication's referenced structural
identities/roles, effective sort occurrences, genuine control prerequisites, and
source authority are already applied locally. Application acceptance may retain an
unsequenced structural-admission stage, but that stage has no occurrence ID and is
not disseminated or reported as structural progress.

For newly allocated sequence `s`, `predecessorVector` is the source's complete
applied vector immediately before allocation; its own component is exactly `s - 1`
(or empty for `s = 1`). Allocation, finite local reconciliation, applied-vector
advance through `s`, and stamped peer-outbox insertion are one serialized
transition. Therefore every provider known only through a same-source structural
publication has a lower sequence already covered by the predecessor. Foreign
components record only effects the source had actually applied, so the stamp makes
no total-order claim between concurrent origins.

`structuralPublicationDigest` is a canonical semantic digest of the source
publication identity, sort occurrence, canonical controlled value, retained
source strength, genuine source-topology/control prerequisites, and carrier role. It excludes per-peer
stream assignment, physical connection, and destination presentation. Equal
source/sequence with a different stamp or digest is a protocol contradiction. The
complete stamped occurrence is retained for the finite run so a gap can be
repaired even after the occurrence loses the current winner comparison.

A structural publication is disseminated through the ordinary direct peer and
private-system-view data plane, never through Raft. Its stabilization cut includes
one appropriate private carrier view at every active-generation Herald. If all of those views
cannot yet be named, the source retains the accepted structural workflow and does
not claim stabilization. The `StructuralPublication` peer item carries and
digest-binds the full stamp. Exact source-stream retransmission/repair reuses the
same receiver-specific item. Definition or alignment history transfer reuses the
same stamp and canonical structural publication semantics in that transfer's own
receiver-qualified carrier/evidence; it MUST NOT retarget an existing
`PublicationBatch`, copy H2's destination/store-incarnation list to H3, or allocate
a new occurrence. Catch-up of structural history transfers missing stamped
occurrences, not merely the current winning payload; otherwise a contiguous version
component could not be justified after an intermediate update was superseded.

For one stamped occurrence the protocol distinguishes five facts:

1. **received**: the complete occurrence and all dependency/repair state needed to
   finish it are retained;
2. **structurally applied**: the occurrence is in the contiguous per-source
   history, its deterministic winner/no-winner consequence has been reconciled
   into the local controlled projection and graph, and every directly implied
   occurrence-qualified placement/alignment/cutover/input-closure **debt**, including the
   inputs needed to materialize its later cut-qualified obligation, has been durably
   retained;
3. **cut locally accepted**: one Herald has reconstructed and immutably accepted
   one exact proposed `TopologyCut`; that fact alone does not establish a shared
   generation;
4. **cut established and installed**: the Herald has validated the complete
   one-per-member acceptance evidence, installed the resulting
   `TopologyCutEstablished`, and may use that shared topology checkpoint; and
5. **historically ready**: the relevant store incarnation has imported its
   required predecessor prefixes and has the continuing subscription represented
   by a `HistoricalCertificate`.

The second fact has a deliberately finite local boundary. It requires creation of
all implied occurrence-qualified debts, not a not-yet-known `AlignmentCut` or
context generation and not completion of remote snapshots/subscriptions. Requiring
either a future established cut or remote fulfilment before structural application
would
create a cycle: a peer could wait for the acknowledgement that permits the very
context request needed to produce that acknowledgement.

A receiver may retain later occurrences from one source, but it MUST NOT expose
them as graph changes or advance that source component across a gap. It applies a
stamped occurrence only after its predecessor vector is componentwise covered in
the same membership generation, or mapped through an installed terminal-successor
base as specified below, and after its genuine control prerequisite is applied and
its data-plane definitions are effective. A stamp whose sequence precedes a same-source structural provider it
requires contradicts valid source admission. Wire reordering may deliver a valid
higher sequence first, but cannot change the lower-sequence causal relationship.

Across different sources concurrent occurrences may be incorporated in either
order. At an equal version vector, however, every member of that generation has
incorporated the same exact occurrence set; the controlled winner rules and graph derivation
MUST be order-independent. The canonical `topologyOccurrenceDigest` covers the
winning occurrence and effective induced state of every structural identity. It
includes the structural carrier's already-existing source publication identity,
but MUST exclude the `StructuralAuthorityEpoch` that this same cut will derive for
a newly induced nabla. After `TopologyCutId` is known and established, that authority
is a deterministic consequence rather than an input to the digest. Equal frontiers
with unequal digests are an invariant fault, not two valid graph versions.

Each active Herald sends its full cumulative report directly to the
lexicographically least active Herald epoch in its installed structural membership
generation, the deterministic topology-cut announcer. The announcer already
retains its own report and does not send it to itself or to other members. A report
is offered whenever cumulative progress advances, and a replacement or explicitly
reoffered announcer binding receives the current report again. Reconnecting to a
different member does not offer a report. Installing a successor structural base
immediately offers the new generation's report to its announcer without waiting
for another input or local progress, including when the announcer changed. A
joining observer remains ineligible until its admission base is installed.

The composed Herald transition retains one final cumulative snapshot at the first
same-generation report slot for the destination binding and suppresses exact
repeats. A predecessor-generation report retains its original ordering before
terminal-source Established evidence; a successor-generation report cannot move
across that boundary. Initial Hello acceptance defers all semantic repair until
the stream resume handshake. The report is:

```text
StructuralAppliedReport {
    reporterHeraldEpoch,
    appliedVersionVector,
    appliedControlPrefix
}
```

The report is a claim about the reporter's own semantic application, not receipt
or TCP delivery. Its vector supplies the generation and member-set coordinate; the
reporter must be active in exactly that generation. For a given reporter and
generation the vector is monotone componentwise and the
control prefix never regresses; a lower report is stale and an incomparable vector
successor is a protocol fault. Only the announcer requires the latest report from
every active-generation member. Its matrix contains `N` reports with an
`N`-component vector each. An ordinary cumulative advance sends at most one report
instead of broadcasting it; an all-member round therefore uses `N - 1` direct
reports. Installing a membership successor resets the remote report matrix, so a
replacement announcer must receive fresh reports for that generation. No
unbounded hierarchy of “what H1 knows that H2 knows that H3 knows” is required.
The advertised vector itself says which source prefixes the reporter has applied.

Every Herald installs the same distinguished `GenesisTopologyCutId` during checked
startup. It is the predecessor root, not a dynamic `TopologyCut` with a recursive
predecessor. The first announced cut MUST use
`SameGenerationPredecessor(GenesisTopologyCutId)`. Later same-generation cuts name
the prior installed `TopologyCutId`; the first cut after retirement instead uses
the membership-successor form below. A different genesis root is a
startup/peer-compatibility fault, and no first dynamic cut may use an absent
predecessor.

#### 7.4.1 Membership-successor structural base

Each authoritative retirement seals the old generation's sequence allocator.
Already stamped work keeps its original identity, vector and payload; unsequenced
work waits for an installed base before taking its first stamp. Several retirements
can extend one `MembershipBaseClosure`. Its checked lineage runs from an actually
established anchor to the current target, and its retired set is exactly the
ordered lineage's removed epochs. No join activates inside this closure.

The anchor follows the unique established cut chain. Inventories carry complete
ordinary `TopologyCutEstablished` certificates and the complete terminal-union
certificates for membership cuts on that chain. A receiver first authenticates
these against its checked Oracle history, repairs the required actual retained
bytes, and installs the chain through the ordinary Graph admission boundary.
Comparable longer proved chains supersede older anchors; an unestablished proposal
never qualifies. Rebase preserves payloads and prior evidence, reseals inventories
and recollects current acceptances. A stale target or obsolete anchor cannot
restore authority or fault a healthy surviving binding merely by arriving late.

```text
TerminalSourceInventory {
    anchorMembershipGenerationId, targetMembershipGenerationId,
    retiredSourceHeraldEpoch, reportingSurvivorHeraldEpoch,
    establishedCutCertificates, establishedMembershipBaseCertificates,
    retainedPriorInventories, actualRetainedOccurrenceClaims,
    greatestContiguousRetainedPrefix,
    occurrenceClaims: ascending [(StructuralOccurrenceId, publicationDigest)]
}
TerminalSourcePayloadRequest {
    anchorMembershipGenerationId, targetMembershipGenerationId,
    retiredSourceHeraldEpoch, occurrenceId, supportingInventoryDigest
}
TerminalSourcePayloadRelay {
    anchorMembershipGenerationId, targetMembershipGenerationId,
    retiredSourceHeraldEpoch, relayHeraldEpoch,
    supportingInventoryDigest, canonicalStructuralOccurrence
}
TerminalSourceUnion {
    anchorMembershipGenerationId, targetMembershipGenerationId,
    orderedGenerationIds, retiredSourceHeraldEpochs, anchorTopologyCutId,
    inventoryDigests: ascending [((retiredOrigin, currentReporter), digest)],
    closedRetiredSourcePrefixes: ascending [(retiredOrigin, prefix)],
    includedOccurrenceIdsAndDigests, retainedAheadOccurrenceIdsAndDigests,
    terminalPredecessorVector, successorInitialVector,
    terminalControlPrefix, terminalTopologyOccurrenceDigest
}
TerminalSourceUnionDigest = digest(canonical TerminalSourceUnion)
TerminalSourceUnionAnnounce(leastCurrentHerald, canonicalTerminalSourceUnion)
TerminalSourceUnionAccepted(unionDigest, reportingCurrentHerald)
TerminalSourceUnionEstablished(canonicalTerminalSourceUnion,
    ascending exact [(reportingCurrentHerald, acceptanceDigest)])
```

The inventory set is the retired-origin × current-reporter product. Every claim
names a complete semantically retained occurrence. Previous inventories preserve
provenance but never substitute for actual available bytes. Only a current active
reporter that actually retains an occurrence may relay it; the embedded source
remains its original retired epoch. Anchor repair may request an earlier retired
origin using a current reporter's retained-claim evidence. Direct new traffic from
that retired origin remains fenced. Repair is destination-free structural history,
not a new application delivery or a recomputation of original destinations.

Union equal claims by exact occurrence identity; unequal payload digests fault.
Compute the unique greatest contiguous causally replayable prefix for every retired
source jointly, descending to the fixed point when another terminal source's gap
makes a dependency unavailable. Keep all ahead claims and payloads separate and
hashed. Never invent a prefix from an old digest without a surviving byte retainer.
Current sources repair their own required history through ordinary retained lanes.

Starting at the established anchor, the terminal vector is the least causal
closure covering those exact terminal prefixes and their dependencies. Interpret
all vectors through their checked generation ancestry. Reconstruct the topology
at the final retirement's exact control prefix, including intervening label and
process consequences and excluding later control. The successor initial vector
projects this closure onto current survivors without renumbering sequences.

One fresh acceptance from every current member establishes the exact canonical
union. Old acceptances do not satisfy a changed target. Its membership-successor
cut installs atomically with the checked terminal base, vector projection and
retained lineage evidence. Already applied survivor work above the base projects
without replaying effects. An opaque descendant-settlement witness requires the
actual installed base evidence for every crossed removal; ancestry alone is not
settlement. Historical completion remains valid after subsequent retirements.

From one report for every member of the same generation, the announcer computes
the conservative all-member stable vector by componentwise minimum. A stamped occurrence is globally stable
when its dot and therefore its causal predecessor vector are covered by that
minimum. It likewise computes the all-member stable control prefix as the minimum
of the reported `appliedControlPrefix` values. Global stability alone is not the
final common-cut fact; the explicit protocol below establishes that fact at every
member.

The full componentwise minimum is causally closed. If its component for source
`H` covers occurrence `H:s`, every reporter has applied `H:s`; application of that
occurrence required the reporter to cover the occurrence's complete predecessor
vector and control prerequisite, and neither frontier can regress. Taking the
minimum therefore still covers those prerequisites. An arbitrary vector merely
bounded by the minimum does not have this property and is not a legal cut
candidate.

An accepting Herald may already be ahead of the proposed stable control prefix. It
therefore retains the finite run's canonical applied topology-affecting control
entries—including process, released label/delete, predefined-disappearance, and
regular-definition-retirement progress—and topology-relevant derived checkpoints,
together with complete structural occurrence history. It can then reconstruct the
exact graph at `(proposed structural vector, proposed control prefix)` without
consulting its newer current view. A later control transition MUST NOT erase the
history needed to validate an older open or established cut.

The version vector identifies an event set, but incomparable transient vectors
must not create incompatible context-generation ancestry. Profile 0.1 therefore
serializes only **established topology cuts**, not structural publication itself. The
lexicographically least active `HeraldEpoch` in the membership generation is the
deterministic cut announcer.
Starting from the last all-member established predecessor cut, it chooses a strictly
advancing candidate whose structural vector is exactly the current componentwise
all-member minimum and whose control prefix is exactly the current all-member
minimum. It may batch several concurrent occurrences into one cut. For a
same-generation predecessor both frontiers MUST componentwise cover the predecessor
cut's frontiers. For a membership successor the validation rule is the distinct
terminal-base rule above, and that membership transition is itself the required
strict advance even when its terminal union is empty and its topology digest is
unchanged. A same-generation candidate must instead incorporate at least one newly
stable structural occurrence or one stable control revision that changes induced
graph identity, controller projection, or presence. Herald-local root activity,
placement, and Store state cannot make such a same-generation cut advance.
Unrelated process, label-authority, or other control-prefix progress alone never
creates a topology cut or alignment generation.
The independently repairable peer vocabulary is:

```text
TopologyCutAnnounce {
    announcerHeraldEpoch,
    topologyCutId,
    canonicalTopologyCut
}

TopologyCutAcceptance {
    topologyCutId,
    reportingHeraldEpoch,
    reporterAppliedVersionVector,
    reporterAppliedControlPrefix
}

TopologyCutAccepted(TopologyCutAcceptance)

TopologyCutEstablished {
    topologyCutId,
    canonicalTopologyCut,
    acceptances: ascending exact [TopologyCutAcceptance]
}

TopologyCutEstablishedAck(topologyCutId, reportingHeraldEpoch,
                          membershipGenerationId)
```

An announce is valid only from the deterministic announcer and its ID must
recompute from the canonical cut. A same-generation predecessor must be the
receiver's last installed established cut. A membership-successor predecessor must
name that last old-generation cut, the exact applied Oracle membership successor,
and the locally established terminal union. A receiver with an unknown or missing
predecessor/union retains the announce and requests/retries that dependency chain;
once repaired, a divergent dependency is a protocol fault. A receiver whose
applied frontier is still behind likewise retains the announce until progress. It
accepts only after it:

- verifies either same-generation non-regression or the exact terminal-base
  projection and successor-initial-vector rule;
- componentwise covers the proposed vector and control prefix;
- verifies that every included occurrence's predecessor vector and exact control
  prerequisite are covered in the same generation or through the specified
  terminal-base mapping, so the named event set is causally closed;
- reconstructs the exact retained winner set at that frontier from retained
  structural and control history rather than its possibly newer current view;
- derives the equal topology digest; and
- has installed the named established predecessor cut and, for a membership
  successor, the exact `TerminalSourceUnionEstablished`.

Acceptance is immutable for `(reporter, topologyCutId)`. An exact repeated announce
or acceptance reoffers the same reply and is idempotent. A same-ID/different-cut,
same-reporter/different-acceptance, two different successor IDs proposed from the
same still-open predecessor, wrong-member, regressing report, or acceptance for
another predecessor is a protocol fault. An announce whose predecessor is
strictly older than the already established cut is stale: the receiver does not
reopen it and instead reoffers its latest `TopologyCutEstablished`. Later applied
structural work never mutates an existing acceptance.

The announcer retains the announce until it has exactly one acceptance from every
active-generation Herald. It then constructs `TopologyCutEstablished` containing
that complete
normalized evidence set and sends it to every member until acknowledged. A member
installs the new established cut only after it recomputes the cut ID, validates the
exact one-per-member acceptance set and every covering frontier, and has installed
the predecessor plus any required terminal union. A future established message is
dependency-held behind those dependencies; an exact duplicate is idempotent;
unequal evidence for one cut ID is a protocol fault.
`TopologyCutEstablishedAck` is emitted only after local
installation. The announcer retains the established message and does not announce
a successor until every active-generation Herald has acknowledged installation.
The reconnect root set is deliberately smaller than the retained transcript. Every
active-generation Herald reoffers its current `StructuralAppliedReport` only when
repairing its announcer binding; the announcer reoffers the complete installed established chain in predecessor order and, when no established
delivery is open, its current open announce. `TopologyCutAccepted` and
`TopologyCutEstablishedAck` are reactive replies: replaying the corresponding
announce or established message reproduces the exact reply. TCP send completion is
never semantic progress.

`TopologyCutAccepted` is addressed only to the announcer. The announcer admits it
only for its one open announce, from the named active-generation member, with the
exact member set and covering report; an exact duplicate is a no-op. If the cut is already
established, a matching late acceptance reoffers `TopologyCutEstablished`; an
acceptance for an unknown or conflicting cut is a protocol fault. Likewise,
`TopologyCutEstablishedAck` is addressed only to the announcer and is valid only
for the retained established cut and a member represented in its acceptance set.
An exact duplicate is a no-op, an acknowledgement for an installed predecessor is
stale, and a foreign-member, unknown, or future-cut acknowledgement is a protocol
fault. Every member retains its installed established chain and its own acceptance
and acknowledgement for the finite run; the announcer retains the complete
announce/accept/establish/ack transcript. Thus a reconnect can repair the exact
predecessor chain and final-established fact without inventing progress.

A structural application call completes at its home Herald only after its
occurrence is globally stable **and** that Herald has installed a
`TopologyCutEstablished` whose vector covers the occurrence. The complete evidence
says every member of the named generation incorporated the occurrence, retained
its finite local consequences, and immutably accepted the exact shared checkpoint; the home has
installed that checkpoint and final-established delivery continues until every
member acknowledges installation. A newly published nabla's initial authority is
usable only from an installed established checkpoint. This does not say the
occurrence is still the winner at every later cut or that context alignment has
completed. Ordinary non-structural writes retain their local-acceptance completion
rule.

If retirement made the original generation's all-member evidence impossible, a
stamped occurrence authored by a survivor may instead complete when the first or a
later successor cut covers that same immutable occurrence through its established
terminal predecessor. It is neither restamped nor attributed to a new source. An
unsequenced survivor stage has no old occurrence to preserve: it waits for the
successor base, discards only any retired frozen destination, and allocates its
stamp in the successor generation.

With concurrent occurrences `A` and `B`, Heralds may temporarily have `[A,0]` and
`[0,B]`; the established successor may be their join `[A,B]`, but the two
incomparable local views never become competing shared predecessor generations. If
the announcer is an eligible non-voter and Oracle retirement removes it, the
deterministic announcer in the successor membership reoffers retained candidate
state after terminal-source union. Failure of an announcer that hosts a Raft voter
remains outside this slice's progress guarantee because that member cannot be
retired. The announcer role itself is not an Oracle or Raft service.

Physical delta placement remains a separately owned version vector. Structural
progress can prove that a delta definition was incorporated without proving that
another Herald has admitted the hosting Herald's current store incarnation.
Consequently an `AlignmentCut` combines one established `TopologyCut` with the exact
physical placement vector; neither model treats graph progress alone as historical
readiness.

#### Planned compact Oracle-activation fallback

The version-vector protocol above is the normative initial model. Before its
implementation is considered complete, generated schedules and model checking
must exercise concurrent structural sources, cut joining, generation ancestry,
and removal input closure. If that proof shows the peer cut protocol to be
disproportionately complex, the planned fallback is a compact Oracle activation
index.

In that fallback the structural payload still travels directly peer to peer. The
Oracle orders only the exact token `(StructuralOccurrenceId, publicationDigest)`,
where `publicationDigest` is the stamp's `structuralPublicationDigest`, and assigns
the next positive scalar `TopologyActivationIndex`. That nominal counter is
distinct from `ControlIndex` and advances only for accepted activation tokens, so
interleaved non-topology Oracle commands create no topology dots. The Oracle does not
receive the payload, validate endpoints or values, approve/reject the publication,
select a context source, or claim that any Herald applied it. A Herald advances its
local scalar applied index only after joining the token with the direct payload and
performing the same finite local reconciliation above. All-member applied evidence
in the named generation, placement cuts, historical certificates, and ordered route-cutover/input-closure
evidence remain
necessary.

The fallback is not a second live protocol and has no dormant message arm in
profile 0.1. Adopting it replaces the vector types, codecs, vectors, and properties
in lockstep and starts a fresh experimental system lineage. Its only intended gain
is scalar graph-cut naming and one common structural order; it deliberately adds
an Oracle-quorum dependency to every structural activation.

## 8. Control- and label-dependent visibility

Every publication names the `controlPrerequisite` defined above: the maximum of
the publishing writer's genuine authority minimum and the exact selected topology
cut's applied-control prefix. The authority minimum may establish the publishing
nabla/process authority, a label or label-fence outcome, or a regular-definition-
retirement cut after which this is a new occurrence. The field does not name an
Oracle approval, instantiation record, current revision, or obsolescence decision
for the target controlled object. Descriptor availability, effective regular
sort-definition presence, the batch's source topology prerequisite, target first
publication, target winner state, and obsolete suppression are data-plane
dependencies. Because Oracle projections are contiguous, applying the named index
also applies every smaller genuine prerequisite. A destination whose Oracle
projection is behind:

1. records and deduplicates the admitted message;
2. holds it outside visible stores;
3. advances its contiguous Oracle watch;
4. revalidates the source authority, label state, fence, and retirement occurrence
   after the prerequisite applies; and
5. either applies it or terminally ignores it.

It MUST NOT reject an otherwise valid message merely because control delivery is
slower than peer delivery.

Regular and controlled application values normally require only the already
committed source-nabla authority epoch; publishing either kind creates no Oracle
command. A first application-origin controlled publication from `newid(p)` consumes
the source Herald's matching pristine reservation and locally establishes the
target's immutable identity/sort binding. Checked genesis and `newenv` origins
instead use their closed checked-manifest/atomic-first-use rule. The Herald-managed
process object uses the exact retained applied Start result, which creates no
root publications. These origins create no synthetic application reservation. A current update is checked against the resulting local binding and
authority. An
obsolete publication installs monotone local suppression before it is released;
later current traffic cannot revive the object. Label fences may add a later
prerequisite when their nabla designates the labelled sequencing object.

A publication accepted after the one allowed label fence opens carries that
decision dependency or remains in the source's held queue. The prototype holds such
work at the source to minimize invisible copies.

`sort-sort` is different: its values are regular content-addressed data. Publishing
a sort definition creates no Oracle command and requires no policy assertion for
the sort being defined. A receiver validates it with the primordial `sort-sort`
descriptor. The sole semantic checked-publication admission path applies the
carrier's closed intrinsic: canonical-decode and re-encode the embedded descriptor,
recompute `SortId`, and reject a mismatched claim. This intrinsic is additional to
the descriptor's portable `Always` predicate and cannot be bypassed by a different
publication caller. Embedded descriptors use application admission unless their
bytes exactly equal one of the six closed primordial descriptors; no new structural
carrier can be smuggled through regular data. Equal repeated definitions are
admitted as the same semantic object regardless of publication identity. Profile
0.1 relies on SHA-256 collision resistance and has no same-ID/different-bytes
containment branch.

A publication, placement, or topology definition whose `SortId` is not yet
effective in `Herald.SortRegistry` remains dependency-held and triggers
`sort-sort` catch-up through the ordinary peer/alignment plane. A sender cannot
make its payload semantically effective by attaching an unobserved descriptor.
Once the definition is effective, controlled publication admission derives the
same policy locally and requires it to agree with any retained target binding.
There is no Oracle sort registration, target-policy assertion, or controlled
publication verdict to wait for.

A deferred descriptor decoding cache would use peer messages outside the ordered
publication stream:

```text
DescriptorRequest(sortId)
DescriptorResponse(sortId, canonicalDescriptorBytes)
```

**Deferred:** the current peer message sum has no descriptor request/response
arms. The following is the proposed decoding-cache boundary if explicit descriptor
fetch is introduced; it is not an additional live protocol.

Any peer with checked equal bytes may respond. The receiver canonical-decodes the
AST, validates it, and requires the recomputed `SortId` to equal the requested ID
before storing it in an immutable decode cache. A response permits
decoding only: it MUST NOT create an effective sort, induce topology, confer a
capability, or bypass arrival of the regular `sort-sort` publication in a private
system-view delta. Malformed or wrong-hash responses are protocol violations; equal
repeats are idempotent. Requests and responses remain independently processable so
an unknown descriptor does not head-of-line block unrelated stream items.

Regular sort definitions themselves reach every active-generation Herald's private
primordial `sort-sort` delta by ordinary frozen publication and continuing
alignment. Snapshot or chunk ordering may deliver dependent data first, so the held dependency is
released only after that actual definition becomes effective. A `sort-sort`
publication still carries the controlled authority of its source nabla and any
applicable sequencing fence; only the definition value lacks an Oracle lifecycle.
Equal declarations share
`digest(SystemId, SortId, lastResolvedRetirementControlIndex | genesis)` as their
`SortDefinitionOccurrenceId`. A retirement resolve changes that occurrence base;
ordinary duplicate writes within one base do not.

The `sortDefinitionOccurrenceId` in the enclosing `PublicationBatch` names the
occurrence of the batch's data sort. For a `SortDefinition` value that is the
primordial `sort-sort` occurrence. Inducing the value separately assigns the
derived occurrence above to the `SortId` contained in the value; a post-retirement
write carries the retirement resolve prerequisite that proves the new base.

## 9. Alignment and continuing anti-entropy

Direct push handles publications for routes known at the accepting Herald. A
one-shot snapshot is insufficient for a newly reachable delta: a publication may
reach its old context source just after the snapshot while the publisher still
lacks the new placement. Context is therefore implemented as a continuing,
revisioned anti-entropy subscription for as long as the relation exists.

### 9.1 Store revisions and class generations

Every active store incarnation begins at `StoreRevision` zero, which denotes the
empty retained-change prefix. It increments exactly once for each actual retained
winner, suppression, or joined-strength transition and appends that transition to
a retained prototype change log. Receipt alone, an equal or older duplicate,
readiness bookkeeping, and a visible-only `local-take` do not advance it. Revisions
are local to one `StoreIncarnationId`.

Each log entry retains the destination-free canonical semantic publication/source
evidence, exact `(SortId, SortDefinitionOccurrenceId)`, source-topology
prerequisite, Oracle-control prerequisite, and—for a structural source—the exact
structural occurrence/stamp needed to re-admit it. It retains no destination
delta/store set, frozen-route `PublicationBatch`, peer stream sequence, or
receiver-specific digest. Snapshot and change DTOs project this evidence into a
new checked alignment carrier; they never retarget an earlier peer batch.

Class generations are comparable across Heralds. For one quiescent reconciled cut,
each Herald computes:

```text
AlignmentPlanAttempt = (membershipChangeControlIndex, retryOrdinal)

AlignmentCut = {
    alignmentPlanAttempt,
    sortId,
    sortDefinitionOccurrenceId,
    topologyCut: canonical TopologyCut,
    physicalPlacementRevisionVector: {
        membershipGenerationId,
        memberSetDigest,
        components:
            ascending [(heraldEpoch, positivePlacementSequence)]
            with exactly one entry per active-generation Herald
    },
    exactMembers: sorted [(deltaId, storeIncarnationId, heraldEpoch)],
    predecessorGenerationIds: sorted [ContextClassGenerationId],
    freshMemberBaseEvidence:
        sorted [(deltaId, storeIncarnationId, baseRevision)]
}

ContextClassGenerationId = digest(canonical AlignmentCut)
```

`ContextClassGenerationId` is the sole canonical digest identity of the cut.
There is no `AlignmentCutId`, `alignmentCutDigest`, or independently admitted hash
that could disagree with it. The attempt's two unsigned words are encoded in
that order before the remaining cut coordinates. Ordinary plans inherit their
attempt; the reset rule below changes it. Placement sequences start at one; zero cannot stand
for either an empty vector or an unreported member. Every cut contains the complete
membership vector named by `topologyCut.frontier.structuralVersionVector` in
ascending `HeraldEpoch` order; a placement vector from another generation or member
digest cannot be combined with that topology cut.

Local structural reconciliation cannot create this record in advance: the
all-member established `TopologyCut`, placement revision, exact store incarnations,
and predecessor generations do not yet exist. It instead retains a
`StructuralConsequenceDebt` qualified by the exact structural occurrence, affected
sort/identities, predecessor resources that must remain live, and all local
placement/store inputs already known. After the required topology cut is installed
and the matching placement cut is available, `Herald.Alignment` deterministically
refines every covered debt into the exact `AlignmentCut`, generation,
cutover/input-closure, and destination-owned unsourced `AlignmentObligation`
records. The
obligation freezes semantic source class and path strength but no supplier or
certificate. That promotion is atomic and occurs before `AlignmentCutAccepted`,
historical certification, any context request, or release of an old-cut resource.
It is not a second structural application and does not change the applied
structural vector.

The embedded `TopologyCut` must be an all-member established cut from section 7.4;
a provisional local vector is not eligible. Its `topologyOccurrenceDigest` covers
each current winning induced-topology occurrence:
its structural publication and only those control revisions that change induced
structural graph identity, controller projection, or presence. For an edge it includes the edge identity,
endpoints, preserving/weakening strength and winning structural publication.
Deletion removes the object's entry; the digest does not encode a second entry
explaining its absence. Deletion controls and retirement facts remain independently
required for admission and stale-input suppression. It excludes label/authority-only revisions. Nabla or
delta label/control revisions are included only when they change that common
structural projection. The digest explicitly excludes Herald-local
`ActiveRootHere`/`RemoteController`, physical placement, Store incarnation, and
active-store state; those are qualified by the separate placement vector and
alignment cut. The digest
also includes the exact `(SortId, SortDefinitionOccurrenceId)` pairs on which typed
nabla, delta, carrier, and other permitted definitions depend, not merely the
graph's shape. An edge contributes no admitted-sort dependency. Republishing equal
`sort-sort` bytes within one definition occurrence does not create a topology
occurrence; retirement followed by redefinition, creation of a new topology
object, or deletion of an existing one does. Replacing a predefined topology
definition requires a fresh object identity; its non-label fields cannot be
rewritten in place. A label-only revision that leaves
the common structural projection unchanged does not. The physical placement vector
names one admitted complete snapshot for every active-generation Herald.
Two Heralds with different topology frontiers, graph digests, or placement cuts
therefore do not pretend to name the same generation; they wait/retry until the
evidence required by the destination matches.

Generation construction is serialized per sort occurrence on the established
`TopologyCut` chain. A Herald never constructs a shared generation from an
incomparable provisional vector. From the immediately preceding established
alignment cut it computes the predecessor set as the sorted unique union of:

1. every prior latest generation of the same
   `SortDefinitionOccurrenceId` whose exact member `DeltaId` set intersects the
   new class; and
2. every prior latest source-class generation whose context relation to this
   destination class is being replaced by the transition.

An exact new member contributes a fresh base iff its `DeltaId` is represented in
none of those selected predecessors. This rule depends only on retained cut/context
facts, never message arrival or provider choice. A Herald does not freeze the new
cut until every selected predecessor certificate and every required fresh base is
available and checked. Certificates gate construction and later bootstrap
admission, but certificate bytes and digests are not fields of `AlignmentCut` and
are not hashed; only the selected generation IDs and fresh-base evidence are. A
later ordinary topology/placement occurrence waits behind an incomplete
predecessor. An exact Store-loss report can instead authorize the explicit fresh
reset below; an absent certificate or delayed message alone cannot do so.

When classes merge, split, gain/lose a member, or change context, every resulting
class has a new generation. Its predecessor-generation IDs and fresh bases are
frozen when that topology/physical-placement occurrence is created and do not
change when readiness certificates are later advertised. A registry keyed by the
plan attempt, topology occurrence, complete physical-placement vector, and exact
member set returns the existing generation rather than recursively creating another one. A
fresh store with no selected predecessor representation contributes its captured
base revision; revision zero explicitly proves an empty retained prefix. Every
current member directly imports a captured prefix from every selected predecessor
and every required fresh-member base before it can claim local readiness. This
deliberately favors a simple non-circular all-to-all bootstrap over an optimized
coordinator.

Bootstrap subscriptions are authorized by a certificate matching one of the frozen
predecessor-generation IDs or fresh-member base evidence embedded in
`AlignmentCut`; their provider need not hold
a certificate for the **new** generation. They may feed only the named members of
that generation and cannot serve an external context obligation. Ordinary source
selection still requires the final `HistoricalCertificate`.

Members gossip sequenced evidence:

```text
ClassMemberReady(evidenceSequence, classGenerationId, memberStoreIncarnation,
                 predecessorAndBasePrefixDigest, memberStoreRevision)
```

There is no circular final-certificate dependency: cut-bound bootstrap evidence and
member-ready evidence are valid before the class certificate. A host issues a
source certificate only after it has the exact ready-evidence set for every member
in `AlignmentCut`. Stale evidence from another generation or member incarnation
cannot carry forward.

Each locally owned `ClassMemberReady` advertisement uses the directed logical
peer delivery lane in section 9.4. Its semantic `evidenceSequence` is distinct
from the lane's `AlignmentDeliverySequence`. An unconfirmed delivery remains
replayable for its exact destination across TCP binding replacement; confirmed
payload and key-index entries are reclaimed without discarding semantic readiness.

A historically ready source advertises this checked host assertion:

```text
HistoricalCertificate = {
    classGenerationId,
    sourceStoreIncarnation,
    exactMemberReadyDigest,
    completedPredecessorPrefixDigest,
    certifiedStoreRevision
}
```

The `classGenerationId` already authenticates the canonical `AlignmentCut`; a
second cut-digest field is forbidden. The certificate digest used to qualify a
later attempt is a digest of this complete certificate, not another name for the
cut.

The trusted hosting Herald is authoritative for its local store revision, but the
destination independently verifies the established topology frontier/digest,
placement cut, member set, and predecessor set from its retained views and accepts
only an exact match. Structural version coverage, SCC membership, or an opaque
host-local occurrence number is never by itself a completeness certificate.
`certifiedStoreRevision` is the frozen readiness base: later ordinary store
revisions flow through the continuing change stream and do not cause certificate
or generation churn.

### 9.2 Destination-owned subscriptions

The destination Herald computes context changes and creates:

```text
AlignmentObligationId =
    (destinationHeraldEpoch, localOccurrenceSequence)

AlignmentObligation = {
    id,
    causeStructuralOccurrenceId,
    sortId,
    sortDefinitionOccurrenceId,
    destinationClassGenerationId,
    destinationStoreIncarnations:
        sorted [(deltaId, storeIncarnationId)],
    sourceClassGenerationId,
    frozenContextPathStrength: Normal | Weak
}

AlignmentAttempt = {
    obligationId,
    subscriptionId,
    sourceHeraldEpoch,
    sourceStoreIncarnation,
    historicalCertificateDigest
}
```

Promotion creates only the `AlignmentObligation`. Its source class is semantic; it
does not choose a Herald or store and contains no certificate digest. Using the
independent certificate advertisements, the destination later deterministically
selects one connected, historically ready representative and creates the
certificate-qualified `AlignmentAttempt`. The attempt projects, but cannot change,
the obligation's destination stores or frozen path strength.

The obligation is stable semantic work; the attempt and its subscription are
disposable resource choices. A checked
`AlignmentCancel(subscriptionId, AlignmentSourceIncarnationLost)` for the exact
active attempt retires that attempt, records its exact
`(sourceHeraldEpoch, sourceStoreIncarnation)` coordinate in a run-global exclusion
set, and selects the least eligible non-excluded certified representative.
Reselection creates a fresh `subscriptionId`; it never retargets or continues the
old transcript. If no candidate remains, the obligation remains unsourced and
pending with no readiness evidence. The owner retains a terminal receipt for the
retired subscription identity, so every late old-attempt frame is idempotently
terminal and cannot contribute applied evidence to its replacement.

TCP binding loss, reconnect, reordered retry, or a deadline is not
`AlignmentSourceIncarnationLost`. Transport repair reoffers the same subscription
and contiguous transcript and changes neither source availability nor any
obligation, attempt, counter, or exclusion. The source-loss input is a checked
kernel boundary fact; profile 0.1 does not derive it from a socket close.

Before creating or sending that attempt, the destination must have the source
Herald's own `AlignmentCutAccepted` for the exact topology and placement cut.
Componentwise structural-vector coverage alone is not enough: a later graph may
have removed the relation, while a lagging source may not yet have created it. An
ahead source may serve the old cut only while it retains that exact generation and
certificate; otherwise the destination recomputes against a successor cut. Equal
structural fields do not imply equal occurrence identity. The Oracle does not
select a source or receive retained data.

### 9.3 Snapshot-plus-change protocol

Messages are:

```text
AlignmentSubscribe(obligationId, subscriptionId, sourceStoreIncarnation,
                   sourceProof, destinationStoreIncarnations)
sourceProof =
    FinalCertificate(sourceClassGenerationId, certificateDigest)
  | BootstrapProof(newClassGenerationId,
                   predecessorCertificateOrFreshBaseEvidenceDigest)
AlignmentSnapshotStart(subscriptionId, baseRevision,
                       semanticSnapshotDigest, chunkCount)
AlignmentSnapshotChunk(subscriptionId, chunkNumber, retainedStates)
AlignmentSnapshotEnd(subscriptionId, baseRevision, semanticSnapshotDigest)
AlignmentChange(subscriptionId, storeRevision, retainedTransition)
AlignmentLive(subscriptionId, throughSourceStoreRevision)
AlignmentAck(subscriptionId, appliedSourceStoreRevision)
AlignmentCancel(subscriptionId, reason)
reason = AlignmentRelationRemoved
       | AlignmentSourceIncarnationLost
       | AlignmentDestinationIncarnationLost
```

For `FinalCertificate`, the provider validates its current advertised certificate.
For `BootstrapProof`, `newClassGenerationId` is itself the digest of the exact
previously admitted `AlignmentCut`; there is no duplicate cut-digest operand. The
provider validates that cut, the matching selected-predecessor certificate or
fresh-base evidence digest, requester, and destination member set. Bootstrap may
feed only members named by that new generation. Thus the exceptional
pre-certificate path is representable and cannot be reused as an ordinary external
context source.

The source linearizes snapshot capture at `baseRevision`, sends the complete hidden
retained state, then sends every change with revision greater than that base in
contiguous order. Changes arriving on any peer stream or from another alignment
after snapshot capture create later store revisions and therefore cannot fall into
an unclassified window. The source retains snapshot chunks and change-log entries
until the subscriber acknowledges them or the relation is explicitly cancelled.

`semanticSnapshotDigest` is computed over the canonical ordered destination-free
retained states plus the source class generation, source store incarnation, and
`baseRevision`. `chunkCount`, chunk numbers, boundaries, encoded frame sizes, and
transport segmentation are excluded. Repartitioning equal semantic state into
different chunks therefore preserves the digest. The destination stages the chunks
and validates the complete reconstructed canonical state at `AlignmentSnapshotEnd`
before committing that snapshot prefix.

The destination validates the designated source and certificate/cut binding,
descriptor and exact immutable sort occurrence, canonical publication and object
identities, revision transcript, and frozen destination incarnations, then applies
snapshot and changes through the ordinary winner transition with applicable
controlled lifecycle suppression. A retained representative and its potentially
distinct strength witness are previously admitted Store evidence. Their original
source, control and topology coordinates remain immutable provenance; the
receiver does not repeat original writer-authority admission or require its old
label/topology history solely to accept alignment. Canonical primordial seed
evidence retains its separate seed-identity check. Routed structural evidence
still checks its intrinsic publication/stamp/carrier consistency and recomputed
digest. This rule does not authorize new ordinary publications or bypass the
exact attempt/source admission above.
The retained source strength is attenuated by the obligation's frozen
`Normal | Weak` context-path strength; the attempt and a later graph cannot upgrade
it. The destination becomes historically ready for the obligation after the
initial snapshot and a contiguous `AlignmentLive` prefix are applied. The
subscription nevertheless remains live and propagates subsequent source
transitions until the context relation disappears.

`AlignmentLive(subscriptionId, r)` is the only incoming revision-prefix fact: it
states that the source emitted every retained transition through source revision
`r` on that subscription. `AlignmentAck(subscriptionId, r)` states that the
destination has applied the snapshot/change/live prefix through the same source
revision. The acknowledgement releases the corresponding retained source transfer
prefix. It has no second child-release meaning. The protocol defines no
`AlignmentHandoffMarker`, `AlignmentRevisionMarker`, or `AlignmentHandoffAck`
constructor.

Transfer repair after reconnect is destination-rooted. The destination reoffers its
retained `AlignmentSubscribe` (or a terminal cancellation that the destination
itself originated), but does not proactively replay its cumulative `AlignmentAck`.
The repeated subscription makes the source reconstruct the retained unacknowledged
snapshot/change/live suffix or its terminal cancellation; replayed source data then
causes the destination to emit its exact current cumulative acknowledgement. A
source-incarnation-loss cancellation received from the source is not reflected back
to that source. This reconstructs the remaining exchange without a full transcript
dump from both endpoints.

A duplicate snapshot/change is idempotent; an older change cannot defeat newer
live data. An old subscription cannot discharge a new equal obligation. Source
incarnation loss follows the stable-obligation/fresh-attempt rule above.

A checked `AlignmentDestinationIncarnationLost` for any exact destination store
tombstones the whole alignment-plan coordinate
`(alignmentPlanAttempt, sortId, sortDefinitionOccurrenceId, topologyCutId,
physicalPlacementRevisionVector)`, not merely the one attempt that first observes
it. Checked disappearance of an exact remote placement member applies this same
whole-plan invalidation on every replica, including after all one-time imports
have finished and no subscription remains to carry a cancellation. The retained
exact Store-loss fact also invalidates a current plan admitted after that loss;
missing placement history or a disconnected peer alone is not evidence of loss.
Superseded generations retain their historical predecessor role. The owner
discards every active obligation, attempt, staged prefix, and incomplete import
for that coordinate, retains only the
minimal tombstone needed to make late snapshot/change/live/ack/cancel frames
idempotently terminal, and makes the coordinate's retained structural debt live
again once every promotion coordinate for the same cause and sort occurrence is
invalidated. A debt with no promotion considers installed cuts in chain order and
chooses the earliest covering cut whose historical structural projection matches
the application-Delta routes at the selected physical-placement vector. This lets
successive reader transfers settle even when placement has advanced beyond the
first covering cut before the debt can promote. Each cause retains its own
promotion receipt when several causes select the same cut; predecessor imports,
certificates, and input-closure obligations still apply. All-invalidated recovery
considers installed cuts in chain order at or after the latest incumbent topology
and chooses the earliest covering
cut whose historical structural projection matches the application-Delta routes at
the selected physical-placement vector. Spontaneous promotion and anchor replay use
the same selection; the latter applies it to the vector supplied by the announced
cut. A fresh placement vector may therefore recover a replacement Store incarnation
at the same topology. The exact tombstoned plan identity remains blocked. An
explicit reset has a distinct attempt even when its topology and placement match
a rejected plan. Promotion receipts and invalidation
tombstones remain immutable history. That history remains available when replaying
an already retained cut, but a tombstoned generation is never a prior-latest input
to a newly derived replacement, even when its historical certificate has arrived;
replacement ancestry therefore cannot depend on certificate arrival order.

There is no active terminal obligation waiting for the old store, no application
into an inactive historical slot, and no retargeting to its successor. If no
matching cut/vector with a replacement store is admitted yet, re-drive leaves
unpromoted debt rather than an active terminal obligation. A successor coordinate
receives new generation, obligation, attempt, and subscription identities;
promotion remains exactly once per coordinate.

A rejected plan whose usable predecessor or carried incarnation has been lost
produces one frozen report per `(planId, reportingHeraldEpoch)`:

```text
AlignmentPlanObsolete = {
    rejectedPlanId,
    reportingHeraldEpoch,
    lostStores: nonempty sorted unique [
        (homeHeraldEpoch, deltaId, storeIncarnationId, withdrawalPlacementRevision)
    ]
}
```

The reporter uses the observed withdrawal snapshot. If checked membership has
already retired the home and its current placement sequence is gone, the exact
birth cut's placement revision supplies the retained witness. The fixed announcer
holds these reports until current placement reaches every witnessed revision and
excludes each named incarnation, or current checked membership excludes its home.
It also waits for an installed topology matching that membership and placement.

The announcer then creates `AlignmentPredecessorReset`: no predecessor plan,
every binding newly created, no predecessor-generation IDs, and exact fresh-base
evidence for every current member. Within-plan context relations remain intact.
The attempt is `(membershipChangeControlIndex, retryOrdinal)`, ordered
lexicographically; genesis uses change index zero. The reset uses the selected
membership's checked Oracle change index and the next ordinal above all observed
attempts in that epoch. Ordinary descendants inherit the same attempt. A receiver
checks the reset epoch against the plan's exact membership history, so announcer
replacement cannot reuse an unseen old-announcer attempt.

Reset atomically cancels superseded executable owners and input-closure work,
preserves Store contents and immutable historical generations/readiness/
certificates, and installs the fresh complete plan. It can bypass missing ordinary
descendants. Older-attempt announcements and acceptances are inert; delayed
historical readiness/certificates can still complete captured history. The first
report's payload is immutable; later losses use a later reset attempt. Reports
use the reliable evidence lane in section 9.4, whose receipt reclaims only delivery
state. Retained semantic reports disappear when a newer reset supersedes them.
No periodic retry or response-to-receipt exchange is introduced.

For predecessor bootstrap, loss of a selected representative preserves the stable
bootstrap obligation and may select another certified representative of that same
predecessor generation with a fresh attempt. The replacement must establish its own
source-store input closure under section 9.4. A fresh-member base names the exact
new store and has no alternate source under the same coordinate; loss of that store
invalidates the plan and requires re-promotion. Once a one-time predecessor or
fresh-base import is locally fulfilled, later source loss does not reopen it.
Ordinary continuing context obligations still reselect for later changes.

The runtime supplies volatile crash-stop detection and exact incarnation loss derived
from an Oracle-committed non-voter retirement. Durable transfer storage, restart
reconstruction, voting-Herald loss, and production failure-detector calibration
remain deferred. This section specifies deterministic kernel behavior only after
the exact incarnation-loss fact has been checked.

### 9.4 Split, removal, and predecessor input closure

Old-cut input-closure work survives ordinary graph/placement advancement until
its prerequisites complete. An explicit reset retires superseded executable work
under section 9.3. The lexicographically least active `HeraldEpoch` in the plan's
membership is the fixed announcer of the complete routing plan, including empty
plans and plans that carry unchanged generations. Peers replay its exact topology,
placement and predecessor policy; a class announcement cannot independently
select another complete plan.

```text
AlignmentPlanId = (alignmentPlanAttempt, sortId, sortDefinitionOccurrenceId,
                   topologyCutId, physicalPlacementRevisionVector)
AlignmentPredecessorStatus = Usable | Invalidated | Reset
AlignmentPlanAnnounce(planId, canonicalTopologyCut, optionalPredecessorPlanId,
                     predecessorStatus, completeClassBindings,
                     completeContextRelations, createdClassCuts)
AlignmentPlanAccepted(planId, heraldEpoch, heraldPublicationPrefix)

HeraldPublicationPrefix = Empty | Through(HeraldPublicationPosition)
AlignmentCutAnnounce(classGenerationId, canonicalAlignmentCut)
AlignmentCutAccepted(classGenerationId, heraldEpoch,
                     topologyCutId, physicalPlacementRevisionVector,
                     heraldPublicationPrefix)
AlignmentRouteCutoverMarker(planId, classGenerationId, sourceHeraldEpoch,
                            predecessorHeraldEpoch)
```

Each binding names its exact class members, generation and `Created | Carried`
disposition. Created cuts must match the plan's full attempt and coordinates;
carried generations retain their original birth cuts. Acceptance of the complete
plan supplies the immutable cut acceptance for its created classes and preserves
the birth acceptance of carried classes. Class-only cut messages remain available
for retained generation evidence outside an explicit plan; they do not override
fixed plan authority. Reset announcements have the fresh-base shape in section
9.3 and require no skipped predecessor plan.

`Empty` is an explicit empty prefix and is never encoded as position zero;
`Through(p)` names the positive greatest accepted position frozen against the old
cut. Accepting the cut is one local transition: it records the exact topology and
placement cut plus this `heraldPublicationPrefix`, and switches the routing view so
every later `HeraldPublicationPosition` is frozen only against the new cut. This
acceptance is the supplier-specific evidence a destination must possess before it
sends a context request; `StructuralAppliedReport` alone is insufficient.

`AlignmentCutAccepted` is not a private reply visible only to the announcer. After
accepting, the Herald queues that immutable evidence for every other
active-generation Herald, including the announcer when remote. The receiver
installs it under the exact accepting Herald and generation only after recomputing
the `AlignmentCut` digest, matching the established topology cut and admitted
placement vector, and validating the accepting/receiving epochs. Same
acceptor/generation with unequal fields and wrong-member evidence are protocol
faults. Thus an arbitrary H1 can independently repair and retain H2's own
acceptance before selecting H2 as a context source; announcer receipt or a
transient connection is not the authority.

Plan/cut acceptance, member-ready evidence and obsolete reports share one
positive, monotonically allocated
`AlignmentDeliverySequence` per directed logical Herald pair. The sequence belongs
to those Herald epochs and survives TCP loss or binding replacement. It is
independent of the publication stream, generation identity, and semantic
member-ready evidence sequence. The wire vocabulary is:

```text
AlignmentRetainedEvidence = PlanAcceptance(AlignmentPlanAccepted)
                          | CutAcceptance(AlignmentCutAccepted)
                          | MemberReady(ClassMemberReady)
                          | PlanObsolete(AlignmentPlanObsolete)
AlignmentEvidenceDelivered(deliverySequence, AlignmentRetainedEvidence)
AlignmentDeliveryProgress

PeerControlEnvelope(oppositeDirectionReceipt, peerControl)
PeerPublicationEnvelope(oppositeDirectionReceipt, peerStreamItem)
oppositeDirectionReceipt: ReceiptRetirement(highWater, unresolvedPositions)
```

`AlignmentRetainedEvidence` is a closed four-arm payload, not a recursive wrapper
around arbitrary peer controls. The receipt uses the shared `ReceiptRetirement`
representation: an absent high water denotes no received delivery; otherwise every
positive position through the high water is received except its explicitly listed,
strictly ascending unresolved positions. Sparse gaps allow deliveries overtaken
across binding replacement to remain unconfirmed. Zero is not a delivery position.
The receiver retains only this high water and gaps as delivery history.

A receipt advances only after the semantic transition has admitted the evidence
or retained its exact payload and unresolved dependencies in owner state. Receipt
therefore proves retained delivery, not historical readiness or permission to use
a source. A newly rejected evidence payload cannot advance its own delivery
receipt. Its valid opposite-direction receipt may still release previously sent
work: that progress is independent of the new payload's semantic outcome.
Malformed wire envelopes fail RPC admission as a whole. For an admitted envelope,
an invalid receipt cannot partially commit its accompanying payload. Already
received delivery positions
skip semantic reapplication but request another cumulative receipt, repairing a
lost progress frame. Receipts beyond the sender's allocated frontier are protocol
faults, and a superseded physical binding cannot release current deliveries.

Every outgoing established control or publication envelope carries the current
receipt for deliveries in the opposite direction. Receipt headers remain outside
the immutable semantic publication and alignment digests. If dirty receipt progress
has no outgoing carrier, the serving Herald arms one idle timer at `T / 500`, where
`T` is its configured takeover target. Any intervening outgoing control or
publication piggybacks that progress and cancels the timer. Expiration emits one
`AlignmentDeliveryProgress` control with the receipt header; this control is
unsequenced and creates no receipt obligation or acknowledgement loop. Stale timer
callbacks cannot consume a newer timer attempt. Hello and heartbeat envelopes do
not carry semantic delivery progress.

The sender retains only unconfirmed payloads and their key-to-sequence index.
Reoffering the same outstanding immutable evidence reuses its delivery position.
Cumulative receipts reclaim covered payload and key entries. Once a logical peer's
initial evidence catalogue has been seeded, reconnect replays its indexed
outstanding deliveries rather than regenerating confirmed facts from semantic
history. Evidence produced while the peer is disconnected is queued for that
logical destination and becomes replayable on reconnect. Announce and historical
certificate repair retain their own existing rules.

Membership retirement releases the retired destination's delivery state and idle
timer. This delivery reclamation does not reclaim alignment generations,
certificates, cut acceptances, predecessor ancestry, or other semantic history.
Those facts remain subject to their own ownership and lifetime rules. These wire
changes have no compatibility decoder or version negotiation; all producers,
consumers, fixtures and golden encodings change together, and measurements start
with fresh experimental runs.

Control delivery order is not a dependency proof. An exact, well-shaped
`AlignmentPlanAnnounce`, `AlignmentPlanAccepted`, `AlignmentCutAnnounce`,
`AlignmentCutAccepted`, `ClassMemberReady`, or
`HistoricalCertificate` that names topology, placement, generation, member-ready,
or certificate evidence not retained yet is kept with its authenticated source and
re-evaluated to a fixed point whenever those facts advance. Equal replay is
idempotent; unequal evidence at the same immutable coordinate faults. In
particular, neither a persistent connection nor reconnect ordering may cause an
ahead announce/evidence message to be forgotten. Superseded executable
announcements and acceptances are successful no-ops; historical readiness and
certificates remain admissible. Obsolete reports have their own retained semantic
owner rather than entering the missing-dependency inbox.

Because an edge is universal, adding or removing it can create cutover and
input-closure work for every affected effective sort, never only a filter-selected
subset. An ordinary non-delete relabel does not change graph reachability. It
creates a new authority tenure only when it assigns
a different live process; a transition from process or zombie to void retains the
prior tenure as inapplicable history. Each alignment generation and subscription
remains keyed by its own sort occurrence.

Every active-generation Herald must locally accept the new `AlignmentCut`, stop freezing new
publications against the superseded cut, and settle every publication in its
recorded `heraldPublicationPrefix` whose frozen old cut names a predecessor. The
call is either terminally rejected/cancelled before publication, or its completely
retained direct publication is inserted in every applicable predecessor stream.
When the source and predecessor hosts differ, the source then appends an
`AlignmentRouteCutoverMarker` to that stream. A predecessor host records its own
source-prefix settlement locally and never sends a marker to itself. Work held
behind a data-plane dependency or label fence blocks cutover instead of being
overtaken by the marker. Controlled target publication has no Oracle-approval wait.

The route-cutover marker may reach `PeerStream` before the unordered anchor/class
control that creates its generation. In that case its exact logical item remains
`received-pending`: it is not semantically committed and does not advance the
completed prefix. Generation promotion re-drives it exactly once, and only when
every earlier logical item in that direction is completed; therefore neither the
control-plane race nor reconnect can let the marker overtake a publication.

For each predecessor store, the old cut freezes the exact set of incoming alignment
closure owners. The predecessor retains an input-closure obligation for each such
owner even if the new cut removes that context relation. Each obligation uses the
stable-obligation/disposable-attempt rule: exact source-incarnation loss selects a
fresh attempt, while absence of an eligible representative leaves closure pending.
After retaining the complete generation-qualified source cutover set—its local
settlement plus one route marker from every other active-generation Herald—and applying a revision-qualified
`AlignmentLive` prefix for every exact incoming closure owner, with the corresponding
ordinary `AlignmentAck` returned, the predecessor atomically records:

```text
PredecessorInputClosureReceipt = {
    newClassGenerationId,
    predecessorHeraldEpoch,
    exactSourceStoreIncarnation,
    throughRevision,
    incomingPrefixes: sorted [{
        subscriptionId,
        appliedRevision,
        liveRevision,
        acknowledgedRevision
    }]
}
```

The generation resolves the exact non-tombstoned plan coordinate; the retained
cutover evidence must contain the predecessor's one local settlement and exactly
one marker from every other active-generation Herald, and `incomingPrefixes` must map
bijectively to the frozen incoming closure-owner set through their current
attempts. Each prefix satisfies
`appliedRevision >= liveRevision` and
`acknowledgedRevision >= liveRevision`. `throughRevision = q` is the local Store
revision after those inputs; it is a lower bound, not a freeze. Later local
publications and continuing alignment changes may advance the store through `q + 1`
and beyond. The closure receipt is internal owner state, not a peer sum arm: there
is no route-set-digest transfer control,
`AlignmentHandoffMarker`, captured old-destination retention, or child
acknowledgement gate. Once local closure at `q` exists, an input-closure-only
subscription for a removed relation may be cancelled independently of every child.

A predecessor-bootstrap subscription may send a snapshot and changes before its
source Store's input closure is recorded. Its first readiness-counting
`AlignmentLive(bootstrapSubscriptionId, r)` is withheld until that exact source
store has a `PredecessorInputClosure` and the transfer has sent through
`r >= throughRevision`. The child's ordinary `AlignmentAck` acknowledges only the
applied revision prefix; bootstrap delivery/retry and closure-subscription release
are otherwise independent.

If a selected source is lost before local input closure, that closure obligation
reselects and the replacement attempt must complete its prefix. If it is lost after
one source Store's input closure is recorded but before a child fulfils bootstrap,
the child's stable bootstrap obligation may select an alternate representative only
after that alternate establishes its own source-store closure. Loss after local fulfilment
does not reopen the one-time import. With no candidate, closure and readiness stay
explicitly pending rather than inventing completeness. A delayed publication
accepted under the old cut is therefore represented before `q`; after a fixed
source's route-cutover marker it cannot accept another old-cut route.

## 10. Label visibility barrier

The Oracle state transitions are defined in
[04-oracle-and-raft.md](04-oracle-and-raft.md#7-label-state-machine). The prototype
retains at most one successful but incompletely installed label decision per
object. Fresh conditional requests for that object wait for its slot; labels of
unrelated objects, ordinary application work and Oracle traffic continue. Busy
admission is a transient deferral, not an additional `NotApplied` condition.

Labels and expected arguments carry `(LabelOwner, Word64)` through EAPP,
canonical values, EORC and peer publications. Private/global translation changes
only owner identity. A successful decision records exactly one successor
generation; decision, retry, reconnect and checkpoint replay install that recorded
pair without incrementing it again. Successful delete records its successor;
automatic disappearance preserves the prior generation.

### 10.1 Accepting and settling the selected publication cut

Before semantic acceptance, the home checks genuine local capability, possession,
nameability and immutable object-shape facts. It waits while an earlier selected
structural publication is unsequenced or the membership successor lacks an
established structural base. The retained candidate contains only session/request
identity and the exact call for retry/conflict classification. It owns no process
position, local fence or Oracle intention, so later provider calls can release the
missing structural stage.

At acceptance the home installs the caller/object fence in the process-scoped
acceptance order and reserves an exact Oracle request. The publication group
contains the caller's previously accepted publications that use the target as
sequencer or publish the target itself. Captured immutable memberships prevent
another caller's publication from entering this drain. A publication matching
both arms is counted once. Later selected work remains held outside the drain.

The group settles when local effects and its frozen per-destination assignments
have completed, or their destinations have canonically retired. Existing contiguous
peer completion may wait behind unrelated stream holes. Receipt alone is
insufficient. No label marker or new all-stream fence is sent. An empty or already
settled group can submit immediately.

The settled request is `DecideLabel`: decision/request identity, home, caller,
object, expected full label pair, optional independent `InitialLabelEvidence`,
first-overlay authority evidence, target and acceptance cut. Initial evidence
binds the raw generation-zero label to a checked bootstrap root or publication;
expected never supplies its own comparison state. The Oracle normalizes canonical
End before comparison. Existing canonical records supply revision and authority,
so stale local provenance does not add another caller-selected CAS operand.

Exact request retries precede fresh admission. An occupied installation slot
returns `OracleSubmissionDeferred` for the original identity and bytes, both at
preflight and in serialized application. It creates no receipt or control event.
The home retries after the blocker completes. Unrelated control entries do not
cause repeated rechecking, and canonical End has no label-phase deferral.

### 10.2 Immutable decision and local installation

One committed `LabelDecided` contains the checked decision, immutable terminal
outcome and digest. The decision references an already installed membership
generation, without broadcasting an H-member roster to every Herald. Checked
membership history resolves the captured participants.

For `NotApplied`, the Oracle changes no label and immediately releases the slot.
The home applies the failure, releases its selected later work for revalidation
and returns the result. No receiver installation collector or completion command
is required for this arm.

For `Applied`, the same entry installs the canonical label/delete, revision and
authority consequence. Each receiver builds a checked direct patch against its
current owner state. The patch atomically installs the overlay, Store/placement
consequences and ownership of required structural/context work. It retains no
precomputed successor leaf substate. Unrelated work already applied is preserved.
Only the target and actual dependencies are held while installation is pending;
there is no universal application-observation gate.

A locally absent defining publication does not prevent the canonical overlay or
delete suppression from being installed. Later descriptions and forwards resolve
against that overlay. Endpoint activation still requires normal possession and a
known immutable sort. Store replacement remains a genuine structural effect where
the endpoint handover requires it.

Endpoint admission is distinct from sequencing membership. Relabelling a nabla
does not fence every write through it merely because it is the endpoint. Its old
owner's home closes admission for new endpoint uses when accepting a handover.
Already accepted publications retain frozen authority, destinations and control
prerequisites; later arrivals are validated against that evidence. New-owner work
uses the new authority/control prerequisite, and queued work revalidates before
acquiring authority.

Same-object installation and relevant End events remain ordered locally. A later
End may supersede the effective owner without erasing the historical fact that the
Applied decision was installed. Acknowledgement therefore certifies installation,
not mere control receipt. The home releases selected later publications only when
its corresponding local outcome permits revalidation.

### 10.3 Direct installation collection

The normal successful path is:

```text
home -> Oracle: DecideLabel(...)
Oracle -> Heralds: LabelDecided(decision, Applied(...), outcomeDigest)
Heralds -> collector: LabelInstalled(decisionId, reportingHeraldEpoch,
                                    decisionIndex, outcomeDigest)
collector -> Oracle: CompleteLabelDecision(decisionId, decisionIndex, outcomeDigest,
                                          collectorEpoch, membershipGenerationId)
Oracle -> Heralds: LabelWorkflowCompleted(decisionId, outcomeDigest)
```

The normal path has two semantic Oracle commands, H installation facts and O(H + R)
messages/bytes for H Heralds and R Raft replicas. The completion payload contains
no roster or individual reports. The outcome digest binds the exact typed
`ECLIPS-LABEL-TERMINAL-OUTCOME` transcript; valid local graph and Store state need
not be equal and are not serialized into that witness.

The sender's live peer binding must identify the reporter. Future-index reports
wait for the matching Oracle prefix. Equal reports are idempotent; conflicting
facts cannot count. Reports for completed decisions do not recreate pending work.
The collector counts its own installation locally and emits one checked completion
only when every captured survivor is accounted for.

The home is collector while active. Its canonical retirement assigns the least
surviving captured epoch. Each receiver retains its installation fact through
canonical completion and resends it after peer-binding replacement or collector
reassignment. An unchanged binding and assignment do not generate repeated sends.
Transport receipt or acknowledgement cannot reclaim semantic installation evidence.

Oracle completion admission checks exact identity/original index/digest, assigned collector and
current membership generation. An unrelated control entry does not invalidate the
attestation. An uncertain submission retries identical bytes and request identity;
a definite stale-generation rejection permits a refreshed request. A captured
survivor may rediscover a retained completed decision/outcome even after the
assignment or generation changes. Each Herald separately promises a consumed
label prefix, piggybacked with receipt progress and flushed on idle. The promise
waits for every local consumer and exact completion offer to settle, including
offers from a former collector. The Oracle reclaims completed witnesses through
the minimum active-member promise; only canonical retirement removes a member.
An absent decision at or below that floor gets a typed obsolete-completion
rejection without another completion event. Exact retained request receipts and
request-retired classifications retain their existing precedence. The
[Oracle lifetime contract](04-oracle-and-raft.md#5-command-submission-and-retry)
defines progress correlation and checkpoint invariants.

`LabelWorkflowCompleted` frees the object's slot, releases historical installation
evidence and permits the successful application result. Caller End leaves cleanup
running. Retirement preserves Applied, removes only retired reporters and completes
vacuously if no captured participant remains. Suspicion, timeout and socket loss
cannot shrink the required set. Peer isolation therefore prevents success until
report delivery or canonical retirement, even with a healthy Oracle connection.

## 11. Predefined-disappearance evidence and deferred obsolescence reclamation

Publishing an obsolete controlled instance is an ordinary direct data-plane
operation. The Oracle neither approves it nor checks that the object is still
current at publication time. Every Herald records monotone obsolete suppression,
so an older or delayed current instance cannot make that object current again
while the suppression fact is retained.

Profile 0.1 retains obsolete suppression for its complete finite run and has no
reclamation message or command. In a later profile the Oracle may participate in
**reclamation**: it can order an evidence
workflow that establishes that every active-generation Herald has recorded the obsolescence
and that the bounded-latency/clock-skew safety interval has passed, after which all
Heralds may discard the agreed obsolete payload or suppression together. The
future reclamation reports would concern observation and safe deletion of retained
negative state, not retroactive permission to publish. No timeout or Raft quorum substitutes for
the required captured-generation evidence; the exact `T`/`D` deployment contract is owned by
the semantic/profile specification.

An Oracle-opened disappearance probe names either a controlled predefined object or
a regular `SortDefinition` `(SortId, canonicalDescriptorDigest,
expectedOccurrenceId)` and uses the same peer stream markers to capture a fixed
active-Herald cut. It also cuts every continuing alignment channel. For each live
subscription at the captured local cut, its source records the current
`StoreRevision`. For a remote subscription it sends:

```text
AlignmentProbeMarker(probeId, subscriptionId, throughStoreRevision)
```

The marker is ordered after every change through that revision. A destination
drains the subscription through the named revision and includes the exact
`(subscriptionId, throughStoreRevision)` set in its absence evidence. Creating,
replacing, or cancelling a live incoming subscription while the probe is open
invalidates the probe; it cannot silently change the captured channel set.
A subscription whose source and destination share one Herald retains and settles
that exact source revision through the local owners. It contributes to the same
captured evidence set without a self-directed peer frame or a fabricated binding.

Either publication-stream or alignment marker may arrive before the receiver's
contiguous Oracle watch has applied the corresponding Open. The receiver retains
such a marker behind the exact probe prerequisite without accepting its subject or
membership claims, then redrives it when `OracleAdvance` applies the matching Open
projection. It neither rejects an unknown-yet-valid marker merely because EPRP
outran EORC nor applies a cut before Oracle authorization.

For a regular-definition subject, applying the open command also captures each
Herald's local `HeraldPublicationPosition` and gates matching later `sort-sort`
writes. Before a Herald appends a probe marker, every matching publication accepted
at or before that position must be terminally rejected or inserted ahead of the
marker in every applicable peer stream and local held/applied work set. This
includes accepted work not yet assigned to an outbox. Matching later work stays
held as complete items until it first invalidates the probe or a resolution
releases it as a post-retirement occurrence carrying the resolve prerequisite.

A Herald reports local absence only after:

- all probe markers have been applied;
- every captured alignment subscription has completed through its probe marker;
- no application-visible delta at that Herald contains the definition;
- no admitted or held matching publication can reveal it;
- no same-sort alignment obligation or captured snapshot could contain it; and
- the report still names the current store and Herald epochs.

For a regular-definition subject it additionally reports that no local semantic
dependant uses that occurrence: no visible or retained value, active/passive typed
entity, unresolved structural reference, peer work, alignment snapshot/change log,
or other retained work requires the sort. Each Herald also reports that no locally
retained current or obsolete controlled binding uses the `SortId`; the Oracle
checks the complete report set rather than consulting a canonical target-object
registry. Any uncertain retained payload conservatively blocks the report.

A universal edge is not a semantic dependant of a `SortDefinition` merely because
traffic of that sort may traverse it: the edge contains no sort reference. Typed
nabla/delta or carrier definitions and retained data still are dependants. Deleting
or disappearing an edge reconciles graph and context generations for every
affected effective sort.

Hidden retained copies of the `SortDefinition` itself left by `local-take` do not
block a regular-subject absence report; the exact captured cut makes them purgeable
or convertible to the appropriate suppression when resolution applies. Retained
values **of the defined sort** are semantic dependants and do block it. Matching
publication or alignment work invalidates an earlier report. For a controlled
subject, relabel work does as well.
The protocol therefore reports evidence, never a conclusion based on timeout or
socket loss. Controlled resolution installs terminal deleted suppression. Regular
resolution purges only the captured definition occurrence and installs
cut-qualified suppression; it never deletes controlled objects using that sort and
a genuinely post-resolution equal write may make the same `SortId` effective again.
Every captured Herald applies that retirement, including a participant that never
learned the descriptor. Such a participant retains the authenticated descriptor
digest, retired occurrence, Resolve index and successor coordinate without
inventing a descriptor or definition publication. Its first later definition must
match that digest and the latest successor occurrence; it cannot reintroduce an
earlier occurrence.
The Oracle decision is specified in
[04-oracle-and-raft.md](04-oracle-and-raft.md#8-predefined-disappearance).

## 12. Oracle client protocol

Each Herald starts with checked bootstrap voter contacts and may learn leader
hints. Committed Oracle configuration events replace the current preferred set: a
joint configuration uses the union, and a final stable configuration uses its new
set. Retained registration contacts are dialing hints. Neither contacts nor Hello
configuration progress grant native voting authority. The current profile-0.2
vocabulary is directional:

```text
client -> server:
OracleHello(systemId, catalogueDigest, configurationDigest,
            initialProjectionDigest,
            heraldId, heraldEpoch,
            lastAppliedControlIndex, membershipGeneration)
SubmitOracleCommand(canonicalOracleEnvelope)
WatchOracle(fromExclusiveControlIndex)
GetOracleRequestResult(clientRequestId)
OracleHealthQuery(round, helloClaims)

server -> client:
OracleHelloAccepted(raftNodeId, currentTerm, localAppliedControlIndex,
                    leaderHint?, serviceReady)
OracleRedirect(leaderHint?, currentTerm)
OracleReceipt(canonicalOracleReceipt)
OracleRequestAbsent(clientRequestId, localAppliedControlIndex)
OracleSubmissionDeferred(clientRequestId, blockingLabelDecisionId,
                         localAppliedControlIndex)
CommittedOracleEntries(nonEmptyContiguousEntries)
OracleSubmissionNotReady(clientRequestId, currentTerm)
OracleHealthReply(round, node, term, configurationObservationGeneration,
                  effectiveConfigurationRef, stableOrJointVoterSets)
OracleProgressRetired(clientRequestId, committedProgress)
OracleProgressNotReady(clientRequestId, offeredProgress, currentTerm)
OracleRequestRetired(clientRequestId, retiredPrefixOrHome)
```

`OracleProgressDto` contains checked receipt retirement followed by the label
completion frontier as a `ControlIndexDto`. Standalone maintenance uses the
receipt high water as its routing request sequence and creates no ordinary
receipt. Its success response carries the full committed componentwise join;
its `NotReady` response echoes the exact offered snapshot. Binding and full-offer
correlation prevent an older response from releasing a newer same-high-water
offer. `OracleRequestRetired` closes the request's retry entitlement, distinct
from authoritative absence. These server constructors have tags 8, 9 and 10,
respectively.

EORC includes one-shot Oracle-health lanes to the same EORC framing. A query is the
first and only client message and its reply completes that lane. Both reject any
subsequent message; neither creates a command/watch connection. The query carries
the usual Hello claims. Admission checks the immutable run identity and the
querying Herald identity against the Oracle owner's current active semantic
membership, without waiting for leader readiness or the caller's history to catch
up. A semantically retired epoch receives no health reply, so missing its final
watch notification cannot keep renewing isolation grace. Explicit voter demotion
preserves this admission. An active source can observe a live native owner even
when it is leaderless or lacks quorum. An ordinary established lane rejects
health messages, and a health lane grants no request-absence authority.

The reply is produced by a fresh request queued to the native owner. It contains
the exact log-effective stable/joint configuration and reference, which may
precede the committed Oracle watch projection, and a local observation generation
advanced on effective-configuration append or truncation. Stopped owners supply
no cached reply. The observer correlates the round, endpoint/node identity, term,
configuration generation and exact voter sets before its pure quorum assessment.
Freshness and both joint majorities are required. These observations are runtime
liveness evidence only; they neither install an uncommitted Oracle projection
nor count as direct failure-probe reports.

The EORC family discriminator is `0x454f5243` (ASCII `EORC`). The authorized
Oracle conformance client exercises submit/query constructors and owns its request
sequence. Ordinary Herald command and watch traffic use their retained pure
Oracle-client owner.

The live process/label command family is `StartProcessEpoch`, `EndProcessEpoch`,
`DecideLabel`, and `CompleteLabelDecision`. Each uses the same canonical envelope,
receipt/query, Raft application, watch and projection path. The failure family adds
`OpenHeraldFailureProbe`, `ReportHeraldFailureProbe`, `DismissHeraldFailureProbe`
and `RetireHeraldEpoch`. Removed readiness/preparation/release/abort arms have no
compatibility decoder; deployments restart after the coordinated schema cutover.

The failure family is qualified by the exact captured stable voter
configuration. Open carries its identifier and projects the complete checked
configuration; Report names that same identifier. The EPRP direct probe is
`DirectFailureProbeRequest(probeId, targetEpoch, semanticMembershipGeneration,
voterConfigurationId)`, and the response echoes the entire request with its
Reachable/Unreachable result. The configuration claim has exactly 32 bytes and
is admitted through the Oracle configuration-identity constructor. Each reporter
begins its direct attempt only after applying the matching committed Open.
An old connection, old request, wrong configuration, or health reply cannot
substitute for that fresh probe.

`AcceptVoterHostFailure` commits the old-configuration certificate and the native
exclusion intention as one ordinary Oracle command. The final native stable
configuration projects `VoterExcludedAwaitingHeraldRetirement`; the subsequent
`RetireHeraldEpoch` consumes the retained certificate and completes the intention
with the semantic membership change. The accepted failure event retains target,
semantic generation, full old voter configuration, reports, resolution, and
acceptance ControlIndex. Neither final exclusion nor a later current voter set
changes its denominator. Exact request replay keeps its original receipt.

The disappearance family extends the process/label and failure families with
`OpenDisappearanceProbe`, `ReportPredefinedAbsence`,
`InvalidateDisappearanceProbe`, `ResolveDisappearanceProbe`, and
`AbortDisappearanceProbe`. Both controlled predefined objects and regular
sort-definition occurrences use the same envelope, receipt, Raft, and watch
path. Inner command tags 13–17 carry checked subject coordinates, full Report
claims, and exact probe identities; receipt result tag 3 carries the typed
accepted disappearance result. Projection tags 17–21 carry full Open headers,
Report claims, invalidations, terminal outcomes, and abort reasons. Rejection
tag 41 preserves the complete typed disappearance rejection. The surrounding
EORC message family is unchanged. The current Oracle state transcript includes
probe history, controlled-deletion facts, and regular-retirement indexes; all
producers, consumers, and goldens change together without a compatibility decoder.

Profile 0.2 adds command family tag 19 for replica registration and explicit
voter Begin/Cancel administration, accepted receipt result tag 4, and rejection
tag 43. Projection tags 23, 24 and 25 carry retained replica registration, voter
change and committed voter configuration respectively. Native configuration
observations are not commands: the closed applied-entry sum uses the
`ECLIPS-APPLIED-ORACLE-CONFIGURATION` constructor domain, exact ControlIndex,
canonical configuration, the configuration/change event vector, and post-state
diagnostic witness. It carries no request identifier or receipt. Ordinary command
entries retain their `ECLIPS-APPLIED-ORACLE-ENTRY` domain and receipt transcript.
Standalone progress uses the `ECLIPS-APPLIED-ORACLE-RECEIPT-RETIREMENT` domain with
its exact ControlIndex, home epoch, full progress, empty semantic event vector
and optional post-state digest. It carries no ordinary request or receipt.
Ordinary entries may also confirm piggybacked full progress. All three
constructors share the same contiguous EORC watch batches and codec boundary.
Replica registrations include exact node/active-host bindings, admission index,
and canonical UTF-8 endpoint hints for EORC, ERFT and restricted discovery.

Every applied-entry constructor and Oracle checkpoint encodes the optional
whole-state diagnostic witness as the canonical option byte: `0` means absent,
and `1` is followed by exactly 32 digest bytes. Other option tags are malformed.
Absence is retained explicitly in projections and exact entry comparison; it
does not match a present digest. The Oracle defaults to absent witnesses, with
one immutable mode across the run. Enabled checkpoints additionally validate
the full-state digest on admission; either mode retains canonical shape and
semantic checks. No compatibility decoder admits the earlier mandatory-digest
encoding. Operational command and workflow digests are unaffected.

An Applied label decision precedes its canonically ordered probe invalidations in
one vector. A membership advance and failure consequences precede the ordered
predecessor-probe aborts, which precede resident-process and label consequences.
Checked canonical decoding preserves these orders and receipt/event coherence.
The independent disappearance reference is confined to test sources.

`OracleSubmissionDeferred` identifies a fresh conditional label request blocked by
the pending installation slot. The service-ready leader's preflight avoids an
unnecessary proposal, and the serialized Oracle transition repeats the busy check
for proposals that race. Deferral is not a receipt or workflow result, consumes no
`ControlIndex`, and appears in neither request-result history nor the watch. The
Herald retains and resubmits the original request after canonical completion.
Exact retained retries and retired identities are classified before this check.
Canonical End remains independently orderable while installation collection runs.

`OracleSubmissionNotReady` distinguishes that serialized leader's transient
proposal unavailability from an authority-changing `OracleRedirect`. It names the
exact retained request and term, leaves the established lane intact, and causes a
resident Herald to delay and reoffer its still-pending request intentions. It
does not allocate a control index or imply another leader.

The reliable physical lane coalesces repeated level-triggered offers of one
request after its first successful send. A NotReady response marks only its
correlated request as retryable; expiry of the owner-minted timer releases those
marked requests immediately before the retained batch is offered again. Requests
already accepted by Oracle remain suppressed on that lane, while a replacement
lane starts empty and therefore reoffers every still-retained intention.
Every owner-minted retry schedule carries its pure connection-or-submission
purpose; the runtime never infers that purpose by scanning physical lanes.
Connection and submission delays have independent exponentially increasing
fruitless-round streaks, each bounded by the same one-second-or-configured-delay
ceiling. Connection retry starts at the positive configured delay. Submission
retry is the fallback behind the exact progress watch and starts no faster than
100 milliseconds, so ordinary Raft apply latency cannot drive polling faster than
ten hertz. A submission streak resets only for strictly advanced committed-prefix
or receipt-prefix evidence, the first tracked terminal receipt for an exact
request, or a strictly newer admitted service-ready leader term. Redirects,
NotReady responses, equal or regressed prefixes and terms, untracked or duplicate
receipts, ordinary Herald inputs, and binding establishment alone do not reset it.
All currently admitted NotReady requests share the one exact pending owner retry.
For every admitted response, the serialized owner first emits an exact
binding/request/retry association and only then, when necessary, its schedule.
The runtime neither marks a request before owner admission nor scans lanes to
infer association, so an elapsed retry crossing a later NotReady cannot lose or
prematurely release that request. No physical response can multiply timers.
Likewise, a deferral keeps its physical submission claimed until the serialized
Herald owner has admitted the deferral; the owner's ordered release action then
precedes any reoffer made possible by an already-completed blocking workflow.
An authoritative `OracleRequestAbsent` after a deferral proves that no request
record exists at that applied prefix; it neither cancels the retained End intention
nor prevents its exact request ID from being admitted for the first time later.

There is no generic `GetOracleWorkflowStatus`/`OracleWorkflowStatus` message.
Workflow facts derive from canonical application through the checked projection
and its replay base. Explicit administration exposes its own typed status surface.

The current-build Hello compares immutable system, initial configuration,
predefined catalogue and initial-projection commitments. It has no future-process
catalogue/digest field. Current Herald membership and process residence are checked
against the applied Oracle projection. A mismatch has one current-build rejection;
there is no compatibility path or protocol negotiation.

After that authorization, a server does not answer an otherwise valid Hello with a
leaderless, not-ready, no-hint response. If the contacted replica is neither
semantically service-ready nor able to name a distinct leader, it parks that one
Hello on the already-open connection until the owning coordination facts change.
It emits one `OracleHelloAccepted` as soon as either local service readiness or a
distinct leader hint makes the response actionable; a hint naming the contacted
node itself is not sufficient. The wait races that authoritative state change
against physical peer readability. EOF, or any pipelined client input while the
Hello is parked, retires the lane without consuming a second request; such input is
invalid before the Hello response establishes the lane. Runtime shutdown likewise
closes the parked connection. This is an STM wait, not polling, and therefore
cannot create a client reconnect loop while the cluster is electing a leader.

The current symmetric Raft family uses magic `0x45524654` (ASCII `ERFT`) and
exactly `RaftHello`, `RequestVote`, `RequestVoteResponse`, `AppendEntries`, and
`AppendEntriesResponse`. Both sides exchange and validate `RaftHello` before an
RPC. That hello carries a protocol-owned exact-width immutable genesis-digest claim
plus source/target `RaftNodeId` claims; the runtime compares its bytes with the
Oracle-owned checked initial digest, which binds the run identity, initial
node/host bindings and timer policy. The explicit transport catalogue can include
learners and retained historical peers; it is independent of native voting sets.
There is no equality check on mutable voting configurations. Exact source/target
binding remains required, including for a lagging receiver or a leader finishing
its own removal. Native quorum and log checks decide authority. Append entries
carry checked stable/joint configuration DTOs and opaque adapter metadata in the
same canonical entry stream as no-ops and applications.
A successful AppendEntries response carries both its exact matched index and
the receiving owner's acknowledged applied index, capped at that matched index.
A rejection carries applied index zero. These are native log positions, including
no-ops and configuration entries. Only a current-term, dispatch-correlated success
can attest to the leader's captured learner frontier; matching without application
does not permit promotion.
There is no separate System/Herald/cluster claim in
`raft-core`. Both protocol families reuse the common length/magic frame and have
no acknowledgement, version, negotiation, or compatibility constructor.

Both protocol packages own their structural DTOs and every `Binary` instance.
Named validating adapters construct core identities/inputs or invoke Oracle's
canonical decoder. They never define `Binary` for a type owned by `raft-core` or
`oracle-core`, and generic DTO bytes never become canonical Oracle identity bytes.

Commands are submitted to the leader. Followers redirect without inventing a
semantic result. A redirect carrying the first admitted Oracle term, or a term
strictly greater than the retained term, and naming a different member of the
checked fixed contact set is fresh routing evidence, so the client rejects the
old lane and contacts that member immediately. Repeated or regressed terms and
absent, unknown, or self-referential hints remain on the paced connection-retry
path. An initially contacted nonleader handling an empty watch at its applied tip
returns a redirect immediately under that client policy; it receives no
watch-authority grace. A behind-tip watch can still be served from the retained
suffix. The committed-entry watch is contiguous; a Herald does not skip an unknown
index and then apply later state. An empty watch already accepted by the local
leader at its applied tip receives one
bounded authority-grace interval per authority-loss episode, equal to the
configured upper bound of the Raft election timeout. During that interval, a
completed watch result, local applied-prefix progress, or recovered local
leadership retains the lane and wins the race, while a distinct known leader
redirects immediately. If authority remains unresolved when the interval expires,
the replica redirects the watch into paced fixed-contact rotation; absent and self
hints therefore do not retain the watch indefinitely. Runtime shutdown still
terminates it. Exact client request retry returns the same immutable command
receipt, while conflicting reuse is rejected.
Only a service-ready leader that has applied its complete committed prefix may
send terminal `OracleRequestAbsent`; follower or lagging-prefix absence redirects
or remains retryable. A command receipt never mutates from pending to terminal;
multi-command label/disappearance progress is the separate monotone workflow
status.

An established EORC watch worker must either emit its owner-provided contiguous
suffix or retire the physical lane. `OracleWatchUnavailable` closes that lane so
the client observes connection loss and resumes from its retained cursor. Failure
to encode an owner-produced contiguous suffix is a runtime invariant fault: the
runtime reports the encoding contradiction and then closes the same lane. It MUST
NOT leave an apparently established connection behind after its watch worker has
terminated.

The Oracle protocol carries process/label decisions and predefined-disappearance
or regular-retirement evidence. It has no current obsolete-suppression reclamation
arm and no message capable of carrying or approving a controlled or regular
publication payload, delta-store snapshot, graph, structural version vector, wait,
process-private identity map, or peer outbox. The compact structural activation
token described in section 7.4 is a planned fallback only and is absent from the
current EORC sum.

## 13. Connection loss and stale traffic

- Peer TCP loss retains logical outbox work and discovery/placement knowledge while
  both Herald epochs remain active.
- A new connection for the same epochs resumes stream acknowledgement state.
- TCP loss or reconnect never marks an alignment source unavailable, changes an
  alignment attempt/subscription, or mutates the global source-exclusion set; only
  checked `AlignmentSourceIncarnationLost` for the exact active attempt does.
- Traffic from an epoch absent from the current Oracle membership generation, or
  present only as a retired epoch, is stale and is terminally ignored. The profile
  MUST NOT attribute a removed or superseded
  epoch to a replacement with the same `HeraldId`.
- A stale `StoreIncarnationId` never targets a replacement store.
- An old label installation fact never satisfies a different decision.
- A Raft term has no effect on Herald epochs, process epochs, publication identity,
  or peer streams.
- No disconnect alone changes canonical membership, process liveness, label,
  controlled lifecycle, or global absence.

The runtime retains one absolute, generation-qualified recovery episode after binding
loss or heartbeat unresponsiveness. Successful authenticated recovery before its
deadline cancels that episode. Expiry produces only a local suspicion; eligible
voter-host Heralds then perform the fresh Oracle-probe protocol in
[04-oracle-and-raft.md](04-oracle-and-raft.md). Only committed retirement changes
membership and derives exact Store-incarnation/source loss.

On retirement, survivors close and stop dialing the target, terminalize its
dispatch attempts, reject its later handshakes and semantic traffic, and establish
the successor structural source from the union of work admitted by any survivor
before that survivor locally sealed the old source. A selected alignment source is
recomputed from its stable obligation; a lost destination obligation is discarded.

Retirement does not retroactively invalidate source liveness/authority proved when
a survivor admitted an ordinary publication. Such a retained item may still apply
only to its already-frozen surviving destination after outstanding data/control
dependencies hold; it is never retargeted. New retired-origin traffic rejects. The
only exception is `TerminalSourcePayloadRelay` from an active survivor, checked
against frozen inventory evidence and admitted into mandatory structural history as
section 7.4.1 specifies. Survivor-authored stamped work may cross through the
terminal predecessor without restamping, while an unsequenced survivor stage waits
for the successor base and then stamps in that generation.

A non-voting target that did not receive the retirement entry cannot infer the
majority's exact Oracle state. Its prolonged inability to regain an Oracle-voter
majority, or an explicit retired-epoch rejection, instead causes irreversible local
self-fencing. It sends `HeraldIsolationBegun`, overlays zombie labels for objects
controlled by its own resident processes, and permits only existing result lookup
and local `read` during a bounded isolation drain. Mutating calls and new/resumed
sessions reject; after the drain it sends/closes with
`HeraldPermanentlyUnavailable(HeraldIsolated)`, emits no new semantic network work,
and never merges that abandoned branch. Voter-host application fencing remains
separate from its supervised native Raft lifetime; fresh quorum health and checked
voter exclusion follow the Oracle/Raft contract without inventing a smaller quorum.


## 14. Restricted discovery and onboarding

The peer TCP listener recognizes the fixed EDSC lane before ordinary EPRP
candidate admission. Its canonical messages retrieve the original immutable
system bootstrap and exchange restricted onboarding requests. A seed address,
learned contact, decoded applicant manifest or successful physical exchange
provides no current Herald membership, publication authority or Raft voter role.

Onboarding requests run through the same supervised Herald owner as ordinary
inputs. Correlation identifies retained command results and exact retries; a
changed command under the same identity conflicts. Captured source histories are
immutable within one admission attempt. Separate contiguous control-history
exchange carries later seal, report and activation entries without changing the
captured data transcript. Transferred structural occurrences preserve their
original canonical payload, stamp, control and topology prerequisites.

A captured control transcript names a covered anchor C and contains exactly
the contiguous canonical suffix C+1 through its captured cursor B. Exact entries
retained for outstanding consumers belong to the checked Projection base rather
than this replay suffix. Portable base admission binds the anchor, cursor and
suffix to that Projection. History-only replay requires the receiver to have
already installed C, including for an empty suffix; the anchor itself grants no
permission to skip semantic installation. A separate control-history request can
start below the covered floor when an outstanding admission retains the complete
contiguous tail from that cursor. Missing tails and cursors ahead of the source
are rejected; sparse exact pins cannot fill a gap. Retained unequal overlap conflicts,
while already-covered overlap completes without recreating discarded entries.

On canonical Begin, each predecessor source retains the exact control tail after
Begin until its applicant has consumed activation, or the admission is cancelled
or the applicant retired. Observing Activate at the source does not discharge
this obligation. An accepted ordinary Hello from the applicant with a semantic
applied-control cursor covering that Activate entry does; reconnect repeats the
same evidence without a separate acknowledgement. A retired local source no
longer retains promises to serve applicants.

The applicant separately retains the controls needed to replace its private
staging. Complete source adoption advances its exclusive floor to the imported
source cursor recorded before replaying a newer local suffix. Local installation of
activation releases that reconstruction promise. Frozen control observation can
release cancelled or retired lifetimes, but neither creates new promises nor
counts as semantic installation. These retention lifetimes permit semantic
base coverage to advance independently of outstanding onboarding tails.

Completed ordinary Oracle batches, complete source adoption and complete joining
history requests reclaim the paired Client and Projection archives. Current
semantic facts remain in a checked replay base; exact outstanding consumer pins
and admission tails retain their required original entries. Accepted Hello that
discharges a donor promise also reclaims at the same control cursor. Unchanged
callbacks do not repeat reclamation, and scratch per-entry replay never releases
the tail of an unfinished import. Frozen semantic owners retain their separate
control-observation behavior.

Read-only voter-failure inspection names an exact failure resolution and returns
its retained accepted certificate, if present. It uses the observed control view,
including while semantics are frozen. Its original acceptance index, membership,
voter denominator and linked change remain inspectable after physical control
history has been reclaimed. A history query is only an offer of still-retained
bytes; it does not promise an archival event log.

The receiver admits those facts through semantic owners into its own six system
views. It does not copy application stores, private access, possession, socket
state, queue state or generated-ID state. Imported historical certificates never
invent an acknowledgement by a Herald absent from that historical membership.
Previously admitted Store observations imported through the checked Join transfer
use the same intrinsic publication checks as alignment; their original source
coordinates do not require reconstructing old writer authority. This does not
remove authority admission for the separate structural-occurrence replay or
terminal-source repair paths.
The exact readiness report is available only after the full sealed input cut and
the locally reconstructed candidate base match. Current EPRP and EORC authority
begins at the Oracle activation and the installed admission base. See the
[Herald membership design](../architecture/herald-membership.md).

The restricted EDSC replica lookup carries a Raft node identity and returns an
optional canonical, committed replica registration. The receiver may use this
fact to provision a provisional transport lane while its own control projection
catches up. Lookup is not a voter grant, command channel, or membership decision.
