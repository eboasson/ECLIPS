# Checked owner contracts

These contracts describe the current owner boundaries checked by
`scripts/check-profile-0.2-schema.sh`, compiled module/type boundaries and
executable properties. The source audit checks vocabulary, consumers, property
registration and gate composition. It does not prove the behavior by itself.
See the [verification guide](README.md) for the complete execution procedure and
[architecture](../architecture/README.md) for the surrounding design.

## Prepared-child lifecycle

| Boundary or owner | Current contract |
| --- | --- |
| Domain and Oracle | Immutable genesis commitments and minimal dynamic `ProcessStart` are retained. A preparation retains its generated identity and exact Oracle Start; cancellation after submission resolves that Start before requesting End. |
| `ProcessPreparation.State` | Private pure owner retains session-qualified lifecycle requests, the checked selected grant, generated child identity, parent child-name result, descriptor, pending candidate claims and terminal End results. These outlive ordinary session request retirement. |
| `UseCase.ProcessPreparation` | Same-Herald acceptance installs child process facts, private namespace, exact grants and normal parent possession of the child name before preparation succeeds. It never waits for the labels that need that returned name. |
| Required readiness | Ordinary writer operation permission and exact local reader placement, store incarnation and local alignment evidence determine readiness. Required keys are checked against live current objects; name-only and empty startup access require no label transfer. Remote readiness or a predecessor incarnation cannot substitute for local readiness. |
| Application session owner | Prepared attachments admit only stable initial claims. Pending claims do not consume an attachment. The first ready claim commits one session and retained reply atomically; exact retries reoffer it, competitors reject, original recovery grace never renews, and normal Resume takes over reconnect authority. Terminal state wins over historical success. |
| Cancellation and End | Parent and administration cancellation share the same pure owner. A never-submitted Start can be suppressed; a submitted Start must resolve before an ordered End. An attached child reports AlreadyAttached. Parent End does not end its children, and an unclaimed child has no lost-session deadline. |
| EAPP lifecycle | Separate lifecycle commands and replies coexist with the eight-operation union. Checked opaque descriptors and correlations carry no API version. A restricted attachment-plus-request query recovers a retained own-End result without opening or resuming a session. |
| EADM cancellation | Explicit child-preparation cancellation has retained administration correlation, exact retry, conflict and status query behavior. It observes the shared lifecycle cancellation result rather than submitting an independent End. |
| Client and application runtime | The pure client owns request correlations and exact reconnect work. The advanced facade exposes begin/prepare/readiness/cancel/End and prepared connect. Pending initial claims use physical heartbeats without creating session authority. Prepared connect confirms receipt through normal Resume before exposing startup. Lost own-End replies use restricted recovery. |
| Herald runtime and TCP | The runtime supplies the actual serving locator, retains pending candidate routing and serializes disposition effects. A candidate lost before delayed acceptance enters the ordinary binding-loss grace path. Own-End terminal reply is offered before session disposal; retained query covers delivery loss. |
| Independent OS child | The founder integration tour creates an ordinary environment, prepares and labels twelve roots, passes only the checked descriptor to a separately executed child, and checks reader operation, writer operation, child newenv, cancellation and explicit End recovery, including the launcher. |

Pending lifecycle requests retain their original call identity. Inspect an
interrupted accepted mutation through that call; submitting a new identity is a
new request. Public constructors, record updates and coercions must not bypass
checked owner admission. Readiness evidence belongs to the local incarnation,
and End has precedence over historical attachment success.

## Native voters and Oracle administration

| Boundary | Contract | Owners and consumers |
| --- | --- | --- |
| Native learner progress | Successful AppendEntries responses carry matched and owner-acknowledged applied log indices. A correlated current-term response may satisfy the leader's captured frontier only when both cover it. | raft-core, protocol-raft, native reference models and wire vectors |
| Oracle control order | Sealed committed configuration entries advance ControlIndex once, with no synthetic client request or receipt. Every native position is acknowledged after ordered application. | oracle-core, Oracle runtime adapter, EORC, Herald projection/watch |
| Voter administration | Committed registration and complete change intent determine preparation, cancellation and joint/final metadata. Final application completes the explicit change. | Oracle pure state/canonical schema, runtime coordinator, operator client |
| Runtime ownership | Each kernel remains private to its owner. Coordination uses current role/configuration projections, proposal reservations and applied frontiers. | Oracle owners, dispatcher, TCP and deployment supervisors |
| Replica reachability | Retained node/active-host registrations and contact hints enable learners and lagging transport binding repair; native configuration supplies votes. | EDSC, ERFT, EORC, deployment, Herald routing |
| Herald roles | Committed stable/joint voter bindings update routing, reporter eligibility and isolation roles without ending ordinary service on demotion. | Herald control projection, failure and isolation owners, administration |

Immutable genesis identifies the finite full-log run. Native membership supplies
voting authority; transport reachability and semantic Herald membership are
separate facts. Configuration entries advance the sealed committed stream and
Oracle control order without fabricating a client request or receipt.

## Failed voter hosts and semantic fencing

| Boundary | Contract | Owners and consumers |
| --- | --- | --- |
| Failure evidence | Open captures the semantic generation and complete stable voter configuration. Each direct probe and report names that exact configuration. The failed target remains in the old denominator. | Oracle failure state/canonical commands, Herald failure owner, EPRP codecs and tests |
| Accepted intent | Accepted voter-host failure retains its original certificate and claims the shared membership slot. Native joint/final exclusion precedes semantic retirement; final exclusion leaves an explicit awaiting-retirement phase. | Oracle failure/voter kernels, native adapter, Herald driver and control watch |
| Oracle health | A one-shot EORC request carries run identity, an active semantic source identity and a fresh round correlation. The owner-published membership rejects retired sources independently of leader readiness or caller history. The live native owner supplies node, term, effective configuration reference and exact voter sets, plus its local configuration observation generation. This is liveness evidence, never a control-watch configuration or a failure report. | EORC, Oracle runtime owner queue, Herald health observer |
| Observation freshness | New rounds cannot combine stale replies. Native append/truncation invalidates effective-configuration observations. Both joint majorities are required; committed configuration provides the floor for later observations. | Pure Herald health/isolation ownership and runtime timers |
| Role lifetime | Every Herald can permanently self-fence its semantic role. Its control/status shell and separately supervised native replica remain available until explicit shutdown or native failure. Fencing cannot locally shrink or stop Raft. | Herald transition/runtime supervisor, deployment role scopes |
| Acceptance | Captured denominators, retained-intent interruption, lost quorum, still-running partitions, pending semantic workflows and fresh-epoch re-admission require properties and causal real-process evidence. | Failure/completion tours, module/type/schema walls, complete profile aggregate |

No lost-quorum downgrade or implicit promotion can manufacture authority.
Fresh native health observations provide liveness evidence; they cannot replace
committed membership configuration or failure certificates. Experimental resource
budgets in [BUILDING.md](../../BUILDING.md) stop an over-budget run without adding
protocol outcomes. The prototype introduces no compatibility decoder, durable
recovery, snapshot protocol or semantic resource ceiling.
