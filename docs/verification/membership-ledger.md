# Membership source obligations

This ledger explains the stable source IDs enforced by the module-boundary
checker. Immutable genesis catalogues establish origin evidence; live authority
comes from checked Oracle membership history. The current behavior is specified
in [Herald membership](../architecture/herald-membership.md); checked portable
history admission is described in [control history](../architecture/control-history.md).

Run the audit from the repository root after preparing the toolchain described
in [BUILDING.md](../../BUILDING.md):

```sh
sh scripts/check-module-boundaries.sh
```

## Mechanical policy

The executable registry in
`tools/module-boundaries/src/Eclips/ModuleBoundaries/MembershipLedger.hs`
defines each stable ID, production source and exact token-count anchors. Its
migration status and target fields record provenance; the obligations below
state the maintained contract without requiring that development chronology.
The checker reads and tokenizes each production source once for both import
policy and this ledger.

It enforces that every row's source and exact anchor counts exist, and that every
occurrence of a watched membership token belongs to a reviewed row for that
source. A zero-count anchor keeps an eliminated token watched: reintroducing it
in its former owner fails the count; introducing it elsewhere fails as an
unreviewed site. New membership vocabulary still needs ordinary semantic review.
The ID gaps are intentional; retired IDs are not reassigned.

The rows distinguish legitimate immutable-genesis reads from checked live
membership decisions. Their rationale supplements executable assertions; it is
not evidence that an aggregate run has passed.

## Genesis membership reads

| ID | Source | Obligation |
| --- | --- | --- |
| MEM-GEN-001 | `domain/src/Eclips/Domain/Startup.hs` | Keep `OracleGenesis`'s checked initial Herald catalogue as the generation-zero input. |
| MEM-GEN-003 | `herald-core/internal/Eclips/Herald/Genesis/Internal.hs` | Keep checked deployment admission and normalized generation-zero members here. |
| MEM-GEN-004 | `herald-core/src/Eclips/Herald/Initialization.hs` | Seed OracleProjection and Discovery with one checked genesis generation; later live decisions read the projection rather than reseeding from genesis. The timing initializer retains the explicit-fixture genesis voter-host fallback only when no native voter configuration or explicit isolation hosts are supplied. |
| MEM-GEN-005 | `herald-core/internal/Eclips/Herald/OracleProjection/State.hs` | Retain the complete opaque checked Oracle membership history; expose current authority and total historical lookup separately, and append only exact canonical successors. Portable identity admission independently binds membership and process facts to receiver genesis; current membership and the catalogue derive from the admitted history and admission records. Portable failure-base admission checks retained supersession through that admitted lineage, separately from the ordinary historical-view query; it preserves the probe's captured denominator and does not recreate a voter-configuration history. |
| MEM-GEN-006 | `herald-core/internal/Eclips/Herald/ConfiguredProcess/State.hs` | Dynamic Start creates no root route. The current audit requires zero reads of the removed membership snapshot. |
| MEM-GEN-007 | `herald-core/internal/Eclips/Herald/Publication/Route.hs` | Extend mandatory private system-view delivery for all six predefined roles from the supplied current membership generation, preserving the original frozen application destinations and publication evidence. |
| MEM-GEN-008 | `herald-core/internal/Eclips/Herald/Startup/Invariant.hs` | Validate current authority, immutable origin coordinates, compound installed bases, and independently retained controls against the complete checked Oracle history. |
| MEM-GEN-009 | `herald-core/internal/Eclips/Herald/UseCase/PeerInput.hs` | Admit surviving historical publications through checked installed descendant bases while retaining their original authors, generations, and predecessor vectors. |
| MEM-GEN-010 | `oracle-core/src/Eclips/Oracle/Genesis.hs` | Retain the Oracle's checked generation-zero catalogue as the immutable origin of the finite authoritative history. |
| MEM-GEN-011 | `oracle-core/src/Eclips/Oracle/Internal/Label.hs` | Keep the original Oracle catalogue as genesis evidence; serialize admission and retirement through the same checked current history and membership-coordination slot. Receipt retirement admits piggyback and standalone frontiers only for a current active home. |
| MEM-GEN-012 | `oracle-runtime/src/Eclips/Oracle/Runtime/ConformanceClient.hs` | Build conformance EORC Hello from the current projected membership generation rather than treating genesis as live authority. |
| MEM-GEN-013 | `oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs` | Authorize activated Heralds from the serialized owner's live catalogue and checked membership history. Pending applicants have no EORC binding and use restricted EDSC onboarding on the peer listener until activation. |
| MEM-GEN-014 | `herald-core/internal/Eclips/Herald/Discovery/Internal.hs` | Retain the immutable identity catalogue and initial generation separately; neither is the mutable live authority. |
| MEM-GEN-015 | `herald-core/internal/Eclips/Herald/Discovery/State.hs` | Own the current generation, advance by the exact checked successor, remove retired bindings, and fence Hello/dial work by current membership. A fresh joining observer may install current membership and the projected admission catalogue together after checking the history's immutable genesis and exact pending admission predecessor; receiver identity, contacts and transport facts remain local. |
| MEM-GEN-016 | `oracle-core/src/Eclips/Oracle/Step15/Reference.hs` | Seed the private failure model from checked Oracle genesis exactly once, then evolve its own canonical membership generation. |
| MEM-GEN-017 | `herald-core/internal/Eclips/Herald/UseCase/AlignmentTransfer.hs` | Project replacement-source selection through the current Oracle membership so a retired Herald cannot own a fresh alignment attempt after the successor base. Retain member-ready delivery for every current logical peer, including disconnected peers, using the same active membership. |
| MEM-GEN-018 | `oracle-core/src/Eclips/Oracle/Internal/Failure.hs` | Seed the failure owner once, retain every exact retirement and its resolution identity in checked history, and admit new probes only against current active authority. |
| MEM-GEN-019 | `examples/hello-world/FounderConfiguration.hs` | Construct the checked experimental founder seed; its configured member catalogue remains an immutable genesis input. |
| MEM-GEN-020 | `examples/hello-world/FounderProperties.hs` | Compare the founder's checked Oracle and Herald seed projections without treating that catalogue as later live authority. |
| MEM-GEN-021 | `deployment/src/Eclips/Deployment/Manifest.hs` | Check the fixed deployment's exact initial member catalogue and sole founder bootstrap; membership changes subsequently belong to Oracle. |
| MEM-GEN-022 | `examples/hello-world/FounderDeployment.hs` | Launch the checked founder runtime fixture from its initial catalogue and return the launcher's ordinary connection descriptor. |
| MEM-GEN-023 | `oracle-core/src/Eclips/Oracle/Internal/Admission.hs` | Restore checkpoint admission state against the immutable locally checked founder catalogue; current membership authority remains the separately checked membership history. |

## Membership vector and cut shapes

| ID | Source | Obligation |
| --- | --- | --- |
| MEM-CUT-001 | `domain/src/Eclips/Domain/Structural.hs` | Project structural vectors only through opaque checked mixed lineage, preserving every surviving sequence, inserting newcomers at zero, and rejecting implicit comparison across membership coordinates. |
| MEM-CUT-002 | `domain/src/Eclips/Domain/Topology.hs` | Check both contraction predecessors and insertion recipes. Admission recipes bind established old cut K and contribution without guessing the activation index; actual activation adds exactly one empty source component. |
| MEM-CUT-003 | `domain/src/Eclips/Domain/Alignment.hs` | Replace fixed-membership alignment generations with generation-qualified member evidence. |
| MEM-CUT-004 | `domain/src/Eclips/Domain/MemberSet.hs` | Preserve the low-level canonical non-empty member-set digest shared below Membership and Topology. |
| MEM-CUT-005 | `domain/src/Eclips/Domain/Membership.hs` | Own canonical admission identities, strict control order, exact single-epoch insertion/contraction, finite mixed histories, and permanent epoch tombstones. |
| MEM-CUT-006 | `domain/src/Eclips/Domain/Label.hs` | Retain checked generation-qualified acceptance/report wrappers. A compact Ready report carries decision, reporter, and member digest; its decision identifies the captured membership generation without per-source rows. |
| MEM-CUT-007 | `herald-core/internal/Eclips/Herald/Graph/Progress.hs` | Install activation certificates directly at their actual control index while preserving old cuts. Joining observers import historical admission and retirement bases without contributing reports, acceptances, or acknowledgements. A portable progress base retains the applied membership-qualified vector, membership history, and installed-cut provenance; its checked fresh-observer import binds the genesis and paired carrier base and rebuilds coverage without donor reports or rounds. Portable certificate claims re-enter this audit with the captured membership/carrier anchor and exact admitted metadata binding. Independent progress evidence binds checked Projection membership and admission facts plus receiver genesis before carrier composition; the paired import compares retained occurrence fields as well as occurrence identities. The retained current-Nabla preparation selects its immutable frontier and genesis membership explicitly; owner changes refresh it. The bounded topology-proposal memo keys its digest by the membership-qualified structural vector and relevant projection inputs; membership changes invalidate it. Installed frontiers and locally covered, closure-checked candidates use the explicit admitted-frontier reconstruction seam; its repeated coverage and closure audit follows the local diagnostic policy. Generic and terminal-source queries remain checked. |
| MEM-CUT-008 | `herald-core/internal/Eclips/Herald/Graph/Protocol.hs` | Carry generation-qualified structural evidence and a canonical established-cut transcript used by complete inventory proof chains. |
| MEM-CUT-009 | `herald-core/internal/Eclips/Herald/Peer/RPC/Internal.hs` | Bridge every membership-sensitive structural and member-digest form without dropping generation identity. |
| MEM-CUT-010 | `herald-core/internal/Eclips/Herald/PeerPublication.hs` | Retain old-generation stamps/vectors and admit the explicit survivor-relay repair form. |
| MEM-CUT-011 | `herald-core/internal/Eclips/Herald/Publication/State.hs` | Keep immutable publication origins while retargeting current base dependencies through checked descendant membership; release only against established evidence. |
| MEM-CUT-012 | `herald-core/internal/Eclips/Herald/Structural/Reconciliation.hs` | Preserve immutable occurrences through checked mixed-generation bases. Admission contributions retain their nominal origin and activation prefix, so old projections exclude the six newly admitted destinations. A portable carrier base preserves the admitted generation, member digest/set, successor bases, admission lineages and contributions alongside original carrier/control meaning. Portable carrier decoding binds the supplied system and generation to checked membership history, admits certificate lineage references, and captures actual observed Registry definitions and retained retirements as immutable context facts. It grants no Registry mutation or coordination authority. Checked fresh-observer import matches the receiver genesis and derives receiver activity without copying donor Store resources or replaying historical meaning through current views. Batched writer projection preparation validates the same exact frontier as the scalar query. Diagnostic-only coverage/closure repetition is confined to explicit admitted-frontier projection and locally minted occurrence preparation against the owner's applied vector. Generic, incoming, carried and terminal preparation remains checked; membership and source-sequence checks are unconditional.  Per-source contiguous coverage and sparse predecessor/control summaries preserve retired-origin and imported-history requirements; generation-incomparable predecessors retain separate anchors, and failed fast admission preserves the exhaustive first error. |
| MEM-CUT-013 | `herald-core/internal/Eclips/Herald/LabelBarrier/State.hs` | Bind each immutable decision to one exact captured membership generation and digest. Selected source publication groups settle before decision submission; successful installation collection uses the captured survivors. |
| MEM-CUT-014 | `herald-core/internal/Eclips/Herald/PeerPayload.hs` | Carry the live generation-qualified structural and terminal-source control family. Label markers and their member digest are removed; disappearance and route controls retain their typed membership evidence. |
| MEM-CUT-015 | `oracle-core/src/Eclips/Oracle/Internal/Label.hs` | Store the authoritative membership generation with each live label decision and derive its exact eligible reporter set from that captured generation. |
| MEM-CUT-016 | `oracle-core/src/Eclips/Oracle/Internal/LabelCanonical.hs` | Canonically encode live generation-qualified label workflow evidence and outcomes together with the failure-owner state cutover. |
| MEM-CUT-017 | `herald-core/internal/Eclips/Herald/OracleClient/State.hs` | Retain checked membership-advance evidence at the exact next ordinary cursor, keeping later live Oracle requests contiguous and replayable. |
| MEM-CUT-018 | `herald-core/internal/Eclips/Herald/Graph/TerminalSource.hs` | Keep full genesis-to-target origin lineage separate from the installed anchor-to-target closure interval. Close retired origins jointly while retaining earlier stamps, inventories, established chains, payloads, and the greatest causally closed prefix vector. |
| MEM-CUT-019 | `herald-core/internal/Eclips/Herald/Placement/State.hs` | Reconstruct physical-placement revision vectors over the exact supplied membership generation rather than a caller-selected member list. |
| MEM-CUT-020 | `herald-core/internal/Eclips/Herald/UseCase/Alignment.hs` | Route alignment planning through the current Oracle membership generation when reconstructing its physical-placement cut and notifying parked work after a membership change. Queue cut-acceptance evidence for every current logical peer so temporary disconnection cannot lose a delivery obligation. Cause-driven alignment retries also use checked membership to recognize retired Store homes and order reset attempts across announcer changes. |
| MEM-CUT-021 | `herald-core/internal/Eclips/Herald/UseCase/Step15StructuralBase.hs` | Own one evolving compound membership-base closure, repair its anchor from complete established evidence, and atomically install the exact target base. |
| MEM-CUT-022 | `herald-core/internal/Eclips/Herald/UseCase/Step15RetirementClosure.hs` | Resume the retirement fixed point only under established evidence for the current descendant target, preserving prior work without duplicating effects. |
| MEM-CUT-023 | `herald-core/internal/Eclips/Herald/TerminalSourceHold/State.hs` | Retain each authenticated occurrence with its own target and wait phase. Later targets may precede intermediate Oracle watches; historical readiness becomes inert without closing surviving peers. |
| MEM-CUT-024 | `domain/src/Eclips/Domain/Disappearance.hs` | Bind private disappearance evidence to one exact subject digest, membership generation, and member-set digest before any Oracle/EPRP disappearance schema becomes live. |
| MEM-CUT-025 | `herald-core/internal/Eclips/Herald/Application/Environment.hs` | Freeze each accepted private environment manifest to the exact current membership generation and member-set digest so retries retain provenance rather than following a later cut; terminal private evidence separately retains the selected common cut's exact membership coordinate. |
| MEM-CUT-026 | `herald-core/internal/Eclips/Herald/UseCase/StructuralSettlement.hs` | Complete a frozen environment under its captured generation or a checked established descendant projection while preserving the manifest and exact surviving peer obligations. |
| MEM-CUT-027 | `herald-core/internal/Eclips/Herald/Graph/DisappearanceReadiness.hs` | Reconstruct graph-readiness evidence over the exact current membership generation and reject mixed-generation readiness. |
| MEM-CUT-028 | `herald-core/internal/Eclips/Herald/UseCase/PeerInput.hs` | Replay original source stamps and canonical semantic publications through the ordinary owner checks. Observer replay preserves historical provenance while descendant vectors follow checked lineage. |
| MEM-CUT-029 | `herald-core/src/Eclips/Herald/Transition.hs` | Replay matching membership-held occurrences against complete Oracle history, preserve independent future targets, and make obsolete readiness inert. |
| MEM-CUT-030 | `herald-core/internal/Eclips/Herald/OracleProjection/Step15.hs` | Prepare detached membership projection changes against the retained checked history and exact canonical successor evidence. |
| MEM-CUT-031 | `herald-core/internal/Eclips/Herald/Startup/State.hs` | Retain one evolving membership-base closure as owned startup state alongside independent terminal control holds. |
| MEM-CUT-032 | `herald-core/internal/Eclips/Herald/UseCase/TerminalStructuralStart.hs` | Capture terminal evidence from the full checked origin history and the installed anchor chain when authority advances repeatedly, including later-retired sources whose stamps predate the current anchor. |
| MEM-CUT-033 | `oracle-core/src/Eclips/Oracle/State.hs` | Expose the opaque checked membership history through a read port; only the Oracle failure owner extends its authoritative sequence. |
| MEM-CUT-034 | `herald-core/internal/Eclips/Herald/Join/History.hs` | Retain each original source prefix and canonical control history inside a frozen aggregate envelope. The sealed member cut commits to every owner frame, while live receivers replay the nested History and its ordinary and membership-base certificates through checked owner transitions. Envelope framing admits no owner snapshot. |
| MEM-CUT-035 | `herald-core/internal/Eclips/Herald/UseCase/ControlBase.hs` | Supply the checked imported Projection's membership history and canonical admission catalogue to Discovery and independent Progress admission. The portable joining envelope binds these facts to the original History's source, admission attempt, control prefix and final structural cut before one atomic fresh-observer installation. Exact Terminal payloads bind Progress stamps and carrier semantics; these reads grant neither history mutation nor historical control replay. |

## Member-qualified waits

| ID | Source | Obligation |
| --- | --- | --- |
| MEM-WAIT-001 | `oracle-core/src/Eclips/Oracle/Internal/Label.hs` | Keep a successful decision immutable and derive collector/survivor obligations from its checked captured generation after retirement. Failed decisions own no collection slot. |
| MEM-WAIT-002 | `herald-core/internal/Eclips/Herald/Graph/Progress.hs` | Successor-base installation rebases report, cut-acceptance, establishment, and acknowledgement waits onto the successor membership; the retirement closure resumes from that exact installed base. |
| MEM-WAIT-003 | `herald-core/internal/Eclips/Herald/LabelBarrier/State.hs` | Source publication-group frontiers and canonical peer retirement establish selected settlement. Historical installation facts remain until compact completion; no global label gate, Ready or Prepared wait exists. |
| MEM-WAIT-004 | `herald-core/internal/Eclips/Herald/PeerStream/State.hs` | Terminally settle exact outgoing assignments for a retired peer, discard its unadmitted gap suffix, retain contiguous admitted input, and never retry that epoch. |
| MEM-WAIT-005 | `herald-core/internal/Eclips/Herald/Alignment/State.hs` | Preserve stable and fulfilled alignment work across source loss, remove destination-owned attempts and pending evidence with their scheduling indexes, and redrive source selection after the successor base. Keyed pending-evidence access and the independent index validator retain this same lifetime boundary. Fresh reset promotion retires superseded pending plan and cut controls while preserving historical readiness and certificate evidence. |
| MEM-WAIT-006 | `herald-core/internal/Eclips/Herald/Publication/State.hs` | Preserve frozen routes and source evidence while retiring exact dead destinations and advancing retained base waits to the current descendant target. |
| MEM-WAIT-007 | `herald-core/internal/Eclips/Herald/UseCase/PeerInput.hs` | Terminally complete retained input from a retired source without sending an acknowledgement or revalidating the frozen source authority. |
| MEM-WAIT-008 | `herald-core/internal/Eclips/Herald/UseCase/StructuralProgress.hs` | Stamp retained source suffixes under the exact installed membership lineage, preserving frozen ordinary destinations and extending mandatory system-view delivery to newly active members. |
| MEM-WAIT-009 | `oracle-core/src/Eclips/Oracle/Step15/WorkflowReference.hs` | Keep the retained reference workflow generation-qualified: retirement makes pre-release work NotApplied and shrinks post-release reporters to the successor set without becoming an alternative live Oracle owner. |
| MEM-WAIT-010 | `herald-core/internal/Eclips/Herald/UseCase/ApplicationCall.hs` | Hold a checked label Open in the ordinary pre-acceptance owner while OracleProjection is ahead of the installed Graph membership base, then permit one ordinary promotion after the exact successor base. |
| MEM-WAIT-011 | `herald-core/internal/Eclips/Herald/Alignment/Loss.hs` | Preserve a stable obligation or bootstrap import on membership-derived source loss, retire its exact attempt, and defer replacement selection until successor-qualified evidence is available. |
| MEM-WAIT-012 | `herald-core/internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs` | Partition pending generation evidence by the retired source, derive each exact lost Store coordinate once from the retired placement, apply deferred alignment-loss plans, and project their surviving-peer transfer effects atomically. |
| MEM-WAIT-013 | `herald-core/internal/Eclips/Herald/UseCase/OracleAdvance.hs` | Apply pending-generation retirement and alignment-source loss to the serialized live Herald state when the successor membership entry is projected. |
| MEM-WAIT-014 | `herald-core/internal/Eclips/Herald/UseCase/LabelCollection.hs` | Resolve the decision's captured generation through checked membership history before collecting surviving installations or reassigning a retired collector. |

## Protocol producers and consumers

| ID | Source | Obligation |
| --- | --- | --- |
| MEM-PROTO-001 | `protocol-peer/src/Eclips/Protocol/Peer/Types.hs` | Carry membership generation and active-member digest in Hello, and define the checked direct-probe plus terminal inventory/request/relay/union/acceptance/established DTO family. |
| MEM-PROTO-002 | `protocol-peer/src/Eclips/Protocol/Peer/Codec.hs` | Round-trip the closed EPRP envelope, including every retained idempotent control arm. |
| MEM-PROTO-003 | `protocol-peer/src/Eclips/Protocol/Peer/Frame.hs` | Preserve direction and framing admission for every arm of the EPRP envelope. |
| MEM-PROTO-004 | `herald-core/internal/Eclips/Herald/Peer/RPC/Internal.hs` | Validate DTO shape and canonical byte/digest consistency, then bridge direct probes and terminal-source controls into package-internal checked carriers. |
| MEM-PROTO-005 | `protocol-oracle/src/Eclips/Protocol/Oracle/Types.hs` | Carry the membership generation in Oracle Hello and admit the live canonical failure-probe/retirement command and receipt family. |
| MEM-PROTO-006 | `protocol-oracle/src/Eclips/Protocol/Oracle/Codec.hs` | Bridge and round-trip the current-generation Oracle Hello and canonical failure/retirement carriers. |
| MEM-PROTO-007 | `protocol-oracle/src/Eclips/Protocol/Oracle/Frame.hs` | Preserve direction, authority, and framing admission for the EORC schema. |
| MEM-PROTO-008 | `oracle-runtime/src/Eclips/Oracle/Runtime/ConformanceClient.hs` | Produce EORC Hello from the current Oracle membership generation and exercise the activated failure/retirement command family. |
| MEM-PROTO-009 | `oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs` | Consume exact current or historical Hello claims through checked ancestry and carry canonical failure/retirement entries through the serialized Oracle owner. |
| MEM-PROTO-010 | `herald-core/internal/Eclips/Herald/Peer/Step15.hs` | Provide the checked generation-qualified peer Hello and direct-probe algebra used by live EPRP conversion and the failure owner as well as the retained reference harness. |
| MEM-PROTO-011 | `herald-core/src/Eclips/Herald/EffectBatch.hs` | Carry the owner-selected peer Hello generation and active-member digest in the typed send effect. |
| MEM-PROTO-012 | `herald-core/src/Eclips/Herald/Input.hs` | Carry generation-qualified peer Hello and checked probe/terminal controls into the serialized pure transition. |
| MEM-PROTO-013 | `herald-core/src/Eclips/Herald/Peer/RPC.hs` | Project live peer Hello with caller-supplied current membership claims while keeping RPC conversion context-free. |
| MEM-PROTO-014 | `herald-core/internal/Eclips/Herald/UseCase/PeerControl.hs` | Route terminal controls by authoritative target history; hold future claims without authority, discard historical claims inertly, and repair complete known-anchor inventory evidence before replay. |
| MEM-PROTO-015 | `herald-runtime/internal/Eclips/Herald/Runtime/Internal/Owner.hs` | Forward typed Hello membership claims and decoded controls into the single Herald owner without reading owner state in the runtime shell. |

## Runtime and live admission seams

| ID | Source | Obligation |
| --- | --- | --- |
| MEM-RUNTIME-001 | `herald-core/internal/Eclips/Herald/Discovery/State.hs` | Gate peer Hello and dial intentions by current membership and atomically remove a retired binding/contact target. |
| MEM-RUNTIME-002 | `herald-core/internal/Eclips/Herald/UseCase/PeerControl.hs` | Recheck both local and remote epochs against OracleProjection before a binding remains current. |
| MEM-RUNTIME-003 | `herald-core/src/Eclips/Herald/EffectBatch.hs` | Define the typed binding-close, dial-cancel, and destination-cancel effects; the effect vocabulary carries a pure decision without granting runtime membership authority. |
| MEM-RUNTIME-004 | `herald-runtime/internal/Eclips/Herald/Runtime/Internal/Owner.hs` | Interpret explicit binding-close, dial-cancel, and destination-cancel effects through the serialized owner without inferring retirement. |
| MEM-RUNTIME-005 | `herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Peer.hs` | Suppress queued and already-handed dial generations after explicit cancellation and leave stale Hello rejection to the pure admission path. |
| MEM-RUNTIME-006 | `herald-runtime/src/Eclips/Herald/Runtime/Oracle.hs` | Project the owner-published current membership generation into typed live EORC Hello. |
| MEM-RUNTIME-007 | `herald-runtime/tcp-internal/Eclips/Herald/Runtime/TCP/Internal/Oracle.hs` | Carry generation-qualified Herald-to-Oracle attempts with the EORC Hello; transport loss alone still never derives retirement. |
| MEM-RUNTIME-008 | `oracle-runtime/src/Eclips/Oracle/Runtime/Internal/TCP/Oracle.hs` | Separate active lane authorization from exact pending-applicant admission traffic; ordinary authority still requires membership in the current checked history. |
| MEM-RUNTIME-009 | `herald-core/internal/Eclips/Herald/UseCase/Step15MembershipAdvance.hs` | Retain the executable reference composition for the atomic OracleProjection, OracleClient, Discovery, liveness, PeerStream, Placement, and target-resident-End advance; live application is owned by `OracleAdvance`. |
| MEM-RUNTIME-010 | `herald-runtime/src/Eclips/Herald/Runtime/Ingress.hs` | Expose one typed generation-qualified peer-Hello ingress operation without consulting kernel state. |
| MEM-RUNTIME-011 | `herald-runtime/internal/Eclips/Herald/Runtime/Internal/Coordination.hs` | Preserve membership claims while coordinating candidate Hello delivery and owner acceptance. |
| MEM-RUNTIME-012 | `herald-runtime/internal/Eclips/Herald/Runtime/Internal/Types.hs` | Retain exact membership claims in the runtime submission interface and owner-published status vocabulary. |
| MEM-RUNTIME-013 | `oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Hello.hs` | Check current active lineage for ordinary callers and exact pending admission identity for a restricted applicant, without treating discovery or catch-up as serving authority. |
| MEM-RUNTIME-014 | `oracle-runtime/internal/Eclips/Oracle/Runtime/Internal/Types.hs` | Retain the owner's opaque checked history as one status fact, so readers cannot mix a current generation with an unrelated ancestry list. |
| MEM-RUNTIME-015 | `oracle-runtime/src/Eclips/Oracle/Runtime/Internal/OracleOwner.hs` | Publish the exact checked history atomically after serialized Oracle progress; TCP readers obtain evidence without reading or mutating kernel state. |
| MEM-RUNTIME-016 | `oracle-runtime/src/Eclips/Oracle/Runtime.hs` | Initialize status from the actual Oracle state's checked history and derive the ordinary current-generation getter from that same fact. |

## Reference models and live interfaces

`Eclips.Oracle.Step15.Reference` is a policy-private executable reference model,
not a live command or protocol owner. It remains Cabal-exposed for focused
cross-package properties; production imports are allowed from exactly:

- `herald-core/internal/Eclips/Herald/OracleProjection/Step15.hs`; and
- `herald-core/internal/Eclips/Herald/UseCase/Step15FailureVertical.hs`.

The module-boundary checker still rejects imports of that model from live Oracle
`Command`/`Transition`, EORC, either runtime, or live Herald `OracleAdvance`.
`Eclips.Oracle.Step15.WorkflowReference`,
`Eclips.Herald.UseCase.Step15FailureVertical`,
`Eclips.Herald.UseCase.Step15MembershipAdvance`, and
`Eclips.Herald.UseCase.Step15RetirementClosure` likewise remain executable
reference/property composition. Their narrow historical imports are retained;
they are not alternative runtime owners.

The following package-private interfaces connect the reviewed live owners:

- `Eclips.Herald.Peer.Step15` is the checked direct-probe and membership-Hello
  algebra used by `Input`, the EPRP adapter, and the serialized failure owner;
- `Eclips.Herald.Graph.TerminalSource` owns canonical sealed inventories,
  exact payload repair, union acceptance/establishment, and successor-base
  evidence used by the RPC/input adapters and live Herald coordinators;
- `Eclips.Herald.UseCase.Step15StructuralBase` is shared by the retained closure
  model and live `PeerControl`, which applies terminal controls and installs the
  exact successor base; and
- `FailureDetection`, `TerminalStructuralArchive`, and
  `TerminalStructuralStart` are live package-private coordinators reached only
  from `Transition` or `OracleAdvance`; isolation state participates in the live
  whole-state invariant and transition; and
- `TerminalSourceHold.State` retains each occurrence's target and wait phase
  across multiple successor-projection races. `Transition` consumes only current
  targets, preserves later targets, and discards obsolete readiness atomically.

The shared `HeraldRetired` process-End reason is a live Oracle-derived value.
The Oracle failure owner may derive it only from a committed retirement, the
ordinary canonical Oracle state records it, and Herald OracleClient projection
consumes it. EADM still rejects an application attempt to submit that reason,
because applications do not own retirement authority.

The reusable negative fixture includes representative reversible cases
for an unledgered watched-token site and a missing exact anchor. It also exercises
an unauthorized live-`OracleAdvance` import of the retained reference model.
These cases reuse the one copied project and the same in-process
mutation/restoration loop as all other module-boundary negatives.
