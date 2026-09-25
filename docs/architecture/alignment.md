# Alignment plans and generation reuse

Current routing coordinates and immutable class birth evidence have distinct
lifetimes. The protocol is specified in [networked alignment](../networked/03-protocols.md#9-alignment-and-continuing-anti-entropy).

## Checked representation and protocol

`Eclips.Herald.Alignment.Plan` separates the current routing coordinate from
immutable class birth evidence. Its identity contains the exact sort occurrence,
topology-cut ID and complete physical-placement vector. A plan retains its current
topology cut, complete class bindings and relations, exact immediate predecessor
claim, and the predecessor's usable/invalidated status. Each binding states
`AlignmentCreated` or `AlignmentCarried` and retains a checked generation.

Current topology evidence remains explicit when every bound generation was born
at an older cut. Every plan's announcer is the least Herald in the current
complete placement membership; a carried class's birth announcer cannot choose
it. Empty removal-only plans retain their coordinate and predecessor, with no
bindings, relations or certificate wait. They still use the same coordinated
announcement and acceptance: choosing empty plans independently could record
different placement vectors and fork the next plan's predecessor frontier.

Plans, bindings, IDs and the ledger are opaque. The complete peer announcement
contains plan and predecessor coordinates, predecessor status, class bindings,
relations, and birth cuts for created classes. Admission reconstructs the plan
from the exact topology, placement and retained predecessor, then compares the
whole claim. Missing, duplicate, extra or altered entries are rejected. A lone
birth announcement cannot establish an incomplete current plan. The checked
announcement is cached when the plan is constructed; its observable projection
is tested against the retained plan fields.

Topology/placement membership and member-set agreement is checked even for empty
and all-carried plans. A new sort occurrence starts its own lineage. Preparing a
successor at its predecessor's coordinate is rejected; exact replay follows the
retention path instead. The internal prototype changes producers, consumers and
codecs together, with no protocol compatibility layer.

## Carry and created work

The planner matches exact context members `(DeltaId, StoreIncarnationId,
HeraldEpoch)`, then finds the greatest dependency-closed subset whose incoming
source generations and strengths are unchanged. An outgoing-only change can
leave the source generation intact. A changed source propagates to downstream
classes even when their local members and immediate relation shape match.

Current Store revisions do not affect eligibility. Carried generations retain
all original birth bytes, fresh bases, readiness and certificates. Immutable
neutral, edge, nabla and delta payloads cannot change in place; test graph changes
represent deletion and fresh identity replacement. Lifecycle, label, placement
and context changes still affect the selected projection and carry decision.

An authority-only label whose checked controller, activity and Store resources
remain identical bypasses structural preparation. Advancing the Oracle cursor
between retained structural consequences preserves current Nabla queries and
does not wake alignment-plan reconstruction; control-dependent readiness still
advances. Historical cut queries survive a pure cursor advance because their
installed topology and placement coordinates are unchanged. A real controller,
activity, incarnation, topology or placement change retains the existing
invalidation and context obligations. Independent full-recomputation properties check the dependency argument.

Membership/member-set changes disable carry and preserve the established
created-generation ancestry law. An invalidated predecessor also disables carry,
but its generations and relations are excluded from ancestry selection: recovery
must not wait indefinitely for the invalidated history's unavailable
certificates. Its plan ID and invalidated status remain explicit frontier
witnesses. With a usable predecessor, created classes select overlapping old
classes plus old sources whose destinations overlap the new destination, and
capture a fresh base only for a delta unrepresented by those predecessors.

Generation drafts defer ancestry materialization and digest construction. Only
created classes demand those computations, and their topology digest is shared
with the plan coordinate. Only created classes wait for selected predecessor
certificates. Certificate availability never changes a carry decision, so an
all-carried plan can proceed without supplying those certificates again.

The old pure generation-planning entry point remains available for isolated
owner properties. Production coordination uses the explicit plan path.

## Owner retention and current acceptance

The ledger retains one exact plan per identity and separate
`(StructuralConsequenceCause, AlignmentPlanId)` coverage. A child must reference
the exact retained predecessor claim, not just a matching predecessor ID. This
prevents two plans with equal coordinates but different captured bases from
being substituted for one another.

The owner admits a newly inserted plan only as the successor of its current
frontier. First insertion creates the necessary work. An additional cause adds
coverage without allocating duplicate bootstrap imports or ordinary relations.
Replaying an old plan, or adding a cause covered by it, does not rewind the
current frontier, close current owners or recreate old work. Invalidated plan
coordinates cannot newly retire debt through this path.

An unchanged relation keeps its exact obligation, supplier attempt, subscription
and revision progress. Equivalence includes the source generation, destination
generation and strength. Changed relations create the required work; removed
relations retain their old closing scope. Re-adding a relation creates a new
owner and subscription instead of reopening a closed or closing owner.

Acceptance is keyed by current plan and accepting Herald, and captures that
Herald's publication prefix. For created generations, the owner projects this
acceptance into the immutable birth-cut acceptance used by existing bootstrap
machinery. Carried generations preserve their old birth acceptance and prefix.
A source serving a newly created destination must accept that destination's
creation plan; its own old certificate and birth acceptance are insufficient.

Required-reader readiness combines exact current placement, absence of relevant
live debt, membership in the current plan, local current-plan acceptance and
retained owner-local historical completion. A remote readiness advertisement
cannot substitute for local completion. Disappearance retains subject-relevant
unsettled-plan blockers and still captures continuing subscriptions. An owner
with no local class members still needs local acceptance, including an empty
plan, but does not wait for remote owners' local readiness.

## Continuing streams and immutable closure receipts

Created generations still discharge their required ancestry, route-marker and
input-prefix work. Carried classes have unchanged incoming history and retain
their existing streams. Completing a child's input closure cancels only owners
already marked closing; an unchanged relation continues serving data.

A whole-closure receipt fixes its exact witnessed input lower bounds. Later data,
Live and acknowledgement progress on a carried subscription advances ordinary
stream counters without rewriting those sealed barrier witnesses. Pending
barriers without a receipt can still advance. Receipt validation requires the
current counters to cover the retained cut, rather than equalling its old
applied/acknowledged values.

A later bootstrap subscriber for the same child generation and source Store
reuses that immutable closure receipt. Its own snapshot/change stream must reach
the receipt's source revision before Live is released. It does not recapture a
new, conflicting receipt after continuing input streams have advanced. Local
source admission and the exact generation/Store key preserve the receipt's
membership and ancestry scope.

## Loss and historical evidence

Loss of a current member invalidates the current plan containing that member,
including when the member's birth generation was carried from an older plan and
its one-time import has already completed. The replicated current-member table
provides this evidence even without a live subscription.

Invalidation retains an immutable set of affected generation IDs. When an old,
superseded Store incarnation is lost, bindings still carried by another valid
current plan are excluded from that old plan's executable retirement. The
affected set is frozen in the receipt and indexed monotonically; later frontier
changes cannot retroactively reinterpret the old invalidation and disable an
unrelated carried generation. Generations, certificates, birth acceptances and
completed history remain retained evidence.

## Join and composed invariants

Join history carries complete explicit plans and the transitive predecessor-plan
closure needed to resolve carried references. It includes empty plans, effective
frontier plan IDs, cause coverage, readiness, certificates and passive plan
acceptances. Import reconstructs the exact tables, retains their birth evidence,
and creates no old-owner obligations, attempts, subscriptions or newcomer votes.
Activation adopts the sealed donor's explicit frontier. Current coordinates are
not inferred by grouping generations with equal birth cuts.

The Alignment owner checks ledger coverage, current-plan/latest-binding
consistency, exact acceptance projections and the frozen invalidation index.
Mixed-age relation endpoints must be justified by a checked plan. Startup
validation replays each explicit plan against installed topology and placement,
then separately validates each generation's immutable birth facts. Route markers
name the created generation's plan and require that exact plan acceptance.

## Verification obligations

Generated reference comparisons cover carry eligibility, exact plan reconstruction,
immutable birth facts, invalidated recovery, mixed-age relations, readiness,
removed/re-added owners, delayed closure traffic and joined activation.
Generation reuse does not establish a bound on all retained plan history.
