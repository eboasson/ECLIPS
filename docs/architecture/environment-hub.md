# Connected bootstrap and private environments

## Requirements

Bootstrap and `newenv` provide the same application graph: six predefined
writer/reader pairs, one neutral hub, and eighteen preserving edges. For each
role the edges are writer-to-hub, hub-to-reader and reader-to-hub. All 31 objects
have ordinary controlled identities, labels and possession. Bootstrap uses the
founder label; dynamic construction uses the caller label, initially generation
zero. No extra permanent process is needed.

Bootstrap uses the configured identities for its twelve endpoints. Primordial
startup selections may be partial: `newenv` requires only its two designated
Nabla- and Delta-description source writers. Its result exposes the whole connected
environment, including readable initial descriptions and the six shared sort
definitions. The introductory example is
[hello-context](../../examples/hello-context/README.md);
[hello-world](../../examples/hello-world/README.md) provides the API tour.

## Design

The Domain package owns the canonical endpoint prefix and wiring suffix. Bootstrap
derives wiring IDs from its checked manifest with a separate identity domain and
checks their disjointness. Canonical bootstrap evidence binds the connected shape.
Its initial descriptions use trusted genesis provenance and ordinary controlled
records; later forwarding, relabelling, deletion and disappearance use the normal
paths. System-view stores and isolated bootstrap source identities are internal
infrastructure.

Dynamic construction reserves all 31 IDs atomically, retaining a retry-stable
request identity. It publishes twelve endpoint descriptions through the two
ordinary authorized startup sources. An installed common topology cut establishes
the fresh environment's vertex and edge writers. Construction then publishes the
remaining nineteen objects through those writers with their actual authority,
source coordinates and sequences. It never invents an authority for an endpoint
whose defining cut has not yet been installed.

Construction routes contain only mandatory system-view destinations. The original
source environment gains no application-visible partial object descriptions.
At the endpoint cut, the exact original endpoint descriptions seed only the new
environment's own matching stores. At the final cut the complete descriptions are
seeded idempotently, preserving ordinary receipts and structural provenance. Its
SortDefinition reader receives the same six immutable catalogue descriptions as
bootstrap. Only then are all 31 private aliases returned. Other application readers
receive data through ordinary graph routing and context alignment.

A normal application `end` waits for its accepted environment constructions before
submitting its Oracle End. Session detachment removes reply reachability without
reallocating identities. Forced End before the wiring phase stops construction:
the published prefix settles without aliases or a result, and the unused reserved
IDs remain consumed. A suffix positioned before End still settles as accepted
work. Construction grants no source authority after End.
Membership retirement retains the original positioned publications and uses the
checked successor lineage; an unpositioned wiring suffix selects actual
current sources and routes when its first cut arrives.

All environments use ordinary retention. An ended label owner remains meaningful
through zombie labels; hub/edge activation does not require that owner to remain
alive. Endpoint operation does. Live stores must retain descriptions that should
survive their creator, and transferring endpoints to a new process does not copy
the old stores. A whole-environment handoff that must preserve readable descriptions
therefore needs another live application copy per structural sort and ordinary
alignment; grants alone are insufficient.

## Verification obligations

Properties cover exact 31-object identity and topology construction, bootstrap
hub/edge relabelling and deletion, retained history and late genesis observation,
two-source partial startup, readable initial descriptions, retry stability,
retirement between construction phases, and normal/forced End. Alignment must
preserve Weak strength and must not create Normal possession from it. Join capture
and installation preserve exact retained carrier meanings and receipt strengths.

The selected acceptance/application/delivery/final settlement ledger is
`2 + R + E`; it excludes phase expansion, context seeding, runtime dispatch and
topology-control work. This is an accounting boundary, not a bound on total cost.

Universal edges leave the paper's sort-filter confinement construction deferred.
Private names and construction routes do not establish that stronger confinement
claim after an application explicitly connects an environment to another graph.
