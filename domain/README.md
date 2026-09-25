# `eclips-domain`

This package owns the pure, deterministic semantic data and transitions shared
by the implemented networked profile:

- nominal checked identities;
- checked values and schemas;
- descriptor-admitted scalar query predicates and deterministic evaluation;
- descriptor admission and total evaluation;
- canonical descriptor bytes and SHA-256 `SortId` derivation;
- the closed predefined profile catalogue, canonical startup digests, primordial
  definition replicas, index-zero bootstrap vocabulary, and minimal dynamic
  process Start identity/residence shared by the Herald and Oracle kernels;
- universal graph-edge and frozen-route data;
- positive structural occurrence identities, exact applied version vectors, and
  physical-placement revision vectors qualified by membership generation and
  member-set digest;
- checked topology frontiers and cuts with explicit same-generation or
  membership-successor predecessor lineage;
- constructor-checked label targets, released overlays, acceptance cuts, marker
  evidence, normalized preparation facts, and explicit process-end projection;
- opaque, canonical Herald membership generations, ordered history and descendant
  lineage, with exact retirement and failure-probe/resolution identities;
- the closed twelve-root private-environment shape, neutral genesis/retirement
  sort-occurrence derivation, and constructor-checked disappearance claims and
  identities used by the final profile slice; and
- checked publications and convergent visible/retained delta stores.

It has no dependency on effects, concurrency, clocks, entropy, sockets, protocol
DTOs, Herald, Oracle, or Raft. The root boundary checker enforces that wall.

The nominal `SortId` representation is owned by `eclips-public-types` and
re-exported through `Eclips.Domain.Identity`. The domain remains responsible for
deriving that public 32-byte content address from canonical descriptor bytes and
for admitting the descriptor; the shared type package performs neither operation.

`Eclips.Domain.Sort.Profile` owns the fixed six-role catalogue and the intrinsic
admission rule for values of the predefined sort-definition carrier. That rule is
enforced by the sole `mkCheckedPublication` constructor: it decodes and re-encodes
the embedded descriptor and recomputes its claimed `SortId`. Embedded structural
descriptors are accepted only when their bytes exactly match one of the six closed
profile entries; all other embedded definitions use application-descriptor
admission.
`Eclips.Domain.Startup` builds on that profile with component-neutral
configuration, the current predefined occurrence set derived through
`Eclips.Domain.SortOccurrence`, checked genesis process templates, and the fully
applied index-zero bootstrap shape. `Eclips.Domain.ProcessStart` carries only
checked process identity, epoch, and residence for dynamic admissions. Raw Herald deployment
manifests, local fixture selection, and the composed Herald state do not belong
here. Opaque checked carriers expose observations and checked smart constructors
instead of record constructors or generic representation access.

`Eclips.Domain.Label` contains only the shared semantic evidence needed on both
sides of the Herald/Oracle boundary. `Label = (LabelOwner, Word64)` retains the
whole owner/generation pair in canonical values and queries. Every successful
label transition proposes one successor, checked preparation binds that successor,
and terminal deletion retains it without introducing a second live counter.
Process-End projection preserves generation; authority tenure compares owners.
Label targets and released/deleted overlays,
positive revisions and marker/process positions, records, authority dispositions,
ready rows, and prepared reports are opaque and eliminated through read-only
views/accessors. It does not construct an Oracle request, derive a label decision
identity from request parts, or own a workflow phase. `Eclips.Domain.ProcessLifecycle` fixes the closed three-reason
vocabulary—explicit administration, application loss, and Herald retirement—and
the pure effective-label projection. EOF and failure suspicion by themselves are
not process-end reasons.

`Eclips.Domain.Membership` owns the shared pure vocabulary for the finite Herald
membership history and its failure workflow. Generation, history, lineage, probe, and resolution
constructors are opaque; callers use checked construction and canonical decoding,
then observe values through read-only accessors. A probe requires a positive Oracle
control index. Each retirement retains its exact retirement resolution, occurs
strictly after that probe and the preceding membership change, and removes one
active epoch without emptying the system. Canonical generation decoding proves
intrinsic shape and identity; checked history then reconstructs every exact
successor from genesis and rejects skipped changes, branches and reused retirement
identities. A lineage is an ordered interval of this history, preserving its
immutable origin separately from descendant authority. Live admission is described
in the [membership contract](../docs/architecture/herald-membership.md).
`Eclips.Domain.MemberSet` owns the lower member-set digest vocabulary so membership
and topology can share it without an import cycle.

`Eclips.Domain.Environment` owns only the one canonical six-role,
writer-before-reader manifest shape. It checks all twelve symbolic roots and
their unsequenced writer declarations, and names the nabla/delta structural
carrier selected by each root kind. Generated identities, process positions,
publications, routes, application access, and codecs remain with their respective
owners.

`Eclips.Domain.SortOccurrence` is the sole derivation of an effective regular
sort-definition occurrence. Genesis is explicit, while a resolved-retirement
base can be constructed only from a positive Oracle control index. The public
`SortId` remains unchanged across those occurrences.

`Eclips.Domain.Disappearance` contains the constructor-hidden subject,
membership/evidence coordinate, probe identity, and resolution-outcome
vocabulary shared by the live Herald and Oracle owners. Its manual canonical
bytes are semantic digest transcripts, not a wire codec. The module owns no
candidate, workflow phase, Oracle command, peer/alignment marker, Store fact, or
application operation.

`Eclips.Domain.Structural` admits each complete normalized structural vector
against one checked membership generation and retains both its generation ID and
member-set digest. Coverage is therefore generation-local even when two component
maps happen to look alike. Explicit projection through checked lineage preserves
surviving source sequence numbers; semantic settlement still needs the owning
Herald's terminal evidence for every removed source. `Eclips.Domain.Alignment` applies the same checked
generation/member-set coordinate to physical-placement revision vectors, so a
placement map cannot be compared or attached to a topology frontier from another
membership coordinate.

`Eclips.Domain.Topology` keeps predecessor construction opaque: ordinary cuts name
one same-generation predecessor, while membership-successor cuts use checked
ordered lineage from the established ancestor through one or several retirements,
the terminal predecessor vector,
survivor-only initial projection, and terminal-source-union digest. Boundary
constructors may preserve shape-checked wire claims, but the owning state must
resolve their generation coordinates against retained checked membership history
before semantic use. The canonical cut decoder performs intrinsic shape checks
and exact re-encoding; it does not turn decoded generation IDs into ancestry.

Profile 0.1 places no resource ceiling on individual values, descriptors, frozen
routes, or store history. The friendly finite run assumes they fit available host
resources; semantic shape and canonical-encoding checks remain mandatory.

## Current-run canonical encoding

The semantic package uses `Data.Serialize` over private raw representations for
canonical values and sort descriptors. Those raw types are codec data only: decode
first reconstructs the public raw syntax through its checked constructors and then
runs ordinary semantic admission. It never deserializes directly into
`CheckedDescriptor`, `CanonicalDescriptor`, `CheckedInstance`, or another type
whose constructor claims that admission already happened.

Before encoding, record members and other unordered inputs are normalized into
their semantic order. After decoding, the admitted value is re-encoded and the
bytes must be identical. This gives each admitted semantic value one byte string
for the current build while rejecting malformed, invalid, or non-canonical input.

The derived cereal representation is deliberately a current-run implementation
contract, not a portable or revision-compatible schema. A source-shape or cereal
change may change every affected byte string and `SortId`; producers, consumers,
goldens, and catalogue digests then change together and the experiment starts a
fresh run.

### Values

`GlobalUniqueId` is the globally meaningful identity carried in canonical semantic
values. It is deliberately not the private identifier exposed to an application;
the Herald translates between those process-local aliases and global identities at
the application boundary. `GlobalUniqueId` and `GlobalObjectId` remain distinct
nominal domains even though each is encoded as the same 32 value octets.

The foundation exports explicit, byte-preserving conversions between these two
global domains. They are trusted semantic-boundary operations: converting a
`GlobalUniqueId` to a `GlobalObjectId` is valid only inside a checked transition
that establishes or validates that controlled-object meaning. No
application-private ID type or private/global registry belongs in this package.

`ProcessEpochId`, `NablaId`, and `DeltaId` are further nominal refinements of
controlled-object identity. Their explicit, exact-byte conversions to and from
`GlobalObjectId` likewise belong only inside checked transitions that establish or
validate the corresponding primordial role. `HeraldEpoch` and
`StoreIncarnationId` are operational identities, not controlled-object
refinements, and have no such conversion.

Record entries are in ascending checked ASCII field-name order, and duplicate
fields are invalid. Delete is an application command target, not a persistent
canonical label or value constructor.

### Queries

`Eclips.Domain.Query` owns the component-neutral scalar predicate grammar. Raw
predicates are admitted separately against each effective descriptor, producing
an opaque checked predicate before evaluation. Boolean composition may be nested;
`all` and `any` are non-empty by construction. Comparisons are limited to scalar
projections with a schema-compatible literal, and ordered comparisons use the
domain's canonical scalar ordering. For an enum projection, the exact literal
symbol must be a declared member and its zero-based declaration ordinal supplies
that order. Query evaluation is pure and does not know about application-private
names, Herald placement, store ownership, or result localization.

### Sort descriptors

The private raw descriptor representation covers the complete first-order
descriptor syntax: schemas, projections, predicates, rank terms, application
mutation, and the optional structural carrier role. The predicate-literal language
contains no global/process identity constructor. Unique-ID fields may be
projected for object keys and total ranks, but a descriptor cannot embed a
controlled identity.

A projection is a non-empty field-name path, while a regular descriptor's list of
key projections may be empty. That empty tuple gives every valid value of the sort
one object key; controlled descriptors still require one direct unique-ID key
projection. Descriptor record fields use checked ASCII names and semantic
ascending order. A descriptor rank ends in exactly one complete application-value
term; for a controlled sort that term omits the canonical label field.

An enum schema is a non-empty declaration-ordered list of pairwise-distinct exact
text symbols. No case folding or normalization occurs. The declaration ordinal
defines enum comparison, enum object-key order, and explicit enum rank fields;
reordering the members therefore changes canonical descriptor bytes and `SortId`.
The checked domain represents these through the `SchemaEnum`/`EnumSchema` schema
arms, `ValueEnum`/`EnumValue` value arms, descriptor `LiteralEnum`, and query
`QueryEnum`. Values and literals retain only their exact symbol, whose membership
is checked against the owning schema.

`SortId` is exactly `SHA-256(canonicalDescriptorBytes)`. Decoding re-runs descriptor
admission, re-encodes the result, and requires byte equality. The independent fixed
vectors in `test/CanonicalGoldenProperties.hs` pin the current derived
representation and cover every current semantic constructor family.

The predefined edge carrier stores strength as the enum
`preserve :| [weaken]`. Its schema rejects every other symbol and every non-enum
value during checked-publication admission. Stable numeric `0/1` codes remain only
in normalized internal startup/reconciliation evidence transcripts; they are not
domain values or an alternative carrier encoding.
