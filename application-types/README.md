# `eclips-application-types`

This package contains the ordinary values and identity types that may cross the
application boundary. It deliberately has no dependency on `eclips-domain`:
global unique IDs and global object, nabla, delta, and process identities therefore
cannot leak into these API types by accident.

`Eclips.Application.Types.Typed` derives the existing value grammar from Haskell
records, nested records, transparent newtypes and closed nullary enums. Define
`ValueType` and `ApplicationSort` once per application payload. Its opaque `Sort a`
contains the full checked policy and locally computed canonical `SortId`; host
type names do not participate in hashing. `Field a b` witnesses typecheck field
names, nested paths and scalar operands. Descriptor predicates exclude identity
and label literals; query predicates permit them. Sort compilation checks the
controlled key/label restrictions and appends the whole-value rank fallback.

The supported primitives are `Bool`, `Int64`, `ByteString`, `Text`, private unique
IDs, labels, and the existing optional-private-ID form. There is no general list,
optional value, floating-point or recursive schema. Generic schemas are the
single source for codecs and projections. Custom codecs must be total over that
schema and preserve both round-trip directions; refined subsets such as positive
integers require a separate application validation step.

`ApplicationValue` is a recursively constructible tree of ordinary scalar,
symbolic enum, private unique-ID, private process-label, and ordered-record values,
plus the dedicated structured sort-definition arm. `EnumValue Text` preserves its
exact symbol. Record keys are ordinary `Text` in this carrier. The Herald admits
their semantic field-name shape when it translates the complete value into the
checked domain, so malformed names remain an ordinary boundary rejection and this
package does not duplicate domain admission.

`ApplicationSortDescriptor` is an application-only draft AST. It contains schema,
projection, predicate, and rank syntax but no canonical bytes, mutation mode,
structural-carrier role, or controlled-identity literal. A declared definition may
carry an optional public `SortId` claim for the Herald to compare with the identity
derived by semantic admission. A separate closed predefined-definition arm names
only one immutable catalogue role and its public `SortId`; this lets read/take
present primordial Herald-managed descriptors faithfully without exposing their
canonical bytes or permitting an arbitrary carrier claim.
`EnumSchema (NonEmpty Text)` makes emptiness structurally impossible; Herald/domain
admission additionally requires pairwise-distinct exact members. Their declaration
order defines comparisons, enum-key order, and explicit enum rank fields, so
reordering members changes the descriptor identity. `LiteralEnum Text` must name a
member of the projected enum schema.
`ApplicationWriteValue` is the current one-write operation's executable
`PublishValue ApplicationValue | DeleteReserved PrivateObjectId` input sum.
Reservation cancellation is therefore not nestable as a value. Ordinary and
structural publication share the first arm and the Herald decides whether a
locally accepted request is immediately terminal or awaits structural
stabilization.

`ApplicationQuery` names a set of private delta handles and a recursively composed
scalar predicate. Query literals may contain private unique IDs and private
process labels, while descriptor literals deliberately cannot. Both grammars
include exact enum symbols through `QueryEnum` and `LiteralEnum`. Textual
projections and their scalar schemas are checked by the Herald against every named
effective descriptor. An empty delta set remains a valid local observation.

`RegularCallResult` is the closed eight-way result sum for newid, write, forward,
read, local take, wait, label, and private-environment creation. Newid returns
only a process-private `PrivateUniqueId`. The write result is either
`SortDefinitionWritten SortId` or the deliberately payload-free `WriteAccepted`.
Forward has the distinct payload-free `ForwardAccepted` terminal result. Label returns the redacted
`LabelApplied | LabelNotApplied` result. Environment creation returns the opaque
`EnvironmentAccess` bundle described below.

`ApplicationOperation` is the sole current eight-call union shared by clients,
protocol DTOs, and Herald request retention. Its exact typed equality classifies
retries and conflicts; there is no parallel request digest. `NewIdTarget` selects
either bare allocation or allocation reserved under a private controlled writer.
`ApplicationLabelTarget` selects a private process, void, or deletion, and the
label operation carries the caller's explicit expected `ApplicationLabel`
owner/generation pair as its value-CAS operand. Samples, queries, and expected
operands share that full pair; every successful label operation advances its
generation, including when the owner stays the same. `OperationPendingReason`
has exactly the executable `StructuralStabilizationPending`,
`LabelSettlementPending`, and `EnvironmentStabilizationPending` arms; each is
protocol request state rather than a separate application operation.
`ApplicationRejection` is likewise shared here so clients can observe semantic
failures without importing Herald internals.

`ApplicationStartupAccess` pairs the private self-process handle with opaque
`PrimordialAccess`: an exact application-defined key map whose values distinguish
identity-only, object, writer, reader, and process names. Empty and partial maps
are valid. Opaque `PrimordialSelection` describes the names to grant; checked
construction rejects duplicate keys, inconsistent role aliases, absent required
endpoint keys, and invalid optional source keys. Herald admission separately
checks current roles and normal possession, then localizes every selected name
into the child's private namespace. Receiving an alias alone grants no possession.
Granted startup access additionally preserves the actual immutable endpoint's
carried `SortId` for every writer/reader key and the object's own `SortId` for
normal object entries. Selections do not assert this evidence.
The application facade accepts it only from its connected opaque `Herald`;
constructing or decoding a startup DTO is not itself a trusted endpoint binding.

`EnvironmentAccess` remains a separate opaque result with exactly six ordered
writer/reader pairs, one neutral hub and eighteen supporting edge names. All
thirty-one identities are distinct. Edges have canonical predefined-role order,
then writer-to-hub, hub-to-reader and reader-to-hub within each role.
`environmentAccessPredefined`, `environmentAccessHub` and `environmentAccessEdges`
are total accessors for those parts.

`selectEnvironment` is the explicit convenience selection for all thirty-one
objects. The hub uses `environment.hub`; edge keys use
`environment.<role>.writer-to-hub`, `.hub-to-reader` and `.reader-to-hub`. Only the
twelve endpoints are readiness requirements, including the two carrier
source-writer keys used by `newenv`. Creating an environment does not
change those retained startup sources or silently enlarge the startup interface.
Both opaque values have canonical checked Binary decoders.

`Lifecycle` keeps process preparation separate from the eight data operations.
Its opaque connection descriptor binds a checked takeover timing target, target
locator, lineage, target epoch, and attachment bytes; it is not a process handle.
`PreparedChild` carries
the preparing parent's private child-process handle and the same preparation
reference. Lifecycle correlations retain the original session scope, session
ordinal, and request ordinal, so separate sessions cannot collide and own-End
result recovery does not need a live session. Initial claims use an independent
checked 32-byte identity. Construction and decoding establish shape; only Herald
admission establishes preparation, grant, readiness, or attachment authority.

`SortId` is re-exported from the smaller `eclips-public-types` package. It is the
deliberate exception to private application naming: a sort is regular shared data,
and its 32-byte content address crosses the application boundary unchanged.

`PrivateUniqueId` is an opaque process-epoch-local name. Its current prototype wire
representation is a positive `Word64`; zero is reserved as a structural sentinel
and rejected by the checked constructor. This representation is not a capability
or a security mechanism. The benign profile assumes applications do not forge
private IDs, and the owning Herald still resolves every private ID in the calling
process epoch before using it.

`PrivateUniqueIdSupply` lets the Herald keep allocator state behind an opaque type
that callers cannot reposition. Taking the next candidate does not create a binding.
The supply relies on profile 0.1's finite-run, no-wrap premise and therefore has no
exhaustion outcome.

The object, process, nabla, and delta wrappers refine an already-local private name
for an API role. They do not prove that the referenced global object exists or has
that role; the Herald establishes those semantic facts after resolving the name.

Ordinary wire-reachable application syntax has generic current-build `Binary`
instances. Opaque private identifiers, startup access, and environment access use
validating decoders that repeat positivity and canonical-role checks. Stock
collection instances are used under the benign same-build deployment premise.
These encodings are internal representation plumbing, not an API or protocol
compatibility promise.
