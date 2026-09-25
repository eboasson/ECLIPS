# Application payload types

## Requirements

One Haskell application type determines one complete sort: value representation,
kind, keys, rank, validity, obsolescence, retention, immutability and any designated
controlled label. Types remain within the existing ECLIPS value grammar. The
application-facing dependency cone must contain no globally unique identities.
The eight-operation wire vocabulary, deterministic kernels and lifecycle ownership
remain unchanged. Endpoint definitions are immutable: the predefined topology
catalogue marks all four structural sorts immutable, and ordinary publication
admission rejects writes to their existing objects. Labels remain mutable through
the separate label operation.

## Design

`ValueType` derives a schema and codec from `Generic` records, nested records,
transparent newtypes and closed nullary enums. `ApplicationSort` adds a kind-indexed
pure policy. Typed field witnesses tie projection paths to their operand types.
Compilation admits the application policy once and returns opaque `Sort a` with
its content address. A changed policy uses a distinct Haskell type, normally a
newtype over an existing representation. Equal descriptors still have equal
identities regardless of Haskell names.

The shared identity-free canonical descriptor syntax and predefined catalogue
live in `eclips-public-types`. Domain code admits descriptors and evaluates values.
Both domain and application compilation use the same encoder and hash and must
match the golden bytes and catalogue digest.

`Eclips.Application.Typed` owns nominal endpoint, query, reservation and controlled
object handles. Each operational handle retains the originating `Herald`. Public
private-ID accessors support child selections without exposing constructors in
the reverse direction. Cross-session query composition and forwarding are rejected
locally even when the numeric private names happen to coincide.

## Binding and lifetime

Startup localization preserves the actual admitted carried `SortId` for every
selected endpoint. Parent selections contain private names only. The facade reads
this evidence from its opaque connected `Herald`, compares it with the compiled
application sort, and binds once. Publicly constructed DTOs are not trusted evidence.
The complete predefined interface is checked against the shared catalogue.

Endpoint creation returns a handle only after the reserved carrier's successful
publication. Advanced callers can retain a nominal `Creation result` between
reservation and publication, cancel through its original writer, and inspect the
exact publish call after interrupting its waiter. Simple creation helpers compose
those same steps; an unknown or interrupted publication remains potentially
accepted and must not be retried under a fresh identity. Discovery is fused with
a library-owned read of the appropriate predefined carrier delta, preserving the
localized identity and verified carried sort. Arbitrary application records cannot
mint endpoint handles.

An endpoint's disappearance or loss of possession remains an ordinary operation
failure. Sort occurrence retirement does not change the static type; restoring
the same descriptor restores the same content address. Handles do not pin lifetime.
This API does not impose type-level session parameters on payload records: private
identity fields retain the existing runtime namespace checks.

## Calls and validation

Typed `Operation result` constructors lower to the existing calls. A `Call result`
retains that exact invocation plus its result decoder. Await, status and cancellation
never resubmit. Wait cancellation preserves its original wake/cancel winner;
cancelling a mutation waiter does not undo or repeat accepted semantic work.
Read/take returns decoded application values or a structured codec error.

Generic codecs must satisfy both value round trips and schema-total decoding.
Custom codecs may implement transformations only when both laws hold for the
fixed derived schema. Unsupported recursive shapes are rejected without arbitrary
resource limits. Descriptor literals exclude identity/label values; query literals
include them. Rank policies always finish with whole-value ordering. Controlled
keys and labels are checked for direct paths and forbidden policy references.

Validation includes shared canonical golden vectors, schema and codec properties,
Herald/app identity agreement, startup evidence properties, real TCP typed-call
and cancellation tests, compile-failure boundary probes, and the two complete
example workflows including context retention after publisher exit and Herald loss.
