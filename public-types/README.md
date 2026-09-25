# `eclips-public-types`

This package owns the deliberately public identity types that must be shared by
application-facing and semantic code. It is intentionally much smaller than either
side of that boundary and contains no global unique IDs, controlled-object IDs,
private process names, capabilities, or component state.

Its first exercised type is `SortId`. A sort is regular shared data, and its ID is
the public content address of a canonical descriptor. The opaque identity checks its 32-byte representation. `SortCanonical` owns the
identity-free descriptor syntax, sole canonical encoder and SHA-256 hash shared
by application and domain code. `SortCatalogue` owns the six predefined descriptor
definitions. Semantic admission and value evaluation remain in `eclips-domain`.
The raw canonical syntax conveys no admission or endpoint authority.

The package also provides one domain-neutral diagnostic renderer used by
fixed-width opaque identities, digests, and protocol claims throughout the
current implementation. It renders lowercase hexadecimal in four-byte groups;
it is deliberately separate from all canonical and wire encodings.

`SortId` has a current-build `Binary` representation for the generic internal
protocol. Decoding repeats the exact 32-byte check, so malformed serialized data
cannot bypass its opaque constructor. The encoding is intentionally not a public
or cross-version compatibility contract.

`Eclips.Public.Types.Timing` owns the checked `TakeoverTarget` and the pure
`TimingPolicy` shared by deployment, Herald, Raft and application configuration.
The default target is five seconds to reach a committed takeover decision with
a responsive surviving quorum. Derived waits reserve thirty percent of that
target for consensus and processing; they never confer authority by elapsed
time alone. The smallest wait is `T / 500`, so admission requires a target of at
least 500 microseconds. Fractions round down at microsecond resolution. The
independent post-fence read-only drain defaults to 500 milliseconds.

The target has a checked current-build `Binary` representation so deployment
manifests and application startup descriptors can carry the same policy input.
See [the takeover timing model](../docs/architecture/takeover-timing.md) for the
budget, derived defaults and rationale.

`Eclips.Public.Types.ReceiptRetirement` defines the shared opaque receipt-lifetime
algebra. `receiptRetirement H E` checks that every unresolved exception is at or
below H. `receiptIsRetired` recognizes released identities; Semigroup/Monoid join
announcements by union of released identities, never reopening a consumed
exception. Binary decoding validates both this bound and canonical ordering.
