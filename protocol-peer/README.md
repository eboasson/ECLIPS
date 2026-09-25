# ECLIPS Herald-peer protocol

This package owns the pure current-build Herald-peer wire vocabulary: nominal
structural claims, the closed current Hello/heartbeat/control/publication DTO
sum, generic `Binary` payload encoding, and incremental EPRP framing built on
`eclips-protocol-frame`.

The dedicated established-lane heartbeat arm uses a nominal lane-local nonce
that correlates `Ping` with `Pong`; the physical connection owner
consumes both, so heartbeat traffic is neither semantic peer control nor an
ordered publication-stream item.

The DTOs contain no Herald owner values. `Eclips.Herald.Peer.RPC` is the sole
adapter that admits these structural claims into checked kernel values and
projects checked outbound values back to DTOs. This package therefore depends
on the public `SortId`, checked application lifecycle values, and framing support; it has no Domain, Herald,
runtime, networking, concurrency, clock, entropy, persistence, or PoC dependency.

The publication item is the closed `OrdinaryPublication |
StructuralPublication` sum. Structural items carry the complete checked-shape
occurrence stamp and every batch names its distinct topology-cut and Oracle
control prerequisites. A batch also binds its Normal/Weak source strength; every
destination is no stronger, preserving Forward attenuation across the wire. The
ordered stream enclosing those publications additionally carries route-cutover
markers and disappearance-probe markers. Labels use source publication-group
completion and Oracle readiness reports, without peer label-fence markers.

Structural progress reports, the announce/accept/establish/established-ack
topology-cut repair vocabulary, and the alignment control family are first-class
peer controls. Alignment consequences distinguish structural-occurrence,
label-release, process-End, and controlled-disappearance causes. The alignment
family also carries exact disappearance-probe revision markers. `AuthorityEpochDto` is exactly the current
three-arm `Genesis | Structural(occurrence, topology cut) | Label(control index)`
sum, mirroring the live Domain authority without a parallel compatibility form.
Structural and placement vectors carry their exact membership-generation ID and
member-set digest, and topology predecessors distinguish ordinary same-generation
lineage from the first membership-successor lineage with its terminal and
survivor-projected vectors. Existing producers, consumers, codecs, and tests use
those shapes together. The live peer-control sum also carries fresh direct failure
probe request/response and the terminal inventory, payload request/relay, union,
acceptance, and establishment family. Canonical destination-free terminal payload
bytes cross EPRP and are admitted back into the checked Herald algebra before
survivor repair materializes them; the runtime only frames and routes these typed
controls.
The peer-stream controls distinguish a one-way monotone allocation-frontier
advance used by ordinary enqueue from the full bidirectional resume exchange used
for establishment, reconnect, and exact gap repair. The first Resume claim on a
new association reactivates only requested work from before its Hello cut; an
attempt already selected or written on that association is preserved. Repeated
equal Resume claims cannot allocate another attempt for the same repair.

Immutable alignment cut acceptances and member-ready facts use a separate
per-destination delivery sequence. The closed `AlignmentRetainedEvidenceDto`
contains only those two payload kinds. `PositiveAlignmentDeliverySequenceDto`
is distinct from the semantic member-ready evidence sequence and publication
stream sequence. The sequence and unconfirmed outbox survive TCP rebinding;
evidence created while disconnected is retained for the logical peer.

Both established control and publication envelopes carry an opposite-direction
`ReceiptRetirement` header: a high water with sparse unresolved positions. The
receiver advances it only after semantic admission or exact dependency retention;
it is not a historical-readiness certificate. The pure Herald admits header and
payload atomically. These headers do not enter semantic payload digests, and Hello
and heartbeat envelopes keep their separate physical-connection roles.

Progress piggybacks on the next outgoing envelope. A single idle flush at `T / 500`
sends the unsequenced `AlignmentDeliveryProgressDto` control if no carrier arrives;
a carrier cancels that timer, and progress-only traffic requires no receipt of its
own. Confirmed outbox payloads and their key index are reclaimed. Reconnect replays
the indexed outstanding tail after initial catalogue seeding, and membership
retirement releases that peer's delivery state. Semantic generation and certificate
history has separate lifetime rules; delivery receipts do not reclaim it.

The current wire schema is intentionally not a stable or version-negotiated
interface. The current profile changes every producer, consumer, codec, and test together
and begins fresh experimental runs after a wire change.

Hello includes an optional advertised application locator. Only admitted
current peer identity supplies that contact; losing a binding retains the known
contact and membership retirement removes it from resolution. Preparation
Offer/Query/Cancel/Reply controls carry stable preparation identities and
canonical retained transfer material. They contain global identities and exact
control evidence; private application handles never cross this boundary. The
Herald transfer owner rechecks local applied facts before admitting a grant.
