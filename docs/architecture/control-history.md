# Control history, joining bases and reclamation

Correctness-relevant evidence has an owner and an explicit release rule. A high
watch cursor, a received frame or an ended application does not by itself end
all uses of an older entry. The current implementation combines request-receipt
retirement, label completion promises, checked semantic replay bases, sparse
request evidence and admission-scoped control tails. These mechanisms reclaim
specific records; they do not establish a bound on every semantic history.

## Oracle request receipts

Each home Herald epoch allocates monotonically increasing request sequences.
Until release, an exact duplicate returns its immutable receipt and conflicting
reuse has the defined conflict outcome. Accepted and rejected committed commands
both participate. A replicated retired frontier ends exact-result entitlement
for covered sequences: later submissions and queries receive an explicit retired
outcome and cannot execute again or create replacement rejection receipts.
Canonical retirement closes a home irrevocably, including sequences beyond its
old prefix. Disconnect and local isolation do not close the canonical home.

The home has separate ready and confirmed progress. Readiness requires exact
results to be projected and retained for local consumers. Awaiting/deferred
requests remain holes. A watch cursor, socket receipt or queue write does not
establish readiness. Confirmation records committed release; until then the home
may reoffer the same or greater ready progress.

Progress metadata is excluded from command identity. An ordinary retry can carry
newer progress without changing its request ID, command digest or retained result.
A coalesced idle flush uses no new ordinary request sequence. Maintenance entries
advance control order but do not constitute unrelated semantic progress that could
release a rejected workflow and create a rejection/acknowledgement loop.

The Oracle request map and runtime published-result map apply the committed release
rule together. Local Herald consumers, entries embedded in the Oracle watch ledger,
semantic request keys and outstanding physical copies have their own lifetimes.
Reclaiming an Oracle receipt is not permission to discard their evidence.

## Label completion promises

A compact Oracle witness records decision ID, original decision index, captured
membership generation and outcome digest. It supports semantic completion
rediscovery independently of an exact command receipt.

A Herald's promise through F says it will create, submit or requeue no further
completion request for a decision whose original index is at or below F. The
promise follows the last locally consumed label-decision index, with indexed pins
for installation/collection, source invocation and every prepared completion
request. A former collector's request remains a pin until its own immutable
result is projected, even if another collector completed the decision.

The earliest pin selects the preceding actual label-decision position; absent
pins permit the complete local high water. Unrelated Oracle indices do not create
label progress. The Oracle keeps a promise for every active epoch and an
irreversible global floor equal to their minimum. Canonical retirement removes a
member; disconnect does not. A newcomer consumes the checked observer history
and starts at the current latest label position without acquiring historical
completion obligations.

Checkpoint admission checks active-member coverage, ordered floor/promises/high
water, and disjoint pending/completed decision coordinates. A fresh absent
completion at or below the floor receives `LabelCompletionObsolete`; it does not
manufacture success. Ordinary exact-receipt and request-retired classification
run first. One old pin may retain arbitrarily many later witnesses.

## Semantic bases and exact evidence

Projection separates current semantic facts from the raw canonical entries which
established them. A checked replay base at B contains those admitted facts; later
entries replay from B. Membership, process, label, disappearance, voter and failure
facts retain their domain-specific meaning and identity. The base does not import
a donor worker's mutable state.

Projection distinguishes exact retained overlap from an entry merely covered by
the base. Retained bytes still reject unequal overlap. An unretained covered entry
is inert and supplies no result, receipt or event. The composed watch path requires
both Projection and Client to cover it before ignoring it.

Client retains sparse exact evidence for outstanding result interpretation,
projected requests, ended-before-submission requests, disappearance suppression
and label consumers. These pins do not hold the whole prefix. Reclamation changes
neither request settlement nor receipt acknowledgement, label consumption, original
start cursor or current applied cursor. Sparse label-order facts can derive from
checked Projection workflows, while exact local request results keep their own
byte and digest requirements.

A paired private operation captures immutable semantic roots and preserves
Client's exact pins in Projection along with the required contiguous tails. It
shares admitted state rather than replaying complete history at every callback.
Exhaustive diagnostics remain a separate check; a genesis replay audit explicitly
reports when its original bytes are no longer available.

## Portable owner admission

One canonical aggregate contains five explicit frames: Projection, Registry,
Progress, structural carriers and semantic History. Its digest binds the entire
envelope and is absent from its own preimage. Each source captures it before Seal
and retains the exact bytes for that admission attempt. Recapturing after Seal
cannot produce a replacement. Equal History with altered owner facts changes the
member cut. A source checks its own frozen capture when accepting the seal.

The receiver independently admits nominal owner facts against immutable genesis,
expected source, system, admission identity/attempt, manifest, predecessor and
control cut. Projection's covered floor equals History's anchor and its exact
suffix agrees with History. Registry admits descriptor/retirement history;
Progress provides independent membership, applied-vector and certificate facts;
carrier admission binds original publications and historical meanings against
those checked facts. Current definitions cannot reinterpret an earlier admitted
carrier or certified cut.

The atomic transaction prepares receiver-owned control and Registry facts,
reconstructs carriers and graph, imports Progress, restores observations and
terminal suppression, and validates the whole successor before exposing effects.
It keeps the receiver's identity and runtime context and does not allocate foreign
private Stores, sessions, request allocators or operational pins.

## Coherent source selection and replacement

A joining observer buffers the exact predecessor membership's frozen bundles.
Partial collection changes only buffered Join material and returns retry; it
publishes no semantic base. The expected source set comes from the checked
admission predecessor, including members admitted after immutable genesis.

The semantic donor is at the common certified structural cut K. Among those
donors, choose the greatest control cursor, then least source epoch. A donor at an
older structural cut may have a larger control cursor; cursor order alone cannot
select the base. From selected cursor D to greatest source cursor T, an
independently admitted donor Projection supplies the contiguous tail D+1..T.
Its anchor may not exceed D. Missing bytes remain a rejection, not an inferred
history. Source overlap is checked after that one tail is applied.

Other sources contribute required data and outstanding obligations through
ordinary admission. Deterministic passes reach a finite fixed point because a
later source may supply an earlier source's plan, certificate, placement or exact
terminal receipt. Completion requires every source, final cursor T, unchanged cut
K/vector, exact admission and the whole-Herald invariant. The least member's
alignment-frontier authority remains independent of the selected semantic donor
and becomes executable only at ordinary activation.

A fresh applicant's staging is unobservable. An independently checked newer
attempt may replace it through the same fresh-owner transaction, preserving local
protocol/request context and receiver-owned retention promises. Canonical
invalidation releases an older readiness promise; a still-current sealed attempt
must preserve its exact promise. Terminal admission cannot revive. Equal completed
source retry is acknowledged without rebuilding the base; conflicting bytes fail.
Higher-attempt evidence releases older partial source groups; lower late input is
retired rather than starting another group.

Control catch-up uses the semantic applied cursor, which can trail separately
observed control during fencing. It stops on activation, including activation
inside a batch, because the ordinary Oracle connection owns subsequent entries.
Cancellation also ends retry eligibility.

## Admission tails

A checked base may replace old control entries while an applicant still requires
a contiguous catch-up tail. Join retains an opaque `AdmissionControlTail` naming
the admission, applicant, immutable Begin and exclusive floor. A floor B retains
all exact entries B+1 through the local semantic cursor. Sparse pins cannot supply
a missing interval.

Canonical Begin registers a source promise at Begin. Seal, readiness and attempt
invalidation retain it. The applicant also retains a reconstruction promise. A
complete source adoption advances that promise to the imported source cursor
before newer local suffix replay: importing at 20 and replaying to 25 must retain
21..25 because another replacement can still use the same frozen source at 20.
The floor is monotone and its change is atomic with adoption. Donor-local pins are
never imported.

Local activation ends the applicant's promise. A donor's observation of Activate
alone does not end its promise: the applicant may still request that entry.
An accepted ordinary Hello from the applicant must carry a semantic cursor
covering the exact Activate. Ordinary Hello is impossible before local activation;
initial and reoffered accepted Hello therefore supply existing completion evidence
without a new acknowledgement message. Rejected, stale or insufficient-cursor
claims cannot release a tail. Historical Activated records cannot recreate it.

Canonical cancellation or applicant retirement releases the corresponding tail.
A retired source clears promises because it no longer serves onboarding. Frozen
canonical observation can discharge terminal promises without resuming semantic
work. Tail reads admit below-base cursors only when the complete suffix exists;
ahead cursors and absent intervals reject.

## Live reclamation boundaries and limits

Paired Herald Client/Projection reclamation runs after a complete ordinary Oracle
batch, outer joining-history request, complete source adoption, or accepted Hello
that actually releases a tail. It does not run inside per-entry scratch joining
replay before the final floors exist. Ordinary batches schedule it on canonical or
coverage progress or actual Client evidence/request removal; unchanged callbacks
are inert. Same-cursor cleanup can release sparse pins without a new control row.

This releases raw canonical Herald archives. It does not release all historical
Projection decisions/authority, compact installed LabelPatch proofs, semantic
structural history, Store histories, alignment certificates or membership facts.
Delayed publications and current object predecessors still consume that evidence.
The Oracle runtime watch ledger separately retains its complete control prefix;
a transferable watch base and native snapshot-recipe lifetime contract are
required before releasing it. A lagging member can pin progress indefinitely.

In-memory Raft checkpoints and native log reclamation are separate from durable
restart and from these watch/Herald lifetimes. No documentation of a release rule
certifies an indefinite-run memory bound or a measured label-cost target.
