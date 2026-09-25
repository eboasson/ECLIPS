# Deterministic failure corpus

These seed IDs select explicit schedules. They do not fix QuickCheck's default
seed: ordinary property components remain randomized and Tasty prints replay
options when a property fails. The corpus belongs to the current prototype
schema and uses fresh in-memory deployments.

| Seed | Component and exact fault script | Required evidence |
| --- | --- | --- |
| 160100 | Herald core: LocalTake; collect Resolve; accept relabel; commit the label decision first; reject late Resolve; complete label collection; repeat ingress | Atomic decision/invalidation, exact label wait, unchanged controlled authority, fresh disappearance candidate after completion, terminal replay, and retention of the compound entry while its disappearance event is still needed |
| 160101 | Herald core: same setup; Resolve wins before the label decision | Controlled deletion, immutable NotApplied decision with transition-no-longer-permitted outcome, no resurrection, terminal replay |
| 160102 | Herald core: LocalTake; collect Resolve; accept republish; commit Invalidate first | Whole publication held before terminal, stable retry identity, release without deletion |
| 160103 | Herald core: same setup; Resolve wins before retained Invalidate | Exact terminal deletion and publication-gate settlement |
| 160110 | Herald core: canonical Open; checked destination subscription created; repeat evidence observations | One stable invalidation, immutable captured cut, no new Report/Resolve |
| 160111 | Herald core: canonical Open; cancel captured subscription and prepare replacement; repeat | Same invariant through a new destination subscription identity |
| 160112 | Herald core: canonical Open; cancel captured relation; repeat | Cancellation invalidates the original cut without rewriting its evidence |
| 160113 | Herald core: canonical Open; cancel captured source incarnation; repeat | Source loss invalidates the original cut; no absence inferred from loss |
| 160120 | Herald core: complete a checked local source/destination base; canonical Open; repeat | Exactly one local marker assignment, receipt and completion, no self-peer frame or fabricated remote report |
| 160121 | Herald core: canonical Open while the local base is outstanding; deliver the base through checked Transfer controls; repeat | Marker completion waits for the actual applied revision; the same three local owner transitions occur exactly once |
| 160130 | Herald core: old dispatch Deferred; replacement writes; first Resume; duplicate Resume; changed gap | The first Resume preserves repairs already written on its association; later changed gap evidence still repairs |
| 160131 | Herald core: old dispatch Deferred; first Resume; replacement writes; duplicate Resume; changed gap | Repair-before-write ordering retains the same one-attempt ownership |
| 160132 | Herald core: old dispatch Deferred; replacement selects an attempt; first Resume; Written; duplicate Resume; changed gap | The first Resume cannot supersede the replacement's already selected repair |
| 160133 | Herald core: two writes; same-binding Hello reoffer; third write; first Resume | The first Resume repairs the two pre-Hello writes and preserves the post-Hello write |
| 160134 | Herald core: first old write succeeds; second callback is Deferred; replacement selects it; first Resume confirms both receipts before Written | Both confirmed items stay suppressed, the superseded callback is inert, and later raw-prefix regression still requests exact repair |
| 160135 | Herald core: retain received prefix; lower raw Resume requests repair; write one item and select the next; a later raw gap confirms that selected item | A confirmed gap remains suppressed even when normalization removes it beneath the retained received prefix; later removal of that raw gap permits repair |
| 161001 | Herald core: Q=0; stop H4; commit retirement; establish successor base | No probe cleanup or reopen work |
| 161002 | Herald core: Q=1; complete old marker cuts; stop H4; retire; submit stale old reports; establish successor base; reopen and resolve | Exact predecessor Abort, successor generation/cut, three real reports, accepted terminal aliases; replay preserves state and disappearance work while permitting unchanged Oracle watch/retained-request offers |
| 161003 | Herald core: Q=3; reverse Open order; otherwise same script with simultaneous subjects | Independent subject ownership and additive cleanup/reopen bound |
| 162001 | Real TCP: loss-free baseline; fresh deployment holds the H1→H2 and H2→H1 publication markers plus the actual H1→H2 alignment marker after writes and before either destination admits/acknowledges them; close that duplex connection once; reconnect | Same logical P and A_remote, exactly one additional write for each of the three preidentified immutable markers, four reports and one canonical resolution; local A is checked in the focused owner corpus |
| 162002 | Real TCP: hold an accepted H1 newenv root on H1–H4; crash H4; retire and establish H1/H2/H3; finish original newenv; controlled LocalTake/disappearance; regular definition retirement/equal redefinition; fresh private successor newenv; publish a private writer-to-reader Edge and definition | Original twelve roots and writer provenance, exact successor lineage, both terminal families, Resolve-qualified redefinition, owner visibility and privacy from Q and the original environment, attributable work |

The controlled race seeds also choose one to three exact duplicate application
ingress deliveries. The membership seeds select Q and the Open order; the TCP
seeds select their named latch/connection fault scripts. Neither uses wall-clock
timing to choose a winner. Existing scenario, progress and application-call
deadlines remain hang detectors.

Prepare the required local toolchain using [BUILDING.md](../../BUILDING.md).
Run one pure schedule from the project root, substituting its seed ID:

```sh
cabal --config-file="$PWD/.cabal/config" test eclips-herald-core:test:herald-core-properties -j4 \
  --ghc-options=-Werror --test-show-details=direct --test-options='-p /seed-161002:/'
```

The four controlled-race names use `seed-160100-` through `seed-160103-` rather
than a colon. Their replay pattern is, for example, `-p /seed-160100-/`.

Run one real-transport case in a fresh process:

```sh
cabal --config-file="$PWD/.cabal/config" test eclips-herald-runtime:test:herald-step16-failure-properties -j4 \
  --ghc-options=-Werror --test-show-details=direct --test-options='-p /seed-162001:/'
```

Run the repetition gate:

```sh
TMPDIR="$PWD/.cabal/tmp" sh scripts/check-step-16-failure-tours.sh
```

The script verifies that each named pattern selects exactly one case, then runs
each case in three consecutive fresh processes. It exits on the first failure;
a failed schedule must be diagnosed before another certification attempt. The
component itself sets `NumThreads 1`; the ordinary development gate runs it once
as a timing-sensitive component. Final work evidence is read after semantic
completion and joined runtime scopes.

The membership fixture pins predecessor cleanup to `a0=0`, `aQ=3` (one canonical
terminal application per surviving owner). Each successor candidate has `M=3`,
`P=6`, `A=0`, `W=0`, so its ordinary work is `7*3 + 4*6 = 45`. The complete
measured interval is exactly `3*Q + Q*45 = 48*Q`, including Q=0. Candidate
reactivation belongs to the successor term and is never charged twice.

Alignment creation, replacement, cancellation and source loss touch respectively
one, two, one and one subscription evidence facts. Each retains one stable
invalidation without allocating another probe or marker; equal observations
change no state, counters or effects. Replacement counts the removed and added
subscription identities; its empty new transcript adds no payload blocker.
The local-subscription schedules independently pin assignment, receipt and
completion to exactly `3*A_local` and wait for the actual destination watermark.

The dispatch schedules check replacement-connection replay when an item is
written before the first Resume. Suppression identifies the successful attempt's
association, so the Resume cannot make an already received item eligible again. A first Resume
preserves attempts selected or written within its own association while repairing
older work. A Hello reoffer starts a new association even on the same binding;
the focused tests retain that repair behavior and later changed-gap repair.
Receipt-before-callback schedules also require the Resume's prefix and exact gap
confirmations to suppress retained active items when they supersede an attempt.
Otherwise the owner can allocate another ticket for an already-received item.
Raw-prefix regression and later gap removal retain their exact repair behavior.

The marker fixture separately counts immutable logical markers and successful
physical frame writes. Its full mesh has `P=12` and an actual remote live
alignment subscription (`A_remote>0`). Local alignment subscriptions are also
captured and complete their checked marker through the owner with no TCP frame.
The loss-free physical result is `P+A_remote`; the one-loss result is
`P+A_remote+L`, with each affected item written exactly twice and
`L=3<=P+A_remote<=P+A`. Both directions belong to the same checked peer
candidate, and the three affected identities are fixed before the one close.
Oracle request identities and canonical Report/Resolve events are checked separately
from transport attempts. Existing per-family `newenv`, removal, retirement and
ordinary disappearance bounds remain enforced by their focused owner suites.

The retirement tour closes each finite setup wave using one coherent snapshot
of immutable kernel traces. Every surviving dispatch has a corresponding owner
selection, write outcome and remote receipt; every distinct control fact has its
remote receipt, including follow-up effects emitted by that receiving transition.
Already-covered exact control replays are inert on the stable survivor bindings.
This is semantic readiness and does not replace physical attempt accounting or
add elapsed-time stability waits.

Each [`newenv` environment](../architecture/environment-hub.md) contains twelve
endpoints, a hub and eighteen edges. The privacy check retains its explicit
owner-private Edge, then proves the private reader
sees its definition while Q and the original P reader do not. The fresh `newenv`
interval ends before this Edge and contains zero physical Oracle submits.
The complete structural interval has exactly 64 source occurrences: two 31-object
environments, one Neutral and this Edge. The first endpoint phase freezes four
members, while its wiring and the complete second environment freeze three.
Its fixed directed handoff bound is `E <= (12+1)*3 + (19+31+1)*2 = 141`.
Oracle attempts are measured below connection deduplication and retain the
ordinary `O + O*V + 4*X` bound (`V=3`, `X=0` here).

## Evidence scope

The schedule descriptions and equations above are assertions required by the
retained tests, not results for an arbitrary checkout. The current aggregate
also runs membership, native Raft, voter, OS-process and completion tours through
`scripts/check-profile-0.2-*-tours.sh`. Use the
[verification guide](README.md) for their relationship to the complete gate.
A passing named schedule covers its specified faults and observations; it does
not establish all network interleavings or certify the complete repository.
