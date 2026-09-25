# Profile 0.2 proof contracts

This independent test project supplies executable reference schedules for
singleton Raft, child attachment, membership changes and object-label
generation/CAS replay.
It imports no ECLIPS production package, exposes no library, and is not a runtime
implementation. Like the boundary tools, it has its own Cabal project and is
run explicitly by the root verification gates.

Prepare the compiler and configuration described in
[BUILDING.md](../../BUILDING.md), then run from the repository root:

```sh
TMPDIR="$PWD/.cabal/tmp" cabal --config-file="$PWD/.cabal/config" test all -j4 \
  --project-dir=tools/profile-0.2-proofs \
  --ghc-options=-Werror --test-show-details=direct
```

Both current gates run this independent project explicitly alongside the root
production suites: `sh scripts/check-development.sh` for development and
`sh scripts/check-profile-0.2-acceptance.sh` for complete release acceptance.
The [verification guide](../../docs/verification/README.md) distinguishes these
independent models from source audits, production tests and complete aggregate
evidence.

## Observation contracts

| Risk | Independent evidence | Incorrect policy distinguished | Production conformance |
| --- | --- | --- | --- |
| Object label generation and exact replay | Mutable object state compared against successful-release event counting | Owner-only CAS, elided same-owner success, incrementing replay or End | Domain, Oracle and Herald label properties |
| Singleton self-vote and commit | Physical no-op/application log, committed prefix and applied application sequence without remote messages | Election or commit progress checked only on remote responses | Native Raft and Oracle runtime properties |
| Lost first child-attachment reply | Event-history oracle locates the winning pre-End claim and expects the identical child/session/full startup access on retry | Forget receipt after consume; replay successful access after End | Lifecycle, attachment and OS-process properties |
| Second retirement before base establishment | Fixed-point dependency closure compared with exhaustive feasible prefix vectors from current-survivor payload reports | Require first successor establishment; count stale retired-reporter evidence | Membership and repeated-retirement properties |
| Join activation racing End | History oracle requires a current captured cut and exact old-member plus newcomer readiness | Keep pre-End seal; count old-member readiness without newcomer | Join, history and admission properties |
| Joint-configuration leader loss | Per-replica logs, terms and votes checked for leader completeness, committed-prefix preservation and commit agreement | Union majority; elections using stale genesis configuration | Native Raft, voter and ERFT properties |

Named counterexamples actually execute the faulty policy and assert an observable
difference or safety fault. Generated schedules exercise the reference models;
finite permutations make the selected races reproducible. Test scenario sizes
are evidence choices, not production limits. QuickCheck remains randomized and
prints replay parameters on a failure.

## Model boundaries

The lifecycle model starts after a prepared child is live, its private map and
selected access are installed, and readiness is satisfied. Reply delivery/loss
is separate from atomic logical claim. The oracle uses event positions rather
than reproducing the retained receipt map. Preparation, source-writer admission,
label transfer, cancellation, reconnect grace and OS spawning are separate
live-owner and TCP test obligations. End overrides old attachment success; this
does not model the separate recovery of a terminal own-End result.

Join capture abstracts already checked old-member seal collection. It does not
require newcomer readiness to obtain the cut needed for transfer. Exact reports
include attempt, semantic token and candidate; duplicate attempts cannot recapture
or clear readiness. End invalidates the token. Old-member and newcomer readiness
are separate. Reconstruction, all stamped high-water marks, publication drain,
other invalidating commands and the real Oracle/watch order are separate
live-owner and runtime test obligations.

The retirement model starts at one established anchor with a sequence of admitted
retirements, retaining that anchor across an interrupted attempt. Reports carry
exact occurrence payloads and dependencies; only current survivors reporting the
current lineage count. Survivor repair histories can extend their coordinates
beyond the anchor when terminal-source dependencies require it. The independent
oracle enumerates all-source feasible vectors, maximizes terminal prefixes and
minimizes the required survivor extension, instead of replaying the fixed-point
and dependency algorithms. It models prefix selection and exactly-once
settlement observations, not graph/store reconstruction or actual report transport.

The Raft model has physical per-node logs, terms, votes, effective logged
configuration, local commit/application progress, crash exclusion, append messages
and separate acknowledgements. Full-prefix messages abstract log repair; they do
not truncate a matching longer suffix on a delayed short message. A global
committed-entry registry is a test observer, never protocol knowledge. It checks
safety without calling the quorum predicate. Singleton application sequences have
an independent expected projection; physical no-op indices do not become application
indices. Native adapters and canonical wire bytes are separate production
conformance obligations.

Joint prefix-interruption schedules assert safety. Progress schedules explicitly
supply disseminated configuration and reachable dual majorities. Their final
configuration append proves native log/quorum progress; the ordered Oracle
adapter application/readiness gate remains a separate production conformance
obligation. The model does not assert availability after arbitrary partial
dissemination or loss of quorum,
nor prove restart/recovery, clocks, full protocol fairness or every possible
configuration history. Passing this suite is evidence that the named schedules
can expose specific errors. It is not a proof that the production implementation is correct.

The label model separates invisible preparation, Release, abort, process End
and automatic disappearance. It checks observable pairs and terminal results
against an event-history oracle that counts successful releases and locates exact
request receipts. Competing prepared calls compare again at their abstract Release
point; this is a reference linearizability contract, not the production barrier
implementation. It models no possession, private identity maps, membership quorum,
transport, authority epochs or publication admission. Those remain domain, Oracle,
Herald and real-TCP conformance obligations. Integer generations avoid adding
exhaustion behavior outside the prototype model.
