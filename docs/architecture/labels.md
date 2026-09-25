# Label decisions and completion

`label` changes the canonical owner of a controlled object through a compare and
swap on its complete label. The [semantic contract](../networked/01-semantics.md#59-label)
and [Oracle state machine](../networked/04-oracle-and-raft.md#7-label-state-machine)
own its exact admission, transcript and outcome rules. This guide explains the
separate lifetimes involved.

## Label, revision and authority

The canonical label is `(LabelOwner, Word64)`, where owner is a live process,
a zombie process or void. Application labels carry the same pair through private
process identities. Every successful operation advances the object generation
once, including same-owner and void-to-void success. Failed comparison, rejection,
End and retirement do not advance it. Exact replay installs the recorded result.

Fresh controlled publications and parent-labelled environment roots start at
zero. First-use admission rejects a supplied nonzero generation. Ordinary updates
restore the authoritative full label instead of trusting the supplied label.
Private/global translation and End's zombie projection preserve generation.

`LabelRevision` is the successful decision's Oracle control index.
`AuthorityEpoch` is an operator tenure. Same-owner success advances generation and
revision but retains tenure; a different live effective operator receives the
new label authority epoch. Deletion retains terminal successor generation `g+1`;
automatic disappearance retains `g`. Neither terminal is a readable sample or
permission to resurrect an object.

A retained publication observed at an earlier generation remains valid historical
evidence. Exact checked publication identity and value prove that observation's
admission. An unseen publication cannot gain admission merely by copying an old
generation. Current overlay and terminal suppression remain authoritative.

## Home acceptance and settlement

The home Herald checks genuine local capability, possession, descriptor support,
role, policy and target nameability. The caller's expected pair is a comparison
operand, not evidence of canonical state. A lagging local label or lifecycle view
cannot decide the canonical comparison or reject a canonically live target.

Pre-acceptance waits for already accepted publications in the exact sequencing
scope to obtain structural positions and for the membership successor to establish
its base. It retains only the typed request for retry/conflict classification:
no process position, fence, decision or Oracle intention exists yet. Session or
process End can discard that unaccepted candidate.

Acceptance retains the whole workflow, captures the process-scoped cut and
returns `LabelSettlementPending`. Preceding caller publications which publish or
forward the object, or use its nabla as authority, settle at their frozen
destinations before `DecideLabel`. Qualifying later publications stay held until
local decision installation. Unrelated application, peer, Store and Oracle work
continues. Caller/session End removes reply reachability but cannot orphan the
accepted command, settlement, installation or collection.

## One canonical decision

The Oracle compares and changes canonical state in one `DecideLabel` transition.
For a first overlay, checked generation-zero observation and separate initial
authority evidence establish the prior. The expected pair cannot authenticate
itself. An existing canonical record, including deletion, takes precedence.
Canonical End normalizes process to zombie before comparison.

Terminal-object admission takes precedence where the object is already deleted.
For an admitted nonterminal prior, the decision checks caller liveness, requested
process-target liveness, the full expected pair and permitted transition. A failed check yields one
immutable `NotApplied` reason; successful application yields one immutable
`Applied` result at the same control index. There is no Ready, Prepared, Release
or Abort wire phase. Internal names such as `Released` and `PreparedLabelFacts`
are immutable state/transcript representations, not extra protocol rounds.

One successful decision per object may await installation. Another decision for
that object is deferred before creating a receipt or control entry and keeps its
exact request bytes and identity for retry. Labels on other objects, End and
unrelated commands remain independently orderable.

## Installation and direct collection

Each captured Herald installs success atomically against its current checked
owner state. It derives effects then, allocates required Store identities once,
and preserves unrelated progress. No prepared whole-owner copy survives across
Oracle entries. A Herald lacking the target publication installs the overlay or
terminal suppression without inventing payload or possession. Later data is
projected through that state.

Historical `PeerLabelInstalled` facts go directly to the collector. They bind the
decision, reporter epoch, decision index and immutable outcome digest. Reconnect
replays exact facts; transport acknowledgement is not semantic completion. Later
End or topology changes do not invalidate an already true installation fact.

The active home collects, or after its retirement the least active captured epoch
collects. Retirement removes impossible requirements without changing the outcome;
new members never enlarge the captured set. If the last captured survivor retires,
that retirement completes the workflow and emits `LabelWorkflowCompleted` directly.
Otherwise a checked `CompleteLabelDecision` commits after required installation
and emits the completion. Success normally needs two semantic Oracle
commands; failed comparison needs one and creates no installation collector.

Uncertain submission reuses exact identity and bytes. A definite stale-generation
rejection permits a refreshed completion request under the ordinary receipt rules.
Completion witnesses support semantic rediscovery until all active members have
relinquished that entitlement.

## Retention and remaining work

Completed Oracle witnesses are compact and indexed by original decision position.
Each active Herald promises a completed prefix after installation, invocation
consumption and every retained completion request finish. The Oracle reclaims
through the minimum active-member promise. Disconnect does not remove a member
from that minimum; canonical retirement does. Pending decisions, current labels,
request receipts and historical authority have separate lifetimes.

This prefix policy can retain later witnesses behind one old pin. Checked Herald
replay bases and admission tails permit reclamation of raw control archives, but
Projection historical decision/authority facts, local installed label proofs and
the Oracle runtime watch ledger retain additional history. The implementation
does not establish bounded memory for indefinite label activity.
[Control history](control-history.md) states the implemented release rules and
remaining owner obligations. Timing defaults and isolated measurements cannot
substitute for a supported end-to-end cost or recovery bound.

## Diagnostic audit policy

`ECLIPS_DIAGNOSTIC_CHECKS=on|off` selects redundant local internal audits; the
default is `on`, and development/aggregate checks explicitly enable it. TCP startup
reads the option and passes strict pure configuration. It is neither serialized
nor inherited from a donor; joining staging retains the receiver's policy and
exact replay records it.

Optional work includes repeated whole-placement audits, whole-Herald/control-base
capture audits, repeated local Projection/Client capture validation, local History
transcript revalidation, and already-admitted structural-frontier coverage/closure
checks. Narrow local occurrence preparation can reuse the exact admitted frontier.
The standalone checked APIs and incoming/carried/terminal work retain their
required admission checks.

Wire canonicality and shape, ownership, monotonicity, conflicts, input admission,
missing meaning/occurrence handling, unfinished-label guards, source eligibility,
history context, seal/readiness bindings and historical cut/placement admission
remain mandatory in both modes. Disabling redundant audits does not remove actual
projection reconstruction, serialization, hashing, route-admission searches or
ordinary bookkeeping. Further reductions require implementation and evidence;
the option is not a claim that the remaining cost target has been achieved.

## Verification obligations

Properties cover full-pair comparison and ABA schedules, successful same-owner
increments, exact replay, canonical process death, intermediate historical
observations, unknown initial state, different-object concurrency, same-object
defer/retry, current-state installation, absent local payload, collector loss,
membership changes and completion-prefix pins. Protocol tests cover exact
transcripts and wrong-scope reports; composed tests cover delayed publication,
application loss and unchanged unrelated work. The [verification guide](../verification/README.md)
identifies current commands without extending earlier results to untested source.
