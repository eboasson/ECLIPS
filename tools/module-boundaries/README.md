# Module-boundary checker

This development-only package checks the exact default package plan, parsed Cabal
component metadata, and production Haskell imports. Cabal files are decoded with
`Cabal-syntax`, so continuation lines, common-stanza imports, component visibility,
source roots, library dependencies, and executable build tools are checked as
Cabal understands them rather than as line-oriented text. The public/application
vocabulary, domain, common framing,
application/peer protocol, pure application client, callable application facade,
pure Raft/Oracle kernels, their wire protocols, the Oracle/Raft runtime, Herald
kernel, and Herald runtime packages' exposed or re-exported module surfaces are
pinned exactly; every package retains its reviewed import and dependency walls.
The membership ledger also watches checked finite histories, descendant lineage,
qualified vector projection, compound closure owners, certificate-backed joining
observers, and Oracle runtime history publication and Hello admission. A new read or projection
site needs an explicit reviewed source entry; merely importing a shared type does
not exempt it from the audit.
Import walls cover ordinary, literate, signature/boot, and Cabal-preprocessed
Haskell sources rather than only files ending in `.hs`.
The Herald-runtime package shape also pins the unexposed fixture-node executable
and its separate process test, including the exact `build-tool-depends` edge that
puts the Cabal-built node on the test path without a guessed binary location.
The Herald- and Oracle-runtime passes also reject the concrete capacity,
exhaustion, versioning, authentication, rate-limit, generic-timer, and dial
identifiers that the finite profile explicitly defers. Their negative fixtures
prove that this source-policy check is active rather than relying on a
release-time search.

The checker policy is declared once per package in an exhaustive `PackageSpec`
registry. That registry generates the exact ordered root `cabal.project` package
list and independently pins each manifest name, component wall, production source
roots, allowed ECLIPS namespaces, runtime-free/runtime-boundary classification,
and diagnostic label. The seventeen architecture packages remain distinct from the
checked auxiliary API tour and context example, so later architecture packages
have one auditable addition point without turning the inspected manifests into
policy. The context example's production modules may import only the ordinary
application facade, advanced typed calls, connection descriptors, and application
types; negative fixtures reject Domain and low-level runtime imports.

The checker package has its own `cabal.project`; it is deliberately absent from
the default architecture/example plan. Select the local toolchain using
[BUILDING.md](../../BUILDING.md), then run from the repository root:

```sh
sh scripts/check-module-boundaries.sh
sh scripts/test-module-boundaries.sh
```

The Herald import checks distinguish leaf owners, owner-only construction modules,
neutral query vocabulary, and reviewed use-case coordinators. In particular, the
request internals and Publication/Wait owners cannot become casual sibling
dependencies. The standalone `PeerStream.State` leaf is pinned to its own
vocabulary, Herald-epoch identity, `bytestring`/`containers`, and the generic
prepared-transition helper; it cannot specialize itself by importing Publication,
the composed effect/input transition, or runtime code. Peer owner modules remain
private; only the reviewed `Eclips.Herald.Peer.RPC` adapter joins the public
re-export list.
The `Eclips.Herald.Join` and `Eclips.Oracle.Admission` facades expose checked
commands and retained evidence. History, replay, recipe, and activation composers
have exact private owner import lists; their negative probes reject concurrency
and clocks in the kernels and private admission-owner imports in deployment.
Discovery framing remains in its reviewed private TCP shell.
The paired control/carrier base coordinator has exact access to current owner
admission, internal system views, and passive historical alignment evidence.
It preserves receiver-local identity and transport state; application deltas and
application alignment do not start before activation. Its Oracle-advance access
supplies current routing and reconciliation views. Only the Join coordinator may
call it to capture a frozen source bundle; live receivers still replay the nested
History. The dependency-light SourceBundle module owns canonical framing and
copies explicit component bytes without admitting a snapshot or accessing owner
state. Negative fixtures keep the local ID allocator outside the composition and
Registry ownership outside the envelope codec.
Deployment's history-transfer helper is pinned as a hidden library module and
compiled directly by its tests. The production import scan includes its internal
source root; negative probes reject private Herald-owner imports there and moving
the helper into deployment's public surface.
The pure Oracle core has an exact Domain/Raft vocabulary wall: ordinary Domain
payload modules and Raft state/transition modules cannot enter it, and only
the reviewed genesis, voter-configuration, and committed-order owners may import
their exact public Raft vocabulary. SHA-256 remains
confined to the reviewed private digest, label, and admission owners, while cereal imports remain
confined to the reviewed canonical and normalized-transcript owners. The
private parameterized canonical-state seam
accepts only already-canonical transcript bytes and delegates hashing to that
digest owner; it adds no codec or public state surface.
The Raft protocol may import only the public Raft configuration/identity/input adapters, and
only its DTO/codec owners may import their exact Binary modules while its framing
owner alone may import the common framer. The Oracle protocol similarly confines
semantic and canonical-byte adapters to Types/Codec, common framing to Frame, and
request-absence authority to its Types/Frame owners. The family-neutral framer
and all four current protocol packages are pinned to pure wire
vocabulary: they cannot import Herald/runtime owners, networking, concurrency, or
effects. The Oracle/Raft runtime then pins production entropy to its private
election-timeout source and direct socket I/O to a private `socket-internal`
sublibrary with an exact consumer list. Its one readiness-watchdog timeout is
confined to that socket owner; other sockets remain in the reviewed TCP,
connected-wait, watch, and retirement workers. The pure status-projection helper
is confined to the two serialized owners, and the shutdown-only retirement seam
to its three stream owners. Each wire-protocol family remains with its
family-specific TCP owner, and each pure transition import remains with its
serialized owner plus the shared fault vocabulary. TCP workers cannot reach raw
kernel state, and deterministic runtime tests cannot acquire the production
entropy dependency. The application client remains pure, while
`application-api` has an exact private implementation-module wall (including its
dedicated socket helper) and cannot import Domain or Herald identities. Within Herald,
only the application and peer RPC adapter pairs may import their corresponding
protocol DTO vocabulary, and only their internal adapters may reconstruct checked
owner values. The shared historical authority resolver may compose only the
Controlled, Graph-progress, and Oracle-projection owners, and is itself confined
to the reviewed operation, invariant, application, Oracle-advance, and peer-input
consumers. The same checks keep owner allocation and advancement functions out of
those adapters. Compile-fail fixtures separately prove that downstream code cannot
import private implementation modules, confuse a structural session claim with a
Herald binding, place a global identity or `DeleteReserved` value in the current
application DTO, inspect exact runtime state or trace, forge a physical connection
reference, import application/TCP implementation modules, or coerce one
physical-plane reference into another.
The second command runs end-to-end negative fixtures, including multiline
source-root injection, dependencies inherited through common stanzas, and attempts
to bypass those reviewed seams. It also proves that missing, extra, reordered, or
duplicated root project entries fail, and that the auxiliary example cannot rename
itself, widen its dependencies, or import a private Herald owner.

The deployment package has an exact public-only composition wall and the checker
pins the ordinary launcher, publisher, and reader components. Their source roots
may import only the typed direct/advanced facade, descriptors, and application
types. Negative fixtures reject low-level runtime or Domain imports in these
applications and private Herald-owner imports in deployment. The remote startup
transfer decoder is a named canonical owner; its cross-owner admission reads
remain individually reviewed.

The checker pins the Oracle voter/configuration facade and its private canonical owner.
EADM carries its canonical public result values only in the reviewed DTO module;
framing cannot interpret voter semantics. Herald voter consumers and local
registration notification may import the exact public Oracle and replica identity
vocabulary at named source sites. They cannot reach Raft state or transition
kernels, Oracle runtime owners, or private Oracle state. Negative fixtures cover
those distinctions, including the deployment registration-discovery callback.

The [membership ledger](../../docs/verification/membership-ledger.md) explains
the stable source IDs and token-count obligations. The
[verification guide](../../docs/verification/README.md) places these source and
negative-fixture checks alongside compiled type boundaries and runtime tests.
