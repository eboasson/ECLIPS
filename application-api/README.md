# eclips-application-api

The normal application interface is `Eclips.Application.Typed`. It binds an
application payload type to a complete ECLIPS sort and an endpoint's originating
session:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, TypeApplications #-}
import Data.Text (Text)
import Eclips.Application.Typed
import GHC.Generics (Generic)
import Prelude hiding (read)

data Message = Message { message :: Text }
  deriving (Eq, Show, Generic)
instance ValueType Message
instance ApplicationSort Message where
  sortPolicy = regularPolicy [key (field @"message")]

-- Within withHerald, bind a server-granted endpoint once:
-- writer <- either (fail . show) pure (bindNabla @Message herald "messages.writer")
-- write writer (Message "Hello, world!")
-- reader <- either (fail . show) pure (bindDelta @Message herald "messages.reader")
-- read (query reader (queryEqual (field @"message") "Hello, world!"))
```

`Nabla a`, `Delta a`, `Query a`, `Sort a`, `Reservation a`, `Creation a` and `Object a` are opaque
and nominal in their payload. Binding compares the full canonical sort identity,
including key, rank and lifecycle policy, against the Herald's verified startup
metadata. The predefined topology sorts are immutable, so ordinary publication
admission prevents changing an endpoint definition after binding. A typed handle
does not keep its endpoint alive or preserve possession after a label transfer.

`startupEnvironment` and `newEnvironment` return checked, session-owned topology
interfaces containing six structural endpoint pairs, a neutral hub and eighteen
preserving edges. `environmentHub` exposes an ordinary `Vertex` for connecting
other graphs; `environmentEdges` exposes the supporting `Edge` handles in
predefined-role order, with writer-to-hub, hub-to-reader and reader-to-hub for each
role. These handles use the same label, deletion and forwarding operations as
application-created objects. Startup binding checks the Herald-verified object
sorts as well as endpoint carried sorts. `environmentSelection` selects all
thirty-one objects, with just the twelve endpoints required for readiness.

`declareSort`, `createNabla`, `createDelta`, `createVertex` and
`createEdge` lower to existing operations. Newly published endpoints get typed
handles only after successful publication. `discoverNablas`/`discoverDeltas` bind
only evidence obtained by the library's own carrier read. There is no conversion
from an arbitrary private ID plus a claimed sort ID. Built-in carrier definitions
and their hashes come from the same shared catalogue as the Herald.

Queries return application values directly. `queryMany` checks connection
ownership once; heterogeneous `wait` takes a nonempty collection of `SomeQuery`
and checks ownership before submission. `readObjects` returns controlled object
references with decoded samples, and controlled reservations support publication,
cancellation, forwarding, updates and explicit full-label CAS. Private IDs stored
inside application values retain the existing dynamic process-namespace rules.

`Eclips.Application.Typed.Advanced` provides typed `Operation`, `Call`, `submit`,
`await`, `status` and `cancel`. Its wrappers retain the original runtime call and
completion cell. Payload decoding failure is `InvalidValue`; startup mismatch is
`EndpointSortMismatch`; underlying protocol/semantic outcomes are `UnderlyingCall`.
Cancellation and unknown mutation outcomes retain the existing rules below.
For staged topology work, `reserveNabla`, `reserveDelta`, `reserveVertex` and
`reserveEdgeFor` return a `Creation result` after reserving one identity. The
creation retains its session, original writer, immutable carrier and future typed
result. `publishCreation` publishes that carrier; `cancelCreation` cancels the
original reservation. All these steps have corresponding advanced operations:

```haskell
-- import qualified Eclips.Application.Typed.Advanced as Advanced
reservationCall <- Advanced.submit (Advanced.ReserveDelta environment messageSort)
creation <- Advanced.await reservationCall >>= either (fail . show) pure
publicationCall <- Advanced.submit (Advanced.PublishCreation creation)
reader <- Advanced.await publicationCall >>= either (fail . show) pure
```

Retain each call before awaiting it when local cancellation is possible. A
definitely rejected publication leaves its `Creation` available for cancellation.
An interrupted or unknown publication may already have succeeded: inspect its
original call instead of allocating a new object or automatically cancelling the
reservation. The `createNabla`/`createDelta`/`createVertex`/`createEdge` conveniences
combine reserve and publish, and do not expose intermediate reservations; use the
staged interface when the application needs to recover or abandon those steps.

Both [hello-world](../examples/hello-world/README.md) and
[hello-context](../examples/hello-context/README.md) use this interface. The
[design contract](../docs/architecture/typed-application-api.md) records its scope
and invariants. `Eclips.Application` and `Eclips.Application.Advanced` retain the
dynamic value-tree interface for low-level applications and boundary fixtures.

This package interprets the pure `eclips-application-client` owner over one
scoped TCP connection. It exposes `newid`, `write`, `forward`, `read`,
`localTake`, `wait`, `label`, and `newenv` for the current internal prototype.
The current write payload admits generic application-value publication or
reservation deletion. Structural write and forward calls remain unresolved at
this facade while their retained protocol request is awaiting a covering
established topology cut; label calls likewise remain pending while their
all-Herald settlement workflow is incomplete. `newenv` creates one complete
private environment and returns its opaque `EnvironmentAccess` only after all
thirty-one structural objects and their internal connections are established.
Its first twelve endpoint definitions use the caller's retained two source
writers; the hub and edges use the new environment's own structural writers.
Bootstrap exposes the same complete environment shape.

`Eclips.Application` exposes an opaque `Herald` session handle. `connect` accepts
one checked connection descriptor; `startup` returns its selected private access.
`disconnect` is idempotent, sends orderly session closure, and joins the transport
workers. `withHerald` brackets the same scope, including callback exceptions.
Each of the eight functions returns `IO (Either CallError result)` with its own
result type. Applications never decode the heterogeneous wire result union.
Calls after closure return `CallClosed`.

```haskell
result <- withHerald descriptor $ \herald -> do
  environment <- newenv herald
  pure environment
```

`Eclips.Application.Advanced` provides the same operations as an `Operation a`
GADT and opaque `Call a`: `submit`, `await`, `status`, and `cancel` support
independent concurrent invocations. A cancelled wait retains the protocol's
wake/cancel winner. Cancelling a thread awaiting an accepted mutation stops that
thread promptly; its exact call remains owned and can still complete. Retain the
advanced call to inspect that outcome. Cleanup does not wait for Oracle quorum,
and no cancelled mutation is retried under a fresh request identity.

Lifecycle requests use a separate typed `LifecycleOperation a` and
`LifecycleCall a`. The direct facade offers `prepareChild`, `awaitChildReady`,
`cancelChild`, and `endProcess`; preparation returns before readiness so the
parent can transfer labels to the child. Advanced `BeginChild` returns the
preparation reference early. Cancelling a local lifecycle wait does not cancel
the child or accepted lifecycle work. `lifecycleRequestId` retains the complete
session-qualified correlation. `recoverEndProcess` uses the original `Herald`
handle after End or disconnect, querying only its attachment and that correlation
without Open, Resume, or a second End request. This is a best-effort query of a
still-retained receipt; terminal session cleanup removes it after constructing
the final reply effect, so lost delivery can remain unknown.

`Eclips.Application.Connection` prints and parses canonical portable descriptor
text. `connect` creates one fresh initial-claim identity and preserves it across
initial transport retries. Advanced `connectWith` accepts an explicit stable
claim identity and checked liveness policy. Both founder and prepared-child
connections return only after readiness, initial claim, and receipt confirmation
on the same TCP connection. The application can remain idle after confirmation.
Loss of that established connection ends its lifetime; applications recover by
restarting. Session closure remains distinct from explicit process End.

Ordinary `connect` derives client timing from the checked takeover target carried
in its connection descriptor. With the default five-second target this means a
10 ms transport retry delay, two-second session recovery grace, 250 ms heartbeat
idle interval and 500 ms heartbeat reply allowance. A deployment's nondefault
target therefore reaches founder and prepared-child applications through their
startup material. `applicationLivenessFromTakeoverTarget` exposes the same pure
derivation to explicit runtime callers; advanced `connectWith` continues to use
its supplied checked policy.

`Eclips.Application.Runtime` is the explicit lower runtime interface for transport
fixtures and callers that need injected endpoint/claim/liveness policy. Ordinary
application examples use the direct or typed advanced facade.

The public liveness policy checks four positive monotonic-microsecond durations:
transport retry pacing, established-session recovery grace, heartbeat idle time,
and heartbeat reply timeout. Initial attachment retries are paced but have no
terminal deadline. Once a session has opened, EOF, write failure, or a missed
heartbeat ends its lifetime immediately. The retained recovery-grace setting
does not postpone this terminal transition. Pending own-End work may use the
restricted result query described above; ordinary work never Resumes a lost
session.
A complete server disposition atomically stops the physical handshake timeout
before entering the pure-owner queue, so local admission delay cannot discard a
reply that reached the binding first. The heartbeat reply interval likewise begins
only after the serialized writer confirms that its Ping's progress-accounted
physical send completed; a Ping waiting behind semantic frames cannot consume
its own reply budget.
The client suppresses an idle Ping only when both inbound bytes and completed
outbound frames have progressed during that interval. Receiving a stream of
server replies must still produce client traffic for the Herald's passive
heartbeat, while sending calls alone cannot establish server responsiveness.
Inbound progress continues to satisfy an outstanding Ping's reply wait.

On POSIX, each physical transfer first uses the socket's nonblocking system call.
Only `EAGAIN`/`EWOULDBLOCK` installs an IO-manager readiness registration, and a
one-second watchdog cancels and refreshes that registration before retrying the
system call. The watchdog never interrupts a transfer: partial writes advance the
exact retained suffix while asynchronous exceptions are masked across that
accounting step. This is a pragmatic guard against a rare lost readiness wake
observed under the macOS/GHC test load. The Windows backend currently retains
`network`'s ordinary blocking operations and does not claim this watchdog
guarantee.

One physical writer serializes calls, lifecycle requests, initial claims,
Open, receipt acknowledgements, End, and heartbeat Ping frames.
Pong and ordinary inbound-byte progress are consumed by the physical connection
owner, so routine heartbeat traffic does not enter the pure session owner. Every
dial, timer, reader, writer, and heartbeat child is canceled or joined when the
scope drains. Physical closure wakes those children immediately while descriptor
closure remains retryable if a cleanup lane is interrupted. Failure to allocate a
required host delay terminates the scoped runtime rather than being reported as
Herald unavailability. The package does not yet infer server-side
application-process End, add compatibility versions, or change the closed eight-operation vocabulary.

Completed ordinary and lifecycle result cells are removed from the runtime's
routing maps immediately after being filled. Returned handles own those cells,
so `await` and status remain repeatable without retaining every completed call in
the connection owner. Fresh calls piggyback the latest receipt high-water marks and unresolved exceptions;
a coalesced idle flush acknowledges the final results when traffic stops.
