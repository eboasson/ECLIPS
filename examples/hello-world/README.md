# ECLIPS application API tour

This example tours the application API, including readiness, acknowledgements,
local take, labels, and explicit process termination. The package lives in
`examples/hello-world` and is also used by verification and measurement workflows.
For the introductory hello-world example,
where a late reader retrieves a greeting after its publisher exits, see
[Hello from context](../hello-context/README.md).

This package provides ordinary independent launcher, publisher, and reader
applications, plus an explicit singleton founder integration tour. All application
work uses application records and checked endpoints from
`Eclips.Application.Typed`; deployment and runtime helpers remain outside the
ordinary `applications` library.

`HelloCommon` defines the message once, including its complete sort policy:

```haskell
data Message = Message {message :: Text}
  deriving stock (Eq, Show, Generic)

instance ValueType Message
instance ApplicationSort Message where
  sortPolicy = regularPolicy [key (field @"message")]
```

The publisher binds `Nabla Message` and the reader binds `Delta Message` from
their named startup endpoints. Binding checks the complete content-addressed sort
identity. Publications use `write writer (Message "Hello, world!")`, and queries
use `query reader (queryEqual (field @"message" @Message) "Hello, world!")`.
Application code constructs neither wire values nor structural carrier records.

The singleton tour starts exactly one Herald, one native Raft voter, and one
genesis launcher. Fresh entropy supplies the run and launcher identities before
checked admission. The launcher exercises all eight operations, including a fresh
private environment, a regular sort, dynamic endpoints and a preserving edge,
read and local take of `Hello, world!`, and an Oracle-backed label-to-void barrier.
It awaits explicit process End before disconnecting. The label decision,
collection and launcher End contribute three semantic Oracle entries; receipt
maintenance is counted separately. The runtime then semantically drains and joins
the Herald and Oracle scopes before printing the greeting.

```sh
cabal --config-file="$PWD/.cabal/config" run -j4 eclips-hello-world:exe:eclips-hello-world -- --bootstrap
```

The fixed-manifest split deployment tour supplies ordinary multi-Herald coverage
through real prepared children. Step14–16 Runtime fixtures provide the detailed
multi-Herald transport and settlement schedules.

The local integration suite checks generated founder configurations, the complete
founder workflow, and the retained prepared-child OS-process tour:

```sh
cabal --config-file="$PWD/.cabal/config" test -j4 eclips-hello-world:test:hello-world-integration
```

`HelloApplication.hs` contains the ordinary founder workflow.
`FounderConfiguration.hs`, `FounderDeployment.hs`, and `HelloDeployment.hs` own
its checked deployment and supervised runtime composition.

## Independent applications

The `applications` library and `eclips-hello-launcher`,
`eclips-hello-publisher`, and `eclips-hello-reader` executables depend only on the
ordinary application API/types and OS process support. They do not import Herald,
Oracle, or domain kernels. The deployment executable produces a portable opaque
launcher descriptor. Pass it using `--connection-file PATH` (or `--connection
TEXT`) and select publisher and reader Herald locators explicitly:

```sh
eclips-hello-launcher --connection-file launcher.connection \
  --publisher-herald 127.0.0.1:7101 --reader-herald 127.0.0.1:7201
```

Install the three application executables together so the launcher finds its
publisher and reader beside itself. The library's `ChildExecutables` argument
also supports explicit executable paths for integration drivers. The launcher CLI
accepts `--publisher-executable PATH --reader-executable PATH` for separately built
programs. Add
`--reader-first` to reverse the OS launch order. Both target locators may name
the same Herald.

The launcher creates separate environments and message endpoints, prepares both
children, labels all selected objects for them, and waits for readiness. Each
child receives only its own connection descriptor through OS arguments; startup
access arrives through EAPP. Readiness, greeting, acknowledgement, and the final
reader-exit command travel through the data space. The publisher completes its
explicit End before the launcher sends the reader-exit command, keeping the final
acknowledgement alive until observed. Both children and then the launcher end
their logical processes; subprocess workers are joined on success and exception.
The launcher retains each preparation handle as soon as admission completes.
If setup fails, it follows any pending admission using the original call and
requests cancellation for every accepted child before waiting for their End
results. An interrupted wait never starts a replacement preparation.

The [topology guide](../../docs/topology.md#give-a-child-an-environment-of-its-own)
explains how this launcher combines each fresh environment with message endpoints
created through its own startup environment. The guide also shows how to wire
structural discovery between environments when children should build their own
message paths. This API tour supplies those message endpoints directly at startup.

`Eclips.Application.Typed` returns application values and operation-specific
results directly. The reader also uses `Eclips.Application.Typed.Advanced` to
cancel and await the same submitted wait. The combined ordinary workflow
exercises all eight data-space operations without wire-value decoding. Raw
lifecycle preparation and selected-endpoint label transfer remain confined to
the launcher.
