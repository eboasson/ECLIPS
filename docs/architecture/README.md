# Architecture and design

These documents explain the maintained implementation and its design constraints.
The [networked specification](../networked/README.md) owns the normative semantics
and protocols; these guides connect those contracts to packages, owners and
runtime behavior. Verification obligations are separate from source-specific
results: see [verification](../verification/README.md).

| Guide | Subject |
| --- | --- |
| [Architecture and deployment](architecture.md) | Shared contracts, identity owners, executable composition and failure behavior |
| [Herald kernel](herald-kernel.md) | Pure owner composition, retained work and runtime interpretation |
| [Application startup](application-startup.md) | Primordial selections, prepared children, possession grants and exact attachment |
| [Typed application API](typed-application-api.md) | Application sorts, nominal handles, codec laws and retained calls |
| [Connected environments](environment-hub.md) | Bootstrap and `newenv` construction, routing and lifetime |
| [Herald membership](herald-membership.md) | Discovery, finite joining cuts, repeated retirement and fencing |
| [Oracle and Raft](oracle-raft.md) | Learners, joint consensus, voter administration and failed-host exclusion |
| [Labels](labels.md) | Full-pair comparison, atomic decision, local installation and completion |
| [Control history and joining bases](control-history.md) | Receipt lifetime, checked replay bases, coherent source selection and reclamation limits |
| [Alignment](alignment.md) | Current plans, immutable generation birth facts and generation carry |
| [Takeover timing](takeover-timing.md) | Deployment policy, derived durations and performance limits |
| [Type-boundary checker](type-boundary-checker.md) | Compiled public-surface audit and positive/negative client probes |

The prototype uses one current source revision for all participants. It provides
no API or protocol compatibility layer, durable restart, Byzantine defenses,
sort-selective edges, or complete bound on retained history. Existing in-memory
checkpoints and selective reclamation do not imply those stronger capabilities.
