# ECLIPS Raft protocol

This package owns the complete current-build Raft wire boundary: the checked
immutable genesis-digest and replica identity claims used by `RaftHello`, the six ERFT RPC DTO
constructors, their protocol-owned `Binary` encodings, validation into public
`eclips-raft-core` inputs, connection-role admission, and incremental ERFT
framing built on `eclips-protocol-frame`.

DTO decoding checks every self-contained current RPC invariant before core
adaptation, including canonical nonempty stable/joint voter sets, positive terms, sentinel index/term coherence, exact
AppendEntries contiguity and non-regressing term history, commit-prefix bounds,
and coherent accepted or rejected append results. Stale but structurally valid
terms and log conflicts remain semantic Raft inputs. Configuration entries retain
their opaque adapter metadata unchanged; the codec does not interpret it.

InstallSnapshot carries one checked native checkpoint index and term, its last
configuration reference, the stable or joint voter sets, and opaque application
checkpoint bytes. Its response names the exact requested index and whether the
application installation completed. Structural admission rejects zero checkpoint
positions/terms, configuration references beyond the captured prefix, and a
request term older than its checkpoint. The native/runtime boundary owns actual
installation and its acknowledgement before success is sent.

Hello equality binds the immutable run/genesis digest, which covers the system,
initial native bindings, and timer policy. Current voter configuration is not a
connection prerequisite. Registered learners, lagging nodes, and a leader finishing
its own removal can exchange history on the same exact source/target binding.
Transport admission never grants votes or changes native replication targets.

The package imports no ECLIPS Domain, Oracle, Herald, runtime, networking,
concurrency, clock, entropy, storage, or PoC module. Generic DTO bytes are not
canonical Oracle bytes. No `Binary` instance is defined for a Raft-core type.

The property suite also runs finite real-TCP schedules between separately
initialized native owners. Dropped joint replies cannot satisfy a new majority;
duplicate configuration traffic exposes metadata once; reordered old joint
traffic cannot replace a committed final suffix; and a new leader can replay
retained history or a checkpoint to a learner that still knows only immutable genesis. A
real-TCP checkpoint test installs an independent application image, then applies
only the new suffix without replaying discarded commands. The
fixture transport registry is separate from native target updates and votes.

The current wire schema is intentionally not a stable or version-negotiated
interface. Profile 0.1 changes every producer, consumer, codec, and test
together. This package is available under the MIT License; see this package's
`LICENSE` file.
