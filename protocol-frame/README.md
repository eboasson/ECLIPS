# ECLIPS protocol framing

This package contains the pure family-neutral envelope shared by the current
ECLIPS application and Herald-peer protocols:

```text
uint32be frameLength
uint32be familyMagic
opaque familyBody
```

The declared length excludes its own four bytes and includes both the family
magic and body. The incremental decoder retains partial input, returns every
complete body before the first partial or failed frame, and makes framing
failure terminal. It imposes no semantic frame-size limit.

Family packages choose their magic, decode exactly one returned body, reject
payload residual and enforce their own direction or connection-phase rules.
This package has no application, Domain, Herald, runtime, networking,
concurrency, clock, entropy, or persistence dependency.
