# Semantic type-boundary checker

Select the local compiler using [BUILDING.md](../../BUILDING.md), then run
the acceptance entry point from the repository root:

```sh
sh scripts/check-type-boundaries.sh
```

It builds the root project once, runs the rejected-client and accepted-client
modules in one GHC API session, compares the compiled public exports with
`api-surface.txt`, and leaves only the two private-sublibrary dependency checks to
Cabal.

To generate a candidate after an intentional public API change, run:

```sh
sh scripts/check-type-boundaries.sh \
  --write-surface "$PWD/.cabal/tmp/api-surface.candidate"
```

Review the sorted additions and removals, then replace `api-surface.txt` in the
same change as the intentional API update. Normal acceptance never rewrites the
manifest.

The implementation is pinned to the GHC 9.14 API in
`Eclips.TypeBoundaries.Ghc914`; a compiler upgrade should add or update that
adapter and regenerate the surface only after reviewing semantic differences.

The [checker design](../../docs/architecture/type-boundary-checker.md) describes
the semantic probes and API surface policy. The
[verification guide](../../docs/verification/README.md) describes the complete
aggregate and the evidence these compiled checks provide.
