# Maintaining public source snapshots

The maintained source, documentation and instructions use the same paths in the
development repository and in public snapshots. Update those files directly when
behavior changes. Describe the current contract and its rationale in
[`networked`](networked/README.md) and [`architecture`](architecture/README.md);
put test obligations in [`verification`](verification/README.md).

Keep experiment diaries, intermediate plans and raw investigation records under
`private-notes/`. Historical collections may live under `docs/development-history/`
on a separate private branch. Both paths have `export-ignore` attributes in
the root `.gitattributes`. These exclusions apply to `git archive`, not to pushes:
the development repository's existing history remains private.

## Prepare an export

Commit the intended source and documentation together. Run the relevant checks
from [BUILDING.md](../BUILDING.md), including the documentation checker:

```sh
python3 scripts/check-documentation.py
git status --short
mkdir -p .cabal/publication
git archive --format=tar --output=.cabal/publication/eclips.tar HEAD
tar -tf .cabal/publication/eclips.tar
```

The archive contains tracked files from the selected commit and honors that
commit's `.gitattributes`. Uncommitted edits and ignored compiler, cache and build
files are not exported. Inspect the member list before using the archive; add any
new private-only directory to `.gitattributes` before committing it. Keep a private
record of the source commit, public commit, export exclusions and verification
performed for each publication.

## First publication and later updates

For the first publication, extract the reviewed archive into an empty directory
and initialize a new Git repository there. Its initial commit has no development
ancestry. Preserve the license and authorship statements. Verify the extracted
tree using the documented setup; a working development checkout alone does not
demonstrate that a fresh checkout is usable.

For later updates, apply the new snapshot to a clean checkout of the public
repository, including removal of files absent from the new snapshot. Preserve the
public checkout's `.git` directory, inspect the complete diff, and make an ordinary
new commit. Do not merge development or archive branches into the public branch:
their ancestry would publish the private history. Update the public repository's
single intended branch explicitly rather than mirroring private refs.

Repository renaming, remote changes and publication are separate operations from
preparing these source snapshots. If the original repository name is reused,
update existing development clones to the renamed private remote first.
