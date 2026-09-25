#!/usr/bin/env python3
"""Check repository-local Markdown links and heading anchors with no dependencies.

Supports inline/reference links, ATX headings and named HTML anchors. Fenced and
indented code examples are ignored. This is a repository check, not a full
Markdown renderer.
"""

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path
from urllib.parse import unquote, urlsplit


PRIVATE = ("docs/development-history/", "private-notes/")
LINK = re.compile(r"!?\[[^\]\n]*\]\(\s*(<[^>\n]+>|[^\s)]+)(?:\s+[^)]+)?\)")
REFERENCE = re.compile(r"^\s{0,3}\[(?!\^)[^\]]+\]:\s*(<[^>\n]+>|\S+)", re.MULTILINE)


def prose(text):
    """Leave line positions intact while excluding fenced/indented code examples."""
    result = []
    fence = None
    for line in text.splitlines():
        marker = re.match(r"^\s{0,3}(`{3,}|~{3,})", line)
        if marker and fence is None:
            token = marker.group(1)
            fence = token
            result.append("")
        elif (marker and marker.group(1)[0] == fence[0]
              and len(marker.group(1)) >= len(fence)
              and not line[marker.end():].strip()):
            fence = None
            result.append("")
        elif fence is not None or line.startswith(("    ", "\t")):
            result.append("")
        else:
            result.append(line)
    return "\n".join(result)


def anchors(text):
    text = prose(text)
    found = set(re.findall(r'<(?:a|h[1-6])\b[^>]*\b(?:id|name)\s*=\s*[\"\']([^\"\']+)', text, re.IGNORECASE))
    headings = set()
    for line in text.splitlines():
        heading = re.match(r"^\s{0,3}#{1,6}\s+(.+?)\s*#*\s*$", line)
        if not heading:
            continue
        title = re.sub(r"\[([^\]]+)\]\([^)]*\)", r"\1", heading.group(1))
        title = re.sub(r"<[^>]*>", "", title).lower()
        slug = "".join(c for c in title if c.isalnum() or c in " _-").replace(" ", "-")
        unique = slug
        suffix = 0
        while unique in headings:
            suffix += 1
            unique = f"{slug}-{suffix}"
        headings.add(unique)
        found.add(unique)
    return found


def check(root, files):
    errors = []
    cache = {}
    for path in files:
        text = prose(path.read_text(encoding="utf-8"))
        for match in sorted([*LINK.finditer(text), *REFERENCE.finditer(text)], key=lambda m: m.start()):
            target = match.group(1).strip("<>")
            url = urlsplit(target)
            if url.scheme or url.netloc:
                continue
            local = unquote(url.path)
            destination = (root / local.lstrip("/") if local.startswith("/") else path.parent / local).resolve() if local else path
            problem = None
            if not destination.is_relative_to(root):
                problem = "link leaves the repository"
            elif not destination.exists():
                problem = "missing target"
            elif url.fragment and destination.suffix.lower() == ".md":
                if destination not in cache:
                    cache[destination] = anchors(destination.read_text(encoding="utf-8"))
                if unquote(url.fragment) not in cache[destination]:
                    problem = "missing heading anchor"
            if problem:
                line = text.count("\n", 0, match.start()) + 1
                errors.append(f"{path.relative_to(root)}:{line}: {problem}: {target}")
    return errors


def maintained_files(root):
    if (root / ".git").exists():
        listing = subprocess.check_output(
            ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard", "--", "*.md"], cwd=root)
        paths = {root / os.fsdecode(name) for name in listing.split(b"\0")
                 if name and not os.fsdecode(name).startswith(PRIVATE)}
        return sorted(path for path in paths if path.is_file())

    # Source archives have no index. Prune before walking so ignored toolchains,
    # package stores and experimental builds are never enumerated.
    paths = []
    for directory, subdirs, files in os.walk(root):
        current = Path(directory)
        subdirs[:] = [name for name in subdirs
                     if name not in {".git", ".cabal", ".stack-work", ".cabal-sandbox", "dist"}
                     and not name.startswith("dist-newstyle")
                     and not ((current / name).relative_to(root).as_posix() + "/").startswith(PRIVATE)
                     and not (current == root and (name in {"build", "tmp"} or name.startswith("build-")))]
        paths.extend(current / name for name in files if name.endswith(".md"))
    return sorted(paths)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", help="Markdown files; default: maintained files (Git index or extracted source tree)")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    if args.paths:
        paths = [Path(p).resolve() for p in args.paths]
    else:
        paths = maintained_files(root)
    errors = check(root, paths)
    for error in errors:
        print(error, file=sys.stderr)
    if errors:
        return 1
    print(f"Documentation links and anchors passed ({len(paths)} Markdown files).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
