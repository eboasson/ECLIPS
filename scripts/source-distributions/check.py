#!/usr/bin/env python3
"""Step-16 source archive certification; Cabal supplies the source inventory."""

import argparse
import io
import json
import os
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import sys
import tarfile
import tempfile
import unittest


FORBIDDEN_PARTS = {
    ".git", ".cabal", ".cache", ".stack-work", "dist", "dist-newstyle",
    "build", "cache", "tmp", "temp", "__pycache__", "node_modules",
    ".pytest_cache", ".mypy_cache", ".DS_Store",
}
FORBIDDEN_SUFFIXES = (".o", ".hi", ".dyn_o", ".dyn_hi", ".pyc", ".tmp", ".swp", ".bak", "~")


def require(condition, message):
    if not condition:
        raise ValueError(message)


def relative_source_path(name):
    path = PurePosixPath(name)
    require(name and not path.is_absolute() and ".." not in path.parts,
            f"archive path is not package-relative: {name!r}")
    require(str(path) == name, f"noncanonical archive path: {name!r}")
    require(not (set(path.parts) & FORBIDDEN_PARTS),
            f"build/cache/temp tree in source distribution: {name}")
    require(not name.endswith(FORBIDDEN_SUFFIXES),
            f"build/cache/temp file in source distribution: {name}")
    return path


def exact_archives(directory, packages):
    expected = {package["id"] + ".tar.gz" for package in packages}
    require(len(expected) == len(packages), "root project contains duplicate package identities")
    actual = {path.name for path in directory.iterdir()}
    require(actual == expected,
            f"archive package set differs from cabal.project; missing={sorted(expected - actual)}, extra={sorted(actual - expected)}")


def audit_archive(archive, package, project_root):
    """Check every Cabal-enumerated file and its exact bytes before extraction."""
    expected = set(package["files"])
    require(len(expected) == len(package["files"]), "duplicate source inventory entry")
    package_file = Path(package["package_file"]).name
    require(package_file in expected, f"missing package description in inventory: {package_file}")
    allowed_directories = {PurePosixPath(".")}
    for name in expected:
        allowed_directories.update(relative_source_path(name).parents)
    source_root = (project_root / package["source_root"]).resolve()
    require(source_root.is_relative_to(project_root.resolve()), "source package escapes project")
    files = {}
    seen = set()
    with tarfile.open(archive, "r:gz") as stream:
        for member in stream:
            name = member.name.rstrip("/") if member.isdir() else member.name
            path = relative_source_path(name)
            require(path.parts[0] == package["id"],
                    f"wrong package root in {archive.name}: {name}")
            require(name not in seen, f"duplicate archive member: {name}")
            seen.add(name)
            relative = PurePosixPath(*path.parts[1:])
            if member.isdir():
                require(relative in allowed_directories, f"undeclared archive directory: {name}")
                continue
            require(member.isfile(), f"source archive contains a link or special file: {name}")
            require(str(relative) in expected, f"undeclared source archive file: {name}")
            source = source_root / str(relative)
            require(source.resolve().is_relative_to(source_root), f"source file escapes package: {source}")
            extracted = stream.extractfile(member)
            require(extracted is not None, f"unreadable source archive member: {name}")
            data = extracted.read()
            require(data == source.read_bytes(), f"archive bytes differ from current source: {name}")
            files[str(relative)] = (data, member.mode & 0o777)
    require(set(files) == expected,
            f"{archive.name}: missing declared sources/docs/licence: {sorted(expected - set(files))}")
    return files


def sync_package(destination, files):
    """Refresh only from audited archive bytes, retaining unchanged build inputs."""
    require(not destination.is_symlink(), f"isolated package directory is a symlink: {destination}")
    destination.mkdir(parents=True, exist_ok=True)
    for path in sorted(destination.rglob("*"), key=lambda item: len(item.parts), reverse=True):
        relative = path.relative_to(destination).as_posix()
        if path.is_symlink() or (path.is_file() and relative not in files):
            path.unlink()
        elif path.is_dir() and not any(path.iterdir()):
            path.rmdir()
    for name, (data, mode) in files.items():
        target = destination / name
        target.parent.mkdir(parents=True, exist_ok=True)
        if not target.is_file() or target.read_bytes() != data:
            target.write_bytes(data)
        target.chmod(mode)


def write_if_changed(path, data):
    if not path.exists() or path.read_bytes() != data:
        path.write_bytes(data)


def run(command, root, env):
    print("+ " + " ".join(map(str, command)), flush=True)
    subprocess.run(command, cwd=root, env=env, check=True)


def certify(root, work, no_build):
    env = dict(os.environ, TMPDIR=str(work / "tmp"), GHC_ENVIRONMENT="-")
    Path(env["TMPDIR"]).mkdir(parents=True, exist_ok=True)
    manifest_command = ["runghc", "-XGHC2024", "-Wall", "-Werror", "-package=Cabal",
                        str(root / "scripts/source-distributions/Manifest.hs"), str(root)]
    manifest = subprocess.check_output(manifest_command, cwd=root, env=env)
    packages = json.loads(manifest)
    write_if_changed(work / "manifest.json", manifest)
    config = "--config-file=" + str(root / ".cabal/config")
    # `cabal check` rejects --project-dir. As in the Step-15 gate, only this
    # command uses each package directory; its configuration is still the root's.
    for package in packages:
        run(["cabal", config, "check"], root / package["source_root"], env)
    with tempfile.TemporaryDirectory(prefix="archives-", dir=work) as temporary:
        archives = Path(temporary)
        run(["cabal", config, "--builddir=" + str(work / "sdist-build"), "sdist", "all",
             "--output-directory=" + str(archives)], root, env)
        exact_archives(archives, packages)
        audited = [(package, audit_archive(archives / (package["id"] + ".tar.gz"), package, root))
                   for package in packages]
        package_roots = work / "packages"
        package_roots.mkdir(exist_ok=True)
        expected_roots = {package["id"] for package in packages}
        for path in package_roots.iterdir():
            if path.name not in expected_roots:
                if path.is_dir() and not path.is_symlink():
                    shutil.rmtree(path)
                else:
                    path.unlink()
        retained_archives = work / "archives"
        retained_archives.mkdir(exist_ok=True)
        for path in retained_archives.iterdir():
            path.unlink()
        for package, files in audited:
            sync_package(package_roots / package["id"], files)
            shutil.copyfile(archives / (package["id"] + ".tar.gz"),
                            retained_archives / (package["id"] + ".tar.gz"))
    project = work / "cabal.project"
    project_text = "packages:\n" + "".join(
        "    ./packages/" + package["id"] + "/" + Path(package["package_file"]).name + "\n"
        for package in packages)
    project_text += "tests: True\nwrite-ghc-environment-files: never\n"
    write_if_changed(project, project_text.encode())
    count = sum(len(package["files"]) for package in packages)
    print(f"Audited exactly {len(packages)} root packages and {count} declared source/doc/licence files.", flush=True)
    if no_build:
        print("Archive audit complete; isolated build explicitly skipped (--no-build).", flush=True)
    else:
        run(["cabal", config, "--project-file=" + str(project), "--builddir=" + str(work / "build"),
             "build", "all", "-j4", "--offline", "--ghc-options=-Werror"], root, env)
        print("Source distributions passed metadata, exact archive audit and isolated build.", flush=True)
    print(f"Retained source-distribution evidence: {work}", flush=True)


class ArchiveAuditProperties(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="audit-test-", dir=TEST_WORK)
        self.root = Path(self.temporary.name)
        self.source = self.root / "fixture"
        self.source.mkdir()
        self.package = {"id": "fixture-0.1", "package_file": "fixture/fixture.cabal",
                        "source_root": "fixture",
                        "files": ["fixture.cabal", "LICENSE", "README.md", "src/Library.hs", "test/Main.hs"]}
        self.contents = {name: name.encode() for name in self.package["files"]}
        for name, data in self.contents.items():
            path = self.source / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(data)
        self.archive = self.root / "fixture-0.1.tar.gz"

    def tearDown(self):
        self.temporary.cleanup()

    def create_archive(self, entries):
        with tarfile.open(self.archive, "w:gz") as stream:
            for name, data in entries:
                member = tarfile.TarInfo(name)
                member.size = len(data)
                member.mode = 0o644
                stream.addfile(member, io.BytesIO(data))

    def entries(self):
        return [("fixture-0.1/" + name, data) for name, data in self.contents.items()]

    def test_exact_inventory_and_byte_preserving_refresh(self):
        self.create_archive(self.entries())
        files = audit_archive(self.archive, self.package, self.root)
        isolated = self.root / "isolated"
        sync_package(isolated, files)
        times = {name: (isolated / name).stat().st_mtime_ns for name in files}
        (isolated / "stale.hs").write_text("not in the archive")
        sync_package(isolated, files)
        self.assertFalse((isolated / "stale.hs").exists())
        self.assertEqual(times, {name: (isolated / name).stat().st_mtime_ns for name in files})

    def test_every_declared_file_is_required(self):
        for missing in self.package["files"]:
            with self.subTest(missing=missing):
                self.create_archive([entry for entry in self.entries() if entry[0] != "fixture-0.1/" + missing])
                with self.assertRaisesRegex(ValueError, "missing declared"):
                    audit_archive(self.archive, self.package, self.root)

    def test_every_declared_file_preserves_its_bytes(self):
        for changed in self.package["files"]:
            with self.subTest(changed=changed):
                self.create_archive([(name, data + b"changed" if name.endswith("/" + changed) else data)
                                     for name, data in self.entries()])
                with self.assertRaisesRegex(ValueError, "bytes differ"):
                    audit_archive(self.archive, self.package, self.root)

    def test_build_cache_and_temporary_paths_are_rejected(self):
        names = ["dist-newstyle/cache/plan.json", ".cabal/config", "dist/setup-config",
                 "build/Module", ".git/config", ".cache/result", "tmp/scratch",
                 "temp/scratch", "__pycache__/script.pyc", "src/Module.o",
                 "src/Module.hi", "src/Module.dyn_o", "src/Module.dyn_hi",
                 "src/Module.hs.tmp", "src/Module.hs~"]
        for name in names:
            with self.subTest(name=name):
                self.create_archive(self.entries() + [("fixture-0.1/" + name, b"junk")])
                with self.assertRaisesRegex(ValueError, "build/cache/temp"):
                    audit_archive(self.archive, self.package, self.root)

    def test_duplicate_undeclared_and_wrong_root_members_are_rejected(self):
        for extra in [self.entries()[0], ("fixture-0.1/extra.hs", b"extra"), ("other/README.md", b"other")]:
            with self.subTest(extra=extra[0]):
                self.create_archive(self.entries() + [extra])
                with self.assertRaises(ValueError):
                    audit_archive(self.archive, self.package, self.root)

    def test_archive_set_matches_root_package_set(self):
        self.create_archive(self.entries())
        archive_dir = self.root / "archives"
        archive_dir.mkdir()
        with self.assertRaisesRegex(ValueError, "package set differs"):
            exact_archives(archive_dir, [self.package])
        shutil.copyfile(self.archive, archive_dir / self.archive.name)
        exact_archives(archive_dir, [self.package])
        (archive_dir / "unlisted.tar.gz").write_bytes(b"extra")
        with self.assertRaisesRegex(ValueError, "package set differs"):
            exact_archives(archive_dir, [self.package])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--no-build", action="store_true", help="audit packages and archives without the isolated build")
    parser.add_argument("--self-test", action="store_true", help="run focused archive-audit properties only")
    arguments = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    work = root / ".cabal/step16-source-distributions"
    work.mkdir(parents=True, exist_ok=True)
    global TEST_WORK
    TEST_WORK = work
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(ArchiveAuditProperties)
    if not unittest.TextTestRunner(verbosity=1).run(suite).wasSuccessful():
        return 1
    if not arguments.self_test:
        certify(root, work, arguments.no_build)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (ValueError, OSError, subprocess.CalledProcessError, tarfile.TarError) as error:
        sys.exit("Source-distribution certification failed: " + str(error))
