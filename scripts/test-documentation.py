#!/usr/bin/env python3
"""Small Markdown-link regression fixtures, isolated inside the checkout."""

import importlib.util
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("documentation", ROOT / "scripts/check-documentation.py")
CHECKER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECKER)


class DocumentationTests(unittest.TestCase):
    def setUp(self):
        scratch = ROOT / ".cabal/public-preparation"
        scratch.mkdir(parents=True, exist_ok=True)
        self.temporary = tempfile.TemporaryDirectory(prefix="documentation-test-", dir=scratch)
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)

    def document(self, path, text):
        target = self.root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8")
        return target

    def test_missing_relative_path_reports_source_line(self):
        source = self.document("README.md", "# Intro\n[missing](docs/missing.md)\n")
        self.assertEqual(CHECKER.check(self.root, [source]),
                         ["README.md:2: missing target: docs/missing.md"])

    def test_relative_cross_file_heading_and_root_path(self):
        self.document("docs/target.md", "# Current `label` contract\n")
        source = self.document("docs/nested/source.md",
                               "[relative](../target.md#current-label-contract)\n"
                               "[root](/docs/target.md#current-label-contract)\n")
        self.assertEqual(CHECKER.check(self.root, [source]), [])

    def test_duplicate_heading_suffixes(self):
        text = "# Repeated\n# Repeated\n# Repeated-1\n# Repeated\n"
        self.assertEqual(CHECKER.anchors(text),
                         {"repeated", "repeated-1", "repeated-1-1", "repeated-2"})
        source = self.document("README.md", text + "[third repeat](#repeated-2)\n")
        self.assertEqual(CHECKER.check(self.root, [source]), [])

    def test_fenced_and_indented_examples_are_ignored(self):
        source = self.document("README.md", "# Visible\n```markdown\n"
                               "[not a link](missing.md)\n```not-a-closing-fence\n"
                               "[still code](also-missing.md)\n"
                               '<a name="hidden"></a>\n# Hidden heading\n```\n'
                               "    [indented](missing.md)\n"
                               "~~~markdown\n[tilde fence](missing.md)\n~~~\n")
        self.assertEqual(CHECKER.check(self.root, [source]), [])
        self.assertEqual(CHECKER.anchors(source.read_text()), {"visible"})

    def test_nonlocal_links_are_ignored(self):
        source = self.document("README.md", "[web](https://example.invalid/no.md#none)\n"
                               "[relative authority](//example.invalid/no.md)\n"
                               "[mail](mailto:example@example.invalid)\n"
                               "[app](app://example)\n")
        self.assertEqual(CHECKER.check(self.root, [source]), [])

    def test_url_escaped_path_and_fragment(self):
        self.document("docs/space name.md", "# Café guide\n")
        source = self.document("README.md", "[guide](docs/space%20name.md#caf%C3%A9-guide)\n")
        self.assertEqual(CHECKER.check(self.root, [source]), [])

    def test_named_anchor_and_self_fragment(self):
        source = self.document("README.md", '<a name="named-section"></a>\n'
                               '<h2 id = "manual-heading">Heading</h2>\n'
                               "# Intro\n[self](#intro)\n[named](#named-section)\n"
                               "[manual](#manual-heading)\n")
        self.assertEqual(CHECKER.check(self.root, [source]), [])

    def test_missing_heading_and_reference_target(self):
        source = self.document("README.md", "# Intro\n[bad anchor](#missing)\n"
                               "[reference]: missing.md\n")
        self.assertEqual(CHECKER.check(self.root, [source]),
                         ["README.md:2: missing heading anchor: #missing",
                          "README.md:3: missing target: missing.md"])

    def test_paper_footnotes_and_pdf_page_links(self):
        (self.root / "paper.pdf").touch()
        source = self.document("paper.md", "# Paper\nHistorical reference[^1].\n"
                               "[^1]: First description in an earlier publication.\n"
                               "    Continued footnote text.\n"
                               "> **Figure 1.** Caption. ([PDF p. 11](paper.pdf#page=11))\n")
        self.assertEqual(CHECKER.check(self.root, [source]), [])

    def test_standalone_tree_prunes_local_and_private_directories(self):
        self.document("README.md", "[guide](docs/guide.md#guide)\n")
        self.document("docs/guide.md", "# Guide\n")
        for directory in (".cabal", "package/.cabal", "dist-newstyle", "dist-newstyle-profile",
                          "build", "build-profile", "tmp", ".git", "private-notes",
                          "docs/development-history"):
            self.document(f"{directory}/nested/broken.md", "[broken](missing.md)\n")
        # An archive has no .git entry. All other excluded subtrees remain.
        shutil.rmtree(self.root / ".git")
        self.assertEqual([p.relative_to(self.root).as_posix()
                          for p in CHECKER.maintained_files(self.root)],
                         ["README.md", "docs/guide.md"])
        script = self.root / "scripts/check-documentation.py"
        script.parent.mkdir()
        shutil.copyfile(ROOT / "scripts/check-documentation.py", script)
        result = subprocess.run([sys.executable, "-B", str(script)], cwd=self.root,
                                text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertIn("passed (2 Markdown files)", result.stdout)


if __name__ == "__main__":
    unittest.main()
