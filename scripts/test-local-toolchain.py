#!/usr/bin/env python3
"""Exercise setup refusal, relocation and heap forwarding without compiling.

Uses tiny stand-in executables and keeps all temporary files inside the checkout.
"""

from pathlib import Path
import shutil
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parent.parent


def run(*args, cwd, check=True):
    return subprocess.run(args, cwd=cwd, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT, check=check)


def main():
    scratch = ROOT / ".cabal/public-preparation"
    scratch.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="toolchain-test-", dir=scratch) as tmp:
        work = Path(tmp) / "original checkout"
        work.mkdir()
        (work / "scripts").mkdir()
        (work / "cabal.project").write_text("packages:\n")
        shutil.copyfile(ROOT / "scripts/configure-local-toolchain.py",
                        work / "scripts/configure-local-toolchain.py")
        compiler_bin = work / ".cabal/profile-0.2/p07/ghc-rc/install/bin"
        compiler_bin.mkdir(parents=True)
        stub = '''#!/bin/sh
case "$1" in
    --numeric-version) printf '%s\\n' 9.14.1.20260728 ;;
    --version) printf '%s\\n' 'GHC package manager version 9.14.1.20260728' ;;
    *) printf '%s\\n' "$@" ;;
esac
'''
        for name in ("ghc", "ghc-pkg", "haddock", "hsc2hs", "runghc"):
            tool = compiler_bin / name
            tool.write_text(stub)
            tool.chmod(0o755)
        cabal = Path(tmp) / "real-cabal"
        cabal.write_text('#!/bin/sh\nprintf "%s\\n" "$CABAL_DIR" "$@"\n')
        cabal.chmod(0o755)
        command = ("python3", "scripts/configure-local-toolchain.py", "--cabal", str(cabal))

        ghc = compiler_bin / "ghc"
        ghc.write_text(stub.replace("9.14.1.20260728", "9.14.1"))
        result = run(*command, cwd=work, check=False)
        assert result.returncode and "Expected ghc" in result.stdout, result.stdout
        assert not (work / ".cabal/config").exists()
        ghc.write_text(stub)
        run(*command, cwd=work)

        config = work / ".cabal/config"
        config.write_text(config.read_text() + "-- user customization\n")
        saved = config.read_bytes()
        result = run(*command, cwd=work, check=False)
        assert result.returncode and "refusing to overwrite" in result.stdout, result.stdout
        assert config.read_bytes() == saved

        moved = work.with_name("relocated checkout")
        work.rename(moved)
        prefix = '. "$PWD/.cabal/profile-0.2/f01/env.sh"\n'
        result = run("sh", "-eu", "-c", prefix + 'printf "%s\\n" "$GHCRTS"; ghc input.hs', cwd=moved)
        assert result.stdout.splitlines() == ["-M768m", "input.hs", "+RTS", "-M2304m", "-RTS"], result.stdout
        result = run("sh", "-eu", "-c", prefix + "runghc Main.hs", cwd=moved)
        assert result.stdout.splitlines() == ["--ghc-arg=+RTS", "--ghc-arg=-M2304m", "--ghc-arg=-RTS", "Main.hs"], result.stdout
        result = run("sh", "-eu", "-c", prefix + 'cabal --config-file="$PWD/.cabal/config" path', cwd=moved)
        assert result.stdout.splitlines() == [str(moved / ".cabal"),
            f"--store-dir={moved}/.cabal/p07-rc-store",
            f"--config-file={moved}/.cabal/config", "path"], result.stdout
        result = run("sh", "-eu", "-c", '. "$1"', "test", str(moved / ".cabal/profile-0.2/f01/env.sh"), cwd=Path(tmp), check=False)
        assert result.returncode and "configured repository root" in result.stdout, result.stdout
    print("Local toolchain setup checks passed (no compilation or downloads).")


if __name__ == "__main__":
    main()
