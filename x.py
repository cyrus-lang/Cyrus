#!/usr/bin/env python3
# SPDX-License-Identifier: MIT
# Copyright (c) 2026 The Cyrus Language

from __future__ import annotations

import argparse
import difflib
import hashlib
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import textwrap
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Dict, Iterable, List, Optional, Sequence, Tuple

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError:  # pragma: no cover
    tomllib = None  # type: ignore[assignment]


# ************* Terminal helpers *************

def _color_enabled() -> bool:
    if os.environ.get("NO_COLOR"):
        return False
    if os.environ.get("CYRUS_X_COLOR", "").lower() in ("0", "false", "no"):
        return False
    return sys.stdout.isatty()


class Style:
    def __init__(self, enabled: bool) -> None:
        self.enabled = enabled

    def _wrap(self, code: str, text: str) -> str:
        if not self.enabled:
            return text
        return f"\x1b[{code}m{text}\x1b[0m"

    def bold(self, t: str) -> str:
        return self._wrap("1", t)

    def dim(self, t: str) -> str:
        return self._wrap("2", t)

    def red(self, t: str) -> str:
        return self._wrap("31", t)

    def green(self, t: str) -> str:
        return self._wrap("32", t)

    def yellow(self, t: str) -> str:
        return self._wrap("33", t)

    def blue(self, t: str) -> str:
        return self._wrap("34", t)

    def cyan(self, t: str) -> str:
        return self._wrap("36", t)


STYLE = Style(_color_enabled())
VERBOSE = False


def log(msg: str = "") -> None:
    print(msg, flush=True)


def debug(msg: str) -> None:
    if VERBOSE:
        log(STYLE.dim(f"[x.py] {msg}"))


# ************* Repository layout / configuration *************

def find_repo_root(start: Optional[Path] = None) -> Path:
    """Walk upwards until we find the directory that contains x.py + toolchain."""
    cur = (start or Path.cwd()).resolve()
    for candidate in [cur, *cur.parents]:
        if (candidate / "x.py").is_file() and (candidate / "toolchain").is_dir():
            return candidate
    # Fallback: directory containing this script.
    here = Path(__file__).resolve().parent
    if (here / "toolchain").is_dir():
        return here
    raise SystemExit("error: unable to locate the Cyrus repository root (x.py + toolchain/)")


DEFAULT_JOBS = max(1, min(24, (os.cpu_count() or 4) * 2))
STAGES = ("stage0", "stage1")
PROFILES = ("debug", "release")


@dataclass
class Config:
    """Resolved configuration for a single x.py invocation."""

    root: Path
    stage: str = "stage0"
    profile: str = "debug"
    jobs: int = DEFAULT_JOBS
    stdlib: str = "lib/std"
    input: str = "tmp/main.cyrus"
    stage0_dir: str = "toolchain/stage0"
    stage1_dir: str = "toolchain/stage1"
    tests_dir: str = "tests"
    output_dir: str = "tmp"
    cargo: str = "cargo"
    linker: str = "clang"
    config_file: Optional[Path] = None
    extra: Dict[str, object] = field(default_factory=dict)

    @property
    def stage0(self) -> Path:
        return self.root / self.stage0_dir

    @property
    def stage1(self) -> Path:
        return self.root / self.stage1_dir

    @property
    def tests(self) -> Path:
        return self.root / self.tests_dir

    @property
    def out(self) -> Path:
        return self.root / self.output_dir

    @property
    def stdlib_path(self) -> Path:
        p = Path(self.stdlib)
        return p if p.is_absolute() else (self.root / p)

    @property
    def release(self) -> bool:
        return self.profile == "release"

    @property
    def stage0_target(self) -> Path:
        return self.stage0 / "target" / self.profile

    @property
    def stage0_cargo_manifest(self) -> Path:
        return self.stage0 / "Cargo.toml"

    @property
    def stage0_binary(self) -> Path:
        exe = ".exe" if os.name == "nt" else ""
        return self.stage0_target / f"cyrus{exe}"

    @property
    def stage1_binary(self) -> Path:
        exe = ".exe" if os.name == "nt" else ""
        return self.stage1 / "build" / "output" / f"cyrus{exe}"

    def compiler_for(self, stage: str) -> Path:
        if stage == "stage0":
            return self.stage0_binary
        if stage == "stage1":
            return self.stage1_binary
        raise ValueError(f"unknown stage: {stage}")

    def tests_for(self, stage: str) -> Path:
        return self.tests / stage


ENV_KEYS = {
    "CYRUS_STAGE": "stage",
    "CYRUS_PROFILE": "profile",
    "CYRUS_JOBS": "jobs",
    "CYRUS_STDLIB": "stdlib",
    "CYRUS_INPUT": "input",
    "CYRUS_STAGE0_DIR": "stage0_dir",
    "CYRUS_STAGE1_DIR": "stage1_dir",
    "CYRUS_TESTS_DIR": "tests_dir",
    "CYRUS_CARGO": "cargo",
    "CYRUS_LINKER": "linker",
}


def load_config(root: Path, args: argparse.Namespace) -> Config:
    cfg = Config(root=root)

    # config file
    config_path = os.environ.get("CYRUS_X_CONFIG")
    candidates = [Path(config_path)] if config_path else [root / "x.toml"]
    for cand in candidates:
        if cand.is_file():
            cfg.config_file = cand
            if tomllib is None:
                log(STYLE.yellow(f"warning: tomllib unavailable, ignoring {cand}"))
                break
            with cand.open("rb") as fh:
                data = tomllib.load(fh)
            for key in (
                "stage",
                "profile",
                "jobs",
                "stdlib",
                "input",
                "stage0_dir",
                "stage1_dir",
                "tests_dir",
                "output_dir",
                "cargo",
                "linker",
            ):
                if key in data:
                    setattr(cfg, key, data[key])
            cfg.extra.update({k: v for k, v in data.items() if k not in {
                "stage", "profile", "jobs", "stdlib", "input", "stage0_dir",
                "stage1_dir", "tests_dir", "output_dir", "cargo", "linker",
            }})
            break

    # environment
    for env_key, field_name in ENV_KEYS.items():
        if env_key in os.environ:
            raw = os.environ[env_key]
            if field_name == "jobs":
                try:
                    setattr(cfg, field_name, int(raw))
                except ValueError:
                    log(STYLE.yellow(f"warning: ignoring invalid {env_key}={raw!r}"))
            else:
                setattr(cfg, field_name, raw)

    # CLI flags (SUPPRESS defaults: only present when actually passed)
    for field_name in (
        "stage",
        "profile",
        "jobs",
        "stdlib",
        "input",
        "stage0_dir",
        "stage1_dir",
        "tests_dir",
        "output_dir",
        "cargo",
        "linker",
    ):
        if hasattr(args, field_name):
            setattr(cfg, field_name, getattr(args, field_name))

    if cfg.stage not in (*STAGES, "all"):
        raise SystemExit(f"error: invalid stage {cfg.stage!r}; expected one of {STAGES} or 'all'")
    if cfg.profile not in PROFILES:
        raise SystemExit(f"error: invalid profile {cfg.profile!r}; expected one of {PROFILES}")
    if cfg.jobs < 1:
        raise SystemExit("error: --jobs must be >= 1")

    return cfg

# ************* Process helpers *************

class CommandFailed(Exception):
    def __init__(self, cmd: Sequence[str], returncode: int, stdout: str, stderr: str, cwd: Optional[Path] = None):
        self.cmd = list(cmd)
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr
        self.cwd = cwd
        pretty = " ".join(shlex.quote(str(c)) for c in cmd)
        where = f" (cwd={cwd})" if cwd else ""
        body = (stderr or stdout or "").strip()
        super().__init__(
            f"command failed with exit code {returncode}{where}: {pretty}\n{body}"
        )


def run_cmd(
    cmd: Sequence[str],
    *,
    cwd: Optional[Path] = None,
    env: Optional[Dict[str, str]] = None,
    check: bool = True,
    capture: bool = True,
    stdin_text: Optional[str] = None,
    timeout: Optional[float] = None,
) -> subprocess.CompletedProcess:
    pretty = " ".join(shlex.quote(str(c)) for c in cmd)
    debug(f"$ {pretty}" + (f"  (cwd={cwd})" if cwd else ""))
    proc = subprocess.run(
        [str(c) for c in cmd],
        cwd=str(cwd) if cwd else None,
        env=env,
        capture_output=capture,
        text=True,
        input=stdin_text,
        timeout=timeout,
    )
    if check and proc.returncode != 0:
        raise CommandFailed(cmd, proc.returncode, proc.stdout or "", proc.stderr or "", cwd)
    return proc


def strip_ansi(text: str) -> str:
    return re.sub(r"\x1b\[[0-9;]*m", "", text)


def normalize_text(text: str) -> str:
    """Normalize line endings and trailing whitespace for stable comparisons."""
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    lines = [line.rstrip() for line in text.split("\n")]
    # Drop trailing empty lines.
    while lines and lines[-1] == "":
        lines.pop()
    return "\n".join(lines)


# Strip absolute root paths and thread-name hashes so expectations survive
# abs/rel path differences and repo restructures.
_THREAD_HASH_RE = re.compile(r"(thread '[^']*?_)[0-9a-f]+")


def normalize_runtime_text(text: str, root: Optional[Path] = None) -> str:
    """Neutralize machine-specific paths and thread-name hashes in program output."""
    text = normalize_text(text)
    if root is not None:
        root_s = str(root)
        text = text.replace(root_s + os.sep, "")
        text = text.replace(root_s + "/", "")
    text = _THREAD_HASH_RE.sub(r"\1", text)
    return text


# EOF offsets equal file size; snapshot blocks live inside the file, so
# neutralize EOF positions for equality checks only.
EOF_OFFSET_RE = re.compile(
    r"^(EOF\b[ \t]+.*?Start:[ \t]*)\d+([ \t]+End:[ \t]*)\d+[ \t]*$",
    re.M,
)


def normalize_eof_offsets(text: str) -> str:
    return EOF_OFFSET_RE.sub(r"\1<file-end>\2<file-end>", text)


# ************* Diff engine *************

def _char_diff_note(expected: str, actual: str) -> str:
    """Describe the first differing region between two single lines."""
    sm = difflib.SequenceMatcher(a=expected, b=actual, autojunk=False)
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == "equal":
            continue
        exp_seg = expected[i1:i2]
        act_seg = actual[j1:j2]
        col = i1 + 1
        return (
            f"column {col}: expected {exp_seg!r} ({len(exp_seg)} chars), "
            f"got {act_seg!r} ({len(act_seg)} chars)"
        )
    return "lines differ"


def render_diff(
    expected: str,
    actual: str,
    *,
    expected_label: str = "expected (test file)",
    actual_label: str = "actual (compiler)",
    context: int = 3,
) -> str:
    """Render a precise, line-numbered diff between two multi-line texts.

    Every differing line is reported with:
      - its 1-based line number in each side
      - the full expected/actual content
      - a caret note describing the exact differing columns
    """
    exp_lines = expected.split("\n")
    act_lines = actual.split("\n")
    if expected == actual:
        return ""

    sm = difflib.SequenceMatcher(a=exp_lines, b=act_lines, autojunk=False)
    opcodes = sm.get_opcodes()

    changed = [k for k, (tag, *_ ) in enumerate(opcodes) if tag != "equal"]
    if not changed:
        return ""

    keep: set[int] = set()
    for k in changed:
        for j in range(max(0, k - context), min(len(opcodes), k + context + 1)):
            keep.add(j)

    out: List[str] = []
    out.append(STYLE.bold(f"diff: {expected_label}  vs  {actual_label}"))
    out.append(STYLE.dim(f"  - lines prefixed '-' come from {expected_label}"))
    out.append(STYLE.dim(f"  - lines prefixed '+' come from {actual_label}"))
    out.append("")

    prev_k = -2
    width = max(4, len(str(max(len(exp_lines), len(act_lines)))))

    for k in sorted(keep):
        if k != prev_k + 1:
            out.append(STYLE.dim("  ..."))
        prev_k = k
        tag, i1, i2, j1, j2 = opcodes[k]

        if tag == "equal":
            for offset, line in enumerate(exp_lines[i1:i2]):
                num = i1 + offset + 1
                out.append(f"  {STYLE.dim(str(num).rjust(width))}   {line}")
            continue

        if tag in ("replace", "delete"):
            for offset, line in enumerate(exp_lines[i1:i2]):
                num = i1 + offset + 1
                # Pair replace-lines with their counterpart when possible.
                if tag == "replace" and (j1 + offset) < j2:
                    other = act_lines[j1 + offset]
                    out.append(STYLE.red(f"  {str(num).rjust(width)} - {line}"))
                    out.append(STYLE.green(f"  {str(j1 + offset + 1).rjust(width)} + {other}"))
                    if line != other:
                        note = _char_diff_note(line, other)
                        out.append(STYLE.yellow(f"  {' ' * width}   ^ {note}"))
                else:
                    out.append(STYLE.red(f"  {str(num).rjust(width)} - {line}"))
                    out.append(STYLE.yellow(f"  {' ' * width}   ^ line missing in {actual_label}"))

        if tag == "insert":
            for offset, line in enumerate(act_lines[j1:j2]):
                num = j1 + offset + 1
                out.append(STYLE.green(f"  {str(num).rjust(width)} + {line}"))
                out.append(STYLE.yellow(f"  {' ' * width}   ^ extra line in {actual_label}"))

    n_exp_changed = sum(i2 - i1 for t, i1, i2, _, _ in opcodes if t in ("replace", "delete"))
    n_act_changed = sum(j2 - j1 for t, _, _, j1, j2 in opcodes if t in ("replace", "insert"))
    out.append("")
    out.append(
        STYLE.dim(
            f"  summary: {n_exp_changed} expected line(s) differ/missing, "
            f"{n_act_changed} actual line(s) differ/extra"
        )
    )
    return "\n".join(out)


# ************* Test annotations & metadata *************

LEVEL_KEYWORDS = {
    "ERROR": "error",
    "WARNING": "warning",
    "WARN": "warning",
    "UNIMPLEMENTED": "unimplemented",
}

ANNOTATION_RE = re.compile(
    r"//~(?P<adjust>[\^v|]*)\s*(?P<level>" + "|".join(LEVEL_KEYWORDS) + r")\b[ \t]*(?P<msg>.*?)\s*$"
)

DIRECTIVE_SINGLE_RE = {
    "stdout": re.compile(r"//\s*@stdout:\s*(.*)"),
    "stderr": re.compile(r"//\s*@stderr:\s*(.*)"),
    "stdin": re.compile(r"//\s*@stdin:\s*(.*)"),
    "args": re.compile(r"//\s*@args:\s*(.*)"),
    "beforeCompile": re.compile(r"//\s*@beforeCompile:\s*(.*)"),
    "compilerArgs": re.compile(r"//\s*@compilerArgs:\s*(.*)"),
}

# Snapshot directives: `// @name` trigger + `/*@name ... @name*/` block.
SNAPSHOT_DIRECTIVES = ("tokenize", "parse")


@dataclass
class ErrorAnnotation:
    line: int
    level: str
    msg: str
    comment_line: int


@dataclass
class SnapshotBlock:
    directive: str
    index: int
    open_line: int   # 1-based line of `/*@directive`
    close_line: int  # 1-based line of `@directive*/`
    expected: str


@dataclass
class TestMetadata:
    stdout: str = ""
    stderr: str = ""
    stdin: str = ""
    args: str = ""
    before_compile: str = ""
    compiler_args: str = ""
    error_annotations: List[ErrorAnnotation] = field(default_factory=list)
    snapshots: List[SnapshotBlock] = field(default_factory=list)
    triggers: Dict[str, int] = field(default_factory=dict)  # directive -> count


def extract_error_annotations(content: str) -> List[ErrorAnnotation]:
    annotations: List[ErrorAnnotation] = []
    prev_target: Optional[int] = None

    for lineno, raw_line in enumerate(content.splitlines(), start=1):
        match = ANNOTATION_RE.search(raw_line)
        if not match:
            continue
        adjust = match.group("adjust")
        level = LEVEL_KEYWORDS[match.group("level")]
        msg = match.group("msg").strip()

        if "|" in adjust:
            target = prev_target if prev_target is not None else lineno
        else:
            target = lineno - adjust.count("^") + adjust.count("v")

        prev_target = target
        annotations.append(ErrorAnnotation(target, level, msg, lineno))

    return annotations


def extract_snapshot_blocks(content: str, directive: str) -> List[SnapshotBlock]:
    """Extract `/*@directive ... @directive*/` expected-output blocks."""
    open_re = re.compile(rf"^/\*@{directive}[ \t]*$")
    close_re = re.compile(rf"@{directive}\*/")

    blocks: List[SnapshotBlock] = []
    lines = content.splitlines()
    i = 0
    idx = 0
    while i < len(lines):
        if open_re.match(lines[i]):
            open_line = i + 1
            j = i + 1
            body: List[str] = []
            found = False
            close_line = open_line
            while j < len(lines):
                if close_re.search(lines[j]):
                    close_line = j + 1
                    found = True
                    break
                body.append(lines[j])
                j += 1
            if not found:
                raise ValueError(
                    f"unterminated /*@{directive} block starting at line {open_line}"
                )
            blocks.append(
                SnapshotBlock(
                    directive=directive,
                    index=idx,
                    open_line=open_line,
                    close_line=close_line,
                    expected="\n".join(body),
                )
            )
            idx += 1
            i = j + 1
            continue
        i += 1
    return blocks


def count_triggers(content: str, directive: str) -> int:
    return len(re.findall(rf"^[ \t]*//[ \t]*@{directive}[ \t]*$", content, flags=re.M))


def extract_test_metadata(content: str) -> TestMetadata:
    md = TestMetadata()
    for key, pattern in DIRECTIVE_SINGLE_RE.items():
        match = pattern.search(content)
        if not match:
            continue
        value = match.group(1)
        if key in ("stdout", "stderr"):
            value = value.replace(r"\n", "\n").replace(r"\t", "\t").replace(r"\r", "\r")
        if key == "stdout":
            md.stdout = value
        elif key == "stderr":
            md.stderr = value
        elif key == "stdin":
            md.stdin = value
        elif key == "args":
            md.args = value
        elif key == "beforeCompile":
            md.before_compile = value
        elif key == "compilerArgs":
            md.compiler_args = value

    md.error_annotations = extract_error_annotations(content)

    for directive in SNAPSHOT_DIRECTIVES:
        md.triggers[directive] = count_triggers(content, directive)
        blocks = extract_snapshot_blocks(content, directive)
        md.snapshots.extend(blocks)

    return md


# ************* Ariadne diagnostic parsing *************

DIAG_START_RE = re.compile(r"^(?P<level>[Ee]rror|[Ww]arning|[Uu]nimplemented):\s*(?P<msg>.*?)\s*$")
DIAG_LOCATION_RE = re.compile(
    r"^\s*╭─\[\s*(?P<path>[^:]+?)\s*:\s*(?P<line>\d+)\s*:\s*(?P<col>\d+)\s*\]\s*$"
)


def parse_compiler_diagnostics(text: str) -> List[Dict[str, object]]:
    diagnostics: List[Dict[str, object]] = []
    lines = text.replace("\r\n", "\n").split("\n")
    i = 0
    while i < len(lines):
        diag_match = DIAG_START_RE.match(lines[i])
        if not diag_match:
            i += 1
            continue
        level = diag_match.group("level").lower()
        msg = diag_match.group("msg").strip()

        j = i + 1
        loc_match = None
        while j < len(lines) and j < i + 5:
            loc_match = DIAG_LOCATION_RE.match(lines[j])
            if loc_match:
                break
            j += 1

        if loc_match:
            path = loc_match.group("path").strip()
            line_num = int(loc_match.group("line"))
            col_num = int(loc_match.group("col"))
            k = j + 1
            while k < len(lines):
                if lines[k].strip().startswith("───╯") or DIAG_START_RE.match(lines[k]):
                    break
                k += 1
            diagnostics.append(
                {"level": level, "path": path, "line": line_num, "col": col_num, "msg": msg}
            )
            i = k
        else:
            i += 1
    return diagnostics


def match_error_annotations(
    annotations: List[ErrorAnnotation],
    diagnostics: List[Dict[str, object]],
    test_path: Path,
) -> List[str]:
    test_basename = os.path.basename(str(test_path))
    in_file = [d for d in diagnostics if os.path.basename(str(d["path"])) == test_basename]
    other = [d for d in diagnostics if os.path.basename(str(d["path"])) != test_basename]

    used = [False] * len(in_file)
    problems: List[str] = []

    for exp in annotations:
        matched = False
        for i, diag in enumerate(in_file):
            if used[i]:
                continue
            if (
                diag["level"] == exp.level
                and diag["line"] == exp.line
                and exp.msg in str(diag["msg"])
            ):
                used[i] = True
                matched = True
                break
        if not matched:
            wanted = f'matching "{exp.msg}"' if exp.msg else "(any message)"
            problems.append(
                f"expected {exp.level} on line {exp.line} {wanted}, but no such diagnostic was produced"
            )

    for i, diag in enumerate(in_file):
        if not used[i]:
            problems.append(
                f'unexpected {diag["level"]} on line {diag["line"]}: "{diag["msg"]}"'
            )

    for diag in other:
        problems.append(
            f'unexpected {diag["level"]} in {diag["path"]}:{diag["line"]}: "{diag["msg"]}"'
        )
    return problems


# ************* Snapshot helpers *************

def rewrite_snapshot_block(content: str, block: SnapshotBlock, new_body: str) -> str:
    lines = content.splitlines()
    open_idx = block.open_line - 1
    close_idx = block.close_line - 1
    if open_idx >= len(lines) or close_idx >= len(lines):
        raise ValueError("snapshot block line numbers out of range")
    new_lines = new_body.split("\n")
    replacement = lines[: open_idx + 1] + new_lines + lines[close_idx:]
    # Preserve a trailing newline if the original had one.
    text = "\n".join(replacement)
    if content.endswith("\n"):
        text += "\n"
    return text


# ************* Stage runners *************

def cargo_cmd(cfg: Config) -> List[str]:
    return [cfg.cargo]


def ensure_stage0(cfg: Config) -> None:
    """Build the stage0 compiler if the binary is missing."""
    if cfg.stage0_binary.is_file():
        return
    log(STYLE.cyan(f"building stage0 ({cfg.profile}) ..."))
    cmd = cargo_cmd(cfg) + [
        "build",
        "-j",
        str(cfg.jobs),
        "--manifest-path",
        str(cfg.stage0_cargo_manifest),
    ]
    if cfg.release:
        cmd.append("--release")
    run_cmd(cmd, cwd=cfg.root, capture=False)


def ensure_stage1(cfg: Config) -> None:
    """Build the stage1 (self-hosted) compiler using stage0."""
    if cfg.stage1_binary.is_file():
        return
    log(STYLE.cyan(f"building stage1 with stage0 ({cfg.profile}) ..."))
    build_stage1(cfg, release=cfg.release)


def cargo_run_bin(
    cfg: Config,
    *,
    package: Optional[str] = None,
    binary: Optional[str] = None,
    args: Sequence[str],
    cwd: Optional[Path] = None,
    capture: bool = False,
    check: bool = True,
) -> subprocess.CompletedProcess:
    cmd = cargo_cmd(cfg) + ["run", "-j", str(cfg.jobs), "--manifest-path", str(cfg.stage0_cargo_manifest)]
    if cfg.release:
        cmd.append("--release")
    if package:
        cmd += ["-p", package]
    if binary:
        cmd += ["--bin", binary]
    cmd.append("--")
    cmd += [str(a) for a in args]
    return run_cmd(cmd, cwd=cwd or cfg.root, capture=capture, check=check)


def stage0_binary_run(
    cfg: Config,
    args: Sequence[str],
    *,
    cwd: Optional[Path] = None,
    capture: bool = False,
    check: bool = True,
    env: Optional[Dict[str, str]] = None,
) -> subprocess.CompletedProcess:
    ensure_stage0(cfg)
    return run_cmd(
        [str(cfg.stage0_binary), *[str(a) for a in args]],
        cwd=cwd or cfg.root,
        capture=capture,
        check=check,
        env=env,
    )


def stage1_binary_run(
    cfg: Config,
    args: Sequence[str],
    *,
    cwd: Optional[Path] = None,
    capture: bool = False,
    check: bool = True,
) -> subprocess.CompletedProcess:
    ensure_stage1(cfg)
    return run_cmd(
        [str(cfg.stage1_binary), *[str(a) for a in args]],
        cwd=cwd or cfg.root,
        capture=capture,
        check=check,
    )


def build_stage0(cfg: Config, *, extra_cargo_args: Sequence[str] = ()) -> None:
    cmd = cargo_cmd(cfg) + [
        "build",
        "-j",
        str(cfg.jobs),
        "--manifest-path",
        str(cfg.stage0_cargo_manifest),
    ]
    if cfg.release:
        cmd.append("--release")
    cmd += list(extra_cargo_args)
    log(STYLE.cyan(f"building stage0 ({cfg.profile}, jobs={cfg.jobs}) ..."))
    run_cmd(cmd, cwd=cfg.root, capture=False)
    log(STYLE.green(f"stage0 binary: {cfg.stage0_binary}"))


def build_stage1(cfg: Config, *, release: bool, extra_args: Sequence[str] = ()) -> None:
    """Bootstrap stage1 by driving stage0's `build` command on Project.toml."""
    ensure_stage0(cfg)
    profile = "release" if release else "debug"

    if release:
        args = [
            "build",
            f"--profile={profile}",
            f"--stdlib={cfg.stdlib_path}",
            f"--linker={cfg.linker}",
            "--optimize=o2",
            '-z=-flto',
            "--module-merge-mode=separate",
            "--disable-modulefs-cache",
        ]
    else:
        args = [
            "build",
            f"--profile={profile}",
            "--sanitize=address",
            "-g",
            f"--stdlib={cfg.stdlib_path}",
            f"--linker={cfg.linker}",
            "--optimize=o0",
            "--module-merge-mode=separate",
            "--disable-modulefs-cache",
        ]
    args += list(extra_args)

    log(STYLE.cyan(f"building stage1 ({profile}, via stage0) ..."))
    run_cmd([str(cfg.stage0_binary), *args], cwd=cfg.stage1, capture=False)
    log(STYLE.green(f"stage1 binary: {cfg.stage1_binary}"))


def cmd_build(cfg: Config, args: argparse.Namespace) -> int:
    stages: List[str]
    if args.build_stage == "both":
        stages = ["stage0", "stage1"]
    else:
        stages = [args.build_stage or cfg.stage]

    for stage in stages:
        if stage == "stage0":
            extra = shlex.split(args.cargo_args or "")
            build_stage0(cfg, extra_cargo_args=extra)
        else:
            build_stage1(cfg, release=cfg.release, extra_args=shlex.split(args.stage1_args or ""))
    return 0


def cmd_clean(cfg: Config, args: argparse.Namespace) -> int:
    stages = ["stage0", "stage1"] if args.clean_stage == "all" else [args.clean_stage]
    for stage in stages:
        if stage == "stage0":
            target = cfg.stage0 / "target"
            if target.exists():
                log(f"removing {target}")
                shutil.rmtree(target)
        elif stage == "stage1":
            build_dir = cfg.stage1 / "build"
            if build_dir.exists():
                log(f"removing {build_dir}")
                shutil.rmtree(build_dir)
    return 0


def cmd_version(cfg: Config, args: argparse.Namespace) -> int:
    version_file = cfg.stage0 / "VERSION"
    if version_file.is_file():
        log(version_file.read_text().strip())
    if cfg.stage0_binary.is_file():
        proc = run_cmd([str(cfg.stage0_binary), "version"], check=False, capture=True)
        log(strip_ansi((proc.stdout or proc.stderr or "").strip()))
    if cfg.stage1_binary.is_file():
        proc = run_cmd([str(cfg.stage1_binary), "version"], check=False, capture=True)
        log("stage1: " + strip_ansi((proc.stdout or proc.stderr or "").strip()))
    return 0


# ************* Stage0 direct sub-commands *************

def _require_input(cfg: Config, args: argparse.Namespace) -> str:
    input_path = getattr(args, "input_file", None) or cfg.input
    p = Path(input_path)
    if not p.is_file():
        raise SystemExit(
            f"error: input file '{input_path}' not found "
            f"(set via positional arg, --input, or CYRUS_INPUT)"
        )
    return str(p)


def _stdlib_flag(cfg: Config) -> str:
    return f"--stdlib={cfg.stdlib_path}"


def cmd_resolver_impl(cfg: Config, args: argparse.Namespace) -> int:
    input_path = _require_input(cfg, args)
    rest = list(args.rest or [])
    if not any(a.startswith("--stdlib") for a in rest):
        rest.append(_stdlib_flag(cfg))
    cmd = cargo_cmd(cfg) + [
        "run", "-j", str(cfg.jobs), "--manifest-path", str(cfg.stage0_cargo_manifest),
    ]
    if cfg.release:
        cmd.append("--release")
    cmd += ["-p", "cyrusc_resolver", "--bin", "cyrusc_resolver", "--", input_path, *rest]
    proc = run_cmd(cmd, cwd=cfg.root, capture=False, check=False)
    return proc.returncode


def cmd_resolver_dump(cfg: Config, args: argparse.Namespace) -> int:
    input_path = _require_input(cfg, args)
    rest = list(args.rest or [])
    dump_path = str(args.dump_path) if getattr(args, "dump_path", None) else None
    positionals: List[str] = []
    forwarded: List[str] = []
    if dump_path is None:
        # Allow: x.py resolver-dump file.cyrus [dump_path] [--flags...]
        for a in rest:
            if not a.startswith("-") and dump_path is None and not positionals:
                dump_path = a
            else:
                forwarded.append(a)
        rest = forwarded
    if dump_path is None:
        dump_path = str(cfg.out / "global_symbols_dump")
    Path(dump_path).parent.mkdir(parents=True, exist_ok=True)

    if not any(a.startswith("--stdlib") for a in rest):
        rest.append(_stdlib_flag(cfg))

    cmd = cargo_cmd(cfg) + [
        "run", "-j", str(cfg.jobs), "--manifest-path", str(cfg.stage0_cargo_manifest),
    ]
    if cfg.release:
        cmd.append("--release")
    cmd += [
        "-p", "cyrusc_resolver", "--bin", "cyrusc_resolver_dump",
        "--", input_path, dump_path, *rest,
    ]
    proc = run_cmd(cmd, cwd=cfg.root, capture=False, check=False)
    if proc.returncode == 0 and args.open and shutil.which("code"):
        run_cmd(["code", dump_path], check=False, capture=False)
    return proc.returncode


def cmd_crate_bin(cfg: Config, args: argparse.Namespace, *, package: str, binary: Optional[str] = None) -> int:
    input_path = _require_input(cfg, args)
    rest = [str(a) for a in (args.rest or [])]
    cmd = cargo_cmd(cfg) + [
        "run", "-j", str(cfg.jobs), "--manifest-path", str(cfg.stage0_cargo_manifest),
    ]
    if cfg.release:
        cmd.append("--release")
    cmd += ["-p", package]
    if binary:
        cmd += ["--bin", binary]
    cmd.append("--")
    cmd.append(input_path)
    cmd += rest
    proc = run_cmd(cmd, cwd=cfg.root, capture=False, check=False)
    return proc.returncode


def cmd_stage0_cli(
    cfg: Config,
    args: argparse.Namespace,
    *,
    cli_args: Sequence[str],
    accepts_stdlib: bool = True,
) -> int:
    """Invoke a subcommand of the main stage0 `cyrus` binary via cargo run."""
    input_path = _require_input(cfg, args)
    rest = [str(a) for a in (args.rest or [])]
    assembled: List[str] = list(cli_args)
    assembled = [input_path if a == "$INPUT" else a for a in assembled]
    has_stdlib = any(str(a).startswith("--stdlib") for a in rest) or any(
        str(a).startswith("--stdlib") for a in assembled
    )
    cmd = cargo_cmd(cfg) + [
        "run", "-j", str(cfg.jobs), "--manifest-path", str(cfg.stage0_cargo_manifest),
    ]
    if cfg.release:
        cmd.append("--release")
    cmd.append("--")
    cmd += assembled
    if accepts_stdlib and not has_stdlib:
        cmd.append(_stdlib_flag(cfg))
    cmd += rest
    proc = run_cmd(cmd, cwd=cfg.root, capture=False, check=False)
    return proc.returncode


def cmd_sanitize_run(cfg: Config, args: argparse.Namespace) -> int:
    input_path = _require_input(cfg, args)
    rest = [str(a) for a in (args.rest or [])]
    cmd = cargo_cmd(cfg) + [
        "run", "-j", str(cfg.jobs), "--manifest-path", str(cfg.stage0_cargo_manifest),
    ]
    if cfg.release:
        cmd.append("--release")
    cmd += ["--", "run", input_path, "--sanitize=address"]
    if not any(str(a).startswith("--stdlib") for a in rest):
        cmd.append(_stdlib_flag(cfg))
    cmd += rest
    proc = run_cmd(cmd, cwd=cfg.root, capture=False, check=False)
    return proc.returncode


def cmd_tokenize(cfg: Config, args: argparse.Namespace) -> int:
    stage = getattr(args, "stage", None) or "stage1"
    input_path = _require_input(cfg, args)
    rest = [str(a) for a in (args.rest or [])]
    if stage == "stage1":
        ensure_stage1(cfg)
        proc = run_cmd([str(cfg.stage1_binary), "tokenize", input_path, *rest],
                       cwd=cfg.root, capture=False, check=False)
    else:
        proc = stage0_binary_run(cfg, ["lex-only", input_path, *rest], capture=False, check=False)
    return proc.returncode


def cmd_parse(cfg: Config, args: argparse.Namespace) -> int:
    stage = getattr(args, "stage", None) or "stage1"
    input_path = _require_input(cfg, args)
    rest = [str(a) for a in (args.rest or [])]
    if stage == "stage1":
        ensure_stage1(cfg)
        proc = run_cmd([str(cfg.stage1_binary), "parse", input_path, *rest],
                       cwd=cfg.root, capture=False, check=False)
    else:
        proc = stage0_binary_run(cfg, ["parse-only", input_path, *rest], capture=False, check=False)
    return proc.returncode


def cmd_run(cfg: Config, args: argparse.Namespace) -> int:
    stage = getattr(args, "stage", None) or "stage0"
    input_path = _require_input(cfg, args)
    rest = [str(a) for a in (args.rest or [])]
    if not any(str(a).startswith("--stdlib") for a in rest):
        rest.append(_stdlib_flag(cfg))
    if stage == "stage0":
        cmd = cargo_cmd(cfg) + [
            "run", "-j", str(cfg.jobs), "--manifest-path", str(cfg.stage0_cargo_manifest),
        ]
        if cfg.release:
            cmd.append("--release")
        cmd += ["--", "run", input_path, *rest]
        proc = run_cmd(cmd, cwd=cfg.root, capture=False, check=False)
        return proc.returncode
    ensure_stage1(cfg)
    proc = run_cmd([str(cfg.stage1_binary), "run", input_path, *rest],
                   cwd=cfg.root, capture=False, check=False)
    return proc.returncode


def cmd_stage0_exec(cfg: Config, args: argparse.Namespace, *, argv0: str) -> int:
    """Run the stage0 binary with a fixed subcommand: ./x.py stage0 version"""
    ensure_stage0(cfg)
    rest = [str(a) for a in (args.rest or [])]
    proc = run_cmd([str(cfg.stage0_binary), argv0, *rest],
                   cwd=cfg.root, capture=False, check=False)
    return proc.returncode


def cmd_stage1(cfg: Config, args: argparse.Namespace) -> int:
    """Pass arbitrary subcommands to the stage1 binary: ./x.py stage1 tokenize f.cyrus"""
    ensure_stage1(cfg)
    rest = [str(a) for a in (args.rest or [])]
    proc = run_cmd([str(cfg.stage1_binary), *rest], cwd=cfg.root, capture=False, check=False)
    return proc.returncode


# ************* Test framework *************

@dataclass
class TestOutcome:
    status: str  # "passed" | "failed" | "blessed"
    name: str
    reason: Optional[str] = None


def unique_name(path: Path) -> str:
    h = hashlib.sha1(str(path).encode()).hexdigest()[:10]
    return f"{path.stem}_{h}"


def snapshot_command_for(stage: str, directive: str, cfg: Config) -> List[str]:
    """Command argv (excluding binary) used to produce snapshot output."""
    if stage == "stage1":
        if directive == "tokenize":
            return ["tokenize"]
        if directive == "parse":
            return ["parse"]
    if stage == "stage0":
        if directive == "tokenize":
            return ["lex-only"]
        if directive == "parse":
            return ["parse-only"]
    raise ValueError(f"no snapshot command for stage={stage} directive={directive}")


def run_snapshot_checks(
    cfg: Config,
    stage: str,
    file_path: Path,
    md: TestMetadata,
    *,
    bless: bool,
    compiler: Path,
) -> Tuple[List[str], bool]:
    """Run `// @tokenize` / `// @parse` snapshot checks.

    Returns (problems, blessed_something).
    """
    problems: List[str] = []
    blessed = False
    content = file_path.read_text()

    for directive in SNAPSHOT_DIRECTIVES:
        trigger_count = md.triggers.get(directive, 0)
        blocks = [b for b in md.snapshots if b.directive == directive]
        if trigger_count == 0 and not blocks:
            continue
        if trigger_count == 0 and blocks:
            problems.append(
                f"found /*@{directive}*/ block(s) but no `// @{directive}` trigger line"
            )
            continue
        if trigger_count > 0 and not blocks:
            problems.append(
                f"missing /*@{directive} ... @{directive}*/ expected-output block "
                f"for `// @{directive}` trigger"
            )
            continue
        if trigger_count != len(blocks):
            problems.append(
                f"mismatch: {trigger_count} `// @{directive}` trigger(s) vs "
                f"{len(blocks)} /*@{directive}*/ block(s)"
            )
            continue

        argv = snapshot_command_for(stage, directive, cfg)
        cmd = [str(compiler), *argv, str(file_path)]
        proc = run_cmd(cmd, cwd=cfg.root, capture=True, check=False)
        raw = proc.stdout or ""
        if proc.returncode != 0:
            detail = strip_ansi((proc.stderr or raw or "").strip())
            problems.append(
                f"`{' '.join(argv)}` failed with exit code {proc.returncode}:\n{detail}"
            )
            continue
        actual = normalize_text(strip_ansi(raw))

        for block in blocks:
            expected = normalize_text(block.expected)
            if normalize_eof_offsets(actual) == normalize_eof_offsets(expected):
                continue
            if bless:
                content = rewrite_snapshot_block(content, block, actual)
                blessed = True
            else:
                exp_norm = normalize_eof_offsets(expected)
                act_norm = normalize_eof_offsets(actual)
                if exp_norm != act_norm:
                    problems.append(
                        f"//@{directive} block (lines {block.open_line}-{block.close_line}) mismatch:\n"
                        + render_diff(
                            exp_norm,
                            act_norm,
                            expected_label=f"expected (@{directive} block)",
                            actual_label="actual (compiler stdout)",
                        )
                    )
                else:
                    problems.append(
                        f"//@{directive} block (lines {block.open_line}-{block.close_line}): "
                        f"EOF offset is stale (file size changed) — re-run with --bless"
                    )

    if blessed:
        file_path.write_text(content)

    return problems, blessed


def build_and_run_stage0(
    cfg: Config,
    file_path: Path,
    md: TestMetadata,
    *,
    compiler: Path,
    compiler_flags: Sequence[str],
    run_number: int,
    total_runs: int,
) -> None:
    """Compile and execute a stage0 runtime/UI test; raise on failure."""
    with tempfile.TemporaryDirectory() as tmp:
        tmpdir = Path(tmp)
        suffix = ".exe" if os.name == "nt" else ""
        output_binary = tmpdir / f"{unique_name(file_path)}{suffix}"
        env = os.environ.copy()
        env["TMPDIR"] = env["TEMP"] = env["TMP"] = str(tmpdir)
        env.setdefault("CYRUS_STDLIB_PATH", str(cfg.stdlib_path))

        if md.before_compile:
            before = shlex.split(md.before_compile)
            before_result = run_cmd(before, capture=True, check=False, env=env, cwd=cfg.root)
            if before_result.returncode != 0:
                raise RuntimeError(
                    f"beforeCompile error:\n{(before_result.stderr or before_result.stdout or '').strip()}"
                )

        # Prefer cwd-relative path so embedded source paths stay stable.
        try:
            rel_src = os.path.relpath(str(file_path), str(cfg.root))
            if rel_src.startswith(".."):
                rel_src = str(file_path)
        except ValueError:
            rel_src = str(file_path)

        build_cmd = [str(compiler), "build", rel_src, "-o", str(output_binary)]
        build_cmd += list(compiler_flags)
        if md.compiler_args:
            build_cmd += shlex.split(md.compiler_args)

        build_result = run_cmd(build_cmd, capture=True, check=False, env=env, cwd=cfg.root)
        combined = strip_ansi((build_result.stdout or "") + "\n" + (build_result.stderr or ""))

        if md.error_annotations:
            diagnostics = parse_compiler_diagnostics(combined)
            problems = match_error_annotations(md.error_annotations, diagnostics, file_path)
            if problems:
                detail = "\n".join(f"  - {p}" for p in problems)
                raise RuntimeError(
                    f"error-annotation mismatch:\n{detail}\n\n"
                    f"--- compiler output ---\n{combined.strip()}\n"
                    f"(run {run_number}/{total_runs})"
                )
            return

        if build_result.returncode != 0:
            raise RuntimeError(
                f"build failed (exit {build_result.returncode}):\n"
                f"{combined.strip() or '(no output)'}\n(run {run_number}/{total_runs})"
            )

        run_args = shlex.split(md.args or "")
        run_cmd_argv = [str(output_binary), *run_args]
        run_result = run_cmd(
            run_cmd_argv,
            capture=True,
            check=False,
            env=env,
            cwd=cfg.root,
            stdin_text=md.stdin or "",
        )

        actual_stdout = normalize_runtime_text(run_result.stdout or "", cfg.root)
        expected_stdout = normalize_runtime_text(md.stdout, cfg.root)
        actual_stderr = normalize_runtime_text(run_result.stderr or "", cfg.root)
        expected_stderr = normalize_runtime_text(md.stderr, cfg.root)

        if actual_stdout != expected_stdout:
            raise RuntimeError(
                "stdout mismatch:\n"
                + render_diff(
                    expected_stdout,
                    actual_stdout,
                    expected_label="expected (@stdout)",
                    actual_label="actual (program stdout)",
                )
                + (
                    f"\nprogram stderr:\n{actual_stderr}" if actual_stderr else ""
                )
                + f"\n(run {run_number}/{total_runs})"
            )

        if actual_stderr != expected_stderr:
            raise RuntimeError(
                "stderr mismatch:\n"
                + render_diff(
                    expected_stderr,
                    actual_stderr,
                    expected_label="expected (@stderr)",
                    actual_label="actual (program stderr)",
                )
                + f"\n(run {run_number}/{total_runs})"
            )

        if expected_stderr == "" and run_result.returncode != 0:
            raise RuntimeError(
                f"program exited with code {run_result.returncode} "
                f"(run {run_number}/{total_runs})"
            )


def run_single_test(
    cfg: Config,
    stage: str,
    file_path: Path,
    base_path: Path,
    *,
    compiler: Path,
    compiler_flags: Sequence[str],
    repeat: int,
    bless: bool,
) -> TestOutcome:
    try:
        relative_name = str(file_path.relative_to(base_path))
    except ValueError:
        relative_name = file_path.name

    try:
        content = file_path.read_text()
    except OSError as exc:
        return TestOutcome("failed", relative_name, f"cannot read test file: {exc}")

    try:
        md = extract_test_metadata(content)
    except ValueError as exc:
        return TestOutcome("failed", relative_name, str(exc))

    has_runtime = bool(
        md.error_annotations
        or md.stdout
        or md.stderr
        or md.args
        or md.stdin
        or md.before_compile
        or md.compiler_args
    )
    has_snapshots = any(md.triggers.get(d, 0) for d in SNAPSHOT_DIRECTIVES) or bool(md.snapshots)

    if not has_runtime and not has_snapshots:
        # No annotations: stage0 smoke test (must compile with empty output).
        if stage == "stage0":
            has_runtime = True
        else:
            return TestOutcome(
                "passed",
                relative_name,
            )

    failures: List[str] = []
    blessed_any = False

    for run_num in range(1, repeat + 1):
        try:
            if has_snapshots:
                problems, blessed = run_snapshot_checks(
                    cfg, stage, file_path, md, bless=bless, compiler=compiler
                )
                blessed_any = blessed_any or blessed
                if problems:
                    raise RuntimeError("\n".join(problems))
                # Reload metadata after bless rewrites the file.
                if blessed:
                    content = file_path.read_text()
                    md = extract_test_metadata(content)

            if has_runtime:
                if stage == "stage0":
                    build_and_run_stage0(
                        cfg,
                        file_path,
                        md,
                        compiler=compiler,
                        compiler_flags=compiler_flags,
                        run_number=run_num,
                        total_runs=repeat,
                    )
                else:
                    raise RuntimeError(
                        "runtime checks (@stdout/@stderr/...) are only supported "
                        "for stage0 tests (stage1 has no codegen yet)"
                    )

            if repeat > 1:
                log(f"    [run {run_num}/{repeat}] passed")
        except (RuntimeError, CommandFailed, ValueError) as exc:
            msg = str(exc)
            failures.append(f"run {run_num}/{repeat} failed:\n{msg}")
            if repeat > 1:
                log(f"    [run {run_num}/{repeat}] failed")

    if blessed_any and not failures:
        return TestOutcome("blessed", relative_name)

    if failures:
        return TestOutcome("failed", relative_name, "\n".join(failures))
    return TestOutcome("passed", relative_name)


def discover_tests(paths: Sequence[Path], stage: str) -> Tuple[Path, List[Path]]:
    """Return (base_path, test_files) for the given paths/stage."""
    test_files: List[Path] = []
    base_path: Optional[Path] = None

    if not paths:
        return Path("."), []

    for p in paths:
        p = p.resolve()
        if p.is_file():
            if p.suffix != ".cyrus":
                raise SystemExit(f"error: {p} is not a .cyrus file")
            if p.name.startswith("_"):
                continue
            test_files.append(p)
            base_path = base_path or p.parent
        elif p.is_dir():
            found = sorted(f for f in p.rglob("*.cyrus") if not f.name.startswith("_"))
            test_files.extend(found)
            base_path = base_path or p
        else:
            raise SystemExit(f"error: test path '{p}' does not exist")

    # De-dup while preserving order.
    seen = set()
    unique: List[Path] = []
    for f in test_files:
        if f not in seen:
            seen.add(f)
            unique.append(f)

    return (base_path or Path(".")), unique


def filter_by_substring(files: Sequence[Path], needle: Optional[str], base: Path) -> List[Path]:
    if not needle:
        return list(files)
    out = []
    for f in files:
        try:
            rel = str(f.relative_to(base))
        except ValueError:
            rel = str(f)
        if needle in rel or needle in f.name:
            out.append(f)
    return out

# ************* Test runner *************

def cmd_test(cfg: Config, args: argparse.Namespace) -> int:
    stage_opt = getattr(args, "stage", None)

    # ************* Unit tests *************
    if args.unit:
        cmd = cargo_cmd(cfg) + [
            "test",
            "-j",
            str(cfg.jobs),
            "--manifest-path",
            str(cfg.stage0_cargo_manifest),
        ]
        if cfg.release:
            cmd.append("--release")
        cmd += shlex.split(getattr(args, "cargo_args", "") or "")
        proc = run_cmd(cmd, cwd=cfg.root, capture=False, check=False)
        if proc.returncode != 0:
            return proc.returncode
        # `./x.py test --unit` alone stops after cargo tests.
        if not args.paths and not stage_opt and not args.suite:
            return 0

    # ************* Stages *************
    if stage_opt == "all":
        stages = list(STAGES)
    elif stage_opt:
        stages = [stage_opt]
    else:
        stages = [cfg.stage if cfg.stage != "all" else "stage0"]
        if cfg.stage == "all":
            stages = list(STAGES)

    # ************* Test paths *************
    raw_paths = [Path(p) for p in (args.paths or [])]
    jobs_workers = cfg.jobs
    all_files: List[Tuple[str, Path, Path]] = []  # (stage, file, base)

    def infer_stage(path: Path) -> str:
        resolved = path if path.is_absolute() else (Path.cwd() / path)
        parts = resolved.resolve().parts
        if "stage0" in parts:
            return "stage0"
        if "stage1" in parts:
            return "stage1"
        return stages[0] if stages else "stage0"

    if raw_paths:
        by_stage: Dict[str, List[Path]] = {}
        for rp in raw_paths:
            resolved = rp if rp.is_absolute() else (Path.cwd() / rp)
            resolved = resolved.resolve()
            stage = stage_opt if stage_opt and stage_opt != "all" else infer_stage(resolved)
            by_stage.setdefault(stage, []).append(resolved)
        for stage, stage_paths in by_stage.items():
            if stage not in STAGES:
                continue
            base, files = discover_tests(stage_paths, stage)
            files = filter_by_substring(files, args.filter, base)
            for f in files:
                all_files.append((stage, f, base))
        stages = sorted(s for s in by_stage if s in STAGES)
    else:
        found_stages: List[str] = []
        for stage in stages:
            d = cfg.tests_for(stage)
            if not d.is_dir():
                continue
            found_stages.append(stage)
            base, files = discover_tests([d], stage)
            files = filter_by_substring(files, args.filter, base)
            for f in files:
                all_files.append((stage, f, base))
        stages = found_stages or stages

    if not all_files:
        log(STYLE.yellow("no .cyrus test files found"))
        return 0

    # ************* Compiler binaries *************
    if "stage0" in stages:
        ensure_stage0(cfg)
    if "stage1" in stages:
        ensure_stage1(cfg)

    compiler_flags: List[str] = [f"--stdlib={cfg.stdlib_path}", "--quiet"]
    if args.flags:
        compiler_flags += shlex.split(args.flags)

    output_dir = cfg.out / "tests"
    output_dir.mkdir(parents=True, exist_ok=True)

    log(STYLE.bold(f"Running {len(all_files)} test(s) "
                   f"[stages: {', '.join(stages)}; profile: {cfg.profile}; jobs: {jobs_workers}]"))
    if args.bless:
        log(STYLE.yellow("--bless: expected outputs will be rewritten on mismatch"))
    if args.repeat > 1:
        log(STYLE.yellow(f"each test will run {args.repeat} time(s)"))
    log()

    compiler_paths = {stage: cfg.compiler_for(stage) for stage in stages}
    print_lock = threading.Lock()
    passed: List[str] = []
    failed: List[Tuple[str, str]] = []
    blessed: List[str] = []
    completed = 0
    total = len(all_files)

    def worker(stage: str, file_path: Path, base: Path) -> TestOutcome:
        return run_single_test(
            cfg,
            stage,
            file_path,
            base,
            compiler=compiler_paths[stage],
            compiler_flags=compiler_flags,
            repeat=args.repeat,
            bless=args.bless,
        )

    max_workers = max(1, min(jobs_workers, total))
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        futures = {
            pool.submit(worker, stage, f, base): (stage, f)
            for stage, f, base in all_files
        }
        for fut in as_completed(futures):
            stage, fpath = futures[fut]
            try:
                outcome = fut.result()
            except Exception as exc:  # surface unexpected errors as failures
                try:
                    name = str(fpath.relative_to(cfg.tests))
                except ValueError:
                    name = fpath.name
                outcome = TestOutcome("failed", name, f"internal error: {exc}")

            with print_lock:
                completed += 1
                prefix = f"[{completed}/{total}]"
                if outcome.status == "passed":
                    passed.append(outcome.name)
                    if not args.fail:
                        log(f"{STYLE.green(prefix)} {STYLE.green('ok')}      {outcome.name}")
                elif outcome.status == "blessed":
                    blessed.append(outcome.name)
                    log(f"{STYLE.yellow(prefix)} {STYLE.yellow('blessed')} {outcome.name}")
                else:
                    failed.append((outcome.name, outcome.reason or ""))
                    log(f"{STYLE.red(prefix)} {STYLE.red('FAIL')}    {outcome.name}")
                    if outcome.reason:
                        indented = "\n".join(
                            "        " + line for line in outcome.reason.strip().splitlines()
                        )
                        log(indented)
                    log()

    passed.sort()
    failed.sort(key=lambda x: x[0])
    blessed.sort()

    log()
    log(STYLE.bold("-------------------------------------------"))
    log(STYLE.bold("Test Summary"))
    log(f"  Total:   {total}")
    log(f"  Passed:  {STYLE.green(str(len(passed)))}")
    if blessed:
        log(f"  Blessed: {STYLE.yellow(str(len(blessed)))}")
    log(f"  Failed:  {STYLE.red(str(len(failed))) if failed else '0'}")

    if args.bless and blessed:
        log()
        log(STYLE.yellow("blessed files:"))
        for name in blessed:
            log(f"  - {name}")

    if failed:
        log()
        log(STYLE.red("failures:"))
        for name, _ in failed:
            log(f"  - {name}")
        return 1
    return 0


# ************* CLI *************

def add_config_args(p: argparse.ArgumentParser) -> None:
    """Add shared config flags. Defaults are SUPPRESS so pre-subcommand values survive."""
    g = p.add_argument_group("configuration")
    g.add_argument("--stage", choices=[*STAGES, "all"], default=argparse.SUPPRESS,
                   help="compiler stage to use (env: CYRUS_STAGE)")
    g.add_argument("--profile", choices=list(PROFILES), default=argparse.SUPPRESS,
                   help="build profile (env: CYRUS_PROFILE)")
    g.add_argument("--jobs", "-j", type=int, default=argparse.SUPPRESS,
                   help="parallel job count (env: CYRUS_JOBS)")
    g.add_argument("--stdlib", default=argparse.SUPPRESS,
                   help="stdlib root containing std modules (env: CYRUS_STDLIB; default: lib/std)")
    g.add_argument("--input", "-i", default=argparse.SUPPRESS,
                   help="default input file (env: CYRUS_INPUT)")
    g.add_argument("--stage0-dir", dest="stage0_dir", default=argparse.SUPPRESS,
                   help="stage0 directory (default: toolchain/stage0)")
    g.add_argument("--stage1-dir", dest="stage1_dir", default=argparse.SUPPRESS,
                   help="stage1 directory (default: toolchain/stage1)")
    g.add_argument("--tests-dir", dest="tests_dir", default=argparse.SUPPRESS,
                   help="tests directory (default: tests)")
    g.add_argument("--cargo", default=argparse.SUPPRESS, help="cargo executable")
    g.add_argument("--linker", default=argparse.SUPPRESS, help="linker used when bootstrapping stage1")
    g.add_argument("--verbose", "-v", action="store_true", default=argparse.SUPPRESS,
                   help="verbose x.py logging")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="x.py",
        description="Cyrus bootstrap driver — build, test, and run every compiler stage.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            configuration precedence: defaults < x.toml < CYRUS_* env < flags

            test annotations:
              // @stdout: ...           expected program stdout (stage0)
              // @stderr: ...           expected program stderr (stage0)
              // @stdin: ...            program stdin
              // @args: ...             program argv
              // @compilerArgs: ...     extra flags for `cyrus build`
              // @beforeCompile: shell  command run before compiling
              //~ ERROR message          expected diagnostic (rustc style)
              // @tokenize              run tokenize and compare /*@tokenize ... @tokenize*/
              // @parse                 run parse and compare /*@parse ... @parse*/

            examples:
              ./x.py build --stage stage0 --profile release
              ./x.py test --stage all -j 8
              ./x.py test tests/stage1 --bless
              ./x.py run tmp/main.cyrus
              ./x.py stage0 resolver tmp/main.cyrus
              ./x.py stage0 lexer tmp/main.cyrus
              ./x.py stage0 tokenize tmp/main.cyrus
              ./x.py stage1 tokenize tmp/main.cyrus
              ./x.py stage1 parse tmp/main.cyrus
            """
        ),
    )
    add_config_args(parser)

    sub = parser.add_subparsers(dest="command", metavar="COMMAND")

    # ************* build *************
    p = sub.add_parser("build", help="build the compiler (stage0 and/or stage1)")
    add_config_args(p)
    p.add_argument("--build-stage", dest="build_stage", choices=[*STAGES, "both"],
                   default=None, help="which stage(s) to build (default: --stage)")
    p.add_argument("--cargo-args", default="", help="extra args passed to cargo (stage0)")
    p.add_argument("--stage1-args", default="", help="extra args passed to stage1 build")
    p.set_defaults(func=cmd_build, build_stage=None)

    # ************* clean *************
    p = sub.add_parser("clean", help="remove build artifacts")
    add_config_args(p)
    p.add_argument("--clean-stage", dest="clean_stage",
                   choices=[*STAGES, "all"], default="all")
    p.set_defaults(func=cmd_clean, clean_stage="all")

    # ************* test *************
    p = sub.add_parser("test", help="run the multi-stage test suites")
    add_config_args(p)
    p.add_argument("paths", nargs="*", help="test files or directories (default: tests/<stage>)")
    p.add_argument("--filter", "-f", default=None,
                   help="only run tests whose relative path contains this substring")
    p.add_argument("--repeat", type=int, default=1,
                   help="run each test N times (flakiness detection)")
    p.add_argument("--fail", action="store_true",
                   help="only print failing tests")
    p.add_argument("--bless", action="store_true",
                   help="rewrite /*@tokenize*/ etc. expected blocks from compiler output")
    p.add_argument("--flags", default="",
                   help="extra compiler flags for stage0 builds (quoted string)")
    p.add_argument("--unit", action="store_true",
                   help="also run cargo unit tests")
    p.add_argument("--suite", action="store_true",
                   help="run the file-based suite (default when no other selection is made)")
    p.add_argument("--cargo-args", default="", help="extra args for --unit cargo test")
    p.set_defaults(func=cmd_test)

    # ************* run *************
    p = sub.add_parser("run", help="compile and run a .cyrus file")
    add_config_args(p)
    p.add_argument("input_file", nargs="?", default=None)
    p.add_argument("rest", nargs=argparse.REMAINDER,
                   help="arguments after -- are forwarded to the program / compiler")
    p.set_defaults(func=cmd_run)

    # ************* version *************
    p = sub.add_parser("version", help="print compiler version information")
    add_config_args(p)
    p.set_defaults(func=cmd_version)

    # ************* stage0 (tools + binary passthrough) *************
    p = sub.add_parser(
        "stage0",
        help="stage0 tools and binary subcommands",
        description="Stage0 compiler tools, crates, and binary subcommands.",
    )
    add_config_args(p)
    s0 = p.add_subparsers(dest="stage0_cmd", metavar="COMMAND")

    def stage0_input_cmd(name: str, help_text: str, func, **defaults) -> None:
        sp = s0.add_parser(name, help=help_text)
        add_config_args(sp)
        sp.add_argument("input_file", nargs="?", default=None)
        sp.add_argument("rest", nargs=argparse.REMAINDER)
        sp.set_defaults(func=func, **defaults)

    def stage0_bin_cmd(name: str, help_text: str) -> None:
        sp = s0.add_parser(name, help=help_text)
        add_config_args(sp)
        sp.add_argument("rest", nargs=argparse.REMAINDER)
        sp.set_defaults(func=lambda cfg, a, _n=name: cmd_stage0_exec(cfg, a, argv0=_n))

    def stage0_cli_cmd(
        name: str,
        cli_prefix: Sequence[str],
        help_text: str,
        *,
        accepts_stdlib: bool = True,
    ) -> None:
        stage0_input_cmd(
            name,
            help_text,
            lambda cfg, a, _pre=list(cli_prefix), _std=accepts_stdlib: cmd_stage0_cli(
                cfg, a, cli_args=_pre, accepts_stdlib=_std
            ),
        )

    # cargo tools
    stage0_input_cmd("resolver", "run the stage0 module resolver on a file", cmd_resolver_impl)

    # resolver-dump needs an extra positional (dump_path)
    sp = s0.add_parser("resolver-dump", help="dump global symbols from the stage0 resolver")
    add_config_args(sp)
    sp.add_argument("input_file", nargs="?", default=None)
    sp.add_argument("dump_path", nargs="?", default=None)
    sp.add_argument("rest", nargs=argparse.REMAINDER)
    sp.add_argument("--open", action="store_true", help="open the dump in VS Code afterwards")
    sp.set_defaults(func=cmd_resolver_dump)

    stage0_input_cmd("lexer", "run the stage0 lexer crate on a file",
                     lambda cfg, a: cmd_crate_bin(cfg, a, package="cyrusc_lexer",
                                                  binary="cyrusc_lexer"))
    stage0_input_cmd("parser", "run the stage0 parser crate on a file",
                     lambda cfg, a: cmd_crate_bin(cfg, a, package="cyrusc_parser",
                                                  binary="cyrusc_parser"))
    stage0_input_cmd("sanitizer", "build+run a file with the address sanitizer",
                     cmd_sanitize_run)

    # stage0 frontends (force stage)
    stage0_input_cmd("tokenize", "tokenize a file with the stage0 frontend",
                     cmd_tokenize, stage="stage0")
    stage0_input_cmd("parse", "parse a file with the stage0 frontend",
                     cmd_parse, stage="stage0")

    # cyrus binary subcommands that take an input file
    stage0_cli_cmd("semantic-only", ["semantic-only", "$INPUT"], "semantic analysis only")
    stage0_cli_cmd("lex-only", ["lex-only", "$INPUT"], "lexical analysis only",
                   accepts_stdlib=False)
    stage0_cli_cmd("parse-only", ["parse-only", "$INPUT"], "parse-only dump",
                   accepts_stdlib=False)
    stage0_cli_cmd("emit-llvm", ["emit-llvm", "$INPUT", "-o", "tmp/llvmir"], "emit LLVM IR")
    stage0_cli_cmd("emit-asm", ["emit-asm", "$INPUT", "-o", "tmp/asm"], "emit assembly")
    stage0_cli_cmd("emit-cir-dump", ["emit-cir-dump", "$INPUT", "-o", "tmp/cir_dump"],
                   "emit textual CIR dump")
    stage0_cli_cmd("emit-bitcode", ["emit-bitcode", "$INPUT", "-o", "tmp/bitcode"],
                   "emit LLVM bitcode")
    stage0_cli_cmd("object", ["object", "$INPUT", "-o", "tmp/obj"], "emit an object file")
    stage0_cli_cmd("build-file", ["build", "$INPUT"], "build a single file")

    # cyrus binary passthroughs (no fixed input_file handling)
    stage0_bin_cmd("version", "print version information")
    stage0_bin_cmd("build", "compile source into an executable")
    stage0_bin_cmd("run", "execute a compiled program")
    stage0_bin_cmd("new", "create a new project")
    stage0_bin_cmd("fetch", "fetch libraries into vendor directory")
    stage0_bin_cmd("clean", "clean the build directory")
    stage0_bin_cmd("shared-lib", "generate a shared library")
    stage0_bin_cmd("static-lib", "generate a static library")
    stage0_bin_cmd("help", "print cyrus binary help")

    # No subtool → show stage0 help (func stays unset).
    p.set_defaults(func=None)

    # ************* stage1 (binary passthrough) *************
    p = sub.add_parser(
        "stage1",
        help="pass a subcommand through to the stage1 binary",
        description="Pass subcommands through to the stage1 (self-hosted) binary.",
    )
    add_config_args(p)
    p.add_argument("rest", nargs=argparse.REMAINDER)
    p.set_defaults(func=cmd_stage1)

    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    global VERBOSE

    parser = build_parser()
    args = parser.parse_args(argv)

    if getattr(args, "verbose", False):
        VERBOSE = True

    if not getattr(args, "command", None):
        parser.print_help()
        return 0

    # `--stage all` only makes sense for test/build; narrow to stage0 elsewhere.
    stage_val = getattr(args, "stage", None)
    if stage_val == "all" and args.command not in ("test", "build"):
        args.stage = "stage0"

    root = find_repo_root()
    cfg = load_config(root, args)

    # For build, allow --build-stage defaulting from --stage.
    if args.command == "build" and getattr(args, "build_stage", None) is None:
        if cfg.stage == "all":
            args.build_stage = "both"
        else:
            args.build_stage = cfg.stage

    func: Optional[Callable[[Config, argparse.Namespace], int]] = getattr(args, "func", None)
    if func is None:
        if args.command in ("stage0", "stage1"):
            parser.parse_args([args.command, "--help"])
        parser.print_help()
        return 0

    try:
        return func(cfg, args)
    except KeyboardInterrupt:
        log(STYLE.red("\ninterrupted"))
        return 130
    except SystemExit:
        raise
    except CommandFailed as exc:
        log(STYLE.red(f"error: {exc}"))
        return exc.returncode or 1
    except Exception as exc:
        if VERBOSE:
            raise
        log(STYLE.red(f"error: {exc}"))
        return 1


if __name__ == "__main__":
    sys.exit(main())
