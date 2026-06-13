# SPDX-License-Identifier: MIT
# Copyright (c) 2026 The Cyrus Language

import sys
from pathlib import Path
import re
import subprocess
import shlex
import shutil
from concurrent.futures import ThreadPoolExecutor, as_completed
import os
import tempfile
import hashlib

def get_compiler_version(compiler_path: str, timeout: float = 3.0) -> str | None:
    exe = compiler_path
    
    if not Path(exe).exists():
        which = shutil.which(exe)
        if which:
            exe = which

    try:
        cp = subprocess.run([exe, "version"], capture_output=True, text=True, timeout=timeout)
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return None

    out = (cp.stdout or "").strip()
    err = (cp.stderr or "").strip()
    combined = "\n".join([s for s in (out, err) if s])
    return combined if combined else None


def print_compiler_version(compiler_path: str):
    ver = get_compiler_version(compiler_path)
    if ver:
        print(f"Compiler ({compiler_path}) version: {ver}\n")
    else:
        print(f"Warning: couldn't determine version for compiler '{compiler_path}'.\n")


def unique_name(path: Path) -> str:
    # collision-free stable name
    h = hashlib.sha1(str(path).encode()).hexdigest()[:10]
    return f"{path.stem}_{h}"


def build_and_run(file_path, metadata, compiler_path, compiler_flags, output_dir):
    # isolate temp per test
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)

        output_binary = tmpdir / unique_name(file_path)

        output_binary.parent.mkdir(parents=True, exist_ok=True)

        env = os.environ.copy()
        env["TMPDIR"] = str(tmpdir)
        env["TEMP"] = str(tmpdir)
        env["TMP"] = str(tmpdir)

        # beforeCompile
        before_compile = metadata.get("beforeCompile")
        if before_compile:
            before_cmd = shlex.split(before_compile)
            before_result = subprocess.run(before_cmd, capture_output=True, text=True, env=env)
            if before_result.returncode != 0:
                raise Exception(f"beforeCompile error:\n{before_result.stderr}")

        # build
        build_cmd = [
            compiler_path, "build", str(file_path), "-o", str(output_binary),
        ]
        
        if compiler_flags:
            build_cmd += shlex.split(compiler_flags)

        extra_args = metadata.get("compilerArgs")
        if extra_args:
            build_cmd += shlex.split(extra_args)
        
        build_result = subprocess.run(build_cmd, capture_output=True, text=True, env=env)
        if build_result.returncode != 0:
            raise Exception(f"Build error:\n{build_result.stderr}")

        # run
        args = shlex.split(metadata.get("args") or "")
        run_cmd = [str(output_binary)] + args

        run_result = subprocess.run(
            run_cmd,
            input=metadata.get("stdin") or "",
            capture_output=True,
            text=True,
            env=env
        )

        actual_stdout = run_result.stdout.strip()
        expected_stdout = (metadata.get("stdout") or "").strip()

        actual_stderr = run_result.stderr.strip()
        expected_stderr = (metadata.get("stderr") or "").strip()
        
        if actual_stdout != expected_stdout:
            raise Exception(
                f"Expected stdout:\n   {expected_stdout}\nGot stdout:\n   {actual_stdout}\nGot stderr:\n   {actual_stderr}\n"
            )

        if actual_stderr != expected_stderr:
            raise Exception(
                f"Expected stderr:\n   {expected_stderr}\nGot stderr:\n   {actual_stderr}"
            )


def extract_test_metadata(content, file_name):
    metadata = {
        "stdout": "",
        "stderr": "",
        "stdin": "",
        "args": "",
        "beforeCompile": "",
        "compilerArgs": ""
    }

    patterns = {
        "stdout": r"//\s*@stdout:\s*(.*)",
        "stderr": r"//\s*@stderr:\s*(.*)",
        "stdin": r"//\s*@stdin:\s*(.*)",
        "args": r"//\s*@args:\s*(.*)",
        "beforeCompile": r"//\s*@beforeCompile:\s*(.*)",
        "compilerArgs": r"//\s*@compilerArgs:\s*(.*)",
    }

    for key, pattern in patterns.items():
        match = re.search(pattern, content)
        if match:
            value = match.group(1)
            if key in ("stdout", "stderr"):
                value = value.replace(r'\n', '\n').replace(r'\t', '\t').replace(r'\r', '\r')
            metadata[key] = value

    return metadata


def run_single_test(test_file, base_path, compiler_path, compiler_flags, output_path):
    # compute relative name for nice display
    try:
        relative_name = str(test_file.relative_to(base_path))
    except ValueError:
        # fallback: if base_path is not a parent.
        # when running a single file and base is its parent
        relative_name = test_file.name

    try:
        content = test_file.read_text()
        metadata = extract_test_metadata(content, test_file.name)

        build_and_run(
            test_file,
            metadata,
            compiler_path,
            compiler_flags,
            output_path
        )

        return ("passed", relative_name, None)
    except Exception as e:
        return ("failed", relative_name, str(e))


def main():
    try:
        if len(sys.argv) < 5 or sys.argv[1] not in ("-d", "--directory"):
            print("Usage: main.py -d <test_path> [--compiler <compiler_path>] "
            "[--flags '<extra_flags>'] --output <output_dir>\n"
            "  <test_path> can be a directory (runs all .cyrus files inside recursively) "
            "or a single .cyrus file.")
            exit(1);
            

        path_arg = sys.argv[2]
        output_dir = None
        compiler_path = "cyrus"
        compiler_flags = "" 

        i = 3
        while i < len(sys.argv):
            if sys.argv[i] == "--compiler":
                compiler_path = sys.argv[i + 1]
                i += 2
            elif sys.argv[i] == "--flags":
                compiler_flags = sys.argv[i + 1]
                i += 2
            elif sys.argv[i] == "--output":
                output_dir = sys.argv[i + 1]
                i += 2
            else:
                raise Exception(f"Unknown argument: {sys.argv[i]}")

        if not output_dir:
            raise Exception("--output is required")

        test_path = Path(path_arg)
        if not test_path.exists():
            raise Exception(f"Provided test path '{path_arg}' does not exist.")

        # determine if we are testing a single file or a whole directory
        if test_path.is_file() and test_path.suffix == ".cyrus":
            test_files = [test_path]
            base_path = test_path.parent  # for relative display
        elif test_path.is_dir():
            test_files = list(test_path.rglob("*.cyrus"))
            base_path = test_path
        else:
            raise Exception(f"Provided test path '{path_arg}' is neither a .cyrus file nor a directory.")

        if not test_files:
            print(f"No .cyrus files found in '{path_arg}'. Exiting.")
            return

        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)

        print_compiler_version(compiler_path)

        passed_tests = []
        failed_tests = []

        max_workers = min(16, os.cpu_count() or 4)

        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {
                executor.submit(
                    run_single_test,
                    test_file,
                    base_path,
                    compiler_path,
                    compiler_flags,
                    output_path
                ): test_file
                for test_file in test_files
            }

            for future in as_completed(futures):
                status, name, reason = future.result()
                
                if status == "passed":
                    passed_tests.append(name)
                    print(f"[ok] {name}")
                else:
                    failed_tests.append((name, reason))
                    print(f"[error] {name}:\n")
                    print("    " + reason.strip().replace('\n', '\n    '))
                    print()

        passed_tests.sort()
        failed_tests.sort(key=lambda x: x[0])

        print("-------------------------------------------")
        print("Test Summary: ")
        total = len(passed_tests) + len(failed_tests)
        print(f"Total: {total}")
        print(f"Passed: {len(passed_tests)}")
        print(f"Failed: {len(failed_tests)}")

        if failed_tests:
            sys.exit(1)

    except Exception as err:
        print(f"\nFatal error: {err}")
        sys.exit(1)


if __name__ == "__main__":
    main()