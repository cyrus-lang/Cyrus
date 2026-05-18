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

        if actual_stdout != expected_stdout:
            raise Exception(
                f"Expected stdout:\n   {expected_stdout}\nGot stdout:\n   {actual_stdout}"
            )

        actual_stderr = run_result.stderr.strip()
        expected_stderr = (metadata.get("stderr") or "").strip()

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
            metadata[key] = match.group(1)

    return metadata


def run_single_test(test_file, tests_path, compiler_path, compiler_flags, output_path):
    relative_name = str(test_file.relative_to(tests_path))
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
            raise Exception(
                "Usage: main.py -d <test_dir> [--compiler <compiler_path>] "
                "[--flags '<extra_flags>'] --output <output_dir>"
            )

        directory = sys.argv[2]
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

        tests_path = Path(directory)
        if not tests_path.exists() or not tests_path.is_dir():
            raise Exception(f"Provided test directory '{directory}' is invalid.")

        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)

        print_compiler_version(compiler_path)

        test_files = [f for f in tests_path.rglob("*.cyrus") if f.is_file()]

        passed_tests = []
        failed_tests = []

        max_workers = min(16, os.cpu_count() or 4)

        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {
                executor.submit(
                    run_single_test,
                    test_file,
                    tests_path,
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
                    print(f"✓ {name}")
                else:
                    failed_tests.append((name, reason))
                    print(f"✗ {name}")

        passed_tests.sort()
        failed_tests.sort(key=lambda x: x[0])

        print("\n=== Test Summary ===")
        total = len(passed_tests) + len(failed_tests)
        print(f"Total: {total}")
        print(f"Passed: {len(passed_tests)}")
        print(f"Failed: {len(failed_tests)}")

        if passed_tests:
            print("\nPassed tests:")
            for name in passed_tests:
                print(f"  - {name}")

        if failed_tests:
            print("\nFailed tests:")
            for name, reason in failed_tests:
                print(f"  - {name}:\n")
                print("    " + reason.strip().replace('\n', '\n    '))
                print()

        if failed_tests:
            sys.exit(1)

    except Exception as err:
        print(f"\nFatal error: {err}")
        sys.exit(1)


if __name__ == "__main__":
    main()