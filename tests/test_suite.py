import sys
from pathlib import Path
import re
import subprocess
import shlex
import shutil

def get_compiler_version(compiler_path: str, timeout: float = 3.0) -> str | None:
    """
    Get the compiler version using the 'version' subcommand.
    Returns the version string or None if the compiler is not found or fails.
    """
    exe = compiler_path
    
    if not Path(exe).exists():
        which = shutil.which(exe)
        if which:
            exe = which

    try:
        cp = subprocess.run([exe, "version"], capture_output=True, text=True, timeout=timeout)
    except FileNotFoundError:
        return None
    except subprocess.TimeoutExpired:
        return None

    out = (cp.stdout or "").strip()
    err = (cp.stderr or "").strip()
    combined = "\n".join([s for s in (out, err) if s])
    return combined if combined else None


def print_compiler_version(compiler_path: str):
    ver = get_compiler_version(compiler_path)
    if ver:
        print(f"Compiler ({compiler_path}) version:\n{ver}\n")
    else:
        print(f"Warning: couldn't determine version for compiler '{compiler_path}'.\n")


def build_and_run(file_path, metadata, compiler_path, compiler_flags, output_dir):
    output_binary = Path(output_dir) / file_path.stem

    build_cmd = [
        compiler_path, "build", str(file_path), "-o", str(output_binary),
    ]
    
    if compiler_flags:
        build_cmd += shlex.split(compiler_flags)

    build_result = subprocess.run(build_cmd, capture_output=True, text=True)
    if build_result.returncode != 0:
        raise Exception(f"Build error:\n{build_result.stderr}")

    args = shlex.split(metadata.get("args") or "")
    run_cmd = [str(output_binary)] + args

    run_result = subprocess.run(run_cmd, input=metadata.get("stdin") or "", capture_output=True, text=True)

    actual_stdout = run_result.stdout.strip()
    expected_stdout = (metadata.get("stdout") or "").strip()

    if actual_stdout != expected_stdout:
        raise Exception(
            f"Expected:\n   {expected_stdout}\nGot:\n   {actual_stdout}"
        )

def extract_test_metadata(content, file_name):
    metadata = {
        "stdout": "",
        "stdin": "",
        "args": ""
    }

    stdout_match = re.search(r"//\s*@stdout:\s*(.*)", content)
    if stdout_match:
        metadata["stdout"] = stdout_match.group(1)

    stdin_match = re.search(r"//\s*@stdin:\s*(.*)", content)
    if stdin_match:
        metadata["stdin"] = stdin_match.group(1)

    args_match = re.search(r"//\s*@args:\s*(.*)", content)
    if args_match:
        metadata["args"] = args_match.group(1)

    return metadata


import sys
from pathlib import Path

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
                if i + 1 >= len(sys.argv):
                    raise Exception("Missing compiler path after --compiler")
                compiler_path = sys.argv[i + 1]
                i += 2
            elif sys.argv[i] == "--flags":
                if i + 1 >= len(sys.argv):
                    raise Exception("Missing compiler flags after --flags")
                compiler_flags = sys.argv[i + 1]
                i += 2
            elif sys.argv[i] == "--output":
                if i + 1 >= len(sys.argv):
                    raise Exception("Missing output directory after --output")
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

        passed_tests = []
        failed_tests = []

        for test_file in tests_path.rglob("*.cyrus"):
            if test_file.is_file():
                relative_name = test_file.relative_to(tests_path)
                try:
                    with open(test_file, "r") as file:
                        content = file.read()
                        metadata = extract_test_metadata(content, test_file.name)
                        build_and_run(test_file, metadata, compiler_path, compiler_flags, output_path)
                    passed_tests.append(str(relative_name))
                except Exception as e:
                    failed_tests.append((str(relative_name), str(e)))

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