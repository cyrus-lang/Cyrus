import sys
from pathlib import Path
import re
import os
import subprocess
import shlex

def build_and_run(file_path, metadata, compiler_path, output_dir):
    output_binary = Path(output_dir) / file_path.stem

    build_result = subprocess.run([
        compiler_path, "build", str(file_path), "-o", str(output_binary),
        "--reloc-mode=pic"
    ], capture_output=True, text=True)

    if build_result.returncode != 0:
        raise Exception(f"Build error:\n{build_result.stderr}")

    args = shlex.split(metadata["args"] or "")
    run_cmd = [str(output_binary)] + args

    run_result = subprocess.run(run_cmd, input=metadata["stdin"] or "", capture_output=True, text=True)

    actual_stdout = run_result.stdout.strip()
    expected_stdout = (metadata["stdout"] or "").strip()

    if actual_stdout != expected_stdout:
        raise Exception(
            f"Expected:\n       {expected_stdout}\n\nGot:\n       {actual_stdout}"
        )

def extract_test_metadata(content, file_name):
    metadata = {
        "stdout": None,
        "stdin": None,
        "args": None
    }

    stdout_match = re.search(r"//\s*@stdout:\s*(.*)", content)
    if stdout_match:
        metadata["stdout"] = stdout_match.group(1)
    else:
        raise Exception(f"Missing required @stdout directive.")

    stdin_match = re.search(r"//\s*@stdin:\s*(.*)", content)
    if stdin_match:
        metadata["stdin"] = stdin_match.group(1)

    args_match = re.search(r"//\s*@args:\s*(.*)", content)
    if args_match:
        metadata["args"] = args_match.group(1)

    return metadata

def main():
    try:
        if len(sys.argv) < 5 or sys.argv[1] not in ("-d", "--directory"):
            raise Exception("Usage: main.py -d <test_dir> [--compiler <compiler_path>] --output <output_dir>")

        directory = sys.argv[2]
        output_dir = None
        compiler_path = "cyrus"

        i = 3
        while i < len(sys.argv):
            if sys.argv[i] == "--compiler":
                if i + 1 >= len(sys.argv):
                    raise Exception("Missing compiler path after --compiler")
                compiler_path = sys.argv[i + 1]
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

        passed_tests = []
        failed_tests = []

        for test_file in tests_path.glob("*.cyr"):
            if test_file.is_file():
                try:
                    with open(test_file, "r") as file:
                        content = file.read()
                        metadata = extract_test_metadata(content, test_file.name)
                        build_and_run(test_file, metadata, compiler_path, output_path)
                    passed_tests.append(test_file.name)
                except Exception as e:
                    failed_tests.append((test_file.name, str(e)))

        print("\n=== Test Summary ===")
        print(f"Total: {len(passed_tests) + len(failed_tests)}")
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
