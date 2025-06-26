#!/bin/bash

# benchmark.sh
# A simple script to measure the execution time of a given program.

usage() {
    echo "Usage: $0 <command_to_benchmark> [arg1] [arg2] ..."
    echo ""
    echo "This script executes the provided command and measures its running time"
    echo "using the 'time' utility. It outputs real, user, and sys time."
    echo ""
    echo "Example: $0 ./my_fibonacci_program"
    echo "Example: $0 python my_script.py arg1 arg2"
    exit 1
}

if [ "$#" -eq 0 ]; then
    usage
fi

echo "--- Starting Benchmark ---"
echo "Command to run: $*"
echo ""

if command -v /usr/bin/time &> /dev/null; then
    /usr/bin/time -v "$@" 2>&1
else
    echo "Warning: /usr/bin/time not found. Using shell's built-in 'time' (less detailed)."
    time "$@"
fi

echo ""
echo "--- Benchmark Finished ---"