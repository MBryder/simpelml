#!/bin/bash

# Run the tests using the Makefile and capture the output
output=$(make 2>&1)  # Captures both stdout and stderr

# Optionally, you can check the output if needed
expected="The number is even."
if echo "$output" | grep -q "$expected"; then
    echo "Test passed: found expected output."
else
    echo "Test failed: expected output not found."
    echo "Output was:"
    echo "$output"
fi