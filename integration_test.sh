#!/bin/bash

run_integration_test() {
    # Check if the correct number of arguments are provided
    if [ "$#" -ne 2 ]; then
        echo "Usage: run_integration_test <source_code.sm> <expected_output or expected_output_file>"
        return 1
    fi

    # Assign arguments to variables
    SOURCE_CODE="$1"
    EXPECTED_OUTPUT="$2"

    echo "Running test for $SOURCE_CODE with expected output $EXPECTED_OUTPUT"

    # Check if the source code file exists
    if [ ! -f "$SOURCE_CODE" ]; then
        echo "Source code file $SOURCE_CODE does not exist."
        return 1
    fi

    # Determine if the expected output is a file or a string
    if [ -f "$EXPECTED_OUTPUT" ]; then
        EXPECTED_OUTPUT_CONTENT=$(cat "$EXPECTED_OUTPUT")
    else
        EXPECTED_OUTPUT_CONTENT="$EXPECTED_OUTPUT"
    fi

    echo "Expected output: $EXPECTED_OUTPUT_CONTENT"

    # Extract the name of the .sm file without the path and extension for output redirection
    SOURCE_BASENAME=$(basename "$SOURCE_CODE" .sm)

    # Compile the executable if it doesn't exist
    make simpelml.exe

    # Run the source code using dune exec and capture the output
    ACTUAL_OUTPUT=$(dune exec lib/simpelml.exe "$SOURCE_CODE" 2>&1 | tail -n 1)

    echo "Actual output: $ACTUAL_OUTPUT"

    # Compare the actual output with the expected output content
    if [ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT_CONTENT" ]; then
        echo "Test passed: Output matches expected output."
        return 0
    else
        echo "Test failed: Output does not match expected output."
        echo "Expected: $EXPECTED_OUTPUT_CONTENT"
        echo "Actual: $ACTUAL_OUTPUT"
        return 1
    fi
}
