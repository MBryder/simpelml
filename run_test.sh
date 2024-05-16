#!/bin/bash

# Source the integration_test.sh script
source ./integration_test.sh

echo "Running tests..."

# Run multiple test cases
run_integration_test "/Users/jenspetersen/Documents/GitHub/simpelml/exampels/testing_print.sm" "hello"
run_integration_test "/Users/jenspetersen/Documents/GitHub/simpelml/exampels/test_addition.sm" "4"

