#!/bin/bash

# Source the integration_test.sh script
source ./integration_test.sh

echo "Running tests..."

# Run multiple test cases
run_integration_test  "exampels/integration_test/test_addition.sm" "hello"
run_integration_test "exampels/integration_test/test_addition.sm" "4"
run_integration_test "exampels/integration_test/test_subtraction.sm" "5"
run_integration_test "exampels/integration_test/test_increment.sm" "4"
run_integration_test "exampels/integration_test/test_decrement.sm" "9"
run_integration_test "exampels/integration_test/testing_matrixT.sm" "[[1, 4], [2, 5], [3, 6]]"
run_integration_test "exampels/integration_test/testing_transM_timesM.sm" "[[17, 22, 27], [22, 29, 36], [27, 36, 45]]"
run_integration_test "exampels/integration_test/testing_linear_reg.sm" "69.9213505863"
