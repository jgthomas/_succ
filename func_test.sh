#!/bin/bash

SUCC_BIN=$(stack exec which succ-exe)

TEST_DIR=$HOME/Code/write_a_c_compiler
TEST_SCRIPT="./test_compiler.sh"
TEST_CASES="1 2 3 4 5 6 7 8 9 10 ops pointers types"

(cd $TEST_DIR && $TEST_SCRIPT $SUCC_BIN $TEST_CASES)
