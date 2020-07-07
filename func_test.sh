#!/bin/bash

TESTS=$@

SUCC_BIN="succ-exe"
SUCC_BIN_PATH=$(stack exec which $SUCC_BIN)

TEST_REPO="git@github.com:jgthomas/succ-functional-tests.git"
TEST_DIR=$(basename ${TEST_REPO%.*})
TEST_DIR_PATH="../$TEST_DIR"

TEST_SCRIPT="test_compiler.sh"

ALL_TEST_CASES="literals \
                unary \
                binary_I \
                binary_II \
                locals \
                conditionals \
                compound \
                loops \
                functions \
                globals \
                ops \
                pointers \
                types \
                bitwise \
                array"


# clone tests if they don't exist locally
if [[ ! -d $TEST_DIR_PATH ]]; then
        git clone $TEST_REPO $TEST_DIR_PATH
fi


# set the tests to run
if [[ ! -z $TESTS ]]; then
        TEST_CASES=$TESTS
else
        TEST_CASES=$ALL_TEST_CASES
fi


# enter subshell in test directory and run tests
(cd $TEST_DIR_PATH && ./$TEST_SCRIPT $SUCC_BIN_PATH $TEST_CASES)
