#!/bin/bash

TESTS=$@

SUCC_BIN="succ-exe"
SUCC_BIN_PATH=$(stack exec which $SUCC_BIN)

TEST_REPO="git@github.com:jgthomas/succ-functional-tests.git"
TEST_DIR=$(basename ${TEST_REPO%.*})
TEST_DIR_PATH="../$TEST_DIR"

TEST_SCRIPT="test_compiler.sh"


if [[ ! -d $TEST_DIR_PATH ]]; then
        git clone $TEST_REPO $TEST_DIR_PATH
fi


(cd $TEST_DIR_PATH && ./$TEST_SCRIPT $SUCC_BIN_PATH $TESTS)
