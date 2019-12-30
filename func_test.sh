#!/bin/bash

TESTS=$@

SUCC_BIN="succ-exe"
SUCC_BIN_PATH=$(stack exec which $SUCC_BIN)

TEST_DIR="$HOME/Code/write_a_c_compiler"
TEST_SCRIPT="./test_compiler.sh"
ALL_TEST_CASES="1 \
                2 \
                3 \
                4 \
                5 \
                6 \
                7 \
                8 \
                9 \
                10 \
                ops \
                pointers \
                types \
                bitwise"


if [[ ! -z $TESTS ]]; then
        TEST_CASES=$TESTS
else
        TEST_CASES=$ALL_TEST_CASES
fi


(cd $TEST_DIR && $TEST_SCRIPT $SUCC_BIN_PATH $TEST_CASES)
