#!/bin/bash

set -eux -o pipefail

LLC=${RILL_LLC:-llc}
CC=${RILL_CC:-gcc}

_=$RILLC_COMPILER
_=$RILLC_CORELIB_SRCDIR
_=$RILLC_CORELIB_LIBDIR

SCRIPT_DIR="$(cd $(dirname $0); pwd)"
TEST_PASS_DIR="$SCRIPT_DIR/pass"
TEST_BUILD_FAILURE_DIR="$SCRIPT_DIR/build_failure"

function failed_to_execute() {
    echo "Failed to execute: $1 in case '$2'" >2
    exit 1
}

function build_and_execute() {
    local CASENAME="$1"
    local FILE="$TEST_PASS_DIR/$CASENAME.rill"
    local TEST_EXPECT_FILE="$TEST_PASS_DIR/$CASENAME.expect"

    OUT_DIR=`mktemp -d '/tmp/rillc.XXXXXXXXXXXXXXXX'`

    # Emit an LLVM IR bitcode
    $RILLC_COMPILER \
        --corelib_srcdir="$RILLC_CORELIB_SRCDIR" \
        --out_dir="$OUT_DIR" \
        --log-level=debug \
        "$FILE" || failed_to_execute "compile" "$CASENAME"

    FILE_BC_PATH="$OUT_DIR/$(basename $FILE).bc"
    FILE_ASM_PATH="$OUT_DIR/$(basename $FILE).s"
    FILE_OUT_PATH="$OUT_DIR/$(basename $FILE).out"

    # Emit an asm file
    $LLC --relocation-model=pic "$FILE_BC_PATH" -o "$FILE_ASM_PATH" \
        || failed_to_execute "asm: $LLC" "$CASENAME"

    # Emit an executable
    $CC -v -fPIE \
        -L"$RILLC_CORELIB_LIBDIR" \
        -static \
        -lcore-c \
        "$FILE_ASM_PATH" \
        -o "$FILE_OUT_PATH" \
        || failed_to_execute "exec: $CC" "$CASENAME"

    # Execute
    local TEST_ACTUAL_FILE="$OUT_DIR/test.actual"
    $FILE_OUT_PATH > $TEST_ACTUAL_FILE 2>&1
    local EXIT_CODE=$?
    if [ ! $EXIT_CODE -eq 0 ]; then
        failed_to_execute "a.out(exitcode: $EXIT_CODE)" "$CASENAME"
    fi

    # Expect file
    if [ ! -f "$TEST_EXPECT_FILE" ]; then
       failed_to_execute "expect: file not found ($TEST_EXPECT_FILE)" "$CASENAME"
    fi

    diff -u "$TEST_ACTUAL_FILE" "$TEST_EXPECT_FILE" \
        || failed_to_execute "expect" "$CASENAME"
}

function build_and_check_failure() {
    local BASE_DIR="$1"
    local CASENAME="$2"

    local FILE="$BASE_DIR/$CASENAME.rill"
    local TEST_EXPECT_FILE="$BASE_DIR/$CASENAME.expect"

    OUT_DIR=`mktemp -d '/tmp/rillc.XXXXXXXXXXXXXXXX'`

    # Emit an LLVM IR bitcode
    local TEST_ACTUAL_FILE="$OUT_DIR/test.buildlog"
    set +e
    $RILLC_COMPILER \
        --corelib_srcdir="$RILLC_CORELIB_SRCDIR" \
        --out_dir="$OUT_DIR" \
        --emit=llvm-ir \
        "$FILE" > $TEST_ACTUAL_FILE 2>&1
    local EXIT_CODE=$?
    set -e
    if [ $EXIT_CODE -eq 0 ]; then
        failed_to_execute "compile: succeeded" "$CASENAME"
    fi

    # Expect file
    if [ ! -f "$TEST_EXPECT_FILE" ]; then
       failed_to_execute "expect: file not found ($TEST_EXPECT_FILE)" "$CASENAME"
    fi

    diff -u "$TEST_ACTUAL_FILE" "$TEST_EXPECT_FILE" \
        || failed_to_execute "expect" "$CASENAME"
}

#
for path in "$TEST_PASS_DIR"/*.rill; do
    casename="$(basename "$path" .rill)"
    build_and_execute "$casename"
done

#
for path in "$TEST_BUILD_FAILURE_DIR"/*.rill; do
    casename="$(basename "$path" .rill)"
    build_and_check_failure "$TEST_BUILD_FAILURE_DIR" "$casename"
done
