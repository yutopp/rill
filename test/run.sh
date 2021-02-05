#!/bin/bash

set -eu -o pipefail

_="$RILL_TEST_COMPILER"
_="$RILL_TEST_TARGET"

SCRIPT_DIR="$(cd $(dirname $0); pwd)"
TEST_PASS_DIR="$SCRIPT_DIR/pass"
TEST_COMPILE_FAILURE_DIR="$SCRIPT_DIR/compile_failure"
TEST_BUILD_FAILURE_DIR="$SCRIPT_DIR/build_failure"

function fail() {
    echo "Failed: CASE='$2', MSG='$1'" >&2
    exit 1
}

function build_and_execute() {
    local CASENAME="$1"
    local FILE="$TEST_PASS_DIR/$CASENAME.rill"
    local TEST_EXPECT_FILE="$TEST_PASS_DIR/$CASENAME.expect"

    OUT_DIR=`mktemp -d '/tmp/rillc.XXXXXXXXXXXXXXXX'`

    FILE_OUT_PATH="$OUT_DIR/$(basename $FILE).out"

    # Emit an executable
    $RILL_TEST_COMPILER compile \
                        --log-level=debug \
                        -o "$FILE_OUT_PATH" \
                        "$FILE" \
        || fail "compile" "$CASENAME"

    # Execute
    local TEST_ACTUAL_FILE="$OUT_DIR/test.actual"
    $FILE_OUT_PATH > $TEST_ACTUAL_FILE 2>&1
    local EXIT_CODE=$?
    if [ ! $EXIT_CODE -eq 0 ]; then
        fail "a.out(exitcode: $EXIT_CODE)" "$CASENAME"
    fi

    # Expect file
    if [ ! -f "$TEST_EXPECT_FILE" ]; then
       fail "unexpected: file not found ($TEST_EXPECT_FILE)" "$CASENAME"
    fi

    diff -u "$TEST_EXPECT_FILE" "$TEST_ACTUAL_FILE" \
        || fail "output has diff" "$CASENAME"
}

function compile_and_check_failure() {
    local BASE_DIR="$1"
    local CASENAME="$2"

    local FILE="$BASE_DIR/$CASENAME.rill"
    local TEST_EXPECT_FILE="$BASE_DIR/$CASENAME.expect"

    OUT_DIR=`mktemp -d '/tmp/rillc.XXXXXXXXXXXXXXXX'`

    # Emit an Rill IR bitcode
    local TEST_ACTUAL_FILE="$OUT_DIR/test.buildlog"
    set +e
    $RILL_TEST_COMPILER compile --emit=rill-ir "$FILE" >& $TEST_ACTUAL_FILE
    local EXIT_CODE=$?
    set -e
    if [ $EXIT_CODE -eq 0 ]; then
        fail "compile: succeeded(expect failure)" "$CASENAME"
    fi

    # Expect file
    if [ ! -f "$TEST_EXPECT_FILE" ]; then
       fail "unexpected: file not found ($TEST_EXPECT_FILE)" "$CASENAME"
    fi

    echo ""
    echo "=== DIFF ==="
    echo ""

    diff -u "$TEST_EXPECT_FILE" "$TEST_ACTUAL_FILE" \
        || fail "output has diff" "$CASENAME"
}

function build_and_check_failure() {
    local BASE_DIR="$1"
    local CASENAME="$2"

    local FILE="$BASE_DIR/$CASENAME.rill"
    local TEST_EXPECT_FILE="$BASE_DIR/$CASENAME.expect"

    OUT_DIR=`mktemp -d '/tmp/rillc.XXXXXXXXXXXXXXXX'`

    local TEST_ACTUAL_FILE="$OUT_DIR/test.buildlog"
    set +e
    $RILL_TEST_COMPILER compile "$FILE" >& $TEST_ACTUAL_FILE
    local EXIT_CODE=$?
    set -e
    if [ $EXIT_CODE -eq 0 ]; then
        fail "compile: succeeded(expect failure)" "$CASENAME"
    fi

    # Expect file
    if [ ! -f "$TEST_EXPECT_FILE" ]; then
       fail "unexpected: file not found ($TEST_EXPECT_FILE)" "$CASENAME"
    fi

    echo ""
    echo "=== DIFF ==="
    echo ""

    diff -u "$TEST_EXPECT_FILE" "$TEST_ACTUAL_FILE" \
        || fail "output has diff" "$CASENAME"
}


#
for path in "$TEST_PASS_DIR"/*.rill; do
    casename="$(basename "$path" .rill)"
    build_and_execute "$casename"
done

#
for path in "$TEST_COMPILE_FAILURE_DIR"/*.rill; do
    casename="$(basename "$path" .rill)"
    compile_and_check_failure "$TEST_COMPILE_FAILURE_DIR" "$casename"
done

#
for path in "$TEST_BUILD_FAILURE_DIR"/*.rill; do
    casename="$(basename "$path" .rill)"
    build_and_check_failure "$TEST_BUILD_FAILURE_DIR" "$casename"
done
