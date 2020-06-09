#!/bin/bash

set -eux -o pipefail

LLVM_AS=${RILL_LLVM_AS:-llvm-as}
LLC=${RILL_LLC:-llc}
CC=${RILL_CC:-gcc}

_=$RILLC_COMPILER
_=$RILLC_CORELIB_SRCDIR
_=$RILLC_CORELIB_LIBDIR

TEST_PASS_DIR="../test/pass"

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
        --emit=llvm-ir \
        "$FILE" || failed_to_execute "compile" "$CASENAME"

    FILE_LL_PATH="$OUT_DIR/$(basename $FILE).ll"
    FILE_BC_PATH="$OUT_DIR/$(basename $FILE).bc"
    FILE_ASM_PATH="$OUT_DIR/$(basename $FILE).s"
    FILE_OUT_PATH="$OUT_DIR/$(basename $FILE).out"

    # Emit an LLVM IR bitcode
    $LLVM_AS "$FILE_LL_PATH" -o "$FILE_BC_PATH"

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
    local OUTPUT=$($FILE_OUT_PATH 2>&1)
    local EXIT_CODE=$?
    if [ ! $EXIT_CODE -eq 0 ]; then
        failed_to_execute "a.out(exitcode: $EXIT_CODE)" "$CASENAME"
    fi
    local TEST_ACTUAL_FILE="$OUT_DIR/test.actual"
    echo "$OUTPUT" > "$TEST_ACTUAL_FILE"

    # Expect file
    if [ ! -f "$TEST_EXPECT_FILE" ]; then
       failed_to_execute "expect: file not found ($TEST_EXPECT_FILE)" "$CASENAME"
    fi

    diff -u "$TEST_ACTUAL_FILE" "$TEST_EXPECT_FILE" \
        || failed_to_execute "expect" "$CASENAME"
}

for path in "$TEST_PASS_DIR"/*.rill; do
    casename="$(basename "$path" .rill)"
    build_and_execute "$casename"
done
