#!/bin/bash

set -eux -o pipefail

LLC=${RILL_LLC:-llc}
CC=${RILL_CC:-gcc}

echo $RILLC_COMPILER
echo $RILLC_CORELIB_SRCDIR
echo $RILLC_CORELIB_LIBDIR

FILE=../rillc/test/simple.rill

OUT_DIR=`mktemp -d '/tmp/rillc.XXXXXXXXXXXXXXXX'`

# Emit an LLVM IR bitcode
$RILLC_COMPILER \
    --corelib_srcdir="$RILLC_CORELIB_SRCDIR" \
    --out_dir="$OUT_DIR" \
    "$FILE"

FILE_BC_PATH="$OUT_DIR/$(basename $FILE).bc"
FILE_ASM_PATH="$OUT_DIR/$(basename $FILE).s"
FILE_OUT_PATH="$OUT_DIR/$(basename $FILE).out"

# Emit an asm file
$LLC --relocation-model=pic "$FILE_BC_PATH" -o "$FILE_ASM_PATH"

# Emit an executable
$CC -v -fPIE -L"$RILLC_CORELIB_LIBDIR" -static -lcore-c "$FILE_ASM_PATH" -o "$FILE_OUT_PATH"

$FILE_OUT_PATH
