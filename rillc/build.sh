#!/bin/bash

set -eux -o pipefail
# Helper script to build rill programs

RILLC_BIN="$1"
shift
ARGS="$@"

echo $RILLC_BIN
echo $@

corelib=../corelib/src/core

$RILLC_BIN compile -I "$corelib" \
           -d /tmp \
           $FILE 1> main.ll
llvm-as main.ll
llc --relocation-model=pic main.bc
gcc -v -fPIE -L$(pwd)/corelib -static -lcore-c main.s
