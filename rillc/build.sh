#!/bin/bash

# Helper script to build rill programs

RILLC_BIN="$1"
shift
ARGS="$@"

echo $RILLC_BIN
echo $@

corelib=../corelib/src/core

$RILLC_BIN compile -I "$corelib" \
           -d /tmp \
           ../rillc/test/simple.rill \
    && llvm-as main.ll \
    && llc --relocation-model=pic main.bc \
    && musl-gcc -fPIE main.s runtime.c \
    && ./a.out
