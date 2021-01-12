#!/bin/bash
set -e -o pipefail -o xtrace

eval `opam config env`

omake COVERAGE=true build_rillc
omake build_corelib
omake build_stdlib
