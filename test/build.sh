#!/bin/bash
set -e
set -o pipefail

eval `opam config env`

omake COVERAGE=true build_rillc
omake build_corelib
omake build_stdlib
