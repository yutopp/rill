#!/bin/bash
set -e -o pipefail -o xtrace

eval `opam config env`
ls -la

omake test
