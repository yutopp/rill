#!/bin/bash

set -eux -o pipefail

# docker run -v $(pwd):/t -it --rm yutopp/rill-build-env:base-x86_64 bash /t/base.sh
opam repo add rillc-deps-opam-repo https://github.com/yutopp/rillc-deps-opam-repo.git
opam update
opam install llvm.11.0.0+rillc -y

