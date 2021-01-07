#!/bin/bash

set -eux -o pipefail

export PATH=$PATH:/usr/local/llvm/bin
which llvm-config

# docker run -v $(pwd):/t -it --rm yutopp/rill-build-env:llvm-11.0.0-x86_64 bash /t/llvm.sh
llvm-config --link-static --libs
